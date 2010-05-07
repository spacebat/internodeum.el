;;; internodeum.el --- Internode Usage Meter

;; Copyright (C) 2010  Andrew Kirkpatrick <ubermonk@gmail.com>

;; Author: Andrew Kirkpatrick <ubermonk@gmail.com>
;; Maintainer: Andrew Kirkpatrick <ubermonk@gmail.com>
;; Created: 5 May 2010
;; Version: 0.5
;; Keywords: internet, ISP, Internode, quota, monitor

;; This file is NOT part of GNU Emacs.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an experiment in using Emacs Lisp as a HTTP client and
;; XML parser. Its not too shabby in this regard for what it is. I
;; was also a little curious about Internode's API. There's only
;; one service I'm provisioned for there, if you have more than
;; that you may have to set the service-type appropriately.

;; The code is probably not very idiomatic for this language.
;; Comments, suggestions and patches welcome. Please see:
;; http://github.com/spacebat/internodeum.el

;; TODO:
;; put data on the mode line
;; make the mode line data configurable with a format string
;; use defcustom
;; background polling
;; logging
;; combine credentials-files and -priority add an option to obtain them from the environment
;; make paranoia boolean; unset after the last fetch
;; or drop paranoia altogether... this is emacs and we're all friends
;; do some date math on the rollover
;; graph something? :)
;; make sure buffers are not left lying around

;;; Usage:

;; In your emacs configuration, either set internodeum/credentials like so:

;; (setq internodeum/credentials (internodeum/make-creds :username "myusername"
;;                                                       :password "mypassword"))

;; Or set up internodeum/credentials files and -priority appropriately. By
;; default, your .fetchmailrc and .netrc will be searched. The first occurrence
;; of username and password like credentials in these files will be taken as
;; your Internode login details. Not very satisfactory I know.

;; Once you have the credentials set, you might want to set the poll interval,
;; which defaults to 30 minutes.

;; (setq internodeum/poll-interval (* 60 60)) ; 1 hour

;; Then you can start polling with:

;; (internodeum/poll-start)

;; or interactively with M-x internodeum/poll-start

;; The function internodeum/poll-stop will stop the polling timer.

;;; Code:

(eval-when-compile (require 'cl))

(require 'rx)

(defvar internodeum/credentials nil
  "Your internode account username and password, as an internodeum/creds struct")

(defvar internodeum/credentials-files '("~/.fetchmailrc" "~/.netrc")
  "The sequence of files internodeum/find-creds should search for credentials")

(defvar internodeum/credentials-priority '(find prompt)
  "The order in which methods to obtain the credentials should be tried.")

(defvar internodeum/paranoia nil
  "If a true value, internodeum/credentials created by internodeum/determine-creds
 will be deleted in 5 seconds. If a number, it is taken as the number of seconds.")

(defvar internodeum/usage-href "https://customer-webtools-api.internode.on.net/api/v1.5/"
  "The base URL of the internode usage API")

(defvar internodeum/usage-url (url-generic-parse-url internodeum/usage-href))

(defvar internodeum/poll-interval (* 30 60)
  "The number of seconds to sleep between polls")

(defvar internodeum/poll-latest-stats nil
  "Usage structure from last poll")

(defvar internodeum/poll-latest-summary nil
  "Usage summary from last poll")

(defvar internodeum/service-type nil
  "The service type to query by default")

(defvar internodeum/poll-service-type nil
  "The service type to query by default at poll time")

(defvar internodeum/poll-timer nil
  "Contains the polling timer handle")

(defstruct (internodeum/creds (:constructor internodeum/make-creds))
  (username)
  (password))

(defun internodeum/creds-basic-auth (this)
  "Create a HTTP basic auth token"
  (concat "Basic "
          (base64-encode-string (concat (internodeum/creds-username this)
                                        ":"
                                        (internodeum/creds-password this))
                                t)))

(defun* internodeum/determine-creds (&optional reset)
  "Step through internodeum/credentials-priorty looking for login credentials"
  (interactive)
  (if reset (setq internodeum/credentials nil))
  (unless internodeum/credentials
    (let (method)
      (dolist (method internodeum/credentials-priority)
        (setq method (intern (concat "internodeum/" (symbol-name method) "-creds")))
        (setq internodeum/credentials
              (if (interactive-form method)
                  (call-interactively method)
                (funcall (symbol-function method))))
        (when internodeum/credentials
          (when internodeum/paranoia
            (lexical-let ((creds internodeum/credentials))
              (run-at-time (concat (if (numberp internodeum/paranoia)
                                       (format "%d" internodeum/paranoia)
                                     "5")
                                   " sec")
                           nil
                           (lambda ()
                             ;; reach out and wipe the credentials struct,
                             ;; but nothing can be done if they've been copied
                             (setf (internodeum/creds-username creds) nil)
                             (setf (internodeum/creds-password creds) nil)
                             (setq internodeum/credentials nil)))))
          (return)))))
  (unless internodeum/credentials
    (when internodeum/poll-timer
      (internodeum/poll-stop)
      (message "Aborted internode polling")))
  internodeum/credentials)

(defun internodeum/prompt-creds (&optional username password)
  "Prompt for username and password, returning a credentials struct. If both are empty, return nil"
  (interactive "sUsername: ")
  (if (called-interactively-p)
      (setq password (read-passwd "Password: " nil password)))
  (if (and (plusp (length username)) (plusp (length password)))
      (internodeum/make-creds :username username :password password)
    nil))

(defun* internodeum/find-creds (&key files prepend)
  (if files
      (when prepend (setq files (nconc files (copy-list internodeum/credentials-files))))
    (setq files internodeum/credentials-files))
  (let ((user-regex
         (rx (and (or "user" "username" "login") (* space) (opt (any ?: ?=)) (* space) (group (+ (not space))))))
        (pass-regex
         (rx (and (or "pass" "password" "passwd") (* space) (opt (any ?: ?=)) (* space) (group (+ (not space))))))
        username password buffer)
    (catch 'done
      (dolist (file files)
        (when (file-readable-p file)
          (save-excursion
            (setq buffer (set-buffer (generate-new-buffer file)))
            (insert-file-contents (expand-file-name (substitute-in-file-name file)))
            (when (search-forward-regexp user-regex nil t)
              (setq username (match-string 1))
              (if (string-match (rx string-start (group (+ (not (any ?@))))) username)
                  (setq username (match-string 1 username))
                (setq username nil)))
            (goto-char (point-min))
            (when (search-forward-regexp pass-regex nil t)
              (setq password (match-string 1)))
            (kill-buffer buffer))
          (when (and username password)
            (throw 'done (internodeum/make-creds :username username :password password))))))))


(defun internodeum/usage-url (&optional path)
  (let ((url (url-generic-parse-url (url-recreate-url internodeum/usage-url))))
    (if path (setf (url-filename url) path))
    url))

(defun internodeum/usage-data (url &optional leave-buffer)
  (let* ((url-request-extra-headers
          `(("Authorization" . ,(internodeum/creds-basic-auth (internodeum/determine-creds)))))
         (buffer (url-retrieve-synchronously url))
         (search-term "<?xml ")
         start data)
    (unless buffer
      (error "Error opening URL %s" url))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (setq start (search-forward search-term nil t))
      (unless start
        (error "No XML found at URL %s" url))
      (decf start (length search-term))
      (setq data (car (xml-parse-region start (point-max))))
      (unless leave-buffer
        (kill-buffer buffer)))
    data))

(defun* internodeum/xml-traverse (function node &key leaves)
  "Breadth-first traversal of all nodes in a parsed XML tree"
  (funcall function node)
  (if (listp node)
      (let ((children (xml-node-children node)))
        (if (listp children)
            (dolist (n children)
              (if (or (listp n) leaves)
                  (internodeum/xml-traverse function n :leaves leaves)))
          (if leaves (funcall function children))))))

(defun internodeum/xml-predicator (predicate)
  "Allow for predicates that are just a symbol of the node name"
  (case (type-of predicate)
    ('symbol
     (lexical-let ((pred predicate)) ;; form a closure over pred
       (lambda (x) (and (listp x) (eq (xml-node-name x) pred)))))
    (t
     predicate)))

(defun* internodeum/xml-accumulate (predicate node &key leaves)
  "Collect a list of all nodes in a parsed XML tree that match predicate"
  (lexical-let (result)
    (setq predicate (internodeum/xml-predicator predicate))
    (internodeum/xml-traverse
     (lambda (x)
       (if (funcall predicate x)
           (push x result)))
     node :leaves leaves)
    result))

(defun internodeum/lookup (symbol alist)
  "Obtain the value for a key in an alist"
  (cdr (assoc symbol alist)))

(defun* internodeum/format-volume (bytes &key (out-unit 'GB) (in-unit 'bytes) (precision 2) (iso t) omit-out-unit)
  "Pretty print a possibly large number of bytes"
  (dolist (i '(in-unit out-unit))
    (when (eq (type-of (symbol-value i)) 'string)
      (set i (intern (symbol-value i)))))

  ;; FIXME: make sure these symbols are what can come out of the internode API
  ;; I've only seen bytes so far
  (let* ((multipliers '(bytes KB MB GB TB))
         (power (- (position out-unit multipliers) (position in-unit multipliers)))
         (divisor (if iso 1000.0 1024.0))
         pieces)
    (dotimes (i power)
      (setq bytes (/ bytes divisor)))
    (setq pieces (list "%." (format "%d" precision) "f"))
    (unless omit-out-unit
      (setq pieces (append pieces (list " " (symbol-name out-unit)))))
    (format (apply 'concat pieces) bytes)))

(defun internodeum/xform-traffic-data (traffic-data)
  "Reform the traffic data into a single alist of data about the service"
  (destructuring-bind (type traffic-alist used-str &rest rest) traffic-data
    (let* ((used (string-to-number used-str))
           (quota (string-to-number (internodeum/lookup 'quota traffic-alist)))
           (unit (internodeum/lookup 'unit traffic-alist))
           (rollover (internodeum/lookup 'rollover traffic-alist))
           (plan-interval (internodeum/lookup 'plan-interval traffic-alist))
           (remaining (- quota used))
           (summary (format "%s %s plan, %s used and %s remaining out of %s %s until %s"
                            plan-interval
                            type
                            (internodeum/format-volume used :in-unit unit)
                            (internodeum/format-volume remaining :in-unit unit)
                            (internodeum/format-volume quota :in-unit unit)
                            unit
                            rollover)))

      (mapcar (lambda (var) (cons var (symbol-value var)))
              '(summary remaining used quota unit rollover plan-interval type)))))

(defun internodeum/get-usage (&optional service-type)
  (unless service-type
    (setq service-type internodeum/service-type))
  (let* ((services-data (internodeum/usage-data (internodeum/usage-url)))
         result type)
    (dolist (service (internodeum/xml-accumulate 'service services-data))
      (setq type (xml-get-attribute service 'type))
      (when (or (not service-type) (equal service-type type))
        (let* ((service-href (xml-get-attribute-or-nil service 'href))
               (service-url (internodeum/usage-url service-href))
               (service-data (internodeum/usage-data service-url))
               (usage-resource (car (internodeum/xml-accumulate
                                     (lambda (x)
                                       (and (eq (xml-node-name x) 'resource)
                                            (string= (xml-get-attribute-or-nil x 'type) "usage")))
                                     service-data)))
               (usage-url (internodeum/usage-url (xml-get-attribute-or-nil usage-resource 'href)))
               (usage-data (internodeum/usage-data usage-url))
               (traffic-data (car (internodeum/xml-accumulate 'traffic usage-data))))

          (push (cons type (internodeum/xform-traffic-data traffic-data)) result))))
    result))


;;; Summary

(defun internodeum/usage-summary (&optional service-type inode-data)
  "Fetch and display a usage summary for an internode account. Accepts
an optional string denoting the service of interest, if omitted or
empty, all services are queried but only the summary of the first is
displayed. For my purposes a service-type of \"Personal_ADSL\" does
the job.

Returns an alist of alists. The outer alist is keyed by service type,
the inner alists contain the relevant data."
  (interactive "sService Type (press enter for all): ")
  (when (and service-type (zerop (length service-type)))
      (setq service-type nil))
  (unless inode-data
    (setq inode-data (internodeum/get-usage service-type)))
  (let ((inode-data (internodeum/get-usage service-type))
        service-data)
    (setq service-data
          (if service-type
              (cdr (assoc service-type inode-data))
            (cdar inode-data)))
    (message "Summary: %s" (cdr (assq 'summary service-data)))
    inode-data))

(defun internodeum/short-usage-summary (&optional service-type inode-data)
  (interactive "sService Type (press enter for all): ")
  (when (and service-type (zerop (length service-type)))
      (setq service-type nil))
  (unless inode-data
    (setq inode-data (internodeum/get-usage service-type)))
  (let (service-data)
    (setq service-data
          (if service-type
              (cdr (assoc service-type inode-data))
            (cdar inode-data)))
    (message "Summary: %s" (cdr (assq 'summary service-data)))
    inode-data))


;;; Polling

(defun internodeum/poll (&optional pre-poll-hook post-poll-hook)
  (when (and pre-poll-hook (funcall pre-poll-hook))
    (when (setq internodeum/poll-latest-stats
                (internodeum/get-usage internodeum/poll-service-type))
      (when post-poll-hook
        (funcall post-poll-hook))))
  ;; (message (concat "polled at " (current-time-string)))
  )

(defun internodeum/poll-start (&optional pre-poll-hook post-poll-hook)
  "Start polling Internode's usage service.
Returns t when polling was not running and was started, nil otherwise."
  (interactive)
  (catch 'done
    (when internodeum/poll-timer
      (throw 'done nil))

    (unless internodeum/credentials
      (unless (call-interactively 'internodeum/determine-creds)
        (message "No credentials found for Internode")
        (throw 'done nil)))

    (unless pre-poll-hook
      (setq pre-poll-hook
            (lambda () t)
            ;; (lambda ()
            ;;   (and current-idle-time (> current-idle-time 10))
            ;;   (message "pre-poll-hook, current-idle-time is %s" current-idle-time))
            ))

    (unless post-poll-hook
      (setq post-poll-hook (lambda ()
                             (internodeum/update-mode-line))))

    (add-to-list 'global-mode-string "")
    (add-to-list 'global-mode-string 'internodeum/poll-latest-summary t)
    (setq internodeum/poll-timer (run-at-time nil internodeum/poll-interval
                                            'internodeum/poll pre-poll-hook post-poll-hook))
    ;; (message "Set timer: %s" internodeum/poll-timer)
    ;; (force-mode-line-update)
    t))

(defun internodeum/poll-stop ()
  "Stop polling Internode's usage service.
Returns t when polling was active, nil otherwise."
  (interactive)
  (let ((result
         (if internodeum/poll-timer
             (progn
               (cancel-timer internodeum/poll-timer)
               (setq internodeum/poll-timer nil)
               t)
           nil)))
    (setq global-mode-string (delq 'internodeum/poll-latest-summary global-mode-string))
    (force-mode-line-update)
    result))

;;; Display

(defun internodeum/update-mode-line ()
  "Format and set the poll summary string"
  (let ((stats (cdar internodeum/poll-latest-stats)))
    (setq internodeum/poll-latest-summary
          (concat "["
                  (internodeum/format-volume (internodeum/lookup 'used stats) :omit-out-unit t)
                  "/"
                  (internodeum/format-volume (internodeum/lookup 'quota stats))
                  " til "
                  (internodeum/lookup 'rollover stats)
                  "]"
                  )
          )

      )
  (force-mode-line-update)
  ;; clear the message area after of garbage from the URL fetches
  (message ""))


(provide 'internode-mum)
;;; internode-mum.el ends here

