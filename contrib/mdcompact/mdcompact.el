;;; -*- lexical-binding: t; -*-

;; Author: Andrea Corallo <andrea.corallo@arm.com>
;; Package: mdcompact
;; Keywords: languages, extensions
;; Package-Requires: ((emacs "29"))

;; This file is part of GCC.

;; GCC is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Convert multi choice GCC machine description patterns to compact
;; syntax.

;;; Usage:

;; With the point on a pattern run 'M-x mdcomp-run-at-point' to
;; convert that pattern.

;; Run 'M-x mdcomp-run-buffer' to convert all convertible patterns in
;; the current buffer.

;; Run 'M-x mdcomp-run-directory' to convert all convertible patterns
;; in a directory.

;; One can invoke the tool from shell as well, ex for running it on
;; the arm backend from the GCC checkout directory:
;; emacs -batch -l ./contrib/mdcompact/mdcompact.el -f mdcomp-run-directory ./gcc/config/arm/

;;; Code:

(require 'cl-lib)
(require 'rx)

(defconst
  mdcomp-constr-rx
  (rx "(match_operand" (? ":" (1+ (or punct alnum)))
      (1+ space) (group-n 1 num) (1+ space) "\""
      (1+ (or alnum "_" "<" ">")) "\""
      (group-n 2 (1+ space) "\"" (group-n 3 (0+ (not "\""))) "\"")
      ")"))

(cl-defstruct mdcomp-operand
  num
  cstr)

(cl-defstruct mdcomp-attr
  name
  vals)

;; A reasonable name
(rx-define mdcomp-name (1+ (or alnum "_")))

(defconst mdcomp-attr-rx
  (rx "(set_attr" (1+ space) "\""
      (group-n 1 mdcomp-name)
      "\"" (1+ space) "\""
      (group-n 2 (1+ (not ")")))
      "\"" (0+ space) ")"))

(defun mdcomp-parse-delete-attr ()
  (save-match-data
    (when (re-search-forward mdcomp-attr-rx nil t)
      (let ((res (save-match-data
		   (make-mdcomp-attr
		    :name (match-string-no-properties 1)
		    :vals (cl-delete-if #'string-empty-p
					(split-string
					 (replace-regexp-in-string
					  (rx "\\") ""
					  (match-string-no-properties 2))
						      (rx (1+ (or space ",")))))))))
	(if (length= (mdcomp-attr-vals res) 1)
	    'short
	  (delete-region (match-beginning 0) (match-end 0))
	  res)))))

(defun mdcomp-parse-attrs ()
  (save-excursion
    (let* ((res (cl-loop for x = (mdcomp-parse-delete-attr)
			 while x
			 collect x))
	   (beg (re-search-backward (rx bol (1+ space) "["))))
      (unless (memq 'short res)
	(when res
	  (delete-region beg (re-search-forward (rx "]")))))
      (cl-delete 'short res))))

(defun mdcomp-remove-quoting (beg)
  (save-excursion
    (save-match-data
      (replace-regexp-in-region (regexp-quote "\\\\") "\\\\" beg (point-max))
      (replace-regexp-in-region (regexp-quote "\\\"") "\"" beg (point-max)))))

(defun mdcomp-remove-escaped-newlines (beg)
  (save-excursion
    (save-match-data
      (replace-regexp-in-region (rx "\\" eol (0+ space)) " " beg (point-max)))))

(defun mdcomp-parse-delete-cstr ()
  (cl-loop while (re-search-forward mdcomp-constr-rx nil t)
	   unless (string= "" (match-string-no-properties 3))
	     collect (save-match-data
		       (make-mdcomp-operand
			:num (string-to-number (match-string-no-properties 1))
			:cstr (cl-delete-if #'string-empty-p
					    (split-string
					     (replace-regexp-in-string " " ""
								       (match-string-no-properties 3))
					     (rx (1+ ","))))))
	   do (delete-region (match-beginning 2) (match-end 2))))

(defun mdcomp-run* ()
  (let* ((ops (mdcomp-parse-delete-cstr))
	     (attrs (mdcomp-parse-attrs))
	     (beg (re-search-forward "\"@")))
	(cl-sort ops (lambda (x y)
		       (< (mdcomp-operand-num x) (mdcomp-operand-num y))))
	(mdcomp-remove-escaped-newlines beg)
	(save-match-data
	  (save-excursion
	    (left-char 2)
	    (forward-sexp)
	    (left-char 1)
	    (delete-char 1)
	    (insert "\n  }")))
	(mdcomp-remove-quoting beg)
	(replace-match "{@")
	(re-search-forward (rx (or "\"" ")")))
	(re-search-backward "@")
	(right-char 1)
	(insert "[ cons: ")
	(cl-loop
	 for op in ops
	 when (string-match "=" (cl-first (mdcomp-operand-cstr op)))
	 do (insert "=")
	 do (insert (number-to-string (mdcomp-operand-num op)) ", ")
	 finally
	 (progn
	   ;; In case add attributes names
	   (when attrs
	     (delete-char -2)
	     (insert "; attrs: ")
	     (cl-loop for attr in attrs
		      do (insert (mdcomp-attr-name attr) ", ")))
	   (delete-char -2)
	   (insert "]")))
	(cl-loop
	 while (re-search-forward (rx bol (0+ space) (or (group-n 1 "* return")
							 (group-n 2 "}")
							 "#" alpha "<"))
				  nil t)
	 for i from 0
	 when (match-string 2)
	   do (cl-return)
	 when (match-string 1)
	   do (progn
		(delete-region (match-beginning 1) (+ (match-beginning 1) (length "* return")))
		(insert "<<")
		(left-char 1))
	 do
	 (progn
	   (left-char 1)
	   (cl-loop
	    initially (insert " [ ")
	    for op in ops
	    for c = (nth i (mdcomp-operand-cstr op))
	    unless c
	      do (cl-return)
	    do (insert (if (string-match "=" c)
			   (substring c 1 nil)
			 c)
		       ", ")
	    finally (progn
		      (when attrs
			(delete-char -2)
			(insert "; ")
			(cl-loop for attr in attrs
				 for str = (nth i (mdcomp-attr-vals attr))
				 when str
				   do (insert str)
				 do (insert ", ")))
		      (delete-char -2)
		      (insert " ] ")
		      (move-end-of-line 1)))))
	;; remove everything after ] align what needs to be aligned
	;; and re-add the asm template
	(re-search-backward (regexp-quote "@[ cons:"))
	(let* ((n (length (mdcomp-operand-cstr (car ops))))
	       (asms (cl-loop
		      initially (re-search-forward "]")
		      repeat n
		      collect (let* ((beg (re-search-forward "]"))
				     (end (re-search-forward (rx eol)))
				     (str (buffer-substring-no-properties beg end)))
				(delete-region beg end)
				str)))
	       (beg (re-search-backward (regexp-quote "@[ cons:")))
	       (indent-tabs-mode nil))
	  (re-search-forward "}")
	  (align-regexp beg (point) (rx  (group-n 1 "") "["))
	  (align-regexp beg (point) (rx  (group-n 1 "") (or "," ";")) nil nil t)
	  (align-regexp beg (point) (rx  (group-n 1 "") "]"))
	  (goto-char beg)
	  (cl-loop
	   initially (re-search-forward "]")
	   for i below n
	   do (progn
		(re-search-forward "]")
		(insert (nth i asms))))
	  (when (re-search-forward (rx (1+ (or space eol)) ")") nil t)
	    (replace-match "\n)" nil t)))))

(defun mdcomp-narrow-to-md-pattern ()
  (condition-case nil
      (let ((beg (re-search-forward "\n("))
	    (end (re-search-forward (rx bol (1+ ")")))))
	(narrow-to-region beg end))
    (error
     (narrow-to-defun))))

(defun mdcomp-run-at-point ()
  "Convert the multi choice top-level form around point to compact syntax."
  (interactive)
  (save-restriction
    (save-mark-and-excursion
      (mdcomp-narrow-to-md-pattern)
      (goto-char (point-min))
      (let ((pattern-name (save-excursion
			    (re-search-forward (rx "\"" (group-n 1 (1+ (not "\""))) "\""))
			    (match-string-no-properties 1)))
	    (orig-text (buffer-substring-no-properties (point-min) (point-max))))
	(condition-case nil
	    (progn
	      (mdcomp-run*)
	      (message "Converted: %s" pattern-name))
	  (error
	   (message "Skipping convertion for: %s" pattern-name)
	   (delete-region (point-min) (point-max))
	   (insert orig-text)
	   'fail))))))

(defun mdcomp-run-buffer ()
  "Convert the multi choice top-level forms in the buffer to compact syntax."
  (interactive)
  (save-excursion
    (message "Conversion for buffer %s started" (buffer-file-name))
    (goto-char (point-min))
    (while (re-search-forward
	    (rx "match_operand" (1+ any) letter (0+ space) "," (0+ space) letter) nil t)
      (when (eq (mdcomp-run-at-point) 'fail)
	(condition-case nil
	    (forward-sexp)
	  (error
	   ;; If forward-sexp fails falls back.
	   (re-search-forward (rx ")" eol eol))))))
    (message "Conversion done")))

(defconst mdcomp-file-rx (rx bol alpha (0+ not-newline) ".md" eol))

(defun mdcomp-run-directory (folder &optional recursive)
  "Run el mdcompact on a FOLDER possibly in a RECURSIVE fashion."
  (interactive "D")
  (let ((before-save-hook nil)
	(init-time (current-time)))
    (mapc (lambda (f)
	    (with-temp-file f
	      (message "Working on %s" f)
	      (insert-file-contents f)
	      (mdcomp-run-buffer)
	      (message "Done with %s" f)))
	  (if recursive
	      (directory-files-recursively folder mdcomp-file-rx)
	    (directory-files folder t mdcomp-file-rx)))
    (message "Converted in %f sec" (float-time (time-since init-time)))))

(defun mdcomp-batch-run-directory ()
  "Same as `mdcomp-run-directory' but use cmd line args."
  (mdcomp-run-directory (nth 0 argv) (nth 1 argv)))

(provide 'mdcompact)

;;; mdcompact.el ends here
