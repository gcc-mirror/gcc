;;; gccbug.el --- forward bug reports to gnats
;; (C) 2000 Free Software Foundation
;; Written by Martin v. Löwis
;; Usage:
;; In rmail, bind a key to rmail-gccbug-reply, e.g.
;;   (require 'rmail)
;;   (require 'gccbug)
;;   (define-key rmail-mode-map "R" 'rmail-gccbug-reply)
;; Then, when reviewing a report, type R to create a gnats-formatted
;; message.

(provide 'gccbug)

(defun gccbug-reply ()
  (interactive)
  (let ((orig-yank-prefix mail-yank-prefix))
    (insert ">Submitter-Id: net\n")
    (insert ">Originator: \n")
    (insert ">Confidential: no\n")
    (insert ">Synopsis: ")
    (save-excursion
      (mail-subject)
      (let ((stop (point)))
	(re-search-backward "Re: ")
	(copy-region-as-kill (match-end 0) stop)))
    (yank)
    (insert "\n")
    (insert ">Severity: serious\n")
    (insert ">Priority: medium\n")
    (insert ">Category: \n")
    (insert ">Class: \n")
    ;(insert ">State: analyzed\n")
    (insert ">Release: 2.95.2\n")
    (insert ">Environment:\n")
    (insert ">Description:\n")
    (set 'mail-yank-prefix nil)
    (set 'mail-yank-ignored-headers 
	 "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^X-.*:\\|^User-Agent:\\|^MIME-Version:\\|^Content-.*:\\|^List-.*:\\|C[Cc]:\\|^Precedence:\\|^Sender:\\|^Mailing-List:\\|^Delivered-To:\\|^>From")
    (mail-yank-original t)
    (set 'mail-yank-prefix orig-yank-prefix)
    ; Copy From: field to Originator:
    (re-search-backward "From: ")
    (let ((beg (match-end 0)))
      (end-of-line)
      (kill-region beg (point)))
    (re-search-backward ">Originator: ")
    (goto-char (match-end 0))
    (yank)
    ; Kill From: line
    (re-search-forward "From:")
    (beginning-of-line)
    (kill-line 1)
    ; Replace Message-ID: with Original-Message-ID
    (beginning-of-buffer)
    (re-search-forward "Message-ID: .*")
    (replace-match "Original-\\&")
    ; Replace To: line, adding recipient to Notify list
    (mail-to)
    (re-search-backward "To: ")
    (replace-match "To: gcc-gnats@gcc.gnu.org\nX-GNATS-Notify: ")
    ; add additional fields
    (end-of-buffer)
    (insert ">How-To-Repeat: \n>Fix: \n")
    ; See whether an Organization: is present
    (let ((org (re-search-backward "Organization:.*" nil t)))
      (if org
	  (progn 
	    (kill-region (point) (match-end 0))
	    (re-search-backward ">Confidential")
	    (insert ">")
	    (yank)
	    (insert "\n"))))
;     This kills CC: lines, but rmail-reply below won't create any
;     (mail-cc)
;     (beginning-of-line)
;     (kill-line 1)
    ))

(defun rmail-gccbug-reply ()
  (interactive)
  (rmail-toggle-header 0)
  (rmail-reply t)
  (gccbug-reply))

