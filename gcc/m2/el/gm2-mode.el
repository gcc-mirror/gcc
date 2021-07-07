;; Copyright (C) 1985-2021

;; Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; Modula-2 mode editing commands for Emacs

;; Author Gaius Mulley <gaius.mulley@southwales.ac.uk>
;;    keywords can be displayed in lowercase and optionally underlined.
;;    reserved procedures and functions displayed in lowercase bold and optionally italic.
;;    contains automatic indentation
;;    automatic END matching
;;    electric THEN, END, ELSE
;;    expression formatting
;;    comment formatting
;;    procedure declaration finding
;;    const, type, var declaration finding.
;;    word expansion

;; with statement expansion and module visiting lisp routines from:

;;    Mick Jordan
;;    amended by Peter Robinson
;;    ported to GNU Michael Schmidt <michael@pbinfo.UUCP>
;;    modified by Tom Perrine <Perrin@LOGICON.ARPA> (TEP)

(defgroup gm2 nil
  "Modula-2 formatting mode."
  :group 'programming)

(defcustom m2-indent-level 3
  "indentation of Modula-2 statements within a containing block."
  :type 'integer
  :group 'gm2)

(defcustom m2-auto-indent-on-end t
  "automatic indentation when END is typed."
  :type 'boolean
  :group 'gm2)

(defcustom m2-auto-indent-on-then t
  "automatic indentation when THEN is typed."
  :type 'boolean
  :group 'gm2)

(defcustom m2-auto-indent-on-else t
  "automatic indentation when ELSE is typed."
  :type 'boolean
  :group 'gm2)

(defcustom m2-auto-use-algol-style t
  "use the algol style type faces, which displays keywords and reserved
   types and functions in lowercase."
  :type 'boolean
  :group 'gm2)

(defcustom m2-auto-keywords-underlined t
  "keywords should be underlined, probably not wanted if you are not using
   algol style, see m2-auto-use-algol-style."
  :type 'boolean
  :group 'gm2)

(defcustom m2-auto-functions-italic nil
  "reserved functions should be rendered as italic, probably not
   wanted if you are not using algol style, see
   m2-auto-use-algol-style."
  :type 'boolean
  :group 'gm2)

(defcustom m2-auto-default-gm2-extensions t
  "true if the GNU Modula-2 compiler is being used.  ASM,
   VOLATILE and UNQUALIFIED keywords are highlighted."
  :type 'boolean
  :group 'gm2)

(defcustom m2-auto-default-dialect 'pim
  "the default dialect of Modula-2 to be rendered.  Only used if
   no explicit dialect tag is in the first n lines of the file.
   The choices are pim (1985), iso (1995) and r10 (2010)."
  :type '(radio
	  (const pim)
	  (const iso)
	  (const r10))
  :group 'gm2)

(defcustom m2-dialect-comment-search-limit 200
  "the maximum number of lines to search at the top of the source
   file in order to find the dialect marker."
  :type 'integer
  :group 'gm2)

(defcustom m2-assign-future nil
  "useful for GNU developers, set this to nil if you have not
   assigned all future code to the fsf or set to t if you
   have.  If you have assigned all future code to the FSF then
   this is useful as it will automatically generate the FSF
   copyright and warranty disclaimer in any new module."
  :type 'boolean
  :group 'gm2)

(defcustom m2-compile-default-path "."
  "the compile include path to find the library def and mod
   files.  This is a UNIX style path, each directory is
   separated by a :"
  :type 'string
  :group 'gm2)

(defcustom m2-options "-g"
  "various compile time options used by the compiler."
  :type 'string
  :group 'gm2)

(defcustom m2-compile-command-default "gm2 -c"
  "command to compile Modula-2 programs"
  :type 'string
  :group 'gm2)

(defun m2-auto-get-compile-command ()
  "returns the compile command and options."
  (interactive)
  (progn
    (concat m2-compile-command-default " " m2-options)))

(defcustom m2-link-command-default "gm2 -fonlylink"
  "command to link Modula-2 programs"
  :type 'string
  :group 'gm2)

(defun m2-auto-get-link-command ()
  "returns the link command and options."
  (interactive)
  (progn
    (concat m2-link-command-default " " m2-options)))

(defvar m2-link-name nil
  "Name of the executable.")

(defconst m2-if-then-same-line nil
  "set this to t if you like the THEN on the same line as IF,
   or set it to nil if you place THEN on the next line.")

(defvar m2-dialect-known nil
  "is the dialect known yet.  The mode will examine the first
   m2-dialect-comment-search-limit lines for a special marker
   indicating dialect.")

(defvar m2-dialect nil
  "the dialect list which can contain item tags such as gm2 m2iso
   m2pim m2r10.  For example a (*!m2iso+gm2*) or (*!m2pim+gm2*)
   or (*!m2r10*) which specifies a dialect and with/without GNU
   Modula-2 extensions.")

(defvar m2-auto-abbrev-table nil
  "Abbrev table in use in gm2-mode buffers.")
(define-abbrev-table 'm2-auto-abbrev-table ())

(defvar m2-auto-map ()
  "Keymap used in M2 mode.")

(defun m2-auto-convert-unix-to-emacs-path (path)
  "convert a UNIX style path into a list"
  (interactive)
  (progn
    (let (epath)
      (setq epath nil)
      (let (i)
	(setq i 0)
	(let (l)
	  (setq l (length path))
	  (let (start)
	    (setq start 0)
	    (while (< i l)
	      (progn
		(if (string-equal (substring path i (+ i 1)) ":")
		    (progn
		      (add-to-list 'epath (substring path start i))
		      (setq start (+ i 1))))
		(setq i (+ i 1))))
	    (if (< start l)
		(add-to-list 'epath (substring path start l))))))
      epath)))

(defvar m2-auto-compile-default-path-emacs
  (m2-auto-convert-unix-to-emacs-path m2-compile-default-path)
  "this is an internal list.")

(defun setup-m2-auto-keys ()
  "sets up the keymap for gm2-mode."
  (setq m2-auto-map (make-sparse-keymap))
  (define-key m2-auto-map ")" 'm2-close-paren)
  (define-key m2-auto-map "\t" 'm2-tab)
  (define-key m2-auto-map "D" 'm2-test-end)
  (define-key m2-auto-map "N" 'm2-test-then)
  (define-key m2-auto-map "E" 'm2-test-else)
;;  (define-key m2-auto-map "%" 'm2-local-test)
;;  (define-key m2-auto-map "!" 'm2-local-recompile)
  (define-key m2-auto-map (kbd "DEL") 'm2-backspace)
  (define-key m2-auto-map "\C-d" 'm2-delete)
  (define-key m2-auto-map (kbd "<delete>") 'm2-delete)
  (define-key m2-auto-map "\e."   'm2-tag)
  (define-key m2-auto-map "\e\t"  'm2-complete)
  (define-key m2-auto-map "\C-cb" 'm2-begin)
  (define-key m2-auto-map "\C-cc" 'm2-case)
  (define-key m2-auto-map "\C-cd" 'm2-definition)
  (define-key m2-auto-map "\C-ce" 'm2-else)
  (define-key m2-auto-map "\C-cf" 'm2-for)
  (define-key m2-auto-map "\C-ch" 'm2-header)
  (define-key m2-auto-map "\C-ci" 'm2-if)
  (define-key m2-auto-map "\C-cm" 'm2-module)
  (define-key m2-auto-map "\C-cl" 'm2-loop)
  (define-key m2-auto-map "\C-co" 'm2-or)
  (define-key m2-auto-map "\C-cp" 'm2-procedure)
  (define-key m2-auto-map "\C-c\C-w" 'm2-with)
  (define-key m2-auto-map "\C-c\C-e" 'm2-elsif)
  (define-key m2-auto-map "\C-cr" 'm2-record)
  (define-key m2-auto-map "\C-cs" 'm2-stdio)
  (define-key m2-auto-map "\C-ct" 'm2-type)
  (define-key m2-auto-map "\C-cu" 'm2-until)
  (define-key m2-auto-map "\C-cv" 'm2-var)
  (define-key m2-auto-map "\C-cw" 'm2-while)
  (define-key m2-auto-map "\C-cx" 'm2-export)
  (define-key m2-auto-map "\C-cy" 'm2-import)
  (define-key m2-auto-map "\C-c\C-h" 'm2-help)
  (define-key m2-auto-map "\C-c\C-z" 'suspend-emacs)
  (define-key m2-auto-map "\C-c\C-v" 'm2-visit)
  (define-key m2-auto-map "\C-c\C-t" 'm2-toggle)
  (define-key m2-auto-map "\C-c\C-l" 'm2-link)
  (define-key m2-auto-map "\C-c\C-d" 'm2-debug)
  (define-key m2-auto-map "\C-c\C-a" 'm2-assembler)
  (define-key m2-auto-map "\C-c\C-c" 'm2-compile))

(defun looking-at-keyword (regexp)
  "return t if the cursor is matching regexp."
  (interactive)
  (progn
    (m2-auto-restore-upper-case-region (line-beginning-position) (line-end-position))
    (looking-at regexp)))

(defun re-search-backward-keyword (regexp &optional bound noerror count)
  "return t if the cursor is matching regexp when searching backwards."
  (interactive)
  (progn
    (m2-auto-restore-upper-case-region (point-min) (point-max))
    (re-search-backward regexp bound noerror count)))

(defun re-search-forward-keyword (regexp &optional bound noerror count)
  "return t if the cursor is matching regexp when searching backwards."
  (interactive)
  (progn
    (m2-auto-restore-upper-case-region (point-min) (point-max))
    (re-search-forward regexp bound noerror count)))

(defun m2-close-paren ()
  "Insert a close parenthesis and call m2-match-parenthesis."
  (interactive)
  (insert ")")
  (m2-match-parenthesis))

(defun m2-match-parenthesis ()
  "Match the current character according to the syntax table."
  (interactive)
  (save-excursion
    (progn
      (let (count)
	(setq count 1)
	(forward-char -1)
	(if (> (point) (point-min))
	    (progn
	      (forward-char -1)
	      (while (and (> (point) (point-min))
			  (not (= count 0)))
		(progn
		  (if (looking-at "(")
		      (setq count (- count 1))
		    (if (looking-at ")")
			(setq count (+ count 1))))
		  (if (not (= count 0))
		      (m2-backward-to-token)))))))
      (if (looking-at "(")
	  (sit-for 1)
	(message "No matching (")))))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun m2-tag ()
  "m2-tag finds the declaration of the modula-2 symbol the cursor is on."
  (interactive)
  (setq case-fold-search nil)
  (save-excursion
    (while (looking-at "[A-Za-z0-9]")
      (forward-char -1))
    (forward-char 1)
    (let (m2-start)
      (setq m2-start (point))
      (while (looking-at "[A-Za-z0-9]")
	(forward-char 1))
      (let (m2-end)
	(setq m2-end (point))
	(let (m2-object)
	  (setq m2-object (buffer-substring m2-start m2-end))
	  (if (not (m2-find-declaration m2-object))
	      (message "cannot find declaration of: %s" m2-object)))))))

(defun m2-complete ()
  "m2-complete - expands the string the cursor is currently on."
  (interactive)
  (setq case-fold-search nil)
  (forward-char -1)
  (while (looking-at "[A-Za-z0-9]")
    (forward-char -1))
  (forward-char 1)
  (let (m2-start)
    (setq m2-start (point))
    (while (looking-at "[A-Za-z0-9]")
      (forward-char 1))
    (let (m2-end)
      (setq m2-end (point))
      (m2-find-previous-string (buffer-substring m2-start m2-end)))))

(defun m2-find-previous-string (m2-object)
  "searches for a previous m2-object and inserts the remaining characters."
  (interactive)
  (save-excursion
    (message "searching for %s" m2-object)
    (let (m2-insert)
      (setq m2-insert (point))
      (forward-char (- 0 (length m2-object)))
      (if (search-backward m2-object (point-min) t nil)
	  (progn
	    (forward-char (length m2-object))
	    (let (m2-start)
	      (setq m2-start (point))
	      (while (looking-at "[A-Za-z0-9]")
		(forward-char 1))
	      (let (m2-end)
		(setq m2-end (point))
		(goto-char m2-insert)
		(insert (buffer-substring m2-start m2-end)))))
	(progn
	  (message "cannot complete: %s" m2-object)))))
  (while (looking-at "[A-Za-z0-9]")
    (forward-char 1)))

(defun m2-find-declaration (m2-object)
  "searches for object locally and then searches externally for object."
  (interactive)
  (let (source-window)
    (let (source-buffer)
      (setq source-window (selected-window))
      (setq source-buffer (current-buffer))
      (delete-other-windows)
      (split-window)
      (if (m2-find-declaration-procedure m2-object)
	  (progn
	    (m2-move-to-procedure-start)
	    (other-window 1)
	    t)
	(if (m2-find-type-or-var m2-object)
	    (progn
	      (m2-move-to-type-or-var-start)
	      (other-window 1)
	      t)
	  (if (m2-find-import-declaration m2-object ".mod")
	      (if (m2-find-declaration m2-object)
		  (progn
		    (switch-to-buffer source-buffer)
		    t)
		(progn
		  (delete-other-windows)
		  (select-window source-window)
		  (switch-to-buffer source-buffer)
		  (split-window)
		  (if (m2-find-import-declaration m2-object ".def")
		      (progn
			(m2-find-declaration m2-object)
			t)
		    (progn
		      (message "not found in the definition module")
		      (sit-for 1)
		      nil))))
	    (progn
	      (message (concat "cannot find declaration of " m2-object))
	      (sit-for 1)
	      nil)))))))

(defun m2-find-type-or-var (m2-object)
  "searches for an object defined as CONST, TYPE or VAR."
  (interactive)
  (goto-char (point-min))
  (let (m2-value)
    (setq m2-value nil)
    (while (and (not m2-value)
		(re-search-forward-keyword "TYPE\\|CONST\\|VAR" nil t))
      (progn
	(m2-forward-to-token)
	(while (and (not (looking-at-keyword "BEGIN\\|END"))
		    (not (looking-at m2-object)))
	  (m2-forward-to-token))
	(if (looking-at m2-object)
	    (progn
	      (m2-forward-to-token)
	      (if (looking-at "[=:,]")
		  (setq m2-value t))))))
    m2-value))

(defun m2-move-to-type-or-var-start ()
  "moves to the start of the CONST, TYPE or VAR declaration."
  (beginning-of-line)
  (let (m2-point)
    (setq m2-point (point))
    (skip-chars-backward " \t\n\f" (point-min))
    (re-search-backward-keyword "TYPE\\|CONST\\|VAR" nil t)
    (set-window-start (selected-window) (point))
    (goto-char m2-point)))

(defun m2-move-to-procedure-start ()
  "moves to the start of the procedure implementation (before the
							      comments start - if they exist)."
  (beginning-of-line)
  (let (m2-point)
    (setq m2-point (point))
    (skip-chars-backward " \t\n\f" (point-min))
    (if (and (> (point) 2)
	     (save-excursion (forward-char -2) (looking-at "\\*)")))
	(progn
	  (m2-run-back-over-comments (point-min))
	  (forward-char 1)))
    (set-window-start (selected-window) (point))))

(defun m2-find-declaration-procedure (m2-object)
  "attempts to find the declaration of m2-object as a procedure."
  (interactive)
  (goto-char (point-min))
  (let (m2-value)
    (setq m2-value nil)
    (setq m2-object (concat m2-object " "))
    (while (and (not m2-value)
		(re-search-forward-keyword "PROCEDURE" nil t))
      (progn
	(m2-forward-to-token)
	(if (and (looking-at m2-object) (not (m2-is-forward-declaration)))
	    (setq m2-value t))))
    m2-value))

(defun m2-is-forward-declaration ()
  "returns true if this procedure heading is just a FORWARD declaration
  of a implementation further down the file."
  (interactive)
  (save-excursion
    (m2-forward-to-token)
    (if (looking-at "(")
	(progn
	  (while (and (< (point) (point-max))
		      (not (looking-at ")")))
	    (m2-forward-to-token))))
    (while (and (< (point) (point-max))
		(not (looking-at ";")))	;
  (m2-forward-to-token))
    (m2-forward-to-token)
    (looking-at-keyword "FORWARD")))

(defun m2-find-import-declaration (m2-object m2-extension)
  "scans the import list of the current module for m2-object.
   If m2-object is found then the appropriate module with
   m2-extension is opened and the cursor is placed at the
   start of the declaration."
  (interactive)
  (goto-char (point-min))
  (let (m2-success)
    (setq m2-success nil)
    (let (m2-module-name)
      (let (m2-continue)
	(setq m2-module-name nil)
	(setq m2-continue t)
	(while (and (< (point) (point-max))
		    (not m2-success)
		    m2-continue
		    (not (m2-indent-commencer)))
	  (progn
	    (if (re-search-forward-keyword "[ ;\n\t]\\(BEGIN\\|FINALLY\\|EXCEPT\\|CONST\\|TYPE\\|VAR\\|FROM\\|PROCEDURE\\)" nil t)
		(progn
		  (forward-char -1)
		  (while (looking-at "[A-Za-z0-9]")
		    (forward-char -1))
		  (forward-char 1)))
	    (if (looking-at-keyword "FROM")
		(progn
		  (m2-forward-until-white (point-max))
		  (m2-forward-to-token)
		  (let (m2-start)
		    (setq m2-start (point))
		    (while (looking-at "[A-Za-z0-9]")
		      (forward-char 1))
		    (setq m2-module-name (buffer-substring m2-start (point)))
		    (setq m2-success (m2-found-import-ident m2-object))
		    (if m2-success
			(progn
			  (message "found %s in module %s" m2-object m2-module-name)
			  (m2-find-module m2-module-name)))))
	      (progn
		(setq m2-continue (not (m2-indent-commencer)))
		(m2-forward-until-white (point-max))
		(m2-forward-to-token)))))))
    m2-success))

(defun m2-found-import-ident (m2-object)
  "scans the import current list for m2-object.
   If m2-object is found then true is returned."
  (interactive)
  (progn
    (let (m2-success)
      (setq m2-success nil)
      (while (and (not (looking-at-keyword ";\\|BEGIN\\|CONST\\|TYPE\\|VAR\\|FROM\\|PROCEDURE"))
		  (not m2-success))
	(progn
	  (setq m2-success (looking-at-keyword m2-object))
	  (m2-forward-to-token)))
      m2-success)))

(defun m2-help ()
  "displays the help buffer."
  (interactive)
  (let (m2-buffer)
    (setq m2-buffer (current-buffer))
    (if (get-buffer "*Modula-2-Help*")
	(kill-buffer "*Modula-2-Help*"))
    (switch-to-buffer "*Modula-2-Help*")

    (insert (concat
"This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing
Control-C followed by the first character of the construct.

  Control-c b begin         Control-c c case
  Control-c d definition    Control-c e else
  Control-c f for           Control-c h header
  Control-c i if            Control-c m module
  Control-c l loop          Control-c o or
  Control-c p procedure     Control-c Control-w with
  Control-c r record        Control-c s stdio
  Control-c t type          Control-c u until
  Control-c v var           Control-c w while
  Control-c x export        Control-c y import
  Control-c Control-d debug             Control-c Control-t toggle def/mod
  Control-c Control-c compile           Control-x ` next-error
  Control-c Control-l link              Esc .   find declaration
  Control-c Control-h help              Esc \\t  complete name

  You can customize the following variables using M-x customize and choosing gm2.

  m2-compile-command: " m2-compile-command-default "
  m2-link-command: " m2-link-command-default "
  m2-compile-default-path: " m2-compile-default-path "
  m2-options: " m2-options))
    (toggle-read-only)
    (goto-char (point-min))
    (delete-other-windows)
    (split-window)
    (switch-to-buffer m2-buffer)))

(defun m2-newline ()
  "insert a newline and tab to the correct indent."
  (interactive)
  (end-of-line)
  (insert "\n")
  (m2-tab))

(defun m2-begin ()
  "Insert a BEGIN keyword and indent for the next line."
  (interactive)
  (insert "BEGIN")
  (m2-newline)
  (m2-tab))

(defun m2-case ()
  "Build skeleton CASE statment, prompting for the <expression>."
  (interactive)
  (insert "CASE " (read-string "expression: ") " OF")
  (m2-tab)
  (m2-newline)
  (insert "\nEND")
  (m2-tab))

(defun m2-fsf-copyright ()
  "emit the a copyright notice providing m2-assign-future is set."
  (if m2-assign-future
      (insert "(* Copyright (C) 2020 Free Software Foundation, Inc.  *)\n")))

(defun m2-fsf-gpl-notice ()
  "emit the fsf gpl notice at the relevant position."
  (insert "(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)\n\n"))

(defun m2-fsf-lgpl-notice ()
  "emit the fsf notice at the relevant position."
  (insert "(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA  02110-1301  USA.  *)\n\n"))

(defun m2-fsf-notice ()
  "if the m2-assign-future is set then prompt the user for 'g'pl or 'l'gpl."
  (interactive)
  (if m2-assign-future
      (let ((type (read-string "which kind of license (g)GPL or (l)LGPL? ")))
	(if (string-equal type "l")
	    (m2-fsf-lgpl-notice)
	  (m2-fsf-gpl-notice)))))

(defun m2-dialect-tag ()
  "Insert m2 dialect comment tag."
  (interactive)
  (if m2-dialect-known
      (progn
	(if (m2-auto-dialect-pim)
	    (insert "(*!m2pim"))
	(if (m2-auto-dialect-iso)
	    (insert "(*!m2iso"))
	(if (m2-auto-dialect-r10)
	    (insert "(*!m2r10"))
	(if (m2-auto-dialect-gm2-extensions)
	    (insert "+gm2"))
	(insert "*)"))))

(defun m2-definition ()
  "Build skeleton DEFINITION MODULE, prompting for the <module name>."
  (interactive)
  (m2-fsf-copyright)
  (m2-fsf-notice)
  (insert "DEFINITION MODULE ")
  (insert (substring (buffer-name) 0 -4) " ;  ")
  (m2-dialect-tag)
  (insert "\n\n\n\nEND " (substring (buffer-name) 0 -4) ".\n")
  (previous-line 3)
  (m2-header))

(defun m2-else ()
  "Insert ELSE keyword and indent for next line."
  (interactive)
  (insert "ELSE")
  (m2-tab)
  (m2-newline))

(defun m2-for ()
  "Build skeleton FOR loop statment, prompting for the loop parameters."
  (interactive)
  (insert "FOR " (read-string "init: ") " TO " (read-string "end: "))
  (let ((by (read-string "by: ")))
    (if (not (string-equal by ""))
	(insert " BY " by)))
  (insert " DO")
  (m2-tab)
  (m2-newline)
  (insert "\nEND")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert "(*\n    Title      : ")
  (if (or (string-equal (substring (buffer-name) -4) ".def")
	  (string-equal (substring (buffer-name) -4) ".mod"))
      (insert (substring (buffer-name) 0 (- (length (buffer-name)) 4)))
    (insert (buffer-name)))
  (insert "\n    Author     : ")
  (insert (user-full-name))
;;;  (insert (concat "\n\t\t<" (user-login-name) "@" (system-name) ">\n"))
  (insert "\n    System     : GNU Modula-2")
  (insert "\n    Date       : ")
  (insert (current-time-string))
  (insert "\n    Revision   : $Version$ ")
  (insert "\n    Description: ")
  (insert "\n*)\n\n")
  (previous-line 3)
  (end-of-line))

(defun m2-if ()
  "Insert skeleton IF statment, prompting for <boolean-expression>."
  (interactive)
  (m2-tab)
  (if m2-if-then-same-line
      (insert "IF " (read-string "<boolean-expression>: ") " THEN")
    (progn
      (insert "IF " (read-string "<boolean-expression>: ") )
      (insert "\nTHEN")))
  (m2-tab)
  (m2-newline)
  (insert "\nEND")
  (m2-tab)
  (previous-line 1))

(defun m2-loop ()
  "Build skeleton LOOP (with END)."
  (interactive)
  (m2-tab)
  (insert "LOOP")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-module ()
  "Build skeleton MODULE, prompting for definition, implementation or module."
  (interactive)
  (m2-fsf-copyright)
  (m2-fsf-notice)
  (let ((type (read-string "(i)mplementation or (m)odule: ")))
    (if (string-equal type "i")
	(insert "IMPLEMENTATION "))
    (insert "MODULE " (substring (buffer-name) 0 -4) " ;  ")
    (m2-dialect-tag)
    (insert "\n\n\n")
    (if (string-equal type "m")
	(insert "BEGIN\n\n"))
    (insert "END " (substring (buffer-name) 0 -4) ".\n"))
  (previous-line 3))

(defun m2-or ()
  (interactive)
  (m2-newline)
  (insert "OR")
  (m2-newline)
  (m2-tab))

(defun m2-procedure ()
  (interactive)
  (insert "(*\n")
  (m2-indent-comment)
  (let ((name (read-string "Procedure name: " ))
	args)
    (insert name " - \n")
    (insert "*)")
    (m2-indent-comment)
    (end-of-line)
    (insert "\n\n")
    (insert "PROCEDURE ")
    (let (results)
      (setq args (read-string "Arguments: "))
      (setq results (read-string "Result Type: "))
      (insert name)
      (if (or (not (string-equal args ""))
	      (not (string-equal results "")))
	  (insert " (" args ")"))
      (if (not (string-equal results ""))
	  (insert " : " results))
      (insert " ;")
      (m2-newline)
      (insert "BEGIN")
      (m2-newline)
      (m2-newline)
      (insert "END ")
      (insert name)
      (insert " ;\n\n")
      (previous-line 2)
      (m2-tab)
      (previous-line 1)
      (m2-tab)
      (previous-line 5)
      (end-of-line))))

(defun m2-with ()
  (interactive)
  (m2-tab)
  (insert "WITH ")
  (insert (read-string "Designator: "))
  (insert " DO")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-elsif ()
  (interactive)
  (m2-tab)
  (insert "ELSIF ")
  (insert (read-string "<boolean expression>: "))
  (end-of-line)
  (m2-tab)
  (insert "\nTHEN")
  (m2-tab)
  (m2-newline))

(defun m2-record ()
  (interactive)
  (m2-tab)
  (insert "RECORD")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-stdio ()
  (interactive)
  (if (m2-auto-dialect-pim)
      (insert "
FROM StrIO IMPORT WriteString, ReadString, WriteLn ;
FROM StdIO IMPORT Write, Read ;
"))
  (if (m2-auto-dialect-iso)
      (insert "
FROM STextIO IMPORT WriteString, WriteLn, ReadString,
                    ReadChar, WriteChar,
                    ReadRestLine, ReadToken, SkipLine ;
"))
  (if (m2-auto-dialect-r10)
      (insert "
m2r10 imports go here
")))

(defun m2-type ()
  (interactive)
  (insert "TYPE")
  (m2-newline)
  (m2-tab))

(defun m2-until ()
  (interactive)
  (insert "REPEAT")
  (m2-tab)
  (m2-newline)
  (m2-newline)
  (m2-tab)
  (insert "UNTIL ")
  (insert (read-string "<expression>: "))
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-var ()
  (interactive)
  (m2-newline)
  (insert "VAR")
  (m2-newline)
  (m2-tab))

(defun m2-while ()
  (interactive)
  (m2-tab)
  (insert "WHILE ")
  (insert (read-string "<expression>: "))
  (insert " DO")
  (m2-newline)
  (m2-newline)
  (insert "END")
  (m2-tab)
  (previous-line 1)
  (m2-tab))

(defun m2-export ()
  (interactive)
  (insert "EXPORT QUALIFIED "))

(defun m2-import ()
  (interactive)
  (insert "FROM ")
  (insert (read-string "Module: "))
  (insert " IMPORT "))

(defun m2-debug ()
  (interactive)
  (gdb m2-debug-command))

(defun m2-compile ()
  (interactive)
  (compile (m2-auto-get-compile-command)))

(defun m2-assembler ()
  (interactive)
  (let (source-window)
    (let (source-buffer)
      (let (source-line)
        (let (m2-file)
          (setq m2-file (concat (substring (buffer-name) 0 -4) ".s"))
          (setq source-line (substring (what-line) 5 (length (what-line))))
          (delete-other-windows)
	  (split-window)
	  (if (not (or (get-buffer m2-file)
		       (find-file  m2-file)))
	      (progn
		(let (old-compile-command)
		  (setq old-compile-command compile-command)
		  (compile (generate-assembler-command m2-file))
		  (setq compile-command old-compile-command)))
	    (m2-search-for-line m2-file source-line)))))))

(defun m2-search-for-line (m2-file source-line)
  "search for the source code equivalent, source-line, in assembly file, m2-file"
  (interactive)
  (save-excursion
    (if (switch-to-buffer m2-file)
	(if (search-forward (concat ".stabn 68,0," source-line))
	    (progn
	      (beginning-of-line)
	      (while (and (< (point) (point-max))
			  (looking-at "^\\."))
		(next-line 1))))))
  (message "failed to compile module %s" (buffer-name)))

(defun generate-assembler-command (assembly-file)
  "generate the compilation string which will generate, assembly-file"
  (interactive)
  (if (string-equal "gm2" (substring compile-command 0 3))
      (concat "gm2 -S -g " (substring compile-command 4 (length compile-command)))
    ""))

(defun m2-link ()
  (interactive)
  (let (modulename)
    (setq modulename (buffer-name))
    (cond ((string-equal (substring (buffer-name) -4) ".def")
	   (compile (concat m2-link-command " "
			    (substring (buffer-name) 0 -4))))
	  ((string-equal (substring (buffer-name) -4) ".mod")
	   (compile (concat m2-link-command " "
			    (substring (buffer-name) 0 -4)))))))

(defun m2-visit ()
  (interactive)
  (let (modulename)
    (save-excursion
      (setq modulename
	    (read-string "Module name: "))
      (m2-find-module modulename))))

(defun m2-find-module (name)
  "attempts to find module, name, by searching the directories
   determined by m2-path."
  (progn
    (let (m2-found)
      (setq m2-found (locate-file name m2-auto-compile-default-path-emacs '(".def" ".mod" ".md" ".mi")))
      (if m2-found
	  (find-file m2-found))
      m2-found)))

(defun m2-toggle ()
  "Toggle between .mod and .def files for the module."
  (interactive)
  (cond ((string-equal (substring (buffer-name) -4) ".def")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4) ".mod")))
	((string-equal (substring (buffer-name) -4) ".mod")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4)  ".def")))
	((string-equal (substring (buffer-name) -3) ".mi")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".md")))
	((string-equal (substring (buffer-name) -3) ".md")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".mi")))))

(defun m2-tab ()
  "tab moves to the correct indentation for the current line."
  (interactive)
  (let (m2-point)
    (if (and (> (point) 1)
	     (save-excursion (forward-char -1) (looking-at "(\\*")))
	(progn
	  (setq m2-point 1)
	  (forward-char -1))
      (setq m2-point 0))
    (if (m2-in-parameter)
	(progn
	  (message "Formatting parameter block")
	  (m2-format-expression))
      (if (m2-in-expression)
	  (progn
	    (message "Formatting expression")
	    (m2-format-expression))
	(m2-indent))
      (forward-char m2-point))))

(defun m2-in-parameter ()
  "returns true if the cursor is currently inside a parameter block."
  (interactive)
  (let (m2-point)
    (setq m2-point (point))
    (save-excursion
      (m2-backward-to-token)
      (if (looking-at-keyword "PROCEDURE[ \n]")
	  nil
	(while (and (> (point) 1) (not (m2-is-parameter-commencer))
		    (not (m2-is-parameter-terminator)))
	  (m2-backward-to-token))
	(if (looking-at-keyword "PROCEDURE[ \n]")
	    (progn
	      (goto-char m2-point)
	      (while (not (m2-is-parameter-terminator))
		(m2-forward-to-token))
	      (looking-at ")"))
	  nil)))))

(defun m2-is-parameter-commencer ()
  "returns true if we are looking at a parameter block commencer"
  (looking-at-keyword "PROCEDURE[ \n]"))

(defun m2-is-parameter-terminator ()
  "returns true if we are looking at a parameter block terminator"
  (looking-at-keyword ")\\|BEGIN\\|IF\\|THEN\\|ELSE\\|END\\|WHILE\\|REPEAT\\|UNTIL\\|LOOP\\|CONST|\\MODULE"))

(defun m2-indent-comment ()
  "moves cursor to the current indentation of the comment."
  (interactive)
  (let (m2-point-end)
    (let (m2-indent)
      (let (m2-start-line)
	(save-excursion
	  (save-excursion
	    (beginning-of-line)
	    (setq m2-start-line (point)))
	  (m2-backward-to-comment-start)
	  (setq m2-point-end (point))
	  (beginning-of-line)
	  (setq m2-indent (- m2-point-end (point))))
	(m2-remove-leading-spaces)
	(if (and (not (looking-at "\\*)"))
		 (>= m2-start-line m2-point-end))
	    (setq m2-indent (+ m2-indent m2-indent-level)))
	(m2-create-indent m2-indent)))))

(defun m2-backward-to-comment-start ()
  "moves back to the start of a comment."
  (interactive)
  (let (m2-point)
    (let (carry-on)
      (if (and (> (point) 1)
	       (looking-at "\\*)"))
	  (forward-char -1))
      (setq m2-point (point))
      (setq carry-on t)
      (while carry-on
	(setq m2-point (point))
	(if (and (>= (point) 3)
		 (save-excursion
		   (forward-char -2)
		   (looking-at "\\*)")))
	    (m2-run-back-over-comments 1))
	(if (> (point) 1)
	    (forward-char -1))
	(setq carry-on (and (> (point) 1)
			    (not (looking-at "(\\*"))))))))

(defun m2-indent ()
  "calculates the indentation for the current line."
  (interactive)
  (let (m2-point)
    (save-excursion
      (m2-remove-leading-spaces))
    (setq m2-point (point))
    (beginning-of-line)
    (setq m2-point (- m2-point (point)))
    (m2-create-indent (m2-calculate-indent))
    (while (> m2-point 0)
      (forward-char 1)
      (setq m2-point (- m2-point 1)))))

(defun m2-remove-leading-spaces ()
  "removes any leading spaces in a source line."
  (interactive)
  (beginning-of-line)
  (if (looking-at "[\t| ]")
      (progn (delete-char 1)
	     (m2-remove-leading-spaces))))

(defun m2-create-indent (num)
  "generates num indent."
  (interactive)
  (while (> num 0)
    (progn
      (insert " ")
      (setq num (- num 1)))))

(defun m2-calculate-indent ()
  "calculates the indentation required for the current source line."
  (interactive)
  (let ((m2-level)
	(m2-found-record))
    (if (m2-indent-terminator)
	(setq m2-level (- m2-indent-level))
      (setq m2-level 0))
    (save-excursion
      (m2-backward-to-token)
      (while (and (> (point) 1)
		  (or (and (m2-in-parameter)
			   (looking-at-keyword "VAR"))
		      (not (or (m2-indent-terminator)
			       (m2-indent-commencer)
			       (looking-at-keyword "CASE")))))
	(m2-backward-to-token))
      (if (looking-at-keyword "CASE")
	  (setq m2-level  0)
	(if (m2-indent-commencer)
	    (setq m2-level  (+ m2-level m2-indent-level))))
      (setq m2-found-record nil)
      (if (looking-at-keyword "END")
	  (progn
	    (save-excursion
	      (m2-match-end)
	      (if (looking-at-keyword "RECORD")
		  (progn
		    (setq m2-level (+ m2-level (m2-calculate-indent)))
		    (setq m2-found-record t))))))
      (if (not m2-found-record)
	  (progn
	    (setq m2-level (+ m2-level (point)))
	    (beginning-of-line)
	    (setq m2-level (- m2-level (point))))))
    m2-level))

(defun m2-in-expression ()
  "returns true if we are currently inside a modula-2 expression."
  (interactive)
  (let (m2-level)
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[\t ]")
	  (progn
	    (m2-forward-to-token)
	    (if (looking-at ")")
		(progn
		  (forward-char -1)
		  (message "found )")))))
      (while (and (> (point) 1)
		  (not (or (m2-expression-commencer)
			   (m2-expression-terminator))))
	(if (looking-at ")")
	    (m2-skip-brackets ")" "(")
	  (m2-backward-to-token)))
      (setq m2-level (and (not (looking-at "(\\*"))
			  (m2-expression-commencer))))
    m2-level))

(defun m2-skip-brackets (m2-open m2-close)
  "skips the brackets () it stops at the next matching bracket."
  (interactive)
  (m2-backward-to-token)
  (while (and (> (point) 1)
	      (not (or (m2-expression-terminator)
		       (looking-at m2-close))))
    (if (looking-at m2-open)
	(m2-skip-brackets m2-open m2-close)
      (m2-backward-to-token))))

(defun m2-expression-backstop ()
  "returns true if we are looking at a Modula-2 keyword behind which
   an expression cannot exist."
  (interactive)
  (or (looking-at-keyword "UNTIL")
      (looking-at-keyword "WHILE")
      (looking-at-keyword "IF")
      (looking-at ":=")
      (looking-at-keyword "TO")
      (looking-at "(\\*")
      (looking-at-keyword "FOR")))

(defun m2-expression-commencer ()
  "returns true if we are currently looking at an expression commencer."
  (interactive)
  (looking-at "[-+*=(<>/#[]"))
;;; be VERY careful if you add anything to the above regexp
;;; test it against:
;;;    a := = b + - c * d : ; ) ( , < > . [ ] # / !

(defun m2-expression-terminator ()
  "returns true if we are currently looking at an expression terminator."
  (interactive)
  (or (looking-at-keyword "THEN")
      (looking-at-keyword "ELSE")
      (looking-at-keyword "ELSIF")
      (looking-at-keyword "END")
      (looking-at-keyword "VAR")
      (looking-at-keyword "TYPE")
      (looking-at-keyword "CONST")
      (looking-at-keyword "BEGIN")
      (looking-at-keyword "WITH")
      (looking-at-keyword "RECORD")
      (looking-at-keyword "PROCEDURE")
      (looking-at-keyword "OF")
      (looking-at-keyword "DO")
      (looking-at "[;:]")))

(defun m2-format-expression ()
  "formats the current expression."
  (interactive)
  (beginning-of-line)
  (let (m2-spaces)
    (let (m2-cursor)
      (let (m2-end)
	(let (m2-start)
	  (let (m2-start-exp)
	    (let (m2-next-tok)
	      (setq m2-cursor (point))
	      (save-excursion
		(m2-find-beginning-of-expression)
		(setq m2-start-exp (point))
		(save-excursion
		  (beginning-of-line)
		  (setq m2-start (point))
		  (end-of-line)
		  (setq m2-end (point)))
		(m2-forward-to-token)
		(setq m2-next-tok (point)))
	      (if (> m2-next-tok m2-end)
		  (progn
		    (setq m2-spaces (+ (- m2-start-exp m2-start) 1))
		    (goto-char m2-cursor)
		    (beginning-of-line)
		    (m2-remove-leading-spaces)
		    (if (looking-at "[\])]")
			(progn
			  (setq m2-spaces (- m2-start-exp m2-start))))
		    (if (or (m2-indent-commencer)
			    (m2-indent-terminator))
			(m2-indent)
		      (m2-create-indent m2-spaces)))
		(progn
		  (m2-remove-leading-spaces)
		  (if (looking-at "[\])]")
		      (progn
			(m2-create-indent (- m2-start-exp m2-start)))
		    (m2-create-indent (- m2-next-tok m2-start))))))))))))

(defun m2-find-beginning-of-expression ()
  "positions the cursor on the start of the current subexpression."
  (interactive)
  (let (m2-level)
    (setq m2-level 1)
    (while (and
	    (not (and (= m2-level 0)
		      (m2-expression-commencer)))
	    (not (m2-expression-backstop)))
      (m2-backward-to-token)
      (if (looking-at ")")
	  (setq m2-level (+ m2-level 1))
	(if (looking-at "(")
	    (setq m2-level (- m2-level 1)))))))

(defun m2-delete-indent-spaces ()
  "removes 3 spaces from the beginning of the line if they exist."
  (interactive)
  (m2-delete-space)
  (m2-delete-space)
  (m2-delete-space))

(defun m2-delete-space ()
  "removes a space from the beginning of the line if one exists."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\t")
	(progn
	  (delete-char 1)
	  (insert "       "))
      (if (looking-at " ")
	  (delete-char 1)))))

(defun m2-old-calculate-indent ()
  "calculates the indentation required for the current source line."
  (interactive)
  (let (m2-level)
    (save-excursion
      (setq m2-level 0)
      (while (> (point) 1)
	(if (m2-indent-terminator)
	    (setq m2-level (- m2-level 1)))
	(m2-backward-to-token)
	(if (m2-indent-commencer)
	    (setq m2-level (+ m2-level 1)))))
    (while (> m2-level 0)
      (progn
	(insert "   ")
	(setq m2-level (- m2-level 1))))))

(defun m2-indent-commencer ()
  "returns true if a token representing the beginning of an indent sequence was found."
  (or (and
       m2-if-then-same-line
       (looking-at-keyword "IF"))
      (and
       (not m2-if-then-same-line)
       (looking-at-keyword "THEN"))
      (looking-at-keyword "BEGIN")
      (looking-at-keyword "FINALLY")
      (looking-at-keyword "EXCEPT")
      (looking-at-keyword "RECORD")
      (looking-at-keyword "FOR")
      (looking-at-keyword "CONST")
      (looking-at-keyword "TYPE")
      (looking-at-keyword "VAR")
      (looking-at-keyword "WITH")
      (looking-at-keyword "LOOP")
      (looking-at-keyword "REPEAT")
      (looking-at-keyword "ELSE")
      (looking-at-keyword "WHILE")))

(defun m2-indent-terminator ()
  "returns true if a token representing the end of an indent sequence was found."
  (or (looking-at-keyword "END")
      (looking-at-keyword "BEGIN")
      (looking-at-keyword "FINALLY")
      (looking-at-keyword "EXCEPT")
      (looking-at-keyword "CONST")
      (looking-at-keyword "TYPE")
      (looking-at-keyword "VAR")
      (looking-at-keyword "ELSE")
      (looking-at-keyword "ELSIF")
      (looking-at-keyword "UNTIL")))

(defun m2-local-recompile ()
 "recompile the gm2-mode and load the file"
 (interactive)
 (byte-compile-file (concat (getenv "HOME") "/m2/comp/el/gm2-mode.el"))
 (read-abbrev-file  (concat (getenv "HOME") "/m2/comp/el/gm2-mode.elc"))
 (message "Compilation complete"))

(defvar m2-auto-syntax-table nil
  "Syntax table in use in gm2-mode buffers.")

(if m2-auto-syntax-table
    ()
  (setq m2-auto-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" m2-auto-syntax-table)
  (modify-syntax-entry ?/ ". 14" m2-auto-syntax-table)
  (modify-syntax-entry ?+ "." m2-auto-syntax-table)
  (modify-syntax-entry ?- "." m2-auto-syntax-table)
  (modify-syntax-entry ?= "." m2-auto-syntax-table)
  (modify-syntax-entry ?% "." m2-auto-syntax-table)
  (modify-syntax-entry ?< "." m2-auto-syntax-table)
  (modify-syntax-entry ?> "." m2-auto-syntax-table)
  (modify-syntax-entry ?& "." m2-auto-syntax-table)
  (modify-syntax-entry ?| "." m2-auto-syntax-table)
  (modify-syntax-entry ?\' "\"" m2-auto-syntax-table)
  ;; sadly cannot use the follow as it conflicts with comment
  ;; delimiters
  ;; (modify-syntax-entry ?\( "($" m2-auto-syntax-table)
  ;; (modify-syntax-entry ?\) ")^" m2-auto-syntax-table)
  ;; Modula-2, Pascal, Mathematica style comment: (* ... *)
  (modify-syntax-entry ?\( ". 1" m2-auto-syntax-table)
  (modify-syntax-entry ?\) ". 4" m2-auto-syntax-table)
  (modify-syntax-entry ?* ". 23" m2-auto-syntax-table))

(defun m2-test-end ()
  "check to see whether END has been typed"
  (interactive)
  (insert "D")
  (setq case-fold-search nil)
  (if (and (not (m2-is-in-string))
	   (> (point) 4))
      (save-excursion
	(forward-char -4)
	(if (looking-at-keyword "[; \t\n]END[; \t\n]")
	    (progn
	      (m2-found-end))))))

(defun m2-found-end ()
  "found END now attempt to line up code"
  (interactive)
  (if m2-auto-indent-on-end
      (m2-tab))
  (save-excursion
    (if (m2-match-end)
	(m2-display-match (point))
      (message "no matching statement found"))))

(defun m2-test-then ()
  "check to see whether THEN has been typed"
  (interactive)
  (insert "N")
  (setq case-fold-search nil)
  (if (and (not (m2-is-in-string))
	   (> (point) 5))
      (save-excursion
	(forward-char -5)
	(if (looking-at-keyword "[; \t\n]THEN[; \t\n]")
	    (m2-found-then)))))

(defun m2-found-then ()
  "found THEN now attempt to line up code"
  (interactive)
  (if m2-auto-indent-on-then
      (m2-tab)))

(defun m2-test-else ()
  "check to see whether ELSE has been typed"
  (interactive)
  (insert "E")
  (setq case-fold-search nil)
  (if (and (not (m2-is-in-string))
	   (> (point) 5))
      (save-excursion
	(forward-char -5)
	(if (looking-at-keyword "[; \t\n]ELSE[; \t\n]")
	    (m2-found-else)))))

(defun m2-found-else ()
  "found ELSE now attempt to line up code"
  (interactive)
  (if m2-auto-indent-on-else
      (m2-tab)))

(defun m2-local-test ()
 "simple test hook."
 (interactive)
 (let ((m2-success (m2-testing)))
   (if (numberp m2-success)
       (message "number found")
     (if (stringp m2-success)
	 (message "Error found missing: %s" m2-success)
       (if (bufferp m2-success)
	   (message "buffer found")
	 (if (symbolp m2-success)
	     (if m2-success
		 (message "No error found")
	       (message "FALSE returned %s" m2-success))))))))

(defun m2-testing ()
  "simple function which returns a number of different symbols"
  nil
)

(defun m2-match-end ()
  "finds the start of the statement matching the END returns true if found."
  (interactive)
  (let (m2-level)
    (let (beginning)
      (setq m2-level 1)
      (while (and (> m2-level 0) (> (point) 1))
	(re-search-backward-keyword "[ ;\n\t]\\(END\\|IF\\|LOOP\\|WITH\\|WHILE\\|RECORD\\|CASE\\|FOR\\|MODULE\\|PROCEDURE\\)" nil t)
	(forward-char 1)
	(if (not (m2-is-in-string))
	    (if (looking-at-keyword "END")
		(setq m2-level (+ m2-level 1))
	      (setq m2-level (- m2-level 1)
		    beginning (point)))))
      (= m2-level 0))))

(defun m2-end-commencer ()
  "returns true if a token representing the beginning of an END was found."
  (and (> (point) 1)
       (or (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]IF"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]LOOP"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]WITH"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]WHILE"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]RECORD"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]CASE"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]FOR"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]MODULE"))
	   (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]PROCEDURE")))))

(defun m2-is-end()
  "returns true if END is found."
  (and (> (point) 1)
       (save-excursion (forward-char -1) (looking-at-keyword "[ ;\n\t]END"))))

(defun m2-display-match (m2-beginning)
  "displays a match"
  (interactive)
  (if (<= (window-start) m2-beginning)
      (progn
	(goto-char m2-beginning)
	(sit-for 1))
    (progn
      (save-excursion
	(let (m2-start)
	  (let (m2-end)
	    (beginning-of-line)
	    (setq m2-start (point))
	    (end-of-line)
	    (setq m2-end (point))
	(message "matches: %s" (buffer-substring m2-start m2-end))))))))

(defun m2-backward-to-token ()
  "moves back to the beginning of the last token"
  (interactive)
  (if (> (point) 1)
      (progn
	(forward-char -1)
	(while (or (and (> (point) 1)
			(save-excursion (forward-char -1) (looking-at "\\*)")))
		   (looking-at "[ \t\n]"))
	  (if (looking-at "[ \t\n]")
	      (forward-char -1)
	    (progn
	      (m2-backward-to-comment-start)
	      (if (> (point) 1)
		  (forward-char -1)))))
	(if (save-excursion (forward-char -1) (or (looking-at ":=")
						  (looking-at "<=")
						  (looking-at ">=")
						  (looking-at "\\.\\.")
						  (looking-at "<>")))
	    (forward-char -1)
	  (if (looking-at "'")
	      (m2-move-backward-over-quotes)
	    (if (looking-at "[0-9a-zA-Z]")
		(progn
		  (while (and (> (point) 1)
			      (looking-at "[0-9a-zA-Z]"))
		    (forward-char -1))
		  (if (> (point) 1)
		      (forward-char 1)))))))))

(defun m2-backward-until-ident-delimeter ()
  "moves the cursor back until an ident delimeter is found."
  (interactive)
  (while (and (> (point) 1)
	      (save-excursion
		(forward-char -1)
		(not (looking-at "[ \n\t=:,;]"))))
    (forward-char -1))
  (if (and (> (point) 1)
	   (save-excursion
	     (forward-char -1)
	     (looking-at "[=:,;]")))
      (forward-char -1)))

(defun m2-backward-until-white ()
  "moves the cursor back until white space is found."
  (interactive)
  (while (and (> (point) 1)
	      (save-excursion
		(forward-char -1)
		(not (looking-at "[ \t\n]"))))
    (forward-char -1)))

(defun m2-backward-to-noncomment (lim)
  "moves back to the start of a non comment text."
  (interactive)
  (let (carry-on)
    (setq carry-on t)
    (while carry-on
      (skip-chars-backward " \t\n\f" lim)
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*)")))
	  (m2-run-back-over-comments lim))
      (setq carry-on (and (> (point) lim)
			  (or (and (>= (point) (+ 2 lim))
				   (save-excursion (forward-char -2) (looking-at "\\*)")))
			      (save-excursion (forward-char -1) (looking-at "[\n\t ]"))))))))

(defun m2-run-back-over-comments (lim)
  "moves over a comment."
  (interactive)
  (let (m2-comment-level)
    (forward-char -2)
    (if (looking-at "(\\*")
	(setq m2-comment-level 1)
      (if (looking-at "\\*)")
	  (setq m2-comment-level -1)
	(message "run-over-comments assumes it is on a comment")))
    (forward-char -1)
    (while (and
	    (not (= m2-comment-level 0))
	    (> (point) lim))
      (if (looking-at "(\\*")
	  (setq m2-comment-level (+ m2-comment-level 1)))
      (if (looking-at "\\*)")
	  (setq m2-comment-level (- m2-comment-level 1)))
      (forward-char -1))))

(defun m2-is-in-string ()
  "returns true if we are in a string."
  (interactive)
  (let (m2-in-quotes)
    (let (m2-point)
      (setq m2-in-quotes nil)
      (setq m2-point (point))
      (save-excursion
	(beginning-of-line)
	(while (< (point) m2-point)
	  (if (looking-at "\"")
	      (setq m2-in-quotes (not (m2-run-over-double-quotes m2-point)))
	    (if (looking-at "'")
		(setq m2-in-quotes (not (m2-run-over-single-quotes m2-point)))))
	  (forward-char 1)))
      m2-in-quotes)))

(defun m2-run-over-double-quotes (m2-opoint)
  "returns true if we see a another double quote before m2-opoint."
  (interactive)
  (forward-char 1)
  (while (and (< (point) m2-opoint)
	      (not (looking-at "\"")))
    (forward-char 1))
  (and (looking-at "\"") (< (point) m2-opoint)))

(defun m2-run-over-single-quotes (m2-opoint)
  "returns true if we see a another single quote before m2-opoint."
  (interactive)
  (forward-char 1)
  (while (and (< (point) m2-opoint)
	      (not (looking-at "'")))
    (forward-char 1))
  (and (looking-at "'") (< (point) m2-opoint)))

(defun m2-is-in-comment ()
  "returns true if we are in a comment."
  (interactive)
  (let (m2-in-comment)
    (let (m2-point)
      (setq m2-point (point))
      (setq m2-in-comment nil)
      (save-excursion
	(goto-char (point-min))
	(while (< (point) m2-point)
	  (if (looking-at "\"")
	      (m2-run-over-double-quotes m2-point)
	    (if (looking-at "'")
	    (m2-run-over-single-quotes m2-point)
	    (if (looking-at "(\\*")
		(setq m2-in-comment (not (m2-run-forward-over-comments m2-point))))))
      (forward-char 1)))
  m2-in-comment)))

(defun m2-run-forward-over-comments (lim)
  "moves over a comment and returns true if we are not in a comment"
  (interactive)
  (let (m2-comment-level)
    (if (looking-at "(\\*")
	(setq m2-comment-level 1)
      (if (looking-at "\\*)")
	  (setq m2-comment-level -1)
	(message "run-over-comments assumes it is on a comment")))
    (forward-char 1)
    (while (and
	    (not (= m2-comment-level 0))
	    (< (point) (- lim 1)))
      (if (looking-at "(\\*")
	  (setq m2-comment-level (+ m2-comment-level 1)))
      (if (looking-at "\\*)")
	  (setq m2-comment-level (- m2-comment-level 1)))
      (forward-char 1))
    (forward-char 1)
    (= m2-comment-level 0)))

(defun m2-forward-to-token ()
  "moves forward to the beginning of the next token"
  (if (looking-at "[][=,;)--+#\\*\\|\\^\\%\\$@]")
      (forward-char 1)
    (if (or (looking-at ":=")
	    (looking-at "<=")
	    (looking-at ">=")
	    (looking-at "\\.\\.")
	    (looking-at "<>"))
	(forward-char 2)
      (if (looking-at "[><:\\.]")
	  (forward-char 1)
	(if (looking-at "[A-Z|a-z|0-9]")
	    (while (looking-at "[A-Z|a-z|0-9]")
	      (forward-char 1))
	  (if (and (looking-at "(") (not (looking-at "(\\*")))
	      (forward-char 1)
	    (if (looking-at "'")
		(progn
		  (m2-move-over-quotes)
		  (forward-char 1))))))))
  (while (or (looking-at "(\\*")
	     (looking-at "[ \t\n]"))
    (m2-forward-to-noncomment (point-max))))

(defun m2-forward-until-white (lim)
  "moves the cursor forward until white space is found."
  (while (and (< (point) lim)
	      (save-excursion
		(forward-char 1)
		(not (looking-at "[ \t\n]"))))
    (forward-char 1)))

(defun m2-forward-until-non-white (lim)
  "moves the cursor forward until non white space is found."
  (while (and (< (point) lim)
	      (save-excursion
		(forward-char 1)
		(looking-at "[ \t\n]|(\\*")))
    (forward-char 1)))

(defun m2-forward-to-noncomment (lim)
  "moves forward to the start of a non comment text."
  (interactive)
  (let (m2-point)
    (let (m2-carry-on)
      (setq m2-point (point))
      (setq m2-carry-on (< (point) lim))
      (while m2-carry-on
	(skip-chars-forward "[ \t\n]" lim)
	(setq m2-point (point))
	(if (and (< (point) lim)
		 (looking-at "(\\*"))
	    (m2-run-forward-over-comments lim))
	(setq m2-carry-on (and (< (point) lim)
			       (or (looking-at "(\\*")
				   (looking-at "[\n\t ]")))))
      (< (point) lim))))

(defun m2-is-token (string)
  "returns true if we can see a token string. It does not eat up the token."
  (let (m2-point)
    (let (m2-found-noncom)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (if (looking-at string)
	      t
	    nil)
	(progn
	  (goto-char m2-point)
	  nil)))))

(defun m2-token-is (string)
  "returns true if we can see a token string. It does eat the token."
  (let (m2-point)
    (let (m2-found-noncom)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (if (looking-at string)
	      (progn
		(forward-char (length string))
		t)
	    nil)
	(progn
	  (goto-char m2-point)
	  nil)))))

(defun m2-ident ()
  "derived from EBNF for ident"
  (let (m2-point)
    (let (m2-found-noncom)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (progn
	    (if (and (looking-at "[A-Za-z][A-Za-z]*[0-9]*")
		     (progn (skip-chars-forward "[A-Za-z][A-Za-z]*[0-9]*")
			    (or (looking-at "[ \n\t]")
				(looking-at "(\\*"))))
		(progn
		  (skip-chars-forward "[A-Za-z][A-Za-z]*[0-9]*")
		  t)
	      "Identifier"))
	(goto-char m2-point)
	"Identifier"))))

(defun m2-string ()
  "derived from EBNF for a modula-2 string"
  (let (m2-found-noncom)
    (let (m2-point)
      (setq m2-point (point))
      (setq m2-found-noncom (m2-forward-to-noncomment (point-max)))
      (if m2-found-noncom
	  (if (looking-at "'")
	      (m2-move-over-quotes)
	    nil)
	(progn
	  (goto-char m2-point)
	  nil)))))

(defun m2-move-over-quotes ()
  "moves the cursor over the quoted string, nil is returned if
   the string encounters a newline before the quote."
  (let (m2-qpoint)
    (let (m2-carry-on)
      (setq m2-qpoint (point))
      (if (looking-at "'")
	  (progn
	    (forward-char 1)
	    (setq m2-carry-on (not (looking-at "['\n]")))
	    (while m2-carry-on
	      (progn
		(forward-char 1)
		(setq m2-carry-on (not (looking-at "['\n]")))))
	    (if (looking-at "\n")
		(progn
		  (goto-char m2-qpoint)
		  "missing end quote before newline")
	      t))
	(progn
	  (forward-char 1)
	  (set m2-carry-on (not (looking-at "[\"\n]")))
	  (while m2-carry-on
	    (progn
	      (forward-char 1)
	      (setq m2-carry-on (not (looking-at "[\"\n]")))))
	  (if (looking-at "\n")
	      (progn
		(goto-char m2-qpoint)
		"missing end quote before newline")
	    t))))))

(defun m2-move-backward-over-quotes ()
  "moves the cursor over the quoted string, nil is returned if
   the string encounters a newline before the quote."
  (let (m2-qpoint)
    (let (m2-carry-on)
      (setq m2-qpoint (point))
      (if (looking-at "'")
	  (progn
	    (forward-char -1)
	    (setq m2-carry-on (not (looking-at "['\n]")))
	    (while m2-carry-on
	      (progn
		(forward-char -1)
		(setq m2-carry-on (not (looking-at "['\n]")))))
	    (if (looking-at "\n")
		(progn
		  (goto-char m2-qpoint)
		  "missing end quote before newline")
	      t))
	(progn
	  (forward-char -1)
	  (setq m2-carry-on (not (looking-at "[\"\n]")))
	  (while m2-carry-on
	    (progn
	      (forward-char -1)
	      (setq m2-carry-on (not (looking-at "[\"\n]")))))
	  (if (looking-at "\n")
	      (progn
		(goto-char m2-qpoint)
		"missing end quote before newline")
	    t))))))

(defun interactive-blink-matching-open ()
  "Indicate momentarily the start of sexp before point."
  (interactive)
  (let ((blink-matching-paren-distance (buffer-size))
	(blink-matching-paren t))
    (blink-matching-open)))

;; define several class of keywords, longest similar string first
;; so that the regexp performs the longer match first.
;; eg MOD and MODULE, PACKED and PACKEDSET

(defvar m2-auto-keywords-pim
  '("AND" "ARRAY" "BEGIN" "BY" "CASE" "CONST" "DEFINITION" "DIV"
    "DO" "ELSE" "ELSIF" "END" "EXCEPT" "EXIT" "EXPORT" "FINALLY"
    "FOR" "FROM" "IF" "IMPLEMENTATION" "IMPORT" "IN" "LOOP"
    "MOD" "NOT" "OF" "OR" "PACKEDSET" "PACKED" "POINTER"
    "QUALIFIED" "RECORD" "REPEAT" "RETURN" "SET" "THEN" "TO" "TYPE"
    "UNTIL" "VAR" "WHILE" "WITH")
  "Modula-2 keywords.")

(defvar m2-auto-keywords-iso
  '("EXCEPT" "FINALLY" "PACKEDSET" "PACKED" "REM" "RETRY")
  "Modula-2 keywords.")

(defvar m2-auto-keywords-gm2
  '("ASM" "VOLATILE" "UNQUALIFIED")
  "GNU Modula-2 keyword extensions.")

(defvar m2-auto-keywords-r10
  '("FIXME")
  "M2R10 keywords. --fixme-- complete this")

(defvar m2-auto-keywords
  '()
  "Modula-2 keywords.")

(defvar m2-auto-types
  '()
  "Modula-2 types.")

(defvar m2-auto-types-gm2
  '("SHORTREAL" "SHORTINT" "SHORTCARD" "SHORTCOMPLEX"
    "INTEGER8" "INTEGER16" "INTEGER32" "INTEGER64"
    "CARDINAL8" "CARDINAL16" "CARDINAL32" "CARDINAL64"
    "WORD16" "WORD32" "WORD64"
    "BITSET8" "BITSET16" "BITSET32"
    "REAL32" "REAL64" "REAL128"
    "COMPLEX32" "COMPLEX64" "COMPLEX128"
    "COMPLEX" "LONGCOMPLEX"
    "CSIZE_T" "CSSIZE_T" "LOC")
  "Modula-2 types.")

(defvar m2-auto-types-iso
  '("REAL" "LONGREAL" "INTEGER" "LONGINT"
    "CARDINAL" "LONGCARD" "CHAR" "BOOLEAN"
    "COMPLEX" "LONGCOMPLEX"
    "ADDRESS" "WORD" "BYTE" "LOC" "BITSET")
  "Modula-2 types.")

(defvar m2-auto-types-pim
  '("REAL" "LONGREAL" "INTEGER" "LONGINT"
    "CARDINAL" "LONGCARD" "CHAR" "BOOLEAN"
    "ADDRESS" "WORD" "BYTE" "BITSET")
  "Modula-2 types.")

(defvar m2-auto-types-r10
  '()
  "R10 types.")

(defvar m2-auto-constants
  '("FALSE" "TRUE" "NIL")
  "Modula-2 constants.")

(defvar m2-auto-functions
  '()
  "Modula-2 functions.")

(defvar m2-auto-functions-pim
  '("ABS" "ADR" "CAP" "CHR" "DEC" "DISPOSE" "EXCL" "FLOAT"
    "HIGH" "INC" "INCL" "MAX" "MIN" "NEW" "ODD" "ORD"
    "SIZE" "TRUNC" "VAL")
  "Modula-2 functions found in PIM.")

(defvar m2-auto-functions-iso
  '("ABS" "ADR" "CAP" "CHR" "CMPLX" "DEC" "DISPOSE" "EXCL" "FLOAT"
    "HIGH" "IM" "INC" "INCL" "LENGTH" "MAX" "MIN" "NEW" "ODD" "ORD"
    "RE" "SIZE" "TRUNC" "VAL")
  "Modula-2 functions found in ISO.")

(defvar m2-auto-functions-gm2
  '("IM" "LENGTH" "MAX" "MIN" "RE" "ROTATE" "SHIFT" "THROW" "TBITSIZE")
  "Modula-2 functions found in SYSTEM and gm2 allows access to ISO
   if PIM is used.")

(defvar m2-auto-functions-r10
  '()
  "R10 functions.")

(defvar m2-auto-block-name
  '("END" "MODULE" "PROCEDURE")
  "Modula-2 keywords which will name a block.")

;; create the regex string for each class of keywords
(defvar m2-auto-keywords-regexp (regexp-opt m2-auto-keywords 'words))
(defvar m2-auto-type-regexp (regexp-opt m2-auto-types 'words))
(defvar m2-auto-constant-regexp (regexp-opt m2-auto-constants 'words))
(defvar m2-auto-functions-regexp (regexp-opt m2-auto-functions 'words))

(defvar m2-auto-traditional-keywords-regexp (regexp-opt m2-auto-keywords 'words))
(defvar m2-auto-traditional-type-regexp (regexp-opt m2-auto-types 'words))
(defvar m2-auto-traditional-constant-regexp (regexp-opt m2-auto-constants 'words))
(defvar m2-auto-traditional-functions-regexp (regexp-opt m2-auto-functions 'words))

(defun m2-debug-message (m)
  "."
  (interactive)
  (message m)
  (while t
    (progn)))

(defun m2-union (a b)
  "."
  (interactive)
  (progn
    (let (result)
      (setq result (copy-tree a))
      (let (l)
	(setq l (length b))
	(let (i)
	  (setq i 0)
	  (while (< i l)
	    (progn
	      (add-to-list 'result (nth i b))
	      (setq i (+ i 1))))))
      result)))

(defun m2-generate-keywords ()
  "generate the variable m2-auto-keywords from the chosen dialect."
  (interactive)
  (setq m2-auto-keywords (m2-union m2-auto-keywords m2-auto-keywords-pim))
  (if (eq m2-auto-default-dialect 'iso)
      (setq m2-auto-keywords (m2-union m2-auto-keywords m2-auto-keywords-iso)))
  (if (eq m2-auto-default-dialect 'r10)
      (setq m2-auto-keywords (m2-union m2-auto-keywords m2-auto-keywords-r10)))
  (if m2-auto-default-gm2-extensions
      (setq m2-auto-keywords (m2-union m2-auto-keywords m2-auto-keywords-gm2)))
  (setq m2-auto-keyword-regexp (concat "\\(\\.\\|(\\|,\\|;\\|^\\| \\|\t\\)\\("
				       (mapconcat 'identity m2-auto-keywords "\\|")
				       "\\)\\(,\\|)\\|(\\|;\\| \\|$\\)"))
  (setq m2-auto-traditional-keywords-regexp (regexp-opt m2-auto-keywords 'words)))

(defun m2-generate-types ()
  "generate the variable m2-auto-types from the chosen dialect."
  (interactive)
  (if (eq m2-auto-default-dialect 'pim)
      (setq m2-auto-types (m2-union m2-auto-types m2-auto-types-pim)))
  (if (eq m2-auto-default-dialect 'iso)
      (setq m2-auto-types (m2-union m2-auto-types m2-auto-types-iso)))
  (if (eq m2-auto-default-dialect 'r10)
      (setq m2-auto-types (m2-union m2-auto-types m2-auto-types-r10)))
  (if m2-auto-default-gm2-extensions
      (setq m2-auto-types (m2-union m2-auto-types m2-auto-types-gm2)))
  (setq m2-auto-type-regexp (concat "\\(\\.\\|(\\|,\\|;\\|^\\| \\|\t\\)\\(" (mapconcat 'identity m2-auto-types "\\|") "\\)\\(,\\|)\\|(\\|;\\| \\|$\\)"))
  (setq m2-auto-traditional-type-regexp (regexp-opt m2-auto-types 'words)))

(defun m2-generate-functions ()
  "generate the variable m2-auto-functions from the chosen dialect."
  (interactive)
  (if (eq m2-auto-default-dialect 'pim)
      (setq m2-auto-functions (m2-union m2-auto-functions m2-auto-functions-pim)))
  (if (eq m2-auto-default-dialect 'iso)
      (setq m2-auto-functions (m2-union m2-auto-functions m2-auto-functions-iso)))
  (if (eq m2-auto-default-dialect 'r10)
      (setq m2-auto-functions (m2-union m2-auto-functions m2-auto-functions-r10)))
  (if m2-auto-default-gm2-extensions
      (setq m2-auto-functions (m2-union m2-auto-functions m2-auto-functions-gm2)))
  (setq m2-auto-builtin-regexp (concat "\\(\\.\\|(\\|,\\|;\\|^\\| \\|\t\\)\\(" (mapconcat 'identity m2-auto-functions "\\|") "\\)\\(,\\|)\\|(\\|;\\| \\|$\\)"))
  (setq m2-auto-traditional-functions-regexp (regexp-opt m2-auto-functions 'words)))

(defun restore-upper (begin end)
  "."
  (interactive)
  (if (m2-auto-on-upper begin)
      (progn
	(let (was-modified)
	  (setq was-modified (buffer-modified-p))
	  (upcase-region begin end)
	  (remove-text-properties begin end '(font-lock-face nil))
	  (remove-text-properties begin end '(upper nil))
	  (remove-text-properties begin end '(face nil))
	  (set-buffer-modified-p was-modified)))))

(defun m2-auto-restore-upper-case ()
  "."
  (interactive)
  (m2-auto-restore-upper-case-region (point-min) (point-max)))

(defun m2-auto-restore-upper-case-region (begin end)
  "."
  (interactive)
  (save-excursion
    (goto-char begin)
    (let (m2-auto-min)
      (setq m2-auto-min (point))
      (let (seen-upper)
	(setq seen-upper (m2-auto-on-upper (point)))
	(while (< (point) end)
	  (progn
	    (if (not (eq seen-upper (m2-auto-on-upper (point))))
		(progn
		  (setq seen-upper (m2-auto-on-upper (point)))
		  (if (m2-auto-on-upper (point))
		      ;; just moved onto an uppercase
		      (setq m2-auto-min (point))
		    ;; just moved off an uppercase
		    (restore-upper m2-auto-min (point)))))
	    (forward-char 1)))
	(if (m2-auto-on-upper m2-auto-min)
	    (restore-upper m2-auto-min (point)))))))

(defun remove-upper-highlight-right ()
  "."
  (interactive)
  ;; (message "insert hook executed")
  (if (< (point) (point-max))
      (progn
	(save-excursion
	  (forward-char 1)
	  (if (< (point) (point-max))
	      (progn
		(let (start)
		  (setq start (point))
		  (forward-char 1)
		  (while (and (< (point) (point-max))
			      (m2-auto-on-upper (point)))
		    (forward-char 1))
		  (forward-char -1)
		  (if (m2-auto-on-upper (point))
		      (restore-upper start (point))))))))))

(defun remove-upper-highlight-left ()
  "."
  (interactive)
  (progn
    ;; (message "insert hook executed")
    (let (bol)
      (setq bol (line-beginning-position))
      (if (> (point) bol)
	  (progn
	    (save-excursion
	      (forward-char -1)
	      (let (start)
		(setq start -1)
		(while (and (>= (point) bol)
			    (m2-auto-on-upper (point)))
		  (progn
		    (setq start (point))
		    (forward-char -1)))
		(if (> start -1)
		    (progn
		      (let (end)
			(goto-char start)
			(setq bol (line-end-position))
			(setq end -1)
			(while (and (<= (point) bol)
				    (m2-auto-on-upper (point)))
			  (progn
			    (forward-char 1)
			    (setq end (point))))
			(if (> end -1)
			    (restore-upper start end))))))))))))

(defun m2-auto-check-on-insertion ()
  "."
  (interactive)
  (m2-auto-restore-upper-case-region (line-beginning-position) (line-end-position)))

(defun m2-backspace ()
  "."
  (interactive)
  (backward-delete-char-untabify 1)
  (m2-auto-check-on-insertion))

(defun m2-delete ()
  "."
  (interactive)
  (delete-char 1)
  (m2-auto-check-on-insertion))

(defun m2-auto-on-upper (pos)
  "."
  (interactive)
  (progn
    (get-text-property pos 'upper)))

(defun m2-auto-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun m2-auto-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun m2-auto-trim (s)
  "Remove whitespace at the beginning and end of S."
  (m2-auto-trim-left (m2-auto-trim-right s)))

(defun m2-auto-lowerise (all-matched left-leader token face)
  "."
  (interactive)
  (progn
    (save-excursion
      (let (l)
	;; (message (concat "m2-auto-keywordise <" all-matched ">"))
	(setq l (length token))
	(goto-char (match-beginning 0))
	;; (message (format "value of l is %d, keyword %d and all-matched %d" l (length keyword) (length all-matched)))
	(forward-char (length left-leader))
	(insert (propertize (downcase token)
			    'font-lock-face face
			    'rear-nonsticky t
			    'upper t))
	(delete-char (length token))))
    nil))

(defun m2-auto-lowerise-block (all-matched left-leader token face)
  "."
  (interactive)
  (progn
    (save-excursion
      (goto-char (match-beginning 0))
      (forward-char (length left-leader))
      (insert (propertize (downcase token)
			  'font-lock-face face
			  'rear-nonsticky t
			  'upper t))
      (delete-char (length token)))
    nil))

(defun m2-auto-lowerise-block-ident (all-matched left-leader token ident keyword-face function-face)
  "."
  (interactive)
  (progn
    (save-excursion
      (let (l)
	;; (message (concat "m2-auto-lowerise-block-ident <" all-matched "> ident <" ident ">"))
	(setq l (length token))
	(goto-char (match-beginning 0))
	;; (message (format "value of l is %d, keyword %d and all-matched %d" l (length keyword) (length all-matched)))
	(forward-char (length left-leader))
	(insert (propertize (downcase token)
			    'font-lock-face keyword-face
			    'rear-nonsticky t
			    'upper t))
	(delete-char (length token))
	(setq l (- (length all-matched) (length ident)))
	(setq l (- l (length token)))
	(forward-char l)
	(insert (propertize ident
			    'font-lock-face function-face
			    'rear-nonsticky t))
	(delete-char (length ident))))
    nil))

(defun m2-all-upper-case-region (begin end)
  "."
  (interactive)
  (progn
    (save-excursion
      (goto-char begin)
      (let (m2-auto-min)
	(setq m2-auto-min (point))
	(let (is-upper)
	  (setq is-upper nil)
	  (while (and (< (point) end)
		      is-upper)
	    (progn
	      (if (not (m2-auto-on-upper (point)))
		  (setq seen-upper nil))
	      (forward-char 1)))
	  is-upper)))))

(defun dbg (kind s)
  "."
  (interactive)
  (message (concat kind "<" s ">"))
  (sit-for 1))

(defun m2-auto-detect-dialect ()
  "."
  (interactive)
  (save-excursion
    (if (re-search-forward "(*!m2" nil t)
	(progn
	  (while (and (< (point) (point-max))
		      (not (looking-at "*)")))
	    (progn
	      ;; (message (concat "while: " (buffer-substring (point) (+ (point) 3))))
	      (if (looking-at "pim")
		  (progn
		    (forward-char 3)
		    (setq m2-dialect-known t)
		    (add-to-list 'm2-dialect 'pim))
		(if (looking-at "iso")
		    (progn
		      (forward-char 3)
		      (setq m2-dialect-known t)
		      (add-to-list 'm2-dialect 'iso))
		  (if (looking-at "r10")
		      (progn
			(forward-char 3)
			(setq m2-dialect-known t)
			(add-to-list 'm2-dialect 'r10))
		  (if (looking-at "gm2")
		      (progn
			(forward-char 3)
			(add-to-list 'm2-dialect 'gm2))
		    (forward-char 1)))))))))))

(defun m2-auto-dialect-pim ()
  "return t if m2pim dialect was configured or detected."
  (interactive)
  (if (not m2-dialect-known)
      (m2-auto-detect-dialect))
  (progn
    (memq 'pim m2-dialect)))

(defun m2-auto-dialect-iso ()
  "return t if m2iso dialect was configured or detected."
  (interactive)
  (if (not m2-dialect-known)
      (m2-auto-detect-dialect))
  (progn
    (memq 'iso m2-dialect)))

(defun m2-auto-dialect-r10 ()
  "return t if m2r10 dialect was configured or detected."
  (interactive)
  (if (not m2-dialect-known)
      (m2-auto-detect-dialect))
  (progn
    (memq 'r10 m2-dialect)))

(defun m2-auto-dialect-gm2-extensions ()
  "return t if either the tag +gm2 detected or gm2 has been configured in customize."
  (interactive)
  (if (not m2-dialect-known)
      (m2-auto-detect-dialect))
  (progn
    (memq 'gm2 m2-dialect)))

(defun m2-auto-message-dialect ()
  "."
  (interactive)
  (if (m2-auto-dialect-pim)
      (message "pim dialect of Modula-2"))
  (if (m2-auto-dialect-iso)
      (message "iso dialect of Modula-2"))
  (if (m2-auto-dialect-r10)
      (message "r10 dialect of Modula-2")))

(defun m2-auto-after-load-hook (filename)
  "."
  (interactive)
  ;; (message "after-load-hook")
  (m2-auto-detect-dialect))
;;  (if m2-dialect-known
;;      (m2-auto-message-dialect)
;;  (message "no dialect of Modula-2 detected or configured yet using emacs customize")))

(defconst m2-auto-keyword-regexp (concat "\\((\\|,\\|;\\|^\\| \\|\t\\)\\(" (mapconcat 'identity m2-auto-keywords "\\|") "\\)\\(,\\|)\\|(\\|;\\| \\|$\\)"))
(defconst m2-auto-type-regexp (concat "\\((\\|,\\|;\\|^\\| \\|\t\\)\\(" (mapconcat 'identity m2-auto-types "\\|") "\\)\\(,\\|)\\|(\\|;\\| \\|$\\)"))
(defconst m2-auto-constant-regexp (concat "\\((\\|,\\|;\\|^\\| \\|\t\\)\\(" (mapconcat 'identity m2-auto-constants "\\|") "\\)\\(,\\|)\\|(\\|;\\| \\|$\\)"))
(defconst m2-auto-builtin-regexp (concat "\\((\\|,\\|;\\|^\\| \\|\t\\)\\(" (mapconcat 'identity m2-auto-functions "\\|") "\\)\\(,\\|)\\|(\\|;\\| \\|$\\)"))
;; (defconst m2-auto-procedure-regexp "\\(PROCEDURE\\( \\|\t\\|\n\\)\\*\\(\\[:alpha:\\]\\[:alpnum:\\]\\*\\)\\)")
;; (defconst m2-auto-procedure-regexp "\\(PROCEDURE\\( \\|\t\\)*\\)")
;; (defconst m2-auto-procedure-regexp (concat "\\((\\|,\\|;\\|^\\| \\|\t\\)\\(" (mapconcat 'identity m2-auto-block-name "\\|") "\\)\\( \\|)\\(\\[:alpha:\\]\\[:alpnum:\\]*\\)\\|(\\|;\\| \\|$\\)")) ; work
;; (defconst m2-auto-procedure-regexp (concat "\\(\\|,\\|;\\^| \\|\t\\)\\(" (mapconcat 'identity m2-auto-block-name "\\|") "\\)\\( \\|,\\|)\\|(\\|;\\|$\\)"))
(defconst m2-auto-block-regexp (concat "\\(\\|,\\|;\\^| \\|\t\\)\\(" (mapconcat 'identity m2-auto-block-name "\\|") "\\)\\(,\\|(\\|(\\|;\\| \\|$\\)"))
;; (defconst m2-auto-block-ident-regexp (concat "\\(\\|,\\|;\\^| \\|\t\\)\\(" (mapconcat 'identity m2-auto-block-name "\\|") "\\)\\( \\)\\([\\[:alpha:]\\]+\\)"))
(defconst m2-auto-block-ident-regexp (concat "\\(\\|,\\|;\\^| \\|\t\\)\\(" (mapconcat 'identity m2-auto-block-name "\\|") "\\)\\( \\)+\\([\\[:alpha:]\\[:alnum:]\\]+\\)"))

(defvar m2-auto-test-keywords nil
  "dynamically generated keyword list from the dialects.")

(defun m2-auto-adapt-font-faces ()
  "."
  (interactive)
  (if m2-auto-keywords-underlined
      (progn
	(make-variable-buffer-local 'font-lock-keyword-face)
	(copy-face 'font-lock-keyword-face 'm2-auto-keyword-face)
	;; (set-face-foreground 'm2-auto-keyword-face "green4")
	(set-face-bold 'm2-auto-keyword-face t)
	(set-face-underline 'm2-auto-keyword-face t)
	(setq font-lock-keyword-face 'm2-auto-keyword-face)))
  (if m2-auto-functions-italic
      (progn
	(make-variable-buffer-local 'font-lock-builtin-face)
	(copy-face 'font-lock-builtin-face 'm2-auto-builtin-face)
	(set-face-bold 'm2-auto-builtin-face t)
	(set-face-italic 'm2-auto-builtin-face t)
	(setq font-lock-builtin-face 'm2-auto-builtin-face))))

;;(make-variable-buffer-local 'font-lock-keyword-face) (copy-face
;;'font-lock-keyword-face 'm2-auto-keyword-face) (set-face-foreground
;;'m2-auto-keyword-face "green4")

;;(add-hook 'lua-mode-hook
;;          (lambda ()
;;            (setq font-lock-keyword-face 'm2-auto-keyword-face)
;;            ))


(defun m2-auto-function-name-test (name)
  "."
  (interactive)
  (message name)
  (sit-for 1))

(defun m2-auto-add-keywords ()
  "."
  (interactive)

  (m2-auto-adapt-font-faces)
  (m2-generate-keywords)
  (m2-generate-types)
  (m2-generate-functions)
  (if m2-auto-use-algol-style
      (progn
	`(
	  (,m2-auto-type-regexp "\\(.*$\\)"
			       (m2-auto-lowerise (match-string 0) (match-string 1) (match-string 2) font-lock-type-face) nil
			       (1 font-lock-type-face))
	  (,m2-auto-constant-regexp "\\(.*$\\)"
				   (m2-auto-lowerise (match-string 0) (match-string 1) (match-string 2) font-lock-constant-face) nil
				   (1 font-lock-constant-face))
	  (,m2-auto-builtin-regexp "\\(.*$\\)"
				  (m2-auto-lowerise (match-string 0) (match-string 1) (match-string 2) font-lock-builtin-face) nil
				  (1 font-lock-builtin-face))
	  ;; (,m2-auto-procedure-regexp "\\(.*$\\)"
	  ;; (m2-auto-function-name-test (match-string 0))
	  ;; (1 font-lock-function-face))
	  ;;
	  ;;  got to here
	  ;;
	  (,m2-auto-block-ident-regexp "\\(.*$\\)"
				       (m2-auto-lowerise-block-ident
					(match-string 0) (match-string 1) (match-string 2) (match-string 4)
					font-lock-keyword-face font-lock-function-name-face) nil
				       (1 font-lock-keyword-face))
	  (,m2-auto-block-regexp "\\(.*$\\)"
				 (m2-auto-lowerise-block (match-string 0) (match-string 1) (match-string 2) font-lock-keyword-face) nil
				 (1 font-lock-keyword-face))
	  ;; end here
	  (,m2-auto-keyword-regexp "\\(.*$\\)"
				  (m2-auto-lowerise (match-string 0) (match-string 1) (match-string 2) font-lock-keyword-face) nil
				  (1 font-lock-keyword-face))))
    (progn
      `(
	(,m2-auto-traditional-type-regexp . font-lock-type-face)
	(,m2-auto-traditional-constant-regexp . font-lock-constant-face)
	(,m2-auto-traditional-functions-regexp . font-lock-function-name-face)
	(,m2-auto-traditional-keywords-regexp . font-lock-keyword-face)
	;; note: order above matters.
	))))

(defun gm2-mode ()
  "Major mode for editing M2 code. User definable variables:
   m2-indent-level
      Indentation of M2 statements within surrounding block."
  (interactive)
  (kill-all-local-variables)
  (setup-m2-auto-keys)
  (use-local-map m2-auto-map)
  (setq major-mode 'gm2-mode)
  (setq mode-name "GM2-trunk")
  (setq local-abbrev-table m2-auto-abbrev-table)
  (set-syntax-table m2-auto-syntax-table)
  (setq blink-matching-paren t)

  ;; code for syntax highlighting
  (setq font-lock-defaults '(m2-auto-add-keywords))
  ;; clear memory
  (setq m2-auto-keywords-regexp nil)
  (setq m2-auto-types-regexp nil)
  (setq m2-auto-constants-regexp nil)
  (setq m2-auto-functions-regexp nil)
  ;; end of syntax highlighting

  ;; (comment-start "(*") (comment-end "*)")
  (setq case-fold-search nil)
  (setq indent-tabs-mode nil)
  (setq m2-auto-hook
	'(lambda ()
	   (progn (make-local-variable 'compile-command)
		  (setq compile-command (concat (m2-auto-get-compile-command) " " (concat (substring (buffer-name) 0 -4) ".mod")))
		  (linum-mode 0))))

  (add-hook 'before-save-hook 'm2-auto-restore-upper-case)
  (add-hook 'post-self-insert-hook 'm2-auto-check-on-insertion nil 'local)
  (add-hook 'after-load-functions 'm2-auto-after-load-hook)
  (run-hooks 'm2-auto-hook))

;;
;;  ----------------------------------------------------------------------
;;  how to install this emacs file on your system?
;;  ----------------------------------------------------------------------
;;
;;  add this to your $HOME/.emacs file
;;
;;  (add-to-list 'package-archives
;;                '("gm2" . "http://floppsie.comp.glam.ac.uk/packages/"))
;;
;;  now in emacs type
;;
;;  M-x package-install gm2
;;
