;;; -*- lexical-binding: t; -*-

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

;;; Usage:
;; $ emacs -batch -l mdcompact.el -l mdcompact-testsuite.el -f ert-run-tests-batch-and-exit 

;;; Code:

(require 'mdcompact)
(require 'ert)

(defconst mdcompat-test-directory (concat (file-name-directory
					   (or load-file-name
                                               buffer-file-name))
					  "tests/"))

(defun mdcompat-test-run (f)
  (with-temp-buffer
    (insert-file-contents f)
    (mdcomp-run-at-point)
    (let ((a (buffer-string))
	  (b (with-temp-buffer
	       (insert-file-contents (concat f ".out"))
	       (buffer-string))))
      (should (string= a b)))))

(defmacro mdcompat-gen-tests ()
  `(progn
     ,@(cl-loop
      for f in (directory-files mdcompat-test-directory t "md$")
      collect
      `(ert-deftest ,(intern (concat "mdcompat-test-"
				     (file-name-sans-extension
				      (file-name-nondirectory f))))
	   ()
	 (mdcompat-test-run ,f)))))

(mdcompat-gen-tests)

;;; mdcompact-testsuite.el ends here
