;; Code and mode itertator and attribute definitions
;; of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2016 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;----------------------------------------------------------------------------
;; Mode iterators.
;;----------------------------------------------------------------------------

;; A list of integer modes that are up to one word long.
(define_mode_iterator QIHISI [QI HI SI])

;; A list of integer modes that are up to one half-word long.
(define_mode_iterator QIHI [QI HI])

;; A list of the modes that are up to double-word long.
(define_mode_iterator DIDF [DI DF])


;;----------------------------------------------------------------------------
;; Mode attributes.
;;----------------------------------------------------------------------------

(define_mode_attr size [(QI "b") (HI "h") (SI "w")])

(define_mode_attr byte [(QI "1") (HI "2") (SI "4")])


;;----------------------------------------------------------------------------
;; Code iterators.
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; Code attributes.
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
