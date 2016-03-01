;;- Machine description for GNU compiler -- S/390 / zSeries version.
;;  Subst patterns.
;;  Copyright (C) 2016 Free Software Foundation, Inc.
;;  Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_code_iterator SUBST [rotate ashift lshiftrt])

; This expands an register/immediate operand to a register+immediate
; operand to draw advantage of the address style operand format
; providing a addition for free.
(define_subst "addr_style_op_subst"
  [(set (match_operand:DSI 0 "" "")
        (SUBST:DSI (match_operand:DSI 1 "" "")
		   (match_operand:SI 2 "" "")))]
  ""
  [(set (match_dup 0)
        (SUBST:DSI (match_dup 1)
		   (plus:SI (match_operand:SI 2 "register_operand" "a")
			    (match_operand 3 "const_int_operand"   "n"))))])

; Use this in the insn name.
(define_subst_attr "addr_style_op"     "addr_style_op_subst" "" "_plus")

; In the subst pattern the additional const int operand will be used
; as displacement.  In the normal version %Y is able to print the
; operand either as displacement or as base register.
(define_subst_attr "addr_style_op_ops" "addr_style_op_subst" "%Y2" "%Y3(%2)")


; This substitution adds an explicit AND operation to the second
; operand.  This way previous operations on the now masked out bits
; might get optimized away.
(define_subst "masked_op_subst"
  [(set (match_operand:DSI 0 ""           "")
        (SUBST:DSI (match_operand:DSI 1 "" "")
		   (match_operand:SI  2 "" "")))]
  ""
  [(set (match_dup 0)
        (SUBST:DSI (match_dup 1)
		   (and:SI (match_dup 2)
			   (match_operand:SI 3 "const_int_6bitset_operand" "jm6"))))])

; Use this in the insn name.
(define_subst_attr "masked_op" "masked_op_subst" "" "_and")

