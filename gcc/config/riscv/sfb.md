;; Machine description for short forward branches(SFB).
;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Patterns for implementations that optimize short forward branches.

(define_insn "*mov<GPR:mode><X:mode>cc"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(if_then_else:GPR
	 (match_operator 5 "ordered_comparison_operator"
		[(match_operand:X 1 "register_operand" "r,r")
		 (match_operand:X 2 "reg_or_0_operand" "rJ,rJ")])
	 (match_operand:GPR 3 "register_operand" "0,0")
	 (match_operand:GPR 4 "sfb_alu_operand" "rJ,IL")))]
  "TARGET_SFB_ALU"
  "@
   b%C5\t%1,%z2,1f\t# movcc\;mv\t%0,%z4\n1:
   b%C5\t%1,%z2,1f\t# movcc\;li\t%0,%4\n1:"
  [(set_attr "length" "8")
   (set_attr "type" "sfb_alu")
   (set_attr "mode" "<GPR:MODE>")])

;; Combine creates this form ((typeof(y))zero_one * z) <op> y
;; for SiFive short forward branches.

(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (sign_extract:X (match_operand:X 1 "register_operand")
			       (const_int 1)
			       (match_operand 2 "immediate_operand"))
	       (match_operand:X 3 "register_operand")))
   (clobber (match_operand:X 4 "register_operand"))]
  "TARGET_SFB_ALU"
  [(set (match_dup 4) (zero_extract:X (match_dup 1) (const_int 1) (match_dup 2)))
   (set (match_dup 0) (if_then_else:X (ne (match_dup 4) (const_int 0))
				      (match_dup 3)
				      (const_int 0)))])

(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (sign_extract:X (match_operand:X 1 "register_operand")
			       (const_int 1)
			       (match_operand 2 "immediate_operand"))
	       (match_operand:X 3 "register_operand")))
   (clobber (match_operand:X 4 "register_operand"))]
  "TARGET_SFB_ALU && (UINTVAL (operands[2]) < 11)"
  [(set (match_dup 4) (and:X (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (if_then_else:X (ne (match_dup 4) (const_int 0))
				      (match_dup 3)
				      (const_int 0)))]
{
  operands[2] = GEN_INT (1 << UINTVAL(operands[2]));
})
