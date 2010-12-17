;; Predicate definitions for Xilinx MicroBlaze
;; Copyright 2009, 2010 Free Software Foundation, Inc.
;;
;; Contributed by Michael Eager <eager@eagercon.com>.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  


;; Return whether OP can be used as an operands in arithmetic.
(define_predicate "arith_operand"
  (ior (match_code "const_int,const_double")
       (match_operand 0 "register_operand")))

(define_predicate "arith_operand32"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int,const_double")
	    (match_test "LARGE_INT (op)"))))

(define_predicate "const_0_operand"
  (and (match_code "const_int,const_double")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

;; Return whether OP is a register or the constant 0.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "const_0_operand")
       (match_operand 0 "register_operand")))

;;  Return if the operand is either the PC or a label_ref.  
(define_special_predicate "pc_or_label_operand"
  (ior (match_code "pc,label_ref")
       (and (match_code "symbol_ref")
            (match_test "!(strcmp ((XSTR (op, 0)), \"_stack_overflow_exit\"))"))))

;; Test for valid call operand
(define_predicate "call_insn_operand"
  (match_test "CALL_INSN_OP (op)"))

;; Return if OPERAND is valid as a source operand for a move instruction.
(define_predicate "move_operand"
  (and (
     not (
       and (match_code "plus")
           (not (match_test "(GET_CODE (XEXP (op, 0)) == REG) ^ (GET_CODE (XEXP (op,1)) == REG)"))
	 )
       )
       (match_operand 0 "general_operand")))

;; Test for valid PIC call operand
(define_predicate "call_insn_plt_operand"
  (match_test "PLT_ADDR_P (op)"))
