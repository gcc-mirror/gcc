;; ALU operations with zero extensions
;;
;; Copyright (C) 2015-2020 Free Software Foundation, Inc.
;; Contributed by Dimitar Dimitrov <dimitar@dinux.eu>
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

; All PRU ALU instructions automatically zero-extend their source operands,
; and zero-extract the result into the destination register.  This is
; described in the machine description by defining a separate pattern
; for each possible combination of zero_extend and mode for input operands.
;
; An unfortunate side effect is that quite a few invalid RTL patterns are
; generated.  For example:
;      ... (zero_extend:SI (match_operand:SI ...)) ...
; These patterns are harmless since no pass should generate such RTL.  This
; shortcut allows us to keep small and concise machine description patterns.


(define_subst_attr "alu2_zext"     "alu2_zext_subst"     "_z" "_noz")

(define_subst_attr "alu3_zext_op1" "alu3_zext_op1_subst" "_z1" "_noz1")
(define_subst_attr "alu3_zext_op2" "alu3_zext_op2_subst" "_z2" "_noz2")
(define_subst_attr "alu3_zext"     "alu3_zext_subst"     "_z" "_noz")

(define_subst_attr "bitalu_zext"   "bitalu_zext_subst"   "_z" "_noz")

(define_code_iterator ALUOP3 [plus minus and ior xor umin umax ashift lshiftrt])
(define_code_iterator ALUOP2 [neg not])

;; Arithmetic Operations

(define_insn "add_impl<EQD:mode><EQS0:mode><EQS1:mode>_<alu3_zext><alu3_zext_op1><alu3_zext_op2>"
  [(set (match_operand:EQD 0 "register_operand" "=r,r,r")
	(plus:EQD
	 (zero_extend:EQD
	  (match_operand:EQS0 1 "register_operand" "%r,r,r"))
	 (zero_extend:EQD
	  (match_operand:EQS1 2 "nonmemory_operand" "r,<EQS1:ubyte_constr>,M"))))]
  ""
  "@
   add\\t%0, %1, %2
   add\\t%0, %1, %u2
   sub\\t%0, %1, %n2"
  [(set_attr "type" "alu")])

(define_insn "sub_impl<EQD:mode><EQS0:mode><EQS1:mode>_<alu3_zext><alu3_zext_op1><alu3_zext_op2>"
  [(set (match_operand:EQD 0 "register_operand" "=r,r")
	(minus:EQD
	 (zero_extend:EQD
	  (match_operand:EQS0 1 "reg_or_ubyte_operand" "r,<EQS0:ubyte_constr>"))
	 (zero_extend:EQD
	  (match_operand:EQS1 2 "register_operand" "r,r"))))]
  ""
  "@
   sub\\t%0, %1, %2
   rsb\\t%0, %2, %u1"
  [(set_attr "type" "alu")])


(define_insn "neg_impl<EQD:mode><EQS0:mode>_<alu2_zext>"
  [(set (match_operand:EQD 0 "register_operand" "=r")
	(neg:EQD
	  (zero_extend:EQD (match_operand:EQS0 1 "register_operand" "r"))))]
  ""
  "rsb\\t%0, %1, 0"
  [(set_attr "type" "alu")])


(define_insn "one_cmpl_impl<EQD:mode><EQS0:mode>_<alu2_zext>"
  [(set (match_operand:EQD 0 "register_operand" "=r")
	(not:EQD
	  (zero_extend:EQD (match_operand:EQS0 1 "register_operand" "r"))))]
  ""
  "not\\t%0, %1"
  [(set_attr "type" "alu")])

; Specialized IOR/AND patterns for matching setbit/clearbit instructions.
;
; TODO - allow clrbit and setbit to support (1 << REG) constructs

(define_insn "clearbit_<EQD:mode><EQS0:mode>_<bitalu_zext>"
  [(set (match_operand:EQD 0 "register_operand" "=r")
	(and:EQD
	  (zero_extend:EQD
	    (match_operand:EQS0 1 "register_operand" "r"))
	  (match_operand:EQD 2 "single_zero_operand" "n")))]
  ""
  "clr\\t%0, %1, %V2"
  [(set_attr "type" "alu")])

(define_insn "setbit_<EQD:mode><EQS0:mode>_<bitalu_zext>"
  [(set (match_operand:EQD 0 "register_operand" "=r")
	(ior:EQD
	  (zero_extend:EQD
	    (match_operand:EQS0 1 "register_operand" "r"))
	  (match_operand:EQD 2 "single_one_operand" "n")))]
  ""
  "set\\t%0, %1, %T2"
  [(set_attr "type" "alu")])

; Regular ALU ops
(define_insn "<code>_impl<EQD:mode><EQS0:mode><EQS1:mode>_<alu3_zext><alu3_zext_op1><alu3_zext_op2>"
  [(set (match_operand:EQD 0 "register_operand" "=r")
	(LOGICAL:EQD
	  (zero_extend:EQD
	    (match_operand:EQS0 1 "register_operand" "%r"))
	  (zero_extend:EQD
	    (match_operand:EQS1 2 "reg_or_ubyte_operand" "r<EQS1:ubyte_constr>"))))]
  ""
  "<logical_asm>\\t%0, %1, %u2"
  [(set_attr "type" "alu")])

; Shift ALU ops
(define_insn "<shift_op>_impl<EQD:mode><EQS0:mode><EQS1:mode>_<alu3_zext><alu3_zext_op1><alu3_zext_op2>"
  [(set (match_operand:EQD 0 "register_operand" "=r")
	(SHIFT:EQD
	 (zero_extend:EQD (match_operand:EQS0 1 "register_operand" "r"))
	 (zero_extend:EQD (match_operand:EQS1 2 "shift_operand" "rL"))))]
  ""
  "<shift_asm>\\t%0, %1, %2"
  [(set_attr "type" "alu")])

;; Substitutions

(define_subst "alu2_zext_subst"
  [(set (match_operand:EQD 0)
	(ALUOP2:EQD (zero_extend:EQD (match_operand:EQD 1))))]
  ""
  [(set (match_dup 0)
	(ALUOP2:EQD (match_dup 1)))])

(define_subst "bitalu_zext_subst"
  [(set (match_operand:EQD 0)
	(ALUOP3:EQD (zero_extend:EQD (match_operand:EQD 1))
		    (match_operand:EQD 2)))]
  ""
  [(set (match_dup 0)
	(ALUOP3:EQD (match_dup 1)
		    (match_dup 2)))])

(define_subst "alu3_zext_subst"
  [(set (match_operand:EQD 0)
	(ALUOP3:EQD (zero_extend:EQD (match_operand:EQD 1))
		    (zero_extend:EQD (match_operand:EQD 2))))]
  ""
  [(set (match_dup 0)
	(ALUOP3:EQD (match_dup 1)
		    (match_dup 2)))])

(define_subst "alu3_zext_op1_subst"
  [(set (match_operand:EQD 0)
	(ALUOP3:EQD (zero_extend:EQD (match_operand:EQD 1))
		    (zero_extend:EQD (match_operand:EQS1 2))))]
  ""
  [(set (match_dup 0)
	(ALUOP3:EQD (match_dup 1)
		    (zero_extend:EQD (match_dup 2))))])

(define_subst "alu3_zext_op2_subst"
  [(set (match_operand:EQD 0)
	(ALUOP3:EQD (zero_extend:EQD (match_operand:EQS0 1))
		    (zero_extend:EQD (match_operand:EQD 2))))]
  ""
  [(set (match_dup 0)
	(ALUOP3:EQD (zero_extend:EQD (match_dup 1))
		    (match_dup 2)))])
