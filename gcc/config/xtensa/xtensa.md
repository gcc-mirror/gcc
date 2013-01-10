;; GCC machine description for Tensilica's Xtensa architecture.
;; Copyright (C) 2001-2013 Free Software Foundation, Inc.
;; Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


(define_constants [
  (A0_REG		0)
  (A1_REG		1)
  (A7_REG		7)
  (A8_REG		8)

  (UNSPEC_NOP		2)
  (UNSPEC_PLT		3)
  (UNSPEC_RET_ADDR	4)
  (UNSPEC_TPOFF		5)
  (UNSPEC_DTPOFF	6)
  (UNSPEC_TLS_FUNC	7)
  (UNSPEC_TLS_ARG	8)
  (UNSPEC_TLS_CALL	9)
  (UNSPEC_TP		10)
  (UNSPEC_MEMW		11)

  (UNSPECV_SET_FP	1)
  (UNSPECV_ENTRY	2)
  (UNSPECV_S32RI	4)
  (UNSPECV_S32C1I	5)
  (UNSPECV_EH_RETURN	6)
  (UNSPECV_SET_TP	7)
])

;; This code iterator allows signed and unsigned widening multiplications
;; to use the same template.
(define_code_iterator any_extend [sign_extend zero_extend])

;; <u> expands to an empty string when doing a signed operation and
;; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")])

;; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

;; This code iterator allows four integer min/max operations to be
;; generated from one template.
(define_code_iterator any_minmax [smin umin smax umax])

;; <minmax> expands to the opcode name for any_minmax operations.
(define_code_attr minmax [(smin "min") (umin "minu")
			  (smax "max") (umax "maxu")])

;; This code iterator is for floating-point comparisons.
(define_code_iterator any_scc_sf [eq lt le uneq unlt unle unordered])
(define_code_attr scc_sf [(eq "oeq") (lt "olt") (le "ole") 
			  (uneq "ueq") (unlt "ult") (unle "ule")
			  (unordered "un")])

;; This iterator and attribute allow to combine most atomic operations.
(define_code_iterator ATOMIC [and ior xor plus minus mult])
(define_code_attr atomic [(and "and") (ior "ior") (xor "xor") 
			  (plus "add") (minus "sub") (mult "nand")])

;; This mode iterator allows the HI and QI patterns to be defined from
;; the same template.
(define_mode_iterator HQI [HI QI])


;; Attributes.

(define_attr "type"
  "unknown,jump,call,load,store,move,arith,multi,nop,farith,fmadd,fdiv,fsqrt,fconv,fload,fstore,mul16,mul32,div32,mac16,rsr,wsr,entry"
  (const_string "unknown"))

(define_attr "mode"
  "unknown,none,QI,HI,SI,DI,SF,DF,BL"
  (const_string "unknown"))

(define_attr "length" "" (const_int 1))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])


;; Pipeline model.

;; The Xtensa basically has simple 5-stage RISC pipeline.
;; Most instructions complete in 1 cycle, and it is OK to assume that
;; everything is fully pipelined.  The exceptions have special insn
;; reservations in the pipeline description below.  The Xtensa can
;; issue one instruction per cycle, so defining CPU units is unnecessary.

(define_insn_reservation "xtensa_any_insn" 1
			 (eq_attr "type" "!load,fload,rsr,mul16,mul32,fmadd,fconv")
			 "nothing")

(define_insn_reservation "xtensa_memory" 2
			 (eq_attr "type" "load,fload")
			 "nothing")

(define_insn_reservation "xtensa_sreg" 2
			 (eq_attr "type" "rsr")
			 "nothing")

(define_insn_reservation "xtensa_mul16" 2
			 (eq_attr "type" "mul16")
			 "nothing")

(define_insn_reservation "xtensa_mul32" 2
			 (eq_attr "type" "mul32")
			 "nothing")

(define_insn_reservation "xtensa_fmadd" 4
			 (eq_attr "type" "fmadd")
			 "nothing")

(define_insn_reservation "xtensa_fconv" 2
			 (eq_attr "type" "fconv")
			 "nothing")

;; Include predicates and constraints.

(include "predicates.md")
(include "constraints.md")


;; Addition.

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=D,D,a,a,a")
	(plus:SI (match_operand:SI 1 "register_operand" "%d,d,r,r,r")
		 (match_operand:SI 2 "add_operand" "d,O,r,J,N")))]
  ""
  "@
   add.n\t%0, %1, %2
   addi.n\t%0, %1, %d2
   add\t%0, %1, %2
   addi\t%0, %1, %d2
   addmi\t%0, %1, %x2"
  [(set_attr "type"	"arith,arith,arith,arith,arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2,2,3,3,3")])

(define_insn "*addx"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			  (match_operand:SI 3 "addsubx_operand" "i"))
		 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_ADDX"
  "addx%3\t%0, %1, %2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "%f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "add.s\t%0, %1, %2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Subtraction.

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
        (minus:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "register_operand" "r")))]
  ""
  "sub\t%0, %1, %2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "*subx"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(minus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			   (match_operand:SI 3 "addsubx_operand" "i"))
		  (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_ADDX"
  "subx%3\t%0, %1, %2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "sub.s\t%0, %1, %2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Multiplication.

(define_expand "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		 (any_extend:DI (match_operand:SI 2 "register_operand"))))]
  "TARGET_MUL32_HIGH"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
  emit_insn (gen_<u>mulsi3_highpart (gen_highpart (SImode, operands[0]),
				     operands[1], operands[2]));
  emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]), temp));
  DONE;
})

(define_insn "<u>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "%r"))
		   (any_extend:DI (match_operand:SI 2 "register_operand" "r")))
	  (const_int 32))))]
  "TARGET_MUL32_HIGH"
  "mul<su>h\t%0, %1, %2"
  [(set_attr "type"	"mul32")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(mult:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_MUL32"
  "mull\t%0, %1, %2"
  [(set_attr "type"	"mul32")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=C,A")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "%r,r"))
		 (sign_extend:SI
		  (match_operand:HI 2 "register_operand" "r,r"))))]
  "TARGET_MUL16 || TARGET_MAC16"
  "@
   mul16s\t%0, %1, %2
   mul.aa.ll\t%1, %2"
  [(set_attr "type"	"mul16,mac16")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=C,A")
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "%r,r"))
		 (zero_extend:SI
		  (match_operand:HI 2 "register_operand" "r,r"))))]
  "TARGET_MUL16 || TARGET_MAC16"
  "@
   mul16u\t%0, %1, %2
   umul.aa.ll\t%1, %2"
  [(set_attr "type"	"mul16,mac16")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_insn "muladdhisi"
  [(set (match_operand:SI 0 "register_operand" "=A")
	(plus:SI (mult:SI (sign_extend:SI
			   (match_operand:HI 1 "register_operand" "%r"))
			  (sign_extend:SI
			   (match_operand:HI 2 "register_operand" "r")))
		 (match_operand:SI 3 "register_operand" "0")))]
  "TARGET_MAC16"
  "mula.aa.ll\t%1, %2"
  [(set_attr "type"	"mac16")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "mulsubhisi"
  [(set (match_operand:SI 0 "register_operand" "=A")
	(minus:SI (match_operand:SI 1 "register_operand" "0")
		  (mult:SI (sign_extend:SI
			    (match_operand:HI 2 "register_operand" "%r"))
			   (sign_extend:SI
			    (match_operand:HI 3 "register_operand" "r")))))]
  "TARGET_MAC16"
  "muls.aa.ll\t%2, %3"
  [(set_attr "type"	"mac16")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "%f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "mul.s\t%0, %1, %2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "fmasf4"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(fma:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")
		(match_operand:SF 3 "register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "madd.s\t%0, %1, %2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

;; Note that (C - A*B) = (-A*B + C)
(define_insn "fnmasf4"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(fma:SF (neg:SF (match_operand:SF 1 "register_operand" "f"))
		(match_operand:SF 2 "register_operand" "f")
		(match_operand:SF 3 "register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "msub.s\t%0, %1, %2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Division.

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "register_operand" "r")))]
  "TARGET_DIV32"
  "quos\t%0, %1, %2"
  [(set_attr "type"	"div32")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(udiv:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_DIV32"
  "quou\t%0, %1, %2"
  [(set_attr "type"	"div32")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT_DIV"
  "div.s\t%0, %1, %2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "*recipsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "const_float_1_operand" "")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT_RECIP && flag_unsafe_math_optimizations"
  "recip.s\t%0, %2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Remainders.

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(mod:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "register_operand" "r")))]
  "TARGET_DIV32"
  "rems\t%0, %1, %2"
  [(set_attr "type"	"div32")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(umod:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_DIV32"
  "remu\t%0, %1, %2"
  [(set_attr "type"	"div32")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])


;; Square roots.

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT_SQRT"
  "sqrt.s\t%0, %1"
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "*rsqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "const_float_1_operand" "")
		(sqrt:SF (match_operand:SF 2 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT_RSQRT && flag_unsafe_math_optimizations"
  "rsqrt.s\t%0, %2"
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Absolute value.

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(abs:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_ABS"
  "abs\t%0, %1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "abs.s\t%0, %1"
  [(set_attr "type"	"farith")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Min and max.

(define_insn "<code>si3"
  [(set (match_operand:SI 0 "register_operand" "=a")
        (any_minmax:SI (match_operand:SI 1 "register_operand" "%r")
		       (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_MINMAX"
  "<minmax>\t%0, %1, %2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])


;; Count leading/trailing zeros and find first bit.

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(clz:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_NSA"
  "nsau\t%0, %1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_expand "ctzsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(ctz:SI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_NSA"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_negsi2 (temp, operands[1]));
  emit_insn (gen_andsi3 (temp, temp, operands[1]));
  emit_insn (gen_clzsi2 (temp, temp));
  emit_insn (gen_negsi2 (temp, temp));
  emit_insn (gen_addsi3 (operands[0], temp, GEN_INT (31)));
  DONE;
})

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(ffs:SI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_NSA"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_negsi2 (temp, operands[1]));
  emit_insn (gen_andsi3 (temp, temp, operands[1]));
  emit_insn (gen_clzsi2 (temp, temp));
  emit_insn (gen_negsi2 (temp, temp));
  emit_insn (gen_addsi3 (operands[0], temp, GEN_INT (32)));
  DONE;
})


;; Negation and one's complement.

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "neg\t%0, %1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(not:SI (match_operand:SI 1 "register_operand" "")))]
  ""
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_movsi (temp, constm1_rtx));
  emit_insn (gen_xorsi3 (operands[0], temp, operands[1]));
  DONE;
})

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "neg.s\t%0, %1"
  [(set_attr "type"	"farith")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Logical instructions.

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(and:SI (match_operand:SI 1 "register_operand" "%r,r")
		(match_operand:SI 2 "mask_operand" "P,r")))]
  ""
  "@
   extui\t%0, %1, 0, %K2
   and\t%0, %1, %2"
  [(set_attr "type"	"arith,arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(ior:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "or\t%0, %1, %2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(xor:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "xor\t%0, %1, %2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])


;; Zero-extend instructions.

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(zero_extend:SI (match_operand:HI 1 "nonimmed_operand" "r,U")))]
  ""
  "@
   extui\t%0, %1, 0, 16
   l16ui\t%0, %1"
  [(set_attr "type"	"arith,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(zero_extend:SI (match_operand:QI 1 "nonimmed_operand" "r,U")))]
  ""
  "@
   extui\t%0, %1, 0, 8
   l8ui\t%0, %1"
  [(set_attr "type"	"arith,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])


;; Sign-extend instructions.

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
{
  if (sext_operand (operands[1], HImode))
    emit_insn (gen_extendhisi2_internal (operands[0], operands[1]));
  else
    xtensa_extend_reg (operands[0], operands[1]);
  DONE;
})

(define_insn "extendhisi2_internal"
  [(set (match_operand:SI 0 "register_operand" "=B,a")
	(sign_extend:SI (match_operand:HI 1 "sext_operand" "r,U")))]
  ""
  "@
   sext\t%0, %1, 15
   l16si\t%0, %1"
  [(set_attr "type"	"arith,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
{
  if (TARGET_SEXT)
    emit_insn (gen_extendqisi2_internal (operands[0], operands[1]));
  else
    xtensa_extend_reg (operands[0], operands[1]);
  DONE;
})

(define_insn "extendqisi2_internal"
  [(set (match_operand:SI 0 "register_operand" "=B")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  "TARGET_SEXT"
  "sext\t%0, %1, 7"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])


;; Field extract instructions.

(define_expand "extv"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  "TARGET_SEXT"
{
  if (!sext_fldsz_operand (operands[2], SImode))
    FAIL;

  /* We could expand to a right shift followed by SEXT but that's
     no better than the standard left and right shift sequence.  */
  if (!lsbitnum_operand (operands[3], SImode))
    FAIL;

  emit_insn (gen_extv_internal (operands[0], operands[1],
				operands[2], operands[3]));
  DONE;
})

(define_insn "extv_internal"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "sext_fldsz_operand" "i")
			 (match_operand:SI 3 "lsbitnum_operand" "i")))]
  "TARGET_SEXT"
{
  int fldsz = INTVAL (operands[2]);
  operands[2] = GEN_INT (fldsz - 1);
  return "sext\t%0, %1, %2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_expand "extzv"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  ""
{
  if (!extui_fldsz_operand (operands[2], SImode))
    FAIL;
  emit_insn (gen_extzv_internal (operands[0], operands[1],
				 operands[2], operands[3]));
  DONE;
})

(define_insn "extzv_internal"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "extui_fldsz_operand" "i")
			 (match_operand:SI 3 "const_int_operand" "i")))]
  ""
{
  int shift;
  if (BITS_BIG_ENDIAN)
    shift = (32 - (INTVAL (operands[2]) + INTVAL (operands[3]))) & 0x1f;
  else
    shift = INTVAL (operands[3]) & 0x1f;
  operands[3] = GEN_INT (shift);
  return "extui\t%0, %1, %3, %2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])


;; Conversions.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "trunc.s\t%0, %1, 0"
  [(set_attr "type"	"fconv")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "utrunc.s\t%0, %1, 0"
  [(set_attr "type"	"fconv")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "a")))]
  "TARGET_HARD_FLOAT"
  "float.s\t%0, %1, 0"
  [(set_attr "type"	"fconv")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unsigned_float:SF (match_operand:SI 1 "register_operand" "a")))]
  "TARGET_HARD_FLOAT"
  "ufloat.s\t%0, %1, 0"
  [(set_attr "type"	"fconv")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])


;; Data movement instructions.

;; 64-bit Integer moves

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmed_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
{
  if (CONSTANT_P (operands[1]) && !TARGET_CONST16)
    operands[1] = force_const_mem (DImode, operands[1]);

  if (!register_operand (operands[0], DImode)
      && !register_operand (operands[1], DImode))
    operands[1] = force_reg (DImode, operands[1]);

  operands[1] = xtensa_copy_incoming_a7 (operands[1]);
})

(define_insn_and_split "movdi_internal"
  [(set (match_operand:DI 0 "nonimmed_operand" "=a,W,a,a,U")
	(match_operand:DI 1 "move_operand" "r,i,T,U,r"))]
  "register_operand (operands[0], DImode)
   || register_operand (operands[1], DImode)"
  "#"
  "reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 1) (match_dup 3))]
{
  xtensa_split_operand_pair (operands, SImode);
  if (reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      rtx tmp;
      tmp = operands[0], operands[0] = operands[1], operands[1] = tmp;
      tmp = operands[2], operands[2] = operands[3], operands[3] = tmp;
    }
})

;; 32-bit Integer moves

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmed_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  if (xtensa_emit_move_sequence (operands, SImode))
    DONE;
})

(define_insn "movsi_internal"
  [(set (match_operand:SI 0 "nonimmed_operand" "=D,D,D,D,R,R,a,q,a,W,a,a,U,*a,*A")
	(match_operand:SI 1 "move_operand" "M,D,d,R,D,d,r,r,I,i,T,U,r,*A,*r"))]
  "xtensa_valid_move (SImode, operands)"
  "@
   movi.n\t%0, %x1
   mov.n\t%0, %1
   mov.n\t%0, %1
   %v1l32i.n\t%0, %1
   %v0s32i.n\t%1, %0
   %v0s32i.n\t%1, %0
   mov\t%0, %1
   movsp\t%0, %1
   movi\t%0, %x1
   const16\t%0, %t1\;const16\t%0, %b1
   %v1l32r\t%0, %1
   %v1l32i\t%0, %1
   %v0s32i\t%1, %0
   rsr\t%0, ACCLO
   wsr\t%1, ACCLO"
  [(set_attr "type" "move,move,move,load,store,store,move,move,move,move,load,load,store,rsr,wsr")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2,2,2,2,2,2,3,3,3,6,3,3,3,3,3")])

;; 16-bit Integer moves

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmed_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
{
  if (xtensa_emit_move_sequence (operands, HImode))
    DONE;
})

(define_insn "movhi_internal"
  [(set (match_operand:HI 0 "nonimmed_operand" "=D,D,a,a,a,U,*a,*A")
	(match_operand:HI 1 "move_operand" "M,d,r,I,U,r,*A,*r"))]
  "xtensa_valid_move (HImode, operands)"
  "@
   movi.n\t%0, %x1
   mov.n\t%0, %1
   mov\t%0, %1
   movi\t%0, %x1
   %v1l16ui\t%0, %1
   %v0s16i\t%1, %0
   rsr\t%0, ACCLO
   wsr\t%1, ACCLO"
  [(set_attr "type"	"move,move,move,move,load,store,rsr,wsr")
   (set_attr "mode"	"HI")
   (set_attr "length"	"2,2,3,3,3,3,3,3")])

;; 8-bit Integer moves

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmed_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
{
  if (xtensa_emit_move_sequence (operands, QImode))
    DONE;
})

(define_insn "movqi_internal"
  [(set (match_operand:QI 0 "nonimmed_operand" "=D,D,a,a,a,U,*a,*A")
	(match_operand:QI 1 "move_operand" "M,d,r,I,U,r,*A,*r"))]
  "xtensa_valid_move (QImode, operands)"
  "@
   movi.n\t%0, %x1
   mov.n\t%0, %1
   mov\t%0, %1
   movi\t%0, %x1
   %v1l8ui\t%0, %1
   %v0s8i\t%1, %0
   rsr\t%0, ACCLO
   wsr\t%1, ACCLO"
  [(set_attr "type"	"move,move,move,move,load,store,rsr,wsr")
   (set_attr "mode"	"QI")
   (set_attr "length"	"2,2,3,3,3,3,3,3")])

;; Sub-word reloads from the constant pool.

(define_expand "reload<mode>_literal"
  [(parallel [(match_operand:HQI 0 "register_operand" "=r")
	      (match_operand:HQI 1 "constantpool_operand" "")
	      (match_operand:SI 2 "register_operand" "=&r")])]
  ""
{
  rtx lit, scratch;
  unsigned word_off, byte_off;

  if (MEM_P (operands[1]))
    {
      lit = operands[1];
      word_off = 0;
      byte_off = 0;
    }
  else
    {
      gcc_assert (GET_CODE (operands[1]) == SUBREG);
      lit = SUBREG_REG (operands[1]);
      word_off = SUBREG_BYTE (operands[1]) & ~(UNITS_PER_WORD - 1);
      byte_off = SUBREG_BYTE (operands[1]) - word_off;
    }

  lit = adjust_address (lit, SImode, word_off);
  scratch = operands[2];
  emit_insn (gen_movsi (scratch, lit));
  emit_insn (gen_mov<mode> (operands[0],
			    gen_rtx_SUBREG (<MODE>mode, scratch, byte_off)));

  DONE;
})

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmed_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
{
  if (!TARGET_CONST16 && CONSTANT_P (operands[1]))
    operands[1] = force_const_mem (SFmode, operands[1]);

  if ((!register_operand (operands[0], SFmode)
       && !register_operand (operands[1], SFmode))
      || (FP_REG_P (xt_true_regnum (operands[0]))
	  && !(reload_in_progress | reload_completed)
	  && (constantpool_mem_p (operands[1])
	      || CONSTANT_P (operands[1]))))
    operands[1] = force_reg (SFmode, operands[1]);

  operands[1] = xtensa_copy_incoming_a7 (operands[1]);
})

(define_insn "movsf_internal"
  [(set (match_operand:SF 0 "nonimmed_operand" "=f,f,U,D,D,R,a,f,a,W,a,a,U")
	(match_operand:SF 1 "move_operand" "f,U,f,d,R,d,r,r,f,iF,T,U,r"))]
  "((register_operand (operands[0], SFmode)
     || register_operand (operands[1], SFmode))
    && !(FP_REG_P (xt_true_regnum (operands[0]))
         && (constantpool_mem_p (operands[1]) || CONSTANT_P (operands[1]))))"
  "@
   mov.s\t%0, %1
   %v1lsi\t%0, %1
   %v0ssi\t%1, %0
   mov.n\t%0, %1
   %v1l32i.n\t%0, %1
   %v0s32i.n\t%1, %0
   mov\t%0, %1
   wfr\t%0, %1
   rfr\t%0, %1
   const16\t%0, %t1\;const16\t%0, %b1
   %v1l32r\t%0, %1
   %v1l32i\t%0, %1
   %v0s32i\t%1, %0"
  [(set_attr "type"	"farith,fload,fstore,move,load,store,move,farith,farith,move,load,load,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3,3,3,2,2,2,3,3,3,6,3,3,3")])

(define_insn "*lsiu"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mem:SF (plus:SI (match_operand:SI 1 "register_operand" "+a")
			 (match_operand:SI 2 "fpmem_offset_operand" "i"))))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_HARD_FLOAT"
{
  if (TARGET_SERIALIZE_VOLATILE && volatile_refs_p (PATTERN (insn)))
    output_asm_insn ("memw", operands);
  return "lsiu\t%0, %1, %2";
}
  [(set_attr "type"	"fload")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "*ssiu"
  [(set (mem:SF (plus:SI (match_operand:SI 0 "register_operand" "+a")
			 (match_operand:SI 1 "fpmem_offset_operand" "i")))
	(match_operand:SF 2 "register_operand" "f"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_dup 1)))]
  "TARGET_HARD_FLOAT"
{
  if (TARGET_SERIALIZE_VOLATILE && volatile_refs_p (PATTERN (insn)))
    output_asm_insn ("memw", operands);
  return "ssiu\t%2, %0, %1";
}
  [(set_attr "type"	"fstore")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmed_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
{
  if (CONSTANT_P (operands[1]) && !TARGET_CONST16)
    operands[1] = force_const_mem (DFmode, operands[1]);

  if (!register_operand (operands[0], DFmode)
      && !register_operand (operands[1], DFmode))
    operands[1] = force_reg (DFmode, operands[1]);

  operands[1] = xtensa_copy_incoming_a7 (operands[1]);
})

(define_insn_and_split "movdf_internal"
  [(set (match_operand:DF 0 "nonimmed_operand" "=a,W,a,a,U")
	(match_operand:DF 1 "move_operand" "r,iF,T,U,r"))]
  "register_operand (operands[0], DFmode)
   || register_operand (operands[1], DFmode)"
  "#"
  "reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 1) (match_dup 3))]
{
  xtensa_split_operand_pair (operands, SFmode);
  if (reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      rtx tmp;
      tmp = operands[0], operands[0] = operands[1], operands[1] = tmp;
      tmp = operands[2], operands[2] = operands[3], operands[3] = tmp;
    }
})

;; Block moves

(define_expand "movmemsi"
  [(parallel [(set (match_operand:BLK 0 "" "")
		   (match_operand:BLK 1 "" ""))
	      (use (match_operand:SI 2 "arith_operand" ""))
	      (use (match_operand:SI 3 "const_int_operand" ""))])]
  ""
{
  if (!xtensa_expand_block_move (operands))
    FAIL;
  DONE;
})


;; Shift instructions.

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "arith_operand" "")))]
  ""
{
  operands[1] = xtensa_copy_incoming_a7 (operands[1]);
})

(define_insn "ashlsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(ashift:SI (match_operand:SI 1 "register_operand" "r,r")
		   (match_operand:SI 2 "arith_operand" "J,r")))]
  ""      
  "@
   slli\t%0, %1, %R2
   ssl\t%2\;sll\t%0, %1"
  [(set_attr "type"	"arith,arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,6")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "arith_operand" "J,r")))]
  ""
  "@
   srai\t%0, %1, %R2
   ssr\t%2\;sra\t%0, %1"
  [(set_attr "type"	"arith,arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,6")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "arith_operand" "J,r")))]
  ""
{
  if (which_alternative == 0)
    {
      if ((INTVAL (operands[2]) & 0x1f) < 16)
        return "srli\t%0, %1, %R2";
      else
      	return "extui\t%0, %1, %R2, %L2";
    }
  return "ssr\t%2\;srl\t%0, %1";
}
  [(set_attr "type"	"arith,arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,6")])

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(rotate:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "arith_operand" "J,r")))]
  ""
  "@
   ssai\t%L2\;src\t%0, %1, %1
   ssl\t%2\;src\t%0, %1, %1"
  [(set_attr "type"	"multi,multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6,6")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "arith_operand" "J,r")))]
  ""
  "@
   ssai\t%R2\;src\t%0, %1, %1
   ssr\t%2\;src\t%0, %1, %1"
  [(set_attr "type"	"multi,multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6,6")])


;; Comparisons.

;; Conditional branches.

(define_expand "cbranchsi4"
  [(match_operator 0 "comparison_operator"
    [(match_operand:SI 1 "register_operand")
     (match_operand:SI 2 "nonmemory_operand")])
   (match_operand 3 "")]
  ""
{
  xtensa_expand_conditional_branch (operands, SImode);
  DONE;
})

(define_expand "cbranchsf4"
  [(match_operator 0 "comparison_operator"
    [(match_operand:SF 1 "register_operand")
     (match_operand:SF 2 "register_operand")])
   (match_operand 3 "")]
  "TARGET_HARD_FLOAT"
{
  xtensa_expand_conditional_branch (operands, SFmode);
  DONE;
})

;; Branch patterns for standard integer comparisons

(define_insn "*btrue"
  [(set (pc)
	(if_then_else (match_operator 3 "branch_operator"
		       [(match_operand:SI 0 "register_operand" "r,r")
			(match_operand:SI 1 "branch_operand" "K,r")])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  return xtensa_emit_branch (false, which_alternative == 0, operands);
}
  [(set_attr "type"	"jump,jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3,3")])

(define_insn "*bfalse"
  [(set (pc)
	(if_then_else (match_operator 3 "branch_operator"
		       [(match_operand:SI 0 "register_operand" "r,r")
			(match_operand:SI 1 "branch_operand" "K,r")])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
{
  return xtensa_emit_branch (true, which_alternative == 0, operands);
}
  [(set_attr "type"	"jump,jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3,3")])

(define_insn "*ubtrue"
  [(set (pc)
	(if_then_else (match_operator 3 "ubranch_operator"
		       [(match_operand:SI 0 "register_operand" "r,r")
			(match_operand:SI 1 "ubranch_operand" "L,r")])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  return xtensa_emit_branch (false, which_alternative == 0, operands);
}
  [(set_attr "type"	"jump,jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3,3")])

(define_insn "*ubfalse"
  [(set (pc)
	(if_then_else (match_operator 3 "ubranch_operator"
			 [(match_operand:SI 0 "register_operand" "r,r")
			  (match_operand:SI 1 "ubranch_operand" "L,r")])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
{
  return xtensa_emit_branch (true, which_alternative == 0, operands);
}
  [(set_attr "type"	"jump,jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3,3")])

;; Branch patterns for bit testing

(define_insn "*bittrue"
  [(set (pc)
	(if_then_else (match_operator 3 "boolean_operator"
			[(zero_extract:SI
			    (match_operand:SI 0 "register_operand" "r,r")
			    (const_int 1)
			    (match_operand:SI 1 "arith_operand" "J,r"))
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  return xtensa_emit_bit_branch (false, which_alternative == 0, operands);
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "*bitfalse"
  [(set (pc)
	(if_then_else (match_operator 3 "boolean_operator"
			[(zero_extract:SI
			    (match_operand:SI 0 "register_operand" "r,r")
			    (const_int 1)
			    (match_operand:SI 1 "arith_operand" "J,r"))
			 (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
{
  return xtensa_emit_bit_branch (true, which_alternative == 0, operands);
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "*masktrue"
  [(set (pc)
	(if_then_else (match_operator 3 "boolean_operator"
		 [(and:SI (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "register_operand" "r"))
		  (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  switch (GET_CODE (operands[3]))
    {
    case EQ:		return "bnone\t%0, %1, %2";
    case NE:		return "bany\t%0, %1, %2";
    default:		gcc_unreachable ();
    }
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "*maskfalse"
  [(set (pc)
	(if_then_else (match_operator 3 "boolean_operator"
		 [(and:SI (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "register_operand" "r"))
		  (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
{
  switch (GET_CODE (operands[3]))
    {
    case EQ:		return "bany\t%0, %1, %2";
    case NE:		return "bnone\t%0, %1, %2";
    default:		gcc_unreachable ();
    }
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])


;; Define the loop insns used by bct optimization to represent the
;; start and end of a zero-overhead loop (in loop.c).  This start
;; template generates the loop insn; the end template doesn't generate
;; any instructions since loop end is handled in hardware.

(define_insn "zero_cost_loop_start"
  [(set (pc)
	(if_then_else (eq (match_operand:SI 0 "register_operand" "a")
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))
   (set (reg:SI 19)
	(plus:SI (match_dup 0) (const_int -1)))]
  ""
  "loopnez\t%0, %l1"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "zero_cost_loop_end"
  [(set (pc)
	(if_then_else (ne (reg:SI 19) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (set (reg:SI 19)
	(plus:SI (reg:SI 19) (const_int -1)))]
  ""
{
    xtensa_emit_loop_end (insn, operands);
    return "";
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])


;; Setting a register from a comparison.

(define_expand "cstoresi4"
  [(match_operand:SI 0 "register_operand")
   (match_operator 1 "xtensa_cstoresi_operator"
    [(match_operand:SI 2 "register_operand")
     (match_operand:SI 3 "nonmemory_operand")])]
  ""
{
  if (!xtensa_expand_scc (operands, SImode))
    FAIL;
  DONE;
})

(define_expand "cstoresf4"
  [(match_operand:SI 0 "register_operand")
   (match_operator:SI 1 "comparison_operator"
    [(match_operand:SF 2 "register_operand")
     (match_operand:SF 3 "register_operand")])]
  "TARGET_HARD_FLOAT"
{
  if (!xtensa_expand_scc (operands, SFmode))
    FAIL;
  DONE;
})



;; Conditional moves.

(define_expand "movsicc"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI (match_operand 1 "comparison_operator" "")
			 (match_operand:SI 2 "register_operand" "")
			 (match_operand:SI 3 "register_operand" "")))]
  ""
{
  if (!xtensa_expand_conditional_move (operands, 0))
    FAIL;
  DONE;
})

(define_expand "movsfcc"
  [(set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (match_operand 1 "comparison_operator" "")
			 (match_operand:SF 2 "register_operand" "")
			 (match_operand:SF 3 "register_operand" "")))]
  ""
{
  if (!xtensa_expand_conditional_move (operands, 1))
    FAIL;
  DONE;
})

(define_insn "movsicc_internal0"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(if_then_else:SI (match_operator 4 "branch_operator"
			   [(match_operand:SI 1 "register_operand" "r,r")
			    (const_int 0)])
			 (match_operand:SI 2 "register_operand" "r,0")
			 (match_operand:SI 3 "register_operand" "0,r")))]
  ""
{
  return xtensa_emit_movcc (which_alternative == 1, false, false, operands);
}
  [(set_attr "type"	"move,move")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_insn "movsicc_internal1"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(if_then_else:SI (match_operator 4 "boolean_operator"
			   [(match_operand:CC 1 "register_operand" "b,b")
			    (const_int 0)])
			 (match_operand:SI 2 "register_operand" "r,0")
			 (match_operand:SI 3 "register_operand" "0,r")))]
  "TARGET_BOOLEANS"
{
  return xtensa_emit_movcc (which_alternative == 1, false, true, operands);
}
  [(set_attr "type"	"move,move")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_insn "movsfcc_internal0"
  [(set (match_operand:SF 0 "register_operand" "=a,a,f,f")
	(if_then_else:SF (match_operator 4 "branch_operator"
			   [(match_operand:SI 1 "register_operand" "r,r,r,r")
			    (const_int 0)])
			 (match_operand:SF 2 "register_operand" "r,0,f,0")
			 (match_operand:SF 3 "register_operand" "0,r,0,f")))]
  ""
{
  return xtensa_emit_movcc ((which_alternative & 1) == 1,
			    which_alternative >= 2, false, operands);
}
  [(set_attr "type"	"move,move,move,move")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3,3,3,3")])

(define_insn "movsfcc_internal1"
  [(set (match_operand:SF 0 "register_operand" "=a,a,f,f")
	(if_then_else:SF (match_operator 4 "boolean_operator"
			   [(match_operand:CC 1 "register_operand" "b,b,b,b")
			    (const_int 0)])
			 (match_operand:SF 2 "register_operand" "r,0,f,0")
			 (match_operand:SF 3 "register_operand" "0,r,0,f")))]
  "TARGET_BOOLEANS"
{
  return xtensa_emit_movcc ((which_alternative & 1) == 1,
			    which_alternative >= 2, true, operands);
}
  [(set_attr "type"	"move,move,move,move")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3,3,3,3")])


;; Floating-point comparisons.

(define_insn "s<code>_sf"
  [(set (match_operand:CC 0 "register_operand" "=b")
	(any_scc_sf:CC (match_operand:SF 1 "register_operand" "f")
		       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "<scc_sf>.s\t%0, %1, %2"
  [(set_attr "type"	"farith")
   (set_attr "mode"	"BL")
   (set_attr "length"	"3")])


;; Unconditional branches.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "j\t%l0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_expand "indirect_jump"
  [(set (pc)
	(match_operand 0 "register_operand" ""))]
  ""
{
  rtx dest = operands[0];
  if (GET_CODE (dest) != REG || GET_MODE (dest) != Pmode)
    operands[0] = copy_to_mode_reg (Pmode, dest);

  emit_jump_insn (gen_indirect_jump_internal (dest));
  DONE;
})

(define_insn "indirect_jump_internal"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jx\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])


(define_expand "tablejump"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (label_ref (match_operand 1 "" "")))]
   ""
{
  rtx target = operands[0];
  if (flag_pic)
    {
      /* For PIC, the table entry is relative to the start of the table.  */
      rtx label = gen_reg_rtx (SImode);
      target = gen_reg_rtx (SImode);
      emit_move_insn (label, gen_rtx_LABEL_REF (SImode, operands[1]));
      emit_insn (gen_addsi3 (target, operands[0], label));
    }
  emit_jump_insn (gen_tablejump_internal (target, operands[1]));
  DONE;
})

(define_insn "tablejump_internal"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jx\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])


;; Function calls.

(define_expand "sym_PLT"
  [(const (unspec [(match_operand:SI 0 "" "")] UNSPEC_PLT))]
  ""
  "")

(define_expand "call"
  [(call (match_operand 0 "memory_operand" "")
	 (match_operand 1 "" ""))]
  ""
{
  rtx addr = XEXP (operands[0], 0);
  if (flag_pic && GET_CODE (addr) == SYMBOL_REF
      && (!SYMBOL_REF_LOCAL_P (addr) || SYMBOL_REF_EXTERNAL_P (addr)))
    addr = gen_sym_PLT (addr);
  if (!call_insn_operand (addr, VOIDmode))
    XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, addr);
})

(define_insn "call_internal"
  [(call (mem (match_operand:SI 0 "call_insn_operand" "nir"))
	 (match_operand 1 "" "i"))]
  ""
{
  return xtensa_emit_call (0, operands);
}
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_expand "call_value"
  [(set (match_operand 0 "register_operand" "")
	(call (match_operand 1 "memory_operand" "")
	      (match_operand 2 "" "")))]
  ""
{
  rtx addr = XEXP (operands[1], 0);
  if (flag_pic && GET_CODE (addr) == SYMBOL_REF
      && (!SYMBOL_REF_LOCAL_P (addr) || SYMBOL_REF_EXTERNAL_P (addr)))
    addr = gen_sym_PLT (addr);
  if (!call_insn_operand (addr, VOIDmode))
    XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, addr);
})

(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "=a")
        (call (mem (match_operand:SI 1 "call_insn_operand" "nir"))
              (match_operand 2 "" "i")))]
  ""
{
  return xtensa_emit_call (1, operands);
}
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "entry"
  [(set (reg:SI A1_REG)
	(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "i")]
			    UNSPECV_ENTRY))]
  ""
  "entry\tsp, %0"
  [(set_attr "type"	"entry")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "return"
  [(return)
   (use (reg:SI A0_REG))]
  "reload_completed"
{
  return (TARGET_DENSITY ? "retw.n" : "retw");
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"2")])


;; Miscellaneous instructions.

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  xtensa_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(return)]
  ""
{
  emit_jump_insn (gen_return ());
  DONE;
})

(define_insn "nop"
  [(const_int 0)]
  ""
{
  return (TARGET_DENSITY ? "nop.n" : "nop");
}
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_expand "nonlocal_goto"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "general_operand" "")
   (match_operand:SI 2 "general_operand" "")
   (match_operand:SI 3 "" "")]
  ""
{
  xtensa_expand_nonlocal_goto (operands);
  DONE;
})

;; Stuff an address into the return address register along with the window
;; size in the high bits.  Because we don't have the window size of the
;; previous frame, assume the function called out with a CALL8 since that
;; is what compilers always use.  Note: __builtin_frob_return_addr has
;; already been applied to the handler, but the generic version doesn't
;; allow us to frob it quite enough, so we just frob here.

(define_insn_and_split "eh_return"
  [(set (reg:SI A0_REG)
	(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")]
			    UNSPECV_EH_RETURN))
   (clobber (match_scratch:SI 1 "=r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 1) (ashift:SI (match_dup 0) (const_int 2)))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 2)))
   (set (reg:SI A0_REG) (rotatert:SI (match_dup 1) (const_int 2)))]
  "")

;; Setting up a frame pointer is tricky for Xtensa because GCC doesn't
;; know if a frame pointer is required until the reload pass, and
;; because there may be an incoming argument value in the hard frame
;; pointer register (a7).  If there is an incoming argument in that
;; register, the "set_frame_ptr" insn gets inserted immediately after
;; the insn that copies the incoming argument to a pseudo or to the
;; stack.  This serves several purposes here: (1) it keeps the
;; optimizer from copy-propagating or scheduling the use of a7 as an
;; incoming argument away from the beginning of the function; (2) we
;; can use a post-reload splitter to expand away the insn if a frame
;; pointer is not required, so that the post-reload scheduler can do
;; the right thing; and (3) it makes it easy for the prologue expander
;; to search for this insn to determine whether it should add a new insn
;; to set up the frame pointer.

(define_insn "set_frame_ptr"
  [(set (reg:SI A7_REG) (unspec_volatile:SI [(const_int 0)] UNSPECV_SET_FP))]
  ""
{
  if (frame_pointer_needed)
    return "mov\ta7, sp";
  return "";
}
  [(set_attr "type"	"move")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

;; Post-reload splitter to remove fp assignment when it's not needed.
(define_split
  [(set (reg:SI A7_REG) (unspec_volatile:SI [(const_int 0)] UNSPECV_SET_FP))]
  "reload_completed && !frame_pointer_needed"
  [(unspec [(const_int 0)] UNSPEC_NOP)]
  "")

;; The preceding splitter needs something to split the insn into;
;; things start breaking if the result is just a "use" so instead we
;; generate the following insn.
(define_insn "*unspec_nop"
  [(unspec [(const_int 0)] UNSPEC_NOP)]
  ""
  ""
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])


;; TLS support

(define_expand "sym_TPOFF"
  [(const (unspec [(match_operand:SI 0 "" "")] UNSPEC_TPOFF))]
  ""
  "")

(define_expand "sym_DTPOFF"
  [(const (unspec [(match_operand:SI 0 "" "")] UNSPEC_DTPOFF))]
  ""
  "")

(define_insn "get_thread_pointersi"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(unspec:SI [(const_int 0)] UNSPEC_TP))]
  "TARGET_THREADPTR"
  "rur\t%0, THREADPTR"
  [(set_attr "type"	"rsr")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "set_thread_pointersi"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")]
		    UNSPECV_SET_TP)]
  "TARGET_THREADPTR"
  "wur\t%0, THREADPTR"
  [(set_attr "type"	"wsr")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "tls_func"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(unspec:SI [(match_operand:SI 1 "tls_symbol_operand" "")]
		   UNSPEC_TLS_FUNC))]
  "TARGET_THREADPTR && HAVE_AS_TLS"
  "movi\t%0, %1@TLSFUNC"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "tls_arg"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(unspec:SI [(match_operand:SI 1 "tls_symbol_operand" "")]
		   UNSPEC_TLS_ARG))]
  "TARGET_THREADPTR && HAVE_AS_TLS"
  "movi\t%0, %1@TLSARG"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "tls_call"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(call (mem:SI (unspec:SI [(match_operand:SI 1 "register_operand" "r")
				  (match_operand:SI 2 "tls_symbol_operand" "")]
				  UNSPEC_TLS_CALL))
	      (match_operand 3 "" "i")))]
  "TARGET_THREADPTR && HAVE_AS_TLS"
  "callx8.tls %1, %2@TLSCALL"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])


;; Instructions for the Xtensa "boolean" option.

(define_insn "*booltrue"
  [(set (pc)
	(if_then_else (match_operator 2 "boolean_operator"
			 [(match_operand:CC 0 "register_operand" "b")
			  (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "TARGET_BOOLEANS"
{
  if (GET_CODE (operands[2]) == EQ)
    return "bf\t%0, %1";
  else
    return "bt\t%0, %1";
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "*boolfalse"
  [(set (pc)
	(if_then_else (match_operator 2 "boolean_operator"
			 [(match_operand:CC 0 "register_operand" "b")
			  (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  "TARGET_BOOLEANS"
{
  if (GET_CODE (operands[2]) == EQ)
    return "bt\t%0, %1";
  else
    return "bf\t%0, %1";
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])


;; Atomic operations

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMW))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMW))]
  ""
  "memw"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

;; sync_lock_release is only implemented for SImode.
;; For other modes, just use the default of a store with a memory_barrier.
(define_insn "sync_lock_releasesi"
  [(set (match_operand:SI 0 "mem_operand" "=U")
	(unspec_volatile:SI
	  [(match_operand:SI 1 "register_operand" "r")]
	  UNSPECV_S32RI))]
  "TARGET_RELEASE_SYNC"
  "s32ri\t%1, %0"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "sync_compare_and_swapsi"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "=a")
	  (match_operand:SI 1 "mem_operand" "+U"))
     (set (match_dup 1)
	  (unspec_volatile:SI
	    [(match_dup 1)
	     (match_operand:SI 2 "register_operand" "r")
	     (match_operand:SI 3 "register_operand" "0")]
	    UNSPECV_S32C1I))])]
  "TARGET_S32C1I"
  "wsr\t%2, SCOMPARE1\;s32c1i\t%3, %1"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_expand "sync_compare_and_swap<mode>"
  [(parallel
    [(set (match_operand:HQI 0 "register_operand" "")
	  (match_operand:HQI 1 "mem_operand" ""))
     (set (match_dup 1)
	  (unspec_volatile:HQI
	    [(match_dup 1)
	     (match_operand:HQI 2 "register_operand" "")
	     (match_operand:HQI 3 "register_operand" "")]
	    UNSPECV_S32C1I))])]
  "TARGET_S32C1I"
{
  xtensa_expand_compare_and_swap (operands[0], operands[1],
				  operands[2], operands[3]);
  DONE;
})

(define_expand "sync_lock_test_and_set<mode>"
  [(match_operand:HQI 0 "register_operand")
   (match_operand:HQI 1 "memory_operand")
   (match_operand:HQI 2 "register_operand")]
  "TARGET_S32C1I"
{
  xtensa_expand_atomic (SET, operands[0], operands[1], operands[2], false);
  DONE;
})

(define_expand "sync_<atomic><mode>"
  [(set (match_operand:HQI 0 "memory_operand")
	(ATOMIC:HQI (match_dup 0)
		    (match_operand:HQI 1 "register_operand")))]
  "TARGET_S32C1I"
{
  xtensa_expand_atomic (<CODE>, NULL_RTX, operands[0], operands[1], false);
  DONE;
})

(define_expand "sync_old_<atomic><mode>"
  [(set (match_operand:HQI 0 "register_operand")
	(match_operand:HQI 1 "memory_operand"))
   (set (match_dup 1)
	(ATOMIC:HQI (match_dup 1)
		    (match_operand:HQI 2 "register_operand")))]
  "TARGET_S32C1I"
{
  xtensa_expand_atomic (<CODE>, operands[0], operands[1], operands[2], false);
  DONE;
})

(define_expand "sync_new_<atomic><mode>"
  [(set (match_operand:HQI 0 "register_operand")
	(ATOMIC:HQI (match_operand:HQI 1 "memory_operand")
		    (match_operand:HQI 2 "register_operand"))) 
   (set (match_dup 1) (ATOMIC:HQI (match_dup 1) (match_dup 2)))]
  "TARGET_S32C1I"
{
  xtensa_expand_atomic (<CODE>, operands[0], operands[1], operands[2], true);
  DONE;
})
