;; GCC machine description for Tensilica's Xtensa architecture.
;; Copyright (C) 2001-2022 Free Software Foundation, Inc.
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
  (A9_REG		9)

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
  (UNSPEC_LSETUP_START  12)
  (UNSPEC_LSETUP_END    13)
  (UNSPEC_FRAME_BLOCKAGE 14)

  (UNSPECV_SET_FP	1)
  (UNSPECV_ENTRY	2)
  (UNSPECV_S32RI	4)
  (UNSPECV_S32C1I	5)
  (UNSPECV_EH_RETURN	6)
  (UNSPECV_SET_TP	7)
  (UNSPECV_BLOCKAGE	8)
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

;; This code iterator is for *shlrd and its variants.
(define_code_iterator ior_op [ior plus])


;; Attributes.

(define_attr "type"
  "unknown,jump,call,load,store,move,arith,multi,nop,farith,fmadd,fconv,fload,fstore,mul16,mul32,div32,mac16,rsr,wsr,entry,trap"
  (const_string "unknown"))

(define_attr "mode"
  "unknown,none,QI,HI,SI,DI,SF,DF,BL"
  (const_string "unknown"))

(define_attr "length" "" (const_int 1))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type"	"multi")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])  ;; Should be the maximum possible length
				;; of a single machine instruction.


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
	(plus:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 3 "addsubx_operand" "i"))
		 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_ADDX"
{
  operands[3] = GEN_INT (1 << INTVAL (operands[3]));
  return "addx%3\t%0, %1, %2";
}
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
	(minus:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			     (match_operand:SI 3 "addsubx_operand" "i"))
		  (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_ADDX"
{
  operands[3] = GEN_INT (1 << INTVAL (operands[3]));
  return "subx%3\t%0, %1, %2";
}
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

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand"))))]
  "TARGET_MUL32_HIGH"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
  emit_insn (gen_mulsi3_highpart (gen_highpart (SImode, operands[0]),
				  operands[1], operands[2]));
  emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]), temp));
  DONE;
})

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand"))))]
  ""
{
  if (TARGET_MUL32_HIGH)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
      emit_insn (gen_umulsi3_highpart (gen_highpart (SImode, operands[0]),
				       operands[1], operands[2]));
      emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]), temp));
    }
  else
    emit_library_call_value (gen_rtx_SYMBOL_REF (Pmode, "__umulsidi3"),
			     operands[0], LCT_NORMAL, DImode,
			     operands[1], SImode,
			     operands[2], SImode);
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

(define_insn "<u>mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=C,A")
	(mult:SI (any_extend:SI
		  (match_operand:HI 1 "register_operand" "%r,r"))
		 (any_extend:SI
		  (match_operand:HI 2 "register_operand" "r,r"))))]
  "TARGET_MUL16 || TARGET_MAC16"
  "@
   mul16<su>\t%0, %1, %2
   <u>mul.aa.ll\t%1, %2"
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


;; Count redundant leading sign bits and leading/trailing zeros,
;; and find first bit.

(define_insn "clrsbsi2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(clrsb:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_NSA"
  "nsa\t%0, %1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

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


;; Byte swap.

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=a")
	(bswap:HI (match_operand:HI 1 "register_operand" "r")))
   (clobber (match_scratch:HI 2 "=&a"))]
  ""
  "extui\t%2, %1, 8, 8\;slli\t%0, %1, 8\;or\t%0, %0, %2"
   [(set_attr "type"	"arith")
    (set_attr "mode"	"HI")
    (set_attr "length"	"9")])

(define_expand "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "")
        (bswap:SI (match_operand:SI 1 "register_operand" "")))]
  "!optimize_debug && optimize > 1"
{
  /* GIMPLE manual byte-swapping recognition is now activated.
     For both built-in and manual bswaps, emit corresponding library call
     if optimizing for size, or a series of dedicated machine instructions
     if otherwise.  */
  if (optimize_size)
    emit_library_call_value (optab_libfunc (bswap_optab, SImode),
			     operands[0], LCT_NORMAL, SImode,
			     operands[1], SImode);
  else
    emit_insn (gen_bswapsi2_internal (operands[0], operands[1]));
  DONE;
})

(define_insn "bswapsi2_internal"
  [(set (match_operand:SI 0 "register_operand" "=a,&a")
	(bswap:SI (match_operand:SI 1 "register_operand" "0,r")))
   (clobber (match_scratch:SI 2 "=&a,X"))]
  "!optimize_debug && optimize > 1 && !optimize_size"
{
  rtx_insn *prev_insn = prev_nonnote_nondebug_insn (insn);
  const char *init = "ssai\t8\;";
  static char result[128];
  if (prev_insn && NONJUMP_INSN_P (prev_insn))
    {
      rtx x = PATTERN (prev_insn);
      if (GET_CODE (x) == PARALLEL && XVECLEN (x, 0) == 2
	  && GET_CODE (XVECEXP (x, 0, 0)) == SET
	  && GET_CODE (XVECEXP (x, 0, 1)) == CLOBBER)
	{
	  x = XEXP (XVECEXP (x, 0, 0), 1);
	  if (GET_CODE (x) == BSWAP && GET_MODE (x) == SImode)
	    init = "";
	}
    }
  sprintf (result,
	   (which_alternative == 0)
	   ? "%s" "srli\t%%2, %%1, 16\;src\t%%2, %%2, %%1\;src\t%%2, %%2, %%2\;src\t%%0, %%1, %%2"
	   : "%s" "srli\t%%0, %%1, 16\;src\t%%0, %%0, %%1\;src\t%%0, %%0, %%0\;src\t%%0, %%1, %%0",
	   init);
  return result;
}
   [(set_attr "type"	"arith,arith")
    (set_attr "mode"	"SI")
    (set_attr "length"	"15,15")])

(define_expand "bswapdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(bswap:DI (match_operand:DI 1 "register_operand" "")))]
  "!optimize_debug && optimize > 1 && optimize_size"
{
  /* Replace with a single DImode library call.
     Without this, two SImode library calls are emitted.  */
  emit_library_call_value (optab_libfunc (bswap_optab, DImode),
			   operands[0], LCT_NORMAL, DImode,
			   operands[1], DImode);
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

(define_insn_and_split "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 2)
	(const_int -1))
   (set (match_dup 0)
	(xor:SI (match_dup 1)
		(match_dup 2)))]
{
  operands[2] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY")
		      (const_int 5)
		      (const_int 6)))])

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

(define_insn_and_split "*andsi3_bitcmpl"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 3)
	(and:SI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(xor:SI (match_dup 3)
		(match_dup 2)))]
{
  operands[3] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_insn_and_split "*andsi3_const_pow2_minus_one"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(and:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "const_int_operand" "i")))]
  "IN_RANGE (exact_log2 (INTVAL (operands[2]) + 1), 17, 31)"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(ashift:SI (match_dup 1)
		   (match_dup 2)))
   (set (match_dup 0)
	(lshiftrt:SI (match_dup 0)
		     (match_dup 2)))]
{
  operands[2] = GEN_INT (32 - floor_log2 (INTVAL (operands[2]) + 1));
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY
				   && INTVAL (operands[2]) == 0x7FFFFFFF")
		      (const_int 5)
		      (const_int 6)))])

(define_insn_and_split "*andsi3_const_negative_pow2"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(and:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "const_int_operand" "i")))]
  "IN_RANGE (exact_log2 (-INTVAL (operands[2])), 12, 31)"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(lshiftrt:SI (match_dup 1)
		     (match_dup 2)))
   (set (match_dup 0)
	(ashift:SI (match_dup 0)
		   (match_dup 2)))]
{
  operands[2] = GEN_INT (floor_log2 (-INTVAL (operands[2])));
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_insn_and_split "*andsi3_const_shifted_mask"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(and:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "shifted_mask_operand" "i")))]
  "! xtensa_simm12b (INTVAL (operands[2]))"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(zero_extract:SI (match_dup 1)
			 (match_dup 3)
			 (match_dup 4)))
   (set (match_dup 0)
	(ashift:SI (match_dup 0)
		   (match_dup 2)))]
{
  HOST_WIDE_INT mask = INTVAL (operands[2]);
  int shift = ctz_hwi (mask);
  int mask_size = floor_log2 (((uint32_t)mask >> shift) + 1);
  int mask_pos = shift;
  if (BITS_BIG_ENDIAN)
    mask_pos = (32 - (mask_size + shift)) & 0x1f;
  operands[2] = GEN_INT (shift);
  operands[3] = GEN_INT (mask_size);
  operands[4] = GEN_INT (mask_pos);
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY
				   && ctz_hwi (INTVAL (operands[2])) == 1")
		      (const_int 5)
		      (const_int 6)))])

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
   %v1l16ui\t%0, %1"
  [(set_attr "type"	"arith,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3,3")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=a,a")
	(zero_extend:SI (match_operand:QI 1 "nonimmed_operand" "r,U")))]
  ""
  "@
   extui\t%0, %1, 0, 8
   %v1l8ui\t%0, %1"
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
   %v1l16si\t%0, %1"
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

(define_expand "extvsi"
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

  emit_insn (gen_extvsi_internal (operands[0], operands[1],
				  operands[2], operands[3]));
  DONE;
})

(define_insn "extvsi_internal"
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

(define_expand "extzvsi"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  ""
{
  if (!extui_fldsz_operand (operands[2], SImode))
    FAIL;
  emit_insn (gen_extzvsi_internal (operands[0], operands[1],
				   operands[2], operands[3]));
  DONE;
})

(define_insn "extzvsi_internal"
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
  if (CONSTANT_P (operands[1]))
    {
      /* Split in halves if 64-bit Const-to-Reg moves
	 because of offering further optimization opportunities.  */
      if (register_operand (operands[0], DImode))
	{
	  rtx ops[4] = { operands[0], operands[1] };
	  xtensa_split_DI_reg_imm (ops);
	  emit_move_insn (ops[0], ops[1]);
	  emit_move_insn (ops[2], ops[3]);
	  DONE;
	}

      if (!TARGET_CONST16)
	operands[1] = force_const_mem (DImode, operands[1]);
    }

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
  "&& reload_completed"
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

(define_split
  [(set (match_operand:DI 0 "register_operand")
	(match_operand:DI 1 "const_int_operand"))]
  "!TARGET_CONST16 && !TARGET_AUTO_LITPOOLS
   && ! xtensa_split1_finished_p ()"
  [(set (match_dup 0)
	(match_dup 1))
   (set (match_dup 2)
	(match_dup 3))]
{
  xtensa_split_DI_reg_imm (operands);
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
  [(set (match_operand:SI 0 "nonimmed_operand" "=D,D,D,D,R,R,a,q,a,a,W,a,a,U,*a,*A")
	(match_operand:SI 1 "move_operand" "M,D,d,R,D,d,r,r,I,Y,i,T,U,r,*A,*r"))]
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
   movi\t%0, %1
   const16\t%0, %t1\;const16\t%0, %b1
   %v1l32r\t%0, %1
   %v1l32i\t%0, %1
   %v0s32i\t%1, %0
   rsr\t%0, ACCLO
   wsr\t%1, ACCLO"
  [(set_attr "type" "move,move,move,load,store,store,move,move,move,move,move,load,load,store,rsr,wsr")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2,2,2,2,2,2,3,3,3,3,6,3,3,3,3,3")])

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(match_operand:SI 1 "const_int_operand"))]
  "!TARGET_CONST16 && !TARGET_AUTO_LITPOOLS
   && ! xtensa_split1_finished_p ()
   && ! xtensa_simm12b (INTVAL (operands[1]))"
  [(set (match_dup 0)
	(match_dup 1))]
{
  operands[1] = force_const_mem (SImode, operands[1]);
})

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(match_operand:SI 1 "constantpool_operand"))]
  "! optimize_debug && reload_completed"
  [(const_int 0)]
{
  rtx x = avoid_constant_pool_reference (operands[1]);
  if (! CONST_INT_P (x))
    FAIL;
  if (! xtensa_constantsynth (operands[0], INTVAL (x)))
    emit_move_insn (operands[0], x);
  DONE;
})

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
  [(set (match_operand:HI 0 "nonimmed_operand" "=D,D,a,a,a,a,U,*a,*A")
	(match_operand:HI 1 "move_operand" "M,d,r,I,Y,U,r,*A,*r"))]
  "xtensa_valid_move (HImode, operands)"
  "@
   movi.n\t%0, %x1
   mov.n\t%0, %1
   mov\t%0, %1
   movi\t%0, %x1
   movi\t%0, %1
   %v1l16ui\t%0, %1
   %v0s16i\t%1, %0
   rsr\t%0, ACCLO
   wsr\t%1, ACCLO"
  [(set_attr "type"	"move,move,move,move,move,load,store,rsr,wsr")
   (set_attr "mode"	"HI")
   (set_attr "length"	"2,2,3,3,3,3,3,3,3")])

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
  if (!TARGET_CONST16 && !TARGET_AUTO_LITPOOLS && CONSTANT_P (operands[1]))
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
  [(set (match_operand:SF 0 "nonimmed_operand" "=f,f,U,D,D,R,a,f,a,a,W,a,a,U")
	(match_operand:SF 1 "move_operand" "f,U,f,d,R,d,r,r,f,Y,iF,T,U,r"))]
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
   movi\t%0, %y1
   const16\t%0, %t1\;const16\t%0, %b1
   %v1l32r\t%0, %1
   %v1l32i\t%0, %1
   %v0s32i\t%1, %0"
  [(set_attr "type"	"farith,fload,fstore,move,load,store,move,farith,farith,move,move,load,load,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3,3,3,2,2,2,3,3,3,3,6,3,3,3")])

(define_insn "*lsiu"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mem:SF (plus:SI (match_operand:SI 1 "register_operand" "+a")
			 (match_operand:SI 2 "fpmem_offset_operand" "i"))))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_HARD_FLOAT && !TARGET_HARD_FLOAT_POSTINC"
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
  "TARGET_HARD_FLOAT && !TARGET_HARD_FLOAT_POSTINC"
{
  if (TARGET_SERIALIZE_VOLATILE && volatile_refs_p (PATTERN (insn)))
    output_asm_insn ("memw", operands);
  return "ssiu\t%2, %0, %1";
}
  [(set_attr "type"	"fstore")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "*lsip"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mem:SF (match_operand:SI 1 "register_operand" "+a")))
   (set (match_dup 1)
	(plus:SI (match_dup 1)
		 (match_operand:SI 2 "fpmem_offset_operand" "i")))]
  "TARGET_HARD_FLOAT && TARGET_HARD_FLOAT_POSTINC"
{
  if (TARGET_SERIALIZE_VOLATILE && volatile_refs_p (PATTERN (insn)))
    output_asm_insn ("memw", operands);
  return "lsip\t%0, %1, %2";
}
  [(set_attr "type"	"fload")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_insn "*ssip"
  [(set (mem:SF (match_operand:SI 0 "register_operand" "+a"))
	(match_operand:SF 1 "register_operand" "f"))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_operand:SI 2 "fpmem_offset_operand" "i")))]
  "TARGET_HARD_FLOAT && TARGET_HARD_FLOAT_POSTINC"
{
  if (TARGET_SERIALIZE_VOLATILE && volatile_refs_p (PATTERN (insn)))
    output_asm_insn ("memw", operands);
  return "ssip\t%1, %0, %2";
}
  [(set_attr "type"	"fstore")
   (set_attr "mode"	"SF")
   (set_attr "length"	"3")])

(define_split
  [(set (match_operand:SF 0 "register_operand")
	(match_operand:SF 1 "constantpool_operand"))]
  "! optimize_debug && reload_completed"
  [(const_int 0)]
{
  rtx x = avoid_constant_pool_reference (operands[1]);
  long l;
  HOST_WIDE_INT value;
  if (! CONST_DOUBLE_P (x) || GET_MODE (x) != SFmode)
    FAIL;
  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), l);
  x = gen_rtx_REG (SImode, REGNO (operands[0]));
  value = (int32_t)l;
  if (! xtensa_constantsynth (x, value))
    emit_move_insn (x, GEN_INT (value));
  DONE;
})

;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmed_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
{
  if (CONSTANT_P (operands[1]) && !TARGET_CONST16 && !TARGET_AUTO_LITPOOLS)
    operands[1] = force_const_mem (DFmode, operands[1]);

  if (!register_operand (operands[0], DFmode)
      && !register_operand (operands[1], DFmode))
    operands[1] = force_reg (DFmode, operands[1]);

  operands[1] = xtensa_copy_incoming_a7 (operands[1]);
})

(define_insn_and_split "movdf_internal"
  [(set (match_operand:DF 0 "nonimmed_operand" "=a,a,W,a,a,U")
	(match_operand:DF 1 "move_operand" "r,Y,iF,T,U,r"))]
  "register_operand (operands[0], DFmode)
   || register_operand (operands[1], DFmode)"
  "#"
  "&& reload_completed"
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

(define_expand "cpymemsi"
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

;; Block sets

(define_expand "setmemsi"
  [(match_operand:BLK 0 "memory_operand")
   (match_operand:SI 1 "")
   (match_operand:SI 2 "")
   (match_operand:SI 3 "const_int_operand")]
  "!optimize_debug && optimize"
{
  if (xtensa_expand_block_set_unrolled_loop (operands))
    DONE;
  if (xtensa_expand_block_set_small_loop (operands))
    DONE;
  FAIL;
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

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(ashift:SI (match_operand:SI 1 "register_operand")
		   (const_int 1)))]
  "TARGET_DENSITY"
  [(set (match_dup 0)
	(plus:SI (match_dup 1)
		 (match_dup 1)))])

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

(define_insn "*shift_per_byte"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(match_operator:SI 3 "xtensa_shift_per_byte_operator"
		[(match_operand:SI 1 "register_operand" "r")
		 (ashift:SI (match_operand:SI 2 "register_operand" "r")
			    (const_int 3))]))]
  "!optimize_debug && optimize"
{
  switch (GET_CODE (operands[3]))
    {
    case ASHIFT:	return "ssa8b\t%2\;sll\t%0, %1";
    case ASHIFTRT:	return "ssa8l\t%2\;sra\t%0, %1";
    case LSHIFTRT:	return "ssa8l\t%2\;srl\t%0, %1";
    default:		gcc_unreachable ();
    }
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_insn_and_split "*shift_per_byte_omit_AND_0"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(match_operator:SI 4 "xtensa_shift_per_byte_operator"
		[(match_operand:SI 1 "register_operand" "r")
		 (and:SI (ashift:SI (match_operand:SI 2 "register_operand" "r")
				    (const_int 3))
			 (match_operand:SI 3 "const_int_operand" "i"))]))]
  "!optimize_debug && optimize
   && (INTVAL (operands[3]) & 0x1f) == 3 << 3"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(match_op_dup 4
		[(match_dup 1)
		 (ashift:SI (match_dup 2)
			    (const_int 3))]))]
  ""
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_insn_and_split "*shift_per_byte_omit_AND_1"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(match_operator:SI 4 "xtensa_shift_per_byte_operator"
		[(match_operand:SI 1 "register_operand" "r")
		 (neg:SI (and:SI (ashift:SI (match_operand:SI 2 "register_operand" "r")
					    (const_int 3))
				 (match_operand:SI 3 "const_int_operand" "i")))]))]
  "!optimize_debug && optimize
   && (INTVAL (operands[3]) & 0x1f) == 3 << 3"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 5)
	(neg:SI (match_dup 2)))
   (set (match_dup 0)
	(match_op_dup 4
		[(match_dup 1)
		 (ashift:SI (match_dup 5)
			    (const_int 3))]))]
{
  operands[5] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"9")])

(define_insn "*shlrd_reg_<code>"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(ior_op:SI (match_operator:SI 4 "logical_shift_operator"
			[(match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "register_operand" "r")])
		   (match_operator:SI 5 "logical_shift_operator"
			[(match_operand:SI 3 "register_operand" "r")
			 (neg:SI (match_dup 2))])))]
  "!optimize_debug && optimize
   && xtensa_shlrd_which_direction (operands[4], operands[5]) != UNKNOWN"
{
  switch (xtensa_shlrd_which_direction (operands[4], operands[5]))
    {
    case ASHIFT:	return "ssl\t%2\;src\t%0, %1, %3";
    case LSHIFTRT:	return "ssr\t%2\;src\t%0, %3, %1";
    default:		gcc_unreachable ();
    }
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_insn "*shlrd_const_<code>"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(ior_op:SI (match_operator:SI 5 "logical_shift_operator"
			[(match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 3 "const_int_operand" "i")])
		   (match_operator:SI 6 "logical_shift_operator"
			[(match_operand:SI 2 "register_operand" "r")
			 (match_operand:SI 4 "const_int_operand" "i")])))]
  "!optimize_debug && optimize
   && xtensa_shlrd_which_direction (operands[5], operands[6]) != UNKNOWN
   && IN_RANGE (INTVAL (operands[3]), 1, 31)
   && IN_RANGE (INTVAL (operands[4]), 1, 31)
   && INTVAL (operands[3]) + INTVAL (operands[4]) == 32"
{
  switch (xtensa_shlrd_which_direction (operands[5], operands[6]))
    {
    case ASHIFT:	return "ssai\t%L3\;src\t%0, %1, %2";
    case LSHIFTRT:	return "ssai\t%R3\;src\t%0, %2, %1";
    default:		gcc_unreachable ();
    }
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_insn "*shlrd_per_byte_<code>"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(ior_op:SI (match_operator:SI 4 "logical_shift_operator"
			[(match_operand:SI 1 "register_operand" "r")
			 (ashift:SI (match_operand:SI 2 "register_operand" "r")
				    (const_int 3))])
		   (match_operator:SI 5 "logical_shift_operator"
			[(match_operand:SI 3 "register_operand" "r")
			 (neg:SI (ashift:SI (match_dup 2)
					    (const_int 3)))])))]
  "!optimize_debug && optimize
   && xtensa_shlrd_which_direction (operands[4], operands[5]) != UNKNOWN"
{
  switch (xtensa_shlrd_which_direction (operands[4], operands[5]))
    {
    case ASHIFT:	return "ssa8b\t%2\;src\t%0, %1, %3";
    case LSHIFTRT:	return "ssa8l\t%2\;src\t%0, %3, %1";
    default:		gcc_unreachable ();
    }
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

(define_insn_and_split "*shlrd_per_byte_<code>_omit_AND"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(ior_op:SI (match_operator:SI 5 "logical_shift_operator"
			[(match_operand:SI 1 "register_operand" "r")
			 (and:SI (ashift:SI (match_operand:SI 2 "register_operand" "r")
					    (const_int 3))
				 (match_operand:SI 4 "const_int_operand" "i"))])
		   (match_operator:SI 6 "logical_shift_operator"
			[(match_operand:SI 3 "register_operand" "r")
			 (neg:SI (and:SI (ashift:SI (match_dup 2)
						    (const_int 3))
					 (match_dup 4)))])))]
  "!optimize_debug && optimize
   && xtensa_shlrd_which_direction (operands[5], operands[6]) != UNKNOWN
   && (INTVAL (operands[4]) & 0x1f) == 3 << 3"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(ior_op:SI (match_op_dup 5
			[(match_dup 1)
			 (ashift:SI (match_dup 2)
				    (const_int 3))])
		   (match_op_dup 6
			[(match_dup 3)
			 (neg:SI (ashift:SI (match_dup 2)
					    (const_int 3)))])))]
  ""
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])

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
  return xtensa_emit_branch (which_alternative == 0, operands);
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
  return xtensa_emit_branch (which_alternative == 0, operands);
}
  [(set_attr "type"	"jump,jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3,3")])

;; Branch patterns for bit testing

(define_insn "*bittrue"
  [(set (pc)
	(if_then_else (match_operator 3 "boolean_operator"
			[(zero_extract:SI (match_operand:SI 0 "register_operand" "r,r")
					  (const_int 1)
					  (match_operand:SI 1 "arith_operand" "J,r"))
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  static char result[64];
  char op;
  switch (GET_CODE (operands[3]))
    {
    case EQ:	op = 'c'; break;
    case NE:	op = 's'; break;
    default:	gcc_unreachable ();
    }
  if (which_alternative == 0)
    {
      operands[1] = GEN_INT (INTVAL (operands[1]) & 0x1f);
      sprintf (result, "bb%ci\t%%0, %%d1, %%2", op);
    }
  else
    sprintf (result, "bb%c\t%%0, %%1, %%2", op);
  return result;
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
    case EQ:	return "bnone\t%0, %1, %2";
    case NE:	return "bany\t%0, %1, %2";
    default:	gcc_unreachable ();
    }
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "*masktrue_bitcmpl"
  [(set (pc)
	(if_then_else (match_operator 3 "boolean_operator"
			[(and:SI (not:SI (match_operand:SI 0 "register_operand" "r"))
				 (match_operand:SI 1 "register_operand" "r"))
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  switch (GET_CODE (operands[3]))
    {
    case EQ:	return "ball\t%0, %1, %2";
    case NE:	return "bnall\t%0, %1, %2";
    default:	gcc_unreachable ();
    }
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn_and_split "*masktrue_const_bitcmpl"
  [(set (pc)
	(if_then_else (match_operator 3 "boolean_operator"
			[(and:SI (not:SI (match_operand:SI 0 "register_operand" "r"))
				 (match_operand:SI 1 "const_int_operand" "i"))
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "exact_log2 (INTVAL (operands[1])) < 0"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 4)
	(match_dup 1))
   (set (pc)
	(if_then_else (match_op_dup 3
			[(and:SI (not:SI (match_dup 0))
				 (match_dup 4))
			 (const_int 0)])
		      (label_ref (match_dup 2))
		      (pc)))]
{
  operands[4] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY
				   && IN_RANGE (INTVAL (operands[1]), -32, 95)")
		      (const_int 5)
		      (if_then_else (match_test "xtensa_simm12b (INTVAL (operands[1]))")
				    (const_int 6)
				    (const_int 10))))])

(define_split
  [(set (pc)
	(if_then_else (match_operator 2 "boolean_operator"
			[(subreg:HQI (not:SI (match_operand:SI 0 "register_operand")) 0)
			 (const_int 0)])
		      (label_ref (match_operand 1 ""))
		      (pc)))]
  "!BYTES_BIG_ENDIAN"
  [(set (pc)
	(if_then_else (match_op_dup 2
			[(and:SI (not:SI (match_dup 0))
				 (match_dup 3))
			 (const_int 0)])
		      (label_ref (match_dup 1))
		      (pc)))]
{
  operands[3] = GEN_INT ((1 << GET_MODE_BITSIZE (<MODE>mode)) - 1);
})

(define_split
  [(set (pc)
	(if_then_else (match_operator 2 "boolean_operator"
			[(subreg:HI (not:SI (match_operand:SI 0 "register_operand")) 2)
			 (const_int 0)])
		      (label_ref (match_operand 1 ""))
		      (pc)))]
  "BYTES_BIG_ENDIAN"
  [(set (pc)
	(if_then_else (match_op_dup 2
			[(and:SI (not:SI (match_dup 0))
				 (const_int 65535))
			 (const_int 0)])
		      (label_ref (match_dup 1))
		      (pc)))])

(define_split
  [(set (pc)
	(if_then_else (match_operator 2 "boolean_operator"
			[(subreg:QI (not:SI (match_operand:SI 0 "register_operand")) 3)
			 (const_int 0)])
		      (label_ref (match_operand 1 ""))
		      (pc)))]
  "BYTES_BIG_ENDIAN"
  [(set (pc)
	(if_then_else (match_op_dup 2
			[(and:SI (not:SI (match_dup 0))
				 (const_int 255))
			 (const_int 0)])
		      (label_ref (match_dup 1))
		      (pc)))])

(define_insn_and_split "*masktrue_const_pow2_minus_one"
  [(set (pc)
	(if_then_else (match_operator 4 "boolean_operator"
			[(and:SI (match_operand:SI 0 "register_operand" "r")
				 (match_operand:SI 1 "const_int_operand" "i"))
			 (match_operand:SI 2 "const_int_operand" "i")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "IN_RANGE (exact_log2 (INTVAL (operands[1]) + 1), 17, 31)
   /* && (~INTVAL (operands[1]) & INTVAL (operands[2])) == 0  // can be omitted */
   && xtensa_b4const_or_zero (INTVAL (operands[2]) << (32 - floor_log2 (INTVAL (operands[1]) + 1)))"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 5)
	(ashift:SI (match_dup 0)
		   (match_dup 1)))
   (set (pc)
	(if_then_else (match_op_dup 4
			[(match_dup 5)
			 (match_dup 2)])
		      (label_ref (match_dup 3))
		      (pc)))]
{
  int shift = 32 - floor_log2 (INTVAL (operands[1]) + 1);
  operands[1] = GEN_INT (shift);
  operands[2] = GEN_INT (INTVAL (operands[2]) << shift);
  operands[5] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set (attr "length")
	(if_then_else (match_test "(TARGET_DENSITY && INTVAL (operands[1]) == 0x7FFFFFFF)
				   && INTVAL (operands[2]) == 0")
		      (const_int 4)
		      (if_then_else (match_test "TARGET_DENSITY
						 && (INTVAL (operands[1]) == 0x7FFFFFFF
						     || INTVAL (operands[2]) == 0)")
				    (const_int 5)
				    (const_int 6))))])

(define_insn_and_split "*masktrue_const_negative_pow2"
  [(set (pc)
	(if_then_else (match_operator 4 "boolean_operator"
			[(and:SI (match_operand:SI 0 "register_operand" "r")
				 (match_operand:SI 1 "const_int_operand" "i"))
			 (match_operand:SI 2 "const_int_operand" "i")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "IN_RANGE (exact_log2 (-INTVAL (operands[1])), 1, 30)
   /* && (~INTVAL (operands[1]) & INTVAL (operands[2])) == 0  // can be omitted */
   && xtensa_b4const_or_zero (INTVAL (operands[2]) >> floor_log2 (-INTVAL (operands[1])))"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 5)
	(lshiftrt:SI (match_dup 0)
		     (match_dup 1)))
   (set (pc)
	(if_then_else (match_op_dup 4
			[(match_dup 5)
			 (match_dup 2)])
		      (label_ref (match_dup 3))
		      (pc)))]
{
  int shift = floor_log2 (-INTVAL (operands[1]));
  operands[1] = GEN_INT (shift);
  operands[2] = GEN_INT (INTVAL (operands[2]) >> shift);
  operands[5] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY && INTVAL (operands[2]) == 0")
		      (const_int 5)
		      (const_int 6)))])

(define_insn_and_split "*masktrue_const_shifted_mask"
  [(set (pc)
	(if_then_else (match_operator 4 "boolean_operator"
			[(and:SI (match_operand:SI 0 "register_operand" "r")
				 (match_operand:SI 1 "shifted_mask_operand" "i"))
			 (match_operand:SI 2 "const_int_operand" "i")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "/* (INTVAL (operands[2]) & ((1 << ctz_hwi (INTVAL (operands[1]))) - 1)) == 0  // can be omitted
   && */ xtensa_b4const_or_zero ((uint32_t)INTVAL (operands[2]) >> ctz_hwi (INTVAL (operands[1])))"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 6)
	(zero_extract:SI (match_dup 0)
			 (match_dup 5)
			 (match_dup 1)))
   (set (pc)
	(if_then_else (match_op_dup 4
			[(match_dup 6)
			 (match_dup 2)])
		      (label_ref (match_dup 3))
		      (pc)))]
{
  HOST_WIDE_INT mask = INTVAL (operands[1]);
  int shift = ctz_hwi (mask);
  int mask_size = floor_log2 (((uint32_t)mask >> shift) + 1);
  int mask_pos = shift;
  if (BITS_BIG_ENDIAN)
    mask_pos = (32 - (mask_size + shift)) & 0x1f;
  operands[1] = GEN_INT (mask_pos);
  operands[2] = GEN_INT ((uint32_t)INTVAL (operands[2]) >> shift);
  operands[5] = GEN_INT (mask_size);
  operands[6] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY
				   && (uint32_t)INTVAL (operands[2]) >> ctz_hwi (INTVAL (operands[1])) == 0")
		      (const_int 5)
		      (const_int 6)))])


;; Zero-overhead looping support.

;; Define the loop insns used by bct optimization to represent the
;; start and end of a zero-overhead loop.  This start template generates
;; the loop insn; the end template doesn't generate any instructions since
;; loop end is handled in hardware.

(define_insn "zero_cost_loop_start"
  [(set (pc)
        (if_then_else (ne (match_operand:SI 2 "register_operand" "0")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
   (set (match_operand:SI 0 "register_operand" "=a")
        (plus (match_dup 0)
              (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_START)]
  "TARGET_LOOPS && optimize"
  "loop\t%0, %l1_LEND"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_insn "zero_cost_loop_end"
  [(set (pc)
        (if_then_else (ne (match_operand:SI 2 "nonimmediate_operand" "0,0")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
   (set (match_operand:SI 0 "nonimmediate_operand" "=a,m")
        (plus (match_dup 0)
              (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)
   (clobber (match_scratch:SI 3 "=X,&r"))]
  "TARGET_LOOPS && optimize"
  "#"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

(define_insn "loop_end"
  [(set (pc)
        (if_then_else (ne (match_operand:SI 2 "register_operand" "0")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
   (set (match_operand:SI 0 "register_operand" "=a")
        (plus (match_dup 0)
              (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)]
  "TARGET_LOOPS && optimize"
{
  xtensa_emit_loop_end (insn, operands);
  return "";
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

(define_split
  [(set (pc)
        (if_then_else (ne (match_operand:SI 0 "nonimmediate_operand" "")
                          (const_int 1))
                      (label_ref (match_operand 1 "" ""))
                      (pc)))
   (set (match_operand:SI 2 "nonimmediate_operand" "")
        (plus:SI (match_dup 0)
                 (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)
   (clobber (match_scratch 3))]
  "TARGET_LOOPS && optimize && reload_completed"
  [(const_int 0)]
{
  if (!REG_P (operands[0]))
    {
      rtx test;

      /* Fallback into a normal conditional branch insn.  */
      emit_move_insn (operands[3], operands[0]);
      emit_insn (gen_addsi3 (operands[3], operands[3], constm1_rtx));
      emit_move_insn (operands[0], operands[3]);
      test = gen_rtx_NE (VOIDmode, operands[3], const0_rtx);
      emit_jump_insn (gen_cbranchsi4 (test, operands[3],
                                      const0_rtx, operands[1]));
    }
  else
    {
      emit_jump_insn (gen_loop_end (operands[0], operands[1], operands[2]));
    }

  DONE;
})

; operand 0 is the loop count pseudo register
; operand 1 is the label to jump to at the top of the loop
(define_expand "doloop_end"
  [(parallel [(set (pc) (if_then_else
                          (ne (match_operand:SI 0 "" "")
                              (const_int 1))
                          (label_ref (match_operand 1 "" ""))
                          (pc)))
              (set (match_dup 0)
                   (plus:SI (match_dup 0)
                            (const_int -1)))
              (unspec [(const_int 0)] UNSPEC_LSETUP_END)
              (clobber (match_dup 2))])] ; match_scratch
  "TARGET_LOOPS && optimize"
{
  /* The loop optimizer doesn't check the predicates... */
  if (GET_MODE (operands[0]) != SImode)
    FAIL;
  operands[2] = gen_rtx_SCRATCH (SImode);
})


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
  xtensa_prepare_expand_call (0, operands);
})

(define_insn "call_internal"
  [(call (mem (match_operand:SI 0 "call_insn_operand" "nir"))
	 (match_operand 1 "" "i"))]
  "!SIBLING_CALL_P (insn)"
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
  xtensa_prepare_expand_call (1, operands);
})

(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "=a")
        (call (mem (match_operand:SI 1 "call_insn_operand" "nir"))
              (match_operand 2 "" "i")))]
  "!SIBLING_CALL_P (insn)"
{
  return xtensa_emit_call (1, operands);
}
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_expand "sibcall"
  [(call (match_operand 0 "memory_operand" "")
	 (match_operand 1 "" ""))]
  "!TARGET_WINDOWED_ABI"
{
  xtensa_prepare_expand_call (0, operands);
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "nic"))
	 (match_operand 1 "" "i"))]
  "!TARGET_WINDOWED_ABI && SIBLING_CALL_P (insn)"
{
  return xtensa_emit_sibcall (0, operands);
}
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_expand "sibcall_value"
  [(set (match_operand 0 "register_operand" "")
	(call (match_operand 1 "memory_operand" "")
	      (match_operand 2 "" "")))]
  "!TARGET_WINDOWED_ABI"
{
  xtensa_prepare_expand_call (1, operands);
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "=a")
	(call (mem:SI (match_operand:SI 1 "call_insn_operand" "nic"))
	      (match_operand 2 "" "i")))]
  "!TARGET_WINDOWED_ABI && SIBLING_CALL_P (insn)"
{
  return xtensa_emit_sibcall (1, operands);
}
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"3")])

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "")
		    (const_int 0))
	      (match_operand 1 "")
	      (match_operand 2 "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  emit_insn (gen_blockage ());
  DONE;
})

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
  "xtensa_use_return_instruction_p ()"
{
  return TARGET_WINDOWED_ABI ?
      (TARGET_DENSITY ? "retw.n" : "retw") :
      (TARGET_DENSITY ? "ret.n" : "ret");
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY")
		      (const_int 2)
		      (const_int 3)))])


;; Miscellaneous instructions.

;; In windowed ABI stack pointer adjustment must happen before any access
;; to the space allocated on stack is allowed, otherwise register spill
;; area may be clobbered.  That's what frame blockage is supposed to enforce.

(define_expand "allocate_stack"
  [(set (match_operand 0 "nonimmed_operand")
        (minus (reg A1_REG) (match_operand 1 "add_operand")))
   (set (reg A1_REG)
        (minus (reg A1_REG) (match_dup 1)))]
  "TARGET_WINDOWED_ABI"
{
  if (CONST_INT_P (operands[1]))
    {
      rtx neg_op0 = GEN_INT (-INTVAL (operands[1]));
      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, neg_op0));
    }
  else
    {
      emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx,
			     operands[1]));
    }
  emit_move_insn (operands[0], virtual_stack_dynamic_rtx);
  emit_insn (gen_frame_blockage ());
  DONE;
})

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
  xtensa_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(return)]
  "!TARGET_WINDOWED_ABI"
{
  xtensa_expand_epilogue (true);
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
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY")
		      (const_int 2)
		      (const_int 3)))])

(define_expand "nonlocal_goto"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "general_operand" "")
   (match_operand:SI 2 "general_operand" "")
   (match_operand:SI 3 "" "")]
  "TARGET_WINDOWED_ABI"
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

(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  if (TARGET_WINDOWED_ABI)
    emit_insn (gen_eh_set_a0_windowed (operands[0]));
  else
    emit_insn (gen_eh_set_a0_call0 (operands[0]));
  DONE;
})

(define_insn_and_split "eh_set_a0_windowed"
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

(define_insn_and_split "eh_set_a0_call0"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")]
		    UNSPECV_EH_RETURN)
   (clobber (match_scratch:SI 1 "=r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  xtensa_set_return_address (operands[0], operands[1]);
  DONE;
})

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

;; Do not schedule instructions accessing memory before this point.

(define_expand "frame_blockage"
  [(set (match_dup 0)
        (unspec:BLK [(match_dup 1)] UNSPEC_FRAME_BLOCKAGE))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
  operands[1] = stack_pointer_rtx;
})

(define_insn "*frame_blockage"
  [(set (match_operand:BLK 0 "" "")
        (unspec:BLK [(match_operand:SI 1 "" "")] UNSPEC_FRAME_BLOCKAGE))]
  ""
  ""
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
{
  if (TARGET_DEBUG)
    return "break\t1, 15";
  else
    return (TARGET_DENSITY ? "ill.n" : "ill");
}
  [(set_attr "type"	"trap")
   (set_attr "mode"	"none")
   (set (attr "length")
	(if_then_else (match_test "!TARGET_DEBUG && TARGET_DENSITY")
		      (const_int 2)
		      (const_int 3)))])

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
{
  if (TARGET_WINDOWED_ABI)
    return "callx8.tls %1, %2@TLSCALL";
  else
    return "callx0.tls %1, %2@TLSCALL";
}
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

(define_insn_and_split "*round_up_to_even"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(and:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 1))
		(const_int -2)))]
  ""
  "#"
  "can_create_pseudo_p ()"
  [(set (match_dup 2)
	(and:SI (match_dup 1)
		(const_int 1)))
   (set (match_dup 0)
	(plus:SI (match_dup 2)
		 (match_dup 1)))]
{
  operands[2] = gen_reg_rtx (SImode);
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY")
		      (const_int 5)
		      (const_int 6)))])

(define_insn_and_split "*signed_ge_zero"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(ge:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))]
  ""
  "#"
  ""
  [(set (match_dup 0)
	(ashiftrt:SI (match_dup 1)
		     (const_int 31)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  ""
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")
	(if_then_else (match_test "TARGET_DENSITY")
		      (const_int 5)
		      (const_int 6)))])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
	(match_operand:SI 6 "reload_operand"))
   (set (match_operand:SI 1 "register_operand")
	(match_operand:SI 7 "reload_operand"))
   (set (match_operand:SF 2 "register_operand")
	(match_operand:SF 4 "register_operand"))
   (set (match_operand:SF 3 "register_operand")
	(match_operand:SF 5 "register_operand"))]
  "REGNO (operands[0]) == REGNO (operands[4])
   && REGNO (operands[1]) == REGNO (operands[5])
   && peep2_reg_dead_p (4, operands[0])
   && peep2_reg_dead_p (4, operands[1])"
  [(set (match_dup 2)
	(match_dup 6))
   (set (match_dup 3)
	(match_dup 7))]
{
  uint32_t check = 0;
  int i;
  for (i = 0; i <= 3; ++i)
    {
      uint32_t mask = (uint32_t)1 << REGNO (operands[i]);
      if (check & mask)
	FAIL;
      check |= mask;
    }
  operands[6] = gen_rtx_MEM (SFmode, XEXP (operands[6], 0));
  operands[7] = gen_rtx_MEM (SFmode, XEXP (operands[7], 0));
})

(define_split
  [(clobber (match_operand 0 "register_operand"))]
  "HARD_REGISTER_P (operands[0])
   && COMPLEX_MODE_P (GET_MODE (operands[0]))"
  [(const_int 0)]
{
  auto_sbitmap bmp (FIRST_PSEUDO_REGISTER);
  rtx_insn *insn;
  rtx reg = gen_rtx_REG (SImode, 0);
  bitmap_set_range (bmp, REGNO (operands[0]), REG_NREGS (operands[0]));
  for (insn = next_nonnote_nondebug_insn_bb (curr_insn);
       insn; insn = next_nonnote_nondebug_insn_bb (insn))
    {
      sbitmap_iterator iter;
      unsigned int regno;
      if (NONJUMP_INSN_P (insn))
	{
	  EXECUTE_IF_SET_IN_BITMAP (bmp, 2, regno, iter)
	    {
	      set_regno_raw (reg, regno, REG_NREGS (reg));
	      if (reg_overlap_mentioned_p (reg, PATTERN (insn)))
		break;
	    }
	  if (GET_CODE (PATTERN (insn)) == SET)
	    {
	      rtx x = SET_DEST (PATTERN (insn));
	      if (REG_P (x) && HARD_REGISTER_P (x))
		bitmap_clear_range (bmp, REGNO (x), REG_NREGS (x));
	      else if (SUBREG_P (x) && HARD_REGISTER_P (SUBREG_REG (x)))
		{
		  struct subreg_info info;
		  subreg_get_info (regno = REGNO (SUBREG_REG (x)),
				   GET_MODE (SUBREG_REG (x)),
				   SUBREG_BYTE (x), GET_MODE (x), &info);
		  if (!info.representable_p)
		    break;
		  bitmap_clear_range (bmp, regno + info.offset, info.nregs);
		}
	    }
	  if (bitmap_empty_p (bmp))
	    goto FALLTHRU;
	}
      else if (CALL_P (insn))
	EXECUTE_IF_SET_IN_BITMAP (bmp, 2, regno, iter)
	 if (call_used_or_fixed_reg_p (regno))
	   break;
    }
  FAIL;
FALLTHRU:;
})

(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
	(match_operand:SI 1 "const_int_operand"))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_operand:SI 2 "const_int_operand")))
   (set (match_operand:SI 3 "register_operand")
	(plus:SI (match_operand:SI 4 "register_operand")
		 (match_dup 0)))]
  "IN_RANGE (INTVAL (operands[1]) + INTVAL (operands[2]),
	     (-128 - 32768), (127 + 32512))
   && REGNO (operands[0]) != REGNO (operands[3])
   && REGNO (operands[0]) != REGNO (operands[4])
   && peep2_reg_dead_p (3, operands[0])"
  [(set (match_dup 3)
	(plus:SI (match_dup 4)
		 (match_dup 1)))
   (set (match_dup 3)
	(plus:SI (match_dup 3)
		 (match_dup 2)))]
{
  HOST_WIDE_INT value = INTVAL (operands[1]) + INTVAL (operands[2]);
  int imm0, imm1;
  value += 128;
  if (value > 32512)
    imm1 = 32512;
  else
    imm1 = value & ~255;
  imm0 = value - imm1 - 128;
  operands[1] = GEN_INT (imm0);
  operands[2] = GEN_INT (imm1);
})
