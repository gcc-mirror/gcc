;; Machine description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

;;SFmode moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (SFmode, operands[1]);
  if (CONST_DOUBLE_P (operands[1])
      && !satisfies_constraint_Cs20 (operands[1]))
    {
      const REAL_VALUE_TYPE *r;
      unsigned long l;

      r = CONST_DOUBLE_REAL_VALUE (operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (*r, l);

      emit_move_insn (operands[0], gen_rtx_HIGH (SFmode, operands[1]));

      if ((l & 0xFFF) != 0)
	emit_insn (gen_movsf_lo (operands[0], operands[0], operands[1]));
      DONE;
    }
})

(define_insn "movsf_lo"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(lo_sum:SF (match_operand:SF 1 "register_operand" "r")
		   (match_operand:SF 2 "immediate_operand" "i")))]
  ""
  "ori\t%0, %1, lo12(%2)"
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")]
)

(define_insn "*movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r, r, U45, U33, U37, U45, m,   l,   l,   l,   d, r, f, *f, *r, f, Q,   r,   r,    r")
	(match_operand:SF 1 "general_operand"      " r, r,   l,   l,   l,   d, r, U45, U33, U37, U45, m, f, *r, *f, Q, f,Cs05,Cs20, Chig"))]
  "(register_operand(operands[0], SFmode)
    || register_operand(operands[1], SFmode))"
{
  switch (which_alternative)
    {
    case 0:
      return "mov55\t%0, %1";
    case 1:
      return "ori\t%0, %1, 0";
    case 2:
    case 3:
    case 4:
    case 5:
      return nds32_output_16bit_store (operands, 4);
    case 6:
      return nds32_output_32bit_store (operands, 4);
    case 7:
    case 8:
    case 9:
    case 10:
      return nds32_output_16bit_load (operands, 4);
    case 11:
      return nds32_output_32bit_load (operands, 4);
    case 12:
      if (TARGET_FPU_SINGLE)
	return "fcpyss\t%0, %1, %1";
      else
	return "#";
    case 13:
      return "fmtsr\t%1, %0";
    case 14:
      return "fmfsr\t%0, %1";
    case 15:
      return nds32_output_float_load (operands);
    case 16:
      return nds32_output_float_store (operands);
    case 17:
      return "movi55\t%0, %1";
    case 18:
      return "movi\t%0, %1";
    case 19:
      return "sethi\t%0, %1";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,store,store,store,store,store,load,load,load,load,load,fcpy,fmtsr,fmfsr,fload,fstore,alu,alu,alu")
   (set_attr "length"  "  2,  4,    2,    2,    2,    2,    4,   2,   2,   2,   2,   4,   4,    4,    4,    4,     4,  2,  4,  4")
   (set_attr "feature" " v1, v1,   v1,   v1,   v1,   v1,   v1,  v1,  v1,  v1,  v1,  v1, fpu,  fpu,  fpu,  fpu,   fpu, v1, v1, v1")])

;; Conditional Move Instructions

(define_expand "mov<mode>cc"
  [(set (match_operand:ANYF 0 "register_operand" "")
	(if_then_else:ANYF (match_operand 1 "nds32_float_comparison_operator" "")
			   (match_operand:ANYF 2 "register_operand" "")
			   (match_operand:ANYF 3 "register_operand" "")))]
  ""
{
  if (nds32_cond_move_p (operands[1]))
    {
      /* Operands[1] condition code is UNORDERED or ORDERED, and
	 sub-operands[1] MODE isn't SFmode or SFmode, return FAIL
	 for gcc, because we don't using slt compare instruction
	 to generate UNORDERED and ORDERED condition.  */
      FAIL;
    }
  else
    nds32_expand_float_movcc (operands);
})

(define_insn "fcmov<mode>_eq"
  [(set (match_operand:ANYF 0 "register_operand" "=f, f")
	(if_then_else:ANYF (eq (match_operand:SI 1 "register_operand" "f, f")
			       (const_int 0))
			   (match_operand:ANYF 2 "register_operand" "f, 0")
			   (match_operand:ANYF 3 "register_operand" "0, f")))]
  ""
  "@
   fcmovz<size>\t%0,%2,%1
   fcmovn<size>\t%0,%3,%1"
  [(set_attr "type"  "fcmov")
   (set_attr "length" "4")]
)

(define_insn "fcmov<mode>_ne"
  [(set (match_operand:ANYF 0 "register_operand" "=f, f")
	(if_then_else:ANYF (ne (match_operand:SI 1 "register_operand" "f, f")
			       (const_int 0))
			   (match_operand:ANYF 2 "register_operand" "f, 0")
			   (match_operand:ANYF 3 "register_operand" "0, f")))]
  ""
  "@
   fcmovn<size>\t%0,%2,%1
   fcmovz<size>\t%0,%3,%1"
  [(set_attr "type"  "fcmov")
   (set_attr "length" "4")]
)

;; Arithmetic instructions.

(define_insn "add<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(plus:ANYF (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fadd<size>\t %0, %1, %2"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

(define_insn "sub<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF (match_operand:ANYF 1 "register_operand" "f")
		    (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fsub<size>\t %0, %1, %2"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

;; Multiplication insns.

(define_insn "mul<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fmul<size>\t %0, %1, %2"
  [(set_attr "type"   "fmul<size>")
   (set_attr "length" "4")]
)

(define_insn "fma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "0")))]
  "TARGET_EXT_FPU_FMA"
  "fmadd<size>\t%0, %1, %2"
  [(set_attr "type"   "fmac<size>")
   (set_attr "length" "4")]
)

(define_insn "fnma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "0")))]
  "TARGET_EXT_FPU_FMA"
  "fmsub<size>\t%0, %1, %2"
  [(set_attr "type"   "fmac<size>")
   (set_attr "length" "4")]
)

(define_insn "fms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" "0"))))]
  "TARGET_EXT_FPU_FMA"
  "fnmsub<size>\t%0, %1, %2"
  [(set_attr "type"   "fmac<size>")
   (set_attr "length" "4")]
)

(define_insn "fnms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		  (match_operand:ANYF 2 "register_operand" "f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" "0"))))]
  "TARGET_EXT_FPU_FMA"
  "fnmadd<size>\t%0, %1, %2"
  [(set_attr "type"   "fmac<size>")
   (set_attr "length" "4")]
)

;; Div Instructions.

(define_insn "div<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fdiv<size>\t %0, %1, %2"
  [(set_attr "type"   "fdiv<size>")
   (set_attr "length" "4")]
)

(define_insn "sqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  ""
  "fsqrt<size>\t %0, %1"
  [(set_attr "type"   "fsqrt<size>")
   (set_attr "length" "4")]
)

;; Conditional Branch patterns

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "nds32_float_comparison_operator"
	 [(match_operand:ANYF 2 "register_operand" "")
	  (match_operand:ANYF 3 "register_operand" "")]))]
  ""
{
  nds32_expand_float_cstore (operands);
  DONE;
})

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_float_comparison_operator"
		       [(match_operand:ANYF 1 "register_operand" "")
			(match_operand:ANYF 2 "register_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
{
  nds32_expand_float_cbranch (operands);
  DONE;
})

;; Copysign Instructions.

(define_insn "copysignsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		     UNSPEC_COPYSIGN))]
  "TARGET_FPU_SINGLE"
  "fcpyss\t%0,%1,%2"
  [(set_attr "type"   "fcpy")
   (set_attr "length" "4")]
)

(define_insn "copysigndf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		     UNSPEC_COPYSIGN))]
  "TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE"
  "fcpysd\t%0,%1,%2"
  [(set_attr "type"   "fcpy")
   (set_attr "length" "4")]
)

(define_insn "*ncopysign<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")
				(match_operand:ANYF 2 "register_operand" "f")]
				UNSPEC_COPYSIGN)))]
  ""
  "fcpyns<size>\t%0,%1,%2"
  [(set_attr "type"   "fcpy")
   (set_attr "length" "4")]
)

;; Absolute Instructions

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f, r")
	(abs:SF (match_operand:SF 1 "register_operand" "f, r")))]
  "TARGET_FPU_SINGLE || TARGET_EXT_PERF"
  "@
   fabss\t%0, %1
   bclr\t%0, %1, 31"
  [(set_attr "type"    "fabs,alu")
   (set_attr "length"  "4")
   (set_attr "feature" "fpu,pe1")]
)

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_FPU_DOUBLE"
  "fabsd\t%0, %1"
  [(set_attr "type"   "fabs")
   (set_attr "length" "4")]
)

;; Negation Instructions

(define_insn "*negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f, r")
	(neg:SF (match_operand:SF 1 "register_operand" "f, r")))]
  "TARGET_FPU_SINGLE || TARGET_EXT_PERF"
  "@
   fcpynss\t%0, %1, %1
   btgl\t%0, %1, 31"
  [(set_attr "type"    "fcpy,alu")
   (set_attr "length"  "4")
   (set_attr "feature" "fpu,pe1")]
)

(define_insn "*negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_FPU_DOUBLE"
  "fcpynsd\t%0, %1, %1"
  [(set_attr "type"   "fcpy")
   (set_attr "length" "4")]
)

;; Data Format Conversion Instructions

(define_insn "floatunssi<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(unsigned_float:ANYF (match_operand:SI 1 "register_operand" "f")))]
  ""
  "fui2<size>\t %0, %1"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

(define_insn "floatsi<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(float:ANYF (match_operand:SI 1 "register_operand" "f")))]
  ""
  "fsi2<size>\t %0, %1"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

(define_insn "fixuns_trunc<mode>si2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(unsigned_fix:SI (fix:ANYF (match_operand:ANYF 1 "register_operand" "f"))))]
  ""
  "f<size>2ui.z\t %0, %1"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

(define_insn "fix_trunc<mode>si2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:ANYF (match_operand:ANYF 1 "register_operand" "f"))))]
  ""
  "f<size>2si.z\t %0, %1"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU_SINGLE && TARGET_FPU_DOUBLE"
  "fs2d\t%0, %1"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_FPU_SINGLE && TARGET_FPU_DOUBLE"
  "fd2s\t%0, %1"
  [(set_attr "type"   "falu")
   (set_attr "length" "4")]
)

;; Compare Instructions

(define_insn "cmp<mode>_eq"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(eq:SI (match_operand:ANYF 1 "register_operand" "f")
	       (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  {
    if (NDS32_EXT_FPU_DOT_E)
      return "fcmpeq<size>.e %0, %1, %2";
    else
      return "fcmpeq<size>\t%0, %1, %2";
  }
  [(set_attr "type"   "fcmp")
   (set_attr "length" "4")]
)

(define_insn "cmp<mode>_lt"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(lt:SI (match_operand:ANYF 1 "register_operand" "f")
	       (match_operand:ANYF 2 "register_operand" "f")))]
  ""
{
  if (NDS32_EXT_FPU_DOT_E)
    return "fcmplt<size>.e %0, %1, %2";
  else
    return "fcmplt<size>\t%0, %1, %2";
}
  [(set_attr "type"   "fcmp")
   (set_attr "length" "4")]
)

(define_insn "cmp<mode>_le"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(le:SI (match_operand:ANYF 1 "register_operand" "f")
	       (match_operand:ANYF 2 "register_operand" "f")))]
  ""
{
  if (NDS32_EXT_FPU_DOT_E)
    return "fcmple<size>.e %0, %1, %2";
  else
    return "fcmple<size>\t%0, %1, %2";
}
  [(set_attr "type"   "fcmp")
   (set_attr "length" "4")]
)

(define_insn "cmp<mode>_un"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(unordered:SI (match_operand:ANYF 1 "register_operand" "f")
		      (match_operand:ANYF 2 "register_operand" "f")))]
  ""
{
  if (NDS32_EXT_FPU_DOT_E)
    return "fcmpun<size>.e %0, %1, %2";
  else
    return "fcmpun<size>\t%0, %1, %2";
}
  [(set_attr "type"   "fcmp")
   (set_attr "length" "4")]
)

(define_split
  [(set (match_operand:SF 0 "register_operand" "")
	(match_operand:SF 1 "register_operand" ""))]
  "!TARGET_FPU_SINGLE
   && NDS32_IS_FPR_REGNUM (REGNO (operands[0]))
   && NDS32_IS_FPR_REGNUM (REGNO (operands[1]))"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (match_dup 2))]
{
  operands[2] = gen_rtx_REG (SFmode, TA_REGNUM);
})

(define_split
  [(set (match_operand:SF 0 "register_operand" "")
	(match_operand:SF 1 "const_double_operand" ""))]
  "!satisfies_constraint_Cs20 (operands[1])
   && !satisfies_constraint_Chig (operands[1])"
  [(set (match_dup 0) (high:SF (match_dup 1)))
   (set (match_dup 0) (lo_sum:SF (match_dup 0) (match_dup 1)))])
;; ----------------------------------------------------------------------------
