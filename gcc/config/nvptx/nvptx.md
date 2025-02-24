;; Machine description for NVPTX.
;; Copyright (C) 2014-2025 Free Software Foundation, Inc.
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
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

(define_c_enum "unspec" [
   UNSPEC_ARG_REG

   UNSPEC_COPYSIGN
   UNSPEC_LOG2
   UNSPEC_EXP2
   UNSPEC_SIN
   UNSPEC_COS
   UNSPEC_TANH
   UNSPEC_ISINF

   UNSPEC_FPINT_FLOOR
   UNSPEC_FPINT_BTRUNC
   UNSPEC_FPINT_CEIL
   UNSPEC_FPINT_NEARBYINT

   UNSPEC_ALLOCA
   UNSPEC_SET_SOFTSTACK
   UNSPEC_STACKSAVE
   UNSPEC_STACKRESTORE

   UNSPEC_DIM_SIZE

   UNSPEC_BIT_CONV

   UNSPEC_VOTE_BALLOT

   UNSPEC_LANEID

   UNSPEC_SHUFFLE
   UNSPEC_BR_UNIFIED
])

(define_c_enum "unspecv" [
   UNSPECV_LOCK
   UNSPECV_CAS
   UNSPECV_CAS_LOCAL
   UNSPECV_XCHG
   UNSPECV_ST
   UNSPECV_BARRED_AND
   UNSPECV_BARRED_OR
   UNSPECV_BARRED_POPC
   UNSPECV_BARSYNC
   UNSPECV_WARPSYNC
   UNSPECV_UNIFORM_WARP_CHECK
   UNSPECV_MEMBAR
   UNSPECV_MEMBAR_CTA
   UNSPECV_MEMBAR_GL
   UNSPECV_DIM_POS

   UNSPECV_FORK
   UNSPECV_FORKED
   UNSPECV_JOINING
   UNSPECV_JOIN

   UNSPECV_NOUNROLL

   UNSPECV_SIMT_ENTER
   UNSPECV_SIMT_EXIT

   UNSPECV_RED_PART
])

(define_attr "subregs_ok" "false,true"
  (const_string "false"))

(define_attr "atomic" "false,true"
  (const_string "false"))

;; The nvptx operand predicates, in general, don't permit subregs and
;; only literal constants, which differ from the generic ones, which
;; permit subregs and symbolc constants (as appropriate)
(define_predicate "nvptx_register_operand"
  (match_code "reg")
{
  return register_operand (op, mode);
})

(define_predicate "nvptx_register_or_complex_di_df_register_operand"
  (ior (match_code "reg")
       (match_code "concat"))
{
  if (GET_CODE (op) == CONCAT)
    return ((GET_MODE (op) == DCmode || GET_MODE (op) == CDImode)
	    && nvptx_register_operand (XEXP (op, 0), mode)
	    && nvptx_register_operand (XEXP (op, 1), mode));

  return nvptx_register_operand (op, mode);
})

(define_predicate "nvptx_nonimmediate_operand"
  (match_code "mem,reg")
{
  return (REG_P (op) ? register_operand (op, mode)
          : memory_operand (op, mode));
})

(define_predicate "nvptx_nonmemory_operand"
  (match_code "reg,const_int,const_double")
{
  return (REG_P (op) ? register_operand (op, mode)
          : immediate_operand (op, mode));
})

(define_predicate "const0_operand"
  (and (match_code "const_int")
       (match_test "op == const0_rtx")))

;; True if this operator is valid for predication.
(define_predicate "predicate_operator"
  (match_code "eq,ne"))

(define_predicate "ne_operator"
  (match_code "ne"))

(define_predicate "nvptx_comparison_operator"
  (match_code "eq,ne,le,ge,lt,gt,leu,geu,ltu,gtu"))

(define_predicate "nvptx_float_comparison_operator"
  (match_code "eq,ne,le,ge,lt,gt,uneq,unle,unge,unlt,ungt,unordered,ordered"))

(define_predicate "nvptx_vector_index_operand"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) < 4")))

;; Test for a valid operand for a call instruction.
(define_predicate "call_insn_operand"
  (match_code "symbol_ref,reg")
{
  return REG_P (op) || SYMBOL_REF_FUNCTION_P (op);
})

;; Return true if OP is a call with parallel USEs of the argument
;; pseudos.
(define_predicate "call_operation"
  (match_code "parallel")
{
  int arg_end = XVECLEN (op, 0);

  for (int i = 1; i < arg_end; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != USE || !REG_P (XEXP (elt, 0)))
        return false;
    }
  return true;
})

;; Test for a function symbol ref operand
(define_predicate "symbol_ref_function_operand"
  (match_code "symbol_ref")
{
  return SYMBOL_REF_FUNCTION_P (op);
})

(define_attr "predicable" "no,yes"
  (const_string "yes"))

(define_cond_exec
  [(match_operator 0 "predicate_operator"
      [(match_operand:BI 1 "nvptx_register_operand" "")
       (match_operand:BI 2 "const0_operand" "")])]
  ""
  ""
  )

(define_constraint "P0"
  "An integer with the value 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "P1"
  "An integer with the value 1."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "Pn"
  "An integer with the value -1."
  (and (match_code "const_int")
       (match_test "ival == -1")))

(define_constraint "R"
  "A pseudo register."
  (match_code "reg"))

(define_constraint "Ia"
  "Any integer constant."
  (and (match_code "const_int") (match_test "true")))

(define_mode_iterator QHSDISDFM [QI HI SI DI SF DF])
(define_mode_iterator QHSDIM [QI HI SI DI])
(define_mode_iterator HSDIM [HI SI DI])
(define_mode_iterator BHSDIM [BI HI SI DI])
(define_mode_iterator SDIM [SI DI])
(define_mode_iterator SDISDFM [SI DI SF DF])
(define_mode_iterator QHIM [QI HI])
(define_mode_iterator QHSIM [QI HI SI])
(define_mode_iterator SDFM [SF DF])
(define_mode_iterator HSFM [HF SF])
(define_mode_iterator SDCM [SC DC])
(define_mode_iterator BITS [SI SF])
(define_mode_iterator BITD [DI DF])
(define_mode_iterator VECIM [V2SI V2DI])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; Define element mode for each vector mode.
(define_mode_attr VECELEM [(V2SI "SI") (V2DI "DI")])
(define_mode_attr Vecelem [(V2SI "si") (V2DI "di")])

;; We should get away with not defining memory alternatives, since we don't
;; get variables in this mode and pseudos are never spilled.
(define_insn "movbi"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R,R,R")
	(match_operand:BI 1 "nvptx_nonmemory_operand" "R,P0,P1"))]
  ""
  "@
   %.\\tmov%t0\\t%0, %1;
   %.\\tsetp.eq.u32\\t%0, 1, 0;
   %.\\tsetp.eq.u32\\t%0, 1, 1;")

(define_insn "*mov<mode>_insn"
  [(set (match_operand:VECIM 0 "nonimmediate_operand" "=R,R,m")
	(match_operand:VECIM 1 "general_operand" "Ri,m,R"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
{
  if (which_alternative == 1)
    return "%.\\tld%A1%u1\\t%0, %1;";
  if (which_alternative == 2)
    return "%.\\tst%A0%u0\\t%0, %1;";

  return nvptx_output_mov_insn (operands[0], operands[1]);
}
  [(set_attr "subregs_ok" "true")])

(define_insn "*mov<mode>_insn"
  [(set (match_operand:QHSDIM 0 "nonimmediate_operand" "=R,R,m")
	(match_operand:QHSDIM 1 "general_operand" "Ri,m,R"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
{
  if (which_alternative == 1)
    return "%.\\tld%A1%u1\\t%0, %1;";
  if (which_alternative == 2)
    return "%.\\tst%A0%u0\\t%0, %1;";

  return nvptx_output_mov_insn (operands[0], operands[1]);
}
  [(set_attr "subregs_ok" "true")])

;; ptxas segfaults on 'mov.u64 %r24,bar+4096', so break it up.
(define_split
  [(set (match_operand:DI 0 "nvptx_register_operand")
	(const:DI (plus:DI (match_operand:DI 1 "symbol_ref_function_operand")
			   (match_operand 2 "const_int_operand"))))]
  ""
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 2)))
  ]
  "")

(define_insn "*mov<mode>_insn"
  [(set (match_operand:SDFM 0 "nonimmediate_operand" "=R,R,m")
	(match_operand:SDFM 1 "general_operand" "RF,m,R"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
{
  if (which_alternative == 1)
    return "%.\\tld%A1%u0\\t%0, %1;";
  if (which_alternative == 2)
    return "%.\\tst%A0%u1\\t%0, %1;";

  return nvptx_output_mov_insn (operands[0], operands[1]);
}
  [(set_attr "subregs_ok" "true")])

(define_insn "*movhf_insn"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=R,R,m")
	(match_operand:HF 1 "nonimmediate_operand" "R,m,R"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
  "@
   %.\\tmov.b16\\t%0, %1;
   %.\\tld.b16\\t%0, %1;
   %.\\tst.b16\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_expand "movhf"
  [(set (match_operand:HF 0 "nonimmediate_operand" "")
	(match_operand:HF 1 "nonimmediate_operand" ""))]
  ""
{
  /* Load HFmode constants as SFmode with an explicit FLOAT_TRUNCATE.  */
  if (CONST_DOUBLE_P (operands[1]))
    {
      rtx tmp1 = gen_reg_rtx (SFmode);
      REAL_VALUE_TYPE d = *CONST_DOUBLE_REAL_VALUE (operands[1]);
      real_convert (&d, SFmode, &d);
      emit_move_insn (tmp1, const_double_from_real_value (d, SFmode));

      if (!REG_P (operands[0]))
	{
	  rtx tmp2 = gen_reg_rtx (HFmode);
	  emit_insn (gen_truncsfhf2 (tmp2, tmp1));
	  emit_move_insn (operands[0], tmp2);
	}
      else
        emit_insn (gen_truncsfhf2 (operands[0], tmp1));
      DONE;
    }

  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    {
      rtx tmp = gen_reg_rtx (HFmode);
      emit_move_insn (tmp, operands[1]);
      emit_move_insn (operands[0], tmp);
      DONE;
    }
})

(define_insn "load_arg_reg<mode>"
  [(set (match_operand:QHIM 0 "nvptx_register_operand" "=R")
	(unspec:QHIM [(match_operand 1 "const_int_operand" "n")]
		     UNSPEC_ARG_REG))]
  ""
  "%.\\tcvt%t0.u32\\t%0, %%ar%1;")

(define_insn "load_arg_reg<mode>"
  [(set (match_operand:SDISDFM 0 "nvptx_register_operand" "=R")
	(unspec:SDISDFM [(match_operand 1 "const_int_operand" "n")]
			UNSPEC_ARG_REG))]
  ""
  "%.\\tmov%t0\\t%0, %%ar%1;")

 (define_expand "mov<mode>"
  [(set (match_operand:VECIM 0 "nonimmediate_operand" "")
	(match_operand:VECIM 1 "general_operand" ""))]
  ""
{
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      emit_move_insn (tmp, operands[1]);
      emit_move_insn (operands[0], tmp);
      DONE;
    }
})

(define_expand "mov<mode>"
  [(set (match_operand:QHSDISDFM 0 "nonimmediate_operand" "")
	(match_operand:QHSDISDFM 1 "general_operand" ""))]
  ""
{
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      emit_move_insn (tmp, operands[1]);
      emit_move_insn (operands[0], tmp);
      DONE;
    }

  if (GET_CODE (operands[1]) == LABEL_REF)
    sorry ("target cannot support label values");
})

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "nvptx_register_operand" "=R,R")
	(zero_extend:HI (match_operand:QI 1 "nvptx_nonimmediate_operand" "R,m")))]
  ""
  "@
   %.\\tcvt.u16.u%T1\\t%0, %1;
   %.\\tld%A1.u8\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "zero_extend<mode>si2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R,R")
	(zero_extend:SI (match_operand:QHIM 1 "nvptx_nonimmediate_operand" "R,m")))]
  ""
  "@
   %.\\tcvt.u32.u%T1\\t%0, %1;
   %.\\tld%A1.u%T1\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "zero_extend<mode>di2"
  [(set (match_operand:DI 0 "nvptx_register_operand" "=R,R")
	(zero_extend:DI (match_operand:QHSIM 1 "nvptx_nonimmediate_operand" "R,m")))]
  ""
  "@
   %.\\tcvt.u64.u%T1\\t%0, %1;
   %.\\tld%A1%u1\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "nvptx_register_operand" "=R")
	(sign_extend:HI (match_operand:QI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt.s16.s8\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "extend<mode>si2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R,R")
	(sign_extend:SI (match_operand:QHIM 1 "nvptx_nonimmediate_operand" "R,m")))]
  ""
  "@
   %.\\tcvt.s32.s%T1\\t%0, %1;
   %.\\tld%A1.s%T1\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "extend<mode>di2"
  [(set (match_operand:DI 0 "nvptx_register_operand" "=R,R")
	(sign_extend:DI (match_operand:QHSIM 1 "nvptx_nonimmediate_operand" "R,m")))]
  ""
  "@
   %.\\tcvt.s64.s%T1\\t%0, %1;
   %.\\tld%A1.s%T1\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "nvptx_nonimmediate_operand" "=R,m")
	(truncate:QI (match_operand:HI 1 "nvptx_register_operand" "R,R")))]
  ""
  "@
   %.\\tcvt%t0.u16\\t%0, %1;
   %.\\tst%A0.u8\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "truncsi<mode>2"
  [(set (match_operand:QHIM 0 "nvptx_nonimmediate_operand" "=R,m")
	(truncate:QHIM (match_operand:SI 1 "nvptx_register_operand" "R,R")))]
  ""
  {
    if (which_alternative == 1)
      return "%.\\tst%A0.u%T0\\t%0, %1;";
    if (GET_MODE (operands[0]) == QImode)
      return "%.\\tmov%t0\\t%0, %1;";
    return "%.\\tcvt%t0.u32\\t%0, %1;";
  }
  [(set_attr "subregs_ok" "true")])

(define_insn "truncdi<mode>2"
  [(set (match_operand:QHSIM 0 "nvptx_nonimmediate_operand" "=R,m")
	(truncate:QHSIM (match_operand:DI 1 "nvptx_register_operand" "R,R")))]
  ""
  "@
   %.\\tcvt%t0.u64\\t%0, %1;
   %.\\tst%A0.u%T0\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

;; Sign-extensions of truncations

(define_insn "*extend_trunc_<mode>2_qi"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(sign_extend:HSDIM
	 (truncate:QI (match_operand:HSDIM 1 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tcvt.s%T0.s8\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "*extend_trunc_<mode>2_hi"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(sign_extend:SDIM
	 (truncate:HI (match_operand:SDIM 1 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tcvt.s%T0.s16\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "*extend_trunc_di2_si"
  [(set (match_operand:DI 0 "nvptx_register_operand" "=R")
	(sign_extend:DI
	 (truncate:SI (match_operand:DI 1 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tcvt.s64.s32\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

;; Integer arithmetic

(define_insn "add<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(plus:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tadd%t0\\t%0, %1, %2;")

(define_insn "*vadd_addsi4"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
        (plus:SI (plus:SI (match_operand:SI 1 "nvptx_register_operand" "R")
			  (match_operand:SI 2 "nvptx_register_operand" "R"))
		 (match_operand:SI 3 "nvptx_register_operand" "R")))]
  ""
  "%.\\tvadd%t0%t1%t2.add\\t%0, %1, %2, %3;")

(define_insn "*vsub_addsi4"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
        (plus:SI (minus:SI (match_operand:SI 1 "nvptx_register_operand" "R")
			   (match_operand:SI 2 "nvptx_register_operand" "R"))
		 (match_operand:SI 3 "nvptx_register_operand" "R")))]
  ""
  "%.\\tvsub%t0%t1%t2.add\\t%0, %1, %2, %3;")

(define_insn "sub<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(minus:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		     (match_operand:HSDIM 2 "nvptx_register_operand" "R")))]
  ""
  {
    if (GET_MODE (operands[0]) == HImode)
      /* Workaround https://developer.nvidia.com/nvidia_bug/3527713.
	 See PR97005.  */
      return "%.\\tsub.s16\\t%0, %1, %2;";

    return "%.\\tsub%t0\\t%0, %1, %2;";
  })

(define_insn "mul<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(mult:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmul.lo%t0\\t%0, %1, %2;")

(define_insn "*mad<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(plus:HSDIM (mult:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
				(match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri"))
		    (match_operand:HSDIM 3 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmad.lo%t0\\t%0, %1, %2, %3;")

(define_insn "div<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(div:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		   (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tdiv.s%T0\\t%0, %1, %2;")

(define_insn "udiv<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(udiv:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		   (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tdiv.u%T0\\t%0, %1, %2;")

(define_insn "mod<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(mod:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "Ri")
		   (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\trem.s%T0\\t%0, %1, %2;")

(define_insn "umod<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(umod:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "Ri")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\trem.u%T0\\t%0, %1, %2;")

(define_insn "smin<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(smin:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmin.s%T0\\t%0, %1, %2;")

(define_insn "umin<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(umin:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmin.u%T0\\t%0, %1, %2;")

(define_insn "smax<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(smax:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmax.s%T0\\t%0, %1, %2;")

(define_insn "umax<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(umax:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmax.u%T0\\t%0, %1, %2;")

(define_insn "abs<mode>2"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(abs:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tabs.s%T0\\t%0, %1;")

(define_insn "neg<mode>2"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(neg:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tneg.s%T0\\t%0, %1;")

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(not:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tnot.b%T0\\t%0, %1;")

(define_insn "one_cmplbi2"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R")
	(not:BI (match_operand:BI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tnot.pred\\t%0, %1;")

(define_insn "*cnot<mode>2"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(eq:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		  (const_int 0)))]
  ""
  "%.\\tcnot.b%T0\\t%0, %1;")

(define_insn "bitrev<mode>2"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(bitreverse:SDIM (match_operand:SDIM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tbrev.b%T0\\t%0, %1;")

(define_insn "clz<mode>2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(clz:SI (match_operand:SDIM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tclz.b%T1\\t%0, %1;")

(define_expand "ctz<mode>2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "")
	(ctz:SI (match_operand:SDIM 1 "nvptx_register_operand" "")))]
  ""
{
  rtx tmpreg = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_bitrev<mode>2 (tmpreg, operands[1]));
  emit_insn (gen_clz<mode>2 (operands[0], tmpreg));
  DONE;
})

(define_insn "popcountsi2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(popcount:SI (match_operand:SI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tpopc.b32\\t%0, %1;")

(define_insn "popcountdi2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(truncate:SI
	  (popcount:DI (match_operand:DI 1 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tpopc.b64\\t%0, %1;")

;; Multiplication variants

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "nvptx_register_operand" "R"))
		 (sign_extend:SI
		  (match_operand:HI 2 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tmul.wide.s16\\t%0, %1, %2;")

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "nvptx_register_operand" "=R")
	(mult:DI (sign_extend:DI
		  (match_operand:SI 1 "nvptx_register_operand" "R"))
		 (sign_extend:DI
		  (match_operand:SI 2 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tmul.wide.s32\\t%0, %1, %2;")

(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "nvptx_register_operand" "R"))
		 (zero_extend:SI
		  (match_operand:HI 2 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tmul.wide.u16\\t%0, %1, %2;")

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "nvptx_register_operand" "=R")
	(mult:DI (zero_extend:DI
		  (match_operand:SI 1 "nvptx_register_operand" "R"))
		 (zero_extend:DI
		  (match_operand:SI 2 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tmul.wide.u32\\t%0, %1, %2;")

(define_expand "mulditi3"
  [(set (match_operand:TI 0 "nvptx_register_operand")
	(mult:TI (sign_extend:TI
		  (match_operand:DI 1 "nvptx_register_operand"))
		 (sign_extend:DI
		  (match_operand:DI 2 "nvptx_nonmemory_operand"))))]
  ""
{
  rtx hi = gen_reg_rtx (DImode);
  rtx lo = gen_reg_rtx (DImode);
  emit_insn (gen_smuldi3_highpart (hi, operands[1], operands[2]));
  emit_insn (gen_muldi3 (lo, operands[1], operands[2]));
  emit_move_insn (gen_highpart (DImode, operands[0]), hi);
  emit_move_insn (gen_lowpart (DImode, operands[0]), lo);
  DONE;
})

(define_expand "umulditi3"
  [(set (match_operand:TI 0 "nvptx_register_operand")
	(mult:TI (zero_extend:TI
		  (match_operand:DI 1 "nvptx_register_operand"))
		 (zero_extend:DI
		  (match_operand:DI 2 "nvptx_nonmemory_operand"))))]
  ""
{
  rtx hi = gen_reg_rtx (DImode);
  rtx lo = gen_reg_rtx (DImode);
  emit_insn (gen_umuldi3_highpart (hi, operands[1], operands[2]));
  emit_insn (gen_muldi3 (lo, operands[1], operands[2]));
  emit_move_insn (gen_highpart (DImode, operands[0]), hi);
  emit_move_insn (gen_lowpart (DImode, operands[0]), lo);
  DONE;
})

(define_insn "smul<mode>3_highpart"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(smul_highpart:HSDIM
	  (match_operand:HSDIM 1 "nvptx_register_operand" "R")
	  (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmul.hi.s%T0\\t%0, %1, %2;")

(define_insn "umul<mode>3_highpart"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(umul_highpart:HSDIM
	  (match_operand:HSDIM 1 "nvptx_register_operand" "R")
	  (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tmul.hi.u%T0\\t%0, %1, %2;")

(define_insn "*smulhi3_highpart_2"
  [(set (match_operand:HI 0 "nvptx_register_operand" "=R")
	(truncate:HI
	 (lshiftrt:SI
	  (mult:SI (sign_extend:SI
		    (match_operand:HI 1 "nvptx_register_operand" "R"))
		   (sign_extend:SI
		    (match_operand:HI 2 "nvptx_register_operand" "R")))
	  (const_int 16))))]
  ""
  "%.\\tmul.hi.s16\\t%0, %1, %2;")

(define_insn "*smulsi3_highpart_2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI
		    (match_operand:SI 1 "nvptx_register_operand" "R"))
		   (sign_extend:DI
		    (match_operand:SI 2 "nvptx_register_operand" "R")))
	  (const_int 32))))]
  ""
  "%.\\tmul.hi.s32\\t%0, %1, %2;")

(define_insn "*umulhi3_highpart_2"
  [(set (match_operand:HI 0 "nvptx_register_operand" "=R")
	(truncate:HI
	 (lshiftrt:SI
	  (mult:SI (zero_extend:SI
		    (match_operand:HI 1 "nvptx_register_operand" "R"))
		   (zero_extend:SI
		    (match_operand:HI 2 "nvptx_register_operand" "R")))
	  (const_int 16))))]
  ""
  "%.\\tmul.hi.u16\\t%0, %1, %2;")

(define_insn "*umulsi3_highpart_2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI
		    (match_operand:SI 1 "nvptx_register_operand" "R"))
		   (zero_extend:DI
		    (match_operand:SI 2 "nvptx_register_operand" "R")))
	  (const_int 32))))]
  ""
  "%.\\tmul.hi.u32\\t%0, %1, %2;")

;; Shifts

(define_insn "ashl<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(ashift:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		      (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tshl.b%T0\\t%0, %1, %2;")

(define_insn "ashr<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(ashiftrt:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
			(match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tshr.s%T0\\t%0, %1, %2;")

(define_insn "lshr<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(lshiftrt:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
			(match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tshr.u%T0\\t%0, %1, %2;")

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(rotate:SI (match_operand:SI 1 "nvptx_register_operand" "R")
		   (and:SI (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")
			   (const_int 31))))]
  "TARGET_SM35"
  "%.\\tshf.l.wrap.b32\\t%0, %1, %1, %2;")

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(rotatert:SI (match_operand:SI 1 "nvptx_register_operand" "R")
		     (and:SI (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")
			     (const_int 31))))]
  "TARGET_SM35"
  "%.\\tshf.r.wrap.b32\\t%0, %1, %1, %2;")

;; Logical operations

(define_code_iterator any_logic [and ior xor])
(define_code_attr logic [(and "and") (ior "or") (xor "xor")])
(define_code_attr ilogic [(and "and") (ior "ior") (xor "xor")])

(define_insn "<ilogic><mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(any_logic:HSDIM
	  (match_operand:HSDIM 1 "nvptx_register_operand" "R")
	  (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\t<logic>.b%T0\\t%0, %1, %2;")

(define_insn "<ilogic>bi3"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R")
	(any_logic:BI (match_operand:BI 1 "nvptx_register_operand" "R")
		      (match_operand:BI 2 "nvptx_register_operand" "R")))]
  ""
  "%.\\t<logic>.pred\\t%0, %1, %2;")

(define_split
  [(set (match_operand:HSDIM 0 "nvptx_register_operand")
	(any_logic:HSDIM
	  (ne:HSDIM (match_operand:BI 1 "nvptx_register_operand")
		    (const_int 0))
	  (ne:HSDIM (match_operand:BI 2 "nvptx_register_operand")
		    (const_int 0))))]
  "can_create_pseudo_p ()"
  [(set (match_dup 3) (any_logic:BI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ne:HSDIM (match_dup 3) (const_int 0)))]
{
  operands[3] = gen_reg_rtx (BImode);
})

;; Comparisons and branches

(define_insn "cmp<mode>"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R")
	(match_operator:BI 1 "nvptx_comparison_operator"
	   [(match_operand:HSDIM 2 "nvptx_register_operand" "R")
	    (match_operand:HSDIM 3 "nvptx_nonmemory_operand" "Ri")]))]
  ""
  "%.\\tsetp%c1\\t%0, %2, %3;")

(define_insn "*cmp<mode>"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R")
	(match_operator:BI 1 "nvptx_float_comparison_operator"
	   [(match_operand:SDFM 2 "nvptx_register_operand" "R")
	    (match_operand:SDFM 3 "nvptx_nonmemory_operand" "RF")]))]
  ""
  "%.\\tsetp%c1\\t%0, %2, %3;")

(define_insn "*cmphf"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R")
	(match_operator:BI 1 "nvptx_float_comparison_operator"
	   [(match_operand:HF 2 "nvptx_register_operand" "R")
	    (match_operand:HF 3 "nvptx_nonmemory_operand" "RF")]))]
  "TARGET_SM53"
  "%.\\tsetp%c1\\t%0, %2, %3;")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "%.\\tbra\\t%l0;")

(define_insn "br_true"
  [(set (pc)
	(if_then_else (ne (match_operand:BI 0 "nvptx_register_operand" "R")
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "%j0\\tbra\\t%l1;"
  [(set_attr "predicable" "no")])

(define_insn "br_false"
  [(set (pc)
	(if_then_else (eq (match_operand:BI 0 "nvptx_register_operand" "R")
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "%J0\\tbra\\t%l1;"
  [(set_attr "predicable" "no")])

;; unified conditional branch
(define_insn "br_true_uni"
  [(set (pc) (if_then_else
	(ne (unspec:BI [(match_operand:BI 0 "nvptx_register_operand" "R")]
		       UNSPEC_BR_UNIFIED) (const_int 0))
        (label_ref (match_operand 1 "" "")) (pc)))]
  ""
  "%j0\\tbra.uni\\t%l1;"
  [(set_attr "predicable" "no")])

(define_insn "br_false_uni"
  [(set (pc) (if_then_else
	(eq (unspec:BI [(match_operand:BI 0 "nvptx_register_operand" "R")]
		       UNSPEC_BR_UNIFIED) (const_int 0))
        (label_ref (match_operand 1 "" "")) (pc)))]
  ""
  "%J0\\tbra.uni\\t%l1;"
  [(set_attr "predicable" "no")])

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "nvptx_comparison_operator"
		       [(match_operand:HSDIM 1 "nvptx_register_operand" "")
			(match_operand:HSDIM 2 "nvptx_nonmemory_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
{
  rtx t = nvptx_expand_compare (operands[0]);
  operands[0] = t;
  operands[1] = XEXP (t, 0);
  operands[2] = XEXP (t, 1);
})

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "nvptx_float_comparison_operator"
		       [(match_operand:SDFM 1 "nvptx_register_operand" "")
			(match_operand:SDFM 2 "nvptx_nonmemory_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
{
  rtx t = nvptx_expand_compare (operands[0]);
  operands[0] = t;
  operands[1] = XEXP (t, 0);
  operands[2] = XEXP (t, 1);
})

(define_expand "cbranchbi4"
  [(set (pc)
	(if_then_else (match_operator 0 "predicate_operator"
		       [(match_operand:BI 1 "nvptx_register_operand" "")
			(match_operand:BI 2 "const0_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "")

;; Conditional stores

(define_insn "setcc<mode>_from_bi"
  [(set (match_operand:QHSDIM 0 "nvptx_register_operand" "=R")
	(ne:QHSDIM (match_operand:BI 1 "nvptx_register_operand" "R")
		   (const_int 0)))]
  ""
  "%.\\tselp%t0\\t%0, 1, 0, %1;")

(define_insn "*setcc<mode>_from_not_bi"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(eq:HSDIM (match_operand:BI 1 "nvptx_register_operand" "R")
		   (const_int 0)))]
  ""
  "%.\\tselp%t0\\t%0, 0, 1, %1;")

(define_insn "extendbi<mode>2"
  [(set (match_operand:QHSDIM 0 "nvptx_register_operand" "=R")
	(sign_extend:QHSDIM
	 (match_operand:BI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tselp%t0\\t%0, -1, 0, %1;")

(define_insn "zero_extendbi<mode>2"
  [(set (match_operand:QHSDIM 0 "nvptx_register_operand" "=R")
	(zero_extend:QHSDIM
	 (match_operand:BI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tselp%t0\\t%0, 1, 0, %1;")

(define_insn "sel_true<mode>"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(if_then_else:HSDIM
	  (ne (match_operand:BI 1 "nvptx_register_operand" "R") (const_int 0))
	  (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")
	  (match_operand:HSDIM 3 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tselp%t0\\t%0, %2, %3, %1;")

(define_insn "sel_true<mode>"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(if_then_else:SDFM
	  (ne (match_operand:BI 1 "nvptx_register_operand" "R") (const_int 0))
	  (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")
	  (match_operand:SDFM 3 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tselp%t0\\t%0, %2, %3, %1;")

(define_insn "sel_false<mode>"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(if_then_else:HSDIM
	  (eq (match_operand:BI 1 "nvptx_register_operand" "R") (const_int 0))
	  (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")
	  (match_operand:HSDIM 3 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tselp%t0\\t%0, %3, %2, %1;")

(define_insn "sel_false<mode>"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(if_then_else:SDFM
	  (eq (match_operand:BI 1 "nvptx_register_operand" "R") (const_int 0))
	  (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")
	  (match_operand:SDFM 3 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tselp%t0\\t%0, %3, %2, %1;")

(define_code_iterator eqne [eq ne])

;; Split negation of a predicate into a conditional move.
(define_insn_and_split "*selp<mode>_neg_<code>"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(neg:HSDIM (eqne:HSDIM
		     (match_operand:BI 1 "nvptx_register_operand" "R")
		     (const_int 0))))]
  ""
  "#"
  "&& 1"
  [(set (match_dup 0)
	(if_then_else:HSDIM
	  (eqne (match_dup 1) (const_int 0))
	  (const_int -1)
	  (const_int 0)))])

;; Split bitwise not of a predicate into a conditional move.
(define_insn_and_split "*selp<mode>_not_<code>"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(not:HSDIM (eqne:HSDIM
		     (match_operand:BI 1 "nvptx_register_operand" "R")
		     (const_int 0))))]
  ""
  "#"
  "&& 1"
  [(set (match_dup 0)
	(if_then_else:HSDIM
	  (eqne (match_dup 1) (const_int 0))
	  (const_int -2)
	  (const_int -1)))])

(define_insn "*setcc_int<mode>"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(neg:SI
	  (match_operator:SI 1 "nvptx_comparison_operator"
	    [(match_operand:HSDIM 2 "nvptx_register_operand" "R")
	     (match_operand:HSDIM 3 "nvptx_nonmemory_operand" "Ri")])))]
  ""
  "%.\\tset%t0%c1\\t%0, %2, %3;")

(define_insn "*setcc_int<mode>"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(neg:SI
	  (match_operator:SI 1 "nvptx_float_comparison_operator"
	    [(match_operand:SDFM 2 "nvptx_register_operand" "R")
	     (match_operand:SDFM 3 "nvptx_nonmemory_operand" "RF")])))]
  ""
  "%.\\tset%t0%c1\\t%0, %2, %3;")

(define_insn "setcc_float<mode>"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(match_operator:SF 1 "nvptx_comparison_operator"
	   [(match_operand:HSDIM 2 "nvptx_register_operand" "R")
	    (match_operand:HSDIM 3 "nvptx_nonmemory_operand" "Ri")]))]
  ""
  "%.\\tset%t0%c1\\t%0, %2, %3;")

(define_insn "setcc_float<mode>"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(match_operator:SF 1 "nvptx_float_comparison_operator"
	   [(match_operand:SDFM 2 "nvptx_register_operand" "R")
	    (match_operand:SDFM 3 "nvptx_nonmemory_operand" "RF")]))]
  ""
  "%.\\tset%t0%c1\\t%0, %2, %3;")

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "nvptx_register_operand")
	(match_operator:SI 1 "nvptx_comparison_operator"
	  [(match_operand:HSDIM 2 "nvptx_register_operand")
	   (match_operand:HSDIM 3 "nvptx_nonmemory_operand")]))]
  ""
{
  rtx reg = gen_reg_rtx (BImode);
  rtx cmp = gen_rtx_fmt_ee (GET_CODE (operands[1]), BImode,
			    operands[2], operands[3]);
  emit_move_insn (reg, cmp);
  emit_insn (gen_setccsi_from_bi (operands[0], reg));
  DONE;
})

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "nvptx_register_operand")
	(match_operator:SI 1 "nvptx_float_comparison_operator"
	  [(match_operand:SDFM 2 "nvptx_register_operand")
	   (match_operand:SDFM 3 "nvptx_nonmemory_operand")]))]
  ""
{
  rtx reg = gen_reg_rtx (BImode);
  rtx cmp = gen_rtx_fmt_ee (GET_CODE (operands[1]), BImode,
			    operands[2], operands[3]);
  emit_move_insn (reg, cmp);
  emit_insn (gen_setccsi_from_bi (operands[0], reg));
  DONE;
})

(define_expand "cstorehf4"
  [(set (match_operand:SI 0 "nvptx_register_operand")
	(match_operator:SI 1 "nvptx_float_comparison_operator"
	  [(match_operand:HF 2 "nvptx_register_operand")
	   (match_operand:HF 3 "nvptx_nonmemory_operand")]))]
  "TARGET_SM53"
{
  rtx reg = gen_reg_rtx (BImode);
  rtx cmp = gen_rtx_fmt_ee (GET_CODE (operands[1]), BImode,
			    operands[2], operands[3]);
  emit_move_insn (reg, cmp);
  emit_insn (gen_setccsi_from_bi (operands[0], reg));
  DONE;
})

;; Calls

(define_insn "call_insn_<mode>"
  [(match_parallel 2 "call_operation"
    [(call (mem:QI (match_operand:P 0 "call_insn_operand" "Rs"))
	   (match_operand 1))])]
  ""
{
  return nvptx_output_call_insn (insn, NULL_RTX, operands[0]);
})

(define_insn "call_value_insn_<mode>"
  [(match_parallel 3 "call_operation"
    [(set (match_operand 0 "nvptx_register_operand" "=R")
	  (call (mem:QI (match_operand:P 1 "call_insn_operand" "Rs"))
		(match_operand 2)))])]
  ""
{
  return nvptx_output_call_insn (insn, operands[0], operands[1]);
})

(define_expand "call"
 [(match_operand 0 "" "")]
 ""
{
  nvptx_expand_call (NULL_RTX, operands[0]);
  DONE;
})

(define_expand "call_value"
  [(match_operand 0 "" "")
   (match_operand 1 "" "")]
 ""
{
  nvptx_expand_call (operands[0], operands[1]);
  DONE;
})

;; Floating point arithmetic.

(define_insn "add<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(plus:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		   (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tadd%t0\\t%0, %1, %2;")

(define_insn "sub<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(minus:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		    (match_operand:SDFM 2 "nvptx_register_operand" "R")))]
  ""
  "%.\\tsub%t0\\t%0, %1, %2;")

(define_insn "mul<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(mult:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		   (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tmul%t0\\t%0, %1, %2;")

(define_insn "fma<mode>4"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(fma:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		  (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")
		  (match_operand:SDFM 3 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tfma%#%t0\\t%0, %1, %2, %3;")

(define_insn "*recip<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(div:SDFM
	  (match_operand:SDFM 2 "const_double_operand" "F")
	  (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  "CONST_DOUBLE_P (operands[2])
   && real_identical (CONST_DOUBLE_REAL_VALUE (operands[2]), &dconst1)"
  "%.\\trcp%#%t0\\t%0, %1;")

(define_insn "div<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(div:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		  (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tdiv%#%t0\\t%0, %1, %2;")

(define_insn "copysign<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(unspec:SDFM [(match_operand:SDFM 1 "nvptx_nonmemory_operand" "RF")
		      (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")]
		      UNSPEC_COPYSIGN))]
  ""
  "%.\\tcopysign%t0\\t%0, %2, %1;")

(define_insn "smin<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(smin:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		    (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tmin%t0\\t%0, %1, %2;")

(define_insn "smax<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(smax:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		    (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tmax%t0\\t%0, %1, %2;")

(define_insn "abs<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(abs:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tabs%t0\\t%0, %1;")

(define_insn "neg<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(neg:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tneg%t0\\t%0, %1;")

(define_insn "sqrt<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(sqrt:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tsqrt%#%t0\\t%0, %1;")

(define_expand "sincossf3"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(unspec:SF [(match_operand:SF 2 "nvptx_register_operand" "R")]
	           UNSPEC_COS))
   (set (match_operand:SF 1 "nvptx_register_operand" "=R")
	(unspec:SF [(match_dup 2)] UNSPEC_SIN))]
  "flag_unsafe_math_optimizations"
{
  operands[2] = make_safe_from (operands[2], operands[0]);
})

(define_insn "sinsf2"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(unspec:SF [(match_operand:SF 1 "nvptx_register_operand" "R")]
		   UNSPEC_SIN))]
  "flag_unsafe_math_optimizations"
  "%.\\tsin.approx%t0\\t%0, %1;")

(define_insn "cossf2"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(unspec:SF [(match_operand:SF 1 "nvptx_register_operand" "R")]
		   UNSPEC_COS))]
  "flag_unsafe_math_optimizations"
  "%.\\tcos.approx%t0\\t%0, %1;")

(define_insn "log2sf2"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(unspec:SF [(match_operand:SF 1 "nvptx_register_operand" "R")]
		   UNSPEC_LOG2))]
  "flag_unsafe_math_optimizations"
  "%.\\tlg2.approx%t0\\t%0, %1;")

(define_insn "exp2sf2"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(unspec:SF [(match_operand:SF 1 "nvptx_register_operand" "R")]
		   UNSPEC_EXP2))]
  "flag_unsafe_math_optimizations"
  "%.\\tex2.approx%t0\\t%0, %1;")

(define_insn "setcc_isinf<mode>"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R")
	(unspec:BI [(match_operand:SDFM 1 "nvptx_register_operand" "R")]
		   UNSPEC_ISINF))]
  ""
  "%.\\ttestp.infinite%t1\\t%0, %1;")

(define_expand "isinf<mode>2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(unspec:SI [(match_operand:SDFM 1 "nvptx_register_operand" "R")]
		   UNSPEC_ISINF))]
  ""
{
  rtx pred = gen_reg_rtx (BImode);
  emit_insn (gen_setcc_isinf<mode> (pred, operands[1]));
  emit_insn (gen_setccsi_from_bi (operands[0], pred));
  DONE;
})

;; HFmode floating point arithmetic.

(define_insn "addhf3"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(plus:HF (match_operand:HF 1 "nvptx_register_operand" "R")
		 (match_operand:HF 2 "nvptx_register_operand" "R")))]
  "TARGET_SM53"
  "%.\\tadd.f16\\t%0, %1, %2;")

(define_insn "subhf3"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(minus:HF (match_operand:HF 1 "nvptx_register_operand" "R")
		  (match_operand:HF 2 "nvptx_register_operand" "R")))]
  "TARGET_SM53"
  "%.\\tsub.f16\\t%0, %1, %2;")

(define_insn "mulhf3"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(mult:HF (match_operand:HF 1 "nvptx_register_operand" "R")
		 (match_operand:HF 2 "nvptx_register_operand" "R")))]
  "TARGET_SM53"
  "%.\\tmul.f16\\t%0, %1, %2;")

(define_insn "fmahf4"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(fma:HF (match_operand:HF 1 "nvptx_register_operand" "R")
		(match_operand:HF 2 "nvptx_nonmemory_operand" "RF")
		(match_operand:HF 3 "nvptx_nonmemory_operand" "RF")))]
  "TARGET_SM53"
  "%.\\tfma%#.f16\\t%0, %1, %2, %3;")

(define_insn "neghf2"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(neg:HF (match_operand:HF 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\txor.b16\\t%0, %1, -32768;")

(define_insn "abshf2"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(abs:HF (match_operand:HF 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tand.b16\\t%0, %1, 32767;")

(define_insn "exp2hf2"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(unspec:HF [(match_operand:HF 1 "nvptx_register_operand" "R")]
		   UNSPEC_EXP2))]
  "TARGET_SM75 && flag_unsafe_math_optimizations"
  "%.\\tex2.approx.f16\\t%0, %1;")

(define_insn "tanh<mode>2"
  [(set (match_operand:HSFM 0 "nvptx_register_operand" "=R")
	(unspec:HSFM [(match_operand:HSFM 1 "nvptx_register_operand" "R")]
		     UNSPEC_TANH))]
  "TARGET_SM75 && flag_unsafe_math_optimizations"
  "%.\\ttanh.approx%t0\\t%0, %1;")

;; HFmode floating point arithmetic.

(define_insn "sminhf3"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(smin:HF (match_operand:HF 1 "nvptx_register_operand" "R")
		 (match_operand:HF 2 "nvptx_register_operand" "R")))]
  "TARGET_SM80"
  "%.\\tmin.f16\\t%0, %1, %2;")

(define_insn "smaxhf3"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(smax:HF (match_operand:HF 1 "nvptx_register_operand" "R")
		 (match_operand:HF 2 "nvptx_register_operand" "R")))]
  "TARGET_SM80"
  "%.\\tmax.f16\\t%0, %1, %2;")

;; Conversions involving floating point

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "nvptx_register_operand" "=R")
	(float_extend:DF (match_operand:SF 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt%t0%t1\\t%0, %1;")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(float_truncate:SF (match_operand:DF 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt%#%t0%t1\\t%0, %1;")

(define_insn "floatunssi<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(unsigned_float:SDFM (match_operand:SI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt%#%t0.u%T1\\t%0, %1;")

(define_insn "floatsi<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(float:SDFM (match_operand:SI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt%#%t0.s%T1\\t%0, %1;")

(define_insn "floatunsdi<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(unsigned_float:SDFM (match_operand:DI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt%#%t0.u%T1\\t%0, %1;")

(define_insn "floatdi<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(float:SDFM (match_operand:DI 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt%#%t0.s%T1\\t%0, %1;")

(define_insn "fixuns_trunc<mode>si2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(unsigned_fix:SI (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt.rzi.u%T0%t1\\t%0, %1;")

(define_insn "fix_trunc<mode>si2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(fix:SI (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt.rzi.s%T0%t1\\t%0, %1;")

(define_insn "fixuns_trunc<mode>di2"
  [(set (match_operand:DI 0 "nvptx_register_operand" "=R")
	(unsigned_fix:DI (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt.rzi.u%T0%t1\\t%0, %1;")

(define_insn "fix_trunc<mode>di2"
  [(set (match_operand:DI 0 "nvptx_register_operand" "=R")
	(fix:DI (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  ""
  "%.\\tcvt.rzi.s%T0%t1\\t%0, %1;")

(define_int_iterator FPINT [UNSPEC_FPINT_FLOOR UNSPEC_FPINT_BTRUNC
			    UNSPEC_FPINT_CEIL UNSPEC_FPINT_NEARBYINT])
(define_int_attr fpint_name [(UNSPEC_FPINT_FLOOR "floor")
			     (UNSPEC_FPINT_BTRUNC "btrunc")
			     (UNSPEC_FPINT_CEIL "ceil")
			     (UNSPEC_FPINT_NEARBYINT "nearbyint")])
(define_int_attr fpint_roundingmode [(UNSPEC_FPINT_FLOOR ".rmi")
				     (UNSPEC_FPINT_BTRUNC ".rzi")
				     (UNSPEC_FPINT_CEIL ".rpi")
				     (UNSPEC_FPINT_NEARBYINT "%#i")])

(define_insn "<FPINT:fpint_name><SDFM:mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(unspec:SDFM [(match_operand:SDFM 1 "nvptx_register_operand" "R")]
		     FPINT))]
  ""
  "%.\\tcvt<FPINT:fpint_roundingmode>%t0%t1\\t%0, %1;")

(define_int_iterator FPINT2 [UNSPEC_FPINT_FLOOR UNSPEC_FPINT_CEIL])
(define_int_attr fpint2_name [(UNSPEC_FPINT_FLOOR "lfloor")
			     (UNSPEC_FPINT_CEIL "lceil")])
(define_int_attr fpint2_roundingmode [(UNSPEC_FPINT_FLOOR ".rmi")
				     (UNSPEC_FPINT_CEIL ".rpi")])

(define_insn "<FPINT2:fpint2_name><SDFM:mode><SDIM:mode>2"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(unspec:SDIM [(match_operand:SDFM 1 "nvptx_register_operand" "R")]
		     FPINT2))]
  ""
  "%.\\tcvt<FPINT2:fpint2_roundingmode>.s%T0%t1\\t%0, %1;")

(define_insn "extendhf<mode>2"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(float_extend:SDFM (match_operand:HF 1 "nvptx_register_operand" "R")))]
  "TARGET_SM53"
  "%.\\tcvt%t0%t1\\t%0, %1;")

(define_insn "trunc<mode>hf2"
  [(set (match_operand:HF 0 "nvptx_register_operand" "=R")
	(float_truncate:HF (match_operand:SDFM 1 "nvptx_register_operand" "R")))]
  "TARGET_SM53"
  "%.\\tcvt%#%t0%t1\\t%0, %1;")

;; Vector operations

(define_insn "*vec_set<mode>_0"
  [(set (match_operand:VECIM 0 "nvptx_register_operand" "=R")
	(vec_merge:VECIM
	  (vec_duplicate:VECIM
	    (match_operand:<VECELEM> 1 "nvptx_register_operand" "R"))
	  (match_dup 0)
	  (const_int 1)))]
  ""
  "%.\\tmov%t1\\t%0.x, %1;")

(define_insn "*vec_set<mode>_1"
  [(set (match_operand:VECIM 0 "nvptx_register_operand" "=R")
	(vec_merge:VECIM
	  (vec_duplicate:VECIM
	    (match_operand:<VECELEM> 1 "nvptx_register_operand" "R"))
	  (match_dup 0)
	  (const_int 2)))]
  ""
  "%.\\tmov%t1\\t%0.y, %1;")

(define_insn "*vec_set<mode>_2"
  [(set (match_operand:VECIM 0 "nvptx_register_operand" "=R")
	(vec_merge:VECIM
	  (vec_duplicate:VECIM
	    (match_operand:<VECELEM> 1 "nvptx_register_operand" "R"))
	  (match_dup 0)
	  (const_int 4)))]
  ""
  "%.\\tmov%t1\\t%0.z, %1;")

(define_insn "*vec_set<mode>_3"
  [(set (match_operand:VECIM 0 "nvptx_register_operand" "=R")
	(vec_merge:VECIM
	  (vec_duplicate:VECIM
	    (match_operand:<VECELEM> 1 "nvptx_register_operand" "R"))
	  (match_dup 0)
	  (const_int 8)))]
  ""
  "%.\\tmov%t1\\t%0.w, %1;")

(define_expand "vec_set<mode>"
  [(match_operand:VECIM 0 "nvptx_register_operand")
   (match_operand:<VECELEM> 1 "nvptx_register_operand")
   (match_operand:SI 2 "nvptx_vector_index_operand")]
  ""
{
  enum machine_mode mode = GET_MODE (operands[0]);
  int mask = 1 << INTVAL (operands[2]);
  rtx tmp = gen_rtx_VEC_DUPLICATE (mode, operands[1]);
  tmp = gen_rtx_VEC_MERGE (mode, tmp, operands[0], GEN_INT (mask));
  emit_insn (gen_rtx_SET (operands[0], tmp));
  DONE;
})

(define_insn "vec_extract<mode><Vecelem>"
  [(set (match_operand:<VECELEM> 0 "nvptx_register_operand" "=R")
	(vec_select:<VECELEM>
	  (match_operand:VECIM 1 "nvptx_register_operand" "R")
	  (parallel [(match_operand:SI 2 "nvptx_vector_index_operand" "")])))]
  ""
{
  static const char *const asms[4] = {
    "%.\\tmov%t0\\t%0, %1.x;",
    "%.\\tmov%t0\\t%0, %1.y;",
    "%.\\tmov%t0\\t%0, %1.z;",
    "%.\\tmov%t0\\t%0, %1.w;"
  };
  return asms[INTVAL (operands[2])];
})

;; Miscellaneous

(define_insn "nop"
  [(const_int 0)]
  ""
  "")

(define_insn "exit"
  [(const_int 1)]
  ""
  "exit;")

(define_insn "fake_nop"
  [(const_int 2)]
  ""
  "{
     .reg .u32 %%nop_src;
     .reg .u32 %%nop_dst;
     mov.u32 %%nop_dst, %%nop_src;
   }")

(define_insn "return"
  [(return)]
  ""
{
  return nvptx_output_return ();
}
  [(set_attr "predicable" "no")])

(define_expand "epilogue"
  [(clobber (const_int 0))]
  ""
{
  if (TARGET_SOFT_STACK)
    emit_insn (gen_set_softstack (Pmode, gen_rtx_REG (Pmode,
						      SOFTSTACK_PREV_REGNUM)));
  emit_jump_insn (gen_return ());
  DONE;
})

(define_expand "exception_receiver"
  [(const_int 0)]
  ""
{
  sorry ("exception handling not supported");
})

(define_expand "nonlocal_goto"
  [(match_operand 0 "" "")
   (match_operand 1 "" "")
   (match_operand 2 "" "")
   (match_operand 3 "" "")]
  ""
{
  sorry ("target cannot support nonlocal goto");
  emit_insn (gen_nop ());
  DONE;
})

(define_expand "nonlocal_goto_receiver"
  [(const_int 0)]
  ""
{
  sorry ("target cannot support nonlocal goto");
})

(define_expand "allocate_stack"
  [(match_operand 0 "nvptx_register_operand")
   (match_operand 1 "nvptx_register_operand")]
  ""
{
  if (!TARGET_SOFT_STACK)
    emit_insn (gen_nvptx_alloca (Pmode, operands[0], operands[1]));
  else if (TARGET_SOFT_STACK)
    {
      emit_move_insn (stack_pointer_rtx,
		      gen_rtx_MINUS (Pmode, stack_pointer_rtx, operands[1]));
      emit_insn (gen_set_softstack (Pmode, stack_pointer_rtx));
      emit_move_insn (operands[0], virtual_stack_dynamic_rtx);
    }
  else
    gcc_unreachable ();
  DONE;
})

(define_insn "@nvptx_alloca_<mode>"
  [(set (match_operand:P 0 "nvptx_register_operand" "=R")
        (unspec:P [(match_operand:P 1 "nvptx_nonmemory_operand" "Ri")]
		  UNSPEC_ALLOCA))]
  ""
  {
    if (TARGET_PTX_7_3
	&& TARGET_SM52)
      {
	/* Convert the address from '.local' state space to generic.  That way,
	   we don't have to use 'st.local', 'ld.local', and can easily pass the
	   address to other "generic functions".
	   TODO 'gcc.target/nvptx/alloca-5.c' */
	output_asm_insn ("{", NULL);
	output_asm_insn ("\\t.reg%t0\\t%0_local;", operands);
	output_asm_insn ("\\talloca%u0\\t%0_local, %1;", operands);
	output_asm_insn ("\\tcvta.local%u0\\t%0, %0_local;", operands);
	output_asm_insn ("}", NULL);
	return "";
      }
    else if (nvptx_fake_ptx_alloca)
      return nvptx_output_fake_ptx_alloca ();
    else
      {
	sorry_at (INSN_LOCATION (insn),
		  "dynamic stack allocation not supported");
	return "";
      }
  }
  [(set_attr "predicable" "no")])

(define_insn "@set_softstack_<mode>"
  [(unspec [(match_operand:P 0 "nvptx_register_operand" "R")]
	   UNSPEC_SET_SOFTSTACK)]
  "TARGET_SOFT_STACK"
{
  return nvptx_output_set_softstack (REGNO (operands[0]));
})

(define_expand "save_stack_block"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  "!TARGET_SOFT_STACK"
{
  if (TARGET_PTX_7_3
      && TARGET_SM52)
    {
      gcc_checking_assert (REG_P (operands[0]));
      emit_insn (gen_nvptx_stacksave (Pmode, operands[0], operands[1]));
    }
  /* We don't bother to special-case '-mfake-ptx-alloca' here.  */
  else
    {
      /* The concept of a '%stack' pointer doesn't apply like this.
         GCC however occasionally synthesizes '__builtin_stack_save ()',
	 '__builtin_stack_restore ()', and isn't able to optimize them all
	 away.  Just submit a dummy -- user code shouldn't be able to observe
	 this.  */
      emit_move_insn (operands[0], GEN_INT (0xdeadbeef));
    }
  DONE;
})

(define_insn "@nvptx_stacksave_<mode>"
  [(set (match_operand:P 0 "nvptx_register_operand" "=R")
        (unspec:P [(match_operand:P 1 "register_operand" "R")]
	 UNSPEC_STACKSAVE))]
  "TARGET_PTX_7_3
   && TARGET_SM52"
  "%.\\tstacksave%u0\\t%0;")

(define_expand "restore_stack_block"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  ""
{
  if (!TARGET_SOFT_STACK
      && TARGET_PTX_7_3
      && TARGET_SM52)
    {
      operands[1] = force_reg (Pmode, operands[1]);
      emit_insn (gen_nvptx_stackrestore (Pmode, operands[0], operands[1]));
    }
  /* We don't bother to special-case '-mfake-ptx-alloca' here.  */
  else if (!TARGET_SOFT_STACK)
    ; /* See 'save_stack_block'.  */
  else if (TARGET_SOFT_STACK)
    {
      emit_move_insn (operands[0], operands[1]);
      emit_insn (gen_set_softstack (Pmode, operands[0]));
    }
  else
    gcc_unreachable ();
  DONE;
})

(define_insn "@nvptx_stackrestore_<mode>"
  [(set (match_operand:P 0 "nvptx_register_operand" "=R")
        (unspec:P [(match_operand:P 1 "nvptx_register_operand" "R")]
         UNSPEC_STACKRESTORE))]
  "TARGET_PTX_7_3
   && TARGET_SM52"
  "%.\\tstackrestore%u1\\t%1;")

(define_expand "save_stack_function"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  "!TARGET_SOFT_STACK"
{
  /* See 'STACK_SAVEAREA_MODE'.  */
  gcc_checking_assert (operands[0] == 0);
  DONE;
})

(define_expand "restore_stack_function"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  ""
{
  if (!TARGET_SOFT_STACK)
    /* See 'STACK_SAVEAREA_MODE'.  */
    gcc_checking_assert (operands[1] == 0);
  DONE;
})

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "trap; exit;")

(define_insn "trap_if_true"
  [(trap_if (ne (match_operand:BI 0 "nvptx_register_operand" "R")
		(const_int 0))
	    (const_int 0))]
  ""
  "%j0 trap; %j0 exit;"
  [(set_attr "predicable" "no")])

(define_insn "trap_if_false"
  [(trap_if (eq (match_operand:BI 0 "nvptx_register_operand" "R")
		(const_int 0))
	    (const_int 0))]
  ""
  "%J0 trap; %J0 exit;"
  [(set_attr "predicable" "no")])

(define_expand "ctrap<mode>4"
  [(trap_if (match_operator 0 "nvptx_comparison_operator"
			    [(match_operand:SDIM 1 "nvptx_register_operand")
			     (match_operand:SDIM 2 "nvptx_nonmemory_operand")])
	    (match_operand 3 "const0_operand"))]
  ""
{
  rtx t = nvptx_expand_compare (operands[0]);
  emit_insn (gen_trap_if_true (t));
  DONE;
})

(define_insn "oacc_dim_size"
  [(set (match_operand:SI 0 "nvptx_register_operand" "")
	(unspec:SI [(match_operand:SI 1 "const_int_operand" "")]
		   UNSPEC_DIM_SIZE))]
  ""
{
  static const char *const asms[] =
{ /* Must match oacc_loop_levels ordering.  */
  "%.\\tmov.u32\\t%0, %%nctaid.x;",	/* gang */
  "%.\\tmov.u32\\t%0, %%ntid.y;",	/* worker */
  "%.\\tmov.u32\\t%0, %%ntid.x;",	/* vector */
};
  return asms[INTVAL (operands[1])];
})

(define_insn "oacc_dim_pos"
  [(set (match_operand:SI 0 "nvptx_register_operand" "")
	(unspec_volatile:SI [(match_operand:SI 1 "const_int_operand" "")]
			    UNSPECV_DIM_POS))]
  ""
{
  static const char *const asms[] =
{ /* Must match oacc_loop_levels ordering.  */
  "%.\\tmov.u32\\t%0, %%ctaid.x;",	/* gang */
  "%.\\tmov.u32\\t%0, %%tid.y;",	/* worker */
  "%.\\tmov.u32\\t%0, %%tid.x;",	/* vector */
};
  return asms[INTVAL (operands[1])];
})

(define_insn "nvptx_fork"
  [(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "")]
		       UNSPECV_FORK)]
  ""
  "// fork %0;"
  [(set_attr "predicable" "no")])

(define_insn "nvptx_forked"
  [(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "")]
		       UNSPECV_FORKED)]
  ""
  "// forked %0;"
  [(set_attr "predicable" "no")])

(define_insn "nvptx_joining"
  [(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "")]
		       UNSPECV_JOINING)]
  ""
  "// joining %0;"
  [(set_attr "predicable" "no")])

(define_insn "nvptx_join"
  [(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "")]
		       UNSPECV_JOIN)]
  ""
  "// join %0;"
  [(set_attr "predicable" "no")])

(define_expand "oacc_fork"
  [(set (match_operand:SI 0 "nvptx_nonmemory_operand" "")
        (match_operand:SI 1 "general_operand" ""))
   (unspec_volatile:SI [(match_operand:SI 2 "const_int_operand" "")]
		        UNSPECV_FORKED)]
  ""
{
  if (operands[0] != const0_rtx)
    emit_move_insn (operands[0], operands[1]);
  nvptx_expand_oacc_fork (INTVAL (operands[2]));
  DONE;
})

(define_expand "oacc_join"
  [(set (match_operand:SI 0 "nvptx_nonmemory_operand" "")
        (match_operand:SI 1 "general_operand" ""))
   (unspec_volatile:SI [(match_operand:SI 2 "const_int_operand" "")]
		        UNSPECV_JOIN)]
  ""
{
  if (operands[0] != const0_rtx)
    emit_move_insn (operands[0], operands[1]);
  nvptx_expand_oacc_join (INTVAL (operands[2]));
  DONE;
})

;; only 32-bit shuffles exist.
(define_insn "nvptx_shuffle<mode>"
  [(set (match_operand:BITS 0 "nvptx_register_operand" "=R")
	(unspec:BITS
		[(match_operand:BITS 1 "nvptx_register_operand" "R")
		 (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")
		 (match_operand:SI 3 "const_int_operand" "n")]
		  UNSPEC_SHUFFLE))]
  ""
  {
    if (TARGET_PTX_6_0)
      return "%.\\tshfl.sync%S3.b32\\t%0, %1, %2, 31, 0xffffffff;";
    else
      return "%.\\tshfl%S3.b32\\t%0, %1, %2, 31;";
  })

(define_insn "nvptx_vote_ballot"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(unspec:SI [(match_operand:BI 1 "nvptx_register_operand" "R")]
		   UNSPEC_VOTE_BALLOT))]
  ""
  {
    if (TARGET_PTX_6_0)
      return "%.\\tvote.sync.ballot.b32\\t%0, %1, 0xffffffff;";
    else
      return "%.\\tvote.ballot.b32\\t%0, %1;";
  })

;; Patterns for OpenMP SIMD-via-SIMT lowering

(define_insn "@omp_simt_enter_<mode>"
  [(set (match_operand:P 0 "nvptx_register_operand" "=R")
	(unspec_volatile:P [(match_operand:P 1 "nvptx_nonmemory_operand" "Ri")
			    (match_operand:P 2 "nvptx_nonmemory_operand" "Ri")]
			   UNSPECV_SIMT_ENTER))]
  ""
{
  return nvptx_output_simt_enter (operands[0], operands[1], operands[2]);
})

(define_expand "omp_simt_enter"
  [(match_operand 0 "nvptx_register_operand" "=R")
   (match_operand 1 "nvptx_nonmemory_operand" "Ri")
   (match_operand 2 "const_int_operand" "n")]
  ""
{
  if (!CONST_INT_P (operands[1]))
    cfun->machine->simt_stack_size = HOST_WIDE_INT_M1U;
  else
    cfun->machine->simt_stack_size = MAX (UINTVAL (operands[1]),
					  cfun->machine->simt_stack_size);
  cfun->machine->simt_stack_align = MAX (UINTVAL (operands[2]),
					 cfun->machine->simt_stack_align);
  cfun->machine->has_simtreg = true;
  emit_insn (gen_omp_simt_enter (Pmode, operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "omp_simt_exit"
  [(match_operand 0 "nvptx_register_operand" "R")]
  ""
{
  emit_insn (gen_omp_simt_exit (Pmode, operands[0]));
  if (TARGET_PTX_6_0)
    emit_insn (gen_nvptx_warpsync ());
  else
    emit_insn (gen_nvptx_uniform_warp_check ());
  DONE;
})

(define_insn "@omp_simt_exit_<mode>"
  [(unspec_volatile [(match_operand:P 0 "nvptx_register_operand" "R")]
		    UNSPECV_SIMT_EXIT)]
  ""
{
  return nvptx_output_simt_exit (operands[0]);
})

;; Implement IFN_GOMP_SIMT_LANE: set operand 0 to lane index
(define_insn "omp_simt_lane"
  [(set (match_operand:SI 0 "nvptx_register_operand" "")
	(unspec:SI [(const_int 0)] UNSPEC_LANEID))]
  ""
  "%.\\tmov.u32\\t%0, %%laneid;")

;; Implement IFN_GOMP_SIMT_ORDERED: copy operand 1 to operand 0 and
;; place a compiler barrier to disallow unrolling/peeling the containing loop
(define_expand "omp_simt_ordered"
  [(match_operand:SI 0 "nvptx_register_operand" "=R")
   (match_operand:SI 1 "nvptx_register_operand" "R")]
  ""
{
  emit_move_insn (operands[0], operands[1]);
  emit_insn (gen_nvptx_nounroll ());
  DONE;
})

;; Implement IFN_GOMP_SIMT_XCHG_BFLY: perform a "butterfly" exchange
;; across lanes
(define_expand "omp_simt_xchg_bfly"
  [(match_operand 0 "nvptx_register_or_complex_di_df_register_operand" "=R")
   (match_operand 1 "nvptx_register_or_complex_di_df_register_operand" "R")
   (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")]
  ""
{
  emit_insn (nvptx_gen_shuffle (operands[0], operands[1], operands[2],
				SHUFFLE_BFLY));
  DONE;
})

;; Implement IFN_GOMP_SIMT_XCHG_IDX: broadcast value in operand 1
;; from lane given by index in operand 2 to operand 0 in all lanes
(define_expand "omp_simt_xchg_idx"
  [(match_operand 0 "nvptx_register_or_complex_di_df_register_operand" "=R")
   (match_operand 1 "nvptx_register_or_complex_di_df_register_operand" "R")
   (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")]
  ""
{
  emit_insn (nvptx_gen_shuffle (operands[0], operands[1], operands[2],
				SHUFFLE_IDX));
  DONE;
})

;; Implement IFN_GOMP_SIMT_VOTE_ANY:
;; set operand 0 to zero iff all lanes supply zero in operand 1
(define_expand "omp_simt_vote_any"
  [(match_operand:SI 0 "nvptx_register_operand" "=R")
   (match_operand:SI 1 "nvptx_register_operand" "R")]
  ""
{
  rtx pred = gen_reg_rtx (BImode);
  emit_move_insn (pred, gen_rtx_NE (BImode, operands[1], const0_rtx));
  emit_insn (gen_nvptx_vote_ballot (operands[0], pred));
  DONE;
})

;; Implement IFN_GOMP_SIMT_LAST_LANE:
;; set operand 0 to the lowest lane index that passed non-zero in operand 1
(define_expand "omp_simt_last_lane"
  [(match_operand:SI 0 "nvptx_register_operand" "=R")
   (match_operand:SI 1 "nvptx_register_operand" "R")]
  ""
{
  rtx pred = gen_reg_rtx (BImode);
  rtx tmp = gen_reg_rtx (SImode);
  emit_move_insn (pred, gen_rtx_NE (BImode, operands[1], const0_rtx));
  emit_insn (gen_nvptx_vote_ballot (tmp, pred));
  emit_insn (gen_ctzsi2 (operands[0], tmp));
  DONE;
})

;; extract parts of a 64 bit object into 2 32-bit ints
(define_insn "unpack<mode>si2"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
        (unspec:SI [(match_operand:BITD 2 "nvptx_register_operand" "R")
		    (const_int 0)] UNSPEC_BIT_CONV))
   (set (match_operand:SI 1 "nvptx_register_operand" "=R")
        (unspec:SI [(match_dup 2) (const_int 1)] UNSPEC_BIT_CONV))]
  ""
  "%.\\tmov.b64\\t{%0,%1}, %2;")

;; pack 2 32-bit ints into a 64 bit object
(define_insn "packsi<mode>2"
  [(set (match_operand:BITD 0 "nvptx_register_operand" "=R")
        (unspec:BITD [(match_operand:SI 1 "nvptx_register_operand" "R")
		      (match_operand:SI 2 "nvptx_register_operand" "R")]
		    UNSPEC_BIT_CONV))]
  ""
  "%.\\tmov.b64\\t%0, {%1,%2};")

;; Atomic insns.

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "nvptx_register_operand")	;; bool success output
   (match_operand:SDIM 1 "nvptx_register_operand")	;; oldval output
   (match_operand:SDIM 2 "memory_operand")		;; memory
   (match_operand:SDIM 3 "nvptx_register_operand")	;; expected input
   (match_operand:SDIM 4 "nvptx_register_operand")	;; newval input
   (match_operand:SI 5 "const_int_operand")		;; is_weak
   (match_operand:SI 6 "const_int_operand")		;; success model
   (match_operand:SI 7 "const_int_operand")]		;; failure model
  ""
{
  if (nvptx_mem_local_p (operands[2]))
    emit_insn (gen_atomic_compare_and_swap<mode>_1_local
		(operands[1], operands[2], operands[3], operands[4],
		 operands[6]));
  else
    emit_insn (gen_atomic_compare_and_swap<mode>_1
		(operands[1], operands[2], operands[3], operands[4],
		 operands[6]));

  rtx cond = gen_reg_rtx (BImode);
  emit_move_insn (cond, gen_rtx_EQ (BImode, operands[1], operands[3]));
  emit_insn (gen_sel_truesi (operands[0], cond, GEN_INT (1), GEN_INT (0)));
  DONE;
})

(define_insn "atomic_compare_and_swap<mode>_1_local"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(unspec_volatile:SDIM
	  [(match_operand:SDIM 1 "memory_operand" "+m")
	   (match_operand:SDIM 2 "nvptx_nonmemory_operand" "Ri")
	   (match_operand:SDIM 3 "nvptx_nonmemory_operand" "Ri")
	   (match_operand:SI 4 "const_int_operand")]
	  UNSPECV_CAS_LOCAL))
   (set (match_dup 1)
	(unspec_volatile:SDIM [(const_int 0)] UNSPECV_CAS_LOCAL))]
  ""
  {
	output_asm_insn ("{", NULL);
	output_asm_insn ("\\t"	      ".reg.pred"  "\\t" "%%eq_p;", NULL);
	output_asm_insn ("\\t"	      ".reg%t0"	   "\\t" "%%val;", operands);
	output_asm_insn ("\\t"	      "ld%A1%t0"   "\\t" "%%val,%1;", operands);
	output_asm_insn ("\\t"	      "setp.eq%t0" "\\t" "%%eq_p, %%val, %2;",
			 operands);
	output_asm_insn ("@%%eq_p\\t" "st%A1%t0"   "\\t" "%1,%3;", operands);
	output_asm_insn ("\\t"	      "mov%t0"	   "\\t" "%0,%%val;", operands);
	output_asm_insn ("}", NULL);
	return "";
  }
  [(set_attr "predicable" "no")])

(define_insn "atomic_compare_and_swap<mode>_1"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(unspec_volatile:SDIM
	  [(match_operand:SDIM 1 "memory_operand" "+m")
	   (match_operand:SDIM 2 "nvptx_nonmemory_operand" "Ri")
	   (match_operand:SDIM 3 "nvptx_nonmemory_operand" "Ri")
	   (match_operand:SI 4 "const_int_operand")]
	  UNSPECV_CAS))
   (set (match_dup 1)
	(unspec_volatile:SDIM [(const_int 0)] UNSPECV_CAS))]
  ""
  {
    const char *t
      = "%.\\tatom%A1.cas.b%T0\\t%x0, %1, %2, %3;";
    return nvptx_output_atomic_insn (t, operands, 1, 4);
  }
  [(set_attr "atomic" "true")])

(define_insn "atomic_exchange<mode>"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")	;; output
	(unspec_volatile:SDIM
	  [(match_operand:SDIM 1 "memory_operand" "+m")		;; memory
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_XCHG))
   (set (match_dup 1)
	(match_operand:SDIM 2 "nvptx_nonmemory_operand" "Ri"))]	;; input
  ""
  {
    if (nvptx_mem_local_p (operands[1]))
      {
	output_asm_insn ("{", NULL);
	output_asm_insn ("\\t"	 ".reg%t0"  "\\t" "%%val;", operands);
	output_asm_insn ("%.\\t" "ld%A1%t0" "\\t" "%%val,%1;", operands);
	output_asm_insn ("%.\\t" "st%A1%t0" "\\t" "%1,%2;", operands);
	output_asm_insn ("%.\\t" "mov%t0"   "\\t" "%0,%%val;", operands);
	output_asm_insn ("}", NULL);
	return "";
      }
    const char *t
      = "%.\tatom%A1.exch.b%T0\t%x0, %1, %2;";
    return nvptx_output_atomic_insn (t, operands, 1, 3);
  }
  [(set_attr "atomic" "true")])

(define_expand "atomic_store<mode>"
  [(match_operand:SDIM 0 "memory_operand" "=m")		  ;; memory
   (match_operand:SDIM 1 "nvptx_nonmemory_operand" "Ri")  ;; input
   (match_operand:SI 2 "const_int_operand")]		  ;; model
  ""
{
  struct address_info info;
  decompose_mem_address (&info, operands[0]);
  if (info.base != NULL && REG_P (*info.base)
      && REGNO_PTR_FRAME_P (REGNO (*info.base)))
    {
      emit_insn (gen_mov<mode> (operands[0], operands[1]));
      DONE;
    }

  if (TARGET_SM70)
    {
       emit_insn (gen_nvptx_atomic_store_sm70<mode> (operands[0], operands[1],
						     operands[2]));
       DONE;
    }

  bool maybe_shared_p = nvptx_mem_maybe_shared_p (operands[0]);
  if (!maybe_shared_p)
    /* Fall back to expand_atomic_store.  */
    FAIL;

  emit_insn (gen_nvptx_atomic_store<mode> (operands[0], operands[1],
					   operands[2]));
  DONE;
})

(define_insn "nvptx_atomic_store_sm70<mode>"
  [(set (match_operand:SDIM 0 "memory_operand" "+m")	      ;; memory
       (unspec_volatile:SDIM
	 [(match_operand:SDIM 1 "nvptx_nonmemory_operand" "Ri") ;; input
	  (match_operand:SI 2 "const_int_operand")]		;; model
	       UNSPECV_ST))]
  "TARGET_SM70"
  {
    const char *t
      = "%.\tst%A0.b%T0\t%0, %1;";
    return nvptx_output_atomic_insn (t, operands, 0, 2);
  }
  [(set_attr "atomic" "false")]) ;; Note: st is not an atomic insn.

(define_insn "nvptx_atomic_store<mode>"
  [(set (match_operand:SDIM 0 "memory_operand" "+m")	      ;; memory
       (unspec_volatile:SDIM
	 [(match_operand:SDIM 1 "nvptx_nonmemory_operand" "Ri") ;; input
	  (match_operand:SI 2 "const_int_operand")]		;; model
	       UNSPECV_ST))]
  "!TARGET_SM70"
  {
    const char *t
      = "%.\tatom%A0.exch.b%T0\t_, %0, %1;";
    return nvptx_output_atomic_insn (t, operands, 0, 2);
  }
  [(set_attr "atomic" "true")])

(define_insn "atomic_fetch_add<mode>"
  [(set (match_operand:SDIM 1 "memory_operand" "+m")
	(unspec_volatile:SDIM
	  [(plus:SDIM (match_dup 1)
		      (match_operand:SDIM 2 "nvptx_nonmemory_operand" "Ri"))
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_LOCK))
   (set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(match_dup 1))]
  ""
  {
    if (nvptx_mem_local_p (operands[1]))
      {
	output_asm_insn ("{", NULL);
	output_asm_insn ("\\t"	 ".reg%t0"  "\\t" "%%val;", operands);
	output_asm_insn ("\\t"	 ".reg%t0"  "\\t" "%%update;", operands);
	output_asm_insn ("%.\\t" "ld%A1%t0" "\\t" "%%val,%1;", operands);
	output_asm_insn ("%.\\t" "add%t0"   "\\t" "%%update,%%val,%2;",
			 operands);
	output_asm_insn ("%.\\t" "st%A1%t0" "\\t" "%1,%%update;", operands);
	output_asm_insn ("%.\\t" "mov%t0"   "\\t" "%0,%%val;", operands);
	output_asm_insn ("}", NULL);
	return "";
      }
    const char *t
      = "%.\\tatom%A1.add%t0\\t%x0, %1, %2;";
    return nvptx_output_atomic_insn (t, operands, 1, 3);
  }
  [(set_attr "atomic" "true")])

(define_insn "atomic_fetch_addsf"
  [(set (match_operand:SF 1 "memory_operand" "+m")
	(unspec_volatile:SF
	 [(plus:SF (match_dup 1)
		   (match_operand:SF 2 "nvptx_nonmemory_operand" "RF"))
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_LOCK))
   (set (match_operand:SF 0 "nvptx_register_operand" "=R")
	(match_dup 1))]
  ""
  {
    if (nvptx_mem_local_p (operands[1]))
      {
	output_asm_insn ("{", NULL);
	output_asm_insn ("\\t"	 ".reg%t0"  "\\t" "%%val;", operands);
	output_asm_insn ("\\t"	 ".reg%t0"  "\\t" "%%update;", operands);
	output_asm_insn ("%.\\t" "ld%A1%t0" "\\t" "%%val,%1;", operands);
	output_asm_insn ("%.\\t" "add%t0"   "\\t" "%%update,%%val,%2;",
			 operands);
	output_asm_insn ("%.\\t" "st%A1%t0" "\\t" "%1,%%update;", operands);
	output_asm_insn ("%.\\t" "mov%t0"   "\\t" "%0,%%val;", operands);
	output_asm_insn ("}", NULL);
	return "";
      }
    const char *t
      = "%.\\tatom%A1.add%t0\\t%x0, %1, %2;";
    return nvptx_output_atomic_insn (t, operands, 1, 3);
  }
  [(set_attr "atomic" "true")])

(define_insn "atomic_fetch_<logic><mode>"
  [(set (match_operand:SDIM 1 "memory_operand" "+m")
	(unspec_volatile:SDIM
	  [(any_logic:SDIM (match_dup 1)
			   (match_operand:SDIM 2 "nvptx_nonmemory_operand" "Ri"))
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_LOCK))
   (set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(match_dup 1))]
  "<MODE>mode == SImode || TARGET_SM35"
  {
    if (nvptx_mem_local_p (operands[1]))
      {
	output_asm_insn ("{", NULL);
	output_asm_insn ("\\t"	 ".reg.b%T0"    "\\t" "%%val;", operands);
	output_asm_insn ("\\t"	 ".reg.b%T0"    "\\t" "%%update;", operands);
	output_asm_insn ("%.\\t" "ld%A1%t0"     "\\t" "%%val,%1;", operands);
	output_asm_insn ("%.\\t" "<logic>.b%T0" "\\t" "%%update,%%val,%2;",
			 operands);
	output_asm_insn ("%.\\t" "st%A1%t0"     "\\t" "%1,%%update;", operands);
	output_asm_insn ("%.\\t" "mov%t0"       "\\t" "%0,%%val;", operands);
	output_asm_insn ("}", NULL);
	return "";
      }
    const char *t
      = "%.\\tatom%A1.<logic>.b%T0\\t%x0, %1, %2;";
    return nvptx_output_atomic_insn (t, operands, 1, 3);
  }

  [(set_attr "atomic" "true")])

(define_expand "atomic_test_and_set"
  [(match_operand:SI 0 "nvptx_register_operand")	;; bool success output
   (match_operand:QI 1 "memory_operand")		;; memory
   (match_operand:SI 2 "const_int_operand")]		;; model
  ""
{
  rtx libfunc;
  rtx addr;
  libfunc = init_one_libfunc ("__atomic_test_and_set_1");
  addr = convert_memory_address (ptr_mode, XEXP (operands[1], 0));
  emit_library_call_value (libfunc, operands[0], LCT_NORMAL, SImode,
			  addr, ptr_mode,
			  operands[2], SImode);
  DONE;
})

(define_insn "nvptx_barsync"
  [(unspec_volatile [(match_operand:SI 0 "nvptx_nonmemory_operand" "Ri")
		     (match_operand:SI 1 "const_int_operand")]
		    UNSPECV_BARSYNC)]
  ""
  {
    if (INTVAL (operands[1]) == 0)
      return (TARGET_PTX_6_0
	      ? "\\tbarrier.sync.aligned\\t%0;"
	      : "\\tbar.sync\\t%0;");
    else
      return (TARGET_PTX_6_0
	      ? "\\tbarrier.sync\\t%0, %1;"
	      : "\\tbar.sync\\t%0, %1;");
  }
  [(set_attr "predicable" "no")])

(define_insn "nvptx_warpsync"
  [(unspec_volatile [(const_int 0)] UNSPECV_WARPSYNC)]
  "TARGET_PTX_6_0"
  "%.\\tbar.warp.sync\\t0xffffffff;")

(define_int_iterator BARRED
  [UNSPECV_BARRED_AND
   UNSPECV_BARRED_OR
   UNSPECV_BARRED_POPC])
(define_int_attr barred_op
  [(UNSPECV_BARRED_AND      "and")
   (UNSPECV_BARRED_OR       "or")
   (UNSPECV_BARRED_POPC     "popc")])
(define_int_attr barred_mode
  [(UNSPECV_BARRED_AND      "BI")
   (UNSPECV_BARRED_OR       "BI")
   (UNSPECV_BARRED_POPC     "SI")])
(define_int_attr barred_ptxtype
  [(UNSPECV_BARRED_AND      "pred")
   (UNSPECV_BARRED_OR       "pred")
   (UNSPECV_BARRED_POPC     "u32")])

(define_insn "nvptx_barred_<barred_op>"
  [(set (match_operand:<barred_mode> 0 "nvptx_register_operand" "=R")
        (unspec_volatile
	  [(match_operand:SI 1 "nvptx_nonmemory_operand" "Ri")
           (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")
	   (match_operand:SI 3 "const_int_operand" "i")
           (match_operand:BI 4 "nvptx_register_operand" "R")]
          BARRED))]
  ""
  "\\tbar.red.<barred_op>.<barred_ptxtype> \\t%0, %1, %2, %p3%4;";"
  [(set_attr "predicable" "no")])

(define_insn "nvptx_uniform_warp_check"
  [(unspec_volatile [(const_int 0)] UNSPECV_UNIFORM_WARP_CHECK)]
  ""
  {
    const char *insns[] = {
      "{",
      "\\t"		".reg.pred"	"\\t" "%%r_sync;",
      "\\t"		"mov.pred"	"\\t" "%%r_sync, 1;",
      "%.\\t"		"vote.all.pred" "\\t" "%%r_sync, 1;",
      "@!%%r_sync\\t"	"trap;",
      "@!%%r_sync\\t"	"exit;",
      "}",
      NULL
    };
    for (const char **p = &insns[0]; *p != NULL; p++)
      output_asm_insn (*p, NULL);
    return "";
  })

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMBAR))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

;; Ptx defines the memory barriers membar.cta, membar.gl and membar.sys
;; (corresponding to cuda functions threadfence_block, threadfence and
;; threadfence_system).  For the insn memory_barrier we use membar.sys.  This
;; may be overconservative, but before using membar.gl instead we'll need to
;; explain in detail why it's safe to use.  For now, use membar.sys.
(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMBAR))]
  ""
  "\\tmembar.sys;"
  [(set_attr "predicable" "no")])

(define_expand "nvptx_membar_cta"
  [(set (match_dup 0)
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMBAR_CTA))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*nvptx_membar_cta"
  [(set (match_operand:BLK 0 "" "")
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMBAR_CTA))]
  ""
  "\\tmembar.cta;"
  [(set_attr "predicable" "no")])

(define_expand "nvptx_membar_gl"
  [(set (match_dup 0)
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMBAR_GL))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*nvptx_membar_gl"
  [(set (match_operand:BLK 0 "" "")
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_MEMBAR_GL))]
  ""
  "\\tmembar.gl;"
  [(set_attr "predicable" "no")])

(define_insn "nvptx_nounroll"
  [(unspec_volatile [(const_int 0)] UNSPECV_NOUNROLL)]
  ""
  "\\t.pragma \\\"nounroll\\\";"
  [(set_attr "predicable" "no")])

(define_insn "nvptx_red_partition"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=R")
	(unspec_volatile:DI [(match_operand:DI 1 "const_int_operand")]
	 UNSPECV_RED_PART))]
  ""
  {
    return nvptx_output_red_partition (operands[0], operands[1]);
  }
  [(set_attr "predicable" "no")])

;; Expand QI mode operations using SI mode instructions.
(define_code_iterator any_sbinary [plus minus smin smax])
(define_code_attr sbinary [(plus "add") (minus "sub") (smin "smin") (smax "smax")])

(define_code_iterator any_ubinary [and ior xor umin umax])
(define_code_attr ubinary [(and "and") (ior "ior") (xor "xor") (umin "umin")
			   (umax "umax")])

(define_code_iterator any_sunary [neg abs])
(define_code_attr sunary [(neg "neg") (abs "abs")])

(define_code_iterator any_uunary [not])
(define_code_attr uunary [(not "one_cmpl")])

(define_expand "<sbinary>qi3"
  [(set (match_operand:QI 0 "nvptx_register_operand")
	(any_sbinary:QI (match_operand:QI 1 "nvptx_nonmemory_operand")
			(match_operand:QI 2 "nvptx_nonmemory_operand")))]
  ""
{
  rtx reg = gen_reg_rtx (SImode);
  rtx op0 = convert_modes (SImode, QImode, operands[1], 0);
  rtx op1 = convert_modes (SImode, QImode, operands[2], 0);
  if (<CODE> == MINUS)
    op0 = force_reg (SImode, op0);
  emit_insn (gen_<sbinary>si3 (reg, op0, op1));
  emit_insn (gen_truncsiqi2 (operands[0], reg));
  DONE;
})

(define_expand "<ubinary>qi3"
  [(set (match_operand:QI 0 "nvptx_register_operand")
	(any_ubinary:QI (match_operand:QI 1 "nvptx_nonmemory_operand")
			(match_operand:QI 2 "nvptx_nonmemory_operand")))]
  ""
{
  rtx reg = gen_reg_rtx (SImode);
  rtx op0 = convert_modes (SImode, QImode, operands[1], 1);
  rtx op1 = convert_modes (SImode, QImode, operands[2], 1);
  emit_insn (gen_<ubinary>si3 (reg, op0, op1));
  emit_insn (gen_truncsiqi2 (operands[0], reg));
  DONE;
})

(define_expand "<sunary>qi2"
  [(set (match_operand:QI 0 "nvptx_register_operand")
	(any_sunary:QI (match_operand:QI 1 "nvptx_nonmemory_operand")))]
  ""
{
  rtx reg = gen_reg_rtx (SImode);
  rtx op0 = convert_modes (SImode, QImode, operands[1], 0);
  emit_insn (gen_<sunary>si2 (reg, op0));
  emit_insn (gen_truncsiqi2 (operands[0], reg));
  DONE;
})

(define_expand "<uunary>qi2"
  [(set (match_operand:QI 0 "nvptx_register_operand")
	(any_uunary:QI (match_operand:QI 1 "nvptx_nonmemory_operand")))]
  ""
{
  rtx reg = gen_reg_rtx (SImode);
  rtx op0 = convert_modes (SImode, QImode, operands[1], 1);
  emit_insn (gen_<uunary>si2 (reg, op0));
  emit_insn (gen_truncsiqi2 (operands[0], reg));
  DONE;
})

(define_expand "cstoreqi4"
  [(set (match_operand:SI 0 "nvptx_register_operand")
	(match_operator:SI 1 "nvptx_comparison_operator"
	  [(match_operand:QI 2 "nvptx_nonmemory_operand")
	   (match_operand:QI 3 "nvptx_nonmemory_operand")]))]
  ""
{
  rtx reg = gen_reg_rtx (BImode);
  enum rtx_code code = GET_CODE (operands[1]);
  int unsignedp = unsigned_condition_p (code);
  rtx op2 = convert_modes (SImode, QImode, operands[2], unsignedp);
  rtx op3 = convert_modes (SImode, QImode, operands[3], unsignedp);
  rtx cmp = gen_rtx_fmt_ee (code, SImode, op2, op3);
  emit_insn (gen_cmpsi (reg, cmp, op2, op3));
  emit_insn (gen_setccsi_from_bi (operands[0], reg));
  DONE;
})

(define_insn "*ext_truncsi2_qi"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(sign_extend:SI
	 (truncate:QI (match_operand:SI 1 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tcvt.s32.s8\\t%0, %1;")

(define_insn "*zext_truncsi2_qi"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(zero_extend:SI
	 (truncate:QI (match_operand:SI 1 "nvptx_register_operand" "R"))))]
  ""
  "%.\\tcvt.u32.u8\\t%0, %1;")
