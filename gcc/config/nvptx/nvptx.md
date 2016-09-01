;; Machine description for NVPTX.
;; Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

   UNSPEC_FPINT_FLOOR
   UNSPEC_FPINT_BTRUNC
   UNSPEC_FPINT_CEIL
   UNSPEC_FPINT_NEARBYINT

   UNSPEC_BITREV

   UNSPEC_ALLOCA

   UNSPEC_DIM_SIZE

   UNSPEC_BIT_CONV

   UNSPEC_SHUFFLE
   UNSPEC_BR_UNIFIED
])

(define_c_enum "unspecv" [
   UNSPECV_LOCK
   UNSPECV_CAS
   UNSPECV_XCHG
   UNSPECV_BARSYNC
   UNSPECV_DIM_POS

   UNSPECV_FORK
   UNSPECV_FORKED
   UNSPECV_JOINING
   UNSPECV_JOIN
])

(define_attr "subregs_ok" "false,true"
  (const_string "false"))

;; The nvptx operand predicates, in general, don't permit subregs and
;; only literal constants, which differ from the generic ones, which
;; permit subregs and symbolc constants (as appropriate)
(define_predicate "nvptx_register_operand"
  (match_code "reg")
{
  return register_operand (op, mode);
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
(define_mode_iterator SDCM [SC DC])
(define_mode_iterator BITS [SI SF])
(define_mode_iterator BITD [DI DF])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; We should get away with not defining memory alternatives, since we don't
;; get variables in this mode and pseudos are never spilled.
(define_insn "movbi"
  [(set (match_operand:BI 0 "nvptx_register_operand" "=R,R,R")
	(match_operand:BI 1 "nvptx_nonmemory_operand" "R,P0,Pn"))]
  ""
  "@
   %.\\tmov%t0\\t%0, %1;
   %.\\tsetp.eq.u32\\t%0, 1, 0;
   %.\\tsetp.eq.u32\\t%0, 1, 1;")

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
  "@
   %.\\tcvt%t0.u32\\t%0, %1;
   %.\\tst%A0.u%T0\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

(define_insn "truncdi<mode>2"
  [(set (match_operand:QHSIM 0 "nvptx_nonimmediate_operand" "=R,m")
	(truncate:QHSIM (match_operand:DI 1 "nvptx_register_operand" "R,R")))]
  ""
  "@
   %.\\tcvt%t0.u64\\t%0, %1;
   %.\\tst%A0.u%T0\\t%0, %1;"
  [(set_attr "subregs_ok" "true")])

;; Integer arithmetic

(define_insn "add<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(plus:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:HSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tadd%t0\\t%0, %1, %2;")

(define_insn "sub<mode>3"
  [(set (match_operand:HSDIM 0 "nvptx_register_operand" "=R")
	(minus:HSDIM (match_operand:HSDIM 1 "nvptx_register_operand" "R")
		     (match_operand:HSDIM 2 "nvptx_register_operand" "R")))]
  ""
  "%.\\tsub%t0\\t%0, %1, %2;")

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

(define_insn "bitrev<mode>2"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(unspec:SDIM [(match_operand:SDIM 1 "nvptx_register_operand" "R")]
		     UNSPEC_BITREV))]
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

;; Shifts

(define_insn "ashl<mode>3"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(ashift:SDIM (match_operand:SDIM 1 "nvptx_register_operand" "R")
		     (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tshl.b%T0\\t%0, %1, %2;")

(define_insn "ashr<mode>3"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(ashiftrt:SDIM (match_operand:SDIM 1 "nvptx_register_operand" "R")
		       (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tshr.s%T0\\t%0, %1, %2;")

(define_insn "lshr<mode>3"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(lshiftrt:SDIM (match_operand:SDIM 1 "nvptx_register_operand" "R")
		       (match_operand:SI 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tshr.u%T0\\t%0, %1, %2;")

;; Logical operations

(define_insn "and<mode>3"
  [(set (match_operand:BHSDIM 0 "nvptx_register_operand" "=R")
	(and:BHSDIM (match_operand:BHSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:BHSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tand.b%T0\\t%0, %1, %2;")

(define_insn "ior<mode>3"
  [(set (match_operand:BHSDIM 0 "nvptx_register_operand" "=R")
	(ior:BHSDIM (match_operand:BHSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:BHSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\tor.b%T0\\t%0, %1, %2;")

(define_insn "xor<mode>3"
  [(set (match_operand:BHSDIM 0 "nvptx_register_operand" "=R")
	(xor:BHSDIM (match_operand:BHSDIM 1 "nvptx_register_operand" "R")
		    (match_operand:BHSDIM 2 "nvptx_nonmemory_operand" "Ri")))]
  ""
  "%.\\txor.b%T0\\t%0, %1, %2;")

;; Comparisons and branches

(define_insn "*cmp<mode>"
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
  "%j0\\tbra\\t%l1;")

(define_insn "br_false"
  [(set (pc)
	(if_then_else (eq (match_operand:BI 0 "nvptx_register_operand" "R")
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "%J0\\tbra\\t%l1;")

;; unified conditional branch
(define_insn "br_true_uni"
  [(set (pc) (if_then_else
	(ne (unspec:BI [(match_operand:BI 0 "nvptx_register_operand" "R")]
		       UNSPEC_BR_UNIFIED) (const_int 0))
        (label_ref (match_operand 1 "" "")) (pc)))]
  ""
  "%j0\\tbra.uni\\t%l1;")

(define_insn "br_false_uni"
  [(set (pc) (if_then_else
	(eq (unspec:BI [(match_operand:BI 0 "nvptx_register_operand" "R")]
		       UNSPEC_BR_UNIFIED) (const_int 0))
        (label_ref (match_operand 1 "" "")) (pc)))]
  ""
  "%J0\\tbra.uni\\t%l1;")

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

(define_insn "setcc_from_bi"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(ne:SI (match_operand:BI 1 "nvptx_register_operand" "R")
	       (const_int 0)))]
  ""
  "%.\\tselp%t0 %0,-1,0,%1;")

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

(define_insn "setcc_int<mode>"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(match_operator:SI 1 "nvptx_comparison_operator"
	  [(match_operand:HSDIM 2 "nvptx_register_operand" "R")
	   (match_operand:HSDIM 3 "nvptx_nonmemory_operand" "Ri")]))]
  ""
  "%.\\tset%t0%c1\\t%0, %2, %3;")

(define_insn "setcc_int<mode>"
  [(set (match_operand:SI 0 "nvptx_register_operand" "=R")
	(match_operator:SI 1 "nvptx_float_comparison_operator"
	   [(match_operand:SDFM 2 "nvptx_register_operand" "R")
	    (match_operand:SDFM 3 "nvptx_nonmemory_operand" "RF")]))]
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

(define_expand "cstorebi4"
  [(set (match_operand:SI 0 "nvptx_register_operand")
	(match_operator:SI 1 "ne_operator"
         [(match_operand:BI 2 "nvptx_register_operand")
          (match_operand:BI 3 "const0_operand")]))]
  ""
  "")

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "nvptx_register_operand")
	(match_operator:SI 1 "nvptx_comparison_operator"
         [(match_operand:HSDIM 2 "nvptx_register_operand")
          (match_operand:HSDIM 3 "nvptx_nonmemory_operand")]))]
  ""
  "")

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "nvptx_register_operand")
	(match_operator:SI 1 "nvptx_float_comparison_operator"
         [(match_operand:SDFM 2 "nvptx_register_operand")
          (match_operand:SDFM 3 "nvptx_nonmemory_operand")]))]
  ""
  "")

;; Calls

(define_insn "call_insn"
  [(match_parallel 2 "call_operation"
    [(call (mem:QI (match_operand 0 "call_insn_operand" "Rs"))
	   (match_operand 1))])]
  ""
{
  return nvptx_output_call_insn (insn, NULL_RTX, operands[0]);
})

(define_insn "call_value_insn"
  [(match_parallel 3 "call_operation"
    [(set (match_operand 0 "nvptx_register_operand" "=R")
	  (call (mem:QI (match_operand 1 "call_insn_operand" "Rs"))
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

(define_insn "div<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(div:SDFM (match_operand:SDFM 1 "nvptx_register_operand" "R")
		  (match_operand:SDFM 2 "nvptx_nonmemory_operand" "RF")))]
  ""
  "%.\\tdiv%#%t0\\t%0, %1, %2;")

(define_insn "copysign<mode>3"
  [(set (match_operand:SDFM 0 "nvptx_register_operand" "=R")
	(unspec:SDFM [(match_operand:SDFM 1 "nvptx_register_operand" "R")
		      (match_operand:SDFM 2 "nvptx_register_operand" "R")]
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

;; Miscellaneous

(define_insn "nop"
  [(const_int 0)]
  ""
  "")

(define_insn "return"
  [(return)]
  ""
{
  return nvptx_output_return ();
})

(define_expand "epilogue"
  [(clobber (const_int 0))]
  ""
{
  emit_jump_insn (gen_return ());
  DONE;
})

(define_expand "nonlocal_goto"
  [(match_operand 0 "" "")
   (match_operand 1 "" "")
   (match_operand 2 "" "")
   (match_operand 3 "" "")]
  ""
{
  sorry ("target cannot support nonlocal goto.");
  emit_insn (gen_nop ());
  DONE;
})

(define_expand "nonlocal_goto_receiver"
  [(const_int 0)]
  ""
{
  sorry ("target cannot support nonlocal goto.");
})

(define_expand "allocate_stack"
  [(match_operand 0 "nvptx_register_operand")
   (match_operand 1 "nvptx_register_operand")]
  ""
{
  /* The ptx documentation specifies an alloca intrinsic (for 32 bit
     only)  but notes it is not implemented.  The assembler emits a
     confused error message.  Issue a blunt one now instead.  */
  sorry ("target cannot support alloca.");
  emit_insn (gen_nop ());
  DONE;
  if (TARGET_ABI64)
    emit_insn (gen_allocate_stack_di (operands[0], operands[1]));
  else
    emit_insn (gen_allocate_stack_si (operands[0], operands[1]));
  DONE;
})

(define_insn "allocate_stack_<mode>"
  [(set (match_operand:P 0 "nvptx_register_operand" "=R")
        (unspec:P [(match_operand:P 1 "nvptx_register_operand" "R")]
                   UNSPEC_ALLOCA))]
  ""
  "%.\\tcall (%0), %%alloca, (%1);")

(define_expand "restore_stack_block"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  ""
{
  DONE;
})

(define_expand "restore_stack_function"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  ""
{
  DONE;
})

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "trap;")

(define_insn "trap_if_true"
  [(trap_if (ne (match_operand:BI 0 "nvptx_register_operand" "R")
		(const_int 0))
	    (const_int 0))]
  ""
  "%j0 trap;")

(define_insn "trap_if_false"
  [(trap_if (eq (match_operand:BI 0 "nvptx_register_operand" "R")
		(const_int 0))
	    (const_int 0))]
  ""
  "%J0 trap;")

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
)

(define_insn "nvptx_forked"
  [(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "")]
		       UNSPECV_FORKED)]
  ""
  "// forked %0;"
)

(define_insn "nvptx_joining"
  [(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "")]
		       UNSPECV_JOINING)]
  ""
  "// joining %0;"
)

(define_insn "nvptx_join"
  [(unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "")]
		       UNSPECV_JOIN)]
  ""
  "// join %0;"
)

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
  "%.\\tshfl%S3.b32\\t%0, %1, %2, 31;")

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
  emit_insn (gen_atomic_compare_and_swap<mode>_1
    (operands[1], operands[2], operands[3], operands[4], operands[6]));

  rtx cond = gen_reg_rtx (BImode);
  emit_move_insn (cond, gen_rtx_EQ (BImode, operands[1], operands[3]));
  emit_insn (gen_sel_truesi (operands[0], cond, GEN_INT (1), GEN_INT (0)));
  DONE;
})

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
  "%.\\tatom%A1.cas.b%T0\\t%0, %1, %2, %3;")

(define_insn "atomic_exchange<mode>"
  [(set (match_operand:SDIM 0 "nvptx_register_operand" "=R")	;; output
	(unspec_volatile:SDIM
	  [(match_operand:SDIM 1 "memory_operand" "+m")		;; memory
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_XCHG))
   (set (match_dup 1)
	(match_operand:SDIM 2 "nvptx_nonmemory_operand" "Ri"))]	;; input
  ""
  "%.\\tatom%A1.exch.b%T0\\t%0, %1, %2;")

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
  "%.\\tatom%A1.add%t0\\t%0, %1, %2;")

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
  "%.\\tatom%A1.add%t0\\t%0, %1, %2;")

(define_code_iterator any_logic [and ior xor])
(define_code_attr logic [(and "and") (ior "or") (xor "xor")])

;; Currently disabled until we add better subtarget support - requires sm_32.
(define_insn "atomic_fetch_<logic><mode>"
  [(set (match_operand:SDIM 1 "memory_operand" "+m")
	(unspec_volatile:SDIM
	  [(any_logic:SDIM (match_dup 1)
			   (match_operand:SDIM 2 "nvptx_nonmemory_operand" "Ri"))
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_LOCK))
   (set (match_operand:SDIM 0 "nvptx_register_operand" "=R")
	(match_dup 1))]
  "0"
  "%.\\tatom%A1.b%T0.<logic>\\t%0, %1, %2;")

(define_insn "nvptx_barsync"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "")]
		    UNSPECV_BARSYNC)]
  ""
  "\\tbar.sync\\t%0;")
