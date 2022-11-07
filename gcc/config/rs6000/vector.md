;; Expander definitions for vector support between altivec & vsx.  No
;; instructions are in this file, this file provides the generic vector
;; expander, and the actual vector instructions will be in altivec.md and
;; vsx.md

;; Copyright (C) 2009-2022 Free Software Foundation, Inc.
;; Contributed by Michael Meissner <meissner@linux.vnet.ibm.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Vector int modes
(define_mode_iterator VEC_I [V16QI V8HI V4SI V2DI])

;; Vector int modes for comparison, shift and rotation
(define_mode_iterator VEC_IC [V16QI V8HI V4SI V2DI (V1TI "TARGET_POWER10")])

;; 128-bit int modes
(define_mode_iterator VEC_TI [V1TI TI])

;; Vector int modes for parity
(define_mode_iterator VEC_IP [V8HI
			      V4SI
			      V2DI
			      V1TI
			      TI])

;; Vector float modes
(define_mode_iterator VEC_F [V4SF V2DF])

;; Vector arithmetic modes
(define_mode_iterator VEC_A [V16QI V8HI V4SI V2DI V4SF V2DF])

;; Vector modes that need alginment via permutes
(define_mode_iterator VEC_K [V16QI V8HI V4SI V4SF])

;; Vector logical modes
(define_mode_iterator VEC_L [V16QI V8HI V4SI V2DI V4SF V2DF V1TI TI KF TF])

;; Vector modes for moves.  Don't do TImode or TFmode here, since their
;; moves are handled elsewhere.
(define_mode_iterator VEC_M [V16QI V8HI V4SI V2DI V4SF V2DF V1TI KF])

;; Vector modes for types that don't need a realignment under VSX
(define_mode_iterator VEC_N [V4SI V4SF V2DI V2DF V1TI KF TF])

;; Vector comparison modes
(define_mode_iterator VEC_C [V16QI V8HI V4SI V2DI V4SF V2DF V1TI])

;; Vector init/extract modes
(define_mode_iterator VEC_E [V16QI V8HI V4SI V2DI V4SF V2DF])

;; Vector modes for 64-bit base types
(define_mode_iterator VEC_64 [V2DI V2DF])

;; Vector integer modes
(define_mode_iterator VI [V4SI V8HI V16QI])

;; Base type from vector mode
(define_mode_attr VEC_base [(V16QI "QI")
			    (V8HI  "HI")
			    (V4SI  "SI")
			    (V2DI  "DI")
			    (V4SF  "SF")
			    (V2DF  "DF")
			    (V1TI  "TI")
			    (TI    "TI")])

;; As above, but in lower case
(define_mode_attr VEC_base_l [(V16QI "qi")
			      (V8HI  "hi")
			      (V4SI  "si")
			      (V2DI  "di")
			      (V4SF  "sf")
			      (V2DF  "df")
			      (V1TI  "ti")
			      (TI    "ti")])

;; Same size integer type for floating point data
(define_mode_attr VEC_int [(V4SF  "v4si")
			   (V2DF  "v2di")])

(define_mode_attr VEC_INT [(V4SF  "V4SI")
			   (V2DF  "V2DI")])

;; constants for unspec
(define_c_enum "unspec" [UNSPEC_PREDICATE
			 UNSPEC_REDUC
			 UNSPEC_NEZ_P])

;; Vector reduction code iterators
(define_code_iterator VEC_reduc [plus smin smax])

(define_code_attr VEC_reduc_name [(plus "plus")
				  (smin "smin")
				  (smax "smax")])

(define_code_attr VEC_reduc_rtx [(plus "add")
				 (smin "smin")
				 (smax "smax")])

;; code iterators and attributes for vector FP comparison operators:
(define_code_iterator
  vector_fp_comparison_simple [lt le ne ungt unge unlt unle])
(define_code_iterator
  vector_fp_comparison_complex [ltgt uneq unordered ordered])


;; Vector move instructions.  Little-endian VSX loads and stores require
;; special handling to circumvent "element endianness."
(define_expand "mov<mode>"
  [(set (match_operand:VEC_M 0 "nonimmediate_operand")
	(match_operand:VEC_M 1 "any_operand"))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (can_create_pseudo_p ())
    {
      if (CONSTANT_P (operands[1]))
	{
	  if (FLOAT128_VECTOR_P (<MODE>mode))
	    {
	      if (!easy_fp_constant (operands[1], <MODE>mode))
		operands[1] = force_const_mem (<MODE>mode, operands[1]);
	    }
	  else if (!easy_vector_constant (operands[1], <MODE>mode))
	    operands[1] = force_const_mem (<MODE>mode, operands[1]);
	}

      if (!vlogical_operand (operands[0], <MODE>mode)
	  && !vlogical_operand (operands[1], <MODE>mode))
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
  /* When generating load/store instructions to/from VSX registers on
     pre-power9 hardware in little endian mode, we need to emit register
     permute instructions to byte swap the contents, since the VSX load/store
     instructions do not include a byte swap as part of their operation.
     Altivec loads and stores have no such problem, so we skip them below.  */
  if (!BYTES_BIG_ENDIAN
      && VECTOR_MEM_VSX_P (<MODE>mode)
      && !TARGET_P9_VECTOR
      && !gpr_or_gpr_p (operands[0], operands[1])
      && ((memory_operand (operands[0], <MODE>mode)
	   && !altivec_indexed_or_indirect_operand(operands[0], <MODE>mode))
	  ^ (memory_operand (operands[1], <MODE>mode)
	     && !altivec_indexed_or_indirect_operand(operands[1], <MODE>mode))))
    {
      rs6000_emit_le_vsx_move (operands[0], operands[1], <MODE>mode);
      DONE;
    }
})

;; Generic vector floating point load/store instructions.  These will match
;; insns defined in vsx.md or altivec.md depending on the switches.
(define_expand "vector_load_<mode>"
  [(set (match_operand:VEC_M 0 "vfloat_operand")
	(match_operand:VEC_M 1 "memory_operand"))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_store_<mode>"
  [(set (match_operand:VEC_M 0 "memory_operand")
	(match_operand:VEC_M 1 "vfloat_operand"))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; Splits if a GPR register was chosen for the move
(define_split
  [(set (match_operand:VEC_L 0 "nonimmediate_operand")
        (match_operand:VEC_L 1 "input_operand"))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)
   && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])
   && !direct_move_p (operands[0], operands[1])
   && !quad_load_store_p (operands[0], operands[1])"
  [(pc)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
})


;; Generic floating point vector arithmetic support
(define_expand "add<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(plus:VEC_F (match_operand:VEC_F 1 "vfloat_operand")
		    (match_operand:VEC_F 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "sub<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(minus:VEC_F (match_operand:VEC_F 1 "vfloat_operand")
		     (match_operand:VEC_F 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "mul<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(mult:VEC_F (match_operand:VEC_F 1 "vfloat_operand")
		    (match_operand:VEC_F 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_mulv4sf3 (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_expand "div<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(div:VEC_F (match_operand:VEC_F 1 "vfloat_operand")
		   (match_operand:VEC_F 2 "vfloat_operand")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
{
  if (RS6000_RECIP_AUTO_RE_P (<MODE>mode)
      && can_create_pseudo_p () && flag_finite_math_only
      && !flag_trapping_math && flag_reciprocal_math)
    {
      rs6000_emit_swdiv (operands[0], operands[1], operands[2], true);
      DONE;
    }
})

(define_expand "neg<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(neg:VEC_F (match_operand:VEC_F 1 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_negv4sf2 (operands[0], operands[1]));
      DONE;
    }
})

(define_expand "abs<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(abs:VEC_F (match_operand:VEC_F 1 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_absv4sf2 (operands[0], operands[1]));
      DONE;
    }
})

(define_expand "smin<mode>3"
  [(set (match_operand:VEC_F 0 "register_operand")
        (smin:VEC_F (match_operand:VEC_F 1 "register_operand")
		    (match_operand:VEC_F 2 "register_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "smax<mode>3"
  [(set (match_operand:VEC_F 0 "register_operand")
        (smax:VEC_F (match_operand:VEC_F 1 "register_operand")
		    (match_operand:VEC_F 2 "register_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")


(define_expand "sqrt<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(sqrt:VEC_F (match_operand:VEC_F 1 "vfloat_operand")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode
      && !optimize_function_for_size_p (cfun)
      && flag_finite_math_only && !flag_trapping_math
      && flag_unsafe_math_optimizations)
    {
      rs6000_emit_swsqrt (operands[0], operands[1], 0);
      DONE;
    }
})

(define_expand "rsqrte<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
        (unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand")]
		      UNSPEC_RSQRT))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "re<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand")]
		      UNSPEC_FRES))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "ftrunc<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
  	(fix:VEC_F (match_operand:VEC_F 1 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_ceil<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand")]
		      UNSPEC_FRIP))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_floor<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand")]
		      UNSPEC_FRIM))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_btrunc<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(fix:VEC_F (match_operand:VEC_F 1 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_copysign<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand")
		       (match_operand:VEC_F 2 "vfloat_operand")] UNSPEC_COPYSIGN))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_copysign_v4sf3 (operands[0], operands[1],
					     operands[2]));
      DONE;
    }
})


;; Vector comparisons
(define_expand "vcond<mode><mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(if_then_else:VEC_F
	 (match_operator 3 "comparison_operator"
			 [(match_operand:VEC_F 4 "vfloat_operand")
			  (match_operand:VEC_F 5 "vfloat_operand")])
	 (match_operand:VEC_F 1 "vfloat_operand")
	 (match_operand:VEC_F 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vcond<mode><mode>"
  [(set (match_operand:VEC_I 0 "vint_operand")
	(if_then_else:VEC_I
	 (match_operator 3 "comparison_operator"
			 [(match_operand:VEC_I 4 "vint_operand")
			  (match_operand:VEC_I 5 "vint_operand")])
	 (match_operand:VEC_I 1 "vector_int_reg_or_same_bit")
	 (match_operand:VEC_I 2 "vector_int_reg_or_same_bit")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vcondv4sfv4si"
  [(set (match_operand:V4SF 0 "vfloat_operand")
	(if_then_else:V4SF
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V4SI 4 "vint_operand")
			  (match_operand:V4SI 5 "vint_operand")])
	 (match_operand:V4SF 1 "vfloat_operand")
	 (match_operand:V4SF 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
   && VECTOR_UNIT_ALTIVEC_P (V4SImode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vcondv4siv4sf"
  [(set (match_operand:V4SI 0 "vint_operand")
	(if_then_else:V4SI
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V4SF 4 "vfloat_operand")
			  (match_operand:V4SF 5 "vfloat_operand")])
	 (match_operand:V4SI 1 "vint_operand")
	 (match_operand:V4SI 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
   && VECTOR_UNIT_ALTIVEC_P (V4SImode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vcondv2dfv2di"
  [(set (match_operand:V2DF 0 "vfloat_operand")
	(if_then_else:V2DF
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V2DI 4 "vint_operand")
			  (match_operand:V2DI 5 "vint_operand")])
	 (match_operand:V2DF 1 "vfloat_operand")
	 (match_operand:V2DF 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V2DFmode)
   && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V2DImode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vcondv2div2df"
  [(set (match_operand:V2DI 0 "vint_operand")
	(if_then_else:V2DI
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V2DF 4 "vfloat_operand")
			  (match_operand:V2DF 5 "vfloat_operand")])
	 (match_operand:V2DI 1 "vint_operand")
	 (match_operand:V2DI 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V2DFmode)
   && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V2DImode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vcondu<mode><mode>"
  [(set (match_operand:VEC_I 0 "vint_operand")
	(if_then_else:VEC_I
	 (match_operator 3 "comparison_operator"
			 [(match_operand:VEC_I 4 "vint_operand")
			  (match_operand:VEC_I 5 "vint_operand")])
	 (match_operand:VEC_I 1 "vector_int_reg_or_same_bit")
	 (match_operand:VEC_I 2 "vector_int_reg_or_same_bit")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vconduv4sfv4si"
  [(set (match_operand:V4SF 0 "vfloat_operand")
	(if_then_else:V4SF
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V4SI 4 "vint_operand")
			  (match_operand:V4SI 5 "vint_operand")])
	 (match_operand:V4SF 1 "vfloat_operand")
	 (match_operand:V4SF 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
   && VECTOR_UNIT_ALTIVEC_P (V4SImode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

(define_expand "vconduv2dfv2di"
  [(set (match_operand:V2DF 0 "vfloat_operand")
	(if_then_else:V2DF
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V2DI 4 "vint_operand")
			  (match_operand:V2DI 5 "vint_operand")])
	 (match_operand:V2DF 1 "vfloat_operand")
	 (match_operand:V2DF 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V2DFmode)
   && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V2DImode)"
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    gcc_unreachable ();
})

;; To support vector condition vectorization, define vcond_mask and vec_cmp.

;; Same mode for condition true/false values and predicate operand.
(define_expand "vcond_mask_<mode><mode>"
  [(match_operand:VEC_I 0 "vint_operand")
   (match_operand:VEC_I 1 "vint_operand")
   (match_operand:VEC_I 2 "vint_operand")
   (match_operand:VEC_I 3 "vint_operand")]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  emit_insn (gen_vector_select_<mode> (operands[0], operands[2], operands[1],
				  operands[3]));
  DONE;
})

;; Condition true/false values are float but predicate operand is of
;; type integer vector with same element size.
(define_expand "vcond_mask_<mode><VEC_int>"
  [(match_operand:VEC_F 0 "vfloat_operand")
   (match_operand:VEC_F 1 "vfloat_operand")
   (match_operand:VEC_F 2 "vfloat_operand")
   (match_operand:<VEC_INT> 3 "vint_operand")]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  emit_insn (gen_vector_select_<mode> (operands[0], operands[2], operands[1],
				  gen_lowpart (<MODE>mode, operands[3])));
  DONE;
})

;; For signed integer vectors comparison.
(define_expand "vec_cmp<mode><mode>"
  [(set (match_operand:VEC_IC 0 "vint_operand")
	(match_operator 1 "signed_or_equality_comparison_operator"
	  [(match_operand:VEC_IC 2 "vint_operand")
	   (match_operand:VEC_IC 3 "vint_operand")]))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx tmp = gen_reg_rtx (<MODE>mode);
  switch (code)
    {
    case NE:
      emit_insn (gen_vector_eq<mode> (operands[0], operands[2], operands[3]));
      emit_insn (gen_one_cmpl<mode>2 (operands[0], operands[0]));
      break;
    case EQ:
      emit_insn (gen_vector_eq<mode> (operands[0], operands[2], operands[3]));
      break;
    case GE:
      emit_insn (gen_vector_nlt<mode> (operands[0],operands[2], operands[3],
				       tmp));
      break;
    case GT:
      emit_insn (gen_vector_gt<mode> (operands[0], operands[2], operands[3]));
      break;
    case LE:
      emit_insn (gen_vector_ngt<mode> (operands[0], operands[2], operands[3],
				       tmp));
      break;
    case LT:
      emit_insn (gen_vector_gt<mode> (operands[0], operands[3], operands[2]));
      break;
    default:
      gcc_unreachable ();
      break;
    }
  DONE;
})

;; For unsigned integer vectors comparison.
(define_expand "vec_cmpu<mode><mode>"
  [(set (match_operand:VEC_IC 0 "vint_operand")
	(match_operator 1 "unsigned_or_equality_comparison_operator"
	  [(match_operand:VEC_IC 2 "vint_operand")
	   (match_operand:VEC_IC 3 "vint_operand")]))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx tmp = gen_reg_rtx (<MODE>mode);
  switch (code)
    {
    case NE:
      emit_insn (gen_vector_eq<mode> (operands[0], operands[2], operands[3]));
      emit_insn (gen_one_cmpl<mode>2 (operands[0], operands[0]));
      break;
    case EQ:
      emit_insn (gen_vector_eq<mode> (operands[0], operands[2], operands[3]));
      break;
    case GEU:
      emit_insn (gen_vector_nltu<mode> (operands[0], operands[2], operands[3],
					tmp));
      break;
    case GTU:
      emit_insn (gen_vector_gtu<mode> (operands[0], operands[2], operands[3]));
      break;
    case LEU:
      emit_insn (gen_vector_ngtu<mode> (operands[0], operands[2], operands[3],
					tmp));
      break;
    case LTU:
      emit_insn (gen_vector_gtu<mode> (operands[0], operands[3], operands[2]));
      break;
    default:
      gcc_unreachable ();
      break;
    }
  DONE;
})

;; For float point vectors comparison.
(define_expand "vec_cmp<mode><VEC_int>"
  [(set (match_operand:<VEC_INT> 0 "vint_operand")
	 (match_operator 1 "comparison_operator"
	    [(match_operand:VEC_F 2 "vfloat_operand")
	    (match_operand:VEC_F 3 "vfloat_operand")]))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx res = gen_reg_rtx (<MODE>mode);
  switch (code)
    {
    case NE:
      emit_insn (gen_vector_ne<mode> (res, operands[2], operands[3]));
      break;
    case EQ:
      emit_insn (gen_vector_eq<mode> (res, operands[2], operands[3]));
      break;
    case GE:
      emit_insn (gen_vector_ge<mode> (res, operands[2], operands[3]));
      break;
    case GT:
      emit_insn (gen_vector_gt<mode> (res, operands[2], operands[3]));
      break;
    case LE:
      emit_insn (gen_vector_le<mode> (res, operands[2], operands[3]));
      break;
    case LT:
      emit_insn (gen_vector_lt<mode> (res, operands[2], operands[3]));
      break;
    case LTGT:
      emit_insn (gen_vector_ltgt<mode> (res, operands[2], operands[3]));
      break;
    case UNORDERED:
      emit_insn (gen_vector_unordered<mode> (res, operands[2], operands[3]));
      break;
    case ORDERED:
      emit_insn (gen_vector_ordered<mode> (res, operands[2], operands[3]));
      break;
    case UNEQ:
      emit_insn (gen_vector_uneq<mode> (res, operands[2], operands[3]));
      break;
    case UNGE:
      emit_insn (gen_vector_unge<mode> (res, operands[2], operands[3]));
      break;
    case UNGT:
      emit_insn (gen_vector_ungt<mode> (res, operands[2], operands[3]));
      break;
    case UNLE:
      emit_insn (gen_vector_unle<mode> (res, operands[2], operands[3]));
      break;
    case UNLT:
      emit_insn (gen_vector_unlt<mode> (res, operands[2], operands[3]));
      break;

    default:
      gcc_unreachable ();
    }

  emit_insn (gen_move_insn (operands[0], gen_lowpart (<VEC_INT>mode, res)));
  DONE;
})

(define_expand "vector_eq<mode>"
  [(set (match_operand:VEC_C 0 "vlogical_operand")
	(eq:VEC_C (match_operand:VEC_C 1 "vlogical_operand")
		  (match_operand:VEC_C 2 "vlogical_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_gt<mode>"
  [(set (match_operand:VEC_C 0 "vlogical_operand")
	(gt:VEC_C (match_operand:VEC_C 1 "vlogical_operand")
		  (match_operand:VEC_C 2 "vlogical_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

; >= for integer vectors: swap operands and apply not-greater-than
(define_expand "vector_nlt<mode>"
  [(set (match_operand:VEC_IC 3 "vlogical_operand")
	(gt:VEC_IC (match_operand:VEC_IC 2 "vlogical_operand")
		   (match_operand:VEC_IC 1 "vlogical_operand")))
   (set (match_operand:VEC_IC 0 "vlogical_operand")
	(not:VEC_IC (match_dup 3)))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx_and_attrs (operands[0]);
})

(define_expand "vector_gtu<mode>"
  [(set (match_operand:VEC_IC 0 "vint_operand")
	(gtu:VEC_IC (match_operand:VEC_IC 1 "vint_operand")
		    (match_operand:VEC_IC 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

; >= for integer vectors: swap operands and apply not-greater-than
(define_expand "vector_nltu<mode>"
  [(set (match_operand:VEC_IC 3 "vlogical_operand")
	(gtu:VEC_IC (match_operand:VEC_IC 2 "vlogical_operand")
		    (match_operand:VEC_IC 1 "vlogical_operand")))
   (set (match_operand:VEC_IC 0 "vlogical_operand")
	(not:VEC_IC (match_dup 3)))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx_and_attrs (operands[0]);
})

(define_expand "vector_geu<mode>"
  [(set (match_operand:VEC_IC 0 "vint_operand")
	(geu:VEC_IC (match_operand:VEC_IC 1 "vint_operand")
		    (match_operand:VEC_IC 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

; <= for integer vectors: apply not-greater-than
(define_expand "vector_ngt<mode>"
  [(set (match_operand:VEC_IC 3 "vlogical_operand")
	(gt:VEC_IC (match_operand:VEC_IC 1 "vlogical_operand")
		   (match_operand:VEC_IC 2 "vlogical_operand")))
   (set (match_operand:VEC_IC 0 "vlogical_operand")
	(not:VEC_IC (match_dup 3)))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx_and_attrs (operands[0]);
})

(define_expand "vector_ngtu<mode>"
  [(set (match_operand:VEC_IC 3 "vlogical_operand")
	(gtu:VEC_IC (match_operand:VEC_IC 1 "vlogical_operand")
		    (match_operand:VEC_IC 2 "vlogical_operand")))
   (set (match_operand:VEC_IC 0 "vlogical_operand")
	(not:VEC_IC (match_dup 3)))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  operands[3] = gen_reg_rtx_and_attrs (operands[0]);
})

; There are 14 possible vector FP comparison operators, gt and eq of them have
; been expanded above, so just support 12 remaining operators here.

; For ge:
(define_expand "vector_ge<mode>"
  [(set (match_operand:VEC_F 0 "vlogical_operand")
	(ge:VEC_F (match_operand:VEC_F 1 "vlogical_operand")
		  (match_operand:VEC_F 2 "vlogical_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

; For lt/le/ne/ungt/unge/unlt/unle:
; lt(a,b)   = gt(b,a)
; le(a,b)   = ge(b,a)
; unge(a,b) = ~lt(a,b)
; unle(a,b) = ~gt(a,b)
; ne(a,b)   = ~eq(a,b)
; ungt(a,b) = ~le(a,b)
; unlt(a,b) = ~ge(a,b)
(define_insn_and_split "vector_<code><mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(vector_fp_comparison_simple:VEC_F
	   (match_operand:VEC_F 1 "vfloat_operand")
	   (match_operand:VEC_F 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode) && can_create_pseudo_p ()"
  "#"
  "&& can_create_pseudo_p ()"
  [(pc)]
{
  enum rtx_code cond = <CODE>;
  bool need_invert = false;

  if (cond == UNLE || cond == UNLT || cond == NE || cond == UNGE
      || cond == UNGT)
    {
      cond = reverse_condition_maybe_unordered (cond);
      need_invert = true;
    }

  if (cond == LT || cond == LE)
    {
      cond = swap_condition (cond);
      std::swap (operands[1], operands[2]);
    }

  gcc_assert (cond == EQ || cond == GE || cond == GT);

  rtx comp = gen_rtx_fmt_ee (cond, <MODE>mode, operands[1], operands[2]);

  if (need_invert)
    {
      rtx res = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_rtx_SET (res, comp));
      emit_insn (gen_one_cmpl<mode>2 (operands[0], res));
    }
  else
    emit_insn (gen_rtx_SET (operands[0], comp));

  DONE;
})

; For ltgt/uneq/ordered/unordered:
; ltgt: gt(a,b) | gt(b,a)
; uneq: ~(gt(a,b) | gt(b,a))
; ordered: ge(a,b) | ge(b,a)
; unordered: ~(ge(a,b) | ge(b,a))
(define_insn_and_split "vector_<code><mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(vector_fp_comparison_complex:VEC_F
	   (match_operand:VEC_F 1 "vfloat_operand")
	   (match_operand:VEC_F 2 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode) && can_create_pseudo_p ()"
  "#"
  "&& can_create_pseudo_p ()"
  [(pc)]
{
  enum rtx_code cond = <CODE>;
  bool need_invert = false;

  if (cond == UNORDERED || cond == UNEQ)
    {
      cond = reverse_condition_maybe_unordered (cond);
      need_invert = true;
    }

  if (cond == LTGT)
    cond = GT;
  else if (cond == ORDERED)
    cond = GE;
  else
    gcc_unreachable ();

  rtx comp1 = gen_rtx_fmt_ee (cond, <MODE>mode, operands[1], operands[2]);
  rtx res1 = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_rtx_SET (res1, comp1));
  rtx comp2 = gen_rtx_fmt_ee (cond, <MODE>mode, operands[2], operands[1]);
  rtx res2 = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_rtx_SET (res2, comp2));

  if (need_invert)
    {
      rtx not1 = gen_rtx_fmt_e (NOT, <MODE>mode, res1);
      rtx not2 = gen_rtx_fmt_e (NOT, <MODE>mode, res2);
      rtx comp3 = gen_rtx_fmt_ee (AND, <MODE>mode, not1, not2);
      emit_insn (gen_rtx_SET (operands[0], comp3));
    }
  else
    emit_insn (gen_ior<mode>3 (operands[0], res1, res2));

  DONE;
})

;; Note the arguments for __builtin_altivec_vsel are op2, op1, mask
;; which is in the reverse order that we want
(define_expand "vector_select_<mode>"
  [(set (match_operand:VEC_L 0 "vlogical_operand")
	(ior:VEC_L
	  (and:VEC_L (not:VEC_L (match_operand:VEC_L 3 "vlogical_operand"))
		     (match_operand:VEC_L 1 "vlogical_operand"))
	  (and:VEC_L (match_dup 3)
		     (match_operand:VEC_L 2 "vlogical_operand"))))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)")

(define_expand "vector_select_<mode>_uns"
  [(set (match_operand:VEC_L 0 "vlogical_operand")
	(ior:VEC_L
	  (and:VEC_L (not:VEC_L (match_operand:VEC_L 3 "vlogical_operand"))
		     (match_operand:VEC_L 1 "vlogical_operand"))
	  (and:VEC_L (match_dup 3)
		     (match_operand:VEC_L 2 "vlogical_operand"))))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)")

;; Expansions that compare vectors producing a vector result and a predicate,
;; setting CR6 to indicate a combined status
(define_expand "vector_eq_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:VEC_A 1 "vlogical_operand")
			     (match_operand:VEC_A 2 "vlogical_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_A 0 "vlogical_operand")
	  (eq:VEC_A (match_dup 1)
		    (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_eq_v1ti_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:V1TI 1 "altivec_register_operand")
			     (match_operand:V1TI 2 "altivec_register_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_operand:V1TI 0 "vlogical_operand")
	  (eq:V1TI (match_dup 1)
		   (match_dup 2)))])]
  "TARGET_POWER10"
  "")

;; This expansion handles the V16QI, V8HI, and V4SI modes in the
;; implementation of the vec_all_ne built-in functions on Power9.
(define_expand "vector_ne_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(ne:CC (match_operand:VI 1 "vlogical_operand")
			     (match_operand:VI 2 "vlogical_operand"))]
	   UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (ne:VI (match_dup 1)
		 (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC CR6_REGNO)
	       (const_int 0)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (<MODE>mode);
})

;; This expansion handles the V16QI, V8HI, and V4SI modes in the
;; implementation of the vec_any_eq built-in functions on Power9.
(define_expand "vector_ae_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(ne:CC (match_operand:VI 1 "vlogical_operand")
			     (match_operand:VI 2 "vlogical_operand"))]
	   UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (ne:VI (match_dup 1)
		 (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC CR6_REGNO)
	       (const_int 0)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (<MODE>mode);
})

;; This expansion handles the V16QI, V8HI, and V4SI modes in the
;; implementation of the vec_all_nez and vec_any_eqz built-in
;; functions on Power9.
(define_expand "vector_nez_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(unspec:VI
		       [(match_operand:VI 1 "vlogical_operand")
			(match_operand:VI 2 "vlogical_operand")]
		       UNSPEC_NEZ_P)]
	   UNSPEC_PREDICATE))
     (set (match_operand:VI 0 "vlogical_operand")
	  (unspec:VI [(match_dup 1)
		      (match_dup 2)]
	   UNSPEC_NEZ_P))])]
  "TARGET_P9_VECTOR"
  "")

;; This expansion handles the V2DI mode in the implementation of the
;; vec_all_ne built-in function on Power9.
;;
;; Since the Power9 "xvcmpne<mode>." instruction does not support DImode,
;; this expands into the same rtl that would be used for the Power8
;; architecture.
(define_expand "vector_ne_v2di_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:V2DI 1 "vlogical_operand")
			     (match_operand:V2DI 2 "vlogical_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (eq:V2DI (match_dup 1)
		   (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (V2DImode);
})

(define_expand "vector_ne_v1ti_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:V1TI 1 "altivec_register_operand")
			     (match_operand:V1TI 2 "altivec_register_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (eq:V1TI (match_dup 1)
		   (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))]
  "TARGET_POWER10"
{
  operands[3] = gen_reg_rtx (V1TImode);
})

;; This expansion handles the V2DI mode in the implementation of the
;; vec_any_eq built-in function on Power9.
;;
;; Since the Power9 "xvcmpne<mode>." instruction does not support DImode,
;; this expands into the same rtl that would be used for the Power8
;; architecture.
(define_expand "vector_ae_v2di_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:V2DI 1 "vlogical_operand")
			     (match_operand:V2DI 2 "vlogical_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (eq:V2DI (match_dup 1)
		   (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (V2DImode);
})

(define_expand "vector_ae_v1ti_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:V1TI 1 "altivec_register_operand")
			     (match_operand:V1TI 2 "altivec_register_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (eq:V1TI (match_dup 1)
		   (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "TARGET_POWER10"
{
  operands[3] = gen_reg_rtx (V1TImode);
})

;; This expansion handles the V4SF and V2DF modes in the Power9
;; implementation of the vec_all_ne built-in functions.  Note that the
;; expansions for this pattern with these modes makes no use of power9-
;; specific instructions since there are no new power9 instructions
;; for vector compare not equal with floating point arguments.
(define_expand "vector_ne_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:VEC_F 1 "vlogical_operand")
			     (match_operand:VEC_F 2 "vlogical_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (eq:VEC_F (match_dup 1)
		    (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (<MODE>mode);
})

;; This expansion handles the V4SF and V2DF modes in the Power9
;; implementation of the vec_any_eq built-in functions.  Note that the
;; expansions for this pattern with these modes makes no use of power9-
;; specific instructions since there are no new power9 instructions
;; for vector compare not equal with floating point arguments.
(define_expand "vector_ae_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(eq:CC (match_operand:VEC_F 1 "vlogical_operand")
			     (match_operand:VEC_F 2 "vlogical_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_dup 3)
	  (eq:VEC_F (match_dup 1)
		    (match_dup 2)))])
   (set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "TARGET_P9_VECTOR"
{
  operands[3] = gen_reg_rtx (<MODE>mode);
})

(define_expand "vector_gt_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(gt:CC (match_operand:VEC_A 1 "vlogical_operand")
			     (match_operand:VEC_A 2 "vlogical_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_A 0 "vlogical_operand")
	  (gt:VEC_A (match_dup 1)
		    (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_gt_v1ti_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(gt:CC (match_operand:V1TI 1 "vlogical_operand")
			     (match_operand:V1TI 2 "vlogical_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_operand:V1TI 0 "vlogical_operand")
	  (gt:V1TI (match_dup 1)
		   (match_dup 2)))])]
  "TARGET_POWER10"
  "")

(define_expand "vector_ge_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(ge:CC (match_operand:VEC_F 1 "vfloat_operand")
			     (match_operand:VEC_F 2 "vfloat_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_F 0 "vfloat_operand")
	  (ge:VEC_F (match_dup 1)
		    (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_gtu_<mode>_p"
  [(parallel
    [(set (reg:CC CR6_REGNO)
	  (unspec:CC [(gtu:CC (match_operand:VEC_IC 1 "vint_operand")
			      (match_operand:VEC_IC 2 "vint_operand"))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_IC 0 "vlogical_operand")
	  (gtu:VEC_IC (match_dup 1)
		      (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; AltiVec/VSX predicates.

;; This expansion is triggered during expansion of predicate built-in
;; functions (built-ins defined with the RS6000_BUILTIN_P macro) by the
;; altivec_expand_predicate_builtin() function when the value of the
;; integer constant first argument equals zero (aka __CR6_EQ in altivec.h).
(define_expand "cr6_test_for_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")

;; This expansion is triggered during expansion of predicate built-in
;; functions (built-ins defined with the RS6000_BUILTIN_P macro) by the
;; altivec_expand_predicate_builtin() function when the value of the
;; integer constant first argument equals one (aka __CR6_EQ_REV in altivec.h).
(define_expand "cr6_test_for_zero_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC CR6_REGNO)
	       (const_int 0)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")

;; This expansion is triggered during expansion of predicate built-in
;; functions (built-ins defined with the RS6000_BUILTIN_P macro) by the
;; altivec_expand_predicate_builtin() function when the value of the
;; integer constant first argument equals two (aka __CR6_LT in altivec.h).
(define_expand "cr6_test_for_lt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC CR6_REGNO)
	       (const_int 0)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")

;; This expansion is triggered during expansion of predicate built-in
;; functions (built-ins defined with the RS6000_BUILTIN_P macro) by the
;; altivec_expand_predicate_builtin() function when the value of the
;; integer constant first argument equals three
;; (aka __CR6_LT_REV in altivec.h).
(define_expand "cr6_test_for_lt_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC CR6_REGNO)
	       (const_int 0)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")


;; Vector count leading zeros
(define_expand "clz<mode>2"
  [(set (match_operand:VEC_I 0 "register_operand")
	(clz:VEC_I (match_operand:VEC_I 1 "register_operand")))]
  "TARGET_P8_VECTOR")

;; Vector count trailing zeros
(define_expand "ctz<mode>2"
  [(set (match_operand:VEC_I 0 "register_operand")
	(ctz:VEC_I (match_operand:VEC_I 1 "register_operand")))]
  "TARGET_P9_VECTOR")

;; Vector population count
(define_expand "popcount<mode>2"
  [(set (match_operand:VEC_I 0 "register_operand")
        (popcount:VEC_I (match_operand:VEC_I 1 "register_operand")))]
  "TARGET_P8_VECTOR")

;; Vector parity
(define_expand "parity<mode>2"
  [(set (match_operand:VEC_IP 0 "register_operand")
	(parity:VEC_IP (match_operand:VEC_IP 1 "register_operand")))]
  "TARGET_P9_VECTOR")


;; Same size conversions
(define_expand "float<VEC_int><mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(float:VEC_F (match_operand:<VEC_INT> 1 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vcfsx (operands[0], operands[1], const0_rtx));
      DONE;
    }
})

(define_expand "floatuns<VEC_int><mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand")
	(unsigned_float:VEC_F (match_operand:<VEC_INT> 1 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vcfux (operands[0], operands[1], const0_rtx));
      DONE;
    }
})

(define_expand "fix_trunc<mode><VEC_int>2"
  [(set (match_operand:<VEC_INT> 0 "vint_operand")
	(fix:<VEC_INT> (match_operand:VEC_F 1 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vctsxs (operands[0], operands[1], const0_rtx));
      DONE;
    }
})

(define_expand "fixuns_trunc<mode><VEC_int>2"
  [(set (match_operand:<VEC_INT> 0 "vint_operand")
	(unsigned_fix:<VEC_INT> (match_operand:VEC_F 1 "vfloat_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vctuxs (operands[0], operands[1], const0_rtx));
      DONE;
    }
})


;; Vector initialization, set, extract
(define_expand "vec_init<mode><VEC_base_l>"
  [(match_operand:VEC_E 0 "vlogical_operand")
   (match_operand:VEC_E 1 "")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rs6000_expand_vector_init (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_set<mode>"
  [(match_operand:VEC_E 0 "vlogical_operand")
   (match_operand:<VEC_base> 1 "register_operand")
   (match_operand 2 "vec_set_index_operand")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rs6000_expand_vector_set (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "vec_extract<mode><VEC_base_l>"
  [(match_operand:<VEC_base> 0 "register_operand")
   (match_operand:VEC_E 1 "vlogical_operand")
   (match_operand 2 "const_int_operand")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rs6000_expand_vector_extract (operands[0], operands[1], operands[2]);
  DONE;
})

;; Convert double word types to single word types
(define_expand "vec_pack_trunc_v2df"
  [(match_operand:V4SF 0 "vfloat_operand")
   (match_operand:V2DF 1 "vfloat_operand")
   (match_operand:V2DF 2 "vfloat_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && TARGET_ALTIVEC"
{
  rtx r1 = gen_reg_rtx (V4SFmode);
  rtx r2 = gen_reg_rtx (V4SFmode);

  emit_insn (gen_vsx_xvcvdpsp (r1, operands[1]));
  emit_insn (gen_vsx_xvcvdpsp (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

(define_expand "vec_pack_sfix_trunc_v2df"
  [(match_operand:V4SI 0 "vint_operand")
   (match_operand:V2DF 1 "vfloat_operand")
   (match_operand:V2DF 2 "vfloat_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && TARGET_ALTIVEC"
{
  rtx r1 = gen_reg_rtx (V4SImode);
  rtx r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_vsx_xvcvdpsxws (r1, operands[1]));
  emit_insn (gen_vsx_xvcvdpsxws (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

(define_expand "vec_pack_ufix_trunc_v2df"
  [(match_operand:V4SI 0 "vint_operand")
   (match_operand:V2DF 1 "vfloat_operand")
   (match_operand:V2DF 2 "vfloat_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && TARGET_ALTIVEC"
{
  rtx r1 = gen_reg_rtx (V4SImode);
  rtx r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_vsx_xvcvdpuxws (r1, operands[1]));
  emit_insn (gen_vsx_xvcvdpuxws (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

;; Convert single word types to double word
(define_expand "vec_unpacks_hi_v4sf"
  [(match_operand:V2DF 0 "vfloat_operand")
   (match_operand:V4SF 1 "vfloat_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)"
{
  rtx reg = gen_reg_rtx (V4SFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvspdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_lo_v4sf"
  [(match_operand:V2DF 0 "vfloat_operand")
   (match_operand:V4SF 1 "vfloat_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)"
{
  rtx reg = gen_reg_rtx (V4SFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvspdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_float_hi_v4si"
  [(match_operand:V2DF 0 "vfloat_operand")
   (match_operand:V4SI 1 "vint_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvsxwdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_float_lo_v4si"
  [(match_operand:V2DF 0 "vfloat_operand")
   (match_operand:V4SI 1 "vint_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvsxwdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacku_float_hi_v4si"
  [(match_operand:V2DF 0 "vfloat_operand")
   (match_operand:V4SI 1 "vint_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvuxwdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacku_float_lo_v4si"
  [(match_operand:V2DF 0 "vfloat_operand")
   (match_operand:V4SI 1 "vint_operand")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvuxwdp (operands[0], reg));
  DONE;
})


;; Align vector loads with a permute.
(define_expand "vec_realign_load_<mode>"
  [(match_operand:VEC_K 0 "vlogical_operand")
   (match_operand:VEC_K 1 "vlogical_operand")
   (match_operand:VEC_K 2 "vlogical_operand")
   (match_operand:V16QI 3 "vlogical_operand")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_altivec_vperm_<mode> (operands[0], operands[1],
    	      				 operands[2], operands[3]));
  else
    {
      /* We have changed lvsr to lvsl, so to complete the transformation
         of vperm for LE, we must swap the inputs.  */
      rtx unspec = gen_rtx_UNSPEC (<MODE>mode,
                                   gen_rtvec (3, operands[2],
                                              operands[1], operands[3]),
                                   UNSPEC_VPERM);
      emit_move_insn (operands[0], unspec);
    }
  DONE;
})

;; Under VSX, vectors of 4/8 byte alignments do not need to be aligned
;; since the load already handles it.
(define_expand "movmisalign<mode>"
 [(set (match_operand:VEC_N 0 "nonimmediate_operand")
       (match_operand:VEC_N 1 "any_operand"))]
 "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_ALLOW_MOVMISALIGN"
{
  rs6000_emit_move (operands[0], operands[1], <MODE>mode);
  DONE;
})

;; Vector shift right in bits. Currently supported ony for shift
;; amounts that can be expressed as byte shifts (divisible by 8).
;; General shift amounts can be supported using vsro + vsr. We're
;; not expecting to see these yet (the vectorizer currently
;; generates only shifts by a whole number of vector elements).
;; Note that the vec_shr operation is actually defined as 
;; 'shift toward element 0' so is a shr for LE and shl for BE.
(define_expand "vec_shr_<mode>"
  [(match_operand:VEC_L 0 "vlogical_operand")
   (match_operand:VEC_L 1 "vlogical_operand")
   (match_operand:QI 2 "reg_or_short_operand")]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rtx bitshift = operands[2];
  rtx shift;
  rtx insn;
  rtx zero_reg, op1, op2;
  HOST_WIDE_INT bitshift_val;
  HOST_WIDE_INT byteshift_val;

  if (! CONSTANT_P (bitshift))
    FAIL;
  bitshift_val = INTVAL (bitshift);
  if (bitshift_val & 0x7)
    FAIL;
  byteshift_val = (bitshift_val >> 3);
  zero_reg = gen_reg_rtx (<MODE>mode);
  emit_move_insn (zero_reg, CONST0_RTX (<MODE>mode));
  if (!BYTES_BIG_ENDIAN)
    {
      /* Note, byteshift_val can be 0!  */
      byteshift_val = -byteshift_val & 15;
      op1 = zero_reg;
      op2 = operands[1];
    }
  else
    {
      op1 = operands[1];
      op2 = zero_reg;
    }

  if (TARGET_VSX && (byteshift_val & 0x3) == 0)
    {
      shift = gen_rtx_CONST_INT (QImode, byteshift_val >> 2);
      insn = gen_vsx_xxsldwi_<mode> (operands[0], op1, op2, shift);
    }
  else
    {
      shift = gen_rtx_CONST_INT (QImode, byteshift_val);
      insn = gen_altivec_vsldoi_<mode> (operands[0], op1, op2, shift);
    }

  emit_insn (insn);
  DONE;
})

;; Expanders for rotate each element in a vector
(define_expand "vrotl<mode>3"
  [(set (match_operand:VEC_IC 0 "vint_operand")
	(rotate:VEC_IC (match_operand:VEC_IC 1 "vint_operand")
		       (match_operand:VEC_IC 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  /* Shift amount in needs to be put in bits[57:63] of 128-bit operand2.  */
  if (<MODE>mode == V1TImode)
    {
      rtx tmp = gen_reg_rtx (V1TImode);

      emit_insn (gen_xxswapd_v1ti (tmp, operands[2]));
      emit_insn (gen_altivec_vrlq (operands[0], operands[1], tmp));
      DONE;
    }
 })

;; Expanders for rotatert to make use of vrotl
(define_expand "vrotr<mode>3"
  [(set (match_operand:VEC_I 0 "vint_operand")
	(rotatert:VEC_I (match_operand:VEC_I 1 "vint_operand")
		(match_operand:VEC_I 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rtx rot_count = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neg<mode>2 (rot_count, operands[2]));
  emit_insn (gen_vrotl<mode>3 (operands[0], operands[1], rot_count));
  DONE;
})

;; Expanders for arithmetic shift left on each vector element
(define_expand "vashl<mode>3"
  [(set (match_operand:VEC_I 0 "vint_operand")
	(ashift:VEC_I (match_operand:VEC_I 1 "vint_operand")
		      (match_operand:VEC_I 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; No immediate version of this 128-bit instruction
(define_expand "vashl<mode>3"
  [(set (match_operand:VEC_TI 0 "vsx_register_operand" "=v")
	(ashift:VEC_TI (match_operand:VEC_TI 1 "vsx_register_operand")
			 (match_operand:VEC_TI 2 "vsx_register_operand")))]
  "TARGET_POWER10"
{
  /* Shift amount in needs to be put in bits[57:63] of 128-bit operand2. */
  rtx tmp = gen_reg_rtx (<MODE>mode);

  emit_insn (gen_xxswapd_v1ti (tmp, operands[2]));
  emit_insn(gen_altivec_vslq_<mode> (operands[0], operands[1], tmp));
  DONE;
})

;; Expanders for logical shift right on each vector element
(define_expand "vlshr<mode>3"
  [(set (match_operand:VEC_I 0 "vint_operand")
	(lshiftrt:VEC_I (match_operand:VEC_I 1 "vint_operand")
			(match_operand:VEC_I 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; No immediate version of this 128-bit instruction
(define_expand "vlshr<mode>3"
  [(set (match_operand:VEC_TI 0 "vsx_register_operand" "=v")
	(lshiftrt:VEC_TI (match_operand:VEC_TI 1 "vsx_register_operand")
			   (match_operand:VEC_TI 2 "vsx_register_operand")))]
  "TARGET_POWER10"
{
  /* Shift amount in needs to be put into bits[57:63] of 128-bit operand2. */
  rtx tmp = gen_reg_rtx (<MODE>mode);

  emit_insn (gen_xxswapd_v1ti (tmp, operands[2]));
  emit_insn(gen_altivec_vsrq_<mode> (operands[0], operands[1], tmp));
  DONE;
})

;; Expanders for arithmetic shift right on each vector element
(define_expand "vashr<mode>3"
  [(set (match_operand:VEC_IC 0 "vint_operand")
	(ashiftrt:VEC_IC (match_operand:VEC_IC 1 "vint_operand")
			 (match_operand:VEC_IC 2 "vint_operand")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  /* Shift amount in needs to be put in bits[57:63] of 128-bit operand2.  */
  if (<MODE>mode == V1TImode)
    {
      rtx tmp = gen_reg_rtx (V1TImode);

      emit_insn (gen_xxswapd_v1ti (tmp, operands[2]));
      emit_insn (gen_altivec_vsraq (operands[0], operands[1], tmp));
      DONE;
    }
})


;; Vector reduction expanders for VSX
; The (VEC_reduc:...
;	(op1)
;	(unspec:... [(const_int 0)] UNSPEC_REDUC))
;
; is to allow us to use a code iterator, but not completely list all of the
; vector rotates, etc. to prevent canonicalization


(define_expand "reduc_<VEC_reduc:VEC_reduc_name>_scal_<VEC_F:mode>"
  [(match_operand:<VEC_base> 0 "register_operand")
   (VEC_reduc:VEC_F (match_operand:VEC_F 1 "vfloat_operand")
		    (unspec:VEC_F [(const_int 0)] UNSPEC_REDUC))]
  "VECTOR_UNIT_VSX_P (<VEC_F:MODE>mode)"
  {
    rtx vec = gen_reg_rtx (<VEC_F:MODE>mode);
    rtx elt = BYTES_BIG_ENDIAN
		? gen_int_mode (GET_MODE_NUNITS (<VEC_F:MODE>mode) - 1, QImode)
		: const0_rtx;
    emit_insn (gen_vsx_reduc_<VEC_reduc:VEC_reduc_name>_<VEC_F:mode> (vec,
	operand1));
    emit_insn (gen_vsx_extract_<VEC_F:mode> (operand0, vec, elt));
    DONE;
  })
