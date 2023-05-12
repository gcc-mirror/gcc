;; Machine description for auto-vectorization using RVV for GNU compiler.
;; Copyright (C) 2023 Free Software Foundation, Inc.
;; Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

;; =========================================================================
;; == Loads/Stores
;; =========================================================================

;; len_load/len_store is a sub-optimal pattern for RVV auto-vectorization support.
;; We will replace them when len_maskload/len_maskstore is supported in loop vectorizer.
(define_expand "len_load_<mode>"
  [(match_operand:V 0 "register_operand")
   (match_operand:V 1 "memory_operand")
   (match_operand 2 "vector_length_operand")
   (match_operand 3 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::emit_nonvlmax_insn (code_for_pred_mov (<MODE>mode),
  				    riscv_vector::RVV_UNOP, operands, operands[2]);
  DONE;
})

(define_expand "len_store_<mode>"
  [(match_operand:V 0 "memory_operand")
   (match_operand:V 1 "register_operand")
   (match_operand 2 "vector_length_operand")
   (match_operand 3 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::emit_nonvlmax_insn (code_for_pred_mov (<MODE>mode),
  				    riscv_vector::RVV_UNOP, operands, operands[2]);
  DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:V 0 "nonimmediate_operand")
	(match_operand:V 1 "general_operand"))]
  "TARGET_VECTOR"
  {
    /* Equivalent to a normal move for our purpooses.  */
    emit_move_insn (operands[0], operands[1]);
    DONE;
  }
)

;; =========================================================================
;; == Vector creation
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Linear series
;; -------------------------------------------------------------------------
;; Includes:
;; - vid.v
;; - vmul.vx
;; - vadd.vx/vadd.vi
;; -------------------------------------------------------------------------

(define_expand "@vec_series<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VEL> 1 "reg_or_int_operand")
   (match_operand:<VEL> 2 "reg_or_int_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_series (operands[0], operands[1], operands[2]);
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Initialize from individual elements
;; -------------------------------------------------------------------------
;; This is the pattern initialize the vector
;; -------------------------------------------------------------------------

(define_expand "vec_init<mode><vel>"
  [(match_operand:V 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_init (operands[0], operands[1]);
    DONE;
  }
)

;; ========================================================================
;; == Vector operations
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Binary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vadd.vv/vsub.vv/...
;; - vadd.vi/vsub.vi/...
;; -------------------------------------------------------------------------

(define_expand "<optab><mode>3"
  [(set (match_operand:VI 0 "register_operand")
    (any_int_binop_no_shift:VI
     (match_operand:VI 1 "<binop_rhs1_predicate>")
     (match_operand:VI 2 "<binop_rhs2_predicate>")))]
  "TARGET_VECTOR"
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				 riscv_vector::RVV_BINOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Binary shifts by scalar.
;; -------------------------------------------------------------------------
;; Includes:
;; - vsll.vx/vsra.vx/vsrl.vx
;; - vsll.vi/vsra.vi/vsrl.vi
;; -------------------------------------------------------------------------

(define_expand "<optab><mode>3"
  [(set (match_operand:VI 0 "register_operand")
    (any_shift:VI
     (match_operand:VI 1 "register_operand")
     (match_operand:<VEL> 2 "csr_operand")))]
  "TARGET_VECTOR"
{
  operands[2] = gen_lowpart (Pmode, operands[2]);
  riscv_vector::emit_vlmax_insn (code_for_pred_scalar (<CODE>, <MODE>mode),
				 riscv_vector::RVV_BINOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Binary shifts by scalar.
;; -------------------------------------------------------------------------
;; Includes:
;; - vsll.vv/vsra.vv/vsrl.vv
;; -------------------------------------------------------------------------

(define_expand "v<optab><mode>3"
  [(set (match_operand:VI 0 "register_operand")
    (any_shift:VI
     (match_operand:VI 1 "register_operand")
     (match_operand:VI 2 "vector_shift_operand")))]
  "TARGET_VECTOR"
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				 riscv_vector::RVV_BINOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [BOOL] Binary logical operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vmand.mm
;; - vmxor.mm
;; - vmor.mm
;; -------------------------------------------------------------------------

(define_insn_and_split "<optab><mode>3"
  [(set (match_operand:VB 0 "register_operand"                 "=vr")
	(any_bitwise:VB (match_operand:VB 1 "register_operand" " vr")
			(match_operand:VB 2 "register_operand" " vr")))]
  "TARGET_VECTOR"
  "#"
  "&& can_create_pseudo_p ()"
  [(const_int 0)]
  {
    insn_code icode = code_for_pred (<CODE>, <MODE>mode);
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_BINOP, operands);
    DONE;
  }
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------
;; ---- [BOOL] Inverse
;; -------------------------------------------------------------------------
;; Includes:
;; - vmnot.m
;; -------------------------------------------------------------------------

(define_insn_and_split "one_cmpl<mode>2"
  [(set (match_operand:VB 0 "register_operand"         "=vr")
	(not:VB (match_operand:VB 1 "register_operand" " vr")))]
  "TARGET_VECTOR"
  "#"
  "&& can_create_pseudo_p ()"
  [(const_int 0)]
  {
    insn_code icode = code_for_pred_not (<MODE>mode);
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
    DONE;
  }
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------
;; ---- [BOOL] Binary logical operations (inverted second input)
;; -------------------------------------------------------------------------
;; Includes:
;; - vmandnot.mm
;; - vmornot.mm
;; -------------------------------------------------------------------------

(define_insn_and_split "*<optab>not<mode>"
  [(set (match_operand:VB 0 "register_operand"           "=vr")
	(bitmanip_bitwise:VB
	  (not:VB (match_operand:VB 2 "register_operand" " vr"))
	  (match_operand:VB 1 "register_operand"         " vr")))]
  "TARGET_VECTOR"
  "#"
  "&& can_create_pseudo_p ()"
  [(const_int 0)]
  {
    insn_code icode = code_for_pred_not (<CODE>, <MODE>mode);
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_BINOP, operands);
    DONE;
  }
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------
;; ---- [BOOL] Binary logical operations (inverted result)
;; -------------------------------------------------------------------------
;; Includes:
;; - vmnand.mm
;; - vmnor.mm
;; - vmxnor.mm
;; -------------------------------------------------------------------------

(define_insn_and_split "*n<optab><mode>"
  [(set (match_operand:VB 0 "register_operand"     "=vr")
	(not:VB
	  (any_bitwise:VB
	    (match_operand:VB 1 "register_operand" " vr")
	    (match_operand:VB 2 "register_operand" " vr"))))]
  "TARGET_VECTOR"
  "#"
  "&& can_create_pseudo_p ()"
  [(const_int 0)]
  {
    insn_code icode = code_for_pred_n (<CODE>, <MODE>mode);
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_BINOP, operands);
    DONE;
  }
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")])

;; =========================================================================
;; == Comparisons and selects
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Select based on masks
;; -------------------------------------------------------------------------
;; Includes merging patterns for:
;; - vmerge.vv
;; - vmerge.vx
;; - vfmerge.vf
;; -------------------------------------------------------------------------

(define_expand "@vcond_mask_<mode><vm>"
  [(match_operand:V 0 "register_operand")
   (match_operand:<VM> 3 "register_operand")
   (match_operand:V 1 "nonmemory_operand")
   (match_operand:V 2 "register_operand")]
  "TARGET_VECTOR"
  {
    /* The order of vcond_mask is opposite to pred_merge.  */
    std::swap (operands[1], operands[2]);
    riscv_vector::emit_vlmax_merge_insn (code_for_pred_merge (<MODE>mode),
    			riscv_vector::RVV_MERGE_OP, operands);
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Comparisons
;; -------------------------------------------------------------------------
;; Includes:
;; - vms<eq/ne/ltu/lt/leu/le/gtu/gt>.<vv/vx/vi>
;; -------------------------------------------------------------------------

(define_expand "vec_cmp<mode><vm>"
  [(set (match_operand:<VM> 0 "register_operand")
	(match_operator:<VM> 1 "comparison_operator"
	  [(match_operand:VI 2 "register_operand")
	   (match_operand:VI 3 "register_operand")]))]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_cmp (operands[0], GET_CODE (operands[1]),
				  operands[2], operands[3]);
    DONE;
  }
)

(define_expand "vec_cmpu<mode><vm>"
  [(set (match_operand:<VM> 0 "register_operand")
	(match_operator:<VM> 1 "comparison_operator"
	  [(match_operand:VI 2 "register_operand")
	   (match_operand:VI 3 "register_operand")]))]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_cmp (operands[0], GET_CODE (operands[1]),
				  operands[2], operands[3]);
    DONE;
  }
)

(define_expand "vec_cmp<mode><vm>"
  [(set (match_operand:<VM> 0 "register_operand")
	(match_operator:<VM> 1 "comparison_operator"
	  [(match_operand:VF 2 "register_operand")
	   (match_operand:VF 3 "register_operand")]))]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_cmp_float (operands[0], GET_CODE (operands[1]),
				        operands[2], operands[3], false);
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Compare and select
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

(define_expand "vcond<V:mode><VI:mode>"
  [(set (match_operand:V 0 "register_operand")
	(if_then_else:V
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VI 4 "register_operand")
	     (match_operand:VI 5 "register_operand")])
	  (match_operand:V 1 "register_operand")
	  (match_operand:V 2 "register_operand")))]
  "TARGET_VECTOR && known_eq (GET_MODE_NUNITS (<V:MODE>mode),
  		GET_MODE_NUNITS (<VI:MODE>mode))"
  {
    riscv_vector::expand_vcond (operands);
    DONE;
  }
)

(define_expand "vcondu<V:mode><VI:mode>"
  [(set (match_operand:V 0 "register_operand")
	(if_then_else:V
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VI 4 "register_operand")
	     (match_operand:VI 5 "register_operand")])
	  (match_operand:V 1 "register_operand")
	  (match_operand:V 2 "register_operand")))]
  "TARGET_VECTOR && known_eq (GET_MODE_NUNITS (<V:MODE>mode),
  		GET_MODE_NUNITS (<VI:MODE>mode))"
  {
    riscv_vector::expand_vcond (operands);
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Sign and zero extension
;; -------------------------------------------------------------------------
;; Includes:
;; - vzext.vf[2|4|8]
;; - vsext.vf[2|4|8]
;; -------------------------------------------------------------------------

(define_expand "<optab><v_double_trunc><mode>2"
  [(set (match_operand:VWEXTI 0 "register_operand")
    (any_extend:VWEXTI
     (match_operand:<V_DOUBLE_TRUNC> 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_vf2 (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
})

(define_expand "<optab><v_quad_trunc><mode>2"
  [(set (match_operand:VQEXTI 0 "register_operand")
    (any_extend:VQEXTI
     (match_operand:<V_QUAD_TRUNC> 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_vf4 (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
})

(define_expand "<optab><v_oct_trunc><mode>2"
  [(set (match_operand:VOEXTI 0 "register_operand")
    (any_extend:VOEXTI
     (match_operand:<V_OCT_TRUNC> 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_vf8 (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Truncation
;; -------------------------------------------------------------------------
;; - vncvt.x.x.w
;; -------------------------------------------------------------------------
(define_expand "trunc<mode><v_double_trunc>2"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand")
    (truncate:<V_DOUBLE_TRUNC>
     (match_operand:VWEXTI 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_trunc (<MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; Truncation to a mode whose inner mode size is a quarter of mode's.
;; We emulate this with two consecutive vncvts.
;; -------------------------------------------------------------------------
(define_expand "trunc<mode><v_quad_trunc>2"
  [(set (match_operand:<V_QUAD_TRUNC> 0 "register_operand")
    (truncate:<V_QUAD_TRUNC>
     (match_operand:VQEXTI 1 "register_operand")))]
  "TARGET_VECTOR"
{
  rtx half = gen_reg_rtx (<V_DOUBLE_TRUNC>mode);
  rtx opshalf[] = {half, operands[1]};
  insn_code icode = code_for_pred_trunc (<MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, opshalf);

  rtx ops[] = {operands[0], half};
  icode = code_for_pred_trunc (<V_DOUBLE_TRUNC>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, ops);
  DONE;
})

;; -------------------------------------------------------------------------
;; Truncation to a mode whose inner mode size is an eigth of mode's.
;; We emulate this with three consecutive vncvts.
;; -------------------------------------------------------------------------
(define_expand "trunc<mode><v_oct_trunc>2"
  [(set (match_operand:<V_OCT_TRUNC> 0 "register_operand")
    (truncate:<V_OCT_TRUNC>
     (match_operand:VOEXTI 1 "register_operand")))]
  "TARGET_VECTOR"
{
  rtx half = gen_reg_rtx (<V_DOUBLE_TRUNC>mode);
  rtx opshalf[] = {half, operands[1]};
  insn_code icode = code_for_pred_trunc (<MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, opshalf);

  rtx quarter = gen_reg_rtx (<V_QUAD_TRUNC>mode);
  rtx opsquarter[] = {quarter, half};
  icode = code_for_pred_trunc (<V_DOUBLE_TRUNC>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, opsquarter);

  rtx ops[] = {operands[0], quarter};
  icode = code_for_pred_trunc (<V_QUAD_TRUNC>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, ops);
  DONE;
})

;; =========================================================================
;; == Unary arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------------
;; ---- [INT] Unary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - vneg.v/vnot.v
;; -------------------------------------------------------------------------------
(define_expand "<optab><mode>2"
  [(set (match_operand:VI 0 "register_operand")
    (any_int_unop:VI
     (match_operand:VI 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------------
;; - ABS expansion to vmslt and vneg
;; -------------------------------------------------------------------------------

(define_expand "abs<mode>2"
  [(set (match_operand:VI 0 "register_operand")
    (match_operand:VI 1 "register_operand"))]
  "TARGET_VECTOR"
{
  rtx zero = gen_const_vec_duplicate (<MODE>mode, GEN_INT (0));
  machine_mode mask_mode = riscv_vector::get_mask_mode (<MODE>mode).require ();
  rtx mask = gen_reg_rtx (mask_mode);
  riscv_vector::expand_vec_cmp (mask, LT, operands[1], zero);

  rtx ops[] = {operands[0], mask, operands[1], operands[1]};
  riscv_vector::emit_vlmax_masked_mu_insn (code_for_pred (NEG, <MODE>mode),
					   riscv_vector::RVV_UNOP_MU, ops);
  DONE;
})
