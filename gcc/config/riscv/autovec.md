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

(define_expand "len_maskload<mode><vm>"
  [(match_operand:V 0 "register_operand")
   (match_operand:V 1 "memory_operand")
   (match_operand 2 "autovec_length_operand")
   (match_operand:<VM> 3 "vector_mask_operand")
   (match_operand 4 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_load_store (operands, true);
  DONE;
})

(define_expand "len_maskstore<mode><vm>"
  [(match_operand:V 0 "memory_operand")
   (match_operand:V 1 "register_operand")
   (match_operand 2 "autovec_length_operand")
   (match_operand:<VM> 3 "vector_mask_operand")
   (match_operand 4 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_load_store (operands, false);
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
;; ---- [INT,FP] permutation
;; -------------------------------------------------------------------------
;; This is the pattern permutes the vector
;; -------------------------------------------------------------------------

(define_expand "vec_perm<mode>"
  [(match_operand:V 0 "register_operand")
   (match_operand:V 1 "register_operand")
   (match_operand:V 2 "register_operand")
   (match_operand:<VINDEX> 3 "vector_perm_operand")]
  "TARGET_VECTOR && GET_MODE_NUNITS (<MODE>mode).is_constant ()"
  {
    riscv_vector::expand_vec_perm (operands[0], operands[1],
				   operands[2], operands[3]);
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

(define_insn_and_split "<optab><mode>3"
  [(set (match_operand:VI 0 "register_operand" "=vr")
    (any_shift:VI
     (match_operand:VI 1 "register_operand"    " vr")
     (match_operand:<VEL> 2 "csr_operand"      " rK")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  operands[2] = gen_lowpart (Pmode, operands[2]);
  riscv_vector::emit_vlmax_insn (code_for_pred_scalar (<CODE>, <MODE>mode),
				 riscv_vector::RVV_BINOP, operands);
  DONE;
}  
 [(set_attr "type" "vshift")
  (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------
;; ---- [INT] Binary shifts by scalar.
;; -------------------------------------------------------------------------
;; Includes:
;; - vsll.vv/vsra.vv/vsrl.vv
;; -------------------------------------------------------------------------

(define_insn_and_split "v<optab><mode>3"
  [(set (match_operand:VI 0 "register_operand"  "=vr,vr")
    (any_shift:VI
     (match_operand:VI 1 "register_operand"     " vr,vr")
     (match_operand:VI 2 "vector_shift_operand" " vr,vk")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				 riscv_vector::RVV_BINOP, operands);
  DONE;
}
 [(set_attr "type" "vshift")
  (set_attr "mode" "<MODE>")])

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
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
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
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    insn_code icode = code_for_pred_not (<MODE>mode);
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
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
;; ---- [INT] Sign and zero extension
;; -------------------------------------------------------------------------
;; Includes:
;; - vzext.vf[2|4|8]
;; - vsext.vf[2|4|8]
;; -------------------------------------------------------------------------

;; Use define_insn_and_split to define vsext.vf2/vzext.vf2 will help
;; to combine instructions as below:
;;   vsext.vf2 + vsext.vf2 + vadd.vv ==> vwadd.vv
(define_insn_and_split "<optab><v_double_trunc><mode>2"
  [(set (match_operand:VWEXTI 0 "register_operand" "=&vr")
    (any_extend:VWEXTI
     (match_operand:<V_DOUBLE_TRUNC> 1 "register_operand" "vr")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_vf2 (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
}
  [(set_attr "type" "vext")
   (set_attr "mode" "<MODE>")])

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
(define_insn_and_split "trunc<mode><v_double_trunc>2"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand" "=vr")
    (truncate:<V_DOUBLE_TRUNC>
     (match_operand:VWEXTI 1 "register_operand"              " vr")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_trunc (<MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
}
  [(set_attr "type" "vshift")
   (set_attr "mode" "<MODE>")])

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
;; == Conversions
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT<-FP] Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfcvt.rtz.xu.f.v
;; - vfcvt.rtz.x.f.v
;; -------------------------------------------------------------------------

(define_expand "<optab><mode><vconvert>2"
  [(set (match_operand:<VCONVERT> 0 "register_operand")
	(any_fix:<VCONVERT>
	  (match_operand:VF 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
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
;; - [INT] ABS expansion to vmslt and vneg.
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

;; -------------------------------------------------------------------------------
;; ---- [FP] Unary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - vfneg.v/vfabs.v
;; -------------------------------------------------------------------------------
(define_expand "<optab><mode>2"
  [(set (match_operand:VF 0 "register_operand")
    (any_float_unop_nofrm:VF
     (match_operand:VF 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------------
;; - [FP] Square root
;; -------------------------------------------------------------------------------
;; Includes:
;; - vfsqrt.v
;; -------------------------------------------------------------------------------
(define_expand "<optab><mode>2"
  [(set (match_operand:VF 0 "register_operand")
    (any_float_unop:VF
     (match_operand:VF 1 "register_operand")))]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_fp_insn (icode, riscv_vector::RVV_UNOP, operands);
  DONE;
})

;; =========================================================================
;; == Ternary arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] VMACC and VMADD
;; -------------------------------------------------------------------------
;; Includes:
;; - vmacc
;; - vmadd
;; -------------------------------------------------------------------------

;; We can't expand FMA for the following reasons:
;; 1. Before RA, we don't know which multiply-add instruction is the ideal one.
;;    The vmacc is the ideal instruction when operands[3] overlaps operands[0].
;;    The vmadd is the ideal instruction when operands[1|2] overlaps operands[0].
;; 2. According to vector.md, the multiply-add patterns has 'merge' operand which
;;    is the operands[5]. Since operands[5] should overlap operands[0], this operand
;;    should be allocated the same regno as operands[1|2|3].
;; 3. The 'merge' operand is always a real merge operand and we don't allow undefined
;;    operand.
;; 4. The operation of FMA pattern needs VLMAX vsetlvi which needs a VL operand.
;;
;; In this situation, we design the codegen of FMA as follows:
;; 1. clobber a scratch in the expand pattern of FMA.
;; 2. Let's RA decide which input operand (operands[1|2|3]) overlap operands[0].
;; 3. Generate instructions (vmacc or vmadd) according to the register allocation
;;    result after reload_completed.
(define_expand "fma<mode>4"
  [(parallel
    [(set (match_operand:VI 0 "register_operand")
	  (plus:VI
	    (mult:VI
	      (match_operand:VI 1 "register_operand")
	      (match_operand:VI 2 "register_operand"))
	    (match_operand:VI 3 "register_operand")))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  })

(define_insn_and_split "*fma<VI:mode><P:mode>"
  [(set (match_operand:VI 0 "register_operand"     "=vr, vr, ?&vr")
	(plus:VI
	  (mult:VI
	    (match_operand:VI 1 "register_operand" " %0, vr,   vr")
	    (match_operand:VI 2 "register_operand" " vr, vr,   vr"))
	  (match_operand:VI 3 "register_operand"   " vr,  0,   vr")))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VI:MODE>mode, operands[4]);
    if (which_alternative == 2)
      emit_insn (gen_rtx_SET (operands[0], operands[3]));
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_ternary_insn (code_for_pred_mul_plus (<VI:MODE>mode),
					   riscv_vector::RVV_TERNOP, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<VI:MODE>")])

;; -------------------------------------------------------------------------
;; ---- [INT] VNMSAC and VNMSUB
;; -------------------------------------------------------------------------
;; Includes:
;; - vnmsac
;; - vnmsub
;; -------------------------------------------------------------------------

(define_expand "fnma<mode>4"
  [(parallel
    [(set (match_operand:VI 0 "register_operand")
   (minus:VI
     (match_operand:VI 3 "register_operand")
     (mult:VI
       (match_operand:VI 1 "register_operand")
       (match_operand:VI 2 "register_operand"))))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  })

(define_insn_and_split "*fnma<VI:mode><P:mode>"
  [(set (match_operand:VI 0 "register_operand"     "=vr, vr, ?&vr")
 (minus:VI
   (match_operand:VI 3 "register_operand"   " vr,  0,   vr")
   (mult:VI
     (match_operand:VI 1 "register_operand" " %0, vr,   vr")
     (match_operand:VI 2 "register_operand" " vr, vr,   vr"))))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VI:MODE>mode, operands[4]);
    if (which_alternative == 2)
      emit_insn (gen_rtx_SET (operands[0], operands[3]));
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_ternary_insn (code_for_pred_minus_mul (<VI:MODE>mode),
    					   riscv_vector::RVV_TERNOP, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<VI:MODE>")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFMACC and VFMADD
;; -------------------------------------------------------------------------
;; Includes:
;; - vfmacc
;; - vfmadd
;; -------------------------------------------------------------------------

(define_expand "fma<mode>4"
  [(parallel
    [(set (match_operand:VF 0 "register_operand")
	  (fma:VF
	    (match_operand:VF 1 "register_operand")
	    (match_operand:VF 2 "register_operand")
	    (match_operand:VF 3 "register_operand")))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  })

(define_insn_and_split "*fma<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"   "=vr, vr, ?&vr")
	(fma:VF
	  (match_operand:VF 1 "register_operand" " %0, vr,   vr")
	  (match_operand:VF 2 "register_operand" " vr, vr,   vr")
	  (match_operand:VF 3 "register_operand" " vr,  0,   vr")))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    if (which_alternative == 2)
      emit_insn (gen_rtx_SET (operands[0], operands[3]));
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_fp_ternary_insn (code_for_pred_mul (PLUS, <VF:MODE>mode),
					      riscv_vector::RVV_TERNOP, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFNMSAC and VFNMSUB
;; -------------------------------------------------------------------------
;; Includes:
;; - vfnmsac
;; - vfnmsub
;; -------------------------------------------------------------------------

(define_expand "fnma<mode>4"
  [(parallel
    [(set (match_operand:VF 0 "register_operand")
	  (fma:VF
	    (neg:VF
	      (match_operand:VF 1 "register_operand"))
	    (match_operand:VF 2 "register_operand")
	    (match_operand:VF 3 "register_operand")))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  })

(define_insn_and_split "*fnma<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"     "=vr, vr, ?&vr")
	(fma:VF
	  (neg:VF
	    (match_operand:VF 1 "register_operand" " %0, vr,   vr"))
	  (match_operand:VF 2 "register_operand"   " vr, vr,   vr")
	  (match_operand:VF 3 "register_operand"   " vr,  0,   vr")))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    if (which_alternative == 2)
      emit_insn (gen_rtx_SET (operands[0], operands[3]));
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_fp_ternary_insn (code_for_pred_mul_neg (PLUS, <VF:MODE>mode),
					      riscv_vector::RVV_TERNOP, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFMSAC and VFMSUB
;; -------------------------------------------------------------------------
;; Includes:
;; - vfmsac
;; - vfmsub
;; -------------------------------------------------------------------------

(define_expand "fms<mode>4"
  [(parallel
    [(set (match_operand:VF 0 "register_operand")
	  (fma:VF
	    (match_operand:VF 1 "register_operand")
	    (match_operand:VF 2 "register_operand")
	    (neg:VF
	      (match_operand:VF 3 "register_operand"))))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  })

(define_insn_and_split "*fms<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"     "=vr, vr, ?&vr")
	(fma:VF
	  (match_operand:VF 1 "register_operand"   " %0, vr,   vr")
	  (match_operand:VF 2 "register_operand"   " vr, vr,   vr")
	  (neg:VF
	    (match_operand:VF 3 "register_operand" " vr,  0,   vr"))))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    if (which_alternative == 2)
      emit_insn (gen_rtx_SET (operands[0], operands[3]));
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_fp_ternary_insn (code_for_pred_mul (MINUS, <VF:MODE>mode),
					      riscv_vector::RVV_TERNOP, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFMSAC and VFMSUB
;; -------------------------------------------------------------------------
;; Includes:
;; - vfmsac
;; - vfmsub
;; -------------------------------------------------------------------------

(define_expand "fnms<mode>4"
  [(parallel
    [(set (match_operand:VF 0 "register_operand")
	  (fma:VF
	    (neg:VF
	      (match_operand:VF 1 "register_operand"))
	    (match_operand:VF 2 "register_operand")
	    (neg:VF
	      (match_operand:VF 3 "register_operand"))))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  })

(define_insn_and_split "*fnms<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"     "=vr, vr, ?&vr")
	(fma:VF
	  (neg:VF
	    (match_operand:VF 1 "register_operand" " %0, vr,   vr"))
	  (match_operand:VF 2 "register_operand"   " vr, vr,   vr")
	  (neg:VF
	    (match_operand:VF 3 "register_operand" " vr,  0,   vr"))))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    if (which_alternative == 2)
      emit_insn (gen_rtx_SET (operands[0], operands[3]));
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_fp_ternary_insn (code_for_pred_mul_neg (MINUS, <VF:MODE>mode),
					      riscv_vector::RVV_TERNOP, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")])

;; =========================================================================
;; == SELECT_VL
;; =========================================================================

(define_expand "select_vl<mode>"
  [(match_operand:P 0 "register_operand")
   (match_operand:P 1 "vector_length_operand")
   (match_operand:P 2 "")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_select_vl (operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Insert a vector element.
;; -------------------------------------------------------------------------

(define_expand "vec_set<mode>"
  [(match_operand:V	0 "register_operand")
   (match_operand:<VEL> 1 "register_operand")
   (match_operand	2 "immediate_operand")]
  "TARGET_VECTOR"
{
  /* If we set the first element, emit an v(f)mv.s.[xf].  */
  if (operands[2] == const0_rtx)
    {
      rtx ops[] = {operands[0], riscv_vector::gen_scalar_move_mask (<VM>mode),
		   RVV_VUNDEF (<MODE>mode), operands[1]};
      riscv_vector::emit_scalar_move_insn
	  (code_for_pred_broadcast (<MODE>mode), ops);
    }
  else
    {
      /* Move the desired value into a vector register and insert
	 it at the proper position using vslideup with an
	 "effective length" of 1 i.e. a VL 1 past the offset.  */

      /* Slide offset = element index.  */
      int offset = INTVAL (operands[2]);

      /* Only insert one element, i.e. VL = offset + 1.  */
      rtx length = gen_reg_rtx (Pmode);
      emit_move_insn (length, GEN_INT (offset + 1));

      /* Move operands[1] into a vector register via vmv.v.x using the same
	 VL we need for the slide.  */
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx ops1[] = {tmp, operands[1]};
      riscv_vector::emit_nonvlmax_integer_move_insn
	(code_for_pred_broadcast (<MODE>mode), ops1, length);

      /* Slide exactly one element up leaving the tail elements
	 unchanged.  */
      rtx ops2[] = {operands[0], operands[0], tmp, operands[2]};
      riscv_vector::emit_nonvlmax_slide_tu_insn
	(code_for_pred_slide (UNSPEC_VSLIDEUP, <MODE>mode), ops2, length);
    }
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Extract a vector element.
;; -------------------------------------------------------------------------
(define_expand "vec_extract<mode><vel>"
  [(set (match_operand:<VEL>	  0 "register_operand")
     (vec_select:<VEL>
       (match_operand:V		  1 "register_operand")
       (parallel
	 [(match_operand	  2 "nonmemory_operand")])))]
  "TARGET_VECTOR"
{
  /* Element extraction can be done by sliding down the requested element
     to index 0 and then v(f)mv.[xf].s it to a scalar register.  */

  /* When extracting any other than the first element we need to slide
     it down.  */
  rtx tmp = NULL_RTX;
  if (operands[2] != const0_rtx)
    {
      /* Emit the slide down to index 0 in a new vector.  */
      tmp = gen_reg_rtx (<MODE>mode);
      rtx ops[] = {tmp, RVV_VUNDEF (<MODE>mode), operands[1], operands[2]};
      riscv_vector::emit_vlmax_slide_insn
	(code_for_pred_slide (UNSPEC_VSLIDEDOWN, <MODE>mode), ops);
    }

  /* Emit v(f)mv.[xf].s.  */
  emit_insn (gen_pred_extract_first (<MODE>mode, operands[0],
				     tmp ? tmp : operands[1]));
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Binary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vfadd.vv/vfsub.vv/...
;; - vfadd.vf/vfsub.vf/...
;; -------------------------------------------------------------------------
(define_expand "<optab><mode>3"
  [(match_operand:VF 0 "register_operand")
   (any_float_binop:VF
    (match_operand:VF 1 "register_operand")
    (match_operand:VF 2 "register_operand"))]
  "TARGET_VECTOR"
{
  riscv_vector::emit_vlmax_fp_insn (code_for_pred (<CODE>, <MODE>mode),
				    riscv_vector::RVV_BINOP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; Includes:
;; - vfmin.vv/vfmax.vv
;; - vfmin.vf/vfmax.vf
;; -------------------------------------------------------------------------
(define_expand "<optab><mode>3"
  [(match_operand:VF 0 "register_operand")
   (any_float_binop_nofrm:VF
    (match_operand:VF 1 "register_operand")
    (match_operand:VF 2 "register_operand"))]
  "TARGET_VECTOR"
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				 riscv_vector::RVV_BINOP, operands);
  DONE;
})
