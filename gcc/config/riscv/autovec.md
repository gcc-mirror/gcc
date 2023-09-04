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

(define_expand "mask_len_load<mode><vm>"
  [(match_operand:V 0 "register_operand")
   (match_operand:V 1 "memory_operand")
   (match_operand:<VM> 2 "vector_mask_operand")
   (match_operand 3 "autovec_length_operand")
   (match_operand 4 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_load_store (operands, true);
  DONE;
})

(define_expand "mask_len_store<mode><vm>"
  [(match_operand:V 0 "memory_operand")
   (match_operand:V 1 "register_operand")
   (match_operand:<VM> 2 "vector_mask_operand")
   (match_operand 3 "autovec_length_operand")
   (match_operand 4 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_load_store (operands, false);
  DONE;
})

;; =========================================================================
;; == Gather Load
;; =========================================================================

(define_expand "mask_len_gather_load<RATIO64:mode><RATIO64I:mode>"
  [(match_operand:RATIO64 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO64I 2 "register_operand")
   (match_operand 3 "<RATIO64:gs_extension>")
   (match_operand 4 "<RATIO64:gs_scale>")
   (match_operand:<RATIO64:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

(define_expand "mask_len_gather_load<RATIO32:mode><RATIO32I:mode>"
  [(match_operand:RATIO32 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO32I 2 "register_operand")
   (match_operand 3 "<RATIO32:gs_extension>")
   (match_operand 4 "<RATIO32:gs_scale>")
   (match_operand:<RATIO32:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

(define_expand "mask_len_gather_load<RATIO16:mode><RATIO16I:mode>"
  [(match_operand:RATIO16 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO16I 2 "register_operand")
   (match_operand 3 "<RATIO16:gs_extension>")
   (match_operand 4 "<RATIO16:gs_scale>")
   (match_operand:<RATIO16:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

(define_expand "mask_len_gather_load<RATIO8:mode><RATIO8I:mode>"
  [(match_operand:RATIO8 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO8I 2 "register_operand")
   (match_operand 3 "<RATIO8:gs_extension>")
   (match_operand 4 "<RATIO8:gs_scale>")
   (match_operand:<RATIO8:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

(define_expand "mask_len_gather_load<RATIO4:mode><RATIO4I:mode>"
  [(match_operand:RATIO4 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO4I 2 "register_operand")
   (match_operand 3 "<RATIO4:gs_extension>")
   (match_operand 4 "<RATIO4:gs_scale>")
   (match_operand:<RATIO4:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

(define_expand "mask_len_gather_load<RATIO2:mode><RATIO2I:mode>"
  [(match_operand:RATIO2 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO2I 2 "register_operand")
   (match_operand 3 "<RATIO2:gs_extension>")
   (match_operand 4 "<RATIO2:gs_scale>")
   (match_operand:<RATIO2:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

;; When SEW = 8 and LMUL = 8, we can't find any index mode with
;; larger SEW. Since RVV indexed load/store support zero extend
;; implicitly and not support scaling, we should only allow
;; operands[3] and operands[4] to be const_1_operand.
(define_expand "mask_len_gather_load<RATIO1:mode><RATIO1:mode>"
  [(match_operand:RATIO1 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO1 2 "register_operand")
   (match_operand 3 "<RATIO1:gs_extension>")
   (match_operand 4 "<RATIO1:gs_scale>")
   (match_operand:<RATIO1:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

;; =========================================================================
;; == Scatter Store
;; =========================================================================

(define_expand "mask_len_scatter_store<RATIO64:mode><RATIO64I:mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO64I 1 "register_operand")
   (match_operand 2 "<RATIO64:gs_extension>")
   (match_operand 3 "<RATIO64:gs_scale>")
   (match_operand:RATIO64 4 "register_operand")
   (match_operand:<RATIO64:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

(define_expand "mask_len_scatter_store<RATIO32:mode><RATIO32I:mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO32I 1 "register_operand")
   (match_operand 2 "<RATIO32:gs_extension>")
   (match_operand 3 "<RATIO32:gs_scale>")
   (match_operand:RATIO32 4 "register_operand")
   (match_operand:<RATIO32:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

(define_expand "mask_len_scatter_store<RATIO16:mode><RATIO16I:mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO16I 1 "register_operand")
   (match_operand 2 "<RATIO16:gs_extension>")
   (match_operand 3 "<RATIO16:gs_scale>")
   (match_operand:RATIO16 4 "register_operand")
   (match_operand:<RATIO16:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

(define_expand "mask_len_scatter_store<RATIO8:mode><RATIO8I:mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO8I 1 "register_operand")
   (match_operand 2 "<RATIO8:gs_extension>")
   (match_operand 3 "<RATIO8:gs_scale>")
   (match_operand:RATIO8 4 "register_operand")
   (match_operand:<RATIO8:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

(define_expand "mask_len_scatter_store<RATIO4:mode><RATIO4I:mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO4I 1 "register_operand")
   (match_operand 2 "<RATIO4:gs_extension>")
   (match_operand 3 "<RATIO4:gs_scale>")
   (match_operand:RATIO4 4 "register_operand")
   (match_operand:<RATIO4:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

(define_expand "mask_len_scatter_store<RATIO2:mode><RATIO2I:mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO2I 1 "register_operand")
   (match_operand 2 "<RATIO2:gs_extension>")
   (match_operand 3 "<RATIO2:gs_scale>")
   (match_operand:RATIO2 4 "register_operand")
   (match_operand:<RATIO2:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

;; When SEW = 8 and LMUL = 8, we can't find any index mode with
;; larger SEW. Since RVV indexed load/store support zero extend
;; implicitly and not support scaling, we should only allow
;; operands[3] and operands[4] to be const_1_operand.
(define_expand "mask_len_scatter_store<RATIO1:mode><RATIO1:mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO1 1 "register_operand")
   (match_operand 2 "<RATIO1:gs_extension>")
   (match_operand 3 "<RATIO1:gs_scale>")
   (match_operand:RATIO1 4 "register_operand")
   (match_operand:<RATIO1:VM> 5 "vector_mask_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

;; =========================================================================
;; == Array Load/Store
;; =========================================================================

(define_expand "vec_mask_len_load_lanes<mode><vsingle>"
  [(match_operand:VT 0 "register_operand")
   (match_operand:VT 1 "memory_operand")
   (match_operand:<VM> 2 "vector_mask_operand")
   (match_operand 3 "autovec_length_operand")
   (match_operand 4 "const_0_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_lanes_load_store (operands, true);
    DONE;
  }
)

(define_expand "vec_mask_len_store_lanes<mode><vsingle>"
  [(match_operand:VT 0 "memory_operand")
   (match_operand:VT 1 "register_operand")
   (match_operand:<VM> 2 "vector_mask_operand")
   (match_operand 3 "autovec_length_operand")
   (match_operand 4 "const_0_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_lanes_load_store (operands, false);
    DONE;
  }
)

;; =========================================================================
;; == Vector creation
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [BOOL] Duplicate element
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Implement a predicate broadcast by shifting the low bit of the scalar
;; input into the top bit by duplicate the input and do a compare with zero.
(define_expand "vec_duplicate<mode>"
  [(set (match_operand:VB 0 "register_operand")
	(vec_duplicate:VB (match_operand:QI 1 "register_operand")))]
  "TARGET_VECTOR"
  {
    poly_int64 nunits = GET_MODE_NUNITS (<MODE>mode);
    machine_mode mode = riscv_vector::get_vector_mode (QImode, nunits).require ();
    rtx dup = expand_vector_broadcast (mode, operands[1]);
    riscv_vector::expand_vec_cmp (operands[0], NE, dup, CONST0_RTX (mode));
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Linear series
;; -------------------------------------------------------------------------
;; Includes:
;; - vid.v
;; - vmul.vx
;; - vadd.vx/vadd.vi
;; -------------------------------------------------------------------------

(define_expand "@vec_series<mode>"
  [(match_operand:V_VLSI 0 "register_operand")
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

;; Slide an RVV vector left and insert a scalar into element 0.
(define_expand "vec_shl_insert_<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:VI 1 "register_operand")
   (match_operand:<VEL> 2 "reg_or_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_slide (UNSPEC_VSLIDE1UP, <MODE>mode);
  rtx ops[] = {operands[0], operands[1], operands[2]};
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops);
  DONE;
})

(define_expand "vec_shl_insert_<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:VF 1 "register_operand")
   (match_operand:<VEL> 2 "register_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_slide (UNSPEC_VFSLIDE1UP, <MODE>mode);
  rtx ops[] = {operands[0], operands[1], operands[2]};
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops);
  DONE;
})

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

(define_insn_and_split "<optab><mode>3"
  [(set (match_operand:VI 0 "register_operand")
    (any_int_binop_no_shift:VI
     (match_operand:VI 1 "<binop_rhs1_predicate>")
     (match_operand:VI 2 "<binop_rhs2_predicate>")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				 riscv_vector::BINARY_OP, operands);
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
  [(set (match_operand:V_VLSI 0 "register_operand"        "=vr")
    (any_shift:V_VLSI
     (match_operand:V_VLSI 1 "register_operand"           " vr")
     (match_operand:<VEL> 2 "vector_scalar_shift_operand" " rK")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  operands[2] = gen_lowpart (Pmode, operands[2]);
  riscv_vector::emit_vlmax_insn (code_for_pred_scalar (<CODE>, <MODE>mode),
				 riscv_vector::BINARY_OP, operands);
  DONE;
}
 [(set_attr "type" "vshift")
  (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------
;; ---- [INT] Binary shifts by vector.
;; -------------------------------------------------------------------------
;; Includes:
;; - vsll.vv/vsra.vv/vsrl.vv
;; -------------------------------------------------------------------------

(define_insn_and_split "v<optab><mode>3"
  [(set (match_operand:V_VLSI 0 "register_operand"  "=vr,vr")
    (any_shift:V_VLSI
     (match_operand:V_VLSI 1 "register_operand"     " vr,vr")
     (match_operand:V_VLSI 2 "vector_shift_operand" " vr,vk")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				 riscv_vector::BINARY_OP, operands);
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
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_MASK_OP, operands);
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
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_MASK_OP, operands);
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

(define_insn_and_split "@vcond_mask_<mode><vm>"
  [(set (match_operand:V 0 "register_operand")
        (if_then_else:V
          (match_operand:<VM> 3 "register_operand")
          (match_operand:V 1 "nonmemory_operand")
          (match_operand:V 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    /* The order of vcond_mask is opposite to pred_merge.  */
    std::swap (operands[1], operands[2]);
    riscv_vector::emit_vlmax_insn (code_for_pred_merge (<MODE>mode),
                                   riscv_vector::MERGE_OP, operands);
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
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
}
  [(set_attr "type" "vext")
   (set_attr "mode" "<MODE>")])

(define_insn_and_split "<optab><v_quad_trunc><mode>2"
  [(set (match_operand:VQEXTI 0 "register_operand")
    (any_extend:VQEXTI
     (match_operand:<V_QUAD_TRUNC> 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_vf4 (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
})

(define_insn_and_split "<optab><v_oct_trunc><mode>2"
  [(set (match_operand:VOEXTI 0 "register_operand")
    (any_extend:VOEXTI
     (match_operand:<V_OCT_TRUNC> 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_vf8 (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
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
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
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
  emit_insn (gen_trunc<mode><v_double_trunc>2 (half, operands[1]));
  emit_insn (gen_trunc<v_double_trunc><v_quad_trunc>2 (operands[0], half));
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
  rtx quarter = gen_reg_rtx (<V_QUAD_TRUNC>mode);
  emit_insn (gen_trunc<mode><v_quad_trunc>2 (quarter, operands[1]));
  emit_insn (gen_trunc<v_quad_trunc><v_oct_trunc>2 (operands[0], quarter));
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Widening.
;; -------------------------------------------------------------------------
;; - vfwcvt.f.f.v
;; -------------------------------------------------------------------------
(define_insn_and_split "extend<v_double_trunc><mode>2"
  [(set (match_operand:VWEXTF_ZVFHMIN 0 "register_operand" "=&vr")
    (float_extend:VWEXTF_ZVFHMIN
     (match_operand:<V_DOUBLE_TRUNC>  1 "register_operand" "  vr")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_extend (<MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
}
  [(set_attr "type" "vfwcvtftof")
   (set_attr "mode" "<MODE>")])

(define_expand "extend<v_quad_trunc><mode>2"
  [(set (match_operand:VQEXTF 0 "register_operand")
    (float_extend:VQEXTF
     (match_operand:<V_QUAD_TRUNC> 1 "register_operand")))]
  "TARGET_VECTOR && (TARGET_ZVFHMIN || TARGET_ZVFH)"
{
  rtx dblw = gen_reg_rtx (<V_DOUBLE_TRUNC>mode);
  emit_insn (gen_extend<v_quad_trunc><v_double_trunc>2 (dblw, operands[1]));
  emit_insn (gen_extend<v_double_trunc><mode>2 (operands[0], dblw));
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Narrowing.
;; -------------------------------------------------------------------------
;; - vfncvt.f.f.w
;; -------------------------------------------------------------------------
(define_insn_and_split "trunc<mode><v_double_trunc>2"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand" "=vr")
    (float_truncate:<V_DOUBLE_TRUNC>
     (match_operand:VWEXTF_ZVFHMIN 1 "register_operand"      " vr")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_trunc (<MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP_FRM_DYN, operands);
  DONE;
}
  [(set_attr "type" "vfncvtftof")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------
;; Narrowing to a mode whose inner mode size is a quarter of mode's.
;; We emulate this with two consecutive vfncvts.
;; -------------------------------------------------------------------------
(define_expand "trunc<mode><v_quad_trunc>2"
  [(set (match_operand:<V_QUAD_TRUNC> 0 "register_operand")
    (float_truncate:<V_QUAD_TRUNC>
     (match_operand:VQEXTF 1 "register_operand")))]
  "TARGET_VECTOR && (TARGET_ZVFHMIN || TARGET_ZVFH)"
{
  rtx half = gen_reg_rtx (<V_DOUBLE_TRUNC>mode);
  rtx opshalf[] = {half, operands[1]};

  /* According to the RISC-V V Spec 13.19. we need to use
     vfncvt.rod.f.f.w for all steps but the last.  */
  insn_code icode = code_for_pred_rod_trunc (<MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, opshalf);

  emit_insn (gen_trunc<v_double_trunc><v_quad_trunc>2 (operands[0], half));
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

(define_insn_and_split "<optab><mode><vconvert>2"
  [(set (match_operand:<VCONVERT> 0 "register_operand")
	(any_fix:<VCONVERT>
	  (match_operand:VF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfcvt.f.xu.v
;; - vfcvt.f.x.v
;; -------------------------------------------------------------------------

(define_insn_and_split "<float_cvt><vconvert><mode>2"
  [(set (match_operand:VF 0 "register_operand")
	(any_float:VF
	  (match_operand:<VCONVERT> 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP_FRM_DYN, operands);
  DONE;
})

;; =========================================================================
;; == Widening/narrowing Conversions
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT<-FP] Widening Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfwcvt.rtz.xu.f.v
;; - vfwcvt.rtz.x.f.v
;; -------------------------------------------------------------------------
(define_insn_and_split "<optab><vnconvert><mode>2"
  [(set (match_operand:VWCONVERTI 0 "register_operand")
	(any_fix:VWCONVERTI
	  (match_operand:<VNCONVERT> 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_widen (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Widening Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfwcvt.f.xu.v
;; - vfwcvt.f.x.v
;; -------------------------------------------------------------------------
(define_insn_and_split "<float_cvt><vnconvert><mode>2"
  [(set (match_operand:VF 0 "register_operand")
	(any_float:VF
	  (match_operand:<VNCONVERT> 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_widen (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT<-FP] Narrowing Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfncvt.rtz.xu.f.v
;; - vfncvt.rtz.x.f.v
;; -------------------------------------------------------------------------
(define_insn_and_split "<optab><mode><vnconvert>2"
  [(set (match_operand:<VNCONVERT> 0 "register_operand")
	(any_fix:<VNCONVERT>
	  (match_operand:VF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_narrow (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Narrowing Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfncvt.f.xu.w
;; - vfncvt.f.x.w
;; -------------------------------------------------------------------------
(define_insn_and_split "<float_cvt><mode><vnconvert>2"
  [(set (match_operand:<VNCONVERT> 0 "register_operand")
	(any_float:<VNCONVERT>
	  (match_operand:VWCONVERTI 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_narrow (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP_FRM_DYN, operands);
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
(define_insn_and_split "<optab><mode>2"
  [(set (match_operand:VI 0 "register_operand")
    (any_int_unop:VI
     (match_operand:VI 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------------
;; - [INT] ABS expansion to vmslt and vneg.
;; -------------------------------------------------------------------------------

(define_insn_and_split "abs<mode>2"
  [(set (match_operand:VI 0 "register_operand")
     (abs:VI
       (match_operand:VI 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx zero = gen_const_vec_duplicate (<MODE>mode, GEN_INT (0));
  machine_mode mask_mode = riscv_vector::get_mask_mode (<MODE>mode);
  rtx mask = gen_reg_rtx (mask_mode);
  riscv_vector::expand_vec_cmp (mask, LT, operands[1], zero);

  rtx ops[] = {operands[0], mask, operands[1], operands[1]};
  riscv_vector::emit_vlmax_insn (code_for_pred (NEG, <MODE>mode),
                                  riscv_vector::UNARY_OP_TAMU, ops);
  DONE;
})

;; -------------------------------------------------------------------------------
;; ---- [FP] Unary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - vfneg.v/vfabs.v
;; -------------------------------------------------------------------------------
(define_insn_and_split "<optab><mode>2"
  [(set (match_operand:VF 0 "register_operand")
    (any_float_unop_nofrm:VF
     (match_operand:VF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------------
;; - [FP] Square root
;; -------------------------------------------------------------------------------
;; Includes:
;; - vfsqrt.v
;; -------------------------------------------------------------------------------
(define_insn_and_split "<optab><mode>2"
  [(set (match_operand:VF 0 "register_operand")
    (any_float_unop:VF
     (match_operand:VF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP_FRM_DYN, operands);
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
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_insn_lra (code_for_pred_mul_plus (<VI:MODE>mode),
					riscv_vector::TERNARY_OP, ops, operands[4]);
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
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_insn_lra (code_for_pred_minus_mul (<VI:MODE>mode),
                                       riscv_vector::TERNARY_OP, ops, operands[4]);
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
	  (unspec:VF
	    [(fma:VF
	      (match_operand:VF 1 "register_operand")
	      (match_operand:VF 2 "register_operand")
	      (match_operand:VF 3 "register_operand"))
	     (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  }
  [(set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

(define_insn_and_split "*fma<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"   "=vr, vr, ?&vr")
	(unspec:VF
	  [(fma:VF
	    (match_operand:VF 1 "register_operand" " %0, vr,   vr")
	    (match_operand:VF 2 "register_operand" " vr, vr,   vr")
	    (match_operand:VF 3 "register_operand" " vr,  0,   vr"))
	   (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_insn_lra (code_for_pred_mul (PLUS, <VF:MODE>mode),
					riscv_vector::TERNARY_OP_FRM_DYN, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")
   (set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

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
	  (unspec:VF
	    [(fma:VF
	      (neg:VF
		(match_operand:VF 1 "register_operand"))
	      (match_operand:VF 2 "register_operand")
	      (match_operand:VF 3 "register_operand"))
	     (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  }
  [(set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

(define_insn_and_split "*fnma<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"     "=vr, vr, ?&vr")
	(unspec:VF
	  [(fma:VF
	    (neg:VF
	      (match_operand:VF 1 "register_operand" " %0, vr,   vr"))
	    (match_operand:VF 2 "register_operand"   " vr, vr,   vr")
	    (match_operand:VF 3 "register_operand"   " vr,  0,   vr"))
	   (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_insn_lra (code_for_pred_mul_neg (PLUS, <VF:MODE>mode),
					riscv_vector::TERNARY_OP_FRM_DYN, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")
   (set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

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
	  (unspec:VF
	    [(fma:VF
	      (match_operand:VF 1 "register_operand")
	      (match_operand:VF 2 "register_operand")
	      (neg:VF
		(match_operand:VF 3 "register_operand")))
	     (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  }
  [(set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

(define_insn_and_split "*fms<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"     "=vr, vr, ?&vr")
	(unspec:VF
	  [(fma:VF
	    (match_operand:VF 1 "register_operand"   " %0, vr,   vr")
	    (match_operand:VF 2 "register_operand"   " vr, vr,   vr")
	    (neg:VF
	      (match_operand:VF 3 "register_operand" " vr,  0,   vr")))
	   (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_insn_lra (code_for_pred_mul (MINUS, <VF:MODE>mode),
					riscv_vector::TERNARY_OP_FRM_DYN, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")
   (set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

;; -------------------------------------------------------------------------
;; ---- [FP] VFNMACC and VFNMADD
;; -------------------------------------------------------------------------
;; Includes:
;; - vfnmacc
;; - vfnmadd
;; -------------------------------------------------------------------------

(define_expand "fnms<mode>4"
  [(parallel
    [(set (match_operand:VF 0 "register_operand")
	  (unspec:VF
	    [(fma:VF
	      (neg:VF
		(match_operand:VF 1 "register_operand"))
	      (match_operand:VF 2 "register_operand")
	      (neg:VF
		(match_operand:VF 3 "register_operand")))
	     (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    operands[4] = gen_reg_rtx (Pmode);
  }
  [(set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

(define_insn_and_split "*fnms<VF:mode><P:mode>"
  [(set (match_operand:VF 0 "register_operand"     "=vr, vr, ?&vr")
	(unspec:VF
	  [(fma:VF
	    (neg:VF
	      (match_operand:VF 1 "register_operand" " %0, vr,   vr"))
	    (match_operand:VF 2 "register_operand"   " vr, vr,   vr")
	    (neg:VF
	      (match_operand:VF 3 "register_operand" " vr,  0,   vr")))
	   (reg:SI FRM_REGNUM)] UNSPEC_VFFMA))
   (clobber (match_operand:P 4 "register_operand" "=r,r,r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_vsetvl (<VF:MODE>mode, operands[4]);
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[0]};
    riscv_vector::emit_vlmax_insn_lra (code_for_pred_mul_neg (MINUS, <VF:MODE>mode),
					riscv_vector::TERNARY_OP_FRM_DYN, ops, operands[4]);
    DONE;
  }
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<VF:MODE>")
   (set (attr "frm_mode") (symbol_ref "riscv_vector::FRM_DYN"))])

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
   (match_operand	2 "nonmemory_operand")]
  "TARGET_VECTOR"
{
  /* If we set the first element, emit an v(f)mv.s.[xf].  */
  if (operands[2] == const0_rtx)
    {
      rtx ops[] = {operands[0], operands[1]};
      riscv_vector::emit_nonvlmax_insn (code_for_pred_broadcast (<MODE>mode),
                                         riscv_vector::SCALAR_MOVE_OP, ops, CONST1_RTX (Pmode));
    }
  else
    {
      /* Move the desired value into a vector register and insert
	 it at the proper position using vslideup with an
	 "effective length" of 1 i.e. a VL 1 past the offset.  */

      /* Here we set VL = offset + 1.  */
      rtx length = gen_reg_rtx (Pmode);
      operands[2] = gen_lowpart (Pmode, operands[2]);
      if (CONST_INT_P (operands[2]))
	  emit_move_insn (length, GEN_INT (INTVAL (operands[2]) + 1));
      else
	{
	  rtx add = gen_rtx_PLUS (GET_MODE (operands[2]),
				  operands[2], GEN_INT (1));
	  emit_move_insn (length, add);
	}

      /* Move operands[1] into a vector register via vmv.v.x using the same
	 VL we need for the slide.  */
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx ops1[] = {tmp, operands[1]};
      emit_nonvlmax_insn (code_for_pred_broadcast (<MODE>mode),
                           riscv_vector::UNARY_OP, ops1, length);

      /* Slide exactly one element up leaving the tail elements
	 unchanged.  */
      rtx ops2[] = {operands[0], operands[0], tmp, operands[2]};
      riscv_vector::emit_nonvlmax_insn
	(code_for_pred_slide (UNSPEC_VSLIDEUP, <MODE>mode), riscv_vector::BINARY_OP_TUMA, ops2, length);
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
      operands[2] = gen_lowpart (Pmode, operands[2]);
      rtx ops[] = {tmp, operands[1], operands[2]};
      riscv_vector::emit_vlmax_insn
	(code_for_pred_slide (UNSPEC_VSLIDEDOWN, <MODE>mode), riscv_vector::BINARY_OP, ops);
    }

  /* Emit v(f)mv.[xf].s.  */
  emit_insn (gen_pred_extract_first (<MODE>mode, operands[0],
				     tmp ? tmp : operands[1]));
  DONE;
})

;; -------------------------------------------------------------------------
;; This extracts a bit (via QImode) from a bitmask vector.
;; -------------------------------------------------------------------------
(define_expand "vec_extract<mode>qi"
  [(set (match_operand:QI	  0 "register_operand")
     (vec_select:QI
       (match_operand:VB	  1 "register_operand")
       (parallel
	 [(match_operand	  2 "nonmemory_operand")])))]
  "TARGET_VECTOR"
{
  /* Create an empty byte vector and set it to one under mask.  */
  machine_mode qimode = riscv_vector::get_vector_mode
      (QImode, GET_MODE_NUNITS (<MODE>mode)).require ();

  rtx tmp1 = gen_reg_rtx (qimode);
  emit_move_insn (tmp1, gen_const_vec_duplicate (qimode, GEN_INT (0)));
  rtx ones = gen_const_vec_duplicate (qimode, GEN_INT (1));

  rtx ops1[] = {tmp1, tmp1, ones, operands[1]};
  riscv_vector::emit_vlmax_insn (code_for_pred_merge (qimode),
				 riscv_vector::MERGE_OP, ops1);

  /* Slide down the requested byte element.  */
  rtx tmp2 = gen_reg_rtx (qimode);

  rtx ops2[] = {tmp2, tmp1, operands[2]};
  riscv_vector::emit_vlmax_insn
    (code_for_pred_slide (UNSPEC_VSLIDEDOWN, qimode),
     riscv_vector::BINARY_OP, ops2);

  /* Extract it.  */
  emit_insn (gen_pred_extract_first (qimode, operands[0], tmp2));
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Binary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vfadd.vv/vfsub.vv/...
;; - vfadd.vf/vfsub.vf/...
;; -------------------------------------------------------------------------
(define_insn_and_split "<optab><mode>3"
  [(set (match_operand:VF 0 "register_operand")
        (any_float_binop:VF
          (match_operand:VF 1 "register_operand")
          (match_operand:VF 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				    riscv_vector::BINARY_OP_FRM_DYN, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; Includes:
;; - vfmin.vv/vfmax.vv
;; - vfmin.vf/vfmax.vf
;; -------------------------------------------------------------------------
(define_insn_and_split "<optab><mode>3"
  [(set (match_operand:VF 0 "register_operand")
        (any_float_binop_nofrm:VF
          (match_operand:VF 1 "register_operand")
          (match_operand:VF 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				  riscv_vector::BINARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------------
;; ---- [FP] Sign copying
;; -------------------------------------------------------------------------------
;; Includes:
;; - vfsgnj.vv/vfsgnjn.vv
;; - vfsgnj.vf/vfsgnjn.vf
;; -------------------------------------------------------------------------------

;; Leave the pattern like this as to still allow combine to match
;; a negated copysign (see vector.md) before adding the UNSPEC_VPREDICATE later.
(define_insn_and_split "copysign<mode>3"
  [(set (match_operand:VF 0 "register_operand"      "=vd, vd, vr, vr")
    (unspec:VF
     [(match_operand:VF 1 "register_operand"        " vr, vr, vr, vr")
     (match_operand:VF 2 "register_operand"         " vr, vr, vr, vr")] UNSPEC_VCOPYSIGN))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (UNSPEC_VCOPYSIGN, <MODE>mode),
				  riscv_vector::BINARY_OP, operands);
  DONE;
}
  [(set_attr "type" "vfsgnj")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; Includes:
;; - vfsgnjx.vv
;; - vfsgnjx.vf
;; -------------------------------------------------------------------------------
(define_expand "xorsign<mode>3"
  [(match_operand:VF 0 "register_operand")
    (match_operand:VF 1 "register_operand")
    (match_operand:VF 2 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::emit_vlmax_insn (code_for_pred (UNSPEC_VXORSIGN, <MODE>mode),
				  riscv_vector::BINARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Highpart multiplication
;; -------------------------------------------------------------------------
;; Includes:
;; - vmulh.vv
;; - vmulhu.vv
;; -------------------------------------------------------------------------

(define_insn_and_split "smul<mode>3_highpart"
  [(set (match_operand:VFULLI 0 "register_operand")
        (smul_highpart:VFULLI
          (match_operand:VFULLI 1 "register_operand")
          (match_operand:VFULLI 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_mulh (UNSPEC_VMULHS, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, operands);
  DONE;
})

(define_insn_and_split "umul<mode>3_highpart"
  [(set (match_operand:VFULLI 0 "register_operand")
        (umul_highpart:VFULLI
          (match_operand:VFULLI 1 "register_operand")
          (match_operand:VFULLI 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_mulh (UNSPEC_VMULHU, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Conditional unary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vneg/vnot
;; -------------------------------------------------------------------------

(define_expand "cond_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_unop:VI
     (match_operand:VI 2 "register_operand"))
   (match_operand:VI 3 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_<optab><mode> (operands[0], operands[1], operands[2],
					 operands[3],
					 gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
					 const0_rtx));
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_unop:VI
     (match_operand:VI 2 "register_operand"))
   (match_operand:VI 3 "register_operand")
   (match_operand 4 "autovec_length_operand")
   (match_operand 5 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_len_unop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Conditional unary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vfneg/vfabs
;; -------------------------------------------------------------------------

(define_expand "cond_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_unop_nofrm:VF
     (match_operand:VF 2 "register_operand"))
   (match_operand:VF 3 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_<optab><mode> (operands[0], operands[1], operands[2],
					 operands[3],
					 gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
					 const0_rtx));
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_unop_nofrm:VF
     (match_operand:VF 2 "register_operand"))
   (match_operand:VF 3 "register_operand")
   (match_operand 4 "autovec_length_operand")
   (match_operand 5 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_len_unop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Conditional binary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vsra/vsrl/vsll
;; -------------------------------------------------------------------------

(define_expand "cond_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_shift:VI
     (match_operand:VI 2 "register_operand")
     (match_operand:VI 3 "vector_shift_operand"))
   (match_operand:VI 4 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_<optab><mode> (operands[0], operands[1], operands[2],
					 operands[3], operands[4],
					 gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
					 const0_rtx));
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_shift:VI
     (match_operand:VI 2 "register_operand")
     (match_operand:VI 3 "vector_shift_operand"))
   (match_operand:VI 4 "register_operand")
   (match_operand 5 "autovec_length_operand")
   (match_operand 6 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_len_binop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Conditional binary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vadd.vv/vsub.vv/...
;; - vadd.vi/vsub.vi/...
;; -------------------------------------------------------------------------

(define_expand "cond_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_binop_no_shift:VI
     (match_operand:VI 2 "<binop_rhs1_predicate>")
     (match_operand:VI 3 "<binop_rhs2_predicate>"))
   (match_operand:VI 4 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_<optab><mode> (operands[0], operands[1], operands[2],
					 operands[3], operands[4],
					 gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
					 const0_rtx));
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_binop_no_shift:VI
     (match_operand:VI 2 "<binop_rhs1_predicate>")
     (match_operand:VI 3 "<binop_rhs2_predicate>"))
   (match_operand:VI 4 "register_operand")
   (match_operand 5 "autovec_length_operand")
   (match_operand 6 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_len_binop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Conditional binary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vfadd.vv/vfsub.vv/...
;; - vfadd.vf/vfsub.vf/...
;; -------------------------------------------------------------------------

(define_expand "cond_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop:VF
     (match_operand:VF 2 "register_operand")
     (match_operand:VF 3 "register_operand"))
   (match_operand:VF 4 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_<optab><mode> (operands[0], operands[1], operands[2],
					 operands[3], operands[4],
					 gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
					 const0_rtx));
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop:VF
     (match_operand:VF 2 "register_operand")
     (match_operand:VF 3 "register_operand"))
   (match_operand:VF 4 "register_operand")
   (match_operand 5 "autovec_length_operand")
   (match_operand 6 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_len_binop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; Includes:
;; - vfmin.vv/vfmax.vv
;; - vfmin.vf/vfmax.vf
;; -------------------------------------------------------------------------

(define_expand "cond_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop_nofrm:VF
     (match_operand:VF 2 "register_operand")
     (match_operand:VF 3 "register_operand"))
   (match_operand:VF 4 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_<optab><mode> (operands[0], operands[1], operands[2],
					 operands[3], operands[4],
					 gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
					 const0_rtx));
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop_nofrm:VF
     (match_operand:VF 2 "register_operand")
     (match_operand:VF 3 "register_operand"))
   (match_operand:VF 4 "register_operand")
   (match_operand 5 "autovec_length_operand")
   (match_operand 6 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_len_binop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Conditional ternary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vmacc/...
;; -------------------------------------------------------------------------

(define_expand "cond_fma<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VI 2 "register_operand")
   (match_operand:VI 3 "register_operand")
   (match_operand:VI 4 "register_operand")
   (match_operand:VI 5 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_fma<mode> (operands[0], operands[1], operands[2],
				     operands[3], operands[4], operands[5],
				     gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
				     const0_rtx));
  DONE;
})

(define_expand "cond_len_fma<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VI 2 "register_operand")
   (match_operand:VI 3 "register_operand")
   (match_operand:VI 4 "register_operand")
   (match_operand:VI 5 "register_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_plus (<MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fnma<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VI 2 "register_operand")
   (match_operand:VI 3 "register_operand")
   (match_operand:VI 4 "register_operand")
   (match_operand:VI 5 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_fnma<mode> (operands[0], operands[1], operands[2],
				      operands[3], operands[4], operands[5],
				      gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
				      const0_rtx));
  DONE;
})

(define_expand "cond_len_fnma<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VI 2 "register_operand")
   (match_operand:VI 3 "register_operand")
   (match_operand:VI 4 "register_operand")
   (match_operand:VI 5 "register_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_minus_mul (<MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Conditional ternary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vfmacc/...
;; -------------------------------------------------------------------------

(define_expand "cond_fma<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_fma<mode> (operands[0], operands[1], operands[2],
				     operands[3], operands[4], operands[5],
				     gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
				     const0_rtx));
  DONE;
})

(define_expand "cond_len_fma<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul (PLUS, <MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fnma<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_fnma<mode> (operands[0], operands[1], operands[2],
				      operands[3], operands[4], operands[5],
				      gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
				      const0_rtx));
  DONE;
})

(define_expand "cond_len_fnma<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_neg (PLUS, <MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fms<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_fms<mode> (operands[0], operands[1], operands[2],
				     operands[3], operands[4], operands[5],
				     gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
				     const0_rtx));
  DONE;
})

(define_expand "cond_len_fms<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul (MINUS, <MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fnms<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")]
  "TARGET_VECTOR"
{
  /* Normalize into cond_len_* operations.  */
  emit_insn (gen_cond_len_fnms<mode> (operands[0], operands[1], operands[2],
				      operands[3], operands[4], operands[5],
				      gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode),
				      const0_rtx));
  DONE;
})

(define_expand "cond_len_fnms<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "register_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_neg (MINUS, <MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

;; =========================================================================
;; == Reductions
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Tree reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - vredsum.vs
;; - vredmaxu.vs
;; - vredmax.vs
;; - vredminu.vs
;; - vredmin.vs
;; - vredand.vs
;; - vredor.vs
;; - vredxor.vs
;; -------------------------------------------------------------------------

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (PLUS, operands, CONST0_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  int prec = GET_MODE_PRECISION (<VEL>mode);
  rtx min = immed_wide_int_const (wi::min_value (prec, SIGNED), <VEL>mode);
  riscv_vector::expand_reduction (SMAX, operands, min);
  DONE;
})

(define_expand "reduc_umax_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (UMAX, operands, CONST0_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  int prec = GET_MODE_PRECISION (<VEL>mode);
  rtx max = immed_wide_int_const (wi::max_value (prec, SIGNED), <VEL>mode);
  riscv_vector::expand_reduction (SMIN, operands, max);
  DONE;
})

(define_expand "reduc_umin_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  int prec = GET_MODE_PRECISION (<VEL>mode);
  rtx max = immed_wide_int_const (wi::max_value (prec, UNSIGNED), <VEL>mode);
  riscv_vector::expand_reduction (UMIN, operands, max);
  DONE;
})

(define_expand "reduc_and_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (AND, operands, CONSTM1_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_ior_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (IOR, operands, CONST0_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_xor_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (XOR, operands, CONST0_RTX (<VEL>mode));
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Tree reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfredusum.vs
;; - vfredmax.vs
;; - vfredmin.vs
;; -------------------------------------------------------------------------

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VF 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (PLUS, operands, CONST0_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VF 1 "register_operand")]
  "TARGET_VECTOR"
{
  REAL_VALUE_TYPE rv;
  real_inf (&rv, true);
  rtx f = const_double_from_real_value (rv, <VEL>mode);
  riscv_vector::expand_reduction (SMAX, operands, f);
  DONE;
})

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:VF 1 "register_operand")]
  "TARGET_VECTOR"
{
  REAL_VALUE_TYPE rv;
  real_inf (&rv, false);
  rtx f = const_double_from_real_value (rv, <VEL>mode);
  riscv_vector::expand_reduction (SMIN, operands, f);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Left-to-right reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfredosum.vs
;; -------------------------------------------------------------------------

;; Unpredicated in-order FP reductions.
(define_expand "fold_left_plus_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:<VEL> 1 "register_operand")
   (match_operand:VF 2 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (PLUS, operands,
				  operands[1],
				  riscv_vector::reduction_type::FOLD_LEFT);
  DONE;
})

;; Predicated in-order FP reductions.
(define_expand "mask_len_fold_left_plus_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:<VEL> 1 "register_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:<VM> 3 "vector_mask_operand")
   (match_operand 4 "autovec_length_operand")
   (match_operand 5 "const_0_operand")]
  "TARGET_VECTOR"
{
  if (rtx_equal_p (operands[4], const0_rtx))
    emit_move_insn (operands[0], operands[1]);
  else
    riscv_vector::expand_reduction (PLUS, operands,
				    operands[1],
				    riscv_vector::reduction_type::MASK_LEN_FOLD_LEFT);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Extract active element
;; -------------------------------------------------------------------------
;; Includes:
;; - vcompress.vm
;; - vcpop.m
;; - vslidedown.vx
;; - vmv.x.s
;; - vfmv.f.s
;; -------------------------------------------------------------------------

(define_expand "len_fold_extract_last_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:<VEL> 1 "register_operand")
   (match_operand:<VM> 2 "register_operand")
   (match_operand:V 3 "register_operand")
   (match_operand 4 "autovec_length_operand")
   (match_operand 5 "const_0_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_fold_extract_last (operands);
    DONE;
  })

;; -------------------------------------------------------------------------
;; ---- [INT] Average.
;; -------------------------------------------------------------------------
;; Implements the following "average" patterns:
;; floor:
;;  op[0] = (narrow) ((wide) op[1] + (wide) op[2]) >> 1;
;; ceil:
;;  op[0] = (narrow) ((wide) op[1] + (wide) op[2] + 1)) >> 1;
;; -------------------------------------------------------------------------

(define_expand "<u>avg<v_double_trunc>3_floor"
 [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand")
   (truncate:<V_DOUBLE_TRUNC>
    (<ext_to_rshift>:VWEXTI
     (plus:VWEXTI
      (any_extend:VWEXTI
       (match_operand:<V_DOUBLE_TRUNC> 1 "register_operand"))
      (any_extend:VWEXTI
       (match_operand:<V_DOUBLE_TRUNC> 2 "register_operand"))))))]
  "TARGET_VECTOR"
{
  /* First emit a widening addition.  */
  rtx tmp1 = gen_reg_rtx (<MODE>mode);
  rtx ops1[] = {tmp1, operands[1], operands[2]};
  insn_code icode = code_for_pred_dual_widen (PLUS, <CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops1);

  /* Then a narrowing shift.  */
  rtx ops2[] = {operands[0], tmp1, const1_rtx};
  icode = code_for_pred_narrow_scalar (<EXT_TO_RSHIFT>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops2);
  DONE;
})

(define_expand "<u>avg<v_double_trunc>3_ceil"
 [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand")
   (truncate:<V_DOUBLE_TRUNC>
    (<ext_to_rshift>:VWEXTI
     (plus:VWEXTI
      (plus:VWEXTI
       (any_extend:VWEXTI
	(match_operand:<V_DOUBLE_TRUNC> 1 "register_operand"))
       (any_extend:VWEXTI
	(match_operand:<V_DOUBLE_TRUNC> 2 "register_operand")))
      (const_int 1)))))]
  "TARGET_VECTOR"
{
  /* First emit a widening addition.  */
  rtx tmp1 = gen_reg_rtx (<MODE>mode);
  rtx ops1[] = {tmp1, operands[1], operands[2]};
  insn_code icode = code_for_pred_dual_widen (PLUS, <CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops1);

  /* Then add 1.  */
  rtx tmp2 = gen_reg_rtx (<MODE>mode);
  rtx ops2[] = {tmp2, tmp1, const1_rtx};
  icode = code_for_pred_scalar (PLUS, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops2);

  /* Finally, a narrowing shift.  */
  rtx ops3[] = {operands[0], tmp2, const1_rtx};
  icode = code_for_pred_narrow_scalar (<EXT_TO_RSHIFT>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops3);
  DONE;
})
