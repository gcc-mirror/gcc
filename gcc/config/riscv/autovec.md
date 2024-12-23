;; Machine description for auto-vectorization using RVV for GNU compiler.
;; Copyright (C) 2023-2025 Free Software Foundation, Inc.
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
   (match_operand:V 3 "maskload_else_operand")
   (match_operand 4 "autovec_length_operand")
   (match_operand 5 "const_0_operand")]
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
   (match_operand 6 "maskload_else_operand")
   (match_operand 7 "autovec_length_operand")
   (match_operand 8 "const_0_operand")]
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO64I:MODE>mode)"
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
   (match_operand 6 "maskload_else_operand")
   (match_operand 7 "autovec_length_operand")
   (match_operand 8 "const_0_operand")]
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO32I:MODE>mode)"
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
   (match_operand 6 "maskload_else_operand")
   (match_operand 7 "autovec_length_operand")
   (match_operand 8 "const_0_operand")]
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO16I:MODE>mode)"
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
   (match_operand 6 "maskload_else_operand")
   (match_operand 7 "autovec_length_operand")
   (match_operand 8 "const_0_operand")]
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO8I:MODE>mode)"
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
   (match_operand 6 "maskload_else_operand")
   (match_operand 7 "autovec_length_operand")
   (match_operand 8 "const_0_operand")]
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO4I:MODE>mode)"
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
   (match_operand 6 "maskload_else_operand")
   (match_operand 7 "autovec_length_operand")
   (match_operand 8 "const_0_operand")]
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO2I:MODE>mode)"
{
  riscv_vector::expand_gather_scatter (operands, true);
  DONE;
})

;; When SEW = 8 and LMUL = 8, we can't find any index mode with
;; larger SEW. Since RVV indexed load/store support zero extend
;; implicitly and not support scaling, we should only allow
;; operands[3] and operands[4] to be const_1_operand.
(define_expand "mask_len_gather_load<mode><mode>"
  [(match_operand:RATIO1 0 "register_operand")
   (match_operand 1 "pmode_reg_or_0_operand")
   (match_operand:RATIO1 2 "register_operand")
   (match_operand 3 "<gs_extension>")
   (match_operand 4 "<gs_scale>")
   (match_operand:<VM> 5 "vector_mask_operand")
   (match_operand 6 "maskload_else_operand")
   (match_operand 7 "autovec_length_operand")
   (match_operand 8 "const_0_operand")]
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
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO64I:MODE>mode)"
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
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO32I:MODE>mode)"
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
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO16I:MODE>mode)"
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
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO8I:MODE>mode)"
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
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO4I:MODE>mode)"
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
  "TARGET_VECTOR && riscv_vector::gather_scatter_valid_offset_p (<RATIO2I:MODE>mode)"
{
  riscv_vector::expand_gather_scatter (operands, false);
  DONE;
})

;; When SEW = 8 and LMUL = 8, we can't find any index mode with
;; larger SEW. Since RVV indexed load/store support zero extend
;; implicitly and not support scaling, we should only allow
;; operands[3] and operands[4] to be const_1_operand.
(define_expand "mask_len_scatter_store<mode><mode>"
  [(match_operand 0 "pmode_reg_or_0_operand")
   (match_operand:RATIO1 1 "register_operand")
   (match_operand 2 "<gs_extension>")
   (match_operand 3 "<gs_scale>")
   (match_operand:RATIO1 4 "register_operand")
   (match_operand:<VM> 5 "vector_mask_operand")
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
   (match_operand 3 "maskload_else_operand")
   (match_operand 4 "autovec_length_operand")
   (match_operand 5 "const_0_operand")]
  "TARGET_VECTOR_AUTOVEC_SEGMENT"
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
  "TARGET_VECTOR_AUTOVEC_SEGMENT"
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

(define_expand "vec_series<mode>"
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
  [(match_operand:V_VLS 0 "register_operand")
   (match_operand:V_VLS 1 "register_operand")
   (match_operand:V_VLS 2 "register_operand")
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
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_init (operands[0], operands[1]);
    DONE;
  }
)

;; We split RVV floating-point because we are going to
;; use vfslide1down/vfslide1up for FP16 which need TARGET_ZVFH.
(define_expand "vec_init<mode><vel>"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_init (operands[0], operands[1]);
    DONE;
  }
)

;; Provide a vec_init for mask registers by initializing
;; a QImode vector and comparing it against 0.
(define_expand "vec_init<mode>qi"
  [(match_operand:VB 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_VECTOR"
  {
    machine_mode qimode = riscv_vector::get_vector_mode
	(QImode, GET_MODE_NUNITS (<MODE>mode)).require ();
    rtx tmp = gen_reg_rtx (qimode);
    riscv_vector::expand_vec_init (tmp, operands[1]);
    riscv_vector::expand_vec_cmp (operands[0], NE, tmp, CONST0_RTX (qimode));
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
  [(set (match_operand:V_VLSI 0 "register_operand")
    (any_int_binop_no_shift:V_VLSI
     (match_operand:V_VLSI 1 "<binop_rhs1_predicate>")
     (match_operand:V_VLSI 2 "<binop_rhs2_predicate>")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				 riscv_vector::BINARY_OP, operands);
  DONE;
}
[(set_attr "type" "vialu")])

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
  [(set (match_operand:VB_VLS 0 "register_operand"                 "=vr")
	(any_bitwise:VB_VLS (match_operand:VB_VLS 1 "register_operand" " vr")
			    (match_operand:VB_VLS 2 "register_operand" " vr")))]
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
  [(set (match_operand:VB_VLS 0 "register_operand"         "=vr")
	(not:VB_VLS (match_operand:VB_VLS 1 "register_operand" " vr")))]
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

(define_insn_and_split "vcond_mask_<mode><vm>"
  [(set (match_operand:V_VLS 0 "register_operand")
        (if_then_else:V_VLS
          (match_operand:<VM> 3 "register_operand")
          (match_operand:V_VLS 1 "nonmemory_operand")
          (match_operand:V_VLS 2 "register_operand")))]
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
  [(set_attr "type" "vector")]
)

(define_expand "vcond_mask_len_<mode>"
  [(match_operand:V 0 "register_operand")
    (match_operand:<VM> 1 "nonmemory_operand")
    (match_operand:V 2 "nonmemory_operand")
    (match_operand:V 3 "autovec_else_operand")
    (match_operand 4 "autovec_length_operand")
    (match_operand 5 "const_0_operand")]
  "TARGET_VECTOR"
  {
    if (satisfies_constraint_Wc1 (operands[1]))
      riscv_vector::expand_cond_len_unop (code_for_pred_mov (<MODE>mode),
					  operands);
    else
      {
	/* The order of then and else is opposite to pred_merge.  */
	rtx ops[] = {operands[0], operands[3], operands[3], operands[2],
		     operands[1]};
	riscv_vector::emit_nonvlmax_insn (code_for_pred_merge (<MODE>mode),
					  riscv_vector::MERGE_OP_TU,
					  ops, operands[4]);
      }
    DONE;
  }
  [(set_attr "type" "vector")]
)

;; -------------------------------------------------------------------------
;; ---- [BOOL] Select based on masks
;; -------------------------------------------------------------------------
;; Includes merging patterns for:
;; - vmand.mm
;; - vmor.mm
;; - vmnot.m
;; -------------------------------------------------------------------------

(define_expand "vcond_mask_<mode><mode>"
  [(match_operand:VB_VLS 0 "register_operand")
   (match_operand:VB_VLS 1 "register_operand")
   (match_operand:VB_VLS 2 "register_operand")
   (match_operand:VB_VLS 3 "register_operand")]
  "TARGET_VECTOR"
  {
    /* mask1 = operands[3] & operands[1].  */
    rtx mask1 = expand_binop (<MODE>mode, and_optab, operands[1],
			      operands[3], NULL_RTX, 0,
			      OPTAB_DIRECT);
    /* mask2 = ~operands[3] & operands[2].  */
    rtx inverse = expand_unop (<MODE>mode, one_cmpl_optab, operands[3],
			       NULL_RTX, 0);
    rtx mask2 = expand_binop (<MODE>mode, and_optab, operands[2],
			      inverse, NULL_RTX, 0,
			      OPTAB_DIRECT);
    /* result = mask1 | mask2.  */
    rtx result = expand_binop (<MODE>mode, ior_optab, mask1,
			       mask2, NULL_RTX, 0,
			       OPTAB_DIRECT);
    emit_move_insn (operands[0], result);
    DONE;
  })

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Comparisons
;; -------------------------------------------------------------------------
;; Includes:
;; - vms<eq/ne/ltu/lt/leu/le/gtu/gt>.<vv/vx/vi>
;; -------------------------------------------------------------------------

(define_expand "vec_cmp<mode><vm>"
  [(set (match_operand:<VM> 0 "register_operand")
	(match_operator:<VM> 1 "comparison_operator"
	  [(match_operand:V_VLSI 2 "register_operand")
	   (match_operand:V_VLSI 3 "nonmemory_operand")]))]
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
	  [(match_operand:V_VLSI 2 "register_operand")
	   (match_operand:V_VLSI 3 "nonmemory_operand")]))]
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
	  [(match_operand:V_VLSF 2 "register_operand")
	   (match_operand:V_VLSF 3 "register_operand")]))]
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
}
[(set_attr "type" "vext")])

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
}
[(set_attr "type" "vext")])

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
;; Truncation to a mode whose inner mode size is an eighth of mode's.
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
	  (match_operand:V_VLSF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
}
[(set_attr "type" "vfcvtftoi")])

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfcvt.f.xu.v
;; - vfcvt.f.x.v
;; -------------------------------------------------------------------------

(define_insn_and_split "<float_cvt><vconvert><mode>2"
  [(set (match_operand:V_VLSF 0 "register_operand")
	(any_float:V_VLSF
	  (match_operand:<VCONVERT> 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP_FRM_DYN, operands);
  DONE;
}
[(set_attr "type" "vfcvtitof")])

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
}
[(set_attr "type" "vfwcvtftoi")])

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Widening Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfwcvt.f.xu.v
;; - vfwcvt.f.x.v
;; -------------------------------------------------------------------------
(define_insn_and_split "<float_cvt><vnconvert><mode>2"
  [(set (match_operand:V_VLSF 0 "register_operand")
	(any_float:V_VLSF
	  (match_operand:<VNCONVERT> 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_widen (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
}
[(set_attr "type" "vfwcvtitof")])

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
	  (match_operand:V_VLSF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_narrow (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
}
[(set_attr "type" "vfncvtftoi")])

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
}
[(set_attr "type" "vfncvtitof")])

;; This operation can be performed in the loop vectorizer but unfortunately
;; not applicable for now. We can remove this pattern after loop vectorizer
;; is able to take care of INT64 to FP16 conversion.
(define_expand "<float_cvt><mode><vnnconvert>2"
  [(set (match_operand:<VNNCONVERT>  0 "register_operand")
	(any_float:<VNNCONVERT>
	  (match_operand:VWWCONVERTI 1 "register_operand")))]
  "TARGET_VECTOR && TARGET_ZVFH && can_create_pseudo_p () && !flag_trapping_math"
  {
    rtx single = gen_reg_rtx (<VNCONVERT>mode); /* Get vector SF mode.  */

    /* Step-1, INT64 => FP32.  */
    emit_insn (gen_<float_cvt><mode><vnconvert>2 (single, operands[1]));
    /* Step-2, FP32 => FP16.  */
    emit_insn (gen_trunc<vnconvert><vnnconvert>2 (operands[0], single));

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
  [(set (match_operand:V_VLSI 0 "register_operand")
    (any_int_unop:V_VLSI
     (match_operand:V_VLSI 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
}
[(set_attr "type" "vialu")])

;; -------------------------------------------------------------------------------
;; - [INT] ABS expansion to vneg and vmax.
;; -------------------------------------------------------------------------------

(define_expand "abs<mode>2"
  [(set (match_operand:V_VLSI 0 "register_operand")
    (smax:V_VLSI
     (match_dup 0)
     (neg:V_VLSI
       (match_operand:V_VLSI 1 "register_operand"))))]
  "TARGET_VECTOR"
{
  DONE;
})

;; -------------------------------------------------------------------------------
;; ---- [FP] Unary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - vfneg.v/vfabs.v
;; -------------------------------------------------------------------------------
(define_insn_and_split "<optab><mode>2"
  [(set (match_operand:V_VLSF 0 "register_operand")
    (any_float_unop_nofrm:V_VLSF
     (match_operand:V_VLSF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP, operands);
  DONE;
}
[(set_attr "type" "vector")])

;; -------------------------------------------------------------------------------
;; - [FP] Square root
;; -------------------------------------------------------------------------------
;; Includes:
;; - vfsqrt.v
;; -------------------------------------------------------------------------------
(define_insn_and_split "<optab><mode>2"
  [(set (match_operand:V_VLSF 0 "register_operand")
    (any_float_unop:V_VLSF
     (match_operand:V_VLSF 1 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::UNARY_OP_FRM_DYN, operands);
  DONE;
}
[(set_attr "type" "vfsqrt")])

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

(define_insn_and_split "fma<mode>4"
  [(set (match_operand:V_VLSI 0 "register_operand")
	(plus:V_VLSI
	  (mult:V_VLSI
	    (match_operand:V_VLSI 1 "register_operand")
	    (match_operand:V_VLSI 2 "register_operand"))
	  (match_operand:V_VLSI 3 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3],
                 RVV_VUNDEF(<MODE>mode)};
    riscv_vector::emit_vlmax_insn (code_for_pred_mul_plus (<MODE>mode),
				   riscv_vector::TERNARY_OP, ops);
    DONE;
  }
  [(set_attr "type" "vector")])

;; -------------------------------------------------------------------------
;; ---- [INT] VNMSAC and VNMSUB
;; -------------------------------------------------------------------------
;; Includes:
;; - vnmsac
;; - vnmsub
;; -------------------------------------------------------------------------

(define_insn_and_split "fnma<mode>4"
  [(set (match_operand:V_VLSI 0 "register_operand")
        (minus:V_VLSI
          (match_operand:V_VLSI 3 "register_operand")
          (mult:V_VLSI
            (match_operand:V_VLSI 1 "register_operand")
            (match_operand:V_VLSI 2 "register_operand"))))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3],
                 RVV_VUNDEF(<MODE>mode)};
    riscv_vector::emit_vlmax_insn (code_for_pred_minus_mul (<MODE>mode),
				   riscv_vector::TERNARY_OP, ops);
    DONE;
  }
  [(set_attr "type" "vector")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFMACC and VFMADD
;; -------------------------------------------------------------------------
;; Includes:
;; - vfmacc
;; - vfmadd
;; -------------------------------------------------------------------------

(define_insn_and_split "fma<mode>4"
  [(set (match_operand:V_VLSF 0 "register_operand")
        (plus:V_VLSF
	  (mult:V_VLSF
	    (match_operand:V_VLSF 1 "register_operand")
	    (match_operand:V_VLSF 2 "register_operand"))
	  (match_operand:V_VLSF 3 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3],
                 RVV_VUNDEF(<MODE>mode)};
    riscv_vector::emit_vlmax_insn (code_for_pred_mul (PLUS, <MODE>mode),
				   riscv_vector::TERNARY_OP_FRM_DYN, ops);
    DONE;
  }
  [(set_attr "type" "vector")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFNMSAC and VFNMSUB
;; -------------------------------------------------------------------------
;; Includes:
;; - vfnmsac
;; - vfnmsub
;; -------------------------------------------------------------------------

(define_insn_and_split "fnma<mode>4"
  [(set (match_operand:V_VLSF 0 "register_operand")
        (minus:V_VLSF
          (match_operand:V_VLSF 3 "register_operand")
	  (mult:V_VLSF
	    (match_operand:V_VLSF 1 "register_operand")
	    (match_operand:V_VLSF 2 "register_operand"))))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3],
                 RVV_VUNDEF(<MODE>mode)};
    riscv_vector::emit_vlmax_insn (code_for_pred_mul_neg (PLUS, <MODE>mode),
				   riscv_vector::TERNARY_OP_FRM_DYN, ops);
    DONE;
  }
  [(set_attr "type" "vector")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFMSAC and VFMSUB
;; -------------------------------------------------------------------------
;; Includes:
;; - vfmsac
;; - vfmsub
;; -------------------------------------------------------------------------

(define_insn_and_split "fms<mode>4"
  [(set (match_operand:V_VLSF 0 "register_operand")
        (minus:V_VLSF
	  (mult:V_VLSF
	    (match_operand:V_VLSF 1 "register_operand")
	    (match_operand:V_VLSF 2 "register_operand"))
	  (match_operand:V_VLSF 3 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3],
                 RVV_VUNDEF(<MODE>mode)};
    riscv_vector::emit_vlmax_insn (code_for_pred_mul (MINUS, <MODE>mode),
				   riscv_vector::TERNARY_OP_FRM_DYN, ops);
    DONE;
  }
  [(set_attr "type" "vector")])

;; -------------------------------------------------------------------------
;; ---- [FP] VFNMACC and VFNMADD
;; -------------------------------------------------------------------------
;; Includes:
;; - vfnmacc
;; - vfnmadd
;; -------------------------------------------------------------------------

(define_insn_and_split "fnms<mode>4"
  [(set (match_operand:V_VLSF 0 "register_operand")
        (minus:V_VLSF
          (neg:V_VLSF
	    (mult:V_VLSF
	      (match_operand:V_VLSF 1 "register_operand")
	      (match_operand:V_VLSF 2 "register_operand")))
	  (match_operand:V_VLSF 3 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    rtx ops[] = {operands[0], operands[1], operands[2], operands[3],
                 RVV_VUNDEF(<MODE>mode)};
    riscv_vector::emit_vlmax_insn (code_for_pred_mul_neg (MINUS, <MODE>mode),
				   riscv_vector::TERNARY_OP_FRM_DYN, ops);
    DONE;
  }
  [(set_attr "type" "vector")])

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
  [(match_operand:V_VLS 0 "register_operand")
   (match_operand:<VEL> 1 "register_operand")
   (match_operand       2 "nonmemory_operand")]
  "TARGET_VECTOR"
{
  /* If we set the first element, emit an v(f)mv.s.[xf].  */
  if (operands[2] == const0_rtx)
    {
      rtx ops[] = {operands[0], operands[0], operands[1]};
      riscv_vector::emit_nonvlmax_insn (code_for_pred_broadcast (<MODE>mode),
					riscv_vector::SCALAR_MOVE_MERGED_OP_TU,
					ops, CONST1_RTX (Pmode));
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
       (match_operand:V_VLS_ZVFH  1 "register_operand")
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
      /* Properly convert a poly_int value and put the result into a
	 register.  */
      if (CONST_POLY_INT_P (operands[2]))
	{
	  rtx pos = gen_reg_rtx (Pmode);
	  riscv_legitimize_poly_move (Pmode, pos, gen_reg_rtx (Pmode),
				      operands[2]);
	  operands[2] = pos;
	}

    /* Emit the slide down to index 0 in a new vector.  */
    tmp = gen_reg_rtx (<MODE>mode);
    operands[2] = gen_lowpart (Pmode, operands[2]);
    rtx ops[] = {tmp, operands[1], operands[2]};
    riscv_vector::emit_vlmax_insn
      (code_for_pred_slide (UNSPEC_VSLIDEDOWN, <MODE>mode),
       riscv_vector::BINARY_OP, ops);
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
       (match_operand:VB_VLS	  1 "register_operand")
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

  /* Extract from it.  */
  riscv_vector::emit_vec_extract (operands[0], tmp1, operands[2]);
  DONE;
})

;; Same for a BImode but still return a QImode.
(define_expand "vec_extract<mode>bi"
  [(set (match_operand:QI	  0 "register_operand")
     (vec_select:QI
       (match_operand:VB_VLS	  1 "register_operand")
       (parallel
	 [(match_operand	  2 "nonmemory_operand")])))]
  "TARGET_VECTOR"
{
  emit_insn (gen_vec_extract<mode>qi (operands[0], operands[1], operands[2]));
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Extract a vector from a vector.
;; -------------------------------------------------------------------------
;; TODO: This can be extended to allow basically any extract mode.
;; For now this helps optimize VLS subregs like (subreg:V2DI (reg:V4DI) 16)
;; that would otherwise need to go via memory.

(define_expand "vec_extract<mode><vls_half>"
  [(set (match_operand:<VLS_HALF>	 0 "nonimmediate_operand")
     (vec_select:<VLS_HALF>
       (match_operand:VLS_HAS_HALF	 1 "register_operand")
       (parallel
	 [(match_operand		 2 "immediate_operand")])))]
  "TARGET_VECTOR"
{
  int sz = GET_MODE_NUNITS (<VLS_HALF>mode).to_constant ();
  int part = INTVAL (operands[2]);

  rtx start = GEN_INT (part * sz);
  rtx tmp = operands[1];

  if (part != 0)
    {
      tmp = gen_reg_rtx (<MODE>mode);

      rtx ops[] = {tmp, operands[1], start};
      riscv_vector::emit_vlmax_insn
	(code_for_pred_slide (UNSPEC_VSLIDEDOWN, <MODE>mode),
	 riscv_vector::BINARY_OP, ops);
    }

  emit_move_insn (operands[0], gen_lowpart (<VLS_HALF>mode, tmp));
  DONE;
})

(define_expand "vec_extract<mode><vls_quarter>"
  [(set (match_operand:<VLS_QUARTER>	 0 "nonimmediate_operand")
     (vec_select:<VLS_QUARTER>
       (match_operand:VLS_HAS_QUARTER	 1 "register_operand")
       (parallel
	 [(match_operand		 2 "immediate_operand")])))]
  "TARGET_VECTOR"
{
  int sz = GET_MODE_NUNITS (<VLS_QUARTER>mode).to_constant ();
  int part = INTVAL (operands[2]);

  rtx start = GEN_INT (part * sz);
  rtx tmp = operands[1];

  if (part != 0)
    {
      tmp = gen_reg_rtx (<MODE>mode);

      rtx ops[] = {tmp, operands[1], start};
      riscv_vector::emit_vlmax_insn
	(code_for_pred_slide (UNSPEC_VSLIDEDOWN, <MODE>mode),
	 riscv_vector::BINARY_OP, ops);
    }

  emit_move_insn (operands[0], gen_lowpart (<VLS_QUARTER>mode, tmp));
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
  [(set (match_operand:V_VLSF 0 "register_operand")
        (any_float_binop:V_VLSF
          (match_operand:V_VLSF 1 "register_operand")
          (match_operand:V_VLSF 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				    riscv_vector::BINARY_OP_FRM_DYN, operands);
  DONE;
}
[(set_attr "type" "vfalu")])

;; -------------------------------------------------------------------------
;; Includes:
;; - vfmin.vv/vfmax.vv
;; - vfmin.vf/vfmax.vf
;; -------------------------------------------------------------------------
(define_insn_and_split "<optab><mode>3"
  [(set (match_operand:V_VLSF 0 "register_operand")
        (any_float_binop_nofrm:V_VLSF
          (match_operand:V_VLSF 1 "register_operand")
          (match_operand:V_VLSF 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<CODE>, <MODE>mode),
				  riscv_vector::BINARY_OP, operands);
  DONE;
}
[(set_attr "type" "vfminmax")])

(define_insn_and_split "<ieee_fmaxmin_op><mode>3"
  [(set (match_operand:V_VLSF 0 "register_operand")
      (unspec:V_VLSF
	[(match_operand:V_VLSF 1 "register_operand")
	 (match_operand:V_VLSF 2 "register_operand")] UNSPEC_VFMAXMIN))]
  "TARGET_VECTOR && !HONOR_SNANS (<MODE>mode) && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::emit_vlmax_insn (code_for_pred (<IEEE_FMAXMIN_OP>, <MODE>mode),
				 riscv_vector::BINARY_OP, operands);
  DONE;
}
[(set_attr "type" "vfminmax")])

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
  [(set (match_operand:V_VLSF 0 "register_operand"  "=vd, vd, vr, vr")
    (unspec:V_VLSF
     [(match_operand:V_VLSF 1 "register_operand"        " vr, vr, vr, vr")
     (match_operand:V_VLSF 2 "register_operand"         " vr, vr, vr, vr")] UNSPEC_VCOPYSIGN))]
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
  [(match_operand:V_VLSF 0 "register_operand")
    (match_operand:V_VLSF 1 "register_operand")
    (match_operand:V_VLSF 2 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::emit_vlmax_insn (code_for_pred (UNSPEC_VXORSIGN, <MODE>mode),
				  riscv_vector::BINARY_OP, operands);
  DONE;
})

;; -------------------------------------------------------------------------------
;; - [INT] POPCOUNT, CTZ and CLZ.
;; -------------------------------------------------------------------------------

(define_expand "popcount<mode>2"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  if (!TARGET_ZVBB)
    riscv_vector::expand_popcount (operands);
  else
    {
      riscv_vector::emit_vlmax_insn (code_for_pred_v (POPCOUNT, <MODE>mode),
				     riscv_vector::CPOP_OP, operands);
    }
  DONE;
})

(define_expand "ctz<mode>2"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_ZVBB"
  {
    riscv_vector::emit_vlmax_insn (code_for_pred_v (CTZ, <MODE>mode),
				   riscv_vector::CPOP_OP, operands);
    DONE;
})

(define_expand "clz<mode>2"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_ZVBB"
  {
    riscv_vector::emit_vlmax_insn (code_for_pred_v (CLZ, <MODE>mode),
				   riscv_vector::CPOP_OP, operands);
    DONE;
})


;; -------------------------------------------------------------------------
;; ---- [INT] Highpart multiplication
;; -------------------------------------------------------------------------
;; Includes:
;; - vmulh.vv
;; - vmulhu.vv
;; -------------------------------------------------------------------------

(define_insn_and_split "<mulh_table><mode>3_highpart"
  [(set (match_operand:VFULLI 0 "register_operand")
        (mulh:VFULLI
          (match_operand:VFULLI 1 "register_operand")
          (match_operand:VFULLI 2 "register_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  insn_code icode = code_for_pred_mulh (<MULH_UNSPEC>, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, operands);
  DONE;
}
[(set_attr "type" "vimul")])

;; -------------------------------------------------------------------------
;; ---- [INT] Conditional unary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vneg/vnot
;; -------------------------------------------------------------------------

(define_expand "cond_<optab><mode>"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_unop:V_VLSI
     (match_operand:V_VLSI 2 "register_operand"))
   (match_operand:V_VLSI 3 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_unop (icode, operands);
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_unop:VI
     (match_operand:VI 2 "register_operand"))
   (match_operand:VI 3 "autovec_else_operand")
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
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_unop_nofrm:V_VLSF
     (match_operand:V_VLSF 2 "register_operand"))
   (match_operand:V_VLSF 3 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_unop (icode, operands);
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_unop_nofrm:VF
     (match_operand:VF 2 "register_operand"))
   (match_operand:VF 3 "autovec_else_operand")
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
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_shift:V_VLSI
     (match_operand:V_VLSI 2 "register_operand")
     (match_operand:V_VLSI 3 "vector_shift_operand"))
   (match_operand:V_VLSI 4 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_binop (icode, operands);
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_shift:VI
     (match_operand:VI 2 "register_operand")
     (match_operand:VI 3 "vector_shift_operand"))
   (match_operand:VI 4 "autovec_else_operand")
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
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_binop_no_shift:V_VLSI
     (match_operand:V_VLSI 2 "<binop_rhs1_predicate>")
     (match_operand:V_VLSI 3 "<binop_rhs2_predicate>"))
   (match_operand:V_VLSI 4 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_binop (icode, operands);
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_int_binop_no_shift:VI
     (match_operand:VI 2 "<binop_rhs1_predicate>")
     (match_operand:VI 3 "<binop_rhs2_predicate>"))
   (match_operand:VI 4 "autovec_else_operand")
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
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop:V_VLSF
     (match_operand:V_VLSF 2 "register_operand")
     (match_operand:V_VLSF 3 "register_operand"))
   (match_operand:V_VLSF 4 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_binop (icode, operands);
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop:VF
     (match_operand:VF 2 "register_operand")
     (match_operand:VF 3 "register_operand"))
   (match_operand:VF 4 "autovec_else_operand")
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
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop_nofrm:V_VLSF
     (match_operand:V_VLSF 2 "register_operand")
     (match_operand:V_VLSF 3 "register_operand"))
   (match_operand:V_VLSF 4 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_binop (icode, operands);
  DONE;
})

(define_expand "cond_len_<optab><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (any_float_binop_nofrm:VF
     (match_operand:VF 2 "register_operand")
     (match_operand:VF 3 "register_operand"))
   (match_operand:VF 4 "autovec_else_operand")
   (match_operand 5 "autovec_length_operand")
   (match_operand 6 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (<CODE>, <MODE>mode);
  riscv_vector::expand_cond_len_binop (icode, operands);
  DONE;
})

(define_expand "cond_<ieee_fmaxmin_op><mode>"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (unspec:V_VLSF
     [(match_operand:V_VLSF 2 "register_operand")
      (match_operand:V_VLSF 3 "register_operand")] UNSPEC_VFMAXMIN)
   (match_operand:V_VLSF 4 "autovec_else_operand")]
  "TARGET_VECTOR && !HONOR_SNANS (<MODE>mode)"
{
  insn_code icode = code_for_pred (<IEEE_FMAXMIN_OP>, <MODE>mode);
  riscv_vector::expand_cond_binop (icode, operands);
  DONE;
})

(define_expand "cond_len_<ieee_fmaxmin_op><mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (unspec:VF
     [(match_operand:VF 2 "register_operand")
      (match_operand:VF 3 "register_operand")] UNSPEC_VFMAXMIN)
   (match_operand:VF 4 "autovec_else_operand")
   (match_operand 5 "autovec_length_operand")
   (match_operand 6 "const_0_operand")]
  "TARGET_VECTOR && !HONOR_SNANS (<MODE>mode)"
{
  insn_code icode = code_for_pred (<IEEE_FMAXMIN_OP>, <MODE>mode);
  riscv_vector::expand_cond_len_binop (icode, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Conditional copysign operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vfsgnj
;; -------------------------------------------------------------------------

(define_expand "cond_copysign<mode>"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "register_operand")
   (match_operand:V_VLSF 2 "register_operand")
   (match_operand:V_VLSF 3 "register_operand")
   (match_operand:V_VLSF 4 "register_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (UNSPEC_VCOPYSIGN, <MODE>mode);
  rtx ops[] = {operands[0], operands[1], operands[2], operands[3], operands[4],
               gen_int_mode (GET_MODE_NUNITS (<MODE>mode), Pmode)};
  riscv_vector::expand_cond_len_binop (icode, ops);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [INT] Conditional ternary operations
;; -------------------------------------------------------------------------
;; Includes:
;; - vmacc/...
;; -------------------------------------------------------------------------

(define_expand "cond_fma<mode>"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:V_VLSI 2 "register_operand")
   (match_operand:V_VLSI 3 "register_operand")
   (match_operand:V_VLSI 4 "register_operand")
   (match_operand:V_VLSI 5 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_plus (<MODE>mode);
  riscv_vector::expand_cond_ternop (icode, operands);
  DONE;
})

(define_expand "cond_len_fma<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VI 2 "register_operand")
   (match_operand:VI 3 "register_operand")
   (match_operand:VI 4 "register_operand")
   (match_operand:VI 5 "autovec_else_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_plus (<MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fnma<mode>"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:V_VLSI 2 "register_operand")
   (match_operand:V_VLSI 3 "register_operand")
   (match_operand:V_VLSI 4 "register_operand")
   (match_operand:V_VLSI 5 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_minus_mul (<MODE>mode);
  riscv_vector::expand_cond_ternop (icode, operands);
  DONE;
})

(define_expand "cond_len_fnma<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VI 2 "register_operand")
   (match_operand:VI 3 "register_operand")
   (match_operand:VI 4 "register_operand")
   (match_operand:VI 5 "autovec_else_operand")
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
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:V_VLSF 2 "register_operand")
   (match_operand:V_VLSF 3 "register_operand")
   (match_operand:V_VLSF 4 "register_operand")
   (match_operand:V_VLSF 5 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul (PLUS, <MODE>mode);
  riscv_vector::expand_cond_ternop (icode, operands);
  DONE;
})

(define_expand "cond_len_fma<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "autovec_else_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul (PLUS, <MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fnma<mode>"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:V_VLSF 2 "register_operand")
   (match_operand:V_VLSF 3 "register_operand")
   (match_operand:V_VLSF 4 "register_operand")
   (match_operand:V_VLSF 5 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_neg (PLUS, <MODE>mode);
  riscv_vector::expand_cond_ternop (icode, operands);
  DONE;
})

(define_expand "cond_len_fnma<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "autovec_else_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_neg (PLUS, <MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fms<mode>"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:V_VLSF 2 "register_operand")
   (match_operand:V_VLSF 3 "register_operand")
   (match_operand:V_VLSF 4 "register_operand")
   (match_operand:V_VLSF 5 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul (MINUS, <MODE>mode);
  riscv_vector::expand_cond_ternop (icode, operands);
  DONE;
})

(define_expand "cond_len_fms<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "autovec_else_operand")
   (match_operand 6 "autovec_length_operand")
   (match_operand 7 "const_0_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul (MINUS, <MODE>mode);
  riscv_vector::expand_cond_len_ternop (icode, operands);
  DONE;
})

(define_expand "cond_fnms<mode>"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:V_VLSF 2 "register_operand")
   (match_operand:V_VLSF 3 "register_operand")
   (match_operand:V_VLSF 4 "register_operand")
   (match_operand:V_VLSF 5 "autovec_else_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred_mul_neg (MINUS, <MODE>mode);
  riscv_vector::expand_cond_ternop (icode, operands);
  DONE;
})

(define_expand "cond_len_fnms<mode>"
  [(match_operand:VF 0 "register_operand")
   (match_operand:<VM> 1 "vector_mask_operand")
   (match_operand:VF 2 "register_operand")
   (match_operand:VF 3 "register_operand")
   (match_operand:VF 4 "register_operand")
   (match_operand:VF 5 "autovec_else_operand")
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

(define_insn_and_split "reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
        (unspec:<VEL> [
             (match_operand:V_VLSI 1 "register_operand")
        ] UNSPEC_REDUC_SUM))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::expand_reduction (UNSPEC_REDUC_SUM,
				  UNSPEC_REDUC_SUM_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, CONST0_RTX (<VEL>mode));
  DONE;
}
[(set_attr "type" "vector")])

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  int prec = GET_MODE_PRECISION (<VEL>mode);
  rtx min = immed_wide_int_const (wi::min_value (prec, SIGNED), <VEL>mode);
  riscv_vector::expand_reduction (UNSPEC_REDUC_MAX,
				  UNSPEC_REDUC_MAX_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, min);
  DONE;
})

(define_expand "reduc_umax_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (UNSPEC_REDUC_MAXU,
				  UNSPEC_REDUC_MAXU_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, CONST0_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  int prec = GET_MODE_PRECISION (<VEL>mode);
  rtx max = immed_wide_int_const (wi::max_value (prec, SIGNED), <VEL>mode);
  riscv_vector::expand_reduction (UNSPEC_REDUC_MIN,
				  UNSPEC_REDUC_MIN_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, max);
  DONE;
})

(define_expand "reduc_umin_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  int prec = GET_MODE_PRECISION (<VEL>mode);
  rtx max = immed_wide_int_const (wi::max_value (prec, UNSIGNED), <VEL>mode);
  riscv_vector::expand_reduction (UNSPEC_REDUC_MINU,
				  UNSPEC_REDUC_MINU_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, max);
  DONE;
})

(define_expand "reduc_and_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (UNSPEC_REDUC_AND,
				  UNSPEC_REDUC_AND_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, CONSTM1_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_ior_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (UNSPEC_REDUC_OR,
				  UNSPEC_REDUC_OR_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, CONST0_RTX (<VEL>mode));
  DONE;
})

(define_expand "reduc_xor_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::expand_reduction (UNSPEC_REDUC_XOR,
				  UNSPEC_REDUC_XOR_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, CONST0_RTX (<VEL>mode));
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

(define_insn_and_split "reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
        (unspec:<VEL> [
             (match_operand:V_VLSF 1 "register_operand")
        ] UNSPEC_REDUC_SUM_UNORDERED))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_vector::expand_reduction (UNSPEC_REDUC_SUM_UNORDERED,
				  UNSPEC_REDUC_SUM_UNORDERED_VL0_SAFE,
                                  riscv_vector::REDUCE_OP_FRM_DYN,
                                  operands, CONST0_RTX (<VEL>mode));
  DONE;
}
[(set_attr "type" "vector")])

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR"
{
  REAL_VALUE_TYPE rv;
  real_inf (&rv, true);
  rtx f = const_double_from_real_value (rv, <VEL>mode);
  riscv_vector::expand_reduction (UNSPEC_REDUC_MAX,
				  UNSPEC_REDUC_MAX_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, f);
  DONE;
})

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR"
{
  REAL_VALUE_TYPE rv;
  real_inf (&rv, false);
  rtx f = const_double_from_real_value (rv, <VEL>mode);
  riscv_vector::expand_reduction (UNSPEC_REDUC_MIN,
				  UNSPEC_REDUC_MIN_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, f);
  DONE;
})

(define_expand "reduc_fmax_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !HONOR_SNANS (<MODE>mode)"
{
  REAL_VALUE_TYPE rv;
  real_inf (&rv, true);
  rtx f = const_double_from_real_value (rv, <VEL>mode);
  riscv_vector::expand_reduction (UNSPEC_REDUC_MAX,
				  UNSPEC_REDUC_MAX_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, f);
  DONE;
})

(define_expand "reduc_fmin_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !HONOR_SNANS (<MODE>mode)"
{
  REAL_VALUE_TYPE rv;
  real_inf (&rv, false);
  rtx f = const_double_from_real_value (rv, <VEL>mode);
  riscv_vector::expand_reduction (UNSPEC_REDUC_MIN,
				  UNSPEC_REDUC_MIN_VL0_SAFE,
				  riscv_vector::REDUCE_OP,
                                  operands, f);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Left-to-right reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - vfredosum.vs
;; -------------------------------------------------------------------------

;; Unpredicated in-order FP reductions.
(define_insn_and_split "fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
        (unspec:<VEL> [
             (match_operand:V_VLSF 2 "register_operand")
             (match_operand:<VEL> 1 "register_operand")
        ] UNSPEC_REDUC_SUM_ORDERED))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx ops[] = {operands[0], operands[2]};
  riscv_vector::expand_reduction (UNSPEC_REDUC_SUM_ORDERED,
				  UNSPEC_REDUC_SUM_ORDERED_VL0_SAFE,
                                  riscv_vector::REDUCE_OP_FRM_DYN,
                                  ops, operands[1]);
  DONE;
}
[(set_attr "type" "vector")])

;; Predicated in-order FP reductions.
(define_insn_and_split "mask_len_fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
        (unspec:<VEL> [
          (match_operand:VF 2 "register_operand")
          (match_operand:<VEL> 1 "register_operand")
          (match_operand:<VM> 3 "vector_mask_operand")
          (match_operand 4 "autovec_length_operand")
          (match_operand 5 "const_0_operand")
        ] UNSPEC_REDUC_SUM_ORDERED))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  if (rtx_equal_p (operands[4], const0_rtx))
    emit_move_insn (operands[0], operands[1]);
  else
    {
      rtx ops[] = {operands[0], operands[2], operands[3], operands[4]};
      riscv_vector::expand_reduction (UNSPEC_REDUC_SUM_ORDERED,
				      UNSPEC_REDUC_SUM_ORDERED_VL0_SAFE,
                                      riscv_vector::REDUCE_OP_M_FRM_DYN,
                                      ops, operands[1]);
    }
  DONE;
}
[(set_attr "type" "vector")])

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

(define_expand "avg<v_double_trunc>3_floor"
 [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand")
   (truncate:<V_DOUBLE_TRUNC>
    (ashiftrt:VWEXTI
     (plus:VWEXTI
      (sign_extend:VWEXTI
       (match_operand:<V_DOUBLE_TRUNC> 1 "register_operand"))
      (sign_extend:VWEXTI
       (match_operand:<V_DOUBLE_TRUNC> 2 "register_operand"))))))]
  "TARGET_VECTOR"
{
  /* First emit a widening addition.  */
  rtx tmp1 = gen_reg_rtx (<MODE>mode);
  rtx ops1[] = {tmp1, operands[1], operands[2]};
  insn_code icode = code_for_pred_dual_widen (PLUS, SIGN_EXTEND, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops1);

  /* Then a narrowing shift.  */
  rtx ops2[] = {operands[0], tmp1, const1_rtx};
  icode = code_for_pred_narrow_scalar (ASHIFTRT, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops2);
  DONE;
})

(define_expand "avg<v_double_trunc>3_ceil"
 [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand")
   (truncate:<V_DOUBLE_TRUNC>
    (ashiftrt:VWEXTI
     (plus:VWEXTI
      (plus:VWEXTI
       (sign_extend:VWEXTI
	(match_operand:<V_DOUBLE_TRUNC> 1 "register_operand"))
       (sign_extend:VWEXTI
	(match_operand:<V_DOUBLE_TRUNC> 2 "register_operand")))
      (const_int 1)))))]
  "TARGET_VECTOR"
{
  /* First emit a widening addition.  */
  rtx tmp1 = gen_reg_rtx (<MODE>mode);
  rtx ops1[] = {tmp1, operands[1], operands[2]};
  insn_code icode = code_for_pred_dual_widen (PLUS, SIGN_EXTEND, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops1);

  /* Then add 1.  */
  rtx tmp2 = gen_reg_rtx (<MODE>mode);
  rtx ops2[] = {tmp2, tmp1, const1_rtx};
  icode = code_for_pred_scalar (PLUS, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops2);

  /* Finally, a narrowing shift.  */
  rtx ops3[] = {operands[0], tmp2, const1_rtx};
  icode = code_for_pred_narrow_scalar (ASHIFTRT, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops3);
  DONE;
})

;; csrwi vxrm, 2
;; vaaddu.vv vd, vs2, vs1
(define_expand "uavg<mode>3_floor"
 [(match_operand:V_VLSI 0 "register_operand")
  (match_operand:V_VLSI 1 "register_operand")
  (match_operand:V_VLSI 2 "register_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (UNSPEC_VAADDU, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP_VXRM_RDN, operands);
  DONE;
})

;; csrwi vxrm, 0
;; vaaddu.vv vd, vs2, vs1
(define_expand "uavg<mode>3_ceil"
 [(match_operand:V_VLSI 0 "register_operand")
  (match_operand:V_VLSI 1 "register_operand")
  (match_operand:V_VLSI 2 "register_operand")]
  "TARGET_VECTOR"
{
  insn_code icode = code_for_pred (UNSPEC_VAADDU, <MODE>mode);
  riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP_VXRM_RNU, operands);
  DONE;
})

;; -------------------------------------------------------------------------
;; ---- [FP] Rounding.
;; -------------------------------------------------------------------------
;; Includes:
;; - ceil/ceilf
;; - floor/floorf
;; - nearbyint/nearbyintf
;; - rint/rintf
;; - round/roundf
;; - trunc/truncf
;; - roundeven/roundevenf
;; - lrint/lrintf
;; - irintf
;; - lceil/lceilf
;; - lfloor/lfloorf
;; -------------------------------------------------------------------------
(define_expand "ceil<mode>2"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_ceil (operands[0], operands[1], <MODE>mode, <VCONVERT>mode);
    DONE;
  }
)

(define_expand "floor<mode>2"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_floor (operands[0], operands[1], <MODE>mode, <VCONVERT>mode);
    DONE;
  }
)

(define_expand "nearbyint<mode>2"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_nearbyint (operands[0], operands[1], <MODE>mode, <VCONVERT>mode);
    DONE;
  }
)

(define_expand "rint<mode>2"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_rint (operands[0], operands[1], <MODE>mode, <VCONVERT>mode);
    DONE;
  }
)

(define_expand "round<mode>2"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_round (operands[0], operands[1], <MODE>mode, <VCONVERT>mode);
    DONE;
  }
)

(define_expand "btrunc<mode>2"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_trunc (operands[0], operands[1], <MODE>mode, <VCONVERT>mode);
    DONE;
  }
)

(define_expand "roundeven<mode>2"
  [(match_operand:V_VLSF 0 "register_operand")
   (match_operand:V_VLSF 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_roundeven (operands[0], operands[1], <MODE>mode, <VCONVERT>mode);
    DONE;
  }
)

(define_expand "lrint<mode><v_f2si_convert>2"
  [(match_operand:<V_F2SI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_SI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lrint (operands[0], operands[1], <MODE>mode,
				    <V_F2SI_CONVERT>mode, VOIDmode);
    DONE;
  }
)

(define_expand "lrint<mode><v_f2di_convert>2"
  [(match_operand:<V_F2DI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_DI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lrint (operands[0], operands[1], <MODE>mode,
				    <V_F2DI_CONVERT>mode,
				    <V_F2DI_CONVERT_BRIDGE>mode);
    DONE;
  }
)

(define_expand "lround<mode><v_f2si_convert>2"
  [(match_operand:<V_F2SI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_SI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lround (operands[0], operands[1], <MODE>mode,
				     <V_F2SI_CONVERT>mode, VOIDmode);
    DONE;
  }
)

(define_expand "lround<mode><v_f2di_convert>2"
  [(match_operand:<V_F2DI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_DI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lround (operands[0], operands[1], <MODE>mode,
				     <V_F2DI_CONVERT>mode,
				     <V_F2DI_CONVERT_BRIDGE>mode);

    DONE;
  }
)

(define_expand "lceil<mode><v_f2si_convert>2"
  [(match_operand:<V_F2SI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_SI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lceil (operands[0], operands[1], <MODE>mode, <V_F2SI_CONVERT>mode);
    DONE;
  }
)

(define_expand "lceil<mode><v_f2di_convert>2"
  [(match_operand:<V_F2DI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_DI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lceil (operands[0], operands[1], <MODE>mode, <V_F2DI_CONVERT>mode);
    DONE;
  }
)

(define_expand "lfloor<mode><v_f2si_convert>2"
  [(match_operand:<V_F2SI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_SI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lfloor (operands[0], operands[1], <MODE>mode, <V_F2SI_CONVERT>mode);
    DONE;
  }
)

(define_expand "lfloor<mode><v_f2di_convert>2"
  [(match_operand:<V_F2DI_CONVERT>   0 "register_operand")
   (match_operand:V_VLS_F_CONVERT_DI 1 "register_operand")]
  "TARGET_VECTOR && !flag_trapping_math && !flag_rounding_math"
  {
    riscv_vector::expand_vec_lfloor (operands[0], operands[1], <MODE>mode, <V_F2DI_CONVERT>mode);
    DONE;
  }
)

;; Implement rawmemchr[qi|si|hi].
(define_expand "rawmemchr<ANYI:mode>"
  [(match_operand      0 "register_operand")
   (match_operand      1 "memory_operand")
   (match_operand:ANYI 2 "const_int_operand")]
  "TARGET_VECTOR && !TARGET_XTHEADVECTOR"
  {
    riscv_vector::expand_rawmemchr(<MODE>mode, operands[0], operands[1],
				   operands[2]);
    DONE;
  }
)

;; =========================================================================
;; == [INT] Saturation ALU.
;; =========================================================================
;; Includes:
;; - add
;; - sub
;; - trunc
;; =========================================================================
(define_expand "usadd<mode>3"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")
   (match_operand:V_VLSI 2 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_usadd (operands[0], operands[1], operands[2], <MODE>mode);
    DONE;
  }
)

(define_expand "ssadd<mode>3"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")
   (match_operand:V_VLSI 2 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_ssadd (operands[0], operands[1], operands[2], <MODE>mode);
    DONE;
  }
)

(define_expand "ussub<mode>3"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")
   (match_operand:V_VLSI 2 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_ussub (operands[0], operands[1], operands[2], <MODE>mode);
    DONE;
  }
)

(define_expand "sssub<mode>3"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")
   (match_operand:V_VLSI 2 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_sssub (operands[0], operands[1], operands[2], <MODE>mode);
    DONE;
  }
)

(define_expand "ustrunc<mode><v_double_trunc>2"
  [(match_operand:<V_DOUBLE_TRUNC> 0 "register_operand")
   (match_operand:VWEXTI           1 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_double_ustrunc (operands[0], operands[1],
					     <MODE>mode);
    DONE;
  }
)

(define_expand "ustrunc<mode><v_quad_trunc>2"
  [(match_operand:<V_QUAD_TRUNC> 0 "register_operand")
   (match_operand:VQEXTI         1 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_quad_ustrunc (operands[0], operands[1], <MODE>mode,
					   <V_DOUBLE_TRUNC>mode);
    DONE;
  }
)

(define_expand "ustrunc<mode><v_oct_trunc>2"
  [(match_operand:<V_OCT_TRUNC> 0 "register_operand")
   (match_operand:VOEXTI        1 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_oct_ustrunc (operands[0], operands[1], <MODE>mode,
					  <V_DOUBLE_TRUNC>mode,
					  <V_QUAD_TRUNC>mode);
    DONE;
  }
)

(define_expand "sstrunc<mode><v_double_trunc>2"
  [(match_operand:<V_DOUBLE_TRUNC> 0 "register_operand")
   (match_operand:VWEXTI           1 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_double_sstrunc (operands[0], operands[1],
					  <MODE>mode);
    DONE;
  }
)

(define_expand "sstrunc<mode><v_quad_trunc>2"
  [(match_operand:<V_QUAD_TRUNC> 0 "register_operand")
   (match_operand:VQEXTI         1 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_quad_sstrunc (operands[0], operands[1], <MODE>mode,
					   <V_DOUBLE_TRUNC>mode);
    DONE;
  }
)

(define_expand "sstrunc<mode><v_oct_trunc>2"
  [(match_operand:<V_OCT_TRUNC> 0 "register_operand")
   (match_operand:VOEXTI        1 "register_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_oct_sstrunc (operands[0], operands[1], <MODE>mode,
					  <V_DOUBLE_TRUNC>mode,
					  <V_QUAD_TRUNC>mode);
    DONE;
  }
)

;; =========================================================================
;; == Early break auto-vectorization patterns
;; =========================================================================

;; vcond_mask_len (mask, 1s, 0s, len, bias)
;; => mask[i] = mask[i] && i < len ? 1 : 0
(define_insn_and_split "vcond_mask_len_<mode>"
  [(set (match_operand:VB 0 "register_operand")
    (unspec: VB [
     (match_operand:VB 1 "register_operand")
     (match_operand:VB 2 "const_1_operand")
     (match_operand:VB 3 "const_0_operand")
     (match_operand 4 "autovec_length_operand")
     (match_operand 5 "const_0_operand")] UNSPEC_SELECT_MASK))]
  "TARGET_VECTOR
   && can_create_pseudo_p ()
   && riscv_vector::get_vector_mode (Pmode, GET_MODE_NUNITS (<MODE>mode)).exists ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    machine_mode mode = riscv_vector::get_vector_mode (Pmode,
			GET_MODE_NUNITS (<MODE>mode)).require ();
    rtx reg = gen_reg_rtx (mode);
    riscv_vector::expand_vec_series (reg, const0_rtx, const1_rtx);
    rtx dup_rtx = gen_rtx_VEC_DUPLICATE (mode, operands[4]);
    insn_code icode = code_for_pred_cmp_scalar (mode);
    rtx cmp = gen_rtx_fmt_ee (LTU, <MODE>mode, reg, dup_rtx);
    rtx ops[] = {operands[0], operands[1], operands[1], cmp, reg, operands[4]};
    emit_vlmax_insn (icode, riscv_vector::COMPARE_OP_MU, ops);
    DONE;
  }
  [(set_attr "type" "vector")])

;; cbranch
(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "equality_operator"
	    [(match_operand:VB_VLS 1 "register_operand")
	     (match_operand:VB_VLS 2 "reg_or_0_operand")])
	  (label_ref (match_operand 3 ""))
	  (pc)))]
  "TARGET_VECTOR"
  {
    rtx pred;
    if (operands[2] == CONST0_RTX (<MODE>mode))
      pred = operands[1];
    else
      pred = expand_binop (<MODE>mode, xor_optab, operands[1],
			   operands[2], NULL_RTX, 0,
			   OPTAB_DIRECT);
    rtx reg = gen_reg_rtx (Pmode);
    rtx cpop_ops[] = {reg, pred};
    emit_vlmax_insn (code_for_pred_popcount (<MODE>mode, Pmode),
		     riscv_vector::CPOP_OP, cpop_ops);
    operands[1] = reg;
    operands[2] = const0_rtx;
  }
)

;; -------------------------------------------------------------------------
;; - vrol.vv vror.vv
;; -------------------------------------------------------------------------
(define_expand "v<bitmanip_optab><mode>3"
  [(set (match_operand:VI 0 "register_operand")
	(bitmanip_rotate:VI
	  (match_operand:VI 1 "register_operand")
	  (match_operand:VI 2 "register_operand")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  {
    riscv_vector::emit_vlmax_insn (code_for_pred_v (<CODE>, <MODE>mode),
				   riscv_vector::BINARY_OP, operands);
    DONE;
  }
)

;; =========================================================================
;; == Strided Load/Store
;; =========================================================================
(define_expand "mask_len_strided_load_<mode>"
  [(match_operand:V_VLS 0 "register_operand")
   (match_operand       1 "pmode_reg_or_0_operand")
   (match_operand       2 "pmode_reg_or_0_operand")
   (match_operand:<VM>  3 "vector_mask_operand")
   (match_operand       4 "maskload_else_operand")
   (match_operand       5 "autovec_length_operand")
   (match_operand       6 "const_0_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_strided_load (<MODE>mode, operands);
    DONE;
  })

(define_expand "mask_len_strided_store_<mode>"
  [(match_operand       0 "pmode_reg_or_0_operand")
   (match_operand       1 "pmode_reg_or_0_operand")
   (match_operand:V_VLS 2 "register_operand")
   (match_operand:<VM>  3 "vector_mask_operand")
   (match_operand       4 "autovec_length_operand")
   (match_operand       5 "const_0_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_strided_store (<MODE>mode, operands);
    DONE;
  })

; ========
; == Absolute difference (not including sum)
; ========
(define_expand "uabd<mode>3"
  [(match_operand:V_VLSI 0 "register_operand")
   (match_operand:V_VLSI 1 "register_operand")
   (match_operand:V_VLSI 2 "register_operand")]
  "TARGET_VECTOR"
  {
    rtx max = gen_reg_rtx (<MODE>mode);
    insn_code icode = code_for_pred (UMAX, <MODE>mode);
    rtx ops1[] = {max, operands[1], operands[2]};
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops1);

    rtx min = gen_reg_rtx (<MODE>mode);
    icode = code_for_pred (UMIN, <MODE>mode);
    rtx ops2[] = {min, operands[1], operands[2]};
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops2);

    icode = code_for_pred (MINUS, <MODE>mode);
    rtx ops3[] = {operands[0], max, min};
    riscv_vector::emit_vlmax_insn (icode, riscv_vector::BINARY_OP, ops3);

    DONE;
  });
