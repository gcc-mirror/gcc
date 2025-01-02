;; Machine description for RISC-V bfloat16 extensions.
;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

(define_mode_iterator VWEXTF_ZVFBF [
  (RVVM8SF  "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM4SF  "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM2SF  "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF  "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2SF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN > 32")
])

(define_mode_attr V_FP32TOBF16_TRUNC [
  (RVVM8SF "RVVM4BF") (RVVM4SF "RVVM2BF") (RVVM2SF "RVVM1BF") (RVVM1SF "RVVMF2BF") (RVVMF2SF "RVVMF4BF")
])

(define_mode_attr VF32_SUBEL [
   (RVVM8SF "BF") (RVVM4SF "BF") (RVVM2SF "BF") (RVVM1SF "BF") (RVVMF2SF "BF")])

;; Zvfbfmin extension

(define_insn "@pred_trunc<mode>_to_bf16"
  [(set (match_operand:<V_FP32TOBF16_TRUNC> 0 "register_operand"   "=vd, vd, vr, vr,  &vr,  &vr")
     (if_then_else:<V_FP32TOBF16_TRUNC>
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand"              " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
          (match_operand 4 "vector_length_operand"                 " rK, rK, rK, rK,   rK,   rK")
          (match_operand 5 "const_int_operand"                     "  i,  i,  i,  i,    i,    i")
          (match_operand 6 "const_int_operand"                     "  i,  i,  i,  i,    i,    i")
          (match_operand 7 "const_int_operand"                     "  i,  i,  i,  i,    i,    i")
          (match_operand 8 "const_int_operand"                     "  i,  i,  i,  i,    i,    i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)
          (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
       (float_truncate:<V_FP32TOBF16_TRUNC>
          (match_operand:VWEXTF_ZVFBF 3 "register_operand"          "  0,  0,  0,  0,   vr,   vr"))
       (match_operand:<V_FP32TOBF16_TRUNC> 2 "vector_merge_operand" " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_ZVFBFMIN"
  "vfncvtbf16.f.f.w\t%0,%3%p1"
  [(set_attr "type" "vfncvtbf16")
   (set_attr "mode" "<V_FP32TOBF16_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

(define_insn "@pred_extend_bf16_to_<mode>"
  [(set (match_operand:VWEXTF_ZVFBF 0 "register_operand"          "=&vr,  &vr")
    (if_then_else:VWEXTF_ZVFBF
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand"              "vmWc1,vmWc1")
         (match_operand 4 "vector_length_operand"                 "   rK,   rK")
         (match_operand 5 "const_int_operand"                     "    i,    i")
         (match_operand 6 "const_int_operand"                     "    i,    i")
         (match_operand 7 "const_int_operand"                     "    i,    i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (float_extend:VWEXTF_ZVFBF
         (match_operand:<V_FP32TOBF16_TRUNC> 3 "register_operand" "   vr,   vr"))
      (match_operand:VWEXTF_ZVFBF 2 "vector_merge_operand"        "   vu,    0")))]
  "TARGET_ZVFBFMIN"
  "vfwcvtbf16.f.f.v\t%0,%3%p1"
  [(set_attr "type" "vfwcvtbf16")
   (set_attr "mode" "<V_FP32TOBF16_TRUNC>")])


(define_insn "@pred_widen_bf16_mul_<mode>"
  [(set (match_operand:VWEXTF_ZVFBF 0 "register_operand"             "=&vr")
    (if_then_else:VWEXTF_ZVFBF
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand"                 "vmWc1")
         (match_operand 5 "vector_length_operand"                    "   rK")
         (match_operand 6 "const_int_operand"                        "    i")
         (match_operand 7 "const_int_operand"                        "    i")
         (match_operand 8 "const_int_operand"                        "    i")
         (match_operand 9 "const_int_operand"                        "    i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)
         (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
      (plus:VWEXTF_ZVFBF
        (mult:VWEXTF_ZVFBF
          (float_extend:VWEXTF_ZVFBF
            (match_operand:<V_FP32TOBF16_TRUNC> 3 "register_operand" "   vr"))
          (float_extend:VWEXTF_ZVFBF
            (match_operand:<V_FP32TOBF16_TRUNC> 4 "register_operand" "   vr")))
        (match_operand:VWEXTF_ZVFBF 2 "register_operand"             "    0"))
      (match_dup 2)))]
  "TARGET_ZVFBFWMA"
  "vfwmaccbf16.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfwmaccbf16")
   (set_attr "mode" "<V_FP32TOBF16_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_widen_bf16_mul_<mode>_scalar"
  [(set (match_operand:VWEXTF_ZVFBF 0 "register_operand"             "=&vr")
    (if_then_else:VWEXTF_ZVFBF
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand"                 "vmWc1")
         (match_operand 5 "vector_length_operand"                    "   rK")
         (match_operand 6 "const_int_operand"                        "    i")
         (match_operand 7 "const_int_operand"                        "    i")
         (match_operand 8 "const_int_operand"                        "    i")
         (match_operand 9 "const_int_operand"                        "    i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)
         (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
      (plus:VWEXTF_ZVFBF
        (mult:VWEXTF_ZVFBF
          (float_extend:VWEXTF_ZVFBF
            (vec_duplicate:<V_FP32TOBF16_TRUNC>
              (match_operand:<VF32_SUBEL> 3 "register_operand"       "    f")))
          (float_extend:VWEXTF_ZVFBF
            (match_operand:<V_FP32TOBF16_TRUNC> 4 "register_operand" "   vr")))
        (match_operand:VWEXTF_ZVFBF 2 "register_operand"             "    0"))
      (match_dup 2)))]
  "TARGET_ZVFBFWMA"
  "vfwmaccbf16.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vfwmaccbf16")
   (set_attr "mode" "<V_FP32TOBF16_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])
