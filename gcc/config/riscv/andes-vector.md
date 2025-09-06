;; Machine description for Andes vendor extensions
;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

(define_c_enum "unspec" [
  UNSPEC_NDS_VFWCVTBF16
  UNSPEC_NDS_VFNCVTBF16
])

;;  ....................
;;
;;    VECTOR BFLOAT16 CONVERSION
;;
;;  ....................

;; Xandesvbfhcvt extension
(define_insn "@pred_nds_vfncvt_bf16<mode>"
  [(set (match_operand:<NDS_V_DOUBLE_TRUNC_BF> 0 "register_operand" "=&vr, &vr")
	(if_then_else:<NDS_V_DOUBLE_TRUNC_BF>
	  (unspec:<VM>
	    [(match_operand 3 "vector_length_operand"           "  rK,  rK")
	     (match_operand 4 "const_int_operand"               "  i,    i")
	     (match_operand 5 "const_int_operand"               "  i,    i")
	     (match_operand 6 "const_int_operand"               "  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_NDS_VFWCVTBF16)
	  (float_truncate:<NDS_V_DOUBLE_TRUNC_BF>
	     (match_operand:NDS_VWEXTBF 2 "register_operand"              "vr, vr"))
	  (match_operand:<NDS_V_DOUBLE_TRUNC_BF> 1 "vector_merge_operand" "vu,  0")))]
  "TARGET_VECTOR && TARGET_XANDESVBFHCVT"
  "nds.vfncvt.bf16.s\t%0,%2"
  [(set_attr "type" "vfncvtbf16")
   (set_attr "mode" "<NDS_V_DOUBLE_TRUNC_BF>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "3")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[4])"))
   (set (attr "avl_type_idx") (const_int 5))
   (set (attr "ma") (const_int INVALID_ATTRIBUTE))
   (set (attr "frm_mode")
        (symbol_ref "riscv_vector::get_frm_mode (operands[6])"))])

(define_insn "@pred_nds_vfwcvt_bf16<mode>"
  [(set (match_operand:NDS_VWEXTBF 0 "register_operand"   "=&vr, &vr")
	(if_then_else:NDS_VWEXTBF
	  (unspec_volatile:<VM>
	    [(match_operand 3 "vector_length_operand" "  rK,  rK")
	     (match_operand 4 "const_int_operand"     "   i,   i")
	     (match_operand 5 "const_int_operand"     "   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_NDS_VFNCVTBF16)
	  (float_extend:NDS_VWEXTBF
	     (match_operand:<NDS_V_DOUBLE_TRUNC_BF> 2 "register_operand" "vr, vr"))
	  (match_operand:NDS_VWEXTBF 1 "vector_merge_operand"            "vu,  0")))]
  "TARGET_VECTOR && TARGET_XANDESVBFHCVT"
  "nds.vfwcvt.s.bf16\t%0,%2"
  [(set_attr "type" "vfwcvtbf16")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "3")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[4])"))
   (set (attr "avl_type_idx") (const_int 5))
   (set (attr "ma") (const_int INVALID_ATTRIBUTE))
   (set_attr "mode" "<NDS_V_DOUBLE_TRUNC_BF>")])
