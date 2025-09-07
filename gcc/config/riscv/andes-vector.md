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
  UNSPEC_NDS_INTLOAD
  UNSPEC_NDS_VFPMADT
  UNSPEC_NDS_VFPMADB
  UNSPEC_NDS_VD4DOT
])

(define_int_iterator NDS_VFPMAD [UNSPEC_NDS_VFPMADT UNSPEC_NDS_VFPMADB])
(define_int_attr nds_tb [(UNSPEC_NDS_VFPMADT "t") (UNSPEC_NDS_VFPMADB "b")])

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

;; Vector INT4 Load Extension.

(define_insn "@pred_intload_mov<su><mode>"
  [(set (match_operand:NDS_QVI 0 "nonimmediate_operand"       "=vr,  vr, vd")
    (if_then_else:NDS_QVI
      (unspec:<VM>
	[(match_operand:<VM> 1 "vector_mask_operand"        "vmWc1, Wc1, vm")
	 (match_operand 4 "vector_length_operand"           "   rK,  rK, rK")
	 (match_operand 5 "const_int_operand"               "    i,   i,  i")
	 (match_operand 6 "const_int_operand"               "    i,   i,  i")
	 (match_operand 7 "const_int_operand"               "    i,   i,  i")
	 (reg:SI VL_REGNUM)
	 (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (unspec:NDS_QVI
	[(any_extend:NDS_QVI (match_operand:VOID 3 "memory_operand" "m,  m,  m"))]
	  UNSPEC_NDS_INTLOAD)
      (match_operand:NDS_QVI 2 "vector_merge_operand"               "0, vu, vu")))]
  "(TARGET_VECTOR && TARGET_XANDESVSINTLOAD
    && register_operand (operands[0], <MODE>mode))"
  "@
   nds.vln<u>8.v\t%0,%3%p1
   nds.vln<u>8.v\t%0,%3
   nds.vln<u>8.v\t%0,%3,%1.t"
  [(set_attr "type" "vlde,vlde,vlde")
   (set_attr "mode" "<MODE>")])

;; Vector Packed FP16.

(define_insn "@pred_nds_vfpmad<nds_tb><mode>"
  [(set (match_operand:VHF 0 "register_operand"               "=&vr, &vr")
	(if_then_else:VHF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       "vmWc1, vmWc1")
	     (match_operand 5 "vector_length_operand"          "   rK,    rK")
	     (match_operand 6 "const_int_operand"              "    i,     i")
	     (match_operand 7 "const_int_operand"              "    i,     i")
	     (match_operand 8 "const_int_operand"              "    i,     i")
	     (match_operand 9 "const_int_operand"              "    i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VHF
	    [(match_operand:VHF 3 "register_operand" "vr, vr")
	     (match_operand:SF 4 "register_operand"   " f,  f")] NDS_VFPMAD)
	  (match_operand:VHF 2 "vector_merge_operand"   "vu,  0")))]
  "TARGET_VECTOR && TARGET_XANDESVPACKFPH"
  "nds.vfpmad<nds_tb>.vf\t%0,%4,%3%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "enabled" "yes")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

;; Vector Dot Product Extension

(define_insn "@pred_nds_vd4dot<su><mode>"
  [(set (match_operand:VQEXTI 0 "register_operand"                    "=&vr")
	(if_then_else:VQEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VQEXTI
	    [(any_extend:VQEXTI
	       (match_operand:<NDS_QUAD_FIX> 3 "register_operand" " vr"))
	     (any_extend:VQEXTI
	       (match_operand:<NDS_QUAD_FIX> 4 "register_operand" " vr"))
	     (any_extend:VQEXTI
	       (match_operand:VQEXTI 2 "register_operand" " 0"))]
	     UNSPEC_NDS_VD4DOT)
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XANDESVDOT"
  "nds.vd4dot<su>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "viwmuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_nds_vd4dotsu<mode>"
  [(set (match_operand:VQEXTI 0 "register_operand"                    "=&vr")
	(if_then_else:VQEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VQEXTI
	    [(sign_extend:VQEXTI
	       (match_operand:<NDS_QUAD_FIX> 3 "register_operand" " vr"))
	     (zero_extend:VQEXTI
	       (match_operand:<NDS_QUAD_FIX> 4 "register_operand" " vr"))
	     (sign_extend:VQEXTI
	       (match_operand:VQEXTI 2 "register_operand" " 0"))]
	    UNSPEC_NDS_VD4DOT)
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XANDESVDOT"
  "nds.vd4dotsu.vv\t%0,%3,%4%p1"
  [(set_attr "type" "viwmuladd")
   (set_attr "mode" "<MODE>")])
