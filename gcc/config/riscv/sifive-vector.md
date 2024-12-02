;; Machine description for RISC-V for GNU compiler.
;; Copyright (C) 2024 Free Software Foundation, Inc.
;; Contributed by SiFive and PLCT Lab.
;; Based on RISC-V target for GNU compiler.

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

(define_insn "@pred_matrix_mul_plus<u><mode>_qoq"
  [(set (match_operand:SF_VSI 0 "register_operand"                    "=&vr")
	(if_then_else:SF_VSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:SF_VSI
	    (mult:SF_VSI
	      (any_extend:SF_VSI
	        (match_operand:RVVM1QI 3 "register_operand" "   vr"))
	      (any_extend:SF_VSI
	        (match_operand:<SF_VQMACC_QOQ> 4 "register_operand" "   vr")))
	    (match_operand:SF_VSI 2 "register_operand"              "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XSFVQMACCQOQ"
  "sf.vqmacc<u>.4x8x4\t%0,%3,%4"
  [(set_attr "type" "sf_vqmacc")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_matrix_mul_plussu<mode>_qoq"
  [(set (match_operand:SF_VSI 0 "register_operand"                    "=&vr")
	(if_then_else:SF_VSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:SF_VSI
	    (mult:SF_VSI
	      (sign_extend:SF_VSI
	        (match_operand:RVVM1QI 3 "register_operand" "   vr"))
	      (zero_extend:SF_VSI
	        (match_operand:<SF_VQMACC_QOQ> 4 "register_operand" "   vr")))
	    (match_operand:SF_VSI 2 "register_operand"              "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XSFVQMACCQOQ"
  "sf.vqmaccsu.4x8x4\t%0,%3,%4"
  [(set_attr "type" "sf_vqmacc")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_matrix_mul_plusus<mode>_qoq"
  [(set (match_operand:SF_VSI 0 "register_operand"                    "=&vr")
	(if_then_else:SF_VSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:SF_VSI
	    (mult:SF_VSI
	      (zero_extend:SF_VSI
	        (match_operand:RVVM1QI 3 "register_operand" "   vr"))
	      (sign_extend:SF_VSI
	        (match_operand:<SF_VQMACC_QOQ> 4 "register_operand" "   vr")))
	    (match_operand:SF_VSI 2 "register_operand"              "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XSFVQMACCQOQ"
  "sf.vqmaccus.4x8x4\t%0,%3,%4"
  [(set_attr "type" "sf_vqmacc")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_matrix_mul_plus<u><mode>_dod"
  [(set (match_operand:SF_VSI 0 "register_operand"                    "=&vr")
	(if_then_else:SF_VSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:SF_VSI
	    (mult:SF_VSI
	      (any_extend:SF_VSI
	        (match_operand:RVVM1QI 3 "register_operand" "   vr"))
	      (any_extend:SF_VSI
	        (match_operand:<SF_VQMACC_DOD> 4 "register_operand" "   vr")))
	    (match_operand:SF_VSI 2 "register_operand"              "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XSFVQMACCDOD"
  "sf.vqmacc<u>.2x8x2\t%0,%3,%4"
  [(set_attr "type" "sf_vqmacc")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_matrix_mul_plussu<mode>_dod"
  [(set (match_operand:SF_VSI 0 "register_operand"                    "=&vr")
	(if_then_else:SF_VSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:SF_VSI
	    (mult:SF_VSI
	      (sign_extend:SF_VSI
	        (match_operand:RVVM1QI 3 "register_operand" "   vr"))
	      (zero_extend:SF_VSI
	        (match_operand:<SF_VQMACC_DOD> 4 "register_operand" "   vr")))
	    (match_operand:SF_VSI 2 "register_operand"              "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XSFVQMACCDOD"
  "sf.vqmaccsu.2x8x2\t%0,%3,%4"
  [(set_attr "type" "sf_vqmacc")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_matrix_mul_plusus<mode>_dod"
  [(set (match_operand:SF_VSI 0 "register_operand"                    "=&vr")
	(if_then_else:SF_VSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:SF_VSI
	    (mult:SF_VSI
	      (zero_extend:SF_VSI
	        (match_operand:RVVM1QI 3 "register_operand" "   vr"))
	      (sign_extend:SF_VSI
	        (match_operand:<SF_VQMACC_DOD> 4 "register_operand" "   vr")))
	    (match_operand:SF_VSI 2 "register_operand"              "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR && TARGET_XSFVQMACCDOD"
  "sf.vqmaccus.2x8x2\t%0,%3,%4"
  [(set_attr "type" "sf_vqmacc")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_sf_vfnrclip<v_su><mode>_x_f_qf"
  [(set (match_operand:<SF_XFQF> 0 "register_operand"        "=vd, vd, vr, vr")
	(if_then_else:<SF_XFQF>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"        " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"            "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<SF_XFQF>
	    [(match_operand:SF 4 "register_operand"          "  f,  f,  f,  f")
	     (match_operand:SF_VF 3 "register_operand"       " vr, vr, vr, vr")] SF_VFNRCLIP)
	  (match_operand:<SF_XFQF> 2 "vector_merge_operand"  " vu,  0, vu,  0")))]
  "TARGET_VECTOR && TARGET_XSFVFNRCLIPXFQF"
  "sf.vfnrclip.x<v_su>.f.qf\t%0,%3,%4%p1"
  [(set_attr "type" "sf_vfnrclip")
   (set_attr "mode" "<MODE>")])
