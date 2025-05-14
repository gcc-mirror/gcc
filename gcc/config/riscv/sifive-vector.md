;; Machine description for RISC-V for GNU compiler.
;; Copyright (C) 2024-2025 Free Software Foundation, Inc.
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
  [(set (match_operand:SF_XF 0 "register_operand"        "=vd, vd, vr, vr")
	(if_then_else:SF_XF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"        " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"            "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:SF_XF
	    [(match_operand:SF 4 "register_operand"          "  f,  f,  f,  f")
	     (match_operand:<SF_XFQF> 3 "register_operand"       " vr, vr, vr, vr")] SF_VFNRCLIP)
	  (match_operand:SF_XF 2 "vector_merge_operand"  " vu,  0, vu,  0")))]
  "TARGET_VECTOR && TARGET_XSFVFNRCLIPXFQF"
  "sf.vfnrclip.x<v_su>.f.qf\t%0,%3,%4%p1"
  [(set_attr "type" "sf_vfnrclip")
   (set_attr "mode" "<MODE>")])

;; SF_VCP
(define_insn "@sf_vc_x_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:SI 2 "const_int_operand" "K")
	     (match_operand:SI 3 "const_int_operand" "K")
	     (match_operand:<VEL> 4 "register_operand" "r")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.x\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_x_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:SI 4 "const_int_operand" "K,K")
	     (match_operand:<VEL> 5 "register_operand" "r,r")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.x\t%3,%4,%0,%5"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_x<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:SI 4 "const_int_operand" "K,K")
	     (match_operand:<VEL> 5 "register_operand" "r,r")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.x\t%3,%4,%0,%5"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_i_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:SI 2 "const_int_operand" "K")
	     (match_operand:SI 3 "const_int_operand" "K")
	     (match_operand:SI 4 "const_int_operand" "P")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.i\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_i_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:SI 4 "const_int_operand" "K,K")
	     (match_operand:SI 5 "const_int_operand" "P,P")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.i\t%3,%4,%0,%5"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_i<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:SI 4 "const_int_operand" "K,K")
	     (match_operand:SI 5 "const_int_operand" "P,P")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.i\t%3,%4,%0,%5"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_vv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:SI 2 "const_int_operand" "K")
	     (match_operand:VFULLI 3 "register_operand" "vr")
	     (match_operand:VFULLI 4 "register_operand" "vr")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.vv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_vv_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vr,vr")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.vv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_vv<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vr,vr")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.vv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_xv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:SI 2 "const_int_operand" "K")
	     (match_operand:VFULLI 3 "register_operand" "vr")
	     (match_operand:<VEL> 4 "register_operand" "r")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.xv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_xv_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vd,vd")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vr,vr")
	     (match_operand:<VEL> 5 "register_operand" "r,r")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.xv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_xv<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vd,vd")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vr,vr")
	     (match_operand:<VEL> 5 "register_operand" "r,r")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.xv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_iv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:SI 2 "const_int_operand" "K")
	     (match_operand:VFULLI 3 "register_operand" "vr")
	     (match_operand:SI 4 "const_int_operand" "P")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.iv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_iv_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vd,vd")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vr,vr")
	     (match_operand:SI 5 "const_int_operand" "P,P")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.iv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_iv<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vd,vd")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vr,vr")
	     (match_operand:SI 5 "const_int_operand" "P,P")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.iv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_fv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:SF_FV
	    [(match_operand:SI 1 "const_int_operand" "Ou01")
	     (match_operand:SI 2 "const_int_operand" "K")
	     (match_operand:SF_FV 3 "register_operand" "vr")
	     (match_operand:<SF_XF> 4 "register_operand" "f")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.fv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_fv_se<mode>"
  [(set (match_operand:SF_FV 0 "register_operand" "=&vd,vd")
	(if_then_else:SF_FV
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:SF_FV
	    [(match_operand:SI 3 "const_int_operand" "Ou01,Ou01")
	     (match_operand:SF_FV 4 "register_operand" "vr,vr")
	     (match_operand:<SF_XF> 5 "register_operand" "f,f")] UNSPECV_SF_CV)
       (match_operand:SF_FV 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.fv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_fv<mode>"
  [(set (match_operand:SF_FV 0 "register_operand" "=&vd,vd")
	(if_then_else:SF_FV
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 6 "vector_length_operand"                "   rK, rK")
	     (match_operand 7 "const_int_operand"                    "    i,  i")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:SF_FV
	    [(match_operand:SI 3 "const_int_operand" "Ou01,Ou01")
	     (match_operand:SF_FV 4 "register_operand" "vr,vr")
	     (match_operand:<SF_XF> 5 "register_operand" "f,f")] UNSPEC_SF_CV)
       (match_operand:SF_FV 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.fv\t%3,%0,%4,%5"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_vvv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:VFULLI 2 "register_operand" "vd")
	     (match_operand:VFULLI 3 "register_operand" "vr")
	     (match_operand:VFULLI 4 "register_operand" "vr")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.vvv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_vvv_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vd,vd")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")
		 (match_operand:VFULLI 6 "register_operand" "vr,vr")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.vvv\t%3,%4,%6,%5"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_vvv<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vd,vd")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")
		 (match_operand:VFULLI 6 "register_operand" "vr,vr")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.vvv\t%3,%4,%6,%5"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_xvv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:VFULLI 2 "register_operand" "vd")
	     (match_operand:VFULLI 3 "register_operand" "vr")
		 (match_operand:<VEL> 4 "register_operand" "r")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.xvv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_xvv_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vd,vd")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")
	     (match_operand:<VEL> 6 "register_operand" "r,r")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.xvv\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_xvv<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vd,vd")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")
	     (match_operand:<VEL> 6 "register_operand" "r,r")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.xvv\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_ivv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:VFULLI 2 "register_operand" "vd")
	     (match_operand:VFULLI 3 "register_operand" "vr")
	     (match_operand:SI 4 "const_int_operand" "P")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.ivv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_ivv_se<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vd,vd")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")
	     (match_operand:SI 6 "const_int_operand" "P,P")] UNSPECV_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.ivv\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_ivv<mode>"
  [(set (match_operand:VFULLI 0 "register_operand" "=&vr,vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:VFULLI 4 "register_operand" "vd,vd")
	     (match_operand:VFULLI 5 "register_operand" "vr,vr")
	     (match_operand:SI 6 "const_int_operand" "P,P")] UNSPEC_SF_CV)
       (match_operand:VFULLI 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.ivv\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_fvv_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:SF_FV
	    [(match_operand:SI 1 "const_int_operand" "Ou01")
	     (match_operand:SF_FV 2 "register_operand" "vd")
	     (match_operand:SF_FV 3 "register_operand" "vr")
	     (match_operand:<SF_XF> 4 "register_operand" "f")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.fvv\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_fvv_se<mode>"
  [(set (match_operand:SF_FV 0 "register_operand" "=&vr,vr")
	(if_then_else:SF_FV
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:SF_FV
	    [(match_operand:SI 3 "const_int_operand" "Ou01,Ou01")
	     (match_operand:SF_FV 4 "register_operand" "vd,vd")
	     (match_operand:SF_FV 5 "register_operand" "vr,vr")
	     (match_operand:<SF_XF> 6 "register_operand" "f,f")] UNSPECV_SF_CV)
       (match_operand:SF_FV 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.fvv\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_fvv<mode>"
  [(set (match_operand:SF_FV 0 "register_operand" "=&vr,vr")
	(if_then_else:SF_FV
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                    "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:SF_FV
	    [(match_operand:SI 3 "const_int_operand" "Ou01,Ou01")
	     (match_operand:SF_FV 4 "register_operand" "vd,vd")
	     (match_operand:SF_FV 5 "register_operand" "vr,vr")
	     (match_operand:<SF_XF> 6 "register_operand" "f,f")] UNSPEC_SF_CV)
       (match_operand:SF_FV 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.fvv\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_vvw_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:<SF_VW> 2 "register_operand" "vd")
	     (match_operand:SF_VC_W 3 "register_operand" "vr")
	     (match_operand:SF_VC_W 4 "register_operand" "vr")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.vvw\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_vvw_se<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_W 5 "register_operand" "vr,vr")
	     (match_operand:SF_VC_W 6 "register_operand" "vr,vr")] UNSPECV_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.vvw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_vvw<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_W 5 "register_operand" "vr,vr")
	     (match_operand:SF_VC_W 6 "register_operand" "vr,vr")] UNSPEC_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.vvw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_xvw_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:<SF_VW> 2 "register_operand" "vd")
	     (match_operand:SF_VC_W 3 "register_operand" "vr")
	     (match_operand:<VEL> 4 "register_operand" "r")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.xvw\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_xvw_se<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_W 5 "register_operand" "vr,vr")
	     (match_operand:<VEL> 6 "register_operand" "r,r")] UNSPECV_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.xvw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_xvw<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_W 5 "register_operand" "vr,vr")
	     (match_operand:<VEL> 6 "register_operand" "r,r")] UNSPEC_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.xvw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_ivw_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 1 "const_int_operand" "Ou02")
	     (match_operand:<SF_VW> 2 "register_operand" "vd")
	     (match_operand:SF_VC_W 3 "register_operand" "vr")
	     (match_operand:SI 4 "immediate_operand" "P")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.ivw\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_ivw_se<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec_volatile:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_W 5 "register_operand" "vr,vr")
	     (match_operand:SI 6 "immediate_operand" "P,P")] UNSPEC_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.ivw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_ivw<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou02,Ou02")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_W 5 "register_operand" "vr,vr")
	     (match_operand:SI 6 "immediate_operand" "P,P")] UNSPEC_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.ivw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_fvw_se<mode>"
  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand"             "  Wc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 1 "const_int_operand" "Ou01")
	     (match_operand:<SF_VW> 2 "register_operand" "vd")
	     (match_operand:SF_VC_FW 3 "register_operand" "vr")
	     (match_operand:<SF_XFW> 4 "register_operand" "f")] UNSPECV_SF_CV)]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.fvw\t%1,%2,%3,%4"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_fvw_se<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec_volatile:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou01,Ou01")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_FW 5 "register_operand" "vr,vr")
	     (match_operand:<SF_XFW> 6 "register_operand" "f,f")] UNSPECV_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.fvw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc_se")
   (set_attr "mode" "<MODE>")])

(define_insn "@sf_vc_v_fvw<mode>"
  [(set (match_operand:<SF_VW> 0 "register_operand" "=&vr,vr")
	(if_then_else:<SF_VW>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "  Wc1,Wc1")
	     (match_operand 7 "vector_length_operand"                "   rK, rK")
	     (match_operand 8 "const_int_operand"                    "    i,  i")
	     (match_operand 9 "const_int_operand"                    "    i,  i")
	     (match_operand 10 "const_int_operand"                   "    i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<SF_VW>
	    [(match_operand:SI 3 "const_int_operand" "Ou01,Ou01")
	     (match_operand:<SF_VW> 4 "register_operand" "vd,vd")
	     (match_operand:SF_VC_FW 5 "register_operand" "vr,vr")
	     (match_operand:<SF_XFW> 6 "register_operand" "f,f")] UNSPEC_SF_CV)
       (match_operand:<SF_VW> 2 "vector_merge_operand"     "vu,vu")))]
  "TARGET_VECTOR && TARGET_XSFVCP"
  "sf.vc.v.fvw\t%3,%4,%5,%6"
  [(set_attr "type" "sf_vc")
   (set_attr "mode" "<MODE>")])
