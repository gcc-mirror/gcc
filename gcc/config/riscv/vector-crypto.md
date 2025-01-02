;; Machine description for the RISC-V Vector Crypto  extensions.
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

(define_c_enum "unspec" [
    ;; Zvbb unspecs
    UNSPEC_VBREV
    UNSPEC_VBREV8
    UNSPEC_VREV8
    UNSPEC_VCLMUL
    UNSPEC_VCLMULH
    UNSPEC_VGHSH
    UNSPEC_VGMUL
    UNSPEC_VAESEF
    UNSPEC_VAESEFVV
    UNSPEC_VAESEFVS
    UNSPEC_VAESEM
    UNSPEC_VAESEMVV
    UNSPEC_VAESEMVS
    UNSPEC_VAESDF
    UNSPEC_VAESDFVV
    UNSPEC_VAESDFVS
    UNSPEC_VAESDM
    UNSPEC_VAESDMVV
    UNSPEC_VAESDMVS
    UNSPEC_VAESZ
    UNSPEC_VAESZVVNULL
    UNSPEC_VAESZVS
    UNSPEC_VAESKF1
    UNSPEC_VAESKF2
    UNSPEC_VSHA2MS
    UNSPEC_VSHA2CH
    UNSPEC_VSHA2CL
    UNSPEC_VSM4K
    UNSPEC_VSM4R
    UNSPEC_VSM4RVV
    UNSPEC_VSM4RVS
    UNSPEC_VSM3ME
    UNSPEC_VSM3C
])

(define_int_attr rev  [(UNSPEC_VBREV "brev") (UNSPEC_VBREV8 "brev8") (UNSPEC_VREV8 "rev8")])

(define_int_attr h [(UNSPEC_VCLMUL "") (UNSPEC_VCLMULH "h")])

(define_int_attr vv_ins_name [(UNSPEC_VGMUL    "gmul" ) (UNSPEC_VAESEFVV "aesef")
                              (UNSPEC_VAESEMVV "aesem") (UNSPEC_VAESDFVV "aesdf")
                              (UNSPEC_VAESDMVV "aesdm") (UNSPEC_VAESEFVS "aesef")
                              (UNSPEC_VAESEMVS "aesem") (UNSPEC_VAESDFVS "aesdf")
                              (UNSPEC_VAESDMVS "aesdm") (UNSPEC_VAESZVS  "aesz" )
                              (UNSPEC_VSM4RVV  "sm4r" ) (UNSPEC_VSM4RVS  "sm4r" )])

(define_int_attr vv_ins1_name [(UNSPEC_VGHSH "ghsh")     (UNSPEC_VSHA2MS "sha2ms")
                               (UNSPEC_VSHA2CH "sha2ch") (UNSPEC_VSHA2CL "sha2cl")])

(define_int_attr vi_ins_name [(UNSPEC_VAESKF1 "aeskf1") (UNSPEC_VSM4K "sm4k")])

(define_int_attr vi_ins1_name [(UNSPEC_VAESKF2 "aeskf2") (UNSPEC_VSM3C "sm3c")])

(define_int_attr ins_type [(UNSPEC_VGMUL    "vv") (UNSPEC_VAESEFVV "vv")
                           (UNSPEC_VAESEMVV "vv") (UNSPEC_VAESDFVV "vv")
                           (UNSPEC_VAESDMVV "vv") (UNSPEC_VAESEFVS "vs")
                           (UNSPEC_VAESEMVS "vs") (UNSPEC_VAESDFVS "vs")
                           (UNSPEC_VAESDMVS "vs") (UNSPEC_VAESZVS  "vs")
                           (UNSPEC_VSM4RVV  "vv") (UNSPEC_VSM4RVS  "vs")])

(define_int_iterator UNSPEC_VRBB8 [UNSPEC_VBREV UNSPEC_VBREV8 UNSPEC_VREV8])

(define_int_iterator UNSPEC_CLMUL_VC [UNSPEC_VCLMUL UNSPEC_VCLMULH])

(define_int_iterator UNSPEC_CRYPTO_VV [UNSPEC_VGMUL    UNSPEC_VAESEFVV UNSPEC_VAESEMVV
                                       UNSPEC_VAESDFVV UNSPEC_VAESDMVV UNSPEC_VAESEFVS
                                       UNSPEC_VAESEMVS UNSPEC_VAESDFVS UNSPEC_VAESDMVS
                                       UNSPEC_VAESZVS  UNSPEC_VSM4RVV  UNSPEC_VSM4RVS])

(define_int_iterator UNSPEC_VGNHAB [UNSPEC_VGHSH UNSPEC_VSHA2MS UNSPEC_VSHA2CH UNSPEC_VSHA2CL])

(define_int_iterator UNSPEC_CRYPTO_VI [UNSPEC_VAESKF1 UNSPEC_VSM4K])

(define_int_iterator UNSPEC_CRYPTO_VI1 [UNSPEC_VAESKF2 UNSPEC_VSM3C])

;; zvbb instructions patterns.
;; vandn.vv vandn.vx vrol.vv vrol.vx
;; vror.vv vror.vx vror.vi
;; vwsll.vv vwsll.vx vwsll.vi
(define_insn "@pred_vandn<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"	  "=vd, vr, vd, vr")
     (if_then_else:V_VLSI
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand"	  "vm,Wc1, vm,Wc1")
          (match_operand 5 "vector_length_operand"        "rK, rK, rK, rK")
          (match_operand 6 "const_int_operand"            " i,  i,  i,  i")
          (match_operand 7 "const_int_operand"            " i,  i,  i,  i")
          (match_operand 8 "const_int_operand"            " i,  i,  i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (and:V_VLSI
         (not:V_VLSI
	    (match_operand:V_VLSI 4 "register_operand"	  "vr, vr, vr, vr"))
         (match_operand:V_VLSI 3 "register_operand"	  "vr, vr, vr, vr"))
       (match_operand:V_VLSI 2 "vector_merge_operand"     "vu, vu,  0,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "vandn.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vandn")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_vandn<mode>_scalar"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand"	  "=vd, vr,vd, vr")
     (if_then_else:V_VLSI_QHS
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand"	  " vm,Wc1,vm,Wc1")
          (match_operand 5 "vector_length_operand"     	  " rK, rK,rK, rK")
          (match_operand 6 "const_int_operand"         	  "  i,  i, i,  i")
          (match_operand 7 "const_int_operand"         	  "  i,  i, i,  i")
          (match_operand 8 "const_int_operand"         	  "  i,  i, i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (and:V_VLSI_QHS
         (not:V_VLSI_QHS
           (vec_duplicate:V_VLSI_QHS
             (match_operand:<VEL> 4 "register_operand"	  " r,  r, r,  r")))
         (match_operand:V_VLSI_QHS 3 "register_operand"   "vr, vr,vr, vr"))
       (match_operand:V_VLSI_QHS 2 "vector_merge_operand" "vu, vu, 0,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "vandn.vx\t%0,%3,%4%p1"
  [(set_attr "type" "vandn")
   (set_attr "mode" "<MODE>")])

;; Handle GET_MODE_INNER (mode) = DImode. We need to split them since
;; we need to deal with SEW = 64 in RV32 system.
(define_expand "@pred_vandn<mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
     (if_then_else:V_VLSI_D
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand")
          (match_operand 5 "vector_length_operand")
          (match_operand 6 "const_int_operand")
          (match_operand 7 "const_int_operand")
          (match_operand 8 "const_int_operand")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (and:V_VLSI_D
         (not:V_VLSI_D
           (vec_duplicate:V_VLSI_D
             (match_operand:<VEL> 4 "reg_or_int_operand")))
         (match_operand:V_VLSI_D 3 "register_operand"))
       (match_operand:V_VLSI_D 2 "vector_merge_operand")))]
  "TARGET_ZVBB || TARGET_ZVKB"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx broadcast_scalar) {
	  emit_insn (gen_pred_vandn<mode> (operands[0], operands[1],
	       operands[2], operands[3], broadcast_scalar, operands[5],
	       operands[6], operands[7], operands[8]));
        },
        (riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_vandn<mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"        "=vd, vr,vd, vr")
     (if_then_else:V_VLSI_D
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand"	    " vm,Wc1,vm,Wc1")
          (match_operand 5 "vector_length_operand"          " rK, rK,rK, rK")
          (match_operand 6 "const_int_operand"              " i,   i, i,  i")
          (match_operand 7 "const_int_operand"              " i,   i, i,  i")
          (match_operand 8 "const_int_operand"              " i,   i, i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (and:V_VLSI_D
         (not:V_VLSI_D
           (vec_duplicate:V_VLSI_D
             (match_operand:<VEL> 4 "reg_or_0_operand"	    " rJ, rJ,rJ, rJ")))
         (match_operand:V_VLSI_D 3 "register_operand"	    " vr, vr,vr, vr"))
       (match_operand:V_VLSI_D 2 "vector_merge_operand"	    " vu, vu, 0,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "vandn.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vandn")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_vandn<mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"        "=vd, vr,vd, vr")
     (if_then_else:V_VLSI_D
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand"       " vm,Wc1,vm,Wc1")
          (match_operand 5 "vector_length_operand"          " rK, rK,rK, rK")
          (match_operand 6 "const_int_operand"              " i,   i, i,  i")
          (match_operand 7 "const_int_operand"              " i,   i, i,  i")
          (match_operand 8 "const_int_operand"              " i,   i, i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (and:V_VLSI_D
         (not:V_VLSI_D
           (vec_duplicate:V_VLSI_D
             (sign_extend:<VEL>
               (match_operand:<VSUBEL> 4 "reg_or_0_operand" " rJ, rJ,rJ, rJ"))))
         (match_operand:V_VLSI_D 3 "register_operand"       " vr, vr,vr, vr"))
       (match_operand:V_VLSI_D 2 "vector_merge_operand"     " vu, vu, 0,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "vandn.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vandn")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_v<bitmanip_optab><mode>"
  [(set (match_operand:VI 0 "register_operand"        "=vd,vd, vr, vr")
     (if_then_else:VI
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand" " vm,vm,Wc1,Wc1")
          (match_operand 5 "vector_length_operand"    " rK,rK, rK, rK")
          (match_operand 6 "const_int_operand"        "  i, i,  i,  i")
          (match_operand 7 "const_int_operand"        "  i, i,  i,  i")
          (match_operand 8 "const_int_operand"        "  i, i,  i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (bitmanip_rotate:VI
         (match_operand:VI 3 "register_operand"       " vr,vr, vr, vr")
         (match_operand:VI 4 "register_operand"       " vr,vr, vr, vr"))
       (match_operand:VI 2 "vector_merge_operand"     " vu, 0, vu,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "v<bitmanip_insn>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "v<bitmanip_insn>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_v<bitmanip_optab><mode>_scalar"
  [(set (match_operand:VI 0 "register_operand"        "=vd,vd, vr, vr")
     (if_then_else:VI
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand" " vm,vm,Wc1,Wc1")
          (match_operand 5 "vector_length_operand"    " rK,rK, rK, rK")
          (match_operand 6 "const_int_operand"        "  i, i,  i,  i")
          (match_operand 7 "const_int_operand"        "  i, i,  i,  i")
          (match_operand 8 "const_int_operand"        "  i, i,  i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (bitmanip_rotate:VI
         (match_operand:VI 3 "register_operand"       " vr,vr, vr, vr")
         (match_operand 4 "pmode_register_operand"    "  r, r,  r,  r"))
       (match_operand:VI 2 "vector_merge_operand"     " vu, 0, vu,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "v<bitmanip_insn>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "v<bitmanip_insn>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_vror<mode>_scalar"
  [(set (match_operand:VI 0 "register_operand"        "=vd,vd, vr,vr")
     (if_then_else:VI
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand" " vm,vm,Wc1,Wc1")
          (match_operand 5 "vector_length_operand"    " rK,rK, rK, rK")
          (match_operand 6 "const_int_operand"        "  i, i,  i,  i")
          (match_operand 7 "const_int_operand"        "  i, i,  i,  i")
          (match_operand 8 "const_int_operand"        "  i, i,  i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (rotatert:VI
         (match_operand:VI 3 "register_operand"       " vr,vr, vr, vr")
         (match_operand    4 "const_csr_operand"      "  K, K,  K,  K"))
       (match_operand:VI 2 "vector_merge_operand"     " vu, 0, vu,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "vror.vi\t%0,%3,%4%p1"
  [(set_attr "type" "vror")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_vwsll<mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"     "=&vr")
     (if_then_else:VWEXTI
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1")
          (match_operand 5 "vector_length_operand"    "  rK")
          (match_operand 6 "const_int_operand"        "   i")
          (match_operand 7 "const_int_operand"        "   i")
          (match_operand 8 "const_int_operand"        "   i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (ashift:VWEXTI
         (zero_extend:VWEXTI
           (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "vr"))
         (match_operand:<V_DOUBLE_TRUNC> 4 "vector_shift_operand"  "vrvk"))
       (match_operand:VWEXTI 2 "vector_merge_operand" "0vu")))]
  "TARGET_ZVBB"
  "vwsll.v%o4\t%0,%3,%4%p1"
  [(set_attr "type" "vwsll")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_vwsll<mode>_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"              "=&vr,    &vr")
     (if_then_else:VWEXTI
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1, vmWc1")
          (match_operand 5 "vector_length_operand"              "   rK,    rK")
          (match_operand 6 "const_int_operand"                  "    i,     i")
          (match_operand 7 "const_int_operand"                  "    i,     i")
          (match_operand 8 "const_int_operand"                  "    i,     i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (ashift:VWEXTI
         (zero_extend:VWEXTI
           (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,    vr"))
         (match_operand 4 "pmode_reg_or_uimm5_operand"		"   rK,    rK"))
       (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_ZVBB"
  "vwsll.v%o4\t%0,%3,%4%p1"
  [(set_attr "type" "vwsll")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

;; vbrev.v vbrev8.v vrev8.v
(define_insn "@pred_v<rev><mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"        "=vd,vr,vd,vr")
     (if_then_else:V_VLSI
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand"	  "vm,Wc1,vm,Wc1")
          (match_operand 4 "vector_length_operand"    	  "rK,rK, rK, rK")
          (match_operand 5 "const_int_operand"        	  "i,  i,  i,  i")
          (match_operand 6 "const_int_operand"        	  "i,  i,  i,  i")
          (match_operand 7 "const_int_operand"        	  "i,  i,  i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:V_VLSI
         [(match_operand:V_VLSI 3 "register_operand"      "vr,vr, vr, vr")]UNSPEC_VRBB8)
       (match_operand:V_VLSI 2 "vector_merge_operand"     "vu,vu,  0,  0")))]
  "TARGET_ZVBB || TARGET_ZVKB"
  "v<rev>.v\t%0,%3%p1"
  [(set_attr "type" "v<rev>")
   (set_attr "mode" "<MODE>")])

;; vclz.v vctz.v vcpop.v
(define_insn "@pred_v<bitmanip_optab><mode>"
  [(set (match_operand:V_VLSI 0	    "register_operand"      "=vd, vr")
     (clz_ctz_pcnt:V_VLSI
       (parallel
         [(match_operand:V_VLSI 2   "register_operand"      " vr, vr")
          (unspec:<VM>
            [(match_operand:<VM> 1  "vector_mask_operand"   " vm,Wc1")
             (match_operand 3       "vector_length_operand" " rK, rK")
             (match_operand 4       "const_int_operand"     "  i,  i")
             (reg:SI VL_REGNUM)
             (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)])))]
  "TARGET_ZVBB"
  "v<bitmanip_insn>.v\t%0,%2%p1"
  [(set_attr "type" "v<bitmanip_insn>")
   (set_attr "mode" "<MODE>")])

;; zvbc instructions patterns.
;; vclmul.vv vclmul.vx
;; vclmulh.vv vclmulh.vx
(define_insn "@pred_vclmul<h><mode>"
  [(set (match_operand:VI_D 0  "register_operand"     "=vd,vr,vd, vr")
     (if_then_else:VI_D
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand" "vm,Wc1,vm,Wc1")
          (match_operand 5 "vector_length_operand"    "rK, rK,rK, rK")
          (match_operand 6 "const_int_operand"        " i,  i, i,  i")
          (match_operand 7 "const_int_operand"        " i,  i, i,  i")
          (match_operand 8 "const_int_operand"        " i,  i, i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VI_D
         [(match_operand:VI_D 3 "register_operand"     "vr, vr,vr, vr")
          (match_operand:VI_D 4 "register_operand"     "vr, vr,vr, vr")] UNSPEC_CLMUL_VC)
       (match_operand:VI_D 2 "vector_merge_operand"    "vu, vu, 0,  0")))]
  "TARGET_ZVBC"
  "vclmul<h>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vclmul<h>")
   (set_attr "mode" "<MODE>")])

;; Deal with SEW = 64 in RV32 system.
(define_expand "@pred_vclmul<h><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand")
     (if_then_else:VI_D
       (unspec:<VM>
         [(match_operand:<VM> 1 "vector_mask_operand")
          (match_operand 5 "vector_length_operand")
          (match_operand 6 "const_int_operand")
          (match_operand 7 "const_int_operand")
          (match_operand 8 "const_int_operand")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VI_D
         [(vec_duplicate:VI_D
            (match_operand:<VEL> 4 "register_operand"))
          (match_operand:VI_D 3 "register_operand")] UNSPEC_CLMUL_VC)
       (match_operand:VI_D 2 "vector_merge_operand")))]
  "TARGET_ZVBC"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx broadcast_scalar) {
	  emit_insn (gen_pred_vclmul<h><mode> (operands[0], operands[1],
	       operands[2], operands[3], broadcast_scalar, operands[5],
	       operands[6], operands[7], operands[8]));
        },
        (riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_vclmul<h><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand"       "=vd,vr,vd, vr")
    (if_then_else:VI_D
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand"  "vm,Wc1,vm,Wc1")
        (match_operand 5 "vector_length_operand"      "rK, rK,rK, rK")
        (match_operand 6 "const_int_operand"          " i,  i, i,  i")
        (match_operand 7 "const_int_operand"          " i,  i, i,  i")
        (match_operand 8 "const_int_operand"          " i,  i, i,  i")
        (reg:SI VL_REGNUM)
        (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (unspec:VI_D
        [(vec_duplicate:VI_D
           (match_operand:<VEL> 4 "reg_or_0_operand"   "rJ, rJ,rJ, rJ"))
         (match_operand:VI_D 3 "register_operand"      "vr, vr,vr, vr")] UNSPEC_CLMUL_VC)
      (match_operand:VI_D 2 "vector_merge_operand"     "vu, vu, 0,  0")))]
  "TARGET_ZVBC"
  "vclmul<h>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "vclmul<h>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_vclmul<h><mode>_extend_scalar"
  [(set (match_operand:VI_D 0 "register_operand"         "=vd,vr,vd, vr")
    (if_then_else:VI_D
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand"    "vm,Wc1,vm,Wc1")
        (match_operand 5 "vector_length_operand"        "rK, rK,rK, rK")
        (match_operand 6 "const_int_operand"            " i,  i, i,  i")
        (match_operand 7 "const_int_operand"            " i,  i, i,  i")
        (match_operand 8 "const_int_operand"            " i,  i, i,  i")
        (reg:SI VL_REGNUM)
        (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (unspec:VI_D
        [(vec_duplicate:VI_D
	   (sign_extend:<VEL>
             (match_operand:<VSUBEL> 4 "reg_or_0_operand" " rJ, rJ,rJ, rJ")))
         (match_operand:VI_D 3 "register_operand"        "vr, vr,vr, vr")] UNSPEC_CLMUL_VC)
      (match_operand:VI_D 2 "vector_merge_operand"       "vu, vu, 0,  0")))]
  "TARGET_ZVBC"
  "vclmul<h>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "vclmul<h>")
   (set_attr "mode" "<MODE>")])

;; zvknh[ab] and zvkg instructions patterns.
;; vsha2ms.vv vsha2ch.vv vsha2cl.vv vghsh.vv
(define_insn "@pred_v<vv_ins1_name><mode>"
  [(set (match_operand:VQEXTI 0 "register_operand"     "=vr")
     (if_then_else:VQEXTI
       (unspec:<VM>
         [(match_operand 4 "vector_length_operand"     "rK")
          (match_operand 5 "const_int_operand"         " i")
          (match_operand 6 "const_int_operand"         " i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VQEXTI
          [(match_operand:VQEXTI 1 "register_operand" " 0")
           (match_operand:VQEXTI 2 "register_operand" "vr")
           (match_operand:VQEXTI 3 "register_operand" "vr")] UNSPEC_VGNHAB)
       (match_dup 1)))]
  "TARGET_ZVKNHA || TARGET_ZVKNHB || TARGET_ZVKG"
  "v<vv_ins1_name>.vv\t%0,%2,%3"
  [(set_attr "type" "v<vv_ins1_name>")
   (set_attr "mode" "<MODE>")])

;; zvkned and zvksed amd zvkg instructions patterns.
;; vgmul.vv       vaesz.vs
;; vaesef.[vv,vs] vaesem.[vv,vs] vaesdf.[vv,vs] vaesdm.[vv,vs]
;; vsm4r.[vv,vs]
(define_insn "@pred_crypto_vv<vv_ins_name><ins_type><mode>"
  [(set (match_operand:VSI 0 "register_operand"    "=vr")
     (if_then_else:VSI
       (unspec:<VM>
         [(match_operand 3 "vector_length_operand" " rK")
          (match_operand 4 "const_int_operand"     "  i")
          (match_operand 5 "const_int_operand"     "  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VSI
         [(match_operand:VSI 1 "register_operand" " 0")
          (match_operand:VSI 2 "register_operand" "vr")] UNSPEC_CRYPTO_VV)
       (match_dup 1)))]
  "TARGET_ZVKNED || TARGET_ZVKSED || TARGET_ZVKG"
  "v<vv_ins_name>.<ins_type>\t%0,%2"
  [(set_attr "type" "v<vv_ins_name>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_crypto_vv<vv_ins_name><ins_type>x1<mode>_scalar"
  [(set (match_operand:VSI 0 "register_operand"    "=&vr")
     (if_then_else:VSI
       (unspec:<VM>
         [(match_operand 3 "vector_length_operand" "  rK")
          (match_operand 4 "const_int_operand"     "   i")
          (match_operand 5 "const_int_operand"     "   i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VSI
         [(match_operand:VSI 1 "register_operand" " 0")
          (match_operand:VSI 2 "register_operand" "vr")] UNSPEC_CRYPTO_VV)
       (match_dup 1)))]
  "TARGET_ZVKNED || TARGET_ZVKSED"
  "v<vv_ins_name>.<ins_type>\t%0,%2"
  [(set_attr "type" "v<vv_ins_name>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_crypto_vv<vv_ins_name><ins_type>x2<mode>_scalar"
  [(set (match_operand:<VSIX2> 0 "register_operand" "=&vr")
     (if_then_else:<VSIX2>
       (unspec:<VM>
         [(match_operand 3 "vector_length_operand"  "rK")
          (match_operand 4 "const_int_operand"      " i")
          (match_operand 5 "const_int_operand"      " i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:<VSIX2>
         [(match_operand:<VSIX2> 1  "register_operand"   " 0")
          (match_operand:VLMULX2_SI 2 "register_operand" "vr")] UNSPEC_CRYPTO_VV)
       (match_dup 1)))]
  "TARGET_ZVKNED || TARGET_ZVKSED"
  "v<vv_ins_name>.<ins_type>\t%0,%2"
  [(set_attr "type" "v<vv_ins_name>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_crypto_vv<vv_ins_name><ins_type>x4<mode>_scalar"
 [(set (match_operand:<VSIX4> 0 "register_operand"      "=&vr")
    (if_then_else:<VSIX4>
      (unspec:<VM>
        [(match_operand 3 "vector_length_operand"       " rK")
         (match_operand 4 "const_int_operand"           "  i")
         (match_operand 5 "const_int_operand"           "  i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:<VSIX4>
         [(match_operand:<VSIX4> 1 "register_operand"    " 0")
          (match_operand:VLMULX4_SI 2 "register_operand" "vr")] UNSPEC_CRYPTO_VV)
       (match_dup 1)))]
 "TARGET_ZVKNED || TARGET_ZVKSED"
 "v<vv_ins_name>.<ins_type>\t%0,%2"
 [(set_attr "type" "v<vv_ins_name>")
  (set_attr "mode" "<MODE>")])

(define_insn "@pred_crypto_vv<vv_ins_name><ins_type>x8<mode>_scalar"
 [(set (match_operand:<VSIX8> 0 "register_operand"      "=&vr")
    (if_then_else:<VSIX8>
      (unspec:<VM>
        [(match_operand 3 "vector_length_operand"       " rK")
         (match_operand 4 "const_int_operand"           "  i")
         (match_operand 5 "const_int_operand"           "  i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (unspec:<VSIX8>
        [(match_operand:<VSIX8> 1 "register_operand"    "  0")
         (match_operand:VLMULX8_SI 2 "register_operand" " vr")] UNSPEC_CRYPTO_VV)
      (match_dup 1)))]
 "TARGET_ZVKNED || TARGET_ZVKSED"
 "v<vv_ins_name>.<ins_type>\t%0,%2"
 [(set_attr "type" "v<vv_ins_name>")
  (set_attr "mode" "<MODE>")])

(define_insn "@pred_crypto_vv<vv_ins_name><ins_type>x16<mode>_scalar"
 [(set (match_operand:<VSIX16> 0 "register_operand"      "=&vr")
    (if_then_else:<VSIX16>
      (unspec:<VM>
        [(match_operand 3 "vector_length_operand"        "  rK")
         (match_operand 4 "const_int_operand"            "   i")
         (match_operand 5 "const_int_operand"            "   i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (unspec:<VSIX16>
        [(match_operand:<VSIX16> 1 "register_operand"    "   0")
         (match_operand:VLMULX16_SI 2 "register_operand" "  vr")] UNSPEC_CRYPTO_VV)
      (match_dup 1)))]
 "TARGET_ZVKNED || TARGET_ZVKSED"
 "v<vv_ins_name>.<ins_type>\t%0,%2"
 [(set_attr "type" "v<vv_ins_name>")
  (set_attr "mode" "<MODE>")])

;; vaeskf1.vi vsm4k.vi
(define_insn "@pred_crypto_vi<vi_ins_name><mode>_scalar"
  [(set (match_operand:VSI 0 "register_operand"        "=vr, vr")
     (if_then_else:VSI
       (unspec:<VM>
         [(match_operand 4 "vector_length_operand"      "rK, rK")
          (match_operand 5 "const_int_operand"          " i,  i")
          (match_operand 6 "const_int_operand"          " i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VSI
         [(match_operand:VSI 2       "register_operand" "vr, vr")
          (match_operand 3  "const_int_operand"         " i,  i")] UNSPEC_CRYPTO_VI)
       (match_operand:VSI 1 "vector_merge_operand"      "vu,  0")))]
  "TARGET_ZVKNED || TARGET_ZVKSED"
  "v<vi_ins_name>.vi\t%0,%2,%3"
  [(set_attr "type" "v<vi_ins_name>")
   (set_attr "mode" "<MODE>")])

;; vaeskf2.vi vsm3c.vi
(define_insn "@pred_vi<vi_ins1_name><mode>_nomaskedoff_scalar"
  [(set (match_operand:VSI 0 "register_operand"       "=vr")
     (if_then_else:VSI
       (unspec:<VM>
         [(match_operand 4 "vector_length_operand"    "rK")
          (match_operand 5 "const_int_operand"        " i")
          (match_operand 6 "const_int_operand"        " i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VSI
          [(match_operand:VSI 1   "register_operand"  " 0")
           (match_operand:VSI 2   "register_operand"  "vr")
           (match_operand 3 "const_int_operand" " i")] UNSPEC_CRYPTO_VI1)
       (match_dup 1)))]
  "TARGET_ZVKNED || TARGET_ZVKSH"
  "v<vi_ins1_name>.vi\t%0,%2,%3"
  [(set_attr "type" "v<vi_ins1_name>")
   (set_attr "mode" "<MODE>")])

;; zvksh instructions patterns.
;; vsm3me.vv
(define_insn "@pred_vsm3me<mode>"
  [(set (match_operand:VSI 0 "register_operand"    "=vr, vr")
     (if_then_else:VSI
       (unspec:<VM>
         [(match_operand 4 "vector_length_operand" " rK, rK")
          (match_operand 5 "const_int_operand"     "  i,  i")
          (match_operand 6 "const_int_operand"     "  i,  i")
          (reg:SI VL_REGNUM)
          (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VSI
          [(match_operand:VSI 2 "register_operand" " vr, vr")
           (match_operand:VSI 3 "register_operand" " vr, vr")] UNSPEC_VSM3ME)
       (match_operand:VSI 1 "vector_merge_operand" " vu, 0")))]
  "TARGET_ZVKSH"
  "vsm3me.vv\t%0,%2,%3"
  [(set_attr "type" "vsm3me")
   (set_attr "mode" "<MODE>")])
