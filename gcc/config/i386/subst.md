;; GCC machine description for AVX512F instructions
;; Copyright (C) 2013-2025 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Some iterators for extending subst as much as possible
;; All vectors (Use it for destination)
(define_mode_iterator SUBST_V
  [V64QI V32QI V16QI
   V32HI V16HI V8HI
   V16SI V8SI  V4SI
   V8DI  V4DI  V2DI
   V32HF V16HF V8HF
   V32BF V16BF V8BF
   V16SF V8SF  V4SF
   V8DF  V4DF  V2DF])

(define_mode_iterator SUBST_CV
  [V32HF V16HF V8HF])

(define_mode_iterator SUBST_S
  [QI HI SI DI])

(define_mode_iterator SUBST_A
  [V64QI V32QI V16QI
   V32HI V16HI V8HI
   V16SI V8SI  V4SI
   V8DI  V4DI  V2DI
   V32HF V16HF V8HF
   V16SF V8SF  V4SF
   V8DF  V4DF  V2DF
   QI HI SI DI SF DF])

(define_subst_attr "mask_name" "mask" "" "_mask")
(define_subst_attr "maskc_name" "maskc" "" "_mask")
(define_subst_attr "mask_applied" "mask" "false" "true")
(define_subst_attr "mask_operand2" "mask" "" "%{%3%}%N2")
(define_subst_attr "mask_operand3" "mask" "" "%{%4%}%N3")
(define_subst_attr "maskc_operand3" "maskc" "" "%{%4%}%N3")
(define_subst_attr "mask_operand3_1" "mask" "" "%%{%%4%%}%%N3") ;; for sprintf
(define_subst_attr "mask_operand4" "mask" "" "%{%5%}%N4")
(define_subst_attr "mask_operand6" "mask" "" "%{%7%}%N6")
(define_subst_attr "mask_operand7" "mask" "" "%{%8%}%N7")
(define_subst_attr "mask_operand10" "mask" "" "%{%11%}%N10")
(define_subst_attr "mask_operand11" "mask" "" "%{%12%}%N11")
(define_subst_attr "mask_operand18" "mask" "" "%{%19%}%N18")
(define_subst_attr "mask_operand19" "mask" "" "%{%20%}%N19")
(define_subst_attr "mask_codefor" "mask" "*" "")
(define_subst_attr "mask_operand_arg34" "mask" "" ", operands[3], operands[4]")
(define_subst_attr "mask_mode512bit_condition" "mask" "1" "(<MODE_SIZE> == 64 || TARGET_AVX512VL)")
(define_subst_attr "mask_avx512vl_condition" "mask" "1" "TARGET_AVX512VL")
(define_subst_attr "mask_avx512bw_condition" "mask" "1" "TARGET_AVX512BW")
(define_subst_attr "mask_avx512dq_condition" "mask" "1" "TARGET_AVX512DQ")
(define_subst_attr "mask_prefix" "mask" "vex" "evex")
(define_subst_attr "mask_prefix2" "mask" "maybe_vex" "evex")
(define_subst_attr "mask_prefix3" "mask" "orig,vex" "evex,evex")
(define_subst_attr "bcst_mask_prefix3" "mask" "orig,maybe_evex" "evex,evex")
(define_subst_attr "mask_prefix4" "mask" "orig,orig,vex" "evex,evex,evex")
(define_subst_attr "bcst_mask_prefix4" "mask" "orig,orig,maybe_evex" "evex,evex,evex")
(define_subst_attr "mask_expand_op3" "mask" "3" "5")
(define_subst_attr "mask3_dest_false_dep_for_glc_cond" "mask" "1" "operands[3] == CONST0_RTX(<MODE>mode)")
(define_subst_attr "mask4_dest_false_dep_for_glc_cond" "mask" "1" "operands[4] == CONST0_RTX(<MODE>mode)")
(define_subst_attr "mask6_dest_false_dep_for_glc_cond" "mask" "1" "operands[6] == CONST0_RTX(<MODE>mode)")
(define_subst_attr "mask10_dest_false_dep_for_glc_cond" "mask" "1" "operands[10] == CONST0_RTX(<MODE>mode)")
(define_subst_attr "maskc_dest_false_dep_for_glc_cond" "maskc" "1" "operands[3] == CONST0_RTX(<MODE>mode)")

(define_subst "mask"
  [(set (match_operand:SUBST_V 0)
        (match_operand:SUBST_V 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
        (vec_merge:SUBST_V
	  (match_dup 1)
	  (match_operand:SUBST_V 2 "nonimm_or_0_operand" "0C")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")))])

(define_subst_attr "merge_mask_name" "merge_mask" "" "_merge_mask")
(define_subst_attr "merge_mask_operand3" "merge_mask" "" "%{%3%}")
(define_subst "merge_mask"
  [(set (match_operand:SUBST_V 0)
        (match_operand:SUBST_V 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
        (vec_merge:SUBST_V
	  (match_dup 1)
	  (match_dup 0)
	  (match_operand:<avx512fmaskmode> 2 "register_operand" "Yk")))])

(define_subst "maskc"
  [(set (match_operand:SUBST_CV 0)
        (match_operand:SUBST_CV 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
        (vec_merge:SUBST_CV
	  (match_dup 1)
	  (match_operand:SUBST_CV 2 "nonimm_or_0_operand" "0C")
	  (unspec:<avx512fmaskmode>
	    [(match_operand:<avx512fmaskcmode> 3 "register_operand" "Yk")]
	    UNSPEC_COMPLEX_MASK)))])

(define_subst_attr "mask_scalar_merge_name" "mask_scalar_merge" "" "_mask")
(define_subst_attr "mask_scalar_merge_operand3" "mask_scalar_merge" "" "%{%3%}")
(define_subst_attr "mask_scalar_merge_operand4" "mask_scalar_merge" "" "%{%4%}")

(define_subst "mask_scalar_merge"
  [(set (match_operand:SUBST_S 0)
        (match_operand:SUBST_S 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
        (and:SUBST_S
	  (match_dup 1)
	  (match_operand:SUBST_S 3 "register_operand" "Yk")))])

(define_subst_attr "sd_maskz_name" "sd" "" "_maskz_1")
(define_subst_attr "sd_mask_op4" "sd" "" "%{%5%}%N4")
(define_subst_attr "sd_mask_op5" "sd" "" "%{%6%}%N5")
(define_subst_attr "sd_mask_codefor" "sd" "*" "")
(define_subst_attr "sd_mask_mode512bit_condition" "sd" "1" "(<MODE_SIZE> == 64 || TARGET_AVX512VL)")

(define_subst "sd"
 [(set (match_operand:SUBST_V 0)
       (match_operand:SUBST_V 1))]
 ""
 [(set (match_dup 0)
       (vec_merge:SUBST_V
	 (match_dup 1)
	 (match_operand:SUBST_V 2 "const0_operand")
	 (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")))
])

(define_subst_attr "maskz_scalar_name" "maskz_scalar" "" "_maskz_1")
(define_subst_attr "maskz_scalar_op5" "maskz_scalar" "" "%{%6%}%N5")

(define_subst "maskz_scalar"
  [(set (match_operand:SUBST_V 0)
	(vec_merge:SUBST_V
	  (match_operand:SUBST_V 1)
	  (match_operand:SUBST_V 2)
	  (const_int 1)))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(vec_merge:SUBST_V
	  (vec_merge:SUBST_V
	    (match_dup 1)
	    (match_operand:SUBST_V 3 "const0_operand")
	    (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk"))
	  (match_dup 2)
	  (const_int 1)))])
(define_subst_attr "sdc_maskz_name" "sdc" "" "_maskz_1")
(define_subst_attr "sdc_mask_op4" "sdc" "" "%{%5%}%N4")
(define_subst_attr "sdc_mask_op5" "sdc" "" "%{%6%}%N5")
(define_subst_attr "sdc_mask_mode512bit_condition" "sdc" "1" "(<MODE_SIZE> == 64 || TARGET_AVX512VL)")

(define_subst "sdc"
 [(set (match_operand:SUBST_CV 0)
       (match_operand:SUBST_CV 1))]
 ""
 [(set (match_dup 0)
       (vec_merge:SUBST_CV
	 (match_dup 1)
	 (match_operand:SUBST_CV 2 "const0_operand")
	 (unspec:<avx512fmaskmode>
	   [(match_operand:<avx512fmaskcmode> 3 "register_operand" "Yk")]
	   UNSPEC_COMPLEX_MASK)))
])

(define_subst_attr "round_name" "round" "" "_round")
(define_subst_attr "round_mask_operand2" "mask" "%R2" "%R4")
(define_subst_attr "round_mask_operand3" "mask" "%R3" "%R5")
(define_subst_attr "round_maskc_operand3" "maskc" "%R3" "%R5")
(define_subst_attr "round_mask_operand4" "mask" "%R4" "%R6")
(define_subst_attr "round_sd_mask_operand4" "sd" "%R4" "%R6")
(define_subst_attr "round_sdc_mask_operand4" "sdc" "%R4" "%R6")
(define_subst_attr "round_op2" "round" "" "%R2")
(define_subst_attr "round_op3" "round" "" "%R3")
(define_subst_attr "round_op4" "round" "" "%R4")
(define_subst_attr "round_op5" "round" "" "%R5")
(define_subst_attr "round_op6" "round" "" "%R6")
(define_subst_attr "round_mask_op2" "round" "" "<round_mask_operand2>")
(define_subst_attr "round_mask_op3" "round" "" "<round_mask_operand3>")
(define_subst_attr "round_maskc_op3" "round" "" "<round_maskc_operand3>")
(define_subst_attr "round_mask_op4" "round" "" "<round_mask_operand4>")
(define_subst_attr "round_sd_mask_op4" "round" "" "<round_sd_mask_operand4>")
(define_subst_attr "round_sdc_mask_op4" "round" "" "<round_sdc_mask_operand4>")
(define_subst_attr "round_constraint" "round" "vm" "v")
(define_subst_attr "round_qq2phsuff" "round" "<qq2phsuff>" "")
(define_subst_attr "bcst_round_constraint" "round" "vmBr" "v")
(define_subst_attr "round_constraint2" "round" "m" "v")
(define_subst_attr "round_constraint3" "round" "rm" "r")
(define_subst_attr "round_nimm_predicate" "round" "vector_operand" "register_operand")
(define_subst_attr "bcst_round_nimm_predicate" "round" "bcst_vector_operand" "register_operand")
(define_subst_attr "round_nimm_scalar_predicate" "round" "nonimmediate_operand" "register_operand")
(define_subst_attr "round_prefix" "round" "vex" "evex")
(define_subst_attr "round_mode512bit_condition" "round" "1" "(<MODE>mode == V16SFmode
							      || <MODE>mode == V8DFmode
							      || <MODE>mode == V8DImode
							      || <MODE>mode == V16SImode
							      || <MODE>mode == V32HFmode)")

(define_subst_attr "round_modev4sf_condition" "round" "1" "(<MODE>mode == V4SFmode)")
(define_subst_attr "round_codefor" "round" "*" "")
(define_subst_attr "round_opnum" "round" "5" "6")

(define_subst "round"
  [(set (match_operand:SUBST_A 0)
	(match_operand:SUBST_A 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(unspec:SUBST_A [(match_dup 1)
	  (match_operand:SI 2 "const_4_or_8_to_11_operand")]
	  UNSPEC_EMBEDDED_ROUNDING))
])

(define_subst_attr "round_saeonly_name" "round_saeonly" "" "_round")
(define_subst_attr "round_saeonly_mask_operand2" "mask" "%r2" "%r4")
(define_subst_attr "round_saeonly_mask_operand3" "mask" "%r3" "%r5")
(define_subst_attr "round_saeonly_mask_operand4" "mask" "%r4" "%r6")
(define_subst_attr "round_saeonly_mask_scalar_merge_operand4" "mask_scalar_merge" "%r4" "%r5")
(define_subst_attr "round_saeonly_maskz_scalar_operand5" "maskz_scalar" "%r5" "%r7")
(define_subst_attr "round_saeonly_sd_mask_operand5" "sd" "%r5" "%r7")
(define_subst_attr "round_saeonly_sdc_mask_operand5" "sdc" "%r5" "%r7")
(define_subst_attr "round_saeonly_op2" "round_saeonly" "" "%r2")
(define_subst_attr "round_saeonly_op3" "round_saeonly" "" "%r3")
(define_subst_attr "round_saeonly_op4" "round_saeonly" "" "%r4")
(define_subst_attr "round_saeonly_op5" "round_saeonly" "" "%r5")
(define_subst_attr "round_saeonly_op6" "round_saeonly" "" "%r6")
(define_subst_attr "round_saeonly_prefix" "round_saeonly" "vex" "evex")
(define_subst_attr "round_saeonly_mask_op2" "round_saeonly" "" "<round_saeonly_mask_operand2>")
(define_subst_attr "round_saeonly_mask_op3" "round_saeonly" "" "<round_saeonly_mask_operand3>")
(define_subst_attr "round_saeonly_mask_op4" "round_saeonly" "" "<round_saeonly_mask_operand4>")
(define_subst_attr "round_saeonly_mask_scalar_merge_op4" "round_saeonly" "" "<round_saeonly_mask_scalar_merge_operand4>")
(define_subst_attr "round_saeonly_sd_mask_op5" "round_saeonly" "" "<round_saeonly_sd_mask_operand5>")
(define_subst_attr "round_saeonly_maskz_scalar_op5" "round_saeonly" "" "<round_saeonly_maskz_scalar_operand5>")
(define_subst_attr "round_saeonly_mask_arg3" "round_saeonly" "" ", operands[<mask_expand_op3>]")
(define_subst_attr "round_saeonly_constraint" "round_saeonly" "vm" "v")
(define_subst_attr "round_saeonly_constraint2" "round_saeonly" "m" "v")
(define_subst_attr "round_saeonly_nimm_predicate" "round_saeonly" "vector_operand" "register_operand")
(define_subst_attr "round_saeonly_nimm_scalar_predicate" "round_saeonly" "nonimmediate_operand" "register_operand")
(define_subst_attr "round_saeonly_mode512bit_condition" "round_saeonly" "1" "(<MODE>mode == V16SFmode
									      || <MODE>mode == V8DFmode
									      || <MODE>mode == V8DImode
									      || <MODE>mode == V16SImode
									      || <MODE>mode == V32HFmode)")


(define_subst "round_saeonly"
  [(set (match_operand:SUBST_A 0)
        (match_operand:SUBST_A 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(unspec:SUBST_A [(match_dup 1)
	  (match_operand:SI 2 "const48_operand")]
	  UNSPEC_EMBEDDED_ROUNDING))
])

(define_subst "round_saeonly"
  [(set (match_operand:CCFP 0)
        (match_operand:CCFP 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(unspec:CCFP [(match_dup 1)
	  (match_operand:SI 2 "const48_operand")]
	  UNSPEC_EMBEDDED_ROUNDING))
])

(define_subst_attr "round_expand_name" "round_expand" "" "_round")
(define_subst_attr "round_expand_nimm_predicate" "round_expand" "nonimmediate_operand" "register_operand")
(define_subst_attr "round_expand_operand" "round_expand" "" ", operands[5]")
(define_subst_attr "round_embedded_complex" "round_expand" "0" "!(CONST_INT_P (operands[5])
								  && (INTVAL (operands[5])
								      == NO_ROUND))")

(define_subst "round_expand"
 [(match_operand:SUBST_V 0)
  (match_operand:SUBST_V 1)
  (match_operand:SUBST_V 2)
  (match_operand:SUBST_V 3)
  (match_operand:SUBST_S 4)]
  "TARGET_AVX512F"
  [(match_dup 0)
   (match_dup 1)
   (match_dup 2)
   (match_dup 3)
   (match_dup 4)
   (unspec [(match_operand:SI 5 "const_4_or_8_to_11_operand")] UNSPEC_EMBEDDED_ROUNDING)])

(define_subst_attr "round_saeonly_expand_name" "round_saeonly_expand" "" "_round")
(define_subst_attr "round_saeonly_expand_nimm_predicate" "round_saeonly_expand" "nonimmediate_operand" "register_operand")
(define_subst_attr "round_saeonly_expand_operand6" "round_saeonly_expand" "" ", operands[6]")

(define_subst "round_saeonly_expand"
 [(match_operand:SUBST_V 0)
  (match_operand:SUBST_V 1)
  (match_operand:SUBST_V 2)
  (match_operand:SUBST_A 3)
  (match_operand:SI 4)
  (match_operand:SUBST_S 5)]
  "TARGET_AVX512F"
  [(match_dup 0)
   (match_dup 1)
   (match_dup 2)
   (match_dup 3)
   (match_dup 4)
   (match_dup 5)
   (unspec [(match_operand:SI 6 "const48_operand")] UNSPEC_EMBEDDED_ROUNDING)])

(define_subst_attr "mask_expand4_name" "mask_expand4" "" "_mask")
(define_subst_attr "mask_expand4_args" "mask_expand4" "" ", operands[4], operands[5]")

(define_subst "mask_expand4"
  [(match_operand:SUBST_V 0)
   (match_operand:SUBST_V 1)
   (match_operand:SUBST_V 2)
   (match_operand:SI 3)]
   "TARGET_AVX512VL"
   [(match_dup 0)
    (match_dup 1)
    (match_dup 2)
    (match_dup 3)
    (match_operand:SUBST_V 4 "nonimm_or_0_operand")
    (match_operand:<avx512fmaskmode> 5 "register_operand")])

(define_subst_attr "mask_scalar_name" "mask_scalar" "" "_mask")
(define_subst_attr "mask_scalarcz_name" "mask_scalarcz" "" "_maskz")
(define_subst_attr "mask_scalarc_name" "mask_scalarc" "" "_mask")
(define_subst_attr "mask_scalarc_operand3" "mask_scalarc" "" "%{%4%}%N3")
(define_subst_attr "mask_scalar_operand3" "mask_scalar" "" "%{%4%}%N3")
(define_subst_attr "mask_scalar_operand4" "mask_scalar" "" "%{%5%}%N4")
(define_subst_attr "mask_scalarcz_operand4" "mask_scalarcz" "" "%{%5%}%N4")
(define_subst_attr "mask_scalar4_dest_false_dep_for_glc_cond" "mask_scalar" "1" "operands[4] == CONST0_RTX(<MODE>mode)")
(define_subst_attr "mask_scalarc_dest_false_dep_for_glc_cond" "mask_scalarc" "1" "operands[3] == CONST0_RTX(V8HFmode)")
(define_subst_attr "mask_scalar_operand_arg34" "mask_scalar" "" ", operands[3], operands[4]")
(define_subst_attr "mask_scalar_expand_op3" "mask_scalar" "3" "5")

(define_subst "mask_scalar"
  [(set (match_operand:SUBST_V 0)
	(vec_merge:SUBST_V
	  (match_operand:SUBST_V 1)
	  (match_operand:SUBST_V 2)
	  (const_int 1)))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(vec_merge:SUBST_V
	  (vec_merge:SUBST_V
	    (match_dup 1)
	    (match_operand:SUBST_V 3 "nonimm_or_0_operand" "0C")
	    (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk"))
	  (match_dup 2)
	  (const_int 1)))])

(define_subst "mask_scalarcz"
  [(set (match_operand:SUBST_CV 0)
	(vec_merge:SUBST_CV
	  (match_operand:SUBST_CV 1)
	  (match_operand:SUBST_CV 2)
	  (const_int 3)))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(vec_merge:SUBST_CV
	  (vec_merge:SUBST_CV
	    (match_dup 1)
	    (match_operand:SUBST_CV 3 "const0_operand")
	    (unspec:<avx512fmaskmode>
	      [(match_operand:<avx512fmaskcmode> 4 "register_operand" "Yk")]
	      UNSPEC_COMPLEX_MASK))
	  (match_dup 2)
	  (const_int 3)))])

(define_subst "mask_scalarc"
  [(set (match_operand:SUBST_CV 0)
	(vec_merge:SUBST_CV
	  (match_operand:SUBST_CV 1)
	  (match_operand:SUBST_CV 2)
	  (const_int 3)))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(vec_merge:SUBST_CV
	  (vec_merge:SUBST_CV
	    (match_dup 1)
	    (match_operand:SUBST_CV 3 "nonimm_or_0_operand" "0C")
	    (unspec:<avx512fmaskmode>
	      [(match_operand:<avx512fmaskcmode> 4 "register_operand" "Yk")]
	      UNSPEC_COMPLEX_MASK))
	  (match_dup 2)
	  (const_int 3)))])

(define_subst_attr "round_scalar_name" "round_scalar" "" "_round")
(define_subst_attr "round_scalarcz_name" "round_scalarcz" "" "_round")
(define_subst_attr "round_scalar_mask_operand3" "mask_scalar" "%R3" "%R5")
(define_subst_attr "round_scalarc_mask_operand3" "mask_scalarc" "%R3" "%R5")
(define_subst_attr "round_scalarcz_mask_operand4" "mask_scalarcz" "%R4" "%R6")
(define_subst_attr "round_scalar_mask_op3" "round_scalar" "" "<round_scalar_mask_operand3>")
(define_subst_attr "round_scalarc_mask_op3" "round_scalarcz" "" "<round_scalarc_mask_operand3>")
(define_subst_attr "round_scalarcz_mask_op4" "round_scalarcz" "" "<round_scalarcz_mask_operand4>")
(define_subst_attr "round_scalar_constraint" "round_scalar" "vm" "v")
(define_subst_attr "round_scalarcz_constraint" "round_scalarcz" "vm" "v")
(define_subst_attr "round_scalar_prefix" "round_scalar" "vex" "evex")
(define_subst_attr "round_scalar_nimm_predicate" "round_scalar" "nonimmediate_operand" "register_operand")
(define_subst_attr "round_scalarcz_nimm_predicate" "round_scalarcz" "vector_operand" "register_operand")

(define_subst "round_scalar"
  [(set (match_operand:SUBST_V 0)
        (vec_merge:SUBST_V
          (match_operand:SUBST_V 1)
          (match_operand:SUBST_V 2)
          (const_int 1)))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(unspec:SUBST_V [
	     (vec_merge:SUBST_V
		(match_dup 1)
		(match_dup 2)
		(const_int 1))
	     (match_operand:SI 3 "const_4_or_8_to_11_operand")]
		UNSPEC_EMBEDDED_ROUNDING))])

(define_subst "round_scalarcz"
  [(set (match_operand:SUBST_V 0)
	(vec_merge:SUBST_V
	  (match_operand:SUBST_V 1)
	  (match_operand:SUBST_V 2)
	  (const_int 3)))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(unspec:SUBST_V [
	     (vec_merge:SUBST_V
		(match_dup 1)
		(match_dup 2)
		(const_int 3))
	     (match_operand:SI 3 "const_4_or_8_to_11_operand")]
		UNSPEC_EMBEDDED_ROUNDING))])

(define_subst_attr "round_saeonly_scalar_name" "round_saeonly_scalar" "" "_round")
(define_subst_attr "round_saeonly_scalar_mask_operand3" "mask_scalar" "%r3" "%r5")
(define_subst_attr "round_saeonly_scalar_mask_operand4" "mask_scalar" "%r4" "%r6")
(define_subst_attr "round_saeonly_scalar_mask_op3" "round_saeonly_scalar" "" "<round_saeonly_scalar_mask_operand3>")
(define_subst_attr "round_saeonly_scalar_mask_op4" "round_saeonly_scalar" "" "<round_saeonly_scalar_mask_operand4>")
(define_subst_attr "round_saeonly_scalar_constraint" "round_saeonly_scalar" "vm" "v")
(define_subst_attr "round_saeonly_scalar_prefix" "round_saeonly_scalar" "vex" "evex")
(define_subst_attr "round_saeonly_scalar_nimm_predicate" "round_saeonly_scalar" "nonimmediate_operand" "register_operand")
(define_subst_attr "round_saeonly_scalar_mask_arg3" "round_saeonly_scalar" "" ", operands[<mask_scalar_expand_op3>]")

(define_subst "round_saeonly_scalar"
  [(set (match_operand:SUBST_V 0)
        (vec_merge:SUBST_V
          (match_operand:SUBST_V 1)
          (match_operand:SUBST_V 2)
          (const_int 1)))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
	(unspec:SUBST_V [
	     (vec_merge:SUBST_V
		(match_dup 1)
		(match_dup 2)
		(const_int 1))
	     (match_operand:SI 3 "const48_operand")]
		UNSPEC_EMBEDDED_ROUNDING))])

(define_subst_attr "maskz_half_name" "maskz_half" "" "_maskz_1")
(define_subst_attr "maskz_half_operand4" "maskz_half" "" "%{%5%}%N4")

(define_subst "maskz_half"
  [(set (match_operand:SUBST_V 0)
        (match_operand:SUBST_V 1))]
  ""
  [(set (match_dup 0)
        (vec_merge:SUBST_V
	  (match_dup 1)
	  (match_operand:SUBST_V 2 "const0_operand")
	  (match_operand:<avx512fmaskhalfmode> 3 "register_operand" "Yk")))])
