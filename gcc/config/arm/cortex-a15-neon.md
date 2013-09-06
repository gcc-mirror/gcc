;; ARM Cortex-A15 NEON pipeline description
;; Copyright (C) 2012-2013 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "cortex_a15_neon")

;; Dispatch unit.
(define_cpu_unit "ca15_cx_ij, ca15_cx_ik" "cortex_a15_neon")

;; Accumulate.
(define_cpu_unit "ca15_cx_acc" "cortex_a15_neon")

;; The 32x32 integer multiply-accumulate pipeline.
(define_cpu_unit "ca15_cx_imac1" "cortex_a15_neon")
(define_reservation "ca15_cx_imac" "(ca15_cx_ij+ca15_cx_imac1)")


;; The 64-bit ALU pipeline.
(define_cpu_unit "ca15_cx_ialu1, ca15_cx_ialu2" "cortex_a15_neon")

;; IALU with accumulate.
(define_reservation "ca15_cx_ialu_with_acc" "ca15_cx_ik+ca15_cx_ialu2+ca15_cx_acc")

(define_reservation "ca15_cx_ialu"
                    "((ca15_cx_ij+ca15_cx_ialu1)|(ca15_cx_ik+ca15_cx_ialu2))")

;; Integer shift pipeline.
(define_cpu_unit "ca15_cx_ishf" "cortex_a15_neon")
(define_reservation "ca15_cx_ishf_with_acc" "ca15_cx_ik+ca15_cx_ishf+ca15_cx_acc")

;; SIMD multiply pipeline.
(define_cpu_unit "ca15_cx_fmul1, ca15_cx_fmul2, ca15_cx_fmul3, ca15_cx_fmul4"
                 "cortex_a15_neon")

(define_reservation "ca15_cx_fmul"
                    "(ca15_cx_ij+(ca15_cx_fmul1|ca15_cx_fmul2))|\
                     (ca15_cx_ik+(ca15_cx_fmul3|ca15_cx_fmul4))")

(define_reservation "ca15_cx_fmul_2"
                    "(ca15_cx_ij+(ca15_cx_fmul1|ca15_cx_fmul2))+\
                     (ca15_cx_ik+(ca15_cx_fmul3|ca15_cx_fmul4))")

;; SIMD ALU pipeline.
(define_cpu_unit "ca15_cx_falu1, ca15_cx_falu2, ca15_cx_falu3, ca15_cx_falu4"
                 "cortex_a15_neon")

(define_reservation "ca15_cx_falu"
                    "(ca15_cx_ij+(ca15_cx_falu1|ca15_cx_falu2))|\
                     (ca15_cx_ik+(ca15_cx_falu3|ca15_cx_falu4))")

(define_reservation "ca15_cx_falu_2"
                    "(ca15_cx_ij+(ca15_cx_falu1|ca15_cx_falu2))+\
                     (ca15_cx_ik+(ca15_cx_falu3|ca15_cx_falu4))")

;; SIMD multiply-accumulate pipeline.
;; This can be used if fmul and falu are not reserved.
(define_reservation "ca15_cx_fmac"
                    "((ca15_cx_ij+ca15_cx_fmul1),nothing*2,ca15_cx_falu1)|\
                     ((ca15_cx_ij+ca15_cx_fmul2),nothing*2,ca15_cx_falu2)|\
                     ((ca15_cx_ik+ca15_cx_fmul3),nothing*2,ca15_cx_falu3)|\
                     ((ca15_cx_ik+ca15_cx_fmul4),nothing*2,ca15_cx_falu4)")

(define_reservation "ca15_cx_fmac_2"
                    "(((ca15_cx_ij+ca15_cx_fmul1),nothing*2,ca15_cx_falu1)|\
                     ((ca15_cx_ij+ca15_cx_fmul2),nothing*2,ca15_cx_falu2))+\
                     (((ca15_cx_ik+ca15_cx_fmul3),nothing*2,ca15_cx_falu3)|\
                     ((ca15_cx_ik+ca15_cx_fmul4),nothing*2,ca15_cx_falu4))")


;; Vector FP multiply pipeline
(define_cpu_unit "ca15_cx_vfp_i" "cortex_a15_neon")

(define_reservation "ca15_cx_vfp" "ca15_cx_ik+ca15_cx_vfp_i")

;; Load permute pipeline
(define_reservation "ca15_cx_perm" "ca15_cx_ij|ca15_cx_ik")
(define_reservation "ca15_cx_perm_2" "ca15_cx_ij+ca15_cx_ik")

(define_insn_reservation  "cortex_a15_neon_int_1" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_int_1"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_int_2" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_int_2"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_int_3" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_int_3"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_int_4" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_int_4"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_int_5" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_int_5"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_vqneg_vqabs" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vqneg_vqabs"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_vmov" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vmov"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_vaba" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vaba"))
  "ca15_issue1,ca15_cx_ialu_with_acc")

(define_insn_reservation  "cortex_a15_neon_vaba_qqq" 8
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vaba_qqq"))
  "ca15_issue2,ca15_cx_ialu_with_acc*2")

(define_insn_reservation
  "cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long"))
  "ca15_issue1,ca15_cx_imac")

(define_insn_reservation "cortex_a15_neon_mul_qqq_8_16_32_ddd_32" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mul_qqq_8_16_32_ddd_32"))
  "ca15_issue1,ca15_cx_imac*2")

(define_insn_reservation
  "cortex_a15_neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type"
              "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"))
  "ca15_issue1,ca15_cx_imac*2")

(define_insn_reservation
  "cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type"
              "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long"))
  "ca15_issue1,ca15_cx_imac")

(define_insn_reservation
  "cortex_a15_neon_mla_qqq_8_16" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type"
              "neon_mla_qqq_8_16"))
  "ca15_issue1,ca15_cx_imac*2")

(define_insn_reservation
  "cortex_a15_neon_mla_ddd_32_qqd_16_ddd_32_scalar_\
     qdd_64_32_lotype_qdd_64_32_long" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type"  "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
  "ca15_issue1,ca15_cx_imac")

(define_insn_reservation
  "cortex_a15_neon_mla_qqq_32_qqd_32_scalar" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mla_qqq_32_qqd_32_scalar"))
  "ca15_issue1,ca15_cx_imac*2")

(define_insn_reservation
  "cortex_a15_neon_mul_ddd_16_scalar_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mul_ddd_16_scalar_32_16_long_scalar"))
  "ca15_issue1,ca15_cx_imac")

(define_insn_reservation
  "cortex_a15_neon_mul_qqd_32_scalar" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mul_qqd_32_scalar"))
  "ca15_issue1,ca15_cx_imac*2")

(define_insn_reservation
  "cortex_a15_neon_mla_ddd_16_scalar_qdd_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar"))
  "ca15_issue1,ca15_cx_imac")

(define_insn_reservation
  "cortex_a15_neon_shift_1" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_shift_1"))
  "ca15_issue1,ca15_cx_ik+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_shift_2" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_shift_2"))
  "ca15_issue1,ca15_cx_ik+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_shift_3" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_shift_3"))
  "ca15_issue2,(ca15_cx_ik+ca15_cx_ishf)*2")

(define_insn_reservation
  "cortex_a15_neon_vshl_ddd" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vshl_ddd"))
  "ca15_issue1,ca15_cx_ik+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_vqshl_vrshl_vqrshl_qqq" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vqshl_vrshl_vqrshl_qqq"))
  "ca15_issue2,(ca15_cx_ik+ca15_cx_ishf)*2")

(define_insn_reservation
  "cortex_a15_neon_vsra_vrsra" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vsra_vrsra"))
  "ca15_issue1,ca15_cx_ishf_with_acc")

(define_insn_reservation
  "cortex_a15_neon_fp_vadd_ddd_vabs_dd" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vadd_ddd_vabs_dd"))
  "ca15_issue1,ca15_cx_falu")

(define_insn_reservation
  "cortex_a15_neon_fp_vadd_qqq_vabs_qq" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vadd_qqq_vabs_qq"))
  "ca15_issue2,ca15_cx_falu_2")

(define_insn_reservation
  "cortex_a15_neon_fp_vmul_ddd" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vmul_ddd"))
  "ca15_issue1,ca15_cx_fmul")

(define_insn_reservation
  "cortex_a15_neon_fp_vmul_qqd" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vmul_qqd"))
  "ca15_issue2,ca15_cx_fmul_2")

(define_insn_reservation
  "cortex_a15_neon_fp_vmla_ddd" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vmla_ddd"))
  "ca15_issue1,ca15_cx_fmac")

(define_insn_reservation
  "cortex_a15_neon_fp_vmla_qqq" 11
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vmla_qqq"))
  "ca15_issue2,ca15_cx_fmac_2")

(define_insn_reservation
  "cortex_a15_neon_fp_vmla_ddd_scalar" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vmla_ddd_scalar"))
  "ca15_issue1,ca15_cx_fmac")

(define_insn_reservation
  "cortex_a15_neon_fp_vmla_qqq_scalar" 11
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vmla_qqq_scalar"))
  "ca15_issue2,ca15_cx_fmac_2")

(define_insn_reservation
  "cortex_a15_neon_fp_vrecps_vrsqrts_ddd" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vrecps_vrsqrts_ddd"))
  "ca15_issue1,ca15_cx_fmac")

(define_insn_reservation
  "cortex_a15_neon_fp_vrecps_vrsqrts_qqq" 11
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_fp_vrecps_vrsqrts_qqq"))
  "ca15_issue2,ca15_cx_fmac_2")

(define_insn_reservation
  "cortex_a15_neon_bp_simple" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_bp_simple"))
  "ca15_issue3,ca15_ls+ca15_cx_perm_2,ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_bp_2cycle" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_bp_2cycle"))
  "ca15_issue1,ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_bp_3cycle" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_bp_3cycle"))
  "ca15_issue3,ca15_cx_ialu+ca15_cx_perm_2,ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_vld1_1_2_regs" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld1_1_2_regs"))
  "ca15_issue2,ca15_ls,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_vld1_3_4_regs" 8
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld1_3_4_regs"))
  "ca15_issue3,ca15_ls1+ca15_ls2,ca15_ldr,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_vld2_2_regs_vld1_vld2_all_lanes" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld2_2_regs_vld1_vld2_all_lanes"))
  "ca15_issue3,ca15_ls,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_vld2_4_regs" 12
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld2_4_regs"))
  "ca15_issue3,ca15_issue3+ca15_ls1+ca15_ls2,ca15_ldr*2")

(define_insn_reservation
  "cortex_a15_neon_vld3_vld4" 12
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld3_vld4"))
  "ca15_issue3,ca15_issue3+ca15_ls1+ca15_ls2,ca15_ldr*2")

(define_insn_reservation
  "cortex_a15_neon_vst1_1_2_regs_vst2_2_regs" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vst1_1_2_regs_vst2_2_regs"))
  "ca15_issue3,ca15_issue3+ca15_cx_perm+ca15_ls1+ca15_ls2,ca15_str*2")

(define_insn_reservation
  "cortex_a15_neon_vst1_3_4_regs" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vst1_3_4_regs"))
  "ca15_issue3,ca15_issue3+ca15_ls1+ca15_ls2,ca15_str*3")

(define_insn_reservation
  "cortex_a15_neon_vst2_4_regs_vst3_vst4" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vst2_4_regs_vst3_vst4"))
  "ca15_issue3,ca15_issue3+ca15_cx_perm_2+ca15_ls1+ca15_ls2,\
   ca15_issue3+ca15_str,ca15_str*3")

(define_insn_reservation
  "cortex_a15_neon_vst3_vst4" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vst3_vst4"))
  "ca15_issue3,ca15_issue3+ca15_cx_perm_2+ca15_ls1+ca15_ls2,ca15_str*4")

(define_insn_reservation
  "cortex_a15_neon_vld1_vld2_lane" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld1_vld2_lane"))
  "ca15_issue3,ca15_ls,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_vld3_vld4_lane" 10
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld3_vld4_lane"))
  "ca15_issue3,ca15_issue3+ca15_ls,ca15_issue3+ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_vst1_vst2_lane" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vst1_vst2_lane"))
  "ca15_issue3,ca15_cx_perm+ca15_ls,ca15_str")

(define_insn_reservation
  "cortex_a15_neon_vst3_vst4_lane" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vst3_vst4_lane"))
  "ca15_issue3,ca15_issue3+ca15_cx_perm+ca15_ls1+ca15_ls2,ca15_str*2")

(define_insn_reservation
  "cortex_a15_neon_vld3_vld4_all_lanes" 11
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_vld3_vld4_all_lanes"))
  "ca15_issue3,ca15_issue3+ca15_ls,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_ldm_2" 20
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_ldm_2"))
  "ca15_issue3*6")

(define_insn_reservation
  "cortex_a15_neon_stm_2" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_stm_2"))
  "ca15_issue3*6")

(define_insn_reservation
  "cortex_a15_neon_mcr" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mcr"))
  "ca15_issue2,ca15_ls,ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_mcr_2_mcrr" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mcr_2_mcrr"))
  "ca15_issue2,ca15_ls1+ca15_ls2")

(define_insn_reservation
  "cortex_a15_neon_mrc" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mrc"))
  "ca15_issue1,ca15_ls")

(define_insn_reservation
  "cortex_a15_neon_mrrc" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "neon_mrrc"))
  "ca15_issue2,ca15_ls1+ca15_ls2")

(define_insn_reservation "cortex_a15_vfp_const" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fconsts,fconstd"))
  "ca15_issue1,ca15_cx_perm")

(define_insn_reservation "cortex_a15_vfp_adds_subs" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fadds"))
  "ca15_issue1,ca15_cx_vfp")

(define_insn_reservation "cortex_a15_vfp_addd_subd" 10
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "faddd"))
  "ca15_issue2,ca15_cx_vfp*2")

(define_insn_reservation "cortex_a15_vfp_muls" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fmuls"))
  "ca15_issue1,ca15_cx_vfp")

(define_insn_reservation "cortex_a15_vfp_muld" 12
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fmuld"))
  "ca15_issue2,ca15_cx_vfp*2")

(define_insn_reservation "cortex_a15_vfp_macs" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fmacs,ffmas"))
  "ca15_issue1,ca15_cx_vfp")

(define_insn_reservation "cortex_a15_vfp_macd" 11
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fmacd,ffmad"))
  "ca15_issue2,ca15_cx_vfp*2")

(define_insn_reservation "cortex_a15_vfp_cvt" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "f_cvt,f_cvtf2i,f_cvti2f"))
  "ca15_issue1,ca15_cx_vfp")

(define_insn_reservation "cortex_a15_vfp_cmpd" 8
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fcmpd"))
  "ca15_issue2,ca15_cx_perm,ca15_cx_vfp")

(define_insn_reservation "cortex_a15_vfp_cmps" 8
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fcmps"))
  "ca15_issue2,ca15_cx_perm,ca15_cx_vfp")

(define_insn_reservation "cortex_a15_vfp_arithd" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "ffarithd"))
  "ca15_issue2,ca15_cx_perm*2")

(define_insn_reservation "cortex_a15_vfp_cpys" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fmov"))
  "ca15_issue1,ca15_cx_perm")

(define_insn_reservation "cortex_a15_vfp_ariths" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "ffariths"))
  "ca15_issue1,ca15_cx_perm")

(define_insn_reservation "cortex_a15_vfp_divs" 10
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fdivs, fsqrts"))
  "ca15_issue1,ca15_cx_ik")

(define_insn_reservation "cortex_a15_vfp_divd" 18
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "fdivd, fsqrtd"))
  "ca15_issue1,ca15_cx_ik")

;; Define bypasses.
(define_bypass 5 "cortex_a15_neon_mcr_2_mcrr"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_mcr"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 10 "cortex_a15_neon_vld3_vld4_all_lanes"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 9 "cortex_a15_neon_vld3_vld4_lane"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a15_neon_vld1_vld2_lane"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 11 "cortex_a15_neon_vld3_vld4"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 11 "cortex_a15_neon_vld2_4_regs"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a15_neon_vld2_2_regs_vld1_vld2_all_lanes"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 7 "cortex_a15_neon_vld1_3_4_regs"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_vld1_1_2_regs"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_bp_3cycle"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a15_neon_bp_2cycle"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a15_neon_bp_simple"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 10 "cortex_a15_neon_fp_vrecps_vrsqrts_qqq"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a15_neon_fp_vrecps_vrsqrts_ddd"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 10 "cortex_a15_neon_fp_vmla_qqq_scalar"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a15_neon_fp_vmla_ddd_scalar"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 10 "cortex_a15_neon_fp_vmla_qqq"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a15_neon_fp_vmla_ddd"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_fp_vmul_qqd"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_fp_vmul_ddd"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_fp_vadd_qqq_vabs_qq"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_fp_vadd_ddd_vabs_dd"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_vsra_vrsra"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_vqshl_vrshl_vqrshl_qqq"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_vshl_ddd"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_shift_3"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_shift_2"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_shift_1"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_mla_ddd_16_scalar_qdd_32_16_long_scalar"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_mul_qqd_32_scalar"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_mul_ddd_16_scalar_32_16_long_scalar"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_mla_qqq_32_qqd_32_scalar"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_mla_qqq_8_16"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6
     "cortex_a15_neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_mul_qqq_8_16_32_ddd_32"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 7 "cortex_a15_neon_vaba_qqq"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a15_neon_vaba"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_vmov"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_vqneg_vqabs"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_int_5"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_int_4"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_int_3"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_int_2"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a15_neon_int_1"
               "cortex_a15_neon_int_1,\
               cortex_a15_neon_int_4,\
               cortex_a15_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a15_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a15_neon_mla_qqq_8_16,\
               cortex_a15_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a15_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a15_neon_fp_vmla_ddd,\
               cortex_a15_neon_fp_vmla_qqq,\
               cortex_a15_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a15_neon_fp_vrecps_vrsqrts_qqq")

