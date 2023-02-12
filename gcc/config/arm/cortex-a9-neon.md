;; ARM Cortex-A9 pipeline description
;; Copyright (C) 2010-2023 Free Software Foundation, Inc.
;;
;; Neon pipeline description contributed by ARM Ltd.
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

(define_attr "cortex_a9_neon_type"
   "neon_int_1,neon_int_2,neon_int_3,neon_int_4,neon_int_5,neon_vqneg_vqabs,
   neon_bit_ops_q,
   neon_vaba,neon_vaba_qqq, neon_vmov,
   neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,neon_mul_qqq_8_16_32_ddd_32,
   neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar,
   neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,neon_mla_qqq_8_16,
   neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long,
   neon_mla_qqq_32_qqd_32_scalar,neon_mul_ddd_16_scalar_32_16_long_scalar,
   neon_mul_qqd_32_scalar,neon_mla_ddd_16_scalar_qdd_32_16_long_scalar,
   neon_shift_1,neon_shift_2,neon_shift_3,
   neon_vqshl_vrshl_vqrshl_qqq,neon_vsra_vrsra,neon_fp_vadd_ddd_vabs_dd,
   neon_fp_vadd_qqq_vabs_qq,neon_fp_vsum,neon_fp_vmul_ddd,neon_fp_vmul_qqd,
   neon_fp_vmla_ddd,neon_fp_vmla_qqq,neon_fp_vmla_ddd_scalar,
   neon_fp_vmla_qqq_scalar,neon_fp_vrecps_vrsqrts_ddd,
   neon_fp_vrecps_vrsqrts_qqq,neon_bp_simple,neon_bp_2cycle,neon_bp_3cycle,
   neon_ldr,neon_str,neon_vld1_1_2_regs,neon_vld1_3_4_regs,
   neon_vld2_2_regs_vld1_vld2_all_lanes,neon_vld2_4_regs,neon_vld3_vld4,
   neon_vst1_1_2_regs_vst2_2_regs,neon_vst1_3_4_regs,
   neon_vst2_4_regs_vst3_vst4,neon_vld1_vld2_lane,
   neon_vld3_vld4_lane,neon_vst1_vst2_lane,neon_vst3_vst4_lane,
   neon_vld3_vld4_all_lanes,neon_mcr,neon_mcr_2_mcrr,neon_mrc,neon_mrrc,
   neon_ldm_2,neon_stm_2,none,unknown"
  (cond [
          (eq_attr "type" "neon_logic, neon_logic_q,\
                           neon_bsl, neon_cls, neon_cnt,\
                           neon_add, neon_add_q")
                          (const_string "neon_int_1")
          (eq_attr "type" "neon_add_widen, neon_sub_widen,\
                           neon_sub, neon_sub_q")
                          (const_string "neon_int_2")
          (eq_attr "type" "neon_neg, neon_neg_q,\
                           neon_reduc_add, neon_reduc_add_q,\
                           neon_reduc_add_long,\
                           neon_add_long, neon_sub_long")
                          (const_string "neon_int_3")
          (eq_attr "type" "neon_abs, neon_abs_q,
                           neon_compare_zero, neon_compare_zero_q,\
                           neon_add_halve_narrow_q,\
                           neon_sub_halve_narrow_q,\
                           neon_add_halve, neon_add_halve_q,\
                           neon_qadd, neon_qadd_q,\
                           neon_tst, neon_tst_q")
                          (const_string "neon_int_4")
          (eq_attr "type" "neon_abd_long, neon_sub_halve, neon_sub_halve_q,\
                           neon_qsub, neon_qsub_q,\
                           neon_abd, neon_abd_q,\
                           neon_compare, neon_compare_q,\
                           neon_minmax, neon_minmax_q, neon_reduc_minmax,\
                           neon_reduc_minmax_q")
                          (const_string "neon_int_5")
          (eq_attr "type" "neon_qneg, neon_qneg_q, neon_qabs, neon_qabs_q")
                           (const_string "neon_vqneg_vqabs")
          (eq_attr "type" "neon_move, neon_move_q")
                           (const_string "neon_vmov")
          (eq_attr "type" "neon_bsl_q, neon_cls_q, neon_cnt_q")
                           (const_string "neon_bit_ops_q")
          (eq_attr "type" "neon_arith_acc, neon_reduc_add_acc")
                          (const_string "neon_vaba")
          (eq_attr "type" "neon_arith_acc_q")
                          (const_string "neon_vaba_qqq")
          (eq_attr "type" "neon_shift_imm, neon_shift_imm_q,\
                           neon_shift_imm_long, neon_shift_imm_narrow_q,\
                           neon_shift_reg")
                           (const_string "neon_shift_1")
          (eq_attr "type" "neon_sat_shift_imm, neon_sat_shift_imm_q,
                           neon_sat_shift_imm_narrow_q,\
                           neon_sat_shift_reg")
                           (const_string "neon_shift_2")
          (eq_attr "type" "neon_shift_reg_q")
                           (const_string "neon_shift_3")
          (eq_attr "type" "neon_sat_shift_reg_q")
                           (const_string "neon_vqshl_vrshl_vqrshl_qqq")
          (eq_attr "type" "neon_shift_acc, neon_shift_acc_q")
                           (const_string "neon_vsra_vrsra")
          (eq_attr "type" "neon_mul_b, neon_mul_h,\
                           neon_mul_b_long, neon_mul_h_long,\
                           neon_sat_mul_b, neon_sat_mul_h,\
                           neon_sat_mul_b_long, neon_sat_mul_h_long")
                           (const_string
                            "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
          (eq_attr "type" "neon_mul_b_q, neon_mul_h_q,\
                           neon_sat_mul_b_q, neon_sat_mul_h_q")
                           (const_string "neon_mul_qqq_8_16_32_ddd_32")
          (eq_attr "type" "neon_mul_s, neon_mul_s_long,\
                           neon_sat_mul_s, neon_sat_mul_s_long,\
                           neon_mul_h_scalar_q, neon_sat_mul_h_scalar_q,\
                           neon_mul_s_scalar, neon_sat_mul_s_scalar,\
                           neon_mul_s_scalar_long,\
                           neon_sat_mul_s_scalar_long")
                           (const_string
             "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")
          (eq_attr "type" "neon_mla_b, neon_mla_h,\
                           neon_mla_b_long, neon_mla_h_long,\
                           neon_sat_mla_b_long, neon_sat_mla_h_long,\
                           neon_sat_mla_h_scalar_long")
                           (const_string
                             "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
          (eq_attr "type" "neon_mla_b_q, neon_mla_h_q")
                           (const_string "neon_mla_qqq_8_16")
          (eq_attr "type" "neon_mla_s, neon_mla_s_long,\
                           neon_sat_mla_s_long,\
                           neon_mla_h_scalar_q, neon_mla_s_scalar,\
                           neon_mla_s_scalar_long,\
                           neon_sat_mla_s_scalar_long")
                           (const_string
 "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")
          (eq_attr "type" "neon_mla_s_q, neon_mla_s_scalar_q")
                           (const_string "neon_mla_qqq_32_qqd_32_scalar")
          (eq_attr "type" "neon_mul_h_scalar, neon_sat_mul_h_scalar,\
                           neon_mul_h_scalar_long,\
                           neon_sat_mul_h_scalar_long")
                          (const_string
                            "neon_mul_ddd_16_scalar_32_16_long_scalar")
          (eq_attr "type" "neon_mul_s_q, neon_sat_mul_s_q,\
                           neon_mul_s_scalar_q")
                           (const_string "neon_mul_qqd_32_scalar")
          (eq_attr "type" "neon_mla_h_scalar, neon_mla_h_scalar_long")
                           (const_string
                             "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
          (eq_attr "type" "neon_fp_abd_s, neon_fp_abs_s, neon_fp_neg_s,\
                           neon_fp_addsub_s, neon_fp_compare_s,\
                           neon_fp_minmax_s, neon_fp_mul_s,\
                           neon_fp_recpe_s, neon_fp_rsqrte_s,\
                           neon_fp_to_int_s, neon_int_to_fp_s")
                           (const_string "neon_fp_vadd_ddd_vabs_dd")
          (eq_attr "type" "neon_fp_abd_s_q, neon_fp_abs_s_q,\
                           neon_fp_neg_s_q,\
                           neon_fp_addsub_s_q, neon_fp_compare_s_q,\
                           neon_fp_minmax_s_q, neon_fp_mul_s_q,\
                           neon_fp_recpe_s_q, neon_fp_rsqrte_s_q,\
                           neon_fp_to_int_s_q, neon_int_to_fp_s_q")
                           (const_string "neon_fp_vadd_qqq_vabs_qq")
          (eq_attr "type" "neon_fp_reduc_add_s, neon_fp_reduc_minmax_s,\
                           neon_fp_reduc_add_s_q, neon_fp_reduc_minmax_s_q")
                           (const_string "neon_fp_vsum")
          (eq_attr "type" "neon_fp_mul_s_scalar")
                           (const_string "neon_fp_vmul_ddd")
          (eq_attr "type" "neon_fp_mul_s_scalar_q")
                           (const_string "neon_fp_vmul_qqd")
          (eq_attr "type" "neon_fp_mla_s")
                           (const_string "neon_fp_vmla_ddd")
          (eq_attr "type" "neon_fp_mla_s_q")
                           (const_string "neon_fp_vmla_qqq")
          (eq_attr "type" "neon_fp_mla_s_scalar")
                           (const_string "neon_fp_vmla_ddd_scalar")
          (eq_attr "type" "neon_fp_mla_s_scalar_q")
                           (const_string "neon_fp_vmla_qqq_scalar")
          (eq_attr "type" "neon_fp_recps_s, neon_fp_rsqrts_s")
                           (const_string "neon_fp_vrecps_vrsqrts_ddd")
          (eq_attr "type" "neon_fp_recps_s_q, neon_fp_rsqrts_s_q")
                           (const_string "neon_fp_vrecps_vrsqrts_qqq")
          (eq_attr "type" "neon_move_narrow_q, neon_dup,\
                           neon_dup_q, neon_permute, neon_zip,\
                           neon_ext, neon_rev, neon_rev_q")
                           (const_string "neon_bp_simple")
          (eq_attr "type" "neon_permute_q, neon_ext_q, neon_tbl1, neon_tbl2")
                           (const_string "neon_bp_2cycle")
          (eq_attr "type" "neon_zip_q, neon_tbl3, neon_tbl4")
                           (const_string "neon_bp_3cycle")
          (eq_attr "type" "neon_ldr")
                           (const_string "neon_ldr")
          (eq_attr "type" "neon_str")
                           (const_string "neon_str")
          (eq_attr "type" "neon_load1_1reg, neon_load1_1reg_q,\
                           neon_load1_2reg, neon_load1_2reg_q,\
                           neon_load2_2reg, neon_load2_2reg_q")
                           (const_string "neon_vld1_1_2_regs")
          (eq_attr "type" "neon_load1_3reg, neon_load1_3reg_q,\
                           neon_load1_4reg, neon_load1_4reg_q")
                           (const_string "neon_vld1_3_4_regs")
          (eq_attr "type" "neon_load1_all_lanes, neon_load1_all_lanes_q,\
                           neon_load2_all_lanes, neon_load2_all_lanes_q")
                           (const_string
                              "neon_vld2_2_regs_vld1_vld2_all_lanes")
          (eq_attr "type" "neon_load3_all_lanes, neon_load3_all_lanes_q,\
                           neon_load4_all_lanes, neon_load4_all_lanes_q,\
                           neon_load2_4reg, neon_load2_4reg_q")
                           (const_string "neon_vld2_4_regs")
          (eq_attr "type" "neon_load3_3reg, neon_load3_3reg_q,\
                           neon_load4_4reg, neon_load4_4reg_q")
                           (const_string "neon_vld3_vld4")
          (eq_attr "type" "neon_load1_one_lane, neon_load1_one_lane_q,\
                           neon_load2_one_lane, neon_load2_one_lane_q")
                           (const_string "neon_vld1_vld2_lane")
          (eq_attr "type" "neon_load3_one_lane, neon_load3_one_lane_q,\
                           neon_load4_one_lane, neon_load4_one_lane_q")
                           (const_string "neon_vld3_vld4_lane")
          (eq_attr "type" "neon_store1_1reg, neon_store1_1reg_q,\
                           neon_store1_2reg, neon_store1_2reg_q,\
                           neon_store2_2reg, neon_store2_2reg_q")
                           (const_string "neon_vst1_1_2_regs_vst2_2_regs")
          (eq_attr "type" "neon_store1_3reg, neon_store1_3reg_q,\
                           neon_store1_4reg, neon_store1_4reg_q")
                           (const_string "neon_vst1_3_4_regs")
          (eq_attr "type" "neon_store2_4reg, neon_store2_4reg_q,\
                           neon_store3_3reg, neon_store3_3reg_q,\
                           neon_store4_4reg, neon_store4_4reg_q")
                           (const_string "neon_vst2_4_regs_vst3_vst4")
          (eq_attr "type" "neon_store1_one_lane, neon_store1_one_lane_q,\
                           neon_store2_one_lane, neon_store2_one_lane_q")
                           (const_string "neon_vst1_vst2_lane")
          (eq_attr "type" "neon_store3_one_lane, neon_store3_one_lane_q,\
                           neon_store4_one_lane, neon_store4_one_lane_q")
                           (const_string "neon_vst3_vst4_lane")
          (eq_attr "type" "neon_from_gp")
                           (const_string "neon_mcr")
          (eq_attr "type" "neon_from_gp_q")
                           (const_string "neon_mcr_2_mcrr")
          (eq_attr "type" "neon_to_gp")
                           (const_string "neon_mrc")
          (eq_attr "type" "neon_to_gp_q")
                           (const_string "neon_mrrc")]
          (const_string "unknown")))

(define_automaton "cortex_a9_neon")

;; Only one instruction can be issued per cycle.
(define_cpu_unit "cortex_a9_neon_issue_perm" "cortex_a9_neon")

;; Only one data-processing instruction can be issued per cycle.
(define_cpu_unit "cortex_a9_neon_issue_dp" "cortex_a9_neon")

;; We need a special mutual exclusion (to be used in addition to
;; cortex_a9_neon_issue_dp) for the case when an instruction such as
;; vmla.f is forwarded from E5 of the floating-point multiply pipeline to
;; E2 of the floating-point add pipeline.  On the cycle previous to that
;; forward we must prevent issue of any instruction to the floating-point
;; add pipeline, but still allow issue of a data-processing instruction
;; to any of the other pipelines.
(define_cpu_unit "cortex_a9_neon_issue_fadd" "cortex_a9_neon")
(define_cpu_unit "cortex_a9_neon_mcr" "cortex_a9_neon")


;; Patterns of reservation.
;; We model the NEON issue units as running in parallel with the core ones.
;; We assume that multi-cycle NEON instructions get decomposed into
;; micro-ops as they are issued into the NEON pipeline.

(define_reservation "cortex_a9_neon_dp"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_dp")
(define_reservation "cortex_a9_neon_dp_2"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_dp,\
                     cortex_a9_neon_issue_dp")
(define_reservation "cortex_a9_neon_dp_4"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_dp,\
                     cortex_a9_neon_issue_dp + cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp + cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp")

(define_reservation "cortex_a9_neon_fadd"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_dp +  \
                     cortex_a9_neon_issue_fadd")
(define_reservation "cortex_a9_neon_fadd_2"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_dp,\
                     cortex_a9_neon_issue_fadd,\
                     cortex_a9_neon_issue_dp")

(define_reservation "cortex_a9_neon_perm"
                    "ca9_issue_vfp_neon+cortex_a9_neon_issue_perm")
(define_reservation "cortex_a9_neon_perm_2"
                    "ca9_issue_vfp_neon+cortex_a9_neon_issue_perm,  \
                     cortex_a9_neon_issue_perm")
(define_reservation "cortex_a9_neon_perm_3"
                    "ca9_issue_vfp_neon+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_perm")

(define_reservation "cortex_a9_neon_ls"
                    "ca9_issue_vfp_neon+cortex_a9_neon_issue_perm+cortex_a9_ls")
(define_reservation "cortex_a9_neon_ls_2"
                    "ca9_issue_vfp_neon+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_perm")
(define_reservation "cortex_a9_neon_ls_3"
                    "ca9_issue_vfp_neon+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_perm")
(define_reservation "cortex_a9_neon_ls_4"
                    "ca9_issue_vfp_neon+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_perm")
(define_reservation "cortex_a9_neon_ls_5"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_dp+cortex_a9_neon_issue_perm,\
                     cortex_a9_neon_issue_perm")

(define_reservation "cortex_a9_neon_fmul_then_fadd"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_dp,\
		     nothing*3,\
		     cortex_a9_neon_issue_fadd")
(define_reservation "cortex_a9_neon_fmul_then_fadd_2"
                    "ca9_issue_vfp_neon + cortex_a9_neon_issue_dp,\
		     cortex_a9_neon_issue_dp,\
		     nothing*2,\
		     cortex_a9_neon_issue_fadd,\
		     cortex_a9_neon_issue_fadd")

;; NEON -> core transfers.
(define_insn_reservation "ca9_neon_mrc" 1
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mrc"))
  "ca9_issue_vfp_neon + cortex_a9_neon_mcr")

(define_insn_reservation "ca9_neon_mrrc" 1
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mrrc"))
  "ca9_issue_vfp_neon + cortex_a9_neon_mcr")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N3.
(define_insn_reservation "cortex_a9_neon_int_1" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_int_1"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)n operands at N2, and produce a result at N3.
(define_insn_reservation "cortex_a9_neon_int_2" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_int_2"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3.
(define_insn_reservation "cortex_a9_neon_int_3" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_int_3"))
   "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N4.
(define_insn_reservation "cortex_a9_neon_int_4" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_int_4"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)n operands at N2, and produce a result at N4.
(define_insn_reservation "cortex_a9_neon_int_5" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_int_5"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4.
(define_insn_reservation "cortex_a9_neon_vqneg_vqabs" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vqneg_vqabs"))
   "cortex_a9_neon_dp")

;; Instructions using this reservation produce a result at N3.
(define_insn_reservation "cortex_a9_neon_vmov" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vmov"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_vaba" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vaba"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vaba_qqq" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vaba_qqq"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N3 on cycle 2.
(define_insn_reservation "cortex_a9_neon_bit_ops_q" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_bit_ops_q"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mul_qqq_8_16_32_ddd_32" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mul_qqq_8_16_32_ddd_32"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mla_qqq_8_16" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mla_qqq_8_16"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 4.
(define_insn_reservation "cortex_a9_neon_mla_qqq_32_qqd_32_scalar" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mla_qqq_32_qqd_32_scalar"))
  "cortex_a9_neon_dp_4")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mul_ddd_16_scalar_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mul_ddd_16_scalar_32_16_long_scalar"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6 on cycle 4.
(define_insn_reservation "cortex_a9_neon_mul_qqd_32_scalar" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mul_qqd_32_scalar"))
  "cortex_a9_neon_dp_4")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mla_ddd_16_scalar_qdd_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3.
(define_insn_reservation "cortex_a9_neon_shift_1" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_shift_1"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4.
(define_insn_reservation "cortex_a9_neon_shift_2" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_shift_2"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3 on cycle 2.
(define_insn_reservation "cortex_a9_neon_shift_3" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_shift_3"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vqshl_vrshl_vqrshl_qqq" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vqshl_vrshl_vqrshl_qqq"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)d operands at N3, and produce a result at N6.
(define_insn_reservation "cortex_a9_neon_vsra_vrsra" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vsra_vrsra"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N5.
(define_insn_reservation "cortex_a9_neon_fp_vadd_ddd_vabs_dd" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vadd_ddd_vabs_dd"))
  "cortex_a9_neon_fadd")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N5 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vadd_qqq_vabs_qq" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vadd_qqq_vabs_qq"))
  "cortex_a9_neon_fadd_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N5.
(define_insn_reservation "cortex_a9_neon_fp_vsum" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vsum"))
  "cortex_a9_neon_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N5.
(define_insn_reservation "cortex_a9_neon_fp_vmul_ddd" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vmul_ddd"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N5 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vmul_qqd" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vmul_qqd"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N9.
(define_insn_reservation "cortex_a9_neon_fp_vmla_ddd" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vmla_ddd"))
  "cortex_a9_neon_fmul_then_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vmla_qqq" 10
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vmla_qqq"))
  "cortex_a9_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N9.
(define_insn_reservation "cortex_a9_neon_fp_vmla_ddd_scalar" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vmla_ddd_scalar"))
  "cortex_a9_neon_fmul_then_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vmla_qqq_scalar" 10
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vmla_qqq_scalar"))
  "cortex_a9_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N9.
(define_insn_reservation "cortex_a9_neon_fp_vrecps_vrsqrts_ddd" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vrecps_vrsqrts_ddd"))
  "cortex_a9_neon_fmul_then_fadd")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vrecps_vrsqrts_qqq" 10
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_fp_vrecps_vrsqrts_qqq"))
  "cortex_a9_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2.
(define_insn_reservation "cortex_a9_neon_bp_simple" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_bp_simple"))
  "cortex_a9_neon_perm")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a9_neon_bp_2cycle" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_bp_2cycle"))
  "cortex_a9_neon_perm_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a9_neon_bp_3cycle" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_bp_3cycle"))
  "cortex_a9_neon_perm_3")

;; Instructions using this reservation produce a result at N1.
(define_insn_reservation "cortex_a9_neon_ldr" 1
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_ldr"))
  "cortex_a9_neon_ls")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_str" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_str"))
  "cortex_a9_neon_ls")

;; Instructions using this reservation produce a result at N1 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vld1_1_2_regs" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld1_1_2_regs"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation produce a result at N1 on cycle 3.
(define_insn_reservation "cortex_a9_neon_vld1_3_4_regs" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld1_3_4_regs"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vld2_2_regs_vld1_vld2_all_lanes" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld2_2_regs_vld1_vld2_all_lanes"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a9_neon_vld2_4_regs" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld2_4_regs"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 4.
(define_insn_reservation "cortex_a9_neon_vld3_vld4" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld3_vld4"))
  "cortex_a9_neon_ls_4")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst1_1_2_regs_vst2_2_regs" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vst1_1_2_regs_vst2_2_regs"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst1_3_4_regs" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vst1_3_4_regs"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst2_4_regs_vst3_vst4" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vst2_4_regs_vst3_vst4"))
  "cortex_a9_neon_ls_4")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a9_neon_vld1_vld2_lane" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld1_vld2_lane"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 5.
(define_insn_reservation "cortex_a9_neon_vld3_vld4_lane" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld3_vld4_lane"))
  "cortex_a9_neon_ls_5")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst1_vst2_lane" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vst1_vst2_lane"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst3_vst4_lane" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vst3_vst4_lane"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vld3_vld4_all_lanes" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_vld3_vld4_all_lanes"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2.
(define_insn_reservation "cortex_a9_neon_mcr" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mcr"))
  "cortex_a9_neon_perm")

;; Instructions using this reservation produce a result at N2.
(define_insn_reservation "cortex_a9_neon_mcr_2_mcrr" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "cortex_a9_neon_type" "neon_mcr_2_mcrr"))
  "cortex_a9_neon_perm_2")

;; Exceptions to the default latencies.

(define_bypass 1 "cortex_a9_neon_mcr_2_mcrr"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 1 "cortex_a9_neon_mcr"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_vld3_vld4_all_lanes"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_vld3_vld4_lane"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_vld1_vld2_lane"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a9_neon_vld3_vld4"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_vld2_4_regs"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_vld2_2_regs_vld1_vld2_all_lanes"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_vld1_3_4_regs"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 1 "cortex_a9_neon_vld1_1_2_regs"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 0 "cortex_a9_neon_ldr"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_bp_3cycle"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_bp_2cycle"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 1 "cortex_a9_neon_bp_simple"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 9 "cortex_a9_neon_fp_vrecps_vrsqrts_qqq"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a9_neon_fp_vrecps_vrsqrts_ddd"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 9 "cortex_a9_neon_fp_vmla_qqq_scalar"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a9_neon_fp_vmla_ddd_scalar"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 9 "cortex_a9_neon_fp_vmla_qqq"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a9_neon_fp_vmla_ddd"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_fp_vmul_qqd"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a9_neon_fp_vmul_ddd"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a9_neon_fp_vsum"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_fp_vadd_qqq_vabs_qq"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a9_neon_fp_vadd_ddd_vabs_dd"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_vsra_vrsra"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a9_neon_vqshl_vrshl_vqrshl_qqq"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_shift_3"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_shift_2"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_shift_1"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_mla_ddd_16_scalar_qdd_32_16_long_scalar"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a9_neon_mul_qqd_32_scalar"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_mul_ddd_16_scalar_32_16_long_scalar"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a9_neon_mla_qqq_32_qqd_32_scalar"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a9_neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a9_neon_mla_qqq_8_16"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a9_neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a9_neon_mul_qqq_8_16_32_ddd_32"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a9_neon_vaba_qqq"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a9_neon_vaba"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_vmov"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_bit_ops_q"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_vqneg_vqabs"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_int_5"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a9_neon_int_4"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_int_3"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_int_2"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a9_neon_int_1"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
               cortex_a9_neon_bit_ops_q,\
               cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a9_neon_mla_qqq_8_16,\
               cortex_a9_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a9_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a9_neon_fp_vmla_ddd,\
               cortex_a9_neon_fp_vmla_qqq,\
               cortex_a9_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a9_neon_fp_vrecps_vrsqrts_qqq")

