;; ARM Cortex-A9 pipeline description
;; Copyright (C) 2010 Free Software Foundation, Inc.
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
       (eq_attr "neon_type" "neon_mrc"))
  "ca9_issue_vfp_neon + cortex_a9_neon_mcr")

(define_insn_reservation "ca9_neon_mrrc" 1
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mrrc"))
  "ca9_issue_vfp_neon + cortex_a9_neon_mcr")

;; The remainder of this file is auto-generated by neon-schedgen.

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N3.
(define_insn_reservation "cortex_a9_neon_int_1" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_int_1"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)n operands at N2, and produce a result at N3.
(define_insn_reservation "cortex_a9_neon_int_2" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_int_2"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3.
(define_insn_reservation "cortex_a9_neon_int_3" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_int_3"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N4.
(define_insn_reservation "cortex_a9_neon_int_4" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_int_4"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)n operands at N2, and produce a result at N4.
(define_insn_reservation "cortex_a9_neon_int_5" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_int_5"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4.
(define_insn_reservation "cortex_a9_neon_vqneg_vqabs" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vqneg_vqabs"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation produce a result at N3.
(define_insn_reservation "cortex_a9_neon_vmov" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vmov"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_vaba" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vaba"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vaba_qqq" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vaba_qqq"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)d operands at N3, and produce a result at N6.
(define_insn_reservation "cortex_a9_neon_vsma" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vsma"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mul_qqq_8_16_32_ddd_32" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mul_qqq_8_16_32_ddd_32"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mla_qqq_8_16" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mla_qqq_8_16"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a9_neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long" 7
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 4.
(define_insn_reservation "cortex_a9_neon_mla_qqq_32_qqd_32_scalar" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mla_qqq_32_qqd_32_scalar"))
  "cortex_a9_neon_dp_4")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mul_ddd_16_scalar_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mul_ddd_16_scalar_32_16_long_scalar"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6 on cycle 4.
(define_insn_reservation "cortex_a9_neon_mul_qqd_32_scalar" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mul_qqd_32_scalar"))
  "cortex_a9_neon_dp_4")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a9_neon_mla_ddd_16_scalar_qdd_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3.
(define_insn_reservation "cortex_a9_neon_shift_1" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_shift_1"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4.
(define_insn_reservation "cortex_a9_neon_shift_2" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_shift_2"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3 on cycle 2.
(define_insn_reservation "cortex_a9_neon_shift_3" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_shift_3"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N1.
(define_insn_reservation "cortex_a9_neon_vshl_ddd" 1
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vshl_ddd"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vqshl_vrshl_vqrshl_qqq" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vqshl_vrshl_vqrshl_qqq"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)d operands at N3, and produce a result at N6.
(define_insn_reservation "cortex_a9_neon_vsra_vrsra" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vsra_vrsra"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N5.
(define_insn_reservation "cortex_a9_neon_fp_vadd_ddd_vabs_dd" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vadd_ddd_vabs_dd"))
  "cortex_a9_neon_fadd")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N5 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vadd_qqq_vabs_qq" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vadd_qqq_vabs_qq"))
  "cortex_a9_neon_fadd_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N5.
(define_insn_reservation "cortex_a9_neon_fp_vsum" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vsum"))
  "cortex_a9_neon_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N5.
(define_insn_reservation "cortex_a9_neon_fp_vmul_ddd" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vmul_ddd"))
  "cortex_a9_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N5 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vmul_qqd" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vmul_qqd"))
  "cortex_a9_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N9.
(define_insn_reservation "cortex_a9_neon_fp_vmla_ddd" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vmla_ddd"))
  "cortex_a9_neon_fmul_then_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vmla_qqq" 10
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vmla_qqq"))
  "cortex_a9_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N9.
(define_insn_reservation "cortex_a9_neon_fp_vmla_ddd_scalar" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vmla_ddd_scalar"))
  "cortex_a9_neon_fmul_then_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vmla_qqq_scalar" 10
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vmla_qqq_scalar"))
  "cortex_a9_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N9.
(define_insn_reservation "cortex_a9_neon_fp_vrecps_vrsqrts_ddd" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vrecps_vrsqrts_ddd"))
  "cortex_a9_neon_fmul_then_fadd")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a9_neon_fp_vrecps_vrsqrts_qqq" 10
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_fp_vrecps_vrsqrts_qqq"))
  "cortex_a9_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2.
(define_insn_reservation "cortex_a9_neon_bp_simple" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_bp_simple"))
  "cortex_a9_neon_perm")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a9_neon_bp_2cycle" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_bp_2cycle"))
  "cortex_a9_neon_perm_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a9_neon_bp_3cycle" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_bp_3cycle"))
  "cortex_a9_neon_perm_3")

;; Instructions using this reservation produce a result at N1.
(define_insn_reservation "cortex_a9_neon_ldr" 1
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_ldr"))
  "cortex_a9_neon_ls")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_str" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_str"))
  "cortex_a9_neon_ls")

;; Instructions using this reservation produce a result at N1 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vld1_1_2_regs" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld1_1_2_regs"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation produce a result at N1 on cycle 3.
(define_insn_reservation "cortex_a9_neon_vld1_3_4_regs" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld1_3_4_regs"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vld2_2_regs_vld1_vld2_all_lanes" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld2_2_regs_vld1_vld2_all_lanes"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a9_neon_vld2_4_regs" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld2_4_regs"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 4.
(define_insn_reservation "cortex_a9_neon_vld3_vld4" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld3_vld4"))
  "cortex_a9_neon_ls_4")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst1_1_2_regs_vst2_2_regs" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst1_3_4_regs" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vst1_3_4_regs"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst2_4_regs_vst3_vst4" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vst2_4_regs_vst3_vst4"))
  "cortex_a9_neon_ls_4")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst3_vst4" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vst3_vst4"))
  "cortex_a9_neon_ls_4")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a9_neon_vld1_vld2_lane" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld1_vld2_lane"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 5.
(define_insn_reservation "cortex_a9_neon_vld3_vld4_lane" 6
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld3_vld4_lane"))
  "cortex_a9_neon_ls_5")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst1_vst2_lane" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vst1_vst2_lane"))
  "cortex_a9_neon_ls_2")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a9_neon_vst3_vst4_lane" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vst3_vst4_lane"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a9_neon_vld3_vld4_all_lanes" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_vld3_vld4_all_lanes"))
  "cortex_a9_neon_ls_3")

;; Instructions using this reservation produce a result at N2.
(define_insn_reservation "cortex_a9_neon_mcr" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mcr"))
  "cortex_a9_neon_perm")

;; Instructions using this reservation produce a result at N2.
(define_insn_reservation "cortex_a9_neon_mcr_2_mcrr" 2
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "neon_type" "neon_mcr_2_mcrr"))
  "cortex_a9_neon_perm_2")

;; Exceptions to the default latencies.

(define_bypass 1 "cortex_a9_neon_mcr_2_mcrr"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
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

(define_bypass 0 "cortex_a9_neon_vshl_ddd"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
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

(define_bypass 5 "cortex_a9_neon_vsma"
               "cortex_a9_neon_int_1,\
               cortex_a9_neon_int_4,\
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

