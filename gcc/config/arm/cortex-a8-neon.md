;; ARM Cortex-A8 NEON scheduling description.
;; Copyright (C) 2007-2013 Free Software Foundation, Inc.
;; Contributed by CodeSourcery.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


(define_automaton "cortex_a8_neon")

;; Only one load, store, permute, MCR or MRC instruction can be issued
;; per cycle.
(define_cpu_unit "cortex_a8_neon_issue_perm" "cortex_a8_neon")

;; Only one data-processing instruction can be issued per cycle.
(define_cpu_unit "cortex_a8_neon_issue_dp" "cortex_a8_neon")

;; The VFPLite unit (non-pipelined).
(define_cpu_unit "cortex_a8_vfplite" "cortex_a8_neon")

;; We need a special mutual exclusion (to be used in addition to
;; cortex_a8_neon_issue_dp) for the case when an instruction such as
;; vmla.f is forwarded from E5 of the floating-point multiply pipeline to
;; E2 of the floating-point add pipeline.  On the cycle previous to that
;; forward we must prevent issue of any instruction to the floating-point
;; add pipeline, but still allow issue of a data-processing instruction
;; to any of the other pipelines.
(define_cpu_unit "cortex_a8_neon_issue_fadd" "cortex_a8_neon")

;; Patterns of reservation.
;; We model the NEON issue units as running in parallel with the core ones.
;; We assume that multi-cycle NEON instructions get decomposed into
;; micro-ops as they are issued into the NEON pipeline, and not as they
;; are issued into the ARM pipeline.  Dual issue may not occur except
;; upon the first and last cycles of a multi-cycle instruction, but it
;; is unclear whether two multi-cycle instructions can issue together (in
;; this model they cannot).  It is also unclear whether a pair of
;; a multi-cycle and single-cycle instructions, that could potentially
;; issue together, only do so if (say) the single-cycle one precedes
;; the other.

(define_reservation "cortex_a8_neon_dp"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp")
(define_reservation "cortex_a8_neon_dp_2"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp,\
                     cortex_a8_neon_issue_dp")
(define_reservation "cortex_a8_neon_dp_4"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp")

(define_reservation "cortex_a8_neon_fadd"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp+\
                     cortex_a8_neon_issue_fadd")
(define_reservation "cortex_a8_neon_fadd_2"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp+\
                     cortex_a8_neon_issue_fadd,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_fadd")

(define_reservation "cortex_a8_neon_perm"
                    "(cortex_a8_alu0|cortex_a8_alu1)+\
                     cortex_a8_neon_issue_perm")
(define_reservation "cortex_a8_neon_perm_2"
                    "(cortex_a8_alu0|cortex_a8_alu1)+\
                     cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_perm")
(define_reservation "cortex_a8_neon_perm_3"
                    "(cortex_a8_alu0|cortex_a8_alu1)+\
                     cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_perm")

(define_reservation "cortex_a8_neon_ls"
                    "cortex_a8_issue_ls+cortex_a8_neon_issue_perm")
(define_reservation "cortex_a8_neon_ls_2"
                    "cortex_a8_issue_ls+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_perm")
(define_reservation "cortex_a8_neon_ls_3"
                    "cortex_a8_issue_ls+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_perm")
(define_reservation "cortex_a8_neon_ls_4"
                    "cortex_a8_issue_ls+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_perm")
(define_reservation "cortex_a8_neon_ls_5"
                    "cortex_a8_issue_ls+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_dp+cortex_a8_neon_issue_perm,\
                     cortex_a8_neon_issue_perm")

(define_reservation "cortex_a8_neon_fmul_then_fadd"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp,\
		     nothing*3,\
		     cortex_a8_neon_issue_fadd")
(define_reservation "cortex_a8_neon_fmul_then_fadd_2"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp,\
		     cortex_a8_neon_issue_dp,\
		     nothing*2,\
		     cortex_a8_neon_issue_fadd,\
		     cortex_a8_neon_issue_fadd")

;; VFP instructions can only be single-issued into the NEON pipeline.
(define_reservation "cortex_a8_vfp"
                    "(cortex_a8_alu0|cortex_a8_alu1)+cortex_a8_neon_issue_dp+\
                     cortex_a8_neon_issue_perm+cortex_a8_vfplite")

;; VFP instructions.
;; The VFPLite unit that executes these isn't pipelined; we give the
;; worst-case latencies (and choose the double-precision ones where we
;; do not distinguish on precision).  We assume RunFast mode is not
;; enabled and therefore do not model the possible VFP instruction
;; execution in the NEON floating point pipelines, nor additional
;; latencies for the processing of subnormals.
;;
;; TODO: RunFast mode could potentially be enabled when -ffast-math
;; is specified.

(define_insn_reservation "cortex_a8_vfp_add_sub" 10
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fconsts,fconstd,fadds,faddd"))
  "cortex_a8_vfp,cortex_a8_vfplite*9")

(define_insn_reservation "cortex_a8_vfp_muls" 12
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fmuls"))
  "cortex_a8_vfp,cortex_a8_vfplite*11")

(define_insn_reservation "cortex_a8_vfp_muld" 17
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fmuld"))
  "cortex_a8_vfp,cortex_a8_vfplite*16")

(define_insn_reservation "cortex_a8_vfp_macs" 21
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fmacs,ffmas"))
  "cortex_a8_vfp,cortex_a8_vfplite*20")

(define_insn_reservation "cortex_a8_vfp_macd" 26
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fmacd,ffmad"))
  "cortex_a8_vfp,cortex_a8_vfplite*25")

(define_insn_reservation "cortex_a8_vfp_divs" 37
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fdivs"))
  "cortex_a8_vfp,cortex_a8_vfplite*36")

(define_insn_reservation "cortex_a8_vfp_divd" 65
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fdivd"))
  "cortex_a8_vfp,cortex_a8_vfplite*64")

;; Comparisons can actually take 7 cycles sometimes instead of four,
;; but given all the other instructions lumped into type=ffarith that
;; take four cycles, we pick that latency.
(define_insn_reservation "cortex_a8_vfp_farith" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "fcpys,ffariths,ffarithd,fconsts,fconstd,fcmps,fcmpd"))
  "cortex_a8_vfp,cortex_a8_vfplite*3")

(define_insn_reservation "cortex_a8_vfp_cvt" 7
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "f_cvt"))
  "cortex_a8_vfp,cortex_a8_vfplite*6")

;; NEON -> core transfers.

(define_insn_reservation "cortex_a8_neon_mrc" 20
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mrc"))
  "cortex_a8_neon_ls")

(define_insn_reservation "cortex_a8_neon_mrrc" 21
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mrrc"))
  "cortex_a8_neon_ls_2")

;; The remainder of this file is auto-generated by neon-schedgen.

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N3.
(define_insn_reservation "cortex_a8_neon_int_1" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_int_1"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)n operands at N2, and produce a result at N3.
(define_insn_reservation "cortex_a8_neon_int_2" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_int_2"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3.
(define_insn_reservation "cortex_a8_neon_int_3" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_int_3"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N4.
(define_insn_reservation "cortex_a8_neon_int_4" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_int_4"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)n operands at N2, and produce a result at N4.
(define_insn_reservation "cortex_a8_neon_int_5" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_int_5"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4.
(define_insn_reservation "cortex_a8_neon_vqneg_vqabs" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vqneg_vqabs"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation produce a result at N3.
(define_insn_reservation "cortex_a8_neon_vmov" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vmov"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a8_neon_vaba" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vaba"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a8_neon_vaba_qqq" 7
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vaba_qqq"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)d operands at N3, and produce a result at N6.
(define_insn_reservation "cortex_a8_neon_vsma" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vsma"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N6.
(define_insn_reservation "cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a8_neon_mul_qqq_8_16_32_ddd_32" 7
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mul_qqq_8_16_32_ddd_32"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a8_neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar" 7
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a8_neon_mla_qqq_8_16" 7
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mla_qqq_8_16"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 2.
(define_insn_reservation "cortex_a8_neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long" 7
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6 on cycle 4.
(define_insn_reservation "cortex_a8_neon_mla_qqq_32_qqd_32_scalar" 9
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mla_qqq_32_qqd_32_scalar"))
  "cortex_a8_neon_dp_4")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6.
(define_insn_reservation "cortex_a8_neon_mul_ddd_16_scalar_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mul_ddd_16_scalar_32_16_long_scalar"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N6 on cycle 4.
(define_insn_reservation "cortex_a8_neon_mul_qqd_32_scalar" 9
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mul_qqd_32_scalar"))
  "cortex_a8_neon_dp_4")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N6.
(define_insn_reservation "cortex_a8_neon_mla_ddd_16_scalar_qdd_32_16_long_scalar" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3.
(define_insn_reservation "cortex_a8_neon_shift_1" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_shift_1"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4.
(define_insn_reservation "cortex_a8_neon_shift_2" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_shift_2"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N3 on cycle 2.
(define_insn_reservation "cortex_a8_neon_shift_3" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_shift_3"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N1.
(define_insn_reservation "cortex_a8_neon_vshl_ddd" 1
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vshl_ddd"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N4 on cycle 2.
(define_insn_reservation "cortex_a8_neon_vqshl_vrshl_vqrshl_qqq" 5
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vqshl_vrshl_vqrshl_qqq"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their (D|Q)m operands at N1,
;; their (D|Q)d operands at N3, and produce a result at N6.
(define_insn_reservation "cortex_a8_neon_vsra_vrsra" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vsra_vrsra"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N5.
(define_insn_reservation "cortex_a8_neon_fp_vadd_ddd_vabs_dd" 5
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vadd_ddd_vabs_dd"))
  "cortex_a8_neon_fadd")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N5 on cycle 2.
(define_insn_reservation "cortex_a8_neon_fp_vadd_qqq_vabs_qq" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vadd_qqq_vabs_qq"))
  "cortex_a8_neon_fadd_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N5.
(define_insn_reservation "cortex_a8_neon_fp_vsum" 5
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vsum"))
  "cortex_a8_neon_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N5.
(define_insn_reservation "cortex_a8_neon_fp_vmul_ddd" 5
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vmul_ddd"))
  "cortex_a8_neon_dp")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, and produce a result at N5 on cycle 2.
(define_insn_reservation "cortex_a8_neon_fp_vmul_qqd" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vmul_qqd"))
  "cortex_a8_neon_dp_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N9.
(define_insn_reservation "cortex_a8_neon_fp_vmla_ddd" 9
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vmla_ddd"))
  "cortex_a8_neon_fmul_then_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N2, their (D|Q)d operands at N3, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a8_neon_fp_vmla_qqq" 10
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vmla_qqq"))
  "cortex_a8_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N9.
(define_insn_reservation "cortex_a8_neon_fp_vmla_ddd_scalar" 9
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vmla_ddd_scalar"))
  "cortex_a8_neon_fmul_then_fadd")

;; Instructions using this reservation read their (D|Q)n operands at N2,
;; their (D|Q)m operands at N1, their (D|Q)d operands at N3, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a8_neon_fp_vmla_qqq_scalar" 10
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vmla_qqq_scalar"))
  "cortex_a8_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N9.
(define_insn_reservation "cortex_a8_neon_fp_vrecps_vrsqrts_ddd" 9
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vrecps_vrsqrts_ddd"))
  "cortex_a8_neon_fmul_then_fadd")

;; Instructions using this reservation read their source operands at N2, and
;; produce a result at N9 on cycle 2.
(define_insn_reservation "cortex_a8_neon_fp_vrecps_vrsqrts_qqq" 10
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_fp_vrecps_vrsqrts_qqq"))
  "cortex_a8_neon_fmul_then_fadd_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2.
(define_insn_reservation "cortex_a8_neon_bp_simple" 2
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_bp_simple"))
  "cortex_a8_neon_perm")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a8_neon_bp_2cycle" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_bp_2cycle"))
  "cortex_a8_neon_perm_2")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a8_neon_bp_3cycle" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_bp_3cycle"))
  "cortex_a8_neon_perm_3")

;; Instructions using this reservation produce a result at N1.
(define_insn_reservation "cortex_a8_neon_ldr" 1
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_ldr"))
  "cortex_a8_neon_ls")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a8_neon_str" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_str"))
  "cortex_a8_neon_ls")

;; Instructions using this reservation produce a result at N1 on cycle 2.
(define_insn_reservation "cortex_a8_neon_vld1_1_2_regs" 2
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld1_1_2_regs"))
  "cortex_a8_neon_ls_2")

;; Instructions using this reservation produce a result at N1 on cycle 3.
(define_insn_reservation "cortex_a8_neon_vld1_3_4_regs" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld1_3_4_regs"))
  "cortex_a8_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a8_neon_vld2_2_regs_vld1_vld2_all_lanes" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld2_2_regs_vld1_vld2_all_lanes"))
  "cortex_a8_neon_ls_2")

;; Instructions using this reservation produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a8_neon_vld2_4_regs" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld2_4_regs"))
  "cortex_a8_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 4.
(define_insn_reservation "cortex_a8_neon_vld3_vld4" 5
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld3_vld4"))
  "cortex_a8_neon_ls_4")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a8_neon_vst1_1_2_regs_vst2_2_regs" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs"))
  "cortex_a8_neon_ls_2")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a8_neon_vst1_3_4_regs" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vst1_3_4_regs"))
  "cortex_a8_neon_ls_3")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a8_neon_vst2_4_regs_vst3_vst4" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vst2_4_regs_vst3_vst4"))
  "cortex_a8_neon_ls_4")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a8_neon_vst3_vst4" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vst3_vst4"))
  "cortex_a8_neon_ls_4")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 3.
(define_insn_reservation "cortex_a8_neon_vld1_vld2_lane" 4
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld1_vld2_lane"))
  "cortex_a8_neon_ls_3")

;; Instructions using this reservation read their source operands at N1, and
;; produce a result at N2 on cycle 5.
(define_insn_reservation "cortex_a8_neon_vld3_vld4_lane" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld3_vld4_lane"))
  "cortex_a8_neon_ls_5")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a8_neon_vst1_vst2_lane" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vst1_vst2_lane"))
  "cortex_a8_neon_ls_2")

;; Instructions using this reservation read their source operands at N1.
(define_insn_reservation "cortex_a8_neon_vst3_vst4_lane" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vst3_vst4_lane"))
  "cortex_a8_neon_ls_3")

;; Instructions using this reservation produce a result at N2 on cycle 2.
(define_insn_reservation "cortex_a8_neon_vld3_vld4_all_lanes" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_vld3_vld4_all_lanes"))
  "cortex_a8_neon_ls_3")

;; Instructions using this reservation produce a result at N2.
(define_insn_reservation "cortex_a8_neon_mcr" 2
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mcr"))
  "cortex_a8_neon_perm")

;; Instructions using this reservation produce a result at N2.
(define_insn_reservation "cortex_a8_neon_mcr_2_mcrr" 2
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "neon_type" "neon_mcr_2_mcrr"))
  "cortex_a8_neon_perm_2")

;; Exceptions to the default latencies.

(define_bypass 1 "cortex_a8_neon_mcr_2_mcrr"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 1 "cortex_a8_neon_mcr"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_vld3_vld4_all_lanes"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_vld3_vld4_lane"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_vld1_vld2_lane"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a8_neon_vld3_vld4"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_vld2_4_regs"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_vld2_2_regs_vld1_vld2_all_lanes"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_vld1_3_4_regs"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 1 "cortex_a8_neon_vld1_1_2_regs"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 0 "cortex_a8_neon_ldr"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_bp_3cycle"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_bp_2cycle"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 1 "cortex_a8_neon_bp_simple"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 9 "cortex_a8_neon_fp_vrecps_vrsqrts_qqq"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a8_neon_fp_vrecps_vrsqrts_ddd"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 9 "cortex_a8_neon_fp_vmla_qqq_scalar"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a8_neon_fp_vmla_ddd_scalar"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 9 "cortex_a8_neon_fp_vmla_qqq"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a8_neon_fp_vmla_ddd"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_fp_vmul_qqd"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a8_neon_fp_vmul_ddd"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a8_neon_fp_vsum"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_fp_vadd_qqq_vabs_qq"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a8_neon_fp_vadd_ddd_vabs_dd"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_vsra_vrsra"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 4 "cortex_a8_neon_vqshl_vrshl_vqrshl_qqq"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 0 "cortex_a8_neon_vshl_ddd"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_shift_3"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_shift_2"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_shift_1"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_mla_ddd_16_scalar_qdd_32_16_long_scalar"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a8_neon_mul_qqd_32_scalar"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_mul_ddd_16_scalar_32_16_long_scalar"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 8 "cortex_a8_neon_mla_qqq_32_qqd_32_scalar"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a8_neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a8_neon_mla_qqq_8_16"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a8_neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a8_neon_mul_qqq_8_16_32_ddd_32"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_vsma"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 6 "cortex_a8_neon_vaba_qqq"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 5 "cortex_a8_neon_vaba"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_vmov"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_vqneg_vqabs"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_int_5"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 3 "cortex_a8_neon_int_4"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_int_3"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_int_2"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

(define_bypass 2 "cortex_a8_neon_int_1"
               "cortex_a8_neon_int_1,\
               cortex_a8_neon_int_4,\
               cortex_a8_neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mul_qqq_8_16_32_ddd_32,\
               cortex_a8_neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
               cortex_a8_neon_mla_qqq_8_16,\
               cortex_a8_neon_fp_vadd_ddd_vabs_dd,\
               cortex_a8_neon_fp_vadd_qqq_vabs_qq,\
               cortex_a8_neon_fp_vmla_ddd,\
               cortex_a8_neon_fp_vmla_qqq,\
               cortex_a8_neon_fp_vrecps_vrsqrts_ddd,\
               cortex_a8_neon_fp_vrecps_vrsqrts_qqq")

