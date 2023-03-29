;; Cavium ThunderX 2 CN99xx pipeline description
;; Copyright (C) 2016-2023 Free Software Foundation, Inc.
;;
;; Contributed by Cavium, Broadcom and Mentor Embedded.

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

(define_automaton "thunderx2t99, thunderx2t99_advsimd, thunderx2t99_ldst")
(define_automaton "thunderx2t99_mult")

(define_cpu_unit "thunderx2t99_i0" "thunderx2t99")
(define_cpu_unit "thunderx2t99_i1" "thunderx2t99")
(define_cpu_unit "thunderx2t99_i2" "thunderx2t99")

(define_cpu_unit "thunderx2t99_ls0" "thunderx2t99_ldst")
(define_cpu_unit "thunderx2t99_ls1" "thunderx2t99_ldst")
(define_cpu_unit "thunderx2t99_sd" "thunderx2t99_ldst")

; Pseudo-units for multiply pipeline.

(define_cpu_unit "thunderx2t99_i1m1" "thunderx2t99_mult")
(define_cpu_unit "thunderx2t99_i1m2" "thunderx2t99_mult")
(define_cpu_unit "thunderx2t99_i1m3" "thunderx2t99_mult")

; Pseudo-units for load delay (assuming dcache hit).

(define_cpu_unit "thunderx2t99_ls0d1" "thunderx2t99_ldst")
(define_cpu_unit "thunderx2t99_ls0d2" "thunderx2t99_ldst")
(define_cpu_unit "thunderx2t99_ls0d3" "thunderx2t99_ldst")

(define_cpu_unit "thunderx2t99_ls1d1" "thunderx2t99_ldst")
(define_cpu_unit "thunderx2t99_ls1d2" "thunderx2t99_ldst")
(define_cpu_unit "thunderx2t99_ls1d3" "thunderx2t99_ldst")

; Make some aliases for f0/f1.
(define_cpu_unit "thunderx2t99_f0" "thunderx2t99_advsimd")
(define_cpu_unit "thunderx2t99_f1" "thunderx2t99_advsimd")

(define_reservation "thunderx2t99_i012" "thunderx2t99_i0|thunderx2t99_i1|thunderx2t99_i2")
(define_reservation "thunderx2t99_ls01" "thunderx2t99_ls0|thunderx2t99_ls1")
(define_reservation "thunderx2t99_f01" "thunderx2t99_f0|thunderx2t99_f1")

; A load with delay in the ls0/ls1 pipes.
(define_reservation "thunderx2t99_l0delay" "thunderx2t99_ls0,\
				      thunderx2t99_ls0d1,thunderx2t99_ls0d2,\
				      thunderx2t99_ls0d3")
(define_reservation "thunderx2t99_l1delay" "thunderx2t99_ls1,\
				      thunderx2t99_ls1d1,thunderx2t99_ls1d2,\
				      thunderx2t99_ls1d3")
(define_reservation "thunderx2t99_l01delay" "thunderx2t99_l0delay|thunderx2t99_l1delay")

;; Branch and call instructions.

(define_insn_reservation "thunderx2t99_branch" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "call,branch,trap"))
  "thunderx2t99_i2")

;; Misc instructions.

(define_insn_reservation "thunderx2t99_nothing" 0
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "block"))
  "nothing")

(define_insn_reservation "thunderx2t99_mrs" 0
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "mrs"))
  "thunderx2t99_i2")

(define_insn_reservation "thunderx2t99_multiple" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "multiple,untyped"))
  "thunderx2t99_i0+thunderx2t99_i1+thunderx2t99_i2+thunderx2t99_ls0+\
   thunderx2t99_ls1+thunderx2t99_sd+thunderx2t99_i1m1+thunderx2t99_i1m2+\
   thunderx2t99_i1m3+thunderx2t99_f0+thunderx2t99_f1")

;; Integer arithmetic/logic instructions.

; Plain register moves are handled by renaming, and don't create any uops.

(define_insn_reservation "thunderx2t99_regmove" 0
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "mov_reg"))
  "nothing")

(define_insn_reservation "thunderx2t99_alu_basic" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "alu_imm,alu_sreg,alus_imm,alus_sreg,\
			adc_reg,adc_imm,adcs_reg,adcs_imm,\
			logic_reg,logic_imm,logics_reg,logics_imm,\
			csel,adr,mov_imm,shift_reg,shift_imm,bfm,\
			bfx,rbit,rev,extend,rotate_imm"))
  "thunderx2t99_i012")

(define_insn_reservation "thunderx2t99_alu_shift" 2
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "alu_shift_imm_lsl_1to4,alu_shift_imm_other,alu_ext,\
			alus_shift_imm,alus_ext,\
			logic_shift_imm,logics_shift_imm"))
  "thunderx2t99_i012,thunderx2t99_i012")

(define_insn_reservation "thunderx2t99_div" 13
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "sdiv,udiv"))
  "thunderx2t99_i1*3")

(define_insn_reservation "thunderx2t99_madd" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "mla,smlal,umlal"))
  "thunderx2t99_i1,thunderx2t99_i1m1,thunderx2t99_i1m2,thunderx2t99_i1m3,\
   thunderx2t99_i012")

; NOTE: smull, umull are used for "high part" multiplies too.
(define_insn_reservation "thunderx2t99_mul" 4
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "mul,smull,umull"))
  "thunderx2t99_i1,thunderx2t99_i1m1,thunderx2t99_i1m2,thunderx2t99_i1m3")

(define_insn_reservation "thunderx2t99_countbits" 3
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "clz"))
  "thunderx2t99_i1")

;; Integer loads and stores.

(define_insn_reservation "thunderx2t99_load_basic" 4
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "load_4"))
  "thunderx2t99_ls01")

(define_insn_reservation "thunderx2t99_loadpair" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "load_8,load_16"))
  "thunderx2t99_i012,thunderx2t99_ls01")

(define_insn_reservation "thunderx2t99_store_basic" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "store_4"))
  "thunderx2t99_ls01,thunderx2t99_sd")

(define_insn_reservation "thunderx2t99_storepair_basic" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "store_8,store_16"))
  "thunderx2t99_ls01,thunderx2t99_sd")

;; FP data processing instructions.

(define_insn_reservation "thunderx2t99_fp_simple" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "ffariths,ffarithd,f_minmaxs,f_minmaxd"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_fp_addsub" 6
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "fadds,faddd"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_fp_cmp" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "fcmps,fcmpd,fccmps,fccmpd"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_fp_divsqrt_s" 16
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "fdivs,fsqrts"))
  "thunderx2t99_f0*3|thunderx2t99_f1*3")

(define_insn_reservation "thunderx2t99_fp_divsqrt_d" 23
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "fdivd,fsqrtd"))
  "thunderx2t99_f0*5|thunderx2t99_f1*5")

(define_insn_reservation "thunderx2t99_fp_mul_mac" 6
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "fmuls,fmuld,fmacs,fmacd"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_frint" 7
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "f_rints,f_rintd"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_fcsel" 4
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "fcsel"))
  "thunderx2t99_f01")

;; FP miscellaneous instructions.

(define_insn_reservation "thunderx2t99_fp_cvt" 7
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "f_cvtf2i,f_cvt,f_cvti2f"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_fp_mov" 4
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "fconsts,fconstd,fmov,f_mrc"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_fp_mov_to_gen" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "f_mcr"))
  "thunderx2t99_f01")

;; FP loads and stores.

(define_insn_reservation "thunderx2t99_fp_load_basic" 4
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "f_loads,f_loadd"))
  "thunderx2t99_ls01")

(define_insn_reservation "thunderx2t99_fp_store_basic" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "f_stores,f_stored"))
  "thunderx2t99_ls01,thunderx2t99_sd")

;; ASIMD integer instructions.

(define_insn_reservation "thunderx2t99_asimd_int" 7
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_abd,neon_abd_q,\
			neon_arith_acc,neon_arith_acc_q,\
			neon_abs,neon_abs_q,\
			neon_add,neon_add_q,\
			neon_sub,neon_sub_q,\
			neon_neg,neon_neg_q,\
			neon_add_long,neon_add_widen,\
			neon_add_halve,neon_add_halve_q,\
			neon_sub_long,neon_sub_widen,\
			neon_sub_halve,neon_sub_halve_q,\
			neon_add_halve_narrow_q,neon_sub_halve_narrow_q,\
			neon_qabs,neon_qabs_q,\
			neon_qadd,neon_qadd_q,\
			neon_qneg,neon_qneg_q,\
			neon_qsub,neon_qsub_q,\
			neon_minmax,neon_minmax_q,\
			neon_reduc_minmax,neon_reduc_minmax_q,\
			neon_mul_b,neon_mul_h,neon_mul_s,\
			neon_mul_b_q,neon_mul_h_q,neon_mul_s_q,\
			neon_sat_mul_b,neon_sat_mul_h,neon_sat_mul_s,\
			neon_sat_mul_b_q,neon_sat_mul_h_q,neon_sat_mul_s_q,\
			neon_mla_b,neon_mla_h,neon_mla_s,\
			neon_mla_b_q,neon_mla_h_q,neon_mla_s_q,\
			neon_mul_b_long,neon_mul_h_long,\
			neon_mul_s_long,neon_mul_d_long,\
			neon_sat_mul_b_long,neon_sat_mul_h_long,\
			neon_sat_mul_s_long,\
			neon_mla_b_long,neon_mla_h_long,neon_mla_s_long,\
			neon_sat_mla_b_long,neon_sat_mla_h_long,\
			neon_sat_mla_s_long,\
			neon_shift_acc,neon_shift_acc_q,\
			neon_shift_imm,neon_shift_imm_q,\
			neon_shift_reg,neon_shift_reg_q,\
			neon_shift_imm_long,neon_shift_imm_narrow_q,\
			neon_sat_shift_imm,neon_sat_shift_imm_q,\
			neon_sat_shift_reg,neon_sat_shift_reg_q,\
			neon_sat_shift_imm_narrow_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_reduc_add" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_reduc_add,neon_reduc_add_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_cmp" 7
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_compare,neon_compare_q,neon_compare_zero,\
			neon_tst,neon_tst_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_logic" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_logic,neon_logic_q"))
  "thunderx2t99_f01")

;; ASIMD floating-point instructions.

(define_insn_reservation "thunderx2t99_asimd_fp_simple" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_fp_abs_s,neon_fp_abs_d,\
			neon_fp_abs_s_q,neon_fp_abs_d_q,\
			neon_fp_compare_s,neon_fp_compare_d,\
			neon_fp_compare_s_q,neon_fp_compare_d_q,\
			neon_fp_minmax_s,neon_fp_minmax_d,\
			neon_fp_minmax_s_q,neon_fp_minmax_d_q,\
			neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_d,\
			neon_fp_reduc_minmax_s_q,neon_fp_reduc_minmax_d_q,\
			neon_fp_neg_s,neon_fp_neg_d,\
			neon_fp_neg_s_q,neon_fp_neg_d_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_fp_arith" 6
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_fp_abd_s,neon_fp_abd_d,\
			neon_fp_abd_s_q,neon_fp_abd_d_q,\
			neon_fp_addsub_s,neon_fp_addsub_d,\
			neon_fp_addsub_s_q,neon_fp_addsub_d_q,\
			neon_fp_reduc_add_s,neon_fp_reduc_add_d,\
			neon_fp_reduc_add_s_q,neon_fp_reduc_add_d_q,\
			neon_fp_mul_s,neon_fp_mul_d,\
			neon_fp_mul_s_q,neon_fp_mul_d_q,\
			neon_fp_mul_s_scalar_q,neon_fp_mul_d_scalar_q,\
			neon_fp_mla_s,neon_fp_mla_d,\
			neon_fp_mla_s_q,neon_fp_mla_d_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_fp_conv" 7
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_fp_cvt_widen_s,neon_fp_cvt_narrow_d_q,\
			neon_fp_to_int_s,neon_fp_to_int_d,\
			neon_fp_to_int_s_q,neon_fp_to_int_d_q,\
			neon_int_to_fp_s,neon_int_to_fp_d,\
			neon_int_to_fp_s_q,neon_int_to_fp_d_q,\
			neon_fp_round_s,neon_fp_round_d,\
			neon_fp_round_s_q,neon_fp_round_d_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_fp_div_s" 16
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_fp_div_s,neon_fp_div_s_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_fp_div_d" 23
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_fp_div_d,neon_fp_div_d_q"))
  "thunderx2t99_f01")

;; ASIMD miscellaneous instructions.

(define_insn_reservation "thunderx2t99_asimd_misc" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_rbit,\
			neon_bsl,neon_bsl_q,\
			neon_cls,neon_cls_q,\
			neon_cnt,neon_cnt_q,\
			neon_from_gp,neon_from_gp_q,\
			neon_dup,neon_dup_q,\
			neon_ext,neon_ext_q,\
			neon_ins,neon_ins_q,\
			neon_move,neon_move_q,\
			neon_fp_recpe_s,neon_fp_recpe_d,\
			neon_fp_recpe_s_q,neon_fp_recpe_d_q,\
			neon_fp_recpx_s,neon_fp_recpx_d,\
			neon_fp_recpx_s_q,neon_fp_recpx_d_q,\
			neon_rev,neon_rev_q,\
			neon_permute,neon_permute_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_recip_step" 6
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_fp_recps_s,neon_fp_recps_s_q,\
			neon_fp_recps_d,neon_fp_recps_d_q,\
			neon_fp_sqrt_s,neon_fp_sqrt_s_q,\
			neon_fp_sqrt_d,neon_fp_sqrt_d_q,\
			neon_fp_rsqrte_s, neon_fp_rsqrte_s_q,\
			neon_fp_rsqrte_d, neon_fp_rsqrte_d_q,\
			neon_fp_rsqrts_s, neon_fp_rsqrts_s_q,\
			neon_fp_rsqrts_d, neon_fp_rsqrts_d_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_lut" 8
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_tbl1,neon_tbl1_q,neon_tbl2,neon_tbl2_q,\
			neon_tbl3,neon_tbl3_q,neon_tbl4,neon_tbl4_q"))
  "thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_elt_to_gr" 6
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_to_gp,neon_to_gp_q"))
  "thunderx2t99_f01")

;; ASIMD load instructions.

; NOTE: These reservations attempt to model latency and throughput correctly,
; but the cycle timing of unit allocation is not necessarily accurate (because
; insns are split into uops, and those may be issued out-of-order).

(define_insn_reservation "thunderx2t99_asimd_load1_ldp" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_ldp,neon_ldp_q"))
  "thunderx2t99_i012,thunderx2t99_ls01")

(define_insn_reservation "thunderx2t99_asimd_load1" 4
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q,\
			neon_load1_2reg,neon_load1_2reg_q,\
			neon_load1_3reg,neon_load1_3reg_q,\
			neon_load1_4reg,neon_load1_4reg_q"))
  "thunderx2t99_ls01")

(define_insn_reservation "thunderx2t99_asimd_load1_onelane" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_load1_one_lane,neon_load1_one_lane_q"))
  "thunderx2t99_l01delay,thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_load1_all" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_load1_all_lanes,neon_load1_all_lanes_q"))
  "thunderx2t99_l01delay,thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_load2" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_load2_2reg,neon_load2_2reg_q,\
			neon_load2_one_lane,neon_load2_one_lane_q,\
			neon_load2_all_lanes,neon_load2_all_lanes_q"))
  "thunderx2t99_l01delay,thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_load3" 7
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_load3_3reg,neon_load3_3reg_q,\
			neon_load3_one_lane,neon_load3_one_lane_q,\
			neon_load3_all_lanes,neon_load3_all_lanes_q"))
  "thunderx2t99_l01delay,thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_load4" 8
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_load4_4reg,neon_load4_4reg_q,\
			neon_load4_one_lane,neon_load4_one_lane_q,\
			neon_load4_all_lanes,neon_load4_all_lanes_q"))
  "thunderx2t99_l01delay,thunderx2t99_f01")

;; ASIMD store instructions.

; Same note applies as for ASIMD load instructions.

(define_insn_reservation "thunderx2t99_asimd_store_stp" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_stp,neon_stp_q"))
  "thunderx2t99_ls01,thunderx2t99_sd")

(define_insn_reservation "thunderx2t99_asimd_store1" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_store1_1reg,neon_store1_1reg_q,\
			neon_store1_2reg,neon_store1_2reg_q,\
			neon_store1_3reg,neon_store1_4reg"))
  "thunderx2t99_ls01")

(define_insn_reservation "thunderx2t99_asimd_store1_onelane" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_store1_one_lane,neon_store1_one_lane_q"))
  "thunderx2t99_ls01,thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_store2" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_store2_2reg,neon_store2_2reg_q,\
			neon_store2_one_lane,neon_store2_one_lane_q"))
  "thunderx2t99_ls01,thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_store3" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_store3_3reg,neon_store3_3reg_q,\
			neon_store3_one_lane,neon_store3_one_lane_q"))
  "thunderx2t99_ls01,thunderx2t99_f01")

(define_insn_reservation "thunderx2t99_asimd_store4" 1
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "neon_store4_4reg,neon_store4_4reg_q,\
			neon_store4_one_lane,neon_store4_one_lane_q"))
  "thunderx2t99_ls01,thunderx2t99_f01")

;; Crypto extensions.

(define_insn_reservation "thunderx2t99_aes" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "crypto_aese,crypto_aesmc"))
  "thunderx2t99_f1")

(define_insn_reservation "thunderx2t99_sha" 7
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "crypto_sha1_fast,crypto_sha1_xor,crypto_sha1_slow,\
			crypto_sha256_fast,crypto_sha256_slow"))
  "thunderx2t99_f1")

;; CRC extension.

(define_insn_reservation "thunderx2t99_crc" 4
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "crc"))
  "thunderx2t99_i1")

;; PMULL extension.

(define_insn_reservation "thunderx2t99_pmull" 5
  (and (eq_attr "tune" "thunderx2t99")
       (eq_attr "type" "crypto_pmull"))
  "thunderx2t99_f1")
