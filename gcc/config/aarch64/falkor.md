;; Falkor pipeline description
;; Copyright (C) 2017 Free Software Foundation, Inc.
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

(define_automaton "falkor")

;; Complex int instructions (e.g. multiply and divide) execute in the X
;; pipeline.  Simple int instructions execute in the X, Y, and Z pipelines.

(define_cpu_unit "falkor_x" "falkor")
(define_cpu_unit "falkor_y" "falkor")
(define_cpu_unit "falkor_z" "falkor")

;; Branches execute in the B pipeline or in one of the int pipelines depending
;; on how complex it is.  Simple int insns (like movz) can also execute here.

(define_cpu_unit "falkor_b" "falkor")

;; Vector and FP insns execute in the VX and VY pipelines.

(define_automaton "falkor_vfp")

(define_cpu_unit "falkor_vx" "falkor_vfp")
(define_cpu_unit "falkor_vy" "falkor_vfp")

;; Loads execute in the LD pipeline.
;; Stores execute in the ST, SD, and VSD pipelines, for address, data, and
;; vector data.

(define_automaton "falkor_mem")

(define_cpu_unit "falkor_ld" "falkor_mem")
(define_cpu_unit "falkor_st" "falkor_mem")
(define_cpu_unit "falkor_sd" "falkor_mem")
(define_cpu_unit "falkor_vsd" "falkor_mem")

;; The GTOV and VTOG pipelines are for general to vector reg moves, and vice
;; versa.

(define_cpu_unit "falkor_gtov" "falkor")
(define_cpu_unit "falkor_vtog" "falkor")

;; Common reservation combinations.

(define_reservation "falkor_vxvy" "falkor_vx|falkor_vy")
(define_reservation "falkor_zb"   "falkor_z|falkor_b")
(define_reservation "falkor_xyz"  "falkor_x|falkor_y|falkor_z")
(define_reservation "falkor_xyzb" "falkor_x|falkor_y|falkor_z|falkor_b")

;; SIMD Floating-Point Instructions

(define_insn_reservation "falkor_afp_1_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_neg_s,neon_fp_neg_d,neon_fp_abs_s,neon_fp_abs_d"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_1_vxvy_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_neg_s_q,neon_fp_neg_d_q,neon_fp_abs_s_q,neon_fp_abs_d_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_afp_2_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_minmax_s,neon_fp_minmax_d,neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_d,neon_fp_compare_s,neon_fp_compare_d,neon_fp_round_s,neon_fp_round_d"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_2_vxvy_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_minmax_s_q,neon_fp_minmax_d_q,neon_fp_compare_s_q,neon_fp_compare_d_q,neon_fp_round_s_q,neon_fp_round_d_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_afp_3_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_reduc_minmax_s_q,neon_fp_reduc_minmax_d_q,neon_fp_abd_s,neon_fp_abd_d,neon_fp_addsub_s,neon_fp_addsub_d,neon_fp_reduc_add_s,neon_fp_reduc_add_d"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_3_vxvy_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_abd_s_q,neon_fp_abd_d_q,neon_fp_addsub_s_q,neon_fp_addsub_d_q,neon_fp_reduc_add_s_q,neon_fp_reduc_add_d_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_afp_4_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_to_int_s,neon_fp_to_int_d,neon_int_to_fp_s,neon_int_to_fp_d,neon_fp_cvt_widen_h,neon_fp_cvt_widen_s"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_4_vxvy_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_to_int_s_q,neon_fp_to_int_d_q,neon_int_to_fp_s_q,neon_int_to_fp_d_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_afp_5_vxvy_mul" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mul_s,neon_fp_mul_s_scalar"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_5_vxvy_mla" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mla_s,neon_fp_mla_s_scalar"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_5_vxvy_vxvy_mul" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mul_s_q,neon_fp_mul_s_scalar_q"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_5_vxvy_vxvy_mla" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mla_s_q,neon_fp_mla_s_scalar_q"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_6_vxvy_mul" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mul_d"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_6_vxvy_mla" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mla_d"))
  "falkor_vxvy")

(define_insn_reservation "falkor_afp_6_vxvy_vxvy_mul" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mul_d_q,neon_fp_mul_d_scalar_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_afp_6_vxvy_vxvy_mla" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_mla_d_q,neon_fp_mla_d_scalar_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_afp_4_vxvy_vxvy_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_cvt_narrow_s_q,neon_fp_cvt_narrow_d_q"))
  "falkor_vxvy+falkor_vxvy,falkor_vxvy")

(define_insn_reservation "falkor_afp_6_vx_vy" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_div_s"))
  "falkor_vx+falkor_vy")

(define_insn_reservation "falkor_afp_11_vx_vy" 11
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_div_d"))
  "falkor_vx+falkor_vy")

(define_insn_reservation "falkor_afp_6_vx_vy_vx_vy" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_div_s_q"))
  "(falkor_vx+falkor_vy),(falkor_vx+falkor_vy)")

(define_insn_reservation "falkor_afp_11_vx_vy_vx_vy" 11
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_div_d_q"))
  "(falkor_vx+falkor_vy),(falkor_vx+falkor_vy)")

(define_insn_reservation "falkor_afp_12_vx_vy" 12
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_sqrt_s"))
  "falkor_vx+falkor_vy")

(define_insn_reservation "falkor_afp_22_vx_vy" 22
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_sqrt_d"))
  "falkor_vx+falkor_vy")

(define_insn_reservation "falkor_afp_12_vx_vy_vx_vy" 12
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_sqrt_s_q"))
  "(falkor_vx+falkor_vy),(falkor_vx+falkor_vy)")

(define_insn_reservation "falkor_afp_22_vx_vy_vx_vy" 22
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_sqrt_d_q"))
  "(falkor_vx+falkor_vy),(falkor_vx+falkor_vy)")

;; SIMD Integer Instructions

(define_insn_reservation "falkor_ai_1_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_add,neon_reduc_add,neon_logic,neon_neg,neon_sub"))
  "falkor_vxvy")

(define_insn_reservation "falkor_ai_1_vxvy_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_shift_imm_long,neon_add_q,neon_reduc_add_q,neon_logic_q,neon_neg_q,neon_sub_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_ai_2_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_add_long,neon_sub_long,neon_add_halve,neon_sub_halve,neon_shift_imm,neon_shift_reg,neon_minmax,neon_abs,neon_compare,neon_compare_zero,neon_tst"))
  "falkor_vxvy")

(define_insn_reservation "falkor_ai_2_vxvy_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_add_halve_q,neon_sub_halve_q,neon_shift_imm_q,neon_shift_reg_q,neon_minmax_q,neon_abs_q,neon_compare_q,neon_compare_zero_q,neon_tst_q,neon_reduc_add_long"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_ai_3_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_shift_acc,neon_reduc_add_acc,neon_abd,neon_qadd,neon_qsub,neon_qabs,neon_qneg,neon_sat_shift_imm,neon_sat_shift_imm_narrow_q,neon_sat_shift_reg,neon_reduc_minmax"))
  "falkor_vxvy")

(define_insn_reservation "falkor_ai_4_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_reduc_minmax_q"))
  "falkor_vxvy")

(define_insn_reservation "falkor_ai_3_vxvy_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_shift_acc_q,neon_reduc_add_acc_q,neon_abd_q,neon_abd_long,neon_qadd_q,neon_qsub_q,neon_qabs_q,neon_qneg_q,neon_sat_shift_imm_q,neon_sat_shift_reg_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_ai_4_vxvy_mul" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_mul_b,neon_mul_h,neon_mul_s,neon_mul_h_scalar,neon_mul_s_scalar,neon_sat_mul_b,neon_sat_mul_h,neon_sat_mul_s,neon_sat_mul_h_scalar,neon_sat_mul_s_scalar"))
  "falkor_vxvy")

(define_insn_reservation "falkor_ai_4_vxvy_mla" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_mla_b,neon_mla_h,neon_mla_s,neon_mla_h_scalar,neon_mla_s_scalar"))
  "falkor_vxvy")

(define_insn_reservation "falkor_ai_4_vxvy_vxvy_mul" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_mul_b_q,neon_mul_h_q,neon_mul_s_q,neon_mul_h_scalar_q,neon_mul_s_scalar_q,neon_sat_mul_b_q,neon_sat_mul_h_q,neon_sat_mul_s_q,neon_mul_b_long,neon_mul_h_long,neon_mul_s_long,neon_mul_d_long,neon_mul_h_scalar_long,neon_mul_s_scalar_long,neon_sat_mul_b_long,neon_sat_mul_h_long,neon_sat_mul_s_long,neon_sat_mul_h_scalar_q,neon_sat_mul_s_scalar_q,neon_sat_mul_h_scalar_long,neon_sat_mul_s_scalar_long"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_ai_4_vxvy_vxvy_mla" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_mla_b_q,neon_mla_h_q,neon_mla_s_q,neon_mla_h_scalar_q,neon_mla_s_scalar_q,neon_mla_b_long,neon_mla_h_long,neon_mla_s_long,neon_mla_h_scalar_long,neon_mla_s_scalar_long,neon_sat_mla_b_long,neon_sat_mla_h_long,neon_sat_mla_s_long,neon_sat_mla_h_scalar_long,neon_sat_mla_s_scalar_long"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_ai_4_vxvy_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_add_halve_narrow_q,neon_sub_halve_narrow_q,neon_arith_acc"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_2_ai_vxvy_vxvy_vxvy_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_add_widen,neon_sub_widen"))
  "(falkor_vxvy+falkor_vxvy),(falkor_vxvy+falkor_vxvy)")

(define_insn_reservation "falkor_4_ai_vxvy_vxvy_vxvy_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_arith_acc_q"))
  "(falkor_vxvy+falkor_vxvy),(falkor_vxvy+falkor_vxvy)")

;; SIMD Load Instructions

(define_insn_reservation "falkor_ald_4_ld" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q,neon_load1_all_lanes,neon_load2_one_lane"))
  "falkor_ld")

(define_insn_reservation "falkor_ald_4_ld_none" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_load1_2reg,neon_load2_2reg,neon_load2_all_lanes"))
  "falkor_ld")

(define_insn_reservation "falkor_ald_4_ld_ld" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_load1_2reg_q,neon_load2_2reg_q,neon_load2_all_lanes_q,neon_load3_one_lane,neon_load4_one_lane,neon_ldp,neon_ldp_q"))
  "falkor_ld,falkor_ld")

(define_insn_reservation "falkor_ald_4_ld_ld_none" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_load1_3reg,neon_load3_3reg,neon_load3_all_lanes"))
  "falkor_ld,falkor_ld")

(define_insn_reservation "falkor_ald_4_ld_ld_ld" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_load1_3reg_q,neon_load3_3reg_q,neon_load3_all_lanes_q"))
  "falkor_ld,falkor_ld,falkor_ld")

(define_insn_reservation "falkor_ald_4_ld_ld_none_none" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_load1_4reg,neon_load4_4reg"))
  "falkor_ld,falkor_ld")

(define_insn_reservation "falkor_ald_4_ld_ld_ld_ld" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_load1_4reg_q,neon_load4_4reg_q,neon_load4_all_lanes,neon_load4_all_lanes_q"))
  "falkor_ld,falkor_ld,falkor_ld,falkor_ld")

;; Arithmetic and Logical Instructions

(define_insn_reservation "falkor_alu_1_xyz" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "alus_sreg,alus_imm,alus_shift_imm,csel,adc_reg,alu_imm,alu_sreg,alu_shift_imm,alu_ext,alus_ext,logic_imm,logic_reg,logic_shift_imm,logics_imm,logics_reg,logics_shift_imm,mov_reg"))
  "falkor_xyz")

;; SIMD Miscellaneous Instructions

;; No separate type for ins and dup.  But this is correct for both.

(define_insn_reservation "falkor_am_3_gtov" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_from_gp"))
  "falkor_gtov")

;; No separate type for ins and dup.  Assuming dup is more common.  Ins is
;; gtov+vxvy and latency of 4.

(define_insn_reservation "falkor_am_3_gtov_gtov" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_from_gp_q"))
  "falkor_gtov,falkor_gtov")

;; neon_to_gp_q is used for 32-bit ARM instructions that move 64-bits of data
;; so no use needed here.

(define_insn_reservation "falkor_am_3_vtog" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_to_gp"))
  "falkor_vtog")

(define_insn_reservation "falkor_am_1_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_bsl,neon_dup,neon_ext,neon_ins,neon_ins_q,neon_move,neon_rev,neon_tbl1,neon_permute,neon_shift_imm_narrow_q"))
  "falkor_vxvy")

(define_insn_reservation "falkor_am_1_vxvy_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_bsl_q,neon_dup_q,neon_ext_q,neon_move_q,neon_rev_q,neon_tbl1_q,neon_permute_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_am_2_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_cls,neon_cnt,neon_rbit"))
  "falkor_vxvy")

(define_insn_reservation "falkor_am_4_vxvy_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_cls_q,neon_cnt_q,neon_rbit_q,neon_tbl2"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_am_3_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_recpe_s,neon_fp_recpe_d,neon_fp_rsqrte_s,neon_fp_rsqrte_d,neon_fp_recpx_s,neon_fp_recpx_d"))
  "falkor_vxvy")

(define_insn_reservation "falkor_am_3_vxvy_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_recpe_s_q,neon_fp_recpe_d_q,neon_fp_rsqrte_s_q,neon_fp_rsqrte_d_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_am_5_vxvy" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_recps_s"))
  "falkor_vxvy")

(define_insn_reservation "falkor_am_5_vxvy_vxvy" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_recps_s_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_am_6_vxvy" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_recps_d,neon_fp_rsqrts_d"))
  "falkor_vxvy")

(define_insn_reservation "falkor_am_6_vxvy_vxvy" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_fp_recps_d_q,neon_fp_rsqrts_d_q"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_am_5_vxvy_vxvy_vxvy" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_tbl2_q,neon_tbl3"))
  "(falkor_vxvy+falkor_vxvy),falkor_vxvy")

(define_insn_reservation "falkor_am_6_vxvy_vxvy_vxvy_vxvy" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_tbl3_q,neon_tbl4"))
  "(falkor_vxvy+falkor_vxvy),(falkor_vxvy+falkor_vxvy)")

(define_insn_reservation "falkor_am_7_vxvy_vxvy_vxvy_vxvy_vxvy" 7
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_tbl4_q"))
  "(falkor_vxvy+falkor_vxvy),(falkor_vxvy+falkor_vxvy),falkor_vxvy")

;; SIMD Store Instructions

;; ??? stp is neon_store1_2reg in aarch64.md, but neon_stp in aarch64-simd.md.
;; Similarly with ldp.

(define_insn_reservation "falkor_ast_st_vsd" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_store1_1reg,neon_store1_1reg_q,neon_store1_one_lane,neon_store1_one_lane_q,neon_store1_2reg,neon_store2_2reg,neon_store2_one_lane,neon_store2_one_lane_q,neon_stp"))
  "falkor_st+falkor_vsd")

(define_insn_reservation "falkor_as_0_st_vsd_st_vsd" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_store1_2reg_q,neon_store1_3reg,neon_store1_4reg,neon_store2_2reg_q,neon_store3_3reg,neon_store4_4reg,neon_store3_one_lane,neon_store3_one_lane_q,neon_store4_one_lane,neon_store4_one_lane_q,neon_stp_q"))
  "(falkor_st+falkor_vsd),(falkor_st+falkor_vsd)")

(define_insn_reservation "falkor_as_0_st_vsd_st_vsd_st_vsd" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_store1_3reg_q,neon_store3_3reg_q"))
  "(falkor_st+falkor_vsd),(falkor_st+falkor_vsd),(falkor_st+falkor_vsd)")

(define_insn_reservation "falkor_as_0_st_vsd_st_vsd_st_vsd_st_vsd" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "neon_store1_4reg_q,neon_store4_4reg_q"))
  "(falkor_st+falkor_vsd),(falkor_st+falkor_vsd),(falkor_st+falkor_vsd),(falkor_st+falkor_vsd)")

;; Branch Instructions

(define_insn_reservation "falkor_branch_0_zb" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "branch"))
  "falkor_zb")

(define_insn_reservation "falkor_call_0_xyzb" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "call"))
  "falkor_xyzb")

;; Cryptography Extensions

(define_insn_reservation "falkor_cry_1_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "crypto_sha1_fast"))
  "falkor_vxvy")

(define_insn_reservation "falkor_cry_2_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "crypto_aesmc"))
  "falkor_vxvy")

(define_insn_reservation "falkor_cry_2_vxvy_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "crypto_sha1_xor,crypto_sha256_fast,crypto_pmull"))
  "falkor_vxvy+falkor_vxvy")

(define_insn_reservation "falkor_cry_4_vy_vx" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "crypto_sha1_slow"))
  "falkor_vy+falkor_vx")

(define_insn_reservation "falkor_cry_6_vy_vx" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "crypto_sha256_slow"))
  "falkor_vy+falkor_vx")

(define_insn_reservation "falkor_cry_3_vxvy_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "crypto_aese"))
  "falkor_vxvy+falkor_vxvy")

;; FP Load Instructions

(define_insn_reservation "falkor_fld_4_ld" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_loads,f_loadd"))
  "falkor_ld")

;; No separate FP store section, these are found in the SIMD store section.

(define_insn_reservation "falkor_fld_0_st_vsd" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_stores,f_stored"))
  "falkor_st+falkor_vsd")

;; FP Data Processing Instructions

(define_insn_reservation "falkor_fpdt_0_vxvy" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fcmps,fcmpd,fccmps,fccmpd"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_5_vtog" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_cvtf2i"))
  "falkor_vtog")

(define_insn_reservation "falkor_fpdt_1_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "ffariths,ffarithd,fcsel"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_2_vxvy" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_minmaxd,f_minmaxs,f_rintd,f_rints"))
  "falkor_vxvy")

;; Scalar FP ABD is handled same as vector FP ABD.

(define_insn_reservation "falkor_fpdt_3_vxvy" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "faddd,fadds"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_4_vxvy" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_cvt"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_5_vxvy_mul" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fmuls"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_5_vxvy_mla" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fmacs,ffmas"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_6_vxvy_mul" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fmuld"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_6_vxvy_mla" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fmacd,ffmad"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_6_vx_vy" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fdivs"))
  "falkor_vx+falkor_vy")

(define_insn_reservation "falkor_fpdt_11_vx_vy" 11
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fdivd"))
  "falkor_vx+falkor_vy")

(define_insn_reservation "falkor_fpdt_12_vx_vy" 12
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fsqrts"))
  "falkor_vxvy")

(define_insn_reservation "falkor_fpdt_22_vx_vy" 22
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fsqrtd"))
  "falkor_vxvy")

;; FP Miscellaneous Instructions

(define_insn_reservation "falkor_fpmsc_3_vtog" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_mrc"))
  "falkor_vtog")

(define_insn_reservation "falkor_fpmsc_3_gtov" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_mcr"))
  "falkor_gtov")

(define_insn_reservation "falkor_fpmsc_1_vxvy" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "fmov,fconsts,fconstd"))
  "falkor_vxvy")

;; No separate type for float-to-fixed conversions.  Same type as
;; float-to-int conversions.  They schedule the same though, so no problem.

(define_insn_reservation "falkor_fpmsc_6_gtov" 6
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "f_cvti2f"))
  "falkor_gtov")

;; Load Instructions

(define_insn_reservation "falkor_ld_3_ld" 3
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "load1,load2"))
  "falkor_ld")

;; Miscellaneous Data-Processing Instructions

(define_insn_reservation "falkor_misc_1_xyz" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "bfx,bfm,extend,rotate_imm,shift_imm"))
  "falkor_xyz")

(define_insn_reservation "falkor_misc_2_x" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "crc"))
  "falkor_x")

(define_insn_reservation "falkor_misc_2_xyz" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "clz,rbit,rev"))
  "falkor_xyz")

;; Divide and Multiply Instructions

(define_insn_reservation "falkor_muldiv_4_x_mul" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "mul"))
  "falkor_x")

(define_insn_reservation "falkor_muldiv_4_x_mla" 4
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "mla,smlal,umlal"))
  "falkor_x")

(define_insn_reservation "falkor_muldiv_5_x_mul" 5
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "smull,umull"))
  "falkor_x")

(define_insn_reservation "falkor_md_11_x_z" 11
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "sdiv,udiv"))
  "falkor_x+falkor_z")

;; Move and Shift Instructions

(define_insn_reservation "falkor_mvs_1_xyz" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "mov_imm,shift_reg"))
  "falkor_xyz")

(define_insn_reservation "falkor_mvs_1_xyzb" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "adr"))
  "falkor_xyzb")

;; Other Instructions

;; Block is for instruction scheduling blockage insns in RTL.  There are no
;; hardware instructions emitted for them, so don't use any resources.

(define_insn_reservation "falkor_other_0_nothing" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "no_insn,trap,block"))
  "nothing")

(define_insn_reservation "falkor_other_2_z" 2
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "mrs"))
  "falkor_z")

;; Assume multiple instructions use all pipes.

(define_insn_reservation "falkor_extra" 1
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "multiple"))
  "falkor_x+falkor_y+falkor_z+falkor_b+falkor_vx+falkor_vy+falkor_ld+falkor_st+falkor_sd+falkor_vsd+falkor_gtov+falkor_vtog")

;; Store Instructions

;; No use of store_rel, store3, or store4 in aarch64.

(define_insn_reservation "falkor_st_0_st_sd" 0
  (and (eq_attr "tune" "falkor")
       (eq_attr "type" "store1,store2"))
  "falkor_st+falkor_sd")

;; Muliply bypasses.

;; 1 cycle latency (0 bubble) for an integer mul or mac feeding into a mac.

(define_bypass 1
  "falkor_ai_4_vxvy_mul,falkor_ai_4_vxvy_mla,falkor_ai_4_vxvy_vxvy_mul,falkor_ai_4_vxvy_vxvy_mla,falkor_muldiv_4_x_mul,falkor_muldiv_4_x_mla,falkor_muldiv_5_x_mul"
  "falkor_ai_4_vxvy_mla,falkor_ai_4_vxvy_vxvy_mla,falkor_muldiv_4_x_mla")

;; 3 cycle latency (2 bubbles) for an FP mul or mac feeding into a mac.

(define_bypass 3
  "falkor_afp_5_vxvy_mul,falkor_afp_5_vxvy_mla,falkor_afp_5_vxvy_vxvy_mul,falkor_afp_5_vxvy_vxvy_mla,falkor_afp_6_vxvy_mul,falkor_afp_6_vxvy_mla,falkor_afp_6_vxvy_vxvy_mul,falkor_afp_6_vxvy_vxvy_mla,falkor_fpdt_5_vxvy_mul,falkor_fpdt_5_vxvy_mla,falkor_fpdt_6_vxvy_mul,falkor_fpdt_6_vxvy_mla"
  "falkor_afp_5_vxvy_mla,falkor_afp_5_vxvy_vxvy_mla,falkor_afp_6_vxvy_mla,falkor_afp_6_vxvy_vxvy_mla,falkor_fpdt_5_vxvy_mla,falkor_fpdt_6_vxvy_mla")
