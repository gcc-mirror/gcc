;; Saphira pipeline description
;; Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

(define_automaton "saphira")

;; Complex int instructions (e.g. multiply and divide) execute in the X
;; pipeline.  Simple int instructions execute in the X, Y, Z and B pipelines.

(define_cpu_unit "saphira_x" "saphira")
(define_cpu_unit "saphira_y" "saphira")

;; Branches execute in the Z or B pipeline or in one of the int pipelines depending
;; on how complex it is.  Simple int insns (like movz) can also execute here.

(define_cpu_unit "saphira_z" "saphira")
(define_cpu_unit "saphira_b" "saphira")

;; Vector and FP insns execute in the VX and VY pipelines.

(define_automaton "saphira_vfp")

(define_cpu_unit "saphira_vx" "saphira_vfp")
(define_cpu_unit "saphira_vy" "saphira_vfp")

;; Loads execute in the LD pipeline.
;; Stores execute in the ST pipeline, for address, data, and
;; vector data.

(define_automaton "saphira_mem")

(define_cpu_unit "saphira_ld" "saphira_mem")
(define_cpu_unit "saphira_st" "saphira_mem")

;; The GTOV and VTOG pipelines are for general to vector reg moves, and vice
;; versa.

(define_cpu_unit "saphira_gtov" "saphira")
(define_cpu_unit "saphira_vtog" "saphira")

;; Common reservation combinations.

(define_reservation "saphira_vxvy" "saphira_vx|saphira_vy")
(define_reservation "saphira_zb"   "saphira_z|saphira_b")
(define_reservation "saphira_xyzb" "saphira_x|saphira_y|saphira_z|saphira_b")

;; SIMD Floating-Point Instructions

(define_insn_reservation "saphira_afp_1_vxvy" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_neg_s,neon_fp_neg_d,neon_fp_abs_s,neon_fp_abs_d,neon_fp_neg_s_q,neon_fp_neg_d_q,neon_fp_abs_s_q,neon_fp_abs_d_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_2_vxvy" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_minmax_s,neon_fp_minmax_d,neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_d,neon_fp_compare_s,neon_fp_compare_d,neon_fp_round_s,neon_fp_round_d,neon_fp_minmax_s_q,neon_fp_minmax_d_q,neon_fp_compare_s_q,neon_fp_compare_d_q,neon_fp_round_s_q,neon_fp_round_d_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_3_vxvy" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_reduc_minmax_s_q,neon_fp_reduc_minmax_d_q,neon_fp_abd_s,neon_fp_abd_d,neon_fp_addsub_s,neon_fp_addsub_d,neon_fp_reduc_add_s,neon_fp_reduc_add_d,neon_fp_abd_s_q,neon_fp_abd_d_q,neon_fp_addsub_s_q,neon_fp_addsub_d_q,neon_fp_reduc_add_s_q,neon_fp_reduc_add_d_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_4_vxvy" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_to_int_s,neon_fp_to_int_d,neon_int_to_fp_s,neon_int_to_fp_d,neon_fp_cvt_widen_h,neon_fp_cvt_widen_s,neon_fp_to_int_s_q,neon_fp_to_int_d_q,neon_int_to_fp_s_q,neon_int_to_fp_d_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_5_vxvy_mul" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_mul_s,neon_fp_mul_s_scalar,neon_fp_mul_s_q,neon_fp_mul_s_scalar_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_5_vxvy_mla" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_mla_s,neon_fp_mla_s_scalar,neon_fp_mla_s_q,neon_fp_mla_s_scalar_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_6_vxvy_mul" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_mul_d,neon_fp_mul_d_q,neon_fp_mul_d_scalar_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_6_vxvy_mla" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_mla_d,neon_fp_mla_d_q,neon_fp_mla_d_scalar_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_afp_4_vxvy_vxvy_vxvy" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_cvt_narrow_s_q,neon_fp_cvt_narrow_d_q"))
  "saphira_vxvy+saphira_vxvy,saphira_vxvy")

(define_insn_reservation "saphira_afp_6_vx_vy" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_div_s"))
  "saphira_vx+saphira_vy")

(define_insn_reservation "saphira_afp_11_vx_vy" 11
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_div_d"))
  "saphira_vx+saphira_vy")

(define_insn_reservation "saphira_afp_6_vx_vy_vx_vy" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_div_s_q"))
  "(saphira_vx+saphira_vy),(saphira_vx+saphira_vy)")

(define_insn_reservation "saphira_afp_11_vx_vy_vx_vy" 11
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_div_d_q"))
  "(saphira_vx+saphira_vy),(saphira_vx+saphira_vy)")

(define_insn_reservation "saphira_afp_12_vx_vy" 12
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_sqrt_s"))
  "saphira_vx+saphira_vy")

(define_insn_reservation "saphira_afp_22_vx_vy" 22
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_sqrt_d"))
  "saphira_vx+saphira_vy")

(define_insn_reservation "saphira_afp_12_vx_vy_vx_vy" 12
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_sqrt_s_q"))
  "(saphira_vx+saphira_vy),(saphira_vx+saphira_vy)")

(define_insn_reservation "saphira_afp_22_vx_vy_vx_vy" 22
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_sqrt_d_q"))
  "(saphira_vx+saphira_vy),(saphira_vx+saphira_vy)")

;; SIMD Integer Instructions

(define_insn_reservation "saphira_ai_1_vxvy" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_add,neon_reduc_add,neon_logic,neon_neg,neon_sub,neon_add_q,neon_reduc_add_q,neon_logic_q,neon_neg_q,neon_sub_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_ai_2_vxvy" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_add_long,neon_sub_long,neon_add_halve,neon_sub_halve,neon_shift_imm,neon_shift_reg,neon_minmax,neon_abs,neon_compare,neon_compare_zero,neon_tst,neon_shift_imm_long,neon_reduc_add_long,neon_add_halve_q,neon_sub_halve_q,neon_shift_imm_q,neon_shift_reg_q,neon_minmax_q,neon_abs_q,neon_compare_q,neon_compare_zero_q,neon_tst_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_ai_3_vxvy" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_shift_acc,neon_reduc_add_acc,neon_abd,neon_qadd,neon_qsub,neon_qabs,neon_qneg,neon_sat_shift_imm,neon_sat_shift_imm_narrow_q,neon_sat_shift_reg,neon_shift_acc_q,neon_reduc_add_acc_q,neon_abd_q,neon_abd_long,neon_qadd_q,neon_qsub_q,neon_qabs_q,neon_qneg_q,neon_sat_shift_imm_q,neon_sat_shift_reg_q,neon_add_halve_narrow_q,neon_sub_halve_narrow_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_ai_4_vxvy" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_reduc_minmax,neon_reduc_minmax_q,neon_arith_acc,neon_arith_acc_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_ai_4_vxvy_mul" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_mul_b,neon_mul_h,neon_mul_s,neon_mul_h_scalar,neon_mul_s_scalar,neon_sat_mul_b,neon_sat_mul_h,neon_sat_mul_s,neon_sat_mul_h_scalar,neon_sat_mul_s_scalar,neon_mul_b_q,neon_mul_h_q,neon_mul_s_q,neon_mul_h_scalar_q,neon_mul_s_scalar_q,neon_sat_mul_b_q,neon_sat_mul_h_q,neon_sat_mul_s_q,neon_mul_b_long,neon_mul_h_long,neon_mul_s_long,neon_mul_d_long,neon_mul_h_scalar_long,neon_mul_s_scalar_long,neon_sat_mul_b_long,neon_sat_mul_h_long,neon_sat_mul_s_long,neon_sat_mul_h_scalar_q,neon_sat_mul_s_scalar_q,neon_sat_mul_h_scalar_long,neon_sat_mul_s_scalar_long"))
  "saphira_vxvy")

(define_insn_reservation "saphira_ai_4_vxvy_mla" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_mla_b,neon_mla_h,neon_mla_s,neon_mla_h_scalar,neon_mla_s_scalar,neon_mla_b_q,neon_mla_h_q,neon_mla_s_q,neon_mla_h_scalar_q,neon_mla_s_scalar_q,neon_mla_b_long,neon_mla_h_long,neon_mla_s_long,neon_mla_h_scalar_long,neon_mla_s_scalar_long,neon_sat_mla_b_long,neon_sat_mla_h_long,neon_sat_mla_s_long,neon_sat_mla_h_scalar_long,neon_sat_mla_s_scalar_long"))
  "saphira_vxvy")

(define_insn_reservation "saphira_2_ai_vxvy_vxvy" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_add_widen,neon_sub_widen"))
  "(saphira_vxvy),(saphira_vxvy)")

;; SIMD Load Instructions

(define_insn_reservation "saphira_ald_4_ld" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q,neon_load1_all_lanes,neon_load2_one_lane"))
  "saphira_ld")

(define_insn_reservation "saphira_ald_4_ld_none" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_load1_2reg,neon_load2_2reg,neon_load2_all_lanes"))
  "saphira_ld")

(define_insn_reservation "saphira_ald_4_ld_ld" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_load1_2reg_q,neon_load2_2reg_q,neon_load2_all_lanes_q,neon_load3_one_lane,neon_load4_one_lane,neon_ldp,neon_ldp_q"))
  "saphira_ld,saphira_ld")

(define_insn_reservation "saphira_ald_4_ld_ld_none" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_load1_3reg,neon_load3_3reg,neon_load3_all_lanes"))
  "saphira_ld,saphira_ld")

(define_insn_reservation "saphira_ald_4_ld_ld_ld" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_load1_3reg_q,neon_load3_3reg_q,neon_load3_all_lanes_q"))
  "saphira_ld,saphira_ld,saphira_ld")

(define_insn_reservation "saphira_ald_4_ld_ld_none_none" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_load1_4reg,neon_load4_4reg"))
  "saphira_ld,saphira_ld")

(define_insn_reservation "saphira_ald_4_ld_ld_ld_ld" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_load1_4reg_q,neon_load4_4reg_q,neon_load4_all_lanes,neon_load4_all_lanes_q"))
  "saphira_ld,saphira_ld,saphira_ld,saphira_ld")

;; Arithmetic and Logical Instructions

(define_insn_reservation "saphira_alu_1_xyz" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "alus_sreg,alus_imm,alus_shift_imm,csel,adc_reg,alu_imm,alu_sreg,alu_shift_imm,alu_ext,alus_ext,logic_imm,logic_reg,logic_shift_imm,logics_imm,logics_reg,logics_shift_imm,mov_reg"))
  "saphira_xyzb")

;; SIMD Miscellaneous Instructions

;; No separate type for ins and dup.  But this is correct for both.

(define_insn_reservation "saphira_am_3_gtov" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_from_gp"))
  "saphira_gtov")

;; No separate type for ins and dup.  Assuming dup is more common.  Ins is
;; gtov+vxvy and latency of 4.

(define_insn_reservation "saphira_am_3_gtov_gtov" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_from_gp_q"))
  "saphira_gtov,saphira_gtov")

;; DUP  does not use vector pipes in Q mode, only gtov+gtov.
(define_insn_reservation "saphira_am_1_gtov_gtov" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_dup_q"))
  "saphira_gtov,saphira_gtov")

;; neon_to_gp_q is used for 32-bit ARM instructions that move 64-bits of data
;; so no use needed here.

(define_insn_reservation "saphira_am_3_vtog" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_to_gp"))
  "saphira_vtog")

(define_insn_reservation "saphira_am_1_vxvy" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_bsl,neon_dup,neon_ext,neon_ins,neon_ins_q,neon_move,neon_rev,neon_tbl1,neon_permute,neon_shift_imm_narrow_q,neon_bsl_q,neon_ext_q,neon_move_q,neon_rev_q,neon_tbl1_q,neon_permute_q,neon_tbl1,neon_tbl1_q,neon_tbl2_q,neon_tbl2"))
  "saphira_vxvy")

(define_insn_reservation "saphira_am_2_vxvy" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_cls,neon_cnt,neon_rbit,neon_cls_q,neon_cnt_q,neon_rbit_q,neon_tbl2,neon_tbl3_q,neon_tbl3"))
  "saphira_vxvy")

(define_insn_reservation "saphira_am_3_vxvy" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_recpe_s,neon_fp_recpe_d,neon_fp_rsqrte_s,neon_fp_rsqrte_d,neon_fp_recpx_s,neon_fp_recpx_d,neon_fp_recpe_s_q,neon_fp_recpe_d_q,neon_fp_rsqrte_s_q,neon_fp_rsqrte_d_q,neon_tbl4_q,neon_tbl4"))
  "saphira_vxvy")

(define_insn_reservation "saphira_am_5_vxvy" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_recps_s,neon_fp_recps_s_q"))
  "saphira_vxvy")

(define_insn_reservation "saphira_am_6_vxvy" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_fp_recps_d,neon_fp_rsqrts_d,neon_fp_recps_d_q,neon_fp_rsqrts_d_q"))
  "saphira_vxvy")

;; SIMD Store Instructions

;; ??? stp is neon_store1_2reg in aarch64.md, but neon_stp in aarch64-simd.md.
;; Similarly with ldp.

(define_insn_reservation "saphira_ast_st_vsd" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_store1_1reg,neon_store1_1reg_q,neon_store1_one_lane,neon_store1_one_lane_q,neon_store1_2reg,neon_store2_2reg,neon_store2_one_lane,neon_store2_one_lane_q,neon_stp"))
  "saphira_st")

(define_insn_reservation "saphira_as_0_st_vsd_st_vsd" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_store1_2reg_q,neon_store1_3reg,neon_store1_4reg,neon_store2_2reg_q,neon_store3_3reg,neon_store4_4reg,neon_store3_one_lane,neon_store3_one_lane_q,neon_store4_one_lane,neon_store4_one_lane_q,neon_stp_q"))
  "(saphira_st),(saphira_st)")

(define_insn_reservation "saphira_as_0_st_vsd_st_vsd_st_vsd" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_store1_3reg_q,neon_store3_3reg_q"))
  "(saphira_st),(saphira_st),(saphira_st)")

(define_insn_reservation "saphira_as_0_st_vsd_st_vsd_st_vsd_st_vsd" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "neon_store1_4reg_q,neon_store4_4reg_q"))
  "(saphira_st),(saphira_st),(saphira_st),(saphira_st)")

;; Branch Instructions

(define_insn_reservation "saphira_branch_0_zb" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "branch"))
  "saphira_zb")

(define_insn_reservation "saphira_call_0_xyzb" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "call"))
  "saphira_xyzb")

;; Cryptography Extensions

(define_insn_reservation "saphira_cry_1_vxvy" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "crypto_sha1_fast"))
  "saphira_vxvy")

(define_insn_reservation "saphira_cry_2_vxvy" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "crypto_aesmc"))
  "saphira_vxvy")

(define_insn_reservation "saphira_cry_2_vxvy_vxvy" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "crypto_sha1_xor,crypto_sha256_fast,crypto_pmull,crypto_aese"))
  "saphira_vxvy")

(define_insn_reservation "saphira_cry_4_vy_vx" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "crypto_sha1_slow"))
  "saphira_vxvy")

(define_insn_reservation "saphira_cry_5_vy_vx" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "crypto_sha256_slow"))
  "saphira_vxvy")

;; FP Load Instructions

(define_insn_reservation "saphira_fld_4_ld" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_loads,f_loadd"))
  "saphira_ld")

;; No separate FP store section, these are found in the SIMD store section.

(define_insn_reservation "saphira_fld_0_st_vsd" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_stores,f_stored"))
  "saphira_st")

;; FP Data Processing Instructions

(define_insn_reservation "saphira_fpdt_0_vxvy" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fcmps,fcmpd,fccmps,fccmpd"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_5_vtog" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_cvtf2i"))
  "saphira_vtog")

(define_insn_reservation "saphira_fpdt_1_vxvy" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "ffariths,ffarithd,fcsel"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_2_vxvy" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_minmaxd,f_minmaxs,f_rintd,f_rints"))
  "saphira_vxvy")

;; Scalar FP ABD is handled same as vector FP ABD.

(define_insn_reservation "saphira_fpdt_3_vxvy" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "faddd,fadds"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_4_vxvy" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_cvt"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_5_vxvy_mul" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fmuls"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_5_vxvy_mla" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fmacs,ffmas"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_6_vxvy_mul" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fmuld"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_6_vxvy_mla" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fmacd,ffmad"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_6_vx_vy" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fdivs"))
  "saphira_vx+saphira_vy")

(define_insn_reservation "saphira_fpdt_11_vx_vy" 11
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fdivd"))
  "saphira_vx+saphira_vy")

(define_insn_reservation "saphira_fpdt_12_vx_vy" 12
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fsqrts"))
  "saphira_vxvy")

(define_insn_reservation "saphira_fpdt_22_vx_vy" 22
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fsqrtd"))
  "saphira_vxvy")

;; FP Miscellaneous Instructions

(define_insn_reservation "saphira_fpmsc_3_vtog" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_mrc"))
  "saphira_vtog")

(define_insn_reservation "saphira_fpmsc_3_gtov" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_mcr"))
  "saphira_gtov")

(define_insn_reservation "saphira_fpmsc_1_vxvy" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "fmov,fconsts,fconstd"))
  "saphira_vxvy")

;; No separate type for float-to-fixed conversions.  Same type as
;; float-to-int conversions.  They schedule the same though, so no problem.

(define_insn_reservation "saphira_fpmsc_6_gtov" 6
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "f_cvti2f"))
  "saphira_gtov")

;; Load Instructions

(define_insn_reservation "saphira_ld_3_ld" 3
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "load_4,load_8,load_16"))
  "saphira_ld")

;; Miscellaneous Data-Processing Instructions

(define_insn_reservation "saphira_misc_1_xyzb" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "bfx,bfm,extend,rotate_imm,shift_imm"))
  "saphira_xyzb")

(define_insn_reservation "saphira_misc_2_x" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "crc"))
  "saphira_x")

(define_insn_reservation "saphira_misc_2_xyzb" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "clz,rbit,rev"))
  "saphira_xyzb")

;; Divide and Multiply Instructions

(define_insn_reservation "saphira_muldiv_4_x_mul" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "mul"))
  "saphira_x")

(define_insn_reservation "saphira_muldiv_4_x_mla" 4
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "mla,smlal,umlal"))
  "saphira_x")

(define_insn_reservation "saphira_muldiv_5_x_mul" 5
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "smull,umull"))
  "saphira_x")

(define_insn_reservation "saphira_md_11_x_zb" 11
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "sdiv,udiv"))
  "saphira_x+saphira_zb")

;; Move and Shift Instructions

(define_insn_reservation "saphira_mvs_1_xyzb" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "mov_imm,shift_reg,adr"))
  "saphira_xyzb")

;; Other Instructions

;; Block is for instruction scheduling blockage insns in RTL.  There are no
;; hardware instructions emitted for them, so don't use any resources.

(define_insn_reservation "saphira_other_0_nothing" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "trap,block"))
  "nothing")

(define_insn_reservation "saphira_other_2_ld" 2
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "mrs"))
  "saphira_ld")

;; Assume multiple instructions use all pipes.

(define_insn_reservation "saphira_extra" 1
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "multiple"))
  "saphira_x+saphira_y+saphira_z+saphira_b+saphira_vx+saphira_vy+saphira_ld+saphira_st+saphira_gtov+saphira_vtog")

;; Store Instructions

;; No use of store_rel, store3, or store4 in aarch64.

(define_insn_reservation "saphira_st_0_st_sd" 0
  (and (eq_attr "tune" "saphira")
       (eq_attr "type" "store_4,store_8,store_16"))
  "saphira_st")

;; Muliply bypasses.

;; 1 cycle latency (0 bubble) for an integer mul or mac feeding into a mac.

(define_bypass 1
  "saphira_ai_4_vxvy_mul,saphira_ai_4_vxvy_mla,saphira_muldiv_4_x_mul,saphira_muldiv_4_x_mla,saphira_muldiv_5_x_mul"
  "saphira_ai_4_vxvy_mla,saphira_muldiv_4_x_mla")

;; 3 cycle latency (2 bubbles) for an FP mul or mac feeding into a mac.

(define_bypass 3
  "saphira_afp_5_vxvy_mul,saphira_afp_5_vxvy_mla,saphira_afp_6_vxvy_mul,saphira_afp_6_vxvy_mla,saphira_fpdt_5_vxvy_mul,saphira_fpdt_5_vxvy_mla,saphira_fpdt_6_vxvy_mul,saphira_fpdt_6_vxvy_mla"
  "saphira_afp_5_vxvy_mla,saphira_afp_6_vxvy_mla,saphira_fpdt_5_vxvy_mla,saphira_fpdt_6_vxvy_mla")

