;; ARM Cortex-A17 NEON pipeline description
;; Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

(define_attr "cortex_a17_neon_type"
  "neon_abd, neon_abd_q, neon_arith_acc, neon_arith_acc_q,
   neon_arith_basic, neon_arith_complex,
   neon_reduc_add_acc, neon_multiply, neon_multiply_q,
   neon_multiply_long, neon_mla, neon_mla_q, neon_mla_long,
   neon_sat_mla_long, neon_shift_acc, neon_shift_imm_basic,\
   neon_shift_imm_complex,
   neon_shift_reg_basic, neon_shift_reg_basic_q, neon_shift_reg_complex,
   neon_shift_reg_complex_q, neon_fp_negabs, neon_fp_arith,
   neon_fp_arith_q, neon_fp_cvt_int,
   neon_fp_cvt_int_q, neon_fp_cvt16, neon_fp_minmax, neon_fp_mul,
   neon_fp_mul_q, neon_fp_mla, neon_fp_mla_q, neon_fp_recpe_rsqrte,
   neon_fp_recpe_rsqrte_q, neon_bitops, neon_bitops_q, neon_from_gp,
   neon_from_gp_q, neon_move, neon_tbl3_tbl4, neon_zip_q, neon_to_gp,
   neon_load_a, neon_load_b, neon_load_c, neon_load_d, neon_load_e,
   neon_load_f, neon_load_g, neon_load_h, neon_store_a, neon_store_b,
   unknown"
  (cond [
          (eq_attr "type" "neon_abd, neon_abd_long")
            (const_string "neon_abd")
          (eq_attr "type" "neon_abd_q")
            (const_string "neon_abd_q")
          (eq_attr "type" "neon_arith_acc, neon_reduc_add_acc,\
                           neon_reduc_add_acc_q")
            (const_string "neon_arith_acc")
          (eq_attr "type" "neon_arith_acc_q")
            (const_string "neon_arith_acc_q")
          (eq_attr "type" "neon_add, neon_add_q, neon_add_long,\
                           neon_add_widen, neon_neg, neon_neg_q,\
                           neon_reduc_add, neon_reduc_add_q,\
                           neon_reduc_add_long, neon_sub, neon_sub_q,\
                           neon_sub_long, neon_sub_widen, neon_logic,\
                           neon_logic_q, neon_tst, neon_tst_q")
            (const_string "neon_arith_basic")
          (eq_attr "type" "neon_abs, neon_abs_q, neon_add_halve_narrow_q,\
                           neon_add_halve, neon_add_halve_q,\
                           neon_sub_halve, neon_sub_halve_q, neon_qabs,\
                           neon_qabs_q, neon_qadd, neon_qadd_q, neon_qneg,\
                           neon_qneg_q, neon_qsub, neon_qsub_q,\
                           neon_sub_halve_narrow_q,\
                           neon_compare, neon_compare_q,\
                           neon_compare_zero, neon_compare_zero_q,\
                           neon_minmax, neon_minmax_q, neon_reduc_minmax,\
                           neon_reduc_minmax_q")
            (const_string "neon_arith_complex")

          (eq_attr "type" "neon_mul_b, neon_mul_h, neon_mul_s,\
                           neon_mul_h_scalar, neon_mul_s_scalar,\
                           neon_sat_mul_b, neon_sat_mul_h,\
                           neon_sat_mul_s, neon_sat_mul_h_scalar,\
                           neon_sat_mul_s_scalar,\
                           neon_mul_b_long, neon_mul_h_long,\
                           neon_mul_s_long,\
                           neon_mul_h_scalar_long, neon_mul_s_scalar_long,\
                           neon_sat_mul_b_long, neon_sat_mul_h_long,\
                           neon_sat_mul_s_long, neon_sat_mul_h_scalar_long,\
                           neon_sat_mul_s_scalar_long")
            (const_string "neon_multiply")
          (eq_attr "type" "neon_mul_b_q, neon_mul_h_q, neon_mul_s_q,\
                           neon_mul_h_scalar_q, neon_mul_s_scalar_q,\
                           neon_sat_mul_b_q, neon_sat_mul_h_q,\
                           neon_sat_mul_s_q, neon_sat_mul_h_scalar_q,\
                           neon_sat_mul_s_scalar_q")
            (const_string "neon_multiply_q")
          (eq_attr "type" "neon_mla_b, neon_mla_h, neon_mla_s,\
                           neon_mla_h_scalar, neon_mla_s_scalar,\
                           neon_mla_b_long, neon_mla_h_long,\
                           neon_mla_s_long,\
                           neon_mla_h_scalar_long, neon_mla_s_scalar_long")
            (const_string "neon_mla")
          (eq_attr "type" "neon_mla_b_q, neon_mla_h_q, neon_mla_s_q,\
                           neon_mla_h_scalar_q, neon_mla_s_scalar_q")
            (const_string "neon_mla_q")
          (eq_attr "type" "neon_sat_mla_b_long, neon_sat_mla_h_long,\
                           neon_sat_mla_s_long, neon_sat_mla_h_scalar_long,\
                           neon_sat_mla_s_scalar_long")
            (const_string "neon_sat_mla_long")

          (eq_attr "type" "neon_shift_acc, neon_shift_acc_q")
            (const_string "neon_shift_acc")
          (eq_attr "type" "neon_shift_imm, neon_shift_imm_q,\
                           neon_shift_imm_narrow_q, neon_shift_imm_long")
            (const_string "neon_shift_imm_basic")
          (eq_attr "type" "neon_sat_shift_imm, neon_sat_shift_imm_q,\
                           neon_sat_shift_imm_narrow_q")
            (const_string "neon_shift_imm_complex")
          (eq_attr "type" "neon_shift_reg")
            (const_string "neon_shift_reg_basic")
          (eq_attr "type" "neon_shift_reg_q")
            (const_string "neon_shift_reg_basic_q")
          (eq_attr "type" "neon_sat_shift_reg")
            (const_string "neon_shift_reg_complex")
          (eq_attr "type" "neon_sat_shift_reg_q")
            (const_string "neon_shift_reg_complex_q")

          (eq_attr "type" "neon_fp_neg_s, neon_fp_neg_s_q,\
                           neon_fp_abs_s, neon_fp_abs_s_q")
            (const_string "neon_fp_negabs")
          (eq_attr "type" "neon_fp_addsub_s, neon_fp_abd_s,\
                           neon_fp_reduc_add_s, neon_fp_compare_s,\
                           neon_fp_minmax_s, neon_fp_minmax_s_q,\
                           neon_fp_reduc_minmax_s, neon_fp_round_s,\
                           neon_fp_round_s_q, neon_fp_round_d,\
	                   neon_fp_round_d_q, neon_fp_reduc_minmax_s_q")
            (const_string "neon_fp_arith")
          (eq_attr "type" "neon_fp_addsub_s_q, neon_fp_abd_s_q,\
                           neon_fp_reduc_add_s_q, neon_fp_compare_s_q")
            (const_string "neon_fp_arith_q")
          (eq_attr "type" "neon_fp_to_int_s, neon_int_to_fp_s")
            (const_string "neon_fp_cvt_int")
          (eq_attr "type" "neon_fp_to_int_s_q, neon_int_to_fp_s_q")
            (const_string "neon_fp_cvt_int_q")
          (eq_attr "type" "neon_fp_cvt_narrow_s_q, neon_fp_cvt_widen_h")
            (const_string "neon_fp_cvt16")
          (eq_attr "type" "neon_fp_mul_s, neon_fp_mul_s_scalar")
            (const_string "neon_fp_mul")
          (eq_attr "type" "neon_fp_mul_s_q, neon_fp_mul_s_scalar_q")
            (const_string "neon_fp_mul_q")
          (eq_attr "type" "neon_fp_mla_s, neon_fp_mla_s_scalar")
            (const_string "neon_fp_mla")
          (eq_attr "type" "neon_fp_mla_s_q, neon_fp_mla_s_scalar_q")
            (const_string "neon_fp_mla_q")
          (eq_attr "type" "neon_fp_recpe_s, neon_fp_rsqrte_s")
            (const_string "neon_fp_recpe_rsqrte")
          (eq_attr "type" "neon_fp_recpe_s_q, neon_fp_rsqrte_s_q")
            (const_string "neon_fp_recpe_rsqrte_q")

          (eq_attr "type" "neon_bsl, neon_cls, neon_cnt,\
                           neon_rev, neon_permute,\
                           neon_tbl1, neon_tbl2, neon_zip,\
                           neon_dup, neon_dup_q, neon_ext, neon_ext_q,\
                           neon_move, neon_move_q, neon_move_narrow_q")
            (const_string "neon_bitops")
          (eq_attr "type" "neon_bsl_q, neon_cls_q, neon_cnt_q,\
                           neon_rev_q, neon_permute_q")
            (const_string "neon_bitops_q")
          (eq_attr "type" "neon_from_gp")
            (const_string "neon_from_gp")
          (eq_attr "type" "neon_from_gp_q")
            (const_string "neon_from_gp_q")
          (eq_attr "type" "neon_tbl3, neon_tbl4")
            (const_string "neon_tbl3_tbl4")
          (eq_attr "type" "neon_zip_q")
            (const_string "neon_zip_q")
          (eq_attr "type" "neon_to_gp, neon_to_gp_q")
            (const_string "neon_to_gp")

          (eq_attr "type" "neon_load1_1reg, neon_load1_1reg_q,\
                           neon_load1_one_lane, neon_load1_one_lane_q")
            (const_string "neon_load_a")

          (eq_attr "type" "neon_load1_2reg, neon_load1_2reg_q")
            (const_string "neon_load_b")

          (eq_attr "type" "neon_load1_3reg, neon_load1_3reg_q,\
                           neon_load1_all_lanes,neon_load1_all_lanes_q,\
                           neon_load2_one_lane, neon_load2_one_lane_q,\
                           neon_load2_all_lanes, neon_load2_all_lanes_q")
            (const_string "neon_load_c")

          (eq_attr "type" "neon_load1_4reg, neon_load1_4reg_q,\
                          neon_load2_2reg, neon_load2_2reg_q")
            (const_string "neon_load_d")

          (eq_attr "type" "neon_load3_one_lane,\
                           neon_load3_all_lanes,\
                           neon_load4_one_lane, neon_load4_all_lanes")
            (const_string "neon_load_e")


          (eq_attr "type" "neon_load3_one_lane_q,\
                           neon_load3_all_lanes_q,\
                           neon_load4_one_lane_q, neon_load4_all_lanes_q")
            (const_string "neon_load_f")

          (eq_attr "type" "neon_load3_3reg,neon_load3_3reg_q")
            (const_string "neon_load_g")

          (eq_attr "type" "neon_load2_4reg,neon_load2_4reg_q,\
                           neon_load4_4reg,neon_load4_4reg_q")
            (const_string "neon_load_h")

          (eq_attr "type" "neon_store1_1reg, neon_store1_1reg_q,\
                           neon_store1_2reg, neon_store1_2reg_q,\
                           neon_store1_3reg, neon_store1_3reg_q,\
                           neon_store1_4reg, neon_store1_4reg_q,\
                           neon_store1_one_lane, neon_store1_one_lane_q,\
                           neon_store2_2reg, neon_store2_2reg_q,\
                           neon_store3_one_lane, neon_store3_one_lane_q,\
                           neon_store4_one_lane, neon_store4_one_lane_q")
            (const_string "neon_store_a")

          (eq_attr "type" "neon_store2_4reg, neon_store2_4reg_q,\
                           neon_store2_one_lane, neon_store2_one_lane_q,\
                           neon_store3_3reg, neon_store3_3reg_q,\
                           neon_store4_4reg, neon_store4_4reg_q")
            (const_string "neon_store_b")
]
          (const_string "unknown")))

(define_automaton "cortex_a17_neon")

(define_cpu_unit "ca17_asimd0, ca17_asimd1" "cortex_a17_neon")
(define_cpu_unit "ca17_fdiv0,ca17_simdfpadd0, ca17_simdfpmul0" "cortex_a17_neon")
(define_cpu_unit "ca17_simdimac0, ca17_simdialu0, ca17_perm0" "cortex_a17_neon")

(define_cpu_unit "ca17_simdialu1, ca17_perm1, ca17_simdshift1" "cortex_a17_neon")
(define_cpu_unit "ca17_iacc1" "cortex_a17_neon")
(define_cpu_unit "ca17_fpmul1, ca17_fpadd1" "cortex_a17_neon")


;; Integer Arithmetic Instructions.

(define_insn_reservation  "cortex_a17_neon_abd" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_abd"))
  "(ca17_asimd0+ca17_simdialu0) | (ca17_asimd1+ca17_simdialu1)")

(define_insn_reservation  "cortex_a17_neon_abd_q" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_abd_q"))
  "ca17_asimd0+ca17_asimd1+ca17_simdialu0+ca17_simdialu1")

(define_insn_reservation  "cortex_a17_neon_aba" 7
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_arith_acc"))
  "ca17_asimd1+ca17_simdialu1, ca17_iacc1")

(define_insn_reservation  "cortex_a17_neon_aba_q" 8
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_arith_acc_q"))
  "ca17_asimd0+ca17_asimd1+ca17_simdialu0+ca17_simdialu1, ca17_iacc1*2")

(define_insn_reservation  "cortex_a17_neon_arith_basic" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_arith_basic"))
  "(ca17_asimd0+ca17_simdialu0) | (ca17_asimd1+ca17_simdialu1)")

(define_insn_reservation  "cortex_a17_neon_arith_complex" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_arith_complex"))
  "(ca17_asimd0+ca17_simdialu0) | (ca17_asimd1+ca17_simdialu1)")

;; Integer Multiply Instructions.

(define_insn_reservation "cortex_a17_neon_multiply" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_multiply"))
  "ca17_asimd0+ca17_simdimac0")

(define_insn_reservation "cortex_a17_neon_multiply_q" 7
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_multiply_q"))
  "(ca17_asimd0+ca17_simdimac0)*2")

(define_insn_reservation "cortex_a17_neon_mla" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_mla"))
  "ca17_asimd0+ca17_simdimac0*2")

(define_insn_reservation "cortex_a17_neon_mla_q" 7
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_mla_q"))
  "(ca17_asimd0+ca17_simdimac0)*2,ca17_simdimac0")

(define_insn_reservation "cortex_a17_neon_sat_mla_long" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_sat_mla_long"))
  "ca17_asimd0+ca17_simdimac0*2")

;; Integer Shift Instructions.

(define_insn_reservation
  "cortex_a17_neon_shift_acc" 7
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_shift_acc"))
  "ca17_asimd1+ca17_simdshift1,ca17_iacc1")

(define_insn_reservation
  "cortex_a17_neon_shift_imm_basic" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_shift_imm_basic"))
  "ca17_asimd1+ca17_simdshift1")

(define_insn_reservation
  "cortex_a17_neon_shift_imm_complex" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_shift_imm_complex"))
  "ca17_asimd1+ca17_simdshift1")

(define_insn_reservation
  "cortex_a17_neon_shift_reg_basic" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_shift_reg_basic"))
  "ca17_asimd1+ca17_simdshift1")

(define_insn_reservation
  "cortex_a17_neon_shift_reg_basic_q" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_shift_reg_basic_q"))
  "(ca17_asimd1+ca17_simdshift1)*2")

(define_insn_reservation
  "cortex_a17_neon_shift_reg_complex" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_shift_reg_complex"))
  "ca17_asimd1+ca17_simdshift1")

(define_insn_reservation
  "cortex_a17_neon_shift_reg_complex_q" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_shift_reg_complex_q"))
  "(ca17_asimd1+ca17_simdshift1)*2")

(define_insn_reservation
  "cortex_a17_neon_fp_negabs" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_negabs"))
  "ca17_asimd0+ca17_simdfpadd0")

(define_insn_reservation
  "cortex_a17_neon_fp_arith" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_arith"))
  "ca17_asimd0+ca17_simdfpadd0")

(define_insn_reservation
  "cortex_a17_neon_fp_arith_q" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_arith_q"))
  "(ca17_asimd0+ca17_simdfpadd0)*2")

(define_insn_reservation
  "cortex_a17_neon_fp_cvt_int" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_cvt_int"))
  "ca17_asimd0+ca17_simdfpadd0")

(define_insn_reservation
  "cortex_a17_neon_fp_cvt_int_q" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_cvt_int_q"))
  "(ca17_asimd0+ca17_simdfpadd0)*2")

(define_insn_reservation
  "cortex_a17_neon_fp_cvt16" 10
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_cvt16"))
  "ca17_asimd0+ca17_simdfpadd0")

(define_insn_reservation
  "cortex_a17_neon_fp_mul" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_mul"))
  "ca17_asimd0+ca17_simdfpmul0")

(define_insn_reservation
  "cortex_a17_neon_fp_mul_q" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_mul_q"))
  "(ca17_asimd0+ca17_simdfpmul0)*2")

(define_insn_reservation
  "cortex_a17_neon_fp_mla" 8
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_mla"))
  "ca17_asimd0+ca17_simdfpmul0,ca17_simdfpadd0")

(define_insn_reservation
  "cortex_a17_neon_fp_mla_q" 9
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_mla_q"))
  "ca17_asimd0+ca17_simdfpmul0,ca17_asimd0+ca17_simdfpadd0+ca17_simdfpmul0,ca17_simdfpadd0")

(define_insn_reservation
  "cortex_a17_neon_fp_recps_rsqrte" 9
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_recpe_rsqrte"))
  "(ca17_asimd0+ca17_perm0)|(ca17_asimd1+ca17_perm1)")

(define_insn_reservation
  "cortex_a17_neon_fp_recps_rsqrte_q" 9
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_fp_recpe_rsqrte_q"))
  "(ca17_asimd0+ca17_perm0)*2|(ca17_asimd1+ca17_perm1)*2")

;; Miscelaneous Instructions.

(define_insn_reservation
  "cortex_a17_neon_bitops" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_bitops"))
  "(ca17_asimd0+ca17_perm0) | (ca17_asimd1+ca17_perm1)")

(define_insn_reservation
  "cortex_a17_neon_bitops_q" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_bitops_q"))
  "(ca17_asimd0+ca17_perm0)*2 | (ca17_asimd1+ca17_perm1)*2")

(define_insn_reservation
  "cortex_a17_neon_from_gp" 2
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_from_gp"))
  "(ca17_asimd0+ca17_perm0)|(ca17_asimd1+ca17_perm1)")

(define_insn_reservation
  "cortex_a17_neon_from_gp_q" 3
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_from_gp_q"))
  "(ca17_asimd0+ca17_perm0)|(ca17_asimd1+ca17_perm1)")

(define_insn_reservation
  "cortex_a17_neon_tbl3_tbl4" 7
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_tbl3_tbl4"))
  "(ca17_asimd0+ca17_perm0)|(ca17_asimd1+ca17_perm1)")

(define_insn_reservation
  "cortex_a17_neon_zip_q" 7
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_zip_q"))
  "(ca17_asimd0+ca17_perm0)|(ca17_asimd1+ca17_perm1)")

(define_insn_reservation
  "cortex_a17_neon_to_gp" 2
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_to_gp"))
  "ca17_asimd0+ca17_perm0*3")

(define_insn_reservation
  "cortex_a17_vfp_flag" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "f_flag"))
  "ca17_asimd0+ca17_perm0")

;; Load Instructions.

(define_insn_reservation
  "cortex_a17_vfp_load" 5
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "f_loads, f_loadd"))
  "ca17_ls0|ca17_ls1")

(define_insn_reservation
  "cortex_a17_neon_load_a" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_a"))
  "ca17_ls0*2|ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_load_b" 7
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_b"))
  "ca17_ls0*2|ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_load_c" 8
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_c"))
  "ca17_ls0*2|ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_load_d" 9
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_d"))
  "ca17_ls0*2|ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_load_e" 9
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_e"))
  "ca17_ls0*2|ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_load_f" 10
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_f"))
  "ca17_ls0*2+ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_load_g" 10
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_g"))
  "ca17_ls0*2+ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_load_h" 11
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_load_h"))
  "ca17_ls0*2+ca17_ls1*2")

;; Store Instructions.

(define_insn_reservation
  "cortex_a17_vfp_store" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "f_stores, f_stored"))
  "ca17_ls0|ca17_ls1")


(define_insn_reservation
  "cortex_a17_neon_store_a" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_store_a"))
  "ca17_ls0*2|ca17_ls1*2")

(define_insn_reservation
  "cortex_a17_neon_store_b" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "cortex_a17_neon_type" "neon_store_b"))
  "ca17_ls0*2+ca17_ls1*2")

;; VFP Operations.

(define_insn_reservation "cortex_a17_vfp_const" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fconsts,fconstd"))
  "ca17_asimd1+ca17_fpadd1")

(define_insn_reservation "cortex_a17_vfp_adds_subs" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fadds"))
  "ca17_asimd1+ca17_fpadd1")


(define_insn_reservation "cortex_a17_vfp_addd_subd" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "faddd"))
  "ca17_asimd1+ca17_fpadd1")

(define_insn_reservation "cortex_a17_vfp_mul" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fmuls,fmuld"))
  "ca17_asimd1+ca17_fpmul1")

(define_insn_reservation "cortex_a17_vfp_mac" 11
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fmacs,ffmas,fmacd,ffmad"))
  "ca17_asimd1+ca17_fpmul1,ca17_fpadd1")

(define_insn_reservation "cortex_a17_vfp_cvt" 6
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "f_cvt,f_cvtf2i,f_cvti2f,f_rints,f_rintd"))
  "ca17_asimd1+ca17_fpadd1")

(define_insn_reservation "cortex_a17_vfp_cmp" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fcmps,fcmpd"))
  "ca17_asimd1+ca17_fpadd1")

(define_insn_reservation "cortex_a17_vfp_arithd" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "ffarithd"))
  "ca17_asimd1+ca17_fpadd1")

(define_insn_reservation "cortex_a17_vfp_cpys" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fmov,fcsel"))
  "ca17_asimd1+ca17_fpadd1")

(define_insn_reservation "cortex_a17_gp_to_vfp" 2
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "f_mcr, f_mcrr"))
  "(ca17_asimd0+ca17_perm0)|(ca17_asimd1+ca17_perm1)")

(define_insn_reservation "cortex_a17_mov_vfp_to_gp" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "f_mrc, f_mrrc"))
  "ca17_asimd0+ca17_perm0*3")

(define_insn_reservation "cortex_a17_vfp_ariths" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "ffariths"))
  "ca17_asimd1+ca17_fpadd1")

(define_insn_reservation "cortex_a17_vfp_divs" 18
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fdivs, fsqrts"))
  "ca17_asimd0+ca17_fdiv0*10")

(define_insn_reservation "cortex_a17_vfp_divd" 32
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "fdivd, fsqrtd"))
  "ca17_asimd0+ca17_fdiv0*10")

