;; ARM Cortex-A15 NEON pipeline description
;; Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

(define_attr "cortex_a15_neon_type"
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
   neon_load_f, neon_store_a, neon_store_b, neon_store_c, neon_store_d,
   neon_store_e, neon_store_f, neon_store_g, neon_store_h,
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
                           neon_fp_reduc_minmax_s, neon_fp_reduc_minmax_s_q")
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

          (eq_attr "type" "f_loads, f_loadd,\
                           neon_load1_1reg, neon_load1_1reg_q,\
                           neon_load1_2reg, neon_load1_2reg_q")
            (const_string "neon_load_a")
          (eq_attr "type" "neon_load1_3reg, neon_load1_3reg_q,\
                           neon_load1_4reg, neon_load1_4reg_q")
            (const_string "neon_load_b")
          (eq_attr "type" "neon_load1_one_lane, neon_load1_one_lane_q,\
                           neon_load1_all_lanes, neon_load1_all_lanes_q,\
                           neon_load2_2reg, neon_load2_2reg_q,\
                           neon_load2_all_lanes, neon_load2_all_lanes_q")
            (const_string "neon_load_c")
          (eq_attr "type" "neon_load2_4reg, neon_load2_4reg_q,\
                           neon_load3_3reg, neon_load3_3reg_q,\
                           neon_load3_one_lane, neon_load3_one_lane_q,\
                           neon_load4_4reg, neon_load4_4reg_q")
            (const_string "neon_load_d")
          (eq_attr "type" "neon_load2_one_lane, neon_load2_one_lane_q,\
                           neon_load3_all_lanes, neon_load3_all_lanes_q,\
                           neon_load4_all_lanes, neon_load4_all_lanes_q")
            (const_string "neon_load_e")
          (eq_attr "type" "neon_load4_one_lane, neon_load4_one_lane_q")
            (const_string "neon_load_f")

          (eq_attr "type" "f_stores, f_stored,\
                           neon_store1_1reg, neon_store1_1reg_q")
            (const_string "neon_store_a")
          (eq_attr "type" "neon_store1_2reg, neon_store1_2reg_q")
            (const_string "neon_store_b")
          (eq_attr "type" "neon_store1_3reg, neon_store1_3reg_q")
            (const_string "neon_store_c")
          (eq_attr "type" "neon_store1_4reg, neon_store1_4reg_q")
            (const_string "neon_store_d")
          (eq_attr "type" "neon_store1_one_lane, neon_store1_one_lane_q,\
                           neon_store2_one_lane, neon_store2_one_lane_q")
            (const_string "neon_store_e")
          (eq_attr "type" "neon_store2_2reg, neon_store2_2reg_q,\
                           neon_store3_one_lane, neon_store3_one_lane_q,\
                           neon_store4_one_lane, neon_store4_one_lane_q")
            (const_string "neon_store_f")
          (eq_attr "type" "neon_store2_4reg, neon_store2_4reg_q,\
                           neon_store4_4reg, neon_store4_4reg_q")
            (const_string "neon_store_g")
          (eq_attr "type" "neon_store3_3reg, neon_store3_3reg_q")
            (const_string "neon_store_h")]
          (const_string "unknown")))

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

;; Integer Arithmetic Instructions.

(define_insn_reservation  "cortex_a15_neon_abd" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_abd"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_abd_q" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_abd_q"))
  "ca15_issue2,ca15_cx_ialu*2")

(define_insn_reservation  "cortex_a15_neon_aba" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_arith_acc"))
  "ca15_issue1,ca15_cx_ialu_with_acc")

(define_insn_reservation  "cortex_a15_neon_aba_q" 8
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_arith_acc_q"))
  "ca15_issue2,ca15_cx_ialu_with_acc*2")

(define_insn_reservation  "cortex_a15_neon_arith_basic" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_arith_basic"))
  "ca15_issue1,ca15_cx_ialu")

(define_insn_reservation  "cortex_a15_neon_arith_complex" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_arith_complex"))
  "ca15_issue1,ca15_cx_ialu")

;; Integer Multiply Instructions.

(define_insn_reservation "cortex_a15_neon_multiply" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_multiply"))
  "ca15_issue1,ca15_cx_imac")

(define_insn_reservation "cortex_a15_neon_multiply_q" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_multiply_q"))
  "ca15_issue2,ca15_cx_imac*2")

(define_insn_reservation "cortex_a15_neon_mla" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_mla"))
  "ca15_issue1,ca15_cx_imac")

(define_insn_reservation "cortex_a15_neon_mla_q" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_mla_q"))
  "ca15_issue1,ca15_cx_imac*2")

(define_insn_reservation "cortex_a15_neon_sat_mla_long" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_sat_mla_long"))
  "ca15_issue1,ca15_cx_imac")

;; Integer Shift Instructions.

(define_insn_reservation
  "cortex_a15_neon_shift_acc" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_shift_acc"))
  "ca15_issue1,ca15_cx_ishf_with_acc")

(define_insn_reservation
  "cortex_a15_neon_shift_imm_basic" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_shift_imm_basic"))
  "ca15_issue1,ca15_cx_ik+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_shift_imm_complex" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_shift_imm_complex"))
  "ca15_issue1,ca15_cx_ik+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_shift_reg_basic" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_shift_reg_basic"))
  "ca15_issue1,ca15_cx_ik+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_shift_reg_basic_q" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_shift_reg_basic_q"))
  "ca15_issue2,(ca15_cx_ik+ca15_cx_ishf*2)")

(define_insn_reservation
  "cortex_a15_neon_shift_reg_complex" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_shift_reg_complex"))
  "ca15_issue2,ca15_cx_ik+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_shift_reg_complex_q" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_shift_reg_complex_q"))
  "ca15_issue2,(ca15_cx_ik+ca15_cx_ishf)*2")

;; Floating Point Instructions.

(define_insn_reservation
  "cortex_a15_neon_fp_negabs" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_negabs"))
  "ca15_issue1,ca15_cx_falu")

(define_insn_reservation
  "cortex_a15_neon_fp_arith" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_arith"))
  "ca15_issue1,ca15_cx_falu")

(define_insn_reservation
  "cortex_a15_neon_fp_arith_q" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_arith_q"))
  "ca15_issue2,ca15_cx_falu_2")

(define_insn_reservation
  "cortex_a15_neon_fp_cvt_int" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_cvt_int"))
  "ca15_issue1,ca15_cx_falu+ca15_cx_ishf")

(define_insn_reservation
  "cortex_a15_neon_fp_cvt_int_q" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_cvt_int_q"))
  "ca15_issue2,(ca15_cx_falu+ca15_cx_ishf)*2")

(define_insn_reservation
  "cortex_a15_neon_fp_cvt16" 10
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_cvt16"))
  "ca15_issue3,(ca15_cx_falu+ca15_cx_ishf)*2+ca15_cx_falu")

(define_insn_reservation
  "cortex_a15_neon_fp_mul" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_mul"))
  "ca15_issue1,ca15_cx_fmul")

(define_insn_reservation
  "cortex_a15_neon_fp_mul_q" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_mul_q"))
  "ca15_issue2,ca15_cx_fmul_2")

(define_insn_reservation
  "cortex_a15_neon_fp_mla" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_mla"))
  "ca15_issue1,ca15_cx_fmul")

(define_insn_reservation
  "cortex_a15_neon_fp_mla_q" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_mla_q"))
  "ca15_issue2,ca15_cx_fmul_2")

(define_insn_reservation
  "cortex_a15_neon_fp_recps_rsqrte" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_recpe_rsqrte"))
  "ca15_issue1,ca15_cx_fmac")

(define_insn_reservation
  "cortex_a15_neon_fp_recps_rsqrte_q" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_fp_recpe_rsqrte_q"))
  "ca15_issue2,ca15_cx_fmac_2")

;; Miscelaaneous Instructions.

(define_insn_reservation
  "cortex_a15_neon_bitops" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_bitops"))
  "ca15_issue1,ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_bitops_q" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_bitops_q"))
  "ca15_issue2,ca15_cx_perm_2")

(define_insn_reservation
  "cortex_a15_neon_from_gp" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_from_gp"))
  "ca15_issue2,ca15_ls1+ca15_ls2+ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_from_gp_q" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_from_gp_q"))
  "ca15_issue2,ca15_ls1+ca15_ls2+ca15_cx_perm_2")

(define_insn_reservation
  "cortex_a15_neon_tbl3_tbl4" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_tbl3_tbl4"))
  "ca15_issue2,ca15_cx_perm_2")

(define_insn_reservation
  "cortex_a15_neon_zip_q" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_zip_q"))
  "ca15_issue3,ca15_cx_perm*3")

(define_insn_reservation
  "cortex_a15_neon_to_gp" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_to_gp"))
  "ca15_issue2,ca15_ls1+ca15_ls2")

;; Load Instructions.

(define_insn_reservation
  "cortex_a15_neon_load_a" 6
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_load_a"))
  "ca15_issue1,ca15_ls,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_load_b" 7
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_load_b"))
  "ca15_issue2,ca15_ls1+ca15_ls2,ca15_ldr,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_load_c" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_load_c"))
  "ca15_issue2,ca15_ls1+ca15_ls2,ca15_ldr,ca15_ldr")

(define_insn_reservation
  "cortex_a15_neon_load_d" 11
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_load_d"))
  "ca15_issue1,ca15_issue3+ca15_ls1+ca15_ls2,ca15_ldr*2")

(define_insn_reservation
  "cortex_a15_neon_load_e" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_load_e"))
  "ca15_issue3+ca15_ls1+ca15_ls2,ca15_ldr*2")

(define_insn_reservation
  "cortex_a15_neon_load_f" 11
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_load_f"))
  "ca15_issue3,ca15_issue3+ca15_ls1+ca15_ls2,ca15_ldr*2")

;; Store Instructions.

(define_insn_reservation
  "cortex_a15_neon_store_a" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_a"))
  "ca15_issue1,ca15_ls1+ca15_ls2,ca15_str")

(define_insn_reservation
  "cortex_a15_neon_store_b" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_b"))
  "ca15_issue2,ca15_ls1+ca15_ls2,ca15_str*2")

(define_insn_reservation
  "cortex_a15_neon_store_c" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_c"))
  "ca15_issue3,ca15_ls1+ca15_ls2,ca15_str*3")

(define_insn_reservation
  "cortex_a15_neon_store_d" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_d"))
  "ca15_issue3,ca15_issue1,ca15_ls1+ca15_ls2,ca15_str*4")

(define_insn_reservation
  "cortex_a15_neon_store_e" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_e"))
  "ca15_issue2,ca15_ls1+ca15_ls2,ca15_str+ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_store_f" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_f"))
  "ca15_issue3,ca15_ls1+ca15_ls2,ca15_str*2+ca15_cx_perm")

(define_insn_reservation
  "cortex_a15_neon_store_g" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_g"))
  "ca15_issue3,ca15_issue3+ca15_cx_perm+ca15_ls1+ca15_ls2,ca15_str*2")

(define_insn_reservation
  "cortex_a15_neon_store_h" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "cortex_a15_neon_type" "neon_store_h"))
  "ca15_issue3,ca15_issue2+ca15_cx_perm+ca15_ls1+ca15_ls2,ca15_str*2")

;; VFP Operations.

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

(define_insn_reservation "cortex_a15_gp_to_vfp" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "f_mcr, f_mcrr"))
  "ca15_issue1,ca15_ls")

(define_insn_reservation "cortex_a15_mov_vfp_to_gp" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "f_mrc, f_mrrc"))
  "ca15_issue1,ca15_ls")

;; Moves from floating point registers to general purpose registers
;; induce additional latency.
(define_bypass 10 "cortex_a15_vfp*, cortex_a15_neon*, cortex_a15_gp_to_vfp" "cortex_a15_mov_vfp_to_gp")


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

