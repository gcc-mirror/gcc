;; Samsung Exynos M1 pipeline description
;; Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

(define_attr "exynos_m1_neon_type"
  "neon_arith_simple, neon_arith_basic, neon_arith_complex,
   neon_multiply, neon_mla, neon_mla_q, neon_mla_long, neon_sat_mla_long,
   neon_shift_acc, neon_shift_imm_basic, neon_shift_imm_complex,
   neon_shift_reg_basic, neon_shift_reg_basic_q,
   neon_shift_reg_complex, neon_shift_reg_complex_q,
   neon_fp_unary, neon_fp_add, neon_fp_abd, neon_fp_compare,
   neon_fp_reduc_minmax, neon_fp_reduc_add, neon_fp_round, neon_fp_cvt,
   neon_fp_minmax, neon_fp_mul, neon_fp_mul_q, neon_fp_mla, neon_fp_mla_q,
   neon_fp_estimate, neon_fp_estimatex, neon_fp_step,
   neon_bitops, neon_bitops_q, neon_bitins,
   neon_to_gp, neon_from_gp, neon_move, neon_tbl,
   neon_load1_1, neon_load1_2, neon_load1_3, neon_load1_4,
   neon_load1_one, neon_load1_all,
   neon_load2_2, neon_load2_one, neon_load2_all,
   neon_load3_3, neon_load3_one, neon_load3_all,
   neon_load4_4, neon_load4_one, neon_load4_all,
   neon_store,
   neon_store1_1, neon_store1_2, neon_store1_3, neon_store1_4, neon_store1_one,
   neon_store2_2, neon_store2_one,
   neon_store3_3, neon_store3_one,
   neon_store4_4, neon_store4_one,
   unknown"
  (cond [
	  (eq_attr "type" "neon_abd, neon_abd_q, neon_abd_long,\
			   neon_abs, neon_abs_q,\
			   neon_minmax, neon_minmax_q")
	    (const_string "neon_arith_simple")

	  (eq_attr "type" "neon_add, neon_add_q, neon_add_long,\
			   neon_neg, neon_neg_q,\
			   neon_sub, neon_sub_q, neon_sub_long, neon_sub_widen,\
			   neon_logic, neon_logic_q, neon_tst, neon_tst_q,\
			   neon_compare_zero, neon_compare_zero_q")
	    (const_string "neon_arith_basic")

	  (eq_attr "type" "neon_add_widen, neon_arith_acc, neon_arith_acc_q,\
			   neon_reduc_add, neon_reduc_add_q,\
			   neon_reduc_add_acc, neon_reduc_add_acc_q,\
			   neon_reduc_add_long, neon_add_halve_narrow_q,\
			   neon_add_halve, neon_add_halve_q,\
			   neon_sub_halve, neon_sub_halve_q, neon_qabs,\
			   neon_qabs_q, neon_qadd, neon_qadd_q, neon_qneg,\
			   neon_qneg_q, neon_qsub, neon_qsub_q,\
			   neon_sub_halve_narrow_q,\
			   neon_compare, neon_compare_q,\
			   neon_reduc_minmax, neon_reduc_minmax_q")
	    (const_string "neon_arith_complex")

	  (eq_attr "type" "neon_mul_b, neon_mul_b_q, neon_mul_h, neon_mul_h_q,\
			   neon_mul_s, neon_mul_s_q,\
			   neon_mul_h_scalar, neon_mul_h_scalar_q,\
			   neon_mul_s_scalar, neon_mul_s_scalar_q,\
			   neon_mul_h_scalar_long, neon_mul_s_scalar_long,\
			   neon_sat_mul_b, neon_sat_mul_b_q,\
			   neon_sat_mul_h, neon_sat_mul_h_q,\
			   neon_sat_mul_s, neon_sat_mul_s_q,\
			   neon_sat_mul_h_scalar, neon_sat_mul_h_scalar_q,\
			   neon_sat_mul_s_scalar, neon_sat_mul_s_scalar_q,\
			   neon_sat_mul_b_long, neon_sat_mul_h_long,\
			   neon_sat_mul_s_long, neon_sat_mul_h_scalar_long,\
			   neon_sat_mul_s_scalar_long, crypto_pmull")
	    (const_string "neon_multiply")

	  (eq_attr "type" "neon_mla_b, neon_mla_h, neon_mla_s,\
			   neon_mla_h_scalar, neon_mla_s_scalar,\
			   neon_mla_b_long, neon_mla_h_long,\
			   neon_mla_s_long,\
			   neon_mla_h_scalar_long, neon_mla_s_scalar_long,\
			   neon_mla_b_q, neon_mla_h_q, neon_mla_s_q,\
			   neon_mla_h_scalar_q, neon_mla_s_scalar_q")
	    (const_string "neon_mla")

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

	  (eq_attr "type" "neon_shift_reg, neon_shift_reg_q")
	    (const_string "neon_shift_reg_basic")

	  (eq_attr "type" "neon_sat_shift_reg, neon_sat_shift_reg_q")
	    (const_string "neon_shift_reg_complex")

	  (eq_attr "type" "neon_fp_neg_s, neon_fp_neg_s_q,\
			   neon_fp_abs_s, neon_fp_abs_s_q,\
			   neon_fp_neg_d, neon_fp_neg_d_q,\
			   neon_fp_abs_d, neon_fp_abs_d_q")
	    (const_string "neon_fp_unary")

	  (eq_attr "type" "neon_fp_addsub_s, neon_fp_addsub_s_q,\
			   neon_fp_addsub_d, neon_fp_addsub_d_q")
	    (const_string "neon_fp_add")

	  (eq_attr "type" "neon_fp_abd_s, neon_fp_abd_s_q,\
			   neon_fp_abd_d, neon_fp_abd_d_q")
	    (const_string "neon_fp_abd")

	  (eq_attr "type" "neon_fp_compare_s, neon_fp_compare_s_q,\
			   neon_fp_compare_d, neon_fp_compare_d_q,\
			   neon_fp_minmax_s, neon_fp_minmax_s_q,\
			   neon_fp_minmax_d, neon_fp_minmax_d_q")
	    (const_string "neon_fp_compare")

	  (eq_attr "type" "neon_fp_reduc_minmax_s, neon_fp_reduc_minmax_s_q,\
			   neon_fp_reduc_minmax_d, neon_fp_reduc_minmax_d_q")
	    (const_string "neon_fp_reduc_minmax")

	  (eq_attr "type" "neon_fp_reduc_add_s, neon_fp_reduc_add_s_q,\
			   neon_fp_reduc_add_d, neon_fp_reduc_add_d_q")
	    (const_string "neon_fp_reduc_add")

	  (eq_attr "type" "neon_fp_round_s, neon_fp_round_s_q,\
			   neon_fp_round_d, neon_fp_round_d_q")
	    (const_string "neon_fp_round")

	  (eq_attr "type" "neon_fp_cvt_narrow_s_q, neon_fp_cvt_widen_h,
			   neon_fp_to_int_s, neon_fp_to_int_s_q,\
			   neon_fp_to_int_d_q, neon_fp_to_int_d,\
			   neon_int_to_fp_s, neon_int_to_fp_s_q,\
			   neon_int_to_fp_d, neon_int_to_fp_d_q")
	    (const_string "neon_fp_cvt")

	  (eq_attr "type" "neon_fp_mul_s, neon_fp_mul_s_q,\
			   neon_fp_mul_s_scalar, neon_fp_mul_s_scalar_q,\
			   neon_fp_mul_d, neon_fp_mul_d_q,\
			   neon_fp_mul_d_scalar_q")
	    (const_string "neon_fp_mul")

	  (eq_attr "type" "neon_fp_mla_s, neon_fp_mla_s_q,\
			   neon_fp_mla_s_scalar,neon_fp_mla_s_scalar_q,\
			   neon_fp_mla_d, neon_fp_mla_d_q,\
			   neon_fp_mla_d_scalar_q")
	    (const_string "neon_fp_mla")

	  (eq_attr "type" "neon_fp_recpe_s, neon_fp_recpe_s_q,\
			   neon_fp_rsqrte_s, neon_fp_rsqrte_s_q,\
			   neon_fp_recpe_d, neon_fp_recpe_d_q,\
			   neon_fp_rsqrte_d, neon_fp_rsqrte_d_q")
	    (const_string "neon_fp_estimate")

	  (eq_attr "type" "neon_fp_recpx_s, neon_fp_recpx_s_q,\
			   neon_fp_recpx_d, neon_fp_recpx_d_q")
	    (const_string "neon_fp_estimatex")

	  (eq_attr "type" "neon_fp_recps_s, neon_fp_recps_s_q,\
			   neon_fp_rsqrts_s, neon_fp_rsqrts_s_q,\
			   neon_fp_recps_d, neon_fp_recps_d_q,\
			   neon_fp_rsqrts_d, neon_fp_rsqrts_d_q")
	    (const_string "neon_fp_step")

	  (eq_attr "type" "neon_rbit, neon_rbit_q,\
			   neon_cls, neon_cls_q, neon_cnt, neon_cnt_q,\
			   neon_dup, neon_dup_q,\
			   neon_rev, neon_rev_q,\
			   neon_move, neon_move_q,
			   neon_ext, neon_permute, neon_zip")
	    (const_string "neon_bitops")

	  (eq_attr "type" "neon_ext_q, neon_permute_q, neon_zip_q")
	    (const_string "neon_bitops_q")

	  (eq_attr "type" "neon_bsl, neon_bsl_q")
	    (const_string "neon_bitins")

	  (eq_attr "type" "neon_tbl1, neon_tbl2, neon_tbl3, neon_tbl4")
	    (const_string "neon_tbl")

	  (eq_attr "type" "neon_from_gp, neon_from_gp_q, f_mcr, f_mcrr")
	    (const_string "neon_from_gp")

	  (eq_attr "type" "neon_to_gp, neon_to_gp_q, f_mrc, f_mrrc")
	    (const_string "neon_to_gp")

	  (eq_attr "type" "neon_load1_1reg, neon_load1_1reg_q")
	    (const_string "neon_load1_1")

	  (eq_attr "type" "neon_load1_2reg, neon_load1_2reg_q")
	    (const_string "neon_load1_2")

	  (eq_attr "type" "neon_load1_3reg, neon_load1_3reg_q")
	    (const_string "neon_load1_3")

	  (eq_attr "type" "neon_load1_4reg, neon_load1_4reg_q")
	    (const_string "neon_load1_4")

	  (eq_attr "type" "neon_load1_one_lane, neon_load1_one_lane_q")
	    (const_string "neon_load1_one")

	  (eq_attr "type" "neon_load1_all_lanes, neon_load1_all_lanes_q")
	    (const_string "neon_load1_all")

	  (eq_attr "type" "neon_load2_2reg, neon_load2_2reg_q,\
			   neon_load2_4reg, neon_load2_4reg_q")
	    (const_string "neon_load2_2")

	  (eq_attr "type" "neon_load2_one_lane, neon_load2_one_lane_q")
	    (const_string "neon_load2_one")

	  (eq_attr "type" "neon_load2_all_lanes, neon_load2_all_lanes_q")
	    (const_string "neon_load2_all")

	  (eq_attr "type" "neon_load3_3reg, neon_load3_3reg_q")
	    (const_string "neon_load3_3")

	  (eq_attr "type" "neon_load3_one_lane, neon_load3_one_lane_q")
	    (const_string "neon_load3_one")

	  (eq_attr "type" "neon_load3_all_lanes, neon_load3_all_lanes_q")
	    (const_string "neon_load3_all")

	  (eq_attr "type" "neon_load4_4reg, neon_load4_4reg_q")
	    (const_string "neon_load4_4")

	  (eq_attr "type" "neon_load4_one_lane, neon_load4_one_lane_q")
	    (const_string "neon_load4_one")

	  (eq_attr "type" "neon_load4_all_lanes, neon_load4_all_lanes_q")
	    (const_string "neon_load4_all")

	  (eq_attr "type" "neon_store1_1reg, neon_store1_1reg_q")
	    (const_string "neon_store1_1")

	  (eq_attr "type" "neon_store1_2reg, neon_store1_2reg_q")
	    (const_string "neon_store1_2")

	  (eq_attr "type" "neon_store1_3reg, neon_store1_3reg_q")
	    (const_string "neon_store1_3")

	  (eq_attr "type" "neon_store1_4reg, neon_store1_4reg_q")
	    (const_string "neon_store1_4")

	  (eq_attr "type" "neon_store1_one_lane, neon_store1_one_lane_q")
	    (const_string "neon_store1_one")

	  (eq_attr "type" "neon_store2_2reg, neon_store2_2reg_q,\
			   neon_store2_4reg, neon_store2_4reg_q")
	    (const_string "neon_store2_2")

	  (eq_attr "type" "neon_store2_one_lane, neon_store2_one_lane_q")
	    (const_string "neon_store2_one")

	  (eq_attr "type" "neon_store3_3reg, neon_store3_3reg_q")
	    (const_string "neon_store3_3")

	  (eq_attr "type" "neon_store3_one_lane, neon_store3_one_lane_q")
	    (const_string "neon_store3_one")

	  (eq_attr "type" "neon_store4_4reg, neon_store4_4reg_q")
	    (const_string "neon_store4_4")

	  (eq_attr "type" "neon_store4_one_lane, neon_store4_one_lane_q")
	    (const_string "neon_store4_one")]

	  (const_string "unknown")))

;; The Exynos M1 core is modeled as a triple issue pipeline that has
;; the following functional units.

(define_automaton "exynos_m1_gp")
(define_automaton "exynos_m1_ls")
(define_automaton "exynos_m1_fp")

;; 1.  Two pipelines for simple integer operations: A, B
;; 2.  One pipeline for simple or complex integer operations: C

(define_cpu_unit "em1_xa, em1_xb, em1_xc" "exynos_m1_gp")

(define_reservation "em1_alu" "(em1_xa | em1_xb | em1_xc)")
(define_reservation "em1_c" "em1_xc")

;; 3.  Two asymmetric pipelines for Neon and FP operations: F0, F1

(define_cpu_unit "em1_f0, em1_f1" "exynos_m1_fp")

(define_reservation "em1_fmac" "em1_f0")
(define_reservation "em1_fcvt" "em1_f0")
(define_reservation "em1_nalu" "(em1_f0 | em1_f1)")
(define_reservation "em1_nalu0" "em1_f0")
(define_reservation "em1_nalu1" "em1_f1")
(define_reservation "em1_nmisc" "em1_f0")
(define_reservation "em1_ncrypt" "em1_f0")
(define_reservation "em1_fadd" "em1_f1")
(define_reservation "em1_fvar" "em1_f1")
(define_reservation "em1_fst" "em1_f1")

;; 4.  One pipeline for branch operations: BX

(define_cpu_unit "em1_bx" "exynos_m1_gp")

(define_reservation "em1_br" "em1_bx")

;; 5.  One AGU for loads: L
;;     One AGU for stores and one pipeline for stores: S, SD

(define_cpu_unit "em1_lx" "exynos_m1_ls")
(define_cpu_unit "em1_sx, em1_sd" "exynos_m1_ls")

(define_reservation "em1_ld" "em1_lx")
(define_reservation "em1_st" "(em1_sx + em1_sd)")

;; Common occurrences
(define_reservation "em1_sfst" "(em1_fst + em1_st)")
(define_reservation "em1_lfst" "(em1_fst + em1_ld)")

;; Branches
;;
;; No latency as there is no result
;; TODO: Unconditional branches use no units;
;; conditional branches add the BX unit;
;; indirect branches add the C unit.
(define_insn_reservation "exynos_m1_branch" 0
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "branch"))
  "em1_br")

(define_insn_reservation "exynos_m1_call" 1
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "call"))
  "em1_alu")

;; Basic ALU
;;
;; Simple ALU without shift, non-predicated
(define_insn_reservation "exynos_m1_alu" 1
  (and (eq_attr "tune" "exynosm1")
       (and (not (eq_attr "predicated" "yes"))
	    (eq_attr "type" "alu_imm, alus_imm, logic_imm, logics_imm,\
			     alu_sreg, alus_sreg, logic_reg, logics_reg,\
			     adc_imm, adcs_imm, adc_reg, adcs_reg,\
			     adr, bfm, bfx, clz, rbit, rev, csel, alu_dsp_reg,\
			     shift_imm, shift_reg, rotate_imm, extend,\
			     mov_imm, mov_reg,\
			     mvn_imm, mvn_reg,\
			     mrs, multiple")))
  "em1_alu")

;; Simple ALU without shift, predicated
(define_insn_reservation "exynos_m1_alu_p" 1
  (and (eq_attr "tune" "exynosm1")
       (and (eq_attr "predicated" "yes")
	    (eq_attr "type" "alu_imm, alus_imm, logic_imm, logics_imm,\
			     alu_sreg, alus_sreg, logic_reg, logics_reg,\
			     adc_imm, adcs_imm, adc_reg, adcs_reg,\
			     adr, bfm, bfx, clz, rbit, rev, alu_dsp_reg,\
			     shift_imm, shift_reg, rotate_imm, extend,\
			     mov_imm, mov_reg,\
			     mvn_imm, mvn_reg,\
			     mrs, multiple")))
  "em1_c")

;; ALU ops with immediate shift
;; TODO: if the shift value is between 0 and 3, the latency is just 1 cycle;
;;       otherwise it takes 2 cycles and the unit is blocked;
;;       for now, assume the latter's latency and the former's units.
(define_insn_reservation "exynos_m1_alu_shift" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "alu_ext, alus_ext,\
			alu_shift_imm_lsl_1to4,alu_shift_imm_other, alus_shift_imm,\
			logic_shift_imm, logics_shift_imm,\
			mov_shift, mvn_shift"))
  "(em1_alu)")

;; ALU ops with register controlled shift, non-predicated
(define_insn_reservation "exynos_m1_alu_shift_reg" 2
  (and (eq_attr "tune" "exynosm1")
       (and (not (eq_attr "predicated" "yes"))
	    (eq_attr "type" "alu_shift_reg, alus_shift_reg,\
			     logic_shift_reg, logics_shift_reg,\
			     mov_shift_reg, mvn_shift_reg")))
   "(em1_alu * 2)")

;; ALU ops with register controlled shift, predicated
(define_insn_reservation "exynos_m1_alu_shift_reg_p" 2
  (and (eq_attr "tune" "exynosm1")
       (and (eq_attr "predicated" "yes")
	    (eq_attr "type" "alu_shift_reg, alus_shift_reg,\
			     logic_shift_reg, logics_shift_reg,\
			     mov_shift_reg, mvn_shift_reg")))
  "(em1_alu, em1_c)")

;; Integer multiply
(define_insn_reservation "exynos_m1_mla" 3
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "mul32" "yes"))
  "em1_c")

(define_insn_reservation "exynos_m1_mlal" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "widen_mul64" "yes"))
  "em1_alu, em1_c")

;; Integer divide
;; TODO: assume the median latency; blocks other divisions
(define_insn_reservation "exynos_m1_div" 13
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "udiv, sdiv"))
  "em1_c")

;; Load-store execution Unit
;;
;; Loads of up to 2 words.
(define_insn_reservation "exynos_m1_load" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "load_byte, load_4, load_8"))
  "em1_ld")

;; Loads of 3 or 4 words.
(define_insn_reservation "exynos_m1_loadm" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "load_12, load_16"))
  "(em1_ld * 3)")

;; Stores of up to 2 words.
(define_insn_reservation "exynos_m1_store" 1
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "store_4, store_8"))
  "em1_st")

;; Stores of 3 or 4 words.
(define_insn_reservation "exynos_m1_storem" 3
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "store_12, store_16"))
  "(em1_st * 3)")

;; Advanced SIMD Unit
;;
;; Integer Arithmetic Instructions.

(define_insn_reservation  "exynos_m1_arith_simple" 1
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_arith_simple"))
  "em1_nmisc")

(define_insn_reservation  "exynos_m1_neon_arith_basic" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_arith_basic"))
  "em1_nalu")

(define_insn_reservation  "exynos_m1_neon_arith_complex" 3
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_arith_complex"))
  "em1_nmisc")

;; Integer Multiply Instructions.

(define_insn_reservation "exynos_m1_neon_multiply" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type"
		"neon_multiply, neon_mla, neon_sat_mla_long"))
  "em1_nmisc")

;; Integer Shift Instructions.

(define_insn_reservation
  "exynos_m1_neon_shift_acc" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_shift_acc"))
  "em1_nalu1")

(define_insn_reservation
  "exynos_m1_neon_shift_basic" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type"
		"neon_shift_imm_basic, neon_shift_reg_basic"))
  "em1_nalu")

(define_insn_reservation
  "exynos_m1_neon_shift_complex" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type"
		"neon_shift_imm_complex, neon_shift_reg_complex"))
  "em1_nalu1")

;; Floating Point Instructions.

(define_insn_reservation
  "exynos_m1_neon_fp_unary" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_unary"))
  "em1_nalu")

(define_insn_reservation
  "exynos_m1_neon_fp_add" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_add"))
  "em1_fadd")

(define_insn_reservation
  "exynos_m1_neon_fp_abd" 3
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_abd"))
  "em1_nmisc")

(define_insn_reservation
  "exynos_m1_neon_fp_compare" 1
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_compare"))
  "em1_nmisc")

;; TODO: the latency and throughput of reduce insns actually varies between
;; 3-5 and 1/4-1, but picked the median values.
(define_insn_reservation
  "exynos_m1_neon_fp_reduc" 5
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_reduc_minmax"))
  "(em1_nmisc * 4)")

(define_insn_reservation
  "exynos_m1_neon_fp_reduc_add" 10
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_reduc_add"))
  "((em1_nalu * 2), em1_fadd)")

(define_insn_reservation
  "exynos_m1_neon_fp_round" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_round"))
  "em1_fcvt")

(define_insn_reservation
  "exynos_m1_neon_fp_cvt" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_cvt"))
  "em1_fcvt")

(define_insn_reservation
  "exynos_m1_neon_fp_mul" 5
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_mul"))
  "em1_fmac")

(define_insn_reservation
  "exynos_m1_neon_fp_mla" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_mla"))
  "em1_fmac")

(define_insn_reservation
  "exynos_m1_neon_fp_estimate" 5
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_estimate"))
  "em1_fcvt")

(define_insn_reservation
  "exynos_m1_neon_fp_estimatex" 1
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_estimatex"))
  "em1_nmisc")

(define_insn_reservation
  "exynos_m1_neon_fp_step" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_fp_step"))
  "em1_fmac")

;; Miscellaneous Instructions.

(define_insn_reservation
  "exynos_m1_neon_bitops" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_bitops"))
  "em1_nalu")

(define_insn_reservation
  "exynos_m1_neon_bitops_q" 3
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_bitops_q"))
  "(em1_nalu, em1_nalu)")

(define_insn_reservation
  "exynos_m1_neon_bitins" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_bitins"))
  "em1_nalu1")

;; TODO: it is more complicated than this.
(define_insn_reservation
  "exynos_m1_neon_tbl" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_tbl"))
  "em1_nalu1")

(define_insn_reservation
  "exynos_m1_neon_from_gp" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_from_gp"))
  "em1_st")

(define_insn_reservation
  "exynos_m1_neon_to_gp" 9
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_to_gp"))
  "em1_lfst")

;; Load Instructions.

(define_insn_reservation
  "exynos_m1_neon_load" 5
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "f_loads, f_loadd, neon_ldp"))
  "em1_ld")

(define_insn_reservation
  "exynos_m1_neon_load_q" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "neon_ldp_q"))
  "(em1_ld, em1_ld)")

(define_insn_reservation
  "exynos_m1_neon_load1_1" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load1_1, neon_load1_all"))
  "em1_ld")

(define_insn_reservation
  "exynos_m1_neon_load1_2" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load1_2"))
  "(em1_ld * 2)")

(define_insn_reservation
  "exynos_m1_neon_load1_3" 7
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load1_3"))
  "(em1_ld * 3)")

(define_insn_reservation
  "exynos_m1_neon_load1_4" 8
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load1_4"))
  "(em1_ld * 4)")

(define_insn_reservation
  "exynos_m1_neon_load1_one" 7
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load1_one"))
  "((em1_ld * 2), em1_nalu)")

(define_insn_reservation
  "exynos_m1_neon_load2_2" 10
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load2_2"))
  "(em1_ld * 5)")

(define_insn_reservation
  "exynos_m1_neon_load2_one" 7
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load2_one"))
  "((em1_ld * 2), (em1_nalu * 2))")

(define_insn_reservation
  "exynos_m1_neon_load2_all" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load2_all"))
  "(em1_ld * 2)")

(define_insn_reservation
  "exynos_m1_neon_load3_3" 12
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load3_3"))
  "(em1_ld * 6)")

(define_insn_reservation
  "exynos_m1_neon_load3_one" 9
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load3_one"))
  "((em1_ld * 4), (em1_nalu * 3))")

(define_insn_reservation
  "exynos_m1_neon_load3_all" 7
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load3_all"))
  "(em1_ld * 3)")

(define_insn_reservation
  "exynos_m1_neon_load4_4" 14
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load4_4"))
  "(em1_ld * 7)")

(define_insn_reservation
  "exynos_m1_neon_load4_one" 9
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load4_one"))
  "((em1_ld * 4), (em1_nalu * 4))")

(define_insn_reservation
  "exynos_m1_neon_load4_all" 8
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_load4_all"))
  "(em1_ld * 4)")

;; Store Instructions.

(define_insn_reservation
  "exynos_m1_neon_store" 1
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "f_stores, f_stored, neon_stp"))
  "em1_sfst")

(define_insn_reservation
  "exynos_m1_neon_store_q" 3
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "neon_stp_q"))
  "(em1_sfst * 2)")

(define_insn_reservation
  "exynos_m1_neon_store1_1" 1
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store1_1"))
  "em1_sfst")

(define_insn_reservation
  "exynos_m1_neon_store1_2" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store1_2"))
  "(em1_sfst * 2)")

(define_insn_reservation
  "exynos_m1_neon_store1_3" 3
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store1_3"))
  "(em1_sfst * 3)")

(define_insn_reservation
  "exynos_m1_neon_store1_4" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store1_4"))
  "(em1_sfst * 4)")

(define_insn_reservation
  "exynos_m1_neon_store1_one" 7
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store1_one"))
  "em1_sfst")

(define_insn_reservation
  "exynos_m1_neon_store2" 7
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store2_2, neon_store2_one"))
  "em1_sfst, em1_fst")

(define_insn_reservation
  "exynos_m1_neon_store3" 16
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store3_3, neon_store3_one"))
  "((em1_sfst * 3), (em1_fst * 2), em1_nalu)")

(define_insn_reservation
  "exynos_m1_neon_store4" 17
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "exynos_m1_neon_type" "neon_store4_4, neon_store4_one"))
  "((em1_sfst * 4), (em1_fst * 2), em1_nalu)")

;; Floating-Point Operations.

(define_insn_reservation "exynos_m1_fp_const" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fconsts, fconstd"))
  "em1_nalu")

(define_insn_reservation "exynos_m1_fp_add" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fadds, faddd"))
  "em1_fadd")

(define_insn_reservation "exynos_m1_fp_mul" 5
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fmuls, fmuld"))
  "em1_fmac")

(define_insn_reservation "exynos_m1_fp_mac" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fmacs, ffmas, fmacd, ffmad"))
  "em1_fmac")

(define_insn_reservation "exynos_m1_fp_cvt" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "f_cvt, f_rints, f_rintd"))
  "em1_fcvt")

(define_insn_reservation "exynos_m1_fp_cvt_i" 13
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "f_cvtf2i"))
  "(em1_fcvt, em1_lfst)")

(define_insn_reservation "exynos_m1_i_cvt_fp" 9
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "f_cvti2f"))
  "(em1_st, em1_fcvt)")

(define_insn_reservation "exynos_m1_fp_cmp" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fcmps, fcmpd"))
  "em1_nmisc")

(define_insn_reservation "exynos_m1_fp_ccmp" 7
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fccmps, fccmpd"))
  "(em1_st, em1_nmisc)")

(define_insn_reservation "exynos_m1_fp_sel" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fcsel"))
  "(em1_st + em1_nalu0)")

(define_insn_reservation "exynos_m1_fp_arith" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "ffariths, ffarithd"))
  "em1_nalu")

(define_insn_reservation "exynos_m1_fp_cpy" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fmov"))
  "em1_nalu")

(define_insn_reservation "exynos_m1_fp_divs" 15
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fdivs, neon_fp_div_s, neon_fp_div_s_q,\
			fsqrts, neon_fp_sqrt_s, neon_fp_sqrt_s_q"))
  "(em1_fvar * 9)")

(define_insn_reservation "exynos_m1_fp_divd" 22
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "fdivd, neon_fp_div_d, neon_fp_div_d_q,\
			fsqrtd, neon_fp_sqrt_d, neon_fp_sqrt_d_q"))
  "(em1_fvar * 9)")

(define_insn_reservation "exynos_m1_fp_minmax" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "f_minmaxs, f_minmaxd"))
  "(em1_nmisc * 2)")

;; Crypto Operations.

(define_insn_reservation "exynos_m1_crypto_simple" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "crypto_aese, crypto_aesmc,\
			crypto_sha1_xor, crypto_sha1_fast, crypto_sha256_fast"))
  "em1_ncrypt")

(define_insn_reservation "exynos_m1_crypto_complex" 6
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "crypto_sha1_slow, crypto_sha256_slow"))
  "em1_ncrypt")

(define_insn_reservation "exynos_m1_crypto_poly" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "neon_mul_b_long, neon_mul_h_long, neon_mul_s_long"))
  "em1_ncrypt")

(define_insn_reservation "exynos_m1_crypto_polyl" 4
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "neon_mul_d_long"))
  "em1_ncrypt")

(define_insn_reservation "exynos_m1_crc" 2
  (and (eq_attr "tune" "exynosm1")
       (eq_attr "type" "crc"))
  "em1_c")

;; Simple execution unit bypasses

;; Pre-decrement and post-increment addressing modes update the register quickly.
;; TODO: figure out how to tell the addressing mode register from the loaded one.
(define_bypass 1 "exynos_m1_store*, exynos_m1_neon_store*"
		 "exynos_m1_store*, exynos_m1_neon_store*,
		  exynos_m1_load*, exynos_m1_neon_load*")

;; MLAs can feed other MLAs quickly.
(define_bypass 1 "exynos_m1_mla*" "exynos_m1_mla*")

;; Insns in FMAC or FADD can feed other such insns quickly.
(define_bypass 4 "exynos_m1_fp_mul"
		 "exynos_m1_fp_add, exynos_m1_fp_mul, exynos_m1_fp_mac")
(define_bypass 5 "exynos_m1_fp_mac"
		 "exynos_m1_fp_add, exynos_m1_fp_mul, exynos_m1_fp_mac")
(define_bypass 4 "exynos_m1_neon_fp_mul"
		 "exynos_m1_neon_fp_add, exynos_m1_neon_fp_mul,\
		  exynos_m1_neon_fp_mla, exynos_m1_neon_fp_step")
(define_bypass 5 "exynos_m1_neon_fp_mla, exynos_m1_neon_fp_step"
		 "exynos_m1_neon_fp_add, exynos_m1_neon_fp_mul,\
		  exynos_m1_neon_fp_mla, exynos_m1_neon_fp_step")
(define_bypass 3 "exynos_m1_fp_add"
		 "exynos_m1_fp_add, exynos_m1_fp_mul, exynos_m1_fp_mac")
(define_bypass 3 "exynos_m1_neon_fp_add"
		 "exynos_m1_neon_fp_add, exynos_m1_neon_fp_mul,\
		  exynos_m1_neon_fp_mla, exynos_m1_neon_fp_step")

;; Insns in NALU can feed other such insns quickly.
(define_bypass 1 "exynos_m1_fp_const, exynos_m1_fp_arith, exynos_m1_fp_cpy"
		 "exynos_m1_fp_const, exynos_m1_fp_arith, exynos_m1_fp_cpy,\
		  exynos_m1_fp_sel")
(define_bypass 3 "exynos_m1_fp_sel"
		 "exynos_m1_fp_const, exynos_m1_fp_arith, exynos_m1_fp_cpy,\
		  exynos_m1_fp_sel")
(define_bypass 1 "exynos_m1_neon_arith_basic, exynos_m1_neon_shift_basic,\
		  exynos_m1_neon_bitops, exynos_m1_neon_bitins,\
		  exynos_m1_neon_tbl"
		 "exynos_m1_neon_arith_basic, exynos_m1_neon_shift_basic,\
		  exynos_m1_neon_shift_acc, exynos_m1_neon_shift_complex,\
		  exynos_m1_neon_bitops*, exynos_m1_neon_bitins,\
		  exynos_m1_neon_tbl")
(define_bypass 3 "exynos_m1_neon_shift_acc, exynos_m1_neon_shift_complex"
		 "exynos_m1_neon_arith_basic, exynos_m1_neon_shift_basic,\
		  exynos_m1_neon_shift_acc, exynos_m1_neon_shift_complex,\
		  exynos_m1_neon_bitops*, exynos_m1_neon_bitins,\
		  exynos_m1_neon_tbl")
(define_bypass 1 "exynos_m1_neon_fp_unary" "exynos_m1_neon_fp_unary")

;; Insns in NCRYPT can feed other such insns quickly.
(define_bypass 1 "exynos_m1_crypto_simple, exynos_m1_crypto_poly"
		 "exynos_m1_crypto_simple, exynos_m1_crypto_complex,\
		  exynos_m1_crypto_poly*")
(define_bypass 3 "exynos_m1_crypto_polyl"
		 "exynos_m1_crypto_simple, exynos_m1_crypto_complex,\
		  exynos_m1_crypto_poly*")
(define_bypass 5 "exynos_m1_crypto_complex"
		 "exynos_m1_crypto_simple, exynos_m1_crypto_complex,\
		  exynos_m1_crypto_poly*")

;; Predicted branches take no time, but mispredicted ones take forever anyway.
(define_bypass 1 "exynos_m1_*"
		 "exynos_m1_call, exynos_m1_branch")
