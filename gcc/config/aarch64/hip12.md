;; Hisilicon hip12 pipeline description
;; Copyright (C) 2026-2026 Free Software Foundation, Inc.
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

(define_automaton "hip12")
(define_automaton "hip12_ldst")
(define_automaton "hip12_v")

(define_attr "hip12_neon_type"
  "neon_arith_basic, neon_arith_complex, neon_aba, neon_addv,
   neon_dot, neon_minmaxv,
   neon_multiply, neon_mla,
   neon_shift_acc, neon_shift_imm_basic, neon_shift_imm_complex,
   neon_shift_reg_basic, neon_shift_reg_complex,
   neon_fp_negabs, neon_fp_arith, neon_fp_reductions,
   neon_fp_round_q, neon_fp_cvt_int, neon_fp_cvt_int_q,
   neon_fp_cvt, neon_fp_mul, neon_fp_mla, neon_fp_fcadd,
   neon_fp_fcmla, neon_fp_div_s, neon_fp_div_d,
   neon_fp_div_s_q, neon_fp_div_d_q, neon_fp_recpe_rsqrte,
   neon_fp_recpe_rsqrte_q, neon_fp_recpx, neon_fp_recps_rsqrts,
   neon_fp_sqrt_s, neon_fp_sqrt_d, neon_fp_sqrt_s_q, neon_fp_sqrt_d_q,
   neon_bitops_basic, neon_bitops_complex, neon_tbl3_tbl4,
   neon_from_gp, neon_to_gp,
   neon_load_a, neon_load_b, neon_load_c, neon_load_d,
   neon_store_a, neon_store_b, neon_store_c,
   crypto_aes, crypto_sha1_fast, crypto_sha_fast, crypto_sha_slow,
   unknown"
  (cond [
	  (eq_attr "type" "neon_add, neon_add_q, neon_add_widen,\
			   neon_neg, neon_neg_q,\
			   neon_sub, neon_sub_q, neon_sub_widen,\
			   neon_logic, neon_logic_q,\
			   neon_abs, neon_abs_q")
	    (const_string "neon_arith_basic")
	  (eq_attr "type" "neon_abd, neon_abd_long, neon_abd_q,\
			   neon_reduc_add_acc, neon_reduc_add_acc_q,\
			   neon_add_long, neon_sub_long,\
			   neon_add_halve_narrow_q, neon_sub_halve_narrow_q,\
			   neon_add_halve, neon_add_halve_q,\
			   neon_sub_halve, neon_sub_halve_q,\
			   neon_qabs, neon_qabs_q, neon_qadd, neon_qadd_q,\
			   neon_qneg, neon_qneg_q, neon_qsub, neon_qsub_q,\
			   neon_tst, neon_tst_q, neon_compare, neon_compare_q,\
			   neon_compare_zero, neon_compare_zero_q,\
			   neon_minmax, neon_minmax_q")
	    (const_string "neon_arith_complex")
	  (eq_attr "type" "neon_arith_acc, neon_arith_acc_q")
	    (const_string "neon_aba")
	  (eq_attr "type" "neon_reduc_add, neon_reduc_add_q, neon_reduc_add_long")
	    (const_string "neon_addv")
	  (eq_attr "type" "neon_dot, neon_dot_q")
	    (const_string "neon_dot")
	  (eq_attr "type" "neon_reduc_minmax, neon_reduc_minmax_q")
	    (const_string "neon_minmaxv")

	  (eq_attr "type" "neon_mul_b, neon_mul_h, neon_mul_s,\
			   neon_mul_h_scalar, neon_mul_s_scalar,\
			   neon_sat_mul_b, neon_sat_mul_h, neon_sat_mul_s,\
			   neon_sat_mul_h_scalar, neon_sat_mul_s_scalar,\
			   neon_mul_b_long, neon_mul_h_long,\
			   neon_mul_s_long, neon_mul_d_long,\
			   neon_mul_h_scalar_long, neon_mul_s_scalar_long,\
			   neon_sat_mul_b_long, neon_sat_mul_h_long, neon_sat_mul_s_long,\
			   neon_sat_mul_h_scalar_long, neon_sat_mul_s_scalar_long,\
			   neon_mul_b_q, neon_mul_h_q, neon_mul_s_q,\
			   neon_mul_h_scalar_q, neon_mul_s_scalar_q,\
			   neon_sat_mul_b_q, neon_sat_mul_h_q, neon_sat_mul_s_q,\
			   neon_sat_mul_h_scalar_q, neon_sat_mul_s_scalar_q")
	    (const_string "neon_multiply")
	  (eq_attr "type" "neon_mla_b, neon_mla_h, neon_mla_s,\
			   neon_mla_h_scalar, neon_mla_s_scalar,\
			   neon_mla_b_long, neon_mla_h_long, neon_mla_s_long,\
			   neon_mla_h_scalar_long, neon_mla_s_scalar_long,\
			   neon_mla_b_q, neon_mla_h_q, neon_mla_s_q,\
			   neon_mla_h_scalar_q, neon_mla_s_scalar_q,\
			   neon_sat_mla_b_long, neon_sat_mla_h_long, neon_sat_mla_s_long,\
			   neon_sat_mla_h_scalar_long, neon_sat_mla_s_scalar_long")
	    (const_string "neon_mla")

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
			   neon_fp_neg_d, neon_fp_neg_d_q,\
			   neon_fp_abs_s, neon_fp_abs_s_q,\
			   neon_fp_abs_d, neon_fp_abs_d_q")
	    (const_string "neon_fp_negabs")
	  (eq_attr "type" "neon_fp_addsub_s, neon_fp_addsub_s_q,\
			   neon_fp_addsub_d, neon_fp_addsub_d_q,\
			   neon_fp_abd_s, neon_fp_abd_s_q,\
			   neon_fp_abd_d, neon_fp_abd_d_q,\
			   neon_fp_compare_s, neon_fp_compare_s_q,\
			   neon_fp_compare_d, neon_fp_compare_d_q,\
			   neon_fp_minmax_s, neon_fp_minmax_s_q,\
			   neon_fp_minmax_d, neon_fp_minmax_d_q")
	    (const_string "neon_fp_arith")
	  (eq_attr "type" "neon_fp_reduc_add_s, neon_fp_reduc_add_s_q,\
			   neon_fp_reduc_add_d, neon_fp_reduc_add_d_q,\
			   neon_fp_round_s, neon_fp_round_d,\
			   neon_fp_reduc_minmax_s, neon_fp_reduc_minmax_s_q,\
			   neon_fp_reduc_minmax_d, neon_fp_reduc_minmax_d_q")
	    (const_string "neon_fp_reductions")
	  (eq_attr "type" "neon_fp_round_s_q, neon_fp_round_d_q")
	    (const_string "neon_fp_round_q")
	  (eq_attr "type" "neon_fp_to_int_s, neon_int_to_fp_s,\
			   neon_fp_to_int_d, neon_int_to_fp_d")
	    (const_string "neon_fp_cvt_int")
	  (eq_attr "type" "neon_fp_to_int_s_q, neon_int_to_fp_s_q,\
			   neon_fp_to_int_d_q, neon_int_to_fp_d_q")
	    (const_string "neon_fp_cvt_int_q")
	  (eq_attr "type" "neon_fp_cvt_narrow_s_q, neon_fp_cvt_narrow_d_q,\
			   neon_fp_cvt_widen_h, neon_fp_cvt_widen_s")
	    (const_string "neon_fp_cvt")
	  (eq_attr "type" "neon_fp_mul_s, neon_fp_mul_s_q,\
			   neon_fp_mul_s_scalar, neon_fp_mul_s_scalar_q,\
			   neon_fp_mul_d, neon_fp_mul_d_q, neon_fp_mul_d_scalar_q")
	    (const_string "neon_fp_mul")
	  (eq_attr "type" "neon_fp_mla_s, neon_fp_mla_s_q,\
			   neon_fp_mla_s_scalar, neon_fp_mla_s_scalar_q,\
			   neon_fp_mla_d, neon_fp_mla_d_q, neon_fp_mla_d_scalar_q")
	    (const_string "neon_fp_mla")
	  (eq_attr "type" "neon_fcadd")
	    (const_string "neon_fp_fcadd")
	  (eq_attr "type" "neon_fcmla")
	    (const_string "neon_fp_fcmla")
	  (eq_attr "type" "neon_fp_div_s")
	    (const_string "neon_fp_div_s")
	  (eq_attr "type" "neon_fp_div_d")
	    (const_string "neon_fp_div_d")
	  (eq_attr "type" "neon_fp_div_s_q")
	    (const_string "neon_fp_div_s_q")
	  (eq_attr "type" "neon_fp_div_d_q")
	    (const_string "neon_fp_div_d_q")
	  (eq_attr "type" "neon_fp_recpe_s, neon_fp_recpe_d,\
			   neon_fp_rsqrte_s, neon_fp_rsqrte_d")
	    (const_string "neon_fp_recpe_rsqrte")
	  (eq_attr "type" "neon_fp_recpe_s_q, neon_fp_recpe_d_q,\
			   neon_fp_rsqrte_s_q, neon_fp_rsqrte_d_q")
	    (const_string "neon_fp_recpe_rsqrte_q")
	  (eq_attr "type" "neon_fp_recpx_s, neon_fp_recpx_d,\
			   neon_fp_recpx_s_q, neon_fp_recpx_d_q")
	    (const_string "neon_fp_recpx")
	  (eq_attr "type" "neon_fp_recps_s, neon_fp_recps_d,\
			   neon_fp_recps_s_q, neon_fp_recps_d_q,\
			   neon_fp_rsqrts_s, neon_fp_rsqrts_d,\
			   neon_fp_rsqrts_s_q, neon_fp_rsqrts_d_q")
	    (const_string "neon_fp_recps_rsqrts")
	  (eq_attr "type" "neon_fp_sqrt_s")
	    (const_string "neon_fp_sqrt_s")
	  (eq_attr "type" "neon_fp_sqrt_d")
	    (const_string "neon_fp_sqrt_d")
	  (eq_attr "type" "neon_fp_sqrt_s_q")
	    (const_string "neon_fp_sqrt_s_q")
	  (eq_attr "type" "neon_fp_sqrt_d_q")
	    (const_string "neon_fp_sqrt_d_q")

	  (eq_attr "type" "neon_bsl, neon_bsl_q, neon_cls, neon_cls_q,\
			   neon_rbit, neon_rbit_q")
	    (const_string "neon_bitops_basic")
	  (eq_attr "type" "neon_cnt, neon_cnt_q, neon_rev, neon_rev_q,\
			   neon_permute, neon_permute_q, neon_zip, neon_zip_q,\
			   neon_tbl1, neon_tbl1_q, neon_tbl2, neon_tbl2_q,\
			   neon_dup, neon_dup_q, neon_ext, neon_ext_q,\
			   neon_move, neon_move_q, neon_move_narrow_q,\
			   neon_ins, neon_ins_q")
	    (const_string "neon_bitops_complex")
	  (eq_attr "type" "neon_tbl3, neon_tbl3_q, neon_tbl4, neon_tbl4_q")
	    (const_string "neon_tbl3_tbl4")
	  (eq_attr "type" "neon_from_gp, neon_from_gp_q, f_mcr, f_mcrr")
	    (const_string "neon_from_gp")
	  (eq_attr "type" "neon_to_gp, neon_to_gp_q, f_mrc, f_mrrc")
	    (const_string "neon_to_gp")

	  (eq_attr "type" "f_loads, f_loadd, neon_ldp, neon_ldp_q,\
			   neon_load1_1reg, neon_load1_1reg_q,\
			   neon_load1_2reg, neon_load1_2reg_q")
	    (const_string "neon_load_a")
	  (eq_attr "type" "neon_load1_3reg, neon_load1_3reg_q,\
			   neon_load1_4reg, neon_load1_4reg_q")
	    (const_string "neon_load_b")
	  (eq_attr "type" "neon_load1_one_lane, neon_load1_one_lane_q,\
			   neon_load1_all_lanes, neon_load1_all_lanes_q,\
			   neon_load2_2reg, neon_load2_2reg_q,\
			   neon_load2_4reg, neon_load2_4reg_q,\
			   neon_load2_one_lane, neon_load2_one_lane_q,\
			   neon_load2_all_lanes, neon_load2_all_lanes_q,\
			   neon_load3_3reg,\
			   neon_load3_one_lane, neon_load3_one_lane_q,\
			   neon_load3_all_lanes, neon_load3_all_lanes_q,\
			   neon_load4_4reg,\
			   neon_load4_one_lane, neon_load4_one_lane_q,\
			   neon_load4_all_lanes, neon_load4_all_lanes_q")
	    (const_string "neon_load_c")
	  (eq_attr "type" "neon_load3_3reg_q, neon_load4_4reg_q")
	    (const_string "neon_load_d")

	  (eq_attr "type" "f_stores, f_stored, neon_stp, neon_stp_q,\
			   neon_store1_1reg_q, neon_store1_2reg_q,\
			   neon_store1_3reg_q, neon_store1_4reg_q,\
			   neon_store1_one_lane, neon_store1_one_lane_q,\
			   neon_store2_2reg, neon_store2_2reg_q,\
			   neon_store2_4reg, neon_store2_4reg_q,\
			   neon_store2_one_lane, neon_store2_one_lane_q")
	    (const_string "neon_store_a")
	  (eq_attr "type" "neon_store1_1reg, neon_store1_2reg, neon_store1_3reg")
	    (const_string "neon_store_b")
	  (eq_attr "type" "neon_store1_4reg,\
			   neon_store3_3reg, neon_store3_3reg_q,\
			   neon_store3_one_lane, neon_store3_one_lane_q,\
			   neon_store4_4reg, neon_store4_4reg_q,\
			   neon_store4_one_lane, neon_store4_one_lane_q")
	    (const_string "neon_store_c")

;; The crypto types below are also covered by the is_neon_type check.
	  (eq_attr "type" "crypto_aese, crypto_aesmc")
	    (const_string "crypto_aes")
	  (eq_attr "type" "crypto_sha1_fast")
	    (const_string "crypto_sha1_fast")
	  (eq_attr "type" "crypto_sha256_fast, crypto_sha1_xor")
	    (const_string "crypto_sha_fast")
	  (eq_attr "type" "crypto_sha1_slow, crypto_sha256_slow")
	    (const_string "crypto_sha_slow")

;; If it doesn't match any of the above that we want to treat specially but is
;; still a NEON type, treat it as a basic NEON type.  This is better than
;; dropping it on the floor and making no assumptions about it whatsoever.
	  (eq_attr "is_neon_type" "yes")
	    (const_string "neon_arith_basic")

;; Treat all SVE instructions as basic NEON instructions to avoid
;; lots of "nothing" in the DFA.
	  (not (eq_attr "sve_type" "none"))
	    (const_string "neon_arith_basic")]
	  (const_string "unknown")))

;; The hip12 core is modelled as issues pipeline that has
;; the following functional units.
;; 1. Four pipelines for single cycle integer operations: ALU0, ALU1, ALU3, ALU4

(define_cpu_unit "hip12_alu0" "hip12")
(define_cpu_unit "hip12_alu1" "hip12")
(define_cpu_unit "hip12_alu3" "hip12")
(define_cpu_unit "hip12_alu4" "hip12")

(define_reservation "hip12_alu0134" "hip12_alu0|hip12_alu1|hip12_alu3|hip12_alu4")
(define_reservation "hip12_alu14" "hip12_alu1|hip12_alu4")

;; 2. Two pipelines for multi cycles integer operations: ALU2, ALU5

(define_cpu_unit "hip12_alu2" "hip12")
(define_cpu_unit "hip12_alu5" "hip12")

(define_reservation "hip12_alu25" "hip12_alu2|hip12_alu5")
(define_reservation "hip12_alu1425" "hip12_alu1|hip12_alu4|hip12_alu2|hip12_alu5")

(define_reservation "hip12_alu" "hip12_alu0|hip12_alu1|hip12_alu2|hip12_alu3|hip12_alu4|hip12_alu5")

;; 3. Three pipelines for load operations: Load0, Load1, Load2

(define_cpu_unit "hip12_load0" "hip12_ldst")
(define_cpu_unit "hip12_load1" "hip12_ldst")
(define_cpu_unit "hip12_load2" "hip12_ldst")

(define_reservation "hip12_ld" "hip12_load0|hip12_load1|hip12_load2")

;; 4. Two pipelines for store operations: Store1, Store2

(define_cpu_unit "hip12_store0" "hip12_ldst")
(define_cpu_unit "hip12_store1" "hip12_ldst")

(define_reservation "hip12_st" "hip12_store0|hip12_store1")

;; 5. Four asymmetric pipelines for ASIMD/FP/SVE operations: V0, V1, V2, V3

(define_cpu_unit "hip12_v0" "hip12_v")
(define_cpu_unit "hip12_v1" "hip12_v")
(define_cpu_unit "hip12_v2" "hip12_v")
(define_cpu_unit "hip12_v3" "hip12_v")

(define_reservation "hip12_v0123" "hip12_v0|hip12_v1|hip12_v2|hip12_v3")
(define_reservation "hip12_v02" "hip12_v0|hip12_v2")

;; 6. Two pipelines for branch operations: Branch0, Branch1

(define_cpu_unit "hip12_b0" "hip12")
(define_cpu_unit "hip12_b1" "hip12")

(define_reservation "hip12_b" "hip12_b0|hip12_b1")

;; Block all issue queues.

(define_reservation "hip12_block" "
	hip12_alu0+hip12_alu1+hip12_alu2+hip12_alu3
	+hip12_alu4+hip12_alu5
	+hip12_load0+hip12_load1+hip12_load2
	+hip12_store0+hip12_store1
	+hip12_v0+hip12_v1+hip12_v2+hip12_v3")

;; Branch instructions.

(define_insn_reservation "hip12_branch" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "branch"))
  "hip12_b")

(define_insn_reservation "hip12_branch_and_link" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "call"))
  "hip12_b+hip12_alu14")

;; Integer Arithmetic and Logical instructions.

(define_insn_reservation "hip12_alu" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "alu_imm,alu_sreg,adc_imm,adc_reg,\
			logic_imm,logic_reg,mov_imm,mvn_imm,\
			mov_reg,mvn_reg,extend"))
  "hip12_alu")

(define_insn_reservation "hip12_alus" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "alus_imm,alus_sreg,adcs_imm,adcs_reg,\
			logics_imm,logics_reg"))
  "hip12_alu1425")

(define_insn_reservation "hip12_alu_shift" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "logics_shift_imm,logics_shift_reg,alu_ext,alus_ext,\
			alu_shift_imm_lsl_1to4,alu_shift_imm_other,\
			alu_shift_reg,alus_shift_imm,alus_shift_reg,\
			mvn_shift,mvn_shift_reg,crc"))
  "hip12_alu25")

(define_insn_reservation "hip12_shift" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "logic_shift_imm,logic_shift_reg,shift_imm,shift_reg,\
			mov_shift,mov_shift_reg,rotate_imm"))
  "hip12_alu14")

(define_insn_reservation "hip12_alu_other" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "adr,bfm,bfx,csel,clz,rbit,rev,alu_dsp_reg,mrs,multiple"))
  "hip12_alu14")

;; Divide and Multiply instructions.

(define_insn_reservation "hip12_div" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "sdiv,udiv"))
  "hip12_alu25*2")

(define_insn_reservation "hip12_mul" 3
  (and (eq_attr "tune" "hip12")
       (ior (eq_attr "mul32" "yes")
	    (eq_attr "widen_mul64" "yes")))
  "hip12_alu25")

;; Block all issue pipes for a cycle.

(define_insn_reservation "hip12_block" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "block"))
  "hip12_block")

;; Load and Store instructions.

(define_insn_reservation "hip12_load1" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "load_byte,load_4,load_8"))
  "hip12_ld")

(define_insn_reservation "hip12_load3" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "load_12,load_16"))
  "hip12_ld*2")

(define_insn_reservation "hip12_store1" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "store_4,store_8"))
  "hip12_st")

(define_insn_reservation "hip12_store3" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "store_12,store_16"))
  "hip12_st*2")

;; Advanced SIMD Unit.
;;
;; Integer Arithmetic Instructions.

(define_insn_reservation "hip12_neon_arith_basic" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_arith_basic"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_arith_complex" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_arith_complex"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_aba" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_aba"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_addv" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_addv"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_dot" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_dot"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_minmaxv" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_minmaxv"))
  "hip12_v0123")

;; Integer Multiply Instructions.

(define_insn_reservation "hip12_neon_multiply" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_multiply"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_mla" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_mla"))
  "hip12_v0123")

;; Integer Shift Instructions.

(define_insn_reservation "hip12_neon_shift_acc" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_shift_acc"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_shift_imm_basic" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_shift_imm_basic"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_shift_imm_complex" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_shift_imm_complex"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_shift_reg_basic" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_shift_reg_basic"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_shift_reg_complex" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_shift_reg_complex"))
  "hip12_v0123")

;; Floating Point Instructions.

(define_insn_reservation "hip12_neon_fp_negabs" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_negabs"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_arith" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_arith"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_reductions" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_reductions"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_round_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_round_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_cvt_int" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_cvt_int"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_cvt_int_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_cvt_int_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_cvt" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_cvt"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_mul" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_mul"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_mla" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_mla"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_fcadd" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_fcadd"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_fcmla" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_fcmla"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_div_s" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_div_s"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_div_d" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_div_d"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_div_s_q" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_div_s_q"))
  "hip12_v0123*2")

(define_insn_reservation "hip12_neon_fp_div_d_q" 9
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_div_d_q"))
  "hip12_v0123*2")

(define_insn_reservation "hip12_neon_fp_recpe_rsqrte" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_recpe_rsqrte"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_recpe_rsqrte_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_recpe_rsqrte_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_recpx" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_recpx"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_recps_rsqrts" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_recps_rsqrts"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_sqrt_s" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_sqrt_s"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_sqrt_d" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_sqrt_d"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_sqrt_s_q" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_sqrt_s_q"))
  "hip12_v0123*2")

(define_insn_reservation "hip12_neon_fp_sqrt_d_q" 9
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_fp_sqrt_d_q"))
  "hip12_v0123*2")

;; Miscellaneous Instructions.

(define_insn_reservation "hip12_neon_bitops_basic" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_bitops_basic"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_bitops_complex" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_bitops_complex"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_tbl3_tbl4" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_tbl3_tbl4"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_from_gp" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_from_gp"))
  "hip12_alu0134+hip12_v0123")

(define_insn_reservation "hip12_neon_to_gp" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_to_gp"))
  "hip12_v0123")

;; Load Instructions.

(define_insn_reservation "hip12_neon_load_a" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_load_a"))
  "hip12_ld")

(define_insn_reservation "hip12_neon_load_b" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_load_b"))
  "hip12_ld")

(define_insn_reservation "hip12_neon_load_c" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_load_c"))
  "hip12_ld+hip12_v0123")

(define_insn_reservation "hip12_neon_load_d" 9
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_load_d"))
  "hip12_ld+hip12_v0123")

;; Store Instructions.

(define_insn_reservation "hip12_neon_store_a" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_store_a"))
  "hip12_st")

(define_insn_reservation "hip12_neon_store_b" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_store_b"))
  "hip12_st+hip12_v0123")

(define_insn_reservation "hip12_neon_store_c" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "neon_store_c"))
  "hip12_st+hip12_v0123")

;; Floating-Point Operations.

(define_insn_reservation "hip12_fp_mov" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fconsts,fconstd,fmov"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_add_sub" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fadds,faddd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_mul" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fmuls,fmuld"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_mac" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fmacs,ffmas,fmacd,ffmad"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_minmax" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_minmaxs,f_minmaxd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_rint" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_rints,f_rintd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_cvt" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_cvt"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_cvtf2i" 5
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_cvtf2i"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_cvti2f" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_cvti2f"))
  "hip12_alu14+hip12_v0123")

(define_insn_reservation "hip12_fp_cmp" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fcmps,fcmpd"))
  "hip12_alu0134+hip12_v02")

(define_insn_reservation "hip12_fp_ccmp" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fccmps,fccmpd"))
  "hip12_alu14+hip12_v0123")

(define_insn_reservation "hip12_fp_arith" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "ffariths,ffarithd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_csel" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fcsel"))
  "hip12_alu14+hip12_v0123")

(define_insn_reservation "hip12_fp_divs_sqrts" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fdivs,fsqrts"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_divd_sqrtd" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fdivd,fsqrtd"))
  "hip12_v0123")

;; Cryptographic extensions.

(define_insn_reservation "hip12_crypto_aes" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "crypto_aes"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_sha1_fast" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "crypto_sha1_fast"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_sha_fast" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "crypto_sha_fast"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_sha_slow" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "hip12_neon_type" "crypto_sha_slow"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_sha3" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha3"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_pmull" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_pmull"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_sha512" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha512"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_sm3" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sm3"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_sm4" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sm4"))
  "hip12_v0123")

;; Bypasses.

(define_bypass 2 "hip12_fp_mac"
		 "hip12_fp_mac")

(define_bypass 1 "hip12_fp_mul"
		 "hip12_fp_mac")

(define_bypass 1 "hip12_neon_aba"
		 "hip12_neon_aba")

(define_bypass 1 "hip12_neon_dot"
		 "hip12_neon_dot")

(define_bypass 1 "hip12_neon_mla"
		 "hip12_neon_mla")

(define_bypass 2 "hip12_neon_shift_acc"
		 "hip12_neon_shift_acc")

(define_bypass 2 "hip12_neon_fp_fcmla"
		 "hip12_neon_fp_fcmla")

(define_bypass 1 "hip12_neon_fp_mul"
		 "hip12_neon_fp_mla")

(define_bypass 2 "hip12_neon_fp_mla"
		 "hip12_neon_fp_mla")

(define_bypass 2 "hip12_neon_fp_recps_rsqrts"
		 "hip12_neon_fp_recps_rsqrts")
