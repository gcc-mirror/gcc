;; ARM Cortex-A57 pipeline description
;; Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

(define_automaton "cortex_a57")

(define_attr "cortex_a57_neon_type"
  "neon_abd, neon_abd_q, neon_arith_acc, neon_arith_acc_q,
   neon_arith_basic, neon_arith_complex,
   neon_reduc_add_acc, neon_multiply, neon_multiply_q,
   neon_multiply_long, neon_mla, neon_mla_q, neon_mla_long,
   neon_sat_mla_long, neon_shift_acc, neon_shift_imm_basic,
   neon_shift_imm_complex,
   neon_shift_reg_basic, neon_shift_reg_basic_q, neon_shift_reg_complex,
   neon_shift_reg_complex_q, neon_fp_negabs, neon_fp_arith,
   neon_fp_arith_q, neon_fp_reductions_q, neon_fp_cvt_int,
   neon_fp_cvt_int_q, neon_fp_cvt16, neon_fp_minmax, neon_fp_mul,
   neon_fp_mul_q, neon_fp_mla, neon_fp_mla_q, neon_fp_recpe_rsqrte,
   neon_fp_recpe_rsqrte_q, neon_fp_recps_rsqrts, neon_fp_recps_rsqrts_q,
   neon_bitops, neon_bitops_q, neon_from_gp,
   neon_from_gp_q, neon_move, neon_tbl3_tbl4, neon_zip_q, neon_to_gp,
   neon_load_a, neon_load_b, neon_load_c, neon_load_d, neon_load_e,
   neon_load_f, neon_store_a, neon_store_b, neon_store_complex,
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
			   neon_mul_s_long, neon_mul_d_long,\
			   neon_mul_h_scalar_long, neon_mul_s_scalar_long,\
			   neon_sat_mul_b_long, neon_sat_mul_h_long,\
			   neon_sat_mul_s_long, neon_sat_mul_h_scalar_long,\
			   neon_sat_mul_s_scalar_long, crypto_pmull")
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
			   neon_fp_abs_s, neon_fp_abs_s_q,\
			   neon_fp_neg_d, neon_fp_neg_d_q,\
			   neon_fp_abs_d, neon_fp_abs_d_q")
	    (const_string "neon_fp_negabs")
	  (eq_attr "type" "neon_fp_addsub_s, neon_fp_abd_s,\
			   neon_fp_reduc_add_s, neon_fp_compare_s,\
			   neon_fp_minmax_s, neon_fp_round_s,\
			   neon_fp_addsub_d, neon_fp_abd_d,\
			   neon_fp_reduc_add_d, neon_fp_compare_d,\
			   neon_fp_minmax_d, neon_fp_round_d,\
			   neon_fp_reduc_minmax_s, neon_fp_reduc_minmax_d")
	    (const_string "neon_fp_arith")
	  (eq_attr "type" "neon_fp_addsub_s_q, neon_fp_abd_s_q,\
			   neon_fp_reduc_add_s_q, neon_fp_compare_s_q,\
			   neon_fp_minmax_s_q, neon_fp_round_s_q,\
			   neon_fp_addsub_d_q, neon_fp_abd_d_q,\
			   neon_fp_reduc_add_d_q, neon_fp_compare_d_q,\
			   neon_fp_minmax_d_q, neon_fp_round_d_q")
	    (const_string "neon_fp_arith_q")
	  (eq_attr "type" "neon_fp_reduc_minmax_s_q,\
			   neon_fp_reduc_minmax_d_q,\
			   neon_fp_reduc_add_s_q, neon_fp_reduc_add_d_q")
	    (const_string "neon_fp_reductions_q")
	  (eq_attr "type" "neon_fp_to_int_s, neon_int_to_fp_s,\
			   neon_fp_to_int_d, neon_int_to_fp_d")
	    (const_string "neon_fp_cvt_int")
	  (eq_attr "type" "neon_fp_to_int_s_q, neon_int_to_fp_s_q,\
			   neon_fp_to_int_d_q, neon_int_to_fp_d_q")
	    (const_string "neon_fp_cvt_int_q")
	  (eq_attr "type" "neon_fp_cvt_narrow_s_q, neon_fp_cvt_widen_h")
	    (const_string "neon_fp_cvt16")
	  (eq_attr "type" "neon_fp_mul_s, neon_fp_mul_s_scalar,\
			   neon_fp_mul_d")
	    (const_string "neon_fp_mul")
	  (eq_attr "type" "neon_fp_mul_s_q, neon_fp_mul_s_scalar_q,\
			   neon_fp_mul_d_q, neon_fp_mul_d_scalar_q")
	    (const_string "neon_fp_mul_q")
	  (eq_attr "type" "neon_fp_mla_s, neon_fp_mla_s_scalar,\
			   neon_fp_mla_d")
	    (const_string "neon_fp_mla")
	  (eq_attr "type" "neon_fp_mla_s_q, neon_fp_mla_s_scalar_q,
			   neon_fp_mla_d_q, neon_fp_mla_d_scalar_q")
	    (const_string "neon_fp_mla_q")
	  (eq_attr "type" "neon_fp_recpe_s, neon_fp_rsqrte_s,\
			   neon_fp_recpx_s,\
			   neon_fp_recpe_d, neon_fp_rsqrte_d,\
			   neon_fp_recpx_d")
	    (const_string "neon_fp_recpe_rsqrte")
	  (eq_attr "type" "neon_fp_recpe_s_q, neon_fp_rsqrte_s_q,\
			   neon_fp_recpx_s_q,\
			   neon_fp_recpe_d_q, neon_fp_rsqrte_d_q,\
			   neon_fp_recpx_d_q")
	    (const_string "neon_fp_recpe_rsqrte_q")
	  (eq_attr "type" "neon_fp_recps_s, neon_fp_rsqrts_s,\
			   neon_fp_recps_d, neon_fp_rsqrts_d")
	    (const_string "neon_fp_recps_rsqrts")
	  (eq_attr "type" "neon_fp_recps_s_q, neon_fp_rsqrts_s_q,\
			   neon_fp_recps_d_q, neon_fp_rsqrts_d_q")
	    (const_string "neon_fp_recps_rsqrts_q")
	  (eq_attr "type" "neon_bsl, neon_cls, neon_cnt,\
			   neon_rev, neon_permute, neon_rbit,\
			   neon_tbl1, neon_tbl2, neon_zip,\
			   neon_dup, neon_dup_q, neon_ext, neon_ext_q,\
			   neon_move, neon_move_q, neon_move_narrow_q")
	    (const_string "neon_bitops")
	  (eq_attr "type" "neon_bsl_q, neon_cls_q, neon_cnt_q,\
			   neon_rev_q, neon_permute_q, neon_rbit_q")
	    (const_string "neon_bitops_q")
	  (eq_attr "type" "neon_from_gp,f_mcr,f_mcrr")
	    (const_string "neon_from_gp")
	  (eq_attr "type" "neon_from_gp_q")
	    (const_string "neon_from_gp_q")
	  (eq_attr "type" "neon_tbl3, neon_tbl4")
	    (const_string "neon_tbl3_tbl4")
	  (eq_attr "type" "neon_zip_q")
	    (const_string "neon_zip_q")
	  (eq_attr "type" "neon_to_gp, neon_to_gp_q,f_mrc,f_mrrc")
	    (const_string "neon_to_gp")

	  (eq_attr "type" "f_loads, f_loadd,\
			   neon_load1_1reg, neon_load1_1reg_q,\
			   neon_load1_2reg, neon_load1_2reg_q")
	    (const_string "neon_load_a")
	  (eq_attr "type" "neon_load1_3reg, neon_load1_3reg_q,\
			   neon_load1_4reg, neon_load1_4reg_q")
	    (const_string "neon_load_b")
	  (eq_attr "type" "neon_ldp, neon_ldp_q,\
			   neon_load1_one_lane, neon_load1_one_lane_q,\
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
			   neon_store1_1reg")
	    (const_string "neon_store_a")
	  (eq_attr "type" "neon_store1_2reg, neon_store1_1reg_q")
	    (const_string "neon_store_b")
	  (eq_attr "type" "neon_stp, neon_stp_q,\
			   neon_store1_3reg, neon_store1_3reg_q,\
			   neon_store3_3reg, neon_store3_3reg_q,\
			   neon_store2_4reg, neon_store2_4reg_q,\
			   neon_store4_4reg, neon_store4_4reg_q,\
			   neon_store2_2reg, neon_store2_2reg_q,\
			   neon_store3_one_lane, neon_store3_one_lane_q,\
			   neon_store4_one_lane, neon_store4_one_lane_q,\
			   neon_store1_4reg, neon_store1_4reg_q,\
			   neon_store1_one_lane, neon_store1_one_lane_q,\
			   neon_store2_one_lane, neon_store2_one_lane_q")
	    (const_string "neon_store_complex")
;; If it doesn't match any of the above that we want to treat specially but is
;; still a NEON type, treat it as a basic NEON type.  This is better than
;; dropping it on the floor and making no assumptions about it whatsoever.
	  (eq_attr "is_neon_type" "yes")
	    (const_string "neon_arith_basic")]
	  (const_string "unknown")))

;; The Cortex-A57 core is modelled as a triple issue pipeline that has
;; the following functional units.
;; 1.  Two pipelines for integer operations: SX1, SX2

(define_cpu_unit "ca57_sx1_issue" "cortex_a57")
(define_reservation "ca57_sx1" "ca57_sx1_issue")

(define_cpu_unit "ca57_sx2_issue" "cortex_a57")
(define_reservation "ca57_sx2" "ca57_sx2_issue")

;; 2.  One pipeline for complex integer operations: MX

(define_cpu_unit "ca57_mx_issue"
		 "cortex_a57")
(define_reservation "ca57_mx" "ca57_mx_issue")
(define_reservation "ca57_mx_block" "ca57_mx_issue")

;; 3.  Two asymmetric pipelines for Neon and FP operations: CX1, CX2
(define_automaton "cortex_a57_cx")

(define_cpu_unit "ca57_cx1_issue"
		 "cortex_a57_cx")
(define_cpu_unit "ca57_cx2_issue"
		 "cortex_a57_cx")

(define_reservation "ca57_cx1" "ca57_cx1_issue")

(define_reservation "ca57_cx2" "ca57_cx2_issue")
(define_reservation "ca57_cx2_block" "ca57_cx2_issue*2")

;; 4.  One pipeline for branch operations: BX

(define_cpu_unit "ca57_bx_issue" "cortex_a57")
(define_reservation "ca57_bx" "ca57_bx_issue")

;; 5.  Two pipelines for load and store operations: LS1, LS2.  The most
;;     valuable thing we can do is force a structural hazard to split
;;     up loads/stores.

(define_cpu_unit "ca57_ls_issue" "cortex_a57")
(define_cpu_unit "ca57_ldr, ca57_str" "cortex_a57")
(define_reservation "ca57_load_model" "ca57_ls_issue,ca57_ldr*2")
(define_reservation "ca57_store_model" "ca57_ls_issue,ca57_str")

;; Block all issue queues.

(define_reservation "ca57_block" "ca57_cx1_issue + ca57_cx2_issue
				  + ca57_mx_issue + ca57_sx1_issue
				  + ca57_sx2_issue + ca57_ls_issue")

;; Simple Execution Unit:
;;
;; Simple ALU without shift
(define_insn_reservation "cortex_a57_alu" 2
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,\
			alu_sreg,alus_sreg,logic_reg,logics_reg,\
			adc_imm,adcs_imm,adc_reg,adcs_reg,\
			adr,bfx,extend,clz,rbit,rev,alu_dsp_reg,\
			rotate_imm,shift_imm,shift_reg,\
			mov_imm,mov_reg,\
			mvn_imm,mvn_reg,\
			mrs,multiple"))
  "ca57_sx1|ca57_sx2")

;; ALU ops with immediate shift
(define_insn_reservation "cortex_a57_alu_shift" 3
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "bfm,\
			alu_shift_imm_lsl_1to4,alu_shift_imm_other,alus_shift_imm,\
			crc,logic_shift_imm,logics_shift_imm,\
			mov_shift,mvn_shift"))
  "ca57_mx")

;; Multi-Cycle Execution Unit:
;;
;; ALU ops with register controlled shift
(define_insn_reservation "cortex_a57_alu_shift_reg" 3
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "alu_shift_reg,alus_shift_reg,\
			logic_shift_reg,logics_shift_reg,\
			mov_shift_reg,mvn_shift_reg"))
   "ca57_mx")

;; All multiplies
;; TODO: AArch32 and AArch64 have different behavior
(define_insn_reservation "cortex_a57_mult32" 3
  (and (eq_attr "tune" "cortexa57")
       (ior (eq_attr "mul32" "yes")
	    (eq_attr "widen_mul64" "yes")))
  "ca57_mx")

;; Integer divide
(define_insn_reservation "cortex_a57_div" 10
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "udiv,sdiv"))
  "ca57_mx_issue,ca57_mx_block*3")

;; Block all issue pipes for a cycle
(define_insn_reservation "cortex_a57_block" 1
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "block"))
  "ca57_block")

;; Branch execution Unit
;;
;; Branches take one issue slot.
;; No latency as there is no result
(define_insn_reservation "cortex_a57_branch" 0
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "branch"))
  "ca57_bx")

;; Load-store execution Unit
;;
;; Loads of up to two words.
(define_insn_reservation "cortex_a57_load1" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "load_byte,load_4,load_8"))
  "ca57_load_model")

;; Loads of three or four words.
(define_insn_reservation "cortex_a57_load3" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "load_12,load_16"))
  "ca57_ls_issue*2,ca57_load_model")

;; Stores of up to two words.
(define_insn_reservation "cortex_a57_store1" 0
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "store_4,store_8"))
  "ca57_store_model")

;; Stores of three or four words.
(define_insn_reservation "cortex_a57_store3" 0
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "store_12,store_16"))
  "ca57_ls_issue*2,ca57_store_model")

;; Advanced SIMD Unit - Integer Arithmetic Instructions.

(define_insn_reservation  "cortex_a57_neon_abd" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_abd"))
  "ca57_cx1|ca57_cx2")

(define_insn_reservation  "cortex_a57_neon_abd_q" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_abd_q"))
  "ca57_cx1+ca57_cx2")

(define_insn_reservation  "cortex_a57_neon_aba" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_arith_acc"))
  "ca57_cx2")

(define_insn_reservation  "cortex_a57_neon_aba_q" 8
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_arith_acc_q"))
  "ca57_cx2+(ca57_cx2_issue,ca57_cx2)")

(define_insn_reservation  "cortex_a57_neon_arith_basic" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_arith_basic"))
  "ca57_cx1|ca57_cx2")

(define_insn_reservation  "cortex_a57_neon_arith_complex" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_arith_complex"))
  "ca57_cx1|ca57_cx2")

;; Integer Multiply Instructions.

(define_insn_reservation "cortex_a57_neon_multiply" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_multiply"))
  "ca57_cx1")

(define_insn_reservation "cortex_a57_neon_multiply_q" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_multiply_q"))
  "ca57_cx1+(ca57_cx1_issue,ca57_cx1)")

(define_insn_reservation "cortex_a57_neon_mla" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_mla"))
  "ca57_cx1")

(define_insn_reservation "cortex_a57_neon_mla_q" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_mla_q"))
  "ca57_cx1+(ca57_cx1_issue,ca57_cx1)")

(define_insn_reservation "cortex_a57_neon_sat_mla_long" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_sat_mla_long"))
  "ca57_cx1")

;; Integer Shift Instructions.

(define_insn_reservation
  "cortex_a57_neon_shift_acc" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_shift_acc"))
  "ca57_cx2")

(define_insn_reservation
  "cortex_a57_neon_shift_imm_basic" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_shift_imm_basic"))
  "ca57_cx2")

(define_insn_reservation
  "cortex_a57_neon_shift_imm_complex" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_shift_imm_complex"))
  "ca57_cx2")

(define_insn_reservation
  "cortex_a57_neon_shift_reg_basic" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_shift_reg_basic"))
  "ca57_cx2")

(define_insn_reservation
  "cortex_a57_neon_shift_reg_basic_q" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_shift_reg_basic_q"))
  "ca57_cx2+(ca57_cx2_issue,ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_shift_reg_complex" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_shift_reg_complex"))
  "ca57_cx2")

(define_insn_reservation
  "cortex_a57_neon_shift_reg_complex_q" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_shift_reg_complex_q"))
  "ca57_cx2+(ca57_cx2_issue,ca57_cx2)")

;; Floating Point Instructions.

(define_insn_reservation
  "cortex_a57_neon_fp_negabs" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_negabs"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_arith" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_arith"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_arith_q" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_arith_q"))
  "(ca57_cx1+ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_reductions_q" 10
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_reductions_q"))
  "(ca57_cx1+ca57_cx2),(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_cvt_int" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_cvt_int"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_cvt_int_q" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_cvt_int_q"))
  "(ca57_cx1+ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_cvt16" 10
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_cvt16"))
  "(ca57_cx1_issue+ca57_cx2_issue),(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_mul" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_mul"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_mul_q" 5
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_mul_q"))
  "(ca57_cx1+ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_mla" 9
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_mla"))
  "(ca57_cx1,ca57_cx1)|(ca57_cx2,ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_mla_q" 9
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_mla_q"))
  "(ca57_cx1+ca57_cx2),(ca57_cx1,ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_recpe_rsqrte" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_recpe_rsqrte"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_recpe_rsqrte_q" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_recpe_rsqrte_q"))
  "(ca57_cx1+ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_recps_rsqrts" 10
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_recps_rsqrts"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_fp_recps_rsqrts_q" 10
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_fp_recps_rsqrts_q"))
  "(ca57_cx1+ca57_cx2)")

;; Miscellaneous Instructions.

(define_insn_reservation
  "cortex_a57_neon_bitops" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_bitops"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_bitops_q" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_bitops_q"))
  "(ca57_cx1+ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_from_gp" 9
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_from_gp"))
  "(ca57_ls_issue+ca57_cx1_issue,ca57_cx1)
	       |(ca57_ls_issue+ca57_cx2_issue,ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_from_gp_q" 9
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_from_gp_q"))
  "(ca57_ls_issue+ca57_cx1_issue,ca57_cx1)
	       +(ca57_ls_issue+ca57_cx2_issue,ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_tbl3_tbl4" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_tbl3_tbl4"))
  "(ca57_cx1_issue,ca57_cx1)
	       +(ca57_cx2_issue,ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_zip_q" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_zip_q"))
  "(ca57_cx1_issue,ca57_cx1)
	       +(ca57_cx2_issue,ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_to_gp" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_to_gp"))
  "((ca57_ls_issue+ca57_sx1_issue),ca57_sx1)
   |((ca57_ls_issue+ca57_sx2_issue),ca57_sx2)")

;; Load Instructions.

(define_insn_reservation
  "cortex_a57_neon_load_a" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_load_a"))
  "ca57_load_model")

(define_insn_reservation
  "cortex_a57_neon_load_b" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_load_b"))
  "ca57_ls_issue,ca57_ls_issue+ca57_ldr,ca57_ldr*2")

(define_insn_reservation
  "cortex_a57_neon_load_c" 9
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_load_c"))
  "ca57_load_model+(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_load_d" 11
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_load_d"))
  "ca57_cx1_issue+ca57_cx2_issue,
   ca57_ls_issue+ca57_ls_issue,ca57_ldr*2")

(define_insn_reservation
  "cortex_a57_neon_load_e" 9
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_load_e"))
  "ca57_load_model+(ca57_cx1|ca57_cx2)")

(define_insn_reservation
  "cortex_a57_neon_load_f" 11
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_load_f"))
  "ca57_cx1_issue+ca57_cx2_issue,
   ca57_ls_issue+ca57_ls_issue,ca57_ldr*2")

;; Store Instructions.

(define_insn_reservation
  "cortex_a57_neon_store_a" 0
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_store_a"))
  "ca57_store_model")

(define_insn_reservation
  "cortex_a57_neon_store_b" 0
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_store_b"))
  "ca57_store_model")

;; These block issue for a number of cycles proportional to the number
;; of 64-bit chunks they will store, we don't attempt to model that
;; precisely, treat them as blocking execution for two cycles when
;; issued.
(define_insn_reservation
  "cortex_a57_neon_store_complex" 0
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "cortex_a57_neon_type" "neon_store_complex"))
  "ca57_block*2")

;; Floating-Point Operations.

(define_insn_reservation "cortex_a57_fp_const" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fconsts,fconstd"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation "cortex_a57_fp_add_sub" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fadds,faddd"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation "cortex_a57_fp_mul" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fmuls,fmuld"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation "cortex_a57_fp_mac" 10
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fmacs,ffmas,fmacd,ffmad"))
  "(ca57_cx1,nothing,nothing,ca57_cx1) \
   |(ca57_cx2,nothing,nothing,ca57_cx2)")

(define_insn_reservation "cortex_a57_fp_cvt" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "f_cvt,f_cvtf2i,f_cvti2f"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation "cortex_a57_fp_cmp" 7
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fcmps,fcmpd,fccmps,fccmpd"))
  "ca57_cx2")

(define_insn_reservation "cortex_a57_fp_arith" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "ffariths,ffarithd"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation "cortex_a57_fp_cpys" 4
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fmov,fcsel"))
  "(ca57_cx1|ca57_cx2)")

(define_insn_reservation "cortex_a57_fp_divs" 12
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fdivs, fsqrts,\
			neon_fp_div_s, neon_fp_sqrt_s"))
  "ca57_cx2_block*5")

(define_insn_reservation "cortex_a57_fp_divd" 16
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fdivd, fsqrtd, neon_fp_div_d, neon_fp_sqrt_d"))
  "ca57_cx2_block*3")

(define_insn_reservation "cortex_a57_neon_fp_div_q" 20
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "fdivd, fsqrtd,\
			 neon_fp_div_s_q, neon_fp_div_d_q,\
			 neon_fp_sqrt_s_q, neon_fp_sqrt_d_q"))
  "ca57_cx2_block*3")

(define_insn_reservation "cortex_a57_crypto_simple" 3
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "crypto_aese,crypto_aesmc,crypto_sha1_fast,crypto_sha256_fast"))
  "ca57_cx1")

(define_insn_reservation "cortex_a57_crypto_complex" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "crypto_sha1_slow,crypto_sha256_slow"))
  "ca57_cx1*2")

(define_insn_reservation "cortex_a57_crypto_xor" 6
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "crypto_sha1_xor"))
  "(ca57_cx1*2)|(ca57_cx2*2)")

;; We lie with calls.  They take up all issue slots, but are otherwise
;; not harmful.
(define_insn_reservation "cortex_a57_call" 1
  (and (eq_attr "tune" "cortexa57")
       (eq_attr "type" "call"))
  "ca57_sx1_issue+ca57_sx2_issue+ca57_cx1_issue+ca57_cx2_issue\
    +ca57_mx_issue+ca57_bx_issue+ca57_ls_issue"
)

;; Simple execution unit bypasses
(define_bypass 1 "cortex_a57_alu"
	         "cortex_a57_alu,cortex_a57_alu_shift,cortex_a57_alu_shift_reg")
(define_bypass 2 "cortex_a57_alu_shift"
	         "cortex_a57_alu,cortex_a57_alu_shift,cortex_a57_alu_shift_reg")
(define_bypass 2 "cortex_a57_alu_shift_reg"
	         "cortex_a57_alu,cortex_a57_alu_shift,cortex_a57_alu_shift_reg")
(define_bypass 1 "cortex_a57_alu" "cortex_a57_load1,cortex_a57_load3")
(define_bypass 2 "cortex_a57_alu_shift" "cortex_a57_load1,cortex_a57_load3")
(define_bypass 2 "cortex_a57_alu_shift_reg"
	         "cortex_a57_load1,cortex_a57_load3")

;; An MLA or a MUL can feed a dependent MLA.
(define_bypass 5 "cortex_a57_neon_*mla*,cortex_a57_neon_*mul*"
		 "cortex_a57_neon_*mla*")

(define_bypass 5 "cortex_a57_fp_mul,cortex_a57_fp_mac"
		 "cortex_a57_fp_mac")

;; We don't need to care about control hazards, either the branch is
;; predicted in which case we pay no penalty, or the branch is
;; mispredicted in which case instruction scheduling will be unlikely to
;; help.
(define_bypass 1 "cortex_a57_*"
		 "cortex_a57_call,cortex_a57_branch")
