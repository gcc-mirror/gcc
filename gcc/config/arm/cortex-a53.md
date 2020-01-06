;; ARM Cortex-A53 pipeline description
;; Copyright (C) 2013-2020 Free Software Foundation, Inc.
;;
;; Contributed by ARM Ltd.
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

(define_automaton "cortex_a53")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General-purpose functional units.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We use slot0 and slot1 to model constraints on which instructions may
;; dual-issue.

(define_cpu_unit "cortex_a53_slot0" "cortex_a53")
(define_cpu_unit "cortex_a53_slot1" "cortex_a53")
(final_presence_set "cortex_a53_slot1" "cortex_a53_slot0")

(define_reservation "cortex_a53_slot_any"
		    "cortex_a53_slot0\
		     |cortex_a53_slot1")

(define_reservation "cortex_a53_single_issue"
		    "cortex_a53_slot0\
		     +cortex_a53_slot1")

;; Used to model load and store pipelines.  Load/store instructions
;; can dual-issue with other instructions, but two load/stores cannot
;; simultaneously issue.

(define_cpu_unit "cortex_a53_store" "cortex_a53")
(define_cpu_unit "cortex_a53_load" "cortex_a53")
(define_cpu_unit "cortex_a53_ls_agen" "cortex_a53")

;; Used to model a branch pipeline.  Branches can dual-issue with other
;; instructions (except when those instructions take multiple cycles
;; to issue).

(define_cpu_unit "cortex_a53_branch" "cortex_a53")

;; Used to model an integer divide pipeline.

(define_cpu_unit "cortex_a53_idiv" "cortex_a53")

;; Used to model an integer multiply/multiply-accumulate pipeline.

(define_cpu_unit "cortex_a53_imul" "cortex_a53")

;; Model general structural hazards, for wherever we need them.

(define_cpu_unit "cortex_a53_hazard" "cortex_a53")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALU instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_shift" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "adr,shift_imm,mov_imm,mvn_imm,mov_shift"))
  "cortex_a53_slot_any")

(define_insn_reservation "cortex_a53_shift_reg" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "shift_reg,mov_shift_reg"))
  "cortex_a53_slot_any+cortex_a53_hazard")

(define_insn_reservation "cortex_a53_alu" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,
			alu_sreg,alus_sreg,logic_reg,logics_reg,
			adc_imm,adcs_imm,adc_reg,adcs_reg,
			csel,clz,rbit,rev,alu_dsp_reg,
			mov_reg,mvn_reg,mrs,multiple"))
  "cortex_a53_slot_any")

(define_insn_reservation "cortex_a53_alu_shift" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "alu_shift_imm,alus_shift_imm,
			crc,logic_shift_imm,logics_shift_imm,
			alu_ext,alus_ext,bfm,bfx,extend,mvn_shift"))
  "cortex_a53_slot_any")

(define_insn_reservation "cortex_a53_alu_shift_reg" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "alu_shift_reg,alus_shift_reg,
			logic_shift_reg,logics_shift_reg,
			mvn_shift_reg"))
  "cortex_a53_slot_any+cortex_a53_hazard")

(define_insn_reservation "cortex_a53_alu_extr" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "rotate_imm"))
  "cortex_a53_slot1|cortex_a53_single_issue")

(define_insn_reservation "cortex_a53_mul" 4
  (and (eq_attr "tune" "cortexa53")
       (ior (eq_attr "mul32" "yes")
	    (eq_attr "widen_mul64" "yes")))
  "cortex_a53_slot_any+cortex_a53_imul")

;; From the perspective of the GCC scheduling state machine, if we wish to
;; model an instruction as serialising other instructions, we are best to do
;; so by modelling it as taking very few cycles.  Scheduling many other
;; instructions underneath it at the cost of freedom to pick from the
;; ready list is likely to hurt us more than it helps.  However, we do
;; want to model some resource and latency cost for divide instructions in
;; order to avoid divides ending up too lumpy.

(define_insn_reservation "cortex_a53_div" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "udiv,sdiv"))
  "cortex_a53_slot0,cortex_a53_idiv*2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/store instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: load<n> is not prescriptive about how much data is to be loaded.
;; This is most obvious for LDRD from AArch32 and LDP (X register) from
;; AArch64, both are tagged load2 but LDP will load 128-bits compared to
;; LDRD which is 64-bits.
;;
;; For the below, we assume AArch64 X-registers for load2, and AArch32
;; registers for load3/load4.

(define_insn_reservation "cortex_a53_load1" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "load_byte,load_4,load_acq"))
  "cortex_a53_slot_any+cortex_a53_ls_agen,
   cortex_a53_load")

(define_insn_reservation "cortex_a53_store1" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "store_4,store_rel"))
  "cortex_a53_slot_any+cortex_a53_ls_agen,
   cortex_a53_store")

;; Model AArch64-sized LDP Xm, Xn, [Xa]

(define_insn_reservation "cortex_a53_load2" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "load_8"))
  "cortex_a53_single_issue+cortex_a53_ls_agen,
   cortex_a53_load+cortex_a53_slot0,
   cortex_a53_load")

(define_insn_reservation "cortex_a53_store2" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "store_8"))
  "cortex_a53_slot_any+cortex_a53_ls_agen,
   cortex_a53_store")

;; Model AArch32-sized LDM Ra, {Rm, Rn, Ro}

(define_insn_reservation "cortex_a53_load3plus" 6
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "load_12,load_16"))
  "cortex_a53_single_issue+cortex_a53_ls_agen,
   cortex_a53_load+cortex_a53_slot0,
   cortex_a53_load")

(define_insn_reservation "cortex_a53_store3plus" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "store_12,store_16"))
  "cortex_a53_slot_any+cortex_a53_ls_agen,
   cortex_a53_store+cortex_a53_slot0,
   cortex_a53_store")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Model all branches as dual-issuable from either execution, which
;; is not strictly true for all cases (indirect branches).

(define_insn_reservation "cortex_a53_branch" 0
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "branch,call"))
  "cortex_a53_slot_any+cortex_a53_branch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General-purpose register bypasses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Model bypasses for ALU to ALU instructions.

(define_bypass 0 "cortex_a53_shift*"
		 "cortex_a53_alu")

(define_bypass 1 "cortex_a53_shift*"
		 "cortex_a53_shift*,cortex_a53_alu_*")

(define_bypass 1 "cortex_a53_alu*"
		 "cortex_a53_alu")

(define_bypass 1 "cortex_a53_alu*"
		 "cortex_a53_alu_shift*"
		 "arm_no_early_alu_shift_dep")

(define_bypass 2 "cortex_a53_alu*"
		 "cortex_a53_alu_*,cortex_a53_shift*")

;; Model a bypass from MUL/MLA to MLA instructions.

(define_bypass 1 "cortex_a53_mul"
		 "cortex_a53_mul"
		 "aarch_accumulator_forwarding")

;; Model a bypass from MUL/MLA to ALU instructions.

(define_bypass 2 "cortex_a53_mul"
		 "cortex_a53_alu")

(define_bypass 3 "cortex_a53_mul"
		 "cortex_a53_alu_*,cortex_a53_shift*")

;; Model bypasses for loads which are to be consumed by the ALU.

(define_bypass 2 "cortex_a53_load1"
		 "cortex_a53_alu")

(define_bypass 3 "cortex_a53_load1"
		 "cortex_a53_alu_*,cortex_a53_shift*")

(define_bypass 3 "cortex_a53_load2"
		 "cortex_a53_alu")

;; Model a bypass for ALU instructions feeding stores.

(define_bypass 0 "cortex_a53_alu*,cortex_a53_shift*"
		 "cortex_a53_store*"
		 "arm_no_early_store_addr_dep")

;; Model a bypass for load and multiply instructions feeding stores.

(define_bypass 1 "cortex_a53_mul,
		  cortex_a53_load*"
		 "cortex_a53_store*"
		 "arm_no_early_store_addr_dep")

;; Model a bypass for load to load/store address.

(define_bypass 3 "cortex_a53_load1"
		 "cortex_a53_load*"
		 "arm_early_load_addr_dep_ptr")

(define_bypass 3 "cortex_a53_load1"
		 "cortex_a53_store*"
		 "arm_early_store_addr_dep_ptr")

;; Model a GP->FP register move as similar to stores.

(define_bypass 0 "cortex_a53_alu*,cortex_a53_shift*"
		 "cortex_a53_r2f")

(define_bypass 1 "cortex_a53_mul,
		  cortex_a53_load1,
		  cortex_a53_load2"
		 "cortex_a53_r2f")

(define_bypass 2 "cortex_a53_alu*"
		 "cortex_a53_r2f_cvt")

(define_bypass 3 "cortex_a53_mul,
		  cortex_a53_load1,
		  cortex_a53_load2"
		 "cortex_a53_r2f_cvt")

;; Model flag forwarding to branches.

(define_bypass 0 "cortex_a53_alu*,cortex_a53_shift*"
		 "cortex_a53_branch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point/Advanced SIMD.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_automaton "cortex_a53_advsimd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Broad Advanced SIMD type categorisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_attr "cortex_a53_advsimd_type"
  "advsimd_alu, advsimd_alu_q,
   advsimd_mul, advsimd_mul_q,
   advsimd_div_s, advsimd_div_s_q,
   advsimd_div_d, advsimd_div_d_q,
   advsimd_load_64, advsimd_store_64,
   advsimd_load_128, advsimd_store_128,
   advsimd_load_lots, advsimd_store_lots,
   unknown"
  (cond [
    (eq_attr "type" "neon_add, neon_qadd, neon_add_halve, neon_sub, neon_qsub,\
		     neon_sub_halve, neon_abs, neon_neg, neon_qneg,\
		     neon_qabs, neon_abd, neon_minmax, neon_compare,\
		     neon_compare_zero, neon_arith_acc, neon_reduc_add,\
		     neon_reduc_add_acc, neon_reduc_minmax,\
		     neon_logic, neon_tst, neon_shift_imm,\
		     neon_shift_reg, neon_shift_acc, neon_sat_shift_imm,\
		     neon_sat_shift_reg, neon_ins, neon_move,\
		     neon_permute, neon_zip, neon_tbl1,\
		     neon_tbl2, neon_tbl3, neon_tbl4, neon_bsl,\
		     neon_cls, neon_cnt, neon_dup,\
		     neon_ext, neon_rbit, neon_rev,\
		     neon_fp_abd_s, neon_fp_abd_d,\
		     neon_fp_abs_s, neon_fp_abs_d,\
		     neon_fp_addsub_s, neon_fp_addsub_d, neon_fp_compare_s,\
		     neon_fp_compare_d, neon_fp_minmax_s,\
		     neon_fp_minmax_d, neon_fp_neg_s, neon_fp_neg_d,\
		     neon_fp_reduc_add_s, neon_fp_reduc_add_d,\
		     neon_fp_reduc_minmax_s, neon_fp_reduc_minmax_d,\
		     neon_fp_cvt_widen_h, neon_fp_to_int_s,neon_fp_to_int_d,\
		     neon_int_to_fp_s, neon_int_to_fp_d, neon_fp_round_s,\
		     neon_fp_recpe_s, neon_fp_recpe_d, neon_fp_recps_s,\
		     neon_fp_recps_d, neon_fp_recpx_s, neon_fp_recpx_d,\
		     neon_fp_rsqrte_s, neon_fp_rsqrte_d, neon_fp_rsqrts_s,\
		     neon_fp_rsqrts_d")
      (const_string "advsimd_alu")
    (eq_attr "type" "neon_add_q, neon_add_widen, neon_add_long,\
		     neon_qadd_q, neon_add_halve_q, neon_add_halve_narrow_q,\
		     neon_sub_q, neon_sub_widen, neon_sub_long,\
		     neon_qsub_q, neon_sub_halve_q, neon_sub_halve_narrow_q,\
		     neon_abs_q, neon_neg_q, neon_qneg_q, neon_qabs_q,\
		     neon_abd_q, neon_abd_long, neon_minmax_q,\
		     neon_compare_q, neon_compare_zero_q,\
		     neon_arith_acc_q, neon_reduc_add_q,\
		     neon_reduc_add_long, neon_reduc_add_acc_q,\
		     neon_reduc_minmax_q, neon_logic_q, neon_tst_q,\
		     neon_shift_imm_q, neon_shift_imm_narrow_q,\
		     neon_shift_imm_long, neon_shift_reg_q,\
		     neon_shift_acc_q, neon_sat_shift_imm_q,\
		     neon_sat_shift_imm_narrow_q, neon_sat_shift_reg_q,\
		     neon_ins_q, neon_move_q, neon_move_narrow_q,\
		     neon_permute_q, neon_zip_q,\
		     neon_tbl1_q, neon_tbl2_q, neon_tbl3_q,\
		     neon_tbl4_q, neon_bsl_q, neon_cls_q, neon_cnt_q,\
		     neon_dup_q, neon_ext_q, neon_rbit_q,\
		     neon_rev_q, neon_fp_abd_s_q, neon_fp_abd_d_q,\
		     neon_fp_abs_s_q, neon_fp_abs_d_q,\
		     neon_fp_addsub_s_q, neon_fp_addsub_d_q,\
		     neon_fp_compare_s_q, neon_fp_compare_d_q,\
		     neon_fp_minmax_s_q, neon_fp_minmax_d_q,\
		     neon_fp_cvt_widen_s, neon_fp_neg_s_q, neon_fp_neg_d_q,\
		     neon_fp_reduc_add_s_q, neon_fp_reduc_add_d_q,\
		     neon_fp_reduc_minmax_s_q, neon_fp_reduc_minmax_d_q,\
		     neon_fp_cvt_narrow_s_q, neon_fp_cvt_narrow_d_q,\
		     neon_fp_to_int_s_q, neon_fp_to_int_d_q,\
		     neon_int_to_fp_s_q, neon_int_to_fp_d_q,\
		     neon_fp_round_s_q,\
		     neon_fp_recpe_s_q, neon_fp_recpe_d_q,\
		     neon_fp_recps_s_q, neon_fp_recps_d_q,\
		     neon_fp_recpx_s_q, neon_fp_recpx_d_q,\
		     neon_fp_rsqrte_s_q, neon_fp_rsqrte_d_q,\
		     neon_fp_rsqrts_s_q, neon_fp_rsqrts_d_q")
      (const_string "advsimd_alu_q")
    (eq_attr "type" "neon_mul_b, neon_mul_h, neon_mul_s,\
		     neon_mul_h_scalar, neon_mul_s_scalar,\
		     neon_sat_mul_b, neon_sat_mul_h, neon_sat_mul_s,\
		     neon_sat_mul_h_scalar, neon_sat_mul_s_scalar,\
		     neon_mla_b, neon_mla_h, neon_mla_s,\
		     neon_mla_h_scalar, neon_mla_s_scalar,\
		     neon_fp_mul_s, neon_fp_mul_s_scalar,\
		     neon_fp_mul_d, neon_fp_mla_s,\
		     neon_fp_mla_s_scalar, neon_fp_mla_d")
      (const_string "advsimd_mul")
    (eq_attr "type" "neon_mul_b_q, neon_mul_h_q, neon_mul_s_q,\
		     neon_mul_b_long, neon_mul_h_long, neon_mul_s_long,\
		     neon_mul_d_long, neon_mul_h_scalar_q,\
		     neon_mul_s_scalar_q, neon_mul_h_scalar_long,\
		     neon_mul_s_scalar_long, neon_sat_mul_b_q,\
		     neon_sat_mul_h_q, neon_sat_mul_s_q,\
		     neon_sat_mul_b_long, neon_sat_mul_h_long,\
		     neon_sat_mul_s_long, neon_sat_mul_h_scalar_q,\
		     neon_sat_mul_s_scalar_q, neon_sat_mul_h_scalar_long,\
		     neon_sat_mul_s_scalar_long, crypto_pmull, neon_mla_b_q,\
		     neon_mla_h_q, neon_mla_s_q, neon_mla_b_long,\
		     neon_mla_h_long, neon_mla_s_long,\
		     neon_mla_h_scalar_q, neon_mla_s_scalar_q,\
		     neon_mla_h_scalar_long, neon_mla_s_scalar_long,\
		     neon_sat_mla_b_long, neon_sat_mla_h_long,\
		     neon_sat_mla_s_long, neon_sat_mla_h_scalar_long,\
		     neon_sat_mla_s_scalar_long,\
		     neon_fp_mul_s_q, neon_fp_mul_s_scalar_q,\
		     neon_fp_mul_d_q, neon_fp_mul_d_scalar_q,\
		     neon_fp_mla_s_q, neon_fp_mla_s_scalar_q,\
		     neon_fp_mla_d_q, neon_fp_mla_d_scalar_q")
      (const_string "advsimd_mul_q")
    (eq_attr "type" "neon_fp_sqrt_s, neon_fp_div_s")
      (const_string "advsimd_div_s")
    (eq_attr "type" "neon_fp_sqrt_s_q, neon_fp_div_s_q")
      (const_string "advsimd_div_s_q")
    (eq_attr "type" "neon_fp_sqrt_d, neon_fp_div_d")
      (const_string "advsimd_div_d")
    (eq_attr "type" "neon_fp_sqrt_d_q, neon_fp_div_d_q")
      (const_string "advsimd_div_d_q")
    (eq_attr "type" "neon_ldr, neon_load1_1reg,\
		     neon_load1_all_lanes, neon_load1_all_lanes_q,\
		     neon_load1_one_lane, neon_load1_one_lane_q")
      (const_string "advsimd_load_64")
    (eq_attr "type" "neon_str, neon_store1_1reg,\
		     neon_store1_one_lane,neon_store1_one_lane_q")
      (const_string "advsimd_store_64")
    (eq_attr "type" "neon_load1_1reg_q, neon_load1_2reg,\
		     neon_load2_2reg,\
		     neon_load2_all_lanes, neon_load2_all_lanes_q,\
		     neon_load2_one_lane, neon_load2_one_lane_q")
      (const_string "advsimd_load_128")
    (eq_attr "type" "neon_store1_1reg_q, neon_store1_2reg,\
		     neon_store2_2reg,\
		     neon_store2_one_lane, neon_store2_one_lane_q")
      (const_string "advsimd_store_128")
    (eq_attr "type" "neon_load1_2reg_q, neon_load1_3reg, neon_load1_3reg_q,\
		     neon_load1_4reg, neon_load1_4reg_q, \
		     neon_load2_2reg_q, neon_load2_4reg,\
		     neon_load2_4reg_q, neon_load3_3reg,\
		     neon_load3_3reg_q, neon_load3_all_lanes,\
		     neon_load3_all_lanes_q, neon_load3_one_lane,\
		     neon_load3_one_lane_q, neon_load4_4reg,\
		     neon_load4_4reg_q, neon_load4_all_lanes,\
		     neon_load4_all_lanes_q, neon_load4_one_lane,\
		     neon_load4_one_lane_q, neon_ldp, neon_ldp_q")
      (const_string "advsimd_load_lots")
    (eq_attr "type" "neon_store1_2reg_q, neon_store1_3reg,\
		     neon_store1_3reg_q, neon_store1_4reg,\
		     neon_store1_4reg_q, neon_store2_2reg_q,\
		     neon_store2_4reg, neon_store2_4reg_q,\
		     neon_store3_3reg, neon_store3_3reg_q,\
		     neon_store3_one_lane, neon_store3_one_lane_q,\
		     neon_store4_4reg, neon_store4_4reg_q,\
		     neon_store4_one_lane, neon_store4_one_lane_q,\
		     neon_stp, neon_stp_q")
      (const_string "advsimd_store_lots")]
      (const_string "unknown")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point/Advanced SIMD functional units.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We model the Advanced SIMD unit as two 64-bit units, each with three
;; pipes, FP_ALU, FP_MUL, FP_DIV.  We also give convenient reservations
;; for 128-bit Advanced SIMD instructions, which use both units.

;; The floating-point/Advanced SIMD ALU pipelines.

(define_cpu_unit "cortex_a53_fp_alu_lo,\
		  cortex_a53_fp_alu_hi"
		 "cortex_a53_advsimd")

(define_reservation "cortex_a53_fp_alu"
		    "cortex_a53_fp_alu_lo\
		     |cortex_a53_fp_alu_hi")

(define_reservation "cortex_a53_fp_alu_q"
		    "cortex_a53_fp_alu_lo\
		     +cortex_a53_fp_alu_hi")

;; The floating-point/Advanced SIMD multiply/multiply-accumulate
;; pipelines.

(define_cpu_unit "cortex_a53_fp_mul_lo,\
		  cortex_a53_fp_mul_hi"
		 "cortex_a53_advsimd")

(define_reservation "cortex_a53_fp_mul"
		    "cortex_a53_fp_mul_lo\
		     |cortex_a53_fp_mul_hi")

(define_reservation "cortex_a53_fp_mul_q"
		    "cortex_a53_fp_mul_lo\
		     +cortex_a53_fp_mul_hi")

;; Floating-point/Advanced SIMD divide/square root.

(define_cpu_unit "cortex_a53_fp_div_lo,\
		  cortex_a53_fp_div_hi"
		 "cortex_a53_advsimd")

;; Once we choose a pipe, stick with it for three simulated cycles.

(define_reservation "cortex_a53_fp_div"
		    "(cortex_a53_fp_div_lo*3)\
		     |(cortex_a53_fp_div_hi*3)")

(define_reservation "cortex_a53_fp_div_q"
		    "(cortex_a53_fp_div_lo*3)\
		     +(cortex_a53_fp_div_hi*3)")

;; Cryptographic extensions

(define_cpu_unit "cortex_a53_crypto"
		 "cortex_a53_advsimd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point arithmetic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_fpalu" 4
  (and (eq_attr "tune" "cortexa53")
	(eq_attr "type" "ffariths, fadds, ffarithd, faddd, fmov,
			f_cvt, fcmps, fcmpd, fccmps, fccmpd, fcsel,
			f_rints, f_rintd, f_minmaxs, f_minmaxd"))
  "cortex_a53_slot_any,cortex_a53_fp_alu")

(define_insn_reservation "cortex_a53_fconst" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fconsts,fconstd"))
  "cortex_a53_slot_any,cortex_a53_fp_alu")

(define_insn_reservation "cortex_a53_fpmul" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fmuls,fmuld"))
  "cortex_a53_slot_any,cortex_a53_fp_mul")

;; For multiply-accumulate, model the add (accumulate) as being issued
;; after the multiply completes.

(define_insn_reservation "cortex_a53_fpmac" 8
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fmacs,fmacd,ffmas,ffmad"))
  "cortex_a53_slot_any,cortex_a53_fp_mul,
   nothing*3, cortex_a53_fp_alu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point to/from core transfers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_r2f" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_mcr,f_mcrr"))
  "cortex_a53_slot_any,cortex_a53_fp_alu")

(define_insn_reservation "cortex_a53_f2r" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_mrc,f_mrrc"))
  "cortex_a53_slot_any,cortex_a53_fp_alu")

(define_insn_reservation "cortex_a53_r2f_cvt" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_cvti2f, neon_from_gp, neon_from_gp_q"))
  "cortex_a53_slot_any,cortex_a53_fp_alu")

(define_insn_reservation "cortex_a53_f2r_cvt" 5
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_cvtf2i, neon_to_gp, neon_to_gp_q"))
  "cortex_a53_slot_any,cortex_a53_fp_alu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point flag transfer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_f_flags" 5
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_flag"))
  "cortex_a53_slot_any")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point load/store.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_f_load_64" 3
  (and (eq_attr "tune" "cortexa53")
       (ior (eq_attr "type" "f_loads,f_loadd")
	    (eq_attr "cortex_a53_advsimd_type"
		     "advsimd_load_64")))
  "cortex_a53_slot_any+cortex_a53_ls_agen,
   cortex_a53_load")

(define_insn_reservation "cortex_a53_f_load_many" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type"
		"advsimd_load_128,advsimd_load_lots"))
  "cortex_a53_single_issue+cortex_a53_ls_agen,
   cortex_a53_load+cortex_a53_slot0,
   cortex_a53_load")

(define_insn_reservation "cortex_a53_f_store_64" 0
  (and (eq_attr "tune" "cortexa53")
       (ior (eq_attr "type" "f_stores,f_stored")
	    (eq_attr "cortex_a53_advsimd_type"
		     "advsimd_store_64")))
  "cortex_a53_slot_any+cortex_a53_ls_agen,
   cortex_a53_store")

(define_insn_reservation "cortex_a53_f_store_many" 0
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type"
		"advsimd_store_128,advsimd_store_lots"))
  "cortex_a53_slot_any+cortex_a53_ls_agen,
   cortex_a53_store+cortex_a53_slot0,
   cortex_a53_store")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced SIMD.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Either we want to model use of the ALU pipe, the multiply pipe or the
;; divide/sqrt pipe.  In all cases we need to check if we are a 64-bit
;; operation (in which case we model dual-issue without penalty)
;; or a 128-bit operation in which case we require in our model that we
;; issue from slot 0.

(define_insn_reservation "cortex_a53_advsimd_alu" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type" "advsimd_alu"))
  "cortex_a53_slot_any,cortex_a53_fp_alu")

(define_insn_reservation "cortex_a53_advsimd_alu_q" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type" "advsimd_alu_q"))
  "cortex_a53_slot0,cortex_a53_fp_alu_q")

(define_insn_reservation "cortex_a53_advsimd_mul" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type" "advsimd_mul"))
  "cortex_a53_slot_any,cortex_a53_fp_mul")

(define_insn_reservation "cortex_a53_advsimd_mul_q" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type" "advsimd_mul_q"))
  "cortex_a53_slot0,cortex_a53_fp_mul_q")

;; SIMD Dividers.

(define_insn_reservation "cortex_a53_advsimd_div_s" 14
  (and (eq_attr "tune" "cortexa53")
       (ior (eq_attr "type" "fdivs,fsqrts")
       (eq_attr "cortex_a53_advsimd_type" "advsimd_div_s")))
  "cortex_a53_slot0,cortex_a53_fp_mul,
   cortex_a53_fp_div")

(define_insn_reservation "cortex_a53_advsimd_div_d" 29
  (and (eq_attr "tune" "cortexa53")
       (ior (eq_attr "type" "fdivd,fsqrtd")
	    (eq_attr "cortex_a53_advsimd_type" "advsimd_div_d")))
  "cortex_a53_slot0,cortex_a53_fp_mul,
   cortex_a53_fp_div")

(define_insn_reservation "cortex_a53_advsimd_div_s_q" 14
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type" "advsimd_div_s_q"))
  "cortex_a53_single_issue,cortex_a53_fp_mul_q,
   cortex_a53_fp_div_q")

(define_insn_reservation "cortex_a53_advsimd_divd_q" 29
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "cortex_a53_advsimd_type" "advsimd_div_d_q"))
  "cortex_a53_single_issue,cortex_a53_fp_mul_q,
   cortex_a53_fp_div_q")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARMv8-A Cryptographic extensions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We want AESE and AESMC to end up consecutive to one another.

(define_insn_reservation "cortex_a53_crypto_aese" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "crypto_aese"))
  "cortex_a53_slot0")

(define_insn_reservation "cortex_a53_crypto_aesmc" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "crypto_aesmc"))
  "cortex_a53_slot_any")

;; SHA1H

(define_insn_reservation "cortex_a53_crypto_sha1_fast" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "crypto_sha1_fast"))
  "cortex_a53_slot_any,cortex_a53_crypto")

(define_insn_reservation "cortex_a53_crypto_sha256_fast" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "crypto_sha256_fast"))
  "cortex_a53_slot0,cortex_a53_crypto")

(define_insn_reservation "cortex_a53_crypto_sha1_xor" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "crypto_sha1_xor"))
  "cortex_a53_slot0,cortex_a53_crypto")

(define_insn_reservation "cortex_a53_crypto_sha_slow" 5
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "crypto_sha1_slow, crypto_sha256_slow"))
  "cortex_a53_slot0,cortex_a53_crypto")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point/Advanced SIMD register bypasses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Model the late use of the accumulator operand for floating-point
;; multiply-accumulate operations as a bypass reducing the latency
;; of producing instructions to near zero.

(define_bypass 1 "cortex_a53_fpalu,
		  cortex_a53_fpmul,
		  cortex_a53_r2f,
		  cortex_a53_r2f_cvt,
		  cortex_a53_fconst,
		  cortex_a53_f_load*"
		 "cortex_a53_fpmac"
		 "aarch_accumulator_forwarding")

(define_bypass 4 "cortex_a53_fpmac"
		 "cortex_a53_fpmac"
		 "aarch_accumulator_forwarding")

