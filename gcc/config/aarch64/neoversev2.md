;; Instruction attribute for dispatch scheduling for Neoverse V2.
;; Copyright The GNU Toolchain Authors.
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

;; Attribute that groups other instruction attributes into dispatch groups
;; for Neoverse V2 cores.  Dispatch groups are groups of pipelines for which
;; the SWOG specifies a dispatch constraint.  For example: Because the SWOG
;; contains a dispatch constraint for the V02 pipelines, there is an attribute
;; value "v02" that groups instructions that are processed by the V0 and V2
;; pipelines.
;; Values that contain a "_" represent combinations of dispatch groups.
;; For example, there are dispatch constraints for the M0 and V pipelines.
;; The value "m0_v" groups instructions that utilize the M0 as well as the
;; V pipelines, such that both dispatch constraints apply.

(define_attr "neoversev2_dispatch"
  "none,bs01,bsm,m,m0,v02,v13,v,l01,l,bsm_l,m_l,m0_v,v_v13,v_l,\
   l01_d,l01_v"
  (cond [(eq_attr "type" "branch,call")
	 (const_string "bs01")
	 (ior
	   (eq_attr "type" "adc_reg,alu_ext,alu_imm,alu_sreg,alus_ext,\
	    alus_imm,alus_sreg,clz,csel,logic_imm,logic_reg,logics_imm,\
	    logics_reg,mov_imm,rbit,rev,shift_reg")
	   (eq_attr "sve_type" "sve_pred_cnt_scalar"))
	 (const_string "bsm")
	 (ior
	   (eq_attr "type" "alu_ext,alus_ext,bfm,bfx,mul,rotate_imm,\
	    smull,umull")
	   (eq_attr "autodetect_type" "alu_shift_asr_op2,alu_shift_lsl_op2,\
	    alu_shift_lsr_op2")
	   (eq_attr "sve_type" "sve_pred_cnt_ctrl,sve_pred_misc"))
	 (const_string "m")
	 (ior
	   (eq_attr "type" "crc,f_cvti2f,mla,neon_from_gp,neon_from_gp_q,\
	    sdiv,smlal,udiv,umlal")
	   (eq_attr "sve_type" "sve_ffr,sve_pred_logical"))
	 (const_string "m0")
	 (ior
	   (eq_attr "type"
	    "crypto_sha256_slow,crypto_sha3,crypto_sha512,crypto_sm3,\
	     crypto_sm4,f_rintd,f_rints,fccmpd,fccmps,fcmpd,fcmps,fdivd,\
	     fdivs,fsqrtd,fsqrts,neon_fp_cvt_narrow_d_q,\
	     neon_fp_cvt_narrow_s_q,neon_fp_cvt_widen_h,neon_fp_cvt_widen_s,\
	     neon_fp_div_d,neon_fp_div_d_q,neon_fp_div_s,neon_fp_div_s_q,\
	     neon_fp_recpe_d,neon_fp_recpe_d_q,neon_fp_recpe_s,\
	     neon_fp_recpe_s_q,neon_fp_recps_d,neon_fp_recps_d_q,\
	     neon_fp_recps_s,neon_fp_recps_s_q,neon_fp_recpx_d,\
	     neon_fp_recpx_d_q,neon_fp_recpx_s,neon_fp_recpx_s_q,\
	     neon_fp_round_d,neon_fp_round_d_q,neon_fp_round_s,\
	     neon_fp_round_s_q,neon_fp_rsqrte_d,neon_fp_rsqrte_d_q,\
	     neon_fp_rsqrte_s,neon_fp_rsqrte_s_q,neon_fp_rsqrts_d,\
	     neon_fp_rsqrts_d_q,neon_fp_rsqrts_s,neon_fp_rsqrts_s_q,\
	     neon_fp_sqrt_d,neon_fp_sqrt_d_q,neon_fp_sqrt_s,\
	     neon_fp_sqrt_s_q,neon_fp_to_int_d,neon_fp_to_int_d_q,\
	     neon_fp_to_int_s,neon_fp_to_int_s_q,neon_int_to_fp_d,\
	     neon_int_to_fp_d_q,neon_int_to_fp_s,neon_int_to_fp_s_q,\
	     neon_mla_b,neon_mla_b_q,neon_mla_h,neon_mla_h_q,\
	     neon_mla_s,neon_mla_s_q,neon_mla_b_long,neon_mla_h_long,\
	     neon_mla_h_scalar,neon_mla_h_scalar_q,neon_mla_s_long,\
	     neon_mla_s_scalar,neon_mla_s_scalar_q,neon_mla_h_scalar_long,\
	     neon_mla_s_scalar_long,neon_mul_b,neon_mul_b_q,\
	     neon_mul_d_long,neon_mul_h,neon_mul_h_q,neon_mul_h_long,\
	     neon_mul_h_scalar,neon_mul_h_scalar_q,neon_mul_h_scalar_long,\
	     neon_mul_s,neon_mul_s_q,neon_mul_s_long,neon_mul_s_scalar,\
	     neon_mul_s_scalar_q,neon_mul_s_scalar_long,neon_sat_mla_b_long,\
	     neon_sat_mla_h_long,neon_sat_mla_h_scalar_long,\
	     neon_sat_mla_s_long,neon_sat_mla_s_scalar_long,\
	     neon_sat_mul_b,neon_sat_mul_b_q,neon_sat_mul_b_long,\
	     neon_sat_mul_h,neon_sat_mul_h_q,neon_sat_mul_h_long,\
	     neon_sat_mul_h_scalar,neon_sat_mul_h_scalar_q,\
	     neon_sat_mul_h_scalar_long,neon_sat_mul_s,neon_sat_mul_s_q,\
	     neon_sat_mul_s_long,neon_sat_mul_s_scalar,\
	     neon_sat_mul_s_scalar_q,neon_sat_mul_s_scalar_long")
	   (eq_attr "sve_type"
	    "sve_crypto_sha3,sve_fp_cmp,sve_fp_cvt,sve_fp_div,sve_fp_log,\
	     sve_fp_sqrt,sve_int_cvt,sve_int_div,sve_int_dot,sve_int_index,\
	     sve_int_mul,sve_int_recip_est"))
	 (const_string "v02")
	 (ior
	   (eq_attr "type"
	    "neon_arith_acc,neon_arith_acc_q,neon_reduc_add,\
	     neon_reduc_add_long,neon_reduc_add_q,neon_reduc_minmax,\
	     neon_reduc_minmax_q,neon_sat_shift_imm,\
	     neon_sat_shift_imm_narrow_q,neon_sat_shift_imm_q,\
	     neon_sat_shift_reg,neon_sat_shift_reg_q,neon_shift_acc,\
	     neon_shift_acc_q,neon_shift_imm,neon_shift_imm_long,\
	     neon_shift_imm_narrow_q,neon_shift_imm_q,neon_shift_reg,\
	     neon_shift_reg_q")
	   (eq_attr "sve_type"
	    "sve_fp_assoc_add,sve_fp_exp,sve_int_accum,sve_int_bit_perm,\
	     sve_int_extend,sve_int_extract,sve_int_shift"))
	 (const_string "v13")
	 (ior
	   (eq_attr "type" "crypto_pmull,f_cvt,f_cvtf2i,f_minmaxd,f_minmaxs,\
	    faddd,fadds,fconstd,fconsts,fcsel,ffarithd,ffariths,fmacd,fmacs,\
	    fmov,fmuld,fmuls,f_mcr,f_mrc,neon_abd,\
	    neon_abd_long,neon_abd_q,neon_abs,neon_abs_q,neon_add,\
	    neon_add_halve,neon_add_halve_narrow_q,neon_add_halve_q,\
	    neon_add_long,neon_add_q,neon_add_widen,neon_bsl,neon_bsl_q,\
	    neon_cls,neon_cls_q,neon_cnt,neon_cnt_q,neon_compare,\
	    neon_compare_q,neon_compare_zero,neon_compare_zero_q,\
	    neon_dup,neon_dup_q,neon_ext,neon_ext_q,neon_fcadd,neon_fcmla,\
	    neon_fp_abd_d,neon_fp_abd_d_q,neon_fp_abd_s,neon_fp_abd_s_q,\
	    neon_fp_abs_d,neon_fp_abs_d_q,neon_fp_abs_s,neon_fp_abs_s_q,\
	    neon_fp_addsub_d,neon_fp_addsub_d_q,neon_fp_addsub_s,\
	    neon_fp_addsub_s_q,neon_fp_compare_d,neon_fp_compare_d_q,\
	    neon_fp_compare_s,neon_fp_compare_s_q,neon_fp_minmax_d,\
	    neon_fp_minmax_d_q,neon_fp_minmax_s,neon_fp_minmax_s_q,\
	    neon_fp_mla_d,neon_fp_mla_d_q,neon_fp_mla_d_scalar_q,\
	    neon_fp_mla_s,neon_fp_mla_s_q,neon_fp_mla_s_scalar,\
	    neon_fp_mla_s_scalar_q,neon_fp_mul_d,neon_fp_mul_d_q,\
	    neon_fp_mul_d_scalar_q,neon_fp_mul_s,neon_fp_mul_s_q,\
	    neon_fp_mul_s_scalar,neon_fp_mul_s_scalar_q,neon_fp_neg_d,\
	    neon_fp_neg_d_q,neon_fp_neg_s,neon_fp_neg_s_q,neon_fp_reduc_add_d,\
	    neon_fp_reduc_add_d_q,neon_fp_reduc_add_s,neon_fp_reduc_add_s_q,\
	    neon_fp_reduc_minmax_d,neon_fp_reduc_minmax_d_q,\
	    neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_s_q,neon_logic,\
	    neon_logic_q,neon_minmax,neon_minmax_q,neon_move,\
	    neon_move_narrow_q,neon_move_q,neon_neg,neon_neg_q,neon_permute,\
	    neon_permute_q,neon_qabs,neon_qabs_q,neon_qadd,neon_qadd_q,\
	    neon_qneg,neon_qneg_q,neon_qsub,neon_qsub_q,neon_rbit,\
	    neon_rbit_q,neon_rev,neon_rev_q,neon_sub,neon_sub_halve,\
	    neon_sub_halve_narrow_q,neon_sub_halve_q,neon_sub_long,\
	    neon_sub_q,neon_sub_widen,neon_tbl1,neon_tbl1_q,neon_tbl2,\
	    neon_tbl2_q,neon_tbl3,neon_tbl3_q,neon_tbl4,neon_tbl4_q,\
	    neon_to_gp,neon_to_gp_q,neon_tst,neon_tst_q,neon_zip,\
	    neon_zip_q")
	   (eq_attr "sve_type" "sve_fp_arith,sve_fp_misc,sve_fp_mul,\
	    sve_fp_reduc,sve_int_general,sve_int_pmul"))
	 (const_string "v")
	 (eq_attr "sve_type" "sve_store_pred")
	 (const_string "l01")
	 (ior
	   (eq_attr "type" "neon_ldp,neon_ldp_q,neon_load1_1reg,\
	    neon_load1_1reg_q,neon_load1_2reg,neon_load1_2reg_q,\
	    neon_load1_3reg,neon_load1_3reg_q,neon_load1_4reg,\
	    neon_load1_4reg_q")
	   (eq_attr "sve_type" "sve_load_1reg"))
	 (const_string "l")
	 (eq_attr "type" "f_loadd,f_loads")
	 (const_string "bsm_l")
	 (eq_attr "sve_type" "sve_load_pred")
	 (const_string "m_l")
	 (ior
	   (eq_attr "type" "neon_ins,neon_ins_q")
	   (eq_attr "sve_type" "sve_int_cmp_set,sve_int_match,sve_pred_vec"))
	 (const_string "m0_v")
	 (eq_attr "sve_type" "sve_int_reduc")
	 (const_string "v_v13")
	 (ior
	   (eq_attr "type" "neon_load1_all_lanes,neon_load1_one_lane,\
	    neon_load1_one_lane_q,neon_load2_2reg,neon_load2_2reg_q,\
	    neon_load2_all_lanes,neon_load2_all_lanes_q,neon_load2_one_lane,\
	    neon_load3_3reg,neon_load3_3reg_q,neon_load3_all_lanes,\
	    neon_load3_all_lanes_q,neon_load3_one_lane,neon_load4_4reg,\
	    neon_load4_4reg_q,neon_load4_all_lanes,neon_load4_all_lanes_q,\
	    neon_load4_one_lane")
	   (eq_attr "sve_type" "sve_gatherload_32,sve_gatherload_64,\
	    sve_load_2reg,sve_load_3reg,sve_load_4reg"))
	 (const_string "v_l")
	 (eq_attr "type" "load_16,load_4,load_8,store_16,store_4,store_8")
	 (const_string "l01_d")
	 (ior
	   (eq_attr "type" "f_stored,f_stores,neon_stp,neon_stp_q,\
	    neon_store1_1reg,neon_store1_1reg_q,neon_store1_2reg,\
	    neon_store1_2reg_q,neon_store1_3reg,neon_store1_3reg_q,\
	    neon_store1_4reg,neon_store1_4reg_q,neon_store1_one_lane,\
	    neon_store1_one_lane_q,neon_store2_2reg,neon_store2_2reg_q,\
	    neon_store2_one_lane,neon_store2_one_lane_q,neon_store3_3reg,\
	    neon_store3_3reg_q,neon_store3_one_lane,neon_store3_one_lane_q,\
	    neon_store4_4reg,neon_store4_4reg_q,neon_store4_one_lane,\
	    neon_store4_one_lane_q")
	   (eq_attr "sve_type" "sve_scatterstore_32,sve_scatterstore_64,\
	    sve_store_1reg,sve_store_2reg,sve_store_3reg,sve_store_4reg"))
	 (const_string "l01_v")]
	(const_string "none")))