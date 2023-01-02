;; Machine description for AppliedMicro xgene1 core.
;; Copyright (C) 2012-2023 Free Software Foundation, Inc.
;; Contributed by Theobroma Systems Design und Consulting GmbH.
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

;; Pipeline description for the xgene1 micro-architecture

(define_automaton "xgene1_main, xgene1_decoder, xgene1_div, xgene1_simd")

(define_cpu_unit "xgene1_decode_out0" "xgene1_decoder")
(define_cpu_unit "xgene1_decode_out1" "xgene1_decoder")
(define_cpu_unit "xgene1_decode_out2" "xgene1_decoder")
(define_cpu_unit "xgene1_decode_out3" "xgene1_decoder")

(define_cpu_unit "xgene1_IXA" "xgene1_main")
(define_cpu_unit "xgene1_IXB" "xgene1_main")
(define_cpu_unit "xgene1_IXB_compl" "xgene1_main")

(define_reservation "xgene1_IXn" "(xgene1_IXA | xgene1_IXB)")

(define_cpu_unit "xgene1_multiply" "xgene1_main")
(define_cpu_unit "xgene1_divide" "xgene1_div")
(define_cpu_unit "xgene1_fp_divide" "xgene1_div")
(define_cpu_unit "xgene1_fsu" "xgene1_simd")
(define_cpu_unit "xgene1_fcmp" "xgene1_simd")
(define_cpu_unit "xgene1_ld" "xgene1_main")
(define_cpu_unit "xgene1_st" "xgene1_main")

(define_reservation "xgene1_decode1op"
        "( xgene1_decode_out0 )
        |( xgene1_decode_out1 )
        |( xgene1_decode_out2 )
        |( xgene1_decode_out3 )"
)
(define_reservation "xgene1_decode2op"
        "( xgene1_decode_out0 + xgene1_decode_out1 )
        |( xgene1_decode_out0 + xgene1_decode_out2 )
        |( xgene1_decode_out0 + xgene1_decode_out3 )
        |( xgene1_decode_out1 + xgene1_decode_out2 )
        |( xgene1_decode_out1 + xgene1_decode_out3 )
        |( xgene1_decode_out2 + xgene1_decode_out3 )"
)
(define_reservation "xgene1_decodeIsolated"
        "( xgene1_decode_out0 + xgene1_decode_out1 + xgene1_decode_out2 + xgene1_decode_out3 )"
)

(define_insn_reservation "xgene1_branch" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "branch"))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_call" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "call"))
  "xgene1_decode2op")

(define_insn_reservation "xgene1_f_load" 10
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_loadd,f_loads"))
  "xgene1_decode2op, xgene1_ld")

(define_insn_reservation "xgene1_f_store" 4
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_stored,f_stores"))
  "xgene1_decode2op, xgene1_st")

(define_insn_reservation "xgene1_fmov" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "fmov,fconsts,fconstd"))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_f_mcr" 10
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_mcr"))
  "xgene1_decodeIsolated")

(define_insn_reservation "xgene1_f_mrc" 4
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_mrc"))
  "xgene1_decode2op")

(define_insn_reservation "xgene1_load_pair" 6
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "load_16"))
  "xgene1_decodeIsolated, xgene1_ld*2")

(define_insn_reservation "xgene1_store_pair" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "store_16"))
  "xgene1_decodeIsolated, xgene1_st*2")

(define_insn_reservation "xgene1_fp_load1" 10
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "load_4, load_8")
       (eq_attr "fp" "yes"))
  "xgene1_decode1op, xgene1_ld")

(define_insn_reservation "xgene1_load1" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "load_4, load_8"))
  "xgene1_decode1op, xgene1_ld")

(define_insn_reservation "xgene1_store1" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "store_4, store_8"))
  "xgene1_decode1op, xgene1_st")

(define_insn_reservation "xgene1_move" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "mov_reg,mov_imm,mrs"))
  "xgene1_decode1op, xgene1_IXn")

(define_insn_reservation "xgene1_alu_cond" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "csel"))
  "xgene1_decode1op, xgene1_IXn")

(define_insn_reservation "xgene1_alu" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "alu_imm,alu_sreg,alu_shift_imm_lsl_1to4,alu_shift_imm_other,\
                        alu_ext,adc_reg,logic_imm,\
                        logic_reg,logic_shift_imm,clz,\
                        rbit,adr,mov_reg,shift_imm,\
                        mov_imm,extend,multiple"))
  "xgene1_decode1op, xgene1_IXn")

(define_insn_reservation "xgene1_shift_rotate" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "shift_reg"))
  "xgene1_decode1op, xgene1_IXB, xgene1_IXB_compl")

(define_insn_reservation "xgene1_simd" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "rev"))
  "xgene1_decode1op, xgene1_IXB, xgene1_IXB_compl")

(define_insn_reservation "xgene1_alus" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "alus_imm,alus_sreg,alus_shift_imm,\
                        alus_ext,logics_imm,logics_reg,\
                        logics_shift_imm"))
  "xgene1_decode1op, xgene1_IXB, xgene1_IXB_compl")

(define_bypass 2 "xgene1_alus"
  "xgene1_alu_cond, xgene1_branch")

(define_insn_reservation "xgene1_mul32" 4
  (and (eq_attr "tune" "xgene1")
       (eq_attr "mul32" "yes"))
  "xgene1_decode2op, xgene1_IXB + xgene1_multiply, xgene1_multiply, nothing, xgene1_IXB_compl")

(define_insn_reservation "xgene1_widen_mul64" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "widen_mul64" "yes"))
  "xgene1_decode2op, xgene1_IXB + xgene1_multiply, xgene1_multiply, nothing*2, xgene1_IXB_compl")

(define_insn_reservation "xgene1_div" 34
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "sdiv,udiv"))
  "xgene1_decode1op, xgene1_IXB + xgene1_divide*7")

(define_insn_reservation "xgene1_fcmp" 10
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "fcmpd,fcmps,fccmpd,fccmps"))
  "xgene1_decode1op, xgene1_fsu + xgene1_fcmp*3")

(define_insn_reservation "xgene1_fcsel" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "fcsel"))
  "xgene1_decode1op, xgene1_fsu")

(define_insn_reservation "xgene1_bfx" 1
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "bfx"))
  "xgene1_decode1op, xgene1_IXn")

(define_insn_reservation "xgene1_bfm" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "bfm"))
  "xgene1_decode1op, xgene1_IXB, xgene1_IXB_compl")

(define_insn_reservation "xgene1_f_rint" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_rintd,f_rints"))
  "xgene1_decode1op, xgene1_fsu")

(define_insn_reservation "xgene1_f_cvt" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_cvt"))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_f_cvtf2i" 11
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_cvtf2i"))
  "xgene1_decodeIsolated,xgene1_fsu")

(define_insn_reservation "xgene1_f_cvti2f" 14
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_cvti2f"))
  "xgene1_decodeIsolated,xgene1_fsu")

(define_insn_reservation "xgene1_f_add" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "faddd,fadds,fmuld,fmuls"))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_f_divs" 22
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "fdivs,fsqrts"))
  "xgene1_decode1op,(xgene1_fp_divide+xgene1_fsu)*8,xgene1_fp_divide*14")

(define_insn_reservation "xgene1_f_divd" 28
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "fdivd"))
  "xgene1_decode1op,(xgene1_fp_divide+xgene1_fsu)*11,xgene1_fp_divide*17")

(define_insn_reservation "xgene1_f_sqrtd" 28
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "fsqrtd"))
  "xgene1_decode1op,(xgene1_fp_divide+xgene1_fsu)*17,xgene1_fp_divide*11")

(define_insn_reservation "xgene1_f_arith" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "ffarithd,ffariths"))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_f_select" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "f_minmaxd,f_minmaxs"))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_neon_dup" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_dup,neon_dup_q"))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_neon_load1" 11
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_load1_1reg, neon_load1_1reg_q"))
  "xgene1_decode2op, xgene1_ld")

(define_insn_reservation "xgene1_neon_store1" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_store1_1reg, neon_store1_1reg_q"))
  "xgene1_decode2op, xgene1_st")

(define_insn_reservation "xgene1_neon_logic" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_logic,\
                        neon_logic_q,\
                        neon_bsl,\
                        neon_bsl_q,\
                        neon_move,\
                        neon_move_q,\
                       "))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_neon_umov" 7
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_to_gp, neon_to_gp_q"))
  "xgene1_decodeIsolated")

(define_insn_reservation "xgene1_neon_ins" 14
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_from_gp,\
                        neon_from_gp_q,\
                        neon_ins,\
                        neon_ins_q,\
                       "))
  "xgene1_decodeIsolated,xgene1_fsu")

(define_insn_reservation "xgene1_neon_shift" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_shift_imm,\
                        neon_shift_imm_q,\
                        neon_shift_reg,\
                        neon_shift_reg_q,\
                        neon_shift_imm_long,\
                        neon_sat_shift_imm,\
                        neon_sat_shift_imm_q,\
                        neon_sat_shift_imm_narrow_q,\
                        neon_sat_shift_reg,\
                        neon_sat_shift_reg_q,\
                        neon_shift_imm_narrow_q,\
                       "))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_neon_arith" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_add,\
                        neon_add_q,\
                        neon_sub,\
                        neon_sub_q,\
                        neon_neg,\
                        neon_neg_q,\
                        neon_abs,\
                        neon_abs_q,\
                        neon_abd_q,\
                        neon_arith_acc,\
                        neon_arith_acc_q,\
                        neon_reduc_add,\
                        neon_reduc_add_q,\
                        neon_add_halve,\
                        neon_add_halve_q,\
                        neon_sub_halve,\
                        neon_sub_halve_q,\
                        neon_qadd,\
                        neon_qadd_q,\
                        neon_compare,\
                        neon_compare_q,\
                        neon_compare_zero,\
                        neon_compare_zero_q,\
                        neon_tst,\
                        neon_tst_q,\
                        neon_minmax,\
                        neon_minmax_q,\
                       "))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_neon_abs_diff" 6
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_arith_acc,neon_arith_acc_q"))
  "xgene1_decode2op,xgene1_fsu*2")

(define_insn_reservation "xgene1_neon_mul" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_mul_b,\
                        neon_mul_b_q,\
                        neon_mul_h,\
                        neon_mul_h_q,\
                        neon_mul_s,\
                        neon_mul_s_q,\
                        neon_fp_mul_s_scalar,\
                        neon_fp_mul_s_scalar_q,\
                        neon_fp_mul_d_scalar_q,\
                        neon_mla_b,neon_mla_b_q,\
                        neon_mla_h,neon_mla_h_q,\
                        neon_mla_s,neon_mla_s_q,\
                        neon_mla_h_scalar,\
                        neon_mla_h_scalar_q,\
                        neon_mla_s_scalar,\
                        neon_mla_s_scalar_q,\
                        neon_mla_b_long,\
                        neon_mla_h_long,\
                        neon_mla_s_long,\
                        neon_fp_mul_s,\
                        neon_fp_mul_s_q,\
                        neon_fp_mul_d,\
                        neon_fp_mul_d_q,\
                        neon_fp_mla_s,\
                        neon_fp_mla_s_q,\
                        neon_fp_mla_d,\
                        neon_fp_mla_d_q,\
                        neon_fp_mla_s_scalar,\
                        neon_fp_mla_s_scalar_q,\
                        neon_fp_mla_d_scalar_q,\
                        neon_sat_mul_b,\
                        neon_sat_mul_b_q,\
                        neon_sat_mul_h,\
                        neon_sat_mul_h_q,\
                        neon_sat_mul_s,\
                        neon_sat_mul_s_q,\
                        neon_sat_mul_h_scalar,\
                        neon_sat_mul_h_scalar_q,\
                        neon_sat_mul_s_scalar,\
                        neon_sat_mul_s_scalar_q,\
                        neon_sat_mul_h_scalar_long,\
                        neon_sat_mul_s_scalar_long,\
                        neon_sat_mla_b_long,\
                        neon_sat_mla_h_long,\
                        neon_sat_mla_s_long,\
                        neon_sat_mla_h_scalar_long,\
                        neon_sat_mla_s_scalar_long,\
                       "))
  "xgene1_decode2op,xgene1_fsu*2")

(define_insn_reservation "xgene1_fp_abd_diff" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_abd_s,\
                        neon_fp_abd_s_q,\
                        neon_fp_abd_d,\
                        neon_fp_abd_d_q,\
                       "))
  "xgene1_decode1op,xgene1_fsu")

(define_insn_reservation "xgene1_neon_f_add" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_addsub_s,\
                        neon_fp_addsub_s_q,\
                        neon_fp_addsub_d,\
                        neon_fp_addsub_d_q,\
                       "))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_f_div" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_div_s,\
                        neon_fp_div_s_q,\
                        neon_fp_div_d,\
                        neon_fp_div_d_q,\
                       "))
  "xgene1_decode1op,(xgene1_fsu+xgene1_fp_divide)")

(define_insn_reservation "xgene1_neon_f_neg" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_neg_s,\
                        neon_fp_neg_s_q,\
                        neon_fp_neg_d,\
                        neon_fp_neg_d_q,\
                        neon_fp_abs_s,\
                        neon_fp_abs_s_q,\
                        neon_fp_abs_d,\
                        neon_fp_abs_d_q,\
                       "))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_f_round" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_round_s,\
                        neon_fp_round_s_q,\
                        neon_fp_round_d,\
                        neon_fp_round_d_q,\
                       "))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_f_cvt" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type"  "neon_int_to_fp_s,\
                         neon_int_to_fp_s_q,\
                         neon_int_to_fp_d,\
                         neon_int_to_fp_d_q,\
                         neon_fp_cvt_widen_s,\
                         neon_fp_cvt_narrow_s_q,\
                         neon_fp_cvt_narrow_d_q,\
                        "))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_f_reduc" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_reduc_add_s,\
                        neon_fp_reduc_add_s_q,\
                        neon_fp_reduc_add_d,\
                        neon_fp_reduc_add_d_q,\
                       "))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_cls" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_cls,neon_cls_q"))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_st1" 4
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_store1_one_lane,\
                        neon_store1_one_lane_q,\
                        neon_stp,\
                        neon_stp_q,\
                       "))
  "xgene1_decodeIsolated, xgene1_st")

(define_insn_reservation "xgene1_neon_halve_narrow" 6
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_sub_halve_narrow_q,\
                        neon_add_halve_narrow_q,\
                       "))
  "xgene1_decodeIsolated")

(define_insn_reservation "xgene1_neon_shift_acc" 6
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_shift_acc,\
                        neon_shift_acc_q,\
                       "))
  "xgene1_decode2op")

(define_insn_reservation "xgene1_neon_fp_compare" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_compare_s,\
                        neon_fp_compare_s_q,\
                        neon_fp_compare_d,\
                        neon_fp_compare_d_q,\
                       "))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_fp_sqrt" 2
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_sqrt_s,\
                        neon_fp_sqrt_s_q,\
                        neon_fp_sqrt_d,\
                        neon_fp_sqrt_d_q,\
                       "))
  "xgene1_decode1op,(xgene1_fsu+xgene1_fp_divide)")

(define_insn_reservation "xgene1_neon_tbl1" 4
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_tbl1,\
                        neon_tbl1_q,\
                       "))
  "xgene1_decode2op")

(define_insn_reservation "xgene1_neon_tbl2" 8
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_tbl2,\
                        neon_tbl2_q,\
                       "))
  "xgene1_decodeIsolated")

(define_insn_reservation "xgene1_neon_permute" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_permute,\
                        neon_permute_q,\
                       "))
  "xgene1_decode2op")

(define_insn_reservation "xgene1_neon_ld1r" 10
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_load1_all_lanes,\
                       "))
  "xgene1_decode1op, xgene1_ld")

(define_insn_reservation "xgene1_neon_fp_recp" 3
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_recpe_s,\
                        neon_fp_recpe_s_q,\
                        neon_fp_recpe_d,\
                        neon_fp_recpe_d_q,\
                        neon_fp_recpx_s,\
                        neon_fp_recpx_s_q,\
                        neon_fp_recpx_d,\
                        neon_fp_recpx_d_q,\
                       "))
  "xgene1_decode1op")


(define_insn_reservation "xgene1_neon_fp_recp_s" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_fp_recps_s,\
                        neon_fp_recps_s_q,\
                        neon_fp_recps_d,\
                        neon_fp_recps_d_q,\
                       "))
  "xgene1_decode1op")

(define_insn_reservation "xgene1_neon_pmull" 5
  (and (eq_attr "tune" "xgene1")
       (eq_attr "type" "neon_mul_d_long,\
			crypto_pmull,\
		       "))
  "xgene1_decode2op")
