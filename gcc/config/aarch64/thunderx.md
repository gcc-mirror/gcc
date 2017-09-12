;; Cavium ThunderX pipeline description
;; Copyright (C) 2014-2017 Free Software Foundation, Inc.
;;
;; Written by Andrew Pinski  <apinski@cavium.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Thunder is a dual-issue processor that can issue all instructions on
;; pipe0 and a subset on pipe1.


(define_automaton "thunderx_main, thunderx_mult, thunderx_divide, thunderx_simd")

(define_cpu_unit "thunderx_pipe0" "thunderx_main")
(define_cpu_unit "thunderx_pipe1" "thunderx_main")
(define_cpu_unit "thunderx_mult" "thunderx_mult")
(define_cpu_unit "thunderx_divide" "thunderx_divide")
(define_cpu_unit "thunderx_simd" "thunderx_simd")

(define_insn_reservation "thunderx_add" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "adc_imm,adc_reg,adr,alu_imm,alu_sreg,alus_imm,alus_sreg,extend,logic_imm,logic_reg,logics_imm,logics_reg,mov_imm,mov_reg"))
  "thunderx_pipe0 | thunderx_pipe1")

(define_insn_reservation "thunderx_shift" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "bfm,bfx,extend,rotate_imm,shift_imm,shift_reg,rbit,rev"))
  "thunderx_pipe0 | thunderx_pipe1")


;; Arthimentic instructions with an extra shift or extend is two cycles.
;; FIXME: This needs more attributes on aarch64 than what is currently there;
;;    this is conserative for now.
;; Except this is not correct as this is only for !(LSL && shift by 0/1/2/3)
;; Except this is not correct as this is only for !(zero extend)

(define_insn_reservation "thunderx_arith_shift" 2
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "alu_ext,alu_shift_imm,alu_shift_reg,alus_ext,logic_shift_imm,logic_shift_reg,logics_shift_imm,logics_shift_reg,alus_shift_imm"))
  "thunderx_pipe0 | thunderx_pipe1")

(define_insn_reservation "thunderx_csel" 2
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "csel"))
  "thunderx_pipe0 | thunderx_pipe1")

;; Multiply and mulitply accumulate and count leading zeros can only happen on pipe 1

(define_insn_reservation "thunderx_mul" 4
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "mul,muls,mla,mlas,clz,smull,umull,smlal,umlal"))
  "thunderx_pipe1 + thunderx_mult")

;; crcb,crch,crcw is 4 cycles and can only happen on pipe 1

(define_insn_reservation "thunderx_crc32" 4
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "crc"))
  "thunderx_pipe1 + thunderx_mult")

;; crcx is 5 cycles and only happen on pipe 1
;(define_insn_reservation "thunderx_crc64" 5
;  (and (eq_attr "tune" "thunderx")
;       (eq_attr "type" "crc")
;       (eq_attr "mode" "DI"))
;  "thunderx_pipe1 + thunderx_mult")

(define_insn_reservation "thunderx_div32" 22
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "udiv,sdiv"))
  "thunderx_pipe1 + thunderx_divide, thunderx_divide * 21")

;(define_insn_reservation "thunderx_div64" 38
;  (and (eq_attr "tune" "thunderx")
;       (eq_attr "type" "udiv,sdiv")
;       (eq_attr "mode" "DI"))
;  "thunderx_pipe1 + thunderx_divide, thunderx_divide * 34")

;; Stores take one cycle in pipe 0
(define_insn_reservation "thunderx_store" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "store_4"))
  "thunderx_pipe0")

;; Store pair are single issued
(define_insn_reservation "thunderx_storepair" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "store_8,store_16"))
  "thunderx_pipe0 + thunderx_pipe1")

;; Prefetch are single issued
;(define_insn_reservation "thunderx_prefetch" 1
;  (and (eq_attr "tune" "thunderx")
;       (eq_attr "type" "prefetch"))
;  "thunderx_pipe0 + thunderx_pipe1")

;; loads (and load pairs) from L1 take 3 cycles in pipe 0
(define_insn_reservation "thunderx_load" 3
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "load_4, load_8, load_16"))
  "thunderx_pipe0")

(define_insn_reservation "thunderx_brj" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "branch,trap,call"))
  "thunderx_pipe1")

;; FPU

(define_insn_reservation "thunderx_fadd" 4
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "faddd,fadds"))
  "thunderx_pipe1")

(define_insn_reservation "thunderx_fconst" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fconsts,fconstd"))
  "thunderx_pipe1")

;; Moves between fp are 2 cycles including min/max
(define_insn_reservation "thunderx_fmov" 2
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fmov,f_minmaxs,f_minmaxd"))
  "thunderx_pipe1")

;; ABS, and NEG are 1 cycle
(define_insn_reservation "thunderx_fabs" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "ffariths,ffarithd"))
  "thunderx_pipe1")

(define_insn_reservation "thunderx_fcsel" 3
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fcsel"))
  "thunderx_pipe1")

(define_insn_reservation "thunderx_fmovgpr" 2
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "f_mrc, f_mcr"))
  "thunderx_pipe1")

(define_insn_reservation "thunderx_fcmp" 3
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fcmps,fcmpd,fccmps,fccmpd"))
  "thunderx_pipe1")

(define_insn_reservation "thunderx_fmul" 6
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fmacs,fmacd,fmuls,fmuld"))
  "thunderx_pipe1")

(define_insn_reservation "thunderx_fdivs" 12
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fdivs"))
  "thunderx_pipe1 + thunderx_divide, thunderx_divide*8")

(define_insn_reservation "thunderx_fdivd" 22
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fdivd"))
  "thunderx_pipe1 + thunderx_divide, thunderx_divide*18")

(define_insn_reservation "thunderx_fsqrts" 17
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fsqrts"))
  "thunderx_pipe1 + thunderx_divide, thunderx_divide*13")

(define_insn_reservation "thunderx_fsqrtd" 31
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "fsqrtd"))
  "thunderx_pipe1 + thunderx_divide, thunderx_divide*27")

;; The rounding conversion inside fp is 4 cycles
(define_insn_reservation "thunderx_frint" 4
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "f_cvt,f_rints,f_rintd"))
  "thunderx_pipe1")

;; Float to integer with a move from int to/from float is 6 cycles
(define_insn_reservation "thunderx_f_cvt" 6
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "f_cvtf2i,f_cvti2f"))
  "thunderx_pipe1")

;; FP/SIMD load/stores happen in pipe 0
;; 64bit Loads register/pairs are 4 cycles from L1
(define_insn_reservation "thunderx_64simd_fp_load" 4
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "f_loadd,f_loads,neon_load1_1reg,\
			neon_load1_1reg_q,neon_load1_2reg"))
  "thunderx_pipe0")

;; 128bit load pair is singled issue and 4 cycles from L1
(define_insn_reservation "thunderx_128simd_pair_load" 4
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_load1_2reg_q"))
  "thunderx_pipe0+thunderx_pipe1")

;; FP/SIMD Stores takes one cycle in pipe 0
;; ST1 with one registers either multiple structures or single structure is
;;    also one cycle.
(define_insn_reservation "thunderx_simd_fp_store" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "f_stored,f_stores,neon_store1_1reg,neon_store1_1reg_q, \
			neon_store1_one_lane, neon_store1_one_lane_q"))
  "thunderx_pipe0")

;; 64bit neon store pairs are single issue for one cycle
(define_insn_reservation "thunderx_64neon_storepair" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_store1_2reg"))
  "thunderx_pipe0 + thunderx_pipe1")

;; 128bit neon store pair are single issued for two cycles
(define_insn_reservation "thunderx_128neon_storepair" 2
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_store1_2reg_q"))
  "(thunderx_pipe0 + thunderx_pipe1)*2")

;; LD1R/LD1 (with a single struct) takes 6 cycles and issued in pipe0
(define_insn_reservation "thunderx_neon_ld1" 6
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_load1_all_lanes"))
  "thunderx_pipe0")

;; SIMD/NEON (q forms take an extra cycle)
;; SIMD For ThunderX is 64bit wide,

;; ThunderX simd move instruction types - 2/3 cycles
;; ThunderX dup, ins is the same
;; ThunderX SIMD fabs/fneg instruction types
(define_insn_reservation "thunderx_neon_move" 2
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_logic, neon_bsl, neon_fp_compare_s, \
			neon_fp_compare_d, neon_move, neon_dup, \
			neon_ins, neon_from_gp, neon_to_gp, \
			neon_abs, neon_neg, \
			neon_fp_neg_s, neon_fp_abs_s"))
  "thunderx_pipe1 + thunderx_simd")

(define_insn_reservation "thunderx_neon_move_q" 3
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_logic_q, neon_bsl_q, neon_fp_compare_s_q, \
			neon_fp_compare_d_q, neon_move_q, neon_dup_q, \
			neon_ins_q, neon_from_gp_q, neon_to_gp_q, \
			neon_abs_q, neon_neg_q, \
			neon_fp_neg_s_q, neon_fp_neg_d_q, \
			neon_fp_abs_s_q, neon_fp_abs_d_q"))
  "thunderx_pipe1 + thunderx_simd, thunderx_simd")

;; ThunderX simd simple/add instruction types - 4/5 cycles

(define_insn_reservation "thunderx_neon_add" 4
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_reduc_add, neon_reduc_minmax, neon_fp_reduc_add_s, \
			neon_fp_reduc_add_d, neon_fp_to_int_s, neon_fp_to_int_d, \
			neon_add_halve, neon_sub_halve, neon_qadd, neon_compare, \
			neon_compare_zero, neon_minmax, neon_abd, neon_add, neon_sub, \
			neon_fp_minmax_s, neon_fp_minmax_d, neon_reduc_add, neon_cls, \
			neon_qabs, neon_qneg, neon_fp_addsub_s, neon_fp_addsub_d, \
			neon_arith_acc, neon_rev, neon_fp_abd_s, neon_fp_abd_d, \
			neon_fp_reduc_minmax_s"))
  "thunderx_pipe1 + thunderx_simd")

;; BIG NOTE: neon_add_long/neon_sub_long don't have a q form which is incorrect

(define_insn_reservation "thunderx_neon_add_q" 5
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_reduc_add_q, neon_reduc_minmax_q, neon_fp_reduc_add_s_q, \
			neon_fp_reduc_add_d_q, neon_fp_to_int_s_q, neon_fp_to_int_d_q, \
			neon_add_halve_q, neon_sub_halve_q, neon_qadd_q, neon_compare_q, \
			neon_compare_zero_q, neon_minmax_q, neon_abd_q, neon_add_q, neon_sub_q, \
			neon_fp_minmax_s_q, neon_fp_minmax_d_q, neon_reduc_add_q, neon_cls_q, \
			neon_qabs_q, neon_qneg_q, neon_fp_addsub_s_q, neon_fp_addsub_d_q, \
			neon_add_long, neon_sub_long, neon_fp_abd_s_q, neon_fp_abd_d_q, \
			neon_arith_acc_q, neon_rev_q, \
			neon_fp_reduc_minmax_s_q, neon_fp_reduc_minmax_d_q"))
  "thunderx_pipe1 + thunderx_simd, thunderx_simd")

;; Multiplies (float and integer) and shifts and permutes (except for TBL) and float conversions
;; are 6/7 cycles
(define_insn_reservation "thunderx_neon_mult" 6
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_fp_mul_s, neon_fp_mul_d, neon_fp_mla_s, neon_fp_mla_d, \
			neon_mla_b, neon_mla_h, neon_mla_s, \
			neon_mla_h_scalar, neon_mla_s_scalar, \
			neon_ext, neon_shift_imm, neon_permute, \
			neon_int_to_fp_s, neon_int_to_fp_d, neon_shift_reg, \
			neon_sat_shift_reg, neon_shift_acc, \
			neon_mul_b, neon_mul_h, neon_mul_s, \
			neon_mul_h_scalar, neon_mul_s_scalar, \
			neon_fp_mul_s_scalar, \
			neon_fp_mla_s_scalar"))
  "thunderx_pipe1 + thunderx_simd")

(define_insn_reservation "thunderx_neon_mult_q" 7
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_fp_mul_s_q, neon_fp_mul_d_q, neon_fp_mla_s_q, neon_fp_mla_d_q, \
			neon_mla_b_q, neon_mla_h_q, neon_mla_s_q, \
			neon_mla_h_scalar_q, neon_mla_s_scalar_q, \
			neon_ext_q, neon_shift_imm_q, neon_permute_q, \
			neon_int_to_fp_s_q, neon_int_to_fp_d_q, neon_shift_reg_q, \
			neon_sat_shift_reg_q, neon_shift_acc_q, \
			neon_shift_imm_long, \
			neon_mul_b_q, neon_mul_h_q, neon_mul_s_q, \
			neon_mul_h_scalar_q, neon_mul_s_scalar_q, \
			neon_fp_mul_s_scalar_q, neon_fp_mul_d_scalar_q, \
			neon_mul_b_long, neon_mul_h_long, neon_mul_s_long, \
			neon_shift_imm_narrow_q, neon_fp_cvt_widen_s, neon_fp_cvt_narrow_d_q, \
			neon_fp_mla_s_scalar_q, neon_fp_mla_d_scalar_q"))
  "thunderx_pipe1 + thunderx_simd, thunderx_simd")


;; AES[ED] is 5 cycles
(define_insn_reservation "thunderx_crypto_aese" 5
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "crypto_aese"))
  "thunderx_pipe1 + thunderx_simd, thunderx_simd")

;; AES{,I}MC is 3 cycles
(define_insn_reservation "thunderx_crypto_aesmc" 3
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "crypto_aesmc"))
  "thunderx_pipe1 + thunderx_simd, thunderx_simd")


;; Thunder 128bit SIMD reads the upper halve in cycle 2 and writes upper halve in the last cycle
(define_bypass 2 "thunderx_neon_move_q" "thunderx_neon_move_q, thunderx_neon_add_q, thunderx_neon_mult_q")
(define_bypass 4 "thunderx_neon_add_q" "thunderx_neon_move_q, thunderx_neon_add_q, thunderx_neon_mult_q")
(define_bypass 6 "thunderx_neon_mult_q" "thunderx_neon_move_q, thunderx_neon_add_q, thunderx_neon_mult_q")

;; 64bit TBL is emulated and takes 160 cycles
(define_insn_reservation "thunderx_tbl" 160
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_tbl1"))
  "(thunderx_pipe1+thunderx_pipe0)*160")

;; 128bit TBL is emulated and takes 320 cycles
(define_insn_reservation "thunderx_tblq" 320
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "neon_tbl1_q"))
  "(thunderx_pipe1+thunderx_pipe0)*320")

;; Assume both pipes are needed for unknown and multiple-instruction
;; patterns.

(define_insn_reservation "thunderx_unknown" 1
  (and (eq_attr "tune" "thunderx")
       (eq_attr "type" "untyped,multiple"))
  "thunderx_pipe0 + thunderx_pipe1")


