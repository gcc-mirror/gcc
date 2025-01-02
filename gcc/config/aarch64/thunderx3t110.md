;; Cavium ThunderX 3 CN11xx pipeline description
;; Copyright (C) 2020-2025 Free Software Foundation, Inc.
;;
;; Contributed by Marvell

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

(define_automaton "thunderx3t110, thunderx3t110_advsimd, thunderx3t110_ldst")
(define_automaton "thunderx3t110_mult")

(define_cpu_unit "thunderx3t110_i0" "thunderx3t110")
(define_cpu_unit "thunderx3t110_i1" "thunderx3t110")
(define_cpu_unit "thunderx3t110_i2" "thunderx3t110")
(define_cpu_unit "thunderx3t110_i3" "thunderx3t110")

(define_cpu_unit "thunderx3t110_ls0" "thunderx3t110_ldst")
(define_cpu_unit "thunderx3t110_ls1" "thunderx3t110_ldst")
(define_cpu_unit "thunderx3t110_sd" "thunderx3t110_ldst")

; Pseudo-units for multiply pipeline.
; unchanged from TX2, occupies I1 for four (1 + 3 additional) slots

(define_cpu_unit "thunderx3t110_i1m1" "thunderx3t110_mult")
(define_cpu_unit "thunderx3t110_i1m2" "thunderx3t110_mult")
(define_cpu_unit "thunderx3t110_i1m3" "thunderx3t110_mult")

; Pseudo-units for load delay (assuming dcache hit).

(define_cpu_unit "thunderx3t110_ls0d1" "thunderx3t110_ldst")
(define_cpu_unit "thunderx3t110_ls0d2" "thunderx3t110_ldst")
(define_cpu_unit "thunderx3t110_ls0d3" "thunderx3t110_ldst")

(define_cpu_unit "thunderx3t110_ls1d1" "thunderx3t110_ldst")
(define_cpu_unit "thunderx3t110_ls1d2" "thunderx3t110_ldst")
(define_cpu_unit "thunderx3t110_ls1d3" "thunderx3t110_ldst")

; Define FP units f0/f1/f2/f3.
(define_cpu_unit "thunderx3t110_f0" "thunderx3t110_advsimd")
(define_cpu_unit "thunderx3t110_f1" "thunderx3t110_advsimd")
(define_cpu_unit "thunderx3t110_f2" "thunderx3t110_advsimd")
(define_cpu_unit "thunderx3t110_f3" "thunderx3t110_advsimd")

(define_reservation "thunderx3t110_i23" "thunderx3t110_i2|thunderx3t110_i3")
(define_reservation "thunderx3t110_i01"
    "thunderx3t110_i0|thunderx3t110_i1")
(define_reservation "thunderx3t110_i012"
    "thunderx3t110_i0|thunderx3t110_i1|thunderx3t110_i2")
(define_reservation "thunderx3t110_i0123"
    "thunderx3t110_i0|thunderx3t110_i1|thunderx3t110_i2|thunderx3t110_i3")
(define_reservation "thunderx3t110_ls01" "thunderx3t110_ls0|thunderx3t110_ls1")
(define_reservation "thunderx3t110_f01" "thunderx3t110_f0|thunderx3t110_f1")
(define_reservation "thunderx3t110_f23" "thunderx3t110_f2|thunderx3t110_f3")
(define_reservation "thunderx3t110_f0123"
    "thunderx3t110_f0|thunderx3t110_f1|thunderx3t110_f2|thunderx3t110_f3")

; A load with delay in the ls0/ls1 pipes.
; this is always a delay of four
(define_reservation "thunderx3t110_l0delay"
    "thunderx3t110_ls0,thunderx3t110_ls0d1,thunderx3t110_ls0d2,\
     thunderx3t110_ls0d3")
(define_reservation "thunderx3t110_l1delay"
    "thunderx3t110_ls1,thunderx3t110_ls1d1,thunderx3t110_ls1d2,\
     thunderx3t110_ls1d3")
(define_reservation "thunderx3t110_l01delay"
    "thunderx3t110_l0delay|thunderx3t110_l1delay")
;; Branch and call instructions.

(define_insn_reservation "thunderx3t110_branch" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "call,branch,trap"))
  "thunderx3t110_i23")

;; Misc instructions.

; Speculation barrier
(define_insn_reservation "thunderx3t110_nothing" 0
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "block"))
  "nothing")

(define_insn_reservation "thunderx3t110_mrs" 0
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "mrs"))
  "thunderx3t110_i2")

(define_insn_reservation "thunderx3t110_multiple" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "multiple"))
  "thunderx3t110_i0+thunderx3t110_i1+thunderx3t110_i3+thunderx3t110_ls0+\
   thunderx3t110_ls1+thunderx3t110_sd+thunderx3t110_i1m1+thunderx3t110_i1m2+\
   thunderx3t110_i1m3+thunderx3t110_f0+thunderx3t110_f1")

;; Integer arithmetic/logic instructions.

; Plain register moves are handled by renaming,
; and don't create any uops.
(define_insn_reservation "thunderx3t110_regmove" 0
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "mov_reg"))
  "nothing")

(define_insn_reservation "thunderx3t110_alu_basic" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "alu_imm,alu_sreg,alus_imm,alus_sreg,\
			adc_reg,adc_imm,adcs_reg,adcs_imm,\
			logic_reg,logic_imm,logics_reg,logics_imm,\
			csel,adr,mov_imm,shift_reg,shift_imm,bfm,\
			bfx,rbit,rev,extend,rotate_imm"))
  "thunderx3t110_i0123")

; distinguish between latency 1|2 and throughput 1/4|2/4?
; is it actually 1,1/2,{i0,i1} vs 2,1/4,{i0,i1,i2,i3}
(define_insn_reservation "thunderx3t110_alu_shift" 2
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "alu_shift_imm_lsl_1to4,alu_shift_imm_other,alu_ext,\
			alus_shift_imm,alus_ext,\
			logic_shift_imm,logics_shift_imm"))
  "thunderx3t110_i0123")

(define_insn_reservation "thunderx3t110_alu_shift1" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "alu_shift_imm_lsl_1to4,alu_shift_imm_other,alu_ext,\
			alus_shift_imm,alus_ext,\
			logic_shift_imm,logics_shift_imm"))
  "thunderx3t110_i01")

; we are going for the optimistic answer (13)
; for now, the worst case is 23
(define_insn_reservation "thunderx3t110_div" 13
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "sdiv,udiv"))
  "thunderx3t110_i1*3")

(define_insn_reservation "thunderx3t110_madd" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "mla,smlal,umlal"))
  "thunderx3t110_i0123,thunderx3t110_i1m1,thunderx3t110_i1m2,thunderx3t110_i1m3,\
   thunderx3t110_i012")

; NOTE: smull, umull are used for "high part" multiplies too.
; mul is alias for MADD
; it has to be distinguished between smulh, umulh (4,1) and
; other (5,1) but there is no such a type, so, we go for the
; conservative approach of (5,1) for now
; smulh, umulh only runs on I1
(define_insn_reservation "thunderx3t110_mul" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "mul,smull,umull"))
  "thunderx3t110_i0123,thunderx3t110_i1m1,thunderx3t110_i1m2,thunderx3t110_i1m3")

(define_insn_reservation "thunderx3t110_countbits" 3
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "clz"))
  "thunderx3t110_i1")

;; Integer loads and stores.

; load_4 matches prefetch, a multitude of move/str/dup variants,
; sign extend
(define_insn_reservation "thunderx3t110_load_basic" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "load_4"))
  "thunderx3t110_ls01")

; model use of I0/I1/I2 for index versions only, model 4|8 2nd on load
(define_insn_reservation "thunderx3t110_loadpair" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "load_8,load_16"))
  "thunderx3t110_i012,thunderx3t110_ls01")

(define_insn_reservation "thunderx3t110_store_basic" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "store_4"))
  "thunderx3t110_ls01,thunderx3t110_sd")

; model use of I0/I1/I2/I3 for index versions, model differing
; throughputs
(define_insn_reservation "thunderx3t110_storepair_basic" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "store_8,store_16"))
  "thunderx3t110_ls01,thunderx3t110_sd")

;; FP data processing instructions.

(define_insn_reservation "thunderx3t110_fp_simple" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "ffariths,ffarithd,f_minmaxs,f_minmaxd"))
  "thunderx3t110_f0123")

; distinguish latency 3/4 throughput 1/2|1/4
(define_insn_reservation "thunderx3t110_fp_addsub3" 3
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fadds,faddd"))
  "thunderx3t110_f23")
(define_insn_reservation "thunderx3t110_fp_addsub4" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fadds,faddd"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_fp_cmp" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fcmps,fcmpd,fccmps,fccmpd"))
  "thunderx3t110_f0123")

; need to split out latency 23 throughput 23/4: F64 from
; latency 16 throughput  16/4: FDIV F32
(define_insn_reservation "thunderx3t110_fp_divsqrt_s" 16
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fdivs,fsqrts"))
  "thunderx3t110_f0*3|thunderx3t110_f1*3|\
   thunderx3t110_f2*3|thunderx3t110_f3*3")

(define_insn_reservation "thunderx3t110_fp_divsqrt_d" 23
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fdivd,fsqrtd"))
  "thunderx3t110_f0*5|thunderx3t110_f1*5|\
   thunderx3t110_f2*5|thunderx3t110_f3*5")

(define_insn_reservation "thunderx3t110_fp_mul_mac" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fmuls,fmuld,fmacs,fmacd"))
  "thunderx3t110_f01")

(define_insn_reservation "thunderx3t110_frint" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "f_rints,f_rintd"))
  "thunderx3t110_f0123")

; mimic latency 3|4 throughput 1/2|1/4
(define_insn_reservation "thunderx3t110_fcsel3" 3
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fcsel"))
  "thunderx3t110_f23")

(define_insn_reservation "thunderx3t110_fcsel4" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fcsel"))
  "thunderx3t110_f0123")

;; FP miscellaneous instructions.

(define_insn_reservation "thunderx3t110_fp_cvt" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "f_cvtf2i,f_cvt,f_cvti2f"))
  "thunderx3t110_f0123")

; even though f_mrc has to belong to fp_mov_to_gen
; we retain this for the sake of legacy as codegen
; doesn't use it anyway
(define_insn_reservation "thunderx3t110_fp_mov3" 3
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fconsts,fconstd,fmov,f_mrc"))
  "thunderx3t110_f23")

(define_insn_reservation "thunderx3t110_fp_mov" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "fconsts,fconstd,fmov,f_mrc"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_fp_mov_to_gen" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "f_mcr"))
  "thunderx3t110_f0123")

;; FP loads and stores.
;  model use of I0/I1/I2 for post/pre index modes

(define_insn_reservation "thunderx3t110_fp_load_basic" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "f_loads,f_loadd"))
  "thunderx3t110_ls01")

; model throughput 1
(define_insn_reservation "thunderx3t110_fp_store_basic" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "f_stores,f_stored"))
  "thunderx3t110_ls01,thunderx3t110_sd")

;; ASIMD integer instructions.

(define_insn_reservation "thunderx3t110_asimd_int" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_abd,neon_abd_q,\
			neon_arith_acc,neon_arith_acc_q,\
			neon_abs,neon_abs_q,\
			neon_add,neon_add_q,\
			neon_sub,neon_sub_q,\
			neon_neg,neon_neg_q,\
			neon_add_long,neon_add_widen,\
			neon_add_halve,neon_add_halve_q,\
			neon_sub_long,neon_sub_widen,\
			neon_sub_halve,neon_sub_halve_q,\
			neon_add_halve_narrow_q,neon_sub_halve_narrow_q,\
			neon_qabs,neon_qabs_q,\
			neon_qadd,neon_qadd_q,\
			neon_qneg,neon_qneg_q,\
			neon_qsub,neon_qsub_q,\
			neon_minmax,neon_minmax_q,\
			neon_reduc_minmax,neon_reduc_minmax_q,\
			neon_mul_b,neon_mul_h,neon_mul_s,\
			neon_mul_b_q,neon_mul_h_q,neon_mul_s_q,\
			neon_sat_mul_b,neon_sat_mul_h,neon_sat_mul_s,\
			neon_sat_mul_b_q,neon_sat_mul_h_q,neon_sat_mul_s_q,\
			neon_mla_b,neon_mla_h,neon_mla_s,\
			neon_mla_b_q,neon_mla_h_q,neon_mla_s_q,\
			neon_mul_b_long,neon_mul_h_long,\
			neon_mul_s_long,neon_mul_d_long,\
			neon_sat_mul_b_long,neon_sat_mul_h_long,\
			neon_sat_mul_s_long,\
			neon_mla_b_long,neon_mla_h_long,neon_mla_s_long,\
			neon_sat_mla_b_long,neon_sat_mla_h_long,\
			neon_sat_mla_s_long,\
			neon_shift_acc,neon_shift_acc_q,\
			neon_shift_imm,neon_shift_imm_q,\
			neon_shift_reg,neon_shift_reg_q,\
			neon_shift_imm_long,neon_shift_imm_narrow_q,\
			neon_sat_shift_imm,neon_sat_shift_imm_q,\
			neon_sat_shift_reg,neon_sat_shift_reg_q,\
			neon_sat_shift_imm_narrow_q"))
  "thunderx3t110_f0123")

; neon_reduc_add is used for both addp and [su]adalp
(define_insn_reservation "thunderx3t110_asimd_reduc_add" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_reduc_add,neon_reduc_add_q"))
  "thunderx3t110_f01")

(define_insn_reservation "thunderx3t110_asimd_cmp" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_compare,neon_compare_q,neon_compare_zero,\
			neon_tst,neon_tst_q"))
  "thunderx3t110_f0123")

; neon_logic used in ldr, str, mov, umov, fmov, mov; orn; bic; and,
;   simd mov immediate; orr, simd mov immediate; eor; not (mvn)
; latency 4 throughput 1/2 LS0/LS1: ldr
; latency 1 throughput 1 LS0/LS1,SDI,I0/I1/I2: str
; latency 3|4 throughput 1/2|1/4 F2/F3 F0/F1/F2/F3: fmov immed, orn,
;   bic, and, orr, eor, not (mvn)
; latency 4 throughput 1/4 F0/F1/F2/F3: fmov register, fmov gen to vec
; latency 5 throughput 1/4 F0/F1/F2/F3: fmov vec to gen, umov, fmov
(define_insn_reservation "thunderx3t110_asimd_logic4" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_logic,neon_logic_q"))
  "thunderx3t110_f23")

(define_insn_reservation "thunderx3t110_asimd_logic5" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_logic,neon_logic_q"))
  "thunderx3t110_f0123")

;; ASIMD floating-point instructions.

; Distinguish between latency 5 throughput 1/4: fabs, fmax, fmin, fneg
; latency 4 throughput 1/4: fcmp
(define_insn_reservation "thunderx3t110_asimd_fp_simple" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_abs_s,neon_fp_abs_d,\
			neon_fp_abs_s_q,neon_fp_abs_d_q,\
			neon_fp_compare_s,neon_fp_compare_d,\
			neon_fp_compare_s_q,neon_fp_compare_d_q,\
			neon_fp_minmax_s,neon_fp_minmax_d,\
			neon_fp_minmax_s_q,neon_fp_minmax_d_q,\
			neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_d,\
			neon_fp_reduc_minmax_s_q,neon_fp_reduc_minmax_d_q,\
			neon_fp_neg_s,neon_fp_neg_d,\
			neon_fp_neg_s_q,neon_fp_neg_d_q"))
  "thunderx3t110_f0123")

; distinguish between latency 3 throughput 1/2,
; latency 4 throughput 1/4
; neon_fp_reduc_add_<stype><q> is used for both faddp and
; vector reduction add. On TX3, faddp is 3|4 1/2|1/4 and reduction is 5 1/4
(define_insn_reservation "thunderx3t110_asimd_fp_arith3" 3
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_abd_s,neon_fp_abd_d,\
			neon_fp_abd_s_q,neon_fp_abd_d_q,\
			neon_fp_addsub_s,neon_fp_addsub_d,\
			neon_fp_addsub_s_q,neon_fp_addsub_d_q,\
			neon_fp_reduc_add_s,neon_fp_reduc_add_d,\
			neon_fp_reduc_add_s_q,neon_fp_reduc_add_d_q"))
  "thunderx3t110_f23")

(define_insn_reservation "thunderx3t110_asimd_fp_arith4" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_abd_s,neon_fp_abd_d,\
			neon_fp_abd_s_q,neon_fp_abd_d_q,\
			neon_fp_addsub_s,neon_fp_addsub_d,\
			neon_fp_addsub_s_q,neon_fp_addsub_d_q,\
			neon_fp_reduc_add_s,neon_fp_reduc_add_d,\
			neon_fp_reduc_add_s_q,neon_fp_reduc_add_d_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_fp_arith5" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_mul_s,neon_fp_mul_d,\
			neon_fp_mul_s_q,neon_fp_mul_d_q,\
			neon_fp_mul_s_scalar_q,neon_fp_mul_d_scalar_q,\
			neon_fp_mla_s,neon_fp_mla_d,\
			neon_fp_mla_s_q,neon_fp_mla_d_q"))
  "thunderx3t110_f0123")

; neon_fp_cvt_widen_s,neon_fp_cvt_narrow_d_q: fcvtl,fctvl2,fcvtn,fcvtn2
; neon_fp_to_int_s,neon_fp_to_int_d: fcvt{<frint_suffix><su>,z<su>}
;   where frint_suffix: zpmixan, su: su (plus other sign/unsign/extract...
; neon_fp_to_int_s_q,neon_fp_to_int_d_q: fcvtz<su> other
; The int_to_fp* is complicated
;   neon_int_to_fp_s,neon_int_to_fp_d: <su_optab>cvtf
;   neon_int_to_fp_s_q,neon_int_to_fp_d_q
; Round matches single define_insn, frint<frint_suffix>
;   neon_fp_round_s,neon_fp_round_d,neon_fp_round_s_q,
;   neon_fp_round_d_q: frint<frint_suffix>
; FCVT*,VCVTAU,[SU]CVTF: latency 5 throughput 1/4
; FRINT*: latency 5 throughput 1/4
(define_insn_reservation "thunderx3t110_asimd_fp_conv" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_cvt_widen_s,neon_fp_cvt_narrow_d_q,\
			neon_fp_to_int_s,neon_fp_to_int_d,\
			neon_fp_to_int_s_q,neon_fp_to_int_d_q,\
			neon_int_to_fp_s,neon_int_to_fp_d,\
			neon_int_to_fp_s_q,neon_int_to_fp_d_q,\
			neon_fp_round_s,neon_fp_round_d,\
			neon_fp_round_s_q,neon_fp_round_d_q"))
  "thunderx3t110_f0123")

; model that pipeline is occupied the whole time D/F32, Q/F32: 16/4
; Q/F64: 23/4
(define_insn_reservation "thunderx3t110_asimd_fp_div_s" 16
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_div_s,neon_fp_div_s_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_fp_div_d" 23
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_div_d,neon_fp_div_d_q"))
  "thunderx3t110_f0123")

;; ASIMD miscellaneous instructions.

;  divided out:
;  rbit,bsl,bsl_q,cls,cls_q,cnt,cnt_q,move,move_q: 3|4 1/2 | 1/4
;  from_gp,from_gp_q : 4 | 1/4
;  dup,dup_q,ext,ext_q,ins,ins_q,all recpe forms, rev,rev_q: 5 1/4
;  permute,permute_q needs to depend on aarch64_expand_vec_perm_const does
;  on TX3
(define_insn_reservation "thunderx3t110_asimd_misc3" 3
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_rbit,\
			neon_bsl,neon_bsl_q,\
			neon_cls,neon_cls_q,\
			neon_cnt,neon_cnt_q,\
			neon_move,neon_move_q"))
  "thunderx3t110_f23")

(define_insn_reservation "thunderx3t110_asimd_misc4" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_rbit,\
			neon_bsl,neon_bsl_q,\
			neon_cls,neon_cls_q,\
			neon_cnt,neon_cnt_q,\
			neon_from_gp,neon_from_gp_q,\
			neon_move,neon_move_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_misc" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "
			neon_dup,neon_dup_q,\
			neon_ext,neon_ext_q,\
			neon_ins,neon_ins_q,\
			neon_move,neon_move_q,\
			neon_fp_recpe_s,neon_fp_recpe_d,\
			neon_fp_recpe_s_q,neon_fp_recpe_d_q,\
			neon_fp_recpx_s,neon_fp_recpx_d,\
			neon_fp_recpx_s_q,neon_fp_recpx_d_q,\
			neon_rev,neon_rev_q,\
			neon_permute,neon_permute_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_recip_step" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_fp_recps_s,neon_fp_recps_s_q,\
			neon_fp_recps_d,neon_fp_recps_d_q,\
			neon_fp_sqrt_s,neon_fp_sqrt_s_q,\
			neon_fp_sqrt_d,neon_fp_sqrt_d_q,\
			neon_fp_rsqrte_s, neon_fp_rsqrte_s_q,\
			neon_fp_rsqrte_d, neon_fp_rsqrte_d_q,\
			neon_fp_rsqrts_s, neon_fp_rsqrts_s_q,\
			neon_fp_rsqrts_d, neon_fp_rsqrts_d_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_lut1" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_tbl1,neon_tbl1_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_lut2" 10
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_tbl2,neon_tbl2_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_lut3" 15
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_tbl3,neon_tbl3_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_lut4" 20
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_tbl4,neon_tbl4_q"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_elt_to_gr" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_to_gp,neon_to_gp_q"))
  "thunderx3t110_f0123")

;; ASIMD load instructions.

; NOTE: These reservations attempt to model latency and throughput
; correctly, but the cycle timing of unit allocation is not
; necessarily accurate (because insns are split into uops, and those
; may be issued out-of-order).

; the LDP/LDNP imm-offset S/D/Q suppplies the first arg with latency 4
; and the 2nd at 5 (Q form) or 8 (S/D form). Can this be modeled? These
;forms, as documented, do not use the I0/I1/I2 units (no I3), but the
; other LDP ones do.
(define_insn_reservation "thunderx3t110_asimd_load1_ldp" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_ldp,neon_ldp_q"))
  "thunderx3t110_i012,thunderx3t110_ls01")

; Need to distinguish latency 6 throughput 2: 4 reg D/Q
; latency 5 throughput 3/2: 3 reg D/Q
; latency 4 throughput 1: 2 reg D/Q
; latency 4 throughput 1/2: 1 reg D/Q
(define_insn_reservation "thunderx3t110_asimd_load1" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q,\
			neon_load1_2reg,neon_load1_2reg_q,\
			neon_load1_3reg,neon_load1_3reg_q,\
			neon_load1_4reg,neon_load1_4reg_q"))
  "thunderx3t110_ls01")

(define_insn_reservation "thunderx3t110_asimd_load1_onelane" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_load1_one_lane,neon_load1_one_lane_q"))
  "thunderx3t110_l01delay,thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_load1_all" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_load1_all_lanes,neon_load1_all_lanes_q"))
  "thunderx3t110_l01delay,thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_load2" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_load2_2reg,neon_load2_2reg_q,\
			neon_load2_one_lane,neon_load2_one_lane_q,\
			neon_load2_all_lanes,neon_load2_all_lanes_q"))
  "thunderx3t110_l01delay,thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_load3" 7
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_load3_3reg,neon_load3_3reg_q,\
			neon_load3_one_lane,neon_load3_one_lane_q,\
			neon_load3_all_lanes,neon_load3_all_lanes_q"))
  "thunderx3t110_l01delay,thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_asimd_load4" 8
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_load4_4reg,neon_load4_4reg_q,\
			neon_load4_one_lane,neon_load4_one_lane_q,\
			neon_load4_all_lanes,neon_load4_all_lanes_q"))
  "thunderx3t110_l01delay,thunderx3t110_f0123")

;; ASIMD store instructions.

; Same note applies as for ASIMD load instructions.

; Vector Store pair Need to distinguish:
; 5 throughput: imm-offset S/D; imm-postindex S/D; imm-preindex S/D
; 2 throughput: imm-offset Q; imm-postindex Q; imm-preindex Q
; all index modes use I0/I1/I2
(define_insn_reservation "thunderx3t110_asimd_store_stp" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_stp,neon_stp_q"))
  "thunderx3t110_ls01,thunderx3t110_sd")

; There are multiple forms of ST1
; The following two groups, as documented, do not use the FP pipelines.
; multiple, 1 reg, D-form     ST1
; tx2_ltp:    x    1/2     LS0/LS1
; tx3_ltp:    x    1/2     LS0/LS1
; multiple, 1 reg, Q-form     ST1
; tx2_ltp:    x    1/2     LS0/LS1
; tx3_ltp:    x    1/2     LS0/LS1
;
; one lane, B/H/S         ST1
; tx2_ltp:    x       1/2     LS0/LS1,F0/F1
; tx3_ltp:    x       1/2     LS0/LS1,F0/F1/F2/F3
; one lane, D             ST1
; tx2_ltp:    x       1/2     LS0/LS1,F0/F1
; tx3_ltp:    x       1/2     LS0/LS1,F0/F1/F2/F3
;; Model for st1 insn needs refinement for different register forms
; multiple, 2 reg, D-form     ST1     x    1     LS0/LS1
; multiple, 2 reg, Q-form     ST1     x    1     LS0/LS1
; multiple, 3 reg, D-form     ST1     x    3/2     LS0/LS1
; multiple, 3 reg, Q-form     ST1     x    3/2     LS0/LS1
; multiple,4 reg, D-form         ST1     x    2     LS0/LS1
; multiple,4 reg, Q-form         ST1     x    2     LS0/LS1
(define_insn_reservation "thunderx3t110_asimd_store1" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_store1_1reg,neon_store1_1reg_q,\
			neon_store1_2reg,neon_store1_2reg_q,\
			neon_store1_3reg,neon_store1_4reg"))
  "thunderx3t110_ls01")

(define_insn_reservation "thunderx3t110_asimd_store1_onelane" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_store1_one_lane,neon_store1_one_lane_q"))
  "thunderx3t110_ls01,thunderx3t110_f0123")

; distinguish between throughput 1: D/Q-form B/H/S, Q-form D and
; throughput 1/2: one lane B/H/S/D
(define_insn_reservation "thunderx3t110_asimd_store2" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_store2_2reg,neon_store2_2reg_q,\
			neon_store2_one_lane,neon_store2_one_lane_q"))
  "thunderx3t110_ls01,thunderx3t110_f0123")

; distinguish between throughput 3: D/Q-form B/H/S, Q-form D and
; throughput 1: one lane B/H/S/D
(define_insn_reservation "thunderx3t110_asimd_store3" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_store3_3reg,neon_store3_3reg_q,\
			neon_store3_one_lane,neon_store3_one_lane_q"))
  "thunderx3t110_ls01,thunderx3t110_f0123")

; distinguish between throughput 4: D/Q-form B/H/S, Q-form D and
; throughput 1: one lane B/H/S/D? (not in doc)
(define_insn_reservation "thunderx3t110_asimd_store4" 1
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "neon_store4_4reg,neon_store4_4reg_q,\
			neon_store4_one_lane,neon_store4_one_lane_q"))
  "thunderx3t110_ls01,thunderx3t110_f0123")

;; Crypto extensions.

(define_insn_reservation "thunderx3t110_aes" 4
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "crypto_aese,crypto_aesmc"))
  "thunderx3t110_f0123")

(define_insn_reservation "thunderx3t110_sha" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "crypto_sha1_fast,crypto_sha1_xor,crypto_sha1_slow,\
			crypto_sha256_fast,crypto_sha256_slow"))
  "thunderx3t110_f0123")

;; CRC extension.

(define_insn_reservation "thunderx3t110_crc" 3
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "crc"))
  "thunderx3t110_i1")

;; PMULL extension.

(define_insn_reservation "thunderx3t110_pmull" 5
  (and (eq_attr "tune" "thunderx3t110")
       (eq_attr "type" "crypto_pmull"))
  "thunderx3t110_f0123")
