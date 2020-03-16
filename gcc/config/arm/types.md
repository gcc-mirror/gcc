;; Instruction Classification for ARM for GNU compiler.

;; Copyright (C) 1991-2020 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

; TYPE attribute is used to classify instructions for use in scheduling.
;
; Instruction classification:
;
; adc_imm            add/subtract with carry and with an immediate operand.
; adc_reg            add/subtract with carry and no immediate operand.
; adcs_imm           as adc_imm, setting condition flags.
; adcs_reg           as adc_reg, setting condition flags.
; adr                calculate address.
; alu_ext            From ARMv8-A: any arithmetic instruction that has a
;                    sign/zero-extended.
;                    AArch64 Only.
;                    source operand
; alu_imm            any arithmetic instruction that doesn't have a shifted
;                    operand and has an immediate operand.  This
;                    excludes MOV, MVN and RSB(S) immediate.
; alu_sreg           any arithmetic instruction that doesn't have a shifted
;                    or an immediate operand.  This excludes
;                    MOV and MVN but includes MOVT.  This also excludes
;                    DSP-kind instructions.  This is also the default.
; alu_shift_imm      any arithmetic instruction that has a source operand
;                    shifted by a constant.  This excludes simple shifts.
; alu_shift_reg      as alu_shift_imm, with the shift amount specified in a
;                    register.
; alu_dsp_reg        any DSP-kind instruction like QSUB8.
; alus_ext           From ARMv8-A: as alu_ext, setting condition flags.
;                    AArch64 Only.
; alus_imm           as alu_imm, setting condition flags.
; alus_sreg          as alu_sreg, setting condition flags.
; alus_shift_imm     as alu_shift_imm, setting condition flags.
; alus_shift_reg     as alu_shift_reg, setting condition flags.
; bfm                bitfield move operation.
; bfx                bitfield extract operation.
; block              blockage insn, this blocks all functional units.
; branch             branch.
; call               subroutine call.
; clz                count leading zeros (CLZ).
; csel               From ARMv8-A: conditional select.
; extend             extend instruction (SXTB, SXTH, UXTB, UXTH).
; f_cvt              conversion between float representations.
; f_cvtf2i           conversion between float and integral types.
; f_cvti2f           conversion between integral and float types.
; f_flag             transfer of co-processor flags to the CPSR.
; f_load[d,s]        double/single load from memory.  Used for VFP unit.
; f_mcr              transfer arm to vfp reg.
; f_mcrr             transfer two arm regs to vfp reg.
; f_minmax[d,s]      double/single floating point minimum/maximum.
; f_mrc              transfer vfp to arm reg.
; f_mrrc             transfer vfp to two arm regs.
; f_rint[d,s]        double/single floating point rount to integral.
; f_store[d,s]       double/single store to memory.  Used for VFP unit.
; fadd[d,s]          double/single floating-point scalar addition.
; fccmp[d,s]         From ARMv8-A: floating-point conditional compare.
; fcmp[d,s]          double/single floating-point compare.
; fconst[d,s]        double/single load immediate.
; fcsel              From ARMv8-A: Floating-point conditional select.
; fdiv[d,s]          double/single precision floating point division.
; ffarith[d,s]       double/single floating point abs/neg/cpy.
; ffma[d,s]          double/single floating point fused multiply-accumulate.
; float              floating point arithmetic operation.
; fmac[d,s]          double/single floating point multiply-accumulate.
; fmov               floating point to floating point register move.
; fmul[d,s]          double/single floating point multiply.
; fsqrt[d,s]         double/single precision floating point square root.
; load_acq           load-acquire.
; load_byte          load 1 byte from memory.
; load_4             load 4 bytes from memory.
; load_8             load 8 bytes from memory.
; load_12            load 12 bytes from memory.
; load_16            load 16 bytes from memory.
; logic_imm          any logical instruction that doesn't have a shifted
;                    operand and has an immediate operand.
; logic_reg          any logical instruction that doesn't have a shifted
;                    operand or an immediate operand.
; logic_shift_imm    any logical instruction that has a source operand
;                    shifted by a constant.  This excludes simple shifts.
; logic_shift_reg    as logic_shift_imm, with the shift amount specified in a
;                    register.
; logics_imm         as logic_imm, setting condition flags.
; logics_reg         as logic_reg, setting condition flags.
; logics_shift_imm   as logic_shift_imm, setting condition flags.
; logics_shift_reg   as logic_shift_reg, setting condition flags.
; mla                integer multiply accumulate.
; mlas               integer multiply accumulate, flag setting.
; mov_imm            simple MOV instruction that moves an immediate to
;                    register.  This includes MOVW, but not MOVT.
; mov_reg            simple MOV instruction that moves a register to another
;                    register.  This includes MOVW, but not MOVT.
; mov_shift          simple MOV instruction, shifted operand by a constant.
; mov_shift_reg      simple MOV instruction, shifted operand by a register.
; mrs                system/special/co-processor register move.
; mul                integer multiply.
; muls               integer multiply, flag setting.
; multiple           more than one instruction, candidate for future
;                    splitting, or better modeling.
; mvn_imm            inverting move instruction, immediate.
; mvn_reg            inverting move instruction, register.
; mvn_shift          inverting move instruction, shifted operand by a constant.
; mvn_shift_reg      inverting move instruction, shifted operand by a register.
; no_insn            an insn which does not represent an instruction in the
;                    final output, thus having no impact on scheduling.
; rbit               reverse bits.
; rev                reverse bytes.
; rotate_imm         rotate by immediate.
; sdiv               signed division.
; shift_imm          simple shift operation (LSL, LSR, ASR, ROR) with an
;                    immediate.
; shift_reg          simple shift by a register.
; smlad              signed multiply accumulate dual.
; smladx             signed multiply accumulate dual reverse.
; smlal              signed multiply accumulate long.
; smlald             signed multiply accumulate long dual.
; smlals             signed multiply accumulate long, flag setting.
; smlalxy            signed multiply accumulate, 16x16-bit, 64-bit accumulate.
; smlawx             signed multiply accumulate, 32x16-bit, 32-bit accumulate.
; smlawy             signed multiply accumulate wide, 32x16-bit,
;                    32-bit accumulate.
; smlaxy             signed multiply accumulate, 16x16-bit, 32-bit accumulate.
; smlsd              signed multiply subtract dual.
; smlsdx             signed multiply subtract dual reverse.
; smlsld             signed multiply subtract long dual.
; smmla              signed most significant word multiply accumulate.
; smmul              signed most significant word multiply.
; smmulr             signed most significant word multiply, rounded.
; smuad              signed dual multiply add.
; smuadx             signed dual multiply add reverse.
; smull              signed multiply long.
; smulls             signed multiply long, flag setting.
; smulwy             signed multiply wide, 32x16-bit, 32-bit accumulate.
; smulxy             signed multiply, 16x16-bit, 32-bit accumulate.
; smusd              signed dual multiply subtract.
; smusdx             signed dual multiply subtract reverse.
; store_rel          store-release.
; store_4            store 4 bytes to memory.
; store_8            store 8 bytes to memory.
; store_12           store 12 bytes to memory.
; store_16           store 16 bytes (or more) to memory.
; trap               cause a trap in the kernel.
; udiv               unsigned division.
; umaal              unsigned multiply accumulate accumulate long.
; umlal              unsigned multiply accumulate long.
; umlals             unsigned multiply accumulate long, flag setting.
; umull              unsigned multiply long.
; umulls             unsigned multiply long, flag setting.
; untyped            insn without type information - default, and error,
;                    case.
;
; The classification below is for instructions used by the Wireless MMX
; Technology. Each attribute value is used to classify an instruction of the
; same name or family.
;
; wmmx_tandc
; wmmx_tbcst
; wmmx_textrc
; wmmx_textrm
; wmmx_tinsr
; wmmx_tmcr
; wmmx_tmcrr
; wmmx_tmia
; wmmx_tmiaph
; wmmx_tmiaxy
; wmmx_tmrc
; wmmx_tmrrc
; wmmx_tmovmsk
; wmmx_torc
; wmmx_torvsc
; wmmx_wabs
; wmmx_wdiff
; wmmx_wacc
; wmmx_wadd
; wmmx_waddbhus
; wmmx_waddsubhx
; wmmx_waligni
; wmmx_walignr
; wmmx_wand
; wmmx_wandn
; wmmx_wavg2
; wmmx_wavg4
; wmmx_wcmpeq
; wmmx_wcmpgt
; wmmx_wmac
; wmmx_wmadd
; wmmx_wmax
; wmmx_wmerge
; wmmx_wmiawxy
; wmmx_wmiaxy
; wmmx_wmin
; wmmx_wmov
; wmmx_wmul
; wmmx_wmulw
; wmmx_wldr
; wmmx_wor
; wmmx_wpack
; wmmx_wqmiaxy
; wmmx_wqmulm
; wmmx_wqmulwm
; wmmx_wror
; wmmx_wsad
; wmmx_wshufh
; wmmx_wsll
; wmmx_wsra
; wmmx_wsrl
; wmmx_wstr
; wmmx_wsub
; wmmx_wsubaddhx
; wmmx_wunpckeh
; wmmx_wunpckel
; wmmx_wunpckih
; wmmx_wunpckil
; wmmx_wxor
;
; The classification below is for NEON instructions.
;
; neon_add
; neon_add_q
; neon_add_widen
; neon_add_long
; neon_qadd
; neon_qadd_q
; neon_add_halve
; neon_add_halve_q
; neon_add_halve_narrow_q
; neon_sub
; neon_sub_q
; neon_sub_widen
; neon_sub_long
; neon_qsub
; neon_qsub_q
; neon_sub_halve
; neon_sub_halve_q
; neon_sub_halve_narrow_q
; neon_abs
; neon_abs_q
; neon_neg
; neon_neg_q
; neon_qneg
; neon_qneg_q
; neon_qabs
; neon_qabs_q
; neon_abd
; neon_abd_q
; neon_abd_long
; neon_minmax
; neon_minmax_q
; neon_compare
; neon_compare_q
; neon_compare_zero
; neon_compare_zero_q
; neon_arith_acc
; neon_arith_acc_q
; neon_reduc_add
; neon_reduc_add_q
; neon_reduc_add_long
; neon_reduc_add_acc
; neon_reduc_add_acc_q
; neon_reduc_minmax
; neon_reduc_minmax_q
; neon_logic
; neon_logic_q
; neon_tst
; neon_tst_q
; neon_shift_imm
; neon_shift_imm_q
; neon_shift_imm_narrow_q
; neon_shift_imm_long
; neon_shift_reg
; neon_shift_reg_q
; neon_shift_acc
; neon_shift_acc_q
; neon_sat_shift_imm
; neon_sat_shift_imm_q
; neon_sat_shift_imm_narrow_q
; neon_sat_shift_reg
; neon_sat_shift_reg_q
; neon_ins
; neon_ins_q
; neon_move
; neon_move_q
; neon_move_narrow_q
; neon_permute
; neon_permute_q
; neon_zip
; neon_zip_q
; neon_tbl1
; neon_tbl1_q
; neon_tbl2
; neon_tbl2_q
; neon_tbl3
; neon_tbl3_q
; neon_tbl4
; neon_tbl4_q
; neon_bsl
; neon_bsl_q
; neon_cls
; neon_cls_q
; neon_cnt
; neon_cnt_q
; neon_dot
; neon_dot_q
; neon_ext
; neon_ext_q
; neon_rbit
; neon_rbit_q
; neon_rev
; neon_rev_q
; neon_mul_b
; neon_mul_b_q
; neon_mul_h
; neon_mul_h_q
; neon_mul_s
; neon_mul_s_q
; neon_mul_b_long
; neon_mul_h_long
; neon_mul_s_long
; neon_mul_d_long
; neon_mul_h_scalar
; neon_mul_h_scalar_q
; neon_mul_s_scalar
; neon_mul_s_scalar_q
; neon_mul_h_scalar_long
; neon_mul_s_scalar_long
; neon_sat_mul_b
; neon_sat_mul_b_q
; neon_sat_mul_h
; neon_sat_mul_h_q
; neon_sat_mul_s
; neon_sat_mul_s_q
; neon_sat_mul_b_long
; neon_sat_mul_h_long
; neon_sat_mul_s_long
; neon_sat_mul_h_scalar
; neon_sat_mul_h_scalar_q
; neon_sat_mul_s_scalar
; neon_sat_mul_s_scalar_q
; neon_sat_mul_h_scalar_long
; neon_sat_mul_s_scalar_long
; neon_mla_b
; neon_mla_b_q
; neon_mla_h
; neon_mla_h_q
; neon_mla_s
; neon_mla_s_q
; neon_mla_b_long
; neon_mla_h_long
; neon_mla_s_long
; neon_mla_h_scalar
; neon_mla_h_scalar_q
; neon_mla_s_scalar
; neon_mla_s_scalar_q
; neon_mla_h_scalar_long
; neon_mla_s_scalar_long
; neon_sat_mla_b_long
; neon_sat_mla_h_long
; neon_sat_mla_s_long
; neon_sat_mla_h_scalar_long
; neon_sat_mla_s_scalar_long
; neon_to_gp
; neon_to_gp_q
; neon_from_gp
; neon_from_gp_q
; neon_ldr
; neon_ldp
; neon_ldp_q
; neon_load1_1reg
; neon_load1_1reg_q
; neon_load1_2reg
; neon_load1_2reg_q
; neon_load1_3reg
; neon_load1_3reg_q
; neon_load1_4reg
; neon_load1_4reg_q
; neon_load1_all_lanes
; neon_load1_all_lanes_q
; neon_load1_one_lane
; neon_load1_one_lane_q
; neon_load2_2reg
; neon_load2_2reg_q
; neon_load2_4reg
; neon_load2_4reg_q
; neon_load2_all_lanes
; neon_load2_all_lanes_q
; neon_load2_one_lane
; neon_load2_one_lane_q
; neon_load3_3reg
; neon_load3_3reg_q
; neon_load3_all_lanes
; neon_load3_all_lanes_q
; neon_load3_one_lane
; neon_load3_one_lane_q
; neon_load4_4reg
; neon_load4_4reg_q
; neon_load4_all_lanes
; neon_load4_all_lanes_q
; neon_load4_one_lane
; neon_load4_one_lane_q
; neon_str
; neon_stp
; neon_stp_q
; neon_store1_1reg
; neon_store1_1reg_q
; neon_store1_2reg
; neon_store1_2reg_q
; neon_store1_3reg
; neon_store1_3reg_q
; neon_store1_4reg
; neon_store1_4reg_q
; neon_store1_one_lane
; neon_store1_one_lane_q
; neon_store2_2reg
; neon_store2_2reg_q
; neon_store2_4reg
; neon_store2_4reg_q
; neon_store2_one_lane
; neon_store2_one_lane_q
; neon_store3_3reg
; neon_store3_3reg_q
; neon_store3_one_lane
; neon_store3_one_lane_q
; neon_store4_4reg
; neon_store4_4reg_q
; neon_store4_one_lane
; neon_store4_one_lane_q
; neon_fp_abs_s
; neon_fp_abs_s_q
; neon_fp_abs_d
; neon_fp_abs_d_q
; neon_fp_neg_s
; neon_fp_neg_s_q
; neon_fp_neg_d
; neon_fp_neg_d_q
; neon_fp_abd_s
; neon_fp_abd_s_q
; neon_fp_abd_d
; neon_fp_abd_d_q
; neon_fp_addsub_s
; neon_fp_addsub_s_q
; neon_fp_addsub_d
; neon_fp_addsub_d_q
; neon_fp_compare_s
; neon_fp_compare_s_q
; neon_fp_compare_d
; neon_fp_compare_d_q
; neon_fp_minmax_s
; neon_fp_minmax_s_q
; neon_fp_minmax_d
; neon_fp_minmax_d_q
; neon_fp_reduc_add_s
; neon_fp_reduc_add_s_q
; neon_fp_reduc_add_d
; neon_fp_reduc_add_d_q
; neon_fp_reduc_minmax_s
; neon_fp_reduc_minmax_s_q
; neon_fp_reduc_minmax_d
; neon_fp_reduc_minmax_d_q
; neon_fp_cvt_narrow_s_q
; neon_fp_cvt_narrow_d_q
; neon_fp_cvt_widen_h
; neon_fp_cvt_widen_s
; neon_fp_to_int_s
; neon_fp_to_int_s_q
; neon_fp_to_int_d
; neon_fp_to_int_d_q
; neon_int_to_fp_s
; neon_int_to_fp_s_q
; neon_int_to_fp_d
; neon_int_to_fp_d_q
; neon_fp_round_s
; neon_fp_round_s_q
; neon_fp_round_d
; neon_fp_round_d_q
; neon_fp_recpe_s
; neon_fp_recpe_s_q
; neon_fp_recpe_d
; neon_fp_recpe_d_q
; neon_fp_recps_s
; neon_fp_recps_s_q
; neon_fp_recps_d
; neon_fp_recps_d_q
; neon_fp_recpx_s
; neon_fp_recpx_s_q
; neon_fp_recpx_d
; neon_fp_recpx_d_q
; neon_fp_rsqrte_s
; neon_fp_rsqrte_s_q
; neon_fp_rsqrte_d
; neon_fp_rsqrte_d_q
; neon_fp_rsqrts_s
; neon_fp_rsqrts_s_q
; neon_fp_rsqrts_d
; neon_fp_rsqrts_d_q
; neon_fp_mul_s
; neon_fp_mul_s_q
; neon_fp_mul_s_scalar
; neon_fp_mul_s_scalar_q
; neon_fp_mul_d
; neon_fp_mul_d_q
; neon_fp_mul_d_scalar_q
; neon_fp_mla_s
; neon_fp_mla_s_q
; neon_fp_mla_s_scalar
; neon_fp_mla_s_scalar_q
; neon_fp_mla_d
; neon_fp_mla_d_q
; neon_fp_mla_d_scalar_q
; neon_fp_sqrt_s
; neon_fp_sqrt_s_q
; neon_fp_sqrt_d
; neon_fp_sqrt_d_q
; neon_fp_div_s
; neon_fp_div_s_q
; neon_fp_div_d
; neon_fp_div_d_q
;
; The classification below is for Crypto instructions.
;
; crypto_aese
; crypto_aesmc
; crypto_sha1_xor
; crypto_sha1_fast
; crypto_sha1_slow
; crypto_sha256_fast
; crypto_sha256_slow
; crypto_pmull
;
; The classification below is for coprocessor instructions
;
; coproc
;
; The classification below is for TME instructions
;
; tme
; The classification below is for M-profile Vector Extension instructions
;
; mve_move
; mve_store
; mve_load

(define_attr "type"
 "adc_imm,\
  adc_reg,\
  adcs_imm,\
  adcs_reg,\
  adr,\
  alu_ext,\
  alu_imm,\
  alu_sreg,\
  alu_shift_imm,\
  alu_shift_reg,\
  alu_dsp_reg,\
  alus_ext,\
  alus_imm,\
  alus_sreg,\
  alus_shift_imm,\
  alus_shift_reg,\
  bfm,\
  bfx,\
  block,\
  branch,\
  call,\
  clz,\
  no_insn,\
  csel,\
  crc,\
  extend,\
  f_cvt,\
  f_cvtf2i,\
  f_cvti2f,\
  f_flag,\
  f_loadd,\
  f_loads,\
  f_mcr,\
  f_mcrr,\
  f_minmaxd,\
  f_minmaxs,\
  f_mrc,\
  f_mrrc,\
  f_rintd,\
  f_rints,\
  f_stored,\
  f_stores,\
  faddd,\
  fadds,\
  fccmpd,\
  fccmps,\
  fcmpd,\
  fcmps,\
  fconstd,\
  fconsts,\
  fcsel,\
  fdivd,\
  fdivs,\
  ffarithd,\
  ffariths,\
  ffmad,\
  ffmas,\
  float,\
  fmacd,\
  fmacs,\
  fmov,\
  fmuld,\
  fmuls,\
  fsqrts,\
  fsqrtd,\
  load_acq,\
  load_byte,\
  load_4,\
  load_8,\
  load_12,\
  load_16,\
  logic_imm,\
  logic_reg,\
  logic_shift_imm,\
  logic_shift_reg,\
  logics_imm,\
  logics_reg,\
  logics_shift_imm,\
  logics_shift_reg,\
  mla,\
  mlas,\
  mov_imm,\
  mov_reg,\
  mov_shift,\
  mov_shift_reg,\
  mrs,\
  mul,\
  muls,\
  multiple,\
  mvn_imm,\
  mvn_reg,\
  mvn_shift,\
  mvn_shift_reg,\
  nop,\
  rbit,\
  rev,\
  rotate_imm,\
  sdiv,\
  shift_imm,\
  shift_reg,\
  smlad,\
  smladx,\
  smlal,\
  smlald,\
  smlals,\
  smlalxy,\
  smlawx,\
  smlawy,\
  smlaxy,\
  smlsd,\
  smlsdx,\
  smlsld,\
  smmla,\
  smmul,\
  smmulr,\
  smuad,\
  smuadx,\
  smull,\
  smulls,\
  smulwy,\
  smulxy,\
  smusd,\
  smusdx,\
  store_rel,\
  store_4,\
  store_8,\
  store_12,\
  store_16,\
  trap,\
  udiv,\
  umaal,\
  umlal,\
  umlals,\
  umull,\
  umulls,\
  untyped,\
  wmmx_tandc,\
  wmmx_tbcst,\
  wmmx_textrc,\
  wmmx_textrm,\
  wmmx_tinsr,\
  wmmx_tmcr,\
  wmmx_tmcrr,\
  wmmx_tmia,\
  wmmx_tmiaph,\
  wmmx_tmiaxy,\
  wmmx_tmrc,\
  wmmx_tmrrc,\
  wmmx_tmovmsk,\
  wmmx_torc,\
  wmmx_torvsc,\
  wmmx_wabs,\
  wmmx_wabsdiff,\
  wmmx_wacc,\
  wmmx_wadd,\
  wmmx_waddbhus,\
  wmmx_waddsubhx,\
  wmmx_waligni,\
  wmmx_walignr,\
  wmmx_wand,\
  wmmx_wandn,\
  wmmx_wavg2,\
  wmmx_wavg4,\
  wmmx_wcmpeq,\
  wmmx_wcmpgt,\
  wmmx_wmac,\
  wmmx_wmadd,\
  wmmx_wmax,\
  wmmx_wmerge,\
  wmmx_wmiawxy,\
  wmmx_wmiaxy,\
  wmmx_wmin,\
  wmmx_wmov,\
  wmmx_wmul,\
  wmmx_wmulw,\
  wmmx_wldr,\
  wmmx_wor,\
  wmmx_wpack,\
  wmmx_wqmiaxy,\
  wmmx_wqmulm,\
  wmmx_wqmulwm,\
  wmmx_wror,\
  wmmx_wsad,\
  wmmx_wshufh,\
  wmmx_wsll,\
  wmmx_wsra,\
  wmmx_wsrl,\
  wmmx_wstr,\
  wmmx_wsub,\
  wmmx_wsubaddhx,\
  wmmx_wunpckeh,\
  wmmx_wunpckel,\
  wmmx_wunpckih,\
  wmmx_wunpckil,\
  wmmx_wxor,\
\
  neon_add,\
  neon_add_q,\
  neon_add_widen,\
  neon_add_long,\
  neon_qadd,\
  neon_qadd_q,\
  neon_add_halve,\
  neon_add_halve_q,\
  neon_add_halve_narrow_q,\
\
  neon_sub,\
  neon_sub_q,\
  neon_sub_widen,\
  neon_sub_long,\
  neon_qsub,\
  neon_qsub_q,\
  neon_sub_halve,\
  neon_sub_halve_q,\
  neon_sub_halve_narrow_q,\
\
  neon_fcadd,\
  neon_fcmla,\
\
  neon_abs,\
  neon_abs_q,\
  neon_dot,\
  neon_dot_q,\
  neon_neg,\
  neon_neg_q,\
  neon_qneg,\
  neon_qneg_q,\
  neon_qabs,\
  neon_qabs_q,\
  neon_abd,\
  neon_abd_q,\
  neon_abd_long,\
\
  neon_minmax,\
  neon_minmax_q,\
  neon_compare,\
  neon_compare_q,\
  neon_compare_zero,\
  neon_compare_zero_q,\
\
  neon_arith_acc,\
  neon_arith_acc_q,\
  neon_reduc_add,\
  neon_reduc_add_q,\
  neon_reduc_add_long,\
  neon_reduc_add_acc,\
  neon_reduc_add_acc_q,\
  neon_reduc_minmax,\
  neon_reduc_minmax_q,\
  neon_logic,\
  neon_logic_q,\
  neon_tst,\
  neon_tst_q,\
\
  neon_shift_imm,\
  neon_shift_imm_q,\
  neon_shift_imm_narrow_q,\
  neon_shift_imm_long,\
  neon_shift_reg,\
  neon_shift_reg_q,\
  neon_shift_acc,\
  neon_shift_acc_q,\
  neon_sat_shift_imm,\
  neon_sat_shift_imm_q,\
  neon_sat_shift_imm_narrow_q,\
  neon_sat_shift_reg,\
  neon_sat_shift_reg_q,\
\
  neon_ins,\
  neon_ins_q,\
  neon_move,\
  neon_move_q,\
  neon_move_narrow_q,\
  neon_permute,\
  neon_permute_q,\
  neon_zip,\
  neon_zip_q,\
  neon_tbl1,\
  neon_tbl1_q,\
  neon_tbl2,\
  neon_tbl2_q,\
  neon_tbl3,\
  neon_tbl3_q,\
  neon_tbl4,\
  neon_tbl4_q,\
\
  neon_bsl,\
  neon_bsl_q,\
  neon_cls,\
  neon_cls_q,\
  neon_cnt,\
  neon_cnt_q,\
  neon_dup,\
  neon_dup_q,\
  neon_ext,\
  neon_ext_q,\
  neon_rbit,\
  neon_rbit_q,\
  neon_rev,\
  neon_rev_q,\
\
  neon_mul_b,\
  neon_mul_b_q,\
  neon_mul_h,\
  neon_mul_h_q,\
  neon_mul_s,\
  neon_mul_s_q,\
  neon_mul_b_long,\
  neon_mul_h_long,\
  neon_mul_s_long,\
  neon_mul_d_long,\
  neon_mul_h_scalar,\
  neon_mul_h_scalar_q,\
  neon_mul_s_scalar,\
  neon_mul_s_scalar_q,\
  neon_mul_h_scalar_long,\
  neon_mul_s_scalar_long,\
\
  neon_sat_mul_b,\
  neon_sat_mul_b_q,\
  neon_sat_mul_h,\
  neon_sat_mul_h_q,\
  neon_sat_mul_s,\
  neon_sat_mul_s_q,\
  neon_sat_mul_b_long,\
  neon_sat_mul_h_long,\
  neon_sat_mul_s_long,\
  neon_sat_mul_h_scalar,\
  neon_sat_mul_h_scalar_q,\
  neon_sat_mul_s_scalar,\
  neon_sat_mul_s_scalar_q,\
  neon_sat_mul_h_scalar_long,\
  neon_sat_mul_s_scalar_long,\
\
  neon_mla_b,\
  neon_mla_b_q,\
  neon_mla_h,\
  neon_mla_h_q,\
  neon_mla_s,\
  neon_mla_s_q,\
  neon_mla_b_long,\
  neon_mla_h_long,\
  neon_mla_s_long,\
  neon_mla_h_scalar,\
  neon_mla_h_scalar_q,\
  neon_mla_s_scalar,\
  neon_mla_s_scalar_q,\
  neon_mla_h_scalar_long,\
  neon_mla_s_scalar_long,\
\
  neon_sat_mla_b_long,\
  neon_sat_mla_h_long,\
  neon_sat_mla_s_long,\
  neon_sat_mla_h_scalar_long,\
  neon_sat_mla_s_scalar_long,\
\
  neon_to_gp,\
  neon_to_gp_q,\
  neon_from_gp,\
  neon_from_gp_q,\
\
  neon_ldr,\
  neon_ldp,\
  neon_ldp_q,\
  neon_load1_1reg,\
  neon_load1_1reg_q,\
  neon_load1_2reg,\
  neon_load1_2reg_q,\
  neon_load1_3reg,\
  neon_load1_3reg_q,\
  neon_load1_4reg,\
  neon_load1_4reg_q,\
  neon_load1_all_lanes,\
  neon_load1_all_lanes_q,\
  neon_load1_one_lane,\
  neon_load1_one_lane_q,\
\
  neon_load2_2reg,\
  neon_load2_2reg_q,\
  neon_load2_4reg,\
  neon_load2_4reg_q,\
  neon_load2_all_lanes,\
  neon_load2_all_lanes_q,\
  neon_load2_one_lane,\
  neon_load2_one_lane_q,\
\
  neon_load3_3reg,\
  neon_load3_3reg_q,\
  neon_load3_all_lanes,\
  neon_load3_all_lanes_q,\
  neon_load3_one_lane,\
  neon_load3_one_lane_q,\
\
  neon_load4_4reg,\
  neon_load4_4reg_q,\
  neon_load4_all_lanes,\
  neon_load4_all_lanes_q,\
  neon_load4_one_lane,\
  neon_load4_one_lane_q,\
\
  neon_str,\
  neon_stp,\
  neon_stp_q,\
  neon_store1_1reg,\
  neon_store1_1reg_q,\
  neon_store1_2reg,\
  neon_store1_2reg_q,\
  neon_store1_3reg,\
  neon_store1_3reg_q,\
  neon_store1_4reg,\
  neon_store1_4reg_q,\
  neon_store1_one_lane,\
  neon_store1_one_lane_q,\
\
  neon_store2_2reg,\
  neon_store2_2reg_q,\
  neon_store2_4reg,\
  neon_store2_4reg_q,\
  neon_store2_one_lane,\
  neon_store2_one_lane_q,\
\
  neon_store3_3reg,\
  neon_store3_3reg_q,\
  neon_store3_one_lane,\
  neon_store3_one_lane_q,\
\
  neon_store4_4reg,\
  neon_store4_4reg_q,\
  neon_store4_one_lane,\
  neon_store4_one_lane_q,\
\
  neon_fp_abs_s,\
  neon_fp_abs_s_q,\
  neon_fp_abs_d,\
  neon_fp_abs_d_q,\
  neon_fp_neg_s,\
  neon_fp_neg_s_q,\
  neon_fp_neg_d,\
  neon_fp_neg_d_q,\
\
  neon_fp_abd_s,\
  neon_fp_abd_s_q,\
  neon_fp_abd_d,\
  neon_fp_abd_d_q,\
  neon_fp_addsub_s,\
  neon_fp_addsub_s_q,\
  neon_fp_addsub_d,\
  neon_fp_addsub_d_q,\
  neon_fp_compare_s,\
  neon_fp_compare_s_q,\
  neon_fp_compare_d,\
  neon_fp_compare_d_q,\
  neon_fp_minmax_s,\
  neon_fp_minmax_s_q,\
  neon_fp_minmax_d,\
  neon_fp_minmax_d_q,\
\
  neon_fp_reduc_add_s,\
  neon_fp_reduc_add_s_q,\
  neon_fp_reduc_add_d,\
  neon_fp_reduc_add_d_q,\
  neon_fp_reduc_minmax_s,\
  neon_fp_reduc_minmax_s_q,\
  neon_fp_reduc_minmax_d,\
  neon_fp_reduc_minmax_d_q,\
\
  neon_fp_cvt_narrow_s_q,\
  neon_fp_cvt_narrow_d_q,\
  neon_fp_cvt_widen_h,\
  neon_fp_cvt_widen_s,\
\
  neon_fp_to_int_s,\
  neon_fp_to_int_s_q,\
  neon_fp_to_int_d,\
  neon_fp_to_int_d_q,\
  neon_int_to_fp_s,\
  neon_int_to_fp_s_q,\
  neon_int_to_fp_d,\
  neon_int_to_fp_d_q,\
  neon_fp_round_s,\
  neon_fp_round_s_q,\
  neon_fp_round_d,\
  neon_fp_round_d_q,\
\
  neon_fp_recpe_s,\
  neon_fp_recpe_s_q,\
  neon_fp_recpe_d,\
  neon_fp_recpe_d_q,\
  neon_fp_recps_s,\
  neon_fp_recps_s_q,\
  neon_fp_recps_d,\
  neon_fp_recps_d_q,\
  neon_fp_recpx_s,\
  neon_fp_recpx_s_q,\
  neon_fp_recpx_d,\
  neon_fp_recpx_d_q,\
\
  neon_fp_rsqrte_s,\
  neon_fp_rsqrte_s_q,\
  neon_fp_rsqrte_d,\
  neon_fp_rsqrte_d_q,\
  neon_fp_rsqrts_s,\
  neon_fp_rsqrts_s_q,\
  neon_fp_rsqrts_d,\
  neon_fp_rsqrts_d_q,\
\
  neon_fp_mul_s,\
  neon_fp_mul_s_q,\
  neon_fp_mul_s_scalar,\
  neon_fp_mul_s_scalar_q,\
  neon_fp_mul_d,\
  neon_fp_mul_d_q,\
  neon_fp_mul_d_scalar_q,\
\
  neon_fp_mla_s,\
  neon_fp_mla_s_q,\
  neon_fp_mla_s_scalar,\
  neon_fp_mla_s_scalar_q,\
  neon_fp_mla_d,\
  neon_fp_mla_d_q,\
  neon_fp_mla_d_scalar_q,\
\
  neon_fp_sqrt_s,\
  neon_fp_sqrt_s_q,\
  neon_fp_sqrt_d,\
  neon_fp_sqrt_d_q,\
  neon_fp_div_s,\
  neon_fp_div_s_q,\
  neon_fp_div_d,\
  neon_fp_div_d_q,\
\
  crypto_aese,\
  crypto_aesmc,\
  crypto_sha1_xor,\
  crypto_sha1_fast,\
  crypto_sha1_slow,\
  crypto_sha256_fast,\
  crypto_sha256_slow,\
  crypto_pmull,\
  crypto_sha512,\
  crypto_sha3,\
  crypto_sm3,\
  crypto_sm4,\
  coproc,\
  tme,\
  memtag,\
  mve_move,\
  mve_store,\
  mve_load"
   (const_string "untyped"))

; Is this an (integer side) multiply with a 32-bit (or smaller) result?
(define_attr "mul32" "no,yes"
  (if_then_else
    (eq_attr "type"
     "smulxy,smlaxy,smulwy,smlawx,mul,muls,mla,mlas,smlawy,smuad,smuadx,\
      smlad,smladx,smusd,smusdx,smlsd,smlsdx,smmul,smmulr,smmla,smlald,smlsld")
    (const_string "yes")
    (const_string "no")))

; Is this an (integer side) widening multiply with a 64-bit result?
(define_attr "widen_mul64" "no,yes"
  (if_then_else
    (eq_attr "type"
     "smlalxy,umull,umulls,umaal,umlal,umlals,smull,smulls,smlal,smlals")
    (const_string "yes")
    (const_string "no")))

; YES if the "type" attribute assigned to the insn denotes an
; Advanced SIMD instruction, NO otherwise.
(define_attr "is_neon_type" "yes,no"
	 (if_then_else (eq_attr "type"
	 "neon_add, neon_add_q, neon_add_widen, neon_add_long,\
          neon_qadd, neon_qadd_q, neon_add_halve, neon_add_halve_q,\
          neon_add_halve_narrow_q,\
          neon_sub, neon_sub_q, neon_sub_widen, neon_sub_long, neon_qsub,\
          neon_qsub_q, neon_sub_halve, neon_sub_halve_q,\
          neon_sub_halve_narrow_q,\
	  neon_abs, neon_abs_q, neon_dot, neon_dot_q, neon_neg, neon_neg_q,\
	  neon_qneg, neon_qneg_q, neon_qabs, neon_qabs_q, neon_abd, neon_abd_q,\
          neon_abd_long, neon_minmax, neon_minmax_q, neon_compare,\
          neon_compare_q, neon_compare_zero, neon_compare_zero_q,\
          neon_arith_acc, neon_arith_acc_q, neon_reduc_add,\
          neon_reduc_add_q, neon_reduc_add_long, neon_reduc_add_acc,\
          neon_reduc_add_acc_q, neon_reduc_minmax, neon_reduc_minmax_q,\
          neon_logic, neon_logic_q, neon_tst, neon_tst_q,\
          neon_shift_imm, neon_shift_imm_q, neon_shift_imm_narrow_q,\
          neon_shift_imm_long, neon_shift_reg, neon_shift_reg_q,\
          neon_shift_acc, neon_shift_acc_q, neon_sat_shift_imm,\
          neon_sat_shift_imm_q, neon_sat_shift_imm_narrow_q,\
          neon_sat_shift_reg, neon_sat_shift_reg_q,\
          neon_ins, neon_ins_q, neon_move, neon_move_q, neon_move_narrow_q,\
          neon_permute, neon_permute_q, neon_zip, neon_zip_q, neon_tbl1,\
          neon_tbl1_q, neon_tbl2, neon_tbl2_q, neon_tbl3, neon_tbl3_q,\
          neon_tbl4, neon_tbl4_q, neon_bsl, neon_bsl_q, neon_cls,\
          neon_cls_q, neon_cnt, neon_cnt_q, neon_dup, neon_dup_q,\
          neon_ext, neon_ext_q, neon_rbit, neon_rbit_q,\
          neon_rev, neon_rev_q, neon_mul_b, neon_mul_b_q, neon_mul_h,\
          neon_mul_h_q, neon_mul_s, neon_mul_s_q, neon_mul_b_long,\
          neon_mul_h_long, neon_mul_s_long, neon_mul_d_long, neon_mul_h_scalar,\
          neon_mul_h_scalar_q, neon_mul_s_scalar, neon_mul_s_scalar_q,\
          neon_mul_h_scalar_long, neon_mul_s_scalar_long, neon_sat_mul_b,\
          neon_sat_mul_b_q, neon_sat_mul_h, neon_sat_mul_h_q,\
          neon_sat_mul_s, neon_sat_mul_s_q, neon_sat_mul_b_long,\
          neon_sat_mul_h_long, neon_sat_mul_s_long, neon_sat_mul_h_scalar,\
          neon_sat_mul_h_scalar_q, neon_sat_mul_s_scalar,\
          neon_sat_mul_s_scalar_q, neon_sat_mul_h_scalar_long,\
          neon_sat_mul_s_scalar_long, neon_mla_b, neon_mla_b_q, neon_mla_h,\
          neon_mla_h_q, neon_mla_s, neon_mla_s_q, neon_mla_b_long,\
          neon_mla_h_long, neon_mla_s_long, neon_mla_h_scalar,\
          neon_mla_h_scalar_q, neon_mla_s_scalar, neon_mla_s_scalar_q,\
          neon_mla_h_scalar_long, neon_mla_s_scalar_long,\
          neon_sat_mla_b_long, neon_sat_mla_h_long,\
          neon_sat_mla_s_long, neon_sat_mla_h_scalar_long,\
          neon_sat_mla_s_scalar_long,\
          neon_to_gp, neon_to_gp_q, neon_from_gp, neon_from_gp_q,\
	   neon_ldr, neon_ldp, neon_ldp_q,\
	   neon_load1_1reg, neon_load1_1reg_q, neon_load1_2reg,\
          neon_load1_2reg_q, neon_load1_3reg, neon_load1_3reg_q,\
          neon_load1_4reg, neon_load1_4reg_q, neon_load1_all_lanes,\
          neon_load1_all_lanes_q, neon_load1_one_lane, neon_load1_one_lane_q,\
          neon_load2_2reg, neon_load2_2reg_q, neon_load2_4reg,\
          neon_load2_4reg_q, neon_load2_all_lanes, neon_load2_all_lanes_q,\
          neon_load2_one_lane, neon_load2_one_lane_q,\
          neon_load3_3reg, neon_load3_3reg_q, neon_load3_all_lanes,\
          neon_load3_all_lanes_q, neon_load3_one_lane, neon_load3_one_lane_q,\
          neon_load4_4reg, neon_load4_4reg_q, neon_load4_all_lanes,\
          neon_load4_all_lanes_q, neon_load4_one_lane, neon_load4_one_lane_q,\
	   neon_str, neon_stp, neon_stp_q,\
	   neon_store1_1reg, neon_store1_1reg_q, neon_store1_2reg,\
          neon_store1_2reg_q, neon_store1_3reg, neon_store1_3reg_q,\
          neon_store1_4reg, neon_store1_4reg_q, neon_store1_one_lane,\
          neon_store1_one_lane_q, neon_store2_2reg, neon_store2_2reg_q,\
          neon_store2_4reg, neon_store2_4reg_q, neon_store2_one_lane,\
          neon_store2_one_lane_q, neon_store3_3reg, neon_store3_3reg_q,\
          neon_store3_one_lane, neon_store3_one_lane_q, neon_store4_4reg,\
          neon_store4_4reg_q, neon_store4_one_lane, neon_store4_one_lane_q,\
          neon_fp_abd_s, neon_fp_abd_s_q, neon_fp_abd_d, neon_fp_abd_d_q,\
          neon_fp_abs_s, neon_fp_abs_s_q, neon_fp_abs_d, neon_fp_abs_d_q,\
          neon_fp_addsub_s, neon_fp_addsub_s_q, neon_fp_addsub_d,\
          neon_fp_addsub_d_q, neon_fp_compare_s, neon_fp_compare_s_q,\
          neon_fp_compare_d, neon_fp_compare_d_q, neon_fp_minmax_s,\
          neon_fp_minmax_s_q, neon_fp_minmax_d, neon_fp_minmax_d_q,\
          neon_fp_neg_s, neon_fp_neg_s_q, neon_fp_neg_d, neon_fp_neg_d_q,\
          neon_fp_reduc_add_s, neon_fp_reduc_add_s_q, neon_fp_reduc_add_d,\
          neon_fp_reduc_add_d_q, neon_fp_reduc_minmax_s,
          neon_fp_reduc_minmax_s_q, neon_fp_reduc_minmax_d,\
          neon_fp_reduc_minmax_d_q,\
          neon_fp_cvt_narrow_s_q, neon_fp_cvt_narrow_d_q,\
          neon_fp_cvt_widen_h, neon_fp_cvt_widen_s, neon_fp_to_int_s,\
          neon_fp_to_int_s_q, neon_int_to_fp_s, neon_int_to_fp_s_q,\
          neon_fp_to_int_d, neon_fp_to_int_d_q,\
          neon_int_to_fp_d, neon_int_to_fp_d_q,\
          neon_fp_round_s, neon_fp_round_s_q, neon_fp_recpe_s,\
          neon_fp_recpe_s_q,\
          neon_fp_recpe_d, neon_fp_recpe_d_q, neon_fp_recps_s,\
          neon_fp_recps_s_q, neon_fp_recps_d, neon_fp_recps_d_q,\
          neon_fp_recpx_s, neon_fp_recpx_s_q, neon_fp_recpx_d,\
          neon_fp_recpx_d_q, neon_fp_rsqrte_s, neon_fp_rsqrte_s_q,\
          neon_fp_rsqrte_d, neon_fp_rsqrte_d_q, neon_fp_rsqrts_s,\
          neon_fp_rsqrts_s_q, neon_fp_rsqrts_d, neon_fp_rsqrts_d_q,\
          neon_fp_mul_s, neon_fp_mul_s_q, neon_fp_mul_s_scalar,\
          neon_fp_mul_s_scalar_q, neon_fp_mul_d, neon_fp_mul_d_q,\
          neon_fp_mul_d_scalar_q, neon_fp_mla_s, neon_fp_mla_s_q,\
          neon_fp_mla_s_scalar, neon_fp_mla_s_scalar_q, neon_fp_mla_d,\
          neon_fp_mla_d_q, neon_fp_mla_d_scalar_q, neon_fp_sqrt_s,\
          neon_fp_sqrt_s_q, neon_fp_sqrt_d, neon_fp_sqrt_d_q,\
          neon_fp_div_s, neon_fp_div_s_q, neon_fp_div_d, neon_fp_div_d_q, crypto_aese,\
          crypto_aesmc, crypto_sha1_xor, crypto_sha1_fast, crypto_sha1_slow,\
          crypto_sha256_fast, crypto_sha256_slow")
        (const_string "yes")
        (const_string "no")))

;; YES if the "type" attribute assigned to the insn denotes an MVE instruction,
;; No otherwise.
(define_attr "is_mve_type" "yes,no"
        (if_then_else (eq_attr "type"
        "mve_move, mve_load, mve_store, mrs")
        (const_string "yes")
        (const_string "no")))

(define_insn_reservation "no_reservation" 0
  (eq_attr "type" "no_insn")
  "nothing")
