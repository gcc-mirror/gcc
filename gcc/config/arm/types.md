;; Instruction Classification for ARM for GNU compiler.

;; Copyright (C) 1991-2013 Free Software Foundation, Inc.
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
; alu_reg            any arithmetic instruction that doesn't have a shifted
;                    or an immediate operand.  This excludes
;                    MOV and MVN but includes MOVT.  This is also the default.
; alu_shift_imm      any arithmetic instruction that has a source operand
;                    shifted by a constant.  This excludes simple shifts.
; alu_shift_reg      as alu_shift_imm, with the shift amount specified in a
;                    register.
; alus_ext           From ARMv8-A: as alu_ext, setting condition flags.
;                    AArch64 Only.
; alus_imm           as alu_imm, setting condition flags.
; alus_reg           as alu_reg, setting condition flags.
; alus_shift_imm     as alu_shift_imm, setting condition flags.
; alus_shift_reg     as alu_shift_reg, setting condition flags.
; bfm                bitfield move operation.
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
; f_sel[d,s]         double/single floating byte select.
; f_store[d,s]       double/single store to memory.  Used for VFP unit.
; fadd[d,s]          double/single floating-point scalar addition.
; fcmp[d,s]          double/single floating-point compare.
; fconst[d,s]        double/single load immediate.
; fcpys              single precision floating point cpy.
; fcsel              From ARMv8-A: Floating-point conditional select.
; fdiv[d,s]          double/single precision floating point division.
; ffarith[d,s]       double/single floating point abs/neg/cpy.
; ffma[d,s]          double/single floating point fused multiply-accumulate.
; float              floating point arithmetic operation.
; fmac[d,s]          double/single floating point multiply-accumulate.
; fmul[d,s]          double/single floating point multiply.
; fsqrt[d,s]         double/single precision floating point square root.
; load_acq           load-acquire.
; load_byte          load byte(s) from memory to arm registers.
; load1              load 1 word from memory to arm registers.
; load2              load 2 words from memory to arm registers.
; load3              load 3 words from memory to arm registers.
; load4              load 4 words from memory to arm registers.
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
; store1             store 1 word to memory from arm registers.
; store2             store 2 words to memory from arm registers.
; store3             store 3 words to memory from arm registers.
; store4             store 4 (or more) words to memory from arm registers.
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
; neon_bp_2cycle
; neon_bp_3cycle
; neon_bp_simple
; neon_fp_vadd_ddd_vabs_dd
; neon_fp_vadd_qqq_vabs_qq
; neon_fp_vmla_ddd_scalar
; neon_fp_vmla_ddd
; neon_fp_vmla_qqq_scalar
; neon_fp_vmla_qqq
; neon_fp_vmul_ddd
; neon_fp_vmul_qqd
; neon_fp_vrecps_vrsqrts_ddd
; neon_fp_vrecps_vrsqrts_qqq
; neon_fp_vsum
; neon_int_1
; neon_int_2
; neon_int_3
; neon_int_4
; neon_int_5
; neon_ldm_2
; neon_ldr
; neon_mcr_2_mcrr
; neon_mcr
; neon_mla_ddd_16_scalar_qdd_32_16_long_scalar
; neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long
; neon_mla_ddd_8_16_qdd_16_8_long_32_16_long
; neon_mla_qqq_32_qqd_32_scalar
; neon_mla_qqq_8_16
; neon_mrc
; neon_mrrc
; neon_mul_ddd_16_scalar_32_16_long_scalar
; neon_mul_ddd_8_16_qdd_16_8_long_32_16_long
; neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar
; neon_mul_qqd_32_scalar
; neon_mul_qqq_8_16_32_ddd_32
; neon_shift_1
; neon_shift_2
; neon_shift_3
; neon_stm_2
; neon_str
; neon_vaba_qqq
; neon_vaba
; neon_vld1_1_2_regs
; neon_vld1_3_4_regs
; neon_vld1_vld2_lane
; neon_vld2_2_regs_vld1_vld2_all_lanes
; neon_vld2_4_regs
; neon_vld3_vld4_all_lanes
; neon_vld3_vld4_lane
; neon_vld3_vld4
; neon_vmov
; neon_vqneg_vqabs
; neon_vqshl_vrshl_vqrshl_qqq
; neon_vshl_ddd
; neon_vsma
; neon_vsra_vrsra
; neon_vst1_1_2_regs_vst2_2_regs
; neon_vst1_3_4_regs
; neon_vst1_vst2_lane
; neon_vst2_4_regs_vst3_vst4
; neon_vst3_vst4_lane
; neon_vst3_vst4

(define_attr "type"
 "adc_imm,\
  adc_reg,\
  adcs_imm,\
  adcs_reg,\
  adr,\
  alu_ext,\
  alu_imm,\
  alu_reg,\
  alu_shift_imm,\
  alu_shift_reg,\
  alus_ext,\
  alus_imm,\
  alus_reg,\
  alus_shift_imm,\
  alus_shift_reg,\
  bfm,\
  block,\
  branch,\
  call,\
  clz,\
  no_insn,\
  csel,\
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
  f_seld,\
  f_sels,\
  f_stored,\
  f_stores,\
  faddd,\
  fadds,\
  fcmpd,\
  fcmps,\
  fconstd,\
  fconsts,\
  fcpys,\
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
  fmuld,\
  fmuls,\
  fsqrts,\
  fsqrtd,\
  load_acq,\
  load_byte,\
  load1,\
  load2,\
  load3,\
  load4,\
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
  store1,\
  store2,\
  store3,\
  store4,\
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
  neon_bp_2cycle,\
  neon_bp_3cycle,\
  neon_bp_simple,\
  neon_fp_vadd_ddd_vabs_dd,\
  neon_fp_vadd_qqq_vabs_qq,\
  neon_fp_vmla_ddd_scalar,\
  neon_fp_vmla_ddd,\
  neon_fp_vmla_qqq_scalar,\
  neon_fp_vmla_qqq,\
  neon_fp_vmul_ddd,\
  neon_fp_vmul_qqd,\
  neon_fp_vrecps_vrsqrts_ddd,\
  neon_fp_vrecps_vrsqrts_qqq,\
  neon_fp_vsum,\
  neon_int_1,\
  neon_int_2,\
  neon_int_3,\
  neon_int_4,\
  neon_int_5,\
  neon_ldm_2,\
  neon_ldr,\
  neon_mcr_2_mcrr,\
  neon_mcr,\
  neon_mla_ddd_16_scalar_qdd_32_16_long_scalar,\
  neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long,\
  neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
  neon_mla_qqq_32_qqd_32_scalar,\
  neon_mla_qqq_8_16,\
  neon_mrc,\
  neon_mrrc,\
  neon_mul_ddd_16_scalar_32_16_long_scalar,\
  neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
  neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar,\
  neon_mul_qqd_32_scalar,\
  neon_mul_qqq_8_16_32_ddd_32,\
  neon_shift_1,\
  neon_shift_2,\
  neon_shift_3,\
  neon_stm_2,\
  neon_str,\
  neon_vaba_qqq,\
  neon_vaba,\
  neon_vld1_1_2_regs,\
  neon_vld1_3_4_regs,\
  neon_vld1_vld2_lane,\
  neon_vld2_2_regs_vld1_vld2_all_lanes,\
  neon_vld2_4_regs,\
  neon_vld3_vld4_all_lanes,\
  neon_vld3_vld4_lane,\
  neon_vld3_vld4,\
  neon_vmov,\
  neon_vqneg_vqabs,\
  neon_vqshl_vrshl_vqrshl_qqq,\
  neon_vshl_ddd,\
  neon_vsma,\
  neon_vsra_vrsra,\
  neon_vst1_1_2_regs_vst2_2_regs,\
  neon_vst1_3_4_regs,\
  neon_vst1_vst2_lane,\
  neon_vst2_4_regs_vst3_vst4,\
  neon_vst3_vst4_lane,\
  neon_vst3_vst4"
    (const_string "untyped"))

; Is this an (integer side) multiply with a 32-bit (or smaller) result?
(define_attr "mul32" "no,yes"
  (if_then_else
    (eq_attr "type"
     "smulxy,smlaxy,smulwy,smlawx,mul,muls,mla,mlas,smlawy,smuad,smuadx,\
      smlad,smladx,smusd,smusdx,smlsd,smlsdx,smmul,smmulr,smmla,smlald,smlsld")
    (const_string "yes")
    (const_string "no")))

; Is this an (integer side) multiply with a 64-bit result?
(define_attr "mul64" "no,yes"
  (if_then_else
    (eq_attr "type"
     "smlalxy,umull,umulls,umaal,umlal,umlals,smull,smulls,smlal,smlals")
    (const_string "yes")
    (const_string "no")))
