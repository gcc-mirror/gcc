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
; arlo_imm           any arithmetic or logical instruction that doesn't have
;                    a shifted operand and has an immediate operand.  This
;                    excludes MOV, MVN and RSB(S) immediate.
; arlo_reg           any arithmetic or logical instruction that doesn't have
;                    a shifted or an immediate operand.  This excludes
;                    MOV and MVN but includes MOVT.  This is also the default.
; arlo_shift         any arithmetic or logical instruction that has a source
;                    operand shifted by a constant.  This excludes
;                    simple shifts.
; arlo_shift_reg     as arlo_shift, with the shift amount specified in a
;                    register.
; block              blockage insn, this blocks all functional units.
; branch             branch.
; call               subroutine call.
; clz                count leading zeros (CLZ).
; extend             extend instruction (SXTB, SXTH, UXTB, UXTH).
; f_2_r              transfer from float to core (no memory needed).
; f_cvt              conversion between float and integral.
; f_flag             transfer of co-processor flags to the CPSR.
; f_load[d,s]        double/single load from memory.  Used for VFP unit.
; f_minmax[d,s]      double/single floating point minimum/maximum.
; f_rint[d,s]        double/single floating point rount to integral.
; f_sel[d,s]         double/single floating byte select.
; f_store[d,s]       double/single store to memory.  Used for VFP unit.
; fadd[d,s]          double/single floating-point scalar addition.
; fcmp[d,s]          double/single floating-point compare.
; fconst[d,s]        double/single load immediate.
; fcpys              single precision floating point cpy.
; fdiv[d,s]          double/single precision floating point division.
; ffarith[d,s]       double/single floating point abs/neg/cpy.
; ffma[d,s]          double/single floating point fused multiply-accumulate.
; float              floating point arithmetic operation.
; fmac[d,s]          double/single floating point multiply-accumulate.
; fmul[d,s]          double/single floating point multiply.
; load_byte          load byte(s) from memory to arm registers.
; load1              load 1 word from memory to arm registers.
; load2              load 2 words from memory to arm registers.
; load3              load 3 words from memory to arm registers.
; load4              load 4 words from memory to arm registers.
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
; mvn_imm            inverting move instruction, immediate.
; mvn_reg            inverting move instruction, register.
; mvn_shift          inverting move instruction, shifted operand by a constant.
; mvn_shift_reg      inverting move instruction, shifted operand by a register.
; r_2_f              transfer from core to float.
; sdiv               signed division.
; shift              simple shift operation (LSL, LSR, ASR, ROR) with an
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

(define_attr "type"
 "arlo_imm,\
  arlo_reg,\
  arlo_shift,\
  arlo_shift_reg,\
  block,\
  branch,\
  call,\
  clz,\
  extend,\
  f_2_r,\
  f_cvt,\
  f_flag,\
  f_loadd,\
  f_loads,\
  f_minmaxd,\
  f_minmaxs,\
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
  load_byte,\
  load1,\
  load2,\
  load3,\
  load4,\
  mla,\
  mlas,\
  mov_imm,\
  mov_reg,\
  mov_shift,\
  mov_shift_reg,\
  mul,\
  muls,\
  mvn_imm,\
  mvn_reg,\
  mvn_shift,\
  mvn_shift_reg,\
  r_2_f,\
  sdiv,\
  shift,\
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
  wmmx_wxor"
  (const_string "arlo_reg"))

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
