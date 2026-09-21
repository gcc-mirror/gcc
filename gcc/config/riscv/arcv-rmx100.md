;; DFA scheduling description of the ARC-V RMX-100 cpu
;; for GNU C compiler
;; Copyright (C) 2026 Free Software Foundation, Inc.

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

(define_automaton "arcv_rmx100")

(define_cpu_unit "arcv_rmx100_ALU"    "arcv_rmx100")
(define_cpu_unit "arcv_rmx100_FPU"    "arcv_rmx100")
(define_cpu_unit "arcv_rmx100_MPY"    "arcv_rmx100")
(define_cpu_unit "arcv_rmx100_DIV"    "arcv_rmx100")
(define_cpu_unit "arcv_rmx100_DMP"    "arcv_rmx100")

;; Instruction reservation for arithmetic instructions.
(define_insn_reservation "arcv_rmx100_alu_arith" 1
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "unknown, const, arith, shift, slt, multi, auipc, nop,
			logical, move, atomic, mvpair, bitmanip, clz, ctz, cpop,
			zicond, condmove, clmul, min, max, minu, maxu, rotate"))
  "arcv_rmx100_ALU")

(define_insn_reservation "arcv_rmx100_jmp_insn" 1
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "branch, jump, call, jalr, ret, trap"))
  "arcv_rmx100_ALU")

; DIV insn: latency may be overridden by a define_bypass
(define_insn_reservation "arcv_rmx100_div_insn" 35
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "idiv"))
  "arcv_rmx100_DIV*7")

; MPY insn: latency may be overridden by a define_bypass
(define_insn_reservation "arcv_rmx100_mpy32_insn" 9
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "imul"))
  "arcv_rmx100_MPY")

(define_insn_reservation "arcv_rmx100_load_insn" 1
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "load,fpload"))
  "arcv_rmx100_DMP")

(define_insn_reservation "arcv_rmx100_store_insn" 1
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "store,fpstore"))
  "arcv_rmx100_DMP")

;; FPU scheduling.  This is based on the "fast" unit for now.

(define_insn_reservation "arcv_rmx100_farith_insn" 2
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "fadd,fcmp"))
  "arcv_rmx100_FPU")

(define_insn_reservation "arcv_rmx100_xfer" 1
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "fmove,mtc,mfc,fcvt,fcvt_f2i,fcvt_i2f"))
   "arcv_rmx100_FPU")

(define_insn_reservation "arcv_rmx100_fmul_insn" 2
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "fmul"))
  "arcv_rmx100_FPU")

(define_insn_reservation "arcv_rmx100_fmac_insn" 2
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "fmadd"))
  "arcv_rmx100_FPU")

(define_insn_reservation "arcv_rmx100_fdiv_insn" 10
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "fdiv,fsqrt"))
  "arcv_rmx100_FPU")

(define_insn_reservation "arcv_rmx100_unknown" 5
  (and (eq_attr "tune" "arcv_rmx100")
       (eq_attr "type" "vfwalu,vfwcvtftoi,vrol,vmidx,vext,vaeskf1,vfredo,
                        vector,sfb_alu,vlds,viminmax,vfcmp,vimov,vsmul,vnclip,
                        vldm,vsetvl_pre,vwsll,vfmerge,vmffs,vclmul,vmpop,wrfrm,
                        vsha2ms,vidiv,vfncvtitof,vaesef,vldr,vlsegdox,vfwmul,
                        vfmul,vfredu,crypto,vmalu,vimul,vghsh,vialu,viwmul,
                        vfcvtftoi,vaalu,vislide1up,vfcvtitof,vfwcvtftof,vgather,
                        vaesz,vbrev,vshift,vsha2ch,vssegtux,vssegtox,vcompress,
                        vcpop,vstux,vfncvtftof,vfrecp,vssegts,sf_vfnrclip,
                        vstox,vstr,vlsegdff,vired,vimovvx,vislide1down,vclz,
                        vfwredu,rdvl,vlde,vaesem,vsm3me,vmiota,vldux,vlsegde,
                        vssegte,vfwmaccbf16,vfwredo,vctz,vsm4k,vsshift,vsts,
                        vmsfs,vfmovvf,vfslide1down,viwred,vslidedown,vfncvtftoi,
                        vsm3c,vnshift,vfalu,vfsqrt,wrvxrm,vfmuladd,vmov,vsetvl,
                        vfclass,vsha2cl,vicmp,vldff,vfdiv,vste,vaeskf2,
                        vfncvtbf16,vandn,vbrev8,vgmul,vaesdm,vlsegdux,vfsgnj,
                        vfmov,rdfrm,vlsegds,vclmulh,vimuladd,viwalu,vfwmuladd,
                        vimerge,vror,rdvlenb,vfwcvtitof,vaesdf,viwmuladd,vrev8,
                        vsm4r,vsalu,vfminmax,vicalu,vslideup,vldox,vstm,
                        vfwcvtbf16,vfmovfv,vfslide1up,vimovxv,sf_vc,sf_vqmacc,
                        sf_vc_se"))
  "arcv_rmx100_ALU")


(define_bypass 1 "arcv_rmx100_mpy32_insn"
  "arcv_rmx100_*" "arcv_mpy_1c_bypass_p")
(define_bypass 2 "arcv_rmx100_mpy32_insn"
  "arcv_rmx100_*" "arcv_mpy_2c_bypass_p")

(define_bypass 9 "arcv_rmx100_div_insn" "arcv_rmx100_*" "arcv_mpy_1c_bypass_p")
(define_bypass 9 "arcv_rmx100_div_insn" "arcv_rmx100_*" "arcv_mpy_2c_bypass_p")
