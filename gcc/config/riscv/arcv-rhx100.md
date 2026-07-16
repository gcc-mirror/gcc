;; DFA scheduling description of the Synopsys RHX-100 cpu
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

(define_automaton "arcv_rhx100")

(define_cpu_unit "arcv_rhx100_ALU_A_fuse0_early"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_ALU_A_fuse1_early"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_ALU_B_fuse0_early"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_ALU_B_fuse1_early"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_MPY"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_DIV"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_DMP_fuse0"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_DMP_fuse1"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_fdivsqrt"	"arcv_rhx100")
(define_cpu_unit "arcv_rhx100_issueA_fuse0" "arcv_rhx100")
(define_cpu_unit "arcv_rhx100_issueA_fuse1" "arcv_rhx100")
(define_cpu_unit "arcv_rhx100_issueB_fuse0" "arcv_rhx100")
(define_cpu_unit "arcv_rhx100_issueB_fuse1" "arcv_rhx100")

;; Instruction reservation for arithmetic instructions (pipe A, pipe B).
(define_insn_reservation "arcv_rhx100_alu_early_arith" 1
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "unknown,move,const,arith,shift,slt,multi,auipc,nop,logical,\
		bitmanip,min,max,minu,maxu,clz,ctz,atomic,\
		condmove,mvpair,zicond,cpop,clmul,rotate"))
  "((arcv_rhx100_issueA_fuse0 + arcv_rhx100_ALU_A_fuse0_early) | (arcv_rhx100_issueA_fuse1 + arcv_rhx100_ALU_A_fuse1_early)) | ((arcv_rhx100_issueB_fuse0 + arcv_rhx100_ALU_B_fuse0_early) | (arcv_rhx100_issueB_fuse1 + arcv_rhx100_ALU_B_fuse1_early))")

(define_insn_reservation "arcv_rhx100_jmp_insn" 1
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "branch,jump,call,jalr,ret,trap"))
  "arcv_rhx100_issueA_fuse0 | arcv_rhx100_issueA_fuse1")

(define_insn_reservation "arcv_rhx100_div_insn" 12
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "idiv"))
  "arcv_rhx100_issueA_fuse0 + arcv_rhx100_DIV")

(define_insn_reservation "arcv_rhx100_mpy_insn" 4
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "imul"))
  "arcv_rhx100_issueA_fuse0 + arcv_rhx100_MPY")

(define_insn_reservation "arcv_rhx100_load_insn" 3
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "load,fpload"))
  "(arcv_rhx100_issueB_fuse0 + arcv_rhx100_DMP_fuse0) | (arcv_rhx100_issueB_fuse1 + arcv_rhx100_DMP_fuse1)")

(define_insn_reservation "arcv_rhx100_store_insn" 1
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "store,fpstore"))
  "(arcv_rhx100_issueB_fuse0 + arcv_rhx100_DMP_fuse0) | (arcv_rhx100_issueB_fuse1 + arcv_rhx100_DMP_fuse1)")

;; (soft) floating points
(define_insn_reservation "arcv_rhx100_xfer" 3
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "mfc,mtc,fcvt,fcvt_i2f,fcvt_f2i,fmove,fcmp"))
  "(arcv_rhx100_ALU_A_fuse0_early | arcv_rhx100_ALU_B_fuse0_early)")

(define_insn_reservation "arcv_rhx100_fmul" 5
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "fadd,fmul,fmadd"))
  "(arcv_rhx100_ALU_A_fuse0_early | arcv_rhx100_ALU_B_fuse0_early)")

(define_insn_reservation "arcv_rhx100_fdiv" 20
  (and (eq_attr "tune" "arcv_rhx100")
       (eq_attr "type" "fdiv,fsqrt"))
  "arcv_rhx100_fdivsqrt*7")

(define_insn_reservation "arcv_rhx100_unknown" 5
  (and (eq_attr "tune" "arcv_rhx100")
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
                        sf_vc_se,xt_vgmulxor,xt_vcrcfold,xt_vile,xt_vilo"))
  "(arcv_rhx100_issueA_fuse0 + arcv_rhx100_issueA_fuse1) | (arcv_rhx100_issueB_fuse0 + arcv_rhx100_issueB_fuse1)")

;; Bypasses
(define_bypass 1 "arcv_rhx100_alu_early_arith" "arcv_rhx100_store_insn" "riscv_store_data_bypass_p")

(define_bypass 1 "arcv_rhx100_load_insn" "arcv_rhx100_store_insn" "riscv_store_data_bypass_p")
(define_bypass 1 "arcv_rhx100_load_insn" "arcv_rhx100_alu_early_arith")
(define_bypass 1 "arcv_rhx100_load_insn" "arcv_rhx100_mpy_insn")
(define_bypass 2 "arcv_rhx100_load_insn" "arcv_rhx100_load_insn")
(define_bypass 1 "arcv_rhx100_load_insn" "arcv_rhx100_div_insn")

(define_bypass 3 "arcv_rhx100_mpy_insn" "arcv_rhx100_mpy_insn")
(define_bypass 3 "arcv_rhx100_mpy_insn" "arcv_rhx100_div_insn")
