;; DFA scheduling description of the MIPS riscv i8500 cpu for GNU C compiler
;; Copyright (C) 2026 Free Software Foundation, Inc.

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


(define_automaton "mips_i8500_int_pipe, mips_i8500_mdu_pipe,
     mips_i8500_fpu_short_pipe, mips_i8500_fpu_long_pipe")

(define_cpu_unit "mips_i8500_gpmul, mips_i8500_gpdiv" "mips_i8500_mdu_pipe")
(define_cpu_unit "mips_i8500_agen,
     mips_i8500_alu1, mips_i8500_lsu" "mips_i8500_int_pipe")
(define_cpu_unit "mips_i8500_control,
     mips_i8500_alu0" "mips_i8500_int_pipe")

(define_cpu_unit "mips_i8500_fpu_short" "mips_i8500_fpu_short_pipe")
(define_cpu_unit "mips_i8500_fpu_long"	"mips_i8500_fpu_long_pipe")

(define_reservation "mips_i8500_control_alu0"
      "mips_i8500_control, mips_i8500_alu0")
(define_reservation "mips_i8500_agen_lsu" "mips_i8500_agen, mips_i8500_lsu")
(define_reservation "mips_i8500_agen_alu1" "mips_i8500_agen, mips_i8500_alu1")

(define_insn_reservation "mips_i8500_alu" 1
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "unknown,const,arith,shift,slt,multi,auipc,logical,move,
		bitmanip,min,max,minu,maxu,clz,ctz,rotate,atomic,crypto,mvpair,
		zicond,clmul"))
  "mips_i8500_control_alu0 | mips_i8500_agen_alu1")

(define_insn_reservation "mips_i8500_cmove" 2
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "condmove"))
  "mips_i8500_control_alu0 | mips_i8500_agen_alu1")

(define_insn_reservation "mips_i8500_nop" 0
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "nop"))
  "nothing")

;; Multiply instructions are fully pipelined and
;; have a fixed latency of 3 cycles.
(define_insn_reservation "mips_i8500_imul" 3
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "imul"))
  "mips_i8500_gpmul")

(define_insn_reservation "mips_i8500_cpop" 3
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "cpop"))
  "mips_i8500_gpmul")

(define_insn_reservation "mips_i8500_idiv" 22
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "idiv"))
  "mips_i8500_gpdiv*7")

(define_insn_reservation "mips_i8500_load" 3
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "load,fpload"))
  "mips_i8500_agen_lsu")

(define_insn_reservation "mips_i8500_store" 1
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "store,fpstore"))
  "mips_i8500_agen_lsu")

(define_insn_reservation "mips_i8500_branch" 1
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "branch,jump,call,jalr,ret,sfb_alu,trap"))
  "mips_i8500_control_alu0")

(define_insn_reservation "mips_i8500_xfer" 1
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "mfc,mtc"))
  "mips_i8500_control_alu0 | mips_i8500_agen_alu1")

;; fmin/fmax takes 2 cycles where as remaining fmove instructions such as
;; fabs, fsgnj, fmv.w.x takes 1 cycle.  so define the latency for fmove as
;; 2 (worstcase)
(define_insn_reservation "mips_i8500_fmove" 2
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "fmove"))
  "mips_i8500_fpu_short")

(define_insn_reservation "mips_i8500_fcmp" 2
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "fcmp"))
  "mips_i8500_fpu_short")

(define_insn_reservation "mips_i8500_fcvt" 4
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "fadd,fcvt,fcvt_i2f,fcvt_f2i"))
  "mips_i8500_fpu_long")

(define_insn_reservation "mips_i8500_fmul" 5
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "fmul"))
  "mips_i8500_fpu_long")

(define_insn_reservation "mips_i8500_fmadd" 8
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "fmadd"))
  "mips_i8500_fpu_long")

(define_insn_reservation "mips_i8500_fdiv_sf" 24
  (and (eq_attr "tune" "mips_i8500")
      (and (eq_attr "type" "fdiv,fsqrt")
      (eq_attr "mode" "SF")))
  "mips_i8500_fpu_long*7")

(define_insn_reservation "mips_i8500_fdiv_df" 32
  (and (eq_attr "tune" "mips_i8500")
      (and (eq_attr "type" "fdiv")
      (eq_attr "mode" "DF")))
  "mips_i8500_fpu_long*7")

(define_insn_reservation "mips_i8500_fsqrt_df" 36
  (and (eq_attr "tune" "mips_i8500")
      (and (eq_attr "type" "fsqrt")
      (eq_attr "mode" "DF")))
  "mips_i8500_fpu_long*7")

(define_insn_reservation "mips_i8500_fdiv_hf" 1
  (and (eq_attr "tune" "mips_i8500")
      (and (eq_attr "type" "fdiv,fsqrt")
      (eq_attr "mode" "HF")))
  "mips_i8500_fpu_short")

(define_insn_reservation "mips_i8500_unknown" 1
  (and (eq_attr "tune" "mips_i8500")
       (eq_attr "type" "vctz, vssegts, vldm, vldox, rdvl, vmffs, vidiv, vnclip,
      vandn, vfredu, vsha2cl, vmsfs, vssegtox, vired, vbrev8, vfncvtftoi,
      vsts, vector, vfcmp, vimuladd, vldr, vfwmuladd, vfmerge, vclmul, vgmul,
      vimov, vfcvtitof, vslideup, vrev8, vfwredo, vstux, vsmul, viwmuladd,
      vimovxv, vfsgnj, vsha2ch, vsalu, vmalu, vnshift, vimerge, viminmax,
      vghsh, sf_vqmacc, vfcvtftoi, vlsegdff, vmpop, vfslide1up, vfalu,
      vaeskf1, vfslide1down, vsm4k, vsm3c, vfncvtbf16, vfwcvtftoi, vror,
      ghost, wrvxrm, vlsegdux, rdfrm, vfwalu, vfmuladd, vlsegdox, vfwmaccbf16,
      wrfrm, vaesem, vfwcvtftof, vste, vfwmul, vaeskf2, vfdiv, vgather,
      viwalu, vbrev, rdvlenb, vfwcvtbf16, vfclass, vsha2ms, sf_vfnrclip,
      vwsll, vfrecp, vclz, vfredo, sf_vc, vldff, vlsegde, viwmul,
      vldux, vicmp, viwred, vlds, vfncvtftof, vstox, vfmov, vfwredu,
      vislide1down, vmidx, vslidedown, vislide1up, vaesdm, vsm3me, vfmovvf,
      vstm, vsetvl_pre, vssegtux, sf_vc_se, vrol, vcompress, vext, vstr,
      vfsqrt, vsshift, vfmovfv, vcpop, vshift, vsm4r, vimovvx, vialu, vmiota,
      vclmulh, vimul, vfmul, vsetvl, vaesdf, vfncvtitof, vfwcvtitof, vmov,
      vssegte, vaesef, vicalu, vaesz, vlsegds, vaalu, vfminmax, vlde,
      xt_vile, xt_vilo, xt_vcrcfold, xt_vgmulxor"))
  "mips_i8500_control_alu0")
