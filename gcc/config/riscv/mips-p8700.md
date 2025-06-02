;; DFA-based pipeline description for MIPS P8700.
;;
;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
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

(define_automaton "mips_p8700_agen_alq_pipe, mips_p8700_mdu_pipe, mips_p8700_fpu_pipe")

;; The address generation queue (AGQ) has AL2, CTISTD and LDSTA pipes
(define_cpu_unit "mips_p8700_agq, mips_p8700_al2, mips_p8700_ctistd, mips_p8700_lsu"
		 "mips_p8700_agen_alq_pipe")

(define_cpu_unit "mips_p8700_gpmul, mips_p8700_gpdiv" "mips_p8700_mdu_pipe")

;; The arithmetic-logic-unit queue (ALQ) has ALU pipe
(define_cpu_unit "mips_p8700_alq, mips_p8700_alu" "mips_p8700_agen_alq_pipe")

;; The floating-point-unit queue (FPQ) has short and long pipes
(define_cpu_unit "mips_p8700_fpu_short, mips_p8700_fpu_long" "mips_p8700_fpu_pipe")

;; Long FPU pipeline.
(define_cpu_unit "mips_p8700_fpu_apu" "mips_p8700_fpu_pipe")

;; P8700 unsupported insns are mapped to dummies reservations
(define_reservation "mips_p8700_dummies"
 "mips_p8700_agq |  mips_p8700_al2 |  mips_p8700_ctistd |  mips_p8700_lsu |
 mips_p8700_fpu_short |  mips_p8700_fpu_long")

(define_reservation "mips_p8700_agq_al2" "mips_p8700_agq, mips_p8700_al2")
(define_reservation "mips_p8700_agq_ctistd" "mips_p8700_agq, mips_p8700_ctistd")
(define_reservation "mips_p8700_agq_lsu" "mips_p8700_agq, mips_p8700_lsu")
(define_reservation "mips_p8700_alq_alu" "mips_p8700_alq, mips_p8700_alu")

;;
;; FPU pipe
;;

(define_insn_reservation "mips_p8700_fpu_fadd" 4
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fadd"))
  "mips_p8700_fpu_long, mips_p8700_fpu_apu")

(define_insn_reservation "mips_p8700_fpu_fabs" 2
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fcmp,fmove"))
  "mips_p8700_fpu_short, mips_p8700_fpu_apu")

(define_insn_reservation "mips_p8700_fpu_fload" 8
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fpload"))
  "mips_p8700_agq_lsu")

(define_insn_reservation "mips_p8700_fpu_fstore" 1
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fpstore"))
  "mips_p8700_agq_lsu")

(define_insn_reservation "mips_p8700_fpu_fmadd" 8
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fmadd"))
  "mips_p8700_fpu_long, mips_p8700_fpu_apu")

(define_insn_reservation "mips_p8700_fpu_fmul" 5
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fmul"))
  "mips_p8700_fpu_long, mips_p8700_fpu_apu")

(define_insn_reservation "mips_p8700_fpu_div" 17
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fdiv,fsqrt"))
  "mips_p8700_fpu_long, mips_p8700_fpu_apu*17")

(define_insn_reservation "mips_p8700_fpu_fcvt" 4
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "fcvt,fcvt_i2f,fcvt_f2i"))
  "mips_p8700_fpu_long, mips_p8700_fpu_apu")

(define_insn_reservation "mips_p8700_fpu_fmtc" 7
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "mtc"))
  "mips_p8700_agq_lsu")

(define_insn_reservation "mips_p8700_fpu_fmfc" 7
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "mfc"))
  "mips_p8700_agq_lsu")

;;
;; Integer pipe
;;

(define_insn_reservation "mips_p8700_int_load" 4
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "load"))
  "mips_p8700_agq_lsu")

(define_insn_reservation "mips_p8700_int_store" 3
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "store"))
  "mips_p8700_agq_lsu")

(define_insn_reservation "mips_p8700_int_arith_1" 1
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "unknown,const,arith,shift,slt,multi,auipc,logical,move,bitmanip,min,max,minu,maxu,clz,ctz,rotate,atomic,condmove,crypto,mvpair,zicond"))
  "mips_p8700_alq_alu | mips_p8700_agq_al2")

(define_insn_reservation "mips_p8700_int_nop" 0
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "nop"))
  "mips_p8700_alq_alu | mips_p8700_agq_al2")

(define_insn_reservation "mips_p8700_dsp_mult" 4
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "imul,cpop,clmul"))
  "mips_p8700_gpmul")

(define_insn_reservation "mips_p8700_int_div" 8
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "idiv"))
  "mips_p8700_gpdiv*5")

(define_insn_reservation "mips_p8700_int_branch" 1
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "branch,jump,ret,sfb_alu,trap"))
  "mips_p8700_agq_ctistd")

(define_insn_reservation "mips_p8700_int_call" 2
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "call,jalr"))
  "mips_p8700_agq_ctistd")

;; mips-p8700 dummies insn and placeholder that had no mapping to p8700 hardware.
(define_insn_reservation "mips_p8700_unknown" 1
  (and (eq_attr "tune" "mips_p8700")
       (eq_attr "type" "rdvlenb,rdvl,wrvxrm,wrfrm,
   rdfrm,vsetvl,vsetvl_pre,vlde,vste,vldm,vstm,vlds,vsts,
   vldux,vldox,vstux,vstox,vldff,vldr,vstr,
   vlsegde,vssegte,vlsegds,vssegts,vlsegdux,vlsegdox,vssegtux,vssegtox,vlsegdff,
   vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,viminmax,
   vimul,vidiv,viwmul,vimuladd,sf_vqmacc,viwmuladd,vimerge,vimov,
   vsalu,vaalu,vsmul,vsshift,vnclip,sf_vfnrclip,
   vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,
   vfcmp,vfminmax,vfsgnj,vfclass,vfmerge,vfmov,
   vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,
   vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,
   vired,viwred,vfredu,vfredo,vfwredu,vfwredo,
   vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,vfmovvf,vfmovfv,
   vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,
   vgather,vcompress,vmov,vector,vandn,vbrev,vbrev8,vrev8,vclz,vctz,vcpop,vrol,vror,vwsll,
   vclmul,vclmulh,vghsh,vgmul,vaesef,vaesem,vaesdf,vaesdm,vaeskf1,vaeskf2,vaesz,
   vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm4r,vsm3me,vsm3c,vfncvtbf16,vfwcvtbf16,vfwmaccbf16,
   sf_vc,sf_vc_se"))
  "mips_p8700_dummies")
