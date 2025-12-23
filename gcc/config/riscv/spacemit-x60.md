;; spacemit_x60 DFA-based pipeline description for RISC-V targets.
;; Copyright (C) 2011-2025 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
;; Based on MIPS target for GNU compiler.

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

;; ----------------------------------------------------
;; Spacemit-x60 Units
;; 2*alu + 2*lsu + 1*fpalu + 1*fdivsqrt + 1*vxu
;;
;; There's actually two VXU units and ops get split across them
;; to give the illusion of a single wider unit with higher 
;; performance.  There are a few ops that can only be fed into
;; one of the two units.  Probably best to initially model as
;; a single unit
;;
;; The VXU is not currently modeled.
;; Some ops like shadd.uw and add.uw, cpop take an extra cycle
;; Given everything is in-order, anti-dependencies probably matter
;; FP sign injection isn't handled correctly
;; ----------------------------------------------------

(define_automaton "spacemit_x60")
(define_cpu_unit "spacemit_x60_alu0,spacemit_x60_alu1" "spacemit_x60")
(define_cpu_unit "spacemit_x60_lsu0,spacemit_x60_lsu1" "spacemit_x60")
;;(define_cpu_unit "spacemit_x60_vxu0" "spacemit_x60")
(define_cpu_unit "spacemit_x60_fpalu" "spacemit_x60")
(define_cpu_unit "spacemit_x60_fdivsqrt" "spacemit_x60")

(define_reservation "spacemit_x60_lsu" "spacemit_x60_lsu0, spacemit_x60_lsu1")
(define_reservation "spacemit_x60_alu" "spacemit_x60_alu0, spacemit_x60_alu1")

;; ----------------------------------------------------
;; Memory (load/store)
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_load" 5
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "load,fpload,atomic"))
  "spacemit_x60_lsu")

(define_insn_reservation "spacemit_x60_store" 3
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "store,fpstore"))
  "spacemit_x60_lsu")

;; ----------------------------------------------------
;; Int
;; ----------------------------------------------------

;; alu0 handles div/rem and jumps
(define_insn_reservation "spacemit_x60_jump" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "branch,jump,call,jalr,ret,trap,sfb_alu"))
  "spacemit_x60_alu0")

(define_insn_reservation "spacemit_x60_idivsi" 12
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "SI")))
  "spacemit_x60_alu0*12")

(define_insn_reservation "spacemit_x60_idivdi" 20
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "DI")))
  "spacemit_x60_alu0*20")

(define_insn_reservation "spacemit_x60_imulsi" 3
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "imul")
	    (eq_attr "mode" "SI")))
  "spacemit_x60_alu")

(define_insn_reservation "spacemit_x60_imuldi" 5
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "imul")
	    (eq_attr "mode" "DI")))
  "spacemit_x60_alu")

(define_insn_reservation "spacemit_x60_clmul" 3
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "clmul"))
  "spacemit_x60_alu")

(define_insn_reservation "spacemit_x60_mtc_mfc" 3
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "mtc,mfc"))
  "spacemit_x60_alu")

(define_insn_reservation "spacemit_x60_fcvt_i2f" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "fcvt_i2f"))
  "spacemit_x60_alu")

(define_insn_reservation "spacemit_x60_fcvt_f2i" 6
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "fcvt_f2i"))
  "spacemit_x60_alu")

(define_insn_reservation "spacemit_x60_alu" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "unknown,const,arith,shift,slt,multi,auipc,nop,logical,\
			move,bitmanip,min,max,minu,maxu,clz,ctz,rotate,\
			condmove,crypto,mvpair,zicond,cpop"))
  "spacemit_x60_alu")

(define_insn_reservation "spacemit_x60_alu2c" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "cpop"))
  "spacemit_x60_alu")

;; ----------------------------------------------------
;; Float
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_fcvt" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "fcvt,fmove"))
  "spacemit_x60_fpalu")

(define_insn_reservation "spacemit_x60_fcmp" 6
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "fcmp"))
  "spacemit_x60_fpalu")

(define_insn_reservation "spacemit_x60_fmul_half_single" 4
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fadd,fmul")
       (ior (eq_attr "mode" "HF")
	    (eq_attr "mode" "SF"))))
  "spacemit_x60_fpalu")

(define_insn_reservation "spacemit_x60_fmadd_half_single" 5
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fmadd")
       (ior (eq_attr "mode" "HF")
	    (eq_attr "mode" "SF"))))
  "spacemit_x60_fpalu")

(define_insn_reservation "spacemit_x60_fmul_double" 5
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fadd,fmul")
	    (eq_attr "mode" "DF")))
  "spacemit_x60_fpalu")

(define_insn_reservation "spacemit_x60_fmadd_double" 5
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fmadd")
	    (eq_attr "mode" "DF")))
  "spacemit_x60_fpalu")

(define_insn_reservation "spacemit_x60_fdiv_half" 12
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "HF")))
  "spacemit_x60_fdivsqrt*12")

(define_insn_reservation "spacemit_x60_fdiv_single" 15
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "SF")))
  "spacemit_x60_fdivsqrt*15")

(define_insn_reservation "spacemit_x60_fdiv_double" 22
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "DF")))
  "spacemit_x60_fdivsqrt*22")

(define_insn_reservation "spacemi6_x60_dummy" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "viminmax,vfmuladd,vfmovvf,vssegte,vlsegds,rdvlenb,vaesef,vfcmp,vmpop,vwsll,vsha2cl,vfwcvtbf16,vfncvtftoi,vgather,vsha2ch,vsts,vldm,vmsfs,vfmul,vcompress,vaesz,vssegtox,vstox,vclmulh,vghsh,vaalu,vslideup,vfalu,vaeskf1,vfcvtitof,vaesdm,vmffs,vandn,vstm,vgmul,vlds,viwmul,vfmerge,vlsegdff,vshift,vaesem,vaesdf,vste,ghost,viwred,vsalu,vfwredu,vmidx,sf_vfnrclip,vstux,vfslide1down,vfcvtftoi,vfncvtitof,vnshift,vsm3me,vired,vlde,vfwalu,sf_vc_se,vlsegdux,vicmp,vfncvtftof,vror,vfwmaccbf16,vfminmax,vldff,vstr,vsm3c,vfwcvtftoi,vbrev,vaeskf2,vidiv,vfwcvtftof,rdvl,vimul,vfsgnj,vimovvx,vsha2ms,vialu,vfredo,vctz,vlsegde,viwmuladd,vcpop,vsetvl,vldux,vfwmuladd,vector,wrvxrm,vsshift,vfredu,vimerge,vlsegdox,vfrecp,vnclip,vfclass,vbrev8,vslidedown,vldox,vmalu,vext,vimuladd,sf_vqmacc,vldr,vrol,vmov,vsmul,vclmul,vfmov,vislide1up,vssegtux,vclz,rdfrm,vfwcvtitof,vfncvtbf16,vfmovfv,vislide1down,vfwmul,vfsqrt,vrev8,vicalu,vimov,wrfrm,vfdiv,sf_vc,vsm4k,vmiota,vsm4r,viwalu,vsetvl_pre,vimovxv,vfwredo,vfslide1up,vssegts"))
  "nothing")

