;; DFA-based pipeline description for Andes 23 series.
;;
;; Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

(define_automaton "andes_23_arch")

(define_cpu_unit
 "andes_23_alu0, andes_23_alu1, andes_23_lsu0,
  andes_23_lsu1, andes_23_lsu2"
 "andes_23_arch")

(define_cpu_unit "andes_23_mdu" "andes_23_arch")
(define_cpu_unit "andes_23_fpu" "andes_23_arch")

;; andes 23 unsupported insns are mapped to dummies reservations
(define_reservation "andes_23_dummies"
  "andes_23_alu0 | andes_23_alu1 | andes_23_lsu0 | andes_23_lsu1 |
   andes_23_lsu2 | andes_23_mdu | andes_23_fpu")

(define_reservation "andes_23_alu"
 "andes_23_alu0 | andes_23_alu1")

(define_reservation "andes_23_lsu"
 "andes_23_lsu0 | andes_23_lsu1 | andes_23_lsu2")

(define_reservation "andes_23_pipe_unify"
 "andes_23_alu0 + andes_23_alu1")

(define_insn_reservation "andes_23_alu_insn" 1
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "unknown,const,arith,slt,multi,nop,move,
                        shift,logical,mvpair,auipc"))
  "andes_23_alu")

(define_insn_reservation "andes_23_load" 3
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "load"))
  "andes_23_pipe_unify, andes_23_lsu*3")

(define_insn_reservation "andes_23_store" 0
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "store"))
  "andes_23_pipe_unify,andes_23_lsu*3")

(define_insn_reservation "andes_23_branch" 0
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "branch,jump,call,jalr,ret,trap"))
  "andes_23_pipe_unify")

(define_insn_reservation "andes_23_imul" 2
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "imul"))
  "andes_23_alu0, andes_23_mdu")

(define_insn_reservation "andes_23_idivsi" 35
  (and (eq_attr "tune" "andes_23_series")
       (and (eq_attr "type" "idiv")
            (eq_attr "mode" "SI")))
  "andes_23_pipe_unify, andes_23_mdu* 34")

(define_insn_reservation "andes_23_idivdi" 35
  (and (eq_attr "tune" "andes_23_series")
       (and (eq_attr "type" "idiv")
            (eq_attr "mode" "DI")))
  "andes_23_pipe_unify, andes_23_mdu* 34")

(define_insn_reservation "andes_23_xfer" 1
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "mfc,mtc"))
  "andes_23_alu")

(define_insn_reservation "andes_23_fpu_alu" 4
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fadd"))
  "andes_23_pipe_unify, andes_23_fpu")

(define_insn_reservation "andes_23_fpu_mul" 4
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fmul"))
  "andes_23_pipe_unify, andes_23_fpu")

(define_insn_reservation "andes_23_fpu_mac" 4
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fmadd"))
  "andes_23_pipe_unify, andes_23_fpu")

(define_insn_reservation "andes_23_fpu_div" 33
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fdiv"))
  "andes_23_pipe_unify, andes_23_fpu*33")

(define_insn_reservation "andes_23_fpu_sqrt" 33
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fsqrt"))
  "andes_23_pipe_unify, andes_23_fpu*33")

(define_insn_reservation "andes_23_fpu_move" 2
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fmove,mtc,mfc"))
  "andes_23_pipe_unify, andes_23_fpu")

(define_insn_reservation "andes_23_fpu_cmp" 3
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fcmp"))
  "andes_23_pipe_unify, andes_23_fpu")

(define_insn_reservation "andes_23_fpu_cvt" 3
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fcvt,fcvt_i2f,fcvt_f2i"))
  "andes_23_pipe_unify, andes_23_fpu")

(define_insn_reservation "andes_23_fpu_load" 3
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fpload"))
  "andes_23_pipe_unify, andes_23_lsu*3")

(define_insn_reservation "andes_23_fpu_store" 0
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "fpstore"))
  "andes_23_pipe_unify, andes_23_lsu*3")

(define_insn_reservation "andes_23_bitmanip" 1
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "bitmanip,minu,maxu,min,max,clmul,rotate,cpop,clz,ctz"))
  "andes_23_alu0")

(define_insn_reservation "andes_23_crypto" 1
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "crypto"))
  "andes_23_alu0")

(define_bypass 3
  "andes_23_fpu_mul"
  "andes_23_fpu_alu,andes_23_fpu_mac,
   andes_23_fpu_div,andes_23_fpu_sqrt")

(define_bypass 3
  "andes_23_fpu_alu"
  "andes_23_fpu_mul,andes_23_fpu_alu,andes_23_fpu_mac,
   andes_23_fpu_div,andes_23_fpu_sqrt")

(define_bypass 3
  "andes_23_fpu_mac"
  "andes_23_fpu_mul,andes_23_fpu_alu,andes_23_fpu_mac,
   andes_23_fpu_div,andes_23_fpu_sqrt")

(define_bypass 2
  "andes_23_fpu_load"
  "andes_23_fpu_div,andes_23_fpu_sqrt")

(define_insn_reservation "andes_23_unknown" 1
  (and (eq_attr "tune" "andes_23_series")
       (eq_attr "type" "ghost,zicond,mvpair,sfb_alu,condmove,atomic,
			vclz,vror,vsha2ch,vsm4k,vaesef,vghsh,vsm4r,vsm3c,
			vaeskf1,vandn,vaesdm,vclmul,vclmulh,vrol,vcpop,vbrev8,
			vsm3me,vbrev,vctz,vgmul,vsha2ms,vaesz,vrev8,
			vaeskf2,vsha2cl,vwsll,vaesdf,vaesem,vfwmaccbf16,
			sf_vqmacc,sf_vc,sf_vc_se,sf_vfnrclip,vmsfs,vfwalu,
			vnshift,vldm,vslidedown,vicmp,vfcvtftoi,vmffs,vlsegdux,
			vfredo,vstux,vsshift,vfwcvtbf16,vmpop,vicalu,vldff,
			vislide1down,vstox,vfwcvtftof,vfmov,vislide1up,vldr,
			vfmul,vfrecp,vfncvtitof,vfwcvtftoi,vsts,viminmax,vext,
			vaalu,vfdiv,vidiv,viwalu,vssegte,wrvxrm,vfmovvf,vlde,
			vfclass,vshift,vimovxv,vssegtox,vfsqrt,vector,vmalu,
			vfcvtitof,vlsegdff,vfslide1down,vimov,vialu,vmidx,
			vsalu,vfmerge,rdvl,vlds,vfmuladd,vfsgnj,vslideup,
			vfcmp,vfmovfv,vfwcvtitof,vfwmuladd,vfwredo,vlsegdox,
			viwmul,vldox,vsmul,vstm,vfminmax,vmov,vfalu,vfncvtbf16,
			vnclip,vimerge,vfwmul,vimovvx,vfncvtftoi,viwred,rdvlenb,
			vfslide1up,vfncvtftof,vsetvl,viwmuladd,vfredu,vfwredu,
			vlsegde,vmiota,vstr,vgather,vssegts,vldux,vlsegds,vimul,
			vste,vsetvl_pre,vimuladd,vcompress,vssegtux,wrfrm,rdfrm,
			vired"))
  "andes_23_dummies")
