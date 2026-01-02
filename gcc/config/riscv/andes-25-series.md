;; DFA-based pipeline description for Andes 25 series.
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

(define_automaton "andes_25_arch, andes_25_vector")

;; Integer pipeline
(define_cpu_unit "andes_25_pipe" "andes_25_arch")
;; Division operation unit
(define_cpu_unit "andes_25_mdu" "andes_25_arch")
;; Floating point units
(define_cpu_unit "andes_25_fpu, andes_25_fpu_eu" "andes_25_arch")

;; Vector execution unit.
(define_cpu_unit "andes_25_vpu_lsu, andes_25_vpu_alu, andes_25_vpu_mac,
		  andes_25_vpu_msk, andes_25_vpu_div, andes_25_vpu_fmac,
		  andes_25_vpu_fmis, andes_25_vpu_perm, andes_25_vpu_pipe"
		 "andes_25_vector")

;; andes 25 series unsupported insns are mapped to dummies reservations
(define_reservation "andes_25_dummies"
  "andes_25_pipe | andes_25_mdu | andes_25_fpu"
)

;; andes 25 series vector unsupported insns are mapped to dummies reservations
(define_reservation "andes_25_vector_dummies"
  "andes_25_vpu_lsu | andes_25_vpu_alu | andes_25_vpu_mac | andes_25_vpu_msk |
   andes_25_vpu_div | andes_25_vpu_fmac | andes_25_vpu_fmis |
   andes_25_vpu_perm | andes_25_vpu_pipe"
)

(define_reservation "andes_25_fpu_arith"
  "(andes_25_pipe + andes_25_fpu), andes_25_fpu_eu * 2")

(define_reservation "andes_25_fpu_pipe"
  "andes_25_pipe + andes_25_fpu")

(define_insn_reservation "andes_25_alu_insn" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "unknown,const,arith,shift,slt,multi,nop,logical,move,
			auipc,atomic"))
  "andes_25_pipe")

(define_insn_reservation "andes_25_load_wd" 2
  (and (eq_attr "tune" "andes_25_series")
       (and (eq_attr "type" "load")
	    (not (eq_attr "mode" "QI,HI"))))
  "andes_25_pipe")

(define_insn_reservation "andes_25_load_bh" 3
  (and (eq_attr "tune" "andes_25_series")
       (and (eq_attr "type" "load")
	    (eq_attr "mode" "QI,HI")))
  "andes_25_pipe")

(define_insn_reservation "andes_25_store" 0
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "store"))
  "andes_25_pipe")

(define_insn_reservation "andes_25_branch" 0
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "branch,jump,call,jalr,trap,ret"))
  "andes_25_pipe")

(define_insn_reservation "andes_25_imul" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "imul"))
  "andes_25_pipe")

(define_insn_reservation "andes_25_idivsi" 38
  (and (eq_attr "tune" "andes_25_series")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "SI")))
  "andes_25_pipe, andes_25_mdu * 34")

(define_insn_reservation "andes_25_idivdi" 70
  (and (eq_attr "tune" "andes_25_series")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "DI")))
  "andes_25_pipe, andes_25_mdu * 66")

(define_insn_reservation "andes_25_xfer" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "mfc,mtc"))
  "andes_25_pipe")

(define_insn_reservation "andes_25_fpu_alu" 5
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fadd"))
  "andes_25_fpu_arith")

(define_insn_reservation "andes_25_fpu_mul" 5
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fmul"))
  "andes_25_fpu_arith")

(define_insn_reservation "andes_25_fpu_mac" 5
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fmadd"))
  "andes_25_fpu_arith")

(define_insn_reservation "andes_25_fpu_div" 33
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fdiv"))
  "andes_25_fpu_arith, andes_25_fpu_eu * 27")

(define_insn_reservation "andes_25_fpu_sqrt" 33
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fsqrt"))
  "andes_25_fpu_arith, andes_25_fpu_eu * 27")

(define_insn_reservation "andes_25_fpu_move" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fmove,mtc,mfc"))
  "andes_25_fpu_pipe")

(define_insn_reservation "andes_25_fpu_cmp" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fcmp"))
  "andes_25_fpu_pipe")

(define_insn_reservation "andes_25_fpu_cvt" 6
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fcvt,fcvt_i2f,fcvt_f2i"))
  "andes_25_fpu_arith, andes_25_fpu_eu")

(define_insn_reservation "andes_25_fpu_load" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fpload"))
  "andes_25_fpu_pipe")

(define_insn_reservation "andes_25_fpu_store" 0
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "fpstore"))
  "andes_25_fpu_pipe")

(define_insn_reservation "andes_25_bitmanip" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "bitmanip"))
  "andes_25_pipe")

;; Vector pipeline.

(define_insn_reservation "andes_25_vload" 5
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vlde,vldm,vlds,vldff,vldr"))
  "(andes_25_vpu_pipe + andes_25_vpu_lsu)*3")

(define_insn_reservation "andes_25_index_vload" 8
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vldux,vldox"))
  "(andes_25_vpu_pipe + andes_25_vpu_lsu)*3")

(define_insn_reservation "andes_25_seg_vload" 16
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vlsegde,vlsegds,vlsegdux,vlsegdox,vlsegdff"))
  "(andes_25_vpu_pipe + andes_25_vpu_lsu)*3")

(define_insn_reservation "andes_25_vstore" 0
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vste,vstm,vsts,vstux,vstox,vstr,vssegte,\
			vssegts,vssegtux,vssegtox"))
  "(andes_25_vpu_pipe + andes_25_vpu_lsu)*3")

(define_insn_reservation "andes_25_vialu" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vialu,vicalu,vshift,viminmax,vicmp,vimov,\
			vsalu,vaalu,vmov,vector,vimerge"))
  "andes_25_vpu_pipe + andes_25_vpu_alu")
  
(define_insn_reservation "andes_25_widen_vialu" 2
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "viwalu, vext, vsshift"))
  "andes_25_vpu_pipe + andes_25_vpu_alu")
  
(define_insn_reservation "andes_25_narrow_vialu" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vnshift,vnclip"))
  "andes_25_vpu_pipe + andes_25_vpu_alu")
  
(define_insn_reservation "andes_25_vimul" 2
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vimul,vimuladd,vsmul"))
  "andes_25_vpu_pipe + andes_25_vpu_mac")

(define_insn_reservation "andes_25_widen_vimul" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "viwmul,viwmuladd"))
  "andes_25_vpu_pipe + andes_25_vpu_mac")

(define_insn_reservation "andes_25_vperm" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vslideup,vslidedown,vislide1up,vislide1down,\
			vfslide1up,vfslide1down,vgather"))
  "andes_25_vpu_pipe + andes_25_vpu_perm")
  
(define_insn_reservation "andes_25_vcompress" 4
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vcompress"))
  "andes_25_vpu_pipe + andes_25_vpu_perm")

(define_insn_reservation "andes_25_vmovv" 7
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vimovvx,vfmovvf"))
  "(andes_25_vpu_pipe + andes_25_vpu_perm)*5")

(define_insn_reservation "andes_25_vmovx" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vimovxv,vfmovfv,vfmov"))
  "andes_25_vpu_pipe + andes_25_vpu_perm")

(define_insn_reservation "andes_25_vreduction" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vired,viwred"))
  "andes_25_vpu_pipe + andes_25_vpu_alu*5")

(define_insn_reservation "andes_25_vidiv" 35
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vidiv"))
  "andes_25_vpu_pipe + andes_25_vpu_div*34")

(define_insn_reservation "andes_25_vmask_2" 2
  (eq_attr "type" "vmalu,vmsfs")
  "andes_25_vpu_pipe + andes_25_vpu_msk")

(define_insn_reservation "andes_25_vmask_3" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vmiota,vmidx"))
  "andes_25_vpu_pipe + andes_25_vpu_msk")

(define_insn_reservation "andes_25_vpopc" 6
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vmpop"))
  "andes_25_vpu_pipe + andes_25_vpu_msk")

(define_insn_reservation "andes_25_vffs" 7
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vmffs"))
  "andes_25_vpu_pipe + andes_25_vpu_msk")

(define_insn_reservation "andes_25_vfadd" 4
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfalu,vfwalu,vfmul,vfwmul,vfmuladd,\
			vfwmuladd"))
  "andes_25_vpu_pipe + andes_25_vpu_fmac")

(define_insn_reservation "andes_25_vfdiv" 39
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfdiv,vfsqrt"))
  "andes_25_vpu_pipe + andes_25_vpu_div*19")

(define_insn_reservation "andes_25_vfmis" 2
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfminmax,vfcmp,vfsgnj,vfclass,vfmerge"))
  "andes_25_vpu_pipe + andes_25_vpu_fmis")

(define_insn_reservation "andes_25_vfrecp" 3
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfrecp"))
  "andes_25_vpu_pipe + andes_25_vpu_div")

(define_insn_reservation "andes_25_vfcvt" 2
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfcvtitof,vfcvtftoi"))
  "andes_25_vpu_pipe + andes_25_vpu_fmis")

(define_insn_reservation "andes_25_widen_vfcvt" 5
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfwcvtitof,vfwcvtftoi,vfwcvtftof,vfwcvtbf16"))
  "andes_25_vpu_pipe + andes_25_vpu_fmis")

(define_insn_reservation "andes_25_narrow_vfcvt" 4
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfncvtitof,vfncvtftoi,vfncvtftof,vfncvtbf16"))
  "andes_25_vpu_pipe + andes_25_vpu_fmis")

(define_insn_reservation "andes_25_vfreduction" 6
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vfredu,vfwredu,vfredo,vfwredo"))
  "andes_25_vpu_pipe + andes_25_vpu_fmac*24")

(define_insn_reservation "andes_25_vesetvl" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vsetvl,vsetvl_pre"))
  "andes_25_vpu_pipe")

(define_insn_reservation "andes_25_vcsr" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "wrvxrm,wrfrm,rdvlenb,rdvl"))
  "andes_25_vpu_pipe")

(define_insn_reservation "andes_25_unknown" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "ghost,cpop,clz,ctz,zicond,mvpair,sfb_alu,minu,maxu,
			min,max,clmul,rotate,crypto,condmove,rdfrm"))
  "andes_25_dummies")

(define_insn_reservation "andes_25_vector_unknown" 1
  (and (eq_attr "tune" "andes_25_series")
       (eq_attr "type" "vclz,vror,vsha2ch,vsm4k,vaesef,vghsh,vsm4r,vsm3c,
			vaeskf1,vandn,vaesdm,vclmul,vclmulh,vrol,vcpop,vbrev8,
			vsm3me,vbrev,vctz,vgmul,vsha2ms,vaesz,vrev8,
			vaeskf2,vsha2cl,vwsll,vaesdf,vaesem,vfwmaccbf16,
			sf_vqmacc,sf_vc,sf_vc_se,sf_vfnrclip"))
  "andes_25_vector_dummies")
