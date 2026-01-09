;; DFA-based pipeline description for Andes 45 series.
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

(define_automaton "andes_45_arch, andes_45_vector")

(define_cpu_unit "andes_45_pipe0" "andes_45_arch")
(define_cpu_unit "andes_45_pipe1" "andes_45_arch")
(define_cpu_unit "andes_45_vpu_pipe0" "andes_45_vector")
(define_cpu_unit "andes_45_vpu_pipe1" "andes_45_vector")

(define_reservation "andes_45_vpu_pipe" "(andes_45_vpu_pipe0 + andes_45_pipe0 | andes_45_vpu_pipe1 + andes_45_pipe1)")

(define_cpu_unit "andes_45_mdu,andes_45_alu0,andes_45_alu1,andes_45_bru0,andes_45_bru1,andes_45_lsu" "andes_45_arch")
(define_cpu_unit "andes_45_fpu_fmac,andes_45_fpu_fdiv,andes_45_fpu_fmis,andes_45_fpu_fmv" "andes_45_arch")
(define_cpu_unit "andes_45_vpu_alu,andes_45_vpu_mac,andes_45_vpu_fmis,andes_45_vpu_permut,
		  andes_45_vpu_div,andes_45_vpu_fdiv,andes_45_vpu_mask,andes_45_vpu_lsu" "andes_45_vector")

(define_reservation "andes_45_fpu_arith"
  "andes_45_pipe0 + andes_45_fpu_fmac | andes_45_pipe1 + andes_45_fpu_fmac")

;; andes 45 series unsupported insns are mapped to dummies reservations
(define_reservation "andes_45_dummies"
  "andes_45_pipe0 | andes_45_pipe1, andes_45_alu0 | andes_45_alu1")

;; andes 45 series vector unsupported insns are mapped to dummies reservations
(define_reservation "andes_45_vector_dummies"
  "andes_45_pipe0 | andes_45_pipe1, andes_45_vpu_alu")

(define_insn_reservation "andes_45_alu_insn_s" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "shift,nop,logical"))
  "andes_45_pipe0 + andes_45_alu0 | andes_45_pipe1 + andes_45_alu1")

(define_insn_reservation "andes_45_alu_insn_l" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "unknown,const,arith,multi,slt,move,auipc,atomic,bitmanip"))
  "andes_45_pipe0 + andes_45_alu0 | andes_45_pipe1 + andes_45_alu1")

(define_insn_reservation "andes_45_cmov" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "condmove"))
  "andes_45_pipe0 + andes_45_alu0 + andes_45_pipe1 + andes_45_alu1")

(define_insn_reservation "andes_45_load_wd" 4
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "load")
	    (not (eq_attr "mode" "QI,HI"))))
  "andes_45_pipe0 + andes_45_lsu | andes_45_pipe1 + andes_45_lsu")

(define_insn_reservation "andes_45_load_bh" 5
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "load")
	    (eq_attr "mode" "QI,HI")))
  "andes_45_pipe0 + andes_45_lsu | andes_45_pipe1 + andes_45_lsu")

(define_insn_reservation "andes_45_store_d" 0
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "store")
	    (eq_attr "mode" "DI,SI")))
  "andes_45_pipe0 + andes_45_lsu | andes_45_pipe1 + andes_45_lsu")

(define_insn_reservation "andes_45_store" 0
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "store")
	    (not (eq_attr "mode" "DI,SI"))))
  "andes_45_pipe0 + andes_45_pipe1 + andes_45_lsu")

(define_insn_reservation "andes_45_branch" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "branch,jump,call,ret,jalr,trap"))
  "andes_45_pipe0 + andes_45_bru0 | andes_45_pipe1 + andes_45_bru1")

(define_insn_reservation "andes_45_imul" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "imul"))
  "andes_45_pipe0 + andes_45_alu0 | andes_45_pipe1 + andes_45_alu1, andes_45_mdu * 2")

(define_insn_reservation "andes_45_idivsi" 38
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "SI")))
  "andes_45_pipe0 + andes_45_alu0 | andes_45_pipe1 + andes_45_alu1, andes_45_mdu * 2")

(define_insn_reservation "andes_45_idivdi" 70
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "DI")))
  "andes_45_pipe0  + andes_45_alu0 | andes_45_pipe1 + andes_45_alu1, andes_45_mdu * 2")

(define_insn_reservation "andes_45_xfer" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "mfc,mtc"))
  "andes_45_pipe0 + andes_45_alu0 | andes_45_pipe1 + andes_45_alu1")

(define_insn_reservation "andes_45_fpu_alu_s" 3
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "fadd")
	    (eq_attr "mode" "BF,HF,SF")))
  "andes_45_fpu_arith")

(define_insn_reservation "andes_45_fpu_alu_d" 4
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "fadd")
	    (eq_attr "mode" "DF")))
  "andes_45_fpu_arith")

(define_insn_reservation "andes_45_fpu_mul_s" 3
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "fmul")
	    (eq_attr "mode" "BF,HF,SF")))
  "andes_45_fpu_arith")

(define_insn_reservation "andes_45_fpu_mul_d" 4
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "fmul")
	    (eq_attr "mode" "DF")))
  "andes_45_fpu_arith")

(define_insn_reservation "andes_45_fpu_mac_s" 3
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "fmadd")
	    (eq_attr "mode" "BF,HF,SF")))
  "(andes_45_pipe0 | andes_45_pipe1) + andes_45_fpu_fmac + andes_45_fpu_fmv + andes_45_fpu_fmis")

(define_insn_reservation "andes_45_fpu_mac_d" 4
  (and (eq_attr "tune" "andes_45_series")
       (and (eq_attr "type" "fmadd")
	    (eq_attr "mode" "DF")))
  "(andes_45_pipe0 | andes_45_pipe1) + andes_45_fpu_fmac + andes_45_fpu_fmv + andes_45_fpu_fmis")

(define_insn_reservation "andes_45_fpu_div" 7
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "fdiv"))
  "andes_45_pipe0 + andes_45_fpu_fdiv | andes_45_pipe1 + andes_45_fpu_fdiv, andes_45_fpu_fdiv * 6")

(define_insn_reservation "andes_45_fpu_sqrt" 7
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "fsqrt"))
  "andes_45_pipe0 + andes_45_fpu_fdiv | andes_45_pipe1 + andes_45_fpu_fdiv, andes_45_fpu_fdiv * 6")

(define_insn_reservation "andes_45_fpu_move" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "fmove,mtc,mfc"))
  "andes_45_pipe0 + andes_45_fpu_fmv | andes_45_pipe1 + andes_45_fpu_fmv")

(define_insn_reservation "andes_45_fpu_cmp" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "fcmp"))
  "andes_45_pipe0 + andes_45_fpu_fmis | andes_45_pipe1 + andes_45_fpu_fmis")

(define_insn_reservation "andes_45_fpu_cvt" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "fcvt,fcvt_f2i,fcvt_i2f"))
  "andes_45_pipe0 + andes_45_fpu_fmis | andes_45_pipe1 + andes_45_fpu_fmis")

(define_insn_reservation "andes_45_fpu_load" 4
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "fpload"))
  "andes_45_pipe0 + andes_45_pipe1 + andes_45_lsu")

(define_insn_reservation "andes_45_fpu_store" 0
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "fpstore"))
  "andes_45_pipe0 + andes_45_pipe1 + andes_45_lsu")

(define_insn_reservation "andes_45_vpu_load_e" 8
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vlde,vldm,vldr,vlsegde,vldff,vlsegdff"))
  "(andes_45_vpu_pipe + andes_45_vpu_lsu), andes_45_vpu_lsu * 2")

(define_insn_reservation "andes_45_vpu_load_s" 10
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vlds,vlsegds"))
  "(andes_45_vpu_pipe + andes_45_vpu_lsu), andes_45_vpu_lsu * 3")

(define_insn_reservation "andes_45_vpu_load_x" 12
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vldox,vldux,vlsegdox,vlsegdux"))
  "(andes_45_vpu_pipe + andes_45_vpu_lsu), andes_45_vpu_lsu * 4")

(define_insn_reservation "andes_45_vpu_store" 0
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vste,vstm,vstr,vsts,vstux,vstox,vssegtox,vssegte,
			vssegtux,vssegts"))
  "andes_45_vpu_pipe + andes_45_lsu + andes_45_vpu_lsu")

(define_insn_reservation "andes_45_vpu_alu" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vialu,viwalu,vicalu,vsalu,vaalu,vector"))
  "andes_45_vpu_pipe + andes_45_vpu_alu")

(define_insn_reservation "andes_45_vpu_ext" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vext"))
  "andes_45_vpu_pipe + andes_45_vpu_permut")

(define_insn_reservation "andes_45_vpu_shift" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vshift,vnshift,vsshift"))
  "andes_45_vpu_pipe + andes_45_vpu_alu")

(define_insn_reservation "andes_45_vpu_minmax" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "viminmax"))
  "andes_45_vpu_pipe + andes_45_vpu_alu")

(define_insn_reservation "andes_45_vpu_cmp" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vicmp"))
  "andes_45_vpu_pipe + andes_45_vpu_alu")

(define_insn_reservation "andes_45_vpu_mul" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vimul,viwmul,vsmul"))
  "andes_45_vpu_pipe + andes_45_vpu_mac")

(define_insn_reservation "andes_45_vpu_div" 7
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vidiv"))
  "andes_45_vpu_pipe + andes_45_vpu_div * 7")

(define_insn_reservation "andes_45_vpu_madd" 4
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vimuladd,viwmuladd"))
  "andes_45_vpu_pipe + andes_45_vpu_mac")

(define_insn_reservation "andes_45_vpu_merge" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vimerge"))
  "andes_45_vpu_pipe + andes_45_vpu_alu")

(define_insn_reservation "andes_45_vpu_move" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vimov,vimovvx,vimovxv,vmov,vslideup,vslidedown,vislide1up,vislide1down"))
  "andes_45_vpu_pipe + andes_45_vpu_permut")

(define_insn_reservation "andes_45_vpu_clip" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vnclip"))
  "andes_45_vpu_pipe + andes_45_vpu_alu")

(define_insn_reservation "andes_45_vpu_falu" 4
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfalu,vfwalu,vfmul,vfwmul"))
  "andes_45_vpu_pipe + andes_45_vpu_mac")

(define_insn_reservation "andes_45_vpu_fdiv" 38
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfdiv,vfsqrt"))
  "andes_45_vpu_pipe + andes_45_vpu_fdiv")

(define_insn_reservation "andes_45_vpu_fmadd" 5
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfmuladd,vfwmuladd"))
  "andes_45_vpu_pipe + andes_45_vpu_mac")

(define_insn_reservation "andes_45_vpu_fminmax" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfminmax"))
  "andes_45_vpu_pipe + andes_45_vpu_fmis")

(define_insn_reservation "andes_45_vpu_fcmp" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfcmp,vfrecp"))
  "andes_45_vpu_pipe + andes_45_vpu_fmis")

(define_insn_reservation "andes_45_vpu_fsgnj" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfsgnj"))
  "andes_45_vpu_pipe + andes_45_vpu_fmis")

(define_insn_reservation "andes_45_vpu_fclass" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfclass"))
  "andes_45_vpu_pipe + andes_45_vpu_fmis")

(define_insn_reservation "andes_45_vpu_fmerge" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfmerge"))
  "andes_45_vpu_pipe + andes_45_vpu_fmis")

(define_insn_reservation "andes_45_vpu_fmove" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfmov,vfmovvf,vfmovfv,vfslide1up,vfslide1down"))
  "andes_45_vpu_pipe + andes_45_vpu_permut")

(define_insn_reservation "andes_45_vpu_fcvt" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,vfwcvtftof,
			vfncvtitof,vfncvtftoi,vfncvtftof,vfwcvtbf16,vfncvtbf16"))
  "andes_45_vpu_pipe + andes_45_vpu_fmis")

(define_insn_reservation "andes_45_vpu_red" 9
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vired,viwred"))
  "andes_45_vpu_pipe + andes_45_vpu_alu")

(define_insn_reservation "andes_45_vpu_fredu" 6
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfredu,vfwredu"))
  "andes_45_vpu_pipe + andes_45_vpu_mac")

(define_insn_reservation "andes_45_vpu_fredo" 34
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vfredo,vfwredo"))
  "andes_45_vpu_pipe + andes_45_vpu_mac")

(define_insn_reservation "andes_45_vpu_malu" 3
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vmalu"))
  "andes_45_vpu_pipe + andes_45_vpu_mask")

(define_insn_reservation "andes_45_vpu_mask" 4
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vmpop,vmffs,vmsfs,vmiota,vmidx"))
  "andes_45_vpu_pipe + andes_45_vpu_mask")

(define_insn_reservation "andes_45_vpu_gather" 2
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vgather"))
  "andes_45_vpu_pipe + andes_45_vpu_permut")

(define_insn_reservation "andes_45_vpu_compress" 4
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vcompress"))
  "andes_45_vpu_pipe + andes_45_vpu_permut")

(define_insn_reservation "andes_45_vcpu_csr" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "wrvxrm,wrfrm,rdvlenb,rdvl,vsetvl,vsetvl_pre"))
  "andes_45_vpu_pipe")

(define_bypass 1
  "andes_45_fpu_alu_s, andes_45_fpu_mul_s, andes_45_fpu_mac_s"
  "andes_45_load_wd, andes_45_load_bh, andes_45_store,
   andes_45_fpu_load, andes_45_fpu_store")

(define_bypass 2
  "andes_45_fpu_alu_d, andes_45_fpu_mul_d, andes_45_fpu_mac_d"
  "andes_45_load_wd, andes_45_load_bh, andes_45_store,
   andes_45_fpu_load, andes_45_fpu_store")

(define_bypass 1
  "andes_45_fpu_cmp, andes_45_fpu_cvt"
  "andes_45_load_wd, andes_45_load_bh, andes_45_store,
   andes_45_fpu_load, andes_45_fpu_store, andes_45_alu_insn_s,
   andes_45_alu_insn_l, andes_45_xfer")

(define_insn_reservation "andes_45_unknown" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "ghost,cpop,clz,ctz,zicond,mvpair,sfb_alu,minu,maxu,
			min,max,clmul,rotate,crypto,condmove,rdfrm"))
  "andes_45_dummies")

(define_insn_reservation "andes_45_vector_unknown" 1
  (and (eq_attr "tune" "andes_45_series")
       (eq_attr "type" "vclz,vror,vsha2ch,vsm4k,vaesef,vghsh,vsm4r,vsm3c,
			vaeskf1,vandn,vaesdm,vclmul,vclmulh,vrol,vcpop,vbrev8,
			vsm3me,vbrev,vctz,vgmul,vsha2ms,vaesz,vrev8,
			vaeskf2,vsha2cl,vwsll,vaesdf,vaesem,vfwmaccbf16,
			sf_vqmacc,sf_vc,sf_vc_se,sf_vfnrclip,vlsegde"))
  "andes_45_vector_dummies")
