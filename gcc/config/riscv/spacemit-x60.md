;; spacemit_x60 DFA-based pipeline description for RISC-V targets.
;; Copyright (C) 2011-2026 Free Software Foundation, Inc.
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
;; one of the two units.  For the purposes of this scheduling
;; model, the VXU is treated as a single unit.
;;
;; Some ops like shadd.uw and add.uw, cpop take an extra cycle
;; Given everything is in-order, anti-dependencies probably matter
;; FP sign injection isn't handled correctly
;; ----------------------------------------------------

(define_automaton "spacemit_x60")
(define_cpu_unit "spacemit_x60_alu0,spacemit_x60_alu1" "spacemit_x60")
(define_cpu_unit "spacemit_x60_lsu0,spacemit_x60_lsu1" "spacemit_x60")
(define_cpu_unit "spacemit_x60_vxu0" "spacemit_x60")
(define_cpu_unit "spacemit_x60_fpalu" "spacemit_x60")
(define_cpu_unit "spacemit_x60_fdivsqrt" "spacemit_x60")

(define_reservation "spacemit_x60_lsu" "spacemit_x60_lsu0, spacemit_x60_lsu1")
(define_reservation "spacemit_x60_alu" "(spacemit_x60_alu0|spacemit_x60_alu1)")

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

(define_insn_reservation "spacemit_x60_idivsi" 7
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "SI")))
  "spacemit_x60_alu0*7")

(define_insn_reservation "spacemit_x60_idivdi" 7
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "DI")))
  "spacemit_x60_alu0*7")

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
			condmove,crypto,mvpair,zicond"))
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

(define_insn_reservation "spacemit_x60_fdiv_half" 7
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "HF")))
  "spacemit_x60_fdivsqrt*7")

(define_insn_reservation "spacemit_x60_fdiv_single" 7
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "SF")))
  "spacemit_x60_fdivsqrt*7")

(define_insn_reservation "spacemit_x60_fdiv_double" 7
  (and (eq_attr "tune" "spacemit_x60")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "DF")))
  "spacemit_x60_fdivsqrt*7")

;; ----------------------------------------------------
;; Vector
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vsetvl" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vsetvl_pre,vsetvl,rdvlenb,rdvl"))
  "spacemit_x60_vxu0")

;; ----------------------------------------------------
;; Vector Memory (load/store)
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vec_load" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vlde,vldr,vldff"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_store" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vste,vstr,vstm"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_mask_load" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vldm"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_strided" 7
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vlds,vsts"))
  "spacemit_x60_vxu0*7")

(define_insn_reservation "spacemit_x60_vec_indexed" 7
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vldux,vldox,vstux,vstox,\
      vlsegdux,vlsegdox,vssegtux,vssegtox"))
  "spacemit_x60_vxu0*7")

(define_insn_reservation "spacemit_x60_vec_segmented" 7
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vlsegde,vlsegds,vlsegdff,vssegte,vssegts"))
  "spacemit_x60_vxu0*7")

;; ----------------------------------------------------
;; Vector Integer Arithmetic
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vec_ialu" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vialu,vicalu,viminmax,vbrev,vext,\
       vbrev8,vrev8,vclz,vctz,vcpop,vrol,vror,vandn"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_shift" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vshift"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_sshift" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vsshift"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_cmp" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vicmp"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_imul" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vimul"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_imuladd" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vimuladd"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_iwalu" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "viwalu,viwmul,viwmuladd,vwsll"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_div" 7
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vidiv"))
  "spacemit_x60_vxu0*7")

;; ----------------------------------------------------
;; Vector Fixed Point
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vec_fixed_point" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vsalu,vaalu"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_fixed_point_slow" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vsmul"))
  "spacemit_x60_vxu0*4")

(define_insn_reservation "spacemit_x60_vec_narrow_fixed_point" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vnshift,vnclip"))
  "spacemit_x60_vxu0*4")

;; ----------------------------------------------------
;; Vector Floating Point
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vec_falu" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vfalu,vfmul,vfmuladd,vfminmax,vfrecp,\
       vfcvtitof,vfcvtftoi,vfmerge"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_fwalu" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vfwalu,vfwmul,vfwmuladd,vfwmaccbf16,vfwcvtitof,\
      vfwcvtftoi,vfwcvtftof,vfwcvtbf16,vfncvtitof,vfncvtftoi,vfncvtftof,\
      vfncvtbf16,sf_vfnrclip,sf_vqmacc"))
  "spacemit_x60_vxu0*4")

(define_insn_reservation "spacemit_x60_vec_fcmp" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vfcmp"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_fmov" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vfmovfv,vfmov"))
  "spacemit_x60_vxu0*4")

(define_insn_reservation "spacemit_x60_vec_fsimple" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vfmovvf,vfclass,vfsgnj"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_fsqrt_fdiv" 7
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vfsqrt,vfdiv"))
  "spacemit_x60_vxu0*7")

;; ----------------------------------------------------
;; Vector Mask Operations
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vec_mask_logical" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vmalu"))
  "spacemit_x60_vxu0")

(define_insn_reservation "spacemit_x60_vec_mask_pop" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vmpop,vmffs"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_mask_special" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vmidx,vmiota"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_mask_vmsfs" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vmsfs"))
  "spacemit_x60_vxu0")

;; ----------------------------------------------------
;; Vector Permutations
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vec_mov" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vmov,vimovxv,vimovvx,vimov"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_merge" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vimerge"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_gather" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vgather"))
  "spacemit_x60_vxu0*4")

(define_insn_reservation "spacemit_x60_vec_compress" 4
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vcompress"))
  "spacemit_x60_vxu0*3")

(define_insn_reservation "spacemit_x60_vec_slide" 2
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vslideup,vslidedown,vislide1up,\
      vislide1down,vfslide1up,vfslide1down"))
  "spacemit_x60_vxu0")

;; ----------------------------------------------------
;; Vector Reductions
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_vec_red" 7
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vired,viwred,vfredu,vfwredu"))
  "spacemit_x60_vxu0*2")

(define_insn_reservation "spacemit_x60_vec_ordered_red" 7
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vfredo,vfwredo"))
  "spacemit_x60_vxu0*7")

;; ----------------------------------------------------
;; Vector Unknown
;; ----------------------------------------------------

(define_insn_reservation "spacemit_x60_dummy" 1
  (and (eq_attr "tune" "spacemit_x60")
       (eq_attr "type" "vaesef,
      vsha2cl,vsha2ch,vaesz,vclmulh,vghsh,vaeskf1,vaesdm,vgmul,
      vaesem,vaesdf,ghost,vsm3me,sf_vc_se,vsm3c,
      vaeskf2,vsha2ms,vector,wrvxrm,
      vclmul,rdfrm,wrfrm,sf_vc,vsm4k,vsm4r"))
  "nothing")
