;; Tenstorrent Ascalon 8 wide code scheduling model.
;; Copyright (C) 2023-2026 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "tt_ascalon_d8")

;; Ascalon has more issue/execution bandwidth than decode/retire bandwidth,
;; so we model decode to place an upper limit on throughput.
(define_cpu_unit "asc-decode0,asc-decode1,asc-decode2,asc-decode3,asc-decode4,asc-decode5,asc-decode6,asc-decode7" "tt_ascalon_d8")

(define_cpu_unit "asc-lsu0,asc-lsu1,asc-lsu2" "tt_ascalon_d8")
(define_cpu_unit "asc-fxu0,asc-fxu1,asc-fxu2,asc-fxu3,asc-fxu4,asc-fxu5" "tt_ascalon_d8")
(define_cpu_unit "asc-fpu0,asc-fpu1" "tt_ascalon_d8")
(define_cpu_unit "asc-vec0,asc-vec1" "tt_ascalon_d8")

;; Shortcuts
(define_reservation "tt_ascalon_d8_decode" "asc-decode0|asc-decode1|asc-decode2|asc-decode3|asc-decode4|asc-decode5|asc-decode6|asc-decode7")
(define_reservation "tt_ascalon_d8_ls" "asc-lsu0|asc-lsu1|asc-lsu2")
(define_reservation "tt_ascalon_d8_alu" "asc-fxu0|asc-fxu1|asc-fxu2|asc-fxu3|asc-fxu4|asc-fxu5")
(define_reservation "tt_ascalon_d8_mul" "asc-fxu0")
(define_reservation "tt_ascalon_d8_div" "asc-fxu0")
(define_reservation "tt_ascalon_d8_br" "asc-fxu2|asc-fxu3")
(define_reservation "tt_ascalon_d8_fp0" "asc-fpu0")
(define_reservation "tt_ascalon_d8_fp1" "asc-fpu1")
(define_reservation "tt_ascalon_d8_fp" "asc-fpu0|asc-fpu1")
(define_reservation "tt_ascalon_d8_vec0" "asc-vec0")
(define_reservation "tt_ascalon_d8_vec1" "asc-vec1")
(define_reservation "tt_ascalon_d8_vec" "asc-vec0|asc-vec1")

;; Integer/float load/store
(define_insn_reservation "tt_ascalon_d8_int_load" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "load,store,fpload,fpstore"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_ls")

;; Generic integer instructions.
(define_insn_reservation "tt_ascalon_d8_alu" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "unknown,const,arith,shift,slt,multi,auipc,nop,logical,\
			move,bitmanip,rotate,min,max,minu,maxu,clz,ctz,atomic,\
			condmove,mvpair,zicond"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_alu")

;; Short forward branch
(define_insn_reservation "tt_ascalon_d8_sfb" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "sfb_alu"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_br")

;; Branch instructions
(define_insn_reservation "tt_ascalon_d8_branch" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "branch,jump,call,jalr,ret,trap"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_br")

;; Float move, convert and compare.
;; INT -> FP moves are executed by the FXU and FP -> INT moves
;; are executed by the FPU, but we can't model that at the moment.
(define_insn_reservation "tt_ascalon_d8_float_move" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "fmove"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_fp")

(define_insn_reservation "tt_ascalon_d8_fcvt" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "fcvt,fcvt_i2f,fcvt_f2i"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_fp")

(define_insn_reservation "tt_ascalon_d8_fcmp" 2
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "fcmp"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_fp")

;; Integer multiplication.
(define_insn_reservation "tt_ascalon_d8_imul" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "imul"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_mul")

;; Integer division is not pipelined.  Do not block the unit for more than
;; three cycles so the DFA does not get too large.  Similar for other
;; non-pipelined instructions. Division is variable cycles so pick a value
;; in the middle.
(define_insn_reservation "tt_ascalon_d8_idiv" 15
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "idiv"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_div,tt_ascalon_d8_div*3")

;; Float addition.
(define_insn_reservation "tt_ascalon_d8_fadd" 2
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "fadd"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_fp")

;; Float multiplication.
(define_insn_reservation "tt_ascalon_d8_mul" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "fmul"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_fp")

;; Float FMA.
(define_insn_reservation "tt_ascalon_d8_float_fma" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "fmadd"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_fp")

;; Float division.
(define_insn_reservation "tt_ascalon_d8_float_div_half" 6
  (and (eq_attr "tune" "tt_ascalon_d8")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "HF")))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_fp0*3 | tt_ascalon_d8_fp1*3)")

(define_insn_reservation "tt_ascalon_d8_float_div_single" 9
  (and (eq_attr "tune" "tt_ascalon_d8")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "SF")))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_fp0*3 | tt_ascalon_d8_fp1*3)")

(define_insn_reservation "tt_ascalon_d8_float_div_double" 16
  (and (eq_attr "tune" "tt_ascalon_d8")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "DF")))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_fp0*3 | tt_ascalon_d8_fp1*3)")

;; Popcount and clmul.
(define_insn_reservation "tt_ascalon_d8_popcount" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "cpop,clmul"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_alu")

;; Vector loads and stores
(define_insn_reservation "tt_ascalon_d8_vec_load" 5
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vlde,vldm,vldff,vldr"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_ls")

(define_insn_reservation "tt_ascalon_d8_vec_store" 5
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vste,vstm,vstr"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_ls")

(define_insn_reservation "tt_ascalon_d8_vec_load_strided" 16
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vlds"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_ls")

(define_insn_reservation "tt_ascalon_d8_vec_store_strided" 16
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vsts"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_ls")

(define_insn_reservation "tt_ascalon_d8_vec_load_indexed" 16
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vldux,vldox"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_ls")

(define_insn_reservation "tt_ascalon_d8_vec_store_indexed" 16
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vstux,vstox"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_ls")

;; Vector segment loads/stores.
(define_insn_reservation "tt_ascalon_d8_vec_loadstore_seg" 11
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vlsegde,vlsegds,vlsegdux,vlsegdox,vlsegdff,\
			vssegte,vssegts,vssegtux,vssegtox"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Regular vector operations and integer comparisons.
(define_insn_reservation "tt_ascalon_d8_vec_alu_1" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vector,vialu,vicalu,viminmax,vimov,vmov,vfmov,\
			vandn,vbrev,vbrev8,vrev8,vclz,vctz,vrol,vror"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

(define_insn_reservation "tt_ascalon_d8_vec_alu_2" 2
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vshift,vsshift,vsalu,vaalu,vext"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

(define_insn_reservation "tt_ascalon_d8_vec_alu_3" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "viwalu,vnshift,vwsll,vicmp"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

(define_insn_reservation "tt_ascalon_d8_vec_alu_4" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vnclip"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector float comparison, conversion etc.
(define_insn_reservation "tt_ascalon_d8_vec_fcmp" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vfrecp,vfminmax,vfcmp,vfsgnj,vfclass,vfcvtitof,\
			vfcvtftoi,vfwcvtitof,vfwcvtftoi,vfwcvtftof,vfncvtitof,\
			vfncvtftoi,vfncvtftof,vfncvtbf16,vfwcvtbf16"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector integer multiplication.
(define_insn_reservation "tt_ascalon_d8_vec_imul" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vimul,viwmul,vimuladd,viwmuladd,vsmul,vclmul,vclmulh,\
			vghsh,vgmul"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector float addition.
(define_insn_reservation "tt_ascalon_d8_vec_fadd" 2
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vfalu,vfwalu"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector float multiplication and FMA.
(define_insn_reservation "tt_ascalon_d8_vec_fmul" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vfmul,vfwmul,vfmuladd,vfwmuladd,vfwmaccbf16"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector population count.
(define_insn_reservation "tt_ascalon_d8_vec_pop" 2
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vcpop"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector crypto, AES.
(define_insn_reservation "tt_ascalon_d8_vec_crypto_aes" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "crypto,vaesef,vaesem,vaesdf,vaesdm,vaeskf1,vaeskf2,vaesz"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector SHA.
(define_insn_reservation "tt_ascalon_d8_vec_crypto_sha" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vsha2ms,vsha2ch,vsha2cl"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector permute.
(define_insn_reservation "tt_ascalon_d8_vec_perm_1" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vimerge,vfmerge"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

(define_insn_reservation "tt_ascalon_d8_vec_perm_3" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vislide1up,vislide1down,vfslide1up,vfslide1down,\
			vgather,vcompress"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

(define_insn_reservation "tt_ascalon_d8_vec_perm_4" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vslideup,vslidedown"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector reduction.
(define_insn_reservation "tt_ascalon_d8_vec_reduction" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vired,viwred,vfredu,vfwredu"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector ordered reduction, assume the latency number is for
;; a 256-bit vector.  We'll need to scale it similar to
;; riscv_sched_adjust_cost for larger vectors.
;; NOTE: The latency depends on the SEW, we should be checking it
(define_insn_reservation "tt_ascalon_d8_vec_ordered_reduction" 10
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vfredo,vfwredo"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

;; Vector integer division
(define_insn_reservation "tt_ascalon_d8_vec_idiv_byte" 16
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vidiv")
       (eq_attr "sew" "8"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

(define_insn_reservation "tt_ascalon_d8_vec_idiv_half" 16
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vidiv")
       (eq_attr "sew" "16"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

(define_insn_reservation "tt_ascalon_d8_vec_idiv_single" 13
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vidiv")
       (eq_attr "sew" "32"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

(define_insn_reservation "tt_ascalon_d8_vec_idiv_double" 20
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vidiv")
       (eq_attr "sew" "64"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

;; Vector float divisions and sqrt
(define_insn_reservation "tt_ascalon_d8_vec_float_divsqrt_half" 11
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vfdiv,vfsqrt")
       (eq_attr "sew" "16"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

(define_insn_reservation "tt_ascalon_d8_vec_float_divsqrt_single" 10
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vfdiv,vfsqrt")
       (eq_attr "sew" "32"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

(define_insn_reservation "tt_ascalon_d8_vec_float_divsqrt_double" 17
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vfdiv,vfsqrt")
       (eq_attr "sew" "64"))
  "tt_ascalon_d8_decode,(tt_ascalon_d8_vec0*3 | tt_ascalon_d8_vec1*3)")

;; Vector mask operations.
(define_insn_reservation "tt_ascalon_d8_vec_mask" 3
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,\
			vfmovvf,vfmovfv"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector vsetvl.
(define_insn_reservation "tt_ascalon_d8_vec_vesetvl" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "vsetvl,vsetvl_pre"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector rounding mode setters, assume pipeline barrier.
(define_insn_reservation "tt_ascalon_d8_vec_setrm" 20
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "wrvxrm,wrfrm"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec*3")

;; Vector read vlen/vlenb.
(define_insn_reservation "tt_ascalon_d8_vec_readlen" 4
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "rdvlenb,rdvl"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")

;; Vector unknown.
(define_insn_reservation "tt_ascalon_d8_vec_unknown" 1
  (and (eq_attr "tune" "tt_ascalon_d8")
       (eq_attr "type" "sf_vc,sf_vc_se,sf_vqmacc,sf_vfnrclip,\
			vsm3me,vsm4r,vsm4k,vsm3c,ghost,\
			mfc,mtc,rdfrm"))
  "tt_ascalon_d8_decode,tt_ascalon_d8_vec")
