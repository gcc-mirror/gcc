;; Copyright (C) 2024-2024 Free Software Foundation, Inc.

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
;; Vector load/store

(define_automaton "vector_ooo")

;; Separate issue queue for vector instructions.
(define_cpu_unit "vxu_ooo_issue" "vector_ooo")

;; Vector execution unit.
(define_cpu_unit "vxu_ooo_alu" "vector_ooo")

;; Vector subunit that does mult/div/sqrt.
(define_cpu_unit "vxu_ooo_multicycle" "vector_ooo")

(define_insn_reservation "vec_load" 6
  (eq_attr "type" "vlde,vldm,vlds,vldux,vldox,vldff,vldr")
  "vxu_ooo_issue,vxu_ooo_alu")

(define_insn_reservation "vec_store" 6
  (eq_attr "type" "vste,vstm,vsts,vstux,vstox,vstr")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector segment loads/stores.
(define_insn_reservation "vec_loadstore_seg" 10
  (eq_attr "type" "vlsegde,vlsegds,vlsegdux,vlsegdox,vlsegdff,\
		   vssegte,vssegts,vssegtux,vssegtox")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Regular vector operations and integer comparisons.
(define_insn_reservation "vec_alu" 3
  (eq_attr "type" "vialu,viwalu,vext,vicalu,vshift,vnshift,viminmax,vicmp,\
		   vimov,vsalu,vaalu,vsshift,vnclip,vmov,vfmov,vector,\
		   vandn,vbrev,vbrev8,vrev8,vclz,vctz,vrol,vror,vwsll")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector float comparison, conversion etc.
(define_insn_reservation "vec_fcmp" 3
  (eq_attr "type" "vfrecp,vfminmax,vfcmp,vfsgnj,vfclass,vfcvtitof,\
                   vfcvtftoi,vfwcvtitof,vfwcvtftoi,vfwcvtftof,vfncvtitof,\
                   vfncvtftoi,vfncvtftof,vfncvtbf16,vfwcvtbf16")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector integer multiplication.
(define_insn_reservation "vec_imul" 4
  (eq_attr "type" "vimul,viwmul,vimuladd,viwmuladd,vsmul,vclmul,vclmulh,\
                   vghsh,vgmul")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector float addition.
(define_insn_reservation "vec_fadd" 4
  (eq_attr "type" "vfalu,vfwalu")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector float multiplication and FMA.
(define_insn_reservation "vec_fmul" 6
  (eq_attr "type" "vfmul,vfwmul,vfmuladd,vfwmuladd,vfwmaccbf16,sf_vqmacc,sf_vfnrclip")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector crypto, assumed to be a generic operation for now.
(define_insn_reservation "vec_crypto" 4
  (eq_attr "type" "crypto,vclz,vctz,vcpop")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector crypto, AES
(define_insn_reservation "vec_crypto_aes" 4
  (eq_attr "type" "vaesef,vaesem,vaesdf,vaesdm,vaeskf1,vaeskf2,vaesz")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector crypto, sha
(define_insn_reservation "vec_crypto_sha" 4
  (eq_attr "type" "vsha2ms,vsha2ch,vsha2cl")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector crypto, SM3/4
(define_insn_reservation "vec_crypto_sm" 4
  (eq_attr "type" "vsm4k,vsm4r,vsm3me,vsm3c")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector permute.
(define_insn_reservation "vec_perm" 3
  (eq_attr "type" "vimerge,vfmerge,vslideup,vslidedown,vislide1up,\
                   vislide1down,vfslide1up,vfslide1down,vgather,vcompress")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector reduction.
(define_insn_reservation "vec_reduction" 8
  (eq_attr "type" "vired,viwred,vfredu,vfwredu")
  "vxu_ooo_issue,vxu_ooo_multicycle")

;; Vector ordered reduction, assume the latency number is for
;; a 128-bit vector.  It is scaled in riscv_sched_adjust_cost
;; for larger vectors.
(define_insn_reservation "vec_ordered_reduction" 10
  (eq_attr "type" "vfredo,vfwredo")
  "vxu_ooo_issue,vxu_ooo_multicycle*3")

;; Vector integer division, assume not pipelined.
(define_insn_reservation "vec_idiv" 16
  (eq_attr "type" "vidiv")
  "vxu_ooo_issue,vxu_ooo_multicycle*3")

;; Vector float divisions and sqrt, assume not pipelined.
(define_insn_reservation "vec_float_divsqrt" 16
  (eq_attr "type" "vfdiv,vfsqrt")
  "vxu_ooo_issue,vxu_ooo_multicycle*3")

;; Vector mask operations.
(define_insn_reservation "vec_mask" 2
  (eq_attr "type" "vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,\
                   vfmovvf,vfmovfv")
  "vxu_ooo_issue,vxu_ooo_alu")

;; Vector vsetvl.
(define_insn_reservation "vec_vesetvl" 1
  (eq_attr "type" "vsetvl,vsetvl_pre")
  "vxu_ooo_issue")

;; Vector rounding mode setters, assume pipeline barrier.
(define_insn_reservation "vec_setrm" 20
  (eq_attr "type" "wrvxrm,wrfrm")
  "vxu_ooo_issue,vxu_ooo_issue*3")

;; Vector read vlen/vlenb.
(define_insn_reservation "vec_readlen" 4
  (eq_attr "type" "rdvlenb,rdvl")
  "vxu_ooo_issue,vxu_ooo_issue")

