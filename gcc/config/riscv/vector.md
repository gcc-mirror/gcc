;; Machine description for RISC-V 'V' Extension for GNU compiler.
;; Copyright (C) 2022-2024 Free Software Foundation, Inc.
;; Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

;; This file describes the RISC-V 'V' Extension, Version 1.0.
;;
;; This file include :
;;
;; - Intrinsics (https://github.com/riscv/rvv-intrinsic-doc)
;; - Auto-vectorization (autovec.md)
;; - Optimization (autovec-opt.md)

(include "vector-iterators.md")

(define_constants [
   (INVALID_ATTRIBUTE            255)
   (X0_REGNUM                      0)
])

;; True if the type is RVV instructions that include VTYPE
;; global status register in the use op list.
;; We known VTYPE has 4 fields: SEW, LMUL, TA, MA.
;; The instruction need any of VTYPE field is set as true
;; in this attribute.
(define_attr "has_vtype_op" "false,true"
  (cond [(eq_attr "type" "vlde,vste,vldm,vstm,vlds,vsts,\
			  vldux,vldox,vstux,vstox,vldff,\
			  vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,viminmax,\
			  vimul,vidiv,viwmul,vimuladd,viwmuladd,vimerge,vimov,\
			  vsalu,vaalu,vsmul,vsshift,vnclip,\
			  vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,\
			  vfcmp,vfminmax,vfsgnj,vfclass,vfmerge,vfmov,\
			  vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,\
			  vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,\
			  vired,viwred,vfredu,vfredo,vfwredu,vfwredo,\
			  vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,vfmovvf,vfmovfv,\
			  vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,\
			  vgather,vcompress,vlsegde,vssegte,vlsegds,vssegts,vlsegdux,vlsegdox,\
			  vssegtux,vssegtox,vlsegdff,vandn,vbrev,vbrev8,vrev8,vclz,vctz,vrol,\
			  vror,vwsll,vclmul,vclmulh,vghsh,vgmul,vaesef,vaesem,vaesdf,vaesdm,\
			  vaeskf1,vaeskf2,vaesz,vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm4r,vsm3me,vsm3c")
	 (const_string "true")]
	(const_string "false")))

;; True if the type is RVV instructions that include VL
;; global status register in the use op list.
;; The instruction need vector length to be specified is set
;; in this attribute.
(define_attr "has_vl_op" "false,true"
  (cond [(eq_attr "type" "vlde,vste,vldm,vstm,vlds,vsts,\
			  vldux,vldox,vstux,vstox,vldff,\
			  vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,viminmax,\
			  vimul,vidiv,viwmul,vimuladd,viwmuladd,vimerge,vimov,\
			  vsalu,vaalu,vsmul,vsshift,vnclip,\
			  vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,\
			  vfcmp,vfminmax,vfsgnj,vfclass,vfmerge,vfmov,\
			  vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,\
			  vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,\
			  vired,viwred,vfredu,vfredo,vfwredu,vfwredo,\
			  vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovxv,vfmovfv,\
			  vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,\
			  vgather,vcompress,vlsegde,vssegte,vlsegds,vssegts,vlsegdux,vlsegdox,\
			  vssegtux,vssegtox,vlsegdff,vandn,vbrev,vbrev8,vrev8,vclz,vctz,vrol,\
			  vror,vwsll,vclmul,vclmulh,vghsh,vgmul,vaesef,vaesem,vaesdf,vaesdm,\
			  vaeskf1,vaeskf2,vaesz,vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm4r,vsm3me,vsm3c")
	 (const_string "true")]
	(const_string "false")))

;; The default SEW of RVV instruction. This attribute doesn't mean the instruction
;; is necessary to require SEW check for example vlm.v which require ratio to
;; check. However, we need default value of SEW for vsetvl instruction since there
;; is no field for ratio in the vsetvl instruction encoding.
(define_attr "sew" ""
  (cond [(eq_attr "mode" "RVVMF8BI,RVVMF4BI,RVVMF2BI,RVVM1BI,\
			  RVVM8QI,RVVM4QI,RVVM2QI,RVVM1QI,RVVMF2QI,RVVMF4QI,RVVMF8QI,\
			  RVVM1x8QI,RVVMF2x8QI,RVVMF4x8QI,RVVMF8x8QI,\
			  RVVM1x7QI,RVVMF2x7QI,RVVMF4x7QI,RVVMF8x7QI,\
			  RVVM1x6QI,RVVMF2x6QI,RVVMF4x6QI,RVVMF8x6QI,\
			  RVVM1x5QI,RVVMF2x5QI,RVVMF4x5QI,RVVMF8x5QI,\
			  RVVM2x4QI,RVVM1x4QI,RVVMF2x4QI,RVVMF4x4QI,RVVMF8x4QI,\
			  RVVM2x3QI,RVVM1x3QI,RVVMF2x3QI,RVVMF4x3QI,RVVMF8x3QI,\
			  RVVM4x2QI,RVVM2x2QI,RVVM1x2QI,RVVMF2x2QI,RVVMF4x2QI,RVVMF8x2QI,\
			  V1QI,V2QI,V4QI,V8QI,V16QI,V32QI,V64QI,V128QI,V256QI,V512QI,V1024QI,V2048QI,V4096QI,\
			  V1BI,V2BI,V4BI,V8BI,V16BI,V32BI,V64BI,V128BI,V256BI,V512BI,V1024BI,V2048BI,V4096BI")
	 (const_int 8)
	 (eq_attr "mode" "RVVMF16BI")
	   (if_then_else (match_test "TARGET_XTHEADVECTOR")
	     (const_int 16)
	     (const_int 8))
	 (eq_attr "mode" "RVVMF32BI")
	   (if_then_else (match_test "TARGET_XTHEADVECTOR")
	     (const_int 32)
	     (const_int 8))
	 (eq_attr "mode" "RVVMF64BI")
	   (if_then_else (match_test "TARGET_XTHEADVECTOR")
	     (const_int 64)
	     (const_int 8))
	 (eq_attr "mode" "RVVM8HI,RVVM4HI,RVVM2HI,RVVM1HI,RVVMF2HI,RVVMF4HI,\
			  RVVM1x8HI,RVVMF2x8HI,RVVMF4x8HI,\
			  RVVM1x7HI,RVVMF2x7HI,RVVMF4x7HI,\
			  RVVM1x6HI,RVVMF2x6HI,RVVMF4x6HI,\
			  RVVM1x5HI,RVVMF2x5HI,RVVMF4x5HI,\
			  RVVM2x4HI,RVVM1x4HI,RVVMF2x4HI,RVVMF4x4HI,\
			  RVVM2x3HI,RVVM1x3HI,RVVMF2x3HI,RVVMF4x3HI,\
			  RVVM4x2HI,RVVM2x2HI,RVVM1x2HI,RVVMF2x2HI,RVVMF4x2HI,\
			  RVVM8HF,RVVM4HF,RVVM2HF,RVVM1HF,RVVMF2HF,RVVMF4HF,\
			  RVVM1x8HF,RVVMF2x8HF,RVVMF4x8HF,\
			  RVVM1x7HF,RVVMF2x7HF,RVVMF4x7HF,\
			  RVVM1x6HF,RVVMF2x6HF,RVVMF4x6HF,\
			  RVVM1x5HF,RVVMF2x5HF,RVVMF4x5HF,\
			  RVVM2x4HF,RVVM1x4HF,RVVMF2x4HF,RVVMF4x4HF,\
			  RVVM2x3HF,RVVM1x3HF,RVVMF2x3HF,RVVMF4x3HF,\
			  RVVM4x2HF,RVVM2x2HF,RVVM1x2HF,RVVMF2x2HF,RVVMF4x2HF,\
			  V1HI,V2HI,V4HI,V8HI,V16HI,V32HI,V64HI,V128HI,V256HI,V512HI,V1024HI,V2048HI,\
			  V1HF,V2HF,V4HF,V8HF,V16HF,V32HF,V64HF,V128HF,V256HF,V512HF,V1024HF,V2048HF")
	 (const_int 16)
	 (eq_attr "mode" "RVVM8SI,RVVM4SI,RVVM2SI,RVVM1SI,RVVMF2SI,\
			  RVVM8SF,RVVM4SF,RVVM2SF,RVVM1SF,RVVMF2SF,\
			  RVVM1x8SI,RVVMF2x8SI,\
			  RVVM1x7SI,RVVMF2x7SI,\
			  RVVM1x6SI,RVVMF2x6SI,\
			  RVVM1x5SI,RVVMF2x5SI,\
			  RVVMF2x4SI,RVVMF2x3SI,\
			  RVVM2x4SI,RVVM1x4SI,\
			  RVVM2x3SI,RVVM1x3SI,\
			  RVVM4x2SI,RVVM2x2SI,RVVM1x2SI,RVVMF2x2SI,\
			  RVVM1x8SF,RVVMF2x8SF,\
			  RVVM1x7SF,RVVMF2x7SF,\
			  RVVM1x6SF,RVVMF2x6SF,\
			  RVVM1x5SF,RVVMF2x5SF,\
			  RVVM2x4SF,RVVM1x4SF,RVVMF2x4SF,\
			  RVVM2x3SF,RVVM1x3SF,RVVMF2x3SF,\
			  RVVM4x2SF,RVVM2x2SF,RVVM1x2SF,RVVMF2x2SF,\
			  V1SI,V2SI,V4SI,V8SI,V16SI,V32SI,V64SI,V128SI,V256SI,V512SI,V1024SI,\
			  V1SF,V2SF,V4SF,V8SF,V16SF,V32SF,V64SF,V128SF,V256SF,V512SF,V1024SF")
	 (const_int 32)
	 (eq_attr "mode" "RVVM8DI,RVVM4DI,RVVM2DI,RVVM1DI,\
			  RVVM8DF,RVVM4DF,RVVM2DF,RVVM1DF,\
			  RVVM1x8DI,RVVM1x7DI,RVVM1x6DI,RVVM1x5DI,\
			  RVVM2x4DI,RVVM1x4DI,\
			  RVVM2x3DI,RVVM1x3DI,\
			  RVVM4x2DI,RVVM2x2DI,RVVM1x2DI,\
			  RVVM1x8DF,RVVM1x7DF,RVVM1x6DF,RVVM1x5DF,\
			  RVVM2x4DF,RVVM1x4DF,\
			  RVVM2x3DF,RVVM1x3DF,\
			  RVVM4x2DF,RVVM2x2DF,RVVM1x2DF,\
			  V1DI,V2DI,V4DI,V8DI,V16DI,V32DI,V64DI,V128DI,V256DI,V512DI,\
			  V1DF,V2DF,V4DF,V8DF,V16DF,V32DF,V64DF,V128DF,V256DF,V512DF")
	 (const_int 64)]
	(const_int INVALID_ATTRIBUTE)))

;; Ditto to LMUL.
(define_attr "vlmul" ""
  (cond [(eq_attr "mode" "RVVM8QI,RVVM1BI") (symbol_ref "riscv_vector::LMUL_8")
	 (eq_attr "mode" "RVVM4QI,RVVMF2BI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2QI,RVVMF4BI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1QI,RVVMF8BI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2QI,RVVMF16BI") (symbol_ref "TARGET_XTHEADVECTOR ? riscv_vector::LMUL_1 : riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4QI,RVVMF32BI") (symbol_ref "TARGET_XTHEADVECTOR ? riscv_vector::LMUL_1 : riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8QI,RVVMF64BI") (symbol_ref "TARGET_XTHEADVECTOR ? riscv_vector::LMUL_1 : riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM8HI") (symbol_ref "riscv_vector::LMUL_8")
	 (eq_attr "mode" "RVVM4HI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2HI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM8HF") (symbol_ref "riscv_vector::LMUL_8")
	 (eq_attr "mode" "RVVM4HF") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2HF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM8SI") (symbol_ref "riscv_vector::LMUL_8")
	 (eq_attr "mode" "RVVM4SI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2SI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM8SF") (symbol_ref "riscv_vector::LMUL_8")
	 (eq_attr "mode" "RVVM4SF") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2SF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM8DI") (symbol_ref "riscv_vector::LMUL_8")
	 (eq_attr "mode" "RVVM4DI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2DI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM8DF") (symbol_ref "riscv_vector::LMUL_8")
	 (eq_attr "mode" "RVVM4DF") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2DF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1DF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x8QI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x8QI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x8QI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8x8QI") (symbol_ref "riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM1x7QI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x7QI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x7QI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8x7QI") (symbol_ref "riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM1x6QI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x6QI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x6QI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8x6QI") (symbol_ref "riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM1x5QI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x5QI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x5QI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8x5QI") (symbol_ref "riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM2x4QI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x4QI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x4QI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x4QI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8x4QI") (symbol_ref "riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM2x3QI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x3QI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x3QI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x3QI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8x3QI") (symbol_ref "riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM4x2QI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2x2QI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x2QI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x2QI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x2QI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVMF8x2QI") (symbol_ref "riscv_vector::LMUL_F8")
	 (eq_attr "mode" "RVVM1x8HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x8HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x8HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x7HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x7HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x7HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x6HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x6HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x6HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x5HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x5HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x5HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM2x4HI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x4HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x4HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x4HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM2x3HI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x3HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x3HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x3HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM4x2HI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2x2HI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x2HI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x2HI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x2HI") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x8HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x8HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x8HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x7HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x7HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x7HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x6HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x6HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x6HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x5HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x5HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x5HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM2x4HF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x4HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x4HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x4HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM2x3HF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x3HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x3HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x3HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM4x2HF") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2x2HF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x2HF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x2HF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVMF4x2HF") (symbol_ref "riscv_vector::LMUL_F4")
	 (eq_attr "mode" "RVVM1x8SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x8SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x7SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x7SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x6SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x6SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x5SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x5SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM2x4SI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x4SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x4SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM2x3SI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x3SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x3SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM4x2SI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2x2SI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x2SI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x2SI") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x8SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x8SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x7SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x7SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x6SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x6SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x5SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x5SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM2x4SF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x4SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x4SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM2x3SF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x3SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x3SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM4x2SF") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2x2SF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x2SF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVMF2x2SF") (symbol_ref "riscv_vector::LMUL_F2")
	 (eq_attr "mode" "RVVM1x8DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x7DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x6DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x5DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM2x4DI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x4DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM2x3DI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x3DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM4x2DI") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2x2DI") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x2DI") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x8DF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x7DF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x6DF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM1x5DF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM2x4DF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x4DF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM2x3DF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x3DF") (symbol_ref "riscv_vector::LMUL_1")
	 (eq_attr "mode" "RVVM4x2DF") (symbol_ref "riscv_vector::LMUL_4")
	 (eq_attr "mode" "RVVM2x2DF") (symbol_ref "riscv_vector::LMUL_2")
	 (eq_attr "mode" "RVVM1x2DF") (symbol_ref "riscv_vector::LMUL_1")

	 ;; VLS modes.
	 (eq_attr "mode" "V1QI,V1BI") (symbol_ref "riscv_vector::get_vlmul(E_V1QImode)")
	 (eq_attr "mode" "V2QI,V2BI") (symbol_ref "riscv_vector::get_vlmul(E_V2QImode)")
	 (eq_attr "mode" "V4QI,V4BI") (symbol_ref "riscv_vector::get_vlmul(E_V4QImode)")
	 (eq_attr "mode" "V8QI,V8BI") (symbol_ref "riscv_vector::get_vlmul(E_V8QImode)")
	 (eq_attr "mode" "V16QI,V16BI") (symbol_ref "riscv_vector::get_vlmul(E_V16QImode)")
	 (eq_attr "mode" "V32QI,V32BI") (symbol_ref "riscv_vector::get_vlmul(E_V32QImode)")
	 (eq_attr "mode" "V64QI,V64BI") (symbol_ref "riscv_vector::get_vlmul(E_V64QImode)")
	 (eq_attr "mode" "V128QI,V128BI") (symbol_ref "riscv_vector::get_vlmul(E_V128QImode)")
	 (eq_attr "mode" "V256QI,V256BI") (symbol_ref "riscv_vector::get_vlmul(E_V256QImode)")
	 (eq_attr "mode" "V512QI,V512BI") (symbol_ref "riscv_vector::get_vlmul(E_V512QImode)")
	 (eq_attr "mode" "V1024QI,V1024BI") (symbol_ref "riscv_vector::get_vlmul(E_V1024QImode)")
	 (eq_attr "mode" "V2048QI,V2048BI") (symbol_ref "riscv_vector::get_vlmul(E_V2048QImode)")
	 (eq_attr "mode" "V4096QI,V4096BI") (symbol_ref "riscv_vector::get_vlmul(E_V4096QImode)")
	 (eq_attr "mode" "V1HI") (symbol_ref "riscv_vector::get_vlmul(E_V1HImode)")
	 (eq_attr "mode" "V2HI") (symbol_ref "riscv_vector::get_vlmul(E_V2HImode)")
	 (eq_attr "mode" "V4HI") (symbol_ref "riscv_vector::get_vlmul(E_V4HImode)")
	 (eq_attr "mode" "V8HI") (symbol_ref "riscv_vector::get_vlmul(E_V8HImode)")
	 (eq_attr "mode" "V16HI") (symbol_ref "riscv_vector::get_vlmul(E_V16HImode)")
	 (eq_attr "mode" "V32HI") (symbol_ref "riscv_vector::get_vlmul(E_V32HImode)")
	 (eq_attr "mode" "V64HI") (symbol_ref "riscv_vector::get_vlmul(E_V64HImode)")
	 (eq_attr "mode" "V128HI") (symbol_ref "riscv_vector::get_vlmul(E_V128HImode)")
	 (eq_attr "mode" "V256HI") (symbol_ref "riscv_vector::get_vlmul(E_V256HImode)")
	 (eq_attr "mode" "V512HI") (symbol_ref "riscv_vector::get_vlmul(E_V512HImode)")
	 (eq_attr "mode" "V1024HI") (symbol_ref "riscv_vector::get_vlmul(E_V1024HImode)")
	 (eq_attr "mode" "V2048HI") (symbol_ref "riscv_vector::get_vlmul(E_V2048HImode)")
	 (eq_attr "mode" "V1SI") (symbol_ref "riscv_vector::get_vlmul(E_V1SImode)")
	 (eq_attr "mode" "V2SI") (symbol_ref "riscv_vector::get_vlmul(E_V2SImode)")
	 (eq_attr "mode" "V4SI") (symbol_ref "riscv_vector::get_vlmul(E_V4SImode)")
	 (eq_attr "mode" "V8SI") (symbol_ref "riscv_vector::get_vlmul(E_V8SImode)")
	 (eq_attr "mode" "V16SI") (symbol_ref "riscv_vector::get_vlmul(E_V16SImode)")
	 (eq_attr "mode" "V32SI") (symbol_ref "riscv_vector::get_vlmul(E_V32SImode)")
	 (eq_attr "mode" "V64SI") (symbol_ref "riscv_vector::get_vlmul(E_V64SImode)")
	 (eq_attr "mode" "V128SI") (symbol_ref "riscv_vector::get_vlmul(E_V128SImode)")
	 (eq_attr "mode" "V256SI") (symbol_ref "riscv_vector::get_vlmul(E_V256SImode)")
	 (eq_attr "mode" "V512SI") (symbol_ref "riscv_vector::get_vlmul(E_V512SImode)")
	 (eq_attr "mode" "V1024SI") (symbol_ref "riscv_vector::get_vlmul(E_V1024SImode)")
	 (eq_attr "mode" "V1DI") (symbol_ref "riscv_vector::get_vlmul(E_V1DImode)")
	 (eq_attr "mode" "V2DI") (symbol_ref "riscv_vector::get_vlmul(E_V2DImode)")
	 (eq_attr "mode" "V4DI") (symbol_ref "riscv_vector::get_vlmul(E_V4DImode)")
	 (eq_attr "mode" "V8DI") (symbol_ref "riscv_vector::get_vlmul(E_V8DImode)")
	 (eq_attr "mode" "V16DI") (symbol_ref "riscv_vector::get_vlmul(E_V16DImode)")
	 (eq_attr "mode" "V32DI") (symbol_ref "riscv_vector::get_vlmul(E_V32DImode)")
	 (eq_attr "mode" "V64DI") (symbol_ref "riscv_vector::get_vlmul(E_V64DImode)")
	 (eq_attr "mode" "V128DI") (symbol_ref "riscv_vector::get_vlmul(E_V128DImode)")
	 (eq_attr "mode" "V256DI") (symbol_ref "riscv_vector::get_vlmul(E_V256DImode)")
	 (eq_attr "mode" "V512DI") (symbol_ref "riscv_vector::get_vlmul(E_V512DImode)")
	 (eq_attr "mode" "V1HF") (symbol_ref "riscv_vector::get_vlmul(E_V1HFmode)")
	 (eq_attr "mode" "V2HF") (symbol_ref "riscv_vector::get_vlmul(E_V2HFmode)")
	 (eq_attr "mode" "V4HF") (symbol_ref "riscv_vector::get_vlmul(E_V4HFmode)")
	 (eq_attr "mode" "V8HF") (symbol_ref "riscv_vector::get_vlmul(E_V8HFmode)")
	 (eq_attr "mode" "V16HF") (symbol_ref "riscv_vector::get_vlmul(E_V16HFmode)")
	 (eq_attr "mode" "V32HF") (symbol_ref "riscv_vector::get_vlmul(E_V32HFmode)")
	 (eq_attr "mode" "V64HF") (symbol_ref "riscv_vector::get_vlmul(E_V64HFmode)")
	 (eq_attr "mode" "V128HF") (symbol_ref "riscv_vector::get_vlmul(E_V128HFmode)")
	 (eq_attr "mode" "V256HF") (symbol_ref "riscv_vector::get_vlmul(E_V256HFmode)")
	 (eq_attr "mode" "V512HF") (symbol_ref "riscv_vector::get_vlmul(E_V512HFmode)")
	 (eq_attr "mode" "V1024HF") (symbol_ref "riscv_vector::get_vlmul(E_V1024HFmode)")
	 (eq_attr "mode" "V2048HF") (symbol_ref "riscv_vector::get_vlmul(E_V2048HFmode)")
	 (eq_attr "mode" "V1SF") (symbol_ref "riscv_vector::get_vlmul(E_V1SFmode)")
	 (eq_attr "mode" "V2SF") (symbol_ref "riscv_vector::get_vlmul(E_V2SFmode)")
	 (eq_attr "mode" "V4SF") (symbol_ref "riscv_vector::get_vlmul(E_V4SFmode)")
	 (eq_attr "mode" "V8SF") (symbol_ref "riscv_vector::get_vlmul(E_V8SFmode)")
	 (eq_attr "mode" "V16SF") (symbol_ref "riscv_vector::get_vlmul(E_V16SFmode)")
	 (eq_attr "mode" "V32SF") (symbol_ref "riscv_vector::get_vlmul(E_V32SFmode)")
	 (eq_attr "mode" "V64SF") (symbol_ref "riscv_vector::get_vlmul(E_V64SFmode)")
	 (eq_attr "mode" "V128SF") (symbol_ref "riscv_vector::get_vlmul(E_V128SFmode)")
	 (eq_attr "mode" "V256SF") (symbol_ref "riscv_vector::get_vlmul(E_V256SFmode)")
	 (eq_attr "mode" "V512SF") (symbol_ref "riscv_vector::get_vlmul(E_V512SFmode)")
	 (eq_attr "mode" "V1024SF") (symbol_ref "riscv_vector::get_vlmul(E_V1024SFmode)")
	 (eq_attr "mode" "V1DF") (symbol_ref "riscv_vector::get_vlmul(E_V1DFmode)")
	 (eq_attr "mode" "V2DF") (symbol_ref "riscv_vector::get_vlmul(E_V2DFmode)")
	 (eq_attr "mode" "V4DF") (symbol_ref "riscv_vector::get_vlmul(E_V4DFmode)")
	 (eq_attr "mode" "V8DF") (symbol_ref "riscv_vector::get_vlmul(E_V8DFmode)")
	 (eq_attr "mode" "V16DF") (symbol_ref "riscv_vector::get_vlmul(E_V16DFmode)")
	 (eq_attr "mode" "V32DF") (symbol_ref "riscv_vector::get_vlmul(E_V32DFmode)")
	 (eq_attr "mode" "V64DF") (symbol_ref "riscv_vector::get_vlmul(E_V64DFmode)")
	 (eq_attr "mode" "V128DF") (symbol_ref "riscv_vector::get_vlmul(E_V128DFmode)")
	 (eq_attr "mode" "V256DF") (symbol_ref "riscv_vector::get_vlmul(E_V256DFmode)")
	 (eq_attr "mode" "V512DF") (symbol_ref "riscv_vector::get_vlmul(E_V512DFmode)")]
	(const_int INVALID_ATTRIBUTE)))

;; It is valid for instruction that require sew/lmul ratio.
(define_attr "ratio" ""
  (cond [(eq_attr "type" "vimov,vfmov,vldux,vldox,vstux,vstox,\
			  vialu,vshift,vicmp,vimul,vidiv,vsalu,\
			  vext,viwalu,viwmul,vicalu,vnshift,\
			  vimuladd,vimerge,vaalu,vsmul,vsshift,\
			  vnclip,viminmax,viwmuladd,\
			  vmiota,vmidx,vfalu,vfmul,vfminmax,vfdiv,\
			  vfwalu,vfwmul,vfsqrt,vfrecp,vfsgnj,vfcmp,\
			  vfmerge,vfcvtitof,vfcvtftoi,vfwcvtitof,\
			  vfwcvtftoi,vfwcvtftof,vfncvtitof,vfncvtftoi,\
			  vfncvtftof,vfmuladd,vfwmuladd,vfclass,vired,\
			  viwred,vfredu,vfredo,vfwredu,vfwredo,vimovvx,\
			  vimovxv,vfmovvf,vfmovfv,vslideup,vslidedown,\
			  vislide1up,vislide1down,vfslide1up,vfslide1down,\
			  vgather,vcompress,vlsegdux,vlsegdox,vssegtux,vssegtox,\
			  vandn,vbrev,vbrev8,vrev8,vclz,vctz,vrol,vror,vwsll,\
			  vclmul,vclmulh,vghsh,vgmul,vaesef,vaesem,vaesdf,vaesdm,\
			  vaeskf1,vaeskf2,vaesz,vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm4r,\
			  vsm3me,vsm3c")
	   (const_int INVALID_ATTRIBUTE)
	(and (eq_attr "type" "vlde,vste,vlsegde,vssegte,vlsegds,vssegts,\
			       vlsegdff,vssegtux,vlsegdox,vlsegdux")
	      (match_test "TARGET_XTHEADVECTOR"))
	   (const_int INVALID_ATTRIBUTE)
	 (eq_attr "mode" "RVVM8QI,RVVM1BI") (const_int 1)
	 (eq_attr "mode" "RVVM4QI,RVVMF2BI") (const_int 2)
	 (eq_attr "mode" "RVVM2QI,RVVMF4BI") (const_int 4)
	 (eq_attr "mode" "RVVM1QI,RVVMF8BI") (const_int 8)
	 (eq_attr "mode" "RVVMF2QI,RVVMF16BI") (const_int 16)
	 (eq_attr "mode" "RVVMF4QI,RVVMF32BI") (const_int 32)
	 (eq_attr "mode" "RVVMF8QI,RVVMF64BI") (const_int 64)
	 (eq_attr "mode" "RVVM8HI") (const_int 2)
	 (eq_attr "mode" "RVVM4HI") (const_int 4)
	 (eq_attr "mode" "RVVM2HI") (const_int 8)
	 (eq_attr "mode" "RVVM1HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4HI") (const_int 64)
	 (eq_attr "mode" "RVVM8HF") (const_int 2)
	 (eq_attr "mode" "RVVM4HF") (const_int 4)
	 (eq_attr "mode" "RVVM2HF") (const_int 8)
	 (eq_attr "mode" "RVVM1HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4HF") (const_int 64)
	 (eq_attr "mode" "RVVM8SI") (const_int 4)
	 (eq_attr "mode" "RVVM4SI") (const_int 8)
	 (eq_attr "mode" "RVVM2SI") (const_int 16)
	 (eq_attr "mode" "RVVM1SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2SI") (const_int 64)
	 (eq_attr "mode" "RVVM8SF") (const_int 4)
	 (eq_attr "mode" "RVVM4SF") (const_int 8)
	 (eq_attr "mode" "RVVM2SF") (const_int 16)
	 (eq_attr "mode" "RVVM1SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2SF") (const_int 64)
	 (eq_attr "mode" "RVVM8DI") (const_int 8)
	 (eq_attr "mode" "RVVM4DI") (const_int 16)
	 (eq_attr "mode" "RVVM2DI") (const_int 32)
	 (eq_attr "mode" "RVVM1DI") (const_int 64)
	 (eq_attr "mode" "RVVM8DF") (const_int 8)
	 (eq_attr "mode" "RVVM4DF") (const_int 16)
	 (eq_attr "mode" "RVVM2DF") (const_int 32)
	 (eq_attr "mode" "RVVM1DF") (const_int 64)
	 (eq_attr "mode" "RVVM1x8QI") (const_int 8)
	 (eq_attr "mode" "RVVMF2x8QI") (const_int 16)
	 (eq_attr "mode" "RVVMF4x8QI") (const_int 32)
	 (eq_attr "mode" "RVVMF8x8QI") (const_int 64)
	 (eq_attr "mode" "RVVM1x7QI") (const_int 8)
	 (eq_attr "mode" "RVVMF2x7QI") (const_int 16)
	 (eq_attr "mode" "RVVMF4x7QI") (const_int 32)
	 (eq_attr "mode" "RVVMF8x7QI") (const_int 64)
	 (eq_attr "mode" "RVVM1x6QI") (const_int 8)
	 (eq_attr "mode" "RVVMF2x6QI") (const_int 16)
	 (eq_attr "mode" "RVVMF4x6QI") (const_int 32)
	 (eq_attr "mode" "RVVMF8x6QI") (const_int 64)
	 (eq_attr "mode" "RVVM1x5QI") (const_int 8)
	 (eq_attr "mode" "RVVMF2x5QI") (const_int 16)
	 (eq_attr "mode" "RVVMF4x5QI") (const_int 32)
	 (eq_attr "mode" "RVVMF8x5QI") (const_int 64)
	 (eq_attr "mode" "RVVM2x4QI") (const_int 4)
	 (eq_attr "mode" "RVVM1x4QI") (const_int 8)
	 (eq_attr "mode" "RVVMF2x4QI") (const_int 16)
	 (eq_attr "mode" "RVVMF4x4QI") (const_int 32)
	 (eq_attr "mode" "RVVMF8x4QI") (const_int 64)
	 (eq_attr "mode" "RVVM2x3QI") (const_int 4)
	 (eq_attr "mode" "RVVM1x3QI") (const_int 8)
	 (eq_attr "mode" "RVVMF2x3QI") (const_int 16)
	 (eq_attr "mode" "RVVMF4x3QI") (const_int 32)
	 (eq_attr "mode" "RVVMF8x3QI") (const_int 64)
	 (eq_attr "mode" "RVVM4x2QI") (const_int 2)
	 (eq_attr "mode" "RVVM2x2QI") (const_int 4)
	 (eq_attr "mode" "RVVM1x2QI") (const_int 8)
	 (eq_attr "mode" "RVVMF2x2QI") (const_int 16)
	 (eq_attr "mode" "RVVMF4x2QI") (const_int 32)
	 (eq_attr "mode" "RVVMF8x2QI") (const_int 64)
	 (eq_attr "mode" "RVVM1x8HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2x8HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4x8HI") (const_int 64)
	 (eq_attr "mode" "RVVM1x7HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2x7HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4x7HI") (const_int 64)
	 (eq_attr "mode" "RVVM1x6HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2x6HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4x6HI") (const_int 64)
	 (eq_attr "mode" "RVVM1x5HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2x5HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4x5HI") (const_int 64)
	 (eq_attr "mode" "RVVM2x4HI") (const_int 8)
	 (eq_attr "mode" "RVVM1x4HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2x4HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4x4HI") (const_int 64)
	 (eq_attr "mode" "RVVM2x3HI") (const_int 8)
	 (eq_attr "mode" "RVVM1x3HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2x3HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4x3HI") (const_int 64)
	 (eq_attr "mode" "RVVM4x2HI") (const_int 4)
	 (eq_attr "mode" "RVVM2x2HI") (const_int 8)
	 (eq_attr "mode" "RVVM1x2HI") (const_int 16)
	 (eq_attr "mode" "RVVMF2x2HI") (const_int 32)
	 (eq_attr "mode" "RVVMF4x2HI") (const_int 64)
	 (eq_attr "mode" "RVVM1x8HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2x8HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4x8HF") (const_int 64)
	 (eq_attr "mode" "RVVM1x7HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2x7HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4x7HF") (const_int 64)
	 (eq_attr "mode" "RVVM1x6HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2x6HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4x6HF") (const_int 64)
	 (eq_attr "mode" "RVVM1x5HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2x5HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4x5HF") (const_int 64)
	 (eq_attr "mode" "RVVM2x4HF") (const_int 8)
	 (eq_attr "mode" "RVVM1x4HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2x4HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4x4HF") (const_int 64)
	 (eq_attr "mode" "RVVM2x3HF") (const_int 8)
	 (eq_attr "mode" "RVVM1x3HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2x3HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4x3HF") (const_int 64)
	 (eq_attr "mode" "RVVM4x2HF") (const_int 4)
	 (eq_attr "mode" "RVVM2x2HF") (const_int 8)
	 (eq_attr "mode" "RVVM1x2HF") (const_int 16)
	 (eq_attr "mode" "RVVMF2x2HF") (const_int 32)
	 (eq_attr "mode" "RVVMF4x2HF") (const_int 64)
	 (eq_attr "mode" "RVVM1x8SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2x8SI") (const_int 64)
	 (eq_attr "mode" "RVVM1x7SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2x7SI") (const_int 64)
	 (eq_attr "mode" "RVVM1x6SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2x6SI") (const_int 64)
	 (eq_attr "mode" "RVVM1x5SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2x5SI") (const_int 64)
	 (eq_attr "mode" "RVVM2x4SI") (const_int 16)
	 (eq_attr "mode" "RVVM1x4SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2x4SI") (const_int 64)
	 (eq_attr "mode" "RVVM2x3SI") (const_int 16)
	 (eq_attr "mode" "RVVM1x3SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2x3SI") (const_int 64)
	 (eq_attr "mode" "RVVM4x2SI") (const_int 8)
	 (eq_attr "mode" "RVVM2x2SI") (const_int 16)
	 (eq_attr "mode" "RVVM1x2SI") (const_int 32)
	 (eq_attr "mode" "RVVMF2x2SI") (const_int 64)
	 (eq_attr "mode" "RVVM1x8SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2x8SF") (const_int 64)
	 (eq_attr "mode" "RVVM1x7SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2x7SF") (const_int 64)
	 (eq_attr "mode" "RVVM1x6SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2x6SF") (const_int 64)
	 (eq_attr "mode" "RVVM1x5SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2x5SF") (const_int 64)
	 (eq_attr "mode" "RVVM2x4SF") (const_int 16)
	 (eq_attr "mode" "RVVM1x4SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2x4SF") (const_int 64)
	 (eq_attr "mode" "RVVM2x3SF") (const_int 16)
	 (eq_attr "mode" "RVVM1x3SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2x3SF") (const_int 64)
	 (eq_attr "mode" "RVVM4x2SF") (const_int 8)
	 (eq_attr "mode" "RVVM2x2SF") (const_int 16)
	 (eq_attr "mode" "RVVM1x2SF") (const_int 32)
	 (eq_attr "mode" "RVVMF2x2SF") (const_int 64)
	 (eq_attr "mode" "RVVM1x8DI") (const_int 64)
	 (eq_attr "mode" "RVVM1x7DI") (const_int 64)
	 (eq_attr "mode" "RVVM1x6DI") (const_int 64)
	 (eq_attr "mode" "RVVM1x5DI") (const_int 64)
	 (eq_attr "mode" "RVVM2x4DI") (const_int 32)
	 (eq_attr "mode" "RVVM1x4DI") (const_int 64)
	 (eq_attr "mode" "RVVM2x3DI") (const_int 32)
	 (eq_attr "mode" "RVVM1x3DI") (const_int 64)
	 (eq_attr "mode" "RVVM4x2DI") (const_int 16)
	 (eq_attr "mode" "RVVM2x2DI") (const_int 32)
	 (eq_attr "mode" "RVVM1x2DI") (const_int 64)
	 (eq_attr "mode" "RVVM1x8DF") (const_int 64)
	 (eq_attr "mode" "RVVM1x7DF") (const_int 64)
	 (eq_attr "mode" "RVVM1x6DF") (const_int 64)
	 (eq_attr "mode" "RVVM1x5DF") (const_int 64)
	 (eq_attr "mode" "RVVM2x4DF") (const_int 32)
	 (eq_attr "mode" "RVVM1x4DF") (const_int 64)
	 (eq_attr "mode" "RVVM2x3DF") (const_int 32)
	 (eq_attr "mode" "RVVM1x3DF") (const_int 64)
	 (eq_attr "mode" "RVVM4x2DF") (const_int 16)
	 (eq_attr "mode" "RVVM2x2DF") (const_int 32)
	 (eq_attr "mode" "RVVM1x2DF") (const_int 64)

	 ;; VLS modes.
	 (eq_attr "mode" "V1QI,V1BI") (symbol_ref "riscv_vector::get_ratio(E_V1QImode)")
	 (eq_attr "mode" "V2QI,V2BI") (symbol_ref "riscv_vector::get_ratio(E_V2QImode)")
	 (eq_attr "mode" "V4QI,V4BI") (symbol_ref "riscv_vector::get_ratio(E_V4QImode)")
	 (eq_attr "mode" "V8QI,V8BI") (symbol_ref "riscv_vector::get_ratio(E_V8QImode)")
	 (eq_attr "mode" "V16QI,V16BI") (symbol_ref "riscv_vector::get_ratio(E_V16QImode)")
	 (eq_attr "mode" "V32QI,V32BI") (symbol_ref "riscv_vector::get_ratio(E_V32QImode)")
	 (eq_attr "mode" "V64QI,V64BI") (symbol_ref "riscv_vector::get_ratio(E_V64QImode)")
	 (eq_attr "mode" "V128QI,V128BI") (symbol_ref "riscv_vector::get_ratio(E_V128QImode)")
	 (eq_attr "mode" "V256QI,V256BI") (symbol_ref "riscv_vector::get_ratio(E_V256QImode)")
	 (eq_attr "mode" "V512QI,V512BI") (symbol_ref "riscv_vector::get_ratio(E_V512QImode)")
	 (eq_attr "mode" "V1024QI,V1024BI") (symbol_ref "riscv_vector::get_ratio(E_V1024QImode)")
	 (eq_attr "mode" "V2048QI,V2048BI") (symbol_ref "riscv_vector::get_ratio(E_V2048QImode)")
	 (eq_attr "mode" "V4096QI,V4096BI") (symbol_ref "riscv_vector::get_ratio(E_V4096QImode)")
	 (eq_attr "mode" "V1HI") (symbol_ref "riscv_vector::get_ratio(E_V1HImode)")
	 (eq_attr "mode" "V2HI") (symbol_ref "riscv_vector::get_ratio(E_V2HImode)")
	 (eq_attr "mode" "V4HI") (symbol_ref "riscv_vector::get_ratio(E_V4HImode)")
	 (eq_attr "mode" "V8HI") (symbol_ref "riscv_vector::get_ratio(E_V8HImode)")
	 (eq_attr "mode" "V16HI") (symbol_ref "riscv_vector::get_ratio(E_V16HImode)")
	 (eq_attr "mode" "V32HI") (symbol_ref "riscv_vector::get_ratio(E_V32HImode)")
	 (eq_attr "mode" "V64HI") (symbol_ref "riscv_vector::get_ratio(E_V64HImode)")
	 (eq_attr "mode" "V128HI") (symbol_ref "riscv_vector::get_ratio(E_V128HImode)")
	 (eq_attr "mode" "V256HI") (symbol_ref "riscv_vector::get_ratio(E_V256HImode)")
	 (eq_attr "mode" "V512HI") (symbol_ref "riscv_vector::get_ratio(E_V512HImode)")
	 (eq_attr "mode" "V1024HI") (symbol_ref "riscv_vector::get_ratio(E_V1024HImode)")
	 (eq_attr "mode" "V2048HI") (symbol_ref "riscv_vector::get_ratio(E_V2048HImode)")
	 (eq_attr "mode" "V1SI") (symbol_ref "riscv_vector::get_ratio(E_V1SImode)")
	 (eq_attr "mode" "V2SI") (symbol_ref "riscv_vector::get_ratio(E_V2SImode)")
	 (eq_attr "mode" "V4SI") (symbol_ref "riscv_vector::get_ratio(E_V4SImode)")
	 (eq_attr "mode" "V8SI") (symbol_ref "riscv_vector::get_ratio(E_V8SImode)")
	 (eq_attr "mode" "V16SI") (symbol_ref "riscv_vector::get_ratio(E_V16SImode)")
	 (eq_attr "mode" "V32SI") (symbol_ref "riscv_vector::get_ratio(E_V32SImode)")
	 (eq_attr "mode" "V64SI") (symbol_ref "riscv_vector::get_ratio(E_V64SImode)")
	 (eq_attr "mode" "V128SI") (symbol_ref "riscv_vector::get_ratio(E_V128SImode)")
	 (eq_attr "mode" "V256SI") (symbol_ref "riscv_vector::get_ratio(E_V256SImode)")
	 (eq_attr "mode" "V512SI") (symbol_ref "riscv_vector::get_ratio(E_V512SImode)")
	 (eq_attr "mode" "V1024SI") (symbol_ref "riscv_vector::get_ratio(E_V1024SImode)")
	 (eq_attr "mode" "V1DI") (symbol_ref "riscv_vector::get_ratio(E_V1DImode)")
	 (eq_attr "mode" "V2DI") (symbol_ref "riscv_vector::get_ratio(E_V2DImode)")
	 (eq_attr "mode" "V4DI") (symbol_ref "riscv_vector::get_ratio(E_V4DImode)")
	 (eq_attr "mode" "V8DI") (symbol_ref "riscv_vector::get_ratio(E_V8DImode)")
	 (eq_attr "mode" "V16DI") (symbol_ref "riscv_vector::get_ratio(E_V16DImode)")
	 (eq_attr "mode" "V32DI") (symbol_ref "riscv_vector::get_ratio(E_V32DImode)")
	 (eq_attr "mode" "V64DI") (symbol_ref "riscv_vector::get_ratio(E_V64DImode)")
	 (eq_attr "mode" "V128DI") (symbol_ref "riscv_vector::get_ratio(E_V128DImode)")
	 (eq_attr "mode" "V256DI") (symbol_ref "riscv_vector::get_ratio(E_V256DImode)")
	 (eq_attr "mode" "V512DI") (symbol_ref "riscv_vector::get_ratio(E_V512DImode)")
	 (eq_attr "mode" "V1HF") (symbol_ref "riscv_vector::get_ratio(E_V1HFmode)")
	 (eq_attr "mode" "V2HF") (symbol_ref "riscv_vector::get_ratio(E_V2HFmode)")
	 (eq_attr "mode" "V4HF") (symbol_ref "riscv_vector::get_ratio(E_V4HFmode)")
	 (eq_attr "mode" "V8HF") (symbol_ref "riscv_vector::get_ratio(E_V8HFmode)")
	 (eq_attr "mode" "V16HF") (symbol_ref "riscv_vector::get_ratio(E_V16HFmode)")
	 (eq_attr "mode" "V32HF") (symbol_ref "riscv_vector::get_ratio(E_V32HFmode)")
	 (eq_attr "mode" "V64HF") (symbol_ref "riscv_vector::get_ratio(E_V64HFmode)")
	 (eq_attr "mode" "V128HF") (symbol_ref "riscv_vector::get_ratio(E_V128HFmode)")
	 (eq_attr "mode" "V256HF") (symbol_ref "riscv_vector::get_ratio(E_V256HFmode)")
	 (eq_attr "mode" "V512HF") (symbol_ref "riscv_vector::get_ratio(E_V512HFmode)")
	 (eq_attr "mode" "V1024HF") (symbol_ref "riscv_vector::get_ratio(E_V1024HFmode)")
	 (eq_attr "mode" "V2048HF") (symbol_ref "riscv_vector::get_ratio(E_V2048HFmode)")
	 (eq_attr "mode" "V1SF") (symbol_ref "riscv_vector::get_ratio(E_V1SFmode)")
	 (eq_attr "mode" "V2SF") (symbol_ref "riscv_vector::get_ratio(E_V2SFmode)")
	 (eq_attr "mode" "V4SF") (symbol_ref "riscv_vector::get_ratio(E_V4SFmode)")
	 (eq_attr "mode" "V8SF") (symbol_ref "riscv_vector::get_ratio(E_V8SFmode)")
	 (eq_attr "mode" "V16SF") (symbol_ref "riscv_vector::get_ratio(E_V16SFmode)")
	 (eq_attr "mode" "V32SF") (symbol_ref "riscv_vector::get_ratio(E_V32SFmode)")
	 (eq_attr "mode" "V64SF") (symbol_ref "riscv_vector::get_ratio(E_V64SFmode)")
	 (eq_attr "mode" "V128SF") (symbol_ref "riscv_vector::get_ratio(E_V128SFmode)")
	 (eq_attr "mode" "V256SF") (symbol_ref "riscv_vector::get_ratio(E_V256SFmode)")
	 (eq_attr "mode" "V512SF") (symbol_ref "riscv_vector::get_ratio(E_V512SFmode)")
	 (eq_attr "mode" "V1024SF") (symbol_ref "riscv_vector::get_ratio(E_V1024SFmode)")
	 (eq_attr "mode" "V1DF") (symbol_ref "riscv_vector::get_ratio(E_V1DFmode)")
	 (eq_attr "mode" "V2DF") (symbol_ref "riscv_vector::get_ratio(E_V2DFmode)")
	 (eq_attr "mode" "V4DF") (symbol_ref "riscv_vector::get_ratio(E_V4DFmode)")
	 (eq_attr "mode" "V8DF") (symbol_ref "riscv_vector::get_ratio(E_V8DFmode)")
	 (eq_attr "mode" "V16DF") (symbol_ref "riscv_vector::get_ratio(E_V16DFmode)")
	 (eq_attr "mode" "V32DF") (symbol_ref "riscv_vector::get_ratio(E_V32DFmode)")
	 (eq_attr "mode" "V64DF") (symbol_ref "riscv_vector::get_ratio(E_V64DFmode)")
	 (eq_attr "mode" "V128DF") (symbol_ref "riscv_vector::get_ratio(E_V128DFmode)")
	 (eq_attr "mode" "V256DF") (symbol_ref "riscv_vector::get_ratio(E_V256DFmode)")
	 (eq_attr "mode" "V512DF") (symbol_ref "riscv_vector::get_ratio(E_V512DFmode)")]
	(const_int INVALID_ATTRIBUTE)))

;; The index of operand[] to get the merge op.
(define_attr "merge_op_idx" ""
	(cond [(eq_attr "type" "vlde,vimov,vfmov,vldm,vlds,vmalu,vldux,vldox,vicmp,\
				vialu,vshift,viminmax,vimul,vidiv,vsalu,vext,viwalu,\
				viwmul,vnshift,vaalu,vsmul,vsshift,vnclip,vmsfs,\
				vmiota,vmidx,vfalu,vfmul,vfminmax,vfdiv,vfwalu,vfwmul,\
				vfsqrt,vfrecp,vfsgnj,vfcmp,vfcvtitof,vfcvtftoi,vfwcvtitof,\
				vfwcvtftoi,vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,vfclass,\
				vired,viwred,vfredu,vfredo,vfwredu,vfwredo,vimovxv,vfmovfv,\
				vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,\
				vgather,vldff,viwmuladd,vfwmuladd,vlsegde,vlsegds,vlsegdux,vlsegdox,vlsegdff,\
				vandn,vbrev,vbrev8,vrev8,vrol,vror,vwsll,vclmul,vclmulh")
	       (const_int 2)

	       (eq_attr "type" "vimerge,vfmerge,vcompress,vghsh,vgmul,vaesef,vaesem,vaesdf,vaesdm,\
                                vaeskf1,vaeskf2,vaesz,vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm4r,vsm3me,vsm3c")
	       (const_int 1)

	       (eq_attr "type" "vimuladd,vfmuladd")
	       (const_int 2)]
	(const_int INVALID_ATTRIBUTE)))

;; The index of operand[] represents the machine mode of the instruction.
(define_attr "mode_idx" ""
	(cond [(eq_attr "type" "vlde,vste,vldm,vstm,vlds,vsts,vldux,vldox,vldff,vldr,vstr,\
				vlsegde,vlsegds,vlsegdux,vlsegdox,vlsegdff,vialu,vext,vicalu,\
				vshift,vicmp,viminmax,vimul,vidiv,vimuladd,vimerge,vimov,\
				vsalu,vaalu,vsmul,vsshift,vfalu,vfmul,vfdiv,vfmuladd,vfsqrt,vfrecp,\
				vfcmp,vfminmax,vfsgnj,vfclass,vfmerge,vfmov,\
				vfcvtitof,vfncvtitof,vfncvtftoi,vfncvtftof,vmalu,vmiota,vmidx,\
				vimovxv,vfmovfv,vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,\
				vgather,vcompress,vmov,vnclip,vnshift")
	       (const_int 0)

	       (eq_attr "type" "vimovvx,vfmovvf")
	       (const_int 1)

	       (eq_attr "type" "vssegte,vmpop,vmffs")
	       (const_int 2)       

	       (eq_attr "type" "vstux,vstox,vssegts,vssegtux,vssegtox,vfcvtftoi,vfwcvtitof,vfwcvtftoi,
				vfwcvtftof,vmsfs,vired,viwred,vfredu,vfredo,vfwredu,vfwredo")
	       (const_int 3)

	       (eq_attr "type" "viwalu,viwmul,viwmuladd,vfwalu,vfwmul,vfwmuladd")
	       (const_int 4)]
	(const_int INVALID_ATTRIBUTE)))

;; The index of operand[] to get the avl op.
(define_attr "vl_op_idx" ""
  (cond [(eq_attr "type" "vlde,vste,vimov,vfmov,vldm,vstm,vmalu,vsts,vstux,\
			  vstox,vext,vmsfs,vmiota,vfsqrt,vfrecp,vfcvtitof,vldff,\
			  vfcvtftoi,vfwcvtitof,vfwcvtftoi,vfwcvtftof,vfncvtitof,\
			  vfncvtftoi,vfncvtftof,vfclass,vimovxv,vfmovfv,vcompress,\
			  vlsegde,vssegts,vssegtux,vssegtox,vlsegdff,vbrev,vbrev8,vrev8,\
                          vghsh,vaeskf1,vaeskf2,vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm3me,vsm3c")
	   (const_int 4)

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
         (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
             (const_int 5)
             (const_int 4))

	 (eq_attr "type" "vldux,vldox,vialu,vshift,viminmax,vimul,vidiv,vsalu,\
			  viwalu,viwmul,vnshift,vimerge,vaalu,vsmul,\
			  vsshift,vnclip,vfalu,vfmul,vfminmax,vfdiv,vfwalu,vfwmul,\
			  vfsgnj,vfmerge,vired,viwred,vfredu,vfredo,vfwredu,vfwredo,\
			  vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,\
			  vgather,viwmuladd,vfwmuladd,vlsegds,vlsegdux,vlsegdox,vandn,vrol,\
                          vror,vwsll,vclmul,vclmulh")
	   (const_int 5)

	 (eq_attr "type" "vicmp,vimuladd,vfcmp,vfmuladd")
	   (const_int 6)

	 (eq_attr "type" "vmpop,vmffs,vmidx,vssegte,vclz,vctz,vgmul,vaesef,vaesem,vaesdf,vaesdm,\
                          vaesz,vsm4r")
	   (const_int 3)]
  (const_int INVALID_ATTRIBUTE)))

;; The tail policy op value.
(define_attr "ta" ""
  (cond [(eq_attr "type" "vlde,vimov,vfmov,vext,vmiota,vfsqrt,vfrecp,\
			  vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,vfwcvtftof,\
			  vfncvtitof,vfncvtftoi,vfncvtftof,vfclass,vimovxv,vfmovfv,\
			  vcompress,vldff,vlsegde,vlsegdff,vbrev,vbrev8,vrev8,vghsh,\
                          vaeskf1,vaeskf2,vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm3me,vsm3c")
	   (symbol_ref "riscv_vector::get_ta(operands[5])")

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
	 (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
	     (symbol_ref "riscv_vector::get_ta(operands[6])")
	     (symbol_ref "riscv_vector::get_ta(operands[5])"))

	 (eq_attr "type" "vldux,vldox,vialu,vshift,viminmax,vimul,vidiv,vsalu,\
			  viwalu,viwmul,vnshift,vimerge,vaalu,vsmul,\
			  vsshift,vnclip,vfalu,vfmul,vfminmax,vfdiv,\
			  vfwalu,vfwmul,vfsgnj,vfmerge,vired,viwred,vfredu,\
			  vfredo,vfwredu,vfwredo,vslideup,vslidedown,vislide1up,\
			  vislide1down,vfslide1up,vfslide1down,vgather,viwmuladd,vfwmuladd,\
			  vlsegds,vlsegdux,vlsegdox,vandn,vrol,vror,vwsll,vclmul,vclmulh")
	   (symbol_ref "riscv_vector::get_ta(operands[6])")

	 (eq_attr "type" "vimuladd,vfmuladd")
	   (symbol_ref "riscv_vector::get_ta(operands[7])")

	 (eq_attr "type" "vmidx,vgmul,vaesef,vaesem,vaesdf,vaesdm,vaesz,vsm4r")
	   (symbol_ref "riscv_vector::get_ta(operands[4])")]
	(const_int INVALID_ATTRIBUTE)))

;; The mask policy op value.
(define_attr "ma" ""
  (cond [(eq_attr "type" "vlde,vext,vmiota,vfsqrt,vfrecp,vfcvtitof,vfcvtftoi,\
			  vfwcvtitof,vfwcvtftoi,vfwcvtftof,vfncvtitof,vfncvtftoi,\
			  vfncvtftof,vfclass,vldff,vlsegde,vlsegdff,vbrev,vbrev8,vrev8")
	   (symbol_ref "riscv_vector::get_ma(operands[6])")

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
	 (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
	     (symbol_ref "riscv_vector::get_ma(operands[7])")
	     (symbol_ref "riscv_vector::get_ma(operands[6])"))

	 (eq_attr "type" "vldux,vldox,vialu,vshift,viminmax,vimul,vidiv,vsalu,\
			  viwalu,viwmul,vnshift,vaalu,vsmul,vsshift,\
			  vnclip,vicmp,vfalu,vfmul,vfminmax,vfdiv,\
			  vfwalu,vfwmul,vfsgnj,vfcmp,vslideup,vslidedown,\
			  vislide1up,vislide1down,vfslide1up,vfslide1down,vgather,\
			  viwmuladd,vfwmuladd,vlsegds,vlsegdux,vlsegdox,vandn,vrol,\
                          vror,vwsll,vclmul,vclmulh")
	   (symbol_ref "riscv_vector::get_ma(operands[7])")

	 (eq_attr "type" "vimuladd,vfmuladd")
	   (symbol_ref "riscv_vector::get_ma(operands[8])")

	 (eq_attr "type" "vmsfs,vmidx")
	   (symbol_ref "riscv_vector::get_ma(operands[5])")]
	(const_int INVALID_ATTRIBUTE)))

;; The avl type value.
(define_attr "avl_type_idx" ""
  (cond [(eq_attr "type" "vlde,vldff,vste,vimov,vfmov,vext,vimerge,\
			  vfsqrt,vfrecp,vfmerge,vfcvtitof,vfcvtftoi,vfwcvtitof,\
			  vfwcvtftoi,vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,\
			  vfclass,vired,viwred,vfredu,vfredo,vfwredu,vfwredo,\
			  vimovxv,vfmovfv,vlsegde,vlsegdff,vmiota,vbrev,vbrev8,vrev8")
	   (const_int 7)
	 (eq_attr "type" "vldm,vstm,vmalu,vmalu,vgmul,vaesef,vaesem,vaesdf,vaesdm,vaesz,\
                          vsm4r")
	   (const_int 5)

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
	 (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
	     (const_int 8)
	     (const_int 7))

	 (eq_attr "type" "vldux,vldox,vialu,vshift,viminmax,vimul,vidiv,vsalu,\
			  viwalu,viwmul,vnshift,vaalu,vsmul,vsshift,\
			  vnclip,vicmp,vfalu,vfmul,vfminmax,vfdiv,vfwalu,vfwmul,\
			  vfsgnj,vfcmp,vslideup,vslidedown,vislide1up,\
			  vislide1down,vfslide1up,vfslide1down,vgather,viwmuladd,vfwmuladd,\
			  vlsegds,vlsegdux,vlsegdox,vandn,vrol,vror,vclmul,vclmulh,vwsll")
	   (const_int 8)
	 (eq_attr "type" "vstux,vstox,vssegts,vssegtux,vssegtox")
	   (const_int 5)

	 (eq_attr "type" "vimuladd,vfmuladd")
	   (const_int 9)

	 (eq_attr "type" "vmsfs,vmidx,vcompress,vghsh,vaeskf1,vaeskf2,vsha2ms,vsha2ch,vsha2cl,\
                          vsm4k,vsm3me,vsm3c")
	   (const_int 6)

	 (eq_attr "type" "vmpop,vmffs,vssegte,vclz,vctz")
	   (const_int 4)]
	(const_int INVALID_ATTRIBUTE)))

;; Defines rounding mode of an fixed-point operation.

(define_attr "vxrm_mode" "rnu,rne,rdn,rod,none"
  (cond [(eq_attr "type" "vaalu,vsmul,vsshift,vnclip")
	 (cond
	   [(match_test "INTVAL (operands[9]) == riscv_vector::VXRM_RNU")
	    (const_string "rnu")

	    (match_test "INTVAL (operands[9]) == riscv_vector::VXRM_RNE")
	    (const_string "rne")

	    (match_test "INTVAL (operands[9]) == riscv_vector::VXRM_RDN")
	    (const_string "rdn")

	    (match_test "INTVAL (operands[9]) == riscv_vector::VXRM_ROD")
	    (const_string "rod")]
	   (const_string "none"))]
        (const_string "none")))

;; Defines rounding mode of an floating-point operation.
(define_attr "frm_mode" ""
  (cond [(eq_attr "type" "vfalu,vfwalu,vfmul,vfdiv,vfwmul")
	 (symbol_ref "riscv_vector::FRM_DYN")]
	(symbol_ref "riscv_vector::FRM_NONE")))

(include "thead-vector.md")

;; -----------------------------------------------------------------
;; ---- Miscellaneous Operations
;; -----------------------------------------------------------------

(define_insn "@vundefined<mode>"
  [(set (match_operand:V 0 "register_operand" "=vr")
	(unspec:V [(reg:SI X0_REGNUM)] UNSPEC_VUNDEF))]
  "TARGET_VECTOR"
  ""
  [(set_attr "type" "vector")])

(define_insn "@vundefined<mode>"
  [(set (match_operand:VB 0 "register_operand" "=vr")
	(unspec:VB [(reg:SI X0_REGNUM)] UNSPEC_VUNDEF))]
  "TARGET_VECTOR"
  ""
  [(set_attr "type" "vector")])

(define_insn "@vundefined<mode>"
  [(set (match_operand:VT 0 "register_operand" "=vr")
	(unspec:VT [(reg:SI X0_REGNUM)] UNSPEC_VUNDEF))]
  "TARGET_VECTOR"
  ""
  [(set_attr "type" "vector")])

(define_expand "@vreinterpret<mode>"
  [(set (match_operand:V 0 "register_operand")
	(match_operand 1 "vector_any_register_operand"))]
  "TARGET_VECTOR"
  {
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, operands[1]));
    DONE;
  }
)

(define_expand "@vreinterpret<mode>"
  [(set (match_operand:VB 0 "register_operand")
	(match_operand    1 "vector_any_register_operand"))]
  "TARGET_VECTOR"
  {
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, operands[1]));
    DONE;
  }
)

;; This pattern is used to hold the AVL operand for
;; RVV instructions that implicity use VLMAX AVL.
;; RVV instruction implicitly use GPR that is ultimately
;; defined by this pattern is safe for VSETVL pass emit
;; a vsetvl instruction modify this register after RA.
;; Case 1:
;;   vlmax_avl a5
;;   ... (across many blocks)
;;   vadd (implicit use a5)  ====> emit: vsetvl a5,zero
;; Case 2:
;;   vlmax_avl a5
;;   ... (across many blocks)
;;   mv a6,a5
;;   ... (across many blocks)
;;   vadd (implicit use a6)  ====> emit: vsetvl a6,zero
;; Case 3:
;;   vlmax_avl a5
;;   ... (across many blocks)
;;   store mem,a5 (spill)
;;   ... (across many blocks)
;;   load a7,mem (spill)
;;   ... (across many blocks)
;;   vadd (implicit use a7)  ====> emit: vsetvl a7,zero
;; Such cases are all safe for VSETVL PASS to emit a vsetvl
;; instruction that modifies the AVL operand.
(define_insn "@vlmax_avl<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "const_int_operand" "i")] UNSPEC_VLMAX))]
  "TARGET_VECTOR"
  ""
  [(set_attr "type" "vsetvl_pre")]
  )

;; Set VXRM
(define_insn "vxrmsi"
  [(set (reg:SI VXRM_REGNUM)
	(match_operand:SI 0 "const_int_operand" "i"))]
  "TARGET_VECTOR"
  "csrwi\tvxrm,%0"
  [(set_attr "type" "wrvxrm")
   (set_attr "mode" "SI")])

;; Set FRM
(define_insn "fsrmsi_backup"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(reg:SI FRM_REGNUM))
   (set (reg:SI FRM_REGNUM)
	(match_operand:SI 1 "reg_or_int_operand" "r,i"))]
   "TARGET_VECTOR"
  "@
   fsrm\t%0,%1
   fsrmi\t%0,%1"
  [(set_attr "type" "wrfrm,wrfrm")
   (set_attr "mode" "SI")]
)

(define_insn "fsrmsi_restore"
  [(set (reg:SI FRM_REGNUM)
	(match_operand:SI 0 "reg_or_int_operand" "r,i"))]
  "TARGET_VECTOR"
  "@
   fsrm\t%0
   fsrmi\t%0"
  [(set_attr "type" "wrfrm,wrfrm")
   (set_attr "mode" "SI")]
 )

;; The volatile fsrmsi restore is used for the exit point for the
;; dynamic mode switching. It will generate one volatile fsrm a5
;; which won't be eliminated.
(define_insn "fsrmsi_restore_volatile"
  [(set (reg:SI FRM_REGNUM)
	(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")]
			    UNSPECV_FRM_RESTORE_EXIT))]
  "TARGET_VECTOR"
  "fsrm\t%0"
  [(set_attr "type" "wrfrm")
   (set_attr "mode" "SI")]
)

;; Read FRM
(define_insn "frrmsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(reg:SI FRM_REGNUM))]
  "TARGET_VECTOR"
  "frrm\t%0"
  [(set_attr "type" "fmove")
   (set_attr "mode" "SI")]
)

;; -----------------------------------------------------------------
;; ---- Moves Operations
;; -----------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:V 0 "reg_or_mem_operand")
	(match_operand:V 1 "general_operand"))]
  "TARGET_VECTOR"
{
  /* For whole register move, we transform the pattern into the format
     that excludes the clobber of scratch register.

     We include clobber of a scalar scratch register which is going to be
     used for emit of vsetvl instruction after reload_completed since we
     need vsetvl instruction to set VL/VTYPE global status for fractional
     vector load/store.

     For example:
       [(set (match_operand:RVVMF8QI v24)
	     (match_operand:RVVMF8QI (mem: a4)))
	     (clobber (scratch:SI a5))]
     ====>> vsetvl a5,zero,e8,mf8
     ====>> vle8.v v24,(a4)

     Philosophy:

       - Clobber a scalar scratch register for each mov<mode>.

       - Classify the machine_mode mode = <MODE>mode into 2 class:
	 Whole register move and fractional register move.

       - Transform and remove scratch clobber register for whole
	 register move so that we can avoid occupying the scalar
	 registers.

       - We can not leave it to TARGET_SECONDARY_RELOAD since it happens
	 before spilling. The clobber scratch is used by spilling fractional
	 registers in IRA/LRA so it's too early.  */
  if (TARGET_XTHEADVECTOR)
    {
      emit_insn (gen_pred_th_whole_mov (<MODE>mode, operands[0], operands[1],
					RVV_VLMAX, GEN_INT(riscv_vector::VLMAX)));
      DONE;
    }

  if (riscv_vector::legitimize_move (operands[0], &operands[1]))
    DONE;
})

;; This pattern is used for code-gen for whole register load/stores.
;; Also applicable for all register moves.
;; Fractional vector modes load/store are not allowed to match this pattern.
;; Mask modes load/store are not allowed to match this pattern.
;; We seperate "*mov<mode>" into "*mov<mode>_whole" and "*mov<mode>_fract" because
;; we don't want to include fractional load/store in "*mov<mode>" which will
;; create unexpected patterns in LRA.
;; For example:
;; ira rtl:
;;   (insn 20 19 9 2 (set (reg/v:RVVMF4QI 97 v1 [ v1 ])
;;      (reg:RVVMF4QI 134 [ _1 ])) "rvv.c":9:22 571 {*movvnx2qi_fract}
;;   (nil))
;; When the value of pseudo register 134 of the insn above is discovered already
;; spilled in the memory during LRA.
;; LRA will reload this pattern into a memory load instruction pattern.
;; Because RVVMF4QI is a fractional vector, we want LRA reload this pattern into
;;  (insn 20 19 9 2 (parallel [
;;       (set (reg:RVVMF4QI 98 v2 [orig:134 _1 ] [134])
;;           (mem/c:RVVMF4QI (reg:SI 13 a3 [155]) [1 %sfp+[-2, -2] S[2, 2] A8]))
;;       (clobber (reg:SI 14 a4 [149]))])
;; So that we could be able to emit vsetvl instruction using clobber sratch a4.
;; To let LRA generate the expected pattern, we should exclude fractional vector
;; load/store in "*mov<mode>_whole". Otherwise, it will reload this pattern into:
;;  (insn 20 19 9 2 (set (reg:RVVMF4QI 98 v2 [orig:134 _1 ] [134])
;;           (mem/c:RVVMF4QI (reg:SI 13 a3 [155]) [1 %sfp+[-2, -2] S[2, 2] A8])))
;; which is not the pattern we want.
;; According the facts above, we make "*mov<mode>_whole" includes load/store/move for whole
;; vector modes according to '-march' and "*mov<mode>_fract" only include fractional vector modes.
(define_insn "*mov<mode>_whole"
  [(set (match_operand:V_WHOLE 0 "reg_or_mem_operand" "=vr, m,vr")
	(match_operand:V_WHOLE 1 "reg_or_mem_operand" "  m,vr,vr"))]
  "TARGET_VECTOR && !TARGET_XTHEADVECTOR"
  "@
   vl%m1re<sew>.v\t%0,%1
   vs%m1r.v\t%1,%0
   vmv%m1r.v\t%0,%1"
  [(set_attr "type" "vldr,vstr,vmov")
   (set_attr "mode" "<MODE>")])

(define_insn "*mov<mode>_fract"
  [(set (match_operand:V_FRACT 0 "register_operand" "=vr")
	(match_operand:V_FRACT 1 "register_operand" " vr"))]
  "TARGET_VECTOR"
  "vmv1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

(define_expand "mov<mode>"
  [(set (match_operand:VB 0 "reg_or_mem_operand")
	(match_operand:VB 1 "general_operand"))]
  "TARGET_VECTOR"
{
  if (TARGET_XTHEADVECTOR)
    {
      emit_insn (gen_pred_th_whole_mov (<MODE>mode, operands[0], operands[1],
					RVV_VLMAX, GEN_INT(riscv_vector::VLMAX)));
      DONE;
    }

  if (riscv_vector::legitimize_move (operands[0], &operands[1]))
    DONE;
})

(define_insn "*mov<mode>"
  [(set (match_operand:VB 0 "register_operand" "=vr")
	(match_operand:VB 1 "register_operand" " vr"))]
  "TARGET_VECTOR && !TARGET_XTHEADVECTOR"
  "vmv1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

(define_expand "@mov<V_FRACT:mode><P:mode>_lra"
  [(parallel
    [(set (match_operand:V_FRACT 0 "reg_or_mem_operand")
	  (match_operand:V_FRACT 1 "reg_or_mem_operand"))
   (clobber (match_scratch:P 2))])]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)"
{})

(define_expand "@mov<VB:mode><P:mode>_lra"
  [(parallel
    [(set (match_operand:VB 0 "reg_or_mem_operand")
	  (match_operand:VB 1 "reg_or_mem_operand"))
   (clobber (match_scratch:P 2))])]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)"
{})

(define_insn_and_split "*mov<V_FRACT:mode><P:mode>_lra"
  [(set (match_operand:V_FRACT 0 "reg_or_mem_operand" "=vr, m,vr")
	(match_operand:V_FRACT 1 "reg_or_mem_operand" "  m,vr,vr"))
   (clobber (match_scratch:P 2 "=&r,&r,X"))]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  if (REG_P (operands[0]) && REG_P (operands[1]))
      emit_insn (gen_rtx_SET (operands[0], operands[1]));
  else
    {
      riscv_vector::emit_vlmax_vsetvl (<V_FRACT:MODE>mode, operands[2]);
      riscv_vector::emit_vlmax_insn_lra (code_for_pred_mov (<V_FRACT:MODE>mode),
				          riscv_vector::UNARY_OP, operands, operands[2]);
    }
  DONE;
}
[(set_attr "type" "vector")]
)

(define_insn_and_split "*mov<VB:mode><P:mode>_lra"
  [(set (match_operand:VB 0 "reg_or_mem_operand" "=vr, m,vr")
	(match_operand:VB 1 "reg_or_mem_operand" "  m,vr,vr"))
   (clobber (match_scratch:P 2 "=&r,&r,X"))]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  if (REG_P (operands[0]) && REG_P (operands[1]))
      emit_insn (gen_rtx_SET (operands[0], operands[1]));
  else
    {
      riscv_vector::emit_vlmax_vsetvl (<VB:MODE>mode, operands[2]);
      riscv_vector::emit_vlmax_insn_lra (code_for_pred_mov (<VB:MODE>mode),
				          riscv_vector::UNARY_MASK_OP, operands, operands[2]);
    }
  DONE;
}
[(set_attr "type" "vector")]
)

;; Define tuple modes data movement.
;; operands[2] is used to save the offset of each subpart.
;; operands[3] is used to calculate the address for each subpart.
;; operands[4] is VL of vsevli instruction.
(define_expand "mov<mode>"
  [(parallel [(set (match_operand:VT 0 "reg_or_mem_operand")
                   (match_operand:VT 1 "general_operand"))
     (clobber (match_dup 2))
     (clobber (match_dup 3))
     (clobber (match_dup 4))])]
  "TARGET_VECTOR"
  {
    /* Need to force register if mem <- !reg.  */
    if (MEM_P (operands[0]) && !REG_P (operands[1]))
      operands[1] = force_reg (<MODE>mode, operands[1]);

    if (GET_CODE (operands[1]) == CONST_VECTOR)
      {
        riscv_vector::expand_tuple_move (operands);
        DONE;
      }

    operands[2] = gen_rtx_SCRATCH (Pmode);
    operands[3] = gen_rtx_SCRATCH (Pmode);
    operands[4] = gen_rtx_SCRATCH (Pmode);
  })

(define_insn_and_split "*mov<VT:mode>_<P:mode>"
  [(set (match_operand:VT 0 "reg_or_mem_operand" "=vr,vr, m")
        (match_operand:VT 1 "reg_or_mem_operand" " vr, m,vr"))
   (clobber (match_scratch:P 2 "=X,&r,&r"))
   (clobber (match_scratch:P 3 "=X,&r,&r"))
   (clobber (match_scratch:P 4 "=X,&r,&r"))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    riscv_vector::expand_tuple_move (operands);
    DONE;
  }
  [(set_attr "type" "vmov,vlde,vste")
   (set_attr "mode" "<VT:MODE>")
   (set (attr "avl_type_idx") (const_int INVALID_ATTRIBUTE))
   (set (attr "mode_idx") (const_int INVALID_ATTRIBUTE))])

;; -----------------------------------------------------------------
;; ---- VLS Moves Operations
;; -----------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:VLS_AVL_IMM 0 "reg_or_mem_operand")
	(match_operand:VLS_AVL_IMM 1 "general_operand"))]
  "TARGET_VECTOR"
{
  if (riscv_vector::legitimize_move (operands[0], &operands[1]))
    DONE;
})

(define_insn_and_split "*mov<mode>"
  [(set (match_operand:VLS_AVL_IMM 0 "reg_or_mem_operand" "=vr, m, vr")
	(match_operand:VLS_AVL_IMM 1 "reg_or_mem_operand" "  m,vr, vr"))]
  "TARGET_VECTOR
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   #
   #
   vmv%m1r.v\t%0,%1"
  "&& reload_completed
   && (!register_operand (operands[0], <MODE>mode)
       || !register_operand (operands[1], <MODE>mode))"
  [(const_int 0)]
  {
    bool ok_p = riscv_vector::legitimize_move (operands[0], &operands[1]);
    gcc_assert (ok_p);
    DONE;
  }
  [(set_attr "type" "vmov")]
)

(define_expand "mov<mode>"
  [(set (match_operand:VLS_AVL_REG 0 "reg_or_mem_operand")
	(match_operand:VLS_AVL_REG 1 "general_operand"))]
  "TARGET_VECTOR"
{
  bool ok_p = riscv_vector::legitimize_move (operands[0], &operands[1]);
  gcc_assert (ok_p);
  DONE;
})

(define_expand "@mov<VLS_AVL_REG:mode><P:mode>_lra"
  [(parallel
    [(set (match_operand:VLS_AVL_REG 0 "reg_or_mem_operand")
	  (match_operand:VLS_AVL_REG 1 "reg_or_mem_operand"))
   (clobber (match_scratch:P 2))])]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)"
{})

(define_insn_and_split "*mov<VLS_AVL_REG:mode><P:mode>_lra"
  [(set (match_operand:VLS_AVL_REG 0 "reg_or_mem_operand" "=vr, m,vr")
	(match_operand:VLS_AVL_REG 1 "reg_or_mem_operand" "  m,vr,vr"))
   (clobber (match_scratch:P 2 "=&r,&r,X"))]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)
   && (register_operand (operands[0], <VLS_AVL_REG:MODE>mode)
       || register_operand (operands[1], <VLS_AVL_REG:MODE>mode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  if (REG_P (operands[0]) && REG_P (operands[1]))
      emit_insn (gen_rtx_SET (operands[0], operands[1]));
  else
    {
      emit_move_insn (operands[2], gen_int_mode (GET_MODE_NUNITS (<VLS_AVL_REG:MODE>mode),
						 Pmode));
      unsigned insn_flags
        = GET_MODE_CLASS (<VLS_AVL_REG:MODE>mode) == MODE_VECTOR_BOOL
						     ? riscv_vector::UNARY_MASK_OP
						     : riscv_vector::UNARY_OP;
      riscv_vector::emit_nonvlmax_insn (code_for_pred_mov (<VLS_AVL_REG:MODE>mode),
					insn_flags, operands, operands[2]);
    }
  DONE;
}
  [(set_attr "type" "vmov")]
)

(define_insn "*mov<mode>_vls"
  [(set (match_operand:VLS 0 "register_operand" "=vr")
	(match_operand:VLS 1 "register_operand" " vr"))]
  "TARGET_VECTOR"
  "vmv%m1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

(define_insn "*mov<mode>_vls"
  [(set (match_operand:VLSB 0 "register_operand" "=vr")
	(match_operand:VLSB 1 "register_operand" " vr"))]
  "TARGET_VECTOR"
  "vmv1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

;; According to RVV ISA:
;; If an element accessed by a vector memory instruction is not naturally aligned to the size of the element,
;; either the element is transferred successfully or an address misaligned exception is raised on that element.
(define_expand "movmisalign<mode>"
  [(set (match_operand:V_VLS 0 "nonimmediate_operand")
	(match_operand:V_VLS 1 "general_operand"))]
  "TARGET_VECTOR && TARGET_VECTOR_MISALIGN_SUPPORTED"
  {
    emit_move_insn (operands[0], operands[1]);
    DONE;
  }
)

;; -----------------------------------------------------------------
;; ---- Duplicate Operations
;; -----------------------------------------------------------------

(define_expand "vec_duplicate<mode>"
  [(set (match_operand:V_VLS 0 "register_operand")
        (vec_duplicate:V_VLS
          (match_operand:<VEL> 1 "direct_broadcast_operand")))]
  "TARGET_VECTOR"
  {
    /* Early expand DImode broadcast in RV32 system to avoid RA reload
       generate (set (reg) (vec_duplicate:DI)).  */
    if (maybe_gt (GET_MODE_SIZE (<VEL>mode), GET_MODE_SIZE (Pmode)))
      {
        riscv_vector::emit_vlmax_insn (code_for_pred_broadcast (<MODE>mode),
				       riscv_vector::UNARY_OP, operands);
	DONE;
      }
    /* Otherwise, allow it fall into general vec_duplicate pattern
       which allow us to have vv->vx combine optimization in later pass.  */
  })

;; According to GCC internal:
;; This pattern only handles duplicates of non-constant inputs.
;; Constant vectors go through the movm pattern instead.
;; So "direct_broadcast_operand" can only be mem or reg, no CONSTANT.
(define_insn_and_split "*vec_duplicate<mode>"
  [(set (match_operand:V_VLS 0 "register_operand")
        (vec_duplicate:V_VLS
          (match_operand:<VEL> 1 "direct_broadcast_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_insn (code_for_pred_broadcast (<MODE>mode),
                                   riscv_vector::UNARY_OP, operands);
    DONE;
  }
  [(set_attr "type" "vector")]
)

;; -----------------------------------------------------------------
;; ---- 6. Configuration-Setting Instructions
;; -----------------------------------------------------------------
;; Includes:
;; - 6.1 vsetvli/vsetivl/vsetvl instructions
;; -----------------------------------------------------------------

;; we dont't define vsetvli as unspec_volatile which has side effects.
;; This instruction can be scheduled by the instruction scheduler.
;; This means these instructions will be deleted when
;; there is no instructions using vl or vtype in the following.
;; rd  | rs1 | AVL value | Effect on vl
;; -   | !x0 | x[rs1]    | Normal stripmining
;; !x0 | x0  | ~0        | Set vl to VLMAX
;; operands[0]: VL.
;; operands[1]: AVL.
;; operands[2]: SEW
;; operands[3]: LMUL
;; operands[4]: Tail policy 0 or 1 (undisturbed/agnostic)
;; operands[5]: Mask policy 0 or 1 (undisturbed/agnostic)

;; We define 2 types of "vsetvl*" instruction patterns:

;; -  "@vsetvl<mode>" is a parallel format which has side effects.

;; -  "@vsetvl<mode>_no_side_effects" has no side effects.

;; -  "@vsetvl<mode>" is used by "vsetvl" intrinsics and "insert-vsetvl" PASS.

;; -  "@vsetvl<mode>_no_side_effects" is used by GCC standard patterns.

;; -  "@vsetvl<mode>" includes VL/VTYPE global registers status (define set)
;; and each RVV instruction includes VL/VTYPE global registers status (use)
;; so that we can guarantee each RVV instruction can execute with correct
;; VL/VTYPE global registers status after "insert-vsetvl" PASS.

;; -  "@vsetvl<mode>_no_side_effects" has no side effects and excludes VL/VTYPE
;; global registers status (define set). It's only used by GCC standard pattern
;; expansion. For example: "mov<mode>" pattern for fractional vector modes which
;; need to set VL/VTYPE. Then we could manually call this pattern to gain benefits
;; from the optimization of each GCC internal PASS.

;; 1. void foo (float *in, float *out)
;;    {
;;      vfloat32mf2_t v = *(vfloat32mf2_t*)in;
;;      *(vfloat32mf2_t*)out = v;
;;    }
;; We could eliminate the second "vsetvl" by calling "@vsetvl<mode>_no_side_effects".
;;
;; "@vsetvl<mode>":               ;; "@vsetvl<mode>_no_side_effects":
;; vsetvli a4,zero,e32,mf2,ta,ma  ;; vsetvli a4,zero,e32,mf2,ta,ma
;; vle32.v v24,(a0)               ;; vle32.v v24,(a0)
;; vsetvli a4,zero,e32,mf2,ta,ma  ;; --
;; vse32.v v24,(a1)               ;; vse32.v v24,(a1)
;; ret                            ;; ret

;; 2. void foo (int8_t *in, int8_t *out, int M)
;;    {
;;      for (int i = 0; i < M; i++){
;;        vint8mf2_t v = *(vint8mf2_t*)(in + i);
;;        *(vint8mf2_t*)(out + i) = v;
;;      }
;;    }
;;
;; Hoist "vsetvl" instruction in LICM:
;; "@vsetvl<mode>":                  ;; "@vsetvl<mode>_no_side_effects":
;; -                                 ;;   vsetvli a4,zero,e32,mf2,ta,ma
;; LOOP:                             ;; LOOP:
;;   vsetvli a4,zero,e32,mf2,ta,ma   ;; -
;;   vle32.v v24,(a0)                ;;   vle32.v v24,(a0)
;;   vsetvli a4,zero,e32,mf2,ta,ma   ;; -
;;   vse32.v v24,(a1)                ;;   vse32.v v24,(a1)

;; However, it may produce wrong codegen if we exclude VL/VTYPE in "vsevl<mode>".
;; 3. void foo (int8_t *in, int8_t *out, int32_t *in2, int32_t *out2, int M)
;;    {
;;      for (int i = 0; i < M; i++){
;;        vint8mf2_t v = *(vint8mf2_t*)(in + i);
;;        vint32mf2_t v2 = *(vint32mf2_t*)(in + i + i);
;;        *(vint8mf2_t*)(out + i) = v;
;;        *(vint32mf2_t*)(out + i + i) = v2;
;;      }
;;    }
;;
;; vsetvli a6,zero,e8,mf2,ta,ma
;; vsetvli a2,zero,e32,mf2,ta,ma
;; LOOP:
;;   vle8.v  v25,(a0)
;;   vle32.v v24,(a5)
;;   addi    a0,a0,1
;;   vse8.v  v25,(a1)
;;   vse32.v v24,(a3)
;;
;; Both vle8.v and vle32.v are using the wrong VL/VTYPE status.
;; We leave it to "insert-vsetvl" PASS to correct this situation.

;; The "insert-vsetvl" PASS mechanism:
;; 1. Before "insert-vsetvl" PASS, only RVV instructions are generated
;;    by GCC standard pattern expansion has the corresponding "vsetvl".
;;    We exploit each GCC internal optimization pass to optimize the "vsetvl".
;; 2. Correct the VL/VTYPE status for each GCC standard pattern RVV instructions.
;;    Insert vsetvl for each RVV instructions that has no VL/VTYPE status if necessary.
;;    For example: RVV intrinsics.
;; 3. Optimize "vsetvl" instructions.

(define_insn "@vsetvl<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "vector_length_operand" "rK")
		   (match_operand 2 "const_int_operand" "i")
		   (match_operand 3 "const_int_operand" "i")
		   (match_operand 4 "const_int_operand" "i")
		   (match_operand 5 "const_int_operand" "i")] UNSPEC_VSETVL))
   (set (reg:SI VL_REGNUM)
	(unspec:SI [(match_dup 1)
		    (match_dup 2)
		    (match_dup 3)] UNSPEC_VSETVL))
   (set (reg:SI VTYPE_REGNUM)
	(unspec:SI [(match_dup 2)
		    (match_dup 3)
		    (match_dup 4)
		    (match_dup 5)] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "vset%i1vli\t%0,%1,e%2,%m3,t%p4,m%p5"
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "<MODE>")
   (set (attr "sew") (symbol_ref "INTVAL (operands[2])"))
   (set (attr "vlmul") (symbol_ref "INTVAL (operands[3])"))
   (set (attr "ta") (symbol_ref "INTVAL (operands[4])"))
   (set (attr "ma") (symbol_ref "INTVAL (operands[5])"))])

;; vsetvl zero,zero,vtype instruction.
;; This pattern has no side effects and does not set X0 register.
(define_insn "vsetvl_vtype_change_only"
  [(set (reg:SI VTYPE_REGNUM)
	(unspec:SI
	  [(match_operand 0 "const_int_operand" "i")
	   (match_operand 1 "const_int_operand" "i")
	   (match_operand 2 "const_int_operand" "i")
	   (match_operand 3 "const_int_operand" "i")] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "vsetvli\tzero,zero,e%0,%m1,t%p2,m%p3"
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "SI")
   (set (attr "sew") (symbol_ref "INTVAL (operands[0])"))
   (set (attr "vlmul") (symbol_ref "INTVAL (operands[1])"))
   (set (attr "ta") (symbol_ref "INTVAL (operands[2])"))
   (set (attr "ma") (symbol_ref "INTVAL (operands[3])"))])

;; vsetvl zero,rs1,vtype instruction.
;; The reason we need this pattern since we should avoid setting X0 register
;; in vsetvl instruction pattern.
(define_insn "@vsetvl_discard_result<mode>"
  [(set (reg:SI VL_REGNUM)
	(unspec:SI [(match_operand:P 0 "vector_length_operand" "rK")
		    (match_operand 1 "const_int_operand" "i")
		    (match_operand 2 "const_int_operand" "i")] UNSPEC_VSETVL))
   (set (reg:SI VTYPE_REGNUM)
	(unspec:SI [(match_dup 1)
		    (match_dup 2)
		    (match_operand 3 "const_int_operand" "i")
		    (match_operand 4 "const_int_operand" "i")] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "vset%i0vli\tzero,%0,e%1,%m2,t%p3,m%p4"
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "<MODE>")
   (set (attr "sew") (symbol_ref "INTVAL (operands[1])"))
   (set (attr "vlmul") (symbol_ref "INTVAL (operands[2])"))
   (set (attr "ta") (symbol_ref "INTVAL (operands[3])"))
   (set (attr "ma") (symbol_ref "INTVAL (operands[4])"))])

;; It's emit by vsetvl/vsetvlmax intrinsics with no side effects.
;; Since we have many optmization passes from "expand" to "reload_completed",
;; such pattern can allow us gain benefits of these optimizations.
(define_insn_and_split "@vsetvl<mode>_no_side_effects"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "vector_length_operand" "rK")
		   (match_operand 2 "const_int_operand" "i")
		   (match_operand 3 "const_int_operand" "i")
		   (match_operand 4 "const_int_operand" "i")
		   (match_operand 5 "const_int_operand" "i")] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "#"
  "&& epilogue_completed"
  [(parallel
    [(set (match_dup 0)
	  (unspec:P [(match_dup 1) (match_dup 2) (match_dup 3)
		     (match_dup 4) (match_dup 5)] UNSPEC_VSETVL))
     (set (reg:SI VL_REGNUM)
	  (unspec:SI [(match_dup 1) (match_dup 2) (match_dup 3)] UNSPEC_VSETVL))
     (set (reg:SI VTYPE_REGNUM)
	  (unspec:SI [(match_dup 2) (match_dup 3) (match_dup 4)
		      (match_dup 5)] UNSPEC_VSETVL))])]
  ""
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "SI")])

;; This pattern use to combine bellow two insns and then further remove
;; unnecessary sign_extend operations:
;;   (set (reg:DI 134 [ _1 ])
;;        (unspec:DI [
;;                (const_int 19 [0x13])
;;                (const_int 8 [0x8])
;;                (const_int 5 [0x5])
;;                (const_int 2 [0x2]) repeated x2
;;            ] UNSPEC_VSETVL))
;;   (set (reg/v:DI 135 [ <retval> ])
;;           (sign_extend:DI (subreg:SI (reg:DI 134 [ _1 ]) 0)))
;;
;; The reason we can remove signe_extend is because currently the vl value
;; returned by the vsetvl instruction ranges from 0 to 65536 (uint16_t), and
;; bits 17 to 63 (including 31) are always 0, so there is no change after
;; sign_extend. Note that for HI and QI modes we cannot do this.
;; Of course, if the range of instructions returned by vsetvl later expands
;; to 32bits, then this combine pattern needs to be removed. But that could be
;; a long time from now.
(define_insn_and_split "*vsetvldi_no_side_effects_si_extend"
  [(set (match_operand:DI 0 "register_operand")
        (sign_extend:DI
          (subreg:SI
	    (unspec:DI [(match_operand:P 1 "vector_length_operand")
		        (match_operand 2 "const_int_operand")
		        (match_operand 3 "const_int_operand")
		        (match_operand 4 "const_int_operand")
		        (match_operand 5 "const_int_operand")] UNSPEC_VSETVL) 0)))]
  "TARGET_VECTOR && TARGET_64BIT"
  "#"
  "&& 1"
  [(set (match_dup 0)
        (unspec:DI [(match_dup 1)
                    (match_dup 2)
                    (match_dup 3)
                    (match_dup 4)
                    (match_dup 5)] UNSPEC_VSETVL))]
  ""
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "SI")])

;; RVV machine description matching format
;; (define_insn ""
;;   [(set (match_operand:MODE 0)
;; 	(if_then_else:MODE
;; 	  (unspec:<MODE:VM>
;; 	    [(match_operand:<VM> 1 "vector_mask_operand")
;; 	     (match_operand N + 4 "vector_length_operand")
;; 	     (match_operand N + 5 "const_int_operand")
;; 	     (match_operand N + 6 "const_int_operand")
;; 	     (reg:SI VL_REGNUM)
;; 	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
;; 	  (instruction operation:MODE
;; 	     (match_operand 3
;; 	     (match_operand 4
;; 	     (match_operand 5
;;           ................
;; 	     (match_operand N + 3)
;; 	  (match_operand:MODE 2 "vector_reg_or_const0_operand")))]
;;
;; (unspec:[........] UNSPEC_VPREDICATE) is a predicate wrapper.
;; Include mask predicate && length predicate && vector policy.

;; -------------------------------------------------------------------------------
;; ---- Predicated Mov
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.4. Vector Unit-Stride Instructions
;; - 11.15 Vector Integer Merge Instructions
;; - 11.16 Vector Integer Move Instructions
;; - 13.16 Vector Floating-Point Move Instruction
;; - 15.1 Vector Mask-Register Logical Instructions
;; -------------------------------------------------------------------------------

;; vle.v/vse.v/vmv.v.v.
;; For vle.v/vmv.v.v, we may need merge and mask operand.
;; For vse.v, we don't need merge operand, so it should always match "vu".
;; constraint alternative 0 ~ 1 match vle.v.
;; constraint alternative 2 match vse.v.
;; constraint alternative 3 match vmv.v.v.

;; If operand 3 is a const_vector, then it is left to pred_braordcast patterns.
(define_expand "@pred_mov<mode>"
  [(set (match_operand:V_VLS 0 "nonimmediate_operand")
    (if_then_else:V_VLS
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand")
         (match_operand 4 "vector_length_operand")
         (match_operand 5 "const_int_operand")
         (match_operand 6 "const_int_operand")
         (match_operand 7 "const_int_operand")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (match_operand:V_VLS 3 "vector_move_operand")
      (match_operand:V_VLS 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

;; vle.v/vse.v,vmv.v.v
(define_insn_and_split "*pred_mov<mode>"
  [(set (match_operand:V_VLS 0 "nonimmediate_operand"            "=vr,    vr,    vd,     m,    vr,    vr")
    (if_then_else:V_VLS
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,   Wc1,    vm, vmWc1,   Wc1,   Wc1")
         (match_operand 4 "vector_length_operand"              "   rK,    rK,    rK,    rK,    rK,    rK")
         (match_operand 5 "const_int_operand"                  "    i,     i,     i,     i,     i,     i")
         (match_operand 6 "const_int_operand"                  "    i,     i,     i,     i,     i,     i")
         (match_operand 7 "const_int_operand"                  "    i,     i,     i,     i,     i,     i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (match_operand:V_VLS 3 "reg_or_mem_operand"              "    m,     m,     m,    vr,    vr,    vr")
      (match_operand:V_VLS 2 "vector_merge_operand"            "    0,    vu,    vu,    vu,    vu,     0")))]
  "(TARGET_VECTOR
    && (register_operand (operands[0], <MODE>mode)
        || register_operand (operands[3], <MODE>mode)))"
  "@
   vle<sew>.v\t%0,%3%p1
   vle<sew>.v\t%0,%3
   vle<sew>.v\t%0,%3,%1.t
   vse<sew>.v\t%3,%0%p1
   vmv.v.v\t%0,%3
   vmv.v.v\t%0,%3"
  "&& riscv_vector::whole_reg_to_reg_move_p (operands, <MODE>mode, 7)"
  [(set (match_dup 0) (match_dup 3))]
  ""
  [(set_attr "type" "vlde,vlde,vlde,vste,vimov,vimov")
   (set_attr "mode" "<MODE>")])

;; Dedicated pattern for vse.v instruction since we can't reuse pred_mov pattern to include
;; memory operand as input which will produce inferior codegen.
(define_insn "@pred_store<mode>"
  [(set (match_operand:V 0 "memory_operand"                 "+m")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1")
	     (match_operand 3 "vector_length_operand"    "   rK")
	     (match_operand 4 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operand:V 2 "register_operand"         "    vr")
	  (match_dup 0)))]
  "TARGET_VECTOR"
  "vse<sew>.v\t%2,%0%p1"
  [(set_attr "type" "vste")
   (set_attr "mode" "<MODE>")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "vl_op_idx" "3")])

;; vlm.v/vsm.v/vmclr.m/vmset.m.
;; constraint alternative 0 match vlm.v.
;; constraint alternative 1 match vsm.v.
;; constraint alternative 3 match vmclr.m.
;; constraint alternative 4 match vmset.m.
(define_insn_and_split "@pred_mov<mode>"
  [(set (match_operand:VB_VLS 0 "nonimmediate_operand"               "=vr,   m,  vr,  vr,  vr")
	(if_then_else:VB_VLS
	  (unspec:VB_VLS
	    [(match_operand:VB_VLS 1 "vector_all_trues_mask_operand" "Wc1, Wc1, Wc1, Wc1, Wc1")
	     (match_operand 4 "vector_length_operand"            " rK,  rK,  rK,  rK,  rK")
	     (match_operand 5 "const_int_operand"                "  i,   i,   i,   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operand:VB_VLS 3 "vector_move_operand"              "  m,  vr,  vr, Wc0, Wc1")
	  (match_operand:VB_VLS 2 "vector_undef_operand"             " vu,  vu,  vu,  vu,  vu")))]
  "TARGET_VECTOR"
  "@
   vlm.v\t%0,%3
   vsm.v\t%3,%0
   vmmv.m\t%0,%3
   vmclr.m\t%0
   vmset.m\t%0"
  "&& riscv_vector::whole_reg_to_reg_move_p (operands, <MODE>mode, 5)"
  [(set (match_dup 0) (match_dup 3))]
  ""
  [(set_attr "type" "vldm,vstm,vmalu,vmalu,vmalu")
   (set_attr "mode" "<MODE>")])

;; Dedicated pattern for vsm.v instruction since we can't reuse pred_mov pattern to include
;; memory operand as input which will produce inferior codegen.
(define_insn "@pred_store<mode>"
  [(set (match_operand:VB 0 "memory_operand"                      "+m")
	(if_then_else:VB
	  (unspec:VB
	    [(match_operand:VB 1 "vector_all_trues_mask_operand" "Wc1")
	     (match_operand 3 "vector_length_operand"            " rK")
	     (match_operand 4 "const_int_operand"                "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operand:VB 2 "register_operand"                 " vr")
	  (match_dup 0)))]
  "TARGET_VECTOR"
  "vsm.v\t%2,%0"
  [(set_attr "type" "vstm")
   (set_attr "mode" "<MODE>")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "vl_op_idx" "3")])

(define_insn "@pred_merge<mode>"
  [(set (match_operand:V_VLS 0 "register_operand"        "=vd,vd,vd,vd")
    (if_then_else:V_VLS
      (unspec:<VM>
        [(match_operand 5 "vector_length_operand"    " rK,rK,rK,rK")
         (match_operand 6 "const_int_operand"        "  i, i, i, i")
         (match_operand 7 "const_int_operand"        "  i, i, i, i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (vec_merge:V_VLS
        (match_operand:V_VLS 3 "vector_arith_operand"    " vr,vr,vi,vi")
        (match_operand:V_VLS 2 "register_operand"        " vr,vr,vr,vr")
	(match_operand:<VM> 4 "register_operand"     " vm,vm,vm,vm"))
      (match_operand:V_VLS 1 "vector_merge_operand"      " vu, 0,vu, 0")))]
  "TARGET_VECTOR"
  "vmerge.v%o3m\t%0,%2,%v3,%4"
  [(set_attr "type" "vimerge")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_merge<mode>_scalar"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand"   "=vd,vd")
    (if_then_else:V_VLSI_QHS
      (unspec:<VM>
        [(match_operand 5 "vector_length_operand"    " rK,rK")
         (match_operand 6 "const_int_operand"        "  i, i")
         (match_operand 7 "const_int_operand"        "  i, i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (vec_merge:V_VLSI_QHS
	(vec_duplicate:V_VLSI_QHS
          (match_operand:<VEL> 3 "register_operand"  "  r, r"))
        (match_operand:V_VLSI_QHS 2 "register_operand"   " vr,vr")
	(match_operand:<VM> 4 "register_operand"     " vm,vm"))
      (match_operand:V_VLSI_QHS 1 "vector_merge_operand" " vu, 0")))]
  "TARGET_VECTOR"
  "vmerge.vxm\t%0,%2,%3,%4"
  [(set_attr "type" "vimerge")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_merge<mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
    (if_then_else:V_VLSI_D
      (unspec:<VM>
        [(match_operand 5 "vector_length_operand")
         (match_operand 6 "const_int_operand")
         (match_operand 7 "const_int_operand")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (vec_merge:V_VLSI_D
	(vec_duplicate:V_VLSI_D
          (match_operand:<VEL> 3 "reg_or_int_operand"))
        (match_operand:V_VLSI_D 2 "register_operand")
	(match_operand:<VM> 4 "register_operand"))
      (match_operand:V_VLSI_D 1 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[3],
	/* vl */operands[5],
	<MODE>mode,
	riscv_vector::simm5_p (operands[3]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_merge<mode> (operands[0], operands[1],
	       operands[2], boardcast_scalar, operands[4], operands[5],
	       operands[6], operands[7]));
        },
	(riscv_vector::avl_type) INTVAL (operands[7])))
    DONE;
})

(define_insn "*pred_merge<mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"     "=vd,vd")
    (if_then_else:V_VLSI_D
      (unspec:<VM>
        [(match_operand 5 "vector_length_operand"    " rK,rK")
         (match_operand 6 "const_int_operand"        "  i, i")
         (match_operand 7 "const_int_operand"        "  i, i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (vec_merge:V_VLSI_D
	(vec_duplicate:V_VLSI_D
          (match_operand:<VEL> 3 "register_operand"  "  r, r"))
        (match_operand:V_VLSI_D 2 "register_operand"     " vr,vr")
	(match_operand:<VM> 4 "register_operand"     " vm,vm"))
      (match_operand:V_VLSI_D 1 "vector_merge_operand"   " vu, 0")))]
  "TARGET_VECTOR"
  "vmerge.vxm\t%0,%2,%3,%4"
  [(set_attr "type" "vimerge")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_merge<mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"         "=vd,vd")
    (if_then_else:V_VLSI_D
      (unspec:<VM>
        [(match_operand 5 "vector_length_operand"        " rK,rK")
         (match_operand 6 "const_int_operand"            "  i, i")
         (match_operand 7 "const_int_operand"            "  i, i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (vec_merge:V_VLSI_D
	(vec_duplicate:V_VLSI_D
	  (sign_extend:<VEL>
            (match_operand:<VSUBEL> 3 "register_operand" "  r, r")))
        (match_operand:V_VLSI_D 2 "register_operand"         " vr,vr")
	(match_operand:<VM> 4 "register_operand"         " vm,vm"))
      (match_operand:V_VLSI_D 1 "vector_merge_operand"       " vu, 0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vmerge.vxm\t%0,%2,%3,%4"
  [(set_attr "type" "vimerge")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated Broadcast
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.5. Vector Strided Instructions (zero stride)
;; - 11.16 Vector Integer Move Instructions (vmv.v.x)
;; - 13.16 Vector Floating-Point Move Instruction (vfmv.v.f)
;; - 16.1 Integer Scalar Move Instructions (vmv.s.x)
;; - 16.2 Floating-Point Scalar Move Instructions (vfmv.s.f)
;; -------------------------------------------------------------------------------

;; According to RVV ISA, vector-scalar instruction doesn't support
;; operand fetched from 2 consecutive registers, so we should use
;; vlse.v which is a memory access to broadcast a DImode scalar into a vector.
;;
;; Since the optimization flow in GCC is as follows:
;; expand --> LICM (Loop invariant) --> split.
;; To use LICM optimization, we postpone generation of vlse.v to split stage since
;; a memory access instruction can not be optimized by LICM (Loop invariant).
(define_expand "@pred_broadcast<mode>"
  [(set (match_operand:V_VLS 0 "register_operand")
	(if_then_else:V_VLS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_broadcast_mask_operand")
	     (match_operand 4 "vector_length_operand")
	     (match_operand 5 "const_int_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_duplicate:V_VLS
	    (match_operand:<VEL> 3 "direct_broadcast_operand"))
	  (match_operand:V_VLS 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  /* Transform vmv.v.x/vfmv.v.f (avl = 1) into vmv.s.x since vmv.s.x/vfmv.s.f
     has better chances to do vsetvl fusion in vsetvl pass.  */
  if (riscv_vector::splat_to_scalar_move_p (operands))
    {
      operands[1] = riscv_vector::gen_scalar_move_mask (<VM>mode);
      operands[3] = force_reg (<VEL>mode, operands[3]);
    }
  /* Handle vmv.s.x instruction (Wb1 mask) which has memory scalar.  */
  else if (satisfies_constraint_Wdm (operands[3]))
    {
      if (satisfies_constraint_Wb1 (operands[1]))
	{
	  /* Case 1: vmv.s.x (TA, x == memory) ==> vlse.v (TA)  */
	  if (satisfies_constraint_vu (operands[2]))
	    operands[1] = CONSTM1_RTX (<VM>mode);
	  else if (GET_MODE_BITSIZE (<VEL>mode) > GET_MODE_BITSIZE (Pmode))
	    {
	      /* Case 2: vmv.s.x (TU, x == memory) ==>
			   vl = 0 or 1; + vlse.v (TU) in RV32 system  */
	      operands[4] = riscv_vector::gen_avl_for_scalar_move (operands[4]);
	      operands[1] = CONSTM1_RTX (<VM>mode);
	    }
	  else
	    /* Case 3: load x (memory) to register.  */
	    operands[3] = force_reg (<VEL>mode, operands[3]);
	}
    }
  else if (GET_MODE_BITSIZE (<VEL>mode) > GET_MODE_BITSIZE (Pmode)
           && (immediate_operand (operands[3], Pmode)
	       || (CONST_POLY_INT_P (operands[3])
	           && known_ge (rtx_to_poly_int64 (operands[3]), 0U)
		   && known_le (rtx_to_poly_int64 (operands[3]), GET_MODE_SIZE (<MODE>mode)))))
    {
      rtx tmp = gen_reg_rtx (Pmode);
      poly_int64 value = rtx_to_poly_int64 (operands[3]);
      emit_move_insn (tmp, gen_int_mode (value, Pmode));
      operands[3] = gen_rtx_SIGN_EXTEND (<VEL>mode, tmp);
    }
  else
    operands[3] = force_reg (<VEL>mode, operands[3]);
})

(define_insn_and_split "*pred_broadcast<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"                 "=vr, vr, vd, vd, vr, vr, vr, vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_broadcast_mask_operand" "Wc1,Wc1, vm, vm,Wc1,Wc1,Wb1,Wb1")
	     (match_operand 4 "vector_length_operand"              " rK, rK, rK, rK, rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"                  "  i,  i,  i,  i,  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"                  "  i,  i,  i,  i,  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"                  "  i,  i,  i,  i,  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_duplicate:V_VLSI
	    (match_operand:<VEL> 3 "direct_broadcast_operand"       " r,  r,Wdm,Wdm,Wdm,Wdm,  r,  r"))
	  (match_operand:V_VLSI 2 "vector_merge_operand"            "vu,  0, vu,  0, vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "@
   vmv.v.x\t%0,%3
   vmv.v.x\t%0,%3
   vlse<sew>.v\t%0,%3,zero,%1.t
   vlse<sew>.v\t%0,%3,zero,%1.t
   vlse<sew>.v\t%0,%3,zero
   vlse<sew>.v\t%0,%3,zero
   vmv.s.x\t%0,%3
   vmv.s.x\t%0,%3"
  "(register_operand (operands[3], <VEL>mode)
  || CONST_POLY_INT_P (operands[3]))
  && GET_MODE_BITSIZE (<VEL>mode) > GET_MODE_BITSIZE (Pmode)"
  [(set (match_dup 0)
	(if_then_else:V_VLSI (unspec:<VM> [(match_dup 1) (match_dup 4)
	     (match_dup 5) (match_dup 6) (match_dup 7)
	     (reg:SI VL_REGNUM) (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_duplicate:V_VLSI (match_dup 3))
	  (match_dup 2)))]
  {
    gcc_assert (can_create_pseudo_p ());
    if (CONST_POLY_INT_P (operands[3]))
      {
	rtx tmp = gen_reg_rtx (<VEL>mode);
	emit_move_insn (tmp, operands[3]);
	operands[3] = tmp;
      }
    rtx m = assign_stack_local (<VEL>mode, GET_MODE_SIZE (<VEL>mode),
				GET_MODE_ALIGNMENT (<VEL>mode));
    m = validize_mem (m);
    emit_move_insn (m, operands[3]);
    m = gen_rtx_MEM (<VEL>mode, force_reg (Pmode, XEXP (m, 0)));
    operands[3] = m;

    /* For SEW = 64 in RV32 system, we expand vmv.s.x:
       andi a2,a2,1
       vsetvl zero,a2,e64
       vlse64.v  */
    if (satisfies_constraint_Wb1 (operands[1]))
      {
	operands[4] = riscv_vector::gen_avl_for_scalar_move (operands[4]);
	operands[1] = CONSTM1_RTX (<VM>mode);
      }
  }
  [(set_attr "type" "vimov,vimov,vlds,vlds,vlds,vlds,vimovxv,vimovxv")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_broadcast<mode>_zvfh"
  [(set (match_operand:V_VLSF    0 "register_operand"              "=vr,  vr,  vr,  vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_broadcast_mask_operand" "Wc1, Wc1, Wb1, Wb1")
	     (match_operand      4 "vector_length_operand"         " rK,  rK,  rK,  rK")
	     (match_operand      5 "const_int_operand"             "  i,   i,   i,   i")
	     (match_operand      6 "const_int_operand"             "  i,   i,   i,   i")
	     (match_operand      7 "const_int_operand"             "  i,   i,   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_duplicate:V_VLSF
	    (match_operand:<VEL> 3 "direct_broadcast_operand"      "  f,   f,   f,   f"))
	  (match_operand:V_VLSF  2 "vector_merge_operand"          " vu,   0,  vu,   0")))]
  "TARGET_VECTOR"
  "@
   vfmv.v.f\t%0,%3
   vfmv.v.f\t%0,%3
   vfmv.s.f\t%0,%3
   vfmv.s.f\t%0,%3"
  [(set_attr "type" "vfmov,vfmov,vfmovfv,vfmovfv")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_broadcast<mode>_zvfhmin"
  [(set (match_operand:V_VLSF_ZVFHMIN   0 "register_operand"              "=vr,  vr,  vr,  vr")
	(if_then_else:V_VLSF_ZVFHMIN
	  (unspec:<VM>
	    [(match_operand:<VM>        1 "vector_broadcast_mask_operand" " vm,  vm, Wc1, Wc1")
	     (match_operand             4 "vector_length_operand"         " rK,  rK,  rK,  rK")
	     (match_operand             5 "const_int_operand"             "  i,   i,   i,   i")
	     (match_operand             6 "const_int_operand"             "  i,   i,   i,   i")
	     (match_operand             7 "const_int_operand"             "  i,   i,   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_duplicate:V_VLSF_ZVFHMIN
	    (match_operand:<VEL>        3 "direct_broadcast_operand"      "Wdm, Wdm, Wdm, Wdm"))
	  (match_operand:V_VLSF_ZVFHMIN 2 "vector_merge_operand"          " vu,   0,  vu,   0")))]
  "TARGET_VECTOR"
  "@
   vlse<sew>.v\t%0,%3,zero,%1.t
   vlse<sew>.v\t%0,%3,zero,%1.t
   vlse<sew>.v\t%0,%3,zero
   vlse<sew>.v\t%0,%3,zero"
  [(set_attr "type" "vlds,vlds,vlds,vlds")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_broadcast<mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"               "=vr, vr, vr, vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_broadcast_mask_operand" "Wc1,Wc1,Wb1,Wb1")
	     (match_operand 4 "vector_length_operand"              " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"                  "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"                  "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"                  "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_duplicate:V_VLSI_D
	    (sign_extend:<VEL>
	      (match_operand:<VSUBEL> 3 "register_operand"          " r,  r,  r,  r")))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand"          "vu,  0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "@
   vmv.v.x\t%0,%3
   vmv.v.x\t%0,%3
   vmv.s.x\t%0,%3
   vmv.s.x\t%0,%3"
  [(set_attr "type" "vimov,vimov,vimovxv,vimovxv")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_broadcast<mode>_zero"
  [(set (match_operand:V_VLS 0 "register_operand"                          "=vr,    vr")
    (if_then_else:V_VLS
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_least_significant_set_mask_operand" "Wb1,   Wb1")
         (match_operand 4 "vector_length_operand"                          " rK,    rK")
         (match_operand 5 "const_int_operand"                              "  i,     i")
         (match_operand 6 "const_int_operand"                              "  i,     i")
         (match_operand 7 "const_int_operand"                              "  i,     i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (match_operand:V_VLS 3 "vector_const_0_operand"                      "Wc0,   Wc0")
      (match_operand:V_VLS 2 "vector_merge_operand"                        " vu,     0")))]
  "TARGET_VECTOR"
  "vmv.s.x\t%0,zero"
  [(set_attr "type" "vimovxv,vimovxv")
   (set_attr "mode" "<MODE>")])

;; Because (vec_duplicate imm) will be converted to (const_vector imm),
;; This pattern is used to handle this case.
(define_insn "*pred_broadcast<mode>_imm"
  [(set (match_operand:V_VLS 0 "register_operand"                     "=vr,    vr")
    (if_then_else:V_VLS
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_all_trues_mask_operand"      "  Wc1,   Wc1")
         (match_operand 4 "vector_length_operand"                   "   rK,    rK")
         (match_operand 5 "const_int_operand"                       "    i,     i")
         (match_operand 6 "const_int_operand"                       "    i,     i")
         (match_operand 7 "const_int_operand"                       "    i,     i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (match_operand:V_VLS 3 "vector_const_int_or_double_0_operand" "viWc0, viWc0")
      (match_operand:V_VLS 2 "vector_merge_operand"                 "   vu,     0")))]
  "TARGET_VECTOR"
  "vmv.v.i\t%0,%v3"
  [(set_attr "type" "vimov,vimov")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated Strided loads/stores
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.5. Vector Strided Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_strided_load<mode>"
  [(set (match_operand:V 0 "register_operand"              "=vr,    vr,    vd,    vr,    vr,    vd")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,   Wc1,    vm,    vmWc1,   Wc1,    vm")
	     (match_operand 5 "vector_length_operand"    "   rK,    rK,    rK,       rK,    rK,    rK")
	     (match_operand 6 "const_int_operand"        "    i,     i,     i,        i,     i,     i")
	     (match_operand 7 "const_int_operand"        "    i,     i,     i,        i,     i,     i")
	     (match_operand 8 "const_int_operand"        "    i,     i,     i,        i,     i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V
	    [(match_operand:V 3 "memory_operand"         "     m,     m,     m,    m,     m,     m")
	     (match_operand 4 "<V:stride_predicate>"     "<V:stride_load_constraint>")] UNSPEC_STRIDED)
	  (match_operand:V 2 "vector_merge_operand"      "     0,    vu,    vu,    0,    vu,    vu")))]
  "TARGET_VECTOR"
  "@
  vlse<sew>.v\t%0,%3,%z4%p1
  vlse<sew>.v\t%0,%3,%z4
  vlse<sew>.v\t%0,%3,%z4,%1.t
  vle<sew>.v\t%0,%3%p1
  vle<sew>.v\t%0,%3
  vle<sew>.v\t%0,%3,%1.t"
  [(set_attr "type" "vlds")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_strided_store<mode>"
  [(set (match_operand:V 0 "memory_operand"                 "+m,    m")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,    vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK,       rK")
	     (match_operand 5 "const_int_operand"        "    i,        i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V
	    [(match_operand 2 "<V:stride_predicate>"     "<V:stride_store_constraint>")
	     (match_operand:V 3 "register_operand"       "   vr,       vr")] UNSPEC_STRIDED)
	  (match_dup 0)))]
  "TARGET_VECTOR"
  "@
  vsse<sew>.v\t%3,%0,%z2%p1
  vse<sew>.v\t%3,%0%p1"
  [(set_attr "type" "vsts")
   (set_attr "mode" "<MODE>")
   (set (attr "avl_type_idx") (const_int 5))])

;; -------------------------------------------------------------------------------
;; ---- Predicated indexed loads/stores
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.6. Vector Indexed Instructions
;; -------------------------------------------------------------------------------

;; DEST eew is same as SOURCE eew, DEST register can overlap SOURCE.
(define_insn "@pred_indexed_<order>load<mode>_same_eew"
  [(set (match_operand:VINDEXED 0 "register_operand"        "=vd, vr,vd, vr")
	(if_then_else:VINDEXED
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"    " vm,Wc1,vm,Wc1")
	     (match_operand 5 "vector_length_operand"       " rK, rK,rK, rK")
	     (match_operand 6 "const_int_operand"           "  i,  i, i,  i")
	     (match_operand 7 "const_int_operand"           "  i,  i, i,  i")
	     (match_operand 8 "const_int_operand"           "  i,  i, i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VINDEXED
	    [(match_operand 3 "pmode_reg_or_0_operand"      " rJ, rJ,rJ, rJ")
	     (mem:BLK (scratch))
	     (match_operand:<VINDEX> 4 "register_operand"   " vr, vr,vr, vr")] ORDER)
	  (match_operand:VINDEXED 2 "vector_merge_operand"  " vu, vu, 0,  0")))]
  "TARGET_VECTOR"
  "vl<order>xei<sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<MODE>")])

;; DEST eew is greater than SOURCE eew.
(define_insn "@pred_indexed_<order>load<mode>_x2_greater_eew"
  [(set (match_operand:VEEWEXT2 0 "register_operand"                    "=&vr,  &vr")
	(if_then_else:VEEWEXT2
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                  "   rK,   rK")
	     (match_operand 6 "const_int_operand"                      "    i,    i")
	     (match_operand 7 "const_int_operand"                      "    i,    i")
	     (match_operand 8 "const_int_operand"                      "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VEEWEXT2
	    [(match_operand 3 "pmode_reg_or_0_operand"                 "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:<VINDEX_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr")] ORDER)
	  (match_operand:VEEWEXT2 2 "vector_merge_operand"             "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xei<double_trunc_sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_indexed_<order>load<mode>_x4_greater_eew"
  [(set (match_operand:VEEWEXT4 0 "register_operand"                    "=&vr,  &vr")
	(if_then_else:VEEWEXT4
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                  "   rK,   rK")
	     (match_operand 6 "const_int_operand"                      "    i,    i")
	     (match_operand 7 "const_int_operand"                      "    i,    i")
	     (match_operand 8 "const_int_operand"                      "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VEEWEXT4
	    [(match_operand 3 "pmode_reg_or_0_operand"                 "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:<VINDEX_QUAD_TRUNC> 4 "register_operand"   "   vr,   vr")] ORDER)
	  (match_operand:VEEWEXT4 2 "vector_merge_operand"             "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xei<quad_trunc_sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_indexed_<order>load<mode>_x8_greater_eew"
  [(set (match_operand:VEEWEXT8 0 "register_operand"                    "=&vr,  &vr")
	(if_then_else:VEEWEXT8
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                  "   rK,   rK")
	     (match_operand 6 "const_int_operand"                      "    i,    i")
	     (match_operand 7 "const_int_operand"                      "    i,    i")
	     (match_operand 8 "const_int_operand"                      "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VEEWEXT8
	    [(match_operand 3 "pmode_reg_or_0_operand"                 "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:<VINDEX_OCT_TRUNC> 4 "register_operand"    "   vr,   vr")] ORDER)
	  (match_operand:VEEWEXT8 2 "vector_merge_operand"             "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xei<oct_trunc_sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<MODE>")])

;; DEST eew is smaller than SOURCE eew.
(define_insn "@pred_indexed_<order>load<mode>_x2_smaller_eew"
  [(set (match_operand:VEEWTRUNC2 0 "register_operand"               "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:VEEWTRUNC2
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 6 "const_int_operand"                    "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                    "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                    "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VEEWTRUNC2
	    [(match_operand 3 "pmode_reg_or_0_operand"               " rJ, rJ, rJ, rJ,   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:<VINDEX_DOUBLE_EXT> 4 "register_operand" "  0,  0,  0,  0,   vr,   vr")] ORDER)
	  (match_operand:VEEWTRUNC2 2 "vector_merge_operand"         " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xei<double_ext_sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_indexed_<order>load<mode>_x4_smaller_eew"
  [(set (match_operand:VEEWTRUNC4 0 "register_operand"             "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:VEEWTRUNC4
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VEEWTRUNC4
	    [(match_operand 3 "pmode_reg_or_0_operand"             " rJ, rJ, rJ, rJ,   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:<VINDEX_QUAD_EXT> 4 "register_operand" "  0,  0,  0,  0,   vr,   vr")] ORDER)
	  (match_operand:VEEWTRUNC4 2 "vector_merge_operand"       " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xei<quad_ext_sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_indexed_<order>load<mode>_x8_smaller_eew"
  [(set (match_operand:VEEWTRUNC8 0 "register_operand"            "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:VEEWTRUNC8
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"          " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"             " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 6 "const_int_operand"                 "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                 "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                 "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VEEWTRUNC8
	    [(match_operand 3 "pmode_reg_or_0_operand"            " rJ, rJ, rJ, rJ,   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:<VINDEX_OCT_EXT> 4 "register_operand" "  0,  0,  0,  0,   vr,   vr")] ORDER)
	  (match_operand:VEEWTRUNC8 2 "vector_merge_operand"      " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xei<oct_ext_sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_indexed_<order>store<RATIO64:mode><RATIO64I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"      "  rJ")
	   (match_operand:RATIO64I 2 "register_operand" "  vr")
	   (match_operand:RATIO64 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<RATIO64I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<RATIO64:MODE>")])

(define_insn "@pred_indexed_<order>store<RATIO32:mode><RATIO32I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"      "  rJ")
	   (match_operand:RATIO32I 2 "register_operand" "  vr")
	   (match_operand:RATIO32 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<RATIO32I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<RATIO32:MODE>")])

(define_insn "@pred_indexed_<order>store<RATIO16:mode><RATIO16I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"      "  rJ")
	   (match_operand:RATIO16I 2 "register_operand" "  vr")
	   (match_operand:RATIO16 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<RATIO16I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<RATIO16:MODE>")])

(define_insn "@pred_indexed_<order>store<RATIO8:mode><RATIO8I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"      "  rJ")
	   (match_operand:RATIO8I 2 "register_operand" "  vr")
	   (match_operand:RATIO8 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<RATIO8I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<RATIO8:MODE>")])

(define_insn "@pred_indexed_<order>store<RATIO4:mode><RATIO4I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"      "  rJ")
	   (match_operand:RATIO4I 2 "register_operand" "  vr")
	   (match_operand:RATIO4 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<RATIO4I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<RATIO4:MODE>")])

(define_insn "@pred_indexed_<order>store<RATIO2:mode><RATIO2I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"       "  rJ")
	   (match_operand:RATIO2I 2 "register_operand"  "  vr")
	   (match_operand:RATIO2 3 "register_operand"   "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<RATIO2I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<RATIO2:MODE>")])

(define_insn "@pred_indexed_<order>store<RATIO1:mode><RATIO1:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"       "  rJ")
	   (match_operand:RATIO1 2 "register_operand"   "  vr")
	   (match_operand:RATIO1 3 "register_operand"    "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<RATIO1:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<RATIO1:MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated integer binary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 11.1 Vector Single-Width Integer Add and Subtract
;; - 11.4 Vector Integer Add-with-Carry/Subtract-with-Borrow Instructions
;; - 11.5 Vector Bitwise Logical Instructions
;; - 11.6 Vector Single-Width Bit Shift Instructions
;; - 11.9 Vector Integer Min/Max Instructions
;; - 11.10 Vector Single-Width Integer Multiply Instructions
;; - 11.11 Vector Integer Divide Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_<optab><mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd, vd, vr, vr, vd, vd, vr, vr, vd, vd, vr, vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1, Wc1, vm, vm,Wc1,Wc1, vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK,  rK, rK, rK, rK, rK, rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,   i,  i,  i,  i,  i,  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,   i,  i,  i,  i,  i,  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,   i,  i,  i,  i,  i,  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_int_binop:V_VLSI
	    (match_operand:V_VLSI 3 "<binop_rhs1_predicate>" "<binop_rhs1_constraint>")
	    (match_operand:V_VLSI 4 "<binop_rhs2_predicate>" "<binop_rhs2_constraint>"))
	  (match_operand:V_VLSI 2 "vector_merge_operand"     "vu,0,vu,0,vu,0,vu,0,vu,0,vu,0")))]
  "TARGET_VECTOR"
  "@
   v<insn>.vv\t%0,%3,%4%p1
   v<insn>.vv\t%0,%3,%4%p1
   v<insn>.vv\t%0,%3,%4%p1
   v<insn>.vv\t%0,%3,%4%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1
   v<binop_reverse_vi_variant_insn>\t%0,<binop_reverse_vi_variant_op>%p1
   v<binop_reverse_vi_variant_insn>\t%0,<binop_reverse_vi_variant_op>%p1
   v<binop_reverse_vi_variant_insn>\t%0,<binop_reverse_vi_variant_op>%p1
   v<binop_reverse_vi_variant_insn>\t%0,<binop_reverse_vi_variant_op>%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

;; vx instructions patterns.
;; Note: Unlike vv patterns, we should split them since they are variant.
;; For vsll.vx/vsra.vx/vsrl.vx the scalar mode should be Pmode wheras the
;; scalar mode is inner mode of the RVV mode for other vx patterns.
(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd,vd, vr, vr,vd,vd, vr, vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vm,vm,Wc1,Wc1,vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"     "rK,rK, rK, rK,rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"         " i, i,  i,  i, i, i,  i,  i")
	     (match_operand 7 "const_int_operand"         " i, i,  i,  i, i, i,  i,  i")
	     (match_operand 8 "const_int_operand"         " i, i,  i,  i, i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_shift:V_VLSI
	    (match_operand:V_VLSI 3 "register_operand"        "vr,vr, vr, vr,vr,vr, vr, vr")
	    (match_operand 4 "pmode_reg_or_uimm5_operand" " r, r,  r,  r, K, K,  K,  K"))
	  (match_operand:V_VLSI 2 "vector_merge_operand"      "vu, 0, vu,  0,vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.v%o4\t%0,%3,%4%p1"
  [(set_attr "type" "vshift")
   (set_attr "mode" "<MODE>")])

;; Handle GET_MODE_INNER (mode) = QImode, HImode, SImode.
(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand"      "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_commutative_binop:V_VLSI_QHS
	    (vec_duplicate:V_VLSI_QHS
	      (match_operand:<VEL> 4 "reg_or_0_operand"  "rJ,rJ, rJ, rJ"))
	    (match_operand:V_VLSI_QHS 3 "register_operand"   "vr,vr, vr, vr"))
	  (match_operand:V_VLSI_QHS 2 "vector_merge_operand" "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand"      "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_non_commutative_binop:V_VLSI_QHS
	    (match_operand:V_VLSI_QHS 3 "register_operand"   "vr,vr, vr, vr")
	    (vec_duplicate:V_VLSI_QHS
	      (match_operand:<VEL> 4 "reg_or_0_operand"  "rJ,rJ, rJ, rJ")))
	  (match_operand:V_VLSI_QHS 2 "vector_merge_operand" "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_sub<mode>_reverse_scalar"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand"      "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_QHS
	    (vec_duplicate:V_VLSI_QHS
	      (match_operand:<VEL> 4 "reg_or_0_operand"  "rJ,rJ, rJ, rJ"))
	    (match_operand:V_VLSI_QHS 3 "register_operand"   "vr,vr, vr, vr"))
	  (match_operand:V_VLSI_QHS 2 "vector_merge_operand" "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vrsub.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vialu")
   (set_attr "mode" "<MODE>")])

;; Handle GET_MODE_INNER (mode) = DImode. We need to split them since
;; we need to deal with SEW = 64 in RV32 system.
(define_expand "@pred_<optab><mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_commutative_binop:V_VLSI_D
	    (vec_duplicate:V_VLSI_D
	      (match_operand:<VEL> 4 "reg_or_int_operand"))
	    (match_operand:V_VLSI_D 3 "register_operand"))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	riscv_vector::has_vi_variant_p (<CODE>, operands[4]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_<optab><mode> (operands[0], operands[1],
	       operands[2], operands[3], boardcast_scalar, operands[5],
	       operands[6], operands[7], operands[8]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_<optab><mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"         "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_commutative_binop:V_VLSI_D
	    (vec_duplicate:V_VLSI_D
	      (match_operand:<VEL> 4 "reg_or_0_operand"  "rJ,rJ, rJ, rJ"))
	    (match_operand:V_VLSI_D 3 "register_operand"     "vr,vr, vr, vr"))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand"   "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_<optab><mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"             "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"        "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"            " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"            " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"            " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_commutative_binop:V_VLSI_D
	    (vec_duplicate:V_VLSI_D
	      (sign_extend:<VEL>
	        (match_operand:<VSUBEL> 4 "reg_or_0_operand" "rJ,rJ, rJ, rJ")))
	    (match_operand:V_VLSI_D 3 "register_operand"         "vr,vr, vr, vr"))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand"       "vu, 0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "v<insn>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_<optab><mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_non_commutative_binop:V_VLSI_D
	    (match_operand:V_VLSI_D 3 "register_operand")
	    (vec_duplicate:V_VLSI_D
	      (match_operand:<VEL> 4 "reg_or_int_operand")))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	riscv_vector::has_vi_variant_p (<CODE>, operands[4]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_<optab><mode> (operands[0], operands[1],
	       operands[2], operands[3], boardcast_scalar, operands[5],
	       operands[6], operands[7], operands[8]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_<optab><mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"         "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_non_commutative_binop:V_VLSI_D
	    (match_operand:V_VLSI_D 3 "register_operand"     "vr,vr, vr, vr")
	    (vec_duplicate:V_VLSI_D
	      (match_operand:<VEL> 4 "reg_or_0_operand"  "rJ,rJ, rJ, rJ")))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand"   "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_<optab><mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"             "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"        "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"            " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"            " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"            " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_non_commutative_binop:V_VLSI_D
	    (match_operand:V_VLSI_D 3 "register_operand"         "vr,vr, vr, vr")
	    (vec_duplicate:V_VLSI_D
	      (sign_extend:<VEL>
	        (match_operand:<VSUBEL> 4 "reg_or_0_operand" "rJ,rJ, rJ, rJ"))))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand"       "vu, 0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "v<insn>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_sub<mode>_reverse_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_D
	    (vec_duplicate:V_VLSI_D
	      (match_operand:<VEL> 4 "reg_or_int_operand"))
	    (match_operand:V_VLSI_D 3 "register_operand"))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	riscv_vector::neg_simm5_p (operands[4]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_sub<mode> (operands[0], operands[1],
	       operands[2], boardcast_scalar, operands[3], operands[5],
	       operands[6], operands[7], operands[8]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_sub<mode>_reverse_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"         "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_D
	    (vec_duplicate:V_VLSI_D
	      (match_operand:<VEL> 4 "reg_or_0_operand"  "rJ,rJ, rJ, rJ"))
	    (match_operand:V_VLSI_D 3 "register_operand"     "vr,vr, vr, vr"))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand"   "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vrsub.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vialu")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_sub<mode>_extended_reverse_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"             "=vd,vd, vr, vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"        "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"            " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"            " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"            " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_D
	    (vec_duplicate:V_VLSI_D
	      (sign_extend:<VEL>
	        (match_operand:<VSUBEL> 4 "reg_or_0_operand" "rJ,rJ, rJ, rJ")))
	    (match_operand:V_VLSI_D 3 "register_operand"         "vr,vr, vr, vr"))
	  (match_operand:V_VLSI_D 2 "vector_merge_operand"       "vu, 0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vrsub.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vialu")
   (set_attr "mode" "<MODE>")])

;; Multiply High instructions.
(define_insn "@pred_mulh<v_su><mode>"
  [(set (match_operand:VFULLI 0 "register_operand"       "=vd,vd, vr, vr")
	(if_then_else:VFULLI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI
	    [(match_operand:VFULLI 3 "register_operand"  "vr,vr, vr, vr")
	     (match_operand:VFULLI 4 "register_operand"  "vr,vr, vr, vr")] VMULH)
	  (match_operand:VFULLI 2 "vector_merge_operand" "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vmulh<v_su>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vimul")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_mulh<v_su><mode>_scalar"
  [(set (match_operand:VI_QHS 0 "register_operand"       "=vd,vd, vr, vr")
	(if_then_else:VI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_QHS
	    [(vec_duplicate:VI_QHS
	       (match_operand:<VEL> 4 "reg_or_0_operand"  "rJ,rJ, rJ, rJ"))
	     (match_operand:VI_QHS 3 "register_operand"   "vr,vr, vr, vr")] VMULH)
	  (match_operand:VI_QHS 2 "vector_merge_operand"  "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vmulh<v_su>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vimul")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_mulh<v_su><mode>_scalar"
  [(set (match_operand:VFULLI_D 0 "register_operand")
	(if_then_else:VFULLI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI_D
	    [(vec_duplicate:VFULLI_D
	       (match_operand:<VEL> 4 "reg_or_int_operand"))
	     (match_operand:VFULLI_D 3 "register_operand")] VMULH)
	  (match_operand:VFULLI_D 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_mulh<v_su><mode> (operands[0], operands[1],
	       operands[2], operands[3], boardcast_scalar, operands[5],
	       operands[6], operands[7], operands[8]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_mulh<v_su><mode>_scalar"
  [(set (match_operand:VFULLI_D 0 "register_operand"       "=vd,vd, vr, vr")
	(if_then_else:VFULLI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"   "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"      "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"          " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"          " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"          " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI_D
	    [(vec_duplicate:VFULLI_D
	       (match_operand:<VEL> 4 "reg_or_0_operand"   "rJ,rJ, rJ, rJ"))
	     (match_operand:VFULLI_D 3 "register_operand"  "vr,vr, vr, vr")] VMULH)
	  (match_operand:VFULLI_D 2 "vector_merge_operand" "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vmulh<v_su>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vimul")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_mulh<v_su><mode>_extended_scalar"
  [(set (match_operand:VFULLI_D 0 "register_operand"          "=vd,vd, vr, vr")
	(if_then_else:VFULLI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"         "rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"             " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"             " i, i,  i,  i")
	     (match_operand 8 "const_int_operand"             " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VFULLI_D
	    [(vec_duplicate:VFULLI_D
	       (sign_extend:<VEL>
	         (match_operand:<VSUBEL> 4 "reg_or_0_operand" "rJ,rJ, rJ, rJ")))
	     (match_operand:VFULLI_D 3 "register_operand"     "vr,vr, vr, vr")] VMULH)
	  (match_operand:VFULLI_D 2 "vector_merge_operand"    "vu, 0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vmulh<v_su>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vimul")
   (set_attr "mode" "<MODE>")])

;; Vector Integer Add-with-Carry / Subtract-with-Borrow Instructions
(define_insn "@pred_adc<mode>"
  [(set (match_operand:VI 0 "register_operand"           "=vd,vd,vd,vd")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"     "rK,rK,rK,rK")
	     (match_operand 6 "const_int_operand"         " i, i, i, i")
	     (match_operand 7 "const_int_operand"         " i, i, i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI
	     [(plus:VI
	       (match_operand:VI 2 "register_operand"     "vr,vr,vr,vr")
	       (match_operand:VI 3 "vector_arith_operand" "vr,vr,vi,vi"))
	     (match_operand:<VM> 4 "register_operand"     "vm,vm,vm,vm")] UNSPEC_VADC)
	  (match_operand:VI 1 "vector_merge_operand"      "vu, 0,vu, 0")))]
  "TARGET_VECTOR"
  "vadc.v%o3m\t%0,%2,%v3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "@pred_sbc<mode>"
  [(set (match_operand:VI 0 "register_operand"           "=vd,vd")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"     "rK,rK")
	     (match_operand 6 "const_int_operand"         " i, i")
	     (match_operand 7 "const_int_operand"         " i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI
	     [(minus:VI
	       (match_operand:VI 2 "register_operand"     "vr,vr")
	       (match_operand:VI 3 "register_operand"     "vr,vr"))
	      (match_operand:<VM> 4 "register_operand"    "vm,vm")] UNSPEC_VSBC)
	  (match_operand:VI 1 "vector_merge_operand"      "vu, 0")))]
  "TARGET_VECTOR"
  "vsbc.vvm\t%0,%2,%3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "@pred_adc<mode>_scalar"
  [(set (match_operand:VI_QHS 0 "register_operand"        "=vd,vd")
	(if_then_else:VI_QHS
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"      "rK,rK")
	     (match_operand 6 "const_int_operand"          " i, i")
	     (match_operand 7 "const_int_operand"          " i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_QHS
	     [(plus:VI_QHS
	       (vec_duplicate:VI_QHS
	         (match_operand:<VEL> 3 "register_operand" " r, r"))
	       (match_operand:VI_QHS 2 "register_operand"  "vr,vr"))
	     (match_operand:<VM> 4 "register_operand"      "vm,vm")] UNSPEC_VADC)
	  (match_operand:VI_QHS 1 "vector_merge_operand"   "vu, 0")))]
  "TARGET_VECTOR"
  "vadc.vxm\t%0,%2,%3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "@pred_sbc<mode>_scalar"
  [(set (match_operand:VI_QHS 0 "register_operand"         "=vd,vd")
	(if_then_else:VI_QHS
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"       "rK,rK")
	     (match_operand 6 "const_int_operand"           " i, i")
	     (match_operand 7 "const_int_operand"           " i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_QHS
	     [(minus:VI_QHS
	        (match_operand:VI_QHS 2 "register_operand"  "vr,vr")
	        (vec_duplicate:VI_QHS
	          (match_operand:<VEL> 3 "reg_or_0_operand" "rJ,rJ")))
	      (match_operand:<VM> 4 "register_operand"      "vm,vm")] UNSPEC_VSBC)
	  (match_operand:VI_QHS 1 "vector_merge_operand"    "vu, 0")))]
  "TARGET_VECTOR"
  "vsbc.vxm\t%0,%2,%z3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_expand "@pred_adc<mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	     [(plus:VI_D
	        (vec_duplicate:VI_D
	          (match_operand:<VEL> 3 "reg_or_int_operand"))
	        (match_operand:VI_D 2 "register_operand"))
	      (match_operand:<VM> 4 "register_operand")] UNSPEC_VADC)
	  (match_operand:VI_D 1 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[3],
	/* vl */operands[5],
	<MODE>mode,
	riscv_vector::simm5_p (operands[3]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_adc<mode> (operands[0], operands[1],
	       operands[2], boardcast_scalar, operands[4], operands[5],
	       operands[6], operands[7]));
        },
	(riscv_vector::avl_type) INTVAL (operands[7])))
    DONE;
})

(define_insn "*pred_adc<mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand"           "=vd,vd")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"       "rK,rK")
	     (match_operand 6 "const_int_operand"           " i, i")
	     (match_operand 7 "const_int_operand"           " i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	     [(plus:VI_D
	        (vec_duplicate:VI_D
	          (match_operand:<VEL> 3 "reg_or_0_operand" "rJ,rJ"))
	        (match_operand:VI_D 2 "register_operand"    "vr,vr"))
	      (match_operand:<VM> 4 "register_operand"      "vm,vm")] UNSPEC_VADC)
	  (match_operand:VI_D 1 "vector_merge_operand"      "vu, 0")))]
  "TARGET_VECTOR"
  "vadc.vxm\t%0,%2,%z3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "*pred_adc<mode>_extended_scalar"
  [(set (match_operand:VI_D 0 "register_operand"                "=vd,vd")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"            "rK,rK")
	     (match_operand 6 "const_int_operand"                " i, i")
	     (match_operand 7 "const_int_operand"                " i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	     [(plus:VI_D
	        (vec_duplicate:VI_D
	          (sign_extend:<VEL>
	            (match_operand:<VSUBEL> 3 "reg_or_0_operand" "rJ,rJ")))
	        (match_operand:VI_D 2 "register_operand"         "vr,vr"))
	      (match_operand:<VM> 4 "register_operand"           "vm,vm")] UNSPEC_VADC)
	  (match_operand:VI_D 1 "vector_merge_operand"           "vu, 0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vadc.vxm\t%0,%2,%z3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_expand "@pred_sbc<mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	     [(minus:VI_D
	        (match_operand:VI_D 2 "register_operand")
	        (vec_duplicate:VI_D
	          (match_operand:<VEL> 3 "reg_or_int_operand")))
	      (match_operand:<VM> 4 "register_operand")] UNSPEC_VSBC)
	  (match_operand:VI_D 1 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[3],
	/* vl */operands[5],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_sbc<mode> (operands[0], operands[1],
	       operands[2], boardcast_scalar, operands[4], operands[5],
	       operands[6], operands[7]));
        },
	(riscv_vector::avl_type) INTVAL (operands[7])))
    DONE;
})

(define_insn "*pred_sbc<mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand"           "=vd,vd")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"       "rK,rK")
	     (match_operand 6 "const_int_operand"           " i, i")
	     (match_operand 7 "const_int_operand"           " i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	     [(minus:VI_D
	        (match_operand:VI_D 2 "register_operand"    "vr,vr")
	        (vec_duplicate:VI_D
	          (match_operand:<VEL> 3 "reg_or_0_operand" "rJ,rJ")))
	      (match_operand:<VM> 4 "register_operand"      "vm,vm")] UNSPEC_VSBC)
	  (match_operand:VI_D 1 "vector_merge_operand"      "vu, 0")))]
  "TARGET_VECTOR"
  "vsbc.vxm\t%0,%2,%z3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "*pred_sbc<mode>_extended_scalar"
  [(set (match_operand:VI_D 0 "register_operand"                "=vd,vd")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand 5 "vector_length_operand"           "rK,rK")
	     (match_operand 6 "const_int_operand"               " i, i")
	     (match_operand 7 "const_int_operand"               " i, i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	     [(minus:VI_D
	        (match_operand:VI_D 2 "register_operand"         "vr,vr")
	        (vec_duplicate:VI_D
	          (sign_extend:<VEL>
	            (match_operand:<VSUBEL> 3 "reg_or_0_operand" "rJ,rJ"))))
	      (match_operand:<VM> 4 "register_operand"           "vm,vm")] UNSPEC_VSBC)
	  (match_operand:VI_D 1 "vector_merge_operand"           "vu, 0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vsbc.vxm\t%0,%2,%z3,%4"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "@pred_madc<mode>"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr, &vr")
	(unspec:<VM>
	   [(plus:VI
	     (match_operand:VI 1 "register_operand"     "  %0,  vr,  vr")
	     (match_operand:VI 2 "vector_arith_operand" "vrvi,  vr,  vi"))
	    (match_operand:<VM> 3 "register_operand"    "  vm,  vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand" "  rK,  rK,  rK")
	       (match_operand 5 "const_int_operand"     "   i,   i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMADC))]
  "TARGET_VECTOR"
  "vmadc.v%o2m\t%0,%1,%v2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,none,none")])

(define_insn "@pred_msbc<mode>"
  [(set (match_operand:<VM> 0 "register_operand"        "=vr, vr, &vr")
	(unspec:<VM>
	   [(minus:VI
	     (match_operand:VI 1 "register_operand"     "  0, vr,  vr")
	     (match_operand:VI 2 "register_operand"     " vr,  0,  vr"))
	    (match_operand:<VM> 3 "register_operand"    " vm, vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand" " rK, rK,  rK")
	       (match_operand 5 "const_int_operand"     "  i,  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMSBC))]
  "TARGET_VECTOR"
  "vmsbc.vvm\t%0,%1,%2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,thv,none")])

(define_insn "@pred_madc<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(plus:VI_QHS
	     (vec_duplicate:VI_QHS
	       (match_operand:<VEL> 2 "register_operand" "  r,   r"))
	     (match_operand:VI_QHS 1 "register_operand"  "  0,  vr"))
	    (match_operand:<VM> 3 "register_operand"     " vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand"  " rK,  rK")
	       (match_operand 5 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMADC))]
  "TARGET_VECTOR"
  "vmadc.vxm\t%0,%1,%2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,none")])

(define_insn "@pred_msbc<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(minus:VI_QHS
	     (vec_duplicate:VI_QHS
	       (match_operand:<VEL> 2 "reg_or_0_operand" " rJ,  rJ"))
	     (match_operand:VI_QHS 1 "register_operand"  "  0,  vr"))
	    (match_operand:<VM> 3 "register_operand"     " vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand"  " rK,  rK")
	       (match_operand 5 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMSBC))]
  "TARGET_VECTOR"
  "vmsbc.vxm\t%0,%1,%z2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,none")])

(define_expand "@pred_madc<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(unspec:<VM>
	   [(plus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_int_operand"))
	     (match_operand:VI_D 1 "register_operand"))
	    (match_operand:<VM> 3 "register_operand")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand")
	       (match_operand 5 "const_int_operand")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMADC))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[2],
	/* vl */operands[4],
	<MODE>mode,
	riscv_vector::simm5_p (operands[2]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_madc<mode> (operands[0], operands[1],
	       boardcast_scalar, operands[3], operands[4], operands[5]));
        },
	(riscv_vector::avl_type) INTVAL (operands[5])))
    DONE;
})

(define_insn "*pred_madc<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(plus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_0_operand" " rJ,  rJ"))
	     (match_operand:VI_D 1 "register_operand"    "  0,  vr"))
	    (match_operand:<VM> 3 "register_operand"     " vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand"  " rK,  rK")
	       (match_operand 5 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMADC))]
  "TARGET_VECTOR"
  "vmadc.vxm\t%0,%1,%z2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,none")])

(define_insn "*pred_madc<mode>_extended_scalar"
  [(set (match_operand:<VM> 0 "register_operand"             "=vr, &vr")
	(unspec:<VM>
	   [(plus:VI_D
	     (vec_duplicate:VI_D
	       (sign_extend:<VEL>
	         (match_operand:<VSUBEL> 2 "reg_or_0_operand" " rJ,  rJ")))
	     (match_operand:VI_D 1 "register_operand"         "  0,  vr"))
	    (match_operand:<VM> 3 "register_operand"          " vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand"       " rK,  rK")
	       (match_operand 5 "const_int_operand"           "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMADC))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vmadc.vxm\t%0,%1,%z2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,none")])

(define_expand "@pred_msbc<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(unspec:<VM>
	   [(minus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_int_operand"))
	     (match_operand:VI_D 1 "register_operand"))
	    (match_operand:<VM> 3 "register_operand")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand")
	       (match_operand 5 "const_int_operand")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMSBC))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[2],
	/* vl */operands[4],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_msbc<mode> (operands[0], operands[1],
	       boardcast_scalar, operands[3], operands[4], operands[5]));
        },
	(riscv_vector::avl_type) INTVAL (operands[5])))
    DONE;
})

(define_insn "*pred_msbc<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(minus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_0_operand" " rJ,  rJ"))
	     (match_operand:VI_D 1 "register_operand"    "  0,  vr"))
	    (match_operand:<VM> 3 "register_operand"     " vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand"  " rK,  rK")
	       (match_operand 5 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMSBC))]
  "TARGET_VECTOR"
  "vmsbc.vxm\t%0,%1,%z2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,none")])

(define_insn "*pred_msbc<mode>_extended_scalar"
  [(set (match_operand:<VM> 0 "register_operand"              "=vr, &vr")
	(unspec:<VM>
	   [(minus:VI_D
	     (vec_duplicate:VI_D
	       (sign_extend:<VEL>
	         (match_operand:<VSUBEL> 2 "reg_or_0_operand" " rJ,  rJ")))
	     (match_operand:VI_D 1 "register_operand"         "  0,  vr"))
	    (match_operand:<VM> 3 "register_operand"          " vm,  vm")
	    (unspec:<VM>
	      [(match_operand 4 "vector_length_operand"       " rK,  rK")
	       (match_operand 5 "const_int_operand"           "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_VMSBC))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vmsbc.vxm\t%0,%1,%z2,%3"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))
   (set_attr "spec_restriction" "thv,none")])

(define_insn "@pred_madc<mode>_overflow"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr, &vr")
	(unspec:<VM>
	   [(plus:VI
	     (match_operand:VI 1 "register_operand"     "  %0,  vr,  vr")
	     (match_operand:VI 2 "vector_arith_operand" "vrvi,  vr,  vi"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand" "  rK,  rK,  rK")
	       (match_operand 4 "const_int_operand"     "   i,   i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
  "vmadc.v%o2\t%0,%1,%v2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,none,none")])

(define_insn "@pred_msbc<mode>_overflow"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, vr, &vr, &vr")
	(unspec:<VM>
	   [(minus:VI
	     (match_operand:VI 1 "register_operand"     "   0,  vr,  vr,  vr")
	     (match_operand:VI 2 "register_operand"     "  vr,   0,  vr,  vi"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand" "  rK,  rK,  rK,  rK")
	       (match_operand 4 "const_int_operand"     "   i,   i,   i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
  "vmsbc.vv\t%0,%1,%2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,thv,none,none")])

(define_insn "@pred_madc<mode>_overflow_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(plus:VI_QHS
	     (vec_duplicate:VI_QHS
	       (match_operand:<VEL> 2 "reg_or_0_operand" " rJ,  rJ"))
	     (match_operand:VI_QHS 1 "register_operand"  "  0,  vr"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand"  " rK,  rK")
	       (match_operand 4 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
  "vmadc.vx\t%0,%1,%z2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,none")])

(define_insn "@pred_msbc<mode>_overflow_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(minus:VI_QHS
	     (vec_duplicate:VI_QHS
	       (match_operand:<VEL> 2 "reg_or_0_operand" " rJ,  rJ"))
	     (match_operand:VI_QHS 1 "register_operand"  "  0,  vr"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand"  " rK,  rK")
	       (match_operand 4 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
  "vmsbc.vx\t%0,%1,%z2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,none")])

(define_expand "@pred_madc<mode>_overflow_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(unspec:<VM>
	   [(plus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_int_operand"))
	     (match_operand:VI_D 1 "register_operand"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand")
	       (match_operand 4 "const_int_operand")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[2],
	/* vl */operands[3],
	<MODE>mode,
	riscv_vector::simm5_p (operands[2]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_madc<mode>_overflow (operands[0], operands[1],
	       boardcast_scalar, operands[3], operands[4]));
        },
	(riscv_vector::avl_type) INTVAL (operands[4])))
    DONE;
})

(define_insn "*pred_madc<mode>_overflow_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(plus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_0_operand" " rJ,  rJ"))
	     (match_operand:VI_D 1 "register_operand"    "  0,  vr"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand"  " rK,  rK")
	       (match_operand 4 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
  "vmadc.vx\t%0,%1,%z2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,none")])

(define_insn "*pred_madc<mode>_overflow_extended_scalar"
  [(set (match_operand:<VM> 0 "register_operand"             "=vr, &vr")
	(unspec:<VM>
	   [(plus:VI_D
	     (vec_duplicate:VI_D
	       (sign_extend:<VEL>
	         (match_operand:<VSUBEL> 2 "reg_or_0_operand" " rJ,  rJ")))
	     (match_operand:VI_D 1 "register_operand"         "  0,  vr"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand"       " rK,  rK")
	       (match_operand 4 "const_int_operand"           "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vmadc.vx\t%0,%1,%z2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,none")])

(define_expand "@pred_msbc<mode>_overflow_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(unspec:<VM>
	   [(minus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_int_operand"))
	     (match_operand:VI_D 1 "register_operand"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand")
	       (match_operand 4 "const_int_operand")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[2],
	/* vl */operands[3],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_msbc<mode>_overflow (operands[0], operands[1],
	       boardcast_scalar, operands[3], operands[4]));
        },
	(riscv_vector::avl_type) INTVAL (operands[4])))
    DONE;
})

(define_insn "*pred_msbc<mode>_overflow_scalar"
  [(set (match_operand:<VM> 0 "register_operand"         "=vr, &vr")
	(unspec:<VM>
	   [(minus:VI_D
	     (vec_duplicate:VI_D
	       (match_operand:<VEL> 2 "reg_or_0_operand" " rJ,  rJ"))
	     (match_operand:VI_D 1 "register_operand"    "  0,  vr"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand"  " rK,  rK")
	       (match_operand 4 "const_int_operand"      "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR"
  "vmsbc.vx\t%0,%1,%z2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,none")])

(define_insn "*pred_msbc<mode>_overflow_extended_scalar"
  [(set (match_operand:<VM> 0 "register_operand"             "=vr, &vr")
	(unspec:<VM>
	   [(minus:VI_D
	     (vec_duplicate:VI_D
	       (sign_extend:<VEL>
	         (match_operand:<VSUBEL> 2 "reg_or_0_operand" " rJ,  rJ")))
	     (match_operand:VI_D 1 "register_operand"         "  0,  vr"))
	    (unspec:<VM>
	      [(match_operand 3 "vector_length_operand"      " rK,  rK")
	       (match_operand 4 "const_int_operand"          "  i,   i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)] UNSPEC_OVERFLOW))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vmsbc.vx\t%0,%1,%z2"
  [(set_attr "type" "vicalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "3")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "spec_restriction" "thv,none")])

;; -------------------------------------------------------------------------------
;; ---- Predicated integer unary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - vneg.v/vnot.v
;; -------------------------------------------------------------------------------

(define_insn "@pred_<optab><mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"          "=vd,vd, vr, vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vm,vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"    "rK,rK, rK, rK")
	     (match_operand 5 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 6 "const_int_operand"        " i, i,  i,  i")
	     (match_operand 7 "const_int_operand"        " i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_int_unop:V_VLSI
	    (match_operand:V_VLSI 3 "register_operand"       "vr,vr, vr, vr"))
	  (match_operand:V_VLSI 2 "vector_merge_operand"     "vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.v\t%0,%3%p1"
  [(set_attr "type" "vialu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[5])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; -------------------------------------------------------------------------------
;; ---- Predicated integer widening binary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 11.2 Vector Widening Integer Add/Subtract
;; - 11.3 Vector Integer Extension
;; - 11.12 Vector Widening Integer Multiply Instructions
;; -------------------------------------------------------------------------------

;; Vector Double-Widening Sign-extend and Zero-extend.
(define_insn "@pred_<optab><mode>_vf2"
  [(set (match_operand:VWEXTI 0 "register_operand"            "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"         "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"            "   rK,   rK")
	     (match_operand 5 "const_int_operand"                "    i,    i")
	     (match_operand 6 "const_int_operand"                "    i,    i")
	     (match_operand 7 "const_int_operand"                "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_extend:VWEXTI
	    (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	  (match_operand:VWEXTI 2 "vector_merge_operand"         "   vu,    0")))]
  "TARGET_VECTOR"
  "v<sz>ext.vf2\t%0,%3%p1"
  [(set_attr "type" "vext")
   (set_attr "mode" "<MODE>")])

;; Vector Quad-Widening Sign-extend and Zero-extend.
(define_insn "@pred_<optab><mode>_vf4"
  [(set (match_operand:VQEXTI 0 "register_operand"          "=&vr,&vr")
	(if_then_else:VQEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"          "   rK,   rK")
	     (match_operand 5 "const_int_operand"              "    i,    i")
	     (match_operand 6 "const_int_operand"              "    i,    i")
	     (match_operand 7 "const_int_operand"              "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_extend:VQEXTI
	    (match_operand:<V_QUAD_TRUNC> 3 "register_operand" "   vr,   vr"))
	  (match_operand:VQEXTI 2 "vector_merge_operand"       "   vu,    0")))]
  "TARGET_VECTOR"
  "v<sz>ext.vf4\t%0,%3%p1"
  [(set_attr "type" "vext")
   (set_attr "mode" "<MODE>")])

;; Vector Oct-Widening Sign-extend and Zero-extend.
(define_insn "@pred_<optab><mode>_vf8"
  [(set (match_operand:VOEXTI 0 "register_operand"         "=&vr,&vr")
	(if_then_else:VOEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"         "   rK,   rK")
	     (match_operand 5 "const_int_operand"             "    i,    i")
	     (match_operand 6 "const_int_operand"             "    i,    i")
	     (match_operand 7 "const_int_operand"             "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_extend:VOEXTI
	    (match_operand:<V_OCT_TRUNC> 3 "register_operand" "   vr,   vr"))
	  (match_operand:VOEXTI 2 "vector_merge_operand"      "   vu,    0")))]
  "TARGET_VECTOR"
  "v<sz>ext.vf8\t%0,%3%p1"
  [(set_attr "type" "vext")
   (set_attr "mode" "<MODE>")])

;; Vector Widening Add/Subtract/Multiply.
(define_insn "@pred_dual_widen_<any_widen_binop:optab><any_extend:su><mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"                  "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_widen_binop:VWEXTI
	    (any_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	    (any_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr")))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vw<any_widen_binop:insn><any_extend:u>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vi<widen_binop_insn_type>")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_dual_widen_<any_widen_binop:optab><any_extend:su><mode>_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                  "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_widen_binop:VWEXTI
	    (any_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	    (any_extend:VWEXTI
	      (vec_duplicate:<V_DOUBLE_TRUNC>
		(match_operand:<VSUBEL> 4 "reg_or_0_operand"       "   rJ,   rJ"))))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vw<any_widen_binop:insn><any_extend:u>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vi<widen_binop_insn_type>")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_single_widen_sub<any_extend:su><mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"                  "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:VWEXTI
	    (match_operand:VWEXTI 3 "register_operand"             "   vr,   vr")
	    (any_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr")))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vwsub<any_extend:u>.wv\t%0,%3,%4%p1"
  [(set_attr "type" "viwalu")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_single_widen_add<any_extend:su><mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"                  "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (any_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr"))
	    (match_operand:VWEXTI 3 "register_operand"             "   vr,   vr"))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vwadd<any_extend:u>.wv\t%0,%3,%4%p1"
  [(set_attr "type" "viwalu")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_single_widen_<plus_minus:optab><any_extend:su><mode>_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                 "=vd,vd, vr, vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	   " vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"              " rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"                  "  i, i,  i,  i")
	     (match_operand 7 "const_int_operand"                  "  i, i,  i,  i")
	     (match_operand 8 "const_int_operand"                  "  i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:VWEXTI
	    (match_operand:VWEXTI 3 "register_operand"             " vr,vr, vr, vr")
	    (any_extend:VWEXTI
	      (vec_duplicate:<V_DOUBLE_TRUNC>
		(match_operand:<VSUBEL> 4 "reg_or_0_operand"       " rJ,rJ, rJ, rJ"))))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           " vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vw<plus_minus:insn><any_extend:u>.wx\t%0,%3,%z4%p1"
  [(set_attr "type" "vi<widen_binop_insn_type>")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_single_widen_add<any_extend:su><mode>_extended_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                 "=vd,vd, vr, vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	   " vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"              " rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"                  "  i, i,  i,  i")
	     (match_operand 7 "const_int_operand"                  "  i, i,  i,  i")
	     (match_operand 8 "const_int_operand"                  "  i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (vec_duplicate:VWEXTI
	      (any_extend:<VEL>
		(match_operand:<VSUBEL> 4 "reg_or_0_operand"       " rJ,rJ, rJ, rJ")))
	    (match_operand:VWEXTI 3 "register_operand"             " vr,vr, vr, vr"))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           " vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vwadd<any_extend:u>.wx\t%0,%3,%z4%p1"
  [(set_attr "type" "viwalu")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_single_widen_sub<any_extend:su><mode>_extended_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                 "=vd,vd, vr, vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	   " vm,vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"              " rK,rK, rK, rK")
	     (match_operand 6 "const_int_operand"                  "  i, i,  i,  i")
	     (match_operand 7 "const_int_operand"                  "  i, i,  i,  i")
	     (match_operand 8 "const_int_operand"                  "  i, i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:VWEXTI
	    (match_operand:VWEXTI 3 "register_operand"             " vr,vr, vr, vr")
	    (vec_duplicate:VWEXTI
	      (any_extend:<VEL>
		(match_operand:<VSUBEL> 4 "reg_or_0_operand"       " rJ,rJ, rJ, rJ"))))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           " vu, 0, vu,  0")))]
  "TARGET_VECTOR"
  "vwsub<any_extend:u>.wx\t%0,%3,%z4%p1"
  [(set_attr "type" "viwalu")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_widen_mulsu<mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"                  "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (mult:VWEXTI
	    (sign_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	    (zero_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr")))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vwmulsu.vv\t%0,%3,%4%p1"
  [(set_attr "type" "viwmul")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_widen_mulsu<mode>_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                  "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (mult:VWEXTI
	    (sign_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	    (zero_extend:VWEXTI
	      (vec_duplicate:<V_DOUBLE_TRUNC>
		(match_operand:<VSUBEL> 4 "reg_or_0_operand"       "   rJ,   rJ"))))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vwmulsu.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "viwmul")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

;; vwcvt<u>.x.x.v
(define_insn "@pred_<optab><mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"                  "=&vr,&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"              "   rK,   rK")
	     (match_operand 5 "const_int_operand"                  "    i,    i")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (any_extend:VWEXTI
	      (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	    (vec_duplicate:VWEXTI
	      (reg:<VEL> X0_REGNUM)))
	  (match_operand:VWEXTI 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vwcvt<u>.x.x.v\t%0,%3%p1"
  [(set_attr "type" "viwalu")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set_attr "vl_op_idx" "4")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[5])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; -------------------------------------------------------------------------------
;; ---- Predicated integer Narrowing operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 11.7 Vector Narrowing Integer Right Shift Instructions
;; -------------------------------------------------------------------------------

;; The destination EEW is smaller than the source EEW and the overlap is in the
;; lowest-numbered part of the source register group
;; e.g, when LMUL = 1, vnsrl.wi v0,v0,3 is legal but a destination of v1 is not.
(define_insn "@pred_narrow_<optab><mode>"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand"           "=vd,vd, vr, vr,vd, vr,  &vr,  &vr, vd, vr,  &vr,  &vr")
	(if_then_else:<V_DOUBLE_TRUNC>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               " vm,vm,Wc1,Wc1,vm,Wc1,vmWc1,vmWc1, vm,Wc1,vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                  " rK,rK, rK, rK,rK, rK,   rK,   rK, rK, rK,   rK,   rK")
	     (match_operand 6 "const_int_operand"                      "  i, i,  i,  i, i,  i,    i,    i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                      "  i, i,  i,  i, i,  i,    i,    i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                      "  i, i,  i,  i, i,  i,    i,    i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (truncate:<V_DOUBLE_TRUNC>
	    (any_shiftrt:VWEXTI
	     (match_operand:VWEXTI 3 "register_operand"                " vr,vr, vr, vr, 0,  0,   vr,   vr,  0,  0,   vr,   vr")
	     (match_operand:<V_DOUBLE_TRUNC> 4 "vector_shift_operand"  "  0, 0,  0,  0,vr, vr,   vr,   vr, vk, vk,   vk,   vk")))
	  (match_operand:<V_DOUBLE_TRUNC> 2 "vector_merge_operand"     "  0,vu,  0, vu,vu, vu,   vu,    0, vu, vu,   vu,    0")))]
  "TARGET_VECTOR"
  "vn<insn>.w%o4\t%0,%3,%v4%p1"
  [(set_attr "type" "vnshift")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set_attr "spec_restriction" "none,none,thv,thv,none,thv,none,none,none,thv,none,none")])

(define_insn "@pred_narrow_<optab><mode>_scalar"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand"           "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<V_DOUBLE_TRUNC>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                  " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 6 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (truncate:<V_DOUBLE_TRUNC>
	    (any_shiftrt:VWEXTI
	     (match_operand:VWEXTI 3 "register_operand"                "  0,  0,  0,  0,   vr,   vr")
	     (match_operand 4 "pmode_reg_or_uimm5_operand"             " rK, rK, rK, rK,   rK,   rK")))
	  (match_operand:<V_DOUBLE_TRUNC> 2 "vector_merge_operand"     " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vn<insn>.w%o4\t%0,%3,%4%p1"
  [(set_attr "type" "vnshift")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set_attr "spec_restriction" "none,none,thv,thv,none,none")])

;; vncvt.x.x.w
(define_insn "@pred_trunc<mode>"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand"           "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<V_DOUBLE_TRUNC>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"                  " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 5 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (match_operand 6 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (truncate:<V_DOUBLE_TRUNC>
	    (match_operand:VWEXTI 3 "register_operand"                 "  0,  0,  0,  0,   vr,   vr"))
	  (match_operand:<V_DOUBLE_TRUNC> 2 "vector_merge_operand"     " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vncvt.x.x.w\t%0,%3%p1"
  [(set_attr "type" "vnshift")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set_attr "vl_op_idx" "4")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[5])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))
   (set_attr "spec_restriction" "none,none,thv,thv,none,none")])

;; -------------------------------------------------------------------------------
;; ---- Predicated fixed-point operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 12.1 Vector Single-Width Saturating Add and Subtract
;; - 12.2 Vector Single-Width Aaveraging Add and Subtract
;; - 12.3 Vector Single-Width Fractional Multiply with Rounding and Saturation
;; - 12.4 Vector Single-Width Scaling Shift Instructions
;; - 12.5 Vector Narrowing Fixed-Point Clip Instructions
;; -------------------------------------------------------------------------------

;; Saturating Add and Subtract
(define_insn "@pred_<optab><mode>"
  [(set (match_operand:VI 0 "register_operand"           "=vd, vd, vr, vr, vd, vd, vr, vr")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1, vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK, rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i,  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i,  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i,  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_sat_int_binop:VI
	    (match_operand:VI 3 "<binop_rhs1_predicate>" " vr, vr, vr, vr, vr, vr, vr, vr")
	    (match_operand:VI 4 "<binop_rhs2_predicate>" "<binop_rhs2_constraint>"))
	  (match_operand:VI 2 "vector_merge_operand"     " vu,  0, vu,  0, vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "@
   v<insn>.vv\t%0,%3,%4%p1
   v<insn>.vv\t%0,%3,%4%p1
   v<insn>.vv\t%0,%3,%4%p1
   v<insn>.vv\t%0,%3,%4%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1
   v<binop_vi_variant_insn>\t%0,<binop_vi_variant_op>%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

;; Handle GET_MODE_INNER (mode) = QImode, HImode, SImode.
(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:VI_QHS 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:VI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_plus_binop:VI_QHS
	    (vec_duplicate:VI_QHS
	      (match_operand:<VEL> 4 "register_operand"  "  r,  r,  r,  r"))
	    (match_operand:VI_QHS 3 "register_operand"   " vr, vr, vr, vr"))
	  (match_operand:VI_QHS 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:VI_QHS 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:VI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_minus_binop:VI_QHS
	    (match_operand:VI_QHS 3 "register_operand"   " vr, vr, vr, vr")
	    (vec_duplicate:VI_QHS
	      (match_operand:<VEL> 4 "register_operand"  "  r,  r,  r,  r")))
	  (match_operand:VI_QHS 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_<optab><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_plus_binop:VI_D
	    (vec_duplicate:VI_D
	      (match_operand:<VEL> 4 "reg_or_int_operand"))
	    (match_operand:VI_D 3 "register_operand"))
	  (match_operand:VI_D 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	riscv_vector::has_vi_variant_p (<CODE>, operands[4]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_<optab><mode> (operands[0], operands[1],
	       operands[2], operands[3], boardcast_scalar, operands[5],
	       operands[6], operands[7], operands[8]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_<optab><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand"         "=vd, vd, vr, vr")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_plus_binop:VI_D
	    (vec_duplicate:VI_D
	      (match_operand:<VEL> 4 "register_operand"  "  r,  r,  r,  r"))
	    (match_operand:VI_D 3 "register_operand"     " vr, vr, vr, vr"))
	  (match_operand:VI_D 2 "vector_merge_operand"   " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_<optab><mode>_extended_scalar"
  [(set (match_operand:VI_D 0 "register_operand"             "=vd, vd, vr, vr")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"        " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"            "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_plus_binop:VI_D
	    (vec_duplicate:VI_D
	      (sign_extend:<VEL>
	        (match_operand:<VSUBEL> 4 "register_operand" "  r,  r,  r,  r")))
	    (match_operand:VI_D 3 "register_operand"         " vr, vr, vr, vr"))
	  (match_operand:VI_D 2 "vector_merge_operand"       " vu,  0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "v<insn>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_<optab><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_minus_binop:VI_D
	    (match_operand:VI_D 3 "register_operand")
	    (vec_duplicate:VI_D
	      (match_operand:<VEL> 4 "reg_or_int_operand")))
	  (match_operand:VI_D 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	riscv_vector::has_vi_variant_p (<CODE>, operands[4]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_<optab><mode> (operands[0], operands[1],
	       operands[2], operands[3], boardcast_scalar, operands[5],
	       operands[6], operands[7], operands[8]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_<optab><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand"         "=vd, vd, vr, vr")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_minus_binop:VI_D
	    (match_operand:VI_D 3 "register_operand"     " vr, vr, vr, vr")
	    (vec_duplicate:VI_D
	      (match_operand:<VEL> 4 "register_operand"  "  r,  r,  r,  r")))
	  (match_operand:VI_D 2 "vector_merge_operand"   " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<insn>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_<optab><mode>_extended_scalar"
  [(set (match_operand:VI_D 0 "register_operand"             "=vd, vd, vr, vr")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"        " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"            "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (sat_int_minus_binop:VI_D
	    (match_operand:VI_D 3 "register_operand"         " vr, vr, vr, vr")
	    (vec_duplicate:VI_D
	      (sign_extend:<VEL>
	        (match_operand:<VSUBEL> 4 "register_operand" "  r,  r,  r,  r"))))
	  (match_operand:VI_D 2 "vector_merge_operand"       " vu,  0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "v<insn>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "<int_binop_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<sat_op><mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V_VLSI
	    [(match_operand:V_VLSI 3 "register_operand"   " vr, vr, vr, vr")
	     (match_operand:V_VLSI 4 "register_operand"   " vr, vr, vr, vr")] VSAT_OP)
	  (match_operand:V_VLSI 2 "vector_merge_operand"  " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<sat_op>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "<sat_insn_type>")
   (set_attr "mode" "<MODE>")])

;; Handle GET_MODE_INNER (mode) = QImode, HImode, SImode.
(define_insn "@pred_<sat_op><mode>_scalar"
  [(set (match_operand:VI_QHS 0 "register_operand"       "=vd, vr, vd, vr")
	(if_then_else:VI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_QHS
	    [(match_operand:VI_QHS 3 "register_operand"  " vr, vr, vr, vr")
	     (match_operand:<VEL> 4 "reg_or_0_operand"   " rJ, rJ, rJ, rJ")] VSAT_ARITH_OP)
	  (match_operand:VI_QHS 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<sat_op>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<sat_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<sat_op><mode>_scalar"
  [(set (match_operand:VI 0 "register_operand"             "=vd, vr, vd, vr")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"   " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"      " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"          "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"          "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"          "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"          "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI
	    [(match_operand:VI 3 "register_operand"        " vr, vr, vr, vr")
	     (match_operand 4 "pmode_reg_or_uimm5_operand" " rK, rK, rK, rK")] VSAT_SHIFT_OP)
	  (match_operand:VI 2 "vector_merge_operand"       " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<sat_op>.v%o4\t%0,%3,%4%p1"
  [(set_attr "type" "<sat_insn_type>")
   (set_attr "mode" "<MODE>")])

;; Handle GET_MODE_INNER (mode) = DImode. We need to split them since
;; we need to deal with SEW = 64 in RV32 system.
(define_expand "@pred_<sat_op><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 5 "vector_length_operand")
	     (match_operand 6 "const_int_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	    [(match_operand:VI_D 3 "register_operand")
	     (match_operand:<VEL> 4 "reg_or_int_operand")] VSAT_ARITH_OP)
	  (match_operand:VI_D 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[4],
	/* vl */operands[5],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_<sat_op><mode> (operands[0], operands[1],
	       operands[2], operands[3], boardcast_scalar, operands[5],
	       operands[6], operands[7], operands[8], operands[9]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_<sat_op><mode>_scalar"
  [(set (match_operand:VI_D 0 "register_operand"         "=vd, vr, vd, vr")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	    [(match_operand:VI_D 3 "register_operand"    " vr, vr, vr, vr")
	     (match_operand:<VEL> 4 "reg_or_0_operand"   " rJ, rJ, rJ, rJ")] VSAT_ARITH_OP)
	  (match_operand:VI_D 2 "vector_merge_operand"   " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<sat_op>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<sat_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_<sat_op><mode>_extended_scalar"
  [(set (match_operand:VI_D 0 "register_operand"            "=vd, vr, vd, vr")
	(if_then_else:VI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"    " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"       " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"           "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"           "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"           "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"           "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI_D
	    [(match_operand:VI_D 3 "register_operand"       " vr, vr, vr, vr")
	     (sign_extend:<VEL>
	       (match_operand:<VSUBEL> 4 "reg_or_0_operand" " rJ, rJ, rJ, rJ"))] VSAT_ARITH_OP)
	  (match_operand:VI_D 2 "vector_merge_operand"      " vu,  0, vu,  0")))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "v<sat_op>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "<sat_insn_type>")
   (set_attr "mode" "<MODE>")])

;; CLIP
(define_insn "@pred_narrow_clip<v_su><mode>"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand"           "=vd,vd, vr, vr,vd, vr,  &vr,  &vr, vd, vr,  &vr,  &vr")
	(if_then_else:<V_DOUBLE_TRUNC>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               " vm,vm,Wc1,Wc1,vm,Wc1,vmWc1,vmWc1, vm,Wc1,vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                  " rK,rK, rK, rK,rK, rK,   rK,   rK, rK, rK,   rK,   rK")
	     (match_operand 6 "const_int_operand"                      "  i, i,  i,  i, i,  i,    i,    i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                      "  i, i,  i,  i, i,  i,    i,    i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                      "  i, i,  i,  i, i,  i,    i,    i,  i,  i,    i,    i")
	     (match_operand 9 "const_int_operand"                      "  i, i,  i,  i, i,  i,    i,    i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<V_DOUBLE_TRUNC>
	    [(match_operand:VWEXTI 3 "register_operand"                " vr,vr, vr, vr, 0,  0,   vr,   vr,  0,  0,   vr,   vr")
	     (match_operand:<V_DOUBLE_TRUNC> 4 "vector_shift_operand"  "  0, 0,  0,  0,vr, vr,   vr,   vr, vk, vk,   vk,   vk")] VNCLIP)
	  (match_operand:<V_DOUBLE_TRUNC> 2 "vector_merge_operand"     "  0,vu,  0, vu,vu, vu,   vu,    0, vu, vu,   vu,    0")))]
  "TARGET_VECTOR"
  "vnclip<v_su>.w%o4\t%0,%3,%v4%p1"
  [(set_attr "type" "vnclip")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set_attr "spec_restriction" "thv,thv,thv,thv,thv,thv,none,none,thv,thv,none,none")])

(define_insn "@pred_narrow_clip<v_su><mode>_scalar"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand"           "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<V_DOUBLE_TRUNC>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"                  " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 6 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (match_operand 9 "const_int_operand"                      "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI VXRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<V_DOUBLE_TRUNC>
	    [(match_operand:VWEXTI 3 "register_operand"                "  0,  0,  0,  0,   vr,   vr")
	     (match_operand 4 "pmode_reg_or_uimm5_operand"             " rK, rK, rK, rK,   rK,   rK")] VNCLIP)
	  (match_operand:<V_DOUBLE_TRUNC> 2 "vector_merge_operand"     " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vnclip<v_su>.w%o4\t%0,%3,%4%p1"
  [(set_attr "type" "vnclip")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set_attr "spec_restriction" "thv,thv,thv,thv,none,none")])

;; -------------------------------------------------------------------------------
;; ---- Predicated integer comparison operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 11.8 Vector Integer Comparision Instructions
;; -------------------------------------------------------------------------------

(define_expand "@pred_cmp<mode>"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_ltge_operator"
	     [(match_operand:V_VLSI 4 "register_operand")
	      (match_operand:V_VLSI 5 "vector_arith_operand")])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

(define_insn "*pred_cmp<mode>_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"              "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"        "   0")
	     (match_operand 5 "vector_length_operand"        "  rK")
	     (match_operand 6 "const_int_operand"            "   i")
	     (match_operand 7 "const_int_operand"            "   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "comparison_except_ltge_operator"
	     [(match_operand:V_VLSI 3 "register_operand"         "  vr")
	      (match_operand:V_VLSI 4 "vector_arith_operand"     "vrvi")])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vms%B2.v%o4\t%0,%3,%v4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_cmp<mode>"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   vr,   vr,   &vr,   &vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_ltge_operator"
	     [(match_operand:V_VLSI 4 "register_operand"          "   vr,   vr,   vr,   vr,   vr,   vr,   vr,   vr")
	      (match_operand:V_VLSI 5 "vector_arith_operand"      "   vr,   vr,   vi,   vi,   vr,   vr,   vi,   vi")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,   vu,    0,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vms%B3.v%o5\t%0,%4,%v5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,thv,thv,rvv,rvv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_cmp<mode>_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,   vr,   vr,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "    0,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_ltge_operator"
	     [(match_operand:V_VLSI 4 "register_operand"          "   vr,    0,   vr,    0,    0,   vr,    0,   vr,   vr")
	      (match_operand:V_VLSI 5 "vector_arith_operand"      " vrvi, vrvi,    0,    0, vrvi,    0,    0, vrvi, vrvi")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,   vu,   vu,   vu,    0,    0,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vms%B3.v%o5\t%0,%4,%v5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,thv,thv,thv,thv,none,none")])

(define_expand "@pred_ltge<mode>"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "ltge_operator"
	     [(match_operand:V_VLSI 4 "register_operand")
	      (match_operand:V_VLSI 5 "vector_neg_arith_operand")])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

(define_insn "*pred_ltge<mode>_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"              "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"        "   0")
	     (match_operand 5 "vector_length_operand"        "  rK")
	     (match_operand 6 "const_int_operand"            "   i")
	     (match_operand 7 "const_int_operand"            "   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "ltge_operator"
	     [(match_operand:V_VLSI 3 "register_operand"         "  vr")
	      (match_operand:V_VLSI 4 "vector_neg_arith_operand" "vrvj")])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vms%B2.v%o4\t%0,%3,%v4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_ltge<mode>"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   vr,   vr,   &vr,   &vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "ltge_operator"
	     [(match_operand:V_VLSI 4 "register_operand"          "   vr,   vr,   vr,   vr,   vr,   vr,   vr,   vr")
	      (match_operand:V_VLSI 5 "vector_neg_arith_operand"  "   vr,   vr,   vj,   vj,   vr,   vr,   vj,   vj")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,   vu,    0,    vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vms%B3.v%o5\t%0,%4,%v5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,thv,thv,rvv,rvv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_ltge<mode>_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,   vr,   vr,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "    0,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "ltge_operator"
	     [(match_operand:V_VLSI 4 "register_operand"          "   vr,    0,   vr,    0,    0,   vr,    0,   vr,   vr")
	      (match_operand:V_VLSI 5 "vector_neg_arith_operand"  " vrvj, vrvj,    0,    0, vrvj,    0,    0, vrvj, vrvj")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,   vu,   vu,   vu,    0,    0,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vms%B3.v%o5\t%0,%4,%v5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,thv,thv,thv,thv,none,none")])

(define_expand "@pred_cmp<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_QHS 4 "register_operand")
	      (vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 5 "register_operand"))])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

(define_insn "*pred_cmp<mode>_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"               "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"          "  0")
	     (match_operand 5 "vector_length_operand"          " rK")
	     (match_operand 6 "const_int_operand"              "  i")
	     (match_operand 7 "const_int_operand"              "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_QHS 3 "register_operand"       " vr")
	      (vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 4 "register_operand"      "  r"))])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vms%B2.vx\t%0,%3,%4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_cmp<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_QHS 4 "register_operand"      "   vr,   vr,   vr,   vr")
	      (vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 5 "register_operand"     "    r,    r,    r,    r"))])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_cmp<mode>_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"             "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"   "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"      "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"          "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"          "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_QHS 4 "register_operand"   "   vr,    0,    0,   vr,   vr")
	      (vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 5 "register_operand"  "    r,    r,    r,    r,    r"))])
	  (match_operand:<VM> 2 "vector_merge_operand"     "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

(define_expand "@pred_eqne<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 5 "register_operand"))
	      (match_operand:V_VLSI_QHS 4 "register_operand")])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

(define_insn "*pred_eqne<mode>_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"           "  0")
	     (match_operand 5 "vector_length_operand"           " rK")
	     (match_operand 6 "const_int_operand"               "  i")
	     (match_operand 7 "const_int_operand"               "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "equality_operator"
	     [(vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 4 "register_operand"       "  r"))
	      (match_operand:V_VLSI_QHS 3 "register_operand"        " vr")])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vms%B2.vx\t%0,%3,%4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_eqne<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 5 "register_operand"     "    r,    r,    r,    r"))
	      (match_operand:V_VLSI_QHS 4 "register_operand"      "   vr,   vr,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_eqne<mode>_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 5 "register_operand"     "    r,    r,    r,    r,    r"))
	      (match_operand:V_VLSI_QHS 4 "register_operand"      "   vr,    0,    0,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

;; Handle GET_MODE_INNER (mode) = DImode. We need to split them since
;; we need to deal with SEW = 64 in RV32 system.
(define_expand "@pred_cmp<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_D 4 "register_operand")
	      (vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 5 "reg_or_int_operand"))])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  enum rtx_code code = GET_CODE (operands[3]);
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[5],
	/* vl */operands[6],
	<MODE>mode,
	riscv_vector::has_vi_variant_p (code, operands[5]),
	code == LT || code == LTU ?
	  [] (rtx *operands, rtx boardcast_scalar) {
	    emit_insn (gen_pred_ltge<mode> (operands[0], operands[1],
	    	operands[2], operands[3], operands[4], boardcast_scalar,
	  	operands[6], operands[7], operands[8]));
          }
	:
	  [] (rtx *operands, rtx boardcast_scalar) {
	    emit_insn (gen_pred_cmp<mode> (operands[0], operands[1],
	    	operands[2], operands[3], operands[4], boardcast_scalar,
	  	operands[6], operands[7], operands[8]));
          },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_expand "@pred_eqne<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 5 "reg_or_int_operand"))
	      (match_operand:V_VLSI_D 4 "register_operand")])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  enum rtx_code code = GET_CODE (operands[3]);
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[5],
	/* vl */operands[6],
	<MODE>mode,
	riscv_vector::has_vi_variant_p (code, operands[5]),
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_cmp<mode> (operands[0], operands[1],
	  	operands[2], operands[3], operands[4], boardcast_scalar,
		operands[6], operands[7], operands[8]));
        },
	(riscv_vector::avl_type) INTVAL (operands[8])))
    DONE;
})

(define_insn "*pred_cmp<mode>_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"           "  0")
	     (match_operand 5 "vector_length_operand"           " rK")
	     (match_operand 6 "const_int_operand"               "  i")
	     (match_operand 7 "const_int_operand"               "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_D 3 "register_operand"          " vr")
	      (vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 4 "register_operand"       "  r"))])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vms%B2.vx\t%0,%3,%4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "*pred_eqne<mode>_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"           "  0")
	     (match_operand 5 "vector_length_operand"           " rK")
	     (match_operand 6 "const_int_operand"               "  i")
	     (match_operand 7 "const_int_operand"               "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "equality_operator"
	     [(vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 4 "register_operand"       "  r"))
	      (match_operand:V_VLSI_D 3 "register_operand"          " vr")])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vms%B2.vx\t%0,%3,%4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_cmp<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_D 4 "register_operand"        "   vr,   vr,   vr,   vr")
	      (vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 5 "register_operand"     "    r,    r,    r,    r"))])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_cmp<mode>_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"             "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"   "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"      "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"          "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"          "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_D 4 "register_operand"     "   vr,    0,    0,   vr,   vr")
	      (vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 5 "register_operand"  "    r,    r,    r,    r,    r"))])
	  (match_operand:<VM> 2 "vector_merge_operand"     "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_eqne<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 5 "register_operand"     "    r,    r,    r,    r"))
	      (match_operand:V_VLSI_D 4 "register_operand"        "   vr,   vr,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_eqne<mode>_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 5 "register_operand"     "    r,    r,    r,    r,    r"))
	      (match_operand:V_VLSI_D 4 "register_operand"        "   vr,    0,    0,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

(define_insn "*pred_cmp<mode>_extended_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"               "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"          "  0")
	     (match_operand 5 "vector_length_operand"          " rK")
	     (match_operand 6 "const_int_operand"              "  i")
	     (match_operand 7 "const_int_operand"              "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_D 3 "register_operand"         " vr")
	      (vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 4 "register_operand" "  r")))])
	  (match_dup 1)))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vms%B2.vx\t%0,%3,%4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_cmp<mode>_extended_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                 "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"          "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"              "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"              "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_D 4 "register_operand"         "   vr,   vr,   vr,   vr")
	      (vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 5 "register_operand" "    r,    r,    r,    r")))])
	  (match_operand:<VM> 2 "vector_merge_operand"         "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode) && !TARGET_64BIT"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

(define_insn "*pred_cmp<mode>_extended_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                 "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"          "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"              "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"              "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "comparison_except_eqge_operator"
	     [(match_operand:V_VLSI_D 4 "register_operand"         "   vr,    0,    0,   vr,   vr")
	      (vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 5 "register_operand" "    r,    r,    r,    r,    r")))])
	  (match_operand:<VM> 2 "vector_merge_operand"         "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode) && !TARGET_64BIT"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

(define_insn "*pred_eqne<mode>_extended_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"                 "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"            "  0")
	     (match_operand 5 "vector_length_operand"            " rK")
	     (match_operand 6 "const_int_operand"                "  i")
	     (match_operand 7 "const_int_operand"                "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "equality_operator"
	     [(vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 4 "register_operand"   "  r")))
	      (match_operand:V_VLSI_D 3 "register_operand"           " vr")])
	  (match_dup 1)))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vms%B2.vx\t%0,%3,%4,v0.t"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_eqne<mode>_extended_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                 "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"          "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"              "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"              "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 5 "register_operand" "    r,    r,    r,    r")))
	      (match_operand:V_VLSI_D 4 "register_operand"         "   vr,   vr,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"         "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode) && !TARGET_64BIT"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

(define_insn "*pred_eqne<mode>_extended_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"          "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"              "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"              "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 5 "register_operand" "    r,    r,    r,    r,    r")))
	      (match_operand:V_VLSI_D 4 "register_operand"         "   vr,    0,    0,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"         "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode) && !TARGET_64BIT"
  "vms%B3.vx\t%0,%4,%5%p1"
  [(set_attr "type" "vicmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

;; GE, vmsge.vx/vmsgeu.vx
;;
;; unmasked va >= x
;;  - pseudoinstruction: vmsge{u}.vx vd, va, x
;;  - expansion: vmslt{u}.vx vd, va, x; vmnand.mm vd, vd, vd
;;
;; masked va >= x, vd != v0
;;  - pseudoinstruction: vmsge{u}.vx vd, va, x, v0.t
;;  - expansion: vmslt{u}.vx vd, va, x, v0.t; vmxor.mm vd, vd, v0
;;
;; masked va >= x, vd == v0
;;  - pseudoinstruction: vmsge{u}.vx vd, va, x, v0.t, vt
;;  - expansion: vmslt{u}.vx vt, va, x;  vmandn.mm vd, vd, vt
(define_expand "@pred_ge<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "ge_operator"
	     [(match_operand:V_VLSI 4 "register_operand")
	      (vec_duplicate:V_VLSI
	        (match_operand:<VEL> 5 "reg_or_int_operand"))])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  enum rtx_code code = GET_CODE (operands[3]);
  rtx undef = RVV_VUNDEF (<VM>mode);
  rtx tmp = gen_reg_rtx (<VM>mode);
  if (code == GEU && rtx_equal_p (operands[5], const0_rtx))
    {
      /* If vmsgeu with 0 immediate, expand it to vmset.  */
      if (satisfies_constraint_Wc1 (operands[1]))
	emit_insn (
	  gen_pred_mov (<VM>mode, operands[0], CONSTM1_RTX (<VM>mode), undef,
			CONSTM1_RTX (<VM>mode), operands[6], operands[8]));
      else
	{
	  /* If vmsgeu_mask with 0 immediate, expand it to vmor mask, maskedoff.
	   */
	  if (rtx_equal_p (operands[1], operands[2]))
	    emit_move_insn (operands[0], operands[1]);
	  else if (register_operand (operands[2], <VM>mode))
	    emit_insn (gen_pred (IOR, <VM>mode, operands[0],
				 CONSTM1_RTX (<VM>mode), undef, operands[1],
				 operands[2], operands[6], operands[8]));
	  else
	    emit_insn (gen_pred (IOR, <VM>mode, operands[0],
				 CONSTM1_RTX (<VM>mode), undef, operands[1],
				 operands[1], operands[6], operands[8]));
	}
    }
  else if (riscv_vector::neg_simm5_p (operands[5]))
    emit_insn (
      gen_pred_ltge<mode> (operands[0], operands[1], operands[2], operands[3],
			   operands[4],
			   gen_const_vec_duplicate (<MODE>mode, operands[5]),
			   operands[6], operands[7], operands[8]));
  else
    {
      if (code == GE)
	operands[3] = gen_rtx_fmt_ee (LT, <VM>mode, XEXP (operands[3], 0),
				      XEXP (operands[3], 1));
      else
	operands[3] = gen_rtx_fmt_ee (LTU, <VM>mode, XEXP (operands[3], 0),
				      XEXP (operands[3], 1));
      if (GET_MODE_BITSIZE (<VEL>mode) <= GET_MODE_BITSIZE (Pmode))
	operands[5] = force_reg (<VEL>mode, operands[5]);

      if (satisfies_constraint_Wc1 (operands[1]))
	{
	  /* unmasked va >= x
	    - pseudoinstruction: vmsge{u}.vx vd, va, x
	    - expansion: vmslt{u}.vx vd, va, x; vmnand.mm vd, vd, vd.  */
	  emit_insn (
	    gen_pred_cmp<mode>_scalar (tmp, operands[1], operands[2],
					operands[3], operands[4], operands[5],
					operands[6], operands[7], operands[8]));
	  emit_insn (gen_pred_nand<vm> (operands[0], CONSTM1_RTX (<VM>mode),
					undef, tmp, tmp, operands[6], operands[8]));
	}
      else
	{
	  if (rtx_equal_p (operands[1], operands[2]))
	    {
	      /* masked va >= x, vd == v0
		- pseudoinstruction: vmsge{u}.vx vd, va, x, v0.t, vt
		- expansion: vmslt{u}.vx vt, va, x;  vmandn.mm vd, vd, vt.  */
	      emit_insn (gen_pred_cmp<mode>_scalar (
		tmp, CONSTM1_RTX (<VM>mode), undef, operands[3], operands[4],
		operands[5], operands[6], operands[7], operands[8]));
	      emit_insn (
		gen_pred_andnot<vm> (operands[0], CONSTM1_RTX (<VM>mode), undef,
				   operands[1], tmp, operands[6], operands[8]));
	    }
	  else
	    {
	      /* masked va >= x, vd != v0
		- pseudoinstruction: vmsge{u}.vx vd, va, x, v0.t
		- expansion: vmslt{u}.vx vd, va, x, v0.t; vmxor.mm vd, vd, v0.
	      */
	      emit_insn (gen_pred_cmp<mode>_scalar (
		tmp, operands[1], operands[2], operands[3], operands[4],
		operands[5], operands[6], operands[7], operands[8]));
	      emit_insn (gen_pred (XOR, <VM>mode, operands[0],
				   CONSTM1_RTX (<VM>mode), undef, tmp,
				   operands[1], operands[6], operands[8]));
	    }
	}
    }
  DONE;
})

;; -------------------------------------------------------------------------------
;; ---- Predicated integer ternary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 11.13 Vector Single-Width Integer Multiply-Add Instructions
;; -------------------------------------------------------------------------------

(define_expand "@pred_mul_plus<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI
	    (mult:V_VLSI
	      (match_operand:V_VLSI 2 "register_operand")
	      (match_operand:V_VLSI 3 "register_operand"))
	    (match_operand:V_VLSI 4 "register_operand"))
	  (match_operand:V_VLSI 5 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  riscv_vector::prepare_ternary_operands (operands);
})

(define_insn "*pred_mul_plus<mode>_undef"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd, vd,?&vd, vr, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,  vm,Wc1,Wc1, Wc1")
	     (match_operand 6 "vector_length_operand"    " rK, rK,  rK, rK, rK,  rK")
	     (match_operand 7 "const_int_operand"        "  i,  i,   i,  i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i,  i,   i,  i,  i,   i")
	     (match_operand 9 "const_int_operand"        "  i,  i,   i,  i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI
	    (mult:V_VLSI
	      (match_operand:V_VLSI 3 "register_operand"     "  0, vr,  vr,  0, vr,  vr")
	      (match_operand:V_VLSI 4 "register_operand"     " vr, vr,  vr, vr, vr,  vr"))
	    (match_operand:V_VLSI 5 "register_operand"       " vr,  0,  vr, vr,  0,  vr"))
	  (match_operand:V_VLSI 2 "vector_undef_operand")))]
  "TARGET_VECTOR"
  "@
   vmadd.vv\t%0,%4,%5%p1
   vmacc.vv\t%0,%3,%4%p1
   vmv%m4r.v\t%0,%4\;vmacc.vv\t%0,%3,%4%p1
   vmadd.vv\t%0,%4,%5%p1
   vmacc.vv\t%0,%3,%4%p1
   vmv%m5r.v\t%0,%5\;vmacc.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_madd<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI
	    (mult:V_VLSI
	      (match_operand:V_VLSI 2 "register_operand"     "  0,  vr,  0,  vr")
	      (match_operand:V_VLSI 3 "register_operand"     " vr,  vr, vr,  vr"))
	    (match_operand:V_VLSI 4 "register_operand"       " vr,  vr, vr,  vr"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "@
   vmadd.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vmadd.vv\t%0,%3,%4%p1
   vmadd.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vmadd.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "2")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_insn "*pred_macc<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI
	    (mult:V_VLSI
	      (match_operand:V_VLSI 2 "register_operand"     " vr,  vr, vr,  vr")
	      (match_operand:V_VLSI 3 "register_operand"     " vr,  vr, vr,  vr"))
	    (match_operand:V_VLSI 4 "register_operand"       "  0,  vr,  0,  vr"))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vmacc.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4;vmacc.vv\t%0,%2,%3%p1
   vmacc.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vmacc.vv\t%0,%2,%3%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_expand "@pred_mul_plus<mode>_scalar"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand")
	(if_then_else:V_VLSI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI_QHS
	    (mult:V_VLSI_QHS
	      (vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 2 "register_operand"))
	      (match_operand:V_VLSI_QHS 3 "register_operand"))
	    (match_operand:V_VLSI_QHS 4 "register_operand"))
	  (match_operand:V_VLSI_QHS 5 "register_operand")))]
  "TARGET_VECTOR"
{})

(define_insn "*pred_madd<mode>_scalar"
  [(set (match_operand:V_VLSI 0 "register_operand"            "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"     " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"         "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI
	    (mult:V_VLSI
	      (vec_duplicate:V_VLSI
	        (match_operand:<VEL> 2 "register_operand" "  r,   r,  r,   r"))
	      (match_operand:V_VLSI 3 "register_operand"      "  0,  vr,  0,  vr"))
	    (match_operand:V_VLSI 4 "register_operand"        " vr,  vr, vr,  vr"))
	  (match_dup 3)))]
  "TARGET_VECTOR"
  "@
   vmadd.vx\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vmadd.vx\t%0,%2,%4%p1
   vmadd.vx\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vmadd.vx\t%0,%2,%4%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "3")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_insn "*pred_macc<mode>_scalar"
  [(set (match_operand:V_VLSI 0 "register_operand"            "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"     " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"         "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI
	    (mult:V_VLSI
	      (vec_duplicate:V_VLSI
	        (match_operand:<VEL> 2 "register_operand" "  r,   r,  r,   r"))
	      (match_operand:V_VLSI 3 "register_operand"      " vr,  vr, vr,  vr"))
	    (match_operand:V_VLSI 4 "register_operand"        "  0,  vr,  0,  vr"))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vmacc.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vmacc.vx\t%0,%2,%3%p1
   vmacc.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vmacc.vx\t%0,%2,%3%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_expand "@pred_mul_plus<mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI_D
	    (mult:V_VLSI_D
	      (vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 2 "reg_or_int_operand"))
	      (match_operand:V_VLSI_D 3 "register_operand"))
	    (match_operand:V_VLSI_D 4 "register_operand"))
	  (match_operand:V_VLSI_D 5 "register_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[2],
	/* vl */operands[6],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_mul_plus<mode> (operands[0], operands[1],
	       boardcast_scalar, operands[3], operands[4], operands[5],
	       operands[6], operands[7], operands[8], operands[9]));
        },
	(riscv_vector::avl_type) INTVAL (operands[9])))
    DONE;
})

(define_insn "*pred_madd<mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"               "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"          " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"              "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI_D
	    (mult:V_VLSI_D
	      (vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 2 "register_operand" "  r,   r,  r,   r")))
	      (match_operand:V_VLSI_D 3 "register_operand"         "  0,  vr,  0,  vr"))
	    (match_operand:V_VLSI_D 4 "register_operand"           " vr,  vr, vr,  vr"))
	  (match_dup 3)))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "@
   vmadd.vx\t%0,%2,%4%p1
   vmv%m2r.v\t%0,%2\;vmadd.vx\t%0,%2,%4%p1
   vmadd.vx\t%0,%2,%4%p1
   vmv%m2r.v\t%0,%2\;vmadd.vx\t%0,%2,%4%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "3")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_insn "*pred_macc<mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"               "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"          " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"              "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:V_VLSI_D
	    (mult:V_VLSI_D
	      (vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 2 "register_operand" "  r,   r,  r,   r")))
	      (match_operand:V_VLSI_D 3 "register_operand"         " vr,  vr, vr,  vr"))
	    (match_operand:V_VLSI_D 4 "register_operand"           "  0,  vr,  0,  vr"))
	  (match_dup 4)))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "@
   vmacc.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vmacc.vx\t%0,%2,%3%p1
   vmacc.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vmacc.vx\t%0,%2,%3%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_expand "@pred_minus_mul<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI
	    (match_operand:V_VLSI 4 "register_operand")
	    (mult:V_VLSI
	      (match_operand:V_VLSI 2 "register_operand")
	      (match_operand:V_VLSI 3 "register_operand")))
	  (match_operand:V_VLSI 5 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  riscv_vector::prepare_ternary_operands (operands);
})

(define_insn "*pred_minus_mul<mode>_undef"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd, vd,?&vd, vr, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,  vm,Wc1,Wc1, Wc1")
	     (match_operand 6 "vector_length_operand"    " rK, rK,  rK, rK, rK,  rK")
	     (match_operand 7 "const_int_operand"        "  i,  i,   i,  i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i,  i,   i,  i,  i,   i")
	     (match_operand 9 "const_int_operand"        "  i,  i,   i,  i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI
            (match_operand:V_VLSI 5 "register_operand"       " vr,  0,  vr, vr,  0,  vr")
	    (mult:V_VLSI
	      (match_operand:V_VLSI 3 "register_operand"     "  0, vr,  vr,  0, vr,  vr")
	      (match_operand:V_VLSI 4 "register_operand"     " vr, vr,  vr, vr, vr,  vr")))
	  (match_operand:V_VLSI 2 "vector_undef_operand")))]
  "TARGET_VECTOR"
  "@
   vnmsub.vv\t%0,%4,%5%p1
   vnmsac.vv\t%0,%3,%4%p1
   vmv%m3r.v\t%0,%3\;vnmsub.vv\t%0,%4,%5%p1
   vnmsub.vv\t%0,%4,%5%p1
   vnmsac.vv\t%0,%3,%4%p1
   vmv%m3r.v\t%0,%3\;vnmsub.vv\t%0,%4,%5%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_nmsub<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI
	    (match_operand:V_VLSI 4 "register_operand"       " vr,  vr, vr,  vr")
	    (mult:V_VLSI
	      (match_operand:V_VLSI 2 "register_operand"     "  0,  vr,  0,  vr")
	      (match_operand:V_VLSI 3 "register_operand"     " vr,  vr, vr,  vr")))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "@
   vnmsub.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vnmsub.vv\t%0,%3,%4%p1
   vnmsub.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vnmsub.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "2")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_insn "*pred_nmsac<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"        "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI
	    (match_operand:V_VLSI 4 "register_operand"       "  0,  vr,  0,  vr")
	    (mult:V_VLSI
	      (match_operand:V_VLSI 2 "register_operand"     " vr,  vr, vr,  vr")
	      (match_operand:V_VLSI 3 "register_operand"     " vr,  vr, vr,  vr")))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vnmsac.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vnmsac.vv\t%0,%2,%3%p1
   vnmsac.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vnmsac.vv\t%0,%2,%3%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_expand "@pred_minus_mul<mode>_scalar"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand")
	(if_then_else:V_VLSI_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_QHS
	    (match_operand:V_VLSI_QHS 4 "register_operand")
	    (mult:V_VLSI_QHS
	      (vec_duplicate:V_VLSI_QHS
	        (match_operand:<VEL> 2 "register_operand"))
	      (match_operand:V_VLSI_QHS 3 "register_operand")))
	  (match_operand:V_VLSI_QHS 5 "register_operand")))]
  "TARGET_VECTOR"
{})

(define_insn "*pred_nmsub<mode>_scalar"
  [(set (match_operand:V_VLSI 0 "register_operand"            "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"     " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"         "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI
	    (match_operand:V_VLSI 4 "register_operand"        " vr,  vr, vr,  vr")
	    (mult:V_VLSI
	      (vec_duplicate:V_VLSI
	        (match_operand:<VEL> 2 "register_operand" "  r,   r,  r,   r"))
	      (match_operand:V_VLSI 3 "register_operand"      "  0,  vr,  0,  vr")))
	  (match_dup 3)))]
  "TARGET_VECTOR"
  "@
   vnmsub.vx\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vnmsub.vx\t%0,%2,%4%p1
   vnmsub.vx\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vnmsub.vx\t%0,%2,%4%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "3")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_insn "*pred_nmsac<mode>_scalar"
  [(set (match_operand:V_VLSI 0 "register_operand"            "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"     " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"         "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"         "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI
	    (match_operand:V_VLSI 4 "register_operand"        "  0,  vr,  0,  vr")
	    (mult:V_VLSI
	      (vec_duplicate:V_VLSI
	        (match_operand:<VEL> 2 "register_operand" "  r,   r,  r,   r"))
	      (match_operand:V_VLSI 3 "register_operand"      " vr,  vr, vr,  vr")))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vnmsac.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vnmsac.vx\t%0,%2,%3%p1
   vnmsac.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vnmsac.vx\t%0,%2,%3%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_expand "@pred_minus_mul<mode>_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_D
	    (match_operand:V_VLSI_D 4 "register_operand")
	    (mult:V_VLSI_D
	      (vec_duplicate:V_VLSI_D
	        (match_operand:<VEL> 2 "reg_or_int_operand"))
	      (match_operand:V_VLSI_D 3 "register_operand")))
	  (match_operand:V_VLSI_D 5 "register_operand")))]
  "TARGET_VECTOR"
{
  if (riscv_vector::sew64_scalar_helper (
	operands,
	/* scalar op */&operands[2],
	/* vl */operands[6],
	<MODE>mode,
	false,
	[] (rtx *operands, rtx boardcast_scalar) {
	  emit_insn (gen_pred_minus_mul<mode> (operands[0], operands[1],
	       boardcast_scalar, operands[3], operands[4], operands[5],
	       operands[6], operands[7], operands[8], operands[9]));
        },
	(riscv_vector::avl_type) INTVAL (operands[9])))
    DONE;
})

(define_insn "*pred_nmsub<mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"               "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"          " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"              "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_D
	    (match_operand:V_VLSI_D 4 "register_operand"           " vr,  vr, vr,  vr")
	    (mult:V_VLSI_D
	      (vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 2 "register_operand" "  r,   r,  r,   r")))
	      (match_operand:V_VLSI_D 3 "register_operand"         "  0,  vr,  0,  vr")))
	  (match_dup 3)))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "@
   vnmsub.vx\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vnmsub.vx\t%0,%2,%4%p1
   vnmsub.vx\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vnmsub.vx\t%0,%2,%4%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "3")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

(define_insn "*pred_nmsac<mode>_extended_scalar"
  [(set (match_operand:V_VLSI_D 0 "register_operand"               "=vd,?&vd, vr,?&vr")
	(if_then_else:V_VLSI_D
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       " vm,  vm,Wc1, Wc1")
	     (match_operand 5 "vector_length_operand"          " rK,  rK, rK,  rK")
	     (match_operand 6 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 7 "const_int_operand"              "  i,   i,  i,   i")
	     (match_operand 8 "const_int_operand"              "  i,   i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:V_VLSI_D
	    (match_operand:V_VLSI_D 4 "register_operand"           "  0,  vr,  0,  vr")
	    (mult:V_VLSI_D
	      (vec_duplicate:V_VLSI_D
	        (sign_extend:<VEL>
	          (match_operand:<VSUBEL> 2 "register_operand" "  r,   r,  r,   r")))
	      (match_operand:V_VLSI_D 3 "register_operand"         " vr,  vr, vr,  vr")))
	  (match_dup 4)))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "@
   vnmsac.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vnmsac.vx\t%0,%2,%3%p1
   vnmsac.vx\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vnmsac.vx\t%0,%2,%3%p1"
  [(set_attr "type" "vimuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))])

;; -------------------------------------------------------------------------------
;; ---- Predicated widen integer ternary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 11.14 Vector Widening Integer Multiply-Add Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_widen_mul_plus<su><mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"                    "=&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (mult:VWEXTI
	      (any_extend:VWEXTI
	        (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr"))
	      (any_extend:VWEXTI
	        (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr")))
	    (match_operand:VWEXTI 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vwmacc<u>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "viwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_widen_mul_plus<su><mode>_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                    "=&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (mult:VWEXTI
	      (any_extend:VWEXTI
	        (vec_duplicate:<V_DOUBLE_TRUNC>
	          (match_operand:<VSUBEL> 3 "register_operand"       "    r")))
	      (any_extend:VWEXTI
	        (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr")))
	    (match_operand:VWEXTI 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vwmacc<u>.vx\t%0,%3,%4%p1"
  [(set_attr "type" "viwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_widen_mul_plussu<mode>"
  [(set (match_operand:VWEXTI 0 "register_operand"                    "=&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (mult:VWEXTI
	      (sign_extend:VWEXTI
	        (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr"))
	      (zero_extend:VWEXTI
	        (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr")))
	    (match_operand:VWEXTI 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vwmaccsu.vv\t%0,%3,%4%p1"
  [(set_attr "type" "viwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_widen_mul_plussu<mode>_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                    "=&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (mult:VWEXTI
	      (sign_extend:VWEXTI
	        (vec_duplicate:<V_DOUBLE_TRUNC>
	          (match_operand:<VSUBEL> 3 "register_operand"       "    r")))
	      (zero_extend:VWEXTI
	        (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr")))
	    (match_operand:VWEXTI 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vwmaccsu.vx\t%0,%3,%4%p1"
  [(set_attr "type" "viwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

(define_insn "@pred_widen_mul_plusus<mode>_scalar"
  [(set (match_operand:VWEXTI 0 "register_operand"                    "=&vr")
	(if_then_else:VWEXTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTI
	    (mult:VWEXTI
	      (zero_extend:VWEXTI
	        (vec_duplicate:<V_DOUBLE_TRUNC>
	          (match_operand:<VSUBEL> 3 "register_operand"       "    r")))
	      (sign_extend:VWEXTI
	        (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr")))
	    (match_operand:VWEXTI 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vwmaccus.vx\t%0,%3,%4%p1"
  [(set_attr "type" "viwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated BOOL mask operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 15.1 Vector Mask-Register Logical Instructions
;; - 15.2 Vector count population in mask vcpop.m
;; - 15.3 vfirst find-first-set mask bit
;; - 15.4 vmsbf.m set-before-first mask bit
;; - 15.5 vmsif.m set-including-first mask bit
;; - 15.6 vmsof.m set-only-first mask bit
;; - 15.8 Vector Iota Instruction
;; - 15.9 Vector Element Index Instruction
;; -------------------------------------------------------------------------------

;; We keep this pattern same as pred_mov so that we can gain more optimizations.
;; For example, if we have vmxor.mm v1,v1,v1. It will be optmized as vmclr.m which
;; is generated by pred_mov.
(define_insn "@pred_<optab><mode>"
  [(set (match_operand:VB_VLS 0 "register_operand"                   "=vr")
	(if_then_else:VB_VLS
	  (unspec:VB_VLS
	    [(match_operand:VB_VLS 1 "vector_all_trues_mask_operand" "Wc1")
	     (match_operand 5 "vector_length_operand"            " rK")
	     (match_operand 6 "const_int_operand"                "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_bitwise:VB_VLS
	    (match_operand:VB_VLS 3 "register_operand"               " vr")
	    (match_operand:VB_VLS 4 "register_operand"               " vr"))
	  (match_operand:VB_VLS 2 "vector_undef_operand"             " vu")))]
  "TARGET_VECTOR"
  "vm<insn>.mm\t%0,%3,%4"
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "5")
   (set (attr "avl_type_idx") (const_int 6))])

(define_insn "@pred_n<optab><mode>"
  [(set (match_operand:VB_VLS 0 "register_operand"                   "=vr")
	(if_then_else:VB_VLS
	  (unspec:VB_VLS
	    [(match_operand:VB_VLS 1 "vector_all_trues_mask_operand" "Wc1")
	     (match_operand 5 "vector_length_operand"            " rK")
	     (match_operand 6 "const_int_operand"                "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (not:VB_VLS
	    (any_bitwise:VB_VLS
	      (match_operand:VB_VLS 3 "register_operand"             " vr")
	      (match_operand:VB_VLS 4 "register_operand"             " vr")))
	  (match_operand:VB_VLS 2 "vector_undef_operand"             " vu")))]
  "TARGET_VECTOR"
  "vm<ninsn>.mm\t%0,%3,%4"
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "5")
   (set (attr "avl_type_idx") (const_int 6))])

(define_insn "@pred_<optab>not<mode>"
  [(set (match_operand:VB_VLS 0 "register_operand"                   "=vr")
	(if_then_else:VB_VLS
	  (unspec:VB_VLS
	    [(match_operand:VB_VLS 1 "vector_all_trues_mask_operand" "Wc1")
	     (match_operand 5 "vector_length_operand"            " rK")
	     (match_operand 6 "const_int_operand"                "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (and_ior:VB_VLS
	    (match_operand:VB_VLS 3 "register_operand"               " vr")
	    (not:VB_VLS
	      (match_operand:VB_VLS 4 "register_operand"             " vr")))
	  (match_operand:VB_VLS 2 "vector_undef_operand"             " vu")))]
  "TARGET_VECTOR"
  "vm<insn>n.mm\t%0,%3,%4"
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "5")
   (set (attr "avl_type_idx") (const_int 6))])

(define_insn "@pred_not<mode>"
  [(set (match_operand:VB_VLS 0 "register_operand"                   "=vr")
	(if_then_else:VB_VLS
	  (unspec:VB_VLS
	    [(match_operand:VB_VLS 1 "vector_all_trues_mask_operand" "Wc1")
	     (match_operand 4 "vector_length_operand"            " rK")
	     (match_operand 5 "const_int_operand"                "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (not:VB_VLS
	    (match_operand:VB_VLS 3 "register_operand"               " vr"))
	  (match_operand:VB_VLS 2 "vector_undef_operand"             " vu")))]
  "TARGET_VECTOR"
  "vmnot.m\t%0,%3"
  [(set_attr "type" "vmalu")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "avl_type_idx") (const_int 5))])

(define_insn "@pred_popcount<VB:mode><P:mode>"
  [(set (match_operand:P 0 "register_operand"               "=r")
	(popcount:P
	  (unspec:VB
	    [(and:VB
	       (match_operand:VB 1 "vector_mask_operand" "vmWc1")
	       (match_operand:VB 2 "register_operand"    "   vr"))
	     (match_operand 3 "vector_length_operand"    "   rK")
	     (match_operand 4 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)))]
  "TARGET_VECTOR"
  "vcpop.m\t%0,%2%p1"
  [(set_attr "type" "vmpop")
   (set_attr "mode" "<VB:MODE>")])

(define_insn "@pred_ffs<VB:mode><P:mode>"
  [(set (match_operand:P 0 "register_operand"                 "=r")
	(plus:P
	  (ffs:P
	    (unspec:VB
	      [(and:VB
	         (match_operand:VB 1 "vector_mask_operand" "vmWc1")
	         (match_operand:VB 2 "register_operand"    "   vr"))
	       (match_operand 3 "vector_length_operand"    "   rK")
	       (match_operand 4 "const_int_operand"        "    i")
	       (reg:SI VL_REGNUM)
	       (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE))
	  (const_int -1)))]
  "TARGET_VECTOR"
  "vfirst.m\t%0,%2%p1"
  [(set_attr "type" "vmffs")
   (set_attr "mode" "<VB:MODE>")])

(define_insn "@pred_<misc_op><mode>"
  [(set (match_operand:VB 0 "register_operand"          "=&vr,  &vr")
	(if_then_else:VB
	  (unspec:VB
	    [(match_operand:VB 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"  "   rK,   rK")
	     (match_operand 5 "const_int_operand"      "    i,    i")
	     (match_operand 6 "const_int_operand"      "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VB
	    [(match_operand:VB 3 "register_operand"    "   vr,   vr")] VMISC)
	  (match_operand:VB 2 "vector_merge_operand"   "   vu,    0")))]
  "TARGET_VECTOR"
  "vm<misc_op>.m\t%0,%3%p1"
  [(set_attr "type" "vmsfs")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_iota<mode>"
  [(set (match_operand:VI 0 "register_operand"            "=&vr,  &vr")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK,   rK")
	     (match_operand 5 "const_int_operand"        "    i,    i")
	     (match_operand 6 "const_int_operand"        "    i,    i")
	     (match_operand 7 "const_int_operand"        "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VI
	    [(match_operand:<VM> 3 "register_operand"    "   vr,   vr")] UNSPEC_VIOTA)
	  (match_operand:VI 2 "vector_merge_operand"     "   vu,    0")))]
  "TARGET_VECTOR"
  "viota.m\t%0,%3%p1"
  [(set_attr "type" "vmiota")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_series<mode>"
  [(set (match_operand:V_VLSI 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:V_VLSI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 3 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 4 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 5 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_series:V_VLSI (const_int 0) (const_int 1))
	  (match_operand:V_VLSI 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vid.v\t%0%p1"
  [(set_attr "type" "vmidx")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point binary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.2 Vector Single-Width Floating-Point Add/Subtract Instructions
;; - 13.4 Vector Single-Width Floating-Point Multiply/Divide Instructions
;; - 13.11 Vector Floating-Point MIN/MAX Instructions
;; - 13.12 Vector Floating-Point Sign-Injection Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_<optab><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (any_float_binop:V_VLSF
	    (match_operand:V_VLSF 3 "register_operand"       " vr, vr, vr, vr")
	    (match_operand:V_VLSF 4 "register_operand"       " vr, vr, vr, vr"))
	  (match_operand:V_VLSF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<insn>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_<optab><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_float_binop_nofrm:V_VLSF
	    (match_operand:V_VLSF 3 "register_operand"       " vr, vr, vr, vr")
	    (match_operand:V_VLSF 4 "register_operand"       " vr, vr, vr, vr"))
	  (match_operand:V_VLSF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<insn>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<ieee_fmaxmin_op><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V_VLSF
	    [(match_operand:V_VLSF 3 "register_operand"      " vr, vr, vr, vr")
	    (match_operand:V_VLSF 4 "register_operand"       " vr, vr, vr, vr")]
	    UNSPEC_VFMAXMIN)
	  (match_operand:V_VLSF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<ieee_fmaxmin_op>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (commutative_float_binop:VF
	    (vec_duplicate:VF
	      (match_operand:<VEL> 4 "register_operand"  "  f,  f,  f,  f"))
	    (match_operand:VF 3 "register_operand"       " vr, vr, vr, vr"))
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<insn>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (commutative_float_binop_nofrm:VF
	    (vec_duplicate:VF
	      (match_operand:<VEL> 4 "register_operand"  "  f,  f,  f,  f"))
	    (match_operand:VF 3 "register_operand"       " vr, vr, vr, vr"))
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<insn>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<ieee_fmaxmin_op><mode>_scalar"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VF
	    [(match_operand:VF 3 "register_operand"        " vr, vr, vr, vr")
	      (vec_duplicate:VF
		(match_operand:<VEL> 4 "register_operand"  "  f,  f,  f,  f"))]
	      UNSPEC_VFMAXMIN)
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "v<ieee_fmaxmin_op>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vfminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<optab><mode>_scalar"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (non_commutative_float_binop:VF
	    (match_operand:VF 3 "register_operand"       " vr, vr, vr, vr")
	    (vec_duplicate:VF
	      (match_operand:<VEL> 4 "register_operand"  "  f,  f,  f,  f")))
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<insn>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_<optab><mode>_reverse_scalar"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (non_commutative_float_binop:VF
	    (vec_duplicate:VF
	      (match_operand:<VEL> 4 "register_operand"  "  f,  f,  f,  f"))
	    (match_operand:VF 3 "register_operand"       " vr, vr, vr, vr"))
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfr<insn>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_<copysign><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V_VLSF
	    [(match_operand:V_VLSF 3 "register_operand"  " vr, vr, vr, vr")
	     (match_operand:V_VLSF 4 "register_operand"  " vr, vr, vr, vr")] VCOPYSIGNS)
	  (match_operand:V_VLSF 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfsgnj<nx>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfsgnj")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_ncopysign<mode>"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (neg:VF
	    (unspec:VF
	      [(match_operand:VF 3 "register_operand"       " vr, vr, vr, vr")
	       (match_operand:VF 4 "register_operand"       " vr, vr, vr, vr")] UNSPEC_VCOPYSIGN))
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfsgnjn.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfsgnj")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<copysign><mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V_VLSF
	    [(match_operand:V_VLSF 3 "register_operand"  " vr, vr, vr, vr")
	     (vec_duplicate:V_VLSF
	       (match_operand:<VEL> 4 "register_operand" "  f,  f,  f,  f"))] VCOPYSIGNS)
	  (match_operand:V_VLSF 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfsgnj<nx>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vfsgnj")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_ncopysign<mode>_scalar"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (neg:VF
	    (unspec:VF
	      [(match_operand:VF 3 "register_operand"      " vr, vr, vr, vr")
	       (vec_duplicate:VF
		 (match_operand:<VEL> 4 "register_operand" "  f,  f,  f,  f"))] UNSPEC_VCOPYSIGN))
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfsgnjn.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vfsgnj")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point ternary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.6 Vector Single-Width Floating-Point Fused Multiply-Add Instructions
;; -------------------------------------------------------------------------------

(define_expand "@pred_mul_<optab><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (match_operand 10 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (mult:V_VLSF
	      (match_operand:V_VLSF 2 "register_operand")
	      (match_operand:V_VLSF 3 "register_operand"))
	    (match_operand:V_VLSF 4 "register_operand"))
	  (match_operand:V_VLSF 5 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  riscv_vector::prepare_ternary_operands (operands);
})

(define_insn "*pred_mul_<optab><mode>_undef"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd,vd,?&vd, vr, vr,?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,vm,  vm,Wc1,Wc1, Wc1")
	     (match_operand 6 "vector_length_operand"    " rK,rK,  rK, rK, rK,  rK")
	     (match_operand 7 "const_int_operand"        "  i, i,   i,  i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i, i,   i,  i,  i,   i")
	     (match_operand 9 "const_int_operand"        "  i, i,   i,  i,  i,   i")
	     (match_operand 10 "const_int_operand"       "  i, i,   i,  i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (mult:V_VLSF
	      (match_operand:V_VLSF 3 "register_operand"     "  0,vr,  vr,  0, vr,  vr")
	      (match_operand:V_VLSF 4 "register_operand"     " vr,vr,  vr, vr, vr,  vr"))
	    (match_operand:V_VLSF 5 "register_operand"       " vr, 0,  vr, vr,  0,  vr"))
	  (match_operand:V_VLSF 2 "vector_undef_operand")))]
  "TARGET_VECTOR"
  "@
   vf<madd_msub>.vv\t%0,%4,%5%p1
   vf<macc_msac>.vv\t%0,%3,%4%p1
   vmv%m3r.v\t%0,%3\;vf<madd_msub>.vv\t%0,%4,%5%p1
   vf<madd_msub>.vv\t%0,%4,%5%p1
   vf<macc_msac>.vv\t%0,%3,%4%p1
   vmv%m3r.v\t%0,%3\;vf<madd_msub>.vv\t%0,%4,%5%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[10])"))])

(define_insn "*pred_<madd_msub><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"        "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (mult:V_VLSF
	      (match_operand:V_VLSF 2 "register_operand"     "  0,   vr,  0,   vr")
	      (match_operand:V_VLSF 3 "register_operand"     " vr,   vr, vr,   vr"))
	    (match_operand:V_VLSF 4 "register_operand"       " vr,   vr, vr,   vr"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "@
   vf<madd_msub>.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vf<madd_msub>.vv\t%0,%3,%4%p1
   vf<madd_msub>.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vf<madd_msub>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "2")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "*pred_<macc_msac><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"        "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (mult:V_VLSF
	      (match_operand:V_VLSF 2 "register_operand"     " vr,   vr, vr,   vr")
	      (match_operand:V_VLSF 3 "register_operand"     " vr,   vr, vr,   vr"))
	    (match_operand:V_VLSF 4 "register_operand"       "  0,   vr,  0,   vr"))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vf<macc_msac>.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<macc_msac>.vv\t%0,%2,%3%p1
   vf<macc_msac>.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<macc_msac>.vv\t%0,%2,%3%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_expand "@pred_mul_<optab><mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (match_operand 10 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (mult:V_VLSF
	      (vec_duplicate:V_VLSF
	        (match_operand:<VEL> 2 "register_operand"))
	      (match_operand:V_VLSF 3 "register_operand"))
	    (match_operand:V_VLSF 4 "register_operand"))
	  (match_operand:V_VLSF 5 "register_operand")))]
  "TARGET_VECTOR"
{})

(define_insn "*pred_<madd_msub><mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand"            "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"     " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"         "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (mult:V_VLSF
	      (vec_duplicate:V_VLSF
	        (match_operand:<VEL> 2 "register_operand" "  f,  f,    f,    f"))
	      (match_operand:V_VLSF 3 "register_operand"      "  0, vr,    0,   vr"))
	    (match_operand:V_VLSF 4 "register_operand"        " vr, vr,   vr,   vr"))
	  (match_dup 3)))]
  "TARGET_VECTOR"
  "@
   vf<madd_msub>.vf\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vf<madd_msub>.vf\t%0,%2,%4%p1
   vf<madd_msub>.vf\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vf<madd_msub>.vf\t%0,%2,%4%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "3")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "*pred_<macc_msac><mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand"            "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"     " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"         "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (mult:V_VLSF
	      (vec_duplicate:V_VLSF
	        (match_operand:<VEL> 2 "register_operand" "  f,  f,    f,    f"))
	      (match_operand:V_VLSF 3 "register_operand"      " vr, vr,   vr,   vr"))
	    (match_operand:V_VLSF 4 "register_operand"        "  0, vr,    0,   vr"))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vf<macc_msac>.vf\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<macc_msac>.vf\t%0,%2,%3%p1
   vf<macc_msac>.vf\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<macc_msac>.vf\t%0,%2,%3%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_expand "@pred_mul_neg_<optab><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (match_operand 10 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (neg:V_VLSF
	      (mult:V_VLSF
	        (match_operand:V_VLSF 2 "register_operand")
	        (match_operand:V_VLSF 3 "register_operand")))
	    (match_operand:V_VLSF 4 "register_operand"))
	  (match_operand:V_VLSF 5 "vector_merge_operand")))]
  "TARGET_VECTOR"
{
  riscv_vector::prepare_ternary_operands (operands);
})

(define_insn "*pred_mul_neg_<optab><mode>_undef"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd,vd,?&vd, vr, vr,?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,vm,  vm,Wc1,Wc1, Wc1")
	     (match_operand 6 "vector_length_operand"    " rK,rK,  rK, rK, rK,  rK")
	     (match_operand 7 "const_int_operand"        "  i, i,   i,  i,  i,   i")
	     (match_operand 8 "const_int_operand"        "  i, i,   i,  i,  i,   i")
	     (match_operand 9 "const_int_operand"        "  i, i,   i,  i,  i,   i")
	     (match_operand 10 "const_int_operand"       "  i, i,   i,  i,  i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
            (neg:V_VLSF
	      (mult:V_VLSF
	        (match_operand:V_VLSF 3 "register_operand"   "  0,vr,  vr,  0, vr,  vr")
	        (match_operand:V_VLSF 4 "register_operand"   " vr,vr,  vr, vr, vr,  vr")))
	    (match_operand:V_VLSF 5 "register_operand"       " vr, 0,  vr, vr,  0,  vr"))
	  (match_operand:V_VLSF 2 "vector_undef_operand")))]
  "TARGET_VECTOR"
  "@
   vf<nmsub_nmadd>.vv\t%0,%4,%5%p1
   vf<nmsac_nmacc>.vv\t%0,%3,%4%p1
   vmv%m3r.v\t%0,%3\;vf<nmsub_nmadd>.vv\t%0,%4,%5%p1
   vf<nmsub_nmadd>.vv\t%0,%4,%5%p1
   vf<nmsac_nmacc>.vv\t%0,%3,%4%p1
   vmv%m3r.v\t%0,%3\;vf<nmsub_nmadd>.vv\t%0,%4,%5%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[10])"))])

(define_insn "*pred_<nmsub_nmadd><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"        "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (neg:V_VLSF
	      (mult:V_VLSF
	        (match_operand:V_VLSF 2 "register_operand"   "  0,   vr,  0,   vr")
	        (match_operand:V_VLSF 3 "register_operand"   " vr,   vr, vr,   vr")))
	    (match_operand:V_VLSF 4 "register_operand"       " vr,   vr, vr,   vr"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "@
   vf<nmsub_nmadd>.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vf<nmsub_nmadd>.vv\t%0,%3,%4%p1
   vf<nmsub_nmadd>.vv\t%0,%3,%4%p1
   vmv%m2r.v\t%0,%2\;vf<nmsub_nmadd>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "2")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "*pred_<nmsac_nmacc><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"    " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"        "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"        "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (neg:V_VLSF
	      (mult:V_VLSF
	        (match_operand:V_VLSF 2 "register_operand"   " vr,   vr, vr,   vr")
	        (match_operand:V_VLSF 3 "register_operand"   " vr,   vr, vr,   vr")))
	    (match_operand:V_VLSF 4 "register_operand"       "  0,   vr,  0,   vr"))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vf<nmsac_nmacc>.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<nmsac_nmacc>.vv\t%0,%2,%3%p1
   vf<nmsac_nmacc>.vv\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<nmsac_nmacc>.vv\t%0,%2,%3%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_expand "@pred_mul_neg_<optab><mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (match_operand 9 "const_int_operand")
	     (match_operand 10 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (neg:V_VLSF
	      (mult:V_VLSF
	        (vec_duplicate:V_VLSF
	          (match_operand:<VEL> 2 "register_operand"))
	        (match_operand:V_VLSF 3 "register_operand")))
	    (match_operand:V_VLSF 4 "register_operand"))
	  (match_operand:V_VLSF 5 "register_operand")))]
  "TARGET_VECTOR"
{})

(define_insn "*pred_<nmsub_nmadd><mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand"            "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"     " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"         "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"         "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (neg:V_VLSF
	      (mult:V_VLSF
	        (vec_duplicate:V_VLSF
	          (match_operand:<VEL> 2 "register_operand" "  f,    f,  f,    f"))
	        (match_operand:V_VLSF 3 "register_operand"      "  0,   vr,  0,   vr")))
	    (match_operand:V_VLSF 4 "register_operand"          " vr,   vr, vr,   vr"))
	  (match_dup 3)))]
  "TARGET_VECTOR"
  "@
   vf<nmsub_nmadd>.vf\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vf<nmsub_nmadd>.vf\t%0,%2,%4%p1
   vf<nmsub_nmadd>.vf\t%0,%2,%4%p1
   vmv%m3r.v\t%0,%3\;vf<nmsub_nmadd>.vf\t%0,%2,%4%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "3")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "*pred_<nmsac_nmacc><mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand"              "=vd, ?&vd, vr, ?&vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"    " vm,   vm,Wc1,  Wc1")
	     (match_operand 5 "vector_length_operand"       " rK,   rK, rK,   rK")
	     (match_operand 6 "const_int_operand"           "  i,    i,  i,    i")
	     (match_operand 7 "const_int_operand"           "  i,    i,  i,    i")
	     (match_operand 8 "const_int_operand"           "  i,    i,  i,    i")
	     (match_operand 9 "const_int_operand"           "  i,    i,  i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:V_VLSF
	    (neg:V_VLSF
	      (mult:V_VLSF
	        (vec_duplicate:V_VLSF
	          (match_operand:<VEL> 2 "register_operand" "  f,    f,  f,    f"))
	        (match_operand:V_VLSF 3 "register_operand"      " vr,   vr, vr,   vr")))
	    (match_operand:V_VLSF 4 "register_operand"          "  0,   vr,  0,   vr"))
	  (match_dup 4)))]
  "TARGET_VECTOR"
  "@
   vf<nmsac_nmacc>.vf\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<nmsac_nmacc>.vf\t%0,%2,%3%p1
   vf<nmsac_nmacc>.vf\t%0,%2,%3%p1
   vmv%m4r.v\t%0,%4\;vf<nmsac_nmacc>.vf\t%0,%2,%3%p1"
  [(set_attr "type" "vfmuladd")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "4")
   (set_attr "vl_op_idx" "5")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[6])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[7])"))
   (set (attr "avl_type_idx") (const_int 8))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point unary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.8 Vector Floating-Point Square-Root Instruction
;; - 13.9 Vector Floating-Point Reciprocal Square-Root Estimate Instruction
;; - 13.10 Vector Floating-Point Reciprocal Estimate Instruction
;; - 13.12 Vector Floating-Point Sign-Injection Instructions (vfneg.v/vfabs.v)
;; - 13.14 Vector Floating-Point Classify Instruction
;; -------------------------------------------------------------------------------

(define_insn "@pred_<optab><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (any_float_unop:V_VLSF
	    (match_operand:V_VLSF 3 "register_operand"       " vr, vr, vr, vr"))
	  (match_operand:V_VLSF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<insn>.v\t%0,%3%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[5])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

(define_insn "@pred_<optab><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_float_unop_nofrm:V_VLSF
	    (match_operand:V_VLSF 3 "register_operand"       " vr, vr, vr, vr"))
	  (match_operand:V_VLSF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<insn>.v\t%0,%3%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")
   (set_attr "vl_op_idx" "4")
   (set (attr "ta") (symbol_ref "riscv_vector::get_ta(operands[5])"))
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

(define_insn "@pred_<misc_op><mode>"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VF
	    [(match_operand:VF 3 "register_operand"       " vr, vr, vr, vr")] VFMISC)
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<misc_op>.v\t%0,%3%p1"
  [(set_attr "type" "<float_insn_type>")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<misc_frm_op><mode>"
  [(set (match_operand:VF 0 "register_operand"           "=vd, vd, vr, vr")
	(if_then_else:VF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"    " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VF
	    [(match_operand:VF 3 "register_operand"       " vr, vr, vr, vr")] VFMISC_FRM)
	  (match_operand:VF 2 "vector_merge_operand"     " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vf<misc_frm_op>.v\t%0,%3%p1"
  [(set_attr "type" "<float_frm_insn_type>")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

(define_insn "@pred_class<mode>"
  [(set (match_operand:<VCONVERT> 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:<VCONVERT>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"        " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"            "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<VCONVERT>
	    [(match_operand:VF 3 "register_operand"          " vr, vr, vr, vr")] UNSPEC_VFCLASS)
	  (match_operand:<VCONVERT> 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfclass.v\t%0,%3%p1"
  [(set_attr "type" "vfclass")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point widen binary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.3 Vector Widening Floating-Point Add/Subtract Instructions
;; - 13.5 Vector Widening Floating-Point Multiply
;; -------------------------------------------------------------------------------

;; Vector Widening Add/Subtract/Multiply.
(define_insn "@pred_dual_widen_<optab><mode>"
  [(set (match_operand:VWEXTF 0 "register_operand"                  "=&vr,  &vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (match_operand 9 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (any_widen_binop:VWEXTF
	    (float_extend:VWEXTF
	      (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	    (float_extend:VWEXTF
	      (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr")))
	  (match_operand:VWEXTF 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vfw<insn>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vf<widen_binop_insn_type>")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_dual_widen_<optab><mode>_scalar"
  [(set (match_operand:VWEXTF 0 "register_operand"                  "=&vr,  &vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (match_operand 9 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (any_widen_binop:VWEXTF
	    (float_extend:VWEXTF
	      (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	    (float_extend:VWEXTF
	      (vec_duplicate:<V_DOUBLE_TRUNC>
		(match_operand:<VSUBEL> 4 "register_operand"       "    f,    f"))))
	  (match_operand:VWEXTF 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vfw<insn>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vf<widen_binop_insn_type>")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_single_widen_add<mode>"
  [(set (match_operand:VWEXTF 0 "register_operand"                  "=&vr,  &vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (match_operand 9 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus:VWEXTF
	    (float_extend:VWEXTF
	      (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr"))
	    (match_operand:VWEXTF 3 "register_operand"             "   vr,   vr"))
	  (match_operand:VWEXTF 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vfwadd.wv\t%0,%3,%4%p1"
  [(set_attr "type" "vfwalu")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_single_widen_sub<mode>"
  [(set (match_operand:VWEXTF 0 "register_operand"                  "=&vr,  &vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"              "   rK,   rK")
	     (match_operand 6 "const_int_operand"                  "    i,    i")
	     (match_operand 7 "const_int_operand"                  "    i,    i")
	     (match_operand 8 "const_int_operand"                  "    i,    i")
	     (match_operand 9 "const_int_operand"                  "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (minus:VWEXTF
	    (match_operand:VWEXTF 3 "register_operand"             "   vr,   vr")
	    (float_extend:VWEXTF
	      (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr,   vr")))
	  (match_operand:VWEXTF 2 "vector_merge_operand"           "   vu,    0")))]
  "TARGET_VECTOR"
  "vfwsub.wv\t%0,%3,%4%p1"
  [(set_attr "type" "vfwalu")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_single_widen_<plus_minus:optab><mode>_scalar"
  [(set (match_operand:VWEXTF 0 "register_operand"                "=vd, vd, vr, vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"          " vm, vm,Wc1,Wc1")
	     (match_operand 5 "vector_length_operand"             " rK, rK, rK, rK")
	     (match_operand 6 "const_int_operand"                 "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"                 "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"                 "  i,  i,  i,  i")
	     (match_operand 9 "const_int_operand"                 "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:VWEXTF
	    (match_operand:VWEXTF 3 "register_operand"            " vr, vr, vr, vr")
	    (float_extend:VWEXTF
	      (vec_duplicate:<V_DOUBLE_TRUNC>
		(match_operand:<VSUBEL> 4 "register_operand"      "  f,  f,  f,  f"))))
	  (match_operand:VWEXTF 2 "vector_merge_operand"          " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfw<insn>.wf\t%0,%3,%4%p1"
  [(set_attr "type" "vf<widen_binop_insn_type>")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

;; -------------------------------------------------------------------------------
;; ---- Predicated widen floating-point ternary operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.7 Vector Widening Floating-Point Fused Multiply-Add Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_widen_mul_<optab><mode>"
  [(set (match_operand:VWEXTF 0 "register_operand"                    "=&vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (match_operand 9 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:VWEXTF
	    (mult:VWEXTF
	      (float_extend:VWEXTF
	        (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr"))
	      (float_extend:VWEXTF
	        (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr")))
	    (match_operand:VWEXTF 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vfw<macc_msac>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_widen_mul_<optab><mode>_scalar"
  [(set (match_operand:VWEXTF 0 "register_operand"                    "=&vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"             "vmWc1")
	     (match_operand 5 "vector_length_operand"                "   rK")
	     (match_operand 6 "const_int_operand"                    "    i")
	     (match_operand 7 "const_int_operand"                    "    i")
	     (match_operand 8 "const_int_operand"                    "    i")
	     (match_operand 9 "const_int_operand"                    "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:VWEXTF
	    (mult:VWEXTF
	      (float_extend:VWEXTF
	        (vec_duplicate:<V_DOUBLE_TRUNC>
	          (match_operand:<VSUBEL> 3 "register_operand"       "    f")))
	      (float_extend:VWEXTF
	        (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr")))
	    (match_operand:VWEXTF 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vfw<macc_msac>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vfwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_widen_mul_neg_<optab><mode>"
  [(set (match_operand:VWEXTF 0 "register_operand"                      "=&vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               "vmWc1")
	     (match_operand 5 "vector_length_operand"                  "   rK")
	     (match_operand 6 "const_int_operand"                      "    i")
	     (match_operand 7 "const_int_operand"                      "    i")
	     (match_operand 8 "const_int_operand"                      "    i")
	     (match_operand 9 "const_int_operand"                      "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:VWEXTF
	    (neg:VWEXTF
	      (mult:VWEXTF
	        (float_extend:VWEXTF
	          (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr"))
	        (float_extend:VWEXTF
	          (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr"))))
	      (match_operand:VWEXTF 2 "register_operand"               "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vfw<nmsac_nmacc>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

(define_insn "@pred_widen_mul_neg_<optab><mode>_scalar"
  [(set (match_operand:VWEXTF 0 "register_operand"                      "=&vr")
	(if_then_else:VWEXTF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"               "vmWc1")
	     (match_operand 5 "vector_length_operand"                  "   rK")
	     (match_operand 6 "const_int_operand"                      "    i")
	     (match_operand 7 "const_int_operand"                      "    i")
	     (match_operand 8 "const_int_operand"                      "    i")
	     (match_operand 9 "const_int_operand"                      "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (plus_minus:VWEXTF
	    (neg:VWEXTF
	      (mult:VWEXTF
	        (float_extend:VWEXTF
	          (vec_duplicate:<V_DOUBLE_TRUNC>
	            (match_operand:<VSUBEL> 3 "register_operand"       "    f")))
	        (float_extend:VWEXTF
	          (match_operand:<V_DOUBLE_TRUNC> 4 "register_operand" "   vr"))))
	    (match_operand:VWEXTF 2 "register_operand"                 "    0"))
	  (match_dup 2)))]
  "TARGET_VECTOR"
  "vfw<nmsac_nmacc>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vfwmuladd")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[9])"))])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point comparison operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.13 Vector Floating-Point Compare Instructions
;; -------------------------------------------------------------------------------

(define_expand "@pred_cmp<mode>"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "signed_order_operator"
	     [(match_operand:V_VLSF 4 "register_operand")
	      (match_operand:V_VLSF 5 "register_operand")])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_cmp<mode>"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "signed_order_operator"
	     [(match_operand:V_VLSF 4 "register_operand"      "   vr,   vr,   vr,   vr")
	      (match_operand:V_VLSF 5 "register_operand"      "   vr,   vr,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vmf%B3.vv\t%0,%4,%5%p1"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

(define_insn "*pred_cmp<mode>_narrow_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"               "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"          "  0")
	     (match_operand 5 "vector_length_operand"          " rK")
	     (match_operand 6 "const_int_operand"              "  i")
	     (match_operand 7 "const_int_operand"              "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "signed_order_operator"
	     [(match_operand:V_VLSF 3 "register_operand"           " vr")
	      (match_operand:V_VLSF 4 "register_operand"           " vr")])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vmf%B2.vv\t%0,%3,%4,v0.t"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_cmp<mode>_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,   vr,   vr,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "    0,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "signed_order_operator"
	     [(match_operand:V_VLSF 4 "register_operand"      "   vr,    0,   vr,    0,    0,   vr,    0,   vr,   vr")
	      (match_operand:V_VLSF 5 "register_operand"      "   vr,   vr,    0,    0,   vr,    0,    0,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,   vu,   vu,   vu,    0,    0,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vmf%B3.vv\t%0,%4,%5%p1"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,thv,thv,thv,thv,none,none")])

(define_expand "@pred_cmp<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "signed_order_operator"
	     [(match_operand:V_VLSF 4 "register_operand")
	      (vec_duplicate:V_VLSF
	        (match_operand:<VEL> 5 "register_operand"))])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

(define_insn "*pred_cmp<mode>_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"              "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"         "  0")
	     (match_operand 5 "vector_length_operand"         " rK")
	     (match_operand 6 "const_int_operand"             "  i")
	     (match_operand 7 "const_int_operand"             "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "signed_order_operator"
	     [(match_operand:V_VLSF 3 "register_operand"      " vr")
	      (vec_duplicate:V_VLSF
	        (match_operand:<VEL> 4 "register_operand"     "  f"))])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vmf%B2.vf\t%0,%3,%4,v0.t"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_cmp<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "signed_order_operator"
	     [(match_operand:V_VLSF 4 "register_operand"      "   vr,   vr,   vr,   vr")
	      (vec_duplicate:V_VLSF
	        (match_operand:<VEL> 5 "register_operand"     "    f,    f,    f,    f"))])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vmf%B3.vf\t%0,%4,%5%p1"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_cmp<mode>_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "signed_order_operator"
	     [(match_operand:V_VLSF 4 "register_operand"      "   vr,    0,    0,   vr,   vr")
	      (vec_duplicate:V_VLSF
	        (match_operand:<VEL> 5 "register_operand"     "    f,    f,    f,    f,    f"))])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vmf%B3.vf\t%0,%4,%5%p1"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

(define_expand "@pred_eqne<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand")
	     (match_operand 6 "vector_length_operand")
	     (match_operand 7 "const_int_operand")
	     (match_operand 8 "const_int_operand")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSF
	        (match_operand:<VEL> 5 "register_operand"))
	      (match_operand:V_VLSF 4 "register_operand")])
	  (match_operand:<VM> 2 "vector_merge_operand")))]
  "TARGET_VECTOR"
  {})

(define_insn "*pred_eqne<mode>_scalar_merge_tie_mask"
  [(set (match_operand:<VM> 0 "register_operand"              "=vm")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "register_operand"         "  0")
	     (match_operand 5 "vector_length_operand"         " rK")
	     (match_operand 6 "const_int_operand"             "  i")
	     (match_operand 7 "const_int_operand"             "  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 2 "equality_operator"
	     [(vec_duplicate:V_VLSF
	        (match_operand:<VEL> 4 "register_operand"     "  f"))
	      (match_operand:V_VLSF 3 "register_operand"      " vr")])
	  (match_dup 1)))]
  "TARGET_VECTOR"
  "vmf%B2.vf\t%0,%3,%4,v0.t"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "merge_op_idx" "1")
   (set_attr "vl_op_idx" "5")
   (set (attr "ma") (symbol_ref "riscv_vector::get_ma(operands[6])"))
   (set (attr "avl_type_idx") (const_int 7))])

;; We don't use early-clobber for LMUL <= 1 to get better codegen.
(define_insn "*pred_eqne<mode>_scalar"
  [(set (match_operand:<VM> 0 "register_operand"                "=vr,   vr,   &vr,   &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSF
	        (match_operand:<VEL> 5 "register_operand"     "    f,    f,    f,    f"))
	      (match_operand:V_VLSF 4 "register_operand"      "   vr,   vr,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,    0,    vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_le_one (<MODE>mode)"
  "vmf%B3.vf\t%0,%4,%5%p1"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "thv,thv,rvv,rvv")])

;; We use early-clobber for source LMUL > dest LMUL.
(define_insn "*pred_eqne<mode>_scalar_narrow"
  [(set (match_operand:<VM> 0 "register_operand"                "=vm,   vr,   vr,  &vr,  &vr")
	(if_then_else:<VM>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "    0,vmWc1,vmWc1,vmWc1,vmWc1")
	     (match_operand 6 "vector_length_operand"         "   rK,   rK,   rK,   rK,   rK")
	     (match_operand 7 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i,    i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operator:<VM> 3 "equality_operator"
	     [(vec_duplicate:V_VLSF
	        (match_operand:<VEL> 5 "register_operand"     "    f,    f,    f,    f,    f"))
	      (match_operand:V_VLSF 4 "register_operand"      "   vr,    0,    0,   vr,   vr")])
	  (match_operand:<VM> 2 "vector_merge_operand"        "   vu,   vu,    0,   vu,    0")))]
  "TARGET_VECTOR && riscv_vector::cmp_lmul_gt_one (<MODE>mode)"
  "vmf%B3.vf\t%0,%4,%5%p1"
  [(set_attr "type" "vfcmp")
   (set_attr "mode" "<MODE>")
   (set_attr "spec_restriction" "none,thv,thv,none,none")])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point merge
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.15 Vector Floating-Point Merge Instruction
;; -------------------------------------------------------------------------------

(define_insn "@pred_merge<mode>_scalar"
  [(set (match_operand:V_VLSF 0 "register_operand"      "=vd,vd")
    (if_then_else:V_VLSF
      (unspec:<VM>
        [(match_operand 5 "vector_length_operand"   " rK,rK")
         (match_operand 6 "const_int_operand"       "  i, i")
         (match_operand 7 "const_int_operand"       "  i, i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (vec_merge:V_VLSF
        (vec_duplicate:V_VLSF
          (match_operand:<VEL> 3 "register_operand" "  f, f"))
        (match_operand:V_VLSF 2 "register_operand"      " vr,vr")
	(match_operand:<VM> 4 "register_operand"    " vm,vm"))
      (match_operand:V_VLSF 1 "vector_merge_operand"    " vu, 0")))]
  "TARGET_VECTOR"
  "vfmerge.vfm\t%0,%2,%3,%4"
  [(set_attr "type" "vfmerge")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point conversions
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.17 Single-Width Floating-Point/Integer Type-Convert Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_fcvt_x<v_su>_f<mode>"
  [(set (match_operand:<VCONVERT> 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:<VCONVERT>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"        " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"            "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<VCONVERT>
	     [(match_operand:V_VLSF 3 "register_operand"     " vr, vr, vr, vr")] VFCVTS)
	  (match_operand:<VCONVERT> 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfcvt.x<v_su>.f.v\t%0,%3%p1"
  [(set_attr "type" "vfcvtftoi")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

(define_insn "@pred_<fix_cvt><mode>"
  [(set (match_operand:<VCONVERT> 0 "register_operand"       "=vd, vd, vr, vr")
	(if_then_else:<VCONVERT>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"        " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"            "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"            "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_fix:<VCONVERT>
	     (match_operand:V_VLSF 3 "register_operand"          " vr, vr, vr, vr"))
	  (match_operand:<VCONVERT> 2 "vector_merge_operand" " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfcvt.rtz.x<u>.f.v\t%0,%3%p1"
  [(set_attr "type" "vfcvtftoi")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_<float_cvt><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"              "=vd, vd, vr, vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"    " vm, vm,Wc1,Wc1")
	     (match_operand 4 "vector_length_operand"       " rK, rK, rK, rK")
	     (match_operand 5 "const_int_operand"           "  i,  i,  i,  i")
	     (match_operand 6 "const_int_operand"           "  i,  i,  i,  i")
	     (match_operand 7 "const_int_operand"           "  i,  i,  i,  i")
	     (match_operand 8 "const_int_operand"           "  i,  i,  i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (any_float:V_VLSF
	     (match_operand:<VCONVERT> 3 "register_operand" " vr, vr, vr, vr"))
	  (match_operand:V_VLSF 2 "vector_merge_operand"        " vu,  0, vu,  0")))]
  "TARGET_VECTOR"
  "vfcvt.f.x<u>.v\t%0,%3%p1"
  [(set_attr "type" "vfcvtitof")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point widen conversions
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.18 Widening Floating-Point/Integer Type-Convert Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_widen_fcvt_x<v_su>_f<mode>"
  [(set (match_operand:VWCONVERTI 0 "register_operand"         "=&vr,  &vr")
	(if_then_else:VWCONVERTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"         "   rK,   rK")
	     (match_operand 5 "const_int_operand"             "    i,    i")
	     (match_operand 6 "const_int_operand"             "    i,    i")
	     (match_operand 7 "const_int_operand"             "    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VWCONVERTI
	     [(match_operand:<VNCONVERT> 3 "register_operand" "   vr,   vr")] VFCVTS)
	  (match_operand:VWCONVERTI 2 "vector_merge_operand"  "   vu,    0")))]
  "TARGET_VECTOR"
  "vfwcvt.x<v_su>.f.v\t%0,%3%p1"
  [(set_attr "type" "vfwcvtftoi")
   (set_attr "mode" "<VNCONVERT>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

(define_insn "@pred_widen_<fix_cvt><mode>"
  [(set (match_operand:VWCONVERTI 0 "register_operand"        "=&vr,  &vr")
	(if_then_else:VWCONVERTI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"        "   rK,   rK")
	     (match_operand 5 "const_int_operand"            "    i,    i")
	     (match_operand 6 "const_int_operand"            "    i,    i")
	     (match_operand 7 "const_int_operand"            "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_fix:VWCONVERTI
	     (match_operand:<VNCONVERT> 3 "register_operand" "   vr,   vr"))
	  (match_operand:VWCONVERTI 2 "vector_merge_operand" "   vu,    0")))]
  "TARGET_VECTOR"
  "vfwcvt.rtz.x<u>.f.v\t%0,%3%p1"
  [(set_attr "type" "vfwcvtftoi")
   (set_attr "mode" "<VNCONVERT>")])

(define_insn "@pred_widen_<float_cvt><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"                "=&vr,  &vr")
	(if_then_else:V_VLSF
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"     "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"        "   rK,   rK")
	     (match_operand 5 "const_int_operand"            "    i,    i")
	     (match_operand 6 "const_int_operand"            "    i,    i")
	     (match_operand 7 "const_int_operand"            "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_float:V_VLSF
	     (match_operand:<VNCONVERT> 3 "register_operand" "   vr,   vr"))
	  (match_operand:V_VLSF 2 "vector_merge_operand"         "   vu,    0")))]
  "TARGET_VECTOR"
  "vfwcvt.f.x<u>.v\t%0,%3%p1"
  [(set_attr "type" "vfwcvtitof")
   (set_attr "mode" "<VNCONVERT>")])

(define_insn "@pred_extend<mode>"
  [(set (match_operand:VWEXTF_ZVFHMIN 0 "register_operand"                 "=&vr,  &vr")
	(if_then_else:VWEXTF_ZVFHMIN
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"          "vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"             "   rK,   rK")
	     (match_operand 5 "const_int_operand"                 "    i,    i")
	     (match_operand 6 "const_int_operand"                 "    i,    i")
	     (match_operand 7 "const_int_operand"                 "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (float_extend:VWEXTF_ZVFHMIN
	     (match_operand:<V_DOUBLE_TRUNC> 3 "register_operand" "   vr,   vr"))
	  (match_operand:VWEXTF_ZVFHMIN 2 "vector_merge_operand"          "   vu,    0")))]
  "TARGET_VECTOR"
  "vfwcvt.f.f.v\t%0,%3%p1"
  [(set_attr "type" "vfwcvtftof")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated floating-point narrow conversions
;; -------------------------------------------------------------------------------
;; Includes:
;; - 13.19 Narrowing Floating-Point/Integer Type-Convert Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_narrow_fcvt_x<v_su>_f<mode>"
  [(set (match_operand:<VNCONVERT> 0 "register_operand"        "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<VNCONVERT>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"       " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"          " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 5 "const_int_operand"              "  i,  i,  i,  i,    i,    i")
	     (match_operand 6 "const_int_operand"              "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"              "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"              "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<VNCONVERT>
	     [(match_operand:V_VLSF 3 "register_operand"       "  0,  0,  0,  0,   vr,   vr")] VFCVTS)
	  (match_operand:<VNCONVERT> 2 "vector_merge_operand"  " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vfncvt.x<v_su>.f.w\t%0,%3%p1"
  [(set_attr "type" "vfncvtftoi")
   (set_attr "mode" "<VNCONVERT>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))
   (set_attr "spec_restriction" "none,none,thv,thv,none,none")])

(define_insn "@pred_narrow_<fix_cvt><mode>"
  [(set (match_operand:<VNCONVERT> 0 "register_operand"        "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<VNCONVERT>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"         " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 5 "const_int_operand"             "  i,  i,  i,  i,    i,    i")
	     (match_operand 6 "const_int_operand"             "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"             "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (any_fix:<VNCONVERT>
	     (match_operand:V_VLSF 3 "register_operand"           "  0,  0,  0,  0,   vr,   vr"))
	  (match_operand:<VNCONVERT> 2 "vector_merge_operand" " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vfncvt.rtz.x<u>.f.w\t%0,%3%p1"
  [(set_attr "type" "vfncvtftoi")
   (set_attr "mode" "<VNCONVERT>")])

(define_insn "@pred_narrow_<float_cvt><mode>"
  [(set (match_operand:<VNCONVERT> 0 "register_operand"       "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<VNCONVERT>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"         " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 5 "const_int_operand"             "  i,  i,  i,  i,    i,    i")
	     (match_operand 6 "const_int_operand"             "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"             "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"             "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (any_float:<VNCONVERT>
	     (match_operand:VWCONVERTI 3 "register_operand"   "  0,  0,  0,  0,   vr,   vr"))
	  (match_operand:<VNCONVERT> 2 "vector_merge_operand" " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vfncvt.f.x<u>.w\t%0,%3%p1"
  [(set_attr "type" "vfncvtitof")
   (set_attr "mode" "<VNCONVERT>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))
   (set_attr "spec_restriction" "none,none,thv,thv,none,none")])

(define_insn "@pred_trunc<mode>"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand"       "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<V_DOUBLE_TRUNC>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"              " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 5 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (match_operand 6 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (match_operand 8 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	  (float_truncate:<V_DOUBLE_TRUNC>
	     (match_operand:VWEXTF_ZVFHMIN 3 "register_operand"            "  0,  0,  0,  0,   vr,   vr"))
	  (match_operand:<V_DOUBLE_TRUNC> 2 "vector_merge_operand" " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vfncvt.f.f.w\t%0,%3%p1"
  [(set_attr "type" "vfncvtftof")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))
   (set_attr "spec_restriction" "none,none,thv,thv,none,none")])

(define_insn "@pred_rod_trunc<mode>"
  [(set (match_operand:<V_DOUBLE_TRUNC> 0 "register_operand"       "=vd, vd, vr, vr,  &vr,  &vr")
	(if_then_else:<V_DOUBLE_TRUNC>
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"           " vm, vm,Wc1,Wc1,vmWc1,vmWc1")
	     (match_operand 4 "vector_length_operand"              " rK, rK, rK, rK,   rK,   rK")
	     (match_operand 5 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (match_operand 6 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (match_operand 7 "const_int_operand"                  "  i,  i,  i,  i,    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:<V_DOUBLE_TRUNC>
	    [(float_truncate:<V_DOUBLE_TRUNC>
	       (match_operand:VWEXTF_ZVFHMIN 3 "register_operand"          "  0,  0,  0,  0,   vr,   vr"))] UNSPEC_ROD)
	  (match_operand:<V_DOUBLE_TRUNC> 2 "vector_merge_operand" " vu,  0, vu,  0,   vu,    0")))]
  "TARGET_VECTOR"
  "vfncvt.rod.f.f.w\t%0,%3%p1"
  [(set_attr "type" "vfncvtftof")
   (set_attr "mode" "<V_DOUBLE_TRUNC>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated reduction operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 14.1 Vector Single-Width Integer Reduction Instructions
;; - 14.2 Vector Widening Integer Reduction Instructions
;; - 14.3 Vector Single-Width Floating-Point Reduction Instructions
;; - 14.4 Vector Widening Floating-Point Reduction Instructions
;; -------------------------------------------------------------------------------

;; Integer Reduction (vred(sum|maxu|max|minu|min|and|or|xor).vs)
(define_insn "@pred_<reduc_op><mode>"
  [(set (match_operand:<V_LMUL1>          0 "register_operand"      "=vr,     vr")
	(unspec:<V_LMUL1>
	  [(unspec:<VM>
	    [(match_operand:<VM>          1 "vector_mask_operand"   "vmWc1,vmWc1")
	     (match_operand               5 "vector_length_operand" "   rK,   rK")
	     (match_operand               6 "const_int_operand"     "    i,    i")
	     (match_operand               7 "const_int_operand"     "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:<V_LMUL1> [
             (match_operand:V_VLSI        3 "register_operand"      "   vr,   vr")
             (match_operand:<V_LMUL1>     4 "register_operand"      "   vr,   vr")
           ] ANY_REDUC)
	   (match_operand:<V_LMUL1>       2 "vector_merge_operand"  "   vu,    0")] UNSPEC_REDUC))]
  "TARGET_VECTOR"
  "v<reduc_op>.vs\t%0,%3,%4%p1"
  [(set_attr "type" "vired")
   (set_attr "mode" "<MODE>")])

;; Integer Widen Reduction Sum (vwredsum[u].vs)
(define_insn "@pred_<reduc_op><mode>"
  [(set (match_operand:<V_EXT_LMUL1>       0 "register_operand"        "=vr,   vr")
	(unspec:<V_EXT_LMUL1>
	  [(unspec:<VM>
	    [(match_operand:<VM>           1 "vector_mask_operand"   "vmWc1,vmWc1")
	     (match_operand                5 "vector_length_operand" "   rK,   rK")
	     (match_operand                6 "const_int_operand"     "    i,    i")
	     (match_operand                7 "const_int_operand"     "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
           (unspec:<V_EXT_LMUL1> [
	     (match_operand:VI_QHS         3 "register_operand"      "   vr,   vr")
	     (match_operand:<V_EXT_LMUL1>  4 "register_operand"      "   vr,   vr")
           ] ANY_WREDUC)
	   (match_operand:<V_EXT_LMUL1>    2 "vector_merge_operand"  "   vu,    0")] UNSPEC_REDUC))]
  "TARGET_VECTOR"
  "v<reduc_op>.vs\t%0,%3,%4%p1"
  [(set_attr "type" "viwred")
   (set_attr "mode" "<MODE>")])

;; Float Reduction (vfred(max|min).vs)
(define_insn "@pred_<reduc_op><mode>"
  [(set (match_operand:<V_LMUL1>          0 "register_operand"      "=vr,     vr")
	(unspec:<V_LMUL1>
	  [(unspec:<VM>
	    [(match_operand:<VM>          1 "vector_mask_operand"   "vmWc1,vmWc1")
	     (match_operand               5 "vector_length_operand" "   rK,   rK")
	     (match_operand               6 "const_int_operand"     "    i,    i")
	     (match_operand               7 "const_int_operand"     "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
           (unspec:<V_LMUL1> [
             (match_operand:V_VLSF        3 "register_operand"      "   vr,   vr")
             (match_operand:<V_LMUL1>     4 "register_operand"      "   vr,   vr")
           ] ANY_FREDUC)
	   (match_operand:<V_LMUL1>       2 "vector_merge_operand"  "   vu,    0")] UNSPEC_REDUC))]
  "TARGET_VECTOR"
  "vf<reduc_op>.vs\t%0,%3,%4%p1"
  [(set_attr "type" "vfredu")
   (set_attr "mode" "<MODE>")])

;; Float Reduction Sum (vfred[ou]sum.vs)
(define_insn "@pred_<reduc_op><mode>"
  [(set (match_operand:<V_LMUL1>           0 "register_operand"      "=vr,vr")
	(unspec:<V_LMUL1>
	  [(unspec:<VM>
	    [(match_operand:<VM>          1 "vector_mask_operand"   "vmWc1,vmWc1")
	     (match_operand               5 "vector_length_operand" "   rK,   rK")
	     (match_operand               6 "const_int_operand"     "    i,    i")
	     (match_operand               7 "const_int_operand"     "    i,    i")
	     (match_operand               8 "const_int_operand"     "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
           (unspec:<V_LMUL1> [
             (match_operand:V_VLSF        3 "register_operand"      "   vr,   vr")
             (match_operand:<V_LMUL1>     4 "register_operand"      "   vr,   vr")
           ] ANY_FREDUC_SUM)
	   (match_operand:<V_LMUL1>       2 "vector_merge_operand"  "   vu,    0")] UNSPEC_REDUC))]
  "TARGET_VECTOR"
  "vf<reduc_op>.vs\t%0,%3,%4%p1"
  [(set_attr "type" "vfred<order>")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

;; Float Widen Reduction Sum (vfwred[ou]sum.vs)
(define_insn "@pred_<reduc_op><mode>"
  [(set (match_operand:<V_EXT_LMUL1>         0 "register_operand"      "=vr,   vr")
	(unspec:<V_EXT_LMUL1>
	  [(unspec:<VM>
	    [(match_operand:<VM>           1 "vector_mask_operand"   "vmWc1,vmWc1")
	     (match_operand                5 "vector_length_operand" "   rK,   rK")
	     (match_operand                6 "const_int_operand"     "    i,    i")
	     (match_operand                7 "const_int_operand"     "    i,    i")
	     (match_operand                8 "const_int_operand"     "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
           (unspec:<V_EXT_LMUL1> [
	     (match_operand:VF_HS          3 "register_operand"      "   vr,   vr")
	     (match_operand:<V_EXT_LMUL1>  4 "register_operand"      "   vr,   vr")
           ] ANY_FWREDUC_SUM)
	   (match_operand:<V_EXT_LMUL1>    2 "vector_merge_operand"  "   vu,    0")] UNSPEC_REDUC))]
  "TARGET_VECTOR"
  "vf<reduc_op>.vs\t%0,%3,%4%p1"
  [(set_attr "type" "vfwred<order>")
   (set_attr "mode" "<MODE>")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[8])"))])

;; -------------------------------------------------------------------------------
;; ---- Predicated permutation operations
;; -------------------------------------------------------------------------------
;; Includes:
;; - 16.1 Integer Scalar Move Instructions
;; - 16.2 Floating-Point Scalar Move Instructions
;; - 16.3 Vector Slide Instructions
;; - 16.4 Vector Register Gather Instructions
;; - 16.5 Vector Compress Instruction
;; -------------------------------------------------------------------------------

(define_expand "@pred_extract_first<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL>
	  [(vec_select:<VEL>
	     (match_operand:V_VLSI 1 "reg_or_mem_operand")
	     (parallel [(const_int 0)]))
	   (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE))]
  "TARGET_VECTOR"
{
  if (MEM_P (operands[1]))
    {
      /* Combine vle.v + vmv.x.s ==> lw.  */
      emit_move_insn (operands[0], gen_rtx_MEM (<VEL>mode, XEXP (operands[1], 0)));
      DONE;
    }
})

(define_insn_and_split "*pred_extract_first<mode>"
  [(set (match_operand:<VEL> 0 "register_operand"   "=r")
	(unspec:<VEL>
	  [(vec_select:<VEL>
	     (match_operand:V_VLSI 1 "register_operand" "vr")
	     (parallel [(const_int 0)]))
	   (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE))]
  "TARGET_VECTOR"
  "vmv.x.s\t%0,%1"
  "known_gt (GET_MODE_BITSIZE (<VEL>mode), GET_MODE_BITSIZE (Pmode))"
  [(const_int 0)]
{
  /* In rv32 system, we can't use vmv.x.s directly.
     Instead, we should generate this following code sequence:
       vsrl.vx v16,v8,a0
       vmv.x.s a1,v16
       vmv.x.s a0,v8  */
  rtx nbits = force_reg (Pmode, gen_int_mode (GET_MODE_BITSIZE (Pmode), Pmode));
  rtx high_bits = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_pred_scalar (LSHIFTRT, <MODE>mode, high_bits, CONSTM1_RTX (<VM>mode),
  			RVV_VUNDEF (<MODE>mode), operands[1], nbits, /* vl */ const1_rtx,
			gen_int_mode (riscv_vector::TAIL_ANY, Pmode),
			gen_int_mode (riscv_vector::MASK_ANY, Pmode),
			gen_int_mode (riscv_vector::NONVLMAX, Pmode)));
  emit_insn (gen_pred_extract_first_trunc (<MODE>mode,
  			gen_highpart (SImode, operands[0]), high_bits));
  emit_insn (gen_pred_extract_first_trunc (<MODE>mode,
  			gen_lowpart (SImode, operands[0]), operands[1]));
  DONE;
}
  [(set_attr "type" "vimovvx")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_extract_first_trunc<mode>"
  [(set (match_operand:SI 0 "register_operand"          "=r")
        (truncate:SI
	  (unspec:<VEL>
	    [(vec_select:<VEL>
	       (match_operand:V_VLSI_D 1 "register_operand" "vr")
	       (parallel [(const_int 0)]))
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)))]
  "TARGET_VECTOR"
  "vmv.x.s\t%0,%1"
  [(set_attr "type" "vimovvx")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_extract_first<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL>
	  [(vec_select:<VEL>
	     (match_operand:V_VLSF 1 "reg_or_mem_operand")
	     (parallel [(const_int 0)]))
	   (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE))]
  "TARGET_VECTOR"
{
  if (MEM_P (operands[1]))
    {
      /* Combine vle.v + vmv.f.s ==> flw.  */
      emit_move_insn (operands[0], gen_rtx_MEM (<VEL>mode, XEXP (operands[1], 0)));
      DONE;
    }
})

(define_insn "*pred_extract_first<mode>"
  [(set (match_operand:<VEL> 0 "register_operand"   "=f")
	(unspec:<VEL>
	  [(vec_select:<VEL>
	     (match_operand:V_VLSF 1 "register_operand" "vr")
	     (parallel [(const_int 0)]))
	   (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE))]
  "TARGET_VECTOR"
  "vfmv.f.s\t%0,%1"
  [(set_attr "type" "vfmovvf")
   (set_attr "mode" "<MODE>")])

;; vslide instructions
(define_insn "@pred_slide<ud><mode>"
  [(set (match_operand:V_VLS 0 "register_operand"             "<ud_constraint>")
	(unspec:V_VLS
	  [(unspec:<VM>
	     [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	      (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	      (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand:V_VLS 2 "vector_merge_operand"      " vu,  0, vu,  0")
	   (match_operand:V_VLS 3 "register_operand"          " vr, vr, vr, vr")
	   (match_operand 4 "pmode_reg_or_uimm5_operand"  " rK, rK, rK, rK")] VSLIDES))]
  "TARGET_VECTOR"
  "vslide<ud>.v%o4\t%0,%3,%4%p1"
  [(set_attr "type" "vslide<ud>")
   (set_attr "mode" "<MODE>")])

;; vslide1 instructions
(define_insn "@pred_slide<ud><mode>"
  [(set (match_operand:V_VLSI_QHS 0 "register_operand"        "<ud_constraint>")
	(unspec:V_VLSI_QHS
	  [(unspec:<VM>
	     [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	      (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	      (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand:V_VLSI_QHS 2 "vector_merge_operand" " vu,  0, vu,  0")
	   (match_operand:V_VLSI_QHS 3 "register_operand"     " vr, vr, vr, vr")
	   (match_operand:<VEL> 4 "reg_or_0_operand"      " rJ, rJ, rJ, rJ")] VSLIDES1))]
  "TARGET_VECTOR"
  "vslide<ud>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vislide<ud>")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_slide<ud><mode>"
  [(set (match_operand:V_VLSI_D 0 "register_operand")
	(unspec:V_VLSI_D
	  [(unspec:<VM>
	     [(match_operand:<VM> 1 "vector_mask_operand")
	      (match_operand 5 "reg_or_int_operand")
	      (match_operand 6 "const_int_operand")
	      (match_operand 7 "const_int_operand")
	      (match_operand 8 "const_int_operand")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand:V_VLSI_D 2 "vector_merge_operand")
	   (match_operand:V_VLSI_D 3 "register_operand")
	   (match_operand:<VEL> 4 "reg_or_int_operand")] VSLIDES1))]
  "TARGET_VECTOR"
{
  poly_uint64 nunits = GET_MODE_NUNITS (<MODE>mode) * 2;
  machine_mode vsimode = riscv_vector::get_vector_mode (SImode, nunits).require ();
  machine_mode vbimode = riscv_vector::get_vector_mode (BImode, nunits).require ();
  if (riscv_vector::slide1_sew64_helper (<UNSPEC>, <MODE>mode,
					 vsimode, vbimode,
					 operands))
    DONE;
})

(define_insn "*pred_slide<ud><mode>"
  [(set (match_operand:V_VLSI_D 0 "register_operand"          "<ud_constraint>")
	(unspec:V_VLSI_D
	  [(unspec:<VM>
	     [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	      (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	      (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand:V_VLSI_D 2 "vector_merge_operand"   " vu,  0, vu,  0")
	   (match_operand:V_VLSI_D 3 "register_operand"       " vr, vr, vr, vr")
	   (match_operand:<VEL> 4 "reg_or_0_operand"      " rJ, rJ, rJ, rJ")] VSLIDES1))]
  "TARGET_VECTOR"
  "vslide<ud>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vislide<ud>")
   (set_attr "mode" "<MODE>")])

(define_insn "*pred_slide<ud><mode>_extended"
  [(set (match_operand:V_VLSI_D 0 "register_operand"          "<ud_constraint>")
	(unspec:V_VLSI_D
	  [(unspec:<VM>
	     [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	      (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	      (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand:V_VLSI_D 2 "vector_merge_operand"   " vu,  0, vu,  0")
	   (match_operand:V_VLSI_D 3 "register_operand"       " vr, vr, vr, vr")
	   (sign_extend:<VEL>
	     (match_operand:<VSUBEL> 4 "reg_or_0_operand" " rJ, rJ, rJ, rJ"))] VSLIDES1))]
  "TARGET_VECTOR && !TARGET_64BIT"
  "vslide<ud>.vx\t%0,%3,%z4%p1"
  [(set_attr "type" "vislide<ud>")
   (set_attr "mode" "<MODE>")])

;; vfslide1 instructions
(define_insn "@pred_slide<ud><mode>"
  [(set (match_operand:V_VLSF 0 "register_operand"  "<ud_constraint>")
	(unspec:V_VLSF
	  [(unspec:<VM>
	     [(match_operand:<VM> 1 "vector_mask_operand" " vm, vm,Wc1,Wc1")
	      (match_operand 5 "vector_length_operand"    " rK, rK, rK, rK")
	      (match_operand 6 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 7 "const_int_operand"        "  i,  i,  i,  i")
	      (match_operand 8 "const_int_operand"        "  i,  i,  i,  i")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand:V_VLSF 2 "vector_merge_operand" " vu,  0, vu,  0")
	   (match_operand:V_VLSF 3 "register_operand"     " vr, vr, vr, vr")
	   (match_operand:<VEL> 4 "register_operand"      "  f,  f,  f,  f")] VFSLIDES1))]
  "TARGET_VECTOR"
  "vfslide<ud>.vf\t%0,%3,%4%p1"
  [(set_attr "type" "vfslide<ud>")
   (set_attr "mode" "<MODE>")])

;; vrgather
(define_insn "@pred_gather<mode>"
  [(set (match_operand:V_VLS 0 "register_operand"              "=&vr,  &vr")
	(if_then_else:V_VLS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK,   rK")
	     (match_operand 6 "const_int_operand"         "    i,    i")
	     (match_operand 7 "const_int_operand"         "    i,    i")
	     (match_operand 8 "const_int_operand"         "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V_VLS
	    [(match_operand:V_VLS 3 "register_operand"        "   vr,   vr")
	     (match_operand:<VINDEX> 4 "register_operand" "   vr,   vr")] UNSPEC_VRGATHER)
	  (match_operand:V_VLS 2 "vector_merge_operand"       "   vu,    0")))]
  "TARGET_VECTOR"
  "vrgather.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vgather")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_gather<mode>_scalar"
  [(set (match_operand:V_VLS 0 "register_operand"               "=&vr,  &vr")
	(if_then_else:V_VLS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"   "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"      "   rK,   rK")
	     (match_operand 6 "const_int_operand"          "    i,    i")
	     (match_operand 7 "const_int_operand"          "    i,    i")
	     (match_operand 8 "const_int_operand"          "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V_VLS
	    [(match_operand:V_VLS 3 "register_operand"         "   vr,   vr")
	     (match_operand 4 "pmode_reg_or_uimm5_operand" "   rK,   rK")] UNSPEC_VRGATHER)
	  (match_operand:V_VLS 2 "vector_merge_operand"        "   vu,    0")))]
  "TARGET_VECTOR"
  "vrgather.v%o4\t%0,%3,%4%p1"
  [(set_attr "type" "vgather")
   (set_attr "mode" "<MODE>")])

;; vrgatherei16
(define_insn "@pred_gatherei16<mode>"
  [(set (match_operand:VEI16 0 "register_operand"              "=&vr,  &vr")
	(if_then_else:VEI16
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"         "   rK,   rK")
	     (match_operand 6 "const_int_operand"             "    i,    i")
	     (match_operand 7 "const_int_operand"             "    i,    i")
	     (match_operand 8 "const_int_operand"             "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VEI16
	    [(match_operand:VEI16 3 "register_operand"        "   vr,   vr")
	     (match_operand:<VINDEXEI16> 4 "register_operand" "   vr,   vr")] UNSPEC_VRGATHEREI16)
	  (match_operand:VEI16 2 "vector_merge_operand"       "   vu,    0")))]
  "TARGET_VECTOR"
  "vrgatherei16.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vgather")
   (set_attr "mode" "<MODE>")])

;; vcompress
(define_insn "@pred_compress<mode>"
  [(set (match_operand:V_VLS 0 "register_operand"            "=&vr,  &vr")
	(unspec:V_VLS
	  [(unspec:<VM>
	    [(match_operand:<VM> 3 "register_operand"    "  vm,  vm")
	     (match_operand 4 "vector_length_operand"    "  rK,  rK")
	     (match_operand 5 "const_int_operand"        "   i,   i")
	     (match_operand 6 "const_int_operand"        "   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand:V_VLS 2 "register_operand"         "  vr,  vr")
	   (match_operand:V_VLS 1 "vector_merge_operand"     "  vu,   0")] UNSPEC_VCOMPRESS))]
  "TARGET_VECTOR"
  "vcompress.vm\t%0,%2,%3"
  [(set_attr "type" "vcompress")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated Fault-Only-First loads
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.7. Unit-stride Fault-Only-First Loads
;; -------------------------------------------------------------------------------

(define_insn "read_vlsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(reg:SI VL_REGNUM))]
  "TARGET_VECTOR"
  "csrr\t%0,vl"
  [(set_attr "type" "rdvl")
   (set_attr "mode" "SI")])

(define_insn "read_vldi_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (reg:SI VL_REGNUM)))]
  "TARGET_VECTOR && TARGET_64BIT"
  "csrr\t%0,vl"
  [(set_attr "type" "rdvl")
   (set_attr "mode" "DI")])

(define_insn "@pred_fault_load<mode>"
  [(set (match_operand:V 0 "register_operand"              "=vd,    vd,    vr,    vr")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "   vm,    vm,   Wc1,   Wc1")
	     (match_operand 4 "vector_length_operand"    "   rK,    rK,    rK,    rK")
	     (match_operand 5 "const_int_operand"        "    i,     i,     i,     i")
	     (match_operand 6 "const_int_operand"        "    i,     i,     i,     i")
	     (match_operand 7 "const_int_operand"        "    i,     i,     i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V
	    [(match_operand:V 3 "memory_operand"         "    m,     m,     m,     m")] UNSPEC_VLEFF)
	  (match_operand:V 2 "vector_merge_operand"      "   vu,     0,    vu,     0")))
   (set (reg:SI VL_REGNUM)
	  (unspec:SI
	    [(if_then_else:V
	       (unspec:<VM>
		[(match_dup 1) (match_dup 4) (match_dup 5)
		 (match_dup 6) (match_dup 7)
		 (reg:SI VL_REGNUM) (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	       (unspec:V [(match_dup 3)] UNSPEC_VLEFF)
	       (match_dup 2))] UNSPEC_MODIFY_VL))]
  "TARGET_VECTOR"
  "vle<sew>ff.v\t%0,%3%p1"
  [(set_attr "type" "vldff")
   (set_attr "mode" "<MODE>")])


;; -------------------------------------------------------------------------------
;; ---- Predicated Segment loads/stores
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.8.1. Vector Unit-Stride Segment Loads and Stores
;; - 7.8.2. Vector Strided Segment Loads and Stores
;; - 7.8.3. Vector Indexed Segment Loads and Stores
;; -------------------------------------------------------------------------------

(define_insn "@pred_unit_strided_load<mode>"
  [(set (match_operand:VT 0 "register_operand"             "=vr,    vr,    vd")
	(if_then_else:VT
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,   Wc1,    vm")
	     (match_operand 4 "vector_length_operand"    "   rK,    rK,    rK")
	     (match_operand 5 "const_int_operand"        "    i,     i,     i")
	     (match_operand 6 "const_int_operand"        "    i,     i,     i")
	     (match_operand 7 "const_int_operand"        "    i,     i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VT
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,    rJ,    rJ")
	     (mem:BLK (scratch))] UNSPEC_UNIT_STRIDED)
	  (match_operand:VT 2 "vector_merge_operand"     "    0,    vu,    vu")))]
  "TARGET_VECTOR"
  "vlseg<nf>e<sew>.v\t%0,(%z3)%p1"
  [(set_attr "type" "vlsegde")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_unit_strided_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	     [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	      (match_operand 3 "vector_length_operand"    "   rK")
	      (match_operand 4 "const_int_operand"        "    i")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"      "   rJ")
	   (match_operand:VT 2 "register_operand"         "   vr")
	   (mem:BLK (scratch))] UNSPEC_UNIT_STRIDED))]
  "TARGET_VECTOR"
  "vsseg<nf>e<sew>.v\t%2,(%z1)%p0"
  [(set_attr "type" "vssegte")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_strided_load<mode>"
  [(set (match_operand:VT 0 "register_operand"             "=vr,    vr,    vd")
	(if_then_else:VT
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,   Wc1,    vm")
	     (match_operand 5 "vector_length_operand"    "   rK,    rK,    rK")
	     (match_operand 6 "const_int_operand"        "    i,     i,     i")
	     (match_operand 7 "const_int_operand"        "    i,     i,     i")
	     (match_operand 8 "const_int_operand"        "    i,     i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VT
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,    rJ,    rJ")
	     (match_operand 4 "pmode_reg_or_0_operand"   "   rJ,    rJ,    rJ")
	     (mem:BLK (scratch))] UNSPEC_STRIDED)
	  (match_operand:VT 2 "vector_merge_operand"     "    0,    vu,    vu")))]
  "TARGET_VECTOR"
  "vlsseg<nf>e<sew>.v\t%0,(%z3),%z4%p1"
  [(set_attr "type" "vlsegds")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_strided_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	     [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	      (match_operand 4 "vector_length_operand"    "   rK")
	      (match_operand 5 "const_int_operand"        "    i")
	      (reg:SI VL_REGNUM)
	      (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"      "   rJ")
	   (match_operand 2 "pmode_reg_or_0_operand"      "   rJ")
	   (match_operand:VT 3 "register_operand"         "   vr")
	   (mem:BLK (scratch))] UNSPEC_STRIDED))]
  "TARGET_VECTOR"
  "vssseg<nf>e<sew>.v\t%3,(%z1),%z2%p0"
  [(set_attr "type" "vssegts")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_fault_load<mode>"
  [(set (match_operand:VT 0 "register_operand"             "=vr,    vr,    vd")
	(if_then_else:VT
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,   Wc1,    vm")
	     (match_operand 4 "vector_length_operand"    "   rK,    rK,    rK")
	     (match_operand 5 "const_int_operand"        "    i,     i,     i")
	     (match_operand 6 "const_int_operand"        "    i,     i,     i")
	     (match_operand 7 "const_int_operand"        "    i,     i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VT
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,    rJ,    rJ")
	     (mem:BLK (scratch))] UNSPEC_VLEFF)
	  (match_operand:VT 2 "vector_merge_operand"     "    0,    vu,    vu")))
   (set (reg:SI VL_REGNUM)
        (unspec:SI
          [(if_then_else:VT
	     (unspec:<VM>
	       [(match_dup 1) (match_dup 4) (match_dup 5)
	        (match_dup 6) (match_dup 7)
	        (reg:SI VL_REGNUM)
	        (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	     (unspec:VT
	        [(match_dup 3) (mem:BLK (scratch))] UNSPEC_VLEFF)
	     (match_dup 2))] UNSPEC_MODIFY_VL))]
  "TARGET_VECTOR"
  "vlseg<nf>e<sew>ff.v\t%0,(%z3)%p1"
  [(set_attr "type" "vlsegdff")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_indexed_<order>load<V1T:mode><RATIO64I:mode>"
  [(set (match_operand:V1T 0 "register_operand"           "=&vr,  &vr")
	(if_then_else:V1T
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"    "   rK,   rK")
	     (match_operand 6 "const_int_operand"        "    i,    i")
	     (match_operand 7 "const_int_operand"        "    i,    i")
	     (match_operand 8 "const_int_operand"        "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V1T
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:RATIO64I 4 "register_operand"     "   vr,   vr")] ORDER)
	  (match_operand:V1T 2 "vector_merge_operand"    "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xseg<nf>ei<RATIO64I:sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vlsegd<order>x")
   (set_attr "mode" "<V1T:MODE>")])

(define_insn "@pred_indexed_<order>load<V2T:mode><RATIO32I:mode>"
  [(set (match_operand:V2T 0 "register_operand"           "=&vr,  &vr")
	(if_then_else:V2T
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"    "   rK,   rK")
	     (match_operand 6 "const_int_operand"        "    i,    i")
	     (match_operand 7 "const_int_operand"        "    i,    i")
	     (match_operand 8 "const_int_operand"        "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V2T
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:RATIO32I 4 "register_operand"     "   vr,   vr")] ORDER)
	  (match_operand:V2T 2 "vector_merge_operand"    "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xseg<nf>ei<RATIO32I:sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vlsegd<order>x")
   (set_attr "mode" "<V2T:MODE>")])

(define_insn "@pred_indexed_<order>load<V4T:mode><RATIO16I:mode>"
  [(set (match_operand:V4T 0 "register_operand"           "=&vr,  &vr")
	(if_then_else:V4T
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"    "   rK,   rK")
	     (match_operand 6 "const_int_operand"        "    i,    i")
	     (match_operand 7 "const_int_operand"        "    i,    i")
	     (match_operand 8 "const_int_operand"        "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V4T
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:RATIO16I 4 "register_operand"     "   vr,   vr")] ORDER)
	  (match_operand:V4T 2 "vector_merge_operand"    "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xseg<nf>ei<RATIO16I:sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vlsegd<order>x")
   (set_attr "mode" "<V4T:MODE>")])

(define_insn "@pred_indexed_<order>load<V8T:mode><RATIO8I:mode>"
  [(set (match_operand:V8T 0 "register_operand"           "=&vr,  &vr")
	(if_then_else:V8T
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"    "   rK,   rK")
	     (match_operand 6 "const_int_operand"        "    i,    i")
	     (match_operand 7 "const_int_operand"        "    i,    i")
	     (match_operand 8 "const_int_operand"        "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V8T
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:RATIO8I 4 "register_operand"     "   vr,   vr")] ORDER)
	  (match_operand:V8T 2 "vector_merge_operand"    "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xseg<nf>ei<RATIO8I:sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vlsegd<order>x")
   (set_attr "mode" "<V8T:MODE>")])

(define_insn "@pred_indexed_<order>load<V16T:mode><RATIO4I:mode>"
  [(set (match_operand:V16T 0 "register_operand"          "=&vr,  &vr")
	(if_then_else:V16T
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"    "   rK,   rK")
	     (match_operand 6 "const_int_operand"        "    i,    i")
	     (match_operand 7 "const_int_operand"        "    i,    i")
	     (match_operand 8 "const_int_operand"        "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V16T
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:RATIO4I 4 "register_operand"    "   vr,   vr")] ORDER)
	  (match_operand:V16T 2 "vector_merge_operand"   "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xseg<nf>ei<RATIO4I:sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vlsegd<order>x")
   (set_attr "mode" "<V16T:MODE>")])

(define_insn "@pred_indexed_<order>load<V32T:mode><RATIO2I:mode>"
  [(set (match_operand:V32T 0 "register_operand"          "=&vr,  &vr")
	(if_then_else:V32T
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,vmWc1")
	     (match_operand 5 "vector_length_operand"    "   rK,   rK")
	     (match_operand 6 "const_int_operand"        "    i,    i")
	     (match_operand 7 "const_int_operand"        "    i,    i")
	     (match_operand 8 "const_int_operand"        "    i,    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V32T
	    [(match_operand 3 "pmode_reg_or_0_operand"   "   rJ,   rJ")
	     (mem:BLK (scratch))
	     (match_operand:RATIO2I 4 "register_operand"    "   vr,   vr")] ORDER)
	  (match_operand:V32T 2 "vector_merge_operand"   "   vu,    0")))]
  "TARGET_VECTOR"
  "vl<order>xseg<nf>ei<RATIO2I:sew>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vlsegd<order>x")
   (set_attr "mode" "<V32T:MODE>")])

(define_insn "@pred_indexed_<order>store<V1T:mode><RATIO64I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"     "   rJ")
	   (match_operand:RATIO64I 2 "register_operand"       "   vr")
	   (match_operand:V1T 3 "register_operand"       "   vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xseg<nf>ei<RATIO64I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vssegt<order>x")
   (set_attr "mode" "<V1T:MODE>")])

(define_insn "@pred_indexed_<order>store<V2T:mode><RATIO32I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"     "   rJ")
	   (match_operand:RATIO32I 2 "register_operand"       "   vr")
	   (match_operand:V2T 3 "register_operand"       "   vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xseg<nf>ei<RATIO32I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vssegt<order>x")
   (set_attr "mode" "<V2T:MODE>")])

(define_insn "@pred_indexed_<order>store<V4T:mode><RATIO16I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"     "   rJ")
	   (match_operand:RATIO16I 2 "register_operand"       "   vr")
	   (match_operand:V4T 3 "register_operand"       "   vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xseg<nf>ei<RATIO16I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vssegt<order>x")
   (set_attr "mode" "<V4T:MODE>")])

(define_insn "@pred_indexed_<order>store<V8T:mode><RATIO8I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"     "   rJ")
	   (match_operand:RATIO8I 2 "register_operand"       "   vr")
	   (match_operand:V8T 3 "register_operand"       "   vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xseg<nf>ei<RATIO8I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vssegt<order>x")
   (set_attr "mode" "<V8T:MODE>")])

(define_insn "@pred_indexed_<order>store<V16T:mode><RATIO4I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"     "   rJ")
	   (match_operand:RATIO4I 2 "register_operand"      "   vr")
	   (match_operand:V16T 3 "register_operand"      "   vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xseg<nf>ei<RATIO4I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vssegt<order>x")
   (set_attr "mode" "<V16T:MODE>")])

(define_insn "@pred_indexed_<order>store<V32T:mode><RATIO2I:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_reg_or_0_operand"     "   rJ")
	   (match_operand:RATIO2I 2 "register_operand"      "   vr")
	   (match_operand:V32T 3 "register_operand"      "   vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xseg<nf>ei<RATIO2I:sew>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vssegt<order>x")
   (set_attr "mode" "<V32T:MODE>")])

(include "autovec.md")
(include "autovec-opt.md")
