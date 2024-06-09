
;; Code and mode itertator and attribute definitions for the ARM backend
;; Copyright (C) 2010-2024 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
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


;;----------------------------------------------------------------------------
;; Mode iterators
;;----------------------------------------------------------------------------

;; A list of modes that are exactly 64 bits in size. This is used to expand
;; some splits that are the same for all modes when operating on ARM
;; registers.
(define_mode_iterator ANY64 [DI DF V8QI V4HI V4HF V2SI V2SF])

;; Additional definition of ANY64 that also includes the special V4BF mode.
;; BFmode is allowed only on define_split between ARM registers.
(define_mode_iterator ANY64_BF [DI DF V8QI V4HI V4BF V4HF V2SI V2SF])

(define_mode_iterator ANY128 [V2DI V2DF V16QI V8HI V4SI V4SF])

;; A list of integer modes that are up to one word long
(define_mode_iterator QHSI [QI HI SI])

;; A list of integer modes that are half and one word long
(define_mode_iterator HSI [HI SI])

;; A list of integer modes that are less than a word
(define_mode_iterator NARROW [QI HI])

;; A list of all the integer modes up to 64bit
(define_mode_iterator QHSD [QI HI SI DI])

;; A list of the 32bit and 64bit integer modes
(define_mode_iterator SIDI [SI DI])

;; A list of atomic compare and swap success return modes
(define_mode_iterator CCSI [(CC_Z "TARGET_32BIT") (SI "TARGET_THUMB1")])

;; A list of modes which the VFP unit can handle
(define_mode_iterator SDF [(SF "") (DF "TARGET_VFP_DOUBLE")])

;; Integer element sizes implemented by IWMMXT.
(define_mode_iterator VMMX [V2SI V4HI V8QI])

(define_mode_iterator VMMX2 [V4HI V2SI])

;; Integer element sizes for shifts.
(define_mode_iterator VSHFT [V4HI V2SI DI])

;; Integer and float modes supported by Neon and IWMMXT.
(define_mode_iterator VALL [V2DI V2SI V4HI V8QI V2SF V4SI V8HI V16QI V4SF])

;; Integer and float modes supported by Neon, IWMMXT and MVE.
(define_mode_iterator VNIM1 [V16QI V8HI V4SI V4SF V2DI])

;; Integer and float modes supported by Neon and IWMMXT but not MVE.
(define_mode_iterator VNINOTM1 [V2SI V4HI V8QI V2SF])

;; Integer and float modes supported by Neon and IWMMXT, except V2DI.
(define_mode_iterator VALLW [V2SI V4HI V8QI V2SF V4SI V8HI V16QI V4SF])

;; Integer modes supported by Neon and IWMMXT
(define_mode_iterator VINT [V2DI V2SI V4HI V8QI V4SI V8HI V16QI])

;; Integer modes supported by Neon and IWMMXT, except V2DI
(define_mode_iterator VINTW [V2SI V4HI V8QI V4SI V8HI V16QI])

;; Double-width vector modes, on which we support arithmetic (no HF!)
(define_mode_iterator VD [V8QI V4HI V2SI V2SF])

;; Double-width vector modes plus 64-bit elements for vreinterpret + vcreate.
(define_mode_iterator VD_RE [V8QI V4HI V2SI V2SF DI])

;; Double-width vector modes plus 64-bit elements.
(define_mode_iterator VDX [V8QI V4HI V4HF V4BF V2SI V2SF DI])

;; Double-width vector modes plus 64-bit elements, including V4BF.
(define_mode_iterator VDXBF [V8QI V4HI V4HF (V4BF "TARGET_BF16_SIMD") V2SI V2SF DI])

;; Double-width vector modes plus 64-bit elements, V4BF and V8BF.
(define_mode_iterator VDXBF2 [V8QI V4HI V4HF V2SI V2SF DI (V4BF "TARGET_BF16_SIMD") (V8BF ("TARGET_BF16_SIMD"))])

;; Double-width vector modes plus 64-bit elements,
;; with V4BFmode added, suitable for moves.
(define_mode_iterator VDXMOV [V8QI V4HI V4HF V4BF V2SI V2SF DI])

;; Double-width vector modes, with V4HF - for vldN_lane and vstN_lane.
(define_mode_iterator VD_LANE [V8QI V4HI V4HF V4BF V2SI V2SF])

;; Double-width vector modes without floating-point elements.
(define_mode_iterator VDI [V8QI V4HI V2SI])

;; Quad-width vector modes supporting arithmetic (no HF!).
(define_mode_iterator VQ [V16QI V8HI V4SI V4SF])

;; Quad-width vector modes, including V8HF.
(define_mode_iterator VQ2 [V16QI V8HI V8HF V4SI V4SF])

;; Quad-width vector modes, including V8BF.
(define_mode_iterator VQ2BF [V16QI V8HI V8HF (V8BF "TARGET_BF16_SIMD") V4SI V4SF])

;; Quad-width vector modes with 16- or 32-bit elements
(define_mode_iterator VQ_HS [V8HI V8HF V4SI V4SF (V8BF "TARGET_BF16_SIMD")])

;; Quad-width vector modes plus 64-bit elements.
(define_mode_iterator VQX [V16QI V8HI V8HF V8BF V4SI V4SF V2DI])

;; Quad-width vector modes plus 64-bit elements.
(define_mode_iterator VQX_NOBF [V16QI V8HI V8HF V4SI V4SF V2DI])

;; Quad-width vector modes plus 64-bit elements and V8BF.
(define_mode_iterator VQXBF [V16QI V8HI V8HF (V8BF "TARGET_BF16_SIMD") V4SI V4SF V2DI])

;; Quad-width vector modes without floating-point elements.
(define_mode_iterator VQI [V16QI V8HI V4SI])

;; Quad-width vector modes, with TImode and V8BFmode added, suitable for moves.
(define_mode_iterator VQXMOV [V16QI V8HI V8HF V8BF V4SI V4SF V2DI TI])

;; Opaque structure types wider than TImode.
(define_mode_iterator VSTRUCT [(EI "!TARGET_HAVE_MVE") OI
			       (CI "!TARGET_HAVE_MVE") XI])

;; Opaque structure types used in table lookups (except vtbl1/vtbx1).
(define_mode_iterator VTAB [TI EI OI])

;; Opaque structure types for x2 variants of VSTR1/VSTR1Q or VLD1/VLD1Q.
(define_mode_iterator VMEMX2 [TI OI])

;; Widenable modes.
(define_mode_iterator VW [V8QI V4HI V2SI])

;; Narrowable modes.
(define_mode_iterator VN [V8HI V4SI V2DI])

;; All supported vector modes (except singleton DImode).
(define_mode_iterator VDQ [V8QI V16QI V4HI V8HI V2SI V4SI V4HF V8HF V2SF V4SF V2DI])

;; All supported floating-point vector modes (except V2DF).
(define_mode_iterator VF [(V4HF "TARGET_NEON_FP16INST")
			   (V8HF "TARGET_NEON_FP16INST") V2SF V4SF])

;; Double vector modes.
(define_mode_iterator VDF [V2SF V4HF])

;; Quad vector Float modes with half/single elements.
(define_mode_iterator VQ_HSF [V8HF V4SF])


;; All supported vector modes (except those with 64-bit integer elements).
(define_mode_iterator VDQW [V8QI V16QI V4HI V8HI V2SI V4SI V2SF V4SF])

;; All supported vector modes including 16-bit float modes.
(define_mode_iterator VDQWH [V8QI V16QI V4HI V8HI V2SI V4SI V2SF V4SF
			     V8HF V4HF])

;; Supported integer vector modes (not 64 bit elements).
(define_mode_iterator VDQIW [V8QI V16QI V4HI V8HI V2SI V4SI])

;; Supported integer vector modes (not singleton DI)
(define_mode_iterator VDQI [V8QI V16QI V4HI V8HI V2SI V4SI V2DI])

;; Vector modes, including 64-bit integer elements.
(define_mode_iterator VDQX [V8QI V16QI V4HI V8HI V2SI V4SI
			    V4HF V8HF V4BF V8BF V2SF V4SF DI V2DI])

;; Vector modes including 64-bit integer elements, but no floats.
(define_mode_iterator VDQIX [V8QI V16QI V4HI V8HI V2SI V4SI DI V2DI])

;; Vector modes for H, S and D types.
(define_mode_iterator VDQHSD [V4HI V8HI V2SI V4SI V2DI])

;; Vector modes for float->int conversions.
(define_mode_iterator VCVTF [V2SF V4SF])

;; Vector modes form int->float conversions.
(define_mode_iterator VCVTI [V2SI V4SI])

;; Vector modes for int->half conversions.
(define_mode_iterator VCVTHI [V4HI V8HI])

;; Vector modes for doubleword multiply-accumulate, etc. insns.
(define_mode_iterator VMD [V4HI V2SI V2SF])

;; Vector modes for quadword multiply-accumulate, etc. insns.
(define_mode_iterator VMQ [V8HI V4SI V4SF])

;; Above modes combined.
(define_mode_iterator VMDQ [V4HI V2SI V2SF V8HI V4SI V4SF])

;; As VMD, but integer modes only.
(define_mode_iterator VMDI [V4HI V2SI])

;; As VMQ, but integer modes only.
(define_mode_iterator VMQI [V8HI V4SI])

;; Above modes combined.
(define_mode_iterator VMDQI [V4HI V2SI V8HI V4SI])

;; Modes with 8-bit and 16-bit elements.
(define_mode_iterator VX [V8QI V4HI V16QI V8HI])

;; Modes with 8-bit elements.
(define_mode_iterator VE [V8QI V16QI])

;; V2DI only (for use with @ patterns).
(define_mode_iterator V2DI_ONLY [V2DI])

;; Modes with 64-bit elements only.
(define_mode_iterator V64 [DI V2DI])

;; Modes with 32-bit elements only.
(define_mode_iterator V32 [V2SI V2SF V4SI V4SF])

;; Modes with 8-bit, 16-bit and 32-bit elements.
(define_mode_iterator VU [V16QI V8HI V4SI])

;; Vector modes for 16-bit floating-point support.
(define_mode_iterator VH [V8HF V4HF])

;; Modes with 16-bit elements only.
(define_mode_iterator V16 [V4HI V4HF V8HI V8HF])

;; 16-bit floating-point vector modes suitable for moving (includes BFmode).
(define_mode_iterator VHFBF [V8HF V4HF V4BF V8BF])

;; 16-bit floating-point vector modes suitable for moving (includes BFmode,
;; without V8HF ).
(define_mode_iterator VHFBF_split [V4HF V4BF V8BF])

;; 16-bit floating-point scalar modes suitable for moving (includes BFmode).
(define_mode_iterator HFBF [HF BF])

;; Iterators used for fixed-point support.
(define_mode_iterator FIXED [QQ HQ SQ UQQ UHQ USQ HA SA UHA USA])

(define_mode_iterator ADDSUB [V4QQ V2HQ V2HA])

(define_mode_iterator UQADDSUB [V4UQQ V2UHQ UQQ UHQ V2UHA UHA])

(define_mode_iterator QADDSUB [V4QQ V2HQ QQ HQ V2HA HA SQ SA])

(define_mode_iterator QMUL [HQ HA])

;; Modes for polynomial or float values.
(define_mode_iterator VPF [V8QI V16QI V2SF V4SF])

;; Modes for BF16 convert instructions.
(define_mode_iterator VBFCVT [V4BF V8BF])
(define_mode_iterator VBFCVTM [V2SI SF])

;; MVE mode iterator.
(define_mode_iterator MVE_types [V16QI V8HI V4SI V2DI TI V8HF V4SF V2DF])
(define_mode_iterator MVE_vecs [V16QI V8HI V4SI V2DI V8HF V4SF V2DF])
(define_mode_iterator MVE_VLD_ST [V16QI V8HI V4SI V8HF V4SF])
(define_mode_iterator MVE_0 [V8HF V4SF])
(define_mode_iterator MVE_1 [V16QI V8HI V4SI V2DI])
(define_mode_iterator MVE_3 [V16QI V8HI])
(define_mode_iterator MVE_2 [V16QI V8HI V4SI])
(define_mode_iterator MVE_5 [V8HI V4SI])
(define_mode_iterator MVE_7 [V16BI V8BI V4BI V2QI])
(define_mode_iterator MVE_7_HI [HI V16BI V8BI V4BI V2QI])
(define_mode_iterator MVE_V8HF [V8HF])
(define_mode_iterator MVE_V16QI [V16QI])

;;----------------------------------------------------------------------------
;; Code iterators
;;----------------------------------------------------------------------------

;; The signed gt, ge comparisons
(define_code_iterator GTGE [gt ge])

;; The signed gt, ge, lt, le comparisons
(define_code_iterator GLTE [gt ge lt le])

;; The unsigned gt, ge comparisons
(define_code_iterator GTUGEU [gtu geu])

;; Comparisons for vc<cmp>
(define_code_iterator COMPARISONS [eq gt ge le lt])
;; Comparisons for MVE
(define_code_iterator MVE_COMPARISONS [eq ge geu gt gtu le lt ne])
(define_code_iterator MVE_FP_COMPARISONS [eq ge gt le lt ne])

;; A list of ...
(define_code_iterator IOR_XOR [ior xor])

(define_code_iterator LOGICAL [and ior xor])

;; Operations on two halves of a quadword vector.
(define_code_iterator VQH_OPS [plus smin smax umin umax])

;; Operations on two halves of a quadword vector,
;; without unsigned variants (for use with *SFmode pattern).
(define_code_iterator VQHS_OPS [plus smin smax])

;; A list of widening operators
(define_code_iterator SE [sign_extend zero_extend])

;; Right shifts
(define_code_iterator RSHIFTS [ashiftrt lshiftrt])

;; Iterator for integer conversions
(define_code_iterator FIXUORS [fix unsigned_fix])

;; Binary operators whose second operand can be shifted.
(define_code_iterator SHIFTABLE_OPS [plus minus ior xor and])

;; Operations on the sign of a number.
(define_code_iterator ABSNEG [abs neg])

;; The PLUS and MINUS operators.
(define_code_iterator PLUSMINUS [plus minus])

;; Conversions.
(define_code_iterator FCVT [unsigned_float float])

;; Saturating addition, subtraction
(define_code_iterator SSPLUSMINUS [ss_plus ss_minus])

;; Max/Min iterator, to factorize MVE patterns
(define_code_iterator MAX_MIN_SU [smax umax smin umin])

;; Floating-point Max/Min iterator, to factorize MVE patterns
(define_code_iterator MAX_MIN_F [smax smin])

;; MVE integer unary operations.
(define_int_iterator MVE_INT_M_UNARY [
		     VABSQ_M_S
		     VCLSQ_M_S
		     VCLZQ_M_S VCLZQ_M_U
		     VNEGQ_M_S
		     VQABSQ_M_S
		     VQNEGQ_M_S
		     ])

(define_int_iterator MVE_INT_UNARY [
		     VCLSQ_S
		     VQABSQ_S
		     VQNEGQ_S
		     ])

(define_int_iterator MVE_FP_UNARY [
		     VRNDQ_F
		     VRNDAQ_F
		     VRNDMQ_F
		     VRNDNQ_F
		     VRNDPQ_F
		     VRNDXQ_F
		     ])

(define_int_iterator MVE_FP_M_UNARY [
		     VABSQ_M_F
		     VNEGQ_M_F
		     VRNDAQ_M_F
		     VRNDMQ_M_F
		     VRNDNQ_M_F
		     VRNDPQ_M_F
		     VRNDQ_M_F
		     VRNDXQ_M_F
		     ])

(define_int_iterator MVE_FP_VREV64Q_ONLY [
		     VREV64Q_F
		     ])

(define_int_iterator MVE_FP_M_VREV64Q_ONLY [
		     VREV64Q_M_F
		     ])

(define_int_iterator MVE_FP_VREV32Q_ONLY [
		     VREV32Q_F
		     ])

(define_int_iterator MVE_FP_M_VREV32Q_ONLY [
		     VREV32Q_M_F
		     ])

(define_int_iterator MVE_FP_M_N_VDUPQ_ONLY [
		     VDUPQ_M_N_F
		     ])

(define_int_iterator MVE_FP_N_VDUPQ_ONLY [
		     VDUPQ_N_F
		     ])

;; MVE integer binary operations.
(define_code_iterator MVE_INT_BINARY_RTX [plus minus mult])

(define_int_iterator MVE_INT_M_BINARY   [
		     VADDQ_M_S VADDQ_M_U
		     VMULQ_M_S VMULQ_M_U
		     VSUBQ_M_S VSUBQ_M_U
		     ])

(define_int_iterator MVE_INT_SU_M_BINARY   [
		     VABDQ_M_S VABDQ_M_U
		     VHADDQ_M_S VHADDQ_M_U
		     VHSUBQ_M_S VHSUBQ_M_U
		     VMAXQ_M_S VMAXQ_M_U
		     VMINQ_M_S VMINQ_M_U
		     VMULHQ_M_S VMULHQ_M_U
		     VQADDQ_M_S VQADDQ_M_U
		     VQDMLADHQ_M_S
		     VQDMLADHXQ_M_S
		     VQDMLSDHQ_M_S
		     VQDMLSDHXQ_M_S
		     VQDMULHQ_M_S
		     VQRDMLADHQ_M_S
		     VQRDMLADHXQ_M_S
		     VQRDMLSDHQ_M_S
		     VQRDMLSDHXQ_M_S
		     VQRDMULHQ_M_S
		     VQRSHLQ_M_S VQRSHLQ_M_U
		     VQSHLQ_M_S VQSHLQ_M_U
		     VQSUBQ_M_S VQSUBQ_M_U
		     VRHADDQ_M_S VRHADDQ_M_U
		     VRMULHQ_M_S VRMULHQ_M_U
		     VRSHLQ_M_S VRSHLQ_M_U
		     VSHLQ_M_S VSHLQ_M_U
		     ])

(define_int_iterator MVE_INT_M_BINARY_LOGIC   [
		     VANDQ_M_S VANDQ_M_U
		     VBICQ_M_S VBICQ_M_U
		     VEORQ_M_S VEORQ_M_U
		     VORRQ_M_S VORRQ_M_U
		     ])

(define_int_iterator MVE_INT_M_N_BINARY [
		     VADDQ_M_N_S VADDQ_M_N_U
		     VMULQ_M_N_S VMULQ_M_N_U
		     VSUBQ_M_N_S VSUBQ_M_N_U
		     ])

(define_int_iterator MVE_INT_M_N_BINARY_LOGIC [
		     VBICQ_M_N_S VBICQ_M_N_U
		     VORRQ_M_N_S VORRQ_M_N_U
		     ])

(define_int_iterator MVE_INT_SU_M_N_BINARY   [
		     VHADDQ_M_N_S VHADDQ_M_N_U
		     VHSUBQ_M_N_S VHSUBQ_M_N_U
		     VMLAQ_M_N_S VMLAQ_M_N_U
		     VMLASQ_M_N_S VMLASQ_M_N_U
		     VQDMLAHQ_M_N_S
		     VQDMLASHQ_M_N_S
		     VQRDMLAHQ_M_N_S
		     VQRDMLASHQ_M_N_S
		     VQADDQ_M_N_S VQADDQ_M_N_U
		     VQSUBQ_M_N_S VQSUBQ_M_N_U
		     VQDMULHQ_M_N_S
		     VQRDMULHQ_M_N_S
		     ])

(define_int_iterator MVE_INT_N_BINARY   [
		     VADDQ_N_S VADDQ_N_U
		     VMULQ_N_S VMULQ_N_U
		     VSUBQ_N_S VSUBQ_N_U
		     ])

(define_int_iterator MVE_VSHRQ_M_N [
		     VRSHRQ_M_N_S VRSHRQ_M_N_U
		     VSHRQ_M_N_S VSHRQ_M_N_U
		     ])

(define_int_iterator MVE_VSHRQ_N [
		     VRSHRQ_N_S VRSHRQ_N_U
		     VSHRQ_N_S VSHRQ_N_U
		     ])

(define_int_iterator MVE_INT_SU_N_BINARY   [
		     VHADDQ_N_S VHADDQ_N_U
		     VHSUBQ_N_S VHSUBQ_N_U
		     VQADDQ_N_S VQADDQ_N_U
		     VQDMULHQ_N_S
		     VQRDMULHQ_N_S
		     VQSUBQ_N_S VQSUBQ_N_U
		     ])

(define_int_iterator MVE_INT_SU_BINARY   [
		     VABDQ_S VABDQ_U
		     VHADDQ_S VHADDQ_U
		     VHSUBQ_S VHSUBQ_U
		     VMULHQ_S VMULHQ_U
		     VQADDQ_S VQADDQ_U
		     VQDMULHQ_S
		     VQRDMULHQ_S
		     VQRSHLQ_S VQRSHLQ_U
		     VQSHLQ_S VQSHLQ_U
		     VQSUBQ_S VQSUBQ_U
		     VRHADDQ_S VRHADDQ_U
		     VRMULHQ_S VRMULHQ_U
		     VRSHLQ_S VRSHLQ_U
		     ])

(define_int_iterator MVE_INT_N_BINARY_LOGIC   [
		     VBICQ_N_S VBICQ_N_U
		     VORRQ_N_S VORRQ_N_U
		     ])

(define_int_iterator MVE_SHIFT_M_R   [
		     VQSHLQ_M_R_S VQSHLQ_M_R_U
		     VSHLQ_M_R_S VSHLQ_M_R_U
		     ])

(define_int_iterator MVE_SHIFT_M_N   [
		     VQSHLQ_M_N_S VQSHLQ_M_N_U
		     VSHLQ_M_N_S VSHLQ_M_N_U
		     ])

(define_int_iterator MVE_SHIFT_N   [
		     VQSHLQ_N_S VQSHLQ_N_U
		     VSHLQ_N_S VSHLQ_N_U
		     ])

(define_int_iterator MVE_SHIFT_R   [
		     VQSHLQ_R_S VQSHLQ_R_U
		     VSHLQ_R_S VSHLQ_R_U
		     ])

(define_int_iterator MVE_RSHIFT_M_N   [
		     VQRSHLQ_M_N_S VQRSHLQ_M_N_U
		     VRSHLQ_M_N_S VRSHLQ_M_N_U
		     ])

(define_int_iterator MVE_RSHIFT_N   [
		     VQRSHLQ_N_S VQRSHLQ_N_U
		     VRSHLQ_N_S VRSHLQ_N_U
		     ])

(define_int_iterator MVE_SHRN_N [
		     VQRSHRNBQ_N_S VQRSHRNBQ_N_U
		     VQRSHRNTQ_N_S VQRSHRNTQ_N_U
		     VQRSHRUNBQ_N_S
		     VQRSHRUNTQ_N_S
		     VQSHRNBQ_N_S VQSHRNBQ_N_U
		     VQSHRNTQ_N_S VQSHRNTQ_N_U
		     VQSHRUNBQ_N_S
		     VQSHRUNTQ_N_S
		     VRSHRNBQ_N_S VRSHRNBQ_N_U
		     VRSHRNTQ_N_S VRSHRNTQ_N_U
		     VSHRNBQ_N_S VSHRNBQ_N_U
		     VSHRNTQ_N_S VSHRNTQ_N_U
		     ])

(define_int_iterator MVE_SHRN_M_N [
		     VQRSHRNBQ_M_N_S VQRSHRNBQ_M_N_U
		     VQRSHRNTQ_M_N_S VQRSHRNTQ_M_N_U
		     VQRSHRUNBQ_M_N_S
		     VQRSHRUNTQ_M_N_S
		     VQSHRNBQ_M_N_S VQSHRNBQ_M_N_U
		     VQSHRNTQ_M_N_S VQSHRNTQ_M_N_U
		     VQSHRUNBQ_M_N_S
		     VQSHRUNTQ_M_N_S
		     VRSHRNBQ_M_N_S VRSHRNBQ_M_N_U
		     VRSHRNTQ_M_N_S VRSHRNTQ_M_N_U
		     VSHRNBQ_M_N_S VSHRNBQ_M_N_U
		     VSHRNTQ_M_N_S VSHRNTQ_M_N_U
		     ])

(define_int_iterator MVE_FP_M_BINARY   [
		     VABDQ_M_F
		     VADDQ_M_F
		     VFMAQ_M_F
		     VFMSQ_M_F
		     VMAXNMQ_M_F
		     VMINNMQ_M_F
		     VMULQ_M_F
		     VSUBQ_M_F
		     ])

(define_int_iterator MVE_FP_M_BINARY_LOGIC   [
		     VANDQ_M_F
		     VBICQ_M_F
		     VEORQ_M_F
		     VORRQ_M_F
		     ])

(define_int_iterator MVE_FP_M_N_BINARY [
		     VADDQ_M_N_F
		     VFMAQ_M_N_F
		     VFMASQ_M_N_F
		     VMULQ_M_N_F
		     VSUBQ_M_N_F
		     ])

(define_int_iterator MVE_FP_N_BINARY   [
		     VADDQ_N_F
		     VMULQ_N_F
		     VSUBQ_N_F
		     ])

(define_int_iterator MVE_FP_VABDQ_ONLY [
		     VABDQ_F
		     ])

(define_int_iterator MVE_FP_CREATE_ONLY [
		     VCREATEQ_F
		     ])

(define_int_iterator MVE_VBRSR_M_N_FP [
		     VBRSRQ_M_N_F
		     ])

(define_int_iterator MVE_VBRSR_N_FP [
		     VBRSRQ_N_F
		     ])

;; MVE comparison iterators
(define_int_iterator MVE_CMP_M [
		     VCMPCSQ_M_U
		     VCMPEQQ_M_S VCMPEQQ_M_U
		     VCMPGEQ_M_S
		     VCMPGTQ_M_S
		     VCMPHIQ_M_U
		     VCMPLEQ_M_S
		     VCMPLTQ_M_S
		     VCMPNEQ_M_S VCMPNEQ_M_U
		     ])

(define_int_iterator MVE_CMP_M_F [
		     VCMPEQQ_M_F
		     VCMPGEQ_M_F
		     VCMPGTQ_M_F
		     VCMPLEQ_M_F
		     VCMPLTQ_M_F
		     VCMPNEQ_M_F
		     ])

(define_int_iterator MVE_CMP_M_N [
		     VCMPCSQ_M_N_U
		     VCMPEQQ_M_N_S VCMPEQQ_M_N_U
		     VCMPGEQ_M_N_S
		     VCMPGTQ_M_N_S
		     VCMPHIQ_M_N_U
		     VCMPLEQ_M_N_S
		     VCMPLTQ_M_N_S
		     VCMPNEQ_M_N_S VCMPNEQ_M_N_U
		     ])

(define_int_iterator MVE_CMP_M_N_F [
		     VCMPEQQ_M_N_F
		     VCMPGEQ_M_N_F
		     VCMPGTQ_M_N_F
		     VCMPLEQ_M_N_F
		     VCMPLTQ_M_N_F
		     VCMPNEQ_M_N_F
		     ])

(define_int_iterator MVE_VFMxQ_F [
		     VFMAQ_F VFMSQ_F
		     ])

(define_int_iterator MVE_VFMAxQ_N_F [
		     VFMAQ_N_F VFMASQ_N_F
		     ])

(define_int_iterator MVE_VMAXVQ_VMINVQ [
		     VMAXAVQ_S
		     VMAXVQ_S VMAXVQ_U
		     VMINAVQ_S
		     VMINVQ_S VMINVQ_U
		     ])

(define_int_iterator MVE_VMAXVQ_VMINVQ_P [
		     VMAXAVQ_P_S
		     VMAXVQ_P_S VMAXVQ_P_U
		     VMINAVQ_P_S
		     VMINVQ_P_S VMINVQ_P_U
		     ])

(define_int_iterator MVE_VMAXNMxV_MINNMxVQ [
		     VMAXNMAVQ_F
		     VMAXNMVQ_F
		     VMINNMAVQ_F
		     VMINNMVQ_F
		     ])

(define_int_iterator MVE_VMAXNMxV_MINNMxVQ_P [
		     VMAXNMAVQ_P_F
		     VMAXNMVQ_P_F
		     VMINNMAVQ_P_F
		     VMINNMVQ_P_F
		     ])

(define_int_iterator MVE_VMAXNMA_VMINNMAQ [
		     VMAXNMAQ_F
		     VMINNMAQ_F
		     ])

(define_int_iterator MVE_VMAXNMA_VMINNMAQ_M [
		     VMAXNMAQ_M_F
		     VMINNMAQ_M_F
		     ])

(define_int_iterator MVE_VMAXAVMINAQ [
		     VMAXAQ_S
		     VMINAQ_S
		     ])

(define_int_iterator MVE_VMAXAVMINAQ_M [
		     VMAXAQ_M_S
		     VMINAQ_M_S
		     ])

(define_int_iterator MVE_VMLxQ_N [
		     VMLAQ_N_S VMLAQ_N_U
		     VMLASQ_N_S VMLASQ_N_U
		     VQDMLAHQ_N_S
		     VQDMLASHQ_N_S
		     VQRDMLAHQ_N_S
		     VQRDMLASHQ_N_S
		     ])

(define_int_iterator MVE_VMLxDAVQ [
		     VMLADAVQ_S VMLADAVQ_U
		     VMLADAVXQ_S
		     VMLSDAVQ_S
		     VMLSDAVXQ_S
		     ])

(define_int_iterator MVE_VMLxDAVQ_P [
		     VMLADAVQ_P_S VMLADAVQ_P_U
		     VMLADAVXQ_P_S
		     VMLSDAVQ_P_S
		     VMLSDAVXQ_P_S
		     ])

(define_int_iterator MVE_VMLxDAVAQ [
		     VMLADAVAQ_S VMLADAVAQ_U
		     VMLSDAVAXQ_S
		     VMLSDAVAQ_S
		     VMLADAVAXQ_S
		     ])

(define_int_iterator MVE_VMLxDAVAQ_P [
		     VMLADAVAQ_P_S VMLADAVAQ_P_U
		     VMLSDAVAXQ_P_S
		     VMLSDAVAQ_P_S
		     VMLADAVAXQ_P_S
		     ])

(define_int_iterator MVE_VMLxLDAVxQ [
		     VMLALDAVQ_S VMLALDAVQ_U
		     VMLALDAVXQ_S
		     VMLSLDAVQ_S
		     VMLSLDAVXQ_S
		     ])

(define_int_iterator MVE_VMLxLDAVxQ_P [
		     VMLALDAVQ_P_S VMLALDAVQ_P_U
		     VMLALDAVXQ_P_S
		     VMLSLDAVQ_P_S
		     VMLSLDAVXQ_P_S
		     ])

(define_int_iterator MVE_VMLxLDAVAxQ [
		     VMLALDAVAQ_S VMLALDAVAQ_U
		     VMLALDAVAXQ_S
		     VMLSLDAVAQ_S
		     VMLSLDAVAXQ_S
		     ])

(define_int_iterator MVE_VMLxLDAVAxQ_P [
		     VMLALDAVAQ_P_S VMLALDAVAQ_P_U
		     VMLALDAVAXQ_P_S
		     VMLSLDAVAQ_P_S
		     VMLSLDAVAXQ_P_S
		     ])

(define_int_iterator MVE_VQDMULLxQ [
		     VQDMULLBQ_S
		     VQDMULLTQ_S
		     ])

(define_int_iterator MVE_VQDMULLxQ_M [
		     VQDMULLBQ_M_S
		     VQDMULLTQ_M_S
		     ])

(define_int_iterator MVE_VQDMULLxQ_M_N [
		     VQDMULLBQ_M_N_S
		     VQDMULLTQ_M_N_S
		     ])

(define_int_iterator MVE_VQDMULLxQ_N [
		     VQDMULLBQ_N_S
		     VQDMULLTQ_N_S
		     ])

(define_int_iterator MVE_VQxDMLxDHxQ_S [
		     VQDMLADHQ_S
		     VQDMLADHXQ_S
		     VQDMLSDHQ_S
		     VQDMLSDHXQ_S
		     VQRDMLADHQ_S
		     VQRDMLADHXQ_S
		     VQRDMLSDHQ_S
		     VQRDMLSDHXQ_S
		     ])

(define_int_iterator MVE_VRMLxLDAVxQ [
		     VRMLALDAVHQ_S VRMLALDAVHQ_U
		     VRMLALDAVHXQ_S
		     VRMLSLDAVHQ_S
		     VRMLSLDAVHXQ_S
		     ])

(define_int_iterator MVE_VRMLxLDAVHxQ_P [
		     VRMLALDAVHQ_P_S VRMLALDAVHQ_P_U
		     VRMLALDAVHXQ_P_S
		     VRMLSLDAVHQ_P_S
		     VRMLSLDAVHXQ_P_S
		     ])

(define_int_iterator MVE_VRMLxLDAVHAxQ [
		     VRMLALDAVHAQ_S VRMLALDAVHAQ_U
		     VRMLALDAVHAXQ_S
		     VRMLSLDAVHAQ_S
		     VRMLSLDAVHAXQ_S
		     ])

(define_int_iterator MVE_VRMLxLDAVHAxQ_P [
		     VRMLALDAVHAQ_P_S VRMLALDAVHAQ_P_U
		     VRMLALDAVHAXQ_P_S
		     VRMLSLDAVHAQ_P_S
		     VRMLSLDAVHAXQ_P_S
		     ])

(define_int_iterator MVE_MOVN [
		     VMOVNBQ_S VMOVNBQ_U
		     VMOVNTQ_S VMOVNTQ_U
		     VQMOVNBQ_S VQMOVNBQ_U
		     VQMOVNTQ_S VQMOVNTQ_U
		     VQMOVUNBQ_S
		     VQMOVUNTQ_S
		     ])

(define_int_iterator MVE_MOVN_M [
		     VMOVNBQ_M_S VMOVNBQ_M_U
		     VMOVNTQ_M_S VMOVNTQ_M_U
		     VQMOVNBQ_M_S VQMOVNBQ_M_U
		     VQMOVNTQ_M_S VQMOVNTQ_M_U
		     VQMOVUNBQ_M_S
		     VQMOVUNTQ_M_S
		     ])

(define_code_attr mve_addsubmul [
		 (minus "vsub")
		 (mult "vmul")
		 (plus "vadd")
		 ])

(define_int_attr mve_vmaxmin_safe_imp [
		 (VMAXVQ_U "yes")
		 (VMAXVQ_S "no")
		 (VMAXAVQ_S "yes")
		 (VMINVQ_U "no")
		 (VMINVQ_S "no")
		 (VMINAVQ_S "no")])

(define_int_attr mve_cmp_op1 [
		 (VCMPCSQ_M_U "cs")
		 (VCMPEQQ_M_S "eq") (VCMPEQQ_M_U "eq")
		 (VCMPGEQ_M_S "ge")
		 (VCMPGTQ_M_S "gt")
		 (VCMPHIQ_M_U "hi")
		 (VCMPLEQ_M_S "le")
		 (VCMPLTQ_M_S "lt")
		 (VCMPNEQ_M_S "ne") (VCMPNEQ_M_U "ne")
		 (VCMPEQQ_M_F "eq")
		 (VCMPGEQ_M_F "ge")
		 (VCMPGTQ_M_F "gt")
		 (VCMPLEQ_M_F "le")
		 (VCMPLTQ_M_F "lt")
		 (VCMPNEQ_M_F "ne")
		 (VCMPCSQ_M_N_U "cs")
		 (VCMPEQQ_M_N_S "eq") (VCMPEQQ_M_N_U "eq")
		 (VCMPGEQ_M_N_S "ge")
		 (VCMPGTQ_M_N_S "gt")
		 (VCMPHIQ_M_N_U "hi")
		 (VCMPLEQ_M_N_S "le")
		 (VCMPLTQ_M_N_S "lt")
		 (VCMPNEQ_M_N_S "ne") (VCMPNEQ_M_N_U "ne")
		 (VCMPEQQ_M_N_F "eq")
		 (VCMPGEQ_M_N_F "ge")
		 (VCMPGTQ_M_N_F "gt")
		 (VCMPLEQ_M_N_F "le")
		 (VCMPLTQ_M_N_F "lt")
		 (VCMPNEQ_M_N_F "ne")
		 ])

(define_int_iterator MVE_VPSELQ_F [
		     VPSELQ_F
		     ])

(define_int_iterator MVE_VCADDQ_VCMULQ [
		     UNSPEC_VCADD90 UNSPEC_VCADD270
		     UNSPEC_VCMUL UNSPEC_VCMUL90 UNSPEC_VCMUL180 UNSPEC_VCMUL270
		     ])

(define_int_iterator MVE_VCADDQ_VCMULQ_M [
		     VCADDQ_ROT90_M_F VCADDQ_ROT270_M_F
		     VCMULQ_M_F VCMULQ_ROT90_M_F VCMULQ_ROT180_M_F VCMULQ_ROT270_M_F
		     ])

(define_int_iterator MVE_VCMLAQ_M [
		     VCMLAQ_M_F VCMLAQ_ROT90_M_F VCMLAQ_ROT180_M_F VCMLAQ_ROT270_M_F
		     ])

(define_int_attr mve_insn [
		 (UNSPEC_VCADD90 "vcadd") (UNSPEC_VCADD270 "vcadd")
		 (UNSPEC_VCMLA "vcmla") (UNSPEC_VCMLA90 "vcmla") (UNSPEC_VCMLA180 "vcmla") (UNSPEC_VCMLA270 "vcmla")
		 (UNSPEC_VCMUL "vcmul") (UNSPEC_VCMUL90 "vcmul") (UNSPEC_VCMUL180 "vcmul") (UNSPEC_VCMUL270 "vcmul")
		 (VABAVQ_P_S "vabav") (VABAVQ_P_U "vabav")
		 (VABAVQ_S "vabav") (VABAVQ_U "vabav")
		 (VABDQ_M_S "vabd") (VABDQ_M_U "vabd") (VABDQ_M_F "vabd")
		 (VABDQ_S "vabd") (VABDQ_U "vabd") (VABDQ_F "vabd")
		 (VABSQ_M_F "vabs")
		 (VABSQ_M_S "vabs")
		 (VADDLVAQ_P_S "vaddlva") (VADDLVAQ_P_U "vaddlva")
		 (VADDLVAQ_S "vaddlva") (VADDLVAQ_U "vaddlva")
		 (VADDLVQ_P_S "vaddlv") (VADDLVQ_P_U "vaddlv")
		 (VADDLVQ_S "vaddlv") (VADDLVQ_U "vaddlv")
		 (VADDQ_M_N_S "vadd") (VADDQ_M_N_U "vadd") (VADDQ_M_N_F "vadd")
		 (VADDQ_M_S "vadd") (VADDQ_M_U "vadd") (VADDQ_M_F "vadd")
		 (VADDQ_N_S "vadd") (VADDQ_N_U "vadd") (VADDQ_N_F "vadd")
		 (VADDVAQ_P_S "vaddva") (VADDVAQ_P_U "vaddva")
		 (VADDVAQ_S "vaddva") (VADDVAQ_U "vaddva")
		 (VADDVQ_P_S "vaddv") (VADDVQ_P_U "vaddv")
		 (VADDVQ_S "vaddv") (VADDVQ_U "vaddv")
		 (VANDQ_M_S "vand") (VANDQ_M_U "vand") (VANDQ_M_F "vand")
		 (VBICQ_M_N_S "vbic") (VBICQ_M_N_U "vbic")
		 (VBICQ_M_S "vbic") (VBICQ_M_U "vbic") (VBICQ_M_F "vbic")
		 (VBICQ_N_S "vbic") (VBICQ_N_U "vbic")
		 (VBRSRQ_M_N_S "vbrsr") (VBRSRQ_M_N_U "vbrsr") (VBRSRQ_M_N_F "vbrsr")
		 (VBRSRQ_N_S "vbrsr") (VBRSRQ_N_U "vbrsr") (VBRSRQ_N_F "vbrsr")
		 (VCADDQ_ROT270_M "vcadd") (VCADDQ_ROT270_M_F "vcadd")
		 (VCADDQ_ROT90_M "vcadd") (VCADDQ_ROT90_M_F "vcadd")
		 (VCLSQ_M_S "vcls")
		 (VCLSQ_S "vcls")
		 (VCLZQ_M_S "vclz") (VCLZQ_M_U "vclz")
		 (VCMLAQ_M_F "vcmla") (VCMLAQ_ROT90_M_F "vcmla") (VCMLAQ_ROT180_M_F "vcmla") (VCMLAQ_ROT270_M_F "vcmla")
		 (VCMULQ_M_F "vcmul") (VCMULQ_ROT90_M_F "vcmul") (VCMULQ_ROT180_M_F "vcmul") (VCMULQ_ROT270_M_F "vcmul")
		 (VCREATEQ_S "vcreate") (VCREATEQ_U "vcreate") (VCREATEQ_F "vcreate")
		 (VDUPQ_M_N_S "vdup") (VDUPQ_M_N_U "vdup") (VDUPQ_M_N_F "vdup")
		 (VDUPQ_N_S "vdup") (VDUPQ_N_U "vdup") (VDUPQ_N_F "vdup")
		 (VEORQ_M_S "veor") (VEORQ_M_U "veor") (VEORQ_M_F "veor")
		 (VFMAQ_F "vfma")
		 (VFMAQ_M_F "vfma")
		 (VFMAQ_M_N_F "vfma")
		 (VFMAQ_N_F "vfma")
		 (VFMASQ_M_N_F "vfmas")
		 (VFMASQ_N_F "vfmas")
		 (VFMSQ_F "vfms")
		 (VFMSQ_M_F "vfms")
		 (VHADDQ_M_N_S "vhadd") (VHADDQ_M_N_U "vhadd")
		 (VHADDQ_M_S "vhadd") (VHADDQ_M_U "vhadd")
		 (VHADDQ_N_S "vhadd") (VHADDQ_N_U "vhadd")
		 (VHADDQ_S "vhadd") (VHADDQ_U "vhadd")
		 (VHCADDQ_ROT90_M_S "vhcadd") (VHCADDQ_ROT270_M_S "vhcadd")
		 (VHCADDQ_ROT90_S "vhcadd") (VHCADDQ_ROT270_S "vhcadd")
		 (VHSUBQ_M_N_S "vhsub") (VHSUBQ_M_N_U "vhsub")
		 (VHSUBQ_M_S "vhsub") (VHSUBQ_M_U "vhsub")
		 (VHSUBQ_N_S "vhsub") (VHSUBQ_N_U "vhsub")
		 (VHSUBQ_S "vhsub") (VHSUBQ_U "vhsub")
		 (VMAXAQ_M_S "vmaxa")
		 (VMAXAQ_S "vmaxa")
		 (VMAXAVQ_P_S "vmaxav")
		 (VMAXAVQ_S "vmaxav")
		 (VMAXNMAQ_F "vmaxnma")
		 (VMAXNMAQ_M_F "vmaxnma")
		 (VMAXNMAVQ_F "vmaxnmav")
		 (VMAXNMAVQ_P_F "vmaxnmav")
		 (VMAXNMQ_M_F "vmaxnm")
		 (VMAXNMVQ_F "vmaxnmv")
		 (VMAXNMVQ_P_F "vmaxnmv")
		 (VMAXQ_M_S "vmax") (VMAXQ_M_U "vmax")
		 (VMAXVQ_P_S "vmaxv") (VMAXVQ_P_U "vmaxv")
		 (VMAXVQ_S "vmaxv") (VMAXVQ_U "vmaxv")
		 (VMINAQ_M_S "vmina")
		 (VMINAQ_S "vmina")
		 (VMINAVQ_P_S "vminav")
		 (VMINAVQ_S "vminav")
		 (VMINNMAQ_F "vminnma")
		 (VMINNMAQ_M_F "vminnma")
		 (VMINNMAVQ_F "vminnmav")
		 (VMINNMAVQ_P_F "vminnmav")
		 (VMINNMQ_M_F "vminnm")
		 (VMINNMVQ_F "vminnmv")
		 (VMINNMVQ_P_F "vminnmv")
		 (VMINQ_M_S "vmin") (VMINQ_M_U "vmin")
		 (VMINVQ_P_S "vminv") (VMINVQ_P_U "vminv")
		 (VMINVQ_S "vminv") (VMINVQ_U "vminv")
		 (VMLADAVAQ_P_S "vmladava") (VMLADAVAQ_P_U "vmladava")
		 (VMLADAVAQ_S "vmladava") (VMLADAVAQ_U "vmladava")
		 (VMLADAVAXQ_P_S "vmladavax")
		 (VMLADAVAXQ_S "vmladavax")
		 (VMLADAVQ_P_S "vmladav") (VMLADAVQ_P_U "vmladav")
		 (VMLADAVQ_S "vmladav") (VMLADAVQ_U "vmladav")
		 (VMLADAVXQ_P_S "vmladavx")
		 (VMLADAVXQ_S "vmladavx")
		 (VMLALDAVAQ_P_S "vmlaldava") (VMLALDAVAQ_P_U "vmlaldava")
		 (VMLALDAVAQ_S "vmlaldava") (VMLALDAVAQ_U "vmlaldava")
		 (VMLALDAVAXQ_P_S "vmlaldavax")
		 (VMLALDAVAXQ_S "vmlaldavax")
		 (VMLALDAVQ_P_S "vmlaldav") (VMLALDAVQ_P_U "vmlaldav")
		 (VMLALDAVQ_S "vmlaldav") (VMLALDAVQ_U "vmlaldav")
		 (VMLALDAVXQ_P_S "vmlaldavx")
		 (VMLALDAVXQ_S "vmlaldavx")
		 (VMLAQ_M_N_S "vmla") (VMLAQ_M_N_U "vmla")
		 (VMLAQ_N_S "vmla") (VMLAQ_N_U "vmla")
		 (VMLASQ_M_N_S "vmlas") (VMLASQ_M_N_U "vmlas")
		 (VMLASQ_N_S "vmlas") (VMLASQ_N_U "vmlas")
		 (VMLSDAVAQ_P_S "vmlsdava")
		 (VMLSDAVAQ_S "vmlsdava")
		 (VMLSDAVAXQ_P_S "vmlsdavax")
		 (VMLSDAVAXQ_S "vmlsdavax")
		 (VMLSDAVQ_P_S "vmlsdav")
		 (VMLSDAVQ_S "vmlsdav")
		 (VMLSDAVXQ_P_S "vmlsdavx")
		 (VMLSDAVXQ_S "vmlsdavx")
		 (VMLSLDAVAQ_P_S "vmlsldava")
		 (VMLSLDAVAQ_S "vmlsldava")
		 (VMLSLDAVAXQ_P_S "vmlsldavax")
		 (VMLSLDAVAXQ_S "vmlsldavax")
		 (VMLSLDAVQ_P_S "vmlsldav")
		 (VMLSLDAVQ_S "vmlsldav")
		 (VMLSLDAVXQ_P_S "vmlsldavx")
		 (VMLSLDAVXQ_S "vmlsldavx")
		 (VMOVLBQ_M_S "vmovlb") (VMOVLBQ_M_U "vmovlb")
		 (VMOVLBQ_S "vmovlb") (VMOVLBQ_U "vmovlb")
		 (VMOVLTQ_M_S "vmovlt") (VMOVLTQ_M_U "vmovlt")
		 (VMOVLTQ_S "vmovlt") (VMOVLTQ_U "vmovlt")
		 (VMOVNBQ_M_S "vmovnb") (VMOVNBQ_M_U "vmovnb")
		 (VMOVNBQ_S "vmovnb") (VMOVNBQ_U "vmovnb")
		 (VMOVNTQ_M_S "vmovnt") (VMOVNTQ_M_U "vmovnt")
		 (VMOVNTQ_S "vmovnt") (VMOVNTQ_U "vmovnt")
		 (VMULHQ_M_S "vmulh") (VMULHQ_M_U "vmulh")
		 (VMULHQ_S "vmulh") (VMULHQ_U "vmulh")
		 (VMULLBQ_INT_M_S "vmullb") (VMULLBQ_INT_M_U "vmullb")
		 (VMULLBQ_INT_S "vmullb") (VMULLBQ_INT_U "vmullb")
		 (VMULLBQ_POLY_M_P "vmullb") (VMULLTQ_POLY_M_P "vmullt")
		 (VMULLBQ_POLY_P "vmullb")
		 (VMULLTQ_INT_M_S "vmullt") (VMULLTQ_INT_M_U "vmullt")
		 (VMULLTQ_INT_S "vmullt") (VMULLTQ_INT_U "vmullt")
		 (VMULLTQ_POLY_P "vmullt")
		 (VMULQ_M_N_S "vmul") (VMULQ_M_N_U "vmul") (VMULQ_M_N_F "vmul")
		 (VMULQ_M_S "vmul") (VMULQ_M_U "vmul") (VMULQ_M_F "vmul")
		 (VMULQ_N_S "vmul") (VMULQ_N_U "vmul") (VMULQ_N_F "vmul")
		 (VMVNQ_M_N_S "vmvn") (VMVNQ_M_N_U "vmvn")
		 (VMVNQ_M_S "vmvn") (VMVNQ_M_U "vmvn")
		 (VMVNQ_N_S "vmvn") (VMVNQ_N_U "vmvn")
		 (VNEGQ_M_F "vneg")
		 (VNEGQ_M_S "vneg")
		 (VORRQ_M_N_S "vorr") (VORRQ_M_N_U "vorr")
		 (VORRQ_M_S "vorr") (VORRQ_M_U "vorr") (VORRQ_M_F "vorr")
		 (VORRQ_N_S "vorr") (VORRQ_N_U "vorr")
		 (VPSELQ_S "vpsel") (VPSELQ_U "vpsel") (VPSELQ_F "vpsel")
		 (VQABSQ_M_S "vqabs")
		 (VQABSQ_S "vqabs")
		 (VQADDQ_M_N_S "vqadd") (VQADDQ_M_N_U "vqadd")
		 (VQADDQ_M_S "vqadd") (VQADDQ_M_U "vqadd")
		 (VQADDQ_N_S "vqadd") (VQADDQ_N_U "vqadd")
		 (VQADDQ_S "vqadd") (VQADDQ_U "vqadd")
		 (VQDMLADHQ_M_S "vqdmladh")
		 (VQDMLADHQ_S "vqdmladh")
		 (VQDMLADHXQ_M_S "vqdmladhx")
		 (VQDMLADHXQ_S "vqdmladhx")
		 (VQDMLAHQ_M_N_S "vqdmlah")
		 (VQDMLAHQ_N_S "vqdmlah")
		 (VQDMLASHQ_M_N_S "vqdmlash")
		 (VQDMLASHQ_N_S "vqdmlash")
		 (VQDMLSDHQ_M_S "vqdmlsdh")
		 (VQDMLSDHQ_S "vqdmlsdh")
		 (VQDMLSDHXQ_M_S "vqdmlsdhx")
		 (VQDMLSDHXQ_S "vqdmlsdhx")
		 (VQDMULHQ_M_N_S "vqdmulh")
		 (VQDMULHQ_M_S "vqdmulh")
		 (VQDMULHQ_N_S "vqdmulh")
		 (VQDMULHQ_S "vqdmulh")
		 (VQDMULLBQ_M_N_S "vqdmullb")
		 (VQDMULLBQ_M_S "vqdmullb")
		 (VQDMULLBQ_N_S "vqdmullb")
		 (VQDMULLBQ_S "vqdmullb")
		 (VQDMULLTQ_M_N_S "vqdmullt")
		 (VQDMULLTQ_M_S "vqdmullt")
		 (VQDMULLTQ_N_S "vqdmullt")
		 (VQDMULLTQ_S "vqdmullt")
		 (VQMOVNBQ_M_S "vqmovnb") (VQMOVNBQ_M_U "vqmovnb")
		 (VQMOVNBQ_S "vqmovnb") (VQMOVNBQ_U "vqmovnb")
		 (VQMOVNTQ_M_S "vqmovnt") (VQMOVNTQ_M_U "vqmovnt")
		 (VQMOVNTQ_S "vqmovnt") (VQMOVNTQ_U "vqmovnt")
		 (VQMOVUNBQ_M_S "vqmovunb")
		 (VQMOVUNBQ_S "vqmovunb")
		 (VQMOVUNTQ_M_S "vqmovunt")
		 (VQMOVUNTQ_S "vqmovunt")
		 (VQNEGQ_M_S "vqneg")
		 (VQNEGQ_S "vqneg")
		 (VQRDMLADHQ_M_S "vqrdmladh")
		 (VQRDMLADHQ_S "vqrdmladh")
		 (VQRDMLADHXQ_M_S "vqrdmladhx")
		 (VQRDMLADHXQ_S "vqrdmladhx")
		 (VQRDMLAHQ_M_N_S "vqrdmlah")
		 (VQRDMLAHQ_N_S "vqrdmlah")
		 (VQRDMLASHQ_M_N_S "vqrdmlash")
		 (VQRDMLASHQ_N_S "vqrdmlash")
		 (VQRDMLSDHQ_M_S "vqrdmlsdh")
		 (VQRDMLSDHQ_S "vqrdmlsdh")
		 (VQRDMLSDHXQ_M_S "vqrdmlsdhx")
		 (VQRDMLSDHXQ_S "vqrdmlsdhx")
		 (VQRDMULHQ_M_N_S "vqrdmulh")
		 (VQRDMULHQ_M_S "vqrdmulh")
		 (VQRDMULHQ_N_S "vqrdmulh")
		 (VQRDMULHQ_S "vqrdmulh")
		 (VQRSHLQ_M_N_S "vqrshl") (VQRSHLQ_M_N_U "vqrshl")
		 (VQRSHLQ_M_S "vqrshl") (VQRSHLQ_M_U "vqrshl")
		 (VQRSHLQ_N_S "vqrshl") (VQRSHLQ_N_U "vqrshl")
		 (VQRSHLQ_S "vqrshl") (VQRSHLQ_U "vqrshl")
		 (VQRSHRNBQ_M_N_S "vqrshrnb") (VQRSHRNBQ_M_N_U "vqrshrnb")
		 (VQRSHRNBQ_N_S "vqrshrnb") (VQRSHRNBQ_N_U "vqrshrnb")
		 (VQRSHRNTQ_M_N_S "vqrshrnt") (VQRSHRNTQ_M_N_U "vqrshrnt")
		 (VQRSHRNTQ_N_S "vqrshrnt") (VQRSHRNTQ_N_U "vqrshrnt")
		 (VQRSHRUNBQ_M_N_S "vqrshrunb")
		 (VQRSHRUNBQ_N_S "vqrshrunb")
		 (VQRSHRUNTQ_M_N_S "vqrshrunt")
		 (VQRSHRUNTQ_N_S "vqrshrunt")
		 (VQSHLQ_M_N_S "vqshl") (VQSHLQ_M_N_U "vqshl")
		 (VQSHLQ_M_R_S "vqshl") (VQSHLQ_M_R_U "vqshl")
		 (VQSHLQ_M_S "vqshl") (VQSHLQ_M_U "vqshl")
		 (VQSHLQ_N_S "vqshl") (VQSHLQ_N_U "vqshl")
		 (VQSHLQ_R_S "vqshl") (VQSHLQ_R_U "vqshl")
		 (VQSHLQ_S "vqshl") (VQSHLQ_U "vqshl")
		 (VQSHLUQ_M_N_S "vqshlu")
		 (VQSHLUQ_N_S "vqshlu")
		 (VQSHRNBQ_M_N_S "vqshrnb") (VQSHRNBQ_M_N_U "vqshrnb")
		 (VQSHRNBQ_N_S "vqshrnb") (VQSHRNBQ_N_U "vqshrnb")
		 (VQSHRNTQ_M_N_S "vqshrnt") (VQSHRNTQ_M_N_U "vqshrnt")
		 (VQSHRNTQ_N_S "vqshrnt") (VQSHRNTQ_N_U "vqshrnt")
		 (VQSHRUNBQ_M_N_S "vqshrunb")
		 (VQSHRUNBQ_N_S "vqshrunb")
		 (VQSHRUNTQ_M_N_S "vqshrunt")
		 (VQSHRUNTQ_N_S "vqshrunt")
		 (VQSUBQ_M_N_S "vqsub") (VQSUBQ_M_N_U "vqsub")
		 (VQSUBQ_M_S "vqsub") (VQSUBQ_M_U "vqsub")
		 (VQSUBQ_N_S "vqsub") (VQSUBQ_N_U "vqsub")
		 (VQSUBQ_S "vqsub") (VQSUBQ_U "vqsub")
		 (VREV16Q_M_S "vrev16") (VREV16Q_M_U "vrev16")
		 (VREV16Q_S "vrev16") (VREV16Q_U "vrev16")
		 (VREV32Q_M_S "vrev32") (VREV32Q_M_U "vrev32") (VREV32Q_M_F "vrev32")
		 (VREV32Q_S "vrev32") (VREV32Q_U "vrev32") (VREV32Q_F "vrev32")
		 (VREV64Q_M_S "vrev64") (VREV64Q_M_U "vrev64") (VREV64Q_M_F "vrev64")
		 (VREV64Q_S "vrev64") (VREV64Q_U "vrev64") (VREV64Q_F "vrev64")
		 (VRHADDQ_M_S "vrhadd") (VRHADDQ_M_U "vrhadd")
		 (VRHADDQ_S "vrhadd") (VRHADDQ_U "vrhadd")
		 (VRMLALDAVHAQ_P_S "vrmlaldavha") (VRMLALDAVHAQ_P_U "vrmlaldavha")
		 (VRMLALDAVHAQ_S "vrmlaldavha") (VRMLALDAVHAQ_U "vrmlaldavha")
		 (VRMLALDAVHAXQ_P_S "vrmlaldavhax")
		 (VRMLALDAVHAXQ_S "vrmlaldavhax")
		 (VRMLALDAVHQ_P_S "vrmlaldavh") (VRMLALDAVHQ_P_U "vrmlaldavh")
		 (VRMLALDAVHQ_S "vrmlaldavh") (VRMLALDAVHQ_U "vrmlaldavh")
		 (VRMLALDAVHXQ_P_S "vrmlaldavhx")
		 (VRMLALDAVHXQ_S "vrmlaldavhx")
		 (VRMLSLDAVHAQ_P_S "vrmlsldavha")
		 (VRMLSLDAVHAQ_S "vrmlsldavha")
		 (VRMLSLDAVHAXQ_P_S "vrmlsldavhax")
		 (VRMLSLDAVHAXQ_S "vrmlsldavhax")
		 (VRMLSLDAVHQ_P_S "vrmlsldavh")
		 (VRMLSLDAVHQ_S "vrmlsldavh")
		 (VRMLSLDAVHXQ_P_S "vrmlsldavhx")
		 (VRMLSLDAVHXQ_S "vrmlsldavhx")
		 (VRMULHQ_M_S "vrmulh") (VRMULHQ_M_U "vrmulh")
		 (VRMULHQ_S "vrmulh") (VRMULHQ_U "vrmulh")
		 (VRNDAQ_F "vrnda") (VRNDAQ_M_F "vrnda")
		 (VRNDMQ_F "vrndm") (VRNDMQ_M_F "vrndm")
		 (VRNDNQ_F "vrndn") (VRNDNQ_M_F "vrndn")
		 (VRNDPQ_F "vrndp") (VRNDPQ_M_F "vrndp")
		 (VRNDQ_F "vrnd") (VRNDQ_M_F "vrnd")
		 (VRNDXQ_F "vrndx") (VRNDXQ_M_F "vrndx")
		 (VRSHLQ_M_N_S "vrshl") (VRSHLQ_M_N_U "vrshl")
		 (VRSHLQ_M_S "vrshl") (VRSHLQ_M_U "vrshl")
		 (VRSHLQ_N_S "vrshl") (VRSHLQ_N_U "vrshl")
		 (VRSHLQ_S "vrshl") (VRSHLQ_U "vrshl")
		 (VRSHRNBQ_M_N_S "vrshrnb") (VRSHRNBQ_M_N_U "vrshrnb")
		 (VRSHRNBQ_N_S "vrshrnb") (VRSHRNBQ_N_U "vrshrnb")
		 (VRSHRNTQ_M_N_S "vrshrnt") (VRSHRNTQ_M_N_U "vrshrnt")
		 (VRSHRNTQ_N_S "vrshrnt") (VRSHRNTQ_N_U "vrshrnt")
		 (VRSHRQ_M_N_S "vrshr") (VRSHRQ_M_N_U "vrshr")
		 (VRSHRQ_N_S "vrshr") (VRSHRQ_N_U "vrshr")
		 (VSHLLBQ_M_N_S "vshllb") (VSHLLBQ_M_N_U "vshllb")
		 (VSHLLBQ_N_S "vshllb") (VSHLLBQ_N_U "vshllb")
		 (VSHLLTQ_M_N_S "vshllt") (VSHLLTQ_M_N_U "vshllt")
		 (VSHLLTQ_N_S "vshllt") (VSHLLTQ_N_U "vshllt")
		 (VSHLQ_M_N_S "vshl") (VSHLQ_M_N_U "vshl")
		 (VSHLQ_M_R_S "vshl") (VSHLQ_M_R_U "vshl")
		 (VSHLQ_M_S "vshl") (VSHLQ_M_U "vshl")
		 (VSHLQ_N_S "vshl") (VSHLQ_N_U "vshl")
		 (VSHLQ_R_S "vshl") (VSHLQ_R_U "vshl")
		 (VSHLQ_S "vshl") (VSHLQ_U "vshl")
		 (VSHRNBQ_M_N_S "vshrnb") (VSHRNBQ_M_N_U "vshrnb")
		 (VSHRNBQ_N_S "vshrnb") (VSHRNBQ_N_U "vshrnb")
		 (VSHRNTQ_M_N_S "vshrnt") (VSHRNTQ_M_N_U "vshrnt")
		 (VSHRNTQ_N_S "vshrnt") (VSHRNTQ_N_U "vshrnt")
		 (VSHRQ_M_N_S "vshr") (VSHRQ_M_N_U "vshr")
		 (VSHRQ_N_S "vshr") (VSHRQ_N_U "vshr")
		 (VSLIQ_M_N_S "vsli") (VSLIQ_M_N_U "vsli")
		 (VSLIQ_N_S "vsli") (VSLIQ_N_U "vsli")
		 (VSRIQ_M_N_S "vsri") (VSRIQ_M_N_U "vsri")
		 (VSRIQ_N_S "vsri") (VSRIQ_N_U "vsri")
		 (VSUBQ_M_N_S "vsub") (VSUBQ_M_N_U "vsub") (VSUBQ_M_N_F "vsub")
		 (VSUBQ_M_S "vsub") (VSUBQ_M_U "vsub") (VSUBQ_M_F "vsub")
		 (VSUBQ_N_S "vsub") (VSUBQ_N_U "vsub") (VSUBQ_N_F "vsub")
		 ])

(define_int_attr isu    [
		 (UNSPEC_VCADD90 "i") (UNSPEC_VCADD270 "i")
		 (VABSQ_M_S "s")
		 (VCADDQ_ROT270_M "i")
		 (VCADDQ_ROT90_M "i")
		 (VCLSQ_M_S "s")
		 (VCLZQ_M_S "i")
		 (VCLZQ_M_U "i")
		 (VCMPCSQ_M_N_U "u")
		 (VCMPCSQ_M_U "u")
		 (VCMPEQQ_M_N_S "i")
		 (VCMPEQQ_M_N_U "i")
		 (VCMPEQQ_M_S "i")
		 (VCMPEQQ_M_U "i")
		 (VCMPGEQ_M_N_S "s")
		 (VCMPGEQ_M_S "s")
		 (VCMPGTQ_M_N_S "s")
		 (VCMPGTQ_M_S "s")
		 (VCMPHIQ_M_N_U "u")
		 (VCMPHIQ_M_U "u")
		 (VCMPLEQ_M_N_S "s")
		 (VCMPLEQ_M_S "s")
		 (VCMPLTQ_M_N_S "s")
		 (VCMPLTQ_M_S "s")
		 (VCMPNEQ_M_N_S "i")
		 (VCMPNEQ_M_N_U "i")
		 (VCMPNEQ_M_S "i")
		 (VCMPNEQ_M_U "i")
		 (VHCADDQ_ROT90_M_S "s") (VHCADDQ_ROT270_M_S "s")
		 (VHCADDQ_ROT90_S "s") (VHCADDQ_ROT270_S "s")
		 (VMOVNBQ_M_S "i") (VMOVNBQ_M_U "i")
		 (VMOVNBQ_S "i") (VMOVNBQ_U "i")
		 (VMOVNTQ_M_S "i") (VMOVNTQ_M_U "i")
		 (VMOVNTQ_S "i") (VMOVNTQ_U "i")
		 (VMULLBQ_INT_S "s") (VMULLBQ_INT_U "u")
		 (VMULLTQ_INT_S "s") (VMULLTQ_INT_U "u")
		 (VNEGQ_M_S "s")
		 (VQABSQ_M_S "s")
		 (VQMOVNBQ_M_S "s") (VQMOVNBQ_M_U "u")
		 (VQMOVNBQ_S "s") (VQMOVNBQ_U "u")
		 (VQMOVNTQ_M_S "s") (VQMOVNTQ_M_U "u")
		 (VQMOVNTQ_S "s") (VQMOVNTQ_U "u")
		 (VQMOVUNBQ_M_S "s")
		 (VQMOVUNBQ_S "s")
		 (VQMOVUNTQ_M_S "s")
		 (VQMOVUNTQ_S "s")
		 (VQNEGQ_M_S "s")
		 (VQRSHRNBQ_M_N_S "s") (VQRSHRNBQ_M_N_U "u")
		 (VQRSHRNBQ_N_S "s") (VQRSHRNBQ_N_U "u")
		 (VQRSHRNTQ_M_N_S "s") (VQRSHRNTQ_M_N_U "u")
		 (VQRSHRNTQ_N_S "s") (VQRSHRNTQ_N_U "u")
		 (VQRSHRUNBQ_M_N_S "s")
		 (VQRSHRUNBQ_N_S "s")
		 (VQRSHRUNTQ_M_N_S "s")
		 (VQRSHRUNTQ_N_S "s")
		 (VQSHRNBQ_M_N_S "s") (VQSHRNBQ_M_N_U "u")
		 (VQSHRNBQ_N_S "s") (VQSHRNBQ_N_U "u")
		 (VQSHRNTQ_M_N_S "s") (VQSHRNTQ_M_N_U "u")
		 (VQSHRNTQ_N_S "s") (VQSHRNTQ_N_U "u")
		 (VQSHRUNBQ_M_N_S "s")
		 (VQSHRUNBQ_N_S "s")
		 (VQSHRUNTQ_M_N_S "s")
		 (VQSHRUNTQ_N_S "s")
		 (VRSHRNBQ_M_N_S "i") (VRSHRNBQ_M_N_U "i")
		 (VRSHRNBQ_N_S "i") (VRSHRNBQ_N_U "i")
		 (VRSHRNTQ_M_N_S "i") (VRSHRNTQ_M_N_U "i")
		 (VRSHRNTQ_N_S "i") (VRSHRNTQ_N_U "i")
		 (VSHRNBQ_M_N_S "i") (VSHRNBQ_M_N_U "i")
		 (VSHRNBQ_N_S "i") (VSHRNBQ_N_U "i")
		 (VSHRNTQ_M_N_S "i") (VSHRNTQ_M_N_U "i")
		 (VSHRNTQ_N_S "i") (VSHRNTQ_N_U "i")
		 ])

(define_int_attr mve_mnemo [
		 (VABSQ_M_S "vabs") (VABSQ_M_F "vabs")
		 (VNEGQ_M_S "vneg") (VNEGQ_M_F "vneg")
		 (VRNDAQ_F "vrinta") (VRNDAQ_M_F "vrinta")
		 (VRNDMQ_F "vrintm") (VRNDMQ_M_F "vrintm")
		 (VRNDNQ_F "vrintn") (VRNDNQ_M_F "vrintn")
		 (VRNDPQ_F "vrintp") (VRNDPQ_M_F "vrintp")
		 (VRNDQ_F "vrintz") (VRNDQ_M_F "vrintz")
		 (VRNDXQ_F "vrintx") (VRNDXQ_M_F "vrintx")
		 ])

;; plus and minus are the only SHIFTABLE_OPS for which Thumb2 allows
;; a stack pointer operand.  The minus operation is a candidate for an rsub
;; and hence only plus is supported.
(define_code_attr t2_binop0
  [(plus "rk") (minus "r") (ior "r") (xor "r") (and "r")])

;; The instruction to use when a SHIFTABLE_OPS has a shift operation as
;; its first operand.
(define_code_attr arith_shift_insn
  [(plus "add") (minus "rsb") (ior "orr") (xor "eor") (and "and")])

(define_code_attr cmp_op [(eq "eq") (gt "gt") (ge "ge") (lt "lt") (le "le")
                          (gtu "gt") (geu "ge")])

(define_code_attr mve_cmp_op [(eq "eq") (gt "gt") (ge "ge") (lt "lt") (le "le")
                              (gtu "hi") (geu "cs") (ne "ne")])

(define_code_attr cmp_type [(eq "i") (gt "s") (ge "s") (lt "s") (le "s")])

(define_code_attr mve_cmp_type [(eq "i") (gt "s") (ge "s") (lt "s") (le "s")
                                (gtu "u") (geu "u") (ne "i")])

(define_code_attr vfml_op [(plus "a") (minus "s")])

(define_code_attr ss_op [(ss_plus "qadd") (ss_minus "qsub")])

;;----------------------------------------------------------------------------
;; Int iterators
;;----------------------------------------------------------------------------

(define_int_iterator VRINT [UNSPEC_VRINTZ UNSPEC_VRINTP UNSPEC_VRINTM
                            UNSPEC_VRINTR UNSPEC_VRINTX UNSPEC_VRINTA])

(define_int_iterator NEON_VCMP [UNSPEC_VCEQ UNSPEC_VCGT UNSPEC_VCGE
				UNSPEC_VCLT UNSPEC_VCLE])

(define_int_iterator NEON_VAGLTE [UNSPEC_VCAGE UNSPEC_VCAGT
				  UNSPEC_VCALE UNSPEC_VCALT])

(define_int_iterator VCVT [UNSPEC_VRINTP UNSPEC_VRINTM UNSPEC_VRINTA])

(define_int_iterator NEON_VRINT [UNSPEC_NVRINTP UNSPEC_NVRINTZ UNSPEC_NVRINTM
                              UNSPEC_NVRINTX UNSPEC_NVRINTA UNSPEC_NVRINTN])

(define_int_iterator NEON_VCVT [UNSPEC_NVRINTP UNSPEC_NVRINTM UNSPEC_NVRINTA])

(define_int_iterator VADDL [UNSPEC_VADDL_S UNSPEC_VADDL_U])

(define_int_iterator VADDW [UNSPEC_VADDW_S UNSPEC_VADDW_U])

(define_int_iterator VHADD [UNSPEC_VRHADD_S UNSPEC_VRHADD_U
			    UNSPEC_VHADD_S UNSPEC_VHADD_U])

(define_int_iterator VQADD [UNSPEC_VQADD_S UNSPEC_VQADD_U])

(define_int_iterator VADDHN [UNSPEC_VADDHN UNSPEC_VRADDHN])

(define_int_iterator VMLAL [UNSPEC_VMLAL_S UNSPEC_VMLAL_U])

(define_int_iterator VMLAL_LANE [UNSPEC_VMLAL_S_LANE UNSPEC_VMLAL_U_LANE])

(define_int_iterator VMLSL [UNSPEC_VMLSL_S UNSPEC_VMLSL_U])

(define_int_iterator VMLSL_LANE [UNSPEC_VMLSL_S_LANE UNSPEC_VMLSL_U_LANE])

(define_int_iterator VQDMULH [UNSPEC_VQDMULH UNSPEC_VQRDMULH])

(define_int_iterator VQDMULH_LANE [UNSPEC_VQDMULH_LANE UNSPEC_VQRDMULH_LANE])

(define_int_iterator VMULL [UNSPEC_VMULL_S UNSPEC_VMULL_U UNSPEC_VMULL_P])

(define_int_iterator VMULL_LANE [UNSPEC_VMULL_S_LANE UNSPEC_VMULL_U_LANE])

(define_int_iterator VSUBL [UNSPEC_VSUBL_S UNSPEC_VSUBL_U])

(define_int_iterator VSUBW [UNSPEC_VSUBW_S UNSPEC_VSUBW_U])

(define_int_iterator VHSUB [UNSPEC_VHSUB_S UNSPEC_VHSUB_U])

(define_int_iterator VQSUB [UNSPEC_VQSUB_S UNSPEC_VQSUB_U])

(define_int_iterator VSUBHN [UNSPEC_VSUBHN UNSPEC_VRSUBHN])

(define_int_iterator VABAL [UNSPEC_VABAL_S UNSPEC_VABAL_U])

(define_int_iterator VABD [UNSPEC_VABD_S UNSPEC_VABD_U])

(define_int_iterator VABDL [UNSPEC_VABDL_S UNSPEC_VABDL_U])

(define_int_iterator VMAXMIN [UNSPEC_VMAX UNSPEC_VMAX_U
			      UNSPEC_VMIN UNSPEC_VMIN_U])

(define_int_iterator VMAXMINF [UNSPEC_VMAX UNSPEC_VMIN])

(define_int_iterator VMAXMINFNM [UNSPEC_VMAXNM UNSPEC_VMINNM])

(define_int_iterator VPADDL [UNSPEC_VPADDL_S UNSPEC_VPADDL_U])

(define_int_iterator VPADAL [UNSPEC_VPADAL_S UNSPEC_VPADAL_U])

(define_int_iterator VPMAXMIN [UNSPEC_VPMAX UNSPEC_VPMAX_U
			       UNSPEC_VPMIN UNSPEC_VPMIN_U])

(define_int_iterator VPMAXMINF [UNSPEC_VPMAX UNSPEC_VPMIN])

(define_int_iterator VCVT_US [UNSPEC_VCVT_S UNSPEC_VCVT_U])

(define_int_iterator VCVT_US_N [UNSPEC_VCVT_S_N UNSPEC_VCVT_U_N])

(define_int_iterator VCVT_HF_US_N [UNSPEC_VCVT_HF_S_N UNSPEC_VCVT_HF_U_N])

(define_int_iterator VCVT_SI_US_N [UNSPEC_VCVT_SI_S_N UNSPEC_VCVT_SI_U_N])

(define_int_iterator VCVT_HF_US [UNSPEC_VCVTA_S UNSPEC_VCVTA_U
				 UNSPEC_VCVTM_S UNSPEC_VCVTM_U
				 UNSPEC_VCVTN_S UNSPEC_VCVTN_U
				 UNSPEC_VCVTP_S UNSPEC_VCVTP_U])

(define_int_iterator VCVTH_US [UNSPEC_VCVTH_S UNSPEC_VCVTH_U])

;; Operators for FP16 instructions.
(define_int_iterator FP16_RND [UNSPEC_VRND UNSPEC_VRNDA
			       UNSPEC_VRNDM UNSPEC_VRNDN
			       UNSPEC_VRNDP UNSPEC_VRNDX])

(define_int_iterator VQMOVN [UNSPEC_VQMOVN_S UNSPEC_VQMOVN_U])

(define_int_iterator VMOVL [UNSPEC_VMOVL_S UNSPEC_VMOVL_U])

(define_int_iterator VSHL [UNSPEC_VSHL_S UNSPEC_VSHL_U
			   UNSPEC_VRSHL_S UNSPEC_VRSHL_U])

(define_int_iterator VQSHL [UNSPEC_VQSHL_S UNSPEC_VQSHL_U
			    UNSPEC_VQRSHL_S UNSPEC_VQRSHL_U])

(define_int_iterator VSHR_N [UNSPEC_VSHR_S_N UNSPEC_VSHR_U_N
			     UNSPEC_VRSHR_S_N UNSPEC_VRSHR_U_N])

(define_int_iterator VSHRN_N [UNSPEC_VSHRN_N UNSPEC_VRSHRN_N])

(define_int_iterator VQSHRN_N [UNSPEC_VQSHRN_S_N UNSPEC_VQSHRN_U_N
			       UNSPEC_VQRSHRN_S_N UNSPEC_VQRSHRN_U_N])

(define_int_iterator VQSHRUN_N [UNSPEC_VQSHRUN_N UNSPEC_VQRSHRUN_N])

(define_int_iterator VQSHL_N [UNSPEC_VQSHL_S_N UNSPEC_VQSHL_U_N])

(define_int_iterator VSHLL_N [UNSPEC_VSHLL_S_N UNSPEC_VSHLL_U_N])

(define_int_iterator VSRA_N [UNSPEC_VSRA_S_N UNSPEC_VSRA_U_N
			     UNSPEC_VRSRA_S_N UNSPEC_VRSRA_U_N])

(define_int_iterator CRC [UNSPEC_CRC32B UNSPEC_CRC32H UNSPEC_CRC32W
                          UNSPEC_CRC32CB UNSPEC_CRC32CH UNSPEC_CRC32CW])

(define_int_iterator CRYPTO_AESMC [UNSPEC_AESMC UNSPEC_AESIMC])

(define_int_iterator CRYPTO_AES [UNSPEC_AESD UNSPEC_AESE])

(define_int_iterator CRYPTO_BINARY [UNSPEC_SHA1SU1 UNSPEC_SHA256SU0])

(define_int_iterator CRYPTO_TERNARY [UNSPEC_SHA1SU0 UNSPEC_SHA256H
                                     UNSPEC_SHA256H2 UNSPEC_SHA256SU1])

(define_int_iterator CRYPTO_SELECTING [UNSPEC_SHA1C UNSPEC_SHA1M
                                       UNSPEC_SHA1P])

(define_int_iterator USXTB16 [UNSPEC_SXTB16 UNSPEC_UXTB16])
(define_int_iterator SIMD32_NOGE_BINOP
				[UNSPEC_QADD8 UNSPEC_QSUB8 UNSPEC_SHADD8
				 UNSPEC_SHSUB8 UNSPEC_UHADD8 UNSPEC_UHSUB8
				 UNSPEC_UQADD8 UNSPEC_UQSUB8
				 UNSPEC_QADD16 UNSPEC_QASX UNSPEC_QSAX
				 UNSPEC_QSUB16 UNSPEC_SHADD16 UNSPEC_SHASX
				 UNSPEC_SHSAX UNSPEC_SHSUB16 UNSPEC_UHADD16
				 UNSPEC_UHASX UNSPEC_UHSAX UNSPEC_UHSUB16
				 UNSPEC_UQADD16 UNSPEC_UQASX UNSPEC_UQSAX
				 UNSPEC_UQSUB16 UNSPEC_SMUSD UNSPEC_SMUSDX
				 UNSPEC_SXTAB16 UNSPEC_UXTAB16 UNSPEC_USAD8])

(define_int_iterator SIMD32_DIMODE [UNSPEC_SMLALD UNSPEC_SMLALDX
				    UNSPEC_SMLSLD UNSPEC_SMLSLDX])

(define_int_iterator SMLAWBT [UNSPEC_SMLAWB UNSPEC_SMLAWT])

(define_int_iterator SIMD32_GE [UNSPEC_SADD8 UNSPEC_SSUB8 UNSPEC_UADD8
				UNSPEC_USUB8 UNSPEC_SADD16 UNSPEC_SASX
				UNSPEC_SSAX UNSPEC_SSUB16 UNSPEC_UADD16
				UNSPEC_UASX UNSPEC_USAX UNSPEC_USUB16])

(define_int_iterator SIMD32_TERNOP_Q [UNSPEC_SMLAD UNSPEC_SMLADX UNSPEC_SMLSD
				      UNSPEC_SMLSDX])

(define_int_iterator SIMD32_BINOP_Q [UNSPEC_SMUAD UNSPEC_SMUADX])

(define_int_iterator USSAT16 [UNSPEC_SSAT16 UNSPEC_USAT16])

(define_int_iterator VQRDMLH_AS [UNSPEC_VQRDMLAH UNSPEC_VQRDMLSH])

(define_int_iterator VFM_LANE_AS [UNSPEC_VFMA_LANE UNSPEC_VFMS_LANE])

(define_int_iterator DOTPROD [UNSPEC_DOT_S UNSPEC_DOT_U])

(define_int_iterator DOTPROD_I8MM [UNSPEC_DOT_US UNSPEC_DOT_SU])

(define_int_iterator VFMLHALVES [UNSPEC_VFML_LO UNSPEC_VFML_HI])

(define_int_iterator VCADD [UNSPEC_VCADD90 UNSPEC_VCADD270])
(define_int_iterator VCMLA [UNSPEC_VCMLA UNSPEC_VCMLA90 UNSPEC_VCMLA180 UNSPEC_VCMLA270])

(define_int_iterator MATMUL [UNSPEC_MATMUL_S UNSPEC_MATMUL_U UNSPEC_MATMUL_US])

(define_int_iterator BF_MA [UNSPEC_BFMAB UNSPEC_BFMAT])

(define_int_iterator CDE_VCX [UNSPEC_VCDE UNSPEC_VCDEA])

;;----------------------------------------------------------------------------
;; Mode attributes
;;----------------------------------------------------------------------------

;; Determine name of atomic compare and swap from success result mode.  This
;; distinguishes between 16-bit Thumb and 32-bit Thumb/ARM.
(define_mode_attr arch [(CC_Z "32") (SI "t1")])

;; Determine element size suffix from vector mode.
(define_mode_attr MMX_char [(V8QI "b") (V4HI "h") (V2SI "w") (DI "d")])

;; vtbl<n> suffix for NEON vector modes.
(define_mode_attr VTAB_n [(TI "2") (EI "3") (OI "4")])

;; Suffix for x2 variants of vld1 and vst1.
(define_mode_attr VMEMX2_q [(TI "") (OI "q")])

;; fp16 or bf16 marker for 16-bit float modes.
(define_mode_attr fporbf [(HF "fp16") (BF "bf16")])

;; (Opposite) mode to convert to/from for NEON mode conversions.
(define_mode_attr V_CVTTO [(V2SI "V2SF") (V2SF "V2SI")
               (V4SI "V4SF") (V4SF "V4SI")])

;; As above but in lower case.
(define_mode_attr V_cvtto [(V2SI "v2sf") (V2SF "v2si")
                           (V4SI "v4sf") (V4SF "v4si")])

;; (Opposite) mode to convert to/from for vector-half mode conversions.
(define_mode_attr VH_CVTTO [(V4HI "V4HF") (V4HF "V4HI")
			    (V8HI "V8HF") (V8HF "V8HI")])
(define_mode_attr VH_cvtto [(V4HI "v4hf") (V4HF "v4hi")
			    (V8HI "v8hf") (V8HF "v8hi")])

;; Define element mode for each vector mode.
(define_mode_attr V_elem [(V8QI "QI") (V16QI "QI")
			  (V4HI "HI") (V8HI "HI")
			  (V4HF "HF") (V8HF "HF")
			  (V4BF "BF") (V8BF "BF")
			  (V2SI "SI") (V4SI "SI")
			  (V2SF "SF") (V4SF "SF")
			  (DI   "DI") (V2DI "DI")
			  (V2DF "DF")])

;; As above but in lower case.
(define_mode_attr V_elem_l [(V8QI "qi") (V16QI "qi")
			    (V4HI "hi") (V8HI "hi")
			    (V4HF "hf") (V8HF "hf")
			    (V4BF "bf") (V8BF "bf")
			    (V2SI "si") (V4SI "si")
			    (V2SF "sf") (V4SF "sf")
			    (DI "di")   (V2DI "di")])

;; Element modes for vector extraction, padded up to register size.

(define_mode_attr V_ext [(V8QI "SI") (V16QI "SI")
             (V4HI "SI") (V8HI "SI")
             (V2SI "SI") (V4SI "SI")
             (V2SF "SF") (V4SF "SF")
             (DI "DI") (V2DI "DI")])

;; Mode of pair of elements for each vector mode, to define transfer
;; size for structure lane/dup loads and stores.
(define_mode_attr V_two_elem [(V8QI "HI")   (V16QI "HI")
                              (V4HI "SI")   (V8HI "SI")
                              (V4HF "SF")   (V8HF "SF")
                              (V4BF "BF")   (V8BF "BF")
                              (V2SI "V2SI") (V4SI "V2SI")
                              (V2SF "V2SF") (V4SF "V2SF")
                              (DI "V2DI")   (V2DI "V2DI")])

;; Mode mapping for VFM[A,S]L instructions.
(define_mode_attr VFML [(V2SF "V4HF") (V4SF "V8HF")])

;; Mode mapping for VFM[A,S]L instructions for the vec_select result.
(define_mode_attr VFMLSEL [(V2SF "V2HF") (V4SF "V4HF")])

;; Mode mapping for VFM[A,S]L instructions for some awkward lane-wise forms.
(define_mode_attr VFMLSEL2 [(V2SF "V8HF") (V4SF "V4HF")])

;; Same as the above, but lowercase.
(define_mode_attr vfmlsel2 [(V2SF "v8hf") (V4SF "v4hf")])

;; Similar, for three elements.
(define_mode_attr V_three_elem [(V8QI "BLK") (V16QI "BLK")
                                (V4HI "BLK") (V8HI "BLK")
                                (V4HF "BLK") (V8HF "BLK")
                                (V4BF "BLK") (V8BF "BLK")
                                (V2SI "BLK") (V4SI "BLK")
                                (V2SF "BLK") (V4SF "BLK")
                                (DI "EI")    (V2DI "EI")])

;; Similar, for four elements.
(define_mode_attr V_four_elem [(V8QI "SI")   (V16QI "SI")
                               (V4HI "V4HI") (V8HI "V4HI")
                               (V4HF "V4HF") (V8HF "V4HF")
                               (V4BF "V4BF") (V8BF "V4BF")
                               (V2SI "V4SI") (V4SI "V4SI")
                               (V2SF "V4SF") (V4SF "V4SF")
                               (DI "OI")     (V2DI "OI")])

;; Register width from element mode
(define_mode_attr V_reg [(V8QI "P") (V16QI "q")
			 (V4HI "P") (V8HI  "q")
			 (V4HF "P") (V8HF  "q")
			 (V4BF "P") (V8BF  "q")
			 (V2SI "P") (V4SI  "q")
			 (V2SF "P") (V4SF  "q")
			 (DI   "P") (V2DI  "q")
			 (V2HF "") (SF   "") (SI "")
			 (DF    "P") (HF   "")])

;; Output template to select the high VFP register of a mult-register value.
(define_mode_attr V_hi [(V2SF "p") (V4SF  "f")])

;; Output template to select the low VFP register of a mult-register value.
(define_mode_attr V_lo [(V2SF "") (V4SF  "e")])

;; Helper attribute for printing output templates for awkward forms of
;; vfmlal/vfmlsl intrinsics.
(define_mode_attr V_lane_reg [(V2SF "") (V4SF  "P")])

;; Wider modes with the same number of elements.
(define_mode_attr V_widen [(V8QI "V8HI") (V4HI "V4SI") (V2SI "V2DI")])

;; Narrower modes with the same number of elements.
(define_mode_attr V_narrow [(V8HI "V8QI") (V4SI "V4HI") (V2DI "V2SI")])

;; Narrower modes with double the number of elements.
(define_mode_attr V_narrow_pack [(V4SI "V8HI") (V8HI "V16QI") (V2DI "V4SI")
				 (V4HI "V8QI") (V2SI "V4HI")  (DI "V2SI")])

;; Modes with half the number of equal-sized elements.
(define_mode_attr V_HALF [(V16QI "V8QI") (V8HI "V4HI")
			  (V8HF "V4HF") (V4SI  "V2SI")
			  (V4SF "V2SF") (V2DF "DF")
			  (V2DI "DI") (V4HF "HF")
			  (V4BF "BF") (V8BF  "V4BF")])

;; Same, but lower-case.
(define_mode_attr V_half [(V16QI "v8qi") (V8HI "v4hi")
              (V4SI  "v2si") (V4SF "v2sf")
                          (V2DI "di")])

;; Modes with twice the number of equal-sized elements.
(define_mode_attr V_DOUBLE [(V8QI "V16QI") (V4HI "V8HI")
			    (V2SI "V4SI") (V4HF "V8HF")
			    (V2SF "V4SF") (DF "V2DF")
			    (DI "V2DI")   (V4BF "V8BF")])

;; Same, but lower-case.
(define_mode_attr V_double [(V8QI "v16qi") (V4HI "v8hi")
                (V2SI "v4si") (V2SF "v4sf")
                            (DI "v2di")])

;; Modes with double-width elements.
(define_mode_attr V_double_width [(V8QI "V4HI") (V16QI "V8HI")
                  (V4HI "V2SI") (V8HI "V4SI")
                  (V2SI "DI")   (V4SI "V2DI")])

;; Double-sized modes with the same element size.
;; Used for neon_vdup_lane, where the second operand is double-sized
;; even when the first one is quad.
(define_mode_attr V_double_vector_mode [(V16QI "V8QI") (V8HI "V4HI")
					(V4SI "V2SI") (V4SF "V2SF")
					(V8QI "V8QI") (V4HI "V4HI")
					(V2SI "V2SI") (V2SF "V2SF")
					(V8BF "V4BF") (V4BF "V4BF")
					(V8HF "V4HF") (V4HF "V4HF")])

;; Mode of result of comparison operations (and bit-select operand 1).
(define_mode_attr V_cmp_result [(V8QI "V8QI") (V16QI "V16QI")
				(V4HI "V4HI") (V8HI  "V8HI")
                                (V2SI "V2SI") (V4SI  "V4SI")
				(V4HF "V4HI") (V8HF  "V8HI")
				(V4BF "V4HI") (V8BF  "V8HI")
                                (V2SF "V2SI") (V4SF  "V4SI")
                                (DI   "DI")   (V2DI  "V2DI")])

(define_mode_attr v_cmp_result [(V8QI "v8qi") (V16QI "v16qi")
				(V4HI "v4hi") (V8HI  "v8hi")
				(V2SI "v2si") (V4SI  "v4si")
				(V4HF "v4hi") (V8HF  "v8hi")
				(DI   "di")   (V2DI  "v2di")
				(V2SF "v2si") (V4SF  "v4si")])

;; Get element type from double-width mode, for operations where we 
;; don't care about signedness.
(define_mode_attr V_if_elem [(V8QI "i8")  (V16QI "i8")
			     (V4HI "i16") (V8HI  "i16")
			     (V2SI "i32") (V4SI  "i32")
			     (DI   "i64") (V2DI  "i64")
			     (V2SF "f32") (V4SF  "f32")
			     (SF   "f32") (DF    "f64")
			     (HF   "f16") (V4HF  "f16")
			     (V8HF "f16")])

;; Same, but for operations which work on signed values.
(define_mode_attr V_s_elem [(V8QI "s8")  (V16QI "s8")
			    (V4HI "s16") (V8HI  "s16")
			    (V2SI "s32") (V4SI  "s32")
			    (DI   "s64") (V2DI  "s64")
			    (V2SF "f32") (V4SF  "f32")
			    (HF   "f16") (V4HF  "f16")
			    (V8HF "f16")])

;; Same, but for operations which work on unsigned values.
(define_mode_attr V_u_elem [(V8QI "u8")  (V16QI "u8")
                (V4HI "u16") (V8HI  "u16")
                            (V2SI "u32") (V4SI  "u32")
                            (DI   "u64") (V2DI  "u64")
                            (V2SF "f32") (V4SF  "f32")])

;; Element types for extraction of unsigned scalars.
(define_mode_attr V_uf_sclr [(V8QI "u8")  (V16QI "u8")
                 (V4HI "u16") (V8HI "u16")
                             (V2SI "32") (V4SI "32")
                             (V4HF "u16") (V8HF "u16")
                             (V4BF "u16") (V8BF "u16")
                             (V2SF "32") (V4SF "32")])

(define_mode_attr V_sz_elem [(V8QI "8")  (V16QI "8")
			     (V4HI "16") (V8HI  "16")
			     (V2SI "32") (V4SI  "32")
			     (DI   "64") (V2DI  "64")
			     (V4HF "16") (V8HF "16")
			     (V4BF "16") (V8BF "16")
			     (V2SF "32") (V4SF  "32")])

(define_mode_attr V_elem_ch [(V8QI "b")  (V16QI "b")
			     (V4HI "h") (V8HI  "h")
			     (V2SI "s") (V4SI  "s")
			     (DI   "d") (V2DI  "d")
			     (V2SF "s") (V4SF  "s")
			     (V2SF "s") (V4SF  "s")])

(define_mode_attr VH_elem_ch [(V4HI "s") (V8HI  "s")
			      (V4HF "s") (V8HF  "s")
			      (HF "s")])

;; Element sizes for duplicating ARM registers to all elements of a vector.
(define_mode_attr VD_dup [(V8QI "8") (V4HI "16") (V2SI "32") (V2SF "32")])

;; Opaque integer types for results of pair-forming intrinsics (vtrn, etc.)
(define_mode_attr V_PAIR [(V8QI "TI") (V16QI "OI")
              (V4HI "TI") (V8HI  "OI")
                          (V2SI "TI") (V4SI  "OI")
                          (V2SF "TI") (V4SF  "OI")
                          (DI   "TI") (V2DI  "OI")])

;; Same, but lower-case.
(define_mode_attr V_pair [(V8QI "ti") (V16QI "oi")
              (V4HI "ti") (V8HI  "oi")
                          (V2SI "ti") (V4SI  "oi")
                          (V2SF "ti") (V4SF  "oi")
                          (DI   "ti") (V2DI  "oi")])

;; Extra suffix on some 64-bit insn names (to avoid collision with standard
;; names which we don't want to define).
(define_mode_attr V_suf64 [(V8QI "") (V16QI "")
                           (V4HI "") (V8HI "")
                           (V2SI "") (V4SI "")
                           (V2SF "") (V4SF "")
                           (DI "_neon") (V2DI "")])

;; To select the low 64 bits of a vector.
(define_mode_attr V_bf_low [(V4BF "P") (V8BF "e")])

;; To generate intermediate modes for BF16 scalar convert.
(define_mode_attr V_bf_cvt_m [(V2SI "BF") (SF "V2SI")])


;; Scalars to be presented to scalar multiplication instructions
;; must satisfy the following constraints.
;; 1. If the mode specifies 16-bit elements, the scalar must be in D0-D7.
;; 2. If the mode specifies 32-bit elements, the scalar must be in D0-D15.

;; This mode attribute is used to obtain the correct register constraints.

(define_mode_attr scalar_mul_constraint [(V4HI "x") (V2SI "t") (V2SF "t")
					 (V8HI "x") (V4SI "t") (V4SF "t")
					 (V8HF "x") (V4HF "x")])

;; Predicates used for setting type for neon instructions

(define_mode_attr Is_float_mode [(V8QI "false") (V16QI "false")
				 (V4HI "false") (V8HI "false")
				 (V2SI "false") (V4SI "false")
				 (V4HF "true") (V8HF "true")
				 (V2SF "true") (V4SF "true")
				 (DI "false") (V2DI "false")])

(define_mode_attr Scalar_mul_8_16 [(V8QI "true") (V16QI "true")
				   (V4HI "true") (V8HI "true")
				   (V2SI "false") (V4SI "false")
				   (V2SF "false") (V4SF "false")
				   (DI "false") (V2DI "false")])

(define_mode_attr Is_d_reg [(V8QI "true") (V16QI "false")
			    (V4HI "true") (V8HI  "false")
			    (V2SI "true") (V4SI  "false")
			    (V2SF "true") (V4SF  "false")
			    (DI   "true") (V2DI  "false")
			    (V4BF "true") (V8BF  "false")
			    (V4HF "true") (V8HF  "false")])

(define_mode_attr V_mode_nunits [(V8QI "8") (V16QI "16")
				 (V4HF "4") (V8HF "8")
				 (V4BF "4") (V8BF "8")
                                 (V4HI "4") (V8HI "8")
                                 (V2SI "2") (V4SI "4")
                                 (V2SF "2") (V4SF "4")
                                 (DI "1")   (V2DI "2")
                                 (DF "1")   (V2DF "2")])

;; Same as V_widen, but lower-case.
(define_mode_attr V_widen_l [(V8QI "v8hi") (V4HI "v4si") ( V2SI "v2di")])

;; Widen. Result is half the number of elements, but widened to double-width.
(define_mode_attr V_unpack   [(V16QI "V8HI") (V8HI "V4SI") (V4SI "V2DI")])

;; Conditions to be used in extend<mode>di patterns.
(define_mode_attr qhs_zextenddi_cond [(SI "") (HI "&& arm_arch6") (QI "")])
(define_mode_attr qhs_sextenddi_cond [(SI "") (HI "&& arm_arch6")
				      (QI "&& arm_arch6")])
(define_mode_attr qhs_zextenddi_op [(SI "s_register_operand")
				   (HI "nonimmediate_operand")
				   (QI "nonimmediate_operand")])
(define_mode_attr qhs_extenddi_op [(SI "s_register_operand")
				   (HI "nonimmediate_operand")
				   (QI "arm_reg_or_extendqisi_mem_op")])
(define_mode_attr qhs_extenddi_cstr [(SI "0,r,r") (HI "0,rm,rm") (QI "0,rUq,rm")])
(define_mode_attr qhs_zextenddi_cstr [(SI "0,r") (HI "0,rm") (QI "0,rm")])

;; Mode attributes used for fixed-point support.
(define_mode_attr qaddsub_suf [(V4UQQ "8") (V2UHQ "16") (UQQ "8") (UHQ "16")
			       (V2UHA "16") (UHA "16")
			       (V4QQ "8") (V2HQ "16") (QQ "8") (HQ "16")
			       (V2HA "16") (HA "16") (SQ "") (SA "")])

(define_mode_attr qaddsub_clob_q [(V4UQQ "0") (V2UHQ "0") (UQQ "0") (UHQ "0")
			       (V2UHA "0") (UHA "0")
			       (V4QQ "0") (V2HQ "0") (QQ "0") (HQ "0")
			       (V2HA "0") (HA "0") (SQ "ARM_Q_BIT_READ")
			       (SA "ARM_Q_BIT_READ")])

;; Mode attribute for vshll.
(define_mode_attr V_innermode [(V8QI "QI") (V4HI "HI") (V2SI "SI")])

;; Mode attributes used for VFP support.
(define_mode_attr F_constraint [(SF "t") (DF "w")])
(define_mode_attr vfp_type [(SF "s") (DF "d")])
(define_mode_attr vfp_double_cond [(SF "") (DF "&& TARGET_VFP_DOUBLE")])
(define_mode_attr VF_constraint [(V4HF "t") (V8HF "t") (V2SF "t") (V4SF "w")])

;; Mode attribute used to build the "type" attribute.
(define_mode_attr q [(V8QI "") (V16QI "_q")
		     (V4HI "") (V8HI "_q")
		     (V2SI "") (V4SI "_q")
		     (V4HF "") (V8HF "_q")
		     (V2SF "") (V4SF "_q")
		     (V4HF "") (V8HF "_q")
		     (V4BF "") (V8BF "_q")
		     (DI "")   (V2DI "_q")
		     (DF "")   (V2DF "_q")
		     (HF "")])

(define_mode_attr pf [(V8QI "p") (V16QI "p") (V2SF "f") (V4SF "f")])

(define_mode_attr VSI2QI [(V2SI "V8QI") (V4SI "V16QI")])
(define_mode_attr vsi2qi [(V2SI "v8qi") (V4SI "v16qi")])

(define_mode_attr VSF2BF [(V2SF "V4BF") (V4SF "V8BF")])

(define_mode_attr cde_suffix [(SI "") (DI "d")])
(define_mode_attr cde_dest [(SI "%0") (DI "%0, %H0")])

;;MVE mode attribute.
(define_mode_attr MVE_CNVT [(V8HI "V8HF") (V4SI "V4SF") (V8HF "V8HI")
			    (V4SF "V4SI")])
(define_mode_attr MVE_LANES [(V16QI "16") (V8HI "8") (V4SI "4")])

(define_mode_attr MVE_constraint [ (V16QI "Ra") (V8HI "Rc") (V4SI "Re")])
(define_mode_attr MVE_constraint1 [ (V8HI "Ra") (V4SI "Rc")])
(define_mode_attr MVE_constraint2 [(V16QI "Rb") (V8HI "Rd") (V4SI "Rf")
				    (V8HF "Rd") (V4SF "Rf")])
(define_mode_attr MVE_constraint3 [ (V8HI "Rb") (V4SI "Rd")])

(define_mode_attr MVE_pred [ (V16QI "mve_imm_7") (V8HI "mve_imm_15")
				   (V4SI "mve_imm_31")])
(define_mode_attr MVE_pred1 [ (V8HI "mve_imm_7") (V4SI "mve_imm_15")])
(define_mode_attr MVE_pred2 [(V16QI "mve_imm_8") (V8HI "mve_imm_16")
			     (V4SI "mve_imm_32")
			     (V8HF "mve_imm_16") (V4SF "mve_imm_32")])
(define_mode_attr MVE_pred3 [ (V8HI "mve_imm_8") (V4SI "mve_imm_16")])

(define_mode_attr MVE_B_ELEM [ (V16QI "V16QI") (V8HI "V8QI") (V4SI "V4QI")])
(define_mode_attr MVE_H_ELEM [ (V8HI "V8HI") (V4SI "V4HI")])

(define_mode_attr V_sz_elem1 [(V16QI "b") (V8HI  "h") (V4SI "w") (V8HF "h")
			      (V4SF "w")])
(define_mode_attr V_extr_elem [(V16QI "u8") (V8HI "u16") (V4SI "32")
			       (V8HF "u16") (V4SF "32")])
(define_mode_attr earlyclobber_32 [(V16QI "=w") (V8HI "=w") (V4SI "=&w")
						(V8HF "=w") (V4SF "=&w")])
(define_mode_attr MVE_VPRED [(V16QI "V16BI") (V8HI "V8BI") (V4SI "V4BI")
			     (V8HF "V8BI")   (V4SF "V4BI") (V2DI "V2QI")])
(define_mode_attr MVE_vpred [(V16QI "v16bi") (V8HI "v8bi") (V4SI "v4bi")
			     (V8HF "v8bi")   (V4SF "v4bi")
			     (V16BI "v16bi") (V8BI "v8bi") (V4BI "v4bi")
			     (V2QI "v2qi")])
(define_mode_attr MVE_vctp [(V16BI "8") (V8BI "16") (V4BI "32") (V2QI "64")])

;;----------------------------------------------------------------------------
;; Code attributes
;;----------------------------------------------------------------------------

;; Determine the mode of a 'wide compare', ie where the carry flag is
;; propagated into the comparison.
(define_code_attr CC_EXTEND [(sign_extend "CC_NV") (zero_extend "CC_B")])

;; Assembler mnemonics for vqh_ops and vqhs_ops iterators.
(define_code_attr VQH_mnem [(plus "vadd") (smin "vmin") (smax "vmax")
                (umin "vmin") (umax "vmax")])

;; Type attributes for vqh_ops and vqhs_ops iterators.
(define_code_attr VQH_type [(plus "add") (smin "minmax") (smax "minmax")
                (umin "minmax") (umax "minmax")])

;; Signs of above, where relevant.
(define_code_attr VQH_sign [(plus "i") (smin "s") (smax "s") (umin "u")
                (umax "u")])

;; Map rtl operator codes to optab names
(define_code_attr optab
 [(and "and")
  (ior "ior")
  (xor "xor")])

;; Assembler mnemonics for signedness of widening operations.
(define_code_attr US [(sign_extend "s") (zero_extend "u")])
(define_code_attr Us [(sign_extend "") (zero_extend "u")])

;; Signedness suffix for float->fixed conversions.  Empty for signed
;; conversion.
(define_code_attr su_optab [(fix "") (unsigned_fix "u")])

;; Sign prefix to use in instruction type suffixes, i.e. s32, u32.
(define_code_attr su [(fix "s") (unsigned_fix "u")])

;; Right shifts
(define_code_attr shift [(ashiftrt "ashr") (lshiftrt "lshr")])
(define_code_attr shifttype [(ashiftrt "signed") (lshiftrt "unsigned")])

;; String reprentations of operations on the sign of a number.
(define_code_attr absneg_str [(abs "abs") (neg "neg")])

;; Conversions.
(define_code_attr FCVTI32typename [(unsigned_float "u32") (float "s32")])

(define_code_attr float_sup [(unsigned_float "u") (float "s")])

(define_code_attr float_SUP [(unsigned_float "U") (float "S")])

;; max/min for MVE
(define_code_attr max_min_su_str [(smax "vmax") (umax "vmax") (smin "vmin") (umin "vmin")])

(define_code_attr max_min_supf [
		 (smax "s") (umax "u")
		 (smin "s") (umin "u")
		 ])

;; Floating-point max/min for MVE
(define_code_attr max_min_f_str [(smax "vmaxnm") (smin "vminnm")])

;;----------------------------------------------------------------------------
;; Int attributes
;;----------------------------------------------------------------------------

;; Mapping between vector UNSPEC operations and the signed ('s'),
;; unsigned ('u'), poly ('p') or float ('f') nature of their data type.
(define_int_attr sup [
  (UNSPEC_SXTB16 "s") (UNSPEC_UXTB16 "u")
  (UNSPEC_VADDL_S "s") (UNSPEC_VADDL_U "u")
  (UNSPEC_VADDW_S "s") (UNSPEC_VADDW_U "u")
  (UNSPEC_VRHADD_S "s") (UNSPEC_VRHADD_U "u")
  (UNSPEC_VHADD_S "s") (UNSPEC_VHADD_U "u")
  (UNSPEC_VQADD_S "s") (UNSPEC_VQADD_U "u")
  (UNSPEC_VMLAL_S "s") (UNSPEC_VMLAL_U "u")
  (UNSPEC_VMLAL_S_LANE "s") (UNSPEC_VMLAL_U_LANE "u")
  (UNSPEC_VMLSL_S "s") (UNSPEC_VMLSL_U "u")
  (UNSPEC_VMLSL_S_LANE "s") (UNSPEC_VMLSL_U_LANE "u")
  (UNSPEC_VMULL_S "s") (UNSPEC_VMULL_U "u") (UNSPEC_VMULL_P "p")
  (UNSPEC_VMULL_S_LANE "s") (UNSPEC_VMULL_U_LANE "u")
  (UNSPEC_VSUBL_S "s") (UNSPEC_VSUBL_U "u")
  (UNSPEC_VSUBW_S "s") (UNSPEC_VSUBW_U "u")
  (UNSPEC_VHSUB_S "s") (UNSPEC_VHSUB_U "u")
  (UNSPEC_VQSUB_S "s") (UNSPEC_VQSUB_U "u")
  (UNSPEC_VABAL_S "s") (UNSPEC_VABAL_U "u")
  (UNSPEC_VABD_S "s") (UNSPEC_VABD_U "u")
  (UNSPEC_VABDL_S "s") (UNSPEC_VABDL_U "u")
  (UNSPEC_VMAX "s") (UNSPEC_VMAX_U "u")
  (UNSPEC_VMIN "s") (UNSPEC_VMIN_U "u")
  (UNSPEC_VPADDL_S "s") (UNSPEC_VPADDL_U "u")
  (UNSPEC_VPADAL_S "s") (UNSPEC_VPADAL_U "u")
  (UNSPEC_VPMAX "s") (UNSPEC_VPMAX_U "u")
  (UNSPEC_VPMIN "s") (UNSPEC_VPMIN_U "u")
  (UNSPEC_VCVT_S "s") (UNSPEC_VCVT_U "u")
  (UNSPEC_VCVTA_S "s") (UNSPEC_VCVTA_U "u")
  (UNSPEC_VCVTM_S "s") (UNSPEC_VCVTM_U "u")
  (UNSPEC_VCVTN_S "s") (UNSPEC_VCVTN_U "u")
  (UNSPEC_VCVTP_S "s") (UNSPEC_VCVTP_U "u")
  (UNSPEC_VCVT_S_N "s") (UNSPEC_VCVT_U_N "u")
  (UNSPEC_VCVT_HF_S_N "s") (UNSPEC_VCVT_HF_U_N "u")
  (UNSPEC_VCVT_SI_S_N "s") (UNSPEC_VCVT_SI_U_N "u")
  (UNSPEC_VQMOVN_S "s") (UNSPEC_VQMOVN_U "u")
  (UNSPEC_VMOVL_S "s") (UNSPEC_VMOVL_U "u")
  (UNSPEC_VSHL_S "s") (UNSPEC_VSHL_U "u")
  (UNSPEC_VRSHL_S "s") (UNSPEC_VRSHL_U "u")
  (UNSPEC_VQSHL_S "s") (UNSPEC_VQSHL_U "u")
  (UNSPEC_VQRSHL_S "s") (UNSPEC_VQRSHL_U "u")
  (UNSPEC_VSHR_S_N "s") (UNSPEC_VSHR_U_N "u")
  (UNSPEC_VRSHR_S_N "s") (UNSPEC_VRSHR_U_N "u")
  (UNSPEC_VQSHRN_S_N "s") (UNSPEC_VQSHRN_U_N "u")
  (UNSPEC_VQRSHRN_S_N "s") (UNSPEC_VQRSHRN_U_N "u")
  (UNSPEC_VQSHL_S_N "s") (UNSPEC_VQSHL_U_N "u")
  (UNSPEC_VSHLL_S_N "s") (UNSPEC_VSHLL_U_N "u")
  (UNSPEC_VSRA_S_N "s") (UNSPEC_VSRA_U_N "u")
  (UNSPEC_VRSRA_S_N "s") (UNSPEC_VRSRA_U_N "u")
  (UNSPEC_VCVTH_S "s") (UNSPEC_VCVTH_U "u")
  (UNSPEC_DOT_S "s") (UNSPEC_DOT_U "u")
  (UNSPEC_DOT_US "us") (UNSPEC_DOT_SU "su")
  (UNSPEC_SSAT16 "s") (UNSPEC_USAT16 "u")
  (UNSPEC_MATMUL_S "s") (UNSPEC_MATMUL_U "u") (UNSPEC_MATMUL_US "us")
])

(define_int_attr vfml_half
 [(UNSPEC_VFML_HI "high") (UNSPEC_VFML_LO "low")])

(define_int_attr vfml_half_selector
 [(UNSPEC_VFML_HI "true") (UNSPEC_VFML_LO "false")])

(define_int_attr vcvth_op
 [(UNSPEC_VCVTA_S "a") (UNSPEC_VCVTA_U "a")
  (UNSPEC_VCVTM_S "m") (UNSPEC_VCVTM_U "m")
  (UNSPEC_VCVTN_S "n") (UNSPEC_VCVTN_U "n")
  (UNSPEC_VCVTP_S "p") (UNSPEC_VCVTP_U "p")])

(define_int_attr fp16_rnd_str
  [(UNSPEC_VRND "rnd") (UNSPEC_VRNDA "rnda")
   (UNSPEC_VRNDM "rndm") (UNSPEC_VRNDN "rndn")
   (UNSPEC_VRNDP "rndp") (UNSPEC_VRNDX "rndx")])

(define_int_attr fp16_rnd_insn
  [(UNSPEC_VRND "vrintz") (UNSPEC_VRNDA "vrinta")
   (UNSPEC_VRNDM "vrintm") (UNSPEC_VRNDN "vrintn")
   (UNSPEC_VRNDP "vrintp") (UNSPEC_VRNDX "vrintx")])

(define_int_attr cmp_op_unsp [(UNSPEC_VCEQ "eq") (UNSPEC_VCGT "gt")
			      (UNSPEC_VCGE "ge") (UNSPEC_VCLE "le")
			      (UNSPEC_VCLT "lt") (UNSPEC_VCAGE "ge")
			      (UNSPEC_VCAGT "gt") (UNSPEC_VCALE "le")
			      (UNSPEC_VCALT "lt")])

(define_int_attr r [
  (UNSPEC_VRHADD_S "r") (UNSPEC_VRHADD_U "r")
  (UNSPEC_VHADD_S "") (UNSPEC_VHADD_U "")
  (UNSPEC_VADDHN "") (UNSPEC_VRADDHN "r")
  (UNSPEC_VQDMULH "") (UNSPEC_VQRDMULH "r")
  (UNSPEC_VQDMULH_LANE "") (UNSPEC_VQRDMULH_LANE "r")
  (UNSPEC_VSUBHN "") (UNSPEC_VRSUBHN "r")
])

(define_int_attr maxmin [
  (UNSPEC_VMAX "max") (UNSPEC_VMAX_U "max")
  (UNSPEC_VMIN "min") (UNSPEC_VMIN_U "min")
  (UNSPEC_VPMAX "max") (UNSPEC_VPMAX_U "max")
  (UNSPEC_VPMIN "min") (UNSPEC_VPMIN_U "min")
])

(define_int_attr fmaxmin [
  (UNSPEC_VMAXNM "fmax") (UNSPEC_VMINNM "fmin")])

(define_int_attr fmaxmin_op [
  (UNSPEC_VMAXNM "vmaxnm") (UNSPEC_VMINNM "vminnm")
])

(define_int_attr shift_op [
  (UNSPEC_VSHL_S "shl") (UNSPEC_VSHL_U "shl")
  (UNSPEC_VRSHL_S "rshl") (UNSPEC_VRSHL_U "rshl")
  (UNSPEC_VQSHL_S "qshl") (UNSPEC_VQSHL_U "qshl")
  (UNSPEC_VQRSHL_S "qrshl") (UNSPEC_VQRSHL_U "qrshl")
  (UNSPEC_VSHR_S_N "shr") (UNSPEC_VSHR_U_N "shr")
  (UNSPEC_VRSHR_S_N "rshr") (UNSPEC_VRSHR_U_N "rshr")
  (UNSPEC_VSHRN_N "shrn") (UNSPEC_VRSHRN_N "rshrn")
  (UNSPEC_VQRSHRN_S_N "qrshrn") (UNSPEC_VQRSHRN_U_N "qrshrn")
  (UNSPEC_VQSHRN_S_N "qshrn") (UNSPEC_VQSHRN_U_N "qshrn")
  (UNSPEC_VQSHRUN_N "qshrun") (UNSPEC_VQRSHRUN_N "qrshrun")
  (UNSPEC_VSRA_S_N "sra") (UNSPEC_VSRA_U_N "sra")
  (UNSPEC_VRSRA_S_N "rsra") (UNSPEC_VRSRA_U_N "rsra")
])

;; Standard names for floating point to integral rounding instructions.
(define_int_attr vrint_pattern [(UNSPEC_VRINTZ "btrunc") (UNSPEC_VRINTP "ceil")
                         (UNSPEC_VRINTA "round") (UNSPEC_VRINTM "floor")
                         (UNSPEC_VRINTR "nearbyint") (UNSPEC_VRINTX "rint")])

;; Suffixes for vrint instructions specifying rounding modes.
(define_int_attr vrint_variant [(UNSPEC_VRINTZ "z") (UNSPEC_VRINTP "p")
                               (UNSPEC_VRINTA "a") (UNSPEC_VRINTM "m")
                               (UNSPEC_VRINTR "r") (UNSPEC_VRINTX "x")])

;; Some of the vrint instuctions are predicable.
(define_int_attr vrint_predicable [(UNSPEC_VRINTZ "yes") (UNSPEC_VRINTP "no")
                                  (UNSPEC_VRINTA "no") (UNSPEC_VRINTM "no")
                                  (UNSPEC_VRINTR "yes") (UNSPEC_VRINTX "yes")])

(define_int_attr vrint_conds [(UNSPEC_VRINTZ "nocond") (UNSPEC_VRINTP "unconditional")
                              (UNSPEC_VRINTA "unconditional") (UNSPEC_VRINTM "unconditional")
                              (UNSPEC_VRINTR "nocond") (UNSPEC_VRINTX "nocond")])

(define_int_attr nvrint_pattern [(UNSPEC_NVRINTZ "btrunc")
				 (UNSPEC_NVRINTP "ceil")
				 (UNSPEC_NVRINTA "round")
				 (UNSPEC_NVRINTM "floor")
				 (UNSPEC_NVRINTX "rint")
				 (UNSPEC_NVRINTN "roundeven")])

(define_int_attr nvrint_variant [(UNSPEC_NVRINTZ "z") (UNSPEC_NVRINTP "p")
                                (UNSPEC_NVRINTA "a") (UNSPEC_NVRINTM "m")
                                (UNSPEC_NVRINTX "x") (UNSPEC_NVRINTN "n")])

(define_int_attr crc_variant [(UNSPEC_CRC32B "crc32b") (UNSPEC_CRC32H "crc32h")
                        (UNSPEC_CRC32W "crc32w") (UNSPEC_CRC32CB "crc32cb")
                        (UNSPEC_CRC32CH "crc32ch") (UNSPEC_CRC32CW "crc32cw")])

(define_int_attr crc_mode [(UNSPEC_CRC32B "QI") (UNSPEC_CRC32H "HI")
                        (UNSPEC_CRC32W "SI") (UNSPEC_CRC32CB "QI")
                        (UNSPEC_CRC32CH "HI") (UNSPEC_CRC32CW "SI")])

(define_int_attr crypto_pattern [(UNSPEC_SHA1H "sha1h") (UNSPEC_AESMC "aesmc")
                          (UNSPEC_AESIMC "aesimc") (UNSPEC_AESD "aesd")
                          (UNSPEC_AESE "aese") (UNSPEC_SHA1SU1 "sha1su1")
                          (UNSPEC_SHA256SU0 "sha256su0") (UNSPEC_SHA1C "sha1c")
                          (UNSPEC_SHA1M "sha1m") (UNSPEC_SHA1P "sha1p")
                          (UNSPEC_SHA1SU0 "sha1su0") (UNSPEC_SHA256H "sha256h")
                          (UNSPEC_SHA256H2 "sha256h2")
                          (UNSPEC_SHA256SU1 "sha256su1")])

(define_int_attr crypto_type
 [(UNSPEC_AESE "crypto_aese") (UNSPEC_AESD "crypto_aese")
 (UNSPEC_AESMC "crypto_aesmc") (UNSPEC_AESIMC "crypto_aesmc")
 (UNSPEC_SHA1C "crypto_sha1_slow") (UNSPEC_SHA1P "crypto_sha1_slow")
 (UNSPEC_SHA1M "crypto_sha1_slow") (UNSPEC_SHA1SU1 "crypto_sha1_fast")
 (UNSPEC_SHA1SU0 "crypto_sha1_xor") (UNSPEC_SHA256H "crypto_sha256_slow")
 (UNSPEC_SHA256H2 "crypto_sha256_slow") (UNSPEC_SHA256SU0 "crypto_sha256_fast")
 (UNSPEC_SHA256SU1 "crypto_sha256_slow")])

(define_int_attr crypto_size_sfx [(UNSPEC_SHA1H "32") (UNSPEC_AESMC "8")
                          (UNSPEC_AESIMC "8") (UNSPEC_AESD "8")
                          (UNSPEC_AESE "8") (UNSPEC_SHA1SU1 "32")
                          (UNSPEC_SHA256SU0 "32") (UNSPEC_SHA1C "32")
                          (UNSPEC_SHA1M "32") (UNSPEC_SHA1P "32")
                          (UNSPEC_SHA1SU0 "32") (UNSPEC_SHA256H "32")
                          (UNSPEC_SHA256H2 "32") (UNSPEC_SHA256SU1 "32")])

(define_int_attr crypto_mode [(UNSPEC_SHA1H "V4SI") (UNSPEC_AESMC "V16QI")
                          (UNSPEC_AESIMC "V16QI") (UNSPEC_AESD "V16QI")
                          (UNSPEC_AESE "V16QI") (UNSPEC_SHA1SU1 "V4SI")
                          (UNSPEC_SHA256SU0 "V4SI") (UNSPEC_SHA1C "V4SI")
                          (UNSPEC_SHA1M "V4SI") (UNSPEC_SHA1P "V4SI")
                          (UNSPEC_SHA1SU0 "V4SI") (UNSPEC_SHA256H "V4SI")
                          (UNSPEC_SHA256H2 "V4SI") (UNSPEC_SHA256SU1 "V4SI")])

(define_int_attr rot [(UNSPEC_VCADD90 "90")
		      (UNSPEC_VCADD270 "270")
		      (VCADDQ_ROT90_M_F "90")
		      (VCADDQ_ROT90_M "90")
		      (VCADDQ_ROT270_M_F "270")
		      (VCADDQ_ROT270_M "270")
		      (VHCADDQ_ROT90_S "90")
		      (VHCADDQ_ROT270_S "270")
		      (VHCADDQ_ROT90_M_S "90")
		      (VHCADDQ_ROT270_M_S "270")
		      (UNSPEC_VCMUL "0")
		      (UNSPEC_VCMUL90 "90")
		      (UNSPEC_VCMUL180 "180")
		      (UNSPEC_VCMUL270 "270")
		      (UNSPEC_VCMLA "0")
		      (UNSPEC_VCMLA90 "90")
		      (UNSPEC_VCMLA180 "180")
		      (UNSPEC_VCMLA270 "270")
		      (VCMULQ_M_F "0")
		      (VCMULQ_ROT90_M_F "90")
		      (VCMULQ_ROT180_M_F "180")
		      (VCMULQ_ROT270_M_F "270")
		      (VCMLAQ_M_F "0")
		      (VCMLAQ_ROT90_M_F "90")
		      (VCMLAQ_ROT180_M_F "180")
		      (VCMLAQ_ROT270_M_F "270")
		      ])

;; The complex operations when performed on a real complex number require two
;; instructions to perform the operation. e.g. complex multiplication requires
;; two VCMUL with a particular rotation value.
;;
;; These values can be looked up in rotsplit1 and rotsplit2.  as an example
;; VCMUL needs the first instruction to use #0 and the second #90.
(define_int_attr rotsplit1 [(UNSPEC_VCMLA "0")
			    (UNSPEC_VCMLA_CONJ "0")
			    (UNSPEC_VCMUL "0")
			    (UNSPEC_VCMUL_CONJ "0")
			    (UNSPEC_VCMLA180 "180")
			    (UNSPEC_VCMLA180_CONJ "180")])

(define_int_attr rotsplit2 [(UNSPEC_VCMLA "90")
			    (UNSPEC_VCMLA_CONJ "270")
			    (UNSPEC_VCMUL "90")
			    (UNSPEC_VCMUL_CONJ "270")
			    (UNSPEC_VCMLA180 "270")
			    (UNSPEC_VCMLA180_CONJ "90")])

(define_int_attr conj_op [(UNSPEC_VCMLA180 "")
			  (UNSPEC_VCMLA180_CONJ "_conj")
			  (UNSPEC_VCMLA "")
			  (UNSPEC_VCMLA_CONJ "_conj")
			  (UNSPEC_VCMUL "")
			  (UNSPEC_VCMUL_CONJ "_conj")])

(define_int_attr mve_rot [(UNSPEC_VCADD90 "_rot90")
			  (UNSPEC_VCADD270 "_rot270")
			  (VCADDQ_ROT90_M_F "_rot90")
			  (VCADDQ_ROT90_M "_rot90")
			  (VCADDQ_ROT270_M_F "_rot270")
			  (VCADDQ_ROT270_M "_rot270")
			  (VHCADDQ_ROT90_S "_rot90")
			  (VHCADDQ_ROT270_S "_rot270")
			  (VHCADDQ_ROT90_M_S "_rot90")
			  (VHCADDQ_ROT270_M_S "_rot270")
			  (UNSPEC_VCMLA "")
			  (UNSPEC_VCMLA90 "_rot90")
			  (UNSPEC_VCMLA180 "_rot180")
			  (UNSPEC_VCMLA270 "_rot270")
			  (UNSPEC_VCMUL "")
			  (UNSPEC_VCMUL90 "_rot90")
			  (UNSPEC_VCMUL180 "_rot180")
			  (UNSPEC_VCMUL270 "_rot270")
			  (VCMULQ_M_F "")
			  (VCMULQ_ROT90_M_F "_rot90")
			  (VCMULQ_ROT180_M_F "_rot180")
			  (VCMULQ_ROT270_M_F "_rot270")
			  (VCMLAQ_M_F "")
			  (VCMLAQ_ROT90_M_F "_rot90")
			  (VCMLAQ_ROT180_M_F "_rot180")
			  (VCMLAQ_ROT270_M_F "_rot270")])

(define_int_attr fcmac1 [(UNSPEC_VCMLA "a") (UNSPEC_VCMLA_CONJ "a")
			 (UNSPEC_VCMLA180 "s") (UNSPEC_VCMLA180_CONJ "s")])

(define_int_attr simd32_op [(UNSPEC_QADD8 "qadd8") (UNSPEC_QSUB8 "qsub8")
			    (UNSPEC_SHADD8 "shadd8") (UNSPEC_SHSUB8 "shsub8")
			    (UNSPEC_UHADD8 "uhadd8") (UNSPEC_UHSUB8 "uhsub8")
			    (UNSPEC_UQADD8 "uqadd8") (UNSPEC_UQSUB8 "uqsub8")
			    (UNSPEC_QADD16 "qadd16") (UNSPEC_QASX "qasx")
			    (UNSPEC_QSAX "qsax") (UNSPEC_QSUB16 "qsub16")
			    (UNSPEC_SHADD16 "shadd16") (UNSPEC_SHASX "shasx")
			    (UNSPEC_SHSAX "shsax") (UNSPEC_SHSUB16 "shsub16")
			    (UNSPEC_UHADD16 "uhadd16") (UNSPEC_UHASX "uhasx")
			    (UNSPEC_UHSAX "uhsax") (UNSPEC_UHSUB16 "uhsub16")
			    (UNSPEC_UQADD16 "uqadd16") (UNSPEC_UQASX "uqasx")
			    (UNSPEC_UQSAX "uqsax") (UNSPEC_UQSUB16 "uqsub16")
			    (UNSPEC_SMUSD "smusd") (UNSPEC_SMUSDX "smusdx")
			    (UNSPEC_SXTAB16 "sxtab16") (UNSPEC_UXTAB16 "uxtab16")
			    (UNSPEC_USAD8 "usad8") (UNSPEC_SMLALD "smlald")
			    (UNSPEC_SMLALDX "smlaldx") (UNSPEC_SMLSLD "smlsld")
			    (UNSPEC_SMLSLDX "smlsldx")(UNSPEC_SADD8 "sadd8")
			    (UNSPEC_UADD8 "uadd8") (UNSPEC_SSUB8 "ssub8")
			    (UNSPEC_USUB8 "usub8") (UNSPEC_SADD16 "sadd16")
			    (UNSPEC_SASX "sasx") (UNSPEC_SSAX "ssax")
			    (UNSPEC_SSUB16 "ssub16") (UNSPEC_UADD16 "uadd16")
			    (UNSPEC_UASX "uasx") (UNSPEC_USAX "usax")
			    (UNSPEC_USUB16 "usub16") (UNSPEC_SMLAD "smlad")
			    (UNSPEC_SMLADX "smladx") (UNSPEC_SMLSD "smlsd")
			    (UNSPEC_SMLSDX "smlsdx") (UNSPEC_SMUAD "smuad")
			    (UNSPEC_SMUADX "smuadx") (UNSPEC_SSAT16 "ssat16")
			    (UNSPEC_USAT16 "usat16")])

(define_int_attr mmla_sfx [(UNSPEC_MATMUL_S "s8") (UNSPEC_MATMUL_U "u8")
			   (UNSPEC_MATMUL_US "s8")])
;;MVE int attribute.
(define_int_attr supf [(VCVTQ_TO_F_S "s") (VCVTQ_TO_F_U "u") (VREV16Q_S "s")
		       (VREV16Q_U "u") (VMVNQ_N_S "s") (VMVNQ_N_U "u")
		       (VCVTAQ_U "u") (VCVTAQ_S "s") (VREV64Q_S "s")
		       (VREV64Q_U "u")
		       (VDUPQ_N_U "u") (VDUPQ_N_S"s") (VADDVQ_S "s")
		       (VADDVQ_U "u") (VADDVQ_S "s") (VADDVQ_U "u")
		       (VMOVLTQ_U "u") (VMOVLTQ_S "s") (VMOVLBQ_S "s")
		       (VMOVLBQ_U "u") (VCVTQ_FROM_F_S "s") (VCVTQ_FROM_F_U "u")
		       (VCVTPQ_S "s") (VCVTPQ_U "u") (VCVTNQ_S "s")
		       (VCVTNQ_U "u") (VCVTMQ_S "s") (VCVTMQ_U "u")
		       (VREV32Q_U "u")
		       (VREV32Q_S "s") (VADDLVQ_U "u") (VADDLVQ_S "s")
		       (VCVTQ_N_TO_F_S "s") (VCVTQ_N_TO_F_U "u")
		       (VCREATEQ_U "u") (VCREATEQ_S "s") (VSHRQ_N_S "s")
		       (VSHRQ_N_U "u") (VCVTQ_N_FROM_F_S "s") (VSHLQ_U "u")
		       (VCVTQ_N_FROM_F_U "u") (VADDLVQ_P_S "s") (VSHLQ_S "s")
		       (VADDLVQ_P_U "u")
		       (VABDQ_M_S "s") (VABDQ_M_U "u") (VABDQ_S "s")
		       (VABDQ_U "u") (VADDQ_N_S "s") (VADDQ_N_U "u")
		       (VADDVQ_P_S "s")	(VADDVQ_P_U "u") (VBRSRQ_N_S "s")
		       (VBRSRQ_N_U "u")
		       (VHADDQ_N_S "s") (VHADDQ_N_U "u") (VHADDQ_S "s")
		       (VHADDQ_U "u") (VHSUBQ_N_S "s")	(VHSUBQ_N_U "u")
		       (VHSUBQ_S "s") (VMAXQ_S "s") (VMAXQ_U "u") (VHSUBQ_U "u")
		       (VMAXVQ_S "s") (VMAXVQ_U "u") (VMINQ_S "s") (VMINQ_U "u")
		       (VMINVQ_S "s") (VMINVQ_U "u") (VMLADAVQ_S "s")
		       (VMLADAVQ_U "u") (VMULHQ_S "s") (VMULHQ_U "u")
		       (VMULLBQ_INT_S "s") (VMULLBQ_INT_U "u") (VQADDQ_S "s")
		       (VMULLTQ_INT_S "s") (VMULLTQ_INT_U "u") (VQADDQ_U "u")
		       (VMULLBQ_POLY_P "p")
		       (VMULLTQ_POLY_P "p")
		       (VMULLBQ_POLY_M_P "p")
		       (VMULLTQ_POLY_M_P "p")
		       (VMULQ_N_S "s") (VMULQ_N_U "u") (VMULQ_S "s")
		       (VMULQ_U "u")
		       (VQADDQ_N_S "s") (VQADDQ_N_U "u")
		       (VQRSHLQ_N_S "s") (VQRSHLQ_N_U "u") (VQRSHLQ_S "s")
		       (VQRSHLQ_U "u") (VQSHLQ_N_S "s")	(VQSHLQ_N_U "u")
		       (VQSHLQ_R_S "s") (VQSHLQ_R_U "u") (VQSHLQ_S "s")
		       (VQSHLQ_U "u") (VQSUBQ_N_S "s") (VQSUBQ_N_U "u")
		       (VQSUBQ_S "s") (VQSUBQ_U "u") (VRHADDQ_S "s")
		       (VRHADDQ_U "u") (VRMULHQ_S "s") (VRMULHQ_U "u")
		       (VRSHLQ_N_S "s") (VRSHLQ_N_U "u") (VRSHLQ_S "s")
		       (VRSHLQ_U "u") (VRSHRQ_N_S "s") (VRSHRQ_N_U "u")
		       (VSHLQ_N_S "s") (VSHLQ_N_U "u") (VSHLQ_R_S "s")
		       (VSHLQ_R_U "u") (VSUBQ_N_S "s") (VSUBQ_N_U "u")
		       (VSUBQ_S "s") (VSUBQ_U "u") (VADDVAQ_S "s")
		       (VADDVAQ_U "u") (VADDLVAQ_S "s") (VADDLVAQ_U "u")
		       (VBICQ_N_S "s") (VBICQ_N_U "u") (VMLALDAVQ_U "u")
		       (VMLALDAVQ_S "s") (VMLALDAVXQ_S "s")
		       (VMOVNBQ_U "u") (VMOVNBQ_S "s") (VMOVNTQ_U "u")
		       (VMOVNTQ_S "s") (VORRQ_N_S "s") (VORRQ_N_U "u")
		       (VQMOVNBQ_U "u") (VQMOVNBQ_S "s") (VQMOVNTQ_S "s")
		       (VQMOVNTQ_U "u") (VSHLLBQ_N_U "u") (VSHLLBQ_N_S "s")
		       (VSHLLTQ_N_U "u") (VSHLLTQ_N_S "s") (VRMLALDAVHQ_U "u")
		       (VRMLALDAVHQ_S "s") (VBICQ_M_N_S "s") (VBICQ_M_N_U "u")
		       (VCVTAQ_M_S "s") (VCVTAQ_M_U "u") (VCVTQ_M_TO_F_S "s")
		       (VCVTQ_M_TO_F_U "u") (VQRSHRNBQ_N_S "s")
		       (VQRSHRNBQ_N_U "u") (VABAVQ_S "s") (VABAVQ_U "u")
		       (VRMLALDAVHAQ_U "u") (VRMLALDAVHAQ_S "s") (VSHLCQ_S "s")
		       (VSHLCQ_U "u") (VADDVAQ_P_S "s") (VADDVAQ_P_U "u")
		       (VCLZQ_M_S "s") (VCLZQ_M_U "u") (VCMPEQQ_M_N_S "s")
		       (VCMPEQQ_M_N_U "u") (VCMPEQQ_M_S "s") (VCMPEQQ_M_U "u")
		       (VCMPNEQ_M_N_S "s") (VCMPNEQ_M_N_U "u") (VCMPNEQ_M_S "s")
		       (VCMPNEQ_M_U "u") (VDUPQ_M_N_S "s") (VDUPQ_M_N_U "u")
		       (VMAXVQ_P_S "s") (VMAXVQ_P_U "u") (VMINVQ_P_S "s")
		       (VMINVQ_P_U "u") (VMLADAVAQ_S "s") (VMLADAVAQ_U "u")
		       (VMLADAVQ_P_S "s") (VMLADAVQ_P_U "u") (VMLAQ_N_S "s")
		       (VMLAQ_N_U "u") (VMLASQ_N_S "s") (VMLASQ_N_U "u")
		       (VMVNQ_M_S "s") (VMVNQ_M_U "u") (VPSELQ_S "s")
		       (VPSELQ_U "u") (VQDMLAHQ_N_S "s")
		       (VQDMLASHQ_N_S "s")
		       (VQRDMLAHQ_N_S "s")
		       (VQRDMLASHQ_N_S "s")
		       (VQRSHLQ_M_N_S "s") (VQRSHLQ_M_N_U "u")
		       (VQSHLQ_M_R_S "s") (VQSHLQ_M_R_U "u") (VSRIQ_N_S "s")
		       (VREV64Q_M_S "s") (VREV64Q_M_U "u") (VSRIQ_N_U "u")
		       (VRSHLQ_M_N_S "s") (VRSHLQ_M_N_U "u") (VSHLQ_M_R_S "s")
		       (VSHLQ_M_R_U "u") (VSLIQ_N_S "s") (VSLIQ_N_U "u")
		       (VMLALDAVQ_P_S "s") (VQMOVNBQ_M_S "s") (VMOVLTQ_M_S "s")
		       (VMOVNBQ_M_S "s") (VRSHRNTQ_N_S "s") (VORRQ_M_N_S "s")
		       (VREV32Q_M_S "s") (VQRSHRNTQ_N_S "s") (VMOVNTQ_M_S "s")
		       (VMOVLBQ_M_S "s") (VMLALDAVAQ_S "s") (VQSHRNBQ_N_S "s")
		       (VSHRNBQ_N_S "s") (VRSHRNBQ_N_S "s") (VMLALDAVXQ_P_S "s")
		       (VQMOVNTQ_M_S "s") (VMVNQ_M_N_S "s") (VQSHRNTQ_N_S "s")
		       (VMLALDAVAXQ_S "s") (VSHRNTQ_N_S "s") (VMLALDAVQ_P_U "u")
		       (VQMOVNBQ_M_U "u") (VMOVLTQ_M_U "u") (VMOVNBQ_M_U "u")
		       (VRSHRNTQ_N_U "u") (VORRQ_M_N_U "u") (VREV32Q_M_U "u")
		       (VREV16Q_M_S "s") (VREV16Q_M_U "u")
		       (VQRSHRNTQ_N_U "u") (VMOVNTQ_M_U "u") (VMOVLBQ_M_U "u")
		       (VMLALDAVAQ_U "u") (VQSHRNBQ_N_U "u") (VSHRNBQ_N_U "u")
		       (VRSHRNBQ_N_U "u")
		       (VMVNQ_M_N_U "u") (VQSHRNTQ_N_U "u")
		       (VQMOVNTQ_M_U "u") (VSHRNTQ_N_U "u") (VCVTMQ_M_S "s")
		       (VCVTMQ_M_U "u") (VCVTNQ_M_S "s") (VCVTNQ_M_U "u")
		       (VCVTPQ_M_S "s") (VCVTPQ_M_U "u") (VADDLVAQ_P_S "s")
		       (VCVTQ_M_N_FROM_F_U "u") (VCVTQ_M_FROM_F_S "s")
		       (VCVTQ_M_FROM_F_U "u") (VRMLALDAVHQ_P_U "u")
		       (VRMLALDAVHQ_P_S "s") (VADDLVAQ_P_U "u")
		       (VCVTQ_M_N_FROM_F_S "s") (VABAVQ_P_U "u")
		       (VABAVQ_P_S "s") (VSHLQ_M_S "s") (VSHLQ_M_U "u")
		       (VSRIQ_M_N_S "s") (VSRIQ_M_N_U "u") (VSUBQ_M_S "s")
		       (VSUBQ_M_U "u") (VCVTQ_M_N_TO_F_S "s")
		       (VCVTQ_M_N_TO_F_U "u") (VADDQ_M_N_U "u")
		       (VSHLQ_M_N_S "s") (VMAXQ_M_U "u") (VHSUBQ_M_N_U "u")
		       (VMULQ_M_N_S "s") (VQSHLQ_M_U "u") (VRHADDQ_M_S "s")
		       (VEORQ_M_U "u") (VSHRQ_M_N_U "u")
		       (VMLADAVAQ_P_U "u") (VEORQ_M_S "s") (VBRSRQ_M_N_S "s")
		       (VMULQ_M_U "u") (VQRDMLAHQ_M_N_S "s") (VHSUBQ_M_N_S "s")
		       (VQRSHLQ_M_S "s") (VMULQ_M_N_U "u")
		       (VMULQ_M_S "s") (VQSHLQ_M_N_U "u") (VSLIQ_M_N_U "u")
		       (VMLADAVAQ_P_S "s") (VQRSHLQ_M_U "u")
		       (VMULLBQ_INT_M_U "u") (VSHLQ_M_N_U "u") (VQSUBQ_M_U "u")
		       (VQDMLASHQ_M_N_S "s")
		       (VQRDMLASHQ_M_N_U "u") (VRSHRQ_M_N_S "s")
		       (VORNQ_M_S "s") (VCADDQ_ROT270_M "") (VRHADDQ_M_U "u")
		       (VRSHRQ_M_N_U "u") (VMLASQ_M_N_U "u") (VHSUBQ_M_U "u")
		       (VQSUBQ_M_N_S "s") (VMULLTQ_INT_M_S "s")
		       (VORRQ_M_S "s") (VQDMLAHQ_M_N_U "u") (VRSHLQ_M_S "s")
		       (VHADDQ_M_U "u") (VHADDQ_M_N_S "s") (VMULLTQ_INT_M_U "u")
		       (VORRQ_M_U "u") (VHADDQ_M_S "s") (VHADDQ_M_N_U "u")
		       (VQDMLAHQ_M_N_S "s") (VMAXQ_M_S "s") (VORNQ_M_U "u")
		       (VQADDQ_M_U "u")
		       (VQRDMLASHQ_M_N_S "s") (VBICQ_M_U "u") (VMINQ_M_U "u")
		       (VSUBQ_M_N_S "s") (VMULLBQ_INT_M_S "s") (VQSUBQ_M_S "s")
		       (VCADDQ_ROT90_M "") (VRMULHQ_M_S "s") (VANDQ_M_U "u")
		       (VMULHQ_M_S "s") (VADDQ_M_S "s") (VQRDMLAHQ_M_N_U "u")
		       (VMLASQ_M_N_S "s") (VHSUBQ_M_S "s") (VRMULHQ_M_U "u")
		       (VQADDQ_M_N_S "s") (VSHRQ_M_N_S "s") (VANDQ_M_S "s")
		       (VABDQ_M_U "u") (VQSHLQ_M_S "s") (VABDQ_M_S "s")
		       (VSUBQ_M_N_U "u") (VMLAQ_M_N_S "s") (VBRSRQ_M_N_U "u")
		       (VADDQ_M_U "u") (VRSHLQ_M_U "u") (VSLIQ_M_N_S "s")
		       (VQADDQ_M_N_U "u") (VADDQ_M_N_S "s") (VQSUBQ_M_N_U "u")
		       (VMLAQ_M_N_U "u") (VMINQ_M_S "s") (VMULHQ_M_U "u")
		       (VQADDQ_M_S "s") (VBICQ_M_S "s") (VQSHLQ_M_N_S "s")
		       (VQSHRNTQ_M_N_S "s") (VQSHRNTQ_M_N_U "u")
		       (VSHRNTQ_M_N_U "u") (VSHRNTQ_M_N_S "s")
		       (VSHRNBQ_M_N_S "s") (VSHRNBQ_M_N_U "u")
		       (VSHLLTQ_M_N_S "s") (VSHLLTQ_M_N_U "u")
		       (VSHLLBQ_M_N_S "s") (VSHLLBQ_M_N_U "u")
		       (VRSHRNTQ_M_N_S "s") (VRSHRNTQ_M_N_U "u")
		       (VRSHRNBQ_M_N_U "u") (VRSHRNBQ_M_N_S "s")
		       (VQSHRNTQ_M_N_U "u") (VQSHRNTQ_M_N_S "s")
		       (VQSHRNBQ_M_N_S "s") (VQSHRNBQ_M_N_U "u")
		       (VQRSHRNTQ_M_N_S "s") (VQRSHRNTQ_M_N_U "u")
		       (VQRSHRNBQ_M_N_S "s") (VQRSHRNBQ_M_N_U "u")
		       (VMLALDAVAXQ_P_S "s")
		       (VMLALDAVAQ_P_S "s") (VMLALDAVAQ_P_U "u")
		       (VSTRWQSB_S "s") (VSTRWQSB_U "u") (VSTRBQSO_S "s")
		       (VSTRBQSO_U "u") (VSTRBQ_S "s") (VSTRBQ_U "u")
		       (VLDRBQGO_S "s") (VLDRBQGO_U "u") (VLDRBQ_S "s")
		       (VLDRBQ_U "u") (VLDRWQGB_S "s") (VLDRWQGB_U "u")
		       (VLD1Q_S "s") (VLD1Q_U "u") (VLDRHQGO_S "s")
		       (VLDRHQGO_U "u") (VLDRHQGSO_S "s") (VLDRHQGSO_U "u")
		       (VLDRHQ_S "s") (VLDRHQ_U "u") (VLDRWQ_S "s")
		       (VLDRWQ_U "u") (VLDRDQGB_S "s") (VLDRDQGB_U "u")
		       (VLDRDQGO_S "s") (VLDRDQGO_U "u") (VLDRDQGSO_S "s")
		       (VLDRDQGSO_U "u") (VLDRWQGO_S "s") (VLDRWQGO_U "u")
		       (VLDRWQGSO_S "s") (VLDRWQGSO_U "u") (VST1Q_S "s")
		       (VST1Q_U "u") (VSTRHQSO_S "s") (VSTRHQSO_U "u")
		       (VSTRHQSSO_S "s") (VSTRHQSSO_U "u") (VSTRHQ_S "s")
		       (VSTRHQ_U "u") (VSTRWQ_S "s") (VSTRWQ_U "u")
		       (VSTRDQSB_S "s") (VSTRDQSB_U "u") (VSTRDQSO_S "s")
		       (VSTRDQSO_U "u") (VSTRDQSSO_S "s") (VSTRDQSSO_U "u")
		       (VSTRWQSO_U "u") (VSTRWQSO_S "s") (VSTRWQSSO_U "u")
		       (VSTRWQSSO_S "s") (VSTRWQSBWB_S "s") (VSTRWQSBWB_U "u")
		       (VLDRWQGBWB_S "s") (VLDRWQGBWB_U "u") (VLDRDQGBWB_S "s")
		       (VLDRDQGBWB_U "u") (VSTRDQSBWB_S "s") (VADCQ_M_S "s")
		       (VSTRDQSBWB_U "u") (VSBCQ_U "u")  (VSBCQ_M_U "u")
		       (VSBCQ_S "s")  (VSBCQ_M_S "s") (VSBCIQ_U "u")
		       (VSBCIQ_M_U "u") (VSBCIQ_S "s") (VSBCIQ_M_S "s")
		       (VADCQ_U "u")  (VADCQ_M_U "u") (VADCQ_S "s")
		       (VADCIQ_U "u") (VADCIQ_M_U "u") (VADCIQ_S "s")
		       (VADCIQ_M_S "s") (SQRSHRL_64 "64") (SQRSHRL_48 "48")
		       (UQRSHLL_64 "64") (UQRSHLL_48 "48") (VSHLCQ_M_S "s")
		       (VSHLCQ_M_U "u")
		       (VQDMLADHQ_M_S "s")
		       (VQDMLADHXQ_M_S "s")
		       (VQDMLSDHQ_M_S "s")
		       (VQDMLSDHXQ_M_S "s")
		       (VQDMULHQ_M_S "s")
		       (VQRDMLADHQ_M_S "s")
		       (VQRDMLADHXQ_M_S "s")
		       (VQRDMLSDHQ_M_S "s")
		       (VQRDMLSDHXQ_M_S "s")
		       (VQRDMULHQ_M_S "s")
		       (VQDMULHQ_N_S "s")
		       (VQRDMULHQ_N_S "s")
		       (VQDMLAHQ_M_N_S "s")
		       (VQDMLASHQ_M_N_S "s")
		       (VQRDMLAHQ_M_N_S "s")
		       (VQRDMLASHQ_M_N_S "s")
		       (VQDMULHQ_M_N_S "s")
		       (VQRDMULHQ_M_N_S "s")
		       (VQDMULHQ_S "s")
		       (VQRDMULHQ_S "s")
		       (VQRSHRUNBQ_M_N_S "s")
		       (VQRSHRUNBQ_N_S "s")
		       (VQRSHRUNTQ_M_N_S "s")
		       (VQRSHRUNTQ_N_S "s")
		       (VQSHRUNBQ_M_N_S "s")
		       (VQSHRUNBQ_N_S "s")
		       (VQSHRUNTQ_M_N_S "s")
		       (VQSHRUNTQ_N_S "s")
		       (VABSQ_M_S "s")
		       (VCLSQ_M_S "s")
		       (VCLZQ_M_S "s") (VCLZQ_M_U "u")
		       (VNEGQ_M_S "s")
		       (VQABSQ_M_S "s")
		       (VQNEGQ_M_S "s")
		       (VCLSQ_S "s")
		       (VQABSQ_S "s")
		       (VQNEGQ_S "s")
		       (VQMOVUNBQ_M_S "s")
		       (VQMOVUNBQ_S "s")
		       (VQMOVUNTQ_M_S "s")
		       (VQMOVUNTQ_S "s")
		       (VMAXAVQ_S "s")
		       (VMAXAVQ_P_S "s")
		       (VMINAVQ_S "s")
		       (VMINAVQ_P_S "s")
		       (VMAXAQ_S "s")
		       (VMAXAQ_M_S "s")
		       (VMINAQ_S "s")
		       (VMINAQ_M_S "s")
		       (VCMPCSQ_M_N_U "u")
		       (VCMPCSQ_M_U "u")
		       (VCMPEQQ_M_N_S "s") (VCMPEQQ_M_N_U "u")
		       (VCMPEQQ_M_S "s") (VCMPEQQ_M_U "u")
		       (VCMPGEQ_M_N_S "s")
		       (VCMPGEQ_M_S "s")
		       (VCMPGTQ_M_N_S "s")
		       (VCMPGTQ_M_S "s")
		       (VCMPHIQ_M_N_U "u")
		       (VCMPHIQ_M_U "u")
		       (VCMPLEQ_M_N_S "s")
		       (VCMPLEQ_M_S "s")
		       (VCMPLTQ_M_N_S "s")
		       (VCMPLTQ_M_S "s")
		       (VCMPNEQ_M_N_S "s") (VCMPNEQ_M_N_U "u")
		       (VCMPNEQ_M_S "s") (VCMPNEQ_M_U "u")
		       (VMLADAVAXQ_P_S "s")
		       (VMLADAVAXQ_S "s")
		       (VMLADAVXQ_P_S "s")
		       (VMLADAVXQ_S "s")
		       (VMLSDAVAQ_P_S "s")
		       (VMLSDAVAQ_S "s")
		       (VMLSDAVAXQ_P_S "s")
		       (VMLSDAVAXQ_S "s")
		       (VMLSDAVQ_P_S "s")
		       (VMLSDAVQ_S "s")
		       (VMLSDAVXQ_P_S "s")
		       (VMLSDAVXQ_S "s")
		       (VMLALDAVXQ_S "s")
		       (VMLSLDAVQ_S "s")
		       (VMLSLDAVXQ_S "s")
		       (VMLALDAVXQ_P_S "s")
		       (VMLSLDAVQ_P_S "s")
		       (VMLSLDAVXQ_P_S "s")
		       (VRMLALDAVHXQ_P_S "s")
		       (VRMLALDAVHXQ_S "s")
		       (VRMLSLDAVHQ_P_S "s")
		       (VRMLSLDAVHQ_S "s")
		       (VRMLSLDAVHXQ_P_S "s")
		       (VRMLSLDAVHXQ_S "s")
		       (VMLALDAVAXQ_P_S "s")
		       (VMLALDAVAXQ_S "s")
		       (VMLSLDAVAQ_P_S "s")
		       (VMLSLDAVAQ_S "s")
		       (VMLSLDAVAXQ_P_S "s")
		       (VMLSLDAVAXQ_S "s")
		       (VQDMLADHQ_S "s")
		       (VQDMLADHXQ_S "s")
		       (VQDMLSDHQ_S "s")
		       (VQDMLSDHXQ_S "s")
		       (VQRDMLADHQ_S "s")
		       (VQRDMLADHXQ_S "s")
		       (VQRDMLSDHQ_S "s")
		       (VQRDMLSDHXQ_S "s")
		       (VQDMLAHQ_N_S "s")
		       (VQDMLASHQ_N_S "s")
		       (VQRDMLAHQ_N_S "s")
		       (VQRDMLASHQ_N_S "s")
		       (VQDMULLBQ_S "s")
		       (VQDMULLBQ_M_S "s")
		       (VQDMULLBQ_M_N_S "s")
		       (VQDMULLBQ_N_S "s")
		       (VQDMULLTQ_S "s")
		       (VQDMULLTQ_M_S "s")
		       (VQDMULLTQ_M_N_S "s")
		       (VQDMULLTQ_N_S "s")
		       (VRMLALDAVHAXQ_P_S "s")
		       (VRMLALDAVHAXQ_S "s")
		       (VRMLSLDAVHAQ_P_S "s")
		       (VRMLSLDAVHAQ_S "s")
		       (VRMLSLDAVHAXQ_P_S "s")
		       (VRMLSLDAVHAXQ_S "s")
		       (VRMLALDAVHAQ_P_S "s") (VRMLALDAVHAQ_P_U "u")
		       (VQSHLUQ_M_N_S "s")
		       (VQSHLUQ_N_S "s")
		       (VHCADDQ_ROT90_M_S "s") (VHCADDQ_ROT270_M_S "s")
		       (VHCADDQ_ROT90_S "s") (VHCADDQ_ROT270_S "s")
		       (UNSPEC_VCADD90 "") (UNSPEC_VCADD270 "")
		       ])

;; Both kinds of return insn.
(define_code_iterator RETURNS [return simple_return])
(define_code_attr return_str [(return "") (simple_return "simple_")])
(define_code_attr return_simple_p [(return "false") (simple_return "true")])
(define_code_attr return_cond_false [(return " && USE_RETURN_INSN (FALSE)")
                               (simple_return " && use_simple_return_p ()")])
(define_code_attr return_cond_true [(return " && USE_RETURN_INSN (TRUE)")
                               (simple_return " && use_simple_return_p ()")])

;; Attributes for VQRDMLAH/VQRDMLSH
(define_int_attr neon_rdma_as [(UNSPEC_VQRDMLAH "a") (UNSPEC_VQRDMLSH "s")])

;; Attributes for VFMA_LANE/ VFMS_LANE
(define_int_attr neon_vfm_lane_as
 [(UNSPEC_VFMA_LANE "a") (UNSPEC_VFMS_LANE "s")])

;; An iterator for the CDP coprocessor instructions
(define_int_iterator CDPI [VUNSPEC_CDP VUNSPEC_CDP2])
(define_int_attr cdp [(VUNSPEC_CDP "cdp") (VUNSPEC_CDP2 "cdp2")])
(define_int_attr CDP [(VUNSPEC_CDP "CDP") (VUNSPEC_CDP2 "CDP2")])

;; An iterator for the LDC coprocessor instruction
(define_int_iterator LDCI [VUNSPEC_LDC VUNSPEC_LDC2
			   VUNSPEC_LDCL VUNSPEC_LDC2L])
(define_int_attr ldc [(VUNSPEC_LDC "ldc") (VUNSPEC_LDC2 "ldc2")
		      (VUNSPEC_LDCL "ldcl") (VUNSPEC_LDC2L "ldc2l")])
(define_int_attr LDC [(VUNSPEC_LDC "LDC") (VUNSPEC_LDC2 "LDC2")
		      (VUNSPEC_LDCL "LDCL") (VUNSPEC_LDC2L "LDC2L")])

;; An iterator for the STC coprocessor instructions
(define_int_iterator STCI [VUNSPEC_STC VUNSPEC_STC2
			   VUNSPEC_STCL VUNSPEC_STC2L])
(define_int_attr stc [(VUNSPEC_STC "stc") (VUNSPEC_STC2 "stc2")
		      (VUNSPEC_STCL "stcl") (VUNSPEC_STC2L "stc2l")])
(define_int_attr STC [(VUNSPEC_STC "STC") (VUNSPEC_STC2 "STC2")
		      (VUNSPEC_STCL "STCL") (VUNSPEC_STC2L "STC2L")])

;; An iterator for the MCR coprocessor instructions
(define_int_iterator MCRI [VUNSPEC_MCR VUNSPEC_MCR2])

(define_int_attr mcr [(VUNSPEC_MCR "mcr") (VUNSPEC_MCR2 "mcr2")])
(define_int_attr MCR [(VUNSPEC_MCR "MCR") (VUNSPEC_MCR2 "MCR2")])

;; An iterator for the MRC coprocessor instructions
(define_int_iterator MRCI [VUNSPEC_MRC VUNSPEC_MRC2])

(define_int_attr mrc [(VUNSPEC_MRC "mrc") (VUNSPEC_MRC2 "mrc2")])
(define_int_attr MRC [(VUNSPEC_MRC "MRC") (VUNSPEC_MRC2 "MRC2")])

;; An iterator for the MCRR coprocessor instructions
(define_int_iterator MCRRI [VUNSPEC_MCRR VUNSPEC_MCRR2])

(define_int_attr mcrr [(VUNSPEC_MCRR "mcrr") (VUNSPEC_MCRR2 "mcrr2")])
(define_int_attr MCRR [(VUNSPEC_MCRR "MCRR") (VUNSPEC_MCRR2 "MCRR2")])

;; An iterator for the MRRC coprocessor instructions
(define_int_iterator MRRCI [VUNSPEC_MRRC VUNSPEC_MRRC2])

(define_int_attr mrrc [(VUNSPEC_MRRC "mrrc") (VUNSPEC_MRRC2 "mrrc2")])
(define_int_attr MRRC [(VUNSPEC_MRRC "MRRC") (VUNSPEC_MRRC2 "MRRC2")])

(define_int_attr opsuffix [(UNSPEC_DOT_S "s8")
			   (UNSPEC_DOT_U "u8")
			   (UNSPEC_DOT_US "s8")
			   (UNSPEC_DOT_SU "u8")
			   ])

(define_int_attr smlaw_op [(UNSPEC_SMLAWB "smlawb") (UNSPEC_SMLAWT "smlawt")])

;; An iterator for VFMA<bt>
(define_int_attr bt [(UNSPEC_BFMAB "b") (UNSPEC_BFMAT "t")])

;; An iterator for CDE MVE accumulator/non-accumulator versions.
(define_int_attr a [(UNSPEC_VCDE "") (UNSPEC_VCDEA "a")])

;; MVE int iterator.
(define_int_iterator VCVTQ_TO_F [VCVTQ_TO_F_S VCVTQ_TO_F_U])
(define_int_iterator VMVNQ_N [VMVNQ_N_U VMVNQ_N_S])
(define_int_iterator VREV64Q [VREV64Q_S VREV64Q_U])
(define_int_iterator VCVTQ_FROM_F [VCVTQ_FROM_F_S VCVTQ_FROM_F_U])
(define_int_iterator VREV16Q [VREV16Q_U VREV16Q_S])
(define_int_iterator VCVTAQ [VCVTAQ_U VCVTAQ_S])
(define_int_iterator VDUPQ_N [VDUPQ_N_U VDUPQ_N_S])
(define_int_iterator VADDVQ [VADDVQ_U VADDVQ_S])
(define_int_iterator VREV32Q [VREV32Q_U VREV32Q_S])
(define_int_iterator VMOVLxQ [VMOVLBQ_S VMOVLBQ_U VMOVLTQ_U VMOVLTQ_S])
(define_int_iterator VCVTPQ [VCVTPQ_S VCVTPQ_U])
(define_int_iterator VCVTNQ [VCVTNQ_S VCVTNQ_U])
(define_int_iterator VCVTMQ [VCVTMQ_S VCVTMQ_U])
(define_int_iterator VADDLVQ [VADDLVQ_U VADDLVQ_S])
(define_int_iterator VCVTQ_N_TO_F [VCVTQ_N_TO_F_S VCVTQ_N_TO_F_U])
(define_int_iterator VCREATEQ [VCREATEQ_U VCREATEQ_S])
(define_int_iterator VSHRQ_N [VSHRQ_N_S VSHRQ_N_U])
(define_int_iterator VCVTQ_N_FROM_F [VCVTQ_N_FROM_F_S VCVTQ_N_FROM_F_U])
(define_int_iterator VADDLVQ_P [VADDLVQ_P_S VADDLVQ_P_U])
(define_int_iterator VSHLQ [VSHLQ_S VSHLQ_U])
(define_int_iterator VABDQ [VABDQ_S VABDQ_U])
(define_int_iterator VADDQ_N [VADDQ_N_S VADDQ_N_U])
(define_int_iterator VADDVAQ [VADDVAQ_S VADDVAQ_U])
(define_int_iterator VADDVQ_P [VADDVQ_P_U VADDVQ_P_S])
(define_int_iterator VBRSRQ_N [VBRSRQ_N_U VBRSRQ_N_S])
(define_int_iterator VHADDQ [VHADDQ_S VHADDQ_U])
(define_int_iterator VHADDQ_N [VHADDQ_N_U VHADDQ_N_S])
(define_int_iterator VHSUBQ [VHSUBQ_S VHSUBQ_U])
(define_int_iterator VHSUBQ_N [VHSUBQ_N_U VHSUBQ_N_S])
(define_int_iterator VMAXQ [VMAXQ_U VMAXQ_S])
(define_int_iterator VMAXVQ [VMAXVQ_U VMAXVQ_S])
(define_int_iterator VMINQ [VMINQ_S VMINQ_U])
(define_int_iterator VMINVQ [VMINVQ_U VMINVQ_S])
(define_int_iterator VMLADAVQ [VMLADAVQ_U VMLADAVQ_S])
(define_int_iterator VMULHQ [VMULHQ_S VMULHQ_U])
(define_int_iterator VMULLxQ_INT [VMULLBQ_INT_U VMULLBQ_INT_S VMULLTQ_INT_U VMULLTQ_INT_S])
(define_int_iterator VMULLxQ_POLY [VMULLBQ_POLY_P VMULLTQ_POLY_P])
(define_int_iterator VMULQ [VMULQ_U VMULQ_S])
(define_int_iterator VMULQ_N [VMULQ_N_U VMULQ_N_S])
(define_int_iterator VQADDQ [VQADDQ_U VQADDQ_S])
(define_int_iterator VQADDQ_N [VQADDQ_N_S VQADDQ_N_U])
(define_int_iterator VQRSHLQ [VQRSHLQ_S VQRSHLQ_U])
(define_int_iterator VQRSHLQ_N [VQRSHLQ_N_S VQRSHLQ_N_U])
(define_int_iterator VQSHLQ [VQSHLQ_S VQSHLQ_U])
(define_int_iterator VQSHLQ_N [VQSHLQ_N_S VQSHLQ_N_U])
(define_int_iterator VQSHLQ_R [VQSHLQ_R_U VQSHLQ_R_S])
(define_int_iterator VQSUBQ [VQSUBQ_U VQSUBQ_S])
(define_int_iterator VQSUBQ_N [VQSUBQ_N_S VQSUBQ_N_U])
(define_int_iterator VRHADDQ [VRHADDQ_S VRHADDQ_U])
(define_int_iterator VRMULHQ [VRMULHQ_S VRMULHQ_U])
(define_int_iterator VRSHLQ [VRSHLQ_S VRSHLQ_U])
(define_int_iterator VRSHLQ_N [VRSHLQ_N_U VRSHLQ_N_S])
(define_int_iterator VRSHRQ_N [VRSHRQ_N_S VRSHRQ_N_U])
(define_int_iterator VSHLQ_N [VSHLQ_N_U VSHLQ_N_S])
(define_int_iterator VSHLQ_R [VSHLQ_R_S VSHLQ_R_U])
(define_int_iterator VSUBQ [VSUBQ_S VSUBQ_U])
(define_int_iterator VSUBQ_N [VSUBQ_N_S VSUBQ_N_U])
(define_int_iterator VADDLVAQ [VADDLVAQ_S VADDLVAQ_U])
(define_int_iterator VBICQ_N [VBICQ_N_S VBICQ_N_U])
(define_int_iterator VMLALDAVQ [VMLALDAVQ_U VMLALDAVQ_S])
(define_int_iterator VMOVNBQ [VMOVNBQ_U VMOVNBQ_S])
(define_int_iterator VMOVNTQ [VMOVNTQ_S VMOVNTQ_U])
(define_int_iterator VORRQ_N [VORRQ_N_U VORRQ_N_S])
(define_int_iterator VQMOVNBQ [VQMOVNBQ_U VQMOVNBQ_S])
(define_int_iterator VQMOVNTQ [VQMOVNTQ_U VQMOVNTQ_S])
(define_int_iterator VSHLLxQ_N [VSHLLBQ_N_S VSHLLBQ_N_U VSHLLTQ_N_S VSHLLTQ_N_U])
(define_int_iterator VRMLALDAVHQ [VRMLALDAVHQ_U VRMLALDAVHQ_S])
(define_int_iterator VBICQ_M_N [VBICQ_M_N_S VBICQ_M_N_U])
(define_int_iterator VCVTAQ_M [VCVTAQ_M_S VCVTAQ_M_U])
(define_int_iterator VCVTQ_M_TO_F [VCVTQ_M_TO_F_S VCVTQ_M_TO_F_U])
(define_int_iterator VQRSHRNBQ_N [VQRSHRNBQ_N_U VQRSHRNBQ_N_S])
(define_int_iterator VABAVQ [VABAVQ_S VABAVQ_U])
(define_int_iterator VSHLCQ [VSHLCQ_S VSHLCQ_U])
(define_int_iterator VRMLALDAVHAQ [VRMLALDAVHAQ_S VRMLALDAVHAQ_U])
(define_int_iterator VADDVAQ_P [VADDVAQ_P_S VADDVAQ_P_U])
(define_int_iterator VCLZQ_M [VCLZQ_M_S VCLZQ_M_U])
(define_int_iterator VCMPEQQ_M_N [VCMPEQQ_M_N_S VCMPEQQ_M_N_U])
(define_int_iterator VCMPEQQ_M [VCMPEQQ_M_S VCMPEQQ_M_U])
(define_int_iterator VCMPNEQ_M_N [VCMPNEQ_M_N_S VCMPNEQ_M_N_U])
(define_int_iterator VCMPNEQ_M [VCMPNEQ_M_S VCMPNEQ_M_U])
(define_int_iterator VDUPQ_M_N [VDUPQ_M_N_S VDUPQ_M_N_U])
(define_int_iterator VMAXVQ_P [VMAXVQ_P_S VMAXVQ_P_U])
(define_int_iterator VMINVQ_P [VMINVQ_P_S VMINVQ_P_U])
(define_int_iterator VMLADAVAQ [VMLADAVAQ_S VMLADAVAQ_U])
(define_int_iterator VMLADAVQ_P [VMLADAVQ_P_S VMLADAVQ_P_U])
(define_int_iterator VMLAQ_N [VMLAQ_N_S VMLAQ_N_U])
(define_int_iterator VMLASQ_N [VMLASQ_N_S VMLASQ_N_U])
(define_int_iterator VMVNQ_M [VMVNQ_M_S VMVNQ_M_U])
(define_int_iterator VPSELQ [VPSELQ_S VPSELQ_U])
(define_int_iterator VQDMLAHQ_N [VQDMLAHQ_N_S])
(define_int_iterator VQDMLASHQ_N [VQDMLASHQ_N_S])
(define_int_iterator VQRDMLAHQ_N [VQRDMLAHQ_N_S])
(define_int_iterator VQRDMLASHQ_N [VQRDMLASHQ_N_S])
(define_int_iterator VQRSHLQ_M_N [VQRSHLQ_M_N_S VQRSHLQ_M_N_U])
(define_int_iterator VQSHLQ_M_R [VQSHLQ_M_R_S VQSHLQ_M_R_U])
(define_int_iterator VREV64Q_M [VREV64Q_M_S VREV64Q_M_U])
(define_int_iterator VRSHLQ_M_N [VRSHLQ_M_N_S VRSHLQ_M_N_U])
(define_int_iterator VSHLQ_M_R [VSHLQ_M_R_S VSHLQ_M_R_U])
(define_int_iterator VSLIQ_N [VSLIQ_N_S VSLIQ_N_U])
(define_int_iterator VSRIQ_N [VSRIQ_N_S VSRIQ_N_U])
(define_int_iterator VMLALDAVQ_P [VMLALDAVQ_P_U VMLALDAVQ_P_S])
(define_int_iterator VQMOVNBQ_M [VQMOVNBQ_M_S VQMOVNBQ_M_U])
(define_int_iterator VMOVLxQ_M [VMOVLBQ_M_U VMOVLBQ_M_S VMOVLTQ_M_U VMOVLTQ_M_S])
(define_int_iterator VMOVNBQ_M [VMOVNBQ_M_U VMOVNBQ_M_S])
(define_int_iterator VRSHRNTQ_N [VRSHRNTQ_N_U VRSHRNTQ_N_S])
(define_int_iterator VORRQ_M_N [VORRQ_M_N_S VORRQ_M_N_U])
(define_int_iterator VREV32Q_M [VREV32Q_M_S VREV32Q_M_U])
(define_int_iterator VREV16Q_M [VREV16Q_M_S VREV16Q_M_U])
(define_int_iterator VQRSHRNTQ_N [VQRSHRNTQ_N_U VQRSHRNTQ_N_S])
(define_int_iterator VMOVNTQ_M [VMOVNTQ_M_U VMOVNTQ_M_S])
(define_int_iterator VMLALDAVAQ [VMLALDAVAQ_S VMLALDAVAQ_U])
(define_int_iterator VQSHRNBQ_N [VQSHRNBQ_N_U VQSHRNBQ_N_S])
(define_int_iterator VSHRNBQ_N [VSHRNBQ_N_U VSHRNBQ_N_S])
(define_int_iterator VRSHRNBQ_N [VRSHRNBQ_N_S VRSHRNBQ_N_U])
(define_int_iterator VQMOVNTQ_M [VQMOVNTQ_M_U VQMOVNTQ_M_S])
(define_int_iterator VMVNQ_M_N [VMVNQ_M_N_U VMVNQ_M_N_S])
(define_int_iterator VQSHRNTQ_N [VQSHRNTQ_N_U VQSHRNTQ_N_S])
(define_int_iterator VSHRNTQ_N [VSHRNTQ_N_S VSHRNTQ_N_U])
(define_int_iterator VCVTMQ_M [VCVTMQ_M_S VCVTMQ_M_U])
(define_int_iterator VCVTNQ_M [VCVTNQ_M_S VCVTNQ_M_U])
(define_int_iterator VCVTPQ_M [VCVTPQ_M_S VCVTPQ_M_U])
(define_int_iterator VCVTQ_M_N_FROM_F [VCVTQ_M_N_FROM_F_S VCVTQ_M_N_FROM_F_U])
(define_int_iterator VCVTQ_M_FROM_F [VCVTQ_M_FROM_F_U VCVTQ_M_FROM_F_S])
(define_int_iterator VRMLALDAVHQ_P [VRMLALDAVHQ_P_S VRMLALDAVHQ_P_U])
(define_int_iterator VADDLVAQ_P [VADDLVAQ_P_U VADDLVAQ_P_S])
(define_int_iterator VABAVQ_P [VABAVQ_P_S VABAVQ_P_U])
(define_int_iterator VSHLQ_M [VSHLQ_M_S VSHLQ_M_U])
(define_int_iterator VSRIQ_M_N [VSRIQ_M_N_S VSRIQ_M_N_U])
(define_int_iterator VSUBQ_M [VSUBQ_M_U VSUBQ_M_S])
(define_int_iterator VCVTQ_M_N_TO_F [VCVTQ_M_N_TO_F_U VCVTQ_M_N_TO_F_S])
(define_int_iterator VHSUBQ_M [VHSUBQ_M_S VHSUBQ_M_U])
(define_int_iterator VSLIQ_M_N [VSLIQ_M_N_U VSLIQ_M_N_S])
(define_int_iterator VRSHLQ_M [VRSHLQ_M_S VRSHLQ_M_U])
(define_int_iterator VMINQ_M [VMINQ_M_S VMINQ_M_U])
(define_int_iterator VMULLxQ_INT_M [VMULLBQ_INT_M_U VMULLBQ_INT_M_S VMULLTQ_INT_M_U VMULLTQ_INT_M_S])
(define_int_iterator VMULLxQ_POLY_M [VMULLBQ_POLY_M_P VMULLTQ_POLY_M_P])
(define_int_iterator VMULHQ_M [VMULHQ_M_S VMULHQ_M_U])
(define_int_iterator VMULQ_M [VMULQ_M_S VMULQ_M_U])
(define_int_iterator VHSUBQ_M_N [VHSUBQ_M_N_S VHSUBQ_M_N_U])
(define_int_iterator VHADDQ_M_N [VHADDQ_M_N_S VHADDQ_M_N_U])
(define_int_iterator VORRQ_M [VORRQ_M_S VORRQ_M_U])
(define_int_iterator VRMULHQ_M [VRMULHQ_M_U VRMULHQ_M_S])
(define_int_iterator VQADDQ_M [VQADDQ_M_U VQADDQ_M_S])
(define_int_iterator VRSHRQ_M_N [VRSHRQ_M_N_S VRSHRQ_M_N_U])
(define_int_iterator VQSUBQ_M_N [VQSUBQ_M_N_U VQSUBQ_M_N_S])
(define_int_iterator VADDQ_M [VADDQ_M_U VADDQ_M_S])
(define_int_iterator VORNQ_M [VORNQ_M_U VORNQ_M_S])
(define_int_iterator VRHADDQ_M [VRHADDQ_M_U VRHADDQ_M_S])
(define_int_iterator VQSHLQ_M [VQSHLQ_M_U VQSHLQ_M_S])
(define_int_iterator VANDQ_M [VANDQ_M_U VANDQ_M_S])
(define_int_iterator VBICQ_M [VBICQ_M_U VBICQ_M_S])
(define_int_iterator VSHLQ_M_N [VSHLQ_M_N_S VSHLQ_M_N_U])
(define_int_iterator VCADDQ_M_F [VCADDQ_ROT90_M_F VCADDQ_ROT270_M_F])
(define_int_iterator VxCADDQ [UNSPEC_VCADD90 UNSPEC_VCADD270 VHCADDQ_ROT90_S VHCADDQ_ROT270_S])
(define_int_iterator VxCADDQ_M [VHCADDQ_ROT90_M_S VHCADDQ_ROT270_M_S VCADDQ_ROT90_M VCADDQ_ROT270_M])
(define_int_iterator VQRSHLQ_M [VQRSHLQ_M_U VQRSHLQ_M_S])
(define_int_iterator VQADDQ_M_N [VQADDQ_M_N_U VQADDQ_M_N_S])
(define_int_iterator VADDQ_M_N [VADDQ_M_N_S VADDQ_M_N_U])
(define_int_iterator VMAXQ_M [VMAXQ_M_S VMAXQ_M_U])
(define_int_iterator VQSUBQ_M [VQSUBQ_M_U VQSUBQ_M_S])
(define_int_iterator VMLASQ_M_N [VMLASQ_M_N_U VMLASQ_M_N_S])
(define_int_iterator VMLADAVAQ_P [VMLADAVAQ_P_U VMLADAVAQ_P_S])
(define_int_iterator VBRSRQ_M_N [VBRSRQ_M_N_U VBRSRQ_M_N_S])
(define_int_iterator VMULQ_M_N [VMULQ_M_N_U VMULQ_M_N_S])
(define_int_iterator VEORQ_M [VEORQ_M_S VEORQ_M_U])
(define_int_iterator VSHRQ_M_N [VSHRQ_M_N_S VSHRQ_M_N_U])
(define_int_iterator VSUBQ_M_N [VSUBQ_M_N_S VSUBQ_M_N_U])
(define_int_iterator VHADDQ_M [VHADDQ_M_S VHADDQ_M_U])
(define_int_iterator VABDQ_M [VABDQ_M_S VABDQ_M_U])
(define_int_iterator VMLAQ_M_N [VMLAQ_M_N_S VMLAQ_M_N_U])
(define_int_iterator VQSHLQ_M_N [VQSHLQ_M_N_S VQSHLQ_M_N_U])
(define_int_iterator VMLALDAVAQ_P [VMLALDAVAQ_P_U VMLALDAVAQ_P_S])
(define_int_iterator VMLALDAVAXQ_P [VMLALDAVAXQ_P_S])
(define_int_iterator VQRSHRNBQ_M_N [VQRSHRNBQ_M_N_U VQRSHRNBQ_M_N_S])
(define_int_iterator VQRSHRNTQ_M_N [VQRSHRNTQ_M_N_S VQRSHRNTQ_M_N_U])
(define_int_iterator VQSHRNBQ_M_N [VQSHRNBQ_M_N_U VQSHRNBQ_M_N_S])
(define_int_iterator VQSHRNTQ_M_N [VQSHRNTQ_M_N_S VQSHRNTQ_M_N_U])
(define_int_iterator VRSHRNBQ_M_N [VRSHRNBQ_M_N_U VRSHRNBQ_M_N_S])
(define_int_iterator VRSHRNTQ_M_N [VRSHRNTQ_M_N_U VRSHRNTQ_M_N_S])
(define_int_iterator VSHLLxQ_M_N [VSHLLBQ_M_N_U VSHLLBQ_M_N_S VSHLLTQ_M_N_U VSHLLTQ_M_N_S])
(define_int_iterator VSHRNBQ_M_N [VSHRNBQ_M_N_S VSHRNBQ_M_N_U])
(define_int_iterator VSHRNTQ_M_N [VSHRNTQ_M_N_S VSHRNTQ_M_N_U])
(define_int_iterator VSTRWSBQ [VSTRWQSB_S VSTRWQSB_U])
(define_int_iterator VSTRBSOQ [VSTRBQSO_S VSTRBQSO_U])
(define_int_iterator VSTRBQ [VSTRBQ_S VSTRBQ_U])
(define_int_iterator VLDRBGOQ [VLDRBQGO_S VLDRBQGO_U])
(define_int_iterator VLDRBQ [VLDRBQ_S VLDRBQ_U])
(define_int_iterator VLDRWGBQ [VLDRWQGB_S VLDRWQGB_U])
(define_int_iterator VLD1Q [VLD1Q_S VLD1Q_U])
(define_int_iterator VLDRHGOQ [VLDRHQGO_S VLDRHQGO_U])
(define_int_iterator VLDRHGSOQ [VLDRHQGSO_S VLDRHQGSO_U])
(define_int_iterator VLDRHQ [VLDRHQ_S VLDRHQ_U])
(define_int_iterator VLDRWQ [VLDRWQ_S VLDRWQ_U])
(define_int_iterator VLDRDGBQ [VLDRDQGB_S VLDRDQGB_U])
(define_int_iterator VLDRDGOQ [VLDRDQGO_S VLDRDQGO_U])
(define_int_iterator VLDRDGSOQ [VLDRDQGSO_S VLDRDQGSO_U])
(define_int_iterator VLDRWGOQ [VLDRWQGO_S VLDRWQGO_U])
(define_int_iterator VLDRWGSOQ [VLDRWQGSO_S VLDRWQGSO_U])
(define_int_iterator VST1Q [VST1Q_S VST1Q_U])
(define_int_iterator VSTRHSOQ [VSTRHQSO_S VSTRHQSO_U])
(define_int_iterator VSTRHSSOQ [VSTRHQSSO_S VSTRHQSSO_U])
(define_int_iterator VSTRHQ [VSTRHQ_S VSTRHQ_U])
(define_int_iterator VSTRWQ [VSTRWQ_S VSTRWQ_U])
(define_int_iterator VSTRDSBQ [VSTRDQSB_S VSTRDQSB_U])
(define_int_iterator VSTRDSOQ [VSTRDQSO_S VSTRDQSO_U])
(define_int_iterator VSTRDSSOQ [VSTRDQSSO_S VSTRDQSSO_U])
(define_int_iterator VSTRWSOQ [VSTRWQSO_S VSTRWQSO_U])
(define_int_iterator VSTRWSSOQ [VSTRWQSSO_S VSTRWQSSO_U])
(define_int_iterator VSTRWSBWBQ [VSTRWQSBWB_S VSTRWQSBWB_U])
(define_int_iterator VLDRWGBWBQ [VLDRWQGBWB_S VLDRWQGBWB_U])
(define_int_iterator VSTRDSBWBQ [VSTRDQSBWB_S VSTRDQSBWB_U])
(define_int_iterator VLDRDGBWBQ [VLDRDQGBWB_S VLDRDQGBWB_U])
(define_int_iterator VADCIQ [VADCIQ_U VADCIQ_S])
(define_int_iterator VADCIQ_M [VADCIQ_M_U VADCIQ_M_S])
(define_int_iterator VSBCQ [VSBCQ_U VSBCQ_S])
(define_int_iterator VSBCQ_M [VSBCQ_M_U VSBCQ_M_S])
(define_int_iterator VSBCIQ [VSBCIQ_U VSBCIQ_S])
(define_int_iterator VSBCIQ_M [VSBCIQ_M_U VSBCIQ_M_S])
(define_int_iterator VADCQ [VADCQ_U VADCQ_S])
(define_int_iterator VADCQ_M [VADCQ_M_U VADCQ_M_S])
(define_int_iterator UQRSHLLQ [UQRSHLL_64 UQRSHLL_48])
(define_int_iterator SQRSHRLQ [SQRSHRL_64 SQRSHRL_48])
(define_int_iterator VSHLCQ_M [VSHLCQ_M_S VSHLCQ_M_U])
(define_int_iterator VQSHLUQ_M_N [VQSHLUQ_M_N_S])
(define_int_iterator VQSHLUQ_N [VQSHLUQ_N_S])

;; Define iterators for VCMLA operations
(define_int_iterator VCMLA_OP [UNSPEC_VCMLA
			       UNSPEC_VCMLA_CONJ
			       UNSPEC_VCMLA180
			       UNSPEC_VCMLA180_CONJ])

;; Define iterators for VCMLA operations as MUL
(define_int_iterator VCMUL_OP [UNSPEC_VCMUL
			       UNSPEC_VCMUL_CONJ])
