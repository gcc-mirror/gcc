;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2021 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
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

;; -------------------------------------------------------------------
;; Mode Iterators
;; -------------------------------------------------------------------

;; Condition-code iterators.
(define_mode_iterator CC_ONLY [CC])
(define_mode_iterator CCFP_CCFPE [CCFP CCFPE])

;; Iterator for General Purpose Integer registers (32- and 64-bit modes)
(define_mode_iterator GPI [SI DI])

;; Iterator for HI, SI, DI, some instructions can only work on these modes.
(define_mode_iterator GPI_I16 [(HI "AARCH64_ISA_F16") SI DI])

;; "Iterator" for just TI -- features like @pattern only work with iterators.
(define_mode_iterator JUST_TI [TI])

;; Iterator for QI and HI modes
(define_mode_iterator SHORT [QI HI])

;; Iterators for single modes, for "@" patterns.
(define_mode_iterator SI_ONLY [SI])
(define_mode_iterator DI_ONLY [DI])

;; Iterator for all integer modes (up to 64-bit)
(define_mode_iterator ALLI [QI HI SI DI])

;; Iterator for all integer modes (up to 128-bit)
(define_mode_iterator ALLI_TI [QI HI SI DI TI])

;; Iterator for all integer modes that can be extended (up to 64-bit)
(define_mode_iterator ALLX [QI HI SI])

;; Iterator for General Purpose Floating-point registers (32- and 64-bit modes)
(define_mode_iterator GPF [SF DF])

;; Iterator for all scalar floating point modes (HF, SF, DF)
(define_mode_iterator GPF_F16 [(HF "AARCH64_ISA_F16") SF DF])

;; Iterator for all scalar floating point modes (HF, SF, DF)
(define_mode_iterator GPF_HF [HF SF DF])

;; Iterator for all 16-bit scalar floating point modes (HF, BF)
(define_mode_iterator HFBF [HF BF])

;; Iterator for all scalar floating point modes (HF, SF, DF and TF)
(define_mode_iterator GPF_TF_F16 [HF SF DF TF])

;; Iterator for all scalar floating point modes suitable for moving, including
;; special BF type (HF, SF, DF, TF and BF)
(define_mode_iterator GPF_TF_F16_MOV [HF BF SF DF TF])

;; Double vector modes.
(define_mode_iterator VDF [V2SF V4HF])

;; Iterator for all scalar floating point modes (SF, DF and TF)
(define_mode_iterator GPF_TF [SF DF TF])

;; Integer Advanced SIMD modes.
(define_mode_iterator VDQ_I [V8QI V16QI V4HI V8HI V2SI V4SI V2DI])

;; Advanced SIMD and scalar, 64 & 128-bit container, all integer modes.
(define_mode_iterator VSDQ_I [V8QI V16QI V4HI V8HI V2SI V4SI V2DI QI HI SI DI])

;; Advanced SIMD and scalar, 64 & 128-bit container: all Advanced SIMD
;; integer modes; 64-bit scalar integer mode.
(define_mode_iterator VSDQ_I_DI [V8QI V16QI V4HI V8HI V2SI V4SI V2DI DI])

;; Double vector modes.
(define_mode_iterator VD [V8QI V4HI V4HF V2SI V2SF V4BF])

;; Double vector modes suitable for moving.  Includes BFmode.
(define_mode_iterator VDMOV [V8QI V4HI V4HF V4BF V2SI V2SF])

;; All modes stored in registers d0-d31.
(define_mode_iterator DREG [V8QI V4HI V4HF V2SI V2SF DF])

;; Copy of the above.
(define_mode_iterator DREG2 [V8QI V4HI V4HF V2SI V2SF DF])

;; All modes suitable to store/load pair (2 elements) using STP/LDP.
(define_mode_iterator VP_2E [V2SI V2SF V2DI V2DF])

;; Advanced SIMD, 64-bit container, all integer modes.
(define_mode_iterator VD_BHSI [V8QI V4HI V2SI])

;; 128 and 64-bit container; 8, 16, 32-bit vector integer modes
(define_mode_iterator VDQ_BHSI [V8QI V16QI V4HI V8HI V2SI V4SI])

;; Quad vector modes.
(define_mode_iterator VQ [V16QI V8HI V4SI V2DI V8HF V4SF V2DF V8BF])

;; Copy of the above.
(define_mode_iterator VQ2 [V16QI V8HI V4SI V2DI V8HF V8BF V4SF V2DF])

;; Quad vector modes suitable for moving.  Includes BFmode.
(define_mode_iterator VQMOV [V16QI V8HI V4SI V2DI V8HF V8BF V4SF V2DF])

;; VQMOV without 2-element modes.
(define_mode_iterator VQMOV_NO2E [V16QI V8HI V4SI V8HF V8BF V4SF])

;; Quad integer vector modes.
(define_mode_iterator VQ_I [V16QI V8HI V4SI V2DI])

;; VQ without 2 element modes.
(define_mode_iterator VQ_NO2E [V16QI V8HI V4SI V8HF V4SF V8BF])

;; BFmode vector modes.
(define_mode_iterator VBF [V4BF V8BF])

;; This mode iterator allows :P to be used for patterns that operate on
;; addresses in different modes.  In LP64, only DI will match, while in
;; ILP32, either can match.
(define_mode_iterator P [(SI "ptr_mode == SImode || Pmode == SImode")
			 (DI "ptr_mode == DImode || Pmode == DImode")])

;; This mode iterator allows :PTR to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator PTR [(SI "ptr_mode == SImode") (DI "ptr_mode == DImode")])

;; Advanced SIMD Float modes suitable for moving, loading and storing.
(define_mode_iterator VDQF_F16 [V4HF V8HF V2SF V4SF V2DF
				V4BF V8BF])

;; Advanced SIMD Float modes.
(define_mode_iterator VDQF [V2SF V4SF V2DF])
(define_mode_iterator VHSDF [(V4HF "TARGET_SIMD_F16INST")
			     (V8HF "TARGET_SIMD_F16INST")
			     V2SF V4SF V2DF])

;; Advanced SIMD Float modes, and DF.
(define_mode_iterator VDQF_DF [V2SF V4SF V2DF DF])
(define_mode_iterator VHSDF_DF [(V4HF "TARGET_SIMD_F16INST")
				(V8HF "TARGET_SIMD_F16INST")
				V2SF V4SF V2DF DF])
(define_mode_iterator VHSDF_HSDF [(V4HF "TARGET_SIMD_F16INST")
				  (V8HF "TARGET_SIMD_F16INST")
				  V2SF V4SF V2DF
				  (HF "TARGET_SIMD_F16INST")
				  SF DF])

;; Scalar and vetor modes for SF, DF.
(define_mode_iterator VSFDF [V2SF V4SF V2DF DF SF])

;; Advanced SIMD single Float modes.
(define_mode_iterator VDQSF [V2SF V4SF])

;; Quad vector Float modes with half/single elements.
(define_mode_iterator VQ_HSF [V8HF V4SF])

;; Modes suitable to use as the return type of a vcond expression.
(define_mode_iterator VDQF_COND [V2SF V2SI V4SF V4SI V2DF V2DI])

;; All scalar and Advanced SIMD Float modes.
(define_mode_iterator VALLF [V2SF V4SF V2DF SF DF])

;; Advanced SIMD Float modes with 2 elements.
(define_mode_iterator V2F [V2SF V2DF])

;; All Advanced SIMD modes on which we support any arithmetic operations.
(define_mode_iterator VALL [V8QI V16QI V4HI V8HI V2SI V4SI V2DI V2SF V4SF V2DF])

;; All Advanced SIMD modes suitable for moving, loading, and storing.
(define_mode_iterator VALL_F16 [V8QI V16QI V4HI V8HI V2SI V4SI V2DI
				V4HF V8HF V4BF V8BF V2SF V4SF V2DF])

;; All Advanced SIMD modes suitable for moving, loading, and storing,
;; including special Bfloat vector types.
(define_mode_iterator VALL_F16MOV [V8QI V16QI V4HI V8HI V2SI V4SI V2DI
				   V4HF V8HF V4BF V8BF V2SF V4SF V2DF])

;; The VALL_F16 modes except the 128-bit 2-element ones.
(define_mode_iterator VALL_F16_NO_V2Q [V8QI V16QI V4HI V8HI V2SI V4SI
				V4HF V8HF V2SF V4SF])

;; All Advanced SIMD modes barring HF modes, plus DI.
(define_mode_iterator VALLDI [V8QI V16QI V4HI V8HI V2SI V4SI V2DI V2SF V4SF V2DF DI])

;; All Advanced SIMD modes and DI.
(define_mode_iterator VALLDI_F16 [V8QI V16QI V4HI V8HI V2SI V4SI V2DI
				  V4HF V8HF V4BF V8BF V2SF V4SF V2DF DI])

;; All Advanced SIMD modes, plus DI and DF.
(define_mode_iterator VALLDIF [V8QI V16QI V4HI V8HI V2SI V4SI V4BF V8BF
			       V2DI V4HF V8HF V2SF V4SF V2DF DI DF])

;; All Advanced SIMD polynomial modes and DI.
(define_mode_iterator VALLP [V8QI V16QI V4HI V8HI V2DI DI])

;; Advanced SIMD modes for Integer reduction across lanes.
(define_mode_iterator VDQV [V8QI V16QI V4HI V8HI V4SI V2DI])

;; Advanced SIMD modes (except V2DI) for Integer reduction across lanes.
(define_mode_iterator VDQV_S [V8QI V16QI V4HI V8HI V4SI])

;; Advanced SIMD modes for Integer reduction across lanes (zero/sign extended).
(define_mode_iterator VDQV_E [V8QI V16QI V4HI V8HI])

;; Advanced SIMD modes for Integer widening reduction across lanes.
(define_mode_iterator VDQV_L [V8QI V16QI V4HI V8HI V4SI V2SI])

;; All double integer narrow-able modes.
(define_mode_iterator VDN [V4HI V2SI DI])

;; All quad integer narrow-able modes.
(define_mode_iterator VQN [V8HI V4SI V2DI])

;; Advanced SIMD and scalar 128-bit container: narrowable 16, 32, 64-bit
;; integer modes
(define_mode_iterator VSQN_HSDI [V8HI V4SI V2DI HI SI DI])

;; All quad integer widen-able modes.
(define_mode_iterator VQW [V16QI V8HI V4SI])

;; Double vector modes for combines.
(define_mode_iterator VDC [V8QI V4HI V4BF V4HF V2SI V2SF DI DF])

;; Advanced SIMD modes except double int.
(define_mode_iterator VDQIF [V8QI V16QI V4HI V8HI V2SI V4SI V2SF V4SF V2DF])
(define_mode_iterator VDQIF_F16 [V8QI V16QI V4HI V8HI V2SI V4SI
                                 V4HF V8HF V2SF V4SF V2DF])

;; Advanced SIMD modes for S type.
(define_mode_iterator VDQ_SI [V2SI V4SI])

;; Advanced SIMD modes for S and D.
(define_mode_iterator VDQ_SDI [V2SI V4SI V2DI])

;; Advanced SIMD modes for H, S and D.
(define_mode_iterator VDQ_HSDI [(V4HI "TARGET_SIMD_F16INST")
				(V8HI "TARGET_SIMD_F16INST")
				V2SI V4SI V2DI])

;; Scalar and Advanced SIMD modes for S and D.
(define_mode_iterator VSDQ_SDI [V2SI V4SI V2DI SI DI])

;; Scalar and Advanced SIMD modes for S and D, Advanced SIMD modes for H.
(define_mode_iterator VSDQ_HSDI [(V4HI "TARGET_SIMD_F16INST")
				 (V8HI "TARGET_SIMD_F16INST")
				 V2SI V4SI V2DI
				 (HI "TARGET_SIMD_F16INST")
				 SI DI])

;; Advanced SIMD modes for Q and H types.
(define_mode_iterator VDQQH [V8QI V16QI V4HI V8HI])

;; Advanced SIMD modes for H and S types.
(define_mode_iterator VDQHS [V4HI V8HI V2SI V4SI])

;; Advanced SIMD modes for H, S and D types.
(define_mode_iterator VDQHSD [V4HI V8HI V2SI V4SI V2DI])

;; Advanced SIMD and scalar integer modes for H and S.
(define_mode_iterator VSDQ_HSI [V4HI V8HI V2SI V4SI HI SI])

;; Advanced SIMD and scalar 64-bit container: 16, 32-bit integer modes.
(define_mode_iterator VSD_HSI [V4HI V2SI HI SI])

;; Advanced SIMD 64-bit container: 16, 32-bit integer modes.
(define_mode_iterator VD_HSI [V4HI V2SI])

;; Scalar 64-bit container: 16, 32-bit integer modes
(define_mode_iterator SD_HSI [HI SI])

;; Advanced SIMD 64-bit container: 16, 32-bit integer modes.
(define_mode_iterator VQ_HSI [V8HI V4SI])

;; All byte modes.
(define_mode_iterator VB [V8QI V16QI])

;; 2 and 4 lane SI modes.
(define_mode_iterator VS [V2SI V4SI])

(define_mode_iterator TX [TI TF])

;; Advanced SIMD opaque structure modes.
(define_mode_iterator VSTRUCT [OI CI XI])

;; Double scalar modes
(define_mode_iterator DX [DI DF])

;; Duplicate of the above
(define_mode_iterator DX2 [DI DF])

;; Single scalar modes
(define_mode_iterator SX [SI SF])

;; Duplicate of the above
(define_mode_iterator SX2 [SI SF])

;; Single and double integer and float modes
(define_mode_iterator DSX [DF DI SF SI])


;; Modes available for Advanced SIMD <f>mul lane operations.
(define_mode_iterator VMUL [V4HI V8HI V2SI V4SI
			    (V4HF "TARGET_SIMD_F16INST")
			    (V8HF "TARGET_SIMD_F16INST")
			    V2SF V4SF V2DF])

;; Modes available for Advanced SIMD <f>mul lane operations changing lane
;; count.
(define_mode_iterator VMUL_CHANGE_NLANES [V4HI V8HI V2SI V4SI V2SF V4SF])

;; Iterators for single modes, for "@" patterns.
(define_mode_iterator VNx16QI_ONLY [VNx16QI])
(define_mode_iterator VNx8HI_ONLY [VNx8HI])
(define_mode_iterator VNx8BF_ONLY [VNx8BF])
(define_mode_iterator VNx4SI_ONLY [VNx4SI])
(define_mode_iterator VNx4SF_ONLY [VNx4SF])
(define_mode_iterator VNx2DI_ONLY [VNx2DI])
(define_mode_iterator VNx2DF_ONLY [VNx2DF])

;; All SVE vector structure modes.
(define_mode_iterator SVE_STRUCT [VNx32QI VNx16HI VNx8SI VNx4DI
				  VNx16BF VNx16HF VNx8SF VNx4DF
				  VNx48QI VNx24HI VNx12SI VNx6DI
				  VNx24BF VNx24HF VNx12SF VNx6DF
				  VNx64QI VNx32HI VNx16SI VNx8DI
				  VNx32BF VNx32HF VNx16SF VNx8DF])

;; All fully-packed SVE vector modes.
(define_mode_iterator SVE_FULL [VNx16QI VNx8HI VNx4SI VNx2DI
			        VNx8BF VNx8HF VNx4SF VNx2DF])

;; All fully-packed SVE integer vector modes.
(define_mode_iterator SVE_FULL_I [VNx16QI VNx8HI VNx4SI VNx2DI])

;; All fully-packed SVE floating-point vector modes.
(define_mode_iterator SVE_FULL_F [VNx8HF VNx4SF VNx2DF])

;; Fully-packed SVE integer vector modes that have 8-bit or 16-bit elements.
(define_mode_iterator SVE_FULL_BHI [VNx16QI VNx8HI])

;; Fully-packed SVE integer vector modes that have 8-bit, 16-bit or 32-bit
;; elements.
(define_mode_iterator SVE_FULL_BHSI [VNx16QI VNx8HI VNx4SI])

;; Fully-packed SVE vector modes that have 16-bit, 32-bit or 64-bit elements.
(define_mode_iterator SVE_FULL_HSD [VNx8HI VNx4SI VNx2DI
				    VNx8BF VNx8HF VNx4SF VNx2DF])

;; Fully-packed SVE integer vector modes that have 16-bit, 32-bit or 64-bit
;; elements.
(define_mode_iterator SVE_FULL_HSDI [VNx8HI VNx4SI VNx2DI])

;; Fully-packed SVE integer vector modes that have 16-bit or 32-bit
;; elements.
(define_mode_iterator SVE_FULL_HSI [VNx8HI VNx4SI])

;; Fully-packed SVE floating-point vector modes that have 16-bit or 32-bit
;; elements.
(define_mode_iterator SVE_FULL_HSF [VNx8HF VNx4SF])

;; Fully-packed SVE integer vector modes that have 16-bit or 64-bit elements.
(define_mode_iterator SVE_FULL_HDI [VNx8HI VNx2DI])

;; Fully-packed SVE vector modes that have 32-bit or 64-bit elements.
(define_mode_iterator SVE_FULL_SD [VNx4SI VNx2DI VNx4SF VNx2DF])

;; Fully-packed SVE integer vector modes that have 32-bit or 64-bit elements.
(define_mode_iterator SVE_FULL_SDI [VNx4SI VNx2DI])

;; Fully-packed SVE floating-point vector modes that have 32-bit or 64-bit
;; elements.
(define_mode_iterator SVE_FULL_SDF [VNx4SF VNx2DF])

;; Same, but with the appropriate conditions for FMMLA support.
(define_mode_iterator SVE_MATMULF [(VNx4SF "TARGET_SVE_F32MM")
				   (VNx2DF "TARGET_SVE_F64MM")])

;; Fully-packed SVE vector modes that have 32-bit elements.
(define_mode_iterator SVE_FULL_S [VNx4SI VNx4SF])

;; Fully-packed SVE vector modes that have 64-bit elements.
(define_mode_iterator SVE_FULL_D [VNx2DI VNx2DF])

;; All partial SVE integer modes.
(define_mode_iterator SVE_PARTIAL_I [VNx8QI VNx4QI VNx2QI
				     VNx4HI VNx2HI
				     VNx2SI])

;; All SVE vector modes.
(define_mode_iterator SVE_ALL [VNx16QI VNx8QI VNx4QI VNx2QI
			       VNx8HI VNx4HI VNx2HI
			       VNx8HF VNx4HF VNx2HF
			       VNx8BF VNx4BF VNx2BF
			       VNx4SI VNx2SI
			       VNx4SF VNx2SF
			       VNx2DI
			       VNx2DF])

;; All SVE integer vector modes.
(define_mode_iterator SVE_I [VNx16QI VNx8QI VNx4QI VNx2QI
			     VNx8HI VNx4HI VNx2HI
			     VNx4SI VNx2SI
			     VNx2DI])

;; SVE integer vector modes whose elements are 16 bits or wider.
(define_mode_iterator SVE_HSDI [VNx8HI VNx4HI VNx2HI
				VNx4SI VNx2SI
				VNx2DI])

;; SVE modes with 2 or 4 elements.
(define_mode_iterator SVE_24 [VNx2QI VNx2HI VNx2HF VNx2BF VNx2SI VNx2SF
			      VNx2DI VNx2DF
			      VNx4QI VNx4HI VNx4HF VNx4BF VNx4SI VNx4SF])

;; SVE integer modes with 2 or 4 elements.
(define_mode_iterator SVE_24I [VNx2QI VNx2HI VNx2SI VNx2DI
			       VNx4QI VNx4HI VNx4SI])

;; SVE modes with 2 elements.
(define_mode_iterator SVE_2 [VNx2QI VNx2HI VNx2HF VNx2BF
			     VNx2SI VNx2SF VNx2DI VNx2DF])

;; SVE integer modes with 2 elements, excluding the widest element.
(define_mode_iterator SVE_2BHSI [VNx2QI VNx2HI VNx2SI])

;; SVE integer modes with 2 elements, excluding the narrowest element.
(define_mode_iterator SVE_2HSDI [VNx2HI VNx2SI VNx2DI])

;; SVE modes with 4 elements.
(define_mode_iterator SVE_4 [VNx4QI VNx4HI VNx4HF VNx4BF VNx4SI VNx4SF])

;; SVE integer modes with 4 elements, excluding the widest element.
(define_mode_iterator SVE_4BHI [VNx4QI VNx4HI])

;; SVE integer modes with 4 elements, excluding the narrowest element.
(define_mode_iterator SVE_4HSI [VNx4HI VNx4SI])

;; SVE integer modes that can form the input to an SVE2 PMULL[BT] instruction.
(define_mode_iterator SVE2_PMULL_PAIR_I [VNx16QI VNx4SI
					 (VNx2DI "TARGET_SVE2_AES")])

;; Modes involved in extending or truncating SVE data, for 8 elements per
;; 128-bit block.
(define_mode_iterator VNx8_NARROW [VNx8QI])
(define_mode_iterator VNx8_WIDE [VNx8HI])

;; ...same for 4 elements per 128-bit block.
(define_mode_iterator VNx4_NARROW [VNx4QI VNx4HI])
(define_mode_iterator VNx4_WIDE [VNx4SI])

;; ...same for 2 elements per 128-bit block.
(define_mode_iterator VNx2_NARROW [VNx2QI VNx2HI VNx2SI])
(define_mode_iterator VNx2_WIDE [VNx2DI])

;; All SVE predicate modes.
(define_mode_iterator PRED_ALL [VNx16BI VNx8BI VNx4BI VNx2BI])

;; SVE predicate modes that control 8-bit, 16-bit or 32-bit elements.
(define_mode_iterator PRED_BHS [VNx16BI VNx8BI VNx4BI])

;; SVE predicate modes that control 16-bit, 32-bit or 64-bit elements.
(define_mode_iterator PRED_HSD [VNx8BI VNx4BI VNx2BI])

;; Bfloat16 modes to which V4SF can be converted
(define_mode_iterator V4SF_TO_BF [V4BF V8BF])

;; ------------------------------------------------------------------
;; Unspec enumerations for Advance SIMD. These could well go into
;; aarch64.md but for their use in int_iterators here.
;; ------------------------------------------------------------------

(define_c_enum "unspec"
 [
    UNSPEC_ASHIFT_SIGNED	; Used in aarch-simd.md.
    UNSPEC_ASHIFT_UNSIGNED	; Used in aarch64-simd.md.
    UNSPEC_ABS		; Used in aarch64-simd.md.
    UNSPEC_FMAX		; Used in aarch64-simd.md.
    UNSPEC_FMAXNMV	; Used in aarch64-simd.md.
    UNSPEC_FMAXV	; Used in aarch64-simd.md.
    UNSPEC_FMIN		; Used in aarch64-simd.md.
    UNSPEC_FMINNMV	; Used in aarch64-simd.md.
    UNSPEC_FMINV	; Used in aarch64-simd.md.
    UNSPEC_FADDV	; Used in aarch64-simd.md.
    UNSPEC_ADDV		; Used in aarch64-simd.md.
    UNSPEC_SADDLV	; Used in aarch64-simd.md.
    UNSPEC_UADDLV	; Used in aarch64-simd.md.
    UNSPEC_SMAXV	; Used in aarch64-simd.md.
    UNSPEC_SMINV	; Used in aarch64-simd.md.
    UNSPEC_UMAXV	; Used in aarch64-simd.md.
    UNSPEC_UMINV	; Used in aarch64-simd.md.
    UNSPEC_SHADD	; Used in aarch64-simd.md.
    UNSPEC_UHADD	; Used in aarch64-simd.md.
    UNSPEC_SRHADD	; Used in aarch64-simd.md.
    UNSPEC_URHADD	; Used in aarch64-simd.md.
    UNSPEC_SHSUB	; Used in aarch64-simd.md.
    UNSPEC_UHSUB	; Used in aarch64-simd.md.
    UNSPEC_ADDHN	; Used in aarch64-simd.md.
    UNSPEC_RADDHN	; Used in aarch64-simd.md.
    UNSPEC_SUBHN	; Used in aarch64-simd.md.
    UNSPEC_RSUBHN	; Used in aarch64-simd.md.
    UNSPEC_ADDHN2	; Used in aarch64-simd.md.
    UNSPEC_RADDHN2	; Used in aarch64-simd.md.
    UNSPEC_SUBHN2	; Used in aarch64-simd.md.
    UNSPEC_RSUBHN2	; Used in aarch64-simd.md.
    UNSPEC_SQDMULH	; Used in aarch64-simd.md.
    UNSPEC_SQRDMULH	; Used in aarch64-simd.md.
    UNSPEC_PMUL		; Used in aarch64-simd.md.
    UNSPEC_FMULX	; Used in aarch64-simd.md.
    UNSPEC_USQADD	; Used in aarch64-simd.md.
    UNSPEC_SUQADD	; Used in aarch64-simd.md.
    UNSPEC_SQXTUN	; Used in aarch64-simd.md.
    UNSPEC_SQXTUN2	; Used in aarch64-simd.md.
    UNSPEC_SSRA		; Used in aarch64-simd.md.
    UNSPEC_USRA		; Used in aarch64-simd.md.
    UNSPEC_SRSRA	; Used in aarch64-simd.md.
    UNSPEC_URSRA	; Used in aarch64-simd.md.
    UNSPEC_SRSHR	; Used in aarch64-simd.md.
    UNSPEC_URSHR	; Used in aarch64-simd.md.
    UNSPEC_SQSHLU	; Used in aarch64-simd.md.
    UNSPEC_SQSHL	; Used in aarch64-simd.md.
    UNSPEC_UQSHL	; Used in aarch64-simd.md.
    UNSPEC_SQSHRUN	; Used in aarch64-simd.md.
    UNSPEC_SQRSHRUN	; Used in aarch64-simd.md.
    UNSPEC_SQSHRN	; Used in aarch64-simd.md.
    UNSPEC_UQSHRN	; Used in aarch64-simd.md.
    UNSPEC_SQRSHRN	; Used in aarch64-simd.md.
    UNSPEC_UQRSHRN	; Used in aarch64-simd.md.
    UNSPEC_SSHL		; Used in aarch64-simd.md.
    UNSPEC_USHL		; Used in aarch64-simd.md.
    UNSPEC_SRSHL	; Used in aarch64-simd.md.
    UNSPEC_URSHL	; Used in aarch64-simd.md.
    UNSPEC_SQRSHL	; Used in aarch64-simd.md.
    UNSPEC_UQRSHL	; Used in aarch64-simd.md.
    UNSPEC_SSLI		; Used in aarch64-simd.md.
    UNSPEC_USLI		; Used in aarch64-simd.md.
    UNSPEC_SSRI		; Used in aarch64-simd.md.
    UNSPEC_USRI		; Used in aarch64-simd.md.
    UNSPEC_SSHLL	; Used in aarch64-simd.md.
    UNSPEC_USHLL	; Used in aarch64-simd.md.
    UNSPEC_ADDP		; Used in aarch64-simd.md.
    UNSPEC_SADDLP	; Used in aarch64-simd.md.
    UNSPEC_UADDLP	; Used in aarch64-simd.md.
    UNSPEC_TBL		; Used in vector permute patterns.
    UNSPEC_TBX		; Used in vector permute patterns.
    UNSPEC_CONCAT	; Used in vector permute patterns.

    ;; The following permute unspecs are generated directly by
    ;; aarch64_expand_vec_perm_const, so any changes to the underlying
    ;; instructions would need a corresponding change there.
    UNSPEC_ZIP1		; Used in vector permute patterns.
    UNSPEC_ZIP2		; Used in vector permute patterns.
    UNSPEC_UZP1		; Used in vector permute patterns.
    UNSPEC_UZP2		; Used in vector permute patterns.
    UNSPEC_TRN1		; Used in vector permute patterns.
    UNSPEC_TRN2		; Used in vector permute patterns.
    UNSPEC_EXT		; Used in vector permute patterns.
    UNSPEC_REV64	; Used in vector reverse patterns (permute).
    UNSPEC_REV32	; Used in vector reverse patterns (permute).
    UNSPEC_REV16	; Used in vector reverse patterns (permute).

    UNSPEC_AESE		; Used in aarch64-simd.md.
    UNSPEC_AESD         ; Used in aarch64-simd.md.
    UNSPEC_AESMC        ; Used in aarch64-simd.md.
    UNSPEC_AESIMC       ; Used in aarch64-simd.md.
    UNSPEC_SHA1C	; Used in aarch64-simd.md.
    UNSPEC_SHA1M        ; Used in aarch64-simd.md.
    UNSPEC_SHA1P        ; Used in aarch64-simd.md.
    UNSPEC_SHA1H        ; Used in aarch64-simd.md.
    UNSPEC_SHA1SU0      ; Used in aarch64-simd.md.
    UNSPEC_SHA1SU1      ; Used in aarch64-simd.md.
    UNSPEC_SHA256H      ; Used in aarch64-simd.md.
    UNSPEC_SHA256H2     ; Used in aarch64-simd.md.
    UNSPEC_SHA256SU0    ; Used in aarch64-simd.md.
    UNSPEC_SHA256SU1    ; Used in aarch64-simd.md.
    UNSPEC_PMULL        ; Used in aarch64-simd.md.
    UNSPEC_PMULL2       ; Used in aarch64-simd.md.
    UNSPEC_REV_REGLIST  ; Used in aarch64-simd.md.
    UNSPEC_VEC_SHR      ; Used in aarch64-simd.md.
    UNSPEC_SQRDMLAH     ; Used in aarch64-simd.md.
    UNSPEC_SQRDMLSH     ; Used in aarch64-simd.md.
    UNSPEC_FMAXNM       ; Used in aarch64-simd.md.
    UNSPEC_FMINNM       ; Used in aarch64-simd.md.
    UNSPEC_SDOT		; Used in aarch64-simd.md.
    UNSPEC_UDOT		; Used in aarch64-simd.md.
    UNSPEC_SM3SS1	; Used in aarch64-simd.md.
    UNSPEC_SM3TT1A	; Used in aarch64-simd.md.
    UNSPEC_SM3TT1B	; Used in aarch64-simd.md.
    UNSPEC_SM3TT2A	; Used in aarch64-simd.md.
    UNSPEC_SM3TT2B	; Used in aarch64-simd.md.
    UNSPEC_SM3PARTW1	; Used in aarch64-simd.md.
    UNSPEC_SM3PARTW2	; Used in aarch64-simd.md.
    UNSPEC_SM4E		; Used in aarch64-simd.md.
    UNSPEC_SM4EKEY	; Used in aarch64-simd.md.
    UNSPEC_SHA512H      ; Used in aarch64-simd.md.
    UNSPEC_SHA512H2     ; Used in aarch64-simd.md.
    UNSPEC_SHA512SU0    ; Used in aarch64-simd.md.
    UNSPEC_SHA512SU1    ; Used in aarch64-simd.md.
    UNSPEC_FMLAL	; Used in aarch64-simd.md.
    UNSPEC_FMLSL	; Used in aarch64-simd.md.
    UNSPEC_FMLAL2	; Used in aarch64-simd.md.
    UNSPEC_FMLSL2	; Used in aarch64-simd.md.
    UNSPEC_ADR		; Used in aarch64-sve.md.
    UNSPEC_SEL		; Used in aarch64-sve.md.
    UNSPEC_BRKA		; Used in aarch64-sve.md.
    UNSPEC_BRKB		; Used in aarch64-sve.md.
    UNSPEC_BRKN		; Used in aarch64-sve.md.
    UNSPEC_BRKPA	; Used in aarch64-sve.md.
    UNSPEC_BRKPB	; Used in aarch64-sve.md.
    UNSPEC_PFIRST	; Used in aarch64-sve.md.
    UNSPEC_PNEXT	; Used in aarch64-sve.md.
    UNSPEC_CNTP		; Used in aarch64-sve.md.
    UNSPEC_SADDV	; Used in aarch64-sve.md.
    UNSPEC_UADDV	; Used in aarch64-sve.md.
    UNSPEC_ANDV		; Used in aarch64-sve.md.
    UNSPEC_IORV		; Used in aarch64-sve.md.
    UNSPEC_XORV		; Used in aarch64-sve.md.
    UNSPEC_ANDF		; Used in aarch64-sve.md.
    UNSPEC_IORF		; Used in aarch64-sve.md.
    UNSPEC_XORF		; Used in aarch64-sve.md.
    UNSPEC_REVB		; Used in aarch64-sve.md.
    UNSPEC_REVH		; Used in aarch64-sve.md.
    UNSPEC_REVW		; Used in aarch64-sve.md.
    UNSPEC_REVBHW	; Used in aarch64-sve.md.
    UNSPEC_SMUL_HIGHPART ; Used in aarch64-sve.md.
    UNSPEC_UMUL_HIGHPART ; Used in aarch64-sve.md.
    UNSPEC_FMLA		; Used in aarch64-sve.md.
    UNSPEC_FMLS		; Used in aarch64-sve.md.
    UNSPEC_FEXPA	; Used in aarch64-sve.md.
    UNSPEC_FMMLA	; Used in aarch64-sve.md.
    UNSPEC_FTMAD	; Used in aarch64-sve.md.
    UNSPEC_FTSMUL	; Used in aarch64-sve.md.
    UNSPEC_FTSSEL	; Used in aarch64-sve.md.
    UNSPEC_SMATMUL	; Used in aarch64-sve.md.
    UNSPEC_UMATMUL	; Used in aarch64-sve.md.
    UNSPEC_USMATMUL	; Used in aarch64-sve.md.
    UNSPEC_TRN1Q	; Used in aarch64-sve.md.
    UNSPEC_TRN2Q	; Used in aarch64-sve.md.
    UNSPEC_UZP1Q	; Used in aarch64-sve.md.
    UNSPEC_UZP2Q	; Used in aarch64-sve.md.
    UNSPEC_ZIP1Q	; Used in aarch64-sve.md.
    UNSPEC_ZIP2Q	; Used in aarch64-sve.md.
    UNSPEC_TRN1_CONV	; Used in aarch64-sve.md.
    UNSPEC_COND_CMPEQ_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPGE_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPGT_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPHI_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPHS_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPLE_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPLO_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPLS_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPLT_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_CMPNE_WIDE ; Used in aarch64-sve.md.
    UNSPEC_COND_FABS	; Used in aarch64-sve.md.
    UNSPEC_COND_FADD	; Used in aarch64-sve.md.
    UNSPEC_COND_FCADD90	; Used in aarch64-sve.md.
    UNSPEC_COND_FCADD270 ; Used in aarch64-sve.md.
    UNSPEC_COND_FCMEQ	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMGE	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMGT	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMLA	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMLA90	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMLA180 ; Used in aarch64-sve.md.
    UNSPEC_COND_FCMLA270 ; Used in aarch64-sve.md.
    UNSPEC_COND_FCMLE	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMLT	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMNE	; Used in aarch64-sve.md.
    UNSPEC_COND_FCMUO	; Used in aarch64-sve.md.
    UNSPEC_COND_FCVT	; Used in aarch64-sve.md.
    UNSPEC_COND_FCVTZS	; Used in aarch64-sve.md.
    UNSPEC_COND_FCVTZU	; Used in aarch64-sve.md.
    UNSPEC_COND_FDIV	; Used in aarch64-sve.md.
    UNSPEC_COND_FMAX	; Used in aarch64-sve.md.
    UNSPEC_COND_FMAXNM	; Used in aarch64-sve.md.
    UNSPEC_COND_FMIN	; Used in aarch64-sve.md.
    UNSPEC_COND_FMINNM	; Used in aarch64-sve.md.
    UNSPEC_COND_FMLA	; Used in aarch64-sve.md.
    UNSPEC_COND_FMLS	; Used in aarch64-sve.md.
    UNSPEC_COND_FMUL	; Used in aarch64-sve.md.
    UNSPEC_COND_FMULX	; Used in aarch64-sve.md.
    UNSPEC_COND_FNEG	; Used in aarch64-sve.md.
    UNSPEC_COND_FNMLA	; Used in aarch64-sve.md.
    UNSPEC_COND_FNMLS	; Used in aarch64-sve.md.
    UNSPEC_COND_FRECPX	; Used in aarch64-sve.md.
    UNSPEC_COND_FRINTA	; Used in aarch64-sve.md.
    UNSPEC_COND_FRINTI	; Used in aarch64-sve.md.
    UNSPEC_COND_FRINTM	; Used in aarch64-sve.md.
    UNSPEC_COND_FRINTN	; Used in aarch64-sve.md.
    UNSPEC_COND_FRINTP	; Used in aarch64-sve.md.
    UNSPEC_COND_FRINTX	; Used in aarch64-sve.md.
    UNSPEC_COND_FRINTZ	; Used in aarch64-sve.md.
    UNSPEC_COND_FSCALE	; Used in aarch64-sve.md.
    UNSPEC_COND_FSQRT	; Used in aarch64-sve.md.
    UNSPEC_COND_FSUB	; Used in aarch64-sve.md.
    UNSPEC_COND_SCVTF	; Used in aarch64-sve.md.
    UNSPEC_COND_UCVTF	; Used in aarch64-sve.md.
    UNSPEC_LASTA	; Used in aarch64-sve.md.
    UNSPEC_LASTB	; Used in aarch64-sve.md.
    UNSPEC_ASHIFT_WIDE  ; Used in aarch64-sve.md.
    UNSPEC_ASHIFTRT_WIDE ; Used in aarch64-sve.md.
    UNSPEC_LSHIFTRT_WIDE ; Used in aarch64-sve.md.
    UNSPEC_LDFF1	; Used in aarch64-sve.md.
    UNSPEC_LDNF1	; Used in aarch64-sve.md.
    UNSPEC_FCADD90	; Used in aarch64-simd.md.
    UNSPEC_FCADD270	; Used in aarch64-simd.md.
    UNSPEC_FCMLA	; Used in aarch64-simd.md.
    UNSPEC_FCMLA90	; Used in aarch64-simd.md.
    UNSPEC_FCMLA180	; Used in aarch64-simd.md.
    UNSPEC_FCMLA270	; Used in aarch64-simd.md.
    UNSPEC_FCMUL	; Used in aarch64-simd.md.
    UNSPEC_FCMUL_CONJ	; Used in aarch64-simd.md.
    UNSPEC_FCMLA_CONJ	; Used in aarch64-simd.md.
    UNSPEC_FCMLA180_CONJ	; Used in aarch64-simd.md.
    UNSPEC_ASRD		; Used in aarch64-sve.md.
    UNSPEC_ADCLB	; Used in aarch64-sve2.md.
    UNSPEC_ADCLT	; Used in aarch64-sve2.md.
    UNSPEC_ADDHNB	; Used in aarch64-sve2.md.
    UNSPEC_ADDHNT	; Used in aarch64-sve2.md.
    UNSPEC_BDEP		; Used in aarch64-sve2.md.
    UNSPEC_BEXT		; Used in aarch64-sve2.md.
    UNSPEC_BGRP		; Used in aarch64-sve2.md.
    UNSPEC_CADD270	; Used in aarch64-sve2.md.
    UNSPEC_CADD90	; Used in aarch64-sve2.md.
    UNSPEC_CDOT		; Used in aarch64-sve2.md.
    UNSPEC_CDOT180	; Used in aarch64-sve2.md.
    UNSPEC_CDOT270	; Used in aarch64-sve2.md.
    UNSPEC_CDOT90	; Used in aarch64-sve2.md.
    UNSPEC_CMLA		; Used in aarch64-sve2.md.
    UNSPEC_CMLA180	; Used in aarch64-sve2.md.
    UNSPEC_CMLA270	; Used in aarch64-sve2.md.
    UNSPEC_CMLA90	; Used in aarch64-sve2.md.
    UNSPEC_CMLA_CONJ	; Used in aarch64-sve2.md.
    UNSPEC_CMLA180_CONJ	; Used in aarch64-sve2.md.
    UNSPEC_CMUL		; Used in aarch64-sve2.md.
    UNSPEC_CMUL_CONJ	; Used in aarch64-sve2.md.
    UNSPEC_COND_FCVTLT	; Used in aarch64-sve2.md.
    UNSPEC_COND_FCVTNT	; Used in aarch64-sve2.md.
    UNSPEC_COND_FCVTX	; Used in aarch64-sve2.md.
    UNSPEC_COND_FCVTXNT	; Used in aarch64-sve2.md.
    UNSPEC_COND_FLOGB	; Used in aarch64-sve2.md.
    UNSPEC_EORBT	; Used in aarch64-sve2.md.
    UNSPEC_EORTB	; Used in aarch64-sve2.md.
    UNSPEC_FADDP	; Used in aarch64-sve2.md.
    UNSPEC_FMAXNMP	; Used in aarch64-sve2.md.
    UNSPEC_FMAXP	; Used in aarch64-sve2.md.
    UNSPEC_FMINNMP	; Used in aarch64-sve2.md.
    UNSPEC_FMINP	; Used in aarch64-sve2.md.
    UNSPEC_FMLALB	; Used in aarch64-sve2.md.
    UNSPEC_FMLALT	; Used in aarch64-sve2.md.
    UNSPEC_FMLSLB	; Used in aarch64-sve2.md.
    UNSPEC_FMLSLT	; Used in aarch64-sve2.md.
    UNSPEC_HISTCNT	; Used in aarch64-sve2.md.
    UNSPEC_HISTSEG	; Used in aarch64-sve2.md.
    UNSPEC_MATCH	; Used in aarch64-sve2.md.
    UNSPEC_NMATCH	; Used in aarch64-sve2.md.
    UNSPEC_PMULLB	; Used in aarch64-sve2.md.
    UNSPEC_PMULLB_PAIR	; Used in aarch64-sve2.md.
    UNSPEC_PMULLT	; Used in aarch64-sve2.md.
    UNSPEC_PMULLT_PAIR	; Used in aarch64-sve2.md.
    UNSPEC_RADDHNB	; Used in aarch64-sve2.md.
    UNSPEC_RADDHNT	; Used in aarch64-sve2.md.
    UNSPEC_RSHRNB	; Used in aarch64-sve2.md.
    UNSPEC_RSHRNT	; Used in aarch64-sve2.md.
    UNSPEC_RSUBHNB	; Used in aarch64-sve2.md.
    UNSPEC_RSUBHNT	; Used in aarch64-sve2.md.
    UNSPEC_SABDLB	; Used in aarch64-sve2.md.
    UNSPEC_SABDLT	; Used in aarch64-sve2.md.
    UNSPEC_SADDLB	; Used in aarch64-sve2.md.
    UNSPEC_SADDLBT	; Used in aarch64-sve2.md.
    UNSPEC_SADDLT	; Used in aarch64-sve2.md.
    UNSPEC_SADDWB	; Used in aarch64-sve2.md.
    UNSPEC_SADDWT	; Used in aarch64-sve2.md.
    UNSPEC_SBCLB	; Used in aarch64-sve2.md.
    UNSPEC_SBCLT	; Used in aarch64-sve2.md.
    UNSPEC_SHRNB	; Used in aarch64-sve2.md.
    UNSPEC_SHRNT	; Used in aarch64-sve2.md.
    UNSPEC_SLI		; Used in aarch64-sve2.md.
    UNSPEC_SMAXP	; Used in aarch64-sve2.md.
    UNSPEC_SMINP	; Used in aarch64-sve2.md.
    UNSPEC_SMULHRS	; Used in aarch64-sve2.md.
    UNSPEC_SMULHS	; Used in aarch64-sve2.md.
    UNSPEC_SMULLB	; Used in aarch64-sve2.md.
    UNSPEC_SMULLT	; Used in aarch64-sve2.md.
    UNSPEC_SQCADD270	; Used in aarch64-sve2.md.
    UNSPEC_SQCADD90	; Used in aarch64-sve2.md.
    UNSPEC_SQDMULLB	; Used in aarch64-sve2.md.
    UNSPEC_SQDMULLBT	; Used in aarch64-sve2.md.
    UNSPEC_SQDMULLT	; Used in aarch64-sve2.md.
    UNSPEC_SQRDCMLAH	; Used in aarch64-sve2.md.
    UNSPEC_SQRDCMLAH180	; Used in aarch64-sve2.md.
    UNSPEC_SQRDCMLAH270	; Used in aarch64-sve2.md.
    UNSPEC_SQRDCMLAH90	; Used in aarch64-sve2.md.
    UNSPEC_SQRSHRNB	; Used in aarch64-sve2.md.
    UNSPEC_SQRSHRNT	; Used in aarch64-sve2.md.
    UNSPEC_SQRSHRUNB	; Used in aarch64-sve2.md.
    UNSPEC_SQRSHRUNT	; Used in aarch64-sve2.md.
    UNSPEC_SQSHRNB	; Used in aarch64-sve2.md.
    UNSPEC_SQSHRNT	; Used in aarch64-sve2.md.
    UNSPEC_SQSHRUNB	; Used in aarch64-sve2.md.
    UNSPEC_SQSHRUNT	; Used in aarch64-sve2.md.
    UNSPEC_SQXTNB	; Used in aarch64-sve2.md.
    UNSPEC_SQXTNT	; Used in aarch64-sve2.md.
    UNSPEC_SQXTUNB	; Used in aarch64-sve2.md.
    UNSPEC_SQXTUNT	; Used in aarch64-sve2.md.
    UNSPEC_SRI		; Used in aarch64-sve2.md.
    UNSPEC_SSHLLB	; Used in aarch64-sve2.md.
    UNSPEC_SSHLLT	; Used in aarch64-sve2.md.
    UNSPEC_SSUBLB	; Used in aarch64-sve2.md.
    UNSPEC_SSUBLBT	; Used in aarch64-sve2.md.
    UNSPEC_SSUBLT	; Used in aarch64-sve2.md.
    UNSPEC_SSUBLTB	; Used in aarch64-sve2.md.
    UNSPEC_SSUBWB	; Used in aarch64-sve2.md.
    UNSPEC_SSUBWT	; Used in aarch64-sve2.md.
    UNSPEC_SUBHNB	; Used in aarch64-sve2.md.
    UNSPEC_SUBHNT	; Used in aarch64-sve2.md.
    UNSPEC_TBL2		; Used in aarch64-sve2.md.
    UNSPEC_UABDLB	; Used in aarch64-sve2.md.
    UNSPEC_UABDLT	; Used in aarch64-sve2.md.
    UNSPEC_UADDLB	; Used in aarch64-sve2.md.
    UNSPEC_UADDLT	; Used in aarch64-sve2.md.
    UNSPEC_UADDWB	; Used in aarch64-sve2.md.
    UNSPEC_UADDWT	; Used in aarch64-sve2.md.
    UNSPEC_UMAXP	; Used in aarch64-sve2.md.
    UNSPEC_UMINP	; Used in aarch64-sve2.md.
    UNSPEC_UMULHRS	; Used in aarch64-sve2.md.
    UNSPEC_UMULHS	; Used in aarch64-sve2.md.
    UNSPEC_UMULLB	; Used in aarch64-sve2.md.
    UNSPEC_UMULLT	; Used in aarch64-sve2.md.
    UNSPEC_UQRSHRNB	; Used in aarch64-sve2.md.
    UNSPEC_UQRSHRNT	; Used in aarch64-sve2.md.
    UNSPEC_UQSHRNB	; Used in aarch64-sve2.md.
    UNSPEC_UQSHRNT	; Used in aarch64-sve2.md.
    UNSPEC_UQXTNB	; Used in aarch64-sve2.md.
    UNSPEC_UQXTNT	; Used in aarch64-sve2.md.
    UNSPEC_USHLLB	; Used in aarch64-sve2.md.
    UNSPEC_USHLLT	; Used in aarch64-sve2.md.
    UNSPEC_USUBLB	; Used in aarch64-sve2.md.
    UNSPEC_USUBLT	; Used in aarch64-sve2.md.
    UNSPEC_USUBWB	; Used in aarch64-sve2.md.
    UNSPEC_USUBWT	; Used in aarch64-sve2.md.
    UNSPEC_USDOT	; Used in aarch64-simd.md.
    UNSPEC_SUDOT	; Used in aarch64-simd.md.
    UNSPEC_BFDOT	; Used in aarch64-simd.md.
    UNSPEC_BFMLALB	; Used in aarch64-sve.md.
    UNSPEC_BFMLALT	; Used in aarch64-sve.md.
    UNSPEC_BFMMLA	; Used in aarch64-sve.md.
    UNSPEC_BFCVTN      ; Used in aarch64-simd.md.
    UNSPEC_BFCVTN2     ; Used in aarch64-simd.md.
    UNSPEC_BFCVT       ; Used in aarch64-simd.md.
    UNSPEC_FCVTXN	; Used in aarch64-simd.md.
])

;; ------------------------------------------------------------------
;; Unspec enumerations for Atomics.  They are here so that they can be
;; used in the int_iterators for atomic operations.
;; ------------------------------------------------------------------

(define_c_enum "unspecv"
 [
    UNSPECV_LX			; Represent a load-exclusive.
    UNSPECV_SX			; Represent a store-exclusive.
    UNSPECV_LDA			; Represent an atomic load or load-acquire.
    UNSPECV_STL			; Represent an atomic store or store-release.
    UNSPECV_ATOMIC_CMPSW	; Represent an atomic compare swap.
    UNSPECV_ATOMIC_EXCHG	; Represent an atomic exchange.
    UNSPECV_ATOMIC_CAS		; Represent an atomic CAS.
    UNSPECV_ATOMIC_SWP		; Represent an atomic SWP.
    UNSPECV_ATOMIC_OP		; Represent an atomic operation.
    UNSPECV_ATOMIC_LDOP_OR	; Represent an atomic load-or
    UNSPECV_ATOMIC_LDOP_BIC	; Represent an atomic load-bic
    UNSPECV_ATOMIC_LDOP_XOR	; Represent an atomic load-xor
    UNSPECV_ATOMIC_LDOP_PLUS	; Represent an atomic load-add
])

;; -------------------------------------------------------------------
;; Mode attributes
;; -------------------------------------------------------------------

;; "e" for signaling operations, "" for quiet operations.
(define_mode_attr e [(CCFP "") (CCFPE "e")])

;; In GPI templates, a string like "%<w>0" will expand to "%w0" in the
;; 32-bit version and "%x0" in the 64-bit version.
(define_mode_attr w [(QI "w") (HI "w") (SI "w") (DI "x") (SF "s") (DF "d")])

;; The size of access, in bytes.
(define_mode_attr ldst_sz [(SI "4") (DI "8")])
;; Likewise for load/store pair.
(define_mode_attr ldpstp_sz [(SI "8") (DI "16")])

;; For inequal width int to float conversion
(define_mode_attr w1 [(HF "w") (SF "w") (DF "x")])
(define_mode_attr w2 [(HF "x") (SF "x") (DF "w")])

;; For width of fp registers in fcvt instruction
(define_mode_attr fpw [(DI "s") (SI "d")])

(define_mode_attr short_mask [(HI "65535") (QI "255")])

;; For constraints used in scalar immediate vector moves
(define_mode_attr hq [(HI "h") (QI "q")])

;; For doubling width of an integer mode
(define_mode_attr DWI [(QI "HI") (HI "SI") (SI "DI") (DI "TI")])

(define_mode_attr fcvt_change_mode [(SI "df") (DI "sf")])

(define_mode_attr FCVT_CHANGE_MODE [(SI "DF") (DI "SF")])

;; For scalar usage of vector/FP registers
(define_mode_attr v [(QI "b") (HI "h") (SI "s") (DI "d")
		    (HF  "h") (SF "s") (DF "d")
		    (V8QI "") (V16QI "")
		    (V4HI "") (V8HI "")
		    (V2SI "") (V4SI  "")
		    (V2DI "") (V2SF "")
		    (V4SF "") (V4HF "")
		    (V8HF "") (V2DF "")])

;; For scalar usage of vector/FP registers, narrowing
(define_mode_attr vn2 [(QI "") (HI "b") (SI "h") (DI "s")
		    (V8QI "") (V16QI "")
		    (V4HI "") (V8HI "")
		    (V2SI "") (V4SI  "")
		    (V2DI "") (V2SF "")
		    (V4SF "") (V2DF "")])

;; For scalar usage of vector/FP registers, widening
(define_mode_attr vw2 [(DI "") (QI "h") (HI "s") (SI "d")
		    (V8QI "") (V16QI "")
		    (V4HI "") (V8HI "")
		    (V2SI "") (V4SI  "")
		    (V2DI "") (V2SF "")
		    (V4SF "") (V2DF "")])

;; Register Type Name and Vector Arrangement Specifier for when
;; we are doing scalar for DI and SIMD for SI (ignoring all but
;; lane 0).
(define_mode_attr rtn [(DI "d") (SI "")])
(define_mode_attr vas [(DI "") (SI ".2s")])

;; Map a vector to the number of units in it, if the size of the mode
;; is constant.
(define_mode_attr nunits [(V8QI "8") (V16QI "16")
			  (V4HI "4") (V8HI "8")
			  (V2SI "2") (V4SI "4")
				     (V2DI "2")
			  (V4HF "4") (V8HF "8")
			  (V4BF "4") (V8BF "8")
			  (V2SF "2") (V4SF "4")
			  (V1DF "1") (V2DF "2")
			  (DI "1") (DF "1")])

;; Map a mode to the number of bits in it, if the size of the mode
;; is constant.
(define_mode_attr bitsize [(V8QI "64") (V16QI "128")
			   (V4HI "64") (V8HI "128")
			   (V2SI "64") (V4SI "128")
				       (V2DI "128")])

;; Map a floating point or integer mode to the appropriate register name prefix
(define_mode_attr s [(HF "h") (SF "s") (DF "d") (SI "s") (DI "d")])

;; Give the length suffix letter for a sign- or zero-extension.
(define_mode_attr size [(QI "b") (HI "h") (SI "w")])

;; Give the number of bits in the mode
(define_mode_attr sizen [(QI "8") (HI "16") (SI "32") (DI "64")])

;; Give the ordinal of the MSB in the mode
(define_mode_attr sizem1 [(QI "#7") (HI "#15") (SI "#31") (DI "#63")
			  (HF "#15") (SF "#31") (DF "#63")])

;; The number of bits in a vector element, or controlled by a predicate
;; element.
(define_mode_attr elem_bits [(VNx16BI "8") (VNx8BI "16")
			     (VNx4BI "32") (VNx2BI "64")
			     (VNx16QI "8") (VNx8HI "16")
			     (VNx4SI "32") (VNx2DI "64")
			     (VNx8HF "16") (VNx4SF "32") (VNx2DF "64")])

;; The number of bits in a vector container.
(define_mode_attr container_bits [(VNx16QI "8")
				  (VNx8HI "16") (VNx8QI "16") (VNx8HF "16")
				  (VNx8BF "16")
				  (VNx4SI "32") (VNx4HI "32") (VNx4QI "32")
				  (VNx4SF "32") (VNx4HF "32") (VNx4BF "32")
				  (VNx2DI "64") (VNx2SI "64") (VNx2HI "64")
				  (VNx2QI "64") (VNx2DF "64") (VNx2SF "64")
				  (VNx2HF "64") (VNx2BF "64")])

;; Attribute to describe constants acceptable in logical operations
(define_mode_attr lconst [(SI "K") (DI "L")])

;; Attribute to describe constants acceptable in logical and operations
(define_mode_attr lconst2 [(SI "UsO") (DI "UsP")])

;; Map a mode to a specific constraint character.
(define_mode_attr cmode [(QI "q") (HI "h") (SI "s") (DI "d")])

;; Map modes to Usg and Usj constraints for SISD right shifts
(define_mode_attr cmode_simd [(SI "g") (DI "j")])

(define_mode_attr Vtype [(V8QI "8b") (V16QI "16b")
			 (V4HI "4h") (V8HI  "8h")
			 (V4BF "4h") (V8BF  "8h")
                         (V2SI "2s") (V4SI  "4s")
                         (DI   "1d") (DF    "1d")
                         (V2DI "2d") (V2SF "2s")
			 (V4SF "4s") (V2DF "2d")
			 (V4HF "4h") (V8HF "8h")])

;; Map mode to type used in widening multiplies.
(define_mode_attr Vcondtype [(V4HI "4h") (V8HI "4h") (V2SI "2s") (V4SI "2s")])

;; Map lane mode to name
(define_mode_attr Qlane [(V4HI "_v4hi") (V8HI  "q_v4hi")
			 (V2SI "_v2si") (V4SI  "q_v2si")])

(define_mode_attr Vrevsuff [(V4HI "16") (V8HI "16") (V2SI "32")
                            (V4SI "32") (V2DI "64")])

(define_mode_attr Vmtype [(V8QI ".8b") (V16QI ".16b")
			 (V4HI ".4h") (V8HI  ".8h")
			 (V2SI ".2s") (V4SI  ".4s")
			 (V2DI ".2d") (V4HF ".4h")
			 (V8HF ".8h") (V4BF ".4h")
			 (V8BF ".8h") (V2SF ".2s")
			 (V4SF ".4s") (V2DF ".2d")
			 (DI   "")    (SI   "")
			 (HI   "")    (QI   "")
			 (TI   "")    (HF   "")
			 (SF   "")    (DF   "")])

;; Register suffix narrowed modes for VQN.
(define_mode_attr Vmntype [(V8HI ".8b") (V4SI ".4h")
			   (V2DI ".2s")
			   (DI   "")    (SI   "")
			   (HI   "")])

;; Mode-to-individual element type mapping.
(define_mode_attr Vetype [(V8QI "b") (V16QI "b")
			  (V4HI "h") (V8HI  "h")
			  (V2SI "s") (V4SI  "s")
			  (V2DI "d")
			  (V4HF "h") (V8HF  "h")
			  (V2SF "s") (V4SF  "s")
			  (V2DF "d")
			  (VNx16BI "b") (VNx8BI "h") (VNx4BI "s") (VNx2BI "d")
			  (VNx16QI "b") (VNx8QI "b") (VNx4QI "b") (VNx2QI "b")
			  (VNx8HI "h") (VNx4HI "h") (VNx2HI "h")
			  (VNx8HF "h") (VNx4HF "h") (VNx2HF "h")
			  (VNx8BF "h") (VNx4BF "h") (VNx2BF "h")
			  (VNx4SI "s") (VNx2SI "s")
			  (VNx4SF "s") (VNx2SF "s")
			  (VNx2DI "d")
			  (VNx2DF "d")
			  (BF "h") (V4BF "h") (V8BF "h")
			  (HF "h")
			  (SF "s") (DF "d")
			  (QI "b") (HI "h")
			  (SI "s") (DI "d")])

;; Like Vetype, but map to types that are a quarter of the element size.
(define_mode_attr Vetype_fourth [(VNx4SI "b") (VNx2DI "h")])

;; Equivalent of "size" for a vector element.
(define_mode_attr Vesize [(VNx16QI "b") (VNx8QI "b") (VNx4QI "b") (VNx2QI "b")
			  (VNx8HI "h") (VNx4HI "h") (VNx2HI "h")
			  (VNx8HF "h") (VNx4HF "h") (VNx2HF "h")
			  (VNx8BF "h") (VNx4BF "h") (VNx2BF "h")
			  (VNx4SI "w") (VNx2SI "w")
			  (VNx4SF "w") (VNx2SF "w")
			  (VNx2DI "d")
			  (VNx2DF "d")
			  (VNx32QI "b") (VNx48QI "b") (VNx64QI "b")
			  (VNx16HI "h") (VNx24HI "h") (VNx32HI "h")
			  (VNx16HF "h") (VNx24HF "h") (VNx32HF "h")
			  (VNx16BF "h") (VNx24BF "h") (VNx32BF "h")
			  (VNx8SI  "w") (VNx12SI "w") (VNx16SI "w")
			  (VNx8SF  "w") (VNx12SF "w") (VNx16SF "w")
			  (VNx4DI  "d") (VNx6DI  "d") (VNx8DI  "d")
			  (VNx4DF  "d") (VNx6DF  "d") (VNx8DF  "d")])

;; The Z register suffix for an SVE mode's element container, i.e. the
;; Vetype of full SVE modes that have the same number of elements.
(define_mode_attr Vctype [(VNx16QI "b") (VNx8QI "h") (VNx4QI "s") (VNx2QI "d")
			  (VNx8HI "h") (VNx4HI "s") (VNx2HI "d")
			  (VNx8HF "h") (VNx4HF "s") (VNx2HF "d")
			  (VNx8BF "h") (VNx4BF "s") (VNx2BF "d")
			  (VNx4SI "s") (VNx2SI "d")
			  (VNx4SF "s") (VNx2SF "d")
			  (VNx2DI "d")
			  (VNx2DF "d")])

;; The instruction mnemonic suffix for an SVE mode's element container,
;; i.e. the Vewtype of full SVE modes that have the same number of elements.
(define_mode_attr Vcwtype [(VNx16QI "b") (VNx8QI "h") (VNx4QI "w") (VNx2QI "d")
			   (VNx8HI "h") (VNx4HI "w") (VNx2HI "d")
			   (VNx8HF "h") (VNx4HF "w") (VNx2HF "d")
			   (VNx8BF "h") (VNx4BF "w") (VNx2BF "d")
			   (VNx4SI "w") (VNx2SI "d")
			   (VNx4SF "w") (VNx2SF "d")
			   (VNx2DI "d")
			   (VNx2DF "d")])

;; Vetype is used everywhere in scheduling type and assembly output,
;; sometimes they are not the same, for example HF modes on some
;; instructions.  stype is defined to represent scheduling type
;; more accurately.
(define_mode_attr stype [(V8QI "b") (V16QI "b") (V4HI "s") (V8HI "s")
			 (V2SI "s") (V4SI "s") (V2DI "d") (V4HF "s")
			 (V8HF "s") (V2SF "s") (V4SF "s") (V2DF "d")
			 (HF "s") (SF "s") (DF "d") (QI "b") (HI "s")
			 (SI "s") (DI "d")])

;; Mode-to-bitwise operation type mapping.
(define_mode_attr Vbtype [(V8QI "8b")  (V16QI "16b")
			  (V4HI "8b") (V8HI  "16b")
			  (V2SI "8b") (V4SI  "16b")
			  (V2DI "16b") (V4HF "8b")
			  (V8HF "16b") (V2SF  "8b")
			  (V4SF "16b") (V2DF  "16b")
			  (DI   "8b")  (DF    "8b")
			  (SI   "8b")  (SF    "8b")
			  (V4BF "8b")  (V8BF  "16b")])

;; Define element mode for each vector mode.
(define_mode_attr VEL [(V8QI  "QI") (V16QI "QI")
		       (V4HI "HI") (V8HI  "HI")
		       (V2SI "SI") (V4SI  "SI")
		       (DI   "DI") (V2DI  "DI")
		       (V4HF "HF") (V8HF  "HF")
		       (V2SF "SF") (V4SF  "SF")
		       (DF   "DF") (V2DF  "DF")
		       (SI   "SI") (HI    "HI")
		       (QI   "QI")
		       (V4BF "BF") (V8BF "BF")
		       (VNx16QI "QI") (VNx8QI "QI") (VNx4QI "QI") (VNx2QI "QI")
		       (VNx8HI "HI") (VNx4HI "HI") (VNx2HI "HI")
		       (VNx8HF "HF") (VNx4HF "HF") (VNx2HF "HF")
		       (VNx8BF "BF") (VNx4BF "BF") (VNx2BF "BF")
		       (VNx4SI "SI") (VNx2SI "SI")
		       (VNx4SF "SF") (VNx2SF "SF")
		       (VNx2DI "DI")
		       (VNx2DF "DF")])

;; Define element mode for each vector mode (lower case).
(define_mode_attr Vel [(V8QI "qi") (V16QI "qi")
		       (V4HI "hi") (V8HI "hi")
		       (V2SI "si") (V4SI "si")
		       (DI   "di") (V2DI "di")
		       (V4HF "hf") (V8HF "hf")
		       (V2SF "sf") (V4SF "sf")
		       (V2DF "df") (DF   "df")
		       (SI   "si") (HI   "hi")
		       (QI   "qi")
		       (V4BF "bf") (V8BF "bf")
		       (VNx16QI "qi") (VNx8QI "qi") (VNx4QI "qi") (VNx2QI "qi")
		       (VNx8HI "hi") (VNx4HI "hi") (VNx2HI "hi")
		       (VNx8HF "hf") (VNx4HF "hf") (VNx2HF "hf")
		       (VNx8BF "bf") (VNx4BF "bf") (VNx2BF "bf")
		       (VNx4SI "si") (VNx2SI "si")
		       (VNx4SF "sf") (VNx2SF "sf")
		       (VNx2DI "di")
		       (VNx2DF "df")])

;; Element mode with floating-point values replaced by like-sized integers.
(define_mode_attr VEL_INT [(VNx16QI "QI")
			   (VNx8HI  "HI") (VNx8HF "HI") (VNx8BF "HI")
			   (VNx4SI  "SI") (VNx4SF "SI")
			   (VNx2DI  "DI") (VNx2DF "DI")])

;; Gives the mode of the 128-bit lowpart of an SVE vector.
(define_mode_attr V128 [(VNx16QI "V16QI")
			(VNx8HI  "V8HI") (VNx8HF "V8HF") (VNx8BF "V8BF")
			(VNx4SI  "V4SI") (VNx4SF "V4SF")
			(VNx2DI  "V2DI") (VNx2DF "V2DF")])

;; ...and again in lower case.
(define_mode_attr v128 [(VNx16QI "v16qi")
			(VNx8HI  "v8hi") (VNx8HF "v8hf") (VNx8BF "v8bf")
			(VNx4SI  "v4si") (VNx4SF "v4sf")
			(VNx2DI  "v2di") (VNx2DF "v2df")])

;; 64-bit container modes the inner or scalar source mode.
(define_mode_attr VCOND [(HI "V4HI") (SI "V2SI")
			 (V4HI "V4HI") (V8HI "V4HI")
			 (V2SI "V2SI") (V4SI "V2SI")
			 (DI   "DI") (V2DI "DI")
			 (V2SF "V2SF") (V4SF "V2SF")
			 (V2DF "DF")])

;; 128-bit container modes the inner or scalar source mode.
(define_mode_attr VCONQ [(V8QI "V16QI") (V16QI "V16QI")
			 (V4HI "V8HI") (V8HI "V8HI")
			 (V2SI "V4SI") (V4SI "V4SI")
			 (DI   "V2DI") (V2DI "V2DI")
			 (V4HF "V8HF") (V8HF "V8HF")
			 (V2SF "V2SF") (V4SF "V4SF")
			 (V2DF "V2DF") (SI   "V4SI")
			 (HI   "V8HI") (QI   "V16QI")])

;; Half modes of all vector modes.
(define_mode_attr VHALF [(V8QI "V4QI")  (V16QI "V8QI")
			 (V4HI "V2HI")  (V8HI  "V4HI")
			 (V2SI "SI")    (V4SI  "V2SI")
			 (V2DI "DI")    (V2SF  "SF")
			 (V4SF "V2SF")  (V4HF "V2HF")
			 (V8HF "V4HF")  (V2DF  "DF")
			 (V8BF "V4BF")])

;; Half modes of all vector modes, in lower-case.
(define_mode_attr Vhalf [(V8QI "v4qi")  (V16QI "v8qi")
			 (V4HI "v2hi")  (V8HI  "v4hi")
			 (V8HF  "v4hf") (V8BF  "v4bf")
			 (V2SI "si")    (V4SI  "v2si")
			 (V2DI "di")    (V2SF  "sf")
			 (V4SF "v2sf")  (V2DF  "df")])

;; Double modes of vector modes.
(define_mode_attr VDBL [(V8QI "V16QI") (V4HI "V8HI")
			(V4HF "V8HF")  (V4BF "V8BF")
			(V2SI "V4SI")  (V2SF "V4SF")
			(SI   "V2SI")  (DI   "V2DI")
			(DF   "V2DF")])

;; Register suffix for double-length mode.
(define_mode_attr Vdtype [(V4HF "8h") (V2SF "4s")])

;; Double modes of vector modes (lower case).
(define_mode_attr Vdbl [(V8QI "v16qi") (V4HI "v8hi")
			(V4HF "v8hf")  (V4BF "v8bf")
			(V2SI "v4si")  (V2SF "v4sf")
			(SI   "v2si")  (DI   "v2di")
			(DF   "v2df")])

;; Modes with double-width elements.
(define_mode_attr VDBLW [(V8QI "V4HI") (V16QI "V8HI")
                  (V4HI "V2SI") (V8HI "V4SI")
                  (V2SI "DI")   (V4SI "V2DI")])

;; Narrowed modes for VDN.
(define_mode_attr VNARROWD [(V4HI "V8QI") (V2SI "V4HI")
			    (DI   "V2SI")])

;; Narrowed double-modes for VQN (Used for XTN).
(define_mode_attr VNARROWQ [(V8HI "V8QI") (V4SI "V4HI")
			    (V2DI "V2SI")
			    (DI	  "SI")	  (SI	"HI")
			    (HI	  "QI")])
(define_mode_attr Vnarrowq [(V8HI "v8qi") (V4SI "v4hi")
			    (V2DI "v2si")])

;; Narrowed quad-modes for VQN (Used for XTN2).
(define_mode_attr VNARROWQ2 [(V8HI "V16QI") (V4SI "V8HI")
			     (V2DI "V4SI")])

;; Narrowed modes of vector modes.
(define_mode_attr VNARROW [(VNx8HI "VNx16QI")
			   (VNx4SI "VNx8HI") (VNx4SF "VNx8HF")
			   (VNx2DI "VNx4SI") (VNx2DF "VNx4SF")])

;; Register suffix narrowed modes for VQN.
(define_mode_attr Vntype [(V8HI "8b") (V4SI "4h")
			  (V2DI "2s")])

;; Register suffix narrowed modes for VQN.
(define_mode_attr V2ntype [(V8HI "16b") (V4SI "8h")
			   (V2DI "4s")])

;; Widened modes of vector modes.
(define_mode_attr VWIDE [(V8QI  "V8HI")  (V4HI  "V4SI")
			 (V2SI  "V2DI")  (V16QI "V8HI")
			 (V8HI  "V4SI")  (V4SI  "V2DI")
			 (HI    "SI")    (SI    "DI")
			 (V8HF  "V4SF")  (V4SF  "V2DF")
			 (V4HF  "V4SF")  (V2SF  "V2DF")
			 (VNx8HF  "VNx4SF") (VNx4SF "VNx2DF")
			 (VNx16QI "VNx8HI") (VNx8HI "VNx4SI")
			 (VNx4SI  "VNx2DI")
			 (VNx16BI "VNx8BI") (VNx8BI "VNx4BI")
			 (VNx4BI  "VNx2BI")])

;; Predicate mode associated with VWIDE.
(define_mode_attr VWIDE_PRED [(VNx8HF "VNx4BI") (VNx4SF "VNx2BI")])

;; Widened modes of vector modes, lowercase
(define_mode_attr Vwide [(V2SF "v2df") (V4HF "v4sf")
			 (VNx16QI "vnx8hi") (VNx8HI "vnx4si")
			 (VNx4SI  "vnx2di")
			 (VNx8HF  "vnx4sf") (VNx4SF "vnx2df")
			 (VNx16BI "vnx8bi") (VNx8BI "vnx4bi")
			 (VNx4BI  "vnx2bi")])

;; Widened mode register suffixes for VD_BHSI/VQW/VQ_HSF.
(define_mode_attr Vwtype [(V8QI "8h") (V4HI "4s")
			  (V2SI "2d") (V16QI "8h")
			  (V8HI "4s") (V4SI "2d")
			  (V8HF "4s") (V4SF "2d")])

;; Widened scalar register suffixes.
(define_mode_attr Vwstype [(V8QI "h") (V4HI "s")
			  (V2SI "") (V16QI "h")
			  (V8HI "s") (V4SI "d")])
;; Add a .1d for V2SI.
(define_mode_attr Vwsuf [(V8QI "") (V4HI "")
			  (V2SI ".1d") (V16QI "")
			  (V8HI "") (V4SI "")])

;; Scalar mode of widened vector reduction.
(define_mode_attr VWIDE_S [(V8QI "HI") (V4HI "SI")
			  (V2SI "DI") (V16QI "HI")
			  (V8HI "SI") (V4SI "DI")])

;; Widened mode with half the element register suffixes for VD_BHSI/VQW/VQ_HSF.
(define_mode_attr Vwhalf [(V8QI "4h") (V4HI "2s")
			  (V2SI "1d") (V16QI "8h")
			  (V8HI "4s") (V4SI "2d")])

;; SVE vector after narrowing.
(define_mode_attr Ventype [(VNx8HI "b")
			   (VNx4SI "h") (VNx4SF "h")
			   (VNx2DI "s") (VNx2DF "s")])

;; SVE vector after widening.
(define_mode_attr Vewtype [(VNx16QI "h")
			   (VNx8HI  "s") (VNx8HF "s")
			   (VNx4SI  "d") (VNx4SF "d")
			   (VNx2DI  "q")])

;; Widened mode register suffixes for VDW/VQW.
(define_mode_attr Vmwtype [(V8QI ".8h") (V4HI ".4s")
			   (V2SI ".2d") (V16QI ".8h")
			   (V8HI ".4s") (V4SI ".2d")
			   (V4HF ".4s") (V2SF ".2d")
			   (SI   "")    (HI   "")])

;; Lower part register suffixes for VQW/VQ_HSF.
(define_mode_attr Vhalftype [(V16QI "8b") (V8HI "4h")
			     (V4SI "2s") (V8HF "4h")
			     (V4SF "2s")])

;; Define corresponding core/FP element mode for each vector mode.
(define_mode_attr vw [(V8QI "w") (V16QI "w")
		      (V4HI "w") (V8HI "w")
		      (V2SI "w") (V4SI "w")
		      (DI   "x") (V2DI "x")
		      (V2SF "s") (V4SF "s")
		      (V2DF "d")])

;; Corresponding core element mode for each vector mode.  This is a
;; variation on <vw> mapping FP modes to GP regs.
(define_mode_attr vwcore [(V8QI "w") (V16QI "w")
			  (V4HI "w") (V8HI "w")
			  (V2SI "w") (V4SI "w")
			  (DI   "x") (V2DI "x")
			  (V4HF "w") (V8HF "w")
			  (V4BF "w") (V8BF "w")
			  (V2SF "w") (V4SF "w")
			  (V2DF "x")
			  (VNx16QI "w") (VNx8QI "w") (VNx4QI "w") (VNx2QI "w")
			  (VNx8HI "w") (VNx4HI "w") (VNx2HI "w")
			  (VNx8HF "w") (VNx4HF "w") (VNx2HF "w")
			  (VNx8BF "w") (VNx4BF "w") (VNx2BF "w")
			  (VNx4SI "w") (VNx2SI "w")
			  (VNx4SF "w") (VNx2SF "w")
			  (VNx2DI "x")
			  (VNx2DF "x")])

;; Like vwcore, but for the container mode rather than the element mode.
(define_mode_attr vccore [(VNx16QI "w") (VNx8QI "w") (VNx4QI "w") (VNx2QI "x")
			  (VNx8HI "w") (VNx4HI "w") (VNx2HI "x")
			  (VNx4SI "w") (VNx2SI "x")
			  (VNx2DI "x")])

;; Double vector types for ALLX.
(define_mode_attr Vallxd [(QI "8b") (HI "4h") (SI "2s")])

;; Mode with floating-point values replaced by like-sized integers.
(define_mode_attr V_INT_EQUIV [(V8QI "V8QI") (V16QI "V16QI")
			       (V4HI "V4HI") (V8HI  "V8HI")
			       (V2SI "V2SI") (V4SI  "V4SI")
			       (DI   "DI")   (V2DI  "V2DI")
			       (V4HF "V4HI") (V8HF  "V8HI")
			       (V4BF "V4HI") (V8BF  "V8HI")
			       (V2SF "V2SI") (V4SF  "V4SI")
			       (DF   "DI")   (V2DF  "V2DI")
			       (SF   "SI")   (SI    "SI")
			       (HF    "HI")
			       (VNx16QI "VNx16QI")
			       (VNx8HI  "VNx8HI") (VNx8HF "VNx8HI")
			       (VNx8BF  "VNx8HI")
			       (VNx4SI  "VNx4SI") (VNx4SF "VNx4SI")
			       (VNx2DI  "VNx2DI") (VNx2DF "VNx2DI")
])

;; Lower case mode with floating-point values replaced by like-sized integers.
(define_mode_attr v_int_equiv [(V8QI "v8qi") (V16QI "v16qi")
			       (V4HI "v4hi") (V8HI  "v8hi")
			       (V2SI "v2si") (V4SI  "v4si")
			       (DI   "di")   (V2DI  "v2di")
			       (V4HF "v4hi") (V8HF  "v8hi")
			       (V4BF "v4hi") (V8BF  "v8hi")
			       (V2SF "v2si") (V4SF  "v4si")
			       (DF   "di")   (V2DF  "v2di")
			       (SF   "si")
			       (VNx16QI "vnx16qi")
			       (VNx8HI  "vnx8hi") (VNx8HF "vnx8hi")
			       (VNx8BF  "vnx8hi")
			       (VNx4SI  "vnx4si") (VNx4SF "vnx4si")
			       (VNx2DI  "vnx2di") (VNx2DF "vnx2di")
])

;; Floating-point equivalent of selected modes.
(define_mode_attr V_FP_EQUIV [(VNx8HI "VNx8HF") (VNx8HF "VNx8HF")
			      (VNx8BF "VNx8HF")
			      (VNx4SI "VNx4SF") (VNx4SF "VNx4SF")
			      (VNx2DI "VNx2DF") (VNx2DF "VNx2DF")])
(define_mode_attr v_fp_equiv [(VNx8HI "vnx8hf") (VNx8HF "vnx8hf")
			      (VNx8BF "vnx8hf")
			      (VNx4SI "vnx4sf") (VNx4SF "vnx4sf")
			      (VNx2DI "vnx2df") (VNx2DF "vnx2df")])

;; Maps full and partial vector modes of any element type to a full-vector
;; integer mode with the same number of units.
(define_mode_attr V_INT_CONTAINER [(VNx16QI "VNx16QI") (VNx8QI "VNx8HI")
				   (VNx4QI "VNx4SI") (VNx2QI "VNx2DI")
				   (VNx8HI "VNx8HI") (VNx4HI "VNx4SI")
				   (VNx2HI "VNx2DI")
				   (VNx4SI "VNx4SI") (VNx2SI "VNx2DI")
				   (VNx2DI "VNx2DI")
				   (VNx8HF "VNx8HI") (VNx4HF "VNx4SI")
				   (VNx2HF "VNx2DI")
				   (VNx8BF "VNx8HI") (VNx4BF "VNx4SI")
				   (VNx2BF "VNx2DI")
				   (VNx4SF "VNx4SI") (VNx2SF "VNx2DI")
				   (VNx2DF "VNx2DI")])

;; Lower-case version of V_INT_CONTAINER.
(define_mode_attr v_int_container [(VNx16QI "vnx16qi") (VNx8QI "vnx8hi")
				   (VNx4QI "vnx4si") (VNx2QI "vnx2di")
				   (VNx8HI "vnx8hi") (VNx4HI "vnx4si")
				   (VNx2HI "vnx2di")
				   (VNx4SI "vnx4si") (VNx2SI "vnx2di")
				   (VNx2DI "vnx2di")
				   (VNx8HF "vnx8hi") (VNx4HF "vnx4si")
				   (VNx2HF "vnx2di")
				   (VNx8BF "vnx8hi") (VNx4BF "vnx4si")
				   (VNx2BF "vnx2di")
				   (VNx4SF "vnx4si") (VNx2SF "vnx2di")
				   (VNx2DF "vnx2di")])

;; Mode for vector conditional operations where the comparison has
;; different type from the lhs.
(define_mode_attr V_cmp_mixed [(V2SI "V2SF") (V4SI "V4SF")
			       (V2DI "V2DF") (V2SF "V2SI")
			       (V4SF "V4SI") (V2DF "V2DI")])

(define_mode_attr v_cmp_mixed [(V2SI "v2sf") (V4SI "v4sf")
			       (V2DI "v2df") (V2SF "v2si")
			       (V4SF "v4si") (V2DF "v2di")])

;; Lower case element modes (as used in shift immediate patterns).
(define_mode_attr ve_mode [(V8QI "qi") (V16QI "qi")
			   (V4HI "hi") (V8HI  "hi")
			   (V2SI "si") (V4SI  "si")
			   (DI   "di") (V2DI  "di")
			   (QI   "qi") (HI    "hi")
			   (SI   "si")])

;; Like ve_mode but for the half-width modes.
(define_mode_attr vn_mode [(V8HI  "qi") (V4SI  "hi") (V2DI  "si")])

;; Vm for lane instructions is restricted to FP_LO_REGS.
(define_mode_attr vwx [(V4HI "x") (V8HI "x") (HI "x")
		       (V2SI "w") (V4SI "w") (SI "w")])

(define_mode_attr Vendreg [(OI "T") (CI "U") (XI "V")])

;; This is both the number of Q-Registers needed to hold the corresponding
;; opaque large integer mode, and the number of elements touched by the
;; ld..._lane and st..._lane operations.
(define_mode_attr nregs [(OI "2") (CI "3") (XI "4")])

;; Mode for atomic operation suffixes
(define_mode_attr atomic_sfx
  [(QI "b") (HI "h") (SI "") (DI "")])

(define_mode_attr fcvt_target [(V2DF "v2di") (V4SF "v4si") (V2SF "v2si")
			       (V2DI "v2df") (V4SI "v4sf") (V2SI "v2sf")
			       (SF "si") (DF "di") (SI "sf") (DI "df")
			       (V4HF "v4hi") (V8HF "v8hi") (V4HI "v4hf")
			       (V8HI "v8hf") (HF "hi") (HI "hf")])
(define_mode_attr FCVT_TARGET [(V2DF "V2DI") (V4SF "V4SI") (V2SF "V2SI")
			       (V2DI "V2DF") (V4SI "V4SF") (V2SI "V2SF")
			       (SF "SI") (DF "DI") (SI "SF") (DI "DF")
			       (V4HF "V4HI") (V8HF "V8HI") (V4HI "V4HF")
			       (V8HI "V8HF") (HF "HI") (HI "HF")])


;; for the inequal width integer to fp conversions
(define_mode_attr fcvt_iesize [(HF "di") (SF "di") (DF "si")])
(define_mode_attr FCVT_IESIZE [(HF "DI") (SF "DI") (DF "SI")])

(define_mode_attr VSWAP_WIDTH [(V8QI "V16QI") (V16QI "V8QI")
				(V4HI "V8HI") (V8HI  "V4HI")
				(V8BF "V4BF") (V4BF  "V8BF")
				(V2SI "V4SI") (V4SI  "V2SI")
				(DI   "V2DI") (V2DI  "DI")
				(V2SF "V4SF") (V4SF  "V2SF")
				(V4HF "V8HF") (V8HF  "V4HF")
				(DF   "V2DF") (V2DF  "DF")])

(define_mode_attr vswap_width_name [(V8QI "to_128") (V16QI "to_64")
				    (V4HI "to_128") (V8HI  "to_64")
				    (V2SI "to_128") (V4SI  "to_64")
				    (DI   "to_128") (V2DI  "to_64")
				    (V4HF "to_128") (V8HF  "to_64")
				    (V2SF "to_128") (V4SF  "to_64")
				    (V4BF "to_128") (V8BF  "to_64")
				    (DF   "to_128") (V2DF  "to_64")])

;; For certain vector-by-element multiplication instructions we must
;; constrain the 16-bit cases to use only V0-V15.  This is covered by
;; the 'x' constraint.  All other modes may use the 'w' constraint.
(define_mode_attr h_con [(V2SI "w") (V4SI "w")
			 (V4HI "x") (V8HI "x")
			 (V4HF "x") (V8HF "x")
			 (V2SF "w") (V4SF "w")
			 (V2DF "w") (DF "w")])

;; Defined to 'f' for types whose element type is a float type.
(define_mode_attr f [(V8QI "")  (V16QI "")
		     (V4HI "")  (V8HI  "")
		     (V2SI "")  (V4SI  "")
		     (DI   "")  (V2DI  "")
		     (V4HF "f") (V8HF  "f")
		     (V2SF "f") (V4SF  "f")
		     (V2DF "f") (DF    "f")])

;; Defined to '_fp' for types whose element type is a float type.
(define_mode_attr fp [(V8QI "")  (V16QI "")
		      (V4HI "")  (V8HI  "")
		      (V2SI "")  (V4SI  "")
		      (DI   "")  (V2DI  "")
		      (V4HF "_fp") (V8HF  "_fp")
		      (V2SF "_fp") (V4SF  "_fp")
		      (V2DF "_fp") (DF    "_fp")
		      (SF "_fp")])

;; Defined to '_q' for 128-bit types.
(define_mode_attr q [(V8QI "") (V16QI "_q")
		     (V4HI "") (V8HI  "_q")
		     (V4BF "") (V8BF  "_q")
		     (V2SI "") (V4SI  "_q")
		     (DI   "") (V2DI  "_q")
		     (V4HF "") (V8HF "_q")
		     (V4BF "") (V8BF "_q")
		     (V2SF "") (V4SF  "_q")
			       (V2DF  "_q")
		     (QI "") (HI "") (SI "") (DI "") (HF "") (SF "") (DF "")])

(define_mode_attr vp [(V8QI "v") (V16QI "v")
		      (V4HI "v") (V8HI  "v")
		      (V2SI "p") (V4SI  "v")
		      (V2DI "p") (V2DF  "p")
		      (V2SF "p") (V4SF  "v")
		      (V4HF "v") (V8HF  "v")])

(define_mode_attr vsi2qi [(V2SI "v8qi") (V4SI "v16qi")
			  (VNx4SI "vnx16qi") (VNx2DI "vnx8hi")])
(define_mode_attr VSI2QI [(V2SI "V8QI") (V4SI "V16QI")
			  (VNx4SI "VNx16QI") (VNx2DI "VNx8HI")])


;; Register suffix for DOTPROD input types from the return type.
(define_mode_attr Vdottype [(V2SI "8b") (V4SI "16b")])

;; Register suffix for BFDOT input types from the return type.
(define_mode_attr Vbfdottype [(V2SF "4h") (V4SF "8h")])

;; Sum of lengths of instructions needed to move vector registers of a mode.
(define_mode_attr insn_count [(OI "8") (CI "12") (XI "16")])

;; -fpic small model GOT reloc modifers: gotpage_lo15/lo14 for ILP64/32.
;; No need of iterator for -fPIC as it use got_lo12 for both modes.
(define_mode_attr got_modifier [(SI "gotpage_lo14") (DI "gotpage_lo15")])

;; Width of 2nd and 3rd arguments to fp16 vector multiply add/sub
(define_mode_attr VFMLA_W [(V2SF "V4HF") (V4SF "V8HF")])

;; Width of 2nd and 3rd arguments to bf16 vector multiply add/sub
(define_mode_attr VBFMLA_W [(V2SF "V4BF") (V4SF "V8BF")])

(define_mode_attr VFMLA_SEL_W [(V2SF "V2HF") (V4SF "V4HF")])

(define_mode_attr f16quad [(V2SF "") (V4SF "q")])

(define_mode_attr isquadop [(V8QI "") (V16QI "q") (V4BF "") (V8BF "q")])

(define_code_attr f16mac [(plus "a") (minus "s")])

;; Map smax to smin and umax to umin.
(define_code_attr max_opp [(smax "smin") (umax "umin")])

;; Same as above, but louder.
(define_code_attr MAX_OPP [(smax "SMIN") (umax "UMIN")])

;; The number of subvectors in an SVE_STRUCT.
(define_mode_attr vector_count [(VNx32QI "2") (VNx16HI "2")
				(VNx8SI  "2") (VNx4DI  "2")
				(VNx16BF "2")
				(VNx16HF "2") (VNx8SF  "2") (VNx4DF "2")
				(VNx48QI "3") (VNx24HI "3")
				(VNx12SI "3") (VNx6DI  "3")
				(VNx24BF "3")
				(VNx24HF "3") (VNx12SF "3") (VNx6DF "3")
				(VNx64QI "4") (VNx32HI "4")
				(VNx16SI "4") (VNx8DI  "4")
				(VNx32BF "4")
				(VNx32HF "4") (VNx16SF "4") (VNx8DF "4")])

;; The number of instruction bytes needed for an SVE_STRUCT move.  This is
;; equal to vector_count * 4.
(define_mode_attr insn_length [(VNx32QI "8")  (VNx16HI "8")
			       (VNx8SI  "8")  (VNx4DI  "8")
			       (VNx16BF "8")
			       (VNx16HF "8")  (VNx8SF  "8")  (VNx4DF "8")
			       (VNx48QI "12") (VNx24HI "12")
			       (VNx12SI "12") (VNx6DI  "12")
			       (VNx24BF "12")
			       (VNx24HF "12") (VNx12SF "12") (VNx6DF "12")
			       (VNx64QI "16") (VNx32HI "16")
			       (VNx16SI "16") (VNx8DI  "16")
			       (VNx32BF "16")
			       (VNx32HF "16") (VNx16SF "16") (VNx8DF "16")])

;; The type of a subvector in an SVE_STRUCT.
(define_mode_attr VSINGLE [(VNx32QI "VNx16QI")
			   (VNx16HI "VNx8HI") (VNx16HF "VNx8HF")
			   (VNx16BF "VNx8BF")
			   (VNx8SI "VNx4SI") (VNx8SF "VNx4SF")
			   (VNx4DI "VNx2DI") (VNx4DF "VNx2DF")
			   (VNx48QI "VNx16QI")
			   (VNx24HI "VNx8HI") (VNx24HF "VNx8HF")
			   (VNx24BF "VNx8BF")
			   (VNx12SI "VNx4SI") (VNx12SF "VNx4SF")
			   (VNx6DI "VNx2DI") (VNx6DF "VNx2DF")
			   (VNx64QI "VNx16QI")
			   (VNx32HI "VNx8HI") (VNx32HF "VNx8HF")
			   (VNx32BF "VNx8BF")
			   (VNx16SI "VNx4SI") (VNx16SF "VNx4SF")
			   (VNx8DI "VNx2DI") (VNx8DF "VNx2DF")])

;; ...and again in lower case.
(define_mode_attr vsingle [(VNx32QI "vnx16qi")
			   (VNx16HI "vnx8hi") (VNx16HF "vnx8hf")
			   (VNx16BF "vnx8bf")
			   (VNx8SI "vnx4si") (VNx8SF "vnx4sf")
			   (VNx4DI "vnx2di") (VNx4DF "vnx2df")
			   (VNx48QI "vnx16qi")
			   (VNx24HI "vnx8hi") (VNx24HF "vnx8hf")
			   (VNx24BF "vnx8bf")
			   (VNx12SI "vnx4si") (VNx12SF "vnx4sf")
			   (VNx6DI "vnx2di") (VNx6DF "vnx2df")
			   (VNx64QI "vnx16qi")
			   (VNx32HI "vnx8hi") (VNx32HF "vnx8hf")
			   (VNx32BF "vnx8bf")
			   (VNx16SI "vnx4si") (VNx16SF "vnx4sf")
			   (VNx8DI "vnx2di") (VNx8DF "vnx2df")])

;; The predicate mode associated with an SVE data mode.  For structure modes
;; this is equivalent to the <VPRED> of the subvector mode.
(define_mode_attr VPRED [(VNx16QI "VNx16BI") (VNx8QI "VNx8BI")
			 (VNx4QI "VNx4BI") (VNx2QI "VNx2BI")
			 (VNx8HI "VNx8BI") (VNx4HI "VNx4BI") (VNx2HI "VNx2BI")
			 (VNx8HF "VNx8BI") (VNx4HF "VNx4BI") (VNx2HF "VNx2BI")
			 (VNx8BF "VNx8BI") (VNx4BF "VNx4BI") (VNx2BF "VNx2BI")
			 (VNx4SI "VNx4BI") (VNx2SI "VNx2BI")
			 (VNx4SF "VNx4BI") (VNx2SF "VNx2BI")
			 (VNx2DI "VNx2BI")
			 (VNx2DF "VNx2BI")
			 (VNx32QI "VNx16BI")
			 (VNx16HI "VNx8BI") (VNx16HF "VNx8BI")
			 (VNx16BF "VNx8BI")
			 (VNx8SI "VNx4BI") (VNx8SF "VNx4BI")
			 (VNx4DI "VNx2BI") (VNx4DF "VNx2BI")
			 (VNx48QI "VNx16BI")
			 (VNx24HI "VNx8BI") (VNx24HF "VNx8BI")
			 (VNx24BF "VNx8BI")
			 (VNx12SI "VNx4BI") (VNx12SF "VNx4BI")
			 (VNx6DI "VNx2BI") (VNx6DF "VNx2BI")
			 (VNx64QI "VNx16BI")
			 (VNx32HI "VNx8BI") (VNx32HF "VNx8BI")
			 (VNx32BF "VNx8BI")
			 (VNx16SI "VNx4BI") (VNx16SF "VNx4BI")
			 (VNx8DI "VNx2BI") (VNx8DF "VNx2BI")])

;; ...and again in lower case.
(define_mode_attr vpred [(VNx16QI "vnx16bi") (VNx8QI "vnx8bi")
			 (VNx4QI "vnx4bi") (VNx2QI "vnx2bi")
			 (VNx8HI "vnx8bi") (VNx4HI "vnx4bi") (VNx2HI "vnx2bi")
			 (VNx8HF "vnx8bi") (VNx4HF "vnx4bi") (VNx2HF "vnx2bi")
			 (VNx8BF "vnx8bi") (VNx4BF "vnx4bi") (VNx2BF "vnx2bi")
			 (VNx4SI "vnx4bi") (VNx2SI "vnx2bi")
			 (VNx4SF "vnx4bi") (VNx2SF "vnx2bi")
			 (VNx2DI "vnx2bi")
			 (VNx2DF "vnx2bi")
			 (VNx32QI "vnx16bi")
			 (VNx16HI "vnx8bi") (VNx16HF "vnx8bi")
			 (VNx16BF "vnx8bi")
			 (VNx8SI "vnx4bi") (VNx8SF "vnx4bi")
			 (VNx4DI "vnx2bi") (VNx4DF "vnx2bi")
			 (VNx48QI "vnx16bi")
			 (VNx24HI "vnx8bi") (VNx24HF "vnx8bi")
			 (VNx24BF "vnx8bi")
			 (VNx12SI "vnx4bi") (VNx12SF "vnx4bi")
			 (VNx6DI "vnx2bi") (VNx6DF "vnx2bi")
			 (VNx64QI "vnx16bi")
			 (VNx32HI "vnx8bi") (VNx32HF "vnx4bi")
			 (VNx32BF "vnx8bi")
			 (VNx16SI "vnx4bi") (VNx16SF "vnx4bi")
			 (VNx8DI "vnx2bi") (VNx8DF "vnx2bi")])

(define_mode_attr VDOUBLE [(VNx16QI "VNx32QI")
			   (VNx8HI "VNx16HI") (VNx8HF "VNx16HF")
			   (VNx8BF "VNx16BF")
			   (VNx4SI "VNx8SI") (VNx4SF "VNx8SF")
			   (VNx2DI "VNx4DI") (VNx2DF "VNx4DF")])

;; On AArch64 the By element instruction doesn't have a 2S variant.
;; However because the instruction always selects a pair of values
;; The normal 3SAME instruction can be used here instead.
(define_mode_attr FCMLA_maybe_lane [(V2SF "<Vtype>") (V4SF "<Vetype>[%4]")
				    (V4HF "<Vetype>[%4]") (V8HF "<Vetype>[%4]")
				    ])

;; The number of bytes controlled by a predicate
(define_mode_attr data_bytes [(VNx16BI "1") (VNx8BI "2")
			      (VNx4BI "4") (VNx2BI "8")])

;; Two-nybble mask for partial vector modes: nunits, byte size.
(define_mode_attr self_mask [(VNx8QI "0x81")
			     (VNx4QI "0x41")
			     (VNx2QI "0x21")
			     (VNx4HI "0x42")
			     (VNx2HI "0x22")
			     (VNx2SI "0x24")])

;; For SVE_HSDI vector modes, the mask of narrower modes, encoded as above.
(define_mode_attr narrower_mask [(VNx8HI "0x81") (VNx4HI "0x41")
				 (VNx2HI "0x21")
				 (VNx4SI "0x43") (VNx2SI "0x23")
				 (VNx2DI "0x27")])

;; The constraint to use for an SVE [SU]DOT, FMUL, FMLA or FMLS lane index.
(define_mode_attr sve_lane_con [(VNx8HI "y") (VNx4SI "y") (VNx2DI "x")
				(VNx8HF "y") (VNx4SF "y") (VNx2DF "x")])

;; The constraint to use for an SVE FCMLA lane index.
(define_mode_attr sve_lane_pair_con [(VNx8HF "y") (VNx4SF "x")])

;; -------------------------------------------------------------------
;; Code Iterators
;; -------------------------------------------------------------------

;; This code iterator allows the various shifts supported on the core
(define_code_iterator SHIFT [ashift ashiftrt lshiftrt rotatert])

;; This code iterator allows the shifts supported in arithmetic instructions
(define_code_iterator ASHIFT [ashift ashiftrt lshiftrt])

(define_code_iterator SHIFTRT [ashiftrt lshiftrt])

;; Code iterator for logical operations
(define_code_iterator LOGICAL [and ior xor])

;; LOGICAL without AND.
(define_code_iterator LOGICAL_OR [ior xor])

;; Code iterator for logical operations whose :nlogical works on SIMD registers.
(define_code_iterator NLOGICAL [and ior])

;; Code iterator for unary negate and bitwise complement.
(define_code_iterator NEG_NOT [neg not])

;; Code iterator for sign/zero extension
(define_code_iterator ANY_EXTEND [sign_extend zero_extend])
(define_code_iterator ANY_EXTEND2 [sign_extend zero_extend])

;; All division operations (signed/unsigned)
(define_code_iterator ANY_DIV [div udiv])

;; Code iterator for sign/zero extraction
(define_code_iterator ANY_EXTRACT [sign_extract zero_extract])

;; Code iterator for equality comparisons
(define_code_iterator EQL [eq ne])

;; Code iterator for less-than and greater/equal-to
(define_code_iterator LTGE [lt ge])

;; Iterator for __sync_<op> operations that where the operation can be
;; represented directly RTL.  This is all of the sync operations bar
;; nand.
(define_code_iterator atomic_op [plus minus ior xor and])

;; Iterator for integer conversions
(define_code_iterator FIXUORS [fix unsigned_fix])

;; Iterator for float conversions
(define_code_iterator FLOATUORS [float unsigned_float])

;; Code iterator for variants of vector max and min.
(define_code_iterator MAXMIN [smax smin umax umin])

(define_code_iterator FMAXMIN [smax smin])

;; Signed and unsigned max operations.
(define_code_iterator USMAX [smax umax])

;; Code iterator for plus and minus.
(define_code_iterator ADDSUB [plus minus])

;; Code iterator for variants of vector saturating binary ops.
(define_code_iterator BINQOPS [ss_plus us_plus ss_minus us_minus])

;; Code iterator for variants of vector saturating unary ops.
(define_code_iterator UNQOPS [ss_neg ss_abs])

;; Code iterator for signed variants of vector saturating binary ops.
(define_code_iterator SBINQOPS [ss_plus ss_minus])

;; Code iterator for unsigned variants of vector saturating binary ops.
(define_code_iterator UBINQOPS [us_plus us_minus])

;; Modular and saturating addition.
(define_code_iterator ANY_PLUS [plus ss_plus us_plus])

;; Saturating addition.
(define_code_iterator SAT_PLUS [ss_plus us_plus])

;; Modular and saturating subtraction.
(define_code_iterator ANY_MINUS [minus ss_minus us_minus])

;; Saturating subtraction.
(define_code_iterator SAT_MINUS [ss_minus us_minus])

;; Comparison operators for <F>CM.
(define_code_iterator COMPARISONS [lt le eq ge gt])

;; Unsigned comparison operators.
(define_code_iterator UCOMPARISONS [ltu leu geu gtu])

;; Unsigned comparison operators.
(define_code_iterator FAC_COMPARISONS [lt le ge gt])

;; Signed and unsigned saturating truncations.
(define_code_iterator SAT_TRUNC [ss_truncate us_truncate])

;; SVE integer unary operations.
(define_code_iterator SVE_INT_UNARY [abs neg not clrsb clz popcount
				     (ss_abs "TARGET_SVE2")
				     (ss_neg "TARGET_SVE2")])

;; SVE integer binary operations.
(define_code_iterator SVE_INT_BINARY [plus minus mult smax umax smin umin
				      ashift ashiftrt lshiftrt
				      and ior xor
				      (ss_plus "TARGET_SVE2")
				      (us_plus "TARGET_SVE2")
				      (ss_minus "TARGET_SVE2")
				      (us_minus "TARGET_SVE2")])

;; SVE integer binary division operations.
(define_code_iterator SVE_INT_BINARY_SD [div udiv])

;; SVE integer binary operations that have an immediate form.
(define_code_iterator SVE_INT_BINARY_IMM [mult smax smin umax umin])

;; SVE floating-point operations with an unpredicated all-register form.
(define_code_iterator SVE_UNPRED_FP_BINARY [plus minus mult])

;; SVE integer comparisons.
(define_code_iterator SVE_INT_CMP [lt le eq ne ge gt ltu leu geu gtu])

;; -------------------------------------------------------------------
;; Code Attributes
;; -------------------------------------------------------------------
;; Map rtl objects to optab names
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
			 (rotatert "rotr")
			 (sign_extend "extend")
			 (zero_extend "zero_extend")
			 (sign_extract "extv")
			 (zero_extract "extzv")
			 (fix "fix")
			 (unsigned_fix "fixuns")
			 (float "float")
			 (unsigned_float "floatuns")
			 (clrsb "clrsb")
			 (clz "clz")
			 (popcount "popcount")
			 (and "and")
			 (ior "ior")
			 (xor "xor")
			 (not "one_cmpl")
			 (neg "neg")
			 (plus "add")
			 (minus "sub")
			 (mult "mul")
			 (div "div")
			 (udiv "udiv")
			 (ss_plus "ssadd")
			 (us_plus "usadd")
			 (ss_minus "sssub")
			 (us_minus "ussub")
			 (ss_neg "qneg")
			 (ss_abs "qabs")
			 (smin "smin")
			 (smax "smax")
			 (umin "umin")
			 (umax "umax")
			 (eq "eq")
			 (ne "ne")
			 (lt "lt")
			 (ge "ge")
			 (le "le")
			 (gt "gt")
			 (ltu "ltu")
			 (leu "leu")
			 (geu "geu")
			 (gtu "gtu")
			 (abs "abs")])

(define_code_attr addsub [(ss_plus "add")
			  (us_plus "add")
			  (ss_minus "sub")
			  (us_minus "sub")])

;; For comparison operators we use the FCM* and CM* instructions.
;; As there are no CMLE or CMLT instructions which act on 3 vector
;; operands, we must use CMGE or CMGT and swap the order of the
;; source operands.

(define_code_attr n_optab [(lt "gt") (le "ge") (eq "eq") (ge "ge") (gt "gt")
			   (ltu "hi") (leu "hs") (geu "hs") (gtu "hi")])
(define_code_attr cmp_1   [(lt "2") (le "2") (eq "1") (ge "1") (gt "1")
			   (ltu "2") (leu "2") (geu "1") (gtu "1")])
(define_code_attr cmp_2   [(lt "1") (le "1") (eq "2") (ge "2") (gt "2")
			   (ltu "1") (leu "1") (geu "2") (gtu "2")])

(define_code_attr CMP [(lt "LT") (le "LE") (eq "EQ") (ge "GE") (gt "GT")
			(ltu "LTU") (leu "LEU") (ne "NE") (geu "GEU")
			(gtu "GTU")])

;; The AArch64 condition associated with an rtl comparison code.
(define_code_attr cmp_op [(lt "lt")
			  (le "le")
			  (eq "eq")
			  (ne "ne")
			  (ge "ge")
			  (gt "gt")
			  (ltu "lo")
			  (leu "ls")
			  (geu "hs")
			  (gtu "hi")])

(define_code_attr fix_trunc_optab [(fix "fix_trunc")
				   (unsigned_fix "fixuns_trunc")])

;; Optab prefix for sign/zero-extending operations
(define_code_attr su_optab [(sign_extend "") (zero_extend "u")
			    (div "") (udiv "u")
			    (fix "") (unsigned_fix "u")
			    (float "s") (unsigned_float "u")
			    (ss_plus "s") (us_plus "u")
			    (ss_minus "s") (us_minus "u")])

;; Similar for the instruction mnemonics
(define_code_attr shift [(ashift "lsl") (ashiftrt "asr")
			 (lshiftrt "lsr") (rotatert "ror")])

;; Op prefix for shift right and accumulate.
(define_code_attr sra_op [(ashiftrt "s") (lshiftrt "u")])

;; Map shift operators onto underlying bit-field instructions
(define_code_attr bfshift [(ashift "ubfiz") (ashiftrt "sbfx")
			   (lshiftrt "ubfx") (rotatert "extr")])

;; Logical operator instruction mnemonics
(define_code_attr logical [(and "and") (ior "orr") (xor "eor")])

;; Operation names for negate and bitwise complement.
(define_code_attr neg_not_op [(neg "neg") (not "not")])

;; csinv, csneg insn suffixes.
(define_code_attr neg_not_cs [(neg "neg") (not "inv")])

;; Similar, but when the second operand is inverted.
(define_code_attr nlogical [(and "bic") (ior "orn") (xor "eon")])

;; Similar, but when both operands are inverted.
(define_code_attr logical_nn [(and "nor") (ior "nand")])

;; Sign- or zero-extending data-op
(define_code_attr su [(sign_extend "s") (zero_extend "u")
		      (sign_extract "s") (zero_extract "u")
		      (fix "s") (unsigned_fix "u")
		      (div "s") (udiv "u")
		      (smax "s") (umax "u")
		      (smin "s") (umin "u")
		      (ss_truncate "s") (us_truncate "u")])

;; "s" for signed ops, empty for unsigned ones.
(define_code_attr s [(sign_extend "s") (zero_extend "")])

;; Map signed/unsigned ops to the corresponding extension.
(define_code_attr paired_extend [(ss_plus "sign_extend")
				 (us_plus "zero_extend")
				 (ss_minus "sign_extend")
				 (us_minus "zero_extend")])

;; Whether a shift is left or right.
(define_code_attr lr [(ashift "l") (ashiftrt "r") (lshiftrt "r")])

;; Emit conditional branch instructions.
(define_code_attr bcond [(eq "beq") (ne "bne") (lt "bne") (ge "beq")])

;; Emit cbz/cbnz depending on comparison type.
(define_code_attr cbz [(eq "cbz") (ne "cbnz") (lt "cbnz") (ge "cbz")])

;; Emit inverted cbz/cbnz depending on comparison type.
(define_code_attr inv_cb [(eq "cbnz") (ne "cbz") (lt "cbz") (ge "cbnz")])

;; Emit tbz/tbnz depending on comparison type.
(define_code_attr tbz [(eq "tbz") (ne "tbnz") (lt "tbnz") (ge "tbz")])

;; Emit inverted tbz/tbnz depending on comparison type.
(define_code_attr inv_tb [(eq "tbnz") (ne "tbz") (lt "tbz") (ge "tbnz")])

;; Max/min attributes.
(define_code_attr maxmin [(smax "max")
			  (smin "min")
			  (umax "max")
			  (umin "min")])

;; MLA/MLS attributes.
(define_code_attr as [(ss_plus "a") (ss_minus "s")])

;; Atomic operations
(define_code_attr atomic_optab
  [(ior "or") (xor "xor") (and "and") (plus "add") (minus "sub")])

(define_code_attr atomic_op_operand
  [(ior "aarch64_logical_operand")
   (xor "aarch64_logical_operand")
   (and "aarch64_logical_operand")
   (plus "aarch64_plus_operand")
   (minus "aarch64_plus_operand")])

;; Constants acceptable for atomic operations.
;; This definition must appear in this file before the iterators it refers to.
(define_code_attr const_atomic
 [(plus "IJ") (minus "IJ")
  (xor "<lconst_atomic>") (ior "<lconst_atomic>")
  (and "<lconst_atomic>")])

;; Attribute to describe constants acceptable in atomic logical operations
(define_mode_attr lconst_atomic [(QI "K") (HI "K") (SI "K") (DI "L")])

;; The integer SVE instruction that implements an rtx code.
(define_code_attr sve_int_op [(plus "add")
			      (minus "sub")
			      (mult "mul")
			      (div "sdiv")
			      (udiv "udiv")
			      (abs "abs")
			      (neg "neg")
			      (smin "smin")
			      (smax "smax")
			      (umin "umin")
			      (umax "umax")
			      (ashift "lsl")
			      (ashiftrt "asr")
			      (lshiftrt "lsr")
			      (and "and")
			      (ior "orr")
			      (xor "eor")
			      (not "not")
			      (clrsb "cls")
			      (clz "clz")
			      (popcount "cnt")
			      (ss_plus "sqadd")
			      (us_plus "uqadd")
			      (ss_minus "sqsub")
			      (us_minus "uqsub")
			      (ss_neg "sqneg")
			      (ss_abs "sqabs")])

(define_code_attr sve_int_op_rev [(plus "add")
				  (minus "subr")
				  (mult "mul")
				  (div "sdivr")
				  (udiv "udivr")
				  (smin "smin")
				  (smax "smax")
				  (umin "umin")
				  (umax "umax")
				  (ashift "lslr")
				  (ashiftrt "asrr")
				  (lshiftrt "lsrr")
				  (and "and")
				  (ior "orr")
				  (xor "eor")
				  (ss_plus "sqadd")
				  (us_plus "uqadd")
				  (ss_minus "sqsubr")
				  (us_minus "uqsubr")])

;; The floating-point SVE instruction that implements an rtx code.
(define_code_attr sve_fp_op [(plus "fadd")
			     (minus "fsub")
			     (mult "fmul")])

;; The SVE immediate constraint to use for an rtl code.
(define_code_attr sve_imm_con [(mult "vsm")
			       (smax "vsm")
			       (smin "vsm")
			       (umax "vsb")
			       (umin "vsb")
			       (eq "vsc")
			       (ne "vsc")
			       (lt "vsc")
			       (ge "vsc")
			       (le "vsc")
			       (gt "vsc")
			       (ltu "vsd")
			       (leu "vsd")
			       (geu "vsd")
			       (gtu "vsd")])

;; The prefix letter to use when printing an immediate operand.
(define_code_attr sve_imm_prefix [(mult "")
				  (smax "")
				  (smin "")
				  (umax "D")
				  (umin "D")])

;; The predicate to use for the second input operand in a cond_<optab><mode>
;; pattern.
(define_code_attr sve_pred_int_rhs2_operand
  [(plus "register_operand")
   (minus "register_operand")
   (mult "register_operand")
   (smax "register_operand")
   (umax "register_operand")
   (smin "register_operand")
   (umin "register_operand")
   (ashift "aarch64_sve_lshift_operand")
   (ashiftrt "aarch64_sve_rshift_operand")
   (lshiftrt "aarch64_sve_rshift_operand")
   (and "aarch64_sve_pred_and_operand")
   (ior "register_operand")
   (xor "register_operand")
   (ss_plus "register_operand")
   (us_plus "register_operand")
   (ss_minus "register_operand")
   (us_minus "register_operand")])

(define_code_attr inc_dec [(minus "dec") (ss_minus "sqdec") (us_minus "uqdec")
			   (plus "inc") (ss_plus "sqinc") (us_plus "uqinc")])

;; -------------------------------------------------------------------
;; Int Iterators.
;; -------------------------------------------------------------------

;; The unspec codes for the SABAL, UABAL AdvancedSIMD instructions.
(define_int_iterator ABAL [UNSPEC_SABAL UNSPEC_UABAL])

;; The unspec codes for the SABDL, UABDL AdvancedSIMD instructions.
(define_int_iterator ABDL [UNSPEC_SABDL UNSPEC_UABDL])

;; The unspec codes for the SABAL2, UABAL2 AdvancedSIMD instructions.
(define_int_iterator ABAL2 [UNSPEC_SABAL2 UNSPEC_UABAL2])

;; The unspec codes for the SABDL2, UABDL2 AdvancedSIMD instructions.
(define_int_iterator ABDL2 [UNSPEC_SABDL2 UNSPEC_UABDL2])

;; The unspec codes for the SADALP, UADALP AdvancedSIMD instructions.
(define_int_iterator ADALP [UNSPEC_SADALP UNSPEC_UADALP])

(define_int_iterator MAXMINV [UNSPEC_UMAXV UNSPEC_UMINV
			      UNSPEC_SMAXV UNSPEC_SMINV])

(define_int_iterator FMAXMINV [UNSPEC_FMAXV UNSPEC_FMINV
			       UNSPEC_FMAXNMV UNSPEC_FMINNMV])

(define_int_iterator SVE_INT_ADDV [UNSPEC_SADDV UNSPEC_UADDV])

(define_int_iterator USADDLP [UNSPEC_SADDLP UNSPEC_UADDLP])

(define_int_iterator USADDLV [UNSPEC_SADDLV UNSPEC_UADDLV])

(define_int_iterator LOGICALF [UNSPEC_ANDF UNSPEC_IORF UNSPEC_XORF])

(define_int_iterator HADDSUB [UNSPEC_SHADD UNSPEC_UHADD
			      UNSPEC_SRHADD UNSPEC_URHADD
			      UNSPEC_SHSUB UNSPEC_UHSUB])

(define_int_iterator HADD [UNSPEC_SHADD UNSPEC_UHADD])

(define_int_iterator RHADD [UNSPEC_SRHADD UNSPEC_URHADD])

(define_int_iterator BSL_DUP [1 2])

(define_int_iterator DOTPROD [UNSPEC_SDOT UNSPEC_UDOT])

(define_int_iterator DOTPROD_I8MM [UNSPEC_USDOT UNSPEC_SUDOT])
(define_int_iterator DOTPROD_US_ONLY [UNSPEC_USDOT])

(define_int_iterator ADDSUBHN [UNSPEC_ADDHN UNSPEC_RADDHN
			       UNSPEC_SUBHN UNSPEC_RSUBHN])

(define_int_iterator ADDSUBHN2 [UNSPEC_ADDHN2 UNSPEC_RADDHN2
			        UNSPEC_SUBHN2 UNSPEC_RSUBHN2])

(define_int_iterator FMAXMIN_UNS [UNSPEC_FMAX UNSPEC_FMIN
				  UNSPEC_FMAXNM UNSPEC_FMINNM])

(define_int_iterator PAUTH_LR_SP [UNSPEC_PACIASP UNSPEC_AUTIASP
				  UNSPEC_PACIBSP UNSPEC_AUTIBSP])

(define_int_iterator PAUTH_17_16 [UNSPEC_PACIA1716 UNSPEC_AUTIA1716
				  UNSPEC_PACIB1716 UNSPEC_AUTIB1716])

(define_int_iterator VQDMULH [UNSPEC_SQDMULH UNSPEC_SQRDMULH])

(define_int_iterator MULHRS [UNSPEC_SMULHS UNSPEC_UMULHS
                             UNSPEC_SMULHRS UNSPEC_UMULHRS])

(define_int_iterator USSUQADD [UNSPEC_SUQADD UNSPEC_USQADD])

(define_int_iterator VSHL [UNSPEC_SSHL UNSPEC_USHL
		           UNSPEC_SRSHL UNSPEC_URSHL])

(define_int_iterator VSHLL [UNSPEC_SSHLL UNSPEC_USHLL])

(define_int_iterator VQSHL [UNSPEC_SQSHL UNSPEC_UQSHL
                            UNSPEC_SQRSHL UNSPEC_UQRSHL])

(define_int_iterator VSRA [UNSPEC_SSRA UNSPEC_USRA
			     UNSPEC_SRSRA UNSPEC_URSRA])

(define_int_iterator VSLRI [UNSPEC_SSLI UNSPEC_USLI
			      UNSPEC_SSRI UNSPEC_USRI])


(define_int_iterator VRSHR_N [UNSPEC_SRSHR UNSPEC_URSHR])

(define_int_iterator VQSHL_N [UNSPEC_SQSHLU UNSPEC_SQSHL UNSPEC_UQSHL])

(define_int_iterator VQSHRN_N [UNSPEC_SQSHRUN UNSPEC_SQRSHRUN
                               UNSPEC_SQSHRN UNSPEC_UQSHRN
                               UNSPEC_SQRSHRN UNSPEC_UQRSHRN])

(define_int_iterator SQRDMLH_AS [UNSPEC_SQRDMLAH UNSPEC_SQRDMLSH])

(define_int_iterator PERMUTE [UNSPEC_ZIP1 UNSPEC_ZIP2
			      UNSPEC_TRN1 UNSPEC_TRN2
			      UNSPEC_UZP1 UNSPEC_UZP2])

(define_int_iterator PERMUTEQ [UNSPEC_ZIP1Q UNSPEC_ZIP2Q
			       UNSPEC_TRN1Q UNSPEC_TRN2Q
			       UNSPEC_UZP1Q UNSPEC_UZP2Q])

(define_int_iterator OPTAB_PERMUTE [UNSPEC_ZIP1 UNSPEC_ZIP2
				    UNSPEC_UZP1 UNSPEC_UZP2])

(define_int_iterator REVERSE [UNSPEC_REV64 UNSPEC_REV32 UNSPEC_REV16])

(define_int_iterator FRINT [UNSPEC_FRINTZ UNSPEC_FRINTP UNSPEC_FRINTM
			     UNSPEC_FRINTN UNSPEC_FRINTI UNSPEC_FRINTX
			     UNSPEC_FRINTA])

(define_int_iterator FCVT [UNSPEC_FRINTZ UNSPEC_FRINTP UNSPEC_FRINTM
			    UNSPEC_FRINTA UNSPEC_FRINTN])

(define_int_iterator FCVT_F2FIXED [UNSPEC_FCVTZS UNSPEC_FCVTZU])
(define_int_iterator FCVT_FIXED2F [UNSPEC_SCVTF UNSPEC_UCVTF])

(define_int_iterator CRC [UNSPEC_CRC32B UNSPEC_CRC32H UNSPEC_CRC32W
                          UNSPEC_CRC32X UNSPEC_CRC32CB UNSPEC_CRC32CH
                          UNSPEC_CRC32CW UNSPEC_CRC32CX])

(define_int_iterator CRYPTO_AES [UNSPEC_AESE UNSPEC_AESD])
(define_int_iterator CRYPTO_AESMC [UNSPEC_AESMC UNSPEC_AESIMC])

(define_int_iterator CRYPTO_SHA1 [UNSPEC_SHA1C UNSPEC_SHA1M UNSPEC_SHA1P])

(define_int_iterator CRYPTO_SHA256 [UNSPEC_SHA256H UNSPEC_SHA256H2])

(define_int_iterator CRYPTO_SHA512 [UNSPEC_SHA512H UNSPEC_SHA512H2])

(define_int_iterator CRYPTO_SM3TT [UNSPEC_SM3TT1A UNSPEC_SM3TT1B
				   UNSPEC_SM3TT2A UNSPEC_SM3TT2B])

(define_int_iterator CRYPTO_SM3PART [UNSPEC_SM3PARTW1 UNSPEC_SM3PARTW2])

;; Iterators for fp16 operations

(define_int_iterator VFMLA16_LOW [UNSPEC_FMLAL UNSPEC_FMLSL])

(define_int_iterator VFMLA16_HIGH [UNSPEC_FMLAL2 UNSPEC_FMLSL2])

(define_int_iterator UNPACK [UNSPEC_UNPACKSHI UNSPEC_UNPACKUHI
			     UNSPEC_UNPACKSLO UNSPEC_UNPACKULO])

(define_int_iterator UNPACK_UNSIGNED [UNSPEC_UNPACKULO UNSPEC_UNPACKUHI])

(define_int_iterator MUL_HIGHPART [UNSPEC_SMUL_HIGHPART UNSPEC_UMUL_HIGHPART])

(define_int_iterator CLAST [UNSPEC_CLASTA UNSPEC_CLASTB])

(define_int_iterator LAST [UNSPEC_LASTA UNSPEC_LASTB])

(define_int_iterator SVE_INT_UNARY [UNSPEC_RBIT UNSPEC_REVB
				    UNSPEC_REVH UNSPEC_REVW])

(define_int_iterator SVE_FP_UNARY [UNSPEC_FRECPE UNSPEC_RSQRTE])

(define_int_iterator SVE_FP_UNARY_INT [UNSPEC_FEXPA])

(define_int_iterator SVE_INT_SHIFT_IMM [UNSPEC_ASRD
					(UNSPEC_SQSHLU "TARGET_SVE2")
					(UNSPEC_SRSHR "TARGET_SVE2")
					(UNSPEC_URSHR "TARGET_SVE2")])

(define_int_iterator SVE_FP_BINARY [UNSPEC_FRECPS UNSPEC_RSQRTS])

(define_int_iterator SVE_FP_BINARY_INT [UNSPEC_FTSMUL UNSPEC_FTSSEL])

(define_int_iterator SVE_BFLOAT_TERNARY_LONG [UNSPEC_BFDOT
					      UNSPEC_BFMLALB
					      UNSPEC_BFMLALT
					      UNSPEC_BFMMLA])

(define_int_iterator SVE_BFLOAT_TERNARY_LONG_LANE [UNSPEC_BFDOT
						   UNSPEC_BFMLALB
						   UNSPEC_BFMLALT])

(define_int_iterator SVE_INT_REDUCTION [UNSPEC_ANDV
					UNSPEC_IORV
					UNSPEC_SMAXV
					UNSPEC_SMINV
					UNSPEC_UMAXV
					UNSPEC_UMINV
					UNSPEC_XORV])

(define_int_iterator SVE_FP_REDUCTION [UNSPEC_FADDV
				       UNSPEC_FMAXV
				       UNSPEC_FMAXNMV
				       UNSPEC_FMINV
				       UNSPEC_FMINNMV])

(define_int_iterator SVE_COND_FP_UNARY [UNSPEC_COND_FABS
					UNSPEC_COND_FNEG
					UNSPEC_COND_FRECPX
					UNSPEC_COND_FRINTA
					UNSPEC_COND_FRINTI
					UNSPEC_COND_FRINTM
					UNSPEC_COND_FRINTN
					UNSPEC_COND_FRINTP
					UNSPEC_COND_FRINTX
					UNSPEC_COND_FRINTZ
					UNSPEC_COND_FSQRT])

;; Same as SVE_COND_FP_UNARY, but without codes that have a dedicated
;; <optab><mode>2 expander.
(define_int_iterator SVE_COND_FP_UNARY_OPTAB [UNSPEC_COND_FABS
					      UNSPEC_COND_FNEG
					      UNSPEC_COND_FRECPX
					      UNSPEC_COND_FRINTA
					      UNSPEC_COND_FRINTI
					      UNSPEC_COND_FRINTM
					      UNSPEC_COND_FRINTN
					      UNSPEC_COND_FRINTP
					      UNSPEC_COND_FRINTX
					      UNSPEC_COND_FRINTZ])

(define_int_iterator SVE_COND_FCVT [UNSPEC_COND_FCVT])
(define_int_iterator SVE_COND_FCVTI [UNSPEC_COND_FCVTZS UNSPEC_COND_FCVTZU])
(define_int_iterator SVE_COND_ICVTF [UNSPEC_COND_SCVTF UNSPEC_COND_UCVTF])

(define_int_iterator SVE_COND_FP_BINARY [UNSPEC_COND_FADD
					 UNSPEC_COND_FDIV
					 UNSPEC_COND_FMAX
					 UNSPEC_COND_FMAXNM
					 UNSPEC_COND_FMIN
					 UNSPEC_COND_FMINNM
					 UNSPEC_COND_FMUL
					 UNSPEC_COND_FMULX
					 UNSPEC_COND_FSUB])

;; Same as SVE_COND_FP_BINARY, but without codes that have a dedicated
;; <optab><mode>3 expander.
(define_int_iterator SVE_COND_FP_BINARY_OPTAB [UNSPEC_COND_FADD
					       UNSPEC_COND_FMAX
					       UNSPEC_COND_FMAXNM
					       UNSPEC_COND_FMIN
					       UNSPEC_COND_FMINNM
					       UNSPEC_COND_FMUL
					       UNSPEC_COND_FMULX
					       UNSPEC_COND_FSUB])

(define_int_iterator SVE_COND_FP_BINARY_INT [UNSPEC_COND_FSCALE])

(define_int_iterator SVE_COND_FP_ADD [UNSPEC_COND_FADD])
(define_int_iterator SVE_COND_FP_SUB [UNSPEC_COND_FSUB])
(define_int_iterator SVE_COND_FP_MUL [UNSPEC_COND_FMUL])

(define_int_iterator SVE_COND_FP_BINARY_I1 [UNSPEC_COND_FMAX
					    UNSPEC_COND_FMAXNM
					    UNSPEC_COND_FMIN
					    UNSPEC_COND_FMINNM
					    UNSPEC_COND_FMUL])

(define_int_iterator SVE_COND_FP_BINARY_REG [UNSPEC_COND_FDIV
					     UNSPEC_COND_FMULX])

(define_int_iterator SVE_COND_FCADD [UNSPEC_COND_FCADD90
				     UNSPEC_COND_FCADD270])

(define_int_iterator SVE_COND_FP_MAXMIN [UNSPEC_COND_FMAX
					 UNSPEC_COND_FMAXNM
					 UNSPEC_COND_FMIN
					 UNSPEC_COND_FMINNM])

;; Floating-point max/min operations that correspond to optabs,
;; as opposed to those that are internal to the port.
(define_int_iterator SVE_COND_FP_MAXMIN_PUBLIC [UNSPEC_COND_FMAXNM
						UNSPEC_COND_FMINNM])

(define_int_iterator SVE_COND_FP_TERNARY [UNSPEC_COND_FMLA
					  UNSPEC_COND_FMLS
					  UNSPEC_COND_FNMLA
					  UNSPEC_COND_FNMLS])

(define_int_iterator SVE_COND_FCMLA [UNSPEC_COND_FCMLA
				     UNSPEC_COND_FCMLA90
				     UNSPEC_COND_FCMLA180
				     UNSPEC_COND_FCMLA270])

(define_int_iterator SVE_COND_INT_CMP_WIDE [UNSPEC_COND_CMPEQ_WIDE
					    UNSPEC_COND_CMPGE_WIDE
					    UNSPEC_COND_CMPGT_WIDE
					    UNSPEC_COND_CMPHI_WIDE
					    UNSPEC_COND_CMPHS_WIDE
					    UNSPEC_COND_CMPLE_WIDE
					    UNSPEC_COND_CMPLO_WIDE
					    UNSPEC_COND_CMPLS_WIDE
					    UNSPEC_COND_CMPLT_WIDE
					    UNSPEC_COND_CMPNE_WIDE])

;; SVE FP comparisons that accept #0.0.
(define_int_iterator SVE_COND_FP_CMP_I0 [UNSPEC_COND_FCMEQ
					 UNSPEC_COND_FCMGE
					 UNSPEC_COND_FCMGT
					 UNSPEC_COND_FCMLE
					 UNSPEC_COND_FCMLT
					 UNSPEC_COND_FCMNE])

(define_int_iterator SVE_COND_FP_ABS_CMP [UNSPEC_COND_FCMGE
					  UNSPEC_COND_FCMGT
					  UNSPEC_COND_FCMLE
					  UNSPEC_COND_FCMLT])

(define_int_iterator SVE_FP_TERNARY_LANE [UNSPEC_FMLA UNSPEC_FMLS])

(define_int_iterator SVE_CFP_TERNARY_LANE [UNSPEC_FCMLA UNSPEC_FCMLA90
					   UNSPEC_FCMLA180 UNSPEC_FCMLA270])

(define_int_iterator SVE_WHILE [UNSPEC_WHILELE UNSPEC_WHILELO
				UNSPEC_WHILELS UNSPEC_WHILELT
				(UNSPEC_WHILEGE "TARGET_SVE2")
				(UNSPEC_WHILEGT "TARGET_SVE2")
				(UNSPEC_WHILEHI "TARGET_SVE2")
				(UNSPEC_WHILEHS "TARGET_SVE2")
				(UNSPEC_WHILERW "TARGET_SVE2")
				(UNSPEC_WHILEWR "TARGET_SVE2")])

(define_int_iterator SVE2_WHILE_PTR [UNSPEC_WHILERW UNSPEC_WHILEWR])

(define_int_iterator SVE_SHIFT_WIDE [UNSPEC_ASHIFT_WIDE
				     UNSPEC_ASHIFTRT_WIDE
				     UNSPEC_LSHIFTRT_WIDE])

(define_int_iterator SVE_LDFF1_LDNF1 [UNSPEC_LDFF1 UNSPEC_LDNF1])

(define_int_iterator SVE2_U32_UNARY [UNSPEC_URECPE UNSPEC_RSQRTE])

(define_int_iterator SVE2_INT_UNARY_NARROWB [UNSPEC_SQXTNB
					     UNSPEC_SQXTUNB
					     UNSPEC_UQXTNB])

(define_int_iterator SVE2_INT_UNARY_NARROWT [UNSPEC_SQXTNT
					     UNSPEC_SQXTUNT
					     UNSPEC_UQXTNT])

(define_int_iterator SVE2_INT_BINARY [UNSPEC_SQDMULH
				      UNSPEC_SQRDMULH])

(define_int_iterator SVE2_INT_BINARY_LANE [UNSPEC_SQDMULH
					   UNSPEC_SQRDMULH])

(define_int_iterator SVE2_INT_BINARY_LONG [UNSPEC_SABDLB
					   UNSPEC_SABDLT
					   UNSPEC_SADDLB
					   UNSPEC_SADDLBT
					   UNSPEC_SADDLT
					   UNSPEC_SMULLB
					   UNSPEC_SMULLT
					   UNSPEC_SQDMULLB
					   UNSPEC_SQDMULLT
					   UNSPEC_SSUBLB
					   UNSPEC_SSUBLBT
					   UNSPEC_SSUBLT
					   UNSPEC_SSUBLTB
					   UNSPEC_UABDLB
					   UNSPEC_UABDLT
					   UNSPEC_UADDLB
					   UNSPEC_UADDLT
					   UNSPEC_UMULLB
					   UNSPEC_UMULLT
					   UNSPEC_USUBLB
					   UNSPEC_USUBLT])

(define_int_iterator SVE2_INT_BINARY_LONG_LANE [UNSPEC_SMULLB
						UNSPEC_SMULLT
						UNSPEC_SQDMULLB
						UNSPEC_SQDMULLT
						UNSPEC_UMULLB
						UNSPEC_UMULLT])

(define_int_iterator SVE2_INT_BINARY_NARROWB [UNSPEC_ADDHNB
					      UNSPEC_RADDHNB
					      UNSPEC_RSUBHNB
					      UNSPEC_SUBHNB])

(define_int_iterator SVE2_INT_BINARY_NARROWT [UNSPEC_ADDHNT
					      UNSPEC_RADDHNT
					      UNSPEC_RSUBHNT
					      UNSPEC_SUBHNT])

(define_int_iterator SVE2_INT_BINARY_PAIR [UNSPEC_ADDP
					   UNSPEC_SMAXP
					   UNSPEC_SMINP
					   UNSPEC_UMAXP
					   UNSPEC_UMINP])

(define_int_iterator SVE2_FP_BINARY_PAIR [UNSPEC_FADDP
					  UNSPEC_FMAXP
					  UNSPEC_FMAXNMP
					  UNSPEC_FMINP
					  UNSPEC_FMINNMP])

(define_int_iterator SVE2_INT_BINARY_PAIR_LONG [UNSPEC_SADALP UNSPEC_UADALP])

(define_int_iterator SVE2_INT_BINARY_WIDE [UNSPEC_SADDWB
					   UNSPEC_SADDWT
					   UNSPEC_SSUBWB
					   UNSPEC_SSUBWT
					   UNSPEC_UADDWB
					   UNSPEC_UADDWT
					   UNSPEC_USUBWB
					   UNSPEC_USUBWT])

(define_int_iterator SVE2_INT_SHIFT_IMM_LONG [UNSPEC_SSHLLB
					      UNSPEC_SSHLLT
					      UNSPEC_USHLLB
					      UNSPEC_USHLLT])

(define_int_iterator SVE2_INT_SHIFT_IMM_NARROWB [UNSPEC_RSHRNB
						 UNSPEC_SHRNB
						 UNSPEC_SQRSHRNB
						 UNSPEC_SQRSHRUNB
						 UNSPEC_SQSHRNB
						 UNSPEC_SQSHRUNB
						 UNSPEC_UQRSHRNB
						 UNSPEC_UQSHRNB])

(define_int_iterator SVE2_INT_SHIFT_IMM_NARROWT [UNSPEC_RSHRNT
						 UNSPEC_SHRNT
						 UNSPEC_SQRSHRNT
						 UNSPEC_SQRSHRUNT
						 UNSPEC_SQSHRNT
						 UNSPEC_SQSHRUNT
						 UNSPEC_UQRSHRNT
						 UNSPEC_UQSHRNT])

(define_int_iterator SVE2_INT_SHIFT_INSERT [UNSPEC_SLI UNSPEC_SRI])

(define_int_iterator SVE2_INT_CADD [UNSPEC_CADD90
				    UNSPEC_CADD270
				    UNSPEC_SQCADD90
				    UNSPEC_SQCADD270])

(define_int_iterator SVE2_INT_BITPERM [UNSPEC_BDEP UNSPEC_BEXT UNSPEC_BGRP])

(define_int_iterator SVE2_INT_TERNARY [UNSPEC_ADCLB
				       UNSPEC_ADCLT
				       UNSPEC_EORBT
				       UNSPEC_EORTB
				       UNSPEC_SBCLB
				       UNSPEC_SBCLT
				       UNSPEC_SQRDMLAH
				       UNSPEC_SQRDMLSH])

(define_int_iterator SVE2_INT_TERNARY_LANE [UNSPEC_SQRDMLAH
					    UNSPEC_SQRDMLSH])

(define_int_iterator SVE2_FP_TERNARY_LONG [UNSPEC_FMLALB
					   UNSPEC_FMLALT
					   UNSPEC_FMLSLB
					   UNSPEC_FMLSLT])

(define_int_iterator SVE2_FP_TERNARY_LONG_LANE [UNSPEC_FMLALB
						UNSPEC_FMLALT
						UNSPEC_FMLSLB
						UNSPEC_FMLSLT])

(define_int_iterator SVE2_INT_CMLA [UNSPEC_CMLA
				    UNSPEC_CMLA90
				    UNSPEC_CMLA180
				    UNSPEC_CMLA270
				    UNSPEC_SQRDCMLAH
				    UNSPEC_SQRDCMLAH90
				    UNSPEC_SQRDCMLAH180
				    UNSPEC_SQRDCMLAH270])

;; Unlike the normal CMLA instructions these represent the actual operation
;; to be performed.  They will always need to be expanded into multiple
;; sequences consisting of CMLA.
(define_int_iterator SVE2_INT_CMLA_OP [UNSPEC_CMLA
				       UNSPEC_CMLA_CONJ
				       UNSPEC_CMLA180
				       UNSPEC_CMLA180_CONJ])

;; Unlike the normal CMLA instructions these represent the actual operation
;; to be performed.  They will always need to be expanded into multiple
;; sequences consisting of CMLA.
(define_int_iterator SVE2_INT_CMUL_OP [UNSPEC_CMUL
				       UNSPEC_CMUL_CONJ])

;; Same as SVE2_INT_CADD but exclude the saturating instructions
(define_int_iterator SVE2_INT_CADD_OP [UNSPEC_CADD90
				       UNSPEC_CADD270])

(define_int_iterator SVE2_INT_CDOT [UNSPEC_CDOT
				    UNSPEC_CDOT90
				    UNSPEC_CDOT180
				    UNSPEC_CDOT270])

(define_int_iterator SVE2_INT_ADD_BINARY_LONG [UNSPEC_SABDLB
					       UNSPEC_SABDLT
					       UNSPEC_SMULLB
					       UNSPEC_SMULLT
					       UNSPEC_UABDLB
					       UNSPEC_UABDLT
					       UNSPEC_UMULLB
					       UNSPEC_UMULLT])

(define_int_iterator SVE2_INT_QADD_BINARY_LONG [UNSPEC_SQDMULLB
					        UNSPEC_SQDMULLBT
					        UNSPEC_SQDMULLT])

(define_int_iterator SVE2_INT_SUB_BINARY_LONG [UNSPEC_SMULLB
					       UNSPEC_SMULLT
					       UNSPEC_UMULLB
					       UNSPEC_UMULLT])

(define_int_iterator SVE2_INT_QSUB_BINARY_LONG [UNSPEC_SQDMULLB
					        UNSPEC_SQDMULLBT
					        UNSPEC_SQDMULLT])

(define_int_iterator SVE2_INT_ADD_BINARY_LONG_LANE [UNSPEC_SMULLB
						    UNSPEC_SMULLT
						    UNSPEC_UMULLB
						    UNSPEC_UMULLT])

(define_int_iterator SVE2_INT_QADD_BINARY_LONG_LANE [UNSPEC_SQDMULLB
						     UNSPEC_SQDMULLT])

(define_int_iterator SVE2_INT_SUB_BINARY_LONG_LANE [UNSPEC_SMULLB
						    UNSPEC_SMULLT
						    UNSPEC_UMULLB
						    UNSPEC_UMULLT])

(define_int_iterator SVE2_INT_QSUB_BINARY_LONG_LANE [UNSPEC_SQDMULLB
						     UNSPEC_SQDMULLT])

(define_int_iterator SVE2_COND_INT_UNARY_FP [UNSPEC_COND_FLOGB])

(define_int_iterator SVE2_COND_FP_UNARY_LONG [UNSPEC_COND_FCVTLT])

(define_int_iterator SVE2_COND_FP_UNARY_NARROWB [UNSPEC_COND_FCVTX])

(define_int_iterator SVE2_COND_INT_BINARY [UNSPEC_SHADD
					   UNSPEC_SHSUB
					   UNSPEC_SQRSHL
					   UNSPEC_SRHADD
					   UNSPEC_SRSHL
					   UNSPEC_SUQADD
					   UNSPEC_UHADD
					   UNSPEC_UHSUB
					   UNSPEC_UQRSHL
					   UNSPEC_URHADD
					   UNSPEC_URSHL
					   UNSPEC_USQADD])

(define_int_iterator SVE2_COND_INT_BINARY_NOREV [UNSPEC_SUQADD
						 UNSPEC_USQADD])

(define_int_iterator SVE2_COND_INT_BINARY_REV [UNSPEC_SHADD
					       UNSPEC_SHSUB
					       UNSPEC_SQRSHL
					       UNSPEC_SRHADD
					       UNSPEC_SRSHL
					       UNSPEC_UHADD
					       UNSPEC_UHSUB
					       UNSPEC_UQRSHL
					       UNSPEC_URHADD
					       UNSPEC_URSHL])

(define_int_iterator SVE2_COND_INT_SHIFT [UNSPEC_SQSHL
					  UNSPEC_UQSHL])

(define_int_iterator SVE2_MATCH [UNSPEC_MATCH UNSPEC_NMATCH])

(define_int_iterator SVE2_PMULL [UNSPEC_PMULLB UNSPEC_PMULLT])

(define_int_iterator SVE2_PMULL_PAIR [UNSPEC_PMULLB_PAIR UNSPEC_PMULLT_PAIR])

(define_int_iterator FCADD [UNSPEC_FCADD90
			    UNSPEC_FCADD270])

(define_int_iterator FCMLA [UNSPEC_FCMLA
			    UNSPEC_FCMLA90
			    UNSPEC_FCMLA180
			    UNSPEC_FCMLA270])

(define_int_iterator FRINTNZX [UNSPEC_FRINT32Z UNSPEC_FRINT32X
			       UNSPEC_FRINT64Z UNSPEC_FRINT64X])

(define_int_iterator SVE_BRK_UNARY [UNSPEC_BRKA UNSPEC_BRKB])

(define_int_iterator SVE_BRK_BINARY [UNSPEC_BRKN UNSPEC_BRKPA UNSPEC_BRKPB])

(define_int_iterator SVE_PITER [UNSPEC_PFIRST UNSPEC_PNEXT])

(define_int_iterator MATMUL [UNSPEC_SMATMUL UNSPEC_UMATMUL
			     UNSPEC_USMATMUL])

(define_int_iterator FMMLA [UNSPEC_FMMLA])

(define_int_iterator BF_MLA [UNSPEC_BFMLALB
			     UNSPEC_BFMLALT])

(define_int_iterator FCMLA_OP [UNSPEC_FCMLA
			       UNSPEC_FCMLA180
			       UNSPEC_FCMLA_CONJ
			       UNSPEC_FCMLA180_CONJ])

(define_int_iterator FCMUL_OP [UNSPEC_FCMUL
			       UNSPEC_FCMUL_CONJ])

;; Iterators for atomic operations.

(define_int_iterator ATOMIC_LDOP
 [UNSPECV_ATOMIC_LDOP_OR UNSPECV_ATOMIC_LDOP_BIC
  UNSPECV_ATOMIC_LDOP_XOR UNSPECV_ATOMIC_LDOP_PLUS])

(define_int_attr atomic_ldop
 [(UNSPECV_ATOMIC_LDOP_OR "set") (UNSPECV_ATOMIC_LDOP_BIC "clr")
  (UNSPECV_ATOMIC_LDOP_XOR "eor") (UNSPECV_ATOMIC_LDOP_PLUS "add")])

(define_int_attr atomic_ldoptab
 [(UNSPECV_ATOMIC_LDOP_OR "ior") (UNSPECV_ATOMIC_LDOP_BIC "bic")
  (UNSPECV_ATOMIC_LDOP_XOR "xor") (UNSPECV_ATOMIC_LDOP_PLUS "add")])

;; -------------------------------------------------------------------
;; Int Iterators Attributes.
;; -------------------------------------------------------------------

;; The optab associated with an operation.  Note that for ANDF, IORF
;; and XORF, the optab pattern is not actually defined; we just use this
;; name for consistency with the integer patterns.
(define_int_attr optab [(UNSPEC_ANDF "and")
			(UNSPEC_IORF "ior")
			(UNSPEC_XORF "xor")
			(UNSPEC_SADDV "sadd")
			(UNSPEC_UADDV "uadd")
			(UNSPEC_ANDV "and")
			(UNSPEC_IORV "ior")
			(UNSPEC_XORV "xor")
			(UNSPEC_FRECPE "frecpe")
			(UNSPEC_FRECPS "frecps")
			(UNSPEC_RSQRTE "frsqrte")
			(UNSPEC_RSQRTS "frsqrts")
			(UNSPEC_RBIT "rbit")
			(UNSPEC_REVB "revb")
			(UNSPEC_REVH "revh")
			(UNSPEC_REVW "revw")
			(UNSPEC_UMAXV "umax")
			(UNSPEC_UMINV "umin")
			(UNSPEC_SMAXV "smax")
			(UNSPEC_SMINV "smin")
			(UNSPEC_CADD90 "cadd90")
			(UNSPEC_CADD270 "cadd270")
			(UNSPEC_CDOT "cdot")
			(UNSPEC_CDOT90 "cdot90")
			(UNSPEC_CDOT180 "cdot180")
			(UNSPEC_CDOT270 "cdot270")
			(UNSPEC_CMLA "cmla")
			(UNSPEC_CMLA90 "cmla90")
			(UNSPEC_CMLA180 "cmla180")
			(UNSPEC_CMLA270 "cmla270")
			(UNSPEC_FADDV "plus")
			(UNSPEC_FMAXNMV "smax")
			(UNSPEC_FMAXV "smax_nan")
			(UNSPEC_FMINNMV "smin")
			(UNSPEC_FMINV "smin_nan")
		        (UNSPEC_SMUL_HIGHPART "smulh")
		        (UNSPEC_UMUL_HIGHPART "umulh")
			(UNSPEC_FMLA "fma")
			(UNSPEC_FMLS "fnma")
			(UNSPEC_FCMLA "fcmla")
			(UNSPEC_FCMLA90 "fcmla90")
			(UNSPEC_FCMLA180 "fcmla180")
			(UNSPEC_FCMLA270 "fcmla270")
			(UNSPEC_FEXPA "fexpa")
			(UNSPEC_FTSMUL "ftsmul")
			(UNSPEC_FTSSEL "ftssel")
			(UNSPEC_PMULLB "pmullb")
			(UNSPEC_PMULLB_PAIR "pmullb_pair")
			(UNSPEC_PMULLT "pmullt")
			(UNSPEC_PMULLT_PAIR "pmullt_pair")
			(UNSPEC_SMATMUL "smatmul")
			(UNSPEC_SQCADD90 "sqcadd90")
			(UNSPEC_SQCADD270 "sqcadd270")
			(UNSPEC_SQRDCMLAH "sqrdcmlah")
			(UNSPEC_SQRDCMLAH90 "sqrdcmlah90")
			(UNSPEC_SQRDCMLAH180 "sqrdcmlah180")
			(UNSPEC_SQRDCMLAH270 "sqrdcmlah270")
			(UNSPEC_TRN1Q "trn1q")
			(UNSPEC_TRN2Q "trn2q")
			(UNSPEC_UMATMUL "umatmul")
			(UNSPEC_USMATMUL "usmatmul")
			(UNSPEC_UZP1Q "uzp1q")
			(UNSPEC_UZP2Q "uzp2q")
			(UNSPEC_WHILERW "vec_check_raw_alias")
			(UNSPEC_WHILEWR "vec_check_war_alias")
			(UNSPEC_ZIP1Q "zip1q")
			(UNSPEC_ZIP2Q "zip2q")
			(UNSPEC_COND_FABS "abs")
			(UNSPEC_COND_FADD "add")
			(UNSPEC_COND_FCADD90 "cadd90")
			(UNSPEC_COND_FCADD270 "cadd270")
			(UNSPEC_COND_FCMLA "fcmla")
			(UNSPEC_COND_FCMLA90 "fcmla90")
			(UNSPEC_COND_FCMLA180 "fcmla180")
			(UNSPEC_COND_FCMLA270 "fcmla270")
			(UNSPEC_COND_FCVT "fcvt")
			(UNSPEC_COND_FCVTZS "fix_trunc")
			(UNSPEC_COND_FCVTZU "fixuns_trunc")
			(UNSPEC_COND_FDIV "div")
			(UNSPEC_COND_FMAX "smax_nan")
			(UNSPEC_COND_FMAXNM "smax")
			(UNSPEC_COND_FMIN "smin_nan")
			(UNSPEC_COND_FMINNM "smin")
			(UNSPEC_COND_FMLA "fma")
			(UNSPEC_COND_FMLS "fnma")
			(UNSPEC_COND_FMUL "mul")
			(UNSPEC_COND_FMULX "mulx")
			(UNSPEC_COND_FNEG "neg")
			(UNSPEC_COND_FNMLA "fnms")
			(UNSPEC_COND_FNMLS "fms")
			(UNSPEC_COND_FRECPX "frecpx")
			(UNSPEC_COND_FRINTA "round")
			(UNSPEC_COND_FRINTI "nearbyint")
			(UNSPEC_COND_FRINTM "floor")
			(UNSPEC_COND_FRINTN "frintn")
			(UNSPEC_COND_FRINTP "ceil")
			(UNSPEC_COND_FRINTX "rint")
			(UNSPEC_COND_FRINTZ "btrunc")
			(UNSPEC_COND_FSCALE "fscale")
			(UNSPEC_COND_FSQRT "sqrt")
			(UNSPEC_COND_FSUB "sub")
			(UNSPEC_COND_SCVTF "float")
			(UNSPEC_COND_UCVTF "floatuns")])

(define_int_attr  maxmin_uns [(UNSPEC_UMAXV "umax")
			      (UNSPEC_UMINV "umin")
			      (UNSPEC_SMAXV "smax")
			      (UNSPEC_SMINV "smin")
			      (UNSPEC_FMAX  "smax_nan")
			      (UNSPEC_FMAXNMV "smax")
			      (UNSPEC_FMAXV "smax_nan")
			      (UNSPEC_FMIN "smin_nan")
			      (UNSPEC_FMINNMV "smin")
			      (UNSPEC_FMINV "smin_nan")
			      (UNSPEC_FMAXNM "fmax")
			      (UNSPEC_FMINNM "fmin")
			      (UNSPEC_COND_FMAX "fmax_nan")
			      (UNSPEC_COND_FMAXNM "fmax")
			      (UNSPEC_COND_FMIN "fmin_nan")
			      (UNSPEC_COND_FMINNM "fmin")])

(define_int_attr  maxmin_uns_op [(UNSPEC_UMAXV "umax")
				 (UNSPEC_UMINV "umin")
				 (UNSPEC_SMAXV "smax")
				 (UNSPEC_SMINV "smin")
				 (UNSPEC_FMAX "fmax")
				 (UNSPEC_FMAXNMV "fmaxnm")
				 (UNSPEC_FMAXV "fmax")
				 (UNSPEC_FMIN "fmin")
				 (UNSPEC_FMINNMV "fminnm")
				 (UNSPEC_FMINV "fmin")
				 (UNSPEC_FMAXNM "fmaxnm")
				 (UNSPEC_FMINNM "fminnm")])

(define_code_attr binqops_op [(ss_plus "sqadd")
			      (us_plus "uqadd")
			      (ss_minus "sqsub")
			      (us_minus "uqsub")])

(define_code_attr binqops_op_rev [(ss_plus "sqsub")
				  (ss_minus "sqadd")])

;; The SVE logical instruction that implements an unspec.
(define_int_attr logicalf_op [(UNSPEC_ANDF "and")
		 	      (UNSPEC_IORF "orr")
			      (UNSPEC_XORF "eor")])

(define_int_attr last_op [(UNSPEC_CLASTA "after_last")
			  (UNSPEC_CLASTB "last")
			  (UNSPEC_LASTA "after_last")
			  (UNSPEC_LASTB "last")])

;; "s" for signed operations and "u" for unsigned ones.
(define_int_attr su [(UNSPEC_SADDV "s")
		     (UNSPEC_UADDV "u")
		     (UNSPEC_SADDLP "s")
		     (UNSPEC_UADDLP "u")
		     (UNSPEC_SADDLV "s")
		     (UNSPEC_UADDLV "u")
		     (UNSPEC_UNPACKSHI "s")
		     (UNSPEC_UNPACKUHI "u")
		     (UNSPEC_UNPACKSLO "s")
		     (UNSPEC_UNPACKULO "u")
		     (UNSPEC_SMUL_HIGHPART "s")
		     (UNSPEC_UMUL_HIGHPART "u")
		     (UNSPEC_COND_FCVTZS "s")
		     (UNSPEC_COND_FCVTZU "u")
		     (UNSPEC_COND_SCVTF "s")
		     (UNSPEC_COND_UCVTF "u")
		     (UNSPEC_SMULHS "s") (UNSPEC_UMULHS "u")
		     (UNSPEC_SMULHRS "s") (UNSPEC_UMULHRS "u")])

(define_int_attr sur [(UNSPEC_SHADD "s") (UNSPEC_UHADD "u")
		      (UNSPEC_SRHADD "sr") (UNSPEC_URHADD "ur")
		      (UNSPEC_SHSUB "s") (UNSPEC_UHSUB "u")
		      (UNSPEC_ADDHN "") (UNSPEC_RADDHN "r")
		      (UNSPEC_SABAL "s") (UNSPEC_UABAL "u")
		      (UNSPEC_SABAL2 "s") (UNSPEC_UABAL2 "u")
		      (UNSPEC_SABDL "s") (UNSPEC_UABDL "u")
		      (UNSPEC_SABDL2 "s") (UNSPEC_UABDL2 "u")
		      (UNSPEC_SADALP "s") (UNSPEC_UADALP "u")
		      (UNSPEC_SUBHN "") (UNSPEC_RSUBHN "r")
		      (UNSPEC_ADDHN2 "") (UNSPEC_RADDHN2 "r")
		      (UNSPEC_SUBHN2 "") (UNSPEC_RSUBHN2 "r")
		      (UNSPEC_USQADD "us") (UNSPEC_SUQADD "su")
		      (UNSPEC_SSLI  "s") (UNSPEC_USLI  "u")
		      (UNSPEC_SSRI  "s") (UNSPEC_USRI  "u")
		      (UNSPEC_USRA  "u") (UNSPEC_SSRA  "s")
		      (UNSPEC_URSRA  "ur") (UNSPEC_SRSRA  "sr")
		      (UNSPEC_URSHR  "ur") (UNSPEC_SRSHR  "sr")
		      (UNSPEC_SQSHLU "s") (UNSPEC_SQSHL   "s")
		      (UNSPEC_UQSHL  "u")
		      (UNSPEC_SQSHRUN "s") (UNSPEC_SQRSHRUN "s")
                      (UNSPEC_SQSHRN "s")  (UNSPEC_UQSHRN "u")
                      (UNSPEC_SQRSHRN "s") (UNSPEC_UQRSHRN "u")
		      (UNSPEC_USHL  "u")   (UNSPEC_SSHL  "s")
		      (UNSPEC_USHLL  "u")  (UNSPEC_SSHLL "s")
		      (UNSPEC_URSHL  "ur") (UNSPEC_SRSHL  "sr")
		      (UNSPEC_UQRSHL  "u") (UNSPEC_SQRSHL  "s")
		      (UNSPEC_SDOT "s") (UNSPEC_UDOT "u")
		      (UNSPEC_USDOT "us") (UNSPEC_SUDOT "su")
		      (UNSPEC_SMATMUL "s") (UNSPEC_UMATMUL "u")
		      (UNSPEC_USMATMUL "us")
])

(define_int_attr r [(UNSPEC_SQDMULH "") (UNSPEC_SQRDMULH "r")
		    (UNSPEC_SQSHRUN "") (UNSPEC_SQRSHRUN "r")
                    (UNSPEC_SQSHRN "")  (UNSPEC_UQSHRN "")
                    (UNSPEC_SQRSHRN "r") (UNSPEC_UQRSHRN "r")
                    (UNSPEC_SQSHL   "")  (UNSPEC_UQSHL  "")
                    (UNSPEC_SQRSHL   "r")(UNSPEC_UQRSHL  "r")
		    (UNSPEC_SMULHS "") (UNSPEC_UMULHS "")
		    (UNSPEC_SMULHRS "r") (UNSPEC_UMULHRS "r")
])

(define_int_attr lr [(UNSPEC_SSLI  "l") (UNSPEC_USLI  "l")
		     (UNSPEC_SSRI  "r") (UNSPEC_USRI  "r")
		     (UNSPEC_SQSHL "l") (UNSPEC_UQSHL "l")
		     (UNSPEC_SQSHLU "l")
		     (UNSPEC_SRSHR "r") (UNSPEC_URSHR "r")
		     (UNSPEC_ASRD  "r")
		     (UNSPEC_SLI   "l") (UNSPEC_SRI   "r")])

(define_int_attr u [(UNSPEC_SQSHLU "u") (UNSPEC_SQSHL "") (UNSPEC_UQSHL "")
		    (UNSPEC_SQSHRUN "u") (UNSPEC_SQRSHRUN "u")
		    (UNSPEC_SQSHRN "")  (UNSPEC_UQSHRN "")
		    (UNSPEC_SQRSHRN "") (UNSPEC_UQRSHRN "")
		    (UNSPEC_SHADD "") (UNSPEC_UHADD "u")
		    (UNSPEC_SRHADD "") (UNSPEC_URHADD "u")])

(define_int_attr fn [(UNSPEC_LDFF1 "f") (UNSPEC_LDNF1 "n")])

(define_int_attr ab [(UNSPEC_CLASTA "a") (UNSPEC_CLASTB "b")
		     (UNSPEC_LASTA "a") (UNSPEC_LASTB "b")])

(define_int_attr bt [(UNSPEC_BFMLALB "b") (UNSPEC_BFMLALT "t")])

(define_int_attr addsub [(UNSPEC_SHADD "add")
			 (UNSPEC_UHADD "add")
			 (UNSPEC_SRHADD "add")
			 (UNSPEC_URHADD "add")
			 (UNSPEC_SHSUB "sub")
			 (UNSPEC_UHSUB "sub")
			 (UNSPEC_ADDHN "add")
			 (UNSPEC_SUBHN "sub")
			 (UNSPEC_RADDHN "add")
			 (UNSPEC_RSUBHN "sub")
			 (UNSPEC_ADDHN2 "add")
			 (UNSPEC_SUBHN2 "sub")
			 (UNSPEC_RADDHN2 "add")
			 (UNSPEC_RSUBHN2 "sub")])

;; BSL variants: first commutative operand.
(define_int_attr bsl_1st [(1 "w") (2 "0")])

;; BSL variants: second commutative operand.
(define_int_attr bsl_2nd [(1 "0") (2 "w")])

;; BSL variants: duplicated input operand.
(define_int_attr bsl_dup [(1 "1") (2 "2")])

;; BSL variants: operand which requires preserving via movprfx.
(define_int_attr bsl_mov [(1 "2") (2 "1")])

(define_int_attr offsetlr [(UNSPEC_SSLI "") (UNSPEC_USLI "")
			   (UNSPEC_SSRI "offset_")
			   (UNSPEC_USRI "offset_")])

;; Standard pattern names for floating-point rounding instructions.
(define_int_attr frint_pattern [(UNSPEC_FRINTZ "btrunc")
				(UNSPEC_FRINTP "ceil")
				(UNSPEC_FRINTM "floor")
				(UNSPEC_FRINTI "nearbyint")
				(UNSPEC_FRINTX "rint")
				(UNSPEC_FRINTA "round")
				(UNSPEC_FRINTN "frintn")])

;; frint suffix for floating-point rounding instructions.
(define_int_attr frint_suffix [(UNSPEC_FRINTZ "z") (UNSPEC_FRINTP "p")
			       (UNSPEC_FRINTM "m") (UNSPEC_FRINTI "i")
			       (UNSPEC_FRINTX "x") (UNSPEC_FRINTA "a")
			       (UNSPEC_FRINTN "n")])

(define_int_attr fcvt_pattern [(UNSPEC_FRINTZ "btrunc") (UNSPEC_FRINTA "round")
			       (UNSPEC_FRINTP "ceil") (UNSPEC_FRINTM "floor")
			       (UNSPEC_FRINTN "frintn")])

(define_int_attr fcvt_fixed_insn [(UNSPEC_SCVTF "scvtf")
				  (UNSPEC_UCVTF "ucvtf")
				  (UNSPEC_FCVTZS "fcvtzs")
				  (UNSPEC_FCVTZU "fcvtzu")])

;; Pointer authentication mnemonic prefix.
(define_int_attr pauth_mnem_prefix [(UNSPEC_PACIASP "pacia")
				    (UNSPEC_PACIBSP "pacib")
				    (UNSPEC_PACIA1716 "pacia")
				    (UNSPEC_PACIB1716 "pacib")
				    (UNSPEC_AUTIASP "autia")
				    (UNSPEC_AUTIBSP "autib")
				    (UNSPEC_AUTIA1716 "autia")
				    (UNSPEC_AUTIB1716 "autib")])

(define_int_attr pauth_key [(UNSPEC_PACIASP "AARCH64_KEY_A")
			    (UNSPEC_PACIBSP "AARCH64_KEY_B")
			    (UNSPEC_PACIA1716 "AARCH64_KEY_A")
			    (UNSPEC_PACIB1716 "AARCH64_KEY_B")
			    (UNSPEC_AUTIASP "AARCH64_KEY_A")
			    (UNSPEC_AUTIBSP "AARCH64_KEY_B")
			    (UNSPEC_AUTIA1716 "AARCH64_KEY_A")
			    (UNSPEC_AUTIB1716 "AARCH64_KEY_B")])

;; Pointer authentication HINT number for NOP space instructions using A and
;; B key.
(define_int_attr pauth_hint_num [(UNSPEC_PACIASP "25")
				   (UNSPEC_PACIBSP "27")
				   (UNSPEC_AUTIASP "29")
				   (UNSPEC_AUTIBSP "31")
				   (UNSPEC_PACIA1716 "8")
				   (UNSPEC_PACIB1716 "10")
				   (UNSPEC_AUTIA1716 "12")
				   (UNSPEC_AUTIB1716 "14")])

(define_int_attr perm_insn [(UNSPEC_ZIP1 "zip1") (UNSPEC_ZIP2 "zip2")
			    (UNSPEC_ZIP1Q "zip1") (UNSPEC_ZIP2Q "zip2")
			    (UNSPEC_TRN1 "trn1") (UNSPEC_TRN2 "trn2")
			    (UNSPEC_TRN1Q "trn1") (UNSPEC_TRN2Q "trn2")
			    (UNSPEC_UZP1 "uzp1") (UNSPEC_UZP2 "uzp2")
			    (UNSPEC_UZP1Q "uzp1") (UNSPEC_UZP2Q "uzp2")])

; op code for REV instructions (size within which elements are reversed).
(define_int_attr rev_op [(UNSPEC_REV64 "64") (UNSPEC_REV32 "32")
			 (UNSPEC_REV16 "16")])

(define_int_attr perm_hilo [(UNSPEC_UNPACKSHI "hi") (UNSPEC_UNPACKUHI "hi")
			    (UNSPEC_UNPACKSLO "lo") (UNSPEC_UNPACKULO "lo")])

;; Return true if the associated optab refers to the high-numbered lanes,
;; false if it refers to the low-numbered lanes.  The convention is for
;; "hi" to refer to the low-numbered lanes (the first ones in memory)
;; for big-endian.
(define_int_attr hi_lanes_optab [(UNSPEC_UNPACKSHI "!BYTES_BIG_ENDIAN")
				 (UNSPEC_UNPACKUHI "!BYTES_BIG_ENDIAN")
				 (UNSPEC_UNPACKSLO "BYTES_BIG_ENDIAN")
				 (UNSPEC_UNPACKULO "BYTES_BIG_ENDIAN")])

(define_int_attr crc_variant [(UNSPEC_CRC32B "crc32b") (UNSPEC_CRC32H "crc32h")
                        (UNSPEC_CRC32W "crc32w") (UNSPEC_CRC32X "crc32x")
                        (UNSPEC_CRC32CB "crc32cb") (UNSPEC_CRC32CH "crc32ch")
                        (UNSPEC_CRC32CW "crc32cw") (UNSPEC_CRC32CX "crc32cx")])

(define_int_attr crc_mode [(UNSPEC_CRC32B "QI") (UNSPEC_CRC32H "HI")
                        (UNSPEC_CRC32W "SI") (UNSPEC_CRC32X "DI")
                        (UNSPEC_CRC32CB "QI") (UNSPEC_CRC32CH "HI")
                        (UNSPEC_CRC32CW "SI") (UNSPEC_CRC32CX "DI")])

(define_int_attr aes_op [(UNSPEC_AESE "e") (UNSPEC_AESD "d")])
(define_int_attr aesmc_op [(UNSPEC_AESMC "mc") (UNSPEC_AESIMC "imc")])

(define_int_attr sha1_op [(UNSPEC_SHA1C "c") (UNSPEC_SHA1P "p")
			  (UNSPEC_SHA1M "m")])

(define_int_attr sha256_op [(UNSPEC_SHA256H "") (UNSPEC_SHA256H2 "2")])

(define_int_attr rdma_as [(UNSPEC_SQRDMLAH "a") (UNSPEC_SQRDMLSH "s")])

(define_int_attr sha512_op [(UNSPEC_SHA512H "") (UNSPEC_SHA512H2 "2")])

(define_int_attr sm3tt_op [(UNSPEC_SM3TT1A "1a") (UNSPEC_SM3TT1B "1b")
			   (UNSPEC_SM3TT2A "2a") (UNSPEC_SM3TT2B "2b")])

(define_int_attr sm3part_op [(UNSPEC_SM3PARTW1 "1") (UNSPEC_SM3PARTW2 "2")])

(define_int_attr f16mac1 [(UNSPEC_FMLAL "a") (UNSPEC_FMLSL "s")
			  (UNSPEC_FMLAL2 "a") (UNSPEC_FMLSL2 "s")])

(define_int_attr frintnzs_op [(UNSPEC_FRINT32Z "frint32z") (UNSPEC_FRINT32X "frint32x")
			      (UNSPEC_FRINT64Z "frint64z") (UNSPEC_FRINT64X "frint64x")])

;; The condition associated with an UNSPEC_COND_<xx>.
(define_int_attr cmp_op [(UNSPEC_COND_CMPEQ_WIDE "eq")
			 (UNSPEC_COND_CMPGE_WIDE "ge")
			 (UNSPEC_COND_CMPGT_WIDE "gt")
			 (UNSPEC_COND_CMPHI_WIDE "hi")
			 (UNSPEC_COND_CMPHS_WIDE "hs")
			 (UNSPEC_COND_CMPLE_WIDE "le")
			 (UNSPEC_COND_CMPLO_WIDE "lo")
			 (UNSPEC_COND_CMPLS_WIDE "ls")
			 (UNSPEC_COND_CMPLT_WIDE "lt")
			 (UNSPEC_COND_CMPNE_WIDE "ne")
			 (UNSPEC_COND_FCMEQ "eq")
			 (UNSPEC_COND_FCMGE "ge")
			 (UNSPEC_COND_FCMGT "gt")
			 (UNSPEC_COND_FCMLE "le")
			 (UNSPEC_COND_FCMLT "lt")
			 (UNSPEC_COND_FCMNE "ne")
			 (UNSPEC_WHILEGE "ge")
			 (UNSPEC_WHILEGT "gt")
			 (UNSPEC_WHILEHI "hi")
			 (UNSPEC_WHILEHS "hs")
			 (UNSPEC_WHILELE "le")
			 (UNSPEC_WHILELO "lo")
			 (UNSPEC_WHILELS "ls")
			 (UNSPEC_WHILELT "lt")
			 (UNSPEC_WHILERW "rw")
			 (UNSPEC_WHILEWR "wr")])

(define_int_attr while_optab_cmp [(UNSPEC_WHILEGE "ge")
				  (UNSPEC_WHILEGT "gt")
				  (UNSPEC_WHILEHI "ugt")
				  (UNSPEC_WHILEHS "uge")
				  (UNSPEC_WHILELE "le")
				  (UNSPEC_WHILELO "ult")
				  (UNSPEC_WHILELS "ule")
				  (UNSPEC_WHILELT "lt")
				  (UNSPEC_WHILERW "rw")
				  (UNSPEC_WHILEWR "wr")])

(define_int_attr raw_war [(UNSPEC_WHILERW "raw")
			  (UNSPEC_WHILEWR "war")])

(define_int_attr brk_op [(UNSPEC_BRKA "a") (UNSPEC_BRKB "b")
			 (UNSPEC_BRKN "n")
			 (UNSPEC_BRKPA "pa") (UNSPEC_BRKPB "pb")])

(define_int_attr sve_pred_op [(UNSPEC_PFIRST "pfirst") (UNSPEC_PNEXT "pnext")])

(define_int_attr sve_int_op [(UNSPEC_ADCLB "adclb")
			     (UNSPEC_ADCLT "adclt")
			     (UNSPEC_ADDHNB "addhnb")
			     (UNSPEC_ADDHNT "addhnt")
			     (UNSPEC_ADDP "addp")
			     (UNSPEC_ANDV "andv")
			     (UNSPEC_ASHIFTRT_WIDE "asr")
			     (UNSPEC_ASHIFT_WIDE "lsl")
			     (UNSPEC_ASRD "asrd")
			     (UNSPEC_BDEP "bdep")
			     (UNSPEC_BEXT "bext")
			     (UNSPEC_BGRP "bgrp")
			     (UNSPEC_CADD90 "cadd")
			     (UNSPEC_CADD270 "cadd")
			     (UNSPEC_CDOT "cdot")
			     (UNSPEC_CDOT90 "cdot")
			     (UNSPEC_CDOT180 "cdot")
			     (UNSPEC_CDOT270 "cdot")
			     (UNSPEC_CMLA "cmla")
			     (UNSPEC_CMLA90 "cmla")
			     (UNSPEC_CMLA180 "cmla")
			     (UNSPEC_CMLA270 "cmla")
			     (UNSPEC_EORBT "eorbt")
			     (UNSPEC_EORTB "eortb")
			     (UNSPEC_IORV "orv")
			     (UNSPEC_LSHIFTRT_WIDE "lsr")
			     (UNSPEC_MATCH "match")
			     (UNSPEC_NMATCH "nmatch")
			     (UNSPEC_PMULLB "pmullb")
			     (UNSPEC_PMULLB_PAIR "pmullb")
			     (UNSPEC_PMULLT "pmullt")
			     (UNSPEC_PMULLT_PAIR "pmullt")
			     (UNSPEC_RADDHNB "raddhnb")
			     (UNSPEC_RADDHNT "raddhnt")
			     (UNSPEC_RBIT "rbit")
			     (UNSPEC_REVB "revb")
			     (UNSPEC_REVH "revh")
			     (UNSPEC_REVW "revw")
			     (UNSPEC_RSHRNB "rshrnb")
			     (UNSPEC_RSHRNT "rshrnt")
			     (UNSPEC_RSQRTE "ursqrte")
			     (UNSPEC_RSUBHNB "rsubhnb")
			     (UNSPEC_RSUBHNT "rsubhnt")
			     (UNSPEC_SABDLB "sabdlb")
			     (UNSPEC_SABDLT "sabdlt")
			     (UNSPEC_SADALP "sadalp")
			     (UNSPEC_SADDLB "saddlb")
			     (UNSPEC_SADDLBT "saddlbt")
			     (UNSPEC_SADDLT "saddlt")
			     (UNSPEC_SADDWB "saddwb")
			     (UNSPEC_SADDWT "saddwt")
			     (UNSPEC_SBCLB "sbclb")
			     (UNSPEC_SBCLT "sbclt")
			     (UNSPEC_SHADD "shadd")
			     (UNSPEC_SHRNB "shrnb")
			     (UNSPEC_SHRNT "shrnt")
			     (UNSPEC_SHSUB "shsub")
			     (UNSPEC_SLI "sli")
			     (UNSPEC_SMAXP "smaxp")
			     (UNSPEC_SMAXV "smaxv")
			     (UNSPEC_SMINP "sminp")
			     (UNSPEC_SMINV "sminv")
			     (UNSPEC_SMUL_HIGHPART "smulh")
			     (UNSPEC_SMULLB "smullb")
			     (UNSPEC_SMULLT "smullt")
			     (UNSPEC_SQCADD90 "sqcadd")
			     (UNSPEC_SQCADD270 "sqcadd")
			     (UNSPEC_SQDMULH "sqdmulh")
			     (UNSPEC_SQDMULLB "sqdmullb")
			     (UNSPEC_SQDMULLBT "sqdmullbt")
			     (UNSPEC_SQDMULLT "sqdmullt")
			     (UNSPEC_SQRDCMLAH "sqrdcmlah")
			     (UNSPEC_SQRDCMLAH90 "sqrdcmlah")
			     (UNSPEC_SQRDCMLAH180 "sqrdcmlah")
			     (UNSPEC_SQRDCMLAH270 "sqrdcmlah")
			     (UNSPEC_SQRDMLAH "sqrdmlah")
			     (UNSPEC_SQRDMLSH "sqrdmlsh")
			     (UNSPEC_SQRDMULH "sqrdmulh")
			     (UNSPEC_SQRSHL "sqrshl")
			     (UNSPEC_SQRSHRNB "sqrshrnb")
			     (UNSPEC_SQRSHRNT "sqrshrnt")
			     (UNSPEC_SQRSHRUNB "sqrshrunb")
			     (UNSPEC_SQRSHRUNT "sqrshrunt")
			     (UNSPEC_SQSHL "sqshl")
			     (UNSPEC_SQSHLU "sqshlu")
			     (UNSPEC_SQSHRNB "sqshrnb")
			     (UNSPEC_SQSHRNT "sqshrnt")
			     (UNSPEC_SQSHRUNB "sqshrunb")
			     (UNSPEC_SQSHRUNT "sqshrunt")
			     (UNSPEC_SQXTNB "sqxtnb")
			     (UNSPEC_SQXTNT "sqxtnt")
			     (UNSPEC_SQXTUNB "sqxtunb")
			     (UNSPEC_SQXTUNT "sqxtunt")
			     (UNSPEC_SRHADD "srhadd")
			     (UNSPEC_SRI "sri")
			     (UNSPEC_SRSHL "srshl")
			     (UNSPEC_SRSHR "srshr")
			     (UNSPEC_SSHLLB "sshllb")
			     (UNSPEC_SSHLLT "sshllt")
			     (UNSPEC_SSUBLB "ssublb")
			     (UNSPEC_SSUBLBT "ssublbt")
			     (UNSPEC_SSUBLT "ssublt")
			     (UNSPEC_SSUBLTB "ssubltb")
			     (UNSPEC_SSUBWB "ssubwb")
			     (UNSPEC_SSUBWT "ssubwt")
			     (UNSPEC_SUBHNB "subhnb")
			     (UNSPEC_SUBHNT "subhnt")
			     (UNSPEC_SUQADD "suqadd")
			     (UNSPEC_UABDLB "uabdlb")
			     (UNSPEC_UABDLT "uabdlt")
			     (UNSPEC_UADALP "uadalp")
			     (UNSPEC_UADDLB "uaddlb")
			     (UNSPEC_UADDLT "uaddlt")
			     (UNSPEC_UADDWB "uaddwb")
			     (UNSPEC_UADDWT "uaddwt")
			     (UNSPEC_UHADD "uhadd")
			     (UNSPEC_UHSUB "uhsub")
			     (UNSPEC_UMAXP "umaxp")
			     (UNSPEC_UMAXV "umaxv")
			     (UNSPEC_UMINP "uminp")
			     (UNSPEC_UMINV "uminv")
			     (UNSPEC_UMUL_HIGHPART "umulh")
			     (UNSPEC_UMULLB "umullb")
			     (UNSPEC_UMULLT "umullt")
			     (UNSPEC_UQRSHL "uqrshl")
			     (UNSPEC_UQRSHRNB "uqrshrnb")
			     (UNSPEC_UQRSHRNT "uqrshrnt")
			     (UNSPEC_UQSHL "uqshl")
			     (UNSPEC_UQSHRNB "uqshrnb")
			     (UNSPEC_UQSHRNT "uqshrnt")
			     (UNSPEC_UQXTNB "uqxtnb")
			     (UNSPEC_UQXTNT "uqxtnt")
			     (UNSPEC_URECPE "urecpe")
			     (UNSPEC_URHADD "urhadd")
			     (UNSPEC_URSHL "urshl")
			     (UNSPEC_URSHR "urshr")
			     (UNSPEC_USHLLB "ushllb")
			     (UNSPEC_USHLLT "ushllt")
			     (UNSPEC_USQADD "usqadd")
			     (UNSPEC_USUBLB "usublb")
			     (UNSPEC_USUBLT "usublt")
			     (UNSPEC_USUBWB "usubwb")
			     (UNSPEC_USUBWT "usubwt")
			     (UNSPEC_XORV "eorv")])

(define_int_attr sve_int_op_rev [(UNSPEC_SHADD "shadd")
				 (UNSPEC_SHSUB "shsubr")
				 (UNSPEC_SQRSHL "sqrshlr")
				 (UNSPEC_SRHADD "srhadd")
				 (UNSPEC_SRSHL "srshlr")
				 (UNSPEC_UHADD "uhadd")
				 (UNSPEC_UHSUB "uhsubr")
				 (UNSPEC_UQRSHL "uqrshlr")
				 (UNSPEC_URHADD "urhadd")
				 (UNSPEC_URSHL "urshlr")])

(define_int_attr sve_int_add_op [(UNSPEC_SABDLB "sabalb")
				 (UNSPEC_SABDLT "sabalt")
				 (UNSPEC_SMULLB "smlalb")
				 (UNSPEC_SMULLT "smlalt")
				 (UNSPEC_UABDLB "uabalb")
				 (UNSPEC_UABDLT "uabalt")
				 (UNSPEC_UMULLB "umlalb")
				 (UNSPEC_UMULLT "umlalt")])

(define_int_attr sve_int_qadd_op [(UNSPEC_SQDMULLB "sqdmlalb")
				  (UNSPEC_SQDMULLBT "sqdmlalbt")
				  (UNSPEC_SQDMULLT "sqdmlalt")])

(define_int_attr sve_int_sub_op [(UNSPEC_SMULLB "smlslb")
				 (UNSPEC_SMULLT "smlslt")
				 (UNSPEC_UMULLB "umlslb")
				 (UNSPEC_UMULLT "umlslt")])

(define_int_attr sve_int_qsub_op [(UNSPEC_SQDMULLB "sqdmlslb")
				  (UNSPEC_SQDMULLBT "sqdmlslbt")
				  (UNSPEC_SQDMULLT "sqdmlslt")])

(define_int_attr sve_fp_op [(UNSPEC_BFDOT "bfdot")
			    (UNSPEC_BFMLALB "bfmlalb")
			    (UNSPEC_BFMLALT "bfmlalt")
			    (UNSPEC_BFMMLA "bfmmla")
			    (UNSPEC_FRECPE "frecpe")
			    (UNSPEC_FRECPS "frecps")
			    (UNSPEC_RSQRTE "frsqrte")
			    (UNSPEC_RSQRTS "frsqrts")
			    (UNSPEC_FADDP "faddp")
			    (UNSPEC_FADDV "faddv")
			    (UNSPEC_FEXPA "fexpa")
			    (UNSPEC_FMAXNMP "fmaxnmp")
			    (UNSPEC_FMAXNMV "fmaxnmv")
			    (UNSPEC_FMAXP "fmaxp")
			    (UNSPEC_FMAXV "fmaxv")
			    (UNSPEC_FMINNMP "fminnmp")
			    (UNSPEC_FMINNMV "fminnmv")
			    (UNSPEC_FMINP "fminp")
			    (UNSPEC_FMINV "fminv")
			    (UNSPEC_FMLA "fmla")
			    (UNSPEC_FMLALB "fmlalb")
			    (UNSPEC_FMLALT "fmlalt")
			    (UNSPEC_FMLS "fmls")
			    (UNSPEC_FMLSLB "fmlslb")
			    (UNSPEC_FMLSLT "fmlslt")
			    (UNSPEC_FMMLA "fmmla")
			    (UNSPEC_FTSMUL "ftsmul")
			    (UNSPEC_FTSSEL "ftssel")
			    (UNSPEC_COND_FABS "fabs")
			    (UNSPEC_COND_FADD "fadd")
			    (UNSPEC_COND_FCVTLT "fcvtlt")
			    (UNSPEC_COND_FCVTX "fcvtx")
			    (UNSPEC_COND_FDIV "fdiv")
			    (UNSPEC_COND_FLOGB "flogb")
			    (UNSPEC_COND_FMAX "fmax")
			    (UNSPEC_COND_FMAXNM "fmaxnm")
			    (UNSPEC_COND_FMIN "fmin")
			    (UNSPEC_COND_FMINNM "fminnm")
			    (UNSPEC_COND_FMUL "fmul")
			    (UNSPEC_COND_FMULX "fmulx")
			    (UNSPEC_COND_FNEG "fneg")
			    (UNSPEC_COND_FRECPX "frecpx")
			    (UNSPEC_COND_FRINTA "frinta")
			    (UNSPEC_COND_FRINTI "frinti")
			    (UNSPEC_COND_FRINTM "frintm")
			    (UNSPEC_COND_FRINTN "frintn")
			    (UNSPEC_COND_FRINTP "frintp")
			    (UNSPEC_COND_FRINTX "frintx")
			    (UNSPEC_COND_FRINTZ "frintz")
			    (UNSPEC_COND_FSCALE "fscale")
			    (UNSPEC_COND_FSQRT "fsqrt")
			    (UNSPEC_COND_FSUB "fsub")])

(define_int_attr sve_fp_op_rev [(UNSPEC_COND_FADD "fadd")
				(UNSPEC_COND_FDIV "fdivr")
				(UNSPEC_COND_FMAX "fmax")
				(UNSPEC_COND_FMAXNM "fmaxnm")
				(UNSPEC_COND_FMIN "fmin")
				(UNSPEC_COND_FMINNM "fminnm")
				(UNSPEC_COND_FMUL "fmul")
				(UNSPEC_COND_FMULX "fmulx")
				(UNSPEC_COND_FSUB "fsubr")])

(define_int_attr rot [(UNSPEC_CADD90 "90")
		      (UNSPEC_CADD270 "270")
		      (UNSPEC_CDOT "0")
		      (UNSPEC_CDOT90 "90")
		      (UNSPEC_CDOT180 "180")
		      (UNSPEC_CDOT270 "270")
		      (UNSPEC_CMLA "0")
		      (UNSPEC_CMLA90 "90")
		      (UNSPEC_CMLA180 "180")
		      (UNSPEC_CMLA270 "270")
		      (UNSPEC_FCADD90 "90")
		      (UNSPEC_FCADD270 "270")
		      (UNSPEC_FCMLA "0")
		      (UNSPEC_FCMLA90 "90")
		      (UNSPEC_FCMLA180 "180")
		      (UNSPEC_FCMLA270 "270")
		      (UNSPEC_SQCADD90 "90")
		      (UNSPEC_SQCADD270 "270")
		      (UNSPEC_SQRDCMLAH "0")
		      (UNSPEC_SQRDCMLAH90 "90")
		      (UNSPEC_SQRDCMLAH180 "180")
		      (UNSPEC_SQRDCMLAH270 "270")
		      (UNSPEC_COND_FCADD90 "90")
		      (UNSPEC_COND_FCADD270 "270")
		      (UNSPEC_COND_FCMLA "0")
		      (UNSPEC_COND_FCMLA90 "90")
		      (UNSPEC_COND_FCMLA180 "180")
		      (UNSPEC_COND_FCMLA270 "270")
		      (UNSPEC_FCMUL "0")
		      (UNSPEC_FCMUL_CONJ "180")])

;; A conjucate is a negation of the imaginary component
;; The number in the unspecs are the rotation component of the instruction, e.g
;; FCMLA180 means use the instruction with #180.
;; The iterator is used to produce the right name mangling for the function.
(define_int_attr conj_op [(UNSPEC_FCMLA180 "")
			  (UNSPEC_FCMLA180_CONJ "_conj")
			  (UNSPEC_FCMLA "")
			  (UNSPEC_FCMLA_CONJ "_conj")
			  (UNSPEC_FCMUL "")
			  (UNSPEC_FCMUL_CONJ "_conj")
			  (UNSPEC_CMLA "")
			  (UNSPEC_CMLA180 "")
			  (UNSPEC_CMLA180_CONJ "_conj")
			  (UNSPEC_CMLA_CONJ "_conj")
			  (UNSPEC_CMUL "")
			  (UNSPEC_CMUL_CONJ "_conj")])

;; The complex operations when performed on a real complex number require two
;; instructions to perform the operation. e.g. complex multiplication requires
;; two FCMUL with a particular rotation value.
;;
;; These values can be looked up in rotsplit1 and rotsplit2.  as an example
;; FCMUL needs the first instruction to use #0 and the second #90.
(define_int_attr rotsplit1 [(UNSPEC_FCMLA "0")
			    (UNSPEC_FCMLA_CONJ "0")
			    (UNSPEC_FCMUL "0")
			    (UNSPEC_FCMUL_CONJ "0")
			    (UNSPEC_FCMLA180 "180")
			    (UNSPEC_FCMLA180_CONJ "180")])

(define_int_attr rotsplit2 [(UNSPEC_FCMLA "90")
			    (UNSPEC_FCMLA_CONJ "270")
			    (UNSPEC_FCMUL "90")
			    (UNSPEC_FCMUL_CONJ "270")
			    (UNSPEC_FCMLA180 "270")
			    (UNSPEC_FCMLA180_CONJ "90")])

;; SVE has slightly different namings from NEON so we have to split these
;; iterators.
(define_int_attr sve_rot1 [(UNSPEC_FCMLA "")
			   (UNSPEC_FCMLA_CONJ "")
			   (UNSPEC_FCMUL "")
			   (UNSPEC_FCMUL_CONJ "")
			   (UNSPEC_FCMLA180 "180")
			   (UNSPEC_FCMLA180_CONJ "180")
			   (UNSPEC_CMLA "")
			   (UNSPEC_CMLA_CONJ "")
			   (UNSPEC_CMUL "")
			   (UNSPEC_CMUL_CONJ "")
			   (UNSPEC_CMLA180 "180")
			   (UNSPEC_CMLA180_CONJ "180")])

(define_int_attr sve_rot2 [(UNSPEC_FCMLA "90")
			   (UNSPEC_FCMLA_CONJ "270")
			   (UNSPEC_FCMUL "90")
			   (UNSPEC_FCMUL_CONJ "270")
			   (UNSPEC_FCMLA180 "270")
			   (UNSPEC_FCMLA180_CONJ "90")
			   (UNSPEC_CMLA "90")
			   (UNSPEC_CMLA_CONJ "270")
			   (UNSPEC_CMUL "90")
			   (UNSPEC_CMUL_CONJ "270")
			   (UNSPEC_CMLA180 "270")
			   (UNSPEC_CMLA180_CONJ "90")])


(define_int_attr fcmac1 [(UNSPEC_FCMLA "a") (UNSPEC_FCMLA_CONJ "a")
			 (UNSPEC_FCMLA180 "s") (UNSPEC_FCMLA180_CONJ "s")
			 (UNSPEC_CMLA "a") (UNSPEC_CMLA_CONJ "a")
			 (UNSPEC_CMLA180 "s") (UNSPEC_CMLA180_CONJ "s")])

(define_int_attr sve_fmla_op [(UNSPEC_COND_FMLA "fmla")
			      (UNSPEC_COND_FMLS "fmls")
			      (UNSPEC_COND_FNMLA "fnmla")
			      (UNSPEC_COND_FNMLS "fnmls")])

(define_int_attr sve_fmad_op [(UNSPEC_COND_FMLA "fmad")
			      (UNSPEC_COND_FMLS "fmsb")
			      (UNSPEC_COND_FNMLA "fnmad")
			      (UNSPEC_COND_FNMLS "fnmsb")])

;; The register constraint to use for the final operand in a binary BRK.
(define_int_attr brk_reg_con [(UNSPEC_BRKN "0")
			      (UNSPEC_BRKPA "Upa") (UNSPEC_BRKPB "Upa")])

;; The register number to print for the above.
(define_int_attr brk_reg_opno [(UNSPEC_BRKN "0")
			       (UNSPEC_BRKPA "3") (UNSPEC_BRKPB "3")])

;; The predicate to use for the first input operand in a floating-point
;; <optab><mode>3 pattern.
(define_int_attr sve_pred_fp_rhs1_operand
  [(UNSPEC_COND_FADD "register_operand")
   (UNSPEC_COND_FDIV "register_operand")
   (UNSPEC_COND_FMAX "register_operand")
   (UNSPEC_COND_FMAXNM "register_operand")
   (UNSPEC_COND_FMIN "register_operand")
   (UNSPEC_COND_FMINNM "register_operand")
   (UNSPEC_COND_FMUL "register_operand")
   (UNSPEC_COND_FMULX "register_operand")
   (UNSPEC_COND_FSUB "aarch64_sve_float_arith_operand")])

;; The predicate to use for the second input operand in a floating-point
;; <optab><mode>3 pattern.
(define_int_attr sve_pred_fp_rhs2_operand
  [(UNSPEC_COND_FADD "aarch64_sve_float_arith_with_sub_operand")
   (UNSPEC_COND_FDIV "register_operand")
   (UNSPEC_COND_FMAX "aarch64_sve_float_maxmin_operand")
   (UNSPEC_COND_FMAXNM "aarch64_sve_float_maxmin_operand")
   (UNSPEC_COND_FMIN "aarch64_sve_float_maxmin_operand")
   (UNSPEC_COND_FMINNM "aarch64_sve_float_maxmin_operand")
   (UNSPEC_COND_FMUL "aarch64_sve_float_mul_operand")
   (UNSPEC_COND_FMULX "register_operand")
   (UNSPEC_COND_FSUB "register_operand")])

;; Likewise for immediates only.
(define_int_attr sve_pred_fp_rhs2_immediate
  [(UNSPEC_COND_FMAX "aarch64_sve_float_maxmin_immediate")
   (UNSPEC_COND_FMAXNM "aarch64_sve_float_maxmin_immediate")
   (UNSPEC_COND_FMIN "aarch64_sve_float_maxmin_immediate")
   (UNSPEC_COND_FMINNM "aarch64_sve_float_maxmin_immediate")
   (UNSPEC_COND_FMUL "aarch64_sve_float_mul_immediate")])

;; The maximum number of element bits that an instruction can handle.
(define_int_attr max_elem_bits [(UNSPEC_UADDV "64") (UNSPEC_SADDV "32")
				(UNSPEC_PFIRST "8") (UNSPEC_PNEXT "64")])

;; The minimum number of element bits that an instruction can handle.
(define_int_attr min_elem_bits [(UNSPEC_RBIT "8")
				(UNSPEC_REVB "16")
				(UNSPEC_REVH "32")
				(UNSPEC_REVW "64")])

(define_int_attr unspec [(UNSPEC_WHILERW "UNSPEC_WHILERW")
			 (UNSPEC_WHILEWR "UNSPEC_WHILEWR")])

;; Iterators and attributes for fpcr fpsr getter setters

(define_int_iterator GET_FPSCR
  [UNSPECV_GET_FPSR UNSPECV_GET_FPCR])

(define_int_iterator SET_FPSCR
  [UNSPECV_SET_FPSR UNSPECV_SET_FPCR])

(define_int_attr fpscr_name
  [(UNSPECV_GET_FPSR "fpsr")
   (UNSPECV_SET_FPSR "fpsr")
   (UNSPECV_GET_FPCR "fpcr")
   (UNSPECV_SET_FPCR "fpcr")])
