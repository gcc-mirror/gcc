;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2018 Free Software Foundation, Inc.
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


;; Iterator for General Purpose Integer registers (32- and 64-bit modes)
(define_mode_iterator GPI [SI DI])

;; Iterator for HI, SI, DI, some instructions can only work on these modes.
(define_mode_iterator GPI_I16 [(HI "AARCH64_ISA_F16") SI DI])

;; Iterator for QI and HI modes
(define_mode_iterator SHORT [QI HI])

;; Iterator for all integer modes (up to 64-bit)
(define_mode_iterator ALLI [QI HI SI DI])

;; Iterator for all integer modes that can be extended (up to 64-bit)
(define_mode_iterator ALLX [QI HI SI])

;; Iterator for General Purpose Floating-point registers (32- and 64-bit modes)
(define_mode_iterator GPF [SF DF])

;; Iterator for all scalar floating point modes (HF, SF, DF)
(define_mode_iterator GPF_F16 [(HF "AARCH64_ISA_F16") SF DF])

;; Iterator for all scalar floating point modes (HF, SF, DF)
(define_mode_iterator GPF_HF [HF SF DF])

;; Iterator for all scalar floating point modes (HF, SF, DF and TF)
(define_mode_iterator GPF_TF_F16 [HF SF DF TF])

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
(define_mode_iterator VD [V8QI V4HI V4HF V2SI V2SF])

;; All modes stored in registers d0-d31.
(define_mode_iterator DREG [V8QI V4HI V4HF V2SI V2SF DF])

;; Copy of the above.
(define_mode_iterator DREG2 [V8QI V4HI V4HF V2SI V2SF DF])

;; Advanced SIMD, 64-bit container, all integer modes.
(define_mode_iterator VD_BHSI [V8QI V4HI V2SI])

;; 128 and 64-bit container; 8, 16, 32-bit vector integer modes
(define_mode_iterator VDQ_BHSI [V8QI V16QI V4HI V8HI V2SI V4SI])

;; Quad vector modes.
(define_mode_iterator VQ [V16QI V8HI V4SI V2DI V8HF V4SF V2DF])

;; Copy of the above.
(define_mode_iterator VQ2 [V16QI V8HI V4SI V2DI V8HF V4SF V2DF])

;; Quad integer vector modes.
(define_mode_iterator VQ_I [V16QI V8HI V4SI V2DI])

;; VQ without 2 element modes.
(define_mode_iterator VQ_NO2E [V16QI V8HI V4SI V8HF V4SF])

;; Quad vector with only 2 element modes.
(define_mode_iterator VQ_2E [V2DI V2DF])

;; This mode iterator allows :P to be used for patterns that operate on
;; addresses in different modes.  In LP64, only DI will match, while in
;; ILP32, either can match.
(define_mode_iterator P [(SI "ptr_mode == SImode || Pmode == SImode")
			 (DI "ptr_mode == DImode || Pmode == DImode")])

;; This mode iterator allows :PTR to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator PTR [(SI "ptr_mode == SImode") (DI "ptr_mode == DImode")])

;; Advanced SIMD Float modes suitable for moving, loading and storing.
(define_mode_iterator VDQF_F16 [V4HF V8HF V2SF V4SF V2DF])

;; Advanced SIMD Float modes.
(define_mode_iterator VDQF [V2SF V4SF V2DF])
(define_mode_iterator VHSDF [(V4HF "TARGET_SIMD_F16INST")
			     (V8HF "TARGET_SIMD_F16INST")
			     V2SF V4SF V2DF])

;; Advanced SIMD Float modes, and DF.
(define_mode_iterator VHSDF_DF [(V4HF "TARGET_SIMD_F16INST")
				(V8HF "TARGET_SIMD_F16INST")
				V2SF V4SF V2DF DF])
(define_mode_iterator VHSDF_HSDF [(V4HF "TARGET_SIMD_F16INST")
				  (V8HF "TARGET_SIMD_F16INST")
				  V2SF V4SF V2DF
				  (HF "TARGET_SIMD_F16INST")
				  SF DF])

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
				V4HF V8HF V2SF V4SF V2DF])

;; The VALL_F16 modes except the 128-bit 2-element ones.
(define_mode_iterator VALL_F16_NO_V2Q [V8QI V16QI V4HI V8HI V2SI V4SI
				V4HF V8HF V2SF V4SF])

;; All Advanced SIMD modes barring HF modes, plus DI.
(define_mode_iterator VALLDI [V8QI V16QI V4HI V8HI V2SI V4SI V2DI V2SF V4SF V2DF DI])

;; All Advanced SIMD modes and DI.
(define_mode_iterator VALLDI_F16 [V8QI V16QI V4HI V8HI V2SI V4SI V2DI
				  V4HF V8HF V2SF V4SF V2DF DI])

;; All Advanced SIMD modes, plus DI and DF.
(define_mode_iterator VALLDIF [V8QI V16QI V4HI V8HI V2SI V4SI
			       V2DI V4HF V8HF V2SF V4SF V2DF DI DF])

;; Advanced SIMD modes for Integer reduction across lanes.
(define_mode_iterator VDQV [V8QI V16QI V4HI V8HI V4SI V2DI])

;; Advanced SIMD modes (except V2DI) for Integer reduction across lanes.
(define_mode_iterator VDQV_S [V8QI V16QI V4HI V8HI V4SI])

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
(define_mode_iterator VDC [V8QI V4HI V4HF V2SI V2SF DI DF])

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

;; All SVE vector modes.
(define_mode_iterator SVE_ALL [VNx16QI VNx8HI VNx4SI VNx2DI
			       VNx8HF VNx4SF VNx2DF])

;; All SVE vector structure modes.
(define_mode_iterator SVE_STRUCT [VNx32QI VNx16HI VNx8SI VNx4DI
				  VNx16HF VNx8SF VNx4DF
				  VNx48QI VNx24HI VNx12SI VNx6DI
				  VNx24HF VNx12SF VNx6DF
				  VNx64QI VNx32HI VNx16SI VNx8DI
				  VNx32HF VNx16SF VNx8DF])

;; All SVE vector modes that have 8-bit or 16-bit elements.
(define_mode_iterator SVE_BH [VNx16QI VNx8HI VNx8HF])

;; All SVE vector modes that have 8-bit, 16-bit or 32-bit elements.
(define_mode_iterator SVE_BHS [VNx16QI VNx8HI VNx4SI VNx8HF VNx4SF])

;; All SVE integer vector modes that have 8-bit, 16-bit or 32-bit elements.
(define_mode_iterator SVE_BHSI [VNx16QI VNx8HI VNx4SI])

;; All SVE integer vector modes that have 16-bit, 32-bit or 64-bit elements.
(define_mode_iterator SVE_HSDI [VNx16QI VNx8HI VNx4SI])

;; All SVE floating-point vector modes that have 16-bit or 32-bit elements.
(define_mode_iterator SVE_HSF [VNx8HF VNx4SF])

;; All SVE vector modes that have 32-bit or 64-bit elements.
(define_mode_iterator SVE_SD [VNx4SI VNx2DI VNx4SF VNx2DF])

;; All SVE vector modes that have 32-bit elements.
(define_mode_iterator SVE_S [VNx4SI VNx4SF])

;; All SVE vector modes that have 64-bit elements.
(define_mode_iterator SVE_D [VNx2DI VNx2DF])

;; All SVE integer vector modes that have 32-bit or 64-bit elements.
(define_mode_iterator SVE_SDI [VNx4SI VNx2DI])

;; All SVE integer vector modes.
(define_mode_iterator SVE_I [VNx16QI VNx8HI VNx4SI VNx2DI])

;; All SVE floating-point vector modes.
(define_mode_iterator SVE_F [VNx8HF VNx4SF VNx2DF])

;; All SVE predicate modes.
(define_mode_iterator PRED_ALL [VNx16BI VNx8BI VNx4BI VNx2BI])

;; SVE predicate modes that control 8-bit, 16-bit or 32-bit elements.
(define_mode_iterator PRED_BHS [VNx16BI VNx8BI VNx4BI])

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
    UNSPEC_SRHSUB	; Used in aarch64-simd.md.
    UNSPEC_URHSUB	; Used in aarch64-simd.md.
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
    UNSPEC_SQXTN	; Used in aarch64-simd.md.
    UNSPEC_UQXTN	; Used in aarch64-simd.md.
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
    UNSPEC_SEL		; Used in aarch64-sve.md.
    UNSPEC_ANDV		; Used in aarch64-sve.md.
    UNSPEC_IORV		; Used in aarch64-sve.md.
    UNSPEC_XORV		; Used in aarch64-sve.md.
    UNSPEC_ANDF		; Used in aarch64-sve.md.
    UNSPEC_IORF		; Used in aarch64-sve.md.
    UNSPEC_XORF		; Used in aarch64-sve.md.
    UNSPEC_SMUL_HIGHPART ; Used in aarch64-sve.md.
    UNSPEC_UMUL_HIGHPART ; Used in aarch64-sve.md.
    UNSPEC_COND_ADD	; Used in aarch64-sve.md.
    UNSPEC_COND_SUB	; Used in aarch64-sve.md.
    UNSPEC_COND_MUL	; Used in aarch64-sve.md.
    UNSPEC_COND_DIV	; Used in aarch64-sve.md.
    UNSPEC_COND_MAX	; Used in aarch64-sve.md.
    UNSPEC_COND_MIN	; Used in aarch64-sve.md.
    UNSPEC_COND_FMLA	; Used in aarch64-sve.md.
    UNSPEC_COND_FMLS	; Used in aarch64-sve.md.
    UNSPEC_COND_FNMLA	; Used in aarch64-sve.md.
    UNSPEC_COND_FNMLS	; Used in aarch64-sve.md.
    UNSPEC_COND_LT	; Used in aarch64-sve.md.
    UNSPEC_COND_LE	; Used in aarch64-sve.md.
    UNSPEC_COND_EQ	; Used in aarch64-sve.md.
    UNSPEC_COND_NE	; Used in aarch64-sve.md.
    UNSPEC_COND_GE	; Used in aarch64-sve.md.
    UNSPEC_COND_GT	; Used in aarch64-sve.md.
    UNSPEC_LASTB	; Used in aarch64-sve.md.
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
    UNSPECV_ATOMIC_LDOP		; Represent an atomic load-operation
    UNSPECV_ATOMIC_LDOP_OR	; Represent an atomic load-or
    UNSPECV_ATOMIC_LDOP_BIC	; Represent an atomic load-bic
    UNSPECV_ATOMIC_LDOP_XOR	; Represent an atomic load-xor
    UNSPECV_ATOMIC_LDOP_PLUS	; Represent an atomic load-add
])

;; -------------------------------------------------------------------
;; Mode attributes
;; -------------------------------------------------------------------

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
(define_mode_attr sizem1 [(QI "#7") (HI "#15") (SI "#31") (DI "#63")])

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
                         (V2SI "2s") (V4SI  "4s")
                         (DI   "1d") (DF    "1d")
                         (V2DI "2d") (V2SF "2s")
			 (V4SF "4s") (V2DF "2d")
			 (V4HF "4h") (V8HF "8h")])

(define_mode_attr Vrevsuff [(V4HI "16") (V8HI "16") (V2SI "32")
                            (V4SI "32") (V2DI "64")])

(define_mode_attr Vmtype [(V8QI ".8b") (V16QI ".16b")
			 (V4HI ".4h") (V8HI  ".8h")
			 (V2SI ".2s") (V4SI  ".4s")
			 (V2DI ".2d") (V4HF ".4h")
			 (V8HF ".8h") (V2SF ".2s")
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
(define_mode_attr Vetype [(V8QI "b") (V16QI "b") (VNx16QI "b") (VNx16BI "b")
			  (V4HI "h") (V8HI  "h") (VNx8HI  "h") (VNx8BI  "h")
			  (V2SI "s") (V4SI  "s") (VNx4SI  "s") (VNx4BI  "s")
			  (V2DI "d")             (VNx2DI  "d") (VNx2BI  "d")
			  (V4HF "h") (V8HF  "h") (VNx8HF  "h")
			  (V2SF "s") (V4SF  "s") (VNx4SF  "s")
			  (V2DF "d")             (VNx2DF  "d")
			  (HF   "h")
			  (SF   "s") (DF  "d")
			  (QI "b")   (HI "h")
			  (SI "s")   (DI "d")])

;; Equivalent of "size" for a vector element.
(define_mode_attr Vesize [(VNx16QI "b")
			  (VNx8HI  "h") (VNx8HF  "h")
			  (VNx4SI  "w") (VNx4SF  "w")
			  (VNx2DI  "d") (VNx2DF  "d")
			  (VNx32QI "b") (VNx48QI "b") (VNx64QI "b")
			  (VNx16HI "h") (VNx24HI "h") (VNx32HI "h")
			  (VNx16HF "h") (VNx24HF "h") (VNx32HF "h")
			  (VNx8SI  "w") (VNx12SI "w") (VNx16SI "w")
			  (VNx8SF  "w") (VNx12SF "w") (VNx16SF "w")
			  (VNx4DI  "d") (VNx6DI  "d") (VNx8DI  "d")
			  (VNx4DF  "d") (VNx6DF  "d") (VNx8DF  "d")])

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
			  (SI   "8b")])

;; Define element mode for each vector mode.
(define_mode_attr VEL [(V8QI  "QI") (V16QI "QI") (VNx16QI "QI")
			(V4HI "HI") (V8HI  "HI") (VNx8HI  "HI")
			(V2SI "SI") (V4SI  "SI") (VNx4SI  "SI")
			(DI   "DI") (V2DI  "DI") (VNx2DI  "DI")
			(V4HF "HF") (V8HF  "HF") (VNx8HF  "HF")
			(V2SF "SF") (V4SF  "SF") (VNx4SF  "SF")
			(DF   "DF") (V2DF  "DF") (VNx2DF  "DF")
			(SI   "SI") (HI    "HI")
			(QI   "QI")])

;; Define element mode for each vector mode (lower case).
(define_mode_attr Vel [(V8QI "qi") (V16QI "qi") (VNx16QI "qi")
			(V4HI "hi") (V8HI "hi") (VNx8HI  "hi")
			(V2SI "si") (V4SI "si") (VNx4SI  "si")
			(DI "di")   (V2DI "di") (VNx2DI  "di")
			(V4HF "hf") (V8HF "hf") (VNx8HF  "hf")
			(V2SF "sf") (V4SF "sf") (VNx4SF  "sf")
			(V2DF "df") (DF "df")   (VNx2DF  "df")
			(SI   "si") (HI   "hi")
			(QI   "qi")])

;; Element mode with floating-point values replaced by like-sized integers.
(define_mode_attr VEL_INT [(VNx16QI "QI")
			   (VNx8HI  "HI") (VNx8HF "HI")
			   (VNx4SI  "SI") (VNx4SF "SI")
			   (VNx2DI  "DI") (VNx2DF "DI")])

;; Gives the mode of the 128-bit lowpart of an SVE vector.
(define_mode_attr V128 [(VNx16QI "V16QI")
			(VNx8HI  "V8HI") (VNx8HF "V8HF")
			(VNx4SI  "V4SI") (VNx4SF "V4SF")
			(VNx2DI  "V2DI") (VNx2DF "V2DF")])

;; ...and again in lower case.
(define_mode_attr v128 [(VNx16QI "v16qi")
			(VNx8HI  "v8hi") (VNx8HF "v8hf")
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
			 (V8HF "V4HF")  (V2DF  "DF")])

;; Half modes of all vector modes, in lower-case.
(define_mode_attr Vhalf [(V8QI "v4qi")  (V16QI "v8qi")
			 (V4HI "v2hi")  (V8HI  "v4hi")
			 (V2SI "si")    (V4SI  "v2si")
			 (V2DI "di")    (V2SF  "sf")
			 (V4SF "v2sf")  (V2DF  "df")])

;; Double modes of vector modes.
(define_mode_attr VDBL [(V8QI "V16QI") (V4HI "V8HI")
			(V4HF "V8HF")
			(V2SI "V4SI")  (V2SF "V4SF")
			(SI   "V2SI")  (DI   "V2DI")
			(DF   "V2DF")])

;; Register suffix for double-length mode.
(define_mode_attr Vdtype [(V4HF "8h") (V2SF "4s")])

;; Double modes of vector modes (lower case).
(define_mode_attr Vdbl [(V8QI "v16qi") (V4HI "v8hi")
			(V4HF "v8hf")
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

;; Narrowed quad-modes for VQN (Used for XTN2).
(define_mode_attr VNARROWQ2 [(V8HI "V16QI") (V4SI "V8HI")
			     (V2DI "V4SI")])

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

;; SVE vector after widening
(define_mode_attr Vewtype [(VNx16QI "h")
			   (VNx8HI  "s") (VNx8HF "s")
			   (VNx4SI  "d") (VNx4SF "d")])

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
(define_mode_attr vw [(V8QI "w") (V16QI "w") (VNx16QI "w")
		      (V4HI "w") (V8HI "w") (VNx8HI "w")
		      (V2SI "w") (V4SI "w") (VNx4SI "w")
		      (DI   "x") (V2DI "x") (VNx2DI "x")
		      (VNx8HF "h")
		      (V2SF "s") (V4SF "s") (VNx4SF "s")
		      (V2DF "d") (VNx2DF "d")])

;; Corresponding core element mode for each vector mode.  This is a
;; variation on <vw> mapping FP modes to GP regs.
(define_mode_attr vwcore [(V8QI "w") (V16QI "w") (VNx16QI "w")
			  (V4HI "w") (V8HI "w") (VNx8HI "w")
			  (V2SI "w") (V4SI "w") (VNx4SI "w")
			  (DI   "x") (V2DI "x") (VNx2DI "x")
			  (V4HF "w") (V8HF "w") (VNx8HF "w")
			  (V2SF "w") (V4SF "w") (VNx4SF "w")
			  (V2DF "x") (VNx2DF "x")])

;; Double vector types for ALLX.
(define_mode_attr Vallxd [(QI "8b") (HI "4h") (SI "2s")])

;; Mode with floating-point values replaced by like-sized integers.
(define_mode_attr V_INT_EQUIV [(V8QI "V8QI") (V16QI "V16QI")
			       (V4HI "V4HI") (V8HI  "V8HI")
			       (V2SI "V2SI") (V4SI  "V4SI")
			       (DI   "DI")   (V2DI  "V2DI")
			       (V4HF "V4HI") (V8HF  "V8HI")
			       (V2SF "V2SI") (V4SF  "V4SI")
			       (DF   "DI")   (V2DF  "V2DI")
			       (SF   "SI")   (SI    "SI")
			       (HF    "HI")
			       (VNx16QI "VNx16QI")
			       (VNx8HI  "VNx8HI") (VNx8HF "VNx8HI")
			       (VNx4SI  "VNx4SI") (VNx4SF "VNx4SI")
			       (VNx2DI  "VNx2DI") (VNx2DF "VNx2DI")
])

;; Lower case mode with floating-point values replaced by like-sized integers.
(define_mode_attr v_int_equiv [(V8QI "v8qi") (V16QI "v16qi")
			       (V4HI "v4hi") (V8HI  "v8hi")
			       (V2SI "v2si") (V4SI  "v4si")
			       (DI   "di")   (V2DI  "v2di")
			       (V4HF "v4hi") (V8HF  "v8hi")
			       (V2SF "v2si") (V4SF  "v4si")
			       (DF   "di")   (V2DF  "v2di")
			       (SF   "si")
			       (VNx16QI "vnx16qi")
			       (VNx8HI  "vnx8hi") (VNx8HF "vnx8hi")
			       (VNx4SI  "vnx4si") (VNx4SF "vnx4si")
			       (VNx2DI  "vnx2di") (VNx2DF "vnx2di")
])

;; Floating-point equivalent of selected modes.
(define_mode_attr V_FP_EQUIV [(VNx4SI "VNx4SF") (VNx4SF "VNx4SF")
			      (VNx2DI "VNx2DF") (VNx2DF "VNx2DF")])
(define_mode_attr v_fp_equiv [(VNx4SI "vnx4sf") (VNx4SF "vnx4sf")
			      (VNx2DI "vnx2df") (VNx2DF "vnx2df")])

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
		     (V2SI "") (V4SI  "_q")
		     (DI   "") (V2DI  "_q")
		     (V4HF "") (V8HF "_q")
		     (V2SF "") (V4SF  "_q")
			       (V2DF  "_q")
		     (QI "") (HI "") (SI "") (DI "") (HF "") (SF "") (DF "")])

(define_mode_attr vp [(V8QI "v") (V16QI "v")
		      (V4HI "v") (V8HI  "v")
		      (V2SI "p") (V4SI  "v")
		      (V2DI "p") (V2DF  "p")
		      (V2SF "p") (V4SF  "v")
		      (V4HF "v") (V8HF  "v")])

(define_mode_attr vsi2qi [(V2SI "v8qi") (V4SI "v16qi")])
(define_mode_attr VSI2QI [(V2SI "V8QI") (V4SI "V16QI")])


;; Register suffix for DOTPROD input types from the return type.
(define_mode_attr Vdottype [(V2SI "8b") (V4SI "16b")])

;; Sum of lengths of instructions needed to move vector registers of a mode.
(define_mode_attr insn_count [(OI "8") (CI "12") (XI "16")])

;; -fpic small model GOT reloc modifers: gotpage_lo15/lo14 for ILP64/32.
;; No need of iterator for -fPIC as it use got_lo12 for both modes.
(define_mode_attr got_modifier [(SI "gotpage_lo14") (DI "gotpage_lo15")])

;; Width of 2nd and 3rd arguments to fp16 vector multiply add/sub
(define_mode_attr VFMLA_W [(V2SF "V4HF") (V4SF "V8HF")])

(define_mode_attr VFMLA_SEL_W [(V2SF "V2HF") (V4SF "V4HF")])

(define_mode_attr f16quad [(V2SF "") (V4SF "q")])

(define_code_attr f16mac [(plus "a") (minus "s")])

;; The number of subvectors in an SVE_STRUCT.
(define_mode_attr vector_count [(VNx32QI "2") (VNx16HI "2")
				(VNx8SI  "2") (VNx4DI  "2")
				(VNx16HF "2") (VNx8SF  "2") (VNx4DF "2")
				(VNx48QI "3") (VNx24HI "3")
				(VNx12SI "3") (VNx6DI  "3")
				(VNx24HF "3") (VNx12SF "3") (VNx6DF "3")
				(VNx64QI "4") (VNx32HI "4")
				(VNx16SI "4") (VNx8DI  "4")
				(VNx32HF "4") (VNx16SF "4") (VNx8DF "4")])

;; The number of instruction bytes needed for an SVE_STRUCT move.  This is
;; equal to vector_count * 4.
(define_mode_attr insn_length [(VNx32QI "8")  (VNx16HI "8")
			       (VNx8SI  "8")  (VNx4DI  "8")
			       (VNx16HF "8")  (VNx8SF  "8")  (VNx4DF "8")
			       (VNx48QI "12") (VNx24HI "12")
			       (VNx12SI "12") (VNx6DI  "12")
			       (VNx24HF "12") (VNx12SF "12") (VNx6DF "12")
			       (VNx64QI "16") (VNx32HI "16")
			       (VNx16SI "16") (VNx8DI  "16")
			       (VNx32HF "16") (VNx16SF "16") (VNx8DF "16")])

;; The type of a subvector in an SVE_STRUCT.
(define_mode_attr VSINGLE [(VNx32QI "VNx16QI")
			   (VNx16HI "VNx8HI") (VNx16HF "VNx8HF")
			   (VNx8SI "VNx4SI") (VNx8SF "VNx4SF")
			   (VNx4DI "VNx2DI") (VNx4DF "VNx2DF")
			   (VNx48QI "VNx16QI")
			   (VNx24HI "VNx8HI") (VNx24HF "VNx8HF")
			   (VNx12SI "VNx4SI") (VNx12SF "VNx4SF")
			   (VNx6DI "VNx2DI") (VNx6DF "VNx2DF")
			   (VNx64QI "VNx16QI")
			   (VNx32HI "VNx8HI") (VNx32HF "VNx8HF")
			   (VNx16SI "VNx4SI") (VNx16SF "VNx4SF")
			   (VNx8DI "VNx2DI") (VNx8DF "VNx2DF")])

;; ...and again in lower case.
(define_mode_attr vsingle [(VNx32QI "vnx16qi")
			   (VNx16HI "vnx8hi") (VNx16HF "vnx8hf")
			   (VNx8SI "vnx4si") (VNx8SF "vnx4sf")
			   (VNx4DI "vnx2di") (VNx4DF "vnx2df")
			   (VNx48QI "vnx16qi")
			   (VNx24HI "vnx8hi") (VNx24HF "vnx8hf")
			   (VNx12SI "vnx4si") (VNx12SF "vnx4sf")
			   (VNx6DI "vnx2di") (VNx6DF "vnx2df")
			   (VNx64QI "vnx16qi")
			   (VNx32HI "vnx8hi") (VNx32HF "vnx8hf")
			   (VNx16SI "vnx4si") (VNx16SF "vnx4sf")
			   (VNx8DI "vnx2di") (VNx8DF "vnx2df")])

;; The predicate mode associated with an SVE data mode.  For structure modes
;; this is equivalent to the <VPRED> of the subvector mode.
(define_mode_attr VPRED [(VNx16QI "VNx16BI")
			 (VNx8HI "VNx8BI") (VNx8HF "VNx8BI")
			 (VNx4SI "VNx4BI") (VNx4SF "VNx4BI")
			 (VNx2DI "VNx2BI") (VNx2DF "VNx2BI")
			 (VNx32QI "VNx16BI")
			 (VNx16HI "VNx8BI") (VNx16HF "VNx8BI")
			 (VNx8SI "VNx4BI") (VNx8SF "VNx4BI")
			 (VNx4DI "VNx2BI") (VNx4DF "VNx2BI")
			 (VNx48QI "VNx16BI")
			 (VNx24HI "VNx8BI") (VNx24HF "VNx8BI")
			 (VNx12SI "VNx4BI") (VNx12SF "VNx4BI")
			 (VNx6DI "VNx2BI") (VNx6DF "VNx2BI")
			 (VNx64QI "VNx16BI")
			 (VNx32HI "VNx8BI") (VNx32HF "VNx8BI")
			 (VNx16SI "VNx4BI") (VNx16SF "VNx4BI")
			 (VNx8DI "VNx2BI") (VNx8DF "VNx2BI")])

;; ...and again in lower case.
(define_mode_attr vpred [(VNx16QI "vnx16bi")
			 (VNx8HI "vnx8bi") (VNx8HF "vnx8bi")
			 (VNx4SI "vnx4bi") (VNx4SF "vnx4bi")
			 (VNx2DI "vnx2bi") (VNx2DF "vnx2bi")
			 (VNx32QI "vnx16bi")
			 (VNx16HI "vnx8bi") (VNx16HF "vnx8bi")
			 (VNx8SI "vnx4bi") (VNx8SF "vnx4bi")
			 (VNx4DI "vnx2bi") (VNx4DF "vnx2bi")
			 (VNx48QI "vnx16bi")
			 (VNx24HI "vnx8bi") (VNx24HF "vnx8bi")
			 (VNx12SI "vnx4bi") (VNx12SF "vnx4bi")
			 (VNx6DI "vnx2bi") (VNx6DF "vnx2bi")
			 (VNx64QI "vnx16bi")
			 (VNx32HI "vnx8bi") (VNx32HF "vnx4bi")
			 (VNx16SI "vnx4bi") (VNx16SF "vnx4bi")
			 (VNx8DI "vnx2bi") (VNx8DF "vnx2bi")])

;; -------------------------------------------------------------------
;; Code Iterators
;; -------------------------------------------------------------------

;; This code iterator allows the various shifts supported on the core
(define_code_iterator SHIFT [ashift ashiftrt lshiftrt rotatert])

;; This code iterator allows the shifts supported in arithmetic instructions
(define_code_iterator ASHIFT [ashift ashiftrt lshiftrt])

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

;; Code iterator for variants of vector max and min.
(define_code_iterator ADDSUB [plus minus])

;; Code iterator for variants of vector saturating binary ops.
(define_code_iterator BINQOPS [ss_plus us_plus ss_minus us_minus])

;; Code iterator for variants of vector saturating unary ops.
(define_code_iterator UNQOPS [ss_neg ss_abs])

;; Code iterator for signed variants of vector saturating binary ops.
(define_code_iterator SBINQOPS [ss_plus ss_minus])

;; Comparison operators for <F>CM.
(define_code_iterator COMPARISONS [lt le eq ge gt])

;; Unsigned comparison operators.
(define_code_iterator UCOMPARISONS [ltu leu geu gtu])

;; Unsigned comparison operators.
(define_code_iterator FAC_COMPARISONS [lt le ge gt])

;; SVE integer unary operations.
(define_code_iterator SVE_INT_UNARY [neg not popcount])

;; SVE floating-point unary operations.
(define_code_iterator SVE_FP_UNARY [neg abs sqrt])

;; SVE integer binary operations.
(define_code_iterator SVE_INT_BINARY [plus minus mult smax umax smin umin
				      and ior xor])

;; SVE integer binary division operations.
(define_code_iterator SVE_INT_BINARY_SD [div udiv])

;; SVE integer comparisons.
(define_code_iterator SVE_INT_CMP [lt le eq ne ge gt ltu leu geu gtu])

;; SVE floating-point comparisons.
(define_code_iterator SVE_FP_CMP [lt le eq ne ge gt])

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
			 (ss_plus "qadd")
			 (us_plus "qadd")
			 (ss_minus "qsub")
			 (us_minus "qsub")
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
			 (abs "abs")
			 (sqrt "sqrt")])

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

;; Map shift operators onto underlying bit-field instructions
(define_code_attr bfshift [(ashift "ubfiz") (ashiftrt "sbfx")
			   (lshiftrt "ubfx") (rotatert "extr")])

;; Logical operator instruction mnemonics
(define_code_attr logical [(and "and") (ior "orr") (xor "eor")])

;; Operation names for negate and bitwise complement.
(define_code_attr neg_not_op [(neg "neg") (not "not")])

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
		      (smin "s") (umin "u")])

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
			      (neg "neg")
			      (smin "smin")
			      (smax "smax")
			      (umin "umin")
			      (umax "umax")
			      (and "and")
			      (ior "orr")
			      (xor "eor")
			      (not "not")
			      (popcount "cnt")])

(define_code_attr sve_int_op_rev [(plus "add")
			          (minus "subr")
			          (mult "mul")
			          (div "sdivr")
			          (udiv "udivr")
			          (smin "smin")
			          (smax "smax")
			          (umin "umin")
			          (umax "umax")
			          (and "and")
			          (ior "orr")
			          (xor "eor")])

;; The floating-point SVE instruction that implements an rtx code.
(define_code_attr sve_fp_op [(plus "fadd")
			     (neg "fneg")
			     (abs "fabs")
			     (sqrt "fsqrt")])

;; The SVE immediate constraint to use for an rtl code.
(define_code_attr sve_imm_con [(eq "vsc")
			       (ne "vsc")
			       (lt "vsc")
			       (ge "vsc")
			       (le "vsc")
			       (gt "vsc")
			       (ltu "vsd")
			       (leu "vsd")
			       (geu "vsd")
			       (gtu "vsd")])

;; -------------------------------------------------------------------
;; Int Iterators.
;; -------------------------------------------------------------------

;; The unspec codes for the SABAL, UABAL AdvancedSIMD instructions.
(define_int_iterator ABAL [UNSPEC_SABAL UNSPEC_UABAL])

;; The unspec codes for the SABDL2, UABDL2 AdvancedSIMD instructions.
(define_int_iterator ABDL2 [UNSPEC_SABDL2 UNSPEC_UABDL2])

;; The unspec codes for the SADALP, UADALP AdvancedSIMD instructions.
(define_int_iterator ADALP [UNSPEC_SADALP UNSPEC_UADALP])

(define_int_iterator MAXMINV [UNSPEC_UMAXV UNSPEC_UMINV
			      UNSPEC_SMAXV UNSPEC_SMINV])

(define_int_iterator FMAXMINV [UNSPEC_FMAXV UNSPEC_FMINV
			       UNSPEC_FMAXNMV UNSPEC_FMINNMV])

(define_int_iterator BITWISEV [UNSPEC_ANDV UNSPEC_IORV UNSPEC_XORV])

(define_int_iterator LOGICALF [UNSPEC_ANDF UNSPEC_IORF UNSPEC_XORF])

(define_int_iterator HADDSUB [UNSPEC_SHADD UNSPEC_UHADD
			      UNSPEC_SRHADD UNSPEC_URHADD
			      UNSPEC_SHSUB UNSPEC_UHSUB
			      UNSPEC_SRHSUB UNSPEC_URHSUB])

(define_int_iterator HADD [UNSPEC_SHADD UNSPEC_UHADD])

(define_int_iterator RHADD [UNSPEC_SRHADD UNSPEC_URHADD])

(define_int_iterator DOTPROD [UNSPEC_SDOT UNSPEC_UDOT])

(define_int_iterator ADDSUBHN [UNSPEC_ADDHN UNSPEC_RADDHN
			       UNSPEC_SUBHN UNSPEC_RSUBHN])

(define_int_iterator ADDSUBHN2 [UNSPEC_ADDHN2 UNSPEC_RADDHN2
			        UNSPEC_SUBHN2 UNSPEC_RSUBHN2])

(define_int_iterator FMAXMIN_UNS [UNSPEC_FMAX UNSPEC_FMIN
				  UNSPEC_FMAXNM UNSPEC_FMINNM])

(define_int_iterator PAUTH_LR_SP [UNSPEC_PACISP UNSPEC_AUTISP])

(define_int_iterator PAUTH_17_16 [UNSPEC_PACI1716 UNSPEC_AUTI1716])

(define_int_iterator VQDMULH [UNSPEC_SQDMULH UNSPEC_SQRDMULH])

(define_int_iterator USSUQADD [UNSPEC_SUQADD UNSPEC_USQADD])

(define_int_iterator SUQMOVN [UNSPEC_SQXTN UNSPEC_UQXTN])

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

(define_int_iterator FRECP [UNSPEC_FRECPE UNSPEC_FRECPX])

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

(define_int_iterator SVE_COND_FP_BINARY [UNSPEC_COND_ADD UNSPEC_COND_SUB
					 UNSPEC_COND_MUL UNSPEC_COND_DIV
					 UNSPEC_COND_MAX UNSPEC_COND_MIN])

(define_int_iterator SVE_COND_FP_TERNARY [UNSPEC_COND_FMLA
					  UNSPEC_COND_FMLS
					  UNSPEC_COND_FNMLA
					  UNSPEC_COND_FNMLS])

(define_int_iterator SVE_COND_FP_CMP [UNSPEC_COND_LT UNSPEC_COND_LE
				      UNSPEC_COND_EQ UNSPEC_COND_NE
				      UNSPEC_COND_GE UNSPEC_COND_GT])

;; Iterators for atomic operations.

(define_int_iterator ATOMIC_LDOP
 [UNSPECV_ATOMIC_LDOP_OR UNSPECV_ATOMIC_LDOP_BIC
  UNSPECV_ATOMIC_LDOP_XOR UNSPECV_ATOMIC_LDOP_PLUS])

(define_int_attr atomic_ldop
 [(UNSPECV_ATOMIC_LDOP_OR "set") (UNSPECV_ATOMIC_LDOP_BIC "clr")
  (UNSPECV_ATOMIC_LDOP_XOR "eor") (UNSPECV_ATOMIC_LDOP_PLUS "add")])

;; -------------------------------------------------------------------
;; Int Iterators Attributes.
;; -------------------------------------------------------------------

;; The optab associated with an operation.  Note that for ANDF, IORF
;; and XORF, the optab pattern is not actually defined; we just use this
;; name for consistency with the integer patterns.
(define_int_attr optab [(UNSPEC_ANDF "and")
			(UNSPEC_IORF "ior")
			(UNSPEC_XORF "xor")
			(UNSPEC_ANDV "and")
			(UNSPEC_IORV "ior")
			(UNSPEC_XORV "xor")
			(UNSPEC_COND_ADD "add")
			(UNSPEC_COND_SUB "sub")
			(UNSPEC_COND_MUL "mul")
			(UNSPEC_COND_DIV "div")
			(UNSPEC_COND_MAX "smax")
			(UNSPEC_COND_MIN "smin")
			(UNSPEC_COND_FMLA "fma")
			(UNSPEC_COND_FMLS "fnma")
			(UNSPEC_COND_FNMLA "fnms")
			(UNSPEC_COND_FNMLS "fms")])

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
			      (UNSPEC_FMINNM "fmin")])

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

(define_int_attr bit_reduc_op [(UNSPEC_ANDV "andv")
			       (UNSPEC_IORV "orv")
			       (UNSPEC_XORV "eorv")])

;; The SVE logical instruction that implements an unspec.
(define_int_attr logicalf_op [(UNSPEC_ANDF "and")
		 	      (UNSPEC_IORF "orr")
			      (UNSPEC_XORF "eor")])

;; "s" for signed operations and "u" for unsigned ones.
(define_int_attr su [(UNSPEC_UNPACKSHI "s")
		     (UNSPEC_UNPACKUHI "u")
		     (UNSPEC_UNPACKSLO "s")
		     (UNSPEC_UNPACKULO "u")
		     (UNSPEC_SMUL_HIGHPART "s")
		     (UNSPEC_UMUL_HIGHPART "u")])

(define_int_attr sur [(UNSPEC_SHADD "s") (UNSPEC_UHADD "u")
		      (UNSPEC_SRHADD "sr") (UNSPEC_URHADD "ur")
		      (UNSPEC_SHSUB "s") (UNSPEC_UHSUB "u")
		      (UNSPEC_SRHSUB "sr") (UNSPEC_URHSUB "ur")
		      (UNSPEC_ADDHN "") (UNSPEC_RADDHN "r")
		      (UNSPEC_SABAL "s") (UNSPEC_UABAL "u")
		      (UNSPEC_SABDL2 "s") (UNSPEC_UABDL2 "u")
		      (UNSPEC_SADALP "s") (UNSPEC_UADALP "u")
		      (UNSPEC_SUBHN "") (UNSPEC_RSUBHN "r")
		      (UNSPEC_ADDHN2 "") (UNSPEC_RADDHN2 "r")
		      (UNSPEC_SUBHN2 "") (UNSPEC_RSUBHN2 "r")
		      (UNSPEC_SQXTN "s") (UNSPEC_UQXTN "u")
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
])

(define_int_attr r [(UNSPEC_SQDMULH "") (UNSPEC_SQRDMULH "r")
		    (UNSPEC_SQSHRUN "") (UNSPEC_SQRSHRUN "r")
                    (UNSPEC_SQSHRN "")  (UNSPEC_UQSHRN "")
                    (UNSPEC_SQRSHRN "r") (UNSPEC_UQRSHRN "r")
                    (UNSPEC_SQSHL   "")  (UNSPEC_UQSHL  "")
                    (UNSPEC_SQRSHL   "r")(UNSPEC_UQRSHL  "r")
])

(define_int_attr lr [(UNSPEC_SSLI  "l") (UNSPEC_USLI  "l")
		     (UNSPEC_SSRI  "r") (UNSPEC_USRI  "r")])

(define_int_attr u [(UNSPEC_SQSHLU "u") (UNSPEC_SQSHL "") (UNSPEC_UQSHL "")
		    (UNSPEC_SQSHRUN "u") (UNSPEC_SQRSHRUN "u")
		    (UNSPEC_SQSHRN "")  (UNSPEC_UQSHRN "")
		    (UNSPEC_SQRSHRN "") (UNSPEC_UQRSHRN "")
		    (UNSPEC_SHADD "") (UNSPEC_UHADD "u")
		    (UNSPEC_SRHADD "") (UNSPEC_URHADD "u")])

(define_int_attr addsub [(UNSPEC_SHADD "add")
			 (UNSPEC_UHADD "add")
			 (UNSPEC_SRHADD "add")
			 (UNSPEC_URHADD "add")
			 (UNSPEC_SHSUB "sub")
			 (UNSPEC_UHSUB "sub")
			 (UNSPEC_SRHSUB "sub")
			 (UNSPEC_URHSUB "sub")
			 (UNSPEC_ADDHN "add")
			 (UNSPEC_SUBHN "sub")
			 (UNSPEC_RADDHN "add")
			 (UNSPEC_RSUBHN "sub")
			 (UNSPEC_ADDHN2 "add")
			 (UNSPEC_SUBHN2 "sub")
			 (UNSPEC_RADDHN2 "add")
			 (UNSPEC_RSUBHN2 "sub")])

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
(define_int_attr pauth_mnem_prefix [(UNSPEC_PACISP "paci")
				    (UNSPEC_AUTISP "auti")
				    (UNSPEC_PACI1716 "paci")
				    (UNSPEC_AUTI1716 "auti")])

;; Pointer authentication HINT number for NOP space instructions using A Key.
(define_int_attr pauth_hint_num_a [(UNSPEC_PACISP "25")
				    (UNSPEC_AUTISP "29")
				    (UNSPEC_PACI1716 "8")
				    (UNSPEC_AUTI1716 "12")])

(define_int_attr perm_insn [(UNSPEC_ZIP1 "zip") (UNSPEC_ZIP2 "zip")
			    (UNSPEC_TRN1 "trn") (UNSPEC_TRN2 "trn")
			    (UNSPEC_UZP1 "uzp") (UNSPEC_UZP2 "uzp")])

; op code for REV instructions (size within which elements are reversed).
(define_int_attr rev_op [(UNSPEC_REV64 "64") (UNSPEC_REV32 "32")
			 (UNSPEC_REV16 "16")])

(define_int_attr perm_hilo [(UNSPEC_ZIP1 "1") (UNSPEC_ZIP2 "2")
			    (UNSPEC_TRN1 "1") (UNSPEC_TRN2 "2")
			    (UNSPEC_UZP1 "1") (UNSPEC_UZP2 "2")
			    (UNSPEC_UNPACKSHI "hi") (UNSPEC_UNPACKUHI "hi")
			    (UNSPEC_UNPACKSLO "lo") (UNSPEC_UNPACKULO "lo")])

;; Return true if the associated optab refers to the high-numbered lanes,
;; false if it refers to the low-numbered lanes.  The convention is for
;; "hi" to refer to the low-numbered lanes (the first ones in memory)
;; for big-endian.
(define_int_attr hi_lanes_optab [(UNSPEC_UNPACKSHI "!BYTES_BIG_ENDIAN")
				 (UNSPEC_UNPACKUHI "!BYTES_BIG_ENDIAN")
				 (UNSPEC_UNPACKSLO "BYTES_BIG_ENDIAN")
				 (UNSPEC_UNPACKULO "BYTES_BIG_ENDIAN")])

(define_int_attr frecp_suffix  [(UNSPEC_FRECPE "e") (UNSPEC_FRECPX "x")])

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

;; The condition associated with an UNSPEC_COND_<xx>.
(define_int_attr cmp_op [(UNSPEC_COND_LT "lt")
			 (UNSPEC_COND_LE "le")
			 (UNSPEC_COND_EQ "eq")
			 (UNSPEC_COND_NE "ne")
			 (UNSPEC_COND_GE "ge")
			 (UNSPEC_COND_GT "gt")])

(define_int_attr sve_fp_op [(UNSPEC_COND_ADD "fadd")
			    (UNSPEC_COND_SUB "fsub")
			    (UNSPEC_COND_MUL "fmul")
			    (UNSPEC_COND_DIV "fdiv")
			    (UNSPEC_COND_MAX "fmaxnm")
			    (UNSPEC_COND_MIN "fminnm")])

(define_int_attr sve_fp_op_rev [(UNSPEC_COND_ADD "fadd")
			        (UNSPEC_COND_SUB "fsubr")
			        (UNSPEC_COND_MUL "fmul")
			        (UNSPEC_COND_DIV "fdivr")
			        (UNSPEC_COND_MAX "fmaxnm")
			        (UNSPEC_COND_MIN "fminnm")])

(define_int_attr sve_fmla_op [(UNSPEC_COND_FMLA "fmla")
			      (UNSPEC_COND_FMLS "fmls")
			      (UNSPEC_COND_FNMLA "fnmla")
			      (UNSPEC_COND_FNMLS "fnmls")])

(define_int_attr sve_fmad_op [(UNSPEC_COND_FMLA "fmad")
			      (UNSPEC_COND_FMLS "fmsb")
			      (UNSPEC_COND_FNMLA "fnmad")
			      (UNSPEC_COND_FNMLS "fnmsb")])

(define_int_attr commutative [(UNSPEC_COND_ADD "true")
			      (UNSPEC_COND_SUB "false")
			      (UNSPEC_COND_MUL "true")
			      (UNSPEC_COND_DIV "false")
			      (UNSPEC_COND_MIN "true")
			      (UNSPEC_COND_MAX "true")])
