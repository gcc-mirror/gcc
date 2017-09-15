;; Code and mode itertator and attribute definitions for the ARM backend
;; Copyright (C) 2010-2017 Free Software Foundation, Inc.
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
(define_mode_iterator ANY64 [DI DF V8QI V4HI V2SI V2SF])

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
(define_mode_iterator VDX [V8QI V4HI V4HF V2SI V2SF DI])

;; Double-width vector modes, with V4HF - for vldN_lane and vstN_lane.
(define_mode_iterator VD_LANE [V8QI V4HI V4HF V2SI V2SF])

;; Double-width vector modes without floating-point elements.
(define_mode_iterator VDI [V8QI V4HI V2SI])

;; Quad-width vector modes supporting arithmetic (no HF!).
(define_mode_iterator VQ [V16QI V8HI V4SI V4SF])

;; Quad-width vector modes, including V8HF.
(define_mode_iterator VQ2 [V16QI V8HI V8HF V4SI V4SF])

;; Quad-width vector modes with 16- or 32-bit elements
(define_mode_iterator VQ_HS [V8HI V8HF V4SI V4SF])

;; Quad-width vector modes plus 64-bit elements.
(define_mode_iterator VQX [V16QI V8HI V8HF V4SI V4SF V2DI])

;; Quad-width vector modes without floating-point elements.
(define_mode_iterator VQI [V16QI V8HI V4SI])

;; Quad-width vector modes, with TImode added, for moves.
(define_mode_iterator VQXMOV [V16QI V8HI V8HF V4SI V4SF V2DI TI])

;; Opaque structure types wider than TImode.
(define_mode_iterator VSTRUCT [EI OI CI XI])

;; Opaque structure types used in table lookups (except vtbl1/vtbx1).
(define_mode_iterator VTAB [TI EI OI])

;; Widenable modes.
(define_mode_iterator VW [V8QI V4HI V2SI])

;; Narrowable modes.
(define_mode_iterator VN [V8HI V4SI V2DI])

;; All supported vector modes (except singleton DImode).
(define_mode_iterator VDQ [V8QI V16QI V4HI V8HI V2SI V4SI V4HF V8HF V2SF V4SF V2DI])

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
			    V4HF V8HF V2SF V4SF DI V2DI])

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

;; Modes with 64-bit elements only.
(define_mode_iterator V64 [DI V2DI])

;; Modes with 32-bit elements only.
(define_mode_iterator V32 [V2SI V2SF V4SI V4SF])

;; Modes with 8-bit, 16-bit and 32-bit elements.
(define_mode_iterator VU [V16QI V8HI V4SI])

;; Vector modes for 16-bit floating-point support.
(define_mode_iterator VH [V8HF V4HF])

;; Iterators used for fixed-point support.
(define_mode_iterator FIXED [QQ HQ SQ UQQ UHQ USQ HA SA UHA USA])

(define_mode_iterator ADDSUB [V4QQ V2HQ V2HA])

(define_mode_iterator UQADDSUB [V4UQQ V2UHQ UQQ UHQ V2UHA UHA])

(define_mode_iterator QADDSUB [V4QQ V2HQ QQ HQ V2HA HA SQ SA])

(define_mode_iterator QMUL [HQ HA])

;; Modes for polynomial or float values.
(define_mode_iterator VPF [V8QI V16QI V2SF V4SF])

;;----------------------------------------------------------------------------
;; Code iterators
;;----------------------------------------------------------------------------

;; A list of condition codes used in compare instructions where
;; the carry flag from the addition is used instead of doing the
;; compare a second time.
(define_code_iterator LTUGEU [ltu geu])

;; The signed gt, ge comparisons
(define_code_iterator GTGE [gt ge])

;; The signed gt, ge, lt, le comparisons
(define_code_iterator GLTE [gt ge lt le])

;; The unsigned gt, ge comparisons
(define_code_iterator GTUGEU [gtu geu])

;; Comparisons for vc<cmp>
(define_code_iterator COMPARISONS [eq gt ge le lt])

;; A list of ...
(define_code_iterator IOR_XOR [ior xor])

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

;; Conversions.
(define_code_iterator FCVT [unsigned_float float])

;; plus and minus are the only SHIFTABLE_OPS for which Thumb2 allows
;; a stack pointer opoerand.  The minus operation is a candidate for an rsub
;; and hence only plus is supported.
(define_code_attr t2_binop0
  [(plus "rk") (minus "r") (ior "r") (xor "r") (and "r")])

;; The instruction to use when a SHIFTABLE_OPS has a shift operation as
;; its first operand.
(define_code_attr arith_shift_insn
  [(plus "add") (minus "rsb") (ior "orr") (xor "eor") (and "and")])

(define_code_attr cmp_op [(eq "eq") (gt "gt") (ge "ge") (lt "lt") (le "le")
                          (gtu "gt") (geu "ge")])

(define_code_attr cmp_type [(eq "i") (gt "s") (ge "s") (lt "s") (le "s")])

;;----------------------------------------------------------------------------
;; Int iterators
;;----------------------------------------------------------------------------

(define_int_iterator VRINT [UNSPEC_VRINTZ UNSPEC_VRINTP UNSPEC_VRINTM
                            UNSPEC_VRINTR UNSPEC_VRINTX UNSPEC_VRINTA])

(define_int_iterator NEON_VCMP [UNSPEC_VCEQ UNSPEC_VCGT UNSPEC_VCGE
				UNSPEC_VCLT UNSPEC_VCLE])

(define_int_iterator NEON_VACMP [UNSPEC_VCAGE UNSPEC_VCAGT])

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

(define_int_iterator CRYPTO_UNARY [UNSPEC_AESMC UNSPEC_AESIMC])

(define_int_iterator CRYPTO_BINARY [UNSPEC_AESD UNSPEC_AESE
                                    UNSPEC_SHA1SU1 UNSPEC_SHA256SU0])

(define_int_iterator CRYPTO_TERNARY [UNSPEC_SHA1SU0 UNSPEC_SHA256H
                                     UNSPEC_SHA256H2 UNSPEC_SHA256SU1])

(define_int_iterator CRYPTO_SELECTING [UNSPEC_SHA1C UNSPEC_SHA1M
                                       UNSPEC_SHA1P])

(define_int_iterator VQRDMLH_AS [UNSPEC_VQRDMLAH UNSPEC_VQRDMLSH])

(define_int_iterator VFM_LANE_AS [UNSPEC_VFMA_LANE UNSPEC_VFMS_LANE])

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

;; (Opposite) mode to convert to/from for NEON mode conversions.
(define_mode_attr V_CVTTO [(V2SI "V2SF") (V2SF "V2SI")
               (V4SI "V4SF") (V4SF "V4SI")])

;; As above but in lower case.
(define_mode_attr V_cvtto [(V2SI "v2sf") (V2SF "v2si")
                           (V4SI "v4sf") (V4SF "v4si")])

;; (Opposite) mode to convert to/from for vector-half mode conversions.
(define_mode_attr VH_CVTTO [(V4HI "V4HF") (V4HF "V4HI")
			    (V8HI "V8HF") (V8HF "V8HI")])

;; Define element mode for each vector mode.
(define_mode_attr V_elem [(V8QI "QI") (V16QI "QI")
			  (V4HI "HI") (V8HI "HI")
			  (V4HF "HF") (V8HF "HF")
                          (V2SI "SI") (V4SI "SI")
                          (V2SF "SF") (V4SF "SF")
                          (DI "DI")   (V2DI "DI")])

;; As above but in lower case.
(define_mode_attr V_elem_l [(V8QI "qi") (V16QI "qi")
			    (V4HI "hi") (V8HI "hi")
			    (V4HF "hf") (V8HF "hf")
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
                              (V2SI "V2SI") (V4SI "V2SI")
                              (V2SF "V2SF") (V4SF "V2SF")
                              (DI "V2DI")   (V2DI "V2DI")])

;; Similar, for three elements.
(define_mode_attr V_three_elem [(V8QI "BLK") (V16QI "BLK")
                                (V4HI "BLK") (V8HI "BLK")
                                (V4HF "BLK") (V8HF "BLK")
                                (V2SI "BLK") (V4SI "BLK")
                                (V2SF "BLK") (V4SF "BLK")
                                (DI "EI")    (V2DI "EI")])

;; Similar, for four elements.
(define_mode_attr V_four_elem [(V8QI "SI")   (V16QI "SI")
                               (V4HI "V4HI") (V8HI "V4HI")
                               (V4HF "V4HF") (V8HF "V4HF")
                               (V2SI "V4SI") (V4SI "V4SI")
                               (V2SF "V4SF") (V4SF "V4SF")
                               (DI "OI")     (V2DI "OI")])

;; Register width from element mode
(define_mode_attr V_reg [(V8QI "P") (V16QI "q")
			 (V4HI "P") (V8HI  "q")
			 (V4HF "P") (V8HF  "q")
			 (V2SI "P") (V4SI  "q")
			 (V2SF "P") (V4SF  "q")
			 (DI   "P") (V2DI  "q")
			 (SF   "")  (DF    "P")
			 (HF   "")])

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
			  (V2DI "DI") (V4HF "HF")])

;; Same, but lower-case.
(define_mode_attr V_half [(V16QI "v8qi") (V8HI "v4hi")
              (V4SI  "v2si") (V4SF "v2sf")
                          (V2DI "di")])

;; Modes with twice the number of equal-sized elements.
(define_mode_attr V_DOUBLE [(V8QI "V16QI") (V4HI "V8HI")
			    (V2SI "V4SI") (V4HF "V8HF")
			    (V2SF "V4SF") (DF "V2DF")
			    (DI "V2DI")])

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
					(V8HF "V4HF") (V4HF "V4HF")])

;; Mode of result of comparison operations (and bit-select operand 1).
(define_mode_attr V_cmp_result [(V8QI "V8QI") (V16QI "V16QI")
				(V4HI "V4HI") (V8HI  "V8HI")
                                (V2SI "V2SI") (V4SI  "V4SI")
				(V4HF "V4HI") (V8HF  "V8HI")
                                (V2SF "V2SI") (V4SF  "V4SI")
                                (DI   "DI")   (V2DI  "V2DI")])

(define_mode_attr v_cmp_result [(V8QI "v8qi") (V16QI "v16qi")
				(V4HI "v4hi") (V8HI  "v8hi")
				(V2SI "v2si") (V4SI  "v4si")
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
                             (V2SF "32") (V4SF "32")])

(define_mode_attr V_sz_elem [(V8QI "8")  (V16QI "8")
			     (V4HI "16") (V8HI  "16")
			     (V2SI "32") (V4SI  "32")
			     (DI   "64") (V2DI  "64")
			     (V4HF "16") (V8HF "16")
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
			    (V4HF "true") (V8HF  "false")])

(define_mode_attr V_mode_nunits [(V8QI "8") (V16QI "16")
				 (V4HF "4") (V8HF "8")
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
(define_mode_attr qhs_extenddi_cstr [(SI "r,0,r,r,r") (HI "r,0,rm,rm,r") (QI "r,0,rUq,rm,r")])
(define_mode_attr qhs_zextenddi_cstr [(SI "r,0,r,r") (HI "r,0,rm,r") (QI "r,0,rm,r")])

;; Mode attributes used for fixed-point support.
(define_mode_attr qaddsub_suf [(V4UQQ "8") (V2UHQ "16") (UQQ "8") (UHQ "16")
			       (V2UHA "16") (UHA "16")
			       (V4QQ "8") (V2HQ "16") (QQ "8") (HQ "16")
			       (V2HA "16") (HA "16") (SQ "") (SA "")])

;; Mode attribute for vshll.
(define_mode_attr V_innermode [(V8QI "QI") (V4HI "HI") (V2SI "SI")])

;; Mode attributes used for VFP support.
(define_mode_attr F_constraint [(SF "t") (DF "w")])
(define_mode_attr vfp_type [(SF "s") (DF "d")])
(define_mode_attr vfp_double_cond [(SF "") (DF "&& TARGET_VFP_DOUBLE")])

;; Mode attribute used to build the "type" attribute.
(define_mode_attr q [(V8QI "") (V16QI "_q")
		     (V4HI "") (V8HI "_q")
		     (V2SI "") (V4SI "_q")
		     (V4HF "") (V8HF "_q")
		     (V2SF "") (V4SF "_q")
		     (V4HF "") (V8HF "_q")
		     (DI "")   (V2DI "_q")
		     (DF "")   (V2DF "_q")
		     (HF "")])

(define_mode_attr pf [(V8QI "p") (V16QI "p") (V2SF "f") (V4SF "f")])

;;----------------------------------------------------------------------------
;; Code attributes
;;----------------------------------------------------------------------------

;; Assembler mnemonics for vqh_ops and vqhs_ops iterators.
(define_code_attr VQH_mnem [(plus "vadd") (smin "vmin") (smax "vmax")
                (umin "vmin") (umax "vmax")])

;; Type attributes for vqh_ops and vqhs_ops iterators.
(define_code_attr VQH_type [(plus "add") (smin "minmax") (smax "minmax")
                (umin "minmax") (umax "minmax")])

;; Signs of above, where relevant.
(define_code_attr VQH_sign [(plus "i") (smin "s") (smax "s") (umin "u")
                (umax "u")])

(define_code_attr cnb [(ltu "CC_C") (geu "CC")])
(define_code_attr optab [(ltu "ltu") (geu "geu")])

;; Assembler mnemonics for signedness of widening operations.
(define_code_attr US [(sign_extend "s") (zero_extend "u")])

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

;;----------------------------------------------------------------------------
;; Int attributes
;;----------------------------------------------------------------------------

;; Mapping between vector UNSPEC operations and the signed ('s'),
;; unsigned ('u'), poly ('p') or float ('f') nature of their data type.
(define_int_attr sup [
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
])

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
