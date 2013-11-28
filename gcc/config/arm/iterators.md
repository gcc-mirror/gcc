;; Code and mode itertator and attribute definitions for the ARM backend
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

;; A list of integer modes that are less than a word
(define_mode_iterator NARROW [QI HI])

;; A list of all the integer modes up to 64bit
(define_mode_iterator QHSD [QI HI SI DI])

;; A list of the 32bit and 64bit integer modes
(define_mode_iterator SIDI [SI DI])

;; A list of modes which the VFP unit can handle
(define_mode_iterator SDF [(SF "TARGET_VFP") (DF "TARGET_VFP_DOUBLE")])

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

;; Double-width vector modes.
(define_mode_iterator VD [V8QI V4HI V2SI V2SF])

;; Double-width vector modes plus 64-bit elements.
(define_mode_iterator VDX [V8QI V4HI V2SI V2SF DI])

;; Double-width vector modes without floating-point elements.
(define_mode_iterator VDI [V8QI V4HI V2SI])

;; Quad-width vector modes.
(define_mode_iterator VQ [V16QI V8HI V4SI V4SF])

;; Quad-width vector modes plus 64-bit elements.
(define_mode_iterator VQX [V16QI V8HI V4SI V4SF V2DI])

;; Quad-width vector modes without floating-point elements.
(define_mode_iterator VQI [V16QI V8HI V4SI])

;; Quad-width vector modes, with TImode added, for moves.
(define_mode_iterator VQXMOV [V16QI V8HI V4SI V4SF V2DI TI])

;; Opaque structure types wider than TImode.
(define_mode_iterator VSTRUCT [EI OI CI XI])

;; Opaque structure types used in table lookups (except vtbl1/vtbx1).
(define_mode_iterator VTAB [TI EI OI])

;; Widenable modes.
(define_mode_iterator VW [V8QI V4HI V2SI])

;; Narrowable modes.
(define_mode_iterator VN [V8HI V4SI V2DI])

;; All supported vector modes (except singleton DImode).
(define_mode_iterator VDQ [V8QI V16QI V4HI V8HI V2SI V4SI V2SF V4SF V2DI])

;; All supported vector modes (except those with 64-bit integer elements).
(define_mode_iterator VDQW [V8QI V16QI V4HI V8HI V2SI V4SI V2SF V4SF])

;; Supported integer vector modes (not 64 bit elements).
(define_mode_iterator VDQIW [V8QI V16QI V4HI V8HI V2SI V4SI])

;; Supported integer vector modes (not singleton DI)
(define_mode_iterator VDQI [V8QI V16QI V4HI V8HI V2SI V4SI V2DI])

;; Vector modes, including 64-bit integer elements.
(define_mode_iterator VDQX [V8QI V16QI V4HI V8HI V2SI V4SI V2SF V4SF DI V2DI])

;; Vector modes including 64-bit integer elements, but no floats.
(define_mode_iterator VDQIX [V8QI V16QI V4HI V8HI V2SI V4SI DI V2DI])

;; Vector modes for float->int conversions.
(define_mode_iterator VCVTF [V2SF V4SF])

;; Vector modes form int->float conversions.
(define_mode_iterator VCVTI [V2SI V4SI])

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

;; Iterators used for fixed-point support.
(define_mode_iterator FIXED [QQ HQ SQ UQQ UHQ USQ HA SA UHA USA])

(define_mode_iterator ADDSUB [V4QQ V2HQ V2HA])

(define_mode_iterator UQADDSUB [V4UQQ V2UHQ UQQ UHQ V2UHA UHA])

(define_mode_iterator QADDSUB [V4QQ V2HQ QQ HQ V2HA HA SQ SA])

(define_mode_iterator QMUL [HQ HA])

;;----------------------------------------------------------------------------
;; Code iterators
;;----------------------------------------------------------------------------

;; A list of condition codes used in compare instructions where 
;; the carry flag from the addition is used instead of doing the 
;; compare a second time.
(define_code_iterator LTUGEU [ltu geu])

;; A list of ...
(define_code_iterator ior_xor [ior xor])

;; Operations on two halves of a quadword vector.
(define_code_iterator vqh_ops [plus smin smax umin umax])

;; Operations on two halves of a quadword vector,
;; without unsigned variants (for use with *SFmode pattern).
(define_code_iterator vqhs_ops [plus smin smax])

;; A list of widening operators
(define_code_iterator SE [sign_extend zero_extend])

;; Right shifts
(define_code_iterator rshifts [ashiftrt lshiftrt])

;;----------------------------------------------------------------------------
;; Int iterators
;;----------------------------------------------------------------------------

(define_int_iterator VRINT [UNSPEC_VRINTZ UNSPEC_VRINTP UNSPEC_VRINTM
                            UNSPEC_VRINTR UNSPEC_VRINTX UNSPEC_VRINTA])

(define_int_iterator NEON_VRINT [UNSPEC_NVRINTP UNSPEC_NVRINTZ UNSPEC_NVRINTM
                              UNSPEC_NVRINTX UNSPEC_NVRINTA UNSPEC_NVRINTN])

;;----------------------------------------------------------------------------
;; Mode attributes
;;----------------------------------------------------------------------------

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

;; Define element mode for each vector mode.
(define_mode_attr V_elem [(V8QI "QI") (V16QI "QI")
              (V4HI "HI") (V8HI "HI")
                          (V2SI "SI") (V4SI "SI")
                          (V2SF "SF") (V4SF "SF")
                          (DI "DI")   (V2DI "DI")])

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
                              (V2SI "V2SI") (V4SI "V2SI")
                              (V2SF "V2SF") (V4SF "V2SF")
                              (DI "V2DI")   (V2DI "V2DI")])

;; Similar, for three elements.
(define_mode_attr V_three_elem [(V8QI "BLK") (V16QI "BLK")
                                (V4HI "BLK") (V8HI "BLK")
                                (V2SI "BLK") (V4SI "BLK")
                                (V2SF "BLK") (V4SF "BLK")
                                (DI "EI")    (V2DI "EI")])

;; Similar, for four elements.
(define_mode_attr V_four_elem [(V8QI "SI")   (V16QI "SI")
                               (V4HI "V4HI") (V8HI "V4HI")
                               (V2SI "V4SI") (V4SI "V4SI")
                               (V2SF "V4SF") (V4SF "V4SF")
                               (DI "OI")     (V2DI "OI")])

;; Register width from element mode
(define_mode_attr V_reg [(V8QI "P") (V16QI "q")
                         (V4HI "P") (V8HI  "q")
                         (V2SI "P") (V4SI  "q")
                         (V2SF "P") (V4SF  "q")
                         (DI   "P") (V2DI  "q")
                         (SF   "")  (DF    "P")])

;; Wider modes with the same number of elements.
(define_mode_attr V_widen [(V8QI "V8HI") (V4HI "V4SI") (V2SI "V2DI")])

;; Narrower modes with the same number of elements.
(define_mode_attr V_narrow [(V8HI "V8QI") (V4SI "V4HI") (V2DI "V2SI")])

;; Narrower modes with double the number of elements.
(define_mode_attr V_narrow_pack [(V4SI "V8HI") (V8HI "V16QI") (V2DI "V4SI")
				 (V4HI "V8QI") (V2SI "V4HI")  (DI "V2SI")])

;; Modes with half the number of equal-sized elements.
(define_mode_attr V_HALF [(V16QI "V8QI") (V8HI "V4HI")
              (V4SI  "V2SI") (V4SF "V2SF") (V2DF "DF")
                          (V2DI "DI")])

;; Same, but lower-case.
(define_mode_attr V_half [(V16QI "v8qi") (V8HI "v4hi")
              (V4SI  "v2si") (V4SF "v2sf")
                          (V2DI "di")])

;; Modes with twice the number of equal-sized elements.
(define_mode_attr V_DOUBLE [(V8QI "V16QI") (V4HI "V8HI")
                (V2SI "V4SI") (V2SF "V4SF") (DF "V2DF")
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
                                        (V2SI "V2SI") (V2SF "V2SF")])

;; Mode of result of comparison operations (and bit-select operand 1).
(define_mode_attr V_cmp_result [(V8QI "V8QI") (V16QI "V16QI")
                    (V4HI "V4HI") (V8HI  "V8HI")
                                (V2SI "V2SI") (V4SI  "V4SI")
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
                 (SF "f32") (DF "f64")])

;; Same, but for operations which work on signed values.
(define_mode_attr V_s_elem [(V8QI "s8")  (V16QI "s8")
                (V4HI "s16") (V8HI  "s16")
                            (V2SI "s32") (V4SI  "s32")
                            (DI   "s64") (V2DI  "s64")
                (V2SF "f32") (V4SF  "f32")])

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
                             (V2SF "32") (V4SF "32")])

(define_mode_attr V_sz_elem [(V8QI "8")  (V16QI "8")
                 (V4HI "16") (V8HI  "16")
                             (V2SI "32") (V4SI  "32")
                             (DI   "64") (V2DI  "64")
                 (V2SF "32") (V4SF  "32")])

(define_mode_attr V_elem_ch [(V8QI "b")  (V16QI "b")
                             (V4HI "h") (V8HI  "h")
                             (V2SI "s") (V4SI  "s")
                             (DI   "d") (V2DI  "d")
                             (V2SF "s") (V4SF  "s")])

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
                                         (V8HI "x") (V4SI "t") (V4SF "t")])

;; Predicates used for setting type for neon instructions

(define_mode_attr Is_float_mode [(V8QI "false") (V16QI "false")
                 (V4HI "false") (V8HI "false")
                 (V2SI "false") (V4SI "false")
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
                            (DI   "true") (V2DI  "false")])

(define_mode_attr V_mode_nunits [(V8QI "8") (V16QI "16")
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
                     (V2SF "") (V4SF "_q")
                     (DI "")   (V2DI "_q")
                     (DF "")   (V2DF "_q")])

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

;; Right shifts
(define_code_attr shift [(ashiftrt "ashr") (lshiftrt "lshr")])
(define_code_attr shifttype [(ashiftrt "signed") (lshiftrt "unsigned")])

;;----------------------------------------------------------------------------
;; Int attributes
;;----------------------------------------------------------------------------

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
;; Both kinds of return insn.
(define_code_iterator returns [return simple_return])
(define_code_attr return_str [(return "") (simple_return "simple_")])
(define_code_attr return_simple_p [(return "false") (simple_return "true")])
(define_code_attr return_cond_false [(return " && USE_RETURN_INSN (FALSE)")
                               (simple_return " && use_simple_return_p ()")])
(define_code_attr return_cond_true [(return " && USE_RETURN_INSN (TRUE)")
                               (simple_return " && use_simple_return_p ()")])
