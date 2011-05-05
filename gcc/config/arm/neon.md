;; ARM NEON coprocessor Machine Description
;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
;; Written by CodeSourcery.
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

;; Constants for unspecs.
(define_constants
  [(UNSPEC_ASHIFT_SIGNED	65)
   (UNSPEC_ASHIFT_UNSIGNED	66)
   (UNSPEC_VABA			67)
   (UNSPEC_VABAL		68)
   (UNSPEC_VABD			69)
   (UNSPEC_VABDL		70)
   (UNSPEC_VABS			71)
   (UNSPEC_VADD			72)
   (UNSPEC_VADDHN		73)
   (UNSPEC_VADDL		74)
   (UNSPEC_VADDW		75)
   (UNSPEC_VAND			76)
   (UNSPEC_VBIC			77)
   (UNSPEC_VBSL			78)
   (UNSPEC_VCAGE		79)
   (UNSPEC_VCAGT		80)
   (UNSPEC_VCEQ			81)
   (UNSPEC_VCGE			82)
   (UNSPEC_VCGT			83)
   (UNSPEC_VCLS			84)
   (UNSPEC_VCLZ			85)
   (UNSPEC_VCNT			86)
   (UNSPEC_VCOMBINE		87)
   (UNSPEC_VCVT			88)
   (UNSPEC_VCVT_N		89)
   (UNSPEC_VDUP_LANE		90)
   (UNSPEC_VDUP_N		91)
   (UNSPEC_VEOR			92)
   (UNSPEC_VEXT			93)
   (UNSPEC_VGET_HIGH		94)
   (UNSPEC_VGET_LANE		95)
   (UNSPEC_VGET_LOW		96)
   (UNSPEC_VHADD		97)
   (UNSPEC_VHSUB		98)
   (UNSPEC_VLD1			99)
   (UNSPEC_VLD1_DUP		100)
   (UNSPEC_VLD1_LANE		101)
   (UNSPEC_VLD2			102)
   (UNSPEC_VLD2_DUP		103)
   (UNSPEC_VLD2_LANE		104)
   (UNSPEC_VLD3			105)
   (UNSPEC_VLD3A		106)
   (UNSPEC_VLD3B		107)
   (UNSPEC_VLD3_DUP		108)
   (UNSPEC_VLD3_LANE		109)
   (UNSPEC_VLD4			110)
   (UNSPEC_VLD4A		111)
   (UNSPEC_VLD4B		112)
   (UNSPEC_VLD4_DUP		113)
   (UNSPEC_VLD4_LANE		114)
   (UNSPEC_VMAX			115)
   (UNSPEC_VMIN			116)
   (UNSPEC_VMLA			117)
   (UNSPEC_VMLAL		118)
   (UNSPEC_VMLA_LANE		119)
   (UNSPEC_VMLAL_LANE		120)
   (UNSPEC_VMLS			121)
   (UNSPEC_VMLSL		122)
   (UNSPEC_VMLS_LANE		123)
   (UNSPEC_VMLSL_LANE		124)
   (UNSPEC_VMOVL		125)
   (UNSPEC_VMOVN		126)
   (UNSPEC_VMUL			127)
   (UNSPEC_VMULL		128)
   (UNSPEC_VMUL_LANE		129)
   (UNSPEC_VMULL_LANE		130)
   (UNSPEC_VMUL_N		131)
   (UNSPEC_VMVN			132)
   (UNSPEC_VORN			133)
   (UNSPEC_VORR			134)
   (UNSPEC_VPADAL		135)
   (UNSPEC_VPADD		136)
   (UNSPEC_VPADDL		137)
   (UNSPEC_VPMAX		138)
   (UNSPEC_VPMIN		139)
   (UNSPEC_VPSMAX		140)
   (UNSPEC_VPSMIN		141)
   (UNSPEC_VPUMAX		142)
   (UNSPEC_VPUMIN		143)
   (UNSPEC_VQABS		144)
   (UNSPEC_VQADD		145)
   (UNSPEC_VQDMLAL		146)
   (UNSPEC_VQDMLAL_LANE		147)
   (UNSPEC_VQDMLSL		148)
   (UNSPEC_VQDMLSL_LANE		149)
   (UNSPEC_VQDMULH		150)
   (UNSPEC_VQDMULH_LANE		151)
   (UNSPEC_VQDMULL		152)
   (UNSPEC_VQDMULL_LANE		153)
   (UNSPEC_VQMOVN		154)
   (UNSPEC_VQMOVUN		155)
   (UNSPEC_VQNEG		156)
   (UNSPEC_VQSHL		157)
   (UNSPEC_VQSHL_N		158)
   (UNSPEC_VQSHLU_N		159)
   (UNSPEC_VQSHRN_N		160)
   (UNSPEC_VQSHRUN_N		161)
   (UNSPEC_VQSUB		162)
   (UNSPEC_VRECPE		163)
   (UNSPEC_VRECPS		164)
   (UNSPEC_VREV16		165)
   (UNSPEC_VREV32		166)
   (UNSPEC_VREV64		167)
   (UNSPEC_VRSQRTE		168)
   (UNSPEC_VRSQRTS		169)
   (UNSPEC_VSET_LANE		170)
   (UNSPEC_VSHL			171)
   (UNSPEC_VSHLL_N		172)
   (UNSPEC_VSHL_N		173)
   (UNSPEC_VSHR_N		174)
   (UNSPEC_VSHRN_N		175)
   (UNSPEC_VSLI			176)
   (UNSPEC_VSRA_N		177)
   (UNSPEC_VSRI			178)
   (UNSPEC_VST1			179)
   (UNSPEC_VST1_LANE		180)
   (UNSPEC_VST2			181)
   (UNSPEC_VST2_LANE		182)
   (UNSPEC_VST3			183)
   (UNSPEC_VST3A		184)
   (UNSPEC_VST3B		185)
   (UNSPEC_VST3_LANE		186)
   (UNSPEC_VST4			187)
   (UNSPEC_VST4A		188)
   (UNSPEC_VST4B		189)
   (UNSPEC_VST4_LANE		190)
   (UNSPEC_VSTRUCTDUMMY		191)
   (UNSPEC_VSUB			192)
   (UNSPEC_VSUBHN		193)
   (UNSPEC_VSUBL		194)
   (UNSPEC_VSUBW		195)
   (UNSPEC_VTBL			196)
   (UNSPEC_VTBX			197)
   (UNSPEC_VTRN1		198)
   (UNSPEC_VTRN2		199)
   (UNSPEC_VTST			200)
   (UNSPEC_VUZP1		201)
   (UNSPEC_VUZP2		202)
   (UNSPEC_VZIP1		203)
   (UNSPEC_VZIP2		204)])

;; Double-width vector modes.
(define_mode_iterator VD [V8QI V4HI V2SI V2SF])

;; Double-width vector modes plus 64-bit elements.
(define_mode_iterator VDX [V8QI V4HI V2SI V2SF DI])

;; Same, without floating-point elements.
(define_mode_iterator VDI [V8QI V4HI V2SI])

;; Quad-width vector modes.
(define_mode_iterator VQ [V16QI V8HI V4SI V4SF])

;; Quad-width vector modes plus 64-bit elements.
(define_mode_iterator VQX [V16QI V8HI V4SI V4SF V2DI])

;; Same, without floating-point elements.
(define_mode_iterator VQI [V16QI V8HI V4SI])

;; Same, with TImode added, for moves.
(define_mode_iterator VQXMOV [V16QI V8HI V4SI V4SF V2DI TI])

;; Opaque structure types wider than TImode.
(define_mode_iterator VSTRUCT [EI OI CI XI])

;; Opaque structure types used in table lookups (except vtbl1/vtbx1).
(define_mode_iterator VTAB [TI EI OI])

;; vtbl<n> suffix for above modes.
(define_mode_attr VTAB_n [(TI "2") (EI "3") (OI "4")])

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

;; (Opposite) mode to convert to/from for above conversions.
(define_mode_attr V_CVTTO [(V2SI "V2SF") (V2SF "V2SI")
			   (V4SI "V4SF") (V4SF "V4SI")])

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
(define_mode_attr V_two_elem [(V8QI "HI") (V16QI "HI")
			      (V4HI "SI") (V8HI "SI")
                              (V2SI "V2SI") (V4SI "V2SI")
                              (V2SF "V2SF") (V4SF "V2SF")
                              (DI "V2DI")   (V2DI "V2DI")])

;; Similar, for three elements.
;; ??? Should we define extra modes so that sizes of all three-element
;; accesses can be accurately represented?
(define_mode_attr V_three_elem [(V8QI "SI")   (V16QI "SI")
			        (V4HI "V4HI") (V8HI "V4HI")
                                (V2SI "V4SI") (V4SI "V4SI")
                                (V2SF "V4SF") (V4SF "V4SF")
                                (DI "EI")     (V2DI "EI")])

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
                         (DI   "P") (V2DI  "q")])

;; Wider modes with the same number of elements.
(define_mode_attr V_widen [(V8QI "V8HI") (V4HI "V4SI") (V2SI "V2DI")])

;; Narrower modes with the same number of elements.
(define_mode_attr V_narrow [(V8HI "V8QI") (V4SI "V4HI") (V2DI "V2SI")])

;; Modes with half the number of equal-sized elements.
(define_mode_attr V_HALF [(V16QI "V8QI") (V8HI "V4HI")
			  (V4SI  "V2SI") (V4SF "V2SF")
                          (V2DI "DI")])

;; Same, but lower-case.
(define_mode_attr V_half [(V16QI "v8qi") (V8HI "v4hi")
			  (V4SI  "v2si") (V4SF "v2sf")
                          (V2DI "di")])

;; Modes with twice the number of equal-sized elements.
(define_mode_attr V_DOUBLE [(V8QI "V16QI") (V4HI "V8HI")
			    (V2SI "V4SI") (V2SF "V4SF")
                            (DI "V2DI")])

;; Same, but lower-case.
(define_mode_attr V_double [(V8QI "v16qi") (V4HI "v8hi")
			    (V2SI "v4si") (V2SF "v4sf")
                            (DI "v2di")])

;; Modes with double-width elements.
(define_mode_attr V_double_width [(V8QI "V4HI") (V16QI "V8HI")
				  (V4HI "V2SI") (V8HI "V4SI")
				  (V2SI "DI")   (V4SI "V2DI")])

;; Mode of result of comparison operations (and bit-select operand 1).
(define_mode_attr V_cmp_result [(V8QI "V8QI") (V16QI "V16QI")
			        (V4HI "V4HI") (V8HI  "V8HI")
                                (V2SI "V2SI") (V4SI  "V4SI")
                                (V2SF "V2SI") (V4SF  "V4SI")
                                (DI   "DI")   (V2DI  "V2DI")])

;; Get element type from double-width mode, for operations where we don't care
;; about signedness.
(define_mode_attr V_if_elem [(V8QI "i8")  (V16QI "i8")
			     (V4HI "i16") (V8HI  "i16")
                             (V2SI "i32") (V4SI  "i32")
                             (DI   "i64") (V2DI  "i64")
			     (V2SF "f32") (V4SF  "f32")])

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

;; Operations on two halves of a quadword vector.
(define_code_iterator vqh_ops [plus smin smax umin umax])

;; Same, without unsigned variants (for use with *SFmode pattern).
(define_code_iterator vqhs_ops [plus smin smax])

;; Assembler mnemonics for above codes.
(define_code_attr VQH_mnem [(plus "vadd") (smin "vmin") (smax "vmax")
			    (umin "vmin") (umax "vmax")])

;; Signs of above, where relevant.
(define_code_attr VQH_sign [(plus "i") (smin "s") (smax "s") (umin "u")
			    (umax "u")])

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

;; Attribute used to permit string comparisons against <VQH_mnem> in
;; neon_type attribute definitions.
(define_attr "vqh_mnem" "vadd,vmin,vmax" (const_string "vadd"))

;; Predicates used for setting neon_type

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
                                 (DI "1")   (V2DI "2")])

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VD 0 "nonimmediate_operand"
	  "=w,Uv,w, w,  ?r,?w,?r,?r, ?Us")
	(match_operand:VD 1 "general_operand"
	  " w,w, Dn,Uvi, w, r, r, Usi,r"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  if (which_alternative == 2)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = neon_immediate_valid_for_move (operands[1], <MODE>mode,
        &operands[1], &width);

      gcc_assert (is_valid != 0);

      if (width == 0)
        return "vmov.f32\t%P0, %1  @ <mode>";
      else
        sprintf (templ, "vmov.i%d\t%%P0, %%1  @ <mode>", width);

      return templ;
    }

  /* FIXME: If the memory layout is changed in big-endian mode, output_move_vfp
     below must be changed to output_move_neon (which will use the
     element/structure loads/stores), and the constraint changed to 'Um' instead
     of 'Uv'.  */

  switch (which_alternative)
    {
    case 0: return "vmov\t%P0, %P1  @ <mode>";
    case 1: case 3: return output_move_vfp (operands);
    case 2: gcc_unreachable ();
    case 4: return "vmov\t%Q0, %R0, %P1  @ <mode>";
    case 5: return "vmov\t%P0, %Q1, %R1  @ <mode>";
    default: return output_move_double (operands);
    }
}
 [(set_attr "neon_type" "neon_int_1,*,neon_vmov,*,neon_mrrc,neon_mcr_2_mcrr,*,*,*")
  (set_attr "type" "*,f_stored,*,f_loadd,*,*,alu,load2,store2")
  (set_attr "insn" "*,*,*,*,*,*,mov,*,*")
  (set_attr "length" "4,4,4,4,4,4,8,8,8")
  (set_attr "pool_range"     "*,*,*,1020,*,*,*,1020,*")
  (set_attr "neg_pool_range" "*,*,*,1008,*,*,*,1008,*")])

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VQXMOV 0 "nonimmediate_operand"
  	  "=w,Un,w, w,  ?r,?w,?r,?r,  ?Us")
	(match_operand:VQXMOV 1 "general_operand"
	  " w,w, Dn,Uni, w, r, r, Usi, r"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  if (which_alternative == 2)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = neon_immediate_valid_for_move (operands[1], <MODE>mode,
        &operands[1], &width);

      gcc_assert (is_valid != 0);

      if (width == 0)
        return "vmov.f32\t%q0, %1  @ <mode>";
      else
        sprintf (templ, "vmov.i%d\t%%q0, %%1  @ <mode>", width);

      return templ;
    }

  switch (which_alternative)
    {
    case 0: return "vmov\t%q0, %q1  @ <mode>";
    case 1: case 3: return output_move_neon (operands);
    case 2: gcc_unreachable ();
    case 4: return "vmov\t%Q0, %R0, %e1  @ <mode>\;vmov\t%J0, %K0, %f1";
    case 5: return "vmov\t%e0, %Q1, %R1  @ <mode>\;vmov\t%f0, %J1, %K1";
    default: return output_move_quad (operands);
    }
}
  [(set_attr "neon_type" "neon_int_1,neon_stm_2,neon_vmov,neon_ldm_2,\
                          neon_mrrc,neon_mcr_2_mcrr,*,*,*")
   (set_attr "type" "*,*,*,*,*,*,alu,load4,store4")
   (set_attr "insn" "*,*,*,*,*,*,mov,*,*")
   (set_attr "length" "4,8,4,8,8,8,16,8,16")
   (set_attr "pool_range" "*,*,*,1020,*,*,*,1020,*")
   (set_attr "neg_pool_range" "*,*,*,1008,*,*,*,1008,*")])

(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "")
	(match_operand:TI 1 "general_operand" ""))]
  "TARGET_NEON"
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (TImode, operands[1]);
    }
})

(define_expand "mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand" "")
	(match_operand:VSTRUCT 1 "general_operand" ""))]
  "TARGET_NEON"
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
})

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand"	"=w,Ut,w")
	(match_operand:VSTRUCT 1 "general_operand"	" w,w, Ut"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0: return "#";
    case 1: case 2: return output_move_neon (operands);
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1,neon_stm_2,neon_ldm_2")
   (set (attr "length") (symbol_ref "arm_attr_length_move_neon (insn)"))])

(define_split
  [(set (match_operand:EI 0 "s_register_operand" "")
	(match_operand:EI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[2], src[2];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (DImode, rdest + 4);
  src[1] = gen_rtx_REG (DImode, rsrc + 4);

  neon_disambiguate_copy (operands, dest, src, 2);
})

(define_split
  [(set (match_operand:OI 0 "s_register_operand" "")
	(match_operand:OI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[2], src[2];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (TImode, rdest + 4);
  src[1] = gen_rtx_REG (TImode, rsrc + 4);

  neon_disambiguate_copy (operands, dest, src, 2);
})

(define_split
  [(set (match_operand:CI 0 "s_register_operand" "")
	(match_operand:CI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[3], src[3];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (TImode, rdest + 4);
  src[1] = gen_rtx_REG (TImode, rsrc + 4);
  dest[2] = gen_rtx_REG (TImode, rdest + 8);
  src[2] = gen_rtx_REG (TImode, rsrc + 8);

  neon_disambiguate_copy (operands, dest, src, 3);
})

(define_split
  [(set (match_operand:XI 0 "s_register_operand" "")
	(match_operand:XI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[4], src[4];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (TImode, rdest + 4);
  src[1] = gen_rtx_REG (TImode, rsrc + 4);
  dest[2] = gen_rtx_REG (TImode, rdest + 8);
  src[2] = gen_rtx_REG (TImode, rsrc + 8);
  dest[3] = gen_rtx_REG (TImode, rdest + 12);
  src[3] = gen_rtx_REG (TImode, rsrc + 12);

  neon_disambiguate_copy (operands, dest, src, 4);
})

(define_insn "vec_set<mode>_internal"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
        (vec_merge:VD
          (vec_duplicate:VD
            (match_operand:<V_elem> 1 "s_register_operand" "r"))
          (match_operand:VD 3 "s_register_operand" "0")
          (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_NEON"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;
  if (BYTES_BIG_ENDIAN)
    elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
  operands[2] = GEN_INT (elt);
  
  return "vmov%?.<V_uf_sclr>\t%P0[%c2], %1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_mcr")])

(define_insn "vec_set<mode>_internal"
  [(set (match_operand:VQ 0 "s_register_operand" "=w")
        (vec_merge:VQ
          (vec_duplicate:VQ
            (match_operand:<V_elem> 1 "s_register_operand" "r"))
          (match_operand:VQ 3 "s_register_operand" "0")
          (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_NEON"
{
  HOST_WIDE_INT elem = ffs ((int) INTVAL (operands[2])) - 1;
  int half_elts = GET_MODE_NUNITS (<MODE>mode) / 2;
  int elt = elem % half_elts;
  int hi = (elem / half_elts) * 2;
  int regno = REGNO (operands[0]);

  if (BYTES_BIG_ENDIAN)
    elt = half_elts - 1 - elt;

  operands[0] = gen_rtx_REG (<V_HALF>mode, regno + hi);
  operands[2] = GEN_INT (elt);

  return "vmov%?.<V_uf_sclr>\t%P0[%c2], %1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_mcr")]
)

(define_insn "vec_setv2di_internal"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
        (vec_merge:V2DI
          (vec_duplicate:V2DI
            (match_operand:DI 1 "s_register_operand" "r"))
          (match_operand:V2DI 3 "s_register_operand" "0")
          (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_NEON"
{
  HOST_WIDE_INT elem = ffs ((int) INTVAL (operands[2])) - 1;
  int regno = REGNO (operands[0]) + 2 * elem;

  operands[0] = gen_rtx_REG (DImode, regno);

  return "vmov%?.64\t%P0, %Q1, %R1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_mcr_2_mcrr")]
)

(define_expand "vec_set<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand:<V_elem> 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);
  emit_insn (gen_vec_set<mode>_internal (operands[0], operands[1],
					 GEN_INT (elem), operands[0]));
  DONE;
})

(define_insn "vec_extract<mode>"
  [(set (match_operand:<V_elem> 0 "s_register_operand" "=r")
        (vec_select:<V_elem>
          (match_operand:VD 1 "s_register_operand" "w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov%?.<V_uf_sclr>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "vec_extract<mode>"
  [(set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(vec_select:<V_elem>
          (match_operand:VQ 1 "s_register_operand" "w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_NEON"
{
  int half_elts = GET_MODE_NUNITS (<MODE>mode) / 2;
  int elt = INTVAL (operands[2]) % half_elts;
  int hi = (INTVAL (operands[2]) / half_elts) * 2;
  int regno = REGNO (operands[1]);

  if (BYTES_BIG_ENDIAN)
    elt = half_elts - 1 - elt;

  operands[1] = gen_rtx_REG (<V_HALF>mode, regno + hi);
  operands[2] = GEN_INT (elt);

  return "vmov%?.<V_uf_sclr>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "vec_extractv2di"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(vec_select:DI
          (match_operand:V2DI 1 "s_register_operand" "w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[1]) + INTVAL (operands[2]);

  operands[1] = gen_rtx_REG (DImode, regno);

  return "vmov%?.64\t%Q0, %R0, %P1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_int_1")]
)

(define_expand "vec_init<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand 1 "" "")]
  "TARGET_NEON"
{
  neon_expand_vector_init (operands[0], operands[1]);
  DONE;
})

;; Doubleword and quadword arithmetic.

;; NOTE: vadd/vsub and some other instructions also support 64-bit integer
;; element size, which we could potentially use for "long long" operations. We
;; don't want to do this at present though, because moving values from the
;; vector unit to the ARM core is currently slow and 64-bit addition (etc.) is
;; easy to do with ARM instructions anyway.

(define_insn "*add<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (plus:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		  (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vadd.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_1")))]
)

(define_insn "*sub<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (minus:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
                   (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vsub.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_2")))]
)

(define_insn "*mul<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (mult:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
                  (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmul.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (if_then_else
                                    (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32"))
                                  (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")))))]
)

(define_insn "*mul<mode>3add<mode>_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (plus:VDQ (mult:VDQ (match_operand:VDQ 2 "s_register_operand" "w")
                            (match_operand:VDQ 3 "s_register_operand" "w"))
		  (match_operand:VDQ 1 "s_register_operand" "0")))]
  "TARGET_NEON"
  "vmla.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (if_then_else
                                    (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "*mul<mode>3neg<mode>add<mode>_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (minus:VDQ (match_operand:VDQ 1 "s_register_operand" "0")
                   (mult:VDQ (match_operand:VDQ 2 "s_register_operand" "w")
                             (match_operand:VDQ 3 "s_register_operand" "w"))))]
  "TARGET_NEON"
  "vmls.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (if_then_else
                                    (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "ior<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w,w")
	(ior:VDQ (match_operand:VDQ 1 "s_register_operand" "w,0")
		 (match_operand:VDQ 2 "neon_logic_op2" "w,Dl")))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: return "vorr\t%<V_reg>0, %<V_reg>1, %<V_reg>2";
    case 1: return neon_output_logic_immediate ("vorr", &operands[2],
		     <MODE>mode, 0, VALID_NEON_QREG_MODE (<MODE>mode));
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "iordi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "w,0")
		    (match_operand:DI 2 "neon_logic_op2" "w,Dl")]
                    UNSPEC_VORR))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: return "vorr\t%P0, %P1, %P2";
    case 1: return neon_output_logic_immediate ("vorr", &operands[2],
		     DImode, 0, VALID_NEON_QREG_MODE (DImode));
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1")]
)

;; The concrete forms of the Neon immediate-logic instructions are vbic and
;; vorr. We support the pseudo-instruction vand instead, because that
;; corresponds to the canonical form the middle-end expects to use for
;; immediate bitwise-ANDs.

(define_insn "and<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w,w")
	(and:VDQ (match_operand:VDQ 1 "s_register_operand" "w,0")
		 (match_operand:VDQ 2 "neon_inv_logic_op2" "w,DL")))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: return "vand\t%<V_reg>0, %<V_reg>1, %<V_reg>2";
    case 1: return neon_output_logic_immediate ("vand", &operands[2],
    		     <MODE>mode, 1, VALID_NEON_QREG_MODE (<MODE>mode));
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "anddi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "w,0")
		    (match_operand:DI 2 "neon_inv_logic_op2" "w,DL")]
                    UNSPEC_VAND))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: return "vand\t%P0, %P1, %P2";
    case 1: return neon_output_logic_immediate ("vand", &operands[2],
    		     DImode, 1, VALID_NEON_QREG_MODE (DImode));
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "orn<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(ior:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		 (not:VDQ (match_operand:VDQ 2 "s_register_operand" "w"))))]
  "TARGET_NEON"
  "vorn\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "orndi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "w")
		    (match_operand:DI 2 "s_register_operand" "w")]
                    UNSPEC_VORN))]
  "TARGET_NEON"
  "vorn\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "bic<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(and:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		  (not:VDQ (match_operand:VDQ 2 "s_register_operand" "w"))))]
  "TARGET_NEON"
  "vbic\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "bicdi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "w")
		     (match_operand:DI 2 "s_register_operand" "w")]
                    UNSPEC_VBIC))]
  "TARGET_NEON"
  "vbic\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "xor<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(xor:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		 (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "veor\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "xordi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "w")
		     (match_operand:DI 2 "s_register_operand" "w")]
                    UNSPEC_VEOR))]
  "TARGET_NEON"
  "veor\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (not:VDQ (match_operand:VDQ 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmvn\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "abs<mode>2"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(abs:VDQW (match_operand:VDQW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_3")))]
)

(define_insn "neg<mode>2"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(neg:VDQW (match_operand:VDQW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vneg.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_3")))]
)

(define_insn "*umin<mode>3_neon"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(umin:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		    (match_operand:VDQIW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmin.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "*umax<mode>3_neon"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(umax:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		    (match_operand:VDQIW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmax.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "*smin<mode>3_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(smin:VDQW (match_operand:VDQW 1 "s_register_operand" "w")
		   (match_operand:VDQW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmin.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

(define_insn "*smax<mode>3_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(smax:VDQW (match_operand:VDQW 1 "s_register_operand" "w")
		   (match_operand:VDQW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmax.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

; TODO: V2DI shifts are current disabled because there are bugs in the
; generic vectorizer code.  It ends up creating a V2DI constructor with
; SImode elements.

(define_insn "vashl<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(ashift:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		      (match_operand:VDQIW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vshl.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

; Used for implementing logical shift-right, which is a left-shift by a negative
; amount, with signed operands. This is essentially the same as ashl<mode>3
; above, but using an unspec in case GCC tries anything tricky with negative
; shift amounts.

(define_insn "ashl<mode>3_signed"
  [(set (match_operand:VDQI 0 "s_register_operand" "=w")
	(unspec:VDQI [(match_operand:VDQI 1 "s_register_operand" "w")
		      (match_operand:VDQI 2 "s_register_operand" "w")]
		     UNSPEC_ASHIFT_SIGNED))]
  "TARGET_NEON"
  "vshl.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

; Used for implementing logical shift-right, which is a left-shift by a negative
; amount, with unsigned operands.

(define_insn "ashl<mode>3_unsigned"
  [(set (match_operand:VDQI 0 "s_register_operand" "=w")
	(unspec:VDQI [(match_operand:VDQI 1 "s_register_operand" "w")
		      (match_operand:VDQI 2 "s_register_operand" "w")]
		     UNSPEC_ASHIFT_UNSIGNED))]
  "TARGET_NEON"
  "vshl.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

(define_expand "vashr<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand" "")
	(ashiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "")
			(match_operand:VDQIW 2 "s_register_operand" "")))]
  "TARGET_NEON"
{
  rtx neg = gen_reg_rtx (<MODE>mode);

  emit_insn (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_ashl<mode>3_signed (operands[0], operands[1], neg));

  DONE;
})

(define_expand "vlshr<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand" "")
	(lshiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "")
			(match_operand:VDQIW 2 "s_register_operand" "")))]
  "TARGET_NEON"
{
  rtx neg = gen_reg_rtx (<MODE>mode);

  emit_insn (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_ashl<mode>3_unsigned (operands[0], operands[1], neg));

  DONE;
})

;; Widening operations

(define_insn "widen_ssum<mode>3"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(plus:<V_widen> (sign_extend:<V_widen>
			  (match_operand:VW 1 "s_register_operand" "%w"))
		        (match_operand:<V_widen> 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vaddw.<V_s_elem>\t%q0, %q2, %P1"
  [(set_attr "neon_type" "neon_int_3")]
)

(define_insn "widen_usum<mode>3"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(plus:<V_widen> (zero_extend:<V_widen>
			  (match_operand:VW 1 "s_register_operand" "%w"))
		        (match_operand:<V_widen> 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vaddw.<V_u_elem>\t%q0, %q2, %P1"
  [(set_attr "neon_type" "neon_int_3")]
)

;; VEXT can be used to synthesize coarse whole-vector shifts with 8-bit
;; shift-count granularity. That's good enough for the middle-end's current
;; needs.

(define_expand "vec_shr_<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand:VDQ 1 "s_register_operand" "")
   (match_operand:SI 2 "const_multiple_of_8_operand" "")]
  "TARGET_NEON"
{
  rtx zero_reg;
  HOST_WIDE_INT num_bits = INTVAL (operands[2]);
  const int width = GET_MODE_BITSIZE (<MODE>mode);
  const enum machine_mode bvecmode = (width == 128) ? V16QImode : V8QImode;
  rtx (*gen_ext) (rtx, rtx, rtx, rtx) =
    (width == 128) ? gen_neon_vextv16qi : gen_neon_vextv8qi;

  if (num_bits == width)
    {
      emit_move_insn (operands[0], operands[1]);
      DONE;
    }

  zero_reg = force_reg (bvecmode, CONST0_RTX (bvecmode));
  operands[0] = gen_lowpart (bvecmode, operands[0]);
  operands[1] = gen_lowpart (bvecmode, operands[1]);

  emit_insn (gen_ext (operands[0], operands[1], zero_reg,
		      GEN_INT (num_bits / BITS_PER_UNIT)));
  DONE;
})

(define_expand "vec_shl_<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand:VDQ 1 "s_register_operand" "")
   (match_operand:SI 2 "const_multiple_of_8_operand" "")]
  "TARGET_NEON"
{
  rtx zero_reg;
  HOST_WIDE_INT num_bits = INTVAL (operands[2]);
  const int width = GET_MODE_BITSIZE (<MODE>mode);
  const enum machine_mode bvecmode = (width == 128) ? V16QImode : V8QImode;
  rtx (*gen_ext) (rtx, rtx, rtx, rtx) =
    (width == 128) ? gen_neon_vextv16qi : gen_neon_vextv8qi;

  if (num_bits == 0)
    {
      emit_move_insn (operands[0], CONST0_RTX (<MODE>mode));
      DONE;
    }

  num_bits = width - num_bits;

  zero_reg = force_reg (bvecmode, CONST0_RTX (bvecmode));
  operands[0] = gen_lowpart (bvecmode, operands[0]);
  operands[1] = gen_lowpart (bvecmode, operands[1]);

  emit_insn (gen_ext (operands[0], zero_reg, operands[1],
		      GEN_INT (num_bits / BITS_PER_UNIT)));
  DONE;
})

;; Helpers for quad-word reduction operations

; Add (or smin, smax...) the low N/2 elements of the N-element vector
; operand[1] to the high N/2 elements of same. Put the result in operand[0], an
; N/2-element vector.

(define_insn "quad_halves_<code>v4si"
  [(set (match_operand:V2SI 0 "s_register_operand" "=w")
        (vqh_ops:V2SI
          (vec_select:V2SI (match_operand:V4SI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)]))
          (vec_select:V2SI (match_dup 1)
                           (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>32\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

(define_insn "quad_halves_<code>v4sf"
  [(set (match_operand:V2SF 0 "s_register_operand" "=w")
        (vqhs_ops:V2SF
          (vec_select:V2SF (match_operand:V4SF 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)]))
          (vec_select:V2SF (match_dup 1)
                           (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.f32\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

(define_insn "quad_halves_<code>v8hi"
  [(set (match_operand:V4HI 0 "s_register_operand" "+w")
        (vqh_ops:V4HI
          (vec_select:V4HI (match_operand:V8HI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)
				      (const_int 2) (const_int 3)]))
          (vec_select:V4HI (match_dup 1)
                           (parallel [(const_int 4) (const_int 5)
				      (const_int 6) (const_int 7)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>16\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

(define_insn "quad_halves_<code>v16qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "+w")
        (vqh_ops:V8QI
          (vec_select:V8QI (match_operand:V16QI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)
				      (const_int 2) (const_int 3)
				      (const_int 4) (const_int 5)
				      (const_int 6) (const_int 7)]))
          (vec_select:V8QI (match_dup 1)
                           (parallel [(const_int 8) (const_int 9)
				      (const_int 10) (const_int 11)
				      (const_int 12) (const_int 13)
				      (const_int 14) (const_int 15)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>8\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

; FIXME: We wouldn't need the following insns if we could write subregs of
; vector registers. Make an attempt at removing unnecessary moves, though
; we're really at the mercy of the register allocator.

(define_insn "move_lo_quad_v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "+w")
        (vec_concat:V4SI
          (match_operand:V2SI 1 "s_register_operand" "w")
          (vec_select:V2SI (match_dup 0)
			   (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_NEON"
{
  int dest = REGNO (operands[0]);
  int src = REGNO (operands[1]);

  if (dest != src)
    return "vmov\t%e0, %P1";
  else
    return "";
}
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "move_lo_quad_v4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "+w")
        (vec_concat:V4SF
          (match_operand:V2SF 1 "s_register_operand" "w")
          (vec_select:V2SF (match_dup 0)
			   (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_NEON"
{
  int dest = REGNO (operands[0]);
  int src = REGNO (operands[1]);

  if (dest != src)
    return "vmov\t%e0, %P1";
  else
    return "";
}
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "move_lo_quad_v8hi"
  [(set (match_operand:V8HI 0 "s_register_operand" "+w")
        (vec_concat:V8HI
          (match_operand:V4HI 1 "s_register_operand" "w")
          (vec_select:V4HI (match_dup 0)
                           (parallel [(const_int 4) (const_int 5)
                                      (const_int 6) (const_int 7)]))))]
  "TARGET_NEON"
{
  int dest = REGNO (operands[0]);
  int src = REGNO (operands[1]);

  if (dest != src)
    return "vmov\t%e0, %P1";
  else
    return "";
}
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "move_lo_quad_v16qi"
  [(set (match_operand:V16QI 0 "s_register_operand" "+w")
        (vec_concat:V16QI
          (match_operand:V8QI 1 "s_register_operand" "w")
          (vec_select:V8QI (match_dup 0)
                           (parallel [(const_int 8)  (const_int 9)
                                      (const_int 10) (const_int 11)
                                      (const_int 12) (const_int 13)
                                      (const_int 14) (const_int 15)]))))]
  "TARGET_NEON"
{
  int dest = REGNO (operands[0]);
  int src = REGNO (operands[1]);

  if (dest != src)
    return "vmov\t%e0, %P1";
  else
    return "";
}
  [(set_attr "neon_type" "neon_bp_simple")]
)

;; Reduction operations

(define_expand "reduc_splus_<mode>"
  [(match_operand:VD 0 "s_register_operand" "")
   (match_operand:VD 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpadd_internal<mode>);
  DONE;
})

(define_expand "reduc_splus_<mode>"
  [(match_operand:VQ 0 "s_register_operand" "")
   (match_operand:VQ 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_plus<mode> (step1, operands[1]));
  emit_insn (gen_reduc_splus_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_insn "reduc_splus_v2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
	(unspec:V2DI [(match_operand:V2DI 1 "s_register_operand" "w")]
		     UNSPEC_VPADD))]
  "TARGET_NEON"
  "vadd.i64\t%e0, %e1, %f1"
  [(set_attr "neon_type" "neon_int_1")]
)

;; NEON does not distinguish between signed and unsigned addition except on
;; widening operations.
(define_expand "reduc_uplus_<mode>"
  [(match_operand:VDQI 0 "s_register_operand" "")
   (match_operand:VDQI 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_reduc_splus_<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "reduc_smin_<mode>"
  [(match_operand:VD 0 "s_register_operand" "")
   (match_operand:VD 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpsmin<mode>);
  DONE;
})

(define_expand "reduc_smin_<mode>"
  [(match_operand:VQ 0 "s_register_operand" "")
   (match_operand:VQ 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_smin<mode> (step1, operands[1]));
  emit_insn (gen_reduc_smin_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_expand "reduc_smax_<mode>"
  [(match_operand:VD 0 "s_register_operand" "")
   (match_operand:VD 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpsmax<mode>);
  DONE;
})

(define_expand "reduc_smax_<mode>"
  [(match_operand:VQ 0 "s_register_operand" "")
   (match_operand:VQ 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_smax<mode> (step1, operands[1]));
  emit_insn (gen_reduc_smax_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_expand "reduc_umin_<mode>"
  [(match_operand:VDI 0 "s_register_operand" "")
   (match_operand:VDI 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpumin<mode>);
  DONE;
})

(define_expand "reduc_umin_<mode>"
  [(match_operand:VQI 0 "s_register_operand" "")
   (match_operand:VQI 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_umin<mode> (step1, operands[1]));
  emit_insn (gen_reduc_umin_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_expand "reduc_umax_<mode>"
  [(match_operand:VDI 0 "s_register_operand" "")
   (match_operand:VDI 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpumax<mode>);
  DONE;
})

(define_expand "reduc_umax_<mode>"
  [(match_operand:VQI 0 "s_register_operand" "")
   (match_operand:VQI 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_umax<mode> (step1, operands[1]));
  emit_insn (gen_reduc_umax_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_insn "neon_vpadd_internal<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPADD))]
  "TARGET_NEON"
  "vpadd.<V_if_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like vadd.
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_1")))]
)

(define_insn "neon_vpsmin<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPSMIN))]
  "TARGET_NEON"
  "vpmin.<V_s_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like vmin.
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vpsmax<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPSMAX))]
  "TARGET_NEON"
  "vpmax.<V_s_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like vmax.
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vpumin<mode>"
  [(set (match_operand:VDI 0 "s_register_operand" "=w")
	(unspec:VDI [(match_operand:VDI 1 "s_register_operand" "w")
		     (match_operand:VDI 2 "s_register_operand" "w")]
                   UNSPEC_VPUMIN))]
  "TARGET_NEON"
  "vpmin.<V_u_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like umin.
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vpumax<mode>"
  [(set (match_operand:VDI 0 "s_register_operand" "=w")
	(unspec:VDI [(match_operand:VDI 1 "s_register_operand" "w")
		     (match_operand:VDI 2 "s_register_operand" "w")]
                   UNSPEC_VPUMAX))]
  "TARGET_NEON"
  "vpmax.<V_u_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like umax.
  [(set_attr "neon_type" "neon_int_5")]
)

;; Saturating arithmetic

; NOTE: Neon supports many more saturating variants of instructions than the
; following, but these are all GCC currently understands.
; FIXME: Actually, GCC doesn't know how to create saturating add/sub by itself
; yet either, although these patterns may be used by intrinsics when they're
; added.

(define_insn "*ss_add<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (ss_plus:VD (match_operand:VD 1 "s_register_operand" "w")
                   (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqadd.<V_s_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "*us_add<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (us_plus:VD (match_operand:VD 1 "s_register_operand" "w")
                   (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqadd.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "*ss_sub<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (ss_minus:VD (match_operand:VD 1 "s_register_operand" "w")
                    (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqsub.<V_s_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "*us_sub<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (us_minus:VD (match_operand:VD 1 "s_register_operand" "w")
                    (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqsub.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_5")]
)

;; Patterns for builtins.

; good for plain vadd, vaddq.

(define_insn "neon_vadd<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
        (unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")
		      (match_operand:VDQX 2 "s_register_operand" "w")
                      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VADD))]
  "TARGET_NEON"
  "vadd.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_1")))]
)

; operand 3 represents in bits:
;  bit 0: signed (vs unsigned).
;  bit 1: rounding (vs none).

(define_insn "neon_vaddl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VDI 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VADDL))]
  "TARGET_NEON"
  "vaddl.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_3")]
)

(define_insn "neon_vaddw<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VADDW))]
  "TARGET_NEON"
  "vaddw.%T3%#<V_sz_elem>\t%q0, %q1, %P2"
  [(set_attr "neon_type" "neon_int_2")]
)

; vhadd and vrhadd.

(define_insn "neon_vhadd<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VHADD))]
  "TARGET_NEON"
  "v%O3hadd.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vqadd<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
        (unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VQADD))]
  "TARGET_NEON"
  "vqadd.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vaddhn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
        (unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
		            (match_operand:VN 2 "s_register_operand" "w")
                            (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VADDHN))]
  "TARGET_NEON"
  "v%O3addhn.<V_if_elem>\t%P0, %q1, %q2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vmul<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
		     UNSPEC_VMUL))]
  "TARGET_NEON"
  "vmul.%F3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (if_then_else
                                    (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32"))
                                  (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")))))]
)

(define_insn "neon_vmla<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:VDQW 3 "s_register_operand" "w")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMLA))]
  "TARGET_NEON"
  "vmla.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (if_then_else
                                    (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "neon_vmlal<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VW 2 "s_register_operand" "w")
		           (match_operand:VW 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VMLAL))]
  "TARGET_NEON"
  "vmlal.%T4%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vmls<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:VDQW 3 "s_register_operand" "w")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMLS))]
  "TARGET_NEON"
  "vmls.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (if_then_else
                                    (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else
                                    (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "neon_vmlsl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VW 2 "s_register_operand" "w")
		           (match_operand:VW 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VMLSL))]
  "TARGET_NEON"
  "vmlsl.%T4%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmulh<mode>"
  [(set (match_operand:VMDQI 0 "s_register_operand" "=w")
        (unspec:VMDQI [(match_operand:VMDQI 1 "s_register_operand" "w")
		       (match_operand:VMDQI 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQDMULH))]
  "TARGET_NEON"
  "vq%O3dmulh.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
        (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                      (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                      (const_string "neon_mul_qqq_8_16_32_ddd_32"))
        (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                      (const_string "neon_mul_qqq_8_16_32_ddd_32")
                      (const_string "neon_mul_qqq_8_16_32_ddd_32"))))]
)

(define_insn "neon_vqdmlal<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VMDI 2 "s_register_operand" "w")
		           (match_operand:VMDI 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMLAL))]
  "TARGET_NEON"
  "vqdmlal.<V_s_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmlsl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VMDI 2 "s_register_operand" "w")
		           (match_operand:VMDI 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMLSL))]
  "TARGET_NEON"
  "vqdmlsl.<V_s_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vmull<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
		           (match_operand:VW 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VMULL))]
  "TARGET_NEON"
  "vmull.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vqdmull<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VQDMULL))]
  "TARGET_NEON"
  "vqdmull.<V_s_elem>\t%q0, %P1, %P2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vsub<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
        (unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")
		      (match_operand:VDQX 2 "s_register_operand" "w")
                      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VSUB))]
  "TARGET_NEON"
  "vsub.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_2")))]
)

(define_insn "neon_vsubl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VDI 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VSUBL))]
  "TARGET_NEON"
  "vsubl.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_2")]
)

(define_insn "neon_vsubw<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VSUBW))]
  "TARGET_NEON"
  "vsubw.%T3%#<V_sz_elem>\t%q0, %q1, %P2"
  [(set_attr "neon_type" "neon_int_2")]
)

(define_insn "neon_vqsub<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
        (unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VQSUB))]
  "TARGET_NEON"
  "vqsub.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vhsub<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VHSUB))]
  "TARGET_NEON"
  "vhsub.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vsubhn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
        (unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
		            (match_operand:VN 2 "s_register_operand" "w")
                            (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VSUBHN))]
  "TARGET_NEON"
  "v%O3subhn.<V_if_elem>\t%P0, %q1, %q2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vceq<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VDQW 1 "s_register_operand" "w")
		                (match_operand:VDQW 2 "s_register_operand" "w")
                                (match_operand:SI 3 "immediate_operand" "i")]
                               UNSPEC_VCEQ))]
  "TARGET_NEON"
  "vceq.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                    (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vcge<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VDQW 1 "s_register_operand" "w")
		                (match_operand:VDQW 2 "s_register_operand" "w")
                                (match_operand:SI 3 "immediate_operand" "i")]
                               UNSPEC_VCGE))]
  "TARGET_NEON"
  "vcge.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                 (const_string "neon_fp_vadd_ddd_vabs_dd")
                                 (const_string "neon_fp_vadd_qqq_vabs_qq"))
                   (const_string "neon_int_5")))]
)

(define_insn "neon_vcgt<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VDQW 1 "s_register_operand" "w")
		                (match_operand:VDQW 2 "s_register_operand" "w")
                                (match_operand:SI 3 "immediate_operand" "i")]
                               UNSPEC_VCGT))]
  "TARGET_NEON"
  "vcgt.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                 (const_string "neon_fp_vadd_ddd_vabs_dd")
                                 (const_string "neon_fp_vadd_qqq_vabs_qq"))
                   (const_string "neon_int_5")))]
)

(define_insn "neon_vcage<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VCVTF 1 "s_register_operand" "w")
		                (match_operand:VCVTF 2 "s_register_operand" "w")
                                (match_operand:SI 3 "immediate_operand" "i")]
                               UNSPEC_VCAGE))]
  "TARGET_NEON"
  "vacge.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcagt<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VCVTF 1 "s_register_operand" "w")
		                (match_operand:VCVTF 2 "s_register_operand" "w")
                                (match_operand:SI 3 "immediate_operand" "i")]
                               UNSPEC_VCAGT))]
  "TARGET_NEON"
  "vacgt.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vtst<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VTST))]
  "TARGET_NEON"
  "vtst.<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vabd<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
		     UNSPEC_VABD))]
  "TARGET_NEON"
  "vabd.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                 (const_string "neon_fp_vadd_ddd_vabs_dd")
                                 (const_string "neon_fp_vadd_qqq_vabs_qq"))
                   (const_string "neon_int_5")))]
)

(define_insn "neon_vabdl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
		           (match_operand:VW 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VABDL))]
  "TARGET_NEON"
  "vabdl.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vaba<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "0")
		       (match_operand:VDQIW 2 "s_register_operand" "w")
		       (match_operand:VDQIW 3 "s_register_operand" "w")
                       (match_operand:SI 4 "immediate_operand" "i")]
		      UNSPEC_VABA))]
  "TARGET_NEON"
  "vaba.%T4%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                   (const_string "neon_vaba") (const_string "neon_vaba_qqq")))]
)

(define_insn "neon_vabal<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VW 2 "s_register_operand" "w")
		           (match_operand:VW 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VABAL))]
  "TARGET_NEON"
  "vabal.%T4%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set_attr "neon_type" "neon_vaba")]
)

(define_insn "neon_vmax<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VMAX))]
  "TARGET_NEON"
  "vmax.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
    (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                  (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                (const_string "neon_fp_vadd_ddd_vabs_dd")
                                (const_string "neon_fp_vadd_qqq_vabs_qq"))
                  (const_string "neon_int_5")))]
)

(define_insn "neon_vmin<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VMIN))]
  "TARGET_NEON"
  "vmin.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
    (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                  (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                (const_string "neon_fp_vadd_ddd_vabs_dd")
                                (const_string "neon_fp_vadd_qqq_vabs_qq"))
                  (const_string "neon_int_5")))]
)

(define_expand "neon_vpadd<mode>"
  [(match_operand:VD 0 "s_register_operand" "=w")
   (match_operand:VD 1 "s_register_operand" "w")
   (match_operand:VD 2 "s_register_operand" "w")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vpadd_internal<mode> (operands[0], operands[1],
					    operands[2]));
  DONE;
})

(define_insn "neon_vpaddl<mode>"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
        (unspec:<V_double_width> [(match_operand:VDQIW 1 "s_register_operand" "w")
                                  (match_operand:SI 2 "immediate_operand" "i")]
                                 UNSPEC_VPADDL))]
  "TARGET_NEON"
  "vpaddl.%T2%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  ;; Assume this schedules like vaddl.
  [(set_attr "neon_type" "neon_int_3")]
)

(define_insn "neon_vpadal<mode>"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
        (unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
                                  (match_operand:VDQIW 2 "s_register_operand" "w")
                                  (match_operand:SI 3 "immediate_operand" "i")]
                                 UNSPEC_VPADAL))]
  "TARGET_NEON"
  "vpadal.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2"
  ;; Assume this schedules like vpadd.
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "neon_vpmax<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
        (unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")
                    (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VPMAX))]
  "TARGET_NEON"
  "vpmax.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  ;; Assume this schedules like vmax.
  [(set (attr "neon_type")
    (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                  (const_string "neon_int_5")))]
)

(define_insn "neon_vpmin<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
        (unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")
                    (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VPMIN))]
  "TARGET_NEON"
  "vpmin.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  ;; Assume this schedules like vmin.
  [(set (attr "neon_type")
    (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                  (const_string "neon_int_5")))]
)

(define_insn "neon_vrecps<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VRECPS))]
  "TARGET_NEON"
  "vrecps.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_fp_vrecps_vrsqrts_ddd")
                    (const_string "neon_fp_vrecps_vrsqrts_qqq")))]
)

(define_insn "neon_vrsqrts<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VRSQRTS))]
  "TARGET_NEON"
  "vrsqrts.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_fp_vrecps_vrsqrts_ddd")
                    (const_string "neon_fp_vrecps_vrsqrts_qqq")))]
)

(define_insn "neon_vabs<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")]
                     UNSPEC_VABS))]
  "TARGET_NEON"
  "vabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (ior (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                        (ne (symbol_ref "<Is_float_mode>") (const_int 0)))
                   (if_then_else
                      (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                      (const_string "neon_fp_vadd_ddd_vabs_dd")
                      (const_string "neon_fp_vadd_qqq_vabs_qq"))
                   (const_string "neon_vqneg_vqabs")))]
)

(define_insn "neon_vqabs<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
		      UNSPEC_VQABS))]
  "TARGET_NEON"
  "vqabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_vqneg_vqabs")]
)

(define_expand "neon_vneg<mode>"
  [(match_operand:VDQW 0 "s_register_operand" "")
   (match_operand:VDQW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_neg<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vqneg<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
		      UNSPEC_VQNEG))]
  "TARGET_NEON"
  "vqneg.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_vqneg_vqabs")]
)

(define_insn "neon_vcls<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
		      UNSPEC_VCLS))]
  "TARGET_NEON"
  "vcls.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "neon_vclz<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
		      UNSPEC_VCLZ))]
  "TARGET_NEON"
  "vclz.<V_if_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "neon_vcnt<mode>"
  [(set (match_operand:VE 0 "s_register_operand" "=w")
	(unspec:VE [(match_operand:VE 1 "s_register_operand" "w")
                    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VCNT))]
  "TARGET_NEON"
  "vcnt.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "neon_vrecpe<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w")
	(unspec:V32 [(match_operand:V32 1 "s_register_operand" "w")
                     (match_operand:SI 2 "immediate_operand" "i")]
                    UNSPEC_VRECPE))]
  "TARGET_NEON"
  "vrecpe.<V_u_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vrsqrte<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w")
	(unspec:V32 [(match_operand:V32 1 "s_register_operand" "w")
                     (match_operand:SI 2 "immediate_operand" "i")]
                    UNSPEC_VRSQRTE))]
  "TARGET_NEON"
  "vrsqrte.<V_u_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_expand "neon_vmvn<mode>"
  [(match_operand:VDQIW 0 "s_register_operand" "")
   (match_operand:VDQIW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_one_cmpl<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vget_lane<mode>_sext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VD 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov%?.s<V_sz_elem>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_lane<mode>_zext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VD 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov%?.u<V_sz_elem>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_lane<mode>_sext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VQ 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  rtx ops[3];
  int regno = REGNO (operands[1]);
  unsigned int halfelts = GET_MODE_NUNITS (<MODE>mode) / 2;
  unsigned int elt = INTVAL (operands[2]);
  unsigned int elt_adj = elt % halfelts;

  if (BYTES_BIG_ENDIAN)
    elt_adj = halfelts - 1 - elt_adj;

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (<V_HALF>mode, regno + 2 * (elt / halfelts));
  ops[2] = GEN_INT (elt_adj);
  output_asm_insn ("vmov%?.s<V_sz_elem>\t%0, %P1[%c2]", ops);

  return "";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_lane<mode>_zext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VQ 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  rtx ops[3];
  int regno = REGNO (operands[1]);
  unsigned int halfelts = GET_MODE_NUNITS (<MODE>mode) / 2;
  unsigned int elt = INTVAL (operands[2]);
  unsigned int elt_adj = elt % halfelts;

  if (BYTES_BIG_ENDIAN)
    elt_adj = halfelts - 1 - elt_adj;

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (<V_HALF>mode, regno + 2 * (elt / halfelts));
  ops[2] = GEN_INT (elt_adj);
  output_asm_insn ("vmov%?.u<V_sz_elem>\t%0, %P1[%c2]", ops);

  return "";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_expand "neon_vget_lane<mode>"
  [(match_operand:<V_ext> 0 "s_register_operand" "")
   (match_operand:VDQW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  HOST_WIDE_INT magic = INTVAL (operands[3]);
  rtx insn;

  neon_lane_bounds (operands[2], 0, GET_MODE_NUNITS (<MODE>mode));

  if (BYTES_BIG_ENDIAN)
    {
      /* The intrinsics are defined in terms of a model where the
	 element ordering in memory is vldm order, whereas the generic
	 RTL is defined in terms of a model where the element ordering
	 in memory is array order.  Convert the lane number to conform
	 to this model.  */
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts
	= 64 / GET_MODE_BITSIZE (GET_MODE_INNER (<MODE>mode));
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }

  if ((magic & 3) == 3 || GET_MODE_BITSIZE (GET_MODE_INNER (<MODE>mode)) == 32)
    insn = gen_vec_extract<mode> (operands[0], operands[1], operands[2]);
  else
    {
      if ((magic & 1) != 0)
	insn = gen_neon_vget_lane<mode>_sext_internal (operands[0], operands[1],
						       operands[2]);
      else
	insn = gen_neon_vget_lane<mode>_zext_internal (operands[0], operands[1],
						       operands[2]);
    }
  emit_insn (insn);
  DONE;
})

; Operand 3 (info word) is ignored because it does nothing useful with 64-bit
; elements.

(define_insn "neon_vget_lanedi"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
       (unspec:DI [(match_operand:DI 1 "s_register_operand" "w")
                   (match_operand:SI 2 "immediate_operand" "i")
                   (match_operand:SI 3 "immediate_operand" "i")]
                  UNSPEC_VGET_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, 1);
  return "vmov%?\t%Q0, %R0, %P1  @ di";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_lanev2di"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
       (unspec:DI [(match_operand:V2DI 1 "s_register_operand" "w")
                   (match_operand:SI 2 "immediate_operand" "i")
                   (match_operand:SI 3 "immediate_operand" "i")]
                  UNSPEC_VGET_LANE))]
  "TARGET_NEON"
{
  rtx ops[2];
  unsigned int regno = REGNO (operands[1]);
  unsigned int elt = INTVAL (operands[2]);

  neon_lane_bounds (operands[2], 0, 2);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno + 2 * elt);
  output_asm_insn ("vmov%?\t%Q0, %R0, %P1  @ v2di", ops);

  return "";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vset_lane<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:<V_elem> 1 "s_register_operand" "r")
		    (match_operand:VD 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VSET_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmov%?.<V_sz_elem>\t%P0[%c3], %1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

; See neon_vget_lanedi comment for reasons operands 2 & 3 are ignored.

(define_insn "neon_vset_lanedi"
  [(set (match_operand:DI 0 "s_register_operand" "=w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "r")
		    (match_operand:DI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VSET_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, 1);
  return "vmov%?\t%P0, %Q1, %R1  @ di";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vset_lane<mode>"
  [(set (match_operand:VQ 0 "s_register_operand" "=w")
	(unspec:VQ [(match_operand:<V_elem> 1 "s_register_operand" "r")
		    (match_operand:VQ 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VSET_LANE))]
  "TARGET_NEON"
{
  rtx ops[4];
  unsigned int regno = REGNO (operands[0]);
  unsigned int halfelts = GET_MODE_NUNITS (<MODE>mode) / 2;
  unsigned int elt = INTVAL (operands[3]);

  neon_lane_bounds (operands[3], 0, halfelts * 2);

  ops[0] = gen_rtx_REG (<V_HALF>mode, regno + 2 * (elt / halfelts));
  ops[1] = operands[1];
  ops[2] = GEN_INT (elt % halfelts);
  output_asm_insn ("vmov%?.<V_sz_elem>\t%P0[%c2], %1", ops);

  return "";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vset_lanev2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
	(unspec:V2DI [(match_operand:DI 1 "s_register_operand" "r")
		      (match_operand:V2DI 2 "s_register_operand" "0")
                      (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VSET_LANE))]
  "TARGET_NEON"
{
  rtx ops[2];
  unsigned int regno = REGNO (operands[0]);
  unsigned int elt = INTVAL (operands[3]);

  neon_lane_bounds (operands[3], 0, 2);

  ops[0] = gen_rtx_REG (DImode, regno + 2 * elt);
  ops[1] = operands[1];
  output_asm_insn ("vmov%?\t%P0, %Q1, %R1  @ v2di", ops);

  return "";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_expand "neon_vcreate<mode>"
  [(match_operand:VDX 0 "s_register_operand" "")
   (match_operand:DI 1 "general_operand" "")]
  "TARGET_NEON"
{
  rtx src = gen_lowpart (<MODE>mode, operands[1]);
  emit_move_insn (operands[0], src);
  DONE;
})

(define_insn "neon_vdup_n<mode>"
  [(set (match_operand:VX 0 "s_register_operand" "=w")
	(unspec:VX [(match_operand:<V_elem> 1 "s_register_operand" "r")]
		   UNSPEC_VDUP_N))]
  "TARGET_NEON"
  "vdup%?.<V_sz_elem>\t%<V_reg>0, %1"
  ;; Assume this schedules like vmov.
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vdup_n<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w,w")
	(unspec:V32 [(match_operand:<V_elem> 1 "s_register_operand" "r,t")]
		    UNSPEC_VDUP_N))]
  "TARGET_NEON"
  "@
  vdup%?.<V_sz_elem>\t%<V_reg>0, %1
  vdup%?.<V_sz_elem>\t%<V_reg>0, %y1"
  ;; Assume this schedules like vmov.
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vdup_ndi"
  [(set (match_operand:DI 0 "s_register_operand" "=w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "r")]
                   UNSPEC_VDUP_N))]
  "TARGET_NEON"
  "vmov%?\t%P0, %Q1, %R1"
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vdup_nv2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
	(unspec:V2DI [(match_operand:DI 1 "s_register_operand" "r")]
                     UNSPEC_VDUP_N))]
  "TARGET_NEON"
  "vmov%?\t%e0, %Q1, %R1\;vmov%?\t%f0, %Q1, %R1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "8")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vdup_lane<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VDUP_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vdup.<V_sz_elem>\t%P0, %P1[%c2]";
}
  ;; Assume this schedules like vmov.
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vdup_lane<mode>"
  [(set (match_operand:VQ 0 "s_register_operand" "=w")
	(unspec:VQ [(match_operand:<V_HALF> 1 "s_register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VDUP_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, GET_MODE_NUNITS (<V_HALF>mode));
  return "vdup.<V_sz_elem>\t%q0, %P1[%c2]";
}
  ;; Assume this schedules like vmov.
  [(set_attr "neon_type" "neon_bp_simple")]
)

; Scalar index is ignored, since only zero is valid here.
(define_expand "neon_vdup_lanedi"
  [(set (match_operand:DI 0 "s_register_operand" "=w")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VDUP_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, 1);
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

; Likewise.
(define_insn "neon_vdup_lanev2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
	(unspec:V2DI [(match_operand:DI 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")]
                     UNSPEC_VDUP_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, 1);
  return "vmov\t%e0, %P1\;vmov\t%f0, %P1";
}
  [(set_attr "length" "8")
   (set_attr "neon_type" "neon_bp_simple")]
)

;; In this insn, operand 1 should be low, and operand 2 the high part of the
;; dest vector.
;; FIXME: A different implementation of this builtin could make it much
;; more likely that we wouldn't actually need to output anything (we could make
;; it so that the reg allocator puts things in the right places magically
;; instead). Lack of subregs for vectors makes that tricky though, I think.

(define_insn "neon_vcombine<mode>"
  [(set (match_operand:<V_DOUBLE> 0 "s_register_operand" "=w")
	(unspec:<V_DOUBLE> [(match_operand:VDX 1 "s_register_operand" "w")
			    (match_operand:VDX 2 "s_register_operand" "w")]
                           UNSPEC_VCOMBINE))]
  "TARGET_NEON"
{
  int dest = REGNO (operands[0]);
  int src1 = REGNO (operands[1]);
  int src2 = REGNO (operands[2]);
  rtx destlo;

  if (src1 == dest && src2 == dest + 2)
    return "";
  else if (src2 == dest && src1 == dest + 2)
    /* Special case of reversed high/low parts.  */
    return "vswp\t%P1, %P2";

  destlo = gen_rtx_REG (<MODE>mode, dest);

  if (!reg_overlap_mentioned_p (operands[2], destlo))
    {
      /* Try to avoid unnecessary moves if part of the result is in the right
         place already.  */
      if (src1 != dest)
        output_asm_insn ("vmov\t%e0, %P1", operands);
      if (src2 != dest + 2)
        output_asm_insn ("vmov\t%f0, %P2", operands);
    }
  else
    {
      if (src2 != dest + 2)
        output_asm_insn ("vmov\t%f0, %P2", operands);
      if (src1 != dest)
        output_asm_insn ("vmov\t%e0, %P1", operands);
    }

  return "";
}
  ;; We set the neon_type attribute based on the vmov instructions above.
  [(set_attr "length" "8")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_high<mode>"
  [(set (match_operand:<V_HALF> 0 "s_register_operand" "=w")
	(unspec:<V_HALF> [(match_operand:VQX 1 "s_register_operand" "w")]
			 UNSPEC_VGET_HIGH))]
  "TARGET_NEON"
{
  int dest = REGNO (operands[0]);
  int src = REGNO (operands[1]);

  if (dest != src + 2)
    return "vmov\t%P0, %f1";
  else
    return "";
}
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_low<mode>"
  [(set (match_operand:<V_HALF> 0 "s_register_operand" "=w")
	(unspec:<V_HALF> [(match_operand:VQX 1 "s_register_operand" "w")]
			 UNSPEC_VGET_LOW))]
  "TARGET_NEON"
{
  int dest = REGNO (operands[0]);
  int src = REGNO (operands[1]);

  if (dest != src)
    return "vmov\t%P0, %e1";
  else
    return "";
}
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vcvt<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTF 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
			  UNSPEC_VCVT))]
  "TARGET_NEON"
  "vcvt.%T2%#32.f32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcvt<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTI 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
			  UNSPEC_VCVT))]
  "TARGET_NEON"
  "vcvt.f32.%T2%#32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcvt_n<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTF 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")
                           (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VCVT_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, 33);
  return "vcvt.%T3%#32.f32\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcvt_n<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTI 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")
                           (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VCVT_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, 33);
  return "vcvt.f32.%T3%#32\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vmovn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           UNSPEC_VMOVN))]
  "TARGET_NEON"
  "vmovn.<V_if_elem>\t%P0, %q1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vqmovn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           UNSPEC_VQMOVN))]
  "TARGET_NEON"
  "vqmovn.%T2%#<V_sz_elem>\t%P0, %q1"
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vqmovun<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           UNSPEC_VQMOVUN))]
  "TARGET_NEON"
  "vqmovun.<V_s_elem>\t%P0, %q1"
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vmovl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
                          UNSPEC_VMOVL))]
  "TARGET_NEON"
  "vmovl.%T2%#<V_sz_elem>\t%q0, %P1"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vmul_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "w")
		     (match_operand:VMD 2 "s_register_operand"
                                        "<scalar_mul_constraint>")
                     (match_operand:SI 3 "immediate_operand" "i")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMUL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmul.<V_if_elem>\t%P0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (const_string "neon_fp_vmul_ddd")
                   (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                 (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                                 (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"))))]
)

(define_insn "neon_vmul_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "w")
		     (match_operand:<V_HALF> 2 "s_register_operand"
                                             "<scalar_mul_constraint>")
                     (match_operand:SI 3 "immediate_operand" "i")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMUL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<V_HALF>mode));
  return "vmul.<V_if_elem>\t%q0, %q1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (const_string "neon_fp_vmul_qqd")
                   (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                 (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")
                                 (const_string "neon_mul_qqd_32_scalar"))))]
)

(define_insn "neon_vmull_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 3 "immediate_operand" "i")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VMULL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmull.%T4%#<V_sz_elem>\t%q0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vqdmull_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 3 "immediate_operand" "i")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMULL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vqdmull.<V_s_elem>\t%q0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vqdmulh_lane<mode>"
  [(set (match_operand:VMQI 0 "s_register_operand" "=w")
	(unspec:VMQI [(match_operand:VMQI 1 "s_register_operand" "w")
		      (match_operand:<V_HALF> 2 "s_register_operand"
					      "<scalar_mul_constraint>")
                      (match_operand:SI 3 "immediate_operand" "i")
                      (match_operand:SI 4 "immediate_operand" "i")]
                      UNSPEC_VQDMULH_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vq%O4dmulh.%T4%#<V_sz_elem>\t%q0, %q1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")
                   (const_string "neon_mul_qqd_32_scalar")))]
)

(define_insn "neon_vqdmulh_lane<mode>"
  [(set (match_operand:VMDI 0 "s_register_operand" "=w")
	(unspec:VMDI [(match_operand:VMDI 1 "s_register_operand" "w")
		      (match_operand:VMDI 2 "s_register_operand"
					  "<scalar_mul_constraint>")
                      (match_operand:SI 3 "immediate_operand" "i")
                      (match_operand:SI 4 "immediate_operand" "i")]
                      UNSPEC_VQDMULH_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vq%O4dmulh.%T4%#<V_sz_elem>\t%P0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vmla_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "0")
		     (match_operand:VMD 2 "s_register_operand" "w")
                     (match_operand:VMD 3 "s_register_operand"
					"<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                     UNSPEC_VMLA_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmla.<V_if_elem>\t%P0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (const_string "neon_fp_vmla_ddd_scalar")
                   (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                 (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))))]
)

(define_insn "neon_vmla_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "0")
		     (match_operand:VMQ 2 "s_register_operand" "w")
                     (match_operand:<V_HALF> 3 "s_register_operand"
					     "<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                     UNSPEC_VMLA_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmla.<V_if_elem>\t%q0, %q2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (const_string "neon_fp_vmla_qqq_scalar")
                   (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")
                                 (const_string "neon_mla_qqq_32_qqd_32_scalar"))))]
)

(define_insn "neon_vmlal_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VMLAL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmlal.%T5%#<V_sz_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmlal_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VQDMLAL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vqdmlal.<V_s_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vmls_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "0")
		     (match_operand:VMD 2 "s_register_operand" "w")
                     (match_operand:VMD 3 "s_register_operand"
					"<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                    UNSPEC_VMLS_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmls.<V_if_elem>\t%P0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (const_string "neon_fp_vmla_ddd_scalar")
                   (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                 (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))))]
)

(define_insn "neon_vmls_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "0")
		     (match_operand:VMQ 2 "s_register_operand" "w")
                     (match_operand:<V_HALF> 3 "s_register_operand"
					     "<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                    UNSPEC_VMLS_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmls.<V_if_elem>\t%q0, %q2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                   (const_string "neon_fp_vmla_qqq_scalar")
                   (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")
                                 (const_string "neon_mla_qqq_32_qqd_32_scalar"))))]
)

(define_insn "neon_vmlsl_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VMLSL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmlsl.%T5%#<V_sz_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmlsl_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VQDMLSL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vqdmlsl.<V_s_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (ne (symbol_ref "<Scalar_mul_8_16>") (const_int 0))
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

; FIXME: For the "_n" multiply/multiply-accumulate insns, we copy a value in a
; core register into a temp register, then use a scalar taken from that. This
; isn't an optimal solution if e.g. the scalar has just been read from memory
; or extracted from another vector. The latter case it's currently better to
; use the "_lane" variant, and the former case can probably be implemented
; using vld1_lane, but that hasn't been done yet.

(define_expand "neon_vmul_n<mode>"
  [(match_operand:VMD 0 "s_register_operand" "")
   (match_operand:VMD 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmul_lane<mode> (operands[0], operands[1], tmp,
				       const0_rtx, const0_rtx));
  DONE;
})

(define_expand "neon_vmul_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand" "")
   (match_operand:VMQ 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmul_lane<mode> (operands[0], operands[1], tmp,
				       const0_rtx, const0_rtx));
  DONE;
})

(define_expand "neon_vmull_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:VMDI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmull_lane<mode> (operands[0], operands[1], tmp,
				        const0_rtx, operands[3]));
  DONE;
})

(define_expand "neon_vqdmull_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:VMDI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmull_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx, const0_rtx));
  DONE;
})

(define_expand "neon_vqdmulh_n<mode>"
  [(match_operand:VMDI 0 "s_register_operand" "")
   (match_operand:VMDI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmulh_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx, operands[3]));
  DONE;
})

(define_expand "neon_vqdmulh_n<mode>"
  [(match_operand:VMQI 0 "s_register_operand" "")
   (match_operand:VMQI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmulh_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx, operands[3]));
  DONE;
})

(define_expand "neon_vmla_n<mode>"
  [(match_operand:VMD 0 "s_register_operand" "")
   (match_operand:VMD 1 "s_register_operand" "")
   (match_operand:VMD 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmla_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmla_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand" "")
   (match_operand:VMQ 1 "s_register_operand" "")
   (match_operand:VMQ 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmla_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmlal_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlal_lane<mode> (operands[0], operands[1], operands[2],
					tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vqdmlal_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmlal_lane<mode> (operands[0], operands[1], operands[2],
					  tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmls_n<mode>"
  [(match_operand:VMD 0 "s_register_operand" "")
   (match_operand:VMD 1 "s_register_operand" "")
   (match_operand:VMD 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmls_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmls_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand" "")
   (match_operand:VMQ 1 "s_register_operand" "")
   (match_operand:VMQ 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmls_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmlsl_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlsl_lane<mode> (operands[0], operands[1], operands[2],
					tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vqdmlsl_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmlsl_lane<mode> (operands[0], operands[1], operands[2],
					  tmp, const0_rtx, operands[4]));
  DONE;
})

(define_insn "neon_vext<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")
		      (match_operand:VDQX 2 "s_register_operand" "w")
                      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VEXT))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vext.<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2, %3";
}
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_2cycle")))]
)

(define_insn "neon_vrev64<mode>"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(unspec:VDQ [(match_operand:VDQ 1 "s_register_operand" "w")
		     (match_operand:SI 2 "immediate_operand" "i")]
                    UNSPEC_VREV64))]
  "TARGET_NEON"
  "vrev64.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vrev32<mode>"
  [(set (match_operand:VX 0 "s_register_operand" "=w")
	(unspec:VX [(match_operand:VX 1 "s_register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VREV32))]
  "TARGET_NEON"
  "vrev32.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vrev16<mode>"
  [(set (match_operand:VE 0 "s_register_operand" "=w")
	(unspec:VE [(match_operand:VE 1 "s_register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VREV16))]
  "TARGET_NEON"
  "vrev16.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

; vbsl_* intrinsics may compile to any of vbsl/vbif/vbit depending on register
; allocation. For an intrinsic of form:
;   rD = vbsl_* (rS, rN, rM)
; We can use any of:
;   vbsl rS, rN, rM  (if D = S)
;   vbit rD, rN, rS  (if D = M, so 1-bits in rS choose bits from rN, else rM)
;   vbif rD, rM, rS  (if D = N, so 0-bits in rS choose bits from rM, else rN)

(define_insn "neon_vbsl<mode>_internal"
  [(set (match_operand:VDQX 0 "s_register_operand"		 "=w,w,w")
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" " 0,w,w")
		      (match_operand:VDQX 2 "s_register_operand" " w,w,0")
                      (match_operand:VDQX 3 "s_register_operand" " w,0,w")]
                     UNSPEC_VBSL))]
  "TARGET_NEON"
  "@
  vbsl\t%<V_reg>0, %<V_reg>2, %<V_reg>3
  vbit\t%<V_reg>0, %<V_reg>2, %<V_reg>1
  vbif\t%<V_reg>0, %<V_reg>3, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_expand "neon_vbsl<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "")
        (unspec:VDQX [(match_operand:<V_cmp_result> 1 "s_register_operand" "")
                      (match_operand:VDQX 2 "s_register_operand" "")
                      (match_operand:VDQX 3 "s_register_operand" "")]
                     UNSPEC_VBSL))]
  "TARGET_NEON"
{
  /* We can't alias operands together if they have different modes.  */
  operands[1] = gen_lowpart (<MODE>mode, operands[1]);
})

(define_insn "neon_vshl<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSHL))]
  "TARGET_NEON"
  "v%O3shl.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

(define_insn "neon_vqshl<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQSHL))]
  "TARGET_NEON"
  "vq%O3shl.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_shift_2")
                    (const_string "neon_vqshl_vrshl_vqrshl_qqq")))]
)

(define_insn "neon_vshr_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSHR_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) + 1);
  return "v%O3shr.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vshrn_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")
			    (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VSHRN_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "v%O3shrn.<V_if_elem>\t%P0, %q1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vqshrn_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")
			    (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VQSHRN_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "vq%O3shrn.%T3%#<V_sz_elem>\t%P0, %q1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vqshrun_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")
			    (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VQSHRUN_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "vq%O3shrun.%T3%#<V_sz_elem>\t%P0, %q1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vshl_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSHL_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vshl.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vqshl_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQSHL_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vqshl.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vqshlu_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQSHLU_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vqshlu.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vshll_n<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")
			   (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VSHLL_N))]
  "TARGET_NEON"
{
  /* The boundaries are: 0 < imm <= size.  */
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode) + 1);
  return "vshll.%T3%#<V_sz_elem>\t%q0, %P1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vsra_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")
                       (match_operand:SI 4 "immediate_operand" "i")]
                      UNSPEC_VSRA_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 1, neon_element_bits (<MODE>mode) + 1);
  return "v%O4sra.%T4%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set_attr "neon_type" "neon_vsra_vrsra")]
)

(define_insn "neon_vsri_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
        	       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSRI))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 1, neon_element_bits (<MODE>mode) + 1);
  return "vsri.<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_shift_1")
                    (const_string "neon_shift_3")))]
)

(define_insn "neon_vsli_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
        	       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSLI))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 0, neon_element_bits (<MODE>mode));
  return "vsli.<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_shift_1")
                    (const_string "neon_shift_3")))]
)

(define_insn "neon_vtbl1v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
  "vtbl.8\t%P0, {%P1}, %P2"
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbl2v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:TI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
{
  rtx ops[4];
  int tabbase = REGNO (operands[1]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = operands[2];
  output_asm_insn ("vtbl.8\t%P0, {%P1, %P2}, %P3", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbl3v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:EI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
{
  rtx ops[5];
  int tabbase = REGNO (operands[1]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = operands[2];
  output_asm_insn ("vtbl.8\t%P0, {%P1, %P2, %P3}, %P4", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

(define_insn "neon_vtbl4v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:OI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
{
  rtx ops[6];
  int tabbase = REGNO (operands[1]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = gen_rtx_REG (V8QImode, tabbase + 6);
  ops[5] = operands[2];
  output_asm_insn ("vtbl.8\t%P0, {%P1, %P2, %P3, %P4}, %P5", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

(define_insn "neon_vtbx1v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:V8QI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
  "vtbx.8\t%P0, {%P2}, %P3"
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbx2v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:TI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
{
  rtx ops[4];
  int tabbase = REGNO (operands[2]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = operands[3];
  output_asm_insn ("vtbx.8\t%P0, {%P1, %P2}, %P3", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbx3v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:EI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
{
  rtx ops[5];
  int tabbase = REGNO (operands[2]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = operands[3];
  output_asm_insn ("vtbx.8\t%P0, {%P1, %P2, %P3}, %P4", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

(define_insn "neon_vtbx4v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:OI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
{
  rtx ops[6];
  int tabbase = REGNO (operands[2]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = gen_rtx_REG (V8QImode, tabbase + 6);
  ops[5] = operands[3];
  output_asm_insn ("vtbx.8\t%P0, {%P1, %P2, %P3, %P4}, %P5", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

(define_insn "neon_vtrn<mode>_internal"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
                      (match_operand:VDQW 2 "s_register_operand" "w")]
                     UNSPEC_VTRN1))
   (set (match_operand:VDQW 3 "s_register_operand" "=2")
         (unspec:VDQW [(match_dup 1) (match_dup 2)]
                     UNSPEC_VTRN2))]
  "TARGET_NEON"
  "vtrn.<V_sz_elem>\t%<V_reg>0, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_3cycle")))]
)

(define_expand "neon_vtrn<mode>"
  [(match_operand:SI 0 "s_register_operand" "r")
   (match_operand:VDQW 1 "s_register_operand" "w")
   (match_operand:VDQW 2 "s_register_operand" "w")]
  "TARGET_NEON"
{
  neon_emit_pair_result_insn (<MODE>mode, gen_neon_vtrn<mode>_internal,
			      operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "neon_vzip<mode>_internal"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
                      (match_operand:VDQW 2 "s_register_operand" "w")]
                     UNSPEC_VZIP1))
   (set (match_operand:VDQW 3 "s_register_operand" "=2")
        (unspec:VDQW [(match_dup 1) (match_dup 2)]
                     UNSPEC_VZIP2))]
  "TARGET_NEON"
  "vzip.<V_sz_elem>\t%<V_reg>0, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_3cycle")))]
)

(define_expand "neon_vzip<mode>"
  [(match_operand:SI 0 "s_register_operand" "r")
   (match_operand:VDQW 1 "s_register_operand" "w")
   (match_operand:VDQW 2 "s_register_operand" "w")]
  "TARGET_NEON"
{
  neon_emit_pair_result_insn (<MODE>mode, gen_neon_vzip<mode>_internal,
			      operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "neon_vuzp<mode>_internal"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
                      (match_operand:VDQW 2 "s_register_operand" "w")]
                     UNSPEC_VUZP1))
   (set (match_operand:VDQW 3 "s_register_operand" "=2")
        (unspec:VDQW [(match_dup 1) (match_dup 2)]
                     UNSPEC_VUZP2))]
  "TARGET_NEON"
  "vuzp.<V_sz_elem>\t%<V_reg>0, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_3cycle")))]
)

(define_expand "neon_vuzp<mode>"
  [(match_operand:SI 0 "s_register_operand" "r")
   (match_operand:VDQW 1 "s_register_operand" "w")
   (match_operand:VDQW 2 "s_register_operand" "w")]
  "TARGET_NEON"
{
  neon_emit_pair_result_insn (<MODE>mode, gen_neon_vuzp<mode>_internal,
			      operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "neon_vreinterpretv8qi<mode>"
  [(match_operand:V8QI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv4hi<mode>"
  [(match_operand:V4HI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv2si<mode>"
  [(match_operand:V2SI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv2sf<mode>"
  [(match_operand:V2SF 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretdi<mode>"
  [(match_operand:DI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv16qi<mode>"
  [(match_operand:V16QI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv8hi<mode>"
  [(match_operand:V8HI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv4si<mode>"
  [(match_operand:V4SI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv4sf<mode>"
  [(match_operand:V4SF 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv2di<mode>"
  [(match_operand:V2DI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_insn "neon_vld1<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
        (unspec:VDQX [(mem:VDQX (match_operand:SI 1 "s_register_operand" "r"))]
                    UNSPEC_VLD1))]
  "TARGET_NEON"
  "vld1.<V_sz_elem>\t%h0, [%1]"
  [(set_attr "neon_type" "neon_vld1_1_2_regs")]
)

(define_insn "neon_vld1_lane<mode>"
  [(set (match_operand:VDX 0 "s_register_operand" "=w")
        (unspec:VDX [(mem:<V_elem> (match_operand:SI 1 "s_register_operand" "r"))
                     (match_operand:VDX 2 "s_register_operand" "0")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VLD1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  if (max == 1)
    return "vld1.<V_sz_elem>\t%P0, [%1]";
  else
    return "vld1.<V_sz_elem>\t{%P0[%c3]}, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_mode_nunits>") (const_int 2))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld1_vld2_lane")))]
)

(define_insn "neon_vld1_lane<mode>"
  [(set (match_operand:VQX 0 "s_register_operand" "=w")
        (unspec:VQX [(mem:<V_elem> (match_operand:SI 1 "s_register_operand" "r"))
                     (match_operand:VQX 2 "s_register_operand" "0")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VLD1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
      operands[3] = GEN_INT (lane);
    }
  operands[0] = gen_rtx_REG (<V_HALF>mode, regno);
  if (max == 2)
    return "vld1.<V_sz_elem>\t%P0, [%1]";
  else
    return "vld1.<V_sz_elem>\t{%P0[%c3]}, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_mode_nunits>") (const_int 2))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld1_vld2_lane")))]
)

(define_insn "neon_vld1_dup<mode>"
  [(set (match_operand:VDX 0 "s_register_operand" "=w")
        (unspec:VDX [(mem:<V_elem> (match_operand:SI 1 "s_register_operand" "r"))]
                    UNSPEC_VLD1_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    return "vld1.<V_sz_elem>\t{%P0[]}, [%1]";
  else
    return "vld1.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_insn "neon_vld1_dup<mode>"
  [(set (match_operand:VQX 0 "s_register_operand" "=w")
        (unspec:VQX [(mem:<V_elem> (match_operand:SI 1 "s_register_operand" "r"))]
                    UNSPEC_VLD1_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 2)
    return "vld1.<V_sz_elem>\t{%e0[], %f0[]}, [%1]";
  else
    return "vld1.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_insn "neon_vst1<mode>"
  [(set (mem:VDQX (match_operand:SI 0 "s_register_operand" "r"))
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")]
		     UNSPEC_VST1))]
  "TARGET_NEON"
  "vst1.<V_sz_elem>\t%h1, [%0]"
  [(set_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs")])

(define_insn "neon_vst1_lane<mode>"
  [(set (mem:<V_elem> (match_operand:SI 0 "s_register_operand" "r"))
	(vec_select:<V_elem>
	  (match_operand:VDX 1 "s_register_operand" "w")
	  (parallel [(match_operand:SI 2 "neon_lane_number" "i")])))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  if (max == 1)
    return "vst1.<V_sz_elem>\t{%P1}, [%0]";
  else
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, [%0]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_mode_nunits>") (const_int 1))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst1_vst2_lane")))])

(define_insn "neon_vst1_lane<mode>"
  [(set (mem:<V_elem> (match_operand:SI 0 "s_register_operand" "r"))
        (vec_select:<V_elem>
           (match_operand:VQX 1 "s_register_operand" "w")
           (parallel [(match_operand:SI 2 "neon_lane_number" "i")])))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
      operands[2] = GEN_INT (lane);
    }
  operands[1] = gen_rtx_REG (<V_HALF>mode, regno);
  if (max == 2)
    return "vst1.<V_sz_elem>\t{%P1}, [%0]";
  else
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, [%0]";
}
  [(set_attr "neon_type" "neon_vst1_vst2_lane")]
)

(define_insn "neon_vld2<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(mem:TI (match_operand:SI 1 "s_register_operand" "r"))
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, [%1]";
  else
    return "vld2.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")))]
)

(define_insn "neon_vld2<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(mem:OI (match_operand:SI 1 "s_register_operand" "r"))
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2))]
  "TARGET_NEON"
  "vld2.<V_sz_elem>\t%h0, [%1]"
  [(set_attr "neon_type" "neon_vld2_2_regs_vld1_vld2_all_lanes")])

(define_insn "neon_vld2_lane<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(mem:<V_two_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (match_operand:TI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = operands[1];
  ops[3] = operands[3];
  output_asm_insn ("vld2.<V_sz_elem>\t{%P0[%c3], %P1[%c3]}, [%2]", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld1_vld2_lane")]
)

(define_insn "neon_vld2_lane<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(mem:<V_two_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (match_operand:OI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = operands[1];
  ops[3] = GEN_INT (lane);
  output_asm_insn ("vld2.<V_sz_elem>\t{%P0[%c3], %P1[%c3]}, [%2]", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld1_vld2_lane")]
)

(define_insn "neon_vld2_dup<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(mem:<V_two_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    return "vld2.<V_sz_elem>\t{%e0[], %f0[]}, [%1]";
  else
    return "vld1.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_insn "neon_vst2<mode>"
  [(set (mem:TI (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:TI [(match_operand:TI 1 "s_register_operand" "w")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST2))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, [%0]";
  else
    return "vst2.<V_sz_elem>\t%h1, [%0]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")))]
)

(define_insn "neon_vst2<mode>"
  [(set (mem:OI (match_operand:SI 0 "s_register_operand" "r"))
	(unspec:OI [(match_operand:OI 1 "s_register_operand" "w")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VST2))]
  "TARGET_NEON"
  "vst2.<V_sz_elem>\t%h1, [%0]"
  [(set_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs")]
)

(define_insn "neon_vst2_lane<mode>"
  [(set (mem:<V_two_elem> (match_operand:SI 0 "s_register_operand" "r"))
	(unspec:<V_two_elem>
	  [(match_operand:TI 1 "s_register_operand" "w")
	   (match_operand:SI 2 "immediate_operand" "i")
	   (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	  UNSPEC_VST2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = operands[2];
  output_asm_insn ("vst2.<V_sz_elem>\t{%P1[%c3], %P2[%c3]}, [%0]", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst1_vst2_lane")]
)

(define_insn "neon_vst2_lane<mode>"
  [(set (mem:<V_two_elem> (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:<V_two_elem>
           [(match_operand:OI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = GEN_INT (lane);
  output_asm_insn ("vst2.<V_sz_elem>\t{%P1[%c3], %P2[%c3]}, [%0]", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst1_vst2_lane")]
)

(define_insn "neon_vld3<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(mem:EI (match_operand:SI 1 "s_register_operand" "r"))
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, [%1]";
  else
    return "vld3.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld3_vld4")))]
)

(define_expand "neon_vld3<mode>"
  [(match_operand:CI 0 "s_register_operand" "=w")
   (match_operand:SI 1 "s_register_operand" "+r")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vld3qa<mode> (operands[0], operands[0],
                                    operands[1], operands[1]));
  emit_insn (gen_neon_vld3qb<mode> (operands[0], operands[0],
                                    operands[1], operands[1]));
  DONE;
})

(define_insn "neon_vld3qa<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(mem:CI (match_operand:SI 3 "s_register_operand" "2"))
                    (match_operand:CI 1 "s_register_operand" "0")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3A))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (plus:SI (match_dup 3)
		 (const_int 24)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[4];
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = operands[2];
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0, %P1, %P2}, [%3]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld3qb<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(mem:CI (match_operand:SI 3 "s_register_operand" "2"))
                    (match_operand:CI 1 "s_register_operand" "0")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3B))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (plus:SI (match_dup 3)
		 (const_int 24)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[4];
  ops[0] = gen_rtx_REG (DImode, regno + 2);
  ops[1] = gen_rtx_REG (DImode, regno + 6);
  ops[2] = gen_rtx_REG (DImode, regno + 10);
  ops[3] = operands[2];
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0, %P1, %P2}, [%3]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld3_lane<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(mem:<V_three_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (match_operand:EI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = operands[1];
  ops[4] = operands[3];
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0[%c4], %P1[%c4], %P2[%c4]}, [%3]",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld3_lane<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(mem:<V_three_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (match_operand:CI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = operands[1];
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0[%c4], %P1[%c4], %P2[%c4]}, [%3]",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld3_dup<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(mem:<V_three_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    {
      int regno = REGNO (operands[0]);
      rtx ops[4];
      ops[0] = gen_rtx_REG (DImode, regno);
      ops[1] = gen_rtx_REG (DImode, regno + 2);
      ops[2] = gen_rtx_REG (DImode, regno + 4);
      ops[3] = operands[1];
      output_asm_insn ("vld3.<V_sz_elem>\t{%P0[], %P1[], %P2[]}, [%3]", ops);
      return "";
    }
  else
    return "vld1.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld3_vld4_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))])

(define_insn "neon_vst3<mode>"
  [(set (mem:EI (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:EI [(match_operand:EI 1 "s_register_operand" "w")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, [%0]";
  else
    return "vst3.<V_sz_elem>\t%h1, [%0]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst2_4_regs_vst3_vst4")))])

(define_expand "neon_vst3<mode>"
  [(match_operand:SI 0 "s_register_operand" "+r")
   (match_operand:CI 1 "s_register_operand" "w")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vst3qa<mode> (operands[0], operands[0], operands[1]));
  emit_insn (gen_neon_vst3qb<mode> (operands[0], operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vst3qa<mode>"
  [(set (mem:EI (match_operand:SI 1 "s_register_operand" "0"))
        (unspec:EI [(match_operand:CI 2 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3A))
   (set (match_operand:SI 0 "s_register_operand" "=r")
        (plus:SI (match_dup 1)
		 (const_int 24)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[2]);
  rtx ops[4];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1, %P2, %P3}, [%0]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst3qb<mode>"
  [(set (mem:EI (match_operand:SI 1 "s_register_operand" "0"))
        (unspec:EI [(match_operand:CI 2 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3B))
   (set (match_operand:SI 0 "s_register_operand" "=r")
        (plus:SI (match_dup 1)
		 (const_int 24)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[2]);
  rtx ops[4];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 6);
  ops[3] = gen_rtx_REG (DImode, regno + 10);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1, %P2, %P3}, [%0]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst3_lane<mode>"
  [(set (mem:<V_three_elem> (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:<V_three_elem>
           [(match_operand:EI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = gen_rtx_REG (DImode, regno + 4);
  ops[4] = operands[2];
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1[%c4], %P2[%c4], %P3[%c4]}, [%0]",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst3_vst4_lane")]
)

(define_insn "neon_vst3_lane<mode>"
  [(set (mem:<V_three_elem> (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:<V_three_elem>
           [(match_operand:CI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1[%c4], %P2[%c4], %P3[%c4]}, [%0]",
                   ops);
  return "";
}
[(set_attr "neon_type" "neon_vst3_vst4_lane")])

(define_insn "neon_vld4<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(mem:OI (match_operand:SI 1 "s_register_operand" "r"))
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, [%1]";
  else
    return "vld4.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld3_vld4")))]
)

(define_expand "neon_vld4<mode>"
  [(match_operand:XI 0 "s_register_operand" "=w")
   (match_operand:SI 1 "s_register_operand" "+r")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vld4qa<mode> (operands[0], operands[0],
                                    operands[1], operands[1]));
  emit_insn (gen_neon_vld4qb<mode> (operands[0], operands[0],
                                    operands[1], operands[1]));
  DONE;
})

(define_insn "neon_vld4qa<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(mem:XI (match_operand:SI 3 "s_register_operand" "2"))
                    (match_operand:XI 1 "s_register_operand" "0")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4A))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (plus:SI (match_dup 3)
		 (const_int 32)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[5];
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = gen_rtx_REG (DImode, regno + 12);
  ops[4] = operands[2];
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0, %P1, %P2, %P3}, [%4]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld4qb<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(mem:XI (match_operand:SI 3 "s_register_operand" "2"))
                    (match_operand:XI 1 "s_register_operand" "0")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4B))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (plus:SI (match_dup 3)
		 (const_int 32)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[5];
  ops[0] = gen_rtx_REG (DImode, regno + 2);
  ops[1] = gen_rtx_REG (DImode, regno + 6);
  ops[2] = gen_rtx_REG (DImode, regno + 10);
  ops[3] = gen_rtx_REG (DImode, regno + 14);
  ops[4] = operands[2];
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0, %P1, %P2, %P3}, [%4]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld4_lane<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(mem:<V_four_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (match_operand:OI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 6);
  ops[4] = operands[1];
  ops[5] = operands[3];
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0[%c5], %P1[%c5], %P2[%c5], %P3[%c5]}, [%4]",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld4_lane<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(mem:<V_four_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (match_operand:XI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = gen_rtx_REG (DImode, regno + 12);
  ops[4] = operands[1];
  ops[5] = GEN_INT (lane);
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0[%c5], %P1[%c5], %P2[%c5], %P3[%c5]}, [%4]",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld4_dup<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(mem:<V_four_elem> (match_operand:SI 1 "s_register_operand" "r"))
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    {
      int regno = REGNO (operands[0]);
      rtx ops[5];
      ops[0] = gen_rtx_REG (DImode, regno);
      ops[1] = gen_rtx_REG (DImode, regno + 2);
      ops[2] = gen_rtx_REG (DImode, regno + 4);
      ops[3] = gen_rtx_REG (DImode, regno + 6);
      ops[4] = operands[1];
      output_asm_insn ("vld4.<V_sz_elem>\t{%P0[], %P1[], %P2[], %P3[]}, [%4]",
                       ops);
      return "";
    }
  else
    return "vld1.<V_sz_elem>\t%h0, [%1]";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld3_vld4_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_insn "neon_vst4<mode>"
  [(set (mem:OI (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:OI [(match_operand:OI 1 "s_register_operand" "w")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, [%0]";
  else
    return "vst4.<V_sz_elem>\t%h1, [%0]";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst2_4_regs_vst3_vst4")))]
)

(define_expand "neon_vst4<mode>"
  [(match_operand:SI 0 "s_register_operand" "+r")
   (match_operand:XI 1 "s_register_operand" "w")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vst4qa<mode> (operands[0], operands[0], operands[1]));
  emit_insn (gen_neon_vst4qb<mode> (operands[0], operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vst4qa<mode>"
  [(set (mem:OI (match_operand:SI 1 "s_register_operand" "0"))
        (unspec:OI [(match_operand:XI 2 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4A))
   (set (match_operand:SI 0 "s_register_operand" "=r")
        (plus:SI (match_dup 1)
		 (const_int 32)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[2]);
  rtx ops[5];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  ops[4] = gen_rtx_REG (DImode, regno + 12);
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1, %P2, %P3, %P4}, [%0]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst4qb<mode>"
  [(set (mem:OI (match_operand:SI 1 "s_register_operand" "0"))
        (unspec:OI [(match_operand:XI 2 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4B))
   (set (match_operand:SI 0 "s_register_operand" "=r")
        (plus:SI (match_dup 1)
		 (const_int 32)))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[2]);
  rtx ops[5];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 6);
  ops[3] = gen_rtx_REG (DImode, regno + 10);
  ops[4] = gen_rtx_REG (DImode, regno + 14);
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1, %P2, %P3, %P4}, [%0]!", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst4_lane<mode>"
  [(set (mem:<V_four_elem> (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:<V_four_elem>
           [(match_operand:OI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = gen_rtx_REG (DImode, regno + 4);
  ops[4] = gen_rtx_REG (DImode, regno + 6);
  ops[5] = operands[2];
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1[%c5], %P2[%c5], %P3[%c5], %P4[%c5]}, [%0]",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst3_vst4_lane")]
)

(define_insn "neon_vst4_lane<mode>"
  [(set (mem:<V_four_elem> (match_operand:SI 0 "s_register_operand" "r"))
        (unspec:<V_four_elem>
           [(match_operand:XI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  ops[4] = gen_rtx_REG (DImode, regno + 12);
  ops[5] = GEN_INT (lane);
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1[%c5], %P2[%c5], %P3[%c5], %P4[%c5]}, [%0]",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst3_vst4_lane")]
)

(define_expand "neon_vand<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_inv_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_and<mode>3<V_suf64> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vorr<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_ior<mode>3<V_suf64> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_veor<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_xor<mode>3<V_suf64> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vbic<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_bic<mode>3_neon (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vorn<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_inv_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_orn<mode>3_neon (operands[0], operands[1], operands[2]));
  DONE;
})
