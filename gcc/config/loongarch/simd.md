;; Machine Description for LoongArch SIMD instructions for GNU compiler.
;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

;; Integer modes supported by LSX.
(define_mode_iterator ILSX    [V2DI V4SI V8HI V16QI])

;; Integer modes supported by LASX.
(define_mode_iterator ILASX   [V4DI V8SI V16HI V32QI])

;; FP modes supported by LSX
(define_mode_iterator FLSX    [V2DF V4SF])

;; FP modes supported by LASX
(define_mode_iterator FLASX   [V4DF V8SF])

;; All modes supported by LSX
(define_mode_iterator LSX    [ILSX FLSX])

;; ALL modes supported by LASX
(define_mode_iterator LASX   [ILASX FLASX])

;; All integer modes available
(define_mode_iterator IVEC    [(ILSX "ISA_HAS_LSX") (ILASX "ISA_HAS_LASX")])

;; All FP modes available
(define_mode_iterator FVEC    [(FLSX "ISA_HAS_LSX") (FLASX "ISA_HAS_LASX")])

;; All vector modes available
(define_mode_iterator ALLVEC  [(LSX "ISA_HAS_LSX") (LASX "ISA_HAS_LASX")])

;; Mnemonic prefix, "x" for LASX modes.
(define_mode_attr x [(V2DI "") (V4SI "") (V8HI "") (V16QI "")
		     (V2DF "") (V4SF "")
		     (V4DI "x") (V8SI "x") (V16HI "x") (V32QI "x")
		     (V4DF "x") (V8SF "x")])

;; Modifier for vector register, "w" for LSX modes, "u" for LASX modes.
(define_mode_attr wu [(V2DI "w") (V4SI "w") (V8HI "w") (V16QI "w")
		      (V2DF "w") (V4SF "w")
		      (V4DI "u") (V8SI "u") (V16HI "u") (V32QI "u")
		      (V4DF "u") (V8SF "u")])

;; define_insn name prefix, "lsx" or "lasx"
(define_mode_attr simd_isa
  [(V2DI "lsx") (V4SI "lsx") (V8HI "lsx") (V16QI "lsx")
   (V2DF "lsx") (V4SF "lsx")
   (V4DI "lasx") (V8SI "lasx") (V16HI "lasx") (V32QI "lasx")
   (V4DF "lasx") (V8SF "lasx")])

;; Widen integer modes for intermediate values in RTX pattern.
(define_mode_attr WVEC [(V2DI "V2TI") (V4DI "V4TI")
			(V4SI "V4DI") (V8SI "V8DI")
			(V8HI "V8SI") (V16HI "V16SI")
			(V16QI "V16HI") (V32QI "V32HI")])

;; The element type is not changed but the number of elements is halved.
(define_mode_attr VEC_HALF [(V2DI "V1DI") (V4DI "V2DI")
			    (V4SI "V2SI") (V8SI "V4SI")
			    (V8HI "V4HI") (V16HI "V8HI")
			    (V16QI "V8QI") (V32QI "V16QI")])

;; Modes with doubled length for intermediate values in RTX pattern.
(define_mode_attr LVEC [(V2DF "V4DF") (V4DF "V8DF")
			(V4SF "V8SF") (V8SF "V16SF")
			(V2DI "V4DI") (V4DI "V8DI")
			(V4SI "V8SI") (V8SI "V16SI")
			(V8HI "V16HI") (V16HI "V32HI")
			(V16QI "V32QI") (V32QI "V64QI")])

;; The elements are widen but the total size is unchanged
;; (i.e. the number of elements is halfed).
(define_mode_attr WVEC_HALF [(V2DI "V1TI") (V4DI "V2TI")
			     (V4SI "V2DI") (V8SI "V4DI")
			     (V8HI "V4SI") (V16HI "V8SI")
			     (V16QI "V8HI") (V32QI "V16HI")])

;; Lower-case version.
(define_mode_attr wvec_half [(V2DI "v1ti") (V4DI "v2ti")
			     (V4SI "v2di") (V8SI "v4di")
			     (V8HI "v4si") (V16HI "v8si")
			     (V16QI "v8hi") (V32QI "v16hi")])

;; Integer vector modes with the same length and unit size as a mode.
(define_mode_attr VIMODE [(V2DI "V2DI") (V4SI "V4SI")
			  (V8HI "V8HI") (V16QI "V16QI")
			  (V2DF "V2DI") (V4SF "V4SI")
			  (V4DI "V4DI") (V8SI "V8SI")
			  (V16HI "V16HI") (V32QI "V32QI")
			  (V4DF "V4DI") (V8SF "V8SI")])

;; Lower-case version.
(define_mode_attr vimode [(V2DF "v2di") (V4SF "v4si")
			  (V4DF "v4di") (V8SF "v8si")])

;; Integer vector modes with the same size, in lower-case.
(define_mode_attr allmode_i [(V2DI "v2di") (V4SI "v4si")
              (V8HI "v8hi") (V16QI "v16qi")
              (V2DF "v2di") (V4SF "v4si")
              (V4DI "v4di") (V8SI "v8si")
              (V16HI "v16hi") (V32QI "v32qi")
              (V4DF "v4di") (V8SF "v8si")])

;; Suffix for LSX or LASX instructions.
(define_mode_attr simdfmt [(V2DF "d") (V4DF "d")
			   (V4SF "s") (V8SF "s")
			   (V2DI "d") (V4DI "d")
			   (V4SI "w") (V8SI "w")
			   (V8HI "h") (V16HI "h")
			   (V16QI "b") (V32QI "b")])

;; Suffix for widening LSX or LASX instructions.
(define_mode_attr simdfmt_w [(V2DI "q") (V4DI "q")
			     (V4SI "d") (V8SI "d")
			     (V8HI "w") (V16HI "w")
			     (V16QI "h") (V32QI "h")])

;; Suffix for integer mode in LSX or LASX instructions with FP input but
;; integer output.
(define_mode_attr simdifmt_for_f [(V2DF "l") (V4DF "l")
				  (V4SF "w") (V8SF "w")])

;; Suffix for integer mode in LSX or LASX instructions to operate FP
;; vectors using integer vector operations.
(define_mode_attr simdfmt_as_i [(V2DF "d") (V4DF "d")
				(V4SF "w") (V8SF "w")
				(V2DI "d") (V4DI "d")
				(V4SI "w") (V8SI "w")
				(V8HI "h") (V16HI "h")
				(V16QI "b") (V32QI "b")])

;; "_f" for FP vectors, "" for integer vectors
(define_mode_attr _f [(V2DF "_f") (V4DF "_f")
		      (V4SF "_f") (V8SF "_f")
		      (V2DI "") (V4DI "")
		      (V4SI "") (V8SI "")
		      (V8HI "") (V16HI "")
		      (V16QI "") (V32QI "")])

;; Size of vector elements in bits.
(define_mode_attr elmbits [(V2DI "64") (V4DI "64")
			   (V4SI "32") (V8SI "32")
			   (V8HI "16") (V16HI "16")
			   (V16QI "8") (V32QI "8")])

;; The index of sign bit in FP vector elements.
(define_mode_attr elmsgnbit [(V2DF "63") (V4DF "63")
			     (V4SF "31") (V8SF "31")])

;; This attribute is used to form an immediate operand constraint using
;; "const_<bitimm>_operand".
(define_mode_attr bitimm [(V16QI "uimm3") (V32QI "uimm3")
			  (V8HI  "uimm4") (V16HI "uimm4")
			  (V4SI  "uimm5") (V8SI "uimm5")
			  (V2DI  "uimm6") (V4DI "uimm6")])

;; =======================================================================
;; For many LASX instructions, the only difference of it from the LSX
;; counterpart is the length of vector operands.  Describe these LSX/LASX
;; instruction here so we can avoid duplicating logics.
;; =======================================================================


;; Move

;; Some immediate values in V1TI or V2TI may be stored in LSX or LASX
;; registers, thus we need to allow moving them for reload.
(define_mode_iterator ALLVEC_TI [ALLVEC
				 (V1TI "ISA_HAS_LSX")
				 (V2TI "ISA_HAS_LASX")])

(define_expand "mov<mode>"
  [(set (match_operand:ALLVEC_TI 0)
	(match_operand:ALLVEC_TI 1))]
  ""
{
  if (loongarch_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:ALLVEC_TI 0)
	(match_operand:ALLVEC_TI 1))]
  ""
{
  if (loongarch_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

(define_insn_and_split "mov<mode>_simd"
  [(set (match_operand:ALLVEC_TI 0 "nonimmediate_operand" "=f,f,R,*r,*f,*r")
	(match_operand:ALLVEC_TI 1 "move_operand" "fYGYI,R,f,*f,*r,*r"))]
  ""
{ return loongarch_output_move (operands); }
  "reload_completed && loongarch_split_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  loongarch_split_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "simd_move,simd_load,simd_store,simd_copy,simd_insert,simd_copy")
   (set_attr "mode" "<MODE>")])


;; REG + REG load

(define_mode_iterator QIVEC [(V16QI "ISA_HAS_LSX") (V32QI "ISA_HAS_LASX")])
(define_expand "<simd_isa>_<x>vldx"
  [(set (match_operand:QIVEC 0 "register_operand" "=f")
	(mem:QIVEC (plus:DI (match_operand:DI 1 "register_operand")
			    (match_operand:DI 2 "register_operand"))))]
  "TARGET_64BIT")

;;
;; FP vector rounding instructions
;;

(define_c_enum "unspec"
  [UNSPEC_SIMD_FRINTRP
   UNSPEC_SIMD_FRINTRZ
   UNSPEC_SIMD_FRINT
   UNSPEC_SIMD_FRINTRM
   UNSPEC_SIMD_FRINTRNE])

(define_int_iterator SIMD_FRINT
  [UNSPEC_SIMD_FRINTRP
   UNSPEC_SIMD_FRINTRZ
   UNSPEC_SIMD_FRINT
   UNSPEC_SIMD_FRINTRM
   UNSPEC_SIMD_FRINTRNE])

(define_int_attr simd_frint_rounding
  [(UNSPEC_SIMD_FRINTRP		"rp")
   (UNSPEC_SIMD_FRINTRZ		"rz")
   (UNSPEC_SIMD_FRINT		"")
   (UNSPEC_SIMD_FRINTRM		"rm")
   (UNSPEC_SIMD_FRINTRNE	"rne")])

;; All these, but rint, are controlled by -ffp-int-builtin-inexact.
;; Note: nearbyint is NOT allowed to raise FE_INEXACT even if
;; -ffp-int-builtin-inexact, but rint is ALLOWED to raise it even if
;; -fno-fp-int-builtin-inexact.
(define_int_attr simd_frint_pattern
  [(UNSPEC_SIMD_FRINTRP		"ceil")
   (UNSPEC_SIMD_FRINTRZ		"btrunc")
   (UNSPEC_SIMD_FRINT		"rint")
   (UNSPEC_SIMD_FRINTRNE	"roundeven")
   (UNSPEC_SIMD_FRINTRM		"floor")])

;; <x>vfrint.{/rp/rz/rm}
(define_insn "<simd_isa>_<x>vfrint<simd_frint_rounding>_<simdfmt>"
  [(set (match_operand:FVEC 0 "register_operand" "=f")
	(unspec:FVEC [(match_operand:FVEC 1 "register_operand" "f")]
		     SIMD_FRINT))]
  ""
  "<x>vfrint<simd_frint_rounding>.<simdfmt>\t%<wu>0,%<wu>1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "<MODE>")])

;; Expand the standard-named patterns to <x>vfrint instructions if
;; raising inexact exception is allowed.

(define_expand "<simd_frint_pattern><mode>2"
  [(set (match_operand:FVEC 0 "register_operand" "=f")
	(unspec:FVEC [(match_operand:FVEC 1 "register_operand" "f")]
		     SIMD_FRINT))]
   "<SIMD_FRINT> == UNSPEC_SIMD_FRINT ||
    flag_fp_int_builtin_inexact ||
    !flag_trapping_math")

;; ftrunc is like btrunc, but it's allowed to raise inexact exception
;; even if -fno-fp-int-builtin-inexact.
(define_expand "ftrunc<mode>2"
  [(set (match_operand:FVEC 0 "register_operand" "=f")
	(unspec:FVEC [(match_operand:FVEC 1 "register_operand" "f")]
		     UNSPEC_SIMD_FRINTRZ))]
  "")

;; Use LSX for scalar ceil/floor/trunc/roundeven when -mlsx and -ffp-int-
;; builtin-inexact.  The base FP instruction set lacks these operations.
;; Yes we are wasting 50% or even 75% of the CPU horsepower, but it's still
;; much faster than calling a libc function: on LA464 and LA664 there is a
;; 3x ~ 5x speed up.
;;
;; Note that a vreplvei instruction is needed or we'll also operate on the
;; junk in high bits of the vector register and produce random FP exceptions.

(define_int_iterator LSX_SCALAR_FRINT
  [UNSPEC_SIMD_FRINTRP
   UNSPEC_SIMD_FRINTRZ
   UNSPEC_SIMD_FRINTRM
   UNSPEC_SIMD_FRINTRNE])

(define_mode_attr VLSX_FOR_FMODE [(DF "V2DF") (SF "V4SF")])

(define_expand "<simd_frint_pattern><mode>2"
  [(set (match_dup 2)
     (vec_duplicate:<VLSX_FOR_FMODE>
       (match_operand:ANYF 1 "register_operand")))
   (set (match_dup 2)
	(unspec:<VLSX_FOR_FMODE> [(match_dup 2)] LSX_SCALAR_FRINT))
   (set (match_operand:ANYF 0 "register_operand")
	(vec_select:ANYF (match_dup 2) (parallel [(const_int 0)])))]
  "ISA_HAS_LSX && (flag_fp_int_builtin_inexact || !flag_trapping_math)"
  "operands[2] = gen_reg_rtx (<VLSX_FOR_FMODE>mode);")

;; <x>vftint.{/rp/rz/rm}
(define_insn
  "<simd_isa>_<x>vftint<simd_frint_rounding>_<simdifmt_for_f>_<simdfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(fix:<VIMODE>
	  (unspec:FVEC [(match_operand:FVEC 1 "register_operand" "f")]
		       SIMD_FRINT)))]
  ""
  "<x>vftint<simd_frint_rounding>.<simdifmt_for_f>.<simdfmt>\t%<wu>0,%<wu>1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "<MODE>")])

;; Expand the standard-named patterns to <x>vftint instructions if
;; raising inexact exception.

(define_expand "l<simd_frint_pattern><mode><vimode>2"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(fix:<VIMODE>
	  (unspec:FVEC [(match_operand:FVEC 1 "register_operand" "f")]
		       SIMD_FRINT)))]
   "<SIMD_FRINT> == UNSPEC_SIMD_FRINT ||
    flag_fp_int_builtin_inexact ||
    !flag_trapping_math")

;; fix_trunc is allowed to raise inexact exception even if
;; -fno-fp-int-builtin-inexact.  Because the middle end trys to match
;; (FIX x) and it does not know (FIX (UNSPEC_SIMD_FRINTRZ x)), we need
;; to use define_insn_and_split instead of define_expand (expanders are
;; not considered during matching).
(define_insn_and_split "fix_trunc<mode><vimode>2"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(fix:<VIMODE> (match_operand:FVEC 1 "register_operand" "f")))]
  ""
  "#"
  ""
  [(const_int 0)]
  {
    emit_insn (gen_<simd_isa>_<x>vftintrz_<simdifmt_for_f>_<simdfmt> (
      operands[0], operands[1]));
    DONE;
  }
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "<MODE>")])

;; <x>vmuh.{b/h/w/d}

(define_code_attr muh
  [(sign_extend "smul_highpart")
   (zero_extend "umul_highpart")])

(define_insn "<su>mul<mode>3_highpart"
  [(set (match_operand:IVEC 0 "register_operand" "=f")
	(<muh>:IVEC (match_operand:IVEC 1 "register_operand" "f")
		    (match_operand:IVEC 2 "register_operand" "f")))
   (any_extend (const_int 0))]
  ""
  "<x>vmuh.<simdfmt><u>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

;; <x>vrotr.{b/h/w/d}

(define_insn "vrotr<mode>3"
  [(set (match_operand:IVEC 0 "register_operand" "=f")
	(rotatert:IVEC (match_operand:IVEC 1 "register_operand" "f")
		       (match_operand:IVEC 2 "register_operand" "f")))]
  ""
  "<x>vrotr.<simdfmt>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

;; Expand left rotate to right rotate.
(define_expand "vrotl<mode>3"
  [(set (match_dup 3)
	(neg:IVEC (match_operand:IVEC 2 "register_operand")))
   (set (match_operand:IVEC 0 "register_operand")
	(rotatert:IVEC (match_operand:IVEC 1 "register_operand")
		       (match_dup 3)))]
  ""
  {
    operands[3] = gen_reg_rtx (<MODE>mode);
  });

;; Expand left rotate with a scalar amount to right rotate: negate the
;; scalar before broadcasting it because scalar negation is cheaper than
;; vector negation.
(define_expand "rotl<mode>3"
  [(set (match_dup 3)
	(neg:SI (match_operand:SI 2 "register_operand")))
   (set (match_dup 4)
	(vec_duplicate:IVEC (subreg:<IVEC:UNITMODE> (match_dup 3) 0)))
   (set (match_operand:IVEC 0 "register_operand")
	(rotatert:IVEC (match_operand:IVEC 1 "register_operand")
		       (match_dup 4)))]
  ""
  {
    operands[3] = gen_reg_rtx (SImode);
    operands[4] = gen_reg_rtx (<MODE>mode);
  });

;; <x>v{rotr/sll/sra/srl}i.{b/h/w/d}

(define_insn "<optab><mode>3"
  [(set (match_operand:IVEC 0 "register_operand" "=f")
	(shift_w:IVEC
	  (match_operand:IVEC 1 "register_operand" "f")
	  (match_operand:SI 2 "const_<bitimm>_operand")))]
  "ISA_HAS_LSX"
  "<x>v<insn>i.<simdfmt>\t%<wu>0,%<wu>1,%2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

;; <x>vfcmp.*.{s/d} with defined RTX code
;; There are no fcmp.{sugt/suge/cgt/cge}.{s/d} menmonics in GAS, so we have
;; to reverse the operands ourselves :(.
(define_code_iterator fcond_simd [unordered uneq unlt unle eq lt le
				  ordered ltgt ne])
(define_insn "<simd_isa>_<x>vfcmp_<fcond>_<simdfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(fcond_simd:<VIMODE>
	  (match_operand:FVEC 1 "register_operand" "f")
	  (match_operand:FVEC 2 "register_operand" "f")))]
  ""
  "<x>vfcmp.<fcond>.<simdfmt>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_fcmp")
   (set_attr "mode" "<MODE>")])

;; There are no fcmp.{sge/sgt/cuge/cugt}.{s/d} menmonics in GAS, so we have
;; to reverse the operands ourselves.
(define_code_iterator fcond_simd_rev [ge gt unge ungt])

(define_code_attr fcond_rev_asm
  [(ge		"sle")
   (gt		"slt")
   (unge	"cule")
   (ungt	"cult")])

(define_insn "<simd_isa>_<x>vfcmp_<fcond>_<simdfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(fcond_simd_rev:<VIMODE>
	  (match_operand:FVEC 1 "register_operand" "f")
	  (match_operand:FVEC 2 "register_operand" "f")))]
  ""
  "<x>vfcmp.<fcond_rev_asm>.<simdfmt>\t%<wu>0,%<wu>2,%<wu>1";
  [(set_attr "type" "simd_fcmp")
   (set_attr "mode" "<MODE>")])

;; <x>vfcmp.*.{s/d} without defined RTX code, but with defined RTX code for
;; its inverse.  Again, there are no fcmp.{sugt/suge/cgt/cge}.{s/d}
;; menmonics in GAS, so we have to reverse the operands ourselves.
(define_code_iterator fcond_inv [ge gt unge ungt])
(define_code_iterator fcond_inv_rev [le lt unle unlt])
(define_code_attr fcond_inv
  [(ge		"sult")
   (gt		"sule")
   (unge	"clt")
   (ungt	"cle")
   (le		"sugt")
   (lt		"suge")
   (unle	"cgt")
   (unlt	"cge")])
(define_code_attr fcond_inv_rev_asm
  [(le		"sult")
   (lt		"sule")
   (unle	"clt")
   (unlt	"cle")])

(define_insn "<simd_isa>_<x>vfcmp_<fcond_inv>_<simdfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(not:<VIMODE>
	  (fcond_inv:<VIMODE>
	    (match_operand:FVEC 1 "register_operand" "f")
	    (match_operand:FVEC 2 "register_operand" "f"))))]
  ""
  "<x>vfcmp.<fcond_inv>.<simdfmt>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_fcmp")
   (set_attr "mode" "<MODE>")])

(define_insn "<simd_isa>_<x>vfcmp_<fcond_inv>_<simdfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(not:<VIMODE>
	  (fcond_inv_rev:<VIMODE>
	    (match_operand:FVEC 1 "register_operand" "f")
	    (match_operand:FVEC 2 "register_operand" "f"))))]
  ""
  "<x>vfcmp.<fcond_inv_rev_asm>.<simdfmt>\t%<wu>0,%<wu>2,%<wu>1"
  [(set_attr "type" "simd_fcmp")
   (set_attr "mode" "<MODE>")])

;; <x>vfcmp.*.{s/d} instructions only as instrinsics
(define_c_enum "unspec"
  [UNSPEC_SIMD_FCMP_CAF
   UNSPEC_SIMD_FCMP_SAF
   UNSPEC_SIMD_FCMP_SEQ
   UNSPEC_SIMD_FCMP_SUN
   UNSPEC_SIMD_FCMP_SUEQ
   UNSPEC_SIMD_FCMP_CNE
   UNSPEC_SIMD_FCMP_SOR
   UNSPEC_SIMD_FCMP_SUNE])

(define_int_iterator SIMD_FCMP
  [UNSPEC_SIMD_FCMP_CAF
   UNSPEC_SIMD_FCMP_SAF
   UNSPEC_SIMD_FCMP_SEQ
   UNSPEC_SIMD_FCMP_SUN
   UNSPEC_SIMD_FCMP_SUEQ
   UNSPEC_SIMD_FCMP_CNE
   UNSPEC_SIMD_FCMP_SOR
   UNSPEC_SIMD_FCMP_SUNE])

(define_int_attr fcond_unspec
  [(UNSPEC_SIMD_FCMP_CAF	"caf")
   (UNSPEC_SIMD_FCMP_SAF	"saf")
   (UNSPEC_SIMD_FCMP_SEQ	"seq")
   (UNSPEC_SIMD_FCMP_SUN	"sun")
   (UNSPEC_SIMD_FCMP_SUEQ	"sueq")
   (UNSPEC_SIMD_FCMP_CNE	"cne")
   (UNSPEC_SIMD_FCMP_SOR	"sor")
   (UNSPEC_SIMD_FCMP_SUNE	"sune")])

(define_insn "<simd_isa>_<x>vfcmp_<fcond_unspec>_<simdfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(unspec:<VIMODE> [(match_operand:FVEC 1 "register_operand" "f")
			  (match_operand:FVEC 2 "register_operand" "f")]
			 SIMD_FCMP))]
  ""
  "<x>vfcmp.<fcond_unspec>.<simdfmt>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_fcmp")
   (set_attr "mode" "<MODE>")])

; [x]vf{min/max} instructions are IEEE-754-2008 conforming, use them for
; the corresponding IEEE-754-2008 operations.  We must use UNSPEC instead
; of smin/smax though, see PR105414 and PR107013.

(define_int_iterator UNSPEC_FMAXMIN [UNSPEC_FMAX UNSPEC_FMIN])
(define_int_attr fmaxmin [(UNSPEC_FMAX "fmax") (UNSPEC_FMIN "fmin")])

(define_insn "<fmaxmin><mode>3"
  [(set (match_operand:FVEC 0 "register_operand" "=f")
	(unspec:FVEC [(match_operand:FVEC 1 "register_operand" "f")
		      (match_operand:FVEC 2 "register_operand" "f")]
		     UNSPEC_FMAXMIN))]
  ""
  "<x>v<fmaxmin>.<simdfmt>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

;; ... and also reduc operations.
(define_expand "reduc_<fmaxmin>_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:FVEC 1 "register_operand")
   (const_int UNSPEC_FMAXMIN)]
  ""
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_<fmaxmin><mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

;; FP negation.
(define_insn "neg<mode>2"
  [(set (match_operand:FVEC 0 "register_operand" "=f")
	(neg:FVEC (match_operand:FVEC 1 "register_operand" "f")))]
  ""
  "<x>vbitrevi.<simdfmt_as_i>\t%<wu>0,%<wu>1,<elmsgnbit>"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

;; vector compare
(define_expand "vec_cmp<mode><allmode_i>"
  [(set (match_operand:<VIMODE> 0 "register_operand")
    (match_operator 1 ""
      [(match_operand:ALLVEC 2 "register_operand")
       (match_operand:ALLVEC 3 "nonmemory_operand")]))]
  ""
{
  loongarch_expand_vec_cmp (operands);
  DONE;
})

(define_expand "vec_cmpu<mode><allmode_i>"
  [(set (match_operand:<VIMODE> 0 "register_operand")
    (match_operator 1 ""
      [(match_operand:IVEC 2 "register_operand")
       (match_operand:IVEC 3 "nonmemory_operand")]))]
  ""
{
  loongarch_expand_vec_cmp (operands);
  DONE;
})

;; cbranch
(define_expand "cbranch<mode>4"
 [(set (pc)
       (if_then_else
         (match_operator 0 "equality_operator"
           [(match_operand:IVEC 1 "register_operand")
            (match_operand:IVEC 2 "reg_or_vector_same_val_operand")])
         (label_ref (match_operand 3 ""))
         (pc)))]
 ""
{
  RTX_CODE code = GET_CODE (operands[0]);
  rtx tmp = operands[1];
  rtx const0 = CONST0_RTX (SImode);

  /* If comparing against a non-zero vector we have to do a comparison first
    so we can have a != 0 comparison with the result.  */
  if (operands[2] != CONST0_RTX (<MODE>mode))
    {
      tmp = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_xor<mode>3 (tmp, operands[1], operands[2]));
    }

  if (code == NE)
    emit_jump_insn (gen_<simd_isa>_<x>bnz_v_b (operands[3], tmp, const0));
  else
    emit_jump_insn (gen_<simd_isa>_<x>bz_v_b (operands[3], tmp, const0));
  DONE;
})

;; Operations on elements at even/odd indices.
(define_int_iterator zero_one [0 1])
(define_int_attr ev_od [(0 "ev") (1 "od")])
(define_int_attr even_odd [(0 "even") (1 "odd")])

;; Integer widening add/sub/mult.
(define_insn "simd_<optab>w_evod_<mode>_<su>"
  [(set (match_operand:<WVEC_HALF> 0 "register_operand" "=f")
	(addsubmul:<WVEC_HALF>
	  (any_extend:<WVEC_HALF>
	    (vec_select:<VEC_HALF>
	      (match_operand:IVEC 1 "register_operand" "f")
	      (match_operand:IVEC 3 "vect_par_cnst_even_or_odd_half")))
	  (any_extend:<WVEC_HALF>
	    (vec_select:<VEC_HALF>
	      (match_operand:IVEC 2 "register_operand" "f")
	      (match_dup 3)))))]
  ""
  "<x>v<optab>w%O3.<simdfmt_w>.<simdfmt><u>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<WVEC_HALF>")])

(define_expand "<simd_isa>_<x>v<optab>w<ev_od>_<simdfmt_w>_<simdfmt><u>"
  [(match_operand:<WVEC_HALF> 0 "register_operand" "=f")
   (match_operand:IVEC	      1 "register_operand" " f")
   (match_operand:IVEC	      2 "register_operand" " f")
   (any_extend (const_int 0))
   (addsubmul (const_int 0) (const_int 0))
   (const_int zero_one)]
  ""
{
  int nelts = GET_MODE_NUNITS (<WVEC_HALF>mode);
  rtx op3 = loongarch_gen_stepped_int_parallel (nelts, <zero_one>, 2);
  rtx insn = gen_simd_<optab>w_evod_<mode>_<su> (operands[0], operands[1],
						 operands[2], op3);
  emit_insn (insn);
  DONE;
})

(define_expand "vec_widen_<su>mult_<even_odd>_<mode>"
  [(match_operand:<WVEC_HALF> 0 "register_operand" "=f")
   (match_operand:IVEC	      1 "register_operand" " f")
   (match_operand:IVEC	      2 "register_operand" " f")
   (any_extend (const_int 0))
   (const_int zero_one)]
  ""
{
  emit_insn (
    gen_<simd_isa>_<x>vmulw<ev_od>_<simdfmt_w>_<simdfmt><u> (operands[0],
							     operands[1],
							     operands[2]));
  DONE;
})

(define_insn "simd_<optab>w_evod_<mode>_hetero"
  [(set (match_operand:<WVEC_HALF> 0 "register_operand" "=f")
	(addsubmul:<WVEC_HALF>
	  (zero_extend:<WVEC_HALF>
	    (vec_select:<VEC_HALF>
	      (match_operand:IVEC 1 "register_operand" "f")
	      (match_operand:IVEC 3 "vect_par_cnst_even_or_odd_half")))
	  (sign_extend:<WVEC>
	    (vec_select:<VEC_HALF>
	      (match_operand:IVEC 2 "register_operand" "f")
	      (match_dup 3)))))]
  ""
  "<x>v<optab>w%O3.<simdfmt_w>.<simdfmt>u.<simdfmt>\t%<wu>0,%<wu>1,%<wu>2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<WVEC_HALF>")])

(define_expand "<simd_isa>_<x>v<optab>w<ev_od>_<simdfmt_w>_<simdfmt>u_<simdfmt>"
  [(match_operand:<WVEC_HALF> 0 "register_operand" "=f")
   (match_operand:IVEC	      1 "register_operand" " f")
   (match_operand:IVEC	      2 "register_operand" " f")
   (addmul (const_int 0) (const_int 0))
   (const_int zero_one)]
  ""
{
  int nelts = GET_MODE_NUNITS (<WVEC_HALF>mode);
  rtx op3 = loongarch_gen_stepped_int_parallel (nelts, <zero_one>, 2);
  rtx insn = gen_simd_<optab>w_evod_<mode>_hetero (operands[0], operands[1],
						   operands[2], op3);
  emit_insn (insn);
  DONE;
})

(define_insn "simd_h<optab>w_<mode>_<su>"
  [(set (match_operand:<WVEC_HALF> 0 "register_operand" "=f")
	(addsub:<WVEC_HALF>
	  (any_extend:<WVEC_HALF>
	    (vec_select:<VEC_HALF>
	      (match_operand:IVEC 1 "register_operand" "f")
	      (match_operand:IVEC 3 "vect_par_cnst_even_or_odd_half")))
	  (any_extend:<WVEC_HALF>
	    (vec_select:<VEC_HALF>
	      (match_operand:IVEC 2 "register_operand" "f")
	      (match_operand:IVEC 4 "vect_par_cnst_even_or_odd_half")))))]
  "!rtx_equal_p (operands[3], operands[4])"
{
  if (!INTVAL (XVECEXP (operands[3], 0, 0)))
    std::swap (operands[1], operands[2]);
  return "<x>vh<optab>w.<simdfmt_w><u>.<simdfmt><u>\t%<wu>0,%<wu>1,%<wu>2";
}
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<WVEC_HALF>")])

(define_expand "<simd_isa>_<x>vh<optab>w_<simdfmt_w><u>_<simdfmt><u>"
  [(match_operand:<WVEC_HALF> 0 "register_operand" "=f")
   (match_operand:IVEC	      1 "register_operand" " f")
   (match_operand:IVEC	      2 "register_operand" " f")
   (any_extend (const_int 0))
   (addsub (const_int 0) (const_int 0))]
  ""
{
  int nelts = GET_MODE_NUNITS (<WVEC_HALF>mode);
  rtx op3 = loongarch_gen_stepped_int_parallel (nelts, 1, 2);
  rtx op4 = loongarch_gen_stepped_int_parallel (nelts, 0, 2);
  rtx insn = gen_simd_h<optab>w_<mode>_<su> (operands[0], operands[1],
					     operands[2], op3, op4);
  emit_insn (insn);
  DONE;
})

(define_insn "simd_maddw_evod_<mode>_<su>"
  [(set (match_operand:<WVEC_HALF> 0 "register_operand" "=f")
	(plus:<WVEC_HALF>
	  (mult:<WVEC_HALF>
	    (any_extend:<WVEC_HALF>
	      (vec_select:<VEC_HALF>
		(match_operand:IVEC 2 "register_operand" "f")
		(match_operand:IVEC 4 "vect_par_cnst_even_or_odd_half")))
	    (any_extend:<WVEC>
	      (vec_select:<VEC_HALF>
		(match_operand:IVEC 3 "register_operand" "f")
		(match_dup 4))))
	  (match_operand:<WVEC_HALF> 1 "register_operand" "0")))]
  ""
  "<x>vmaddw%O4.<simdfmt_w>.<simdfmt><u>\t%<wu>0,%<wu>2,%<wu>3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<WVEC_HALF>")])

(define_expand "<simd_isa>_<x>vmaddw<ev_od>_<simdfmt_w>_<simdfmt><u>"
  [(match_operand:<WVEC_HALF> 0 "register_operand" "=f")
   (match_operand:<WVEC_HALF> 1 "register_operand" " 0")
   (match_operand:IVEC	      2 "register_operand" " f")
   (match_operand:IVEC	      3 "register_operand" " f")
   (any_extend (const_int 0))
   (const_int zero_one)]
  ""
{
  int nelts = GET_MODE_NUNITS (<WVEC_HALF>mode);
  rtx op4 = loongarch_gen_stepped_int_parallel (nelts, <zero_one>, 2);
  rtx insn = gen_simd_maddw_evod_<mode>_<su> (operands[0], operands[1],
					      operands[2], operands[3],
					      op4);
  emit_insn (insn);
  DONE;
})

(define_expand "<su>dot_prod<wvec_half><mode>"
  [(match_operand:<WVEC_HALF> 0 "register_operand" "=f,f")
   (match_operand:IVEC	      1 "register_operand" " f,f")
   (match_operand:IVEC	      2 "register_operand" " f,f")
   (match_operand:<WVEC_HALF> 3 "reg_or_0_operand" " 0,YG")
   (any_extend (const_int 0))]
  ""
{
  rtx *op = operands;

  if (op[3] == CONST0_RTX (<WVEC_HALF>mode))
    emit_insn (
      gen_<simd_isa>_<x>vmulwev_<simdfmt_w>_<simdfmt><u> (op[0], op[1],
							  op[2]));
  else
    emit_insn (
      gen_<simd_isa>_<x>vmaddwev_<simdfmt_w>_<simdfmt><u> (op[0], op[3],
							   op[1], op[2]));

  emit_insn (
    gen_<simd_isa>_<x>vmaddwod_<simdfmt_w>_<simdfmt><u> (op[0], op[0],
							 op[1], op[2]));
  DONE;
})

(define_insn "simd_maddw_evod_<mode>_hetero"
  [(set (match_operand:<WVEC_HALF> 0 "register_operand" "=f")
	(plus:<WVEC_HALF>
	  (mult:<WVEC_HALF>
	    (zero_extend:<WVEC_HALF>
	      (vec_select:<VEC_HALF>
		(match_operand:IVEC 2 "register_operand" "f")
		(match_operand:IVEC 4 "vect_par_cnst_even_or_odd_half")))
	    (sign_extend:<WVEC_HALF>
	      (vec_select:<VEC_HALF>
		(match_operand:IVEC 3 "register_operand" "f")
		(match_dup 4))))
	  (match_operand:<WVEC_HALF> 1 "register_operand" "0")))]
  ""
  "<x>vmaddw%O4.<simdfmt_w>.<simdfmt>u.<simdfmt>\t%<wu>0,%<wu>2,%<wu>3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<WVEC_HALF>")])

(define_expand "<simd_isa>_<x>vmaddw<ev_od>_<simdfmt_w>_<simdfmt>u_<simdfmt>"
  [(match_operand:<WVEC_HALF> 0 "register_operand" "=f")
   (match_operand:<WVEC_HALF> 1 "register_operand" " 0")
   (match_operand:IVEC	      2 "register_operand" " f")
   (match_operand:IVEC	      3 "register_operand" " f")
   (const_int zero_one)]
  ""
{
  int nelts = GET_MODE_NUNITS (<WVEC_HALF>mode);
  rtx op4 = loongarch_gen_stepped_int_parallel (nelts, <zero_one>, 2);
  rtx insn = gen_simd_maddw_evod_<mode>_hetero (operands[0], operands[1],
						operands[2], operands[3],
						op4);
  emit_insn (insn);
  DONE;
})

; For "historical" reason we need a punned version of q_d variants.
(define_mode_iterator DIVEC [(V2DI "ISA_HAS_LSX") (V4DI "ISA_HAS_LASX")])

(define_expand "<simd_isa>_<optab>w<ev_od>_q_d<u>_punned"
  [(match_operand:DIVEC 0 "register_operand" "=f")
   (match_operand:DIVEC 1 "register_operand" " f")
   (match_operand:DIVEC 2 "register_operand" " f")
   (any_extend (const_int 0))
   (addsubmul (const_int 0) (const_int 0))
   (const_int zero_one)]
  ""
{
  rtx t = gen_reg_rtx (<WVEC_HALF>mode);
  emit_insn (gen_<simd_isa>_<x>v<optab>w<ev_od>_q_d<u> (t, operands[1],
							operands[2]));
  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, t));
  DONE;
})

(define_expand "<simd_isa>_<optab>w<ev_od>_q_du_d_punned"
  [(match_operand:DIVEC 0 "register_operand" "=f")
   (match_operand:DIVEC 1 "register_operand" " f")
   (match_operand:DIVEC 2 "register_operand" " f")
   (addmul (const_int 0) (const_int 0))
   (const_int zero_one)]
  ""
{
  rtx t = gen_reg_rtx (<WVEC_HALF>mode);
  emit_insn (gen_<simd_isa>_<x>v<optab>w<ev_od>_q_du_d (t, operands[1],
							operands[2]));
  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, t));
  DONE;
})

(define_expand "<simd_isa>_h<optab>w_q<u>_d<u>_punned"
  [(match_operand:DIVEC 0 "register_operand" "=f")
   (match_operand:DIVEC 1 "register_operand" "f")
   (match_operand:DIVEC 2 "register_operand" "f")
   (any_extend (const_int 0))
   (addsub (const_int 0) (const_int 0))]
  ""
{
  rtx t = gen_reg_rtx (<WVEC_HALF>mode);
  emit_insn (gen_<simd_isa>_<x>vh<optab>w_q<u>_d<u> (t, operands[1],
						     operands[2]));
  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, t));
  DONE;
})

(define_expand "<simd_isa>_maddw<ev_od>_q_d<u>_punned"
  [(match_operand:DIVEC 0 "register_operand" "=f")
   (match_operand:DIVEC 1 "register_operand" " 0")
   (match_operand:DIVEC 2 "register_operand" " f")
   (match_operand:DIVEC 3 "register_operand" " f")
   (const_int zero_one)
   (any_extend (const_int 0))]
  ""
{
  rtx t = gen_reg_rtx (<WVEC_HALF>mode);
  rtx op1 = gen_lowpart (<WVEC_HALF>mode, operands[1]);
  emit_insn (gen_<simd_isa>_<x>vmaddw<ev_od>_q_d<u> (t, op1, operands[2],
						     operands[3]));
  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, t));
  DONE;
})

(define_expand "<simd_isa>_maddw<ev_od>_q_du_d_punned"
  [(match_operand:DIVEC 0 "register_operand" "=f")
   (match_operand:DIVEC 1 "register_operand" " 0")
   (match_operand:DIVEC 2 "register_operand" " f")
   (match_operand:DIVEC 3 "register_operand" " f")
   (const_int zero_one)]
  ""
{
  rtx t = gen_reg_rtx (<WVEC_HALF>mode);
  rtx op1 = gen_lowpart (<WVEC_HALF>mode, operands[1]);
  emit_insn (gen_<simd_isa>_<x>vmaddw<ev_od>_q_du_d (t, op1, operands[2],
						     operands[3]));
  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, t));
  DONE;
})

;; Integer shift right with rounding.
(define_insn "simd_<optab>_imm_round_<mode>"
  [(set (match_operand:IVEC 0 "register_operand" "=f")
	(any_shiftrt:IVEC
	  (plus:IVEC
	    (match_operand:IVEC 1 "register_operand" "f")
	    (match_operand:IVEC 2 "const_vector_same_val_operand" "Uuvx"))
	  (match_operand:SI 3 "const_<bitimm>_operand" "I")))]
  "(HOST_WIDE_INT_1U << UINTVAL (operands[3]) >> 1)
   == UINTVAL (CONST_VECTOR_ELT (operands[2], 0))"
  "<x>v<insn>ri.<simdfmt>\t%<wu>0,%<wu>1,%d3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_expand "<simd_isa>_<x>v<insn>ri_<simdfmt>"
  [(match_operand:IVEC 0 "register_operand" "=f")
   (match_operand:IVEC 1 "register_operand" " f")
   (match_operand 2 "const_<bitimm>_operand")
   (any_shiftrt (const_int 0) (const_int 0))]
  ""
{
  auto addend = HOST_WIDE_INT_1U << UINTVAL (operands[2]) >> 1;
  rtx addend_v = loongarch_gen_const_int_vector (<MODE>mode, addend);

  emit_insn (gen_simd_<optab>_imm_round_<mode> (operands[0], operands[1],
						addend_v, operands[2]));
  DONE;
})

; The LoongArch SX Instructions.
(include "lsx.md")

; The LoongArch ASX Instructions.
(include "lasx.md")
