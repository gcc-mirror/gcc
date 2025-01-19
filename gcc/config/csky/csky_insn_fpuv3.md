;; C-SKY FPUV3 instruction descriptions.
;; Copyright (C) 2018-2025 Free Software Foundation, Inc.
;; Contributed by C-SKY Microsystems and Mentor Graphics.
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
;; <http://www.gnu.org/licenses/>.  */

(define_c_enum "unspec" [
  UNSPEC_MAXNM_F3
  UNSPEC_MINNM_F3
])

;; -------------------------------------------------------------------------
;; Float mov instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_movhf"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=r,r,v,r,m,r,Q,v,v,v, v")
	(match_operand:HF 1 "general_operand"      " r,F,r,v,r,m,v,Q,v,W,Dv"))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "*
  switch (which_alternative)
    {
    case 2:
      return \"fmtvr.16\\t%0, %1\";
    case 3:
      return \"fmfvr.16\\t%0, %1\";
    case 6:
    case 7:
    case 9:
      return fpuv3_output_move(operands);
    case 8:
      return \"fmov.16\\t%0, %1\";
    case 10:
      return \"fmovi.16\\t%0, %1\";
    case 1:
      {
	long bits;
	rtx ops[4];

	bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (operands[1]), HFmode);
	ops[0] = operands[0];
	ops[1] = GEN_INT (bits);

	output_asm_insn (\"lrw\\t%0, %1\", ops);
	return \"\";
      }
    default:
      return csky_output_move(insn, operands, HFmode);
    }
  "
)

(define_insn "*fpv3_movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r, r,m,v,r,Q,v,v,v, v")
	(match_operand:SF 1 "general_operand"      " r,m,mF,r,r,v,v,Q,v,W,Dv"))]
  "CSKY_ISA_FEATURE(fpv3_sf)"
  "*
  switch (which_alternative)
    {
    case 4:
      return \"fmtvr.32.1\\t%0, %1\";
    case 5:
      return \"fmfvr.32.1\\t%0, %1\";
    case 6:
    case 7:
    case 9:
      return fpuv3_output_move(operands);
    case 8:
      return \"fmov.32\\t%0, %1\";
    case 10:
      return \"fmovi.32\\t%0, %1\";
    default:
      return csky_output_move(insn, operands, SFmode);
    }
  "
)

(define_insn "*fpv3_movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r, v,?r,Q,v,v,v, v,r, r,Y")
	(match_operand:DF 1 "general_operand"      " r,?r, v,v,Q,v,m,Dv,Y,YF,r"))]
  "CSKY_ISA_FEATURE(fpv3_df)"
  "*
  switch (which_alternative)
    {
    case 1:
      if (TARGET_BIG_ENDIAN)
	return \"fmtvr.64\\t%0, %R1, %1\";
      return \"fmtvr.64\\t%0, %1, %R1\";
    case 2:
      if (TARGET_BIG_ENDIAN)
	return \"fmfvr.64\\t%R0, %0, %1\";
      return \"fmfvr.64\\t%0, %R0, %1\";
    case 3:
    case 4:
    case 6:
      return fpuv3_output_move(operands);
    case 5:
      return \"fmov.64\\t%0, %1\";
    case 7:
      return \"fmovi.64\\t%0, %1\";
    default:
      return csky_output_movedouble(operands, DFmode);
    }
  "
)

;; -------------------------------------------------------------------------
;; Float Mul instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_mul<mode>3"
  [(set (match_operand:F3ANY	      0 "register_operand" "=v")
	(mult:F3ANY (match_operand:F3ANY  1 "register_operand" " v")
		    (match_operand:F3ANY  2 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fmul.<f3t>\t%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Float Muladd and mulsub instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_mula<mode>3"
  [(set (match_operand:F3ANY	      0 "register_operand" "+v")
	(plus:F3ANY (mult:F3ANY (match_operand:F3ANY  1 "register_operand" " v")
				(match_operand:F3ANY  2 "register_operand" " v"))
		    (match_dup 0)))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fmula.<f3t>\t%0, %1, %2"
)

(define_insn "*fpv3_muls<mode>3"
  [(set (match_operand:F3ANY	      0 "register_operand" "+v")
	(minus:F3ANY (match_dup 0)
		     (mult:F3ANY (match_operand:F3ANY  1 "register_operand" " v")
				 (match_operand:F3ANY  2 "register_operand" " v"))))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fmuls.<f3t>\t%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Float fmula/fmuls/fnmula/fnmuls instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_fmuls_<mode>4"
  [(set (match_operand:F3ANY	      0 "register_operand" "=v")
	(fma:F3ANY (neg:F3ANY (match_operand:F3ANY  1 "register_operand" "v"))
		   (match_operand:F3ANY   2 "register_operand" "v")
		   (match_operand:F3ANY   3 "register_operand"  "0")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "ffmuls.<f3t>\t%0, %1, %2"
)

(define_insn "*fpv3_fmula_<mode>4"
  [(set (match_operand:F3ANY	    0 "register_operand" "=v")
	(fma:F3ANY (match_operand:F3ANY 1 "register_operand" " v")
		   (match_operand:F3ANY 2 "register_operand" " v")
		   (match_operand:F3ANY 3 "register_operand" "0")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "ffmula.<f3t>\t%0, %1, %2"
)

(define_insn "*fpv3_fnmula_<mode>4"
  [(set (match_operand:F3ANY	      0 "register_operand" "=v")
	(neg: F3ANY (fma:F3ANY (match_operand:F3ANY  1 "register_operand" " v")
			       (match_operand:F3ANY  2 "register_operand" " v")
			       (match_operand:F3ANY  3 "register_operand" "0"))))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "ffnmula.<f3t>\t%0, %1, %2"
)

(define_insn "*fpv3_fnmuls_<mode>4"
  [(set (match_operand:F3ANY	      0 "register_operand" "=v")
	(fma:F3ANY (match_operand:F3ANY  1 "register_operand" " v")
		   (match_operand:F3ANY  2 "register_operand" " v")
		   (neg:F3ANY (match_operand:F3ANY  3 "register_operand" "0"))))]
  "CSKY_ISA_FEATURE(fpv3_sf)"
  "ffnmuls.<f3t>\t%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Float div/recipe/sqrt instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_div<mode>3"
  [(set (match_operand:F3ANY	      0 "register_operand" "=v")
	(div:F3ANY (match_operand:F3ANY   1 "register_operand" " v")
		   (match_operand:F3ANY   2 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fdiv.<f3t>\t%0, %1, %2"
)

(define_insn "*fpv3_recip<mode>3"
  [(set (match_operand:F3ANY	     0 "register_operand" "=v")
	(div:F3ANY (match_operand:F3ANY  1 "csky_const_float1_operand" " i")
		   (match_operand:F3ANY  2 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "frecip.<f3t>\t%0, %2"
)

(define_insn "*fpv3_sqrt<mode>2"
  [(set (match_operand:F3ANY	      0 "register_operand" "=v")
	(sqrt:F3ANY (match_operand:F3ANY  1 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fsqrt.<f3t>\t%0, %1"
)

;; -------------------------------------------------------------------------
;; Float fmax/fmin instructions
;; -------------------------------------------------------------------------

(define_insn "fmax<mode>3"
  [(set (match_operand:F3ANY		 0 "register_operand" "=v")
	(unspec:F3ANY [(match_operand:F3ANY  1 "register_operand" " v")
		       (match_operand:F3ANY  2 "register_operand" " v")]
		      UNSPEC_MAXNM_F3))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fmaxnm.<f3t>\t%0, %1, %2"
)

(define_insn "fmin<mode>3"
  [(set (match_operand:F3ANY		 0 "register_operand" "=v")
	(unspec:F3ANY [(match_operand:F3ANY  1 "register_operand" " v")
		       (match_operand:F3ANY  2 "register_operand" " v")]
		      UNSPEC_MINNM_F3))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fminnm.<f3t>\t%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Float compare instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_<zero_inst>_<mode>3"
  [(set (reg:CC CSKY_CC_REGNUM)
	(FCMPZ:CC (match_operand:F3ANY 0 "register_operand" "v")
		  (match_operand:F3ANY 1 "csky_const_float0_operand" "i")))]
   "CSKY_ISA_FEATURE(fpv3_<mode>)"
   "fcmp<zero_inst>.<f3t>\t%0"
)

(define_insn "*fpv3_<reg_inst>_<mode>3"
  [(set (reg:CC CSKY_CC_REGNUM)
	(FCMP:CC (match_operand:F3ANY 0 "register_operand" "v")
		 (match_operand:F3ANY 1 "register_operand" "v")))]
   "CSKY_ISA_FEATURE(fpv3_<mode>)"
   "fcmp<reg_inst>.<f3t>\t%0, %1"
)

(define_insn "*fpv3_gt<mode>3"
  [(set (reg:CC CSKY_CC_REGNUM)
	(gt:CC (match_operand:F3ANY 0 "register_operand" "v")
	       (match_operand:F3ANY 1 "register_operand" "v")))]
   "CSKY_ISA_FEATURE(fpv3_<mode>)"
   "fcmplt.<f3t>\t%1, %0"
)

(define_insn "*fpv3_le<mode>3"
  [(set (reg:CC CSKY_CC_REGNUM)
	(le:CC (match_operand:F3ANY 0 "register_operand" "v")
	       (match_operand:F3ANY 1 "register_operand" "v")))]
   "CSKY_ISA_FEATURE(fpv3_<mode>)"
   "fcmphs.<f3t>\t%1, %0"
)

(define_insn "*fpv3_unordered"
  [(set (reg:CC CSKY_CC_REGNUM)
	(unordered:CC (match_operand:F3ANY 0 "register_operand" "v")
		      (match_operand:F3ANY 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fcmpuo.<f3t>\t%0, %1")

(define_insn "*fpv3_unordered_zero"
  [(set (reg:CC CSKY_CC_REGNUM)
	(unordered:CC (match_operand:F3ANY 0 "register_operand" "v")
		      (match_operand:F3ANY 1 "csky_const_float0_operand" "i")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fcmpuoz.<f3t>\t%0")

;; -------------------------------------------------------------------------
;; Float ADD instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_add<mode>3"
  [(set (match_operand:F3ANY	      0 "register_operand" "=v")
	(plus:F3ANY (match_operand:F3ANY  1 "register_operand" " v")
		    (match_operand:F3ANY  2 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fadd.<f3t>\t%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Float SUB instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_sub<mode>3"
  [(set (match_operand:F3ANY	       0 "register_operand" "=v")
	(minus:F3ANY (match_operand:F3ANY  1 "register_operand" " v")
		     (match_operand:F3ANY  2 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fsub.<f3t>\t%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Float NEG instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_neg<mode>2"
  [(set (match_operand:F3ANY	     0 "register_operand" "=v")
	(neg:F3ANY (match_operand:F3ANY  1 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fneg.<f3t>\t%0, %1"
)

;; -------------------------------------------------------------------------
;; Float ABS instructions
;; -------------------------------------------------------------------------

(define_insn "*fpv3_abs<mode>2"
  [(set (match_operand:F3ANY	     0 "register_operand" "=v")
	(abs:F3ANY (match_operand:F3ANY  1 "register_operand" " v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fabs.<f3t>\t%0, %1"
)

;; -------------------------------------------------------------------------
;; Float common convert instructions
;; -------------------------------------------------------------------------

;; SF <- HF
(define_insn "*fpv3_extendhfsf2"
  [(set (match_operand:SF		  0 "register_operand" "=v")
	(float_extend:SF (match_operand:HF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "fhtos\t%0, %1")

;; HF <- SF
(define_insn "*fpv3_truncsfhf2"
  [(set (match_operand:HF		     0 "register_operand" "=v")
	(float_truncate:HF (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_hf)"
  "fstoh\t%0, %1")

;; DF <- SF
(define_insn "*fpv3_extendsfdf2"
  [(set (match_operand:DF		  0 "register_operand" "=v")
	(float_extend:DF (match_operand:SF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_df)"
  "fstod\t%0, %1")

;; SF <- DF
(define_insn "*fpv3_truncdfsf2"
  [(set (match_operand:SF		    0 "register_operand" "=v")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_df)"
  "fdtos\t%0, %1")

;; DF,SF,HF <- unsigned SI,SI
(define_insn "*fpv3_float<floatsuop>si<mode>2"
  [(set (match_operand:F3ANY	   0 "register_operand" "=v")
	(FLOAT_SU:F3ANY (match_operand:SI 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fitof.<floatsu>32.f<f3t>\t%0, %1")

;; HF <- unsigned HI,HI
(define_insn "*fpv3_float<floatsuop>hihf2"
  [(set (match_operand:HF	   0 "register_operand" "=v")
	(FLOAT_SU:HF (match_operand:HI 1 "register_operand" "v")))]
  "CSKY_ISA_FEATURE(fpv3_hi) && CSKY_ISA_FEATURE(fpv3_hf)"
  "fitof.<floatsu>16.f16\t%0, %1")

;; unsigned SI,SI <- DF,SF,HF
(define_insn "*fpv3_fix<fixsuop>_trunc<mode>si2"
  [(set (match_operand:SI	    0 "register_operand" "=v")
	(FIX_SU:SI (fix:F3ANY (match_operand:F3ANY 1 "register_operand" "v"))))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)"
  "fftoi.f<f3t>.<fixsu>32.rz\t%0, %1")

;; -------------------------------------------------------------------------
;; Float complex convert instructions
;; -------------------------------------------------------------------------

;; Fixed point to floating point conversions.

;(define_insn "*combine_fcvt_fixed16_<mode>"
;  [(set (match_operand:F3ANY 0 "register_operand" "=v")
;	(mult:F3ANY (float:F3ANY (match_operand:HI 1 "register_operand" "0"))
;	       (match_operand 2
;			"const_double_fcvt_power_of_two_reciprocal_hq" "Dt")))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math
;   && CSKY_ISA_FEATURE(fpv3_hi)"
;  "fxtof.s16.f<f3t>\t%0, %1, %v2")
;
;(define_insn "*combine_fcvt_fixed32_<mode>"
;  [(set (match_operand:F3ANY 0 "register_operand" "=v")
;	(mult:F3ANY (float:F3ANY (match_operand:SI 1 "register_operand" "0"))
;	       (match_operand 2
;			"const_double_fcvt_power_of_two_reciprocal_sq" "Dt")))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math"
;  "fxtof.s32.f<f3t>\t%0, %1, %v2")
;
;(define_insn "*combine_fcvt_unfixed16_<mode>"
;  [(set (match_operand:F3ANY 0 "register_operand" "=v")
;	(mult:F3ANY (unsigned_float:F3ANY (match_operand:HI 1 "register_operand" "0"))
;	       (match_operand 2
;			"const_double_fcvt_power_of_two_reciprocal_hq" "Dt")))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math
;   && CSKY_ISA_FEATURE(fpv3_hi)"
;  "fxtof.u16.f<f3t>\t%0, %1, %v2")
;
;(define_insn "*combine_fcvt_unfixed32_<mode>"
;  [(set (match_operand:F3ANY 0 "register_operand" "=v")
;	(mult:F3ANY (unsigned_float:F3ANY (match_operand:SI 1 "register_operand" "0"))
;	       (match_operand 2
;			"const_double_fcvt_power_of_two_reciprocal_sq" "Dt")))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math"
;  "fxtof.u32.f<f3t>\t%0, %1, %v2")

;; Floating point to fixed point conversions.

;(define_insn "*combine_fcvt<mode>_fixed16"
;  [(set (match_operand:HI 0 "register_operand" "=v")
;	(fix:HI (fix:F3ANY (mult:F3ANY (match_operand:F3ANY 1 "register_operand" "0")
;			    (match_operand 2
;			     "const_double_fcvt_power_of_two_hq" "Du")))))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math
;   && CSKY_ISA_FEATURE(fpv3_hi)"
;  "fftox.f<f3t>.s16\t%0, %1, %v2"
; )
;
;(define_insn "*combine_fcvt<mode>_fixed32"
;  [(set (match_operand:SI 0 "register_operand" "=v")
;	(fix:SI (fix:F3ANY (mult:F3ANY (match_operand:F3ANY 1 "register_operand" "0")
;			    (match_operand 2
;			     "const_double_fcvt_power_of_two_sq" "Du")))))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math"
;  "fftox.f<f3t>.s32\t%0, %1, %v2"
; )
;
;(define_insn "*combine_fcvt<mode>_unfixed16"
;  [(set (match_operand:HI 0 "register_operand" "=v")
;	(unsigned_fix:HI (fix:F3ANY (mult:F3ANY (match_operand:F3ANY 1 "register_operand" "0")
;				     (match_operand 2
;				      "const_double_fcvt_power_of_two_hq" "Du")))))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math
;   && CSKY_ISA_FEATURE(fpv3_hi)"
;  "fftox.f<f3t>.u16\t%0, %1, %v2"
; )
;
;(define_insn "*combine_fcvt<mode>_unfixed32"
;  [(set (match_operand:SI 0 "register_operand" "=v")
;	(unsigned_fix:SI (fix:F3ANY (mult:F3ANY (match_operand:F3ANY 1 "register_operand" "0")
;				     (match_operand 2
;				      "const_double_fcvt_power_of_two_sq" "Du")))))]
;  "CSKY_ISA_FEATURE(fpv3_<mode>) && !flag_rounding_math"
;  "fftox.f<f3t>.u32\t%0, %1, %v2"
; )

;; conversions need to be rounding to nearest.

(define_insn "l<frm_pattern><fixsuop><mode>si2"
  [(set (match_operand:SI 0 "register_operand" "=v")
	(FIX_SU:SI (unspec:F3ANY [(match_operand:F3ANY 1 "register_operand" "0")]
				   FRM)))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)
   && (flag_fp_int_builtin_inexact || !flag_trapping_math)"
  "fftoi.f<f3t>.<fixsu>32<rm>\t%0, %1"
)

(define_insn "<frm_pattern><mode>2"
  [(set (match_operand:F3ANY 0 "register_operand" "=v")
	(unspec:F3ANY [(match_operand:F3ANY 1 "register_operand" "0")] FRMF))]
  "CSKY_ISA_FEATURE(fpv3_<mode>)
   && (flag_fp_int_builtin_inexact || !flag_trapping_math)"
  "fftofi.f<f3t><rm>\t%0, %1"
)

;; Write Floating-point Control Register.
(define_insn "csky_setfcrsi"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")] VUNSPEC_SET_FCR)]
  "CSKY_ISA_FEATURE(fcr)"
  "mtcr\t%0, fcr"
)

;; Read Floating-point Control Register.
(define_insn "csky_getfcrsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] VUNSPEC_GET_FCR))]
  "CSKY_ISA_FEATURE(fcr)"
  "mfcr\t%0, fcr"
)

;; Insert Floating-point Control Register.
(define_insn "csky_insfcrsi"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")
		     (match_operand:SI 1 "const_int_operand" "i")
		     (match_operand:SI 2 "const_int_operand" "i")]VUNSPEC_INS_FCR)
   (clobber (reg: SI 13))]
  "CSKY_ISA_FEATURE(fcr)"
  {
    operands[1] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[1]) - 1);
    return "mfcr\tt1, fcr\n\tins\tt1, %0, %1, %2\n\tmtcr\tt1, fcr";
  }
)
