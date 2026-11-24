;; Machine description for IBM RISC System 6000 (POWER) for GNU C compiler
;; Copyright (C) 1990-2025 Free Software Foundation, Inc.
;; Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

;; Support for _Float16 (HFmode) and __bf16 (BFmode)

;; Mode iterator for 16-bit floating point modes both as a scalar and
;; as a vector.
(define_mode_iterator FP16	[BF HF])
(define_mode_iterator VS_FP16	[BF HF V8BF V8HF])
(define_mode_iterator VFP16	[V8BF V8HF])

;; Mode iterator for 16-bit floating point modes on machines with
;; hardware support both as a scalar and as a vector.
(define_mode_iterator FP16_HW [(BF "TARGET_BFLOAT16_HW")
			       (HF "TARGET_FLOAT16_HW")])

(define_mode_iterator VFP16_HW [(V8BF "TARGET_BFLOAT16_HW")
				(V8HF "TARGET_FLOAT16_HW")])

;; Mode iterator for floating point modes other than SF/DFmode that we
;; convert to/from _Float16 (HFmode) via DFmode.
(define_mode_iterator FP16_FLOAT_CONVERT [TF KF IF SD DD TD])

;; Mode attribute giving the instruction to convert the even
;; V8HFmode or V8BFmode elements to V4SFmode
(define_mode_attr cvt_fp16_to_v4sf [(BF   "xvcvbf16spn")
				    (HF   "xvcvhpsp")
				    (V8BF "xvcvbf16spn")
				    (V8HF "xvcvhpsp")])

;; Mode attribute giving the instruction to convert the V4SFmode
;; elements to the even V8HFmode or V8BFmode elements
(define_mode_attr cvt_v4sf_to_fp16 [(BF   "xvcvspbf16")
				    (HF   "xvcvsphp")
				    (V8BF "xvcvspbf16")
				    (V8HF "xvcvsphp")])

;; Mode attribute giving the vector mode for a 16-bit floating point
;; scalar in both upper and lower case.
(define_mode_attr FP16_VECTOR8 [(BF "V8BF")
				(HF "V8HF")])

(define_mode_attr fp16_vector8 [(BF "v8bf")
				(HF "v8hf")])

;; Mode attribute giving the vector mode with 4 16-bit floating point
;; elements given a scalar or 8 element vector.
(define_mode_attr FP16_VECTOR4 [(BF   "V4BF")
				(HF   "V4HF")
				(V8BF "V4BF")
				(V8HF "V4HF")])

;; Binary operators for bfloat16/float16 vectorization.
(define_code_iterator FP16_BINARY_OP [plus minus mult smax smin])

;; Standard names for the unary/binary/ternary operators
(define_code_attr fp16_names [(abs   "abs")
			      (fma   "fma")
			      (plus  "add")
			      (minus "sub")
			      (mult  "mul")
			      (neg   "neg")
			      (smax  "smax")
			      (smin  "smin")])

;; UNSPEC constants
(define_c_enum "unspec"
  [UNSPEC_BF_SHIFT_LEFT_16BIT
   UNSPEC_XXSPLTW_FP16
   UNSPEC_XVCVSPBF16_BF
   UNSPEC_XVCVSPHP_V8HF
   UNSPEC_XVCVSPBF16_V8BF
   UNSPEC_CVT_FP16_TO_V4SF
   UNSPEC_CVT_V4SF_TO_FP16])

;; _Float16 and __bf16 moves
(define_expand "mov<mode>"
  [(set (match_operand:FP16 0 "nonimmediate_operand")
	(match_operand:FP16 1 "any_operand"))]
  "TARGET_FLOAT16"
{
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

;; On power10, we can load up HFmode and BFmode constants with xxspltiw
;; or pli.
(define_insn "*mov<mode>_xxspltiw"
  [(set (match_operand:FP16 0 "gpc_reg_operand"        "=wa,wa,?r,?r")
	(match_operand:FP16 1 "fp16_xxspltiw_constant"   "j,eP,j, eP"))]
  "TARGET_FLOAT16
   && (TARGET_PREFIXED || operands[1] == CONST0_RTX (<MODE>mode))"
{
  rtx op1 = operands[1];
  const REAL_VALUE_TYPE *rtype = CONST_DOUBLE_REAL_VALUE (op1);
  long real_words[1];

  if (op1 == CONST0_RTX (<MODE>mode))
    return (!vsx_register_operand (operands[0], <MODE>mode)
	    ? "li %0,0"
	    : "xxlxor %x0,%x0,%x0");

  real_to_target (real_words, rtype, <MODE>mode);
  operands[2] = GEN_INT (real_words[0]);
  return (vsx_register_operand (operands[0], <MODE>mode)
	  ? "xxspltiw %x0,%2"
	  : "li %0,%2");
}
  [(set_attr "type"     "veclogical, vecsimple, *,  *")
   (set_attr "prefixed" "no,         yes,       no, yes")])

;; Handle creating -0.0 if we don't have XXSPLTIW.  For the scalar
;; modes, we can't do the gen_lowpart call until after register
;; allocation.
(define_split
  [(set (match_operand:VS_FP16 0 "altivec_register_operand")
	(match_operand:VS_FP16 1 "minus_zero_constant"))]
  "TARGET_FLOAT16 && reload_completed"
  [(const_int 0)]
{
  int dest_r = reg_or_subregno (operands[0]);
  rtx dest = gen_rtx_REG (V8HImode, dest_r);
  size_t nunits = GET_MODE_NUNITS (V8HFmode);

  rtvec v = rtvec_alloc (nunits);
  for (size_t i = 0; i < nunits; i++)
    RTVEC_ELT (v, i) = constm1_rtx;

  rs6000_expand_vector_init (dest, gen_rtx_PARALLEL (V8HImode, v));
  emit_insn (gen_rtx_SET (dest, gen_rtx_ASHIFT (V8HImode, dest, dest)));
  DONE;
})


(define_insn "*mov<mode>_internal"
  [(set (match_operand:FP16 0 "nonimmediate_operand"
                      "=wa,       wa,       Z,         r,          r,
                        m,        r,        wa,        wa,         r,
                        v")

	(match_operand:FP16 1 "any_operand"
                      "wa,        Z,        wa,        r,          m,
                       r,         wa,       r,         j,          j,
                       eZ"))]
  "TARGET_FLOAT16
   && (gpc_reg_operand (operands[0], <MODE>mode)
       || gpc_reg_operand (operands[1], <MODE>mode))"
  "@
   xxlor %x0,%x1,%x1
   lxsihzx %x0,%y1
   stxsihx %x1,%y0
   mr %0,%1
   lhz%U1%X1 %0,%1
   sth%U0%X0 %1,%0
   mfvsrwz %0,%x1
   mtvsrwz %x0,%1
   xxlxor %x0,%x0,%x0
   li %0,0
   #"
  [(set_attr "type"   "vecsimple, fpload,    fpstore,   *,          load,
                       store,     mtvsr,     mfvsr,     veclogical, *,
                       vecperm")
   (set_attr "isa"    "*,         p9v,       p9v,       *,          *,
                       *,         p8v,       p8v,       p9v,        *,
                       *")
   (set_attr "length" "*,         *,         *,         *,          *,
                       *,         *,         *,         *,          *,
                       8")])

;; Vector duplicate
(define_insn "*vecdup<mode>_reg"
  [(set (match_operand:<FP16_VECTOR8> 0 "altivec_register_operand" "=v")
	(vec_duplicate:<FP16_VECTOR8>
	 (match_operand:FP16 1 "altivec_register_operand" "v")))]
  "TARGET_FLOAT16"
  "vsplth %0,%1,3"
  [(set_attr "type" "vecperm")])

(define_insn "*vecdup<mode>_const"
  [(set (match_operand:<FP16_VECTOR8> 0 "vsx_register_operand" "=wa,wa")
	(vec_duplicate:<FP16_VECTOR8>
	 (match_operand:FP16 1 "fp16_xxspltiw_constant" "j,eP")))]
  "TARGET_FLOAT16
   && (TARGET_PREFIXED || operands[1] == CONST0_RTX (<MODE>mode))"
{
  rtx op1 = operands[1];
  if (op1 == CONST0_RTX (<MODE>mode))
    return "xxlxor %x0,%x0,%x0";

  const REAL_VALUE_TYPE *rtype = CONST_DOUBLE_REAL_VALUE (op1);
  long real_words[1];

  real_to_target (real_words, rtype, <MODE>mode);
  operands[2] = GEN_INT (real_words[0]);
  return "xxspltiw %x0,2";
}
  [(set_attr "type" "veclogical,vecperm")
   (set_attr "prefixed" "*,yes")])

;; Convert IEEE 16-bit floating point to/from other floating point modes.

(define_insn "extendhf<mode>2"
  [(set (match_operand:SFDF 0 "vsx_register_operand" "=wa")
	(float_extend:SFDF
	 (match_operand:HF 1 "vsx_register_operand" "wa")))]
  "TARGET_FLOAT16_HW"
  "xscvhpdp %x0,%x1"
  [(set_attr "type" "fpsimple")])

(define_insn "trunc<mode>hf2"
  [(set (match_operand:HF 0 "vsx_register_operand" "=wa")
	(float_truncate:HF
	 (match_operand:SFDF 1 "vsx_register_operand" "wa")))]
  "TARGET_FLOAT16_HW"
  "xscvdphp %x0,%x1"
  [(set_attr "type" "fpsimple")])

;; Convert BFmode to SFmode/DFmode.
;; We could use the following sequence on power10, but for normal conversions
;; we will continue to use the shift left sequence on power9.  However, if we
;; are optiming
;;	__bf16 a, b, c; a = b + c
;;
;; to use xvaddsp, we will generate the sequence:
;;	XXSPLTW		-- duplicate BFmode into all even elements
;;	XVCVBF16SPN	-- convert even BFmode elements to SFmode
;;	XSCVSPNDP	-- convert memory format of SFmode to DFmode.
;;
;; 3 instructions are generated on power8/power9
;;	SLDI		-- shift left 48 bits
;;	MTVSRD		-- move to VSX register
;;	XSCVSPNDP	-- convert memory format of SFmode to DFmode.
;;
;; We have 2 variants for testing.  One that uses the xvcvbf16spn
;; instruction on power10, and one that does not.

(define_insn_and_split "extendbf<mode>2"
  [(set (match_operand:SFDF 0 "vsx_register_operand" "=wa")
	(float_extend:SFDF
	 (match_operand:BF 1 "int_reg_operand" "r")))
   (clobber (match_scratch:V4SF 2 "=wa"))
   (clobber (match_scratch:DI 3 "=r"))]
  "TARGET_FLOAT16"
  "#"
  "&& reload_completed"
  [(pc)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx op3 = operands[3];
  rtx op1_di = gen_rtx_REG (DImode, reg_or_subregno (op1));
  rtx op2_di = gen_rtx_REG (DImode, reg_or_subregno (op2));

  /* The shift left of 48 combines a shift left of 16 bits to
     convert a BFmode to a SFmode with shift left of 32 bits
     to do the xscvspdpn instruction.  */
  emit_insn (gen_ashldi3 (op3, op1_di, GEN_INT (48)));
  emit_move_insn (op2_di, op3);

  /* XSCVSPNDP -- convert single V4SFmode element to DFmode.  */
  emit_insn (GET_MODE (op0) == SFmode
	     ? gen_xscvspdpn_sf (op0, op2)
	     : gen_vsx_xscvspdpn (op0, op2));

  DONE;
}
  [(set_attr "type" "fpsimple")
   (set_attr "length" "12")])

;; Convert a SFmode scalar represented as DFmode to elements 0 and 1 of
;; V4SFmode.
(define_insn "xscvdpspn_sf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:SF 1 "vsx_register_operand" "wa")]
		     UNSPEC_VSX_CVSPDP))]
  "VECTOR_UNIT_VSX_P (SFmode)"
  "xscvdpspn %x0,%x1"
  [(set_attr "type" "fp")])

;; Convert element 0 of a V4SFmode to scalar SFmode (which on the
;; PowerPC uses the DFmode encoding).
(define_insn "xscvspdpn_sf"
  [(set (match_operand:SF 0 "vsx_register_operand" "=wa")
	(unspec:SF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		   UNSPEC_VSX_CVSPDPN))]
  "TARGET_XSCVSPDPN"
  "xscvspdpn %x0,%x1"
  [(set_attr "type" "fp")])

;; Optimize storing the conversion of BFmode to SFmode by shifting the
;; BFmode left 16 bits.
(define_insn_and_split "*extendbfsf2_memory"
  [(set (match_operand:SF 0 "memory_operand" "=m")
	(float_extend:SF
	 (match_operand:BF 1 "int_reg_operand" "r")))
   (clobber (match_scratch:SF 2 "=r"))]
  "TARGET_FLOAT16"
  "#"
  "&& 1"
  [(set (match_dup 2)
	(unspec:SF [(match_dup 1)] UNSPEC_BF_SHIFT_LEFT_16BIT))
   (set (match_dup 0)
	(match_dup 2))]
{
  if (GET_CODE (operands[2]) == SCRATCH)
    operands[2] = gen_reg_rtx (SFmode);
}
  [(set_attr "length" "8")
   (set_attr "type" "store")])

;; Shfit a BFmode left 16 bits getting a SFmode memory value in the GPR
(define_insn "*shift_bf_16bits"
  [(set (match_operand:SF 0 "int_reg_operand" "=r")
	(unspec:SF
	 [(match_operand:BF 1 "int_reg_operand" "r")]
	 UNSPEC_BF_SHIFT_LEFT_16BIT))]
  "TARGET_FLOAT16"
  "slwi %0,%1,16"
  [(set_attr "type" "shift")])

;; Convert SFmode/DFmode to BFmode.
;; 2 instructions are generated:
;;	XSCVDPSPN	-- convert SFmode/DFmode scalar to V4SFmode
;;	XVCVSPBF16	-- convert V4SFmode to even V8BFmode

(define_insn_and_split "trunc<mode>bf2"
  [(set (match_operand:BF 0 "vsx_register_operand" "=wa")
	(float_truncate:BF
	 (match_operand:SFDF 1 "vsx_register_operand" "wa")))
   (clobber (match_scratch:V4SF 2 "=wa"))]
  "TARGET_BFLOAT16_HW"
  "#"
  "&& 1"
  [(pc)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];

  if (GET_CODE (op2) == SCRATCH)
    op2 = gen_reg_rtx (V4SFmode);

  emit_insn (GET_MODE (op1) == SFmode
	     ? gen_xscvdpspn_sf (op2, op1)
	     : gen_vsx_xscvdpspn (op2, op1));

  emit_insn (gen_xvcvspbf16_bf (op0, op2));
  DONE;
}
  [(set_attr "type" "fpsimple")])

(define_insn "vsx_xscvdpspn_sf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:SF 1 "vsx_register_operand" "wa")]
		     UNSPEC_VSX_CVDPSPN))]
  "TARGET_XSCVDPSPN"
  "xscvdpspn %x0,%x1"
  [(set_attr "type" "fp")])


;; Convert 128-bit integers to 16-bit floating point if IEEE 128-bit
;; floating point is available by first using a conversion to IEEE
;; 128-bit, then converting that to double, and finally converting it
;; to 16-bit floating point.

(define_expand "floatti<mode>2"
  [(use (match_operand:FP16 0 "vsx_register_operand"))
   (use (match_operand:TI 1 "altivec_register_operand"))]
  "TARGET_POWER10 && TARGET_FLOAT128_HW && can_create_pseudo_p ()"
{
  rtx tmp_df = gen_reg_rtx (DFmode);

  if (FLOAT128_IEEE_P (TFmode))
    {
      rtx tmp_tf = gen_reg_rtx (TFmode);
      emit_insn (gen_floattitf2 (tmp_tf, operands[1]));
      emit_insn (gen_trunctfdf2 (tmp_df, tmp_tf));
    }

  else if (FLOAT128_IEEE_P (KFmode))
    {
      rtx tmp_kf = gen_reg_rtx (KFmode);
      emit_insn (gen_floattikf2 (tmp_kf, operands[1]));
      emit_insn (gen_trunckfdf2 (tmp_df, tmp_kf));
    }

  else
    gcc_unreachable ();

  emit_insn (gen_truncdf<mode>2 (operands[0], tmp_df));
  DONE;
})

(define_expand "floatunsti<mode>2"
  [(use (match_operand:FP16 0 "vsx_register_operand"))
   (use (match_operand:TI 1 "altivec_register_operand"))]
  "TARGET_POWER10 && TARGET_FLOAT128_HW && can_create_pseudo_p ()"
{
  rtx tmp_df = gen_reg_rtx (DFmode);

  if (FLOAT128_IEEE_P (TFmode))
    {
      rtx tmp_tf = gen_reg_rtx (TFmode);
      emit_insn (gen_floatunstitf2 (tmp_tf, operands[1]));
      emit_insn (gen_trunctfdf2 (tmp_df, tmp_tf));
    }

  else if (FLOAT128_IEEE_P (KFmode))
    {
      rtx tmp_kf = gen_reg_rtx (KFmode);
      emit_insn (gen_floatunstikf2 (tmp_kf, operands[1]));
      emit_insn (gen_trunckfdf2 (tmp_df, tmp_kf));
    }

  else
    gcc_unreachable ();

  emit_insn (gen_truncdf<mode>2 (operands[0], tmp_df));
  DONE;
})

;; Convert 16-bit floating point to 128-bit integers if IEEE 128-bit
;; floating point is available by first using a conversion to double,
;; then to IEEE 128-bit, and finally to 128-bit integers.

(define_expand "fix_trunc<mode>ti2"
  [(use (match_operand:TI 0 "altivec_register_operand"))
   (use (match_operand:FP16 1 "vsx_register_operand"))]
  "TARGET_POWER10 && TARGET_FLOAT128_HW && can_create_pseudo_p ()"
{
  rtx tmp_df = gen_reg_rtx (DFmode);

  emit_insn (gen_extend<mode>df2 (tmp_df, operands[1]));
  if (FLOAT128_IEEE_P (TFmode))
    {
      rtx tmp_tf = gen_reg_rtx (TFmode);
      emit_insn (gen_extenddftf2 (tmp_tf, tmp_df));
      emit_insn (gen_fix_trunctfti2 (operands[0], tmp_tf));
    }

  else if (FLOAT128_IEEE_P (KFmode))
    {
      rtx tmp_kf = gen_reg_rtx (KFmode);
      emit_insn (gen_extenddfkf2 (tmp_kf, tmp_df));
      emit_insn (gen_fix_trunckfti2 (operands[0], tmp_kf));
    }

  else
    gcc_unreachable ();

  DONE;
})

(define_expand "fixuns_trunc<mode>ti2"
  [(use (match_operand:TI 0 "altivec_register_operand"))
   (use (match_operand:FP16 1 "vsx_register_operand"))]
  "TARGET_POWER10 && TARGET_FLOAT128_HW && can_create_pseudo_p ()"
{
  rtx tmp_df = gen_reg_rtx (DFmode);

  emit_insn (gen_extend<mode>df2 (tmp_df, operands[1]));
  if (FLOAT128_IEEE_P (TFmode))
    {
      rtx tmp_tf = gen_reg_rtx (TFmode);
      emit_insn (gen_extenddftf2 (tmp_tf, tmp_df));
      emit_insn (gen_fixuns_trunctfti2 (operands[0], tmp_tf));
    }

  else if (FLOAT128_IEEE_P (KFmode))
    {
      rtx tmp_kf = gen_reg_rtx (KFmode);
      emit_insn (gen_extenddfkf2 (tmp_kf, tmp_df));
      emit_insn (gen_fixuns_trunckfti2 (operands[0], tmp_kf));
    }

  else
    gcc_unreachable ();

  DONE;
})

;; Convert the even elements of a vector 16-bit floating point to
;; V4SFmode.  Deal with little endian vs. big endian element ordering
;; in identifying which elements are converted.

(define_expand "cvt_fp16_to_v4sf_<mode>"
  [(set (match_operand:V4SF 0 "vsx_register_operand")
	(float_extend:V4SF
	 (vec_select:<FP16_VECTOR4>
	  (match_operand:VFP16_HW 1 "vsx_register_operand")
	  (parallel [(match_dup 2)
		     (match_dup 3)
		     (match_dup 4)
		     (match_dup 5)]))))]
  ""
{
  int endian_adjust = WORDS_BIG_ENDIAN ? 0 : 1;
  operands[2] = GEN_INT (0 + endian_adjust);
  operands[3] = GEN_INT (2 + endian_adjust);
  operands[4] = GEN_INT (4 + endian_adjust);
  operands[5] = GEN_INT (6 + endian_adjust);
})

(define_insn "*cvt_fp16_to_v4sf_<mode>_le"
  [(set (match_operand:V4SF 0 "vsx_register_operand")
	(float_extend:V4SF
	 (vec_select:<FP16_VECTOR4>
	  (match_operand:VFP16_HW 1 "vsx_register_operand")
	  (parallel [(const_int 1)
		     (const_int 3)
		     (const_int 5)
		     (const_int 7)]))))]
  "!WORDS_BIG_ENDIAN"
  "<cvt_fp16_to_v4sf> %x0,%x1"
  [(set_attr "type" "vecfloat")])

(define_insn "*cvt_fp16_to_v4sf_<mode>_be"
  [(set (match_operand:V4SF 0 "vsx_register_operand")
	(float_extend:V4SF
	 (vec_select:<FP16_VECTOR4>
	  (match_operand:VFP16_HW 1 "vsx_register_operand")
	  (parallel [(const_int 0)
		     (const_int 2)
		     (const_int 4)
		     (const_int 6)]))))]
  "WORDS_BIG_ENDIAN"
  "<cvt_fp16_to_v4sf> %x0,%x1"
  [(set_attr "type" "vecfloat")])

;; Convert a V4SFmode vector element to a 16-bit floating point value.
;; We only care about the 2nd element.  This is used to convert BFmode
;; to HFmode or HFmode to BFmode.

(define_insn "cvt_v4sf_to_fp16_<mode>"
  [(set (match_operand:FP16_HW 0 "vsx_register_operand")
	(unspec:FP16_HW [(match_operand:V4SF 1 "vsx_register_operand")]
			UNSPEC_CVT_V4SF_TO_FP16))]
  ""
  "<cvt_v4sf_to_fp16> %x0,%x1"
  [(set_attr "type" "vecfloat")])

;; Duplicate and convert a 16-bit floating point scalar to V4SFmode.

(define_insn_and_split "*dup_<mode>_to_v4sf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(vec_duplicate:V4SF
	 (float_extend:SF
	  (match_operand:FP16_HW 1 "vsx_register_operand" "wa"))))]
  ""
  "#"
  "&& 1"
  [(pc)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op0_vfp16 = gen_lowpart (<FP16_VECTOR8>mode, op0);

  emit_insn (gen_xxspltw_<mode> (op0, op1));
  emit_insn (gen_cvt_fp16_to_v4sf_<fp16_vector8> (op0, op0_vfp16));
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "vecperm")])

;; Duplicate a HF/BF value so it can be used for xvcvhpspn/xvcvbf16spn.
;; Because xvcvhpspn/xvcvbf16spn only uses the even elements, we can
;; use xxspltw instead of vspltw.  This has the advantage that the
;; register allocator can use any of the 64 VSX registers instead of
;; being limited to the 32 Altivec registers that VSPLTH would require.

(define_insn "xxspltw_<mode>"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:FP16_HW 1 "vsx_register_operand" "wa")]
		     UNSPEC_XXSPLTW_FP16))]
  ""
  "xxspltw %x0,%x1,1"
  [(set_attr "type" "vecperm")])

;; Convert a V4SFmode vector to a 16-bit floating point scalar.  We
;; only care about the 2nd V4SFmode element, which is the element we
;; converted the 16-bit scalar (4th element) to V4SFmode to do the
;; operation, and converted it back.

(define_insn "xvcvspbf16_bf"
  [(set (match_operand:BF 0 "vsx_register_operand" "=wa")
	(unspec:BF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		   UNSPEC_XVCVSPBF16_BF))]
  "TARGET_BFLOAT16_HW"
  "xvcvspbf16 %x0,%x1"
  [(set_attr "type" "vecfloat")])


;; Convert HFmode to BFmode
;; The instructions generated are:
;;	XXSPLTIW  tmp1,r1
;;	XVCVHPSP  tmp1,tmp1
;;	XVCVSPHPN r0,tmp1

(define_insn_and_split "trunchfbf2"
  [(set (match_operand:BF 0 "vsx_register_operand" "=wa")
	(float_truncate:BF
	 (match_operand:HF 1 "vsx_register_operand" "wa")))
   (clobber (match_scratch:V4SF 2 "=wa"))]
  "TARGET_BFLOAT16_HW"
  "#"
  "&& 1"
  [(pc)]
{
  rtx tmp_v4sf = operands[2];

  if (GET_CODE (tmp_v4sf) == SCRATCH)
    tmp_v4sf = gen_reg_rtx (V4SFmode);

  rtx tmp_v8hf = gen_lowpart (V8HFmode, tmp_v4sf);

  emit_insn (gen_xxspltw_hf (tmp_v4sf, operands[1]));
  emit_insn (gen_cvt_fp16_to_v4sf_v8hf (tmp_v4sf, tmp_v8hf));
  emit_insn (gen_cvt_v4sf_to_fp16_bf (operands[0], tmp_v4sf));
  DONE;
}
  [(set_attr "length" "12")
   (set_attr "type" "vecfloat")])

;; Optimize converting HFmode to BFmode if the BFmode is coming from
;; memory.  We can eliminate the XXSPLTW instruction since the load
;; zeros out the other elements.

(define_insn_and_split "*trunchfbf2_memory"
  [(set (match_operand:BF 0 "vsx_register_operand" "=wa")
	(float_truncate:BF
	 (match_operand:HF 1 "memory_operand" "Z")))
   (clobber (match_scratch:V4SF 2 "=wa"))]
  "TARGET_BFLOAT16_HW"
  "#"
  "&& reload_completed"
  [(pc)]
{
  rtx tmp_v4sf = operands[2];
  unsigned int r = reg_or_subregno (tmp_v4sf);
  rtx tmp_v8hf = gen_rtx_REG (V8HFmode, r);
  rtx tmp_hf = gen_rtx_REG (HFmode, r);

  emit_move_insn (tmp_hf, operands[1]);
  emit_insn (gen_cvt_fp16_to_v4sf_v8hf (tmp_v4sf, tmp_v8hf));
  emit_insn (gen_cvt_v4sf_to_fp16_bf (operands[0], tmp_v4sf));
  DONE;
}
  [(set_attr "length" "12")
   (set_attr "type" "vecfloat")])

;; Convert BFmode to HFmode
;; The instructions generated are:
;;	XXSPLTIW  tmp1,r1
;;	XVCVBF16SP  tmp1,tmp1
;;	XVCVSPBF16N r0,tmp1

(define_insn_and_split "extendbfhf2"
  [(set (match_operand:HF 0 "vsx_register_operand" "=wa")
	(float_extend:HF
	 (match_operand:BF 1 "vsx_register_operand" "wa")))
   (clobber (match_scratch:V4SF 2 "=wa"))]
  "TARGET_BFLOAT16_HW"
  "#"
  "&& 1"
  [(pc)]
{
  rtx tmp_v4sf = operands[2];

  if (GET_CODE (tmp_v4sf) == SCRATCH)
    tmp_v4sf = gen_reg_rtx (V4SFmode);

  rtx tmp_v8bf = gen_lowpart (V8BFmode, tmp_v4sf);

  emit_insn (gen_xxspltw_bf (tmp_v4sf, operands[1]));
  emit_insn (gen_cvt_fp16_to_v4sf_v8bf (tmp_v4sf, tmp_v8bf));
  emit_insn (gen_cvt_v4sf_to_fp16_hf (operands[0], tmp_v4sf));
  DONE;
}
  [(set_attr "length" "12")
   (set_attr "type" "vecfloat")])

;; Optimize converting BFmode to HFmode if the HFmode is coming from
;; memory.  We can eliminate the XXSPLTW instruction since the load
;; zeros out the other elements.

(define_insn_and_split "*extendbfhf2_memory"
  [(set (match_operand:HF 0 "vsx_register_operand" "=wa")
	(float_extend:HF
	 (match_operand:BF 1 "memory_operand" "Z")))
   (clobber (match_scratch:V4SF 2 "=wa"))]
  "TARGET_BFLOAT16_HW"
  "#"
  "&& reload_completed"
  [(pc)]
{
  rtx tmp_v4sf = operands[2];
  unsigned int r = reg_or_subregno (tmp_v4sf);
  rtx tmp_v8bf = gen_rtx_REG (V8BFmode, r);
  rtx tmp_bf = gen_rtx_REG (BFmode, r);

  emit_move_insn (tmp_bf, operands[1]);
  emit_insn (gen_cvt_fp16_to_v4sf_v8bf (tmp_v4sf, tmp_v8bf));
  emit_insn (gen_cvt_v4sf_to_fp16_hf (operands[0], tmp_v4sf));
  DONE;
}
  [(set_attr "length" "12")
   (set_attr "type" "vecfloat")])

;; Convert between HFmode/BFmode and 128-bit binary floating point and
;; decimal floating point types.  We use convert_move since some of the
;; types might not have valid RTX expanders.  We use DFmode as the
;; intermediate conversion destination.

(define_expand "extend<FP16_HW:mode><FP16_FLOAT_CONVERT:mode>2"
  [(use (match_operand:FP16_FLOAT_CONVERT 0 "vsx_register_operand"))
   (use (match_operand:FP16_HW 1 "vsx_register_operand"))]
  "can_create_pseudo_p ()"
{
  rtx df_tmp = gen_reg_rtx (DFmode);

  convert_move (df_tmp, operands[1], 0);
  convert_move (operands[0], df_tmp, 0);
  DONE;
})

(define_expand "trunc<FP16_FLOAT_CONVERT:mode><FP16_HW:mode>2"
  [(use (match_operand:FP16_HW 0 "vsx_register_operand"))
   (use (match_operand:FP16_FLOAT_CONVERT 1 "vsx_register_operand"))]
  "can_create_pseudo_p ()"
{
  rtx df_tmp = gen_reg_rtx (DFmode);

  convert_move (df_tmp, operands[1], 0);
  convert_move (operands[0], df_tmp, 0);
  DONE;
})

;; Convert 16-bit floating point to integers
(define_expand "fix_trunc<FP16:mode><GPR:mode>2"
  [(use (match_operand:GPR 0 "vsx_register_operand"))
   (use (match_operand:FP16 1 "vsx_register_operand"))]
  "can_create_pseudo_p ()"
{
  rtx df_tmp = gen_reg_rtx (DFmode);

  convert_move (df_tmp, operands[1], 0);
  emit_insn (gen_fix_truncdf<GPR:mode>2 (operands[0], df_tmp));

  DONE;
})

(define_expand "fixuns_trunc<FP16:mode><GPR:mode>2"
  [(use (match_operand:GPR 0 "vsx_register_operand"))
   (use (match_operand:FP16 1 "vsx_register_operand"))]
  "can_create_pseudo_p ()"
{
  rtx df_tmp = gen_reg_rtx (DFmode);

  convert_move (df_tmp, operands[1], 0);
  emit_insn (gen_fixuns_truncdf<GPR:mode>2 (operands[0], df_tmp));

  DONE;
})

;; Convert integers to 16-bit floating point
(define_expand "float<GPR:mode><FP16:mode>2"
  [(use (match_operand:FP16 0 "vsx_register_operand"))
   (use (match_operand:GPR 1 "vsx_register_operand"))]
  "can_create_pseudo_p ()"
{
  rtx df_tmp = gen_reg_rtx (DFmode);

  emit_insn (gen_float<GPR:mode>df2 (df_tmp, operands[1]));
  convert_move (operands[0], df_tmp, 0);
  DONE;
})

(define_expand "floatuns<GPR:mode><FP16:mode>2"
  [(use (match_operand:FP16 0 "vsx_register_operand"))
   (use (match_operand:GPR 1 "vsx_register_operand"))]
  "can_create_pseudo_p ()"
{
  rtx df_tmp = gen_reg_rtx (DFmode);
  emit_insn (gen_floatuns<GPR:mode>df2 (df_tmp, operands[1]));
  convert_move (operands[0], df_tmp, 0);
  DONE;
})

;; Negate 16-bit floating point by XOR with -0.0.

(define_insn_and_split "neg<mode>2"
  [(set (match_operand:FP16 0 "gpc_reg_operand" "=wa,?wr")
	(neg:FP16 (match_operand:FP16 1 "gpc_reg_operand" "wa,wr")))
   (clobber (match_scratch:FP16 2 "=&wa,&r"))]
  "TARGET_FLOAT16"
  "#"
  "&& 1"
  [(set (match_dup 2)
	(match_dup 3))
   (set (match_dup 0)
	(xor:FP16 (match_dup 1)
		  (match_dup 2)))]
{
  if (GET_CODE (operands[2]) == SCRATCH)
    operands[2] = gen_reg_rtx (<MODE>mode);

  REAL_VALUE_TYPE dconst;

  gcc_assert (real_from_string (&dconst, "-0.0") == 0);

  rtx rc = const_double_from_real_value (dconst, <MODE>mode);
  if (!TARGET_PREFIXED)
    rc = force_const_mem (<MODE>mode, rc);

  operands[3] = rc;
}
  [(set_attr "type" "veclogical,integer")
   (set_attr "length" "16")])

;; 16-bit floating point absolute value

(define_insn_and_split "abs<mode>2"
  [(set (match_operand:FP16 0 "gpc_reg_operand" "=wa,?wr")
	(abs:FP16
	 (match_operand:FP16 1 "gpc_reg_operand" "wa,wr")))
   (clobber (match_scratch:FP16 2 "=&wa,&r"))]
  "TARGET_FLOAT16"
  "#"
  "&& 1"
  [(set (match_dup 2)
	(match_dup 3))
   (set (match_dup 0)
	(and:FP16 (not:FP16 (match_dup 2))
		  (match_dup 1)))]
		  
{
  if (GET_CODE (operands[2]) == SCRATCH)
    operands[2] = gen_reg_rtx (<MODE>mode);

  REAL_VALUE_TYPE dconst;

  gcc_assert (real_from_string (&dconst, "-0.0") == 0);

  rtx rc = const_double_from_real_value (dconst, <MODE>mode);

  if (!TARGET_PREFIXED)
    rc = force_const_mem (<MODE>mode, rc);

  operands[3] = rc;
}
  [(set_attr "type" "veclogical,integer")
   (set_attr "length" "16")])

;; 16-bit negative floating point absolute value

(define_insn_and_split "*nabs<mode>2"
  [(set (match_operand:FP16 0 "gpc_reg_operand" "=wa,?wr")
	(neg:FP16
	 (abs:FP16
	  (match_operand:FP16 1 "gpc_reg_operand" "wa,wr"))))
   (clobber (match_scratch:FP16 2 "=&wa,&r"))]
  "TARGET_FLOAT16"
  "#"
  "&& 1"
  [(set (match_dup 2)
	(match_dup 3))
   (set (match_dup 0)
	(ior:FP16 (match_dup 1)
		  (match_dup 2)))]
{
  if (GET_CODE (operands[2]) == SCRATCH)
    operands[2] = gen_reg_rtx (<MODE>mode);

  REAL_VALUE_TYPE dconst;

  gcc_assert (real_from_string (&dconst, "-0.0") == 0);
  rtx rc = const_double_from_real_value (dconst, <MODE>mode);

  if (!TARGET_PREFIXED)
    rc = force_const_mem (<MODE>mode, rc);

  operands[3] = rc;
}
  [(set_attr "type" "veclogical,integer")
   (set_attr "length" "16")])

;; Add logical operations for 16-bit floating point types that are used
;; for things like negate, abs, and negative abs.  Possibly in the
;; future we might need logical operators for extracting exponents and
;; mantissas.
(define_expand "and<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
	(and:FP16 (match_operand:FP16 1 "gpc_reg_operand")
		  (match_operand:FP16 2 "gpc_reg_operand")))]
  "TARGET_FLOAT16"
  "")

(define_expand "ior<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
        (ior:FP16 (match_operand:FP16 1 "gpc_reg_operand")
		  (match_operand:FP16 2 "gpc_reg_operand")))]
  "TARGET_FLOAT16"
  "")

(define_expand "xor<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
        (xor:FP16 (match_operand:FP16 1 "gpc_reg_operand")
		  (match_operand:FP16 2 "gpc_reg_operand")))]
  "TARGET_FLOAT16"
  "")

(define_expand "nor<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
	(and:FP16
	 (not:FP16 (match_operand:FP16 1 "gpc_reg_operand"))
	 (not:FP16 (match_operand:FP16 2 "gpc_reg_operand"))))]
  "TARGET_FLOAT16"
  "")

(define_expand "andn<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
        (and:FP16
	 (not:FP16 (match_operand:FP16 2 "gpc_reg_operand"))
	 (match_operand:FP16 1 "gpc_reg_operand")))]
  "TARGET_FLOAT16"
  "")

(define_expand "eqv<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
	(not:FP16
	 (xor:FP16 (match_operand:FP16 1 "gpc_reg_operand")
		   (match_operand:FP16 2 "gpc_reg_operand"))))]
  "TARGET_FLOAT16"
  "")

;; Rewrite nand into canonical form
(define_expand "nand<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
	(ior:FP16
	 (not:FP16 (match_operand:FP16 1 "gpc_reg_operand"))
	 (not:FP16 (match_operand:FP16 2 "gpc_reg_operand"))))]
  "TARGET_FLOAT16"
  "")

;; The canonical form is to have the negated element first, so we need to
;; reverse arguments.
(define_expand "iorn<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand")
	(ior:FP16
	 (not:FP16 (match_operand:FP16 2 "gpc_reg_operand"))
	 (match_operand:FP16 1 "gpc_reg_operand")))]
  "TARGET_FLOAT16"
  "")

;; AND, IOR, and XOR insns.  Unlike HImode operations prefer using
;; floating point/vector registers over GPRs.
(define_insn "*bool<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand" "=wa,r")
	(match_operator:FP16 3 "boolean_operator"
	 [(match_operand:FP16 1 "gpc_reg_operand" "wa,r")
	  (match_operand:FP16 2 "gpc_reg_operand" "wa,r")]))]
  "TARGET_FLOAT16"
  "@
   xxl%q3 %x0,%x1,%x2
   %q3 %0,%1,%2"
  [(set_attr "type" "veclogical,logical")])

;; ANDC, IORC, and EQV insns.
(define_insn "*boolc<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand" "=wa,r")
	(match_operator:FP16 3 "boolean_operator"
	 [(not:FP16 (match_operand:FP16 2 "gpc_reg_operand" "wa,r"))
	  (match_operand:FP16 1 "gpc_reg_operand" "wa,r")]))]
  "TARGET_FLOAT16"
  "@
   xxl%q3 %x0,%x1,%x2
   %q3 %0,%1,%2"
  [(set_attr "type" "veclogical,logical")])

;; NOR and NAND insns.
(define_insn "*boolcc<mode>3"
  [(set (match_operand:FP16 0 "gpc_reg_operand" "=wa,r")
	(match_operator:FP16 3 "boolean_operator"
	 [(not:FP16 (match_operand:FP16 1 "gpc_reg_operand" "wa,r"))
	  (not:FP16 (match_operand:FP16 2 "gpc_reg_operand" "wa,r"))]))]
  "TARGET_FLOAT16"
  "@
   xxl%q3 %x0,%x1,%x2
   %q3 %0,%1,%2"
  [(set_attr "type" "veclogical,logical")])

;; Add vectorization support for 16-bit floating point.

;; Binary operators being vectorized.
(define_insn_and_split "<fp16_names><mode>3"
  [(set (match_operand:VFP16_HW 0 "vsx_register_operand")
	(FP16_BINARY_OP:VFP16_HW
	 (match_operand:VFP16_HW 1 "vsx_register_operand")
	 (match_operand:VFP16_HW 2 "vsx_register_operand")))]
  "can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  fp16_vectorization (<CODE>, operands[0], operands[1], operands[2], NULL_RTX,
		      FP16_BINARY);
  DONE;
})

;; FMA operations being vectorized.
(define_insn_and_split "fma<mode>4"
  [(set (match_operand:VFP16_HW 0 "vsx_register_operand")
	(fma:VFP16_HW
	 (match_operand:VFP16_HW 1 "vsx_register_operand")
	 (match_operand:VFP16_HW 2 "vsx_register_operand")
	 (match_operand:VFP16_HW 3 "vsx_register_operand")))]
  "can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  fp16_vectorization (FMA, operands[0], operands[1], operands[2],
		      operands[3], FP16_FMA);
  DONE;
})

(define_insn_and_split "*fms<mode>4"
  [(set (match_operand:VFP16_HW 0 "vsx_register_operand")
	(fma:VFP16_HW
	 (match_operand:VFP16_HW 1 "vsx_register_operand")
	 (match_operand:VFP16_HW 2 "vsx_register_operand")
	 (neg:VFP16_HW
	  (match_operand:VFP16_HW 3 "vsx_register_operand"))))]
  "can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  fp16_vectorization (FMA, operands[0], operands[1], operands[2],
		      operands[3], FP16_FMS);
  DONE;
})

(define_insn_and_split "*nfma<mode>4"
  [(set (match_operand:VFP16_HW 0 "vsx_register_operand")
	(neg:VFP16_HW
	 (fma:VFP16_HW
	  (match_operand:VFP16_HW 1 "vsx_register_operand")
	  (match_operand:VFP16_HW 2 "vsx_register_operand")
	  (match_operand:VFP16_HW 3 "vsx_register_operand"))))]
  "can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  fp16_vectorization (FMA, operands[0], operands[1], operands[2],
		      operands[3], FP16_NFMA);
  DONE;
})

(define_insn_and_split "*nfms<mode>4"
  [(set (match_operand:VFP16_HW 0 "vsx_register_operand")
	(neg:VFP16_HW
	 (fma:VFP16_HW
	  (match_operand:VFP16_HW 1 "vsx_register_operand")
	  (match_operand:VFP16_HW 2 "vsx_register_operand")
	  (neg:VFP16_HW
	   (match_operand:VFP16_HW 3 "vsx_register_operand")))))]
  "can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  fp16_vectorization (FMA, operands[0], operands[1], operands[2],
		      operands[3], FP16_NFMS);
  DONE;
})

;; Vector Pack support.

(define_expand "vec_pack_trunc_v4sf_v8hf"
  [(match_operand:V8HF 0 "vfloat_operand")
   (match_operand:V4SF 1 "vfloat_operand")
   (match_operand:V4SF 2 "vfloat_operand")]
  "TARGET_FLOAT16_HW"
{
  rtx r1 = gen_reg_rtx (V8HFmode);
  rtx r2 = gen_reg_rtx (V8HFmode);

  emit_insn (gen_xvcvsphp_v8hf (r1, operands[1]));
  emit_insn (gen_xvcvsphp_v8hf (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

(define_expand "vec_pack_trunc_v4sf_v8bf"
  [(match_operand:V8BF 0 "vfloat_operand")
   (match_operand:V4SF 1 "vfloat_operand")
   (match_operand:V4SF 2 "vfloat_operand")]
  "TARGET_BFLOAT16_HW"
{
  rtx r1 = gen_reg_rtx (V8BFmode);
  rtx r2 = gen_reg_rtx (V8BFmode);

  emit_insn (gen_xvcvspbf16_v8bf (r1, operands[1]));
  emit_insn (gen_xvcvspbf16_v8bf (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

;; Unfortunately the machine independent code assumes there is only one
;; 16-bit floating point type.  This means we have to choose whether to
;; support packing _Float16 or __bfloat16.  It looks like __bfloat16 is
;; more popular, so we choose __bfloat16 to be the default.

(define_expand "vec_pack_trunc_v4sf"
  [(match_operand:V8BF 0 "vfloat_operand")
   (match_operand:V4SF 1 "vfloat_operand")
   (match_operand:V4SF 2 "vfloat_operand")]
  "TARGET_BFLOAT16_HW"
{
  rtx r1 = gen_reg_rtx (V8BFmode);
  rtx r2 = gen_reg_rtx (V8BFmode);

  emit_insn (gen_xvcvspbf16_v8bf (r1, operands[1]));
  emit_insn (gen_xvcvspbf16_v8bf (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

;; Used for vector conversion to _Float16
(define_insn "xvcvsphp_v8hf"
  [(set (match_operand:V8HF 0 "vsx_register_operand" "=wa")
	(unspec:V8HF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		     UNSPEC_XVCVSPHP_V8HF))]
  "TARGET_FLOAT16_HW"
  "xvcvsphp %x0,%x1"
[(set_attr "type" "vecfloat")])

;; Used for vector conversion to __bfloat16
(define_insn "xvcvspbf16_v8bf"
  [(set (match_operand:V8BF 0 "vsx_register_operand" "=wa")
	(unspec:V8BF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		     UNSPEC_XVCVSPBF16_V8BF))]
  "TARGET_BFLOAT16_HW"
  "xvcvspbf16 %x0,%x1"
  [(set_attr "type" "vecfloat")])

;; Vector unpack support.  Given the name is for the type being
;; unpacked, we can unpack both __bfloat16 and _Float16.

;; Unpack vector _Float16
(define_expand "vec_unpacks_hi_v8hf"
  [(match_operand:V4SF 0 "vfloat_operand")
   (match_operand:V8HF 1 "vfloat_operand")]
  "TARGET_FLOAT16_HW"
{
  rtx reg = gen_reg_rtx (V8HFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_xvcvhpsp_v8hf (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_lo_v8hf"
  [(match_operand:V4SF 0 "vfloat_operand")
   (match_operand:V8HF 1 "vfloat_operand")]
  "TARGET_FLOAT16_HW"
{
  rtx reg = gen_reg_rtx (V8HFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_xvcvhpsp_v8hf (operands[0], reg));
  DONE;
})

;; Used for vector conversion from _Float16
(define_insn "xvcvhpsp_v8hf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:V8HF 1 "vsx_register_operand" "wa")]
		     UNSPEC_CVT_FP16_TO_V4SF))]
  "TARGET_FLOAT16_HW"
  "xvcvhpsp %x0,%x1"
  [(set_attr "type" "vecperm")])

;; Unpack vector __bfloat16
(define_expand "vec_unpacks_hi_v8bf"
  [(match_operand:V4SF 0 "vfloat_operand")
   (match_operand:V8BF 1 "vfloat_operand")]
  "TARGET_BFLOAT16_HW"
{
  rtx reg = gen_reg_rtx (V8BFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_xvcvbf16spn_v8bf (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_lo_v8bf"
  [(match_operand:V4SF 0 "vfloat_operand")
   (match_operand:V8BF 1 "vfloat_operand")]
  "TARGET_BFLOAT16_HW"
{
  rtx reg = gen_reg_rtx (V8BFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_xvcvbf16spn_v8bf (operands[0], reg));
  DONE;
})

;; Used for vector conversion from __bfloat16
(define_insn "xvcvbf16spn_v8bf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:V8BF 1 "vsx_register_operand" "wa")]
		     UNSPEC_CVT_FP16_TO_V4SF))]
  "TARGET_BFLOAT16_HW"
  "xvcvbf16spn %x0,%x1"
  [(set_attr "type" "vecperm")])

(define_insn "xvcvbf16spn_bf"
  [(set (match_operand:V4SF 0 "vsx_register_operand" "=wa")
	(unspec:V4SF [(match_operand:V4SF 1 "vsx_register_operand" "wa")]
		     UNSPEC_CVT_FP16_TO_V4SF))]
  "TARGET_BFLOAT16_HW"
  "xvcvbf16spn %x0,%x1"
  [(set_attr "type" "vecperm")])

;; Optimize __bfloat16 binary operations.  Unlike _Float16 where we
;; have instructions to convert between HFmode and SFmode as scalar
;; values, with BFmode, we only have vector conversions.  Thus to do:
;;
;;      __bfloat16 a, b, c;
;;      a = b + c;
;;
;; the GCC compiler would normally generate:
;;
;;      lxsihzx 0,4,2           // load __bfloat16 value b
;;      lxsihzx 12,5,2          // load __bfloat16 value c
;;      xxsldwi 0,0,0,1         // shift b into bits 16..31
;;      xxsldwi 12,12,12,1      // shift c into bits 16..31
;;      xvcvbf16spn 0,0         // vector convert b into V4SFmode
;;      xvcvbf16spn 12,12       // vector convert c into V4SFmode
;;      xscvspdpn 0,0           // convert b into SFmode scalar
;;      xscvspdpn 12,12         // convert c into SFmode scalar
;;      fadds 0,0,12            // add b+c
;;      xscvdpspn 0,0           // convert b+c into SFmode memory format
;;      xvcvspbf16 0,0          // convert b+c into BFmode memory format
;;      stxsihx 0,3,2           // store b+c
;;
;; Using the following combiner patterns, the code generated would now
;; be:
;;
;;      lxsihzx 12,4,2          // load __bfloat16 value b
;;      lxsihzx 0,5,2           // load __bfloat16 value c
;;      xxspltw 12,12,1         // shift b into bits 16..31
;;      xxspltw 0,0,1           // shift c into bits 16..31
;;      xvcvbf16spn 12,12       // vector convert b into V4SFmode
;;      xvcvbf16spn 0,0         // vector convert c into V4SFmode
;;      xvaddsp 0,0,12          // vector b+c in V4SFmode
;;      xvcvspbf16 0,0          // convert b+c into BFmode memory format
;;      stxsihx 0,3,2           // store b+c
;;
;; We cannot just define insns like 'addbf3' to keep the operation as
;; BFmode because GCC will not generate these patterns unless the user
;; uses -Ofast.  Without -Ofast, it will always convert BFmode into
;; SFmode.

(define_insn_and_split "*bfloat16_as_v4sf_1"
  [(set (match_operand:BF 0 "vsx_register_operand")
	(match_operand:BF 1 "bfloat16_to_v4sf_operand"))]
  "TARGET_BF_V4SF
   && TARGET_BFLOAT16_HW
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  bfloat16_to_v4sf_split (operands[0], operands[1]);
  DONE;
})

(define_insn_and_split "*bfloat16_as_v4sf_2"
  [(set (match_operand:BF 0 "vsx_register_operand")
	(float_truncate:BF
	 (match_operand:SF 1 "bfloat16_to_v4sf_operand")))]
  "TARGET_BF_V4SF
   && TARGET_BFLOAT16_HW
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  bfloat16_to_v4sf_split (operands[0], operands[1]);
  DONE;
})

(define_insn_and_split "*bfloat16_as_v4sf_3"
  [(set (match_operand:SF 0 "vsx_register_operand")
	(match_operand:SF 1 "bfloat16_to_v4sf_operand"))]
  "TARGET_BF_V4SF
   && TARGET_BFLOAT16_HW
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(pc)]
{
  bfloat16_to_v4sf_split (operands[0], operands[1]);
  DONE;
})

