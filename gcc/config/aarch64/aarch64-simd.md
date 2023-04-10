;; Machine description for AArch64 AdvSIMD architecture.
;; Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

(define_expand "mov<mode>"
  [(set (match_operand:VALL_F16 0 "nonimmediate_operand")
	(match_operand:VALL_F16 1 "general_operand"))]
  "TARGET_FLOAT"
  "
  /* Force the operand into a register if it is not an
     immediate whose use can be replaced with xzr.
     If the mode is 16 bytes wide, then we will be doing
     a stp in DI mode, so we check the validity of that.
     If the mode is 8 bytes wide, then we will do doing a
     normal str, so the check need not apply.  */
  if (GET_CODE (operands[0]) == MEM
      && !(aarch64_simd_imm_zero (operands[1], <MODE>mode)
	   && ((known_eq (GET_MODE_SIZE (<MODE>mode), 16)
		&& aarch64_mem_pair_operand (operands[0], DImode))
	       || known_eq (GET_MODE_SIZE (<MODE>mode), 8))))
      operands[1] = force_reg (<MODE>mode, operands[1]);

  /* If a constant is too complex to force to memory (e.g. because it
     contains CONST_POLY_INTs), build it up from individual elements instead.
     We should only need to do this before RA; aarch64_legitimate_constant_p
     should ensure that we don't try to rematerialize the constant later.  */
  if (GET_CODE (operands[1]) == CONST_VECTOR
      && targetm.cannot_force_const_mem (<MODE>mode, operands[1]))
    {
      aarch64_expand_vector_init (operands[0], operands[1]);
      DONE;
    }
  "
)

(define_expand "movmisalign<mode>"
  [(set (match_operand:VALL_F16 0 "nonimmediate_operand")
        (match_operand:VALL_F16 1 "general_operand"))]
  "TARGET_FLOAT && !STRICT_ALIGNMENT"
{
  /* This pattern is not permitted to fail during expansion: if both arguments
     are non-registers (e.g. memory := constant, which can be created by the
     auto-vectorizer), force operand 1 into a register.  */
  if (!register_operand (operands[0], <MODE>mode)
      && !register_operand (operands[1], <MODE>mode))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn "aarch64_simd_dup<mode>"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w, w")
	(vec_duplicate:VDQ_I
	  (match_operand:<VEL> 1 "register_operand" "w,?r")))]
  "TARGET_SIMD"
  "@
   dup\\t%0.<Vtype>, %1.<Vetype>[0]
   dup\\t%0.<Vtype>, %<vwcore>1"
  [(set_attr "type" "neon_dup<q>, neon_from_gp<q>")]
)

(define_insn "aarch64_simd_dup<mode>"
  [(set (match_operand:VDQF_F16 0 "register_operand" "=w,w")
	(vec_duplicate:VDQF_F16
	  (match_operand:<VEL> 1 "register_operand" "w,r")))]
  "TARGET_SIMD"
  "@
   dup\\t%0.<Vtype>, %1.<Vetype>[0]
   dup\\t%0.<Vtype>, %<vwcore>1"
  [(set_attr "type" "neon_dup<q>, neon_from_gp<q>")]
)

(define_insn "aarch64_dup_lane<mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(vec_duplicate:VALL_F16
	  (vec_select:<VEL>
	    (match_operand:VALL_F16 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")])
          )))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[2]));
    return "dup\\t%0.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_dup<q>")]
)

(define_insn "aarch64_dup_lane_<vswap_width_name><mode>"
  [(set (match_operand:VALL_F16_NO_V2Q 0 "register_operand" "=w")
	(vec_duplicate:VALL_F16_NO_V2Q
	  (vec_select:<VEL>
	    (match_operand:<VSWAP_WIDTH> 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")])
          )))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSWAP_WIDTH>mode, INTVAL (operands[2]));
    return "dup\\t%0.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_dup<q>")]
)

(define_insn "*aarch64_simd_mov<VDMOV:mode>"
  [(set (match_operand:VDMOV 0 "nonimmediate_operand"
		"=w, m,  m,  w, ?r, ?w, ?r,  w,  w")
	(match_operand:VDMOV 1 "general_operand"
		"m,  Dz, w,  w,  w,  r,  r, Dn, Dz"))]
  "TARGET_FLOAT
   && (register_operand (operands[0], <MODE>mode)
       || aarch64_simd_reg_or_zero (operands[1], <MODE>mode))"
{
   switch (which_alternative)
     {
     case 0: return "ldr\t%d0, %1";
     case 1: return "str\txzr, %0";
     case 2: return "str\t%d1, %0";
     case 3:
       if (TARGET_SIMD)
	 return "mov\t%0.<Vbtype>, %1.<Vbtype>";
       return "fmov\t%d0, %d1";
     case 4:
       if (TARGET_SIMD)
	 return "umov\t%0, %1.d[0]";
       return "fmov\t%x0, %d1";
     case 5: return "fmov\t%d0, %1";
     case 6: return "mov\t%0, %1";
     case 7:
	return aarch64_output_simd_mov_immediate (operands[1], 64);
     case 8: return "fmov\t%d0, xzr";
     default: gcc_unreachable ();
     }
}
  [(set_attr "type" "neon_load1_1reg<q>, store_8, neon_store1_1reg<q>,\
		     neon_logic<q>, neon_to_gp<q>, f_mcr,\
		     mov_reg, neon_move<q>, f_mcr")
   (set_attr "arch" "*,*,*,*,*,*,*,simd,*")]
)

(define_insn "*aarch64_simd_mov<VQMOV:mode>"
  [(set (match_operand:VQMOV 0 "nonimmediate_operand"
		"=w, Umn,  m,  w, ?r, ?w, ?r, w,  w")
	(match_operand:VQMOV 1 "general_operand"
		"m,  Dz, w,  w,  w,  r,  r, Dn, Dz"))]
  "TARGET_FLOAT
   && (register_operand (operands[0], <MODE>mode)
       || aarch64_simd_reg_or_zero (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0:
	return "ldr\t%q0, %1";
    case 1:
	return "stp\txzr, xzr, %0";
    case 2:
	return "str\t%q1, %0";
    case 3:
	return "mov\t%0.<Vbtype>, %1.<Vbtype>";
    case 4:
    case 5:
    case 6:
	return "#";
    case 7:
	return aarch64_output_simd_mov_immediate (operands[1], 128);
    case 8:
	return "fmov\t%d0, xzr";
    default:
	gcc_unreachable ();
    }
}
  [(set_attr "type" "neon_load1_1reg<q>, store_16, neon_store1_1reg<q>,\
		     neon_logic<q>, multiple, multiple,\
		     multiple, neon_move<q>, fmov")
   (set_attr "length" "4,4,4,4,8,8,8,4,4")
   (set_attr "arch" "*,*,*,simd,*,*,*,simd,*")]
)

;; When storing lane zero we can use the normal STR and its more permissive
;; addressing modes.

(define_insn "aarch64_store_lane0<mode>"
  [(set (match_operand:<VEL> 0 "memory_operand" "=m")
	(vec_select:<VEL> (match_operand:VALL_F16 1 "register_operand" "w")
			(parallel [(match_operand 2 "const_int_operand" "n")])))]
  "TARGET_SIMD
   && ENDIAN_LANE_N (<nunits>, INTVAL (operands[2])) == 0"
  "str\\t%<Vetype>1, %0"
  [(set_attr "type" "neon_store1_1reg<q>")]
)

(define_insn "load_pair<DREG:mode><DREG2:mode>"
  [(set (match_operand:DREG 0 "register_operand" "=w")
	(match_operand:DREG 1 "aarch64_mem_pair_operand" "Ump"))
   (set (match_operand:DREG2 2 "register_operand" "=w")
	(match_operand:DREG2 3 "memory_operand" "m"))]
  "TARGET_FLOAT
   && rtx_equal_p (XEXP (operands[3], 0),
		   plus_constant (Pmode,
				  XEXP (operands[1], 0),
				  GET_MODE_SIZE (<DREG:MODE>mode)))"
  "ldp\\t%d0, %d2, %z1"
  [(set_attr "type" "neon_ldp")]
)

(define_insn "vec_store_pair<DREG:mode><DREG2:mode>"
  [(set (match_operand:DREG 0 "aarch64_mem_pair_operand" "=Ump")
	(match_operand:DREG 1 "register_operand" "w"))
   (set (match_operand:DREG2 2 "memory_operand" "=m")
	(match_operand:DREG2 3 "register_operand" "w"))]
  "TARGET_FLOAT
   && rtx_equal_p (XEXP (operands[2], 0),
		   plus_constant (Pmode,
				  XEXP (operands[0], 0),
				  GET_MODE_SIZE (<DREG:MODE>mode)))"
  "stp\\t%d1, %d3, %z0"
  [(set_attr "type" "neon_stp")]
)

(define_insn "load_pair<VQ:mode><VQ2:mode>"
  [(set (match_operand:VQ 0 "register_operand" "=w")
	(match_operand:VQ 1 "aarch64_mem_pair_operand" "Ump"))
   (set (match_operand:VQ2 2 "register_operand" "=w")
	(match_operand:VQ2 3 "memory_operand" "m"))]
  "TARGET_FLOAT
    && rtx_equal_p (XEXP (operands[3], 0),
		    plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (<VQ:MODE>mode)))"
  "ldp\\t%q0, %q2, %z1"
  [(set_attr "type" "neon_ldp_q")]
)

(define_insn "vec_store_pair<VQ:mode><VQ2:mode>"
  [(set (match_operand:VQ 0 "aarch64_mem_pair_operand" "=Ump")
	(match_operand:VQ 1 "register_operand" "w"))
   (set (match_operand:VQ2 2 "memory_operand" "=m")
	(match_operand:VQ2 3 "register_operand" "w"))]
  "TARGET_FLOAT
   && rtx_equal_p (XEXP (operands[2], 0),
		   plus_constant (Pmode,
				  XEXP (operands[0], 0),
				  GET_MODE_SIZE (<VQ:MODE>mode)))"
  "stp\\t%q1, %q3, %z0"
  [(set_attr "type" "neon_stp_q")]
)


(define_split
  [(set (match_operand:VQMOV 0 "register_operand" "")
	(match_operand:VQMOV 1 "register_operand" ""))]
  "TARGET_FLOAT
   && reload_completed
   && GP_REGNUM_P (REGNO (operands[0]))
   && GP_REGNUM_P (REGNO (operands[1]))"
  [(const_int 0)]
{
  aarch64_simd_emit_reg_reg_move (operands, DImode, 2);
  DONE;
})

(define_split
  [(set (match_operand:VQMOV 0 "register_operand" "")
        (match_operand:VQMOV 1 "register_operand" ""))]
  "TARGET_FLOAT
   && reload_completed
   && ((FP_REGNUM_P (REGNO (operands[0])) && GP_REGNUM_P (REGNO (operands[1])))
       || (GP_REGNUM_P (REGNO (operands[0])) && FP_REGNUM_P (REGNO (operands[1]))))"
  [(const_int 0)]
{
  aarch64_split_simd_move (operands[0], operands[1]);
  DONE;
})

(define_expand "@aarch64_split_simd_mov<mode>"
  [(set (match_operand:VQMOV 0)
	(match_operand:VQMOV 1))]
  "TARGET_FLOAT"
  {
    rtx dst = operands[0];
    rtx src = operands[1];

    if (GP_REGNUM_P (REGNO (src)))
      {
	rtx src_low_part = gen_lowpart (<VHALF>mode, src);
	rtx src_high_part = gen_highpart (<VHALF>mode, src);
	rtx dst_low_part = gen_lowpart (<VHALF>mode, dst);

	emit_move_insn (dst_low_part, src_low_part);
	emit_insn (gen_aarch64_combine<Vhalf> (dst, dst_low_part,
					       src_high_part));
      }
    else
      {
	rtx dst_low_part = gen_lowpart (<VHALF>mode, dst);
	rtx dst_high_part = gen_highpart (<VHALF>mode, dst);
	rtx lo = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
	rtx hi = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
	emit_insn (gen_aarch64_get_half<mode> (dst_low_part, src, lo));
	emit_insn (gen_aarch64_get_half<mode> (dst_high_part, src, hi));
      }
    DONE;
  }
)

(define_expand "aarch64_get_half<mode>"
  [(set (match_operand:<VHALF> 0 "register_operand")
        (vec_select:<VHALF>
          (match_operand:VQMOV 1 "register_operand")
          (match_operand 2 "ascending_int_parallel")))]
  "TARGET_FLOAT"
  {
    if (vect_par_cnst_lo_half (operands[2], <MODE>mode))
      {
	emit_move_insn (operands[0], gen_lowpart (<VHALF>mode, operands[1]));
	DONE;
      }
  }
)

(define_expand "aarch64_get_low<mode>"
  [(match_operand:<VHALF> 0 "register_operand")
   (match_operand:VQMOV 1 "register_operand")]
  "TARGET_FLOAT"
  {
    rtx lo = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
    emit_insn (gen_aarch64_get_half<mode> (operands[0], operands[1], lo));
    DONE;
  }
)

(define_expand "aarch64_get_high<mode>"
  [(match_operand:<VHALF> 0 "register_operand")
   (match_operand:VQMOV 1 "register_operand")]
  "TARGET_FLOAT"
  {
    rtx hi = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
    emit_insn (gen_aarch64_get_half<mode> (operands[0], operands[1], hi));
    DONE;
  }
)

(define_insn_and_split "aarch64_simd_mov_from_<mode>low"
  [(set (match_operand:<VHALF> 0 "register_operand" "=w,?r")
        (vec_select:<VHALF>
          (match_operand:VQMOV_NO2E 1 "register_operand" "w,w")
          (match_operand:VQMOV_NO2E 2 "vect_par_cnst_lo_half" "")))]
  "TARGET_SIMD"
  "@
   #
   umov\t%0, %1.d[0]"
  "&& reload_completed && aarch64_simd_register (operands[0], <VHALF>mode)"
  [(set (match_dup 0) (match_dup 1))]
  {
    operands[1] = aarch64_replace_reg_mode (operands[1], <VHALF>mode);
  }
  [(set_attr "type" "mov_reg,neon_to_gp<q>")
   (set_attr "length" "4")]
)

(define_insn "aarch64_simd_mov_from_<mode>high"
  [(set (match_operand:<VHALF> 0 "register_operand" "=w,?r,?r")
        (vec_select:<VHALF>
          (match_operand:VQMOV_NO2E 1 "register_operand" "w,w,w")
          (match_operand:VQMOV_NO2E 2 "vect_par_cnst_hi_half" "")))]
  "TARGET_FLOAT"
  "@
   dup\t%d0, %1.d[1]
   umov\t%0, %1.d[1]
   fmov\t%0, %1.d[1]"
  [(set_attr "type" "neon_dup<q>,neon_to_gp<q>,f_mrc")
   (set_attr "arch" "simd,simd,*")
   (set_attr "length" "4")]
)

(define_insn "orn<mode>3"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (ior:VDQ_I (not:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w"))
		(match_operand:VDQ_I 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "orn\t%0.<Vbtype>, %2.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "bic<mode>3"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (and:VDQ_I (not:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w"))
		(match_operand:VDQ_I 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "bic\t%0.<Vbtype>, %2.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "add<mode>3"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (plus:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		  (match_operand:VDQ_I 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "add\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_add<q>")]
)

(define_insn "sub<mode>3"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (minus:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		   (match_operand:VDQ_I 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "sub\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_sub<q>")]
)

(define_insn "mul<mode>3"
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
        (mult:VDQ_BHSI (match_operand:VDQ_BHSI 1 "register_operand" "w")
		   (match_operand:VDQ_BHSI 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "mul\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_mul_<Vetype><q>")]
)

(define_insn "bswap<mode>2"
  [(set (match_operand:VDQHSD 0 "register_operand" "=w")
        (bswap:VDQHSD (match_operand:VDQHSD 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "rev<Vrevsuff>\\t%0.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_rev<q>")]
)

(define_insn "aarch64_rbit<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "w")]
		   UNSPEC_RBIT))]
  "TARGET_SIMD"
  "rbit\\t%0.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_rbit")]
)

(define_expand "ctz<mode>2"
  [(set (match_operand:VS 0 "register_operand")
        (ctz:VS (match_operand:VS 1 "register_operand")))]
  "TARGET_SIMD"
  {
     emit_insn (gen_bswap<mode>2 (operands[0], operands[1]));
     rtx op0_castsi2qi = simplify_gen_subreg(<VS:VSI2QI>mode, operands[0],
					     <MODE>mode, 0);
     emit_insn (gen_aarch64_rbit<VS:vsi2qi> (op0_castsi2qi, op0_castsi2qi));
     emit_insn (gen_clz<mode>2 (operands[0], operands[0]));
     DONE;
  }
)

(define_expand "xorsign<mode>3"
  [(match_operand:VHSDF 0 "register_operand")
   (match_operand:VHSDF 1 "register_operand")
   (match_operand:VHSDF 2 "register_operand")]
  "TARGET_SIMD"
{

  machine_mode imode = <V_INT_EQUIV>mode;
  rtx v_bitmask = gen_reg_rtx (imode);
  rtx op1x = gen_reg_rtx (imode);
  rtx op2x = gen_reg_rtx (imode);

  rtx arg1 = lowpart_subreg (imode, operands[1], <MODE>mode);
  rtx arg2 = lowpart_subreg (imode, operands[2], <MODE>mode);

  int bits = GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1;

  emit_move_insn (v_bitmask,
		  aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
						     HOST_WIDE_INT_M1U << bits));

  emit_insn (gen_and<v_int_equiv>3 (op2x, v_bitmask, arg2));
  emit_insn (gen_xor<v_int_equiv>3 (op1x, arg1, op2x));
  emit_move_insn (operands[0],
		  lowpart_subreg (<MODE>mode, op1x, imode));
  DONE;
}
)

;; The fcadd and fcmla patterns are made UNSPEC for the explicitly due to the
;; fact that their usage need to guarantee that the source vectors are
;; contiguous.  It would be wrong to describe the operation without being able
;; to describe the permute that is also required, but even if that is done
;; the permute would have been created as a LOAD_LANES which means the values
;; in the registers are in the wrong order.
(define_insn "aarch64_fcadd<rot><mode>"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")
		       (match_operand:VHSDF 2 "register_operand" "w")]
		       FCADD))]
  "TARGET_COMPLEX"
  "fcadd\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>, #<rot>"
  [(set_attr "type" "neon_fcadd")]
)

(define_expand "cadd<rot><mode>3"
  [(set (match_operand:VHSDF 0 "register_operand")
	(unspec:VHSDF [(match_operand:VHSDF 1 "register_operand")
		       (match_operand:VHSDF 2 "register_operand")]
		       FCADD))]
  "TARGET_COMPLEX && !BYTES_BIG_ENDIAN"
)

(define_insn "aarch64_fcmla<rot><mode>"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(plus:VHSDF (match_operand:VHSDF 1 "register_operand" "0")
		    (unspec:VHSDF [(match_operand:VHSDF 2 "register_operand" "w")
				   (match_operand:VHSDF 3 "register_operand" "w")]
				   FCMLA)))]
  "TARGET_COMPLEX"
  "fcmla\t%0.<Vtype>, %2.<Vtype>, %3.<Vtype>, #<rot>"
  [(set_attr "type" "neon_fcmla")]
)


(define_insn "aarch64_fcmla_lane<rot><mode>"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(plus:VHSDF (match_operand:VHSDF 1 "register_operand" "0")
		    (unspec:VHSDF [(match_operand:VHSDF 2 "register_operand" "w")
				   (match_operand:VHSDF 3 "register_operand" "w")
				   (match_operand:SI 4 "const_int_operand" "n")]
				   FCMLA)))]
  "TARGET_COMPLEX"
{
  operands[4] = aarch64_endian_lane_rtx (<VHALF>mode, INTVAL (operands[4]));
  return "fcmla\t%0.<Vtype>, %2.<Vtype>, %3.<FCMLA_maybe_lane>, #<rot>";
}
  [(set_attr "type" "neon_fcmla")]
)

(define_insn "aarch64_fcmla_laneq<rot>v4hf"
  [(set (match_operand:V4HF 0 "register_operand" "=w")
	(plus:V4HF (match_operand:V4HF 1 "register_operand" "0")
		   (unspec:V4HF [(match_operand:V4HF 2 "register_operand" "w")
				 (match_operand:V8HF 3 "register_operand" "w")
				 (match_operand:SI 4 "const_int_operand" "n")]
				 FCMLA)))]
  "TARGET_COMPLEX"
{
  operands[4] = aarch64_endian_lane_rtx (V4HFmode, INTVAL (operands[4]));
  return "fcmla\t%0.4h, %2.4h, %3.h[%4], #<rot>";
}
  [(set_attr "type" "neon_fcmla")]
)

(define_insn "aarch64_fcmlaq_lane<rot><mode>"
  [(set (match_operand:VQ_HSF 0 "register_operand" "=w")
	(plus:VQ_HSF (match_operand:VQ_HSF 1 "register_operand" "0")
		     (unspec:VQ_HSF [(match_operand:VQ_HSF 2 "register_operand" "w")
				     (match_operand:<VHALF> 3 "register_operand" "w")
				     (match_operand:SI 4 "const_int_operand" "n")]
				     FCMLA)))]
  "TARGET_COMPLEX"
{
  int nunits = GET_MODE_NUNITS (<VHALF>mode).to_constant ();
  operands[4]
    = gen_int_mode (ENDIAN_LANE_N (nunits / 2, INTVAL (operands[4])), SImode);
  return "fcmla\t%0.<Vtype>, %2.<Vtype>, %3.<FCMLA_maybe_lane>, #<rot>";
}
  [(set_attr "type" "neon_fcmla")]
)

;; The complex mla/mls operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cml<fcmac1><conj_op><mode>4"
  [(set (match_operand:VHSDF 0 "register_operand")
	(plus:VHSDF (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand")
				   (match_operand:VHSDF 2 "register_operand")]
				   FCMLA_OP)
		    (match_operand:VHSDF 3 "register_operand")))]
  "TARGET_COMPLEX && !BYTES_BIG_ENDIAN"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_aarch64_fcmla<rotsplit1><mode> (tmp, operands[3],
						 operands[2], operands[1]));
  emit_insn (gen_aarch64_fcmla<rotsplit2><mode> (operands[0], tmp,
						 operands[2], operands[1]));
  DONE;
})

;; The complex mul operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cmul<conj_op><mode>3"
  [(set (match_operand:VHSDF 0 "register_operand")
	(unspec:VHSDF [(match_operand:VHSDF 1 "register_operand")
		       (match_operand:VHSDF 2 "register_operand")]
		       FCMUL_OP))]
  "TARGET_COMPLEX && !BYTES_BIG_ENDIAN"
{
  rtx tmp = force_reg (<MODE>mode, CONST0_RTX (<MODE>mode));
  rtx res1 = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_aarch64_fcmla<rotsplit1><mode> (res1, tmp,
						 operands[2], operands[1]));
  emit_insn (gen_aarch64_fcmla<rotsplit2><mode> (operands[0], res1,
						 operands[2], operands[1]));
  DONE;
})

;; These expands map to the Dot Product optab the vectorizer checks for
;; and to the intrinsics patttern.
;; The auto-vectorizer expects a dot product builtin that also does an
;; accumulation into the provided register.
;; Given the following pattern
;;
;; for (i=0; i<len; i++) {
;;     c = a[i] * b[i];
;;     r += c;
;; }
;; return result;
;;
;; This can be auto-vectorized to
;; r  = a[0]*b[0] + a[1]*b[1] + a[2]*b[2] + a[3]*b[3];
;;
;; given enough iterations.  However the vectorizer can keep unrolling the loop
;; r += a[4]*b[4] + a[5]*b[5] + a[6]*b[6] + a[7]*b[7];
;; r += a[8]*b[8] + a[9]*b[9] + a[10]*b[10] + a[11]*b[11];
;; ...
;;
;; and so the vectorizer provides r, in which the result has to be accumulated.
(define_insn "<sur>dot_prod<vsi2qi>"
  [(set (match_operand:VS 0 "register_operand" "=w")
	(plus:VS
	  (unspec:VS [(match_operand:<VSI2QI> 1 "register_operand" "w")
		      (match_operand:<VSI2QI> 2 "register_operand" "w")]
		      DOTPROD)
	  (match_operand:VS 3 "register_operand" "0")))]
  "TARGET_DOTPROD"
  "<sur>dot\\t%0.<Vtype>, %1.<Vdottype>, %2.<Vdottype>"
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the Armv8.6-a I8MM usdot
;; (vector) Dot Product operation and the vectorized optab.
(define_insn "usdot_prod<vsi2qi>"
  [(set (match_operand:VS 0 "register_operand" "=w")
	(plus:VS
	  (unspec:VS [(match_operand:<VSI2QI> 1 "register_operand" "w")
		      (match_operand:<VSI2QI> 2 "register_operand" "w")]
	  UNSPEC_USDOT)
	  (match_operand:VS 3 "register_operand" "0")))]
  "TARGET_I8MM"
  "usdot\\t%0.<Vtype>, %1.<Vdottype>, %2.<Vdottype>"
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the Dot Product
;; indexed operations.
(define_insn "aarch64_<sur>dot_lane<vsi2qi>"
  [(set (match_operand:VS 0 "register_operand" "=w")
	(plus:VS
	  (unspec:VS [(match_operand:<VSI2QI> 2 "register_operand" "w")
		      (match_operand:V8QI 3 "register_operand" "<h_con>")
		      (match_operand:SI 4 "immediate_operand" "i")]
		      DOTPROD)
	  (match_operand:VS 1 "register_operand" "0")))]
  "TARGET_DOTPROD"
  {
    operands[4] = aarch64_endian_lane_rtx (V8QImode, INTVAL (operands[4]));
    return "<sur>dot\\t%0.<Vtype>, %2.<Vdottype>, %3.4b[%4]";
  }
  [(set_attr "type" "neon_dot<q>")]
)

(define_insn "aarch64_<sur>dot_laneq<vsi2qi>"
  [(set (match_operand:VS 0 "register_operand" "=w")
	(plus:VS
	  (unspec:VS [(match_operand:<VSI2QI> 2 "register_operand" "w")
		      (match_operand:V16QI 3 "register_operand" "<h_con>")
		      (match_operand:SI 4 "immediate_operand" "i")]
		      DOTPROD)
	  (match_operand:VS 1 "register_operand" "0")))]
  "TARGET_DOTPROD"
  {
    operands[4] = aarch64_endian_lane_rtx (V16QImode, INTVAL (operands[4]));
    return "<sur>dot\\t%0.<Vtype>, %2.<Vdottype>, %3.4b[%4]";
  }
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the armv8.6a I8MM usdot, sudot
;; (by element) Dot Product operations.
(define_insn "aarch64_<DOTPROD_I8MM:sur>dot_lane<VB:isquadop><VS:vsi2qi>"
  [(set (match_operand:VS 0 "register_operand" "=w")
	(plus:VS
	  (unspec:VS [(match_operand:<VS:VSI2QI> 2 "register_operand" "w")
		      (match_operand:VB 3 "register_operand" "w")
		      (match_operand:SI 4 "immediate_operand" "i")]
	  DOTPROD_I8MM)
	  (match_operand:VS 1 "register_operand" "0")))]
  "TARGET_I8MM"
  {
    int nunits = GET_MODE_NUNITS (<VB:MODE>mode).to_constant ();
    int lane = INTVAL (operands[4]);
    operands[4] = gen_int_mode (ENDIAN_LANE_N (nunits / 4, lane), SImode);
    return "<DOTPROD_I8MM:sur>dot\\t%0.<VS:Vtype>, %2.<VS:Vdottype>, %3.4b[%4]";
  }
  [(set_attr "type" "neon_dot<VS:q>")]
)

(define_expand "copysign<mode>3"
  [(match_operand:VHSDF 0 "register_operand")
   (match_operand:VHSDF 1 "register_operand")
   (match_operand:VHSDF 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx v_bitmask = gen_reg_rtx (<V_INT_EQUIV>mode);
  int bits = GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1;

  emit_move_insn (v_bitmask,
		  aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
						     HOST_WIDE_INT_M1U << bits));
  emit_insn (gen_aarch64_simd_bsl<mode> (operands[0], v_bitmask,
					 operands[2], operands[1]));
  DONE;
}
)

(define_insn "mul_lane<mode>3"
 [(set (match_operand:VMULD 0 "register_operand" "=w")
       (mult:VMULD
	 (vec_duplicate:VMULD
	   (vec_select:<VEL>
	     (match_operand:<VCOND> 2 "register_operand" "<h_con>")
	     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	 (match_operand:VMULD 1 "register_operand" "w")))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[3]));
    return "<f>mul\\t%0.<Vtype>, %1.<Vtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon<fp>_mul_<stype>_scalar<q>")]
)

(define_insn "mul_laneq<mode>3"
  [(set (match_operand:VMUL 0 "register_operand" "=w")
     (mult:VMUL
       (vec_duplicate:VMUL
	  (vec_select:<VEL>
	    (match_operand:<VCONQ> 2 "register_operand" "<h_con>")
	    (parallel [(match_operand:SI 3 "immediate_operand")])))
      (match_operand:VMUL 1 "register_operand" "w")))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[3]));
    return "<f>mul\\t%0.<Vtype>, %1.<Vtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon<fp>_mul_<stype>_scalar<q>")]
)

(define_insn "mul_n<mode>3"
 [(set (match_operand:VMUL 0 "register_operand" "=w")
       (mult:VMUL
	 (vec_duplicate:VMUL
	   (match_operand:<VEL> 2 "register_operand" "<h_con>"))
	 (match_operand:VMUL 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "<f>mul\t%0.<Vtype>, %1.<Vtype>, %2.<Vetype>[0]";
  [(set_attr "type" "neon<fp>_mul_<stype>_scalar<q>")]
)

(define_insn "@aarch64_rsqrte<mode>"
  [(set (match_operand:VHSDF_HSDF 0 "register_operand" "=w")
	(unspec:VHSDF_HSDF [(match_operand:VHSDF_HSDF 1 "register_operand" "w")]
		     UNSPEC_RSQRTE))]
  "TARGET_SIMD"
  "frsqrte\\t%<v>0<Vmtype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_fp_rsqrte_<stype><q>")])

(define_insn "@aarch64_rsqrts<mode>"
  [(set (match_operand:VHSDF_HSDF 0 "register_operand" "=w")
	(unspec:VHSDF_HSDF [(match_operand:VHSDF_HSDF 1 "register_operand" "w")
			    (match_operand:VHSDF_HSDF 2 "register_operand" "w")]
	 UNSPEC_RSQRTS))]
  "TARGET_SIMD"
  "frsqrts\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_fp_rsqrts_<stype><q>")])

(define_expand "rsqrt<mode>2"
  [(set (match_operand:VALLF 0 "register_operand")
	(unspec:VALLF [(match_operand:VALLF 1 "register_operand")]
		     UNSPEC_RSQRT))]
  "TARGET_SIMD"
{
  aarch64_emit_approx_sqrt (operands[0], operands[1], true);
  DONE;
})

(define_insn "aarch64_ursqrte<mode>"
[(set (match_operand:VDQ_SI 0 "register_operand" "=w")
      (unspec:VDQ_SI [(match_operand:VDQ_SI 1 "register_operand" "w")]
		   UNSPEC_RSQRTE))]
"TARGET_SIMD"
"ursqrte\\t%<v>0<Vmtype>, %<v>1<Vmtype>"
[(set_attr "type" "neon_fp_rsqrte_<stype><q>")])

(define_insn "*aarch64_mul3_elt_to_64v2df"
  [(set (match_operand:DF 0 "register_operand" "=w")
     (mult:DF
       (vec_select:DF
	 (match_operand:V2DF 1 "register_operand" "w")
	 (parallel [(match_operand:SI 2 "immediate_operand")]))
       (match_operand:DF 3 "register_operand" "w")))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (V2DFmode, INTVAL (operands[2]));
    return "fmul\\t%0.2d, %3.2d, %1.d[%2]";
  }
  [(set_attr "type" "neon_fp_mul_d_scalar_q")]
)

(define_insn "neg<mode>2"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
	(neg:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "neg\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_neg<q>")]
)

(define_insn "abs<mode>2"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (abs:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "abs\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_abs<q>")]
)

;; The intrinsic version of integer ABS must not be allowed to
;; combine with any operation with an integrated ABS step, such
;; as SABD.
(define_insn "aarch64_abs<mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand" "=w")
	  (unspec:VSDQ_I_DI
	    [(match_operand:VSDQ_I_DI 1 "register_operand" "w")]
	   UNSPEC_ABS))]
  "TARGET_SIMD"
  "abs\t%<v>0<Vmtype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_abs<q>")]
)

;; It's tempting to represent SABD as ABS (MINUS op1 op2).
;; This isn't accurate as ABS treats always its input as a signed value.
;; So (ABS:QI (minus:QI 64 -128)) == (ABS:QI (192 or -64 signed)) == 64.
;; Whereas SABD would return 192 (-64 signed) on the above example.
;; Use MINUS ([us]max (op1, op2), [us]min (op1, op2)) instead.
(define_insn "aarch64_<su>abd<mode>"
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
	(minus:VDQ_BHSI
	  (USMAX:VDQ_BHSI
	    (match_operand:VDQ_BHSI 1 "register_operand" "w")
	    (match_operand:VDQ_BHSI 2 "register_operand" "w"))
	  (<max_opp>:VDQ_BHSI
	    (match_dup 1)
	    (match_dup 2))))]
  "TARGET_SIMD"
  "<su>abd\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_abd<q>")]
)


(define_insn "aarch64_<sur>abdl<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:VD_BHSI 1 "register_operand" "w")
			 (match_operand:VD_BHSI 2 "register_operand" "w")]
	ABDL))]
  "TARGET_SIMD"
  "<sur>abdl\t%0.<Vwtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_abd<q>")]
)

(define_insn "aarch64_<sur>abdl2<mode>"
  [(set (match_operand:<VDBLW> 0 "register_operand" "=w")
	(unspec:<VDBLW> [(match_operand:VQW 1 "register_operand" "w")
			 (match_operand:VQW 2 "register_operand" "w")]
	ABDL2))]
  "TARGET_SIMD"
  "<sur>abdl2\t%0.<Vwtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_abd<q>")]
)

(define_insn "aarch64_<sur>abal<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:VD_BHSI 2 "register_operand" "w")
			  (match_operand:VD_BHSI 3 "register_operand" "w")
			 (match_operand:<VWIDE> 1 "register_operand" "0")]
	ABAL))]
  "TARGET_SIMD"
  "<sur>abal\t%0.<Vwtype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_arith_acc<q>")]
)

(define_insn "aarch64_<sur>abal2<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:VQW 2 "register_operand" "w")
			  (match_operand:VQW 3 "register_operand" "w")
			 (match_operand:<VWIDE> 1 "register_operand" "0")]
	ABAL2))]
  "TARGET_SIMD"
  "<sur>abal2\t%0.<Vwtype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_arith_acc<q>")]
)

(define_insn "aarch64_<sur>adalp<mode>"
  [(set (match_operand:<VDBLW> 0 "register_operand" "=w")
	(unspec:<VDBLW> [(match_operand:VDQV_L 2 "register_operand" "w")
			 (match_operand:<VDBLW> 1 "register_operand" "0")]
	ADALP))]
  "TARGET_SIMD"
  "<sur>adalp\t%0.<Vwhalf>, %2.<Vtype>"
  [(set_attr "type" "neon_reduc_add<q>")]
)

;; Emit a sequence to produce a sum-of-absolute-differences of the V16QI
;; inputs in operands 1 and 2.  The sequence also has to perform a widening
;; reduction of the difference into a V4SI vector and accumulate that into
;; operand 3 before copying that into the result operand 0.
;; Perform that with a sequence of:
;; UABDL2	tmp.8h, op1.16b, op2.16b
;; UABAL	tmp.8h, op1.8b, op2.8b
;; UADALP	op3.4s, tmp.8h
;; MOV		op0, op3 // should be eliminated in later passes.
;;
;; For TARGET_DOTPROD we do:
;; MOV	tmp1.16b, #1 // Can be CSE'd and hoisted out of loops.
;; UABD	tmp2.16b, op1.16b, op2.16b
;; UDOT	op3.4s, tmp2.16b, tmp1.16b
;; MOV	op0, op3 // RA will tie the operands of UDOT appropriately.
;;
;; The signed version just uses the signed variants of the above instructions
;; but for TARGET_DOTPROD still emits a UDOT as the absolute difference is
;; unsigned.

(define_expand "<sur>sadv16qi"
  [(use (match_operand:V4SI 0 "register_operand"))
   (unspec:V16QI [(use (match_operand:V16QI 1 "register_operand"))
		  (use (match_operand:V16QI 2 "register_operand"))] ABAL)
   (use (match_operand:V4SI 3 "register_operand"))]
  "TARGET_SIMD"
  {
    if (TARGET_DOTPROD)
      {
	rtx ones = force_reg (V16QImode, CONST1_RTX (V16QImode));
	rtx abd = gen_reg_rtx (V16QImode);
	emit_insn (gen_aarch64_<sur>abdv16qi (abd, operands[1], operands[2]));
	emit_insn (gen_udot_prodv16qi (operands[0], abd, ones, operands[3]));
	DONE;
      }
    rtx reduc = gen_reg_rtx (V8HImode);
    emit_insn (gen_aarch64_<sur>abdl2v16qi (reduc, operands[1],
					    operands[2]));
    emit_insn (gen_aarch64_<sur>abalv8qi (reduc, reduc,
					  gen_lowpart (V8QImode, operands[1]),
					  gen_lowpart (V8QImode,
						       operands[2])));
    emit_insn (gen_aarch64_<sur>adalpv8hi (operands[3], operands[3], reduc));
    emit_move_insn (operands[0], operands[3]);
    DONE;
  }
)

(define_insn "aarch64_<su>aba<mode>"
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
	(plus:VDQ_BHSI (minus:VDQ_BHSI
			 (USMAX:VDQ_BHSI
			   (match_operand:VDQ_BHSI 2 "register_operand" "w")
			   (match_operand:VDQ_BHSI 3 "register_operand" "w"))
			 (<max_opp>:VDQ_BHSI
			   (match_dup 2)
			   (match_dup 3)))
		       (match_operand:VDQ_BHSI 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "<su>aba\t%0.<Vtype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_arith_acc<q>")]
)

(define_insn "fabd<mode>3"
  [(set (match_operand:VHSDF_HSDF 0 "register_operand" "=w")
	(abs:VHSDF_HSDF
	  (minus:VHSDF_HSDF
	    (match_operand:VHSDF_HSDF 1 "register_operand" "w")
	    (match_operand:VHSDF_HSDF 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "fabd\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_fp_abd_<stype><q>")]
)

;; For AND (vector, register) and BIC (vector, immediate)
(define_insn "and<mode>3"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w,w")
	(and:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w,0")
		   (match_operand:VDQ_I 2 "aarch64_reg_or_bic_imm" "w,Db")))]
  "TARGET_SIMD"
  {
    switch (which_alternative)
      {
      case 0:
	return "and\t%0.<Vbtype>, %1.<Vbtype>, %2.<Vbtype>";
      case 1:
	return aarch64_output_simd_mov_immediate (operands[2], <bitsize>,
						  AARCH64_CHECK_BIC);
      default:
	gcc_unreachable ();
      }
  }
  [(set_attr "type" "neon_logic<q>")]
)

;; For ORR (vector, register) and ORR (vector, immediate)
(define_insn "ior<mode>3"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w,w")
	(ior:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w,0")
		   (match_operand:VDQ_I 2 "aarch64_reg_or_orr_imm" "w,Do")))]
  "TARGET_SIMD"
  {
    switch (which_alternative)
      {
      case 0:
	return "orr\t%0.<Vbtype>, %1.<Vbtype>, %2.<Vbtype>";
      case 1:
	return aarch64_output_simd_mov_immediate (operands[2], <bitsize>,
						  AARCH64_CHECK_ORR);
      default:
	gcc_unreachable ();
      }
  }
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "xor<mode>3"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (xor:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		 (match_operand:VDQ_I 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "eor\t%0.<Vbtype>, %1.<Vbtype>, %2.<Vbtype>"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (not:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "not\t%0.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "aarch64_simd_vec_set<mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w,w,w")
	(vec_merge:VALL_F16
	    (vec_duplicate:VALL_F16
		(match_operand:<VEL> 1 "aarch64_simd_nonimmediate_operand" "w,?r,Utv"))
	    (match_operand:VALL_F16 3 "register_operand" "0,0,0")
	    (match_operand:SI 2 "immediate_operand" "i,i,i")))]
  "TARGET_SIMD && exact_log2 (INTVAL (operands[2])) >= 0"
  {
   int elt = ENDIAN_LANE_N (<nunits>, exact_log2 (INTVAL (operands[2])));
   operands[2] = GEN_INT ((HOST_WIDE_INT) 1 << elt);
   switch (which_alternative)
     {
     case 0:
	return "ins\\t%0.<Vetype>[%p2], %1.<Vetype>[0]";
     case 1:
	return "ins\\t%0.<Vetype>[%p2], %<vwcore>1";
     case 2:
        return "ld1\\t{%0.<Vetype>}[%p2], %1";
     default:
	gcc_unreachable ();
     }
  }
  [(set_attr "type" "neon_ins<q>, neon_from_gp<q>, neon_load1_one_lane<q>")]
)

(define_insn "@aarch64_simd_vec_copy_lane<mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(vec_merge:VALL_F16
	    (vec_duplicate:VALL_F16
	      (vec_select:<VEL>
		(match_operand:VALL_F16 3 "register_operand" "w")
		(parallel
		  [(match_operand:SI 4 "immediate_operand" "i")])))
	    (match_operand:VALL_F16 1 "register_operand" "0")
	    (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_SIMD && exact_log2 (INTVAL (operands[2])) >= 0"
  {
    int elt = ENDIAN_LANE_N (<nunits>, exact_log2 (INTVAL (operands[2])));
    operands[2] = GEN_INT (HOST_WIDE_INT_1 << elt);
    operands[4] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[4]));

    return "ins\t%0.<Vetype>[%p2], %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_ins<q>")]
)

(define_insn "*aarch64_simd_vec_copy_lane_<vswap_width_name><mode>"
  [(set (match_operand:VALL_F16_NO_V2Q 0 "register_operand" "=w")
	(vec_merge:VALL_F16_NO_V2Q
	    (vec_duplicate:VALL_F16_NO_V2Q
	      (vec_select:<VEL>
		(match_operand:<VSWAP_WIDTH> 3 "register_operand" "w")
		(parallel
		  [(match_operand:SI 4 "immediate_operand" "i")])))
	    (match_operand:VALL_F16_NO_V2Q 1 "register_operand" "0")
	    (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_SIMD && exact_log2 (INTVAL (operands[2])) >= 0"
  {
    int elt = ENDIAN_LANE_N (<nunits>, exact_log2 (INTVAL (operands[2])));
    operands[2] = GEN_INT (HOST_WIDE_INT_1 << elt);
    operands[4] = aarch64_endian_lane_rtx (<VSWAP_WIDTH>mode,
					   INTVAL (operands[4]));

    return "ins\t%0.<Vetype>[%p2], %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_ins<q>")]
)

(define_expand "signbit<mode>2"
  [(use (match_operand:<V_INT_EQUIV> 0 "register_operand"))
   (use (match_operand:VDQSF 1 "register_operand"))]
  "TARGET_SIMD"
{
  int shift_amount = GET_MODE_UNIT_BITSIZE (<V_INT_EQUIV>mode) - 1;
  rtx shift_vector = aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
                                                        shift_amount);
  operands[1] = lowpart_subreg (<V_INT_EQUIV>mode, operands[1], <MODE>mode);

  emit_insn (gen_aarch64_simd_lshr<v_int_equiv> (operands[0], operands[1],
                                                 shift_vector));
  DONE;
})

(define_insn "aarch64_simd_lshr<mode>"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (lshiftrt:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		     (match_operand:VDQ_I  2 "aarch64_simd_rshift_imm" "Dr")))]
 "TARGET_SIMD"
 "ushr\t%0.<Vtype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm<q>")]
)

(define_insn "aarch64_simd_ashr<mode>"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w,w")
       (ashiftrt:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w,w")
		     (match_operand:VDQ_I  2 "aarch64_simd_rshift_imm" "D1,Dr")))]
 "TARGET_SIMD"
 "@
  cmlt\t%0.<Vtype>, %1.<Vtype>, #0
  sshr\t%0.<Vtype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_compare<q>,neon_shift_imm<q>")]
)

(define_insn "*aarch64_simd_sra<mode>"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
	(plus:VDQ_I
	   (SHIFTRT:VDQ_I
		(match_operand:VDQ_I 1 "register_operand" "w")
		(match_operand:VDQ_I 2 "aarch64_simd_rshift_imm" "Dr"))
	   (match_operand:VDQ_I 3 "register_operand" "0")))]
  "TARGET_SIMD"
  "<sra_op>sra\t%0.<Vtype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_acc<q>")]
)

(define_insn "aarch64_simd_imm_shl<mode>"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (ashift:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		   (match_operand:VDQ_I  2 "aarch64_simd_lshift_imm" "Dl")))]
 "TARGET_SIMD"
  "shl\t%0.<Vtype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm<q>")]
)

(define_insn "aarch64_simd_reg_sshl<mode>"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (ashift:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		   (match_operand:VDQ_I 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "sshl\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_shift_reg<q>")]
)

(define_insn "aarch64_simd_reg_shl<mode>_unsigned"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (unspec:VDQ_I [(match_operand:VDQ_I 1 "register_operand" "w")
		    (match_operand:VDQ_I 2 "register_operand" "w")]
		   UNSPEC_ASHIFT_UNSIGNED))]
 "TARGET_SIMD"
 "ushl\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_shift_reg<q>")]
)

(define_insn "aarch64_simd_reg_shl<mode>_signed"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (unspec:VDQ_I [(match_operand:VDQ_I 1 "register_operand" "w")
		    (match_operand:VDQ_I 2 "register_operand" "w")]
		   UNSPEC_ASHIFT_SIGNED))]
 "TARGET_SIMD"
 "sshl\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_shift_reg<q>")]
)

(define_expand "ashl<mode>3"
  [(match_operand:VDQ_I 0 "register_operand")
   (match_operand:VDQ_I 1 "register_operand")
   (match_operand:SI  2 "general_operand")]
 "TARGET_SIMD"
{
  int bit_width = GET_MODE_UNIT_SIZE (<MODE>mode) * BITS_PER_UNIT;
  int shift_amount;

  if (CONST_INT_P (operands[2]))
    {
      shift_amount = INTVAL (operands[2]);
      if (shift_amount >= 0 && shift_amount < bit_width)
        {
	  rtx tmp = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						       shift_amount);
	  emit_insn (gen_aarch64_simd_imm_shl<mode> (operands[0],
						     operands[1],
						     tmp));
          DONE;
        }
    }

  operands[2] = force_reg (SImode, operands[2]);

  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_aarch64_simd_dup<mode> (tmp, convert_to_mode (<VEL>mode,
							       operands[2],
							       0)));
  emit_insn (gen_aarch64_simd_reg_sshl<mode> (operands[0], operands[1], tmp));
  DONE;
})

(define_expand "lshr<mode>3"
  [(match_operand:VDQ_I 0 "register_operand")
   (match_operand:VDQ_I 1 "register_operand")
   (match_operand:SI  2 "general_operand")]
 "TARGET_SIMD"
{
  int bit_width = GET_MODE_UNIT_SIZE (<MODE>mode) * BITS_PER_UNIT;
  int shift_amount;

  if (CONST_INT_P (operands[2]))
    {
      shift_amount = INTVAL (operands[2]);
      if (shift_amount > 0 && shift_amount <= bit_width)
        {
	  rtx tmp = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						       shift_amount);
          emit_insn (gen_aarch64_simd_lshr<mode> (operands[0],
						  operands[1],
						  tmp));
	  DONE;
	}
    }

  operands[2] = force_reg (SImode, operands[2]);

  rtx tmp = gen_reg_rtx (SImode);
  rtx tmp1 = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_negsi2 (tmp, operands[2]));
  emit_insn (gen_aarch64_simd_dup<mode> (tmp1,
					 convert_to_mode (<VEL>mode, tmp, 0)));
  emit_insn (gen_aarch64_simd_reg_shl<mode>_unsigned (operands[0], operands[1],
						      tmp1));
  DONE;
})

(define_expand "ashr<mode>3"
  [(match_operand:VDQ_I 0 "register_operand")
   (match_operand:VDQ_I 1 "register_operand")
   (match_operand:SI  2 "general_operand")]
 "TARGET_SIMD"
{
  int bit_width = GET_MODE_UNIT_SIZE (<MODE>mode) * BITS_PER_UNIT;
  int shift_amount;

  if (CONST_INT_P (operands[2]))
    {
      shift_amount = INTVAL (operands[2]);
      if (shift_amount > 0 && shift_amount <= bit_width)
        {
	  rtx tmp = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						       shift_amount);
          emit_insn (gen_aarch64_simd_ashr<mode> (operands[0],
						  operands[1],
						  tmp));
          DONE;
	}
    }

  operands[2] = force_reg (SImode, operands[2]);

  rtx tmp = gen_reg_rtx (SImode);
  rtx tmp1 = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_negsi2 (tmp, operands[2]));
  emit_insn (gen_aarch64_simd_dup<mode> (tmp1, convert_to_mode (<VEL>mode,
								tmp, 0)));
  emit_insn (gen_aarch64_simd_reg_shl<mode>_signed (operands[0], operands[1],
						    tmp1));
  DONE;
})

(define_expand "vashl<mode>3"
 [(match_operand:VDQ_I 0 "register_operand")
  (match_operand:VDQ_I 1 "register_operand")
  (match_operand:VDQ_I 2 "register_operand")]
 "TARGET_SIMD"
{
  emit_insn (gen_aarch64_simd_reg_sshl<mode> (operands[0], operands[1],
					      operands[2]));
  DONE;
})

(define_expand "vashr<mode>3"
 [(match_operand:VDQ_I 0 "register_operand")
  (match_operand:VDQ_I 1 "register_operand")
  (match_operand:VDQ_I 2 "register_operand")]
 "TARGET_SIMD"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  emit (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_aarch64_simd_reg_shl<mode>_signed (operands[0], operands[1],
						    neg));
  DONE;
})

;; DI vector shift
(define_expand "aarch64_ashr_simddi"
  [(match_operand:DI 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand:SI 2 "aarch64_shift_imm64_di")]
  "TARGET_SIMD"
  {
    /* An arithmetic shift right by 64 fills the result with copies of the sign
       bit, just like asr by 63 - however the standard pattern does not handle
       a shift by 64.  */
    if (INTVAL (operands[2]) == 64)
      operands[2] = GEN_INT (63);
    emit_insn (gen_ashrdi3 (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_expand "vlshr<mode>3"
 [(match_operand:VDQ_I 0 "register_operand")
  (match_operand:VDQ_I 1 "register_operand")
  (match_operand:VDQ_I 2 "register_operand")]
 "TARGET_SIMD"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  emit (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_aarch64_simd_reg_shl<mode>_unsigned (operands[0], operands[1],
						      neg));
  DONE;
})

(define_expand "aarch64_lshr_simddi"
  [(match_operand:DI 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand:SI 2 "aarch64_shift_imm64_di")]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[2]) == 64)
      emit_move_insn (operands[0], const0_rtx);
    else
      emit_insn (gen_lshrdi3 (operands[0], operands[1], operands[2]));
    DONE;
  }
)

;; For 64-bit modes we use ushl/r, as this does not require a SIMD zero.
(define_insn "vec_shr_<mode>"
  [(set (match_operand:VD 0 "register_operand" "=w")
        (unspec:VD [(match_operand:VD 1 "register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
		   UNSPEC_VEC_SHR))]
  "TARGET_SIMD"
  {
    if (BYTES_BIG_ENDIAN)
      return "shl %d0, %d1, %2";
    else
      return "ushr %d0, %d1, %2";
  }
  [(set_attr "type" "neon_shift_imm")]
)

(define_expand "vec_set<mode>"
  [(match_operand:VALL_F16 0 "register_operand")
   (match_operand:<VEL> 1 "aarch64_simd_nonimmediate_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_SIMD"
  {
    HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);
    emit_insn (gen_aarch64_simd_vec_set<mode> (operands[0], operands[1],
					  GEN_INT (elem), operands[0]));
    DONE;
  }
)


(define_insn "aarch64_mla<mode>"
 [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
       (plus:VDQ_BHSI (mult:VDQ_BHSI
			(match_operand:VDQ_BHSI 2 "register_operand" "w")
			(match_operand:VDQ_BHSI 3 "register_operand" "w"))
		      (match_operand:VDQ_BHSI 1 "register_operand" "0")))]
 "TARGET_SIMD"
 "mla\t%0.<Vtype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_mla_<Vetype><q>")]
)

(define_insn "*aarch64_mla_elt<mode>"
 [(set (match_operand:VDQHS 0 "register_operand" "=w")
       (plus:VDQHS
	 (mult:VDQHS
	   (vec_duplicate:VDQHS
	      (vec_select:<VEL>
		(match_operand:VDQHS 1 "register_operand" "<h_con>")
		  (parallel [(match_operand:SI 2 "immediate_operand")])))
	   (match_operand:VDQHS 3 "register_operand" "w"))
	 (match_operand:VDQHS 4 "register_operand" "0")))]
 "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[2]));
    return "mla\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_mla_elt_<vswap_width_name><mode>"
 [(set (match_operand:VDQHS 0 "register_operand" "=w")
       (plus:VDQHS
	 (mult:VDQHS
	   (vec_duplicate:VDQHS
	      (vec_select:<VEL>
		(match_operand:<VSWAP_WIDTH> 1 "register_operand" "<h_con>")
		  (parallel [(match_operand:SI 2 "immediate_operand")])))
	   (match_operand:VDQHS 3 "register_operand" "w"))
	 (match_operand:VDQHS 4 "register_operand" "0")))]
 "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSWAP_WIDTH>mode, INTVAL (operands[2]));
    return "mla\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar<q>")]
)

(define_insn "aarch64_mla_n<mode>"
 [(set (match_operand:VDQHS 0 "register_operand" "=w")
	(plus:VDQHS
	  (mult:VDQHS
	    (vec_duplicate:VDQHS
	      (match_operand:<VEL> 3 "register_operand" "<h_con>"))
	    (match_operand:VDQHS 2 "register_operand" "w"))
	  (match_operand:VDQHS 1 "register_operand" "0")))]
 "TARGET_SIMD"
 "mla\t%0.<Vtype>, %2.<Vtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_mla_<Vetype>_scalar<q>")]
)

(define_insn "aarch64_mls<mode>"
 [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
       (minus:VDQ_BHSI (match_operand:VDQ_BHSI 1 "register_operand" "0")
		   (mult:VDQ_BHSI (match_operand:VDQ_BHSI 2 "register_operand" "w")
			      (match_operand:VDQ_BHSI 3 "register_operand" "w"))))]
 "TARGET_SIMD"
 "mls\t%0.<Vtype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_mla_<Vetype><q>")]
)

(define_insn "*aarch64_mls_elt<mode>"
 [(set (match_operand:VDQHS 0 "register_operand" "=w")
       (minus:VDQHS
	 (match_operand:VDQHS 4 "register_operand" "0")
	 (mult:VDQHS
	   (vec_duplicate:VDQHS
	      (vec_select:<VEL>
		(match_operand:VDQHS 1 "register_operand" "<h_con>")
		  (parallel [(match_operand:SI 2 "immediate_operand")])))
	   (match_operand:VDQHS 3 "register_operand" "w"))))]
 "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[2]));
    return "mls\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_mls_elt_<vswap_width_name><mode>"
 [(set (match_operand:VDQHS 0 "register_operand" "=w")
       (minus:VDQHS
	 (match_operand:VDQHS 4 "register_operand" "0")
	 (mult:VDQHS
	   (vec_duplicate:VDQHS
	      (vec_select:<VEL>
		(match_operand:<VSWAP_WIDTH> 1 "register_operand" "<h_con>")
		  (parallel [(match_operand:SI 2 "immediate_operand")])))
	   (match_operand:VDQHS 3 "register_operand" "w"))))]
 "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSWAP_WIDTH>mode, INTVAL (operands[2]));
    return "mls\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar<q>")]
)

(define_insn "aarch64_mls_n<mode>"
  [(set (match_operand:VDQHS 0 "register_operand" "=w")
	(minus:VDQHS
	  (match_operand:VDQHS 1 "register_operand" "0")
	  (mult:VDQHS
	    (vec_duplicate:VDQHS
	      (match_operand:<VEL> 3 "register_operand" "<h_con>"))
	    (match_operand:VDQHS 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "mls\t%0.<Vtype>, %2.<Vtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_mla_<Vetype>_scalar<q>")]
)

;; Max/Min operations.
(define_insn "<su><maxmin><mode>3"
 [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
       (MAXMIN:VDQ_BHSI (match_operand:VDQ_BHSI 1 "register_operand" "w")
		    (match_operand:VDQ_BHSI 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "<su><maxmin>\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_minmax<q>")]
)

(define_expand "<su><maxmin>v2di3"
 [(set (match_operand:V2DI 0 "register_operand")
       (MAXMIN:V2DI (match_operand:V2DI 1 "register_operand")
                    (match_operand:V2DI 2 "register_operand")))]
 "TARGET_SIMD"
{
  enum rtx_code cmp_operator;
  rtx cmp_fmt;

  switch (<CODE>)
    {
    case UMIN:
      cmp_operator = LTU;
      break;
    case SMIN:
      cmp_operator = LT;
      break;
    case UMAX:
      cmp_operator = GTU;
      break;
    case SMAX:
      cmp_operator = GT;
      break;
    default:
      gcc_unreachable ();
    }

  cmp_fmt = gen_rtx_fmt_ee (cmp_operator, V2DImode, operands[1], operands[2]);
  emit_insn (gen_vcondv2div2di (operands[0], operands[1],
              operands[2], cmp_fmt, operands[1], operands[2]));
  DONE;
})

;; Pairwise Integer Max/Min operations.
(define_insn "aarch64_<optab>p<mode>"
 [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
       (unspec:VDQ_BHSI [(match_operand:VDQ_BHSI 1 "register_operand" "w")
			 (match_operand:VDQ_BHSI 2 "register_operand" "w")]
			MAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op>p\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_minmax<q>")]
)

;; Pairwise FP Max/Min operations.
(define_insn "aarch64_<optab>p<mode>"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")
		      (match_operand:VHSDF 2 "register_operand" "w")]
		      FMAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op>p\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_minmax<q>")]
)

;; vec_concat gives a new vector with the low elements from operand 1, and
;; the high elements from operand 2.  That is to say, given op1 = { a, b }
;; op2 = { c, d }, vec_concat (op1, op2) = { a, b, c, d }.
;; What that means, is that the RTL descriptions of the below patterns
;; need to change depending on endianness.

;; Narrowing operations.

(define_insn "aarch64_xtn<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (truncate:<VNARROWQ> (match_operand:VQN 1 "register_operand" "w"))
	  (match_operand:<VNARROWQ> 2 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "xtn\\t%0.<Vntype>, %1.<Vtype>"
  [(set_attr "type" "neon_move_narrow_q")]
)

(define_insn "aarch64_xtn<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 2 "aarch64_simd_or_scalar_imm_zero")
	  (truncate:<VNARROWQ> (match_operand:VQN 1 "register_operand" "w"))))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "xtn\\t%0.<Vntype>, %1.<Vtype>"
  [(set_attr "type" "neon_move_narrow_q")]
)

(define_expand "aarch64_xtn<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand")
	(truncate:<VNARROWQ> (match_operand:VQN 1 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (<VNARROWQ2>mode);
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_xtn<mode>_insn_be (tmp, operands[1],
				CONST0_RTX (<VNARROWQ>mode)));
    else
      emit_insn (gen_aarch64_xtn<mode>_insn_le (tmp, operands[1],
				CONST0_RTX (<VNARROWQ>mode)));

    /* The intrinsic expects a narrow result, so emit a subreg that will get
       optimized away as appropriate.  */
    emit_move_insn (operands[0], lowpart_subreg (<VNARROWQ>mode, tmp,
						 <VNARROWQ2>mode));
    DONE;
  }
)

(define_insn "aarch64_xtn2<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (truncate:<VNARROWQ> (match_operand:VQN 2 "register_operand" "w"))))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "xtn2\t%0.<V2ntype>, %2.<Vtype>"
  [(set_attr "type" "neon_move_narrow_q")]
)

(define_insn "aarch64_xtn2<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (truncate:<VNARROWQ> (match_operand:VQN 2 "register_operand" "w"))
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "xtn2\t%0.<V2ntype>, %2.<Vtype>"
  [(set_attr "type" "neon_move_narrow_q")]
)

(define_expand "aarch64_xtn2<mode>"
  [(match_operand:<VNARROWQ2> 0 "register_operand")
   (match_operand:<VNARROWQ> 1 "register_operand")
   (truncate:<VNARROWQ> (match_operand:VQN 2 "register_operand"))]
  "TARGET_SIMD"
  {
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_xtn2<mode>_insn_be (operands[0], operands[1],
						 operands[2]));
    else
      emit_insn (gen_aarch64_xtn2<mode>_insn_le (operands[0], operands[1],
						 operands[2]));
    DONE;
  }
)

(define_insn "*aarch64_narrow_trunc<mode>"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
          (truncate:<VNARROWQ>
            (match_operand:VQN 1 "register_operand" "w"))
	  (truncate:<VNARROWQ>
	    (match_operand:VQN 2 "register_operand" "w"))))]
  "TARGET_SIMD"
{
  if (!BYTES_BIG_ENDIAN)
    return "uzp1\\t%0.<V2ntype>, %1.<V2ntype>, %2.<V2ntype>";
  else
    return "uzp1\\t%0.<V2ntype>, %2.<V2ntype>, %1.<V2ntype>";
}
  [(set_attr "type" "neon_permute<q>")]
)

;; Packing doubles.

(define_expand "vec_pack_trunc_<mode>"
 [(match_operand:<VNARROWD> 0 "register_operand")
  (match_operand:VDN 1 "general_operand")
  (match_operand:VDN 2 "general_operand")]
 "TARGET_SIMD"
{
  rtx tempreg = gen_reg_rtx (<VDBL>mode);
  emit_insn (gen_aarch64_vec_concat<mode> (tempreg, operands[1], operands[2]));
  emit_insn (gen_trunc<Vdbl><Vnarrowd>2 (operands[0], tempreg));
  DONE;
})

;; Packing quads.

(define_expand "vec_pack_trunc_<mode>"
 [(set (match_operand:<VNARROWQ2> 0 "register_operand")
       (vec_concat:<VNARROWQ2>
	 (truncate:<VNARROWQ> (match_operand:VQN 1 "register_operand"))
	 (truncate:<VNARROWQ> (match_operand:VQN 2 "register_operand"))))]
 "TARGET_SIMD"
 {
   rtx tmpreg = gen_reg_rtx (<VNARROWQ>mode);
   int lo = BYTES_BIG_ENDIAN ? 2 : 1;
   int hi = BYTES_BIG_ENDIAN ? 1 : 2;

   emit_insn (gen_trunc<mode><Vnarrowq>2 (tmpreg, operands[lo]));

   if (BYTES_BIG_ENDIAN)
     emit_insn (gen_aarch64_xtn2<mode>_insn_be (operands[0], tmpreg,
						operands[hi]));
   else
     emit_insn (gen_aarch64_xtn2<mode>_insn_le (operands[0], tmpreg,
						operands[hi]));
   DONE;
 }
)

(define_insn "aarch64_shrn<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (truncate:<VNARROWQ>
	    (lshiftrt:VQN (match_operand:VQN 1 "register_operand" "w")
	      (match_operand:VQN 2 "aarch64_simd_shift_imm_vec_<vn_mode>")))
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "shrn\\t%0.<Vntype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "aarch64_shrn<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")
	  (truncate:<VNARROWQ>
	    (lshiftrt:VQN (match_operand:VQN 1 "register_operand" "w")
	      (match_operand:VQN 2 "aarch64_simd_shift_imm_vec_<vn_mode>")))))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "shrn\\t%0.<Vntype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "*aarch64_<srn_op>shrn<mode>_vect"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
        (truncate:<VNARROWQ>
          (SHIFTRT:VQN (match_operand:VQN 1 "register_operand" "w")
            (match_operand:VQN 2 "aarch64_simd_shift_imm_vec_<vn_mode>"))))]
  "TARGET_SIMD"
  "shrn\\t%0.<Vntype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "*aarch64_<srn_op>shrn<mode>2_vect_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (truncate:<VNARROWQ>
	    (SHIFTRT:VQN (match_operand:VQN 2 "register_operand" "w")
	      (match_operand:VQN 3 "aarch64_simd_shift_imm_vec_<vn_mode>")))))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "shrn2\\t%0.<V2ntype>, %2.<Vtype>, %3"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "*aarch64_<srn_op>shrn<mode>2_vect_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (truncate:<VNARROWQ>
	    (SHIFTRT:VQN (match_operand:VQN 2 "register_operand" "w")
	      (match_operand:VQN 3 "aarch64_simd_shift_imm_vec_<vn_mode>")))
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "shrn2\\t%0.<V2ntype>, %2.<Vtype>, %3"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "*aarch64_<srn_op>topbits_shuffle<mode>_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
          (truncate:<VNARROWQ>
            (SHIFTRT:VQN (match_operand:VQN 1 "register_operand" "w")
	      (match_operand:VQN 2 "aarch64_simd_shift_imm_vec_exact_top")))
	  (truncate:<VNARROWQ>
	    (SHIFTRT:VQN (match_operand:VQN 3 "register_operand" "w")
	      (match_dup 2)))))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "uzp2\\t%0.<V2ntype>, %1.<V2ntype>, %3.<V2ntype>"
  [(set_attr "type" "neon_permute<q>")]
)

(define_insn "*aarch64_<srn_op>topbits_shuffle<mode>_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (truncate:<VNARROWQ>
	    (SHIFTRT:VQN (match_operand:VQN 3 "register_operand" "w")
	      (match_operand:VQN 2 "aarch64_simd_shift_imm_vec_exact_top")))
          (truncate:<VNARROWQ>
            (SHIFTRT:VQN (match_operand:VQN 1 "register_operand" "w")
	      (match_dup 2)))))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "uzp2\\t%0.<V2ntype>, %1.<V2ntype>, %3.<V2ntype>"
  [(set_attr "type" "neon_permute<q>")]
)

(define_expand "aarch64_shrn<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand")
	(truncate:<VNARROWQ>
	  (lshiftrt:VQN (match_operand:VQN 1 "register_operand")
	    (match_operand:SI 2 "aarch64_simd_shift_imm_offset_<vn_mode>"))))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						 INTVAL (operands[2]));
    rtx tmp = gen_reg_rtx (<VNARROWQ2>mode);
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_shrn<mode>_insn_be (tmp, operands[1],
				operands[2], CONST0_RTX (<VNARROWQ>mode)));
    else
      emit_insn (gen_aarch64_shrn<mode>_insn_le (tmp, operands[1],
				operands[2], CONST0_RTX (<VNARROWQ>mode)));

    /* The intrinsic expects a narrow result, so emit a subreg that will get
       optimized away as appropriate.  */
    emit_move_insn (operands[0], lowpart_subreg (<VNARROWQ>mode, tmp,
						 <VNARROWQ2>mode));
    DONE;
  }
)

(define_insn "aarch64_rshrn<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand" "w")
		(match_operand:VQN 2
		  "aarch64_simd_shift_imm_vec_<vn_mode>")] UNSPEC_RSHRN)
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "rshrn\\t%0.<Vntype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "aarch64_rshrn<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")
	  (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand" "w")
		(match_operand:VQN 2 "aarch64_simd_shift_imm_vec_<vn_mode>")]
		  UNSPEC_RSHRN)))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "rshrn\\t%0.<Vntype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_expand "aarch64_rshrn<mode>"
  [(match_operand:<VNARROWQ> 0 "register_operand")
   (match_operand:VQN 1 "register_operand")
   (match_operand:SI 2 "aarch64_simd_shift_imm_offset_<vn_mode>")]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[2]) == GET_MODE_UNIT_BITSIZE (<VNARROWQ>mode))
      {
	rtx tmp0 = aarch64_gen_shareable_zero (<MODE>mode);
	emit_insn (gen_aarch64_raddhn<mode> (operands[0], operands[1], tmp0));
      }
    else
      {
	rtx tmp = gen_reg_rtx (<VNARROWQ2>mode);
	operands[2] = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						         INTVAL (operands[2]));
	if (BYTES_BIG_ENDIAN)
	  emit_insn (
		gen_aarch64_rshrn<mode>_insn_be (tmp, operands[1],
						 operands[2],
						 CONST0_RTX (<VNARROWQ>mode)));
	else
	  emit_insn (
		gen_aarch64_rshrn<mode>_insn_le (tmp, operands[1],
						 operands[2],
						 CONST0_RTX (<VNARROWQ>mode)));

	/* The intrinsic expects a narrow result, so emit a subreg that will
	   get optimized away as appropriate.  */
	emit_move_insn (operands[0], lowpart_subreg (<VNARROWQ>mode, tmp,
						     <VNARROWQ2>mode));
      }
    DONE;
  }
)

(define_insn "aarch64_shrn2<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (truncate:<VNARROWQ>
	    (lshiftrt:VQN (match_operand:VQN 2 "register_operand" "w")
	      (match_operand:VQN 3 "aarch64_simd_shift_imm_vec_<vn_mode>")))))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "shrn2\\t%0.<V2ntype>, %2.<Vtype>, %3"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "aarch64_shrn2<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (truncate:<VNARROWQ>
	    (lshiftrt:VQN (match_operand:VQN 2 "register_operand" "w")
	      (match_operand:VQN 3
		"aarch64_simd_shift_imm_vec_<vn_mode>")))
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "shrn2\\t%0.<V2ntype>, %2.<Vtype>, %3"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_expand "aarch64_shrn2<mode>"
  [(match_operand:<VNARROWQ2> 0 "register_operand")
   (match_operand:<VNARROWQ> 1 "register_operand")
   (match_operand:VQN 2 "register_operand")
   (match_operand:SI 3 "aarch64_simd_shift_imm_offset_<vn_mode>")]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						 INTVAL (operands[3]));
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_shrn2<mode>_insn_be (operands[0], operands[1],
						  operands[2], operands[3]));
    else
      emit_insn (gen_aarch64_shrn2<mode>_insn_le (operands[0], operands[1],
						  operands[2], operands[3]));
    DONE;
  }
)

(define_insn "aarch64_rshrn2<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (unspec:<VNARROWQ> [(match_operand:VQN 2 "register_operand" "w")
	    (match_operand:VQN 3 "aarch64_simd_shift_imm_vec_<vn_mode>")]
		UNSPEC_RSHRN)))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "rshrn2\\t%0.<V2ntype>, %2.<Vtype>, %3"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "aarch64_rshrn2<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (unspec:<VNARROWQ> [(match_operand:VQN 2 "register_operand" "w")
		(match_operand:VQN 3 "aarch64_simd_shift_imm_vec_<vn_mode>")]
		  UNSPEC_RSHRN)
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "rshrn2\\t%0.<V2ntype>, %2.<Vtype>, %3"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_expand "aarch64_rshrn2<mode>"
  [(match_operand:<VNARROWQ2> 0 "register_operand")
   (match_operand:<VNARROWQ> 1 "register_operand")
   (match_operand:VQN 2 "register_operand")
   (match_operand:SI 3 "aarch64_simd_shift_imm_offset_<vn_mode>")]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[3]) == GET_MODE_UNIT_BITSIZE (<VNARROWQ2>mode))
      {
	rtx tmp = aarch64_gen_shareable_zero (<MODE>mode);
	emit_insn (gen_aarch64_raddhn2<mode> (operands[0], operands[1],
					      operands[2], tmp));
      }
    else
      {
	operands[3] = aarch64_simd_gen_const_vector_dup (<MODE>mode,
							 INTVAL (operands[3]));
	if (BYTES_BIG_ENDIAN)
	  emit_insn (gen_aarch64_rshrn2<mode>_insn_be (operands[0],
						       operands[1],
						       operands[2],
						       operands[3]));
	else
	  emit_insn (gen_aarch64_rshrn2<mode>_insn_le (operands[0],
						       operands[1],
						       operands[2],
						       operands[3]));
      }
    DONE;
  }
)

;; Widening operations.

(define_insn "aarch64_simd_vec_unpack<su>_lo_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			       (match_operand:VQW 1 "register_operand" "w")
			       (match_operand:VQW 2 "vect_par_cnst_lo_half" "")
			    )))]
  "TARGET_SIMD"
  "<su>xtl\t%0.<Vwtype>, %1.<Vhalftype>"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "aarch64_simd_vec_unpack<su>_hi_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			       (match_operand:VQW 1 "register_operand" "w")
			       (match_operand:VQW 2 "vect_par_cnst_hi_half" "")
			    )))]
  "TARGET_SIMD"
  "<su>xtl2\t%0.<Vwtype>, %1.<Vtype>"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_expand "vec_unpack<su>_hi_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
    emit_insn (gen_aarch64_simd_vec_unpack<su>_hi_<mode> (operands[0],
							  operands[1], p));
    DONE;
  }
)

(define_expand "vec_unpack<su>_lo_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
    emit_insn (gen_aarch64_simd_vec_unpack<su>_lo_<mode> (operands[0],
							  operands[1], p));
    DONE;
  }
)

;; Widening arithmetic.

(define_insn "*aarch64_<su>mlal_lo<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (plus:<VWIDE>
          (mult:<VWIDE>
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 2 "register_operand" "w")
                 (match_operand:VQW 3 "vect_par_cnst_lo_half" "")))
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 4 "register_operand" "w")
                 (match_dup 3))))
          (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "<su>mlal\t%0.<Vwtype>, %2.<Vhalftype>, %4.<Vhalftype>"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_insn "aarch64_<su>mlal_hi<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (plus:<VWIDE>
          (mult:<VWIDE>
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 2 "register_operand" "w")
                 (match_operand:VQW 3 "vect_par_cnst_hi_half" "")))
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 4 "register_operand" "w")
                 (match_dup 3))))
          (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "<su>mlal2\t%0.<Vwtype>, %2.<Vtype>, %4.<Vtype>"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_expand "aarch64_<su>mlal_hi<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQW 2 "register_operand"))
   (match_operand:VQW 3 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlal_hi<mode>_insn (operands[0], operands[1],
						 operands[2], p, operands[3]));
  DONE;
}
)

(define_insn "aarch64_<su>mlal_hi_n<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (vec_select:<VHALF>
		(match_operand:VQ_HSI 2 "register_operand" "w")
		(match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(match_operand:<VEL> 4 "register_operand" "<h_con>"))))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "<su>mlal2\t%0.<Vwtype>, %2.<Vtype>, %4.<Vetype>[0]"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_expand "aarch64_<su>mlal_hi_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 2 "register_operand"))
   (match_operand:<VEL> 3 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlal_hi_n<mode>_insn (operands[0],
             operands[1], operands[2], p, operands[3]));
  DONE;
}
)

(define_insn "*aarch64_<su>mlsl_lo<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (minus:<VWIDE>
          (match_operand:<VWIDE> 1 "register_operand" "0")
          (mult:<VWIDE>
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 2 "register_operand" "w")
                 (match_operand:VQW 3 "vect_par_cnst_lo_half" "")))
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 4 "register_operand" "w")
                 (match_dup 3))))))]
  "TARGET_SIMD"
  "<su>mlsl\t%0.<Vwtype>, %2.<Vhalftype>, %4.<Vhalftype>"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_insn "aarch64_<su>mlsl_hi<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (minus:<VWIDE>
          (match_operand:<VWIDE> 1 "register_operand" "0")
          (mult:<VWIDE>
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 2 "register_operand" "w")
                 (match_operand:VQW 3 "vect_par_cnst_hi_half" "")))
              (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                 (match_operand:VQW 4 "register_operand" "w")
                 (match_dup 3))))))]
  "TARGET_SIMD"
  "<su>mlsl2\t%0.<Vwtype>, %2.<Vtype>, %4.<Vtype>"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_expand "aarch64_<su>mlsl_hi<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQW 2 "register_operand"))
   (match_operand:VQW 3 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlsl_hi<mode>_insn (operands[0], operands[1],
						 operands[2], p, operands[3]));
  DONE;
}
)

(define_insn "aarch64_<su>mlsl_hi_n<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (vec_select:<VHALF>
		(match_operand:VQ_HSI 2 "register_operand" "w")
		(match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(match_operand:<VEL> 4 "register_operand" "<h_con>"))))))]
  "TARGET_SIMD"
  "<su>mlsl2\t%0.<Vwtype>, %2.<Vtype>, %4.<Vetype>[0]"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_expand "aarch64_<su>mlsl_hi_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 2 "register_operand"))
   (match_operand:<VEL> 3 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlsl_hi_n<mode>_insn (operands[0],
             operands[1], operands[2], p, operands[3]));
  DONE;
}
)

(define_insn "aarch64_<su>mlal<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (plus:<VWIDE>
          (mult:<VWIDE>
            (ANY_EXTEND:<VWIDE>
              (match_operand:VD_BHSI 2 "register_operand" "w"))
            (ANY_EXTEND:<VWIDE>
              (match_operand:VD_BHSI 3 "register_operand" "w")))
          (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "<su>mlal\t%0.<Vwtype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_insn "aarch64_<su>mlal_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (match_operand:VD_HSI 2 "register_operand" "w"))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(match_operand:<VEL> 3 "register_operand" "<h_con>"))))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "<su>mlal\t%0.<Vwtype>, %2.<Vtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_insn "aarch64_<su>mlsl<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (minus:<VWIDE>
          (match_operand:<VWIDE> 1 "register_operand" "0")
          (mult:<VWIDE>
            (ANY_EXTEND:<VWIDE>
              (match_operand:VD_BHSI 2 "register_operand" "w"))
            (ANY_EXTEND:<VWIDE>
              (match_operand:VD_BHSI 3 "register_operand" "w")))))]
  "TARGET_SIMD"
  "<su>mlsl\t%0.<Vwtype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_insn "aarch64_<su>mlsl_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (match_operand:VD_HSI 2 "register_operand" "w"))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(match_operand:<VEL> 3 "register_operand" "<h_con>"))))))]
  "TARGET_SIMD"
  "<su>mlsl\t%0.<Vwtype>, %2.<Vtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_insn "aarch64_simd_vec_<su>mult_lo_<mode>"
 [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
       (mult:<VWIDE> (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			   (match_operand:VQW 1 "register_operand" "w")
                           (match_operand:VQW 3 "vect_par_cnst_lo_half" "")))
		     (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                           (match_operand:VQW 2 "register_operand" "w")
                           (match_dup 3)))))]
  "TARGET_SIMD"
  "<su>mull\\t%0.<Vwtype>, %1.<Vhalftype>, %2.<Vhalftype>"
  [(set_attr "type" "neon_mul_<Vetype>_long")]
)

(define_insn "aarch64_intrinsic_vec_<su>mult_lo_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(mult:<VWIDE> (ANY_EXTEND:<VWIDE>
			 (match_operand:VD_BHSI 1 "register_operand" "w"))
		      (ANY_EXTEND:<VWIDE>
			 (match_operand:VD_BHSI 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "<su>mull\\t%0.<Vwtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_mul_<Vetype>_long")]
)

(define_expand "vec_widen_<su>mult_lo_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand"))]
 "TARGET_SIMD"
 {
   rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
   emit_insn (gen_aarch64_simd_vec_<su>mult_lo_<mode> (operands[0],
						       operands[1],
						       operands[2], p));
   DONE;
 }
)

(define_insn "aarch64_simd_vec_<su>mult_hi_<mode>"
 [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
      (mult:<VWIDE> (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			    (match_operand:VQW 1 "register_operand" "w")
			    (match_operand:VQW 3 "vect_par_cnst_hi_half" "")))
		    (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			    (match_operand:VQW 2 "register_operand" "w")
			    (match_dup 3)))))]
  "TARGET_SIMD"
  "<su>mull2\\t%0.<Vwtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_mul_<Vetype>_long")]
)

(define_expand "vec_widen_<su>mult_hi_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand"))]
 "TARGET_SIMD"
 {
   rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
   emit_insn (gen_aarch64_simd_vec_<su>mult_hi_<mode> (operands[0],
						       operands[1],
						       operands[2], p));
   DONE;

 }
)

;; vmull_lane_s16 intrinsics
(define_insn "aarch64_vec_<su>mult_lane<Qlane>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(mult:<VWIDE>
	  (ANY_EXTEND:<VWIDE>
	    (match_operand:<VCOND> 1 "register_operand" "w"))
	  (vec_duplicate:<VWIDE>
	    (ANY_EXTEND:<VWIDE_S>
	      (vec_select:<VEL>
		(match_operand:VDQHS 2 "register_operand" "<vwx>")
		(parallel [(match_operand:SI 3 "immediate_operand" "i")]))))))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[3]));
    return "<su>mull\\t%0.<Vwtype>, %1.<Vcondtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_mul_<Vetype>_scalar_long")]
)

(define_insn "aarch64_<su>mull_hi_lane<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(mult:<VWIDE>
	  (ANY_EXTEND:<VWIDE>
	    (vec_select:<VHALF>
	      (match_operand:VQ_HSI 1 "register_operand" "w")
	      (match_operand:VQ_HSI 2 "vect_par_cnst_hi_half" "")))
	  (vec_duplicate:<VWIDE>
	    (ANY_EXTEND:<VWIDE_S>
	      (vec_select:<VEL>
		(match_operand:<VCOND> 3 "register_operand" "<vwx>")
		(parallel [(match_operand:SI 4 "immediate_operand" "i")]))))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
    return "<su>mull2\\t%0.<Vwtype>, %1.<Vtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_<su>mull_hi_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 1 "register_operand"))
   (match_operand:<VCOND> 2 "register_operand")
   (match_operand:SI 3 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mull_hi_lane<mode>_insn (operands[0],
	     operands[1], p, operands[2], operands[3]));
  DONE;
}
)

(define_insn "aarch64_<su>mull_hi_laneq<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(mult:<VWIDE>
	  (ANY_EXTEND:<VWIDE>
	    (vec_select:<VHALF>
	      (match_operand:VQ_HSI 1 "register_operand" "w")
	      (match_operand:VQ_HSI 2 "vect_par_cnst_hi_half" "")))
	  (vec_duplicate:<VWIDE>
	    (ANY_EXTEND:<VWIDE_S>
	      (vec_select:<VEL>
		(match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		(parallel [(match_operand:SI 4 "immediate_operand" "i")]))))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
    return "<su>mull2\\t%0.<Vwtype>, %1.<Vtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_<su>mull_hi_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 1 "register_operand"))
   (match_operand:<VCONQ> 2 "register_operand")
   (match_operand:SI 3 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mull_hi_laneq<mode>_insn (operands[0],
	     operands[1], p, operands[2], operands[3]));
  DONE;
}
)

(define_insn "aarch64_<su>mull_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(mult:<VWIDE>
	  (ANY_EXTEND:<VWIDE>
	    (match_operand:VD_HSI 1 "register_operand" "w"))
	  (vec_duplicate:<VWIDE>
	    (ANY_EXTEND:<VWIDE_S>
	      (match_operand:<VEL> 2 "register_operand" "<h_con>")))))]
  "TARGET_SIMD"
  "<su>mull\t%0.<Vwtype>, %1.<Vtype>, %2.<Vetype>[0]"
  [(set_attr "type" "neon_mul_<Vetype>_scalar_long")]
)

(define_insn "aarch64_<su>mull_hi_n<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(mult:<VWIDE>
	  (ANY_EXTEND:<VWIDE>
	    (vec_select:<VHALF>
	      (match_operand:VQ_HSI 1 "register_operand" "w")
	      (match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	  (vec_duplicate:<VWIDE>
	    (ANY_EXTEND:<VWIDE_S>
	      (match_operand:<VEL> 2 "register_operand" "<h_con>")))))]
  "TARGET_SIMD"
  "<su>mull2\\t%0.<Vwtype>, %1.<Vtype>, %2.<Vetype>[0]"
  [(set_attr "type" "neon_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_<su>mull_hi_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQ_HSI 1 "register_operand"))
   (match_operand:<VEL> 2 "register_operand")]
 "TARGET_SIMD"
 {
   rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
   emit_insn (gen_aarch64_<su>mull_hi_n<mode>_insn (operands[0], operands[1],
						    operands[2], p));
   DONE;
 }
)

;; vmlal_lane_s16 intrinsics
(define_insn "aarch64_vec_<su>mlal_lane<Qlane>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (match_operand:<VCOND> 2 "register_operand" "w"))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(vec_select:<VEL>
		  (match_operand:VDQHS 3 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 4 "immediate_operand" "i")])))))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[4]));
    return "<su>mlal\\t%0.<Vwtype>, %2.<Vcondtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_<su>mlal_hi_lane<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (vec_select:<VHALF>
		(match_operand:VQ_HSI 2 "register_operand" "w")
		(match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(vec_select:<VEL>
		  (match_operand:<VCOND> 4 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 5 "immediate_operand" "i")])))))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[5] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[5]));
    return "<su>mlal2\\t%0.<Vwtype>, %2.<Vtype>, %4.<Vetype>[%5]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_<su>mlal_hi_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 2 "register_operand"))
   (match_operand:<VCOND> 3 "register_operand")
   (match_operand:SI 4 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlal_hi_lane<mode>_insn (operands[0],
	     operands[1], operands[2], p, operands[3], operands[4]));
  DONE;
}
)

(define_insn "aarch64_<su>mlal_hi_laneq<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (vec_select:<VHALF>
		(match_operand:VQ_HSI 2 "register_operand" "w")
		(match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(vec_select:<VEL>
		  (match_operand:<VCONQ> 4 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 5 "immediate_operand" "i")])))))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[5] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[5]));
    return "<su>mlal2\\t%0.<Vwtype>, %2.<Vtype>, %4.<Vetype>[%5]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_<su>mlal_hi_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 2 "register_operand"))
   (match_operand:<VCONQ> 3 "register_operand")
   (match_operand:SI 4 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlal_hi_laneq<mode>_insn (operands[0],
	     operands[1], operands[2], p, operands[3], operands[4]));
  DONE;
}
)

(define_insn "aarch64_vec_<su>mlsl_lane<Qlane>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
   (minus:<VWIDE>
     (match_operand:<VWIDE> 1 "register_operand" "0")
     (mult:<VWIDE>
       (ANY_EXTEND:<VWIDE>
	 (match_operand:<VCOND> 2 "register_operand" "w"))
       (vec_duplicate:<VWIDE>
	 (ANY_EXTEND:<VWIDE_S>
	   (vec_select:<VEL>
	     (match_operand:VDQHS 3 "register_operand" "<vwx>")
	     (parallel [(match_operand:SI 4 "immediate_operand" "i")])))))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[4]));
    return "<su>mlsl\\t%0.<Vwtype>, %2.<Vcondtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_<su>mlsl_hi_lane<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (vec_select:<VHALF>
		(match_operand:VQ_HSI 2 "register_operand" "w")
		(match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(vec_select:<VEL>
		  (match_operand:<VCOND> 4 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 5 "immediate_operand" "i")]))))
	  )))]
  "TARGET_SIMD"
  {
    operands[5] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[5]));
    return "<su>mlsl2\\t%0.<Vwtype>, %2.<Vtype>, %4.<Vetype>[%5]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_<su>mlsl_hi_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 2 "register_operand"))
   (match_operand:<VCOND> 3 "register_operand")
   (match_operand:SI 4 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlsl_hi_lane<mode>_insn (operands[0],
	     operands[1], operands[2], p, operands[3], operands[4]));
  DONE;
}
)

(define_insn "aarch64_<su>mlsl_hi_laneq<mode>_insn"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (mult:<VWIDE>
	    (ANY_EXTEND:<VWIDE>
	      (vec_select:<VHALF>
		(match_operand:VQ_HSI 2 "register_operand" "w")
		(match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	    (vec_duplicate:<VWIDE>
	      (ANY_EXTEND:<VWIDE_S>
		(vec_select:<VEL>
		  (match_operand:<VCONQ> 4 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 5 "immediate_operand" "i")]))))
	  )))]
  "TARGET_SIMD"
  {
    operands[5] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[5]));
    return "<su>mlsl2\\t%0.<Vwtype>, %2.<Vtype>, %4.<Vetype>[%5]";
  }
  [(set_attr "type" "neon_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_<su>mlsl_hi_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (ANY_EXTEND:<VWIDE>(match_operand:VQ_HSI 2 "register_operand"))
   (match_operand:<VCONQ> 3 "register_operand")
   (match_operand:SI 4 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>mlsl_hi_laneq<mode>_insn (operands[0],
	     operands[1], operands[2], p, operands[3], operands[4]));
  DONE;
}
)

;; FP vector operations.
;; AArch64 AdvSIMD supports single-precision (32-bit) and 
;; double-precision (64-bit) floating-point data types and arithmetic as
;; defined by the IEEE 754-2008 standard.  This makes them vectorizable 
;; without the need for -ffast-math or -funsafe-math-optimizations.
;;
;; Floating-point operations can raise an exception.  Vectorizing such
;; operations are safe because of reasons explained below.
;;
;; ARMv8 permits an extension to enable trapped floating-point
;; exception handling, however this is an optional feature.  In the
;; event of a floating-point exception being raised by vectorised
;; code then:
;; 1.  If trapped floating-point exceptions are available, then a trap
;;     will be taken when any lane raises an enabled exception.  A trap
;;     handler may determine which lane raised the exception.
;; 2.  Alternatively a sticky exception flag is set in the
;;     floating-point status register (FPSR).  Software may explicitly
;;     test the exception flags, in which case the tests will either
;;     prevent vectorisation, allowing precise identification of the
;;     failing operation, or if tested outside of vectorisable regions
;;     then the specific operation and lane are not of interest.

;; FP arithmetic operations.

(define_insn "add<mode>3"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (plus:VHSDF (match_operand:VHSDF 1 "register_operand" "w")
		   (match_operand:VHSDF 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "fadd\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_addsub_<stype><q>")]
)

(define_insn "sub<mode>3"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (minus:VHSDF (match_operand:VHSDF 1 "register_operand" "w")
		    (match_operand:VHSDF 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "fsub\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_addsub_<stype><q>")]
)

(define_insn "mul<mode>3"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (mult:VHSDF (match_operand:VHSDF 1 "register_operand" "w")
		   (match_operand:VHSDF 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "fmul\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_mul_<stype><q>")]
)

(define_expand "div<mode>3"
 [(set (match_operand:VHSDF 0 "register_operand")
       (div:VHSDF (match_operand:VHSDF 1 "register_operand")
		  (match_operand:VHSDF 2 "register_operand")))]
 "TARGET_SIMD"
{
  if (aarch64_emit_approx_div (operands[0], operands[1], operands[2]))
    DONE;

  operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn "*div<mode>3"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (div:VHSDF (match_operand:VHSDF 1 "register_operand" "w")
		 (match_operand:VHSDF 2 "register_operand" "w")))]
 "TARGET_SIMD"
 "fdiv\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_div_<stype><q>")]
)

(define_insn "neg<mode>2"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (neg:VHSDF (match_operand:VHSDF 1 "register_operand" "w")))]
 "TARGET_SIMD"
 "fneg\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_neg_<stype><q>")]
)

(define_insn "abs<mode>2"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (abs:VHSDF (match_operand:VHSDF 1 "register_operand" "w")))]
 "TARGET_SIMD"
 "fabs\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_abs_<stype><q>")]
)

(define_expand "aarch64_float_mla<mode>"
  [(set (match_operand:VDQF_DF 0 "register_operand")
	(plus:VDQF_DF
	  (mult:VDQF_DF
	    (match_operand:VDQF_DF 2 "register_operand")
	    (match_operand:VDQF_DF 3 "register_operand"))
	  (match_operand:VDQF_DF 1 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (scratch, operands[2], operands[3]));
    emit_insn (gen_add<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_expand "aarch64_float_mls<mode>"
  [(set (match_operand:VDQF_DF 0 "register_operand")
	(minus:VDQF_DF
	  (match_operand:VDQF_DF 1 "register_operand")
	  (mult:VDQF_DF
	    (match_operand:VDQF_DF 2 "register_operand")
	    (match_operand:VDQF_DF 3 "register_operand"))))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (scratch, operands[2], operands[3]));
    emit_insn (gen_sub<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_expand "aarch64_float_mla_n<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(plus:VDQSF
	  (mult:VDQSF
	    (vec_duplicate:VDQSF
	      (match_operand:<VEL> 3 "register_operand"))
	    (match_operand:VDQSF 2 "register_operand"))
	  (match_operand:VDQSF 1 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul_n<mode>3 (scratch, operands[2], operands[3]));
    emit_insn (gen_add<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_expand "aarch64_float_mls_n<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(minus:VDQSF
	  (match_operand:VDQSF 1 "register_operand")
	  (mult:VDQSF
	    (vec_duplicate:VDQSF
	      (match_operand:<VEL> 3 "register_operand"))
	    (match_operand:VDQSF 2 "register_operand"))))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul_n<mode>3 (scratch, operands[2], operands[3]));
    emit_insn (gen_sub<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_expand "aarch64_float_mla_lane<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(plus:VDQSF
	  (mult:VDQSF
	    (vec_duplicate:VDQSF
	      (vec_select:<VEL>
		(match_operand:V2SF 3 "register_operand")
		(parallel [(match_operand:SI 4 "immediate_operand")])))
	    (match_operand:VDQSF 2 "register_operand"))
	  (match_operand:VDQSF 1 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul_lane<mode>3 (scratch, operands[2],
				    operands[3], operands[4]));
    emit_insn (gen_add<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_expand "aarch64_float_mls_lane<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(minus:VDQSF
	  (match_operand:VDQSF 1 "register_operand")
	  (mult:VDQSF
	    (vec_duplicate:VDQSF
	      (vec_select:<VEL>
		(match_operand:V2SF 3 "register_operand")
		(parallel [(match_operand:SI 4 "immediate_operand")])))
	    (match_operand:VDQSF 2 "register_operand"))))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul_lane<mode>3 (scratch, operands[2],
				    operands[3], operands[4]));
    emit_insn (gen_sub<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_expand "aarch64_float_mla_laneq<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(plus:VDQSF
	  (mult:VDQSF
	    (vec_duplicate:VDQSF
	      (vec_select:<VEL>
		(match_operand:V4SF 3 "register_operand")
		(parallel [(match_operand:SI 4 "immediate_operand")])))
	    (match_operand:VDQSF 2 "register_operand"))
	  (match_operand:VDQSF 1 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul_laneq<mode>3 (scratch, operands[2],
				     operands[3], operands[4]));
    emit_insn (gen_add<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_expand "aarch64_float_mls_laneq<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(minus:VDQSF
	  (match_operand:VDQSF 1 "register_operand")
	  (mult:VDQSF
	    (vec_duplicate:VDQSF
	      (vec_select:<VEL>
		(match_operand:V4SF 3 "register_operand")
		(parallel [(match_operand:SI 4 "immediate_operand")])))
	    (match_operand:VDQSF 2 "register_operand"))))]
  "TARGET_SIMD"
  {
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul_laneq<mode>3 (scratch, operands[2],
				     operands[3], operands[4]));
    emit_insn (gen_sub<mode>3 (operands[0], operands[1], scratch));
    DONE;
  }
)

(define_insn "fma<mode>4"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (fma:VHSDF (match_operand:VHSDF 1 "register_operand" "w")
		  (match_operand:VHSDF 2 "register_operand" "w")
		  (match_operand:VHSDF 3 "register_operand" "0")))]
  "TARGET_SIMD"
 "fmla\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_mla_<stype><q>")]
)

(define_insn "*aarch64_fma4_elt<mode>"
  [(set (match_operand:VDQF 0 "register_operand" "=w")
    (fma:VDQF
      (vec_duplicate:VDQF
	(vec_select:<VEL>
	  (match_operand:VDQF 1 "register_operand" "<h_con>")
	  (parallel [(match_operand:SI 2 "immediate_operand")])))
      (match_operand:VDQF 3 "register_operand" "w")
      (match_operand:VDQF 4 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[2]));
    return "fmla\\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_fp_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_fma4_elt_<vswap_width_name><mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
    (fma:VDQSF
      (vec_duplicate:VDQSF
	(vec_select:<VEL>
	  (match_operand:<VSWAP_WIDTH> 1 "register_operand" "<h_con>")
	  (parallel [(match_operand:SI 2 "immediate_operand")])))
      (match_operand:VDQSF 3 "register_operand" "w")
      (match_operand:VDQSF 4 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSWAP_WIDTH>mode, INTVAL (operands[2]));
    return "fmla\\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_fp_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_fma4_elt_from_dup<mode>"
  [(set (match_operand:VMUL 0 "register_operand" "=w")
    (fma:VMUL
      (vec_duplicate:VMUL
	  (match_operand:<VEL> 1 "register_operand" "<h_con>"))
      (match_operand:VMUL 2 "register_operand" "w")
      (match_operand:VMUL 3 "register_operand" "0")))]
  "TARGET_SIMD"
  "fmla\t%0.<Vtype>, %2.<Vtype>, %1.<Vetype>[0]"
  [(set_attr "type" "neon<fp>_mla_<stype>_scalar<q>")]
)

(define_insn "*aarch64_fma4_elt_to_64v2df"
  [(set (match_operand:DF 0 "register_operand" "=w")
    (fma:DF
	(vec_select:DF
	  (match_operand:V2DF 1 "register_operand" "w")
	  (parallel [(match_operand:SI 2 "immediate_operand")]))
      (match_operand:DF 3 "register_operand" "w")
      (match_operand:DF 4 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (V2DFmode, INTVAL (operands[2]));
    return "fmla\\t%0.2d, %3.2d, %1.d[%2]";
  }
  [(set_attr "type" "neon_fp_mla_d_scalar_q")]
)

(define_insn "fnma<mode>4"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(fma:VHSDF
	  (neg:VHSDF (match_operand:VHSDF 1 "register_operand" "w"))
	  (match_operand:VHSDF 2 "register_operand" "w")
	  (match_operand:VHSDF 3 "register_operand" "0")))]
  "TARGET_SIMD"
  "fmls\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_mla_<stype><q>")]
)

(define_insn "*aarch64_fnma4_elt<mode>"
  [(set (match_operand:VDQF 0 "register_operand" "=w")
    (fma:VDQF
      (neg:VDQF
        (match_operand:VDQF 3 "register_operand" "w"))
      (vec_duplicate:VDQF
	(vec_select:<VEL>
	  (match_operand:VDQF 1 "register_operand" "<h_con>")
	  (parallel [(match_operand:SI 2 "immediate_operand")])))
      (match_operand:VDQF 4 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[2]));
    return "fmls\\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_fp_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_fnma4_elt_<vswap_width_name><mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
    (fma:VDQSF
      (neg:VDQSF
        (match_operand:VDQSF 3 "register_operand" "w"))
      (vec_duplicate:VDQSF
	(vec_select:<VEL>
	  (match_operand:<VSWAP_WIDTH> 1 "register_operand" "<h_con>")
	  (parallel [(match_operand:SI 2 "immediate_operand")])))
      (match_operand:VDQSF 4 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSWAP_WIDTH>mode, INTVAL (operands[2]));
    return "fmls\\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_fp_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_fnma4_elt_from_dup<mode>"
  [(set (match_operand:VMUL 0 "register_operand" "=w")
    (fma:VMUL
      (neg:VMUL
        (match_operand:VMUL 2 "register_operand" "w"))
      (vec_duplicate:VMUL
	(match_operand:<VEL> 1 "register_operand" "<h_con>"))
      (match_operand:VMUL 3 "register_operand" "0")))]
  "TARGET_SIMD"
  "fmls\t%0.<Vtype>, %2.<Vtype>, %1.<Vetype>[0]"
  [(set_attr "type" "neon<fp>_mla_<stype>_scalar<q>")]
)

(define_insn "*aarch64_fnma4_elt_to_64v2df"
  [(set (match_operand:DF 0 "register_operand" "=w")
    (fma:DF
      (vec_select:DF
	(match_operand:V2DF 1 "register_operand" "w")
	(parallel [(match_operand:SI 2 "immediate_operand")]))
      (neg:DF
        (match_operand:DF 3 "register_operand" "w"))
      (match_operand:DF 4 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (V2DFmode, INTVAL (operands[2]));
    return "fmls\\t%0.2d, %3.2d, %1.d[%2]";
  }
  [(set_attr "type" "neon_fp_mla_d_scalar_q")]
)

;; Vector versions of the floating-point frint patterns.
;; Expands to btrunc, ceil, floor, nearbyint, rint, round, frintn.
(define_insn "<frint_pattern><mode>2"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")]
		       FRINT))]
  "TARGET_SIMD"
  "frint<frint_suffix>\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_round_<stype><q>")]
)

;; Vector versions of the fcvt standard patterns.
;; Expands to lbtrunc, lround, lceil, lfloor
(define_insn "l<fcvt_pattern><su_optab><VHSDF:mode><fcvt_target>2"
  [(set (match_operand:<FCVT_TARGET> 0 "register_operand" "=w")
	(FIXUORS:<FCVT_TARGET> (unspec:<FCVT_TARGET>
			       [(match_operand:VHSDF 1 "register_operand" "w")]
			       FCVT)))]
  "TARGET_SIMD"
  "fcvt<frint_suffix><su>\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_to_int_<stype><q>")]
)

;; HF Scalar variants of related SIMD instructions.
(define_insn "l<fcvt_pattern><su_optab>hfhi2"
  [(set (match_operand:HI 0 "register_operand" "=w")
	(FIXUORS:HI (unspec:HF [(match_operand:HF 1 "register_operand" "w")]
		      FCVT)))]
  "TARGET_SIMD_F16INST"
  "fcvt<frint_suffix><su>\t%h0, %h1"
  [(set_attr "type" "neon_fp_to_int_s")]
)

(define_insn "<optab>_trunchfhi2"
  [(set (match_operand:HI 0 "register_operand" "=w")
	(FIXUORS:HI (match_operand:HF 1 "register_operand" "w")))]
  "TARGET_SIMD_F16INST"
  "fcvtz<su>\t%h0, %h1"
  [(set_attr "type" "neon_fp_to_int_s")]
)

(define_insn "<optab>hihf2"
  [(set (match_operand:HF 0 "register_operand" "=w")
	(FLOATUORS:HF (match_operand:HI 1 "register_operand" "w")))]
  "TARGET_SIMD_F16INST"
  "<su_optab>cvtf\t%h0, %h1"
  [(set_attr "type" "neon_int_to_fp_s")]
)

(define_insn "*aarch64_fcvt<su_optab><VDQF:mode><fcvt_target>2_mult"
  [(set (match_operand:<FCVT_TARGET> 0 "register_operand" "=w")
	(FIXUORS:<FCVT_TARGET> (unspec:<FCVT_TARGET>
			       [(mult:VDQF
	 (match_operand:VDQF 1 "register_operand" "w")
	 (match_operand:VDQF 2 "aarch64_fp_vec_pow2" ""))]
			       UNSPEC_FRINTZ)))]
  "TARGET_SIMD
   && IN_RANGE (aarch64_vec_fpconst_pow_of_2 (operands[2]), 1,
		GET_MODE_BITSIZE (GET_MODE_INNER (<VDQF:MODE>mode)))"
  {
    int fbits = aarch64_vec_fpconst_pow_of_2 (operands[2]);
    char buf[64];
    snprintf (buf, 64, "fcvtz<su>\\t%%0.<Vtype>, %%1.<Vtype>, #%d", fbits);
    output_asm_insn (buf, operands);
    return "";
  }
  [(set_attr "type" "neon_fp_to_int_<Vetype><q>")]
)

(define_expand "<optab><VHSDF:mode><fcvt_target>2"
  [(set (match_operand:<FCVT_TARGET> 0 "register_operand")
	(FIXUORS:<FCVT_TARGET> (unspec:<FCVT_TARGET>
			       [(match_operand:VHSDF 1 "register_operand")]
				UNSPEC_FRINTZ)))]
  "TARGET_SIMD"
  {})

(define_expand "<fix_trunc_optab><VHSDF:mode><fcvt_target>2"
  [(set (match_operand:<FCVT_TARGET> 0 "register_operand")
	(FIXUORS:<FCVT_TARGET> (unspec:<FCVT_TARGET>
			       [(match_operand:VHSDF 1 "register_operand")]
				UNSPEC_FRINTZ)))]
  "TARGET_SIMD"
  {})

(define_expand "ftrunc<VHSDF:mode>2"
  [(set (match_operand:VHSDF 0 "register_operand")
	(unspec:VHSDF [(match_operand:VHSDF 1 "register_operand")]
		       UNSPEC_FRINTZ))]
  "TARGET_SIMD"
  {})

(define_insn "<optab><fcvt_target><VHSDF:mode>2"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(FLOATUORS:VHSDF
	  (match_operand:<FCVT_TARGET> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "<su_optab>cvtf\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_int_to_fp_<stype><q>")]
)

;; Conversions between vectors of floats and doubles.
;; Contains a mix of patterns to match standard pattern names
;; and those for intrinsics.

;; Float widening operations.

(define_insn "aarch64_simd_vec_unpacks_lo_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (float_extend:<VWIDE> (vec_select:<VHALF>
			       (match_operand:VQ_HSF 1 "register_operand" "w")
			       (match_operand:VQ_HSF 2 "vect_par_cnst_lo_half" "")
			    )))]
  "TARGET_SIMD"
  "fcvtl\\t%0.<Vwtype>, %1.<Vhalftype>"
  [(set_attr "type" "neon_fp_cvt_widen_s")]
)

;; Convert between fixed-point and floating-point (vector modes)

(define_insn "<FCVT_F2FIXED:fcvt_fixed_insn><VHSDF:mode>3"
  [(set (match_operand:<VHSDF:FCVT_TARGET> 0 "register_operand" "=w")
	(unspec:<VHSDF:FCVT_TARGET>
	  [(match_operand:VHSDF 1 "register_operand" "w")
	   (match_operand:SI 2 "immediate_operand" "i")]
	 FCVT_F2FIXED))]
  "TARGET_SIMD"
  "<FCVT_F2FIXED:fcvt_fixed_insn>\t%<v>0<Vmtype>, %<v>1<Vmtype>, #%2"
  [(set_attr "type" "neon_fp_to_int_<VHSDF:stype><q>")]
)

(define_insn "<FCVT_FIXED2F:fcvt_fixed_insn><VDQ_HSDI:mode>3"
  [(set (match_operand:<VDQ_HSDI:FCVT_TARGET> 0 "register_operand" "=w")
	(unspec:<VDQ_HSDI:FCVT_TARGET>
	  [(match_operand:VDQ_HSDI 1 "register_operand" "w")
	   (match_operand:SI 2 "immediate_operand" "i")]
	 FCVT_FIXED2F))]
  "TARGET_SIMD"
  "<FCVT_FIXED2F:fcvt_fixed_insn>\t%<v>0<Vmtype>, %<v>1<Vmtype>, #%2"
  [(set_attr "type" "neon_int_to_fp_<VDQ_HSDI:stype><q>")]
)

;; ??? Note that the vectorizer usage of the vec_unpacks_[lo/hi] patterns
;; is inconsistent with vector ordering elsewhere in the compiler, in that
;; the meaning of HI and LO changes depending on the target endianness.
;; While elsewhere we map the higher numbered elements of a vector to
;; the lower architectural lanes of the vector, for these patterns we want
;; to always treat "hi" as referring to the higher architectural lanes.
;; Consequently, while the patterns below look inconsistent with our
;; other big-endian patterns their behavior is as required.

(define_expand "vec_unpacks_lo_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQ_HSF 1 "register_operand")]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
    emit_insn (gen_aarch64_simd_vec_unpacks_lo_<mode> (operands[0],
						       operands[1], p));
    DONE;
  }
)

(define_insn "aarch64_simd_vec_unpacks_hi_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (float_extend:<VWIDE> (vec_select:<VHALF>
			       (match_operand:VQ_HSF 1 "register_operand" "w")
			       (match_operand:VQ_HSF 2 "vect_par_cnst_hi_half" "")
			    )))]
  "TARGET_SIMD"
  "fcvtl2\\t%0.<Vwtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_cvt_widen_s")]
)

(define_expand "vec_unpacks_hi_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQ_HSF 1 "register_operand")]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
    emit_insn (gen_aarch64_simd_vec_unpacks_lo_<mode> (operands[0],
						       operands[1], p));
    DONE;
  }
)
(define_insn "aarch64_float_extend_lo_<Vwide>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(float_extend:<VWIDE>
	  (match_operand:VDF 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "fcvtl\\t%0<Vmwtype>, %1<Vmtype>"
  [(set_attr "type" "neon_fp_cvt_widen_s")]
)

;; Float narrowing operations.

(define_insn "aarch64_float_trunc_rodd_df"
  [(set (match_operand:SF 0 "register_operand" "=w")
	(unspec:SF [(match_operand:DF 1 "register_operand" "w")]
		UNSPEC_FCVTXN))]
  "TARGET_SIMD"
  "fcvtxn\\t%s0, %d1"
  [(set_attr "type" "neon_fp_cvt_narrow_d_q")]
)

(define_insn "aarch64_float_trunc_rodd_lo_v2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(unspec:V2SF [(match_operand:V2DF 1 "register_operand" "w")]
		UNSPEC_FCVTXN))]
  "TARGET_SIMD"
  "fcvtxn\\t%0.2s, %1.2d"
  [(set_attr "type" "neon_fp_cvt_narrow_d_q")]
)

(define_insn "aarch64_float_trunc_rodd_hi_v4sf_le"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(vec_concat:V4SF
	  (match_operand:V2SF 1 "register_operand" "0")
	  (unspec:V2SF [(match_operand:V2DF 2 "register_operand" "w")]
		UNSPEC_FCVTXN)))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "fcvtxn2\\t%0.4s, %2.2d"
  [(set_attr "type" "neon_fp_cvt_narrow_d_q")]
)

(define_insn "aarch64_float_trunc_rodd_hi_v4sf_be"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(vec_concat:V4SF
	  (unspec:V2SF [(match_operand:V2DF 2 "register_operand" "w")]
		UNSPEC_FCVTXN)
	  (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "fcvtxn2\\t%0.4s, %2.2d"
  [(set_attr "type" "neon_fp_cvt_narrow_d_q")]
)

(define_expand "aarch64_float_trunc_rodd_hi_v4sf"
  [(match_operand:V4SF 0 "register_operand")
   (match_operand:V2SF 1 "register_operand")
   (match_operand:V2DF 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx (*gen) (rtx, rtx, rtx) = BYTES_BIG_ENDIAN
			     ? gen_aarch64_float_trunc_rodd_hi_v4sf_be
			     : gen_aarch64_float_trunc_rodd_hi_v4sf_le;
  emit_insn (gen (operands[0], operands[1], operands[2]));
  DONE;
}
)

(define_insn "aarch64_float_truncate_lo_<mode>"
  [(set (match_operand:VDF 0 "register_operand" "=w")
      (float_truncate:VDF
	(match_operand:<VWIDE> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "fcvtn\\t%0.<Vtype>, %1<Vmwtype>"
  [(set_attr "type" "neon_fp_cvt_narrow_d_q")]
)

(define_insn "aarch64_float_truncate_hi_<Vdbl>_le"
  [(set (match_operand:<VDBL> 0 "register_operand" "=w")
    (vec_concat:<VDBL>
      (match_operand:VDF 1 "register_operand" "0")
      (float_truncate:VDF
	(match_operand:<VWIDE> 2 "register_operand" "w"))))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "fcvtn2\\t%0.<Vdtype>, %2<Vmwtype>"
  [(set_attr "type" "neon_fp_cvt_narrow_d_q")]
)

(define_insn "aarch64_float_truncate_hi_<Vdbl>_be"
  [(set (match_operand:<VDBL> 0 "register_operand" "=w")
    (vec_concat:<VDBL>
      (float_truncate:VDF
	(match_operand:<VWIDE> 2 "register_operand" "w"))
      (match_operand:VDF 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "fcvtn2\\t%0.<Vdtype>, %2<Vmwtype>"
  [(set_attr "type" "neon_fp_cvt_narrow_d_q")]
)

(define_expand "aarch64_float_truncate_hi_<Vdbl>"
  [(match_operand:<VDBL> 0 "register_operand")
   (match_operand:VDF 1 "register_operand")
   (match_operand:<VWIDE> 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx (*gen) (rtx, rtx, rtx) = BYTES_BIG_ENDIAN
			     ? gen_aarch64_float_truncate_hi_<Vdbl>_be
			     : gen_aarch64_float_truncate_hi_<Vdbl>_le;
  emit_insn (gen (operands[0], operands[1], operands[2]));
  DONE;
}
)

(define_expand "vec_pack_trunc_v2df"
  [(set (match_operand:V4SF 0 "register_operand")
      (vec_concat:V4SF
	(float_truncate:V2SF
	    (match_operand:V2DF 1 "register_operand"))
	(float_truncate:V2SF
	    (match_operand:V2DF 2 "register_operand"))
	  ))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (V2SFmode);
    int lo = BYTES_BIG_ENDIAN ? 2 : 1;
    int hi = BYTES_BIG_ENDIAN ? 1 : 2;

    emit_insn (gen_aarch64_float_truncate_lo_v2sf (tmp, operands[lo]));
    emit_insn (gen_aarch64_float_truncate_hi_v4sf (operands[0],
						   tmp, operands[hi]));
    DONE;
  }
)

(define_expand "vec_pack_trunc_df"
  [(set (match_operand:V2SF 0 "register_operand")
	(vec_concat:V2SF
	  (float_truncate:SF (match_operand:DF 1 "general_operand"))
	  (float_truncate:SF (match_operand:DF 2 "general_operand"))))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (V2SFmode);
    emit_insn (gen_aarch64_vec_concatdf (tmp, operands[1], operands[2]));
    emit_insn (gen_aarch64_float_truncate_lo_v2sf (operands[0], tmp));
    DONE;
  }
)

;; FP Max/Min
;; Max/Min are introduced by idiom recognition by GCC's mid-end.  An
;; expression like:
;;      a = (b < c) ? b : c;
;; is idiom-matched as MIN_EXPR<b,c> only if -ffinite-math-only and
;; -fno-signed-zeros are enabled either explicitly or indirectly via
;; -ffast-math.
;;
;; MIN_EXPR and MAX_EXPR eventually map to 'smin' and 'smax' in RTL.
;; The 'smax' and 'smin' RTL standard pattern names do not specify which
;; operand will be returned when both operands are zero (i.e. they may not
;; honour signed zeroes), or when either operand is NaN.  Therefore GCC
;; only introduces MIN_EXPR/MAX_EXPR in fast math mode or when not honouring
;; NaNs.

(define_insn "<su><maxmin><mode>3"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(FMAXMIN:VHSDF (match_operand:VHSDF 1 "register_operand" "w")
		       (match_operand:VHSDF 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "f<maxmin>nm\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_minmax_<stype><q>")]
)

;; Vector forms for fmax, fmin, fmaxnm, fminnm.
;; fmaxnm and fminnm are used for the fmax<mode>3 standard pattern names,
;; which implement the IEEE fmax ()/fmin () functions.
(define_insn "<fmaxmin><mode>3"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")
		      (match_operand:VHSDF 2 "register_operand" "w")]
		      FMAXMIN_UNS))]
  "TARGET_SIMD"
  "<maxmin_uns_op>\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_minmax_<stype><q>")]
)

;; 'across lanes' add.

(define_insn "aarch64_faddp<mode>"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")
		      (match_operand:VHSDF 2 "register_operand" "w")]
	UNSPEC_FADDV))]
 "TARGET_SIMD"
 "faddp\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_reduc_add_<stype><q>")]
)

(define_insn "reduc_plus_scal_<mode>"
 [(set (match_operand:<VEL> 0 "register_operand" "=w")
       (unspec:<VEL> [(match_operand:VDQV 1 "register_operand" "w")]
		    UNSPEC_ADDV))]
 "TARGET_SIMD"
 "add<VDQV:vp>\\t%<Vetype>0, %1.<Vtype>"
  [(set_attr "type" "neon_reduc_add<q>")]
)

(define_insn "reduc_plus_scal_v2si"
 [(set (match_operand:SI 0 "register_operand" "=w")
       (unspec:SI [(match_operand:V2SI 1 "register_operand" "w")]
		    UNSPEC_ADDV))]
 "TARGET_SIMD"
 "addp\\t%0.2s, %1.2s, %1.2s"
  [(set_attr "type" "neon_reduc_add")]
)

;; ADDV with result zero-extended to SI/DImode (for popcount).
(define_insn "aarch64_zero_extend<GPI:mode>_reduc_plus_<VDQV_E:mode>"
 [(set (match_operand:GPI 0 "register_operand" "=w")
       (zero_extend:GPI
	(unspec:<VDQV_E:VEL> [(match_operand:VDQV_E 1 "register_operand" "w")]
			     UNSPEC_ADDV)))]
 "TARGET_SIMD"
 "add<VDQV_E:vp>\\t%<VDQV_E:Vetype>0, %1.<VDQV_E:Vtype>"
  [(set_attr "type" "neon_reduc_add<VDQV_E:q>")]
)

(define_insn "reduc_plus_scal_<mode>"
 [(set (match_operand:<VEL> 0 "register_operand" "=w")
       (unspec:<VEL> [(match_operand:V2F 1 "register_operand" "w")]
		   UNSPEC_FADDV))]
 "TARGET_SIMD"
 "faddp\\t%<Vetype>0, %1.<Vtype>"
  [(set_attr "type" "neon_fp_reduc_add_<Vetype><q>")]
)

(define_expand "reduc_plus_scal_v4sf"
 [(set (match_operand:SF 0 "register_operand")
       (unspec:SF [(match_operand:V4SF 1 "register_operand")]
		    UNSPEC_FADDV))]
 "TARGET_SIMD"
{
  rtx elt = aarch64_endian_lane_rtx (V4SFmode, 0);
  rtx scratch = gen_reg_rtx (V4SFmode);
  emit_insn (gen_aarch64_faddpv4sf (scratch, operands[1], operands[1]));
  emit_insn (gen_aarch64_faddpv4sf (scratch, scratch, scratch));
  emit_insn (gen_aarch64_get_lanev4sf (operands[0], scratch, elt));
  DONE;
})

(define_insn "aarch64_<su>addlv<mode>"
 [(set (match_operand:<VWIDE_S> 0 "register_operand" "=w")
       (unspec:<VWIDE_S> [(match_operand:VDQV_L 1 "register_operand" "w")]
		    USADDLV))]
 "TARGET_SIMD"
 "<su>addl<vp>\\t%<Vwstype>0<Vwsuf>, %1.<Vtype>"
  [(set_attr "type" "neon_reduc_add<q>")]
)

(define_insn "aarch64_<su>addlp<mode>"
 [(set (match_operand:<VDBLW> 0 "register_operand" "=w")
       (unspec:<VDBLW> [(match_operand:VDQV_L 1 "register_operand" "w")]
		    USADDLP))]
 "TARGET_SIMD"
 "<su>addlp\\t%0.<Vwhalf>, %1.<Vtype>"
  [(set_attr "type" "neon_reduc_add<q>")]
)

(define_insn "clrsb<mode>2"
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
        (clrsb:VDQ_BHSI (match_operand:VDQ_BHSI 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "cls\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_cls<q>")]
)

(define_insn "clz<mode>2"
 [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
       (clz:VDQ_BHSI (match_operand:VDQ_BHSI 1 "register_operand" "w")))]
 "TARGET_SIMD"
 "clz\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_cls<q>")]
)

(define_insn "popcount<mode>2"
  [(set (match_operand:VB 0 "register_operand" "=w")
        (popcount:VB (match_operand:VB 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "cnt\\t%0.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_cnt<q>")]
)

;; 'across lanes' max and min ops.

;; Template for outputting a scalar, so we can create __builtins which can be
;; gimple_fold'd to the IFN_REDUC_(MAX|MIN) function.  (This is FP smax/smin).
(define_expand "reduc_<optab>_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (unspec:<VEL> [(match_operand:VHSDF 1 "register_operand")]
		 FMAXMINV)]
  "TARGET_SIMD"
  {
    rtx elt = aarch64_endian_lane_rtx (<MODE>mode, 0);
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_aarch64_reduc_<optab>_internal<mode> (scratch,
							 operands[1]));
    emit_insn (gen_aarch64_get_lane<mode> (operands[0], scratch, elt));
    DONE;
  }
)

(define_expand "reduc_<fmaxmin>_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (unspec:<VEL> [(match_operand:VHSDF 1 "register_operand")]
		 FMAXMINNMV)]
  "TARGET_SIMD"
  {
    emit_insn (gen_reduc_<optab>_scal_<mode> (operands[0], operands[1]));
    DONE;
  }
)

;; Likewise for integer cases, signed and unsigned.
(define_expand "reduc_<optab>_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (unspec:VDQ_BHSI [(match_operand:VDQ_BHSI 1 "register_operand")]
		    MAXMINV)]
  "TARGET_SIMD"
  {
    rtx elt = aarch64_endian_lane_rtx (<MODE>mode, 0);
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_aarch64_reduc_<optab>_internal<mode> (scratch,
							 operands[1]));
    emit_insn (gen_aarch64_get_lane<mode> (operands[0], scratch, elt));
    DONE;
  }
)

(define_insn "aarch64_reduc_<optab>_internal<mode>"
 [(set (match_operand:VDQV_S 0 "register_operand" "=w")
       (unspec:VDQV_S [(match_operand:VDQV_S 1 "register_operand" "w")]
		    MAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op>v\\t%<Vetype>0, %1.<Vtype>"
  [(set_attr "type" "neon_reduc_minmax<q>")]
)

(define_insn "aarch64_reduc_<optab>_internalv2si"
 [(set (match_operand:V2SI 0 "register_operand" "=w")
       (unspec:V2SI [(match_operand:V2SI 1 "register_operand" "w")]
		    MAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op>p\\t%0.2s, %1.2s, %1.2s"
  [(set_attr "type" "neon_reduc_minmax")]
)

(define_insn "aarch64_reduc_<optab>_internal<mode>"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")]
		      FMAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op><vp>\\t%<Vetype>0, %1.<Vtype>"
  [(set_attr "type" "neon_fp_reduc_minmax_<stype><q>")]
)

;; aarch64_simd_bsl may compile to any of bsl/bif/bit depending on register
;; allocation.
;; Operand 1 is the mask, operands 2 and 3 are the bitfields from which
;; to select.
;;
;; Thus our BSL is of the form:
;;   op0 = bsl (mask, op2, op3)
;; We can use any of:
;;
;;   if (op0 = mask)
;;     bsl mask, op1, op2
;;   if (op0 = op1) (so 1-bits in mask choose bits from op2, else op0)
;;     bit op0, op2, mask
;;   if (op0 = op2) (so 0-bits in mask choose bits from op1, else op0)
;;     bif op0, op1, mask
;;
;; This pattern is expanded to by the aarch64_simd_bsl<mode> expander.
;; Some forms of straight-line code may generate the equivalent form
;; in *aarch64_simd_bsl<mode>_alt.

(define_insn "aarch64_simd_bsl<mode>_internal"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w,w,w")
	(xor:VDQ_I
	   (and:VDQ_I
	     (xor:VDQ_I
	       (match_operand:<V_INT_EQUIV> 3 "register_operand" "w,0,w")
	       (match_operand:VDQ_I 2 "register_operand" "w,w,0"))
	     (match_operand:VDQ_I 1 "register_operand" "0,w,w"))
	  (match_dup:<V_INT_EQUIV> 3)
	))]
  "TARGET_SIMD"
  "@
  bsl\\t%0.<Vbtype>, %2.<Vbtype>, %3.<Vbtype>
  bit\\t%0.<Vbtype>, %2.<Vbtype>, %1.<Vbtype>
  bif\\t%0.<Vbtype>, %3.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_bsl<q>")]
)

;; We need this form in addition to the above pattern to match the case
;; when combine tries merging three insns such that the second operand of
;; the outer XOR matches the second operand of the inner XOR rather than
;; the first.  The two are equivalent but since recog doesn't try all
;; permutations of commutative operations, we have to have a separate pattern.

(define_insn "*aarch64_simd_bsl<mode>_alt"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w,w,w")
	(xor:VDQ_I
	   (and:VDQ_I
	     (xor:VDQ_I
	       (match_operand:VDQ_I 3 "register_operand" "w,w,0")
	       (match_operand:<V_INT_EQUIV> 2 "register_operand" "w,0,w"))
	      (match_operand:VDQ_I 1 "register_operand" "0,w,w"))
	  (match_dup:<V_INT_EQUIV> 2)))]
  "TARGET_SIMD"
  "@
  bsl\\t%0.<Vbtype>, %3.<Vbtype>, %2.<Vbtype>
  bit\\t%0.<Vbtype>, %3.<Vbtype>, %1.<Vbtype>
  bif\\t%0.<Vbtype>, %2.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_bsl<q>")]
)

;; DImode is special, we want to avoid computing operations which are
;; more naturally computed in general purpose registers in the vector
;; registers.  If we do that, we need to move all three operands from general
;; purpose registers to vector registers, then back again.  However, we
;; don't want to make this pattern an UNSPEC as we'd lose scope for
;; optimizations based on the component operations of a BSL.
;;
;; That means we need a splitter back to the individual operations, if they
;; would be better calculated on the integer side.

(define_insn_and_split "aarch64_simd_bsldi_internal"
  [(set (match_operand:DI 0 "register_operand" "=w,w,w,&r")
	(xor:DI
	   (and:DI
	     (xor:DI
	       (match_operand:DI 3 "register_operand" "w,0,w,r")
	       (match_operand:DI 2 "register_operand" "w,w,0,r"))
	     (match_operand:DI 1 "register_operand" "0,w,w,r"))
	  (match_dup:DI 3)
	))]
  "TARGET_SIMD"
  "@
  bsl\\t%0.8b, %2.8b, %3.8b
  bit\\t%0.8b, %2.8b, %1.8b
  bif\\t%0.8b, %3.8b, %1.8b
  #"
  "&& REG_P (operands[0]) && GP_REGNUM_P (REGNO (operands[0]))"
  [(match_dup 1) (match_dup 1) (match_dup 2) (match_dup 3)]
{
  /* Split back to individual operations.  If we're before reload, and
     able to create a temporary register, do so.  If we're after reload,
     we've got an early-clobber destination register, so use that.
     Otherwise, we can't create pseudos and we can't yet guarantee that
     operands[0] is safe to write, so FAIL to split.  */

  rtx scratch;
  if (reload_completed)
    scratch = operands[0];
  else if (can_create_pseudo_p ())
    scratch = gen_reg_rtx (DImode);
  else
    FAIL;

  emit_insn (gen_xordi3 (scratch, operands[2], operands[3]));
  emit_insn (gen_anddi3 (scratch, scratch, operands[1]));
  emit_insn (gen_xordi3 (operands[0], scratch, operands[3]));
  DONE;
}
  [(set_attr "type" "neon_bsl,neon_bsl,neon_bsl,multiple")
   (set_attr "length" "4,4,4,12")]
)

(define_insn_and_split "aarch64_simd_bsldi_alt"
  [(set (match_operand:DI 0 "register_operand" "=w,w,w,&r")
	(xor:DI
	   (and:DI
	     (xor:DI
	       (match_operand:DI 3 "register_operand" "w,w,0,r")
	       (match_operand:DI 2 "register_operand" "w,0,w,r"))
	     (match_operand:DI 1 "register_operand" "0,w,w,r"))
	  (match_dup:DI 2)
	))]
  "TARGET_SIMD"
  "@
  bsl\\t%0.8b, %3.8b, %2.8b
  bit\\t%0.8b, %3.8b, %1.8b
  bif\\t%0.8b, %2.8b, %1.8b
  #"
  "&& REG_P (operands[0]) && GP_REGNUM_P (REGNO (operands[0]))"
  [(match_dup 0) (match_dup 1) (match_dup 2) (match_dup 3)]
{
  /* Split back to individual operations.  If we're before reload, and
     able to create a temporary register, do so.  If we're after reload,
     we've got an early-clobber destination register, so use that.
     Otherwise, we can't create pseudos and we can't yet guarantee that
     operands[0] is safe to write, so FAIL to split.  */

  rtx scratch;
  if (reload_completed)
    scratch = operands[0];
  else if (can_create_pseudo_p ())
    scratch = gen_reg_rtx (DImode);
  else
    FAIL;

  emit_insn (gen_xordi3 (scratch, operands[2], operands[3]));
  emit_insn (gen_anddi3 (scratch, scratch, operands[1]));
  emit_insn (gen_xordi3 (operands[0], scratch, operands[2]));
  DONE;
}
  [(set_attr "type" "neon_bsl,neon_bsl,neon_bsl,multiple")
   (set_attr "length" "4,4,4,12")]
)

(define_expand "aarch64_simd_bsl<mode>"
  [(match_operand:VALLDIF 0 "register_operand")
   (match_operand:<V_INT_EQUIV> 1 "register_operand")
   (match_operand:VALLDIF 2 "register_operand")
   (match_operand:VALLDIF 3 "register_operand")]
 "TARGET_SIMD"
{
  /* We can't alias operands together if they have different modes.  */
  rtx tmp = operands[0];
  if (FLOAT_MODE_P (<MODE>mode))
    {
      operands[2] = gen_lowpart (<V_INT_EQUIV>mode, operands[2]);
      operands[3] = gen_lowpart (<V_INT_EQUIV>mode, operands[3]);
      tmp = gen_reg_rtx (<V_INT_EQUIV>mode);
    }
  operands[1] = gen_lowpart (<V_INT_EQUIV>mode, operands[1]);
  emit_insn (gen_aarch64_simd_bsl<v_int_equiv>_internal (tmp,
							 operands[1],
							 operands[2],
							 operands[3]));
  if (tmp != operands[0])
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, tmp));

  DONE;
})

(define_expand "vcond_mask_<mode><v_int_equiv>"
  [(match_operand:VALLDI 0 "register_operand")
   (match_operand:VALLDI 1 "nonmemory_operand")
   (match_operand:VALLDI 2 "nonmemory_operand")
   (match_operand:<V_INT_EQUIV> 3 "register_operand")]
  "TARGET_SIMD"
{
  /* If we have (a = (P) ? -1 : 0);
     Then we can simply move the generated mask (result must be int).  */
  if (operands[1] == CONSTM1_RTX (<MODE>mode)
      && operands[2] == CONST0_RTX (<MODE>mode))
    emit_move_insn (operands[0], operands[3]);
  /* Similarly, (a = (P) ? 0 : -1) is just inverting the generated mask.  */
  else if (operands[1] == CONST0_RTX (<MODE>mode)
	   && operands[2] == CONSTM1_RTX (<MODE>mode))
    emit_insn (gen_one_cmpl<v_int_equiv>2 (operands[0], operands[3]));
  else
    {
      if (!REG_P (operands[1]))
	operands[1] = force_reg (<MODE>mode, operands[1]);
      if (!REG_P (operands[2]))
	operands[2] = force_reg (<MODE>mode, operands[2]);
      emit_insn (gen_aarch64_simd_bsl<mode> (operands[0], operands[3],
					     operands[1], operands[2]));
    }

  DONE;
})

;; Patterns comparing two vectors to produce a mask.

(define_expand "vec_cmp<mode><mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand")
	  (match_operator 1 "comparison_operator"
	    [(match_operand:VSDQ_I_DI 2 "register_operand")
	     (match_operand:VSDQ_I_DI 3 "nonmemory_operand")]))]
  "TARGET_SIMD"
{
  rtx mask = operands[0];
  enum rtx_code code = GET_CODE (operands[1]);

  switch (code)
    {
    case NE:
    case LE:
    case LT:
    case GE:
    case GT:
    case EQ:
      if (operands[3] == CONST0_RTX (<MODE>mode))
	break;

      /* Fall through.  */
    default:
      if (!REG_P (operands[3]))
	operands[3] = force_reg (<MODE>mode, operands[3]);

      break;
    }

  switch (code)
    {
    case LT:
      emit_insn (gen_aarch64_cmlt<mode> (mask, operands[2], operands[3]));
      break;

    case GE:
      emit_insn (gen_aarch64_cmge<mode> (mask, operands[2], operands[3]));
      break;

    case LE:
      emit_insn (gen_aarch64_cmle<mode> (mask, operands[2], operands[3]));
      break;

    case GT:
      emit_insn (gen_aarch64_cmgt<mode> (mask, operands[2], operands[3]));
      break;

    case LTU:
      emit_insn (gen_aarch64_cmgtu<mode> (mask, operands[3], operands[2]));
      break;

    case GEU:
      emit_insn (gen_aarch64_cmgeu<mode> (mask, operands[2], operands[3]));
      break;

    case LEU:
      emit_insn (gen_aarch64_cmgeu<mode> (mask, operands[3], operands[2]));
      break;

    case GTU:
      emit_insn (gen_aarch64_cmgtu<mode> (mask, operands[2], operands[3]));
      break;

    case NE:
      /* Handle NE as !EQ.  */
      emit_insn (gen_aarch64_cmeq<mode> (mask, operands[2], operands[3]));
      emit_insn (gen_one_cmpl<v_int_equiv>2 (mask, mask));
      break;

    case EQ:
      emit_insn (gen_aarch64_cmeq<mode> (mask, operands[2], operands[3]));
      break;

    default:
      gcc_unreachable ();
    }

  DONE;
})

(define_expand "vec_cmp<mode><v_int_equiv>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand")
	(match_operator 1 "comparison_operator"
	    [(match_operand:VDQF 2 "register_operand")
	     (match_operand:VDQF 3 "nonmemory_operand")]))]
  "TARGET_SIMD"
{
  int use_zero_form = 0;
  enum rtx_code code = GET_CODE (operands[1]);
  rtx tmp = gen_reg_rtx (<V_INT_EQUIV>mode);

  rtx (*comparison) (rtx, rtx, rtx) = NULL;

  switch (code)
    {
    case LE:
    case LT:
    case GE:
    case GT:
    case EQ:
      if (operands[3] == CONST0_RTX (<MODE>mode))
	{
	  use_zero_form = 1;
	  break;
	}
      /* Fall through.  */
    default:
      if (!REG_P (operands[3]))
	operands[3] = force_reg (<MODE>mode, operands[3]);

      break;
    }

  switch (code)
    {
    case LT:
      if (use_zero_form)
	{
	  comparison = gen_aarch64_cmlt<mode>;
	  break;
	}
      /* Fall through.  */
    case UNLT:
      std::swap (operands[2], operands[3]);
      /* Fall through.  */
    case UNGT:
    case GT:
      comparison = gen_aarch64_cmgt<mode>;
      break;
    case LE:
      if (use_zero_form)
	{
	  comparison = gen_aarch64_cmle<mode>;
	  break;
	}
      /* Fall through.  */
    case UNLE:
      std::swap (operands[2], operands[3]);
      /* Fall through.  */
    case UNGE:
    case GE:
      comparison = gen_aarch64_cmge<mode>;
      break;
    case NE:
    case EQ:
      comparison = gen_aarch64_cmeq<mode>;
      break;
    case UNEQ:
    case ORDERED:
    case UNORDERED:
    case LTGT:
      break;
    default:
      gcc_unreachable ();
    }

  switch (code)
    {
    case UNGE:
    case UNGT:
    case UNLE:
    case UNLT:
      {
	/* All of the above must not raise any FP exceptions.  Thus we first
	   check each operand for NaNs and force any elements containing NaN to
	   zero before using them in the compare.
	   Example: UN<cc> (a, b) -> UNORDERED (a, b) |
				     (cm<cc> (isnan (a) ? 0.0 : a,
					      isnan (b) ? 0.0 : b))
	   We use the following transformations for doing the comparisions:
	   a UNGE b -> a GE b
	   a UNGT b -> a GT b
	   a UNLE b -> b GE a
	   a UNLT b -> b GT a.  */

	rtx tmp0 = gen_reg_rtx (<V_INT_EQUIV>mode);
	rtx tmp1 = gen_reg_rtx (<V_INT_EQUIV>mode);
	rtx tmp2 = gen_reg_rtx (<V_INT_EQUIV>mode);
	emit_insn (gen_aarch64_cmeq<mode> (tmp0, operands[2], operands[2]));
	emit_insn (gen_aarch64_cmeq<mode> (tmp1, operands[3], operands[3]));
	emit_insn (gen_and<v_int_equiv>3 (tmp2, tmp0, tmp1));
	emit_insn (gen_and<v_int_equiv>3 (tmp0, tmp0,
					  lowpart_subreg (<V_INT_EQUIV>mode,
							  operands[2],
							  <MODE>mode)));
	emit_insn (gen_and<v_int_equiv>3 (tmp1, tmp1,
					  lowpart_subreg (<V_INT_EQUIV>mode,
							  operands[3],
							  <MODE>mode)));
	gcc_assert (comparison != NULL);
	emit_insn (comparison (operands[0],
			       lowpart_subreg (<MODE>mode,
					       tmp0, <V_INT_EQUIV>mode),
			       lowpart_subreg (<MODE>mode,
					       tmp1, <V_INT_EQUIV>mode)));
	emit_insn (gen_orn<v_int_equiv>3 (operands[0], tmp2, operands[0]));
      }
      break;

    case LT:
    case LE:
    case GT:
    case GE:
    case EQ:
    case NE:
      /* The easy case.  Here we emit one of FCMGE, FCMGT or FCMEQ.
	 As a LT b <=> b GE a && a LE b <=> b GT a.  Our transformations are:
	 a GE b -> a GE b
	 a GT b -> a GT b
	 a LE b -> b GE a
	 a LT b -> b GT a
	 a EQ b -> a EQ b
	 a NE b -> ~(a EQ b)  */
      gcc_assert (comparison != NULL);
      emit_insn (comparison (operands[0], operands[2], operands[3]));
      if (code == NE)
	emit_insn (gen_one_cmpl<v_int_equiv>2 (operands[0], operands[0]));
      break;

    case LTGT:
      /* LTGT is not guranteed to not generate a FP exception.  So let's
	 go the faster way : ((a > b) || (b > a)).  */
      emit_insn (gen_aarch64_cmgt<mode> (operands[0],
					 operands[2], operands[3]));
      emit_insn (gen_aarch64_cmgt<mode> (tmp, operands[3], operands[2]));
      emit_insn (gen_ior<v_int_equiv>3 (operands[0], operands[0], tmp));
      break;

    case ORDERED:
    case UNORDERED:
    case UNEQ:
      /* cmeq (a, a) & cmeq (b, b).  */
      emit_insn (gen_aarch64_cmeq<mode> (operands[0],
					 operands[2], operands[2]));
      emit_insn (gen_aarch64_cmeq<mode> (tmp, operands[3], operands[3]));
      emit_insn (gen_and<v_int_equiv>3 (operands[0], operands[0], tmp));

      if (code == UNORDERED)
	emit_insn (gen_one_cmpl<v_int_equiv>2 (operands[0], operands[0]));
      else if (code == UNEQ)
	{
	  emit_insn (gen_aarch64_cmeq<mode> (tmp, operands[2], operands[3]));
	  emit_insn (gen_orn<v_int_equiv>3 (operands[0], operands[0], tmp));
	}
      break;

    default:
      gcc_unreachable ();
    }

  DONE;
})

(define_expand "vec_cmpu<mode><mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand")
	  (match_operator 1 "comparison_operator"
	    [(match_operand:VSDQ_I_DI 2 "register_operand")
	     (match_operand:VSDQ_I_DI 3 "nonmemory_operand")]))]
  "TARGET_SIMD"
{
  emit_insn (gen_vec_cmp<mode><mode> (operands[0], operands[1],
				      operands[2], operands[3]));
  DONE;
})

(define_expand "vcond<mode><mode>"
  [(set (match_operand:VALLDI 0 "register_operand")
	(if_then_else:VALLDI
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VALLDI 4 "register_operand")
	     (match_operand:VALLDI 5 "nonmemory_operand")])
	  (match_operand:VALLDI 1 "nonmemory_operand")
	  (match_operand:VALLDI 2 "nonmemory_operand")))]
  "TARGET_SIMD"
{
  rtx mask = gen_reg_rtx (<V_INT_EQUIV>mode);
  enum rtx_code code = GET_CODE (operands[3]);

  /* NE is handled as !EQ in vec_cmp patterns, we can explicitly invert
     it as well as switch operands 1/2 in order to avoid the additional
     NOT instruction.  */
  if (code == NE)
    {
      operands[3] = gen_rtx_fmt_ee (EQ, GET_MODE (operands[3]),
				    operands[4], operands[5]);
      std::swap (operands[1], operands[2]);
    }
  emit_insn (gen_vec_cmp<mode><v_int_equiv> (mask, operands[3],
					     operands[4], operands[5]));
  emit_insn (gen_vcond_mask_<mode><v_int_equiv> (operands[0], operands[1],
						 operands[2], mask));

  DONE;
})

(define_expand "vcond<v_cmp_mixed><mode>"
  [(set (match_operand:<V_cmp_mixed> 0 "register_operand")
	(if_then_else:<V_cmp_mixed>
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VDQF_COND 4 "register_operand")
	     (match_operand:VDQF_COND 5 "nonmemory_operand")])
	  (match_operand:<V_cmp_mixed> 1 "nonmemory_operand")
	  (match_operand:<V_cmp_mixed> 2 "nonmemory_operand")))]
  "TARGET_SIMD"
{
  rtx mask = gen_reg_rtx (<V_INT_EQUIV>mode);
  enum rtx_code code = GET_CODE (operands[3]);

  /* NE is handled as !EQ in vec_cmp patterns, we can explicitly invert
     it as well as switch operands 1/2 in order to avoid the additional
     NOT instruction.  */
  if (code == NE)
    {
      operands[3] = gen_rtx_fmt_ee (EQ, GET_MODE (operands[3]),
				    operands[4], operands[5]);
      std::swap (operands[1], operands[2]);
    }
  emit_insn (gen_vec_cmp<mode><v_int_equiv> (mask, operands[3],
					     operands[4], operands[5]));
  emit_insn (gen_vcond_mask_<v_cmp_mixed><v_int_equiv> (
						operands[0], operands[1],
						operands[2], mask));

  DONE;
})

(define_expand "vcondu<mode><mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand")
	(if_then_else:VSDQ_I_DI
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VSDQ_I_DI 4 "register_operand")
	     (match_operand:VSDQ_I_DI 5 "nonmemory_operand")])
	  (match_operand:VSDQ_I_DI 1 "nonmemory_operand")
	  (match_operand:VSDQ_I_DI 2 "nonmemory_operand")))]
  "TARGET_SIMD"
{
  rtx mask = gen_reg_rtx (<MODE>mode);
  enum rtx_code code = GET_CODE (operands[3]);

  /* NE is handled as !EQ in vec_cmp patterns, we can explicitly invert
     it as well as switch operands 1/2 in order to avoid the additional
     NOT instruction.  */
  if (code == NE)
    {
      operands[3] = gen_rtx_fmt_ee (EQ, GET_MODE (operands[3]),
				    operands[4], operands[5]);
      std::swap (operands[1], operands[2]);
    }
  emit_insn (gen_vec_cmp<mode><mode> (mask, operands[3],
				      operands[4], operands[5]));
  emit_insn (gen_vcond_mask_<mode><v_int_equiv> (operands[0], operands[1],
						 operands[2], mask));
  DONE;
})

(define_expand "vcondu<mode><v_cmp_mixed>"
  [(set (match_operand:VDQF 0 "register_operand")
	(if_then_else:VDQF
	  (match_operator 3 "comparison_operator"
	    [(match_operand:<V_cmp_mixed> 4 "register_operand")
	     (match_operand:<V_cmp_mixed> 5 "nonmemory_operand")])
	  (match_operand:VDQF 1 "nonmemory_operand")
	  (match_operand:VDQF 2 "nonmemory_operand")))]
  "TARGET_SIMD"
{
  rtx mask = gen_reg_rtx (<V_INT_EQUIV>mode);
  enum rtx_code code = GET_CODE (operands[3]);

  /* NE is handled as !EQ in vec_cmp patterns, we can explicitly invert
     it as well as switch operands 1/2 in order to avoid the additional
     NOT instruction.  */
  if (code == NE)
    {
      operands[3] = gen_rtx_fmt_ee (EQ, GET_MODE (operands[3]),
				    operands[4], operands[5]);
      std::swap (operands[1], operands[2]);
    }
  emit_insn (gen_vec_cmp<v_cmp_mixed><v_cmp_mixed> (
						  mask, operands[3],
						  operands[4], operands[5]));
  emit_insn (gen_vcond_mask_<mode><v_int_equiv> (operands[0], operands[1],
						 operands[2], mask));
  DONE;
})

;; Patterns for AArch64 SIMD Intrinsics.

;; Lane extraction with sign extension to general purpose register.
(define_insn "*aarch64_get_lane_extend<GPI:mode><VDQQH:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(sign_extend:GPI
	  (vec_select:<VDQQH:VEL>
	    (match_operand:VDQQH 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VDQQH:MODE>mode,
					   INTVAL (operands[2]));
    return "smov\\t%<GPI:w>0, %1.<VDQQH:Vetype>[%2]";
  }
  [(set_attr "type" "neon_to_gp<VDQQH:q>")]
)

(define_insn "*aarch64_get_lane_zero_extend<GPI:mode><VDQQH:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(zero_extend:GPI
	  (vec_select:<VDQQH:VEL>
	    (match_operand:VDQQH 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VDQQH:MODE>mode,
					   INTVAL (operands[2]));
    return "umov\\t%w0, %1.<VDQQH:Vetype>[%2]";
  }
  [(set_attr "type" "neon_to_gp<VDQQH:q>")]
)

;; Lane extraction of a value, neither sign nor zero extension
;; is guaranteed so upper bits should be considered undefined.
;; RTL uses GCC vector extension indices throughout so flip only for assembly.
;; Extracting lane zero is split into a simple move when it is between SIMD
;; registers or a store.
(define_insn_and_split "aarch64_get_lane<mode>"
  [(set (match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "=?r, w, Utv")
	(vec_select:<VEL>
	  (match_operand:VALL_F16 1 "register_operand" "w, w, w")
	  (parallel [(match_operand:SI 2 "immediate_operand" "i, i, i")])))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[2]));
    switch (which_alternative)
      {
	case 0:
	  return "umov\\t%<vwcore>0, %1.<Vetype>[%2]";
	case 1:
	  return "dup\\t%<Vetype>0, %1.<Vetype>[%2]";
	case 2:
	  return "st1\\t{%1.<Vetype>}[%2], %0";
	default:
	  gcc_unreachable ();
      }
  }
 "&& reload_completed
  && ENDIAN_LANE_N (<nunits>, INTVAL (operands[2])) == 0"
 [(set (match_dup 0) (match_dup 1))]
 {
   operands[1] = aarch64_replace_reg_mode (operands[1], <VEL>mode);
 }
  [(set_attr "type" "neon_to_gp<q>, neon_dup<q>, neon_store1_one_lane<q>")]
)

(define_insn "*aarch64_get_high<mode>"
  [(set (match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "=r")
	(vec_select:<VEL>
	  (match_operand:VQ_2E 1 "register_operand" "w")
	  (parallel [(match_operand:SI 2 "immediate_operand")])))]
  "TARGET_FLOAT && ENDIAN_LANE_N (<nunits>, INTVAL (operands[2])) == 1"
  "fmov\t%0, %1.d[1]"
  [(set_attr "type" "f_mrc")]
)

(define_insn "load_pair_lanes<mode>"
  [(set (match_operand:<VDBL> 0 "register_operand" "=w")
	(vec_concat:<VDBL>
	   (match_operand:VDCSIF 1 "memory_operand" "Utq")
	   (match_operand:VDCSIF 2 "memory_operand" "m")))]
  "TARGET_FLOAT
   && aarch64_mergeable_load_pair_p (<VDBL>mode, operands[1], operands[2])"
  "ldr\\t%<single_dtype>0, %1"
  [(set_attr "type" "neon_load1_1reg<dblq>")]
)

;; This STP pattern is a partial duplicate of the general vec_concat patterns
;; below.  The reason for having both of them is that the alternatives of
;; the later patterns do not have consistent register preferences: the STP
;; alternatives have no preference between GPRs and FPRs (and if anything,
;; the GPR form is more natural for scalar integers) whereas the other
;; alternatives *require* an FPR for operand 1 and prefer one for operand 2.
;;
;; Using "*" to hide the STP alternatives from the RA penalizes cases in
;; which the destination was always memory.  On the other hand, expressing
;; the true preferences makes GPRs seem more palatable than they really are
;; for register destinations.
;;
;; Despite that, we do still want the general form to have STP alternatives,
;; in order to handle cases where a register destination is spilled.
;;
;; The best compromise therefore seemed to be to have a dedicated STP
;; pattern to catch cases in which the destination was always memory.
;; This dedicated pattern must come first.

(define_insn "store_pair_lanes<mode>"
  [(set (match_operand:<VDBL> 0 "aarch64_mem_pair_lanes_operand" "=Umn, Umn")
	(vec_concat:<VDBL>
	   (match_operand:VDCSIF 1 "register_operand" "w, r")
	   (match_operand:VDCSIF 2 "register_operand" "w, r")))]
  "TARGET_FLOAT"
  "@
   stp\t%<single_type>1, %<single_type>2, %y0
   stp\t%<single_wx>1, %<single_wx>2, %y0"
  [(set_attr "type" "neon_stp, store_16")]
)

;; Form a vector whose least significant half comes from operand 1 and whose
;; most significant half comes from operand 2.  The register alternatives
;; tie the least significant half to the same register as the destination,
;; so that only the other half needs to be handled explicitly.  For the
;; reasons given above, the STP alternatives use ? for constraints that
;; the register alternatives either don't accept or themselves disparage.

(define_insn "*aarch64_combine_internal<mode>"
  [(set (match_operand:<VDBL> 0 "aarch64_reg_or_mem_pair_operand" "=w, w, w, w, Umn, Umn")
	(vec_concat:<VDBL>
	  (match_operand:VDCSIF 1 "register_operand" "0, 0, 0, 0, ?w, ?r")
	  (match_operand:VDCSIF 2 "aarch64_simd_nonimmediate_operand" "w, ?r, ?r, Utv, w, ?r")))]
  "TARGET_FLOAT
   && !BYTES_BIG_ENDIAN
   && (register_operand (operands[0], <VDBL>mode)
       || register_operand (operands[2], <MODE>mode))"
  "@
   ins\t%0.<single_type>[1], %2.<single_type>[0]
   ins\t%0.<single_type>[1], %<single_wx>2
   fmov\t%0.d[1], %2
   ld1\t{%0.<single_type>}[1], %2
   stp\t%<single_type>1, %<single_type>2, %y0
   stp\t%<single_wx>1, %<single_wx>2, %y0"
  [(set_attr "type" "neon_ins<dblq>, neon_from_gp<dblq>, f_mcr,
		     neon_load1_one_lane<dblq>, neon_stp, store_16")
   (set_attr "arch" "simd,simd,*,simd,*,*")]
)

(define_insn "*aarch64_combine_internal_be<mode>"
  [(set (match_operand:<VDBL> 0 "aarch64_reg_or_mem_pair_operand" "=w, w, w, w, Umn, Umn")
	(vec_concat:<VDBL>
	  (match_operand:VDCSIF 2 "aarch64_simd_nonimmediate_operand" "w, ?r, ?r, Utv, ?w, ?r")
	  (match_operand:VDCSIF 1 "register_operand" "0, 0, 0, 0, ?w, ?r")))]
  "TARGET_FLOAT
   && BYTES_BIG_ENDIAN
   && (register_operand (operands[0], <VDBL>mode)
       || register_operand (operands[2], <MODE>mode))"
  "@
   ins\t%0.<single_type>[1], %2.<single_type>[0]
   ins\t%0.<single_type>[1], %<single_wx>2
   fmov\t%0.d[1], %2
   ld1\t{%0.<single_type>}[1], %2
   stp\t%<single_type>2, %<single_type>1, %y0
   stp\t%<single_wx>2, %<single_wx>1, %y0"
  [(set_attr "type" "neon_ins<dblq>, neon_from_gp<dblq>, f_mcr, neon_load1_one_lane<dblq>, neon_stp, store_16")
   (set_attr "arch" "simd,simd,*,simd,*,*")]
)

;; In this insn, operand 1 should be low, and operand 2 the high part of the
;; dest vector.

(define_insn "*aarch64_combinez<mode>"
  [(set (match_operand:<VDBL> 0 "register_operand" "=w,w,w")
	(vec_concat:<VDBL>
	  (match_operand:VDCSIF 1 "nonimmediate_operand" "w,?r,m")
	  (match_operand:VDCSIF 2 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_FLOAT && !BYTES_BIG_ENDIAN"
  "@
   fmov\\t%<single_type>0, %<single_type>1
   fmov\t%<single_type>0, %<single_wx>1
   ldr\\t%<single_type>0, %1"
  [(set_attr "type" "neon_move<q>, neon_from_gp, neon_load1_1reg")]
)

(define_insn "*aarch64_combinez_be<mode>"
  [(set (match_operand:<VDBL> 0 "register_operand" "=w,w,w")
        (vec_concat:<VDBL>
	  (match_operand:VDCSIF 2 "aarch64_simd_or_scalar_imm_zero")
	  (match_operand:VDCSIF 1 "nonimmediate_operand" "w,?r,m")))]
  "TARGET_FLOAT && BYTES_BIG_ENDIAN"
  "@
   fmov\\t%<single_type>0, %<single_type>1
   fmov\t%<single_type>0, %<single_wx>1
   ldr\\t%<single_type>0, %1"
  [(set_attr "type" "neon_move<q>, neon_from_gp, neon_load1_1reg")]
)

;; Form a vector whose first half (in array order) comes from operand 1
;; and whose second half (in array order) comes from operand 2.
;; This operand order follows the RTL vec_concat operation.
(define_expand "@aarch64_vec_concat<mode>"
  [(set (match_operand:<VDBL> 0 "register_operand")
	(vec_concat:<VDBL>
	  (match_operand:VDCSIF 1 "general_operand")
	  (match_operand:VDCSIF 2 "general_operand")))]
  "TARGET_FLOAT"
{
  int lo = BYTES_BIG_ENDIAN ? 2 : 1;
  int hi = BYTES_BIG_ENDIAN ? 1 : 2;

  if (MEM_P (operands[1])
      && MEM_P (operands[2])
      && aarch64_mergeable_load_pair_p (<VDBL>mode, operands[1], operands[2]))
    /* Use load_pair_lanes<mode>.  */
    ;
  else if (operands[hi] == CONST0_RTX (<MODE>mode))
    {
      /* Use *aarch64_combinez<mode>.  */
      if (!nonimmediate_operand (operands[lo], <MODE>mode))
	operands[lo] = force_reg (<MODE>mode, operands[lo]);
    }
  else
    {
      /* Use *aarch64_combine_internal<mode>.  */
      operands[lo] = force_reg (<MODE>mode, operands[lo]);
      if (!aarch64_simd_nonimmediate_operand (operands[hi], <MODE>mode))
	{
	  if (MEM_P (operands[hi]))
	    {
	      rtx addr = force_reg (Pmode, XEXP (operands[hi], 0));
	      operands[hi] = replace_equiv_address (operands[hi], addr);
	    }
	  else
	    operands[hi] = force_reg (<MODE>mode, operands[hi]);
	}
    }
})

;; Form a vector whose least significant half comes from operand 1 and whose
;; most significant half comes from operand 2.  This operand order follows
;; arm_neon.h vcombine* intrinsics.
(define_expand "aarch64_combine<mode>"
  [(match_operand:<VDBL> 0 "register_operand")
   (match_operand:VDC 1 "general_operand")
   (match_operand:VDC 2 "general_operand")]
  "TARGET_FLOAT"
{
  if (BYTES_BIG_ENDIAN)
    std::swap (operands[1], operands[2]);
  emit_insn (gen_aarch64_vec_concat<mode> (operands[0], operands[1],
					   operands[2]));
  DONE;
}
)

;; <su><addsub>l<q>.

(define_insn "aarch64_<ANY_EXTEND:su><ADDSUB:optab>l<mode>_hi_internal"
 [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
       (ADDSUB:<VWIDE> (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			   (match_operand:VQW 1 "register_operand" "w")
			   (match_operand:VQW 3 "vect_par_cnst_hi_half" "")))
		       (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			   (match_operand:VQW 2 "register_operand" "w")
			   (match_dup 3)))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su><ADDSUB:optab>l2\t%0.<Vwtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<ADDSUB:optab>_long")]
)

(define_insn "aarch64_<ANY_EXTEND:su><ADDSUB:optab>l<mode>_lo_internal"
 [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
       (ADDSUB:<VWIDE> (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                           (match_operand:VQW 1 "register_operand" "w")
                           (match_operand:VQW 3 "vect_par_cnst_lo_half" "")))
                       (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
                           (match_operand:VQW 2 "register_operand" "w")
                           (match_dup 3)))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su><ADDSUB:optab>l\t%0.<Vwtype>, %1.<Vhalftype>, %2.<Vhalftype>"
  [(set_attr "type" "neon_<ADDSUB:optab>_long")]
)

(define_expand "vec_widen_<su>addl_lo_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand"))]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
  emit_insn (gen_aarch64_<su>addl<mode>_lo_internal (operands[0], operands[1],
						     operands[2], p));
  DONE;
})

(define_expand "vec_widen_<su>addl_hi_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand"))]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>addl<mode>_hi_internal (operands[0], operands[1],
						     operands[2], p));
  DONE;
})

(define_expand "vec_widen_<su>subl_lo_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand"))]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
  emit_insn (gen_aarch64_<su>subl<mode>_lo_internal (operands[0], operands[1],
						     operands[2], p));
  DONE;
})

(define_expand "vec_widen_<su>subl_hi_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand"))]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_<su>subl<mode>_hi_internal (operands[0], operands[1],
						     operands[2], p));
  DONE;
})

(define_expand "aarch64_saddl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQW 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_saddl<mode>_hi_internal (operands[0], operands[1],
                                                  operands[2], p));
  DONE;
})

(define_expand "aarch64_uaddl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQW 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_uaddl<mode>_hi_internal (operands[0], operands[1],
                                                  operands[2], p));
  DONE;
})

(define_expand "aarch64_ssubl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQW 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_ssubl<mode>_hi_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

(define_expand "aarch64_usubl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQW 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_usubl<mode>_hi_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

(define_insn "aarch64_<ANY_EXTEND:su><ADDSUB:optab>l<mode>"
 [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
       (ADDSUB:<VWIDE> (ANY_EXTEND:<VWIDE>
			   (match_operand:VD_BHSI 1 "register_operand" "w"))
		       (ANY_EXTEND:<VWIDE>
			   (match_operand:VD_BHSI 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su><ADDSUB:optab>l\t%0.<Vwtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<ADDSUB:optab>_long")]
)

;; <su><addsub>w<q>.

(define_expand "widen_ssum<mode>3"
  [(set (match_operand:<VDBLW> 0 "register_operand")
	(plus:<VDBLW> (sign_extend:<VDBLW> 
		        (match_operand:VQW 1 "register_operand"))
		      (match_operand:<VDBLW> 2 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
    rtx temp = gen_reg_rtx (GET_MODE (operands[0]));

    emit_insn (gen_aarch64_saddw<mode>_internal (temp, operands[2],
						operands[1], p));
    emit_insn (gen_aarch64_saddw2<mode> (operands[0], temp, operands[1]));
    DONE;
  }
)

(define_expand "widen_ssum<mode>3"
  [(set (match_operand:<VWIDE> 0 "register_operand")
	(plus:<VWIDE> (sign_extend:<VWIDE>
		        (match_operand:VD_BHSI 1 "register_operand"))
		      (match_operand:<VWIDE> 2 "register_operand")))]
  "TARGET_SIMD"
{
  emit_insn (gen_aarch64_saddw<mode> (operands[0], operands[2], operands[1]));
  DONE;
})

(define_expand "widen_usum<mode>3"
  [(set (match_operand:<VDBLW> 0 "register_operand")
	(plus:<VDBLW> (zero_extend:<VDBLW> 
		        (match_operand:VQW 1 "register_operand"))
		      (match_operand:<VDBLW> 2 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
    rtx temp = gen_reg_rtx (GET_MODE (operands[0]));

    emit_insn (gen_aarch64_uaddw<mode>_internal (temp, operands[2],
						 operands[1], p));
    emit_insn (gen_aarch64_uaddw2<mode> (operands[0], temp, operands[1]));
    DONE;
  }
)

(define_expand "widen_usum<mode>3"
  [(set (match_operand:<VWIDE> 0 "register_operand")
	(plus:<VWIDE> (zero_extend:<VWIDE>
		        (match_operand:VD_BHSI 1 "register_operand"))
		      (match_operand:<VWIDE> 2 "register_operand")))]
  "TARGET_SIMD"
{
  emit_insn (gen_aarch64_uaddw<mode> (operands[0], operands[2], operands[1]));
  DONE;
})

(define_insn "aarch64_<ANY_EXTEND:su>subw<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(minus:<VWIDE> (match_operand:<VWIDE> 1 "register_operand" "w")
	  (ANY_EXTEND:<VWIDE>
	    (match_operand:VD_BHSI 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su>subw\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vtype>"
  [(set_attr "type" "neon_sub_widen")]
)

(define_insn "aarch64_<ANY_EXTEND:su>subw<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(minus:<VWIDE> (match_operand:<VWIDE> 1 "register_operand" "w")
	  (ANY_EXTEND:<VWIDE>
	    (vec_select:<VHALF>
	      (match_operand:VQW 2 "register_operand" "w")
	      (match_operand:VQW 3 "vect_par_cnst_lo_half" "")))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su>subw\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vhalftype>"
  [(set_attr "type" "neon_sub_widen")]
)

(define_insn "aarch64_<ANY_EXTEND:su>subw2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(minus:<VWIDE> (match_operand:<VWIDE> 1 "register_operand" "w")
	  (ANY_EXTEND:<VWIDE>
	    (vec_select:<VHALF>
	      (match_operand:VQW 2 "register_operand" "w")
	      (match_operand:VQW 3 "vect_par_cnst_hi_half" "")))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su>subw2\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vtype>"
  [(set_attr "type" "neon_sub_widen")]
)

(define_insn "aarch64_<ANY_EXTEND:su>addw<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (ANY_EXTEND:<VWIDE> (match_operand:VD_BHSI 2 "register_operand" "w"))
	  (match_operand:<VWIDE> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su>addw\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vtype>"
  [(set_attr "type" "neon_add_widen")]
)

(define_insn "aarch64_<ANY_EXTEND:su>addw<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (ANY_EXTEND:<VWIDE>
	    (vec_select:<VHALF>
	      (match_operand:VQW 2 "register_operand" "w")
	      (match_operand:VQW 3 "vect_par_cnst_lo_half" "")))
	  (match_operand:<VWIDE> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su>addw\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vhalftype>"
  [(set_attr "type" "neon_add_widen")]
)

(define_insn "aarch64_<ANY_EXTEND:su>addw2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(plus:<VWIDE>
	  (ANY_EXTEND:<VWIDE>
	    (vec_select:<VHALF>
	      (match_operand:VQW 2 "register_operand" "w")
	      (match_operand:VQW 3 "vect_par_cnst_hi_half" "")))
	  (match_operand:<VWIDE> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su>addw2\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vtype>"
  [(set_attr "type" "neon_add_widen")]
)

(define_expand "aarch64_saddw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_saddw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

(define_expand "aarch64_uaddw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_uaddw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})


(define_expand "aarch64_ssubw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_ssubw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

(define_expand "aarch64_usubw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:<VWIDE> 1 "register_operand")
   (match_operand:VQW 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_usubw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

;; <su><r>h<addsub>.

(define_expand "<u>avg<mode>3_floor"
  [(set (match_operand:VDQ_BHSI 0 "register_operand")
	(unspec:VDQ_BHSI [(match_operand:VDQ_BHSI 1 "register_operand")
			  (match_operand:VDQ_BHSI 2 "register_operand")]
			 HADD))]
  "TARGET_SIMD"
)

(define_expand "<u>avg<mode>3_ceil"
  [(set (match_operand:VDQ_BHSI 0 "register_operand")
	(unspec:VDQ_BHSI [(match_operand:VDQ_BHSI 1 "register_operand")
			  (match_operand:VDQ_BHSI 2 "register_operand")]
			 RHADD))]
  "TARGET_SIMD"
)

(define_insn "aarch64_<sur>h<addsub><mode>"
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
        (unspec:VDQ_BHSI [(match_operand:VDQ_BHSI 1 "register_operand" "w")
		      (match_operand:VDQ_BHSI 2 "register_operand" "w")]
		     HADDSUB))]
  "TARGET_SIMD"
  "<sur>h<addsub>\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<addsub>_halve<q>")]
)

;; <r><addsub>hn<q>.

(define_insn "aarch64_<sur><addsub>hn<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand" "w")
			      (match_operand:VQN 2 "register_operand" "w")]
			     ADDSUBHN)
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "<sur><addsub>hn\\t%0.<Vntype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<addsub>_halve_narrow_q")]
)

(define_insn "aarch64_<sur><addsub>hn<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")
	  (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand" "w")
			      (match_operand:VQN 2 "register_operand" "w")]
			     ADDSUBHN)))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "<sur><addsub>hn\\t%0.<Vntype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<addsub>_halve_narrow_q")]
)

(define_expand "aarch64_<sur><addsub>hn<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand")
	(unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand")
			    (match_operand:VQN 2 "register_operand")]
			   ADDSUBHN))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (<VNARROWQ2>mode);
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_<sur><addsub>hn<mode>_insn_be (tmp, operands[1],
				operands[2], CONST0_RTX (<VNARROWQ>mode)));
    else
      emit_insn (gen_aarch64_<sur><addsub>hn<mode>_insn_le (tmp, operands[1],
				operands[2], CONST0_RTX (<VNARROWQ>mode)));

    /* The intrinsic expects a narrow result, so emit a subreg that will get
       optimized away as appropriate.  */
    emit_move_insn (operands[0], lowpart_subreg (<VNARROWQ>mode, tmp,
						 <VNARROWQ2>mode));
    DONE;
  }
)

(define_insn "aarch64_<sur><addsub>hn2<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (unspec:<VNARROWQ> [(match_operand:VQN 2 "register_operand" "w")
			      (match_operand:VQN 3 "register_operand" "w")]
			     ADDSUBHN)))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "<sur><addsub>hn2\\t%0.<V2ntype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_<addsub>_halve_narrow_q")]
)

(define_insn "aarch64_<sur><addsub>hn2<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (unspec:<VNARROWQ> [(match_operand:VQN 2 "register_operand" "w")
			      (match_operand:VQN 3 "register_operand" "w")]
			     ADDSUBHN)
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "<sur><addsub>hn2\\t%0.<V2ntype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_<addsub>_halve_narrow_q")]
)

(define_expand "aarch64_<sur><addsub>hn2<mode>"
  [(match_operand:<VNARROWQ2> 0 "register_operand")
   (match_operand:<VNARROWQ> 1 "register_operand")
   (unspec [(match_operand:VQN 2 "register_operand")
	    (match_operand:VQN 3 "register_operand")]
	   ADDSUBHN)]
  "TARGET_SIMD"
  {
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_<sur><addsub>hn2<mode>_insn_be (operands[0],
				operands[1], operands[2], operands[3]));
    else
      emit_insn (gen_aarch64_<sur><addsub>hn2<mode>_insn_le (operands[0],
				operands[1], operands[2], operands[3]));
    DONE;
  }
)

;; Optimize ((a + b) >> n) + c where n is half the bitsize of the vector
(define_insn_and_split "*bitmask_shift_plus<mode>"
  [(set (match_operand:VQN 0 "register_operand" "=&w")
	(plus:VQN
	  (lshiftrt:VQN
	    (plus:VQN (match_operand:VQN 1 "register_operand" "w")
		      (match_operand:VQN 2 "register_operand" "w"))
	    (match_operand:VQN 3 "aarch64_simd_shift_imm_vec_exact_top" ""))
	  (match_operand:VQN 4 "register_operand" "w")))]
  "TARGET_SIMD"
  "#"
  "&& true"
  [(const_int 0)]
{
  rtx tmp;
  if (can_create_pseudo_p ())
    tmp = gen_reg_rtx (<VNARROWQ>mode);
  else
    tmp = gen_rtx_REG (<VNARROWQ>mode, REGNO (operands[0]));
  emit_insn (gen_aarch64_addhn<mode> (tmp, operands[1], operands[2]));
  emit_insn (gen_aarch64_uaddw<Vnarrowq> (operands[0], operands[4], tmp));
  DONE;
})

;; pmul.

(define_insn "aarch64_pmul<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
        (unspec:VB [(match_operand:VB 1 "register_operand" "w")
		    (match_operand:VB 2 "register_operand" "w")]
		   UNSPEC_PMUL))]
 "TARGET_SIMD"
 "pmul\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_mul_<Vetype><q>")]
)

(define_insn "aarch64_pmullv8qi"
  [(set (match_operand:V8HI 0 "register_operand" "=w")
        (unspec:V8HI [(match_operand:V8QI 1 "register_operand" "w")
		      (match_operand:V8QI 2 "register_operand" "w")]
		     UNSPEC_PMULL))]
 "TARGET_SIMD"
 "pmull\\t%0.8h, %1.8b, %2.8b"
  [(set_attr "type" "neon_mul_b_long")]
)

(define_insn "aarch64_pmull_hiv16qi_insn"
  [(set (match_operand:V8HI 0 "register_operand" "=w")
	(unspec:V8HI
	  [(vec_select:V8QI
	     (match_operand:V16QI 1 "register_operand" "w")
	     (match_operand:V16QI 3 "vect_par_cnst_hi_half" ""))
	   (vec_select:V8QI
	     (match_operand:V16QI 2 "register_operand" "w")
	     (match_dup 3))]
	  UNSPEC_PMULL))]
 "TARGET_SIMD"
 "pmull2\\t%0.8h, %1.16b, %2.16b"
  [(set_attr "type" "neon_mul_b_long")]
)

(define_expand "aarch64_pmull_hiv16qi"
  [(match_operand:V8HI 0 "register_operand")
   (match_operand:V16QI 1 "register_operand")
   (match_operand:V16QI 2 "register_operand")]
 "TARGET_SIMD"
 {
   rtx p = aarch64_simd_vect_par_cnst_half (V16QImode, 16, true);
   emit_insn (gen_aarch64_pmull_hiv16qi_insn (operands[0], operands[1],
					      operands[2], p));
   DONE;
 }
)

;; fmulx.

(define_insn "aarch64_fmulx<mode>"
  [(set (match_operand:VHSDF_HSDF 0 "register_operand" "=w")
	(unspec:VHSDF_HSDF
	  [(match_operand:VHSDF_HSDF 1 "register_operand" "w")
	   (match_operand:VHSDF_HSDF 2 "register_operand" "w")]
	   UNSPEC_FMULX))]
 "TARGET_SIMD"
 "fmulx\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
 [(set_attr "type" "neon_fp_mul_<stype>")]
)

;; vmulxq_lane_f32, and vmulx_laneq_f32

(define_insn "*aarch64_mulx_elt_<vswap_width_name><mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
	(unspec:VDQSF
	 [(match_operand:VDQSF 1 "register_operand" "w")
	  (vec_duplicate:VDQSF
	   (vec_select:<VEL>
	    (match_operand:<VSWAP_WIDTH> 2 "register_operand" "w")
	    (parallel [(match_operand:SI 3 "immediate_operand" "i")])))]
	 UNSPEC_FMULX))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VSWAP_WIDTH>mode, INTVAL (operands[3]));
    return "fmulx\t%<v>0<Vmtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_fp_mul_<Vetype>_scalar<q>")]
)

;; vmulxq_laneq_f32, vmulxq_laneq_f64, vmulx_lane_f32

(define_insn "*aarch64_mulx_elt<mode>"
  [(set (match_operand:VDQF 0 "register_operand" "=w")
	(unspec:VDQF
	 [(match_operand:VDQF 1 "register_operand" "w")
	  (vec_duplicate:VDQF
	   (vec_select:<VEL>
	    (match_operand:VDQF 2 "register_operand" "w")
	    (parallel [(match_operand:SI 3 "immediate_operand" "i")])))]
	 UNSPEC_FMULX))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[3]));
    return "fmulx\t%<v>0<Vmtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_fp_mul_<Vetype><q>")]
)

;; vmulxq_lane

(define_insn "*aarch64_mulx_elt_from_dup<mode>"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(unspec:VHSDF
	 [(match_operand:VHSDF 1 "register_operand" "w")
	  (vec_duplicate:VHSDF
	    (match_operand:<VEL> 2 "register_operand" "<h_con>"))]
	 UNSPEC_FMULX))]
  "TARGET_SIMD"
  "fmulx\t%0.<Vtype>, %1.<Vtype>, %2.<Vetype>[0]";
  [(set_attr "type" "neon<fp>_mul_<stype>_scalar<q>")]
)

;; vmulxs_lane_f32, vmulxs_laneq_f32
;; vmulxd_lane_f64 ==  vmulx_lane_f64
;; vmulxd_laneq_f64 == vmulx_laneq_f64

(define_insn "*aarch64_vgetfmulx<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL>
	 [(match_operand:<VEL> 1 "register_operand" "w")
	  (vec_select:<VEL>
	   (match_operand:VDQF 2 "register_operand" "w")
	    (parallel [(match_operand:SI 3 "immediate_operand" "i")]))]
	 UNSPEC_FMULX))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[3]));
    return "fmulx\t%<Vetype>0, %<Vetype>1, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "fmul<Vetype>")]
)
;; <su>q<addsub>

(define_insn "aarch64_<su_optab>q<addsub><mode>"
  [(set (match_operand:VSDQ_I 0 "register_operand" "=w")
	(BINQOPS:VSDQ_I (match_operand:VSDQ_I 1 "register_operand" "w")
			(match_operand:VSDQ_I 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "<su_optab>q<addsub>\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_q<addsub><q>")]
)

;; suqadd and usqadd

(define_insn "aarch64_<sur>qadd<mode>"
  [(set (match_operand:VSDQ_I 0 "register_operand" "=w")
	(unspec:VSDQ_I [(match_operand:VSDQ_I 1 "register_operand" "0")
			(match_operand:VSDQ_I 2 "register_operand" "w")]
		       USSUQADD))]
  "TARGET_SIMD"
  "<sur>qadd\\t%<v>0<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_qadd<q>")]
)

;; sqmovn and uqmovn

(define_insn "aarch64_<su>qmovn<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
	(SAT_TRUNC:<VNARROWQ>
	  (match_operand:SD_HSDI 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "<su>qxtn\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_<su>qmovn<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (SAT_TRUNC:<VNARROWQ>
	    (match_operand:VQN 1 "register_operand" "w"))
	  (match_operand:<VNARROWQ> 2 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "<su>qxtn\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_<su>qmovn<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 2 "aarch64_simd_or_scalar_imm_zero")
	  (SAT_TRUNC:<VNARROWQ>
	    (match_operand:VQN 1 "register_operand" "w"))))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "<su>qxtn\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_expand "aarch64_<su>qmovn<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand")
	(SAT_TRUNC:<VNARROWQ>
	  (match_operand:VQN 1 "register_operand")))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (<VNARROWQ2>mode);
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_<su>qmovn<mode>_insn_be (tmp, operands[1],
				CONST0_RTX (<VNARROWQ>mode)));
    else
      emit_insn (gen_aarch64_<su>qmovn<mode>_insn_le (tmp, operands[1],
				CONST0_RTX (<VNARROWQ>mode)));

    /* The intrinsic expects a narrow result, so emit a subreg that will get
       optimized away as appropriate.  */
    emit_move_insn (operands[0], lowpart_subreg (<VNARROWQ>mode, tmp,
						 <VNARROWQ2>mode));
    DONE;
  }
)

(define_insn "aarch64_<su>qxtn2<mode>_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (SAT_TRUNC:<VNARROWQ>
	    (match_operand:VQN 2 "register_operand" "w"))))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "<su>qxtn2\\t%0.<V2ntype>, %2.<Vtype>"
   [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_<su>qxtn2<mode>_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (SAT_TRUNC:<VNARROWQ>
	    (match_operand:VQN 2 "register_operand" "w"))
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "<su>qxtn2\\t%0.<V2ntype>, %2.<Vtype>"
   [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_expand "aarch64_<su>qxtn2<mode>"
  [(match_operand:<VNARROWQ2> 0 "register_operand")
   (match_operand:<VNARROWQ> 1 "register_operand")
   (SAT_TRUNC:<VNARROWQ>
     (match_operand:VQN 2 "register_operand"))]
  "TARGET_SIMD"
  {
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_<su>qxtn2<mode>_be (operands[0], operands[1],
						 operands[2]));
    else
      emit_insn (gen_aarch64_<su>qxtn2<mode>_le (operands[0], operands[1],
						 operands[2]));
    DONE;
  }
)

;; sqmovun

(define_insn "aarch64_sqmovun<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
	(unspec:<VNARROWQ> [(match_operand:SD_HSDI 1 "register_operand" "w")]
			   UNSPEC_SQXTUN))]
   "TARGET_SIMD"
   "sqxtun\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
   [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_sqmovun<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand" "w")]
			     UNSPEC_SQXTUN)
	  (match_operand:<VNARROWQ> 2 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "sqxtun\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_sqmovun<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 2 "aarch64_simd_or_scalar_imm_zero")
	  (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand" "w")]
			     UNSPEC_SQXTUN)))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "sqxtun\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_expand "aarch64_sqmovun<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand")
	(unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand")]
			   UNSPEC_SQXTUN))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (<VNARROWQ2>mode);
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_sqmovun<mode>_insn_be (tmp, operands[1],
				CONST0_RTX (<VNARROWQ>mode)));
    else
      emit_insn (gen_aarch64_sqmovun<mode>_insn_le (tmp, operands[1],
				CONST0_RTX (<VNARROWQ>mode)));

    /* The intrinsic expects a narrow result, so emit a subreg that will get
       optimized away as appropriate.  */
    emit_move_insn (operands[0], lowpart_subreg (<VNARROWQ>mode, tmp,
						 <VNARROWQ2>mode));
    DONE;
  }
)

(define_insn "aarch64_sqxtun2<mode>_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (unspec:<VNARROWQ>
	    [(match_operand:VQN 2 "register_operand" "w")] UNSPEC_SQXTUN)))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "sqxtun2\\t%0.<V2ntype>, %2.<Vtype>"
   [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_sqxtun2<mode>_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (unspec:<VNARROWQ>
	    [(match_operand:VQN 2 "register_operand" "w")] UNSPEC_SQXTUN)
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "sqxtun2\\t%0.<V2ntype>, %2.<Vtype>"
   [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_expand "aarch64_sqxtun2<mode>"
  [(match_operand:<VNARROWQ2> 0 "register_operand")
   (match_operand:<VNARROWQ> 1 "register_operand")
   (unspec:<VNARROWQ>
     [(match_operand:VQN 2 "register_operand")] UNSPEC_SQXTUN)]
  "TARGET_SIMD"
  {
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_sqxtun2<mode>_be (operands[0], operands[1],
					      operands[2]));
    else
      emit_insn (gen_aarch64_sqxtun2<mode>_le (operands[0], operands[1],
					       operands[2]));
    DONE;
  }
)

;; <su>q<absneg>

(define_insn "aarch64_s<optab><mode>"
  [(set (match_operand:VSDQ_I 0 "register_operand" "=w")
	(UNQOPS:VSDQ_I
	  (match_operand:VSDQ_I 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "s<optab>\\t%<v>0<Vmtype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_<optab><q>")]
)

;; sq<r>dmulh.

(define_insn "aarch64_sq<r>dmulh<mode>"
  [(set (match_operand:VSDQ_HSI 0 "register_operand" "=w")
	(unspec:VSDQ_HSI
	  [(match_operand:VSDQ_HSI 1 "register_operand" "w")
	   (match_operand:VSDQ_HSI 2 "register_operand" "w")]
	 VQDMULH))]
  "TARGET_SIMD"
  "sq<r>dmulh\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_sat_mul_<Vetype><q>")]
)

(define_insn "aarch64_sq<r>dmulh_n<mode>"
  [(set (match_operand:VDQHS 0 "register_operand" "=w")
	(unspec:VDQHS
	  [(match_operand:VDQHS 1 "register_operand" "w")
	   (vec_duplicate:VDQHS
	     (match_operand:<VEL> 2 "register_operand" "<h_con>"))]
	 VQDMULH))]
  "TARGET_SIMD"
  "sq<r>dmulh\\t%0.<Vtype>, %1.<Vtype>, %2.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar<q>")]
)

;; sq<r>dmulh_lane

(define_insn "aarch64_sq<r>dmulh_lane<mode>"
  [(set (match_operand:VDQHS 0 "register_operand" "=w")
        (unspec:VDQHS
	  [(match_operand:VDQHS 1 "register_operand" "w")
           (vec_select:<VEL>
             (match_operand:<VCOND> 2 "register_operand" "<vwx>")
             (parallel [(match_operand:SI 3 "immediate_operand" "i")]))]
	 VQDMULH))]
  "TARGET_SIMD"
  "*
   operands[3] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[3]));
   return \"sq<r>dmulh\\t%0.<Vtype>, %1.<Vtype>, %2.<Vetype>[%3]\";"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar<q>")]
)

(define_insn "aarch64_sq<r>dmulh_laneq<mode>"
  [(set (match_operand:VDQHS 0 "register_operand" "=w")
        (unspec:VDQHS
	  [(match_operand:VDQHS 1 "register_operand" "w")
           (vec_select:<VEL>
             (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
             (parallel [(match_operand:SI 3 "immediate_operand" "i")]))]
	 VQDMULH))]
  "TARGET_SIMD"
  "*
   operands[3] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[3]));
   return \"sq<r>dmulh\\t%0.<Vtype>, %1.<Vtype>, %2.<Vetype>[%3]\";"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar<q>")]
)

(define_insn "aarch64_sq<r>dmulh_lane<mode>"
  [(set (match_operand:SD_HSI 0 "register_operand" "=w")
        (unspec:SD_HSI
	  [(match_operand:SD_HSI 1 "register_operand" "w")
           (vec_select:<VEL>
             (match_operand:<VCOND> 2 "register_operand" "<vwx>")
             (parallel [(match_operand:SI 3 "immediate_operand" "i")]))]
	 VQDMULH))]
  "TARGET_SIMD"
  "*
   operands[3] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[3]));
   return \"sq<r>dmulh\\t%<v>0, %<v>1, %2.<v>[%3]\";"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar<q>")]
)

(define_insn "aarch64_sq<r>dmulh_laneq<mode>"
  [(set (match_operand:SD_HSI 0 "register_operand" "=w")
        (unspec:SD_HSI
	  [(match_operand:SD_HSI 1 "register_operand" "w")
           (vec_select:<VEL>
             (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
             (parallel [(match_operand:SI 3 "immediate_operand" "i")]))]
	 VQDMULH))]
  "TARGET_SIMD"
  "*
   operands[3] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[3]));
   return \"sq<r>dmulh\\t%<v>0, %<v>1, %2.<v>[%3]\";"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar<q>")]
)

;; sqrdml[as]h.

(define_insn "aarch64_sqrdml<SQRDMLH_AS:rdma_as>h<mode>"
  [(set (match_operand:VSDQ_HSI 0 "register_operand" "=w")
	(unspec:VSDQ_HSI
	  [(match_operand:VSDQ_HSI 1 "register_operand" "0")
	   (match_operand:VSDQ_HSI 2 "register_operand" "w")
	   (match_operand:VSDQ_HSI 3 "register_operand" "w")]
	  SQRDMLH_AS))]
   "TARGET_SIMD_RDMA"
   "sqrdml<SQRDMLH_AS:rdma_as>h\\t%<v>0<Vmtype>, %<v>2<Vmtype>, %<v>3<Vmtype>"
   [(set_attr "type" "neon_sat_mla_<Vetype>_long")]
)

;; sqrdml[as]h_lane.

(define_insn "aarch64_sqrdml<SQRDMLH_AS:rdma_as>h_lane<mode>"
  [(set (match_operand:VDQHS 0 "register_operand" "=w")
	(unspec:VDQHS
	  [(match_operand:VDQHS 1 "register_operand" "0")
	   (match_operand:VDQHS 2 "register_operand" "w")
	   (vec_select:<VEL>
	     (match_operand:<VCOND> 3 "register_operand" "<vwx>")
	     (parallel [(match_operand:SI 4 "immediate_operand" "i")]))]
	  SQRDMLH_AS))]
   "TARGET_SIMD_RDMA"
   {
     operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
     return
      "sqrdml<SQRDMLH_AS:rdma_as>h\\t%0.<Vtype>, %2.<Vtype>, %3.<Vetype>[%4]";
   }
   [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqrdml<SQRDMLH_AS:rdma_as>h_lane<mode>"
  [(set (match_operand:SD_HSI 0 "register_operand" "=w")
	(unspec:SD_HSI
	  [(match_operand:SD_HSI 1 "register_operand" "0")
	   (match_operand:SD_HSI 2 "register_operand" "w")
	   (vec_select:<VEL>
	     (match_operand:<VCOND> 3 "register_operand" "<vwx>")
	     (parallel [(match_operand:SI 4 "immediate_operand" "i")]))]
	  SQRDMLH_AS))]
   "TARGET_SIMD_RDMA"
   {
     operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
     return
      "sqrdml<SQRDMLH_AS:rdma_as>h\\t%<v>0, %<v>2, %3.<Vetype>[%4]";
   }
   [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

;; sqrdml[as]h_laneq.

(define_insn "aarch64_sqrdml<SQRDMLH_AS:rdma_as>h_laneq<mode>"
  [(set (match_operand:VDQHS 0 "register_operand" "=w")
	(unspec:VDQHS
	  [(match_operand:VDQHS 1 "register_operand" "0")
	   (match_operand:VDQHS 2 "register_operand" "w")
	   (vec_select:<VEL>
	     (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
	     (parallel [(match_operand:SI 4 "immediate_operand" "i")]))]
	  SQRDMLH_AS))]
   "TARGET_SIMD_RDMA"
   {
     operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
     return
      "sqrdml<SQRDMLH_AS:rdma_as>h\\t%0.<Vtype>, %2.<Vtype>, %3.<Vetype>[%4]";
   }
   [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqrdml<SQRDMLH_AS:rdma_as>h_laneq<mode>"
  [(set (match_operand:SD_HSI 0 "register_operand" "=w")
	(unspec:SD_HSI
	  [(match_operand:SD_HSI 1 "register_operand" "0")
	   (match_operand:SD_HSI 2 "register_operand" "w")
	   (vec_select:<VEL>
	     (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
	     (parallel [(match_operand:SI 4 "immediate_operand" "i")]))]
	  SQRDMLH_AS))]
   "TARGET_SIMD_RDMA"
   {
     operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
     return
      "sqrdml<SQRDMLH_AS:rdma_as>h\\t%<v>0, %<v>2, %3.<v>[%4]";
   }
   [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

;; vqdml[sa]l

(define_insn "aarch64_sqdmlal<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		      (match_operand:VSD_HSI 2 "register_operand" "w"))
		(sign_extend:<VWIDE>
		      (match_operand:VSD_HSI 3 "register_operand" "w")))
	      (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "sqdmlal\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %<v>3<Vmtype>"
  [(set_attr "type" "neon_sat_mla_<Vetype>_long")]
)

(define_insn "aarch64_sqdmlsl<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		      (match_operand:VSD_HSI 2 "register_operand" "w"))
		(sign_extend:<VWIDE>
		      (match_operand:VSD_HSI 3 "register_operand" "w")))
	      (const_int 1))))]
  "TARGET_SIMD"
  "sqdmlsl\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %<v>3<Vmtype>"
  [(set_attr "type" "neon_sat_mla_<Vetype>_long")]
)

;; vqdml[sa]l_lane

(define_insn "aarch64_sqdmlal_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:VD_HSI 2 "register_operand" "w"))
	      (vec_duplicate:<VWIDE>
		(sign_extend:<VWIDE_S>
		  (vec_select:<VEL>
		    (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		    (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
	      ))
	    (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
    return
      "sqdmlal\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlsl_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:VD_HSI 2 "register_operand" "w"))
	      (vec_duplicate:<VWIDE>
		(sign_extend:<VWIDE_S>
		  (vec_select:<VEL>
		    (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		    (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
	      ))
	    (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
    return
      "sqdmlsl\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)


(define_insn "aarch64_sqdmlsl_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:VD_HSI 2 "register_operand" "w"))
	      (vec_duplicate:<VWIDE>
		(sign_extend:<VWIDE_S>
		  (vec_select:<VEL>
		    (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		    (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
	      ))
	    (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
    return
      "sqdmlsl\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlal_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:VD_HSI 2 "register_operand" "w"))
	      (vec_duplicate:<VWIDE>
		(sign_extend:<VWIDE_S>
		  (vec_select:<VEL>
		    (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		    (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
	      ))
	    (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
    return
      "sqdmlal\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)


(define_insn "aarch64_sqdmlal_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:SD_HSI 2 "register_operand" "w"))
	      (sign_extend:<VWIDE>
		(vec_select:<VEL>
		  (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
              )
	    (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
    return
      "sqdmlal\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlsl_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:SD_HSI 2 "register_operand" "w"))
	      (sign_extend:<VWIDE>
		(vec_select:<VEL>
		  (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
              )
	    (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
    return
      "sqdmlsl\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)


(define_insn "aarch64_sqdmlal_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:SD_HSI 2 "register_operand" "w"))
	      (sign_extend:<VWIDE>
		(vec_select:<VEL>
		  (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
              )
	    (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
    return
      "sqdmlal\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlsl_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:SD_HSI 2 "register_operand" "w"))
	      (sign_extend:<VWIDE>
		(vec_select:<VEL>
		  (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		  (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
              )
	    (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
    return
      "sqdmlsl\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

;; vqdml[sa]l_n

(define_insn "aarch64_sqdmlsl_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		      (match_operand:VD_HSI 2 "register_operand" "w"))
		(vec_duplicate:<VWIDE>
		  (sign_extend:<VWIDE_S>
		    (match_operand:<VEL> 3 "register_operand" "<vwx>"))))
	      (const_int 1))))]
  "TARGET_SIMD"
  "sqdmlsl\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlal_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		      (match_operand:VD_HSI 2 "register_operand" "w"))
		(vec_duplicate:<VWIDE>
		  (sign_extend:<VWIDE_S>
		    (match_operand:<VEL> 3 "register_operand" "<vwx>"))))
	      (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "sqdmlal\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)


;; sqdml[as]l2

(define_insn "aarch64_sqdmlal2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_plus:<VWIDE>
         (ss_ashift:<VWIDE>
             (mult:<VWIDE>
               (sign_extend:<VWIDE>
                 (vec_select:<VHALF>
                     (match_operand:VQ_HSI 2 "register_operand" "w")
                     (match_operand:VQ_HSI 4 "vect_par_cnst_hi_half" "")))
               (sign_extend:<VWIDE>
                 (vec_select:<VHALF>
                     (match_operand:VQ_HSI 3 "register_operand" "w")
                     (match_dup 4))))
             (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "sqdmlal2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %<v>3<Vmtype>"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlsl2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
         (match_operand:<VWIDE> 1 "register_operand" "0")
         (ss_ashift:<VWIDE>
             (mult:<VWIDE>
               (sign_extend:<VWIDE>
                 (vec_select:<VHALF>
                     (match_operand:VQ_HSI 2 "register_operand" "w")
                     (match_operand:VQ_HSI 4 "vect_par_cnst_hi_half" "")))
               (sign_extend:<VWIDE>
                 (vec_select:<VHALF>
                     (match_operand:VQ_HSI 3 "register_operand" "w")
                     (match_dup 4))))
             (const_int 1))))]
  "TARGET_SIMD"
  "sqdmlsl2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %<v>3<Vmtype>"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdml<SBINQOPS:as>l2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (SBINQOPS:<VWIDE>
     (match_operand:<VWIDE> 1 "register_operand")
     (match_dup 1))
   (match_operand:VQ_HSI 2 "register_operand")
   (match_operand:VQ_HSI 3 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdml<SBINQOPS:as>l2<mode>_internal (operands[0],
						operands[1], operands[2],
						operands[3], p));
  DONE;
})

;; vqdml[sa]l2_lane

(define_insn "aarch64_sqdmlsl2_lane<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		  (vec_select:<VHALF>
		    (match_operand:VQ_HSI 2 "register_operand" "w")
		    (match_operand:VQ_HSI 5 "vect_par_cnst_hi_half" "")))
		(vec_duplicate:<VWIDE>
		  (sign_extend:<VWIDE_S>
		    (vec_select:<VEL>
		      (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		      (parallel [(match_operand:SI 4 "immediate_operand" "i")])
		    ))))
	      (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
    return
     "sqdmlsl2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlal2_lane<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		  (vec_select:<VHALF>
		    (match_operand:VQ_HSI 2 "register_operand" "w")
		    (match_operand:VQ_HSI 5 "vect_par_cnst_hi_half" "")))
		(vec_duplicate:<VWIDE>
		  (sign_extend:<VWIDE_S>
		    (vec_select:<VEL>
		      (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		      (parallel [(match_operand:SI 4 "immediate_operand" "i")])
		    ))))
	      (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[4]));
    return
     "sqdmlal2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlsl2_laneq<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		  (vec_select:<VHALF>
		    (match_operand:VQ_HSI 2 "register_operand" "w")
		    (match_operand:VQ_HSI 5 "vect_par_cnst_hi_half" "")))
		(vec_duplicate:<VWIDE>
		  (sign_extend:<VWIDE_S>
		    (vec_select:<VEL>
		      (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		      (parallel [(match_operand:SI 4 "immediate_operand" "i")])
		    ))))
	      (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
    return
     "sqdmlsl2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlal2_laneq<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		  (vec_select:<VHALF>
		    (match_operand:VQ_HSI 2 "register_operand" "w")
		    (match_operand:VQ_HSI 5 "vect_par_cnst_hi_half" "")))
		(vec_duplicate:<VWIDE>
		  (sign_extend:<VWIDE_S>
		    (vec_select:<VEL>
		      (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		      (parallel [(match_operand:SI 4 "immediate_operand" "i")])
		    ))))
	      (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  {
    operands[4] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[4]));
    return
     "sqdmlal2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdml<SBINQOPS:as>l2_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (SBINQOPS:<VWIDE>
     (match_operand:<VWIDE> 1 "register_operand")
     (match_dup 1))
   (match_operand:VQ_HSI 2 "register_operand")
   (match_operand:<VCOND> 3 "register_operand")
   (match_operand:SI 4 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdml<SBINQOPS:as>l2_lane<mode>_internal (operands[0],
						operands[1], operands[2],
						operands[3], operands[4], p));
  DONE;
})

(define_expand "aarch64_sqdml<SBINQOPS:as>l2_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (SBINQOPS:<VWIDE>
     (match_operand:<VWIDE> 1 "register_operand")
     (match_dup 1))
   (match_operand:VQ_HSI 2 "register_operand")
   (match_operand:<VCONQ> 3 "register_operand")
   (match_operand:SI 4 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdml<SBINQOPS:as>l2_laneq<mode>_internal (operands[0],
						operands[1], operands[2],
						operands[3], operands[4], p));
  DONE;
})

(define_insn "aarch64_sqdmlsl2_n<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_minus:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(vec_select:<VHALF>
		  (match_operand:VQ_HSI 2 "register_operand" "w")
		  (match_operand:VQ_HSI 4 "vect_par_cnst_hi_half" "")))
	      (vec_duplicate:<VWIDE>
		(sign_extend:<VWIDE_S>
		  (match_operand:<VEL> 3 "register_operand" "<vwx>"))))
	    (const_int 1))))]
  "TARGET_SIMD"
  "sqdmlsl2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmlal2_n<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_plus:<VWIDE>
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(vec_select:<VHALF>
		  (match_operand:VQ_HSI 2 "register_operand" "w")
		  (match_operand:VQ_HSI 4 "vect_par_cnst_hi_half" "")))
	      (vec_duplicate:<VWIDE>
		(sign_extend:<VWIDE_S>
		  (match_operand:<VEL> 3 "register_operand" "<vwx>"))))
	    (const_int 1))
	  (match_operand:<VWIDE> 1 "register_operand" "0")))]
  "TARGET_SIMD"
  "sqdmlal2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdml<SBINQOPS:as>l2_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (SBINQOPS:<VWIDE>
     (match_operand:<VWIDE> 1 "register_operand")
     (match_dup 1))
   (match_operand:VQ_HSI 2 "register_operand")
   (match_operand:<VEL> 3 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdml<SBINQOPS:as>l2_n<mode>_internal (operands[0],
						operands[1], operands[2],
						operands[3], p));
  DONE;
})

;; vqdmull

(define_insn "aarch64_sqdmull<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		     (match_operand:VSD_HSI 1 "register_operand" "w"))
	       (sign_extend:<VWIDE>
		     (match_operand:VSD_HSI 2 "register_operand" "w")))
	     (const_int 1)))]
  "TARGET_SIMD"
  "sqdmull\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_sat_mul_<Vetype>_long")]
)

;; vqdmull_lane

(define_insn "aarch64_sqdmull_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (match_operand:VD_HSI 1 "register_operand" "w"))
	       (vec_duplicate:<VWIDE>
		 (sign_extend:<VWIDE_S>
		   (vec_select:<VEL>
		     (match_operand:<VCOND> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[3]));
    return "sqdmull\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmull_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (match_operand:VD_HSI 1 "register_operand" "w"))
	       (vec_duplicate:<VWIDE>
		 (sign_extend:<VWIDE_S>
		   (vec_select:<VEL>
		     (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[3]));
    return "sqdmull\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmull_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (match_operand:SD_HSI 1 "register_operand" "w"))
	       (sign_extend:<VWIDE>
                 (vec_select:<VEL>
		   (match_operand:<VCOND> 2 "register_operand" "<vwx>")
		   (parallel [(match_operand:SI 3 "immediate_operand" "i")]))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[3]));
    return "sqdmull\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmull_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (match_operand:SD_HSI 1 "register_operand" "w"))
	       (sign_extend:<VWIDE>
                 (vec_select:<VEL>
		   (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
		   (parallel [(match_operand:SI 3 "immediate_operand" "i")]))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[3]));
    return "sqdmull\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

;; vqdmull_n

(define_insn "aarch64_sqdmull_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (match_operand:VD_HSI 1 "register_operand" "w"))
	       (vec_duplicate:<VWIDE>
		 (sign_extend:<VWIDE_S>
		   (match_operand:<VEL> 2 "register_operand" "<vwx>")))
	       )
	     (const_int 1)))]
  "TARGET_SIMD"
  "sqdmull\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

;; vqdmull2

(define_insn "aarch64_sqdmull2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (vec_select:<VHALF>
                   (match_operand:VQ_HSI 1 "register_operand" "w")
                   (match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	       (sign_extend:<VWIDE>
		 (vec_select:<VHALF>
                   (match_operand:VQ_HSI 2 "register_operand" "w")
                   (match_dup 3)))
	       )
	     (const_int 1)))]
  "TARGET_SIMD"
  "sqdmull2\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmull2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQ_HSI 1 "register_operand")
   (match_operand:VQ_HSI 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdmull2<mode>_internal (operands[0], operands[1],
						  operands[2], p));
  DONE;
})

;; vqdmull2_lane

(define_insn "aarch64_sqdmull2_lane<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (vec_select:<VHALF>
		   (match_operand:VQ_HSI 1 "register_operand" "w")
		   (match_operand:VQ_HSI 4 "vect_par_cnst_hi_half" "")))
	       (vec_duplicate:<VWIDE>
		 (sign_extend:<VWIDE_S>
		   (vec_select:<VEL>
		     (match_operand:<VCOND> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCOND>mode, INTVAL (operands[3]));
    return "sqdmull2\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdmull2_laneq<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (vec_select:<VHALF>
		   (match_operand:VQ_HSI 1 "register_operand" "w")
		   (match_operand:VQ_HSI 4 "vect_par_cnst_hi_half" "")))
	       (vec_duplicate:<VWIDE>
		 (sign_extend:<VWIDE_S>
		   (vec_select:<VEL>
		     (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VCONQ>mode, INTVAL (operands[3]));
    return "sqdmull2\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmull2_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQ_HSI 1 "register_operand")
   (match_operand:<VCOND> 2 "register_operand")
   (match_operand:SI 3 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdmull2_lane<mode>_internal (operands[0], operands[1],
						       operands[2], operands[3],
						       p));
  DONE;
})

(define_expand "aarch64_sqdmull2_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQ_HSI 1 "register_operand")
   (match_operand:<VCONQ> 2 "register_operand")
   (match_operand:SI 3 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdmull2_laneq<mode>_internal (operands[0], operands[1],
						       operands[2], operands[3],
						       p));
  DONE;
})

;; vqdmull2_n

(define_insn "aarch64_sqdmull2_n<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(ss_ashift:<VWIDE>
	     (mult:<VWIDE>
	       (sign_extend:<VWIDE>
		 (vec_select:<VHALF>
		   (match_operand:VQ_HSI 1 "register_operand" "w")
		   (match_operand:VQ_HSI 3 "vect_par_cnst_hi_half" "")))
	       (vec_duplicate:<VWIDE>
		 (sign_extend:<VWIDE_S>
		   (match_operand:<VEL> 2 "register_operand" "<vwx>")))
	       )
	     (const_int 1)))]
  "TARGET_SIMD"
  "sqdmull2\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmull2_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (match_operand:VQ_HSI 1 "register_operand")
   (match_operand:<VEL> 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
  emit_insn (gen_aarch64_sqdmull2_n<mode>_internal (operands[0], operands[1],
						    operands[2], p));
  DONE;
})

;; vshl

(define_insn "aarch64_<sur>shl<mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand" "=w")
        (unspec:VSDQ_I_DI
	  [(match_operand:VSDQ_I_DI 1 "register_operand" "w")
           (match_operand:VSDQ_I_DI 2 "register_operand" "w")]
         VSHL))]
  "TARGET_SIMD"
  "<sur>shl\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>";
  [(set_attr "type" "neon_shift_reg<q>")]
)


;; vqshl

(define_insn "aarch64_<sur>q<r>shl<mode>"
  [(set (match_operand:VSDQ_I 0 "register_operand" "=w")
        (unspec:VSDQ_I
	  [(match_operand:VSDQ_I 1 "register_operand" "w")
           (match_operand:VSDQ_I 2 "register_operand" "w")]
         VQSHL))]
  "TARGET_SIMD"
  "<sur>q<r>shl\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>";
  [(set_attr "type" "neon_sat_shift_reg<q>")]
)

(define_expand "vec_widen_<sur>shiftl_lo_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:VQW 1 "register_operand" "w")
			 (match_operand:SI 2
			   "aarch64_simd_shift_imm_bitsize_<ve_mode>" "i")]
			 VSHLL))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, false);
    emit_insn (gen_aarch64_<sur>shll<mode>_internal (operands[0], operands[1],
						     p, operands[2]));
    DONE;
  }
)

(define_expand "vec_widen_<sur>shiftl_hi_<mode>"
   [(set (match_operand:<VWIDE> 0 "register_operand")
	(unspec:<VWIDE> [(match_operand:VQW 1 "register_operand" "w")
			 (match_operand:SI 2
			   "immediate_operand" "i")]
			  VSHLL))]
   "TARGET_SIMD"
   {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, <nunits>, true);
    emit_insn (gen_aarch64_<sur>shll2<mode>_internal (operands[0], operands[1],
						      p, operands[2]));
    DONE;
   }
)

;; vshll_n

(define_insn "aarch64_<sur>shll<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(vec_select:<VHALF>
			    (match_operand:VQW 1 "register_operand" "w")
			    (match_operand:VQW 2 "vect_par_cnst_lo_half" ""))
			 (match_operand:SI 3
			   "aarch64_simd_shift_imm_bitsize_<ve_mode>" "i")]
			 VSHLL))]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[3]) == GET_MODE_UNIT_BITSIZE (<MODE>mode))
      return "shll\\t%0.<Vwtype>, %1.<Vhalftype>, %3";
    else
      return "<sur>shll\\t%0.<Vwtype>, %1.<Vhalftype>, %3";
  }
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "aarch64_<sur>shll2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(vec_select:<VHALF>
			    (match_operand:VQW 1 "register_operand" "w")
			    (match_operand:VQW 2 "vect_par_cnst_hi_half" ""))
			 (match_operand:SI 3
			   "aarch64_simd_shift_imm_bitsize_<ve_mode>" "i")]
			 VSHLL))]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[3]) == GET_MODE_UNIT_BITSIZE (<MODE>mode))
      return "shll2\\t%0.<Vwtype>, %1.<Vtype>, %3";
    else
      return "<sur>shll2\\t%0.<Vwtype>, %1.<Vtype>, %3";
  }
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "aarch64_<sur>shll_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:VD_BHSI 1 "register_operand" "w")
			 (match_operand:SI 2
			   "aarch64_simd_shift_imm_bitsize_<ve_mode>" "i")]
                         VSHLL))]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[2]) == GET_MODE_UNIT_BITSIZE (<MODE>mode))
      return "shll\\t%0.<Vwtype>, %1.<Vtype>, %2";
    else
      return "<sur>shll\\t%0.<Vwtype>, %1.<Vtype>, %2";
  }
  [(set_attr "type" "neon_shift_imm_long")]
)

;; vshll_high_n

(define_insn "aarch64_<sur>shll2_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:VQW 1 "register_operand" "w")
			 (match_operand:SI 2 "immediate_operand" "i")]
                         VSHLL))]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[2]) == GET_MODE_UNIT_BITSIZE (<MODE>mode))
      return "shll2\\t%0.<Vwtype>, %1.<Vtype>, %2";
    else
      return "<sur>shll2\\t%0.<Vwtype>, %1.<Vtype>, %2";
  }
  [(set_attr "type" "neon_shift_imm_long")]
)

;; vrshr_n

(define_insn "aarch64_<sur>shr_n<mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand" "=w")
        (unspec:VSDQ_I_DI [(match_operand:VSDQ_I_DI 1 "register_operand" "w")
			   (match_operand:SI 2
			     "aarch64_simd_shift_imm_offset_<ve_mode>" "i")]
			  VRSHR_N))]
  "TARGET_SIMD"
  "<sur>shr\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %2"
  [(set_attr "type" "neon_sat_shift_imm<q>")]
)

;; v(r)sra_n

(define_insn "aarch64_<sur>sra_n<mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand" "=w")
	(unspec:VSDQ_I_DI [(match_operand:VSDQ_I_DI 1 "register_operand" "0")
		       (match_operand:VSDQ_I_DI 2 "register_operand" "w")
                       (match_operand:SI 3
			 "aarch64_simd_shift_imm_offset_<ve_mode>" "i")]
                      VSRA))]
  "TARGET_SIMD"
  "<sur>sra\\t%<v>0<Vmtype>, %<v>2<Vmtype>, %3"
  [(set_attr "type" "neon_shift_acc<q>")]
)

;; vs<lr>i_n

(define_insn "aarch64_<sur>s<lr>i_n<mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand" "=w")
	(unspec:VSDQ_I_DI [(match_operand:VSDQ_I_DI 1 "register_operand" "0")
		       (match_operand:VSDQ_I_DI 2 "register_operand" "w")
                       (match_operand:SI 3
			 "aarch64_simd_shift_imm_<offsetlr><ve_mode>" "i")]
                      VSLRI))]
  "TARGET_SIMD"
  "s<lr>i\\t%<v>0<Vmtype>, %<v>2<Vmtype>, %3"
  [(set_attr "type" "neon_shift_imm<q>")]
)

;; vqshl(u)

(define_insn "aarch64_<sur>qshl<u>_n<mode>"
  [(set (match_operand:VSDQ_I 0 "register_operand" "=w")
	(unspec:VSDQ_I [(match_operand:VSDQ_I 1 "register_operand" "w")
		       (match_operand:SI 2
			 "aarch64_simd_shift_imm_<ve_mode>" "i")]
                      VQSHL_N))]
  "TARGET_SIMD"
  "<sur>qshl<u>\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %2"
  [(set_attr "type" "neon_sat_shift_imm<q>")]
)


;; vq(r)shr(u)n_n

(define_insn "aarch64_<sur>q<r>shr<u>n_n<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
        (unspec:<VNARROWQ> [(match_operand:SD_HSDI 1 "register_operand" "w")
			    (match_operand:SI 2
			      "aarch64_simd_shift_imm_offset_<ve_mode>" "i")]
			   VQSHRN_N))]
  "TARGET_SIMD"
  "<sur>q<r>shr<u>n\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>, %2"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_<sur>q<r>shr<u>n_n<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (unspec:<VNARROWQ>
		[(match_operand:VQN 1 "register_operand" "w")
		 (match_operand:VQN 2 "aarch64_simd_shift_imm_vec_<vn_mode>")]
		VQSHRN_N)
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "<sur>q<r>shr<u>n\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>, %2"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "aarch64_<sur>q<r>shr<u>n_n<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 3 "aarch64_simd_or_scalar_imm_zero")
	  (unspec:<VNARROWQ>
		[(match_operand:VQN 1 "register_operand" "w")
		 (match_operand:VQN 2 "aarch64_simd_shift_imm_vec_<vn_mode>")]
		VQSHRN_N)))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "<sur>q<r>shr<u>n\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>, %2"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_expand "aarch64_<sur>q<r>shr<u>n_n<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
        (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand")
			    (match_operand:SI 2
			      "aarch64_simd_shift_imm_offset_<ve_mode>")]
			   VQSHRN_N))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						 INTVAL (operands[2]));
    rtx tmp = gen_reg_rtx (<VNARROWQ2>mode);
    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_<sur>q<r>shr<u>n_n<mode>_insn_be (tmp,
		    operands[1], operands[2], CONST0_RTX (<VNARROWQ>mode)));
    else
      emit_insn (gen_aarch64_<sur>q<r>shr<u>n_n<mode>_insn_le (tmp,
		    operands[1], operands[2], CONST0_RTX (<VNARROWQ>mode)));

    /* The intrinsic expects a narrow result, so emit a subreg that will get
       optimized away as appropriate.  */
    emit_move_insn (operands[0], lowpart_subreg (<VNARROWQ>mode, tmp,
						 <VNARROWQ2>mode));
    DONE;
  }
)

(define_insn "aarch64_<sur>q<r>shr<u>n2_n<mode>_insn_le"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")
	  (unspec:<VNARROWQ> [(match_operand:VQN 2 "register_operand" "w")
			      (match_operand:VQN 3
				"aarch64_simd_shift_imm_vec_<vn_mode>")]
			     VQSHRN_N)))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "<sur>q<r>shr<u>n2\\t%<vn2>0.<V2ntype>, %<v>2.<Vtype>, %3"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "aarch64_<sur>q<r>shr<u>n2_n<mode>_insn_be"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
	(vec_concat:<VNARROWQ2>
          (unspec:<VNARROWQ> [(match_operand:VQN 2 "register_operand" "w")
			      (match_operand:VQN 3
				"aarch64_simd_shift_imm_vec_<vn_mode>")]
			     VQSHRN_N)
	  (match_operand:<VNARROWQ> 1 "register_operand" "0")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "<sur>q<r>shr<u>n2\\t%<vn2>0.<V2ntype>, %<v>2.<Vtype>, %3"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_expand "aarch64_<sur>q<r>shr<u>n2_n<mode>"
  [(match_operand:<VNARROWQ2> 0 "register_operand")
   (match_operand:<VNARROWQ> 1 "register_operand")
   (unspec:<VNARROWQ>
	[(match_operand:VQN 2 "register_operand")
	 (match_operand:SI 3 "aarch64_simd_shift_imm_offset_<vn_mode>")]
        VQSHRN_N)]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_simd_gen_const_vector_dup (<MODE>mode,
						 INTVAL (operands[3]));

    if (BYTES_BIG_ENDIAN)
      emit_insn (gen_aarch64_<sur>q<r>shr<u>n2_n<mode>_insn_be (operands[0],
				operands[1], operands[2], operands[3]));
    else
      emit_insn (gen_aarch64_<sur>q<r>shr<u>n2_n<mode>_insn_le (operands[0],
				operands[1], operands[2], operands[3]));
    DONE;
  }
)


;; cm(eq|ge|gt|lt|le)
;; Note, we have constraints for Dz and Z as different expanders
;; have different ideas of what should be passed to this pattern.

(define_insn "aarch64_cm<optab><mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=w,w")
	(neg:<V_INT_EQUIV>
	  (COMPARISONS:<V_INT_EQUIV>
	    (match_operand:VDQ_I 1 "register_operand" "w,w")
	    (match_operand:VDQ_I 2 "aarch64_simd_reg_or_zero" "w,ZDz")
	  )))]
  "TARGET_SIMD"
  "@
  cm<n_optab>\t%<v>0<Vmtype>, %<v><cmp_1><Vmtype>, %<v><cmp_2><Vmtype>
  cm<optab>\t%<v>0<Vmtype>, %<v>1<Vmtype>, #0"
  [(set_attr "type" "neon_compare<q>, neon_compare_zero<q>")]
)

(define_insn_and_split "aarch64_cm<optab>di"
  [(set (match_operand:DI 0 "register_operand" "=w,w,r")
	(neg:DI
	  (COMPARISONS:DI
	    (match_operand:DI 1 "register_operand" "w,w,r")
	    (match_operand:DI 2 "aarch64_simd_reg_or_zero" "w,ZDz,r")
	  )))
     (clobber (reg:CC CC_REGNUM))]
  "TARGET_SIMD"
  "#"
  "&& reload_completed"
  [(set (match_operand:DI 0 "register_operand")
	(neg:DI
	  (COMPARISONS:DI
	    (match_operand:DI 1 "register_operand")
	    (match_operand:DI 2 "aarch64_simd_reg_or_zero")
	  )))]
  {
    /* If we are in the general purpose register file,
       we split to a sequence of comparison and store.  */
    if (GP_REGNUM_P (REGNO (operands[0]))
	&& GP_REGNUM_P (REGNO (operands[1])))
      {
	machine_mode mode = SELECT_CC_MODE (<CMP>, operands[1], operands[2]);
	rtx cc_reg = aarch64_gen_compare_reg (<CMP>, operands[1], operands[2]);
	rtx comparison = gen_rtx_<CMP> (mode, operands[1], operands[2]);
	emit_insn (gen_cstoredi_neg (operands[0], comparison, cc_reg));
	DONE;
      }
    /* Otherwise, we expand to a similar pattern which does not
       clobber CC_REGNUM.  */
  }
  [(set_attr "type" "neon_compare, neon_compare_zero, multiple")]
)

(define_insn "*aarch64_cm<optab>di"
  [(set (match_operand:DI 0 "register_operand" "=w,w")
	(neg:DI
	  (COMPARISONS:DI
	    (match_operand:DI 1 "register_operand" "w,w")
	    (match_operand:DI 2 "aarch64_simd_reg_or_zero" "w,ZDz")
	  )))]
  "TARGET_SIMD && reload_completed"
  "@
  cm<n_optab>\t%d0, %d<cmp_1>, %d<cmp_2>
  cm<optab>\t%d0, %d1, #0"
  [(set_attr "type" "neon_compare, neon_compare_zero")]
)

;; cm(hs|hi)

(define_insn "aarch64_cm<optab><mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=w")
	(neg:<V_INT_EQUIV>
	  (UCOMPARISONS:<V_INT_EQUIV>
	    (match_operand:VDQ_I 1 "register_operand" "w")
	    (match_operand:VDQ_I 2 "register_operand" "w")
	  )))]
  "TARGET_SIMD"
  "cm<n_optab>\t%<v>0<Vmtype>, %<v><cmp_1><Vmtype>, %<v><cmp_2><Vmtype>"
  [(set_attr "type" "neon_compare<q>")]
)

(define_insn_and_split "aarch64_cm<optab>di"
  [(set (match_operand:DI 0 "register_operand" "=w,r")
	(neg:DI
	  (UCOMPARISONS:DI
	    (match_operand:DI 1 "register_operand" "w,r")
	    (match_operand:DI 2 "aarch64_simd_reg_or_zero" "w,r")
	  )))
    (clobber (reg:CC CC_REGNUM))]
  "TARGET_SIMD"
  "#"
  "&& reload_completed"
  [(set (match_operand:DI 0 "register_operand")
	(neg:DI
	  (UCOMPARISONS:DI
	    (match_operand:DI 1 "register_operand")
	    (match_operand:DI 2 "aarch64_simd_reg_or_zero")
	  )))]
  {
    /* If we are in the general purpose register file,
       we split to a sequence of comparison and store.  */
    if (GP_REGNUM_P (REGNO (operands[0]))
	&& GP_REGNUM_P (REGNO (operands[1])))
      {
	machine_mode mode = CCmode;
	rtx cc_reg = aarch64_gen_compare_reg (<CMP>, operands[1], operands[2]);
	rtx comparison = gen_rtx_<CMP> (mode, operands[1], operands[2]);
	emit_insn (gen_cstoredi_neg (operands[0], comparison, cc_reg));
	DONE;
      }
    /* Otherwise, we expand to a similar pattern which does not
       clobber CC_REGNUM.  */
  }
  [(set_attr "type" "neon_compare,multiple")]
)

(define_insn "*aarch64_cm<optab>di"
  [(set (match_operand:DI 0 "register_operand" "=w")
	(neg:DI
	  (UCOMPARISONS:DI
	    (match_operand:DI 1 "register_operand" "w")
	    (match_operand:DI 2 "aarch64_simd_reg_or_zero" "w")
	  )))]
  "TARGET_SIMD && reload_completed"
  "cm<n_optab>\t%d0, %d<cmp_1>, %d<cmp_2>"
  [(set_attr "type" "neon_compare")]
)

;; cmtst

;; Although neg (ne (and x y) 0) is the natural way of expressing a cmtst,
;; we don't have any insns using ne, and aarch64_vcond outputs
;; not (neg (eq (and x y) 0))
;; which is rewritten by simplify_rtx as
;; plus (eq (and x y) 0) -1.

(define_insn "aarch64_cmtst<mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=w")
	(plus:<V_INT_EQUIV>
	  (eq:<V_INT_EQUIV>
	    (and:VDQ_I
	      (match_operand:VDQ_I 1 "register_operand" "w")
	      (match_operand:VDQ_I 2 "register_operand" "w"))
	    (match_operand:VDQ_I 3 "aarch64_simd_imm_zero"))
	  (match_operand:<V_INT_EQUIV> 4 "aarch64_simd_imm_minus_one")))
  ]
  "TARGET_SIMD"
  "cmtst\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_tst<q>")]
)

;; One can also get a cmtsts by having to combine a
;; not (neq (eq x 0)) in which case you rewrite it to
;; a comparison against itself

(define_insn "*aarch64_cmtst_same_<mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=w")
	(plus:<V_INT_EQUIV>
	  (eq:<V_INT_EQUIV>
	    (match_operand:VDQ_I 1 "register_operand" "w")
	    (match_operand:VDQ_I 2 "aarch64_simd_imm_zero"))
	  (match_operand:<V_INT_EQUIV> 3 "aarch64_simd_imm_minus_one")))
  ]
  "TARGET_SIMD"
  "cmtst\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_tst<q>")]
)

(define_insn_and_split "aarch64_cmtstdi"
  [(set (match_operand:DI 0 "register_operand" "=w,r")
	(neg:DI
	  (ne:DI
	    (and:DI
	      (match_operand:DI 1 "register_operand" "w,r")
	      (match_operand:DI 2 "register_operand" "w,r"))
	    (const_int 0))))
    (clobber (reg:CC CC_REGNUM))]
  "TARGET_SIMD"
  "#"
  "&& reload_completed"
  [(set (match_operand:DI 0 "register_operand")
	(neg:DI
	  (ne:DI
	    (and:DI
	      (match_operand:DI 1 "register_operand")
	      (match_operand:DI 2 "register_operand"))
	    (const_int 0))))]
  {
    /* If we are in the general purpose register file,
       we split to a sequence of comparison and store.  */
    if (GP_REGNUM_P (REGNO (operands[0]))
	&& GP_REGNUM_P (REGNO (operands[1])))
      {
	rtx and_tree = gen_rtx_AND (DImode, operands[1], operands[2]);
	machine_mode mode = SELECT_CC_MODE (NE, and_tree, const0_rtx);
	rtx cc_reg = aarch64_gen_compare_reg (NE, and_tree, const0_rtx);
	rtx comparison = gen_rtx_NE (mode, and_tree, const0_rtx);
	emit_insn (gen_cstoredi_neg (operands[0], comparison, cc_reg));
	DONE;
      }
    /* Otherwise, we expand to a similar pattern which does not
       clobber CC_REGNUM.  */
  }
  [(set_attr "type" "neon_tst,multiple")]
)

(define_insn "*aarch64_cmtstdi"
  [(set (match_operand:DI 0 "register_operand" "=w")
	(neg:DI
	  (ne:DI
	    (and:DI
	      (match_operand:DI 1 "register_operand" "w")
	      (match_operand:DI 2 "register_operand" "w"))
	    (const_int 0))))]
  "TARGET_SIMD"
  "cmtst\t%d0, %d1, %d2"
  [(set_attr "type" "neon_tst")]
)

;; fcm(eq|ge|gt|le|lt)

(define_insn "aarch64_cm<optab><mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=w,w")
	(neg:<V_INT_EQUIV>
	  (COMPARISONS:<V_INT_EQUIV>
	    (match_operand:VHSDF_HSDF 1 "register_operand" "w,w")
	    (match_operand:VHSDF_HSDF 2 "aarch64_simd_reg_or_zero" "w,YDz")
	  )))]
  "TARGET_SIMD"
  "@
  fcm<n_optab>\t%<v>0<Vmtype>, %<v><cmp_1><Vmtype>, %<v><cmp_2><Vmtype>
  fcm<optab>\t%<v>0<Vmtype>, %<v>1<Vmtype>, 0"
  [(set_attr "type" "neon_fp_compare_<stype><q>")]
)

;; fac(ge|gt)
;; Note we can also handle what would be fac(le|lt) by
;; generating fac(ge|gt).

(define_insn "aarch64_fac<optab><mode>"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand" "=w")
	(neg:<V_INT_EQUIV>
	  (FAC_COMPARISONS:<V_INT_EQUIV>
	    (abs:VHSDF_HSDF
	      (match_operand:VHSDF_HSDF 1 "register_operand" "w"))
	    (abs:VHSDF_HSDF
	      (match_operand:VHSDF_HSDF 2 "register_operand" "w"))
  )))]
  "TARGET_SIMD"
  "fac<n_optab>\t%<v>0<Vmtype>, %<v><cmp_1><Vmtype>, %<v><cmp_2><Vmtype>"
  [(set_attr "type" "neon_fp_compare_<stype><q>")]
)

;; addp

(define_insn "aarch64_addp<mode>"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (unspec:VDQ_I
          [(match_operand:VDQ_I 1 "register_operand" "w")
	   (match_operand:VDQ_I 2 "register_operand" "w")]
          UNSPEC_ADDP))]
  "TARGET_SIMD"
  "addp\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_reduc_add<q>")]
)

(define_insn "aarch64_addpdi"
  [(set (match_operand:DI 0 "register_operand" "=w")
        (unspec:DI
          [(match_operand:V2DI 1 "register_operand" "w")]
          UNSPEC_ADDP))]
  "TARGET_SIMD"
  "addp\t%d0, %1.2d"
  [(set_attr "type" "neon_reduc_add")]
)

;; sqrt

(define_expand "sqrt<mode>2"
  [(set (match_operand:VHSDF 0 "register_operand")
	(sqrt:VHSDF (match_operand:VHSDF 1 "register_operand")))]
  "TARGET_SIMD"
{
  if (aarch64_emit_approx_sqrt (operands[0], operands[1], false))
    DONE;
})

(define_insn "*sqrt<mode>2"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(sqrt:VHSDF (match_operand:VHSDF 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "fsqrt\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_sqrt_<stype><q>")]
)

;; Patterns for vector struct loads and stores.

(define_insn "aarch64_simd_ld2<vstruct_elt>"
  [(set (match_operand:VSTRUCT_2Q 0 "register_operand" "=w")
	(unspec:VSTRUCT_2Q [
	  (match_operand:VSTRUCT_2Q 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD2))]
  "TARGET_SIMD"
  "ld2\\t{%S0.<Vtype> - %T0.<Vtype>}, %1"
  [(set_attr "type" "neon_load2_2reg<q>")]
)

(define_insn "aarch64_simd_ld2r<vstruct_elt>"
  [(set (match_operand:VSTRUCT_2QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_2QD [
	  (match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
          UNSPEC_LD2_DUP))]
  "TARGET_SIMD"
  "ld2r\\t{%S0.<Vtype> - %T0.<Vtype>}, %1"
  [(set_attr "type" "neon_load2_all_lanes<q>")]
)

(define_insn "aarch64_vec_load_lanes<mode>_lane<vstruct_elt>"
  [(set (match_operand:VSTRUCT_2QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_2QD [
		(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
		(match_operand:VSTRUCT_2QD 2 "register_operand" "0")
		(match_operand:SI 3 "immediate_operand" "i")]
		UNSPEC_LD2_LANE))]
  "TARGET_SIMD"
  {
    operands[3] = aarch64_endian_lane_rtx (<VSTRUCT_ELT>mode,
					   INTVAL (operands[3]));
    return "ld2\\t{%S0.<Vetype> - %T0.<Vetype>}[%3], %1";
  }
  [(set_attr "type" "neon_load2_one_lane")]
)

(define_expand "vec_load_lanes<mode><vstruct_elt>"
  [(set (match_operand:VSTRUCT_2Q 0 "register_operand")
	(unspec:VSTRUCT_2Q [
		(match_operand:VSTRUCT_2Q 1 "aarch64_simd_struct_operand")]
		UNSPEC_LD2))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx mask = aarch64_reverse_mask (<VSTRUCT_ELT>mode,
			GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>);
      emit_insn (gen_aarch64_simd_ld2<vstruct_elt> (tmp, operands[1]));
      emit_insn (gen_aarch64_rev_reglist<mode> (operands[0], tmp, mask));
    }
  else
    emit_insn (gen_aarch64_simd_ld2<vstruct_elt> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_st2<vstruct_elt>"
  [(set (match_operand:VSTRUCT_2Q 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_2Q [
		(match_operand:VSTRUCT_2Q 1 "register_operand" "w")]
                UNSPEC_ST2))]
  "TARGET_SIMD"
  "st2\\t{%S1.<Vtype> - %T1.<Vtype>}, %0"
  [(set_attr "type" "neon_store2_2reg<q>")]
)

;; RTL uses GCC vector extension indices, so flip only for assembly.
(define_insn "aarch64_vec_store_lanes<mode>_lane<vstruct_elt>"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:VSTRUCT_2QD 1 "register_operand" "w")
		     (match_operand:SI 2 "immediate_operand" "i")]
		     UNSPEC_ST2_LANE))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSTRUCT_ELT>mode,
					   INTVAL (operands[2]));
    return "st2\\t{%S1.<Vetype> - %T1.<Vetype>}[%2], %0";
  }
  [(set_attr "type" "neon_store2_one_lane<q>")]
)

(define_expand "vec_store_lanes<mode><vstruct_elt>"
  [(set (match_operand:VSTRUCT_2Q 0 "aarch64_simd_struct_operand")
	(unspec:VSTRUCT_2Q [(match_operand:VSTRUCT_2Q 1 "register_operand")]
                   UNSPEC_ST2))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx mask = aarch64_reverse_mask (<VSTRUCT_ELT>mode,
			GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>);
      emit_insn (gen_aarch64_rev_reglist<mode> (tmp, operands[1], mask));
      emit_insn (gen_aarch64_simd_st2<vstruct_elt> (operands[0], tmp));
    }
  else
    emit_insn (gen_aarch64_simd_st2<vstruct_elt> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_ld3<vstruct_elt>"
  [(set (match_operand:VSTRUCT_3Q 0 "register_operand" "=w")
	(unspec:VSTRUCT_3Q [
	  (match_operand:VSTRUCT_3Q 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD3))]
  "TARGET_SIMD"
  "ld3\\t{%S0.<Vtype> - %U0.<Vtype>}, %1"
  [(set_attr "type" "neon_load3_3reg<q>")]
)

(define_insn "aarch64_simd_ld3r<vstruct_elt>"
  [(set (match_operand:VSTRUCT_3QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_3QD [
	  (match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
          UNSPEC_LD3_DUP))]
  "TARGET_SIMD"
  "ld3r\\t{%S0.<Vtype> - %U0.<Vtype>}, %1"
  [(set_attr "type" "neon_load3_all_lanes<q>")]
)

(define_insn "aarch64_vec_load_lanes<mode>_lane<vstruct_elt>"
  [(set (match_operand:VSTRUCT_3QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_3QD [
		(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
		(match_operand:VSTRUCT_3QD 2 "register_operand" "0")
		(match_operand:SI 3 "immediate_operand" "i")]
		UNSPEC_LD3_LANE))]
  "TARGET_SIMD"
{
    operands[3] = aarch64_endian_lane_rtx (<VSTRUCT_ELT>mode,
					   INTVAL (operands[3]));
    return "ld3\\t{%S0.<Vetype> - %U0.<Vetype>}[%3], %1";
}
  [(set_attr "type" "neon_load3_one_lane")]
)

(define_expand "vec_load_lanes<mode><vstruct_elt>"
  [(set (match_operand:VSTRUCT_3Q 0 "register_operand")
	(unspec:VSTRUCT_3Q [
		(match_operand:VSTRUCT_3Q 1 "aarch64_simd_struct_operand")]
		UNSPEC_LD3))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx mask = aarch64_reverse_mask (<VSTRUCT_ELT>mode,
			GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>);
      emit_insn (gen_aarch64_simd_ld3<vstruct_elt> (tmp, operands[1]));
      emit_insn (gen_aarch64_rev_reglist<mode> (operands[0], tmp, mask));
    }
  else
    emit_insn (gen_aarch64_simd_ld3<vstruct_elt> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_st3<vstruct_elt>"
  [(set (match_operand:VSTRUCT_3Q 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_3Q [(match_operand:VSTRUCT_3Q 1 "register_operand" "w")]
                   UNSPEC_ST3))]
  "TARGET_SIMD"
  "st3\\t{%S1.<Vtype> - %U1.<Vtype>}, %0"
  [(set_attr "type" "neon_store3_3reg<q>")]
)

;; RTL uses GCC vector extension indices, so flip only for assembly.
(define_insn "aarch64_vec_store_lanes<mode>_lane<vstruct_elt>"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:VSTRUCT_3QD 1 "register_operand" "w")
		     (match_operand:SI 2 "immediate_operand" "i")]
		     UNSPEC_ST3_LANE))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSTRUCT_ELT>mode,
					   INTVAL (operands[2]));
    return "st3\\t{%S1.<Vetype> - %U1.<Vetype>}[%2], %0";
  }
  [(set_attr "type" "neon_store3_one_lane<q>")]
)

(define_expand "vec_store_lanes<mode><vstruct_elt>"
  [(set (match_operand:VSTRUCT_3Q 0 "aarch64_simd_struct_operand")
	(unspec:VSTRUCT_3Q [
		(match_operand:VSTRUCT_3Q 1 "register_operand")]
                UNSPEC_ST3))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx mask = aarch64_reverse_mask (<VSTRUCT_ELT>mode,
			GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>);
      emit_insn (gen_aarch64_rev_reglist<mode> (tmp, operands[1], mask));
      emit_insn (gen_aarch64_simd_st3<vstruct_elt> (operands[0], tmp));
    }
  else
    emit_insn (gen_aarch64_simd_st3<vstruct_elt> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_ld4<vstruct_elt>"
  [(set (match_operand:VSTRUCT_4Q 0 "register_operand" "=w")
	(unspec:VSTRUCT_4Q [
	  (match_operand:VSTRUCT_4Q 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD4))]
  "TARGET_SIMD"
  "ld4\\t{%S0.<Vtype> - %V0.<Vtype>}, %1"
  [(set_attr "type" "neon_load4_4reg<q>")]
)

(define_insn "aarch64_simd_ld4r<vstruct_elt>"
  [(set (match_operand:VSTRUCT_4QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_4QD [
	  (match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
          UNSPEC_LD4_DUP))]
  "TARGET_SIMD"
  "ld4r\\t{%S0.<Vtype> - %V0.<Vtype>}, %1"
  [(set_attr "type" "neon_load4_all_lanes<q>")]
)

(define_insn "aarch64_vec_load_lanes<mode>_lane<vstruct_elt>"
  [(set (match_operand:VSTRUCT_4QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_4QD [
		(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
		(match_operand:VSTRUCT_4QD 2 "register_operand" "0")
		(match_operand:SI 3 "immediate_operand" "i")]
		UNSPEC_LD4_LANE))]
  "TARGET_SIMD"
{
    operands[3] = aarch64_endian_lane_rtx (<VSTRUCT_ELT>mode,
					   INTVAL (operands[3]));
    return "ld4\\t{%S0.<Vetype> - %V0.<Vetype>}[%3], %1";
}
  [(set_attr "type" "neon_load4_one_lane")]
)

(define_expand "vec_load_lanes<mode><vstruct_elt>"
  [(set (match_operand:VSTRUCT_4Q 0 "register_operand")
	(unspec:VSTRUCT_4Q [
		(match_operand:VSTRUCT_4Q 1 "aarch64_simd_struct_operand")]
		UNSPEC_LD4))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx mask = aarch64_reverse_mask (<VSTRUCT_ELT>mode,
			GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>);
      emit_insn (gen_aarch64_simd_ld4<vstruct_elt> (tmp, operands[1]));
      emit_insn (gen_aarch64_rev_reglist<mode> (operands[0], tmp, mask));
    }
  else
    emit_insn (gen_aarch64_simd_ld4<vstruct_elt> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_st4<vstruct_elt>"
  [(set (match_operand:VSTRUCT_4Q 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_4Q [
		(match_operand:VSTRUCT_4Q 1 "register_operand" "w")]
                UNSPEC_ST4))]
  "TARGET_SIMD"
  "st4\\t{%S1.<Vtype> - %V1.<Vtype>}, %0"
  [(set_attr "type" "neon_store4_4reg<q>")]
)

;; RTL uses GCC vector extension indices, so flip only for assembly.
(define_insn "aarch64_vec_store_lanes<mode>_lane<vstruct_elt>"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:VSTRUCT_4QD 1 "register_operand" "w")
		     (match_operand:SI 2 "immediate_operand" "i")]
		     UNSPEC_ST4_LANE))]
  "TARGET_SIMD"
  {
    operands[2] = aarch64_endian_lane_rtx (<VSTRUCT_ELT>mode,
					   INTVAL (operands[2]));
    return "st4\\t{%S1.<Vetype> - %V1.<Vetype>}[%2], %0";
  }
  [(set_attr "type" "neon_store4_one_lane<q>")]
)

(define_expand "vec_store_lanes<mode><vstruct_elt>"
  [(set (match_operand:VSTRUCT_4Q 0 "aarch64_simd_struct_operand")
	(unspec:VSTRUCT_4Q [(match_operand:VSTRUCT_4Q 1 "register_operand")]
                   UNSPEC_ST4))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      rtx mask = aarch64_reverse_mask (<VSTRUCT_ELT>mode,
			GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>);
      emit_insn (gen_aarch64_rev_reglist<mode> (tmp, operands[1], mask));
      emit_insn (gen_aarch64_simd_st4<vstruct_elt> (operands[0], tmp));
    }
  else
    emit_insn (gen_aarch64_simd_st4<vstruct_elt> (operands[0], operands[1]));
  DONE;
})

(define_insn_and_split "aarch64_rev_reglist<mode>"
[(set (match_operand:VSTRUCT_QD 0 "register_operand" "=&w")
	(unspec:VSTRUCT_QD
	           [(match_operand:VSTRUCT_QD 1 "register_operand" "w")
		    (match_operand:V16QI 2 "register_operand" "w")]
                   UNSPEC_REV_REGLIST))]
  "TARGET_SIMD"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  int i;
  int nregs = GET_MODE_SIZE (<MODE>mode).to_constant () / UNITS_PER_VREG;
  for (i = 0; i < nregs; i++)
    {
      rtx op0 = gen_rtx_REG (V16QImode, REGNO (operands[0]) + i);
      rtx op1 = gen_rtx_REG (V16QImode, REGNO (operands[1]) + i);
      emit_insn (gen_aarch64_qtbl1v16qi (op0, op1, operands[2]));
    }
  DONE;
}
  [(set_attr "type" "neon_tbl1_q")
   (set_attr "length" "<insn_count>")]
)

;; Reload patterns for AdvSIMD register list operands.

(define_expand "mov<mode>"
  [(set (match_operand:VSTRUCT_QD 0 "nonimmediate_operand")
	(match_operand:VSTRUCT_QD 1 "general_operand"))]
  "TARGET_FLOAT"
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
})

(define_expand "mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand")
	(match_operand:VSTRUCT 1 "general_operand"))]
  "TARGET_FLOAT"
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
})

(define_expand "movv8di"
  [(set (match_operand:V8DI 0 "nonimmediate_operand")
	(match_operand:V8DI 1 "general_operand"))]
  ""
{
  if (can_create_pseudo_p () && MEM_P (operands[0]))
    operands[1] = force_reg (V8DImode, operands[1]);
})

(define_expand "aarch64_ld1x3<vstruct_elt>"
  [(match_operand:VSTRUCT_3QD 0 "register_operand")
   (match_operand:DI 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[1]);
  emit_insn (gen_aarch64_ld1_x3_<vstruct_elt> (operands[0], mem));
  DONE;
})

(define_insn "aarch64_ld1_x3_<vstruct_elt>"
  [(set (match_operand:VSTRUCT_3QD 0 "register_operand" "=w")
        (unspec:VSTRUCT_3QD
	  [(match_operand:VSTRUCT_3QD 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD1))]
  "TARGET_SIMD"
  "ld1\\t{%S0.<Vtype> - %U0.<Vtype>}, %1"
  [(set_attr "type" "neon_load1_3reg<q>")]
)

(define_expand "aarch64_ld1x4<vstruct_elt>"
  [(match_operand:VSTRUCT_4QD 0 "register_operand" "=w")
   (match_operand:DI 1 "register_operand" "r")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[1]);
  emit_insn (gen_aarch64_ld1_x4_<vstruct_elt> (operands[0], mem));
  DONE;
})

(define_insn "aarch64_ld1_x4_<vstruct_elt>"
  [(set (match_operand:VSTRUCT_4QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_4QD
	  [(match_operand:VSTRUCT_4QD 1 "aarch64_simd_struct_operand" "Utv")]
	UNSPEC_LD1))]
  "TARGET_SIMD"
  "ld1\\t{%S0.<Vtype> - %V0.<Vtype>}, %1"
  [(set_attr "type" "neon_load1_4reg<q>")]
)

(define_expand "aarch64_st1x2<vstruct_elt>"
  [(match_operand:DI 0 "register_operand")
   (match_operand:VSTRUCT_2QD 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[0]);
  emit_insn (gen_aarch64_st1_x2_<vstruct_elt> (mem, operands[1]));
  DONE;
})

(define_insn "aarch64_st1_x2_<vstruct_elt>"
  [(set (match_operand:VSTRUCT_2QD 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_2QD
		[(match_operand:VSTRUCT_2QD 1 "register_operand" "w")]
		UNSPEC_ST1))]
  "TARGET_SIMD"
  "st1\\t{%S1.<Vtype> - %T1.<Vtype>}, %0"
  [(set_attr "type" "neon_store1_2reg<q>")]
)

(define_expand "aarch64_st1x3<vstruct_elt>"
  [(match_operand:DI 0 "register_operand")
   (match_operand:VSTRUCT_3QD 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[0]);
  emit_insn (gen_aarch64_st1_x3_<vstruct_elt> (mem, operands[1]));
  DONE;
})

(define_insn "aarch64_st1_x3_<vstruct_elt>"
  [(set (match_operand:VSTRUCT_3QD 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_3QD
		[(match_operand:VSTRUCT_3QD 1 "register_operand" "w")]
		UNSPEC_ST1))]
  "TARGET_SIMD"
  "st1\\t{%S1.<Vtype> - %U1.<Vtype>}, %0"
  [(set_attr "type" "neon_store1_3reg<q>")]
)

(define_expand "aarch64_st1x4<vstruct_elt>"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:VSTRUCT_4QD 1 "register_operand" "")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[0]);
  emit_insn (gen_aarch64_st1_x4_<vstruct_elt> (mem, operands[1]));
  DONE;
})

(define_insn "aarch64_st1_x4_<vstruct_elt>"
  [(set (match_operand:VSTRUCT_4QD 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_4QD
		[(match_operand:VSTRUCT_4QD 1 "register_operand" "w")]
		UNSPEC_ST1))]
  "TARGET_SIMD"
  "st1\\t{%S1.<Vtype> - %V1.<Vtype>}, %0"
  [(set_attr "type" "neon_store1_4reg<q>")]
)

(define_insn "*aarch64_mov<mode>"
  [(set (match_operand:VSTRUCT_QD 0 "aarch64_simd_nonimmediate_operand" "=w,Utv,w")
	(match_operand:VSTRUCT_QD 1 "aarch64_simd_general_operand" " w,w,Utv"))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   #
   st1\\t{%S1.<Vtype> - %<Vendreg>1.<Vtype>}, %0
   ld1\\t{%S0.<Vtype> - %<Vendreg>0.<Vtype>}, %1"
  [(set_attr "type" "multiple,neon_store<nregs>_<nregs>reg_q,\
		     neon_load<nregs>_<nregs>reg_q")
   (set_attr "length" "<insn_count>,4,4")]
)

(define_insn "*aarch64_mov<mode>"
  [(set (match_operand:VSTRUCT 0 "aarch64_simd_nonimmediate_operand" "=w,Utv,w")
	(match_operand:VSTRUCT 1 "aarch64_simd_general_operand" " w,w,Utv"))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   #
   st1\\t{%S1.16b - %<Vendreg>1.16b}, %0
   ld1\\t{%S0.16b - %<Vendreg>0.16b}, %1"
  [(set_attr "type" "multiple,neon_store<nregs>_<nregs>reg_q,\
		     neon_load<nregs>_<nregs>reg_q")
   (set_attr "length" "<insn_count>,4,4")]
)

(define_insn "*aarch64_movv8di"
  [(set (match_operand:V8DI 0 "nonimmediate_operand" "=r,m,r")
	(match_operand:V8DI 1 "general_operand" " r,r,m"))]
  "(register_operand (operands[0], V8DImode)
    || register_operand (operands[1], V8DImode))"
  "#"
  [(set_attr "type" "multiple,multiple,multiple")
   (set_attr "length" "32,16,16")]
)

(define_insn "aarch64_be_ld1<mode>"
  [(set (match_operand:VALLDI_F16 0	"register_operand" "=w")
	(unspec:VALLDI_F16 [(match_operand:VALLDI_F16 1
			     "aarch64_simd_struct_operand" "Utv")]
	UNSPEC_LD1))]
  "TARGET_SIMD"
  "ld1\\t{%0<Vmtype>}, %1"
  [(set_attr "type" "neon_load1_1reg<q>")]
)

(define_insn "aarch64_be_st1<mode>"
  [(set (match_operand:VALLDI_F16 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VALLDI_F16 [(match_operand:VALLDI_F16 1 "register_operand" "w")]
	UNSPEC_ST1))]
  "TARGET_SIMD"
  "st1\\t{%1<Vmtype>}, %0"
  [(set_attr "type" "neon_store1_1reg<q>")]
)

(define_insn "*aarch64_be_mov<mode>"
  [(set (match_operand:VSTRUCT_2D 0 "nonimmediate_operand" "=w,m,w")
	(match_operand:VSTRUCT_2D 1 "general_operand"      " w,w,m"))]
  "TARGET_FLOAT
   && (!TARGET_SIMD || BYTES_BIG_ENDIAN)
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   #
   stp\\t%d1, %R1, %0
   ldp\\t%d0, %R0, %1"
  [(set_attr "type" "multiple,neon_stp,neon_ldp")
   (set_attr "length" "8,4,4")]
)

(define_insn "*aarch64_be_mov<mode>"
  [(set (match_operand:VSTRUCT_2Q 0 "nonimmediate_operand" "=w,m,w")
	(match_operand:VSTRUCT_2Q 1 "general_operand"      " w,w,m"))]
  "TARGET_FLOAT
   && (!TARGET_SIMD || BYTES_BIG_ENDIAN)
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   #
   stp\\t%q1, %R1, %0
   ldp\\t%q0, %R0, %1"
  [(set_attr "type" "multiple,neon_stp_q,neon_ldp_q")
   (set_attr "arch" "simd,*,*")
   (set_attr "length" "8,4,4")]
)

(define_insn "*aarch64_be_movoi"
  [(set (match_operand:OI 0 "nonimmediate_operand" "=w,m,w")
	(match_operand:OI 1 "general_operand"      " w,w,m"))]
  "TARGET_FLOAT
   && (!TARGET_SIMD || BYTES_BIG_ENDIAN)
   && (register_operand (operands[0], OImode)
       || register_operand (operands[1], OImode))"
  "@
   #
   stp\\t%q1, %R1, %0
   ldp\\t%q0, %R0, %1"
  [(set_attr "type" "multiple,neon_stp_q,neon_ldp_q")
   (set_attr "arch" "simd,*,*")
   (set_attr "length" "8,4,4")]
)

(define_insn "*aarch64_be_mov<mode>"
  [(set (match_operand:VSTRUCT_3QD 0 "nonimmediate_operand" "=w,o,w")
	(match_operand:VSTRUCT_3QD 1 "general_operand"      " w,w,o"))]
  "TARGET_FLOAT
   && (!TARGET_SIMD || BYTES_BIG_ENDIAN)
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "#"
  [(set_attr "type" "multiple")
   (set_attr "arch" "fp<q>,*,*")
   (set_attr "length" "12,8,8")]
)

(define_insn "*aarch64_be_movci"
  [(set (match_operand:CI 0 "nonimmediate_operand" "=w,o,w")
	(match_operand:CI 1 "general_operand"      " w,w,o"))]
  "TARGET_FLOAT
   && (!TARGET_SIMD || BYTES_BIG_ENDIAN)
   && (register_operand (operands[0], CImode)
       || register_operand (operands[1], CImode))"
  "#"
  [(set_attr "type" "multiple")
   (set_attr "arch" "simd,*,*")
   (set_attr "length" "12,8,8")]
)

(define_insn "*aarch64_be_mov<mode>"
  [(set (match_operand:VSTRUCT_4QD 0 "nonimmediate_operand" "=w,o,w")
	(match_operand:VSTRUCT_4QD 1 "general_operand"      " w,w,o"))]
  "TARGET_FLOAT
   && (!TARGET_SIMD || BYTES_BIG_ENDIAN)
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "#"
  [(set_attr "type" "multiple")
   (set_attr "arch" "fp<q>,*,*")
   (set_attr "length" "16,8,8")]
)

(define_insn "*aarch64_be_movxi"
  [(set (match_operand:XI 0 "nonimmediate_operand" "=w,o,w")
	(match_operand:XI 1 "general_operand"      " w,w,o"))]
  "TARGET_FLOAT
   && (!TARGET_SIMD || BYTES_BIG_ENDIAN)
   && (register_operand (operands[0], XImode)
       || register_operand (operands[1], XImode))"
  "#"
  [(set_attr "type" "multiple")
   (set_attr "arch" "simd,*,*")
   (set_attr "length" "16,8,8")]
)

(define_split
  [(set (match_operand:VSTRUCT_2QD 0 "register_operand")
	(match_operand:VSTRUCT_2QD 1 "register_operand"))]
  "TARGET_FLOAT && reload_completed"
  [(const_int 0)]
{
  aarch64_simd_emit_reg_reg_move (operands, <VSTRUCT_ELT>mode, 2);
  DONE;
})

(define_split
  [(set (match_operand:OI 0 "register_operand")
	(match_operand:OI 1 "register_operand"))]
  "TARGET_FLOAT && reload_completed"
  [(const_int 0)]
{
  aarch64_simd_emit_reg_reg_move (operands, TImode, 2);
  DONE;
})

(define_split
  [(set (match_operand:VSTRUCT_3QD 0 "nonimmediate_operand")
	(match_operand:VSTRUCT_3QD 1 "general_operand"))]
  "TARGET_FLOAT && reload_completed"
  [(const_int 0)]
{
  if (register_operand (operands[0], <MODE>mode)
      && register_operand (operands[1], <MODE>mode))
    {
      aarch64_simd_emit_reg_reg_move (operands, <VSTRUCT_ELT>mode, 3);
      DONE;
    }
  else if (!TARGET_SIMD || BYTES_BIG_ENDIAN)
    {
      int elt_size = GET_MODE_SIZE (<MODE>mode).to_constant () / <nregs>;
      machine_mode pair_mode = elt_size == 16 ? V2x16QImode : V2x8QImode;
      emit_move_insn (simplify_gen_subreg (pair_mode, operands[0],
					   <MODE>mode, 0),
		      simplify_gen_subreg (pair_mode, operands[1],
					   <MODE>mode, 0));
      emit_move_insn (gen_lowpart (<VSTRUCT_ELT>mode,
				   simplify_gen_subreg (<VSTRUCT_ELT>mode,
							operands[0],
							<MODE>mode,
							2 * elt_size)),
		      gen_lowpart (<VSTRUCT_ELT>mode,
				   simplify_gen_subreg (<VSTRUCT_ELT>mode,
							operands[1],
							<MODE>mode,
							2 * elt_size)));
      DONE;
    }
  else
    FAIL;
})

(define_split
  [(set (match_operand:CI 0 "nonimmediate_operand")
	(match_operand:CI 1 "general_operand"))]
  "TARGET_FLOAT && reload_completed"
  [(const_int 0)]
{
  if (register_operand (operands[0], CImode)
      && register_operand (operands[1], CImode))
    {
      aarch64_simd_emit_reg_reg_move (operands, TImode, 3);
      DONE;
    }
  else if (!TARGET_SIMD || BYTES_BIG_ENDIAN)
    {
      emit_move_insn (simplify_gen_subreg (OImode, operands[0], CImode, 0),
		      simplify_gen_subreg (OImode, operands[1], CImode, 0));
      emit_move_insn (gen_lowpart (V16QImode,
				   simplify_gen_subreg (TImode, operands[0],
							CImode, 32)),
		      gen_lowpart (V16QImode,
				   simplify_gen_subreg (TImode, operands[1],
							CImode, 32)));
      DONE;
    }
  else
    FAIL;
})

(define_split
  [(set (match_operand:VSTRUCT_4QD 0 "nonimmediate_operand")
	(match_operand:VSTRUCT_4QD 1 "general_operand"))]
  "TARGET_FLOAT && reload_completed"
  [(const_int 0)]
{
  if (register_operand (operands[0], <MODE>mode)
      && register_operand (operands[1], <MODE>mode))
    {
      aarch64_simd_emit_reg_reg_move (operands, <VSTRUCT_ELT>mode, 4);
      DONE;
    }
  else if (!TARGET_SIMD || BYTES_BIG_ENDIAN)
    {
      int elt_size = GET_MODE_SIZE (<MODE>mode).to_constant () / <nregs>;
      machine_mode pair_mode = elt_size == 16 ? V2x16QImode : V2x8QImode;
      emit_move_insn (simplify_gen_subreg (pair_mode, operands[0],
					   <MODE>mode, 0),
		      simplify_gen_subreg (pair_mode, operands[1],
					   <MODE>mode, 0));
      emit_move_insn (simplify_gen_subreg (pair_mode, operands[0],
					   <MODE>mode, 2 * elt_size),
		      simplify_gen_subreg (pair_mode, operands[1],
					   <MODE>mode, 2 * elt_size));
      DONE;
    }
  else
    FAIL;
})

(define_split
  [(set (match_operand:XI 0 "nonimmediate_operand")
	(match_operand:XI 1 "general_operand"))]
  "TARGET_FLOAT && reload_completed"
  [(const_int 0)]
{
  if (register_operand (operands[0], XImode)
      && register_operand (operands[1], XImode))
    {
      aarch64_simd_emit_reg_reg_move (operands, TImode, 4);
      DONE;
    }
  else if (!TARGET_SIMD || BYTES_BIG_ENDIAN)
    {
      emit_move_insn (simplify_gen_subreg (OImode, operands[0], XImode, 0),
		      simplify_gen_subreg (OImode, operands[1], XImode, 0));
      emit_move_insn (simplify_gen_subreg (OImode, operands[0], XImode, 32),
		      simplify_gen_subreg (OImode, operands[1], XImode, 32));
      DONE;
    }
  else
    FAIL;
})

(define_split
  [(set (match_operand:V8DI 0 "nonimmediate_operand")
        (match_operand:V8DI 1 "general_operand"))]
  "reload_completed"
  [(const_int 0)]
{
  if (register_operand (operands[0], V8DImode)
      && register_operand (operands[1], V8DImode))
    {
      aarch64_simd_emit_reg_reg_move (operands, DImode, 8);
      DONE;
    }
  else if ((register_operand (operands[0], V8DImode)
	    && memory_operand (operands[1], V8DImode))
	   || (memory_operand (operands[0], V8DImode)
	       && register_operand (operands[1], V8DImode)))
    {
      for (int offset = 0; offset < 64; offset += 16)
	emit_move_insn (simplify_gen_subreg (TImode, operands[0],
					     V8DImode, offset),
			simplify_gen_subreg (TImode, operands[1],
					     V8DImode, offset));
      DONE;
    }
  else
    FAIL;
})

(define_expand "aarch64_ld<nregs>r<vstruct_elt>"
  [(match_operand:VSTRUCT_QD 0 "register_operand")
   (match_operand:DI 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[1]);
  set_mem_size (mem, GET_MODE_SIZE (GET_MODE_INNER (<MODE>mode)) * <nregs>);

  emit_insn (gen_aarch64_simd_ld<nregs>r<vstruct_elt> (operands[0], mem));
  DONE;
})

(define_insn "aarch64_ld2<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_2DNX 0 "register_operand" "=w")
	(unspec:VSTRUCT_2DNX [
	  (match_operand:VSTRUCT_2DNX 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD2_DREG))]
  "TARGET_SIMD"
  "ld2\\t{%S0.<Vtype> - %T0.<Vtype>}, %1"
  [(set_attr "type" "neon_load2_2reg<q>")]
)

(define_insn "aarch64_ld2<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_2DX 0 "register_operand" "=w")
	(unspec:VSTRUCT_2DX [
	  (match_operand:VSTRUCT_2DX 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD2_DREG))]
  "TARGET_SIMD"
  "ld1\\t{%S0.1d - %T0.1d}, %1"
  [(set_attr "type" "neon_load1_2reg<q>")]
)

(define_insn "aarch64_ld3<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_3DNX 0 "register_operand" "=w")
	(unspec:VSTRUCT_3DNX [
	  (match_operand:VSTRUCT_3DNX 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD3_DREG))]
  "TARGET_SIMD"
  "ld3\\t{%S0.<Vtype> - %U0.<Vtype>}, %1"
  [(set_attr "type" "neon_load3_3reg<q>")]
)

(define_insn "aarch64_ld3<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_3DX 0 "register_operand" "=w")
	(unspec:VSTRUCT_3DX [
	  (match_operand:VSTRUCT_3DX 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD3_DREG))]
  "TARGET_SIMD"
  "ld1\\t{%S0.1d - %U0.1d}, %1"
  [(set_attr "type" "neon_load1_3reg<q>")]
)

(define_insn "aarch64_ld4<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_4DNX 0 "register_operand" "=w")
	(unspec:VSTRUCT_4DNX [
	  (match_operand:VSTRUCT_4DNX 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD4_DREG))]
  "TARGET_SIMD"
  "ld4\\t{%S0.<Vtype> - %V0.<Vtype>}, %1"
  [(set_attr "type" "neon_load4_4reg<q>")]
)

(define_insn "aarch64_ld4<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_4DX 0 "register_operand" "=w")
	(unspec:VSTRUCT_4DX [
	  (match_operand:VSTRUCT_4DX 1 "aarch64_simd_struct_operand" "Utv")]
	  UNSPEC_LD4_DREG))]
  "TARGET_SIMD"
  "ld1\\t{%S0.1d - %V0.1d}, %1"
  [(set_attr "type" "neon_load1_4reg<q>")]
)

(define_expand "aarch64_ld<nregs><vstruct_elt>"
 [(match_operand:VSTRUCT_D 0 "register_operand")
  (match_operand:DI 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[1]);
  emit_insn (gen_aarch64_ld<nregs><vstruct_elt>_dreg (operands[0], mem));
  DONE;
})

(define_expand "aarch64_ld1<VALL_F16:mode>"
 [(match_operand:VALL_F16 0 "register_operand")
  (match_operand:DI 1 "register_operand")]
  "TARGET_SIMD"
{
  machine_mode mode = <VALL_F16:MODE>mode;
  rtx mem = gen_rtx_MEM (mode, operands[1]);

  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_aarch64_be_ld1<VALL_F16:mode> (operands[0], mem));
  else
    emit_move_insn (operands[0], mem);
  DONE;
})

(define_expand "aarch64_ld<nregs><vstruct_elt>"
 [(match_operand:VSTRUCT_Q 0 "register_operand")
  (match_operand:DI 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[1]);
  emit_insn (gen_aarch64_simd_ld<nregs><vstruct_elt> (operands[0], mem));
  DONE;
})

(define_expand "aarch64_ld1x2<vstruct_elt>"
 [(match_operand:VSTRUCT_2QD 0 "register_operand")
  (match_operand:DI 1 "register_operand")]
  "TARGET_SIMD"
{
  machine_mode mode = <MODE>mode;
  rtx mem = gen_rtx_MEM (mode, operands[1]);

  emit_insn (gen_aarch64_simd_ld1<vstruct_elt>_x2 (operands[0], mem));
  DONE;
})

(define_expand "aarch64_ld<nregs>_lane<vstruct_elt>"
  [(match_operand:VSTRUCT_QD 0 "register_operand")
	(match_operand:DI 1 "register_operand")
	(match_operand:VSTRUCT_QD 2 "register_operand")
	(match_operand:SI 3 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[1]);
  set_mem_size (mem, GET_MODE_SIZE (GET_MODE_INNER (<MODE>mode)) * <nregs>);

  aarch64_simd_lane_bounds (operands[3], 0,
		GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>, NULL);
  emit_insn (gen_aarch64_vec_load_lanes<mode>_lane<vstruct_elt> (operands[0],
				mem, operands[2], operands[3]));
  DONE;
})

;; Permuted-store expanders for neon intrinsics.

;; Permute instructions

;; vec_perm support

(define_expand "vec_perm<mode>"
  [(match_operand:VB 0 "register_operand")
   (match_operand:VB 1 "register_operand")
   (match_operand:VB 2 "register_operand")
   (match_operand:VB 3 "register_operand")]
  "TARGET_SIMD"
{
  aarch64_expand_vec_perm (operands[0], operands[1],
			   operands[2], operands[3], <nunits>);
  DONE;
})

(define_insn "aarch64_qtbl1<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:V16QI 1 "register_operand" "w")
		    (match_operand:VB 2 "register_operand" "w")]
		   UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%0.<Vtype>, {%1.16b}, %2.<Vtype>"
  [(set_attr "type" "neon_tbl1<q>")]
)

(define_insn "aarch64_qtbx1<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "0")
		    (match_operand:V16QI 2 "register_operand" "w")
		    (match_operand:VB 3 "register_operand" "w")]
		   UNSPEC_TBX))]
  "TARGET_SIMD"
  "tbx\\t%0.<Vtype>, {%2.16b}, %3.<Vtype>"
  [(set_attr "type" "neon_tbl1<q>")]
)

;; Two source registers.

(define_insn "aarch64_qtbl2<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:V2x16QI 1 "register_operand" "w")
		      (match_operand:VB 2 "register_operand" "w")]
		      UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%S0.<Vbtype>, {%S1.16b - %T1.16b}, %S2.<Vbtype>"
  [(set_attr "type" "neon_tbl2")]
)

(define_insn "aarch64_qtbx2<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "0")
		      (match_operand:V2x16QI 2 "register_operand" "w")
		      (match_operand:VB 3 "register_operand" "w")]
		      UNSPEC_TBX))]
  "TARGET_SIMD"
  "tbx\\t%S0.<Vbtype>, {%S2.16b - %T2.16b}, %S3.<Vbtype>"
  [(set_attr "type" "neon_tbl2")]
)

;; Three source registers.

(define_insn "aarch64_qtbl3<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:V3x16QI 1 "register_operand" "w")
		      (match_operand:VB 2 "register_operand" "w")]
		      UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%S0.<Vbtype>, {%S1.16b - %U1.16b}, %S2.<Vbtype>"
  [(set_attr "type" "neon_tbl3")]
)

(define_insn "aarch64_qtbx3<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "0")
		      (match_operand:V3x16QI 2 "register_operand" "w")
		      (match_operand:VB 3 "register_operand" "w")]
		      UNSPEC_TBX))]
  "TARGET_SIMD"
  "tbx\\t%S0.<Vbtype>, {%S2.16b - %U2.16b}, %S3.<Vbtype>"
  [(set_attr "type" "neon_tbl3")]
)

;; Four source registers.

(define_insn "aarch64_qtbl4<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:V4x16QI 1 "register_operand" "w")
		      (match_operand:VB 2 "register_operand" "w")]
		      UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%S0.<Vbtype>, {%S1.16b - %V1.16b}, %S2.<Vbtype>"
  [(set_attr "type" "neon_tbl4")]
)

(define_insn "aarch64_qtbx4<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "0")
		      (match_operand:V4x16QI 2 "register_operand" "w")
		      (match_operand:VB 3 "register_operand" "w")]
		      UNSPEC_TBX))]
  "TARGET_SIMD"
  "tbx\\t%S0.<Vbtype>, {%S2.16b - %V2.16b}, %S3.<Vbtype>"
  [(set_attr "type" "neon_tbl4")]
)

(define_insn_and_split "aarch64_combinev16qi"
  [(set (match_operand:V2x16QI 0 "register_operand" "=w")
	(unspec:V2x16QI [(match_operand:V16QI 1 "register_operand" "w")
			 (match_operand:V16QI 2 "register_operand" "w")]
			UNSPEC_CONCAT))]
  "TARGET_SIMD"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  aarch64_split_combinev16qi (operands);
  DONE;
}
[(set_attr "type" "multiple")]
)

;; This instruction's pattern is generated directly by
;; aarch64_expand_vec_perm_const, so any changes to the pattern would
;; need corresponding changes there.
(define_insn "aarch64_<PERMUTE:perm_insn><mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(unspec:VALL_F16 [(match_operand:VALL_F16 1 "register_operand" "w")
			  (match_operand:VALL_F16 2 "register_operand" "w")]
	 PERMUTE))]
  "TARGET_SIMD"
  "<PERMUTE:perm_insn>\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_permute<q>")]
)

;; This instruction's pattern is generated directly by
;; aarch64_expand_vec_perm_const, so any changes to the pattern would
;; need corresponding changes there.  Note that the immediate (third)
;; operand is a lane index not a byte index.
(define_insn "aarch64_ext<mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
        (unspec:VALL_F16 [(match_operand:VALL_F16 1 "register_operand" "w")
			  (match_operand:VALL_F16 2 "register_operand" "w")
			  (match_operand:SI 3 "immediate_operand" "i")]
	 UNSPEC_EXT))]
  "TARGET_SIMD"
{
  operands[3] = GEN_INT (INTVAL (operands[3])
      * GET_MODE_UNIT_SIZE (<MODE>mode));
  return "ext\\t%0.<Vbtype>, %1.<Vbtype>, %2.<Vbtype>, #%3";
}
  [(set_attr "type" "neon_ext<q>")]
)

;; This instruction's pattern is generated directly by
;; aarch64_expand_vec_perm_const, so any changes to the pattern would
;; need corresponding changes there.
(define_insn "aarch64_rev<REVERSE:rev_op><mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(unspec:VALL_F16 [(match_operand:VALL_F16 1 "register_operand" "w")]
                    REVERSE))]
  "TARGET_SIMD"
  "rev<REVERSE:rev_op>\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_rev<q>")]
)

(define_insn "aarch64_st2<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_2DNX 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_2DNX [
		(match_operand:VSTRUCT_2DNX 1 "register_operand" "w")]
		UNSPEC_ST2))]
  "TARGET_SIMD"
  "st2\\t{%S1.<Vtype> - %T1.<Vtype>}, %0"
  [(set_attr "type" "neon_store2_2reg")]
)

(define_insn "aarch64_st2<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_2DX 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_2DX [
		(match_operand:VSTRUCT_2DX 1 "register_operand" "w")]
		UNSPEC_ST2))]
  "TARGET_SIMD"
  "st1\\t{%S1.1d - %T1.1d}, %0"
  [(set_attr "type" "neon_store1_2reg")]
)

(define_insn "aarch64_st3<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_3DNX 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_3DNX [
		(match_operand:VSTRUCT_3DNX 1 "register_operand" "w")]
		UNSPEC_ST3))]
  "TARGET_SIMD"
  "st3\\t{%S1.<Vtype> - %U1.<Vtype>}, %0"
  [(set_attr "type" "neon_store3_3reg")]
)

(define_insn "aarch64_st3<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_3DX 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_3DX [
		(match_operand:VSTRUCT_3DX 1 "register_operand" "w")]
		UNSPEC_ST3))]
  "TARGET_SIMD"
  "st1\\t{%S1.1d - %U1.1d}, %0"
  [(set_attr "type" "neon_store1_3reg")]
)

(define_insn "aarch64_st4<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_4DNX 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_4DNX [
		(match_operand:VSTRUCT_4DNX 1 "register_operand" "w")]
		UNSPEC_ST4))]
  "TARGET_SIMD"
  "st4\\t{%S1.<Vtype> - %V1.<Vtype>}, %0"
  [(set_attr "type" "neon_store4_4reg")]
)

(define_insn "aarch64_st4<vstruct_elt>_dreg"
  [(set (match_operand:VSTRUCT_4DX 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:VSTRUCT_4DX [
		(match_operand:VSTRUCT_4DX 1 "register_operand" "w")]
		UNSPEC_ST4))]
  "TARGET_SIMD"
  "st1\\t{%S1.1d - %V1.1d}, %0"
  [(set_attr "type" "neon_store1_4reg")]
)

(define_expand "aarch64_st<nregs><vstruct_elt>"
 [(match_operand:DI 0 "register_operand")
  (match_operand:VSTRUCT_D 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[0]);
  emit_insn (gen_aarch64_st<nregs><vstruct_elt>_dreg (mem, operands[1]));
  DONE;
})

(define_expand "aarch64_st<nregs><vstruct_elt>"
 [(match_operand:DI 0 "register_operand")
  (match_operand:VSTRUCT_Q 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (<MODE>mode, operands[0]);
  emit_insn (gen_aarch64_simd_st<nregs><vstruct_elt> (mem, operands[1]));
  DONE;
})

(define_expand "aarch64_st<nregs>_lane<vstruct_elt>"
 [(match_operand:DI 0 "register_operand")
  (match_operand:VSTRUCT_QD 1 "register_operand")
  (match_operand:SI 2 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[0]);
  set_mem_size (mem, GET_MODE_SIZE (GET_MODE_INNER (<MODE>mode)) * <nregs>);

  aarch64_simd_lane_bounds (operands[2], 0,
		GET_MODE_NUNITS (<MODE>mode).to_constant () / <nregs>, NULL);
  emit_insn (gen_aarch64_vec_store_lanes<mode>_lane<vstruct_elt> (mem,
					operands[1], operands[2]));
  DONE;
})

(define_expand "aarch64_st1<VALL_F16:mode>"
 [(match_operand:DI 0 "register_operand")
  (match_operand:VALL_F16 1 "register_operand")]
  "TARGET_SIMD"
{
  machine_mode mode = <VALL_F16:MODE>mode;
  rtx mem = gen_rtx_MEM (mode, operands[0]);

  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_aarch64_be_st1<VALL_F16:mode> (mem, operands[1]));
  else
    emit_move_insn (mem, operands[1]);
  DONE;
})

;; Standard pattern name vec_init<mode><Vel>.

(define_expand "vec_init<mode><Vel>"
  [(match_operand:VALL_F16 0 "register_operand")
   (match_operand 1 "" "")]
  "TARGET_SIMD"
{
  aarch64_expand_vector_init (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_init<mode><Vhalf>"
  [(match_operand:VQ_NO2E 0 "register_operand")
   (match_operand 1 "" "")]
  "TARGET_SIMD"
{
  aarch64_expand_vector_init (operands[0], operands[1]);
  DONE;
})

(define_insn "*aarch64_simd_ld1r<mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(vec_duplicate:VALL_F16
	  (match_operand:<VEL> 1 "aarch64_simd_struct_operand" "Utv")))]
  "TARGET_SIMD"
  "ld1r\\t{%0.<Vtype>}, %1"
  [(set_attr "type" "neon_load1_all_lanes")]
)

(define_insn "aarch64_simd_ld1<vstruct_elt>_x2"
  [(set (match_operand:VSTRUCT_2QD 0 "register_operand" "=w")
	(unspec:VSTRUCT_2QD [
	    (match_operand:VSTRUCT_2QD 1 "aarch64_simd_struct_operand" "Utv")]
	    UNSPEC_LD1))]
  "TARGET_SIMD"
  "ld1\\t{%S0.<Vtype> - %T0.<Vtype>}, %1"
  [(set_attr "type" "neon_load1_2reg<q>")]
)


(define_insn "@aarch64_frecpe<mode>"
  [(set (match_operand:VHSDF_HSDF 0 "register_operand" "=w")
	(unspec:VHSDF_HSDF
	 [(match_operand:VHSDF_HSDF 1 "register_operand" "w")]
	 UNSPEC_FRECPE))]
  "TARGET_SIMD"
  "frecpe\t%<v>0<Vmtype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_fp_recpe_<stype><q>")]
)

(define_insn "aarch64_frecpx<mode>"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(unspec:GPF_F16 [(match_operand:GPF_F16 1 "register_operand" "w")]
	 UNSPEC_FRECPX))]
  "TARGET_SIMD"
  "frecpx\t%<s>0, %<s>1"
  [(set_attr "type" "neon_fp_recpx_<GPF_F16:stype>")]
)

(define_insn "@aarch64_frecps<mode>"
  [(set (match_operand:VHSDF_HSDF 0 "register_operand" "=w")
	(unspec:VHSDF_HSDF
	  [(match_operand:VHSDF_HSDF 1 "register_operand" "w")
	  (match_operand:VHSDF_HSDF 2 "register_operand" "w")]
	  UNSPEC_FRECPS))]
  "TARGET_SIMD"
  "frecps\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_fp_recps_<stype><q>")]
)

(define_insn "aarch64_urecpe<mode>"
  [(set (match_operand:VDQ_SI 0 "register_operand" "=w")
        (unspec:VDQ_SI [(match_operand:VDQ_SI 1 "register_operand" "w")]
                UNSPEC_URECPE))]
 "TARGET_SIMD"
 "urecpe\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_recpe_<Vetype><q>")])

;; Standard pattern name vec_extract<mode><Vel>.

(define_expand "vec_extract<mode><Vel>"
  [(match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand")
   (match_operand:VALL_F16 1 "register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_SIMD"
{
    emit_insn
      (gen_aarch64_get_lane<mode> (operands[0], operands[1], operands[2]));
    DONE;
})

;; Extract a 64-bit vector from one half of a 128-bit vector.
(define_expand "vec_extract<mode><Vhalf>"
  [(match_operand:<VHALF> 0 "register_operand")
   (match_operand:VQMOV_NO2E 1 "register_operand")
   (match_operand 2 "immediate_operand")]
  "TARGET_SIMD"
{
  int start = INTVAL (operands[2]);
  if (start != 0 && start != <nunits> / 2)
    FAIL;
  rtx sel = aarch64_gen_stepped_int_parallel (<nunits> / 2, start, 1);
  emit_insn (gen_aarch64_get_half<mode> (operands[0], operands[1], sel));
  DONE;
})

;; Extract a single-element 64-bit vector from one half of a 128-bit vector.
(define_expand "vec_extract<mode><V1half>"
  [(match_operand:<V1HALF> 0 "register_operand")
   (match_operand:VQ_2E 1 "register_operand")
   (match_operand 2 "immediate_operand")]
  "TARGET_SIMD"
{
  /* V1DI and V1DF are rarely used by other patterns, so it should be better
     to hide it in a subreg destination of a normal DI or DF op.  */
  rtx scalar0 = gen_lowpart (<VHALF>mode, operands[0]);
  emit_insn (gen_vec_extract<mode><Vhalf> (scalar0, operands[1], operands[2]));
  DONE;
})

;; aes

(define_insn "aarch64_crypto_aes<aes_op>v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
		[(xor:V16QI
		 (match_operand:V16QI 1 "register_operand" "%0")
		 (match_operand:V16QI 2 "register_operand" "w"))]
         CRYPTO_AES))]
  "TARGET_AES"
  "aes<aes_op>\\t%0.16b, %2.16b"
  [(set_attr "type" "crypto_aese")]
)

(define_insn "aarch64_crypto_aes<aesmc_op>v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "w")]
	 CRYPTO_AESMC))]
  "TARGET_AES"
  "aes<aesmc_op>\\t%0.16b, %1.16b"
  [(set_attr "type" "crypto_aesmc")]
)

;; When AESE/AESMC fusion is enabled we really want to keep the two together
;; and enforce the register dependency without scheduling or register
;; allocation messing up the order or introducing moves inbetween.
;;  Mash the two together during combine.

(define_insn "*aarch64_crypto_aese_fused"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
	  [(unspec:V16QI
	   [(xor:V16QI
		(match_operand:V16QI 1 "register_operand" "%0")
		(match_operand:V16QI 2 "register_operand" "w"))]
	     UNSPEC_AESE)]
	UNSPEC_AESMC))]
  "TARGET_AES
   && aarch64_fusion_enabled_p (AARCH64_FUSE_AES_AESMC)"
  "aese\\t%0.16b, %2.16b\;aesmc\\t%0.16b, %0.16b"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

;; When AESD/AESIMC fusion is enabled we really want to keep the two together
;; and enforce the register dependency without scheduling or register
;; allocation messing up the order or introducing moves inbetween.
;;  Mash the two together during combine.

(define_insn "*aarch64_crypto_aesd_fused"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI
	  [(unspec:V16QI
		    [(xor:V16QI
			(match_operand:V16QI 1 "register_operand" "%0")
			(match_operand:V16QI 2 "register_operand" "w"))]
		UNSPEC_AESD)]
	  UNSPEC_AESIMC))]
  "TARGET_AES
   && aarch64_fusion_enabled_p (AARCH64_FUSE_AES_AESMC)"
  "aesd\\t%0.16b, %2.16b\;aesimc\\t%0.16b, %0.16b"
  [(set_attr "type" "crypto_aese")
   (set_attr "length" "8")]
)

;; sha1

(define_insn "aarch64_crypto_sha1hsi"
  [(set (match_operand:SI 0 "register_operand" "=w")
        (unspec:SI [(match_operand:SI 1
                       "register_operand" "w")]
         UNSPEC_SHA1H))]
  "TARGET_SHA2"
  "sha1h\\t%s0, %s1"
  [(set_attr "type" "crypto_sha1_fast")]
)

(define_insn "aarch64_crypto_sha1hv4si"
  [(set (match_operand:SI 0 "register_operand" "=w")
	(unspec:SI [(vec_select:SI (match_operand:V4SI 1 "register_operand" "w")
		     (parallel [(const_int 0)]))]
	 UNSPEC_SHA1H))]
  "TARGET_SHA2 && !BYTES_BIG_ENDIAN"
  "sha1h\\t%s0, %s1"
  [(set_attr "type" "crypto_sha1_fast")]
)

(define_insn "aarch64_be_crypto_sha1hv4si"
  [(set (match_operand:SI 0 "register_operand" "=w")
	(unspec:SI [(vec_select:SI (match_operand:V4SI 1 "register_operand" "w")
		     (parallel [(const_int 3)]))]
	 UNSPEC_SHA1H))]
  "TARGET_SHA2 && BYTES_BIG_ENDIAN"
  "sha1h\\t%s0, %s1"
  [(set_attr "type" "crypto_sha1_fast")]
)

(define_insn "aarch64_crypto_sha1su1v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")]
         UNSPEC_SHA1SU1))]
  "TARGET_SHA2"
  "sha1su1\\t%0.4s, %2.4s"
  [(set_attr "type" "crypto_sha1_fast")]
)

(define_insn "aarch64_crypto_sha1<sha1_op>v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:SI 2 "register_operand" "w")
                      (match_operand:V4SI 3 "register_operand" "w")]
         CRYPTO_SHA1))]
  "TARGET_SHA2"
  "sha1<sha1_op>\\t%q0, %s2, %3.4s"
  [(set_attr "type" "crypto_sha1_slow")]
)

(define_insn "aarch64_crypto_sha1su0v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")
                      (match_operand:V4SI 3 "register_operand" "w")]
         UNSPEC_SHA1SU0))]
  "TARGET_SHA2"
  "sha1su0\\t%0.4s, %2.4s, %3.4s"
  [(set_attr "type" "crypto_sha1_xor")]
)

;; sha256

(define_insn "aarch64_crypto_sha256h<sha256_op>v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")
                      (match_operand:V4SI 3 "register_operand" "w")]
         CRYPTO_SHA256))]
  "TARGET_SHA2"
  "sha256h<sha256_op>\\t%q0, %q2, %3.4s"
  [(set_attr "type" "crypto_sha256_slow")]
)

(define_insn "aarch64_crypto_sha256su0v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")]
         UNSPEC_SHA256SU0))]
  "TARGET_SHA2"
  "sha256su0\\t%0.4s, %2.4s"
  [(set_attr "type" "crypto_sha256_fast")]
)

(define_insn "aarch64_crypto_sha256su1v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")
                      (match_operand:V4SI 3 "register_operand" "w")]
         UNSPEC_SHA256SU1))]
  "TARGET_SHA2"
  "sha256su1\\t%0.4s, %2.4s, %3.4s"
  [(set_attr "type" "crypto_sha256_slow")]
)

;; sha512

(define_insn "aarch64_crypto_sha512h<sha512_op>qv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=w")
        (unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
                      (match_operand:V2DI 2 "register_operand" "w")
                      (match_operand:V2DI 3 "register_operand" "w")]
         CRYPTO_SHA512))]
  "TARGET_SHA3"
  "sha512h<sha512_op>\\t%q0, %q2, %3.2d"
  [(set_attr "type" "crypto_sha512")]
)

(define_insn "aarch64_crypto_sha512su0qv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=w")
        (unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
                      (match_operand:V2DI 2 "register_operand" "w")]
         UNSPEC_SHA512SU0))]
  "TARGET_SHA3"
  "sha512su0\\t%0.2d, %2.2d"
  [(set_attr "type" "crypto_sha512")]
)

(define_insn "aarch64_crypto_sha512su1qv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=w")
        (unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
                      (match_operand:V2DI 2 "register_operand" "w")
                      (match_operand:V2DI 3 "register_operand" "w")]
         UNSPEC_SHA512SU1))]
  "TARGET_SHA3"
  "sha512su1\\t%0.2d, %2.2d, %3.2d"
  [(set_attr "type" "crypto_sha512")]
)

;; sha3

(define_insn "eor3q<mode>4"
  [(set (match_operand:VQ_I 0 "register_operand" "=w")
	(xor:VQ_I
	 (xor:VQ_I
	  (match_operand:VQ_I 2 "register_operand" "w")
	  (match_operand:VQ_I 3 "register_operand" "w"))
	 (match_operand:VQ_I 1 "register_operand" "w")))]
  "TARGET_SHA3"
  "eor3\\t%0.16b, %1.16b, %2.16b, %3.16b"
  [(set_attr "type" "crypto_sha3")]
)

(define_insn "aarch64_rax1qv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=w")
	(xor:V2DI
	 (rotate:V2DI
	  (match_operand:V2DI 2 "register_operand" "w")
	  (const_int 1))
	 (match_operand:V2DI 1 "register_operand" "w")))]
  "TARGET_SHA3"
  "rax1\\t%0.2d, %1.2d, %2.2d"
  [(set_attr "type" "crypto_sha3")]
)

(define_insn "aarch64_xarqv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=w")
	(rotatert:V2DI
	 (xor:V2DI
	  (match_operand:V2DI 1 "register_operand" "%w")
	  (match_operand:V2DI 2 "register_operand" "w"))
	 (match_operand:SI 3 "aarch64_simd_shift_imm_di" "Usd")))]
  "TARGET_SHA3"
  "xar\\t%0.2d, %1.2d, %2.2d, %3"
  [(set_attr "type" "crypto_sha3")]
)

(define_insn "bcaxq<mode>4"
  [(set (match_operand:VQ_I 0 "register_operand" "=w")
	(xor:VQ_I
	 (and:VQ_I
	  (not:VQ_I (match_operand:VQ_I 3 "register_operand" "w"))
	  (match_operand:VQ_I 2 "register_operand" "w"))
	 (match_operand:VQ_I 1 "register_operand" "w")))]
  "TARGET_SHA3"
  "bcax\\t%0.16b, %1.16b, %2.16b, %3.16b"
  [(set_attr "type" "crypto_sha3")]
)

;; SM3

(define_insn "aarch64_sm3ss1qv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "register_operand" "w")
		      (match_operand:V4SI 2 "register_operand" "w")
		      (match_operand:V4SI 3 "register_operand" "w")]
	 UNSPEC_SM3SS1))]
  "TARGET_SM4"
  "sm3ss1\\t%0.4s, %1.4s, %2.4s, %3.4s"
  [(set_attr "type" "crypto_sm3")]
)


(define_insn "aarch64_sm3tt<sm3tt_op>qv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
		      (match_operand:V4SI 2 "register_operand" "w")
		      (match_operand:V4SI 3 "register_operand" "w")
		      (match_operand:SI 4 "aarch64_imm2" "Ui2")]
	 CRYPTO_SM3TT))]
  "TARGET_SM4"
  "sm3tt<sm3tt_op>\\t%0.4s, %2.4s, %3.4s[%4]"
  [(set_attr "type" "crypto_sm3")]
)

(define_insn "aarch64_sm3partw<sm3part_op>qv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
		      (match_operand:V4SI 2 "register_operand" "w")
		      (match_operand:V4SI 3 "register_operand" "w")]
	 CRYPTO_SM3PART))]
  "TARGET_SM4"
  "sm3partw<sm3part_op>\\t%0.4s, %2.4s, %3.4s"
  [(set_attr "type" "crypto_sm3")]
)

;; SM4

(define_insn "aarch64_sm4eqv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
		      (match_operand:V4SI 2 "register_operand" "w")]
	 UNSPEC_SM4E))]
  "TARGET_SM4"
  "sm4e\\t%0.4s, %2.4s"
  [(set_attr "type" "crypto_sm4")]
)

(define_insn "aarch64_sm4ekeyqv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "register_operand" "w")
		      (match_operand:V4SI 2 "register_operand" "w")]
	 UNSPEC_SM4EKEY))]
  "TARGET_SM4"
  "sm4ekey\\t%0.4s, %1.4s, %2.4s"
  [(set_attr "type" "crypto_sm4")]
)

;; fp16fml

(define_expand "aarch64_fml<f16mac1>l<f16quad>_low<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(unspec:VDQSF
	 [(match_operand:VDQSF 1 "register_operand")
	  (match_operand:<VFMLA_W> 2 "register_operand")
	  (match_operand:<VFMLA_W> 3 "register_operand")]
	 VFMLA16_LOW))]
  "TARGET_F16FML"
{
  rtx p1 = aarch64_simd_vect_par_cnst_half (<VFMLA_W>mode,
					    <nunits> * 2, false);
  rtx p2 = aarch64_simd_vect_par_cnst_half (<VFMLA_W>mode,
					    <nunits> * 2, false);

  emit_insn (gen_aarch64_simd_fml<f16mac1>l<f16quad>_low<mode> (operands[0],
								operands[1],
								operands[2],
								operands[3],
								p1, p2));
  DONE;

})

(define_expand "aarch64_fml<f16mac1>l<f16quad>_high<mode>"
  [(set (match_operand:VDQSF 0 "register_operand")
	(unspec:VDQSF
	 [(match_operand:VDQSF 1 "register_operand")
	  (match_operand:<VFMLA_W> 2 "register_operand")
	  (match_operand:<VFMLA_W> 3 "register_operand")]
	 VFMLA16_HIGH))]
  "TARGET_F16FML"
{
  rtx p1 = aarch64_simd_vect_par_cnst_half (<VFMLA_W>mode, <nunits> * 2, true);
  rtx p2 = aarch64_simd_vect_par_cnst_half (<VFMLA_W>mode, <nunits> * 2, true);

  emit_insn (gen_aarch64_simd_fml<f16mac1>l<f16quad>_high<mode> (operands[0],
								 operands[1],
								 operands[2],
								 operands[3],
								 p1, p2));
  DONE;
})

(define_insn "aarch64_simd_fmlal<f16quad>_low<mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
	(fma:VDQSF
	 (float_extend:VDQSF
	  (vec_select:<VFMLA_SEL_W>
	   (match_operand:<VFMLA_W> 2 "register_operand" "w")
	   (match_operand:<VFMLA_W> 4 "vect_par_cnst_lo_half" "")))
	 (float_extend:VDQSF
	  (vec_select:<VFMLA_SEL_W>
	   (match_operand:<VFMLA_W> 3 "register_operand" "w")
	   (match_operand:<VFMLA_W> 5 "vect_par_cnst_lo_half" "")))
	 (match_operand:VDQSF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal\\t%0.<nunits>s, %2.<nunits>h, %3.<nunits>h"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlsl<f16quad>_low<mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
	(fma:VDQSF
	 (float_extend:VDQSF
	  (neg:<VFMLA_SEL_W>
	   (vec_select:<VFMLA_SEL_W>
	    (match_operand:<VFMLA_W> 2 "register_operand" "w")
	    (match_operand:<VFMLA_W> 4 "vect_par_cnst_lo_half" ""))))
	 (float_extend:VDQSF
	  (vec_select:<VFMLA_SEL_W>
	   (match_operand:<VFMLA_W> 3 "register_operand" "w")
	   (match_operand:<VFMLA_W> 5 "vect_par_cnst_lo_half" "")))
	 (match_operand:VDQSF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl\\t%0.<nunits>s, %2.<nunits>h, %3.<nunits>h"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlal<f16quad>_high<mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
	(fma:VDQSF
	 (float_extend:VDQSF
	  (vec_select:<VFMLA_SEL_W>
	   (match_operand:<VFMLA_W> 2 "register_operand" "w")
	   (match_operand:<VFMLA_W> 4 "vect_par_cnst_hi_half" "")))
	 (float_extend:VDQSF
	  (vec_select:<VFMLA_SEL_W>
	   (match_operand:<VFMLA_W> 3 "register_operand" "w")
	   (match_operand:<VFMLA_W> 5 "vect_par_cnst_hi_half" "")))
	 (match_operand:VDQSF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal2\\t%0.<nunits>s, %2.<nunits>h, %3.<nunits>h"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlsl<f16quad>_high<mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
	(fma:VDQSF
	 (float_extend:VDQSF
	  (neg:<VFMLA_SEL_W>
	   (vec_select:<VFMLA_SEL_W>
	    (match_operand:<VFMLA_W> 2 "register_operand" "w")
	    (match_operand:<VFMLA_W> 4 "vect_par_cnst_hi_half" ""))))
	 (float_extend:VDQSF
	  (vec_select:<VFMLA_SEL_W>
	   (match_operand:<VFMLA_W> 3 "register_operand" "w")
	   (match_operand:<VFMLA_W> 5 "vect_par_cnst_hi_half" "")))
	 (match_operand:VDQSF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl2\\t%0.<nunits>s, %2.<nunits>h, %3.<nunits>h"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_expand "aarch64_fml<f16mac1>l_lane_lowv2sf"
  [(set (match_operand:V2SF 0 "register_operand")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand")
			   (match_operand:V4HF 2 "register_operand")
			   (match_operand:V4HF 3 "register_operand")
			   (match_operand:SI 4 "aarch64_imm2")]
	 VFMLA16_LOW))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V4HFmode, 4, false);
    rtx lane = aarch64_endian_lane_rtx (V4HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>l_lane_lowv2sf (operands[0],
							    operands[1],
							    operands[2],
							    operands[3],
							    p1, lane));
    DONE;
}
)

(define_expand "aarch64_fml<f16mac1>l_lane_highv2sf"
  [(set (match_operand:V2SF 0 "register_operand")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand")
			   (match_operand:V4HF 2 "register_operand")
			   (match_operand:V4HF 3 "register_operand")
			   (match_operand:SI 4 "aarch64_imm2")]
	 VFMLA16_HIGH))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V4HFmode, 4, true);
    rtx lane = aarch64_endian_lane_rtx (V4HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>l_lane_highv2sf (operands[0],
							     operands[1],
							     operands[2],
							     operands[3],
							     p1, lane));
    DONE;
})

(define_insn "aarch64_simd_fmlal_lane_lowv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	   (vec_select:V2HF
	    (match_operand:V4HF 2 "register_operand" "w")
	    (match_operand:V4HF 4 "vect_par_cnst_lo_half" "")))
	 (float_extend:V2SF
	   (vec_duplicate:V2HF
	    (vec_select:HF
	     (match_operand:V4HF 3 "register_operand" "x")
	     (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlsl_lane_lowv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	  (neg:V2HF
	   (vec_select:V2HF
	    (match_operand:V4HF 2 "register_operand" "w")
	    (match_operand:V4HF 4 "vect_par_cnst_lo_half" ""))))
	 (float_extend:V2SF
	  (vec_duplicate:V2HF
	   (vec_select:HF
	    (match_operand:V4HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlal_lane_highv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	   (vec_select:V2HF
	    (match_operand:V4HF 2 "register_operand" "w")
	    (match_operand:V4HF 4 "vect_par_cnst_hi_half" "")))
	 (float_extend:V2SF
	   (vec_duplicate:V2HF
	    (vec_select:HF
	     (match_operand:V4HF 3 "register_operand" "x")
	     (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal2\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlsl_lane_highv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	   (neg:V2HF
	    (vec_select:V2HF
	     (match_operand:V4HF 2 "register_operand" "w")
	     (match_operand:V4HF 4 "vect_par_cnst_hi_half" ""))))
	 (float_extend:V2SF
	   (vec_duplicate:V2HF
	    (vec_select:HF
	     (match_operand:V4HF 3 "register_operand" "x")
	     (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl2\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_expand "aarch64_fml<f16mac1>lq_laneq_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand")
	(unspec:V4SF [(match_operand:V4SF 1 "register_operand")
			   (match_operand:V8HF 2 "register_operand")
			   (match_operand:V8HF 3 "register_operand")
			   (match_operand:SI 4 "aarch64_lane_imm3")]
	 VFMLA16_LOW))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V8HFmode, 8, false);
    rtx lane = aarch64_endian_lane_rtx (V8HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>lq_laneq_lowv4sf (operands[0],
							      operands[1],
							      operands[2],
							      operands[3],
							      p1, lane));
    DONE;
})

(define_expand "aarch64_fml<f16mac1>lq_laneq_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand")
	(unspec:V4SF [(match_operand:V4SF 1 "register_operand")
			   (match_operand:V8HF 2 "register_operand")
			   (match_operand:V8HF 3 "register_operand")
			   (match_operand:SI 4 "aarch64_lane_imm3")]
	 VFMLA16_HIGH))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V8HFmode, 8, true);
    rtx lane = aarch64_endian_lane_rtx (V8HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>lq_laneq_highv4sf (operands[0],
							       operands[1],
							       operands[2],
							       operands[3],
							       p1, lane));
    DONE;
})

(define_insn "aarch64_simd_fmlalq_laneq_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	 (float_extend:V4SF
	  (vec_select:V4HF
	    (match_operand:V8HF 2 "register_operand" "w")
	    (match_operand:V8HF 4 "vect_par_cnst_lo_half" "")))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlslq_laneq_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	  (float_extend:V4SF
	   (neg:V4HF
	    (vec_select:V4HF
	     (match_operand:V8HF 2 "register_operand" "w")
	     (match_operand:V8HF 4 "vect_par_cnst_lo_half" ""))))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlalq_laneq_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	 (float_extend:V4SF
	  (vec_select:V4HF
	    (match_operand:V8HF 2 "register_operand" "w")
	    (match_operand:V8HF 4 "vect_par_cnst_hi_half" "")))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal2\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlslq_laneq_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	 (float_extend:V4SF
	  (neg:V4HF
	   (vec_select:V4HF
	    (match_operand:V8HF 2 "register_operand" "w")
	    (match_operand:V8HF 4 "vect_par_cnst_hi_half" ""))))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl2\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_expand "aarch64_fml<f16mac1>l_laneq_lowv2sf"
  [(set (match_operand:V2SF 0 "register_operand")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand")
		      (match_operand:V4HF 2 "register_operand")
		      (match_operand:V8HF 3 "register_operand")
		      (match_operand:SI 4 "aarch64_lane_imm3")]
	 VFMLA16_LOW))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V4HFmode, 4, false);
    rtx lane = aarch64_endian_lane_rtx (V8HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>l_laneq_lowv2sf (operands[0],
							     operands[1],
							     operands[2],
							     operands[3],
							     p1, lane));
    DONE;

})

(define_expand "aarch64_fml<f16mac1>l_laneq_highv2sf"
  [(set (match_operand:V2SF 0 "register_operand")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand")
		      (match_operand:V4HF 2 "register_operand")
		      (match_operand:V8HF 3 "register_operand")
		      (match_operand:SI 4 "aarch64_lane_imm3")]
	 VFMLA16_HIGH))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V4HFmode, 4, true);
    rtx lane = aarch64_endian_lane_rtx (V8HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>l_laneq_highv2sf (operands[0],
							      operands[1],
							      operands[2],
							      operands[3],
							      p1, lane));
    DONE;

})

(define_insn "aarch64_simd_fmlal_laneq_lowv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	   (vec_select:V2HF
	    (match_operand:V4HF 2 "register_operand" "w")
	    (match_operand:V4HF 4 "vect_par_cnst_lo_half" "")))
	 (float_extend:V2SF
	  (vec_duplicate:V2HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlsl_laneq_lowv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	  (neg:V2HF
	   (vec_select:V2HF
	    (match_operand:V4HF 2 "register_operand" "w")
	    (match_operand:V4HF 4 "vect_par_cnst_lo_half" ""))))
	 (float_extend:V2SF
	  (vec_duplicate:V2HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlal_laneq_highv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	   (vec_select:V2HF
	    (match_operand:V4HF 2 "register_operand" "w")
	    (match_operand:V4HF 4 "vect_par_cnst_hi_half" "")))
	 (float_extend:V2SF
	  (vec_duplicate:V2HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal2\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlsl_laneq_highv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=w")
	(fma:V2SF
	 (float_extend:V2SF
	  (neg:V2HF
	   (vec_select:V2HF
	    (match_operand:V4HF 2 "register_operand" "w")
	    (match_operand:V4HF 4 "vect_par_cnst_hi_half" ""))))
	 (float_extend:V2SF
	  (vec_duplicate:V2HF
	   (vec_select:HF
	    (match_operand:V8HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_lane_imm3" "Ui7")]))))
	 (match_operand:V2SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl2\\t%0.2s, %2.2h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_expand "aarch64_fml<f16mac1>lq_lane_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand")
	(unspec:V4SF [(match_operand:V4SF 1 "register_operand")
		      (match_operand:V8HF 2 "register_operand")
		      (match_operand:V4HF 3 "register_operand")
		      (match_operand:SI 4 "aarch64_imm2")]
	 VFMLA16_LOW))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V8HFmode, 8, false);
    rtx lane = aarch64_endian_lane_rtx (V4HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>lq_lane_lowv4sf (operands[0],
							     operands[1],
							     operands[2],
							     operands[3],
							     p1, lane));
    DONE;
})

(define_expand "aarch64_fml<f16mac1>lq_lane_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand")
	(unspec:V4SF [(match_operand:V4SF 1 "register_operand")
		      (match_operand:V8HF 2 "register_operand")
		      (match_operand:V4HF 3 "register_operand")
		      (match_operand:SI 4 "aarch64_imm2")]
	 VFMLA16_HIGH))]
  "TARGET_F16FML"
{
    rtx p1 = aarch64_simd_vect_par_cnst_half (V8HFmode, 8, true);
    rtx lane = aarch64_endian_lane_rtx (V4HFmode, INTVAL (operands[4]));

    emit_insn (gen_aarch64_simd_fml<f16mac1>lq_lane_highv4sf (operands[0],
							      operands[1],
							      operands[2],
							      operands[3],
							      p1, lane));
    DONE;
})

(define_insn "aarch64_simd_fmlalq_lane_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	 (float_extend:V4SF
	  (vec_select:V4HF
	   (match_operand:V8HF 2 "register_operand" "w")
	   (match_operand:V8HF 4 "vect_par_cnst_lo_half" "")))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V4HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlslq_lane_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	 (float_extend:V4SF
	  (neg:V4HF
	   (vec_select:V4HF
	    (match_operand:V8HF 2 "register_operand" "w")
	    (match_operand:V8HF 4 "vect_par_cnst_lo_half" ""))))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V4HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlalq_lane_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	 (float_extend:V4SF
	  (vec_select:V4HF
	   (match_operand:V8HF 2 "register_operand" "w")
	   (match_operand:V8HF 4 "vect_par_cnst_hi_half" "")))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V4HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlal2\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

(define_insn "aarch64_simd_fmlslq_lane_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(fma:V4SF
	 (float_extend:V4SF
	  (neg:V4HF
	   (vec_select:V4HF
	    (match_operand:V8HF 2 "register_operand" "w")
	    (match_operand:V8HF 4 "vect_par_cnst_hi_half" ""))))
	 (float_extend:V4SF
	  (vec_duplicate:V4HF
	   (vec_select:HF
	    (match_operand:V4HF 3 "register_operand" "x")
	    (parallel [(match_operand:SI 5 "aarch64_imm2" "Ui2")]))))
	 (match_operand:V4SF 1 "register_operand" "0")))]
  "TARGET_F16FML"
  "fmlsl2\\t%0.4s, %2.4h, %3.h[%5]"
  [(set_attr "type" "neon_fp_mul_s")]
)

;; pmull

(define_insn "aarch64_crypto_pmulldi"
  [(set (match_operand:TI 0 "register_operand" "=w")
        (unspec:TI  [(match_operand:DI 1 "register_operand" "w")
		     (match_operand:DI 2 "register_operand" "w")]
		    UNSPEC_PMULL))]
 "TARGET_AES"
 "pmull\\t%0.1q, %1.1d, %2.1d"
  [(set_attr "type" "crypto_pmull")]
)

(define_insn "aarch64_crypto_pmullv2di"
 [(set (match_operand:TI 0 "register_operand" "=w")
       (unspec:TI [(match_operand:V2DI 1 "register_operand" "w")
		   (match_operand:V2DI 2 "register_operand" "w")]
		  UNSPEC_PMULL2))]
  "TARGET_AES"
  "pmull2\\t%0.1q, %1.2d, %2.2d"
  [(set_attr "type" "crypto_pmull")]
)

;; Sign- or zero-extend a 64-bit integer vector to a 128-bit vector.
(define_insn "<optab><Vnarrowq><mode>2"
  [(set (match_operand:VQN 0 "register_operand" "=w")
	(ANY_EXTEND:VQN (match_operand:<VNARROWQ> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "<su>xtl\t%0.<Vtype>, %1.<Vntype>"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_expand "aarch64_<su>xtl<mode>"
  [(set (match_operand:VQN 0 "register_operand" "=w")
	(ANY_EXTEND:VQN (match_operand:<VNARROWQ> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  ""
)

;; Truncate a 128-bit integer vector to a 64-bit vector.
(define_insn "trunc<mode><Vnarrowq>2"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
	(truncate:<VNARROWQ> (match_operand:VQN 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "xtn\t%0.<Vntype>, %1.<Vtype>"
  [(set_attr "type" "neon_move_narrow_q")]
)

(define_insn "aarch64_bfdot<mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
	(plus:VDQSF
	  (unspec:VDQSF
	   [(match_operand:<VBFMLA_W> 2 "register_operand" "w")
	    (match_operand:<VBFMLA_W> 3 "register_operand" "w")]
	    UNSPEC_BFDOT)
	  (match_operand:VDQSF 1 "register_operand" "0")))]
  "TARGET_BF16_SIMD"
  "bfdot\t%0.<Vtype>, %2.<Vbfdottype>, %3.<Vbfdottype>"
  [(set_attr "type" "neon_dot<q>")]
)

(define_insn "aarch64_bfdot_lane<VBF:isquadop><VDQSF:mode>"
  [(set (match_operand:VDQSF 0 "register_operand" "=w")
	(plus:VDQSF
	  (unspec:VDQSF
	   [(match_operand:<VDQSF:VBFMLA_W> 2 "register_operand" "w")
	    (match_operand:VBF 3 "register_operand" "w")
	    (match_operand:SI 4 "const_int_operand" "n")]
	    UNSPEC_BFDOT)
	  (match_operand:VDQSF 1 "register_operand" "0")))]
  "TARGET_BF16_SIMD"
{
  int nunits = GET_MODE_NUNITS (<VBF:MODE>mode).to_constant ();
  int lane = INTVAL (operands[4]);
  operands[4] = gen_int_mode (ENDIAN_LANE_N (nunits / 2, lane), SImode);
  return "bfdot\t%0.<VDQSF:Vtype>, %2.<VDQSF:Vbfdottype>, %3.2h[%4]";
}
  [(set_attr "type" "neon_dot<VDQSF:q>")]
)

;; vget_low/high_bf16
(define_expand "aarch64_vget_lo_halfv8bf"
  [(match_operand:V4BF 0 "register_operand")
   (match_operand:V8BF 1 "register_operand")]
  "TARGET_BF16_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (V8BFmode, 8, false);
  emit_insn (gen_aarch64_get_halfv8bf (operands[0], operands[1], p));
  DONE;
})

(define_expand "aarch64_vget_hi_halfv8bf"
  [(match_operand:V4BF 0 "register_operand")
   (match_operand:V8BF 1 "register_operand")]
  "TARGET_BF16_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (V8BFmode, 8, true);
  emit_insn (gen_aarch64_get_halfv8bf (operands[0], operands[1], p));
  DONE;
})

;; bfmmla
(define_insn "aarch64_bfmmlaqv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
        (plus:V4SF (match_operand:V4SF 1 "register_operand" "0")
                   (unspec:V4SF [(match_operand:V8BF 2 "register_operand" "w")
                                 (match_operand:V8BF 3 "register_operand" "w")]
                    UNSPEC_BFMMLA)))]
  "TARGET_BF16_SIMD"
  "bfmmla\\t%0.4s, %2.8h, %3.8h"
  [(set_attr "type" "neon_fp_mla_s_q")]
)

;; bfmlal<bt>
(define_insn "aarch64_bfmlal<bt>v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
        (plus: V4SF (match_operand:V4SF 1 "register_operand" "0")
                    (unspec:V4SF [(match_operand:V8BF 2 "register_operand" "w")
                                  (match_operand:V8BF 3 "register_operand" "w")]
                     BF_MLA)))]
  "TARGET_BF16_SIMD"
  "bfmlal<bt>\\t%0.4s, %2.8h, %3.8h"
  [(set_attr "type" "neon_fp_mla_s_q")]
)

(define_insn "aarch64_bfmlal<bt>_lane<q>v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
        (plus: V4SF (match_operand:V4SF 1 "register_operand" "0")
                    (unspec:V4SF [(match_operand:V8BF 2 "register_operand" "w")
                                  (match_operand:VBF 3 "register_operand" "x")
                                  (match_operand:SI 4 "const_int_operand" "n")]
                     BF_MLA)))]
  "TARGET_BF16_SIMD"
{
  operands[4] = aarch64_endian_lane_rtx (<MODE>mode, INTVAL (operands[4]));
  return "bfmlal<bt>\\t%0.4s, %2.8h, %3.h[%4]";
}
  [(set_attr "type" "neon_fp_mla_s_scalar_q")]
)

;; 8-bit integer matrix multiply-accumulate
(define_insn "aarch64_simd_<sur>mmlav16qi"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(plus:V4SI
	 (unspec:V4SI [(match_operand:V16QI 2 "register_operand" "w")
		       (match_operand:V16QI 3 "register_operand" "w")] MATMUL)
	 (match_operand:V4SI 1 "register_operand" "0")))]
  "TARGET_I8MM"
  "<sur>mmla\\t%0.4s, %2.16b, %3.16b"
  [(set_attr "type" "neon_mla_s_q")]
)

;; bfcvtn
(define_insn "aarch64_bfcvtn<q><mode>"
  [(set (match_operand:V4SF_TO_BF 0 "register_operand" "=w")
        (unspec:V4SF_TO_BF [(match_operand:V4SF 1 "register_operand" "w")]
                            UNSPEC_BFCVTN))]
  "TARGET_BF16_SIMD"
  "bfcvtn\\t%0.4h, %1.4s"
  [(set_attr "type" "neon_fp_cvt_narrow_s_q")]
)

(define_insn "aarch64_bfcvtn2v8bf"
  [(set (match_operand:V8BF 0 "register_operand" "=w")
        (unspec:V8BF [(match_operand:V8BF 1 "register_operand" "0")
                      (match_operand:V4SF 2 "register_operand" "w")]
                      UNSPEC_BFCVTN2))]
  "TARGET_BF16_SIMD"
  "bfcvtn2\\t%0.8h, %2.4s"
  [(set_attr "type" "neon_fp_cvt_narrow_s_q")]
)

(define_insn "aarch64_bfcvtbf"
  [(set (match_operand:BF 0 "register_operand" "=w")
        (unspec:BF [(match_operand:SF 1 "register_operand" "w")]
                    UNSPEC_BFCVT))]
  "TARGET_BF16_FP"
  "bfcvt\\t%h0, %s1"
  [(set_attr "type" "f_cvt")]
)

;; Use shl/shll/shll2 to convert BF scalar/vector modes to SF modes.
(define_insn "aarch64_vbfcvt<mode>"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(unspec:V4SF [(match_operand:VBF 1 "register_operand" "w")]
		      UNSPEC_BFCVTN))]
  "TARGET_BF16_SIMD"
  "shll\\t%0.4s, %1.4h, #16"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "aarch64_vbfcvt_highv8bf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
	(unspec:V4SF [(match_operand:V8BF 1 "register_operand" "w")]
		      UNSPEC_BFCVTN2))]
  "TARGET_BF16_SIMD"
  "shll2\\t%0.4s, %1.8h, #16"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "aarch64_bfcvtsf"
  [(set (match_operand:SF 0 "register_operand" "=w")
	(unspec:SF [(match_operand:BF 1 "register_operand" "w")]
		    UNSPEC_BFCVT))]
  "TARGET_BF16_FP"
  "shl\\t%d0, %d1, #16"
  [(set_attr "type" "neon_shift_imm")]
)
