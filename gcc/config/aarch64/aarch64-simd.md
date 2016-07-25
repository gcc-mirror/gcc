;; Machine description for AArch64 AdvSIMD architecture.
;; Copyright (C) 2011-2016 Free Software Foundation, Inc.
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
  [(set (match_operand:VALL_F16 0 "nonimmediate_operand" "")
	(match_operand:VALL_F16 1 "general_operand" ""))]
  "TARGET_SIMD"
  "
    if (GET_CODE (operands[0]) == MEM)
      operands[1] = force_reg (<MODE>mode, operands[1]);
  "
)

(define_expand "movmisalign<mode>"
  [(set (match_operand:VALL 0 "nonimmediate_operand" "")
        (match_operand:VALL 1 "general_operand" ""))]
  "TARGET_SIMD"
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
	  (match_operand:<VEL> 1 "register_operand" "r, w")))]
  "TARGET_SIMD"
  "@
   dup\\t%0.<Vtype>, %<vw>1
   dup\\t%0.<Vtype>, %1.<Vetype>[0]"
  [(set_attr "type" "neon_from_gp<q>, neon_dup<q>")]
)

(define_insn "aarch64_simd_dup<mode>"
  [(set (match_operand:VDQF_F16 0 "register_operand" "=w")
	(vec_duplicate:VDQF_F16
	  (match_operand:<VEL> 1 "register_operand" "w")))]
  "TARGET_SIMD"
  "dup\\t%0.<Vtype>, %1.<Vetype>[0]"
  [(set_attr "type" "neon_dup<q>")]
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "dup\\t%0.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_dup<q>")]
)

(define_insn "aarch64_dup_lane_<vswap_width_name><mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(vec_duplicate:VALL_F16
	  (vec_select:<VEL>
	    (match_operand:<VSWAP_WIDTH> 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")])
          )))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
					  INTVAL (operands[2])));
    return "dup\\t%0.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_dup<q>")]
)

(define_insn "*aarch64_simd_mov<mode>"
  [(set (match_operand:VD 0 "nonimmediate_operand"
		"=w, m,  w, ?r, ?w, ?r, w")
	(match_operand:VD 1 "general_operand"
		"m,  w,  w,  w,  r,  r, Dn"))]
  "TARGET_SIMD
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
   switch (which_alternative)
     {
     case 0: return "ldr\\t%d0, %1";
     case 1: return "str\\t%d1, %0";
     case 2: return "orr\t%0.<Vbtype>, %1.<Vbtype>, %1.<Vbtype>";
     case 3: return "umov\t%0, %1.d[0]";
     case 4: return "ins\t%0.d[0], %1";
     case 5: return "mov\t%0, %1";
     case 6:
	return aarch64_output_simd_mov_immediate (operands[1],
						  <MODE>mode, 64);
     default: gcc_unreachable ();
     }
}
  [(set_attr "type" "neon_load1_1reg<q>, neon_store1_1reg<q>,\
                     neon_logic<q>, neon_to_gp<q>, neon_from_gp<q>,\
                     mov_reg, neon_move<q>")]
)

(define_insn "*aarch64_simd_mov<mode>"
  [(set (match_operand:VQ 0 "nonimmediate_operand"
		"=w, m,  w, ?r, ?w, ?r, w")
	(match_operand:VQ 1 "general_operand"
		"m,  w,  w,  w,  r,  r, Dn"))]
  "TARGET_SIMD
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0:
	return "ldr\\t%q0, %1";
    case 1:
	return "str\\t%q1, %0";
    case 2:
	return "orr\t%0.<Vbtype>, %1.<Vbtype>, %1.<Vbtype>";
    case 3:
    case 4:
    case 5:
	return "#";
    case 6:
	return aarch64_output_simd_mov_immediate (operands[1], <MODE>mode, 128);
    default:
	gcc_unreachable ();
    }
}
  [(set_attr "type" "neon_load1_1reg<q>, neon_store1_1reg<q>,\
                     neon_logic<q>, multiple, multiple, multiple,\
                     neon_move<q>")
   (set_attr "length" "4,4,4,8,8,8,4")]
)

(define_insn "load_pair<mode>"
  [(set (match_operand:VD 0 "register_operand" "=w")
	(match_operand:VD 1 "aarch64_mem_pair_operand" "Ump"))
   (set (match_operand:VD 2 "register_operand" "=w")
	(match_operand:VD 3 "memory_operand" "m"))]
  "TARGET_SIMD
   && rtx_equal_p (XEXP (operands[3], 0),
		   plus_constant (Pmode,
				  XEXP (operands[1], 0),
				  GET_MODE_SIZE (<MODE>mode)))"
  "ldp\\t%d0, %d2, %1"
  [(set_attr "type" "neon_ldp")]
)

(define_insn "store_pair<mode>"
  [(set (match_operand:VD 0 "aarch64_mem_pair_operand" "=Ump")
	(match_operand:VD 1 "register_operand" "w"))
   (set (match_operand:VD 2 "memory_operand" "=m")
	(match_operand:VD 3 "register_operand" "w"))]
  "TARGET_SIMD
   && rtx_equal_p (XEXP (operands[2], 0),
		   plus_constant (Pmode,
				  XEXP (operands[0], 0),
				  GET_MODE_SIZE (<MODE>mode)))"
  "stp\\t%d1, %d3, %0"
  [(set_attr "type" "neon_stp")]
)

(define_split
  [(set (match_operand:VQ 0 "register_operand" "")
      (match_operand:VQ 1 "register_operand" ""))]
  "TARGET_SIMD && reload_completed
   && GP_REGNUM_P (REGNO (operands[0]))
   && GP_REGNUM_P (REGNO (operands[1]))"
  [(const_int 0)]
{
  aarch64_simd_emit_reg_reg_move (operands, DImode, 2);
  DONE;
})

(define_split
  [(set (match_operand:VQ 0 "register_operand" "")
        (match_operand:VQ 1 "register_operand" ""))]
  "TARGET_SIMD && reload_completed
   && ((FP_REGNUM_P (REGNO (operands[0])) && GP_REGNUM_P (REGNO (operands[1])))
       || (GP_REGNUM_P (REGNO (operands[0])) && FP_REGNUM_P (REGNO (operands[1]))))"
  [(const_int 0)]
{
  aarch64_split_simd_move (operands[0], operands[1]);
  DONE;
})

(define_expand "aarch64_split_simd_mov<mode>"
  [(set (match_operand:VQ 0)
        (match_operand:VQ 1))]
  "TARGET_SIMD"
  {
    rtx dst = operands[0];
    rtx src = operands[1];

    if (GP_REGNUM_P (REGNO (src)))
      {
        rtx src_low_part = gen_lowpart (<VHALF>mode, src);
        rtx src_high_part = gen_highpart (<VHALF>mode, src);

        emit_insn
          (gen_move_lo_quad_<mode> (dst, src_low_part));
        emit_insn
          (gen_move_hi_quad_<mode> (dst, src_high_part));
      }

    else
      {
        rtx dst_low_part = gen_lowpart (<VHALF>mode, dst);
        rtx dst_high_part = gen_highpart (<VHALF>mode, dst);
        rtx lo = aarch64_simd_vect_par_cnst_half (<MODE>mode, false);
        rtx hi = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);

        emit_insn
          (gen_aarch64_simd_mov_from_<mode>low (dst_low_part, src, lo));
        emit_insn
          (gen_aarch64_simd_mov_from_<mode>high (dst_high_part, src, hi));
      }
    DONE;
  }
)

(define_insn "aarch64_simd_mov_from_<mode>low"
  [(set (match_operand:<VHALF> 0 "register_operand" "=r")
        (vec_select:<VHALF>
          (match_operand:VQ 1 "register_operand" "w")
          (match_operand:VQ 2 "vect_par_cnst_lo_half" "")))]
  "TARGET_SIMD && reload_completed"
  "umov\t%0, %1.d[0]"
  [(set_attr "type" "neon_to_gp<q>")
   (set_attr "length" "4")
  ])

(define_insn "aarch64_simd_mov_from_<mode>high"
  [(set (match_operand:<VHALF> 0 "register_operand" "=r")
        (vec_select:<VHALF>
          (match_operand:VQ 1 "register_operand" "w")
          (match_operand:VQ 2 "vect_par_cnst_hi_half" "")))]
  "TARGET_SIMD && reload_completed"
  "umov\t%0, %1.d[1]"
  [(set_attr "type" "neon_to_gp<q>")
   (set_attr "length" "4")
  ])

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

(define_insn "*aarch64_mul3_elt<mode>"
 [(set (match_operand:VMUL 0 "register_operand" "=w")
    (mult:VMUL
      (vec_duplicate:VMUL
	  (vec_select:<VEL>
	    (match_operand:VMUL 1 "register_operand" "<h_con>")
	    (parallel [(match_operand:SI 2 "immediate_operand")])))
      (match_operand:VMUL 3 "register_operand" "w")))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "<f>mul\\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon<fp>_mul_<stype>_scalar<q>")]
)

(define_insn "*aarch64_mul3_elt_<vswap_width_name><mode>"
  [(set (match_operand:VMUL_CHANGE_NLANES 0 "register_operand" "=w")
     (mult:VMUL_CHANGE_NLANES
       (vec_duplicate:VMUL_CHANGE_NLANES
	  (vec_select:<VEL>
	    (match_operand:<VSWAP_WIDTH> 1 "register_operand" "<h_con>")
	    (parallel [(match_operand:SI 2 "immediate_operand")])))
      (match_operand:VMUL_CHANGE_NLANES 3 "register_operand" "w")))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
					  INTVAL (operands[2])));
    return "<f>mul\\t%0.<Vtype>, %3.<Vtype>, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon<fp>_mul_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_mul3_elt_from_dup<mode>"
 [(set (match_operand:VMUL 0 "register_operand" "=w")
    (mult:VMUL
      (vec_duplicate:VMUL
	    (match_operand:<VEL> 1 "register_operand" "<h_con>"))
      (match_operand:VMUL 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "<f>mul\t%0.<Vtype>, %2.<Vtype>, %1.<Vetype>[0]";
  [(set_attr "type" "neon<fp>_mul_<stype>_scalar<q>")]
)

(define_insn "aarch64_rsqrte<mode>"
  [(set (match_operand:VHSDF_SDF 0 "register_operand" "=w")
	(unspec:VHSDF_SDF [(match_operand:VHSDF_SDF 1 "register_operand" "w")]
		     UNSPEC_RSQRTE))]
  "TARGET_SIMD"
  "frsqrte\\t%<v>0<Vmtype>, %<v>1<Vmtype>"
  [(set_attr "type" "neon_fp_rsqrte_<stype><q>")])

(define_insn "aarch64_rsqrts<mode>"
  [(set (match_operand:VHSDF_SDF 0 "register_operand" "=w")
	(unspec:VHSDF_SDF [(match_operand:VHSDF_SDF 1 "register_operand" "w")
			   (match_operand:VHSDF_SDF 2 "register_operand" "w")]
	 UNSPEC_RSQRTS))]
  "TARGET_SIMD"
  "frsqrts\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_fp_rsqrts_<stype><q>")])

(define_expand "rsqrt<mode>2"
  [(set (match_operand:VALLF 0 "register_operand" "=w")
	(unspec:VALLF [(match_operand:VALLF 1 "register_operand" "w")]
		     UNSPEC_RSQRT))]
  "TARGET_SIMD"
{
  aarch64_emit_approx_sqrt (operands[0], operands[1], true);
  DONE;
})

(define_insn "*aarch64_mul3_elt_to_64v2df"
  [(set (match_operand:DF 0 "register_operand" "=w")
     (mult:DF
       (vec_select:DF
	 (match_operand:V2DF 1 "register_operand" "w")
	 (parallel [(match_operand:SI 2 "immediate_operand")]))
       (match_operand:DF 3 "register_operand" "w")))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (V2DFmode, INTVAL (operands[2])));
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
;; combine with any operation with an integerated ABS step, such
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

(define_insn "abd<mode>_3"
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
	(abs:VDQ_BHSI (minus:VDQ_BHSI
		       (match_operand:VDQ_BHSI 1 "register_operand" "w")
		       (match_operand:VDQ_BHSI 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "sabd\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_abd<q>")]
)

(define_insn "aba<mode>_3"
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
	(plus:VDQ_BHSI (abs:VDQ_BHSI (minus:VDQ_BHSI
			 (match_operand:VDQ_BHSI 1 "register_operand" "w")
			 (match_operand:VDQ_BHSI 2 "register_operand" "w")))
		       (match_operand:VDQ_BHSI 3 "register_operand" "0")))]
  "TARGET_SIMD"
  "saba\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_arith_acc<q>")]
)

(define_insn "fabd<mode>3"
  [(set (match_operand:VHSDF_SDF 0 "register_operand" "=w")
	(abs:VHSDF_SDF
	  (minus:VHSDF_SDF
	    (match_operand:VHSDF_SDF 1 "register_operand" "w")
	    (match_operand:VHSDF_SDF 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "fabd\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_fp_abd_<stype><q>")]
)

(define_insn "and<mode>3"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (and:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		 (match_operand:VDQ_I 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "and\t%0.<Vbtype>, %1.<Vbtype>, %2.<Vbtype>"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "ior<mode>3"
  [(set (match_operand:VDQ_I 0 "register_operand" "=w")
        (ior:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		 (match_operand:VDQ_I 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "orr\t%0.<Vbtype>, %1.<Vbtype>, %2.<Vbtype>"
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
  [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w,w,w")
        (vec_merge:VDQ_BHSI
	    (vec_duplicate:VDQ_BHSI
		(match_operand:<VEL> 1 "aarch64_simd_general_operand" "r,w,Utv"))
	    (match_operand:VDQ_BHSI 3 "register_operand" "0,0,0")
	    (match_operand:SI 2 "immediate_operand" "i,i,i")))]
  "TARGET_SIMD"
  {
   int elt = ENDIAN_LANE_N (<MODE>mode, exact_log2 (INTVAL (operands[2])));
   operands[2] = GEN_INT ((HOST_WIDE_INT) 1 << elt);
   switch (which_alternative)
     {
     case 0:
	return "ins\\t%0.<Vetype>[%p2], %w1";
     case 1:
	return "ins\\t%0.<Vetype>[%p2], %1.<Vetype>[0]";
     case 2:
        return "ld1\\t{%0.<Vetype>}[%p2], %1";
     default:
	gcc_unreachable ();
     }
  }
  [(set_attr "type" "neon_from_gp<q>, neon_ins<q>, neon_load1_1reg<q>")]
)

(define_insn "*aarch64_simd_vec_copy_lane<mode>"
  [(set (match_operand:VALL 0 "register_operand" "=w")
	(vec_merge:VALL
	    (vec_duplicate:VALL
	      (vec_select:<VEL>
		(match_operand:VALL 3 "register_operand" "w")
		(parallel
		  [(match_operand:SI 4 "immediate_operand" "i")])))
	    (match_operand:VALL 1 "register_operand" "0")
	    (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_SIMD"
  {
    int elt = ENDIAN_LANE_N (<MODE>mode, exact_log2 (INTVAL (operands[2])));
    operands[2] = GEN_INT (HOST_WIDE_INT_1 << elt);
    operands[4] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[4])));

    return "ins\t%0.<Vetype>[%p2], %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_ins<q>")]
)

(define_insn "*aarch64_simd_vec_copy_lane_<vswap_width_name><mode>"
  [(set (match_operand:VALL 0 "register_operand" "=w")
	(vec_merge:VALL
	    (vec_duplicate:VALL
	      (vec_select:<VEL>
		(match_operand:<VSWAP_WIDTH> 3 "register_operand" "w")
		(parallel
		  [(match_operand:SI 4 "immediate_operand" "i")])))
	    (match_operand:VALL 1 "register_operand" "0")
	    (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_SIMD"
  {
    int elt = ENDIAN_LANE_N (<MODE>mode, exact_log2 (INTVAL (operands[2])));
    operands[2] = GEN_INT (HOST_WIDE_INT_1 << elt);
    operands[4] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
			   INTVAL (operands[4])));

    return "ins\t%0.<Vetype>[%p2], %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_ins<q>")]
)

(define_insn "aarch64_simd_lshr<mode>"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (lshiftrt:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		     (match_operand:VDQ_I  2 "aarch64_simd_rshift_imm" "Dr")))]
 "TARGET_SIMD"
 "ushr\t%0.<Vtype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm<q>")]
)

(define_insn "aarch64_simd_ashr<mode>"
 [(set (match_operand:VDQ_I 0 "register_operand" "=w")
       (ashiftrt:VDQ_I (match_operand:VDQ_I 1 "register_operand" "w")
		     (match_operand:VDQ_I  2 "aarch64_simd_rshift_imm" "Dr")))]
 "TARGET_SIMD"
 "sshr\t%0.<Vtype>, %1.<Vtype>, %2"
  [(set_attr "type" "neon_shift_imm<q>")]
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
  [(match_operand:VDQ_I 0 "register_operand" "")
   (match_operand:VDQ_I 1 "register_operand" "")
   (match_operand:SI  2 "general_operand" "")]
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
      else
        {
          operands[2] = force_reg (SImode, operands[2]);
        }
    }
  else if (MEM_P (operands[2]))
    {
      operands[2] = force_reg (SImode, operands[2]);
    }

  if (REG_P (operands[2]))
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_aarch64_simd_dup<mode> (tmp,
					     convert_to_mode (<VEL>mode,
							      operands[2],
							      0)));
      emit_insn (gen_aarch64_simd_reg_sshl<mode> (operands[0], operands[1],
						  tmp));
      DONE;
    }
  else
    FAIL;
}
)

(define_expand "lshr<mode>3"
  [(match_operand:VDQ_I 0 "register_operand" "")
   (match_operand:VDQ_I 1 "register_operand" "")
   (match_operand:SI  2 "general_operand" "")]
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
      else
        operands[2] = force_reg (SImode, operands[2]);
    }
  else if (MEM_P (operands[2]))
    {
      operands[2] = force_reg (SImode, operands[2]);
    }

  if (REG_P (operands[2]))
    {
      rtx tmp = gen_reg_rtx (SImode);
      rtx tmp1 = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_negsi2 (tmp, operands[2]));
      emit_insn (gen_aarch64_simd_dup<mode> (tmp1,
					     convert_to_mode (<VEL>mode,
							      tmp, 0)));
      emit_insn (gen_aarch64_simd_reg_shl<mode>_unsigned (operands[0],
							  operands[1],
							  tmp1));
      DONE;
    }
  else
    FAIL;
}
)

(define_expand "ashr<mode>3"
  [(match_operand:VDQ_I 0 "register_operand" "")
   (match_operand:VDQ_I 1 "register_operand" "")
   (match_operand:SI  2 "general_operand" "")]
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
      else
        operands[2] = force_reg (SImode, operands[2]);
    }
  else if (MEM_P (operands[2]))
    {
      operands[2] = force_reg (SImode, operands[2]);
    }

  if (REG_P (operands[2]))
    {
      rtx tmp = gen_reg_rtx (SImode);
      rtx tmp1 = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_negsi2 (tmp, operands[2]));
      emit_insn (gen_aarch64_simd_dup<mode> (tmp1,
					     convert_to_mode (<VEL>mode,
							      tmp, 0)));
      emit_insn (gen_aarch64_simd_reg_shl<mode>_signed (operands[0],
							operands[1],
							tmp1));
      DONE;
    }
  else
    FAIL;
}
)

(define_expand "vashl<mode>3"
 [(match_operand:VDQ_I 0 "register_operand" "")
  (match_operand:VDQ_I 1 "register_operand" "")
  (match_operand:VDQ_I 2 "register_operand" "")]
 "TARGET_SIMD"
{
  emit_insn (gen_aarch64_simd_reg_sshl<mode> (operands[0], operands[1],
					      operands[2]));
  DONE;
})

;; Using mode VDQ_BHSI as there is no V2DImode neg!
;; Negating individual lanes most certainly offsets the
;; gain from vectorization.
(define_expand "vashr<mode>3"
 [(match_operand:VDQ_BHSI 0 "register_operand" "")
  (match_operand:VDQ_BHSI 1 "register_operand" "")
  (match_operand:VDQ_BHSI 2 "register_operand" "")]
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
  [(match_operand:DI 0 "register_operand" "=w")
   (match_operand:DI 1 "register_operand" "w")
   (match_operand:SI 2 "aarch64_shift_imm64_di" "")]
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
 [(match_operand:VDQ_BHSI 0 "register_operand" "")
  (match_operand:VDQ_BHSI 1 "register_operand" "")
  (match_operand:VDQ_BHSI 2 "register_operand" "")]
 "TARGET_SIMD"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  emit (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_aarch64_simd_reg_shl<mode>_unsigned (operands[0], operands[1],
						      neg));
  DONE;
})

(define_expand "aarch64_lshr_simddi"
  [(match_operand:DI 0 "register_operand" "=w")
   (match_operand:DI 1 "register_operand" "w")
   (match_operand:SI 2 "aarch64_shift_imm64_di" "")]
  "TARGET_SIMD"
  {
    if (INTVAL (operands[2]) == 64)
      emit_move_insn (operands[0], const0_rtx);
    else
      emit_insn (gen_lshrdi3 (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_expand "vec_set<mode>"
  [(match_operand:VDQ_BHSI 0 "register_operand")
   (match_operand:<VEL> 1 "register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_SIMD"
  {
    HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);
    emit_insn (gen_aarch64_simd_vec_set<mode> (operands[0], operands[1],
					    GEN_INT (elem), operands[0]));
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

(define_insn "aarch64_simd_vec_setv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=w,w")
        (vec_merge:V2DI
	    (vec_duplicate:V2DI
		(match_operand:DI 1 "register_operand" "r,w"))
	    (match_operand:V2DI 3 "register_operand" "0,0")
	    (match_operand:SI 2 "immediate_operand" "i,i")))]
  "TARGET_SIMD"
  {
    int elt = ENDIAN_LANE_N (V2DImode, exact_log2 (INTVAL (operands[2])));
    operands[2] = GEN_INT ((HOST_WIDE_INT) 1 << elt);
    switch (which_alternative)
      {
      case 0:
	return "ins\\t%0.d[%p2], %1";
      case 1:
        return "ins\\t%0.d[%p2], %1.d[0]";
      default:
	gcc_unreachable ();
      }
  }
  [(set_attr "type" "neon_from_gp, neon_ins_q")]
)

(define_expand "vec_setv2di"
  [(match_operand:V2DI 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_SIMD"
  {
    HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);
    emit_insn (gen_aarch64_simd_vec_setv2di (operands[0], operands[1],
					  GEN_INT (elem), operands[0]));
    DONE;
  }
)

(define_insn "aarch64_simd_vec_set<mode>"
  [(set (match_operand:VDQF_F16 0 "register_operand" "=w")
	(vec_merge:VDQF_F16
	    (vec_duplicate:VDQF_F16
		(match_operand:<VEL> 1 "register_operand" "w"))
	    (match_operand:VDQF_F16 3 "register_operand" "0")
	    (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_SIMD"
  {
    int elt = ENDIAN_LANE_N (<MODE>mode, exact_log2 (INTVAL (operands[2])));

    operands[2] = GEN_INT ((HOST_WIDE_INT)1 << elt);
    return "ins\t%0.<Vetype>[%p2], %1.<Vetype>[0]";
  }
  [(set_attr "type" "neon_ins<q>")]
)

(define_expand "vec_set<mode>"
  [(match_operand:VDQF_F16 0 "register_operand" "+w")
   (match_operand:<VEL> 1 "register_operand" "w")
   (match_operand:SI 2 "immediate_operand" "")]
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "mla\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
					  INTVAL (operands[2])));
    return "mla\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
  }
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "mls\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
					  INTVAL (operands[2])));
    return "mls\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
  }
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
 [(set (match_operand:V2DI 0 "register_operand" "")
       (MAXMIN:V2DI (match_operand:V2DI 1 "register_operand" "")
                    (match_operand:V2DI 2 "register_operand" "")))]
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
  emit_insn (gen_aarch64_vcond_internalv2div2di (operands[0], operands[1],
              operands[2], cmp_fmt, operands[1], operands[2]));
  DONE;
})

;; Pairwise Integer Max/Min operations.
(define_insn "aarch64_<maxmin_uns>p<mode>"
 [(set (match_operand:VDQ_BHSI 0 "register_operand" "=w")
       (unspec:VDQ_BHSI [(match_operand:VDQ_BHSI 1 "register_operand" "w")
			 (match_operand:VDQ_BHSI 2 "register_operand" "w")]
			MAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op>p\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_minmax<q>")]
)

;; Pairwise FP Max/Min operations.
(define_insn "aarch64_<maxmin_uns>p<mode>"
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

;; Move to the low architectural bits of the register.
;; On little-endian this is { operand, zeroes }
;; On big-endian this is { zeroes, operand }

(define_insn "move_lo_quad_internal_<mode>"
  [(set (match_operand:VQ_NO2E 0 "register_operand" "=w,w,w")
	(vec_concat:VQ_NO2E
	  (match_operand:<VHALF> 1 "register_operand" "w,r,r")
	  (vec_duplicate:<VHALF> (const_int 0))))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "@
   dup\\t%d0, %1.d[0]
   fmov\\t%d0, %1
   dup\\t%d0, %1"
  [(set_attr "type" "neon_dup<q>,f_mcr,neon_dup<q>")
   (set_attr "simd" "yes,*,yes")
   (set_attr "fp" "*,yes,*")
   (set_attr "length" "4")]
)

(define_insn "move_lo_quad_internal_<mode>"
  [(set (match_operand:VQ_2E 0 "register_operand" "=w,w,w")
	(vec_concat:VQ_2E
	  (match_operand:<VHALF> 1 "register_operand" "w,r,r")
	  (const_int 0)))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "@
   dup\\t%d0, %1.d[0]
   fmov\\t%d0, %1
   dup\\t%d0, %1"
  [(set_attr "type" "neon_dup<q>,f_mcr,neon_dup<q>")
   (set_attr "simd" "yes,*,yes")
   (set_attr "fp" "*,yes,*")
   (set_attr "length" "4")]
)

(define_insn "move_lo_quad_internal_be_<mode>"
  [(set (match_operand:VQ_NO2E 0 "register_operand" "=w,w,w")
	(vec_concat:VQ_NO2E
	  (vec_duplicate:<VHALF> (const_int 0))
	  (match_operand:<VHALF> 1 "register_operand" "w,r,r")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "@
   dup\\t%d0, %1.d[0]
   fmov\\t%d0, %1
   dup\\t%d0, %1"
  [(set_attr "type" "neon_dup<q>,f_mcr,neon_dup<q>")
   (set_attr "simd" "yes,*,yes")
   (set_attr "fp" "*,yes,*")
   (set_attr "length" "4")]
)

(define_insn "move_lo_quad_internal_be_<mode>"
  [(set (match_operand:VQ_2E 0 "register_operand" "=w,w,w")
	(vec_concat:VQ_2E
	  (const_int 0)
	  (match_operand:<VHALF> 1 "register_operand" "w,r,r")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "@
   dup\\t%d0, %1.d[0]
   fmov\\t%d0, %1
   dup\\t%d0, %1"
  [(set_attr "type" "neon_dup<q>,f_mcr,neon_dup<q>")
   (set_attr "simd" "yes,*,yes")
   (set_attr "fp" "*,yes,*")
   (set_attr "length" "4")]
)

(define_expand "move_lo_quad_<mode>"
  [(match_operand:VQ 0 "register_operand")
   (match_operand:VQ 1 "register_operand")]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_move_lo_quad_internal_be_<mode> (operands[0], operands[1]));
  else
    emit_insn (gen_move_lo_quad_internal_<mode> (operands[0], operands[1]));
  DONE;
}
)

;; Move operand1 to the high architectural bits of the register, keeping
;; the low architectural bits of operand2.
;; For little-endian this is { operand2, operand1 }
;; For big-endian this is { operand1, operand2 }

(define_insn "aarch64_simd_move_hi_quad_<mode>"
  [(set (match_operand:VQ 0 "register_operand" "+w,w")
        (vec_concat:VQ
          (vec_select:<VHALF>
                (match_dup 0)
                (match_operand:VQ 2 "vect_par_cnst_lo_half" ""))
	  (match_operand:<VHALF> 1 "register_operand" "w,r")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "@
   ins\\t%0.d[1], %1.d[0]
   ins\\t%0.d[1], %1"
  [(set_attr "type" "neon_ins")]
)

(define_insn "aarch64_simd_move_hi_quad_be_<mode>"
  [(set (match_operand:VQ 0 "register_operand" "+w,w")
        (vec_concat:VQ
	  (match_operand:<VHALF> 1 "register_operand" "w,r")
          (vec_select:<VHALF>
                (match_dup 0)
                (match_operand:VQ 2 "vect_par_cnst_lo_half" ""))))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "@
   ins\\t%0.d[1], %1.d[0]
   ins\\t%0.d[1], %1"
  [(set_attr "type" "neon_ins")]
)

(define_expand "move_hi_quad_<mode>"
 [(match_operand:VQ 0 "register_operand" "")
  (match_operand:<VHALF> 1 "register_operand" "")]
 "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, false);
  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_aarch64_simd_move_hi_quad_be_<mode> (operands[0],
		    operands[1], p));
  else
    emit_insn (gen_aarch64_simd_move_hi_quad_<mode> (operands[0],
		    operands[1], p));
  DONE;
})

;; Narrowing operations.

;; For doubles.
(define_insn "aarch64_simd_vec_pack_trunc_<mode>"
 [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
       (truncate:<VNARROWQ> (match_operand:VQN 1 "register_operand" "w")))]
 "TARGET_SIMD"
 "xtn\\t%0.<Vntype>, %1.<Vtype>"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_expand "vec_pack_trunc_<mode>"
 [(match_operand:<VNARROWD> 0 "register_operand" "")
  (match_operand:VDN 1 "register_operand" "")
  (match_operand:VDN 2 "register_operand" "")]
 "TARGET_SIMD"
{
  rtx tempreg = gen_reg_rtx (<VDBL>mode);
  int lo = BYTES_BIG_ENDIAN ? 2 : 1;
  int hi = BYTES_BIG_ENDIAN ? 1 : 2;

  emit_insn (gen_move_lo_quad_<Vdbl> (tempreg, operands[lo]));
  emit_insn (gen_move_hi_quad_<Vdbl> (tempreg, operands[hi]));
  emit_insn (gen_aarch64_simd_vec_pack_trunc_<Vdbl> (operands[0], tempreg));
  DONE;
})

;; For quads.

(define_insn "vec_pack_trunc_<mode>"
 [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=&w")
       (vec_concat:<VNARROWQ2>
	 (truncate:<VNARROWQ> (match_operand:VQN 1 "register_operand" "w"))
	 (truncate:<VNARROWQ> (match_operand:VQN 2 "register_operand" "w"))))]
 "TARGET_SIMD"
 {
   if (BYTES_BIG_ENDIAN)
     return "xtn\\t%0.<Vntype>, %2.<Vtype>\;xtn2\\t%0.<V2ntype>, %1.<Vtype>";
   else
     return "xtn\\t%0.<Vntype>, %1.<Vtype>\;xtn2\\t%0.<V2ntype>, %2.<Vtype>";
 }
  [(set_attr "type" "multiple")
   (set_attr "length" "8")]
)

;; Widening operations.

(define_insn "aarch64_simd_vec_unpack<su>_lo_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			       (match_operand:VQW 1 "register_operand" "w")
			       (match_operand:VQW 2 "vect_par_cnst_lo_half" "")
			    )))]
  "TARGET_SIMD"
  "<su>shll\t%0.<Vwtype>, %1.<Vhalftype>, 0"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "aarch64_simd_vec_unpack<su>_hi_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ANY_EXTEND:<VWIDE> (vec_select:<VHALF>
			       (match_operand:VQW 1 "register_operand" "w")
			       (match_operand:VQW 2 "vect_par_cnst_hi_half" "")
			    )))]
  "TARGET_SIMD"
  "<su>shll2\t%0.<Vwtype>, %1.<Vtype>, 0"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_expand "vec_unpack<su>_hi_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand"))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
    emit_insn (gen_aarch64_simd_vec_unpack<su>_hi_<mode> (operands[0],
							  operands[1], p));
    DONE;
  }
)

(define_expand "vec_unpack<su>_lo_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand" ""))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, false);
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

(define_insn "*aarch64_<su>mlal_hi<mode>"
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

(define_insn "*aarch64_<su>mlsl_hi<mode>"
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

(define_insn "*aarch64_<su>mlal<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (plus:<VWIDE>
          (mult:<VWIDE>
            (ANY_EXTEND:<VWIDE>
              (match_operand:VD_BHSI 1 "register_operand" "w"))
            (ANY_EXTEND:<VWIDE>
              (match_operand:VD_BHSI 2 "register_operand" "w")))
          (match_operand:<VWIDE> 3 "register_operand" "0")))]
  "TARGET_SIMD"
  "<su>mlal\t%0.<Vwtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_mla_<Vetype>_long")]
)

(define_insn "*aarch64_<su>mlsl<mode>"
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

(define_expand "vec_widen_<su>mult_lo_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand" ""))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand" ""))]
 "TARGET_SIMD"
 {
   rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, false);
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
  [(match_operand:<VWIDE> 0 "register_operand" "")
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 1 "register_operand" ""))
   (ANY_EXTEND:<VWIDE> (match_operand:VQW 2 "register_operand" ""))]
 "TARGET_SIMD"
 {
   rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
   emit_insn (gen_aarch64_simd_vec_<su>mult_hi_<mode> (operands[0],
						       operands[1],
						       operands[2], p));
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
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (div:VHSDF (match_operand:VHSDF 1 "register_operand" "w")
		  (match_operand:VHSDF 2 "register_operand" "w")))]
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "fmla\\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
					  INTVAL (operands[2])));
    return "fmla\\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
  }
  [(set_attr "type" "neon_fp_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_fma4_elt_from_dup<mode>"
  [(set (match_operand:VMUL 0 "register_operand" "=w")
    (fma:VMUL
      (vec_duplicate:VMUL
	  (match_operand:<VEL> 1 "register_operand" "w"))
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (V2DFmode, INTVAL (operands[2])));
    return "fmla\\t%0.2d, %3.2d, %1.2d[%2]";
  }
  [(set_attr "type" "neon_fp_mla_d_scalar_q")]
)

(define_insn "fnma<mode>4"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(fma:VHSDF
	  (match_operand:VHSDF 1 "register_operand" "w")
          (neg:VHSDF
	    (match_operand:VHSDF 2 "register_operand" "w"))
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "fmls\\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
					  INTVAL (operands[2])));
    return "fmls\\t%0.<Vtype>, %3.<Vtype>, %1.<Vtype>[%2]";
  }
  [(set_attr "type" "neon_fp_mla_<Vetype>_scalar<q>")]
)

(define_insn "*aarch64_fnma4_elt_from_dup<mode>"
  [(set (match_operand:VMUL 0 "register_operand" "=w")
    (fma:VMUL
      (neg:VMUL
        (match_operand:VMUL 2 "register_operand" "w"))
      (vec_duplicate:VMUL
	(match_operand:<VEL> 1 "register_operand" "w"))
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
    operands[2] = GEN_INT (ENDIAN_LANE_N (V2DFmode, INTVAL (operands[2])));
    return "fmls\\t%0.2d, %3.2d, %1.2d[%2]";
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
  [(match_operand:<VWIDE> 0 "register_operand" "")
   (match_operand:VQ_HSF 1 "register_operand" "")]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, false);
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
  [(match_operand:<VWIDE> 0 "register_operand" "")
   (match_operand:VQ_HSF 1 "register_operand" "")]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
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
  [(match_operand:<VDBL> 0 "register_operand" "=w")
   (match_operand:VDF 1 "register_operand" "0")
   (match_operand:<VWIDE> 2 "register_operand" "w")]
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
	(float_truncate:SF
	    (match_operand:DF 1 "register_operand"))
	(float_truncate:SF
	    (match_operand:DF 2 "register_operand"))
	  ))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (V2SFmode);
    int lo = BYTES_BIG_ENDIAN ? 2 : 1;
    int hi = BYTES_BIG_ENDIAN ? 1 : 2;

    emit_insn (gen_move_lo_quad_v2df (tmp, operands[lo]));
    emit_insn (gen_move_hi_quad_v2df (tmp, operands[hi]));
    emit_insn (gen_aarch64_float_truncate_lo_v2sf (operands[0], tmp));
    DONE;
  }
)

;; FP Max/Min
;; Max/Min are introduced by idiom recognition by GCC's mid-end.  An
;; expression like:
;;      a = (b < c) ? b : c;
;; is idiom-matched as MIN_EXPR<b,c> only if -ffinite-math-only is enabled
;; either explicitly or indirectly via -ffast-math.
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

(define_insn "<maxmin_uns><mode>3"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")
		      (match_operand:VHSDF 2 "register_operand" "w")]
		      FMAXMIN_UNS))]
  "TARGET_SIMD"
  "<maxmin_uns_op>\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_minmax_<stype><q>")]
)

;; Auto-vectorized forms for the IEEE-754 fmax()/fmin() functions
(define_insn "<fmaxmin><mode>3"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")
		       (match_operand:VHSDF 2 "register_operand" "w")]
		       FMAXMIN))]
  "TARGET_SIMD"
  "<fmaxmin_op>\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_minmax_<stype><q>")]
)

;; 'across lanes' add.

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand" "=w")
   (unspec:VDQ_I [(match_operand:VDQ_I 1 "register_operand" "w")]
	       UNSPEC_ADDV)]
  "TARGET_SIMD"
  {
    rtx elt = GEN_INT (ENDIAN_LANE_N (<MODE>mode, 0));
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_aarch64_reduc_plus_internal<mode> (scratch, operands[1]));
    emit_insn (gen_aarch64_get_lane<mode> (operands[0], scratch, elt));
    DONE;
  }
)

(define_insn "aarch64_faddp<mode>"
 [(set (match_operand:VHSDF 0 "register_operand" "=w")
       (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")
		      (match_operand:VHSDF 2 "register_operand" "w")]
	UNSPEC_FADDV))]
 "TARGET_SIMD"
 "faddp\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_fp_reduc_add_<stype><q>")]
)

(define_insn "aarch64_reduc_plus_internal<mode>"
 [(set (match_operand:VDQV 0 "register_operand" "=w")
       (unspec:VDQV [(match_operand:VDQV 1 "register_operand" "w")]
		    UNSPEC_ADDV))]
 "TARGET_SIMD"
 "add<VDQV:vp>\\t%<Vetype>0, %1.<Vtype>"
  [(set_attr "type" "neon_reduc_add<q>")]
)

(define_insn "aarch64_reduc_plus_internalv2si"
 [(set (match_operand:V2SI 0 "register_operand" "=w")
       (unspec:V2SI [(match_operand:V2SI 1 "register_operand" "w")]
		    UNSPEC_ADDV))]
 "TARGET_SIMD"
 "addp\\t%0.2s, %1.2s, %1.2s"
  [(set_attr "type" "neon_reduc_add")]
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
       (unspec:V4SF [(match_operand:V4SF 1 "register_operand")]
		    UNSPEC_FADDV))]
 "TARGET_SIMD"
{
  rtx elt = GEN_INT (ENDIAN_LANE_N (V4SFmode, 0));
  rtx scratch = gen_reg_rtx (V4SFmode);
  emit_insn (gen_aarch64_faddpv4sf (scratch, operands[1], operands[1]));
  emit_insn (gen_aarch64_faddpv4sf (scratch, scratch, scratch));
  emit_insn (gen_aarch64_get_lanev4sf (operands[0], scratch, elt));
  DONE;
})

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
;; gimple_fold'd to the REDUC_(MAX|MIN)_EXPR tree code.  (This is FP smax/smin).
(define_expand "reduc_<maxmin_uns>_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (unspec:VHSDF [(match_operand:VHSDF 1 "register_operand")]
		  FMAXMINV)]
  "TARGET_SIMD"
  {
    rtx elt = GEN_INT (ENDIAN_LANE_N (<MODE>mode, 0));
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_aarch64_reduc_<maxmin_uns>_internal<mode> (scratch,
							      operands[1]));
    emit_insn (gen_aarch64_get_lane<mode> (operands[0], scratch, elt));
    DONE;
  }
)

;; Likewise for integer cases, signed and unsigned.
(define_expand "reduc_<maxmin_uns>_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (unspec:VDQ_BHSI [(match_operand:VDQ_BHSI 1 "register_operand")]
		    MAXMINV)]
  "TARGET_SIMD"
  {
    rtx elt = GEN_INT (ENDIAN_LANE_N (<MODE>mode, 0));
    rtx scratch = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_aarch64_reduc_<maxmin_uns>_internal<mode> (scratch,
							      operands[1]));
    emit_insn (gen_aarch64_get_lane<mode> (operands[0], scratch, elt));
    DONE;
  }
)

(define_insn "aarch64_reduc_<maxmin_uns>_internal<mode>"
 [(set (match_operand:VDQV_S 0 "register_operand" "=w")
       (unspec:VDQV_S [(match_operand:VDQV_S 1 "register_operand" "w")]
		    MAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op>v\\t%<Vetype>0, %1.<Vtype>"
  [(set_attr "type" "neon_reduc_minmax<q>")]
)

(define_insn "aarch64_reduc_<maxmin_uns>_internalv2si"
 [(set (match_operand:V2SI 0 "register_operand" "=w")
       (unspec:V2SI [(match_operand:V2SI 1 "register_operand" "w")]
		    MAXMINV))]
 "TARGET_SIMD"
 "<maxmin_uns_op>p\\t%0.2s, %1.2s, %1.2s"
  [(set_attr "type" "neon_reduc_minmax")]
)

(define_insn "aarch64_reduc_<maxmin_uns>_internal<mode>"
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
  [(set (match_operand:VSDQ_I_DI 0 "register_operand" "=w,w,w")
	(xor:VSDQ_I_DI
	   (and:VSDQ_I_DI
	     (xor:VSDQ_I_DI
	       (match_operand:<V_cmp_result> 3 "register_operand" "w,0,w")
	       (match_operand:VSDQ_I_DI 2 "register_operand" "w,w,0"))
	     (match_operand:VSDQ_I_DI 1 "register_operand" "0,w,w"))
	  (match_dup:<V_cmp_result> 3)
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
  [(set (match_operand:VSDQ_I_DI 0 "register_operand" "=w,w,w")
	(xor:VSDQ_I_DI
	   (and:VSDQ_I_DI
	     (xor:VSDQ_I_DI
	       (match_operand:VSDQ_I_DI 3 "register_operand" "w,w,0")
	       (match_operand:VSDQ_I_DI 2 "register_operand" "w,0,w"))
	      (match_operand:VSDQ_I_DI 1 "register_operand" "0,w,w"))
	  (match_dup:VSDQ_I_DI 2)))]
  "TARGET_SIMD"
  "@
  bsl\\t%0.<Vbtype>, %3.<Vbtype>, %2.<Vbtype>
  bit\\t%0.<Vbtype>, %3.<Vbtype>, %1.<Vbtype>
  bif\\t%0.<Vbtype>, %2.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "neon_bsl<q>")]
)

(define_expand "aarch64_simd_bsl<mode>"
  [(match_operand:VALLDIF 0 "register_operand")
   (match_operand:<V_cmp_result> 1 "register_operand")
   (match_operand:VALLDIF 2 "register_operand")
   (match_operand:VALLDIF 3 "register_operand")]
 "TARGET_SIMD"
{
  /* We can't alias operands together if they have different modes.  */
  rtx tmp = operands[0];
  if (FLOAT_MODE_P (<MODE>mode))
    {
      operands[2] = gen_lowpart (<V_cmp_result>mode, operands[2]);
      operands[3] = gen_lowpart (<V_cmp_result>mode, operands[3]);
      tmp = gen_reg_rtx (<V_cmp_result>mode);
    }
  operands[1] = gen_lowpart (<V_cmp_result>mode, operands[1]);
  emit_insn (gen_aarch64_simd_bsl<v_cmp_result>_internal (tmp,
							  operands[1],
							  operands[2],
							  operands[3]));
  if (tmp != operands[0])
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, tmp));

  DONE;
})

(define_expand "aarch64_vcond_internal<mode><mode>"
  [(set (match_operand:VSDQ_I_DI 0 "register_operand")
	(if_then_else:VSDQ_I_DI
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VSDQ_I_DI 4 "register_operand")
	     (match_operand:VSDQ_I_DI 5 "nonmemory_operand")])
	  (match_operand:VSDQ_I_DI 1 "nonmemory_operand")
	  (match_operand:VSDQ_I_DI 2 "nonmemory_operand")))]
  "TARGET_SIMD"
{
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx mask = gen_reg_rtx (<MODE>mode);
  enum rtx_code code = GET_CODE (operands[3]);

  /* Switching OP1 and OP2 is necessary for NE (to output a cmeq insn),
     and desirable for other comparisons if it results in FOO ? -1 : 0
     (this allows direct use of the comparison result without a bsl).  */
  if (code == NE
      || (code != EQ
	  && op1 == CONST0_RTX (<V_cmp_result>mode)
	  && op2 == CONSTM1_RTX (<V_cmp_result>mode)))
    {
      op1 = operands[2];
      op2 = operands[1];
      switch (code)
        {
        case LE: code = GT; break;
        case LT: code = GE; break;
        case GE: code = LT; break;
        case GT: code = LE; break;
        /* No case EQ.  */
        case NE: code = EQ; break;
        case LTU: code = GEU; break;
        case LEU: code = GTU; break;
        case GTU: code = LEU; break;
        case GEU: code = LTU; break;
        default: gcc_unreachable ();
        }
    }

  /* Make sure we can handle the last operand.  */
  switch (code)
    {
    case NE:
      /* Normalized to EQ above.  */
      gcc_unreachable ();

    case LE:
    case LT:
    case GE:
    case GT:
    case EQ:
      /* These instructions have a form taking an immediate zero.  */
      if (operands[5] == CONST0_RTX (<MODE>mode))
        break;
      /* Fall through, as may need to load into register.  */
    default:
      if (!REG_P (operands[5]))
        operands[5] = force_reg (<MODE>mode, operands[5]);
      break;
    }

  switch (code)
    {
    case LT:
      emit_insn (gen_aarch64_cmlt<mode> (mask, operands[4], operands[5]));
      break;

    case GE:
      emit_insn (gen_aarch64_cmge<mode> (mask, operands[4], operands[5]));
      break;

    case LE:
      emit_insn (gen_aarch64_cmle<mode> (mask, operands[4], operands[5]));
      break;

    case GT:
      emit_insn (gen_aarch64_cmgt<mode> (mask, operands[4], operands[5]));
      break;

    case LTU:
      emit_insn (gen_aarch64_cmgtu<mode> (mask, operands[5], operands[4]));
      break;

    case GEU:
      emit_insn (gen_aarch64_cmgeu<mode> (mask, operands[4], operands[5]));
      break;

    case LEU:
      emit_insn (gen_aarch64_cmgeu<mode> (mask, operands[5], operands[4]));
      break;

    case GTU:
      emit_insn (gen_aarch64_cmgtu<mode> (mask, operands[4], operands[5]));
      break;

    /* NE has been normalized to EQ above.  */
    case EQ:
      emit_insn (gen_aarch64_cmeq<mode> (mask, operands[4], operands[5]));
      break;

    default:
      gcc_unreachable ();
    }

    /* If we have (a = (b CMP c) ? -1 : 0);
       Then we can simply move the generated mask.  */

    if (op1 == CONSTM1_RTX (<V_cmp_result>mode)
	&& op2 == CONST0_RTX (<V_cmp_result>mode))
      emit_move_insn (operands[0], mask);
    else
      {
	if (!REG_P (op1))
	  op1 = force_reg (<MODE>mode, op1);
	if (!REG_P (op2))
	  op2 = force_reg (<MODE>mode, op2);
	emit_insn (gen_aarch64_simd_bsl<mode> (operands[0], mask,
					       op1, op2));
      }

  DONE;
})

(define_expand "aarch64_vcond_internal<VDQF_COND:mode><VDQF:mode>"
  [(set (match_operand:VDQF_COND 0 "register_operand")
	(if_then_else:VDQF
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VDQF 4 "register_operand")
	     (match_operand:VDQF 5 "nonmemory_operand")])
	  (match_operand:VDQF_COND 1 "nonmemory_operand")
	  (match_operand:VDQF_COND 2 "nonmemory_operand")))]
  "TARGET_SIMD"
{
  int inverse = 0;
  int use_zero_form = 0;
  int swap_bsl_operands = 0;
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx mask = gen_reg_rtx (<VDQF_COND:V_cmp_result>mode);
  rtx tmp = gen_reg_rtx (<VDQF_COND:V_cmp_result>mode);

  rtx (*base_comparison) (rtx, rtx, rtx);
  rtx (*complimentary_comparison) (rtx, rtx, rtx);

  switch (GET_CODE (operands[3]))
    {
    case GE:
    case GT:
    case LE:
    case LT:
    case EQ:
      if (operands[5] == CONST0_RTX (<MODE>mode))
	{
	  use_zero_form = 1;
	  break;
	}
      /* Fall through.  */
    default:
      if (!REG_P (operands[5]))
	operands[5] = force_reg (<VDQF:MODE>mode, operands[5]);
    }

  switch (GET_CODE (operands[3]))
    {
    case LT:
    case UNLT:
      inverse = 1;
      /* Fall through.  */
    case GE:
    case UNGE:
    case ORDERED:
    case UNORDERED:
      base_comparison = gen_aarch64_cmge<VDQF:mode>;
      complimentary_comparison = gen_aarch64_cmgt<VDQF:mode>;
      break;
    case LE:
    case UNLE:
      inverse = 1;
      /* Fall through.  */
    case GT:
    case UNGT:
      base_comparison = gen_aarch64_cmgt<VDQF:mode>;
      complimentary_comparison = gen_aarch64_cmge<VDQF:mode>;
      break;
    case EQ:
    case NE:
    case UNEQ:
      base_comparison = gen_aarch64_cmeq<VDQF:mode>;
      complimentary_comparison = gen_aarch64_cmeq<VDQF:mode>;
      break;
    default:
      gcc_unreachable ();
    }

  switch (GET_CODE (operands[3]))
    {
    case LT:
    case LE:
    case GT:
    case GE:
    case EQ:
      /* The easy case.  Here we emit one of FCMGE, FCMGT or FCMEQ.
	 As a LT b <=> b GE a && a LE b <=> b GT a.  Our transformations are:
	 a GE b -> a GE b
	 a GT b -> a GT b
	 a LE b -> b GE a
	 a LT b -> b GT a
	 a EQ b -> a EQ b
	 Note that there also exist direct comparison against 0 forms,
	 so catch those as a special case.  */
      if (use_zero_form)
	{
	  inverse = 0;
	  switch (GET_CODE (operands[3]))
	    {
	    case LT:
	      base_comparison = gen_aarch64_cmlt<VDQF:mode>;
	      break;
	    case LE:
	      base_comparison = gen_aarch64_cmle<VDQF:mode>;
	      break;
	    default:
	      /* Do nothing, other zero form cases already have the correct
		 base_comparison.  */
	      break;
	    }
	}

      if (!inverse)
	emit_insn (base_comparison (mask, operands[4], operands[5]));
      else
	emit_insn (complimentary_comparison (mask, operands[5], operands[4]));
      break;
    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
    case NE:
      /* FCM returns false for lanes which are unordered, so if we use
	 the inverse of the comparison we actually want to emit, then
	 swap the operands to BSL, we will end up with the correct result.
	 Note that a NE NaN and NaN NE b are true for all a, b.

	 Our transformations are:
	 a GE b -> !(b GT a)
	 a GT b -> !(b GE a)
	 a LE b -> !(a GT b)
	 a LT b -> !(a GE b)
	 a NE b -> !(a EQ b)  */

      if (inverse)
	emit_insn (base_comparison (mask, operands[4], operands[5]));
      else
	emit_insn (complimentary_comparison (mask, operands[5], operands[4]));

      swap_bsl_operands = 1;
      break;
    case UNEQ:
      /* We check (a > b ||  b > a).  combining these comparisons give us
	 true iff !(a != b && a ORDERED b), swapping the operands to BSL
	 will then give us (a == b ||  a UNORDERED b) as intended.  */

      emit_insn (gen_aarch64_cmgt<VDQF:mode> (mask, operands[4], operands[5]));
      emit_insn (gen_aarch64_cmgt<VDQF:mode> (tmp, operands[5], operands[4]));
      emit_insn (gen_ior<VDQF_COND:v_cmp_result>3 (mask, mask, tmp));
      swap_bsl_operands = 1;
      break;
    case UNORDERED:
       /* Operands are ORDERED iff (a > b || b >= a).
	 Swapping the operands to BSL will give the UNORDERED case.  */
     swap_bsl_operands = 1;
     /* Fall through.  */
    case ORDERED:
      emit_insn (gen_aarch64_cmgt<VDQF:mode> (tmp, operands[4], operands[5]));
      emit_insn (gen_aarch64_cmge<VDQF:mode> (mask, operands[5], operands[4]));
      emit_insn (gen_ior<VDQF_COND:v_cmp_result>3 (mask, mask, tmp));
      break;
    default:
      gcc_unreachable ();
    }

  if (swap_bsl_operands)
    {
      op1 = operands[2];
      op2 = operands[1];
    }

    /* If we have (a = (b CMP c) ? -1 : 0);
       Then we can simply move the generated mask.  */

    if (op1 == CONSTM1_RTX (<VDQF_COND:V_cmp_result>mode)
	&& op2 == CONST0_RTX (<VDQF_COND:V_cmp_result>mode))
      emit_move_insn (operands[0], mask);
    else
      {
	if (!REG_P (op1))
	  op1 = force_reg (<VDQF_COND:MODE>mode, op1);
	if (!REG_P (op2))
	  op2 = force_reg (<VDQF_COND:MODE>mode, op2);
	emit_insn (gen_aarch64_simd_bsl<VDQF_COND:mode> (operands[0], mask,
					       op1, op2));
      }

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
  emit_insn (gen_aarch64_vcond_internal<mode><mode> (operands[0], operands[1],
					       operands[2], operands[3],
					       operands[4], operands[5]));
  DONE;
})

(define_expand "vcond<v_cmp_result><mode>"
  [(set (match_operand:<V_cmp_result> 0 "register_operand")
	(if_then_else:<V_cmp_result>
	  (match_operator 3 "comparison_operator"
	    [(match_operand:VDQF 4 "register_operand")
	     (match_operand:VDQF 5 "nonmemory_operand")])
	  (match_operand:<V_cmp_result> 1 "nonmemory_operand")
	  (match_operand:<V_cmp_result> 2 "nonmemory_operand")))]
  "TARGET_SIMD"
{
  emit_insn (gen_aarch64_vcond_internal<v_cmp_result><mode> (
						operands[0], operands[1],
						operands[2], operands[3],
						operands[4], operands[5]));
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
  emit_insn (gen_aarch64_vcond_internal<mode><mode> (operands[0], operands[1],
					       operands[2], operands[3],
					       operands[4], operands[5]));
  DONE;
})

;; Patterns for AArch64 SIMD Intrinsics.

;; Lane extraction with sign extension to general purpose register.
(define_insn "*aarch64_get_lane_extend<GPI:mode><VDQQH:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(sign_extend:GPI
	  (vec_select:<VEL>
	    (match_operand:VDQQH 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "smov\\t%<GPI:w>0, %1.<VDQQH:Vetype>[%2]";
  }
  [(set_attr "type" "neon_to_gp<q>")]
)

(define_insn "*aarch64_get_lane_zero_extendsi<mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI
	  (vec_select:<VEL>
	    (match_operand:VDQQH 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "umov\\t%w0, %1.<Vetype>[%2]";
  }
  [(set_attr "type" "neon_to_gp<q>")]
)

;; Lane extraction of a value, neither sign nor zero extension
;; is guaranteed so upper bits should be considered undefined.
;; RTL uses GCC vector extension indices throughout so flip only for assembly.
(define_insn "aarch64_get_lane<mode>"
  [(set (match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "=r, w, Utv")
	(vec_select:<VEL>
	  (match_operand:VALL_F16 1 "register_operand" "w, w, w")
	  (parallel [(match_operand:SI 2 "immediate_operand" "i, i, i")])))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
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
  [(set_attr "type" "neon_to_gp<q>, neon_dup<q>, neon_store1_one_lane<q>")]
)

;; In this insn, operand 1 should be low, and operand 2 the high part of the
;; dest vector.

(define_insn "*aarch64_combinez<mode>"
  [(set (match_operand:<VDBL> 0 "register_operand" "=w,w,w")
        (vec_concat:<VDBL>
	   (match_operand:VD_BHSI 1 "general_operand" "w,?r,m")
	   (match_operand:VD_BHSI 2 "aarch64_simd_imm_zero" "Dz,Dz,Dz")))]
  "TARGET_SIMD && !BYTES_BIG_ENDIAN"
  "@
   mov\\t%0.8b, %1.8b
   fmov\t%d0, %1
   ldr\\t%d0, %1"
  [(set_attr "type" "neon_move<q>, neon_from_gp, neon_load1_1reg")
   (set_attr "simd" "yes,*,yes")
   (set_attr "fp" "*,yes,*")]
)

(define_insn "*aarch64_combinez_be<mode>"
  [(set (match_operand:<VDBL> 0 "register_operand" "=w,w,w")
        (vec_concat:<VDBL>
	   (match_operand:VD_BHSI 2 "aarch64_simd_imm_zero" "Dz,Dz,Dz")
	   (match_operand:VD_BHSI 1 "general_operand" "w,?r,m")))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN"
  "@
   mov\\t%0.8b, %1.8b
   fmov\t%d0, %1
   ldr\\t%d0, %1"
  [(set_attr "type" "neon_move<q>, neon_from_gp, neon_load1_1reg")
   (set_attr "simd" "yes,*,yes")
   (set_attr "fp" "*,yes,*")]
)

(define_expand "aarch64_combine<mode>"
  [(match_operand:<VDBL> 0 "register_operand")
   (match_operand:VDC 1 "register_operand")
   (match_operand:VDC 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx op1, op2;
  if (BYTES_BIG_ENDIAN)
    {
      op1 = operands[2];
      op2 = operands[1];
    }
  else
    {
      op1 = operands[1];
      op2 = operands[2];
    }
  emit_insn (gen_aarch64_combine_internal<mode> (operands[0], op1, op2));
  DONE;
}
)

(define_insn_and_split "aarch64_combine_internal<mode>"
  [(set (match_operand:<VDBL> 0 "register_operand" "=&w")
        (vec_concat:<VDBL> (match_operand:VDC 1 "register_operand" "w")
			   (match_operand:VDC 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  if (BYTES_BIG_ENDIAN)
    aarch64_split_simd_combine (operands[0], operands[2], operands[1]);
  else
    aarch64_split_simd_combine (operands[0], operands[1], operands[2]);
  DONE;
}
[(set_attr "type" "multiple")]
)

(define_expand "aarch64_simd_combine<mode>"
  [(match_operand:<VDBL> 0 "register_operand")
   (match_operand:VDC 1 "register_operand")
   (match_operand:VDC 2 "register_operand")]
  "TARGET_SIMD"
  {
    emit_insn (gen_move_lo_quad_<Vdbl> (operands[0], operands[1]));
    emit_insn (gen_move_hi_quad_<Vdbl> (operands[0], operands[2]));
    DONE;
  }
[(set_attr "type" "multiple")]
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


(define_expand "aarch64_saddl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQW 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_saddl<mode>_hi_internal (operands[0], operands[1],
                                                  operands[2], p));
  DONE;
})

(define_expand "aarch64_uaddl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQW 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_uaddl<mode>_hi_internal (operands[0], operands[1],
                                                  operands[2], p));
  DONE;
})

(define_expand "aarch64_ssubl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQW 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_ssubl<mode>_hi_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

(define_expand "aarch64_usubl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQW 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
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
  [(set (match_operand:<VDBLW> 0 "register_operand" "")
	(plus:<VDBLW> (sign_extend:<VDBLW> 
		        (match_operand:VQW 1 "register_operand" ""))
		      (match_operand:<VDBLW> 2 "register_operand" "")))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, false);
    rtx temp = gen_reg_rtx (GET_MODE (operands[0]));

    emit_insn (gen_aarch64_saddw<mode>_internal (temp, operands[2],
						operands[1], p));
    emit_insn (gen_aarch64_saddw2<mode> (operands[0], temp, operands[1]));
    DONE;
  }
)

(define_expand "widen_ssum<mode>3"
  [(set (match_operand:<VWIDE> 0 "register_operand" "")
	(plus:<VWIDE> (sign_extend:<VWIDE>
		        (match_operand:VD_BHSI 1 "register_operand" ""))
		      (match_operand:<VWIDE> 2 "register_operand" "")))]
  "TARGET_SIMD"
{
  emit_insn (gen_aarch64_saddw<mode> (operands[0], operands[2], operands[1]));
  DONE;
})

(define_expand "widen_usum<mode>3"
  [(set (match_operand:<VDBLW> 0 "register_operand" "")
	(plus:<VDBLW> (zero_extend:<VDBLW> 
		        (match_operand:VQW 1 "register_operand" ""))
		      (match_operand:<VDBLW> 2 "register_operand" "")))]
  "TARGET_SIMD"
  {
    rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, false);
    rtx temp = gen_reg_rtx (GET_MODE (operands[0]));

    emit_insn (gen_aarch64_uaddw<mode>_internal (temp, operands[2],
						 operands[1], p));
    emit_insn (gen_aarch64_uaddw2<mode> (operands[0], temp, operands[1]));
    DONE;
  }
)

(define_expand "widen_usum<mode>3"
  [(set (match_operand:<VWIDE> 0 "register_operand" "")
	(plus:<VWIDE> (zero_extend:<VWIDE>
		        (match_operand:VD_BHSI 1 "register_operand" ""))
		      (match_operand:<VWIDE> 2 "register_operand" "")))]
  "TARGET_SIMD"
{
  emit_insn (gen_aarch64_uaddw<mode> (operands[0], operands[2], operands[1]));
  DONE;
})

(define_insn "aarch64_<ANY_EXTEND:su><ADDSUB:optab>w<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ADDSUB:<VWIDE> (match_operand:<VWIDE> 1 "register_operand" "w")
			(ANY_EXTEND:<VWIDE>
			  (match_operand:VD_BHSI 2 "register_operand" "w"))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su><ADDSUB:optab>w\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<ADDSUB:optab>_widen")]
)

(define_insn "aarch64_<ANY_EXTEND:su><ADDSUB:optab>w<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ADDSUB:<VWIDE> (match_operand:<VWIDE> 1 "register_operand" "w")
			(ANY_EXTEND:<VWIDE>
			  (vec_select:<VHALF>
			   (match_operand:VQW 2 "register_operand" "w")
			   (match_operand:VQW 3 "vect_par_cnst_lo_half" "")))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su><ADDSUB:optab>w\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vhalftype>"
  [(set_attr "type" "neon_<ADDSUB:optab>_widen")]
)

(define_insn "aarch64_<ANY_EXTEND:su><ADDSUB:optab>w2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (ADDSUB:<VWIDE> (match_operand:<VWIDE> 1 "register_operand" "w")
			(ANY_EXTEND:<VWIDE>
			  (vec_select:<VHALF>
			   (match_operand:VQW 2 "register_operand" "w")
			   (match_operand:VQW 3 "vect_par_cnst_hi_half" "")))))]
  "TARGET_SIMD"
  "<ANY_EXTEND:su><ADDSUB:optab>w2\\t%0.<Vwtype>, %1.<Vwtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<ADDSUB:optab>_widen")]
)

(define_expand "aarch64_saddw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_saddw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

(define_expand "aarch64_uaddw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_uaddw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})


(define_expand "aarch64_ssubw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_ssubw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

(define_expand "aarch64_usubw2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQW 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_usubw2<mode>_internal (operands[0], operands[1],
						operands[2], p));
  DONE;
})

;; <su><r>h<addsub>.

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

(define_insn "aarch64_<sur><addsub>hn<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
        (unspec:<VNARROWQ> [(match_operand:VQN 1 "register_operand" "w")
			    (match_operand:VQN 2 "register_operand" "w")]
                           ADDSUBHN))]
  "TARGET_SIMD"
  "<sur><addsub>hn\\t%0.<Vntype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_<addsub>_halve_narrow_q")]
)

(define_insn "aarch64_<sur><addsub>hn2<mode>"
  [(set (match_operand:<VNARROWQ2> 0 "register_operand" "=w")
        (unspec:<VNARROWQ2> [(match_operand:<VNARROWQ> 1 "register_operand" "0")
			     (match_operand:VQN 2 "register_operand" "w")
			     (match_operand:VQN 3 "register_operand" "w")]
                            ADDSUBHN2))]
  "TARGET_SIMD"
  "<sur><addsub>hn2\\t%0.<V2ntype>, %2.<Vtype>, %3.<Vtype>"
  [(set_attr "type" "neon_<addsub>_halve_narrow_q")]
)

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

;; fmulx.

(define_insn "aarch64_fmulx<mode>"
  [(set (match_operand:VHSDF_SDF 0 "register_operand" "=w")
	(unspec:VHSDF_SDF
	  [(match_operand:VHSDF_SDF 1 "register_operand" "w")
	   (match_operand:VHSDF_SDF 2 "register_operand" "w")]
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
    operands[3] = GEN_INT (ENDIAN_LANE_N (<VSWAP_WIDTH>mode,
					  INTVAL (operands[3])));
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
    operands[3] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[3])));
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
	    (match_operand:<VEL> 2 "register_operand" "w"))]
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
	   (match_operand:VDQF_DF 2 "register_operand" "w")
	    (parallel [(match_operand:SI 3 "immediate_operand" "i")]))]
	 UNSPEC_FMULX))]
  "TARGET_SIMD"
  {
    operands[3] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[3])));
    return "fmulx\t%<Vetype>0, %<Vetype>1, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "fmul<Vetype>")]
)
;; <su>q<addsub>

(define_insn "aarch64_<su_optab><optab><mode>"
  [(set (match_operand:VSDQ_I 0 "register_operand" "=w")
	(BINQOPS:VSDQ_I (match_operand:VSDQ_I 1 "register_operand" "w")
			  (match_operand:VSDQ_I 2 "register_operand" "w")))]
  "TARGET_SIMD"
  "<su_optab><optab>\\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
  [(set_attr "type" "neon_<optab><q>")]
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

;; sqmovun

(define_insn "aarch64_sqmovun<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
	(unspec:<VNARROWQ> [(match_operand:VSQN_HSDI 1 "register_operand" "w")]
                            UNSPEC_SQXTUN))]
   "TARGET_SIMD"
   "sqxtun\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
   [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

;; sqmovn and uqmovn

(define_insn "aarch64_<sur>qmovn<mode>"
  [(set (match_operand:<VNARROWQ> 0 "register_operand" "=w")
	(unspec:<VNARROWQ> [(match_operand:VSQN_HSDI 1 "register_operand" "w")]
                            SUQMOVN))]
  "TARGET_SIMD"
  "<sur>qxtn\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>"
   [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
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
   operands[3] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[3])));
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
   operands[3] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[3])));
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
   operands[3] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[3])));
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
   operands[3] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[3])));
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
     operands[4] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[4])));
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
     operands[4] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[4])));
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
     operands[4] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[4])));
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
     operands[4] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[4])));
     return
      "sqrdml<SQRDMLH_AS:rdma_as>h\\t%<v>0, %<v>2, %3.<v>[%4]";
   }
   [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

;; vqdml[sa]l

(define_insn "aarch64_sqdml<SBINQOPS:as>l<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		      (match_operand:VSD_HSI 2 "register_operand" "w"))
		(sign_extend:<VWIDE>
		      (match_operand:VSD_HSI 3 "register_operand" "w")))
	      (const_int 1))))]
  "TARGET_SIMD"
  "sqdml<SBINQOPS:as>l\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %<v>3<Vmtype>"
  [(set_attr "type" "neon_sat_mla_<Vetype>_long")]
)

;; vqdml[sa]l_lane

(define_insn "aarch64_sqdml<SBINQOPS:as>l_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:VD_HSI 2 "register_operand" "w"))
	      (sign_extend:<VWIDE>
		(vec_duplicate:VD_HSI
		  (vec_select:<VEL>
		    (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		    (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
              ))
	    (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[4])));
    return
      "sqdml<SBINQOPS:as>l\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdml<SBINQOPS:as>l_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
		(match_operand:VD_HSI 2 "register_operand" "w"))
	      (sign_extend:<VWIDE>
		(vec_duplicate:VD_HSI
		  (vec_select:<VEL>
		    (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		    (parallel [(match_operand:SI 4 "immediate_operand" "i")])))
              ))
	    (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[4])));
    return
      "sqdml<SBINQOPS:as>l\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdml<SBINQOPS:as>l_lane<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
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
    operands[4] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[4])));
    return
      "sqdml<SBINQOPS:as>l\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdml<SBINQOPS:as>l_laneq<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
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
    operands[4] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[4])));
    return
      "sqdml<SBINQOPS:as>l\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

;; vqdml[sa]l_n

(define_insn "aarch64_sqdml<SBINQOPS:as>l_n<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
		      (match_operand:VD_HSI 2 "register_operand" "w"))
		(sign_extend:<VWIDE>
		  (vec_duplicate:VD_HSI
		    (match_operand:<VEL> 3 "register_operand" "<vwx>"))))
	      (const_int 1))))]
  "TARGET_SIMD"
  "sqdml<SBINQOPS:as>l\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

;; sqdml[as]l2

(define_insn "aarch64_sqdml<SBINQOPS:as>l2<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
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
  "sqdml<SBINQOPS:as>l2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %<v>3<Vmtype>"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmlal2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:VQ_HSI 3 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlal2<mode>_internal (operands[0], operands[1],
						  operands[2], operands[3], p));
  DONE;
})

(define_expand "aarch64_sqdmlsl2<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:VQ_HSI 3 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlsl2<mode>_internal (operands[0], operands[1],
						  operands[2], operands[3], p));
  DONE;
})

;; vqdml[sa]l2_lane

(define_insn "aarch64_sqdml<SBINQOPS:as>l2_lane<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
                  (vec_select:<VHALF>
                    (match_operand:VQ_HSI 2 "register_operand" "w")
                    (match_operand:VQ_HSI 5 "vect_par_cnst_hi_half" "")))
		(sign_extend:<VWIDE>
                  (vec_duplicate:<VHALF>
		    (vec_select:<VEL>
		      (match_operand:<VCOND> 3 "register_operand" "<vwx>")
		      (parallel [(match_operand:SI 4 "immediate_operand" "i")])
		    ))))
	      (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[4])));
    return
     "sqdml<SBINQOPS:as>l2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_insn "aarch64_sqdml<SBINQOPS:as>l2_laneq<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	      (mult:<VWIDE>
		(sign_extend:<VWIDE>
                  (vec_select:<VHALF>
                    (match_operand:VQ_HSI 2 "register_operand" "w")
                    (match_operand:VQ_HSI 5 "vect_par_cnst_hi_half" "")))
		(sign_extend:<VWIDE>
                  (vec_duplicate:<VHALF>
		    (vec_select:<VEL>
		      (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
		      (parallel [(match_operand:SI 4 "immediate_operand" "i")])
		    ))))
	      (const_int 1))))]
  "TARGET_SIMD"
  {
    operands[4] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[4])));
    return
     "sqdml<SBINQOPS:as>l2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[%4]";
  }
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmlal2_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:<VCOND> 3 "register_operand" "<vwx>")
   (match_operand:SI 4 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlal2_lane<mode>_internal (operands[0], operands[1],
						       operands[2], operands[3],
						       operands[4], p));
  DONE;
})

(define_expand "aarch64_sqdmlal2_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
   (match_operand:SI 4 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlal2_laneq<mode>_internal (operands[0], operands[1],
						       operands[2], operands[3],
						       operands[4], p));
  DONE;
})

(define_expand "aarch64_sqdmlsl2_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:<VCOND> 3 "register_operand" "<vwx>")
   (match_operand:SI 4 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlsl2_lane<mode>_internal (operands[0], operands[1],
						       operands[2], operands[3],
						       operands[4], p));
  DONE;
})

(define_expand "aarch64_sqdmlsl2_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:<VCONQ> 3 "register_operand" "<vwx>")
   (match_operand:SI 4 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlsl2_laneq<mode>_internal (operands[0], operands[1],
						       operands[2], operands[3],
						       operands[4], p));
  DONE;
})

(define_insn "aarch64_sqdml<SBINQOPS:as>l2_n<mode>_internal"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
        (SBINQOPS:<VWIDE>
	  (match_operand:<VWIDE> 1 "register_operand" "0")
	  (ss_ashift:<VWIDE>
	    (mult:<VWIDE>
	      (sign_extend:<VWIDE>
                (vec_select:<VHALF>
                  (match_operand:VQ_HSI 2 "register_operand" "w")
                  (match_operand:VQ_HSI 4 "vect_par_cnst_hi_half" "")))
	      (sign_extend:<VWIDE>
                (vec_duplicate:<VHALF>
		  (match_operand:<VEL> 3 "register_operand" "<vwx>"))))
	    (const_int 1))))]
  "TARGET_SIMD"
  "sqdml<SBINQOPS:as>l2\\t%<vw2>0<Vmwtype>, %<v>2<Vmtype>, %3.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mla_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmlal2_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:<VEL> 3 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlal2_n<mode>_internal (operands[0], operands[1],
						    operands[2], operands[3],
						    p));
  DONE;
})

(define_expand "aarch64_sqdmlsl2_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:<VWIDE> 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")
   (match_operand:<VEL> 3 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmlsl2_n<mode>_internal (operands[0], operands[1],
						    operands[2], operands[3],
						    p));
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
	       (sign_extend:<VWIDE>
                 (vec_duplicate:VD_HSI
                   (vec_select:<VEL>
		     (match_operand:<VCOND> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[3])));
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
	       (sign_extend:<VWIDE>
                 (vec_duplicate:VD_HSI
                   (vec_select:<VEL>
		     (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[3])));
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
    operands[3] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[3])));
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
    operands[3] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[3])));
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
	       (sign_extend:<VWIDE>
                 (vec_duplicate:VD_HSI
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
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQ_HSI 1 "register_operand" "w")
   (match_operand:VQ_HSI 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
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
	       (sign_extend:<VWIDE>
                 (vec_duplicate:<VHALF>
                   (vec_select:<VEL>
		     (match_operand:<VCOND> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = GEN_INT (ENDIAN_LANE_N (<VCOND>mode, INTVAL (operands[3])));
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
	       (sign_extend:<VWIDE>
                 (vec_duplicate:<VHALF>
                   (vec_select:<VEL>
		     (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
		     (parallel [(match_operand:SI 3 "immediate_operand" "i")])))
	       ))
	     (const_int 1)))]
  "TARGET_SIMD"
  {
    operands[3] = GEN_INT (ENDIAN_LANE_N (<VCONQ>mode, INTVAL (operands[3])));
    return "sqdmull2\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[%3]";
  }
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmull2_lane<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQ_HSI 1 "register_operand" "w")
   (match_operand:<VCOND> 2 "register_operand" "<vwx>")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
  emit_insn (gen_aarch64_sqdmull2_lane<mode>_internal (operands[0], operands[1],
						       operands[2], operands[3],
						       p));
  DONE;
})

(define_expand "aarch64_sqdmull2_laneq<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQ_HSI 1 "register_operand" "w")
   (match_operand:<VCONQ> 2 "register_operand" "<vwx>")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
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
	       (sign_extend:<VWIDE>
                 (vec_duplicate:<VHALF>
                   (match_operand:<VEL> 2 "register_operand" "<vwx>")))
	       )
	     (const_int 1)))]
  "TARGET_SIMD"
  "sqdmull2\\t%<vw2>0<Vmwtype>, %<v>1<Vmtype>, %2.<Vetype>[0]"
  [(set_attr "type" "neon_sat_mul_<Vetype>_scalar_long")]
)

(define_expand "aarch64_sqdmull2_n<mode>"
  [(match_operand:<VWIDE> 0 "register_operand" "=w")
   (match_operand:VQ_HSI 1 "register_operand" "w")
   (match_operand:<VEL> 2 "register_operand" "w")]
  "TARGET_SIMD"
{
  rtx p = aarch64_simd_vect_par_cnst_half (<MODE>mode, true);
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

;; vshll_n

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
        (unspec:<VNARROWQ> [(match_operand:VSQN_HSDI 1 "register_operand" "w")
			    (match_operand:SI 2
			      "aarch64_simd_shift_imm_offset_<ve_mode>" "i")]
			   VQSHRN_N))]
  "TARGET_SIMD"
  "<sur>q<r>shr<u>n\\t%<vn2>0<Vmntype>, %<v>1<Vmtype>, %2"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)


;; cm(eq|ge|gt|lt|le)
;; Note, we have constraints for Dz and Z as different expanders
;; have different ideas of what should be passed to this pattern.

(define_insn "aarch64_cm<optab><mode>"
  [(set (match_operand:<V_cmp_result> 0 "register_operand" "=w,w")
	(neg:<V_cmp_result>
	  (COMPARISONS:<V_cmp_result>
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
  "reload_completed"
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
  [(set (match_operand:<V_cmp_result> 0 "register_operand" "=w")
	(neg:<V_cmp_result>
	  (UCOMPARISONS:<V_cmp_result>
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
  "reload_completed"
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
;; we don't have any insns using ne, and aarch64_vcond_internal outputs
;; not (neg (eq (and x y) 0))
;; which is rewritten by simplify_rtx as
;; plus (eq (and x y) 0) -1.

(define_insn "aarch64_cmtst<mode>"
  [(set (match_operand:<V_cmp_result> 0 "register_operand" "=w")
	(plus:<V_cmp_result>
	  (eq:<V_cmp_result>
	    (and:VDQ_I
	      (match_operand:VDQ_I 1 "register_operand" "w")
	      (match_operand:VDQ_I 2 "register_operand" "w"))
	    (match_operand:VDQ_I 3 "aarch64_simd_imm_zero"))
	  (match_operand:<V_cmp_result> 4 "aarch64_simd_imm_minus_one")))
  ]
  "TARGET_SIMD"
  "cmtst\t%<v>0<Vmtype>, %<v>1<Vmtype>, %<v>2<Vmtype>"
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
  "reload_completed"
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
  [(set (match_operand:<V_cmp_result> 0 "register_operand" "=w,w")
	(neg:<V_cmp_result>
	  (COMPARISONS:<V_cmp_result>
	    (match_operand:VHSDF_SDF 1 "register_operand" "w,w")
	    (match_operand:VHSDF_SDF 2 "aarch64_simd_reg_or_zero" "w,YDz")
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
  [(set (match_operand:<V_cmp_result> 0 "register_operand" "=w")
	(neg:<V_cmp_result>
	  (FAC_COMPARISONS:<V_cmp_result>
	    (abs:VHSDF_SDF
	      (match_operand:VHSDF_SDF 1 "register_operand" "w"))
	    (abs:VHSDF_SDF
	      (match_operand:VHSDF_SDF 2 "register_operand" "w"))
  )))]
  "TARGET_SIMD"
  "fac<n_optab>\t%<v>0<Vmtype>, %<v><cmp_1><Vmtype>, %<v><cmp_2><Vmtype>"
  [(set_attr "type" "neon_fp_compare_<stype><q>")]
)

;; addp

(define_insn "aarch64_addp<mode>"
  [(set (match_operand:VD_BHSI 0 "register_operand" "=w")
        (unspec:VD_BHSI
          [(match_operand:VD_BHSI 1 "register_operand" "w")
	   (match_operand:VD_BHSI 2 "register_operand" "w")]
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
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(sqrt:VHSDF (match_operand:VHSDF 1 "register_operand" "w")))]
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

(define_insn "aarch64_simd_ld2<mode>"
  [(set (match_operand:OI 0 "register_operand" "=w")
	(unspec:OI [(match_operand:OI 1 "aarch64_simd_struct_operand" "Utv")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD2))]
  "TARGET_SIMD"
  "ld2\\t{%S0.<Vtype> - %T0.<Vtype>}, %1"
  [(set_attr "type" "neon_load2_2reg<q>")]
)

(define_insn "aarch64_simd_ld2r<mode>"
  [(set (match_operand:OI 0 "register_operand" "=w")
       (unspec:OI [(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
                   (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY) ]
                  UNSPEC_LD2_DUP))]
  "TARGET_SIMD"
  "ld2r\\t{%S0.<Vtype> - %T0.<Vtype>}, %1"
  [(set_attr "type" "neon_load2_all_lanes<q>")]
)

(define_insn "aarch64_vec_load_lanesoi_lane<mode>"
  [(set (match_operand:OI 0 "register_operand" "=w")
	(unspec:OI [(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
		    (match_operand:OI 2 "register_operand" "0")
		    (match_operand:SI 3 "immediate_operand" "i")
		    (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY) ]
		   UNSPEC_LD2_LANE))]
  "TARGET_SIMD"
  {
    operands[3] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[3])));
    return "ld2\\t{%S0.<Vetype> - %T0.<Vetype>}[%3], %1";
  }
  [(set_attr "type" "neon_load2_one_lane")]
)

(define_expand "vec_load_lanesoi<mode>"
  [(set (match_operand:OI 0 "register_operand" "=w")
	(unspec:OI [(match_operand:OI 1 "aarch64_simd_struct_operand" "Utv")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD2))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (OImode);
      rtx mask = aarch64_reverse_mask (<MODE>mode);
      emit_insn (gen_aarch64_simd_ld2<mode> (tmp, operands[1]));
      emit_insn (gen_aarch64_rev_reglistoi (operands[0], tmp, mask));
    }
  else
    emit_insn (gen_aarch64_simd_ld2<mode> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_st2<mode>"
  [(set (match_operand:OI 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:OI [(match_operand:OI 1 "register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST2))]
  "TARGET_SIMD"
  "st2\\t{%S1.<Vtype> - %T1.<Vtype>}, %0"
  [(set_attr "type" "neon_store2_2reg<q>")]
)

;; RTL uses GCC vector extension indices, so flip only for assembly.
(define_insn "aarch64_vec_store_lanesoi_lane<mode>"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:OI 1 "register_operand" "w")
		    (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)
		    (match_operand:SI 2 "immediate_operand" "i")]
		   UNSPEC_ST2_LANE))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "st2\\t{%S1.<Vetype> - %T1.<Vetype>}[%2], %0";
  }
  [(set_attr "type" "neon_store2_one_lane<q>")]
)

(define_expand "vec_store_lanesoi<mode>"
  [(set (match_operand:OI 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:OI [(match_operand:OI 1 "register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST2))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (OImode);
      rtx mask = aarch64_reverse_mask (<MODE>mode);
      emit_insn (gen_aarch64_rev_reglistoi (tmp, operands[1], mask));
      emit_insn (gen_aarch64_simd_st2<mode> (operands[0], tmp));
    }
  else
    emit_insn (gen_aarch64_simd_st2<mode> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_ld3<mode>"
  [(set (match_operand:CI 0 "register_operand" "=w")
	(unspec:CI [(match_operand:CI 1 "aarch64_simd_struct_operand" "Utv")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD3))]
  "TARGET_SIMD"
  "ld3\\t{%S0.<Vtype> - %U0.<Vtype>}, %1"
  [(set_attr "type" "neon_load3_3reg<q>")]
)

(define_insn "aarch64_simd_ld3r<mode>"
  [(set (match_operand:CI 0 "register_operand" "=w")
       (unspec:CI [(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
                   (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY) ]
                  UNSPEC_LD3_DUP))]
  "TARGET_SIMD"
  "ld3r\\t{%S0.<Vtype> - %U0.<Vtype>}, %1"
  [(set_attr "type" "neon_load3_all_lanes<q>")]
)

(define_insn "aarch64_vec_load_lanesci_lane<mode>"
  [(set (match_operand:CI 0 "register_operand" "=w")
	(unspec:CI [(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
		    (match_operand:CI 2 "register_operand" "0")
		    (match_operand:SI 3 "immediate_operand" "i")
		    (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD3_LANE))]
  "TARGET_SIMD"
{
    operands[3] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[3])));
    return "ld3\\t{%S0.<Vetype> - %U0.<Vetype>}[%3], %1";
}
  [(set_attr "type" "neon_load3_one_lane")]
)

(define_expand "vec_load_lanesci<mode>"
  [(set (match_operand:CI 0 "register_operand" "=w")
	(unspec:CI [(match_operand:CI 1 "aarch64_simd_struct_operand" "Utv")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD3))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (CImode);
      rtx mask = aarch64_reverse_mask (<MODE>mode);
      emit_insn (gen_aarch64_simd_ld3<mode> (tmp, operands[1]));
      emit_insn (gen_aarch64_rev_reglistci (operands[0], tmp, mask));
    }
  else
    emit_insn (gen_aarch64_simd_ld3<mode> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_st3<mode>"
  [(set (match_operand:CI 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:CI [(match_operand:CI 1 "register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST3))]
  "TARGET_SIMD"
  "st3\\t{%S1.<Vtype> - %U1.<Vtype>}, %0"
  [(set_attr "type" "neon_store3_3reg<q>")]
)

;; RTL uses GCC vector extension indices, so flip only for assembly.
(define_insn "aarch64_vec_store_lanesci_lane<mode>"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:CI 1 "register_operand" "w")
		     (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)
		     (match_operand:SI 2 "immediate_operand" "i")]
		    UNSPEC_ST3_LANE))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "st3\\t{%S1.<Vetype> - %U1.<Vetype>}[%2], %0";
  }
  [(set_attr "type" "neon_store3_one_lane<q>")]
)

(define_expand "vec_store_lanesci<mode>"
  [(set (match_operand:CI 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:CI [(match_operand:CI 1 "register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST3))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (CImode);
      rtx mask = aarch64_reverse_mask (<MODE>mode);
      emit_insn (gen_aarch64_rev_reglistci (tmp, operands[1], mask));
      emit_insn (gen_aarch64_simd_st3<mode> (operands[0], tmp));
    }
  else
    emit_insn (gen_aarch64_simd_st3<mode> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_ld4<mode>"
  [(set (match_operand:XI 0 "register_operand" "=w")
	(unspec:XI [(match_operand:XI 1 "aarch64_simd_struct_operand" "Utv")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD4))]
  "TARGET_SIMD"
  "ld4\\t{%S0.<Vtype> - %V0.<Vtype>}, %1"
  [(set_attr "type" "neon_load4_4reg<q>")]
)

(define_insn "aarch64_simd_ld4r<mode>"
  [(set (match_operand:XI 0 "register_operand" "=w")
       (unspec:XI [(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
                   (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY) ]
                  UNSPEC_LD4_DUP))]
  "TARGET_SIMD"
  "ld4r\\t{%S0.<Vtype> - %V0.<Vtype>}, %1"
  [(set_attr "type" "neon_load4_all_lanes<q>")]
)

(define_insn "aarch64_vec_load_lanesxi_lane<mode>"
  [(set (match_operand:XI 0 "register_operand" "=w")
	(unspec:XI [(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")
		    (match_operand:XI 2 "register_operand" "0")
		    (match_operand:SI 3 "immediate_operand" "i")
		    (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD4_LANE))]
  "TARGET_SIMD"
{
    operands[3] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[3])));
    return "ld4\\t{%S0.<Vetype> - %V0.<Vetype>}[%3], %1";
}
  [(set_attr "type" "neon_load4_one_lane")]
)

(define_expand "vec_load_lanesxi<mode>"
  [(set (match_operand:XI 0 "register_operand" "=w")
	(unspec:XI [(match_operand:XI 1 "aarch64_simd_struct_operand" "Utv")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_LD4))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (XImode);
      rtx mask = aarch64_reverse_mask (<MODE>mode);
      emit_insn (gen_aarch64_simd_ld4<mode> (tmp, operands[1]));
      emit_insn (gen_aarch64_rev_reglistxi (operands[0], tmp, mask));
    }
  else
    emit_insn (gen_aarch64_simd_ld4<mode> (operands[0], operands[1]));
  DONE;
})

(define_insn "aarch64_simd_st4<mode>"
  [(set (match_operand:XI 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:XI [(match_operand:XI 1 "register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST4))]
  "TARGET_SIMD"
  "st4\\t{%S1.<Vtype> - %V1.<Vtype>}, %0"
  [(set_attr "type" "neon_store4_4reg<q>")]
)

;; RTL uses GCC vector extension indices, so flip only for assembly.
(define_insn "aarch64_vec_store_lanesxi_lane<mode>"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:XI 1 "register_operand" "w")
		     (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)
		     (match_operand:SI 2 "immediate_operand" "i")]
		    UNSPEC_ST4_LANE))]
  "TARGET_SIMD"
  {
    operands[2] = GEN_INT (ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[2])));
    return "st4\\t{%S1.<Vetype> - %V1.<Vetype>}[%2], %0";
  }
  [(set_attr "type" "neon_store4_one_lane<q>")]
)

(define_expand "vec_store_lanesxi<mode>"
  [(set (match_operand:XI 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:XI [(match_operand:XI 1 "register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST4))]
  "TARGET_SIMD"
{
  if (BYTES_BIG_ENDIAN)
    {
      rtx tmp = gen_reg_rtx (XImode);
      rtx mask = aarch64_reverse_mask (<MODE>mode);
      emit_insn (gen_aarch64_rev_reglistxi (tmp, operands[1], mask));
      emit_insn (gen_aarch64_simd_st4<mode> (operands[0], tmp));
    }
  else
    emit_insn (gen_aarch64_simd_st4<mode> (operands[0], operands[1]));
  DONE;
})

(define_insn_and_split "aarch64_rev_reglist<mode>"
[(set (match_operand:VSTRUCT 0 "register_operand" "=&w")
	(unspec:VSTRUCT
	           [(match_operand:VSTRUCT 1 "register_operand" "w")
		    (match_operand:V16QI 2 "register_operand" "w")]
                   UNSPEC_REV_REGLIST))]
  "TARGET_SIMD"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  int i;
  int nregs = GET_MODE_SIZE (<MODE>mode) / UNITS_PER_VREG;
  for (i = 0; i < nregs; i++)
    {
      rtx op0 = gen_rtx_REG (V16QImode, REGNO (operands[0]) + i);
      rtx op1 = gen_rtx_REG (V16QImode, REGNO (operands[1]) + i);
      emit_insn (gen_aarch64_tbl1v16qi (op0, op1, operands[2]));
    }
  DONE;
}
  [(set_attr "type" "neon_tbl1_q")
   (set_attr "length" "<insn_count>")]
)

;; Reload patterns for AdvSIMD register list operands.

(define_expand "mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand" "")
	(match_operand:VSTRUCT 1 "general_operand" ""))]
  "TARGET_SIMD"
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
})

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

(define_insn "*aarch64_be_movoi"
  [(set (match_operand:OI 0 "nonimmediate_operand" "=w,m,w")
	(match_operand:OI 1 "general_operand"      " w,w,m"))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN
   && (register_operand (operands[0], OImode)
       || register_operand (operands[1], OImode))"
  "@
   #
   stp\\t%q1, %R1, %0
   ldp\\t%q0, %R0, %1"
  [(set_attr "type" "multiple,neon_stp_q,neon_ldp_q")
   (set_attr "length" "8,4,4")]
)

(define_insn "*aarch64_be_movci"
  [(set (match_operand:CI 0 "nonimmediate_operand" "=w,o,w")
	(match_operand:CI 1 "general_operand"      " w,w,o"))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN
   && (register_operand (operands[0], CImode)
       || register_operand (operands[1], CImode))"
  "#"
  [(set_attr "type" "multiple")
   (set_attr "length" "12,4,4")]
)

(define_insn "*aarch64_be_movxi"
  [(set (match_operand:XI 0 "nonimmediate_operand" "=w,o,w")
	(match_operand:XI 1 "general_operand"      " w,w,o"))]
  "TARGET_SIMD && BYTES_BIG_ENDIAN
   && (register_operand (operands[0], XImode)
       || register_operand (operands[1], XImode))"
  "#"
  [(set_attr "type" "multiple")
   (set_attr "length" "16,4,4")]
)

(define_split
  [(set (match_operand:OI 0 "register_operand")
	(match_operand:OI 1 "register_operand"))]
  "TARGET_SIMD && reload_completed"
  [(const_int 0)]
{
  aarch64_simd_emit_reg_reg_move (operands, TImode, 2);
  DONE;
})

(define_split
  [(set (match_operand:CI 0 "nonimmediate_operand")
	(match_operand:CI 1 "general_operand"))]
  "TARGET_SIMD && reload_completed"
  [(const_int 0)]
{
  if (register_operand (operands[0], CImode)
      && register_operand (operands[1], CImode))
    {
      aarch64_simd_emit_reg_reg_move (operands, TImode, 3);
      DONE;
    }
  else if (BYTES_BIG_ENDIAN)
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
  [(set (match_operand:XI 0 "nonimmediate_operand")
	(match_operand:XI 1 "general_operand"))]
  "TARGET_SIMD && reload_completed"
  [(const_int 0)]
{
  if (register_operand (operands[0], XImode)
      && register_operand (operands[1], XImode))
    {
      aarch64_simd_emit_reg_reg_move (operands, TImode, 4);
      DONE;
    }
  else if (BYTES_BIG_ENDIAN)
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

(define_expand "aarch64_ld<VSTRUCT:nregs>r<VALLDIF:mode>"
  [(match_operand:VSTRUCT 0 "register_operand" "=w")
   (match_operand:DI 1 "register_operand" "w")
   (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[1]);
  set_mem_size (mem, GET_MODE_SIZE (GET_MODE_INNER (<VALLDIF:MODE>mode))
		     * <VSTRUCT:nregs>);

  emit_insn (gen_aarch64_simd_ld<VSTRUCT:nregs>r<VALLDIF:mode> (operands[0],
								mem));
  DONE;
})

(define_insn "aarch64_ld2<mode>_dreg"
  [(set (match_operand:OI 0 "register_operand" "=w")
	(subreg:OI
	  (vec_concat:<VRL2>
	    (vec_concat:<VDBL>
	     (unspec:VD
		[(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
		UNSPEC_LD2)
	     (vec_duplicate:VD (const_int 0)))
	    (vec_concat:<VDBL>
	     (unspec:VD [(match_dup 1)]
			UNSPEC_LD2)
	     (vec_duplicate:VD (const_int 0)))) 0))]
  "TARGET_SIMD"
  "ld2\\t{%S0.<Vtype> - %T0.<Vtype>}, %1"
  [(set_attr "type" "neon_load2_2reg<q>")]
)

(define_insn "aarch64_ld2<mode>_dreg"
  [(set (match_operand:OI 0 "register_operand" "=w")
	(subreg:OI
	  (vec_concat:<VRL2>
	    (vec_concat:<VDBL>
	     (unspec:DX
		[(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
		UNSPEC_LD2)
	     (const_int 0))
	    (vec_concat:<VDBL>
	     (unspec:DX [(match_dup 1)]
			UNSPEC_LD2)
	     (const_int 0))) 0))]
  "TARGET_SIMD"
  "ld1\\t{%S0.1d - %T0.1d}, %1"
  [(set_attr "type" "neon_load1_2reg<q>")]
)

(define_insn "aarch64_ld3<mode>_dreg"
  [(set (match_operand:CI 0 "register_operand" "=w")
	(subreg:CI
	 (vec_concat:<VRL3>
	  (vec_concat:<VRL2>
	    (vec_concat:<VDBL>
	     (unspec:VD
		[(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
		UNSPEC_LD3)
	     (vec_duplicate:VD (const_int 0)))
	    (vec_concat:<VDBL>
	     (unspec:VD [(match_dup 1)]
			UNSPEC_LD3)
	     (vec_duplicate:VD (const_int 0))))
	  (vec_concat:<VDBL>
	     (unspec:VD [(match_dup 1)]
			UNSPEC_LD3)
	     (vec_duplicate:VD (const_int 0)))) 0))]
  "TARGET_SIMD"
  "ld3\\t{%S0.<Vtype> - %U0.<Vtype>}, %1"
  [(set_attr "type" "neon_load3_3reg<q>")]
)

(define_insn "aarch64_ld3<mode>_dreg"
  [(set (match_operand:CI 0 "register_operand" "=w")
	(subreg:CI
	 (vec_concat:<VRL3>
	  (vec_concat:<VRL2>
	    (vec_concat:<VDBL>
	     (unspec:DX
		[(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
		UNSPEC_LD3)
	     (const_int 0))
	    (vec_concat:<VDBL>
	     (unspec:DX [(match_dup 1)]
			UNSPEC_LD3)
	     (const_int 0)))
	  (vec_concat:<VDBL>
	     (unspec:DX [(match_dup 1)]
			UNSPEC_LD3)
	     (const_int 0))) 0))]
  "TARGET_SIMD"
  "ld1\\t{%S0.1d - %U0.1d}, %1"
  [(set_attr "type" "neon_load1_3reg<q>")]
)

(define_insn "aarch64_ld4<mode>_dreg"
  [(set (match_operand:XI 0 "register_operand" "=w")
	(subreg:XI
	 (vec_concat:<VRL4>
	   (vec_concat:<VRL2>
	     (vec_concat:<VDBL>
	       (unspec:VD
		[(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
		UNSPEC_LD4)
	       (vec_duplicate:VD (const_int 0)))
	      (vec_concat:<VDBL>
	        (unspec:VD [(match_dup 1)]
			UNSPEC_LD4)
	        (vec_duplicate:VD (const_int 0))))
	   (vec_concat:<VRL2>
	     (vec_concat:<VDBL>
	       (unspec:VD [(match_dup 1)]
			UNSPEC_LD4)
	       (vec_duplicate:VD (const_int 0)))
	     (vec_concat:<VDBL>
	       (unspec:VD [(match_dup 1)]
			UNSPEC_LD4)
	       (vec_duplicate:VD (const_int 0))))) 0))]
  "TARGET_SIMD"
  "ld4\\t{%S0.<Vtype> - %V0.<Vtype>}, %1"
  [(set_attr "type" "neon_load4_4reg<q>")]
)

(define_insn "aarch64_ld4<mode>_dreg"
  [(set (match_operand:XI 0 "register_operand" "=w")
	(subreg:XI
	 (vec_concat:<VRL4>
	   (vec_concat:<VRL2>
	     (vec_concat:<VDBL>
	       (unspec:DX
		[(match_operand:BLK 1 "aarch64_simd_struct_operand" "Utv")]
		UNSPEC_LD4)
	       (const_int 0))
	      (vec_concat:<VDBL>
	        (unspec:DX [(match_dup 1)]
			UNSPEC_LD4)
	        (const_int 0)))
	   (vec_concat:<VRL2>
	     (vec_concat:<VDBL>
	       (unspec:DX [(match_dup 1)]
			UNSPEC_LD4)
	       (const_int 0))
	     (vec_concat:<VDBL>
	       (unspec:DX [(match_dup 1)]
			UNSPEC_LD4)
	       (const_int 0)))) 0))]
  "TARGET_SIMD"
  "ld1\\t{%S0.1d - %V0.1d}, %1"
  [(set_attr "type" "neon_load1_4reg<q>")]
)

(define_expand "aarch64_ld<VSTRUCT:nregs><VDC:mode>"
 [(match_operand:VSTRUCT 0 "register_operand" "=w")
  (match_operand:DI 1 "register_operand" "r")
  (unspec:VDC [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[1]);
  set_mem_size (mem, <VSTRUCT:nregs> * 8);

  emit_insn (gen_aarch64_ld<VSTRUCT:nregs><VDC:mode>_dreg (operands[0], mem));
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

(define_expand "aarch64_ld<VSTRUCT:nregs><VQ:mode>"
 [(match_operand:VSTRUCT 0 "register_operand" "=w")
  (match_operand:DI 1 "register_operand" "r")
  (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_SIMD"
{
  machine_mode mode = <VSTRUCT:MODE>mode;
  rtx mem = gen_rtx_MEM (mode, operands[1]);

  emit_insn (gen_aarch64_simd_ld<VSTRUCT:nregs><VQ:mode> (operands[0], mem));
  DONE;
})

(define_expand "aarch64_ld<VSTRUCT:nregs>_lane<VALLDIF:mode>"
  [(match_operand:VSTRUCT 0 "register_operand" "=w")
	(match_operand:DI 1 "register_operand" "w")
	(match_operand:VSTRUCT 2 "register_operand" "0")
	(match_operand:SI 3 "immediate_operand" "i")
	(unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[1]);
  set_mem_size (mem, GET_MODE_SIZE (GET_MODE_INNER (<VALLDIF:MODE>mode))
		     * <VSTRUCT:nregs>);

  aarch64_simd_lane_bounds (operands[3], 0,
			    GET_MODE_NUNITS (<VALLDIF:MODE>mode),
			    NULL);
  emit_insn (gen_aarch64_vec_load_lanes<VSTRUCT:mode>_lane<VALLDIF:mode> (
	operands[0], mem, operands[2], operands[3]));
  DONE;
})

;; Expanders for builtins to extract vector registers from large
;; opaque integer modes.

;; D-register list.

(define_expand "aarch64_get_dreg<VSTRUCT:mode><VDC:mode>"
 [(match_operand:VDC 0 "register_operand" "=w")
  (match_operand:VSTRUCT 1 "register_operand" "w")
  (match_operand:SI 2 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  int part = INTVAL (operands[2]);
  rtx temp = gen_reg_rtx (<VDC:VDBL>mode);
  int offset = part * 16;

  emit_move_insn (temp, gen_rtx_SUBREG (<VDC:VDBL>mode, operands[1], offset));
  emit_move_insn (operands[0], gen_lowpart (<VDC:MODE>mode, temp));
  DONE;
})

;; Q-register list.

(define_expand "aarch64_get_qreg<VSTRUCT:mode><VQ:mode>"
 [(match_operand:VQ 0 "register_operand" "=w")
  (match_operand:VSTRUCT 1 "register_operand" "w")
  (match_operand:SI 2 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  int part = INTVAL (operands[2]);
  int offset = part * 16;

  emit_move_insn (operands[0],
		  gen_rtx_SUBREG (<VQ:MODE>mode, operands[1], offset));
  DONE;
})

;; Permuted-store expanders for neon intrinsics.

;; Permute instructions

;; vec_perm support

(define_expand "vec_perm_const<mode>"
  [(match_operand:VALL_F16 0 "register_operand")
   (match_operand:VALL_F16 1 "register_operand")
   (match_operand:VALL_F16 2 "register_operand")
   (match_operand:<V_cmp_result> 3)]
  "TARGET_SIMD"
{
  if (aarch64_expand_vec_perm_const (operands[0], operands[1],
				     operands[2], operands[3]))
    DONE;
  else
    FAIL;
})

(define_expand "vec_perm<mode>"
  [(match_operand:VB 0 "register_operand")
   (match_operand:VB 1 "register_operand")
   (match_operand:VB 2 "register_operand")
   (match_operand:VB 3 "register_operand")]
  "TARGET_SIMD"
{
  aarch64_expand_vec_perm (operands[0], operands[1],
			   operands[2], operands[3]);
  DONE;
})

(define_insn "aarch64_tbl1<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:V16QI 1 "register_operand" "w")
		    (match_operand:VB 2 "register_operand" "w")]
		   UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%0.<Vtype>, {%1.16b}, %2.<Vtype>"
  [(set_attr "type" "neon_tbl1<q>")]
)

;; Two source registers.

(define_insn "aarch64_tbl2v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
	(unspec:V16QI [(match_operand:OI 1 "register_operand" "w")
		       (match_operand:V16QI 2 "register_operand" "w")]
		      UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%0.16b, {%S1.16b - %T1.16b}, %2.16b"
  [(set_attr "type" "neon_tbl2_q")]
)

(define_insn "aarch64_tbl3<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:OI 1 "register_operand" "w")
		      (match_operand:VB 2 "register_operand" "w")]
		      UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%S0.<Vbtype>, {%S1.16b - %T1.16b}, %S2.<Vbtype>"
  [(set_attr "type" "neon_tbl3")]
)

(define_insn "aarch64_tbx4<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "0")
		      (match_operand:OI 2 "register_operand" "w")
		      (match_operand:VB 3 "register_operand" "w")]
		      UNSPEC_TBX))]
  "TARGET_SIMD"
  "tbx\\t%S0.<Vbtype>, {%S2.16b - %T2.16b}, %S3.<Vbtype>"
  [(set_attr "type" "neon_tbl4")]
)

;; Three source registers.

(define_insn "aarch64_qtbl3<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:CI 1 "register_operand" "w")
		      (match_operand:VB 2 "register_operand" "w")]
		      UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%S0.<Vbtype>, {%S1.16b - %U1.16b}, %S2.<Vbtype>"
  [(set_attr "type" "neon_tbl3")]
)

(define_insn "aarch64_qtbx3<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "0")
		      (match_operand:CI 2 "register_operand" "w")
		      (match_operand:VB 3 "register_operand" "w")]
		      UNSPEC_TBX))]
  "TARGET_SIMD"
  "tbx\\t%S0.<Vbtype>, {%S2.16b - %U2.16b}, %S3.<Vbtype>"
  [(set_attr "type" "neon_tbl3")]
)

;; Four source registers.

(define_insn "aarch64_qtbl4<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:XI 1 "register_operand" "w")
		      (match_operand:VB 2 "register_operand" "w")]
		      UNSPEC_TBL))]
  "TARGET_SIMD"
  "tbl\\t%S0.<Vbtype>, {%S1.16b - %V1.16b}, %S2.<Vbtype>"
  [(set_attr "type" "neon_tbl4")]
)

(define_insn "aarch64_qtbx4<mode>"
  [(set (match_operand:VB 0 "register_operand" "=w")
	(unspec:VB [(match_operand:VB 1 "register_operand" "0")
		      (match_operand:XI 2 "register_operand" "w")
		      (match_operand:VB 3 "register_operand" "w")]
		      UNSPEC_TBX))]
  "TARGET_SIMD"
  "tbx\\t%S0.<Vbtype>, {%S2.16b - %V2.16b}, %S3.<Vbtype>"
  [(set_attr "type" "neon_tbl4")]
)

(define_insn_and_split "aarch64_combinev16qi"
  [(set (match_operand:OI 0 "register_operand" "=w")
	(unspec:OI [(match_operand:V16QI 1 "register_operand" "w")
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

(define_insn "aarch64_<PERMUTE:perm_insn><PERMUTE:perm_hilo><mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(unspec:VALL_F16 [(match_operand:VALL_F16 1 "register_operand" "w")
			  (match_operand:VALL_F16 2 "register_operand" "w")]
	 PERMUTE))]
  "TARGET_SIMD"
  "<PERMUTE:perm_insn><PERMUTE:perm_hilo>\\t%0.<Vtype>, %1.<Vtype>, %2.<Vtype>"
  [(set_attr "type" "neon_permute<q>")]
)

;; Note immediate (third) operand is lane index not byte index.
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

(define_insn "aarch64_rev<REVERSE:rev_op><mode>"
  [(set (match_operand:VALL_F16 0 "register_operand" "=w")
	(unspec:VALL_F16 [(match_operand:VALL_F16 1 "register_operand" "w")]
                    REVERSE))]
  "TARGET_SIMD"
  "rev<REVERSE:rev_op>\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_rev<q>")]
)

(define_insn "aarch64_st2<mode>_dreg"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:OI 1 "register_operand" "w")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST2))]
  "TARGET_SIMD"
  "st2\\t{%S1.<Vtype> - %T1.<Vtype>}, %0"
  [(set_attr "type" "neon_store2_2reg")]
)

(define_insn "aarch64_st2<mode>_dreg"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:OI 1 "register_operand" "w")
                    (unspec:DX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST2))]
  "TARGET_SIMD"
  "st1\\t{%S1.1d - %T1.1d}, %0"
  [(set_attr "type" "neon_store1_2reg")]
)

(define_insn "aarch64_st3<mode>_dreg"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:CI 1 "register_operand" "w")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST3))]
  "TARGET_SIMD"
  "st3\\t{%S1.<Vtype> - %U1.<Vtype>}, %0"
  [(set_attr "type" "neon_store3_3reg")]
)

(define_insn "aarch64_st3<mode>_dreg"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:CI 1 "register_operand" "w")
                    (unspec:DX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST3))]
  "TARGET_SIMD"
  "st1\\t{%S1.1d - %U1.1d}, %0"
  [(set_attr "type" "neon_store1_3reg")]
)

(define_insn "aarch64_st4<mode>_dreg"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:XI 1 "register_operand" "w")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST4))]
  "TARGET_SIMD"
  "st4\\t{%S1.<Vtype> - %V1.<Vtype>}, %0"
  [(set_attr "type" "neon_store4_4reg")]
)

(define_insn "aarch64_st4<mode>_dreg"
  [(set (match_operand:BLK 0 "aarch64_simd_struct_operand" "=Utv")
	(unspec:BLK [(match_operand:XI 1 "register_operand" "w")
                    (unspec:DX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_ST4))]
  "TARGET_SIMD"
  "st1\\t{%S1.1d - %V1.1d}, %0"
  [(set_attr "type" "neon_store1_4reg")]
)

(define_expand "aarch64_st<VSTRUCT:nregs><VDC:mode>"
 [(match_operand:DI 0 "register_operand" "r")
  (match_operand:VSTRUCT 1 "register_operand" "w")
  (unspec:VDC [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[0]);
  set_mem_size (mem, <VSTRUCT:nregs> * 8);

  emit_insn (gen_aarch64_st<VSTRUCT:nregs><VDC:mode>_dreg (mem, operands[1]));
  DONE;
})

(define_expand "aarch64_st<VSTRUCT:nregs><VQ:mode>"
 [(match_operand:DI 0 "register_operand" "r")
  (match_operand:VSTRUCT 1 "register_operand" "w")
  (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_SIMD"
{
  machine_mode mode = <VSTRUCT:MODE>mode;
  rtx mem = gen_rtx_MEM (mode, operands[0]);

  emit_insn (gen_aarch64_simd_st<VSTRUCT:nregs><VQ:mode> (mem, operands[1]));
  DONE;
})

(define_expand "aarch64_st<VSTRUCT:nregs>_lane<VALLDIF:mode>"
 [(match_operand:DI 0 "register_operand" "r")
  (match_operand:VSTRUCT 1 "register_operand" "w")
  (unspec:VALLDIF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)
  (match_operand:SI 2 "immediate_operand")]
  "TARGET_SIMD"
{
  rtx mem = gen_rtx_MEM (BLKmode, operands[0]);
  set_mem_size (mem, GET_MODE_SIZE (GET_MODE_INNER (<VALLDIF:MODE>mode))
		     * <VSTRUCT:nregs>);

  emit_insn (gen_aarch64_vec_store_lanes<VSTRUCT:mode>_lane<VALLDIF:mode> (
		mem, operands[1], operands[2]));
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

;; Expander for builtins to insert vector registers into large
;; opaque integer modes.

;; Q-register list.  We don't need a D-reg inserter as we zero
;; extend them in arm_neon.h and insert the resulting Q-regs.

(define_expand "aarch64_set_qreg<VSTRUCT:mode><VQ:mode>"
 [(match_operand:VSTRUCT 0 "register_operand" "+w")
  (match_operand:VSTRUCT 1 "register_operand" "0")
  (match_operand:VQ 2 "register_operand" "w")
  (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_SIMD"
{
  int part = INTVAL (operands[3]);
  int offset = part * 16;

  emit_move_insn (operands[0], operands[1]);
  emit_move_insn (gen_rtx_SUBREG (<VQ:MODE>mode, operands[0], offset),
		  operands[2]);
  DONE;
})

;; Standard pattern name vec_init<mode>.

(define_expand "vec_init<mode>"
  [(match_operand:VALL_F16 0 "register_operand" "")
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

(define_insn "aarch64_frecpe<mode>"
  [(set (match_operand:VHSDF 0 "register_operand" "=w")
	(unspec:VHSDF [(match_operand:VHSDF 1 "register_operand" "w")]
	 UNSPEC_FRECPE))]
  "TARGET_SIMD"
  "frecpe\\t%0.<Vtype>, %1.<Vtype>"
  [(set_attr "type" "neon_fp_recpe_<stype><q>")]
)

(define_insn "aarch64_frecp<FRECP:frecp_suffix><mode>"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(unspec:GPF [(match_operand:GPF 1 "register_operand" "w")]
		    FRECP))]
  "TARGET_SIMD"
  "frecp<FRECP:frecp_suffix>\\t%<s>0, %<s>1"
  [(set_attr "type" "neon_fp_recp<FRECP:frecp_suffix>_<GPF:Vetype><GPF:q>")]
)

(define_insn "aarch64_frecps<mode>"
  [(set (match_operand:VHSDF_SDF 0 "register_operand" "=w")
	(unspec:VHSDF_SDF
	  [(match_operand:VHSDF_SDF 1 "register_operand" "w")
	  (match_operand:VHSDF_SDF 2 "register_operand" "w")]
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

;; Standard pattern name vec_extract<mode>.

(define_expand "vec_extract<mode>"
  [(match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "")
   (match_operand:VALL_F16 1 "register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_SIMD"
{
    emit_insn
      (gen_aarch64_get_lane<mode> (operands[0], operands[1], operands[2]));
    DONE;
})

;; aes

(define_insn "aarch64_crypto_aes<aes_op>v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=w")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "0")
		       (match_operand:V16QI 2 "register_operand" "w")]
         CRYPTO_AES))]
  "TARGET_SIMD && TARGET_CRYPTO"
  "aes<aes_op>\\t%0.16b, %2.16b"
  [(set_attr "type" "crypto_aese")]
)

;; When AES/AESMC fusion is enabled we want the register allocation to
;; look like:
;;    AESE Vn, _
;;    AESMC Vn, Vn
;; So prefer to tie operand 1 to operand 0 when fusing.

(define_insn "aarch64_crypto_aes<aesmc_op>v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=w,w")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "0,w")]
	 CRYPTO_AESMC))]
  "TARGET_SIMD && TARGET_CRYPTO"
  "aes<aesmc_op>\\t%0.16b, %1.16b"
  [(set_attr "type" "crypto_aesmc")
   (set_attr_alternative "enabled"
     [(if_then_else (match_test
		       "aarch64_fusion_enabled_p (AARCH64_FUSE_AES_AESMC)")
		     (const_string "yes" )
		     (const_string "no"))
      (const_string "yes")])]
)

;; sha1

(define_insn "aarch64_crypto_sha1hsi"
  [(set (match_operand:SI 0 "register_operand" "=w")
        (unspec:SI [(match_operand:SI 1
                       "register_operand" "w")]
         UNSPEC_SHA1H))]
  "TARGET_SIMD && TARGET_CRYPTO"
  "sha1h\\t%s0, %s1"
  [(set_attr "type" "crypto_sha1_fast")]
)

(define_insn "aarch64_crypto_sha1su1v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")]
         UNSPEC_SHA1SU1))]
  "TARGET_SIMD && TARGET_CRYPTO"
  "sha1su1\\t%0.4s, %2.4s"
  [(set_attr "type" "crypto_sha1_fast")]
)

(define_insn "aarch64_crypto_sha1<sha1_op>v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:SI 2 "register_operand" "w")
                      (match_operand:V4SI 3 "register_operand" "w")]
         CRYPTO_SHA1))]
  "TARGET_SIMD && TARGET_CRYPTO"
  "sha1<sha1_op>\\t%q0, %s2, %3.4s"
  [(set_attr "type" "crypto_sha1_slow")]
)

(define_insn "aarch64_crypto_sha1su0v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")
                      (match_operand:V4SI 3 "register_operand" "w")]
         UNSPEC_SHA1SU0))]
  "TARGET_SIMD && TARGET_CRYPTO"
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
  "TARGET_SIMD && TARGET_CRYPTO"
  "sha256h<sha256_op>\\t%q0, %q2, %3.4s"
  [(set_attr "type" "crypto_sha256_slow")]
)

(define_insn "aarch64_crypto_sha256su0v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")]
         UNSPEC_SHA256SU0))]
  "TARGET_SIMD &&TARGET_CRYPTO"
  "sha256su0\\t%0.4s, %2.4s"
  [(set_attr "type" "crypto_sha256_fast")]
)

(define_insn "aarch64_crypto_sha256su1v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
                      (match_operand:V4SI 2 "register_operand" "w")
                      (match_operand:V4SI 3 "register_operand" "w")]
         UNSPEC_SHA256SU1))]
  "TARGET_SIMD &&TARGET_CRYPTO"
  "sha256su1\\t%0.4s, %2.4s, %3.4s"
  [(set_attr "type" "crypto_sha256_slow")]
)

;; pmull

(define_insn "aarch64_crypto_pmulldi"
  [(set (match_operand:TI 0 "register_operand" "=w")
        (unspec:TI  [(match_operand:DI 1 "register_operand" "w")
		     (match_operand:DI 2 "register_operand" "w")]
		    UNSPEC_PMULL))]
 "TARGET_SIMD && TARGET_CRYPTO"
 "pmull\\t%0.1q, %1.1d, %2.1d"
  [(set_attr "type" "neon_mul_d_long")]
)

(define_insn "aarch64_crypto_pmullv2di"
 [(set (match_operand:TI 0 "register_operand" "=w")
       (unspec:TI [(match_operand:V2DI 1 "register_operand" "w")
		   (match_operand:V2DI 2 "register_operand" "w")]
		  UNSPEC_PMULL2))]
  "TARGET_SIMD && TARGET_CRYPTO"
  "pmull2\\t%0.1q, %1.2d, %2.2d"
  [(set_attr "type" "neon_mul_d_long")]
)
