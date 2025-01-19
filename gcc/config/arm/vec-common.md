;; Machine Description for shared bits common to IWMMXT and Neon.
;; Copyright (C) 2006-2025 Free Software Foundation, Inc.
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

;; Vector Moves

(define_expand "mov<mode>"
  [(set (match_operand:VNIM1 0 "nonimmediate_operand")
	(match_operand:VNIM1 1 "general_operand"))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
   {
  gcc_checking_assert (aligned_operand (operands[0], <MODE>mode));
  gcc_checking_assert (aligned_operand (operands[1], <MODE>mode));
  if (can_create_pseudo_p ())
    {
      if (!REG_P (operands[0]))
	operands[1] = force_reg (<MODE>mode, operands[1]);
      else if ((TARGET_NEON || TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT)
	       && (CONSTANT_P (operands[1])))
	{
	  operands[1] = neon_make_constant (operands[1]);
	  gcc_assert (operands[1] != NULL_RTX);
	}
    }
})

(define_expand "mov<mode>"
  [(set (match_operand:VNINOTM1 0 "nonimmediate_operand")
	(match_operand:VNINOTM1 1 "general_operand"))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
  gcc_checking_assert (aligned_operand (operands[0], <MODE>mode));
  gcc_checking_assert (aligned_operand (operands[1], <MODE>mode));
  if (can_create_pseudo_p ())
    {
      if (!REG_P (operands[0]))
	operands[1] = force_reg (<MODE>mode, operands[1]);
      else if (TARGET_NEON && CONSTANT_P (operands[1]))
	{
	  operands[1] = neon_make_constant (operands[1]);
	  gcc_assert (operands[1] != NULL_RTX);
	}
    }
})

(define_expand "movv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand")
       (match_operand:V8HF 1 "s_register_operand"))]
   "TARGET_NEON || TARGET_HAVE_MVE_FLOAT"
{
  gcc_checking_assert (aligned_operand (operands[0], E_V8HFmode));
  gcc_checking_assert (aligned_operand (operands[1], E_V8HFmode));
   if (can_create_pseudo_p ())
     {
       if (!REG_P (operands[0]))
	 operands[1] = force_reg (E_V8HFmode, operands[1]);
	else if (TARGET_HAVE_MVE_FLOAT && CONSTANT_P (operands[1]))
	  {
	    operands[1] = neon_make_constant (operands[1]);
	    gcc_assert (operands[1] != NULL_RTX);
	  }
     }
})

;; Vector arithmetic.  Expanders are blank, then unnamed insns implement
;; patterns separately for Neon, IWMMXT and MVE.

(define_expand "add<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand")
	(plus:VDQ (match_operand:VDQ 1 "s_register_operand")
		  (match_operand:VDQ 2 "s_register_operand")))]
  "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "sub<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand")
	(minus:VDQ (match_operand:VDQ 1 "s_register_operand")
		   (match_operand:VDQ 2 "s_register_operand")))]
  "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "mul<mode>3"
  [(set (match_operand:VDQWH 0 "s_register_operand")
	(mult:VDQWH (match_operand:VDQWH 1 "s_register_operand")
		    (match_operand:VDQWH 2 "s_register_operand")))]
  "ARM_HAVE_<MODE>_ARITH
   && (!TARGET_REALLY_IWMMXT
       || <MODE>mode == V4HImode
       || <MODE>mode == V2SImode)"
)

(define_expand "smin<mode>3"
  [(set (match_operand:VDQWH 0 "s_register_operand")
	(smin:VDQWH (match_operand:VDQWH 1 "s_register_operand")
		    (match_operand:VDQWH 2 "s_register_operand")))]
   "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "umin<mode>3"
  [(set (match_operand:VINTW 0 "s_register_operand")
	(umin:VINTW (match_operand:VINTW 1 "s_register_operand")
		    (match_operand:VINTW 2 "s_register_operand")))]
   "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "smax<mode>3"
  [(set (match_operand:VDQWH 0 "s_register_operand")
	(smax:VDQWH (match_operand:VDQWH 1 "s_register_operand")
		    (match_operand:VDQWH 2 "s_register_operand")))]
   "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "umax<mode>3"
  [(set (match_operand:VINTW 0 "s_register_operand")
	(umax:VINTW (match_operand:VINTW 1 "s_register_operand")
		    (match_operand:VINTW 2 "s_register_operand")))]
   "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "vec_perm<mode>"
  [(match_operand:VE 0 "s_register_operand")
   (match_operand:VE 1 "s_register_operand")
   (match_operand:VE 2 "s_register_operand")
   (match_operand:VE 3 "s_register_operand")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  arm_expand_vec_perm (operands[0], operands[1], operands[2], operands[3]);
  DONE;
})

(define_expand "vec_extract<mode><V_elem_l>"
 [(match_operand:<V_elem> 0 "nonimmediate_operand")
  (match_operand:VQX_NOBF 1 "s_register_operand")
  (match_operand:SI 2 "immediate_operand")]
 "TARGET_NEON || TARGET_HAVE_MVE"
{
  if (TARGET_NEON)
    emit_insn (gen_neon_vec_extract<mode><V_elem_l> (operands[0], operands[1],
						     operands[2]));
  else if (TARGET_HAVE_MVE)
    emit_insn (gen_mve_vec_extract<mode><V_elem_l> (operands[0], operands[1],
						     operands[2]));
  else
    gcc_unreachable ();
  DONE;
})

(define_expand "vec_set<mode>"
  [(match_operand:VQX_NOBF 0 "s_register_operand" "")
   (match_operand:<V_elem> 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON || TARGET_HAVE_MVE"
{
  HOST_WIDE_INT elem = HOST_WIDE_INT_1 << INTVAL (operands[2]);
  if (TARGET_NEON)
    emit_insn (gen_vec_set<mode>_internal (operands[0], operands[1],
					   GEN_INT (elem), operands[0]));
  else
    emit_insn (gen_mve_vec_set<mode>_internal (operands[0], operands[1],
					       GEN_INT (elem), operands[0]));
  DONE;
})

(define_expand "and<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "")
	(and:VDQ (match_operand:VDQ 1 "s_register_operand" "")
		 (match_operand:VDQ 2 "neon_inv_logic_op2" "")))]
  "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "ior<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "")
	(ior:VDQ (match_operand:VDQ 1 "s_register_operand" "")
		 (match_operand:VDQ 2 "neon_logic_op2" "")))]
  "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "xor<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "")
	(xor:VDQ (match_operand:VDQ 1 "s_register_operand" "")
		 (match_operand:VDQ 2 "s_register_operand" "")))]
  "ARM_HAVE_<MODE>_ARITH"
)

(define_expand "one_cmpl<mode>2"
  [(set (match_operand:VDQ 0 "s_register_operand")
	(not:VDQ (match_operand:VDQ 1 "s_register_operand")))]
  "ARM_HAVE_<MODE>_ARITH && !TARGET_REALLY_IWMMXT"
)

(define_expand "<absneg_str><mode>2"
  [(set (match_operand:VDQWH 0 "s_register_operand" "")
	(ABSNEG:VDQWH (match_operand:VDQWH 1 "s_register_operand" "")))]
  "ARM_HAVE_<MODE>_ARITH && !TARGET_REALLY_IWMMXT"
)

(define_expand "cadd<rot><mode>3"
  [(set (match_operand:VF 0 "register_operand")
	(unspec:VF [(match_operand:VF 1 "register_operand")
		    (match_operand:VF 2 "register_operand")]
		   VCADD))]
  "(TARGET_COMPLEX || (TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT
		      && ARM_HAVE_<MODE>_ARITH)) && !BYTES_BIG_ENDIAN"
)

;; The complex mul operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cmul<conj_op><mode>3"
  [(set (match_operand:VQ_HSF 0 "register_operand")
        (unspec:VQ_HSF [(match_operand:VQ_HSF 1 "register_operand")
			(match_operand:VQ_HSF 2 "register_operand")]
		       VCMUL_OP))]
  "(TARGET_COMPLEX || (TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT))
   && !BYTES_BIG_ENDIAN"
{
  rtx res1 = gen_reg_rtx (<MODE>mode);
  if (TARGET_COMPLEX)
    {
      rtx tmp = force_reg (<MODE>mode, CONST0_RTX (<MODE>mode));
      emit_insn (gen_arm_vcmla<rotsplit1><mode> (res1, tmp,
						 operands[2], operands[1]));
    }
  else
    emit_insn (gen_arm_vcmla<rotsplit1><mode> (res1, CONST0_RTX (<MODE>mode),
					       operands[2], operands[1]));

  emit_insn (gen_arm_vcmla<rotsplit2><mode> (operands[0], res1,
					     operands[2], operands[1]));
  DONE;
})

(define_expand "arm_vcmla<rot><mode>"
  [(set (match_operand:VF 0 "register_operand")
	(plus:VF (match_operand:VF 1 "register_operand")
		 (unspec:VF [(match_operand:VF 2 "register_operand")
			     (match_operand:VF 3 "register_operand")]
			     VCMLA)))]
  "(TARGET_COMPLEX || (TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT
		      && ARM_HAVE_<MODE>_ARITH)) && !BYTES_BIG_ENDIAN"
)

;; The complex mla/mls operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cml<fcmac1><conj_op><mode>4"
  [(set (match_operand:VF 0 "register_operand")
	(plus:VF (unspec:VF [(match_operand:VF 1 "register_operand")
			     (match_operand:VF 2 "register_operand")]
			    VCMLA_OP)
		 (match_operand:VF 3 "register_operand")))]
  "(TARGET_COMPLEX || (TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT
		      && ARM_HAVE_<MODE>_ARITH)) && !BYTES_BIG_ENDIAN"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_arm_vcmla<rotsplit1><mode> (tmp, operands[3],
					     operands[2], operands[1]));
  emit_insn (gen_arm_vcmla<rotsplit2><mode> (operands[0], tmp,
					     operands[2], operands[1]));
  DONE;
})

(define_expand "@movmisalign<mode>"
 [(set (match_operand:VDQ 0 "nonimmediate_operand")
	(unspec:VDQ [(match_operand:VDQ 1 "general_operand")]
	 UNSPEC_MISALIGNED_ACCESS))]
 "ARM_HAVE_<MODE>_LDST && !BYTES_BIG_ENDIAN
  && unaligned_access && !TARGET_REALLY_IWMMXT"
{
  rtx *memloc;
  bool for_store = false;
  /* This pattern is not permitted to fail during expansion: if both arguments
     are non-registers (e.g. memory := constant, which can be created by the
     auto-vectorizer), force operand 1 into a register.  */
  if (!s_register_operand (operands[0], <MODE>mode)
      && !s_register_operand (operands[1], <MODE>mode))
    operands[1] = force_reg (<MODE>mode, operands[1]);

  if (s_register_operand (operands[0], <MODE>mode))
    memloc = &operands[1];
  else
    {
      memloc = &operands[0];
      for_store = true;
    }

  /* For MVE, vector loads/stores must be aligned to the element size.  If the
     alignment is less than that convert the load/store to a suitable mode.  */
  if (TARGET_HAVE_MVE
      && (MEM_ALIGN (*memloc)
	  < GET_MODE_ALIGNMENT (GET_MODE_INNER (<MODE>mode))))
    {
      scalar_mode new_smode;
      switch (MEM_ALIGN (*memloc))
	{
	case 64:
	case 32:
	  new_smode = SImode;
	  break;
	case 16:
	  new_smode = HImode;
	  break;
	default:
	  new_smode = QImode;
	  break;
	}
      machine_mode new_mode
	= mode_for_vector (new_smode,
			   GET_MODE_SIZE (<MODE>mode)
			   / GET_MODE_SIZE (new_smode)).require ();
      rtx new_mem = adjust_address (*memloc, new_mode, 0);

      if (!for_store)
	{
	  rtx reg = gen_reg_rtx (new_mode);
	  emit_insn (gen_movmisalign (new_mode, reg, new_mem));
	  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, reg));
	  DONE;
	}
      emit_insn (gen_movmisalign (new_mode, new_mem,
				  gen_lowpart (new_mode, operands[1])));
      DONE;
    }

  /* Legitimize address.  */
  if ((TARGET_HAVE_MVE
       && !mve_vector_mem_operand (<MODE>mode, XEXP (*memloc, 0), false))
      || (!TARGET_HAVE_MVE
	  && !neon_vector_mem_operand (*memloc, 2, false)))
    {
      rtx new_mem
	= replace_equiv_address (*memloc,
				 force_reg (Pmode, XEXP (*memloc, 0)),
				 false);
      gcc_assert (MEM_ALIGN (new_mem) == MEM_ALIGN (*memloc));
      *memloc = new_mem;
    }
})

(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w,w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w,w")
		       (match_operand:VDQIW 2 "imm_lshift_or_reg_neon" "w,Ds")]
	 VSHLQ))]
  "ARM_HAVE_<MODE>_ARITH && !TARGET_REALLY_IWMMXT"
  "@
   <mve_insn>.<supf>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2
   * return neon_output_shift_immediate (\"vshl\", 'i', &operands[2], <MODE>mode, VALID_NEON_QREG_MODE (<MODE>mode), true);"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "neon_shift_reg<q>, neon_shift_imm<q>")]
)

(define_expand "vashl<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand" "")
	(ashift:VDQIW (match_operand:VDQIW 1 "s_register_operand" "")
		      (match_operand:VDQIW 2 "imm_lshift_or_reg_neon" "")))]
  "ARM_HAVE_<MODE>_ARITH && !TARGET_REALLY_IWMMXT"
{
  emit_insn (gen_mve_vshlq_u<mode> (operands[0], operands[1], operands[2]));
  DONE;
})

;; When operand 2 is an immediate, use the normal expansion to match
;; gen_vashr<mode>3_imm for Neon and gen_mve_vshrq_n_s<mode>_imm for
;; MVE.
(define_expand "vashr<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand")
	(ashiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand")
			(match_operand:VDQIW 2 "imm_rshift_or_reg_neon")))]
  "ARM_HAVE_<MODE>_ARITH && !TARGET_REALLY_IWMMXT"
{
  if (s_register_operand (operands[2], <MODE>mode))
    {
      rtx neg = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_neg<mode>2 (neg, operands[2]));
      emit_insn (gen_mve_vshlq_s<mode> (operands[0], operands[1], neg));
      DONE;
    }
})

;; When operand 2 is an immediate, use the normal expansion to match
;; gen_vashr<mode>3_imm for Neon and gen_mve_vshrq_n_u<mode>_imm for
;; MVE.
(define_expand "vlshr<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand")
	(lshiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand")
			(match_operand:VDQIW 2 "imm_rshift_or_reg_neon")))]
  "ARM_HAVE_<MODE>_ARITH && !TARGET_REALLY_IWMMXT"
{
  if (s_register_operand (operands[2], <MODE>mode))
    {
      rtx neg = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_neg<mode>2 (neg, operands[2]));
      emit_insn (gen_mve_vshlq_u<mode> (operands[0], operands[1], neg));
      DONE;
    }
})

(define_expand "vec_load_lanesoi<mode>"
  [(set (match_operand:OI 0 "s_register_operand")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand")
                    (unspec:VQ2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD2))]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vld2<mode> (operands[0], operands[1]));
  DONE;
})

;;; On MVE we use V2xYYY modes instead of OI
(define_expand "vec_load_lanes<MVE_vld2_vst2><mode>"
  [(set (match_operand:<MVE_VLD2_VST2> 0 "s_register_operand")
        (unspec:<MVE_VLD2_VST2> [(match_operand:<MVE_VLD2_VST2> 1 "neon_struct_operand")
                    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD2))]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD2_VST2>mode))"
{
  emit_insn (gen_mve_vld2q<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "vec_store_lanesoi<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand")
	(unspec:OI [(match_operand:OI 1 "s_register_operand")
                    (unspec:VQ2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST2))]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vst2<mode> (operands[0], operands[1]));
  DONE;
})

;;; On MVE we use V2xYYY modes instead of OI
(define_expand "vec_store_lanes<MVE_vld2_vst2><mode>"
  [(set (match_operand:<MVE_VLD2_VST2> 0 "neon_struct_operand")
	(unspec:<MVE_VLD2_VST2> [(match_operand:<MVE_VLD2_VST2> 1 "s_register_operand")
                    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST2))]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD2_VST2>mode))"
{
  emit_insn (gen_mve_vst2q<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "vec_load_lanesxi<mode>"
  [(match_operand:XI 0 "s_register_operand")
   (match_operand:XI 1 "neon_struct_operand")
   (unspec:VQ2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vld4<mode> (operands[0], operands[1]));
  DONE;
})

;;; On MVE we use V4xYYY modes instead of XI
(define_expand "vec_load_lanes<MVE_vld4_vst4><mode>"
  [(set (match_operand:<MVE_VLD4_VST4> 0 "s_register_operand")
        (unspec:<MVE_VLD4_VST4> [(match_operand:<MVE_VLD4_VST4> 1 "neon_struct_operand")
                    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD4))]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD4_VST4>mode))"
{
  emit_insn (gen_mve_vld4q<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "vec_store_lanesxi<mode>"
  [(match_operand:XI 0 "neon_struct_operand")
   (match_operand:XI 1 "s_register_operand")
   (unspec:VQ2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vst4<mode> (operands[0], operands[1]));
  DONE;
})

;;; On MVE we use V4xYYY modes instead of XI
(define_expand "vec_store_lanes<MVE_vld4_vst4><mode>"
  [(set (match_operand:<MVE_VLD4_VST4> 0 "neon_struct_operand")
	(unspec:<MVE_VLD4_VST4> [(match_operand:<MVE_VLD4_VST4> 1 "s_register_operand")
                    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4))]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD4_VST4>mode))"
{
  emit_insn (gen_mve_vst4q<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VQ 1 "s_register_operand")]
  "ARM_HAVE_<MODE>_ARITH
   && !(TARGET_HAVE_MVE && FLOAT_MODE_P (<MODE>mode))
   && !BYTES_BIG_ENDIAN"
{
  if (TARGET_NEON)
    {
      rtx step1 = gen_reg_rtx (<V_HALF>mode);

      emit_insn (gen_quad_halves_plus<mode> (step1, operands[1]));
      emit_insn (gen_reduc_plus_scal_<V_half> (operands[0], step1));
    }
  else
    {
      /* vaddv generates a 32 bits accumulator.  */
      rtx op0 = gen_reg_rtx (SImode);

      emit_insn (gen_mve_q (VADDVQ_S, VADDVQ_S, <MODE>mode, op0, operands[1]));
      emit_move_insn (operands[0], gen_lowpart (<V_elem>mode, op0));
    }

  DONE;
})

(define_expand "avg<mode>3_floor"
  [(match_operand:MVE_2 0 "s_register_operand")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:MVE_2 2 "s_register_operand")]
  "ARM_HAVE_<MODE>_ARITH"
{
  if (TARGET_HAVE_MVE)
    emit_insn (gen_mve_q (VHADDQ_S, VHADDQ_S, <MODE>mode,
			       operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vhadd (UNSPEC_VHADD_S, UNSPEC_VHADD_S, <MODE>mode,
			       operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "uavg<mode>3_floor"
  [(match_operand:MVE_2 0 "s_register_operand")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:MVE_2 2 "s_register_operand")]
  "ARM_HAVE_<MODE>_ARITH"
{
  if (TARGET_HAVE_MVE)
    emit_insn (gen_mve_q (VHADDQ_U, VHADDQ_U, <MODE>mode,
			       operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vhadd (UNSPEC_VHADD_U, UNSPEC_VHADD_U, <MODE>mode,
			       operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "avg<mode>3_ceil"
  [(match_operand:MVE_2 0 "s_register_operand")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:MVE_2 2 "s_register_operand")]
  "ARM_HAVE_<MODE>_ARITH"
{
  if (TARGET_HAVE_MVE)
    emit_insn (gen_mve_q (VRHADDQ_S, VRHADDQ_S, <MODE>mode,
				operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vhadd (UNSPEC_VRHADD_S, UNSPEC_VRHADD_S, <MODE>mode,
			       operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "uavg<mode>3_ceil"
  [(match_operand:MVE_2 0 "s_register_operand")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:MVE_2 2 "s_register_operand")]
  "ARM_HAVE_<MODE>_ARITH"
{
  if (TARGET_HAVE_MVE)
    emit_insn (gen_mve_q (VRHADDQ_U, VRHADDQ_U, <MODE>mode,
				operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vhadd (UNSPEC_VRHADD_U, UNSPEC_VRHADD_U, <MODE>mode,
			       operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "clz<mode>2"
 [(set (match_operand:VDQIW 0 "s_register_operand")
       (clz:VDQIW (match_operand:VDQIW 1 "s_register_operand")))]
  "ARM_HAVE_<MODE>_ARITH
   && !TARGET_REALLY_IWMMXT"
)
(define_expand "vec_init<mode><V_elem_l>"
  [(match_operand:VDQX 0 "s_register_operand")
   (match_operand 1 "" "")]
  "TARGET_NEON || (TARGET_HAVE_MVE && VALID_MVE_MODE (<MODE>mode))"
{
  neon_expand_vector_init (operands[0], operands[1]);
  DONE;
})
