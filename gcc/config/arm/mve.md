;; Arm M-profile Vector Extension Machine Description
;; Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

(define_mode_attr V_sz_elem2 [(V16QI "s8") (V8HI "u16") (V4SI "u32")
			      (V2DI "u64")])
(define_mode_iterator MVE_types [V16QI V8HI V4SI V2DI TI V8HF V4SF V2DF])
(define_mode_iterator MVE_VLD_ST [V16QI V8HI V4SI V8HF V4SF])
(define_mode_iterator MVE_0 [V8HF V4SF])
(define_mode_iterator MVE_1 [V16QI V8HI V4SI V2DI])
(define_mode_iterator MVE_3 [V16QI V8HI])
(define_mode_iterator MVE_2 [V16QI V8HI V4SI])
(define_mode_iterator MVE_5 [V8HI V4SI])

(define_c_enum "unspec" [VST4Q VRNDXQ_F VRNDQ_F VRNDPQ_F VRNDNQ_F VRNDMQ_F
			 VRNDAQ_F VREV64Q_F VNEGQ_F VDUPQ_N_F VABSQ_F VREV32Q_F
			 VCVTTQ_F32_F16 VCVTBQ_F32_F16 VCVTQ_TO_F_S VQNEGQ_S
			 VCVTQ_TO_F_U VREV16Q_S VREV16Q_U VADDLVQ_S VMVNQ_N_S
			 VMVNQ_N_U VCVTAQ_S VCVTAQ_U VREV64Q_S VREV64Q_U
			 VQABSQ_S VNEGQ_S VMVNQ_S VMVNQ_U VDUPQ_N_U VDUPQ_N_S
			 VCLZQ_U VCLZQ_S VCLSQ_S VADDVQ_S VADDVQ_U VABSQ_S
			 VREV32Q_U VREV32Q_S VMOVLTQ_U VMOVLTQ_S VMOVLBQ_S
			 VMOVLBQ_U VCVTQ_FROM_F_S VCVTQ_FROM_F_U VCVTPQ_S
			 VCVTPQ_U VCVTNQ_S VCVTNQ_U VCVTMQ_S VCVTMQ_U
			 VADDLVQ_U VCTP8Q VCTP16Q VCTP32Q VCTP64Q VPNOT
			 VCREATEQ_F VCVTQ_N_TO_F_S VCVTQ_N_TO_F_U VBRSRQ_N_F
			 VSUBQ_N_F VCREATEQ_U VCREATEQ_S VSHRQ_N_S VSHRQ_N_U
			 VCVTQ_N_FROM_F_S VCVTQ_N_FROM_F_U])

(define_mode_attr MVE_CNVT [(V8HI "V8HF") (V4SI "V4SF")
			    (V8HF "V8HI") (V4SF "V4SI")])

(define_int_attr supf [(VCVTQ_TO_F_S "s") (VCVTQ_TO_F_U "u") (VREV16Q_S "s")
		       (VREV16Q_U "u") (VMVNQ_N_S "s") (VMVNQ_N_U "u")
		       (VCVTAQ_U "u") (VCVTAQ_S "s") (VREV64Q_S "s")
		       (VREV64Q_U "u") (VMVNQ_S "s") (VMVNQ_U "u")
		       (VDUPQ_N_U "u") (VDUPQ_N_S"s") (VADDVQ_S "s")
		       (VADDVQ_U "u") (VADDVQ_S "s") (VADDVQ_U "u")
		       (VMOVLTQ_U "u") (VMOVLTQ_S "s") (VMOVLBQ_S "s")
		       (VMOVLBQ_U "u") (VCVTQ_FROM_F_S "s") (VCVTQ_FROM_F_U "u")
		       (VCVTPQ_S "s") (VCVTPQ_U "u") (VCVTNQ_S "s")
		       (VCVTNQ_U "u") (VCVTMQ_S "s") (VCVTMQ_U "u")
		       (VCLZQ_U "u") (VCLZQ_S "s") (VREV32Q_U "u")
		       (VREV32Q_S "s") (VADDLVQ_U "u") (VADDLVQ_S "s")
		       (VCVTQ_N_TO_F_S "s") (VCVTQ_N_TO_F_U "u")
		       (VCREATEQ_U "u") (VCREATEQ_S "s") (VSHRQ_N_S "s")
		       (VSHRQ_N_U "u") (VCVTQ_N_FROM_F_S "s")
		       (VCVTQ_N_FROM_F_U "u")])

(define_int_attr mode1 [(VCTP8Q "8") (VCTP16Q "16") (VCTP32Q "32")
			(VCTP64Q "64")])
(define_mode_attr MVE_pred2 [(V16QI "mve_imm_8") (V8HI "mve_imm_16")
			     (V4SI "mve_imm_32")])
(define_mode_attr MVE_constraint2 [(V16QI "Rb") (V8HI "Rd") (V4SI "Rf")])

(define_int_iterator VCVTQ_TO_F [VCVTQ_TO_F_S VCVTQ_TO_F_U])
(define_int_iterator VMVNQ_N [VMVNQ_N_U VMVNQ_N_S])
(define_int_iterator VREV64Q [VREV64Q_S VREV64Q_U])
(define_int_iterator VCVTQ_FROM_F [VCVTQ_FROM_F_S VCVTQ_FROM_F_U])
(define_int_iterator VREV16Q [VREV16Q_U VREV16Q_S])
(define_int_iterator VCVTAQ [VCVTAQ_U VCVTAQ_S])
(define_int_iterator VMVNQ [VMVNQ_U VMVNQ_S])
(define_int_iterator VDUPQ_N [VDUPQ_N_U VDUPQ_N_S])
(define_int_iterator VCLZQ [VCLZQ_U VCLZQ_S])
(define_int_iterator VADDVQ [VADDVQ_U VADDVQ_S])
(define_int_iterator VREV32Q [VREV32Q_U VREV32Q_S])
(define_int_iterator VMOVLBQ [VMOVLBQ_S VMOVLBQ_U])
(define_int_iterator VMOVLTQ [VMOVLTQ_U VMOVLTQ_S])
(define_int_iterator VCVTPQ [VCVTPQ_S VCVTPQ_U])
(define_int_iterator VCVTNQ [VCVTNQ_S VCVTNQ_U])
(define_int_iterator VCVTMQ [VCVTMQ_S VCVTMQ_U])
(define_int_iterator VADDLVQ [VADDLVQ_U VADDLVQ_S])
(define_int_iterator VCTPQ [VCTP8Q VCTP16Q VCTP32Q VCTP64Q])
(define_int_iterator VCVTQ_N_TO_F [VCVTQ_N_TO_F_S VCVTQ_N_TO_F_U])
(define_int_iterator VCREATEQ [VCREATEQ_U VCREATEQ_S])
(define_int_iterator VSHRQ_N [VSHRQ_N_S VSHRQ_N_U])
(define_int_iterator VCVTQ_N_FROM_F [VCVTQ_N_FROM_F_S VCVTQ_N_FROM_F_U])

(define_insn "*mve_mov<mode>"
  [(set (match_operand:MVE_types 0 "nonimmediate_operand" "=w,w,r,w,w,r,w,Us")
	(match_operand:MVE_types 1 "general_operand" "w,r,w,Dn,Usi,r,Dm,w"))]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
{
  if (which_alternative == 3 || which_alternative == 6)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = simd_immediate_valid_for_move (operands[1], <MODE>mode,
	&operands[1], &width);

      gcc_assert (is_valid != 0);

      if (width == 0)
	return "vmov.f32\t%q0, %1  @ <mode>";
      else
	sprintf (templ, "vmov.i%d\t%%q0, %%x1  @ <mode>", width);
      return templ;
    }
  switch (which_alternative)
    {
    case 0:
      return "vmov\t%q0, %q1";
    case 1:
      return "vmov\t%e0, %Q1, %R1  @ <mode>\;vmov\t%f0, %J1, %K1";
    case 2:
      return "vmov\t%Q0, %R0, %e1  @ <mode>\;vmov\t%J0, %K0, %f1";
    case 4:
      if ((TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))
	  || (MEM_P (operands[1])
	      && GET_CODE (XEXP (operands[1], 0)) == LABEL_REF))
	return output_move_neon (operands);
      else
	return "vldrb.8 %q0, %E1";
    case 5:
      return output_move_neon (operands);
    case 7:
      return "vstrb.8 %q1, %E0";
    default:
      gcc_unreachable ();
      return "";
    }
}
  [(set_attr "type" "mve_move,mve_move,mve_move,mve_move,mve_load,mve_move,mve_move,mve_store")
   (set_attr "length" "4,8,8,4,8,8,4,4")
   (set_attr "thumb2_pool_range" "*,*,*,*,1018,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,*,996,*,*,*")])

(define_insn "*mve_mov<mode>"
  [(set (match_operand:MVE_types 0 "s_register_operand" "=w,w")
	(vec_duplicate:MVE_types
	  (match_operand:SI 1 "nonmemory_operand" "r,i")))]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
{
  if (which_alternative == 0)
    return "vdup.<V_sz_elem>\t%q0, %1";
  return "vmov.<V_sz_elem>\t%q0, %1";
}
  [(set_attr "length" "4,4")
   (set_attr "type" "mve_move,mve_move")])

;;
;; [vst4q])
;;
(define_insn "mve_vst4q<mode>"
  [(set (match_operand:XI 0 "neon_struct_operand" "=Um")
	(unspec:XI [(match_operand:XI 1 "s_register_operand" "w")
		    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VST4Q))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[6];
   int regno = REGNO (operands[1]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1] = gen_rtx_REG (TImode, regno+4);
   ops[2] = gen_rtx_REG (TImode, regno+8);
   ops[3] = gen_rtx_REG (TImode, regno+12);
   rtx reg  = operands[0];
   while (reg && !REG_P (reg))
    reg = XEXP (reg, 0);
   gcc_assert (REG_P (reg));
   ops[4] = reg;
   ops[5] = operands[0];
   /* Here in first three instructions data is stored to ops[4]'s location but
      in the fourth instruction data is stored to operands[0], this is to
      support the writeback.  */
   output_asm_insn ("vst40.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vst41.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vst42.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vst43.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, %5", ops);
   return "";
}
  [(set_attr "length" "16")])

;;
;; [vrndxq_f])
;;
(define_insn "mve_vrndxq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDXQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintx.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndq_f])
;;
(define_insn "mve_vrndq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintz.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndpq_f])
;;
(define_insn "mve_vrndpq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDPQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintp.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndnq_f])
;;
(define_insn "mve_vrndnq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDNQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintn.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndmq_f])
;;
(define_insn "mve_vrndmq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDMQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintm.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndaq_f])
;;
(define_insn "mve_vrndaq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDAQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrinta.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev64q_f])
;;
(define_insn "mve_vrev64q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VREV64Q_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrev64.%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vnegq_f])
;;
(define_insn "mve_vnegq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VNEGQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vneg.f%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vdupq_n_f])
;;
(define_insn "mve_vdupq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<V_elem> 1 "s_register_operand" "r")]
	 VDUPQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vdup.%#<V_sz_elem>   %q0, %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vabsq_f])
;;
(define_insn "mve_vabsq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VABSQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vabs.f%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev32q_f])
;;
(define_insn "mve_vrev32q_fv8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VREV32Q_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrev32.16 %q0, %q1"
  [(set_attr "type" "mve_move")
])
;;
;; [vcvttq_f32_f16])
;;
(define_insn "mve_vcvttq_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VCVTTQ_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtt.f32.f16 %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtbq_f32_f16])
;;
(define_insn "mve_vcvtbq_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VCVTBQ_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtb.f32.f16 %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_to_f_s, vcvtq_to_f_u])
;;
(define_insn "mve_vcvtq_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTQ_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.f%#<V_sz_elem>.<supf>%#<V_sz_elem>       %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev64q_u, vrev64q_s])
;;
(define_insn "mve_vrev64q_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VREV64Q))
  ]
  "TARGET_HAVE_MVE"
  "vrev64.%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_from_f_s, vcvtq_from_f_u])
;;
(define_insn "mve_vcvtq_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTQ_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.<supf>%#<V_sz_elem>.f%#<V_sz_elem>       %q0, %q1"
  [(set_attr "type" "mve_move")
])
;; [vqnegq_s])
;;
(define_insn "mve_vqnegq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VQNEGQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqneg.s%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vqabsq_s])
;;
(define_insn "mve_vqabsq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VQABSQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqabs.s%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vnegq_s])
;;
(define_insn "mve_vnegq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VNEGQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vneg.s%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmvnq_u, vmvnq_s])
;;
(define_insn "mve_vmvnq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VMVNQ))
  ]
  "TARGET_HAVE_MVE"
  "vmvn %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vdupq_n_u, vdupq_n_s])
;;
(define_insn "mve_vdupq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:<V_elem> 1 "s_register_operand" "r")]
	 VDUPQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vdup.%#<V_sz_elem>   %q0, %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vclzq_u, vclzq_s])
;;
(define_insn "mve_vclzq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VCLZQ))
  ]
  "TARGET_HAVE_MVE"
  "vclz.i%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vclsq_s])
;;
(define_insn "mve_vclsq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VCLSQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vcls.s%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddvq_s, vaddvq_u])
;;
(define_insn "mve_vaddvq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VADDVQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddv.<supf>%#<V_sz_elem>\t%0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vabsq_s])
;;
(define_insn "mve_vabsq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VABSQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vabs.s%#<V_sz_elem>\t%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev32q_u, vrev32q_s])
;;
(define_insn "mve_vrev32q_<supf><mode>"
  [
   (set (match_operand:MVE_3 0 "s_register_operand" "=w")
	(unspec:MVE_3 [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VREV32Q))
  ]
  "TARGET_HAVE_MVE"
  "vrev32.%#<V_sz_elem>\t%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovltq_u, vmovltq_s])
;;
(define_insn "mve_vmovltq_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VMOVLTQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovlt.<supf>%#<V_sz_elem>   %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovlbq_s, vmovlbq_u])
;;
(define_insn "mve_vmovlbq_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VMOVLBQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovlb.<supf>%#<V_sz_elem>   %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtpq_s, vcvtpq_u])
;;
(define_insn "mve_vcvtpq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTPQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtp.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtnq_s, vcvtnq_u])
;;
(define_insn "mve_vcvtnq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTNQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtn.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtmq_s, vcvtmq_u])
;;
(define_insn "mve_vcvtmq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTMQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtm.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtaq_u, vcvtaq_s])
;;
(define_insn "mve_vcvtaq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTAQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvta.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmvnq_n_u, vmvnq_n_s])
;;
(define_insn "mve_vmvnq_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:HI 1 "immediate_operand" "i")]
	 VMVNQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vmvn.i%#<V_sz_elem>  %q0, %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev16q_u, vrev16q_s])
;;
(define_insn "mve_vrev16q_<supf>v16qi"
  [
   (set (match_operand:V16QI 0 "s_register_operand" "=w")
	(unspec:V16QI [(match_operand:V16QI 1 "s_register_operand" "w")]
	 VREV16Q))
  ]
  "TARGET_HAVE_MVE"
  "vrev16.8 %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddlvq_s vaddlvq_u])
;;
(define_insn "mve_vaddlvq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")]
	 VADDLVQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddlv.<supf>32 %Q0, %R0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vctp8q vctp16q vctp32q vctp64q])
;;
(define_insn "mve_vctp<mode1>qhi"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:SI 1 "s_register_operand" "r")]
	VCTPQ))
  ]
  "TARGET_HAVE_MVE"
  "vctp.<mode1> %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vpnot])
;;
(define_insn "mve_vpnothi"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:HI 1 "vpr_register_operand" "0")]
	 VPNOT))
  ]
  "TARGET_HAVE_MVE"
  "vpnot"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_n_f])
;;
(define_insn "mve_vsubq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VSUBQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vsub.f<V_sz_elem>  %q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbrsrq_n_f])
;;
(define_insn "mve_vbrsrq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VBRSRQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vbrsr.<V_sz_elem>  %q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_n_to_f_s, vcvtq_n_to_f_u])
;;
(define_insn "mve_vcvtq_n_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "mve_imm_16" "Rd")]
	 VCVTQ_N_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.f<V_sz_elem>.<supf><V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;; [vcreateq_f])
;;
(define_insn "mve_vcreateq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:DI 1 "s_register_operand" "r")
		       (match_operand:DI 2 "s_register_operand" "r")]
	 VCREATEQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmov %q0[2], %q0[0], %Q2, %Q1\;vmov %q0[3], %q0[1], %R2, %R1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcreateq_u, vcreateq_s])
;;
(define_insn "mve_vcreateq_<supf><mode>"
  [
   (set (match_operand:MVE_1 0 "s_register_operand" "=w")
	(unspec:MVE_1 [(match_operand:DI 1 "s_register_operand" "r")
		       (match_operand:DI 2 "s_register_operand" "r")]
	 VCREATEQ))
  ]
  "TARGET_HAVE_MVE"
  "vmov %q0[2], %q0[0], %Q2, %Q1\;vmov %q0[3], %q0[1], %R2, %R1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshrq_n_s, vshrq_n_u])
;;
(define_insn "mve_vshrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 VSHRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshr.<supf><V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_n_from_f_s, vcvtq_n_from_f_u])
;;
(define_insn "mve_vcvtq_n_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "mve_imm_16" "Rd")]
	 VCVTQ_N_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.<supf><V_sz_elem>.f<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])
