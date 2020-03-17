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

(define_c_enum "unspec" [VST4Q VRNDXQ_F VRNDQ_F VRNDPQ_F VRNDNQ_F VRNDMQ_F
			 VRNDAQ_F VREV64Q_F VNEGQ_F VDUPQ_N_F VABSQ_F VREV32Q_F
			 VCVTTQ_F32_F16 VCVTBQ_F32_F16 VCVTQ_TO_F_S
			 VCVTQ_TO_F_U])

(define_mode_attr MVE_CNVT [(V8HI "V8HF") (V4SI "V4SF")
			    (V8HF "V8HI") (V4SF "V4SI")])

(define_int_attr supf [(VCVTQ_TO_F_S "s") (VCVTQ_TO_F_U "u")])
(define_int_iterator VCVTQ_TO_F [VCVTQ_TO_F_S VCVTQ_TO_F_U])

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
