;; Arm M-profile Vector Extension Machine Description
;; Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

(define_insn "*mve_mov<mode>"
  [(set (match_operand:MVE_types 0 "nonimmediate_operand" "=w,w,r,w,w,r,w,Ux,w")
	(match_operand:MVE_types 1 "general_operand" "w,r,w,Dn,UxUi,r,Dm,w,Ul"))]
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

  if (which_alternative == 4 || which_alternative == 7)
    {
      if (<MODE>mode == V2DFmode || <MODE>mode == V2DImode || <MODE>mode == TImode)
	{
	  if (which_alternative == 7)
	    output_asm_insn ("vstrw.32\t%q1, %E0", operands);
	  else
	    output_asm_insn ("vldrw.u32\t%q0, %E1",operands);
	}
      else
	{
	  if (which_alternative == 7)
	    output_asm_insn ("vstr<V_sz_elem1>.<V_sz_elem>\t%q1, %E0", operands);
	  else
	    output_asm_insn ("vldr<V_sz_elem1>.<V_sz_elem>\t%q0, %E1", operands);
	}
      return "";
    }
  switch (which_alternative)
    {
    case 0:
      return "vmov\t%q0, %q1";
    case 1:
      return "vmov\t%e0, %Q1, %R1  @ <mode>\;vmov\t%f0, %J1, %K1";
    case 2:
      return "vmov\t%Q0, %R0, %e1  @ <mode>\;vmov\t%J0, %K0, %f1";
    case 5:
      return output_move_quad (operands);
    case 8:
	return output_move_neon (operands);
    default:
      gcc_unreachable ();
      return "";
    }
}
  [(set_attr "type" "mve_move,mve_move,mve_move,mve_move,mve_load,multiple,mve_move,mve_store,mve_load")
   (set_attr "length" "4,8,8,4,8,8,4,4,4")
   (set_attr "thumb2_pool_range" "*,*,*,*,1018,*,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,*,996,*,*,*,*")])

(define_insn "*mve_vdup<mode>"
  [(set (match_operand:MVE_vecs 0 "s_register_operand" "=w")
	(vec_duplicate:MVE_vecs
	  (match_operand:<V_elem> 1 "s_register_operand" "r")))]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
  "vdup.<V_sz_elem>\t%q0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "mve_move")])

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
;; [vrndq_m_f])
;;
(define_insn "mve_vrndq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRNDQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrintzt.f%#<V_sz_elem> %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

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
   (set (match_operand:MVE_0 0 "s_register_operand" "=&w")
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
	(neg:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")))
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
	(abs:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")))
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
   (set (match_operand:MVE_2 0 "s_register_operand" "=&w")
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
	(neg:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vneg.s%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmvnq_u, vmvnq_s])
;;
(define_insn "mve_vmvnq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(not:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vmvn\t%q0, %q1"
  [(set_attr "type" "mve_move")
])
(define_expand "mve_vmvnq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(not:MVE_2 (match_operand:MVE_2 1 "s_register_operand")))
  ]
  "TARGET_HAVE_MVE"
)

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
(define_insn "@mve_vclzq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(clz:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vclz.i%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])
(define_expand "mve_vclzq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(clz:MVE_2 (match_operand:MVE_2 1 "s_register_operand")))
  ]
  "TARGET_HAVE_MVE"
)

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
(define_insn "@mve_vaddvq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
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
	(abs:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")))
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

(define_insn "mve_vec_unpack<US>_lo_<mode>"
  [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
	(SE:<V_unpack> (vec_select:<V_HALF>
			  (match_operand:MVE_3 1 "register_operand" "w")
			  (match_operand:MVE_3 2 "vect_par_constant_low" ""))))]
  "TARGET_HAVE_MVE"
  "vmovlb.<US>%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")]
)

(define_insn "mve_vec_unpack<US>_hi_<mode>"
  [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
	(SE:<V_unpack> (vec_select:<V_HALF>
			  (match_operand:MVE_3 1 "register_operand" "w")
			  (match_operand:MVE_3 2 "vect_par_constant_high" ""))))]
  "TARGET_HAVE_MVE"
  "vmovlt.<US>%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")]
)

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
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
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
;; Version that takes an immediate as operand 2.
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

;; Versions that take constant vectors as operand 2 (with all elements
;; equal).
(define_insn "mve_vshrq_n_s<mode>_imm"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(ashiftrt:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
			(match_operand:MVE_2 2 "imm_for_neon_rshift_operand" "i")))
  ]
  "TARGET_HAVE_MVE"
  {
    return neon_output_shift_immediate ("vshr", 's', &operands[2],
					<MODE>mode,
					VALID_NEON_QREG_MODE (<MODE>mode),
					true);
  }
  [(set_attr "type" "mve_move")
])
(define_insn "mve_vshrq_n_u<mode>_imm"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(lshiftrt:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
			(match_operand:MVE_2 2 "imm_for_neon_rshift_operand" "i")))
  ]
  "TARGET_HAVE_MVE"
  {
    return neon_output_shift_immediate ("vshr", 'u', &operands[2],
					<MODE>mode,
					VALID_NEON_QREG_MODE (<MODE>mode),
					true);
  }
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_n_from_f_s, vcvtq_n_from_f_u])
;;
(define_insn "mve_vcvtq_n_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 VCVTQ_N_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.<supf><V_sz_elem>.f<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddlvq_p_s])
;;
(define_insn "mve_vaddlvq_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VADDLVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddlvt.<supf>32 %Q0, %R0, %q1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpneq_, vcmpcsq_, vcmpeqq_, vcmpgeq_, vcmpgtq_, vcmphiq_, vcmpleq_, vcmpltq_])
;;
(define_insn "@mve_vcmp<mve_cmp_op>q_<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(MVE_COMPARISONS:HI (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.<mve_cmp_type>%#<V_sz_elem>  <mve_cmp_op>, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpcsq_n_, vcmpeqq_n_, vcmpgeq_n_, vcmpgtq_n_, vcmphiq_n_, vcmpleq_n_, vcmpltq_n_, vcmpneq_n_])
;;
(define_insn "mve_vcmp<mve_cmp_op>q_n_<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(MVE_COMPARISONS:HI (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.<mve_cmp_type>%#<V_sz_elem>	<mve_cmp_op>, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlq_s, vshlq_u])
;; See vec-common.md

;;
;; [vabdq_s, vabdq_u])
;;
(define_insn "mve_vabdq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VABDQ))
  ]
  "TARGET_HAVE_MVE"
  "vabd.<supf>%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddq_n_s, vaddq_n_u])
;;
(define_insn "mve_vaddq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VADDQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vadd.i%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddvaq_s, vaddvaq_u])
;;
(define_insn "mve_vaddvaq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VADDVAQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddva.<supf>%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddvq_p_u, vaddvq_p_s])
;;
(define_insn "mve_vaddvq_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VADDVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddvt.<supf>%#<V_sz_elem>	%0, %q1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vandq_u, vandq_s])
;;
;; signed and unsigned versions are the same: define the unsigned
;; insn, and use an expander for the signed one as we still reference
;; both names from arm_mve.h.
;; We use the same code as in neon.md (TODO: avoid this duplication).
(define_insn "mve_vandq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w,w")
	(and:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w,0")
		   (match_operand:MVE_2 2 "neon_inv_logic_op2" "w,DL")))
  ]
  "TARGET_HAVE_MVE"
  "@
   vand\t%q0, %q1, %q2
   * return neon_output_logic_immediate (\"vand\", &operands[2], <MODE>mode, 1, VALID_NEON_QREG_MODE (<MODE>mode));"
  [(set_attr "type" "mve_move")
])
(define_expand "mve_vandq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(and:MVE_2 (match_operand:MVE_2 1 "s_register_operand")
		   (match_operand:MVE_2 2 "neon_inv_logic_op2")))
  ]
  "TARGET_HAVE_MVE"
)

;;
;; [vbicq_s, vbicq_u])
;;
(define_insn "mve_vbicq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(and:MVE_2 (not:MVE_2 (match_operand:MVE_2 2 "s_register_operand" "w"))
			      (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vbic\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

(define_expand "mve_vbicq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(and:MVE_2 (not:MVE_2 (match_operand:MVE_2 2 "s_register_operand"))
		   (match_operand:MVE_2 1 "s_register_operand")))
  ]
  "TARGET_HAVE_MVE"
)

;;
;; [vbrsrq_n_u, vbrsrq_n_s])
;;
(define_insn "mve_vbrsrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VBRSRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vbrsr.%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcaddq, vcaddq_rot90, vcadd_rot180, vcadd_rot270])
;;
(define_insn "mve_vcaddq<mve_rot><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCADD))
  ]
  "TARGET_HAVE_MVE"
  "vcadd.i%#<V_sz_elem>	%q0, %q1, %q2, #<rot>"
  [(set_attr "type" "mve_move")
])

;; Auto vectorizer pattern for int vcadd
(define_expand "cadd<rot><mode>3"
  [(set (match_operand:MVE_2 0 "register_operand")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "register_operand")
		       (match_operand:MVE_2 2 "register_operand")]
	  VCADD))]
  "TARGET_HAVE_MVE && !BYTES_BIG_ENDIAN"
)

;;
;; [veorq_u, veorq_s])
;;
(define_insn "mve_veorq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(xor:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		   (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "veor\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])
(define_expand "mve_veorq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(xor:MVE_2 (match_operand:MVE_2 1 "s_register_operand")
		   (match_operand:MVE_2 2 "s_register_operand")))
  ]
  "TARGET_HAVE_MVE"
)

;;
;; [vhaddq_n_u, vhaddq_n_s])
;;
(define_insn "mve_vhaddq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VHADDQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vhadd.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vhaddq_s, vhaddq_u])
;;
(define_insn "@mve_vhaddq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHADDQ))
  ]
  "TARGET_HAVE_MVE"
  "vhadd.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vhcaddq_rot270_s])
;;
(define_insn "mve_vhcaddq_rot270_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHCADDQ_ROT270_S))
  ]
  "TARGET_HAVE_MVE"
  "vhcadd.s%#<V_sz_elem>\t%q0, %q1, %q2, #270"
  [(set_attr "type" "mve_move")
])

;;
;; [vhcaddq_rot90_s])
;;
(define_insn "mve_vhcaddq_rot90_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHCADDQ_ROT90_S))
  ]
  "TARGET_HAVE_MVE"
  "vhcadd.s%#<V_sz_elem>\t%q0, %q1, %q2, #90"
  [(set_attr "type" "mve_move")
])

;;
;; [vhsubq_n_u, vhsubq_n_s])
;;
(define_insn "mve_vhsubq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VHSUBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vhsub.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vhsubq_s, vhsubq_u])
;;
(define_insn "mve_vhsubq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHSUBQ))
  ]
  "TARGET_HAVE_MVE"
  "vhsub.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxaq_s])
;;
(define_insn "mve_vmaxaq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMAXAQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmaxa.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxavq_s])
;;
(define_insn "mve_vmaxavq_s<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMAXAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmaxav.s%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxq_u, vmaxq_s])
;;
(define_insn "mve_vmaxq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(smax:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vmax.%#<V_s_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

(define_insn "mve_vmaxq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(umax:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vmax.%#<V_u_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxvq_u, vmaxvq_s])
;;
(define_insn "mve_vmaxvq_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMAXVQ))
  ]
  "TARGET_HAVE_MVE"
  "vmaxv.<supf>%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminaq_s])
;;
(define_insn "mve_vminaq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMINAQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmina.s%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminavq_s])
;;
(define_insn "mve_vminavq_s<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMINAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vminav.s%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminq_s, vminq_u])
;;
(define_insn "mve_vminq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(smin:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vmin.%#<V_s_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

(define_insn "mve_vminq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(umin:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vmin.%#<V_u_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminvq_u, vminvq_s])
;;
(define_insn "mve_vminvq_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMINVQ))
  ]
  "TARGET_HAVE_MVE"
  "vminv.<supf>%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmladavq_u, vmladavq_s])
;;
(define_insn "mve_vmladavq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLADAVQ))
  ]
  "TARGET_HAVE_MVE"
  "vmladav.<supf>%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmladavxq_s])
;;
(define_insn "mve_vmladavxq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLADAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmladavx.s%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsdavq_s])
;;
(define_insn "mve_vmlsdavq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLSDAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsdav.s%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsdavxq_s])
;;
(define_insn "mve_vmlsdavxq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLSDAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsdavx.s%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulhq_s, vmulhq_u])
;;
(define_insn "mve_vmulhq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULHQ))
  ]
  "TARGET_HAVE_MVE"
  "vmulh.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmullbq_int_u, vmullbq_int_s])
;;
(define_insn "mve_vmullbq_int_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_2 1 "s_register_operand" "w")
				  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULLBQ_INT))
  ]
  "TARGET_HAVE_MVE"
  "vmullb.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulltq_int_u, vmulltq_int_s])
;;
(define_insn "mve_vmulltq_int_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_2 1 "s_register_operand" "w")
				  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULLTQ_INT))
  ]
  "TARGET_HAVE_MVE"
  "vmullt.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_n_u, vmulq_n_s])
;;
(define_insn "mve_vmulq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VMULQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vmul.i%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_u, vmulq_s])
;;
(define_insn "mve_vmulq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULQ))
  ]
  "TARGET_HAVE_MVE"
  "vmul.i%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

(define_insn "mve_vmulq<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(mult:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vmul.i%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vornq_u, vornq_s])
;;
(define_insn "mve_vornq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(ior:MVE_2 (not:MVE_2 (match_operand:MVE_2 2 "s_register_operand" "w"))
		   (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
   "vorn\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

(define_expand "mve_vornq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(ior:MVE_2 (not:MVE_2 (match_operand:MVE_2 2 "s_register_operand"))
		   (match_operand:MVE_2 1 "s_register_operand")))
  ]
  "TARGET_HAVE_MVE"
)

;;
;; [vorrq_s, vorrq_u])
;;
;; signed and unsigned versions are the same: define the unsigned
;; insn, and use an expander for the signed one as we still reference
;; both names from arm_mve.h.
;; We use the same code as in neon.md (TODO: avoid this duplication).
(define_insn "mve_vorrq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w,w")
	(ior:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w,0")
		   (match_operand:MVE_2 2 "neon_logic_op2" "w,Dl")))
  ]
  "TARGET_HAVE_MVE"
  "@
   vorr\t%q0, %q1, %q2
   * return neon_output_logic_immediate (\"vorr\", &operands[2], <MODE>mode, 0, VALID_NEON_QREG_MODE (<MODE>mode));"
  [(set_attr "type" "mve_move")
])
(define_expand "mve_vorrq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(ior:MVE_2 (match_operand:MVE_2 1 "s_register_operand")
		   (match_operand:MVE_2 2 "neon_logic_op2")))
  ]
  "TARGET_HAVE_MVE"
)

;;
;; [vqaddq_n_s, vqaddq_n_u])
;;
(define_insn "mve_vqaddq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQADDQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqadd.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqaddq_u, vqaddq_s])
;;
(define_insn "mve_vqaddq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQADDQ))
  ]
  "TARGET_HAVE_MVE"
  "vqadd.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulhq_n_s])
;;
(define_insn "mve_vqdmulhq_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQDMULHQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmulh.s%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulhq_s])
;;
(define_insn "mve_vqdmulhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQDMULHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmulh.s%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmulhq_n_s])
;;
(define_insn "mve_vqrdmulhq_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQRDMULHQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmulh.s%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmulhq_s])
;;
(define_insn "mve_vqrdmulhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQRDMULHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmulh.s%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrshlq_n_s, vqrshlq_n_u])
;;
(define_insn "mve_vqrshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VQRSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqrshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrshlq_s, vqrshlq_u])
;;
(define_insn "mve_vqrshlq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQRSHLQ))
  ]
  "TARGET_HAVE_MVE"
  "vqrshl.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshlq_n_s, vqshlq_n_u])
;;
(define_insn "mve_vqshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VQSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqshl.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshlq_r_u, vqshlq_r_s])
;;
(define_insn "mve_vqshlq_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VQSHLQ_R))
  ]
  "TARGET_HAVE_MVE"
  "vqshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshlq_s, vqshlq_u])
;;
(define_insn "mve_vqshlq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQSHLQ))
  ]
  "TARGET_HAVE_MVE"
  "vqshl.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshluq_n_s])
;;
(define_insn "mve_vqshluq_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "mve_imm_7" "Ra")]
	 VQSHLUQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqshlu.s%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqsubq_n_s, vqsubq_n_u])
;;
(define_insn "mve_vqsubq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQSUBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqsub.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqsubq_u, vqsubq_s])
;;
(define_insn "mve_vqsubq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQSUBQ))
  ]
  "TARGET_HAVE_MVE"
  "vqsub.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrhaddq_s, vrhaddq_u])
;;
(define_insn "@mve_vrhaddq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VRHADDQ))
  ]
  "TARGET_HAVE_MVE"
  "vrhadd.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmulhq_s, vrmulhq_u])
;;
(define_insn "mve_vrmulhq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VRMULHQ))
  ]
  "TARGET_HAVE_MVE"
  "vrmulh.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrshlq_n_u, vrshlq_n_s])
;;
(define_insn "mve_vrshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VRSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vrshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrshlq_s, vrshlq_u])
;;
(define_insn "mve_vrshlq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VRSHLQ))
  ]
  "TARGET_HAVE_MVE"
  "vrshl.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrshrq_n_s, vrshrq_n_u])
;;
(define_insn "mve_vrshrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 VRSHRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vrshr.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlq_n_u, vshlq_n_s])
;;
(define_insn "mve_vshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshl.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlq_r_s, vshlq_r_u])
;;
(define_insn "mve_vshlq_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VSHLQ_R))
  ]
  "TARGET_HAVE_MVE"
  "vshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_n_s, vsubq_n_u])
;;
(define_insn "mve_vsubq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VSUBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vsub.i%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_s, vsubq_u])
;;
(define_insn "mve_vsubq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VSUBQ))
  ]
  "TARGET_HAVE_MVE"
  "vsub.i%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

(define_insn "mve_vsubq<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(minus:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		     (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vsub.i%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vabdq_f])
;;
(define_insn "mve_vabdq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VABDQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vabd.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddlvaq_s vaddlvaq_u])
;;
(define_insn "mve_vaddlvaq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VADDLVAQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddlva.<supf>32 %Q0, %R0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddq_n_f])
;;
(define_insn "mve_vaddq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VADDQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vadd.f%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vandq_f])
;;
(define_insn "mve_vandq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(and:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		   (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vand %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbicq_f])
;;
(define_insn "mve_vbicq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(and:MVE_0 (not:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w"))
			      (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vbic %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbicq_n_s, vbicq_n_u])
;;
(define_insn "mve_vbicq_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VBICQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vbic.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcaddq, vcaddq_rot90, vcadd_rot180, vcadd_rot270])
;;
(define_insn "mve_vcaddq<mve_rot><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCADD))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcadd.f%#<V_sz_elem>	%q0, %q1, %q2, #<rot>"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_f, vcmpgeq_f, vcmpgtq_f, vcmpleq_f, vcmpltq_f, vcmpneq_f])
;;
(define_insn "@mve_vcmp<mve_cmp_op>q_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(MVE_FP_COMPARISONS:HI (match_operand:MVE_0 1 "s_register_operand" "w")
			       (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	<mve_cmp_op>, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_n_f, vcmpgeq_n_f, vcmpgtq_n_f, vcmpleq_n_f, vcmpltq_n_f, vcmpneq_n_f])
;;
(define_insn "@mve_vcmp<mve_cmp_op>q_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(MVE_FP_COMPARISONS:HI (match_operand:MVE_0 1 "s_register_operand" "w")
			       (match_operand:<V_elem> 2 "s_register_operand" "r")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	<mve_cmp_op>, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmulq, vcmulq_rot90, vcmulq_rot180, vcmulq_rot270])
;;
(define_insn "mve_vcmulq<mve_rot><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMUL))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmul.f%#<V_sz_elem>	%q0, %q1, %q2, #<rot>"
  [(set_attr "type" "mve_move")
])

;;
;; [vctp8q_m vctp16q_m vctp32q_m vctp64q_m])
;;
(define_insn "mve_vctp<mode1>q_mhi"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:SI 1 "s_register_operand" "r")
		    (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VCTPQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vctpt.<mode1> %1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtbq_f16_f32])
;;
(define_insn "mve_vcvtbq_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		      (match_operand:V4SF 2 "s_register_operand" "w")]
	 VCVTBQ_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtb.f16.f32 %q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvttq_f16_f32])
;;
(define_insn "mve_vcvttq_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		      (match_operand:V4SF 2 "s_register_operand" "w")]
	 VCVTTQ_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtt.f16.f32 %q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [veorq_f])
;;
(define_insn "mve_veorq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(xor:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		   (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "veor %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmaq_f])
;;
(define_insn "mve_vmaxnmaq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMAXNMAQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnma.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmavq_f])
;;
(define_insn "mve_vmaxnmavq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMAXNMAVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnmav.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmq_f])
;;
(define_insn "mve_vmaxnmq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(smax:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnm.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmvq_f])
;;
(define_insn "mve_vmaxnmvq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMAXNMVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnmv.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmaq_f])
;;
(define_insn "mve_vminnmaq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMINNMAQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnma.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmavq_f])
;;
(define_insn "mve_vminnmavq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMINNMAVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnmav.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmq_f])
;;
(define_insn "mve_vminnmq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(smin:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnm.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmvq_f])
;;
(define_insn "mve_vminnmvq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMINNMVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnmv.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlaldavq_u, vmlaldavq_s])
;;
(define_insn "mve_vmlaldavq_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLALDAVQ))
  ]
  "TARGET_HAVE_MVE"
  "vmlaldav.<supf>%#<V_sz_elem>	%Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlaldavxq_s])
;;
(define_insn "mve_vmlaldavxq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLALDAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlaldavx.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsldavq_s])
;;
(define_insn "mve_vmlsldavq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLSLDAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsldav.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsldavxq_s])
;;
(define_insn "mve_vmlsldavxq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLSLDAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsldavx.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovnbq_u, vmovnbq_s])
;;
(define_insn "mve_vmovnbq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMOVNBQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovnb.i%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;; vmovnb pattern used by the vec_pack_trunc expander to avoid the
;; need for an uninitialized input operand.
(define_insn "@mve_vec_pack_trunc_lo_<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:MVE_5 1 "s_register_operand" "w")]
	 VMOVNBQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmovnb.i%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovntq_s, vmovntq_u])
;;
(define_insn "@mve_vmovntq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMOVNTQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovnt.i%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_f])
;;
(define_insn "mve_vmulq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(mult:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmul.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_n_f])
;;
(define_insn "mve_vmulq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VMULQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmul.f%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vornq_f])
;;
(define_insn "mve_vornq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(ior:MVE_0 (not:MVE_0 (match_operand:MVE_0 2 "s_register_operand" "w"))
		   (match_operand:MVE_0 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vorn %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vorrq_f])
;;
(define_insn "mve_vorrq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(ior:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		   (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vorr %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vorrq_n_u, vorrq_n_s])
;;
(define_insn "mve_vorrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VORRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vorr.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmullbq_n_s])
;;
(define_insn "mve_vqdmullbq_n_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQDMULLBQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullb.s%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmullbq_s])
;;
(define_insn "mve_vqdmullbq_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQDMULLBQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullb.s%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulltq_n_s])
;;
(define_insn "mve_vqdmulltq_n_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQDMULLTQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullt.s%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulltq_s])
;;
(define_insn "mve_vqdmulltq_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQDMULLTQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullt.s%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovnbq_u, vqmovnbq_s])
;;
(define_insn "mve_vqmovnbq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVNBQ))
  ]
  "TARGET_HAVE_MVE"
  "vqmovnb.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovntq_u, vqmovntq_s])
;;
(define_insn "mve_vqmovntq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVNTQ))
  ]
  "TARGET_HAVE_MVE"
  "vqmovnt.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovunbq_s])
;;
(define_insn "mve_vqmovunbq_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVUNBQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqmovunb.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovuntq_s])
;;
(define_insn "mve_vqmovuntq_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVUNTQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqmovunt.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlaldavhxq_s])
;;
(define_insn "mve_vrmlaldavhxq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLALDAVHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlaldavhx.s32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlsldavhq_s])
;;
(define_insn "mve_vrmlsldavhq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLSLDAVHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlsldavh.s32\t%Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlsldavhxq_s])
;;
(define_insn "mve_vrmlsldavhxq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLSLDAVHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlsldavhx.s32\t%Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshllbq_n_s, vshllbq_n_u])
;;
(define_insn "mve_vshllbq_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:SI 2 "immediate_operand" "i")]
	 VSHLLBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshllb.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlltq_n_u, vshlltq_n_s])
;;
(define_insn "mve_vshlltq_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:SI 2 "immediate_operand" "i")]
	 VSHLLTQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshllt.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_f])
;;
(define_insn "mve_vsubq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(minus:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		     (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vsub.f%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulltq_poly_p])
;;
(define_insn "mve_vmulltq_poly_p<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:MVE_3 2 "s_register_operand" "w")]
	 VMULLTQ_POLY_P))
  ]
  "TARGET_HAVE_MVE"
  "vmullt.p%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmullbq_poly_p])
;;
(define_insn "mve_vmullbq_poly_p<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:MVE_3 2 "s_register_operand" "w")]
	 VMULLBQ_POLY_P))
  ]
  "TARGET_HAVE_MVE"
  "vmullb.p%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlaldavhq_u vrmlaldavhq_s])
;;
(define_insn "mve_vrmlaldavhq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLALDAVHQ))
  ]
  "TARGET_HAVE_MVE"
  "vrmlaldavh.<supf>32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbicq_m_n_s, vbicq_m_n_u])
;;
(define_insn "mve_vbicq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VBICQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vbict.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcmpeqq_m_f])
;;
(define_insn "mve_vcmpeqq_m_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")
		    (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPEQQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	eq, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcvtaq_m_u, vcvtaq_m_s])
;;
(define_insn "mve_vcvtaq_m_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTAQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtat.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcvtq_m_to_f_s, vcvtq_m_to_f_u])
;;
(define_insn "mve_vcvtq_m_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTQ_M_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtt.f%#<V_sz_elem>.<supf>%#<V_sz_elem>	 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vqrshrnbq_n_u, vqrshrnbq_n_s])
;;
(define_insn "mve_vqrshrnbq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")
				 (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VQRSHRNBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqrshrnb.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])
;;
;; [vqrshrunbq_n_s])
;;
(define_insn "mve_vqrshrunbq_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")
				 (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VQRSHRUNBQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrshrunb.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])
;;
;; [vrmlaldavhaq_s vrmlaldavhaq_u])
;;
(define_insn "mve_vrmlaldavhaq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		    (match_operand:V4SI 2 "s_register_operand" "w")
		    (match_operand:V4SI 3 "s_register_operand" "w")]
	 VRMLALDAVHAQ))
  ]
  "TARGET_HAVE_MVE"
  "vrmlaldavha.<supf>32 %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vabavq_s, vabavq_u])
;;
(define_insn "mve_vabavq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VABAVQ))
  ]
  "TARGET_HAVE_MVE"
  "vabav.<supf>%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlcq_u vshlcq_s]
;;
(define_expand "mve_vshlcq_vec_<supf><mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_32")
  (unspec:MVE_2 [(const_int 0)] VSHLCQ)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (SImode);
  emit_insn(gen_mve_vshlcq_<supf><mode>(operands[0], ignore_wb, operands[1],
				      operands[2], operands[3]));
  DONE;
})

(define_expand "mve_vshlcq_carry_<supf><mode>"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_32")
  (unspec:MVE_2 [(const_int 0)] VSHLCQ)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn(gen_mve_vshlcq_<supf><mode>(ignore_vec, operands[0], operands[1],
				      operands[2], operands[3]));
  DONE;
})

(define_insn "mve_vshlcq_<supf><mode>"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 2 "s_register_operand" "0")
		      (match_operand:SI 3 "s_register_operand" "1")
		      (match_operand:SI 4 "mve_imm_32" "Rf")]
	VSHLCQ))
  (set (match_operand:SI  1 "s_register_operand" "=r")
       (unspec:SI [(match_dup 2)
		   (match_dup 3)
		   (match_dup 4)]
	VSHLCQ))]
 "TARGET_HAVE_MVE"
 "vshlc %q0, %1, %4")

;;
;; [vabsq_m_s])
;;
(define_insn "mve_vabsq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VABSQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vabst.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddvaq_p_u, vaddvaq_p_s])
;;
(define_insn "mve_vaddvaq_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VADDVAQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddvat.<supf>%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vclsq_m_s])
;;
(define_insn "mve_vclsq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCLSQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vclst.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vclzq_m_s, vclzq_m_u])
;;
(define_insn "mve_vclzq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCLZQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vclzt.i%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpcsq_m_n_u])
;;
(define_insn "mve_vcmpcsq_m_n_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPCSQ_M_N_U))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.u%#<V_sz_elem>	cs, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpcsq_m_u])
;;
(define_insn "mve_vcmpcsq_m_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPCSQ_M_U))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.u%#<V_sz_elem>	cs, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpeqq_m_n_u, vcmpeqq_m_n_s])
;;
(define_insn "mve_vcmpeqq_m_n_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPEQQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.i%#<V_sz_elem>	eq, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpeqq_m_u, vcmpeqq_m_s])
;;
(define_insn "mve_vcmpeqq_m_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPEQQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.i%#<V_sz_elem>	eq, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgeq_m_n_s])
;;
(define_insn "mve_vcmpgeq_m_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGEQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	ge, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgeq_m_s])
;;
(define_insn "mve_vcmpgeq_m_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGEQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	ge, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgtq_m_n_s])
;;
(define_insn "mve_vcmpgtq_m_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGTQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	gt, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgtq_m_s])
;;
(define_insn "mve_vcmpgtq_m_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGTQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	gt, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmphiq_m_n_u])
;;
(define_insn "mve_vcmphiq_m_n_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPHIQ_M_N_U))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.u%#<V_sz_elem>	hi, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmphiq_m_u])
;;
(define_insn "mve_vcmphiq_m_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPHIQ_M_U))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.u%#<V_sz_elem>	hi, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpleq_m_n_s])
;;
(define_insn "mve_vcmpleq_m_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLEQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	le, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpleq_m_s])
;;
(define_insn "mve_vcmpleq_m_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLEQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	le, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpltq_m_n_s])
;;
(define_insn "mve_vcmpltq_m_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLTQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	lt, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpltq_m_s])
;;
(define_insn "mve_vcmpltq_m_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLTQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.s%#<V_sz_elem>	lt, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpneq_m_n_u, vcmpneq_m_n_s])
;;
(define_insn "mve_vcmpneq_m_n_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPNEQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.i%#<V_sz_elem>	ne, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpneq_m_s, vcmpneq_m_u])
;;
(define_insn "mve_vcmpneq_m_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPNEQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.i%#<V_sz_elem>	ne, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vdupq_m_n_s, vdupq_m_n_u])
;;
(define_insn "mve_vdupq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VDUPQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vdupt.%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxaq_m_s])
;;
(define_insn "mve_vmaxaq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMAXAQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmaxat.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxavq_p_s])
;;
(define_insn "mve_vmaxavq_p_s<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMAXAVQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmaxavt.s%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxvq_p_u, vmaxvq_p_s])
;;
(define_insn "mve_vmaxvq_p_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMAXVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmaxvt.<supf>%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vminaq_m_s])
;;
(define_insn "mve_vminaq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMINAQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vminat.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vminavq_p_s])
;;
(define_insn "mve_vminavq_p_s<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMINAVQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vminavt.s%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vminvq_p_s, vminvq_p_u])
;;
(define_insn "mve_vminvq_p_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMINVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vminvt.<supf>%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmladavaq_u, vmladavaq_s])
;;
(define_insn "mve_vmladavaq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VMLADAVAQ))
  ]
  "TARGET_HAVE_MVE"
  "vmladava.<supf>%#<V_sz_elem>	%0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmladavq_p_u, vmladavq_p_s])
;;
(define_insn "mve_vmladavq_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLADAVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmladavt.<supf>%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmladavxq_p_s])
;;
(define_insn "mve_vmladavxq_p_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLADAVXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmladavxt.s%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaq_n_u, vmlaq_n_s])
;;
(define_insn "mve_vmlaq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VMLAQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vmla.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlasq_n_u, vmlasq_n_s])
;;
(define_insn "mve_vmlasq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VMLASQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vmlas.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsdavq_p_s])
;;
(define_insn "mve_vmlsdavq_p_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLSDAVQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsdavt.s%#<V_sz_elem>	%0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlsdavxq_p_s])
;;
(define_insn "mve_vmlsdavxq_p_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLSDAVXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsdavxt.s%#<V_sz_elem>	%0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmvnq_m_s, vmvnq_m_u])
;;
(define_insn "mve_vmvnq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMVNQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmvnt %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vnegq_m_s])
;;
(define_insn "mve_vnegq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VNEGQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vnegt.s%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vpselq_u, vpselq_s])
;;
(define_insn "@mve_vpselq_<supf><mode>"
  [
   (set (match_operand:MVE_1 0 "s_register_operand" "=w")
	(unspec:MVE_1 [(match_operand:MVE_1 1 "s_register_operand" "w")
		       (match_operand:MVE_1 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VPSELQ))
  ]
  "TARGET_HAVE_MVE"
  "vpsel %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqabsq_m_s])
;;
(define_insn "mve_vqabsq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQABSQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqabst.s%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmlahq_n_s])
;;
(define_insn "mve_vqdmlahq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VQDMLAHQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqdmlah.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmlashq_n_s])
;;
(define_insn "mve_vqdmlashq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VQDMLASHQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqdmlash.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqnegq_m_s])
;;
(define_insn "mve_vqnegq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQNEGQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqnegt.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmladhq_s])
;;
(define_insn "mve_vqrdmladhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQRDMLADHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmladh.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmladhxq_s])
;;
(define_insn "mve_vqrdmladhxq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQRDMLADHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmladhx.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmlahq_n_s])
;;
(define_insn "mve_vqrdmlahq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VQRDMLAHQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmlah.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmlashq_n_s])
;;
(define_insn "mve_vqrdmlashq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VQRDMLASHQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmlash.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmlsdhq_s])
;;
(define_insn "mve_vqrdmlsdhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQRDMLSDHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmlsdh.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmlsdhxq_s])
;;
(define_insn "mve_vqrdmlsdhxq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQRDMLSDHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmlsdhx.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrshlq_m_n_s, vqrshlq_m_n_u])
;;
(define_insn "mve_vqrshlq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQRSHLQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrshlt.<supf>%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshlq_m_r_u, vqshlq_m_r_s])
;;
(define_insn "mve_vqshlq_m_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQSHLQ_M_R))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqshlt.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev64q_m_u, vrev64q_m_s])
;;
(define_insn "mve_vrev64q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VREV64Q_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrev64t.%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshlq_m_n_s, vrshlq_m_n_u])
;;
(define_insn "mve_vrshlq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRSHLQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrshlt.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshlq_m_r_u, vshlq_m_r_s])
;;
(define_insn "mve_vshlq_m_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VSHLQ_M_R))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshlt.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vsliq_n_u, vsliq_n_s])
;;
(define_insn "mve_vsliq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred>" "<MVE_constraint>")]
	 VSLIQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vsli.%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vsriq_n_u, vsriq_n_s])
;;
(define_insn "mve_vsriq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_selective_upto_8" "Rg")]
	 VSRIQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vsri.%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmlsdhxq_s])
;;
(define_insn "mve_vqdmlsdhxq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQDMLSDHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmlsdhx.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmlsdhq_s])
;;
(define_insn "mve_vqdmlsdhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQDMLSDHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmlsdh.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmladhxq_s])
;;
(define_insn "mve_vqdmladhxq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQDMLADHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmladhx.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmladhq_s])
;;
(define_insn "mve_vqdmladhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VQDMLADHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmladh.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsdavaxq_s])
;;
(define_insn "mve_vmlsdavaxq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VMLSDAVAXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsdavax.s%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsdavaq_s])
;;
(define_insn "mve_vmlsdavaq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VMLSDAVAQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsdava.s%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmladavaxq_s])
;;
(define_insn "mve_vmladavaxq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VMLADAVAXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmladavax.s%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
])
;;
;; [vabsq_m_f])
;;
(define_insn "mve_vabsq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VABSQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vabst.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddlvaq_p_s vaddlvaq_p_u])
;;
(define_insn "mve_vaddlvaq_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VADDLVAQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddlvat.<supf>32 %Q0, %R0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcmlaq, vcmlaq_rot90, vcmlaq_rot180, vcmlaq_rot270])
;;
(define_insn "mve_vcmlaq<mve_rot><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w,w")
	(plus:MVE_0 (match_operand:MVE_0 1 "reg_or_zero_operand" "Dz,0")
		    (unspec:MVE_0
		        [(match_operand:MVE_0 2 "s_register_operand" "w,w")
		         (match_operand:MVE_0 3 "s_register_operand" "w,w")]
		     VCMLA)))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "@
   vcmul.f%#<V_sz_elem>	%q0, %q2, %q3, #<rot>
   vcmla.f%#<V_sz_elem>	%q0, %q2, %q3, #<rot>"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_m_n_f])
;;
(define_insn "mve_vcmpeqq_m_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPEQQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	eq, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgeq_m_f])
;;
(define_insn "mve_vcmpgeq_m_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGEQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	ge, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgeq_m_n_f])
;;
(define_insn "mve_vcmpgeq_m_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGEQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	ge, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgtq_m_f])
;;
(define_insn "mve_vcmpgtq_m_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGTQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	gt, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpgtq_m_n_f])
;;
(define_insn "mve_vcmpgtq_m_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPGTQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	gt, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpleq_m_f])
;;
(define_insn "mve_vcmpleq_m_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLEQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	le, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpleq_m_n_f])
;;
(define_insn "mve_vcmpleq_m_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLEQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	le, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpltq_m_f])
;;
(define_insn "mve_vcmpltq_m_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLTQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	lt, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpltq_m_n_f])
;;
(define_insn "mve_vcmpltq_m_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPLTQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	lt, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpneq_m_f])
;;
(define_insn "mve_vcmpneq_m_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPNEQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	ne, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpneq_m_n_f])
;;
(define_insn "mve_vcmpneq_m_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPNEQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	ne, %q1, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtbq_m_f16_f32])
;;
(define_insn "mve_vcvtbq_m_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		       (match_operand:V4SF 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTBQ_M_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtbt.f16.f32 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtbq_m_f32_f16])
;;
(define_insn "mve_vcvtbq_m_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V4SF 1 "s_register_operand" "0")
		       (match_operand:V8HF 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTBQ_M_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtbt.f32.f16 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvttq_m_f16_f32])
;;
(define_insn "mve_vcvttq_m_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		       (match_operand:V4SF 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTTQ_M_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvttt.f16.f32 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvttq_m_f32_f16])
;;
(define_insn "mve_vcvttq_m_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V4SF 1 "s_register_operand" "0")
		       (match_operand:V8HF 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTTQ_M_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvttt.f32.f16 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vdupq_m_n_f])
;;
(define_insn "mve_vdupq_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VDUPQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vdupt.%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vfmaq_f])
;;
(define_insn "mve_vfmaq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")]
	 VFMAQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vfma.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vfmaq_n_f])
;;
(define_insn "mve_vfmaq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VFMAQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vfma.f%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vfmasq_n_f])
;;
(define_insn "mve_vfmasq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 VFMASQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vfmas.f%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])
;;
;; [vfmsq_f])
;;
(define_insn "mve_vfmsq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")]
	 VFMSQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vfms.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmaq_m_f])
;;
(define_insn "mve_vmaxnmaq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMAXNMAQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vmaxnmat.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vmaxnmavq_p_f])
;;
(define_insn "mve_vmaxnmavq_p_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMAXNMAVQ_P_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vmaxnmavt.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxnmvq_p_f])
;;
(define_insn "mve_vmaxnmvq_p_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMAXNMVQ_P_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vmaxnmvt.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vminnmaq_m_f])
;;
(define_insn "mve_vminnmaq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMINNMAQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vminnmat.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vminnmavq_p_f])
;;
(define_insn "mve_vminnmavq_p_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMINNMAVQ_P_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vminnmavt.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vminnmvq_p_f])
;;
(define_insn "mve_vminnmvq_p_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMINNMVQ_P_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vminnmvt.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaldavaq_s, vmlaldavaq_u])
;;
(define_insn "mve_vmlaldavaq_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")]
	 VMLALDAVAQ))
  ]
  "TARGET_HAVE_MVE"
  "vmlaldava.<supf>%#<V_sz_elem> %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlaldavaxq_s])
;;
(define_insn "mve_vmlaldavaxq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")]
	 VMLALDAVAXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlaldavax.s%#<V_sz_elem> %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlaldavq_p_u, vmlaldavq_p_s])
;;
(define_insn "mve_vmlaldavq_p_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLALDAVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlaldavt.<supf>%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaldavxq_p_s])
;;
(define_insn "mve_vmlaldavxq_p_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLALDAVXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlaldavxt.s%#<V_sz_elem>\t%Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vmlsldavaq_s])
;;
(define_insn "mve_vmlsldavaq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")]
	 VMLSLDAVAQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsldava.s%#<V_sz_elem> %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsldavaxq_s])
;;
(define_insn "mve_vmlsldavaxq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")]
	 VMLSLDAVAXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsldavax.s%#<V_sz_elem> %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsldavq_p_s])
;;
(define_insn "mve_vmlsldavq_p_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLSLDAVQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsldavt.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlsldavxq_p_s])
;;
(define_insn "mve_vmlsldavxq_p_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMLSLDAVXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsldavxt.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vmovlbq_m_u, vmovlbq_m_s])
;;
(define_insn "mve_vmovlbq_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMOVLBQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmovlbt.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vmovltq_m_u, vmovltq_m_s])
;;
(define_insn "mve_vmovltq_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMOVLTQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmovltt.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vmovnbq_m_u, vmovnbq_m_s])
;;
(define_insn "mve_vmovnbq_m_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMOVNBQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmovnbt.i%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmovntq_m_u, vmovntq_m_s])
;;
(define_insn "mve_vmovntq_m_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMOVNTQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmovntt.i%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmvnq_m_n_u, vmvnq_m_n_s])
;;
(define_insn "mve_vmvnq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VMVNQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmvnt.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vnegq_m_f])
;;
(define_insn "mve_vnegq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VNEGQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vnegt.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vorrq_m_n_s, vorrq_m_n_u])
;;
(define_insn "mve_vorrq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VORRQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vorrt.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vpselq_f])
;;
(define_insn "@mve_vpselq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VPSELQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpsel %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovnbq_m_s, vqmovnbq_m_u])
;;
(define_insn "mve_vqmovnbq_m_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQMOVNBQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqmovnbt.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqmovntq_m_u, vqmovntq_m_s])
;;
(define_insn "mve_vqmovntq_m_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQMOVNTQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqmovntt.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqmovunbq_m_s])
;;
(define_insn "mve_vqmovunbq_m_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQMOVUNBQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqmovunbt.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqmovuntq_m_s])
;;
(define_insn "mve_vqmovuntq_m_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VQMOVUNTQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqmovuntt.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrshrntq_n_u, vqrshrntq_n_s])
;;
(define_insn "mve_vqrshrntq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VQRSHRNTQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqrshrnt.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrshruntq_n_s])
;;
(define_insn "mve_vqrshruntq_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VQRSHRUNTQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrshrunt.s%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshrnbq_n_u, vqshrnbq_n_s])
;;
(define_insn "mve_vqshrnbq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")]
	 VQSHRNBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqshrnb.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshrntq_n_u, vqshrntq_n_s])
;;
(define_insn "mve_vqshrntq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")]
	 VQSHRNTQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqshrnt.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshrunbq_n_s])
;;
(define_insn "mve_vqshrunbq_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")]
	 VQSHRUNBQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqshrunb.s%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshruntq_n_s])
;;
(define_insn "mve_vqshruntq_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")]
	 VQSHRUNTQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqshrunt.s%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev32q_m_f])
;;
(define_insn "mve_vrev32q_m_fv8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		       (match_operand:V8HF 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VREV32Q_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrev32t.16 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev32q_m_s, vrev32q_m_u])
;;
(define_insn "mve_vrev32q_m_<supf><mode>"
  [
   (set (match_operand:MVE_3 0 "s_register_operand" "=w")
	(unspec:MVE_3 [(match_operand:MVE_3 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VREV32Q_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrev32t.%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev64q_m_f])
;;
(define_insn "mve_vrev64q_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VREV64Q_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrev64t.%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlaldavhaxq_s])
;;
(define_insn "mve_vrmlaldavhaxq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")]
	 VRMLALDAVHAXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlaldavhax.s32 %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlaldavhxq_p_s])
;;
(define_insn "mve_vrmlaldavhxq_p_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRMLALDAVHXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlaldavhxt.s32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlsldavhaxq_s])
;;
(define_insn "mve_vrmlsldavhaxq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")]
	 VRMLSLDAVHAXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlsldavhax.s32 %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlsldavhq_p_s])
;;
(define_insn "mve_vrmlsldavhq_p_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRMLSLDAVHQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlsldavht.s32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlsldavhxq_p_s])
;;
(define_insn "mve_vrmlsldavhxq_p_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRMLSLDAVHXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlsldavhxt.s32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrndaq_m_f])
;;
(define_insn "mve_vrndaq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRNDAQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrintat.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrndmq_m_f])
;;
(define_insn "mve_vrndmq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRNDMQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrintmt.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrndnq_m_f])
;;
(define_insn "mve_vrndnq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRNDNQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrintnt.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrndpq_m_f])
;;
(define_insn "mve_vrndpq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRNDPQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrintpt.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrndxq_m_f])
;;
(define_insn "mve_vrndxq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRNDXQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vrintxt.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshrnbq_n_s, vrshrnbq_n_u])
;;
(define_insn "mve_vrshrnbq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VRSHRNBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vrshrnb.i%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vrshrntq_n_u, vrshrntq_n_s])
;;
(define_insn "mve_vrshrntq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VRSHRNTQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vrshrnt.i%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vshrnbq_n_u, vshrnbq_n_s])
;;
(define_insn "mve_vshrnbq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")]
	 VSHRNBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshrnb.i%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vshrntq_n_s, vshrntq_n_u])
;;
(define_insn "mve_vshrntq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")
				 (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")]
	 VSHRNTQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshrnt.i%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtmq_m_s, vcvtmq_m_u])
;;
(define_insn "mve_vcvtmq_m_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTMQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtmt.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtpq_m_u, vcvtpq_m_s])
;;
(define_insn "mve_vcvtpq_m_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTPQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtpt.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtnq_m_s, vcvtnq_m_u])
;;
(define_insn "mve_vcvtnq_m_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTNQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtnt.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtq_m_n_from_f_s, vcvtq_m_n_from_f_u])
;;
(define_insn "mve_vcvtq_m_n_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCVTQ_M_N_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtt.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev16q_m_u, vrev16q_m_s])
;;
(define_insn "mve_vrev16q_m_<supf>v16qi"
  [
   (set (match_operand:V16QI 0 "s_register_operand" "=w")
	(unspec:V16QI [(match_operand:V16QI 1 "s_register_operand" "0")
		       (match_operand:V16QI 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VREV16Q_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrev16t.8 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtq_m_from_f_u, vcvtq_m_from_f_s])
;;
(define_insn "mve_vcvtq_m_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTQ_M_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtt.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlaldavhq_p_u vrmlaldavhq_p_s])
;;
(define_insn "mve_vrmlaldavhq_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")
		    (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VRMLALDAVHQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlaldavht.<supf>32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlsldavhaq_s])
;;
(define_insn "mve_vrmlsldavhaq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		    (match_operand:V4SI 2 "s_register_operand" "w")
		    (match_operand:V4SI 3 "s_register_operand" "w")]
	 VRMLSLDAVHAQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlsldavha.s32 %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vabavq_p_s, vabavq_p_u])
;;
(define_insn "mve_vabavq_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")
		    (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VABAVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vabavt.<supf>%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshluq_m_n_s])
;;
(define_insn "mve_vqshluq_m_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_7" "Ra")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSHLUQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\n\tvqshlut.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")])

;;
;; [vshlq_m_s, vshlq_m_u])
;;
(define_insn "mve_vshlq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSHLQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshlt.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")])

;;
;; [vsriq_m_n_s, vsriq_m_n_u])
;;
(define_insn "mve_vsriq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_selective_upto_8" "Rg")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSRIQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vsrit.%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")])

;;
;; [vsubq_m_u, vsubq_m_s])
;;
(define_insn "mve_vsubq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSUBQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vsubt.i%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")])

;;
;; [vcvtq_m_n_to_f_u, vcvtq_m_n_to_f_s])
;;
(define_insn "mve_vcvtq_m_n_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCVTQ_M_N_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtt.f%#<V_sz_elem>.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vabdq_m_s, vabdq_m_u])
;;
(define_insn "mve_vabdq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VABDQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vabdt.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddq_m_n_s, vaddq_m_n_u])
;;
(define_insn "mve_vaddq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VADDQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddt.i%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddq_m_u, vaddq_m_s])
;;
(define_insn "mve_vaddq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VADDQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddt.i%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vandq_m_u, vandq_m_s])
;;
(define_insn "mve_vandq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VANDQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vandt %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vbicq_m_u, vbicq_m_s])
;;
(define_insn "mve_vbicq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VBICQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vbict %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vbrsrq_m_n_u, vbrsrq_m_n_s])
;;
(define_insn "mve_vbrsrq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VBRSRQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vbrsrt.%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcaddq_rot270_m_u, vcaddq_rot270_m_s])
;;
(define_insn "mve_vcaddq_rot270_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCADDQ_ROT270_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcaddt.i%#<V_sz_elem>	%q0, %q2, %q3, #270"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcaddq_rot90_m_u, vcaddq_rot90_m_s])
;;
(define_insn "mve_vcaddq_rot90_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCADDQ_ROT90_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcaddt.i%#<V_sz_elem>	%q0, %q2, %q3, #90"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [veorq_m_s, veorq_m_u])
;;
(define_insn "mve_veorq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VEORQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;veort %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vhaddq_m_n_s, vhaddq_m_n_u])
;;
(define_insn "mve_vhaddq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VHADDQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vhaddt.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vhaddq_m_s, vhaddq_m_u])
;;
(define_insn "mve_vhaddq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VHADDQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vhaddt.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vhsubq_m_n_s, vhsubq_m_n_u])
;;
(define_insn "mve_vhsubq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VHSUBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vhsubt.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vhsubq_m_s, vhsubq_m_u])
;;
(define_insn "mve_vhsubq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VHSUBQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vhsubt.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxq_m_s, vmaxq_m_u])
;;
(define_insn "mve_vmaxq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMAXQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmaxt.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vminq_m_s, vminq_m_u])
;;
(define_insn "mve_vminq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMINQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmint.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmladavaq_p_u, vmladavaq_p_s])
;;
(define_insn "mve_vmladavaq_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")
		    (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLADAVAQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmladavat.<supf>%#<V_sz_elem>	%0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaq_m_n_s, vmlaq_m_n_u])
;;
(define_insn "mve_vmlaq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLAQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlat.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlasq_m_n_u, vmlasq_m_n_s])
;;
(define_insn "mve_vmlasq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLASQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlast.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmulhq_m_s, vmulhq_m_u])
;;
(define_insn "mve_vmulhq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULHQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmulht.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmullbq_int_m_u, vmullbq_int_m_s])
;;
(define_insn "mve_vmullbq_int_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
				  (match_operand:MVE_2 2 "s_register_operand" "w")
				  (match_operand:MVE_2 3 "s_register_operand" "w")
				  (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULLBQ_INT_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmullbt.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmulltq_int_m_s, vmulltq_int_m_u])
;;
(define_insn "mve_vmulltq_int_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
				  (match_operand:MVE_2 2 "s_register_operand" "w")
				  (match_operand:MVE_2 3 "s_register_operand" "w")
				  (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULLTQ_INT_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmulltt.<supf>%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmulq_m_n_u, vmulq_m_n_s])
;;
(define_insn "mve_vmulq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmult.i%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmulq_m_s, vmulq_m_u])
;;
(define_insn "mve_vmulq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmult.i%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vornq_m_u, vornq_m_s])
;;
(define_insn "mve_vornq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VORNQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vornt %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vorrq_m_s, vorrq_m_u])
;;
(define_insn "mve_vorrq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VORRQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vorrt %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqaddq_m_n_u, vqaddq_m_n_s])
;;
(define_insn "mve_vqaddq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQADDQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqaddt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqaddq_m_u, vqaddq_m_s])
;;
(define_insn "mve_vqaddq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQADDQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqaddt.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmlahq_m_n_s])
;;
(define_insn "mve_vqdmlahq_m_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMLAHQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmlaht.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmlashq_m_n_s])
;;
(define_insn "mve_vqdmlashq_m_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMLASHQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmlasht.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmlahq_m_n_s])
;;
(define_insn "mve_vqrdmlahq_m_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMLAHQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmlaht.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmlashq_m_n_s])
;;
(define_insn "mve_vqrdmlashq_m_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMLASHQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmlasht.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrshlq_m_u, vqrshlq_m_s])
;;
(define_insn "mve_vqrshlq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRSHLQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrshlt.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshlq_m_n_s, vqshlq_m_n_u])
;;
(define_insn "mve_vqshlq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSHLQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqshlt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshlq_m_u, vqshlq_m_s])
;;
(define_insn "mve_vqshlq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSHLQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqshlt.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqsubq_m_n_u, vqsubq_m_n_s])
;;
(define_insn "mve_vqsubq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSUBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqsubt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqsubq_m_u, vqsubq_m_s])
;;
(define_insn "mve_vqsubq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSUBQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqsubt.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrhaddq_m_u, vrhaddq_m_s])
;;
(define_insn "mve_vrhaddq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRHADDQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrhaddt.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmulhq_m_u, vrmulhq_m_s])
;;
(define_insn "mve_vrmulhq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRMULHQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmulht.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshlq_m_s, vrshlq_m_u])
;;
(define_insn "mve_vrshlq_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRSHLQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrshlt.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshrq_m_n_s, vrshrq_m_n_u])
;;
(define_insn "mve_vrshrq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRSHRQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrshrt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshlq_m_n_s, vshlq_m_n_u])
;;
(define_insn "mve_vshlq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSHLQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshlt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshrq_m_n_s, vshrq_m_n_u])
;;
(define_insn "mve_vshrq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSHRQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshrt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vsliq_m_n_u, vsliq_m_n_s])
;;
(define_insn "mve_vsliq_m_n_<supf><mode>"
   [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred>" "<MVE_constraint>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSLIQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vslit.%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vsubq_m_n_s, vsubq_m_n_u])
;;
(define_insn "mve_vsubq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSUBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vsubt.i%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vhcaddq_rot270_m_s])
;;
(define_insn "mve_vhcaddq_rot270_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VHCADDQ_ROT270_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vhcaddt.s%#<V_sz_elem>\t%q0, %q2, %q3, #270"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vhcaddq_rot90_m_s])
;;
(define_insn "mve_vhcaddq_rot90_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VHCADDQ_ROT90_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vhcaddt.s%#<V_sz_elem>\t%q0, %q2, %q3, #90"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmladavaxq_p_s])
;;
(define_insn "mve_vmladavaxq_p_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLADAVAXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmladavaxt.s%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlsdavaq_p_s])
;;
(define_insn "mve_vmlsdavaq_p_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLSDAVAQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsdavat.s%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlsdavaxq_p_s])
;;
(define_insn "mve_vmlsdavaxq_p_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLSDAVAXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsdavaxt.s%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmladhq_m_s])
;;
(define_insn "mve_vqdmladhq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMLADHQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmladht.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmladhxq_m_s])
;;
(define_insn "mve_vqdmladhxq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMLADHXQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmladhxt.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmlsdhq_m_s])
;;
(define_insn "mve_vqdmlsdhq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMLSDHQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmlsdht.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmlsdhxq_m_s])
;;
(define_insn "mve_vqdmlsdhxq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMLSDHXQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmlsdhxt.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmulhq_m_n_s])
;;
(define_insn "mve_vqdmulhq_m_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMULHQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmulht.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmulhq_m_s])
;;
(define_insn "mve_vqdmulhq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMULHQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmulht.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmladhq_m_s])
;;
(define_insn "mve_vqrdmladhq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMLADHQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmladht.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmladhxq_m_s])
;;
(define_insn "mve_vqrdmladhxq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMLADHXQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmladhxt.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmlsdhq_m_s])
;;
(define_insn "mve_vqrdmlsdhq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMLSDHQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmlsdht.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmlsdhxq_m_s])
;;
(define_insn "mve_vqrdmlsdhxq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMLSDHXQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmlsdhxt.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmulhq_m_n_s])
;;
(define_insn "mve_vqrdmulhq_m_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMULHQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmulht.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrdmulhq_m_s])
;;
(define_insn "mve_vqrdmulhq_m_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRDMULHQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrdmulht.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaldavaq_p_u, vmlaldavaq_p_s])
;;
(define_insn "mve_vmlaldavaq_p_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLALDAVAQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlaldavat.<supf>%#<V_sz_elem>	%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaldavaxq_p_s])
;;
(define_insn "mve_vmlaldavaxq_p_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLALDAVAXQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlaldavaxt.<supf>%#<V_sz_elem> %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrshrnbq_m_n_u, vqrshrnbq_m_n_s])
;;
(define_insn "mve_vqrshrnbq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRSHRNBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrshrnbt.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrshrntq_m_n_s, vqrshrntq_m_n_u])
;;
(define_insn "mve_vqrshrntq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRSHRNTQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrshrntt.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshrnbq_m_n_u, vqshrnbq_m_n_s])
;;
(define_insn "mve_vqshrnbq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSHRNBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\n\tvqshrnbt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshrntq_m_n_s, vqshrntq_m_n_u])
;;
(define_insn "mve_vqshrntq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSHRNTQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqshrntt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlaldavhaq_p_s])
;;
(define_insn "mve_vrmlaldavhaq_p_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRMLALDAVHAQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlaldavhat.s32\t%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshrnbq_m_n_u, vrshrnbq_m_n_s])
;;
(define_insn "mve_vrshrnbq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRSHRNBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrshrnbt.i%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshrntq_m_n_u, vrshrntq_m_n_s])
;;
(define_insn "mve_vrshrntq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRSHRNTQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrshrntt.i%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshllbq_m_n_u, vshllbq_m_n_s])
;;
(define_insn "mve_vshllbq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSHLLBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshllbt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshlltq_m_n_u, vshlltq_m_n_s])
;;
(define_insn "mve_vshlltq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSHLLTQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshlltt.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshrnbq_m_n_s, vshrnbq_m_n_u])
;;
(define_insn "mve_vshrnbq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSHRNBQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshrnbt.i%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshrntq_m_n_s, vshrntq_m_n_u])
;;
(define_insn "mve_vshrntq_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSHRNTQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vshrntt.i%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlsldavaq_p_s])
;;
(define_insn "mve_vmlsldavaq_p_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLSLDAVAQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsldavat.s%#<V_sz_elem>\t%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlsldavaxq_p_s])
;;
(define_insn "mve_vmlsldavaxq_p_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMLSLDAVAXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmlsldavaxt.s%#<V_sz_elem>\t%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmullbq_poly_m_p])
;;
(define_insn "mve_vmullbq_poly_m_p<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:MVE_3 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULLBQ_POLY_M_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmullbt.p%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmulltq_poly_m_p])
;;
(define_insn "mve_vmulltq_poly_m_p<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:MVE_3 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULLTQ_POLY_M_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vmulltt.p%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmullbq_m_n_s])
;;
(define_insn "mve_vqdmullbq_m_n_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMULLBQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmullbt.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmullbq_m_s])
;;
(define_insn "mve_vqdmullbq_m_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMULLBQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmullbt.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmulltq_m_n_s])
;;
(define_insn "mve_vqdmulltq_m_n_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMULLTQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmulltt.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmulltq_m_s])
;;
(define_insn "mve_vqdmulltq_m_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQDMULLTQ_M_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqdmulltt.s%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrshrunbq_m_n_s])
;;
(define_insn "mve_vqrshrunbq_m_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "mve_imm_8" "Rb")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRSHRUNBQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrshrunbt.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrshruntq_m_n_s])
;;
(define_insn "mve_vqrshruntq_m_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQRSHRUNTQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqrshruntt.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshrunbq_m_n_s])
;;
(define_insn "mve_vqshrunbq_m_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSHRUNBQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqshrunbt.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshruntq_m_n_s])
;;
(define_insn "mve_vqshruntq_m_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VQSHRUNTQ_M_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vqshruntt.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlaldavhaq_p_u])
;;
(define_insn "mve_vrmlaldavhaq_p_uv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRMLALDAVHAQ_P_U))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlaldavhat.u32\t%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlaldavhaxq_p_s])
;;
(define_insn "mve_vrmlaldavhaxq_p_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRMLALDAVHAXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlaldavhaxt.s32\t%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlsldavhaq_p_s])
;;
(define_insn "mve_vrmlsldavhaq_p_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRMLSLDAVHAQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlsldavhat.s32\t%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlsldavhaxq_p_s])
;;
(define_insn "mve_vrmlsldavhaxq_p_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VRMLSLDAVHAXQ_P_S))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vrmlsldavhaxt.s32\t%Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vabdq_m_f])
;;
(define_insn "mve_vabdq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VABDQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vabdt.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddq_m_f])
;;
(define_insn "mve_vaddq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VADDQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vaddt.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddq_m_n_f])
;;
(define_insn "mve_vaddq_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VADDQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vaddt.f%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vandq_m_f])
;;
(define_insn "mve_vandq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VANDQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vandt %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vbicq_m_f])
;;
(define_insn "mve_vbicq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VBICQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vbict %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vbrsrq_m_n_f])
;;
(define_insn "mve_vbrsrq_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:SI 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VBRSRQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vbrsrt.%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcaddq_rot270_m_f])
;;
(define_insn "mve_vcaddq_rot270_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCADDQ_ROT270_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcaddt.f%#<V_sz_elem>	%q0, %q2, %q3, #270"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcaddq_rot90_m_f])
;;
(define_insn "mve_vcaddq_rot90_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCADDQ_ROT90_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcaddt.f%#<V_sz_elem>	%q0, %q2, %q3, #90"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmlaq_m_f])
;;
(define_insn "mve_vcmlaq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMLAQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmlat.f%#<V_sz_elem>	%q0, %q2, %q3, #0"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmlaq_rot180_m_f])
;;
(define_insn "mve_vcmlaq_rot180_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMLAQ_ROT180_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmlat.f%#<V_sz_elem>	%q0, %q2, %q3, #180"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmlaq_rot270_m_f])
;;
(define_insn "mve_vcmlaq_rot270_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMLAQ_ROT270_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmlat.f%#<V_sz_elem>	%q0, %q2, %q3, #270"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmlaq_rot90_m_f])
;;
(define_insn "mve_vcmlaq_rot90_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMLAQ_ROT90_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmlat.f%#<V_sz_elem>	%q0, %q2, %q3, #90"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmulq_m_f])
;;
(define_insn "mve_vcmulq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMULQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmult.f%#<V_sz_elem>	%q0, %q2, %q3, #0"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmulq_rot180_m_f])
;;
(define_insn "mve_vcmulq_rot180_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMULQ_ROT180_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmult.f%#<V_sz_elem>	%q0, %q2, %q3, #180"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmulq_rot270_m_f])
;;
(define_insn "mve_vcmulq_rot270_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMULQ_ROT270_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmult.f%#<V_sz_elem>	%q0, %q2, %q3, #270"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmulq_rot90_m_f])
;;
(define_insn "mve_vcmulq_rot90_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VCMULQ_ROT90_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmult.f%#<V_sz_elem>	%q0, %q2, %q3, #90"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [veorq_m_f])
;;
(define_insn "mve_veorq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VEORQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;veort %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vfmaq_m_f])
;;
(define_insn "mve_vfmaq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VFMAQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vfmat.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vfmaq_m_n_f])
;;
(define_insn "mve_vfmaq_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VFMAQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vfmat.f%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vfmasq_m_n_f])
;;
(define_insn "mve_vfmasq_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VFMASQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vfmast.f%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vfmsq_m_f])
;;
(define_insn "mve_vfmsq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VFMSQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vfmst.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxnmq_m_f])
;;
(define_insn "mve_vmaxnmq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMAXNMQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vmaxnmt.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vminnmq_m_f])
;;
(define_insn "mve_vminnmq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMINNMQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vminnmt.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmulq_m_f])
;;
(define_insn "mve_vmulq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vmult.f%#<V_sz_elem>	%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmulq_m_n_f])
;;
(define_insn "mve_vmulq_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VMULQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vmult.f%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vornq_m_f])
;;
(define_insn "mve_vornq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VORNQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vornt %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vorrq_m_f])
;;
(define_insn "mve_vorrq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VORRQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vorrt %q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vsubq_m_f])
;;
(define_insn "mve_vsubq_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSUBQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vsubt.f%#<V_sz_elem>\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vsubq_m_n_f])
;;
(define_insn "mve_vsubq_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSUBQ_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vsubt.f%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vstrbq_s vstrbq_u]
;;
(define_insn "mve_vstrbq_<supf><mode>"
  [(set (match_operand:<MVE_B_ELEM> 0 "mve_memory_operand" "=Ux")
	(unspec:<MVE_B_ELEM> [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VSTRBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn("vstrb.<V_sz_elem>\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrbq_scatter_offset_s vstrbq_scatter_offset_u]
;;
(define_expand "mve_vstrbq_scatter_offset_<supf><mode>"
  [(match_operand:<MVE_B_ELEM> 0 "mve_scatter_memory")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:MVE_2 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRBSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrbq_scatter_offset_<supf><mode>_insn (ind, operands[1],
							      operands[2]));
  DONE;
})

(define_insn "mve_vstrbq_scatter_offset_<supf><mode>_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:MVE_2 1 "s_register_operand" "w")
	   (match_operand:MVE_2 2 "s_register_operand" "w")]
	  VSTRBSOQ))]
  "TARGET_HAVE_MVE"
  "vstrb.<V_sz_elem>\t%q2, [%0, %q1]"
  [(set_attr "length" "4")])

;;
;; [vstrwq_scatter_base_s vstrwq_scatter_base_u]
;;
(define_insn "mve_vstrwq_scatter_base_<supf>v4si"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V4SI 0 "s_register_operand" "w")
		 (match_operand:SI 1 "immediate_operand" "i")
		 (match_operand:V4SI 2 "s_register_operand" "w")]
	 VSTRWSBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn("vstrw.u32\t%q2, [%q0, %1]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrbq_gather_offset_s vldrbq_gather_offset_u]
;;
(define_insn "mve_vldrbq_gather_offset_<supf><mode>"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=&w")
	(unspec:MVE_2 [(match_operand:<MVE_B_ELEM> 1 "memory_operand" "Us")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VLDRBGOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   if (!strcmp ("<supf>","s") && <V_sz_elem> == 8)
     output_asm_insn ("vldrb.u8\t%q0, [%m1, %q2]",ops);
   else
     output_asm_insn ("vldrb.<supf><V_sz_elem>\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrbq_s vldrbq_u]
;;
(define_insn "mve_vldrbq_<supf><mode>"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:<MVE_B_ELEM> 1 "mve_memory_operand" "Ux")]
	 VLDRBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   if (<V_sz_elem> == 8)
     output_asm_insn ("vldrb.<V_sz_elem>\t%q0, %E1",ops);
   else
     output_asm_insn ("vldrb.<supf><V_sz_elem>\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_gather_base_s vldrwq_gather_base_u]
;;
(define_insn "mve_vldrwq_gather_base_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")]
	 VLDRWGBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrw.u32\t%q0, [%q1, %2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrbq_scatter_offset_p_s vstrbq_scatter_offset_p_u]
;;
(define_expand "mve_vstrbq_scatter_offset_p_<supf><mode>"
  [(match_operand:<MVE_B_ELEM>  0 "mve_scatter_memory")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:MVE_2 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand" "Up")
   (unspec:V4SI [(const_int 0)] VSTRBSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrbq_scatter_offset_p_<supf><mode>_insn (ind, operands[1],
						       operands[2],
						       operands[3]));
  DONE;
})

(define_insn "mve_vstrbq_scatter_offset_p_<supf><mode>_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:MVE_2 1 "s_register_operand" "w")
	   (match_operand:MVE_2 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRBSOQ))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrbt.<V_sz_elem>\t%q2, [%0, %q1]"
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_base_p_s vstrwq_scatter_base_p_u]
;;
(define_insn "mve_vstrwq_scatter_base_p_<supf>v4si"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V4SI 0 "s_register_operand" "w")
		 (match_operand:SI 1 "immediate_operand" "i")
		 (match_operand:V4SI 2 "s_register_operand" "w")
		 (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VSTRWSBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vpst\n\tvstrwt.u32\t%q2, [%q0, %1]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrbq_p_s vstrbq_p_u]
;;
(define_insn "mve_vstrbq_p_<supf><mode>"
  [(set (match_operand:<MVE_B_ELEM> 0 "mve_memory_operand" "=Ux")
	(unspec:<MVE_B_ELEM> [(match_operand:MVE_2 1 "s_register_operand" "w")
			      (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VSTRBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vpst\;vstrbt.<V_sz_elem>\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrbq_gather_offset_z_s vldrbq_gather_offset_z_u]
;;
(define_insn "mve_vldrbq_gather_offset_z_<supf><mode>"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=&w")
	(unspec:MVE_2 [(match_operand:<MVE_B_ELEM> 1 "memory_operand" "Us")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRBGOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   if (!strcmp ("<supf>","s") && <V_sz_elem> == 8)
     output_asm_insn ("vpst\n\tvldrbt.u8\t%q0, [%m1, %q2]",ops);
   else
     output_asm_insn ("vpst\n\tvldrbt.<supf><V_sz_elem>\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrbq_z_s vldrbq_z_u]
;;
(define_insn "mve_vldrbq_z_<supf><mode>"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:<MVE_B_ELEM> 1 "mve_memory_operand" "Ux")
		       (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VLDRBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   if (<V_sz_elem> == 8)
     output_asm_insn ("vpst\;vldrbt.<V_sz_elem>\t%q0, %E1",ops);
   else
     output_asm_insn ("vpst\;vldrbt.<supf><V_sz_elem>\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_gather_base_z_s vldrwq_gather_base_z_u]
;;
(define_insn "mve_vldrwq_gather_base_z_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRWGBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vpst\n\tvldrwt.u32\t%q0, [%q1, %2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrhq_f]
;;
(define_insn "mve_vldrhq_fv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HI 1 "mve_memory_operand" "Ux")]
	 VLDRHQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   output_asm_insn ("vldrh.16\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrhq_gather_offset_s vldrhq_gather_offset_u]
;;
(define_insn "mve_vldrhq_gather_offset_<supf><mode>"
  [(set (match_operand:MVE_6 0 "s_register_operand" "=&w")
	(unspec:MVE_6 [(match_operand:<MVE_H_ELEM> 1 "memory_operand" "Us")
		       (match_operand:MVE_6 2 "s_register_operand" "w")]
	VLDRHGOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   if (!strcmp ("<supf>","s") && <V_sz_elem> == 16)
     output_asm_insn ("vldrh.u16\t%q0, [%m1, %q2]",ops);
   else
     output_asm_insn ("vldrh.<supf><V_sz_elem>\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrhq_gather_offset_z_s vldrhq_gather_offset_z_u]
;;
(define_insn "mve_vldrhq_gather_offset_z_<supf><mode>"
  [(set (match_operand:MVE_6 0 "s_register_operand" "=&w")
	(unspec:MVE_6 [(match_operand:<MVE_H_ELEM> 1 "memory_operand" "Us")
		       (match_operand:MVE_6 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")
	]VLDRHGOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   if (!strcmp ("<supf>","s") && <V_sz_elem> == 16)
     output_asm_insn ("vpst\n\tvldrht.u16\t%q0, [%m1, %q2]",ops);
   else
     output_asm_insn ("vpst\n\tvldrht.<supf><V_sz_elem>\t%q0, [%m1, %q2]",ops);
   return "";
}
 [(set_attr "length" "8")])

;;
;; [vldrhq_gather_shifted_offset_s vldrhq_gather_shifted_offset_u]
;;
(define_insn "mve_vldrhq_gather_shifted_offset_<supf><mode>"
  [(set (match_operand:MVE_6 0 "s_register_operand" "=&w")
	(unspec:MVE_6 [(match_operand:<MVE_H_ELEM> 1 "memory_operand" "Us")
		       (match_operand:MVE_6 2 "s_register_operand" "w")]
	VLDRHGSOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
      if (!strcmp ("<supf>","s") && <V_sz_elem> == 16)
     output_asm_insn ("vldrh.u16\t%q0, [%m1, %q2, uxtw #1]",ops);
   else
     output_asm_insn ("vldrh.<supf><V_sz_elem>\t%q0, [%m1, %q2, uxtw #1]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrhq_gather_shifted_offset_z_s vldrhq_gather_shited_offset_z_u]
;;
(define_insn "mve_vldrhq_gather_shifted_offset_z_<supf><mode>"
  [(set (match_operand:MVE_6 0 "s_register_operand" "=&w")
	(unspec:MVE_6 [(match_operand:<MVE_H_ELEM> 1 "memory_operand" "Us")
		       (match_operand:MVE_6 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")
	]VLDRHGSOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   if (!strcmp ("<supf>","s") && <V_sz_elem> == 16)
     output_asm_insn ("vpst\n\tvldrht.u16\t%q0, [%m1, %q2, uxtw #1]",ops);
   else
     output_asm_insn ("vpst\n\tvldrht.<supf><V_sz_elem>\t%q0, [%m1, %q2, uxtw #1]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrhq_s, vldrhq_u]
;;
(define_insn "mve_vldrhq_<supf><mode>"
  [(set (match_operand:MVE_6 0 "s_register_operand" "=w")
	(unspec:MVE_6 [(match_operand:<MVE_H_ELEM> 1 "mve_memory_operand" "Ux")]
	 VLDRHQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   if (<V_sz_elem> == 16)
     output_asm_insn ("vldrh.16\t%q0, %E1",ops);
   else
     output_asm_insn ("vldrh.<supf><V_sz_elem>\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrhq_z_f]
;;
(define_insn "mve_vldrhq_z_fv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HI 1 "mve_memory_operand" "Ux")
	(match_operand:HI 2 "vpr_register_operand" "Up")]
	 VLDRHQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   output_asm_insn ("vpst\;vldrht.16\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrhq_z_s vldrhq_z_u]
;;
(define_insn "mve_vldrhq_z_<supf><mode>"
  [(set (match_operand:MVE_6 0 "s_register_operand" "=w")
	(unspec:MVE_6 [(match_operand:<MVE_H_ELEM> 1 "mve_memory_operand" "Ux")
	(match_operand:HI 2 "vpr_register_operand" "Up")]
	 VLDRHQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   if (<V_sz_elem> == 16)
     output_asm_insn ("vpst\;vldrht.16\t%q0, %E1",ops);
   else
     output_asm_insn ("vpst\;vldrht.<supf><V_sz_elem>\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_f]
;;
(define_insn "mve_vldrwq_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V4SI 1 "mve_memory_operand" "Ux")]
	 VLDRWQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   output_asm_insn ("vldrw.32\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_s vldrwq_u]
;;
(define_insn "mve_vldrwq_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "mve_memory_operand" "Ux")]
	 VLDRWQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   output_asm_insn ("vldrw.32\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_z_f]
;;
(define_insn "mve_vldrwq_z_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V4SI 1 "mve_memory_operand" "Ux")
	(match_operand:HI 2 "vpr_register_operand" "Up")]
	 VLDRWQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   output_asm_insn ("vpst\;vldrwt.32\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_z_s vldrwq_z_u]
;;
(define_insn "mve_vldrwq_z_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "mve_memory_operand" "Ux")
	(match_operand:HI 2 "vpr_register_operand" "Up")]
	 VLDRWQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1]  = operands[1];
   output_asm_insn ("vpst\;vldrwt.32\t%q0, %E1",ops);
   return "";
}
  [(set_attr "length" "8")])

(define_expand "mve_vld1q_f<mode>"
  [(match_operand:MVE_0 0 "s_register_operand")
   (unspec:MVE_0 [(match_operand:<MVE_CNVT> 1 "mve_memory_operand")] VLD1Q_F)
  ]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
{
  emit_insn (gen_mve_vldr<V_sz_elem1>q_f<mode>(operands[0],operands[1]));
  DONE;
})

(define_expand "mve_vld1q_<supf><mode>"
  [(match_operand:MVE_2 0 "s_register_operand")
   (unspec:MVE_2 [(match_operand:MVE_2 1 "mve_memory_operand")] VLD1Q)
  ]
  "TARGET_HAVE_MVE"
{
  emit_insn (gen_mve_vldr<V_sz_elem1>q_<supf><mode>(operands[0],operands[1]));
  DONE;
})

;;
;; [vldrdq_gather_base_s vldrdq_gather_base_u]
;;
(define_insn "mve_vldrdq_gather_base_<supf>v2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
	(unspec:V2DI [(match_operand:V2DI 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")]
	 VLDRDGBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrd.64\t%q0, [%q1, %2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrdq_gather_base_z_s vldrdq_gather_base_z_u]
;;
(define_insn "mve_vldrdq_gather_base_z_<supf>v2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
	(unspec:V2DI [(match_operand:V2DI 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRDGBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vpst\n\tvldrdt.u64\t%q0, [%q1, %2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrdq_gather_offset_s vldrdq_gather_offset_u]
;;
(define_insn "mve_vldrdq_gather_offset_<supf>v2di"
 [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
       (unspec:V2DI [(match_operand:V2DI 1 "memory_operand" "Us")
		     (match_operand:V2DI 2 "s_register_operand" "w")]
	VLDRDGOQ))
 ]
 "TARGET_HAVE_MVE"
{
  rtx ops[3];
  ops[0] = operands[0];
  ops[1] = operands[1];
  ops[2] = operands[2];
  output_asm_insn ("vldrd.u64\t%q0, [%m1, %q2]",ops);
  return "";
}
 [(set_attr "length" "4")])

;;
;; [vldrdq_gather_offset_z_s vldrdq_gather_offset_z_u]
;;
(define_insn "mve_vldrdq_gather_offset_z_<supf>v2di"
 [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
       (unspec:V2DI [(match_operand:V2DI 1 "memory_operand" "Us")
		     (match_operand:V2DI 2 "s_register_operand" "w")
		     (match_operand:HI 3 "vpr_register_operand" "Up")]
	VLDRDGOQ))
 ]
 "TARGET_HAVE_MVE"
{
  rtx ops[3];
  ops[0] = operands[0];
  ops[1] = operands[1];
  ops[2] = operands[2];
  output_asm_insn ("vpst\n\tvldrdt.u64\t%q0, [%m1, %q2]",ops);
  return "";
}
 [(set_attr "length" "8")])

;;
;; [vldrdq_gather_shifted_offset_s vldrdq_gather_shifted_offset_u]
;;
(define_insn "mve_vldrdq_gather_shifted_offset_<supf>v2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
	(unspec:V2DI [(match_operand:V2DI 1 "memory_operand" "Us")
		      (match_operand:V2DI 2 "s_register_operand" "w")]
	 VLDRDGSOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrd.u64\t%q0, [%m1, %q2, uxtw #3]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrdq_gather_shifted_offset_z_s vldrdq_gather_shifted_offset_z_u]
;;
(define_insn "mve_vldrdq_gather_shifted_offset_z_<supf>v2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
	(unspec:V2DI [(match_operand:V2DI 1 "memory_operand" "Us")
		      (match_operand:V2DI 2 "s_register_operand" "w")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRDGSOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vpst\n\tvldrdt.u64\t%q0, [%m1, %q2, uxtw #3]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrhq_gather_offset_f]
;;
(define_insn "mve_vldrhq_gather_offset_fv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand" "=&w")
	(unspec:V8HF [(match_operand:V8HI 1 "memory_operand" "Us")
		      (match_operand:V8HI 2 "s_register_operand" "w")]
	 VLDRHQGO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrh.f16\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrhq_gather_offset_z_f]
;;
(define_insn "mve_vldrhq_gather_offset_z_fv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand" "=&w")
	(unspec:V8HF [(match_operand:V8HI 1 "memory_operand" "Us")
		      (match_operand:V8HI 2 "s_register_operand" "w")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRHQGO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   output_asm_insn ("vpst\n\tvldrht.f16\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrhq_gather_shifted_offset_f]
;;
(define_insn "mve_vldrhq_gather_shifted_offset_fv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand" "=&w")
	(unspec:V8HF [(match_operand:V8HI 1 "memory_operand" "Us")
		      (match_operand:V8HI 2 "s_register_operand" "w")]
	 VLDRHQGSO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrh.f16\t%q0, [%m1, %q2, uxtw #1]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrhq_gather_shifted_offset_z_f]
;;
(define_insn "mve_vldrhq_gather_shifted_offset_z_fv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand" "=&w")
	(unspec:V8HF [(match_operand:V8HI 1 "memory_operand" "Us")
		      (match_operand:V8HI 2 "s_register_operand" "w")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRHQGSO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   output_asm_insn ("vpst\n\tvldrht.f16\t%q0, [%m1, %q2, uxtw #1]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_gather_base_f]
;;
(define_insn "mve_vldrwq_gather_base_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")]
	 VLDRWQGB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrw.u32\t%q0, [%q1, %2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_gather_base_z_f]
;;
(define_insn "mve_vldrwq_gather_base_z_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:SI 2 "immediate_operand" "i")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRWQGB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vpst\n\tvldrwt.u32\t%q0, [%q1, %2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_gather_offset_f]
;;
(define_insn "mve_vldrwq_gather_offset_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 1 "memory_operand" "Us")
		       (match_operand:V4SI 2 "s_register_operand" "w")]
	 VLDRWQGO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrw.u32\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_gather_offset_s vldrwq_gather_offset_u]
;;
(define_insn "mve_vldrwq_gather_offset_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 1 "memory_operand" "Us")
		       (match_operand:V4SI 2 "s_register_operand" "w")]
	 VLDRWGOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrw.u32\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_gather_offset_z_f]
;;
(define_insn "mve_vldrwq_gather_offset_z_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 1 "memory_operand" "Us")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRWQGO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   output_asm_insn ("vpst\n\tvldrwt.u32\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_gather_offset_z_s vldrwq_gather_offset_z_u]
;;
(define_insn "mve_vldrwq_gather_offset_z_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 1 "memory_operand" "Us")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRWGOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   output_asm_insn ("vpst\n\tvldrwt.u32\t%q0, [%m1, %q2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_gather_shifted_offset_f]
;;
(define_insn "mve_vldrwq_gather_shifted_offset_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 1 "memory_operand" "Us")
		      (match_operand:V4SI 2 "s_register_operand" "w")]
	 VLDRWQGSO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrw.u32\t%q0, [%m1, %q2, uxtw #2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_gather_shifted_offset_s vldrwq_gather_shifted_offset_u]
;;
(define_insn "mve_vldrwq_gather_shifted_offset_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 1 "memory_operand" "Us")
		      (match_operand:V4SI 2 "s_register_operand" "w")]
	 VLDRWGSOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vldrw.u32\t%q0, [%m1, %q2, uxtw #2]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vldrwq_gather_shifted_offset_z_f]
;;
(define_insn "mve_vldrwq_gather_shifted_offset_z_fv4sf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 1 "memory_operand" "Us")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRWQGSO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   output_asm_insn ("vpst\n\tvldrwt.u32\t%q0, [%m1, %q2, uxtw #2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vldrwq_gather_shifted_offset_z_s vldrwq_gather_shifted_offset_z_u]
;;
(define_insn "mve_vldrwq_gather_shifted_offset_z_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 1 "memory_operand" "Us")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VLDRWGSOQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[4];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   ops[3] = operands[3];
   output_asm_insn ("vpst\n\tvldrwt.u32\t%q0, [%m1, %q2, uxtw #2]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrhq_f]
;;
(define_insn "mve_vstrhq_fv8hf"
  [(set (match_operand:V8HI 0 "mve_memory_operand" "=Ux")
	(unspec:V8HI [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VSTRHQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vstrh.16\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrhq_p_f]
;;
(define_insn "mve_vstrhq_p_fv8hf"
  [(set (match_operand:V8HI 0 "mve_memory_operand" "=Ux")
	(unspec:V8HI [(match_operand:V8HF 1 "s_register_operand" "w")
		      (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VSTRHQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vpst\;vstrht.16\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrhq_p_s vstrhq_p_u]
;;
(define_insn "mve_vstrhq_p_<supf><mode>"
  [(set (match_operand:<MVE_H_ELEM> 0 "mve_memory_operand" "=Ux")
	(unspec:<MVE_H_ELEM> [(match_operand:MVE_6 1 "s_register_operand" "w")
			      (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VSTRHQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vpst\;vstrht.<V_sz_elem>\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrhq_scatter_offset_p_s vstrhq_scatter_offset_p_u]
;;
(define_expand "mve_vstrhq_scatter_offset_p_<supf><mode>"
  [(match_operand:<MVE_H_ELEM> 0 "mve_scatter_memory")
   (match_operand:MVE_6 1 "s_register_operand")
   (match_operand:MVE_6 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRHSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrhq_scatter_offset_p_<supf><mode>_insn (ind, operands[1],
						       operands[2],
						       operands[3]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_offset_p_<supf><mode>_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:MVE_6 1 "s_register_operand" "w")
	   (match_operand:MVE_6 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRHSOQ))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrht.<V_sz_elem>\t%q2, [%0, %q1]"
  [(set_attr "length" "8")])

;;
;; [vstrhq_scatter_offset_s vstrhq_scatter_offset_u]
;;
(define_expand "mve_vstrhq_scatter_offset_<supf><mode>"
  [(match_operand:<MVE_H_ELEM> 0 "mve_scatter_memory")
   (match_operand:MVE_6 1 "s_register_operand")
   (match_operand:MVE_6 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRHSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrhq_scatter_offset_<supf><mode>_insn (ind, operands[1],
							      operands[2]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_offset_<supf><mode>_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:MVE_6 1 "s_register_operand" "w")
	   (match_operand:MVE_6 2 "s_register_operand" "w")]
	  VSTRHSOQ))]
  "TARGET_HAVE_MVE"
  "vstrh.<V_sz_elem>\t%q2, [%0, %q1]"
  [(set_attr "length" "4")])

;;
;; [vstrhq_scatter_shifted_offset_p_s vstrhq_scatter_shifted_offset_p_u]
;;
(define_expand "mve_vstrhq_scatter_shifted_offset_p_<supf><mode>"
  [(match_operand:<MVE_H_ELEM> 0 "mve_scatter_memory")
   (match_operand:MVE_6 1 "s_register_operand")
   (match_operand:MVE_6 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRHSSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrhq_scatter_shifted_offset_p_<supf><mode>_insn (ind, operands[1],
							       operands[2],
							       operands[3]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_shifted_offset_p_<supf><mode>_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:MVE_6 1 "s_register_operand" "w")
	   (match_operand:MVE_6 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRHSSOQ))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrht.<V_sz_elem>\t%q2, [%0, %q1, uxtw #1]"
  [(set_attr "length" "8")])

;;
;; [vstrhq_scatter_shifted_offset_s vstrhq_scatter_shifted_offset_u]
;;
(define_expand "mve_vstrhq_scatter_shifted_offset_<supf><mode>"
  [(match_operand:<MVE_H_ELEM> 0 "mve_scatter_memory")
   (match_operand:MVE_6 1 "s_register_operand")
   (match_operand:MVE_6 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRHSSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrhq_scatter_shifted_offset_<supf><mode>_insn (ind, operands[1],
							     operands[2]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_shifted_offset_<supf><mode>_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:MVE_6 1 "s_register_operand" "w")
	   (match_operand:MVE_6 2 "s_register_operand" "w")]
	  VSTRHSSOQ))]
  "TARGET_HAVE_MVE"
  "vstrh.<V_sz_elem>\t%q2, [%0, %q1, uxtw #1]"
  [(set_attr "length" "4")])

;;
;; [vstrhq_s, vstrhq_u]
;;
(define_insn "mve_vstrhq_<supf><mode>"
  [(set (match_operand:<MVE_H_ELEM> 0 "mve_memory_operand" "=Ux")
	(unspec:<MVE_H_ELEM> [(match_operand:MVE_6 1 "s_register_operand" "w")]
	 VSTRHQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vstrh.<V_sz_elem>\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrwq_f]
;;
(define_insn "mve_vstrwq_fv4sf"
  [(set (match_operand:V4SI 0 "mve_memory_operand" "=Ux")
	(unspec:V4SI [(match_operand:V4SF 1 "s_register_operand" "w")]
	 VSTRWQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vstrw.32\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrwq_p_f]
;;
(define_insn "mve_vstrwq_p_fv4sf"
  [(set (match_operand:V4SI 0 "mve_memory_operand" "=Ux")
	(unspec:V4SI [(match_operand:V4SF 1 "s_register_operand" "w")
		      (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VSTRWQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vpst\;vstrwt.32\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrwq_p_s vstrwq_p_u]
;;
(define_insn "mve_vstrwq_p_<supf>v4si"
  [(set (match_operand:V4SI 0 "mve_memory_operand" "=Ux")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VSTRWQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vpst\;vstrwt.32\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrwq_s vstrwq_u]
;;
(define_insn "mve_vstrwq_<supf>v4si"
  [(set (match_operand:V4SI 0 "mve_memory_operand" "=Ux")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")]
	 VSTRWQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[2];
   int regno = REGNO (operands[1]);
   ops[1] = gen_rtx_REG (TImode, regno);
   ops[0]  = operands[0];
   output_asm_insn ("vstrw.32\t%q1, %E0",ops);
   return "";
}
  [(set_attr "length" "4")])

(define_expand "mve_vst1q_f<mode>"
  [(match_operand:<MVE_CNVT> 0 "mve_memory_operand")
   (unspec:<MVE_CNVT> [(match_operand:MVE_0 1 "s_register_operand")] VST1Q_F)
  ]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
{
  emit_insn (gen_mve_vstr<V_sz_elem1>q_f<mode>(operands[0],operands[1]));
  DONE;
})

(define_expand "mve_vst1q_<supf><mode>"
  [(match_operand:MVE_2 0 "mve_memory_operand")
   (unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand")] VST1Q)
  ]
  "TARGET_HAVE_MVE"
{
  emit_insn (gen_mve_vstr<V_sz_elem1>q_<supf><mode>(operands[0],operands[1]));
  DONE;
})

;;
;; [vstrdq_scatter_base_p_s vstrdq_scatter_base_p_u]
;;
(define_insn "mve_vstrdq_scatter_base_p_<supf>v2di"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V2DI 0 "s_register_operand" "w")
		 (match_operand:SI 1 "mve_vldrd_immediate" "Ri")
		 (match_operand:V2DI 2 "s_register_operand" "w")
		 (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VSTRDSBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vpst\;\tvstrdt.u64\t%q2, [%q0, %1]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrdq_scatter_base_s vstrdq_scatter_base_u]
;;
(define_insn "mve_vstrdq_scatter_base_<supf>v2di"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V2DI 0 "s_register_operand" "=w")
		 (match_operand:SI 1 "mve_vldrd_immediate" "Ri")
		 (match_operand:V2DI 2 "s_register_operand" "w")]
	 VSTRDSBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vstrd.u64\t%q2, [%q0, %1]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrdq_scatter_offset_p_s vstrdq_scatter_offset_p_u]
;;
(define_expand "mve_vstrdq_scatter_offset_p_<supf>v2di"
  [(match_operand:V2DI 0 "mve_scatter_memory")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:V2DI 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRDSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrdq_scatter_offset_p_<supf>v2di_insn (ind, operands[1],
							      operands[2],
							      operands[3]));
  DONE;
})

(define_insn "mve_vstrdq_scatter_offset_p_<supf>v2di_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V2DI 1 "s_register_operand" "w")
	   (match_operand:V2DI 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRDSOQ))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrdt.64\t%q2, [%0, %q1]"
  [(set_attr "length" "8")])

;;
;; [vstrdq_scatter_offset_s vstrdq_scatter_offset_u]
;;
(define_expand "mve_vstrdq_scatter_offset_<supf>v2di"
  [(match_operand:V2DI 0 "mve_scatter_memory")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:V2DI 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRDSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrdq_scatter_offset_<supf>v2di_insn (ind, operands[1],
							    operands[2]));
  DONE;
})

(define_insn "mve_vstrdq_scatter_offset_<supf>v2di_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V2DI 1 "s_register_operand" "w")
	   (match_operand:V2DI 2 "s_register_operand" "w")]
	  VSTRDSOQ))]
  "TARGET_HAVE_MVE"
  "vstrd.64\t%q2, [%0, %q1]"
  [(set_attr "length" "4")])

;;
;; [vstrdq_scatter_shifted_offset_p_s vstrdq_scatter_shifted_offset_p_u]
;;
(define_expand "mve_vstrdq_scatter_shifted_offset_p_<supf>v2di"
  [(match_operand:V2DI 0 "mve_scatter_memory")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:V2DI 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRDSSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrdq_scatter_shifted_offset_p_<supf>v2di_insn (ind, operands[1],
							     operands[2],
							     operands[3]));
  DONE;
})

(define_insn "mve_vstrdq_scatter_shifted_offset_p_<supf>v2di_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V2DI 1 "s_register_operand" "w")
	   (match_operand:V2DI 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRDSSOQ))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrdt.64\t%q2, [%0, %q1, UXTW #3]"
  [(set_attr "length" "8")])

;;
;; [vstrdq_scatter_shifted_offset_s vstrdq_scatter_shifted_offset_u]
;;
(define_expand "mve_vstrdq_scatter_shifted_offset_<supf>v2di"
  [(match_operand:V2DI 0 "mve_scatter_memory")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:V2DI 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRDSSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrdq_scatter_shifted_offset_<supf>v2di_insn (ind, operands[1],
							   operands[2]));
  DONE;
})

(define_insn "mve_vstrdq_scatter_shifted_offset_<supf>v2di_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V2DI 1 "s_register_operand" "w")
	   (match_operand:V2DI 2 "s_register_operand" "w")]
	  VSTRDSSOQ))]
  "TARGET_HAVE_MVE"
  "vstrd.64\t%q2, [%0, %q1, UXTW #3]"
  [(set_attr "length" "4")])

;;
;; [vstrhq_scatter_offset_f]
;;
(define_expand "mve_vstrhq_scatter_offset_fv8hf"
  [(match_operand:V8HI 0 "mve_scatter_memory")
   (match_operand:V8HI 1 "s_register_operand")
   (match_operand:V8HF 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRHQSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrhq_scatter_offset_fv8hf_insn (ind, operands[1],
						       operands[2]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_offset_fv8hf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V8HI 1 "s_register_operand" "w")
	   (match_operand:V8HF 2 "s_register_operand" "w")]
	  VSTRHQSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vstrh.16\t%q2, [%0, %q1]"
  [(set_attr "length" "4")])

;;
;; [vstrhq_scatter_offset_p_f]
;;
(define_expand "mve_vstrhq_scatter_offset_p_fv8hf"
  [(match_operand:V8HI 0 "mve_scatter_memory")
   (match_operand:V8HI 1 "s_register_operand")
   (match_operand:V8HF 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRHQSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrhq_scatter_offset_p_fv8hf_insn (ind, operands[1],
							 operands[2],
							 operands[3]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_offset_p_fv8hf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V8HI 1 "s_register_operand" "w")
	   (match_operand:V8HF 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRHQSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vstrht.16\t%q2, [%0, %q1]"
  [(set_attr "length" "8")])

;;
;; [vstrhq_scatter_shifted_offset_f]
;;
(define_expand "mve_vstrhq_scatter_shifted_offset_fv8hf"
  [(match_operand:V8HI 0 "memory_operand" "=Us")
   (match_operand:V8HI 1 "s_register_operand" "w")
   (match_operand:V8HF 2 "s_register_operand" "w")
   (unspec:V4SI [(const_int 0)] VSTRHQSSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrhq_scatter_shifted_offset_fv8hf_insn (ind, operands[1],
							       operands[2]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_shifted_offset_fv8hf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V8HI 1 "s_register_operand" "w")
	   (match_operand:V8HF 2 "s_register_operand" "w")]
	  VSTRHQSSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vstrh.16\t%q2, [%0, %q1, uxtw #1]"
  [(set_attr "length" "4")])

;;
;; [vstrhq_scatter_shifted_offset_p_f]
;;
(define_expand "mve_vstrhq_scatter_shifted_offset_p_fv8hf"
  [(match_operand:V8HI 0 "memory_operand" "=Us")
   (match_operand:V8HI 1 "s_register_operand" "w")
   (match_operand:V8HF 2 "s_register_operand" "w")
   (match_operand:HI 3 "vpr_register_operand" "Up")
   (unspec:V4SI [(const_int 0)] VSTRHQSSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrhq_scatter_shifted_offset_p_fv8hf_insn (ind, operands[1],
							operands[2],
							operands[3]));
  DONE;
})

(define_insn "mve_vstrhq_scatter_shifted_offset_p_fv8hf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V8HI 1 "s_register_operand" "w")
	   (match_operand:V8HF 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRHQSSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vstrht.16\t%q2, [%0, %q1, uxtw #1]"
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_base_f]
;;
(define_insn "mve_vstrwq_scatter_base_fv4sf"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V4SI 0 "s_register_operand" "w")
		 (match_operand:SI 1 "immediate_operand" "i")
		 (match_operand:V4SF 2 "s_register_operand" "w")]
	 VSTRWQSB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vstrw.u32\t%q2, [%q0, %1]",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrwq_scatter_base_p_f]
;;
(define_insn "mve_vstrwq_scatter_base_p_fv4sf"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V4SI 0 "s_register_operand" "w")
		 (match_operand:SI 1 "immediate_operand" "i")
		 (match_operand:V4SF 2 "s_register_operand" "w")
		 (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VSTRWQSB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[1];
   ops[2] = operands[2];
   output_asm_insn ("vpst\n\tvstrwt.u32\t%q2, [%q0, %1]",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_offset_f]
;;
(define_expand "mve_vstrwq_scatter_offset_fv4sf"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SF 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWQSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrwq_scatter_offset_fv4sf_insn (ind, operands[1],
						       operands[2]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_offset_fv4sf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SF 2 "s_register_operand" "w")]
	  VSTRWQSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vstrw.32\t%q2, [%0, %q1]"
  [(set_attr "length" "4")])

;;
;; [vstrwq_scatter_offset_p_f]
;;
(define_expand "mve_vstrwq_scatter_offset_p_fv4sf"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SF 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWQSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrwq_scatter_offset_p_fv4sf_insn (ind, operands[1],
							 operands[2],
							 operands[3]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_offset_p_fv4sf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SF 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRWQSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vstrwt.32\t%q2, [%0, %q1]"
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_offset_s vstrwq_scatter_offset_u]
;;
(define_expand "mve_vstrwq_scatter_offset_p_<supf>v4si"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SI 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrwq_scatter_offset_p_<supf>v4si_insn (ind, operands[1],
							      operands[2],
							      operands[3]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_offset_p_<supf>v4si_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SI 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRWSOQ))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrwt.32\t%q2, [%0, %q1]"
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_offset_s vstrwq_scatter_offset_u]
;;
(define_expand "mve_vstrwq_scatter_offset_<supf>v4si"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SI 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrwq_scatter_offset_<supf>v4si_insn (ind, operands[1],
							    operands[2]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_offset_<supf>v4si_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SI 2 "s_register_operand" "w")]
	  VSTRWSOQ))]
  "TARGET_HAVE_MVE"
  "vstrw.32\t%q2, [%0, %q1]"
  [(set_attr "length" "4")])

;;
;; [vstrwq_scatter_shifted_offset_f]
;;
(define_expand "mve_vstrwq_scatter_shifted_offset_fv4sf"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SF 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWQSSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (gen_mve_vstrwq_scatter_shifted_offset_fv4sf_insn (ind, operands[1],
							       operands[2]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_shifted_offset_fv4sf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SF 2 "s_register_operand" "w")]
	 VSTRWQSSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vstrw.32\t%q2, [%0, %q1, uxtw #2]"
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_shifted_offset_p_f]
;;
(define_expand "mve_vstrwq_scatter_shifted_offset_p_fv4sf"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SF 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWQSSO_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrwq_scatter_shifted_offset_p_fv4sf_insn (ind, operands[1],
							operands[2],
							operands[3]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_shifted_offset_p_fv4sf_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SF 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRWQSSO_F))]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vstrwt.32\t%q2, [%0, %q1, uxtw #2]"
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_shifted_offset_p_s vstrwq_scatter_shifted_offset_p_u]
;;
(define_expand "mve_vstrwq_scatter_shifted_offset_p_<supf>v4si"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SI 2 "s_register_operand")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWSSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrwq_scatter_shifted_offset_p_<supf>v4si_insn (ind, operands[1],
							     operands[2],
							     operands[3]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_shifted_offset_p_<supf>v4si_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SI 2 "s_register_operand" "w")
	   (match_operand:HI 3 "vpr_register_operand" "Up")]
	  VSTRWSSOQ))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrwt.32\t%q2, [%0, %q1, uxtw #2]"
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_shifted_offset_s vstrwq_scatter_shifted_offset_u]
;;
(define_expand "mve_vstrwq_scatter_shifted_offset_<supf>v4si"
  [(match_operand:V4SI 0 "mve_scatter_memory")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:V4SI 2 "s_register_operand")
   (unspec:V4SI [(const_int 0)] VSTRWSSOQ)]
  "TARGET_HAVE_MVE"
{
  rtx ind = XEXP (operands[0], 0);
  gcc_assert (REG_P (ind));
  emit_insn (
    gen_mve_vstrwq_scatter_shifted_offset_<supf>v4si_insn (ind, operands[1],
							   operands[2]));
  DONE;
})

(define_insn "mve_vstrwq_scatter_shifted_offset_<supf>v4si_insn"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (match_operand:V4SI 2 "s_register_operand" "w")]
	  VSTRWSSOQ))]
  "TARGET_HAVE_MVE"
  "vstrw.32\t%q2, [%0, %q1, uxtw #2]"
  [(set_attr "length" "4")])

;;
;; [vaddq_s, vaddq_u])
;;
(define_insn "mve_vaddq<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(plus:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vadd.i%#<V_sz_elem>  %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddq_f])
;;
(define_insn "mve_vaddq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(plus:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vadd.f%#<V_sz_elem> %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vidupq_n_u])
;;
(define_expand "mve_vidupq_n_u<mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:SI 2 "mve_imm_selective_upto_8")]
 "TARGET_HAVE_MVE"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_move_insn (temp, operands[1]);
  rtx inc = gen_int_mode (INTVAL(operands[2]) * <MVE_LANES>, SImode);
  emit_insn (gen_mve_vidupq_u<mode>_insn (operands[0], temp, operands[1],
					  operands[2], inc));
  DONE;
})

;;
;; [vidupq_u_insn])
;;
(define_insn "mve_vidupq_u<mode>_insn"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:SI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_imm_selective_upto_8" "Rg")]
	 VIDUPQ))
  (set (match_operand:SI 1 "s_register_operand" "=Te")
       (plus:SI (match_dup 2)
		(match_operand:SI 4 "immediate_operand" "i")))]
 "TARGET_HAVE_MVE"
 "vidup.u%#<V_sz_elem>\t%q0, %1, %3")

;;
;; [vidupq_m_n_u])
;;
(define_expand "mve_vidupq_m_n_u<mode>"
  [(match_operand:MVE_2 0 "s_register_operand")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:SI 2 "s_register_operand")
   (match_operand:SI 3 "mve_imm_selective_upto_8")
   (match_operand:HI 4 "vpr_register_operand")]
  "TARGET_HAVE_MVE"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_move_insn (temp, operands[2]);
  rtx inc = gen_int_mode (INTVAL(operands[3]) * <MVE_LANES>, SImode);
  emit_insn (gen_mve_vidupq_m_wb_u<mode>_insn(operands[0], operands[1], temp,
					     operands[2], operands[3],
					     operands[4], inc));
  DONE;
})

;;
;; [vidupq_m_wb_u_insn])
;;
(define_insn "mve_vidupq_m_wb_u<mode>_insn"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		      (match_operand:SI 3 "s_register_operand" "2")
		      (match_operand:SI 4 "mve_imm_selective_upto_8" "Rg")
		      (match_operand:HI 5 "vpr_register_operand" "Up")]
	VIDUPQ_M))
  (set (match_operand:SI 2 "s_register_operand" "=Te")
       (plus:SI (match_dup 3)
		(match_operand:SI 6 "immediate_operand" "i")))]
 "TARGET_HAVE_MVE"
 "vpst\;\tvidupt.u%#<V_sz_elem>\t%q0, %2, %4"
 [(set_attr "length""8")])

;;
;; [vddupq_n_u])
;;
(define_expand "mve_vddupq_n_u<mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:SI 2 "mve_imm_selective_upto_8")]
 "TARGET_HAVE_MVE"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_move_insn (temp, operands[1]);
  rtx inc = gen_int_mode (INTVAL(operands[2]) * <MVE_LANES>, SImode);
  emit_insn (gen_mve_vddupq_u<mode>_insn (operands[0], temp, operands[1],
					  operands[2], inc));
  DONE;
})

;;
;; [vddupq_u_insn])
;;
(define_insn "mve_vddupq_u<mode>_insn"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:SI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "immediate_operand" "i")]
	VDDUPQ))
  (set (match_operand:SI 1 "s_register_operand" "=Te")
       (minus:SI (match_dup 2)
		 (match_operand:SI 4 "immediate_operand" "i")))]
 "TARGET_HAVE_MVE"
 "vddup.u%#<V_sz_elem>  %q0, %1, %3")

;;
;; [vddupq_m_n_u])
;;
(define_expand "mve_vddupq_m_n_u<mode>"
  [(match_operand:MVE_2 0 "s_register_operand")
   (match_operand:MVE_2 1 "s_register_operand")
   (match_operand:SI 2 "s_register_operand")
   (match_operand:SI 3 "mve_imm_selective_upto_8")
   (match_operand:HI 4 "vpr_register_operand")]
  "TARGET_HAVE_MVE"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_move_insn (temp, operands[2]);
  rtx inc = gen_int_mode (INTVAL(operands[3]) * <MVE_LANES>, SImode);
  emit_insn (gen_mve_vddupq_m_wb_u<mode>_insn(operands[0], operands[1], temp,
					     operands[2], operands[3],
					     operands[4], inc));
  DONE;
})

;;
;; [vddupq_m_wb_u_insn])
;;
(define_insn "mve_vddupq_m_wb_u<mode>_insn"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		      (match_operand:SI 3 "s_register_operand" "2")
		      (match_operand:SI 4 "mve_imm_selective_upto_8" "Rg")
		      (match_operand:HI 5 "vpr_register_operand" "Up")]
	VDDUPQ_M))
  (set (match_operand:SI 2 "s_register_operand" "=Te")
       (minus:SI (match_dup 3)
		 (match_operand:SI 6 "immediate_operand" "i")))]
 "TARGET_HAVE_MVE"
 "vpst\;\tvddupt.u%#<V_sz_elem>\t%q0, %2, %4"
 [(set_attr "length""8")])

;;
;; [vdwdupq_n_u])
;;
(define_expand "mve_vdwdupq_n_u<mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:DI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_selective_upto_8")]
 "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (SImode);
  emit_insn (gen_mve_vdwdupq_wb_u<mode>_insn (operands[0], ignore_wb,
					      operands[1], operands[2],
					      operands[3]));
  DONE;
})

;;
;; [vdwdupq_wb_u])
;;
(define_expand "mve_vdwdupq_wb_u<mode>"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:DI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_selective_upto_8")
  (unspec:MVE_2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_mve_vdwdupq_wb_u<mode>_insn (ignore_vec, operands[0],
					      operands[1], operands[2],
					      operands[3]));
  DONE;
})

;;
;; [vdwdupq_wb_u_insn])
;;
(define_insn "mve_vdwdupq_wb_u<mode>_insn"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:SI 2 "s_register_operand" "1")
		       (subreg:SI (match_operand:DI 3 "s_register_operand" "r") 4)
		       (match_operand:SI 4 "mve_imm_selective_upto_8" "Rg")]
	 VDWDUPQ))
   (set (match_operand:SI 1 "s_register_operand" "=Te")
	(unspec:SI [(match_dup 2)
		    (subreg:SI (match_dup 3) 4)
		    (match_dup 4)]
	 VDWDUPQ))]
  "TARGET_HAVE_MVE"
  "vdwdup.u%#<V_sz_elem>\t%q0, %2, %R3, %4"
)

;;
;; [vdwdupq_m_n_u])
;;
(define_expand "mve_vdwdupq_m_n_u<mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:DI 3 "s_register_operand")
  (match_operand:SI 4 "mve_imm_selective_upto_8")
  (match_operand:HI 5 "vpr_register_operand")]
 "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (SImode);
  emit_insn (gen_mve_vdwdupq_m_wb_u<mode>_insn (operands[0], ignore_wb,
						operands[1], operands[2],
						operands[3], operands[4],
						operands[5]));
  DONE;
})

;;
;; [vdwdupq_m_wb_u])
;;
(define_expand "mve_vdwdupq_m_wb_u<mode>"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:DI 3 "s_register_operand")
  (match_operand:SI 4 "mve_imm_selective_upto_8")
  (match_operand:HI 5 "vpr_register_operand")]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_mve_vdwdupq_m_wb_u<mode>_insn (ignore_vec, operands[0],
						operands[1], operands[2],
						operands[3], operands[4],
						operands[5]));
  DONE;
})

;;
;; [vdwdupq_m_wb_u_insn])
;;
(define_insn "mve_vdwdupq_m_wb_u<mode>_insn"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 2 "s_register_operand" "0")
		       (match_operand:SI 3 "s_register_operand" "1")
		       (subreg:SI (match_operand:DI 4 "s_register_operand" "r") 4)
		       (match_operand:SI 5 "mve_imm_selective_upto_8" "Rg")
		       (match_operand:HI 6 "vpr_register_operand" "Up")]
	 VDWDUPQ_M))
   (set (match_operand:SI 1 "s_register_operand" "=Te")
	(unspec:SI [(match_dup 2)
		    (match_dup 3)
		    (subreg:SI (match_dup 4) 4)
		    (match_dup 5)
		    (match_dup 6)]
	 VDWDUPQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;\tvdwdupt.u%#<V_sz_elem>\t%q2, %3, %R4, %5"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [viwdupq_n_u])
;;
(define_expand "mve_viwdupq_n_u<mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:DI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_selective_upto_8")]
 "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (SImode);
  emit_insn (gen_mve_viwdupq_wb_u<mode>_insn (operands[0], ignore_wb,
					      operands[1], operands[2],
					      operands[3]));
  DONE;
})

;;
;; [viwdupq_wb_u])
;;
(define_expand "mve_viwdupq_wb_u<mode>"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:DI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_selective_upto_8")
  (unspec:MVE_2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_mve_viwdupq_wb_u<mode>_insn (ignore_vec, operands[0],
					      operands[1], operands[2],
					      operands[3]));
  DONE;
})

;;
;; [viwdupq_wb_u_insn])
;;
(define_insn "mve_viwdupq_wb_u<mode>_insn"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:SI 2 "s_register_operand" "1")
		       (subreg:SI (match_operand:DI 3 "s_register_operand" "r") 4)
		       (match_operand:SI 4 "mve_imm_selective_upto_8" "Rg")]
	 VIWDUPQ))
   (set (match_operand:SI 1 "s_register_operand" "=Te")
	(unspec:SI [(match_dup 2)
		    (subreg:SI (match_dup 3) 4)
		    (match_dup 4)]
	 VIWDUPQ))]
  "TARGET_HAVE_MVE"
  "viwdup.u%#<V_sz_elem>\t%q0, %2, %R3, %4"
)

;;
;; [viwdupq_m_n_u])
;;
(define_expand "mve_viwdupq_m_n_u<mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:DI 3 "s_register_operand")
  (match_operand:SI 4 "mve_imm_selective_upto_8")
  (match_operand:HI 5 "vpr_register_operand")]
 "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (SImode);
  emit_insn (gen_mve_viwdupq_m_wb_u<mode>_insn (operands[0], ignore_wb,
						operands[1], operands[2],
						operands[3], operands[4],
						operands[5]));
  DONE;
})

;;
;; [viwdupq_m_wb_u])
;;
(define_expand "mve_viwdupq_m_wb_u<mode>"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:DI 3 "s_register_operand")
  (match_operand:SI 4 "mve_imm_selective_upto_8")
  (match_operand:HI 5 "vpr_register_operand")]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_mve_viwdupq_m_wb_u<mode>_insn (ignore_vec, operands[0],
						operands[1], operands[2],
						operands[3], operands[4],
						operands[5]));
  DONE;
})

;;
;; [viwdupq_m_wb_u_insn])
;;
(define_insn "mve_viwdupq_m_wb_u<mode>_insn"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 2 "s_register_operand" "0")
		       (match_operand:SI 3 "s_register_operand" "1")
		       (subreg:SI (match_operand:DI 4 "s_register_operand" "r") 4)
		       (match_operand:SI 5 "mve_imm_selective_upto_8" "Rg")
		       (match_operand:HI 6 "vpr_register_operand" "Up")]
	 VIWDUPQ_M))
   (set (match_operand:SI 1 "s_register_operand" "=Te")
	(unspec:SI [(match_dup 2)
		    (match_dup 3)
		    (subreg:SI (match_dup 4) 4)
		    (match_dup 5)
		    (match_dup 6)]
	 VIWDUPQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;\tviwdupt.u%#<V_sz_elem>\t%q2, %3, %R4, %5"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vstrwq_scatter_base_wb_s vstrwq_scatter_base_wb_u]
;;
(define_insn "mve_vstrwq_scatter_base_wb_<supf>v4si"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V4SI 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:V4SI 3 "s_register_operand" "w")]
	 VSTRWSBWBQ))
   (set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_dup 1) (match_dup 2)]
	 VSTRWSBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[1];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vstrw.u32\t%q2, [%q0, %1]!",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrwq_scatter_base_wb_p_s vstrwq_scatter_base_wb_p_u]
;;
(define_insn "mve_vstrwq_scatter_base_wb_p_<supf>v4si"
 [(set (mem:BLK (scratch))
       (unspec:BLK
		[(match_operand:V4SI 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:V4SI 3 "s_register_operand" "w")
		 (match_operand:HI 4 "vpr_register_operand")]
	VSTRWSBWBQ))
   (set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_dup 1) (match_dup 2)]
	 VSTRWSBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[1];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vpst\;\tvstrwt.u32\t%q2, [%q0, %1]!",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrwq_scatter_base_wb_f]
;;
(define_insn "mve_vstrwq_scatter_base_wb_fv4sf"
 [(set (mem:BLK (scratch))
       (unspec:BLK
		[(match_operand:V4SI 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:V4SF 3 "s_register_operand" "w")]
	 VSTRWQSBWB_F))
   (set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_dup 1) (match_dup 2)]
	 VSTRWQSBWB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[1];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vstrw.u32\t%q2, [%q0, %1]!",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrwq_scatter_base_wb_p_f]
;;
(define_insn "mve_vstrwq_scatter_base_wb_p_fv4sf"
 [(set (mem:BLK (scratch))
       (unspec:BLK
		[(match_operand:V4SI 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:V4SF 3 "s_register_operand" "w")
		 (match_operand:HI 4 "vpr_register_operand")]
	VSTRWQSBWB_F))
   (set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_dup 1) (match_dup 2)]
	 VSTRWQSBWB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[1];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vpst\;\tvstrwt.u32\t%q2, [%q0, %1]!",ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vstrdq_scatter_base_wb_s vstrdq_scatter_base_wb_u]
;;
(define_insn "mve_vstrdq_scatter_base_wb_<supf>v2di"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V2DI 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:V2DI 3 "s_register_operand" "w")]
	 VSTRDSBWBQ))
   (set (match_operand:V2DI 0 "s_register_operand" "=&w")
	(unspec:V2DI [(match_dup 1) (match_dup 2)]
	 VSTRDSBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[1];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vstrd.u64\t%q2, [%q0, %1]!",ops);
   return "";
}
  [(set_attr "length" "4")])

;;
;; [vstrdq_scatter_base_wb_p_s vstrdq_scatter_base_wb_p_u]
;;
(define_insn "mve_vstrdq_scatter_base_wb_p_<supf>v2di"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:V2DI 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:V2DI 3 "s_register_operand" "w")
		 (match_operand:HI 4 "vpr_register_operand")]
	 VSTRDSBWBQ))
   (set (match_operand:V2DI 0 "s_register_operand" "=w")
	(unspec:V2DI [(match_dup 1) (match_dup 2)]
	 VSTRDSBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[1];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vpst;vstrdt.u64\t%q2, [%q0, %1]!",ops);
   return "";
}
  [(set_attr "length" "8")])

(define_expand "mve_vldrwq_gather_base_wb_<supf>v4si"
  [(match_operand:V4SI 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (unspec:V4SI [(const_int 0)] VLDRWGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_result = gen_reg_rtx (V4SImode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_<supf>v4si_insn (ignore_result, operands[0],
						 operands[1], operands[2]));
  DONE;
})

(define_expand "mve_vldrwq_gather_base_nowb_<supf>v4si"
  [(match_operand:V4SI 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (unspec:V4SI [(const_int 0)] VLDRWGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (V4SImode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_<supf>v4si_insn (operands[0], ignore_wb,
						 operands[1], operands[2]));
  DONE;
})

;;
;; [vldrwq_gather_base_wb_s vldrwq_gather_base_wb_u]
;;
(define_insn "mve_vldrwq_gather_base_wb_<supf>v4si_insn"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		      (mem:BLK (scratch))]
	 VLDRWGBWBQ))
   (set (match_operand:V4SI 1 "s_register_operand" "=&w")
	(unspec:V4SI [(match_dup 2) (match_dup 3)]
	 VLDRWGBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vldrw.u32\t%q0, [%q1, %2]!",ops);
   return "";
}
  [(set_attr "length" "4")])

(define_expand "mve_vldrwq_gather_base_wb_z_<supf>v4si"
  [(match_operand:V4SI 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VLDRWGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_result = gen_reg_rtx (V4SImode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_z_<supf>v4si_insn (ignore_result, operands[0],
						   operands[1], operands[2],
						   operands[3]));
  DONE;
})
(define_expand "mve_vldrwq_gather_base_nowb_z_<supf>v4si"
  [(match_operand:V4SI 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VLDRWGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (V4SImode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_z_<supf>v4si_insn (operands[0], ignore_wb,
						   operands[1], operands[2],
						   operands[3]));
  DONE;
})

;;
;; [vldrwq_gather_base_wb_z_s vldrwq_gather_base_wb_z_u]
;;
(define_insn "mve_vldrwq_gather_base_wb_z_<supf>v4si_insn"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(unspec:V4SI [(match_operand:V4SI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		      (match_operand:HI 4 "vpr_register_operand" "Up")
		      (mem:BLK (scratch))]
	 VLDRWGBWBQ))
   (set (match_operand:V4SI 1 "s_register_operand" "=&w")
	(unspec:V4SI [(match_dup 2) (match_dup 3)]
	 VLDRWGBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vpst\;vldrwt.u32\t%q0, [%q1, %2]!",ops);
   return "";
}
  [(set_attr "length" "8")])

(define_expand "mve_vldrwq_gather_base_wb_fv4sf"
  [(match_operand:V4SI 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (unspec:V4SI [(const_int 0)] VLDRWQGBWB_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ignore_result = gen_reg_rtx (V4SFmode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_fv4sf_insn (ignore_result, operands[0],
					    operands[1], operands[2]));
  DONE;
})

(define_expand "mve_vldrwq_gather_base_nowb_fv4sf"
  [(match_operand:V4SF 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (unspec:V4SI [(const_int 0)] VLDRWQGBWB_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ignore_wb = gen_reg_rtx (V4SImode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_fv4sf_insn (operands[0], ignore_wb,
					    operands[1], operands[2]));
  DONE;
})

;;
;; [vldrwq_gather_base_wb_f]
;;
(define_insn "mve_vldrwq_gather_base_wb_fv4sf_insn"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		      (mem:BLK (scratch))]
	 VLDRWQGBWB_F))
   (set (match_operand:V4SI 1 "s_register_operand" "=&w")
	(unspec:V4SI [(match_dup 2) (match_dup 3)]
	 VLDRWQGBWB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vldrw.u32\t%q0, [%q1, %2]!",ops);
   return "";
}
  [(set_attr "length" "4")])

(define_expand "mve_vldrwq_gather_base_wb_z_fv4sf"
  [(match_operand:V4SI 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VLDRWQGBWB_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ignore_result = gen_reg_rtx (V4SFmode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_z_fv4sf_insn (ignore_result, operands[0],
					      operands[1], operands[2],
					      operands[3]));
  DONE;
})

(define_expand "mve_vldrwq_gather_base_nowb_z_fv4sf"
  [(match_operand:V4SF 0 "s_register_operand")
   (match_operand:V4SI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V4SI [(const_int 0)] VLDRWQGBWB_F)]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
  rtx ignore_wb = gen_reg_rtx (V4SImode);
  emit_insn (
  gen_mve_vldrwq_gather_base_wb_z_fv4sf_insn (operands[0], ignore_wb,
					      operands[1], operands[2],
					      operands[3]));
  DONE;
})

;;
;; [vldrwq_gather_base_wb_z_f]
;;
(define_insn "mve_vldrwq_gather_base_wb_z_fv4sf_insn"
  [(set (match_operand:V4SF 0 "s_register_operand" "=&w")
	(unspec:V4SF [(match_operand:V4SI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		      (match_operand:HI 4 "vpr_register_operand" "Up")
		      (mem:BLK (scratch))]
	 VLDRWQGBWB_F))
   (set (match_operand:V4SI 1 "s_register_operand" "=&w")
	(unspec:V4SI [(match_dup 2) (match_dup 3)]
	 VLDRWQGBWB_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vpst\;vldrwt.u32\t%q0, [%q1, %2]!",ops);
   return "";
}
  [(set_attr "length" "8")])

(define_expand "mve_vldrdq_gather_base_wb_<supf>v2di"
  [(match_operand:V2DI 0 "s_register_operand")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (unspec:V2DI [(const_int 0)] VLDRDGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_result = gen_reg_rtx (V2DImode);
  emit_insn (
  gen_mve_vldrdq_gather_base_wb_<supf>v2di_insn (ignore_result, operands[0],
						 operands[1], operands[2]));
  DONE;
})

(define_expand "mve_vldrdq_gather_base_nowb_<supf>v2di"
  [(match_operand:V2DI 0 "s_register_operand")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (unspec:V2DI [(const_int 0)] VLDRDGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (V2DImode);
  emit_insn (
  gen_mve_vldrdq_gather_base_wb_<supf>v2di_insn (operands[0], ignore_wb,
						 operands[1], operands[2]));
  DONE;
})


;;
;; [vldrdq_gather_base_wb_s vldrdq_gather_base_wb_u]
;;
(define_insn "mve_vldrdq_gather_base_wb_<supf>v2di_insn"
  [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
	(unspec:V2DI [(match_operand:V2DI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		      (mem:BLK (scratch))]
	 VLDRDGBWBQ))
   (set (match_operand:V2DI 1 "s_register_operand" "=&w")
	(unspec:V2DI [(match_dup 2) (match_dup 3)]
	 VLDRDGBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vldrd.64\t%q0, [%q1, %2]!",ops);
   return "";
}
  [(set_attr "length" "4")])

(define_expand "mve_vldrdq_gather_base_wb_z_<supf>v2di"
  [(match_operand:V2DI 0 "s_register_operand")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V2DI [(const_int 0)] VLDRDGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_result = gen_reg_rtx (V2DImode);
  emit_insn (
  gen_mve_vldrdq_gather_base_wb_z_<supf>v2di_insn (ignore_result, operands[0],
						   operands[1], operands[2],
						   operands[3]));
  DONE;
})

(define_expand "mve_vldrdq_gather_base_nowb_z_<supf>v2di"
  [(match_operand:V2DI 0 "s_register_operand")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:SI 2 "mve_vldrd_immediate")
   (match_operand:HI 3 "vpr_register_operand")
   (unspec:V2DI [(const_int 0)] VLDRDGBWBQ)]
  "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (V2DImode);
  emit_insn (
  gen_mve_vldrdq_gather_base_wb_z_<supf>v2di_insn (operands[0], ignore_wb,
						   operands[1], operands[2],
						   operands[3]));
  DONE;
})

(define_insn "get_fpscr_nzcvqc"
 [(set (match_operand:SI 0 "register_operand" "=r")
   (unspec:SI [(reg:SI VFPCC_REGNUM)] UNSPEC_GET_FPSCR_NZCVQC))]
 "TARGET_HAVE_MVE"
 "vmrs\\t%0, FPSCR_nzcvqc"
  [(set_attr "type" "mve_move")])

(define_insn "set_fpscr_nzcvqc"
 [(set (reg:SI VFPCC_REGNUM)
   (unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")]
    VUNSPEC_SET_FPSCR_NZCVQC))]
 "TARGET_HAVE_MVE"
 "vmsr\\tFPSCR_nzcvqc, %0"
  [(set_attr "type" "mve_move")])

;;
;; [vldrdq_gather_base_wb_z_s vldrdq_gather_base_wb_z_u]
;;
(define_insn "mve_vldrdq_gather_base_wb_z_<supf>v2di_insn"
  [(set (match_operand:V2DI 0 "s_register_operand" "=&w")
	(unspec:V2DI [(match_operand:V2DI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		      (match_operand:HI 4 "vpr_register_operand" "Up")
		      (mem:BLK (scratch))]
	 VLDRDGBWBQ))
   (set (match_operand:V2DI 1 "s_register_operand" "=&w")
	(unspec:V2DI [(match_dup 2) (match_dup 3)]
	 VLDRDGBWBQ))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[3];
   ops[0] = operands[0];
   ops[1] = operands[2];
   ops[2] = operands[3];
   output_asm_insn ("vpst\;vldrdt.u64\t%q0, [%q1, %2]!",ops);
   return "";
}
  [(set_attr "length" "8")])
;;
;; [vadciq_m_s, vadciq_m_u])
;;
(define_insn "mve_vadciq_m_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "0")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:V4SI 3 "s_register_operand" "w")
		      (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VADCIQ_M))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(const_int 0)]
	 VADCIQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vadcit.i32\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length" "8")])

;;
;; [vadciq_u, vadciq_s])
;;
(define_insn "mve_vadciq_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:V4SI 2 "s_register_operand" "w")]
	 VADCIQ))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(const_int 0)]
	 VADCIQ))
  ]
  "TARGET_HAVE_MVE"
  "vadci.i32\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length" "4")])

;;
;; [vadcq_m_s, vadcq_m_u])
;;
(define_insn "mve_vadcq_m_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "0")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:V4SI 3 "s_register_operand" "w")
		      (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VADCQ_M))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(reg:SI VFPCC_REGNUM)]
	 VADCQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vadct.i32\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length" "8")])

;;
;; [vadcq_u, vadcq_s])
;;
(define_insn "mve_vadcq_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		       (match_operand:V4SI 2 "s_register_operand" "w")]
	 VADCQ))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(reg:SI VFPCC_REGNUM)]
	 VADCQ))
  ]
  "TARGET_HAVE_MVE"
  "vadc.i32\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length" "4")
   (set_attr "conds" "set")])

;;
;; [vsbciq_m_u, vsbciq_m_s])
;;
(define_insn "mve_vsbciq_m_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:V4SI 3 "s_register_operand" "w")
		      (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSBCIQ_M))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(const_int 0)]
	 VSBCIQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vsbcit.i32\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length" "8")])

;;
;; [vsbciq_s, vsbciq_u])
;;
(define_insn "mve_vsbciq_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:V4SI 2 "s_register_operand" "w")]
	 VSBCIQ))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(const_int 0)]
	 VSBCIQ))
  ]
  "TARGET_HAVE_MVE"
  "vsbci.i32\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length" "4")])

;;
;; [vsbcq_m_u, vsbcq_m_s])
;;
(define_insn "mve_vsbcq_m_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:V4SI 3 "s_register_operand" "w")
		      (match_operand:HI 4 "vpr_register_operand" "Up")]
	 VSBCQ_M))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(reg:SI VFPCC_REGNUM)]
	 VSBCQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vsbct.i32\t%q0, %q2, %q3"
  [(set_attr "type" "mve_move")
   (set_attr "length" "8")])

;;
;; [vsbcq_s, vsbcq_u])
;;
(define_insn "mve_vsbcq_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:V4SI 2 "s_register_operand" "w")]
	 VSBCQ))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(reg:SI VFPCC_REGNUM)]
	 VSBCQ))
  ]
  "TARGET_HAVE_MVE"
  "vsbc.i32\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length" "4")])

;;
;; [vst2q])
;;
(define_insn "mve_vst2q<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
	(unspec:OI [(match_operand:OI 1 "s_register_operand" "w")
		    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VST2Q))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
   rtx ops[4];
   int regno = REGNO (operands[1]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1] = gen_rtx_REG (TImode, regno + 4);
   rtx reg  = operands[0];
   while (reg && !REG_P (reg))
    reg = XEXP (reg, 0);
   gcc_assert (REG_P (reg));
   ops[2] = reg;
   ops[3] = operands[0];
   output_asm_insn ("vst20.<V_sz_elem>\t{%q0, %q1}, [%2]\n\t"
		    "vst21.<V_sz_elem>\t{%q0, %q1}, %3", ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vld2q])
;;
(define_insn "mve_vld2q<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
	(unspec:OI [(match_operand:OI 1 "neon_struct_operand" "Um")
		    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VLD2Q))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
   rtx ops[4];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1] = gen_rtx_REG (TImode, regno + 4);
   rtx reg  = operands[1];
   while (reg && !REG_P (reg))
    reg = XEXP (reg, 0);
   gcc_assert (REG_P (reg));
   ops[2] = reg;
   ops[3] = operands[1];
   output_asm_insn ("vld20.<V_sz_elem>\t{%q0, %q1}, [%2]\n\t"
		    "vld21.<V_sz_elem>\t{%q0, %q1}, %3", ops);
   return "";
}
  [(set_attr "length" "8")])

;;
;; [vld4q])
;;
(define_insn "mve_vld4q<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
	(unspec:XI [(match_operand:XI 1 "neon_struct_operand" "Um")
		    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VLD4Q))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
   rtx ops[6];
   int regno = REGNO (operands[0]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1] = gen_rtx_REG (TImode, regno+4);
   ops[2] = gen_rtx_REG (TImode, regno+8);
   ops[3] = gen_rtx_REG (TImode, regno + 12);
   rtx reg  = operands[1];
   while (reg && !REG_P (reg))
    reg = XEXP (reg, 0);
   gcc_assert (REG_P (reg));
   ops[4] = reg;
   ops[5] = operands[1];
   output_asm_insn ("vld40.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vld41.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vld42.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vld43.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, %5", ops);
   return "";
}
  [(set_attr "length" "16")])
;;
;; [vgetq_lane_u, vgetq_lane_s, vgetq_lane_f])
;;
(define_insn "mve_vec_extract<mode><V_elem_l>"
 [(set (match_operand:<V_elem> 0 "nonimmediate_operand" "=r")
   (vec_select:<V_elem>
    (match_operand:MVE_VLD_ST 1 "s_register_operand" "w")
    (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov.<V_extr_elem>\t%0, %q1[%c2]";
}
 [(set_attr "type" "mve_move")])

(define_insn "mve_vec_extractv2didi"
 [(set (match_operand:DI 0 "nonimmediate_operand" "=r")
   (vec_select:DI
    (match_operand:V2DI 1 "s_register_operand" "w")
    (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_HAVE_MVE"
{
  int elt = INTVAL (operands[2]);
  if (BYTES_BIG_ENDIAN)
    elt = 1 - elt;

  if (elt == 0)
   return "vmov\t%Q0, %R0, %e1";
  else
   return "vmov\t%Q0, %R0, %f1";
}
 [(set_attr "type" "mve_move")])

(define_insn "*mve_vec_extract_sext_internal<mode>"
 [(set (match_operand:SI 0 "s_register_operand" "=r")
   (sign_extend:SI
    (vec_select:<V_elem>
     (match_operand:MVE_2 1 "s_register_operand" "w")
     (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov.s<V_sz_elem>\t%0, %q1[%c2]";
}
 [(set_attr "type" "mve_move")])

(define_insn "*mve_vec_extract_zext_internal<mode>"
 [(set (match_operand:SI 0 "s_register_operand" "=r")
   (zero_extend:SI
    (vec_select:<V_elem>
     (match_operand:MVE_2 1 "s_register_operand" "w")
     (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov.u<V_sz_elem>\t%0, %q1[%c2]";
}
 [(set_attr "type" "mve_move")])

;;
;; [vsetq_lane_u, vsetq_lane_s, vsetq_lane_f])
;;
(define_insn "mve_vec_set<mode>_internal"
 [(set (match_operand:VQ2 0 "s_register_operand" "=w")
       (vec_merge:VQ2
	(vec_duplicate:VQ2
	  (match_operand:<V_elem> 1 "nonimmediate_operand" "r"))
	(match_operand:VQ2 3 "s_register_operand" "0")
	(match_operand:SI 2 "immediate_operand" "i")))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;
  if (BYTES_BIG_ENDIAN)
    elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
  operands[2] = GEN_INT (elt);

  return "vmov.<V_sz_elem>\t%q0[%c2], %1";
}
 [(set_attr "type" "mve_move")])

(define_insn "mve_vec_setv2di_internal"
 [(set (match_operand:V2DI 0 "s_register_operand" "=w")
       (vec_merge:V2DI
	(vec_duplicate:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" "r"))
	(match_operand:V2DI 3 "s_register_operand" "0")
	(match_operand:SI 2 "immediate_operand" "i")))]
 "TARGET_HAVE_MVE"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;
  if (BYTES_BIG_ENDIAN)
    elt = 1 - elt;

  if (elt == 0)
   return "vmov\t%e0, %Q1, %R1";
  else
   return "vmov\t%f0, %J1, %K1";
}
 [(set_attr "type" "mve_move")])

;;
;; [uqrshll_di]
;;
(define_insn "mve_uqrshll_sat<supf>_di"
  [(set (match_operand:DI 0 "arm_low_register_operand" "=l")
	(unspec:DI [(match_operand:DI 1 "arm_low_register_operand" "0")
		    (match_operand:SI 2 "register_operand" "r")]
	 UQRSHLLQ))]
  "TARGET_HAVE_MVE"
  "uqrshll%?\\t%Q1, %R1, #<supf>, %2"
  [(set_attr "predicable" "yes")])

;;
;; [sqrshrl_di]
;;
(define_insn "mve_sqrshrl_sat<supf>_di"
  [(set (match_operand:DI 0 "arm_low_register_operand" "=l")
	(unspec:DI [(match_operand:DI 1 "arm_low_register_operand" "0")
		    (match_operand:SI 2 "register_operand" "r")]
	 SQRSHRLQ))]
  "TARGET_HAVE_MVE"
  "sqrshrl%?\\t%Q1, %R1, #<supf>, %2"
  [(set_attr "predicable" "yes")])

;;
;; [uqrshl_si]
;;
(define_insn "mve_uqrshl_si"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "arm_general_register_operand" "0")
		    (match_operand:SI 2 "register_operand" "r")]
	 UQRSHL))]
  "TARGET_HAVE_MVE"
  "uqrshl%?\\t%1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [sqrshr_si]
;;
(define_insn "mve_sqrshr_si"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "arm_general_register_operand" "0")
		    (match_operand:SI 2 "register_operand" "r")]
	 SQRSHR))]
  "TARGET_HAVE_MVE"
  "sqrshr%?\\t%1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [uqshll_di]
;;
(define_insn "mve_uqshll_di"
  [(set (match_operand:DI 0 "arm_low_register_operand" "=l")
	(us_ashift:DI (match_operand:DI 1 "arm_low_register_operand" "0")
		      (match_operand:SI 2 "immediate_operand" "Pg")))]
  "TARGET_HAVE_MVE"
  "uqshll%?\\t%Q1, %R1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [urshrl_di]
;;
(define_insn "mve_urshrl_di"
  [(set (match_operand:DI 0 "arm_low_register_operand" "=l")
	(unspec:DI [(match_operand:DI 1 "arm_low_register_operand" "0")
		    (match_operand:SI 2 "immediate_operand" "Pg")]
	 URSHRL))]
  "TARGET_HAVE_MVE"
  "urshrl%?\\t%Q1, %R1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [uqshl_si]
;;
(define_insn "mve_uqshl_si"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r")
	(us_ashift:SI (match_operand:SI 1 "arm_general_register_operand" "0")
		      (match_operand:SI 2 "immediate_operand" "Pg")))]
  "TARGET_HAVE_MVE"
  "uqshl%?\\t%1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [urshr_si]
;;
(define_insn "mve_urshr_si"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "arm_general_register_operand" "0")
		    (match_operand:SI 2 "immediate_operand" "Pg")]
	 URSHR))]
  "TARGET_HAVE_MVE"
  "urshr%?\\t%1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [sqshl_si]
;;
(define_insn "mve_sqshl_si"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r")
	(ss_ashift:SI (match_operand:DI 1 "arm_general_register_operand" "0")
		      (match_operand:SI 2 "immediate_operand" "Pg")))]
  "TARGET_HAVE_MVE"
  "sqshl%?\\t%1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [srshr_si]
;;
(define_insn "mve_srshr_si"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r")
	(unspec:SI [(match_operand:DI 1 "arm_general_register_operand" "0")
		    (match_operand:SI 2 "immediate_operand" "Pg")]
	 SRSHR))]
  "TARGET_HAVE_MVE"
  "srshr%?\\t%1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [srshrl_di]
;;
(define_insn "mve_srshrl_di"
  [(set (match_operand:DI 0 "arm_low_register_operand" "=l")
	(unspec:DI [(match_operand:DI 1 "arm_low_register_operand" "0")
		    (match_operand:SI 2 "immediate_operand" "Pg")]
	 SRSHRL))]
  "TARGET_HAVE_MVE"
  "srshrl%?\\t%Q1, %R1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [sqshll_di]
;;
(define_insn "mve_sqshll_di"
  [(set (match_operand:DI 0 "arm_low_register_operand" "=l")
	(ss_ashift:DI (match_operand:DI 1 "arm_low_register_operand" "0")
		      (match_operand:SI 2 "immediate_operand" "Pg")))]
  "TARGET_HAVE_MVE"
  "sqshll%?\\t%Q1, %R1, %2"
  [(set_attr "predicable" "yes")])

;;
;; [vshlcq_m_u vshlcq_m_s]
;;
(define_expand "mve_vshlcq_m_vec_<supf><mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_32")
  (match_operand:HI 4 "vpr_register_operand")
  (unspec:MVE_2 [(const_int 0)] VSHLCQ_M)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (SImode);
  emit_insn (gen_mve_vshlcq_m_<supf><mode> (operands[0], ignore_wb, operands[1],
					    operands[2], operands[3],
					    operands[4]));
  DONE;
})

(define_expand "mve_vshlcq_m_carry_<supf><mode>"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_32")
  (match_operand:HI 4 "vpr_register_operand")
  (unspec:MVE_2 [(const_int 0)] VSHLCQ_M)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_mve_vshlcq_m_<supf><mode> (ignore_vec, operands[0],
					    operands[1], operands[2],
					    operands[3], operands[4]));
  DONE;
})

(define_insn "mve_vshlcq_m_<supf><mode>"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 2 "s_register_operand" "0")
		      (match_operand:SI 3 "s_register_operand" "1")
		      (match_operand:SI 4 "mve_imm_32" "Rf")
		      (match_operand:HI 5 "vpr_register_operand" "Up")]
	VSHLCQ_M))
  (set (match_operand:SI  1 "s_register_operand" "=r")
       (unspec:SI [(match_dup 2)
		   (match_dup 3)
		   (match_dup 4)
		   (match_dup 5)]
	VSHLCQ_M))
 ]
 "TARGET_HAVE_MVE"
 "vpst\;vshlct\t%q0, %1, %4"
 [(set_attr "type" "mve_move")
  (set_attr "length" "8")])

;; CDE instructions on MVE registers.

(define_insn "arm_vcx1qv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			   (match_operand:SI 2 "const_int_mve_cde1_operand" "i")]
	 UNSPEC_VCDE))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx1\\tp%c1, %q0, #%c2"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_vcx1qav16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			    (match_operand:V16QI 2 "register_operand" "0")
			    (match_operand:SI 3 "const_int_mve_cde1_operand" "i")]
	 UNSPEC_VCDEA))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx1a\\tp%c1, %q0, #%c3"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_vcx2qv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "t")
			  (match_operand:SI 3 "const_int_mve_cde2_operand" "i")]
	 UNSPEC_VCDE))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx2\\tp%c1, %q0, %q2, #%c3"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_vcx2qav16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "0")
			  (match_operand:V16QI 3 "register_operand" "t")
			  (match_operand:SI 4 "const_int_mve_cde2_operand" "i")]
	 UNSPEC_VCDEA))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx2a\\tp%c1, %q0, %q3, #%c4"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_vcx3qv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "t")
			  (match_operand:V16QI 3 "register_operand" "t")
			  (match_operand:SI 4 "const_int_mve_cde3_operand" "i")]
	 UNSPEC_VCDE))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx3\\tp%c1, %q0, %q2, %q3, #%c4"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_vcx3qav16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "0")
			  (match_operand:V16QI 3 "register_operand" "t")
			  (match_operand:V16QI 4 "register_operand" "t")
			  (match_operand:SI 5 "const_int_mve_cde3_operand" "i")]
	 UNSPEC_VCDEA))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx3a\\tp%c1, %q0, %q3, %q4, #%c5"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_vcx1q<a>_p_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			   (match_operand:V16QI 2 "register_operand" "0")
			   (match_operand:SI 3 "const_int_mve_cde1_operand" "i")
			   (match_operand:HI 4 "vpr_register_operand" "Up")]
	 CDE_VCX))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vpst\;vcx1<a>t\\tp%c1, %q0, #%c3"
  [(set_attr "type" "coproc")
   (set_attr "length" "8")]
)

(define_insn "arm_vcx2q<a>_p_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "0")
			  (match_operand:V16QI 3 "register_operand" "t")
			  (match_operand:SI 4 "const_int_mve_cde2_operand" "i")
			  (match_operand:HI 5 "vpr_register_operand" "Up")]
	 CDE_VCX))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vpst\;vcx2<a>t\\tp%c1, %q0, %q3, #%c4"
  [(set_attr "type" "coproc")
   (set_attr "length" "8")]
)

(define_insn "arm_vcx3q<a>_p_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "0")
			  (match_operand:V16QI 3 "register_operand" "t")
			  (match_operand:V16QI 4 "register_operand" "t")
			  (match_operand:SI 5 "const_int_mve_cde3_operand" "i")
			  (match_operand:HI 6 "vpr_register_operand" "Up")]
	 CDE_VCX))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vpst\;vcx3<a>t\\tp%c1, %q0, %q3, %q4, #%c5"
  [(set_attr "type" "coproc")
   (set_attr "length" "8")]
)

(define_insn "*movmisalign<mode>_mve_store"
  [(set (match_operand:MVE_VLD_ST 0 "neon_permissive_struct_operand"	     "=Ux")
	(unspec:MVE_VLD_ST [(match_operand:MVE_VLD_ST 1 "s_register_operand" " w")]
	 UNSPEC_MISALIGNED_ACCESS))]
  "((TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
    || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode)))
   && !BYTES_BIG_ENDIAN && unaligned_access"
  "vstr<V_sz_elem1>.<V_sz_elem>\t%q1, %E0"
  [(set_attr "type" "mve_store")]
)


(define_insn "*movmisalign<mode>_mve_load"
  [(set (match_operand:MVE_VLD_ST 0 "s_register_operand"				 "=w")
	(unspec:MVE_VLD_ST [(match_operand:MVE_VLD_ST 1 "neon_permissive_struct_operand" " Ux")]
	 UNSPEC_MISALIGNED_ACCESS))]
  "((TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
    || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode)))
   && !BYTES_BIG_ENDIAN && unaligned_access"
  "vldr<V_sz_elem1>.<V_sz_elem>\t%q0, %E1"
  [(set_attr "type" "mve_load")]
)
