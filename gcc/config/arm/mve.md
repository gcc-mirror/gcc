;; Arm M-profile Vector Extension Machine Description
;; Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

(define_insn "mve_mov<mode>"
  [(set (match_operand:MVE_types 0 "nonimmediate_operand" "=w,w,r,w   , w,   r,Ux,w")
	(match_operand:MVE_types 1 "general_operand"      " w,r,w,DnDm,UxUi,r,w, Ul"))]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
{
  switch (which_alternative)
    {
    case 0:  /* [w,w].  */
      return "vmov\t%q0, %q1";

    case 1:  /* [w,r].  */
      return "vmov\t%e0, %Q1, %R1  %@ <mode>\;vmov\t%f0, %J1, %K1";

    case 2:  /* [r,w].  */
      return "vmov\t%Q0, %R0, %e1  %@ <mode>\;vmov\t%J0, %K0, %f1";

    case 3:  /* [w,DnDm].  */
      {
	int width, is_valid;

	is_valid = simd_immediate_valid_for_move (operands[1], <MODE>mode,
						  &operands[1], &width);

	gcc_assert (is_valid);

	if (width == 0)
	  return "vmov.f32\t%q0, %1  %@ <mode>";
	else
	  {
	    const int templ_size = 40;
	    static char templ[templ_size];
	    if (snprintf (templ, templ_size,
			  "vmov.i%d\t%%q0, %%x1  %%@ <mode>", width)
		> templ_size)
	      abort ();
	    return templ;
	  }
      }

    case 4:  /* [w,UxUi].  */
      if (<MODE>mode == V2DFmode || <MODE>mode == V2DImode
	  || <MODE>mode == TImode)
	return "vldrw.u32\t%q0, %E1";
      else
	return "vldr<V_sz_elem1>.<V_sz_elem>\t%q0, %E1";

    case 5:  /* [r,r].  */
      return output_move_quad (operands);

    case 6:  /* [Ux,w].  */
      if (<MODE>mode == V2DFmode || <MODE>mode == V2DImode
	  || <MODE>mode == TImode)
	return "vstrw.32\t%q1, %E0";
      else
	return "vstr<V_sz_elem1>.<V_sz_elem>\t%q1, %E0";

    case 7:  /* [w,Ul].  */
	return output_move_neon (operands);

    default:
      gcc_unreachable ();
      return "";
    }
}
   [(set_attr_alternative "mve_unpredicated_insn" [(symbol_ref "CODE_FOR_mve_mov<mode>")
						   (symbol_ref "CODE_FOR_nothing")
						   (symbol_ref "CODE_FOR_nothing")
						   (symbol_ref "CODE_FOR_mve_mov<mode>")
						   (symbol_ref "CODE_FOR_mve_mov<mode>")
						   (symbol_ref "CODE_FOR_nothing")
						   (symbol_ref "CODE_FOR_mve_mov<mode>")
						   (symbol_ref "CODE_FOR_nothing")])
   (set_attr "type" "mve_move,mve_move,mve_move,mve_move,mve_load,multiple,mve_store,mve_load")
   (set_attr "length" "4,8,8,4,4,8,4,8")
   (set_attr "thumb2_pool_range" "*,*,*,*,1018,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,*,996,*,*,*")])

;;
;; [vdupq_n_u, vdupq_n_s, vdupq_n_f]
;;
(define_insn "@mve_vdupq_n<mode>"
  [(set (match_operand:MVE_VLD_ST 0 "s_register_operand" "=w")
	(vec_duplicate:MVE_VLD_ST
	  (match_operand:<V_elem> 1 "s_register_operand" "r")))]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
  "vdup.<V_sz_elem>\t%q0, %1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vdupq_n<mode>"))
  (set_attr "length" "4")
   (set_attr "type" "mve_move")])

;;
;; [vst4q])
;;
(define_insn "@mve_vst4q<mode>"
  [(set (match_operand:<MVE_VLD4_VST4> 0 "mve_struct_operand" "=Ug")
	(unspec:<MVE_VLD4_VST4>
		[(match_operand:<MVE_VLD4_VST4> 1 "s_register_operand" "w")
		 (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VST4Q))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD4_VST4>mode))"
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
;; [vrndaq_f]
;; [vrndmq_f]
;; [vrndnq_f]
;; [vrndpq_f]
;; [vrndq_f]
;; [vrndxq_f]
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 MVE_FP_UNARY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_mnemo>.f%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vrev64q_f])
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=&w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 MVE_FP_VREV64Q_ONLY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vabsq_f]
;; [vnegq_f]
;;
(define_insn "mve_v<absneg_str>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(ABSNEG:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "v<absneg_str>.f%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_v<absneg_str>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vrev32q_f])
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_V8HF 0 "s_register_operand" "=w")
	(unspec:MVE_V8HF [(match_operand:MVE_V8HF 1 "s_register_operand" "w")]
	 MVE_FP_VREV32Q_ONLY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcvtbq_f32_f16]
;; [vcvttq_f32_f16]
;;
(define_insn "@mve_<mve_insn>q_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VCVTxQ_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f32.f16\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f32_f16v4sf"))
  (set_attr "type" "mve_move")
])

;;
;; [vcvtq_to_f_s, vcvtq_to_f_u]
;;
(define_insn "@mve_<mve_insn>q_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTQ_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>.<supf>%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_to_f_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vrev64q_u, vrev64q_s])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=&w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VREV64Q))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcvtq_from_f_s, vcvtq_from_f_u]
;;
(define_insn "@mve_<mve_insn>q_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTQ_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_from_f_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vabsq_s]
;; [vnegq_s]
;;
(define_insn "mve_v<absneg_str>q_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(ABSNEG:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "v<absneg_str>.s%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_v<absneg_str>q_s<mode>"))
  (set_attr "type" "mve_move")
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vmvnq_u<mode>"))
  (set_attr "type" "mve_move")
])
(define_expand "mve_vmvnq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(not:MVE_2 (match_operand:MVE_2 1 "s_register_operand")))
  ]
  "TARGET_HAVE_MVE"
)

;;
;; [vclzq_u, vclzq_s])
;;
(define_insn "@mve_vclzq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(clz:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vclz.i%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vclzq_s<mode>"))
  (set_attr "type" "mve_move")
])
(define_expand "mve_vclzq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand")
	(clz:MVE_2 (match_operand:MVE_2 1 "s_register_operand")))
  ]
  "TARGET_HAVE_MVE"
)

;;
;; [vclsq_s]
;; [vqabsq_s]
;; [vqnegq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 MVE_INT_UNARY))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddvq_s, vaddvq_u])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VADDVQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vrev32q_u, vrev32q_s])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_3 0 "s_register_operand" "=w")
	(unspec:MVE_3 [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VREV32Q))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmovlbq_s, vmovlbq_u]
;; [vmovltq_u, vmovltq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VMOVLxQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcvtaq_u, vcvtaq_s]
;; [vcvtmq_s, vcvtmq_u]
;; [vcvtnq_s, vcvtnq_u]
;; [vcvtpq_s, vcvtpq_u]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTxQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmvnq_n_u, vmvnq_n_s])
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<V_elem> 1 "immediate_operand" "i")]
	 VMVNQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.i%#<V_sz_elem>\t%q0, %1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vrev16q_u, vrev16q_s])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_V16QI 0 "s_register_operand" "=w")
	(unspec:MVE_V16QI [(match_operand:MVE_V16QI 1 "s_register_operand" "w")]
	 VREV16Q))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<V_sz_elem>\t%q0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddlvq_s vaddlvq_u])
;;
(define_insn "@mve_<mve_insn>q_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")]
	 VADDLVQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>32\t%Q0, %R0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vctp8q vctp16q vctp32q vctp64q])
;;
(define_insn "@mve_vctp<MVE_vctp>q<MVE_vpred>"
  [
   (set (match_operand:MVE_7 0 "vpr_register_operand" "=Up")
	(unspec:MVE_7 [(match_operand:SI 1 "s_register_operand" "r")]
	VCTP))
  ]
  "TARGET_HAVE_MVE"
  "vctp.<MVE_vctp>\t%1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vctp<MVE_vctp>q<MVE_vpred>"))
  (set_attr "type" "mve_move")
])

;;
;; [vpnot])
;;
(define_insn "mve_vpnotv16bi"
  [
   (set (match_operand:V16BI 0 "vpr_register_operand" "=Up")
	(unspec:V16BI [(match_operand:V16BI 1 "vpr_register_operand" "0")]
	 VPNOT))
  ]
  "TARGET_HAVE_MVE"
  "vpnot"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vpnotv16bi"))
  (set_attr "type" "mve_move")
])

;;
;; [vbrsrq_n_f])
;;
(define_insn "@mve_<mve_insn>q_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 MVE_VBRSR_N_FP))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcvtq_n_to_f_s, vcvtq_n_to_f_u]
;;
(define_insn "@mve_<mve_insn>q_n_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 VCVTQ_N_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f<V_sz_elem>.<supf><V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_to_f_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;; [vcreateq_f])
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:DI 1 "s_register_operand" "r")
		       (match_operand:DI 2 "s_register_operand" "r")]
	 MVE_FP_CREATE_ONLY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmov %q0[2], %q0[0], %Q1, %Q2\;vmov %q0[3], %q0[1], %R1, %R2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcreateq_u, vcreateq_s])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_1 0 "s_register_operand" "=w")
	(unspec:MVE_1 [(match_operand:DI 1 "s_register_operand" "r")
		       (match_operand:DI 2 "s_register_operand" "r")]
	 VCREATEQ))
  ]
  "TARGET_HAVE_MVE"
  "vmov %q0[2], %q0[0], %Q1, %Q2\;vmov %q0[3], %q0[1], %R1, %R2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshrq_n_s, vrshrq_n_u]
;; [vshrq_n_s, vshrq_n_u]
;;
;; Version that takes an immediate as operand 2.
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 MVE_VSHRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf><V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vshrq_n_s<mode>_imm"))
  (set_attr "type" "mve_move")
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vshrq_n_u<mode>_imm"))
  (set_attr "type" "mve_move")
])

;;
;; [vcvtq_n_from_f_s, vcvtq_n_from_f_u]
;;
(define_insn "@mve_<mve_insn>q_n_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 VCVTQ_N_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.<supf><V_sz_elem>.f<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_from_f_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddlvq_p_s])
;;
(define_insn "@mve_<mve_insn>q_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4BI 2 "vpr_register_operand" "Up")]
	 VADDLVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>32\t%Q0, %R0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
  (set_attr "length""8")])

;;
;; [vcmpneq_, vcmpcsq_, vcmpeqq_, vcmpgeq_, vcmpgtq_, vcmphiq_, vcmpleq_, vcmpltq_])
;;
(define_insn "@mve_vcmp<mve_cmp_op>q_<mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(MVE_COMPARISONS:<MVE_VPRED> (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.<mve_cmp_type>%#<V_sz_elem>\t<mve_cmp_op>, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op>q_<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcmpcsq_n_, vcmpeqq_n_, vcmpgeq_n_, vcmpgtq_n_, vcmphiq_n_, vcmpleq_n_, vcmpltq_n_, vcmpneq_n_])
;;
(define_insn "@mve_vcmp<mve_cmp_op>q_n_<mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(MVE_COMPARISONS:<MVE_VPRED>
	 (match_operand:MVE_2 1 "s_register_operand" "w")
	 (vec_duplicate:MVE_2 (match_operand:<V_elem> 2 "s_register_operand" "r"))))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.<mve_cmp_type>%#<V_sz_elem>	<mve_cmp_op>, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op>q_n_<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vshlq_s, vshlq_u])
;; See vec-common.md

;;
;; [vabdq_s, vabdq_u]
;; [vhaddq_s, vhaddq_u]
;; [vhsubq_s, vhsubq_u]
;; [vmulhq_s, vmulhq_u]
;; [vqaddq_u, vqaddq_s]
;; [vqdmulhq_s]
;; [vqrdmulhq_s]
;; [vqrshlq_s, vqrshlq_u]
;; [vqshlq_s, vqshlq_u]
;; [vqsubq_u, vqsubq_s]
;; [vrhaddq_s, vrhaddq_u]
;; [vrmulhq_s, vrmulhq_u]
;; [vrshlq_s, vrshlq_u]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 MVE_INT_SU_BINARY))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddq_n_s, vaddq_n_u]
;; [vsubq_n_s, vsubq_n_u]
;; [vmulq_n_s, vmulq_n_u]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 MVE_INT_N_BINARY))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.i%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddvaq_s, vaddvaq_u])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VADDVAQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vaddvq_p_u, vaddvq_p_s])
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<MVE_VPRED> 2 "vpr_register_operand" "Up")]
	 VADDVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%0, %q1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
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
   [(set_attr_alternative "mve_unpredicated_insn" [(symbol_ref "CODE_FOR_mve_vandq_u<mode>")
						   (symbol_ref "CODE_FOR_nothing")])
  (set_attr "type" "mve_move")
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
(define_insn "@mve_vbicq_u<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(and:MVE_2 (not:MVE_2 (match_operand:MVE_2 2 "s_register_operand" "w"))
			      (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "vbic\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vbicq_u<mode>"))
  (set_attr "type" "mve_move")
])

(define_expand "@mve_vbicq_s<mode>"
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
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VBRSRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcaddq_rot90_s, vcaddq_rot90_u]
;; [vcaddq_rot270_s, vcaddq_rot270_u]
;; [vhcaddq_rot90_s]
;; [vhcaddq_rot270_s]
;;
(define_insn "@mve_<mve_insn>q<mve_rot>_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VxCADDQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<isu>%#<V_sz_elem>\t%q0, %q1, %q2, #<rot>"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q<mve_rot>_<supf><mode>"))
  (set_attr "type" "mve_move")
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_veorq_u<mode>"))
  (set_attr "type" "mve_move")
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
;; [vhaddq_n_u, vhaddq_n_s]
;; [vhsubq_n_u, vhsubq_n_s]
;; [vqaddq_n_s, vqaddq_n_u]
;; [vqdmulhq_n_s]
;; [vqrdmulhq_n_s]
;; [vqsubq_n_s, vqsubq_n_u]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 MVE_INT_SU_N_BINARY))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmaxaq_s]
;; [vminaq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 MVE_VMAXAVMINAQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.s%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmaxq_u, vmaxq_s]
;; [vminq_s, vminq_u]
;;
(define_insn "mve_<max_min_su_str>q_<max_min_supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(MAX_MIN_SU:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "<max_min_su_str>.<max_min_supf>%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<max_min_su_str>q_<max_min_supf><mode>"))
  (set_attr "type" "mve_move")
])


;;
;; [vmaxavq_s]
;; [vmaxvq_u, vmaxvq_s]
;; [vminavq_s]
;; [vminvq_u, vminvq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 MVE_VMAXVQ_VMINVQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "<mve_vmaxmin_safe_imp>")
  (set_attr "type" "mve_move")
])

;;
;; [vmladavq_u, vmladavq_s]
;; [vmladavxq_s]
;; [vmlsdavq_s]
;; [vmlsdavxq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 MVE_VMLxDAVQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vmullbq_int_u, vmullbq_int_s]
;; [vmulltq_int_u, vmulltq_int_s]
;;
(define_insn "@mve_<mve_insn>q_int_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_2 1 "s_register_operand" "w")
				  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULLxQ_INT))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<isu>%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_int_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddq_s, vaddq_u]
;; [vmulq_u, vmulq_s]
;; [vsubq_s, vsubq_u]
;;
(define_insn "mve_<mve_addsubmul>q<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(MVE_INT_BINARY_RTX:MVE_2 (match_operand:MVE_2 1 "s_register_operand" "w")
			      (match_operand:MVE_2 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
  "<mve_addsubmul>.i%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_addsubmul>q<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vornq_u, vornq_s]
;;
(define_insn "@mve_vornq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(ior:MVE_2 (not:MVE_2 (match_operand:MVE_2 2 "s_register_operand" "w"))
		   (match_operand:MVE_2 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE"
   "vorn\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vornq_s<mode>"))
  (set_attr "type" "mve_move")
])

(define_expand "@mve_vornq_u<mode>"
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vorrq_s<mode>"))
  (set_attr "type" "mve_move")
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
;; [vqrshlq_n_s, vqrshlq_n_u]
;; [vrshlq_n_u, vrshlq_n_s]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 MVE_RSHIFT_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vqshlq_n_s, vqshlq_n_u]
;; [vshlq_n_u, vshlq_n_s]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 MVE_SHIFT_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vqshlq_r_u, vqshlq_r_s]
;; [vshlq_r_s, vshlq_r_u]
;;
(define_insn "@mve_<mve_insn>q_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 MVE_SHIFT_R))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_r_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vqshluq_n_s])
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred>" "<MVE_constraint>")]
	 VQSHLUQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vabdq_f]
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 MVE_FP_VABDQ_ONLY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddlvaq_s vaddlvaq_u])
;;
(define_insn "@mve_<mve_insn>q_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VADDLVAQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>32\t%Q0, %R0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vaddq_n_f]
;; [vsubq_n_f]
;; [vmulq_n_f]
;;
(define_insn "@mve_<mve_insn>q_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 MVE_FP_N_BINARY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_f<mode>"))
  (set_attr "type" "mve_move")
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
  "vand\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vandq_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vbicq_f])
;;
(define_insn "@mve_vbicq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(and:MVE_0 (not:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w"))
			      (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vbic\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vbicq_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcaddq_rot90_f, vcaddq_rot270_f]
;; [vcmulq, vcmulq_rot90, vcmulq_rot180, vcmulq_rot270]
;;
(define_insn "@mve_<mve_insn>q<mve_rot>_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 MVE_VCADDQ_VCMULQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>\t%q0, %q1, %q2, #<rot>"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q<mve_rot>_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_f, vcmpgeq_f, vcmpgtq_f, vcmpleq_f, vcmpltq_f, vcmpneq_f])
;;
(define_insn "@mve_vcmp<mve_cmp_op>q_f<mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(MVE_FP_COMPARISONS:<MVE_VPRED> (match_operand:MVE_0 1 "s_register_operand" "w")
			       (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	<mve_cmp_op>, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_n_f, vcmpgeq_n_f, vcmpgtq_n_f, vcmpleq_n_f, vcmpltq_n_f, vcmpneq_n_f])
;;
(define_insn "@mve_vcmp<mve_cmp_op>q_n_f<mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(MVE_FP_COMPARISONS:<MVE_VPRED>
	 (match_operand:MVE_0 1 "s_register_operand" "w")
	 (vec_duplicate:MVE_0 (match_operand:<V_elem> 2 "s_register_operand" "r"))))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	<mve_cmp_op>, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op>q_n_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vctp8q_m vctp16q_m vctp32q_m vctp64q_m])
;;
(define_insn "@mve_vctp<MVE_vctp>q_m<MVE_vpred>"
  [
   (set (match_operand:MVE_7 0 "vpr_register_operand" "=Up")
	(unspec:MVE_7 [(match_operand:SI 1 "s_register_operand" "r")
		    (match_operand:MVE_7 2 "vpr_register_operand" "Up")]
	 VCTP_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vctpt.<MVE_vctp>\t%1"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vctp<MVE_vctp>q<MVE_vpred>"))
  (set_attr "type" "mve_move")
  (set_attr "length""8")
])

;;
;; [vcvtbq_f16_f32]
;; [vcvttq_f16_f32]
;;
(define_insn "@mve_<mve_insn>q_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		      (match_operand:V4SF 2 "s_register_operand" "w")]
	 VCVTxQ_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f16.f32\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f16_f32v8hf"))
  (set_attr "type" "mve_move")
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
  "veor\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_veorq_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmaxnmaq_f]
;; [vminnmaq_f]
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 MVE_VMAXNMA_VMINNMAQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmaxnmavq_f]
;; [vmaxnmvq_f]
;; [vminnmavq_f]
;; [vminnmvq_f]
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 MVE_VMAXNMxV_MINNMxVQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>\t%0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmaxnmq_f]
;; [vminnmq_f]
;;
(define_insn "@mve_<max_min_f_str>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(MAX_MIN_F:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
			 (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<max_min_f_str>.f%#<V_sz_elem>	%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<max_min_f_str>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmlaldavq_u, vmlaldavq_s]
;; [vmlaldavxq_s]
;; [vmlsldavq_s]
;; [vmlsldavxq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 MVE_VMLxLDAVxQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%Q0, %R0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vmovnbq_u, vmovnbq_s]
;; [vmovntq_s, vmovntq_u]
;; [vqmovnbq_u, vqmovnbq_s]
;; [vqmovntq_u, vqmovntq_s]
;; [vqmovunbq_s]
;; [vqmovuntq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 MVE_MOVN))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<isu>%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vaddq_f]
;; [vmulq_f]
;; [vsubq_f]
;;
(define_insn "mve_<mve_addsubmul>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(MVE_INT_BINARY_RTX:MVE_0 (match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_addsubmul>.f%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_addsubmul>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vornq_f]
;;
(define_insn "@mve_vornq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(ior:MVE_0 (not:MVE_0 (match_operand:MVE_0 2 "s_register_operand" "w"))
		   (match_operand:MVE_0 1 "s_register_operand" "w")))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vorn\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vornq_f<mode>"))
  (set_attr "type" "mve_move")
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
  "vorr\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vorrq_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vbicq_n_s, vbicq_n_u]
;; [vorrq_n_u, vorrq_n_s]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 MVE_INT_N_BINARY_LOGIC))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.i%#<V_sz_elem>	%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vqdmullbq_n_s]
;; [vqdmulltq_n_s]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 MVE_VQDMULLxQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.s%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vqdmullbq_s]
;; [vqdmulltq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:MVE_5 2 "s_register_operand" "w")]
	 MVE_VQDMULLxQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.s%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vrmlaldavhq_u vrmlaldavhq_s]
;; [vrmlaldavhxq_s]
;; [vrmlsldavhq_s]
;; [vrmlsldavhxq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 MVE_VRMLxLDAVxQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>32\t%Q0, %R0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vshllbq_n_s, vshllbq_n_u]
;; [vshlltq_n_u, vshlltq_n_s]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:SI 2 "immediate_operand" "i")]
	 VSHLLxQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmulltq_poly_p]
;; [vmullbq_poly_p]
;;
(define_insn "@mve_<mve_insn>q_poly_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:MVE_3 2 "s_register_operand" "w")]
	 VMULLxQ_POLY))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_poly_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_m_f]
;; [vcmpgeq_m_f]
;; [vcmpgtq_m_f]
;; [vcmpleq_m_f]
;; [vcmpltq_m_f]
;; [vcmpneq_m_f]
;;
(define_insn "@mve_vcmp<mve_cmp_op1>q_m_f<mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(unspec:<MVE_VPRED> [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")
		    (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_CMP_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>\t<mve_cmp_op1>, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op1>q_f<mode>"))
  (set_attr "length""8")])

;;
;; [vcvtaq_m_u, vcvtaq_m_s]
;; [vcvtmq_m_s, vcvtmq_m_u]
;; [vcvtnq_m_s, vcvtnq_m_u]
;; [vcvtpq_m_u, vcvtpq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VCVTxQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length""8")])

;;
;; [vcvtq_m_to_f_s, vcvtq_m_to_f_u]
;;
(define_insn "@mve_<mve_insn>q_m_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VCVTQ_M_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>.<supf>%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_to_f_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length""8")])

;;
;; [vqrshrnbq_n_u, vqrshrnbq_n_s]
;; [vqrshrntq_n_u, vqrshrntq_n_s]
;; [vqrshrunbq_n_s]
;; [vqrshruntq_n_s]
;; [vqshrnbq_n_u, vqshrnbq_n_s]
;; [vqshrntq_n_u, vqshrntq_n_s]
;; [vqshrunbq_n_s]
;; [vqshruntq_n_s]
;; [vrshrnbq_n_s, vrshrnbq_n_u]
;; [vrshrntq_n_u, vrshrntq_n_s]
;; [vshrnbq_n_u, vshrnbq_n_s]
;; [vshrntq_n_s, vshrntq_n_u]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")
				 (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")]
	 MVE_SHRN_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<isu>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vrmlaldavhaq_s vrmlaldavhaq_u]
;; [vrmlaldavhaxq_s]
;; [vrmlsldavhaq_s]
;; [vrmlsldavhaxq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		    (match_operand:V4SI 2 "s_register_operand" "w")
		    (match_operand:V4SI 3 "s_register_operand" "w")]
	 MVE_VRMLxLDAVHAxQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>32\t%Q0, %R0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vabavq_s, vabavq_u])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VABAVQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vshlcq_u vshlcq_s]
;;
(define_insn "@mve_vshlcq_<supf><mode>"
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
 "vshlc\t%q0, %1, %4"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vshlcq_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vabsq_m_s]
;; [vclsq_m_s]
;; [vclzq_m_s, vclzq_m_u]
;; [vnegq_m_s]
;; [vqabsq_m_s]
;; [vqnegq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_INT_M_UNARY))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<isu>%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddvaq_p_u, vaddvaq_p_s])
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VADDVAQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpcsq_m_n_u])
;; [vcmpeqq_m_n_u, vcmpeqq_m_n_s])
;; [vcmpgeq_m_n_s])
;; [vcmpgtq_m_n_s])
;; [vcmphiq_m_n_u])
;; [vcmpleq_m_n_s])
;; [vcmpltq_m_n_s])
;; [vcmpneq_m_n_u, vcmpneq_m_n_s])
;;
(define_insn "@mve_vcmp<mve_cmp_op1>q_m_n_<supf><mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(unspec:<MVE_VPRED> [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_CMP_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.<isu>%#<V_sz_elem>\t<mve_cmp_op1>, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op1>q_n_<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpcsq_m_u]
;; [vcmpeqq_m_u, vcmpeqq_m_s]
;; [vcmpgeq_m_s]
;; [vcmpgtq_m_s]
;; [vcmphiq_m_u]
;; [vcmpleq_m_s]
;; [vcmpltq_m_s]
;; [vcmpneq_m_s, vcmpneq_m_u]
;;
(define_insn "@mve_vcmp<mve_cmp_op1>q_m_<supf><mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(unspec:<MVE_VPRED> [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_CMP_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vcmpt.<isu>%#<V_sz_elem>\t<mve_cmp_op1>, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op1>q_<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vdupq_m_n_s, vdupq_m_n_u])
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VDUPQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxaq_m_s]
;; [vminaq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_VMAXAVMINAQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.s%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxavq_p_s]
;; [vmaxvq_p_u, vmaxvq_p_s]
;; [vminavq_p_s]
;; [vminvq_p_s, vminvq_p_u]
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_VMAXVQ_VMINVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmladavaq_u, vmladavaq_s]
;; [vmladavaxq_s]
;; [vmlsdavaq_s]
;; [vmlsdavaxq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 MVE_VMLxDAVAQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vmladavq_p_u, vmladavq_p_s]
;; [vmladavxq_p_s]
;; [vmlsdavq_p_s]
;; [vmlsdavxq_p_s]
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_VMLxDAVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaq_n_u, vmlaq_n_s]
;; [vmlasq_n_u, vmlasq_n_s]
;; [vqdmlahq_n_s]
;; [vqdmlashq_n_s]
;; [vqrdmlahq_n_s]
;; [vqrdmlashq_n_s]
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 MVE_VMLxQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmvnq_m_s, vmvnq_m_u])
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VMVNQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vpselq_u, vpselq_s])
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_1 0 "s_register_operand" "=w")
	(unspec:MVE_1 [(match_operand:MVE_1 1 "s_register_operand" "w")
		       (match_operand:MVE_1 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VPSELQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vqdmladhq_s]
;; [vqdmladhxq_s]
;; [vqdmlsdhq_s]
;; [vqdmlsdhxq_s]
;; [vqrdmladhq_s]
;; [vqrdmladhxq_s]
;; [vqrdmlsdhq_s]
;; [vqrdmlsdhxq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")]
	 MVE_VQxDMLxDHxQ_S))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.s%#<V_sz_elem>\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vqrshlq_m_n_s, vqrshlq_m_n_u]
;; [vrshlq_m_n_s, vrshlq_m_n_u]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_RSHIFT_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshlq_m_r_u, vqshlq_m_r_s]
;; [vshlq_m_r_u, vshlq_m_r_s]
;;
(define_insn "@mve_<mve_insn>q_m_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_SHIFT_M_R))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_r_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev64q_m_u, vrev64q_m_s])
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=&w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VREV64Q_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vsliq_n_u, vsliq_n_s])
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred>" "<MVE_constraint>")]
	 VSLIQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vsriq_n_u, vsriq_n_s])
;;
(define_insn "@mve_<mve_insn>q_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")]
	 VSRIQ_N))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vabsq_m_f]
;; [vnegq_m_f]
;; [vrndaq_m_f]
;; [vrndmq_m_f]
;; [vrndnq_m_f]
;; [vrndpq_m_f]
;; [vrndq_m_f]
;; [vrndxq_m_f]
;;
(define_insn "@mve_<mve_insn>q_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_FP_M_UNARY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_mnemo>t.f%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddlvaq_p_s vaddlvaq_p_u])
;;
(define_insn "@mve_<mve_insn>q_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4BI 3 "vpr_register_operand" "Up")]
	 VADDLVAQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>32\t%Q0, %R0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcmlaq, vcmlaq_rot90, vcmlaq_rot180, vcmlaq_rot270])
;;
(define_insn "@mve_<mve_insn>q<mve_rot>_f<mode>"
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
  [(set_attr_alternative "mve_unpredicated_insn" [(symbol_ref "CODE_FOR_mve_<mve_insn>q<mve_rot>_f<mode>")
						  (symbol_ref "CODE_FOR_mve_<mve_insn>q<mve_rot>_f<mode>")])
  (set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_m_n_f])
;; [vcmpgeq_m_n_f])
;; [vcmpgtq_m_n_f])
;; [vcmpleq_m_n_f])
;; [vcmpltq_m_n_f])
;; [vcmpneq_m_n_f])
;;
(define_insn "@mve_vcmp<mve_cmp_op1>q_m_n_f<mode>"
  [
   (set (match_operand:<MVE_VPRED> 0 "vpr_register_operand" "=Up")
	(unspec:<MVE_VPRED> [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_CMP_M_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>\t<mve_cmp_op1>, %q1, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vcmp<mve_cmp_op1>q_n_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtbq_m_f16_f32]
;; [vcvttq_m_f16_f32]
;;
(define_insn "@mve_<mve_insn>q_m_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		       (match_operand:V4SF 2 "s_register_operand" "w")
		       (match_operand:V4BI 3 "vpr_register_operand" "Up")]
	 VCVTxQ_M_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f16.f32\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f16_f32v8hf"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtbq_m_f32_f16]
;; [vcvttq_m_f32_f16]
;;
(define_insn "@mve_<mve_insn>q_m_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V4SF 1 "s_register_operand" "0")
		       (match_operand:V8HF 2 "s_register_operand" "w")
		       (match_operand:V8BI 3 "vpr_register_operand" "Up")]
	 VCVTxQ_M_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f32.f16\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f32_f16v4sf"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vdupq_m_n_f])
;;
(define_insn "@mve_<mve_insn>q_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_FP_M_N_VDUPQ_ONLY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vfmaq_f]
;; [vfmsq_f]
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")]
	 MVE_VFMxQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vfmaq_n_f]
;; [vfmasq_n_f]
;;
(define_insn "@mve_<mve_insn>q_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")]
	 MVE_VFMAxQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>.f%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vmaxnmaq_m_f]
;; [vminnmaq_m_f]
;;
(define_insn "@mve_<mve_insn>q_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_VMAXNMA_VMINNMAQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmaxnmavq_p_f]
;; [vmaxnmvq_p_f]
;; [vminnmavq_p_f]
;; [vminnmvq_p_f]
;;
(define_insn "@mve_<mve_insn>q_p_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_VMAXNMxV_MINNMxVQ_P))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>\t%0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaldavaq_s, vmlaldavaq_u]
;; [vmlaldavaxq_s]
;; [vmlsldavaq_s]
;; [vmlsldavaxq_s]
;;
(define_insn "@mve_<mve_insn>q_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")]
	 MVE_VMLxLDAVAxQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.<supf>%#<V_sz_elem>\t%Q0, %R0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "mve_safe_imp_xlane_pred" "yes")
  (set_attr "type" "mve_move")
])

;;
;; [vmlaldavq_p_u, vmlaldavq_p_s]
;; [vmlaldavxq_p_s]
;; [vmlsldavq_p_s]
;; [vmlsldavxq_p_s]
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_VMLxLDAVxQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%Q0, %R0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmovlbq_m_u, vmovlbq_m_s])
;; [vmovltq_m_u, vmovltq_m_s])
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VMOVLxQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmovnbq_m_u, vmovnbq_m_s]
;; [vmovntq_m_u, vmovntq_m_s]
;; [vqmovnbq_m_s, vqmovnbq_m_u]
;; [vqmovntq_m_u, vqmovntq_m_s]
;; [vqmovunbq_m_s]
;; [vqmovuntq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_MOVN_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<isu>%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmvnq_m_n_u, vmvnq_m_n_s])
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VMVNQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.i%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vbicq_m_n_s, vbicq_m_n_u]
;; [vorrq_m_n_s, vorrq_m_n_u]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_INT_M_N_BINARY_LOGIC))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.i%#<V_sz_elem>\t%q0, %2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vpselq_f])
;;
(define_insn "@mve_<mve_insn>q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_VPSELQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "<mve_insn>\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
])

;;
;; [vrev32q_m_f])
;;
(define_insn "@mve_<mve_insn>q_m_f<mode>"
  [
   (set (match_operand:MVE_V8HF 0 "s_register_operand" "=w")
	(unspec:MVE_V8HF [(match_operand:MVE_V8HF 1 "s_register_operand" "0")
			  (match_operand:MVE_V8HF 2 "s_register_operand" "w")
			  (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_FP_M_VREV32Q_ONLY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev32q_m_s, vrev32q_m_u])
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_3 0 "s_register_operand" "=w")
	(unspec:MVE_3 [(match_operand:MVE_3 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VREV32Q_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev64q_m_f])
;;
(define_insn "@mve_<mve_insn>q_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=&w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 MVE_FP_M_VREV64Q_ONLY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlaldavhq_p_u vrmlaldavhq_p_s]
;; [vrmlaldavhxq_p_s]
;; [vrmlsldavhq_p_s]
;; [vrmlsldavhxq_p_s]
;;
(define_insn "@mve_<mve_insn>q_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4BI 3 "vpr_register_operand" "Up")]
	 MVE_VRMLxLDAVHxQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>32\t%Q0, %R0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])


;;
;; [vcvtq_m_n_from_f_s, vcvtq_m_n_from_f_u]
;;
(define_insn "@mve_<mve_insn>q_m_n_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VCVTQ_M_N_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_from_f_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrev16q_m_u, vrev16q_m_s])
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_V16QI 0 "s_register_operand" "=w")
	(unspec:MVE_V16QI [(match_operand:MVE_V16QI 1 "s_register_operand" "0")
			   (match_operand:MVE_V16QI 2 "s_register_operand" "w")
			   (match_operand:V16BI 3 "vpr_register_operand" "Up")]
	 VREV16Q_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtq_m_from_f_u, vcvtq_m_from_f_s]
;;
(define_insn "@mve_<mve_insn>q_m_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VCVTQ_M_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_from_f_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length""8")])

;;
;; [vabavq_p_s, vabavq_p_u])
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")
		    (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VABAVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length" "8")])

;;
;; [vqshluq_m_n_s])
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred>" "<MVE_constraint>")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VQSHLUQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\n\t<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length" "8")])

;;
;; [vsriq_m_n_s, vsriq_m_n_u])
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VSRIQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length" "8")])

;;
;; [vcvtq_m_n_to_f_u, vcvtq_m_n_to_f_s]
;;
(define_insn "@mve_<mve_insn>q_m_n_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VCVTQ_M_N_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_to_f_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vabdq_m_s, vabdq_m_u]
;; [vhaddq_m_s, vhaddq_m_u]
;; [vhsubq_m_s, vhsubq_m_u]
;; [vmaxq_m_s, vmaxq_m_u]
;; [vminq_m_s, vminq_m_u]
;; [vmulhq_m_s, vmulhq_m_u]
;; [vqaddq_m_u, vqaddq_m_s]
;; [vqdmladhq_m_s]
;; [vqdmladhxq_m_s]
;; [vqdmlsdhq_m_s]
;; [vqdmlsdhxq_m_s]
;; [vqdmulhq_m_s]
;; [vqrdmladhq_m_s]
;; [vqrdmladhxq_m_s]
;; [vqrdmlsdhq_m_s]
;; [vqrdmlsdhxq_m_s]
;; [vqrdmulhq_m_s]
;; [vqrshlq_m_u, vqrshlq_m_s]
;; [vqshlq_m_u, vqshlq_m_s]
;; [vqsubq_m_u, vqsubq_m_s]
;; [vrhaddq_m_u, vrhaddq_m_s]
;; [vrmulhq_m_u, vrmulhq_m_s]
;; [vrshlq_m_s, vrshlq_m_u]
;; [vshlq_m_s, vshlq_m_u]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_INT_SU_M_BINARY))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddq_m_n_s, vaddq_m_n_u]
;; [vsubq_m_n_s, vsubq_m_n_u]
;; [vmulq_m_n_s, vmulq_m_n_u]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_INT_M_N_BINARY))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.i%#<V_sz_elem>	%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length""8")])

;;
;; [vaddq_m_u, vaddq_m_s]
;; [vsubq_m_u, vsubq_m_s]
;; [vmulq_m_u, vmulq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_INT_M_BINARY))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.i%#<V_sz_elem>\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vandq_m_u, vandq_m_s]
;; [vbicq_m_u, vbicq_m_s]
;; [veorq_m_u, veorq_m_s]
;; [vornq_m_u, vornq_m_s]
;; [vorrq_m_u, vorrq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_INT_M_BINARY_LOGIC))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vbrsrq_m_n_u, vbrsrq_m_n_s])
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VBRSRQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length""8")])

;;
;; [vcaddq_rot90_m_u, vcaddq_rot90_m_s]
;; [vcaddq_rot270_m_u, vcaddq_rot270_m_s]
;; [vhcaddq_rot90_m_s]
;; [vhcaddq_rot270_m_s]
;;
(define_insn "@mve_<mve_insn>q<mve_rot>_m_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:MVE_2 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VxCADDQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<isu>%#<V_sz_elem>\t%q0, %q2, %q3, #<rot>"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q<mve_rot>_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vhaddq_m_n_s, vhaddq_m_n_u]
;; [vhsubq_m_n_s, vhsubq_m_n_u]
;; [vmlaq_m_n_s, vmlaq_m_n_u]
;; [vmlasq_m_n_u, vmlasq_m_n_s]
;; [vqaddq_m_n_u, vqaddq_m_n_s]
;; [vqdmlahq_m_n_s]
;; [vqdmlashq_m_n_s]
;; [vqdmulhq_m_n_s]
;; [vqrdmlahq_m_n_s]
;; [vqrdmlashq_m_n_s]
;; [vqrdmulhq_m_n_s]
;; [vqsubq_m_n_u, vqsubq_m_n_s]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_INT_SU_M_N_BINARY))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;;
;; [vmladavaq_p_u, vmladavaq_p_s]
;; [vmladavaxq_p_s]
;; [vmlsdavaq_p_s]
;; [vmlsdavaxq_p_s]
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=Te")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")
		    (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VMLxDAVAQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmullbq_int_m_u, vmullbq_int_m_s]
;; [vmulltq_int_m_s, vmulltq_int_m_u]
;;
(define_insn "@mve_<mve_insn>q_int_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
				  (match_operand:MVE_2 2 "s_register_operand" "w")
				  (match_operand:MVE_2 3 "s_register_operand" "w")
				  (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VMULLxQ_INT_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_int_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqshlq_m_n_s, vqshlq_m_n_u]
;; [vshlq_m_n_s, vshlq_m_n_u]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_SHIFT_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrshrq_m_n_s, vrshrq_m_n_u])
;; [vshrq_m_n_s, vshrq_m_n_u])
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred2>" "<MVE_constraint2>")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VSHRQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vsliq_m_n_u, vsliq_m_n_s])
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
   [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")
		       (match_operand:SI 3 "<MVE_pred>" "<MVE_constraint>")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VSLIQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmlaldavaq_p_u, vmlaldavaq_p_s]
;; [vmlaldavaxq_p_s]
;; [vmlsldavaq_p_s]
;; [vmlsldavaxq_p_s]
;;
(define_insn "@mve_<mve_insn>q_p_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VMLxLDAVAxQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%Q0, %R0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqrshrnbq_m_n_u, vqrshrnbq_m_n_s]
;; [vqrshrntq_m_n_s, vqrshrntq_m_n_u]
;; [vqrshrunbq_m_n_s]
;; [vqrshruntq_m_n_s]
;; [vqshrnbq_m_n_u, vqshrnbq_m_n_s]
;; [vqshrntq_m_n_s, vqshrntq_m_n_u]
;; [vqshrunbq_m_n_s]
;; [vqshruntq_m_n_s]
;; [vrshrnbq_m_n_u, vrshrnbq_m_n_s]
;; [vrshrntq_m_n_u, vrshrntq_m_n_s]
;; [vshrnbq_m_n_s, vshrnbq_m_n_u]
;; [vshrntq_m_n_s, vshrntq_m_n_u]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")
				 (match_operand:SI 3 "<MVE_pred3>" "<MVE_constraint3>")
				 (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_SHRN_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<isu>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vrmlaldavhaq_p_s, vrmlaldavhaq_p_u]
;; [vrmlaldavhaxq_p_s]
;; [vrmlsldavhaq_p_s]
;; [vrmlsldavhaxq_p_s]
;;
(define_insn "@mve_<mve_insn>q_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		       (match_operand:V4SI 2 "s_register_operand" "w")
		       (match_operand:V4SI 3 "s_register_operand" "w")
		       (match_operand:V4BI 4 "vpr_register_operand" "Up")]
	 MVE_VRMLxLDAVHAxQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>32\t%Q0, %R0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshllbq_m_n_u, vshllbq_m_n_s]
;; [vshlltq_m_n_u, vshlltq_m_n_s]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VSHLLxQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vmullbq_poly_m_p]
;; [vmulltq_poly_m_p]
;;
(define_insn "@mve_<mve_insn>q_poly_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_3 2 "s_register_operand" "w")
		       (match_operand:MVE_3 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 VMULLxQ_POLY_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.<supf>%#<V_sz_elem>\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_poly_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmullbq_m_n_s]
;; [vqdmulltq_m_n_s]
;;
(define_insn "@mve_<mve_insn>q_m_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VQDMULLxQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.s%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vqdmullbq_m_s]
;; [vqdmulltq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
		       (match_operand:MVE_5 2 "s_register_operand" "w")
		       (match_operand:MVE_5 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VQDMULLxQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.s%#<V_sz_elem>\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf><mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vabdq_m_f]
;; [vaddq_m_f]
;; [vfmaq_m_f]
;; [vfmsq_m_f]
;; [vmaxnmq_m_f]
;; [vminnmq_m_f]
;; [vmulq_m_f]
;; [vsubq_m_f]
;;
(define_insn "@mve_<mve_insn>q_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_FP_M_BINARY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>	%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vaddq_m_n_f]
;; [vsubq_m_n_f]
;; [vmulq_m_n_f]
;; [vfmaq_m_n_f]
;; [vfmasq_m_n_f]
;;
(define_insn "@mve_<mve_insn>q_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:<V_elem> 3 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_FP_M_N_BINARY))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vandq_m_f]
;; [vbicq_m_f]
;; [veorq_m_f]
;; [vornq_m_f]
;; [vorrq_m_f]
;;
(define_insn "@mve_<mve_insn>q_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_FP_M_BINARY_LOGIC))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vbrsrq_m_n_f])
;;
(define_insn "@mve_<mve_insn>q_m_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:SI 3 "s_register_operand" "r")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VBRSR_M_N_FP))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.%#<V_sz_elem>\t%q0, %q2, %3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_n_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcaddq_rot90_m_f]
;; [vcaddq_rot270_m_f]
;; [vcmulq_m_f]
;; [vcmulq_rot90_m_f]
;; [vcmulq_rot180_m_f]
;; [vcmulq_rot270_m_f]
;;
(define_insn "@mve_<mve_insn>q<mve_rot>_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "<earlyclobber_32>")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VCADDQ_VCMULQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>\t%q0, %q2, %q3, #<rot>"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q<mve_rot>_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmlaq_m_f]
;; [vcmlaq_rot90_m_f]
;; [vcmlaq_rot180_m_f]
;; [vcmlaq_rot270_m_f]
;;
(define_insn "@mve_<mve_insn>q<mve_rot>_m_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")
		       (match_operand:MVE_0 3 "s_register_operand" "w")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	 MVE_VCMLAQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;<mve_insn>t.f%#<V_sz_elem>\t%q0, %q2, %q3, #<rot>"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q<mve_rot>_f<mode>"))
  (set_attr "type" "mve_move")
   (set_attr "length""8")])

;; Vector stores
;; [vstrbq_s8, vstrhq_s16, vstrwq_s32,
;;  vstrbq_u8, vstrhq_u16, vstrwq_u32,
;;  vst1q ]
(define_insn "@mve_vstrq_<mode>"
  [(set (match_operand:MVE_VLD_ST 0 "mve_memory_operand" "=Ux")
	(unspec:MVE_VLD_ST
	  [(match_operand:MVE_VLD_ST 1 "s_register_operand" "w")]
	  VSTRQ))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  rtx ops[2];
  int regno = REGNO (operands[1]);
  ops[1] = gen_rtx_REG (TImode, regno);
  ops[0]  = operands[0];
  output_asm_insn ("vstr<MVE_elem_ch>.<V_sz_elem>\t%q1, %E0",ops);
  return "";
}
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_<mode>"))
  (set_attr "length" "4")])

;; Predicated vector stores
;; [vstrbq_p_s8, vstrhq_p_s16, vstrwq_p_s32,
;;  vstrbq_p_u8, vstrhq_p_u16, vstrwq_p_u32,
;;  vst1q_p ]
(define_insn "@mve_vstrq_p_<mode>"
  [(set (match_operand:MVE_VLD_ST 0 "mve_memory_operand" "=Ux")
	(unspec:MVE_VLD_ST [
	   (match_operand:MVE_VLD_ST 1 "s_register_operand" "w")
	   (match_operand:<MVE_VPRED> 2 "vpr_register_operand" "Up")
	   (match_dup 0)
	] VSTRQ_P))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  rtx ops[2];
  int regno = REGNO (operands[1]);
  ops[1] = gen_rtx_REG (TImode, regno);
  ops[0]  = operands[0];
  output_asm_insn ("vpst\;vstr<MVE_elem_ch>t.<V_sz_elem>\t%q1, %E0",ops);
  return "";
}
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_<mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length" "8")])

;; Truncating vector stores
;; [vstrbq_s16, vstrbq_s32, vstrhq_s32,
;;  vstrbq_u16, vstrbq_u32, vstrhq_u32]
(define_insn "@mve_vstrq_truncate_<mode>"
  [(set (match_operand:MVE_w_narrow_TYPE 0 "mve_memory_operand" "=Ux")
	(unspec:MVE_w_narrow_TYPE
	  [(truncate:MVE_w_narrow_TYPE
	    (match_operand:<MVE_wide_n_TYPE> 1 "s_register_operand" "w"))]
	  VSTRQ_TRUNC
	))]
  "TARGET_HAVE_MVE"
{
  rtx ops[2];
  int regno = REGNO (operands[1]);
  ops[1] = gen_rtx_REG (TImode, regno);
  ops[0]  = operands[0];
  output_asm_insn ("vstr<MVE_elem_ch>.<MVE_wide_n_sz_elem>\t%q1, %E0",ops);
  return "";
}
  [(set (attr "mve_unpredicated_insn")
	(symbol_ref "CODE_FOR_mve_vstrq_truncate_<mode>"))
   (set_attr "length" "4")])

;; Predicated truncating vector stores
;; [vstrbq_p_s16, vstrbq_p_s32, vstrhq_p_s32,
;;  vstrbq_p_u16, vstrbq_p_u32, vstrhq_p_u32]
(define_insn "@mve_vstrq_p_truncate_<mode>"
  [(set (match_operand:MVE_w_narrow_TYPE 0 "mve_memory_operand" "=Ux")
	(unspec:MVE_w_narrow_TYPE [
	  (truncate:MVE_w_narrow_TYPE
	    (match_operand:<MVE_wide_n_TYPE> 1 "s_register_operand" "w"))
	  (match_operand:<MVE_wide_n_VPRED> 2 "vpr_register_operand" "Up")
	  (match_dup 0)
	] VSTRQ_TRUNC_P))]
  "TARGET_HAVE_MVE"
{
  rtx ops[2];
  int regno = REGNO (operands[1]);
  ops[1] = gen_rtx_REG (TImode, regno);
  ops[0]  = operands[0];
  output_asm_insn (
    "vpst\;vstr<MVE_elem_ch>t.<MVE_wide_n_sz_elem>\t%q1, %E0",
    ops
  );
  return "";
}
 [(set (attr "mve_unpredicated_insn")
       (symbol_ref "CODE_FOR_mve_vstrq_truncate_<mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length" "8")])

;; Vector Loads
;; [vldrbq_s8, vldrhq_s16, vldrwq_s32,
;;  vldrbq_u8, vldrhq_u16, vldrwq_u32,
;;  vld1q ]
(define_insn "@mve_vldrq_<mode>"
  [(set (match_operand:MVE_VLD_ST 0 "s_register_operand" "=w")
	(unspec:MVE_VLD_ST
	  [(match_operand:MVE_VLD_ST 1 "mve_memory_operand" "Ux")]
	  VLDRQ))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  rtx ops[2];
  int regno = REGNO (operands[0]);
  ops[0] = gen_rtx_REG (TImode, regno);
  ops[1]  = operands[1];
  output_asm_insn ("vldr<MVE_elem_ch>.<V_sz_elem>\t%q0, %E1",ops);
  return "";
 }
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_<mode>"))
  (set_attr "length" "4")])

;; Predicated vector loads
;; [vldrbq_z_s8, vldrhq_z_s16, vldrwq_z_s32,
;;  vldrbq_z_u8, vldrhq_z_u16, vldrwq_z_u32,
;;  vld1q_z ]
(define_insn "@mve_vldrq_z_<mode>"
  [(set (match_operand:MVE_VLD_ST 0 "s_register_operand" "=w")
	(unspec:MVE_VLD_ST [
	   (match_operand:MVE_VLD_ST 1 "mve_memory_operand" "Ux")
	   (match_operand:<MVE_VPRED> 2 "vpr_register_operand" "Up")
	] VLDRQ_Z))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
{
  rtx ops[2];
  int regno = REGNO (operands[0]);
  ops[0] = gen_rtx_REG (TImode, regno);
  ops[1]  = operands[1];
  output_asm_insn ("vpst\;vldr<MVE_elem_ch>t.<V_sz_elem>\t%q0, %E1",ops);
  return "";
}
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_<mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length" "8")])

;; Extending vector loads
;; [vldrbq_s16, vldrbq_s32, vldrhq_s32,
;;  vldrbq_u16, vldrbq_u32, vldrhq_u32]
(define_insn "@mve_vldrq_extend_<mode><US>"
  [(set (match_operand:<MVE_wide_n_TYPE> 0 "s_register_operand" "=w")
	(unspec:<MVE_wide_n_TYPE>
	  [(SE:<MVE_wide_n_TYPE>
	    (match_operand:MVE_w_narrow_TYPE 1 "mve_memory_operand" "Ux"))]
	  VLDRQ_EXT))]
  "TARGET_HAVE_MVE"
{
  rtx ops[2];
  int regno = REGNO (operands[0]);
  ops[0] = gen_rtx_REG (TImode, regno);
  ops[1]  = operands[1];
  output_asm_insn ("vldr<MVE_elem_ch>.<US><MVE_wide_n_sz_elem>\t%q0, %E1",ops);
  return "";
}
 [(set (attr "mve_unpredicated_insn")
       (symbol_ref "CODE_FOR_mve_vldrq_extend_<mode><US>"))
  (set_attr "length" "4")])

;; Predicated extending vector loads
;; [vldrbq_z_s16, vldrbq_z_s32, vldrhq_z_s32,
;;  vldrbq_z_u16, vldrbq_z_u32, vldrhq_z_u32]
(define_insn "@mve_vldrq_z_extend_<mode><US>"
  [(set (match_operand:<MVE_wide_n_TYPE> 0 "s_register_operand" "=w")
	  (unspec:<MVE_wide_n_TYPE> [
	      (SE:<MVE_wide_n_TYPE>
		(match_operand:MVE_w_narrow_TYPE 1 "mve_memory_operand" "Ux"))
	      (match_operand:<MVE_wide_n_VPRED> 2 "vpr_register_operand" "Up")
	  ] VLDRQ_EXT_Z))]
  "TARGET_HAVE_MVE"
{
  rtx ops[2];
  int regno = REGNO (operands[0]);
  ops[0] = gen_rtx_REG (TImode, regno);
  ops[1]  = operands[1];
  output_asm_insn (
    "vpst\;vldr<MVE_elem_ch>t.<US><MVE_wide_n_sz_elem>\t%q0, %E1",
    ops
  );
  return "";
}
 [(set (attr "mve_unpredicated_insn")
       (symbol_ref "CODE_FOR_mve_vldrq_extend_<mode><US>"))
  (set_attr "type" "mve_move")
  (set_attr "length" "8")])

;; Vector scatter stores with offset
;;
;; [vstrbq_scatter_offset_s8,  vstrbq_scatter_offset_u8,
;;  vstrhq_scatter_offset_s16, vstrhq_scatter_offset_u16,
;;  vstrwq_scatter_offset_s32, vstrwq_scatter_offset_u32,
;;  vstrdq_scatter_offset_s64, vstrdq_scatter_offset_u64,
;;  vstrhq_scatter_offset_f16,
;;  vstrwq_scatter_offset_f32]
;;
(define_insn "@mve_vstrq_scatter_offset_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:<MVE_scatter_offset> 1 "s_register_operand" "w")
	   (match_operand:MVE_VLD_ST_scatter 2 "s_register_operand" "w")]
	  VSTRQSO))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vstr<MVE_elem_ch>.<V_sz_elem>\t%q2, [%0, %q1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_offset_<mode>"))
  (set_attr "length" "4")])

;; Predicated vector scatter stores with offset
;;
;; [vstrbq_scatter_offset_p_s8, vstrbq_scatter_offset_p_u8,
;; [vstrhq_scatter_offset_p_s16, vstrhq_scatter_offset_p_u16,
;; [vstrwq_scatter_offset_p_s32, vstrwq_scatter_offset_p_u32,
;; [vstrdq_scatter_offset_p_s64, vstrdq_scatter_offset_p_u64,
;; [vstrhq_scatter_offset_p_f16,
;; [vstrwq_scatter_offset_p_f32]
;;
(define_insn "@mve_vstrq_scatter_offset_p_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:<MVE_scatter_offset> 1 "s_register_operand" "w")
	   (match_operand:MVE_VLD_ST_scatter 2 "s_register_operand" "w")
	   (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	  VSTRQSO_P))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\;vstr<MVE_elem_ch>t.<V_sz_elem>\t%q2, [%0, %q1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_offset_<mode>"))
  (set_attr "length" "8")])

;; Truncating vector scatter stores with offset
;;
;; [vstrbq_scatter_offset_s16, vstrbq_scatter_offset_u16,
;; [vstrbq_scatter_offset_s32, vstrbq_scatter_offset_u32,
;; [vstrhq_scatter_offset_s32, vstrhq_scatter_offset_u32]
;;
(define_insn "@mve_vstrq_truncate_scatter_offset_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:<MVE_wide_n_TYPE> 1 "s_register_operand" "w")
	   (truncate:MVE_w_narrow_TYPE
	     (match_operand:<MVE_wide_n_TYPE> 2 "s_register_operand" "w"))]
	  VSTRQSO_TRUNC))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MVE_wide_n_TYPE>mode))"
  "vstr<MVE_elem_ch>.<MVE_wide_n_sz_elem>\t%q2, [%0, %q1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_truncate_scatter_offset_<mode>"))
  (set_attr "length" "4")])


;; Predicated truncating vector scatter stores with offset
;;
;; [vstrbq_scatter_offset_p_s16, vstrbq_scatter_offset_p_u16,
;; [vstrbq_scatter_offset_p_s32, vstrbq_scatter_offset_p_u32,
;; [vstrhq_scatter_offset_p_s32, vstrhq_scatter_offset_p_u32]
;;
(define_insn "@mve_vstrq_truncate_scatter_offset_p_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:<MVE_wide_n_TYPE> 1 "s_register_operand" "w")
	   (truncate:MVE_w_narrow_TYPE
	     (match_operand:<MVE_wide_n_TYPE> 2 "s_register_operand" "w"))
	   (match_operand:<MVE_wide_n_VPRED> 3 "vpr_register_operand" "Up")]
	  VSTRQSO_TRUNC_P))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MVE_wide_n_TYPE>mode))"
  "vpst\;vstr<MVE_elem_ch>t.<MVE_wide_n_sz_elem>\t%q2, [%0, %q1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_truncate_scatter_offset_<mode>"))
  (set_attr "length" "8")])

;; Vector scatter stores with base
;;
;; [vstrdq_scatter_base_s vstrdq_scatter_base_u]
;; [vstrwq_scatter_base_s vstrwq_scatter_base_u]
;; [vstrwq_scatter_base_f]
;;
(define_insn "@mve_vstrq_scatter_base_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:<MVE_scatter_offset> 0 "s_register_operand" "w")
		 (match_operand:SI 1 "immediate_operand" "i")
		 (match_operand:MVE_4 2 "s_register_operand" "w")]
	 VSTRSBQ))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vstr<MVE_elem_ch>.u<V_sz_elem>\t%q2, [%q0, %1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_base_<mode>"))
  (set_attr "length" "4")])

;; Vector gather loads with offset
;;
;; [vldrbq_gather_offset_s vldrbq_gather_offset_u]
;; [vldrhq_gather_offset_s vldrhq_gather_offset_u]
;; [vldrhq_gather_offset_f]
;; [vldrwq_gather_offset_s vldrwq_gather_offset_u]
;; [vldrwq_gather_offset_f]
;; [vldrdq_gather_offset_s vldrdq_gather_offset_u]
;;
(define_insn "@mve_vldrq_gather_offset_<mode>"
  [(set (match_operand:MVE_VLD_ST_scatter 0 "s_register_operand" "=&w")
	(unspec:MVE_VLD_ST_scatter
	    [(match_operand:SI 1 "register_operand" "r")
	     (match_operand:<MVE_scatter_offset> 2 "s_register_operand" "w")
	     (mem:BLK (scratch))]
	 VLDRGOQ))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vldr<MVE_elem_ch>.<MVE_u_elem>\t%q0, [%1, %q2]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_offset_<mode>"))
  (set_attr "length" "4")])

;; Extending vector gather loads with offset
;;
;; [vldrbq_gather_offset_s vldrbq_gather_offset_u]
;; [vldrhq_gather_offset_s vldrhq_gather_offset_u]
;;
(define_insn "@mve_vldrq_gather_offset_extend_<mode><US>"
  [(set (match_operand:<MVE_wide_n_TYPE> 0 "s_register_operand" "=&w")
	(SE:<MVE_wide_n_TYPE>
	  (unspec:MVE_w_narrow_TYPE
	    [(match_operand:SI 1 "register_operand" "r")
	     (match_operand:<MVE_wide_n_TYPE> 2 "s_register_operand" "w")
	     (mem:BLK (scratch))]
	   VLDRGOQ_EXT)))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MVE_wide_n_TYPE>mode))"
  "vldr<MVE_elem_ch>.<US><MVE_wide_n_sz_elem>\t%q0, [%1, %q2]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_offset_extend_<mode><US>"))
  (set_attr "length" "4")])

;; Predicated gather loads with offset
;;
;; [vldrbq_gather_offset_z_s vldrbq_gather_offset_z_u]
;; [vldrhq_gather_offset_z_s vldrhq_gather_offset_z_u]
;; [vldrhq_gather_offset_z_f]
;; [vldrwq_gather_offset_z_s vldrwq_gather_offset_z_u]
;; [vldrwq_gather_offset_z_f]
;; [vldrdq_gather_offset_z_s vldrdq_gather_offset_z_u]
;;
(define_insn "@mve_vldrq_gather_offset_z_<mode>"
  [(set (match_operand:MVE_VLD_ST_scatter 0 "s_register_operand" "=&w")
	(unspec:MVE_VLD_ST_scatter
	    [(match_operand:SI 1 "register_operand" "r")
	     (match_operand:<MVE_scatter_offset> 2 "s_register_operand" "w")
	     (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")
	     (mem:BLK (scratch))]
	 VLDRGOQ_Z))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\n\tvldr<MVE_elem_ch>t.<MVE_u_elem>\t%q0, [%1, %q2]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_offset_<mode>"))
  (set_attr "length" "8")])

;; Predicated extending gather loads with offset
;;
;; [vldrbq_gather_offset_z_s vldrbq_gather_offset_z_u]
;; [vldrhq_gather_offset_z_s vldrhq_gather_offset_z_u]
;;
(define_insn "@mve_vldrq_gather_offset_z_extend_<mode><US>"
  [(set (match_operand:<MVE_wide_n_TYPE> 0 "s_register_operand" "=&w")
	(SE:<MVE_wide_n_TYPE>
	   (unspec:MVE_w_narrow_TYPE
	     [(match_operand:SI 1 "register_operand" "r")
	      (match_operand:<MVE_wide_n_TYPE> 2 "s_register_operand" "w")
	      (match_operand:<MVE_wide_n_VPRED> 3 "vpr_register_operand" "Up")
	      (mem:BLK (scratch))]
	VLDRGOQ_EXT_Z)))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MVE_wide_n_TYPE>mode))"
  "vpst\n\tvldr<MVE_elem_ch>t.<US><MVE_wide_n_sz_elem>\t%q0, [%1, %q2]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_offset_extend_<mode><US>"))
  (set_attr "length" "8")])

;; Predicated vector scatter stores with base
;;
;; [vstrdq_scatter_base_p_s vstrdq_scatter_base_p_u]
;; [vstrwq_scatter_base_p_s vstrwq_scatter_base_p_u]
;; [vstrwq_scatter_base_p_f]
;;
(define_insn "@mve_vstrq_scatter_base_p_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:<MVE_scatter_offset> 0 "s_register_operand" "w")
		 (match_operand:SI 1 "immediate_operand" "i")
		 (match_operand:MVE_4 2 "s_register_operand" "w")
		 (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VSTRSBQ_P))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\n\tvstr<MVE_elem_ch>t.u<V_sz_elem>\t%q2, [%q0, %1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_base_<mode>"))
  (set_attr "length" "8")])

;; Vector gather loads with base
;;
;; [vldrwq_gather_base_s vldrwq_gather_base_u]
;; [vldrwq_gather_base_f]
;; [vldrdq_gather_base_s vldrdq_gather_base_u]
;;
(define_insn "@mve_vldrq_gather_base_<mode>"
  [(set (match_operand:MVE_4 0 "s_register_operand" "=&w")
	(unspec:MVE_4 [(match_operand:<MVE_scatter_offset> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VLDRGBQ))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vldr<MVE_elem_ch>.u<V_sz_elem>\t%q0, [%q1, %2]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_base_<mode>"))
  (set_attr "length" "4")])

;; Predicated vector gather loads with base
;;
;; [vldrwq_gather_base_z_s vldrwq_gather_base_z_u]
;; [vldrwq_gather_base_z_f]
;; [vldrdq_gather_base_z_s vldrdq_gather_base_z_u]
;;
(define_insn "@mve_vldrq_gather_base_z_<mode>"
  [(set (match_operand:MVE_4 0 "s_register_operand" "=&w")
	(unspec:MVE_4 [(match_operand:<MVE_scatter_offset> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
		       (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	 VLDRGBQ_Z))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\n\tvldr<MVE_elem_ch>t.u<V_sz_elem>\t%q0, [%q1, %2]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_base_<mode>"))
  (set_attr "length" "8")])


;; Gather loads with shifted offset
;;
;; [vldrhq_gather_shifted_offset_s vldrhq_gather_shifted_offset_u]
;; [vldrhq_gather_shifted_offset_f]
;; [vldrwq_gather_shifted_offset_s vldrwq_gather_shifted_offset_u]
;; [vldrwq_gather_shifted_offset_f]
;; [vldrdq_gather_shifted_offset_s vldrdq_gather_shifted_offset_u]
;;
(define_insn "@mve_vldrq_gather_shifted_offset_<mode>"
  [(set (match_operand:MVE_VLD_ST_scatter_shifted 0 "s_register_operand" "=&w")
	(unspec:MVE_VLD_ST_scatter_shifted
		[(match_operand:SI 1 "register_operand" "r")
		 (match_operand:<MVE_scatter_offset> 2 "s_register_operand" "w")
		 (mem:BLK (scratch))]
	VLDRGSOQ))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vldr<MVE_elem_ch>.<MVE_u_elem>\t%q0, [%1, %q2, uxtw #<MVE_scatter_shift>]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_shifted_offset_<mode>"))
  (set_attr "length" "4")])

;; Extending gather loads with shifted offset
;;
;; [vldrhq_gather_shifted_offset_s vldrhq_gather_shifted_offset_u]
;;
(define_insn "@mve_vldrq_gather_shifted_offset_extend_v4si<US>"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(SE:V4SI
	  (unspec:V4HI
	    [(match_operand:SI 1 "register_operand" "r")
	     (match_operand:V4SI 2 "s_register_operand" "w")
	     (mem:BLK (scratch))]
	   VLDRGSOQ_EXT)))
  ]
  "TARGET_HAVE_MVE"
  "vldrh.<US>32\t%q0, [%1, %q2, uxtw #1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_shifted_offset_extend_v4si<US>"))
  (set_attr "length" "4")])

;; Predicated gather loads with shifted offset
;;
;; [vldrhq_gather_shifted_offset_z_s vldrhq_gather_shited_offset_z_u]
;; [vldrhq_gather_shifted_offset_z_f]
;; [vldrwq_gather_shifted_offset_z_s vldrwq_gather_shifted_offset_z_u]
;; [vldrwq_gather_shifted_offset_z_f]
;; [vldrdq_gather_shifted_offset_z_s vldrdq_gather_shifted_offset_z_u]
;;
(define_insn "@mve_vldrq_gather_shifted_offset_z_<mode>"
  [(set (match_operand:MVE_VLD_ST_scatter_shifted 0 "s_register_operand" "=&w")
	(unspec:MVE_VLD_ST_scatter_shifted
		[(match_operand:SI 1 "register_operand" "r")
		 (match_operand:<MVE_scatter_offset> 2 "s_register_operand" "w")
		 (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")
		 (mem:BLK (scratch))]
	VLDRGSOQ_Z))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\n\tvldr<MVE_elem_ch>t.<MVE_u_elem>\t%q0, [%1, %q2, uxtw #<MVE_scatter_shift>]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_shifted_offset_<mode>"))
  (set_attr "length" "8")])

;; Predicated extending gather loads with shifted offset
;;
;; [vldrhq_gather_shifted_offset_z_s vldrhq_gather_shifted_offset_z_u]
;;
(define_insn "@mve_vldrq_gather_shifted_offset_z_extend_v4si<US>"
  [(set (match_operand:V4SI 0 "s_register_operand" "=&w")
	(SE:V4SI
	  (unspec:V4HI
	    [(match_operand:SI 1 "register_operand" "r")
	     (match_operand:V4SI 2 "s_register_operand" "w")
	     (match_operand:V4BI 3 "vpr_register_operand" "Up")
	     (mem:BLK (scratch))]
	   VLDRGSOQ_EXT_Z)))
  ]
  "TARGET_HAVE_MVE"
  "vpst\n\tvldrht.<US>32\t%q0, [%1, %q2, uxtw #1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_shifted_offset_extend_v4si<US>"))
  (set_attr "length" "4")])

;; Vector scatter stores with shifted offset
;;
;; [vstrhq_scatter_shifted_offset_s vstrhq_scatter_shifted_offset_u]
;; [vstrhq_scatter_shifted_offset_f]
;; [vstrwq_scatter_shifted_offset_s vstrwq_scatter_shifted_offset_u]
;; [vstrwq_scatter_shifted_offset_f]
;; [vstrdq_scatter_shifted_offset_s vstrdq_scatter_shifted_offset_u]
(define_insn "@mve_vstrq_scatter_shifted_offset_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:<MVE_scatter_offset> 1 "s_register_operand" "w")
	   (match_operand:MVE_VLD_ST_scatter_shifted 2 "s_register_operand" "w")]
	  VSTRSSOQ))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vstr<MVE_elem_ch>.<V_sz_elem>\t%q2, [%0, %q1, uxtw #<MVE_scatter_shift>]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_shifted_offset_<mode>"))
  (set_attr "length" "4")])

;; Truncating vector scatter stores with shifted offset
;;
;; [vstrhq_scatter_shifted_offset_s32 vstrhq_scatter_shifted_offset_u32]
(define_insn "mve_vstrq_truncate_scatter_shifted_offset_v4si"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (truncate:V4HI
	     (match_operand:V4SI 2 "s_register_operand" "w"))]
	  VSTRSSOQ_TRUNC))]
  "TARGET_HAVE_MVE"
  "vstrh.32\t%q2, [%0, %q1, uxtw #1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_truncate_scatter_shifted_offset_v4si"))
  (set_attr "length" "4")])

;; Predicated vector scatter stores with shifted offset
;;
;; [vstrhq_scatter_shifted_offset_p_s vstrhq_scatter_shifted_offset_p_u]
;; [vstrhq_scatter_shifted_offset_p_f]
;; [vstrwq_scatter_shifted_offset_p_s vstrwq_scatter_shifted_offset_p_u]
;; [vstrwq_scatter_shifted_offset_p_f]
;; [vstrdq_scatter_shifted_offset_p_s vstrdq_scatter_shifted_offset_p_u]
;;
(define_insn "@mve_vstrq_scatter_shifted_offset_p_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:<MVE_scatter_offset> 1 "s_register_operand" "w")
	   (match_operand:MVE_VLD_ST_scatter_shifted 2 "s_register_operand" "w")
	   (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	  VSTRSSOQ_P))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\;vstr<MVE_elem_ch>t.<V_sz_elem>\t%q2, [%0, %q1, uxtw #<MVE_scatter_shift>]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_shifted_offset_<mode>"))
  (set_attr "length" "8")])

;; Predicated truncating vector scatter stores with shifted offset
;;
;; [vstrhq_scatter_shifted_offset_p_s32 vstrhq_scatter_shifted_offset_p_u32]
(define_insn "mve_vstrq_truncate_scatter_shifted_offset_p_v4si"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:SI 0 "register_operand" "r")
	   (match_operand:V4SI 1 "s_register_operand" "w")
	   (truncate:V4HI
	     (match_operand:V4SI 2 "s_register_operand" "w"))
	   (match_operand:<MVE_VPRED> 3 "vpr_register_operand" "Up")]
	  VSTRSSOQ_TRUNC_P))]
  "TARGET_HAVE_MVE"
  "vpst\;vstrht.32\t%q2, [%0, %q1, uxtw #1]"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_truncate_scatter_shifted_offset_v4si"))
  (set_attr "length" "8")])

;;
;;
;; [vddupq_u_insn, vidupq_u_insn]
;;
(define_insn "@mve_<mve_insn>q_u<mode>_insn"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:SI 2 "s_register_operand" "1")
		      (match_operand:SI 3 "mve_imm_selective_upto_8" "Rg")]
	VIDDUPQ))
  (set (match_operand:SI 1 "s_register_operand" "=Te")
       (<viddupq_op>:SI (match_dup 2)
			(match_operand:SI 4 "immediate_operand" "i")))]
 "TARGET_HAVE_MVE"
 "<mve_insn>.u%#<V_sz_elem>\t%q0, %1, %3")

;;
;; [vddupq_m_wb_u_insn, vidupq_m_wb_u_insn]
;;
(define_insn "@mve_<mve_insn>q_m_wb_u<mode>_insn"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		      (match_operand:SI 3 "s_register_operand" "2")
		      (match_operand:SI 4 "mve_imm_selective_upto_8" "Rg")
		      (match_operand:<MVE_VPRED> 5 "vpr_register_operand" "Up")]
	VIDDUPQ_M))
  (set (match_operand:SI 2 "s_register_operand" "=Te")
       (<viddupq_m_op>:SI (match_dup 3)
			  (match_operand:SI 6 "immediate_operand" "i")))]
 "TARGET_HAVE_MVE"
 "vpst\;<mve_insn>t.u%#<V_sz_elem>\t%q0, %2, %4"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_u<mode>_insn"))
  (set_attr "length""8")])

;;
;; [vdwdupq_wb_u_insn, viwdupq_wb_u_insn]
;;
(define_insn "@mve_<mve_insn>q_wb_u<mode>_insn"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:SI 2 "s_register_operand" "1")
		       (subreg:SI (match_operand:DI 3 "s_register_operand" "r") 4)
		       (match_operand:SI 4 "mve_imm_selective_upto_8" "Rg")]
	 VIDWDUPQ))
   (set (match_operand:SI 1 "s_register_operand" "=Te")
	(unspec:SI [(match_dup 2)
		    (subreg:SI (match_dup 3) 4)
		    (match_dup 4)]
	 VIDWDUPQ))]
  "TARGET_HAVE_MVE"
  "<mve_insn>.u%#<V_sz_elem>\t%q0, %2, %R3, %4"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_wb_u<mode>_insn"))
  (set_attr "type" "mve_move")])

;;
;; [vdwdupq_m_wb_u_insn, viwdupq_m_wb_u_insn]
;;
(define_insn "@mve_<mve_insn>q_m_wb_u<mode>_insn"
  [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 3 "s_register_operand" "2")
		       (subreg:SI (match_operand:DI 4 "s_register_operand" "r") 4)
		       (match_operand:SI 5 "mve_imm_selective_upto_8" "Rg")
		       (match_operand:<MVE_VPRED> 6 "vpr_register_operand" "Up")]
	 VIDWDUPQ_M))
   (set (match_operand:SI 2 "s_register_operand" "=Te")
	(unspec:SI [(match_dup 1)
		    (match_dup 3)
		    (subreg:SI (match_dup 4) 4)
		    (match_dup 5)
		    (match_dup 6)]
	 VIDWDUPQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.u%#<V_sz_elem>\t%q1, %3, %R4, %5"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_wb_u<mode>_insn"))
  (set_attr "type" "mve_move")
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
;; [viwdupq_m_n_u])
;;
(define_expand "mve_viwdupq_m_n_u<mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:DI 3 "s_register_operand")
  (match_operand:SI 4 "mve_imm_selective_upto_8")
  (match_operand:<MVE_VPRED> 5 "vpr_register_operand")]
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
  (match_operand:<MVE_VPRED> 5 "vpr_register_operand")]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_mve_viwdupq_m_wb_u<mode>_insn (ignore_vec, operands[0],
						operands[1], operands[2],
						operands[3], operands[4],
						operands[5]));
  DONE;
})

;; Vector scatter stores with base and write-back
;;
;; [vstrwq_scatter_base_wb_s vstrwq_scatter_base_wb_u]
;; [vstrwq_scatter_base_wb_f]
;; [vstrdq_scatter_base_wb_s vstrdq_scatter_base_wb_u]
;;
(define_insn "@mve_vstrq_scatter_base_wb_<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
		[(match_operand:<MVE_scatter_offset> 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:MVE_4 3 "s_register_operand" "w")]
	 VSTRSBWBQ))
   (set (match_operand:<MVE_scatter_offset> 0 "s_register_operand" "=w")
	(unspec:<MVE_scatter_offset> [(match_dup 1) (match_dup 2)]
	 VSTRSBWBQ))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vstr<MVE_elem_ch>.u<V_sz_elem>\t%q3, [%q1, %2]!"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_base_wb_<mode>"))
  (set_attr "length" "4")])

;; Predicated vector scatter stores with base and write-back
;;
;; [vstrwq_scatter_base_wb_p_s vstrwq_scatter_base_wb_p_u]
;; [vstrwq_scatter_base_wb_p_f]
;; [vstrdq_scatter_base_wb_p_s vstrdq_scatter_base_wb_p_u]
;;
(define_insn "@mve_vstrq_scatter_base_wb_p_<mode>"
 [(set (mem:BLK (scratch))
       (unspec:BLK
		[(match_operand:<MVE_scatter_offset> 1 "s_register_operand" "0")
		 (match_operand:SI 2 "mve_vldrd_immediate" "Ri")
		 (match_operand:MVE_4 3 "s_register_operand" "w")
		 (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")]
	VSTRSBWBQ_P))
   (set (match_operand:<MVE_scatter_offset> 0 "s_register_operand" "=w")
	(unspec:<MVE_scatter_offset> [(match_dup 1) (match_dup 2)]
	 VSTRSBWBQ_P))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\;\tvstr<MVE_elem_ch>t.u<V_sz_elem>\t%q3, [%q1, %2]!"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vstrq_scatter_base_wb_<mode>"))
  (set_attr "length" "8")])

;; Vector gather loads with base and write-back
;;
;; [vldrwq_gather_base_wb_s vldrwq_gather_base_wb_u]
;; [vldrwq_gather_base_wb_f]
;; [vldrdq_gather_base_wb_s vldrdq_gather_base_wb_u]
;;
(define_insn "@mve_vldrq_gather_base_wb_<mode>"
  [(set (match_operand:MVE_4 0 "s_register_operand" "=&w")
	(unspec:MVE_4 [(match_operand:<MVE_scatter_offset> 2 "s_register_operand" "1")
		       (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		       (mem:BLK (scratch))]
	 VLDRGBWBQ))
   (set (match_operand:<MVE_scatter_offset> 1 "s_register_operand" "=&w")
	(unspec:<MVE_scatter_offset> [(match_dup 2) (match_dup 3)]
	 VLDRGBWBQ))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vldr<MVE_elem_ch>.u<V_sz_elem>\t%q0, [%q1, %3]!"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_base_wb_<mode>"))
  (set_attr "length" "4")])

;; Predicated vector gather loads with base and write-back
;;
;; [vldrwq_gather_base_wb_z_s vldrwq_gather_base_wb_z_u]
;; [vldrwq_gather_base_wb_z_f]
;; [vldrdq_gather_base_wb_z_s vldrdq_gather_base_wb_z_u]
;;
(define_insn "@mve_vldrq_gather_base_wb_z_<mode>"
  [(set (match_operand:MVE_4 0 "s_register_operand" "=&w")
	(unspec:MVE_4 [(match_operand:<MVE_scatter_offset> 2 "s_register_operand" "1")
		       (match_operand:SI 3 "mve_vldrd_immediate" "Ri")
		       (match_operand:<MVE_VPRED> 4 "vpr_register_operand" "Up")
		       (mem:BLK (scratch))]
	 VLDRGBWBQ_Z))
   (set (match_operand:<MVE_scatter_offset> 1 "s_register_operand" "=&w")
	(unspec:<MVE_scatter_offset> [(match_dup 2) (match_dup 3)]
	 VLDRGBWBQ_Z))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
   || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  "vpst\;vldr<MVE_elem_ch>t.u<V_sz_elem>\t%q0, [%q1, %3]!"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vldrq_gather_base_wb_<mode>"))
  (set_attr "length" "8")])

(define_insn "get_fpscr_nzcvqc"
 [(set (match_operand:SI 0 "register_operand" "=r")
   (unspec_volatile:SI [(reg:SI VFPCC_REGNUM)] UNSPEC_GET_FPSCR_NZCVQC))]
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
;; [vadciq_u, vadciq_s]
;; [vsbciq_s, vsbciq_u]
;;
(define_insn "@mve_<mve_insn>q_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		      (match_operand:V4SI 2 "s_register_operand" "w")]
	 VxCIQ))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(const_int 0)]
	 VxCIQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.i32\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
   (set_attr "length" "4")])

;;
;; [vadciq_m_s, vadciq_m_u]
;; [vsbciq_m_u, vsbciq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "0")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:V4SI 3 "s_register_operand" "w")
		      (match_operand:V4BI 4 "vpr_register_operand" "Up")]
	 VxCIQ_M))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(const_int 0)]
	 VxCIQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.i32\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
   (set_attr "length" "8")])

;;
;; [vadcq_u, vadcq_s]
;; [vsbcq_s, vsbcq_u]
;;
(define_insn "@mve_<mve_insn>q_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "w")
		       (match_operand:V4SI 2 "s_register_operand" "w")]
	 VxCQ))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(reg:SI VFPCC_REGNUM)]
	 VxCQ))
  ]
  "TARGET_HAVE_MVE"
  "<mve_insn>.i32\t%q0, %q1, %q2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
   (set_attr "length" "4")
   (set_attr "conds" "set")])

;;
;; [vadcq_m_s, vadcq_m_u]
;; [vsbcq_m_u, vsbcq_m_s]
;;
(define_insn "@mve_<mve_insn>q_m_<supf>v4si"
  [(set (match_operand:V4SI 0 "s_register_operand" "=w")
	(unspec:V4SI [(match_operand:V4SI 1 "s_register_operand" "0")
		      (match_operand:V4SI 2 "s_register_operand" "w")
		      (match_operand:V4SI 3 "s_register_operand" "w")
		      (match_operand:V4BI 4 "vpr_register_operand" "Up")]
	 VxCQ_M))
   (set (reg:SI VFPCC_REGNUM)
	(unspec:SI [(reg:SI VFPCC_REGNUM)]
	 VxCQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;<mve_insn>t.i32\t%q0, %q2, %q3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_<mve_insn>q_<supf>v4si"))
  (set_attr "type" "mve_move")
   (set_attr "length" "8")])

;;
;; [vst2q])
;;
(define_insn "@mve_vst2q<mode>"
  [(set (match_operand:<MVE_VLD2_VST2> 0 "mve_struct_operand" "=Ug")
	(unspec:<MVE_VLD2_VST2>
		[(match_operand:<MVE_VLD2_VST2> 1 "s_register_operand" "w")
		 (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VST2Q))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD2_VST2>mode))"
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
(define_insn "@mve_vld2q<mode>"
  [(set (match_operand:<MVE_VLD2_VST2> 0 "s_register_operand" "=w")
	(unspec:<MVE_VLD2_VST2>
		[(match_operand:<MVE_VLD2_VST2> 1 "mve_struct_operand" "Ug")
		 (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VLD2Q))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD2_VST2>mode))"
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
(define_insn "@mve_vld4q<mode>"
  [(set (match_operand:<MVE_VLD4_VST4> 0 "s_register_operand" "=w")
	(unspec:<MVE_VLD4_VST4>
		[(match_operand:<MVE_VLD4_VST4> 1 "mve_struct_operand" "Ug")
		 (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VLD4Q))
  ]
  "(TARGET_HAVE_MVE && VALID_MVE_STRUCT_MODE (<MVE_VLD4_VST4>mode))"
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
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))"
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
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))"
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
   return "vmov\t%f0, %Q1, %R1";
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
(define_insn "@mve_vshlcq_m_<supf><mode>"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 2 "s_register_operand" "0")
		      (match_operand:SI 3 "s_register_operand" "1")
		      (match_operand:SI 4 "mve_imm_32" "Rf")
		      (match_operand:<MVE_VPRED> 5 "vpr_register_operand" "Up")]
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_mve_vshlcq_<supf><mode>"))
  (set_attr "type" "mve_move")
  (set_attr "length" "8")])

;; CDE instructions on MVE registers.

(define_insn "arm_vcx1qv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			   (match_operand:SI 2 "const_int_mve_cde1_operand" "i")]
	 UNSPEC_VCDE))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx1\\tp%c1, %q0, #%c2"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx1qv16qi"))
  (set_attr "type" "coproc")]
)

(define_insn "arm_vcx1qav16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			    (match_operand:V16QI 2 "register_operand" "0")
			    (match_operand:SI 3 "const_int_mve_cde1_operand" "i")]
	 UNSPEC_VCDEA))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx1a\\tp%c1, %q0, #%c3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx1qav16qi"))
  (set_attr "type" "coproc")]
)

(define_insn "arm_vcx2qv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "t")
			  (match_operand:SI 3 "const_int_mve_cde2_operand" "i")]
	 UNSPEC_VCDE))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vcx2\\tp%c1, %q0, %q2, #%c3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx2qv16qi"))
  (set_attr "type" "coproc")]
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx2qav16qi"))
  (set_attr "type" "coproc")]
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx3qv16qi"))
  (set_attr "type" "coproc")]
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
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx3qav16qi"))
  (set_attr "type" "coproc")]
)

(define_insn "arm_vcx1q<a>_p_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			   (match_operand:V16QI 2 "register_operand" "0")
			   (match_operand:SI 3 "const_int_mve_cde1_operand" "i")
			   (match_operand:V16BI 4 "vpr_register_operand" "Up")]
	 CDE_VCX))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vpst\;vcx1<a>t\\tp%c1, %q0, #%c3"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx1q<a>v16qi"))
  (set_attr "type" "coproc")
   (set_attr "length" "8")]
)

(define_insn "arm_vcx2q<a>_p_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "0")
			  (match_operand:V16QI 3 "register_operand" "t")
			  (match_operand:SI 4 "const_int_mve_cde2_operand" "i")
			  (match_operand:V16BI 5 "vpr_register_operand" "Up")]
	 CDE_VCX))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vpst\;vcx2<a>t\\tp%c1, %q0, %q3, #%c4"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx2q<a>v16qi"))
  (set_attr "type" "coproc")
   (set_attr "length" "8")]
)

(define_insn "arm_vcx3q<a>_p_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=t")
	(unspec:V16QI [(match_operand:SI 1 "const_int_coproc_operand" "i")
			  (match_operand:V16QI 2 "register_operand" "0")
			  (match_operand:V16QI 3 "register_operand" "t")
			  (match_operand:V16QI 4 "register_operand" "t")
			  (match_operand:SI 5 "const_int_mve_cde3_operand" "i")
			  (match_operand:V16BI 6 "vpr_register_operand" "Up")]
	 CDE_VCX))]
  "TARGET_CDE && TARGET_HAVE_MVE"
  "vpst\;vcx3<a>t\\tp%c1, %q0, %q3, %q4, #%c5"
 [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_arm_vcx3q<a>v16qi"))
  (set_attr "type" "coproc")
   (set_attr "length" "8")]
)

(define_insn "movmisalign<mode>_mve_store"
  [(set (match_operand:MVE_VLD_ST 0 "mve_memory_operand"	     "=Ux")
	(unspec:MVE_VLD_ST [(match_operand:MVE_VLD_ST 1 "s_register_operand" " w")]
	 UNSPEC_MISALIGNED_ACCESS))]
  "((TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
    || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode)))
   && !BYTES_BIG_ENDIAN && unaligned_access"
  "vstr<V_sz_elem1>.<V_sz_elem>\t%q1, %E0"
  [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_movmisalign<mode>_mve_store"))
   (set_attr "type" "mve_store")]
)


(define_insn "movmisalign<mode>_mve_load"
  [(set (match_operand:MVE_VLD_ST 0 "s_register_operand"				 "=w")
	(unspec:MVE_VLD_ST [(match_operand:MVE_VLD_ST 1 "mve_memory_operand" " Ux")]
	 UNSPEC_MISALIGNED_ACCESS))]
  "((TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
    || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode)))
   && !BYTES_BIG_ENDIAN && unaligned_access"
  "vldr<V_sz_elem1>.<V_sz_elem>\t%q0, %E1"
  [(set (attr "mve_unpredicated_insn") (symbol_ref "CODE_FOR_movmisalign<mode>_mve_load"))
   (set_attr "type" "mve_load")]
)

;; Expander for VxBI moves
(define_expand "mov<mode>"
  [(set (match_operand:MVE_7 0 "nonimmediate_operand")
        (match_operand:MVE_7 1 "general_operand"))]
  "TARGET_HAVE_MVE"
  {
    if (!register_operand (operands[0], <MODE>mode))
      operands[1] = force_reg (<MODE>mode, operands[1]);
  }
)

;; Expanders for vec_cmp and vcond

(define_expand "vec_cmp<mode><MVE_vpred>"
  [(set (match_operand:<MVE_VPRED> 0 "vpr_register_operand")
	(match_operator:<MVE_VPRED> 1 "comparison_operator"
	  [(match_operand:MVE_VLD_ST 2 "s_register_operand")
	   (match_operand:MVE_VLD_ST 3 "reg_or_zero_operand")]))]
  "TARGET_HAVE_MVE
   && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
{
  arm_expand_vector_compare (operands[0], GET_CODE (operands[1]),
			     operands[2], operands[3], false);
  DONE;
})

(define_expand "vec_cmpu<mode><MVE_vpred>"
  [(set (match_operand:<MVE_VPRED> 0 "vpr_register_operand")
	(match_operator:<MVE_VPRED> 1 "comparison_operator"
	  [(match_operand:MVE_2 2 "s_register_operand")
	   (match_operand:MVE_2 3 "reg_or_zero_operand")]))]
  "TARGET_HAVE_MVE"
{
  arm_expand_vector_compare (operands[0], GET_CODE (operands[1]),
			     operands[2], operands[3], false);
  DONE;
})

(define_expand "vcond_mask_<mode><MVE_vpred>"
  [(set (match_operand:MVE_VLD_ST 0 "s_register_operand")
	(if_then_else:MVE_VLD_ST
	  (match_operand:<MVE_VPRED> 3 "vpr_register_operand")
	  (match_operand:MVE_VLD_ST 1 "s_register_operand")
	  (match_operand:MVE_VLD_ST 2 "s_register_operand")))]
  "TARGET_HAVE_MVE"
{
  switch (GET_MODE_CLASS (<MODE>mode))
    {
      case MODE_VECTOR_INT:
	emit_insn (gen_mve_q (VPSELQ_S,	VPSELQ_S, <MODE>mode, operands[0],
			      operands[1], operands[2], operands[3]));
	break;
      case MODE_VECTOR_FLOAT:
	emit_insn (gen_mve_q_f (VPSELQ_F, <MODE>mode, operands[0],
				operands[1], operands[2], operands[3]));
	break;
      default:
	gcc_unreachable ();
    }
  DONE;
})

;; Reinterpret operand 1 in operand 0's mode, without changing its contents.
(define_expand "@arm_mve_reinterpret<mode>"
  [(set (match_operand:MVE_vecs 0 "register_operand")
	(unspec:MVE_vecs
	  [(match_operand 1 "arm_any_register_operand")]
	  REINTERPRET))]
  "(TARGET_HAVE_MVE && VALID_MVE_SI_MODE (<MODE>mode))
    || (TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))"
  {
    machine_mode src_mode = GET_MODE (operands[1]);
    if (targetm.can_change_mode_class (<MODE>mode, src_mode, VFP_REGS))
      {
	emit_move_insn (operands[0], gen_lowpart (<MODE>mode, operands[1]));
	DONE;
      }
  }
)

;; Originally expanded by 'predicated_doloop_end'.
;; In the rare situation where the branch is too far, we do also need to
;; revert FPSCR.LTPSIZE back to 0x100 after the last iteration.
(define_insn "predicated_doloop_end_internal<letp_num_lanes>"
  [(set (pc)
	(if_then_else
	 (gtu (plus:SI (reg:SI LR_REGNUM)
	       (const_int <letp_num_lanes_neg>))
	      (const_int <letp_num_lanes_minus_1>))
	 (match_operand 0 "" "")
	 (pc)))
   (set (reg:SI LR_REGNUM)
	(plus:SI (reg:SI LR_REGNUM) (const_int <letp_num_lanes_neg>)))
  ;; We use UNSPEC here to guarantee this pattern can not be
  ;; generated by a RTL optimization and be matched by other
  ;; patterns, since this pattern is also responsible for turning off
  ;; the tail predication machinery if we were to exit the loop.
  ;; This is done by either the LETP or the LCTP instructions that
  ;; this pattern generates.
   (use (unspec:SI [(const_int 0)] LETP))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_HAVE_MVE"
  {
    if (get_attr_length (insn) == 4)
      return "letp\t%|lr, %l0";
    else
      return "subs\t%|lr, #<letp_num_lanes>\n\tbhi\t%l0\n\tlctp";
  }
  [(set (attr "length")
	(if_then_else
	   (ltu (minus (pc) (match_dup 0)) (const_int 1024))
	    (const_int 4)
	    (const_int 12)))
   (set_attr "type" "branch")
   (set_attr "conds" "unconditional")])

(define_insn "dlstp<dlstp_elemsize>_insn"
  [
    (set (reg:SI LR_REGNUM)
;; Similar to the previous pattern, we use UNSPEC here to make sure this
;; rtx construct is not matched by other patterns, as this pattern is also
;; responsible for setting the element size of the tail predication machinery
;; using the dlsp.<size> instruction.
	 (unspec_volatile:SI [(match_operand:SI 0 "s_register_operand" "r")]
	  DLSTP))
  ]
  "TARGET_HAVE_MVE"
  "dlstp.<dlstp_elemsize>\t%|lr, %0"
  [(set_attr "type" "mve_misc")])
