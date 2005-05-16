;; AltiVec patterns.
;; Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
;; Contributed by Aldy Hernandez (aldy@quesejoda.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(define_constants
  [(UNSPEC_VCMPBFP       50)
   (UNSPEC_VCMPEQUB      51)
   (UNSPEC_VCMPEQUH      52)
   (UNSPEC_VCMPEQUW      53)
   (UNSPEC_VCMPEQFP      54)
   (UNSPEC_VCMPGEFP      55)
   (UNSPEC_VCMPGTUB      56)
   (UNSPEC_VCMPGTSB      57)
   (UNSPEC_VCMPGTUH      58)
   (UNSPEC_VCMPGTSH      59)
   (UNSPEC_VCMPGTUW      60)
   (UNSPEC_VCMPGTSW      61)
   (UNSPEC_VCMPGTFP      62)
   (UNSPEC_VSLW         109)
   (UNSPEC_SUBS         126)
   (UNSPEC_VSEL4SI      159)
   (UNSPEC_VSEL4SF      160)
   (UNSPEC_VSEL8HI      161)
   (UNSPEC_VSEL16QI     162)
   (UNSPEC_SET_VSCR     213)
   (UNSPEC_VCOND_V4SI   301)
   (UNSPEC_VCOND_V4SF   302)
   (UNSPEC_VCOND_V8HI   303)
   (UNSPEC_VCOND_V16QI  304)
   (UNSPEC_VCONDU_V4SI  305)
   (UNSPEC_VCONDU_V8HI  306)
   (UNSPEC_VCONDU_V16QI 307)
   ])

;; Vec int modes
(define_mode_macro VI [V4SI V8HI V16QI])
;; Short vec in modes
(define_mode_macro VIshort [V8HI V16QI])
;; Vec float modes
(define_mode_macro VF [V4SF])
;; Vec modes, pity mode macros are not composable
(define_mode_macro V [V4SI V8HI V16QI V4SF])

(define_mode_attr VI_char [(V4SI "w") (V8HI "h") (V16QI "b")])

;; Generic LVX load instruction.
(define_insn "altivec_lvx_<mode>"
  [(set (match_operand:V 0 "altivec_register_operand" "=v")
	(match_operand:V 1 "memory_operand" "m"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

;; Generic STVX store instruction.
(define_insn "altivec_stvx_<mode>"
  [(set (match_operand:V 0 "memory_operand" "=m")
	(match_operand:V 1 "altivec_register_operand" "v"))]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

;; Vector move instructions.
(define_expand "mov<mode>"
  [(set (match_operand:V 0 "nonimmediate_operand" "")
	(match_operand:V 1 "any_operand" ""))]
  "TARGET_ALTIVEC"
{
  rs6000_emit_move (operands[0], operands[1], <MODE>mode);
  DONE;
})

(define_insn "*mov<mode>_internal"
  [(set (match_operand:V 0 "nonimmediate_operand" "=m,v,v,o,r,r,v")
	(match_operand:V 1 "input_operand" "v,m,v,r,o,r,W"))]
  "TARGET_ALTIVEC 
   && (register_operand (operands[0], <MODE>mode) 
       || register_operand (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0: return "stvx %1,%y0";
    case 1: return "lvx %0,%y1";
    case 2: return "vor %0,%1,%1";
    case 3: return "#";
    case 4: return "#";
    case 5: return "#";
    case 6: return output_vec_const_move (operands);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,*")])

(define_split
  [(set (match_operand:V4SI 0 "nonimmediate_operand" "")
        (match_operand:V4SI 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{
  rs6000_split_multireg_move (operands[0], operands[1]); DONE;
})

(define_split
  [(set (match_operand:V4SI 0 "altivec_register_operand" "")
	(match_operand:V4SI 1 "easy_vector_constant_add_self" ""))]
  "TARGET_ALTIVEC && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0)
	(plus:V4SI (match_dup 0)
		   (match_dup 0)))]
{ 
  operands[3] = gen_easy_vector_constant_add_self (operands[1]);
})    

(define_split
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "")
        (match_operand:V8HI 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; })

(define_split
  [(set (match_operand:V8HI 0 "altivec_register_operand" "")
	(match_operand:V8HI 1 "easy_vector_constant_add_self" ""))]
  "TARGET_ALTIVEC && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0)
	(plus:V8HI (match_dup 0)
		   (match_dup 0)))]
{
  operands[3] = gen_easy_vector_constant_add_self (operands[1]);
})

(define_split
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "")
        (match_operand:V16QI 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; })

(define_split
  [(set (match_operand:V16QI 0 "altivec_register_operand" "")
	(match_operand:V16QI 1 "easy_vector_constant_add_self" ""))]
  "TARGET_ALTIVEC && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0)
	(plus:V16QI (match_dup 0)
		   (match_dup 0)))]
{
  operands[3] = gen_easy_vector_constant_add_self (operands[1]);
})

(define_split
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "")
        (match_operand:V4SF 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{
  rs6000_split_multireg_move (operands[0], operands[1]); DONE;
})

(define_insn "get_vrsave_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(reg:SI 109)] 214))]
  "TARGET_ALTIVEC"
{
  if (TARGET_MACHO)
     return "mfspr %0,256";
  else
     return "mfvrsave %0";
}
  [(set_attr "type" "*")])

(define_insn "*set_vrsave_internal"
  [(match_parallel 0 "vrsave_operation"
     [(set (reg:SI 109)
	   (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r")
				(reg:SI 109)] 30))])]
  "TARGET_ALTIVEC"
{
  if (TARGET_MACHO)
    return "mtspr 256,%1";
  else
    return "mtvrsave %1";
}
  [(set_attr "type" "*")])

(define_insn "*save_world"
 [(match_parallel 0 "save_world_operation"
                  [(clobber (match_operand:SI 1 "register_operand" "=l"))
                   (use (match_operand:SI 2 "call_operand" "s"))])]
 "TARGET_MACHO && (DEFAULT_ABI == ABI_DARWIN) && TARGET_32BIT"         
 "bl %z2"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "*restore_world"
 [(match_parallel 0 "restore_world_operation"
                  [(return)
                   (use (match_operand:SI 1 "register_operand" "l"))
                   (use (match_operand:SI 2 "call_operand" "s"))
                   (clobber (match_operand:SI 3 "gpc_reg_operand" "=r"))])]
 "TARGET_MACHO && (DEFAULT_ABI == ABI_DARWIN) && TARGET_32BIT"
 "b %z2")

;; Simple binary operations.

;; add
(define_insn "add<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (plus:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vaddu<VI_char>m %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "addv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (plus:V4SF (match_operand:V4SF 1 "register_operand" "v")
	 	   (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vaddfp %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vaddcuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 35))]
  "TARGET_ALTIVEC"
  "vaddcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vaddu<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] 36))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vaddu<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vadds<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] 37))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vadds<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; sub
(define_insn "sub<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (minus:VI (match_operand:VI 1 "register_operand" "v")
                  (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsubu<VI_char>m %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "subv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (minus:V4SF (match_operand:V4SF 1 "register_operand" "v")
                    (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsubfp %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vsubcuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 124))]
  "TARGET_ALTIVEC"
  "vsubcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubu<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] 125))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsubu<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubs<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] UNSPEC_SUBS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsubs<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;;
(define_insn "altivec_vavgu<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] 44))]
  "TARGET_ALTIVEC"
  "vavgu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavgs<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] 45))]
  "TARGET_ALTIVEC"
  "vavgs<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpbfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 
                      UNSPEC_VCMPBFP))]
  "TARGET_ALTIVEC"
  "vcmpbfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vcmpequb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 
                       UNSPEC_VCMPEQUB))]
  "TARGET_ALTIVEC"
  "vcmpequb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpequh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 
                      UNSPEC_VCMPEQUH))]
  "TARGET_ALTIVEC"
  "vcmpequh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpequw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 
	              UNSPEC_VCMPEQUW))]
  "TARGET_ALTIVEC"
  "vcmpequw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpeqfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 
	              UNSPEC_VCMPEQFP))]
  "TARGET_ALTIVEC"
  "vcmpeqfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vcmpgefp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 
	               UNSPEC_VCMPGEFP))]
  "TARGET_ALTIVEC"
  "vcmpgefp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vcmpgtub"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 
                       UNSPEC_VCMPGTUB))]
  "TARGET_ALTIVEC"
  "vcmpgtub %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtsb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 
                       UNSPEC_VCMPGTSB))]
  "TARGET_ALTIVEC"
  "vcmpgtsb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtuh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 
                      UNSPEC_VCMPGTUH))]
  "TARGET_ALTIVEC"
  "vcmpgtuh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtsh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 
                      UNSPEC_VCMPGTSH))]
  "TARGET_ALTIVEC"
  "vcmpgtsh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 
	              UNSPEC_VCMPGTUW))]
  "TARGET_ALTIVEC"
  "vcmpgtuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtsw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 
	              UNSPEC_VCMPGTSW))]
  "TARGET_ALTIVEC"
  "vcmpgtsw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 
	              UNSPEC_VCMPGTFP))]
  "TARGET_ALTIVEC"
  "vcmpgtfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

;; Fused multiply add
(define_insn "altivec_vmaddfp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(plus:V4SF (mult:V4SF (match_operand:V4SF 1 "register_operand" "v")
			      (match_operand:V4SF 2 "register_operand" "v"))
	  	   (match_operand:V4SF 3 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaddfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

;; We do multiply as a fused multiply-add with an add of a -0.0 vector.

(define_expand "mulv4sf3"
  [(use (match_operand:V4SF 0 "register_operand" ""))
   (use (match_operand:V4SF 1 "register_operand" ""))
   (use (match_operand:V4SF 2 "register_operand" ""))]
  "TARGET_ALTIVEC && TARGET_FUSED_MADD"
  "
{
  rtx neg0;

  /* Generate [-0.0, -0.0, -0.0, -0.0].  */
  neg0 = gen_reg_rtx (V4SFmode);
  emit_insn (gen_altivec_vspltisw_v4sf (neg0, constm1_rtx));
  emit_insn (gen_altivec_vslw_v4sf (neg0, neg0, neg0));

  /* Use the multiply-add.  */
  emit_insn (gen_altivec_vmaddfp (operands[0], operands[1], operands[2],
				  neg0));
  DONE;
}")

;; 32 bit integer multiplication
;; A_high = Operand_0 & 0xFFFF0000 >> 16
;; A_low = Operand_0 & 0xFFFF
;; B_high = Operand_1 & 0xFFFF0000 >> 16
;; B_low = Operand_1 & 0xFFFF
;; result = A_low * B_low + (A_high * B_low + B_high * A_low) << 16

;; (define_insn "mulv4si3"
;;   [(set (match_operand:V4SI 0 "register_operand" "=v")
;;         (mult:V4SI (match_operand:V4SI 1 "register_operand" "v")
;;                    (match_operand:V4SI 2 "register_operand" "v")))]
(define_expand "mulv4si3"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V4SI 1 "register_operand" ""))
   (use (match_operand:V4SI 2 "register_operand" ""))]
   "TARGET_ALTIVEC"
   "
 {
   rtx zero;
   rtx swap;
   rtx small_swap;
   rtx sixteen;
   rtx one;
   rtx two;
   rtx low_product;
   rtx high_product;
       
   zero = gen_reg_rtx (V4SImode);
   emit_insn (gen_altivec_vspltisw (zero, const0_rtx));
 
   sixteen = gen_reg_rtx (V4SImode);   
   emit_insn (gen_altivec_vspltisw (sixteen,  gen_rtx_CONST_INT (V4SImode, -16)));
 
   swap = gen_reg_rtx (V4SImode);
   emit_insn (gen_altivec_vrlw (swap, operands[2], sixteen));
 
   one = gen_reg_rtx (V8HImode);
   convert_move (one, operands[1], 0);
 
   two = gen_reg_rtx (V8HImode);
   convert_move (two, operands[2], 0);
 
   small_swap = gen_reg_rtx (V8HImode);
   convert_move (small_swap, swap, 0);
 
   low_product = gen_reg_rtx (V4SImode);
   emit_insn (gen_altivec_vmulouh (low_product, one, two));
 
   high_product = gen_reg_rtx (V4SImode);
   emit_insn (gen_altivec_vmsumuhm (high_product, one, small_swap, zero));
 
   emit_insn (gen_altivec_vslw (high_product, high_product, sixteen));
 
   emit_insn (gen_addv4si3 (operands[0], high_product, low_product));
   
   DONE;
 }")
 

;; Fused multiply subtract 
(define_insn "altivec_vnmsubfp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(neg:V4SF (minus:V4SF (mult:V4SF (match_operand:V4SF 1 "register_operand" "v")
			       (match_operand:V4SF 2 "register_operand" "v"))
	  	    (match_operand:V4SF 3 "register_operand" "v"))))]
  "TARGET_ALTIVEC"
  "vnmsubfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vmsumu<VI_char>m"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")
		      (match_operand:VIshort 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 65))]
  "TARGET_ALTIVEC"
  "vmsumu<VI_char>m %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumm<VI_char>m"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")
		      (match_operand:VIshort 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 66))]
  "TARGET_ALTIVEC"
  "vmsumm<VI_char>m %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumshm"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 68))]
  "TARGET_ALTIVEC"
  "vmsumshm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumuhs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 69))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmsumuhs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumshs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 70))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmsumshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

;; max

(define_insn "umax<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (umax:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smax<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (smax:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxs<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smaxv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (smax:V4SF (match_operand:V4SF 1 "register_operand" "v")
                   (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "umin<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (umin:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smin<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (smin:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmins<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "sminv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (smin:V4SF (match_operand:V4SF 1 "register_operand" "v")
                   (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vmhaddshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 71))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmhaddshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])
(define_insn "altivec_vmhraddshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 72))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmhraddshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])
(define_insn "altivec_vmladduhm"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 73))]
  "TARGET_ALTIVEC"
  "vmladduhm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmrghb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_merge:V16QI (vec_select:V16QI (match_operand:V16QI 1 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 8)
					   	      (const_int 1)
					   	      (const_int 9)
					   	      (const_int 2)
					   	      (const_int 10)
						      (const_int 3)
						      (const_int 11)
					   	      (const_int 4)
					   	      (const_int 12)
					   	      (const_int 5)
					   	      (const_int 13)
					   	      (const_int 6)
					   	      (const_int 14)
					   	      (const_int 7)
						      (const_int 15)]))
                        (vec_select:V16QI (match_operand:V16QI 2 "register_operand" "v")
					   (parallel [(const_int 8)
					   	      (const_int 0)
					   	      (const_int 9)
					   	      (const_int 1)
					   	      (const_int 10)
					   	      (const_int 2)
						      (const_int 11)
						      (const_int 3)
					   	      (const_int 12)
					   	      (const_int 4)
					   	      (const_int 13)
					   	      (const_int 5)
					   	      (const_int 14)
					   	      (const_int 6)
					   	      (const_int 15)
						      (const_int 7)]))
		      (const_int 21845)))]
  "TARGET_ALTIVEC"
  "vmrghb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_merge:V8HI (vec_select:V8HI (match_operand:V8HI 1 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 4)
					   	      (const_int 1)
					   	      (const_int 5)
					   	      (const_int 2)
					   	      (const_int 6)
					   	      (const_int 3)
					   	      (const_int 7)]))
                        (vec_select:V8HI (match_operand:V8HI 2 "register_operand" "v")
					   (parallel [(const_int 4)
					   	      (const_int 0)
					   	      (const_int 5)
					   	      (const_int 1)
					   	      (const_int 6)
					   	      (const_int 2)
					   	      (const_int 7)
					   	      (const_int 3)]))
		      (const_int 85)))]
  "TARGET_ALTIVEC"
  "vmrghh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_merge:V4SI (vec_select:V4SI (match_operand:V4SI 1 "register_operand" "v")
					 (parallel [(const_int 0)
					 	    (const_int 2)
						    (const_int 1)
						    (const_int 3)]))
                        (vec_select:V4SI (match_operand:V4SI 2 "register_operand" "v")
					 (parallel [(const_int 2)
					 	    (const_int 0)
						    (const_int 3)
						    (const_int 1)]))
		      (const_int 5)))]
  "TARGET_ALTIVEC"
  "vmrghw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_merge:V16QI (vec_select:V16QI (match_operand:V16QI 1 "register_operand" "v")
					   (parallel [(const_int 8)
					   	      (const_int 0)
					   	      (const_int 9)
					   	      (const_int 1)
					   	      (const_int 10)
					   	      (const_int 2)
						      (const_int 11)
						      (const_int 3)
					   	      (const_int 12)
					   	      (const_int 4)
					   	      (const_int 13)
					   	      (const_int 5)
					   	      (const_int 14)
					   	      (const_int 6)
					   	      (const_int 15)
						      (const_int 7)]))
                      (vec_select:V16QI (match_operand:V16QI 2 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 8)
					   	      (const_int 1)
					   	      (const_int 9)
					   	      (const_int 2)
					   	      (const_int 10)
						      (const_int 3)
						      (const_int 11)
					   	      (const_int 4)
					   	      (const_int 12)
					   	      (const_int 5)
					   	      (const_int 13)
					   	      (const_int 6)
					   	      (const_int 14)
					   	      (const_int 7)
						      (const_int 15)]))
		      (const_int 21845)))]
  "TARGET_ALTIVEC"
  "vmrglb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_merge:V8HI (vec_select:V8HI (match_operand:V8HI 1 "register_operand" "v")
					   (parallel [(const_int 4)
					   	      (const_int 0)
					   	      (const_int 5)
					   	      (const_int 1)
					   	      (const_int 6)
					   	      (const_int 2)
					   	      (const_int 7)
					   	      (const_int 3)]))
                        (vec_select:V8HI (match_operand:V8HI 2 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 4)
					   	      (const_int 1)
					   	      (const_int 5)
					   	      (const_int 2)
					   	      (const_int 6)
					   	      (const_int 3)
					   	      (const_int 7)]))
		      (const_int 85)))]
  "TARGET_ALTIVEC"
  "vmrglh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_merge:V4SI (vec_select:V4SI (match_operand:V4SI 1 "register_operand" "v")
					 (parallel [(const_int 2)
					 	    (const_int 0)
						    (const_int 3)
						    (const_int 1)]))
                        (vec_select:V4SI (match_operand:V4SI 2 "register_operand" "v")
					 (parallel [(const_int 0)
					 	    (const_int 2)
						    (const_int 1)
						    (const_int 3)]))
		      (const_int 5)))]
  "TARGET_ALTIVEC"
  "vmrglw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmuleub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 83))]
  "TARGET_ALTIVEC"
  "vmuleub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 84))]
  "TARGET_ALTIVEC"
  "vmulesb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuleuh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 85))]
  "TARGET_ALTIVEC"
  "vmuleuh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 86))]
  "TARGET_ALTIVEC"
  "vmulesh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuloub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 87))]
  "TARGET_ALTIVEC"
  "vmuloub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 88))]
  "TARGET_ALTIVEC"
  "vmulosb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulouh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 89))]
  "TARGET_ALTIVEC"
  "vmulouh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 90))]
  "TARGET_ALTIVEC"
  "vmulosh %0,%1,%2"
  [(set_attr "type" "veccomplex")])


;; logical ops

(define_insn "and<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (and:VI (match_operand:VI 1 "register_operand" "v")
                (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vand %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "ior<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (ior:VI (match_operand:VI 1 "register_operand" "v")
                (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "xor<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (xor:VI (match_operand:VI 1 "register_operand" "v")
                (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vxor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (not:VI (match_operand:VI 1 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vnor %0,%1,%1"
  [(set_attr "type" "vecsimple")])
  
(define_insn "altivec_nor<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (not:VI (ior:VI (match_operand:VI 1 "register_operand" "v")
                        (match_operand:VI 2 "register_operand" "v"))))]
  "TARGET_ALTIVEC"
  "vnor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "andc<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (and:VI (not:VI (match_operand:VI 2 "register_operand" "v"))
                (match_operand:VI 1 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vandc %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*andc3_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (and:V4SF (not:V4SF (match_operand:V4SF 2 "register_operand" "v"))
                  (match_operand:V4SF 1 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vandc %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vpkuhum"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 93))]
  "TARGET_ALTIVEC"
  "vpkuhum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwum"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 94))]
  "TARGET_ALTIVEC"
  "vpkuwum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkpx"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 95))]
  "TARGET_ALTIVEC"
  "vpkpx %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuhss"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 96))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkuhss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkshss"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 97))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkshss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwss"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 98))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkuwss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkswss"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 99))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkswss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuhus"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 100))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkuhus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkshus"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 101))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkshus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwus"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 102))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkuwus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkswus"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 103))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkswus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vrl<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] 104))]
  "TARGET_ALTIVEC"
  "vrl<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsl<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")] 107))]
  "TARGET_ALTIVEC"
  "vsl<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vslw_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] UNSPEC_VSLW))]
  "TARGET_ALTIVEC"
  "vslw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsl"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 110))]
  "TARGET_ALTIVEC"
  "vsl %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vslo"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 111))]
  "TARGET_ALTIVEC"
  "vslo %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "lshr<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (lshiftrt:VI (match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v") ))]
  "TARGET_ALTIVEC"
  "vsr<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "ashr<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (ashiftrt:VI (match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v") ))]
  "TARGET_ALTIVEC"
  "vsra<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsr"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 118))]
  "TARGET_ALTIVEC"
  "vsr %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsro"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 119))]
  "TARGET_ALTIVEC"
  "vsro %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsum4ubs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 131))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsum4ubs %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsum4s<VI_char>s"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 132))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsum4s<VI_char>s %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsum2sws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 134))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsum2sws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsumsws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 135))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsumsws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vspltb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_duplicate:V16QI
	 (vec_select:QI (match_operand:V16QI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "immediate_operand" "i")]))))]
  "TARGET_ALTIVEC"
  "vspltb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsplth"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(vec_duplicate:V8HI
	 (vec_select:HI (match_operand:V8HI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "immediate_operand" "i")]))))]
  "TARGET_ALTIVEC"
  "vsplth %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_duplicate:V4SI
	 (vec_select:SI (match_operand:V4SI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "immediate_operand" "i")]))))]
  "TARGET_ALTIVEC"
  "vspltw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltis<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
	(vec_duplicate:VI
	 (match_operand:QI 1 "const_int_operand" "i")))]
  "TARGET_ALTIVEC"
  "vspltis<VI_char> %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltisw_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_duplicate:V4SF
	 (float:SF (match_operand:QI 1 "const_int_operand" "i"))))]
  "TARGET_ALTIVEC"
  "vspltisw %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "ftruncv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
  	(fix:V4SF (match_operand:V4SF 1 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vrfiz %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vperm_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
		      (match_operand:V4SF 2 "register_operand" "v")
		      (match_operand:V16QI 3 "register_operand" "v")] 145))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vperm_<mode>"
  [(set (match_operand:VI 0 "register_operand" "=v")
	(unspec:VI [(match_operand:VI 1 "register_operand" "v")
		    (match_operand:VI 2 "register_operand" "v")
		    (match_operand:V16QI 3 "register_operand" "v")] 144))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vrfip"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 148))]
  "TARGET_ALTIVEC"
  "vrfip %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrfin"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 149))]
  "TARGET_ALTIVEC"
  "vrfin %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrfim"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 150))]
  "TARGET_ALTIVEC"
  "vrfim %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vcfux"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SI 1 "register_operand" "v")
	              (match_operand:QI 2 "immediate_operand" "i")] 151))]
  "TARGET_ALTIVEC"
  "vcfux %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vcfsx"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SI 1 "register_operand" "v")
	              (match_operand:QI 2 "immediate_operand" "i")] 152))]
  "TARGET_ALTIVEC"
  "vcfsx %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vctuxs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")] 153))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vctuxs %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vctsxs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")] 154))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vctsxs %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vlogefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 155))]
  "TARGET_ALTIVEC"
  "vlogefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vexptefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 156))]
  "TARGET_ALTIVEC"
  "vexptefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrsqrtefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 157))]
  "TARGET_ALTIVEC"
  "vrsqrtefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 158))]
  "TARGET_ALTIVEC"
  "vrefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_expand "vcondv4si"
	[(set (match_operand:V4SI 0 "register_operand" "=v")
	      (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
	       (match_operand:V4SI 2 "register_operand" "v")
	       (match_operand:V4SI 3 "comparison_operator" "")
	       (match_operand:V4SI 4 "register_operand" "v")
	       (match_operand:V4SI 5 "register_operand" "v")
	       ] UNSPEC_VCOND_V4SI))]
	"TARGET_ALTIVEC"
	"
{
	if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
					  operands[3], operands[4], operands[5]))
	DONE;
	else
	FAIL;
}
	")

(define_expand "vconduv4si"
	[(set (match_operand:V4SI 0 "register_operand" "=v")
	      (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
	       (match_operand:V4SI 2 "register_operand" "v")
	       (match_operand:V4SI 3 "comparison_operator" "")
	       (match_operand:V4SI 4 "register_operand" "v")
	       (match_operand:V4SI 5 "register_operand" "v")
	       ] UNSPEC_VCONDU_V4SI))]
	"TARGET_ALTIVEC"
	"
{
	if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
					  operands[3], operands[4], operands[5]))
	DONE;
	else
	FAIL;
}
	")

(define_expand "vcondv4sf"
	[(set (match_operand:V4SF 0 "register_operand" "=v")
	      (unspec:V4SF [(match_operand:V4SI 1 "register_operand" "v")
	       (match_operand:V4SF 2 "register_operand" "v")
	       (match_operand:V4SF 3 "comparison_operator" "")
	       (match_operand:V4SF 4 "register_operand" "v")
	       (match_operand:V4SF 5 "register_operand" "v")
	       ] UNSPEC_VCOND_V4SF))]
	"TARGET_ALTIVEC"
	"
{
	if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
					  operands[3], operands[4], operands[5]))
	DONE;
	else
	FAIL;
}
	")

(define_expand "vcondv8hi"
	[(set (match_operand:V4SF 0 "register_operand" "=v")
	      (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
	       (match_operand:V8HI 2 "register_operand" "v")
	       (match_operand:V8HI 3 "comparison_operator" "")
	       (match_operand:V8HI 4 "register_operand" "v")
	       (match_operand:V8HI 5 "register_operand" "v")
	       ] UNSPEC_VCOND_V8HI))]
	"TARGET_ALTIVEC"
	"
{
	if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
					  operands[3], operands[4], operands[5]))
	DONE;
	else
	FAIL;
}
	")

(define_expand "vconduv8hi"
	[(set (match_operand:V4SF 0 "register_operand" "=v")
	      (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
	       (match_operand:V8HI 2 "register_operand" "v")
	       (match_operand:V8HI 3 "comparison_operator" "")
	       (match_operand:V8HI 4 "register_operand" "v")
	       (match_operand:V8HI 5 "register_operand" "v")
	       ] UNSPEC_VCONDU_V8HI))]
	"TARGET_ALTIVEC"
	"
{
	if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
					  operands[3], operands[4], operands[5]))
	DONE;
	else
	FAIL;
}
	")

(define_expand "vcondv16qi"
	[(set (match_operand:V4SF 0 "register_operand" "=v")
	      (unspec:V16QI [(match_operand:V4SI 1 "register_operand" "v")
	       (match_operand:V16QI 2 "register_operand" "v")
	       (match_operand:V16QI 3 "comparison_operator" "")
	       (match_operand:V16QI 4 "register_operand" "v")
	       (match_operand:V16QI 5 "register_operand" "v")
	       ] UNSPEC_VCOND_V16QI))]
	"TARGET_ALTIVEC"
	"
{
	if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
					  operands[3], operands[4], operands[5]))
	DONE;
	else
	FAIL;
}
	")

(define_expand "vconduv16qi"
	[(set (match_operand:V4SF 0 "register_operand" "=v")
	      (unspec:V16QI [(match_operand:V4SI 1 "register_operand" "v")
	       (match_operand:V16QI 2 "register_operand" "v")
	       (match_operand:V16QI 3 "comparison_operator" "")
	       (match_operand:V16QI 4 "register_operand" "v")
	       (match_operand:V16QI 5 "register_operand" "v")
	       ] UNSPEC_VCONDU_V16QI))]
	"TARGET_ALTIVEC"
	"
{
	if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
					  operands[3], operands[4], operands[5]))
	DONE;
	else
	FAIL;
}
	")


(define_insn "altivec_vsel_v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 
	              UNSPEC_VSEL4SI))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsel_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 
	              UNSPEC_VSEL4SF))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsel_v8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 
	              UNSPEC_VSEL8HI))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsel_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")
                       (match_operand:V16QI 3 "register_operand" "v")] 
	               UNSPEC_VSEL16QI))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
		      (match_operand:V4SI 2 "register_operand" "v")
                      (match_operand:QI 3 "immediate_operand" "i")] 163))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
		      (match_operand:V4SF 2 "register_operand" "v")
                      (match_operand:QI 3 "immediate_operand" "i")] 164))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_v8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:QI 3 "immediate_operand" "i")] 165))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
		       (match_operand:V16QI 2 "register_operand" "v")
		       (match_operand:QI 3 "immediate_operand" "i")] 166))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhsb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
  	(unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")] 167))]
  "TARGET_ALTIVEC"
  "vupkhsb %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 168))]
  "TARGET_ALTIVEC"
  "vupkhpx %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhsh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 169))]
  "TARGET_ALTIVEC"
  "vupkhsh %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklsb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
  	(unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")] 170))]
  "TARGET_ALTIVEC"
  "vupklsb %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 171))]
  "TARGET_ALTIVEC"
  "vupklpx %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklsh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 172))]
  "TARGET_ALTIVEC"
  "vupklsh %0,%1"
  [(set_attr "type" "vecperm")])

;; AltiVec predicates.

(define_expand "cr6_test_for_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC 74)
	       (const_int 0)))]
  "TARGET_ALTIVEC"
  "")	

(define_expand "cr6_test_for_zero_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC 74)
	       (const_int 0)))
   (set (match_dup 0) (minus:SI (const_int 1) (match_dup 0)))]
  "TARGET_ALTIVEC"
  "")

(define_expand "cr6_test_for_lt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC 74)
	       (const_int 0)))]
  "TARGET_ALTIVEC"
  "")

(define_expand "cr6_test_for_lt_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC 74)
	       (const_int 0)))
   (set (match_dup 0) (minus:SI (const_int 1) (match_dup 0)))]
  "TARGET_ALTIVEC"
  "")

;; We can get away with generating the opcode on the fly (%3 below)
;; because all the predicates have the same scheduling parameters.

(define_insn "altivec_predicate_v4sf"
  [(set (reg:CC 74)
	(unspec:CC [(match_operand:V4SF 1 "register_operand" "v")
		    (match_operand:V4SF 2 "register_operand" "v")
		    (match_operand 3 "any_operand" "")] 174))
   (clobber (match_scratch:V4SF 0 "=v"))]
  "TARGET_ALTIVEC"
  "%3 %0,%1,%2"
[(set_attr "type" "veccmp")])

(define_insn "altivec_predicate_<mode>"
  [(set (reg:CC 74)
	(unspec:CC [(match_operand:VI 1 "register_operand" "v")
		    (match_operand:VI 2 "register_operand" "v")
		    (match_operand 3 "any_operand" "")] 173))
   (clobber (match_scratch:VI 0 "=v"))]
  "TARGET_ALTIVEC"
  "%3 %0,%1,%2"
[(set_attr "type" "veccmp")])

(define_insn "altivec_mtvscr"
  [(set (reg:SI 110)
	(unspec_volatile:SI
	 [(match_operand:V4SI 0 "register_operand" "v")] 186))]
  "TARGET_ALTIVEC"
  "mtvscr %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_mfvscr"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(unspec_volatile:V8HI [(reg:SI 110)] 187))]
  "TARGET_ALTIVEC"
  "mfvscr %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dssall"
  [(unspec_volatile [(const_int 0)] 188)]
  "TARGET_ALTIVEC"
  "dssall"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dss"
  [(unspec_volatile [(match_operand:QI 0 "immediate_operand" "i")] 189)]
  "TARGET_ALTIVEC"
  "dss %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dst"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 190)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dst %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dstt"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 191)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dstt %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dstst"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 192)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dstst %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dststt"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 193)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dststt %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_lvsl"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand 1 "memory_operand" "m")] 194))]
  "TARGET_ALTIVEC"
  "lvsl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvsr"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand 1 "memory_operand" "m")] 195))]
  "TARGET_ALTIVEC"
  "lvsr %0,%y1"
  [(set_attr "type" "vecload")])

(define_expand "build_vector_mask_for_load"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand 1 "memory_operand" "m")] 195))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx addr;
  rtx temp;

  gcc_assert (GET_CODE (operands[1]) == MEM);

  addr = XEXP (operands[1], 0);
  temp = gen_reg_rtx (GET_MODE (addr));
  emit_insn (gen_rtx_SET (VOIDmode, temp, 
			  gen_rtx_NEG (GET_MODE (addr), addr)));
  emit_insn (gen_altivec_lvsr (operands[0], 
			       gen_rtx_MEM (GET_MODE (operands[1]), temp)));
  DONE;
}")

;; Parallel some of the LVE* and STV*'s with unspecs because some have
;; identical rtl but different instructions-- and gcc gets confused.

(define_insn "altivec_lve<VI_char>x"
  [(parallel
    [(set (match_operand:VI 0 "register_operand" "=v")
	  (match_operand:VI 1 "memory_operand" "m"))
     (unspec [(const_int 0)] 196)])]
  "TARGET_ALTIVEC"
  "lve<VI_char>x %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvxl"
  [(parallel
    [(set (match_operand:V4SI 0 "register_operand" "=v")
	  (match_operand:V4SI 1 "memory_operand" "m"))
     (unspec [(const_int 0)] UNSPEC_SET_VSCR)])]
  "TARGET_ALTIVEC"
  "lvxl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(match_operand:V4SI 1 "memory_operand" "m"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_stvx"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=m")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 201)])]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvxl"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=m")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 202)])]
  "TARGET_ALTIVEC"
  "stvxl %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stve<VI_char>x"
  [(parallel
    [(set (match_operand:VI 0 "memory_operand" "=m")
	  (match_operand:VI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 203)])]
  "TARGET_ALTIVEC"
  "stve<VI_char>x %1,%y0"
  [(set_attr "type" "vecstore")])

;; Generate
;;    vspltis? SCRATCH0,0
;;    vsubu?m SCRATCH2,SCRATCH1,%1
;;    vmaxs? %0,%1,SCRATCH2"
(define_expand "abs<mode>2"
  [(set (match_dup 2) (vec_duplicate:VI (const_int 0)))
   (set (match_dup 3)
        (minus:VI (match_dup 2)
                  (match_operand:VI 1 "register_operand" "v")))
   (set (match_operand:VI 0 "register_operand" "=v")
        (smax:VI (match_dup 1) (match_dup 3)))]
  "TARGET_ALTIVEC"
{
  operands[2] = gen_reg_rtx (GET_MODE (operands[0]));
  operands[3] = gen_reg_rtx (GET_MODE (operands[0]));
})

;; Generate
;;    vspltisw SCRATCH1,-1
;;    vslw SCRATCH2,SCRATCH1,SCRATCH1
;;    vandc %0,%1,SCRATCH2
(define_expand "absv4sf2"
  [(set (match_dup 2)
	(vec_duplicate:V4SF (float:SF (const_int -1))))
   (set (match_dup 3)
        (unspec:V4SF [(match_dup 2) (match_dup 2)] UNSPEC_VSLW))
   (set (match_operand:V4SF 0 "register_operand" "=v")
        (and:V4SF (not:V4SF (match_dup 3))
                  (match_operand:V4SF 1 "register_operand" "v")))]
  "TARGET_ALTIVEC"
{
  operands[2] = gen_reg_rtx (V4SFmode);
  operands[3] = gen_reg_rtx (V4SFmode);
})

;; Generate
;;    vspltis? SCRATCH0,0
;;    vsubs?s SCRATCH2,SCRATCH1,%1
;;    vmaxs? %0,%1,SCRATCH2"
(define_expand "altivec_abss_<mode>"
  [(set (match_dup 2) (vec_duplicate:VI (const_int 0)))
   (parallel [(set (match_dup 3)
		   (unspec:VI [(match_dup 2)
			       (match_operand:VI 1 "register_operand" "v")]
			      UNSPEC_SUBS))
              (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))])
   (set (match_operand:VI 0 "register_operand" "=v")
        (smax:VI (match_dup 1) (match_dup 3)))]
  "TARGET_ALTIVEC"
{
  operands[2] = gen_reg_rtx (GET_MODE (operands[0]));
  operands[3] = gen_reg_rtx (GET_MODE (operands[0]));
})

(define_insn "vec_realign_load_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")
                      (match_operand:V16QI 3 "register_operand" "v")] 216))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "vec_realign_load_<mode>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")
                    (match_operand:V16QI 3 "register_operand" "v")] 215))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])
