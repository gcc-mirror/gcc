;; ARM NEON coprocessor Machine Description
;; Copyright (C) 2006, 2007, 2008, 2009, 2010, 2012
;;   Free Software Foundation, Inc.
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

;; Enumerators for unspecs.
(define_c_enum "unspec" [
  UNSPEC_ASHIFT_SIGNED
  UNSPEC_ASHIFT_UNSIGNED
  UNSPEC_VABD
  UNSPEC_VABDL
  UNSPEC_VADD
  UNSPEC_VADDHN
  UNSPEC_VADDL
  UNSPEC_VADDW
  UNSPEC_VBSL
  UNSPEC_VCAGE
  UNSPEC_VCAGT
  UNSPEC_VCEQ
  UNSPEC_VCGE
  UNSPEC_VCGT
  UNSPEC_VCLS
  UNSPEC_VCONCAT
  UNSPEC_VCVT
  UNSPEC_VCVT_N
  UNSPEC_VEXT
  UNSPEC_VHADD
  UNSPEC_VHSUB
  UNSPEC_VLD1
  UNSPEC_VLD1_DUP
  UNSPEC_VLD1_LANE
  UNSPEC_VLD2
  UNSPEC_VLD2_DUP
  UNSPEC_VLD2_LANE
  UNSPEC_VLD3
  UNSPEC_VLD3A
  UNSPEC_VLD3B
  UNSPEC_VLD3_DUP
  UNSPEC_VLD3_LANE
  UNSPEC_VLD4
  UNSPEC_VLD4A
  UNSPEC_VLD4B
  UNSPEC_VLD4_DUP
  UNSPEC_VLD4_LANE
  UNSPEC_VMAX
  UNSPEC_VMIN
  UNSPEC_VMLA
  UNSPEC_VMLAL
  UNSPEC_VMLA_LANE
  UNSPEC_VMLAL_LANE
  UNSPEC_VMLS
  UNSPEC_VMLSL
  UNSPEC_VMLS_LANE
  UNSPEC_VMLSL_LANE
  UNSPEC_VMOVL
  UNSPEC_VMOVN
  UNSPEC_VMUL
  UNSPEC_VMULL
  UNSPEC_VMUL_LANE
  UNSPEC_VMULL_LANE
  UNSPEC_VPADAL
  UNSPEC_VPADD
  UNSPEC_VPADDL
  UNSPEC_VPMAX
  UNSPEC_VPMIN
  UNSPEC_VPSMAX
  UNSPEC_VPSMIN
  UNSPEC_VPUMAX
  UNSPEC_VPUMIN
  UNSPEC_VQABS
  UNSPEC_VQADD
  UNSPEC_VQDMLAL
  UNSPEC_VQDMLAL_LANE
  UNSPEC_VQDMLSL
  UNSPEC_VQDMLSL_LANE
  UNSPEC_VQDMULH
  UNSPEC_VQDMULH_LANE
  UNSPEC_VQDMULL
  UNSPEC_VQDMULL_LANE
  UNSPEC_VQMOVN
  UNSPEC_VQMOVUN
  UNSPEC_VQNEG
  UNSPEC_VQSHL
  UNSPEC_VQSHL_N
  UNSPEC_VQSHLU_N
  UNSPEC_VQSHRN_N
  UNSPEC_VQSHRUN_N
  UNSPEC_VQSUB
  UNSPEC_VRECPE
  UNSPEC_VRECPS
  UNSPEC_VREV16
  UNSPEC_VREV32
  UNSPEC_VREV64
  UNSPEC_VRSQRTE
  UNSPEC_VRSQRTS
  UNSPEC_VSHL
  UNSPEC_VSHLL_N
  UNSPEC_VSHL_N
  UNSPEC_VSHR_N
  UNSPEC_VSHRN_N
  UNSPEC_VSLI
  UNSPEC_VSRA_N
  UNSPEC_VSRI
  UNSPEC_VST1
  UNSPEC_VST1_LANE
  UNSPEC_VST2
  UNSPEC_VST2_LANE
  UNSPEC_VST3
  UNSPEC_VST3A
  UNSPEC_VST3B
  UNSPEC_VST3_LANE
  UNSPEC_VST4
  UNSPEC_VST4A
  UNSPEC_VST4B
  UNSPEC_VST4_LANE
  UNSPEC_VSTRUCTDUMMY
  UNSPEC_VSUB
  UNSPEC_VSUBHN
  UNSPEC_VSUBL
  UNSPEC_VSUBW
  UNSPEC_VTBL
  UNSPEC_VTBX
  UNSPEC_VTRN1
  UNSPEC_VTRN2
  UNSPEC_VTST
  UNSPEC_VUZP1
  UNSPEC_VUZP2
  UNSPEC_VZIP1
  UNSPEC_VZIP2
  UNSPEC_MISALIGNED_ACCESS
  UNSPEC_VCLE
  UNSPEC_VCLT
])


;; Attribute used to permit string comparisons against <VQH_mnem> in
;; neon_type attribute definitions.
(define_attr "vqh_mnem" "vadd,vmin,vmax" (const_string "vadd"))

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VD 0 "nonimmediate_operand"
	  "=w,Uv,w, w,  ?r,?w,?r,?r, ?Us")
	(match_operand:VD 1 "general_operand"
	  " w,w, Dn,Uvi, w, r, r, Usi,r"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  if (which_alternative == 2)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = neon_immediate_valid_for_move (operands[1], <MODE>mode,
        &operands[1], &width);

      gcc_assert (is_valid != 0);

      if (width == 0)
        return "vmov.f32\t%P0, %1  @ <mode>";
      else
        sprintf (templ, "vmov.i%d\t%%P0, %%1  @ <mode>", width);

      return templ;
    }

  /* FIXME: If the memory layout is changed in big-endian mode, output_move_vfp
     below must be changed to output_move_neon (which will use the
     element/structure loads/stores), and the constraint changed to 'Um' instead
     of 'Uv'.  */

  switch (which_alternative)
    {
    case 0: return "vmov\t%P0, %P1  @ <mode>";
    case 1: case 3: return output_move_vfp (operands);
    case 2: gcc_unreachable ();
    case 4: return "vmov\t%Q0, %R0, %P1  @ <mode>";
    case 5: return "vmov\t%P0, %Q1, %R1  @ <mode>";
    default: return output_move_double (operands, true, NULL);
    }
}
 [(set_attr "neon_type" "neon_int_1,*,neon_vmov,*,neon_mrrc,neon_mcr_2_mcrr,*,*,*")
  (set_attr "type" "*,f_stored,*,f_loadd,*,*,alu,load2,store2")
  (set_attr "insn" "*,*,*,*,*,*,mov,*,*")
  (set_attr "length" "4,4,4,4,4,4,8,8,8")
  (set_attr "pool_range"     "*,*,*,1020,*,*,*,1020,*")
  (set_attr "neg_pool_range" "*,*,*,1004,*,*,*,1004,*")])

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VQXMOV 0 "nonimmediate_operand"
  	  "=w,Un,w, w,  ?r,?w,?r,?r,  ?Us")
	(match_operand:VQXMOV 1 "general_operand"
	  " w,w, Dn,Uni, w, r, r, Usi, r"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  if (which_alternative == 2)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = neon_immediate_valid_for_move (operands[1], <MODE>mode,
        &operands[1], &width);

      gcc_assert (is_valid != 0);

      if (width == 0)
        return "vmov.f32\t%q0, %1  @ <mode>";
      else
        sprintf (templ, "vmov.i%d\t%%q0, %%1  @ <mode>", width);

      return templ;
    }

  switch (which_alternative)
    {
    case 0: return "vmov\t%q0, %q1  @ <mode>";
    case 1: case 3: return output_move_neon (operands);
    case 2: gcc_unreachable ();
    case 4: return "vmov\t%Q0, %R0, %e1  @ <mode>\;vmov\t%J0, %K0, %f1";
    case 5: return "vmov\t%e0, %Q1, %R1  @ <mode>\;vmov\t%f0, %J1, %K1";
    default: return output_move_quad (operands);
    }
}
  [(set_attr "neon_type" "neon_int_1,neon_stm_2,neon_vmov,neon_ldm_2,\
                          neon_mrrc,neon_mcr_2_mcrr,*,*,*")
   (set_attr "type" "*,*,*,*,*,*,alu,load4,store4")
   (set_attr "insn" "*,*,*,*,*,*,mov,*,*")
   (set_attr "length" "4,8,4,8,8,8,16,8,16")
   (set_attr "pool_range" "*,*,*,1020,*,*,*,1020,*")
   (set_attr "neg_pool_range" "*,*,*,996,*,*,*,996,*")])

(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "")
	(match_operand:TI 1 "general_operand" ""))]
  "TARGET_NEON"
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (TImode, operands[1]);
    }
})

(define_expand "mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand" "")
	(match_operand:VSTRUCT 1 "general_operand" ""))]
  "TARGET_NEON"
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
})

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand"	"=w,Ut,w")
	(match_operand:VSTRUCT 1 "general_operand"	" w,w, Ut"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0: return "#";
    case 1: case 2: return output_move_neon (operands);
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1,neon_stm_2,neon_ldm_2")
   (set (attr "length") (symbol_ref "arm_attr_length_move_neon (insn)"))])

(define_split
  [(set (match_operand:EI 0 "s_register_operand" "")
	(match_operand:EI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[2], src[2];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (DImode, rdest + 4);
  src[1] = gen_rtx_REG (DImode, rsrc + 4);

  neon_disambiguate_copy (operands, dest, src, 2);
})

(define_split
  [(set (match_operand:OI 0 "s_register_operand" "")
	(match_operand:OI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[2], src[2];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (TImode, rdest + 4);
  src[1] = gen_rtx_REG (TImode, rsrc + 4);

  neon_disambiguate_copy (operands, dest, src, 2);
})

(define_split
  [(set (match_operand:CI 0 "s_register_operand" "")
	(match_operand:CI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[3], src[3];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (TImode, rdest + 4);
  src[1] = gen_rtx_REG (TImode, rsrc + 4);
  dest[2] = gen_rtx_REG (TImode, rdest + 8);
  src[2] = gen_rtx_REG (TImode, rsrc + 8);

  neon_disambiguate_copy (operands, dest, src, 3);
})

(define_split
  [(set (match_operand:XI 0 "s_register_operand" "")
	(match_operand:XI 1 "s_register_operand" ""))]
  "TARGET_NEON && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))]
{
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);
  rtx dest[4], src[4];

  dest[0] = gen_rtx_REG (TImode, rdest);
  src[0] = gen_rtx_REG (TImode, rsrc);
  dest[1] = gen_rtx_REG (TImode, rdest + 4);
  src[1] = gen_rtx_REG (TImode, rsrc + 4);
  dest[2] = gen_rtx_REG (TImode, rdest + 8);
  src[2] = gen_rtx_REG (TImode, rsrc + 8);
  dest[3] = gen_rtx_REG (TImode, rdest + 12);
  src[3] = gen_rtx_REG (TImode, rsrc + 12);

  neon_disambiguate_copy (operands, dest, src, 4);
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:VDQX 0 "neon_struct_or_register_operand")
	(unspec:VDQX [(match_operand:VDQX 1 "neon_struct_or_register_operand")]
		     UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  /* This pattern is not permitted to fail during expansion: if both arguments
     are non-registers (e.g. memory := constant, which can be created by the
     auto-vectorizer), force operand 1 into a register.  */
  if (!s_register_operand (operands[0], <MODE>mode)
      && !s_register_operand (operands[1], <MODE>mode))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn "*movmisalign<mode>_neon_store"
  [(set (match_operand:VDX 0 "neon_struct_operand"	       "=Um")
	(unspec:VDX [(match_operand:VDX 1 "s_register_operand" " w")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vst1.<V_sz_elem>\t{%P1}, %A0"
  [(set_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs")])

(define_insn "*movmisalign<mode>_neon_load"
  [(set (match_operand:VDX 0 "s_register_operand"		"=w")
	(unspec:VDX [(match_operand:VDX 1 "neon_struct_operand" " Um")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vld1.<V_sz_elem>\t{%P0}, %A1"
  [(set_attr "neon_type" "neon_vld1_1_2_regs")])

(define_insn "*movmisalign<mode>_neon_store"
  [(set (match_operand:VQX 0 "neon_struct_operand"	       "=Um")
	(unspec:VQX [(match_operand:VQX 1 "s_register_operand" " w")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vst1.<V_sz_elem>\t{%q1}, %A0"
  [(set_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs")])

(define_insn "*movmisalign<mode>_neon_load"
  [(set (match_operand:VQX 0 "s_register_operand"	         "=w")
	(unspec:VQX [(match_operand:VQX 1 "neon_struct_operand" " Um")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vld1.<V_sz_elem>\t{%q0}, %A1"
  [(set_attr "neon_type" "neon_vld1_1_2_regs")])

(define_insn "vec_set<mode>_internal"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
        (vec_merge:VD
          (vec_duplicate:VD
            (match_operand:<V_elem> 1 "s_register_operand" "r"))
          (match_operand:VD 3 "s_register_operand" "0")
          (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_NEON"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;
  if (BYTES_BIG_ENDIAN)
    elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
  operands[2] = GEN_INT (elt);
  
  return "vmov%?.<V_sz_elem>\t%P0[%c2], %1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_mcr")])

(define_insn "vec_set<mode>_internal"
  [(set (match_operand:VQ 0 "s_register_operand" "=w")
        (vec_merge:VQ
          (vec_duplicate:VQ
            (match_operand:<V_elem> 1 "s_register_operand" "r"))
          (match_operand:VQ 3 "s_register_operand" "0")
          (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_NEON"
{
  HOST_WIDE_INT elem = ffs ((int) INTVAL (operands[2])) - 1;
  int half_elts = GET_MODE_NUNITS (<MODE>mode) / 2;
  int elt = elem % half_elts;
  int hi = (elem / half_elts) * 2;
  int regno = REGNO (operands[0]);

  if (BYTES_BIG_ENDIAN)
    elt = half_elts - 1 - elt;

  operands[0] = gen_rtx_REG (<V_HALF>mode, regno + hi);
  operands[2] = GEN_INT (elt);

  return "vmov%?.<V_sz_elem>\t%P0[%c2], %1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_mcr")]
)

(define_insn "vec_setv2di_internal"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
        (vec_merge:V2DI
          (vec_duplicate:V2DI
            (match_operand:DI 1 "s_register_operand" "r"))
          (match_operand:V2DI 3 "s_register_operand" "0")
          (match_operand:SI 2 "immediate_operand" "i")))]
  "TARGET_NEON"
{
  HOST_WIDE_INT elem = ffs ((int) INTVAL (operands[2])) - 1;
  int regno = REGNO (operands[0]) + 2 * elem;

  operands[0] = gen_rtx_REG (DImode, regno);

  return "vmov%?\t%P0, %Q1, %R1";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_mcr_2_mcrr")]
)

(define_expand "vec_set<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand:<V_elem> 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);
  emit_insn (gen_vec_set<mode>_internal (operands[0], operands[1],
					 GEN_INT (elem), operands[0]));
  DONE;
})

(define_insn "vec_extract<mode>"
  [(set (match_operand:<V_elem> 0 "s_register_operand" "=r")
        (vec_select:<V_elem>
          (match_operand:VD 1 "s_register_operand" "w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov%?.<V_uf_sclr>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "vec_extract<mode>"
  [(set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(vec_select:<V_elem>
          (match_operand:VQ 1 "s_register_operand" "w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_NEON"
{
  int half_elts = GET_MODE_NUNITS (<MODE>mode) / 2;
  int elt = INTVAL (operands[2]) % half_elts;
  int hi = (INTVAL (operands[2]) / half_elts) * 2;
  int regno = REGNO (operands[1]);

  if (BYTES_BIG_ENDIAN)
    elt = half_elts - 1 - elt;

  operands[1] = gen_rtx_REG (<V_HALF>mode, regno + hi);
  operands[2] = GEN_INT (elt);

  return "vmov%?.<V_uf_sclr>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "vec_extractv2di"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(vec_select:DI
          (match_operand:V2DI 1 "s_register_operand" "w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[1]) + 2 * INTVAL (operands[2]);

  operands[1] = gen_rtx_REG (DImode, regno);

  return "vmov%?\t%Q0, %R0, %P1  @ v2di";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_int_1")]
)

(define_expand "vec_init<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand 1 "" "")]
  "TARGET_NEON"
{
  neon_expand_vector_init (operands[0], operands[1]);
  DONE;
})

;; Doubleword and quadword arithmetic.

;; NOTE: some other instructions also support 64-bit integer
;; element size, which we could potentially use for "long long" operations.

(define_insn "*add<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (plus:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		  (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
  "vadd.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_1")))]
)

(define_insn "adddi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,?&r,?&r,?w")
        (plus:DI (match_operand:DI 1 "s_register_operand" "%w,0,0,w")
                 (match_operand:DI 2 "s_register_operand" "w,r,0,w")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: /* fall through */
    case 3: return "vadd.i64\t%P0, %P1, %P2";
    case 1: return "#";
    case 2: return "#";
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1,*,*,neon_int_1")
   (set_attr "conds" "*,clob,clob,*")
   (set_attr "length" "*,8,8,*")
   (set_attr "arch" "nota8,*,*,onlya8")]
)

(define_insn "*sub<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (minus:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
                   (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
  "vsub.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_2")))]
)

(define_insn "subdi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,?&r,?&r,?&r,?w")
        (minus:DI (match_operand:DI 1 "s_register_operand" "w,0,r,0,w")
                  (match_operand:DI 2 "s_register_operand" "w,r,0,0,w")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: /* fall through */
    case 4: return "vsub.i64\t%P0, %P1, %P2";
    case 1: /* fall through */ 
    case 2: /* fall through */
    case 3: return  "subs\\t%Q0, %Q1, %Q2\;sbc\\t%R0, %R1, %R2";
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_2,*,*,*,neon_int_2")
   (set_attr "conds" "*,clob,clob,clob,*")
   (set_attr "length" "*,8,8,8,*")
   (set_attr "arch" "nota8,*,*,*,onlya8")]
)

(define_insn "*mul<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (mult:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
                  (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
  "vmul.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (if_then_else (match_test "<Is_d_reg>")
                                  (if_then_else
                                    (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32"))
                                  (if_then_else (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")))))]
)

(define_insn "mul<mode>3add<mode>_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (plus:VDQ (mult:VDQ (match_operand:VDQ 2 "s_register_operand" "w")
                            (match_operand:VDQ 3 "s_register_operand" "w"))
		  (match_operand:VDQ 1 "s_register_operand" "0")))]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
  "vmla.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (match_test "<Is_d_reg>")
                                  (if_then_else
                                    (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "mul<mode>3neg<mode>add<mode>_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (minus:VDQ (match_operand:VDQ 1 "s_register_operand" "0")
                   (mult:VDQ (match_operand:VDQ 2 "s_register_operand" "w")
                             (match_operand:VDQ 3 "s_register_operand" "w"))))]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
  "vmls.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (match_test "<Is_d_reg>")
                                  (if_then_else
                                    (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "ior<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w,w")
	(ior:VDQ (match_operand:VDQ 1 "s_register_operand" "w,0")
		 (match_operand:VDQ 2 "neon_logic_op2" "w,Dl")))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: return "vorr\t%<V_reg>0, %<V_reg>1, %<V_reg>2";
    case 1: return neon_output_logic_immediate ("vorr", &operands[2],
		     <MODE>mode, 0, VALID_NEON_QREG_MODE (<MODE>mode));
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "iordi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,w,?&r,?&r,?w,?w")
        (ior:DI (match_operand:DI 1 "s_register_operand" "%w,0,0,r,w,0")
		(match_operand:DI 2 "neon_logic_op2" "w,Dl,r,r,w,Dl")))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: /* fall through */
    case 4: return "vorr\t%P0, %P1, %P2";
    case 1: /* fall through */
    case 5: return neon_output_logic_immediate ("vorr", &operands[2],
		     DImode, 0, VALID_NEON_QREG_MODE (DImode));
    case 2: return "#";
    case 3: return "#";
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1,neon_int_1,*,*,neon_int_1,neon_int_1")
   (set_attr "length" "*,*,8,8,*,*")
   (set_attr "arch" "nota8,nota8,*,*,onlya8,onlya8")]
)

;; The concrete forms of the Neon immediate-logic instructions are vbic and
;; vorr. We support the pseudo-instruction vand instead, because that
;; corresponds to the canonical form the middle-end expects to use for
;; immediate bitwise-ANDs.

(define_insn "and<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w,w")
	(and:VDQ (match_operand:VDQ 1 "s_register_operand" "w,0")
		 (match_operand:VDQ 2 "neon_inv_logic_op2" "w,DL")))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: return "vand\t%<V_reg>0, %<V_reg>1, %<V_reg>2";
    case 1: return neon_output_logic_immediate ("vand", &operands[2],
    		     <MODE>mode, 1, VALID_NEON_QREG_MODE (<MODE>mode));
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "anddi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,w,?&r,?&r,?w,?w")
        (and:DI (match_operand:DI 1 "s_register_operand" "%w,0,0,r,w,0")
		(match_operand:DI 2 "neon_inv_logic_op2" "w,DL,r,r,w,DL")))]
  "TARGET_NEON"
{
  switch (which_alternative)
    {
    case 0: /* fall through */
    case 4: return "vand\t%P0, %P1, %P2";
    case 1: /* fall through */
    case 5: return neon_output_logic_immediate ("vand", &operands[2],
    		     DImode, 1, VALID_NEON_QREG_MODE (DImode));
    case 2: return "#";
    case 3: return "#";
    default: gcc_unreachable ();
    }
}
  [(set_attr "neon_type" "neon_int_1,neon_int_1,*,*,neon_int_1,neon_int_1")
   (set_attr "length" "*,*,8,8,*,*")
   (set_attr "arch" "nota8,nota8,*,*,onlya8,onlya8")]
)

(define_insn "orn<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(ior:VDQ (not:VDQ (match_operand:VDQ 2 "s_register_operand" "w"))
		 (match_operand:VDQ 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vorn\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_1")]
)

;; TODO: investigate whether we should disable 
;; this and bicdi3_neon for the A8 in line with the other
;; changes above. 
(define_insn_and_split "orndi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,?&r,?&r,?&r")
	(ior:DI (not:DI (match_operand:DI 2 "s_register_operand" "w,0,0,r"))
		(match_operand:DI 1 "s_register_operand" "w,r,r,0")))]
  "TARGET_NEON"
  "@
   vorn\t%P0, %P1, %P2
   #
   #
   #"
  "reload_completed && 
   (TARGET_NEON && !(IS_VFP_REGNUM (REGNO (operands[0]))))"
  [(set (match_dup 0) (ior:SI (not:SI (match_dup 2)) (match_dup 1)))
   (set (match_dup 3) (ior:SI (not:SI (match_dup 4)) (match_dup 5)))]
  "
  {
    if (TARGET_THUMB2)
      {
        operands[3] = gen_highpart (SImode, operands[0]);
        operands[0] = gen_lowpart (SImode, operands[0]);
        operands[4] = gen_highpart (SImode, operands[2]);
        operands[2] = gen_lowpart (SImode, operands[2]);
        operands[5] = gen_highpart (SImode, operands[1]);
        operands[1] = gen_lowpart (SImode, operands[1]);
      }
    else
      {
        emit_insn (gen_one_cmpldi2 (operands[0], operands[2]));
        emit_insn (gen_iordi3 (operands[0], operands[1], operands[0]));
        DONE;
      }
  }"
  [(set_attr "neon_type" "neon_int_1,*,*,*")
   (set_attr "length" "*,16,8,8")
   (set_attr "arch" "any,a,t2,t2")]
)

(define_insn "bic<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(and:VDQ (not:VDQ (match_operand:VDQ 2 "s_register_operand" "w"))
		 (match_operand:VDQ 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vbic\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_1")]
)

;; Compare to *anddi_notdi_di.
(define_insn "bicdi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,?=&r,?&r")
        (and:DI (not:DI (match_operand:DI 2 "s_register_operand" "w,r,0"))
		(match_operand:DI 1 "s_register_operand" "w,0,r")))]
  "TARGET_NEON"
  "@
   vbic\t%P0, %P1, %P2
   #
   #"
  [(set_attr "neon_type" "neon_int_1,*,*")
   (set_attr "length" "*,8,8")]
)

(define_insn "xor<mode>3"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(xor:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		 (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "veor\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "xordi3_neon"
  [(set (match_operand:DI 0 "s_register_operand" "=w,?&r,?&r,?w")
        (xor:DI (match_operand:DI 1 "s_register_operand" "%w,0,r,w")
	        (match_operand:DI 2 "s_register_operand" "w,r,r,w")))]
  "TARGET_NEON"
  "@
   veor\t%P0, %P1, %P2
   #
   #
   veor\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_1,*,*,neon_int_1")
   (set_attr "length" "*,8,8,*")
   (set_attr "arch" "nota8,*,*,onlya8")]
)

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (not:VDQ (match_operand:VDQ 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmvn\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "abs<mode>2"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(abs:VDQW (match_operand:VDQW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_3")))]
)

(define_insn "neg<mode>2"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(neg:VDQW (match_operand:VDQW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vneg.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_3")))]
)

(define_insn "*umin<mode>3_neon"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(umin:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		    (match_operand:VDQIW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmin.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "*umax<mode>3_neon"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(umax:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		    (match_operand:VDQIW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmax.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "*smin<mode>3_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(smin:VDQW (match_operand:VDQW 1 "s_register_operand" "w")
		   (match_operand:VDQW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmin.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

(define_insn "*smax<mode>3_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(smax:VDQW (match_operand:VDQW 1 "s_register_operand" "w")
		   (match_operand:VDQW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmax.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

; TODO: V2DI shifts are current disabled because there are bugs in the
; generic vectorizer code.  It ends up creating a V2DI constructor with
; SImode elements.

(define_insn "vashl<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w,w")
	(ashift:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w,w")
		      (match_operand:VDQIW 2 "imm_lshift_or_reg_neon" "w,Dn")))]
  "TARGET_NEON"
  {
    switch (which_alternative)
      {
        case 0: return "vshl.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2";
        case 1: return neon_output_shift_immediate ("vshl", 'i', &operands[2],
                         			    <MODE>mode,
						    VALID_NEON_QREG_MODE (<MODE>mode),
						    true);
        default: gcc_unreachable ();
      }
  }
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

(define_insn "vashr<mode>3_imm"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(ashiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
			(match_operand:VDQIW 2 "imm_for_neon_rshift_operand" "Dn")))]
  "TARGET_NEON"
  {
    return neon_output_shift_immediate ("vshr", 's', &operands[2],
					<MODE>mode, VALID_NEON_QREG_MODE (<MODE>mode),
					false);
  }
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

(define_insn "vlshr<mode>3_imm"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(lshiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
			(match_operand:VDQIW 2 "imm_for_neon_rshift_operand" "Dn")))]
  "TARGET_NEON"
  {
    return neon_output_shift_immediate ("vshr", 'u', &operands[2],
					<MODE>mode, VALID_NEON_QREG_MODE (<MODE>mode),
					false);
  }              
  [(set (attr "neon_type")
	(if_then_else (match_test "<Is_d_reg>")
		      (const_string "neon_vshl_ddd")
		      (const_string "neon_shift_3")))]
)

; Used for implementing logical shift-right, which is a left-shift by a negative
; amount, with signed operands. This is essentially the same as ashl<mode>3
; above, but using an unspec in case GCC tries anything tricky with negative
; shift amounts.

(define_insn "ashl<mode>3_signed"
  [(set (match_operand:VDQI 0 "s_register_operand" "=w")
	(unspec:VDQI [(match_operand:VDQI 1 "s_register_operand" "w")
		      (match_operand:VDQI 2 "s_register_operand" "w")]
		     UNSPEC_ASHIFT_SIGNED))]
  "TARGET_NEON"
  "vshl.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

; Used for implementing logical shift-right, which is a left-shift by a negative
; amount, with unsigned operands.

(define_insn "ashl<mode>3_unsigned"
  [(set (match_operand:VDQI 0 "s_register_operand" "=w")
	(unspec:VDQI [(match_operand:VDQI 1 "s_register_operand" "w")
		      (match_operand:VDQI 2 "s_register_operand" "w")]
		     UNSPEC_ASHIFT_UNSIGNED))]
  "TARGET_NEON"
  "vshl.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

(define_expand "vashr<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand" "")
	(ashiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "")
			(match_operand:VDQIW 2 "imm_rshift_or_reg_neon" "")))]
  "TARGET_NEON"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  if (REG_P (operands[2]))
    {
      emit_insn (gen_neg<mode>2 (neg, operands[2]));
      emit_insn (gen_ashl<mode>3_signed (operands[0], operands[1], neg));
    }
  else
    emit_insn (gen_vashr<mode>3_imm (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vlshr<mode>3"
  [(set (match_operand:VDQIW 0 "s_register_operand" "")
	(lshiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "")
			(match_operand:VDQIW 2 "imm_rshift_or_reg_neon" "")))]
  "TARGET_NEON"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  if (REG_P (operands[2]))
    {
      emit_insn (gen_neg<mode>2 (neg, operands[2]));
      emit_insn (gen_ashl<mode>3_unsigned (operands[0], operands[1], neg));
    }
  else
    emit_insn (gen_vlshr<mode>3_imm (operands[0], operands[1], operands[2]));
  DONE;
})

;; Widening operations

(define_insn "widen_ssum<mode>3"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(plus:<V_widen> (sign_extend:<V_widen>
			  (match_operand:VW 1 "s_register_operand" "%w"))
		        (match_operand:<V_widen> 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vaddw.<V_s_elem>\t%q0, %q2, %P1"
  [(set_attr "neon_type" "neon_int_3")]
)

(define_insn "widen_usum<mode>3"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(plus:<V_widen> (zero_extend:<V_widen>
			  (match_operand:VW 1 "s_register_operand" "%w"))
		        (match_operand:<V_widen> 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vaddw.<V_u_elem>\t%q0, %q2, %P1"
  [(set_attr "neon_type" "neon_int_3")]
)

;; VEXT can be used to synthesize coarse whole-vector shifts with 8-bit
;; shift-count granularity. That's good enough for the middle-end's current
;; needs.

;; Note that it's not safe to perform such an operation in big-endian mode,
;; due to element-ordering issues.

(define_expand "vec_shr_<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand:VDQ 1 "s_register_operand" "")
   (match_operand:SI 2 "const_multiple_of_8_operand" "")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx zero_reg;
  HOST_WIDE_INT num_bits = INTVAL (operands[2]);
  const int width = GET_MODE_BITSIZE (<MODE>mode);
  const enum machine_mode bvecmode = (width == 128) ? V16QImode : V8QImode;
  rtx (*gen_ext) (rtx, rtx, rtx, rtx) =
    (width == 128) ? gen_neon_vextv16qi : gen_neon_vextv8qi;

  if (num_bits == width)
    {
      emit_move_insn (operands[0], operands[1]);
      DONE;
    }

  zero_reg = force_reg (bvecmode, CONST0_RTX (bvecmode));
  operands[0] = gen_lowpart (bvecmode, operands[0]);
  operands[1] = gen_lowpart (bvecmode, operands[1]);

  emit_insn (gen_ext (operands[0], operands[1], zero_reg,
		      GEN_INT (num_bits / BITS_PER_UNIT)));
  DONE;
})

(define_expand "vec_shl_<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "")
   (match_operand:VDQ 1 "s_register_operand" "")
   (match_operand:SI 2 "const_multiple_of_8_operand" "")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx zero_reg;
  HOST_WIDE_INT num_bits = INTVAL (operands[2]);
  const int width = GET_MODE_BITSIZE (<MODE>mode);
  const enum machine_mode bvecmode = (width == 128) ? V16QImode : V8QImode;
  rtx (*gen_ext) (rtx, rtx, rtx, rtx) =
    (width == 128) ? gen_neon_vextv16qi : gen_neon_vextv8qi;

  if (num_bits == 0)
    {
      emit_move_insn (operands[0], CONST0_RTX (<MODE>mode));
      DONE;
    }

  num_bits = width - num_bits;

  zero_reg = force_reg (bvecmode, CONST0_RTX (bvecmode));
  operands[0] = gen_lowpart (bvecmode, operands[0]);
  operands[1] = gen_lowpart (bvecmode, operands[1]);

  emit_insn (gen_ext (operands[0], zero_reg, operands[1],
		      GEN_INT (num_bits / BITS_PER_UNIT)));
  DONE;
})

;; Helpers for quad-word reduction operations

; Add (or smin, smax...) the low N/2 elements of the N-element vector
; operand[1] to the high N/2 elements of same. Put the result in operand[0], an
; N/2-element vector.

(define_insn "quad_halves_<code>v4si"
  [(set (match_operand:V2SI 0 "s_register_operand" "=w")
        (vqh_ops:V2SI
          (vec_select:V2SI (match_operand:V4SI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)]))
          (vec_select:V2SI (match_dup 1)
                           (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>32\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

(define_insn "quad_halves_<code>v4sf"
  [(set (match_operand:V2SF 0 "s_register_operand" "=w")
        (vqhs_ops:V2SF
          (vec_select:V2SF (match_operand:V4SF 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)]))
          (vec_select:V2SF (match_dup 1)
                           (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_NEON && flag_unsafe_math_optimizations"
  "<VQH_mnem>.f32\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

(define_insn "quad_halves_<code>v8hi"
  [(set (match_operand:V4HI 0 "s_register_operand" "+w")
        (vqh_ops:V4HI
          (vec_select:V4HI (match_operand:V8HI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)
				      (const_int 2) (const_int 3)]))
          (vec_select:V4HI (match_dup 1)
                           (parallel [(const_int 4) (const_int 5)
				      (const_int 6) (const_int 7)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>16\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

(define_insn "quad_halves_<code>v16qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "+w")
        (vqh_ops:V8QI
          (vec_select:V8QI (match_operand:V16QI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)
				      (const_int 2) (const_int 3)
				      (const_int 4) (const_int 5)
				      (const_int 6) (const_int 7)]))
          (vec_select:V8QI (match_dup 1)
                           (parallel [(const_int 8) (const_int 9)
				      (const_int 10) (const_int 11)
				      (const_int 12) (const_int 13)
				      (const_int 14) (const_int 15)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>8\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set (attr "neon_type")
      (if_then_else (eq_attr "vqh_mnem" "vadd")
                    (const_string "neon_int_1") (const_string "neon_int_5")))]
)

(define_expand "move_hi_quad_<mode>"
 [(match_operand:ANY128 0 "s_register_operand" "")
  (match_operand:<V_HALF> 1 "s_register_operand" "")]
 "TARGET_NEON"
{
  emit_move_insn (simplify_gen_subreg (<V_HALF>mode, operands[0], <MODE>mode,
				       GET_MODE_SIZE (<V_HALF>mode)),
		  operands[1]);
  DONE;
})

(define_expand "move_lo_quad_<mode>"
 [(match_operand:ANY128 0 "s_register_operand" "")
  (match_operand:<V_HALF> 1 "s_register_operand" "")]
 "TARGET_NEON"
{
  emit_move_insn (simplify_gen_subreg (<V_HALF>mode, operands[0],
				       <MODE>mode, 0),
		  operands[1]);
  DONE;
})

;; Reduction operations

(define_expand "reduc_splus_<mode>"
  [(match_operand:VD 0 "s_register_operand" "")
   (match_operand:VD 1 "s_register_operand" "")]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpadd_internal<mode>);
  DONE;
})

(define_expand "reduc_splus_<mode>"
  [(match_operand:VQ 0 "s_register_operand" "")
   (match_operand:VQ 1 "s_register_operand" "")]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)
   && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_plus<mode> (step1, operands[1]));
  emit_insn (gen_reduc_splus_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_insn "reduc_splus_v2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
	(unspec:V2DI [(match_operand:V2DI 1 "s_register_operand" "w")]
		     UNSPEC_VPADD))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vadd.i64\t%e0, %e1, %f1"
  [(set_attr "neon_type" "neon_int_1")]
)

;; NEON does not distinguish between signed and unsigned addition except on
;; widening operations.
(define_expand "reduc_uplus_<mode>"
  [(match_operand:VDQI 0 "s_register_operand" "")
   (match_operand:VDQI 1 "s_register_operand" "")]
  "TARGET_NEON && (<Is_d_reg> || !BYTES_BIG_ENDIAN)"
{
  emit_insn (gen_reduc_splus_<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "reduc_smin_<mode>"
  [(match_operand:VD 0 "s_register_operand" "")
   (match_operand:VD 1 "s_register_operand" "")]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpsmin<mode>);
  DONE;
})

(define_expand "reduc_smin_<mode>"
  [(match_operand:VQ 0 "s_register_operand" "")
   (match_operand:VQ 1 "s_register_operand" "")]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)
   && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_smin<mode> (step1, operands[1]));
  emit_insn (gen_reduc_smin_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_expand "reduc_smax_<mode>"
  [(match_operand:VD 0 "s_register_operand" "")
   (match_operand:VD 1 "s_register_operand" "")]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpsmax<mode>);
  DONE;
})

(define_expand "reduc_smax_<mode>"
  [(match_operand:VQ 0 "s_register_operand" "")
   (match_operand:VQ 1 "s_register_operand" "")]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)
   && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_smax<mode> (step1, operands[1]));
  emit_insn (gen_reduc_smax_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_expand "reduc_umin_<mode>"
  [(match_operand:VDI 0 "s_register_operand" "")
   (match_operand:VDI 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpumin<mode>);
  DONE;
})

(define_expand "reduc_umin_<mode>"
  [(match_operand:VQI 0 "s_register_operand" "")
   (match_operand:VQI 1 "s_register_operand" "")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_umin<mode> (step1, operands[1]));
  emit_insn (gen_reduc_umin_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_expand "reduc_umax_<mode>"
  [(match_operand:VDI 0 "s_register_operand" "")
   (match_operand:VDI 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_pairwise_reduce (operands[0], operands[1], <MODE>mode,
			&gen_neon_vpumax<mode>);
  DONE;
})

(define_expand "reduc_umax_<mode>"
  [(match_operand:VQI 0 "s_register_operand" "")
   (match_operand:VQI 1 "s_register_operand" "")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);
  rtx res_d = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_umax<mode> (step1, operands[1]));
  emit_insn (gen_reduc_umax_<V_half> (res_d, step1));
  emit_insn (gen_move_lo_quad_<mode> (operands[0], res_d));

  DONE;
})

(define_insn "neon_vpadd_internal<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPADD))]
  "TARGET_NEON"
  "vpadd.<V_if_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like vadd.
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_1")))]
)

(define_insn "neon_vpsmin<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPSMIN))]
  "TARGET_NEON"
  "vpmin.<V_s_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like vmin.
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vpsmax<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPSMAX))]
  "TARGET_NEON"
  "vpmax.<V_s_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like vmax.
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vpumin<mode>"
  [(set (match_operand:VDI 0 "s_register_operand" "=w")
	(unspec:VDI [(match_operand:VDI 1 "s_register_operand" "w")
		     (match_operand:VDI 2 "s_register_operand" "w")]
                   UNSPEC_VPUMIN))]
  "TARGET_NEON"
  "vpmin.<V_u_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like umin.
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vpumax<mode>"
  [(set (match_operand:VDI 0 "s_register_operand" "=w")
	(unspec:VDI [(match_operand:VDI 1 "s_register_operand" "w")
		     (match_operand:VDI 2 "s_register_operand" "w")]
                   UNSPEC_VPUMAX))]
  "TARGET_NEON"
  "vpmax.<V_u_elem>\t%P0, %P1, %P2"
  ;; Assume this schedules like umax.
  [(set_attr "neon_type" "neon_int_5")]
)

;; Saturating arithmetic

; NOTE: Neon supports many more saturating variants of instructions than the
; following, but these are all GCC currently understands.
; FIXME: Actually, GCC doesn't know how to create saturating add/sub by itself
; yet either, although these patterns may be used by intrinsics when they're
; added.

(define_insn "*ss_add<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (ss_plus:VD (match_operand:VD 1 "s_register_operand" "w")
                   (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqadd.<V_s_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "*us_add<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (us_plus:VD (match_operand:VD 1 "s_register_operand" "w")
                   (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqadd.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "*ss_sub<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (ss_minus:VD (match_operand:VD 1 "s_register_operand" "w")
                    (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqsub.<V_s_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "*us_sub<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (us_minus:VD (match_operand:VD 1 "s_register_operand" "w")
                    (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqsub.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_5")]
)

;; Conditional instructions.  These are comparisons with conditional moves for
;; vectors.  They perform the assignment:
;;   
;;     Vop0 = (Vop4 <op3> Vop5) ? Vop1 : Vop2;
;;
;; where op3 is <, <=, ==, !=, >= or >.  Operations are performed
;; element-wise.

(define_expand "vcond<mode><mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "")
	(if_then_else:VDQW
	  (match_operator 3 "arm_comparison_operator"
	    [(match_operand:VDQW 4 "s_register_operand" "")
	     (match_operand:VDQW 5 "nonmemory_operand" "")])
	  (match_operand:VDQW 1 "s_register_operand" "")
	  (match_operand:VDQW 2 "s_register_operand" "")))]
  "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
{
  rtx mask;
  int inverse = 0, immediate_zero = 0;
  /* See the description of "magic" bits in the 'T' case of
     arm_print_operand.  */
  HOST_WIDE_INT magic_word = (<MODE>mode == V2SFmode || <MODE>mode == V4SFmode)
			     ? 3 : 1;
  rtx magic_rtx = GEN_INT (magic_word);
  
  mask = gen_reg_rtx (<V_cmp_result>mode);
  
  if (operands[5] == CONST0_RTX (<MODE>mode))
    immediate_zero = 1;
  else if (!REG_P (operands[5]))
    operands[5] = force_reg (<MODE>mode, operands[5]);
  
  switch (GET_CODE (operands[3]))
    {
    case GE:
      emit_insn (gen_neon_vcge<mode> (mask, operands[4], operands[5],
				      magic_rtx));
      break;
    
    case GT:
      emit_insn (gen_neon_vcgt<mode> (mask, operands[4], operands[5],
				      magic_rtx));
      break;
    
    case EQ:
      emit_insn (gen_neon_vceq<mode> (mask, operands[4], operands[5],
				      magic_rtx));
      break;
    
    case LE:
      if (immediate_zero)
	emit_insn (gen_neon_vcle<mode> (mask, operands[4], operands[5],
					magic_rtx));
      else
	emit_insn (gen_neon_vcge<mode> (mask, operands[5], operands[4],
					magic_rtx));
      break;
    
    case LT:
      if (immediate_zero)
	emit_insn (gen_neon_vclt<mode> (mask, operands[4], operands[5],
					magic_rtx));
      else
	emit_insn (gen_neon_vcgt<mode> (mask, operands[5], operands[4],
					magic_rtx));
      break;
    
    case NE:
      emit_insn (gen_neon_vceq<mode> (mask, operands[4], operands[5],
				      magic_rtx));
      inverse = 1;
      break;
    
    default:
      gcc_unreachable ();
    }
  
  if (inverse)
    emit_insn (gen_neon_vbsl<mode> (operands[0], mask, operands[2],
				    operands[1]));
  else
    emit_insn (gen_neon_vbsl<mode> (operands[0], mask, operands[1],
				    operands[2]));

  DONE;
})

(define_expand "vcondu<mode><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "")
	(if_then_else:VDQIW
	  (match_operator 3 "arm_comparison_operator"
	    [(match_operand:VDQIW 4 "s_register_operand" "")
	     (match_operand:VDQIW 5 "s_register_operand" "")])
	  (match_operand:VDQIW 1 "s_register_operand" "")
	  (match_operand:VDQIW 2 "s_register_operand" "")))]
  "TARGET_NEON"
{
  rtx mask;
  int inverse = 0, immediate_zero = 0;
  
  mask = gen_reg_rtx (<V_cmp_result>mode);
  
  if (operands[5] == CONST0_RTX (<MODE>mode))
    immediate_zero = 1;
  else if (!REG_P (operands[5]))
    operands[5] = force_reg (<MODE>mode, operands[5]);
  
  switch (GET_CODE (operands[3]))
    {
    case GEU:
      emit_insn (gen_neon_vcge<mode> (mask, operands[4], operands[5],
				      const0_rtx));
      break;
    
    case GTU:
      emit_insn (gen_neon_vcgt<mode> (mask, operands[4], operands[5],
				      const0_rtx));
      break;
    
    case EQ:
      emit_insn (gen_neon_vceq<mode> (mask, operands[4], operands[5],
				      const0_rtx));
      break;
    
    case LEU:
      if (immediate_zero)
	emit_insn (gen_neon_vcle<mode> (mask, operands[4], operands[5],
					const0_rtx));
      else
	emit_insn (gen_neon_vcge<mode> (mask, operands[5], operands[4],
					const0_rtx));
      break;
    
    case LTU:
      if (immediate_zero)
        emit_insn (gen_neon_vclt<mode> (mask, operands[4], operands[5],
					const0_rtx));
      else
	emit_insn (gen_neon_vcgt<mode> (mask, operands[5], operands[4],
					const0_rtx));
      break;
    
    case NE:
      emit_insn (gen_neon_vceq<mode> (mask, operands[4], operands[5],
				      const0_rtx));
      inverse = 1;
      break;
    
    default:
      gcc_unreachable ();
    }
  
  if (inverse)
    emit_insn (gen_neon_vbsl<mode> (operands[0], mask, operands[2],
				    operands[1]));
  else
    emit_insn (gen_neon_vbsl<mode> (operands[0], mask, operands[1],
				    operands[2]));

  DONE;
})

;; Patterns for builtins.

; good for plain vadd, vaddq.

(define_expand "neon_vadd<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "=w")
   (match_operand:VDQX 1 "s_register_operand" "w")
   (match_operand:VDQX 2 "s_register_operand" "w")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  if (!<Is_float_mode> || flag_unsafe_math_optimizations)
    emit_insn (gen_add<mode>3 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vadd<mode>_unspec (operands[0], operands[1],
					   operands[2]));
  DONE;
})

; Note that NEON operations don't support the full IEEE 754 standard: in
; particular, denormal values are flushed to zero.  This means that GCC cannot
; use those instructions for autovectorization, etc. unless
; -funsafe-math-optimizations is in effect (in which case flush-to-zero
; behaviour is permissible).  Intrinsic operations (provided by the arm_neon.h
; header) must work in either case: if -funsafe-math-optimizations is given,
; intrinsics expand to "canonical" RTL where possible, otherwise intrinsics
; expand to unspecs (which may potentially limit the extent to which they might
; be optimized by generic code).

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vadd<mode>_unspec"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
        (unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")
		      (match_operand:VDQX 2 "s_register_operand" "w")]
                     UNSPEC_VADD))]
  "TARGET_NEON"
  "vadd.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_1")))]
)

; operand 3 represents in bits:
;  bit 0: signed (vs unsigned).
;  bit 1: rounding (vs none).

(define_insn "neon_vaddl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VDI 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VADDL))]
  "TARGET_NEON"
  "vaddl.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_3")]
)

(define_insn "neon_vaddw<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VADDW))]
  "TARGET_NEON"
  "vaddw.%T3%#<V_sz_elem>\t%q0, %q1, %P2"
  [(set_attr "neon_type" "neon_int_2")]
)

; vhadd and vrhadd.

(define_insn "neon_vhadd<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")
		       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VHADD))]
  "TARGET_NEON"
  "v%O3hadd.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vqadd<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
        (unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VQADD))]
  "TARGET_NEON"
  "vqadd.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vaddhn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
        (unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
		            (match_operand:VN 2 "s_register_operand" "w")
                            (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VADDHN))]
  "TARGET_NEON"
  "v%O3addhn.<V_if_elem>\t%P0, %q1, %q2"
  [(set_attr "neon_type" "neon_int_4")]
)

;; We cannot replace this unspec with mul<mode>3 because of the odd 
;; polynomial multiplication case that can specified by operand 3.
(define_insn "neon_vmul<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
		     UNSPEC_VMUL))]
  "TARGET_NEON"
  "vmul.%F3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (if_then_else (match_test "<Is_d_reg>")
                                  (if_then_else
                                    (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32"))
                                  (if_then_else (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")
                                    (const_string "neon_mul_qqq_8_16_32_ddd_32")))))]
)

(define_expand "neon_vmla<mode>"
  [(match_operand:VDQW 0 "s_register_operand" "=w")
   (match_operand:VDQW 1 "s_register_operand" "0")
   (match_operand:VDQW 2 "s_register_operand" "w")
   (match_operand:VDQW 3 "s_register_operand" "w")
   (match_operand:SI 4 "immediate_operand" "i")]
  "TARGET_NEON"
{
  if (!<Is_float_mode> || flag_unsafe_math_optimizations)
    emit_insn (gen_mul<mode>3add<mode>_neon (operands[0], operands[1],
				             operands[2], operands[3]));
  else
    emit_insn (gen_neon_vmla<mode>_unspec (operands[0], operands[1],
					   operands[2], operands[3]));
  DONE;
})

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vmla<mode>_unspec"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(unspec:VDQ [(match_operand:VDQ 1 "s_register_operand" "0")
		     (match_operand:VDQ 2 "s_register_operand" "w")
		     (match_operand:VDQ 3 "s_register_operand" "w")]
		    UNSPEC_VMLA))]
  "TARGET_NEON"
  "vmla.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (match_test "<Is_d_reg>")
                                  (if_then_else
                                    (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "neon_vmlal<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VW 2 "s_register_operand" "w")
		           (match_operand:VW 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VMLAL))]
  "TARGET_NEON"
  "vmlal.%T4%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_expand "neon_vmls<mode>"
  [(match_operand:VDQW 0 "s_register_operand" "=w")
   (match_operand:VDQW 1 "s_register_operand" "0")
   (match_operand:VDQW 2 "s_register_operand" "w")
   (match_operand:VDQW 3 "s_register_operand" "w")
   (match_operand:SI 4 "immediate_operand" "i")]
  "TARGET_NEON"
{
  if (!<Is_float_mode> || flag_unsafe_math_optimizations)
    emit_insn (gen_mul<mode>3neg<mode>add<mode>_neon (operands[0],
		 operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_neon_vmls<mode>_unspec (operands[0], operands[1],
					   operands[2], operands[3]));
  DONE;
})

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vmls<mode>_unspec"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(unspec:VDQ [(match_operand:VDQ 1 "s_register_operand" "0")
		     (match_operand:VDQ 2 "s_register_operand" "w")
		     (match_operand:VDQ 3 "s_register_operand" "w")]
		    UNSPEC_VMLS))]
  "TARGET_NEON"
  "vmls.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vmla_ddd")
                                  (const_string "neon_fp_vmla_qqq"))
                    (if_then_else (match_test "<Is_d_reg>")
                                  (if_then_else
                                    (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                                    (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))
                                  (if_then_else
                                    (match_test "<Scalar_mul_8_16>")
                                    (const_string "neon_mla_qqq_8_16")
                                    (const_string "neon_mla_qqq_32_qqd_32_scalar")))))]
)

(define_insn "neon_vmlsl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VW 2 "s_register_operand" "w")
		           (match_operand:VW 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VMLSL))]
  "TARGET_NEON"
  "vmlsl.%T4%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmulh<mode>"
  [(set (match_operand:VMDQI 0 "s_register_operand" "=w")
        (unspec:VMDQI [(match_operand:VMDQI 1 "s_register_operand" "w")
		       (match_operand:VMDQI 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQDMULH))]
  "TARGET_NEON"
  "vq%O3dmulh.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
        (if_then_else (match_test "<Scalar_mul_8_16>")
                      (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                      (const_string "neon_mul_qqq_8_16_32_ddd_32"))
        (if_then_else (match_test "<Scalar_mul_8_16>")
                      (const_string "neon_mul_qqq_8_16_32_ddd_32")
                      (const_string "neon_mul_qqq_8_16_32_ddd_32"))))]
)

(define_insn "neon_vqdmlal<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VMDI 2 "s_register_operand" "w")
		           (match_operand:VMDI 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMLAL))]
  "TARGET_NEON"
  "vqdmlal.<V_s_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmlsl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VMDI 2 "s_register_operand" "w")
		           (match_operand:VMDI 3 "s_register_operand" "w")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMLSL))]
  "TARGET_NEON"
  "vqdmlsl.<V_s_elem>\t%q0, %P2, %P3"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vmull<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
		           (match_operand:VW 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VMULL))]
  "TARGET_NEON"
  "vmull.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vqdmull<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VQDMULL))]
  "TARGET_NEON"
  "vqdmull.<V_s_elem>\t%q0, %P1, %P2"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_expand "neon_vsub<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "=w")
   (match_operand:VDQX 1 "s_register_operand" "w")
   (match_operand:VDQX 2 "s_register_operand" "w")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  if (!<Is_float_mode> || flag_unsafe_math_optimizations)
    emit_insn (gen_sub<mode>3 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vsub<mode>_unspec (operands[0], operands[1],
					   operands[2]));
  DONE;
})

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vsub<mode>_unspec"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
        (unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")
		      (match_operand:VDQX 2 "s_register_operand" "w")]
                     UNSPEC_VSUB))]
  "TARGET_NEON"
  "vsub.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_2")))]
)

(define_insn "neon_vsubl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VDI 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VSUBL))]
  "TARGET_NEON"
  "vsubl.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_2")]
)

(define_insn "neon_vsubw<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VSUBW))]
  "TARGET_NEON"
  "vsubw.%T3%#<V_sz_elem>\t%q0, %q1, %P2"
  [(set_attr "neon_type" "neon_int_2")]
)

(define_insn "neon_vqsub<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
        (unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VQSUB))]
  "TARGET_NEON"
  "vqsub.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vhsub<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VHSUB))]
  "TARGET_NEON"
  "vhsub.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vsubhn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
        (unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
		            (match_operand:VN 2 "s_register_operand" "w")
                            (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VSUBHN))]
  "TARGET_NEON"
  "v%O3subhn.<V_if_elem>\t%P0, %q1, %q2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vceq<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w,w")
        (unspec:<V_cmp_result>
	  [(match_operand:VDQW 1 "s_register_operand" "w,w")
	   (match_operand:VDQW 2 "nonmemory_operand" "w,Dz")
	   (match_operand:SI 3 "immediate_operand" "i,i")]
          UNSPEC_VCEQ))]
  "TARGET_NEON"
  "@
  vceq.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2
  vceq.<V_if_elem>\t%<V_reg>0, %<V_reg>1, #0"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vcge<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w,w")
        (unspec:<V_cmp_result>
	  [(match_operand:VDQW 1 "s_register_operand" "w,w")
	   (match_operand:VDQW 2 "nonmemory_operand" "w,Dz")
	   (match_operand:SI 3 "immediate_operand" "i,i")]
          UNSPEC_VCGE))]
  "TARGET_NEON"
  "@
  vcge.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2
  vcge.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, #0"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (if_then_else (match_test "<Is_d_reg>")
                                 (const_string "neon_fp_vadd_ddd_vabs_dd")
                                 (const_string "neon_fp_vadd_qqq_vabs_qq"))
                   (const_string "neon_int_5")))]
)

(define_insn "neon_vcgt<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w,w")
        (unspec:<V_cmp_result>
	  [(match_operand:VDQW 1 "s_register_operand" "w,w")
	   (match_operand:VDQW 2 "nonmemory_operand" "w,Dz")
           (match_operand:SI 3 "immediate_operand" "i,i")]
          UNSPEC_VCGT))]
  "TARGET_NEON"
  "@
  vcgt.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2
  vcgt.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, #0"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (if_then_else (match_test "<Is_d_reg>")
                                 (const_string "neon_fp_vadd_ddd_vabs_dd")
                                 (const_string "neon_fp_vadd_qqq_vabs_qq"))
                   (const_string "neon_int_5")))]
)

;; VCLE and VCLT only support comparisons with immediate zero (register
;; variants are VCGE and VCGT with operands reversed).

(define_insn "neon_vcle<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result>
	  [(match_operand:VDQW 1 "s_register_operand" "w")
	   (match_operand:VDQW 2 "nonmemory_operand" "Dz")
	   (match_operand:SI 3 "immediate_operand" "i")]
          UNSPEC_VCLE))]
  "TARGET_NEON"
  "vcle.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, #0"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vclt<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result>
	  [(match_operand:VDQW 1 "s_register_operand" "w")
	   (match_operand:VDQW 2 "nonmemory_operand" "Dz")
	   (match_operand:SI 3 "immediate_operand" "i")]
          UNSPEC_VCLT))]
  "TARGET_NEON"
  "vclt.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, #0"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_float_mode>")
                    (if_then_else (match_test "<Is_d_reg>")
                                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                                  (const_string "neon_fp_vadd_qqq_vabs_qq"))
                    (const_string "neon_int_5")))]
)

(define_insn "neon_vcage<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VCVTF 1 "s_register_operand" "w")
		                (match_operand:VCVTF 2 "s_register_operand" "w")
                                (match_operand:SI 3 "immediate_operand" "i")]
                               UNSPEC_VCAGE))]
  "TARGET_NEON"
  "vacge.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcagt<mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VCVTF 1 "s_register_operand" "w")
		                (match_operand:VCVTF 2 "s_register_operand" "w")
                                (match_operand:SI 3 "immediate_operand" "i")]
                               UNSPEC_VCAGT))]
  "TARGET_NEON"
  "vacgt.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vtst<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
		      UNSPEC_VTST))]
  "TARGET_NEON"
  "vtst.<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "neon_type" "neon_int_4")]
)

(define_insn "neon_vabd<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
		     UNSPEC_VABD))]
  "TARGET_NEON"
  "vabd.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (if_then_else (match_test "<Is_d_reg>")
                                 (const_string "neon_fp_vadd_ddd_vabs_dd")
                                 (const_string "neon_fp_vadd_qqq_vabs_qq"))
                   (const_string "neon_int_5")))]
)

(define_insn "neon_vabdl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
		           (match_operand:VW 2 "s_register_operand" "w")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VABDL))]
  "TARGET_NEON"
  "vabdl.%T3%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "neon_type" "neon_int_5")]
)

(define_insn "neon_vaba<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (plus:VDQIW (match_operand:VDQIW 1 "s_register_operand" "0")
                    (unspec:VDQIW [(match_operand:VDQIW 2 "s_register_operand" "w")
		                   (match_operand:VDQIW 3 "s_register_operand" "w")
                                   (match_operand:SI 4 "immediate_operand" "i")]
		                  UNSPEC_VABD)))]
  "TARGET_NEON"
  "vaba.%T4%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_vaba") (const_string "neon_vaba_qqq")))]
)

(define_insn "neon_vabal<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (plus:<V_widen> (match_operand:<V_widen> 1 "s_register_operand" "0")
                        (unspec:<V_widen> [(match_operand:VW 2 "s_register_operand" "w")
                                           (match_operand:VW 3 "s_register_operand" "w")
                                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VABDL)))]
  "TARGET_NEON"
  "vabal.%T4%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set_attr "neon_type" "neon_vaba")]
)

(define_insn "neon_vmax<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VMAX))]
  "TARGET_NEON"
  "vmax.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
    (if_then_else (match_test "<Is_float_mode>")
                  (if_then_else (match_test "<Is_d_reg>")
                                (const_string "neon_fp_vadd_ddd_vabs_dd")
                                (const_string "neon_fp_vadd_qqq_vabs_qq"))
                  (const_string "neon_int_5")))]
)

(define_insn "neon_vmin<mode>"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "w")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VMIN))]
  "TARGET_NEON"
  "vmin.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
    (if_then_else (match_test "<Is_float_mode>")
                  (if_then_else (match_test "<Is_d_reg>")
                                (const_string "neon_fp_vadd_ddd_vabs_dd")
                                (const_string "neon_fp_vadd_qqq_vabs_qq"))
                  (const_string "neon_int_5")))]
)

(define_expand "neon_vpadd<mode>"
  [(match_operand:VD 0 "s_register_operand" "=w")
   (match_operand:VD 1 "s_register_operand" "w")
   (match_operand:VD 2 "s_register_operand" "w")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vpadd_internal<mode> (operands[0], operands[1],
					    operands[2]));
  DONE;
})

(define_insn "neon_vpaddl<mode>"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
        (unspec:<V_double_width> [(match_operand:VDQIW 1 "s_register_operand" "w")
                                  (match_operand:SI 2 "immediate_operand" "i")]
                                 UNSPEC_VPADDL))]
  "TARGET_NEON"
  "vpaddl.%T2%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  ;; Assume this schedules like vaddl.
  [(set_attr "neon_type" "neon_int_3")]
)

(define_insn "neon_vpadal<mode>"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
        (unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
                                  (match_operand:VDQIW 2 "s_register_operand" "w")
                                  (match_operand:SI 3 "immediate_operand" "i")]
                                 UNSPEC_VPADAL))]
  "TARGET_NEON"
  "vpadal.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2"
  ;; Assume this schedules like vpadd.
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "neon_vpmax<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
        (unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")
                    (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VPMAX))]
  "TARGET_NEON"
  "vpmax.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  ;; Assume this schedules like vmax.
  [(set (attr "neon_type")
    (if_then_else (match_test "<Is_float_mode>")
                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                  (const_string "neon_int_5")))]
)

(define_insn "neon_vpmin<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
        (unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")
                    (match_operand:SI 3 "immediate_operand" "i")]
                   UNSPEC_VPMIN))]
  "TARGET_NEON"
  "vpmin.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  ;; Assume this schedules like vmin.
  [(set (attr "neon_type")
    (if_then_else (match_test "<Is_float_mode>")
                  (const_string "neon_fp_vadd_ddd_vabs_dd")
                  (const_string "neon_int_5")))]
)

(define_insn "neon_vrecps<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VRECPS))]
  "TARGET_NEON"
  "vrecps.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_fp_vrecps_vrsqrts_ddd")
                    (const_string "neon_fp_vrecps_vrsqrts_qqq")))]
)

(define_insn "neon_vrsqrts<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VRSQRTS))]
  "TARGET_NEON"
  "vrsqrts.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_fp_vrecps_vrsqrts_ddd")
                    (const_string "neon_fp_vrecps_vrsqrts_qqq")))]
)

(define_expand "neon_vabs<mode>"
  [(match_operand:VDQW 0 "s_register_operand" "")
   (match_operand:VDQW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_abs<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vqabs<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
		      UNSPEC_VQABS))]
  "TARGET_NEON"
  "vqabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_vqneg_vqabs")]
)

(define_expand "neon_vneg<mode>"
  [(match_operand:VDQW 0 "s_register_operand" "")
   (match_operand:VDQW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_neg<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vqneg<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
		      UNSPEC_VQNEG))]
  "TARGET_NEON"
  "vqneg.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_vqneg_vqabs")]
)

(define_insn "neon_vcls<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
		      UNSPEC_VCLS))]
  "TARGET_NEON"
  "vcls.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_insn "clz<mode>2"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (clz:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vclz.<V_if_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_expand "neon_vclz<mode>"
  [(match_operand:VDQIW 0 "s_register_operand" "")
   (match_operand:VDQIW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_clz<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "popcount<mode>2"
  [(set (match_operand:VE 0 "s_register_operand" "=w")
        (popcount:VE (match_operand:VE 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vcnt.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_expand "neon_vcnt<mode>"
  [(match_operand:VE 0 "s_register_operand" "=w")
   (match_operand:VE 1 "s_register_operand" "w")
   (match_operand:SI 2 "immediate_operand" "i")]
  "TARGET_NEON"
{
  emit_insn (gen_popcount<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vrecpe<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w")
	(unspec:V32 [(match_operand:V32 1 "s_register_operand" "w")
                     (match_operand:SI 2 "immediate_operand" "i")]
                    UNSPEC_VRECPE))]
  "TARGET_NEON"
  "vrecpe.<V_u_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vrsqrte<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w")
	(unspec:V32 [(match_operand:V32 1 "s_register_operand" "w")
                     (match_operand:SI 2 "immediate_operand" "i")]
                    UNSPEC_VRSQRTE))]
  "TARGET_NEON"
  "vrsqrte.<V_u_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_fp_vadd_ddd_vabs_dd")
                    (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_expand "neon_vmvn<mode>"
  [(match_operand:VDQIW 0 "s_register_operand" "")
   (match_operand:VDQIW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_one_cmpl<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vget_lane<mode>_sext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VD 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov%?.s<V_sz_elem>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_lane<mode>_zext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VD 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  return "vmov%?.u<V_sz_elem>\t%0, %P1[%c2]";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_lane<mode>_sext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VQ 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  rtx ops[3];
  int regno = REGNO (operands[1]);
  unsigned int halfelts = GET_MODE_NUNITS (<MODE>mode) / 2;
  unsigned int elt = INTVAL (operands[2]);
  unsigned int elt_adj = elt % halfelts;

  if (BYTES_BIG_ENDIAN)
    elt_adj = halfelts - 1 - elt_adj;

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (<V_HALF>mode, regno + 2 * (elt / halfelts));
  ops[2] = GEN_INT (elt_adj);
  output_asm_insn ("vmov%?.s<V_sz_elem>\t%0, %P1[%c2]", ops);

  return "";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vget_lane<mode>_zext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VQ 1 "s_register_operand" "w")
	    (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  rtx ops[3];
  int regno = REGNO (operands[1]);
  unsigned int halfelts = GET_MODE_NUNITS (<MODE>mode) / 2;
  unsigned int elt = INTVAL (operands[2]);
  unsigned int elt_adj = elt % halfelts;

  if (BYTES_BIG_ENDIAN)
    elt_adj = halfelts - 1 - elt_adj;

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (<V_HALF>mode, regno + 2 * (elt / halfelts));
  ops[2] = GEN_INT (elt_adj);
  output_asm_insn ("vmov%?.u<V_sz_elem>\t%0, %P1[%c2]", ops);

  return "";
}
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_expand "neon_vget_lane<mode>"
  [(match_operand:<V_ext> 0 "s_register_operand" "")
   (match_operand:VDQW 1 "s_register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  HOST_WIDE_INT magic = INTVAL (operands[3]);
  rtx insn;

  neon_lane_bounds (operands[2], 0, GET_MODE_NUNITS (<MODE>mode));

  if (BYTES_BIG_ENDIAN)
    {
      /* The intrinsics are defined in terms of a model where the
	 element ordering in memory is vldm order, whereas the generic
	 RTL is defined in terms of a model where the element ordering
	 in memory is array order.  Convert the lane number to conform
	 to this model.  */
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts
	= 64 / GET_MODE_BITSIZE (GET_MODE_INNER (<MODE>mode));
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }

  if ((magic & 3) == 3 || GET_MODE_BITSIZE (GET_MODE_INNER (<MODE>mode)) == 32)
    insn = gen_vec_extract<mode> (operands[0], operands[1], operands[2]);
  else
    {
      if ((magic & 1) != 0)
	insn = gen_neon_vget_lane<mode>_sext_internal (operands[0], operands[1],
						       operands[2]);
      else
	insn = gen_neon_vget_lane<mode>_zext_internal (operands[0], operands[1],
						       operands[2]);
    }
  emit_insn (insn);
  DONE;
})

; Operand 3 (info word) is ignored because it does nothing useful with 64-bit
; elements.

(define_expand "neon_vget_lanedi"
  [(match_operand:DI 0 "s_register_operand" "=r")
   (match_operand:DI 1 "s_register_operand" "w")
   (match_operand:SI 2 "immediate_operand" "i")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, 1);
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vget_lanev2di"
  [(match_operand:DI 0 "s_register_operand" "=r")
   (match_operand:V2DI 1 "s_register_operand" "w")
   (match_operand:SI 2 "immediate_operand" "i")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, 2);
  emit_insn (gen_vec_extractv2di (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vset_lane<mode>"
  [(match_operand:VDQ 0 "s_register_operand" "=w")
   (match_operand:<V_elem> 1 "s_register_operand" "r")
   (match_operand:VDQ 2 "s_register_operand" "0")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  unsigned int elt = INTVAL (operands[3]);
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));

  if (BYTES_BIG_ENDIAN)
    {
      unsigned int reg_nelts
	= 64 / GET_MODE_BITSIZE (GET_MODE_INNER (<MODE>mode));
      elt ^= reg_nelts - 1;
    }

  emit_insn (gen_vec_set<mode>_internal (operands[0], operands[1],
                                         GEN_INT (1 << elt), operands[2]));
  DONE;
})

; See neon_vget_lanedi comment for reasons operands 2 & 3 are ignored.

(define_expand "neon_vset_lanedi"
  [(match_operand:DI 0 "s_register_operand" "=w")
   (match_operand:DI 1 "s_register_operand" "r")
   (match_operand:DI 2 "s_register_operand" "0")
   (match_operand:SI 3 "immediate_operand" "i")]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, 1);
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vcreate<mode>"
  [(match_operand:VDX 0 "s_register_operand" "")
   (match_operand:DI 1 "general_operand" "")]
  "TARGET_NEON"
{
  rtx src = gen_lowpart (<MODE>mode, operands[1]);
  emit_move_insn (operands[0], src);
  DONE;
})

(define_insn "neon_vdup_n<mode>"
  [(set (match_operand:VX 0 "s_register_operand" "=w")
        (vec_duplicate:VX (match_operand:<V_elem> 1 "s_register_operand" "r")))]
  "TARGET_NEON"
  "vdup%?.<V_sz_elem>\t%<V_reg>0, %1"
  ;; Assume this schedules like vmov.
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vdup_n<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w,w")
        (vec_duplicate:V32 (match_operand:<V_elem> 1 "s_register_operand" "r,t")))]
  "TARGET_NEON"
  "@
  vdup%?.<V_sz_elem>\t%<V_reg>0, %1
  vdup%?.<V_sz_elem>\t%<V_reg>0, %y1"
  ;; Assume this schedules like vmov.
  [(set_attr "predicable" "yes")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_expand "neon_vdup_ndi"
  [(match_operand:DI 0 "s_register_operand" "=w")
   (match_operand:DI 1 "s_register_operand" "r")]
  "TARGET_NEON"
{
  emit_move_insn (operands[0], operands[1]);
  DONE;
}
)

(define_insn "neon_vdup_nv2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w,w")
        (vec_duplicate:V2DI (match_operand:DI 1 "s_register_operand" "r,w")))]
  "TARGET_NEON"
  "@
  vmov%?\t%e0, %Q1, %R1\;vmov%?\t%f0, %Q1, %R1
  vmov%?\t%e0, %P1\;vmov%?\t%f0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "8")
   (set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vdup_lane<mode>_internal"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
  	(vec_duplicate:VDQW 
          (vec_select:<V_elem>
            (match_operand:<V_double_vector_mode> 1 "s_register_operand" "w")
            (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<V_double_vector_mode>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }
  if (<Is_d_reg>)
    return "vdup.<V_sz_elem>\t%P0, %P1[%c2]";
  else
    return "vdup.<V_sz_elem>\t%q0, %P1[%c2]";
}
  ;; Assume this schedules like vmov.
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_expand "neon_vdup_lane<mode>"
  [(match_operand:VDQW 0 "s_register_operand" "=w")
   (match_operand:<V_double_vector_mode> 1 "s_register_operand" "w")
   (match_operand:SI 2 "immediate_operand" "i")]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, GET_MODE_NUNITS (<V_double_vector_mode>mode));
  if (BYTES_BIG_ENDIAN)
    {
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts
	= 64 / GET_MODE_BITSIZE (GET_MODE_INNER (<V_double_vector_mode>mode));
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }
    emit_insn (gen_neon_vdup_lane<mode>_internal (operands[0], operands[1],
                                                  operands[2]));
    DONE;
})

; Scalar index is ignored, since only zero is valid here.
(define_expand "neon_vdup_lanedi"
  [(match_operand:DI 0 "s_register_operand" "=w")
   (match_operand:DI 1 "s_register_operand" "w")
   (match_operand:SI 2 "immediate_operand" "i")]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, 1);
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

; Likewise for v2di, as the DImode second operand has only a single element.
(define_expand "neon_vdup_lanev2di"
  [(match_operand:V2DI 0 "s_register_operand" "=w")
   (match_operand:DI 1 "s_register_operand" "w")
   (match_operand:SI 2 "immediate_operand" "i")]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[2], 0, 1);
  emit_insn (gen_neon_vdup_nv2di (operands[0], operands[1]));
  DONE;
})

; Disabled before reload because we don't want combine doing something silly,
; but used by the post-reload expansion of neon_vcombine.
(define_insn "*neon_vswp<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "+w")
	(match_operand:VDQX 1 "s_register_operand" "+w"))
   (set (match_dup 1) (match_dup 0))]
  "TARGET_NEON && reload_completed"
  "vswp\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
	(if_then_else (match_test "<Is_d_reg>")
		      (const_string "neon_bp_simple")
		      (const_string "neon_bp_2cycle")))]
)

;; In this insn, operand 1 should be low, and operand 2 the high part of the
;; dest vector.
;; FIXME: A different implementation of this builtin could make it much
;; more likely that we wouldn't actually need to output anything (we could make
;; it so that the reg allocator puts things in the right places magically
;; instead). Lack of subregs for vectors makes that tricky though, I think.

(define_insn_and_split "neon_vcombine<mode>"
  [(set (match_operand:<V_DOUBLE> 0 "s_register_operand" "=w")
        (vec_concat:<V_DOUBLE>
	  (match_operand:VDX 1 "s_register_operand" "w")
	  (match_operand:VDX 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  neon_split_vcombine (operands);
  DONE;
})

(define_expand "neon_vget_high<mode>"
  [(match_operand:<V_HALF> 0 "s_register_operand")
   (match_operand:VQX 1 "s_register_operand")]
  "TARGET_NEON"
{
  emit_move_insn (operands[0],
		  simplify_gen_subreg (<V_HALF>mode, operands[1], <MODE>mode,
				       GET_MODE_SIZE (<V_HALF>mode)));
  DONE;
})

(define_expand "neon_vget_low<mode>"
  [(match_operand:<V_HALF> 0 "s_register_operand")
   (match_operand:VQX 1 "s_register_operand")]
  "TARGET_NEON"
{
  emit_move_insn (operands[0],
		  simplify_gen_subreg (<V_HALF>mode, operands[1],
				       <MODE>mode, 0));
  DONE;
})

(define_insn "float<mode><V_cvtto>2"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
        (float:<V_CVTTO> (match_operand:VCVTI 1 "s_register_operand" "w")))]
  "TARGET_NEON && !flag_rounding_math"
  "vcvt.f32.s32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "floatuns<mode><V_cvtto>2"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
        (unsigned_float:<V_CVTTO> (match_operand:VCVTI 1 "s_register_operand" "w")))] 
  "TARGET_NEON && !flag_rounding_math"
  "vcvt.f32.u32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "fix_trunc<mode><V_cvtto>2"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
        (fix:<V_CVTTO> (match_operand:VCVTF 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vcvt.s32.f32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "fixuns_trunc<mode><V_cvtto>2"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
        (unsigned_fix:<V_CVTTO> (match_operand:VCVTF 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vcvt.u32.f32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcvt<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTF 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
			  UNSPEC_VCVT))]
  "TARGET_NEON"
  "vcvt.%T2%#32.f32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcvt<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTI 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
			  UNSPEC_VCVT))]
  "TARGET_NEON"
  "vcvt.f32.%T2%#32\t%<V_reg>0, %<V_reg>1"
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcvt_n<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTF 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")
                           (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VCVT_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, 33);
  return "vcvt.%T3%#32.f32\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vcvt_n<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTI 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")
                           (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VCVT_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, 33);
  return "vcvt.f32.%T3%#32\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_d_reg>")
                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                   (const_string "neon_fp_vadd_qqq_vabs_qq")))]
)

(define_insn "neon_vmovn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           UNSPEC_VMOVN))]
  "TARGET_NEON"
  "vmovn.<V_if_elem>\t%P0, %q1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vqmovn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           UNSPEC_VQMOVN))]
  "TARGET_NEON"
  "vqmovn.%T2%#<V_sz_elem>\t%P0, %q1"
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vqmovun<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           UNSPEC_VQMOVUN))]
  "TARGET_NEON"
  "vqmovun.<V_s_elem>\t%P0, %q1"
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vmovl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
                          UNSPEC_VMOVL))]
  "TARGET_NEON"
  "vmovl.%T2%#<V_sz_elem>\t%q0, %P1"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vmul_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "w")
		     (match_operand:VMD 2 "s_register_operand"
                                        "<scalar_mul_constraint>")
                     (match_operand:SI 3 "immediate_operand" "i")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMUL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmul.<V_if_elem>\t%P0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_vmul_ddd")
                   (if_then_else (match_test "<Scalar_mul_8_16>")
                                 (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                                 (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar"))))]
)

(define_insn "neon_vmul_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "w")
		     (match_operand:<V_HALF> 2 "s_register_operand"
                                             "<scalar_mul_constraint>")
                     (match_operand:SI 3 "immediate_operand" "i")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMUL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<V_HALF>mode));
  return "vmul.<V_if_elem>\t%q0, %q1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_vmul_qqd")
                   (if_then_else (match_test "<Scalar_mul_8_16>")
                                 (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")
                                 (const_string "neon_mul_qqd_32_scalar"))))]
)

(define_insn "neon_vmull_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 3 "immediate_operand" "i")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VMULL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmull.%T4%#<V_sz_elem>\t%q0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vqdmull_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 3 "immediate_operand" "i")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMULL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vqdmull.<V_s_elem>\t%q0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vqdmulh_lane<mode>"
  [(set (match_operand:VMQI 0 "s_register_operand" "=w")
	(unspec:VMQI [(match_operand:VMQI 1 "s_register_operand" "w")
		      (match_operand:<V_HALF> 2 "s_register_operand"
					      "<scalar_mul_constraint>")
                      (match_operand:SI 3 "immediate_operand" "i")
                      (match_operand:SI 4 "immediate_operand" "i")]
                      UNSPEC_VQDMULH_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vq%O4dmulh.%T4%#<V_sz_elem>\t%q0, %q1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")
                   (const_string "neon_mul_qqd_32_scalar")))]
)

(define_insn "neon_vqdmulh_lane<mode>"
  [(set (match_operand:VMDI 0 "s_register_operand" "=w")
	(unspec:VMDI [(match_operand:VMDI 1 "s_register_operand" "w")
		      (match_operand:VMDI 2 "s_register_operand"
					  "<scalar_mul_constraint>")
                      (match_operand:SI 3 "immediate_operand" "i")
                      (match_operand:SI 4 "immediate_operand" "i")]
                      UNSPEC_VQDMULH_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vq%O4dmulh.%T4%#<V_sz_elem>\t%P0, %P1, %P2[%c3]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mul_ddd_16_scalar_32_16_long_scalar")
                   (const_string "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar")))]
)

(define_insn "neon_vmla_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "0")
		     (match_operand:VMD 2 "s_register_operand" "w")
                     (match_operand:VMD 3 "s_register_operand"
					"<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                     UNSPEC_VMLA_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmla.<V_if_elem>\t%P0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_vmla_ddd_scalar")
                   (if_then_else (match_test "<Scalar_mul_8_16>")
                                 (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))))]
)

(define_insn "neon_vmla_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "0")
		     (match_operand:VMQ 2 "s_register_operand" "w")
                     (match_operand:<V_HALF> 3 "s_register_operand"
					     "<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                     UNSPEC_VMLA_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmla.<V_if_elem>\t%q0, %q2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_vmla_qqq_scalar")
                   (if_then_else (match_test "<Scalar_mul_8_16>")
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")
                                 (const_string "neon_mla_qqq_32_qqd_32_scalar"))))]
)

(define_insn "neon_vmlal_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VMLAL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmlal.%T5%#<V_sz_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmlal_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VQDMLAL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vqdmlal.<V_s_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vmls_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "0")
		     (match_operand:VMD 2 "s_register_operand" "w")
                     (match_operand:VMD 3 "s_register_operand"
					"<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                    UNSPEC_VMLS_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmls.<V_if_elem>\t%P0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_vmla_ddd_scalar")
                   (if_then_else (match_test "<Scalar_mul_8_16>")
                                 (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long"))))]
)

(define_insn "neon_vmls_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "0")
		     (match_operand:VMQ 2 "s_register_operand" "w")
                     (match_operand:<V_HALF> 3 "s_register_operand"
					     "<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")
                     (match_operand:SI 5 "immediate_operand" "i")]
                    UNSPEC_VMLS_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmls.<V_if_elem>\t%q0, %q2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_vmla_qqq_scalar")
                   (if_then_else (match_test "<Scalar_mul_8_16>")
                                 (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")
                                 (const_string "neon_mla_qqq_32_qqd_32_scalar"))))]
)

(define_insn "neon_vmlsl_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VMLSL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vmlsl.%T5%#<V_sz_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

(define_insn "neon_vqdmlsl_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")
                           (match_operand:SI 5 "immediate_operand" "i")]
                          UNSPEC_VQDMLSL_LANE))]
  "TARGET_NEON"
{
  neon_lane_bounds (operands[4], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vqdmlsl.<V_s_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set (attr "neon_type")
     (if_then_else (match_test "<Scalar_mul_8_16>")
                   (const_string "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar")
                   (const_string "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long")))]
)

; FIXME: For the "_n" multiply/multiply-accumulate insns, we copy a value in a
; core register into a temp register, then use a scalar taken from that. This
; isn't an optimal solution if e.g. the scalar has just been read from memory
; or extracted from another vector. The latter case it's currently better to
; use the "_lane" variant, and the former case can probably be implemented
; using vld1_lane, but that hasn't been done yet.

(define_expand "neon_vmul_n<mode>"
  [(match_operand:VMD 0 "s_register_operand" "")
   (match_operand:VMD 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmul_lane<mode> (operands[0], operands[1], tmp,
				       const0_rtx, const0_rtx));
  DONE;
})

(define_expand "neon_vmul_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand" "")
   (match_operand:VMQ 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmul_lane<mode> (operands[0], operands[1], tmp,
				       const0_rtx, const0_rtx));
  DONE;
})

(define_expand "neon_vmull_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:VMDI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmull_lane<mode> (operands[0], operands[1], tmp,
				        const0_rtx, operands[3]));
  DONE;
})

(define_expand "neon_vqdmull_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:VMDI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmull_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx, const0_rtx));
  DONE;
})

(define_expand "neon_vqdmulh_n<mode>"
  [(match_operand:VMDI 0 "s_register_operand" "")
   (match_operand:VMDI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmulh_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx, operands[3]));
  DONE;
})

(define_expand "neon_vqdmulh_n<mode>"
  [(match_operand:VMQI 0 "s_register_operand" "")
   (match_operand:VMQI 1 "s_register_operand" "")
   (match_operand:<V_elem> 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmulh_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx, operands[3]));
  DONE;
})

(define_expand "neon_vmla_n<mode>"
  [(match_operand:VMD 0 "s_register_operand" "")
   (match_operand:VMD 1 "s_register_operand" "")
   (match_operand:VMD 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmla_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmla_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand" "")
   (match_operand:VMQ 1 "s_register_operand" "")
   (match_operand:VMQ 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmla_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmlal_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlal_lane<mode> (operands[0], operands[1], operands[2],
					tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vqdmlal_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmlal_lane<mode> (operands[0], operands[1], operands[2],
					  tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmls_n<mode>"
  [(match_operand:VMD 0 "s_register_operand" "")
   (match_operand:VMD 1 "s_register_operand" "")
   (match_operand:VMD 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmls_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmls_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand" "")
   (match_operand:VMQ 1 "s_register_operand" "")
   (match_operand:VMQ 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmls_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vmlsl_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlsl_lane<mode> (operands[0], operands[1], operands[2],
					tmp, const0_rtx, operands[4]));
  DONE;
})

(define_expand "neon_vqdmlsl_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand" "")
   (match_operand:<V_widen> 1 "s_register_operand" "")
   (match_operand:VMDI 2 "s_register_operand" "")
   (match_operand:<V_elem> 3 "s_register_operand" "")
   (match_operand:SI 4 "immediate_operand" "")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmlsl_lane<mode> (operands[0], operands[1], operands[2],
					  tmp, const0_rtx, operands[4]));
  DONE;
})

(define_insn "neon_vext<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")
		      (match_operand:VDQX 2 "s_register_operand" "w")
                      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VEXT))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vext.<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2, %3";
}
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_2cycle")))]
)

(define_insn "neon_vrev64<mode>"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(unspec:VDQ [(match_operand:VDQ 1 "s_register_operand" "w")
		     (match_operand:SI 2 "immediate_operand" "i")]
                    UNSPEC_VREV64))]
  "TARGET_NEON"
  "vrev64.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vrev32<mode>"
  [(set (match_operand:VX 0 "s_register_operand" "=w")
	(unspec:VX [(match_operand:VX 1 "s_register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VREV32))]
  "TARGET_NEON"
  "vrev32.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

(define_insn "neon_vrev16<mode>"
  [(set (match_operand:VE 0 "s_register_operand" "=w")
	(unspec:VE [(match_operand:VE 1 "s_register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
                   UNSPEC_VREV16))]
  "TARGET_NEON"
  "vrev16.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "neon_type" "neon_bp_simple")]
)

; vbsl_* intrinsics may compile to any of vbsl/vbif/vbit depending on register
; allocation. For an intrinsic of form:
;   rD = vbsl_* (rS, rN, rM)
; We can use any of:
;   vbsl rS, rN, rM  (if D = S)
;   vbit rD, rN, rS  (if D = M, so 1-bits in rS choose bits from rN, else rM)
;   vbif rD, rM, rS  (if D = N, so 0-bits in rS choose bits from rM, else rN)

(define_insn "neon_vbsl<mode>_internal"
  [(set (match_operand:VDQX 0 "s_register_operand"		 "=w,w,w")
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" " 0,w,w")
		      (match_operand:VDQX 2 "s_register_operand" " w,w,0")
                      (match_operand:VDQX 3 "s_register_operand" " w,0,w")]
                     UNSPEC_VBSL))]
  "TARGET_NEON"
  "@
  vbsl\t%<V_reg>0, %<V_reg>2, %<V_reg>3
  vbit\t%<V_reg>0, %<V_reg>2, %<V_reg>1
  vbif\t%<V_reg>0, %<V_reg>3, %<V_reg>1"
  [(set_attr "neon_type" "neon_int_1")]
)

(define_expand "neon_vbsl<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "")
        (unspec:VDQX [(match_operand:<V_cmp_result> 1 "s_register_operand" "")
                      (match_operand:VDQX 2 "s_register_operand" "")
                      (match_operand:VDQX 3 "s_register_operand" "")]
                     UNSPEC_VBSL))]
  "TARGET_NEON"
{
  /* We can't alias operands together if they have different modes.  */
  operands[1] = gen_lowpart (<MODE>mode, operands[1]);
})

(define_insn "neon_vshl<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSHL))]
  "TARGET_NEON"
  "v%O3shl.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_vshl_ddd")
                    (const_string "neon_shift_3")))]
)

(define_insn "neon_vqshl<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQSHL))]
  "TARGET_NEON"
  "vq%O3shl.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_shift_2")
                    (const_string "neon_vqshl_vrshl_vqrshl_qqq")))]
)

(define_insn "neon_vshr_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSHR_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) + 1);
  return "v%O3shr.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vshrn_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")
			    (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VSHRN_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "v%O3shrn.<V_if_elem>\t%P0, %q1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vqshrn_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")
			    (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VQSHRN_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "vq%O3shrn.%T3%#<V_sz_elem>\t%P0, %q1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vqshrun_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")
			    (match_operand:SI 3 "immediate_operand" "i")]
                           UNSPEC_VQSHRUN_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "vq%O3shrun.%T3%#<V_sz_elem>\t%P0, %q1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vshl_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSHL_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vshl.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vqshl_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQSHL_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vqshl.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vqshlu_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VQSHLU_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vqshlu.%T3%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "neon_type" "neon_shift_2")]
)

(define_insn "neon_vshll_n<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")
			   (match_operand:SI 3 "immediate_operand" "i")]
			  UNSPEC_VSHLL_N))]
  "TARGET_NEON"
{
  /* The boundaries are: 0 < imm <= size.  */
  neon_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode) + 1);
  return "vshll.%T3%#<V_sz_elem>\t%q0, %P1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vsra_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")
                       (match_operand:SI 4 "immediate_operand" "i")]
                      UNSPEC_VSRA_N))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 1, neon_element_bits (<MODE>mode) + 1);
  return "v%O4sra.%T4%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set_attr "neon_type" "neon_vsra_vrsra")]
)

(define_insn "neon_vsri_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
        	       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSRI))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 1, neon_element_bits (<MODE>mode) + 1);
  return "vsri.<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_shift_1")
                    (const_string "neon_shift_3")))]
)

(define_insn "neon_vsli_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
        	       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSLI))]
  "TARGET_NEON"
{
  neon_const_bounds (operands[3], 0, neon_element_bits (<MODE>mode));
  return "vsli.<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_shift_1")
                    (const_string "neon_shift_3")))]
)

(define_insn "neon_vtbl1v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
  "vtbl.8\t%P0, {%P1}, %P2"
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbl2v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:TI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
{
  rtx ops[4];
  int tabbase = REGNO (operands[1]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = operands[2];
  output_asm_insn ("vtbl.8\t%P0, {%P1, %P2}, %P3", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbl3v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:EI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
{
  rtx ops[5];
  int tabbase = REGNO (operands[1]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = operands[2];
  output_asm_insn ("vtbl.8\t%P0, {%P1, %P2, %P3}, %P4", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

(define_insn "neon_vtbl4v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:OI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
{
  rtx ops[6];
  int tabbase = REGNO (operands[1]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = gen_rtx_REG (V8QImode, tabbase + 6);
  ops[5] = operands[2];
  output_asm_insn ("vtbl.8\t%P0, {%P1, %P2, %P3, %P4}, %P5", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

;; These three are used by the vec_perm infrastructure for V16QImode.
(define_insn_and_split "neon_vtbl1v16qi"
  [(set (match_operand:V16QI 0 "s_register_operand" "=&w")
	(unspec:V16QI [(match_operand:V16QI 1 "s_register_operand" "w")
		       (match_operand:V16QI 2 "s_register_operand" "w")]
		      UNSPEC_VTBL))]
  "TARGET_NEON"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op0, op1, op2, part0, part2;
  unsigned ofs;

  op0 = operands[0];
  op1 = gen_lowpart (TImode, operands[1]);
  op2 = operands[2];

  ofs = subreg_lowpart_offset (V8QImode, V16QImode);
  part0 = simplify_subreg (V8QImode, op0, V16QImode, ofs);
  part2 = simplify_subreg (V8QImode, op2, V16QImode, ofs);
  emit_insn (gen_neon_vtbl2v8qi (part0, op1, part2));

  ofs = subreg_highpart_offset (V8QImode, V16QImode);
  part0 = simplify_subreg (V8QImode, op0, V16QImode, ofs);
  part2 = simplify_subreg (V8QImode, op2, V16QImode, ofs);
  emit_insn (gen_neon_vtbl2v8qi (part0, op1, part2));
  DONE;
})

(define_insn_and_split "neon_vtbl2v16qi"
  [(set (match_operand:V16QI 0 "s_register_operand" "=&w")
	(unspec:V16QI [(match_operand:OI 1 "s_register_operand" "w")
		       (match_operand:V16QI 2 "s_register_operand" "w")]
		      UNSPEC_VTBL))]
  "TARGET_NEON"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op0, op1, op2, part0, part2;
  unsigned ofs;

  op0 = operands[0];
  op1 = operands[1];
  op2 = operands[2];

  ofs = subreg_lowpart_offset (V8QImode, V16QImode);
  part0 = simplify_subreg (V8QImode, op0, V16QImode, ofs);
  part2 = simplify_subreg (V8QImode, op2, V16QImode, ofs);
  emit_insn (gen_neon_vtbl2v8qi (part0, op1, part2));

  ofs = subreg_highpart_offset (V8QImode, V16QImode);
  part0 = simplify_subreg (V8QImode, op0, V16QImode, ofs);
  part2 = simplify_subreg (V8QImode, op2, V16QImode, ofs);
  emit_insn (gen_neon_vtbl2v8qi (part0, op1, part2));
  DONE;
})

;; ??? Logically we should extend the regular neon_vcombine pattern to
;; handle quad-word input modes, producing octa-word output modes.  But
;; that requires us to add support for octa-word vector modes in moves.
;; That seems overkill for this one use in vec_perm.
(define_insn_and_split "neon_vcombinev16qi"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
	(unspec:OI [(match_operand:V16QI 1 "s_register_operand" "w")
		    (match_operand:V16QI 2 "s_register_operand" "w")]
		   UNSPEC_VCONCAT))]
  "TARGET_NEON"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  neon_split_vcombine (operands);
  DONE;
})

(define_insn "neon_vtbx1v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:V8QI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
  "vtbx.8\t%P0, {%P2}, %P3"
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbx2v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:TI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
{
  rtx ops[4];
  int tabbase = REGNO (operands[2]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = operands[3];
  output_asm_insn ("vtbx.8\t%P0, {%P1, %P2}, %P3", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_2cycle")]
)

(define_insn "neon_vtbx3v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:EI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
{
  rtx ops[5];
  int tabbase = REGNO (operands[2]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = operands[3];
  output_asm_insn ("vtbx.8\t%P0, {%P1, %P2, %P3}, %P4", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

(define_insn "neon_vtbx4v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:OI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
{
  rtx ops[6];
  int tabbase = REGNO (operands[2]);

  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (V8QImode, tabbase);
  ops[2] = gen_rtx_REG (V8QImode, tabbase + 2);
  ops[3] = gen_rtx_REG (V8QImode, tabbase + 4);
  ops[4] = gen_rtx_REG (V8QImode, tabbase + 6);
  ops[5] = operands[3];
  output_asm_insn ("vtbx.8\t%P0, {%P1, %P2, %P3, %P4}, %P5", ops);

  return "";
}
  [(set_attr "neon_type" "neon_bp_3cycle")]
)

(define_insn "neon_vtrn<mode>_internal"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
                      (match_operand:VDQW 2 "s_register_operand" "w")]
                     UNSPEC_VTRN1))
   (set (match_operand:VDQW 3 "s_register_operand" "=2")
         (unspec:VDQW [(match_dup 1) (match_dup 2)]
                     UNSPEC_VTRN2))]
  "TARGET_NEON"
  "vtrn.<V_sz_elem>\t%<V_reg>0, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_3cycle")))]
)

(define_expand "neon_vtrn<mode>"
  [(match_operand:SI 0 "s_register_operand" "r")
   (match_operand:VDQW 1 "s_register_operand" "w")
   (match_operand:VDQW 2 "s_register_operand" "w")]
  "TARGET_NEON"
{
  neon_emit_pair_result_insn (<MODE>mode, gen_neon_vtrn<mode>_internal,
			      operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "neon_vzip<mode>_internal"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
                      (match_operand:VDQW 2 "s_register_operand" "w")]
                     UNSPEC_VZIP1))
   (set (match_operand:VDQW 3 "s_register_operand" "=2")
        (unspec:VDQW [(match_dup 1) (match_dup 2)]
                     UNSPEC_VZIP2))]
  "TARGET_NEON"
  "vzip.<V_sz_elem>\t%<V_reg>0, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_3cycle")))]
)

(define_expand "neon_vzip<mode>"
  [(match_operand:SI 0 "s_register_operand" "r")
   (match_operand:VDQW 1 "s_register_operand" "w")
   (match_operand:VDQW 2 "s_register_operand" "w")]
  "TARGET_NEON"
{
  neon_emit_pair_result_insn (<MODE>mode, gen_neon_vzip<mode>_internal,
			      operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "neon_vuzp<mode>_internal"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
                      (match_operand:VDQW 2 "s_register_operand" "w")]
                     UNSPEC_VUZP1))
   (set (match_operand:VDQW 3 "s_register_operand" "=2")
        (unspec:VDQW [(match_dup 1) (match_dup 2)]
                     UNSPEC_VUZP2))]
  "TARGET_NEON"
  "vuzp.<V_sz_elem>\t%<V_reg>0, %<V_reg>3"
  [(set (attr "neon_type")
      (if_then_else (match_test "<Is_d_reg>")
                    (const_string "neon_bp_simple")
                    (const_string "neon_bp_3cycle")))]
)

(define_expand "neon_vuzp<mode>"
  [(match_operand:SI 0 "s_register_operand" "r")
   (match_operand:VDQW 1 "s_register_operand" "w")
   (match_operand:VDQW 2 "s_register_operand" "w")]
  "TARGET_NEON"
{
  neon_emit_pair_result_insn (<MODE>mode, gen_neon_vuzp<mode>_internal,
			      operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "neon_vreinterpretv8qi<mode>"
  [(match_operand:V8QI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv4hi<mode>"
  [(match_operand:V4HI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv2si<mode>"
  [(match_operand:V2SI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv2sf<mode>"
  [(match_operand:V2SF 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretdi<mode>"
  [(match_operand:DI 0 "s_register_operand" "")
   (match_operand:VDX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv16qi<mode>"
  [(match_operand:V16QI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv8hi<mode>"
  [(match_operand:V8HI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv4si<mode>"
  [(match_operand:V4SI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv4sf<mode>"
  [(match_operand:V4SF 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vreinterpretv2di<mode>"
  [(match_operand:V2DI 0 "s_register_operand" "")
   (match_operand:VQX 1 "s_register_operand" "")]
  "TARGET_NEON"
{
  neon_reinterpret (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_load_lanes<mode><mode>"
  [(set (match_operand:VDQX 0 "s_register_operand")
        (unspec:VDQX [(match_operand:VDQX 1 "neon_struct_operand")]
                     UNSPEC_VLD1))]
  "TARGET_NEON")

(define_insn "neon_vld1<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
        (unspec:VDQX [(match_operand:VDQX 1 "neon_struct_operand" "Um")]
                    UNSPEC_VLD1))]
  "TARGET_NEON"
  "vld1.<V_sz_elem>\t%h0, %A1"
  [(set_attr "neon_type" "neon_vld1_1_2_regs")]
)

(define_insn "neon_vld1_lane<mode>"
  [(set (match_operand:VDX 0 "s_register_operand" "=w")
        (unspec:VDX [(match_operand:<V_elem> 1 "neon_struct_operand" "Um")
                     (match_operand:VDX 2 "s_register_operand" "0")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VLD1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  if (max == 1)
    return "vld1.<V_sz_elem>\t%P0, %A1";
  else
    return "vld1.<V_sz_elem>\t{%P0[%c3]}, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_mode_nunits>") (const_int 2))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld1_vld2_lane")))]
)

(define_insn "neon_vld1_lane<mode>"
  [(set (match_operand:VQX 0 "s_register_operand" "=w")
        (unspec:VQX [(match_operand:<V_elem> 1 "neon_struct_operand" "Um")
                     (match_operand:VQX 2 "s_register_operand" "0")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VLD1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
      operands[3] = GEN_INT (lane);
    }
  operands[0] = gen_rtx_REG (<V_HALF>mode, regno);
  if (max == 2)
    return "vld1.<V_sz_elem>\t%P0, %A1";
  else
    return "vld1.<V_sz_elem>\t{%P0[%c3]}, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_mode_nunits>") (const_int 2))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld1_vld2_lane")))]
)

(define_insn "neon_vld1_dup<mode>"
  [(set (match_operand:VDX 0 "s_register_operand" "=w")
        (unspec:VDX [(match_operand:<V_elem> 1 "neon_struct_operand" "Um")]
                    UNSPEC_VLD1_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    return "vld1.<V_sz_elem>\t{%P0[]}, %A1";
  else
    return "vld1.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_insn "neon_vld1_dup<mode>"
  [(set (match_operand:VQX 0 "s_register_operand" "=w")
        (unspec:VQX [(match_operand:<V_elem> 1 "neon_struct_operand" "Um")]
                    UNSPEC_VLD1_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 2)
    return "vld1.<V_sz_elem>\t{%e0[], %f0[]}, %A1";
  else
    return "vld1.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_expand "vec_store_lanes<mode><mode>"
  [(set (match_operand:VDQX 0 "neon_struct_operand")
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand")]
		     UNSPEC_VST1))]
  "TARGET_NEON")

(define_insn "neon_vst1<mode>"
  [(set (match_operand:VDQX 0 "neon_struct_operand" "=Um")
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")]
		     UNSPEC_VST1))]
  "TARGET_NEON"
  "vst1.<V_sz_elem>\t%h1, %A0"
  [(set_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs")])

(define_insn "neon_vst1_lane<mode>"
  [(set (match_operand:<V_elem> 0 "neon_struct_operand" "=Um")
	(vec_select:<V_elem>
	  (match_operand:VDX 1 "s_register_operand" "w")
	  (parallel [(match_operand:SI 2 "neon_lane_number" "i")])))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  if (max == 1)
    return "vst1.<V_sz_elem>\t{%P1}, %A0";
  else
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, %A0";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_mode_nunits>") (const_int 1))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst1_vst2_lane")))])

(define_insn "neon_vst1_lane<mode>"
  [(set (match_operand:<V_elem> 0 "neon_struct_operand" "=Um")
        (vec_select:<V_elem>
           (match_operand:VQX 1 "s_register_operand" "w")
           (parallel [(match_operand:SI 2 "neon_lane_number" "i")])))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
      operands[2] = GEN_INT (lane);
    }
  operands[1] = gen_rtx_REG (<V_HALF>mode, regno);
  if (max == 2)
    return "vst1.<V_sz_elem>\t{%P1}, %A0";
  else
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, %A0";
}
  [(set_attr "neon_type" "neon_vst1_vst2_lane")]
)

(define_expand "vec_load_lanesti<mode>"
  [(set (match_operand:TI 0 "s_register_operand")
        (unspec:TI [(match_operand:TI 1 "neon_struct_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD2))]
  "TARGET_NEON")

(define_insn "neon_vld2<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(match_operand:TI 1 "neon_struct_operand" "Um")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, %A1";
  else
    return "vld2.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")))]
)

(define_expand "vec_load_lanesoi<mode>"
  [(set (match_operand:OI 0 "s_register_operand")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD2))]
  "TARGET_NEON")

(define_insn "neon_vld2<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2))]
  "TARGET_NEON"
  "vld2.<V_sz_elem>\t%h0, %A1"
  [(set_attr "neon_type" "neon_vld2_2_regs_vld1_vld2_all_lanes")])

(define_insn "neon_vld2_lane<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(match_operand:<V_two_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:TI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = operands[1];
  ops[3] = operands[3];
  output_asm_insn ("vld2.<V_sz_elem>\t{%P0[%c3], %P1[%c3]}, %A2", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld1_vld2_lane")]
)

(define_insn "neon_vld2_lane<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:<V_two_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:OI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = operands[1];
  ops[3] = GEN_INT (lane);
  output_asm_insn ("vld2.<V_sz_elem>\t{%P0[%c3], %P1[%c3]}, %A2", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld1_vld2_lane")]
)

(define_insn "neon_vld2_dup<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(match_operand:<V_two_elem> 1 "neon_struct_operand" "Um")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    return "vld2.<V_sz_elem>\t{%e0[], %f0[]}, %A1";
  else
    return "vld1.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld2_2_regs_vld1_vld2_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_expand "vec_store_lanesti<mode>"
  [(set (match_operand:TI 0 "neon_struct_operand")
	(unspec:TI [(match_operand:TI 1 "s_register_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST2))]
  "TARGET_NEON")

(define_insn "neon_vst2<mode>"
  [(set (match_operand:TI 0 "neon_struct_operand" "=Um")
        (unspec:TI [(match_operand:TI 1 "s_register_operand" "w")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST2))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, %A0";
  else
    return "vst2.<V_sz_elem>\t%h1, %A0";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")))]
)

(define_expand "vec_store_lanesoi<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand")
	(unspec:OI [(match_operand:OI 1 "s_register_operand")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST2))]
  "TARGET_NEON")

(define_insn "neon_vst2<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
	(unspec:OI [(match_operand:OI 1 "s_register_operand" "w")
		    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VST2))]
  "TARGET_NEON"
  "vst2.<V_sz_elem>\t%h1, %A0"
  [(set_attr "neon_type" "neon_vst1_1_2_regs_vst2_2_regs")]
)

(define_insn "neon_vst2_lane<mode>"
  [(set (match_operand:<V_two_elem> 0 "neon_struct_operand" "=Um")
	(unspec:<V_two_elem>
	  [(match_operand:TI 1 "s_register_operand" "w")
	   (match_operand:SI 2 "immediate_operand" "i")
	   (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	  UNSPEC_VST2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = operands[2];
  output_asm_insn ("vst2.<V_sz_elem>\t{%P1[%c3], %P2[%c3]}, %A0", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst1_vst2_lane")]
)

(define_insn "neon_vst2_lane<mode>"
  [(set (match_operand:<V_two_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_two_elem>
           [(match_operand:OI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[4];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = GEN_INT (lane);
  output_asm_insn ("vst2.<V_sz_elem>\t{%P1[%c3], %P2[%c3]}, %A0", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst1_vst2_lane")]
)

(define_expand "vec_load_lanesei<mode>"
  [(set (match_operand:EI 0 "s_register_operand")
        (unspec:EI [(match_operand:EI 1 "neon_struct_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD3))]
  "TARGET_NEON")

(define_insn "neon_vld3<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(match_operand:EI 1 "neon_struct_operand" "Um")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, %A1";
  else
    return "vld3.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld3_vld4")))]
)

(define_expand "vec_load_lanesci<mode>"
  [(match_operand:CI 0 "s_register_operand")
   (match_operand:CI 1 "neon_struct_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vld3<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "neon_vld3<mode>"
  [(match_operand:CI 0 "s_register_operand")
   (match_operand:CI 1 "neon_struct_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem;

  mem = adjust_address (operands[1], EImode, 0);
  emit_insn (gen_neon_vld3qa<mode> (operands[0], mem));
  mem = adjust_address (mem, EImode, GET_MODE_SIZE (EImode));
  emit_insn (gen_neon_vld3qb<mode> (operands[0], mem, operands[0]));
  DONE;
})

(define_insn "neon_vld3qa<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:EI 1 "neon_struct_operand" "Um")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3A))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[4];
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = operands[1];
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0, %P1, %P2}, %A3", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld3qb<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:EI 1 "neon_struct_operand" "Um")
                    (match_operand:CI 2 "s_register_operand" "0")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3B))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[4];
  ops[0] = gen_rtx_REG (DImode, regno + 2);
  ops[1] = gen_rtx_REG (DImode, regno + 6);
  ops[2] = gen_rtx_REG (DImode, regno + 10);
  ops[3] = operands[1];
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0, %P1, %P2}, %A3", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld3_lane<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(match_operand:<V_three_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:EI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = operands[1];
  ops[4] = operands[3];
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0[%c4], %P1[%c4], %P2[%c4]}, %A3",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld3_lane<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:<V_three_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:CI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = operands[1];
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0[%c4], %P1[%c4], %P2[%c4]}, %A3",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld3_dup<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(match_operand:<V_three_elem> 1 "neon_struct_operand" "Um")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    {
      int regno = REGNO (operands[0]);
      rtx ops[4];
      ops[0] = gen_rtx_REG (DImode, regno);
      ops[1] = gen_rtx_REG (DImode, regno + 2);
      ops[2] = gen_rtx_REG (DImode, regno + 4);
      ops[3] = operands[1];
      output_asm_insn ("vld3.<V_sz_elem>\t{%P0[], %P1[], %P2[]}, %A3", ops);
      return "";
    }
  else
    return "vld1.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld3_vld4_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))])

(define_expand "vec_store_lanesei<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand")
	(unspec:EI [(match_operand:EI 1 "s_register_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3))]
  "TARGET_NEON")

(define_insn "neon_vst3<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:EI 1 "s_register_operand" "w")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, %A0";
  else
    return "vst3.<V_sz_elem>\t%h1, %A0";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst2_4_regs_vst3_vst4")))])

(define_expand "vec_store_lanesci<mode>"
  [(match_operand:CI 0 "neon_struct_operand")
   (match_operand:CI 1 "s_register_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vst3<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "neon_vst3<mode>"
  [(match_operand:CI 0 "neon_struct_operand")
   (match_operand:CI 1 "s_register_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem;

  mem = adjust_address (operands[0], EImode, 0);
  emit_insn (gen_neon_vst3qa<mode> (mem, operands[1]));
  mem = adjust_address (mem, EImode, GET_MODE_SIZE (EImode));
  emit_insn (gen_neon_vst3qb<mode> (mem, operands[1]));
  DONE;
})

(define_insn "neon_vst3qa<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:CI 1 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3A))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[1]);
  rtx ops[4];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1, %P2, %P3}, %A0", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst3qb<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:CI 1 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3B))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[1]);
  rtx ops[4];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 6);
  ops[3] = gen_rtx_REG (DImode, regno + 10);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1, %P2, %P3}, %A0", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst3_lane<mode>"
  [(set (match_operand:<V_three_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_three_elem>
           [(match_operand:EI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = gen_rtx_REG (DImode, regno + 4);
  ops[4] = operands[2];
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1[%c4], %P2[%c4], %P3[%c4]}, %A0",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst3_vst4_lane")]
)

(define_insn "neon_vst3_lane<mode>"
  [(set (match_operand:<V_three_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_three_elem>
           [(match_operand:CI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[5];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1[%c4], %P2[%c4], %P3[%c4]}, %A0",
                   ops);
  return "";
}
[(set_attr "neon_type" "neon_vst3_vst4_lane")])

(define_expand "vec_load_lanesoi<mode>"
  [(set (match_operand:OI 0 "s_register_operand")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD4))]
  "TARGET_NEON")

(define_insn "neon_vld4<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, %A1";
  else
    return "vld4.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vld1_1_2_regs")
                    (const_string "neon_vld3_vld4")))]
)

(define_expand "vec_load_lanesxi<mode>"
  [(match_operand:XI 0 "s_register_operand")
   (match_operand:XI 1 "neon_struct_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vld4<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "neon_vld4<mode>"
  [(match_operand:XI 0 "s_register_operand")
   (match_operand:XI 1 "neon_struct_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem;

  mem = adjust_address (operands[1], OImode, 0);
  emit_insn (gen_neon_vld4qa<mode> (operands[0], mem));
  mem = adjust_address (mem, OImode, GET_MODE_SIZE (OImode));
  emit_insn (gen_neon_vld4qb<mode> (operands[0], mem, operands[0]));
  DONE;
})

(define_insn "neon_vld4qa<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4A))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[5];
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = gen_rtx_REG (DImode, regno + 12);
  ops[4] = operands[1];
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0, %P1, %P2, %P3}, %A4", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld4qb<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (match_operand:XI 2 "s_register_operand" "0")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4B))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[0]);
  rtx ops[5];
  ops[0] = gen_rtx_REG (DImode, regno + 2);
  ops[1] = gen_rtx_REG (DImode, regno + 6);
  ops[2] = gen_rtx_REG (DImode, regno + 10);
  ops[3] = gen_rtx_REG (DImode, regno + 14);
  ops[4] = operands[1];
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0, %P1, %P2, %P3}, %A4", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4")]
)

(define_insn "neon_vld4_lane<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:<V_four_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:OI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 6);
  ops[4] = operands[1];
  ops[5] = operands[3];
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0[%c5], %P1[%c5], %P2[%c5], %P3[%c5]}, %A4",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld4_lane<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:<V_four_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:XI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[3]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = gen_rtx_REG (DImode, regno + 12);
  ops[4] = operands[1];
  ops[5] = GEN_INT (lane);
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0[%c5], %P1[%c5], %P2[%c5], %P3[%c5]}, %A4",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vld3_vld4_lane")]
)

(define_insn "neon_vld4_dup<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:<V_four_elem> 1 "neon_struct_operand" "Um")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    {
      int regno = REGNO (operands[0]);
      rtx ops[5];
      ops[0] = gen_rtx_REG (DImode, regno);
      ops[1] = gen_rtx_REG (DImode, regno + 2);
      ops[2] = gen_rtx_REG (DImode, regno + 4);
      ops[3] = gen_rtx_REG (DImode, regno + 6);
      ops[4] = operands[1];
      output_asm_insn ("vld4.<V_sz_elem>\t{%P0[], %P1[], %P2[], %P3[]}, %A4",
                       ops);
      return "";
    }
  else
    return "vld1.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "neon_type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_vld3_vld4_all_lanes")
                    (const_string "neon_vld1_1_2_regs")))]
)

(define_expand "vec_store_lanesoi<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand")
	(unspec:OI [(match_operand:OI 1 "s_register_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4))]
  "TARGET_NEON")

(define_insn "neon_vst4<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
        (unspec:OI [(match_operand:OI 1 "s_register_operand" "w")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, %A0";
  else
    return "vst4.<V_sz_elem>\t%h1, %A0";
}
  [(set (attr "neon_type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_vst1_1_2_regs_vst2_2_regs")
                    (const_string "neon_vst2_4_regs_vst3_vst4")))]
)

(define_expand "vec_store_lanesxi<mode>"
  [(match_operand:XI 0 "neon_struct_operand")
   (match_operand:XI 1 "s_register_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vst4<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "neon_vst4<mode>"
  [(match_operand:XI 0 "neon_struct_operand")
   (match_operand:XI 1 "s_register_operand")
   (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem;

  mem = adjust_address (operands[0], OImode, 0);
  emit_insn (gen_neon_vst4qa<mode> (mem, operands[1]));
  mem = adjust_address (mem, OImode, GET_MODE_SIZE (OImode));
  emit_insn (gen_neon_vst4qb<mode> (mem, operands[1]));
  DONE;
})

(define_insn "neon_vst4qa<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
        (unspec:OI [(match_operand:XI 1 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4A))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[1]);
  rtx ops[5];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  ops[4] = gen_rtx_REG (DImode, regno + 12);
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1, %P2, %P3, %P4}, %A0", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst4qb<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
        (unspec:OI [(match_operand:XI 1 "s_register_operand" "w")
                    (unspec:VQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4B))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[1]);
  rtx ops[5];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 6);
  ops[3] = gen_rtx_REG (DImode, regno + 10);
  ops[4] = gen_rtx_REG (DImode, regno + 14);
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1, %P2, %P3, %P4}, %A0", ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst2_4_regs_vst3_vst4")]
)

(define_insn "neon_vst4_lane<mode>"
  [(set (match_operand:<V_four_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_four_elem>
           [(match_operand:OI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VD [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = gen_rtx_REG (DImode, regno + 4);
  ops[4] = gen_rtx_REG (DImode, regno + 6);
  ops[5] = operands[2];
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1[%c5], %P2[%c5], %P3[%c5], %P4[%c5]}, %A0",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst3_vst4_lane")]
)

(define_insn "neon_vst4_lane<mode>"
  [(set (match_operand:<V_four_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_four_elem>
           [(match_operand:XI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VMQ [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = INTVAL (operands[2]);
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[6];
  if (lane < 0 || lane >= max)
    error ("lane out of range");
  else if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  ops[4] = gen_rtx_REG (DImode, regno + 12);
  ops[5] = GEN_INT (lane);
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1[%c5], %P2[%c5], %P3[%c5], %P4[%c5]}, %A0",
                   ops);
  return "";
}
  [(set_attr "neon_type" "neon_vst3_vst4_lane")]
)

(define_expand "neon_vand<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_inv_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_and<mode>3<V_suf64> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vorr<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_ior<mode>3<V_suf64> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_veor<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "s_register_operand" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_xor<mode>3<V_suf64> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vbic<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_bic<mode>3_neon (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vorn<mode>"
  [(match_operand:VDQX 0 "s_register_operand" "")
   (match_operand:VDQX 1 "s_register_operand" "")
   (match_operand:VDQX 2 "neon_inv_logic_op2" "")
   (match_operand:SI 3 "immediate_operand" "")]
  "TARGET_NEON"
{
  emit_insn (gen_orn<mode>3_neon (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "neon_vec_unpack<US>_lo_<mode>"
  [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
        (SE:<V_unpack> (vec_select:<V_HALF>
			  (match_operand:VU 1 "register_operand" "w")
			  (match_operand:VU 2 "vect_par_constant_low" ""))))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vmovl.<US><V_sz_elem> %q0, %e1"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_insn "neon_vec_unpack<US>_hi_<mode>"
  [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
        (SE:<V_unpack> (vec_select:<V_HALF>
			  (match_operand:VU 1 "register_operand" "w")
			  (match_operand:VU 2 "vect_par_constant_high" ""))))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vmovl.<US><V_sz_elem> %q0, %f1"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_expand "vec_unpack<US>_hi_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand" "")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand"))]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
  {
   rtvec v = rtvec_alloc (<V_mode_nunits>/2)  ;
   rtx t1;
   int i;
   for (i = 0; i < (<V_mode_nunits>/2); i++)
     RTVEC_ELT (v, i) = GEN_INT ((<V_mode_nunits>/2) + i);
  
   t1 = gen_rtx_PARALLEL (<MODE>mode, v);
   emit_insn (gen_neon_vec_unpack<US>_hi_<mode> (operands[0], 
                                                 operands[1], 
					         t1));
   DONE;
  }
)

(define_expand "vec_unpack<US>_lo_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand" "")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand" ""))]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
  {
   rtvec v = rtvec_alloc (<V_mode_nunits>/2)  ;
   rtx t1;
   int i;
   for (i = 0; i < (<V_mode_nunits>/2) ; i++)
     RTVEC_ELT (v, i) = GEN_INT (i);
   t1 = gen_rtx_PARALLEL (<MODE>mode, v);
   emit_insn (gen_neon_vec_unpack<US>_lo_<mode> (operands[0], 
                                                 operands[1], 
				   	         t1));
   DONE;
  }
)

(define_insn "neon_vec_<US>mult_lo_<mode>"
 [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
       (mult:<V_unpack> (SE:<V_unpack> (vec_select:<V_HALF>
			   (match_operand:VU 1 "register_operand" "w") 
                           (match_operand:VU 2 "vect_par_constant_low" "")))
 		        (SE:<V_unpack> (vec_select:<V_HALF>
                           (match_operand:VU 3 "register_operand" "w") 
                           (match_dup 2)))))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vmull.<US><V_sz_elem> %q0, %e1, %e3"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_expand "vec_widen_<US>mult_lo_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand" "")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand" ""))
   (SE:<V_unpack> (match_operand:VU 2 "register_operand" ""))]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 {
   rtvec v = rtvec_alloc (<V_mode_nunits>/2)  ;
   rtx t1;
   int i;
   for (i = 0; i < (<V_mode_nunits>/2) ; i++)
     RTVEC_ELT (v, i) = GEN_INT (i);
   t1 = gen_rtx_PARALLEL (<MODE>mode, v);

   emit_insn (gen_neon_vec_<US>mult_lo_<mode> (operands[0],
 					       operands[1],
					       t1,
					       operands[2]));
   DONE;
 }
)

(define_insn "neon_vec_<US>mult_hi_<mode>"
 [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
      (mult:<V_unpack> (SE:<V_unpack> (vec_select:<V_HALF>
			    (match_operand:VU 1 "register_operand" "w") 
			    (match_operand:VU 2 "vect_par_constant_high" "")))
		       (SE:<V_unpack> (vec_select:<V_HALF>
			    (match_operand:VU 3 "register_operand" "w") 
			    (match_dup 2)))))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vmull.<US><V_sz_elem> %q0, %f1, %f3"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_expand "vec_widen_<US>mult_hi_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand" "")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand" ""))
   (SE:<V_unpack> (match_operand:VU 2 "register_operand" ""))]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 {
   rtvec v = rtvec_alloc (<V_mode_nunits>/2)  ;
   rtx t1;
   int i;
   for (i = 0; i < (<V_mode_nunits>/2) ; i++)
     RTVEC_ELT (v, i) = GEN_INT (<V_mode_nunits>/2 + i);
   t1 = gen_rtx_PARALLEL (<MODE>mode, v);

   emit_insn (gen_neon_vec_<US>mult_hi_<mode> (operands[0],
 					       operands[1],
					       t1,
					       operands[2]));
   DONE;

 }
)

(define_insn "neon_vec_<US>shiftl_<mode>"
 [(set (match_operand:<V_widen> 0 "register_operand" "=w")
       (SE:<V_widen> (ashift:VW (match_operand:VW 1 "register_operand" "w")
       (match_operand:<V_innermode> 2 "const_neon_scalar_shift_amount_operand" ""))))]
  "TARGET_NEON"
{
  return "vshll.<US><V_sz_elem> %q0, %P1, %2";
}
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_expand "vec_widen_<US>shiftl_lo_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand" "")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand" ""))
   (match_operand:SI 2 "immediate_operand" "i")]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 {
  emit_insn (gen_neon_vec_<US>shiftl_<V_half> (operands[0],
		simplify_gen_subreg (<V_HALF>mode, operands[1], <MODE>mode, 0),
		operands[2]));
   DONE;
 }
)

(define_expand "vec_widen_<US>shiftl_hi_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand" "")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand" ""))
   (match_operand:SI 2 "immediate_operand" "i")]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 {
  emit_insn (gen_neon_vec_<US>shiftl_<V_half> (operands[0],
                simplify_gen_subreg (<V_HALF>mode, operands[1], <MODE>mode,
				     GET_MODE_SIZE (<V_HALF>mode)),
                operands[2]));
   DONE;
 }
)

;; Vectorize for non-neon-quad case
(define_insn "neon_unpack<US>_<mode>"
 [(set (match_operand:<V_widen> 0 "register_operand" "=w")
       (SE:<V_widen> (match_operand:VDI 1 "register_operand" "w")))]
 "TARGET_NEON"
 "vmovl.<US><V_sz_elem> %q0, %P1"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_expand "vec_unpack<US>_lo_<mode>"
 [(match_operand:<V_double_width> 0 "register_operand" "")
  (SE:<V_double_width>(match_operand:VDI 1 "register_operand"))]
 "TARGET_NEON"
{
  rtx tmpreg = gen_reg_rtx (<V_widen>mode);
  emit_insn (gen_neon_unpack<US>_<mode> (tmpreg, operands[1]));
  emit_insn (gen_neon_vget_low<V_widen_l> (operands[0], tmpreg));

  DONE;
}
)

(define_expand "vec_unpack<US>_hi_<mode>"
 [(match_operand:<V_double_width> 0 "register_operand" "")
  (SE:<V_double_width>(match_operand:VDI 1 "register_operand"))]
 "TARGET_NEON"
{
  rtx tmpreg = gen_reg_rtx (<V_widen>mode);
  emit_insn (gen_neon_unpack<US>_<mode> (tmpreg, operands[1]));
  emit_insn (gen_neon_vget_high<V_widen_l> (operands[0], tmpreg));

  DONE;
}
)

(define_insn "neon_vec_<US>mult_<mode>"
 [(set (match_operand:<V_widen> 0 "register_operand" "=w")
       (mult:<V_widen> (SE:<V_widen> 
		 	   (match_operand:VDI 1 "register_operand" "w"))
 		       (SE:<V_widen> 
			   (match_operand:VDI 2 "register_operand" "w"))))]
  "TARGET_NEON"
  "vmull.<US><V_sz_elem> %q0, %P1, %P2"
  [(set_attr "neon_type" "neon_shift_1")]
)

(define_expand "vec_widen_<US>mult_hi_<mode>"
  [(match_operand:<V_double_width> 0 "register_operand" "")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand" ""))
   (SE:<V_double_width> (match_operand:VDI 2 "register_operand" ""))]
 "TARGET_NEON"
 {
   rtx tmpreg = gen_reg_rtx (<V_widen>mode);
   emit_insn (gen_neon_vec_<US>mult_<mode> (tmpreg, operands[1], operands[2]));
   emit_insn (gen_neon_vget_high<V_widen_l> (operands[0], tmpreg));
 					    
   DONE;

 }
)

(define_expand "vec_widen_<US>mult_lo_<mode>"
  [(match_operand:<V_double_width> 0 "register_operand" "")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand" ""))
   (SE:<V_double_width> (match_operand:VDI 2 "register_operand" ""))]
 "TARGET_NEON"
 {
   rtx tmpreg = gen_reg_rtx (<V_widen>mode);
   emit_insn (gen_neon_vec_<US>mult_<mode> (tmpreg, operands[1], operands[2]));
   emit_insn (gen_neon_vget_low<V_widen_l> (operands[0], tmpreg));
 					    
   DONE;

 }
)

(define_expand "vec_widen_<US>shiftl_hi_<mode>"
 [(match_operand:<V_double_width> 0 "register_operand" "")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand" ""))
   (match_operand:SI 2 "immediate_operand" "i")]
 "TARGET_NEON"
 {
   rtx tmpreg = gen_reg_rtx (<V_widen>mode);
   emit_insn (gen_neon_vec_<US>shiftl_<mode> (tmpreg, operands[1], operands[2]));
   emit_insn (gen_neon_vget_high<V_widen_l> (operands[0], tmpreg));

   DONE;
 }
)

(define_expand "vec_widen_<US>shiftl_lo_<mode>"
  [(match_operand:<V_double_width> 0 "register_operand" "")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand" ""))
   (match_operand:SI 2 "immediate_operand" "i")]
 "TARGET_NEON"
 {
   rtx tmpreg = gen_reg_rtx (<V_widen>mode);
   emit_insn (gen_neon_vec_<US>shiftl_<mode> (tmpreg, operands[1], operands[2]));
   emit_insn (gen_neon_vget_low<V_widen_l> (operands[0], tmpreg));

   DONE;
 }
)

; FIXME: These instruction patterns can't be used safely in big-endian mode
; because the ordering of vector elements in Q registers is different from what
; the semantics of the instructions require.

(define_insn "vec_pack_trunc_<mode>"
 [(set (match_operand:<V_narrow_pack> 0 "register_operand" "=&w")
       (vec_concat:<V_narrow_pack> 
		(truncate:<V_narrow> 
			(match_operand:VN 1 "register_operand" "w"))
		(truncate:<V_narrow>
			(match_operand:VN 2 "register_operand" "w"))))]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 "vmovn.i<V_sz_elem>\t%e0, %q1\;vmovn.i<V_sz_elem>\t%f0, %q2"
 [(set_attr "neon_type" "neon_shift_1")
  (set_attr "length" "8")]
)

;; For the non-quad case.
(define_insn "neon_vec_pack_trunc_<mode>"
 [(set (match_operand:<V_narrow> 0 "register_operand" "=w")
       (truncate:<V_narrow> (match_operand:VN 1 "register_operand" "w")))]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 "vmovn.i<V_sz_elem>\t%P0, %q1"
 [(set_attr "neon_type" "neon_shift_1")]
)

(define_expand "vec_pack_trunc_<mode>"
 [(match_operand:<V_narrow_pack> 0 "register_operand" "")
  (match_operand:VSHFT 1 "register_operand" "")
  (match_operand:VSHFT 2 "register_operand")]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx tempreg = gen_reg_rtx (<V_DOUBLE>mode);
  
  emit_insn (gen_move_lo_quad_<V_double> (tempreg, operands[1])); 
  emit_insn (gen_move_hi_quad_<V_double> (tempreg, operands[2])); 
  emit_insn (gen_neon_vec_pack_trunc_<V_double> (operands[0], tempreg));
  DONE;
})

(define_insn "neon_vabd<mode>_2"
 [(set (match_operand:VDQ 0 "s_register_operand" "=w")
       (abs:VDQ (minus:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
                           (match_operand:VDQ 2 "s_register_operand" "w"))))]
 "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
 "vabd.<V_s_elem> %<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set (attr "neon_type")
       (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                                   (const_string "neon_fp_vadd_qqq_vabs_qq"))
                     (const_string "neon_int_5")))]
)

(define_insn "neon_vabd<mode>_3"
 [(set (match_operand:VDQ 0 "s_register_operand" "=w")
       (abs:VDQ (unspec:VDQ [(match_operand:VDQ 1 "s_register_operand" "w")
                             (match_operand:VDQ 2 "s_register_operand" "w")]
                 UNSPEC_VSUB)))]
 "TARGET_NEON && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
 "vabd.<V_if_elem> %<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set (attr "neon_type")
       (if_then_else (ne (symbol_ref "<Is_float_mode>") (const_int 0))
                     (if_then_else (ne (symbol_ref "<Is_d_reg>") (const_int 0))
                                   (const_string "neon_fp_vadd_ddd_vabs_dd")
                                   (const_string "neon_fp_vadd_qqq_vabs_qq"))
                     (const_string "neon_int_5")))]
)
