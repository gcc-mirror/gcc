;; ARM NEON coprocessor Machine Description
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


;; Attribute used to permit string comparisons against <VQH_mnem> in
;; type attribute definitions.
(define_attr "vqh_mnem" "vadd,vmin,vmax" (const_string "vadd"))

(define_insn "unaligned_storev8qi"
  [(set (match_operand:V8QI 0 "memory_operand" "=Un")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "w")]
		     UNSPEC_UNALIGNED_STORE))]
  "TARGET_NEON"
  "*
  return output_move_neon (operands);
  "
  [(set_attr "type" "neon_store1_1reg")])

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VDXMOV 0 "nonimmediate_operand"
	  "=w,Un,w, w, w,  ?r,?w,?r, ?Us,*r")
	(match_operand:VDXMOV 1 "general_operand"
	  " w,w, Dm,Dn,Uni, w, r, Usi,r,*r"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  if (which_alternative == 2 || which_alternative == 3)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = simd_immediate_valid_for_move (operands[1], <MODE>mode,
        &operands[1], &width);

      gcc_assert (is_valid != 0);

      if (width == 0)
        return "vmov.f32\t%P0, %1  @ <mode>";
      else
        sprintf (templ, "vmov.i%d\t%%P0, %%x1  @ <mode>", width);

      return templ;
    }

  switch (which_alternative)
    {
    case 0: return "vmov\t%P0, %P1  @ <mode>";
    case 1: case 4: return output_move_neon (operands);
    case 2: case 3: gcc_unreachable ();
    case 5: return "vmov\t%Q0, %R0, %P1  @ <mode>";
    case 6: return "vmov\t%P0, %Q1, %R1  @ <mode>";
    case 9: return "#";
    default: return output_move_double (operands, true, NULL);
    }
}
 [(set_attr "type" "neon_move<q>,neon_store1_1reg,neon_move<q>,\
                    neon_move<q>,neon_load1_1reg, neon_to_gp<q>,\
                    neon_from_gp<q>,neon_load1_2reg, neon_store1_2reg,\
		    multiple")
  (set_attr "length" "4,4,4,4,4,4,4,8,8,8")
  (set_attr "arm_pool_range"     "*,*,*,*,1020,*,*,1020,*,*")
  (set_attr "thumb2_pool_range"     "*,*,*,*,1018,*,*,1018,*,*")
  (set_attr "neg_pool_range" "*,*,*,*,1004,*,*,1004,*,*")])

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VQXMOV 0 "nonimmediate_operand"
	  "=w,Un,w, w, w,  ?r,?w,?r,?r,  ?Us")
	(match_operand:VQXMOV 1 "general_operand"
	  " w,w, Dm,DN,Uni, w, r, r, Usi, r"))]
  "TARGET_NEON
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  if (which_alternative == 2 || which_alternative == 3)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = simd_immediate_valid_for_move (operands[1], <MODE>mode,
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
    case 1: case 4: return output_move_neon (operands);
    case 2: case 3: gcc_unreachable ();
    case 5: return "vmov\t%Q0, %R0, %e1  @ <mode>\;vmov\t%J0, %K0, %f1";
    case 6: return "vmov\t%e0, %Q1, %R1  @ <mode>\;vmov\t%f0, %J1, %K1";
    default: return output_move_quad (operands);
    }
}
  [(set_attr "type" "neon_move_q,neon_store2_2reg_q,neon_move_q,\
                     neon_move_q,neon_load2_2reg_q,neon_to_gp_q,\
                     neon_from_gp_q,mov_reg,neon_load1_4reg,neon_store1_4reg")
   (set_attr "length" "4,8,4,4,8,8,8,16,8,16")
   (set_attr "arm_pool_range" "*,*,*,*,1020,*,*,*,1020,*")
   (set_attr "thumb2_pool_range" "*,*,*,*,1018,*,*,*,1018,*")
   (set_attr "neg_pool_range" "*,*,*,*,996,*,*,*,996,*")])

/* We define these mov expanders to match the standard mov$a optab to prevent
   the mid-end from trying to do a subreg for these modes which is the most
   inefficient way to expand the move.  Also big-endian subreg's aren't
   allowed for a subset of modes, See TARGET_CAN_CHANGE_MODE_CLASS.
   Without these RTL generation patterns the mid-end would attempt to take a
   sub-reg and may ICE if it can't.  */

(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand")
	(match_operand:TI 1 "general_operand"))]
  "TARGET_NEON"
{
  gcc_checking_assert (aligned_operand (operands[0], TImode));
  gcc_checking_assert (aligned_operand (operands[1], TImode));
  if (can_create_pseudo_p ())
    {
      if (!REG_P (operands[0]))
	operands[1] = force_reg (TImode, operands[1]);
    }
})

(define_expand "mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand")
	(match_operand:VSTRUCT 1 "general_operand"))]
  "TARGET_NEON || TARGET_HAVE_MVE"
{
  gcc_checking_assert (aligned_operand (operands[0], <MODE>mode));
  gcc_checking_assert (aligned_operand (operands[1], <MODE>mode));
  if (can_create_pseudo_p ())
    {
      if (!REG_P (operands[0]))
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
})

;; The pattern mov<mode> where mode is v8hf, v4hf, v4bf and v8bf are split into
;; two groups.  The pattern movv8hf is common for MVE and NEON, so it is moved
;; into vec-common.md file.  Remaining mov expand patterns with half float and
;; bfloats are implemented below.
(define_expand "mov<mode>"
  [(set (match_operand:VHFBF_split 0 "s_register_operand")
	(match_operand:VHFBF_split 1 "s_register_operand"))]
  "TARGET_NEON"
{
  gcc_checking_assert (aligned_operand (operands[0], <MODE>mode));
  gcc_checking_assert (aligned_operand (operands[1], <MODE>mode));
  if (can_create_pseudo_p ())
    {
      if (!REG_P (operands[0]))
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
})

(define_insn "*neon_mov<mode>"
  [(set (match_operand:VSTRUCT 0 "nonimmediate_operand"	"=w,Ut,w")
	(match_operand:VSTRUCT 1 "general_operand"	" w,w, Ut"))]
  "(TARGET_NEON || TARGET_HAVE_MVE)
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
  [(set_attr "type" "neon_move_q,neon_store2_2reg_q,neon_load2_2reg_q")
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
  [(set (match_operand:VSTRUCT2 0 "s_register_operand" "")
	(match_operand:VSTRUCT2 1 "s_register_operand" ""))]
  "(TARGET_NEON || TARGET_HAVE_MVE)&& reload_completed"
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
  [(set (match_operand:VSTRUCT4 0 "s_register_operand" "")
	(match_operand:VSTRUCT4 1 "s_register_operand" ""))]
  "(TARGET_NEON || TARGET_HAVE_MVE) && reload_completed"
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

(define_insn "*movmisalign<mode>_neon_store"
  [(set (match_operand:VDX 0 "neon_permissive_struct_operand"	"=Um")
	(unspec:VDX [(match_operand:VDX 1 "s_register_operand" " w")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN && unaligned_access"
  "vst1.<V_sz_elem>\t{%P1}, %A0"
  [(set_attr "type" "neon_store1_1reg<q>")])

(define_insn "*movmisalign<mode>_neon_load"
  [(set (match_operand:VDX 0 "s_register_operand"			"=w")
	(unspec:VDX [(match_operand:VDX 1 "neon_permissive_struct_operand"
									" Um")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN && unaligned_access"
  "vld1.<V_sz_elem>\t{%P0}, %A1"
  [(set_attr "type" "neon_load1_1reg<q>")])

(define_insn "*movmisalign<mode>_neon_store"
  [(set (match_operand:VQX 0 "neon_permissive_struct_operand"  "=Um")
	(unspec:VQX [(match_operand:VQX 1 "s_register_operand" " w")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN && unaligned_access"
  "vst1.<V_sz_elem>\t{%q1}, %A0"
  [(set_attr "type" "neon_store1_1reg<q>")])

(define_insn "*movmisalign<mode>_neon_load"
  [(set (match_operand:VQX 0 "s_register_operand"			"=w")
	(unspec:VQX [(match_operand:VQX 1 "neon_permissive_struct_operand"
									" Um")]
		    UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN && unaligned_access"
  "vld1.<V_sz_elem>\t{%q0}, %A1"
  [(set_attr "type" "neon_load1_1reg<q>")])

(define_insn "@vec_set<mode>_internal"
  [(set (match_operand:VD_LANE 0 "s_register_operand" "=w,w")
        (vec_merge:VD_LANE
          (vec_duplicate:VD_LANE
            (match_operand:<V_elem> 1 "nonimmediate_operand" "Um,r"))
          (match_operand:VD_LANE 3 "s_register_operand" "0,0")
          (match_operand:SI 2 "immediate_operand" "i,i")))]
  "TARGET_NEON"
{
  int elt = ffs ((int) INTVAL (operands[2])) - 1;
  if (BYTES_BIG_ENDIAN)
    elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
  operands[2] = GEN_INT (elt);

  if (which_alternative == 0)
    return "vld1.<V_sz_elem>\t{%P0[%c2]}, %A1";
  else
    return "vmov.<V_sz_elem>\t%P0[%c2], %1";
}
  [(set_attr "type" "neon_load1_all_lanes<q>,neon_from_gp<q>")])

(define_insn "@vec_set<mode>_internal"
  [(set (match_operand:VQ2 0 "s_register_operand" "=w,w")
        (vec_merge:VQ2
          (vec_duplicate:VQ2
            (match_operand:<V_elem> 1 "nonimmediate_operand" "Um,r"))
          (match_operand:VQ2 3 "s_register_operand" "0,0")
          (match_operand:SI 2 "immediate_operand" "i,i")))]
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

  if (which_alternative == 0)
    return "vld1.<V_sz_elem>\t{%P0[%c2]}, %A1";
  else
    return "vmov.<V_sz_elem>\t%P0[%c2], %1";
}
  [(set_attr "type" "neon_load1_all_lanes<q>,neon_from_gp<q>")]
)

(define_insn "@vec_set<mode>_internal"
  [(set (match_operand:V2DI_ONLY 0 "s_register_operand" "=w,w")
        (vec_merge:V2DI_ONLY
          (vec_duplicate:V2DI_ONLY
            (match_operand:DI 1 "nonimmediate_operand" "Um,r"))
          (match_operand:V2DI_ONLY 3 "s_register_operand" "0,0")
          (match_operand:SI 2 "immediate_operand" "i,i")))]
  "TARGET_NEON"
{
  HOST_WIDE_INT elem = ffs ((int) INTVAL (operands[2])) - 1;
  int regno = REGNO (operands[0]) + 2 * elem;

  operands[0] = gen_rtx_REG (DImode, regno);

  if (which_alternative == 0)
    return "vld1.64\t%P0, %A1";
  else
    return "vmov\t%P0, %Q1, %R1";
}
  [(set_attr "type" "neon_load1_all_lanes_q,neon_from_gp_q")]
)

(define_insn "vec_extract<mode><V_elem_l>"
  [(set (match_operand:<V_elem> 0 "nonimmediate_operand" "=Um,r")
        (vec_select:<V_elem>
          (match_operand:VD_LANE 1 "s_register_operand" "w,w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i,i")])))]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      int elt = INTVAL (operands[2]);
      elt = GET_MODE_NUNITS (<MODE>mode) - 1 - elt;
      operands[2] = GEN_INT (elt);
    }

  if (which_alternative == 0)
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, %A0";
  else
    return "vmov.<V_uf_sclr>\t%0, %P1[%c2]";
}
  [(set_attr "type" "neon_store1_one_lane<q>,neon_to_gp<q>")]
)

;; Patterns comparing two vectors and conditionally jump.
;; Avdanced SIMD lacks a vector != comparison, but this is a quite common
;; operation.  To not pay the penalty for inverting == we can map our any
;; comparisons to all i.e. any(~x) => all(x).
;;
;; However unlike the AArch64 version, we can't optimize this further as the
;; chain is too long for combine due to these being unspecs so it doesn't fold
;; the operation to something simpler.
(define_expand "cbranch<mode>4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:VDQI 1 "register_operand")
	        (match_operand:VDQI 2 "reg_or_zero_operand")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_NEON"
{
  rtx mask = operands[1];

  /* If comparing against a non-zero vector we have to do a comparison first
     so we can have a != 0 comparison with the result.  */
  if (operands[2] != CONST0_RTX (<MODE>mode))
    {
      mask = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_xor<mode>3 (mask, operands[1], operands[2]));
    }

  /* For 128-bit vectors we need an additional reductions.  */
  if (known_eq (128, GET_MODE_BITSIZE (<MODE>mode)))
    {
      /* Always reduce using a V4SI.  */
      rtx op1 = lowpart_subreg (V4SImode, mask, <MODE>mode);
      mask = gen_reg_rtx (V2SImode);
      rtx low = gen_reg_rtx (V2SImode);
      rtx high = gen_reg_rtx (V2SImode);
      emit_insn (gen_neon_vget_lowv4si (low, op1));
      emit_insn (gen_neon_vget_highv4si (high, op1));
      emit_insn (gen_neon_vpumaxv2si (mask, low, high));
    }

  rtx op1 = lowpart_subreg (V2SImode, mask, GET_MODE (mask));
  emit_insn (gen_neon_vpumaxv2si (op1, op1, op1));

  rtx val = gen_reg_rtx (SImode);
  emit_move_insn (val, gen_lowpart (SImode, mask));
  emit_jump_insn (gen_cbranch_cc (operands[0], val, const0_rtx, operands[3]));
  DONE;
})

;; This pattern is renamed from "vec_extract<mode><V_elem_l>" to
;; "neon_vec_extract<mode><V_elem_l>" and this pattern is called
;; by define_expand in vec-common.md file.
(define_insn "neon_vec_extract<mode><V_elem_l>"
  [(set (match_operand:<V_elem> 0 "nonimmediate_operand" "=Um,r")
	(vec_select:<V_elem>
          (match_operand:VQ2 1 "s_register_operand" "w,w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i,i")])))]
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

  if (which_alternative == 0)
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, %A0";
  else
    return "vmov.<V_uf_sclr>\t%0, %P1[%c2]";
}
  [(set_attr "type" "neon_store1_one_lane<q>,neon_to_gp<q>")]
)

;; This pattern is renamed from "vec_extractv2didi" to "neon_vec_extractv2didi"
;; and this pattern is called by define_expand in vec-common.md file.
(define_insn "neon_vec_extractv2didi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=Um,r")
	(vec_select:DI
          (match_operand:V2DI 1 "s_register_operand" "w,w")
          (parallel [(match_operand:SI 2 "immediate_operand" "i,i")])))]
  "TARGET_NEON"
{
  int regno = REGNO (operands[1]) + 2 * INTVAL (operands[2]);

  operands[1] = gen_rtx_REG (DImode, regno);

  if (which_alternative == 0)
    return "vst1.64\t{%P1}, %A0  @ v2di";
  else
    return "vmov\t%Q0, %R0, %P1  @ v2di";
}
  [(set_attr "type" "neon_store1_one_lane_q,neon_to_gp_q")]
)

;; Doubleword and quadword arithmetic.

;; NOTE: some other instructions also support 64-bit integer
;; element size, which we could potentially use for "long long" operations.

(define_insn "*add<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (plus:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		  (match_operand:VDQ 2 "s_register_operand" "w")))]
  "ARM_HAVE_NEON_<MODE>_ARITH"
  "vadd.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_addsub_s<q>")
                    (const_string "neon_add<q>")))]
)

(define_insn "*sub<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (minus:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
                   (match_operand:VDQ 2 "s_register_operand" "w")))]
  "ARM_HAVE_NEON_<MODE>_ARITH"
  "vsub.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_addsub_s<q>")
                    (const_string "neon_sub<q>")))]
)

(define_insn "*mul<mode>3_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (mult:VDQW (match_operand:VDQW 1 "s_register_operand" "w")
                   (match_operand:VDQW 2 "s_register_operand" "w")))]
  "ARM_HAVE_NEON_<MODE>_ARITH"
  "vmul.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
		    (const_string "neon_fp_mul_s<q>")
                    (const_string "neon_mul_<V_elem_ch><q>")))]
)

/* Perform division using multiply-by-reciprocal.
   Reciprocal is calculated using Newton-Raphson method.
   Enabled with -funsafe-math-optimizations -freciprocal-math
   and disabled for -Os since it increases code size .  */

(define_expand "div<VCVTF:mode>3"
  [(set (match_operand:VCVTF 0 "s_register_operand")
        (div:VCVTF (match_operand:VCVTF 1 "s_register_operand")
		  (match_operand:VCVTF 2 "s_register_operand")))]
  "ARM_HAVE_NEON_<MODE>_ARITH && !optimize_size
   && flag_reciprocal_math"
  {
    rtx rec = gen_reg_rtx (<MODE>mode);
    rtx vrecps_temp = gen_reg_rtx (<MODE>mode);

    /* Reciprocal estimate.  */
    emit_insn (gen_neon_vrecpe<mode> (rec, operands[2]));

    /* Perform 2 iterations of newton-raphson method.  */
    for (int i = 0; i < 2; i++)
      {
	emit_insn (gen_neon_vrecps<mode> (vrecps_temp, rec, operands[2]));
	emit_insn (gen_mul<mode>3 (rec, rec, vrecps_temp));
      }

    /* We now have reciprocal in rec, perform operands[0] = operands[1] * rec.  */
    emit_insn (gen_mul<mode>3 (operands[0], operands[1], rec));
    DONE;
  }
)


(define_insn "mul<mode>3add<mode>_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (plus:VDQW (mult:VDQW (match_operand:VDQW 2 "s_register_operand" "w")
                            (match_operand:VDQW 3 "s_register_operand" "w"))
		  (match_operand:VDQW 1 "s_register_operand" "0")))]
  "ARM_HAVE_NEON_<MODE>_ARITH"
  "vmla.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
		    (const_string "neon_fp_mla_s<q>")
		    (const_string "neon_mla_<V_elem_ch><q>")))]
)

(define_insn "mul<mode>3add<mode>_neon"
  [(set (match_operand:VH 0 "s_register_operand" "=w")
	(plus:VH (mult:VH (match_operand:VH 2 "s_register_operand" "w")
			  (match_operand:VH 3 "s_register_operand" "w"))
		  (match_operand:VH 1 "s_register_operand" "0")))]
  "ARM_HAVE_NEON_<MODE>_ARITH"
  "vmla.f16\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "mul<mode>3neg<mode>add<mode>_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
        (minus:VDQW (match_operand:VDQW 1 "s_register_operand" "0")
                    (mult:VDQW (match_operand:VDQW 2 "s_register_operand" "w")
                               (match_operand:VDQW 3 "s_register_operand" "w"))))]
  "ARM_HAVE_NEON_<MODE>_ARITH"
  "vmls.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
		    (const_string "neon_fp_mla_s<q>")
		    (const_string "neon_mla_<V_elem_ch><q>")))]
)

;; Fused multiply-accumulate
;; We define each insn twice here:
;;    1: with flag_unsafe_math_optimizations for the widening multiply phase
;;       to be able to use when converting to FMA.
;;    2: without flag_unsafe_math_optimizations for the intrinsics to use.
(define_insn "fma<VCVTF:mode>4"
  [(set (match_operand:VCVTF 0 "register_operand" "=w")
        (fma:VCVTF (match_operand:VCVTF 1 "register_operand" "w")
		 (match_operand:VCVTF 2 "register_operand" "w")
		 (match_operand:VCVTF 3 "register_operand" "0")))]
  "ARM_HAVE_NEON_<MODE>_ARITH && TARGET_FMA"
  "vfma.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "fma<VCVTF:mode>4_intrinsic"
  [(set (match_operand:VCVTF 0 "register_operand" "=w")
        (fma:VCVTF (match_operand:VCVTF 1 "register_operand" "w")
		 (match_operand:VCVTF 2 "register_operand" "w")
		 (match_operand:VCVTF 3 "register_operand" "0")))]
  "TARGET_NEON && TARGET_FMA"
  "vfma.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "fma<VH:mode>4"
 [(set (match_operand:VH 0 "register_operand" "=w")
   (fma:VH
    (match_operand:VH 1 "register_operand" "w")
    (match_operand:VH 2 "register_operand" "w")
    (match_operand:VH 3 "register_operand" "0")))]
 "ARM_HAVE_NEON_<MODE>_ARITH"
 "vfma.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "*fmsub<VCVTF:mode>4"
  [(set (match_operand:VCVTF 0 "register_operand" "=w")
        (fma:VCVTF (neg:VCVTF (match_operand:VCVTF 1 "register_operand" "w"))
		   (match_operand:VCVTF 2 "register_operand" "w")
		   (match_operand:VCVTF 3 "register_operand" "0")))]
  "ARM_HAVE_NEON_<MODE>_ARITH && TARGET_FMA"
  "vfms.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "fmsub<VCVTF:mode>4_intrinsic"
 [(set (match_operand:VCVTF 0 "register_operand" "=w")
   (fma:VCVTF
    (neg:VCVTF (match_operand:VCVTF 1 "register_operand" "w"))
    (match_operand:VCVTF 2 "register_operand" "w")
    (match_operand:VCVTF 3 "register_operand" "0")))]
 "TARGET_NEON && TARGET_FMA"
 "vfms.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "fmsub<VH:mode>4_intrinsic"
 [(set (match_operand:VH 0 "register_operand" "=w")
   (fma:VH
    (neg:VH (match_operand:VH 1 "register_operand" "w"))
    (match_operand:VH 2 "register_operand" "w")
    (match_operand:VH 3 "register_operand" "0")))]
 "TARGET_NEON_FP16INST"
 "vfms.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_expand "<NEON_VRINT:nvrint_pattern><VCVTF:mode>2"
  [(set (match_operand:VCVTF 0 "s_register_operand")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand")]
		      NEON_VRINT))]
  "TARGET_NEON && TARGET_VFP5 && flag_unsafe_math_optimizations"
)

(define_insn "neon_vrint<NEON_VRINT:nvrint_variant><VCVTF:mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1
		         "s_register_operand" "w")]
		NEON_VRINT))]
  "TARGET_NEON && TARGET_VFP5"
  "vrint<nvrint_variant>.f32\\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_round_<V_elem_ch><q>")]
)

(define_expand "l<NEON_VCVT:nvrint_pattern><su_optab><VCVTF:mode><v_cmp_result>2"
  [(set (match_operand:<V_cmp_result> 0 "register_operand")
	(FIXUORS:<V_cmp_result>
	  (unspec:VCVTF [(match_operand:VCVTF 1 "register_operand")]
			NEON_VCVT)))]
  "TARGET_NEON && TARGET_VFP5 && flag_unsafe_math_optimizations"
)

(define_insn "neon_vcvt<NEON_VCVT:nvrint_variant><su_optab><VCVTF:mode><v_cmp_result>"
  [(set (match_operand:<V_cmp_result> 0 "register_operand" "=w")
	(FIXUORS:<V_cmp_result> (unspec:VCVTF
			       [(match_operand:VCVTF 1 "register_operand" "w")]
			       NEON_VCVT)))]
  "TARGET_NEON && TARGET_VFP5"
  "vcvt<nvrint_variant>.<su>32.f32\\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_to_int_<V_elem_ch><q>")
   (set_attr "predicable" "no")]
)

(define_insn "ior<mode>3_neon"
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
  [(set_attr "type" "neon_logic<q>")]
)

;; The concrete forms of the Neon immediate-logic instructions are vbic and
;; vorr. We support the pseudo-instruction vand instead, because that
;; corresponds to the canonical form the middle-end expects to use for
;; immediate bitwise-ANDs.

(define_insn "and<mode>3_neon"
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
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "orn<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(ior:VDQ (not:VDQ (match_operand:VDQ 2 "s_register_operand" "w"))
		 (match_operand:VDQ 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vorn\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "bic<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(and:VDQ (not:VDQ (match_operand:VDQ 2 "s_register_operand" "w"))
		 (match_operand:VDQ 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vbic\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "xor<mode>3_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(xor:VDQ (match_operand:VDQ 1 "s_register_operand" "w")
		 (match_operand:VDQ 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "veor\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_logic<q>")]
)

(define_insn "one_cmpl<mode>2_neon"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
        (not:VDQ (match_operand:VDQ 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmvn\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_move<q>")]
)

(define_insn "neon_abs<mode>2"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(abs:VDQW (match_operand:VDQW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_abs_s<q>")
                    (const_string "neon_abs<q>")))]
)

(define_insn "neon_neg<mode>2"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(neg:VDQW (match_operand:VDQW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vneg.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_neg_s<q>")
                    (const_string "neon_neg<q>")))]
)

(define_insn "neon_<absneg_str><mode>2"
  [(set (match_operand:VH 0 "s_register_operand" "=w")
    (ABSNEG:VH (match_operand:VH 1 "s_register_operand" "w")))]
 "TARGET_NEON_FP16INST"
 "v<absneg_str>.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
 [(set_attr "type" "neon_abs<q>")]
)

(define_expand "neon_v<absneg_str><mode>"
 [(set
   (match_operand:VH 0 "s_register_operand")
   (ABSNEG:VH (match_operand:VH 1 "s_register_operand")))]
 "TARGET_NEON_FP16INST"
{
  emit_insn (gen_neon_<absneg_str><mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_v<fp16_rnd_str><mode>"
  [(set (match_operand:VH 0 "s_register_operand" "=w")
    (unspec:VH
     [(match_operand:VH 1 "s_register_operand" "w")]
     FP16_RND))]
 "TARGET_NEON_FP16INST"
 "<fp16_rnd_insn>.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
 [(set_attr "type" "neon_fp_round_s<q>")]
)

(define_insn "neon_vrsqrte<mode>"
  [(set (match_operand:VH 0 "s_register_operand" "=w")
    (unspec:VH
     [(match_operand:VH 1 "s_register_operand" "w")]
     UNSPEC_VRSQRTE))]
  "TARGET_NEON_FP16INST"
  "vrsqrte.f16\t%<V_reg>0, %<V_reg>1"
 [(set_attr "type" "neon_fp_rsqrte_s<q>")]
)

(define_insn "*umin<mode>3_neon"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(umin:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		    (match_operand:VDQIW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmin.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_minmax<q>")]
)

(define_insn "*umax<mode>3_neon"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(umax:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		    (match_operand:VDQIW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmax.<V_u_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_minmax<q>")]
)

(define_insn "*smin<mode>3_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(smin:VDQW (match_operand:VDQW 1 "s_register_operand" "w")
		   (match_operand:VDQW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmin.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_minmax_s<q>")
                    (const_string "neon_minmax<q>")))]
)

(define_insn "*smax<mode>3_neon"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(smax:VDQW (match_operand:VDQW 1 "s_register_operand" "w")
		   (match_operand:VDQW 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vmax.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_minmax_s<q>")
                    (const_string "neon_minmax<q>")))]
)

; TODO: V2DI shifts are current disabled because there are bugs in the
; generic vectorizer code.  It ends up creating a V2DI constructor with
; SImode elements.

(define_insn "vashr<mode>3_imm"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(ashiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
			(match_operand:VDQIW 2 "imm_for_neon_rshift_operand" "Dm")))]
  "TARGET_NEON"
  {
    return neon_output_shift_immediate ("vshr", 's', &operands[2],
					<MODE>mode, VALID_NEON_QREG_MODE (<MODE>mode),
					false);
  }
  [(set_attr "type" "neon_shift_imm<q>")]
)

(define_insn "vlshr<mode>3_imm"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(lshiftrt:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
			(match_operand:VDQIW 2 "imm_for_neon_rshift_operand" "Dm")))]
  "TARGET_NEON"
  {
    return neon_output_shift_immediate ("vshr", 'u', &operands[2],
					<MODE>mode, VALID_NEON_QREG_MODE (<MODE>mode),
					false);
  }              
  [(set_attr "type" "neon_shift_imm<q>")]
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
  [(set_attr "type" "neon_shift_reg<q>")]
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
  [(set_attr "type" "neon_shift_reg<q>")]
)

;; 64-bit shifts

;; This pattern loads a 32-bit shift count into a 64-bit NEON register,
;; leaving the upper half uninitalized.  This is OK since the shift
;; instruction only looks at the low 8 bits anyway.  To avoid confusing
;; data flow analysis however, we pretend the full register is set
;; using an unspec.
(define_insn "neon_load_count"
  [(set (match_operand:DI 0 "s_register_operand" "=w,w")
        (unspec:DI [(match_operand:SI 1 "nonimmediate_operand" "Um,r")]
                   UNSPEC_LOAD_COUNT))]
  "TARGET_NEON"
  "@
   vld1.32\t{%P0[0]}, %A1
   vmov.32\t%P0[0], %1"
  [(set_attr "type" "neon_load1_1reg,neon_from_gp")]
)

;; Widening operations

(define_expand "widen_ssum<mode>3"
  [(set (match_operand:<V_double_width> 0 "s_register_operand")
	(plus:<V_double_width>
	 (sign_extend:<V_double_width>
	  (match_operand:VQI 1 "s_register_operand"))
	 (match_operand:<V_double_width> 2 "s_register_operand")))]
  "TARGET_NEON"
  {
    machine_mode mode = GET_MODE (operands[1]);
    rtx p1, p2;

    p1  = arm_simd_vect_par_cnst_half (mode, false);
    p2  = arm_simd_vect_par_cnst_half (mode, true);

    if (operands[0] != operands[2])
      emit_move_insn (operands[0], operands[2]);

    emit_insn (gen_vec_sel_widen_ssum_lo<mode><V_half>3 (operands[0],
							 operands[1],
							 p1,
							 operands[0]));
    emit_insn (gen_vec_sel_widen_ssum_hi<mode><V_half>3 (operands[0],
							 operands[1],
							 p2,
							 operands[0]));
    DONE;
  }
)

(define_insn "vec_sel_widen_ssum_lo<mode><V_half>3"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(plus:<V_double_width>
	 (sign_extend:<V_double_width>
	  (vec_select:<V_HALF>
	   (match_operand:VQI 1 "s_register_operand" "%w")
	   (match_operand:VQI 2 "vect_par_constant_low" "")))
	 (match_operand:<V_double_width> 3 "s_register_operand" "0")))]
  "TARGET_NEON"
{
  return BYTES_BIG_ENDIAN ?  "vaddw.<V_s_elem>\t%q0, %q3, %f1" :
    "vaddw.<V_s_elem>\t%q0, %q3, %e1";
}
  [(set_attr "type" "neon_add_widen")])

(define_insn "vec_sel_widen_ssum_hi<mode><V_half>3"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(plus:<V_double_width>
	 (sign_extend:<V_double_width>
	  (vec_select:<V_HALF>
			 (match_operand:VQI 1 "s_register_operand" "%w")
			 (match_operand:VQI 2 "vect_par_constant_high" "")))
	 (match_operand:<V_double_width> 3 "s_register_operand" "0")))]
  "TARGET_NEON"
{
  return BYTES_BIG_ENDIAN ?  "vaddw.<V_s_elem>\t%q0, %q3, %e1" :
    "vaddw.<V_s_elem>\t%q0, %q3, %f1";
}
  [(set_attr "type" "neon_add_widen")])

(define_insn "widen_ssum<mode>3"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(plus:<V_widen>
	 (sign_extend:<V_widen>
	  (match_operand:VW 1 "s_register_operand" "%w"))
	 (match_operand:<V_widen> 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vaddw.<V_s_elem>\t%q0, %q2, %P1"
  [(set_attr "type" "neon_add_widen")]
)

(define_expand "widen_usum<mode>3"
  [(set (match_operand:<V_double_width> 0 "s_register_operand")
	(plus:<V_double_width>
	 (zero_extend:<V_double_width>
	  (match_operand:VQI 1 "s_register_operand"))
	 (match_operand:<V_double_width> 2 "s_register_operand")))]
  "TARGET_NEON"
  {
    machine_mode mode = GET_MODE (operands[1]);
    rtx p1, p2;

    p1  = arm_simd_vect_par_cnst_half (mode, false);
    p2  = arm_simd_vect_par_cnst_half (mode, true);

    if (operands[0] != operands[2])
      emit_move_insn (operands[0], operands[2]);

    emit_insn (gen_vec_sel_widen_usum_lo<mode><V_half>3 (operands[0],
							 operands[1],
							 p1,
							 operands[0]));
    emit_insn (gen_vec_sel_widen_usum_hi<mode><V_half>3 (operands[0],
							 operands[1],
							 p2,
							 operands[0]));
    DONE;
  }
)

(define_insn "vec_sel_widen_usum_lo<mode><V_half>3"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(plus:<V_double_width>
	 (zero_extend:<V_double_width>
	  (vec_select:<V_HALF>
	   (match_operand:VQI 1 "s_register_operand" "%w")
	   (match_operand:VQI 2 "vect_par_constant_low" "")))
	 (match_operand:<V_double_width> 3 "s_register_operand" "0")))]
  "TARGET_NEON"
{
  return BYTES_BIG_ENDIAN ?  "vaddw.<V_u_elem>\t%q0, %q3, %f1" :
    "vaddw.<V_u_elem>\t%q0, %q3, %e1";
}
  [(set_attr "type" "neon_add_widen")])

(define_insn "vec_sel_widen_usum_hi<mode><V_half>3"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(plus:<V_double_width>
	 (zero_extend:<V_double_width>
	  (vec_select:<V_HALF>
			 (match_operand:VQI 1 "s_register_operand" "%w")
			 (match_operand:VQI 2 "vect_par_constant_high" "")))
	 (match_operand:<V_double_width> 3 "s_register_operand" "0")))]
  "TARGET_NEON"
{
 return BYTES_BIG_ENDIAN ?  "vaddw.<V_u_elem>\t%q0, %q3, %e1" :
    "vaddw.<V_u_elem>\t%q0, %q3, %f1";
}
  [(set_attr "type" "neon_add_widen")])

(define_insn "widen_usum<mode>3"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(plus:<V_widen> (zero_extend:<V_widen>
			  (match_operand:VW 1 "s_register_operand" "%w"))
		        (match_operand:<V_widen> 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vaddw.<V_u_elem>\t%q0, %q2, %P1"
  [(set_attr "type" "neon_add_widen")]
)

;; Helpers for quad-word reduction operations

; Add (or smin, smax...) the low N/2 elements of the N-element vector
; operand[1] to the high N/2 elements of same. Put the result in operand[0], an
; N/2-element vector.

(define_insn "quad_halves_<code>v4si"
  [(set (match_operand:V2SI 0 "s_register_operand" "=w")
        (VQH_OPS:V2SI
          (vec_select:V2SI (match_operand:V4SI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)]))
          (vec_select:V2SI (match_dup 1)
                           (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>32\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set_attr "type" "neon_reduc_<VQH_type>_q")]
)

(define_insn "quad_halves_<code>v4sf"
  [(set (match_operand:V2SF 0 "s_register_operand" "=w")
        (VQHS_OPS:V2SF
          (vec_select:V2SF (match_operand:V4SF 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)]))
          (vec_select:V2SF (match_dup 1)
                           (parallel [(const_int 2) (const_int 3)]))))]
  "ARM_HAVE_NEON_V4SF_ARITH"
  "<VQH_mnem>.f32\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set_attr "type" "neon_fp_reduc_<VQH_type>_s_q")]
)

(define_insn "quad_halves_<code>v8hi"
  [(set (match_operand:V4HI 0 "s_register_operand" "+w")
        (VQH_OPS:V4HI
          (vec_select:V4HI (match_operand:V8HI 1 "s_register_operand" "w")
                           (parallel [(const_int 0) (const_int 1)
				      (const_int 2) (const_int 3)]))
          (vec_select:V4HI (match_dup 1)
                           (parallel [(const_int 4) (const_int 5)
				      (const_int 6) (const_int 7)]))))]
  "TARGET_NEON"
  "<VQH_mnem>.<VQH_sign>16\t%P0, %e1, %f1"
  [(set_attr "vqh_mnem" "<VQH_mnem>")
   (set_attr "type" "neon_reduc_<VQH_type>_q")]
)

(define_insn "quad_halves_<code>v16qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "+w")
        (VQH_OPS:V8QI
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
   (set_attr "type" "neon_reduc_<VQH_type>_q")]
)

(define_expand "move_hi_quad_<mode>"
 [(match_operand:ANY128 0 "s_register_operand")
  (match_operand:<V_HALF> 1 "s_register_operand")]
 "TARGET_NEON"
{
  emit_move_insn (simplify_gen_subreg (<V_HALF>mode, operands[0], <MODE>mode,
				       GET_MODE_SIZE (<V_HALF>mode)),
		  operands[1]);
  DONE;
})

(define_expand "move_lo_quad_<mode>"
 [(match_operand:ANY128 0 "s_register_operand")
  (match_operand:<V_HALF> 1 "s_register_operand")]
 "TARGET_NEON"
{
  emit_move_insn (simplify_gen_subreg (<V_HALF>mode, operands[0],
				       <MODE>mode, 0),
		  operands[1]);
  DONE;
})

;; Reduction operations

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VD 1 "s_register_operand")]
  "ARM_HAVE_NEON_<MODE>_ARITH"
{
  rtx vec = gen_reg_rtx (<MODE>mode);
  neon_pairwise_reduce (vec, operands[1], <MODE>mode,
			&gen_neon_vpadd_internal<mode>);
  /* The same result is actually computed into every element.  */
  emit_insn (gen_vec_extract<mode><V_elem_l> (operands[0], vec, const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_v2di"
  [(match_operand:DI 0 "nonimmediate_operand")
   (match_operand:V2DI 1 "s_register_operand")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx vec = gen_reg_rtx (V2DImode);

  emit_insn (gen_arm_reduc_plus_internal_v2di (vec, operands[1]));
  emit_insn (gen_vec_extractv2didi (operands[0], vec, const0_rtx));

  DONE;
})

(define_insn "arm_reduc_plus_internal_v2di"
  [(set (match_operand:V2DI 0 "s_register_operand" "=w")
	(unspec:V2DI [(match_operand:V2DI 1 "s_register_operand" "w")]
		     UNSPEC_VPADD))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vadd.i64\t%e0, %e1, %f1"
  [(set_attr "type" "neon_add_q")]
)

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VD 1 "s_register_operand")]
  "ARM_HAVE_NEON_<MODE>_ARITH"
{
  rtx vec = gen_reg_rtx (<MODE>mode);

  neon_pairwise_reduce (vec, operands[1], <MODE>mode,
			&gen_neon_vpsmin<mode>);
  /* The result is computed into every element of the vector.  */
  emit_insn (gen_vec_extract<mode><V_elem_l> (operands[0], vec, const0_rtx));
  DONE;
})

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VQ 1 "s_register_operand")]
  "ARM_HAVE_NEON_<MODE>_ARITH && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_smin<mode> (step1, operands[1]));
  emit_insn (gen_reduc_smin_scal_<V_half> (operands[0], step1));

  DONE;
})

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VD 1 "s_register_operand")]
  "ARM_HAVE_NEON_<MODE>_ARITH"
{
  rtx vec = gen_reg_rtx (<MODE>mode);
  neon_pairwise_reduce (vec, operands[1], <MODE>mode,
			&gen_neon_vpsmax<mode>);
  /* The result is computed into every element of the vector.  */
  emit_insn (gen_vec_extract<mode><V_elem_l> (operands[0], vec, const0_rtx));
  DONE;
})

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VQ 1 "s_register_operand")]
  "ARM_HAVE_NEON_<MODE>_ARITH && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_smax<mode> (step1, operands[1]));
  emit_insn (gen_reduc_smax_scal_<V_half> (operands[0], step1));

  DONE;
})

(define_expand "reduc_umin_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VDI 1 "s_register_operand")]
  "TARGET_NEON"
{
  rtx vec = gen_reg_rtx (<MODE>mode);
  neon_pairwise_reduce (vec, operands[1], <MODE>mode,
			&gen_neon_vpumin<mode>);
  /* The result is computed into every element of the vector.  */
  emit_insn (gen_vec_extract<mode><V_elem_l> (operands[0], vec, const0_rtx));
  DONE;
})

(define_expand "reduc_umin_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VQI 1 "s_register_operand")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_umin<mode> (step1, operands[1]));
  emit_insn (gen_reduc_umin_scal_<V_half> (operands[0], step1));

  DONE;
})

(define_expand "reduc_umax_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VDI 1 "s_register_operand")]
  "TARGET_NEON"
{
  rtx vec = gen_reg_rtx (<MODE>mode);
  neon_pairwise_reduce (vec, operands[1], <MODE>mode,
			&gen_neon_vpumax<mode>);
  /* The result is computed into every element of the vector.  */
  emit_insn (gen_vec_extract<mode><V_elem_l> (operands[0], vec, const0_rtx));
  DONE;
})

(define_expand "reduc_umax_scal_<mode>"
  [(match_operand:<V_elem> 0 "nonimmediate_operand")
   (match_operand:VQI 1 "s_register_operand")]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
{
  rtx step1 = gen_reg_rtx (<V_HALF>mode);

  emit_insn (gen_quad_halves_umax<mode> (step1, operands[1]));
  emit_insn (gen_reduc_umax_scal_<V_half> (operands[0], step1));

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
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_reduc_add_s<q>")
                    (const_string "neon_reduc_add<q>")))]
)

(define_insn "neon_vpaddv4hf"
 [(set
   (match_operand:V4HF 0 "s_register_operand" "=w")
   (unspec:V4HF [(match_operand:V4HF 1 "s_register_operand" "w")
		 (match_operand:V4HF 2 "s_register_operand" "w")]
    UNSPEC_VPADD))]
 "TARGET_NEON_FP16INST"
 "vpadd.f16\t%P0, %P1, %P2"
 [(set_attr "type" "neon_reduc_add")]
)

(define_insn "neon_vpsmin<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPSMIN))]
  "TARGET_NEON"
  "vpmin.<V_s_elem>\t%P0, %P1, %P2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_reduc_minmax_s<q>")
                    (const_string "neon_reduc_minmax<q>")))]
)

(define_insn "neon_vpsmax<mode>"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
	(unspec:VD [(match_operand:VD 1 "s_register_operand" "w")
		    (match_operand:VD 2 "s_register_operand" "w")]
                   UNSPEC_VPSMAX))]
  "TARGET_NEON"
  "vpmax.<V_s_elem>\t%P0, %P1, %P2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_reduc_minmax_s<q>")
                    (const_string "neon_reduc_minmax<q>")))]
)

(define_insn "neon_vpumin<mode>"
  [(set (match_operand:VDI 0 "s_register_operand" "=w")
	(unspec:VDI [(match_operand:VDI 1 "s_register_operand" "w")
		     (match_operand:VDI 2 "s_register_operand" "w")]
                   UNSPEC_VPUMIN))]
  "TARGET_NEON"
  "vpmin.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "type" "neon_reduc_minmax<q>")]
)

(define_insn "neon_vpumax<mode>"
  [(set (match_operand:VDI 0 "s_register_operand" "=w")
	(unspec:VDI [(match_operand:VDI 1 "s_register_operand" "w")
		     (match_operand:VDI 2 "s_register_operand" "w")]
                   UNSPEC_VPUMAX))]
  "TARGET_NEON"
  "vpmax.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "type" "neon_reduc_minmax<q>")]
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
  [(set_attr "type" "neon_qadd<q>")]
)

(define_insn "*us_add<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (us_plus:VD (match_operand:VD 1 "s_register_operand" "w")
                   (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqadd.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "type" "neon_qadd<q>")]
)

(define_insn "*ss_sub<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (ss_minus:VD (match_operand:VD 1 "s_register_operand" "w")
                    (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqsub.<V_s_elem>\t%P0, %P1, %P2"
  [(set_attr "type" "neon_qsub<q>")]
)

(define_insn "*us_sub<mode>_neon"
  [(set (match_operand:VD 0 "s_register_operand" "=w")
       (us_minus:VD (match_operand:VD 1 "s_register_operand" "w")
                    (match_operand:VD 2 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vqsub.<V_u_elem>\t%P0, %P1, %P2"
  [(set_attr "type" "neon_qsub<q>")]
)

(define_expand "vec_cmp<mode><v_cmp_result>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand")
	(match_operator:<V_cmp_result> 1 "comparison_operator"
	  [(match_operand:VDQWH 2 "s_register_operand")
	   (match_operand:VDQWH 3 "reg_or_zero_operand")]))]
  "TARGET_NEON
   && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
{
  arm_expand_vector_compare (operands[0], GET_CODE (operands[1]),
			     operands[2], operands[3], false);
  DONE;
})

(define_expand "vec_cmpu<mode><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand")
	(match_operator:VDQIW 1 "comparison_operator"
	  [(match_operand:VDQIW 2 "s_register_operand")
	   (match_operand:VDQIW 3 "reg_or_zero_operand")]))]
  "TARGET_NEON"
{
  arm_expand_vector_compare (operands[0], GET_CODE (operands[1]),
			     operands[2], operands[3], false);
  DONE;
})

(define_expand "vcond_mask_<mode><v_cmp_result>"
  [(set (match_operand:VDQWH 0 "s_register_operand")
	(if_then_else:VDQWH
	  (match_operand:<V_cmp_result> 3 "s_register_operand")
	  (match_operand:VDQWH 1 "s_register_operand")
	  (match_operand:VDQWH 2 "s_register_operand")))]
  "TARGET_NEON
   && (!<Is_float_mode> || flag_unsafe_math_optimizations)"
{
  emit_insn (gen_neon_vbsl<mode> (operands[0], operands[3], operands[1],
				  operands[2]));
  DONE;
})

;; Patterns for builtins.

; good for plain vadd, vaddq.

(define_expand "neon_vadd<mode>"
  [(match_operand:VCVTF 0 "s_register_operand")
   (match_operand:VCVTF 1 "s_register_operand")
   (match_operand:VCVTF 2 "s_register_operand")]
  "TARGET_NEON"
{
  if (ARM_HAVE_NEON_<MODE>_ARITH)
    emit_insn (gen_add<mode>3 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vadd<mode>_unspec (operands[0], operands[1],
					   operands[2]));
  DONE;
})

(define_expand "neon_vadd<mode>"
  [(match_operand:VH 0 "s_register_operand")
   (match_operand:VH 1 "s_register_operand")
   (match_operand:VH 2 "s_register_operand")]
  "TARGET_NEON_FP16INST"
{
  emit_insn (gen_add<mode>3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "neon_vsub<mode>"
  [(match_operand:VH 0 "s_register_operand")
   (match_operand:VH 1 "s_register_operand")
   (match_operand:VH 2 "s_register_operand")]
  "TARGET_NEON_FP16INST"
{
  emit_insn (gen_sub<mode>3 (operands[0], operands[1], operands[2]));
  DONE;
})

; Note that NEON operations don't support the full IEEE 754 standard: in
; particular, denormal values are flushed to zero.  This means that GCC cannot
; use those instructions for autovectorization, etc. unless
; -funsafe-math-optimizations is in effect (in which case flush-to-zero
; behavior is permissible).  Intrinsic operations (provided by the arm_neon.h
; header) must work in either case: if -funsafe-math-optimizations is given,
; intrinsics expand to "canonical" RTL where possible, otherwise intrinsics
; expand to unspecs (which may potentially limit the extent to which they might
; be optimized by generic code).

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vadd<mode>_unspec"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		      (match_operand:VCVTF 2 "s_register_operand" "w")]
                     UNSPEC_VADD))]
  "TARGET_NEON"
  "vadd.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_addsub_s<q>")
                    (const_string "neon_add<q>")))]
)

(define_insn "neon_vaddl<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VDI 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")]
                          VADDL))]
  "TARGET_NEON"
  "vaddl.<sup>%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "type" "neon_add_long")]
)

(define_insn "neon_vaddw<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")]
                          VADDW))]
  "TARGET_NEON"
  "vaddw.<sup>%#<V_sz_elem>\t%q0, %q1, %P2"
  [(set_attr "type" "neon_add_widen")]
)

; vhadd and vrhadd.

(define_insn "@neon_v<r>hadd<sup><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")]
		      VHADD))]
  "TARGET_NEON"
  "v<r>hadd.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_add_halve_q")]
)

(define_insn "neon_vqadd<sup><mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
        (unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")]
                     VQADD))]
  "TARGET_NEON"
  "vqadd.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_qadd<q>")]
)

(define_insn "neon_v<r>addhn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
        (unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
		            (match_operand:VN 2 "s_register_operand" "w")]
                           VADDHN))]
  "TARGET_NEON"
  "v<r>addhn.<V_if_elem>\t%P0, %q1, %q2"
  [(set_attr "type" "neon_add_halve_narrow_q")]
)

;; Polynomial and Float multiplication.
(define_insn "neon_vmul<pf><mode>"
  [(set (match_operand:VPF 0 "s_register_operand" "=w")
        (unspec:VPF [(match_operand:VPF 1 "s_register_operand" "w")
		      (match_operand:VPF 2 "s_register_operand" "w")]
		     UNSPEC_VMUL))]
  "TARGET_NEON"
  "vmul.<pf>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_mul_s<q>")
                    (const_string "neon_mul_<V_elem_ch><q>")))]
)

(define_insn "neon_vmulf<mode>"
 [(set
   (match_operand:VH 0 "s_register_operand" "=w")
   (mult:VH
    (match_operand:VH 1 "s_register_operand" "w")
    (match_operand:VH 2 "s_register_operand" "w")))]
  "TARGET_NEON_FP16INST"
  "vmul.f16\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_mul_<VH_elem_ch><q>")]
)

(define_expand "neon_vmla<mode>"
  [(match_operand:VDQW 0 "s_register_operand")
   (match_operand:VDQW 1 "s_register_operand")
   (match_operand:VDQW 2 "s_register_operand")
   (match_operand:VDQW 3 "s_register_operand")]
  "TARGET_NEON"
{
  if (ARM_HAVE_NEON_<MODE>_ARITH)
    emit_insn (gen_mul<mode>3add<mode>_neon (operands[0], operands[1],
				             operands[2], operands[3]));
  else
    emit_insn (gen_neon_vmla<mode>_unspec (operands[0], operands[1],
					   operands[2], operands[3]));
  DONE;
})

(define_expand "neon_vfma<VCVTF:mode>"
  [(match_operand:VCVTF 0 "s_register_operand")
   (match_operand:VCVTF 1 "s_register_operand")
   (match_operand:VCVTF 2 "s_register_operand")
   (match_operand:VCVTF 3 "s_register_operand")]
  "TARGET_NEON && TARGET_FMA"
{
  emit_insn (gen_fma<mode>4_intrinsic (operands[0], operands[2], operands[3],
				       operands[1]));
  DONE;
})

(define_expand "neon_vfma<VH:mode>"
  [(match_operand:VH 0 "s_register_operand")
   (match_operand:VH 1 "s_register_operand")
   (match_operand:VH 2 "s_register_operand")
   (match_operand:VH 3 "s_register_operand")]
  "TARGET_NEON_FP16INST"
{
  emit_insn (gen_fma<mode>4 (operands[0], operands[2], operands[3],
			     operands[1]));
  DONE;
})

(define_expand "neon_vfms<VCVTF:mode>"
  [(match_operand:VCVTF 0 "s_register_operand")
   (match_operand:VCVTF 1 "s_register_operand")
   (match_operand:VCVTF 2 "s_register_operand")
   (match_operand:VCVTF 3 "s_register_operand")]
  "TARGET_NEON && TARGET_FMA"
{
  emit_insn (gen_fmsub<mode>4_intrinsic (operands[0], operands[2], operands[3],
					 operands[1]));
  DONE;
})

(define_expand "neon_vfms<VH:mode>"
  [(match_operand:VH 0 "s_register_operand")
   (match_operand:VH 1 "s_register_operand")
   (match_operand:VH 2 "s_register_operand")
   (match_operand:VH 3 "s_register_operand")]
  "TARGET_NEON_FP16INST"
{
  emit_insn (gen_fmsub<mode>4_intrinsic (operands[0], operands[2], operands[3],
					 operands[1]));
  DONE;
})

;; The expand RTL structure here is not important.
;; We use the gen_* functions anyway.
;; We just need something to wrap the iterators around.

(define_expand "neon_vfm<vfml_op>l_<vfml_half><mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand")
     (unspec:VCVTF
	[(match_operand:VCVTF 1 "s_register_operand")
	   (PLUSMINUS:<VFML>
	     (match_operand:<VFML> 2 "s_register_operand")
	     (match_operand:<VFML> 3 "s_register_operand"))] VFMLHALVES))]
  "TARGET_FP16FML"
{
  rtx half = arm_simd_vect_par_cnst_half (<VFML>mode, <vfml_half_selector>);
  emit_insn (gen_vfm<vfml_op>l_<vfml_half><mode>_intrinsic (operands[0],
							     operands[1],
							     operands[2],
							     operands[3],
							     half, half));
  DONE;
})

(define_insn "vfmal_low<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 4 "vect_par_constant_low" "")))
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 3 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 5 "vect_par_constant_low" "")))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 "vfmal.f16\\t%<V_reg>0, %<V_lo>2, %<V_lo>3"
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "vfmsl_high<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (neg:<VFMLSEL>
	    (vec_select:<VFMLSEL>
	      (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	      (match_operand:<VFML> 4 "vect_par_constant_high" ""))))
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 3 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 5 "vect_par_constant_high" "")))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 "vfmsl.f16\\t%<V_reg>0, %<V_hi>2, %<V_hi>3"
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "vfmal_high<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 4 "vect_par_constant_high" "")))
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 3 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 5 "vect_par_constant_high" "")))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 "vfmal.f16\\t%<V_reg>0, %<V_hi>2, %<V_hi>3"
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "vfmsl_low<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (neg:<VFMLSEL>
	    (vec_select:<VFMLSEL>
	      (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	      (match_operand:<VFML> 4 "vect_par_constant_low" ""))))
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 3 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 5 "vect_par_constant_low" "")))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 "vfmsl.f16\\t%<V_reg>0, %<V_lo>2, %<V_lo>3"
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_expand "neon_vfm<vfml_op>l_lane_<vfml_half><VCVTF:mode>"
  [(set:VCVTF (match_operand:VCVTF 0 "s_register_operand")
     (unspec:VCVTF
	[(match_operand:VCVTF 1 "s_register_operand")
	 (PLUSMINUS:<VFML>
	   (match_operand:<VFML> 2 "s_register_operand")
	   (match_operand:<VFML> 3 "s_register_operand"))
	 (match_operand:SI 4 "const_int_operand")] VFMLHALVES))]
  "TARGET_FP16FML"
{
  rtx lane = GEN_INT (NEON_ENDIAN_LANE_N (<VFML>mode, INTVAL (operands[4])));
  rtx half = arm_simd_vect_par_cnst_half (<VFML>mode, <vfml_half_selector>);
  emit_insn (gen_vfm<vfml_op>l_lane_<vfml_half><mode>_intrinsic
					       (operands[0], operands[1],
						operands[2], operands[3],
						half, lane));
  DONE;
})

(define_insn "vfmal_lane_low<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 4 "vect_par_constant_low" "")))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFML> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 {
    int lane = NEON_ENDIAN_LANE_N (<VFML>mode, INTVAL (operands[5]));
    if (lane > GET_MODE_NUNITS (<VFMLSEL>mode) - 1)
      {
	operands[5] = GEN_INT (lane - GET_MODE_NUNITS (<VFMLSEL>mode));
	return "vfmal.f16\\t%<V_reg>0, %<V_lo>2, %<V_hi>3[%c5]";
      }
    else
      {
	operands[5] = GEN_INT (lane);
	return "vfmal.f16\\t%<V_reg>0, %<V_lo>2, %<V_lo>3[%c5]";
      }
  }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_expand "neon_vfm<vfml_op>l_lane_<vfml_half><vfmlsel2><mode>"
  [(set:VCVTF (match_operand:VCVTF 0 "s_register_operand")
     (unspec:VCVTF
	[(match_operand:VCVTF 1 "s_register_operand")
	 (PLUSMINUS:<VFML>
	   (match_operand:<VFML> 2 "s_register_operand")
	   (match_operand:<VFMLSEL2> 3 "s_register_operand"))
	 (match_operand:SI 4 "const_int_operand")] VFMLHALVES))]
  "TARGET_FP16FML"
{
  rtx lane
    = GEN_INT (NEON_ENDIAN_LANE_N (<VFMLSEL2>mode, INTVAL (operands[4])));
  rtx half = arm_simd_vect_par_cnst_half (<VFML>mode, <vfml_half_selector>);
  emit_insn (gen_vfm<vfml_op>l_lane_<vfml_half><vfmlsel2><mode>_intrinsic
		(operands[0], operands[1], operands[2], operands[3],
		 half, lane));
  DONE;
})

;; Used to implement the intrinsics:
;; float32x4_t vfmlalq_lane_low_f16 (float32x4_t r, float16x8_t a, float16x4_t b, const int lane)
;; float32x2_t vfmlal_laneq_low_f16 (float32x2_t r, float16x4_t a, float16x8_t b, const int lane)
;; Needs a bit of care to get the modes of the different sub-expressions right
;; due to 'a' and 'b' having different sizes and make sure we use the right
;; S or D subregister to select the appropriate lane from.

(define_insn "vfmal_lane_low<vfmlsel2><mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 4 "vect_par_constant_low" "")))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFMLSEL2> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 {
   int lane = NEON_ENDIAN_LANE_N (<VFMLSEL2>mode, INTVAL (operands[5]));
   int elts_per_reg = GET_MODE_NUNITS (<VFMLSEL>mode);
   int new_lane = lane % elts_per_reg;
   int regdiff = lane / elts_per_reg;
   operands[5] = GEN_INT (new_lane);
   /* We re-create operands[2] and operands[3] in the halved VFMLSEL modes
      because we want the print_operand code to print the appropriate
      S or D register prefix.  */
   operands[3] = gen_rtx_REG (<VFMLSEL>mode, REGNO (operands[3]) + regdiff);
   operands[2] = gen_rtx_REG (<VFMLSEL>mode, REGNO (operands[2]));
   return "vfmal.f16\\t%<V_reg>0, %<V_lane_reg>2, %<V_lane_reg>3[%c5]";
 }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

;; Used to implement the intrinsics:
;; float32x4_t vfmlalq_lane_high_f16 (float32x4_t r, float16x8_t a, float16x4_t b, const int lane)
;; float32x2_t vfmlal_laneq_high_f16 (float32x2_t r, float16x4_t a, float16x8_t b, const int lane)
;; Needs a bit of care to get the modes of the different sub-expressions right
;; due to 'a' and 'b' having different sizes and make sure we use the right
;; S or D subregister to select the appropriate lane from.

(define_insn "vfmal_lane_high<vfmlsel2><mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 4 "vect_par_constant_high" "")))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFMLSEL2> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 {
   int lane = NEON_ENDIAN_LANE_N (<VFMLSEL2>mode, INTVAL (operands[5]));
   int elts_per_reg = GET_MODE_NUNITS (<VFMLSEL>mode);
   int new_lane = lane % elts_per_reg;
   int regdiff = lane / elts_per_reg;
   operands[5] = GEN_INT (new_lane);
   /* We re-create operands[3] in the halved VFMLSEL mode
      because we've calculated the correct half-width subreg to extract
      the lane from and we want to print *that* subreg instead.  */
   operands[3] = gen_rtx_REG (<VFMLSEL>mode, REGNO (operands[3]) + regdiff);
   return "vfmal.f16\\t%<V_reg>0, %<V_hi>2, %<V_lane_reg>3[%c5]";
 }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "vfmal_lane_high<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (vec_select:<VFMLSEL>
	   (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	   (match_operand:<VFML> 4 "vect_par_constant_high" "")))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFML> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
  {
    int lane = NEON_ENDIAN_LANE_N (<VFML>mode, INTVAL (operands[5]));
    if (lane > GET_MODE_NUNITS (<VFMLSEL>mode) - 1)
      {
	operands[5] = GEN_INT (lane - GET_MODE_NUNITS (<VFMLSEL>mode));
	return "vfmal.f16\\t%<V_reg>0, %<V_hi>2, %<V_hi>3[%c5]";
      }
    else
      {
	operands[5] = GEN_INT (lane);
	return "vfmal.f16\\t%<V_reg>0, %<V_hi>2, %<V_lo>3[%c5]";
      }
  }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "vfmsl_lane_low<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (neg:<VFMLSEL>
	    (vec_select:<VFMLSEL>
	      (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	      (match_operand:<VFML> 4 "vect_par_constant_low" ""))))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFML> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 {
    int lane = NEON_ENDIAN_LANE_N (<VFML>mode, INTVAL (operands[5]));
    if (lane > GET_MODE_NUNITS (<VFMLSEL>mode) - 1)
      {
	operands[5] = GEN_INT (lane - GET_MODE_NUNITS (<VFMLSEL>mode));
	return "vfmsl.f16\\t%<V_reg>0, %<V_lo>2, %<V_hi>3[%c5]";
      }
    else
      {
	operands[5] = GEN_INT (lane);
	return "vfmsl.f16\\t%<V_reg>0, %<V_lo>2, %<V_lo>3[%c5]";
      }
  }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

;; Used to implement the intrinsics:
;; float32x4_t vfmlslq_lane_low_f16 (float32x4_t r, float16x8_t a, float16x4_t b, const int lane)
;; float32x2_t vfmlsl_laneq_low_f16 (float32x2_t r, float16x4_t a, float16x8_t b, const int lane)
;; Needs a bit of care to get the modes of the different sub-expressions right
;; due to 'a' and 'b' having different sizes and make sure we use the right
;; S or D subregister to select the appropriate lane from.

(define_insn "vfmsl_lane_low<vfmlsel2><mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (neg:<VFMLSEL>
	    (vec_select:<VFMLSEL>
	      (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	      (match_operand:<VFML> 4 "vect_par_constant_low" ""))))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFMLSEL2> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 {
   int lane = NEON_ENDIAN_LANE_N (<VFMLSEL2>mode, INTVAL (operands[5]));
   int elts_per_reg = GET_MODE_NUNITS (<VFMLSEL>mode);
   int new_lane = lane % elts_per_reg;
   int regdiff = lane / elts_per_reg;
   operands[5] = GEN_INT (new_lane);
   /* We re-create operands[2] and operands[3] in the halved VFMLSEL modes
      because we want the print_operand code to print the appropriate
      S or D register prefix.  */
   operands[3] = gen_rtx_REG (<VFMLSEL>mode, REGNO (operands[3]) + regdiff);
   operands[2] = gen_rtx_REG (<VFMLSEL>mode, REGNO (operands[2]));
   return "vfmsl.f16\\t%<V_reg>0, %<V_lane_reg>2, %<V_lane_reg>3[%c5]";
 }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

;; Used to implement the intrinsics:
;; float32x4_t vfmlslq_lane_high_f16 (float32x4_t r, float16x8_t a, float16x4_t b, const int lane)
;; float32x2_t vfmlsl_laneq_high_f16 (float32x2_t r, float16x4_t a, float16x8_t b, const int lane)
;; Needs a bit of care to get the modes of the different sub-expressions right
;; due to 'a' and 'b' having different sizes and make sure we use the right
;; S or D subregister to select the appropriate lane from.

(define_insn "vfmsl_lane_high<vfmlsel2><mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (neg:<VFMLSEL>
	    (vec_select:<VFMLSEL>
	     (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	     (match_operand:<VFML> 4 "vect_par_constant_high" ""))))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFMLSEL2> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
 {
   int lane = NEON_ENDIAN_LANE_N (<VFMLSEL2>mode, INTVAL (operands[5]));
   int elts_per_reg = GET_MODE_NUNITS (<VFMLSEL>mode);
   int new_lane = lane % elts_per_reg;
   int regdiff = lane / elts_per_reg;
   operands[5] = GEN_INT (new_lane);
   /* We re-create operands[3] in the halved VFMLSEL mode
      because we've calculated the correct half-width subreg to extract
      the lane from and we want to print *that* subreg instead.  */
   operands[3] = gen_rtx_REG (<VFMLSEL>mode, REGNO (operands[3]) + regdiff);
   return "vfmsl.f16\\t%<V_reg>0, %<V_hi>2, %<V_lane_reg>3[%c5]";
 }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

(define_insn "vfmsl_lane_high<mode>_intrinsic"
 [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(fma:VCVTF
	 (float_extend:VCVTF
	  (neg:<VFMLSEL>
	    (vec_select:<VFMLSEL>
	     (match_operand:<VFML> 2 "s_register_operand" "<VF_constraint>")
	     (match_operand:<VFML> 4 "vect_par_constant_high" ""))))
	 (float_extend:VCVTF
	   (vec_duplicate:<VFMLSEL>
	     (vec_select:HF
	       (match_operand:<VFML> 3 "s_register_operand" "x")
	       (parallel [(match_operand:SI 5 "const_int_operand" "n")]))))
	 (match_operand:VCVTF 1 "s_register_operand" "0")))]
 "TARGET_FP16FML"
  {
    int lane = NEON_ENDIAN_LANE_N (<VFML>mode, INTVAL (operands[5]));
    if (lane > GET_MODE_NUNITS (<VFMLSEL>mode) - 1)
      {
	operands[5] = GEN_INT (lane - GET_MODE_NUNITS (<VFMLSEL>mode));
	return "vfmsl.f16\\t%<V_reg>0, %<V_hi>2, %<V_hi>3[%c5]";
      }
    else
      {
	operands[5] = GEN_INT (lane);
	return "vfmsl.f16\\t%<V_reg>0, %<V_hi>2, %<V_lo>3[%c5]";
      }
  }
 [(set_attr "type" "neon_fp_mla_s<q>")]
)

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vmla<mode>_unspec"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:VDQW 3 "s_register_operand" "w")]
		    UNSPEC_VMLA))]
  "TARGET_NEON"
  "vmla.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_mla_s<q>")
                    (const_string "neon_mla_<V_elem_ch><q>")))]
)

(define_insn "neon_vmlal<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VW 2 "s_register_operand" "w")
		           (match_operand:VW 3 "s_register_operand" "w")]
                          VMLAL))]
  "TARGET_NEON"
  "vmlal.<sup>%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set_attr "type" "neon_mla_<V_elem_ch>_long")]
)

(define_expand "neon_vmls<mode>"
  [(match_operand:VDQW 0 "s_register_operand")
   (match_operand:VDQW 1 "s_register_operand")
   (match_operand:VDQW 2 "s_register_operand")
   (match_operand:VDQW 3 "s_register_operand")]
  "TARGET_NEON"
{
  if (ARM_HAVE_NEON_<MODE>_ARITH)
    emit_insn (gen_mul<mode>3neg<mode>add<mode>_neon (operands[0],
		 operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_neon_vmls<mode>_unspec (operands[0], operands[1],
					   operands[2], operands[3]));
  DONE;
})

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vmls<mode>_unspec"
  [(set (match_operand:VDQW 0 "s_register_operand" "=w")
	(unspec:VDQW [(match_operand:VDQW 1 "s_register_operand" "0")
		      (match_operand:VDQW 2 "s_register_operand" "w")
		      (match_operand:VDQW 3 "s_register_operand" "w")]
		    UNSPEC_VMLS))]
  "TARGET_NEON"
  "vmls.<V_if_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_mla_s<q>")
                    (const_string "neon_mla_<V_elem_ch><q>")))]
)

(define_insn "neon_vmlsl<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VW 2 "s_register_operand" "w")
		           (match_operand:VW 3 "s_register_operand" "w")]
                          VMLSL))]
  "TARGET_NEON"
  "vmlsl.<sup>%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set_attr "type" "neon_mla_<V_elem_ch>_long")]
)

;; vqdmulh, vqrdmulh
(define_insn "neon_vq<r>dmulh<mode>"
  [(set (match_operand:VMDQI 0 "s_register_operand" "=w")
        (unspec:VMDQI [(match_operand:VMDQI 1 "s_register_operand" "w")
		       (match_operand:VMDQI 2 "s_register_operand" "w")]
                      VQDMULH))]
  "TARGET_NEON"
  "vq<r>dmulh.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_sat_mul_<V_elem_ch><q>")]
)

;; vqrdmlah, vqrdmlsh
(define_insn "neon_vqrdml<VQRDMLH_AS:neon_rdma_as>h<mode>"
  [(set (match_operand:VMDQI 0 "s_register_operand" "=w")
	(unspec:VMDQI [(match_operand:VMDQI 1 "s_register_operand" "0")
		       (match_operand:VMDQI 2 "s_register_operand" "w")
		       (match_operand:VMDQI 3 "s_register_operand" "w")]
		      VQRDMLH_AS))]
  "TARGET_NEON_RDMA"
  "vqrdml<VQRDMLH_AS:neon_rdma_as>h.<V_s_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set_attr "type" "neon_sat_mla_<V_elem_ch>_long")]
)

(define_insn "neon_vqdmlal<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VMDI 2 "s_register_operand" "w")
		           (match_operand:VMDI 3 "s_register_operand" "w")]
                          UNSPEC_VQDMLAL))]
  "TARGET_NEON"
  "vqdmlal.<V_s_elem>\t%q0, %P2, %P3"
  [(set_attr "type" "neon_sat_mla_<V_elem_ch>_long")]
)

(define_insn "neon_vqdmlsl<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
		           (match_operand:VMDI 2 "s_register_operand" "w")
		           (match_operand:VMDI 3 "s_register_operand" "w")]
                          UNSPEC_VQDMLSL))]
  "TARGET_NEON"
  "vqdmlsl.<V_s_elem>\t%q0, %P2, %P3"
  [(set_attr "type" "neon_sat_mla_<V_elem_ch>_long")]
)

(define_insn "neon_vmull<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
		           (match_operand:VW 2 "s_register_operand" "w")]
                          VMULL))]
  "TARGET_NEON"
  "vmull.<sup>%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "type" "neon_mul_<V_elem_ch>_long")]
)

(define_insn "neon_vqdmull<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand" "w")]
                          UNSPEC_VQDMULL))]
  "TARGET_NEON"
  "vqdmull.<V_s_elem>\t%q0, %P1, %P2"
  [(set_attr "type" "neon_sat_mul_<V_elem_ch>_long")]
)

(define_expand "neon_vsub<mode>"
  [(match_operand:VCVTF 0 "s_register_operand")
   (match_operand:VCVTF 1 "s_register_operand")
   (match_operand:VCVTF 2 "s_register_operand")]
  "TARGET_NEON"
{
  if (ARM_HAVE_NEON_<MODE>_ARITH)
    emit_insn (gen_sub<mode>3 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vsub<mode>_unspec (operands[0], operands[1],
					   operands[2]));
  DONE;
})

; Used for intrinsics when flag_unsafe_math_optimizations is false.

(define_insn "neon_vsub<mode>_unspec"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		      (match_operand:VCVTF 2 "s_register_operand" "w")]
                     UNSPEC_VSUB))]
  "TARGET_NEON"
  "vsub.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set (attr "type")
      (if_then_else (match_test "<Is_float_mode>")
                    (const_string "neon_fp_addsub_s<q>")
                    (const_string "neon_sub<q>")))]
)

(define_insn "neon_vsubl<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VDI 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")]
                          VSUBL))]
  "TARGET_NEON"
  "vsubl.<sup>%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "type" "neon_sub_long")]
)

(define_insn "neon_vsubw<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "w")
		           (match_operand:VDI 2 "s_register_operand" "w")]
			  VSUBW))]
  "TARGET_NEON"
  "vsubw.<sup>%#<V_sz_elem>\t%q0, %q1, %P2"
  [(set_attr "type" "neon_sub_widen")]
)

(define_insn "neon_vqsub<sup><mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
        (unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")]
		      VQSUB))]
  "TARGET_NEON"
  "vqsub.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_qsub<q>")]
)

(define_insn "neon_vhsub<sup><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w")]
		      VHSUB))]
  "TARGET_NEON"
  "vhsub.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_sub_halve<q>")]
)

(define_insn "neon_v<r>subhn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
        (unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
		            (match_operand:VN 2 "s_register_operand" "w")]
                           VSUBHN))]
  "TARGET_NEON"
  "v<r>subhn.<V_if_elem>\t%P0, %q1, %q2"
  [(set_attr "type" "neon_sub_halve_narrow_q")]
)

;; These may expand to an UNSPEC pattern when a floating point mode is used
;; without unsafe math optimizations.
(define_expand "@neon_vc<cmp_op><mode>"
  [(match_operand:<V_cmp_result> 0 "s_register_operand")
     (neg:<V_cmp_result>
       (COMPARISONS:VDQW (match_operand:VDQW 1 "s_register_operand")
                         (match_operand:VDQW 2 "reg_or_zero_operand")))]
  "TARGET_NEON"
  {
    /* For FP comparisons use UNSPECS unless -funsafe-math-optimizations
       are enabled.  */
    if (GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_FLOAT
        && !flag_unsafe_math_optimizations)
      {
        /* We don't just emit a gen_neon_vc<cmp_op><mode>_insn_unspec because
           we define gen_neon_vceq<mode>_insn_unspec only for float modes
           whereas this expander iterates over the integer modes as well,
           but we will never expand to UNSPECs for the integer comparisons.  */
        switch (<MODE>mode)
          {
            case E_V2SFmode:
              emit_insn (gen_neon_vc<cmp_op>v2sf_insn_unspec (operands[0],
                                                              operands[1],
                                                              operands[2]));
              break;
            case E_V4SFmode:
              emit_insn (gen_neon_vc<cmp_op>v4sf_insn_unspec (operands[0],
                                                              operands[1],
                                                              operands[2]));
              break;
            default:
              gcc_unreachable ();
          }
      }
    else
      emit_insn (gen_neon_vc<cmp_op><mode>_insn (operands[0],
                                                 operands[1],
                                                 operands[2]));
    DONE;
  }
)

(define_insn "@neon_vc<cmp_op><mode>_insn"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w,w")
        (neg:<V_cmp_result>
          (COMPARISONS:<V_cmp_result>
            (match_operand:VDQW 1 "s_register_operand" "w,w")
            (match_operand:VDQW 2 "reg_or_zero_operand" "w,Dz"))))]
  "TARGET_NEON && !(GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_FLOAT
                    && !flag_unsafe_math_optimizations)"
  {
    char pattern[100];
    sprintf (pattern, "vc<cmp_op>.%s%%#<V_sz_elem>\t%%<V_reg>0,"
                      " %%<V_reg>1, %s",
                       GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_FLOAT
                         ? "f" : "<cmp_type>",
                       which_alternative == 0
                         ? "%<V_reg>2" : "#0");
    output_asm_insn (pattern, operands);
    return "";
  }
  [(set (attr "type")
        (if_then_else (match_operand 2 "zero_operand")
                      (const_string "neon_compare_zero<q>")
                      (const_string "neon_compare<q>")))]
)

(define_insn "neon_vc<cmp_op_unsp><mode>_insn_unspec"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w,w")
        (unspec:<V_cmp_result>
	  [(match_operand:VCVTF 1 "s_register_operand" "w,w")
	   (match_operand:VCVTF 2 "reg_or_zero_operand" "w,Dz")]
          NEON_VCMP))]
  "TARGET_NEON"
  {
    char pattern[100];
    sprintf (pattern, "vc<cmp_op_unsp>.f%%#<V_sz_elem>\t%%<V_reg>0,"
                       " %%<V_reg>1, %s",
                       which_alternative == 0
                         ? "%<V_reg>2" : "#0");
    output_asm_insn (pattern, operands);
    return "";
}
  [(set_attr "type" "neon_fp_compare_s<q>")]
)

(define_expand "@neon_vc<cmp_op><mode>"
 [(match_operand:<V_cmp_result> 0 "s_register_operand")
  (neg:<V_cmp_result>
   (COMPARISONS:VH
    (match_operand:VH 1 "s_register_operand")
    (match_operand:VH 2 "reg_or_zero_operand")))]
 "TARGET_NEON_FP16INST"
{
  /* For FP comparisons use UNSPECS unless -funsafe-math-optimizations
     are enabled.  */
  if (GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_FLOAT
      && !flag_unsafe_math_optimizations)
    emit_insn
      (gen_neon_vc<cmp_op><mode>_fp16insn_unspec
       (operands[0], operands[1], operands[2]));
  else
    emit_insn
      (gen_neon_vc<cmp_op><mode>_fp16insn
       (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "neon_vc<cmp_op><mode>_fp16insn"
 [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w,w")
   (neg:<V_cmp_result>
    (COMPARISONS:<V_cmp_result>
     (match_operand:VH 1 "s_register_operand" "w,w")
     (match_operand:VH 2 "reg_or_zero_operand" "w,Dz"))))]
 "TARGET_NEON_FP16INST
  && !(GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_FLOAT
  && !flag_unsafe_math_optimizations)"
{
  char pattern[100];
  sprintf (pattern, "vc<cmp_op>.%s%%#<V_sz_elem>\t%%<V_reg>0,"
	   " %%<V_reg>1, %s",
	   GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_FLOAT
	   ? "f" : "<cmp_type>",
	   which_alternative == 0
	   ? "%<V_reg>2" : "#0");
  output_asm_insn (pattern, operands);
  return "";
}
 [(set (attr "type")
   (if_then_else (match_operand 2 "zero_operand")
    (const_string "neon_compare_zero<q>")
    (const_string "neon_compare<q>")))])

(define_insn "neon_vc<cmp_op_unsp><mode>_fp16insn_unspec"
 [(set
   (match_operand:<V_cmp_result> 0 "s_register_operand" "=w,w")
   (unspec:<V_cmp_result>
    [(match_operand:VH 1 "s_register_operand" "w,w")
     (match_operand:VH 2 "reg_or_zero_operand" "w,Dz")]
    NEON_VCMP))]
 "TARGET_NEON_FP16INST"
{
  char pattern[100];
  sprintf (pattern, "vc<cmp_op_unsp>.f%%#<V_sz_elem>\t%%<V_reg>0,"
	   " %%<V_reg>1, %s",
	   which_alternative == 0
	   ? "%<V_reg>2" : "#0");
  output_asm_insn (pattern, operands);
  return "";
}
 [(set_attr "type" "neon_fp_compare_s<q>")])

(define_insn "@neon_vc<code><mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (neg:<V_cmp_result>
          (GTUGEU:<V_cmp_result>
	    (match_operand:VDQIW 1 "s_register_operand" "w")
	    (match_operand:VDQIW 2 "s_register_operand" "w"))))]
  "TARGET_NEON"
  "vc<cmp_op>.u%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_compare<q>")]
)

(define_expand "neon_vca<cmp_op><mode>"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand")
        (neg:<V_cmp_result>
          (GLTE:<V_cmp_result>
            (abs:VCVTF (match_operand:VCVTF 1 "s_register_operand"))
            (abs:VCVTF (match_operand:VCVTF 2 "s_register_operand")))))]
  "TARGET_NEON"
  {
    if (flag_unsafe_math_optimizations)
      emit_insn (gen_neon_vca<cmp_op><mode>_insn (operands[0], operands[1],
                                                  operands[2]));
    else
      emit_insn (gen_neon_vca<cmp_op><mode>_insn_unspec (operands[0],
                                                         operands[1],
                                                         operands[2]));
    DONE;
  }
)

(define_insn "neon_vca<cmp_op><mode>_insn"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (neg:<V_cmp_result>
          (GLTE:<V_cmp_result>
            (abs:VCVTF (match_operand:VCVTF 1 "s_register_operand" "w"))
            (abs:VCVTF (match_operand:VCVTF 2 "s_register_operand" "w")))))]
  "TARGET_NEON && flag_unsafe_math_optimizations"
  "vac<cmp_op>.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_compare_s<q>")]
)

(define_insn "neon_vca<cmp_op_unsp><mode>_insn_unspec"
  [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
        (unspec:<V_cmp_result> [(match_operand:VCVTF 1 "s_register_operand" "w")
		                (match_operand:VCVTF 2 "s_register_operand" "w")]
                               NEON_VAGLTE))]
  "TARGET_NEON"
  "vac<cmp_op_unsp>.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_compare_s<q>")]
)

(define_expand "neon_vca<cmp_op><mode>"
  [(set
    (match_operand:<V_cmp_result> 0 "s_register_operand")
    (neg:<V_cmp_result>
     (GLTE:<V_cmp_result>
      (abs:VH (match_operand:VH 1 "s_register_operand"))
      (abs:VH (match_operand:VH 2 "s_register_operand")))))]
 "TARGET_NEON_FP16INST"
{
  if (flag_unsafe_math_optimizations)
    emit_insn (gen_neon_vca<cmp_op><mode>_fp16insn
	       (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_neon_vca<cmp_op><mode>_fp16insn_unspec
	       (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "neon_vca<cmp_op><mode>_fp16insn"
  [(set
    (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
    (neg:<V_cmp_result>
     (GLTE:<V_cmp_result>
      (abs:VH (match_operand:VH 1 "s_register_operand" "w"))
      (abs:VH (match_operand:VH 2 "s_register_operand" "w")))))]
 "TARGET_NEON_FP16INST && flag_unsafe_math_optimizations"
 "vac<cmp_op>.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_compare_s<q>")]
)

(define_insn "neon_vca<cmp_op_unsp><mode>_fp16insn_unspec"
 [(set (match_operand:<V_cmp_result> 0 "s_register_operand" "=w")
   (unspec:<V_cmp_result>
    [(match_operand:VH 1 "s_register_operand" "w")
     (match_operand:VH 2 "s_register_operand" "w")]
    NEON_VAGLTE))]
 "TARGET_NEON"
 "vac<cmp_op_unsp>.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_compare_s<q>")]
)

(define_expand "neon_vc<cmp_op>z<mode>"
 [(set
   (match_operand:<V_cmp_result> 0 "s_register_operand")
   (COMPARISONS:<V_cmp_result>
    (match_operand:VH 1 "s_register_operand")
    (const_int 0)))]
 "TARGET_NEON_FP16INST"
 {
  emit_insn (gen_neon_vc<cmp_op><mode> (operands[0], operands[1],
					CONST0_RTX (<MODE>mode)));
  DONE;
})

(define_insn "neon_vtst_combine<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (plus:VDQIW
	  (eq:VDQIW
	    (and:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")
		       (match_operand:VDQIW 2 "s_register_operand" "w"))
	    (match_operand:VDQIW 3 "zero_operand" "i"))
	  (match_operand:VDQIW 4 "minus_one_operand" "i")))]
  "TARGET_NEON"
  "vtst.<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_tst<q>")]
)

(define_insn "neon_vabd<sup><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		      (match_operand:VDQIW 2 "s_register_operand" "w")]
		     VABD))]
  "TARGET_NEON"
  "vabd.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_abd<q>")]
)

(define_insn "neon_vabd<mode>"
  [(set (match_operand:VH 0 "s_register_operand" "=w")
    (unspec:VH [(match_operand:VH 1 "s_register_operand" "w")
		(match_operand:VH 2 "s_register_operand" "w")]
     UNSPEC_VABD_F))]
 "TARGET_NEON_FP16INST"
 "vabd.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_abd<q>")]
)

(define_insn "neon_vabdf<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		      (match_operand:VCVTF 2 "s_register_operand" "w")]
		     UNSPEC_VABD_F))]
  "TARGET_NEON"
  "vabd.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_abd_s<q>")]
)

(define_insn "neon_vabdl<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
		           (match_operand:VW 2 "s_register_operand" "w")]
                          VABDL))]
  "TARGET_NEON"
  "vabdl.<sup>%#<V_sz_elem>\t%q0, %P1, %P2"
  [(set_attr "type" "neon_abd_long")]
)

(define_insn "neon_vaba<sup><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (plus:VDQIW (unspec:VDQIW [(match_operand:VDQIW 2 "s_register_operand" "w")
		                   (match_operand:VDQIW 3 "s_register_operand" "w")]
		                  VABD)
		    (match_operand:VDQIW 1 "s_register_operand" "0")))]
  "TARGET_NEON"
  "vaba.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set_attr "type" "neon_arith_acc<q>")]
)

(define_insn "neon_vabal<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
        (plus:<V_widen> (unspec:<V_widen> [(match_operand:VW 2 "s_register_operand" "w")
                                           (match_operand:VW 3 "s_register_operand" "w")]
					   VABDL)
			 (match_operand:<V_widen> 1 "s_register_operand" "0")))]
  "TARGET_NEON"
  "vabal.<sup>%#<V_sz_elem>\t%q0, %P2, %P3"
  [(set_attr "type" "neon_arith_acc<q>")]
)

(define_expand "<sup>sadv16qi"
  [(use (match_operand:V4SI 0 "register_operand"))
   (unspec:V16QI [(use (match_operand:V16QI 1 "register_operand"))
                  (use (match_operand:V16QI 2 "register_operand"))] VABAL)
   (use (match_operand:V4SI 3 "register_operand"))]
  "TARGET_NEON"
  {
    rtx reduc = gen_reg_rtx (V8HImode);
    rtx op1_highpart = gen_reg_rtx (V8QImode);
    rtx op2_highpart = gen_reg_rtx (V8QImode);

    emit_insn (gen_neon_vabdl<sup>v8qi (reduc,
                                        gen_lowpart (V8QImode, operands[1]),
                                        gen_lowpart (V8QImode, operands[2])));

    emit_insn (gen_neon_vget_highv16qi (op1_highpart, operands[1]));
    emit_insn (gen_neon_vget_highv16qi (op2_highpart, operands[2]));
    emit_insn (gen_neon_vabal<sup>v8qi (reduc, reduc,
                                        op1_highpart, op2_highpart));
    emit_insn (gen_neon_vpadal<sup>v8hi (operands[3], operands[3], reduc));

    emit_move_insn (operands[0], operands[3]);
    DONE;
  }
)

(define_insn "neon_v<maxmin><sup><mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")
		      (match_operand:VDQIW 2 "s_register_operand" "w")]
                     VMAXMIN))]
  "TARGET_NEON"
  "v<maxmin>.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_minmax<q>")]
)

(define_insn "neon_v<maxmin>f<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		      (match_operand:VCVTF 2 "s_register_operand" "w")]
                     VMAXMINF))]
  "TARGET_NEON"
  "v<maxmin>.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_minmax_s<q>")]
)

(define_insn "neon_v<maxmin>f<mode>"
 [(set (match_operand:VH 0 "s_register_operand" "=w")
   (unspec:VH
    [(match_operand:VH 1 "s_register_operand" "w")
     (match_operand:VH 2 "s_register_operand" "w")]
    VMAXMINF))]
 "TARGET_NEON_FP16INST"
 "v<maxmin>.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_minmax_s<q>")]
)

(define_insn "neon_vp<maxmin>fv4hf"
 [(set (match_operand:V4HF 0 "s_register_operand" "=w")
   (unspec:V4HF
    [(match_operand:V4HF 1 "s_register_operand" "w")
     (match_operand:V4HF 2 "s_register_operand" "w")]
    VPMAXMINF))]
 "TARGET_NEON_FP16INST"
 "vp<maxmin>.f16\t%P0, %P1, %P2"
  [(set_attr "type" "neon_reduc_minmax")]
)

(define_insn "neon_<fmaxmin_op><mode>"
 [(set
   (match_operand:VH 0 "s_register_operand" "=w")
   (unspec:VH
    [(match_operand:VH 1 "s_register_operand" "w")
     (match_operand:VH 2 "s_register_operand" "w")]
    VMAXMINFNM))]
 "TARGET_NEON_FP16INST"
 "<fmaxmin_op>.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_minmax_s<q>")]
)

;; v<maxmin>nm intrinsics.
(define_insn "neon_<fmaxmin_op><mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")]
		       VMAXMINFNM))]
  "TARGET_NEON && TARGET_VFP5"
  "<fmaxmin_op>.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_minmax_s<q>")]
)

;; Vector forms for the IEEE-754 fmax()/fmin() functions
(define_insn "<fmaxmin><mode>3"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
	(unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")]
		       VMAXMINFNM))]
  "TARGET_NEON && TARGET_VFP5"
  "<fmaxmin_op>.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_minmax_s<q>")]
)

(define_expand "neon_vpadd<mode>"
  [(match_operand:VD 0 "s_register_operand")
   (match_operand:VD 1 "s_register_operand")
   (match_operand:VD 2 "s_register_operand")]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vpadd_internal<mode> (operands[0], operands[1],
					    operands[2]));
  DONE;
})

(define_insn "neon_vpaddl<sup><mode>"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
        (unspec:<V_double_width> [(match_operand:VDQIW 1 "s_register_operand" "w")]
                                 VPADDL))]
  "TARGET_NEON"
  "vpaddl.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_reduc_add_long")]
)

(define_insn "neon_vpadal<sup><mode>"
  [(set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
        (unspec:<V_double_width> [(match_operand:<V_double_width> 1 "s_register_operand" "0")
                                  (match_operand:VDQIW 2 "s_register_operand" "w")]
                                 VPADAL))]
  "TARGET_NEON"
  "vpadal.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2"
  [(set_attr "type" "neon_reduc_add_acc")]
)

(define_insn "neon_vp<maxmin><sup><mode>"
  [(set (match_operand:VDI 0 "s_register_operand" "=w")
        (unspec:VDI [(match_operand:VDI 1 "s_register_operand" "w")
		    (match_operand:VDI 2 "s_register_operand" "w")]
                   VPMAXMIN))]
  "TARGET_NEON"
  "vp<maxmin>.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_reduc_minmax<q>")]
)

(define_insn "neon_vp<maxmin>f<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		    (match_operand:VCVTF 2 "s_register_operand" "w")]
                   VPMAXMINF))]
  "TARGET_NEON"
  "vp<maxmin>.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_reduc_minmax_s<q>")]
)

(define_insn "neon_vrecps<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")]
                      UNSPEC_VRECPS))]
  "TARGET_NEON"
  "vrecps.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_recps_s<q>")]
)

(define_insn "neon_vrecps<mode>"
  [(set
    (match_operand:VH 0 "s_register_operand" "=w")
    (unspec:VH [(match_operand:VH 1 "s_register_operand" "w")
		(match_operand:VH 2 "s_register_operand" "w")]
     UNSPEC_VRECPS))]
  "TARGET_NEON_FP16INST"
  "vrecps.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_recps_s<q>")]
)

(define_insn "neon_vrsqrts<mode>"
  [(set (match_operand:VCVTF 0 "s_register_operand" "=w")
        (unspec:VCVTF [(match_operand:VCVTF 1 "s_register_operand" "w")
		       (match_operand:VCVTF 2 "s_register_operand" "w")]
                      UNSPEC_VRSQRTS))]
  "TARGET_NEON"
  "vrsqrts.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_fp_rsqrts_s<q>")]
)

(define_insn "neon_vrsqrts<mode>"
  [(set
    (match_operand:VH 0 "s_register_operand" "=w")
    (unspec:VH [(match_operand:VH 1 "s_register_operand" "w")
		 (match_operand:VH 2 "s_register_operand" "w")]
     UNSPEC_VRSQRTS))]
 "TARGET_NEON_FP16INST"
 "vrsqrts.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_rsqrts_s<q>")]
)

(define_expand "neon_vabs<mode>"
  [(match_operand:VDQW 0 "s_register_operand")
   (match_operand:VDQW 1 "s_register_operand")]
  "TARGET_NEON"
{
  emit_insn (gen_abs<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vqabs<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")]
		      UNSPEC_VQABS))]
  "TARGET_NEON"
  "vqabs.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_qabs<q>")]
)

(define_insn "neon_bswap<mode>"
  [(set (match_operand:VDQHSD 0 "register_operand" "=w")
        (bswap:VDQHSD (match_operand:VDQHSD 1 "register_operand" "w")))]
  "TARGET_NEON"
  "vrev<V_sz_elem>.8\\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_rev<q>")]
)

(define_expand "neon_vneg<mode>"
  [(match_operand:VDQW 0 "s_register_operand")
   (match_operand:VDQW 1 "s_register_operand")]
  "TARGET_NEON"
{
  emit_insn (gen_neon_neg<mode>2 (operands[0], operands[1]));
  DONE;
})


;; The vcadd and vcmla patterns are made UNSPEC for the explicitly due to the
;; fact that their usage need to guarantee that the source vectors are
;; contiguous.  It would be wrong to describe the operation without being able
;; to describe the permute that is also required, but even if that is done
;; the permute would have been created as a LOAD_LANES which means the values
;; in the registers are in the wrong order.
(define_insn "neon_vcadd<rot><mode>"
  [(set (match_operand:VF 0 "register_operand" "=w")
	(unspec:VF [(match_operand:VF 1 "register_operand" "w")
		    (match_operand:VF 2 "register_operand" "w")]
		    VCADD))]
  "TARGET_COMPLEX"
  "vcadd.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2, #<rot>"
  [(set_attr "type" "neon_fcadd")]
)

(define_insn "neon_vcmla<rot><mode>"
  [(set (match_operand:VF 0 "register_operand" "=w")
	(plus:VF (match_operand:VF 1 "register_operand" "0")
		 (unspec:VF [(match_operand:VF 2 "register_operand" "w")
			     (match_operand:VF 3 "register_operand" "w")]
			     VCMLA)))]
  "TARGET_COMPLEX"
  "vcmla.<V_s_elem>\t%<V_reg>0, %<V_reg>2, %<V_reg>3, #<rot>"
  [(set_attr "type" "neon_fcmla")]
)

(define_insn "neon_vcmla_lane<rot><mode>"
  [(set (match_operand:VF 0 "s_register_operand" "=w")
	(plus:VF (match_operand:VF 1 "s_register_operand" "0")
		 (unspec:VF [(match_operand:VF 2 "s_register_operand" "w")
			     (match_operand:VF 3 "s_register_operand" "<VF_constraint>")
			     (match_operand:SI 4 "const_int_operand" "n")]
			     VCMLA)))]
  "TARGET_COMPLEX"
  {
    operands = neon_vcmla_lane_prepare_operands (operands);
    return "vcmla.<V_s_elem>\t%<V_reg>0, %<V_reg>2, d%c3[%c4], #<rot>";
  }
  [(set_attr "type" "neon_fcmla")]
)

(define_insn "neon_vcmla_laneq<rot><mode>"
  [(set (match_operand:VDF 0 "s_register_operand" "=w")
	(plus:VDF (match_operand:VDF 1 "s_register_operand" "0")
		  (unspec:VDF [(match_operand:VDF 2 "s_register_operand" "w")
			      (match_operand:<V_DOUBLE> 3 "s_register_operand" "<VF_constraint>")
			      (match_operand:SI 4 "const_int_operand" "n")]
			      VCMLA)))]
  "TARGET_COMPLEX"
  {
    operands = neon_vcmla_lane_prepare_operands (operands);
    return "vcmla.<V_s_elem>\t%<V_reg>0, %<V_reg>2, d%c3[%c4], #<rot>";
  }
  [(set_attr "type" "neon_fcmla")]
)

(define_insn "neon_vcmlaq_lane<rot><mode>"
  [(set (match_operand:VQ_HSF 0 "s_register_operand" "=w")
	(plus:VQ_HSF (match_operand:VQ_HSF 1 "s_register_operand" "0")
		 (unspec:VQ_HSF [(match_operand:VQ_HSF 2 "s_register_operand" "w")
				 (match_operand:<V_HALF> 3 "s_register_operand" "<VF_constraint>")
				 (match_operand:SI 4 "const_int_operand" "n")]
				 VCMLA)))]
  "TARGET_COMPLEX"
  {
    operands = neon_vcmla_lane_prepare_operands (operands);
    return "vcmla.<V_s_elem>\t%<V_reg>0, %<V_reg>2, d%c3[%c4], #<rot>";
  }
  [(set_attr "type" "neon_fcmla")]
)

;; The complex mul operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cmul<conj_op><mode>3"
  [(set (match_operand:VDF 0 "register_operand")
	(unspec:VDF [(match_operand:VDF 1 "register_operand")
		     (match_operand:VDF 2 "register_operand")]
		    VCMUL_OP))]
  "TARGET_COMPLEX && !BYTES_BIG_ENDIAN"
{
  rtx res1 = gen_reg_rtx (<MODE>mode);
  rtx tmp = force_reg (<MODE>mode, CONST0_RTX (<MODE>mode));
  emit_insn (gen_neon_vcmla<rotsplit1><mode> (res1, tmp,
					      operands[2], operands[1]));
  emit_insn (gen_neon_vcmla<rotsplit2><mode> (operands[0], res1,
					      operands[2], operands[1]));
  DONE;
})


;; These map to the auto-vectorizer Dot Product optab.
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
(define_insn "<sup>dot_prod<mode><vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand" "=w")
	(plus:VCVTI
	  (unspec:VCVTI [(match_operand:<VSI2QI> 1 "register_operand" "w")
			 (match_operand:<VSI2QI> 2 "register_operand" "w")]
			 DOTPROD)
	  (match_operand:VCVTI 3 "register_operand" "0")))]
  "TARGET_DOTPROD"
  "v<sup>dot.<opsuffix>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the Dot Product operations
(define_expand "neon_<sup>dot<mode><vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand" "=w")
	(plus:VCVTI
	  (unspec:VCVTI [(match_operand:<VSI2QI> 2 "register_operand")
			 (match_operand:<VSI2QI> 3 "register_operand")]
			 DOTPROD)
	  (match_operand:VCVTI 1 "register_operand")))]
  "TARGET_DOTPROD"
)

;; These instructions map to the __builtins for the Dot Product operations.
(define_insn "neon_usdot<mode><vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand" "=w")
	(plus:VCVTI
	  (unspec:VCVTI
	    [(match_operand:<VSI2QI> 2 "register_operand" "w")
	    (match_operand:<VSI2QI> 3 "register_operand" "w")]
	    UNSPEC_DOT_US)
	  (match_operand:VCVTI 1 "register_operand" "0")))]
  "TARGET_I8MM"
  "vusdot.s8\\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the Dot Product
;; indexed operations.
(define_insn "neon_<sup>dot_lane<vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand" "=w")
	(plus:VCVTI
	  (unspec:VCVTI [(match_operand:<VSI2QI> 2 "register_operand" "w")
			 (match_operand:V8QI 3 "register_operand" "t")
			 (match_operand:SI 4 "immediate_operand" "i")]
			 DOTPROD)
	  (match_operand:VCVTI 1 "register_operand" "0")))]
  "TARGET_DOTPROD"
  "v<sup>dot.<opsuffix>\\t%<V_reg>0, %<V_reg>2, %P3[%c4]";
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the Dot Product
;; indexed operations.
(define_insn "neon_<sup>dot_laneq<vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand" "=w")
	(plus:VCVTI
	  (unspec:VCVTI [(match_operand:<VSI2QI> 2 "register_operand" "w")
			 (match_operand:V16QI 3 "register_operand" "t")
			 (match_operand:SI 4 "immediate_operand" "i")]
			 DOTPROD)
	  (match_operand:VCVTI 1 "register_operand" "0")))]
  "TARGET_DOTPROD"
  {
    int lane = INTVAL (operands[4]);
    if (lane > GET_MODE_NUNITS (V2SImode) - 1)
      {
	operands[4] = GEN_INT (lane - GET_MODE_NUNITS (V2SImode));
	return "v<sup>dot.<opsuffix>\\t%<V_reg>0, %<V_reg>2, %f3[%c4]";
      }
    else
      {
	operands[4] = GEN_INT (lane);
	return "v<sup>dot.<opsuffix>\\t%<V_reg>0, %<V_reg>2, %e3[%c4]";
      }
  }
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the Dot Product
;; indexed operations in the v8.6 I8MM extension.
(define_insn "neon_<sup>dot_lane<vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand" "=w")
	(plus:VCVTI
	  (unspec:VCVTI
	   [(match_operand:<VSI2QI> 2 "register_operand" "w")
	    (match_operand:V8QI 3 "register_operand" "t")
	    (match_operand:SI 4 "immediate_operand" "i")]
	    DOTPROD_I8MM)
	  (match_operand:VCVTI 1 "register_operand" "0")))]
  "TARGET_I8MM"
  "v<sup>dot.<opsuffix>\\t%<V_reg>0, %<V_reg>2, %P3[%c4]"
  [(set_attr "type" "neon_dot<q>")]
)

;; These instructions map to the __builtins for the Dot Product
;; indexed operations in the v8.6 I8MM extension.
(define_insn "neon_<sup>dot_laneq<vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand" "=w")
	(plus:VCVTI
	  (unspec:VCVTI [(match_operand:<VSI2QI> 2 "register_operand" "w")
			 (match_operand:V16QI 3 "register_operand" "t")
			 (match_operand:SI 4 "immediate_operand" "i")]
			 DOTPROD_I8MM)
	  (match_operand:VCVTI 1 "register_operand" "0")))]
  "TARGET_I8MM"
  {
    int lane = INTVAL (operands[4]);
    if (lane > GET_MODE_NUNITS (V2SImode) - 1)
      {
	operands[4] = GEN_INT (lane - GET_MODE_NUNITS (V2SImode));
	return "v<sup>dot.<opsuffix>\\t%<V_reg>0, %<V_reg>2, %f3[%c4]";
      }
    else
      {
	operands[4] = GEN_INT (lane);
	return "v<sup>dot.<opsuffix>\\t%<V_reg>0, %<V_reg>2, %e3[%c4]";
      }
  }
  [(set_attr "type" "neon_dot<q>")]
)

;; Auto-vectorizer pattern for usdot
(define_expand "usdot_prod<mode><vsi2qi>"
  [(set (match_operand:VCVTI 0 "register_operand")
	(plus:VCVTI (unspec:VCVTI [(match_operand:<VSI2QI> 1
							"register_operand")
				   (match_operand:<VSI2QI> 2
							"register_operand")]
		     UNSPEC_DOT_US)
		    (match_operand:VCVTI 3 "register_operand")))]
  "TARGET_I8MM"
)

(define_expand "copysign<mode>3"
  [(match_operand:VCVTF 0 "register_operand")
   (match_operand:VCVTF 1 "register_operand")
   (match_operand:VCVTF 2 "register_operand")]
  "TARGET_NEON"
  "{
     rtx v_bitmask_cast;
     rtx v_bitmask = gen_reg_rtx (<VCVTF:V_cmp_result>mode);
     rtx c = gen_int_mode (0x80000000, SImode);

     emit_move_insn (v_bitmask,
		     gen_const_vec_duplicate (<VCVTF:V_cmp_result>mode, c));
     emit_move_insn (operands[0], operands[2]);
     v_bitmask_cast = simplify_gen_subreg (<MODE>mode, v_bitmask,
					   <VCVTF:V_cmp_result>mode, 0);
     emit_insn (gen_neon_vbsl<mode> (operands[0], v_bitmask_cast, operands[0],
				     operands[1]));

     DONE;
  }"
)

(define_insn "neon_vqneg<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")]
		      UNSPEC_VQNEG))]
  "TARGET_NEON"
  "vqneg.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_qneg<q>")]
)

(define_insn "neon_vcls<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
	(unspec:VDQIW [(match_operand:VDQIW 1 "s_register_operand" "w")]
		      UNSPEC_VCLS))]
  "TARGET_NEON"
  "vcls.<V_s_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_cls<q>")]
)

(define_insn "neon_vclz<mode>"
  [(set (match_operand:VDQIW 0 "s_register_operand" "=w")
        (clz:VDQIW (match_operand:VDQIW 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vclz.<V_if_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_cnt<q>")]
)

(define_insn "popcount<mode>2"
  [(set (match_operand:VE 0 "s_register_operand" "=w")
        (popcount:VE (match_operand:VE 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vcnt.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_cnt<q>")]
)

(define_expand "neon_vcnt<mode>"
  [(match_operand:VE 0 "s_register_operand")
   (match_operand:VE 1 "s_register_operand")]
  "TARGET_NEON"
{
  emit_insn (gen_popcount<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_insn "neon_vrecpe<mode>"
  [(set (match_operand:VH 0 "s_register_operand" "=w")
	(unspec:VH [(match_operand:VH 1 "s_register_operand" "w")]
		   UNSPEC_VRECPE))]
  "TARGET_NEON_FP16INST"
  "vrecpe.f16\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_recpe_s<q>")]
)

(define_insn "neon_vrecpe<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w")
	(unspec:V32 [(match_operand:V32 1 "s_register_operand" "w")]
                    UNSPEC_VRECPE))]
  "TARGET_NEON"
  "vrecpe.<V_u_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_recpe_s<q>")]
)

(define_insn "neon_vrsqrte<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w")
	(unspec:V32 [(match_operand:V32 1 "s_register_operand" "w")]
                    UNSPEC_VRSQRTE))]
  "TARGET_NEON"
  "vrsqrte.<V_u_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_rsqrte_s<q>")]
)

(define_expand "neon_vmvn<mode>"
  [(match_operand:VDQIW 0 "s_register_operand")
   (match_operand:VDQIW 1 "s_register_operand")]
  "TARGET_NEON"
{
  emit_insn (gen_one_cmpl<mode>2_neon (operands[0], operands[1]));
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
  return "vmov.s<V_sz_elem>\t%0, %P1[%c2]";
}
  [(set_attr "type" "neon_to_gp")]
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
  return "vmov.u<V_sz_elem>\t%0, %P1[%c2]";
}
  [(set_attr "type" "neon_to_gp")]
)

(define_insn "neon_vget_lane<mode>_sext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VQ2 1 "s_register_operand" "w")
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
  output_asm_insn ("vmov.s<V_sz_elem>\t%0, %P1[%c2]", ops);

  return "";
}
  [(set_attr "type" "neon_to_gp_q")]
)

(define_insn "neon_vget_lane<mode>_zext_internal"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI
	  (vec_select:<V_elem>
	    (match_operand:VQ2 1 "s_register_operand" "w")
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
  output_asm_insn ("vmov.u<V_sz_elem>\t%0, %P1[%c2]", ops);

  return "";
}
  [(set_attr "type" "neon_to_gp_q")]
)

(define_expand "neon_vget_lane<mode>"
  [(match_operand:<V_ext> 0 "s_register_operand")
   (match_operand:VDQW 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      /* The intrinsics are defined in terms of a model where the
	 element ordering in memory is vldm order, whereas the generic
	 RTL is defined in terms of a model where the element ordering
	 in memory is array order.  Convert the lane number to conform
	 to this model.  */
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts
	= 64 / GET_MODE_UNIT_BITSIZE (<MODE>mode);
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }

  if (GET_MODE_UNIT_BITSIZE (<MODE>mode) == 32)
    emit_insn (gen_vec_extract<mode><V_elem_l> (operands[0], operands[1],
						operands[2]));
  else
    emit_insn (gen_neon_vget_lane<mode>_sext_internal (operands[0],
						       operands[1],
						       operands[2]));
  DONE;
})

(define_expand "neon_vget_laneu<mode>"
  [(match_operand:<V_ext> 0 "s_register_operand")
   (match_operand:VDQIW 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      /* The intrinsics are defined in terms of a model where the
	 element ordering in memory is vldm order, whereas the generic
	 RTL is defined in terms of a model where the element ordering
	 in memory is array order.  Convert the lane number to conform
	 to this model.  */
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts
	= 64 / GET_MODE_UNIT_BITSIZE (<MODE>mode);
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }

  if (GET_MODE_UNIT_BITSIZE (<MODE>mode) == 32)
    emit_insn (gen_vec_extract<mode><V_elem_l> (operands[0], operands[1],
						operands[2]));
  else
    emit_insn (gen_neon_vget_lane<mode>_zext_internal (operands[0],
						       operands[1],
						       operands[2]));
  DONE;
})

(define_expand "neon_vget_lanedi"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON"
{
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vget_lanev2di"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:V2DI 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON"
{
  int lane;

if (BYTES_BIG_ENDIAN)
    {
      /* The intrinsics are defined in terms of a model where the
	 element ordering in memory is vldm order, whereas the generic
	 RTL is defined in terms of a model where the element ordering
	 in memory is array order.  Convert the lane number to conform
	 to this model.  */
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts = 2;
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }

  lane = INTVAL (operands[2]);
  gcc_assert ((lane ==0) || (lane == 1));
  emit_move_insn (operands[0], lane == 0
				? gen_lowpart (DImode, operands[1])
				: gen_highpart (DImode, operands[1]));
  DONE;
})

(define_expand "neon_vset_lane<mode>"
  [(match_operand:VDQ 0 "s_register_operand")
   (match_operand:<V_elem> 1 "s_register_operand")
   (match_operand:VDQ 2 "s_register_operand")
   (match_operand:SI 3 "immediate_operand")]
  "TARGET_NEON"
{
  unsigned int elt = INTVAL (operands[3]);

  if (BYTES_BIG_ENDIAN)
    {
      unsigned int reg_nelts
	= 64 / GET_MODE_UNIT_BITSIZE (<MODE>mode);
      elt ^= reg_nelts - 1;
    }

  emit_insn (gen_vec_set<mode>_internal (operands[0], operands[1],
                                         GEN_INT (1 << elt), operands[2]));
  DONE;
})

; See neon_vget_lanedi comment for reasons operands 2 & 3 are ignored.

(define_expand "neon_vset_lanedi"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "s_register_operand")
   (match_operand:DI 2 "s_register_operand")
   (match_operand:SI 3 "immediate_operand")]
  "TARGET_NEON"
{
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "neon_vcreate<mode>"
  [(match_operand:VD_RE 0 "s_register_operand")
   (match_operand:DI 1 "general_operand")]
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
  "vdup.<V_sz_elem>\t%<V_reg>0, %1"
  [(set_attr "type" "neon_from_gp<q>")]
)

(define_insn "neon_vdup_nv4hf"
  [(set (match_operand:V4HF 0 "s_register_operand" "=w")
        (vec_duplicate:V4HF (match_operand:HF 1 "s_register_operand" "r")))]
  "TARGET_NEON"
  "vdup.16\t%P0, %1"
  [(set_attr "type" "neon_from_gp")]
)

(define_insn "neon_vdup_nv8hf"
  [(set (match_operand:V8HF 0 "s_register_operand" "=w")
        (vec_duplicate:V8HF (match_operand:HF 1 "s_register_operand" "r")))]
  "TARGET_NEON"
  "vdup.16\t%q0, %1"
  [(set_attr "type" "neon_from_gp_q")]
)

(define_insn "neon_vdup_nv4bf"
  [(set (match_operand:V4BF 0 "s_register_operand" "=w")
        (vec_duplicate:V4BF (match_operand:BF 1 "s_register_operand" "r")))]
  "TARGET_NEON"
  "vdup.16\t%P0, %1"
  [(set_attr "type" "neon_from_gp")]
)

(define_insn "neon_vdup_nv8bf"
  [(set (match_operand:V8BF 0 "s_register_operand" "=w")
        (vec_duplicate:V8BF (match_operand:BF 1 "s_register_operand" "r")))]
  "TARGET_NEON"
  "vdup.16\t%q0, %1"
  [(set_attr "type" "neon_from_gp_q")]
)

(define_insn "neon_vdup_n<mode>"
  [(set (match_operand:V32 0 "s_register_operand" "=w,w")
        (vec_duplicate:V32 (match_operand:<V_elem> 1 "s_register_operand" "r,t")))]
  "TARGET_NEON"
  "@
  vdup.<V_sz_elem>\t%<V_reg>0, %1
  vdup.<V_sz_elem>\t%<V_reg>0, %y1"
  [(set_attr "type" "neon_from_gp<q>,neon_dup<q>")]
)

(define_expand "neon_vdup_ndi"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "s_register_operand")]
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
  vmov\t%e0, %Q1, %R1\;vmov\t%f0, %Q1, %R1
  vmov\t%e0, %P1\;vmov\t%f0, %P1"
  [(set_attr "length" "8")
   (set_attr "type" "multiple")]
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
  [(set_attr "type" "neon_dup<q>")]
)

(define_insn "neon_vdup_lane<mode>_internal"
 [(set (match_operand:VHFBF 0 "s_register_operand" "=w")
   (vec_duplicate:VHFBF
    (vec_select:<V_elem>
     (match_operand:<V_double_vector_mode> 1 "s_register_operand" "w")
     (parallel [(match_operand:SI 2 "immediate_operand" "i")]))))]
 "TARGET_NEON && (TARGET_FP16 || TARGET_BF16_SIMD)"
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
  [(set_attr "type" "neon_dup<q>")]
)

(define_expand "neon_vdup_lane<mode>"
  [(match_operand:VDQW 0 "s_register_operand")
   (match_operand:<V_double_vector_mode> 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON"
{
  if (BYTES_BIG_ENDIAN)
    {
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts
	= 64 / GET_MODE_UNIT_BITSIZE (<V_double_vector_mode>mode);
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }
    emit_insn (gen_neon_vdup_lane<mode>_internal (operands[0], operands[1],
                                                  operands[2]));
    DONE;
})

(define_expand "neon_vdup_lane<mode>"
  [(match_operand:VHFBF 0 "s_register_operand")
   (match_operand:<V_double_vector_mode> 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON && (TARGET_FP16 || TARGET_BF16_SIMD)"
{
  if (BYTES_BIG_ENDIAN)
    {
      unsigned int elt = INTVAL (operands[2]);
      unsigned int reg_nelts
	= 64 / GET_MODE_UNIT_BITSIZE (<V_double_vector_mode>mode);
      elt ^= reg_nelts - 1;
      operands[2] = GEN_INT (elt);
    }
  emit_insn (gen_neon_vdup_lane<mode>_internal (operands[0], operands[1],
						operands[2]));
  DONE;
})

; Scalar index is ignored, since only zero is valid here.
(define_expand "neon_vdup_lanedi"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON"
{
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

; Likewise for v2di, as the DImode second operand has only a single element.
(define_expand "neon_vdup_lanev2di"
  [(match_operand:V2DI 0 "s_register_operand")
   (match_operand:DI 1 "s_register_operand")
   (match_operand:SI 2 "immediate_operand")]
  "TARGET_NEON"
{
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
  [(set_attr "type" "neon_permute<q>")]
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
}
[(set_attr "type" "multiple")]
)

(define_expand "neon_vget_high<mode>"
  [(match_operand:<V_HALF> 0 "s_register_operand")
   (match_operand:VQXBF 1 "s_register_operand")]
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
  [(set_attr "type" "neon_int_to_fp_<V_elem_ch><q>")]
)

(define_insn "floatuns<mode><V_cvtto>2"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
        (unsigned_float:<V_CVTTO> (match_operand:VCVTI 1 "s_register_operand" "w")))] 
  "TARGET_NEON && !flag_rounding_math"
  "vcvt.f32.u32\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_int_to_fp_<V_elem_ch><q>")]
)

(define_insn "fix_trunc<mode><V_cvtto>2"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
        (fix:<V_CVTTO> (match_operand:VCVTF 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vcvt.s32.f32\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_to_int_<V_elem_ch><q>")]
)

(define_insn "fixuns_trunc<mode><V_cvtto>2"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
        (unsigned_fix:<V_CVTTO> (match_operand:VCVTF 1 "s_register_operand" "w")))]
  "TARGET_NEON"
  "vcvt.u32.f32\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_to_int_<V_elem_ch><q>")]
)

(define_insn "neon_vcvt<sup><mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTF 1 "s_register_operand" "w")]
			  VCVT_US))]
  "TARGET_NEON"
  "vcvt.<sup>%#32.f32\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_to_int_<V_elem_ch><q>")]
)

(define_insn "neon_vcvt<sup><mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTI 1 "s_register_operand" "w")]
			  VCVT_US))]
  "TARGET_NEON"
  "vcvt.f32.<sup>%#32\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_int_to_fp_<V_elem_ch><q>")]
)

(define_insn "neon_vcvtv4sfv4hf"
  [(set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V4HF 1 "s_register_operand" "w")]
			  UNSPEC_VCVT))]
  "TARGET_NEON && TARGET_FP16"
  "vcvt.f32.f16\t%q0, %P1"
  [(set_attr "type" "neon_fp_cvt_widen_h")]
)

(define_insn "neon_vcvtv4hfv4sf"
  [(set (match_operand:V4HF 0 "s_register_operand" "=w")
	(unspec:V4HF [(match_operand:V4SF 1 "s_register_operand" "w")]
			  UNSPEC_VCVT))]
  "TARGET_NEON && TARGET_FP16"
  "vcvt.f16.f32\t%P0, %q1"
  [(set_attr "type" "neon_fp_cvt_narrow_s_q")]
)

(define_insn "neon_vcvt<sup><mode>"
 [(set
   (match_operand:<VH_CVTTO> 0 "s_register_operand" "=w")
   (unspec:<VH_CVTTO>
    [(match_operand:VCVTHI 1 "s_register_operand" "w")]
    VCVT_US))]
 "TARGET_NEON_FP16INST"
 "vcvt.f16.<sup>%#16\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_int_to_fp_<VH_elem_ch><q>")]
)

(define_insn "neon_vcvt<sup><mode>"
 [(set
   (match_operand:<VH_CVTTO> 0 "s_register_operand" "=w")
   (unspec:<VH_CVTTO>
    [(match_operand:VH 1 "s_register_operand" "w")]
    VCVT_US))]
 "TARGET_NEON_FP16INST"
 "vcvt.<sup>%#16.f16\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_to_int_<VH_elem_ch><q>")]
)

(define_insn "neon_vcvt<sup>_n<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTF 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
			  VCVT_US_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 1, 33);
  return "vcvt.<sup>%#32.f32\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "type" "neon_fp_to_int_<V_elem_ch><q>")]
)

(define_insn "neon_vcvt<sup>_n<mode>"
 [(set (match_operand:<VH_CVTTO> 0 "s_register_operand" "=w")
   (unspec:<VH_CVTTO>
    [(match_operand:VH 1 "s_register_operand" "w")
     (match_operand:SI 2 "immediate_operand" "i")]
    VCVT_US_N))]
  "TARGET_NEON_FP16INST"
{
  arm_const_bounds (operands[2], 0, 17);
  return "vcvt.<sup>%#16.f16\t%<V_reg>0, %<V_reg>1, %2";
}
 [(set_attr "type" "neon_fp_to_int_<VH_elem_ch><q>")]
)

(define_insn "neon_vcvt<sup>_n<mode>"
  [(set (match_operand:<V_CVTTO> 0 "s_register_operand" "=w")
	(unspec:<V_CVTTO> [(match_operand:VCVTI 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
			  VCVT_US_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 1, 33);
  return "vcvt.f32.<sup>%#32\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "type" "neon_int_to_fp_<V_elem_ch><q>")]
)

(define_insn "neon_vcvt<sup>_n<mode>"
 [(set (match_operand:<VH_CVTTO> 0 "s_register_operand" "=w")
   (unspec:<VH_CVTTO>
    [(match_operand:VCVTHI 1 "s_register_operand" "w")
     (match_operand:SI 2 "immediate_operand" "i")]
    VCVT_US_N))]
 "TARGET_NEON_FP16INST"
{
  arm_const_bounds (operands[2], 0, 17);
  return "vcvt.f16.<sup>%#16\t%<V_reg>0, %<V_reg>1, %2";
}
 [(set_attr "type" "neon_int_to_fp_<VH_elem_ch><q>")]
)

(define_insn "neon_vcvt<vcvth_op><sup><mode>"
 [(set
   (match_operand:<VH_CVTTO> 0 "s_register_operand" "=w")
   (unspec:<VH_CVTTO>
    [(match_operand:VH 1 "s_register_operand" "w")]
    VCVT_HF_US))]
 "TARGET_NEON_FP16INST"
 "vcvt<vcvth_op>.<sup>%#16.f16\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_fp_to_int_<VH_elem_ch><q>")]
)

(define_insn "neon_vmovn<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")]
                           UNSPEC_VMOVN))]
  "TARGET_NEON"
  "vmovn.<V_if_elem>\t%P0, %q1"
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

(define_insn "neon_vqmovn<sup><mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")]
                           VQMOVN))]
  "TARGET_NEON"
  "vqmovn.<sup>%#<V_sz_elem>\t%P0, %q1"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "neon_vqmovun<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")]
                           UNSPEC_VQMOVUN))]
  "TARGET_NEON"
  "vqmovun.<V_s_elem>\t%P0, %q1"
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "neon_vmovl<sup><mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")]
                          VMOVL))]
  "TARGET_NEON"
  "vmovl.<sup>%#<V_sz_elem>\t%q0, %P1"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "neon_vmul_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "w")
		     (match_operand:VMD 2 "s_register_operand"
                                        "<scalar_mul_constraint>")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VMUL_LANE))]
  "TARGET_NEON"
{
  return "vmul.<V_if_elem>\t%P0, %P1, %P2[%c3]";
}
  [(set (attr "type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_mul_s_scalar<q>")
                   (const_string "neon_mul_<V_elem_ch>_scalar<q>")))]
)

(define_insn "neon_vmul_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "w")
		     (match_operand:<V_HALF> 2 "s_register_operand"
                                             "<scalar_mul_constraint>")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VMUL_LANE))]
  "TARGET_NEON"
{
  return "vmul.<V_if_elem>\t%q0, %q1, %P2[%c3]";
}
  [(set (attr "type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_mul_s_scalar<q>")
                   (const_string "neon_mul_<V_elem_ch>_scalar<q>")))]
)

(define_insn "neon_vmul_lane<mode>"
  [(set (match_operand:VH 0 "s_register_operand" "=w")
	(unspec:VH [(match_operand:VH 1 "s_register_operand" "w")
		    (match_operand:V4HF 2 "s_register_operand"
		     "<scalar_mul_constraint>")
		     (match_operand:SI 3 "immediate_operand" "i")]
		     UNSPEC_VMUL_LANE))]
  "TARGET_NEON_FP16INST"
  "vmul.f16\t%<V_reg>0, %<V_reg>1, %P2[%c3]"
  [(set_attr "type" "neon_fp_mul_s_scalar<q>")]
)

(define_insn "neon_vmull<sup>_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          VMULL_LANE))]
  "TARGET_NEON"
{
  return "vmull.<sup>%#<V_sz_elem>\t%q0, %P1, %P2[%c3]";
}
  [(set_attr "type" "neon_mul_<V_elem_ch>_scalar_long")]
)

(define_insn "neon_vqdmull_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VMDI 1 "s_register_operand" "w")
		           (match_operand:VMDI 2 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 3 "immediate_operand" "i")]
                          UNSPEC_VQDMULL_LANE))]
  "TARGET_NEON"
{
  return "vqdmull.<V_s_elem>\t%q0, %P1, %P2[%c3]";
}
  [(set_attr "type" "neon_sat_mul_<V_elem_ch>_scalar_long")]
)

(define_insn "neon_vq<r>dmulh_lane<mode>"
  [(set (match_operand:VMQI 0 "s_register_operand" "=w")
	(unspec:VMQI [(match_operand:VMQI 1 "s_register_operand" "w")
		      (match_operand:<V_HALF> 2 "s_register_operand"
					      "<scalar_mul_constraint>")
                      (match_operand:SI 3 "immediate_operand" "i")]
                      VQDMULH_LANE))]
  "TARGET_NEON"
{
  return "vq<r>dmulh.<V_s_elem>\t%q0, %q1, %P2[%c3]";
}
  [(set_attr "type" "neon_sat_mul_<V_elem_ch>_scalar_q")]
)

(define_insn "neon_vq<r>dmulh_lane<mode>"
  [(set (match_operand:VMDI 0 "s_register_operand" "=w")
	(unspec:VMDI [(match_operand:VMDI 1 "s_register_operand" "w")
		      (match_operand:VMDI 2 "s_register_operand"
					  "<scalar_mul_constraint>")
                      (match_operand:SI 3 "immediate_operand" "i")]
                      VQDMULH_LANE))]
  "TARGET_NEON"
{
  return "vq<r>dmulh.<V_s_elem>\t%P0, %P1, %P2[%c3]";
}
  [(set_attr "type" "neon_sat_mul_<V_elem_ch>_scalar_q")]
)

;; vqrdmlah_lane, vqrdmlsh_lane
(define_insn "neon_vqrdml<VQRDMLH_AS:neon_rdma_as>h_lane<mode>"
  [(set (match_operand:VMQI 0 "s_register_operand" "=w")
	(unspec:VMQI [(match_operand:VMQI 1 "s_register_operand" "0")
		      (match_operand:VMQI 2 "s_register_operand" "w")
		      (match_operand:<V_HALF> 3 "s_register_operand"
					  "<scalar_mul_constraint>")
		      (match_operand:SI 4 "immediate_operand" "i")]
		     VQRDMLH_AS))]
  "TARGET_NEON_RDMA"
{
  return
   "vqrdml<VQRDMLH_AS:neon_rdma_as>h.<V_s_elem>\t%q0, %q2, %P3[%c4]";
}
  [(set_attr "type" "neon_mla_<V_elem_ch>_scalar<q>")]
)

(define_insn "neon_vqrdml<VQRDMLH_AS:neon_rdma_as>h_lane<mode>"
  [(set (match_operand:VMDI 0 "s_register_operand" "=w")
	(unspec:VMDI [(match_operand:VMDI 1 "s_register_operand" "0")
		      (match_operand:VMDI 2 "s_register_operand" "w")
		      (match_operand:VMDI 3 "s_register_operand"
					  "<scalar_mul_constraint>")
		      (match_operand:SI 4 "immediate_operand" "i")]
		     VQRDMLH_AS))]
  "TARGET_NEON_RDMA"
{
  return
   "vqrdml<VQRDMLH_AS:neon_rdma_as>h.<V_s_elem>\t%P0, %P2, %P3[%c4]";
}
  [(set_attr "type" "neon_mla_<V_elem_ch>_scalar")]
)

(define_insn "neon_vmla_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "0")
		     (match_operand:VMD 2 "s_register_operand" "w")
                     (match_operand:VMD 3 "s_register_operand"
					"<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")]
                     UNSPEC_VMLA_LANE))]
  "TARGET_NEON"
{
  return "vmla.<V_if_elem>\t%P0, %P2, %P3[%c4]";
}
  [(set (attr "type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_mla_s_scalar<q>")
                   (const_string "neon_mla_<V_elem_ch>_scalar<q>")))]
)

(define_insn "neon_vmla_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "0")
		     (match_operand:VMQ 2 "s_register_operand" "w")
                     (match_operand:<V_HALF> 3 "s_register_operand"
					     "<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")]
                     UNSPEC_VMLA_LANE))]
  "TARGET_NEON"
{
  return "vmla.<V_if_elem>\t%q0, %q2, %P3[%c4]";
}
  [(set (attr "type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_mla_s_scalar<q>")
                   (const_string "neon_mla_<V_elem_ch>_scalar<q>")))]
)

(define_insn "neon_vmlal<sup>_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          VMLAL_LANE))]
  "TARGET_NEON"
{
  return "vmlal.<sup>%#<V_sz_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set_attr "type" "neon_mla_<V_elem_ch>_scalar_long")]
)

(define_insn "neon_vqdmlal_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMLAL_LANE))]
  "TARGET_NEON"
{
  return "vqdmlal.<V_s_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set_attr "type" "neon_sat_mla_<V_elem_ch>_scalar_long")]
)

(define_insn "neon_vmls_lane<mode>"
  [(set (match_operand:VMD 0 "s_register_operand" "=w")
	(unspec:VMD [(match_operand:VMD 1 "s_register_operand" "0")
		     (match_operand:VMD 2 "s_register_operand" "w")
                     (match_operand:VMD 3 "s_register_operand"
					"<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMLS_LANE))]
  "TARGET_NEON"
{
  return "vmls.<V_if_elem>\t%P0, %P2, %P3[%c4]";
}
  [(set (attr "type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_mla_s_scalar<q>")
                   (const_string "neon_mla_<V_elem_ch>_scalar<q>")))]
)

(define_insn "neon_vmls_lane<mode>"
  [(set (match_operand:VMQ 0 "s_register_operand" "=w")
	(unspec:VMQ [(match_operand:VMQ 1 "s_register_operand" "0")
		     (match_operand:VMQ 2 "s_register_operand" "w")
                     (match_operand:<V_HALF> 3 "s_register_operand"
					     "<scalar_mul_constraint>")
                     (match_operand:SI 4 "immediate_operand" "i")]
                    UNSPEC_VMLS_LANE))]
  "TARGET_NEON"
{
  return "vmls.<V_if_elem>\t%q0, %q2, %P3[%c4]";
}
  [(set (attr "type")
     (if_then_else (match_test "<Is_float_mode>")
                   (const_string "neon_fp_mla_s_scalar<q>")
                   (const_string "neon_mla_<V_elem_ch>_scalar<q>")))]
)

(define_insn "neon_vmlsl<sup>_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          VMLSL_LANE))]
  "TARGET_NEON"
{
  return "vmlsl.<sup>%#<V_sz_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set_attr "type" "neon_mla_<V_elem_ch>_scalar_long")]
)

(define_insn "neon_vqdmlsl_lane<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:<V_widen> 1 "s_register_operand" "0")
			   (match_operand:VMDI 2 "s_register_operand" "w")
                           (match_operand:VMDI 3 "s_register_operand"
					       "<scalar_mul_constraint>")
                           (match_operand:SI 4 "immediate_operand" "i")]
                          UNSPEC_VQDMLSL_LANE))]
  "TARGET_NEON"
{
  return "vqdmlsl.<V_s_elem>\t%q0, %P2, %P3[%c4]";
}
  [(set_attr "type" "neon_sat_mla_<V_elem_ch>_scalar_long")]
)

; FIXME: For the "_n" multiply/multiply-accumulate insns, we copy a value in a
; core register into a temp register, then use a scalar taken from that. This
; isn't an optimal solution if e.g. the scalar has just been read from memory
; or extracted from another vector. The latter case it's currently better to
; use the "_lane" variant, and the former case can probably be implemented
; using vld1_lane, but that hasn't been done yet.

(define_expand "neon_vmul_n<mode>"
  [(match_operand:VMD 0 "s_register_operand")
   (match_operand:VMD 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmul_lane<mode> (operands[0], operands[1], tmp,
				       const0_rtx));
  DONE;
})

(define_expand "neon_vmul_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand")
   (match_operand:VMQ 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmul_lane<mode> (operands[0], operands[1], tmp,
				       const0_rtx));
  DONE;
})

(define_expand "neon_vmul_n<mode>"
  [(match_operand:VH 0 "s_register_operand")
   (match_operand:VH 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON_FP16INST"
{
  rtx tmp = gen_reg_rtx (V4HFmode);
  emit_insn (gen_neon_vset_lanev4hf (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmul_lane<mode> (operands[0], operands[1], tmp,
				       const0_rtx));
  DONE;
})

(define_expand "neon_vmulls_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:VMDI 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmulls_lane<mode> (operands[0], operands[1], tmp,
					 const0_rtx));
  DONE;
})

(define_expand "neon_vmullu_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:VMDI 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vmullu_lane<mode> (operands[0], operands[1], tmp,
					 const0_rtx));
  DONE;
})

(define_expand "neon_vqdmull_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:VMDI 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmull_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx));
  DONE;
})

(define_expand "neon_vqdmulh_n<mode>"
  [(match_operand:VMDI 0 "s_register_operand")
   (match_operand:VMDI 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmulh_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx));
  DONE;
})

(define_expand "neon_vqrdmulh_n<mode>"
  [(match_operand:VMDI 0 "s_register_operand")
   (match_operand:VMDI 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqrdmulh_lane<mode> (operands[0], operands[1], tmp,
				          const0_rtx));
  DONE;
})

(define_expand "neon_vqdmulh_n<mode>"
  [(match_operand:VMQI 0 "s_register_operand")
   (match_operand:VMQI 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmulh_lane<mode> (operands[0], operands[1], tmp,
					  const0_rtx));
  DONE;
})

(define_expand "neon_vqrdmulh_n<mode>"
  [(match_operand:VMQI 0 "s_register_operand")
   (match_operand:VMQI 1 "s_register_operand")
   (match_operand:<V_elem> 2 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[2], tmp, const0_rtx));
  emit_insn (gen_neon_vqrdmulh_lane<mode> (operands[0], operands[1], tmp,
					   const0_rtx));
  DONE;
})

(define_expand "neon_vmla_n<mode>"
  [(match_operand:VMD 0 "s_register_operand")
   (match_operand:VMD 1 "s_register_operand")
   (match_operand:VMD 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmla_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vmla_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand")
   (match_operand:VMQ 1 "s_register_operand")
   (match_operand:VMQ 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmla_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vmlals_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:<V_widen> 1 "s_register_operand")
   (match_operand:VMDI 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlals_lane<mode> (operands[0], operands[1], operands[2],
					 tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vmlalu_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:<V_widen> 1 "s_register_operand")
   (match_operand:VMDI 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlalu_lane<mode> (operands[0], operands[1], operands[2],
					 tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vqdmlal_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:<V_widen> 1 "s_register_operand")
   (match_operand:VMDI 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmlal_lane<mode> (operands[0], operands[1], operands[2],
					  tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vmls_n<mode>"
  [(match_operand:VMD 0 "s_register_operand")
   (match_operand:VMD 1 "s_register_operand")
   (match_operand:VMD 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmls_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vmls_n<mode>"
  [(match_operand:VMQ 0 "s_register_operand")
   (match_operand:VMQ 1 "s_register_operand")
   (match_operand:VMQ 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<V_HALF>mode);
  emit_insn (gen_neon_vset_lane<V_half> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmls_lane<mode> (operands[0], operands[1], operands[2],
				       tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vmlsls_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:<V_widen> 1 "s_register_operand")
   (match_operand:VMDI 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlsls_lane<mode> (operands[0], operands[1], operands[2],
					tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vmlslu_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:<V_widen> 1 "s_register_operand")
   (match_operand:VMDI 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vmlslu_lane<mode> (operands[0], operands[1], operands[2],
					tmp, const0_rtx));
  DONE;
})

(define_expand "neon_vqdmlsl_n<mode>"
  [(match_operand:<V_widen> 0 "s_register_operand")
   (match_operand:<V_widen> 1 "s_register_operand")
   (match_operand:VMDI 2 "s_register_operand")
   (match_operand:<V_elem> 3 "s_register_operand")]
  "TARGET_NEON"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neon_vset_lane<mode> (tmp, operands[3], tmp, const0_rtx));
  emit_insn (gen_neon_vqdmlsl_lane<mode> (operands[0], operands[1], operands[2],
					  tmp, const0_rtx));
  DONE;
})

(define_insn "@neon_vext<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand" "=w")
	(unspec:VDQX [(match_operand:VDQX 1 "s_register_operand" "w")
		      (match_operand:VDQX 2 "s_register_operand" "w")
                      (match_operand:SI 3 "immediate_operand" "i")]
                     UNSPEC_VEXT))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[3], 0, GET_MODE_NUNITS (<MODE>mode));
  return "vext.<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2, %3";
}
  [(set_attr "type" "neon_ext<q>")]
)

(define_insn "@neon_vrev64<mode>"
  [(set (match_operand:VDQ 0 "s_register_operand" "=w")
	(unspec:VDQ [(match_operand:VDQ 1 "s_register_operand" "w")]
                    UNSPEC_VREV64))]
  "TARGET_NEON"
  "vrev64.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_rev<q>")]
)

(define_insn "@neon_vrev32<mode>"
  [(set (match_operand:VX 0 "s_register_operand" "=w")
	(unspec:VX [(match_operand:VX 1 "s_register_operand" "w")]
                   UNSPEC_VREV32))]
  "TARGET_NEON"
  "vrev32.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_rev<q>")]
)

(define_insn "@neon_vrev16<mode>"
  [(set (match_operand:VE 0 "s_register_operand" "=w")
	(unspec:VE [(match_operand:VE 1 "s_register_operand" "w")]
                   UNSPEC_VREV16))]
  "TARGET_NEON"
  "vrev16.<V_sz_elem>\t%<V_reg>0, %<V_reg>1"
  [(set_attr "type" "neon_rev<q>")]
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
  [(set_attr "type" "neon_bsl<q>")]
)

(define_expand "@neon_vbsl<mode>"
  [(set (match_operand:VDQX 0 "s_register_operand")
        (unspec:VDQX [(match_operand:<V_cmp_result> 1 "s_register_operand")
                      (match_operand:VDQX 2 "s_register_operand")
                      (match_operand:VDQX 3 "s_register_operand")]
                     UNSPEC_VBSL))]
  "TARGET_NEON"
{
  /* We can't alias operands together if they have different modes.  */
  operands[1] = gen_lowpart (<MODE>mode, operands[1]);
})

;; vshl, vrshl
(define_insn "neon_v<shift_op><sup><mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")]
                      VSHL))]
  "TARGET_NEON"
  "v<shift_op>.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_shift_imm<q>")]
)

;; vqshl, vqrshl
(define_insn "neon_v<shift_op><sup><mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:VDQIX 2 "s_register_operand" "w")]
                      VQSHL))]
  "TARGET_NEON"
  "v<shift_op>.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "neon_sat_shift_imm<q>")]
)

;; vshr_n, vrshr_n
(define_insn "neon_v<shift_op><sup>_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
                      VSHR_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) + 1);
  return "v<shift_op>.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "type" "neon_shift_imm<q>")]
)

;; vshrn_n, vrshrn_n
(define_insn "neon_v<shift_op>_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           VSHRN_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "v<shift_op>.<V_if_elem>\t%P0, %q1, %2";
}
  [(set_attr "type" "neon_shift_imm_narrow_q")]
)

;; vqshrn_n, vqrshrn_n
(define_insn "neon_v<shift_op><sup>_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           VQSHRN_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "v<shift_op>.<sup>%#<V_sz_elem>\t%P0, %q1, %2";
}
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

;; vqshrun_n, vqrshrun_n
(define_insn "neon_v<shift_op>_n<mode>"
  [(set (match_operand:<V_narrow> 0 "s_register_operand" "=w")
	(unspec:<V_narrow> [(match_operand:VN 1 "s_register_operand" "w")
			    (match_operand:SI 2 "immediate_operand" "i")]
                           VQSHRUN_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 1, neon_element_bits (<MODE>mode) / 2 + 1);
  return "v<shift_op>.<V_s_elem>\t%P0, %q1, %2";
}
  [(set_attr "type" "neon_sat_shift_imm_narrow_q")]
)

(define_insn "neon_vshl_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
                      UNSPEC_VSHL_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vshl.<V_if_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "type" "neon_shift_imm<q>")]
)

(define_insn "neon_vqshl_<sup>_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
                      VQSHL_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vqshl.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "type" "neon_sat_shift_imm<q>")]
)

(define_insn "neon_vqshlu_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
                      UNSPEC_VQSHLU_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode));
  return "vqshlu.<V_s_elem>\t%<V_reg>0, %<V_reg>1, %2";
}
  [(set_attr "type" "neon_sat_shift_imm<q>")]
)

(define_insn "neon_vshll<sup>_n<mode>"
  [(set (match_operand:<V_widen> 0 "s_register_operand" "=w")
	(unspec:<V_widen> [(match_operand:VW 1 "s_register_operand" "w")
			   (match_operand:SI 2 "immediate_operand" "i")]
			  VSHLL_N))]
  "TARGET_NEON"
{
  /* The boundaries are: 0 < imm <= size.  */
  arm_const_bounds (operands[2], 0, neon_element_bits (<MODE>mode) + 1);
  return "vshll.<sup>%#<V_sz_elem>\t%q0, %P1, %2";
}
  [(set_attr "type" "neon_shift_imm_long")]
)

;; vsra_n, vrsra_n
(define_insn "neon_v<shift_op><sup>_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
		       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      VSRA_N))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[3], 1, neon_element_bits (<MODE>mode) + 1);
  return "v<shift_op>.<sup>%#<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set_attr "type" "neon_shift_acc<q>")]
)

(define_insn "neon_vsri_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
        	       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSRI))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[3], 1, neon_element_bits (<MODE>mode) + 1);
  return "vsri.<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set_attr "type" "neon_shift_reg<q>")]
)

(define_insn "neon_vsli_n<mode>"
  [(set (match_operand:VDQIX 0 "s_register_operand" "=w")
	(unspec:VDQIX [(match_operand:VDQIX 1 "s_register_operand" "0")
        	       (match_operand:VDQIX 2 "s_register_operand" "w")
                       (match_operand:SI 3 "immediate_operand" "i")]
                      UNSPEC_VSLI))]
  "TARGET_NEON"
{
  arm_const_bounds (operands[3], 0, neon_element_bits (<MODE>mode));
  return "vsli.<V_sz_elem>\t%<V_reg>0, %<V_reg>2, %3";
}
  [(set_attr "type" "neon_shift_reg<q>")]
)

(define_insn "neon_vtbl1v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "w")
		      (match_operand:V8QI 2 "s_register_operand" "w")]
                     UNSPEC_VTBL))]
  "TARGET_NEON"
  "vtbl.8\t%P0, {%P1}, %P2"
  [(set_attr "type" "neon_tbl1")]
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
  [(set_attr "type" "neon_tbl2")]
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
  [(set_attr "type" "neon_tbl3")]
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
  [(set_attr "type" "neon_tbl4")]
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
}
  [(set_attr "type" "multiple")]
)

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
}
  [(set_attr "type" "multiple")]
)

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
}
[(set_attr "type" "multiple")]
)

(define_insn "neon_vtbx1v8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=w")
	(unspec:V8QI [(match_operand:V8QI 1 "s_register_operand" "0")
		      (match_operand:V8QI 2 "s_register_operand" "w")
		      (match_operand:V8QI 3 "s_register_operand" "w")]
                     UNSPEC_VTBX))]
  "TARGET_NEON"
  "vtbx.8\t%P0, {%P2}, %P3"
  [(set_attr "type" "neon_tbl1")]
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
  [(set_attr "type" "neon_tbl2")]
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
  [(set_attr "type" "neon_tbl3")]
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
  [(set_attr "type" "neon_tbl4")]
)

(define_expand "@neon_vtrn<mode>_internal"
  [(parallel
    [(set (match_operand:VDQWH 0 "s_register_operand")
	  (unspec:VDQWH [(match_operand:VDQWH 1 "s_register_operand")
			 (match_operand:VDQWH 2 "s_register_operand")]
	   UNSPEC_VTRN1))
     (set (match_operand:VDQWH 3 "s_register_operand")
	  (unspec:VDQWH [(match_dup 1) (match_dup 2)] UNSPEC_VTRN2))])]
  "TARGET_NEON"
  ""
)

;; Note: Different operand numbering to handle tied registers correctly.
(define_insn "*neon_vtrn<mode>_insn"
  [(set (match_operand:VDQWH 0 "s_register_operand" "=&w")
	(unspec:VDQWH [(match_operand:VDQWH 1 "s_register_operand" "0")
		       (match_operand:VDQWH 3 "s_register_operand" "2")]
	 UNSPEC_VTRN1))
   (set (match_operand:VDQWH 2 "s_register_operand" "=&w")
	(unspec:VDQWH [(match_dup 1) (match_dup 3)]
	 UNSPEC_VTRN2))]
  "TARGET_NEON"
  "vtrn.<V_sz_elem>\t%<V_reg>0, %<V_reg>2"
  [(set_attr "type" "neon_permute<q>")]
)

(define_expand "@neon_vzip<mode>_internal"
  [(parallel
    [(set (match_operand:VDQWH 0 "s_register_operand")
	  (unspec:VDQWH [(match_operand:VDQWH 1 "s_register_operand")
			 (match_operand:VDQWH 2 "s_register_operand")]
	   UNSPEC_VZIP1))
    (set (match_operand:VDQWH 3 "s_register_operand")
	 (unspec:VDQWH [(match_dup 1) (match_dup 2)] UNSPEC_VZIP2))])]
  "TARGET_NEON"
  ""
)

;; Note: Different operand numbering to handle tied registers correctly.
(define_insn "*neon_vzip<mode>_insn"
  [(set (match_operand:VDQWH 0 "s_register_operand" "=&w")
	(unspec:VDQWH [(match_operand:VDQWH 1 "s_register_operand" "0")
		       (match_operand:VDQWH 3 "s_register_operand" "2")]
	 UNSPEC_VZIP1))
   (set (match_operand:VDQWH 2 "s_register_operand" "=&w")
	(unspec:VDQWH [(match_dup 1) (match_dup 3)]
	 UNSPEC_VZIP2))]
  "TARGET_NEON"
  "vzip.<V_sz_elem>\t%<V_reg>0, %<V_reg>2"
  [(set_attr "type" "neon_zip<q>")]
)

(define_expand "@neon_vuzp<mode>_internal"
  [(parallel
    [(set (match_operand:VDQWH 0 "s_register_operand")
	  (unspec:VDQWH [(match_operand:VDQWH 1 "s_register_operand")
			(match_operand:VDQWH 2 "s_register_operand")]
	   UNSPEC_VUZP1))
     (set (match_operand:VDQWH 3 "s_register_operand")
	  (unspec:VDQWH [(match_dup 1) (match_dup 2)] UNSPEC_VUZP2))])]
  "TARGET_NEON"
  ""
)

;; Note: Different operand numbering to handle tied registers correctly.
(define_insn "*neon_vuzp<mode>_insn"
  [(set (match_operand:VDQWH 0 "s_register_operand" "=&w")
	(unspec:VDQWH [(match_operand:VDQWH 1 "s_register_operand" "0")
		       (match_operand:VDQWH 3 "s_register_operand" "2")]
	 UNSPEC_VUZP1))
   (set (match_operand:VDQWH 2 "s_register_operand" "=&w")
	(unspec:VDQWH [(match_dup 1) (match_dup 3)]
	 UNSPEC_VUZP2))]
  "TARGET_NEON"
  "vuzp.<V_sz_elem>\t%<V_reg>0, %<V_reg>2"
  [(set_attr "type" "neon_zip<q>")]
)

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
  [(set_attr "type" "neon_load1_1reg<q>")]
)

(define_insn "neon_vld1<VMEMX2_q>_x2<VDQX:mode>"
  [(set (match_operand:VMEMX2 0 "s_register_operand" "=w")
        (unspec:VMEMX2 [(match_operand:VMEMX2 1 "neon_struct_operand" "Um")
                        (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                       UNSPEC_VLD1))]
  "TARGET_NEON"
  "vld1.<V_sz_elem>\t%h0, %A1"
  [(set_attr "type" "neon_load1_2reg<q>")]
)

(define_insn "neon_vld1_x3<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(match_operand:EI 1 "neon_struct_operand" "Um")
                    (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                    UNSPEC_VLD1))]
  "TARGET_NEON"
  "vld1.<V_sz_elem>\t%h0, %A1"
  [(set_attr "type" "neon_load1_3reg<q>")]
)

(define_expand "neon_vld1q_x3<mode>"
  [(match_operand:CI 0 "s_register_operand")
   (match_operand:CI 1 "neon_struct_operand")
   (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem = adjust_address (operands[1], EImode, 0);
  emit_insn (gen_neon_vld1x3qa<mode> (operands[0], mem));
  mem = adjust_address (mem, EImode, GET_MODE_SIZE (EImode));
  emit_insn (gen_neon_vld1x3qb<mode> (operands[0], mem, operands[0]));
  DONE;
})

(define_insn "neon_vld1x3qa<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:EI 1 "neon_struct_operand" "Um")
                    (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD1X3A))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = gen_rtx_REG (EImode, REGNO (operands[0]));
  ops[1] = operands[1];

  output_asm_insn ("vld1.<V_sz_elem>\t%h0, %A1", ops);
  return "";
}
  [(set_attr "type" "neon_load1_3reg<q>")]
)

(define_insn "neon_vld1x3qb<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:EI 1 "neon_struct_operand" "Um")
                    (match_operand:CI 2 "s_register_operand" "0")
                    (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD1X3B))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = gen_rtx_REG (EImode, REGNO (operands[0]) + 6);
  ops[1] = operands[1];

  output_asm_insn ("vld1.<V_sz_elem>\t%h0, %A1", ops);
  return "";
}
  [(set_attr "type" "neon_load1_3reg<q>")]
)

(define_insn "neon_vld1_x4<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                    UNSPEC_VLD1))]
  "TARGET_NEON"
  "vld1.<V_sz_elem>\t%h0, %A1"
  [(set_attr "type" "neon_load1_4reg<q>")]
)
(define_expand "neon_vld1q_x4<mode>"
  [(match_operand:XI 0 "s_register_operand")
   (match_operand:XI 1 "neon_struct_operand")
   (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem = adjust_address (operands[1], OImode, 0);
  emit_insn (gen_neon_vld1x4qa<mode> (operands[0], mem));
  mem = adjust_address (mem, OImode, GET_MODE_SIZE (OImode));
  emit_insn (gen_neon_vld1x4qb<mode> (operands[0], mem, operands[0]));
  DONE;
})

(define_insn "neon_vld1x4qa<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD1X4A))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = gen_rtx_REG (OImode, REGNO (operands[0]));
  ops[1] = operands[1];

  output_asm_insn ("vld1.<V_sz_elem>\t%h0, %A1", ops);
  return "";
}
  [(set_attr "type" "neon_load1_4reg<q>")]
)

(define_insn "neon_vld1x4qb<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (match_operand:XI 2 "s_register_operand" "0")
                    (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD1X4B))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = gen_rtx_REG (OImode, REGNO (operands[0]) + 8);
  ops[1] = operands[1];

  output_asm_insn ("vld1.<V_sz_elem>\t%h0, %A1", ops);
  return "";
}
  [(set_attr "type" "neon_load1_4reg<q>")]
)

;; The lane numbers in the RTL are in GCC lane order, having been flipped
;; in arm_expand_neon_args. The lane numbers are restored to architectural
;; lane order here.
(define_insn "neon_vld1_lane<mode>"
  [(set (match_operand:VDX 0 "s_register_operand" "=w")
        (unspec:VDX [(match_operand:<V_elem> 1 "neon_struct_operand" "Um")
                     (match_operand:VDX 2 "s_register_operand" "0")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VLD1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[3]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  operands[3] = GEN_INT (lane);
  if (max == 1)
    return "vld1.<V_sz_elem>\t%P0, %A1";
  else
    return "vld1.<V_sz_elem>\t{%P0[%c3]}, %A1";
}
  [(set_attr "type" "neon_load1_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vld1_lane<mode>"
  [(set (match_operand:VQX 0 "s_register_operand" "=w")
        (unspec:VQX [(match_operand:<V_elem> 1 "neon_struct_operand" "Um")
                     (match_operand:VQX 2 "s_register_operand" "0")
                     (match_operand:SI 3 "immediate_operand" "i")]
                    UNSPEC_VLD1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[3]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  operands[3] = GEN_INT (lane);
  int regno = REGNO (operands[0]);
  if (lane >= max / 2)
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
  [(set_attr "type" "neon_load1_one_lane<q>")]
)

(define_insn "neon_vld1_dup<mode>"
  [(set (match_operand:VD_LANE 0 "s_register_operand" "=w")
        (vec_duplicate:VD_LANE (match_operand:<V_elem> 1 "neon_struct_operand" "Um")))]
  "TARGET_NEON"
  "vld1.<V_sz_elem>\t{%P0[]}, %A1"
  [(set_attr "type" "neon_load1_all_lanes<q>")]
)

;; Special case for DImode.  Treat it exactly like a simple load.
(define_expand "neon_vld1_dupdi"
  [(set (match_operand:DI 0 "s_register_operand")
        (unspec:DI [(match_operand:DI 1 "neon_struct_operand")]
		   UNSPEC_VLD1))]
  "TARGET_NEON"
  ""
)

(define_insn "neon_vld1_dup<mode>"
  [(set (match_operand:VQ2 0 "s_register_operand" "=w")
        (vec_duplicate:VQ2 (match_operand:<V_elem> 1 "neon_struct_operand" "Um")))]
  "TARGET_NEON"
{
  return "vld1.<V_sz_elem>\t{%e0[], %f0[]}, %A1";
}
  [(set_attr "type" "neon_load1_all_lanes<q>")]
)

(define_insn_and_split "neon_vld1_dupv2di"
   [(set (match_operand:V2DI 0 "s_register_operand" "=w")
    (vec_duplicate:V2DI (match_operand:DI 1 "neon_struct_operand" "Um")))]
   "TARGET_NEON"
   "#"
   "&& reload_completed"
   [(const_int 0)]
   {
    rtx tmprtx = gen_lowpart (DImode, operands[0]);
    emit_insn (gen_neon_vld1_dupdi (tmprtx, operands[1]));
    emit_move_insn (gen_highpart (DImode, operands[0]), tmprtx );
    DONE;
    }
  [(set_attr "length" "8")
   (set_attr "type" "neon_load1_all_lanes_q")]
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
  [(set_attr "type" "neon_store1_1reg<q>")])

(define_insn "neon_vst1<VMEMX2_q>_x2<VDQX:mode>"
  [(set (match_operand:VMEMX2 0 "neon_struct_operand" "=Um")
        (unspec:VMEMX2 [(match_operand:VMEMX2 1 "s_register_operand" "w")
                    (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST1))]
  "TARGET_NEON"
  "vst1.<V_sz_elem>\t%h1, %A0"
  [(set_attr "type" "neon_store1_2reg<q>")]
)

(define_insn "neon_vst1_x3<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:EI 1 "s_register_operand" "w")
                    (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST1))]
  "TARGET_NEON"
  "vst1.<V_sz_elem>\t%h1, %A0"
  [(set_attr "type" "neon_store1_3reg<q>")]
)

(define_expand "neon_vst1q_x3<mode>"
  [(match_operand:CI 0 "neon_struct_operand")
   (match_operand:CI 1 "s_register_operand")
   (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem = adjust_address (operands[0], EImode, 0);
  emit_insn (gen_neon_vst1x3qa<mode> (mem, operands[1]));
  mem = adjust_address (mem, EImode, GET_MODE_SIZE (EImode));
  emit_insn (gen_neon_vst1x3qb<mode> (mem, operands[1]));
  DONE;
})

(define_insn "neon_vst1x3qa<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:CI 1 "s_register_operand" "w")
                    (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST1X3A))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (EImode, REGNO (operands[1]));

  output_asm_insn ("vst1.<V_sz_elem>\t%h1, %A0", ops);
  return "";
}
  [(set_attr "type" "neon_store1_3reg<q>")]
)

(define_insn "neon_vst1x3qb<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:CI 1 "s_register_operand" "w")
                    (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST1X3B))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (EImode, REGNO (operands[1]) + 6);

  output_asm_insn ("vst1.<V_sz_elem>\t%h1, %A0", ops);
  return "";
}
  [(set_attr "type" "neon_store1_3reg<q>")]
)

(define_insn "neon_vst1_x4<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
        (unspec:OI [(match_operand:OI 1 "s_register_operand" "w")
                    (unspec:VDQX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST1))]
  "TARGET_NEON"
  "vst1.<V_sz_elem>\t%h1, %A0"
  [(set_attr "type" "neon_store1_4reg<q>")]
)

(define_expand "neon_vst1q_x4<mode>"
  [(match_operand:XI 0 "neon_struct_operand")
   (match_operand:XI 1 "s_register_operand")
   (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  rtx mem = adjust_address (operands[0], OImode, 0);
  emit_insn (gen_neon_vst1x4qa<mode> (mem, operands[1]));
  mem = adjust_address (mem, OImode, GET_MODE_SIZE (OImode));
  emit_insn (gen_neon_vst1x4qb<mode> (mem, operands[1]));
  DONE;
})

(define_insn "neon_vst1x4qa<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
        (unspec:OI [(match_operand:XI 1 "s_register_operand" "w")
                    (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST1X4A))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (OImode, REGNO (operands[1]));

  output_asm_insn ("vst1.<V_sz_elem>\t%h1, %A0", ops);
  return "";
}
  [(set_attr "type" "neon_store1_4reg<q>")]
)

(define_insn "neon_vst1x4qb<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
        (unspec:OI [(match_operand:XI 1 "s_register_operand" "w")
                    (unspec:VQXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST1X4B))]
  "TARGET_NEON"
{
  rtx ops[2];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (OImode, REGNO (operands[1]) + 8);

  output_asm_insn ("vst1.<V_sz_elem>\t%h1, %A0", ops);
  return "";
}
  [(set_attr "type" "neon_store1_4reg<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst1_lane<mode>"
  [(set (match_operand:<V_elem> 0 "neon_struct_operand" "=Um")
	(unspec:<V_elem>
	  [(match_operand:VDX 1 "s_register_operand" "w")
	   (match_operand:SI 2 "immediate_operand" "i")]
	  UNSPEC_VST1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  operands[2] = GEN_INT (lane);
  if (max == 1)
    return "vst1.<V_sz_elem>\t{%P1}, %A0";
  else
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, %A0";
}
  [(set_attr "type" "neon_store1_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst1_lane<mode>"
  [(set (match_operand:<V_elem> 0 "neon_struct_operand" "=Um")
	(unspec:<V_elem>
	  [(match_operand:VQX 1 "s_register_operand" "w")
	   (match_operand:SI 2 "immediate_operand" "i")]
	  UNSPEC_VST1_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  operands[2] = GEN_INT (lane);
  operands[1] = gen_rtx_REG (<V_HALF>mode, regno);
  if (max == 2)
    return "vst1.<V_sz_elem>\t{%P1}, %A0";
  else
    return "vst1.<V_sz_elem>\t{%P1[%c2]}, %A0";
}
  [(set_attr "type" "neon_store1_one_lane<q>")]
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
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, %A1";
  else
    return "vld2.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_load1_2reg<q>")
                    (const_string "neon_load2_2reg<q>")))]
)

(define_insn "neon_vld2<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2))]
  "TARGET_NEON"
  "vld2.<V_sz_elem>\t%h0, %A1"
  [(set_attr "type" "neon_load2_2reg_q")])

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vld2_lane<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(match_operand:<V_two_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:TI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD_LANE [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[3]));
  int regno = REGNO (operands[0]);
  rtx ops[4];
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = operands[1];
  ops[3] = GEN_INT (lane);
  output_asm_insn ("vld2.<V_sz_elem>\t{%P0[%c3], %P1[%c3]}, %A2", ops);
  return "";
}
  [(set_attr "type" "neon_load2_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vld2_lane<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:<V_two_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:OI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VQ_HS [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[3]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[4];
  if (lane >= max / 2)
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
  [(set_attr "type" "neon_load2_one_lane<q>")]
)

(define_insn "neon_vld2_dup<mode>"
  [(set (match_operand:TI 0 "s_register_operand" "=w")
        (unspec:TI [(match_operand:<V_two_elem> 1 "neon_struct_operand" "Um")
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_DUP))]
  "TARGET_NEON"
{
  if (GET_MODE_NUNITS (<MODE>mode) > 1)
    return "vld2.<V_sz_elem>\t{%e0[], %f0[]}, %A1";
  else
    return "vld1.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_load2_all_lanes<q>")
                    (const_string "neon_load1_1reg<q>")))]
)

(define_insn "neon_vld2_dupv8bf"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:V2BF 1 "neon_struct_operand" "Um")
                    (unspec:V8BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_DUP))]
  "TARGET_BF16_SIMD"
  {
    rtx ops[5];
    int tabbase = REGNO (operands[0]);

    ops[4] = operands[1];
    ops[0] = gen_rtx_REG (V4BFmode, tabbase);
    ops[1] = gen_rtx_REG (V4BFmode, tabbase + 2);
    ops[2] = gen_rtx_REG (V4BFmode, tabbase + 4);
    ops[3] = gen_rtx_REG (V4BFmode, tabbase + 6);
    output_asm_insn ("vld2.16\t{%P0, %P1, %P2, %P3}, %A4", ops);
    return "";
  }
  [(set_attr "type" "neon_load2_all_lanes_q")]
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
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST2))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, %A0";
  else
    return "vst2.<V_sz_elem>\t%h1, %A0";
}
  [(set (attr "type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_store1_2reg<q>")
                    (const_string "neon_store2_one_lane<q>")))]
)

(define_insn "neon_vst2<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
	(unspec:OI [(match_operand:OI 1 "s_register_operand" "w")
		    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VST2))]
  "TARGET_NEON"
  "vst2.<V_sz_elem>\t%h1, %A0"
  [(set_attr "type" "neon_store2_4reg<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst2_lane<mode>"
  [(set (match_operand:<V_two_elem> 0 "neon_struct_operand" "=Um")
	(unspec:<V_two_elem>
	  [(match_operand:TI 1 "s_register_operand" "w")
	   (match_operand:SI 2 "immediate_operand" "i")
	   (unspec:VD_LANE [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	  UNSPEC_VST2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  int regno = REGNO (operands[1]);
  rtx ops[4];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = GEN_INT (lane);
  output_asm_insn ("vst2.<V_sz_elem>\t{%P1[%c3], %P2[%c3]}, %A0", ops);
  return "";
}
  [(set_attr "type" "neon_store2_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst2_lane<mode>"
  [(set (match_operand:<V_two_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_two_elem>
           [(match_operand:OI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VQ_HS [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST2_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[4];
  if (lane >= max / 2)
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
  [(set_attr "type" "neon_store2_one_lane<q>")]
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
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, %A1";
  else
    return "vld3.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_load1_3reg<q>")
                    (const_string "neon_load3_3reg<q>")))]
)

(define_expand "vec_load_lanesci<mode>"
  [(match_operand:CI 0 "s_register_operand")
   (match_operand:CI 1 "neon_struct_operand")
   (unspec:VQ2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vld3<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "neon_vld3<mode>"
  [(match_operand:CI 0 "s_register_operand")
   (match_operand:CI 1 "neon_struct_operand")
   (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_load3_3reg<q>")]
)

(define_insn "neon_vld3qb<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:EI 1 "neon_struct_operand" "Um")
                    (match_operand:CI 2 "s_register_operand" "0")
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_load3_3reg<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vld3_lane<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(match_operand:<V_three_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:EI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD_LANE [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N (<MODE>mode, INTVAL (operands[3]));
  int regno = REGNO (operands[0]);
  rtx ops[5];
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = operands[1];
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0[%c4], %P1[%c4], %P2[%c4]}, %3",
                   ops);
  return "";
}
  [(set_attr "type" "neon_load3_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vld3_lane<mode>"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:<V_three_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:CI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VQ_HS [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[3]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[5];
  if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 4);
  ops[2] = gen_rtx_REG (DImode, regno + 8);
  ops[3] = operands[1];
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vld3.<V_sz_elem>\t{%P0[%c4], %P1[%c4], %P2[%c4]}, %3",
                   ops);
  return "";
}
  [(set_attr "type" "neon_load3_one_lane<q>")]
)

(define_insn "neon_vld3_dup<mode>"
  [(set (match_operand:EI 0 "s_register_operand" "=w")
        (unspec:EI [(match_operand:<V_three_elem> 1 "neon_struct_operand" "Um")
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
      output_asm_insn ("vld3.<V_sz_elem>\t{%P0[], %P1[], %P2[]}, %3", ops);
      return "";
    }
  else
    return "vld1.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_load3_all_lanes<q>")
                    (const_string "neon_load1_1reg<q>")))])

(define_insn "neon_vld3_dupv8bf"
  [(set (match_operand:CI 0 "s_register_operand" "=w")
        (unspec:CI [(match_operand:V2BF 1 "neon_struct_operand" "Um")
                    (unspec:V8BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_DUP))]
  "TARGET_BF16_SIMD"
  {
    rtx ops[4];
    int tabbase = REGNO (operands[0]);

    ops[3] = operands[1];
    ops[0] = gen_rtx_REG (V4BFmode, tabbase);
    ops[1] = gen_rtx_REG (V4BFmode, tabbase + 2);
    ops[2] = gen_rtx_REG (V4BFmode, tabbase + 4);
    output_asm_insn ("vld3.16\t{%P0[], %P1[], %P2[]}, %A3", ops);
    return "";
  }
  [(set_attr "type" "neon_load3_all_lanes_q")]
)

(define_expand "vec_store_lanesei<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand")
	(unspec:EI [(match_operand:EI 1 "s_register_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3))]
  "TARGET_NEON")

(define_insn "neon_vst3<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:EI 1 "s_register_operand" "w")
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST3))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, %A0";
  else
    return "vst3.<V_sz_elem>\t%h1, %A0";
}
  [(set (attr "type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_store1_3reg<q>")
                    (const_string "neon_store3_one_lane<q>")))])

(define_expand "vec_store_lanesci<mode>"
  [(match_operand:CI 0 "neon_struct_operand")
   (match_operand:CI 1 "s_register_operand")
   (unspec:VQ2 [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
  "TARGET_NEON"
{
  emit_insn (gen_neon_vst3<mode> (operands[0], operands[1]));
  DONE;
})

(define_expand "neon_vst3<mode>"
  [(match_operand:CI 0 "neon_struct_operand")
   (match_operand:CI 1 "s_register_operand")
   (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_store3_3reg<q>")]
)

(define_insn "neon_vst3qb<mode>"
  [(set (match_operand:EI 0 "neon_struct_operand" "=Um")
        (unspec:EI [(match_operand:CI 1 "s_register_operand" "w")
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_store3_3reg<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst3_lane<mode>"
  [(set (match_operand:<V_three_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_three_elem>
           [(match_operand:EI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VD_LANE [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  int regno = REGNO (operands[1]);
  rtx ops[5];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = gen_rtx_REG (DImode, regno + 4);
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1[%c4], %P2[%c4], %P3[%c4]}, %0",
                   ops);
  return "";
}
  [(set_attr "type" "neon_store3_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst3_lane<mode>"
  [(set (match_operand:<V_three_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_three_elem>
           [(match_operand:CI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VQ_HS [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST3_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[5];
  if (lane >= max / 2)
    {
      lane -= max / 2;
      regno += 2;
    }
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 8);
  ops[4] = GEN_INT (lane);
  output_asm_insn ("vst3.<V_sz_elem>\t{%P1[%c4], %P2[%c4], %P3[%c4]}, %0",
                   ops);
  return "";
}
  [(set_attr "type" "neon_store3_one_lane<q>")]
)

(define_expand "vec_load_lanesoi<mode>"
  [(set (match_operand:OI 0 "s_register_operand")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand")
                    (unspec:VDX [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
		   UNSPEC_VLD4))]
  "TARGET_NEON")

(define_insn "neon_vld4<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vld1.64\t%h0, %A1";
  else
    return "vld4.<V_sz_elem>\t%h0, %A1";
}
  [(set (attr "type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_load1_4reg<q>")
                    (const_string "neon_load4_4reg<q>")))]
)

(define_expand "neon_vld4<mode>"
  [(match_operand:XI 0 "s_register_operand")
   (match_operand:XI 1 "neon_struct_operand")
   (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_load4_4reg<q>")]
)

(define_insn "neon_vld4qb<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:OI 1 "neon_struct_operand" "Um")
                    (match_operand:XI 2 "s_register_operand" "0")
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_load4_4reg<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vld4_lane<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:<V_four_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:OI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VD_LANE [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[3]));
  int regno = REGNO (operands[0]);
  rtx ops[6];
  ops[0] = gen_rtx_REG (DImode, regno);
  ops[1] = gen_rtx_REG (DImode, regno + 2);
  ops[2] = gen_rtx_REG (DImode, regno + 4);
  ops[3] = gen_rtx_REG (DImode, regno + 6);
  ops[4] = operands[1];
  ops[5] = GEN_INT (lane);
  output_asm_insn ("vld4.<V_sz_elem>\t{%P0[%c5], %P1[%c5], %P2[%c5], %P3[%c5]}, %A4",
                   ops);
  return "";
}
  [(set_attr "type" "neon_load4_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vld4_lane<mode>"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:<V_four_elem> 1 "neon_struct_operand" "Um")
                    (match_operand:XI 2 "s_register_operand" "0")
                    (match_operand:SI 3 "immediate_operand" "i")
                    (unspec:VQ_HS [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[3]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[0]);
  rtx ops[6];
  if (lane >= max / 2)
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
  [(set_attr "type" "neon_load4_one_lane<q>")]
)

(define_insn "neon_vld4_dup<mode>"
  [(set (match_operand:OI 0 "s_register_operand" "=w")
        (unspec:OI [(match_operand:<V_four_elem> 1 "neon_struct_operand" "Um")
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set (attr "type")
      (if_then_else (gt (const_string "<V_mode_nunits>") (const_string "1"))
                    (const_string "neon_load4_all_lanes<q>")
                    (const_string "neon_load1_1reg<q>")))]
)

(define_insn "neon_vld4_dupv8bf"
  [(set (match_operand:XI 0 "s_register_operand" "=w")
        (unspec:XI [(match_operand:V2BF 1 "neon_struct_operand" "Um")
                    (unspec:V8BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VLD2_DUP))]
  "TARGET_BF16_SIMD"
  {
    rtx ops[5];
    int tabbase = REGNO (operands[0]);

    ops[4] = operands[1];
    ops[0] = gen_rtx_REG (V4BFmode, tabbase);
    ops[1] = gen_rtx_REG (V4BFmode, tabbase + 2);
    ops[2] = gen_rtx_REG (V4BFmode, tabbase + 4);
    ops[3] = gen_rtx_REG (V4BFmode, tabbase + 6);
    output_asm_insn ("vld4.16\t{%P0[], %P1[], %P2[], %P3[]}, %A4", ops);
    return "";
  }
  [(set_attr "type" "neon_load4_all_lanes_q")]
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
                    (unspec:VDXBF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
                   UNSPEC_VST4))]
  "TARGET_NEON"
{
  if (<V_sz_elem> == 64)
    return "vst1.64\t%h1, %A0";
  else
    return "vst4.<V_sz_elem>\t%h1, %A0";
}
  [(set (attr "type")
      (if_then_else (eq (const_string "<V_sz_elem>") (const_string "64"))
                    (const_string "neon_store1_4reg<q>")
                    (const_string "neon_store4_4reg<q>")))]
)

(define_expand "neon_vst4<mode>"
  [(match_operand:XI 0 "neon_struct_operand")
   (match_operand:XI 1 "s_register_operand")
   (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_store4_4reg<q>")]
)

(define_insn "neon_vst4qb<mode>"
  [(set (match_operand:OI 0 "neon_struct_operand" "=Um")
        (unspec:OI [(match_operand:XI 1 "s_register_operand" "w")
                    (unspec:VQ2BF [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
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
  [(set_attr "type" "neon_store4_4reg<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst4_lane<mode>"
  [(set (match_operand:<V_four_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_four_elem>
           [(match_operand:OI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VD_LANE [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  int regno = REGNO (operands[1]);
  rtx ops[6];
  ops[0] = operands[0];
  ops[1] = gen_rtx_REG (DImode, regno);
  ops[2] = gen_rtx_REG (DImode, regno + 2);
  ops[3] = gen_rtx_REG (DImode, regno + 4);
  ops[4] = gen_rtx_REG (DImode, regno + 6);
  ops[5] = GEN_INT (lane);
  output_asm_insn ("vst4.<V_sz_elem>\t{%P1[%c5], %P2[%c5], %P3[%c5], %P4[%c5]}, %A0",
                   ops);
  return "";
}
  [(set_attr "type" "neon_store4_one_lane<q>")]
)

;; see comment on neon_vld1_lane for reason why the lane numbers are reversed
;; here on big endian targets.
(define_insn "neon_vst4_lane<mode>"
  [(set (match_operand:<V_four_elem> 0 "neon_struct_operand" "=Um")
        (unspec:<V_four_elem>
           [(match_operand:XI 1 "s_register_operand" "w")
            (match_operand:SI 2 "immediate_operand" "i")
            (unspec:VQ_HS [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
           UNSPEC_VST4_LANE))]
  "TARGET_NEON"
{
  HOST_WIDE_INT lane = NEON_ENDIAN_LANE_N(<MODE>mode, INTVAL (operands[2]));
  HOST_WIDE_INT max = GET_MODE_NUNITS (<MODE>mode);
  int regno = REGNO (operands[1]);
  rtx ops[6];
  if (lane >= max / 2)
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
  [(set_attr "type" "neon_store4_4reg<q>")]
)

(define_insn "neon_vec_unpack<US>_lo_<mode>"
  [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
        (SE:<V_unpack> (vec_select:<V_HALF>
			  (match_operand:VU 1 "register_operand" "w")
			  (match_operand:VU 2 "vect_par_constant_low" ""))))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vmovl.<US><V_sz_elem> %q0, %e1"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_insn "neon_vec_unpack<US>_hi_<mode>"
  [(set (match_operand:<V_unpack> 0 "register_operand" "=w")
        (SE:<V_unpack> (vec_select:<V_HALF>
			  (match_operand:VU 1 "register_operand" "w")
			  (match_operand:VU 2 "vect_par_constant_high" ""))))]
  "TARGET_NEON && !BYTES_BIG_ENDIAN"
  "vmovl.<US><V_sz_elem> %q0, %f1"
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_expand "vec_unpack<US>_hi_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand")
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
  [(match_operand:<V_unpack> 0 "register_operand")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand"))]
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
  [(set_attr "type" "neon_mul_<V_elem_ch>_long")]
)

(define_expand "vec_widen_<US>mult_lo_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand"))
   (SE:<V_unpack> (match_operand:VU 2 "register_operand"))]
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
  [(set_attr "type" "neon_mul_<V_elem_ch>_long")]
)

(define_expand "vec_widen_<US>mult_hi_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand"))
   (SE:<V_unpack> (match_operand:VU 2 "register_operand"))]
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
  [(set_attr "type" "neon_shift_imm_long")]
)

(define_expand "vec_widen_<US>shiftl_lo_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand"))
   (match_operand:SI 2 "immediate_operand")]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 {
  emit_insn (gen_neon_vec_<US>shiftl_<V_half> (operands[0],
		simplify_gen_subreg (<V_HALF>mode, operands[1], <MODE>mode, 0),
		operands[2]));
   DONE;
 }
)

(define_expand "vec_widen_<US>shiftl_hi_<mode>"
  [(match_operand:<V_unpack> 0 "register_operand")
   (SE:<V_unpack> (match_operand:VU 1 "register_operand"))
   (match_operand:SI 2 "immediate_operand")]
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
  [(set_attr "type" "neon_move")]
)

(define_expand "vec_unpack<US>_lo_<mode>"
 [(match_operand:<V_double_width> 0 "register_operand")
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
 [(match_operand:<V_double_width> 0 "register_operand")
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
  [(set_attr "type" "neon_mul_<V_elem_ch>_long")]
)

(define_expand "vec_widen_<US>mult_hi_<mode>"
  [(match_operand:<V_double_width> 0 "register_operand")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand"))
   (SE:<V_double_width> (match_operand:VDI 2 "register_operand"))]
 "TARGET_NEON"
 {
   rtx tmpreg = gen_reg_rtx (<V_widen>mode);
   emit_insn (gen_neon_vec_<US>mult_<mode> (tmpreg, operands[1], operands[2]));
   emit_insn (gen_neon_vget_high<V_widen_l> (operands[0], tmpreg));
 					    
   DONE;

 }
)

(define_expand "vec_widen_<US>mult_lo_<mode>"
  [(match_operand:<V_double_width> 0 "register_operand")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand"))
   (SE:<V_double_width> (match_operand:VDI 2 "register_operand"))]
 "TARGET_NEON"
 {
   rtx tmpreg = gen_reg_rtx (<V_widen>mode);
   emit_insn (gen_neon_vec_<US>mult_<mode> (tmpreg, operands[1], operands[2]));
   emit_insn (gen_neon_vget_low<V_widen_l> (operands[0], tmpreg));
 					    
   DONE;

 }
)

(define_expand "vec_widen_<US>shiftl_hi_<mode>"
 [(match_operand:<V_double_width> 0 "register_operand")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand"))
   (match_operand:SI 2 "immediate_operand")]
 "TARGET_NEON"
 {
   rtx tmpreg = gen_reg_rtx (<V_widen>mode);
   emit_insn (gen_neon_vec_<US>shiftl_<mode> (tmpreg, operands[1], operands[2]));
   emit_insn (gen_neon_vget_high<V_widen_l> (operands[0], tmpreg));

   DONE;
 }
)

(define_expand "vec_widen_<US>shiftl_lo_<mode>"
  [(match_operand:<V_double_width> 0 "register_operand")
   (SE:<V_double_width> (match_operand:VDI 1 "register_operand"))
   (match_operand:SI 2 "immediate_operand")]
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
 [(set_attr "type" "multiple")
  (set_attr "length" "8")]
)

;; For the non-quad case.
(define_insn "neon_vec_pack_trunc_<mode>"
 [(set (match_operand:<V_narrow> 0 "register_operand" "=w")
       (truncate:<V_narrow> (match_operand:VN 1 "register_operand" "w")))]
 "TARGET_NEON && !BYTES_BIG_ENDIAN"
 "vmovn.i<V_sz_elem>\t%P0, %q1"
 [(set_attr "type" "neon_move_narrow_q")]
)

(define_expand "vec_pack_trunc_<mode>"
 [(match_operand:<V_narrow_pack> 0 "register_operand")
  (match_operand:VSHFT 1 "register_operand")
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
 [(set (match_operand:VF 0 "s_register_operand" "=w")
       (abs:VF (minus:VF (match_operand:VF 1 "s_register_operand" "w")
			 (match_operand:VF 2 "s_register_operand" "w"))))]
 "ARM_HAVE_NEON_<MODE>_ARITH"
 "vabd.<V_s_elem> %<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_abd_s<q>")]
)

(define_insn "neon_vabd<mode>_3"
 [(set (match_operand:VF 0 "s_register_operand" "=w")
       (abs:VF (unspec:VF [(match_operand:VF 1 "s_register_operand" "w")
			    (match_operand:VF 2 "s_register_operand" "w")]
		UNSPEC_VSUB)))]
 "ARM_HAVE_NEON_<MODE>_ARITH"
 "vabd.<V_if_elem> %<V_reg>0, %<V_reg>1, %<V_reg>2"
 [(set_attr "type" "neon_fp_abd_s<q>")]
)

(define_insn "neon_<sup>mmlav16qi"
  [(set (match_operand:V4SI 0 "register_operand" "=w")
	(plus:V4SI
	 (unspec:V4SI [(match_operand:V16QI 2 "register_operand" "w")
		       (match_operand:V16QI 3 "register_operand" "w")] MATMUL)
	 (match_operand:V4SI 1 "register_operand" "0")))]
  "TARGET_I8MM"
  "v<sup>mmla.<mmla_sfx>\t%q0, %q2, %q3"
  [(set_attr "type" "neon_mla_s_q")]
)

(define_insn "neon_vbfdot<VCVTF:mode>"
  [(set (match_operand:VCVTF 0 "register_operand" "=w")
	(plus:VCVTF (match_operand:VCVTF 1 "register_operand" "0")
		    (unspec:VCVTF [
			    (match_operand:<VSF2BF> 2 "register_operand" "w")
			    (match_operand:<VSF2BF> 3 "register_operand" "w")]
		     UNSPEC_DOT_S)))]
  "TARGET_BF16_SIMD"
  "vdot.bf16\\t%<V_reg>0, %<V_reg>2, %<V_reg>3"
  [(set_attr "type" "neon_dot<q>")]
)

(define_insn "neon_vbfdot_lanev4bf<VCVTF:mode>"
  [(set (match_operand:VCVTF 0 "register_operand" "=w")
	(plus:VCVTF (match_operand:VCVTF 1 "register_operand" "0")
		    (unspec:VCVTF [
			    (match_operand:<VSF2BF> 2 "register_operand" "w")
			    (match_operand:V4BF 3 "register_operand" "x")
			    (match_operand:SI 4 "immediate_operand" "i")]
		     UNSPEC_DOT_S)))]
  "TARGET_BF16_SIMD"
  "vdot.bf16\\t%<V_reg>0, %<V_reg>2, %P3[%c4]"
  [(set_attr "type" "neon_dot<q>")]
)

(define_insn "neon_vbfdot_lanev8bf<VCVTF:mode>"
  [(set (match_operand:VCVTF 0 "register_operand" "=w")
	(plus:VCVTF (match_operand:VCVTF 1 "register_operand" "0")
		    (unspec:VCVTF [
			    (match_operand:<VSF2BF> 2 "register_operand" "w")
			    (match_operand:V8BF 3 "register_operand" "x")
			    (match_operand:SI 4 "immediate_operand" "i")]
		     UNSPEC_DOT_S)))]
  "TARGET_BF16_SIMD"
  {
    int lane = INTVAL (operands[4]);
    int half = GET_MODE_NUNITS (GET_MODE (operands[3])) / 4;
    if (lane < half)
      return "vdot.bf16\\t%<V_reg>0, %<V_reg>2, %e3[%c4]";
    else
      {
	operands[4] = GEN_INT (lane - half);
	return "vdot.bf16\\t%<V_reg>0, %<V_reg>2, %f3[%c4]";
      }
  }
  [(set_attr "type" "neon_dot<q>")]
)

(define_insn "neon_vbfcvtv4sf<VBFCVT:mode>"
  [(set (match_operand:VBFCVT 0 "register_operand" "=w")
       (unspec:VBFCVT [(match_operand:V4SF 1 "register_operand" "w")]
	UNSPEC_BFCVT))]
  "TARGET_BF16_SIMD"
  "vcvt.bf16.f32\\t%<V_bf_low>0, %q1"
  [(set_attr "type" "neon_fp_cvt_narrow_s_q")]
)

(define_insn "neon_vbfcvtv4sf_highv8bf"
  [(set (match_operand:V8BF 0 "register_operand" "=w")
       (unspec:V8BF [(match_operand:V8BF 1 "register_operand" "0")
		     (match_operand:V4SF 2 "register_operand" "w")]
	UNSPEC_BFCVT_HIGH))]
  "TARGET_BF16_SIMD"
  "vcvt.bf16.f32\\t%f0, %q2"
  [(set_attr "type" "neon_fp_cvt_narrow_s_q")]
)

(define_insn "neon_vbfcvtsf"
  [(set (match_operand:BF 0 "register_operand" "=t")
       (unspec:BF [(match_operand:SF 1 "register_operand" "t")]
	UNSPEC_BFCVT))]
  "TARGET_BF16_FP"
  "vcvtb.bf16.f32\\t%0, %1"
  [(set_attr "type" "f_cvt")]
)

(define_insn "neon_vbfcvt<VBFCVT:mode>"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
       (unspec:V4SF [(match_operand:VBFCVT 1 "register_operand" "w")]
	UNSPEC_BFCVT))]
  "TARGET_BF16_SIMD"
  "vshll.u32\\t%q0, %<V_bf_low>1, #16"
  [(set_attr "type" "neon_shift_imm_q")]
)

(define_insn "neon_vbfcvt_highv8bf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
       (unspec:V4SF [(match_operand:V8BF 1 "register_operand" "w")]
	UNSPEC_BFCVT_HIGH))]
  "TARGET_BF16_SIMD"
  "vshll.u32\\t%q0, %f1, #16"
  [(set_attr "type" "neon_shift_imm_q")]
)

;; Convert a BF scalar operand to SF via VSHL.
;; VSHL doesn't accept 32-bit registers where the BF and SF scalar operands
;; would be allocated, therefore the operands must be converted to intermediate
;; vectors (i.e. V2SI) in order to apply 64-bit registers.
(define_expand "neon_vbfcvtbf"
  [(match_operand:SF 0 "register_operand")
   (unspec:SF [(match_operand:BF 1 "register_operand")] UNSPEC_BFCVT)]
  "TARGET_BF16_FP"
{
  rtx op0 = gen_reg_rtx (V2SImode);
  rtx op1 = gen_reg_rtx (V2SImode);
  emit_insn (gen_neon_vbfcvtbf_cvtmodev2si (op1, operands[1]));
  emit_insn (gen_neon_vshl_nv2si (op0, op1, gen_int_mode(16, SImode)));
  emit_insn (gen_neon_vbfcvtbf_cvtmodesf (operands[0], op0));
  DONE;
})

;; Convert BF mode to V2SI and V2SI to SF.
;; Implement this by allocating a 32-bit operand in the low half of a 64-bit
;; register indexed by a 32-bit sub-register number.
;; This will generate reloads but compiler can optimize out the moves.
;; Use 'x' constraint to guarantee the 32-bit sub-registers in an indexable
;; range so that to avoid extra moves.
(define_insn "neon_vbfcvtbf_cvtmode<mode>"
  [(set (match_operand:VBFCVTM 0 "register_operand" "=x")
       (unspec:VBFCVTM [(match_operand:<V_bf_cvt_m> 1 "register_operand" "0")]
	UNSPEC_BFCVT))]
  "TARGET_BF16_FP"
  ""
)

(define_insn "neon_vmmlav8bf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
        (plus:V4SF (match_operand:V4SF 1 "register_operand" "0")
                   (unspec:V4SF [(match_operand:V8BF 2 "register_operand" "w")
                                 (match_operand:V8BF 3 "register_operand" "w")]
                    UNSPEC_BFMMLA)))]
  "TARGET_BF16_SIMD"
  "vmmla.bf16\\t%q0, %q2, %q3"
  [(set_attr "type" "neon_fp_mla_s_q")]
)

(define_insn "neon_vfma<bt>v8bf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
        (plus: V4SF (match_operand:V4SF 1 "register_operand" "0")
                    (unspec:V4SF [(match_operand:V8BF 2 "register_operand" "w")
                                  (match_operand:V8BF 3 "register_operand" "w")]
                     BF_MA)))]
  "TARGET_BF16_SIMD"
  "vfma<bt>.bf16\\t%q0, %q2, %q3"
  [(set_attr "type" "neon_fp_mla_s_q")]
)

(define_insn "neon_vfma<bt>_lanev8bf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
        (plus: V4SF (match_operand:V4SF 1 "register_operand" "0")
                    (unspec:V4SF [(match_operand:V8BF 2 "register_operand" "w")
                                  (match_operand:V4BF 3 "register_operand" "x")
                                  (match_operand:SI 4 "const_int_operand" "n")]
                     BF_MA)))]
  "TARGET_BF16_SIMD"
  "vfma<bt>.bf16\\t%q0, %q2, %P3[%c4]"
  [(set_attr "type" "neon_fp_mla_s_scalar_q")]
)

(define_expand "neon_vfma<bt>_laneqv8bf"
  [(set (match_operand:V4SF 0 "register_operand" "=w")
        (plus: V4SF (match_operand:V4SF 1 "register_operand" "0")
                    (unspec:V4SF [(match_operand:V8BF 2 "register_operand" "w")
                                  (match_operand:V8BF 3 "register_operand" "x")
                                  (match_operand:SI 4 "const_int_operand" "n")]
                     BF_MA)))]
  "TARGET_BF16_SIMD"
  {
    int lane = INTVAL (operands[4]);
    gcc_assert (IN_RANGE(lane, 0, 7));
    if (lane < 4)
    {
	emit_insn (gen_neon_vfma<bt>_lanev8bf (operands[0], operands[1], operands[2], operands[3], operands[4]));
    }
    else
      {
	rtx op_highpart = gen_reg_rtx (V4BFmode);
	emit_insn (gen_neon_vget_highv8bf (op_highpart, operands[3]));
	operands[4] = GEN_INT (lane - 4);
	emit_insn (gen_neon_vfma<bt>_lanev8bf (operands[0], operands[1], operands[2], op_highpart, operands[4]));
      }
    DONE;
  }
  [(set_attr "type" "neon_fp_mla_s_scalar_q")]
)
