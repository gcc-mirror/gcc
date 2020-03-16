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

(define_mode_iterator MVE_types [V16QI V8HI V4SI V2DI TI V8HF V4SF V2DF])
(define_mode_attr V_sz_elem2 [(V16QI "s8") (V8HI "u16") (V4SI "u32")
			      (V2DI "u64")])

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
