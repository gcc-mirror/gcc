;; Expander definitions for vector support between altivec & vsx.  No
;; instructions are in this file, this file provides the generic vector
;; expander, and the actual vector instructions will be in altivec.md and
;; vsx.md

;; Copyright (C) 2009-2013 Free Software Foundation, Inc.
;; Contributed by Michael Meissner <meissner@linux.vnet.ibm.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Vector int modes
(define_mode_iterator VEC_I [V16QI V8HI V4SI V2DI])

;; Vector float modes
(define_mode_iterator VEC_F [V4SF V2DF])

;; Vector arithmetic modes
(define_mode_iterator VEC_A [V16QI V8HI V4SI V2DI V4SF V2DF])

;; Vector modes that need alginment via permutes
(define_mode_iterator VEC_K [V16QI V8HI V4SI V4SF])

;; Vector logical modes
(define_mode_iterator VEC_L [V16QI V8HI V4SI V2DI V4SF V2DF TI])

;; Vector modes for moves.  Don't do TImode here.
(define_mode_iterator VEC_M [V16QI V8HI V4SI V2DI V4SF V2DF])

;; Vector modes for types that don't need a realignment under VSX
(define_mode_iterator VEC_N [V4SI V4SF V2DI V2DF])

;; Vector comparison modes
(define_mode_iterator VEC_C [V16QI V8HI V4SI V2DI V4SF V2DF])

;; Vector init/extract modes
(define_mode_iterator VEC_E [V16QI V8HI V4SI V2DI V4SF V2DF])

;; Vector modes for 64-bit base types
(define_mode_iterator VEC_64 [V2DI V2DF])

;; Vector reload iterator
(define_mode_iterator VEC_R [V16QI V8HI V4SI V2DI V4SF V2DF SF SD SI DF DD DI TI])

;; Base type from vector mode
(define_mode_attr VEC_base [(V16QI "QI")
			    (V8HI  "HI")
			    (V4SI  "SI")
			    (V2DI  "DI")
			    (V4SF  "SF")
			    (V2DF  "DF")
			    (TI    "TI")])

;; Same size integer type for floating point data
(define_mode_attr VEC_int [(V4SF  "v4si")
			   (V2DF  "v2di")])

(define_mode_attr VEC_INT [(V4SF  "V4SI")
			   (V2DF  "V2DI")])

;; constants for unspec
(define_c_enum "unspec" [UNSPEC_PREDICATE
			 UNSPEC_REDUC])

;; Vector reduction code iterators
(define_code_iterator VEC_reduc [plus smin smax])

(define_code_attr VEC_reduc_name [(plus "splus")
				  (smin "smin")
				  (smax "smax")])

(define_code_attr VEC_reduc_rtx [(plus "add")
				 (smin "smin")
				 (smax "smax")])


;; Vector move instructions.  Little-endian VSX loads and stores require
;; special handling to circumvent "element endianness."
(define_expand "mov<mode>"
  [(set (match_operand:VEC_M 0 "nonimmediate_operand" "")
	(match_operand:VEC_M 1 "any_operand" ""))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (can_create_pseudo_p ())
    {
      if (CONSTANT_P (operands[1])
	  && !easy_vector_constant (operands[1], <MODE>mode))
	operands[1] = force_const_mem (<MODE>mode, operands[1]);

      else if (!vlogical_operand (operands[0], <MODE>mode)
	       && !vlogical_operand (operands[1], <MODE>mode))
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
  if (!BYTES_BIG_ENDIAN
      && VECTOR_MEM_VSX_P (<MODE>mode)
      && <MODE>mode != TImode
      && !gpr_or_gpr_p (operands[0], operands[1])
      && (memory_operand (operands[0], <MODE>mode)
          ^ memory_operand (operands[1], <MODE>mode)))
    {
      rs6000_emit_le_vsx_move (operands[0], operands[1], <MODE>mode);
      DONE;
    }
})

;; Generic vector floating point load/store instructions.  These will match
;; insns defined in vsx.md or altivec.md depending on the switches.
(define_expand "vector_load_<mode>"
  [(set (match_operand:VEC_M 0 "vfloat_operand" "")
	(match_operand:VEC_M 1 "memory_operand" ""))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_store_<mode>"
  [(set (match_operand:VEC_M 0 "memory_operand" "")
	(match_operand:VEC_M 1 "vfloat_operand" ""))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; Splits if a GPR register was chosen for the move
(define_split
  [(set (match_operand:VEC_L 0 "nonimmediate_operand" "")
        (match_operand:VEC_L 1 "input_operand" ""))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)
   && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])
   && !direct_move_p (operands[0], operands[1])
   && !quad_load_store_p (operands[0], operands[1])"
  [(pc)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
})

;; Vector floating point load/store instructions that uses the Altivec
;; instructions even if we are compiling for VSX, since the Altivec
;; instructions silently ignore the bottom 3 bits of the address, and VSX does
;; not.
(define_expand "vector_altivec_load_<mode>"
  [(set (match_operand:VEC_M 0 "vfloat_operand" "")
	(match_operand:VEC_M 1 "memory_operand" ""))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  gcc_assert (VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode));

  if (VECTOR_MEM_VSX_P (<MODE>mode))
    {
      operands[1] = rs6000_address_for_altivec (operands[1]);
      emit_insn (gen_altivec_lvx_<mode> (operands[0], operands[1]));
      DONE;
    }
}")

(define_expand "vector_altivec_store_<mode>"
  [(set (match_operand:VEC_M 0 "memory_operand" "")
	(match_operand:VEC_M 1 "vfloat_operand" ""))]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  gcc_assert (VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode));

  if (VECTOR_MEM_VSX_P (<MODE>mode))
    {
      operands[0] = rs6000_address_for_altivec (operands[0]);
      emit_insn (gen_altivec_stvx_<mode> (operands[0], operands[1]));
      DONE;
    }
}")



;; Reload patterns for vector operations.  We may need an additional base
;; register to convert the reg+offset addressing to reg+reg for vector
;; registers and reg+reg or (reg+reg)&(-16) addressing to just an index
;; register for gpr registers.
(define_expand "reload_<VEC_R:mode>_<P:mptrsize>_store"
  [(parallel [(match_operand:VEC_R 0 "memory_operand" "m")
              (match_operand:VEC_R 1 "gpc_reg_operand" "r")
              (match_operand:P 2 "register_operand" "=&b")])]
  "<P:tptrsize>"
{
  rs6000_secondary_reload_inner (operands[1], operands[0], operands[2], true);
  DONE;
})

(define_expand "reload_<VEC_R:mode>_<P:mptrsize>_load"
  [(parallel [(match_operand:VEC_R 0 "gpc_reg_operand" "=&r")
              (match_operand:VEC_R 1 "memory_operand" "m")
              (match_operand:P 2 "register_operand" "=&b")])]
  "<P:tptrsize>"
{
  rs6000_secondary_reload_inner (operands[0], operands[1], operands[2], false);
  DONE;
})

;; Reload sometimes tries to move the address to a GPR, and can generate
;; invalid RTL for addresses involving AND -16.  Allow addresses involving
;; reg+reg, reg+small constant, or just reg, all wrapped in an AND -16.

(define_insn_and_split "*vec_reload_and_plus_<mptrsize>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=b")
	(and:P (plus:P (match_operand:P 1 "gpc_reg_operand" "r")
		       (match_operand:P 2 "reg_or_cint_operand" "rI"))
	       (const_int -16)))]
  "(TARGET_ALTIVEC || TARGET_VSX) && (reload_in_progress || reload_completed)"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(plus:P (match_dup 1)
		(match_dup 2)))
   (parallel [(set (match_dup 0)
		   (and:P (match_dup 0)
			  (const_int -16)))
	      (clobber:CC (scratch:CC))])])

;; The normal ANDSI3/ANDDI3 won't match if reload decides to move an AND -16
;; address to a register because there is no clobber of a (scratch), so we add
;; it here.
(define_insn_and_split "*vec_reload_and_reg_<mptrsize>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=b")
	(and:P (match_operand:P 1 "gpc_reg_operand" "r")
	       (const_int -16)))]
  "(TARGET_ALTIVEC || TARGET_VSX) && (reload_in_progress || reload_completed)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (and:P (match_dup 1)
			  (const_int -16)))
	      (clobber:CC (scratch:CC))])])

;; Generic floating point vector arithmetic support
(define_expand "add<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(plus:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
		    (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "sub<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(minus:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
		     (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "mul<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(mult:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
		    (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_mulv4sf3 (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_expand "div<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(div:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
		   (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "")

(define_expand "neg<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(neg:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_negv4sf2 (operands[0], operands[1]));
      DONE;
    }
}")

(define_expand "abs<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(abs:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_absv4sf2 (operands[0], operands[1]));
      DONE;
    }
}")

(define_expand "smin<mode>3"
  [(set (match_operand:VEC_F 0 "register_operand" "")
        (smin:VEC_F (match_operand:VEC_F 1 "register_operand" "")
		    (match_operand:VEC_F 2 "register_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "smax<mode>3"
  [(set (match_operand:VEC_F 0 "register_operand" "")
        (smax:VEC_F (match_operand:VEC_F 1 "register_operand" "")
		    (match_operand:VEC_F 2 "register_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")


(define_expand "sqrt<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(sqrt:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")))]
  "VECTOR_UNIT_VSX_P (<MODE>mode)"
  "")

(define_expand "rsqrte<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
        (unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand" "")]
		      UNSPEC_RSQRT))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "re<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand" "f")]
		      UNSPEC_FRES))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "ftrunc<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
  	(fix:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_ceil<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand" "")]
		      UNSPEC_FRIP))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_floor<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand" "")]
		      UNSPEC_FRIM))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_btrunc<mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(fix:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_copysign<mode>3"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(unspec:VEC_F [(match_operand:VEC_F 1 "vfloat_operand" "")
		       (match_operand:VEC_F 2 "vfloat_operand" "")] UNSPEC_COPYSIGN))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_copysign_v4sf3 (operands[0], operands[1],
					     operands[2]));
      DONE;
    }
}")


;; Vector comparisons
(define_expand "vcond<mode><mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(if_then_else:VEC_F
	 (match_operator 3 "comparison_operator"
			 [(match_operand:VEC_F 4 "vfloat_operand" "")
			  (match_operand:VEC_F 5 "vfloat_operand" "")])
	 (match_operand:VEC_F 1 "vfloat_operand" "")
	 (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    FAIL;
}")

(define_expand "vcond<mode><mode>"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(if_then_else:VEC_I
	 (match_operator 3 "comparison_operator"
			 [(match_operand:VEC_I 4 "vint_operand" "")
			  (match_operand:VEC_I 5 "vint_operand" "")])
	 (match_operand:VEC_I 1 "vint_operand" "")
	 (match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    FAIL;
}")

(define_expand "vcondv4sfv4si"
  [(set (match_operand:V4SF 0 "vfloat_operand" "")
	(if_then_else:V4SF
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V4SI 4 "vint_operand" "")
			  (match_operand:V4SI 5 "vint_operand" "")])
	 (match_operand:V4SF 1 "vfloat_operand" "")
	 (match_operand:V4SF 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
   && VECTOR_UNIT_ALTIVEC_P (V4SImode)"
  "
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    FAIL;
}")

(define_expand "vcondv4siv4sf"
  [(set (match_operand:V4SI 0 "vint_operand" "")
	(if_then_else:V4SI
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V4SF 4 "vfloat_operand" "")
			  (match_operand:V4SF 5 "vfloat_operand" "")])
	 (match_operand:V4SI 1 "vint_operand" "")
	 (match_operand:V4SI 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
   && VECTOR_UNIT_ALTIVEC_P (V4SImode)"
  "
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    FAIL;
}")

(define_expand "vcondu<mode><mode>"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(if_then_else:VEC_I
	 (match_operator 3 "comparison_operator"
			 [(match_operand:VEC_I 4 "vint_operand" "")
			  (match_operand:VEC_I 5 "vint_operand" "")])
	 (match_operand:VEC_I 1 "vint_operand" "")
	 (match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    FAIL;
}")

(define_expand "vconduv4sfv4si"
  [(set (match_operand:V4SF 0 "vfloat_operand" "")
	(if_then_else:V4SF
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V4SI 4 "vint_operand" "")
			  (match_operand:V4SI 5 "vint_operand" "")])
	 (match_operand:V4SF 1 "vfloat_operand" "")
	 (match_operand:V4SF 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
   && VECTOR_UNIT_ALTIVEC_P (V4SImode)"
  "
{
  if (rs6000_emit_vector_cond_expr (operands[0], operands[1], operands[2],
				    operands[3], operands[4], operands[5]))
    DONE;
  else
    FAIL;
}")

(define_expand "vector_eq<mode>"
  [(set (match_operand:VEC_C 0 "vlogical_operand" "")
	(eq:VEC_C (match_operand:VEC_C 1 "vlogical_operand" "")
		  (match_operand:VEC_C 2 "vlogical_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_gt<mode>"
  [(set (match_operand:VEC_C 0 "vlogical_operand" "")
	(gt:VEC_C (match_operand:VEC_C 1 "vlogical_operand" "")
		  (match_operand:VEC_C 2 "vlogical_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_ge<mode>"
  [(set (match_operand:VEC_C 0 "vlogical_operand" "")
	(ge:VEC_C (match_operand:VEC_C 1 "vlogical_operand" "")
		  (match_operand:VEC_C 2 "vlogical_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_gtu<mode>"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(gtu:VEC_I (match_operand:VEC_I 1 "vint_operand" "")
		   (match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_geu<mode>"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(geu:VEC_I (match_operand:VEC_I 1 "vint_operand" "")
		   (match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_insn_and_split "*vector_uneq<mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(uneq:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
		    (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "#"
  ""
  [(set (match_dup 3)
	(gt:VEC_F (match_dup 1)
		  (match_dup 2)))
   (set (match_dup 4)
	(gt:VEC_F (match_dup 2)
		  (match_dup 1)))
   (set (match_dup 0)
	(not:VEC_F (ior:VEC_F (match_dup 3)
			      (match_dup 4))))]
  "
{
  operands[3] = gen_reg_rtx (<MODE>mode);
  operands[4] = gen_reg_rtx (<MODE>mode);
}")

(define_insn_and_split "*vector_ltgt<mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(ltgt:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
		    (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "#"
  ""
  [(set (match_dup 3)
	(gt:VEC_F (match_dup 1)
		  (match_dup 2)))
   (set (match_dup 4)
	(gt:VEC_F (match_dup 2)
		  (match_dup 1)))
   (set (match_dup 0)
	(ior:VEC_F (match_dup 3)
		   (match_dup 4)))]
  "
{
  operands[3] = gen_reg_rtx (<MODE>mode);
  operands[4] = gen_reg_rtx (<MODE>mode);
}")

(define_insn_and_split "*vector_ordered<mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(ordered:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
		       (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "#"
  ""
  [(set (match_dup 3)
	(ge:VEC_F (match_dup 1)
		  (match_dup 2)))
   (set (match_dup 4)
	(ge:VEC_F (match_dup 2)
		  (match_dup 1)))
   (set (match_dup 0)
	(ior:VEC_F (match_dup 3)
		   (match_dup 4)))]
  "
{
  operands[3] = gen_reg_rtx (<MODE>mode);
  operands[4] = gen_reg_rtx (<MODE>mode);
}")

(define_insn_and_split "*vector_unordered<mode>"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(unordered:VEC_F (match_operand:VEC_F 1 "vfloat_operand" "")
			 (match_operand:VEC_F 2 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "#"
  ""
  [(set (match_dup 3)
	(ge:VEC_F (match_dup 1)
		  (match_dup 2)))
   (set (match_dup 4)
	(ge:VEC_F (match_dup 2)
		  (match_dup 1)))
   (set (match_dup 0)
	(not:VEC_F (ior:VEC_F (match_dup 3)
			      (match_dup 4))))]
  "
{
  operands[3] = gen_reg_rtx (<MODE>mode);
  operands[4] = gen_reg_rtx (<MODE>mode);
}")

;; Note the arguments for __builtin_altivec_vsel are op2, op1, mask
;; which is in the reverse order that we want
(define_expand "vector_select_<mode>"
  [(set (match_operand:VEC_L 0 "vlogical_operand" "")
	(if_then_else:VEC_L
	 (ne:CC (match_operand:VEC_L 3 "vlogical_operand" "")
		(match_dup 4))
	 (match_operand:VEC_L 2 "vlogical_operand" "")
	 (match_operand:VEC_L 1 "vlogical_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "operands[4] = CONST0_RTX (<MODE>mode);")

(define_expand "vector_select_<mode>_uns"
  [(set (match_operand:VEC_L 0 "vlogical_operand" "")
	(if_then_else:VEC_L
	 (ne:CCUNS (match_operand:VEC_L 3 "vlogical_operand" "")
		   (match_dup 4))
	 (match_operand:VEC_L 2 "vlogical_operand" "")
	 (match_operand:VEC_L 1 "vlogical_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "operands[4] = CONST0_RTX (<MODE>mode);")

;; Expansions that compare vectors producing a vector result and a predicate,
;; setting CR6 to indicate a combined status
(define_expand "vector_eq_<mode>_p"
  [(parallel
    [(set (reg:CC 74)
	  (unspec:CC [(eq:CC (match_operand:VEC_A 1 "vlogical_operand" "")
			     (match_operand:VEC_A 2 "vlogical_operand" ""))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_A 0 "vlogical_operand" "")
	  (eq:VEC_A (match_dup 1)
		    (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_gt_<mode>_p"
  [(parallel
    [(set (reg:CC 74)
	  (unspec:CC [(gt:CC (match_operand:VEC_A 1 "vlogical_operand" "")
			     (match_operand:VEC_A 2 "vlogical_operand" ""))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_A 0 "vlogical_operand" "")
	  (gt:VEC_A (match_dup 1)
		    (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_ge_<mode>_p"
  [(parallel
    [(set (reg:CC 74)
	  (unspec:CC [(ge:CC (match_operand:VEC_F 1 "vfloat_operand" "")
			     (match_operand:VEC_F 2 "vfloat_operand" ""))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_F 0 "vfloat_operand" "")
	  (ge:VEC_F (match_dup 1)
		    (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

(define_expand "vector_gtu_<mode>_p"
  [(parallel
    [(set (reg:CC 74)
	  (unspec:CC [(gtu:CC (match_operand:VEC_I 1 "vint_operand" "")
			      (match_operand:VEC_I 2 "vint_operand" ""))]
		     UNSPEC_PREDICATE))
     (set (match_operand:VEC_I 0 "vlogical_operand" "")
	  (gtu:VEC_I (match_dup 1)
		     (match_dup 2)))])]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; AltiVec/VSX predicates.

(define_expand "cr6_test_for_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC 74)
	       (const_int 0)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")

(define_expand "cr6_test_for_zero_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC 74)
	       (const_int 0)))
   (set (match_dup 0) (minus:SI (const_int 1) (match_dup 0)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")

(define_expand "cr6_test_for_lt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC 74)
	       (const_int 0)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")

(define_expand "cr6_test_for_lt_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC 74)
	       (const_int 0)))
   (set (match_dup 0) (minus:SI (const_int 1) (match_dup 0)))]
  "TARGET_ALTIVEC || TARGET_VSX"
  "")


;; Vector count leading zeros
(define_expand "clz<mode>2"
  [(set (match_operand:VEC_I 0 "register_operand" "")
	(clz:VEC_I (match_operand:VEC_I 1 "register_operand" "")))]
  "TARGET_P8_VECTOR")

;; Vector population count
(define_expand "popcount<mode>2"
  [(set (match_operand:VEC_I 0 "register_operand" "")
        (popcount:VEC_I (match_operand:VEC_I 1 "register_operand" "")))]
  "TARGET_P8_VECTOR")


;; Same size conversions
(define_expand "float<VEC_int><mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(float:VEC_F (match_operand:<VEC_INT> 1 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vcfsx (operands[0], operands[1], const0_rtx));
      DONE;
    }
}")

(define_expand "floatuns<VEC_int><mode>2"
  [(set (match_operand:VEC_F 0 "vfloat_operand" "")
	(unsigned_float:VEC_F (match_operand:<VEC_INT> 1 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vcfux (operands[0], operands[1], const0_rtx));
      DONE;
    }
}")

(define_expand "fix_trunc<mode><VEC_int>2"
  [(set (match_operand:<VEC_INT> 0 "vint_operand" "")
	(fix:<VEC_INT> (match_operand:VEC_F 1 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vctsxs (operands[0], operands[1], const0_rtx));
      DONE;
    }
}")

(define_expand "fixuns_trunc<mode><VEC_int>2"
  [(set (match_operand:<VEC_INT> 0 "vint_operand" "")
	(unsigned_fix:<VEC_INT> (match_operand:VEC_F 1 "vfloat_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "
{
  if (<MODE>mode == V4SFmode && VECTOR_UNIT_ALTIVEC_P (<MODE>mode))
    {
      emit_insn (gen_altivec_vctuxs (operands[0], operands[1], const0_rtx));
      DONE;
    }
}")


;; Vector initialization, set, extract
(define_expand "vec_init<mode>"
  [(match_operand:VEC_E 0 "vlogical_operand" "")
   (match_operand:VEC_E 1 "" "")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rs6000_expand_vector_init (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_set<mode>"
  [(match_operand:VEC_E 0 "vlogical_operand" "")
   (match_operand:<VEC_base> 1 "register_operand" "")
   (match_operand 2 "const_int_operand" "")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rs6000_expand_vector_set (operands[0], operands[1], INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_extract<mode>"
  [(match_operand:<VEC_base> 0 "register_operand" "")
   (match_operand:VEC_E 1 "vlogical_operand" "")
   (match_operand 2 "const_int_operand" "")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  rs6000_expand_vector_extract (operands[0], operands[1],
				INTVAL (operands[2]));
  DONE;
})

;; Convert double word types to single word types
(define_expand "vec_pack_trunc_v2df"
  [(match_operand:V4SF 0 "vfloat_operand" "")
   (match_operand:V2DF 1 "vfloat_operand" "")
   (match_operand:V2DF 2 "vfloat_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && TARGET_ALTIVEC"
{
  rtx r1 = gen_reg_rtx (V4SFmode);
  rtx r2 = gen_reg_rtx (V4SFmode);

  emit_insn (gen_vsx_xvcvdpsp (r1, operands[1]));
  emit_insn (gen_vsx_xvcvdpsp (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

(define_expand "vec_pack_sfix_trunc_v2df"
  [(match_operand:V4SI 0 "vint_operand" "")
   (match_operand:V2DF 1 "vfloat_operand" "")
   (match_operand:V2DF 2 "vfloat_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && TARGET_ALTIVEC"
{
  rtx r1 = gen_reg_rtx (V4SImode);
  rtx r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_vsx_xvcvdpsxws (r1, operands[1]));
  emit_insn (gen_vsx_xvcvdpsxws (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

(define_expand "vec_pack_ufix_trunc_v2df"
  [(match_operand:V4SI 0 "vint_operand" "")
   (match_operand:V2DF 1 "vfloat_operand" "")
   (match_operand:V2DF 2 "vfloat_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && TARGET_ALTIVEC"
{
  rtx r1 = gen_reg_rtx (V4SImode);
  rtx r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_vsx_xvcvdpuxws (r1, operands[1]));
  emit_insn (gen_vsx_xvcvdpuxws (r2, operands[2]));
  rs6000_expand_extract_even (operands[0], r1, r2);
  DONE;
})

;; Convert single word types to double word
(define_expand "vec_unpacks_hi_v4sf"
  [(match_operand:V2DF 0 "vfloat_operand" "")
   (match_operand:V4SF 1 "vfloat_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)"
{
  rtx reg = gen_reg_rtx (V4SFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvspdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_lo_v4sf"
  [(match_operand:V2DF 0 "vfloat_operand" "")
   (match_operand:V4SF 1 "vfloat_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)"
{
  rtx reg = gen_reg_rtx (V4SFmode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvspdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_float_hi_v4si"
  [(match_operand:V2DF 0 "vfloat_operand" "")
   (match_operand:V4SI 1 "vint_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvsxwdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacks_float_lo_v4si"
  [(match_operand:V2DF 0 "vfloat_operand" "")
   (match_operand:V4SI 1 "vint_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvsxwdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacku_float_hi_v4si"
  [(match_operand:V2DF 0 "vfloat_operand" "")
   (match_operand:V4SI 1 "vint_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvuxwdp (operands[0], reg));
  DONE;
})

(define_expand "vec_unpacku_float_lo_v4si"
  [(match_operand:V2DF 0 "vfloat_operand" "")
   (match_operand:V4SI 1 "vint_operand" "")]
  "VECTOR_UNIT_VSX_P (V2DFmode) && VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SImode)"
{
  rtx reg = gen_reg_rtx (V4SImode);

  rs6000_expand_interleave (reg, operands[1], operands[1], !BYTES_BIG_ENDIAN);
  emit_insn (gen_vsx_xvcvuxwdp (operands[0], reg));
  DONE;
})


;; Align vector loads with a permute.
(define_expand "vec_realign_load_<mode>"
  [(match_operand:VEC_K 0 "vlogical_operand" "")
   (match_operand:VEC_K 1 "vlogical_operand" "")
   (match_operand:VEC_K 2 "vlogical_operand" "")
   (match_operand:V16QI 3 "vlogical_operand" "")]
  "VECTOR_MEM_ALTIVEC_OR_VSX_P (<MODE>mode)"
{
  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_altivec_vperm_<mode> (operands[0], operands[1],
    	      				 operands[2], operands[3]));
  else
    {
      /* We have changed lvsr to lvsl, so to complete the transformation
         of vperm for LE, we must swap the inputs.  */
      rtx unspec = gen_rtx_UNSPEC (<MODE>mode,
                                   gen_rtvec (3, operands[2],
                                              operands[1], operands[3]),
                                   UNSPEC_VPERM);
      emit_move_insn (operands[0], unspec);
    }
  DONE;
})

;; Under VSX, vectors of 4/8 byte alignments do not need to be aligned
;; since the load already handles it.
(define_expand "movmisalign<mode>"
 [(set (match_operand:VEC_N 0 "nonimmediate_operand" "")
       (match_operand:VEC_N 1 "any_operand" ""))]
 "VECTOR_MEM_VSX_P (<MODE>mode) && TARGET_ALLOW_MOVMISALIGN"
 "")


;; Vector shift left in bits.  Currently supported ony for shift
;; amounts that can be expressed as byte shifts (divisible by 8).
;; General shift amounts can be supported using vslo + vsl. We're
;; not expecting to see these yet (the vectorizer currently
;; generates only shifts divisible by byte_size).
(define_expand "vec_shl_<mode>"
  [(match_operand:VEC_L 0 "vlogical_operand" "")
   (match_operand:VEC_L 1 "vlogical_operand" "")
   (match_operand:QI 2 "reg_or_short_operand" "")]
  "TARGET_ALTIVEC"
  "
{
  rtx bitshift = operands[2];
  rtx shift;
  rtx insn;
  HOST_WIDE_INT bitshift_val;
  HOST_WIDE_INT byteshift_val;

  if (! CONSTANT_P (bitshift))
    FAIL;
  bitshift_val = INTVAL (bitshift);
  if (bitshift_val & 0x7)
    FAIL;
  byteshift_val = bitshift_val >> 3;
  if (TARGET_VSX && (byteshift_val & 0x3) == 0)
    {
      shift = gen_rtx_CONST_INT (QImode, byteshift_val >> 2);
      insn = gen_vsx_xxsldwi_<mode> (operands[0], operands[1], operands[1],
				     shift);
    }
  else
    {
      shift = gen_rtx_CONST_INT (QImode, byteshift_val);
      insn = gen_altivec_vsldoi_<mode> (operands[0], operands[1], operands[1],
					shift);
    }

  emit_insn (insn);
  DONE;
}")

;; Vector shift right in bits. Currently supported ony for shift
;; amounts that can be expressed as byte shifts (divisible by 8).
;; General shift amounts can be supported using vsro + vsr. We're
;; not expecting to see these yet (the vectorizer currently
;; generates only shifts divisible by byte_size).
(define_expand "vec_shr_<mode>"
  [(match_operand:VEC_L 0 "vlogical_operand" "")
   (match_operand:VEC_L 1 "vlogical_operand" "")
   (match_operand:QI 2 "reg_or_short_operand" "")]
  "TARGET_ALTIVEC"
  "
{
  rtx bitshift = operands[2];
  rtx shift;
  rtx insn;
  HOST_WIDE_INT bitshift_val;
  HOST_WIDE_INT byteshift_val;

  if (! CONSTANT_P (bitshift))
    FAIL;
  bitshift_val = INTVAL (bitshift);
  if (bitshift_val & 0x7)
    FAIL;
  byteshift_val = 16 - (bitshift_val >> 3);
  if (TARGET_VSX && (byteshift_val & 0x3) == 0)
    {
      shift = gen_rtx_CONST_INT (QImode, byteshift_val >> 2);
      insn = gen_vsx_xxsldwi_<mode> (operands[0], operands[1], operands[1],
				     shift);
    }
  else
    {
      shift = gen_rtx_CONST_INT (QImode, byteshift_val);
      insn = gen_altivec_vsldoi_<mode> (operands[0], operands[1], operands[1],
					shift);
    }

  emit_insn (insn);
  DONE;
}")

;; Expanders for rotate each element in a vector
(define_expand "vrotl<mode>3"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(rotate:VEC_I (match_operand:VEC_I 1 "vint_operand" "")
		      (match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; Expanders for arithmetic shift left on each vector element
(define_expand "vashl<mode>3"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(ashift:VEC_I (match_operand:VEC_I 1 "vint_operand" "")
		      (match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; Expanders for logical shift right on each vector element
(define_expand "vlshr<mode>3"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(lshiftrt:VEC_I (match_operand:VEC_I 1 "vint_operand" "")
			(match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; Expanders for arithmetic shift right on each vector element
(define_expand "vashr<mode>3"
  [(set (match_operand:VEC_I 0 "vint_operand" "")
	(ashiftrt:VEC_I (match_operand:VEC_I 1 "vint_operand" "")
			(match_operand:VEC_I 2 "vint_operand" "")))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode)"
  "")

;; Vector reduction expanders for VSX

(define_expand "reduc_<VEC_reduc_name>_v2df"
  [(parallel [(set (match_operand:V2DF 0 "vfloat_operand" "")
		   (VEC_reduc:V2DF
		    (vec_concat:V2DF
		     (vec_select:DF
		      (match_operand:V2DF 1 "vfloat_operand" "")
		      (parallel [(const_int 1)]))
		     (vec_select:DF
		      (match_dup 1)
		      (parallel [(const_int 0)])))
		    (match_dup 1)))
	      (clobber (match_scratch:V2DF 2 ""))])]
  "VECTOR_UNIT_VSX_P (V2DFmode)"
  "")

; The (VEC_reduc:V4SF
;	(op1)
;	(unspec:V4SF [(const_int 0)] UNSPEC_REDUC))
;
; is to allow us to use a code iterator, but not completely list all of the
; vector rotates, etc. to prevent canonicalization

(define_expand "reduc_<VEC_reduc_name>_v4sf"
  [(parallel [(set (match_operand:V4SF 0 "vfloat_operand" "")
		   (VEC_reduc:V4SF
		    (unspec:V4SF [(const_int 0)] UNSPEC_REDUC)
		    (match_operand:V4SF 1 "vfloat_operand" "")))
	      (clobber (match_scratch:V4SF 2 ""))
	      (clobber (match_scratch:V4SF 3 ""))])]
  "VECTOR_UNIT_VSX_P (V4SFmode)"
  "")


;;; Expanders for vector insn patterns shared between the SPE and TARGET_PAIRED systems.

(define_expand "absv2sf2"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "")
	(abs:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "")))]
  "TARGET_PAIRED_FLOAT || TARGET_SPE"
  "")

(define_expand "negv2sf2"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "")
	(neg:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "")))]
  "TARGET_PAIRED_FLOAT || TARGET_SPE"
  "")

(define_expand "addv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "")
	(plus:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "")
		   (match_operand:V2SF 2 "gpc_reg_operand" "")))]
  "TARGET_PAIRED_FLOAT || TARGET_SPE"
  "
{
  if (TARGET_SPE)
    {
      /* We need to make a note that we clobber SPEFSCR.  */
      rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));

      XVECEXP (par, 0, 0) = gen_rtx_SET (VOIDmode, operands[0],
                                         gen_rtx_PLUS (V2SFmode, operands[1], operands[2]));
      XVECEXP (par, 0, 1) = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, SPEFSCR_REGNO));
      emit_insn (par);
      DONE;
    }
}")

(define_expand "subv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "")
	(minus:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "")
		    (match_operand:V2SF 2 "gpc_reg_operand" "")))]
  "TARGET_PAIRED_FLOAT || TARGET_SPE"
  "
{
  if (TARGET_SPE)
    {
      /* We need to make a note that we clobber SPEFSCR.  */
      rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));

      XVECEXP (par, 0, 0) = gen_rtx_SET (VOIDmode, operands[0],
                                         gen_rtx_MINUS (V2SFmode, operands[1], operands[2]));
      XVECEXP (par, 0, 1) = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, SPEFSCR_REGNO));
      emit_insn (par);
      DONE;
    }
}")

(define_expand "mulv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "")
	(mult:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "")
		   (match_operand:V2SF 2 "gpc_reg_operand" "")))]
  "TARGET_PAIRED_FLOAT || TARGET_SPE"
  "
{
  if (TARGET_SPE)
    {
      /* We need to make a note that we clobber SPEFSCR.  */
      rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));

      XVECEXP (par, 0, 0) = gen_rtx_SET (VOIDmode, operands[0],
                                         gen_rtx_MULT (V2SFmode, operands[1], operands[2]));
      XVECEXP (par, 0, 1) = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, SPEFSCR_REGNO));
      emit_insn (par);
      DONE;
    }
}")

(define_expand "divv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "")
	(div:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "")
		  (match_operand:V2SF 2 "gpc_reg_operand" "")))]
  "TARGET_PAIRED_FLOAT || TARGET_SPE"
  "
{
  if (TARGET_SPE)
    {
      /* We need to make a note that we clobber SPEFSCR.  */
      rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));

      XVECEXP (par, 0, 0) = gen_rtx_SET (VOIDmode, operands[0],
                                         gen_rtx_DIV (V2SFmode, operands[1], operands[2]));
      XVECEXP (par, 0, 1) = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, SPEFSCR_REGNO));
      emit_insn (par);
      DONE;
    }
}")
