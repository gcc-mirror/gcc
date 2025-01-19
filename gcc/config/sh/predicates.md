;; Predicate definitions for Renesas / SuperH SH.
;; Copyright (C) 2005-2025 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Returns 1 if OP is a normal arithmetic register.
(define_predicate "arith_reg_operand"
  (match_code "subreg,reg,sign_extend")
{
  if (register_operand (op, mode))
    {
      int regno;

      if (REG_P (op))
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && REG_P (SUBREG_REG (op)))
	regno = REGNO (SUBREG_REG (op));
      else
	return 1;

      return (regno != T_REG && regno != PR_REG
	      && regno != FPUL_REG && regno != FPSCR_REG
	      && regno != MACH_REG && regno != MACL_REG);
    }
  /* Allow a no-op sign extension - compare LOAD_EXTEND_OP.
     We allow SImode here, as not using an FP register is just a matter of
     proper register allocation.  */

#if 0 /* Can't do this because of PROMOTE_MODE for unsigned vars.  */
  if (GET_MODE (op) == SImode && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (XEXP (op, 0)) == HImode
      && REG_P (XEXP (op, 0))
      && REGNO (XEXP (op, 0)) <= LAST_GENERAL_REG)
    return register_operand (XEXP (op, 0), VOIDmode);
#endif
  if (GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_INT
      && GET_CODE (op) == SUBREG
      && GET_MODE (SUBREG_REG (op)) == DImode
      && GET_CODE (SUBREG_REG (op)) == SIGN_EXTEND
      && GET_MODE (XEXP (SUBREG_REG (op), 0)) == SImode
      && GET_CODE (XEXP (SUBREG_REG (op), 0)) != SUBREG)
    return register_operand (XEXP (SUBREG_REG (op), 0), VOIDmode);
  return 0;
})

;; Like above, but for DImode destinations: forbid paradoxical DImode
;; subregs, because this would lead to missing sign extensions when
;; truncating from DImode to SImode.
(define_predicate "arith_reg_dest"
  (and (match_code "subreg,reg")
       (match_operand 0 "arith_reg_operand")))

;; Returns true if OP is a valid source operand for an arithmetic insn.
(define_predicate "arith_operand"
  (and (match_code "subreg,reg,const_int,truncate")
       (ior (match_operand 0 "arith_reg_operand")
	    (match_test "satisfies_constraint_I08 (op)"))))

;; Likewise arith_operand but always permits const_int.
(define_predicate "arith_or_int_operand"
  (and (match_code "subreg,reg,const_int,const_vector")
       (ior (match_operand 0 "arith_operand")
	    (match_operand 0 "const_int_operand"))))

;; Returns true if OP is a valid source operand for a compare insn.
(define_predicate "arith_reg_or_0_operand" 
  (and (match_code "subreg,reg,const_int,const_vector")
       (ior (match_operand 0 "arith_reg_operand")
	    (match_test "satisfies_constraint_Z (op)"))))

;; Returns true if OP is either a register or constant 0 or constant 1.
(define_predicate "arith_reg_or_0_or_1_operand"
  (and (match_code "subreg,reg,const_int,const_vector")
       (ior (match_operand 0 "arith_reg_or_0_operand")
	    (match_test "satisfies_constraint_M (op)"))))

;; Returns true if OP is a suitable constant for the minimum value of a
;; clips.b or clips.w insn.
(define_predicate "clips_min_const_int"
  (and (match_code "const_int")
       (ior (match_test "INTVAL (op) == -128")
	    (match_test "INTVAL (op) == -32768"))))

;; Returns true if OP is a suitable constant for the maximum value of a
;; clips.b or clips.w insn.
(define_predicate "clips_max_const_int"
  (and (match_code "const_int")
       (ior (match_test "INTVAL (op) == 127")
	    (match_test "INTVAL (op) == 32767"))))

;; Returns true if OP is a suitable constant for the maximum value of a
;; clipu.b or clipu.w insn.
(define_predicate "clipu_max_const_int"
  (and (match_code "const_int")
       (ior (match_test "INTVAL (op) == 255")
	    (match_test "INTVAL (op) == 65535"))))

;; Returns true if OP is a floating point register that can be used in floating
;; point arithmetic operations.
(define_predicate "fp_arith_reg_operand"
  (match_code "subreg,reg")
{
  if (register_operand (op, mode))
    {
      int regno;

      if (REG_P (op))
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && REG_P (SUBREG_REG (op)))
	regno = REGNO (SUBREG_REG (op));
      else
	return 1;

      return (regno >= FIRST_PSEUDO_REGISTER
	      || FP_REGISTER_P (regno));
    }
  return 0;
})

;; Returns true if OP is the FPSCR.
(define_predicate "fpscr_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == FPSCR_REG")))

;; Returns true if OP is a valid source operand for a FPSCR move insn.
(define_predicate "fpscr_movsrc_operand"
  (match_code "reg,subreg,mem")
{
  if (arith_reg_operand (op, mode))
    return true;

  return MEM_P (op) && GET_CODE (XEXP (op, 0)) == POST_INC;
})

;; Returns true if OP is a valid destination operand for a FPSCR move insn.
(define_predicate "fpscr_movdst_operand"
  (match_code "reg,subreg,mem")
{
  if (arith_reg_dest (op, mode))
    return true;

  return MEM_P (op) && GET_CODE (XEXP (op, 0)) == PRE_DEC;
})

;; Returns true if OP is an operand that is either the fpul hard reg or
;; a pseudo.  This prevents combine from propagating function arguments
;; in hard regs into insns that need the operand in fpul.  If it's a pseudo
;; reload can fix it up.
(define_predicate "fpul_operand"
  (match_code "reg")
{
  return REG_P (op)
	 && (REGNO (op) == FPUL_REG || REGNO (op) >= FIRST_PSEUDO_REGISTER)
	 && GET_MODE (op) == mode;
})

;; Returns true if OP is a valid fpul input operand for the fsca insn.
;; The value in fpul is a fixed-point value and its scaling is described
;; in the fsca insn by a mult:SF.  To allow pre-scaled fixed-point inputs
;; in fpul we have to permit things like
;;   (reg:SI)
;;   (fix:SF (float:SF (reg:SI)))
(define_predicate "fpul_fsca_operand"
  (match_code "fix,reg")
{
  if (fpul_operand (op, SImode))
    return true;
  if (GET_CODE (op) == FIX && GET_MODE (op) == SImode
      && GET_CODE (XEXP (op, 0)) == FLOAT && GET_MODE (XEXP (op, 0)) == SFmode)
    return fpul_fsca_operand (XEXP (XEXP (op, 0), 0),
			      GET_MODE (XEXP (XEXP (op, 0), 0)));
  return false;
})

;; Returns true if OP is a valid constant scale factor for the fsca insn.
(define_predicate "fsca_scale_factor"
  (and (match_code "const_double")
       (match_test "op == sh_fsca_int2sf ()")))

;; Returns true if OP is an operand that is zero extended during an operation.
(define_predicate "general_extend_operand"
  (match_code "subreg,reg,mem,truncate")
{
  if (reload_completed && GET_CODE (op) == TRUNCATE)
    return arith_operand (op, mode);

  if (MEM_P (op) || (GET_CODE (op) == SUBREG && MEM_P (SUBREG_REG (op))))
    return general_movsrc_operand (op, mode);

  return nonimmediate_operand (op, mode);
})

;; Returns 1 if OP is a simple register address.
(define_predicate "simple_mem_operand"
  (and (match_code "mem")
       (match_code "reg" "0")
       (match_test "arith_reg_operand (XEXP (op, 0), SImode)")))

;; Returns 1 if OP is a valid displacement address.
(define_predicate "displacement_mem_operand"
  (and (match_code "mem")
       (match_code "plus" "0")
       (match_code "reg" "00")
       (match_test "arith_reg_operand (XEXP (XEXP (op, 0), 0), SImode)")
       (match_test "sh_legitimate_index_p (GET_MODE (op),
					   XEXP (XEXP (op, 0), 1),
					   TARGET_SH2A, true)")))

;; Returns true if OP is a displacement address that can fit into a
;; 16 bit (non-SH2A) memory load / store insn.
(define_predicate "short_displacement_mem_operand"
  (and (match_code "mem")
       (match_operand 0 "displacement_mem_operand")
       (match_test "sh_disp_addr_displacement (op)
		    <= sh_max_mov_insn_displacement (GET_MODE (op), false)")))

;; Returns true if OP is a displacement address that does not fit into
;; a 16 bit (non-SH2A) memory load / store insn.
(define_predicate "long_displacement_mem_operand"
  (and (match_operand 0 "displacement_mem_operand")
       (not (match_operand 0 "short_displacement_mem_operand"))))

;; Returns true if OP is a post-increment addressing mode memory reference.
(define_predicate "post_inc_mem"
  (and (match_code "mem")
       (match_code "post_inc" "0")
       (match_code "reg" "00")))

;; Returns true if OP is a pre-decrement addressing mode memory reference.
(define_predicate "pre_dec_mem"
  (and (match_code "mem")
       (match_code "pre_dec" "0")
       (match_code "reg" "00")))

;; Returns 1 if the operand can be used in an SH2A movu.{b|w} insn.
(define_predicate "zero_extend_movu_operand"
  (and (ior (match_operand 0 "displacement_mem_operand")
	    (match_operand 0 "simple_mem_operand"))
       (ior (match_test "GET_MODE (op) == QImode")
	    (match_test "GET_MODE (op) == HImode"))))

;; Returns 1 if OP can be source of a simple move operation. Same as
;; general_operand, but a LABEL_REF is valid, PRE_DEC is invalid as
;; are subregs of system registers.
(define_predicate "general_movsrc_operand"
  (match_code "subreg,reg,const_int,const_double,mem,symbol_ref,label_ref,
	       const,const_vector")
{
  if (t_reg_operand (op, mode))
    return 0;

  if (fpscr_operand (op, mode))
    return false;

  /* Disallow PC relative QImode loads, since these is no insn to do that
     and an imm8 load should be used instead.  */
  if (IS_PC_RELATIVE_LOAD_ADDR_P (op) && GET_MODE (op) == QImode)
    return false;

  if (MEM_P (op))
    {
      rtx inside = XEXP (op, 0);

      /* Disallow mems with GBR address here.  They have to go through
	 separate special patterns.  */
      if ((REG_P (inside) && REGNO (inside) == GBR_REG)
	  || (GET_CODE (inside) == PLUS && REG_P (XEXP (inside, 0))
	      && REGNO (XEXP (inside, 0)) == GBR_REG))
	return 0;

      if (GET_CODE (inside) == CONST)
	inside = XEXP (inside, 0);

      if (GET_CODE (inside) == LABEL_REF)
	return 1;

      if (GET_CODE (inside) == PLUS
	  && GET_CODE (XEXP (inside, 0)) == LABEL_REF
	  && CONST_INT_P (XEXP (inside, 1)))
	return 1;

      /* Only post inc allowed.  */
      if (GET_CODE (inside) == PRE_DEC)
	return 0;
    }

  if (mode == GET_MODE (op)
      && (MEM_P (op) || (GET_CODE (op) == SUBREG && MEM_P (SUBREG_REG (op)))))
    {
      rtx mem_rtx = MEM_P (op) ? op : SUBREG_REG (op);
      rtx x = XEXP (mem_rtx, 0);

      if (GET_CODE (x) == PLUS)
	{
	  rtx y = XEXP (x, 0);

	  if (! REG_P (y)
	      && ! (GET_CODE (y) == SUBREG && REG_P (SUBREG_REG (y))))
	    return false;
	  y = XEXP (x, 1);
	  if (! REG_P (y)
	      && ! (GET_CODE (y) == SUBREG && REG_P (SUBREG_REG (y)))
	      && ! CONST_INT_P (y))
	    return false;
	}

      /* LRA will try to satisfy the constraints for the memory displacements
	 and thus we must not reject invalid displacements in the predicate,
	 or else LRA will bail out.
	 FIXME: maybe remove this check completely?  */
      if (!lra_in_progress && (mode == QImode || mode == HImode)
	  && GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && CONST_INT_P (XEXP (x, 1)))
	return sh_legitimate_index_p (mode, XEXP (x, 1), TARGET_SH2A, false);

      /* Allow reg+reg addressing here without validating the register
	 numbers.  Usually one of the regs must be R0 or a pseudo reg.
	 In some cases it can happen that arguments from hard regs are
	 propagated directly into address expressions.  In this cases reload
	 will have to fix it up later.  However, allow this only for native
	 1, 2 or 4 byte addresses.  */
      if (can_create_pseudo_p () && GET_CODE (x) == PLUS
	  && GET_MODE_SIZE (mode) <= 4
	  && REG_P (XEXP (x, 0)) && REG_P (XEXP (x, 1)))
	return true;

      /* 'general_operand' does not allow volatile mems during RTL expansion to
	 avoid matching arithmetic that operates on mems, it seems.
	 On SH this leads to redundant sign extensions for QImode or HImode
	 loads.  Thus we mimic the behavior but allow volatile mems.  */
        if (memory_address_addr_space_p (GET_MODE (mem_rtx), x,
					 MEM_ADDR_SPACE (mem_rtx)))
	  return true;
    }

  return general_operand (op, mode);
})

;; Returns true if OP is a MEM that does not use displacement addressing.
(define_predicate "movsrc_no_disp_mem_operand"
  (and (match_code "mem")
       (match_operand 0 "general_movsrc_operand")
       (match_test "satisfies_constraint_Snd (op)")))

;; Returns 1 if OP can be a destination of a move. Same as
;; general_operand, but no preinc allowed.
(define_predicate "general_movdst_operand"
  (match_code "subreg,reg,mem")
{
  if (t_reg_operand (op, mode))
    return 0;

  if (fpscr_operand (op, mode))
    return false;

  if (MEM_P (op))
    {
      rtx inside = XEXP (op, 0);
      /* Disallow mems with GBR address here.  They have to go through
	 separate special patterns.  */
      if ((REG_P (inside) && REGNO (inside) == GBR_REG)
	  || (GET_CODE (inside) == PLUS && REG_P (XEXP (inside, 0))
	      && REGNO (XEXP (inside, 0)) == GBR_REG))
	return 0;
    }

  /* Only pre dec allowed.  */
  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == POST_INC)
    return 0;

  if (mode == GET_MODE (op)
      && (MEM_P (op) || (GET_CODE (op) == SUBREG && MEM_P (SUBREG_REG (op)))))
    {
      rtx mem_rtx = MEM_P (op) ? op : SUBREG_REG (op);
      rtx x = XEXP (mem_rtx, 0);

      if (GET_CODE (x) == PLUS)
	{
	  rtx y = XEXP (x, 0);

	  if (! REG_P (y)
	      && ! (GET_CODE (y) == SUBREG && REG_P (SUBREG_REG (y))))
	    return false;
	  y = XEXP (x, 1);
	  if (! REG_P (y)
	      && ! (GET_CODE (y) == SUBREG && REG_P (SUBREG_REG (y)))
	      && ! CONST_INT_P (y))
	    return false;
	}

      /* LRA will try to satisfy the constraints for the memory displacements
	 and thus we must not reject invalid displacements in the predicate,
	 or else LRA will bail out.
	 FIXME: maybe remove this check completely?  */
      if (!lra_in_progress && (mode == QImode || mode == HImode)
	  && GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && CONST_INT_P (XEXP (x, 1)))
	return sh_legitimate_index_p (mode, XEXP (x, 1), TARGET_SH2A, false);

      /* Allow reg+reg addressing here without validating the register
	 numbers.  Usually one of the regs must be R0 or a pseudo reg.
	 In some cases it can happen that arguments from hard regs are
	 propagated directly into address expressions.  In this cases reload
	 will have to fix it up later.  However, allow this only for native
	 1, 2 or 4 byte addresses.  */
      if (can_create_pseudo_p () && GET_CODE (x) == PLUS
	  && GET_MODE_SIZE (mode) <= 4
	  && REG_P (XEXP (x, 0)) && REG_P (XEXP (x, 1)))
	return true;

      /* 'general_operand' does not allow volatile mems during RTL expansion to
	 avoid matching arithmetic that operates on mems, it seems.
	 On SH this leads to redundant sign extensions for QImode or HImode
	 stores.  Thus we mimic the behavior but allow volatile mems.  */
        if (memory_address_addr_space_p (GET_MODE (mem_rtx), x,
					 MEM_ADDR_SPACE (mem_rtx)))
	  return true;
    }

  return general_operand (op, mode);
})

;; Returns 1 if OP is a MEM that can be source of a simple move operation.
(define_predicate "unaligned_load_operand"
  (match_code "mem")
{
  rtx inside;

  if (!MEM_P (op) || GET_MODE (op) != mode)
    return 0;

  inside = XEXP (op, 0);

  if (GET_CODE (inside) == POST_INC)
    inside = XEXP (inside, 0);

  if (REG_P (inside))
    return 1;

  return 0;
})

;; Returns 1 if OP is a MEM that can be used in "index_disp" combiner
;; patterns.
(define_predicate "mem_index_disp_operand"
  (match_code "mem")
{
  rtx plus0_rtx, plus1_rtx, mult_rtx;

  plus0_rtx = XEXP (op, 0);
  if (GET_CODE (plus0_rtx) != PLUS)
    return 0;

  plus1_rtx = XEXP (plus0_rtx, 0);
  if (GET_CODE (plus1_rtx) != PLUS)
    return 0;
  if (! arith_reg_operand (XEXP (plus1_rtx, 1), GET_MODE (XEXP (plus1_rtx, 1))))
    return 0;

  mult_rtx = XEXP (plus1_rtx, 0);
  if (GET_CODE (mult_rtx) != MULT)
    return 0;
  if (! arith_reg_operand (XEXP (mult_rtx, 0), GET_MODE (XEXP (mult_rtx, 0)))
      || ! CONST_INT_P (XEXP (mult_rtx, 1)))
    return 0;

  return exact_log2 (INTVAL (XEXP (mult_rtx, 1))) > 0
	 && sh_legitimate_index_p (mode, XEXP (plus0_rtx, 1), TARGET_SH2A, true);
})

;; Returns true if OP is a valid source operand for a logical operation.
(define_predicate "logical_operand"
  (and (match_code "subreg,reg,const_int")
       (ior (match_operand 0 "arith_reg_operand")
	    (match_test "satisfies_constraint_K08 (op)"))))

;; Returns true if OP is a valid constant source operand for a logical
;; operations tst/and/or/xor #imm,r0.
(define_predicate "const_logical_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_K08 (op)")))

;; Like logical_operand but allows additional constant values which can be
;; done with zero extensions.  Used for the second operand of and insns.
(define_predicate "logical_and_operand"
  (and (match_code "subreg,reg,const_int")
       (ior (match_operand 0 "logical_operand")
	    (match_test "satisfies_constraint_Jmb (op)")
	    (match_test "satisfies_constraint_Jmw (op)"))))

;; Returns true if OP is a logical operator.
(define_predicate "logical_operator"
  (match_code "and,ior,xor"))

;; Returns true if OP is a constant vector.
(define_predicate "sh_const_vec"
  (match_code "const_vector")
{
  for (int i = XVECLEN (op, 0) - 1; i >= 0; i--)
    if (!CONST_INT_P (XVECEXP (op, 0, i)))
      return false;
  return true;
})

;; Determine if OP is a constant vector matching MODE with only one
;; element that is not a sign extension.  Two byte-sized elements
;; count as one.
(define_predicate "sh_1el_vec"
  (match_code "const_vector")
{
  /* Determine numbers of last and of least significant elements.  */
  int last = XVECLEN (op, 0) - 1;
  int least = TARGET_LITTLE_ENDIAN ? 0 : last;
  if (!CONST_INT_P (XVECEXP (op, 0, least)))
    return false;
  int sign_ix = least;
  if (GET_MODE_UNIT_SIZE (mode) == 1)
    sign_ix = TARGET_LITTLE_ENDIAN ? 1 : last - 1;
  if (!CONST_INT_P (XVECEXP (op, 0, sign_ix)))
    return false;
  int unit_size = GET_MODE_UNIT_SIZE (GET_MODE (op));
  rtx sign = INTVAL (XVECEXP (op, 0, sign_ix)) >> (unit_size * BITS_PER_UNIT - 1)
	     ? constm1_rtx : const0_rtx;
  int i = XVECLEN (op, 0) - 1;
  do
    if (i != least && i != sign_ix && XVECEXP (op, 0, i) != sign)
      return 0;
  while (--i);
  return true;
})

;; Returns true if OP is a vector which is composed of one element that is
;; repeated.
(define_predicate "sh_rep_vec"
  (match_code "const_vector,parallel")
{
  int i = XVECLEN (op, 0) - 2;
  rtx x = XVECEXP (op, 0, i + 1);
  if (GET_MODE_UNIT_SIZE (mode) == 1)
    {
      rtx y = XVECEXP (op, 0, i);
      for (i -= 2; i >= 0; i -= 2)
	if (! rtx_equal_p (XVECEXP (op, 0, i + 1), x)
	    || ! rtx_equal_p (XVECEXP (op, 0, i), y))
	  return false;
    }
  else
    for (; i >= 0; i--)
      if (XVECEXP (op, 0, i) != x)
	return false;
  return true;
})

;; Returns true if OP is a valid shift count operand for shift operations.
(define_predicate "shift_count_operand"
  (match_code "const_int,const_double,const,symbol_ref,label_ref,subreg,reg,
	       zero_extend,sign_extend")
{
  /* Allow T_REG as shift count for dynamic shifts, although it is not
     really possible.  It will then be copied to a general purpose reg.  */
  return const_int_operand (op, mode) || arith_reg_operand (op, mode)
	 || (TARGET_DYNSHIFT && t_reg_operand (op, mode));
})

;; Predicates for matching operands that are constant shift
;; amounts 1, 2, 8, 16.
(define_predicate "p27_shift_count_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_P27 (op)")))

(define_predicate "not_p27_shift_count_operand"
  (and (match_code "const_int")
       (match_test "! satisfies_constraint_P27 (op)")))

;; For right shifts the constant 1 is a special case because the shlr insn
;; clobbers the T_REG and is handled by the T_REG clobbering version of the
;; insn, which is also used for non-P27 shift sequences.
(define_predicate "p27_rshift_count_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_P27 (op)")
       (match_test "! satisfies_constraint_M (op)")))

(define_predicate "not_p27_rshift_count_operand"
  (and (match_code "const_int")
       (ior (match_test "! satisfies_constraint_P27 (op)")
	    (match_test "satisfies_constraint_M (op)"))))

;; Returns true if OP is a symbol reference.
(define_predicate "symbol_ref_operand"
  (match_code "symbol_ref"))

(define_predicate "bitwise_memory_operand"
  (match_code "mem")
{
  if (MEM_P (op))
    {
      if (REG_P (XEXP (op, 0)))
	return 1;

      if (GET_CODE (XEXP (op, 0)) == PLUS
	  && REG_P (XEXP (XEXP (op, 0), 0))
	  && satisfies_constraint_K12 (XEXP (XEXP (op, 0), 1)))
        return 1;
    }
  return 0;
})

;; A predicate that matches any expression for which there is an
;; insn pattern that sets the T bit.
(define_predicate "treg_set_expr"
  (match_test "sh_recog_treg_set_expr (op, mode)"))

;; Same as treg_set_expr but disallow constants 0 and 1 which can be loaded
;; into the T bit.
(define_predicate "treg_set_expr_not_const01"
  (and (match_test "op != const0_rtx")
       (match_test "op != const1_rtx")
       (match_operand 0 "treg_set_expr")))

;; A predicate describing the T bit register in any form.
(define_predicate "t_reg_operand"
  (match_code "reg,subreg,sign_extend,zero_extend,ne,eq")
{
  switch (GET_CODE (op))
    {
      case EQ:
	return t_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)))
	       && XEXP (op, 1) == const1_rtx;

      case NE:
	return t_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)))
	       && XEXP (op, 1) == const0_rtx;

      case REG:
	return REGNO (op) == T_REG;

      case SUBREG:
	return REG_P (SUBREG_REG (op)) && REGNO (SUBREG_REG (op)) == T_REG;

      case ZERO_EXTEND:
      case SIGN_EXTEND:
        if (REG_P (XEXP (op, 0)) && REGNO (XEXP (op, 0)) == T_REG)
	  return true;
	return GET_CODE (XEXP (op, 0)) == SUBREG
	       && REG_P (SUBREG_REG (XEXP (op, 0)))
	       && REGNO (SUBREG_REG (XEXP (op, 0))) == T_REG;

      default:
	return 0;
    }
})

;; A predicate describing a negated T bit register.
(define_predicate "negt_reg_operand"
  (match_code "subreg,xor,ne,eq")
{
  switch (GET_CODE (op))
    {
      case EQ:
	return t_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)))
	       && XEXP (op, 1) == const0_rtx;

      case NE:
	return t_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)))
	       && XEXP (op, 1) == const1_rtx;

      case XOR:
	return t_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)))
	       && XEXP (op, 1) == const1_rtx;

      case SUBREG:
	return negt_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)));

      default:
	return 0;
    }
})

;; Returns true if OP is an operand that can be used as the first operand in
;; the cstoresi4 expander pattern.
(define_predicate "cmpsi_operand"
  (and (match_code "subreg,reg,const_int")
       (ior (match_operand:SI 0 "t_reg_operand")
	    (match_operand 0 "arith_operand"))))

;; A predicate that returns true if OP is a valid construct around the T bit
;; that can be used as an operand for conditional branches.
(define_predicate "cbranch_treg_value"
  (and (match_code "eq,ne,reg,subreg,xor,sign_extend,zero_extend")
       (match_test "sh_eval_treg_value (op) >= 0")))

;; Returns true if OP is arith_reg_operand or t_reg_operand.
(define_predicate "arith_reg_or_t_reg_operand"
  (ior (match_operand 0 "arith_reg_operand")
       (match_operand 0 "t_reg_operand")))

(define_predicate "arith_reg_or_treg_set_expr"
  (ior (match_operand 0 "arith_reg_operand")
       (match_operand 0 "treg_set_expr")))

;; A predicate describing the negated value of the T bit register shifted
;; left by 31.
(define_predicate "negt_reg_shl31_operand"
  (match_code "plus,minus,if_then_else")
{
  /* (minus:SI (const_int -2147483648)  ;; 0xffffffff80000000
	       (ashift:SI (match_operand:SI 1 "t_reg_operand")
			  (const_int 31)))
  */
  if (GET_CODE (op) == MINUS && satisfies_constraint_Jhb (XEXP (op, 0))
      && GET_CODE (XEXP (op, 1)) == ASHIFT
      && t_reg_operand (XEXP (XEXP (op, 1), 0), SImode)
      && CONST_INT_P (XEXP (XEXP (op, 1), 1))
      && INTVAL (XEXP (XEXP (op, 1), 1)) == 31)
    return true;

  /* (plus:SI (ashift:SI (match_operand:SI 1 "t_reg_operand")
			 (const_int 31))
	      (const_int -2147483648))  ;; 0xffffffff80000000
  */
  if (GET_CODE (op) == PLUS && satisfies_constraint_Jhb (XEXP (op, 1))
      && GET_CODE (XEXP (op, 0)) == ASHIFT
      && t_reg_operand (XEXP (XEXP (op, 0), 0), SImode)
      && CONST_INT_P (XEXP (XEXP (op, 0), 1))
      && INTVAL (XEXP (XEXP (op, 0), 1)) == 31)
    return true;

  /* (plus:SI (mult:SI (match_operand:SI 1 "t_reg_operand")
		       (const_int -2147483648))  ;; 0xffffffff80000000
	      (const_int -2147483648))
  */
  if (GET_CODE (op) == PLUS && satisfies_constraint_Jhb (XEXP (op, 1))
      && GET_CODE (XEXP (op, 0)) == MULT
      && t_reg_operand (XEXP (XEXP (op, 0), 0), SImode)
      && satisfies_constraint_Jhb (XEXP (XEXP (op, 0), 1)))
    return true;

  /* (minus:SI (const_int -2147483648)  ;; 0xffffffff80000000
	       (mult:SI (match_operand:SI 1 "t_reg_operand")
			(const_int -2147483648)))
  */
  if (GET_CODE (op) == MINUS
      && satisfies_constraint_Jhb (XEXP (op, 0))
      && GET_CODE (XEXP (op, 1)) == MULT
      && t_reg_operand (XEXP (XEXP (op, 1), 0), SImode)
      && satisfies_constraint_Jhb (XEXP (XEXP (op, 1), 1)))
    return true;

  /*  (if_then_else:SI (match_operand:SI 1 "t_reg_operand")
		       (const_int 0)
		       (const_int -2147483648))  ;; 0xffffffff80000000
  */
  if (GET_CODE (op) == IF_THEN_ELSE && t_reg_operand (XEXP (op, 0), SImode)
      && satisfies_constraint_Z (XEXP (op, 1))
      && satisfies_constraint_Jhb (XEXP (op, 2)))
    return true;

  return false;
})

;; A predicate that determines whether a given constant is a valid
;; displacement for a GBR load/store of the specified mode.
(define_predicate "gbr_displacement"
  (match_code "const_int")
{
  const int mode_sz = GET_MODE_SIZE (mode);
  const int move_sz = mode_sz > GET_MODE_SIZE (SImode)
				? GET_MODE_SIZE (SImode)
				: mode_sz;
  int max_disp = 255 * move_sz;
  if (mode_sz > move_sz)
    max_disp -= mode_sz - move_sz;

  return INTVAL (op) >= 0 && INTVAL (op) <= max_disp;
})

;; A predicate that determines whether OP is a valid GBR addressing mode
;; memory reference.
(define_predicate "gbr_address_mem"
  (match_code "mem")
{
  rtx addr = XEXP (op, 0);

  if (REG_P (addr) && REGNO (addr) == GBR_REG)
    return true;
  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && REGNO (XEXP (addr, 0)) == GBR_REG
      && gbr_displacement (XEXP (addr, 1), mode))
    return true;

  return false;
})
