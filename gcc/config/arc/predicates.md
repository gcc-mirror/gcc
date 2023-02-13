;; Predicate definitions for Synopsys DesignWare ARC.
;; Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

(define_predicate "dest_reg_operand"
  (match_code "reg,subreg")
{
  return register_operand (op, mode);
})

(define_predicate "mpy_dest_reg_operand"
  (match_code "reg,subreg")
{
  return register_operand (op, mode);
})


;; Returns 1 if OP is a symbol reference.
(define_predicate "symbolic_operand"
  (match_code "symbol_ref, label_ref, const")
)

;; Acceptable arguments to the call insn.
(define_predicate "call_address_operand"
  (ior (match_code "const_int, reg")
       (match_operand 0 "symbolic_operand")
       (match_test "CONSTANT_P (op)
		    && arc_legitimate_constant_p (VOIDmode, op)"))
)

(define_predicate "call_operand"
  (and (match_code "mem")
       (match_test "call_address_operand (XEXP (op, 0), mode)"))
)

;; Return true if OP is a unsigned 6-bit immediate (u6) value.
(define_predicate "u6_immediate_operand"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (INTVAL (op))"))
)

;; Return true if OP is a short immediate (shimm) value.
(define_predicate "short_immediate_operand"
  (and (match_code "const_int")
       (match_test "SMALL_INT (INTVAL (op))"))
)

(define_predicate "p2_immediate_operand"
  (and (match_code "const_int")
       (match_test "((INTVAL (op) - 1) & INTVAL (op)) == 0")
       (match_test "INTVAL (op)"))
)

;; Return true if OP will require a long immediate (limm) value.
;; This is currently only used when calculating length attributes.
(define_predicate "long_immediate_operand"
  (match_code "symbol_ref, label_ref, const, const_double, const_int")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
    case LABEL_REF :
    case CONST :
      return 1;
    case CONST_INT :
      return !SIGNED_INT12 (INTVAL (op));
    case CONST_DOUBLE :
      /* These can happen because large unsigned 32 bit constants are
	 represented this way (the multiplication patterns can cause these
	 to be generated).  They also occur for SFmode values.  */
      return 1;
    default:
      break;
    }
  return 0;
}
)

;; Return true if OP is a MEM that when used as a load or store address will
;; require an 8 byte insn.
;; Load and store instructions don't allow the same possibilities but they're
;; similar enough that this one function will do.
;; This is currently only used when calculating length attributes.  */
(define_predicate "long_immediate_loadstore_operand"
  (match_code "mem")
{
  int size = GET_MODE_SIZE (GET_MODE (op));

  op = XEXP (op, 0);
  if (TARGET_NPS_CMEM && cmem_address (op, SImode))
    return 0;
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
    case LABEL_REF :
    case CONST :
      return 1;
    case CONST_INT :
      /* This must be handled as "st c,[limm]".  Ditto for load.
	 Technically, the assembler could translate some possibilities to
	 "st c,[limm/2 + limm/2]" if limm/2 will fit in a shimm, but we don't
	 assume that it does.  */
      return 1;
    case CONST_DOUBLE :
      /* These can happen because large unsigned 32 bit constants are
	 represented this way (the multiplication patterns can cause these
	 to be generated).  They also occur for SFmode values.  */
      return 1;
    case REG :
      return 0;
    case PLUS :
      {
	rtx x = XEXP (op, 1);

	if ((GET_CODE (XEXP (op, 0)) == MULT)
	    && REG_P (XEXP (XEXP (op, 0), 0))
	    && CONSTANT_P (x))
	  return 1;

	if (GET_CODE (x) == CONST)
	  {
	    x = XEXP (x, 0);
	    if (GET_CODE (x) == PLUS)
	      x = XEXP (x, 0);
	  }
	if (CONST_INT_P (x))
	  return (!SMALL_INT (INTVAL (x))
		  && (size <= 1 || size > 4
		      || (INTVAL (x) & (size - 1)) != 0
		      || !SMALL_INT (INTVAL (x) / size)));
	else if (GET_CODE (x) == SYMBOL_REF)
	  return TARGET_NO_SDATA_SET || !SYMBOL_REF_SMALL_P (x);
	return 0;
      }
    default:
      break;
    }
  return 0;
}
)

;; Return true if OP is any of R0-R3,R12-R15 for ARCompact 16-bit
;; instructions
(define_predicate "compact_register_operand"
  (match_code "reg, subreg")
  {
     if ((GET_MODE (op) != mode) && (mode != VOIDmode))
	 return 0;

      return (GET_CODE (op) == REG)
      && (REGNO (op) >= FIRST_PSEUDO_REGISTER
		|| COMPACT_GP_REG_P (REGNO (op))) ;
  }
)

(define_predicate "compact_hreg_operand"
  (match_code "reg, subreg")
  {
     if ((GET_MODE (op) != mode) && (mode != VOIDmode))
	 return 0;

      return (GET_CODE (op) == REG)
      && (REGNO (op) >= FIRST_PSEUDO_REGISTER
		|| (TARGET_V2 && REGNO (op) <= 31 && REGNO (op) != 30)
		|| !TARGET_V2);
  }
)

;; Return true if OP is an acceptable memory operand for ARCompact
;; 16-bit store instructions
(define_predicate "compact_store_memory_operand"
  (match_code "mem")
{
  rtx addr, plus0, plus1;
  int size, off;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* .di instructions have no 16-bit form.  */
  if (MEM_VOLATILE_P (op) && !TARGET_VOLATILE_CACHE_SET)
     return 0;

  /* likewise for uncached types.  */
  if (arc_is_uncached_mem_p (op))
     return 0;

  size = GET_MODE_SIZE (mode);

  /* dword operations really put out 2 instructions, so eliminate them.  */
  if (size > UNITS_PER_WORD)
    return 0;

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    case REG:
      return (REGNO (addr) >= FIRST_PSEUDO_REGISTER
		|| COMPACT_GP_REG_P (REGNO (addr))
	      || (SP_REG_P (REGNO (addr)) && (size != 2)));
	/* stw_s does not support SP as a parameter.  */
    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);

      if ((GET_CODE (plus0) == REG)
	  && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
	      || COMPACT_GP_REG_P (REGNO (plus0)))
	  && (GET_CODE (plus1) == CONST_INT))
	{
	  off = INTVAL (plus1);

	  /* Negative offset is not supported in 16-bit load/store insns.  */
	  if (off < 0)
	    return 0;

	  switch (size)
	    {
	    case 1:
	      return (off < 32);
	    case 2:
	      return ((off < 64) && (off % 2 == 0));
	    case 4:
	      return ((off < 128) && (off % 4 == 0));
	    }
	}

      if ((GET_CODE (plus0) == REG)
	  && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
	      || SP_REG_P (REGNO (plus0)))
	  && (GET_CODE (plus1) == CONST_INT))
	{
	  off = INTVAL (plus1);

	  return ((size != 2) && (off >= 0 && off < 128) && (off % 4 == 0));
	}
    default:
      break;
    }
  return 0;
  }
)

;; Return true if OP is an acceptable argument for a single word
;;   move source.
(define_predicate "move_src_operand"
  (match_code "symbol_ref, label_ref, const, const_int, const_double, reg, subreg, mem")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
      if (SYMBOL_REF_TLS_MODEL (op))
	return 0;
      return 1;
    case LABEL_REF :
      return 1;
    case CONST :
      return arc_legitimate_constant_p (mode, op);
    case CONST_INT :
      return (LARGE_INT (INTVAL (op)));
    case CONST_DOUBLE :
      /* We can handle DImode integer constants in SImode if the value
	 (signed or unsigned) will fit in 32 bits.  This is needed because
	 large unsigned 32 bit constants are represented as CONST_DOUBLEs.  */
      if (mode == SImode)
	return arc_double_limm_p (op);
      /* We can handle 32 bit floating point constants.  */
      if (mode == SFmode)
	return GET_MODE (op) == SFmode;
      return 0;
    case REG :
      if (REGNO (op) == LP_COUNT)
	return 1;
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return register_operand (op, mode);
    case MEM :
      return address_operand (XEXP (op, 0), mode);
    default :
      return 0;
    }
}
)

;; Return true if OP is an acceptable argument for a double word
;; move source.
(define_predicate "move_double_src_operand"
  (match_code "reg, subreg, mem, const_int, const_double")
{
  switch (GET_CODE (op))
    {
    case REG :
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return register_operand (op, mode);
    case MEM :
      return address_operand (XEXP (op, 0), mode);
    case CONST_INT :
    case CONST_DOUBLE :
      return 1;
    default :
      return 0;
    }
}
)

;; Return true if OP is an acceptable argument for a move destination.
(define_predicate "move_dest_operand"
  (match_code "reg, subreg, mem")
{
  switch (GET_CODE (op))
    {
    case REG :
     /* Program Counter register cannot be the target of a move.  It is
	 a readonly register.  */
      if (REGNO (op) == PCL_REG)
	return 0;
      else if (TARGET_MULMAC_32BY16_SET
	       && (REGNO (op) == MUL32x16_REG || REGNO (op) == R57_REG))
	return 0;
      else if (TARGET_MUL64_SET
	       && (REGNO (op) == R57_REG || REGNO (op) == MUL64_OUT_REG
		   || REGNO (op) == R59_REG))
	return 0;
      else if (REGNO (op) == LP_COUNT)
        return 1;
      else
	return dest_reg_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return dest_reg_operand (op, mode);
    case MEM :
      {
	rtx addr = XEXP (op, 0);

	if (GET_CODE (addr) == PLUS
	    && (GET_CODE (XEXP (addr, 0)) == MULT
		|| (!CONST_INT_P (XEXP (addr, 1))
		    && (TARGET_NO_SDATA_SET
			|| GET_CODE (XEXP (addr, 1)) != SYMBOL_REF
			|| !SYMBOL_REF_SMALL_P (XEXP (addr, 1))))))
	  return 0;
	if ((GET_CODE (addr) == PRE_MODIFY || GET_CODE (addr) == POST_MODIFY)
	    && (GET_CODE (XEXP (addr, 1)) != PLUS
		|| !CONST_INT_P (XEXP (XEXP (addr, 1), 1))))
	  return 0;
	/* CONST_INT / CONST_DOUBLE is fine, but the PIC CONST ([..] UNSPEC))
	   constructs are effectively indexed.  */
	if (flag_pic)
	  {
	    rtx ad0 = addr;
	    while (GET_CODE (ad0) == PLUS)
	      ad0 = XEXP (ad0, 0);
	    if (GET_CODE (ad0) == CONST || GET_CODE (ad0) == UNSPEC)
	      return 0;
	  }
	return address_operand (addr, mode);
      }
    default :
      return 0;
    }

}
)

;; Return true if OP is a non-volatile non-immediate operand.
;; Volatile memory refs require a special "cache-bypass" instruction
;; and only the standard movXX patterns are set up to handle them.
(define_predicate "nonvol_nonimm_operand"
  (and (match_code "subreg, reg, mem")
       (match_test "(GET_CODE (op) != MEM || !MEM_VOLATILE_P (op)) && nonimmediate_operand (op, mode)")
       (match_test "!arc_is_uncached_mem_p (op)"))
)

;; Return 1 if OP is a comparison operator valid for the mode of CC.
;; This allows the use of MATCH_OPERATOR to recognize all the branch insns.

(define_predicate "proper_comparison_operator"
  (match_code "eq, ne, le, lt, ge, gt, leu, ltu, geu, gtu, unordered, ordered, uneq, unge, ungt, unle, unlt, ltgt")
{
  enum rtx_code code = GET_CODE (op);

  if (!COMPARISON_P (op))
    return 0;

  /* After generic flag-setting insns, we can use eq / ne / pl / mi / pnz .
     There are some creative uses for hi / ls after shifts, but these are
     hard to understand for the compiler and could be at best the target of
     a peephole.  */
  switch (GET_MODE (XEXP (op, 0)))
    {
    case E_CC_ZNmode:
      return (code == EQ || code == NE || code == GE || code == LT
	      || code == GT);
    case E_CC_Zmode:
      return code == EQ || code == NE;
    case E_CC_Cmode:
      return code == LTU || code == GEU;
    case E_CC_FP_GTmode:
      return code == GT || code == UNLE;
    case E_CC_FP_GEmode:
      return code == GE || code == UNLT;
    case E_CC_FP_ORDmode:
      return code == ORDERED || code == UNORDERED;
    case E_CC_FP_UNEQmode:
      return code == UNEQ || code == LTGT;
    case E_CC_FPXmode:
      return (code == EQ || code == NE || code == UNEQ || code == LTGT
	      || code == ORDERED || code == UNORDERED);

    case E_CC_FPUmode:
    case E_CC_FPUEmode:
      return 1;
    case E_CC_FPU_UNEQmode:
      return 1;

    case E_CCmode:
    case E_SImode: /* Used for BRcc.  */
      return 1;
    /* From combiner.  */
    case E_QImode: case E_HImode: case E_DImode: case E_SFmode: case E_DFmode:
      return 0;
    case E_VOIDmode:
      return 0;
    default:
      gcc_unreachable ();
  }
})

(define_predicate "equality_comparison_operator"
  (match_code "eq, ne"))

(define_predicate "ge_lt_comparison_operator"
  (match_code "ge, lt"))

(define_predicate "brcc_nolimm_operator"
  (ior (match_test "REG_P (XEXP (op, 1))")
       (and (match_code "eq, ne, lt, ge, ltu, geu")
	    (match_test "CONST_INT_P (XEXP (op, 1))")
	    (match_test "u6_immediate_operand (XEXP (op, 1), SImode)"))
       (and (match_code "le, gt, leu, gtu")
	    (match_test "CONST_INT_P (XEXP (op, 1))")
	    (match_test "UNSIGNED_INT6 (INTVAL (XEXP (op, 1)) + 1)"))))

;; Return TRUE if this is the condition code register, if we aren't given
;; a mode, accept any CCmode register
(define_special_predicate "cc_register"
  (match_code "reg")
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (op);
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return FALSE;
    }

  if (mode == GET_MODE (op) && GET_CODE (op) == REG && REGNO (op) == CC_REG)
    return TRUE;

  return FALSE;
})

;; Return TRUE if this is the condition code register; if we aren't given
;; a mode, accept any CCmode register.  If we are given a mode, accept
;; modes that set a subset of flags.
(define_special_predicate "cc_set_register"
  (match_code "reg")
{
  machine_mode rmode = GET_MODE (op);

  if (mode == VOIDmode)
    {
      mode = rmode;
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return FALSE;
    }

  if (REGNO (op) != CC_REG)
    return FALSE;
  if (mode == rmode
      || (mode == CC_ZNmode && rmode == CC_Zmode)
      || (mode == CCmode && rmode == CC_Zmode)
      || (mode == CCmode && rmode == CC_ZNmode)
      || (mode == CCmode && rmode == CC_Cmode))
    return TRUE;

  return FALSE;
})

; Accept CC_REG in modes which provide the flags needed for MODE.  */
(define_special_predicate "cc_use_register"
  (match_code "reg")
{
  if (REGNO (op) != CC_REG)
    return 0;
  if (GET_MODE (op) == mode)
    return 1;
  switch (mode)
    {
    case E_CC_Zmode:
      if (GET_MODE (op) == CC_ZNmode)
	return 1;
      /* Fall through.  */
    case E_CC_ZNmode: case E_CC_Cmode:
      return GET_MODE (op) == CCmode;
    default:
      gcc_unreachable ();
    }
})

(define_special_predicate "zn_compare_operator"
  (match_code "compare")
{
  return GET_MODE (op) == CC_ZNmode || GET_MODE (op) == CC_Zmode;
})

;; Return true if OP is a shift operator.
(define_predicate "shift_operator"
  (match_code "ashiftrt, lshiftrt, ashift")
)

;; Return true if OP is a left shift operator that can be implemented in
;; four insn words or less without a barrel shifter or multiplier.
(define_predicate "shiftl4_operator"
  (and (match_code "ashift")
       (match_test "const_int_operand (XEXP (op, 1), VOIDmode) ")
       (match_test "UINTVAL (XEXP (op, 1)) <= 9U
		    || INTVAL (XEXP (op, 1)) == 29
		    || INTVAL (XEXP (op, 1)) == 30
		    || INTVAL (XEXP (op, 1)) == 31")))

;; Return true if OP is a right shift operator that can be implemented in
;; four insn words or less without a barrel shifter or multiplier.
(define_predicate "shiftr4_operator"
  (and (match_code "ashiftrt, lshiftrt")
       (match_test "const_int_operand (XEXP (op, 1), VOIDmode) ")
       (match_test "UINTVAL (XEXP (op, 1)) <= 4U
		    || INTVAL (XEXP (op, 1)) == 30
		    || INTVAL (XEXP (op, 1)) == 31")))

;; Return true if OP is a shift operator that can be implemented in
;; four insn words or less without a barrel shifter or multiplier.
(define_predicate "shift4_operator"
  (ior (match_operand 0 "shiftl4_operator")
       (match_operand 0 "shiftr4_operator")))

(define_predicate "mult_operator"
    (and (match_code "mult") (match_test "TARGET_MPY"))
)

(define_predicate "commutative_operator"
  (ior (match_code "plus,ior,xor,and")
       (match_operand 0 "mult_operator")
       (and (match_code "ss_plus")
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "commutative_operator_sans_mult"
  (ior (match_code "plus,ior,xor,and")
       (and (match_code "ss_plus")
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "noncommutative_operator"
  (ior (and (match_code "ashift,ashiftrt,lshiftrt,rotatert")
	    (match_test "TARGET_BARREL_SHIFTER"))
       (match_code "minus")
       (and (match_code "ss_minus")
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "unary_operator"
  (ior (match_code "abs,neg,not,sign_extend,zero_extend")
       (and (ior (match_code "ss_neg")
		 (and (match_code "ss_truncate")
		      (match_test "GET_MODE (XEXP (op, 0)) == HImode")))
	    (match_test "TARGET_ARC700 || TARGET_EA_SET")))
)

(define_predicate "_1_2_3_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 2 || INTVAL (op) == 3"))
)

(define_predicate "_2_4_8_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4 || INTVAL (op) == 8"))
)

(define_predicate "arc_double_register_operand"
  (match_code "reg")
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
    return 0;

  return (GET_CODE (op) == REG
		   && (REGNO (op) >= FIRST_PSEUDO_REGISTER
			     || REGNO_REG_CLASS (REGNO (op)) == DOUBLE_REGS));
})

(define_predicate "shouldbe_register_operand"
  (match_code "reg,subreg,mem")
{
  return ((reload_in_progress || reload_completed)
	  ? general_operand : register_operand) (op, mode);
})

(define_predicate "vector_register_operand"
  (match_code "reg")
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
    return 0;

  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == SIMD_VR_REGS));
})

(define_predicate "vector_register_or_memory_operand"
  ( ior (match_code "reg")
	(match_code "mem"))
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
    return 0;

  if ((GET_CODE (op) == MEM)
      && (mode == V8HImode)
      && GET_CODE (XEXP (op,0)) == REG)
    return 1;

  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == SIMD_VR_REGS));
})

(define_predicate "arc_dpfp_operator"
  (match_code "plus, mult,minus")
)

(define_predicate "arc_simd_dma_register_operand"
  (match_code "reg")
{
  if ((GET_MODE (op) != mode) && (mode != VOIDmode))
    return 0;

  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == SIMD_DMA_CONFIG_REGS));
})

(define_predicate "acc1_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == (TARGET_BIG_ENDIAN ? 56 : 57)")))

(define_predicate "acc2_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == (TARGET_BIG_ENDIAN ? 57 : 56)")))

(define_predicate "mlo_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == R58_REG")))

(define_predicate "mhi_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == R59_REG")))

(define_predicate "accl_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == (TARGET_BIG_ENDIAN ? 59 : 58)")
       (match_test "TARGET_V2")))

; Unfortunately, we cannot allow a const_int_operand before reload, because
; reload needs a non-void mode to guide it how to reload the inside of a
; {sign_}extend.
(define_predicate "extend_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "immediate_operand")
	    (ior (not (match_operand 0 "const_int_operand"))
		 (match_test "reload_in_progress || reload_completed")))))

(define_predicate "millicode_store_operation"
  (match_code "parallel")
{
  return arc_check_millicode (op, 0, 0);
})

(define_predicate "millicode_load_operation"
  (match_code "parallel")
{
  return arc_check_millicode (op, 2, 2);
})

(define_predicate "millicode_load_clob_operation"
  (match_code "parallel")
{
  return arc_check_millicode (op, 0, 1);
})

(define_special_predicate "immediate_usidi_operand"
  (if_then_else
    (match_code "const_int")
    (match_test "INTVAL (op) >= 0")
    (and (match_test "const_double_operand (op, mode)")
	 (match_test "CONST_DOUBLE_HIGH (op) == 0"))))

(define_predicate "short_const_int_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_C16 (op)")))

(define_predicate "mem_noofs_operand"
  (and (match_code "mem")
       (match_code "reg" "0")))

(define_predicate "any_mem_operand"
  (match_code "mem"))

; Special predicate to match even-odd double register pair
(define_predicate "even_register_operand"
  (match_code "reg")
  {
   if ((GET_MODE (op) != mode) && (mode != VOIDmode))
      return 0;

   return (REG_P (op) && ((REGNO (op) >= FIRST_PSEUDO_REGISTER)
			  || ((REGNO (op) & 1) == 0)));
  })

(define_predicate "double_register_operand"
  (ior (match_test "even_register_operand (op, mode)")
       (match_test "arc_double_register_operand (op, mode)")))

(define_predicate "cmem_address_0"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_FLAGS (op) & SYMBOL_FLAG_CMEM")))

(define_predicate "cmem_address_1"
  (and (match_code "plus")
       (match_test "cmem_address_0 (XEXP (op, 0), SImode)")))

(define_predicate "cmem_address_2"
  (and (match_code "const")
       (match_test "cmem_address_1 (XEXP (op, 0), SImode)")))

(define_predicate "cmem_address"
  (ior (match_operand:SI 0 "cmem_address_0")
       (match_operand:SI 0 "cmem_address_1")
       (match_operand:SI 0 "cmem_address_2")))

(define_predicate "short_unsigned_const_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_J16 (op)")))

(define_predicate "arc_short_operand"
  (ior (match_test "register_operand (op, mode)")
       (match_test "short_unsigned_const_operand (op, mode)")))

(define_special_predicate "push_multi_operand"
  (match_code "parallel")
  {
   return arc_check_multi (op, true);
})

(define_special_predicate "pop_multi_operand"
  (match_code "parallel")
  {
   return arc_check_multi (op, false);
})

(define_predicate "arc_nonmemory_operand"
  (ior (match_test "register_operand (op, mode)")
       (and (match_code "const_int, symbol_ref")
	    (match_test "!optimize_size"))))
