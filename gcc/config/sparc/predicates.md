;; Predicate definitions for SPARC.
;; Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

;; Predicates for numerical constants.

;; Return true if OP is the zero constant for MODE.
(define_predicate "const_zero_operand"
  (and (match_code "const_int,const_wide_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; Return true if the integer representation of OP is all ones.
(define_predicate "const_all_ones_operand"
  (and (match_code "const_int,const_wide_int,const_double,const_vector")
       (match_test "INTEGRAL_MODE_P (GET_MODE (op))")
       (match_test "op == CONSTM1_RTX (GET_MODE (op))")))

;; Return true if OP is the integer constant 4096.
(define_predicate "const_4096_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 4096")))

;; Return true if OP is a constant that is representable by a 13-bit
;; signed field.  This is an acceptable immediate operand for most
;; 3-address instructions.
(define_predicate "small_int_operand"
  (and (match_code "const_int")
       (match_test "SPARC_SIMM13_P (INTVAL (op))")))

;; Return true if OP is a constant operand for the umul instruction.  That
;; instruction sign-extends immediate values just like all other SPARC
;; instructions, but interprets the extended result as an unsigned number.
(define_predicate "uns_small_int_operand"
  (and (match_code "const_int")
       (match_test "((INTVAL (op) >= 0 && INTVAL (op) < 0x1000)
		    || (INTVAL (op) >= 0xFFFFF000
			&& INTVAL (op) <= 0xFFFFFFFF))")))

;; Return true if OP is a constant that can be loaded by the sethi instruction.
;; The first test avoids emitting sethi to load zero for example.
(define_predicate "const_high_operand"
  (and (match_code "const_int")
       (and (not (match_operand 0 "small_int_operand"))
            (match_test "SPARC_SETHI_P (INTVAL (op) & GET_MODE_MASK (mode))"))))

;; Return true if OP is a constant whose 1's complement can be loaded by the
;; sethi instruction.
(define_predicate "const_compl_high_operand"
  (and (match_code "const_int")
       (and (not (match_operand 0 "small_int_operand"))
            (match_test "SPARC_SETHI_P (~INTVAL (op) & GET_MODE_MASK (mode))"))))

;; Return true if OP is a FP constant that needs to be loaded by the sethi/losum
;; pair of instructions.
(define_predicate "fp_const_high_losum_operand"
  (match_operand 0 "const_double_operand")
{
  gcc_assert (mode == SFmode);
  return fp_high_losum_p (op);
})

;; Return true if OP is a const_double or const_vector.
(define_predicate "const_double_or_vector_operand"
  (match_code "const_double,const_vector"))

;; Return true if OP is Zero, or if the target is V7.
(define_predicate "zero_or_v7_operand"
  (and (match_code "const_int")
       (ior (match_test "INTVAL (op) == 0")
	    (match_test "!TARGET_V8 && !TARGET_V9"))))

;; Predicates for symbolic constants.

;; Return true if OP is either a symbol reference or a sum of a symbol
;; reference and a constant.
(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  machine_mode omode = GET_MODE (op);

  if (omode != mode && omode != VOIDmode && mode != VOIDmode)
    return false;

  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
      return !SYMBOL_REF_TLS_MODEL (op);

    case LABEL_REF:
      return true;

    case CONST:
      op = XEXP (op, 0);
      return (((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		&& !SYMBOL_REF_TLS_MODEL (XEXP (op, 0)))
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);

    default:
      gcc_unreachable ();
    }
})

;; Return true if OP is a symbolic operand for the TLS Global Dynamic model.
(define_predicate "tgd_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_GLOBAL_DYNAMIC")))

;; Return true if OP is a symbolic operand for the TLS Local Dynamic model.
(define_predicate "tld_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_DYNAMIC")))

;; Return true if OP is a symbolic operand for the TLS Initial Exec model.
(define_predicate "tie_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_INITIAL_EXEC")))

;; Return true if OP is a symbolic operand for the TLS Local Exec model.
(define_predicate "tle_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_EXEC")))

;; Return true if the operand is an argument used in generating PIC references
;; in either the medium/low or embedded medium/anywhere code models on V9.
;; Check for (const (minus (symbol_ref:GOT)
;;                         (const (minus (label) (pc)))))
(define_predicate "medium_pic_operand"
  (match_code "const")
{
  /* Check for (const (minus (symbol_ref:GOT)
                             (const (minus (label) (pc))))).  */
  op = XEXP (op, 0);
  return GET_CODE (op) == MINUS
         && GET_CODE (XEXP (op, 0)) == SYMBOL_REF
         && GET_CODE (XEXP (op, 1)) == CONST
         && GET_CODE (XEXP (XEXP (op, 1), 0)) == MINUS;
})

;; Return true if OP is a LABEL_REF of mode MODE.
(define_predicate "label_ref_operand"
  (and (match_code "label_ref")
       (match_test "GET_MODE (op) == mode")))

;; Return true if OP is a data segment reference.  This includes the readonly
;; data segment or, in other words, anything but the text segment.
;; This is needed in the embedded medium/anywhere code model on V9.  These
;; values are accessed with EMBMEDANY_BASE_REG.  */
(define_predicate "data_segment_operand"
  (match_code "symbol_ref,plus,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
      return ! SYMBOL_REF_FUNCTION_P (op);
    case PLUS :
      /* Assume canonical format of symbol + constant.
	 Fall through.  */
    case CONST :
      return data_segment_operand (XEXP (op, 0), VOIDmode);
    default :
      gcc_unreachable ();
    }
})

;; Return true if OP is a text segment reference.
;; This is needed in the embedded medium/anywhere code model on V9.
(define_predicate "text_segment_operand"
  (match_code "label_ref,symbol_ref,plus,const")
{
  switch (GET_CODE (op))
    {
    case LABEL_REF :
      return true;
    case SYMBOL_REF :
      return SYMBOL_REF_FUNCTION_P (op);
    case PLUS :
      /* Assume canonical format of symbol + constant.
	 Fall through.  */
    case CONST :
      return text_segment_operand (XEXP (op, 0), VOIDmode);
    default :
      gcc_unreachable ();
    }
})


;; Predicates for registers.

;; Return true if OP is either the zero constant or a register.
(define_predicate "register_or_zero_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_zero_operand")))

(define_predicate "register_or_v9_zero_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_test "TARGET_V9")
	    (match_operand 0 "const_zero_operand"))))

;; Return true if OP is either the zero constant, the all-ones
;; constant, or a register.
(define_predicate "register_or_zero_or_all_ones_operand"
  (ior (match_operand 0 "register_or_zero_operand")
       (match_operand 0 "const_all_ones_operand")))

;; Return true if OP is a register operand in a floating point register.
(define_predicate "fp_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op); /* Possibly a MEM */
  return REG_P (op) && SPARC_FP_REG_P (REGNO (op));
})

;; Return true if OP is an integer register of the appropriate mode
;; for a cstore result.
(define_special_predicate "cstore_result_operand"
  (match_test "register_operand (op, TARGET_ARCH64 ? DImode : SImode)"))

;; Return true if OP is a floating point condition code register.
(define_predicate "fcc_register_operand"
  (and (match_code "reg")
       (match_test "((unsigned) REGNO (op) - SPARC_FIRST_V9_FCC_REG) < 4")))

;; Return true if OP is the floating point condition code register fcc0.
(define_predicate "fcc0_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == SPARC_FCC_REG")))

;; Return true if OP is an integer condition code register.
(define_predicate "icc_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == SPARC_ICC_REG")))

;; Return true if OP is an integer or floating point condition code register.
(define_predicate "icc_or_fcc_register_operand"
  (ior (match_operand 0 "icc_register_operand")
       (match_operand 0 "fcc_register_operand")))


;; Predicates for arithmetic instructions.

;; Return true if OP is a register, or is a constant that is representable
;; by a 13-bit signed field.  This is an acceptable operand for most
;; 3-address instructions.
(define_predicate "arith_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "small_int_operand")))

;; 64-bit: Same as above.
;; 32-bit: Return true if OP is a register, or is a constant that is 
;; representable by a couple of 13-bit signed fields.  This is an
;; acceptable operand for most 3-address splitters.
(define_predicate "arith_double_operand"
  (match_code "const_int,reg,subreg")
{
  bool arith_simple_operand = arith_operand (op, mode);
  HOST_WIDE_INT m1, m2;

  if (TARGET_ARCH64 || arith_simple_operand)
    return arith_simple_operand;

  if (GET_CODE (op) != CONST_INT)
    return false;

  m1 = trunc_int_for_mode (INTVAL (op), SImode);
  m2 = trunc_int_for_mode (INTVAL (op) >> 32, SImode);

  return SPARC_SIMM13_P (m1) && SPARC_SIMM13_P (m2);
})

;; Return true if OP is suitable as second operand for add/sub.
(define_predicate "arith_add_operand"
  (ior (match_operand 0 "arith_operand")
       (match_operand 0 "const_4096_operand")))

;; Return true if OP is suitable as second double operand for add/sub.
(define_predicate "arith_double_add_operand"
  (match_code "const_int,reg,subreg")
{
  if (arith_double_operand (op, mode))
    return true;

  /* Turning an add/sub instruction into the other changes the Carry flag
     so the 4096 trick cannot be used for double operations in 32-bit mode.  */
  return TARGET_ARCH64 && const_4096_operand (op, mode);
})

;; Return true if OP is a register, or is a CONST_INT that can fit in a
;; signed 10-bit immediate field.  This is an acceptable SImode operand for
;; the movrcc instructions.
(define_predicate "arith10_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
            (match_test "SPARC_SIMM10_P (INTVAL (op))"))))

;; Return true if OP is a register, or is a CONST_INT that can fit in a
;; signed 11-bit immediate field.  This is an acceptable SImode operand for
;; the movcc instructions.
(define_predicate "arith11_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
            (match_test "SPARC_SIMM11_P (INTVAL (op))"))))

;; Return true if OP is a register or a constant for the umul instruction.
(define_predicate "uns_arith_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "uns_small_int_operand")))

;; Return true if OP is a register, or is a CONST_INT that can fit in a
;; signed 5-bit immediate field.  This is an acceptable second operand for
;; the cbcond instructions.
(define_predicate "arith5_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
            (match_test "SPARC_SIMM5_P (INTVAL (op))"))))

;; Return true if OP is a constant in the range 0..7.  This is an
;; acceptable second operand for dictunpack instructions setting a
;; V8QI mode in the destination register.
(define_predicate "imm5_operand_dictunpack8"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) >= 0 && INTVAL (op) < 8)")))

;; Return true if OP is a constant in the range 7..15.  This is an
;; acceptable second operand for dictunpack instructions setting a
;; V4HI mode in the destination register.
(define_predicate "imm5_operand_dictunpack16"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) >= 8 && INTVAL (op) < 16)")))

;; Return true if OP is a constant in the range 15..31.  This is an
;; acceptable second operand for dictunpack instructions setting a
;; V2SI mode in the destination register.
(define_predicate "imm5_operand_dictunpack32"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) >= 16 && INTVAL (op) < 32)")))

;; Return true if OP is a constant that is representable by a 2-bit
;; unsigned field.  This is an acceptable third operand for
;; fpcmp*shl instructions.
(define_predicate "imm2_operand"
  (and (match_code "const_int")
       (match_test "SPARC_IMM2_P (INTVAL (op))")))

;; Predicates for miscellaneous instructions.

;; Return true if OP is valid for the lhs of a comparison insn.
(define_predicate "compare_operand"
  (match_code "reg,subreg,zero_extract")
{
  if (GET_CODE (op) == ZERO_EXTRACT)
    return (register_operand (XEXP (op, 0), mode)
	    && small_int_operand (XEXP (op, 1), mode)
	    && small_int_operand (XEXP (op, 2), mode)
	    /* This matches cmp_zero_extract.  */
	    && ((mode == SImode
		 && INTVAL (XEXP (op, 2)) > 19)
		/* This matches cmp_zero_extract_sp64.  */
		|| (TARGET_ARCH64
		    && mode == DImode
		    && INTVAL (XEXP (op, 2)) > 51)));

  return register_operand (op, mode);
})

;; Return true if OP is a valid operand for the source of a move insn.
(define_predicate "input_operand"
  (match_code "const_int,const_double,const_vector,reg,subreg,mem")
{
  enum mode_class mclass;

  /* If both modes are non-void they must be the same.  */
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return false;

  mclass = GET_MODE_CLASS (mode);

  /* Allow any 1-instruction integer constant.  */
  if (mclass == MODE_INT
      && mode != TImode
      && (small_int_operand (op, mode) || const_high_operand (op, mode)))
    return true;

  /* If 32-bit mode and this is a DImode constant, allow it
     so that the splits can be generated.  */
  if (TARGET_ARCH32 && mode == DImode && GET_CODE (op) == CONST_INT)
    return true;

  /* Allow FP constants to be built in integer registers.  */
  if (mclass == MODE_FLOAT && GET_CODE (op) == CONST_DOUBLE)
    return true;

  if (mclass == MODE_VECTOR_INT && const_all_ones_operand (op, mode))
    return true;

  if (register_or_zero_operand (op, mode))
    return true;

  /* If this is a SUBREG, look inside so that we handle paradoxical ones.  */
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* Check for valid MEM forms.  */
  if (GET_CODE (op) == MEM)
    {
      /* Except when LRA is precisely working hard to make them valid
	 and relying entirely on the constraints.  */
      if (lra_in_progress)
	return true;

      return memory_address_p (mode, XEXP (op, 0));
    }

  return false;
})

;; Return true if OP is an address suitable for a call insn.
;; Call insn on SPARC can take a PC-relative constant address
;; or any regular memory address.
(define_predicate "call_address_operand"
  (ior (match_operand 0 "symbolic_operand")
       (match_test "memory_address_p (Pmode, op)")))

;; Return true if OP is an operand suitable for a call insn.
(define_predicate "call_operand"
  (and (match_code "mem")
       (match_test "call_address_operand (XEXP (op, 0), mode)")))


(define_predicate "mem_noofs_operand"
  (and (match_code "mem")
       (match_code "reg" "0")))

;; Predicates for operators.

;; Return true if OP is a valid comparison operator for CCNZmode.
(define_predicate "nz_comparison_operator"
  (match_code "eq,ne,lt,ge"))

;; Return true if OP is a valid comparison operator for CCCmode.
(define_predicate "c_comparison_operator"
  (match_code "ltu,geu"))

;; Return true if OP is a valid comparison operator for CCVmode.
(define_predicate "v_comparison_operator"
  (match_code "eq,ne"))

;; Return true if OP is an integer comparison operator.  This allows
;; the use of MATCH_OPERATOR to recognize all the branch insns.
(define_predicate "icc_comparison_operator"
  (match_operand 0 "ordered_comparison_operator")
{
  switch (GET_MODE (XEXP (op, 0)))
    {
    case E_CCmode:
    case E_CCXmode:
      return true;
    case E_CCNZmode:
    case E_CCXNZmode:
      return nz_comparison_operator (op, mode);
    case E_CCCmode:
    case E_CCXCmode:
      return c_comparison_operator (op, mode);
    case E_CCVmode:
    case E_CCXVmode:
      return v_comparison_operator (op, mode);
    default:
      return false;
    }
})

;; Return true if OP is a FP comparison operator.
(define_predicate "fcc_comparison_operator"
  (match_operand 0 "comparison_operator")
{
  switch (GET_MODE (XEXP (op, 0)))
    {
    case E_CCFPmode:
    case E_CCFPEmode:
      return true;
    default:
      return false;
    }
})

;; Return true if OP is an integer or FP comparison operator.  This allows
;; the use of MATCH_OPERATOR to recognize all the conditional move insns.
(define_predicate "icc_or_fcc_comparison_operator"
  (ior (match_operand 0 "icc_comparison_operator")
       (match_operand 0 "fcc_comparison_operator")))

;; Return true if OP is an integer comparison operator for V9.
(define_predicate "v9_comparison_operator"
  (and (match_operand 0 "ordered_comparison_operator")
       (match_test "TARGET_V9")))

;; Return true if OP is a comparison operator suitable for use in V9
;; conditional move or branch on register contents instructions.
(define_predicate "v9_register_comparison_operator"
  (match_code "eq,ne,ge,lt,le,gt"))

;; Return true if OP is an operator which can set the condition codes
;; explicitly.  We do not include PLUS/MINUS/NEG/ASHIFT because these
;; require CCNZmode, which we handle explicitly.
(define_predicate "cc_arith_operator"
  (match_code "and,ior,xor"))

;; Return true if OP is an operator which can bitwise complement its
;; second operand and set the condition codes explicitly.
;; XOR is not here because combine canonicalizes (xor (not ...) ...)
;; and (xor ... (not ...)) to (not (xor ...)).
(define_predicate "cc_arith_not_operator"
  (match_code "and,ior"))
