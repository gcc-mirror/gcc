;; Predicate definitions for HP PA-RISC.
;; Copyright (C) 2005, 2007, 2010, 2011 Free Software Foundation, Inc.
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

;; Return truth value of whether OP is an integer which fits the range
;; constraining 5-bit signed immediate operands in three-address insns.

(define_predicate "int5_operand"
  (and (match_code "const_int")
       (match_test "INT_5_BITS (op)")))

;; Return truth value of whether OP is an integer which fits the range
;; constraining 5-bit unsigned immediate operands in three-address insns.

(define_predicate "uint5_operand"
  (and (match_code "const_int")
       (match_test "INT_U5_BITS (op)")))

;; Return truth value of whether OP is an integer which fits the range
;; constraining 11-bit signed immediate operands in three-address insns.

(define_predicate "int11_operand"
  (and (match_code "const_int")
       (match_test "INT_11_BITS (op)")))

;; Return truth value of whether OP is an integer which fits the range
;; constraining 14-bit signed immediate operands in three-address insns.

(define_predicate "int14_operand"
  (and (match_code "const_int")
       (match_test "INT_14_BITS (op)")))

;; True iff OP is a const_int or const_double that will fit in 32 bits.

(define_predicate "uint32_operand"
  (if_then_else (match_test "HOST_BITS_PER_WIDE_INT > 32")
    (and (match_code "const_int")
         (match_test "INTVAL (op) >= 0
		      && INTVAL (op) < (HOST_WIDE_INT) 1 << 32"))
    (and (match_code "const_int,const_double")
         (match_test "CONST_INT_P (op) || CONST_DOUBLE_HIGH (op) == 0"))))

;; True iff depi can be used to compute (reg | OP).

(define_predicate "cint_ior_operand"
  (and (match_code "const_int")
       (match_test "pa_ior_mask_p (INTVAL (op))")))

;; True iff OP is CONST_INT that can be moved in one instruction
;; into a general register.

(define_predicate "cint_move_operand"
  (and (match_code "const_int")
       (match_test "pa_cint_ok_for_move (INTVAL (op))")))

;; True iff OP is a CONST0_RTX for MODE.

(define_predicate "const_0_operand"
  (and (match_code "const_int,const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; A constant integer suitable for use in a PRE_MODIFY memory reference.

(define_predicate "pre_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= -0x2000 && INTVAL (op) < 0x10")))

;; A constant integer suitable for use in a POST_MODIFY memory reference.

(define_predicate "post_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) < 0x2000 && INTVAL (op) >= -0x10")))

;; True iff depi or extru can be used to compute (reg & OP).

(define_predicate "and_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "pa_and_mask_p (INTVAL (op))"))))

;; Return truth value of whether OP can be used as an operand in a
;; three operand arithmetic insn that accepts registers of mode MODE
;; or 5-bit signed integers.

(define_predicate "arith5_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "int5_operand")))

;; Return truth value of whether OP can be used as an operand in a
;; three operand arithmetic insn that accepts registers of mode MODE
;; or 11-bit signed integers.

(define_predicate "arith11_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "int11_operand")))

;; Return truth value of whether OP can be used as an operand in a
;; three operand arithmetic insn that accepts registers of mode MODE
;; or 14-bit signed integers.

(define_predicate "arith14_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "int14_operand")))

;; Return truth value of whether OP can be used as an operand in a
;; three operand arithmetic insn that accepts registers of mode MODE
;; or 32-bit signed integers.

(define_predicate "arith32_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "const_int")))

;; True iff OP can be used as an operand in an adddi3 insn.

(define_predicate "adddi3_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (match_operand 0 "arith14_operand")
    (match_operand 0 "arith11_operand")))

;; True iff OP is valid as a base or index register in a REG+REG address.

(define_predicate "borx_reg_operand"
  (match_code "reg")
{
  /* We must reject virtual registers as the only expressions that
     can be instantiated are REG and REG+CONST.  */
  if (op == virtual_incoming_args_rtx
      || op == virtual_stack_vars_rtx
      || op == virtual_stack_dynamic_rtx
      || op == virtual_outgoing_args_rtx
      || op == virtual_cfa_rtx)
    return false;

  /* While it's always safe to index off the frame pointer, it's not
     profitable to do so when the frame pointer is being eliminated.  */
  if (!reload_completed
      && flag_omit_frame_pointer
      && !cfun->calls_alloca
      && op == frame_pointer_rtx)
    return false;

  return register_operand (op, mode);
})

;; Return nonzero if OP is suitable for use in a call to a named
;; function.
;;
;; For 2.5 try to eliminate either call_operand_address or
;; function_label_operand, they perform very similar functions.

(define_predicate "call_operand_address"
  (match_code "label_ref,symbol_ref,const_int,const_double,const,high")
{
  return (GET_MODE (op) == word_mode
	  && CONSTANT_P (op) && ! TARGET_PORTABLE_RUNTIME);
})

;; True iff OP can be used as the divisor in a div millicode call.

(define_predicate "div_operand"
  (match_code "reg,const_int")
{
  return (mode == SImode
	  && ((REG_P (op) && REGNO (op) == 25)
	      || (CONST_INT_P (op)
		  && INTVAL (op) > 0 && INTVAL (op) < 16
		  && pa_magic_milli[INTVAL (op)])));
})

;; True iff OP is a reloading floating point register

(define_predicate "fp_reg_operand"
  (and (match_code "reg")
       (match_test "reg_renumber && FP_REG_P (op)")))

;; True iff OP is a function label operand.

(define_special_predicate "function_label_operand"
  (and (match_code "symbol_ref")
       (match_test "FUNCTION_NAME_P (XSTR (op, 0))")))

;; True iff OP is an indexed memory operand.

(define_predicate "indexed_memory_operand"
  (match_code "subreg,mem")
{
  if (GET_MODE (op) != mode)
    return false;

  /* Before reload, a (SUBREG (MEM...)) forces reloading into a register.  */
  if (reload_completed && GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (! MEM_P (op))
    return false;

  op = XEXP (op, 0);
  return IS_INDEX_ADDR_P (op) && memory_address_p (mode, op);
})

;; True iff the operand OP can be used as the destination operand of
;; an integer store.  This also implies the operand could be used as
;; the source operand of an integer load.  Symbolic, lo_sum and indexed
;; memory operands are not allowed.  We accept reloading pseudos and
;; other memory operands.

(define_predicate "integer_store_memory_operand"
  (match_code "reg,mem")
{
  return ((reload_in_progress
           && REG_P (op)
           && REGNO (op) >= FIRST_PSEUDO_REGISTER
           && reg_renumber [REGNO (op)] < 0)
          || (MEM_P (op)
              && (reload_in_progress || memory_address_p (mode, XEXP (op, 0)))
              && !symbolic_memory_operand (op, VOIDmode)
              && !IS_LO_SUM_DLT_ADDR_P (XEXP (op, 0))
              && !IS_INDEX_ADDR_P (XEXP (op, 0))));
})

;; Return true iff OP is an integer register.

(define_predicate "ireg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) > 0 && REGNO (op) < 32")))

;; Return truth value of whether OP is an integer which fits the range
;; constraining immediate operands in three-address insns, or is an
;; integer register.

(define_predicate "ireg_or_int5_operand"
  (ior (match_operand 0 "ireg_operand")
       (match_operand 0 "int5_operand")))

;; True iff OP is a CONST_INT of the forms 0...0xxxx, 0...01...1xxxx,
;; or 1...1xxxx. Such values can be the left hand side x in (x << r),
;; using the zvdepi instruction.

(define_predicate "lhs_lshift_cint_operand"
  (match_code "const_int")
{
  unsigned HOST_WIDE_INT x;
  x = INTVAL (op) >> 4;
  return (x & (x + 1)) == 0;
})

;; True iff OP can be used in a zvdep instruction.

(define_predicate "lhs_lshift_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "lhs_lshift_cint_operand")))

;; Accept anything that can be used as a destination operand for a
;; move instruction.  We don't accept indexed memory operands since
;; they are supported only for floating point stores.

(define_predicate "move_dest_operand"
  (match_code "subreg,reg,mem")
{
  if (register_operand (op, mode))
    return true;

  if (GET_MODE (op) != mode)
    return false;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (GET_CODE (op) != MEM || symbolic_memory_operand (op, mode))
    return false;

  op = XEXP (op, 0);

  return (memory_address_p (mode, op)
	  && !IS_INDEX_ADDR_P (op)
	  && !IS_LO_SUM_DLT_ADDR_P (op));
})

;; Accept anything that can be used as a source operand for a move
;; instruction.

(define_predicate "move_src_operand"
  (match_code "subreg,reg,const_int,const_double,mem")
{
  if (register_operand (op, mode))
    return true;

  if (op == CONST0_RTX (mode))
    return true;

  if (CONST_INT_P (op))
    return pa_cint_ok_for_move (INTVAL (op));

  if (GET_MODE (op) != mode)
    return false;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (! MEM_P (op))
    return false;

  /* Until problems with management of the REG_POINTER flag are resolved,
     we need to delay creating move insns with unscaled indexed addresses
     until CSE is not expected.  */
  if (!TARGET_NO_SPACE_REGS
      && !cse_not_expected
      && GET_CODE (XEXP (op, 0)) == PLUS
      && REG_P (XEXP (XEXP (op, 0), 0))
      && REG_P (XEXP (XEXP (op, 0), 1)))
    return false;

  return memory_address_p (mode, XEXP (op, 0));
})

;; True iff OP is not a symbolic memory operand. 

(define_predicate "nonsymb_mem_operand"
  (match_code "subreg,mem")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (! MEM_P (op))
    return false;

  /* Until problems with management of the REG_POINTER flag are resolved,
     we need to delay creating move insns with unscaled indexed addresses
     until CSE is not expected.  */
  if (!TARGET_NO_SPACE_REGS
      && !cse_not_expected
      && GET_CODE (XEXP (op, 0)) == PLUS
      && REG_P (XEXP (XEXP (op, 0), 0))
      && REG_P (XEXP (XEXP (op, 0), 1)))
    return false;

  return (!symbolic_memory_operand (op, mode)
	  && memory_address_p (mode, XEXP (op, 0)));
})

;; True iff OP is anything other than a hard register.

(define_predicate "non_hard_reg_operand"
  (match_test "! (REG_P (op) && REGNO (op) < FIRST_PSEUDO_REGISTER)"))

;; True iff OP is a reference to a label whose address can be loaded
;; while generating PIC code.

(define_predicate "pic_label_operand"
  (match_code "label_ref,const")
{
  if (!flag_pic)
    return false;

  switch (GET_CODE (op))
    {
    case LABEL_REF:
      return true;
    case CONST:
      op = XEXP (op, 0);
      return (GET_CODE (XEXP (op, 0)) == LABEL_REF
	      && CONST_INT_P (XEXP (op, 1)));
    default:
      gcc_unreachable ();
    }
  return false;
})

;; True iff the operand OP lives in text space.  OP is a symbolic operand.
;; If so, SYMBOL_REF_FLAG, which is set by pa_encode_section_info, is true.

(define_special_predicate "read_only_operand"
  (match_test "true")
{
  if (GET_CODE (op) == CONST)
    op = XEXP (XEXP (op, 0), 0);
  if (GET_CODE (op) == SYMBOL_REF)
    {
      if (flag_pic)
        return SYMBOL_REF_FLAG (op) && !CONSTANT_POOL_ADDRESS_P (op);
      else
        return SYMBOL_REF_FLAG (op) || CONSTANT_POOL_ADDRESS_P (op);
    }
  return true;
})

;; True iff the operand is a register operand, or a non-symbolic
;; memory operand after reload.  A SUBREG is not accepted since it
;; will need a reload.
;;
;; This predicate is used for branch patterns that internally handle
;; register reloading.  We need to accept non-symbolic memory operands
;; after reload to ensure that the pattern is still valid if reload
;; didn't find a hard register for the operand.

(define_predicate "reg_before_reload_operand"
  (match_code "reg,mem")
{
  if (register_operand (op, mode))
    return true;

  if (reload_completed
      && memory_operand (op, mode)
      && !symbolic_memory_operand (op, mode))
    return true;

  return false;
})

;; True iff OP is a register or const_0 operand for MODE.

(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_0_operand")))

;; True iff OP is either a register, zero, or a non-symbolic memory operand.

(define_predicate "reg_or_0_or_nonsymb_mem_operand"
  (ior (match_operand 0 "reg_or_0_operand")
       (match_operand 0 "nonsymb_mem_operand")))

;; Accept REG and any CONST_INT that can be moved in one instruction
;; into a general register.

(define_predicate "reg_or_cint_move_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "cint_move_operand")))

;; True iff OP can be used to compute (reg | OP).

(define_predicate "reg_or_cint_ior_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "cint_ior_operand")))

;; Return 1 if OP is a CONST_INT with the value 2, 4, or 8.  These are
;; the valid constants for shadd instructions.

(define_predicate "shadd_operand"
  (and (match_code "const_int")
       (match_test "pa_shadd_constant_p (INTVAL (op))")))

;; Return truth value of statement that OP is a symbolic memory operand.

(define_predicate "symbolic_memory_operand"
  (match_code "subreg,mem")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (!MEM_P (op))
    return false;
  return pa_symbolic_expression_p (XEXP (op, 0));
})

;; True iff OP is a symbolic operand.
;; Note: an inline copy of this code is present in pa_secondary_reload.

(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
      return !SYMBOL_REF_TLS_MODEL (op);
    case LABEL_REF:
      return true;
    case CONST:
      op = XEXP (op, 0);
      return (GET_CODE (op) == PLUS
	      && ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		   && !SYMBOL_REF_TLS_MODEL (XEXP (op, 0)))
		  || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      gcc_unreachable ();
    }
  return true;
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

;; True iff this is a comparison operator.  This allows the use of
;; MATCH_OPERATOR to recognize all the branch insns.

(define_predicate "cmpib_comparison_operator"
  (match_code "eq,ne,lt,le,leu,gt,gtu,ge"))

;; True iff OP is an operator suitable for use in a movb instruction.

(define_predicate "movb_comparison_operator"
  (match_code "eq,ne,lt,ge"))

;; True iff OP is a PLUS, XOR or IOR operator.

(define_predicate "plus_xor_ior_operator"
  (match_code "plus,xor,ior"))
