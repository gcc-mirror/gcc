;; Predicate definitions for IA-32 and x86-64.
;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
;; Free Software Foundation, Inc.
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

;; Return nonzero if OP is either a i387 or SSE fp register.
(define_predicate "any_fp_register_operand"
  (and (match_code "reg")
       (match_test "ANY_FP_REGNO_P (REGNO (op))")))

;; Return nonzero if OP is an i387 fp register.
(define_predicate "fp_register_operand"
  (and (match_code "reg")
       (match_test "FP_REGNO_P (REGNO (op))")))

;; Return nonzero if OP is a non-fp register_operand.
(define_predicate "register_and_not_any_fp_reg_operand"
  (and (match_code "reg")
       (not (match_test "ANY_FP_REGNO_P (REGNO (op))"))))

;; Return nonzero if OP is a register operand other than an i387 fp register.
(define_predicate "register_and_not_fp_reg_operand"
  (and (match_code "reg")
       (not (match_test "FP_REGNO_P (REGNO (op))"))))

;; True if the operand is an MMX register.
(define_predicate "mmx_reg_operand"
  (and (match_code "reg")
       (match_test "MMX_REGNO_P (REGNO (op))")))

;; True if the operand is a Q_REGS class register.
(define_predicate "q_regs_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return ANY_QI_REG_P (op);
})

;; Match an SI or HImode register for a zero_extract.
(define_special_predicate "ext_register_operand"
  (match_operand 0 "register_operand")
{
  if ((!TARGET_64BIT || GET_MODE (op) != DImode)
      && GET_MODE (op) != SImode && GET_MODE (op) != HImode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* Be careful to accept only registers having upper parts.  */
  return REGNO (op) > LAST_VIRTUAL_REGISTER || REGNO (op) < 4;
})

;; Return true if op is the AX register.
(define_predicate "ax_reg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == 0")))

;; Return true if op is the flags register.
(define_predicate "flags_reg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == FLAGS_REG")))

;; Return true if op is a QImode register operand other than
;; %[abcd][hl].
(define_predicate "ext_QIreg_operand"
  (and (match_code "reg")
       (match_test "TARGET_64BIT
		    && GET_MODE (op) == QImode
		    && REGNO (op) > BX_REG")))

;; Similarly, but don't check mode of the operand.
(define_predicate "ext_QIreg_nomode_operand"
  (and (match_code "reg")
       (match_test "TARGET_64BIT
		    && REGNO (op) > BX_REG")))

;; Return true if op is not xmm0 register.
(define_predicate "reg_not_xmm0_operand"
   (and (match_operand 0 "register_operand")
	(match_test "!REG_P (op) 
		     || REGNO (op) != FIRST_SSE_REG")))

;; As above, but allow nonimmediate operands.
(define_predicate "nonimm_not_xmm0_operand"
   (and (match_operand 0 "nonimmediate_operand")
	(match_test "!REG_P (op) 
		     || REGNO (op) != FIRST_SSE_REG")))

;; Return 1 if VALUE can be stored in a sign extended immediate field.
(define_predicate "x86_64_immediate_operand"
  (match_code "const_int,symbol_ref,label_ref,const")
{
  if (!TARGET_64BIT)
    return immediate_operand (op, mode);

  switch (GET_CODE (op))
    {
    case CONST_INT:
      /* CONST_DOUBLES never match, since HOST_BITS_PER_WIDE_INT is known
         to be at least 32 and this all acceptable constants are
	 represented as CONST_INT.  */
      if (HOST_BITS_PER_WIDE_INT == 32)
	return 1;
      else
	{
	  HOST_WIDE_INT val = trunc_int_for_mode (INTVAL (op), DImode);
	  return trunc_int_for_mode (val, SImode) == val;
	}
      break;

    case SYMBOL_REF:
      /* For certain code models, the symbolic references are known to fit.
	 in CM_SMALL_PIC model we know it fits if it is local to the shared
	 library.  Don't count TLS SYMBOL_REFs here, since they should fit
	 only if inside of UNSPEC handled below.  */
      /* TLS symbols are not constant.  */
      if (SYMBOL_REF_TLS_MODEL (op))
	return false;
      return (ix86_cmodel == CM_SMALL || ix86_cmodel == CM_KERNEL
	      || (ix86_cmodel == CM_MEDIUM && !SYMBOL_REF_FAR_ADDR_P (op)));

    case LABEL_REF:
      /* For certain code models, the code is near as well.  */
      return (ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM
	      || ix86_cmodel == CM_KERNEL);

    case CONST:
      /* We also may accept the offsetted memory references in certain
	 special cases.  */
      if (GET_CODE (XEXP (op, 0)) == UNSPEC)
	switch (XINT (XEXP (op, 0), 1))
	  {
	  case UNSPEC_GOTPCREL:
	  case UNSPEC_DTPOFF:
	  case UNSPEC_GOTNTPOFF:
	  case UNSPEC_NTPOFF:
	    return 1;
	  default:
	    break;
	  }

      if (GET_CODE (XEXP (op, 0)) == PLUS)
	{
	  rtx op1 = XEXP (XEXP (op, 0), 0);
	  rtx op2 = XEXP (XEXP (op, 0), 1);
	  HOST_WIDE_INT offset;

	  if (ix86_cmodel == CM_LARGE)
	    return 0;
	  if (!CONST_INT_P (op2))
	    return 0;
	  offset = trunc_int_for_mode (INTVAL (op2), DImode);
	  switch (GET_CODE (op1))
	    {
	    case SYMBOL_REF:
	      /* TLS symbols are not constant.  */
	      if (SYMBOL_REF_TLS_MODEL (op1))
		return 0;
	      /* For CM_SMALL assume that latest object is 16MB before
		 end of 31bits boundary.  We may also accept pretty
		 large negative constants knowing that all objects are
		 in the positive half of address space.  */
	      if ((ix86_cmodel == CM_SMALL
		   || (ix86_cmodel == CM_MEDIUM
		       && !SYMBOL_REF_FAR_ADDR_P (op1)))
		  && offset < 16*1024*1024
		  && trunc_int_for_mode (offset, SImode) == offset)
		return 1;
	      /* For CM_KERNEL we know that all object resist in the
		 negative half of 32bits address space.  We may not
		 accept negative offsets, since they may be just off
		 and we may accept pretty large positive ones.  */
	      if (ix86_cmodel == CM_KERNEL
		  && offset > 0
		  && trunc_int_for_mode (offset, SImode) == offset)
		return 1;
	      break;

	    case LABEL_REF:
	      /* These conditions are similar to SYMBOL_REF ones, just the
		 constraints for code models differ.  */
	      if ((ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM)
		  && offset < 16*1024*1024
		  && trunc_int_for_mode (offset, SImode) == offset)
		return 1;
	      if (ix86_cmodel == CM_KERNEL
		  && offset > 0
		  && trunc_int_for_mode (offset, SImode) == offset)
		return 1;
	      break;

	    case UNSPEC:
	      switch (XINT (op1, 1))
		{
		case UNSPEC_DTPOFF:
		case UNSPEC_NTPOFF:
		  if (offset > 0
		      && trunc_int_for_mode (offset, SImode) == offset)
		    return 1;
		}
	      break;

	    default:
	      break;
	    }
	}
      break;

      default:
	gcc_unreachable ();
    }

  return 0;
})

;; Return 1 if VALUE can be stored in the zero extended immediate field.
(define_predicate "x86_64_zext_immediate_operand"
  (match_code "const_double,const_int,symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case CONST_DOUBLE:
      if (HOST_BITS_PER_WIDE_INT == 32)
	return (GET_MODE (op) == VOIDmode && !CONST_DOUBLE_HIGH (op));
      else
	return 0;

    case CONST_INT:
      if (HOST_BITS_PER_WIDE_INT == 32)
	return INTVAL (op) >= 0;
      else
	return !(INTVAL (op) & ~(HOST_WIDE_INT) 0xffffffff);

    case SYMBOL_REF:
      /* For certain code models, the symbolic references are known to fit.  */
      /* TLS symbols are not constant.  */
      if (SYMBOL_REF_TLS_MODEL (op))
	return false;
      return (ix86_cmodel == CM_SMALL
	      || (ix86_cmodel == CM_MEDIUM
		  && !SYMBOL_REF_FAR_ADDR_P (op)));

    case LABEL_REF:
      /* For certain code models, the code is near as well.  */
      return ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM;

    case CONST:
      /* We also may accept the offsetted memory references in certain
	 special cases.  */
      if (GET_CODE (XEXP (op, 0)) == PLUS)
	{
	  rtx op1 = XEXP (XEXP (op, 0), 0);
	  rtx op2 = XEXP (XEXP (op, 0), 1);

	  if (ix86_cmodel == CM_LARGE)
	    return 0;
	  switch (GET_CODE (op1))
	    {
	    case SYMBOL_REF:
	      /* TLS symbols are not constant.  */
	      if (SYMBOL_REF_TLS_MODEL (op1))
		return 0;
	      /* For small code model we may accept pretty large positive
		 offsets, since one bit is available for free.  Negative
		 offsets are limited by the size of NULL pointer area
		 specified by the ABI.  */
	      if ((ix86_cmodel == CM_SMALL
		   || (ix86_cmodel == CM_MEDIUM
		       && !SYMBOL_REF_FAR_ADDR_P (op1)))
		  && CONST_INT_P (op2)
		  && trunc_int_for_mode (INTVAL (op2), DImode) > -0x10000
		  && trunc_int_for_mode (INTVAL (op2), SImode) == INTVAL (op2))
		return 1;
	      /* ??? For the kernel, we may accept adjustment of
		 -0x10000000, since we know that it will just convert
		 negative address space to positive, but perhaps this
		 is not worthwhile.  */
	      break;

	    case LABEL_REF:
	      /* These conditions are similar to SYMBOL_REF ones, just the
		 constraints for code models differ.  */
	      if ((ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM)
		  && CONST_INT_P (op2)
		  && trunc_int_for_mode (INTVAL (op2), DImode) > -0x10000
		  && trunc_int_for_mode (INTVAL (op2), SImode) == INTVAL (op2))
		return 1;
	      break;

	    default:
	      return 0;
	    }
	}
      break;

    default:
      gcc_unreachable ();
    }
  return 0;
})

;; Return nonzero if OP is general operand representable on x86_64.
(define_predicate "x86_64_general_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "nonimmediate_operand")
	 (match_operand 0 "x86_64_immediate_operand"))
    (match_operand 0 "general_operand")))

;; Return nonzero if OP is general operand representable on x86_64
;; as either sign extended or zero extended constant.
(define_predicate "x86_64_szext_general_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "nonimmediate_operand")
	 (match_operand 0 "x86_64_immediate_operand")
	 (match_operand 0 "x86_64_zext_immediate_operand"))
    (match_operand 0 "general_operand")))

;; Return nonzero if OP is nonmemory operand representable on x86_64.
(define_predicate "x86_64_nonmemory_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "register_operand")
	 (match_operand 0 "x86_64_immediate_operand"))
    (match_operand 0 "nonmemory_operand")))

;; Return nonzero if OP is nonmemory operand representable on x86_64.
(define_predicate "x86_64_szext_nonmemory_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "register_operand")
	 (match_operand 0 "x86_64_immediate_operand")
	 (match_operand 0 "x86_64_zext_immediate_operand"))
    (match_operand 0 "nonmemory_operand")))

;; Return true when operand is PIC expression that can be computed by lea
;; operation.
(define_predicate "pic_32bit_operand"
  (match_code "const,symbol_ref,label_ref")
{
  if (!flag_pic)
    return 0;
  /* Rule out relocations that translate into 64bit constants.  */
  if (TARGET_64BIT && GET_CODE (op) == CONST)
    {
      op = XEXP (op, 0);
      if (GET_CODE (op) == PLUS && CONST_INT_P (XEXP (op, 1)))
	op = XEXP (op, 0);
      if (GET_CODE (op) == UNSPEC
	  && (XINT (op, 1) == UNSPEC_GOTOFF
	      || XINT (op, 1) == UNSPEC_GOT))
	return 0;
    }
  return symbolic_operand (op, mode);
})


;; Return nonzero if OP is nonmemory operand acceptable by movabs patterns.
(define_predicate "x86_64_movabs_operand"
  (if_then_else (match_test "!TARGET_64BIT || !flag_pic")
    (match_operand 0 "nonmemory_operand")
    (ior (match_operand 0 "register_operand")
	 (and (match_operand 0 "const_double_operand")
	      (match_test "GET_MODE_SIZE (mode) <= 8")))))

;; Returns nonzero if OP is either a symbol reference or a sum of a symbol
;; reference and a constant.
(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF
	  || (GET_CODE (op) == UNSPEC
	      && (XINT (op, 1) == UNSPEC_GOT
		  || XINT (op, 1) == UNSPEC_GOTOFF
		  || XINT (op, 1) == UNSPEC_GOTPCREL)))
	return 1;
      if (GET_CODE (op) != PLUS
	  || !CONST_INT_P (XEXP (op, 1)))
	return 0;

      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return 1;
      /* Only @GOTOFF gets offsets.  */
      if (GET_CODE (op) != UNSPEC
	  || XINT (op, 1) != UNSPEC_GOTOFF)
	return 0;

      op = XVECEXP (op, 0, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return 1;
      return 0;

    default:
      gcc_unreachable ();
    }
})

;; Return true if the operand contains a @GOT or @GOTOFF reference.
(define_predicate "pic_symbolic_operand"
  (match_code "const")
{
  op = XEXP (op, 0);
  if (TARGET_64BIT)
    {
      if (GET_CODE (op) == UNSPEC
	  && XINT (op, 1) == UNSPEC_GOTPCREL)
	return 1;
      if (GET_CODE (op) == PLUS
	  && GET_CODE (XEXP (op, 0)) == UNSPEC
	  && XINT (XEXP (op, 0), 1) == UNSPEC_GOTPCREL)
	return 1;
    }
  else
    {
      if (GET_CODE (op) == UNSPEC)
	return 1;
      if (GET_CODE (op) != PLUS
	  || !CONST_INT_P (XEXP (op, 1)))
	return 0;
      op = XEXP (op, 0);
      if (GET_CODE (op) == UNSPEC
	  && XINT (op, 1) != UNSPEC_MACHOPIC_OFFSET)
	return 1;
    }
  return 0;
})

;; Return true if OP is a symbolic operand that resolves locally.
(define_predicate "local_symbolic_operand"
  (match_code "const,label_ref,symbol_ref")
{
  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (op, 0), 1)))
    op = XEXP (XEXP (op, 0), 0);

  if (GET_CODE (op) == LABEL_REF)
    return 1;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;

  if (SYMBOL_REF_TLS_MODEL (op) != 0)
    return 0;

  if (SYMBOL_REF_LOCAL_P (op))
    return 1;

  /* There is, however, a not insubstantial body of code in the rest of
     the compiler that assumes it can just stick the results of
     ASM_GENERATE_INTERNAL_LABEL in a symbol_ref and have done.  */
  /* ??? This is a hack.  Should update the body of the compiler to
     always create a DECL an invoke targetm.encode_section_info.  */
  if (strncmp (XSTR (op, 0), internal_label_prefix,
	       internal_label_prefix_len) == 0)
    return 1;

  return 0;
})

;; Test for a legitimate @GOTOFF operand.
;;
;; VxWorks does not impose a fixed gap between segments; the run-time
;; gap can be different from the object-file gap.  We therefore can't
;; use @GOTOFF unless we are absolutely sure that the symbol is in the
;; same segment as the GOT.  Unfortunately, the flexibility of linker
;; scripts means that we can't be sure of that in general, so assume
;; that @GOTOFF is never valid on VxWorks.
(define_predicate "gotoff_operand"
  (and (match_test "!TARGET_VXWORKS_RTP")
       (match_operand 0 "local_symbolic_operand")))

;; Test for various thread-local symbols.
(define_predicate "tls_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) != 0")))

(define_predicate "tls_modbase_operand"
  (and (match_code "symbol_ref")
       (match_test "op == ix86_tls_module_base ()")))

(define_predicate "tp_or_register_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "unspec")
	    (match_test "XINT (op, 1) == UNSPEC_TP"))))

;; Test for a pc-relative call operand
(define_predicate "constant_call_address_operand"
  (match_code "symbol_ref")
{
  if (ix86_cmodel == CM_LARGE || ix86_cmodel == CM_LARGE_PIC)
    return false;
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES && SYMBOL_REF_DLLIMPORT_P (op))
    return false;
  return true;
})

;; True for any non-virtual or eliminable register.  Used in places where
;; instantiation of such a register may cause the pattern to not be recognized.
(define_predicate "register_no_elim_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return !(op == arg_pointer_rtx
	   || op == frame_pointer_rtx
	   || IN_RANGE (REGNO (op),
			FIRST_PSEUDO_REGISTER, LAST_VIRTUAL_REGISTER));
})

;; P6 processors will jump to the address after the decrement when %esp
;; is used as a call operand, so they will execute return address as a code.
;; See Pentium Pro errata 70, Pentium 2 errata A33 and Pentium 3 errata E17.

(define_predicate "call_register_no_elim_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!TARGET_64BIT && op == stack_pointer_rtx)
    return 0;

  return register_no_elim_operand (op, mode);
})

;; Similarly, but include the stack pointer.  This is used to prevent esp
;; from being used as an index reg.
(define_predicate "index_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (reload_in_progress || reload_completed)
    return REG_OK_FOR_INDEX_STRICT_P (op);
  else
    return REG_OK_FOR_INDEX_NONSTRICT_P (op);
})

;; Return false if this is any eliminable register.  Otherwise general_operand.
(define_predicate "general_no_elim_operand"
  (if_then_else (match_code "reg,subreg")
    (match_operand 0 "register_no_elim_operand")
    (match_operand 0 "general_operand")))

;; Return false if this is any eliminable register.  Otherwise
;; register_operand or a constant.
(define_predicate "nonmemory_no_elim_operand"
  (ior (match_operand 0 "register_no_elim_operand")
       (match_operand 0 "immediate_operand")))

;; Test for a valid operand for a call instruction.
(define_predicate "call_insn_operand"
  (ior (match_operand 0 "constant_call_address_operand")
       (match_operand 0 "call_register_no_elim_operand")
       (match_operand 0 "memory_operand")))

;; Similarly, but for tail calls, in which we cannot allow memory references.
(define_predicate "sibcall_insn_operand"
  (ior (match_operand 0 "constant_call_address_operand")
       (match_operand 0 "register_no_elim_operand")))

;; Match exactly zero.
(define_predicate "const0_operand"
  (match_code "const_int,const_double,const_vector")
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);
  return op == CONST0_RTX (mode);
})

;; Match exactly one.
(define_predicate "const1_operand"
  (and (match_code "const_int")
       (match_test "op == const1_rtx")))

;; Match exactly eight.
(define_predicate "const8_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 8")))

;; Match exactly 128.
(define_predicate "const128_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 128")))

;; Match 2, 4, or 8.  Used for leal multiplicands.
(define_predicate "const248_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 2 || i == 4 || i == 8;
})

;; Match 0 or 1.
(define_predicate "const_0_to_1_operand"
  (and (match_code "const_int")
       (match_test "op == const0_rtx || op == const1_rtx")))

;; Match 0 to 3.
(define_predicate "const_0_to_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 3)")))

;; Match 0 to 7.
(define_predicate "const_0_to_7_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 7)")))

;; Match 0 to 15.
(define_predicate "const_0_to_15_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 15)")))

;; Match 0 to 31.
(define_predicate "const_0_to_31_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 31)")))

;; Match 0 to 63.
(define_predicate "const_0_to_63_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 63)")))

;; Match 0 to 255.
(define_predicate "const_0_to_255_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 255)")))

;; Match (0 to 255) * 8
(define_predicate "const_0_to_255_mul_8_operand"
  (match_code "const_int")
{
  unsigned HOST_WIDE_INT val = INTVAL (op);
  return val <= 255*8 && val % 8 == 0;
})

;; Return nonzero if OP is CONST_INT >= 1 and <= 31 (a valid operand
;; for shift & compare patterns, as shifting by 0 does not change flags).
(define_predicate "const_1_to_31_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 31)")))

;; Return nonzero if OP is CONST_INT >= 1 and <= 63 (a valid operand
;; for 64bit shift & compare patterns, as shifting by 0 does not change flags).
(define_predicate "const_1_to_63_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 63)")))

;; Match 2 or 3.
(define_predicate "const_2_to_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 2, 3)")))

;; Match 4 to 5.
(define_predicate "const_4_to_5_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 4, 5)")))

;; Match 4 to 7.
(define_predicate "const_4_to_7_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 4, 7)")))

;; Match 6 to 7.
(define_predicate "const_6_to_7_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 6, 7)")))

;; Match 8 to 11.
(define_predicate "const_8_to_11_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 8, 11)")))

;; Match 12 to 15.
(define_predicate "const_12_to_15_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 12, 15)")))

;; Match exactly one bit in 2-bit mask.
(define_predicate "const_pow2_1_to_2_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 2")))

;; Match exactly one bit in 4-bit mask.
(define_predicate "const_pow2_1_to_8_operand"
  (match_code "const_int")
{
  unsigned int log = exact_log2 (INTVAL (op));
  return log <= 3;
})

;; Match exactly one bit in 8-bit mask.
(define_predicate "const_pow2_1_to_128_operand"
  (match_code "const_int")
{
  unsigned int log = exact_log2 (INTVAL (op));
  return log <= 7;
})

;; Match exactly one bit in 16-bit mask.
(define_predicate "const_pow2_1_to_32768_operand"
  (match_code "const_int")
{
  unsigned int log = exact_log2 (INTVAL (op));
  return log <= 15;
})

;; True if this is a constant appropriate for an increment or decrement.
(define_predicate "incdec_operand"
  (match_code "const_int")
{
  /* On Pentium4, the inc and dec operations causes extra dependency on flag
     registers, since carry flag is not set.  */
  if (!TARGET_USE_INCDEC && !optimize_insn_for_size_p ())
    return 0;
  return op == const1_rtx || op == constm1_rtx;
})

;; True for registers, or 1 or -1.  Used to optimize double-word shifts.
(define_predicate "reg_or_pm1_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "op == const1_rtx || op == constm1_rtx"))))

;; True if OP is acceptable as operand of DImode shift expander.
(define_predicate "shiftdi_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (match_operand 0 "nonimmediate_operand")
    (match_operand 0 "register_operand")))

(define_predicate "ashldi_input_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (match_operand 0 "nonimmediate_operand")
    (match_operand 0 "reg_or_pm1_operand")))

;; Return true if OP is a vector load from the constant pool with just
;; the first element nonzero.
(define_predicate "zero_extended_scalar_load_operand"
  (match_code "mem")
{
  unsigned n_elts;
  op = maybe_get_pool_constant (op);

  if (!(op && GET_CODE (op) == CONST_VECTOR))
    return 0;

  n_elts = CONST_VECTOR_NUNITS (op);

  for (n_elts--; n_elts > 0; n_elts--)
    {
      rtx elt = CONST_VECTOR_ELT (op, n_elts);
      if (elt != CONST0_RTX (GET_MODE_INNER (GET_MODE (op))))
	return 0;
    }
  return 1;
})

/* Return true if operand is a vector constant that is all ones. */
(define_predicate "vector_all_ones_operand"
  (match_code "const_vector")
{
  int nunits = GET_MODE_NUNITS (mode);

  if (GET_CODE (op) == CONST_VECTOR
      && CONST_VECTOR_NUNITS (op) == nunits)
    {
      int i;
      for (i = 0; i < nunits; ++i)
        {
          rtx x = CONST_VECTOR_ELT (op, i);
          if (x != constm1_rtx)
            return 0;
        }
      return 1;
    }

  return 0;
})

; Return 1 when OP is operand acceptable for standard SSE move.
(define_predicate "vector_move_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "const0_operand")))

;; Return 1 when OP is nonimmediate or standard SSE constant.
(define_predicate "nonimmediate_or_sse_const_operand"
  (match_operand 0 "general_operand")
{
  if (nonimmediate_operand (op, mode))
    return 1;
  if (standard_sse_constant_p (op) > 0)
    return 1;
  return 0;
})

;; Return true if OP is a register or a zero.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

;; Return true if op if a valid address, and does not contain
;; a segment override.
(define_special_predicate "no_seg_address_operand"
  (match_operand 0 "address_operand")
{
  struct ix86_address parts;
  int ok;

  ok = ix86_decompose_address (op, &parts);
  gcc_assert (ok);
  return parts.seg == SEG_DEFAULT;
})

;; Return nonzero if the rtx is known to be at least 32 bits aligned.
(define_predicate "aligned_operand"
  (match_operand 0 "general_operand")
{
  struct ix86_address parts;
  int ok;

  /* Registers and immediate operands are always "aligned".  */
  if (!MEM_P (op))
    return 1;

  /* All patterns using aligned_operand on memory operands ends up
     in promoting memory operand to 64bit and thus causing memory mismatch.  */
  if (TARGET_MEMORY_MISMATCH_STALL && !optimize_insn_for_size_p ())
    return 0;

  /* Don't even try to do any aligned optimizations with volatiles.  */
  if (MEM_VOLATILE_P (op))
    return 0;

  if (MEM_ALIGN (op) >= 32)
    return 1;

  op = XEXP (op, 0);

  /* Pushes and pops are only valid on the stack pointer.  */
  if (GET_CODE (op) == PRE_DEC
      || GET_CODE (op) == POST_INC)
    return 1;

  /* Decode the address.  */
  ok = ix86_decompose_address (op, &parts);
  gcc_assert (ok);

  /* Look for some component that isn't known to be aligned.  */
  if (parts.index)
    {
      if (REGNO_POINTER_ALIGN (REGNO (parts.index)) * parts.scale < 32)
	return 0;
    }
  if (parts.base)
    {
      if (REGNO_POINTER_ALIGN (REGNO (parts.base)) < 32)
	return 0;
    }
  if (parts.disp)
    {
      if (!CONST_INT_P (parts.disp)
	  || (INTVAL (parts.disp) & 3) != 0)
	return 0;
    }

  /* Didn't find one -- this must be an aligned address.  */
  return 1;
})

;; Returns 1 if OP is memory operand with a displacement.
(define_predicate "memory_displacement_operand"
  (match_operand 0 "memory_operand")
{
  struct ix86_address parts;
  int ok;

  ok = ix86_decompose_address (XEXP (op, 0), &parts);
  gcc_assert (ok);
  return parts.disp != NULL_RTX;
})

;; Returns 1 if OP is memory operand with a displacement only.
(define_predicate "memory_displacement_only_operand"
  (match_operand 0 "memory_operand")
{
  struct ix86_address parts;
  int ok;

  if (TARGET_64BIT)
    return 0;

  ok = ix86_decompose_address (XEXP (op, 0), &parts);
  gcc_assert (ok);

  if (parts.base || parts.index)
    return 0;

  return parts.disp != NULL_RTX;
})

;; Returns 1 if OP is memory operand which will need zero or
;; one register at most, not counting stack pointer or frame pointer.
(define_predicate "cmpxchg8b_pic_memory_operand"
  (match_operand 0 "memory_operand")
{
  struct ix86_address parts;
  int ok;

  ok = ix86_decompose_address (XEXP (op, 0), &parts);
  gcc_assert (ok);
  if (parts.base == NULL_RTX
      || parts.base == arg_pointer_rtx
      || parts.base == frame_pointer_rtx
      || parts.base == hard_frame_pointer_rtx
      || parts.base == stack_pointer_rtx)
    return 1;

  if (parts.index == NULL_RTX
      || parts.index == arg_pointer_rtx
      || parts.index == frame_pointer_rtx
      || parts.index == hard_frame_pointer_rtx
      || parts.index == stack_pointer_rtx)
    return 1;

  return 0;
})


;; Returns 1 if OP is memory operand that cannot be represented
;; by the modRM array.
(define_predicate "long_memory_operand"
  (and (match_operand 0 "memory_operand")
       (match_test "memory_address_length (op) != 0")))

;; Return 1 if OP is a comparison operator that can be issued by fcmov.
(define_predicate "fcmov_comparison_operator"
  (match_operand 0 "comparison_operator")
{
  enum machine_mode inmode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);

  if (inmode == CCFPmode || inmode == CCFPUmode)
    {
      if (!ix86_trivial_fp_comparison_operator (op, mode))
	return 0;
      code = ix86_fp_compare_code_to_integer (code);
    }
  /* i387 supports just limited amount of conditional codes.  */
  switch (code)
    {
    case LTU: case GTU: case LEU: case GEU:
      if (inmode == CCmode || inmode == CCFPmode || inmode == CCFPUmode
	  || inmode == CCCmode)
	return 1;
      return 0;
    case ORDERED: case UNORDERED:
    case EQ: case NE:
      return 1;
    default:
      return 0;
    }
})

;; Return 1 if OP is a comparison that can be used in the CMPSS/CMPPS insns.
;; The first set are supported directly; the second set can't be done with
;; full IEEE support, i.e. NaNs.

(define_predicate "sse_comparison_operator"
  (match_code "eq,lt,le,unordered,ne,unge,ungt,ordered"))

;; Return 1 if OP is a comparison operator that can be issued by
;; avx predicate generation instructions
(define_predicate "avx_comparison_float_operator"
  (match_code "ne,eq,ge,gt,le,lt,unordered,ordered,uneq,unge,ungt,unle,unlt,ltgt"))

(define_predicate "ix86_comparison_int_operator"
  (match_code "ne,eq,ge,gt,le,lt"))

(define_predicate "ix86_comparison_uns_operator"
  (match_code "ne,eq,geu,gtu,leu,ltu"))

(define_predicate "bt_comparison_operator"
  (match_code "ne,eq"))

;; Return 1 if OP is a valid comparison operator in valid mode.
(define_predicate "ix86_comparison_operator"
  (match_operand 0 "comparison_operator")
{
  enum machine_mode inmode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);

  if (inmode == CCFPmode || inmode == CCFPUmode)
    return ix86_trivial_fp_comparison_operator (op, mode);

  switch (code)
    {
    case EQ: case NE:
      return 1;
    case LT: case GE:
      if (inmode == CCmode || inmode == CCGCmode
	  || inmode == CCGOCmode || inmode == CCNOmode)
	return 1;
      return 0;
    case LTU: case GTU: case LEU: case GEU:
      if (inmode == CCmode || inmode == CCCmode)
	return 1;
      return 0;
    case ORDERED: case UNORDERED:
      if (inmode == CCmode)
	return 1;
      return 0;
    case GT: case LE:
      if (inmode == CCmode || inmode == CCGCmode || inmode == CCNOmode)
	return 1;
      return 0;
    default:
      return 0;
    }
})

;; Return 1 if OP is a valid comparison operator testing carry flag to be set.
(define_predicate "ix86_carry_flag_operator"
  (match_code "ltu,lt,unlt,gtu,gt,ungt,le,unle,ge,unge,ltgt,uneq")
{
  enum machine_mode inmode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);

  if (inmode == CCFPmode || inmode == CCFPUmode)
    {
      if (!ix86_trivial_fp_comparison_operator (op, mode))
	return 0;
      code = ix86_fp_compare_code_to_integer (code);
    }
  else if (inmode == CCCmode)
   return code == LTU || code == GTU;
  else if (inmode != CCmode)
    return 0;

  return code == LTU;
})

;; Return 1 if this comparison only requires testing one flag bit.
(define_predicate "ix86_trivial_fp_comparison_operator"
  (match_code "gt,ge,unlt,unle,uneq,ltgt,ordered,unordered"))

;; Return 1 if we know how to do this comparison.  Others require
;; testing more than one flag bit, and we let the generic middle-end
;; code do that.
(define_predicate "ix86_fp_comparison_operator"
  (if_then_else (match_test "ix86_fp_comparison_strategy (GET_CODE (op))
                             == IX86_FPCMP_ARITH")
               (match_operand 0 "comparison_operator")
               (match_operand 0 "ix86_trivial_fp_comparison_operator")))

;; Same as above, but for swapped comparison used in fp_jcc_4_387.
(define_predicate "ix86_swapped_fp_comparison_operator"
  (match_operand 0 "comparison_operator")
{
  enum rtx_code code = GET_CODE (op);
  int ret;

  PUT_CODE (op, swap_condition (code));
  ret = ix86_fp_comparison_operator (op, mode);
  PUT_CODE (op, code);
  return ret;
})

;; Nearly general operand, but accept any const_double, since we wish
;; to be able to drop them into memory rather than have them get pulled
;; into registers.
(define_predicate "cmp_fp_expander_operand"
  (ior (match_code "const_double")
       (match_operand 0 "general_operand")))

;; Return true if this is a valid binary floating-point operation.
(define_predicate "binary_fp_operator"
  (match_code "plus,minus,mult,div"))

;; Return true if this is a multiply operation.
(define_predicate "mult_operator"
  (match_code "mult"))

;; Return true if this is a division operation.
(define_predicate "div_operator"
  (match_code "div"))

;; Return true if this is a float extend operation.
(define_predicate "float_operator"
  (match_code "float"))

;; Return true for ARITHMETIC_P.
(define_predicate "arith_or_logical_operator"
  (match_code "plus,mult,and,ior,xor,smin,smax,umin,umax,compare,minus,div,
	       mod,udiv,umod,ashift,rotate,ashiftrt,lshiftrt,rotatert"))

;; Return true for COMMUTATIVE_P.
(define_predicate "commutative_operator"
  (match_code "plus,mult,and,ior,xor,smin,smax,umin,umax"))

;; Return 1 if OP is a binary operator that can be promoted to wider mode.
(define_predicate "promotable_binary_operator"
  (ior (match_code "plus,and,ior,xor,ashift")
       (and (match_code "mult")
	    (match_test "TARGET_TUNE_PROMOTE_HIMODE_IMUL"))))

(define_predicate "compare_operator"
  (match_code "compare"))

(define_predicate "absneg_operator"
  (match_code "abs,neg"))

;; Return 1 if OP is misaligned memory operand
(define_predicate "misaligned_operand"
  (and (match_code "mem")
       (match_test "MEM_ALIGN (op) < GET_MODE_ALIGNMENT (mode)")))

;; Return 1 if OP is a emms operation, known to be a PARALLEL.
(define_predicate "emms_operation"
  (match_code "parallel")
{
  unsigned i;

  if (XVECLEN (op, 0) != 17)
    return 0;

  for (i = 0; i < 8; i++)
    {
      rtx elt = XVECEXP (op, 0, i+1);

      if (GET_CODE (elt) != CLOBBER
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != XFmode
	  || REGNO (SET_DEST (elt)) != FIRST_STACK_REG + i)
        return 0;

      elt = XVECEXP (op, 0, i+9);

      if (GET_CODE (elt) != CLOBBER
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != DImode
	  || REGNO (SET_DEST (elt)) != FIRST_MMX_REG + i)
	return 0;
    }
  return 1;
})

;; Return 1 if OP is a vzeroall operation, known to be a PARALLEL.
(define_predicate "vzeroall_operation"
  (match_code "parallel")
{
  unsigned i, nregs = TARGET_64BIT ? 16 : 8;

  if ((unsigned) XVECLEN (op, 0) != 1 + nregs)
    return 0;

  for (i = 0; i < nregs; i++)
    {
      rtx elt = XVECEXP (op, 0, i+1);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != V8SImode
	  || REGNO (SET_DEST (elt)) != SSE_REGNO (i)
	  || SET_SRC (elt) != CONST0_RTX (V8SImode))
	return 0;
    }
  return 1;
})

;; Return 1 if OP is a vzeroupper operation, known to be a PARALLEL.
(define_predicate "vzeroupper_operation"
  (match_code "parallel")
{
  unsigned i, nregs = TARGET_64BIT ? 16 : 8;
 
  if ((unsigned) XVECLEN (op, 0) != 1 + nregs)
    return 0;

  for (i = 0; i < nregs; i++)
    {
      rtx elt = XVECEXP (op, 0, i+1);

      if (GET_CODE (elt) != CLOBBER
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != V8SImode
	  || REGNO (SET_DEST (elt)) != SSE_REGNO (i))
	return 0;
    }
  return 1;
})

;; Return 1 if OP is a parallel for a vpermilp[ds] permute.
;; ??? It would be much easier if the PARALLEL for a VEC_SELECT
;; had a mode, but it doesn't.  So we have 4 copies and install
;; the mode by hand.

(define_predicate "avx_vpermilp_v8sf_operand"
  (and (match_code "parallel")
       (match_test "avx_vpermilp_parallel (op, V8SFmode)")))

(define_predicate "avx_vpermilp_v4df_operand"
  (and (match_code "parallel")
       (match_test "avx_vpermilp_parallel (op, V4DFmode)")))

(define_predicate "avx_vpermilp_v4sf_operand"
  (and (match_code "parallel")
       (match_test "avx_vpermilp_parallel (op, V4SFmode)")))

(define_predicate "avx_vpermilp_v2df_operand"
  (and (match_code "parallel")
       (match_test "avx_vpermilp_parallel (op, V2DFmode)")))

;; Return 1 if OP is a parallel for a vperm2f128 permute.

(define_predicate "avx_vperm2f128_v8sf_operand"
  (and (match_code "parallel")
       (match_test "avx_vperm2f128_parallel (op, V8SFmode)")))

(define_predicate "avx_vperm2f128_v8si_operand"
  (and (match_code "parallel")
       (match_test "avx_vperm2f128_parallel (op, V8SImode)")))

(define_predicate "avx_vperm2f128_v4df_operand"
  (and (match_code "parallel")
       (match_test "avx_vperm2f128_parallel (op, V4DFmode)")))

;; Return 1 if OP is a parallel for a vbroadcast permute.

(define_predicate "avx_vbroadcast_operand"
  (and (match_code "parallel")
       (match_code "const_int" "a"))
{
  rtx elt = XVECEXP (op, 0, 0);
  int i, nelt = XVECLEN (op, 0);

  /* Don't bother checking there are the right number of operands,
     merely that they're all identical.  */
  for (i = 1; i < nelt; ++i)
    if (XVECEXP (op, 0, i) != elt)
      return false;
  return true;
})
