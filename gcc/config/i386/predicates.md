;; Predicate definitions for IA-32 and x86-64.
;; Copyright (C) 2004-2022 Free Software Foundation, Inc.
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

;; Return true if OP is either a i387 or SSE fp register.
(define_predicate "any_fp_register_operand"
  (and (match_code "reg")
       (match_test "ANY_FP_REGNO_P (REGNO (op))")))

;; Return true if OP is an i387 fp register.
(define_predicate "fp_register_operand"
  (and (match_code "reg")
       (match_test "STACK_REGNO_P (REGNO (op))")))

;; True if the operand is a GENERAL class register.
(define_predicate "general_reg_operand"
  (and (match_code "reg")
       (match_test "GENERAL_REGNO_P (REGNO (op))")))

;; True if the operand is a nonimmediate operand with GENERAL class register.
(define_predicate "nonimmediate_gr_operand"
  (if_then_else (match_code "reg")
    (match_test "GENERAL_REGNO_P (REGNO (op))")
    (match_operand 0 "nonimmediate_operand")))

;; True if the operand is a general operand with GENERAL class register.
(define_predicate "general_gr_operand"
  (if_then_else (match_code "reg")
    (match_test "GENERAL_REGNO_P (REGNO (op))")
    (match_operand 0 "general_operand")))

;; True if the operand is an MMX register.
(define_predicate "mmx_reg_operand"
  (and (match_code "reg")
       (match_test "MMX_REGNO_P (REGNO (op))")))

;; Match register operands, but include memory operands for
;; !TARGET_MMX_WITH_SSE.
(define_predicate "register_mmxmem_operand"
  (ior (match_operand 0 "register_operand")
       (and (not (match_test "TARGET_MMX_WITH_SSE"))
	    (match_operand 0 "memory_operand"))))

;; True if the operand is an SSE register.
(define_predicate "sse_reg_operand"
  (and (match_code "reg")
       (match_test "SSE_REGNO_P (REGNO (op))")))

;; Return true if op is a QImode register.
(define_predicate "any_QIreg_operand"
  (and (match_code "reg")
       (match_test "ANY_QI_REGNO_P (REGNO (op))")))

;; Return true if op is one of QImode registers: %[abcd][hl].
(define_predicate "QIreg_operand"
  (and (match_code "reg")
       (match_test "QI_REGNO_P (REGNO (op))")))

;; Return true if op is a QImode register operand other than %[abcd][hl].
(define_predicate "ext_QIreg_operand"
  (and (match_test "TARGET_64BIT")
       (match_code "reg")
       (not (match_test "QI_REGNO_P (REGNO (op))"))))

;; Return true if op is the AX register.
(define_predicate "ax_reg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == AX_REG")))

;; Return true if op is the flags register.
(define_predicate "flags_reg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == FLAGS_REG")))

;; True if the operand is a MASK register.
(define_predicate "mask_reg_operand"
  (and (match_code "reg")
       (match_test "MASK_REGNO_P (REGNO (op))")))

;; Match a DI, SI, HI or QImode nonimmediate_operand.
(define_special_predicate "int_nonimmediate_operand"
  (and (match_operand 0 "nonimmediate_operand")
       (ior (and (match_test "TARGET_64BIT")
		 (match_test "GET_MODE (op) == DImode"))
	    (match_test "GET_MODE (op) == SImode")
	    (match_test "GET_MODE (op) == HImode")
	    (match_test "GET_MODE (op) == QImode"))))

;; Match register operands, but include memory operands for TARGET_SSE_MATH.
(define_predicate "register_ssemem_operand"
  (if_then_else
    (match_test "SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH")
    (match_operand 0 "nonimmediate_operand")
    (match_operand 0 "register_operand")))

;; Match nonimmediate operands, but exclude memory operands
;; for TARGET_SSE_MATH if TARGET_MIX_SSE_I387 is not enabled.
(define_predicate "nonimm_ssenomem_operand"
  (if_then_else
    (and (match_test "SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH")
	 (not (match_test "TARGET_MIX_SSE_I387")))
    (match_operand 0 "register_operand")
    (match_operand 0 "nonimmediate_operand")))

;; The above predicate, suitable for x87 arithmetic operators.
(define_predicate "x87nonimm_ssenomem_operand"
  (if_then_else
    (and (match_test "SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH")
	 (not (match_test "TARGET_MIX_SSE_I387 && X87_ENABLE_ARITH (mode)")))
    (match_operand 0 "register_operand")
    (match_operand 0 "nonimmediate_operand")))

;; Match register operands, include memory operand for TARGET_SSE4_1.
(define_predicate "register_sse4nonimm_operand"
  (if_then_else (match_test "TARGET_SSE4_1")
    (match_operand 0 "nonimmediate_operand")
    (match_operand 0 "register_operand")))

;; Return true if VALUE is symbol reference
(define_predicate "symbol_operand"
  (match_code "symbol_ref"))

;; Return true if VALUE is an ENDBR opcode in immediate field.
(define_predicate "ix86_endbr_immediate_operand"
  (match_code "const_int")
{
  if (flag_cf_protection & CF_BRANCH)
     {
       unsigned HOST_WIDE_INT imm = UINTVAL (op);
       unsigned HOST_WIDE_INT val = TARGET_64BIT ? 0xfa1e0ff3 : 0xfb1e0ff3;

       if (imm == val)
	 return true;

       /* NB: Encoding is byte based.  */
       if (TARGET_64BIT)
	 for (; imm >= val; imm >>= 8)
	   if (imm == val)
	     return true;
      }

  return false;
})

;; Return true if VALUE can be stored in a sign extended immediate field.
(define_predicate "x86_64_immediate_operand"
  (match_code "const_int,symbol_ref,label_ref,const")
{
  if (ix86_endbr_immediate_operand (op, VOIDmode))
    return false;

  if (!TARGET_64BIT)
    return immediate_operand (op, mode);

  switch (GET_CODE (op))
    {
    case CONST_INT:
      {
        HOST_WIDE_INT val = INTVAL (op);
        return trunc_int_for_mode (val, SImode) == val;
      }
    case SYMBOL_REF:
      /* TLS symbols are not constant.  */
      if (SYMBOL_REF_TLS_MODEL (op))
	return false;

      /* Load the external function address via the GOT slot.  */
      if (ix86_force_load_from_GOT_p (op))
	return false;

      /* For certain code models, the symbolic references are known to fit.
	 in CM_SMALL_PIC model we know it fits if it is local to the shared
	 library.  Don't count TLS SYMBOL_REFs here, since they should fit
	 only if inside of UNSPEC handled below.  */
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
	    return true;
	  default:
	    break;
	  }

      if (GET_CODE (XEXP (op, 0)) == PLUS)
	{
	  rtx op1 = XEXP (XEXP (op, 0), 0);
	  rtx op2 = XEXP (XEXP (op, 0), 1);

	  if (ix86_cmodel == CM_LARGE && GET_CODE (op1) != UNSPEC)
	    return false;
	  if (!CONST_INT_P (op2))
	    return false;

	  HOST_WIDE_INT offset = INTVAL (op2);
	  if (trunc_int_for_mode (offset, SImode) != offset)
	    return false;

	  switch (GET_CODE (op1))
	    {
	    case SYMBOL_REF:
	      /* TLS symbols are not constant.  */
	      if (SYMBOL_REF_TLS_MODEL (op1))
		return false;

	      /* Load the external function address via the GOT slot.  */
	      if (ix86_force_load_from_GOT_p (op1))
	        return false;

	      /* For CM_SMALL assume that latest object is 16MB before
		 end of 31bits boundary.  We may also accept pretty
		 large negative constants knowing that all objects are
		 in the positive half of address space.  */
	      if ((ix86_cmodel == CM_SMALL
		   || (ix86_cmodel == CM_MEDIUM
		       && !SYMBOL_REF_FAR_ADDR_P (op1)))
		  && offset < 16*1024*1024)
		return true;
	      /* For CM_KERNEL we know that all object resist in the
		 negative half of 32bits address space.  We may not
		 accept negative offsets, since they may be just off
		 and we may accept pretty large positive ones.  */
	      if (ix86_cmodel == CM_KERNEL
		  && offset > 0)
		return true;
	      break;

	    case LABEL_REF:
	      /* These conditions are similar to SYMBOL_REF ones, just the
		 constraints for code models differ.  */
	      if ((ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM)
		  && offset < 16*1024*1024)
		return true;
	      if (ix86_cmodel == CM_KERNEL
		  && offset > 0)
		return true;
	      break;

	    case UNSPEC:
	      switch (XINT (op1, 1))
		{
		case UNSPEC_DTPOFF:
		case UNSPEC_NTPOFF:
		  return true;
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

  return false;
})

;; Return true if VALUE can be stored in the zero extended immediate field.
(define_predicate "x86_64_zext_immediate_operand"
  (match_code "const_int,symbol_ref,label_ref,const")
{
  if (ix86_endbr_immediate_operand (op, VOIDmode))
    return false;

  switch (GET_CODE (op))
    {
    case CONST_INT:
      return !(INTVAL (op) & ~(HOST_WIDE_INT) 0xffffffff);

    case SYMBOL_REF:
      /* TLS symbols are not constant.  */
      if (SYMBOL_REF_TLS_MODEL (op))
	return false;

      /* Load the external function address via the GOT slot.  */
      if (ix86_force_load_from_GOT_p (op))
	return false;

     /* For certain code models, the symbolic references are known to fit.  */
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
	    return false;
	  if (!CONST_INT_P (op2))
	    return false;

	  HOST_WIDE_INT offset = INTVAL (op2);
	  if (trunc_int_for_mode (offset, SImode) != offset)
	    return false;

	  switch (GET_CODE (op1))
	    {
	    case SYMBOL_REF:
	      /* TLS symbols are not constant.  */
	      if (SYMBOL_REF_TLS_MODEL (op1))
		return false;

	      /* Load the external function address via the GOT slot.  */
	      if (ix86_force_load_from_GOT_p (op1))
	        return false;

	      /* For small code model we may accept pretty large positive
		 offsets, since one bit is available for free.  Negative
		 offsets are limited by the size of NULL pointer area
		 specified by the ABI.  */
	      if ((ix86_cmodel == CM_SMALL
		   || (ix86_cmodel == CM_MEDIUM
		       && !SYMBOL_REF_FAR_ADDR_P (op1)))
		  && offset > -0x10000)
		return true;
	      /* ??? For the kernel, we may accept adjustment of
		 -0x10000000, since we know that it will just convert
		 negative address space to positive, but perhaps this
		 is not worthwhile.  */
	      break;

	    case LABEL_REF:
	      /* These conditions are similar to SYMBOL_REF ones, just the
		 constraints for code models differ.  */
	      if ((ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM)
		  && offset > -0x10000)
		return true;
	      break;

	    default:
	      return false;
	    }
	}
      break;

    default:
      gcc_unreachable ();
    }
  return false;
})

;; Return true if VALUE is a constant integer whose low and high words satisfy
;; x86_64_immediate_operand.
(define_predicate "x86_64_hilo_int_operand"
  (match_code "const_int,const_wide_int")
{
  switch (GET_CODE (op))
    {
    case CONST_INT:
      return x86_64_immediate_operand (op, mode);

    case CONST_WIDE_INT:
      gcc_assert (CONST_WIDE_INT_NUNITS (op) == 2);
      return (x86_64_immediate_operand (GEN_INT (CONST_WIDE_INT_ELT (op, 0)),
					DImode)
	      && x86_64_immediate_operand (GEN_INT (CONST_WIDE_INT_ELT (op,
									1)),
					   DImode));

    default:
      gcc_unreachable ();
    }
})

;; Return true if VALUE is a constant integer whose value is
;; x86_64_immediate_operand value zero extended from word mode to mode.
(define_predicate "x86_64_dwzext_immediate_operand"
  (match_code "const_int,const_wide_int")
{
  if (ix86_endbr_immediate_operand (op, VOIDmode))
    return false;

  switch (GET_CODE (op))
    {
    case CONST_INT:
      if (!TARGET_64BIT)
	return UINTVAL (op) <= HOST_WIDE_INT_UC (0xffffffff);
      return UINTVAL (op) <= HOST_WIDE_INT_UC (0x7fffffff);

    case CONST_WIDE_INT:
      if (!TARGET_64BIT)
	return false;
      return (CONST_WIDE_INT_NUNITS (op) == 2
	      && CONST_WIDE_INT_ELT (op, 1) == 0
	      && (trunc_int_for_mode (CONST_WIDE_INT_ELT (op, 0), SImode)
		  == (HOST_WIDE_INT) CONST_WIDE_INT_ELT (op, 0)));

    default:
      gcc_unreachable ();
    }
})

;; Return true if size of VALUE can be stored in a sign
;; extended immediate field.
(define_predicate "x86_64_immediate_size_operand"
  (and (match_code "symbol_ref")
       (ior (not (match_test "TARGET_64BIT"))
	    (match_test "ix86_cmodel == CM_SMALL")
	    (match_test "ix86_cmodel == CM_KERNEL"))))

;; Return true if OP is general operand representable on x86_64.
(define_predicate "x86_64_general_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "nonimmediate_operand")
	 (match_operand 0 "x86_64_immediate_operand"))
    (match_operand 0 "general_operand")))

;; Return true if OP's both words are general operands representable
;; on x86_64.
(define_predicate "x86_64_hilo_general_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "nonimmediate_operand")
	 (match_operand 0 "x86_64_hilo_int_operand"))
    (match_operand 0 "general_operand")))

;; Return true if OP is non-VOIDmode general operand representable
;; on x86_64.  This predicate is used in sign-extending conversion
;; operations that require non-VOIDmode immediate operands.
(define_predicate "x86_64_sext_operand"
  (and (match_test "GET_MODE (op) != VOIDmode")
       (match_operand 0 "x86_64_general_operand")))

;; Return true if OP is non-VOIDmode general operand.  This predicate
;; is used in sign-extending conversion operations that require
;; non-VOIDmode immediate operands.
(define_predicate "sext_operand"
  (and (match_test "GET_MODE (op) != VOIDmode")
       (match_operand 0 "general_operand")))

;; Return true if OP is representable on x86_64 as zero-extended operand.
;; This predicate is used in zero-extending conversion operations that
;; require non-VOIDmode immediate operands.
(define_predicate "x86_64_zext_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "nonimmediate_operand")
	 (and (match_operand 0 "x86_64_zext_immediate_operand")
	      (match_test "GET_MODE (op) != VOIDmode")))
    (match_operand 0 "nonimmediate_operand")))

;; Return true if OP is general operand representable on x86_64
;; as either sign extended or zero extended constant.
(define_predicate "x86_64_szext_general_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "nonimmediate_operand")
	 (match_operand 0 "x86_64_immediate_operand")
	 (match_operand 0 "x86_64_zext_immediate_operand"))
    (match_operand 0 "general_operand")))

;; Return true if OP is nonmemory operand representable on x86_64.
(define_predicate "x86_64_nonmemory_operand"
  (if_then_else (match_test "TARGET_64BIT")
    (ior (match_operand 0 "register_operand")
	 (match_operand 0 "x86_64_immediate_operand"))
    (match_operand 0 "nonmemory_operand")))

;; Return true if OP is nonmemory operand representable on x86_64.
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
    return false;

  /* Rule out relocations that translate into 64bit constants.  */
  if (TARGET_64BIT && GET_CODE (op) == CONST)
    {
      op = XEXP (op, 0);
      if (GET_CODE (op) == PLUS && CONST_INT_P (XEXP (op, 1)))
	op = XEXP (op, 0);
      if (GET_CODE (op) == UNSPEC
	  && (XINT (op, 1) == UNSPEC_GOTOFF
	      || XINT (op, 1) == UNSPEC_GOT))
	return false;
    }

  return symbolic_operand (op, mode);
})

;; Return true if OP is nonmemory operand acceptable by movabs patterns.
(define_predicate "x86_64_movabs_operand"
  (and (match_operand 0 "nonmemory_operand")
       (not (match_operand 0 "pic_32bit_operand"))))

;; Return true if OP is either a symbol reference or a sum of a symbol
;; reference and a constant.
(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return true;

    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF
	  || (GET_CODE (op) == UNSPEC
	      && (XINT (op, 1) == UNSPEC_GOT
		  || XINT (op, 1) == UNSPEC_GOTOFF
		  || XINT (op, 1) == UNSPEC_PCREL
		  || XINT (op, 1) == UNSPEC_GOTPCREL)))
	return true;
      if (GET_CODE (op) != PLUS
	  || !CONST_INT_P (XEXP (op, 1)))
	return false;

      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return true;
      /* Only @GOTOFF gets offsets.  */
      if (GET_CODE (op) != UNSPEC
	  || XINT (op, 1) != UNSPEC_GOTOFF)
	return false;

      op = XVECEXP (op, 0, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return true;
      return false;

    default:
      gcc_unreachable ();
    }
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
    return true;

  if (GET_CODE (op) != SYMBOL_REF)
    return false;

  if (SYMBOL_REF_TLS_MODEL (op))
    return false;

  /* Dll-imported symbols are always external.  */
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES && SYMBOL_REF_DLLIMPORT_P (op))
    return false;
  if (SYMBOL_REF_LOCAL_P (op))
    return true;

  /* There is, however, a not insubstantial body of code in the rest of
     the compiler that assumes it can just stick the results of
     ASM_GENERATE_INTERNAL_LABEL in a symbol_ref and have done.  */
  /* ??? This is a hack.  Should update the body of the compiler to
     always create a DECL an invoke targetm.encode_section_info.  */
  if (strncmp (XSTR (op, 0), internal_label_prefix,
	       internal_label_prefix_len) == 0)
    return true;

  return false;
})

(define_predicate "local_func_symbolic_operand"
  (match_operand 0 "local_symbolic_operand")
{
  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (op, 0), 1)))
    op = XEXP (XEXP (op, 0), 0);

  if (GET_CODE (op) == SYMBOL_REF
      && !SYMBOL_REF_FUNCTION_P (op))
    return false;

  return true;
})

;; Test for a legitimate @GOTOFF operand.
;;
;; VxWorks does not impose a fixed gap between segments; the run-time
;; gap can be different from the object-file gap.  We therefore can't
;; use @GOTOFF unless we are absolutely sure that the symbol is in the
;; same segment as the GOT.  Unfortunately, the flexibility of linker
;; scripts means that we can't be sure of that in general, so assume
;; @GOTOFF is not valid on VxWorks, except with the large code model.
(define_predicate "gotoff_operand"
  (and (ior (not (match_test "TARGET_VXWORKS_RTP"))
            (match_test "ix86_cmodel == CM_LARGE")
            (match_test "ix86_cmodel == CM_LARGE_PIC"))
       (match_operand 0 "local_symbolic_operand")))

;; Test for various thread-local symbols.
(define_special_predicate "tls_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op)")))

(define_special_predicate "tls_modbase_operand"
  (and (match_code "symbol_ref")
       (match_test "op == ix86_tls_module_base ()")))

(define_predicate "tls_address_pattern"
  (and (match_code "set,parallel,unspec,unspec_volatile")
       (match_test "ix86_tls_address_pattern_p (op)")))

;; Test for a pc-relative call operand
(define_predicate "constant_call_address_operand"
  (match_code "symbol_ref")
{
  if (ix86_cmodel == CM_LARGE || ix86_cmodel == CM_LARGE_PIC
      || flag_force_indirect_call)
    return false;
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES && SYMBOL_REF_DLLIMPORT_P (op))
    return false;
  return true;
})

;; P6 processors will jump to the address after the decrement when %esp
;; is used as a call operand, so they will execute return address as a code.
;; See Pentium Pro errata 70, Pentium 2 errata A33 and Pentium 3 errata E17.

(define_predicate "call_register_no_elim_operand"
  (match_operand 0 "register_operand")
{
  if (SUBREG_P (op))
    op = SUBREG_REG (op);

  if (!TARGET_64BIT && op == stack_pointer_rtx)
    return false;

  return register_no_elim_operand (op, mode);
})

;; True for any non-virtual or eliminable register.  Used in places where
;; instantiation of such a register may cause the pattern to not be recognized.
(define_predicate "register_no_elim_operand"
  (match_operand 0 "register_operand")
{
  if (SUBREG_P (op))
    op = SUBREG_REG (op);

  /* Before reload, we can allow (SUBREG (MEM...)) as a register operand
     because it is guaranteed to be reloaded into one.  */
  if (MEM_P (op))
    return true;

  return !(op == arg_pointer_rtx
	   || op == frame_pointer_rtx
	   || IN_RANGE (REGNO (op),
			FIRST_PSEUDO_REGISTER, LAST_VIRTUAL_REGISTER));
})

;; Similarly, but include the stack pointer.  This is used to prevent esp
;; from being used as an index reg.
(define_predicate "index_register_operand"
  (match_operand 0 "register_operand")
{
  if (SUBREG_P (op))
    op = SUBREG_REG (op);

  if (reload_completed)
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

;; Test for a valid operand for indirect branch.
(define_predicate "indirect_branch_operand"
  (ior (match_operand 0 "register_operand")
       (and (not (match_test "TARGET_INDIRECT_BRANCH_REGISTER"))
	    (not (match_test "TARGET_X32"))
	    (match_operand 0 "memory_operand"))))

;; Return true if OP is a memory operands that can be used in sibcalls.
;; Since sibcall never returns, we can only use call-clobbered register
;; as GOT base.  Allow GOT slot here only with pseudo register as GOT
;; base.  Properly handle sibcall over GOT slot with *sibcall_GOT_32
;; and *sibcall_value_GOT_32 patterns.
(define_predicate "sibcall_memory_operand"
  (match_operand 0 "memory_operand")
{
  op = XEXP (op, 0);
  if (CONSTANT_P (op))
    return true;
  if (GET_CODE (op) == PLUS && REG_P (XEXP (op, 0)))
    {
      int regno = REGNO (XEXP (op, 0));
      if (!HARD_REGISTER_NUM_P (regno) || call_used_or_fixed_reg_p (regno))
	{
	  op = XEXP (op, 1);
	  if (GOT32_symbol_operand (op, VOIDmode))
	    return true;
	}
    }
  return false;
})

;; Return true if OP is a GOT memory operand.
(define_predicate "GOT_memory_operand"
  (and (match_operand 0 "memory_operand")
       (match_code "const" "0")
       (match_code "unspec" "00")
       (match_test "XINT (XEXP (XEXP (op, 0), 0), 1) == UNSPEC_GOTPCREL")))

;; Test for a valid operand for a call instruction.
;; Allow constant call address operands in Pmode only.
(define_special_predicate "call_insn_operand"
  (ior (match_test "constant_call_address_operand
		     (op, mode == VOIDmode ? mode : Pmode)")
       (match_operand 0 "call_register_no_elim_operand")
       (and (not (match_test "TARGET_INDIRECT_BRANCH_REGISTER"))
	    (ior (and (not (match_test "TARGET_X32"))
		      (match_operand 0 "memory_operand"))
		 (and (match_test "TARGET_X32 && Pmode == DImode")
		      (match_operand 0 "GOT_memory_operand"))))))

;; Similarly, but for tail calls, in which we cannot allow memory references.
(define_special_predicate "sibcall_insn_operand"
  (ior (match_test "constant_call_address_operand
		     (op, mode == VOIDmode ? mode : Pmode)")
       (match_operand 0 "register_no_elim_operand")
       (and (not (match_test "TARGET_INDIRECT_BRANCH_REGISTER"))
	    (ior (and (not (match_test "TARGET_X32"))
		      (match_operand 0 "sibcall_memory_operand"))
		 (and (match_test "TARGET_X32 && Pmode == DImode")
		      (match_operand 0 "GOT_memory_operand"))))))

;; Return true if OP is a 32-bit GOT symbol operand.
(define_predicate "GOT32_symbol_operand"
  (and (match_code "const")
       (match_code "unspec" "0")
       (match_test "XINT (XEXP (op, 0), 1) == UNSPEC_GOT")))

;; Match exactly zero.
(define_predicate "const0_operand"
  (match_code "const_int,const_double,const_vector")
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);
  return op == CONST0_RTX (mode);
})

;; Match one or a vector with all elements equal to one.
(define_predicate "const1_operand"
  (match_code "const_int,const_double,const_vector")
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);
  return op == CONST1_RTX (mode);
})

;; Match exactly -1.
(define_predicate "constm1_operand"
  (and (match_code "const_int")
       (match_test "op == constm1_rtx")))

;; Match exactly eight.
(define_predicate "const8_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 8")))

;; Match exactly 128.
(define_predicate "const128_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 128")))

;; Match exactly 0x0FFFFFFFF in anddi as a zero-extension operation
(define_predicate "const_32bit_mask"
  (and (match_code "const_int")
       (match_test "trunc_int_for_mode (INTVAL (op), DImode)
		    == (HOST_WIDE_INT) 0xffffffff")))

;; Match 2, 4, or 8.  Used for leal multiplicands.
(define_predicate "const248_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 2 || i == 4 || i == 8;
})

;; Match 1, 2, or 3.  Used for lea shift amounts.
(define_predicate "const123_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 1 || i == 2 || i == 3;
})

;; Match 2, 3, 6, or 7
(define_predicate "const2367_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 2 || i == 3 || i == 6 || i == 7;
})

;; Match 1, 2, 4, or 8
(define_predicate "const1248_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 1 || i == 2 || i == 4 || i == 8;
})

;; Match 3, 5, or 9.  Used for leal multiplicands.
(define_predicate "const359_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 3 || i == 5 || i == 9;
})

;; Match 4 or 8 to 11.  Used for embeded rounding.
(define_predicate "const_4_or_8_to_11_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 4 || (i >= 8 && i <= 11);
})

;; Match 4 or 8. Used for SAE.
(define_predicate "const48_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT i = INTVAL (op);
  return i == 4 || i == 8;
})

;; Match 0 or 1.
(define_predicate "const_0_to_1_operand"
  (and (match_code "const_int")
       (ior (match_test "op == const0_rtx")
	    (match_test "op == const1_rtx"))))

;; Match 0 to 3.
(define_predicate "const_0_to_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 3)")))

;; Match 0 to 4.
(define_predicate "const_0_to_4_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 4)")))

;; Match 0 to 5.
(define_predicate "const_0_to_5_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 5)")))

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

;; Match 0 to 127.
(define_predicate "const_0_to_127_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 127)")))

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

;; Match 1 to 255 except multiples of 8
(define_predicate "const_0_to_255_not_mul_8_operand"
  (match_code "const_int")
{
  unsigned HOST_WIDE_INT val = INTVAL (op);
  return val <= 255 && val % 8 != 0;
})

;; Return true if OP is CONST_INT >= 1 and <= 31 (a valid operand
;; for shift & compare patterns, as shifting by 0 does not change flags).
(define_predicate "const_1_to_31_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 31)")))

;; Return true if OP is CONST_INT >= 1 and <= 63 (a valid operand
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

;; Match 8 to 9.
(define_predicate "const_8_to_9_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 8, 9)")))

;; Match 8 to 11.
(define_predicate "const_8_to_11_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 8, 11)")))

;; Match 8 to 15.
(define_predicate "const_8_to_15_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 8, 15)")))

;; Match 10 to 11.
(define_predicate "const_10_to_11_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 10, 11)")))

;; Match 12 to 13.
(define_predicate "const_12_to_13_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 12, 13)")))

;; Match 12 to 15.
(define_predicate "const_12_to_15_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 12, 15)")))

;; Match 14 to 15.
(define_predicate "const_14_to_15_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 14, 15)")))

;; Match 16 to 19.
(define_predicate "const_16_to_19_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 16, 19)")))

;; Match 16 to 31.
(define_predicate "const_16_to_31_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 16, 31)")))

;; Match 20 to 23.
(define_predicate "const_20_to_23_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 20, 23)")))

;; Match 24 to 27.
(define_predicate "const_24_to_27_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 24, 27)")))

;; Match 28 to 31.
(define_predicate "const_28_to_31_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 28, 31)")))

;; True if this is a constant appropriate for an increment or decrement.
(define_predicate "incdec_operand"
  (match_code "const_int")
{
  /* On Pentium4, the inc and dec operations causes extra dependency on flag
     registers, since carry flag is not set.  */
  if (!TARGET_USE_INCDEC && !optimize_insn_for_size_p ())
    return false;
  return op == const1_rtx || op == constm1_rtx;
})

;; True for registers, or const_int_operand, used to vec_setm expander.
(define_predicate "vec_setm_sse41_operand"
  (ior (and (match_operand 0 "register_operand")
	    (match_test "TARGET_SSE4_1"))
       (match_code "const_int")))

(define_predicate "vec_setm_avx2_operand"
  (ior (and (match_operand 0 "register_operand")
	    (match_test "TARGET_AVX2"))
       (match_code "const_int")))

(define_predicate "vec_setm_mmx_operand"
  (ior (and (match_operand 0 "register_operand")
	    (match_test "TARGET_SSE4_1")
	    (match_test "TARGET_MMX_WITH_SSE"))
       (match_code "const_int")))

;; True for registers, or 1 or -1.  Used to optimize double-word shifts.
(define_predicate "reg_or_pm1_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (ior (match_test "op == const1_rtx")
		 (match_test "op == constm1_rtx")))))

;; True for registers, or (not: registers).  Used to optimize 3-operand
;; bitwise operation.
(define_predicate "regmem_or_bitnot_regmem_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (and (match_code "not")
	    (match_test "nonimmediate_operand (XEXP (op, 0), mode)"))))

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
  op = avoid_constant_pool_reference (op);

  if (GET_CODE (op) != CONST_VECTOR)
    return false;

  n_elts = CONST_VECTOR_NUNITS (op);

  for (n_elts--; n_elts > 0; n_elts--)
    {
      rtx elt = CONST_VECTOR_ELT (op, n_elts);
      if (elt != CONST0_RTX (GET_MODE_INNER (GET_MODE (op))))
	return false;
    }
  return true;
})

/* Return true if operand is a float vector constant that is all ones. */
(define_predicate "float_vector_all_ones_operand"
  (match_code "const_vector,mem")
{
  mode = GET_MODE (op);
  if (!FLOAT_MODE_P (mode)
      || (MEM_P (op)
	  && (!SYMBOL_REF_P (XEXP (op, 0))
	      || !CONSTANT_POOL_ADDRESS_P (XEXP (op, 0)))))
    return false;

  if (MEM_P (op))
    {
      op = get_pool_constant (XEXP (op, 0));
      if (GET_CODE (op) != CONST_VECTOR)
	return false;

      if (GET_MODE (op) != mode
	 && INTEGRAL_MODE_P (GET_MODE (op))
	 && op == CONSTM1_RTX (GET_MODE (op)))
	return true;
    }

  rtx first = XVECEXP (op, 0, 0);
  for (int i = 1; i != GET_MODE_NUNITS (GET_MODE (op)); i++)
    {
      rtx tmp = XVECEXP (op, 0, i);
      if (!rtx_equal_p (tmp, first))
	return false;
    }
  if (GET_MODE (first) == E_SFmode)
    {
      long l;
      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (first), l);
      return (l & 0xffffffff) == 0xffffffff;
    }
  else if (GET_MODE (first) == E_DFmode)
    {
      long l[2];
      REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (first), l);
      return ((l[0] & 0xffffffff) == 0xffffffff
	     && (l[1] & 0xffffffff) == 0xffffffff);
    }
  else
    return false;
})

/* Return true if operand is a vector constant that is all ones. */
(define_predicate "vector_all_ones_operand"
  (and (match_code "const_vector")
       (match_test "INTEGRAL_MODE_P (GET_MODE (op))")
       (match_test "op == CONSTM1_RTX (GET_MODE (op))")))

/* Return true if operand is an 128/256bit all ones vector
   that zero-extends to 256/512bit.  */
(define_predicate "vector_all_ones_zero_extend_half_operand"
  (match_code "const_vector")
{
  mode = GET_MODE (op);
  if (GET_MODE_CLASS (mode) != MODE_VECTOR_INT
      || (GET_MODE_SIZE (mode) != 32
	  && GET_MODE_SIZE (mode) != 64))
    return false;

  int nelts = CONST_VECTOR_NUNITS (op);
  for (int i = 0; i != nelts; i++)
    {
      rtx elt = CONST_VECTOR_ELT (op, i);
      if (i < nelts / 2
	  && elt != CONSTM1_RTX (GET_MODE_INNER (mode)))
	return false;
      if (i >= nelts / 2
	  && elt != CONST0_RTX (GET_MODE_INNER (mode)))
	return false;
    }
  return true;
})

/* Return true if operand is an 128bit all ones vector
   that zero extends to 512bit.  */
(define_predicate "vector_all_ones_zero_extend_quarter_operand"
  (match_code "const_vector")
{
  mode = GET_MODE (op);
  if (GET_MODE_CLASS (mode) != MODE_VECTOR_INT
      || GET_MODE_SIZE (mode) != 64)
    return false;

  int nelts = CONST_VECTOR_NUNITS (op);
  for (int i = 0; i != nelts; i++)
    {
      rtx elt = CONST_VECTOR_ELT (op, i);
      if (i < nelts / 4
	  && elt != CONSTM1_RTX (GET_MODE_INNER (mode)))
	return false;
      if (i >= nelts / 4
	  && elt != CONST0_RTX (GET_MODE_INNER (mode)))
	return false;
    }
  return true;
})

; Return true when OP is operand acceptable for vector memory operand.
; Only AVX can have misaligned memory operand.
(define_predicate "vector_memory_operand"
  (and (match_operand 0 "memory_operand")
       (ior (match_test "TARGET_AVX")
	    (match_test "MEM_ALIGN (op) >= GET_MODE_ALIGNMENT (mode)"))))

; Return true when OP is register_operand or vector_memory_operand.
(define_predicate "vector_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "vector_memory_operand")))

; Return true when OP is register_operand, vector_memory_operand
; or const_vector.
(define_predicate "vector_or_const_vector_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "vector_memory_operand")
       (match_code "const_vector")))

(define_predicate "bcst_mem_operand"
  (and (match_code "vec_duplicate")
       (and (match_test "TARGET_AVX512F")
	    (ior (match_test "TARGET_AVX512VL")
		 (match_test "GET_MODE_SIZE (GET_MODE (op)) == 64")))
       (match_test "VALID_BCST_MODE_P (GET_MODE_INNER (GET_MODE (op)))")
       (match_test "GET_MODE (XEXP (op, 0))
		    == GET_MODE_INNER (GET_MODE (op))")
       (match_test "memory_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)))")))

; Return true when OP is bcst_mem_operand or vector_memory_operand.
(define_predicate "bcst_vector_operand"
  (ior (match_operand 0 "vector_operand")
       (match_operand 0 "bcst_mem_operand")))

;; Return true when OP is either nonimmediate operand, or any
;; CONST_VECTOR.
(define_predicate "nonimmediate_or_const_vector_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_code "const_vector")))

(define_predicate "nonimmediate_or_const_vec_dup_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_test "const_vec_duplicate_p (op)")))

;; Return true when OP is either register operand, or any
;; CONST_VECTOR.
(define_predicate "reg_or_const_vector_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "const_vector")))

;; Return true when OP is CONST_VECTOR which can be converted to a
;; sign extended 32-bit integer.
(define_predicate "x86_64_const_vector_operand"
  (match_code "const_vector")
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);
  else if (GET_MODE (op) != mode)
    return false;
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return false;
  HOST_WIDE_INT val = ix86_convert_const_vector_to_integer (op, mode);
  return trunc_int_for_mode (val, SImode) == val;
})

(define_predicate "nonimmediate_or_x86_64_const_vector_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "x86_64_const_vector_operand")))

;; Return true when OP is nonimmediate or standard SSE constant.
(define_predicate "nonimmediate_or_sse_const_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_test "standard_sse_constant_p (op, mode)")))

;; Return true if OP is a register or a zero.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

; Return true when OP is a nonimmediate or zero.
(define_predicate "nonimm_or_0_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "const0_operand")))

(define_predicate "norex_memory_operand"
  (and (match_operand 0 "memory_operand")
       (not (match_test "x86_extended_reg_mentioned_p (op)"))))

;; Return true for RTX codes that force SImode address.
(define_predicate "SImode_address_operand"
  (match_code "subreg,zero_extend,and"))

;; Return true if op is a valid address for LEA, and does not contain
;; a segment override.  Defined as a special predicate to allow
;; mode-less const_int operands pass to address_operand.
(define_special_predicate "address_no_seg_operand"
  (match_test "address_operand (op, VOIDmode)")
{
  struct ix86_address parts;
  int ok;

  if (!CONST_INT_P (op)
      && mode != VOIDmode
      && GET_MODE (op) != mode)
    return false;

  ok = ix86_decompose_address (op, &parts);
  gcc_assert (ok);
  return parts.seg == ADDR_SPACE_GENERIC;
})

;; Return true if op if a valid base register, displacement or
;; sum of base register and displacement for VSIB addressing.
(define_predicate "vsib_address_operand"
  (match_test "address_operand (op, VOIDmode)")
{
  struct ix86_address parts;
  int ok;
  rtx disp;

  ok = ix86_decompose_address (op, &parts);
  gcc_assert (ok);
  if (parts.index || parts.seg != ADDR_SPACE_GENERIC)
    return false;

  /* VSIB addressing doesn't support (%rip).  */
  if (parts.disp)
    {
      disp = parts.disp;
      if (GET_CODE (disp) == CONST)
	{
	  disp = XEXP (disp, 0);
	  if (GET_CODE (disp) == PLUS)
	    disp = XEXP (disp, 0);
	  if (GET_CODE (disp) == UNSPEC)
	    switch (XINT (disp, 1))
	      {
	      case UNSPEC_GOTPCREL:
	      case UNSPEC_PCREL:
	      case UNSPEC_GOTNTPOFF:
		return false;
	      }
	}
      if (TARGET_64BIT
	  && flag_pic
	  && (GET_CODE (disp) == SYMBOL_REF
	      || GET_CODE (disp) == LABEL_REF))
	return false;
    }

  return true;
})

(define_predicate "vsib_mem_operator"
  (match_code "mem"))

;; Return true if the rtx is known to be at least 32 bits aligned.
(define_predicate "aligned_operand"
  (match_operand 0 "general_operand")
{
  struct ix86_address parts;
  int ok;

  /* Registers and immediate operands are always "aligned".  */
  if (!MEM_P (op))
    return true;

  /* All patterns using aligned_operand on memory operands ends up
     in promoting memory operand to 64bit and thus causing memory mismatch.  */
  if (TARGET_MEMORY_MISMATCH_STALL && !optimize_insn_for_size_p ())
    return false;

  /* Don't even try to do any aligned optimizations with volatiles.  */
  if (MEM_VOLATILE_P (op))
    return false;

  if (MEM_ALIGN (op) >= 32)
    return true;

  op = XEXP (op, 0);

  /* Pushes and pops are only valid on the stack pointer.  */
  if (GET_CODE (op) == PRE_DEC
      || GET_CODE (op) == POST_INC)
    return true;

  /* Decode the address.  */
  ok = ix86_decompose_address (op, &parts);
  gcc_assert (ok);

  if (parts.base && SUBREG_P (parts.base))
    parts.base = SUBREG_REG (parts.base);
  if (parts.index && SUBREG_P (parts.index))
    parts.index = SUBREG_REG (parts.index);

  /* Look for some component that isn't known to be aligned.  */
  if (parts.index)
    {
      if (REGNO_POINTER_ALIGN (REGNO (parts.index)) * parts.scale < 32)
	return false;
    }
  if (parts.base)
    {
      if (REGNO_POINTER_ALIGN (REGNO (parts.base)) < 32)
	return false;
    }
  if (parts.disp)
    {
      if (!CONST_INT_P (parts.disp)
	  || (INTVAL (parts.disp) & 3))
	return false;
    }

  /* Didn't find one -- this must be an aligned address.  */
  return true;
})

;; Return true if OP is memory operand with a displacement.
(define_predicate "memory_displacement_operand"
  (match_operand 0 "memory_operand")
{
  struct ix86_address parts;
  int ok;

  ok = ix86_decompose_address (XEXP (op, 0), &parts);
  gcc_assert (ok);
  return parts.disp != NULL_RTX;
})

;; Return true if OP is memory operand with a displacement only.
(define_predicate "memory_displacement_only_operand"
  (match_operand 0 "memory_operand")
{
  struct ix86_address parts;
  int ok;

  if (TARGET_64BIT)
    return false;

  ok = ix86_decompose_address (XEXP (op, 0), &parts);
  gcc_assert (ok);

  if (parts.base || parts.index)
    return false;

  return parts.disp != NULL_RTX;
})

;; Return true if OP is memory operand that cannot be represented
;; by the modRM array.
(define_predicate "long_memory_operand"
  (and (match_operand 0 "memory_operand")
       (match_test "memory_address_length (op, false)")))

;; Return true if OP is a comparison operator that can be issued by fcmov.
(define_predicate "fcmov_comparison_operator"
  (match_operand 0 "comparison_operator")
{
  machine_mode inmode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);

  if (inmode == CCFPmode)
    code = ix86_fp_compare_code_to_integer (code);

  /* i387 supports just limited amount of conditional codes.  */
  switch (code)
    {
    case GEU: case LTU:
      if (inmode == CCCmode || inmode == CCGZmode)
	return true;
      /* FALLTHRU */
    case GTU: case LEU:
      if (inmode == CCmode || inmode == CCFPmode)
	return true;
      return false;
    case ORDERED: case UNORDERED:
    case EQ: case NE:
      return true;
    default:
      return false;
    }
})

;; Return true if OP is a comparison that can be used in the CMPSS/CMPPS insns.
;; The first set are supported directly; the second set can't be done with
;; full IEEE support, i.e. NaNs.

(define_predicate "sse_comparison_operator"
  (ior (match_code "eq,ne,lt,le,unordered,unge,ungt,ordered")
       (and (match_test "TARGET_AVX")
	    (match_code "ge,gt,uneq,unle,unlt,ltgt"))))

(define_predicate "ix86_comparison_int_operator"
  (match_code "ne,eq,ge,gt,le,lt"))

(define_predicate "ix86_comparison_uns_operator"
  (match_code "ne,eq,geu,gtu,leu,ltu"))

(define_predicate "bt_comparison_operator"
  (match_code "ne,eq"))

(define_predicate "shr_comparison_operator"
  (match_code "gtu,leu"))

(define_predicate "add_comparison_operator"
  (match_code "geu,ltu"))

;; Return true if OP is a valid comparison operator in valid mode.
(define_predicate "ix86_comparison_operator"
  (match_operand 0 "comparison_operator")
{
  machine_mode inmode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);

  if (inmode == CCFPmode)
    return ix86_trivial_fp_comparison_operator (op, mode);

  switch (code)
    {
    case EQ: case NE:
      if (inmode == CCGZmode)
	return false;
      return true;
    case GE: case LT:
      if (inmode == CCmode || inmode == CCGCmode
	  || inmode == CCGOCmode || inmode == CCNOmode || inmode == CCGZmode)
	return true;
      return false;
    case GEU: case LTU:
      if (inmode == CCCmode || inmode == CCGZmode)
	return true;
      /* FALLTHRU */
    case GTU: case LEU:
      if (inmode == CCmode)
	return true;
      return false;
    case ORDERED: case UNORDERED:
      if (inmode == CCmode)
	return true;
      return false;
    case GT: case LE:
      if (inmode == CCmode || inmode == CCGCmode || inmode == CCNOmode)
	return true;
      return false;
    default:
      return false;
    }
})

;; Return true if OP is a valid comparison operator
;; testing carry flag to be set.
(define_predicate "ix86_carry_flag_operator"
  (match_code "ltu,unlt")
{
  machine_mode inmode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);

  if (inmode == CCFPmode)
    code = ix86_fp_compare_code_to_integer (code);
  else if (inmode != CCmode && inmode != CCCmode && inmode != CCGZmode)
    return false;

  return code == LTU;
})

;; Return true if OP is a valid comparison operator
;; testing carry flag to be unset.
(define_predicate "ix86_carry_flag_unset_operator"
  (match_code "geu,ge")
{
  machine_mode inmode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);

  if (inmode == CCFPmode)
    code = ix86_fp_compare_code_to_integer (code);
  else if (inmode != CCmode && inmode != CCCmode && inmode != CCGZmode)
    return false;

  return code == GEU;
})

;; Return true if this comparison only requires testing one flag bit.
(define_predicate "ix86_trivial_fp_comparison_operator"
  (match_code "gt,ge,unlt,unle,uneq,ltgt,ordered,unordered"))

;; Return true if we know how to do this comparison.  Others require
;; testing more than one flag bit, and we let the generic middle-end
;; code do that.
(define_predicate "ix86_fp_comparison_operator"
  (if_then_else (match_test "ix86_fp_comparison_strategy (GET_CODE (op))
                             == IX86_FPCMP_ARITH")
               (match_operand 0 "comparison_operator")
               (match_operand 0 "ix86_trivial_fp_comparison_operator")))

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

;; Return true if this is a and, ior or xor operation.
(define_predicate "logic_operator"
  (match_code "and,ior,xor"))

;; Return true if this is a plus, minus, and, ior or xor operation.
(define_predicate "plusminuslogic_operator"
  (match_code "plus,minus,and,ior,xor"))

;; Return true for ARITHMETIC_P.
(define_predicate "arith_or_logical_operator"
  (match_code "plus,mult,and,ior,xor,smin,smax,umin,umax,compare,minus,div,
	       mod,udiv,umod,ashift,rotate,ashiftrt,lshiftrt,rotatert"))

;; Return true for COMMUTATIVE_P.
(define_predicate "commutative_operator"
  (match_code "plus,mult,and,ior,xor,smin,smax,umin,umax"))

;; Return true if OP is a binary operator that can be promoted to wider mode.
(define_predicate "promotable_binary_operator"
  (ior (match_code "plus,minus,and,ior,xor,ashift")
       (and (match_code "mult")
	    (match_test "TARGET_TUNE_PROMOTE_HIMODE_IMUL"))))

(define_predicate "compare_operator"
  (match_code "compare"))

;; Return true if OP is a memory operand, aligned to
;; less than its natural alignment.
(define_predicate "misaligned_operand"
  (and (match_code "mem")
       (match_test "MEM_ALIGN (op) < GET_MODE_BITSIZE (mode)")))

;; Return true if OP is a parallel for an mov{d,q,dqa,ps,pd} vec_select,
;; where one of the two operands of the vec_concat is const0_operand.
(define_predicate "movq_parallel"
  (match_code "parallel")
{
  unsigned nelt = XVECLEN (op, 0);
  unsigned nelt2 = nelt >> 1;
  unsigned i;

  if (nelt < 2)
    return false;

  /* Validate that all of the elements are constants,
     lower halves of permute are lower halves of the first operand,
     upper halves of permute come from any of the second operand.  */
  for (i = 0; i < nelt; ++i)
    {
      rtx er = XVECEXP (op, 0, i);
      unsigned HOST_WIDE_INT ei;

      if (!CONST_INT_P (er))
	return false;
      ei = INTVAL (er);
      if (i < nelt2 && ei != i)
	return false;
      if (i >= nelt2 && (ei < nelt || ei >= nelt << 1))
	return false;
    }

  return true;
})

;; Return true if OP is a vzeroall operation, known to be a PARALLEL.
(define_predicate "vzeroall_operation"
  (match_code "parallel")
{
  unsigned i, nregs = TARGET_64BIT ? 16 : 8;

  if ((unsigned) XVECLEN (op, 0) != 1 + nregs)
    return false;

  for (i = 0; i < nregs; i++)
    {
      rtx elt = XVECEXP (op, 0, i+1);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != V8SImode
	  || REGNO (SET_DEST (elt)) != GET_SSE_REGNO (i)
	  || SET_SRC (elt) != CONST0_RTX (V8SImode))
	return false;
    }
  return true;
})

;; return true if OP is a vzeroall pattern.
(define_predicate "vzeroall_pattern"
  (and (match_code "parallel")
       (match_code "unspec_volatile" "a")
       (match_test "XINT (XVECEXP (op, 0, 0), 1) == UNSPECV_VZEROALL")))

;; return true if OP is a vzeroupper pattern.
(define_predicate "vzeroupper_pattern"
  (and (match_code "parallel")
       (match_code "unspec" "b")
       (match_test "XINT (XVECEXP (op, 0, 1), 1) == UNSPEC_CALLEE_ABI")
       (match_test "INTVAL (XVECEXP (XVECEXP (op, 0, 1), 0, 0)) == ABI_VZEROUPPER")))

;; Return true if OP is an addsub vec_merge operation
(define_predicate "addsub_vm_operator"
  (match_code "vec_merge")
{
  rtx op0, op1;
  int swapped;
  HOST_WIDE_INT mask;
  int nunits, elt;

  op0 = XEXP (op, 0);
  op1 = XEXP (op, 1);

  /* Sanity check.  */
  if (GET_CODE (op0) == MINUS && GET_CODE (op1) == PLUS)
    swapped = 0;
  else if (GET_CODE (op0) == PLUS && GET_CODE (op1) == MINUS)
    swapped = 1;
  else
    gcc_unreachable ();

  mask = INTVAL (XEXP (op, 2));
  nunits = GET_MODE_NUNITS (mode);

  for (elt = 0; elt < nunits; elt++)
    {
      /* bit clear: take from op0, set: take from op1  */
      int bit = !(mask & (HOST_WIDE_INT_1U << elt));

      if (bit != ((elt & 1) ^ swapped))
	return false;
    }

  return true;
})

;; Return true if OP is an addsub vec_select/vec_concat operation
(define_predicate "addsub_vs_operator"
  (and (match_code "vec_select")
       (match_code "vec_concat" "0"))
{
  rtx op0, op1;
  bool swapped;
  int nunits, elt;

  op0 = XEXP (XEXP (op, 0), 0);
  op1 = XEXP (XEXP (op, 0), 1);

  /* Sanity check.  */
  if (GET_CODE (op0) == MINUS && GET_CODE (op1) == PLUS)
    swapped = false;
  else if (GET_CODE (op0) == PLUS && GET_CODE (op1) == MINUS)
    swapped = true;
  else
    gcc_unreachable ();

  nunits = GET_MODE_NUNITS (mode);
  if (XVECLEN (XEXP (op, 1), 0) != nunits)
    return false;

  /* We already checked that permutation is suitable for addsub,
     so only look at the first element of the parallel.  */
  elt = INTVAL (XVECEXP (XEXP (op, 1), 0, 0));

  return elt == (swapped ? nunits : 0);
})

;; Return true if OP is a parallel for an addsub vec_select.
(define_predicate "addsub_vs_parallel"
  (and (match_code "parallel")
       (match_code "const_int" "a"))
{
  int nelt = XVECLEN (op, 0);
  int elt, i;
  
  if (nelt < 2)
    return false;

  /* Check that the permutation is suitable for addsub.
     For example, { 0 9 2 11 4 13 6 15 } or { 8 1 10 3 12 5 14 7 }.  */
  elt = INTVAL (XVECEXP (op, 0, 0));
  if (elt == 0)
    {
      for (i = 1; i < nelt; ++i)
	if (INTVAL (XVECEXP (op, 0, i)) != (i + (i & 1) * nelt))
	  return false;
    }
  else if (elt == nelt)
    {
      for (i = 1; i < nelt; ++i)
	if (INTVAL (XVECEXP (op, 0, i)) != (elt + i - (i & 1) * nelt))
	  return false;
    }
  else
    return false;

  return true;
})

;; Return true if OP is a constant pool in perm{w,d,b} which constains index
;; match pmov{dw,wb,qd}.
(define_predicate "permvar_truncate_operand"
 (match_code "mem")
{
  int nelt = GET_MODE_NUNITS (mode);
  int perm[128];
  int id;

  if (!INTEGRAL_MODE_P (mode) || !VECTOR_MODE_P (mode))
    return false;

  if (nelt < 2)
    return false;

  if (!ix86_extract_perm_from_pool_constant (&perm[0], op))
    return false;

  id = exact_log2 (nelt);

  /* Check that the permutation is suitable for pmovz{bw,wd,dq}.
     For example V16HImode to V8HImode
     { 0 2 4 6 8 10 12 14 * * * * * * * * }.  */
  for (int i = 0; i != nelt / 2; i++)
    if ((perm[i] & ((1 << id) - 1)) != i * 2)
      return false;

  return true;
})

;; Return true if OP is a constant pool in shufb which constains index
;; match pmovdw.
(define_predicate "pshufb_truncv4siv4hi_operand"
 (match_code "mem")
{
  int perm[128];

  if (mode != E_V16QImode)
    return false;

  if (!ix86_extract_perm_from_pool_constant (&perm[0], op))
    return false;

  /* Check that the permutation is suitable for pmovdw.
     For example V4SImode to V4HImode
     { 0 1 4 5 8 9 12 13 * * * * * * * * }.
     index = i % 2 + (i / 2) * 4.  */
  for (int i = 0; i != 8; i++)
    {
      /* if (SRC2[(i * 8)+7] = 1) then DEST[(i*8)+7..(i*8)+0] := 0;  */
      if (perm[i] & 128)
	return false;

      if ((perm[i] & 15) != ((i & 1) + (i & 0xFE) * 2))
	return false;
     }

  return true;
})

;; Return true if OP is a constant pool in shufb which constains index
;; match pmovdw.
(define_predicate "pshufb_truncv8hiv8qi_operand"
 (match_code "mem")
{
  int perm[128];

  if (mode != E_V16QImode)
    return false;

  if (!ix86_extract_perm_from_pool_constant (&perm[0], op))
    return false;

  /* Check that the permutation is suitable for pmovwb.
     For example V16QImode to V8QImode
     { 0 2 4 6 8 10 12 14 * * * * * * * * }.
     index = i % 2 + (i / 2) * 4.  */
  for (int i = 0; i != 8; i++)
    {
      /* if (SRC2[(i * 8)+7] = 1) then DEST[(i*8)+7..(i*8)+0] := 0;  */
      if (perm[i] & 128)
	return false;

      if ((perm[i] & 15) != i * 2)
	 return false;
    }

  return true;
})

;; Return true if OP is a parallel for an pmovz{bw,wd,dq} vec_select,
;; where one of the two operands of the vec_concat is const0_operand.
(define_predicate "pmovzx_parallel"
  (and (match_code "parallel")
       (match_code "const_int" "a"))
{
  int nelt = XVECLEN (op, 0);
  int elt, i;

  if (nelt < 2)
    return false;

  /* Check that the permutation is suitable for pmovz{bw,wd,dq}.
     For example { 0 16 1 17 2 18 3 19 4 20 5 21 6 22 7 23 }.  */
  elt = INTVAL (XVECEXP (op, 0, 0));
  if (elt == 0)
    {
      for (i = 1; i < nelt; ++i)
	if ((i & 1) != 0)
	  {
	    if (INTVAL (XVECEXP (op, 0, i)) < nelt)
	      return false;
	  }
	else if (INTVAL (XVECEXP (op, 0, i)) != i / 2)
	  return false;
    }
  else
    return false;

  return true;
})

;; Return true if OP is a const vector with duplicate value.
(define_predicate "const_vector_duplicate_operand"
  (match_code "const_vector")
{
  rtx elt = XVECEXP (op, 0, 0);
  int i, nelt = XVECLEN (op, 0);

  for (i = 1; i < nelt; ++i)
    if (!rtx_equal_p (elt, XVECEXP (op, 0, i)))
      return false;
  return true;
})

;; Return true if OP is a parallel for a vbroadcast permute.
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

;; Return true if OP is a parallel for a palignr permute.
(define_predicate "palignr_operand"
  (and (match_code "parallel")
       (match_code "const_int" "a"))
{
  int elt = INTVAL (XVECEXP (op, 0, 0));
  int i, nelt = XVECLEN (op, 0);

  /* Check that an order in the permutation is suitable for palignr.
     For example, {5 6 7 0 1 2 3 4} is "palignr 5, xmm, xmm".  */
  for (i = 1; i < nelt; ++i)
    if (INTVAL (XVECEXP (op, 0, i)) != ((elt + i) % nelt))
      return false;
  return true;
})

;; Return true if OP is a proper third operand to vpblendw256.
(define_predicate "avx2_pblendw_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT val = INTVAL (op);
  HOST_WIDE_INT low = val & 0xff;
  return val == ((low << 8) | low);
})

;; Return true if OP is vector_operand or CONST_VECTOR.
(define_predicate "general_vector_operand"
  (ior (match_operand 0 "vector_operand")
       (match_code "const_vector")))

;; Return true if OP is either -1 constant or stored in register.
(define_predicate "register_or_constm1_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "op == constm1_rtx"))))

;; Return true if the vector ends with between 12 and 18 register saves using
;; RAX as the base address.
(define_predicate "save_multiple"
  (match_code "parallel")
{
  const unsigned len = XVECLEN (op, 0);
  unsigned i;

  /* Starting from end of vector, count register saves.  */
  for (i = 0; i < len; ++i)
    {
      rtx src, dest, addr;
      rtx e = XVECEXP (op, 0, len - 1 - i);

      if (GET_CODE (e) != SET)
	break;

      src  = SET_SRC (e);
      dest = SET_DEST (e);

      if (!REG_P (src) || !MEM_P (dest))
	break;

      addr = XEXP (dest, 0);

      /* Good if dest address is in RAX.  */
      if (REG_P (addr) && REGNO (addr) == AX_REG)
	continue;

      /* Good if dest address is offset of RAX.  */
      if (GET_CODE (addr) == PLUS
	  && REG_P (XEXP (addr, 0))
	  && REGNO (XEXP (addr, 0)) == AX_REG)
	continue;

      break;
    }
  return (i >= 12 && i <= 18);
})


;; Return true if the vector ends with between 12 and 18 register loads using
;; RSI as the base address.
(define_predicate "restore_multiple"
  (match_code "parallel")
{
  const unsigned len = XVECLEN (op, 0);
  unsigned i;

  /* Starting from end of vector, count register restores.  */
  for (i = 0; i < len; ++i)
    {
      rtx src, dest, addr;
      rtx e = XVECEXP (op, 0, len - 1 - i);

      if (GET_CODE (e) != SET)
	break;

      src  = SET_SRC (e);
      dest = SET_DEST (e);

      if (!MEM_P (src) || !REG_P (dest))
	break;

      addr = XEXP (src, 0);

      /* Good if src address is in RSI.  */
      if (REG_P (addr) && REGNO (addr) == SI_REG)
	continue;

      /* Good if src address is offset of RSI.  */
      if (GET_CODE (addr) == PLUS
	  && REG_P (XEXP (addr, 0))
	  && REGNO (XEXP (addr, 0)) == SI_REG)
	continue;

      break;
    }
  return (i >= 12 && i <= 18);
})

;; Keylocker specific predicates
(define_predicate "encodekey128_operation"
  (match_code "parallel")
{
  unsigned i;
  rtx elt;

  if (XVECLEN (op, 0) != 8)
    return false;

  for(i = 0; i < 3; i++)
    {
      elt = XVECEXP (op, 0, i + 1);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != V2DImode
	  || REGNO (SET_DEST (elt)) != GET_SSE_REGNO (i)
	  || GET_CODE (SET_SRC (elt)) != UNSPEC_VOLATILE
	  || GET_MODE (SET_SRC (elt)) != V2DImode
	  || XVECLEN(SET_SRC (elt), 0) != 1
	  || XVECEXP(SET_SRC (elt), 0, 0) != const0_rtx)
	return false;
    }

  for(i = 4; i < 7; i++)
    {
      elt = XVECEXP (op, 0, i);
      if (GET_CODE (elt) != CLOBBER
	  || GET_MODE (elt) != VOIDmode
	  || GET_CODE (XEXP (elt, 0)) != REG
	  || GET_MODE (XEXP (elt, 0)) != V2DImode
	  || REGNO (XEXP (elt, 0)) != GET_SSE_REGNO (i))
	return false;
    }

  elt = XVECEXP (op, 0, 7);
  if (GET_CODE (elt) != CLOBBER
      || GET_MODE (elt) != VOIDmode
      || GET_CODE (XEXP (elt, 0)) != REG
      || GET_MODE (XEXP (elt, 0)) != CCmode
      || REGNO (XEXP (elt, 0)) != FLAGS_REG)
    return false;
  return true;
})

(define_predicate "encodekey256_operation"
  (match_code "parallel")
{
  unsigned i;
  rtx elt;

  if (XVECLEN (op, 0) != 9)
    return false;

  elt = SET_SRC (XVECEXP (op, 0, 0));
  elt = XVECEXP (elt, 0, 2);
  if (!REG_P (elt)
      || REGNO(elt) != GET_SSE_REGNO (1))
    return false;

  for(i = 0; i < 4; i++)
    {
      elt = XVECEXP (op, 0, i + 1);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != V2DImode
	  || REGNO (SET_DEST (elt)) != GET_SSE_REGNO (i)
	  || GET_CODE (SET_SRC (elt)) != UNSPEC_VOLATILE
	  || GET_MODE (SET_SRC (elt)) != V2DImode
	  || XVECLEN(SET_SRC (elt), 0) != 1
	  || XVECEXP(SET_SRC (elt), 0, 0) != const0_rtx)
	return false;
    }

  for(i = 4; i < 7; i++)
    {
      elt = XVECEXP (op, 0, i + 1);
      if (GET_CODE (elt) != CLOBBER
	  || GET_MODE (elt) != VOIDmode
	  || GET_CODE (XEXP (elt, 0)) != REG
	  || GET_MODE (XEXP (elt, 0)) != V2DImode
	  || REGNO (XEXP (elt, 0)) != GET_SSE_REGNO (i))
	return false;
    }

  elt = XVECEXP (op, 0, 8);
  if (GET_CODE (elt) != CLOBBER
      || GET_MODE (elt) != VOIDmode
      || GET_CODE (XEXP (elt, 0)) != REG
      || GET_MODE (XEXP (elt, 0)) != CCmode
      || REGNO (XEXP (elt, 0)) != FLAGS_REG)
    return false;
  return true;
})


(define_predicate "aeswidekl_operation"
  (match_code "parallel")
{
  unsigned i;
  rtx elt;

  for (i = 0; i < 8; i++)
    {
      elt = XVECEXP (op, 0, i + 1);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != V2DImode
	  || REGNO (SET_DEST (elt)) != GET_SSE_REGNO (i)
	  || GET_CODE (SET_SRC (elt)) != UNSPEC_VOLATILE
	  || GET_MODE (SET_SRC (elt)) != V2DImode
	  || XVECLEN (SET_SRC (elt), 0) != 1
	  || REGNO (XVECEXP (SET_SRC (elt), 0, 0)) != GET_SSE_REGNO (i))
	return false;
    }
  return true;
})
