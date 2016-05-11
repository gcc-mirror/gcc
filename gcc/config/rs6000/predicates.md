;; Predicate definitions for POWER and PowerPC.
;; Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

;; Return 1 for anything except PARALLEL.
(define_predicate "any_operand"
  (match_code "const_int,const_double,const_wide_int,const,symbol_ref,label_ref,subreg,reg,mem"))

;; Return 1 for any PARALLEL.
(define_predicate "any_parallel_operand"
  (match_code "parallel"))

;; Return 1 if op is COUNT register.
(define_predicate "count_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == CTR_REGNO
		    || REGNO (op) > LAST_VIRTUAL_REGISTER")))

;; Return 1 if op is an Altivec register.
(define_predicate "altivec_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 1;

  return ALTIVEC_REGNO_P (REGNO (op));
})

;; Return 1 if op is a VSX register.
(define_predicate "vsx_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 1;

  return VSX_REGNO_P (REGNO (op));
})

;; Return 1 if op is a vector register that operates on floating point vectors
;; (either altivec or VSX).
(define_predicate "vfloat_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 1;

  return VFLOAT_REGNO_P (REGNO (op));
})

;; Return 1 if op is a vector register that operates on integer vectors
;; (only altivec, VSX doesn't support integer vectors)
(define_predicate "vint_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 1;

  return VINT_REGNO_P (REGNO (op));
})

;; Return 1 if op is a vector register to do logical operations on (and, or,
;; xor, etc.)
(define_predicate "vlogical_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 1;

  return VLOGICAL_REGNO_P (REGNO (op));
})

;; Return 1 if op is the carry register.
(define_predicate "ca_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  return CA_REGNO_P (REGNO (op));
})

;; Return 1 if op is a signed 5-bit constant integer.
(define_predicate "s5bit_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= -16 && INTVAL (op) <= 15")))

;; Return 1 if op is a unsigned 3-bit constant integer.
(define_predicate "u3bit_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 7")))

;; Return 1 if op is a unsigned 5-bit constant integer.
(define_predicate "u5bit_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 31")))

;; Return 1 if op is a unsigned 6-bit constant integer.
(define_predicate "u6bit_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 63")))

;; Return 1 if op is a signed 8-bit constant integer.
;; Integer multiplication complete more quickly
(define_predicate "s8bit_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= -128 && INTVAL (op) <= 127")))

;; Return 1 if op is a unsigned 10-bit constant integer.
(define_predicate "u10bit_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 1023")))

;; Return 1 if op is a constant integer that can fit in a D field.
(define_predicate "short_cint_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_I (op)")))

;; Return 1 if op is a constant integer that can fit in an unsigned D field.
(define_predicate "u_short_cint_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_K (op)")))

;; Return 1 if op is a constant integer that is a signed 16-bit constant
;; shifted left 16 bits
(define_predicate "upper16_cint_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_L (op)")))

;; Return 1 if op is a constant integer that cannot fit in a signed D field.
(define_predicate "non_short_cint_operand"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)
		    (INTVAL (op) + 0x8000) >= 0x10000")))

;; Return 1 if op is a positive constant integer that is an exact power of 2.
(define_predicate "exact_log2_cint_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) > 0 && exact_log2 (INTVAL (op)) >= 0")))

;; Match op = 0 or op = 1.
(define_predicate "const_0_to_1_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 1)")))

;; Match op = 0..3.
(define_predicate "const_0_to_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 3)")))

;; Match op = 2 or op = 3.
(define_predicate "const_2_to_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 2, 3)")))

;; Match op = 0..15
(define_predicate "const_0_to_15_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 15)")))

;; Return 1 if op is a register that is not special.
(define_predicate "gpc_reg_operand"
  (match_operand 0 "register_operand")
{
  if ((TARGET_E500_DOUBLE || TARGET_SPE) && invalid_e500_subreg (op, mode))
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 1;

  if (TARGET_ALTIVEC && ALTIVEC_REGNO_P (REGNO (op)))
    return 1;

  if (TARGET_VSX && VSX_REGNO_P (REGNO (op)))
    return 1;

  return INT_REGNO_P (REGNO (op)) || FP_REGNO_P (REGNO (op));
})

;; Return 1 if op is a general purpose register.  Unlike gpc_reg_operand, don't
;; allow floating point or vector registers.
(define_predicate "int_reg_operand"
  (match_operand 0 "register_operand")
{
  if ((TARGET_E500_DOUBLE || TARGET_SPE) && invalid_e500_subreg (op, mode))
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 1;

  return INT_REGNO_P (REGNO (op));
})

;; Like int_reg_operand, but don't return true for pseudo registers
(define_predicate "int_reg_operand_not_pseudo"
  (match_operand 0 "register_operand")
{
  if ((TARGET_E500_DOUBLE || TARGET_SPE) && invalid_e500_subreg (op, mode))
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
    return 0;

  return INT_REGNO_P (REGNO (op));
})

;; Like int_reg_operand, but only return true for base registers
(define_predicate "base_reg_operand"
  (match_operand 0 "int_reg_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  return (REGNO (op) != FIRST_GPR_REGNO);
})


;; Return true if this is a traditional floating point register
(define_predicate "fpr_reg_operand"
  (match_code "reg,subreg")
{
  HOST_WIDE_INT r;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  r = REGNO (op);
  if (r >= FIRST_PSEUDO_REGISTER)
    return 1;

  return FP_REGNO_P (r);
})

;; Return true if this is a register that can has D-form addressing (GPR and
;; traditional FPR registers for scalars).  ISA 3.0 (power9) adds D-form
;; addressing for scalars in Altivec registers.
;;
;; If this is a pseudo only allow for GPR fusion in power8.  If we have the
;; power9 fusion allow the floating point types.
(define_predicate "toc_fusion_or_p9_reg_operand"
  (match_code "reg,subreg")
{
  HOST_WIDE_INT r;
  bool gpr_p = (mode == QImode || mode == HImode || mode == SImode
		|| mode == SFmode
		|| (TARGET_POWERPC64 && (mode == DImode || mode == DFmode)));
  bool fpr_p = (TARGET_P9_FUSION
		&& (mode == DFmode || mode == SFmode
		    || (TARGET_POWERPC64 && mode == DImode)));
  bool vmx_p = (TARGET_P9_FUSION && TARGET_P9_VECTOR
		&& (mode == DFmode || mode == SFmode));

  if (!TARGET_P8_FUSION)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  r = REGNO (op);
  if (r >= FIRST_PSEUDO_REGISTER)
    return (gpr_p || fpr_p || vmx_p);

  if (INT_REGNO_P (r))
    return gpr_p;

  if (FP_REGNO_P (r))
    return fpr_p;

  if (ALTIVEC_REGNO_P (r))
    return vmx_p;

  return 0;
})

;; Return 1 if op is a HTM specific SPR register.
(define_predicate "htm_spr_reg_operand"
  (match_operand 0 "register_operand")
{
  if (!TARGET_HTM)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  switch (REGNO (op))
    {
      case TFHAR_REGNO:
      case TFIAR_REGNO:
      case TEXASR_REGNO:
	return 1;
      default:
	break;
    }
  
  /* Unknown SPR.  */
  return 0;
})

;; Return 1 if op is a general purpose register that is an even register
;; which suitable for a load/store quad operation
;; Subregs are not allowed here because when they are combine can
;; create (subreg:PTI (reg:TI pseudo)) which will cause reload to
;; think the innermost reg needs reloading, in TImode instead of
;; PTImode.  So reload will choose a reg in TImode which has no
;; requirement that the reg be even.
(define_predicate "quad_int_reg_operand"
  (match_code "reg")
{
  HOST_WIDE_INT r;

  if (!TARGET_QUAD_MEMORY && !TARGET_QUAD_MEMORY_ATOMIC)
    return 0;

  r = REGNO (op);
  if (r >= FIRST_PSEUDO_REGISTER)
    return 1;

  return (INT_REGNO_P (r) && ((r & 1) == 0));
})

;; Return 1 if op is a register that is a condition register field.
(define_predicate "cc_reg_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) > LAST_VIRTUAL_REGISTER)
    return 1;

  return CR_REGNO_P (REGNO (op));
})

;; Return 1 if op is a register that is a condition register field not cr0.
(define_predicate "cc_reg_not_cr0_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) > LAST_VIRTUAL_REGISTER)
    return 1;

  return CR_REGNO_NOT_CR0_P (REGNO (op));
})

;; Return 1 if op is a register that is a condition register field and if generating microcode, not cr0.
(define_predicate "cc_reg_not_micro_cr0_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return 0;

  if (REGNO (op) > LAST_VIRTUAL_REGISTER)
    return 1;

  if (rs6000_gen_cell_microcode)
    return CR_REGNO_NOT_CR0_P (REGNO (op));
  else
    return CR_REGNO_P (REGNO (op));
})

;; Return 1 if op is a constant integer valid for D field
;; or non-special register register.
(define_predicate "reg_or_short_operand"
  (if_then_else (match_code "const_int")
    (match_operand 0 "short_cint_operand")
    (match_operand 0 "gpc_reg_operand")))

;; Return 1 if op is a constant integer valid for DS field
;; or non-special register.
(define_predicate "reg_or_aligned_short_operand"
  (if_then_else (match_code "const_int")
    (and (match_operand 0 "short_cint_operand")
	 (match_test "!(INTVAL (op) & 3)"))
    (match_operand 0 "gpc_reg_operand")))

;; Return 1 if op is a constant integer whose high-order 16 bits are zero
;; or non-special register.
(define_predicate "reg_or_u_short_operand"
  (if_then_else (match_code "const_int")
    (match_operand 0 "u_short_cint_operand")
    (match_operand 0 "gpc_reg_operand")))

;; Return 1 if op is any constant integer 
;; or non-special register.
(define_predicate "reg_or_cint_operand"
  (ior (match_code "const_int")
       (match_operand 0 "gpc_reg_operand")))

;; Return 1 if op is a constant integer valid for addition with addis, addi.
(define_predicate "add_cint_operand"
  (and (match_code "const_int")
       (match_test "((unsigned HOST_WIDE_INT) INTVAL (op)
		       + (mode == SImode ? 0x80000000 : 0x80008000))
		    < (unsigned HOST_WIDE_INT) 0x100000000ll")))

;; Return 1 if op is a constant integer valid for addition
;; or non-special register.
(define_predicate "reg_or_add_cint_operand"
  (if_then_else (match_code "const_int")
    (match_operand 0 "add_cint_operand")
    (match_operand 0 "gpc_reg_operand")))

;; Return 1 if op is a constant integer valid for subtraction
;; or non-special register.
(define_predicate "reg_or_sub_cint_operand"
  (if_then_else (match_code "const_int")
    (match_test "(unsigned HOST_WIDE_INT)
		   (- UINTVAL (op) + (mode == SImode ? 0x80000000 : 0x80008000))
		 < (unsigned HOST_WIDE_INT) 0x100000000ll")
    (match_operand 0 "gpc_reg_operand")))

;; Return 1 if op is any 32-bit unsigned constant integer
;; or non-special register.
(define_predicate "reg_or_logical_cint_operand"
  (if_then_else (match_code "const_int")
    (match_test "(GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT
		  && INTVAL (op) >= 0)
		 || ((INTVAL (op) & GET_MODE_MASK (mode)
		      & (~ (unsigned HOST_WIDE_INT) 0xffffffff)) == 0)")
    (match_operand 0 "gpc_reg_operand")))

;; Like reg_or_logical_cint_operand, but allow vsx registers
(define_predicate "vsx_reg_or_cint_operand"
  (ior (match_operand 0 "vsx_register_operand")
       (match_operand 0 "reg_or_logical_cint_operand")))

;; Return 1 if operand is a CONST_DOUBLE that can be set in a register
;; with no more than one instruction per word.
(define_predicate "easy_fp_constant"
  (match_code "const_double")
{
  if (GET_MODE (op) != mode
      || (!SCALAR_FLOAT_MODE_P (mode) && mode != DImode))
    return 0;

  /* Consider all constants with -msoft-float to be easy.  */
  if ((TARGET_SOFT_FLOAT || TARGET_E500_SINGLE 
      || (TARGET_HARD_FLOAT && (TARGET_SINGLE_FLOAT && ! TARGET_DOUBLE_FLOAT)))
      && mode != DImode)
    return 1;

  /* 0.0D is not all zero bits.  */
  if (DECIMAL_FLOAT_MODE_P (mode))
    return 0;

  /* The constant 0.0 is easy under VSX.  */
  if (TARGET_VSX && SCALAR_FLOAT_MODE_P (mode) && op == CONST0_RTX (mode))
    return 1;

  /* If we are using V.4 style PIC, consider all constants to be hard.  */
  if (flag_pic && DEFAULT_ABI == ABI_V4)
    return 0;

  /* If we have real FPRs, consider floating point constants hard (other than
     0.0 under VSX), so that the constant gets pushed to memory during the
     early RTL phases.  This has the advantage that double precision constants
     that can be represented in single precision without a loss of precision
     will use single precision loads.  */

  switch (mode)
    {
    case KFmode:
    case IFmode:
    case TFmode:
    case DFmode:
    case SFmode:
      return 0;

    case DImode:
      return (num_insns_constant (op, DImode) <= 2);

    case SImode:
      return 1;

    default:
      gcc_unreachable ();
    }
})

;; Return 1 if the operand is a CONST_VECTOR and can be loaded into a
;; vector register without using memory.
(define_predicate "easy_vector_constant"
  (match_code "const_vector")
{
  /* As the paired vectors are actually FPRs it seems that there is
     no easy way to load a CONST_VECTOR without using memory.  */
  if (TARGET_PAIRED_FLOAT)
    return false;

  /* Because IEEE 128-bit floating point is considered a vector type
     in order to pass it in VSX registers, it might use this function
     instead of easy_fp_constant.  */
  if (FLOAT128_VECTOR_P (mode))
    return easy_fp_constant (op, mode);

  if (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode))
    {
      if (zero_constant (op, mode))
	return true;

      return easy_altivec_constant (op, mode);
    }

  if (SPE_VECTOR_MODE (mode))
    {
      int cst, cst2;
      if (zero_constant (op, mode))
	return true;
      if (GET_MODE_CLASS (mode) != MODE_VECTOR_INT)
        return false;

      /* Limit SPE vectors to 15 bits signed.  These we can generate with:
	   li r0, CONSTANT1
	   evmergelo r0, r0, r0
	   li r0, CONSTANT2

	 I don't know how efficient it would be to allow bigger constants,
	 considering we'll have an extra 'ori' for every 'li'.  I doubt 5
	 instructions is better than a 64-bit memory load, but I don't
	 have the e500 timing specs.  */
      if (mode == V2SImode)
	{
	  cst  = INTVAL (CONST_VECTOR_ELT (op, 0));
	  cst2 = INTVAL (CONST_VECTOR_ELT (op, 1));
	  return cst  >= -0x7fff && cst <= 0x7fff
	         && cst2 >= -0x7fff && cst2 <= 0x7fff;
	}
    }

  return false;
})

;; Same as easy_vector_constant but only for EASY_VECTOR_15_ADD_SELF.
(define_predicate "easy_vector_constant_add_self"
  (and (match_code "const_vector")
       (and (match_test "TARGET_ALTIVEC")
	    (match_test "easy_altivec_constant (op, mode)")))
{
  HOST_WIDE_INT val;
  int elt;
  if (mode == V2DImode || mode == V2DFmode)
    return 0;
  elt = BYTES_BIG_ENDIAN ? GET_MODE_NUNITS (mode) - 1 : 0;
  val = const_vector_elt_as_int (op, elt);
  val = ((val & 0xff) ^ 0x80) - 0x80;
  return EASY_VECTOR_15_ADD_SELF (val);
})

;; Same as easy_vector_constant but only for EASY_VECTOR_MSB.
(define_predicate "easy_vector_constant_msb"
  (and (match_code "const_vector")
       (and (match_test "TARGET_ALTIVEC")
	    (match_test "easy_altivec_constant (op, mode)")))
{
  HOST_WIDE_INT val;
  int elt;
  if (mode == V2DImode || mode == V2DFmode)
    return 0;
  elt = BYTES_BIG_ENDIAN ? GET_MODE_NUNITS (mode) - 1 : 0;
  val = const_vector_elt_as_int (op, elt);
  return EASY_VECTOR_MSB (val, GET_MODE_INNER (mode));
})

;; Return true if this is an easy altivec constant that we form
;; by using VSLDOI.
(define_predicate "easy_vector_constant_vsldoi"
  (and (match_code "const_vector")
       (and (match_test "TARGET_ALTIVEC")
	    (and (match_test "easy_altivec_constant (op, mode)")
		 (match_test "vspltis_shifted (op) != 0")))))

;; Return 1 if operand is constant zero (scalars and vectors).
(define_predicate "zero_constant"
  (and (match_code "const_int,const_double,const_wide_int,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; Return 1 if operand is 0.0.
(define_predicate "zero_fp_constant"
  (and (match_code "const_double")
       (match_test "SCALAR_FLOAT_MODE_P (mode)
		    && op == CONST0_RTX (mode)")))

;; Return 1 if the operand is in volatile memory.  Note that during the
;; RTL generation phase, memory_operand does not return TRUE for volatile
;; memory references.  So this function allows us to recognize volatile
;; references where it's safe.
(define_predicate "volatile_mem_operand"
  (and (and (match_code "mem")
	    (match_test "MEM_VOLATILE_P (op)"))
       (if_then_else (match_test "reload_completed")
         (match_operand 0 "memory_operand")
         (if_then_else (match_test "reload_in_progress")
	   (match_test "strict_memory_address_p (mode, XEXP (op, 0))")
	   (match_test "memory_address_p (mode, XEXP (op, 0))")))))

;; Return 1 if the operand is an offsettable memory operand.
(define_predicate "offsettable_mem_operand"
  (and (match_operand 0 "memory_operand")
       (match_test "offsettable_nonstrict_memref_p (op)")))

;; Return 1 if the operand is suitable for load/store quad memory.
;; This predicate only checks for non-atomic loads/stores (not lqarx/stqcx).
(define_predicate "quad_memory_operand"
  (match_code "mem")
{
  if (!TARGET_QUAD_MEMORY && !TARGET_SYNC_TI)
    return false;

  if (GET_MODE_SIZE (mode) != 16 || !MEM_P (op) || MEM_ALIGN (op) < 128)
    return false;

  return quad_address_p (XEXP (op, 0), mode, true);
})

;; Return 1 if the operand is suitable for load/store to vector registers with
;; d-form addressing (register+offset), which was added in ISA 3.0.
;; Unlike quad_memory_operand, we do not have to check for alignment.
(define_predicate "vsx_quad_dform_memory_operand"
  (match_code "mem")
{
  if (!TARGET_P9_DFORM_VECTOR || !MEM_P (op) || GET_MODE_SIZE (mode) != 16)
    return false;

  return quad_address_p (XEXP (op, 0), mode, false);
})

;; Return 1 if the operand is an indexed or indirect memory operand.
(define_predicate "indexed_or_indirect_operand"
  (match_code "mem")
{
  op = XEXP (op, 0);
  if (VECTOR_MEM_ALTIVEC_P (mode)
      && GET_CODE (op) == AND
      && GET_CODE (XEXP (op, 1)) == CONST_INT
      && INTVAL (XEXP (op, 1)) == -16)
    op = XEXP (op, 0);

  return indexed_or_indirect_address (op, mode);
})

;; Like indexed_or_indirect_operand, but also allow a GPR register if direct
;; moves are supported.
(define_predicate "reg_or_indexed_operand"
  (match_code "mem,reg,subreg")
{
  if (MEM_P (op))
    return indexed_or_indirect_operand (op, mode);
  else if (TARGET_DIRECT_MOVE)
    return register_operand (op, mode);
  return
    0;
})

;; Return 1 if the operand is an indexed or indirect memory operand with an
;; AND -16 in it, used to recognize when we need to switch to Altivec loads
;; to realign loops instead of VSX (altivec silently ignores the bottom bits,
;; while VSX uses the full address and traps)
(define_predicate "altivec_indexed_or_indirect_operand"
  (match_code "mem")
{
  op = XEXP (op, 0);
  if (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode)
      && GET_CODE (op) == AND
      && GET_CODE (XEXP (op, 1)) == CONST_INT
      && INTVAL (XEXP (op, 1)) == -16)
    return indexed_or_indirect_address (XEXP (op, 0), mode);

  return 0;
})

;; Return 1 if the operand is an indexed or indirect address.
(define_special_predicate "indexed_or_indirect_address"
  (and (match_test "REG_P (op)
		    || (GET_CODE (op) == PLUS
			/* Omit testing REG_P (XEXP (op, 0)).  */
			&& REG_P (XEXP (op, 1)))")
       (match_operand 0 "address_operand")))

;; Return 1 if the operand is an index-form address.
(define_special_predicate "indexed_address"
  (match_test "(GET_CODE (op) == PLUS
		&& REG_P (XEXP (op, 0))
		&& REG_P (XEXP (op, 1)))"))

;; Return 1 if the operand is a MEM with an update-form address. This may
;; also include update-indexed form.
(define_special_predicate "update_address_mem"
  (match_test "(MEM_P (op)
		&& (GET_CODE (XEXP (op, 0)) == PRE_INC
		    || GET_CODE (XEXP (op, 0)) == PRE_DEC
		    || GET_CODE (XEXP (op, 0)) == PRE_MODIFY))"))

;; Return 1 if the operand is a MEM with an indexed-form address.
(define_special_predicate "indexed_address_mem"
  (match_test "(MEM_P (op)
		&& (indexed_address (XEXP (op, 0), mode)
		    || (GET_CODE (XEXP (op, 0)) == PRE_MODIFY
			&& indexed_address (XEXP (XEXP (op, 0), 1), mode))))"))

;; Return 1 if the operand is either a non-special register or can be used
;; as the operand of a `mode' add insn.
(define_predicate "add_operand"
  (if_then_else (match_code "const_int")
    (match_test "satisfies_constraint_I (op)
		 || satisfies_constraint_L (op)")
    (match_operand 0 "gpc_reg_operand")))

;; Return 1 if the operand is either a non-special register, or 0, or -1.
(define_predicate "adde_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) == 0 || INTVAL (op) == -1")
    (match_operand 0 "gpc_reg_operand")))

;; Return 1 if OP is a constant but not a valid add_operand.
(define_predicate "non_add_cint_operand"
  (and (match_code "const_int")
       (match_test "!satisfies_constraint_I (op)
		    && !satisfies_constraint_L (op)")))

;; Return 1 if the operand is a constant that can be used as the operand
;; of an OR or XOR.
(define_predicate "logical_const_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT opl;

  opl = INTVAL (op) & GET_MODE_MASK (mode);

  return ((opl & ~ (unsigned HOST_WIDE_INT) 0xffff) == 0
	  || (opl & ~ (unsigned HOST_WIDE_INT) 0xffff0000) == 0);
})

;; Return 1 if the operand is a non-special register or a constant that
;; can be used as the operand of an OR or XOR.
(define_predicate "logical_operand"
  (ior (match_operand 0 "gpc_reg_operand")
       (match_operand 0 "logical_const_operand")))

;; Return 1 if op is a constant that is not a logical operand, but could
;; be split into one.
(define_predicate "non_logical_cint_operand"
  (and (match_code "const_int,const_wide_int")
       (and (not (match_operand 0 "logical_operand"))
	    (match_operand 0 "reg_or_logical_cint_operand"))))

;; Return 1 if the operand is either a non-special register or a
;; constant that can be used as the operand of a logical AND.
(define_predicate "and_operand"
  (ior (and (match_code "const_int")
	    (match_test "rs6000_is_valid_and_mask (op, mode)"))
       (if_then_else (match_test "fixed_regs[CR0_REGNO]")
	 (match_operand 0 "gpc_reg_operand")
	 (match_operand 0 "logical_operand"))))

;; Return 1 if the operand is either a logical operand or a short cint operand.
(define_predicate "scc_eq_operand"
  (ior (match_operand 0 "logical_operand")
       (match_operand 0 "short_cint_operand")))

;; Return 1 if the operand is a general non-special register or memory operand.
(define_predicate "reg_or_mem_operand"
     (ior (match_operand 0 "memory_operand")
	  (ior (and (match_code "mem")
		    (match_test "macho_lo_sum_memory_operand (op, mode)"))
	       (ior (match_operand 0 "volatile_mem_operand")
		    (match_operand 0 "gpc_reg_operand")))))

;; Return 1 if the operand is either an easy FP constant or memory or reg.
(define_predicate "reg_or_none500mem_operand"
  (if_then_else (match_code "mem")
     (and (match_test "!TARGET_E500_DOUBLE")
	  (ior (match_operand 0 "memory_operand")
	       (ior (match_test "macho_lo_sum_memory_operand (op, mode)")
		    (match_operand 0 "volatile_mem_operand"))))
     (match_operand 0 "gpc_reg_operand")))

;; Return 1 if the operand is CONST_DOUBLE 0, register or memory operand.
(define_predicate "zero_reg_mem_operand"
  (ior (and (match_test "TARGET_VSX")
	    (match_operand 0 "zero_fp_constant"))
       (match_operand 0 "reg_or_mem_operand")))

;; Return 1 if the operand is a CONST_INT and it is the element for 64-bit
;; data types inside of a vector that scalar instructions operate on
(define_predicate "vsx_scalar_64bit"
  (match_code "const_int")
{
  return (INTVAL (op) == VECTOR_ELEMENT_SCALAR_64BIT);
})

;; Return 1 if the operand is a general register or memory operand without
;; pre_inc or pre_dec or pre_modify, which produces invalid form of PowerPC
;; lwa instruction.
(define_predicate "lwa_operand"
  (match_code "reg,subreg,mem")
{
  rtx inner, addr, offset;

  inner = op;
  if (reload_completed && GET_CODE (inner) == SUBREG)
    inner = SUBREG_REG (inner);

  if (gpc_reg_operand (inner, mode))
    return true;
  if (!memory_operand (inner, mode))
    return false;
  if (!rs6000_gen_cell_microcode)
    return false;

  addr = XEXP (inner, 0);
  if (GET_CODE (addr) == PRE_INC
      || GET_CODE (addr) == PRE_DEC
      || (GET_CODE (addr) == PRE_MODIFY
	  && !legitimate_indexed_address_p (XEXP (addr, 1), 0)))
    return false;
  if (GET_CODE (addr) == LO_SUM
      && GET_CODE (XEXP (addr, 0)) == REG
      && GET_CODE (XEXP (addr, 1)) == CONST)
    addr = XEXP (XEXP (addr, 1), 0);
  if (GET_CODE (addr) != PLUS)
    return true;
  offset = XEXP (addr, 1);
  if (GET_CODE (offset) != CONST_INT)
    return true;
  return INTVAL (offset) % 4 == 0;
})

;; Return 1 if the operand, used inside a MEM, is a SYMBOL_REF.
(define_predicate "symbol_ref_operand"
  (and (match_code "symbol_ref")
       (match_test "(mode == VOIDmode || GET_MODE (op) == mode)
		    && (DEFAULT_ABI != ABI_AIX || SYMBOL_REF_FUNCTION_P (op))")))

;; Return 1 if op is an operand that can be loaded via the GOT.
;; or non-special register register field no cr0
(define_predicate "got_operand"
  (match_code "symbol_ref,const,label_ref"))

;; Return 1 if op is a simple reference that can be loaded via the GOT,
;; excluding labels involving addition.
(define_predicate "got_no_const_operand"
  (match_code "symbol_ref,label_ref"))

;; Return 1 if op is a SYMBOL_REF for a TLS symbol.
(define_predicate "rs6000_tls_symbol_ref"
  (and (match_code "symbol_ref")
       (match_test "RS6000_SYMBOL_REF_TLS_P (op)")))

;; Return 1 if the operand, used inside a MEM, is a valid first argument
;; to CALL.  This is a SYMBOL_REF, a pseudo-register, LR or CTR.
(define_predicate "call_operand"
  (if_then_else (match_code "reg")
     (match_test "REGNO (op) == LR_REGNO
		  || REGNO (op) == CTR_REGNO
		  || REGNO (op) >= FIRST_PSEUDO_REGISTER")
     (match_code "symbol_ref")))

;; Return 1 if the operand is a SYMBOL_REF for a function known to be in
;; this file.
(define_predicate "current_file_function_operand"
  (and (match_code "symbol_ref")
       (match_test "(DEFAULT_ABI != ABI_AIX || SYMBOL_REF_FUNCTION_P (op))
		    && (SYMBOL_REF_LOCAL_P (op)
			|| op == XEXP (DECL_RTL (current_function_decl), 0))
		    && !((DEFAULT_ABI == ABI_AIX
			  || DEFAULT_ABI == ABI_ELFv2)
			 && (SYMBOL_REF_EXTERNAL_P (op)
			     || SYMBOL_REF_WEAK (op)))")))

;; Return 1 if this operand is a valid input for a move insn.
(define_predicate "input_operand"
  (match_code "symbol_ref,const,reg,subreg,mem,
	       const_double,const_wide_int,const_vector,const_int")
{
  /* Memory is always valid.  */
  if (memory_operand (op, mode))
    return 1;

  /* For floating-point, easy constants are valid.  */
  if (SCALAR_FLOAT_MODE_P (mode)
      && easy_fp_constant (op, mode))
    return 1;

  /* Allow any integer constant.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && CONST_SCALAR_INT_P (op))
    return 1;

  /* Allow easy vector constants.  */
  if (GET_CODE (op) == CONST_VECTOR
      && easy_vector_constant (op, mode))
    return 1;

  /* Do not allow invalid E500 subregs.  */
  if ((TARGET_E500_DOUBLE || TARGET_SPE)
      && GET_CODE (op) == SUBREG
      && invalid_e500_subreg (op, mode))
    return 0;

  /* For floating-point or multi-word mode, the only remaining valid type
     is a register.  */
  if (SCALAR_FLOAT_MODE_P (mode)
      || GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return register_operand (op, mode);

  /* We don't allow moving the carry bit around.  */
  if (ca_operand (op, mode))
    return 0;

  /* The only cases left are integral modes one word or smaller (we
     do not get called for MODE_CC values).  These can be in any
     register.  */
  if (register_operand (op, mode))
    return 1;

  /* V.4 allows SYMBOL_REFs and CONSTs that are in the small data region
     to be valid.  */
  if (DEFAULT_ABI == ABI_V4
      && (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST)
      && small_data_operand (op, Pmode))
    return 1;

  return 0;
})

;; Return 1 if this operand is a valid input for a vsx_splat insn.
(define_predicate "splat_input_operand"
  (match_code "symbol_ref,const,reg,subreg,mem,
	       const_double,const_wide_int,const_vector,const_int")
{
  if (MEM_P (op))
    {
      if (! volatile_ok && MEM_VOLATILE_P (op))
	return 0;
      if (mode == DFmode)
	mode = V2DFmode;
      else if (mode == DImode)
	mode = V2DImode;
      else
	gcc_unreachable ();
      return memory_address_addr_space_p (mode, XEXP (op, 0),
					  MEM_ADDR_SPACE (op));
    }
  return input_operand (op, mode);
})

;; Return true if OP is a non-immediate operand and not an invalid
;; SUBREG operation on the e500.
(define_predicate "rs6000_nonimmediate_operand"
  (match_code "reg,subreg,mem")
{
  if ((TARGET_E500_DOUBLE || TARGET_SPE)
      && GET_CODE (op) == SUBREG
      && invalid_e500_subreg (op, mode))
    return 0;

  return nonimmediate_operand (op, mode);
})

;; Return true if operand is an operator used in rotate-and-mask instructions.
(define_predicate "rotate_mask_operator"
  (match_code "rotate,ashift,lshiftrt"))

;; Return true if operand is boolean operator.
(define_predicate "boolean_operator"
  (match_code "and,ior,xor"))

;; Return true if operand is OR-form of boolean operator.
(define_predicate "boolean_or_operator"
  (match_code "ior,xor"))

;; Return true if operand is an equality operator.
(define_special_predicate "equality_operator"
  (match_code "eq,ne"))

;; Return true if operand is MIN or MAX operator.
(define_predicate "min_max_operator"
  (match_code "smin,smax,umin,umax"))

;; Return 1 if OP is a comparison operation that is valid for a branch
;; instruction.  We check the opcode against the mode of the CC value.
;; validate_condition_mode is an assertion.
(define_predicate "branch_comparison_operator"
   (and (match_operand 0 "comparison_operator")
	(and (match_test "GET_MODE_CLASS (GET_MODE (XEXP (op, 0))) == MODE_CC")
	     (match_test "validate_condition_mode (GET_CODE (op),
						   GET_MODE (XEXP (op, 0))),
			  1"))))

;; Return 1 if OP is a valid comparison operator for "cbranch" instructions.
;; If we're assuming that FP operations cannot generate user-visible traps,
;; then on e500 we can use the ordered-signaling instructions to implement
;; the unordered-quiet FP comparison predicates modulo a reversal.
(define_predicate "rs6000_cbranch_operator"
  (if_then_else (match_test "TARGET_HARD_FLOAT && !TARGET_FPRS")
		(if_then_else (match_test "flag_trapping_math")
			      (match_operand 0 "ordered_comparison_operator")
			      (ior (match_operand 0 "ordered_comparison_operator")
				   (match_code ("unlt,unle,ungt,unge"))))
		(match_operand 0 "comparison_operator")))

;; Return 1 if OP is an unsigned comparison operator.
(define_predicate "unsigned_comparison_operator"
  (match_code "ltu,gtu,leu,geu"))

;; Return 1 if OP is a signed comparison operator.
(define_predicate "signed_comparison_operator"
  (match_code "lt,gt,le,ge"))

;; Return 1 if OP is a comparison operation that is valid for an SCC insn --
;; it must be a positive comparison.
(define_predicate "scc_comparison_operator"
  (and (match_operand 0 "branch_comparison_operator")
       (match_code "eq,lt,gt,ltu,gtu,unordered")))

;; Return 1 if OP is a comparison operation whose inverse would be valid for
;; an SCC insn.
(define_predicate "scc_rev_comparison_operator"
  (and (match_operand 0 "branch_comparison_operator")
       (match_code "ne,le,ge,leu,geu,ordered")))

;; Return 1 if OP is a comparison operation that is valid for a branch
;; insn, which is true if the corresponding bit in the CC register is set.
(define_predicate "branch_positive_comparison_operator"
  (and (match_operand 0 "branch_comparison_operator")
       (match_code "eq,lt,gt,ltu,gtu,unordered")))

;; Return 1 if OP is a load multiple operation, known to be a PARALLEL.
(define_predicate "load_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
})

;; Return 1 if OP is a store multiple operation, known to be a PARALLEL.
;; The second vector element is a CLOBBER.
(define_predicate "store_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0) - 1;
  unsigned int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + 1);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
})

;; Return 1 if OP is valid for a save_world call in prologue, known to be
;; a PARLLEL.
(define_predicate "save_world_operation"
  (match_code "parallel")
{
  int index;
  int i;
  rtx elt;
  int count = XVECLEN (op, 0);

  if (count != 54)
    return 0;

  index = 0;
  if (GET_CODE (XVECEXP (op, 0, index++)) != CLOBBER
      || GET_CODE (XVECEXP (op, 0, index++)) != USE)
    return 0;

  for (i=1; i <= 18; i++)
    {
      elt = XVECEXP (op, 0, index++);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || ! memory_operand (SET_DEST (elt), DFmode)
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != DFmode)
	return 0;
    }

  for (i=1; i <= 12; i++)
    {
      elt = XVECEXP (op, 0, index++);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != V4SImode)
	return 0;
    }

  for (i=1; i <= 19; i++)
    {
      elt = XVECEXP (op, 0, index++);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || ! memory_operand (SET_DEST (elt), Pmode)
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != Pmode)
	return 0;
    }

  elt = XVECEXP (op, 0, index++);
  if (GET_CODE (elt) != SET
      || GET_CODE (SET_DEST (elt)) != MEM
      || ! memory_operand (SET_DEST (elt), Pmode)
      || GET_CODE (SET_SRC (elt)) != REG
      || REGNO (SET_SRC (elt)) != CR2_REGNO
      || GET_MODE (SET_SRC (elt)) != Pmode)
    return 0;

  if (GET_CODE (XVECEXP (op, 0, index++)) != SET
      || GET_CODE (XVECEXP (op, 0, index++)) != SET)
    return 0;
  return 1;
})

;; Return 1 if OP is valid for a restore_world call in epilogue, known to be
;; a PARLLEL.
(define_predicate "restore_world_operation"
  (match_code "parallel")
{
  int index;
  int i;
  rtx elt;
  int count = XVECLEN (op, 0);

  if (count != 59)
    return 0;

  index = 0;
  if (GET_CODE (XVECEXP (op, 0, index++)) != RETURN
      || GET_CODE (XVECEXP (op, 0, index++)) != USE
      || GET_CODE (XVECEXP (op, 0, index++)) != USE
      || GET_CODE (XVECEXP (op, 0, index++)) != CLOBBER)
    return 0;

  elt = XVECEXP (op, 0, index++);
  if (GET_CODE (elt) != SET
      || GET_CODE (SET_SRC (elt)) != MEM
      || ! memory_operand (SET_SRC (elt), Pmode)
      || GET_CODE (SET_DEST (elt)) != REG
      || REGNO (SET_DEST (elt)) != CR2_REGNO
      || GET_MODE (SET_DEST (elt)) != Pmode)
    return 0;

  for (i=1; i <= 19; i++)
    {
      elt = XVECEXP (op, 0, index++);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || ! memory_operand (SET_SRC (elt), Pmode)
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != Pmode)
	return 0;
    }

  for (i=1; i <= 12; i++)
    {
      elt = XVECEXP (op, 0, index++);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != V4SImode)
	return 0;
    }

  for (i=1; i <= 18; i++)
    {
      elt = XVECEXP (op, 0, index++);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || ! memory_operand (SET_SRC (elt), DFmode)
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != DFmode)
	return 0;
    }

  if (GET_CODE (XVECEXP (op, 0, index++)) != CLOBBER
      || GET_CODE (XVECEXP (op, 0, index++)) != CLOBBER
      || GET_CODE (XVECEXP (op, 0, index++)) != CLOBBER
      || GET_CODE (XVECEXP (op, 0, index++)) != CLOBBER
      || GET_CODE (XVECEXP (op, 0, index++)) != USE)
    return 0;
  return 1;
})

;; Return 1 if OP is valid for a vrsave call, known to be a PARALLEL.
(define_predicate "vrsave_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno, src_regno;
  int i;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC_VOLATILE
      || XINT (SET_SRC (XVECEXP (op, 0, 0)), 1) != UNSPECV_SET_VRSAVE)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_regno  = REGNO (XVECEXP (SET_SRC (XVECEXP (op, 0, 0)), 0, 1));

  if (dest_regno != VRSAVE_REGNO || src_regno != VRSAVE_REGNO)
    return 0;

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != CLOBBER
	  && GET_CODE (elt) != SET)
	return 0;
    }

  return 1;
})

;; Return 1 if OP is valid for mfcr insn, known to be a PARALLEL.
(define_predicate "mfcr_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count < 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC
      || XVECLEN (SET_SRC (XVECEXP (op, 0, 0)), 0) != 2)
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx exp = XVECEXP (op, 0, i);
      rtx unspec;
      int maskval;
      rtx src_reg;

      src_reg = XVECEXP (SET_SRC (exp), 0, 0);

      if (GET_CODE (src_reg) != REG
	  || GET_MODE (src_reg) != CCmode
	  || ! CR_REGNO_P (REGNO (src_reg)))
	return 0;

      if (GET_CODE (exp) != SET
	  || GET_CODE (SET_DEST (exp)) != REG
	  || GET_MODE (SET_DEST (exp)) != SImode
	  || ! INT_REGNO_P (REGNO (SET_DEST (exp))))
	return 0;
      unspec = SET_SRC (exp);
      maskval = 1 << (MAX_CR_REGNO - REGNO (src_reg));

      if (GET_CODE (unspec) != UNSPEC
	  || XINT (unspec, 1) != UNSPEC_MOVESI_FROM_CR
	  || XVECLEN (unspec, 0) != 2
	  || XVECEXP (unspec, 0, 0) != src_reg
	  || GET_CODE (XVECEXP (unspec, 0, 1)) != CONST_INT
	  || INTVAL (XVECEXP (unspec, 0, 1)) != maskval)
	return 0;
    }
  return 1;
})

;; Return 1 if OP is valid for mtcrf insn, known to be a PARALLEL.
(define_predicate "mtcrf_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int i;
  rtx src_reg;

  /* Perform a quick check so we don't blow up below.  */
  if (count < 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC
      || XVECLEN (SET_SRC (XVECEXP (op, 0, 0)), 0) != 2)
    return 0;
  src_reg = XVECEXP (SET_SRC (XVECEXP (op, 0, 0)), 0, 0);

  if (GET_CODE (src_reg) != REG
      || GET_MODE (src_reg) != SImode
      || ! INT_REGNO_P (REGNO (src_reg)))
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx exp = XVECEXP (op, 0, i);
      rtx unspec;
      int maskval;

      if (GET_CODE (exp) != SET
	  || GET_CODE (SET_DEST (exp)) != REG
	  || GET_MODE (SET_DEST (exp)) != CCmode
	  || ! CR_REGNO_P (REGNO (SET_DEST (exp))))
	return 0;
      unspec = SET_SRC (exp);
      maskval = 1 << (MAX_CR_REGNO - REGNO (SET_DEST (exp)));

      if (GET_CODE (unspec) != UNSPEC
	  || XINT (unspec, 1) != UNSPEC_MOVESI_TO_CR
	  || XVECLEN (unspec, 0) != 2
	  || XVECEXP (unspec, 0, 0) != src_reg
	  || GET_CODE (XVECEXP (unspec, 0, 1)) != CONST_INT
	  || INTVAL (XVECEXP (unspec, 0, 1)) != maskval)
	return 0;
    }
  return 1;
})

;; Return 1 if OP is valid for crsave insn, known to be a PARALLEL.
(define_predicate "crsave_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int i;

  for (i = 1; i < count; i++)
    {
      rtx exp = XVECEXP (op, 0, i);

      if (GET_CODE (exp) != USE
	  || GET_CODE (XEXP (exp, 0)) != REG
	  || GET_MODE (XEXP (exp, 0)) != CCmode
	  || ! CR_REGNO_P (REGNO (XEXP (exp, 0))))
	return 0;
    }
  return 1;
})

;; Return 1 if OP is valid for lmw insn, known to be a PARALLEL.
(define_predicate "lmw_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr;
  unsigned int base_regno;
  HOST_WIDE_INT offset;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  if (dest_regno > 31
      || count != 32 - (int) dest_regno)
    return 0;

  if (legitimate_indirect_address_p (src_addr, 0))
    {
      offset = 0;
      base_regno = REGNO (src_addr);
      if (base_regno == 0)
	return 0;
    }
  else if (rs6000_legitimate_offset_address_p (SImode, src_addr, false, false))
    {
      offset = INTVAL (XEXP (src_addr, 1));
      base_regno = REGNO (XEXP (src_addr, 0));
    }
  else
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      rtx newaddr;
      rtx addr_reg;
      HOST_WIDE_INT newoffset;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode)
	return 0;
      newaddr = XEXP (SET_SRC (elt), 0);
      if (legitimate_indirect_address_p (newaddr, 0))
	{
	  newoffset = 0;
	  addr_reg = newaddr;
	}
      else if (rs6000_legitimate_offset_address_p (SImode, newaddr, false, false))
	{
	  addr_reg = XEXP (newaddr, 0);
	  newoffset = INTVAL (XEXP (newaddr, 1));
	}
      else
	return 0;
      if (REGNO (addr_reg) != base_regno
	  || newoffset != offset + 4 * i)
	return 0;
    }

  return 1;
})

;; Return 1 if OP is valid for stmw insn, known to be a PARALLEL.
(define_predicate "stmw_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  unsigned int src_regno;
  rtx dest_addr;
  unsigned int base_regno;
  HOST_WIDE_INT offset;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  if (src_regno > 31
      || count != 32 - (int) src_regno)
    return 0;

  if (legitimate_indirect_address_p (dest_addr, 0))
    {
      offset = 0;
      base_regno = REGNO (dest_addr);
      if (base_regno == 0)
	return 0;
    }
  else if (rs6000_legitimate_offset_address_p (SImode, dest_addr, false, false))
    {
      offset = INTVAL (XEXP (dest_addr, 1));
      base_regno = REGNO (XEXP (dest_addr, 0));
    }
  else
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      rtx newaddr;
      rtx addr_reg;
      HOST_WIDE_INT newoffset;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode)
	return 0;
      newaddr = XEXP (SET_DEST (elt), 0);
      if (legitimate_indirect_address_p (newaddr, 0))
	{
	  newoffset = 0;
	  addr_reg = newaddr;
	}
      else if (rs6000_legitimate_offset_address_p (SImode, newaddr, false, false))
	{
	  addr_reg = XEXP (newaddr, 0);
	  newoffset = INTVAL (XEXP (newaddr, 1));
	}
      else
	return 0;
      if (REGNO (addr_reg) != base_regno
	  || newoffset != offset + 4 * i)
	return 0;
    }

  return 1;
})

;; Return 1 if OP is a stack tie operand.
(define_predicate "tie_operand"
  (match_code "parallel")
{
  return (GET_CODE (XVECEXP (op, 0, 0)) == SET
	  && GET_CODE (XEXP (XVECEXP (op, 0, 0), 0)) == MEM
	  && GET_MODE (XEXP (XVECEXP (op, 0, 0), 0)) == BLKmode
	  && XEXP (XVECEXP (op, 0, 0), 1) == const0_rtx);
})

;; Match a small code model toc reference (or medium and large
;; model toc references before reload).
(define_predicate "small_toc_ref"
  (match_code "unspec,plus")
{
  if (GET_CODE (op) == PLUS && add_cint_operand (XEXP (op, 1), mode))
    op = XEXP (op, 0);

  return GET_CODE (op) == UNSPEC && XINT (op, 1) == UNSPEC_TOCREL;
})

;; Match the TOC memory operand that can be fused with an addis instruction.
;; This is used in matching a potential fused address before register
;; allocation.
(define_predicate "toc_fusion_mem_raw"
  (match_code "mem")
{
  if (!TARGET_TOC_FUSION_INT || !can_create_pseudo_p ())
    return false;

  return small_toc_ref (XEXP (op, 0), Pmode);
})

;; Match the memory operand that has been fused with an addis instruction and
;; wrapped inside of an (unspec [...] UNSPEC_FUSION_ADDIS) wrapper.
(define_predicate "toc_fusion_mem_wrapped"
  (match_code "mem")
{
  rtx addr;

  if (!TARGET_TOC_FUSION_INT)
    return false;

  if (!MEM_P (op))
    return false;

  addr = XEXP (op, 0);
  return (GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_FUSION_ADDIS);
})

;; Match the first insn (addis) in fusing the combination of addis and loads to
;; GPR registers on power8.
(define_predicate "fusion_gpr_addis"
  (match_code "const_int,high,plus")
{
  HOST_WIDE_INT value;
  rtx int_const;

  if (GET_CODE (op) == HIGH)
    return 1;

  if (CONST_INT_P (op))
    int_const = op;

  else if (GET_CODE (op) == PLUS
	   && base_reg_operand (XEXP (op, 0), Pmode)
	   && CONST_INT_P (XEXP (op, 1)))
    int_const = XEXP (op, 1);

  else
    return 0;

  value = INTVAL (int_const);
  if ((value & (HOST_WIDE_INT)0xffff) != 0)
    return 0;

  if ((value & (HOST_WIDE_INT)0xffff0000) == 0)
    return 0;

  /* Power8 currently will only do the fusion if the top 11 bits of the addis
     value are all 1's or 0's.  Ignore this restriction if we are testing
     advanced fusion.  */
  if (TARGET_P9_FUSION)
    return 1;

  return (IN_RANGE (value >> 16, -32, 31));
})

;; Match the second insn (lbz, lhz, lwz, ld) in fusing the combination of addis
;; and loads to GPR registers on power8.
(define_predicate "fusion_gpr_mem_load"
  (match_code "mem,sign_extend,zero_extend")
{
  rtx addr, base, offset;

  /* Handle sign/zero extend.  */
  if (GET_CODE (op) == ZERO_EXTEND
      || (TARGET_P8_FUSION_SIGN && GET_CODE (op) == SIGN_EXTEND))
    {
      op = XEXP (op, 0);
      mode = GET_MODE (op);
    }

  if (!MEM_P (op))
    return 0;

  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
      break;

    case DImode:
      if (!TARGET_POWERPC64)
	return 0;
      break;

    default:
      return 0;
    }

  addr = XEXP (op, 0);
  if (GET_CODE (addr) != PLUS && GET_CODE (addr) != LO_SUM)
    return 0;

  base = XEXP (addr, 0);
  if (!base_reg_operand (base, GET_MODE (base)))
    return 0;

  offset = XEXP (addr, 1);

  if (GET_CODE (addr) == PLUS)
    return satisfies_constraint_I (offset);

  else if (GET_CODE (addr) == LO_SUM)
    {
      if (TARGET_XCOFF || (TARGET_ELF && TARGET_POWERPC64))
	return small_toc_ref (offset, GET_MODE (offset));

      else if (TARGET_ELF && !TARGET_POWERPC64)
	return CONSTANT_P (offset);
    }

  return 0;
})

;; Match a GPR load (lbz, lhz, lwz, ld) that uses a combined address in the
;; memory field with both the addis and the memory offset.  Sign extension
;; is not handled here, since lha and lwa are not fused.
;; With extended fusion, also match a FPR load (lfd, lfs) and float_extend
(define_predicate "fusion_addis_mem_combo_load"
  (match_code "mem,zero_extend,float_extend")
{
  rtx addr, base, offset;

  /* Handle zero/float extend.  */
  if (GET_CODE (op) == ZERO_EXTEND || GET_CODE (op) == FLOAT_EXTEND)
    {
      op = XEXP (op, 0);
      mode = GET_MODE (op);
    }

  if (!MEM_P (op))
    return 0;

  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
      break;

    case DImode:
      if (!TARGET_POWERPC64)
	return 0;
      break;

    case SFmode:
    case DFmode:
      if (!TARGET_P9_FUSION)
	return 0;
      break;

    default:
      return 0;
    }

  addr = XEXP (op, 0);
  if (GET_CODE (addr) != PLUS && GET_CODE (addr) != LO_SUM)
    return 0;

  base = XEXP (addr, 0);
  if (!fusion_gpr_addis (base, GET_MODE (base)))
    return 0;

  offset = XEXP (addr, 1);
  if (GET_CODE (addr) == PLUS)
    return satisfies_constraint_I (offset);

  else if (GET_CODE (addr) == LO_SUM)
    {
      if (TARGET_XCOFF || (TARGET_ELF && TARGET_POWERPC64))
	return small_toc_ref (offset, GET_MODE (offset));

      else if (TARGET_ELF && !TARGET_POWERPC64)
	return CONSTANT_P (offset);
    }

  return 0;
})

;; Like fusion_addis_mem_combo_load, but for stores
(define_predicate "fusion_addis_mem_combo_store"
  (match_code "mem")
{
  rtx addr, base, offset;

  if (!MEM_P (op) || !TARGET_P9_FUSION)
    return 0;

  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
      break;

    case DImode:
      if (!TARGET_POWERPC64)
	return 0;
      break;

    case SFmode:
      if (!TARGET_SF_FPR)
	return 0;
      break;

    case DFmode:
      if (!TARGET_DF_FPR)
	return 0;
      break;

    default:
      return 0;
    }

  addr = XEXP (op, 0);
  if (GET_CODE (addr) != PLUS && GET_CODE (addr) != LO_SUM)
    return 0;

  base = XEXP (addr, 0);
  if (!fusion_gpr_addis (base, GET_MODE (base)))
    return 0;

  offset = XEXP (addr, 1);
  if (GET_CODE (addr) == PLUS)
    return satisfies_constraint_I (offset);

  else if (GET_CODE (addr) == LO_SUM)
    {
      if (TARGET_XCOFF || (TARGET_ELF && TARGET_POWERPC64))
	return small_toc_ref (offset, GET_MODE (offset));

      else if (TARGET_ELF && !TARGET_POWERPC64)
	return CONSTANT_P (offset);
    }

  return 0;
})

;; Return true if the operand is a float_extend or zero extend of an
;; offsettable memory operand suitable for use in fusion
(define_predicate "fusion_offsettable_mem_operand"
  (match_code "mem,zero_extend,float_extend")
{
  if (GET_CODE (op) == ZERO_EXTEND || GET_CODE (op) == FLOAT_EXTEND)
    {
      op = XEXP (op, 0);
      mode = GET_MODE (op);
    }

  if (!memory_operand (op, mode))
    return 0;

  return offsettable_nonstrict_memref_p (op);
})
