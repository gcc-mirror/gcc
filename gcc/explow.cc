/* Subroutines for manipulating rtx's in semantically interesting ways.
   Copyright (C) 1987-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "expmed.h"
#include "profile-count.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "except.h"
#include "dojump.h"
#include "explow.h"
#include "expr.h"
#include "stringpool.h"
#include "common/common-target.h"
#include "output.h"

static rtx break_out_memory_refs (rtx);


/* Truncate and perhaps sign-extend C as appropriate for MODE.  */

HOST_WIDE_INT
trunc_int_for_mode (HOST_WIDE_INT c, machine_mode mode)
{
  /* Not scalar_int_mode because we also allow pointer bound modes.  */
  scalar_mode smode = as_a <scalar_mode> (mode);
  int width = GET_MODE_PRECISION (smode);

  /* You want to truncate to a _what_?  */
  gcc_assert (SCALAR_INT_MODE_P (mode));

  /* Canonicalize BImode to 0 and STORE_FLAG_VALUE.  */
  if (smode == BImode)
    return c & 1 ? STORE_FLAG_VALUE : 0;

  /* Sign-extend for the requested mode.  */

  if (width < HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT sign = 1;
      sign <<= width - 1;
      c &= (sign << 1) - 1;
      c ^= sign;
      c -= sign;
    }

  return c;
}

/* Likewise for polynomial values, using the sign-extended representation
   for each individual coefficient.  */

poly_int64
trunc_int_for_mode (poly_int64 x, machine_mode mode)
{
  for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
    x.coeffs[i] = trunc_int_for_mode (x.coeffs[i], mode);
  return x;
}

/* Return an rtx for the sum of X and the integer C, given that X has
   mode MODE.  INPLACE is true if X can be modified inplace or false
   if it must be treated as immutable.  */

rtx
plus_constant (machine_mode mode, rtx x, poly_int64 c, bool inplace)
{
  RTX_CODE code;
  rtx y;
  rtx tem;
  int all_constant = 0;

  gcc_assert (GET_MODE (x) == VOIDmode || GET_MODE (x) == mode);

  if (known_eq (c, 0))
    return x;

 restart:

  code = GET_CODE (x);
  y = x;

  switch (code)
    {
    CASE_CONST_SCALAR_INT:
      return immed_wide_int_const (wi::add (rtx_mode_t (x, mode), c), mode);
    case MEM:
      /* If this is a reference to the constant pool, try replacing it with
	 a reference to a new constant.  If the resulting address isn't
	 valid, don't return it because we have no way to validize it.  */
      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))
	{
	  rtx cst = get_pool_constant (XEXP (x, 0));

	  if (GET_CODE (cst) == CONST_VECTOR
	      && GET_MODE_INNER (GET_MODE (cst)) == mode)
	    {
	      cst = gen_lowpart (mode, cst);
	      gcc_assert (cst);
	    }
	  else if (GET_MODE (cst) == VOIDmode
		   && get_pool_mode (XEXP (x, 0)) != mode)
	    break;
	  if (GET_MODE (cst) == VOIDmode || GET_MODE (cst) == mode)
	    {
	      tem = plus_constant (mode, cst, c);
	      tem = force_const_mem (GET_MODE (x), tem);
	      /* Targets may disallow some constants in the constant pool, thus
		 force_const_mem may return NULL_RTX.  */
	      if (tem && memory_address_p (GET_MODE (tem), XEXP (tem, 0)))
		return tem;
	    }
	}
      break;

    case CONST:
      /* If adding to something entirely constant, set a flag
	 so that we can add a CONST around the result.  */
      if (inplace && shared_const_p (x))
	inplace = false;
      x = XEXP (x, 0);
      all_constant = 1;
      goto restart;

    case SYMBOL_REF:
    case LABEL_REF:
      all_constant = 1;
      break;

    case PLUS:
      /* The interesting case is adding the integer to a sum.  Look
	 for constant term in the sum and combine with C.  For an
	 integer constant term or a constant term that is not an
	 explicit integer, we combine or group them together anyway.

	 We may not immediately return from the recursive call here, lest
	 all_constant gets lost.  */

      if (CONSTANT_P (XEXP (x, 1)))
	{
	  rtx term = plus_constant (mode, XEXP (x, 1), c, inplace);
	  if (term == const0_rtx)
	    x = XEXP (x, 0);
	  else if (inplace)
	    XEXP (x, 1) = term;
	  else
	    x = gen_rtx_PLUS (mode, XEXP (x, 0), term);
	  c = 0;
	}
      else if (rtx *const_loc = find_constant_term_loc (&y))
	{
	  if (!inplace)
	    {
	      /* We need to be careful since X may be shared and we can't
		 modify it in place.  */
	      x = copy_rtx (x);
	      const_loc = find_constant_term_loc (&x);
	    }
	  *const_loc = plus_constant (mode, *const_loc, c, true);
	  c = 0;
	}
      break;

    default:
      if (CONST_POLY_INT_P (x))
	return immed_wide_int_const (const_poly_int_value (x) + c, mode);
      break;
    }

  if (maybe_ne (c, 0))
    x = gen_rtx_PLUS (mode, x, gen_int_mode (c, mode));

  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    return x;
  else if (all_constant)
    return gen_rtx_CONST (mode, x);
  else
    return x;
}

/* If X is a sum, return a new sum like X but lacking any constant terms.
   Add all the removed constant terms into *CONSTPTR.
   X itself is not altered.  The result != X if and only if
   it is not isomorphic to X.  */

rtx
eliminate_constant_term (rtx x, rtx *constptr)
{
  rtx x0, x1;
  rtx tem;

  if (GET_CODE (x) != PLUS)
    return x;

  /* First handle constants appearing at this level explicitly.  */
  if (CONST_INT_P (XEXP (x, 1))
      && (tem = simplify_binary_operation (PLUS, GET_MODE (x), *constptr,
					   XEXP (x, 1))) != 0
      && CONST_INT_P (tem))
    {
      *constptr = tem;
      return eliminate_constant_term (XEXP (x, 0), constptr);
    }

  tem = const0_rtx;
  x0 = eliminate_constant_term (XEXP (x, 0), &tem);
  x1 = eliminate_constant_term (XEXP (x, 1), &tem);
  if ((x1 != XEXP (x, 1) || x0 != XEXP (x, 0))
      && (tem = simplify_binary_operation (PLUS, GET_MODE (x),
					   *constptr, tem)) != 0
      && CONST_INT_P (tem))
    {
      *constptr = tem;
      return gen_rtx_PLUS (GET_MODE (x), x0, x1);
    }

  return x;
}


/* Return a copy of X in which all memory references
   and all constants that involve symbol refs
   have been replaced with new temporary registers.
   Also emit code to load the memory locations and constants
   into those registers.

   If X contains no such constants or memory references,
   X itself (not a copy) is returned.

   If a constant is found in the address that is not a legitimate constant
   in an insn, it is left alone in the hope that it might be valid in the
   address.

   X may contain no arithmetic except addition, subtraction and multiplication.
   Values returned by expand_expr with 1 for sum_ok fit this constraint.  */

static rtx
break_out_memory_refs (rtx x)
{
  if (MEM_P (x)
      || (CONSTANT_P (x) && CONSTANT_ADDRESS_P (x)
	  && GET_MODE (x) != VOIDmode))
    x = force_reg (GET_MODE (x), x);
  else if (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	   || GET_CODE (x) == MULT)
    {
      rtx op0 = break_out_memory_refs (XEXP (x, 0));
      rtx op1 = break_out_memory_refs (XEXP (x, 1));

      if (op0 != XEXP (x, 0) || op1 != XEXP (x, 1))
	x = simplify_gen_binary (GET_CODE (x), GET_MODE (x), op0, op1);
    }

  return x;
}

/* Given X, a memory address in address space AS' pointer mode, convert it to
   an address in the address space's address mode, or vice versa (TO_MODE says
   which way).  We take advantage of the fact that pointers are not allowed to
   overflow by commuting arithmetic operations over conversions so that address
   arithmetic insns can be used. IN_CONST is true if this conversion is inside
   a CONST. NO_EMIT is true if no insns should be emitted, and instead
   it should return NULL if it can't be simplified without emitting insns.  */

rtx
convert_memory_address_addr_space_1 (scalar_int_mode to_mode ATTRIBUTE_UNUSED,
				     rtx x, addr_space_t as ATTRIBUTE_UNUSED,
				     bool in_const ATTRIBUTE_UNUSED,
				     bool no_emit ATTRIBUTE_UNUSED)
{
#ifndef POINTERS_EXTEND_UNSIGNED
  gcc_assert (GET_MODE (x) == to_mode || GET_MODE (x) == VOIDmode);
  return x;
#else /* defined(POINTERS_EXTEND_UNSIGNED) */
  scalar_int_mode pointer_mode, address_mode, from_mode;
  rtx temp;
  enum rtx_code code;

  /* If X already has the right mode, just return it.  */
  if (GET_MODE (x) == to_mode)
    return x;

  pointer_mode = targetm.addr_space.pointer_mode (as);
  address_mode = targetm.addr_space.address_mode (as);
  from_mode = to_mode == pointer_mode ? address_mode : pointer_mode;

  /* Here we handle some special cases.  If none of them apply, fall through
     to the default case.  */
  switch (GET_CODE (x))
    {
    CASE_CONST_SCALAR_INT:
      if (GET_MODE_SIZE (to_mode) < GET_MODE_SIZE (from_mode))
	code = TRUNCATE;
      else if (POINTERS_EXTEND_UNSIGNED < 0)
	break;
      else if (POINTERS_EXTEND_UNSIGNED > 0)
	code = ZERO_EXTEND;
      else
	code = SIGN_EXTEND;
      temp = simplify_unary_operation (code, to_mode, x, from_mode);
      if (temp)
	return temp;
      break;

    case SUBREG:
      if ((SUBREG_PROMOTED_VAR_P (x) || REG_POINTER (SUBREG_REG (x)))
	  && GET_MODE (SUBREG_REG (x)) == to_mode)
	return SUBREG_REG (x);
      break;

    case LABEL_REF:
      temp = gen_rtx_LABEL_REF (to_mode, label_ref_label (x));
      LABEL_REF_NONLOCAL_P (temp) = LABEL_REF_NONLOCAL_P (x);
      return temp;

    case SYMBOL_REF:
      temp = shallow_copy_rtx (x);
      PUT_MODE (temp, to_mode);
      return temp;

    case CONST:
      {
	auto *last = no_emit ? nullptr : get_last_insn ();
	temp = convert_memory_address_addr_space_1 (to_mode, XEXP (x, 0), as,
						    true, no_emit);
	if (temp && (no_emit || last == get_last_insn ()))
	  return gen_rtx_CONST (to_mode, temp);
	return temp;
      }

    case PLUS:
    case MULT:
      /* For addition we can safely permute the conversion and addition
	 operation if one operand is a constant and converting the constant
	 does not change it or if one operand is a constant and we are
	 using a ptr_extend instruction  (POINTERS_EXTEND_UNSIGNED < 0).
	 We can always safely permute them if we are making the address
	 narrower. Inside a CONST RTL, this is safe for both pointers
	 zero or sign extended as pointers cannot wrap. */
      if (GET_MODE_SIZE (to_mode) < GET_MODE_SIZE (from_mode)
	  || (GET_CODE (x) == PLUS
	      && CONST_INT_P (XEXP (x, 1))
	      && ((in_const && POINTERS_EXTEND_UNSIGNED != 0)
		  || XEXP (x, 1) == convert_memory_address_addr_space_1
				     (to_mode, XEXP (x, 1), as, in_const,
				      no_emit)
                  || POINTERS_EXTEND_UNSIGNED < 0)))
	{
	  temp = convert_memory_address_addr_space_1 (to_mode, XEXP (x, 0),
						      as, in_const, no_emit);
	  return (temp ? gen_rtx_fmt_ee (GET_CODE (x), to_mode,
					 temp, XEXP (x, 1))
		       : temp);
	}
      break;

    case UNSPEC:
      /* Assume that all UNSPECs in a constant address can be converted
	 operand-by-operand.  We could add a target hook if some targets
	 require different behavior.  */
      if (in_const && GET_MODE (x) == from_mode)
	{
	  unsigned int n = XVECLEN (x, 0);
	  rtvec v = gen_rtvec (n);
	  for (unsigned int i = 0; i < n; ++i)
	    {
	      rtx op = XVECEXP (x, 0, i);
	      if (GET_MODE (op) == from_mode)
		op = convert_memory_address_addr_space_1 (to_mode, op, as,
							  in_const, no_emit);
	      RTVEC_ELT (v, i) = op;
	    }
	  return gen_rtx_UNSPEC (to_mode, v, XINT (x, 1));
	}
      break;

    default:
      break;
    }

  if (no_emit)
    return NULL_RTX;

  return convert_modes (to_mode, from_mode,
			x, POINTERS_EXTEND_UNSIGNED);
#endif /* defined(POINTERS_EXTEND_UNSIGNED) */
}

/* Given X, a memory address in address space AS' pointer mode, convert it to
   an address in the address space's address mode, or vice versa (TO_MODE says
   which way).  We take advantage of the fact that pointers are not allowed to
   overflow by commuting arithmetic operations over conversions so that address
   arithmetic insns can be used.  */

rtx
convert_memory_address_addr_space (scalar_int_mode to_mode, rtx x,
				   addr_space_t as)
{
  return convert_memory_address_addr_space_1 (to_mode, x, as, false, false);
}


/* Return something equivalent to X but valid as a memory address for something
   of mode MODE in the named address space AS.  When X is not itself valid,
   this works by copying X or subexpressions of it into registers.  */

rtx
memory_address_addr_space (machine_mode mode, rtx x, addr_space_t as)
{
  rtx oldx = x;
  scalar_int_mode address_mode = targetm.addr_space.address_mode (as);

  x = convert_memory_address_addr_space (address_mode, x, as);

  /* By passing constant addresses through registers
     we get a chance to cse them.  */
  if (! cse_not_expected && CONSTANT_P (x) && CONSTANT_ADDRESS_P (x))
    x = force_reg (address_mode, x);

  /* We get better cse by rejecting indirect addressing at this stage.
     Let the combiner create indirect addresses where appropriate.
     For now, generate the code so that the subexpressions useful to share
     are visible.  But not if cse won't be done!  */
  else
    {
      if (! cse_not_expected && !REG_P (x))
	x = break_out_memory_refs (x);

      /* At this point, any valid address is accepted.  */
      if (memory_address_addr_space_p (mode, x, as))
	goto done;

      /* If it was valid before but breaking out memory refs invalidated it,
	 use it the old way.  */
      if (memory_address_addr_space_p (mode, oldx, as))
	{
	  x = oldx;
	  goto done;
	}

      /* Perform machine-dependent transformations on X
	 in certain cases.  This is not necessary since the code
	 below can handle all possible cases, but machine-dependent
	 transformations can make better code.  */
      {
	rtx orig_x = x;
	x = targetm.addr_space.legitimize_address (x, oldx, mode, as);
	if (orig_x != x && memory_address_addr_space_p (mode, x, as))
	  goto done;
      }

      /* PLUS and MULT can appear in special ways
	 as the result of attempts to make an address usable for indexing.
	 Usually they are dealt with by calling force_operand, below.
	 But a sum containing constant terms is special
	 if removing them makes the sum a valid address:
	 then we generate that address in a register
	 and index off of it.  We do this because it often makes
	 shorter code, and because the addresses thus generated
	 in registers often become common subexpressions.  */
      if (GET_CODE (x) == PLUS)
	{
	  rtx constant_term = const0_rtx;
	  rtx y = eliminate_constant_term (x, &constant_term);
	  if (constant_term == const0_rtx
	      || ! memory_address_addr_space_p (mode, y, as))
	    x = force_operand (x, NULL_RTX);
	  else
	    {
	      y = gen_rtx_PLUS (GET_MODE (x), copy_to_reg (y), constant_term);
	      if (! memory_address_addr_space_p (mode, y, as))
		x = force_operand (x, NULL_RTX);
	      else
		x = y;
	    }
	}

      else if (GET_CODE (x) == MULT || GET_CODE (x) == MINUS)
	x = force_operand (x, NULL_RTX);

      /* If we have a register that's an invalid address,
	 it must be a hard reg of the wrong class.  Copy it to a pseudo.  */
      else if (REG_P (x))
	x = copy_to_reg (x);

      /* Last resort: copy the value to a register, since
	 the register is a valid address.  */
      else
	x = force_reg (address_mode, x);
    }

 done:

  gcc_assert (memory_address_addr_space_p (mode, x, as));
  /* If we didn't change the address, we are done.  Otherwise, mark
     a reg as a pointer if we have REG or REG + CONST_INT.  */
  if (oldx == x)
    return x;
  else if (REG_P (x))
    mark_reg_pointer (x, BITS_PER_UNIT);
  else if (GET_CODE (x) == PLUS
	   && REG_P (XEXP (x, 0))
	   && CONST_INT_P (XEXP (x, 1)))
    mark_reg_pointer (XEXP (x, 0), BITS_PER_UNIT);

  /* OLDX may have been the address on a temporary.  Update the address
     to indicate that X is now used.  */
  update_temp_slot_address (oldx, x);

  return x;
}

/* Convert a mem ref into one with a valid memory address.
   Pass through anything else unchanged.  */

rtx
validize_mem (rtx ref)
{
  if (!MEM_P (ref))
    return ref;
  ref = use_anchored_address (ref);
  if (memory_address_addr_space_p (GET_MODE (ref), XEXP (ref, 0),
				   MEM_ADDR_SPACE (ref)))
    return ref;

  /* Don't alter REF itself, since that is probably a stack slot.  */
  return replace_equiv_address (ref, XEXP (ref, 0));
}

/* If X is a memory reference to a member of an object block, try rewriting
   it to use an anchor instead.  Return the new memory reference on success
   and the old one on failure.  */

rtx
use_anchored_address (rtx x)
{
  rtx base;
  HOST_WIDE_INT offset;
  machine_mode mode;

  if (!flag_section_anchors)
    return x;

  if (!MEM_P (x))
    return x;

  /* Split the address into a base and offset.  */
  base = XEXP (x, 0);
  offset = 0;
  if (GET_CODE (base) == CONST
      && GET_CODE (XEXP (base, 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (base, 0), 1)))
    {
      offset += INTVAL (XEXP (XEXP (base, 0), 1));
      base = XEXP (XEXP (base, 0), 0);
    }

  /* Check whether BASE is suitable for anchors.  */
  if (GET_CODE (base) != SYMBOL_REF
      || !SYMBOL_REF_HAS_BLOCK_INFO_P (base)
      || SYMBOL_REF_ANCHOR_P (base)
      || SYMBOL_REF_BLOCK (base) == NULL
      || !targetm.use_anchors_for_symbol_p (base))
    return x;

  /* Decide where BASE is going to be.  */
  place_block_symbol (base);

  /* Get the anchor we need to use.  */
  offset += SYMBOL_REF_BLOCK_OFFSET (base);
  base = get_section_anchor (SYMBOL_REF_BLOCK (base), offset,
			     SYMBOL_REF_TLS_MODEL (base));

  /* Work out the offset from the anchor.  */
  offset -= SYMBOL_REF_BLOCK_OFFSET (base);

  /* If we're going to run a CSE pass, force the anchor into a register.
     We will then be able to reuse registers for several accesses, if the
     target costs say that that's worthwhile.  */
  mode = GET_MODE (base);
  if (!cse_not_expected)
    base = force_reg (mode, base);

  return replace_equiv_address (x, plus_constant (mode, base, offset));
}

/* Copy the value or contents of X to a new temp reg and return that reg.  */

rtx
copy_to_reg (rtx x)
{
  rtx temp = gen_reg_rtx (GET_MODE (x));

  /* If not an operand, must be an address with PLUS and MULT so
     do the computation.  */
  if (! general_operand (x, VOIDmode))
    x = force_operand (x, temp);

  if (x != temp)
    emit_move_insn (temp, x);

  return temp;
}

/* Like copy_to_reg but always give the new register mode Pmode
   in case X is a constant.  */

rtx
copy_addr_to_reg (rtx x)
{
  return copy_to_mode_reg (Pmode, x);
}

/* Like copy_to_reg but always give the new register mode MODE
   in case X is a constant.  */

rtx
copy_to_mode_reg (machine_mode mode, rtx x)
{
  rtx temp = gen_reg_rtx (mode);

  /* If not an operand, must be an address with PLUS and MULT so
     do the computation.  */
  if (! general_operand (x, VOIDmode))
    x = force_operand (x, temp);

  gcc_assert (GET_MODE (x) == mode || GET_MODE (x) == VOIDmode);
  if (x != temp)
    emit_move_insn (temp, x);
  return temp;
}

/* Load X into a register if it is not already one.
   Use mode MODE for the register.
   X should be valid for mode MODE, but it may be a constant which
   is valid for all integer modes; that's why caller must specify MODE.

   The caller must not alter the value in the register we return,
   since we mark it as a "constant" register.  */

rtx
force_reg (machine_mode mode, rtx x)
{
  rtx temp, set;
  rtx_insn *insn;

  if (REG_P (x))
    return x;

  if (general_operand (x, mode))
    {
      temp = gen_reg_rtx (mode);
      insn = emit_move_insn (temp, x);
    }
  else
    {
      temp = force_operand (x, NULL_RTX);
      if (REG_P (temp))
	insn = get_last_insn ();
      else
	{
	  rtx temp2 = gen_reg_rtx (mode);
	  insn = emit_move_insn (temp2, temp);
	  temp = temp2;
	}
    }

  /* Let optimizers know that TEMP's value never changes
     and that X can be substituted for it.  Don't get confused
     if INSN set something else (such as a SUBREG of TEMP).  */
  if (CONSTANT_P (x)
      && (set = single_set (insn)) != 0
      && SET_DEST (set) == temp
      && ! rtx_equal_p (x, SET_SRC (set)))
    set_unique_reg_note (insn, REG_EQUAL, x);

  /* Let optimizers know that TEMP is a pointer, and if so, the
     known alignment of that pointer.  */
  {
    unsigned align = 0;
    if (GET_CODE (x) == SYMBOL_REF)
      {
        align = BITS_PER_UNIT;
	if (SYMBOL_REF_DECL (x) && DECL_P (SYMBOL_REF_DECL (x)))
	  align = DECL_ALIGN (SYMBOL_REF_DECL (x));
      }
    else if (GET_CODE (x) == LABEL_REF)
      align = BITS_PER_UNIT;
    else if (GET_CODE (x) == CONST
	     && GET_CODE (XEXP (x, 0)) == PLUS
	     && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	     && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
      {
	rtx s = XEXP (XEXP (x, 0), 0);
	rtx c = XEXP (XEXP (x, 0), 1);
	unsigned sa, ca;

	sa = BITS_PER_UNIT;
	if (SYMBOL_REF_DECL (s) && DECL_P (SYMBOL_REF_DECL (s)))
	  sa = DECL_ALIGN (SYMBOL_REF_DECL (s));

	if (INTVAL (c) == 0)
	  align = sa;
	else
	  {
	    ca = ctz_hwi (INTVAL (c)) * BITS_PER_UNIT;
	    align = MIN (sa, ca);
	  }
      }

    if (align || (MEM_P (x) && MEM_POINTER (x)))
      mark_reg_pointer (temp, align);
  }

  return temp;
}

/* If X is a memory ref, copy its contents to a new temp reg and return
   that reg.  Otherwise, return X.  */

rtx
force_not_mem (rtx x)
{
  rtx temp;

  if (!MEM_P (x) || GET_MODE (x) == BLKmode)
    return x;

  temp = gen_reg_rtx (GET_MODE (x));

  if (MEM_POINTER (x))
    REG_POINTER (temp) = 1;

  emit_move_insn (temp, x);
  return temp;
}

/* Copy X to TARGET (if it's nonzero and a reg)
   or to a new temp reg and return that reg.
   MODE is the mode to use for X in case it is a constant.  */

rtx
copy_to_suggested_reg (rtx x, rtx target, machine_mode mode)
{
  rtx temp;

  if (target && REG_P (target))
    temp = target;
  else
    temp = gen_reg_rtx (mode);

  emit_move_insn (temp, x);
  return temp;
}

/* Return the mode to use to pass or return a scalar of TYPE and MODE.
   PUNSIGNEDP points to the signedness of the type and may be adjusted
   to show what signedness to use on extension operations.

   FOR_RETURN is nonzero if the caller is promoting the return value
   of FNDECL, else it is for promoting args.  */

machine_mode
promote_function_mode (const_tree type, machine_mode mode, int *punsignedp,
		       const_tree funtype, int for_return)
{
  /* Called without a type node for a libcall.  */
  if (type == NULL_TREE)
    {
      if (INTEGRAL_MODE_P (mode))
	return targetm.calls.promote_function_mode (NULL_TREE, mode,
						    punsignedp, funtype,
						    for_return);
      else
	return mode;
    }

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:   case ENUMERAL_TYPE:   case BOOLEAN_TYPE:
    case REAL_TYPE:      case OFFSET_TYPE:     case FIXED_POINT_TYPE:
    case POINTER_TYPE:   case REFERENCE_TYPE:
      return targetm.calls.promote_function_mode (type, mode, punsignedp, funtype,
						  for_return);

    default:
      return mode;
    }
}
/* Return the mode to use to store a scalar of TYPE and MODE.
   PUNSIGNEDP points to the signedness of the type and may be adjusted
   to show what signedness to use on extension operations.  */

machine_mode
promote_mode (const_tree type ATTRIBUTE_UNUSED, machine_mode mode,
	      int *punsignedp ATTRIBUTE_UNUSED)
{
#ifdef PROMOTE_MODE
  enum tree_code code;
  int unsignedp;
  scalar_mode smode;
#endif

  /* For libcalls this is invoked without TYPE from the backends
     TARGET_PROMOTE_FUNCTION_MODE hooks.  Don't do anything in that
     case.  */
  if (type == NULL_TREE)
    return mode;

  /* FIXME: this is the same logic that was there until GCC 4.4, but we
     probably want to test POINTERS_EXTEND_UNSIGNED even if PROMOTE_MODE
     is not defined.  The affected targets are M32C, S390, SPARC.  */
#ifdef PROMOTE_MODE
  code = TREE_CODE (type);
  unsignedp = *punsignedp;

  switch (code)
    {
    case INTEGER_TYPE:   case ENUMERAL_TYPE:   case BOOLEAN_TYPE:
    case REAL_TYPE:      case OFFSET_TYPE:     case FIXED_POINT_TYPE:
      /* Values of these types always have scalar mode.  */
      smode = as_a <scalar_mode> (mode);
      PROMOTE_MODE (smode, unsignedp, type);
      *punsignedp = unsignedp;
      return smode;

#ifdef POINTERS_EXTEND_UNSIGNED
    case REFERENCE_TYPE:
    case POINTER_TYPE:
      *punsignedp = POINTERS_EXTEND_UNSIGNED;
      return targetm.addr_space.address_mode
	       (TYPE_ADDR_SPACE (TREE_TYPE (type)));
#endif

    default:
      return mode;
    }
#else
  return mode;
#endif
}


/* Use one of promote_mode or promote_function_mode to find the promoted
   mode of DECL.  If PUNSIGNEDP is not NULL, store there the unsignedness
   of DECL after promotion.  */

machine_mode
promote_decl_mode (const_tree decl, int *punsignedp)
{
  tree type = TREE_TYPE (decl);
  int unsignedp = TYPE_UNSIGNED (type);
  machine_mode mode = DECL_MODE (decl);
  machine_mode pmode;

  if (TREE_CODE (decl) == RESULT_DECL && !DECL_BY_REFERENCE (decl))
    pmode = promote_function_mode (type, mode, &unsignedp,
                                   TREE_TYPE (current_function_decl), 1);
  else if (TREE_CODE (decl) == RESULT_DECL || TREE_CODE (decl) == PARM_DECL)
    pmode = promote_function_mode (type, mode, &unsignedp,
                                   TREE_TYPE (current_function_decl), 2);
  else
    pmode = promote_mode (type, mode, &unsignedp);

  if (punsignedp)
    *punsignedp = unsignedp;
  return pmode;
}

/* Return the promoted mode for name.  If it is a named SSA_NAME, it
   is the same as promote_decl_mode.  Otherwise, it is the promoted
   mode of a temp decl of same type as the SSA_NAME, if we had created
   one.  */

machine_mode
promote_ssa_mode (const_tree name, int *punsignedp)
{
  gcc_assert (TREE_CODE (name) == SSA_NAME);

  /* Partitions holding parms and results must be promoted as expected
     by function.cc.  */
  if (SSA_NAME_VAR (name)
      && (TREE_CODE (SSA_NAME_VAR (name)) == PARM_DECL
	  || TREE_CODE (SSA_NAME_VAR (name)) == RESULT_DECL))
    {
      machine_mode mode = promote_decl_mode (SSA_NAME_VAR (name), punsignedp);
      if (mode != BLKmode)
	return mode;
    }

  tree type = TREE_TYPE (name);
  int unsignedp = TYPE_UNSIGNED (type);
  machine_mode pmode = promote_mode (type, TYPE_MODE (type), &unsignedp);
  if (punsignedp)
    *punsignedp = unsignedp;

  return pmode;
}



/* Controls the behavior of {anti_,}adjust_stack.  */
static bool suppress_reg_args_size;

/* A helper for adjust_stack and anti_adjust_stack.  */

static void
adjust_stack_1 (rtx adjust, bool anti_p)
{
  rtx temp;
  rtx_insn *insn;

  /* Hereafter anti_p means subtract_p.  */
  if (!STACK_GROWS_DOWNWARD)
    anti_p = !anti_p;

  temp = expand_binop (Pmode,
		       anti_p ? sub_optab : add_optab,
		       stack_pointer_rtx, adjust, stack_pointer_rtx, 0,
		       OPTAB_LIB_WIDEN);

  if (temp != stack_pointer_rtx)
    insn = emit_move_insn (stack_pointer_rtx, temp);
  else
    {
      insn = get_last_insn ();
      temp = single_set (insn);
      gcc_assert (temp != NULL && SET_DEST (temp) == stack_pointer_rtx);
    }

  if (!suppress_reg_args_size)
    add_args_size_note (insn, stack_pointer_delta);
}

/* Adjust the stack pointer by ADJUST (an rtx for a number of bytes).
   This pops when ADJUST is positive.  ADJUST need not be constant.  */

void
adjust_stack (rtx adjust)
{
  if (adjust == const0_rtx)
    return;

  /* We expect all variable sized adjustments to be multiple of
     PREFERRED_STACK_BOUNDARY.  */
  poly_int64 const_adjust;
  if (poly_int_rtx_p (adjust, &const_adjust))
    stack_pointer_delta -= const_adjust;

  adjust_stack_1 (adjust, false);
}

/* Adjust the stack pointer by minus ADJUST (an rtx for a number of bytes).
   This pushes when ADJUST is positive.  ADJUST need not be constant.  */

void
anti_adjust_stack (rtx adjust)
{
  if (adjust == const0_rtx)
    return;

  /* We expect all variable sized adjustments to be multiple of
     PREFERRED_STACK_BOUNDARY.  */
  poly_int64 const_adjust;
  if (poly_int_rtx_p (adjust, &const_adjust))
    stack_pointer_delta += const_adjust;

  adjust_stack_1 (adjust, true);
}

/* Round the size of a block to be pushed up to the boundary required
   by this machine.  SIZE is the desired size, which need not be constant.  */

static rtx
round_push (rtx size)
{
  rtx align_rtx, alignm1_rtx;

  if (!SUPPORTS_STACK_ALIGNMENT
      || crtl->preferred_stack_boundary == MAX_SUPPORTED_STACK_ALIGNMENT)
    {
      int align = crtl->preferred_stack_boundary / BITS_PER_UNIT;

      if (align == 1)
	return size;

      if (CONST_INT_P (size))
	{
	  HOST_WIDE_INT new_size = (INTVAL (size) + align - 1) / align * align;

	  if (INTVAL (size) != new_size)
	    size = GEN_INT (new_size);
	  return size;
	}

      align_rtx = GEN_INT (align);
      alignm1_rtx = GEN_INT (align - 1);
    }
  else
    {
      /* If crtl->preferred_stack_boundary might still grow, use
	 virtual_preferred_stack_boundary_rtx instead.  This will be
	 substituted by the right value in vregs pass and optimized
	 during combine.  */
      align_rtx = virtual_preferred_stack_boundary_rtx;
      alignm1_rtx = force_operand (plus_constant (Pmode, align_rtx, -1),
				   NULL_RTX);
    }

  /* CEIL_DIV_EXPR needs to worry about the addition overflowing,
     but we know it can't.  So add ourselves and then do
     TRUNC_DIV_EXPR.  */
  size = expand_binop (Pmode, add_optab, size, alignm1_rtx,
		       NULL_RTX, 1, OPTAB_LIB_WIDEN);
  size = expand_divmod (0, TRUNC_DIV_EXPR, Pmode, size, align_rtx,
			NULL_RTX, 1);
  size = expand_mult (Pmode, size, align_rtx, NULL_RTX, 1);

  return size;
}

/* Save the stack pointer for the purpose in SAVE_LEVEL.  PSAVE is a pointer
   to a previously-created save area.  If no save area has been allocated,
   this function will allocate one.  If a save area is specified, it
   must be of the proper mode.  */

void
emit_stack_save (enum save_level save_level, rtx *psave)
{
  rtx sa = *psave;
  /* The default is that we use a move insn and save in a Pmode object.  */
  rtx_insn *(*fcn) (rtx, rtx) = gen_move_insn;
  machine_mode mode = STACK_SAVEAREA_MODE (save_level);

  /* See if this machine has anything special to do for this kind of save.  */
  switch (save_level)
    {
    case SAVE_BLOCK:
      if (targetm.have_save_stack_block ())
	fcn = targetm.gen_save_stack_block;
      break;
    case SAVE_FUNCTION:
      if (targetm.have_save_stack_function ())
	fcn = targetm.gen_save_stack_function;
      break;
    case SAVE_NONLOCAL:
      if (targetm.have_save_stack_nonlocal ())
	fcn = targetm.gen_save_stack_nonlocal;
      break;
    default:
      break;
    }

  /* If there is no save area and we have to allocate one, do so.  Otherwise
     verify the save area is the proper mode.  */

  if (sa == 0)
    {
      if (mode != VOIDmode)
	{
	  if (save_level == SAVE_NONLOCAL)
	    *psave = sa = assign_stack_local (mode, GET_MODE_SIZE (mode), 0);
	  else
	    *psave = sa = gen_reg_rtx (mode);
	}
    }

  do_pending_stack_adjust ();
  if (sa != 0)
    sa = validize_mem (sa);
  emit_insn (fcn (sa, stack_pointer_rtx));
}

/* Restore the stack pointer for the purpose in SAVE_LEVEL.  SA is the save
   area made by emit_stack_save.  If it is zero, we have nothing to do.  */

void
emit_stack_restore (enum save_level save_level, rtx sa)
{
  /* The default is that we use a move insn.  */
  rtx_insn *(*fcn) (rtx, rtx) = gen_move_insn;

  /* If stack_realign_drap, the x86 backend emits a prologue that aligns both
     STACK_POINTER and HARD_FRAME_POINTER.
     If stack_realign_fp, the x86 backend emits a prologue that aligns only
     STACK_POINTER. This renders the HARD_FRAME_POINTER unusable for accessing
     aligned variables, which is reflected in ix86_can_eliminate.
     We normally still have the realigned STACK_POINTER that we can use.
     But if there is a stack restore still present at reload, it can trigger 
     mark_not_eliminable for the STACK_POINTER, leaving no way to eliminate
     FRAME_POINTER into a hard reg.
     To prevent this situation, we force need_drap if we emit a stack
     restore.  */
  if (SUPPORTS_STACK_ALIGNMENT)
    crtl->need_drap = true;

  /* See if this machine has anything special to do for this kind of save.  */
  switch (save_level)
    {
    case SAVE_BLOCK:
      if (targetm.have_restore_stack_block ())
	fcn = targetm.gen_restore_stack_block;
      break;
    case SAVE_FUNCTION:
      if (targetm.have_restore_stack_function ())
	fcn = targetm.gen_restore_stack_function;
      break;
    case SAVE_NONLOCAL:
      if (targetm.have_restore_stack_nonlocal ())
	fcn = targetm.gen_restore_stack_nonlocal;
      break;
    default:
      break;
    }

  if (sa != 0)
    {
      sa = validize_mem (sa);
      /* These clobbers prevent the scheduler from moving
	 references to variable arrays below the code
	 that deletes (pops) the arrays.  */
      emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));
      emit_clobber (gen_rtx_MEM (BLKmode, stack_pointer_rtx));
    }

  discard_pending_stack_adjust ();

  emit_insn (fcn (stack_pointer_rtx, sa));
}

/* Invoke emit_stack_save on the nonlocal_goto_save_area for the current
   function.  This should be called whenever we allocate or deallocate
   dynamic stack space.  */

void
update_nonlocal_goto_save_area (void)
{
  tree t_save;
  rtx r_save;

  /* The nonlocal_goto_save_area object is an array of N pointers.  The
     first one is used for the frame pointer save; the rest are sized by
     STACK_SAVEAREA_MODE.  Create a reference to array index 1, the first
     of the stack save area slots.  */
  t_save = build4 (ARRAY_REF,
		   TREE_TYPE (TREE_TYPE (cfun->nonlocal_goto_save_area)),
		   cfun->nonlocal_goto_save_area,
		   integer_one_node, NULL_TREE, NULL_TREE);
  r_save = expand_expr (t_save, NULL_RTX, VOIDmode, EXPAND_WRITE);

  emit_stack_save (SAVE_NONLOCAL, &r_save);
}

/* Record a new stack level for the current function.  This should be called
   whenever we allocate or deallocate dynamic stack space.  */

void
record_new_stack_level (void)
{
  /* Record the new stack level for nonlocal gotos.  */
  if (cfun->nonlocal_goto_save_area)
    update_nonlocal_goto_save_area ();
 
  /* Record the new stack level for SJLJ exceptions.  */
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    update_sjlj_context ();
}

/* Return an rtx doing runtime alignment to REQUIRED_ALIGN on TARGET.  */

rtx
align_dynamic_address (rtx target, unsigned required_align)
{
  if (required_align == BITS_PER_UNIT)
    return target;

  /* CEIL_DIV_EXPR needs to worry about the addition overflowing,
     but we know it can't.  So add ourselves and then do
     TRUNC_DIV_EXPR.  */
  target = expand_binop (Pmode, add_optab, target,
			 gen_int_mode (required_align / BITS_PER_UNIT - 1,
				       Pmode),
			 NULL_RTX, 1, OPTAB_LIB_WIDEN);
  target = expand_divmod (0, TRUNC_DIV_EXPR, Pmode, target,
			  gen_int_mode (required_align / BITS_PER_UNIT,
					Pmode),
			  NULL_RTX, 1);
  target = expand_mult (Pmode, target,
			gen_int_mode (required_align / BITS_PER_UNIT,
				      Pmode),
			NULL_RTX, 1);

  return target;
}

/* Return an rtx through *PSIZE, representing the size of an area of memory to
   be dynamically pushed on the stack.

   *PSIZE is an rtx representing the size of the area.

   SIZE_ALIGN is the alignment (in bits) that we know SIZE has.  This
   parameter may be zero.  If so, a proper value will be extracted
   from SIZE if it is constant, otherwise BITS_PER_UNIT will be assumed.

   REQUIRED_ALIGN is the alignment (in bits) required for the region
   of memory.

   If PSTACK_USAGE_SIZE is not NULL it points to a value that is increased for
   the additional size returned.  */
void
get_dynamic_stack_size (rtx *psize, unsigned size_align,
			unsigned required_align,
			HOST_WIDE_INT *pstack_usage_size)
{
  rtx size = *psize;

  /* Ensure the size is in the proper mode.  */
  if (GET_MODE (size) != VOIDmode && GET_MODE (size) != Pmode)
    size = convert_to_mode (Pmode, size, 1);

  if (CONST_INT_P (size))
    {
      unsigned HOST_WIDE_INT lsb;

      lsb = INTVAL (size);
      lsb &= -lsb;

      /* Watch out for overflow truncating to "unsigned".  */
      if (lsb > UINT_MAX / BITS_PER_UNIT)
	size_align = 1u << (HOST_BITS_PER_INT - 1);
      else
	size_align = (unsigned)lsb * BITS_PER_UNIT;
    }
  else if (size_align < BITS_PER_UNIT)
    size_align = BITS_PER_UNIT;

  /* We can't attempt to minimize alignment necessary, because we don't
     know the final value of preferred_stack_boundary yet while executing
     this code.  */
  if (crtl->preferred_stack_boundary < PREFERRED_STACK_BOUNDARY)
    crtl->preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;

  /* We will need to ensure that the address we return is aligned to
     REQUIRED_ALIGN.  At this point in the compilation, we don't always
     know the final value of the STACK_DYNAMIC_OFFSET used in function.cc
     (it might depend on the size of the outgoing parameter lists, for
     example), so we must preventively align the value.  We leave space
     in SIZE for the hole that might result from the alignment operation.  */

  unsigned known_align = REGNO_POINTER_ALIGN (VIRTUAL_STACK_DYNAMIC_REGNUM);
  if (known_align == 0)
    known_align = BITS_PER_UNIT;
  if (required_align > known_align)
    {
      unsigned extra = (required_align - known_align) / BITS_PER_UNIT;
      size = plus_constant (Pmode, size, extra);
      size = force_operand (size, NULL_RTX);
      if (size_align > known_align)
	size_align = known_align;

      if (flag_stack_usage_info && pstack_usage_size)
	*pstack_usage_size += extra;
    }

  /* Round the size to a multiple of the required stack alignment.
     Since the stack is presumed to be rounded before this allocation,
     this will maintain the required alignment.

     If the stack grows downward, we could save an insn by subtracting
     SIZE from the stack pointer and then aligning the stack pointer.
     The problem with this is that the stack pointer may be unaligned
     between the execution of the subtraction and alignment insns and
     some machines do not allow this.  Even on those that do, some
     signal handlers malfunction if a signal should occur between those
     insns.  Since this is an extremely rare event, we have no reliable
     way of knowing which systems have this problem.  So we avoid even
     momentarily mis-aligning the stack.  */
  if (size_align % MAX_SUPPORTED_STACK_ALIGNMENT != 0)
    {
      size = round_push (size);

      if (flag_stack_usage_info && pstack_usage_size)
	{
	  int align = crtl->preferred_stack_boundary / BITS_PER_UNIT;
	  *pstack_usage_size =
	    (*pstack_usage_size + align - 1) / align * align;
	}
    }

  *psize = size;
}

/* Return the number of bytes to "protect" on the stack for -fstack-check.

   "protect" in the context of -fstack-check means how many bytes we need
   to always ensure are available on the stack; as a consequence, this is
   also how many bytes are first skipped when probing the stack.

   On some targets we want to reuse the -fstack-check prologue support
   to give a degree of protection against stack clashing style attacks.

   In that scenario we do not want to skip bytes before probing as that
   would render the stack clash protections useless.

   So we never use STACK_CHECK_PROTECT directly.  Instead we indirectly
   use it through this helper, which allows to provide different values
   for -fstack-check and -fstack-clash-protection.  */

HOST_WIDE_INT
get_stack_check_protect (void)
{
  if (flag_stack_clash_protection)
    return 0;

 return STACK_CHECK_PROTECT;
}

/* Return an rtx representing the address of an area of memory dynamically
   pushed on the stack.

   Any required stack pointer alignment is preserved.

   SIZE is an rtx representing the size of the area.

   SIZE_ALIGN is the alignment (in bits) that we know SIZE has.  This
   parameter may be zero.  If so, a proper value will be extracted
   from SIZE if it is constant, otherwise BITS_PER_UNIT will be assumed.

   REQUIRED_ALIGN is the alignment (in bits) required for the region
   of memory.

   MAX_SIZE is an upper bound for SIZE, if SIZE is not constant, or -1 if
   no such upper bound is known.

   If CANNOT_ACCUMULATE is set to TRUE, the caller guarantees that the
   stack space allocated by the generated code cannot be added with itself
   in the course of the execution of the function.  It is always safe to
   pass FALSE here and the following criterion is sufficient in order to
   pass TRUE: every path in the CFG that starts at the allocation point and
   loops to it executes the associated deallocation code.  */

rtx
allocate_dynamic_stack_space (rtx size, unsigned size_align,
			      unsigned required_align,
			      HOST_WIDE_INT max_size,
			      bool cannot_accumulate)
{
  HOST_WIDE_INT stack_usage_size = -1;
  rtx_code_label *final_label;
  rtx final_target, target;
  rtx addr = (virtuals_instantiated
	      ? plus_constant (Pmode, stack_pointer_rtx,
			       get_stack_dynamic_offset ())
	      : virtual_stack_dynamic_rtx);

  /* If we're asking for zero bytes, it doesn't matter what we point
     to since we can't dereference it.  But return a reasonable
     address anyway.  */
  if (size == const0_rtx)
    return addr;

  /* Otherwise, show we're calling alloca or equivalent.  */
  cfun->calls_alloca = 1;

  /* If stack usage info is requested, look into the size we are passed.
     We need to do so this early to avoid the obfuscation that may be
     introduced later by the various alignment operations.  */
  if (flag_stack_usage_info)
    {
      if (CONST_INT_P (size))
	stack_usage_size = INTVAL (size);
      else if (REG_P (size))
        {
	  /* Look into the last emitted insn and see if we can deduce
	     something for the register.  */
	  rtx_insn *insn;
	  rtx set, note;
	  insn = get_last_insn ();
	  if ((set = single_set (insn)) && rtx_equal_p (SET_DEST (set), size))
	    {
	      if (CONST_INT_P (SET_SRC (set)))
		stack_usage_size = INTVAL (SET_SRC (set));
	      else if ((note = find_reg_equal_equiv_note (insn))
		       && CONST_INT_P (XEXP (note, 0)))
		stack_usage_size = INTVAL (XEXP (note, 0));
	    }
	}

      /* If the size is not constant, try the maximum size.  */
      if (stack_usage_size < 0)
	stack_usage_size = max_size;

      /* If the size is still not constant, we can't say anything.  */
      if (stack_usage_size < 0)
	{
	  current_function_has_unbounded_dynamic_stack_size = 1;
	  stack_usage_size = 0;
	}
    }

  get_dynamic_stack_size (&size, size_align, required_align, &stack_usage_size);

  target = gen_reg_rtx (Pmode);

  /* The size is supposed to be fully adjusted at this point so record it
     if stack usage info is requested.  */
  if (flag_stack_usage_info)
    {
      current_function_dynamic_stack_size += stack_usage_size;

      /* ??? This is gross but the only safe stance in the absence
	 of stack usage oriented flow analysis.  */
      if (!cannot_accumulate)
	current_function_has_unbounded_dynamic_stack_size = 1;
    }

  do_pending_stack_adjust ();

  final_label = NULL;
  final_target = NULL_RTX;

  /* If we are splitting the stack, we need to ask the backend whether
     there is enough room on the current stack.  If there isn't, or if
     the backend doesn't know how to tell is, then we need to call a
     function to allocate memory in some other way.  This memory will
     be released when we release the current stack segment.  The
     effect is that stack allocation becomes less efficient, but at
     least it doesn't cause a stack overflow.  */
  if (flag_split_stack)
    {
      rtx_code_label *available_label;
      rtx ask, space, func;

      available_label = NULL;

      if (targetm.have_split_stack_space_check ())
	{
	  available_label = gen_label_rtx ();

	  /* This instruction will branch to AVAILABLE_LABEL if there
	     are SIZE bytes available on the stack.  */
	  emit_insn (targetm.gen_split_stack_space_check
		     (size, available_label));
	}

      /* The __morestack_allocate_stack_space function will allocate
	 memory using malloc.  If the alignment of the memory returned
	 by malloc does not meet REQUIRED_ALIGN, we increase SIZE to
	 make sure we allocate enough space.  */
      if (MALLOC_ABI_ALIGNMENT >= required_align)
	ask = size;
      else
	ask = expand_binop (Pmode, add_optab, size,
			    gen_int_mode (required_align / BITS_PER_UNIT - 1,
					  Pmode),
			    NULL_RTX, 1, OPTAB_LIB_WIDEN);

      func = init_one_libfunc ("__morestack_allocate_stack_space");

      space = emit_library_call_value (func, target, LCT_NORMAL, Pmode,
				       ask, Pmode);

      if (available_label == NULL_RTX)
	return space;

      final_target = gen_reg_rtx (Pmode);

      emit_move_insn (final_target, space);

      final_label = gen_label_rtx ();
      emit_jump (final_label);

      emit_label (available_label);
    }

 /* We ought to be called always on the toplevel and stack ought to be aligned
    properly.  */
  gcc_assert (multiple_p (stack_pointer_delta,
			  PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT));

  /* If needed, check that we have the required amount of stack.  Take into
     account what has already been checked.  */
  if (STACK_CHECK_MOVING_SP)
    ;
  else if (flag_stack_check == GENERIC_STACK_CHECK)
    probe_stack_range (STACK_OLD_CHECK_PROTECT + STACK_CHECK_MAX_FRAME_SIZE,
		       size);
  else if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    probe_stack_range (get_stack_check_protect (), size);

  /* Don't let anti_adjust_stack emit notes.  */
  suppress_reg_args_size = true;

  /* Perform the required allocation from the stack.  Some systems do
     this differently than simply incrementing/decrementing from the
     stack pointer, such as acquiring the space by calling malloc().  */
  if (targetm.have_allocate_stack ())
    {
      class expand_operand ops[2];
      /* We don't have to check against the predicate for operand 0 since
	 TARGET is known to be a pseudo of the proper mode, which must
	 be valid for the operand.  */
      create_fixed_operand (&ops[0], target);
      create_convert_operand_to (&ops[1], size, STACK_SIZE_MODE, true);
      expand_insn (targetm.code_for_allocate_stack, 2, ops);
    }
  else
    {
      poly_int64 saved_stack_pointer_delta;

      if (!STACK_GROWS_DOWNWARD)
	emit_move_insn (target, force_operand (addr, target));

      /* Check stack bounds if necessary.  */
      if (crtl->limit_stack)
	{
	  rtx available;
	  rtx_code_label *space_available = gen_label_rtx ();
	  if (STACK_GROWS_DOWNWARD)
	    available = expand_binop (Pmode, sub_optab,
				      stack_pointer_rtx, stack_limit_rtx,
				      NULL_RTX, 1, OPTAB_WIDEN);
	  else
	    available = expand_binop (Pmode, sub_optab,
				      stack_limit_rtx, stack_pointer_rtx,
				      NULL_RTX, 1, OPTAB_WIDEN);

	  emit_cmp_and_jump_insns (available, size, GEU, NULL_RTX, Pmode, 1,
				   space_available);
	  if (targetm.have_trap ())
	    emit_insn (targetm.gen_trap ());
	  else
	    error ("stack limits not supported on this target");
	  emit_barrier ();
	  emit_label (space_available);
	}

      saved_stack_pointer_delta = stack_pointer_delta;

      /* If stack checking or stack clash protection is requested,
	 then probe the stack while allocating space from it.  */
      if (flag_stack_check && STACK_CHECK_MOVING_SP)
	anti_adjust_stack_and_probe (size, false);
      else if (flag_stack_clash_protection)
	anti_adjust_stack_and_probe_stack_clash (size);
      else
	anti_adjust_stack (size);

      /* Even if size is constant, don't modify stack_pointer_delta.
	 The constant size alloca should preserve
	 crtl->preferred_stack_boundary alignment.  */
      stack_pointer_delta = saved_stack_pointer_delta;

      if (STACK_GROWS_DOWNWARD)
	emit_move_insn (target, force_operand (addr, target));
    }

  suppress_reg_args_size = false;

  /* Finish up the split stack handling.  */
  if (final_label != NULL_RTX)
    {
      gcc_assert (flag_split_stack);
      emit_move_insn (final_target, target);
      emit_label (final_label);
      target = final_target;
    }

  target = align_dynamic_address (target, required_align);

  /* Now that we've committed to a return value, mark its alignment.  */
  mark_reg_pointer (target, required_align);

  /* Record the new stack level.  */
  record_new_stack_level ();

  return target;
}

/* Return an rtx representing the address of an area of memory already
   statically pushed onto the stack in the virtual stack vars area.  (It is
   assumed that the area is allocated in the function prologue.)

   Any required stack pointer alignment is preserved.

   OFFSET is the offset of the area into the virtual stack vars area.

   REQUIRED_ALIGN is the alignment (in bits) required for the region
   of memory.

   BASE is the rtx of the base of this virtual stack vars area.
   The only time this is not `virtual_stack_vars_rtx` is when tagging pointers
   on the stack.  */

rtx
get_dynamic_stack_base (poly_int64 offset, unsigned required_align, rtx base)
{
  rtx target;

  if (crtl->preferred_stack_boundary < PREFERRED_STACK_BOUNDARY)
    crtl->preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;

  target = gen_reg_rtx (Pmode);
  emit_move_insn (target, base);
  target = expand_binop (Pmode, add_optab, target,
			 gen_int_mode (offset, Pmode),
			 NULL_RTX, 1, OPTAB_LIB_WIDEN);
  target = align_dynamic_address (target, required_align);

  /* Now that we've committed to a return value, mark its alignment.  */
  mark_reg_pointer (target, required_align);

  return target;
}

/* A front end may want to override GCC's stack checking by providing a
   run-time routine to call to check the stack, so provide a mechanism for
   calling that routine.  */

static GTY(()) rtx stack_check_libfunc;

void
set_stack_check_libfunc (const char *libfunc_name)
{
  gcc_assert (stack_check_libfunc == NULL_RTX);
  stack_check_libfunc = gen_rtx_SYMBOL_REF (Pmode, libfunc_name);
  tree ptype
    = Pmode == ptr_mode
      ? ptr_type_node
      : lang_hooks.types.type_for_mode (Pmode, 1);
  tree ftype
    = build_function_type_list (void_type_node, ptype, NULL_TREE);
  tree decl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
			  get_identifier (libfunc_name), ftype);
  DECL_EXTERNAL (decl) = 1;
  SET_SYMBOL_REF_DECL (stack_check_libfunc, decl);
}

/* Emit one stack probe at ADDRESS, an address within the stack.  */

void
emit_stack_probe (rtx address)
{
  if (targetm.have_probe_stack_address ())
    {
      class expand_operand ops[1];
      insn_code icode = targetm.code_for_probe_stack_address;
      create_address_operand (ops, address);
      maybe_legitimize_operands (icode, 0, 1, ops);
      expand_insn (icode, 1, ops);
    }
  else
    {
      rtx memref = gen_rtx_MEM (word_mode, address);

      MEM_VOLATILE_P (memref) = 1;
      memref = validize_mem (memref);

      /* See if we have an insn to probe the stack.  */
      if (targetm.have_probe_stack ())
	emit_insn (targetm.gen_probe_stack (memref));
      else
	emit_move_insn (memref, const0_rtx);
    }
}

/* Probe a range of stack addresses from FIRST to FIRST+SIZE, inclusive.
   FIRST is a constant and size is a Pmode RTX.  These are offsets from
   the current stack pointer.  STACK_GROWS_DOWNWARD says whether to add
   or subtract them from the stack pointer.  */

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

#if STACK_GROWS_DOWNWARD
#define STACK_GROW_OP MINUS
#define STACK_GROW_OPTAB sub_optab
#define STACK_GROW_OFF(off) -(off)
#else
#define STACK_GROW_OP PLUS
#define STACK_GROW_OPTAB add_optab
#define STACK_GROW_OFF(off) (off)
#endif

void
probe_stack_range (HOST_WIDE_INT first, rtx size)
{
  /* First ensure SIZE is Pmode.  */
  if (GET_MODE (size) != VOIDmode && GET_MODE (size) != Pmode)
    size = convert_to_mode (Pmode, size, 1);

  /* Next see if we have a function to check the stack.  */
  if (stack_check_libfunc)
    {
      rtx addr = memory_address (Pmode,
				 gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					         stack_pointer_rtx,
					         plus_constant (Pmode,
								size, first)));
      emit_library_call (stack_check_libfunc, LCT_THROW, VOIDmode,
			 addr, Pmode);
    }

  /* Next see if we have an insn to check the stack.  */
  else if (targetm.have_check_stack ())
    {
      class expand_operand ops[1];
      rtx addr = memory_address (Pmode,
				 gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					         stack_pointer_rtx,
					         plus_constant (Pmode,
								size, first)));
      bool success;
      create_input_operand (&ops[0], addr, Pmode);
      success = maybe_expand_insn (targetm.code_for_check_stack, 1, ops);
      gcc_assert (success);
    }

  /* Otherwise we have to generate explicit probes.  If we have a constant
     small number of them to generate, that's the easy case.  */
  else if (CONST_INT_P (size) && INTVAL (size) < 7 * PROBE_INTERVAL)
    {
      HOST_WIDE_INT isize = INTVAL (size), i;
      rtx addr;

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 1 until
	 it exceeds SIZE.  If only one probe is needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = PROBE_INTERVAL; i < isize; i += PROBE_INTERVAL)
	{
	  addr = memory_address (Pmode,
				 plus_constant (Pmode, stack_pointer_rtx,
				 		STACK_GROW_OFF (first + i)));
	  emit_stack_probe (addr);
	}

      addr = memory_address (Pmode,
			     plus_constant (Pmode, stack_pointer_rtx,
					    STACK_GROW_OFF (first + isize)));
      emit_stack_probe (addr);
    }

  /* In the variable case, do the same as above, but in a loop.  Note that we
     must be extra careful with variables wrapping around because we might be
     at the very top (or the very bottom) of the address space and we have to
     be able to handle this case properly; in particular, we use an equality
     test for the loop condition.  */
  else
    {
      rtx rounded_size, rounded_size_op, test_addr, last_addr, temp;
      rtx_code_label *loop_lab = gen_label_rtx ();
      rtx_code_label *end_lab = gen_label_rtx ();

      /* Step 1: round SIZE to the previous multiple of the interval.  */

      /* ROUNDED_SIZE = SIZE & -PROBE_INTERVAL  */
      rounded_size
	= simplify_gen_binary (AND, Pmode, size,
			       gen_int_mode (-PROBE_INTERVAL, Pmode));
      rounded_size_op = force_operand (rounded_size, NULL_RTX);


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_ADDR = SP + FIRST.  */
      test_addr = force_operand (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					 	 stack_pointer_rtx,
						 gen_int_mode (first, Pmode)),
				 NULL_RTX);

      /* LAST_ADDR = SP + FIRST + ROUNDED_SIZE.  */
      last_addr = force_operand (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
						 test_addr,
						 rounded_size_op), NULL_RTX);


      /* Step 3: the loop

	 while (TEST_ADDR != LAST_ADDR)
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      emit_label (loop_lab);

      /* Jump to END_LAB if TEST_ADDR == LAST_ADDR.  */
      emit_cmp_and_jump_insns (test_addr, last_addr, EQ, NULL_RTX, Pmode, 1,
			       end_lab);

      /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
      temp = expand_binop (Pmode, STACK_GROW_OPTAB, test_addr,
			   gen_int_mode (PROBE_INTERVAL, Pmode), test_addr,
			   1, OPTAB_WIDEN);

      /* There is no guarantee that expand_binop constructs its result
	 in TEST_ADDR.  So copy into TEST_ADDR if necessary.  */
      if (temp != test_addr)
	emit_move_insn (test_addr, temp);

      /* Probe at TEST_ADDR.  */
      emit_stack_probe (test_addr);

      emit_jump (loop_lab);

      emit_label (end_lab);


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      /* TEMP = SIZE - ROUNDED_SIZE.  */
      temp = simplify_gen_binary (MINUS, Pmode, size, rounded_size);
      if (temp != const0_rtx)
	{
	  rtx addr;

	  if (CONST_INT_P (temp))
	    {
	      /* Use [base + disp} addressing mode if supported.  */
	      HOST_WIDE_INT offset = INTVAL (temp);
	      addr = memory_address (Pmode,
				     plus_constant (Pmode, last_addr,
						    STACK_GROW_OFF (offset)));
	    }
	  else
	    {
	      /* Manual CSE if the difference is not known at compile-time.  */
	      temp = gen_rtx_MINUS (Pmode, size, rounded_size_op);
	      addr = memory_address (Pmode,
				     gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
						     last_addr, temp));
	    }

	  emit_stack_probe (addr);
	}
    }

  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Compute parameters for stack clash probing a dynamic stack
   allocation of SIZE bytes.

   We compute ROUNDED_SIZE, LAST_ADDR, RESIDUAL and PROBE_INTERVAL.

   Additionally we conditionally dump the type of probing that will
   be needed given the values computed.  */

void
compute_stack_clash_protection_loop_data (rtx *rounded_size, rtx *last_addr,
					  rtx *residual,
					  HOST_WIDE_INT *probe_interval,
					  rtx size)
{
  /* Round SIZE down to STACK_CLASH_PROTECTION_PROBE_INTERVAL */
  *probe_interval
    = 1 << param_stack_clash_protection_probe_interval;
  *rounded_size = simplify_gen_binary (AND, Pmode, size,
				        GEN_INT (-*probe_interval));

  /* Compute the value of the stack pointer for the last iteration.
     It's just SP + ROUNDED_SIZE.  */
  rtx rounded_size_op = force_operand (*rounded_size, NULL_RTX);
  *last_addr = force_operand (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					      stack_pointer_rtx,
					      rounded_size_op),
			      NULL_RTX);

  /* Compute any residuals not allocated by the loop above.  Residuals
     are just the ROUNDED_SIZE - SIZE.  */
  *residual = simplify_gen_binary (MINUS, Pmode, size, *rounded_size);

  /* Dump key information to make writing tests easy.  */
  if (dump_file)
    {
      if (*rounded_size == CONST0_RTX (Pmode))
	fprintf (dump_file,
		 "Stack clash skipped dynamic allocation and probing loop.\n");
      else if (CONST_INT_P (*rounded_size)
	       && INTVAL (*rounded_size) <= 4 * *probe_interval)
	fprintf (dump_file,
		 "Stack clash dynamic allocation and probing inline.\n");
      else if (CONST_INT_P (*rounded_size))
	fprintf (dump_file,
		 "Stack clash dynamic allocation and probing in "
		 "rotated loop.\n");
      else
	fprintf (dump_file,
		 "Stack clash dynamic allocation and probing in loop.\n");

      if (*residual != CONST0_RTX (Pmode))
	fprintf (dump_file,
		 "Stack clash dynamic allocation and probing residuals.\n");
      else
	fprintf (dump_file,
		 "Stack clash skipped dynamic allocation and "
		 "probing residuals.\n");
    }
}

/* Emit the start of an allocate/probe loop for stack
   clash protection.

   LOOP_LAB and END_LAB are returned for use when we emit the
   end of the loop.

   LAST addr is the value for SP which stops the loop.  */
void
emit_stack_clash_protection_probe_loop_start (rtx *loop_lab,
					      rtx *end_lab,
					      rtx last_addr,
					      bool rotated)
{
  /* Essentially we want to emit any setup code, the top of loop
     label and the comparison at the top of the loop.  */
  *loop_lab = gen_label_rtx ();
  *end_lab = gen_label_rtx ();

  emit_label (*loop_lab);
  if (!rotated)
    emit_cmp_and_jump_insns (stack_pointer_rtx, last_addr, EQ, NULL_RTX,
			     Pmode, 1, *end_lab);
}

/* Emit the end of a stack clash probing loop.

   This consists of just the jump back to LOOP_LAB and
   emitting END_LOOP after the loop.  */

void
emit_stack_clash_protection_probe_loop_end (rtx loop_lab, rtx end_loop,
					    rtx last_addr, bool rotated)
{
  if (rotated)
    emit_cmp_and_jump_insns (stack_pointer_rtx, last_addr, NE, NULL_RTX,
			     Pmode, 1, loop_lab);
  else
    emit_jump (loop_lab);

  emit_label (end_loop);

}

/* Adjust the stack pointer by minus SIZE (an rtx for a number of bytes)
   while probing it.  This pushes when SIZE is positive.  SIZE need not
   be constant.

   This is subtly different than anti_adjust_stack_and_probe to try and
   prevent stack-clash attacks

     1. It must assume no knowledge of the probing state, any allocation
	must probe.

	Consider the case of a 1 byte alloca in a loop.  If the sum of the
	allocations is large, then this could be used to jump the guard if
	probes were not emitted.

     2. It never skips probes, whereas anti_adjust_stack_and_probe will
	skip the probe on the first PROBE_INTERVAL on the assumption it
	was already done in the prologue and in previous allocations.

     3. It only allocates and probes SIZE bytes, it does not need to
	allocate/probe beyond that because this probing style does not
	guarantee signal handling capability if the guard is hit.  */

void
anti_adjust_stack_and_probe_stack_clash (rtx size)
{
  /* First ensure SIZE is Pmode.  */
  if (GET_MODE (size) != VOIDmode && GET_MODE (size) != Pmode)
    size = convert_to_mode (Pmode, size, 1);

  /* We can get here with a constant size on some targets.  */
  rtx rounded_size, last_addr, residual;
  HOST_WIDE_INT probe_interval, probe_range;
  bool target_probe_range_p = false;
  compute_stack_clash_protection_loop_data (&rounded_size, &last_addr,
					    &residual, &probe_interval, size);

  /* Get the back-end specific probe ranges.  */
  probe_range = targetm.stack_clash_protection_alloca_probe_range ();
  target_probe_range_p = probe_range != 0;
  gcc_assert (probe_range >= 0);

  /* If no back-end specific range defined, default to the top of the newly
     allocated range.  */
  if (probe_range == 0)
    probe_range = probe_interval - GET_MODE_SIZE (word_mode);

  if (rounded_size != CONST0_RTX (Pmode))
    {
      if (CONST_INT_P (rounded_size)
	  && INTVAL (rounded_size) <= 4 * probe_interval)
	{
	  for (HOST_WIDE_INT i = 0;
	       i < INTVAL (rounded_size);
	       i += probe_interval)
	    {
	      anti_adjust_stack (GEN_INT (probe_interval));
	      /* The prologue does not probe residuals.  Thus the offset
		 here to probe just beyond what the prologue had already
		 allocated.  */
	      emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					       probe_range));

	      emit_insn (gen_blockage ());
	    }
	}
      else
	{
	  rtx loop_lab, end_loop;
	  bool rotate_loop = CONST_INT_P (rounded_size);
	  emit_stack_clash_protection_probe_loop_start (&loop_lab, &end_loop,
							last_addr, rotate_loop);

	  anti_adjust_stack (GEN_INT (probe_interval));

	  /* The prologue does not probe residuals.  Thus the offset here
	     to probe just beyond what the prologue had already
	     allocated.  */
	  emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					   probe_range));

	  emit_stack_clash_protection_probe_loop_end (loop_lab, end_loop,
						      last_addr, rotate_loop);
	  emit_insn (gen_blockage ());
	}
    }

  if (residual != CONST0_RTX (Pmode))
    {
      rtx label = NULL_RTX;
      /* RESIDUAL could be zero at runtime and in that case *sp could
	 hold live data.  Furthermore, we do not want to probe into the
	 red zone.

	 If TARGET_PROBE_RANGE_P then the target has promised it's safe to
	 probe at offset 0.  In which case we no longer have to check for
	 RESIDUAL == 0.  However we still need to probe at the right offset
	 when RESIDUAL > PROBE_RANGE, in which case we probe at PROBE_RANGE.

	 If !TARGET_PROBE_RANGE_P then go ahead and just guard the probe at *sp
	 on RESIDUAL != 0 at runtime if RESIDUAL is not a compile time constant.
	 */
      anti_adjust_stack (residual);

      if (!CONST_INT_P (residual))
	{
	  label = gen_label_rtx ();
	  rtx_code op = target_probe_range_p ? LT : EQ;
	  rtx probe_cmp_value = target_probe_range_p
	    ? gen_rtx_CONST_INT (GET_MODE (residual), probe_range)
	    : CONST0_RTX (GET_MODE (residual));

	  if (target_probe_range_p)
	    emit_stack_probe (stack_pointer_rtx);

	  emit_cmp_and_jump_insns (residual, probe_cmp_value,
				   op, NULL_RTX, Pmode, 1, label);
	}

      rtx x = NULL_RTX;

      /* If RESIDUAL isn't a constant and TARGET_PROBE_RANGE_P then we probe up
	 by the ABI defined safe value.  */
      if (!CONST_INT_P (residual) && target_probe_range_p)
	x = GEN_INT (probe_range);
      /* If RESIDUAL is a constant but smaller than the ABI defined safe value,
	 we still want to probe up, but the safest amount if a word.  */
      else if (target_probe_range_p)
	{
	  if (INTVAL (residual) <= probe_range)
	    x = GEN_INT (GET_MODE_SIZE (word_mode));
	  else
	    x = GEN_INT (probe_range);
	}
      else
      /* If nothing else, probe at the top of the new allocation.  */
	x = plus_constant (Pmode, residual, -GET_MODE_SIZE (word_mode));

      emit_stack_probe (gen_rtx_PLUS (Pmode, stack_pointer_rtx, x));

      emit_insn (gen_blockage ());
      if (!CONST_INT_P (residual))
	  emit_label (label);
    }
}


/* Adjust the stack pointer by minus SIZE (an rtx for a number of bytes)
   while probing it.  This pushes when SIZE is positive.  SIZE need not
   be constant.  If ADJUST_BACK is true, adjust back the stack pointer
   by plus SIZE at the end.  */

void
anti_adjust_stack_and_probe (rtx size, bool adjust_back)
{
  /* We skip the probe for the first interval + a small dope of 4 words and
     probe that many bytes past the specified size to maintain a protection
     area at the botton of the stack.  */
  const int dope = 4 * UNITS_PER_WORD;

  /* First ensure SIZE is Pmode.  */
  if (GET_MODE (size) != VOIDmode && GET_MODE (size) != Pmode)
    size = convert_to_mode (Pmode, size, 1);

  /* If we have a constant small number of probes to generate, that's the
     easy case.  */
  if (CONST_INT_P (size) && INTVAL (size) < 7 * PROBE_INTERVAL)
    {
      HOST_WIDE_INT isize = INTVAL (size), i;
      bool first_probe = true;

      /* Adjust SP and probe at PROBE_INTERVAL + N * PROBE_INTERVAL for
	 values of N from 1 until it exceeds SIZE.  If only one probe is
	 needed, this will not generate any code.  Then adjust and probe
	 to PROBE_INTERVAL + SIZE.  */
      for (i = PROBE_INTERVAL; i < isize; i += PROBE_INTERVAL)
	{
	  if (first_probe)
	    {
	      anti_adjust_stack (GEN_INT (2 * PROBE_INTERVAL + dope));
	      first_probe = false;
	    }
	  else
	    anti_adjust_stack (GEN_INT (PROBE_INTERVAL));
	  emit_stack_probe (stack_pointer_rtx);
	}

      if (first_probe)
	anti_adjust_stack (plus_constant (Pmode, size, PROBE_INTERVAL + dope));
      else
	anti_adjust_stack (plus_constant (Pmode, size, PROBE_INTERVAL - i));
      emit_stack_probe (stack_pointer_rtx);
    }

  /* In the variable case, do the same as above, but in a loop.  Note that we
     must be extra careful with variables wrapping around because we might be
     at the very top (or the very bottom) of the address space and we have to
     be able to handle this case properly; in particular, we use an equality
     test for the loop condition.  */
  else
    {
      rtx rounded_size, rounded_size_op, last_addr, temp;
      rtx_code_label *loop_lab = gen_label_rtx ();
      rtx_code_label *end_lab = gen_label_rtx ();


      /* Step 1: round SIZE to the previous multiple of the interval.  */

      /* ROUNDED_SIZE = SIZE & -PROBE_INTERVAL  */
      rounded_size
	= simplify_gen_binary (AND, Pmode, size,
			       gen_int_mode (-PROBE_INTERVAL, Pmode));
      rounded_size_op = force_operand (rounded_size, NULL_RTX);


      /* Step 2: compute initial and final value of the loop counter.  */

      /* SP = SP_0 + PROBE_INTERVAL.  */
      anti_adjust_stack (GEN_INT (PROBE_INTERVAL + dope));

      /* LAST_ADDR = SP_0 + PROBE_INTERVAL + ROUNDED_SIZE.  */
      last_addr = force_operand (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
						 stack_pointer_rtx,
						 rounded_size_op), NULL_RTX);


      /* Step 3: the loop

	 while (SP != LAST_ADDR)
	   {
	     SP = SP + PROBE_INTERVAL
	     probe at SP
	   }

	 adjusts SP and probes at PROBE_INTERVAL + N * PROBE_INTERVAL for
	 values of N from 1 until it is equal to ROUNDED_SIZE.  */

      emit_label (loop_lab);

      /* Jump to END_LAB if SP == LAST_ADDR.  */
      emit_cmp_and_jump_insns (stack_pointer_rtx, last_addr, EQ, NULL_RTX,
			       Pmode, 1, end_lab);

      /* SP = SP + PROBE_INTERVAL and probe at SP.  */
      anti_adjust_stack (GEN_INT (PROBE_INTERVAL));
      emit_stack_probe (stack_pointer_rtx);

      emit_jump (loop_lab);

      emit_label (end_lab);


      /* Step 4: adjust SP and probe at PROBE_INTERVAL + SIZE if we cannot
	 assert at compile-time that SIZE is equal to ROUNDED_SIZE.  */

      /* TEMP = SIZE - ROUNDED_SIZE.  */
      temp = simplify_gen_binary (MINUS, Pmode, size, rounded_size);
      if (temp != const0_rtx)
	{
	  /* Manual CSE if the difference is not known at compile-time.  */
	  if (GET_CODE (temp) != CONST_INT)
	    temp = gen_rtx_MINUS (Pmode, size, rounded_size_op);
	  anti_adjust_stack (temp);
	  emit_stack_probe (stack_pointer_rtx);
	}
    }

  /* Adjust back and account for the additional first interval.  */
  if (adjust_back)
    adjust_stack (plus_constant (Pmode, size, PROBE_INTERVAL + dope));
  else
    adjust_stack (GEN_INT (PROBE_INTERVAL + dope));
}

/* Return an rtx representing the register or memory location
   in which a scalar value of data type VALTYPE
   was returned by a function call to function FUNC.
   FUNC is a FUNCTION_DECL, FNTYPE a FUNCTION_TYPE node if the precise
   function is known, otherwise 0.
   OUTGOING is 1 if on a machine with register windows this function
   should return the register in which the function will put its result
   and 0 otherwise.  */

rtx
hard_function_value (const_tree valtype, const_tree func, const_tree fntype,
		     int outgoing ATTRIBUTE_UNUSED)
{
  rtx val;

  val = targetm.calls.function_value (valtype, func ? func : fntype, outgoing);

  if (REG_P (val)
      && GET_MODE (val) == BLKmode)
    {
      unsigned HOST_WIDE_INT bytes = arg_int_size_in_bytes (valtype);
      opt_scalar_int_mode tmpmode;

      /* int_size_in_bytes can return -1.  We don't need a check here
	 since the value of bytes will then be large enough that no
	 mode will match anyway.  */

      FOR_EACH_MODE_IN_CLASS (tmpmode, MODE_INT)
	{
	  /* Have we found a large enough mode?  */
	  if (GET_MODE_SIZE (tmpmode.require ()) >= bytes)
	    break;
	}

      PUT_MODE (val, tmpmode.require ());
    }
  return val;
}

/* Return an rtx representing the register or memory location
   in which a scalar value of mode MODE was returned by a library call.  */

rtx
hard_libcall_value (machine_mode mode, rtx fun)
{
  return targetm.calls.libcall_value (mode, fun);
}

/* Look up the tree code for a given rtx code
   to provide the arithmetic operation for real_arithmetic.
   The function returns an int because the caller may not know
   what `enum tree_code' means.  */

int
rtx_to_tree_code (enum rtx_code code)
{
  enum tree_code tcode;

  switch (code)
    {
    case PLUS:
      tcode = PLUS_EXPR;
      break;
    case MINUS:
      tcode = MINUS_EXPR;
      break;
    case MULT:
      tcode = MULT_EXPR;
      break;
    case DIV:
      tcode = RDIV_EXPR;
      break;
    case SMIN:
      tcode = MIN_EXPR;
      break;
    case SMAX:
      tcode = MAX_EXPR;
      break;
    default:
      tcode = LAST_AND_UNUSED_TREE_CODE;
      break;
    }
  return ((int) tcode);
}

#include "gt-explow.h"
