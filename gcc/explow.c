/* Subroutines for manipulating rtx's in semantically interesting ways.
   Copyright (C) 1987, 1991, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "toplev.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "ggc.h"
#include "recog.h"
#include "langhooks.h"

static rtx break_out_memory_refs	PARAMS ((rtx));
static void emit_stack_probe		PARAMS ((rtx));


/* Truncate and perhaps sign-extend C as appropriate for MODE.  */

HOST_WIDE_INT
trunc_int_for_mode (c, mode)
     HOST_WIDE_INT c;
     enum machine_mode mode;
{
  int width = GET_MODE_BITSIZE (mode);

  /* You want to truncate to a _what_?  */
  if (! SCALAR_INT_MODE_P (mode))
    abort ();

  /* Canonicalize BImode to 0 and STORE_FLAG_VALUE.  */
  if (mode == BImode)
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

/* Return an rtx for the sum of X and the integer C.

   This function should be used via the `plus_constant' macro.  */

rtx
plus_constant_wide (x, c)
     rtx x;
     HOST_WIDE_INT c;
{
  RTX_CODE code;
  rtx y;
  enum machine_mode mode;
  rtx tem;
  int all_constant = 0;

  if (c == 0)
    return x;

 restart:

  code = GET_CODE (x);
  mode = GET_MODE (x);
  y = x;

  switch (code)
    {
    case CONST_INT:
      return GEN_INT (INTVAL (x) + c);

    case CONST_DOUBLE:
      {
	unsigned HOST_WIDE_INT l1 = CONST_DOUBLE_LOW (x);
	HOST_WIDE_INT h1 = CONST_DOUBLE_HIGH (x);
	unsigned HOST_WIDE_INT l2 = c;
	HOST_WIDE_INT h2 = c < 0 ? ~0 : 0;
	unsigned HOST_WIDE_INT lv;
	HOST_WIDE_INT hv;

	add_double (l1, h1, l2, h2, &lv, &hv);

	return immed_double_const (lv, hv, VOIDmode);
      }

    case MEM:
      /* If this is a reference to the constant pool, try replacing it with
	 a reference to a new constant.  If the resulting address isn't
	 valid, don't return it because we have no way to validize it.  */
      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))
	{
	  tem
	    = force_const_mem (GET_MODE (x),
			       plus_constant (get_pool_constant (XEXP (x, 0)),
					      c));
	  if (memory_address_p (GET_MODE (tem), XEXP (tem, 0)))
	    return tem;
	}
      break;

    case CONST:
      /* If adding to something entirely constant, set a flag
	 so that we can add a CONST around the result.  */
      x = XEXP (x, 0);
      all_constant = 1;
      goto restart;

    case SYMBOL_REF:
    case LABEL_REF:
      all_constant = 1;
      break;

    case PLUS:
      /* The interesting case is adding the integer to a sum.
	 Look for constant term in the sum and combine
	 with C.  For an integer constant term, we make a combined
	 integer.  For a constant term that is not an explicit integer,
	 we cannot really combine, but group them together anyway.

	 Restart or use a recursive call in case the remaining operand is
	 something that we handle specially, such as a SYMBOL_REF.

	 We may not immediately return from the recursive call here, lest
	 all_constant gets lost.  */

      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  c += INTVAL (XEXP (x, 1));

	  if (GET_MODE (x) != VOIDmode)
	    c = trunc_int_for_mode (c, GET_MODE (x));

	  x = XEXP (x, 0);
	  goto restart;
	}
      else if (CONSTANT_P (XEXP (x, 1)))
	{
	  x = gen_rtx_PLUS (mode, XEXP (x, 0), plus_constant (XEXP (x, 1), c));
	  c = 0;
	}
      else if (find_constant_term_loc (&y))
	{
	  /* We need to be careful since X may be shared and we can't
	     modify it in place.  */
	  rtx copy = copy_rtx (x);
	  rtx *const_loc = find_constant_term_loc (&copy);

	  *const_loc = plus_constant (*const_loc, c);
	  x = copy;
	  c = 0;
	}
      break;

    default:
      break;
    }

  if (c != 0)
    x = gen_rtx_PLUS (mode, x, GEN_INT (c));

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
eliminate_constant_term (x, constptr)
     rtx x;
     rtx *constptr;
{
  rtx x0, x1;
  rtx tem;

  if (GET_CODE (x) != PLUS)
    return x;

  /* First handle constants appearing at this level explicitly.  */
  if (GET_CODE (XEXP (x, 1)) == CONST_INT
      && 0 != (tem = simplify_binary_operation (PLUS, GET_MODE (x), *constptr,
						XEXP (x, 1)))
      && GET_CODE (tem) == CONST_INT)
    {
      *constptr = tem;
      return eliminate_constant_term (XEXP (x, 0), constptr);
    }

  tem = const0_rtx;
  x0 = eliminate_constant_term (XEXP (x, 0), &tem);
  x1 = eliminate_constant_term (XEXP (x, 1), &tem);
  if ((x1 != XEXP (x, 1) || x0 != XEXP (x, 0))
      && 0 != (tem = simplify_binary_operation (PLUS, GET_MODE (x),
						*constptr, tem))
      && GET_CODE (tem) == CONST_INT)
    {
      *constptr = tem;
      return gen_rtx_PLUS (GET_MODE (x), x0, x1);
    }

  return x;
}

/* Returns the insn that next references REG after INSN, or 0
   if REG is clobbered before next referenced or we cannot find
   an insn that references REG in a straight-line piece of code.  */

rtx
find_next_ref (reg, insn)
     rtx reg;
     rtx insn;
{
  rtx next;

  for (insn = NEXT_INSN (insn); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (GET_CODE (insn) == NOTE)
	continue;
      if (GET_CODE (insn) == CODE_LABEL
	  || GET_CODE (insn) == BARRIER)
	return 0;
      if (GET_CODE (insn) == INSN
	  || GET_CODE (insn) == JUMP_INSN
	  || GET_CODE (insn) == CALL_INSN)
	{
	  if (reg_set_p (reg, insn))
	    return 0;
	  if (reg_mentioned_p (reg, PATTERN (insn)))
	    return insn;
	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      if (any_uncondjump_p (insn))
		next = JUMP_LABEL (insn);
	      else
		return 0;
	    }
	  if (GET_CODE (insn) == CALL_INSN
	      && REGNO (reg) < FIRST_PSEUDO_REGISTER
	      && call_used_regs[REGNO (reg)])
	    return 0;
	}
      else
	abort ();
    }
  return 0;
}

/* Return an rtx for the size in bytes of the value of EXP.  */

rtx
expr_size (exp)
     tree exp;
{
  tree size = (*lang_hooks.expr_size) (exp);

  if (TREE_CODE (size) != INTEGER_CST
      && contains_placeholder_p (size))
    size = build (WITH_RECORD_EXPR, sizetype, size, exp);

  return expand_expr (size, NULL_RTX, TYPE_MODE (sizetype), 0);
}

/* Return a wide integer for the size in bytes of the value of EXP, or -1
   if the size can vary or is larger than an integer.  */

HOST_WIDE_INT
int_expr_size (exp)
     tree exp;
{
  tree t = (*lang_hooks.expr_size) (exp);

  if (t == 0
      || TREE_CODE (t) != INTEGER_CST
      || TREE_OVERFLOW (t)
      || TREE_INT_CST_HIGH (t) != 0
      /* If the result would appear negative, it's too big to represent.  */
      || (HOST_WIDE_INT) TREE_INT_CST_LOW (t) < 0)
    return -1;

  return TREE_INT_CST_LOW (t);
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
break_out_memory_refs (x)
     rtx x;
{
  if (GET_CODE (x) == MEM
      || (CONSTANT_P (x) && CONSTANT_ADDRESS_P (x)
	  && GET_MODE (x) != VOIDmode))
    x = force_reg (GET_MODE (x), x);
  else if (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	   || GET_CODE (x) == MULT)
    {
      rtx op0 = break_out_memory_refs (XEXP (x, 0));
      rtx op1 = break_out_memory_refs (XEXP (x, 1));

      if (op0 != XEXP (x, 0) || op1 != XEXP (x, 1))
	x = gen_rtx_fmt_ee (GET_CODE (x), Pmode, op0, op1);
    }

  return x;
}

#ifdef POINTERS_EXTEND_UNSIGNED

/* Given X, a memory address in ptr_mode, convert it to an address
   in Pmode, or vice versa (TO_MODE says which way).  We take advantage of
   the fact that pointers are not allowed to overflow by commuting arithmetic
   operations over conversions so that address arithmetic insns can be
   used.  */

rtx
convert_memory_address (to_mode, x)
     enum machine_mode to_mode;
     rtx x;
{
  enum machine_mode from_mode = to_mode == ptr_mode ? Pmode : ptr_mode;
  rtx temp;
  enum rtx_code code;

  /* Here we handle some special cases.  If none of them apply, fall through
     to the default case.  */
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
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
      temp = gen_rtx_LABEL_REF (to_mode, XEXP (x, 0));
      LABEL_REF_NONLOCAL_P (temp) = LABEL_REF_NONLOCAL_P (x);
      return temp;
      break;

    case SYMBOL_REF:
      temp = shallow_copy_rtx (x);
      PUT_MODE (temp, to_mode);
      return temp;
      break;

    case CONST:
      return gen_rtx_CONST (to_mode,
			    convert_memory_address (to_mode, XEXP (x, 0)));
      break;

    case PLUS:
    case MULT:
      /* For addition we can safely permute the conversion and addition
	 operation if one operand is a constant and converting the constant
	 does not change it.  We can always safely permute them if we are
	 making the address narrower.  */
      if (GET_MODE_SIZE (to_mode) < GET_MODE_SIZE (from_mode)
	  || (GET_CODE (x) == PLUS
	      && GET_CODE (XEXP (x, 1)) == CONST_INT
	      && XEXP (x, 1) == convert_memory_address (to_mode, XEXP (x, 1))))
	return gen_rtx_fmt_ee (GET_CODE (x), to_mode,
			       convert_memory_address (to_mode, XEXP (x, 0)),
			       XEXP (x, 1));
      break;

    default:
      break;
    }

  return convert_modes (to_mode, from_mode,
			x, POINTERS_EXTEND_UNSIGNED);
}
#endif

/* Given a memory address or facsimile X, construct a new address,
   currently equivalent, that is stable: future stores won't change it.

   X must be composed of constants, register and memory references
   combined with addition, subtraction and multiplication:
   in other words, just what you can get from expand_expr if sum_ok is 1.

   Works by making copies of all regs and memory locations used
   by X and combining them the same way X does.
   You could also stabilize the reference to this address
   by copying the address to a register with copy_to_reg;
   but then you wouldn't get indexed addressing in the reference.  */

rtx
copy_all_regs (x)
     rtx x;
{
  if (GET_CODE (x) == REG)
    {
      if (REGNO (x) != FRAME_POINTER_REGNUM
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
	  && REGNO (x) != HARD_FRAME_POINTER_REGNUM
#endif
	  )
	x = copy_to_reg (x);
    }
  else if (GET_CODE (x) == MEM)
    x = copy_to_reg (x);
  else if (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	   || GET_CODE (x) == MULT)
    {
      rtx op0 = copy_all_regs (XEXP (x, 0));
      rtx op1 = copy_all_regs (XEXP (x, 1));
      if (op0 != XEXP (x, 0) || op1 != XEXP (x, 1))
	x = gen_rtx_fmt_ee (GET_CODE (x), Pmode, op0, op1);
    }
  return x;
}

/* Return something equivalent to X but valid as a memory address
   for something of mode MODE.  When X is not itself valid, this
   works by copying X or subexpressions of it into registers.  */

rtx
memory_address (mode, x)
     enum machine_mode mode;
     rtx x;
{
  rtx oldx = x;

  if (GET_CODE (x) == ADDRESSOF)
    return x;

#ifdef POINTERS_EXTEND_UNSIGNED
  if (GET_MODE (x) != Pmode)
    x = convert_memory_address (Pmode, x);
#endif

  /* By passing constant addresses thru registers
     we get a chance to cse them.  */
  if (! cse_not_expected && CONSTANT_P (x) && CONSTANT_ADDRESS_P (x))
    x = force_reg (Pmode, x);

  /* Accept a QUEUED that refers to a REG
     even though that isn't a valid address.
     On attempting to put this in an insn we will call protect_from_queue
     which will turn it into a REG, which is valid.  */
  else if (GET_CODE (x) == QUEUED
      && GET_CODE (QUEUED_VAR (x)) == REG)
    ;

  /* We get better cse by rejecting indirect addressing at this stage.
     Let the combiner create indirect addresses where appropriate.
     For now, generate the code so that the subexpressions useful to share
     are visible.  But not if cse won't be done!  */
  else
    {
      if (! cse_not_expected && GET_CODE (x) != REG)
	x = break_out_memory_refs (x);

      /* At this point, any valid address is accepted.  */
      GO_IF_LEGITIMATE_ADDRESS (mode, x, win);

      /* If it was valid before but breaking out memory refs invalidated it,
	 use it the old way.  */
      if (memory_address_p (mode, oldx))
	goto win2;

      /* Perform machine-dependent transformations on X
	 in certain cases.  This is not necessary since the code
	 below can handle all possible cases, but machine-dependent
	 transformations can make better code.  */
      LEGITIMIZE_ADDRESS (x, oldx, mode, win);

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
	      || ! memory_address_p (mode, y))
	    x = force_operand (x, NULL_RTX);
	  else
	    {
	      y = gen_rtx_PLUS (GET_MODE (x), copy_to_reg (y), constant_term);
	      if (! memory_address_p (mode, y))
		x = force_operand (x, NULL_RTX);
	      else
		x = y;
	    }
	}

      else if (GET_CODE (x) == MULT || GET_CODE (x) == MINUS)
	x = force_operand (x, NULL_RTX);

      /* If we have a register that's an invalid address,
	 it must be a hard reg of the wrong class.  Copy it to a pseudo.  */
      else if (GET_CODE (x) == REG)
	x = copy_to_reg (x);

      /* Last resort: copy the value to a register, since
	 the register is a valid address.  */
      else
	x = force_reg (Pmode, x);

      goto done;

    win2:
      x = oldx;
    win:
      if (flag_force_addr && ! cse_not_expected && GET_CODE (x) != REG
	  /* Don't copy an addr via a reg if it is one of our stack slots.  */
	  && ! (GET_CODE (x) == PLUS
		&& (XEXP (x, 0) == virtual_stack_vars_rtx
		    || XEXP (x, 0) == virtual_incoming_args_rtx)))
	{
	  if (general_operand (x, Pmode))
	    x = force_reg (Pmode, x);
	  else
	    x = force_operand (x, NULL_RTX);
	}
    }

 done:

  /* If we didn't change the address, we are done.  Otherwise, mark
     a reg as a pointer if we have REG or REG + CONST_INT.  */
  if (oldx == x)
    return x;
  else if (GET_CODE (x) == REG)
    mark_reg_pointer (x, BITS_PER_UNIT);
  else if (GET_CODE (x) == PLUS
	   && GET_CODE (XEXP (x, 0)) == REG
	   && GET_CODE (XEXP (x, 1)) == CONST_INT)
    mark_reg_pointer (XEXP (x, 0), BITS_PER_UNIT);

  /* OLDX may have been the address on a temporary.  Update the address
     to indicate that X is now used.  */
  update_temp_slot_address (oldx, x);

  return x;
}

/* Like `memory_address' but pretend `flag_force_addr' is 0.  */

rtx
memory_address_noforce (mode, x)
     enum machine_mode mode;
     rtx x;
{
  int ambient_force_addr = flag_force_addr;
  rtx val;

  flag_force_addr = 0;
  val = memory_address (mode, x);
  flag_force_addr = ambient_force_addr;
  return val;
}

/* Convert a mem ref into one with a valid memory address.
   Pass through anything else unchanged.  */

rtx
validize_mem (ref)
     rtx ref;
{
  if (GET_CODE (ref) != MEM)
    return ref;
  if (! (flag_force_addr && CONSTANT_ADDRESS_P (XEXP (ref, 0)))
      && memory_address_p (GET_MODE (ref), XEXP (ref, 0)))
    return ref;

  /* Don't alter REF itself, since that is probably a stack slot.  */
  return replace_equiv_address (ref, XEXP (ref, 0));
}

/* Given REF, either a MEM or a REG, and T, either the type of X or
   the expression corresponding to REF, set RTX_UNCHANGING_P if
   appropriate.  */

void
maybe_set_unchanging (ref, t)
     rtx ref;
     tree t;
{
  /* We can set RTX_UNCHANGING_P from TREE_READONLY for decls whose
     initialization is only executed once, or whose initializer always
     has the same value.  Currently we simplify this to PARM_DECLs in the
     first case, and decls with TREE_CONSTANT initializers in the second.

     We cannot do this for non-static aggregates, because of the double
     writes that can be generated by store_constructor, depending on the
     contents of the initializer.  Yes, this does eliminate a good fraction
     of the number of uses of RTX_UNCHANGING_P for a language like Ada.
     It also eliminates a good quantity of bugs.  Let this be incentive to
     eliminate RTX_UNCHANGING_P entirely in favour of a more reliable
     solution, perhaps based on alias sets.  */

  if ((TREE_READONLY (t) && DECL_P (t)
       && (TREE_STATIC (t) || ! AGGREGATE_TYPE_P (TREE_TYPE (t)))
       && (TREE_CODE (t) == PARM_DECL
	   || DECL_INITIAL (t) == NULL_TREE
	   || TREE_CONSTANT (DECL_INITIAL (t))))
      || TREE_CODE_CLASS (TREE_CODE (t)) == 'c')
    RTX_UNCHANGING_P (ref) = 1;
}

/* Return a modified copy of X with its memory address copied
   into a temporary register to protect it from side effects.
   If X is not a MEM, it is returned unchanged (and not copied).
   Perhaps even if it is a MEM, if there is no need to change it.  */

rtx
stabilize (x)
     rtx x;
{

  if (GET_CODE (x) != MEM
      || ! rtx_unstable_p (XEXP (x, 0)))
    return x;

  return
    replace_equiv_address (x, force_reg (Pmode, copy_all_regs (XEXP (x, 0))));
}

/* Copy the value or contents of X to a new temp reg and return that reg.  */

rtx
copy_to_reg (x)
     rtx x;
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
copy_addr_to_reg (x)
     rtx x;
{
  return copy_to_mode_reg (Pmode, x);
}

/* Like copy_to_reg but always give the new register mode MODE
   in case X is a constant.  */

rtx
copy_to_mode_reg (mode, x)
     enum machine_mode mode;
     rtx x;
{
  rtx temp = gen_reg_rtx (mode);

  /* If not an operand, must be an address with PLUS and MULT so
     do the computation.  */
  if (! general_operand (x, VOIDmode))
    x = force_operand (x, temp);

  if (GET_MODE (x) != mode && GET_MODE (x) != VOIDmode)
    abort ();
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
force_reg (mode, x)
     enum machine_mode mode;
     rtx x;
{
  rtx temp, insn, set;

  if (GET_CODE (x) == REG)
    return x;

  if (general_operand (x, mode))
    {
      temp = gen_reg_rtx (mode);
      insn = emit_move_insn (temp, x);
    }
  else
    {
      temp = force_operand (x, NULL_RTX);
      if (GET_CODE (temp) == REG)
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
      && SET_DEST (set) == temp)
    set_unique_reg_note (insn, REG_EQUAL, x);

  return temp;
}

/* If X is a memory ref, copy its contents to a new temp reg and return
   that reg.  Otherwise, return X.  */

rtx
force_not_mem (x)
     rtx x;
{
  rtx temp;

  if (GET_CODE (x) != MEM || GET_MODE (x) == BLKmode)
    return x;

  temp = gen_reg_rtx (GET_MODE (x));
  emit_move_insn (temp, x);
  return temp;
}

/* Copy X to TARGET (if it's nonzero and a reg)
   or to a new temp reg and return that reg.
   MODE is the mode to use for X in case it is a constant.  */

rtx
copy_to_suggested_reg (x, target, mode)
     rtx x, target;
     enum machine_mode mode;
{
  rtx temp;

  if (target && GET_CODE (target) == REG)
    temp = target;
  else
    temp = gen_reg_rtx (mode);

  emit_move_insn (temp, x);
  return temp;
}

/* Return the mode to use to store a scalar of TYPE and MODE.
   PUNSIGNEDP points to the signedness of the type and may be adjusted
   to show what signedness to use on extension operations.

   FOR_CALL is nonzero if this call is promoting args for a call.  */

enum machine_mode
promote_mode (type, mode, punsignedp, for_call)
     tree type;
     enum machine_mode mode;
     int *punsignedp;
     int for_call ATTRIBUTE_UNUSED;
{
  enum tree_code code = TREE_CODE (type);
  int unsignedp = *punsignedp;

#ifdef PROMOTE_FOR_CALL_ONLY
  if (! for_call)
    return mode;
#endif

  switch (code)
    {
#ifdef PROMOTE_MODE
    case INTEGER_TYPE:   case ENUMERAL_TYPE:   case BOOLEAN_TYPE:
    case CHAR_TYPE:      case REAL_TYPE:       case OFFSET_TYPE:
      PROMOTE_MODE (mode, unsignedp, type);
      break;
#endif

#ifdef POINTERS_EXTEND_UNSIGNED
    case REFERENCE_TYPE:
    case POINTER_TYPE:
      mode = Pmode;
      unsignedp = POINTERS_EXTEND_UNSIGNED;
      break;
#endif

    default:
      break;
    }

  *punsignedp = unsignedp;
  return mode;
}

/* Adjust the stack pointer by ADJUST (an rtx for a number of bytes).
   This pops when ADJUST is positive.  ADJUST need not be constant.  */

void
adjust_stack (adjust)
     rtx adjust;
{
  rtx temp;
  adjust = protect_from_queue (adjust, 0);

  if (adjust == const0_rtx)
    return;

  /* We expect all variable sized adjustments to be multiple of
     PREFERRED_STACK_BOUNDARY.  */
  if (GET_CODE (adjust) == CONST_INT)
    stack_pointer_delta -= INTVAL (adjust);

  temp = expand_binop (Pmode,
#ifdef STACK_GROWS_DOWNWARD
		       add_optab,
#else
		       sub_optab,
#endif
		       stack_pointer_rtx, adjust, stack_pointer_rtx, 0,
		       OPTAB_LIB_WIDEN);

  if (temp != stack_pointer_rtx)
    emit_move_insn (stack_pointer_rtx, temp);
}

/* Adjust the stack pointer by minus ADJUST (an rtx for a number of bytes).
   This pushes when ADJUST is positive.  ADJUST need not be constant.  */

void
anti_adjust_stack (adjust)
     rtx adjust;
{
  rtx temp;
  adjust = protect_from_queue (adjust, 0);

  if (adjust == const0_rtx)
    return;

  /* We expect all variable sized adjustments to be multiple of
     PREFERRED_STACK_BOUNDARY.  */
  if (GET_CODE (adjust) == CONST_INT)
    stack_pointer_delta += INTVAL (adjust);

  temp = expand_binop (Pmode,
#ifdef STACK_GROWS_DOWNWARD
		       sub_optab,
#else
		       add_optab,
#endif
		       stack_pointer_rtx, adjust, stack_pointer_rtx, 0,
		       OPTAB_LIB_WIDEN);

  if (temp != stack_pointer_rtx)
    emit_move_insn (stack_pointer_rtx, temp);
}

/* Round the size of a block to be pushed up to the boundary required
   by this machine.  SIZE is the desired size, which need not be constant.  */

rtx
round_push (size)
     rtx size;
{
  int align = PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT;
  if (align == 1)
    return size;
  if (GET_CODE (size) == CONST_INT)
    {
      int new = (INTVAL (size) + align - 1) / align * align;
      if (INTVAL (size) != new)
	size = GEN_INT (new);
    }
  else
    {
      /* CEIL_DIV_EXPR needs to worry about the addition overflowing,
	 but we know it can't.  So add ourselves and then do
	 TRUNC_DIV_EXPR.  */
      size = expand_binop (Pmode, add_optab, size, GEN_INT (align - 1),
			   NULL_RTX, 1, OPTAB_LIB_WIDEN);
      size = expand_divmod (0, TRUNC_DIV_EXPR, Pmode, size, GEN_INT (align),
			    NULL_RTX, 1);
      size = expand_mult (Pmode, size, GEN_INT (align), NULL_RTX, 1);
    }
  return size;
}

/* Save the stack pointer for the purpose in SAVE_LEVEL.  PSAVE is a pointer
   to a previously-created save area.  If no save area has been allocated,
   this function will allocate one.  If a save area is specified, it
   must be of the proper mode.

   The insns are emitted after insn AFTER, if nonzero, otherwise the insns
   are emitted at the current position.  */

void
emit_stack_save (save_level, psave, after)
     enum save_level save_level;
     rtx *psave;
     rtx after;
{
  rtx sa = *psave;
  /* The default is that we use a move insn and save in a Pmode object.  */
  rtx (*fcn) PARAMS ((rtx, rtx)) = gen_move_insn;
  enum machine_mode mode = STACK_SAVEAREA_MODE (save_level);

  /* See if this machine has anything special to do for this kind of save.  */
  switch (save_level)
    {
#ifdef HAVE_save_stack_block
    case SAVE_BLOCK:
      if (HAVE_save_stack_block)
	fcn = gen_save_stack_block;
      break;
#endif
#ifdef HAVE_save_stack_function
    case SAVE_FUNCTION:
      if (HAVE_save_stack_function)
	fcn = gen_save_stack_function;
      break;
#endif
#ifdef HAVE_save_stack_nonlocal
    case SAVE_NONLOCAL:
      if (HAVE_save_stack_nonlocal)
	fcn = gen_save_stack_nonlocal;
      break;
#endif
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
  else
    {
      if (mode == VOIDmode || GET_MODE (sa) != mode)
	abort ();
    }

  if (after)
    {
      rtx seq;

      start_sequence ();
      /* We must validize inside the sequence, to ensure that any instructions
	 created by the validize call also get moved to the right place.  */
      if (sa != 0)
	sa = validize_mem (sa);
      emit_insn (fcn (sa, stack_pointer_rtx));
      seq = get_insns ();
      end_sequence ();
      emit_insn_after (seq, after);
    }
  else
    {
      if (sa != 0)
	sa = validize_mem (sa);
      emit_insn (fcn (sa, stack_pointer_rtx));
    }
}

/* Restore the stack pointer for the purpose in SAVE_LEVEL.  SA is the save
   area made by emit_stack_save.  If it is zero, we have nothing to do.

   Put any emitted insns after insn AFTER, if nonzero, otherwise at
   current position.  */

void
emit_stack_restore (save_level, sa, after)
     enum save_level save_level;
     rtx after;
     rtx sa;
{
  /* The default is that we use a move insn.  */
  rtx (*fcn) PARAMS ((rtx, rtx)) = gen_move_insn;

  /* See if this machine has anything special to do for this kind of save.  */
  switch (save_level)
    {
#ifdef HAVE_restore_stack_block
    case SAVE_BLOCK:
      if (HAVE_restore_stack_block)
	fcn = gen_restore_stack_block;
      break;
#endif
#ifdef HAVE_restore_stack_function
    case SAVE_FUNCTION:
      if (HAVE_restore_stack_function)
	fcn = gen_restore_stack_function;
      break;
#endif
#ifdef HAVE_restore_stack_nonlocal
    case SAVE_NONLOCAL:
      if (HAVE_restore_stack_nonlocal)
	fcn = gen_restore_stack_nonlocal;
      break;
#endif
    default:
      break;
    }

  if (sa != 0)
    {
      sa = validize_mem (sa);
      /* These clobbers prevent the scheduler from moving
	 references to variable arrays below the code
	 that deletes (pops) the arrays.  */
      emit_insn (gen_rtx_CLOBBER (VOIDmode,
		    gen_rtx_MEM (BLKmode, 
			gen_rtx_SCRATCH (VOIDmode))));
      emit_insn (gen_rtx_CLOBBER (VOIDmode,
		    gen_rtx_MEM (BLKmode, stack_pointer_rtx)));
    }

  if (after)
    {
      rtx seq;

      start_sequence ();
      emit_insn (fcn (stack_pointer_rtx, sa));
      seq = get_insns ();
      end_sequence ();
      emit_insn_after (seq, after);
    }
  else
    emit_insn (fcn (stack_pointer_rtx, sa));
}

#ifdef SETJMP_VIA_SAVE_AREA
/* Optimize RTL generated by allocate_dynamic_stack_space for targets
   where SETJMP_VIA_SAVE_AREA is true.  The problem is that on these
   platforms, the dynamic stack space used can corrupt the original
   frame, thus causing a crash if a longjmp unwinds to it.  */

void
optimize_save_area_alloca (insns)
     rtx insns;
{
  rtx insn;

  for (insn = insns; insn; insn = NEXT_INSN(insn))
    {
      rtx note;

      if (GET_CODE (insn) != INSN)
	continue;

      for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	{
	  if (REG_NOTE_KIND (note) != REG_SAVE_AREA)
	    continue;

	  if (!current_function_calls_setjmp)
	    {
	      rtx pat = PATTERN (insn);

	      /* If we do not see the note in a pattern matching
		 these precise characteristics, we did something
		 entirely wrong in allocate_dynamic_stack_space.

		 Note, one way this could happen is if SETJMP_VIA_SAVE_AREA
		 was defined on a machine where stacks grow towards higher
		 addresses.

		 Right now only supported port with stack that grow upward
		 is the HPPA and it does not define SETJMP_VIA_SAVE_AREA.  */
	      if (GET_CODE (pat) != SET
		  || SET_DEST (pat) != stack_pointer_rtx
		  || GET_CODE (SET_SRC (pat)) != MINUS
		  || XEXP (SET_SRC (pat), 0) != stack_pointer_rtx)
		abort ();

	      /* This will now be transformed into a (set REG REG)
		 so we can just blow away all the other notes.  */
	      XEXP (SET_SRC (pat), 1) = XEXP (note, 0);
	      REG_NOTES (insn) = NULL_RTX;
	    }
	  else
	    {
	      /* setjmp was called, we must remove the REG_SAVE_AREA
		 note so that later passes do not get confused by its
		 presence.  */
	      if (note == REG_NOTES (insn))
		{
		  REG_NOTES (insn) = XEXP (note, 1);
		}
	      else
		{
		  rtx srch;

		  for (srch = REG_NOTES (insn); srch; srch = XEXP (srch, 1))
		    if (XEXP (srch, 1) == note)
		      break;

		  if (srch == NULL_RTX)
		    abort ();

		  XEXP (srch, 1) = XEXP (note, 1);
		}
	    }
	  /* Once we've seen the note of interest, we need not look at
	     the rest of them.  */
	  break;
	}
    }
}
#endif /* SETJMP_VIA_SAVE_AREA */

/* Return an rtx representing the address of an area of memory dynamically
   pushed on the stack.  This region of memory is always aligned to
   a multiple of BIGGEST_ALIGNMENT.

   Any required stack pointer alignment is preserved.

   SIZE is an rtx representing the size of the area.
   TARGET is a place in which the address can be placed.

   KNOWN_ALIGN is the alignment (in bits) that we know SIZE has.  */

rtx
allocate_dynamic_stack_space (size, target, known_align)
     rtx size;
     rtx target;
     int known_align;
{
#ifdef SETJMP_VIA_SAVE_AREA
  rtx setjmpless_size = NULL_RTX;
#endif

  /* If we're asking for zero bytes, it doesn't matter what we point
     to since we can't dereference it.  But return a reasonable
     address anyway.  */
  if (size == const0_rtx)
    return virtual_stack_dynamic_rtx;

  /* Otherwise, show we're calling alloca or equivalent.  */
  current_function_calls_alloca = 1;

  /* Ensure the size is in the proper mode.  */
  if (GET_MODE (size) != VOIDmode && GET_MODE (size) != Pmode)
    size = convert_to_mode (Pmode, size, 1);

  /* We can't attempt to minimize alignment necessary, because we don't
     know the final value of preferred_stack_boundary yet while executing
     this code.  */
  cfun->preferred_stack_boundary = PREFERRED_STACK_BOUNDARY;

  /* We will need to ensure that the address we return is aligned to
     BIGGEST_ALIGNMENT.  If STACK_DYNAMIC_OFFSET is defined, we don't
     always know its final value at this point in the compilation (it
     might depend on the size of the outgoing parameter lists, for
     example), so we must align the value to be returned in that case.
     (Note that STACK_DYNAMIC_OFFSET will have a default nonzero value if
     STACK_POINTER_OFFSET or ACCUMULATE_OUTGOING_ARGS are defined).
     We must also do an alignment operation on the returned value if
     the stack pointer alignment is less strict that BIGGEST_ALIGNMENT.

     If we have to align, we must leave space in SIZE for the hole
     that might result from the alignment operation.  */

#if defined (STACK_DYNAMIC_OFFSET) || defined (STACK_POINTER_OFFSET)
#define MUST_ALIGN 1
#else
#define MUST_ALIGN (PREFERRED_STACK_BOUNDARY < BIGGEST_ALIGNMENT)
#endif

  if (MUST_ALIGN)
    size
      = force_operand (plus_constant (size,
				      BIGGEST_ALIGNMENT / BITS_PER_UNIT - 1),
		       NULL_RTX);

#ifdef SETJMP_VIA_SAVE_AREA
  /* If setjmp restores regs from a save area in the stack frame,
     avoid clobbering the reg save area.  Note that the offset of
     virtual_incoming_args_rtx includes the preallocated stack args space.
     It would be no problem to clobber that, but it's on the wrong side
     of the old save area.  */
  {
    rtx dynamic_offset
      = expand_binop (Pmode, sub_optab, virtual_stack_dynamic_rtx,
		      stack_pointer_rtx, NULL_RTX, 1, OPTAB_LIB_WIDEN);

    if (!current_function_calls_setjmp)
      {
	int align = PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT;

	/* See optimize_save_area_alloca to understand what is being
	   set up here.  */

	/* ??? Code below assumes that the save area needs maximal
	   alignment.  This constraint may be too strong.  */
	if (PREFERRED_STACK_BOUNDARY != BIGGEST_ALIGNMENT)
	  abort ();

	if (GET_CODE (size) == CONST_INT)
	  {
	    HOST_WIDE_INT new = INTVAL (size) / align * align;

	    if (INTVAL (size) != new)
	      setjmpless_size = GEN_INT (new);
	    else
	      setjmpless_size = size;
	  }
	else
	  {
	    /* Since we know overflow is not possible, we avoid using
	       CEIL_DIV_EXPR and use TRUNC_DIV_EXPR instead.  */
	    setjmpless_size = expand_divmod (0, TRUNC_DIV_EXPR, Pmode, size,
					     GEN_INT (align), NULL_RTX, 1);
	    setjmpless_size = expand_mult (Pmode, setjmpless_size,
					   GEN_INT (align), NULL_RTX, 1);
	  }
	/* Our optimization works based upon being able to perform a simple
	   transformation of this RTL into a (set REG REG) so make sure things
	   did in fact end up in a REG.  */
	if (!register_operand (setjmpless_size, Pmode))
	  setjmpless_size = force_reg (Pmode, setjmpless_size);
      }

    size = expand_binop (Pmode, add_optab, size, dynamic_offset,
			 NULL_RTX, 1, OPTAB_LIB_WIDEN);
  }
#endif /* SETJMP_VIA_SAVE_AREA */

  /* Round the size to a multiple of the required stack alignment.
     Since the stack if presumed to be rounded before this allocation,
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

  /* If we added a variable amount to SIZE,
     we can no longer assume it is aligned.  */
#if !defined (SETJMP_VIA_SAVE_AREA)
  if (MUST_ALIGN || known_align % PREFERRED_STACK_BOUNDARY != 0)
#endif
    size = round_push (size);

  do_pending_stack_adjust ();

 /* We ought to be called always on the toplevel and stack ought to be aligned
    properly.  */
  if (stack_pointer_delta % (PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT))
    abort ();

  /* If needed, check that we have the required amount of stack.  Take into
     account what has already been checked.  */
  if (flag_stack_check && ! STACK_CHECK_BUILTIN)
    probe_stack_range (STACK_CHECK_MAX_FRAME_SIZE + STACK_CHECK_PROTECT, size);

  /* Don't use a TARGET that isn't a pseudo or is the wrong mode.  */
  if (target == 0 || GET_CODE (target) != REG
      || REGNO (target) < FIRST_PSEUDO_REGISTER
      || GET_MODE (target) != Pmode)
    target = gen_reg_rtx (Pmode);

  mark_reg_pointer (target, known_align);

  /* Perform the required allocation from the stack.  Some systems do
     this differently than simply incrementing/decrementing from the
     stack pointer, such as acquiring the space by calling malloc().  */
#ifdef HAVE_allocate_stack
  if (HAVE_allocate_stack)
    {
      enum machine_mode mode = STACK_SIZE_MODE;
      insn_operand_predicate_fn pred;

      /* We don't have to check against the predicate for operand 0 since
	 TARGET is known to be a pseudo of the proper mode, which must
	 be valid for the operand.  For operand 1, convert to the
	 proper mode and validate.  */
      if (mode == VOIDmode)
	mode = insn_data[(int) CODE_FOR_allocate_stack].operand[1].mode;

      pred = insn_data[(int) CODE_FOR_allocate_stack].operand[1].predicate;
      if (pred && ! ((*pred) (size, mode)))
	size = copy_to_mode_reg (mode, size);

      emit_insn (gen_allocate_stack (target, size));
    }
  else
#endif
    {
#ifndef STACK_GROWS_DOWNWARD
      emit_move_insn (target, virtual_stack_dynamic_rtx);
#endif

      /* Check stack bounds if necessary.  */
      if (current_function_limit_stack)
	{
	  rtx available;
	  rtx space_available = gen_label_rtx ();
#ifdef STACK_GROWS_DOWNWARD
	  available = expand_binop (Pmode, sub_optab,
				    stack_pointer_rtx, stack_limit_rtx,
				    NULL_RTX, 1, OPTAB_WIDEN);
#else
	  available = expand_binop (Pmode, sub_optab,
				    stack_limit_rtx, stack_pointer_rtx,
				    NULL_RTX, 1, OPTAB_WIDEN);
#endif
	  emit_cmp_and_jump_insns (available, size, GEU, NULL_RTX, Pmode, 1,
				   space_available);
#ifdef HAVE_trap
	  if (HAVE_trap)
	    emit_insn (gen_trap ());
	  else
#endif
	    error ("stack limits not supported on this target");
	  emit_barrier ();
	  emit_label (space_available);
	}

      anti_adjust_stack (size);
#ifdef SETJMP_VIA_SAVE_AREA
      if (setjmpless_size != NULL_RTX)
	{
	  rtx note_target = get_last_insn ();

	  REG_NOTES (note_target)
	    = gen_rtx_EXPR_LIST (REG_SAVE_AREA, setjmpless_size,
				 REG_NOTES (note_target));
	}
#endif /* SETJMP_VIA_SAVE_AREA */

#ifdef STACK_GROWS_DOWNWARD
      emit_move_insn (target, virtual_stack_dynamic_rtx);
#endif
    }

  if (MUST_ALIGN)
    {
      /* CEIL_DIV_EXPR needs to worry about the addition overflowing,
	 but we know it can't.  So add ourselves and then do
	 TRUNC_DIV_EXPR.  */
      target = expand_binop (Pmode, add_optab, target,
			     GEN_INT (BIGGEST_ALIGNMENT / BITS_PER_UNIT - 1),
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);
      target = expand_divmod (0, TRUNC_DIV_EXPR, Pmode, target,
			      GEN_INT (BIGGEST_ALIGNMENT / BITS_PER_UNIT),
			      NULL_RTX, 1);
      target = expand_mult (Pmode, target,
			    GEN_INT (BIGGEST_ALIGNMENT / BITS_PER_UNIT),
			    NULL_RTX, 1);
    }

  /* Some systems require a particular insn to refer to the stack
     to make the pages exist.  */
#ifdef HAVE_probe
  if (HAVE_probe)
    emit_insn (gen_probe ());
#endif

  /* Record the new stack level for nonlocal gotos.  */
  if (nonlocal_goto_handler_slots != 0)
    emit_stack_save (SAVE_NONLOCAL, &nonlocal_goto_stack_level, NULL_RTX);

  return target;
}

/* A front end may want to override GCC's stack checking by providing a
   run-time routine to call to check the stack, so provide a mechanism for
   calling that routine.  */

static GTY(()) rtx stack_check_libfunc;

void
set_stack_check_libfunc (libfunc)
     rtx libfunc;
{
  stack_check_libfunc = libfunc;
}

/* Emit one stack probe at ADDRESS, an address within the stack.  */

static void
emit_stack_probe (address)
     rtx address;
{
  rtx memref = gen_rtx_MEM (word_mode, address);

  MEM_VOLATILE_P (memref) = 1;

  if (STACK_CHECK_PROBE_LOAD)
    emit_move_insn (gen_reg_rtx (word_mode), memref);
  else
    emit_move_insn (memref, const0_rtx);
}

/* Probe a range of stack addresses from FIRST to FIRST+SIZE, inclusive.
   FIRST is a constant and size is a Pmode RTX.  These are offsets from the
   current stack pointer.  STACK_GROWS_DOWNWARD says whether to add or
   subtract from the stack.  If SIZE is constant, this is done
   with a fixed number of probes.  Otherwise, we must make a loop.  */

#ifdef STACK_GROWS_DOWNWARD
#define STACK_GROW_OP MINUS
#else
#define STACK_GROW_OP PLUS
#endif

void
probe_stack_range (first, size)
     HOST_WIDE_INT first;
     rtx size;
{
  /* First ensure SIZE is Pmode.  */
  if (GET_MODE (size) != VOIDmode && GET_MODE (size) != Pmode)
    size = convert_to_mode (Pmode, size, 1);

  /* Next see if the front end has set up a function for us to call to
     check the stack.  */
  if (stack_check_libfunc != 0)
    {
      rtx addr = memory_address (QImode,
				 gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					         stack_pointer_rtx,
					         plus_constant (size, first)));

#ifdef POINTERS_EXTEND_UNSIGNED
      if (GET_MODE (addr) != ptr_mode)
	addr = convert_memory_address (ptr_mode, addr);
#endif

      emit_library_call (stack_check_libfunc, LCT_NORMAL, VOIDmode, 1, addr,
			 ptr_mode);
    }

  /* Next see if we have an insn to check the stack.  Use it if so.  */
#ifdef HAVE_check_stack
  else if (HAVE_check_stack)
    {
      insn_operand_predicate_fn pred;
      rtx last_addr
	= force_operand (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					 stack_pointer_rtx,
					 plus_constant (size, first)),
			 NULL_RTX);

      pred = insn_data[(int) CODE_FOR_check_stack].operand[0].predicate;
      if (pred && ! ((*pred) (last_addr, Pmode)))
	last_addr = copy_to_mode_reg (Pmode, last_addr);

      emit_insn (gen_check_stack (last_addr));
    }
#endif

  /* If we have to generate explicit probes, see if we have a constant
     small number of them to generate.  If so, that's the easy case.  */
  else if (GET_CODE (size) == CONST_INT
	   && INTVAL (size) < 10 * STACK_CHECK_PROBE_INTERVAL)
    {
      HOST_WIDE_INT offset;

      /* Start probing at FIRST + N * STACK_CHECK_PROBE_INTERVAL
	 for values of N from 1 until it exceeds LAST.  If only one
	 probe is needed, this will not generate any code.  Then probe
	 at LAST.  */
      for (offset = first + STACK_CHECK_PROBE_INTERVAL;
	   offset < INTVAL (size);
	   offset = offset + STACK_CHECK_PROBE_INTERVAL)
	emit_stack_probe (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					  stack_pointer_rtx,
					  GEN_INT (offset)));

      emit_stack_probe (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					stack_pointer_rtx,
					plus_constant (size, first)));
    }

  /* In the variable case, do the same as above, but in a loop.  We emit loop
     notes so that loop optimization can be done.  */
  else
    {
      rtx test_addr
	= force_operand (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					 stack_pointer_rtx,
					 GEN_INT (first + STACK_CHECK_PROBE_INTERVAL)),
			 NULL_RTX);
      rtx last_addr
	= force_operand (gen_rtx_fmt_ee (STACK_GROW_OP, Pmode,
					 stack_pointer_rtx,
					 plus_constant (size, first)),
			 NULL_RTX);
      rtx incr = GEN_INT (STACK_CHECK_PROBE_INTERVAL);
      rtx loop_lab = gen_label_rtx ();
      rtx test_lab = gen_label_rtx ();
      rtx end_lab = gen_label_rtx ();
      rtx temp;

      if (GET_CODE (test_addr) != REG
	  || REGNO (test_addr) < FIRST_PSEUDO_REGISTER)
	test_addr = force_reg (Pmode, test_addr);

      emit_note (NULL, NOTE_INSN_LOOP_BEG);
      emit_jump (test_lab);

      emit_label (loop_lab);
      emit_stack_probe (test_addr);

      emit_note (NULL, NOTE_INSN_LOOP_CONT);

#ifdef STACK_GROWS_DOWNWARD
#define CMP_OPCODE GTU
      temp = expand_binop (Pmode, sub_optab, test_addr, incr, test_addr,
			   1, OPTAB_WIDEN);
#else
#define CMP_OPCODE LTU
      temp = expand_binop (Pmode, add_optab, test_addr, incr, test_addr,
			   1, OPTAB_WIDEN);
#endif

      if (temp != test_addr)
	abort ();

      emit_label (test_lab);
      emit_cmp_and_jump_insns (test_addr, last_addr, CMP_OPCODE,
			       NULL_RTX, Pmode, 1, loop_lab);
      emit_jump (end_lab);
      emit_note (NULL, NOTE_INSN_LOOP_END);
      emit_label (end_lab);

      emit_stack_probe (last_addr);
    }
}

/* Return an rtx representing the register or memory location
   in which a scalar value of data type VALTYPE
   was returned by a function call to function FUNC.
   FUNC is a FUNCTION_DECL node if the precise function is known,
   otherwise 0.
   OUTGOING is 1 if on a machine with register windows this function
   should return the register in which the function will put its result
   and 0 otherwise.  */

rtx
hard_function_value (valtype, func, outgoing)
     tree valtype;
     tree func ATTRIBUTE_UNUSED;
     int outgoing ATTRIBUTE_UNUSED;
{
  rtx val;

#ifdef FUNCTION_OUTGOING_VALUE
  if (outgoing)
    val = FUNCTION_OUTGOING_VALUE (valtype, func);
  else
#endif
    val = FUNCTION_VALUE (valtype, func);

  if (GET_CODE (val) == REG
      && GET_MODE (val) == BLKmode)
    {
      unsigned HOST_WIDE_INT bytes = int_size_in_bytes (valtype);
      enum machine_mode tmpmode;

      /* int_size_in_bytes can return -1.  We don't need a check here
	 since the value of bytes will be large enough that no mode
	 will match and we will abort later in this function.  */

      for (tmpmode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   tmpmode != VOIDmode;
	   tmpmode = GET_MODE_WIDER_MODE (tmpmode))
	{
	  /* Have we found a large enough mode?  */
	  if (GET_MODE_SIZE (tmpmode) >= bytes)
	    break;
	}

      /* No suitable mode found.  */
      if (tmpmode == VOIDmode)
	abort ();

      PUT_MODE (val, tmpmode);
    }
  return val;
}

/* Return an rtx representing the register or memory location
   in which a scalar value of mode MODE was returned by a library call.  */

rtx
hard_libcall_value (mode)
     enum machine_mode mode;
{
  return LIBCALL_VALUE (mode);
}

/* Look up the tree code for a given rtx code
   to provide the arithmetic operation for REAL_ARITHMETIC.
   The function returns an int because the caller may not know
   what `enum tree_code' means.  */

int
rtx_to_tree_code (code)
     enum rtx_code code;
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
