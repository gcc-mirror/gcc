/* Subroutines used for code generation on the DEC Alpha.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "reload.h"
#include "expr.h"
#include "obstack.h"
#include "tree.h"

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */

rtx alpha_compare_op0, alpha_compare_op1;
int alpha_compare_fp_p;

/* Save the name of the current function as used by the assembler.  This
   is used by the epilogue.  */

char *alpha_function_name;

/* Nonzero if the current function needs gp.  */

int alpha_function_needs_gp;

/* Returns 1 if VALUE is a mask that contains full bytes of zero or ones.  */

int
zap_mask (value)
     HOST_WIDE_INT value;
{
  int i;

  for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
       i++, value >>= 8)
    if ((value & 0xff) != 0 && (value & 0xff) != 0xff)
      return 0;

  return 1;
}

/* Returns 1 if OP is either the constant zero or a register.  If a
   register, it must be in the proper mode unless MODE is VOIDmode.  */

int
reg_or_0_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  return op == const0_rtx || register_operand (op, mode);
}

/* Return 1 if OP is an 8-bit constant or any register.  */

int
reg_or_8bit_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT
	   && (unsigned HOST_WIDE_INT) INTVAL (op) < 0x100)
	  || register_operand (op, mode));
}

/* Return 1 if the operand is a valid second operand to an add insn.  */

int
add_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return ((unsigned HOST_WIDE_INT) (INTVAL (op) + 0x8000) < 0x10000
	    || ((INTVAL (op) & 0xffff) == 0
		&& (INTVAL (op) >> 31 == -1
		    || INTVAL (op) >> 31 == 0)));

  return register_operand (op, mode);
}

/* Return 1 if the operand is a valid second operand to a sign-extending
   add insn.  */

int
sext_add_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return ((unsigned HOST_WIDE_INT) INTVAL (op) < 255
	    || (unsigned HOST_WIDE_INT) (- INTVAL (op)) < 255);

  return register_operand (op, mode);
}

/* Return 1 if OP is the constant 4 or 8.  */

int
const48_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 4 || INTVAL (op) == 8));
}

/* Return 1 if OP is a valid first operand to an AND insn.  */

int
and_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == VOIDmode)
    return (zap_mask (CONST_DOUBLE_LOW (op))
	    && zap_mask (CONST_DOUBLE_HIGH (op)));

  if (GET_CODE (op) == CONST_INT)
    return ((unsigned HOST_WIDE_INT) INTVAL (op) < 0x100
	    || (unsigned HOST_WIDE_INT) ~ INTVAL (op) < 0x100
	    || zap_mask (INTVAL (op)));

  return register_operand (op, mode);
}

/* Return 1 if OP is a constant that is the width, in bits, of an integral
   mode smaller than DImode.  */

int
mode_width_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 8 || INTVAL (op) == 16 || INTVAL (op) == 32));
}

/* Return 1 if OP is a constant that is the width of an integral machine mode
   smaller than an integer.  */

int
mode_mask_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
#if HOST_BITS_PER_WIDE_INT == 32
  if (GET_CODE (op) == CONST_DOUBLE)
    return CONST_DOUBLE_HIGH (op) == 0 && CONST_DOUBLE_LOW (op) == -1;
#endif

  if (GET_CODE (op) == CONST_INT)
    return (INTVAL (op) == 0xff
	    || INTVAL (op) == 0xffff
#if HOST_BITS_PER_WIDE_INT == 64
	    || INTVAL (op) == 0xffffffff
#endif
	    );
}

/* Return 1 if OP is a multiple of 8 less than 64.  */

int
mul8_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (op) < 64
	  && (INTVAL (op) & 7) == 0);
}

/* Return 1 if OP is the constant zero in floating-point.  */

int
fp0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_MODE (op) == mode
	  && GET_MODE_CLASS (mode) == MODE_FLOAT && op == CONST0_RTX (mode));
}

/* Return 1 if OP is the floating-point constant zero or a register.  */

int
reg_or_fp0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return fp0_operand (op, mode) || register_operand (op, mode);
}

/* Return 1 if OP is a register or a constant integer.  */


int
reg_or_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return GET_CODE (op) == CONST_INT || register_operand (op, mode);
}

/* Return 1 if OP is a valid operand for the source of a move insn.  */

int
input_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return 0;

  if (GET_MODE_CLASS (mode) == MODE_FLOAT && GET_MODE (op) != mode)
    return 0;

  switch (GET_CODE (op))
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      return mode == DImode;

    case REG:
      return 1;

    case SUBREG:
      if (register_operand (op, mode))
	return 1;
      /* ... fall through ... */
    case MEM:
      return mode != HImode && mode != QImode && general_operand (op, mode);

    case CONST_DOUBLE:
      return GET_MODE_CLASS (mode) == MODE_FLOAT && op == CONST0_RTX (mode);

    case CONST_INT:
      return mode == QImode || mode == HImode || add_operand (op, mode);
    }

  return 0;
}

/* Return 1 if OP is a SYMBOL_REF for the current function.  */

int
current_function_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == SYMBOL_REF
	  && ! strcmp (XSTR (op, 0), current_function_name));
}

/* Return 1 if OP is a valid Alpha comparison operator.  Here we know which
   comparisons are valid in which insn.  */

int
alpha_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);

  if (mode != GET_MODE (op) || GET_RTX_CLASS (code) != '<')
    return 0;

  return (code == EQ || code == LE || code == LT
	  || (mode == DImode && (code == LEU || code == LTU)));
}

/* Return 1 if OP is a signed comparison operation.  */

int
signed_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case EQ:  case NE:  case LE:  case LT:  case GE:   case GT:
      return 1;
    }

  return 0;
}

/* Return 1 if this is a divide or modulus operator.  */

int
divmod_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case DIV:  case MOD:  case UDIV:  case UMOD:
      return 1;
    }

  return 0;
}

/* Return 1 if this memory address is a known aligned register plus
   a constant.  It must be a valid address.  This means that we can do
   this as an aligned reference plus some offset.

   Take into account what reload will do.

   We could say that out-of-range stack slots are alignable, but that would
   complicate get_aligned_mem and it isn't worth the trouble since few
   functions have large stack space.  */

int
aligned_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (GET_MODE (op) != mode)
	return 0;
      op = SUBREG_REG (op);
      mode = GET_MODE (op);
    }

  if (reload_in_progress && GET_CODE (op) == REG
      && REGNO (op) >= FIRST_PSEUDO_REGISTER)
    op = reg_equiv_mem[REGNO (op)];

  if (GET_CODE (op) != MEM || GET_MODE (op) != mode
      || ! memory_address_p (mode, XEXP (op, 0)))
    return 0;

  op = XEXP (op, 0);

  if (GET_CODE (op) == PLUS)
    op = XEXP (op, 0);

  return (GET_CODE (op) == REG
	  && (REGNO (op) == STACK_POINTER_REGNUM || op == frame_pointer_rtx
	      || (REGNO (op) >= FIRST_VIRTUAL_REGISTER
		  && REGNO (op) <= LAST_VIRTUAL_REGISTER)));
}

/* Similar, but return 1 if OP is a MEM which is not alignable.  */

int
unaligned_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (GET_MODE (op) != mode)
	return 0;
      op = SUBREG_REG (op);
      mode = GET_MODE (op);
    }

  if (reload_in_progress && GET_CODE (op) == REG
      && REGNO (op) >= FIRST_PSEUDO_REGISTER)
    op = reg_equiv_mem[REGNO (op)];

  if (GET_CODE (op) != MEM || GET_MODE (op) != mode)
    return 0;

  op = XEXP (op, 0);

  if (! memory_address_p (mode, op))
    return 1;

  if (GET_CODE (op) == PLUS)
    op = XEXP (op, 0);

  return (GET_CODE (op) != REG
	  || (REGNO (op) != STACK_POINTER_REGNUM && op != frame_pointer_rtx
	      && (REGNO (op) < FIRST_VIRTUAL_REGISTER
		  || REGNO (op) > LAST_VIRTUAL_REGISTER)));
}

/* Return 1 if OP is any memory location.  During reload a pseudo matches.  */

int
any_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == MEM
	  || (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
	  || (reload_in_progress && GET_CODE (op) == REG
	      && REGNO (op) >= FIRST_PSEUDO_REGISTER)
	  || (reload_in_progress && GET_CODE (op) == SUBREG
	      && GET_CODE (SUBREG_REG (op)) == REG
	      && REGNO (SUBREG_REG (op)) >= FIRST_PSEUDO_REGISTER));
}

/* REF is an alignable memory location.  Place an aligned SImode
   reference into *PALIGNED_MEM and the number of bits to shift into
   *PBITNUM.  */

void
get_aligned_mem (ref, paligned_mem, pbitnum)
     rtx ref;
     rtx *paligned_mem, *pbitnum;
{
  rtx base;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_WORD (ref) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (ref)))
		   - MIN (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (SUBREG_REG (ref)))));
      ref = SUBREG_REG (ref);
    }

  if (GET_CODE (ref) == REG)
    ref = reg_equiv_mem[REGNO (ref)];

  if (reload_in_progress)
    base = find_replacement (&XEXP (ref, 0));
  else
    base = XEXP (ref, 0);

  if (GET_CODE (base) == PLUS)
    offset += INTVAL (XEXP (base, 1)), base = XEXP (base, 0);

  *paligned_mem = gen_rtx (MEM, SImode,
			   plus_constant (base, offset & ~3));
  MEM_IN_STRUCT_P (*paligned_mem) = MEM_IN_STRUCT_P (ref);
  MEM_VOLATILE_P (*paligned_mem) = MEM_VOLATILE_P (ref);
  RTX_UNCHANGING_P (*paligned_mem) = RTX_UNCHANGING_P (ref);

  *pbitnum = GEN_INT ((offset & 3) * 8);
}

/* Similar, but just get the address.  Handle the two reload cases.  */

rtx
get_unaligned_address (ref)
     rtx ref;
{
  rtx base;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_WORD (ref) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (ref)))
		   - MIN (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (SUBREG_REG (ref)))));
      ref = SUBREG_REG (ref);
    }

  if (GET_CODE (ref) == REG)
    ref = reg_equiv_mem[REGNO (ref)];

  if (reload_in_progress)
    base = find_replacement (&XEXP (ref, 0));
  else
    base = XEXP (ref, 0);

  if (GET_CODE (base) == PLUS)
    offset += INTVAL (XEXP (base, 1)), base = XEXP (base, 0);

  return plus_constant (base, offset);
}

/* Subfunction of the following function.  Update the flags of any MEM
   found in part of X.  */

static void
alpha_set_memflags_1 (x, in_struct_p, volatile_p, unchanging_p)
     rtx x;
     int in_struct_p, volatile_p, unchanging_p;
{
  int i;

  switch (GET_CODE (x))
    {
    case SEQUENCE:
    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	alpha_set_memflags_1 (XVECEXP (x, 0, i), in_struct_p, volatile_p,
			      unchanging_p);
      break;

    case INSN:
      alpha_set_memflags_1 (PATTERN (x), in_struct_p, volatile_p,
			    unchanging_p);
      break;

    case SET:
      alpha_set_memflags_1 (SET_DEST (x), in_struct_p, volatile_p,
			    unchanging_p);
      alpha_set_memflags_1 (SET_SRC (x), in_struct_p, volatile_p,
			    unchanging_p);
      break;

    case MEM:
      MEM_IN_STRUCT_P (x) = in_struct_p;
      MEM_VOLATILE_P (x) = volatile_p;
      RTX_UNCHANGING_P (x) = unchanging_p;
      break;
    }
}

/* Given INSN, which is either an INSN or a SEQUENCE generated to
   perform a memory operation, look for any MEMs in either a SET_DEST or
   a SET_SRC and copy the in-struct, unchanging, and volatile flags from
   REF into each of the MEMs found.  If REF is not a MEM, don't do
   anything.  */

void
alpha_set_memflags (insn, ref)
     rtx insn;
     rtx ref;
{
  /* Note that it is always safe to get these flags, though they won't
     be what we think if REF is not a MEM.  */
  int in_struct_p = MEM_IN_STRUCT_P (ref);
  int volatile_p = MEM_VOLATILE_P (ref);
  int unchanging_p = RTX_UNCHANGING_P (ref);

  if (GET_CODE (ref) != MEM
      || (! in_struct_p && ! volatile_p && ! unchanging_p))
    return;

  alpha_set_memflags_1 (insn, in_struct_p, volatile_p, unchanging_p);
}

/* Try to output insns to set TARGET equal to the constant C if it can be
   done in less than N insns.  Returns 1 if it can be done and the
   insns have been emitted.  If it would take more than N insns, zero is
   returned and no insns and emitted.  */

int
alpha_emit_set_const (target, c, n)
     rtx target;
     HOST_WIDE_INT c;
     int n;
{
  HOST_WIDE_INT new = c;
  int i, bits;

#if HOST_BITS_PER_WIDE_INT == 64
  /* We are only called for SImode and DImode.  If this is SImode, ensure that
     we are sign extended to a full word.  This does not make any sense when
     cross-compiling on a narrow machine.  */

  if (GET_MODE (target) == SImode)
    c = (c & 0xffffffff) - 2 * (c & 0x80000000);
#endif

  /* If this is a sign-extended 32-bit constant, we can do this in at most
     three insns, so do it if we have enough insns left.  We always have
     a sign-extended 32-bit constant when compiling on a narrow machine.  */

  if (HOST_BITS_PER_WIDE_INT != 64
      || c >> 31 == -1 || c >> 31 == 0)
    {
      HOST_WIDE_INT low = (c & 0xffff) - 2 * (c & 0x8000);
      HOST_WIDE_INT tmp1 = c - low;
      HOST_WIDE_INT high
	= ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
      HOST_WIDE_INT tmp2 = c - (high << 16) - low;
      HOST_WIDE_INT extra = 0;

      if (tmp2)
	{
	  extra = 0x4000;
	  tmp1 -= 0x40000000;
	  high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	}

      if (c == low || (low == 0 && extra == 0))
	{
	  emit_move_insn (target, GEN_INT (c));
	  return 1;
	}
      else if (n >= 2 + (extra != 0))
	{
	  emit_move_insn (target, GEN_INT (low));
	  if (extra != 0)
	    emit_insn (gen_add2_insn (target, GEN_INT (extra << 16)));

	  emit_insn (gen_add2_insn (target, GEN_INT (high << 16)));
	  return 1;
	}
    }

  /* If we couldn't do it that way, try some other methods (that depend on
     being able to compute in the target's word size).  But if we have no
     instructions left, don't bother.  Also, don't even try if this is 
     SImode (in which case we should have already done something, but
     do a sanity check here).  */

  if (n == 1 || HOST_BITS_PER_WIDE_INT < 64 || GET_MODE (target) != DImode)
    return 0;

  /* First, see if can load a value into the target that is the same as the
     constant except that all bytes that are 0 are changed to be 0xff.  If we
     can, then we can do a ZAPNOT to obtain the desired constant.  */

  for (i = 0; i < 64; i += 8)
    if ((new & ((HOST_WIDE_INT) 0xff << i)) == 0)
      new |= (HOST_WIDE_INT) 0xff << i;

  if (alpha_emit_set_const (target, new, n - 1))
    {
      emit_insn (gen_anddi3 (target, target, GEN_INT (c | ~ new)));
      return 1;
    }

  /* Find, see if we can load a related constant and then shift and possibly
     negate it to get the constant we want.  Try this once each increasing
     numbers of insns.  */

  for (i = 1; i < n; i++)
    {
      /* First try complementing.  */
      if (alpha_emit_set_const (target, ~ c, i))
	{
	  emit_insn (gen_one_cmpldi2 (target, target));
	  return 1;
	}

      /* First try to form a constant and do a left shift.  We can do this
	 if some low-order bits are zero; the exact_log2 call below tells
	 us that information.  The bits we are shifting out could be any
	 value, but here we'll just try the 0- and sign-extended forms of
	 the constant.  To try to increase the chance of having the same
	 constant in more than one insn, start at the highest number of
	 bits to shift, but try all possibilities in case a ZAPNOT will
	 be useful.  */

      if ((bits = exact_log2 (c & - c)) > 0)
	for (; bits > 0; bits--)
	  if (alpha_emit_set_const (target, c >> bits, i)
	      || alpha_emit_set_const (target,
				       ((unsigned HOST_WIDE_INT) c) >> bits,
				       i))
	    {
	      emit_insn (gen_ashldi3 (target, target, GEN_INT (bits)));
	      return 1;
	    }

      /* Now try high-order zero bits.  Here we try the shifted-in bits as
	 all zero and all ones.  */

      if ((bits = HOST_BITS_PER_WIDE_INT - floor_log2 (c) - 1) > 0)
	for (; bits > 0; bits--)
	  if (alpha_emit_set_const (target, c << bits, i)
	      || alpha_emit_set_const (target,
				       ((c << bits)
					| (((HOST_WIDE_INT) 1 << bits) - 1)),
				       i))
	    {
	      emit_insn (gen_lshrdi3 (target, target, GEN_INT (bits)));
	      return 1;
	    }

      /* Now try high-order 1 bits.  We get that with a sign-extension.
	 But one bit isn't enough here.  */
      
      if ((bits = HOST_BITS_PER_WIDE_INT - floor_log2 (~ c) - 2) > 0)
	for (; bits > 0; bits--)
	  if (alpha_emit_set_const (target, c << bits, i)
	      || alpha_emit_set_const (target,
				       ((c << bits)
					| (((HOST_WIDE_INT) 1 << bits) - 1)),
				       i))
	    {
	      emit_insn (gen_ashrdi3 (target, target, GEN_INT (bits)));
	      return 1;
	    }
    }

  return 0;
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

int
alpha_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  rtx set;

  /* If the dependence is an anti-dependence, there is no cost.  For an
     output dependence, there is sometimes a cost, but it doesn't seem
     worth handling those few cases.  */

  if (REG_NOTE_KIND (link) != 0)
    return 0;

  /* If INSN is a store insn and DEP_INSN is setting the data being stored,
     we can sometimes lower the cost.  */

  if (recog_memoized (insn) >= 0 && get_attr_type (insn) == TYPE_ST
      && (set = single_set (dep_insn)) != 0
      && GET_CODE (PATTERN (insn)) == SET
      && rtx_equal_p (SET_DEST (set), SET_SRC (PATTERN (insn))))
    switch (get_attr_type (dep_insn))
      {
      case TYPE_LD:
	/* No savings here.  */
	return cost;

      case TYPE_IMULL:
      case TYPE_IMULQ:
	/* In these cases, we save one cycle.  */
	return cost - 2;

      default:
	/* In all other cases, we save two cycles.  */
	return MAX (0, cost - 4);
      }

  /* Another case that needs adjustment is an arithmetic or logical
     operation.  It's cost is usually one cycle, but we default it to
     two in the MD file.  The only case that it is actually two is
     for the address in loads and stores.  */

  if (recog_memoized (dep_insn) >= 0
      && get_attr_type (dep_insn) == TYPE_IADDLOG)
    switch (get_attr_type (insn))
      {
      case TYPE_LD:
      case TYPE_ST:
	return cost;

      default:
	return 2;
      }

  /* The final case is when a compare feeds into an integer branch.  The cost
     is only one cycle in that case.  */

  if (recog_memoized (dep_insn) >= 0
      && get_attr_type (dep_insn) == TYPE_ICMP
      && recog_memoized (insn) >= 0
      && get_attr_type (insn) == TYPE_IBR)
    return 2;

  /* Otherwise, return the default cost. */

  return cost;
}

/* Print an operand.  Recognize special options, documented below.  */

void
print_operand (file, x, code)
    FILE *file;
    rtx x;
    char code;
{
  int i;

  switch (code)
    {
    case 'r':
      /* If this operand is the constant zero, write it as "$31".  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fprintf (file, "$31");
      else
	output_operand_lossage ("invalid %%r value");

      break;

    case 'R':
      /* Similar, but for floating-point.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fprintf (file, "$f31");
      else
	output_operand_lossage ("invalid %%R value");

      break;

    case 'N':
      /* Write the 1's complement of a constant.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%N value");

      fprintf (file, "%ld", ~ INTVAL (x));
      break;

    case 'P':
      /* Write 1 << C, for a constant C.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%P value");

      fprintf (file, "%ld", (HOST_WIDE_INT) 1 << INTVAL (x));
      break;

    case 'h':
      /* Write the high-order 16 bits of a constant, sign-extended.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%h value");

      fprintf (file, "%ld", INTVAL (x) >> 16);
      break;

    case 'L':
      /* Write the low-order 16 bits of a constant, sign-extended.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%L value");

      fprintf (file, "%ld", (INTVAL (x) & 0xffff) - 2 * (INTVAL (x) & 0x8000));
      break;

    case 'm':
      /* Write mask for ZAP insn.  */
      if (GET_CODE (x) == CONST_DOUBLE)
	{
	  HOST_WIDE_INT mask = 0;
	  HOST_WIDE_INT value;

	  value = CONST_DOUBLE_LOW (x);
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
	       i++, value >>= 8)
	    if (value & 0xff)
	      mask |= (1 << i);

	  value = CONST_DOUBLE_HIGH (x);
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
	       i++, value >>= 8)
	    if (value & 0xff)
	      mask |= (1 << (i + sizeof (int)));

	  fprintf (file, "%ld", mask & 0xff);
	}

      else if (GET_CODE (x) == CONST_INT)
	{
	  HOST_WIDE_INT mask = 0, value = INTVAL (x);

	  for (i = 0; i < 8; i++, value >>= 8)
	    if (value & 0xff)
	      mask |= (1 << i);

	  fprintf (file, "%ld", mask);
	}
      else
	output_operand_lossage ("invalid %%m value");
      break;

    case 'M':
      /* 'b', 'w', or 'l' as the value of the constant.  */
      if (GET_CODE (x) != CONST_INT
	  || (INTVAL (x) != 8 && INTVAL (x) != 16 && INTVAL (x) != 32))
	output_operand_lossage ("invalid %%M value");

      fprintf (file, "%s",
	       INTVAL (x) == 8 ? "b" : INTVAL (x) == 16 ? "w" : "l");
      break;

    case 'U':
      /* Similar, except do it from the mask.  */
      if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xff)
	fprintf (file, "b");
      else if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xffff)
	fprintf (file, "w");
#if HOST_BITS_PER_WIDE_INT == 32
      else if (GET_CODE (x) == CONST_DOUBLE
	       && CONST_DOUBLE_HIGH (x) == 0
	       && CONST_DOUBLE_LOW (x) == -1)
	fprintf (file, "l");
#else
      else if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0xffffffff)
	fprintf (file, "l");
#endif
      else
	output_operand_lossage ("invalid %%U value");
      break;

    case 's':
      /* Write the constant value divided by 8.  */
      if (GET_CODE (x) != CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (x) >= 64
	  && (INTVAL (x) & 7) != 8)
	output_operand_lossage ("invalid %%s value");

      fprintf (file, "%ld", INTVAL (x) / 8);
      break;

    case 'S':
      /* Same, except compute (64 - c) / 8 */

      if (GET_CODE (x) != CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (x) >= 64
	  && (INTVAL (x) & 7) != 8)
	output_operand_lossage ("invalid %%s value");

      fprintf (file, "%ld", (64 - INTVAL (x)) / 8);
      break;

    case 'C':
      /* Write out comparison name.  */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%C value");

      if (GET_CODE (x) == LEU)
	fprintf (file, "ule");
      else if (GET_CODE (x) == LTU)
	fprintf (file, "ult");
      else
	fprintf (file, "%s", GET_RTX_NAME (GET_CODE (x)));
      break;

    case 'D':
      /* Similar, but write reversed code.  We can't get an unsigned code
	 here.  */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%D value");

      fprintf (file, "%s", GET_RTX_NAME (reverse_condition (GET_CODE (x))));
      break;

    case 'E':
      /* Write the divide or modulus operator.  */
      switch (GET_CODE (x))
	{
	case DIV:
	  fprintf (file, "div%s", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	case UDIV:
	  fprintf (file, "div%su", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	case MOD:
	  fprintf (file, "rem%s", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	case UMOD:
	  fprintf (file, "rem%su", GET_MODE (x) == SImode ? "l" : "q");
	  break;
	default:
	  output_operand_lossage ("invalid %%E value");
	  break;
	}
      break;

    case 'F':
      /* Write the symbol; if the current function uses GP, write a
	 modified version.  */
      if (GET_CODE (x) != SYMBOL_REF)
	output_operand_lossage ("invalid %%F value");

      output_addr_const (file, x);
      if (alpha_function_needs_gp)
	fprintf (file, "..ng");
      break;

    case 'A':
      /* Write "_u" for unaligned access.  */
      if (GET_CODE (x) == MEM && GET_CODE (XEXP (x, 0)) == AND)
	fprintf (file, "_u");
      break;

    case 0:
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (GET_CODE (x) == MEM)
	output_address (XEXP (x, 0));
      else
	output_addr_const (file, x);
      break;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

/* Do what is necessary for `va_start'.  The argument is ignored;
   We look at the current function to determine if stdarg or varargs
   is used and fill in an initial va_list.  A pointer to this constructor
   is returned.  */

struct rtx_def *
alpha_builtin_saveregs (arglist)
     tree arglist;
{
  rtx block, addr, argsize;
  tree fntype = TREE_TYPE (current_function_decl);
  int stdarg = (TYPE_ARG_TYPES (fntype) != 0
		&& (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		    != void_type_node));
  int nregs = current_function_args_info;

  /* If we have a variable-sized argument already, we will have used all
     the registers, so set up to indicate that.  */

  if (GET_CODE (current_function_arg_offset_rtx) != CONST_INT)
    {
      argsize = plus_constant (current_function_arg_offset_rtx,
			       (6 * UNITS_PER_WORD + UNITS_PER_WORD - 1));
      argsize = expand_shift (RSHIFT_EXPR, Pmode, argsize,
			      build_int_2 (3, 0), argsize, 0);
    }
  else
    {
      /* Compute the number of args in memory and number of arguments already
	 processed.  Then adjust the number of registers if this is stdarg.  */
      int memargs = ((INTVAL (current_function_arg_offset_rtx)
		      + UNITS_PER_WORD - 1)
		     / UNITS_PER_WORD);

      argsize = GEN_INT (MIN (nregs, 6) + memargs);

      if (nregs <= 6)
	nregs -= stdarg;
    }

  /* Allocate the va_list constructor */
  block = assign_stack_local (BLKmode, 4 * UNITS_PER_WORD, BITS_PER_WORD);
  RTX_UNCHANGING_P (block) = 1;
  RTX_UNCHANGING_P (XEXP (block, 0)) = 1;

  /* Store the argsize as the __va_arg member.  */
  emit_move_insn (change_address (block, DImode, XEXP (block, 0)),
		  argsize);

  /* Store the arg pointer in the __va_stack member.  */
  emit_move_insn (change_address (block, Pmode,
				  plus_constant (XEXP (block, 0),
						 UNITS_PER_WORD)),
		  virtual_incoming_args_rtx);

  /* Allocate the integer register space, and store it as the
     __va_ireg member.  */
  addr = assign_stack_local (BLKmode, 6 * UNITS_PER_WORD, -1);
  MEM_IN_STRUCT_P (addr) = 1;
  RTX_UNCHANGING_P (addr) = 1;
  RTX_UNCHANGING_P (XEXP (addr, 0)) = 1;

  emit_move_insn (change_address (block, Pmode,
				  plus_constant (XEXP (block, 0),
						 2 * UNITS_PER_WORD)),
		  copy_to_reg (XEXP (addr, 0)));

  /* Now store the incoming integer registers.  */
  if (nregs < 6)
      move_block_from_reg
	(16 + nregs,
	 change_address (addr, Pmode,
			 plus_constant (XEXP (addr, 0),
					nregs * UNITS_PER_WORD)),
	 6 - nregs);

  /* Allocate the FP register space, and store it as the
     __va_freg member.  */
  addr = assign_stack_local (BLKmode, 6 * UNITS_PER_WORD, -1);
  MEM_IN_STRUCT_P (addr) = 1;
  RTX_UNCHANGING_P (addr) = 1;
  RTX_UNCHANGING_P (XEXP (addr, 0)) = 1;

  emit_move_insn (change_address (block, Pmode,
				  plus_constant (XEXP (block, 0),
						 3 * UNITS_PER_WORD)),
		  copy_to_reg (XEXP (addr, 0)));

  /* Now store the incoming floating-point registers.   If we are not
     to use the floating-point registers, store the integer registers
     in those locations too.  */
  if (nregs < 6)
      move_block_from_reg
	(16 + 32 * (TARGET_FPREGS != 0) + nregs,
	 change_address (addr, Pmode,
			 plus_constant (XEXP (addr, 0),
					nregs * UNITS_PER_WORD)),
	 6 - nregs);

  /* Return the address of the va_list constructor, but don't put it in a
     register.  This fails when not optimizing and produces worse code when
     optimizing.  */
  return XEXP (block, 0);
}

/* This page contains routines that are used to determine what the function
   prologue and epilogue code will do and write them out.  */

/* Compute the size of the save area in the stack.  */

int
alpha_sa_size ()
{
  int size = 0;
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i])
      size++;

  return size * 8;
}

/* Return non-zero if this function needs gp.  It does if it has
   an LDSYM insn.  */

int
alpha_need_gp ()
{
  rtx insn;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	&& GET_CODE (PATTERN (insn)) != USE
	&& GET_CODE (PATTERN (insn)) != CLOBBER
	&& get_attr_type (insn) == TYPE_LDSYM)
      return 1;

  return 0;
}

/* Return 1 if GP is dead at after INSN.  */

int
alpha_gp_dead_after (insn)
     rtx insn;
{
  int jump_count = 0;
  int found = 0;
  rtx p;

  /* If we aren't optimizing, don't do this optimization.  More importantly,
     JUMP_LABEL isn't properly set when not optimizing.  */

  if (optimize == 0)
    return 0;

  /* If we are followed by a BARRIER, we don't return.  */
  if (NEXT_INSN (insn) && GET_CODE (NEXT_INSN (insn)) == BARRIER)
    return 1;

  /* Otherwise search for a use of GP before a return.  */

  for (p = next_active_insn (insn); p; p = next_active_insn (p))
    {
      if (get_attr_type (p) == TYPE_LDSYM
	  || get_attr_type (p) == TYPE_JSR)
	{
	  found = 1;
	  break;
	}

      if (GET_CODE (p) == JUMP_INSN)
	{
	  if (GET_CODE (PATTERN (p)) == RETURN)
	    break;

	  if (! simplejump_p (p) || jump_count++ > 10)
	    {
	      found = 1;
	      break;
	    }

	  p = JUMP_LABEL (p);
	}
    }

  /* Restore any operands destroyed by the attribute calls above.  */
  insn_extract (insn);

  return ! found;
}

/* Return 1 if this function can directly return via $26.  */

int
direct_return ()
{
  return (reload_completed && alpha_sa_size () == 0
	  && get_frame_size () == 0
	  && current_function_pretend_args_size == 0);
}

/* Write function prologue.  */

void
output_prolog (file, size)
     FILE *file;
     int size;
{
  HOST_WIDE_INT frame_size = ((size + current_function_outgoing_args_size
			       + current_function_pretend_args_size
			       + alpha_sa_size () + 15) & ~15);
  int reg_offset = current_function_outgoing_args_size;
  int start_reg_offset = reg_offset;
  unsigned reg_mask = 0;
  int i;

  /* If we need a GP, load it first.  */
  alpha_function_needs_gp = alpha_need_gp ();

  if (alpha_function_needs_gp)
    {
      rtx insn;

      fprintf (file, "\tldgp $29,0($27)\n");

      /* If we have a recursive call, put a special label here.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == CALL_INSN
	    && get_attr_type (insn) != TYPE_JSR)
	  {
	    fprintf (file, "%s..ng:\n", current_function_name);
	    break;
	  }
    }

  /* Adjust the stack by the frame size.  If the frame size is > 32768
     bytes, we have to load it into a register first and then subtract
     from sp.  Note that we are only allowed to adjust sp once in the
     prologue.  */

  if (frame_size > 32768)
    {
      HOST_WIDE_INT low = (frame_size & 0xffff) - 2 * (frame_size & 0x8000);
      HOST_WIDE_INT tmp1 = frame_size - low;
      HOST_WIDE_INT high
	= ((tmp1 >> 16) & 0xfff) - 2 * ((tmp1 >> 16) & 0x8000);
      HOST_WIDE_INT tmp2 = frame_size - (high << 16) - low;
      HOST_WIDE_INT extra = 0;
      int in_reg = 31;

      /* We haven't written code to handle frames > 4GB.  */
#if HOST_BITS_PER_LONG_INT == 64
      if ((unsigned HOST_WIDE_INT) frame_size >> 32 != 0)
	abort ();
#endif

      if (tmp2)
	{
	  extra = 0x4000;
	  tmp1 -= 0x40000000;
	  high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	}

      if (low != 0)
	{
	  fprintf (file, "\tlda $28,%d($%d)\n", low, in_reg);
	  in_reg = 28;
	}

      if (extra)
	{
	  fprintf (file, "\tldah $28,%d($%d)\n", extra, in_reg);
	  in_reg = 28;
	}

      fprintf (file, "\tldah $28,%d($%d)\n", high, in_reg);

      fprintf (file, "\tsubq $30,$28,$30\n");
    }
  else if (frame_size)
    fprintf (file, "\tlda $30,-%d($30)\n", frame_size);

  /* Write out the .frame line.  If we need a frame pointer, we use
     an offset of zero.  */

  if (frame_pointer_needed)
    fprintf (file, "\t.frame $15,0,$26\n");
  else
    fprintf (file, "\t.frame $30,%d,$26\n", frame_size);

    
  /* Save register 26 if it is used.  */
  if (regs_ever_live[26])
    {
      reg_mask |= 1 << 26;
      fprintf (file, "\tstq $26,%d($30)\n", reg_offset);
      reg_offset += 8;
    }

  /* Now save any other used register that are required to be saved.  */
  for (i = 0; i < 32; i++)
    if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i] && i != 26)
      {
	reg_mask |= 1 << i;
	fprintf (file, "\tstq $%d,%d($30)\n", i, reg_offset);
	reg_offset += 8;
      }

  /* Print the register mask and do floating-point saves.  */
  if (reg_mask)
    fprintf (file, "\t.mask 0x%x,%d\n", reg_mask,
	     start_reg_offset - frame_size);

  start_reg_offset = reg_offset;
  reg_mask = 0;

  for (i = 0; i < 32; i++)
    if (! fixed_regs[i + 32] && ! call_used_regs[i + 32]
	&& regs_ever_live[i + 32])
      {
	reg_mask |= 1 << i;
	fprintf (file, "\tstt $f%d,%d($30)\n", i, reg_offset);
	reg_offset += 8;
      }

  /* Print the floating-point mask, if we've saved any fp register.  */
  if (reg_mask)
    fprintf (file, "\t.fmask 0x%x,%d\n", reg_mask, start_reg_offset);

  /* If we need a frame pointer, set it to the value of incoming stack
     which we compute by adding back the frame size pointer.  Because we
     can subtract one more than we can add, we have to special-case
     frame sizes of 32K.  Note that there is no restriction that the frame
     pointer be updated in one instruction.  */

  if (frame_pointer_needed)
    {
      if (frame_size == 32768)
	fprintf (file, "\tlda $15,16384($30)\n\tlda $15,16384($15)\n");
      else if (frame_size > 32768)
	fprintf (file, "\taddq $30,$28,$15\n");
      else
	fprintf (file, "\tlda $15,%d($30)\n", frame_size);
    }
}

/* Write function epilogue.  */

void
output_epilog (file, size)
     FILE *file;
     int size;
{
  rtx insn = get_last_insn ();
  HOST_WIDE_INT frame_size = ((size + current_function_outgoing_args_size
			       + current_function_pretend_args_size
			       + alpha_sa_size () + 15) & ~15);
  int reg_offset = current_function_outgoing_args_size;
  int reg_offset_from = STACK_POINTER_REGNUM;
  int i;

  /* If the last insn was a BARRIER, we don't have to write anything except
     the .end pseudo-op.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn == 0 || GET_CODE (insn) != BARRIER)
    {
      /* If we have a frame pointer, we restore the registers from an
	 offset from it, assuming that we can reach the offset.  If not,
	 we have to compute the address using a scratch register.  This is
	 messy, but should not be common.  We have to copy the frame
	 pointer elsewhere here since we will be restoring it before we can
	 use it to restore the stack pointer.  We use $25.  */

      if (frame_pointer_needed)
	{
	  fprintf (file, "\tbis $15,$15,$25\n");

	  if (frame_size < 32768)
	    reg_offset -= frame_size, reg_offset_from = 25;
	  else
	    {
	      HOST_WIDE_INT low
		= (frame_size & 0xffff) - 2 * (frame_size & 0x8000);
	      HOST_WIDE_INT tmp1 = frame_size - low;
	      HOST_WIDE_INT high
		= ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	      HOST_WIDE_INT tmp2 = frame_size - (high << 16) - low;
	      int extra = 0;
	      int in_reg = 31;

	      if (tmp2)
		{
		  extra = 0x4000;
		  tmp1 -= 0x40000000;	
		  high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
		}

	      if (low != 0)
		{
		  fprintf (file, "\tlda $28,%d($%d)\n", low, in_reg);
		  in_reg = 28;
		}

	      if (extra)
		{
		  fprintf (file, "\tldah $28,%d($%d)\n", extra, in_reg);
		  in_reg = 28;
		}

	      fprintf (file, "\tldah $28,%d($%d)\n", high, in_reg);

	      fprintf (file, "\tsubq $25,$28,$28\n");

	      reg_offset_from = 28;
	    }
	}

      /* Restore all the registers, starting with the return address
	 register.  */
      if (regs_ever_live[26])
	{
	  fprintf (file, "\tldq $26,%d($%d)\n", reg_offset, reg_offset_from);
	  reg_offset += 8;
	}

      /* Now restore any other used register that that we saved.  */
      for (i = 0; i < 32; i++)
	if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i]
	    && i != 26)
	  {
	    fprintf (file, "\tldq $%d,%d($%d)\n",
		     i, reg_offset, reg_offset_from);
	    reg_offset += 8;
	  }

      for (i = 0; i < 32; i++)
	if (! fixed_regs[i + 32] && ! call_used_regs[i + 32]
	    && regs_ever_live[i + 32])
	  {
	    fprintf (file, "\tldt $f%d,%d($%d)\n",
		     i, reg_offset, reg_offset_from);
	    reg_offset += 8;
	  }

      /* Restore the stack.  If we have a frame pointer, use it.  Otherwise,
	 add the size back into the stack, handling the large frame size.  */

      if (frame_pointer_needed)
	fprintf (file, "\tbis $25,$25,$30\n");
      else if (frame_size > 32767)
	{
	  HOST_WIDE_INT low
	    = (frame_size & 0xffff) - 2 * (frame_size & 0x8000);
	  HOST_WIDE_INT tmp1 = frame_size - low;
	  HOST_WIDE_INT high
	    = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	  HOST_WIDE_INT tmp2 = frame_size - (high << 16) - low;
	  HOST_WIDE_INT extra = 0;
	  int in_reg = 31;

	  /* We haven't written code to handle frames > 4GB.  */
#if HOST_BITS_PER_LONG_INT == 64
	  if ((unsigned HOST_WIDE_INT) frame_size >> 32 != 0)
	    abort ();
#endif

	  if (tmp2)
	    {
	      extra = 0x4000;
	      tmp1 -= 0x40000000;
	      high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	    }

	  if (low != 0)
	    {
	      fprintf (file, "\tlda $28,%d($%d)\n", low, in_reg);
	      in_reg = 28;
	    }

	  if (extra)
	    {
	      fprintf (file, "\tldah $28,%d($%d)\n", extra, in_reg);
	      in_reg = 28;
	    }

	  fprintf (file, "\tldah $28,%d($%d)\n", high, in_reg);

	  fprintf (file, "\taddq $30,$28,$30\n");
	}
      else if (frame_size)
	fprintf (file, "\tlda $30,%d($30)\n", frame_size);

      /* Now return to the caller.  */
      fprintf (file, "\tret $31,($26),1\n");
    }

  /* End the function.  */
  fprintf (file, "\t.end %s\n", alpha_function_name);
}
