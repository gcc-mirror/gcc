/* Subroutines used for code generation on the DEC Alpha.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
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

extern char *version_string;

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

/* Return 1 if OP is a SYMBOL_REF for a function known to be in this
   file.  */

int
current_file_function_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == SYMBOL_REF
	  && (SYMBOL_REF_FLAG (op)
	      || op == XEXP (DECL_RTL (current_function_decl), 0)));
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
      HOST_WIDE_INT extra = 0;

      /* If HIGH will be interpreted as negative but the constant is
	 positive, we must adjust it to do two ldha insns.  */

      if ((high & 0x8000) != 0 && c >= 0)
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

  /* Compute the current position into the args, taking into account
     both registers and memory.  */

  argsize = plus_constant (current_function_arg_offset_rtx,
			   current_function_args_info * UNITS_PER_WORD);

  /* Allocate the va_list constructor */
  block = assign_stack_local (BLKmode, 2 * UNITS_PER_WORD, BITS_PER_WORD);
  RTX_UNCHANGING_P (block) = 1;
  RTX_UNCHANGING_P (XEXP (block, 0)) = 1;

  /* Store the address of the first integer register in the
     __va_base member.   */

  emit_move_insn (change_address (block, DImode, XEXP (block, 0)),
		  force_operand (plus_constant (virtual_incoming_args_rtx,
						6 * UNITS_PER_WORD),
				 NULL_RTX));

  /* Store the argsize as the __va_offset member.  */
  emit_move_insn (change_address (block, Pmode,
				  plus_constant (XEXP (block, 0),
						 UNITS_PER_WORD)),
		  force_operand (argsize, NULL_RTX));

  /* Return the address of the va_list constructor, but don't put it in a
     register.  Doing so would fail when not optimizing and produce worse
     code when optimizing.  */
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

  /* If some registers were saved but not reg 26, reg 26 must also
     be saved, so leave space for it.  */
  if (size != 0 && ! regs_ever_live[26])
    size++;

  return size * 8;
}

/* Return 1 if this function can directly return via $26.  */

int
direct_return ()
{
  return (reload_completed && alpha_sa_size () == 0
	  && get_frame_size () == 0
	  && current_function_pretend_args_size == 0);
}

/* Write a version stamp.  Don't write anything if we are running as a
   cross-compiler.  Otherwise, use the versions in /usr/include/stamp.h.  */

#ifndef CROSS_COMPILE
#include <stamp.h>
#endif

void
alpha_write_verstamp (file)
     FILE *file;
{
#ifdef MS_STAMP
  char *p;

  fprintf (file, "\t.verstamp %d %d ", MS_STAMP, LS_STAMP);
  for (p = version_string; *p != ' ' && *p != 0; p++)
    fprintf (file, "%c", *p == '.' ? ' ' : *p);
  fprintf (file, "\n");
#endif
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
  int reg_offset = size + current_function_outgoing_args_size;
  rtx insn;
  int start_reg_offset = reg_offset;
  unsigned reg_mask = 0;
  int i;

  /* If we need a GP (we have a LDSYM insn or a CALL_INSN), load it first. 
     Even if we are a static function, we still need to do this in case
     our address is taken and passed to something like qsort.  */

  alpha_function_needs_gp = 0;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if ((GET_CODE (insn) == CALL_INSN)
	|| (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	    && GET_CODE (PATTERN (insn)) != USE
	    && GET_CODE (PATTERN (insn)) != CLOBBER
	    && get_attr_type (insn) == TYPE_LDSYM))
      {
	alpha_function_needs_gp = 1;
	break;
      }

  if (alpha_function_needs_gp)
    fprintf (file, "\tldgp $29,0($27)\n");

  /* Put a label after the GP load so we can enter the function at it.  */
  fprintf (file, "%s..ng:\n", alpha_function_name);

  /* Adjust the stack by the frame size.  If the frame size is > 4096
     bytes, we need to be sure we probe somewhere in the first and last
     4096 bytes (we can probably get away without the latter test) and
     every 8192 bytes in between.  If the frame size is > 32768, we
     do this in a loop.  Otherwise, we generate the explicit probe
     instructions. 

     Note that we are only allowed to adjust sp once in the prologue.  */

  if (frame_size < 32768)
    {
      if (frame_size > 4096)
	{
	  int probed = 4096;
	  int regnum = 2;

	  fprintf (file, "\tldq $%d,-%d($30)\n", regnum++, probed);

	  while (probed + 8192 < frame_size)
	    fprintf (file, "\tldq $%d,-%d($30)\n", regnum++, probed += 8192);

	  if (probed + 4096 < frame_size)
	    fprintf (file, "\tldq $%d,-%d($30)\n", regnum++, probed += 4096);

	  if (regnum > 9)
	    abort ();
	}

      if (frame_size != 0)
	fprintf (file, "\tlda $30,-%d($30)\n", frame_size);
    }
  else
    {
      /* Here we generate code to set R4 to SP + 4096 and set R5 to the
	 number of 8192 byte blocks to probe.  We then probe each block
	 in the loop and then set SP to the proper location.  If the
	 amount remaining is > 4096, we have to do one more probe.

	 This is complicated by the code we would generate if
	 the number of blocks > 32767.  */

      HOST_WIDE_INT blocks = (frame_size + 4096) / 8192;
      HOST_WIDE_INT leftover = frame_size + 4096 - blocks * 8192;
      HOST_WIDE_INT low = (blocks & 0xffff) - 2 * (blocks & 0x8000);
      HOST_WIDE_INT tmp1 = blocks - low;
      HOST_WIDE_INT high
	= ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
      HOST_WIDE_INT extra = 0;
      int in_reg = 31;

      /* If HIGH will be interpreted as negative, we must adjust it to
	 do two ldha insns.  Note that we will never be building a negative
	 constant here.  */

      if (high & 0x8000)
	{
	  extra = 0x4000;
	  tmp1 -= 0x40000000;
	  high = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	}

      if (low != 0)
	{
	  if (low < 255)
	    fprintf (file, "\tbis $31,%d,$5\n", low);
	  else
	    fprintf (file, "\tlda $5,%d($31)\n", low);
	  in_reg = 5;
	}

      if (extra)
	{
	  fprintf (file, "\tldah $5,%d($%d)\n", extra, in_reg);
	  in_reg = 5;
	}

      if (high)
	fprintf (file, "\tldah $5,%d($%d)\n", high, in_reg);

      fprintf (file, "\tlda $4,4096($30)\n");
      fprintf (file, "%s..sc:\n", alpha_function_name);
      fprintf (file, "\tldq $6,-8192($4)\n");
      fprintf (file, "\tsubq $5,1,$5\n");
      fprintf (file, "\tlda $4,-8192($4)\n");
      fprintf (file, "\tbne $5,%s..sc\n", alpha_function_name);
      fprintf (file, "\tlda $30,-%d($4)\n", leftover);

      if (leftover > 4096)
	fprintf (file, "\tldq $2,%d(sp)\n", leftover - 4096);
    }

  /* Describe our frame.  */
  fprintf (file, "\t.frame $%d,%d,$26,%d\n", 
	   frame_pointer_needed ? FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM,
	   frame_size, current_function_pretend_args_size);
    
  /* Save register 26 if it is used or if any other register needs to
     be saved.  */
  if (regs_ever_live[26] || alpha_sa_size () != 0)
    {
      reg_mask |= 1 << 26;
      fprintf (file, "\tstq $26,%d($30)\n", reg_offset);
      reg_offset += 8;
    }

  /* Now save any other used integer registers required to be saved.  */
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

  /* If we need a frame pointer, set it from the stack pointer.  Note that
     this must always be the last instruction in the prologue.  */
  if (frame_pointer_needed)
    fprintf (file, "\tbis $30,$30,$15\n");

  /* End the prologue and say if we used gp.  */
  fprintf (file, "\t.prologue %d\n", alpha_function_needs_gp);
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
  int reg_offset = size + current_function_outgoing_args_size;
  int i;

  /* If the last insn was a BARRIER, we don't have to write anything except
     the .end pseudo-op.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn == 0 || GET_CODE (insn) != BARRIER)
    {
      int fp_offset;

      /* If we have a frame pointer, restore SP from it.  */
      if (frame_pointer_needed)
	fprintf (file, "\tbis $15,$15,$30\n");

      /* Restore all the registers, starting with the return address
	 register.  */
      if (regs_ever_live[26] || alpha_sa_size () != 0)
	{
	  fprintf (file, "\tldq $26,%d($30)\n", reg_offset);
	  reg_offset += 8;
	}

      /* Now restore any other used integer registers that that we saved,
	 except for FP if it is being used as FP, since it must be
	 restored last.  */

      for (i = 0; i < 32; i++)
	if (! fixed_regs[i] && ! call_used_regs[i] && regs_ever_live[i]
	    && i != 26)
	  {
	    if (i == FRAME_POINTER_REGNUM && frame_pointer_needed)
	      fp_offset = reg_offset;
	    else
	      fprintf (file, "\tldq $%d,%d($30)\n", i, reg_offset);
	    reg_offset += 8;
	  }

      for (i = 0; i < 32; i++)
	if (! fixed_regs[i + 32] && ! call_used_regs[i + 32]
	    && regs_ever_live[i + 32])
	  {
	    fprintf (file, "\tldt $f%d,%d($30)\n", i, reg_offset);
	    reg_offset += 8;
	  }

      /* If the stack size is large, compute the size of the stack into
	 a register because the old FP restore, stack pointer adjust,
	 and return are required to be consecutive instructions.  */
      if (frame_size > 32767)
	{
	  HOST_WIDE_INT low
	    = (frame_size & 0xffff) - 2 * (frame_size & 0x8000);
	  HOST_WIDE_INT tmp1 = frame_size - low;
	  HOST_WIDE_INT high
	    = ((tmp1 >> 16) & 0xffff) - 2 * ((tmp1 >> 16) & 0x8000);
	  HOST_WIDE_INT extra = 0;
	  int in_reg = 31;

	  /* We haven't written code to handle frames > 4GB.  */
#if HOST_BITS_PER_LONG_INT == 64
	  if ((unsigned HOST_WIDE_INT) frame_size >> 32 != 0)
	    abort ();
#endif

	  /* If HIGH will be interpreted as negative, we must adjust it to
	     do two ldha insns.  Note that we will never be building a negative
	     constant here.  */

	  if (high & 0x8000)
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
	}

      /* If we needed a frame pointer and we have to restore it, do it
	 now.  */

      if (frame_pointer_needed && regs_ever_live[FRAME_POINTER_REGNUM])
	fprintf (file, "\tldq $15,%d($30)\n", fp_offset);

      /* Now update the stack pointer, if needed.  This must be done in
	 one, stylized, instruction.  */
      if (frame_size > 32768)
	fprintf (file, "\taddq $28,$30,$30\n");
      else if (frame_size != 0)
	fprintf (file, "\tlda $30,%d($30)\n", frame_size);

      /* Finally return to the caller.  */
      fprintf (file, "\tret $31,($26),1\n");
    }

  /* End the function.  */
  fprintf (file, "\t.end %s\n", alpha_function_name);

  /* Show that we know this function if it is called again.  */
  SYMBOL_REF_FLAG (XEXP (DECL_RTL (current_function_decl), 0)) = 1;
}
