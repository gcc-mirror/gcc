/* Analyze RTL for C-Compiler
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
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
#include "hard-reg-set.h"
#include "tm_p.h"

/* Forward declarations */
static void set_of_1		PARAMS ((rtx, rtx, void *));
static void insn_dependent_p_1	PARAMS ((rtx, rtx, void *));
static int computed_jump_p_1	PARAMS ((rtx));
static void parms_set 		PARAMS ((rtx, rtx, void *));

/* Bit flags that specify the machine subtype we are compiling for.
   Bits are tested using macros TARGET_... defined in the tm.h file
   and set by `-m...' switches.  Must be defined in rtlanal.c.  */

int target_flags;

/* Return 1 if the value of X is unstable
   (would be different at a different point in the program).
   The frame pointer, arg pointer, etc. are considered stable
   (within one function) and so is anything marked `unchanging'.  */

int
rtx_unstable_p (x)
     rtx x;
{
  RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  switch (code)
    {
    case MEM:
      return ! RTX_UNCHANGING_P (x) || rtx_unstable_p (XEXP (x, 0));

    case QUEUED:
      return 1;

    case ADDRESSOF:
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case REG:
      /* As in rtx_varies_p, we have to use the actual rtx, not reg number.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  /* The arg pointer varies if it is not a fixed register.  */
	  || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM])
	  || RTX_UNCHANGING_P (x))
	return 0;
#ifndef PIC_OFFSET_TABLE_REG_CALL_CLOBBERED
      /* ??? When call-clobbered, the value is stable modulo the restore
	 that must happen after a call.  This currently screws up local-alloc
	 into believing that the restore is not needed.  */
      if (x == pic_offset_table_rtx)
	return 0;
#endif
      return 1;

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

      /* FALLTHROUGH */

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (rtx_unstable_p (XEXP (x, i)))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (rtx_unstable_p (XVECEXP (x, i, j)))
	    return 1;
      }

  return 0;
}

/* Return 1 if X has a value that can vary even between two
   executions of the program.  0 means X can be compared reliably
   against certain constants or near-constants.
   FOR_ALIAS is nonzero if we are called from alias analysis; if it is
   zero, we are slightly more conservative.
   The frame pointer and the arg pointer are considered constant.  */

int
rtx_varies_p (x, for_alias)
     rtx x;
     int for_alias;
{
  RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  switch (code)
    {
    case MEM:
      return ! RTX_UNCHANGING_P (x) || rtx_varies_p (XEXP (x, 0), for_alias);

    case QUEUED:
      return 1;

    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case REG:
      /* Note that we have to test for the actual rtx used for the frame
	 and arg pointers and not just the register number in case we have
	 eliminated the frame and/or arg pointer and are using it
	 for pseudos.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  /* The arg pointer varies if it is not a fixed register.  */
	  || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM]))
	return 0;
      if (x == pic_offset_table_rtx
#ifdef PIC_OFFSET_TABLE_REG_CALL_CLOBBERED
	  /* ??? When call-clobbered, the value is stable modulo the restore
	     that must happen after a call.  This currently screws up
	     local-alloc into believing that the restore is not needed, so we
	     must return 0 only if we are called from alias analysis.  */
	  && for_alias
#endif
	  )
	return 0;
      return 1;

    case LO_SUM:
      /* The operand 0 of a LO_SUM is considered constant
	 (in fact it is related specifically to operand 1)
	 during alias analysis.  */
      return (! for_alias && rtx_varies_p (XEXP (x, 0), for_alias))
	     || rtx_varies_p (XEXP (x, 1), for_alias);
      
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

      /* FALLTHROUGH */

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (rtx_varies_p (XEXP (x, i), for_alias))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (rtx_varies_p (XVECEXP (x, i, j), for_alias))
	    return 1;
      }

  return 0;
}

/* Return 0 if the use of X as an address in a MEM can cause a trap.  */

int
rtx_addr_can_trap_p (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case SYMBOL_REF:
      return SYMBOL_REF_WEAK (x);

    case LABEL_REF:
      return 0;

    case REG:
      /* As in rtx_varies_p, we have to use the actual rtx, not reg number.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  || x == stack_pointer_rtx
	  /* The arg pointer varies if it is not a fixed register.  */
	  || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM]))
	return 0;
      /* All of the virtual frame registers are stack references.  */
      if (REGNO (x) >= FIRST_VIRTUAL_REGISTER
	  && REGNO (x) <= LAST_VIRTUAL_REGISTER)
	return 0;
      return 1;

    case CONST:
      return rtx_addr_can_trap_p (XEXP (x, 0));

    case PLUS:
      /* An address is assumed not to trap if it is an address that can't
	 trap plus a constant integer or it is the pic register plus a
	 constant.  */
      return ! ((! rtx_addr_can_trap_p (XEXP (x, 0))
		 && GET_CODE (XEXP (x, 1)) == CONST_INT)
		|| (XEXP (x, 0) == pic_offset_table_rtx
		    && CONSTANT_P (XEXP (x, 1))));

    case LO_SUM:
    case PRE_MODIFY:
      return rtx_addr_can_trap_p (XEXP (x, 1));

    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      return rtx_addr_can_trap_p (XEXP (x, 0));

    default:
      break;
    }

  /* If it isn't one of the case above, it can cause a trap.  */
  return 1;
}

/* Return 1 if X refers to a memory location whose address 
   cannot be compared reliably with constant addresses,
   or if X refers to a BLKmode memory object. 
   FOR_ALIAS is nonzero if we are called from alias analysis; if it is
   zero, we are slightly more conservative.  */

int
rtx_addr_varies_p (x, for_alias)
     rtx x;
     int for_alias;
{
  enum rtx_code code;
  int i;
  const char *fmt;

  if (x == 0)
    return 0;

  code = GET_CODE (x);
  if (code == MEM)
    return GET_MODE (x) == BLKmode || rtx_varies_p (XEXP (x, 0), for_alias);

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (rtx_addr_varies_p (XEXP (x, i), for_alias))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (rtx_addr_varies_p (XVECEXP (x, i, j), for_alias))
	    return 1;
      }
  return 0;
}

/* Return the value of the integer term in X, if one is apparent;
   otherwise return 0.
   Only obvious integer terms are detected.
   This is used in cse.c with the `related_value' field.  */

HOST_WIDE_INT
get_integer_term (x)
     rtx x;
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == MINUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    return - INTVAL (XEXP (x, 1));
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    return INTVAL (XEXP (x, 1));
  return 0;
}

/* If X is a constant, return the value sans apparent integer term;
   otherwise return 0.
   Only obvious integer terms are detected.  */

rtx
get_related_value (x)
     rtx x;
{
  if (GET_CODE (x) != CONST)
    return 0;
  x = XEXP (x, 0);
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    return XEXP (x, 0);
  else if (GET_CODE (x) == MINUS
	   && GET_CODE (XEXP (x, 1)) == CONST_INT)
    return XEXP (x, 0);
  return 0;
}

/* Given a tablejump insn INSN, return the RTL expression for the offset
   into the jump table.  If the offset cannot be determined, then return
   NULL_RTX.

   If EARLIEST is non-zero, it is a pointer to a place where the earliest
   insn used in locating the offset was found.  */

rtx
get_jump_table_offset (insn, earliest)
     rtx insn;
     rtx *earliest;
{
  rtx label;
  rtx table;
  rtx set;
  rtx old_insn;
  rtx x;
  rtx old_x;
  rtx y;
  rtx old_y;
  int i;

  if (GET_CODE (insn) != JUMP_INSN
      || ! (label = JUMP_LABEL (insn))
      || ! (table = NEXT_INSN (label))
      || GET_CODE (table) != JUMP_INSN
      || (GET_CODE (PATTERN (table)) != ADDR_VEC
	  && GET_CODE (PATTERN (table)) != ADDR_DIFF_VEC)
      || ! (set = single_set (insn)))
    return NULL_RTX;

  x = SET_SRC (set);

  /* Some targets (eg, ARM) emit a tablejump that also
     contains the out-of-range target.  */
  if (GET_CODE (x) == IF_THEN_ELSE
      && GET_CODE (XEXP (x, 2)) == LABEL_REF)
    x = XEXP (x, 1);

  /* Search backwards and locate the expression stored in X.  */
  for (old_x = NULL_RTX; GET_CODE (x) == REG && x != old_x;
       old_x = x, x = find_last_value (x, &insn, NULL_RTX, 0))
    ;

  /* If X is an expression using a relative address then strip
     off the addition / subtraction of PC, PIC_OFFSET_TABLE_REGNUM,
     or the jump table label.  */
  if (GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC
      && (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS))
    {
      for (i = 0; i < 2; i++)
	{
	  old_insn = insn;
	  y = XEXP (x, i);

	  if (y == pc_rtx || y == pic_offset_table_rtx)
	    break;

	  for (old_y = NULL_RTX; GET_CODE (y) == REG && y != old_y;
	       old_y = y, y = find_last_value (y, &old_insn, NULL_RTX, 0))
	    ;

	  if ((GET_CODE (y) == LABEL_REF && XEXP (y, 0) == label))
	    break;
	}

      if (i >= 2)
	return NULL_RTX;

      x = XEXP (x, 1 - i);

      for (old_x = NULL_RTX; GET_CODE (x) == REG && x != old_x;
	   old_x = x, x = find_last_value (x, &insn, NULL_RTX, 0))
	;
    }

  /* Strip off any sign or zero extension.  */
  if (GET_CODE (x) == SIGN_EXTEND || GET_CODE (x) == ZERO_EXTEND)
    {
      x = XEXP (x, 0);

      for (old_x = NULL_RTX; GET_CODE (x) == REG && x != old_x;
	   old_x = x, x = find_last_value (x, &insn, NULL_RTX, 0))
	;
    }

  /* If X isn't a MEM then this isn't a tablejump we understand.  */
  if (GET_CODE (x) != MEM)
    return NULL_RTX;

  /* Strip off the MEM.  */
  x = XEXP (x, 0);

  for (old_x = NULL_RTX; GET_CODE (x) == REG && x != old_x;
       old_x = x, x = find_last_value (x, &insn, NULL_RTX, 0))
    ;

  /* If X isn't a PLUS than this isn't a tablejump we understand.  */
  if (GET_CODE (x) != PLUS)
    return NULL_RTX;

  /* At this point we should have an expression representing the jump table
     plus an offset.  Examine each operand in order to determine which one
     represents the jump table.  Knowing that tells us that the other operand
     must represent the offset.  */
  for (i = 0; i < 2; i++)
    {
      old_insn = insn;
      y = XEXP (x, i);

      for (old_y = NULL_RTX; GET_CODE (y) == REG && y != old_y;
	   old_y = y, y = find_last_value (y, &old_insn, NULL_RTX, 0))
	;

      if ((GET_CODE (y) == CONST || GET_CODE (y) == LABEL_REF)
	  && reg_mentioned_p (label, y))
	break;
    }

  if (i >= 2)
    return NULL_RTX;

  x = XEXP (x, 1 - i);

  /* Strip off the addition / subtraction of PIC_OFFSET_TABLE_REGNUM.  */
  if (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS)
    for (i = 0; i < 2; i++)
      if (XEXP (x, i) == pic_offset_table_rtx)
	{
	  x = XEXP (x, 1 - i);
	  break;
	}

  if (earliest)
    *earliest = insn;

  /* Return the RTL expression representing the offset.  */
  return x;
}

/* Return the number of places FIND appears within X.  If COUNT_DEST is
   zero, we do not count occurrences inside the destination of a SET.  */

int
count_occurrences (x, find, count_dest)
     rtx x, find;
     int count_dest;
{
  int i, j;
  enum rtx_code code;
  const char *format_ptr;
  int count;

  if (x == find)
    return 1;

  code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
      return 0;

    case MEM:
      if (GET_CODE (find) == MEM && rtx_equal_p (x, find))
	return 1;
      break;

    case SET:
      if (SET_DEST (x) == find && ! count_dest)
	return count_occurrences (SET_SRC (x), find, count_dest);
      break;

    default:
      break;
    }

  format_ptr = GET_RTX_FORMAT (code);
  count = 0;

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  count += count_occurrences (XEXP (x, i), find, count_dest);
	  break;

	case 'E':
	  for (j = 0; j < XVECLEN (x, i); j++)
	    count += count_occurrences (XVECEXP (x, i, j), find, count_dest);
	  break;
	}
    }
  return count;
}

/* Nonzero if register REG appears somewhere within IN.
   Also works if REG is not a register; in this case it checks
   for a subexpression of IN that is Lisp "equal" to REG.  */

int
reg_mentioned_p (reg, in)
     rtx reg, in;
{
  const char *fmt;
  int i;
  enum rtx_code code;

  if (in == 0)
    return 0;

  if (reg == in)
    return 1;

  if (GET_CODE (in) == LABEL_REF)
    return reg == XEXP (in, 0);

  code = GET_CODE (in);

  switch (code)
    {
      /* Compare registers by number.  */
    case REG:
      return GET_CODE (reg) == REG && REGNO (in) == REGNO (reg);

      /* These codes have no constituent expressions
	 and are unique.  */
    case SCRATCH:
    case CC0:
    case PC:
      return 0;

    case CONST_INT:
      return GET_CODE (reg) == CONST_INT && INTVAL (in) == INTVAL (reg);

    case CONST_VECTOR:
    case CONST_DOUBLE:
      /* These are kept unique for a given value.  */
      return 0;
      
    default:
      break;
    }

  if (GET_CODE (reg) == code && rtx_equal_p (reg, in))
    return 1;

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	    if (reg_mentioned_p (reg, XVECEXP (in, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && reg_mentioned_p (reg, XEXP (in, i)))
	return 1;
    }
  return 0;
}

/* Return 1 if in between BEG and END, exclusive of BEG and END, there is
   no CODE_LABEL insn.  */

int
no_labels_between_p (beg, end)
     rtx beg, end;
{
  rtx p;
  if (beg == end)
    return 0;
  for (p = NEXT_INSN (beg); p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == CODE_LABEL)
      return 0;
  return 1;
}

/* Return 1 if in between BEG and END, exclusive of BEG and END, there is
   no JUMP_INSN insn.  */

int
no_jumps_between_p (beg, end)
     rtx beg, end;
{
  rtx p;
  for (p = NEXT_INSN (beg); p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == JUMP_INSN)
      return 0;
  return 1;
}

/* Nonzero if register REG is used in an insn between
   FROM_INSN and TO_INSN (exclusive of those two).  */

int
reg_used_between_p (reg, from_insn, to_insn)
     rtx reg, from_insn, to_insn;
{
  rtx insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& (reg_overlap_mentioned_p (reg, PATTERN (insn))
	   || (GET_CODE (insn) == CALL_INSN
	      && (find_reg_fusage (insn, USE, reg)
		  || find_reg_fusage (insn, CLOBBER, reg)))))
      return 1;
  return 0;
}

/* Nonzero if the old value of X, a register, is referenced in BODY.  If X
   is entirely replaced by a new value and the only use is as a SET_DEST,
   we do not consider it a reference.  */

int
reg_referenced_p (x, body)
     rtx x;
     rtx body;
{
  int i;

  switch (GET_CODE (body))
    {
    case SET:
      if (reg_overlap_mentioned_p (x, SET_SRC (body)))
	return 1;

      /* If the destination is anything other than CC0, PC, a REG or a SUBREG
	 of a REG that occupies all of the REG, the insn references X if
	 it is mentioned in the destination.  */
      if (GET_CODE (SET_DEST (body)) != CC0
	  && GET_CODE (SET_DEST (body)) != PC
	  && GET_CODE (SET_DEST (body)) != REG
	  && ! (GET_CODE (SET_DEST (body)) == SUBREG
		&& GET_CODE (SUBREG_REG (SET_DEST (body))) == REG
		&& (((GET_MODE_SIZE (GET_MODE (SUBREG_REG (SET_DEST (body))))
		      + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)
		    == ((GET_MODE_SIZE (GET_MODE (SET_DEST (body)))
			 + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)))
	  && reg_overlap_mentioned_p (x, SET_DEST (body)))
	return 1;
      return 0;

    case ASM_OPERANDS:
      for (i = ASM_OPERANDS_INPUT_LENGTH (body) - 1; i >= 0; i--)
	if (reg_overlap_mentioned_p (x, ASM_OPERANDS_INPUT (body, i)))
	  return 1;
      return 0;

    case CALL:
    case USE:
    case IF_THEN_ELSE:
      return reg_overlap_mentioned_p (x, body);

    case TRAP_IF:
      return reg_overlap_mentioned_p (x, TRAP_CONDITION (body));

    case PREFETCH:
      return reg_overlap_mentioned_p (x, XEXP (body, 0));

    case UNSPEC:
    case UNSPEC_VOLATILE:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	if (reg_overlap_mentioned_p (x, XVECEXP (body, 0, i)))
	  return 1;
      return 0;

    case PARALLEL:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	if (reg_referenced_p (x, XVECEXP (body, 0, i)))
	  return 1;
      return 0;
      
    case CLOBBER:
      if (GET_CODE (XEXP (body, 0)) == MEM)
	if (reg_overlap_mentioned_p (x, XEXP (XEXP (body, 0), 0)))
	  return 1;
      return 0;

    case COND_EXEC:
      if (reg_overlap_mentioned_p (x, COND_EXEC_TEST (body)))
	return 1;
      return reg_referenced_p (x, COND_EXEC_CODE (body));

    default:
      return 0;
    }
}

/* Nonzero if register REG is referenced in an insn between
   FROM_INSN and TO_INSN (exclusive of those two).  Sets of REG do
   not count.  */

int
reg_referenced_between_p (reg, from_insn, to_insn)
     rtx reg, from_insn, to_insn;
{
  rtx insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& (reg_referenced_p (reg, PATTERN (insn))
	   || (GET_CODE (insn) == CALL_INSN
	      && find_reg_fusage (insn, USE, reg))))
      return 1;
  return 0;
}

/* Nonzero if register REG is set or clobbered in an insn between
   FROM_INSN and TO_INSN (exclusive of those two).  */

int
reg_set_between_p (reg, from_insn, to_insn)
     rtx reg, from_insn, to_insn;
{
  rtx insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && reg_set_p (reg, insn))
      return 1;
  return 0;
}

/* Internals of reg_set_between_p.  */
int
reg_set_p (reg, insn)
     rtx reg, insn;
{
  rtx body = insn;

  /* We can be passed an insn or part of one.  If we are passed an insn,
     check if a side-effect of the insn clobbers REG.  */
  if (INSN_P (insn))
    {
      if (FIND_REG_INC_NOTE (insn, reg)
	  || (GET_CODE (insn) == CALL_INSN
	      /* We'd like to test call_used_regs here, but rtlanal.c can't
		 reference that variable due to its use in genattrtab.  So
		 we'll just be more conservative.

		 ??? Unless we could ensure that the CALL_INSN_FUNCTION_USAGE
		 information holds all clobbered registers.  */
	      && ((GET_CODE (reg) == REG
		   && REGNO (reg) < FIRST_PSEUDO_REGISTER)
		  || GET_CODE (reg) == MEM
		  || find_reg_fusage (insn, CLOBBER, reg))))
	return 1;

      body = PATTERN (insn);
    }

  return set_of (reg, insn) != NULL_RTX;
}

/* Similar to reg_set_between_p, but check all registers in X.  Return 0
   only if none of them are modified between START and END.  Do not
   consider non-registers one way or the other.  */

int
regs_set_between_p (x, start, end)
     rtx x;
     rtx start, end;
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
    case CC0:
      return 0;

    case REG:
      return reg_set_between_p (x, start, end);
      
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && regs_set_between_p (XEXP (x, i), start, end))
	return 1;

      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (regs_set_between_p (XVECEXP (x, i, j), start, end))
	    return 1;
    }

  return 0;
}

/* Similar to reg_set_between_p, but check all registers in X.  Return 0
   only if none of them are modified between START and END.  Return 1 if
   X contains a MEM; this routine does not perform any memory aliasing.  */

int
modified_between_p (x, start, end)
     rtx x;
     rtx start, end;
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case PC:
    case CC0:
      return 1;

    case MEM:
      /* If the memory is not constant, assume it is modified.  If it is
	 constant, we still have to check the address.  */
      if (! RTX_UNCHANGING_P (x))
	return 1;
      break;

    case REG:
      return reg_set_between_p (x, start, end);
      
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && modified_between_p (XEXP (x, i), start, end))
	return 1;

      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (modified_between_p (XVECEXP (x, i, j), start, end))
	    return 1;
    }

  return 0;
}

/* Similar to reg_set_p, but check all registers in X.  Return 0 only if none
   of them are modified in INSN.  Return 1 if X contains a MEM; this routine
   does not perform any memory aliasing.  */

int
modified_in_p (x, insn)
     rtx x;
     rtx insn;
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case PC:
    case CC0:
      return 1;

    case MEM:
      /* If the memory is not constant, assume it is modified.  If it is
	 constant, we still have to check the address.  */
      if (! RTX_UNCHANGING_P (x))
	return 1;
      break;

    case REG:
      return reg_set_p (x, insn);

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && modified_in_p (XEXP (x, i), insn))
	return 1;

      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (modified_in_p (XVECEXP (x, i, j), insn))
	    return 1;
    }

  return 0;
}

/* Return true if anything in insn X is (anti,output,true) dependent on
   anything in insn Y.  */

int
insn_dependent_p (x, y)
     rtx x, y;
{
  rtx tmp;

  if (! INSN_P (x) || ! INSN_P (y))
    abort ();

  tmp = PATTERN (y);
  note_stores (PATTERN (x), insn_dependent_p_1, &tmp);
  if (tmp == NULL_RTX)
    return 1;

  tmp = PATTERN (x);
  note_stores (PATTERN (y), insn_dependent_p_1, &tmp);
  if (tmp == NULL_RTX)
    return 1;

  return 0;
}

/* A helper routine for insn_dependent_p called through note_stores.  */

static void
insn_dependent_p_1 (x, pat, data)
     rtx x;
     rtx pat ATTRIBUTE_UNUSED;
     void *data;
{
  rtx * pinsn = (rtx *) data;

  if (*pinsn && reg_mentioned_p (x, *pinsn))
    *pinsn = NULL_RTX;
}

/* Helper function for set_of.  */
struct set_of_data
  {
    rtx found;
    rtx pat;
  };

static void
set_of_1 (x, pat, data1)
     rtx x;
     rtx pat;
     void *data1;
{
   struct set_of_data *data = (struct set_of_data *) (data1);
   if (rtx_equal_p (x, data->pat)
       || (GET_CODE (x) != MEM && reg_overlap_mentioned_p (data->pat, x)))
     data->found = pat;
}

/* Give an INSN, return a SET or CLOBBER expression that does modify PAT
   (either directly or via STRICT_LOW_PART and similar modifiers).  */
rtx
set_of (pat, insn)
     rtx pat, insn;
{
  struct set_of_data data;
  data.found = NULL_RTX;
  data.pat = pat;
  note_stores (INSN_P (insn) ? PATTERN (insn) : insn, set_of_1, &data);
  return data.found;
}

/* Given an INSN, return a SET expression if this insn has only a single SET.
   It may also have CLOBBERs, USEs, or SET whose output
   will not be used, which we ignore.  */

rtx
single_set_2 (insn, pat)
     rtx insn, pat;
{
  rtx set = NULL;
  int set_verified = 1;
  int i;

  if (GET_CODE (pat) == PARALLEL)
    {
      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx sub = XVECEXP (pat, 0, i);
	  switch (GET_CODE (sub))
	    {
	    case USE:
	    case CLOBBER:
	      break;

	    case SET:
	      /* We can consider insns having multiple sets, where all
		 but one are dead as single set insns.  In common case
		 only single set is present in the pattern so we want
		 to avoid checking for REG_UNUSED notes unless necessary.

		 When we reach set first time, we just expect this is
		 the single set we are looking for and only when more
		 sets are found in the insn, we check them.  */
	      if (!set_verified)
		{
		  if (find_reg_note (insn, REG_UNUSED, SET_DEST (set))
		      && !side_effects_p (set))
		    set = NULL;
		  else
		    set_verified = 1;
		}
	      if (!set)
		set = sub, set_verified = 0;
	      else if (!find_reg_note (insn, REG_UNUSED, SET_DEST (sub))
		       || side_effects_p (sub))
		return NULL_RTX;
	      break;

	    default:
	      return NULL_RTX;
	    }
	}
    }
  return set;
}

/* Given an INSN, return nonzero if it has more than one SET, else return
   zero.  */

int
multiple_sets (insn)
     rtx insn;
{
  int found;
  int i;
  
  /* INSN must be an insn.  */
  if (! INSN_P (insn))
    return 0;

  /* Only a PARALLEL can have multiple SETs.  */
  if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      for (i = 0, found = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == SET)
	  {
	    /* If we have already found a SET, then return now.  */
	    if (found)
	      return 1;
	    else
	      found = 1;
	  }
    }
  
  /* Either zero or one SET.  */
  return 0;
}

/* Return nonzero if the destination of SET equals the source
   and there are no side effects.  */

int
set_noop_p (set)
     rtx set;
{
  rtx src = SET_SRC (set);
  rtx dst = SET_DEST (set);

  if (side_effects_p (src) || side_effects_p (dst))
    return 0;

  if (GET_CODE (dst) == MEM && GET_CODE (src) == MEM)
    return rtx_equal_p (dst, src);

  if (dst == pc_rtx && src == pc_rtx)
    return 1;

  if (GET_CODE (dst) == SIGN_EXTRACT
      || GET_CODE (dst) == ZERO_EXTRACT)
    return rtx_equal_p (XEXP (dst, 0), src)
	   && ! BYTES_BIG_ENDIAN && XEXP (dst, 2) == const0_rtx;

  if (GET_CODE (dst) == STRICT_LOW_PART)
    dst = XEXP (dst, 0);

  if (GET_CODE (src) == SUBREG && GET_CODE (dst) == SUBREG)
    {
      if (SUBREG_BYTE (src) != SUBREG_BYTE (dst))
	return 0;
      src = SUBREG_REG (src);
      dst = SUBREG_REG (dst);
    }

  return (GET_CODE (src) == REG && GET_CODE (dst) == REG
	  && REGNO (src) == REGNO (dst));
}

/* Return nonzero if an insn consists only of SETs, each of which only sets a
   value to itself.  */

int
noop_move_p (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);

  if (INSN_CODE (insn) == NOOP_MOVE_INSN_CODE)
    return 1;

  /* Insns carrying these notes are useful later on.  */
  if (find_reg_note (insn, REG_EQUAL, NULL_RTX))
    return 0;

  /* For now treat an insn with a REG_RETVAL note as a
     a special insn which should not be considered a no-op.  */
  if (find_reg_note (insn, REG_RETVAL, NULL_RTX))
    return 0;

  if (GET_CODE (pat) == SET && set_noop_p (pat))
    return 1;

  if (GET_CODE (pat) == PARALLEL)
    {
      int i;
      /* If nothing but SETs of registers to themselves,
	 this insn can also be deleted.  */
      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx tem = XVECEXP (pat, 0, i);

	  if (GET_CODE (tem) == USE
	      || GET_CODE (tem) == CLOBBER)
	    continue;

	  if (GET_CODE (tem) != SET || ! set_noop_p (tem))
	    return 0;
	}

      return 1;
    }
  return 0;
}


/* Return the last thing that X was assigned from before *PINSN.  If VALID_TO
   is not NULL_RTX then verify that the object is not modified up to VALID_TO.
   If the object was modified, if we hit a partial assignment to X, or hit a
   CODE_LABEL first, return X.  If we found an assignment, update *PINSN to
   point to it.  ALLOW_HWREG is set to 1 if hardware registers are allowed to
   be the src.  */

rtx
find_last_value (x, pinsn, valid_to, allow_hwreg)
     rtx x;
     rtx *pinsn;
     rtx valid_to;
     int allow_hwreg;
{
  rtx p;

  for (p = PREV_INSN (*pinsn); p && GET_CODE (p) != CODE_LABEL;
       p = PREV_INSN (p))
    if (INSN_P (p))
      {
	rtx set = single_set (p);
	rtx note = find_reg_note (p, REG_EQUAL, NULL_RTX);

	if (set && rtx_equal_p (x, SET_DEST (set)))
	  {
	    rtx src = SET_SRC (set);

	    if (note && GET_CODE (XEXP (note, 0)) != EXPR_LIST)
	      src = XEXP (note, 0);

	    if ((valid_to == NULL_RTX
		 || ! modified_between_p (src, PREV_INSN (p), valid_to))
		/* Reject hard registers because we don't usually want
		   to use them; we'd rather use a pseudo.  */
		&& (! (GET_CODE (src) == REG
		      && REGNO (src) < FIRST_PSEUDO_REGISTER) || allow_hwreg))
	      {
		*pinsn = p;
		return src;
	      }
	  }
	  
	/* If set in non-simple way, we don't have a value.  */
	if (reg_set_p (x, p))
	  break;
      }

  return x;
}     

/* Return nonzero if register in range [REGNO, ENDREGNO)
   appears either explicitly or implicitly in X
   other than being stored into.

   References contained within the substructure at LOC do not count.
   LOC may be zero, meaning don't ignore anything.  */

int
refers_to_regno_p (regno, endregno, x, loc)
     unsigned int regno, endregno;
     rtx x;
     rtx *loc;
{
  int i;
  unsigned int x_regno;
  RTX_CODE code;
  const char *fmt;

 repeat:
  /* The contents of a REG_NONNEG note is always zero, so we must come here
     upon repeat in case the last REG_NOTE is a REG_NONNEG note.  */
  if (x == 0)
    return 0;

  code = GET_CODE (x);

  switch (code)
    {
    case REG:
      x_regno = REGNO (x);

      /* If we modifying the stack, frame, or argument pointer, it will
	 clobber a virtual register.  In fact, we could be more precise,
	 but it isn't worth it.  */
      if ((x_regno == STACK_POINTER_REGNUM
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	   || x_regno == ARG_POINTER_REGNUM
#endif
	   || x_regno == FRAME_POINTER_REGNUM)
	  && regno >= FIRST_VIRTUAL_REGISTER && regno <= LAST_VIRTUAL_REGISTER)
	return 1;

      return (endregno > x_regno
	      && regno < x_regno + (x_regno < FIRST_PSEUDO_REGISTER 
				    ? HARD_REGNO_NREGS (x_regno, GET_MODE (x))
			      : 1));

    case SUBREG:
      /* If this is a SUBREG of a hard reg, we can see exactly which
	 registers are being modified.  Otherwise, handle normally.  */
      if (GET_CODE (SUBREG_REG (x)) == REG
	  && REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER)
	{
	  unsigned int inner_regno = subreg_regno (x);
	  unsigned int inner_endregno
	    = inner_regno + (inner_regno < FIRST_PSEUDO_REGISTER
			     ? HARD_REGNO_NREGS (regno, GET_MODE (x)) : 1);

	  return endregno > inner_regno && regno < inner_endregno;
	}
      break;

    case CLOBBER:
    case SET:
      if (&SET_DEST (x) != loc
	  /* Note setting a SUBREG counts as referring to the REG it is in for
	     a pseudo but not for hard registers since we can
	     treat each word individually.  */
	  && ((GET_CODE (SET_DEST (x)) == SUBREG
	       && loc != &SUBREG_REG (SET_DEST (x))
	       && GET_CODE (SUBREG_REG (SET_DEST (x))) == REG
	       && REGNO (SUBREG_REG (SET_DEST (x))) >= FIRST_PSEUDO_REGISTER
	       && refers_to_regno_p (regno, endregno,
				     SUBREG_REG (SET_DEST (x)), loc))
	      || (GET_CODE (SET_DEST (x)) != REG
		  && refers_to_regno_p (regno, endregno, SET_DEST (x), loc))))
	return 1;

      if (code == CLOBBER || loc == &SET_SRC (x))
	return 0;
      x = SET_SRC (x);
      goto repeat;

    default:
      break;
    }

  /* X does not match, so try its subexpressions.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && loc != &XEXP (x, i))
	{
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }
	  else
	    if (refers_to_regno_p (regno, endregno, XEXP (x, i), loc))
	      return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >=0; j--)
	    if (loc != &XVECEXP (x, i, j)
		&& refers_to_regno_p (regno, endregno, XVECEXP (x, i, j), loc))
	      return 1;
	}
    }
  return 0;
}

/* Nonzero if modifying X will affect IN.  If X is a register or a SUBREG,
   we check if any register number in X conflicts with the relevant register
   numbers.  If X is a constant, return 0.  If X is a MEM, return 1 iff IN
   contains a MEM (we don't bother checking for memory addresses that can't
   conflict because we expect this to be a rare case.  */

int
reg_overlap_mentioned_p (x, in)
     rtx x, in;
{
  unsigned int regno, endregno;

  /* Overly conservative.  */
  if (GET_CODE (x) == STRICT_LOW_PART)
    x = XEXP (x, 0);

  /* If either argument is a constant, then modifying X can not affect IN.  */
  if (CONSTANT_P (x) || CONSTANT_P (in))
    return 0;

  switch (GET_CODE (x))
    {
    case SUBREG:
      regno = REGNO (SUBREG_REG (x));
      if (regno < FIRST_PSEUDO_REGISTER)
	regno = subreg_regno (x);
      goto do_reg;

    case REG:
      regno = REGNO (x);
    do_reg:
      endregno = regno + (regno < FIRST_PSEUDO_REGISTER
			  ? HARD_REGNO_NREGS (regno, GET_MODE (x)) : 1);
      return refers_to_regno_p (regno, endregno, in, (rtx*) 0);

    case MEM:
      {
	const char *fmt;
	int i;

	if (GET_CODE (in) == MEM)
	  return 1;

	fmt = GET_RTX_FORMAT (GET_CODE (in));
	for (i = GET_RTX_LENGTH (GET_CODE (in)) - 1; i >= 0; i--)
	  if (fmt[i] == 'e' && reg_overlap_mentioned_p (x, XEXP (in, i)))
	    return 1;

	return 0;
      }

    case SCRATCH:
    case PC:
    case CC0:
      return reg_mentioned_p (x, in);

    case PARALLEL:
      {
	int i;

	/* If any register in here refers to it we return true.  */
	for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	  if (XEXP (XVECEXP (x, 0, i), 0) != 0
	      && reg_overlap_mentioned_p (XEXP (XVECEXP (x, 0, i), 0), in))
	      return 1;
	return 0;
      }

    default:
      break;
    }

  abort ();
}

/* Return the last value to which REG was set prior to INSN.  If we can't
   find it easily, return 0.

   We only return a REG, SUBREG, or constant because it is too hard to
   check if a MEM remains unchanged.  */

rtx
reg_set_last (x, insn)
     rtx x;
     rtx insn;
{
  rtx orig_insn = insn;

  /* Scan backwards until reg_set_last_1 changed one of the above flags.
     Stop when we reach a label or X is a hard reg and we reach a
     CALL_INSN (if reg_set_last_last_regno is a hard reg).

     If we find a set of X, ensure that its SET_SRC remains unchanged.  */

  /* We compare with <= here, because reg_set_last_last_regno
     is actually the number of the first reg *not* in X.  */
  for (;
       insn && GET_CODE (insn) != CODE_LABEL
       && ! (GET_CODE (insn) == CALL_INSN
	     && REGNO (x) <= FIRST_PSEUDO_REGISTER);
       insn = PREV_INSN (insn))
    if (INSN_P (insn))
      {
	rtx set = set_of (x, insn);
	/* OK, this function modify our register.  See if we understand it.  */
	if (set)
	  {
	    rtx last_value;
	    if (GET_CODE (set) != SET || SET_DEST (set) != x)
	      return 0;
	    last_value = SET_SRC (x);
	    if (CONSTANT_P (last_value)
		|| ((GET_CODE (last_value) == REG
		     || GET_CODE (last_value) == SUBREG)
		    && ! reg_set_between_p (last_value,
					    insn, orig_insn)))
	      return last_value;
	    else
	      return 0;
	  }
      }

  return 0;
}

/* Call FUN on each register or MEM that is stored into or clobbered by X.
   (X would be the pattern of an insn).
   FUN receives two arguments:
     the REG, MEM, CC0 or PC being stored in or clobbered,
     the SET or CLOBBER rtx that does the store.

  If the item being stored in or clobbered is a SUBREG of a hard register,
  the SUBREG will be passed.  */
     
void
note_stores (x, fun, data)
     rtx x;
     void (*fun) PARAMS ((rtx, rtx, void *));
     void *data;
{
  int i;

  if (GET_CODE (x) == COND_EXEC)
    x = COND_EXEC_CODE (x);

  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
    {
      rtx dest = SET_DEST (x);

      while ((GET_CODE (dest) == SUBREG
	      && (GET_CODE (SUBREG_REG (dest)) != REG
		  || REGNO (SUBREG_REG (dest)) >= FIRST_PSEUDO_REGISTER))
	     || GET_CODE (dest) == ZERO_EXTRACT
	     || GET_CODE (dest) == SIGN_EXTRACT
	     || GET_CODE (dest) == STRICT_LOW_PART)
	dest = XEXP (dest, 0);

      /* If we have a PARALLEL, SET_DEST is a list of EXPR_LIST expressions,
	 each of whose first operand is a register.  */
      if (GET_CODE (dest) == PARALLEL)
	{
	  for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	    if (XEXP (XVECEXP (dest, 0, i), 0) != 0)
	      (*fun) (XEXP (XVECEXP (dest, 0, i), 0), x, data);
	}
      else
	(*fun) (dest, x, data);
    }

  else if (GET_CODE (x) == PARALLEL)
    for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
      note_stores (XVECEXP (x, 0, i), fun, data);
}

/* Like notes_stores, but call FUN for each expression that is being
   referenced in PBODY, a pointer to the PATTERN of an insn.  We only call
   FUN for each expression, not any interior subexpressions.  FUN receives a
   pointer to the expression and the DATA passed to this function.

   Note that this is not quite the same test as that done in reg_referenced_p
   since that considers something as being referenced if it is being
   partially set, while we do not.  */

void
note_uses (pbody, fun, data)
     rtx *pbody;
     void (*fun) PARAMS ((rtx *, void *));
     void *data;
{
  rtx body = *pbody;
  int i;

  switch (GET_CODE (body))
    {
    case COND_EXEC:
      (*fun) (&COND_EXEC_TEST (body), data);
      note_uses (&COND_EXEC_CODE (body), fun, data);
      return;

    case PARALLEL:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	note_uses (&XVECEXP (body, 0, i), fun, data);
      return;

    case USE:
      (*fun) (&XEXP (body, 0), data);
      return;

    case ASM_OPERANDS:
      for (i = ASM_OPERANDS_INPUT_LENGTH (body) - 1; i >= 0; i--)
	(*fun) (&ASM_OPERANDS_INPUT (body, i), data);
      return;

    case TRAP_IF:
      (*fun) (&TRAP_CONDITION (body), data);
      return;

    case PREFETCH:
      (*fun) (&XEXP (body, 0), data);
      return;

    case UNSPEC:
    case UNSPEC_VOLATILE:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	(*fun) (&XVECEXP (body, 0, i), data);
      return;

    case CLOBBER:
      if (GET_CODE (XEXP (body, 0)) == MEM)
	(*fun) (&XEXP (XEXP (body, 0), 0), data);
      return;

    case SET:
      {
	rtx dest = SET_DEST (body);

	/* For sets we replace everything in source plus registers in memory
	   expression in store and operands of a ZERO_EXTRACT.  */
	(*fun) (&SET_SRC (body), data);

	if (GET_CODE (dest) == ZERO_EXTRACT)
	  {
	    (*fun) (&XEXP (dest, 1), data);
	    (*fun) (&XEXP (dest, 2), data);
	  }

	while (GET_CODE (dest) == SUBREG || GET_CODE (dest) == STRICT_LOW_PART)
	  dest = XEXP (dest, 0);

	if (GET_CODE (dest) == MEM)
	  (*fun) (&XEXP (dest, 0), data);
      }
      return;

    default:
      /* All the other possibilities never store.  */
      (*fun) (pbody, data);
      return;
    }
}

/* Return nonzero if X's old contents don't survive after INSN.
   This will be true if X is (cc0) or if X is a register and
   X dies in INSN or because INSN entirely sets X.

   "Entirely set" means set directly and not through a SUBREG,
   ZERO_EXTRACT or SIGN_EXTRACT, so no trace of the old contents remains.
   Likewise, REG_INC does not count.

   REG may be a hard or pseudo reg.  Renumbering is not taken into account,
   but for this use that makes no difference, since regs don't overlap
   during their lifetimes.  Therefore, this function may be used
   at any time after deaths have been computed (in flow.c).

   If REG is a hard reg that occupies multiple machine registers, this
   function will only return 1 if each of those registers will be replaced
   by INSN.  */

int
dead_or_set_p (insn, x)
     rtx insn;
     rtx x;
{
  unsigned int regno, last_regno;
  unsigned int i;

  /* Can't use cc0_rtx below since this file is used by genattrtab.c.  */
  if (GET_CODE (x) == CC0)
    return 1;

  if (GET_CODE (x) != REG)
    abort ();

  regno = REGNO (x);
  last_regno = (regno >= FIRST_PSEUDO_REGISTER ? regno
		: regno + HARD_REGNO_NREGS (regno, GET_MODE (x)) - 1);

  for (i = regno; i <= last_regno; i++)
    if (! dead_or_set_regno_p (insn, i))
      return 0;

  return 1;
}

/* Utility function for dead_or_set_p to check an individual register.  Also
   called from flow.c.  */

int
dead_or_set_regno_p (insn, test_regno)
     rtx insn;
     unsigned int test_regno;
{
  unsigned int regno, endregno;
  rtx pattern;

  /* See if there is a death note for something that includes TEST_REGNO.  */
  if (find_regno_note (insn, REG_DEAD, test_regno))
    return 1;

  if (GET_CODE (insn) == CALL_INSN
      && find_regno_fusage (insn, CLOBBER, test_regno))
    return 1;

  pattern = PATTERN (insn);

  if (GET_CODE (pattern) == COND_EXEC)
    pattern = COND_EXEC_CODE (pattern);

  if (GET_CODE (pattern) == SET)
    {
      rtx dest = SET_DEST (PATTERN (insn));
 
      /* A value is totally replaced if it is the destination or the
	 destination is a SUBREG of REGNO that does not change the number of
	 words in it.  */
      if (GET_CODE (dest) == SUBREG
	  && (((GET_MODE_SIZE (GET_MODE (dest))
		+ UNITS_PER_WORD - 1) / UNITS_PER_WORD)
	      == ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest)))
		   + UNITS_PER_WORD - 1) / UNITS_PER_WORD)))
	dest = SUBREG_REG (dest);

      if (GET_CODE (dest) != REG)
	return 0;

      regno = REGNO (dest);
      endregno = (regno >= FIRST_PSEUDO_REGISTER ? regno + 1
		  : regno + HARD_REGNO_NREGS (regno, GET_MODE (dest)));

      return (test_regno >= regno && test_regno < endregno);
    }
  else if (GET_CODE (pattern) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (pattern, 0) - 1; i >= 0; i--)
	{
	  rtx body = XVECEXP (pattern, 0, i);

	  if (GET_CODE (body) == COND_EXEC)
	    body = COND_EXEC_CODE (body);

	  if (GET_CODE (body) == SET || GET_CODE (body) == CLOBBER)
	    {
	      rtx dest = SET_DEST (body);

	      if (GET_CODE (dest) == SUBREG
		  && (((GET_MODE_SIZE (GET_MODE (dest))
			+ UNITS_PER_WORD - 1) / UNITS_PER_WORD)
		      == ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest)))
			   + UNITS_PER_WORD - 1) / UNITS_PER_WORD)))
		dest = SUBREG_REG (dest);

	      if (GET_CODE (dest) != REG)
		continue;

	      regno = REGNO (dest);
	      endregno = (regno >= FIRST_PSEUDO_REGISTER ? regno + 1
			  : regno + HARD_REGNO_NREGS (regno, GET_MODE (dest)));

	      if (test_regno >= regno && test_regno < endregno)
		return 1;
	    }
	}
    }

  return 0;
}

/* Return the reg-note of kind KIND in insn INSN, if there is one.
   If DATUM is nonzero, look for one whose datum is DATUM.  */

rtx
find_reg_note (insn, kind, datum)
     rtx insn;
     enum reg_note kind;
     rtx datum;
{
  rtx link;

  /* Ignore anything that is not an INSN, JUMP_INSN or CALL_INSN.  */
  if (! INSN_P (insn))
    return 0;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == kind
	&& (datum == 0 || datum == XEXP (link, 0)))
      return link;
  return 0;
}

/* Return the reg-note of kind KIND in insn INSN which applies to register
   number REGNO, if any.  Return 0 if there is no such reg-note.  Note that
   the REGNO of this NOTE need not be REGNO if REGNO is a hard register;
   it might be the case that the note overlaps REGNO.  */

rtx
find_regno_note (insn, kind, regno)
     rtx insn;
     enum reg_note kind;
     unsigned int regno;
{
  rtx link;

  /* Ignore anything that is not an INSN, JUMP_INSN or CALL_INSN.  */
  if (! INSN_P (insn))
    return 0;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == kind
	/* Verify that it is a register, so that scratch and MEM won't cause a
	   problem here.  */
	&& GET_CODE (XEXP (link, 0)) == REG
	&& REGNO (XEXP (link, 0)) <= regno
	&& ((REGNO (XEXP (link, 0))
	     + (REGNO (XEXP (link, 0)) >= FIRST_PSEUDO_REGISTER ? 1
		: HARD_REGNO_NREGS (REGNO (XEXP (link, 0)),
				    GET_MODE (XEXP (link, 0)))))
	    > regno))
      return link;
  return 0;
}

/* Return a REG_EQUIV or REG_EQUAL note if insn has only a single set and
   has such a note.  */

rtx
find_reg_equal_equiv_note (insn)
     rtx insn;
{
  rtx note;

  if (single_set (insn) == 0)
    return 0;
  else if ((note = find_reg_note (insn, REG_EQUIV, NULL_RTX)) != 0)
    return note;
  else
    return find_reg_note (insn, REG_EQUAL, NULL_RTX);
}

/* Return true if DATUM, or any overlap of DATUM, of kind CODE is found
   in the CALL_INSN_FUNCTION_USAGE information of INSN.  */

int
find_reg_fusage (insn, code, datum)
     rtx insn;
     enum rtx_code code;
     rtx datum;
{
  /* If it's not a CALL_INSN, it can't possibly have a
     CALL_INSN_FUNCTION_USAGE field, so don't bother checking.  */
  if (GET_CODE (insn) != CALL_INSN)
    return 0;

  if (! datum)
    abort ();

  if (GET_CODE (datum) != REG)
    {
      rtx link;

      for (link = CALL_INSN_FUNCTION_USAGE (insn);
           link;
	   link = XEXP (link, 1))
        if (GET_CODE (XEXP (link, 0)) == code
	    && rtx_equal_p (datum, XEXP (XEXP (link, 0), 0)))
          return 1;
    }
  else
    {
      unsigned int regno = REGNO (datum);

      /* CALL_INSN_FUNCTION_USAGE information cannot contain references
	 to pseudo registers, so don't bother checking.  */

      if (regno < FIRST_PSEUDO_REGISTER)
        {
	  unsigned int end_regno
	    = regno + HARD_REGNO_NREGS (regno, GET_MODE (datum));
	  unsigned int i;

	  for (i = regno; i < end_regno; i++)
	    if (find_regno_fusage (insn, code, i))
	      return 1;
        }
    }

  return 0;
}

/* Return true if REGNO, or any overlap of REGNO, of kind CODE is found
   in the CALL_INSN_FUNCTION_USAGE information of INSN.  */

int
find_regno_fusage (insn, code, regno)
     rtx insn;
     enum rtx_code code;
     unsigned int regno;
{
  rtx link;

  /* CALL_INSN_FUNCTION_USAGE information cannot contain references
     to pseudo registers, so don't bother checking.  */

  if (regno >= FIRST_PSEUDO_REGISTER
      || GET_CODE (insn) != CALL_INSN )
    return 0;

  for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
    {
      unsigned int regnote;
      rtx op, reg;

      if (GET_CODE (op = XEXP (link, 0)) == code
	  && GET_CODE (reg = XEXP (op, 0)) == REG
	  && (regnote = REGNO (reg)) <= regno
	  && regnote + HARD_REGNO_NREGS (regnote, GET_MODE (reg)) > regno)
	return 1;
    }

  return 0;
}

/* Return true if INSN is a call to a pure function.  */

int
pure_call_p (insn)
     rtx insn;
{
  rtx link;

  if (GET_CODE (insn) != CALL_INSN || ! CONST_OR_PURE_CALL_P (insn))
    return 0;

  /* Look for the note that differentiates const and pure functions.  */
  for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
    {
      rtx u, m;

      if (GET_CODE (u = XEXP (link, 0)) == USE
	  && GET_CODE (m = XEXP (u, 0)) == MEM && GET_MODE (m) == BLKmode
	  && GET_CODE (XEXP (m, 0)) == SCRATCH)
	return 1;
    }

  return 0;
}

/* Remove register note NOTE from the REG_NOTES of INSN.  */

void
remove_note (insn, note)
     rtx insn;
     rtx note;
{
  rtx link;

  if (note == NULL_RTX)
    return;

  if (REG_NOTES (insn) == note)
    {
      REG_NOTES (insn) = XEXP (note, 1);
      return;
    }

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (XEXP (link, 1) == note)
      {
	XEXP (link, 1) = XEXP (note, 1);
	return;
      }

  abort ();
}

/* Search LISTP (an EXPR_LIST) for an entry whose first operand is NODE and
   return 1 if it is found.  A simple equality test is used to determine if
   NODE matches.  */

int
in_expr_list_p (listp, node)
     rtx listp;
     rtx node;
{
  rtx x;

  for (x = listp; x; x = XEXP (x, 1))
    if (node == XEXP (x, 0))
      return 1;

  return 0;
}

/* Search LISTP (an EXPR_LIST) for an entry whose first operand is NODE and
   remove that entry from the list if it is found.

   A simple equality test is used to determine if NODE matches.  */

void
remove_node_from_expr_list (node, listp)
     rtx node;
     rtx *listp;
{
  rtx temp = *listp;
  rtx prev = NULL_RTX;

  while (temp)
    {
      if (node == XEXP (temp, 0))
	{
	  /* Splice the node out of the list.  */
	  if (prev)
	    XEXP (prev, 1) = XEXP (temp, 1);
	  else
	    *listp = XEXP (temp, 1);

	  return;
	}

      prev = temp;
      temp = XEXP (temp, 1);
    }
}

/* Nonzero if X contains any volatile instructions.  These are instructions
   which may cause unpredictable machine state instructions, and thus no
   instructions should be moved or combined across them.  This includes
   only volatile asms and UNSPEC_VOLATILE instructions.  */

int
volatile_insn_p (x)
     rtx x;
{
  RTX_CODE code;

  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case CLOBBER:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case CALL:
    case MEM:
      return 0;

    case UNSPEC_VOLATILE:
 /* case TRAP_IF: This isn't clear yet.  */
      return 1;

    case ASM_INPUT:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    const char *fmt = GET_RTX_FORMAT (code);
    int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (volatile_insn_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (volatile_insn_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Nonzero if X contains any volatile memory references
   UNSPEC_VOLATILE operations or volatile ASM_OPERANDS expressions.  */

int
volatile_refs_p (x)
     rtx x;
{
  RTX_CODE code;

  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case CLOBBER:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    case CALL:
    case UNSPEC_VOLATILE:
 /* case TRAP_IF: This isn't clear yet.  */
      return 1;

    case MEM:
    case ASM_INPUT:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    const char *fmt = GET_RTX_FORMAT (code);
    int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (volatile_refs_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (volatile_refs_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Similar to above, except that it also rejects register pre- and post-
   incrementing.  */

int
side_effects_p (x)
     rtx x;
{
  RTX_CODE code;

  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    case CLOBBER:
      /* Reject CLOBBER with a non-VOID mode.  These are made by combine.c
	 when some combination can't be done.  If we see one, don't think
	 that we can simplify the expression.  */
      return (GET_MODE (x) != VOIDmode);

    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
    case CALL:
    case UNSPEC_VOLATILE:
 /* case TRAP_IF: This isn't clear yet.  */
      return 1;

    case MEM:
    case ASM_INPUT:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    const char *fmt = GET_RTX_FORMAT (code);
    int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (side_effects_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (side_effects_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Return nonzero if evaluating rtx X might cause a trap.  */

int
may_trap_p (x)
     rtx x;
{
  int i;
  enum rtx_code code;
  const char *fmt;

  if (x == 0)
    return 0;
  code = GET_CODE (x);
  switch (code)
    {
      /* Handle these cases quickly.  */
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
    case PC:
    case CC0:
    case REG:
    case SCRATCH:
      return 0;

    case ASM_INPUT:
    case UNSPEC_VOLATILE:
    case TRAP_IF:
      return 1;

    case ASM_OPERANDS:
      return MEM_VOLATILE_P (x);

      /* Memory ref can trap unless it's a static var or a stack slot.  */
    case MEM:
      return rtx_addr_can_trap_p (XEXP (x, 0));

      /* Division by a non-constant might trap.  */
    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      if (! CONSTANT_P (XEXP (x, 1))
	  || GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	return 1;
      /* This was const0_rtx, but by not using that,
	 we can link this file into other programs.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT && INTVAL (XEXP (x, 1)) == 0)
	return 1;
      break;

    case EXPR_LIST:
      /* An EXPR_LIST is used to represent a function call.  This
	 certainly may trap.  */
      return 1;

    case GE:
    case GT:
    case LE:
    case LT:
    case COMPARE:
      /* Some floating point comparisons may trap.  */
      /* ??? There is no machine independent way to check for tests that trap
	 when COMPARE is used, though many targets do make this distinction.
	 For instance, sparc uses CCFPE for compares which generate exceptions
	 and CCFP for compares which do not generate exceptions.  */
      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	return 1;
      /* But often the compare has some CC mode, so check operand
	 modes as well.  */
      if (GET_MODE_CLASS (GET_MODE (XEXP (x, 0))) == MODE_FLOAT
	  || GET_MODE_CLASS (GET_MODE (XEXP (x, 1))) == MODE_FLOAT)
	return 1;
      break;

    case NEG:
    case ABS:
      /* These operations don't trap even with floating point.  */
      break;

    default:
      /* Any floating arithmetic may trap.  */
      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	return 1;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (may_trap_p (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (may_trap_p (XVECEXP (x, i, j)))
	      return 1;
	}
    }
  return 0;
}

/* Return nonzero if X contains a comparison that is not either EQ or NE,
   i.e., an inequality.  */

int
inequality_comparisons_p (x)
     rtx x;
{
  const char *fmt;
  int len, i;
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case SCRATCH:
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      return 0;

    case LT:
    case LTU:
    case GT:
    case GTU:
    case LE:
    case LEU:
    case GE:
    case GEU:
      return 1;
      
    default:
      break;
    }

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e')
	{
	  if (inequality_comparisons_p (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (inequality_comparisons_p (XVECEXP (x, i, j)))
	      return 1;
	}
    }
	    
  return 0;
}

/* Replace any occurrence of FROM in X with TO.  The function does
   not enter into CONST_DOUBLE for the replace.

   Note that copying is not done so X must not be shared unless all copies
   are to be modified.  */

rtx
replace_rtx (x, from, to)
     rtx x, from, to;
{
  int i, j;
  const char *fmt;

  /* The following prevents loops occurrence when we change MEM in
     CONST_DOUBLE onto the same CONST_DOUBLE.  */
  if (x != 0 && GET_CODE (x) == CONST_DOUBLE)
    return x;

  if (x == from)
    return to;

  /* Allow this function to make replacements in EXPR_LISTs.  */
  if (x == 0)
    return 0;

  if (GET_CODE (x) == SUBREG)
    {
      rtx new = replace_rtx (SUBREG_REG (x), from, to);

      if (GET_CODE (new) == CONST_INT)
	{
	  x = simplify_subreg (GET_MODE (x), new,
			       GET_MODE (SUBREG_REG (x)),
			       SUBREG_BYTE (x));
	  if (! x)
	    abort ();
	}
      else
	SUBREG_REG (x) = new;

      return x;
    }
  else if (GET_CODE (x) == ZERO_EXTEND)
    {
      rtx new = replace_rtx (XEXP (x, 0), from, to);

      if (GET_CODE (new) == CONST_INT)
	{
	  x = simplify_unary_operation (ZERO_EXTEND, GET_MODE (x),
					new, GET_MODE (XEXP (x, 0)));
	  if (! x)
	    abort ();
	}
      else
	XEXP (x, 0) = new;

      return x;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = replace_rtx (XEXP (x, i), from, to);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  XVECEXP (x, i, j) = replace_rtx (XVECEXP (x, i, j), from, to);
    }

  return x;
}  

/* Throughout the rtx X, replace many registers according to REG_MAP.
   Return the replacement for X (which may be X with altered contents).
   REG_MAP[R] is the replacement for register R, or 0 for don't replace.
   NREGS is the length of REG_MAP; regs >= NREGS are not mapped.  

   We only support REG_MAP entries of REG or SUBREG.  Also, hard registers
   should not be mapped to pseudos or vice versa since validate_change
   is not called.

   If REPLACE_DEST is 1, replacements are also done in destinations;
   otherwise, only sources are replaced.  */

rtx
replace_regs (x, reg_map, nregs, replace_dest)
     rtx x;
     rtx *reg_map;
     unsigned int nregs;
     int replace_dest;
{
  enum rtx_code code;
  int i;
  const char *fmt;

  if (x == 0)
    return x;

  code = GET_CODE (x);
  switch (code)
    {
    case SCRATCH:
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return x;

    case REG:
      /* Verify that the register has an entry before trying to access it.  */
      if (REGNO (x) < nregs && reg_map[REGNO (x)] != 0)
	{
	  /* SUBREGs can't be shared.  Always return a copy to ensure that if
	     this replacement occurs more than once then each instance will
	     get distinct rtx.  */
	  if (GET_CODE (reg_map[REGNO (x)]) == SUBREG)
	    return copy_rtx (reg_map[REGNO (x)]);
	  return reg_map[REGNO (x)];
	}
      return x;

    case SUBREG:
      /* Prevent making nested SUBREGs.  */
      if (GET_CODE (SUBREG_REG (x)) == REG && REGNO (SUBREG_REG (x)) < nregs
	  && reg_map[REGNO (SUBREG_REG (x))] != 0
	  && GET_CODE (reg_map[REGNO (SUBREG_REG (x))]) == SUBREG)
	{
	  rtx map_val = reg_map[REGNO (SUBREG_REG (x))];
	  return simplify_gen_subreg (GET_MODE (x), map_val,
				      GET_MODE (SUBREG_REG (x)), 
				      SUBREG_BYTE (x));
	}
      break;

    case SET:
      if (replace_dest)
	SET_DEST (x) = replace_regs (SET_DEST (x), reg_map, nregs, 0);

      else if (GET_CODE (SET_DEST (x)) == MEM
	       || GET_CODE (SET_DEST (x)) == STRICT_LOW_PART)
	/* Even if we are not to replace destinations, replace register if it
	   is CONTAINED in destination (destination is memory or
	   STRICT_LOW_PART).  */
	XEXP (SET_DEST (x), 0) = replace_regs (XEXP (SET_DEST (x), 0),
					       reg_map, nregs, 0);
      else if (GET_CODE (SET_DEST (x)) == ZERO_EXTRACT)
	/* Similarly, for ZERO_EXTRACT we replace all operands.  */
	break;

      SET_SRC (x) = replace_regs (SET_SRC (x), reg_map, nregs, 0);
      return x;
      
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = replace_regs (XEXP (x, i), reg_map, nregs, replace_dest);
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j) = replace_regs (XVECEXP (x, i, j), reg_map,
					      nregs, replace_dest);
	}
    }
  return x;
}

/* A subroutine of computed_jump_p, return 1 if X contains a REG or MEM or
   constant that is not in the constant pool and not in the condition
   of an IF_THEN_ELSE.  */

static int
computed_jump_p_1 (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  switch (code)
    {
    case LABEL_REF:
    case PC:
      return 0;

    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case REG:
      return 1;

    case MEM:
      return ! (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
		&& CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)));

    case IF_THEN_ELSE:
      return (computed_jump_p_1 (XEXP (x, 1))
	      || computed_jump_p_1 (XEXP (x, 2)));

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e'
	  && computed_jump_p_1 (XEXP (x, i)))
	return 1;

      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (computed_jump_p_1 (XVECEXP (x, i, j)))
	    return 1;
    }

  return 0;
}

/* Return nonzero if INSN is an indirect jump (aka computed jump).

   Tablejumps and casesi insns are not considered indirect jumps;
   we can recognize them by a (use (label_ref)).  */

int
computed_jump_p (insn)
     rtx insn;
{
  int i;
  if (GET_CODE (insn) == JUMP_INSN)
    {
      rtx pat = PATTERN (insn);

      if (find_reg_note (insn, REG_LABEL, NULL_RTX))
	return 0;
      else if (GET_CODE (pat) == PARALLEL)
	{
	  int len = XVECLEN (pat, 0);
	  int has_use_labelref = 0;

	  for (i = len - 1; i >= 0; i--)
	    if (GET_CODE (XVECEXP (pat, 0, i)) == USE
		&& (GET_CODE (XEXP (XVECEXP (pat, 0, i), 0))
		    == LABEL_REF))
	      has_use_labelref = 1;

	  if (! has_use_labelref)
	    for (i = len - 1; i >= 0; i--)
	      if (GET_CODE (XVECEXP (pat, 0, i)) == SET
		  && SET_DEST (XVECEXP (pat, 0, i)) == pc_rtx
		  && computed_jump_p_1 (SET_SRC (XVECEXP (pat, 0, i))))
		return 1;
	}
      else if (GET_CODE (pat) == SET
	       && SET_DEST (pat) == pc_rtx
	       && computed_jump_p_1 (SET_SRC (pat)))
	return 1;
    }
  return 0;
}

/* Traverse X via depth-first search, calling F for each
   sub-expression (including X itself).  F is also passed the DATA.
   If F returns -1, do not traverse sub-expressions, but continue
   traversing the rest of the tree.  If F ever returns any other
   non-zero value, stop the traversal, and return the value returned
   by F.  Otherwise, return 0.  This function does not traverse inside
   tree structure that contains RTX_EXPRs, or into sub-expressions
   whose format code is `0' since it is not known whether or not those
   codes are actually RTL.

   This routine is very general, and could (should?) be used to
   implement many of the other routines in this file.  */

int
for_each_rtx (x, f, data)
     rtx *x;
     rtx_function f;
     void *data;
{
  int result;
  int length;
  const char *format;
  int i;

  /* Call F on X.  */
  result = (*f) (x, data);
  if (result == -1)
    /* Do not traverse sub-expressions.  */
    return 0;
  else if (result != 0)
    /* Stop the traversal.  */
    return result;

  if (*x == NULL_RTX)
    /* There are no sub-expressions.  */
    return 0;

  length = GET_RTX_LENGTH (GET_CODE (*x));
  format = GET_RTX_FORMAT (GET_CODE (*x));

  for (i = 0; i < length; ++i) 
    {
      switch (format[i]) 
	{
	case 'e':
	  result = for_each_rtx (&XEXP (*x, i), f, data);
	  if (result != 0)
	    return result;
	  break;

	case 'V':
	case 'E':
	  if (XVEC (*x, i) != 0) 
	    {
	      int j;
	      for (j = 0; j < XVECLEN (*x, i); ++j)
		{
		  result = for_each_rtx (&XVECEXP (*x, i, j), f, data);
		  if (result != 0)
		    return result;
		}
	    }
	  break; 

	default:
	  /* Nothing to do.  */
	  break;
	}

    }

  return 0;
}

/* Searches X for any reference to REGNO, returning the rtx of the
   reference found if any.  Otherwise, returns NULL_RTX.  */

rtx
regno_use_in (regno, x)
     unsigned int regno;
     rtx x;
{
  const char *fmt;
  int i, j;
  rtx tem;

  if (GET_CODE (x) == REG && REGNO (x) == regno)
    return x;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if ((tem = regno_use_in (regno, XEXP (x, i))))
	    return tem;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if ((tem = regno_use_in (regno , XVECEXP (x, i, j))))
	    return tem;
    }

  return NULL_RTX;
}

/* Return a value indicating whether OP, an operand of a commutative
   operation, is preferred as the first or second operand.  The higher
   the value, the stronger the preference for being the first operand.
   We use negative values to indicate a preference for the first operand
   and positive values for the second operand.  */

int
commutative_operand_precedence (op)
     rtx op;
{
  /* Constants always come the second operand.  Prefer "nice" constants.  */
  if (GET_CODE (op) == CONST_INT)
    return -5;
  if (GET_CODE (op) == CONST_DOUBLE)
    return -4;
  if (CONSTANT_P (op))
    return -3;

  /* SUBREGs of objects should come second.  */
  if (GET_CODE (op) == SUBREG
      && GET_RTX_CLASS (GET_CODE (SUBREG_REG (op))) == 'o')
    return -2;

  /* If only one operand is a `neg', `not',
    `mult', `plus', or `minus' expression, it will be the first
    operand.  */
  if (GET_CODE (op) == NEG || GET_CODE (op) == NOT
      || GET_CODE (op) == MULT || GET_CODE (op) == PLUS
      || GET_CODE (op) == MINUS)
    return 2;

  /* Complex expressions should be the first, so decrease priority
     of objects.  */
  if (GET_RTX_CLASS (GET_CODE (op)) == 'o')
    return -1;
  return 0;
}

/* Return 1 iff it is necessary to swap operands of commutative operation
   in order to canonicalize expression.  */

int
swap_commutative_operands_p (x, y)
     rtx x, y;
{
  return (commutative_operand_precedence (x)
	  < commutative_operand_precedence (y));
}

/* Return 1 if X is an autoincrement side effect and the register is
   not the stack pointer.  */
int
auto_inc_p (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case PRE_INC:
    case POST_INC:
    case PRE_DEC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      /* There are no REG_INC notes for SP.  */
      if (XEXP (x, 0) != stack_pointer_rtx)
	return 1;
    default:
      break;
    }
  return 0;
}

/* Return 1 if the sequence of instructions beginning with FROM and up
   to and including TO is safe to move.  If NEW_TO is non-NULL, and
   the sequence is not already safe to move, but can be easily
   extended to a sequence which is safe, then NEW_TO will point to the
   end of the extended sequence.  
 
   For now, this function only checks that the region contains whole
   exception regions, but it could be extended to check additional
   conditions as well.  */

int
insns_safe_to_move_p (from, to, new_to)
     rtx from;
     rtx to;
     rtx *new_to;
{
  int eh_region_count = 0;
  int past_to_p = 0;
  rtx r = from;

  /* By default, assume the end of the region will be what was
     suggested.  */
  if (new_to)
    *new_to = to;

  while (r)
    {
      if (GET_CODE (r) == NOTE)
	{
	  switch (NOTE_LINE_NUMBER (r))
	    {
	    case NOTE_INSN_EH_REGION_BEG:
	      ++eh_region_count;
	      break;

	    case NOTE_INSN_EH_REGION_END:
	      if (eh_region_count == 0)
		/* This sequence of instructions contains the end of
		   an exception region, but not he beginning.  Moving
		   it will cause chaos.  */
		return 0;

	      --eh_region_count;
	      break;

	    default:
	      break;
	    }
	}
      else if (past_to_p)
	/* If we've passed TO, and we see a non-note instruction, we
	   can't extend the sequence to a movable sequence.  */
	return 0;

      if (r == to)
	{
	  if (!new_to)
	    /* It's OK to move the sequence if there were matched sets of
	       exception region notes.  */
	    return eh_region_count == 0;
	  
	  past_to_p = 1;
	}

      /* It's OK to move the sequence if there were matched sets of
	 exception region notes.  */
      if (past_to_p && eh_region_count == 0)
	{
	  *new_to = r;
	  return 1;
	}

      /* Go to the next instruction.  */
      r = NEXT_INSN (r);
    }
  
  return 0;
}

/* Return non-zero if IN contains a piece of rtl that has the address LOC */
int
loc_mentioned_in_p (loc, in)
     rtx *loc, in;
{
  enum rtx_code code = GET_CODE (in);
  const char *fmt = GET_RTX_FORMAT (code);
  int i, j;

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (loc == &in->fld[i].rtx)
	return 1;
      if (fmt[i] == 'e')
	{
	  if (loc_mentioned_in_p (loc, XEXP (in, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	  if (loc_mentioned_in_p (loc, XVECEXP (in, i, j)))
	    return 1;
    }
  return 0;
}

/* Given a subreg X, return the bit offset where the subreg begins
   (counting from the least significant bit of the reg).  */

unsigned int
subreg_lsb (x)
     rtx x;
{
  enum machine_mode inner_mode = GET_MODE (SUBREG_REG (x));
  enum machine_mode mode = GET_MODE (x);
  unsigned int bitpos;
  unsigned int byte;
  unsigned int word;

  /* A paradoxical subreg begins at bit position 0.  */
  if (GET_MODE_BITSIZE (mode) > GET_MODE_BITSIZE (inner_mode))
    return 0;

  if (WORDS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
    /* If the subreg crosses a word boundary ensure that
       it also begins and ends on a word boundary.  */
    if ((SUBREG_BYTE (x) % UNITS_PER_WORD
	 + GET_MODE_SIZE (mode)) > UNITS_PER_WORD
	&& (SUBREG_BYTE (x) % UNITS_PER_WORD
	    || GET_MODE_SIZE (mode) % UNITS_PER_WORD))
	abort ();

  if (WORDS_BIG_ENDIAN)
    word = (GET_MODE_SIZE (inner_mode)
	    - (SUBREG_BYTE (x) + GET_MODE_SIZE (mode))) / UNITS_PER_WORD;
  else
    word = SUBREG_BYTE (x) / UNITS_PER_WORD;
  bitpos = word * BITS_PER_WORD;

  if (BYTES_BIG_ENDIAN)
    byte = (GET_MODE_SIZE (inner_mode)
	    - (SUBREG_BYTE (x) + GET_MODE_SIZE (mode))) % UNITS_PER_WORD;
  else
    byte = SUBREG_BYTE (x) % UNITS_PER_WORD;
  bitpos += byte * BITS_PER_UNIT;

  return bitpos;
}

/* This function returns the regno offset of a subreg expression.
   xregno - A regno of an inner hard subreg_reg (or what will become one).
   xmode  - The mode of xregno.
   offset - The byte offset.
   ymode  - The mode of a top level SUBREG (or what may become one).
   RETURN - The regno offset which would be used.  */
unsigned int
subreg_regno_offset (xregno, xmode, offset, ymode)
     unsigned int xregno;
     enum machine_mode xmode;
     unsigned int offset;
     enum machine_mode ymode;
{
  int nregs_xmode, nregs_ymode;
  int mode_multiple, nregs_multiple;
  int y_offset;

  if (xregno >= FIRST_PSEUDO_REGISTER)
    abort ();

  nregs_xmode = HARD_REGNO_NREGS (xregno, xmode);
  nregs_ymode = HARD_REGNO_NREGS (xregno, ymode);
  if (offset == 0 || nregs_xmode == nregs_ymode)
    return 0;
  
  /* size of ymode must not be greater than the size of xmode.  */
  mode_multiple = GET_MODE_SIZE (xmode) / GET_MODE_SIZE (ymode);
  if (mode_multiple == 0)
    abort ();

  y_offset = offset / GET_MODE_SIZE (ymode);
  nregs_multiple =  nregs_xmode / nregs_ymode;
  return (y_offset / (mode_multiple / nregs_multiple)) * nregs_ymode;
}

/* Return the final regno that a subreg expression refers to.  */
unsigned int 
subreg_regno (x)
     rtx x;
{
  unsigned int ret;
  rtx subreg = SUBREG_REG (x);
  int regno = REGNO (subreg);

  ret = regno + subreg_regno_offset (regno, 
				     GET_MODE (subreg), 
				     SUBREG_BYTE (x),
				     GET_MODE (x));
  return ret;

}
struct parms_set_data
{
  int nregs;
  HARD_REG_SET regs;
};

/* Helper function for noticing stores to parameter registers.  */
static void
parms_set (x, pat, data)
	rtx x, pat ATTRIBUTE_UNUSED;
	void *data;
{
  struct parms_set_data *d = data;
  if (REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER
      && TEST_HARD_REG_BIT (d->regs, REGNO (x)))
    {
      CLEAR_HARD_REG_BIT (d->regs, REGNO (x));
      d->nregs--;
    }
}

/* Look backward for first parameter to be loaded.  
   Do not skip BOUNDARY.  */
rtx
find_first_parameter_load (call_insn, boundary)
     rtx call_insn, boundary;
{
  struct parms_set_data parm;
  rtx p, before;

  /* Since different machines initialize their parameter registers
     in different orders, assume nothing.  Collect the set of all
     parameter registers.  */
  CLEAR_HARD_REG_SET (parm.regs);
  parm.nregs = 0;
  for (p = CALL_INSN_FUNCTION_USAGE (call_insn); p; p = XEXP (p, 1))
    if (GET_CODE (XEXP (p, 0)) == USE
	&& GET_CODE (XEXP (XEXP (p, 0), 0)) == REG)
      {
	if (REGNO (XEXP (XEXP (p, 0), 0)) >= FIRST_PSEUDO_REGISTER)
	  abort ();

	/* We only care about registers which can hold function
	   arguments.  */
	if (!FUNCTION_ARG_REGNO_P (REGNO (XEXP (XEXP (p, 0), 0))))
	  continue;

	SET_HARD_REG_BIT (parm.regs, REGNO (XEXP (XEXP (p, 0), 0)));
	parm.nregs++;
      }
  before = call_insn;

  /* Search backward for the first set of a register in this set.  */
  while (parm.nregs && before != boundary)
    {
      before = PREV_INSN (before);

      /* It is possible that some loads got CSEed from one call to
         another.  Stop in that case.  */
      if (GET_CODE (before) == CALL_INSN)
	break;

      /* Our caller needs either ensure that we will find all sets
         (in case code has not been optimized yet), or take care
         for possible labels in a way by setting boundary to preceding
         CODE_LABEL.  */
      if (GET_CODE (before) == CODE_LABEL)
	{
	  if (before != boundary)
	    abort ();
	  break;
	}

      if (INSN_P (before))
        note_stores (PATTERN (before), parms_set, &parm);
    }
  return before;
}
