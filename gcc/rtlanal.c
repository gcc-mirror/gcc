/* Analyze RTL for C-Compiler
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "rtl.h"

static int rtx_addr_can_trap_p	PARAMS ((rtx));
static void reg_set_p_1		PARAMS ((rtx, rtx, void *));
static void reg_set_last_1	PARAMS ((rtx, rtx, void *));


/* Forward declarations */
static int jmp_uses_reg_or_mem		PARAMS ((rtx));

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
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register const char *fmt;

  if (code == MEM)
    return ! RTX_UNCHANGING_P (x);

  if (code == QUEUED)
    return 1;

  if (code == CONST || code == CONST_INT)
    return 0;

  if (code == REG)
    return ! (REGNO (x) == FRAME_POINTER_REGNUM
	      || REGNO (x) == HARD_FRAME_POINTER_REGNUM
	      || REGNO (x) == ARG_POINTER_REGNUM
	      || RTX_UNCHANGING_P (x));

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      if (rtx_unstable_p (XEXP (x, i)))
	return 1;
  return 0;
}

/* Return 1 if X has a value that can vary even between two
   executions of the program.  0 means X can be compared reliably
   against certain constants or near-constants.
   The frame pointer and the arg pointer are considered constant.  */

int
rtx_varies_p (x)
     rtx x;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register const char *fmt;

  switch (code)
    {
    case MEM:
    case QUEUED:
      return 1;

    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case REG:
      /* Note that we have to test for the actual rtx used for the frame
	 and arg pointers and not just the register number in case we have
	 eliminated the frame and/or arg pointer and are using it
	 for pseudos.  */
      return ! (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
		|| x == arg_pointer_rtx || x == pic_offset_table_rtx);

    case LO_SUM:
      /* The operand 0 of a LO_SUM is considered constant
	 (in fact is it related specifically to operand 1).  */
      return rtx_varies_p (XEXP (x, 1));
      
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      if (rtx_varies_p (XEXP (x, i)))
	return 1;
  return 0;
}

/* Return 0 if the use of X as an address in a MEM can cause a trap.  */

static int
rtx_addr_can_trap_p (x)
     register rtx x;
{
  register enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case SYMBOL_REF:
    case LABEL_REF:
      /* SYMBOL_REF is problematic due to the possible presence of
	 a #pragma weak, but to say that loads from symbols can trap is
	 *very* costly.  It's not at all clear what's best here.  For
	 now, we ignore the impact of #pragma weak.  */
      return 0;

    case REG:
      /* As in rtx_varies_p, we have to use the actual rtx, not reg number.  */
      return ! (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
		|| x == stack_pointer_rtx || x == arg_pointer_rtx);

    case CONST:
      return rtx_addr_can_trap_p (XEXP (x, 0));

    case PLUS:
      /* An address is assumed not to trap if it is an address that can't
	 trap plus a constant integer.  */
      return (rtx_addr_can_trap_p (XEXP (x, 0))
	      || GET_CODE (XEXP (x, 1)) != CONST_INT);

    case LO_SUM:
      return rtx_addr_can_trap_p (XEXP (x, 1));
      
    default:
      break;
    }

  /* If it isn't one of the case above, it can cause a trap.  */
  return 1;
}

/* Return 1 if X refers to a memory location whose address 
   cannot be compared reliably with constant addresses,
   or if X refers to a BLKmode memory object.  */

int
rtx_addr_varies_p (x)
     rtx x;
{
  register enum rtx_code code;
  register int i;
  register const char *fmt;

  if (x == 0)
    return 0;

  code = GET_CODE (x);
  if (code == MEM)
    return GET_MODE (x) == BLKmode || rtx_varies_p (XEXP (x, 0));

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (rtx_addr_varies_p (XEXP (x, i)))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (rtx_addr_varies_p (XVECEXP (x, i, j)))
	    return 1;
      }
  return 0;
}

/* Return the value of the integer term in X, if one is apparent;
   otherwise return 0.
   Only obvious integer terms are detected.
   This is used in cse.c with the `related_value' field.*/

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

/* Nonzero if register REG appears somewhere within IN.
   Also works if REG is not a register; in this case it checks
   for a subexpression of IN that is Lisp "equal" to REG.  */

int
reg_mentioned_p (reg, in)
     register rtx reg, in;
{
  register const char *fmt;
  register int i;
  register enum rtx_code code;

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
	  register int j;
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
  register rtx p;
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
  register rtx p;
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
  register rtx insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
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
  register rtx insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
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
  register rtx insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	&& reg_set_p (reg, insn))
      return 1;
  return 0;
}

/* Internals of reg_set_between_p.  */

static rtx reg_set_reg;
static int reg_set_flag;

static void
reg_set_p_1 (x, pat, data)
     rtx x;
     rtx pat ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
{
  /* We don't want to return 1 if X is a MEM that contains a register
     within REG_SET_REG.  */

  if ((GET_CODE (x) != MEM)
      && reg_overlap_mentioned_p (reg_set_reg, x))
    reg_set_flag = 1;
}

int
reg_set_p (reg, insn)
     rtx reg, insn;
{
  rtx body = insn;

  /* We can be passed an insn or part of one.  If we are passed an insn,
     check if a side-effect of the insn clobbers REG.  */
  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
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

  reg_set_reg = reg;
  reg_set_flag = 0;
  note_stores (body, reg_set_p_1, NULL);
  return reg_set_flag;
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

/* Given an INSN, return a SET expression if this insn has only a single SET.
   It may also have CLOBBERs, USEs, or SET whose output
   will not be used, which we ignore.  */

rtx
single_set (insn)
     rtx insn;
{
  rtx set;
  int i;
  
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;

  if (GET_CODE (PATTERN (insn)) == SET)
    return PATTERN (insn);
  
  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      for (i = 0, set = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	{
	  rtx sub = XVECEXP (PATTERN (insn), 0, i);

	  switch (GET_CODE (sub))
	    {
	    case USE:
	    case CLOBBER:
	      break;

	    case SET:
	      if (! find_reg_note (insn, REG_UNUSED, SET_DEST (sub))
		  || side_effects_p (sub))
		{
		  if (set)
		    return 0;
		  else
		    set = sub;
		}
	      break;

	    default:
	      return 0;
	    }
	}
      return set;
    }
  
  return 0;
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
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
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
    if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
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
	  unsigned int inner_regno = REGNO (SUBREG_REG (x)) + SUBREG_WORD (x);
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
	  register int j;
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
	regno += SUBREG_WORD (x);
      goto do_reg;

    case REG:
      regno = REGNO (x);
    do_reg:
      endregno = regno + (regno < FIRST_PSEUDO_REGISTER
			  ? HARD_REGNO_NREGS (regno, GET_MODE (x)) : 1);
      return refers_to_regno_p (regno, endregno, in, NULL_PTR);

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
      if (GET_MODE (x) == BLKmode)
	{
	  register int i;

	  /* If any register in here refers to it we return true.  */
	  for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	    if (reg_overlap_mentioned_p (SET_DEST (XVECEXP (x, 0, i)), in))
	      return 1;
	  return 0;
	}
      break;

    default:
      break;
    }

  abort ();
}

/* Used for communications between the next few functions.  */

static int reg_set_last_unknown;
static rtx reg_set_last_value;
static unsigned int reg_set_last_first_regno, reg_set_last_last_regno;

/* Called via note_stores from reg_set_last.  */

static void
reg_set_last_1 (x, pat, data)
     rtx x;
     rtx pat;
     void *data ATTRIBUTE_UNUSED;
{
  unsigned int first, last;

  /* If X is not a register, or is not one in the range we care
     about, ignore.  */
  if (GET_CODE (x) != REG)
    return;

  first = REGNO (x);
  last = first + (first < FIRST_PSEUDO_REGISTER
		  ? HARD_REGNO_NREGS (first, GET_MODE (x)) : 1);

  if (first >= reg_set_last_last_regno
      || last <= reg_set_last_first_regno)
    return;

  /* If this is a CLOBBER or is some complex LHS, or doesn't modify
     exactly the registers we care about, show we don't know the value.  */
  if (GET_CODE (pat) == CLOBBER || SET_DEST (pat) != x
      || first != reg_set_last_first_regno
      || last != reg_set_last_last_regno)
    reg_set_last_unknown = 1;
  else
    reg_set_last_value = SET_SRC (pat);
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

  reg_set_last_first_regno = REGNO (x);

  reg_set_last_last_regno
    = reg_set_last_first_regno
      + (reg_set_last_first_regno < FIRST_PSEUDO_REGISTER
	 ? HARD_REGNO_NREGS (reg_set_last_first_regno, GET_MODE (x)) : 1);

  reg_set_last_unknown = 0;
  reg_set_last_value = 0;

  /* Scan backwards until reg_set_last_1 changed one of the above flags.
     Stop when we reach a label or X is a hard reg and we reach a
     CALL_INSN (if reg_set_last_last_regno is a hard reg).

     If we find a set of X, ensure that its SET_SRC remains unchanged.  */

  /* We compare with <= here, because reg_set_last_last_regno
     is actually the number of the first reg *not* in X.  */
  for (;
       insn && GET_CODE (insn) != CODE_LABEL
       && ! (GET_CODE (insn) == CALL_INSN
	     && reg_set_last_last_regno <= FIRST_PSEUDO_REGISTER);
       insn = PREV_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      {
	note_stores (PATTERN (insn), reg_set_last_1, NULL);
	if (reg_set_last_unknown)
	  return 0;
	else if (reg_set_last_value)
	  {
	    if (CONSTANT_P (reg_set_last_value)
		|| ((GET_CODE (reg_set_last_value) == REG
		     || GET_CODE (reg_set_last_value) == SUBREG)
		    && ! reg_set_between_p (reg_set_last_value,
					    insn, orig_insn)))
	      return reg_set_last_value;
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
     register rtx x;
     void (*fun) PARAMS ((rtx, rtx, void *));
     void *data;
{
  if (GET_CODE (x) == COND_EXEC)
    x = COND_EXEC_CODE (x);
  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
    {
      register rtx dest = SET_DEST (x);
      while ((GET_CODE (dest) == SUBREG
	      && (GET_CODE (SUBREG_REG (dest)) != REG
		  || REGNO (SUBREG_REG (dest)) >= FIRST_PSEUDO_REGISTER))
	     || GET_CODE (dest) == ZERO_EXTRACT
	     || GET_CODE (dest) == SIGN_EXTRACT
	     || GET_CODE (dest) == STRICT_LOW_PART)
	dest = XEXP (dest, 0);

      if (GET_CODE (dest) == PARALLEL
	  && GET_MODE (dest) == BLKmode)
	{
	  register int i;
	  for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	    (*fun) (SET_DEST (XVECEXP (dest, 0, i)), x, data);
	}
      else
	(*fun) (dest, x, data);
    }
  else if (GET_CODE (x) == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  register rtx y = XVECEXP (x, 0, i);
	  if (GET_CODE (y) == COND_EXEC)
	    y = COND_EXEC_CODE (y);
	  if (GET_CODE (y) == SET || GET_CODE (y) == CLOBBER)
	    {
	      register rtx dest = SET_DEST (y);
	      while ((GET_CODE (dest) == SUBREG
		      && (GET_CODE (SUBREG_REG (dest)) != REG
			  || (REGNO (SUBREG_REG (dest))
			      >= FIRST_PSEUDO_REGISTER)))
		     || GET_CODE (dest) == ZERO_EXTRACT
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == STRICT_LOW_PART)
		dest = XEXP (dest, 0);
	      if (GET_CODE (dest) == PARALLEL
		  && GET_MODE (dest) == BLKmode)
		{
		  register int i;

		  for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
		    (*fun) (SET_DEST (XVECEXP (dest, 0, i)), y, data);
		}
	      else
		(*fun) (dest, y, data);
	    }
	}
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
  rtx link, pattern;

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
      register int i;

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
  register rtx link;

  /* Ignore anything that is not an INSN, JUMP_INSN or CALL_INSN.  */
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
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
  register rtx link;

  /* Ignore anything that is not an INSN, JUMP_INSN or CALL_INSN.  */
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
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
    abort();

  if (GET_CODE (datum) != REG)
    {
      register rtx link;

      for (link = CALL_INSN_FUNCTION_USAGE (insn);
           link;
	   link = XEXP (link, 1))
        if (GET_CODE (XEXP (link, 0)) == code
	    && rtx_equal_p (datum, SET_DEST (XEXP (link, 0))))
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
  register rtx link;

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

/* Remove register note NOTE from the REG_NOTES of INSN.  */

void
remove_note (insn, note)
     register rtx insn;
     register rtx note;
{
  register rtx link;

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

/* Search LISTP (an EXPR_LIST) for NODE and remove NODE from the list
   if it is found.

   A simple equality test is used to determine if NODE is on the
   EXPR_LIST.  */

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
  register RTX_CODE code;

  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case CALL:
    case MEM:
      return 0;

    case UNSPEC_VOLATILE:
 /* case TRAP_IF: This isn't clear yet.  */
      return 1;

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register const char *fmt = GET_RTX_FORMAT (code);
    register int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (volatile_insn_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    register int j;
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
  register RTX_CODE code;

  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    case CALL:
    case UNSPEC_VOLATILE:
 /* case TRAP_IF: This isn't clear yet.  */
      return 1;

    case MEM:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register const char *fmt = GET_RTX_FORMAT (code);
    register int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (volatile_refs_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    register int j;
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
  register RTX_CODE code;

  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case ASM_INPUT:
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
    case CALL:
    case UNSPEC_VOLATILE:
 /* case TRAP_IF: This isn't clear yet.  */
      return 1;

    case MEM:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register const char *fmt = GET_RTX_FORMAT (code);
    register int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (side_effects_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    register int j;
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
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
    case PC:
    case CC0:
    case REG:
    case SCRATCH:
      return 0;

      /* Conditional trap can trap!  */
    case UNSPEC_VOLATILE:
    case TRAP_IF:
      return 1;

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
	  register int j;
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
  register const char *fmt;
  register int len, i;
  register enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case SCRATCH:
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
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
	  register int j;
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
  register int i, j;
  register const char *fmt;

  /* The following prevents loops occurrence when we change MEM in
     CONST_DOUBLE onto the same CONST_DOUBLE. */
  if (x != 0 && GET_CODE (x) == CONST_DOUBLE)
    return x;

  if (x == from)
    return to;

  /* Allow this function to make replacements in EXPR_LISTs.  */
  if (x == 0)
    return 0;

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
  register enum rtx_code code;
  register int i;
  register const char *fmt;

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
	  rtx map_inner = SUBREG_REG (map_val);

	  if (GET_MODE (x) == GET_MODE (map_inner))
	    return map_inner;
	  else
	    {
	      /* We cannot call gen_rtx here since we may be linked with
		 genattrtab.c.  */
	      /* Let's try clobbering the incoming SUBREG and see
		 if this is really safe.  */
	      SUBREG_REG (x) = map_inner;
	      SUBREG_WORD (x) += SUBREG_WORD (map_val);
	      return x;
#if 0
	      rtx new = rtx_alloc (SUBREG);
	      PUT_MODE (new, GET_MODE (x));
	      SUBREG_REG (new) = map_inner;
	      SUBREG_WORD (new) = SUBREG_WORD (x) + SUBREG_WORD (map_val);
#endif
	    }
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
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j) = replace_regs (XVECEXP (x, i, j), reg_map,
					      nregs, replace_dest);
	}
    }
  return x;
}

/* Return 1 if X, the SRC_SRC of  SET of (pc) contain a REG or MEM that is
   not in the constant pool and not in the condition of an IF_THEN_ELSE.  */

static int
jmp_uses_reg_or_mem (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  switch (code)
    {
    case CONST:
    case LABEL_REF:
    case PC:
      return 0;

    case REG:
      return 1;

    case MEM:
      return ! (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
		&& CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)));

    case IF_THEN_ELSE:
      return (jmp_uses_reg_or_mem (XEXP (x, 1))
	      || jmp_uses_reg_or_mem (XEXP (x, 2)));

    case PLUS:  case MINUS:  case MULT:
      return (jmp_uses_reg_or_mem (XEXP (x, 0))
	      || jmp_uses_reg_or_mem (XEXP (x, 1)));

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e'
	  && jmp_uses_reg_or_mem (XEXP (x, i)))
	return 1;

      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (jmp_uses_reg_or_mem (XVECEXP (x, i, j)))
	    return 1;
    }

  return 0;
}

/* Return nonzero if INSN is an indirect jump (aka computed jump).

   Tablejumps and casesi insns are not considered indirect jumps;
   we can recognize them by a (use (lael_ref)).  */

int
computed_jump_p (insn)
     rtx insn;
{
  int i;
  if (GET_CODE (insn) == JUMP_INSN)
    {
      rtx pat = PATTERN (insn);

      if (GET_CODE (pat) == PARALLEL)
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
		  && jmp_uses_reg_or_mem (SET_SRC (XVECEXP (pat, 0, i))))
		return 1;
	}
      else if (GET_CODE (pat) == SET
	       && SET_DEST (pat) == pc_rtx
	       && jmp_uses_reg_or_mem (SET_SRC (pat)))
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
  const char* format;
  int i;

  /* Call F on X.  */
  result = (*f)(x, data);
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
  register const char *fmt;
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
   exception regiongs, but it could be extended to check additional
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
