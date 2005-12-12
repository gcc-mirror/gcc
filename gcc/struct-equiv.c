/* Control flow optimization code for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* This file contains helper functions for Cross jumping (tail merging).  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "output.h"
#include "insn-config.h"
#include "flags.h"
#include "recog.h"
#include "tm_p.h"
#include "target.h"
#include "emit-rtl.h"

static void merge_memattrs (rtx, rtx);



/* Removes the memory attributes of MEM expression
   if they are not equal.  */

void
merge_memattrs (rtx x, rtx y)
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;

  if (x == y)
    return;
  if (x == 0 || y == 0)
    return;

  code = GET_CODE (x);

  if (code != GET_CODE (y))
    return;

  if (GET_MODE (x) != GET_MODE (y))
    return;

  if (code == MEM && MEM_ATTRS (x) != MEM_ATTRS (y))
    {
      if (! MEM_ATTRS (x))
	MEM_ATTRS (y) = 0;
      else if (! MEM_ATTRS (y))
	MEM_ATTRS (x) = 0;
      else
	{
	  rtx mem_size;

	  if (MEM_ALIAS_SET (x) != MEM_ALIAS_SET (y))
	    {
	      set_mem_alias_set (x, 0);
	      set_mem_alias_set (y, 0);
	    }

	  if (! mem_expr_equal_p (MEM_EXPR (x), MEM_EXPR (y)))
	    {
	      set_mem_expr (x, 0);
	      set_mem_expr (y, 0);
	      set_mem_offset (x, 0);
	      set_mem_offset (y, 0);
	    }
	  else if (MEM_OFFSET (x) != MEM_OFFSET (y))
	    {
	      set_mem_offset (x, 0);
	      set_mem_offset (y, 0);
	    }

	  if (!MEM_SIZE (x))
	    mem_size = NULL_RTX;
	  else if (!MEM_SIZE (y))
	    mem_size = NULL_RTX;
	  else
	    mem_size = GEN_INT (MAX (INTVAL (MEM_SIZE (x)),
				     INTVAL (MEM_SIZE (y))));
	  set_mem_size (x, mem_size);
	  set_mem_size (y, mem_size);

	  set_mem_align (x, MIN (MEM_ALIGN (x), MEM_ALIGN (y)));
	  set_mem_align (y, MEM_ALIGN (x));
	}
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return;

	  for (j = 0; j < XVECLEN (x, i); j++)
	    merge_memattrs (XVECEXP (x, i, j), XVECEXP (y, i, j));

	  break;

	case 'e':
	  merge_memattrs (XEXP (x, i), XEXP (y, i));
	}
    }
  return;
}

/* Check if the set of REG_DEAD notes attached to I1 and I2 allows us to
   go ahead with merging I1 and I2, which otherwise look fine.  */
static bool
death_notes_match_p (rtx i1 ATTRIBUTE_UNUSED, rtx i2 ATTRIBUTE_UNUSED,
		     int mode ATTRIBUTE_UNUSED)
{
#ifdef STACK_REGS
  /* If cross_jump_death_matters is not 0, the insn's mode
     indicates whether or not the insn contains any stack-like
     regs.  */

  if ((mode & CLEANUP_POST_REGSTACK) && stack_regs_mentioned (i1))
    {
      /* If register stack conversion has already been done, then
	 death notes must also be compared before it is certain that
	 the two instruction streams match.  */

      rtx note;
      HARD_REG_SET i1_regset, i2_regset;

      CLEAR_HARD_REG_SET (i1_regset);
      CLEAR_HARD_REG_SET (i2_regset);

      for (note = REG_NOTES (i1); note; note = XEXP (note, 1))
	if (REG_NOTE_KIND (note) == REG_DEAD && STACK_REG_P (XEXP (note, 0)))
	  SET_HARD_REG_BIT (i1_regset, REGNO (XEXP (note, 0)));

      for (note = REG_NOTES (i2); note; note = XEXP (note, 1))
	if (REG_NOTE_KIND (note) == REG_DEAD && STACK_REG_P (XEXP (note, 0)))
	  SET_HARD_REG_BIT (i2_regset, REGNO (XEXP (note, 0)));

      GO_IF_HARD_REG_EQUAL (i1_regset, i2_regset, done);

      return false;

    done:
      ;
    }
#endif
  return true;
}

/* Return true if I1 and I2 are equivalent and thus can be crossjumped.  */

bool
insns_match_p (int mode, rtx i1, rtx i2)
{
  rtx p1, p2;

  /* Verify that I1 and I2 are equivalent.  */
  if (GET_CODE (i1) != GET_CODE (i2))
    return false;

  p1 = PATTERN (i1);
  p2 = PATTERN (i2);

  if (GET_CODE (p1) != GET_CODE (p2))
    return false;

  /* If this is a CALL_INSN, compare register usage information.
     If we don't check this on stack register machines, the two
     CALL_INSNs might be merged leaving reg-stack.c with mismatching
     numbers of stack registers in the same basic block.
     If we don't check this on machines with delay slots, a delay slot may
     be filled that clobbers a parameter expected by the subroutine.

     ??? We take the simple route for now and assume that if they're
     equal, they were constructed identically.  */

  if (CALL_P (i1)
      && (!rtx_equal_p (CALL_INSN_FUNCTION_USAGE (i1),
			CALL_INSN_FUNCTION_USAGE (i2))
	  || SIBLING_CALL_P (i1) != SIBLING_CALL_P (i2)))
    return false;

  if (!death_notes_match_p (i1, i2, mode))
    return false;

  if (reload_completed
      ? rtx_renumbered_equal_p (p1, p2) : rtx_equal_p (p1, p2))
    return true;

  /* Do not do EQUIV substitution after reload.  First, we're undoing the
     work of reload_cse.  Second, we may be undoing the work of the post-
     reload splitting pass.  */
  /* ??? Possibly add a new phase switch variable that can be used by
     targets to disallow the troublesome insns after splitting.  */
  if (!reload_completed)
    {
      /* The following code helps take care of G++ cleanups.  */
      rtx equiv1 = find_reg_equal_equiv_note (i1);
      rtx equiv2 = find_reg_equal_equiv_note (i2);

      if (equiv1 && equiv2
	  /* If the equivalences are not to a constant, they may
	     reference pseudos that no longer exist, so we can't
	     use them.  */
	  && (! reload_completed
	      || (CONSTANT_P (XEXP (equiv1, 0))
		  && rtx_equal_p (XEXP (equiv1, 0), XEXP (equiv2, 0)))))
	{
	  rtx s1 = single_set (i1);
	  rtx s2 = single_set (i2);
	  if (s1 != 0 && s2 != 0
	      && rtx_renumbered_equal_p (SET_DEST (s1), SET_DEST (s2)))
	    {
	      validate_change (i1, &SET_SRC (s1), XEXP (equiv1, 0), 1);
	      validate_change (i2, &SET_SRC (s2), XEXP (equiv2, 0), 1);
	      if (! rtx_renumbered_equal_p (p1, p2))
		cancel_changes (0);
	      else if (apply_change_group ())
		return true;
	    }
	}
    }

  return false;
}

/* Look through the insns at the end of BB1 and BB2 and find the longest
   sequence that are equivalent.  Store the first insns for that sequence
   in *F1 and *F2 and return the sequence length.

   To simplify callers of this function, if the blocks match exactly,
   store the head of the blocks in *F1 and *F2.  */

int
flow_find_cross_jump (int mode ATTRIBUTE_UNUSED, basic_block bb1,
		      basic_block bb2, rtx *f1, rtx *f2)
{
  rtx i1, i2, last1, last2, afterlast1, afterlast2;
  int ninsns = 0;

  /* Skip simple jumps at the end of the blocks.  Complex jumps still
     need to be compared for equivalence, which we'll do below.  */

  i1 = BB_END (bb1);
  last1 = afterlast1 = last2 = afterlast2 = NULL_RTX;
  if (onlyjump_p (i1)
      || (returnjump_p (i1) && !side_effects_p (PATTERN (i1))))
    {
      last1 = i1;
      i1 = PREV_INSN (i1);
    }

  i2 = BB_END (bb2);
  if (onlyjump_p (i2)
      || (returnjump_p (i2) && !side_effects_p (PATTERN (i2))))
    {
      last2 = i2;
      /* Count everything except for unconditional jump as insn.  */
      if (!simplejump_p (i2) && !returnjump_p (i2) && last1)
	ninsns++;
      i2 = PREV_INSN (i2);
    }

  while (true)
    {
      /* Ignore notes.  */
      while (!INSN_P (i1) && i1 != BB_HEAD (bb1))
	i1 = PREV_INSN (i1);

      while (!INSN_P (i2) && i2 != BB_HEAD (bb2))
	i2 = PREV_INSN (i2);

      if (i1 == BB_HEAD (bb1) || i2 == BB_HEAD (bb2))
	break;

      if (!insns_match_p (mode, i1, i2))
	break;

      merge_memattrs (i1, i2);

      /* Don't begin a cross-jump with a NOTE insn.  */
      if (INSN_P (i1))
	{
	  /* If the merged insns have different REG_EQUAL notes, then
	     remove them.  */
	  rtx equiv1 = find_reg_equal_equiv_note (i1);
	  rtx equiv2 = find_reg_equal_equiv_note (i2);

	  if (equiv1 && !equiv2)
	    remove_note (i1, equiv1);
	  else if (!equiv1 && equiv2)
	    remove_note (i2, equiv2);
	  else if (equiv1 && equiv2
		   && !rtx_equal_p (XEXP (equiv1, 0), XEXP (equiv2, 0)))
	    {
	      remove_note (i1, equiv1);
	      remove_note (i2, equiv2);
	    }

	  afterlast1 = last1, afterlast2 = last2;
	  last1 = i1, last2 = i2;
	  ninsns++;
	}

      i1 = PREV_INSN (i1);
      i2 = PREV_INSN (i2);
    }

#ifdef HAVE_cc0
  /* Don't allow the insn after a compare to be shared by
     cross-jumping unless the compare is also shared.  */
  if (ninsns && reg_mentioned_p (cc0_rtx, last1) && ! sets_cc0_p (last1))
    last1 = afterlast1, last2 = afterlast2, ninsns--;
#endif

  /* Include preceding notes and labels in the cross-jump.  One,
     this may bring us to the head of the blocks as requested above.
     Two, it keeps line number notes as matched as may be.  */
  if (ninsns)
    {
      while (last1 != BB_HEAD (bb1) && !INSN_P (PREV_INSN (last1)))
	last1 = PREV_INSN (last1);

      if (last1 != BB_HEAD (bb1) && LABEL_P (PREV_INSN (last1)))
	last1 = PREV_INSN (last1);

      while (last2 != BB_HEAD (bb2) && !INSN_P (PREV_INSN (last2)))
	last2 = PREV_INSN (last2);

      if (last2 != BB_HEAD (bb2) && LABEL_P (PREV_INSN (last2)))
	last2 = PREV_INSN (last2);

      *f1 = last1;
      *f2 = last2;
    }

  return ninsns;
}
