/* Optimize jump instructions, for GNU compiler.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996, 1997
   1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/* This is the pathetic reminder of old fame of the jump-optimization pass
   of the compiler.  Now it contains basically set of utility function to
   operate with jumps.

   Each CODE_LABEL has a count of the times it is used
   stored in the LABEL_NUSES internal field, and each JUMP_INSN
   has one label that it refers to stored in the
   JUMP_LABEL internal field.  With this we can detect labels that
   become unused because of the deletion of all the jumps that
   formerly used them.  The JUMP_LABEL info is sometimes looked
   at by later passes.

   The subroutines delete_insn, redirect_jump, and invert_jump are used
   from other passes as well.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "function.h"
#include "expr.h"
#include "real.h"
#include "except.h"
#include "toplev.h"
#include "reload.h"
#include "predict.h"

/* Optimize jump y; x: ... y: jumpif... x?
   Don't know if it is worth bothering with.  */
/* Optimize two cases of conditional jump to conditional jump?
   This can never delete any instruction or make anything dead,
   or even change what is live at any point.
   So perhaps let combiner do it.  */

static int init_label_info		PARAMS ((rtx));
static void mark_all_labels		PARAMS ((rtx));
static int duplicate_loop_exit_test	PARAMS ((rtx));
static void delete_computation		PARAMS ((rtx));
static void redirect_exp_1		PARAMS ((rtx *, rtx, rtx, rtx));
static int redirect_exp			PARAMS ((rtx, rtx, rtx));
static void invert_exp_1		PARAMS ((rtx));
static int invert_exp			PARAMS ((rtx));
static int returnjump_p_1	        PARAMS ((rtx *, void *));
static void delete_prior_computation    PARAMS ((rtx, rtx));

/* Alternate entry into the jump optimizer.  This entry point only rebuilds
   the JUMP_LABEL field in jumping insns and REG_LABEL notes in non-jumping
   instructions.  */
void
rebuild_jump_labels (f)
     rtx f;
{
  rtx insn;
  int max_uid = 0;

  max_uid = init_label_info (f) + 1;

  mark_all_labels (f);

  /* Keep track of labels used from static data; we don't track them
     closely enough to delete them here, so make sure their reference
     count doesn't drop to zero.  */

  for (insn = forced_labels; insn; insn = XEXP (insn, 1))
    if (GET_CODE (XEXP (insn, 0)) == CODE_LABEL)
      LABEL_NUSES (XEXP (insn, 0))++;
}

/* Some old code expects exactly one BARRIER as the NEXT_INSN of a
   non-fallthru insn.  This is not generally true, as multiple barriers
   may have crept in, or the BARRIER may be separated from the last
   real insn by one or more NOTEs.

   This simple pass moves barriers and removes duplicates so that the
   old code is happy.
 */
void
cleanup_barriers ()
{
  rtx insn, next, prev;
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (GET_CODE (insn) == BARRIER)
	{
	  prev = prev_nonnote_insn (insn);
	  if (GET_CODE (prev) == BARRIER)
	    delete_barrier (insn);
	  else if (prev != PREV_INSN (insn))
	    reorder_insns (insn, insn, prev);
	}
    }
}

void
copy_loop_headers (f)
     rtx f;
{
  rtx insn, next;
  /* Now iterate optimizing jumps until nothing changes over one pass.  */
  for (insn = f; insn; insn = next)
    {
      rtx temp, temp1;

      next = NEXT_INSN (insn);

      /* See if this is a NOTE_INSN_LOOP_BEG followed by an unconditional
	 jump.  Try to optimize by duplicating the loop exit test if so.
	 This is only safe immediately after regscan, because it uses
	 the values of regno_first_uid and regno_last_uid.  */
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
	  && (temp1 = next_nonnote_insn (insn)) != 0
	  && any_uncondjump_p (temp1) && onlyjump_p (temp1))
	{
	  temp = PREV_INSN (insn);
	  if (duplicate_loop_exit_test (insn))
	    {
	      next = NEXT_INSN (temp);
	    }
	}
    }
}

void
purge_line_number_notes (f)
     rtx f;
{
  rtx last_note = 0;
  rtx insn;
  /* Delete extraneous line number notes.
     Note that two consecutive notes for different lines are not really
     extraneous.  There should be some indication where that line belonged,
     even if it became empty.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_BEG)
	  /* Any previous line note was for the prologue; gdb wants a new
	     note after the prologue even if it is for the same line.  */
	  last_note = NULL_RTX;
	else if (NOTE_LINE_NUMBER (insn) >= 0)
	  {
	    /* Delete this note if it is identical to previous note.  */
	    if (last_note
		&& NOTE_SOURCE_FILE (insn) == NOTE_SOURCE_FILE (last_note)
		&& NOTE_LINE_NUMBER (insn) == NOTE_LINE_NUMBER (last_note))
	      {
		delete_related_insns (insn);
		continue;
	      }

	    last_note = insn;
	  }
      }
}

/* Initialize LABEL_NUSES and JUMP_LABEL fields.  Delete any REG_LABEL
   notes whose labels don't occur in the insn any more.  Returns the
   largest INSN_UID found.  */
static int
init_label_info (f)
     rtx f;
{
  int largest_uid = 0;
  rtx insn;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CODE_LABEL)
	LABEL_NUSES (insn) = (LABEL_PRESERVE_P (insn) != 0);
      else if (GET_CODE (insn) == JUMP_INSN)
	JUMP_LABEL (insn) = 0;
      else if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
	{
	  rtx note, next;

	  for (note = REG_NOTES (insn); note; note = next)
	    {
	      next = XEXP (note, 1);
	      if (REG_NOTE_KIND (note) == REG_LABEL
		  && ! reg_mentioned_p (XEXP (note, 0), PATTERN (insn)))
		remove_note (insn, note);
	    }
	}
      if (INSN_UID (insn) > largest_uid)
	largest_uid = INSN_UID (insn);
    }

  return largest_uid;
}

/* Mark the label each jump jumps to.
   Combine consecutive labels, and count uses of labels.  */

static void
mark_all_labels (f)
     rtx f;
{
  rtx insn;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	if (GET_CODE (insn) == CALL_INSN
	    && GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	  {
	    mark_all_labels (XEXP (PATTERN (insn), 0));
	    mark_all_labels (XEXP (PATTERN (insn), 1));
	    mark_all_labels (XEXP (PATTERN (insn), 2));

	    /* Canonicalize the tail recursion label attached to the
	       CALL_PLACEHOLDER insn.  */
	    if (XEXP (PATTERN (insn), 3))
	      {
		rtx label_ref = gen_rtx_LABEL_REF (VOIDmode,
						   XEXP (PATTERN (insn), 3));
		mark_jump_label (label_ref, insn, 0);
		XEXP (PATTERN (insn), 3) = XEXP (label_ref, 0);
	      }

	    continue;
	  }

	mark_jump_label (PATTERN (insn), insn, 0);
	if (! INSN_DELETED_P (insn) && GET_CODE (insn) == JUMP_INSN)
	  {
	    /* When we know the LABEL_REF contained in a REG used in
	       an indirect jump, we'll have a REG_LABEL note so that
	       flow can tell where it's going.  */
	    if (JUMP_LABEL (insn) == 0)
	      {
		rtx label_note = find_reg_note (insn, REG_LABEL, NULL_RTX);
		if (label_note)
		  {
		    /* But a LABEL_REF around the REG_LABEL note, so
		       that we can canonicalize it.  */
		    rtx label_ref = gen_rtx_LABEL_REF (VOIDmode,
						       XEXP (label_note, 0));

		    mark_jump_label (label_ref, insn, 0);
		    XEXP (label_note, 0) = XEXP (label_ref, 0);
		    JUMP_LABEL (insn) = XEXP (label_note, 0);
		  }
	      }
	  }
      }
}

/* LOOP_START is a NOTE_INSN_LOOP_BEG note that is followed by an unconditional
   jump.  Assume that this unconditional jump is to the exit test code.  If
   the code is sufficiently simple, make a copy of it before INSN,
   followed by a jump to the exit of the loop.  Then delete the unconditional
   jump after INSN.

   Return 1 if we made the change, else 0.

   This is only safe immediately after a regscan pass because it uses the
   values of regno_first_uid and regno_last_uid.  */

static int
duplicate_loop_exit_test (loop_start)
     rtx loop_start;
{
  rtx insn, set, reg, p, link;
  rtx copy = 0, first_copy = 0;
  int num_insns = 0;
  rtx exitcode = NEXT_INSN (JUMP_LABEL (next_nonnote_insn (loop_start)));
  rtx lastexit;
  int max_reg = max_reg_num ();
  rtx *reg_map = 0;
  rtx loop_pre_header_label;

  /* Scan the exit code.  We do not perform this optimization if any insn:

         is a CALL_INSN
	 is a CODE_LABEL
	 has a REG_RETVAL or REG_LIBCALL note (hard to adjust)
	 is a NOTE_INSN_LOOP_BEG because this means we have a nested loop
	 is a NOTE_INSN_BLOCK_{BEG,END} because duplicating these notes
	      is not valid.

     We also do not do this if we find an insn with ASM_OPERANDS.  While
     this restriction should not be necessary, copying an insn with
     ASM_OPERANDS can confuse asm_noperands in some cases.

     Also, don't do this if the exit code is more than 20 insns.  */

  for (insn = exitcode;
       insn
       && ! (GET_CODE (insn) == NOTE
	     && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END);
       insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case CODE_LABEL:
	case CALL_INSN:
	  return 0;
	case NOTE:
	  /* We could be in front of the wrong NOTE_INSN_LOOP_END if there is
	     a jump immediately after the loop start that branches outside
	     the loop but within an outer loop, near the exit test.
	     If we copied this exit test and created a phony
	     NOTE_INSN_LOOP_VTOP, this could make instructions immediately
	     before the exit test look like these could be safely moved
	     out of the loop even if they actually may be never executed.
	     This can be avoided by checking here for NOTE_INSN_LOOP_CONT.  */

	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_CONT)
	    return 0;

	  if (optimize < 2
	      && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
		  || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END))
	    /* If we were to duplicate this code, we would not move
	       the BLOCK notes, and so debugging the moved code would
	       be difficult.  Thus, we only move the code with -O2 or
	       higher.  */
	    return 0;

	  break;
	case JUMP_INSN:
	case INSN:
	  /* The code below would grossly mishandle REG_WAS_0 notes,
	     so get rid of them here.  */
	  while ((p = find_reg_note (insn, REG_WAS_0, NULL_RTX)) != 0)
	    remove_note (insn, p);
	  if (++num_insns > 20
	      || find_reg_note (insn, REG_RETVAL, NULL_RTX)
	      || find_reg_note (insn, REG_LIBCALL, NULL_RTX))
	    return 0;
	  break;
	default:
	  break;
	}
    }

  /* Unless INSN is zero, we can do the optimization.  */
  if (insn == 0)
    return 0;

  lastexit = insn;

  /* See if any insn sets a register only used in the loop exit code and
     not a user variable.  If so, replace it with a new register.  */
  for (insn = exitcode; insn != lastexit; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN
	&& (set = single_set (insn)) != 0
	&& ((reg = SET_DEST (set), GET_CODE (reg) == REG)
	    || (GET_CODE (reg) == SUBREG
		&& (reg = SUBREG_REG (reg), GET_CODE (reg) == REG)))
	&& REGNO (reg) >= FIRST_PSEUDO_REGISTER
	&& REGNO_FIRST_UID (REGNO (reg)) == INSN_UID (insn))
      {
	for (p = NEXT_INSN (insn); p != lastexit; p = NEXT_INSN (p))
	  if (REGNO_LAST_UID (REGNO (reg)) == INSN_UID (p))
	    break;

	if (p != lastexit)
	  {
	    /* We can do the replacement.  Allocate reg_map if this is the
	       first replacement we found.  */
	    if (reg_map == 0)
	      reg_map = (rtx *) xcalloc (max_reg, sizeof (rtx));

	    REG_LOOP_TEST_P (reg) = 1;

	    reg_map[REGNO (reg)] = gen_reg_rtx (GET_MODE (reg));
	  }
      }
  loop_pre_header_label = gen_label_rtx ();

  /* Now copy each insn.  */
  for (insn = exitcode; insn != lastexit; insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case BARRIER:
	  copy = emit_barrier_before (loop_start);
	  break;
	case NOTE:
	  /* Only copy line-number notes.  */
	  if (NOTE_LINE_NUMBER (insn) >= 0)
	    {
	      copy = emit_note_before (NOTE_LINE_NUMBER (insn), loop_start);
	      NOTE_SOURCE_FILE (copy) = NOTE_SOURCE_FILE (insn);
	    }
	  break;

	case INSN:
	  copy = emit_insn_before (copy_insn (PATTERN (insn)), loop_start);
	  if (reg_map)
	    replace_regs (PATTERN (copy), reg_map, max_reg, 1);

	  mark_jump_label (PATTERN (copy), copy, 0);

	  /* Copy all REG_NOTES except REG_LABEL since mark_jump_label will
	     make them.  */
	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) != REG_LABEL)
	      {
		if (GET_CODE (link) == EXPR_LIST)
		  REG_NOTES (copy)
		    = copy_insn_1 (gen_rtx_EXPR_LIST (REG_NOTE_KIND (link),
						      XEXP (link, 0),
						      REG_NOTES (copy)));
		else
		  REG_NOTES (copy)
		    = copy_insn_1 (gen_rtx_INSN_LIST (REG_NOTE_KIND (link),
						      XEXP (link, 0),
						      REG_NOTES (copy)));
	      }

	  if (reg_map && REG_NOTES (copy))
	    replace_regs (REG_NOTES (copy), reg_map, max_reg, 1);
	  break;

	case JUMP_INSN:
	  copy = emit_jump_insn_before (copy_insn (PATTERN (insn)),
					loop_start);
	  if (reg_map)
	    replace_regs (PATTERN (copy), reg_map, max_reg, 1);
	  mark_jump_label (PATTERN (copy), copy, 0);
	  if (REG_NOTES (insn))
	    {
	      REG_NOTES (copy) = copy_insn_1 (REG_NOTES (insn));
	      if (reg_map)
		replace_regs (REG_NOTES (copy), reg_map, max_reg, 1);
	    }

	  /* Predict conditional jump that do make loop looping as taken.
	     Other jumps are probably exit conditions, so predict
	     them as untaken.  */
	  if (any_condjump_p (copy))
	    {
	      rtx label = JUMP_LABEL (copy);
	      if (label)
		{
		  /* The jump_insn after loop_start should be followed
		     by barrier and loopback label.  */
		  if (prev_nonnote_insn (label)
		      && (prev_nonnote_insn (prev_nonnote_insn (label))
			  == next_nonnote_insn (loop_start)))
		    {
		      predict_insn_def (copy, PRED_LOOP_HEADER, TAKEN);
		      /* To keep pre-header, we need to redirect all loop
		         entrances before the LOOP_BEG note.  */
		      redirect_jump (copy, loop_pre_header_label, 0);
		    }
		  else
		    predict_insn_def (copy, PRED_LOOP_HEADER, NOT_TAKEN);
		}
	    }
	  break;

	default:
	  abort ();
	}

      /* Record the first insn we copied.  We need it so that we can
	 scan the copied insns for new pseudo registers.  */
      if (! first_copy)
	first_copy = copy;
    }

  /* Now clean up by emitting a jump to the end label and deleting the jump
     at the start of the loop.  */
  if (! copy || GET_CODE (copy) != BARRIER)
    {
      copy = emit_jump_insn_before (gen_jump (get_label_after (insn)),
				    loop_start);

      /* Record the first insn we copied.  We need it so that we can
	 scan the copied insns for new pseudo registers.   This may not
	 be strictly necessary since we should have copied at least one
	 insn above.  But I am going to be safe.  */
      if (! first_copy)
	first_copy = copy;

      mark_jump_label (PATTERN (copy), copy, 0);
      emit_barrier_before (loop_start);
    }

  emit_label_before (loop_pre_header_label, loop_start);

  /* Now scan from the first insn we copied to the last insn we copied
     (copy) for new pseudo registers.  Do this after the code to jump to
     the end label since that might create a new pseudo too.  */
  reg_scan_update (first_copy, copy, max_reg);

  /* Mark the exit code as the virtual top of the converted loop.  */
  emit_note_before (NOTE_INSN_LOOP_VTOP, exitcode);

  delete_related_insns (next_nonnote_insn (loop_start));

  /* Clean up.  */
  if (reg_map)
    free (reg_map);

  return 1;
}

/* Move all block-beg, block-end, loop-beg, loop-cont, loop-vtop, loop-end,
   notes between START and END out before START.  START and END may be such
   notes.  Returns the values of the new starting and ending insns, which
   may be different if the original ones were such notes.
   Return true if there were only such notes and no real instructions.  */

bool
squeeze_notes (startp, endp)
     rtx* startp;
     rtx* endp;
{
  rtx start = *startp;
  rtx end = *endp;

  rtx insn;
  rtx next;
  rtx last = NULL;
  rtx past_end = NEXT_INSN (end);

  for (insn = start; insn != past_end; insn = next)
    {
      next = NEXT_INSN (insn);
      if (GET_CODE (insn) == NOTE
	  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_CONT
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_VTOP))
	{
	  if (insn == start)
	    start = next;
	  else
	    {
	      rtx prev = PREV_INSN (insn);
	      PREV_INSN (insn) = PREV_INSN (start);
	      NEXT_INSN (insn) = start;
	      NEXT_INSN (PREV_INSN (insn)) = insn;
	      PREV_INSN (NEXT_INSN (insn)) = insn;
	      NEXT_INSN (prev) = next;
	      PREV_INSN (next) = prev;
	    }
	}
      else
	last = insn;
    }

  /* There were no real instructions.  */
  if (start == past_end)
    return true;

  end = last;

  *startp = start;
  *endp = end;
  return false;
}

/* Return the label before INSN, or put a new label there.  */

rtx
get_label_before (insn)
     rtx insn;
{
  rtx label;

  /* Find an existing label at this point
     or make a new one if there is none.  */
  label = prev_nonnote_insn (insn);

  if (label == 0 || GET_CODE (label) != CODE_LABEL)
    {
      rtx prev = PREV_INSN (insn);

      label = gen_label_rtx ();
      emit_label_after (label, prev);
      LABEL_NUSES (label) = 0;
    }
  return label;
}

/* Return the label after INSN, or put a new label there.  */

rtx
get_label_after (insn)
     rtx insn;
{
  rtx label;

  /* Find an existing label at this point
     or make a new one if there is none.  */
  label = next_nonnote_insn (insn);

  if (label == 0 || GET_CODE (label) != CODE_LABEL)
    {
      label = gen_label_rtx ();
      emit_label_after (label, insn);
      LABEL_NUSES (label) = 0;
    }
  return label;
}

/* Given a comparison (CODE ARG0 ARG1), inside an insn, INSN, return a code
   of reversed comparison if it is possible to do so.  Otherwise return UNKNOWN.
   UNKNOWN may be returned in case we are having CC_MODE compare and we don't
   know whether it's source is floating point or integer comparison.  Machine
   description should define REVERSIBLE_CC_MODE and REVERSE_CONDITION macros
   to help this function avoid overhead in these cases.  */
enum rtx_code
reversed_comparison_code_parts (code, arg0, arg1, insn)
     rtx insn, arg0, arg1;
     enum rtx_code code;
{
  enum machine_mode mode;

  /* If this is not actually a comparison, we can't reverse it.  */
  if (GET_RTX_CLASS (code) != '<')
    return UNKNOWN;

  mode = GET_MODE (arg0);
  if (mode == VOIDmode)
    mode = GET_MODE (arg1);

  /* First see if machine description supply us way to reverse the comparison.
     Give it priority over everything else to allow machine description to do
     tricks.  */
#ifdef REVERSIBLE_CC_MODE
  if (GET_MODE_CLASS (mode) == MODE_CC
      && REVERSIBLE_CC_MODE (mode))
    {
#ifdef REVERSE_CONDITION
      return REVERSE_CONDITION (code, mode);
#endif
      return reverse_condition (code);
    }
#endif

  /* Try a few special cases based on the comparison code.  */
  switch (code)
    {
    case GEU:
    case GTU:
    case LEU:
    case LTU:
    case NE:
    case EQ:
      /* It is always safe to reverse EQ and NE, even for the floating
	 point.  Similary the unsigned comparisons are never used for
	 floating point so we can reverse them in the default way.  */
      return reverse_condition (code);
    case ORDERED:
    case UNORDERED:
    case LTGT:
    case UNEQ:
      /* In case we already see unordered comparison, we can be sure to
	 be dealing with floating point so we don't need any more tests.  */
      return reverse_condition_maybe_unordered (code);
    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
      /* We don't have safe way to reverse these yet.  */
      return UNKNOWN;
    default:
      break;
    }

  /* In case we give up IEEE compatibility, all comparisons are reversible.  */
  if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
      || flag_unsafe_math_optimizations)
    return reverse_condition (code);

  if (GET_MODE_CLASS (mode) == MODE_CC
#ifdef HAVE_cc0
      || arg0 == cc0_rtx
#endif
      )
    {
      rtx prev;
      /* Try to search for the comparison to determine the real mode.
         This code is expensive, but with sane machine description it
         will be never used, since REVERSIBLE_CC_MODE will return true
         in all cases.  */
      if (! insn)
	return UNKNOWN;

      for (prev = prev_nonnote_insn (insn);
	   prev != 0 && GET_CODE (prev) != CODE_LABEL;
	   prev = prev_nonnote_insn (prev))
	{
	  rtx set = set_of (arg0, prev);
	  if (set && GET_CODE (set) == SET
	      && rtx_equal_p (SET_DEST (set), arg0))
	    {
	      rtx src = SET_SRC (set);

	      if (GET_CODE (src) == COMPARE)
		{
		  rtx comparison = src;
		  arg0 = XEXP (src, 0);
		  mode = GET_MODE (arg0);
		  if (mode == VOIDmode)
		    mode = GET_MODE (XEXP (comparison, 1));
		  break;
		}
	      /* We can get past reg-reg moves.  This may be useful for model
	         of i387 comparisons that first move flag registers around.  */
	      if (REG_P (src))
		{
		  arg0 = src;
		  continue;
		}
	    }
	  /* If register is clobbered in some ununderstandable way,
	     give up.  */
	  if (set)
	    return UNKNOWN;
	}
    }

  /* An integer condition.  */
  if (GET_CODE (arg0) == CONST_INT
      || (GET_MODE (arg0) != VOIDmode
	  && GET_MODE_CLASS (mode) != MODE_CC
	  && ! FLOAT_MODE_P (mode)))
    return reverse_condition (code);

  return UNKNOWN;
}

/* An wrapper around the previous function to take COMPARISON as rtx
   expression.  This simplifies many callers.  */
enum rtx_code
reversed_comparison_code (comparison, insn)
     rtx comparison, insn;
{
  if (GET_RTX_CLASS (GET_CODE (comparison)) != '<')
    return UNKNOWN;
  return reversed_comparison_code_parts (GET_CODE (comparison),
					 XEXP (comparison, 0),
					 XEXP (comparison, 1), insn);
}

/* Given an rtx-code for a comparison, return the code for the negated
   comparison.  If no such code exists, return UNKNOWN.

   WATCH OUT!  reverse_condition is not safe to use on a jump that might
   be acting on the results of an IEEE floating point comparison, because
   of the special treatment of non-signaling nans in comparisons.
   Use reversed_comparison_code instead.  */

enum rtx_code
reverse_condition (code)
     enum rtx_code code;
{
  switch (code)
    {
    case EQ:
      return NE;
    case NE:
      return EQ;
    case GT:
      return LE;
    case GE:
      return LT;
    case LT:
      return GE;
    case LE:
      return GT;
    case GTU:
      return LEU;
    case GEU:
      return LTU;
    case LTU:
      return GEU;
    case LEU:
      return GTU;
    case UNORDERED:
      return ORDERED;
    case ORDERED:
      return UNORDERED;

    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
    case UNEQ:
    case LTGT:
      return UNKNOWN;

    default:
      abort ();
    }
}

/* Similar, but we're allowed to generate unordered comparisons, which
   makes it safe for IEEE floating-point.  Of course, we have to recognize
   that the target will support them too...  */

enum rtx_code
reverse_condition_maybe_unordered (code)
     enum rtx_code code;
{
  /* Non-IEEE formats don't have unordered conditions.  */
  if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT)
    return reverse_condition (code);

  switch (code)
    {
    case EQ:
      return NE;
    case NE:
      return EQ;
    case GT:
      return UNLE;
    case GE:
      return UNLT;
    case LT:
      return UNGE;
    case LE:
      return UNGT;
    case LTGT:
      return UNEQ;
    case UNORDERED:
      return ORDERED;
    case ORDERED:
      return UNORDERED;
    case UNLT:
      return GE;
    case UNLE:
      return GT;
    case UNGT:
      return LE;
    case UNGE:
      return LT;
    case UNEQ:
      return LTGT;

    default:
      abort ();
    }
}

/* Similar, but return the code when two operands of a comparison are swapped.
   This IS safe for IEEE floating-point.  */

enum rtx_code
swap_condition (code)
     enum rtx_code code;
{
  switch (code)
    {
    case EQ:
    case NE:
    case UNORDERED:
    case ORDERED:
    case UNEQ:
    case LTGT:
      return code;

    case GT:
      return LT;
    case GE:
      return LE;
    case LT:
      return GT;
    case LE:
      return GE;
    case GTU:
      return LTU;
    case GEU:
      return LEU;
    case LTU:
      return GTU;
    case LEU:
      return GEU;
    case UNLT:
      return UNGT;
    case UNLE:
      return UNGE;
    case UNGT:
      return UNLT;
    case UNGE:
      return UNLE;

    default:
      abort ();
    }
}

/* Given a comparison CODE, return the corresponding unsigned comparison.
   If CODE is an equality comparison or already an unsigned comparison,
   CODE is returned.  */

enum rtx_code
unsigned_condition (code)
     enum rtx_code code;
{
  switch (code)
    {
    case EQ:
    case NE:
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      return code;

    case GT:
      return GTU;
    case GE:
      return GEU;
    case LT:
      return LTU;
    case LE:
      return LEU;

    default:
      abort ();
    }
}

/* Similarly, return the signed version of a comparison.  */

enum rtx_code
signed_condition (code)
     enum rtx_code code;
{
  switch (code)
    {
    case EQ:
    case NE:
    case GT:
    case GE:
    case LT:
    case LE:
      return code;

    case GTU:
      return GT;
    case GEU:
      return GE;
    case LTU:
      return LT;
    case LEU:
      return LE;

    default:
      abort ();
    }
}

/* Return non-zero if CODE1 is more strict than CODE2, i.e., if the
   truth of CODE1 implies the truth of CODE2.  */

int
comparison_dominates_p (code1, code2)
     enum rtx_code code1, code2;
{
  /* UNKNOWN comparison codes can happen as a result of trying to revert
     comparison codes.
     They can't match anything, so we have to reject them here.  */
  if (code1 == UNKNOWN || code2 == UNKNOWN)
    return 0;

  if (code1 == code2)
    return 1;

  switch (code1)
    {
    case UNEQ:
      if (code2 == UNLE || code2 == UNGE)
	return 1;
      break;

    case EQ:
      if (code2 == LE || code2 == LEU || code2 == GE || code2 == GEU
	  || code2 == ORDERED)
	return 1;
      break;

    case UNLT:
      if (code2 == UNLE || code2 == NE)
	return 1;
      break;

    case LT:
      if (code2 == LE || code2 == NE || code2 == ORDERED || code2 == LTGT)
	return 1;
      break;

    case UNGT:
      if (code2 == UNGE || code2 == NE)
	return 1;
      break;

    case GT:
      if (code2 == GE || code2 == NE || code2 == ORDERED || code2 == LTGT)
	return 1;
      break;

    case GE:
    case LE:
      if (code2 == ORDERED)
	return 1;
      break;

    case LTGT:
      if (code2 == NE || code2 == ORDERED)
	return 1;
      break;

    case LTU:
      if (code2 == LEU || code2 == NE)
	return 1;
      break;

    case GTU:
      if (code2 == GEU || code2 == NE)
	return 1;
      break;

    case UNORDERED:
      if (code2 == NE || code2 == UNEQ || code2 == UNLE || code2 == UNLT
	  || code2 == UNGE || code2 == UNGT)
	return 1;
      break;

    default:
      break;
    }

  return 0;
}

/* Return 1 if INSN is an unconditional jump and nothing else.  */

int
simplejump_p (insn)
     rtx insn;
{
  return (GET_CODE (insn) == JUMP_INSN
	  && GET_CODE (PATTERN (insn)) == SET
	  && GET_CODE (SET_DEST (PATTERN (insn))) == PC
	  && GET_CODE (SET_SRC (PATTERN (insn))) == LABEL_REF);
}

/* Return nonzero if INSN is a (possibly) conditional jump
   and nothing more.

   Use this function is deprecated, since we need to support combined
   branch and compare insns.  Use any_condjump_p instead whenever possible.  */

int
condjump_p (insn)
     rtx insn;
{
  rtx x = PATTERN (insn);

  if (GET_CODE (x) != SET
      || GET_CODE (SET_DEST (x)) != PC)
    return 0;

  x = SET_SRC (x);
  if (GET_CODE (x) == LABEL_REF)
    return 1;
  else
    return (GET_CODE (x) == IF_THEN_ELSE
	    && ((GET_CODE (XEXP (x, 2)) == PC
		 && (GET_CODE (XEXP (x, 1)) == LABEL_REF
		     || GET_CODE (XEXP (x, 1)) == RETURN))
		|| (GET_CODE (XEXP (x, 1)) == PC
		    && (GET_CODE (XEXP (x, 2)) == LABEL_REF
			|| GET_CODE (XEXP (x, 2)) == RETURN))));

  return 0;
}

/* Return nonzero if INSN is a (possibly) conditional jump inside a
   PARALLEL.

   Use this function is deprecated, since we need to support combined
   branch and compare insns.  Use any_condjump_p instead whenever possible.  */

int
condjump_in_parallel_p (insn)
     rtx insn;
{
  rtx x = PATTERN (insn);

  if (GET_CODE (x) != PARALLEL)
    return 0;
  else
    x = XVECEXP (x, 0, 0);

  if (GET_CODE (x) != SET)
    return 0;
  if (GET_CODE (SET_DEST (x)) != PC)
    return 0;
  if (GET_CODE (SET_SRC (x)) == LABEL_REF)
    return 1;
  if (GET_CODE (SET_SRC (x)) != IF_THEN_ELSE)
    return 0;
  if (XEXP (SET_SRC (x), 2) == pc_rtx
      && (GET_CODE (XEXP (SET_SRC (x), 1)) == LABEL_REF
	  || GET_CODE (XEXP (SET_SRC (x), 1)) == RETURN))
    return 1;
  if (XEXP (SET_SRC (x), 1) == pc_rtx
      && (GET_CODE (XEXP (SET_SRC (x), 2)) == LABEL_REF
	  || GET_CODE (XEXP (SET_SRC (x), 2)) == RETURN))
    return 1;
  return 0;
}

/* Return set of PC, otherwise NULL.  */

rtx
pc_set (insn)
     rtx insn;
{
  rtx pat;
  if (GET_CODE (insn) != JUMP_INSN)
    return NULL_RTX;
  pat = PATTERN (insn);

  /* The set is allowed to appear either as the insn pattern or
     the first set in a PARALLEL.  */
  if (GET_CODE (pat) == PARALLEL)
    pat = XVECEXP (pat, 0, 0);
  if (GET_CODE (pat) == SET && GET_CODE (SET_DEST (pat)) == PC)
    return pat;

  return NULL_RTX;
}

/* Return true when insn is an unconditional direct jump,
   possibly bundled inside a PARALLEL.  */

int
any_uncondjump_p (insn)
     rtx insn;
{
  rtx x = pc_set (insn);
  if (!x)
    return 0;
  if (GET_CODE (SET_SRC (x)) != LABEL_REF)
    return 0;
  return 1;
}

/* Return true when insn is a conditional jump.  This function works for
   instructions containing PC sets in PARALLELs.  The instruction may have
   various other effects so before removing the jump you must verify
   onlyjump_p.

   Note that unlike condjump_p it returns false for unconditional jumps.  */

int
any_condjump_p (insn)
     rtx insn;
{
  rtx x = pc_set (insn);
  enum rtx_code a, b;

  if (!x)
    return 0;
  if (GET_CODE (SET_SRC (x)) != IF_THEN_ELSE)
    return 0;

  a = GET_CODE (XEXP (SET_SRC (x), 1));
  b = GET_CODE (XEXP (SET_SRC (x), 2));

  return ((b == PC && (a == LABEL_REF || a == RETURN))
	  || (a == PC && (b == LABEL_REF || b == RETURN)));
}

/* Return the label of a conditional jump.  */

rtx
condjump_label (insn)
     rtx insn;
{
  rtx x = pc_set (insn);

  if (!x)
    return NULL_RTX;
  x = SET_SRC (x);
  if (GET_CODE (x) == LABEL_REF)
    return x;
  if (GET_CODE (x) != IF_THEN_ELSE)
    return NULL_RTX;
  if (XEXP (x, 2) == pc_rtx && GET_CODE (XEXP (x, 1)) == LABEL_REF)
    return XEXP (x, 1);
  if (XEXP (x, 1) == pc_rtx && GET_CODE (XEXP (x, 2)) == LABEL_REF)
    return XEXP (x, 2);
  return NULL_RTX;
}

/* Return true if INSN is a (possibly conditional) return insn.  */

static int
returnjump_p_1 (loc, data)
     rtx *loc;
     void *data ATTRIBUTE_UNUSED;
{
  rtx x = *loc;

  return x && (GET_CODE (x) == RETURN
	       || (GET_CODE (x) == SET && SET_IS_RETURN_P (x)));
}

int
returnjump_p (insn)
     rtx insn;
{
  if (GET_CODE (insn) != JUMP_INSN)
    return 0;
  return for_each_rtx (&PATTERN (insn), returnjump_p_1, NULL);
}

/* Return true if INSN is a jump that only transfers control and
   nothing more.  */

int
onlyjump_p (insn)
     rtx insn;
{
  rtx set;

  if (GET_CODE (insn) != JUMP_INSN)
    return 0;

  set = single_set (insn);
  if (set == NULL)
    return 0;
  if (GET_CODE (SET_DEST (set)) != PC)
    return 0;
  if (side_effects_p (SET_SRC (set)))
    return 0;

  return 1;
}

#ifdef HAVE_cc0

/* Return non-zero if X is an RTX that only sets the condition codes
   and has no side effects.  */

int
only_sets_cc0_p (x)
     rtx x;
{

  if (! x)
    return 0;

  if (INSN_P (x))
    x = PATTERN (x);

  return sets_cc0_p (x) == 1 && ! side_effects_p (x);
}

/* Return 1 if X is an RTX that does nothing but set the condition codes
   and CLOBBER or USE registers.
   Return -1 if X does explicitly set the condition codes,
   but also does other things.  */

int
sets_cc0_p (x)
     rtx x;
{

  if (! x)
    return 0;

  if (INSN_P (x))
    x = PATTERN (x);

  if (GET_CODE (x) == SET && SET_DEST (x) == cc0_rtx)
    return 1;
  if (GET_CODE (x) == PARALLEL)
    {
      int i;
      int sets_cc0 = 0;
      int other_things = 0;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  if (GET_CODE (XVECEXP (x, 0, i)) == SET
	      && SET_DEST (XVECEXP (x, 0, i)) == cc0_rtx)
	    sets_cc0 = 1;
	  else if (GET_CODE (XVECEXP (x, 0, i)) == SET)
	    other_things = 1;
	}
      return ! sets_cc0 ? 0 : other_things ? -1 : 1;
    }
  return 0;
}
#endif

/* Follow any unconditional jump at LABEL;
   return the ultimate label reached by any such chain of jumps.
   If LABEL is not followed by a jump, return LABEL.
   If the chain loops or we can't find end, return LABEL,
   since that tells caller to avoid changing the insn.

   If RELOAD_COMPLETED is 0, we do not chain across a NOTE_INSN_LOOP_BEG or
   a USE or CLOBBER.  */

rtx
follow_jumps (label)
     rtx label;
{
  rtx insn;
  rtx next;
  rtx value = label;
  int depth;

  for (depth = 0;
       (depth < 10
	&& (insn = next_active_insn (value)) != 0
	&& GET_CODE (insn) == JUMP_INSN
	&& ((JUMP_LABEL (insn) != 0 && any_uncondjump_p (insn)
	     && onlyjump_p (insn))
	    || GET_CODE (PATTERN (insn)) == RETURN)
	&& (next = NEXT_INSN (insn))
	&& GET_CODE (next) == BARRIER);
       depth++)
    {
      /* Don't chain through the insn that jumps into a loop
	 from outside the loop,
	 since that would create multiple loop entry jumps
	 and prevent loop optimization.  */
      rtx tem;
      if (!reload_completed)
	for (tem = value; tem != insn; tem = NEXT_INSN (tem))
	  if (GET_CODE (tem) == NOTE
	      && (NOTE_LINE_NUMBER (tem) == NOTE_INSN_LOOP_BEG
		  /* ??? Optional.  Disables some optimizations, but makes
		     gcov output more accurate with -O.  */
		  || (flag_test_coverage && NOTE_LINE_NUMBER (tem) > 0)))
	    return value;

      /* If we have found a cycle, make the insn jump to itself.  */
      if (JUMP_LABEL (insn) == label)
	return label;

      tem = next_active_insn (JUMP_LABEL (insn));
      if (tem && (GET_CODE (PATTERN (tem)) == ADDR_VEC
		  || GET_CODE (PATTERN (tem)) == ADDR_DIFF_VEC))
	break;

      value = JUMP_LABEL (insn);
    }
  if (depth == 10)
    return label;
  return value;
}


/* Find all CODE_LABELs referred to in X, and increment their use counts.
   If INSN is a JUMP_INSN and there is at least one CODE_LABEL referenced
   in INSN, then store one of them in JUMP_LABEL (INSN).
   If INSN is an INSN or a CALL_INSN and there is at least one CODE_LABEL
   referenced in INSN, add a REG_LABEL note containing that label to INSN.
   Also, when there are consecutive labels, canonicalize on the last of them.

   Note that two labels separated by a loop-beginning note
   must be kept distinct if we have not yet done loop-optimization,
   because the gap between them is where loop-optimize
   will want to move invariant code to.  CROSS_JUMP tells us
   that loop-optimization is done with.  */

void
mark_jump_label (x, insn, in_mem)
     rtx x;
     rtx insn;
     int in_mem;
{
  RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  switch (code)
    {
    case PC:
    case CC0:
    case REG:
    case SUBREG:
    case CONST_INT:
    case CONST_DOUBLE:
    case CLOBBER:
    case CALL:
      return;

    case MEM:
      in_mem = 1;
      break;

    case SYMBOL_REF:
      if (!in_mem)
	return;

      /* If this is a constant-pool reference, see if it is a label.  */
      if (CONSTANT_POOL_ADDRESS_P (x))
	mark_jump_label (get_pool_constant (x), insn, in_mem);
      break;

    case LABEL_REF:
      {
	rtx label = XEXP (x, 0);

	/* Ignore remaining references to unreachable labels that
	   have been deleted.  */
	if (GET_CODE (label) == NOTE
	    && NOTE_LINE_NUMBER (label) == NOTE_INSN_DELETED_LABEL)
	  break;

	if (GET_CODE (label) != CODE_LABEL)
	  abort ();

	/* Ignore references to labels of containing functions.  */
	if (LABEL_REF_NONLOCAL_P (x))
	  break;

	XEXP (x, 0) = label;
	if (! insn || ! INSN_DELETED_P (insn))
	  ++LABEL_NUSES (label);

	if (insn)
	  {
	    if (GET_CODE (insn) == JUMP_INSN)
	      JUMP_LABEL (insn) = label;
	    else
	      {
		/* Add a REG_LABEL note for LABEL unless there already
		   is one.  All uses of a label, except for labels
		   that are the targets of jumps, must have a
		   REG_LABEL note.  */
		if (! find_reg_note (insn, REG_LABEL, label))
		  REG_NOTES (insn) = gen_rtx_INSN_LIST (REG_LABEL, label,
							REG_NOTES (insn));
	      }
	  }
	return;
      }

  /* Do walk the labels in a vector, but not the first operand of an
     ADDR_DIFF_VEC.  Don't set the JUMP_LABEL of a vector.  */
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      if (! INSN_DELETED_P (insn))
	{
	  int eltnum = code == ADDR_DIFF_VEC ? 1 : 0;

	  for (i = 0; i < XVECLEN (x, eltnum); i++)
	    mark_jump_label (XVECEXP (x, eltnum, i), NULL_RTX, in_mem);
	}
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	mark_jump_label (XEXP (x, i), insn, in_mem);
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    mark_jump_label (XVECEXP (x, i, j), insn, in_mem);
	}
    }
}

/* If all INSN does is set the pc, delete it,
   and delete the insn that set the condition codes for it
   if that's what the previous thing was.  */

void
delete_jump (insn)
     rtx insn;
{
  rtx set = single_set (insn);

  if (set && GET_CODE (SET_DEST (set)) == PC)
    delete_computation (insn);
}

/* Verify INSN is a BARRIER and delete it.  */

void
delete_barrier (insn)
     rtx insn;
{
  if (GET_CODE (insn) != BARRIER)
    abort ();

  delete_insn (insn);
}

/* Recursively delete prior insns that compute the value (used only by INSN
   which the caller is deleting) stored in the register mentioned by NOTE
   which is a REG_DEAD note associated with INSN.  */

static void
delete_prior_computation (note, insn)
     rtx note;
     rtx insn;
{
  rtx our_prev;
  rtx reg = XEXP (note, 0);

  for (our_prev = prev_nonnote_insn (insn);
       our_prev && (GET_CODE (our_prev) == INSN
		    || GET_CODE (our_prev) == CALL_INSN);
       our_prev = prev_nonnote_insn (our_prev))
    {
      rtx pat = PATTERN (our_prev);

      /* If we reach a CALL which is not calling a const function
	 or the callee pops the arguments, then give up.  */
      if (GET_CODE (our_prev) == CALL_INSN
	  && (! CONST_OR_PURE_CALL_P (our_prev)
	      || GET_CODE (pat) != SET || GET_CODE (SET_SRC (pat)) != CALL))
	break;

      /* If we reach a SEQUENCE, it is too complex to try to
	 do anything with it, so give up.  */
      if (GET_CODE (pat) == SEQUENCE)
	break;

      if (GET_CODE (pat) == USE
	  && GET_CODE (XEXP (pat, 0)) == INSN)
	/* reorg creates USEs that look like this.  We leave them
	   alone because reorg needs them for its own purposes.  */
	break;

      if (reg_set_p (reg, pat))
	{
	  if (side_effects_p (pat) && GET_CODE (our_prev) != CALL_INSN)
	    break;

	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* If we find a SET of something else, we can't
		 delete the insn.  */

	      int i;

	      for (i = 0; i < XVECLEN (pat, 0); i++)
		{
		  rtx part = XVECEXP (pat, 0, i);

		  if (GET_CODE (part) == SET
		      && SET_DEST (part) != reg)
		    break;
		}

	      if (i == XVECLEN (pat, 0))
		delete_computation (our_prev);
	    }
	  else if (GET_CODE (pat) == SET
		   && GET_CODE (SET_DEST (pat)) == REG)
	    {
	      int dest_regno = REGNO (SET_DEST (pat));
	      int dest_endregno
		= (dest_regno
		   + (dest_regno < FIRST_PSEUDO_REGISTER
		      ? HARD_REGNO_NREGS (dest_regno,
					  GET_MODE (SET_DEST (pat))) : 1));
	      int regno = REGNO (reg);
	      int endregno
		= (regno
		   + (regno < FIRST_PSEUDO_REGISTER
		      ? HARD_REGNO_NREGS (regno, GET_MODE (reg)) : 1));

	      if (dest_regno >= regno
		  && dest_endregno <= endregno)
		delete_computation (our_prev);

	      /* We may have a multi-word hard register and some, but not
		 all, of the words of the register are needed in subsequent
		 insns.  Write REG_UNUSED notes for those parts that were not
		 needed.  */
	      else if (dest_regno <= regno
		       && dest_endregno >= endregno)
		{
		  int i;

		  REG_NOTES (our_prev)
		    = gen_rtx_EXPR_LIST (REG_UNUSED, reg,
					 REG_NOTES (our_prev));

		  for (i = dest_regno; i < dest_endregno; i++)
		    if (! find_regno_note (our_prev, REG_UNUSED, i))
		      break;

		  if (i == dest_endregno)
		    delete_computation (our_prev);
		}
	    }

	  break;
	}

      /* If PAT references the register that dies here, it is an
	 additional use.  Hence any prior SET isn't dead.  However, this
	 insn becomes the new place for the REG_DEAD note.  */
      if (reg_overlap_mentioned_p (reg, pat))
	{
	  XEXP (note, 1) = REG_NOTES (our_prev);
	  REG_NOTES (our_prev) = note;
	  break;
	}
    }
}

/* Delete INSN and recursively delete insns that compute values used only
   by INSN.  This uses the REG_DEAD notes computed during flow analysis.
   If we are running before flow.c, we need do nothing since flow.c will
   delete dead code.  We also can't know if the registers being used are
   dead or not at this point.

   Otherwise, look at all our REG_DEAD notes.  If a previous insn does
   nothing other than set a register that dies in this insn, we can delete
   that insn as well.

   On machines with CC0, if CC0 is used in this insn, we may be able to
   delete the insn that set it.  */

static void
delete_computation (insn)
     rtx insn;
{
  rtx note, next;

#ifdef HAVE_cc0
  if (reg_referenced_p (cc0_rtx, PATTERN (insn)))
    {
      rtx prev = prev_nonnote_insn (insn);
      /* We assume that at this stage
	 CC's are always set explicitly
	 and always immediately before the jump that
	 will use them.  So if the previous insn
	 exists to set the CC's, delete it
	 (unless it performs auto-increments, etc.).  */
      if (prev && GET_CODE (prev) == INSN
	  && sets_cc0_p (PATTERN (prev)))
	{
	  if (sets_cc0_p (PATTERN (prev)) > 0
	      && ! side_effects_p (PATTERN (prev)))
	    delete_computation (prev);
	  else
	    /* Otherwise, show that cc0 won't be used.  */
	    REG_NOTES (prev) = gen_rtx_EXPR_LIST (REG_UNUSED,
						  cc0_rtx, REG_NOTES (prev));
	}
    }
#endif

  for (note = REG_NOTES (insn); note; note = next)
    {
      next = XEXP (note, 1);

      if (REG_NOTE_KIND (note) != REG_DEAD
	  /* Verify that the REG_NOTE is legitimate.  */
	  || GET_CODE (XEXP (note, 0)) != REG)
	continue;

      delete_prior_computation (note, insn);
    }

  delete_related_insns (insn);
}

/* Delete insn INSN from the chain of insns and update label ref counts
   and delete insns now unreachable. 

   Returns the first insn after INSN that was not deleted. 

   Usage of this instruction is deprecated.  Use delete_insn instead and
   subsequent cfg_cleanup pass to delete unreachable code if needed.  */

rtx
delete_related_insns (insn)
     rtx insn;
{
  int was_code_label = (GET_CODE (insn) == CODE_LABEL);
  rtx note;
  rtx next = NEXT_INSN (insn), prev = PREV_INSN (insn);

  while (next && INSN_DELETED_P (next))
    next = NEXT_INSN (next);

  /* This insn is already deleted => return first following nondeleted.  */
  if (INSN_DELETED_P (insn))
    return next;

  delete_insn (insn);

  /* If instruction is followed by a barrier,
     delete the barrier too.  */

  if (next != 0 && GET_CODE (next) == BARRIER)
    delete_insn (next);

  /* If deleting a jump, decrement the count of the label,
     and delete the label if it is now unused.  */

  if (GET_CODE (insn) == JUMP_INSN && JUMP_LABEL (insn))
    {
      rtx lab = JUMP_LABEL (insn), lab_next;

      if (LABEL_NUSES (lab) == 0)
	{
	  /* This can delete NEXT or PREV,
	     either directly if NEXT is JUMP_LABEL (INSN),
	     or indirectly through more levels of jumps.  */
	  delete_related_insns (lab);

	  /* I feel a little doubtful about this loop,
	     but I see no clean and sure alternative way
	     to find the first insn after INSN that is not now deleted.
	     I hope this works.  */
	  while (next && INSN_DELETED_P (next))
	    next = NEXT_INSN (next);
	  return next;
	}
      else if ((lab_next = next_nonnote_insn (lab)) != NULL
	       && GET_CODE (lab_next) == JUMP_INSN
	       && (GET_CODE (PATTERN (lab_next)) == ADDR_VEC
		   || GET_CODE (PATTERN (lab_next)) == ADDR_DIFF_VEC))
	{
	  /* If we're deleting the tablejump, delete the dispatch table.
	     We may not be able to kill the label immediately preceding
	     just yet, as it might be referenced in code leading up to
	     the tablejump.  */
	  delete_related_insns (lab_next);
	}
    }

  /* Likewise if we're deleting a dispatch table.  */

  if (GET_CODE (insn) == JUMP_INSN
      && (GET_CODE (PATTERN (insn)) == ADDR_VEC
	  || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC))
    {
      rtx pat = PATTERN (insn);
      int i, diff_vec_p = GET_CODE (pat) == ADDR_DIFF_VEC;
      int len = XVECLEN (pat, diff_vec_p);

      for (i = 0; i < len; i++)
	if (LABEL_NUSES (XEXP (XVECEXP (pat, diff_vec_p, i), 0)) == 0)
	  delete_related_insns (XEXP (XVECEXP (pat, diff_vec_p, i), 0));
      while (next && INSN_DELETED_P (next))
	next = NEXT_INSN (next);
      return next;
    }

  /* Likewise for an ordinary INSN / CALL_INSN with a REG_LABEL note.  */
  if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
    for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
      if (REG_NOTE_KIND (note) == REG_LABEL
	  /* This could also be a NOTE_INSN_DELETED_LABEL note.  */
	  && GET_CODE (XEXP (note, 0)) == CODE_LABEL)
	if (LABEL_NUSES (XEXP (note, 0)) == 0)
	  delete_related_insns (XEXP (note, 0));

  while (prev && (INSN_DELETED_P (prev) || GET_CODE (prev) == NOTE))
    prev = PREV_INSN (prev);

  /* If INSN was a label and a dispatch table follows it,
     delete the dispatch table.  The tablejump must have gone already.
     It isn't useful to fall through into a table.  */

  if (was_code_label
      && NEXT_INSN (insn) != 0
      && GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
      && (GET_CODE (PATTERN (NEXT_INSN (insn))) == ADDR_VEC
	  || GET_CODE (PATTERN (NEXT_INSN (insn))) == ADDR_DIFF_VEC))
    next = delete_related_insns (NEXT_INSN (insn));

  /* If INSN was a label, delete insns following it if now unreachable.  */

  if (was_code_label && prev && GET_CODE (prev) == BARRIER)
    {
      RTX_CODE code;
      while (next != 0
	     && (GET_RTX_CLASS (code = GET_CODE (next)) == 'i'
		 || code == NOTE || code == BARRIER
		 || (code == CODE_LABEL && INSN_DELETED_P (next))))
	{
	  if (code == NOTE
	      && NOTE_LINE_NUMBER (next) != NOTE_INSN_FUNCTION_END)
	    next = NEXT_INSN (next);
	  /* Keep going past other deleted labels to delete what follows.  */
	  else if (code == CODE_LABEL && INSN_DELETED_P (next))
	    next = NEXT_INSN (next);
	  else
	    /* Note: if this deletes a jump, it can cause more
	       deletion of unreachable code, after a different label.
	       As long as the value from this recursive call is correct,
	       this invocation functions correctly.  */
	    next = delete_related_insns (next);
	}
    }

  return next;
}

/* Advance from INSN till reaching something not deleted
   then return that.  May return INSN itself.  */

rtx
next_nondeleted_insn (insn)
     rtx insn;
{
  while (INSN_DELETED_P (insn))
    insn = NEXT_INSN (insn);
  return insn;
}

/* Delete a range of insns from FROM to TO, inclusive.
   This is for the sake of peephole optimization, so assume
   that whatever these insns do will still be done by a new
   peephole insn that will replace them.  */

void
delete_for_peephole (from, to)
     rtx from, to;
{
  rtx insn = from;

  while (1)
    {
      rtx next = NEXT_INSN (insn);
      rtx prev = PREV_INSN (insn);

      if (GET_CODE (insn) != NOTE)
	{
	  INSN_DELETED_P (insn) = 1;

	  /* Patch this insn out of the chain.  */
	  /* We don't do this all at once, because we
	     must preserve all NOTEs.  */
	  if (prev)
	    NEXT_INSN (prev) = next;

	  if (next)
	    PREV_INSN (next) = prev;
	}

      if (insn == to)
	break;
      insn = next;
    }

  /* Note that if TO is an unconditional jump
     we *do not* delete the BARRIER that follows,
     since the peephole that replaces this sequence
     is also an unconditional jump in that case.  */
}

/* We have determined that INSN is never reached, and are about to
   delete it.  Print a warning if the user asked for one.

   To try to make this warning more useful, this should only be called
   once per basic block not reached, and it only warns when the basic
   block contains more than one line from the current function, and
   contains at least one operation.  CSE and inlining can duplicate insns,
   so it's possible to get spurious warnings from this.  */

void
never_reached_warning (avoided_insn, finish)
     rtx avoided_insn, finish;
{
  rtx insn;
  rtx a_line_note = NULL;
  int two_avoided_lines = 0, contains_insn = 0, reached_end = 0;

  if (! warn_notreached)
    return;

  /* Scan forwards, looking at LINE_NUMBER notes, until
     we hit a LABEL or we run out of insns.  */

  for (insn = avoided_insn; insn != NULL; insn = NEXT_INSN (insn))
    {
      if (finish == NULL && GET_CODE (insn) == CODE_LABEL)
	break;

      if (GET_CODE (insn) == NOTE		/* A line number note?  */
	  && NOTE_LINE_NUMBER (insn) >= 0)
	{
	  if (a_line_note == NULL)
	    a_line_note = insn;
	  else
	    two_avoided_lines |= (NOTE_LINE_NUMBER (a_line_note)
				  != NOTE_LINE_NUMBER (insn));
	}
      else if (INSN_P (insn))
	{
	  if (reached_end)
	    break;
	  contains_insn = 1;
	}

      if (insn == finish)
	reached_end = 1;
    }
  if (two_avoided_lines && contains_insn)
    warning_with_file_and_line (NOTE_SOURCE_FILE (a_line_note),
				NOTE_LINE_NUMBER (a_line_note),
				"will never be executed");
}

/* Throughout LOC, redirect OLABEL to NLABEL.  Treat null OLABEL or
   NLABEL as a return.  Accrue modifications into the change group.  */

static void
redirect_exp_1 (loc, olabel, nlabel, insn)
     rtx *loc;
     rtx olabel, nlabel;
     rtx insn;
{
  rtx x = *loc;
  RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  if (code == LABEL_REF)
    {
      if (XEXP (x, 0) == olabel)
	{
	  rtx n;
	  if (nlabel)
	    n = gen_rtx_LABEL_REF (VOIDmode, nlabel);
	  else
	    n = gen_rtx_RETURN (VOIDmode);

	  validate_change (insn, loc, n, 1);
	  return;
	}
    }
  else if (code == RETURN && olabel == 0)
    {
      x = gen_rtx_LABEL_REF (VOIDmode, nlabel);
      if (loc == &PATTERN (insn))
	x = gen_rtx_SET (VOIDmode, pc_rtx, x);
      validate_change (insn, loc, x, 1);
      return;
    }

  if (code == SET && nlabel == 0 && SET_DEST (x) == pc_rtx
      && GET_CODE (SET_SRC (x)) == LABEL_REF
      && XEXP (SET_SRC (x), 0) == olabel)
    {
      validate_change (insn, loc, gen_rtx_RETURN (VOIDmode), 1);
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	redirect_exp_1 (&XEXP (x, i), olabel, nlabel, insn);
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    redirect_exp_1 (&XVECEXP (x, i, j), olabel, nlabel, insn);
	}
    }
}

/* Similar, but apply the change group and report success or failure.  */

static int
redirect_exp (olabel, nlabel, insn)
     rtx olabel, nlabel;
     rtx insn;
{
  rtx *loc;

  if (GET_CODE (PATTERN (insn)) == PARALLEL)
    loc = &XVECEXP (PATTERN (insn), 0, 0);
  else
    loc = &PATTERN (insn);

  redirect_exp_1 (loc, olabel, nlabel, insn);
  if (num_validated_changes () == 0)
    return 0;

  return apply_change_group ();
}

/* Make JUMP go to NLABEL instead of where it jumps now.  Accrue
   the modifications into the change group.  Return false if we did
   not see how to do that.  */

int
redirect_jump_1 (jump, nlabel)
     rtx jump, nlabel;
{
  int ochanges = num_validated_changes ();
  rtx *loc;

  if (GET_CODE (PATTERN (jump)) == PARALLEL)
    loc = &XVECEXP (PATTERN (jump), 0, 0);
  else
    loc = &PATTERN (jump);

  redirect_exp_1 (loc, JUMP_LABEL (jump), nlabel, jump);
  return num_validated_changes () > ochanges;
}

/* Make JUMP go to NLABEL instead of where it jumps now.  If the old
   jump target label is unused as a result, it and the code following
   it may be deleted.

   If NLABEL is zero, we are to turn the jump into a (possibly conditional)
   RETURN insn.

   The return value will be 1 if the change was made, 0 if it wasn't
   (this can only occur for NLABEL == 0).  */

int
redirect_jump (jump, nlabel, delete_unused)
     rtx jump, nlabel;
     int delete_unused;
{
  rtx olabel = JUMP_LABEL (jump);

  if (nlabel == olabel)
    return 1;

  if (! redirect_exp (olabel, nlabel, jump))
    return 0;

  JUMP_LABEL (jump) = nlabel;
  if (nlabel)
    ++LABEL_NUSES (nlabel);

  /* If we're eliding the jump over exception cleanups at the end of a
     function, move the function end note so that -Wreturn-type works.  */
  if (olabel && nlabel
      && NEXT_INSN (olabel)
      && GET_CODE (NEXT_INSN (olabel)) == NOTE
      && NOTE_LINE_NUMBER (NEXT_INSN (olabel)) == NOTE_INSN_FUNCTION_END)
    emit_note_after (NOTE_INSN_FUNCTION_END, nlabel);

  if (olabel && --LABEL_NUSES (olabel) == 0 && delete_unused
      /* Undefined labels will remain outside the insn stream.  */
      && INSN_UID (olabel))
    delete_related_insns (olabel);

  return 1;
}

/* Invert the jump condition of rtx X contained in jump insn, INSN.
   Accrue the modifications into the change group.  */

static void
invert_exp_1 (insn)
     rtx insn;
{
  RTX_CODE code;
  rtx x = pc_set (insn);

  if (!x)
    abort ();
  x = SET_SRC (x);

  code = GET_CODE (x);

  if (code == IF_THEN_ELSE)
    {
      rtx comp = XEXP (x, 0);
      rtx tem;
      enum rtx_code reversed_code;

      /* We can do this in two ways:  The preferable way, which can only
	 be done if this is not an integer comparison, is to reverse
	 the comparison code.  Otherwise, swap the THEN-part and ELSE-part
	 of the IF_THEN_ELSE.  If we can't do either, fail.  */

      reversed_code = reversed_comparison_code (comp, insn);

      if (reversed_code != UNKNOWN)
	{
	  validate_change (insn, &XEXP (x, 0),
			   gen_rtx_fmt_ee (reversed_code,
					   GET_MODE (comp), XEXP (comp, 0),
					   XEXP (comp, 1)),
			   1);
	  return;
	}

      tem = XEXP (x, 1);
      validate_change (insn, &XEXP (x, 1), XEXP (x, 2), 1);
      validate_change (insn, &XEXP (x, 2), tem, 1);
    }
  else
    abort ();
}

/* Invert the jump condition of conditional jump insn, INSN.

   Return 1 if we can do so, 0 if we cannot find a way to do so that
   matches a pattern.  */

static int
invert_exp (insn)
     rtx insn;
{
  invert_exp_1 (insn);
  if (num_validated_changes () == 0)
    return 0;

  return apply_change_group ();
}

/* Invert the condition of the jump JUMP, and make it jump to label
   NLABEL instead of where it jumps now.  Accrue changes into the
   change group.  Return false if we didn't see how to perform the
   inversion and redirection.  */

int
invert_jump_1 (jump, nlabel)
     rtx jump, nlabel;
{
  int ochanges;

  ochanges = num_validated_changes ();
  invert_exp_1 (jump);
  if (num_validated_changes () == ochanges)
    return 0;

  return redirect_jump_1 (jump, nlabel);
}

/* Invert the condition of the jump JUMP, and make it jump to label
   NLABEL instead of where it jumps now.  Return true if successful.  */

int
invert_jump (jump, nlabel, delete_unused)
     rtx jump, nlabel;
     int delete_unused;
{
  /* We have to either invert the condition and change the label or
     do neither.  Either operation could fail.  We first try to invert
     the jump. If that succeeds, we try changing the label.  If that fails,
     we invert the jump back to what it was.  */

  if (! invert_exp (jump))
    return 0;

  if (redirect_jump (jump, nlabel, delete_unused))
    {
      invert_br_probabilities (jump);

      return 1;
    }

  if (! invert_exp (jump))
    /* This should just be putting it back the way it was.  */
    abort ();

  return 0;
}


/* Like rtx_equal_p except that it considers two REGs as equal
   if they renumber to the same value and considers two commutative
   operations to be the same if the order of the operands has been
   reversed.

   ??? Addition is not commutative on the PA due to the weird implicit
   space register selection rules for memory addresses.  Therefore, we
   don't consider a + b == b + a.

   We could/should make this test a little tighter.  Possibly only
   disabling it on the PA via some backend macro or only disabling this
   case when the PLUS is inside a MEM.  */

int
rtx_renumbered_equal_p (x, y)
     rtx x, y;
{
  int i;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;

  if (x == y)
    return 1;

  if ((code == REG || (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG))
      && (GET_CODE (y) == REG || (GET_CODE (y) == SUBREG
				  && GET_CODE (SUBREG_REG (y)) == REG)))
    {
      int reg_x = -1, reg_y = -1;
      int byte_x = 0, byte_y = 0;

      if (GET_MODE (x) != GET_MODE (y))
	return 0;

      /* If we haven't done any renumbering, don't
	 make any assumptions.  */
      if (reg_renumber == 0)
	return rtx_equal_p (x, y);

      if (code == SUBREG)
	{
	  reg_x = REGNO (SUBREG_REG (x));
	  byte_x = SUBREG_BYTE (x);

	  if (reg_renumber[reg_x] >= 0)
	    {
	      reg_x = subreg_regno_offset (reg_renumber[reg_x],
					   GET_MODE (SUBREG_REG (x)),
					   byte_x,
					   GET_MODE (x));
	      byte_x = 0;
	    }
	}
      else
	{
	  reg_x = REGNO (x);
	  if (reg_renumber[reg_x] >= 0)
	    reg_x = reg_renumber[reg_x];
	}

      if (GET_CODE (y) == SUBREG)
	{
	  reg_y = REGNO (SUBREG_REG (y));
	  byte_y = SUBREG_BYTE (y);

	  if (reg_renumber[reg_y] >= 0)
	    {
	      reg_y = subreg_regno_offset (reg_renumber[reg_y],
					   GET_MODE (SUBREG_REG (y)),
					   byte_y,
					   GET_MODE (y));
	      byte_y = 0;
	    }
	}
      else
	{
	  reg_y = REGNO (y);
	  if (reg_renumber[reg_y] >= 0)
	    reg_y = reg_renumber[reg_y];
	}

      return reg_x >= 0 && reg_x == reg_y && byte_x == byte_y;
    }

  /* Now we have disposed of all the cases
     in which different rtx codes can match.  */
  if (code != GET_CODE (y))
    return 0;

  switch (code)
    {
    case PC:
    case CC0:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    case CONST_INT:
      return INTVAL (x) == INTVAL (y);

    case LABEL_REF:
      /* We can't assume nonlocal labels have their following insns yet.  */
      if (LABEL_REF_NONLOCAL_P (x) || LABEL_REF_NONLOCAL_P (y))
	return XEXP (x, 0) == XEXP (y, 0);

      /* Two label-refs are equivalent if they point at labels
	 in the same position in the instruction stream.  */
      return (next_real_insn (XEXP (x, 0))
	      == next_real_insn (XEXP (y, 0)));

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case CODE_LABEL:
      /* If we didn't match EQ equality above, they aren't the same.  */
      return 0;

    default:
      break;
    }

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* For commutative operations, the RTX match if the operand match in any
     order.  Also handle the simple binary and unary cases without a loop.

     ??? Don't consider PLUS a commutative operator; see comments above.  */
  if ((code == EQ || code == NE || GET_RTX_CLASS (code) == 'c')
      && code != PLUS)
    return ((rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 0))
	     && rtx_renumbered_equal_p (XEXP (x, 1), XEXP (y, 1)))
	    || (rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 1))
		&& rtx_renumbered_equal_p (XEXP (x, 1), XEXP (y, 0))));
  else if (GET_RTX_CLASS (code) == '<' || GET_RTX_CLASS (code) == '2')
    return (rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 0))
	    && rtx_renumbered_equal_p (XEXP (x, 1), XEXP (y, 1)));
  else if (GET_RTX_CLASS (code) == '1')
    return rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 0));

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      int j;
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 't':
	  if (XTREE (x, i) != XTREE (y, i))
	    return 0;
	  break;

	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'e':
	  if (! rtx_renumbered_equal_p (XEXP (x, i), XEXP (y, i)))
	    return 0;
	  break;

	case 'u':
	  if (XEXP (x, i) != XEXP (y, i))
	    return 0;
	  /* fall through.  */
	case '0':
	  break;

	case 'E':
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (!rtx_renumbered_equal_p (XVECEXP (x, i, j), XVECEXP (y, i, j)))
	      return 0;
	  break;

	default:
	  abort ();
	}
    }
  return 1;
}

/* If X is a hard register or equivalent to one or a subregister of one,
   return the hard register number.  If X is a pseudo register that was not
   assigned a hard register, return the pseudo register number.  Otherwise,
   return -1.  Any rtx is valid for X.  */

int
true_regnum (x)
     rtx x;
{
  if (GET_CODE (x) == REG)
    {
      if (REGNO (x) >= FIRST_PSEUDO_REGISTER && reg_renumber[REGNO (x)] >= 0)
	return reg_renumber[REGNO (x)];
      return REGNO (x);
    }
  if (GET_CODE (x) == SUBREG)
    {
      int base = true_regnum (SUBREG_REG (x));
      if (base >= 0 && base < FIRST_PSEUDO_REGISTER)
	return base + subreg_regno_offset (REGNO (SUBREG_REG (x)),
					   GET_MODE (SUBREG_REG (x)),
					   SUBREG_BYTE (x), GET_MODE (x));
    }
  return -1;
}
