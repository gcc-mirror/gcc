/* Optimize jump instructions, for GNU compiler.
   Copyright (C) 1987, 88, 89, 91-99, 2000 Free Software Foundation, Inc.

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


/* This is the jump-optimization pass of the compiler.
   It is run two or three times: once before cse, sometimes once after cse,
   and once after reload (before final).

   jump_optimize deletes unreachable code and labels that are not used.
   It also deletes jumps that jump to the following insn,
   and simplifies jumps around unconditional jumps and jumps
   to unconditional jumps.

   Each CODE_LABEL has a count of the times it is used
   stored in the LABEL_NUSES internal field, and each JUMP_INSN
   has one label that it refers to stored in the
   JUMP_LABEL internal field.  With this we can detect labels that
   become unused because of the deletion of all the jumps that
   formerly used them.  The JUMP_LABEL info is sometimes looked
   at by later passes.

   Optionally, cross-jumping can be done.  Currently it is done
   only the last time (when after reload and before final).
   In fact, the code for cross-jumping now assumes that register
   allocation has been done, since it uses `rtx_renumbered_equal_p'.

   Jump optimization is done after cse when cse's constant-propagation
   causes jumps to become unconditional or to be deleted.

   Unreachable loops are not detected here, because the labels
   have references and the insns appear reachable from the labels.
   find_basic_blocks in flow.c finds and deletes such loops.

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
#include "insn-flags.h"
#include "insn-attr.h"
#include "recog.h"
#include "function.h"
#include "expr.h"
#include "real.h"
#include "except.h"
#include "toplev.h"

/* ??? Eventually must record somehow the labels used by jumps
   from nested functions.  */
/* Pre-record the next or previous real insn for each label?
   No, this pass is very fast anyway.  */
/* Condense consecutive labels?
   This would make life analysis faster, maybe.  */
/* Optimize jump y; x: ... y: jumpif... x?
   Don't know if it is worth bothering with.  */
/* Optimize two cases of conditional jump to conditional jump?
   This can never delete any instruction or make anything dead,
   or even change what is live at any point.
   So perhaps let combiner do it.  */

/* Vector indexed by uid.
   For each CODE_LABEL, index by its uid to get first unconditional jump
   that jumps to the label.
   For each JUMP_INSN, index by its uid to get the next unconditional jump
   that jumps to the same label.
   Element 0 is the start of a chain of all return insns.
   (It is safe to use element 0 because insn uid 0 is not used.  */

static rtx *jump_chain;

/* Maximum index in jump_chain.  */

static int max_jump_chain;

/* Set nonzero by jump_optimize if control can fall through
   to the end of the function.  */
int can_reach_end;

/* Indicates whether death notes are significant in cross jump analysis.
   Normally they are not significant, because of A and B jump to C,
   and R dies in A, it must die in B.  But this might not be true after
   stack register conversion, and we must compare death notes in that
   case.  */

static int cross_jump_death_matters = 0;

static int init_label_info		PARAMS ((rtx));
static void delete_barrier_successors	PARAMS ((rtx));
static void mark_all_labels		PARAMS ((rtx, int));
static rtx delete_unreferenced_labels	PARAMS ((rtx));
static void delete_noop_moves		PARAMS ((rtx));
static int calculate_can_reach_end	PARAMS ((rtx, int, int));
static int duplicate_loop_exit_test	PARAMS ((rtx));
static void find_cross_jump		PARAMS ((rtx, rtx, int, rtx *, rtx *));
static void do_cross_jump		PARAMS ((rtx, rtx, rtx));
static int jump_back_p			PARAMS ((rtx, rtx));
static int tension_vector_labels	PARAMS ((rtx, int));
static void mark_jump_label		PARAMS ((rtx, rtx, int));
static void delete_computation		PARAMS ((rtx));
static void delete_from_jump_chain	PARAMS ((rtx));
static int delete_labelref_insn		PARAMS ((rtx, rtx, int));
static void mark_modified_reg		PARAMS ((rtx, rtx, void *));
static void redirect_tablejump		PARAMS ((rtx, rtx));
static void jump_optimize_1		PARAMS ((rtx, int, int, int, int));
#if ! defined(HAVE_cc0) && ! defined(HAVE_conditional_arithmetic)
static rtx find_insert_position         PARAMS ((rtx, rtx));
#endif
static int returnjump_p_1	        PARAMS ((rtx *, void *));
static void delete_prior_computation    PARAMS ((rtx, rtx));

/* Main external entry point into the jump optimizer.  See comments before
   jump_optimize_1 for descriptions of the arguments.  */
void
jump_optimize (f, cross_jump, noop_moves, after_regscan)
     rtx f;
     int cross_jump;
     int noop_moves;
     int after_regscan;
{
  jump_optimize_1 (f, cross_jump, noop_moves, after_regscan, 0);
}

/* Alternate entry into the jump optimizer.  This entry point only rebuilds
   the JUMP_LABEL field in jumping insns and REG_LABEL notes in non-jumping
   instructions.  */
void
rebuild_jump_labels (f)
     rtx f;
{
  jump_optimize_1 (f, 0, 0, 0, 1);
}


/* Delete no-op jumps and optimize jumps to jumps
   and jumps around jumps.
   Delete unused labels and unreachable code.

   If CROSS_JUMP is 1, detect matching code
   before a jump and its destination and unify them.
   If CROSS_JUMP is 2, do cross-jumping, but pay attention to death notes.

   If NOOP_MOVES is nonzero, delete no-op move insns.

   If AFTER_REGSCAN is nonzero, then this jump pass is being run immediately
   after regscan, and it is safe to use regno_first_uid and regno_last_uid.

   If MARK_LABELS_ONLY is nonzero, then we only rebuild the jump chain
   and JUMP_LABEL field for jumping insns.

   If `optimize' is zero, don't change any code,
   just determine whether control drops off the end of the function.
   This case occurs when we have -W and not -O.
   It works because `delete_insn' checks the value of `optimize'
   and refrains from actually deleting when that is 0.  */

static void
jump_optimize_1 (f, cross_jump, noop_moves, after_regscan, mark_labels_only)
     rtx f;
     int cross_jump;
     int noop_moves;
     int after_regscan;
     int mark_labels_only;
{
  register rtx insn, next;
  int changed;
  int old_max_reg;
  int first = 1;
  int max_uid = 0;
  rtx last_insn;

  cross_jump_death_matters = (cross_jump == 2);
  max_uid = init_label_info (f) + 1;

  /* If we are performing cross jump optimizations, then initialize
     tables mapping UIDs to EH regions to avoid incorrect movement
     of insns from one EH region to another.  */
  if (flag_exceptions && cross_jump)
    init_insn_eh_region (f, max_uid);

  delete_barrier_successors (f);

  /* Leave some extra room for labels and duplicate exit test insns
     we make.  */
  max_jump_chain = max_uid * 14 / 10;
  jump_chain = (rtx *) xcalloc (max_jump_chain, sizeof (rtx));

  mark_all_labels (f, cross_jump);

  /* Keep track of labels used from static data;
     they cannot ever be deleted.  */

  for (insn = forced_labels; insn; insn = XEXP (insn, 1))
    LABEL_NUSES (XEXP (insn, 0))++;

  check_exception_handler_labels ();

  /* Keep track of labels used for marking handlers for exception
     regions; they cannot usually be deleted.  */

  for (insn = exception_handler_labels; insn; insn = XEXP (insn, 1))
    LABEL_NUSES (XEXP (insn, 0))++;

  /* Quit now if we just wanted to rebuild the JUMP_LABEL and REG_LABEL
     notes and recompute LABEL_NUSES.  */
  if (mark_labels_only)
    goto end;

  exception_optimize ();

  last_insn = delete_unreferenced_labels (f);

  if (optimize == 0)
    {
      /* CAN_REACH_END is persistent for each function.  Once set it should
	 not be cleared.  This is especially true for the case where we
	 delete the NOTE_FUNCTION_END note.  CAN_REACH_END is cleared by
	 the front-end before compiling each function.  */
      if (calculate_can_reach_end (last_insn, 1, 0))
	can_reach_end = 1;

      /* Zero the "deleted" flag of all the "deleted" insns.  */
      for (insn = f; insn; insn = NEXT_INSN (insn))
	INSN_DELETED_P (insn) = 0;
      
      goto end;
    }

#ifdef HAVE_return
  if (HAVE_return)
    {
      /* If we fall through to the epilogue, see if we can insert a RETURN insn
	 in front of it.  If the machine allows it at this point (we might be
	 after reload for a leaf routine), it will improve optimization for it
	 to be there.  */
      insn = get_last_insn ();
      while (insn && GET_CODE (insn) == NOTE)
	insn = PREV_INSN (insn);

      if (insn && GET_CODE (insn) != BARRIER)
	{
	  emit_jump_insn (gen_return ());
	  emit_barrier ();
	}
    }
#endif

  if (noop_moves)
    delete_noop_moves (f);

  /* If we haven't yet gotten to reload and we have just run regscan,
     delete any insn that sets a register that isn't used elsewhere.
     This helps some of the optimizations below by having less insns
     being jumped around.  */

  if (! reload_completed && after_regscan)
    for (insn = f; insn; insn = next)
      {
	rtx set = single_set (insn);

	next = NEXT_INSN (insn);

	if (set && GET_CODE (SET_DEST (set)) == REG
	    && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER
	    && REGNO_FIRST_UID (REGNO (SET_DEST (set))) == INSN_UID (insn)
	    /* We use regno_last_note_uid so as not to delete the setting
	       of a reg that's used in notes.  A subsequent optimization
	       might arrange to use that reg for real.  */	       
	    && REGNO_LAST_NOTE_UID (REGNO (SET_DEST (set))) == INSN_UID (insn)
	    && ! side_effects_p (SET_SRC (set))
	    && ! find_reg_note (insn, REG_RETVAL, 0)
	    /* An ADDRESSOF expression can turn into a use of the internal arg
	       pointer, so do not delete the initialization of the internal
	       arg pointer yet.  If it is truly dead, flow will delete the
	       initializing insn.  */
	    && SET_DEST (set) != current_function_internal_arg_pointer)
	  delete_insn (insn);
      }

  /* Now iterate optimizing jumps until nothing changes over one pass.  */
  changed = 1;
  old_max_reg = max_reg_num ();
  while (changed)
    {
      changed = 0;

      for (insn = f; insn; insn = next)
	{
	  rtx reallabelprev;
	  rtx temp, temp1, temp2 = NULL_RTX, temp3, temp4, temp5, temp6;
	  rtx nlabel;
	  int this_is_simplejump, this_is_condjump, reversep = 0;
	  int this_is_condjump_in_parallel;

	  next = NEXT_INSN (insn);

	  /* See if this is a NOTE_INSN_LOOP_BEG followed by an unconditional
	     jump.  Try to optimize by duplicating the loop exit test if so.
	     This is only safe immediately after regscan, because it uses
	     the values of regno_first_uid and regno_last_uid.  */
	  if (after_regscan && GET_CODE (insn) == NOTE
	      && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
	      && (temp1 = next_nonnote_insn (insn)) != 0
	      && simplejump_p (temp1))
	    {
	      temp = PREV_INSN (insn);
	      if (duplicate_loop_exit_test (insn))
		{
		  changed = 1;
		  next = NEXT_INSN (temp);
		  continue;
		}
	    }

	  if (GET_CODE (insn) != JUMP_INSN)
	    continue;

	  this_is_simplejump = simplejump_p (insn);
	  this_is_condjump = condjump_p (insn);
	  this_is_condjump_in_parallel = condjump_in_parallel_p (insn);

	  /* Tension the labels in dispatch tables.  */

	  if (GET_CODE (PATTERN (insn)) == ADDR_VEC)
	    changed |= tension_vector_labels (PATTERN (insn), 0);
	  if (GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
	    changed |= tension_vector_labels (PATTERN (insn), 1);

	  /* See if this jump goes to another jump and redirect if so.  */
	  nlabel = follow_jumps (JUMP_LABEL (insn));
	  if (nlabel != JUMP_LABEL (insn))
	    changed |= redirect_jump (insn, nlabel);

	  /* If a dispatch table always goes to the same place,
	     get rid of it and replace the insn that uses it.  */

	  if (GET_CODE (PATTERN (insn)) == ADDR_VEC
	      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
	    {
	      int i;
	      rtx pat = PATTERN (insn);
	      int diff_vec_p = GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC;
	      int len = XVECLEN (pat, diff_vec_p);
	      rtx dispatch = prev_real_insn (insn);
	      rtx set;

	      for (i = 0; i < len; i++)
		if (XEXP (XVECEXP (pat, diff_vec_p, i), 0)
		    != XEXP (XVECEXP (pat, diff_vec_p, 0), 0))
		  break;

	      if (i == len
		  && dispatch != 0
		  && GET_CODE (dispatch) == JUMP_INSN
		  && JUMP_LABEL (dispatch) != 0
		  /* Don't mess with a casesi insn. 
		     XXX according to the comment before computed_jump_p(),
		     all casesi insns should be a parallel of the jump
		     and a USE of a LABEL_REF.  */
		  && ! ((set = single_set (dispatch)) != NULL
			&& (GET_CODE (SET_SRC (set)) == IF_THEN_ELSE))
		  && next_real_insn (JUMP_LABEL (dispatch)) == insn)
		{
		  redirect_tablejump (dispatch,
				      XEXP (XVECEXP (pat, diff_vec_p, 0), 0));
		  changed = 1;
		}
	    }

	  /* If a jump references the end of the function, try to turn
	     it into a RETURN insn, possibly a conditional one.  */
	  if (JUMP_LABEL (insn) != 0
	      && (next_active_insn (JUMP_LABEL (insn)) == 0
		  || GET_CODE (PATTERN (next_active_insn (JUMP_LABEL (insn))))
		      == RETURN))
	    changed |= redirect_jump (insn, NULL_RTX);

	  reallabelprev = prev_active_insn (JUMP_LABEL (insn));

	  /* Detect jump to following insn.  */
	  if (reallabelprev == insn && this_is_condjump)
	    {
	      next = next_real_insn (JUMP_LABEL (insn));
	      delete_jump (insn);
	      changed = 1;
	      continue;
	    }

	  /* Detect a conditional jump going to the same place
	     as an immediately following unconditional jump.  */
	  else if (this_is_condjump
		   && (temp = next_active_insn (insn)) != 0
		   && simplejump_p (temp)
		   && (next_active_insn (JUMP_LABEL (insn))
		       == next_active_insn (JUMP_LABEL (temp))))
	    {
	      /* Don't mess up test coverage analysis.  */
	      temp2 = temp;
	      if (flag_test_coverage && !reload_completed)
		for (temp2 = insn; temp2 != temp; temp2 = NEXT_INSN (temp2))
		  if (GET_CODE (temp2) == NOTE && NOTE_LINE_NUMBER (temp2) > 0)
		    break;
		  
	      if (temp2 == temp)
		{
		  delete_jump (insn);
		  changed = 1;
		  continue;
		}
	    }

	  /* Detect a conditional jump jumping over an unconditional jump.  */

	  else if ((this_is_condjump || this_is_condjump_in_parallel)
		   && ! this_is_simplejump
		   && reallabelprev != 0
		   && GET_CODE (reallabelprev) == JUMP_INSN
		   && prev_active_insn (reallabelprev) == insn
		   && no_labels_between_p (insn, reallabelprev)
		   && simplejump_p (reallabelprev))
	    {
	      /* When we invert the unconditional jump, we will be
		 decrementing the usage count of its old label.
		 Make sure that we don't delete it now because that
		 might cause the following code to be deleted.  */
	      rtx prev_uses = prev_nonnote_insn (reallabelprev);
	      rtx prev_label = JUMP_LABEL (insn);

	      if (prev_label)
		++LABEL_NUSES (prev_label);

	      if (invert_jump (insn, JUMP_LABEL (reallabelprev)))
		{
		  /* It is very likely that if there are USE insns before
		     this jump, they hold REG_DEAD notes.  These REG_DEAD
		     notes are no longer valid due to this optimization,
		     and will cause the life-analysis that following passes
		     (notably delayed-branch scheduling) to think that
		     these registers are dead when they are not.

		     To prevent this trouble, we just remove the USE insns
		     from the insn chain.  */

		  while (prev_uses && GET_CODE (prev_uses) == INSN
			 && GET_CODE (PATTERN (prev_uses)) == USE)
		    {
		      rtx useless = prev_uses;
		      prev_uses = prev_nonnote_insn (prev_uses);
		      delete_insn (useless);
		    }

		  delete_insn (reallabelprev);
		  changed = 1;
		}

	      /* We can now safely delete the label if it is unreferenced
		 since the delete_insn above has deleted the BARRIER.  */
	      if (prev_label && --LABEL_NUSES (prev_label) == 0)
		delete_insn (prev_label);

	      next = NEXT_INSN (insn);
	    }

	  /* If we have an unconditional jump preceded by a USE, try to put
	     the USE before the target and jump there.  This simplifies many
	     of the optimizations below since we don't have to worry about
	     dealing with these USE insns.  We only do this if the label
	     being branch to already has the identical USE or if code
	     never falls through to that label.  */

	  else if (this_is_simplejump
		   && (temp = prev_nonnote_insn (insn)) != 0
		   && GET_CODE (temp) == INSN
		   && GET_CODE (PATTERN (temp)) == USE
		   && (temp1 = prev_nonnote_insn (JUMP_LABEL (insn))) != 0
		   && (GET_CODE (temp1) == BARRIER
		       || (GET_CODE (temp1) == INSN
			   && rtx_equal_p (PATTERN (temp), PATTERN (temp1))))
		   /* Don't do this optimization if we have a loop containing
		      only the USE instruction, and the loop start label has
		      a usage count of 1.  This is because we will redo this
		      optimization everytime through the outer loop, and jump
		      opt will never exit.  */
		   && ! ((temp2 = prev_nonnote_insn (temp)) != 0
			 && temp2 == JUMP_LABEL (insn)
			 && LABEL_NUSES (temp2) == 1))
	    {
	      if (GET_CODE (temp1) == BARRIER)
		{
		  emit_insn_after (PATTERN (temp), temp1);
		  temp1 = NEXT_INSN (temp1);
		}

	      delete_insn (temp);
	      redirect_jump (insn, get_label_before (temp1));
	      reallabelprev = prev_real_insn (temp1);
	      changed = 1;
	      next = NEXT_INSN (insn);
	    }

	  /* Simplify   if (...) x = a; else x = b; by converting it
	     to         x = b; if (...) x = a;
	     if B is sufficiently simple, the test doesn't involve X,
	     and nothing in the test modifies B or X.

	     If we have small register classes, we also can't do this if X
	     is a hard register.

	     If the "x = b;" insn has any REG_NOTES, we don't do this because
	     of the possibility that we are running after CSE and there is a
	     REG_EQUAL note that is only valid if the branch has already been
	     taken.  If we move the insn with the REG_EQUAL note, we may
	     fold the comparison to always be false in a later CSE pass.
	     (We could also delete the REG_NOTES when moving the insn, but it
	     seems simpler to not move it.)  An exception is that we can move
	     the insn if the only note is a REG_EQUAL or REG_EQUIV whose
	     value is the same as "b".

	     INSN is the branch over the `else' part. 

	     We set:

	     TEMP to the jump insn preceding "x = a;"
	     TEMP1 to X
	     TEMP2 to the insn that sets "x = b;"
	     TEMP3 to the insn that sets "x = a;"
	     TEMP4 to the set of "x = b";  */

	  if (this_is_simplejump
	      && (temp3 = prev_active_insn (insn)) != 0
	      && GET_CODE (temp3) == INSN
	      && (temp4 = single_set (temp3)) != 0
	      && GET_CODE (temp1 = SET_DEST (temp4)) == REG
	      && (! SMALL_REGISTER_CLASSES
		  || REGNO (temp1) >= FIRST_PSEUDO_REGISTER)
	      && (temp2 = next_active_insn (insn)) != 0
	      && GET_CODE (temp2) == INSN
	      && (temp4 = single_set (temp2)) != 0
	      && rtx_equal_p (SET_DEST (temp4), temp1)
	      && ! side_effects_p (SET_SRC (temp4))
	      && ! may_trap_p (SET_SRC (temp4))
	      && (REG_NOTES (temp2) == 0
		  || ((REG_NOTE_KIND (REG_NOTES (temp2)) == REG_EQUAL
		       || REG_NOTE_KIND (REG_NOTES (temp2)) == REG_EQUIV)
		      && XEXP (REG_NOTES (temp2), 1) == 0
		      && rtx_equal_p (XEXP (REG_NOTES (temp2), 0),
				      SET_SRC (temp4))))
	      && (temp = prev_active_insn (temp3)) != 0
	      && condjump_p (temp) && ! simplejump_p (temp)
	      /* TEMP must skip over the "x = a;" insn */
	      && prev_real_insn (JUMP_LABEL (temp)) == insn
	      && no_labels_between_p (insn, JUMP_LABEL (temp))
	      /* There must be no other entries to the "x = b;" insn.  */
	      && no_labels_between_p (JUMP_LABEL (temp), temp2)
	      /* INSN must either branch to the insn after TEMP2 or the insn
		 after TEMP2 must branch to the same place as INSN.  */
	      && (reallabelprev == temp2
		  || ((temp5 = next_active_insn (temp2)) != 0
		      && simplejump_p (temp5)
		      && JUMP_LABEL (temp5) == JUMP_LABEL (insn))))
	    {
	      /* The test expression, X, may be a complicated test with
		 multiple branches.  See if we can find all the uses of
		 the label that TEMP branches to without hitting a CALL_INSN
		 or a jump to somewhere else.  */
	      rtx target = JUMP_LABEL (temp);
	      int nuses = LABEL_NUSES (target);
	      rtx p;
#ifdef HAVE_cc0
	      rtx q;
#endif

	      /* Set P to the first jump insn that goes around "x = a;".  */
	      for (p = temp; nuses && p; p = prev_nonnote_insn (p))
		{
		  if (GET_CODE (p) == JUMP_INSN)
		    {
		      if (condjump_p (p) && ! simplejump_p (p)
			  && JUMP_LABEL (p) == target)
			{
			  nuses--;
			  if (nuses == 0)
			    break;
			}
		      else
			break;
		    }
		  else if (GET_CODE (p) == CALL_INSN)
		    break;
		}

#ifdef HAVE_cc0
	      /* We cannot insert anything between a set of cc and its use
		 so if P uses cc0, we must back up to the previous insn.  */
	      q = prev_nonnote_insn (p);
	      if (q && GET_RTX_CLASS (GET_CODE (q)) == 'i'
		  && sets_cc0_p (PATTERN (q)))
		p = q;
#endif

	      if (p)
		p = PREV_INSN (p);

	      /* If we found all the uses and there was no data conflict, we
		 can move the assignment unless we can branch into the middle
		 from somewhere.  */
	      if (nuses == 0 && p
		  && no_labels_between_p (p, insn)
		  && ! reg_referenced_between_p (temp1, p, NEXT_INSN (temp3))
		  && ! reg_set_between_p (temp1, p, temp3)
		  && (GET_CODE (SET_SRC (temp4)) == CONST_INT
		      || ! modified_between_p (SET_SRC (temp4), p, temp2))
		  /* Verify that registers used by the jump are not clobbered
		     by the instruction being moved.  */
		  && ! regs_set_between_p (PATTERN (temp),
					   PREV_INSN (temp2),
					   NEXT_INSN (temp2)))
		{
		  emit_insn_after_with_line_notes (PATTERN (temp2), p, temp2);
		  delete_insn (temp2);

		  /* Set NEXT to an insn that we know won't go away.  */
		  next = next_active_insn (insn);

		  /* Delete the jump around the set.  Note that we must do
		     this before we redirect the test jumps so that it won't
		     delete the code immediately following the assignment
		     we moved (which might be a jump).  */

		  delete_insn (insn);

		  /* We either have two consecutive labels or a jump to
		     a jump, so adjust all the JUMP_INSNs to branch to where
		     INSN branches to.  */
		  for (p = NEXT_INSN (p); p != next; p = NEXT_INSN (p))
		    if (GET_CODE (p) == JUMP_INSN)
		      redirect_jump (p, target);

		  changed = 1;
		  next = NEXT_INSN (insn);
		  continue;
		}
	    }

	  /* Simplify   if (...) { x = a; goto l; } x = b; by converting it
	     to         x = a; if (...) goto l; x = b;
	     if A is sufficiently simple, the test doesn't involve X,
	     and nothing in the test modifies A or X.

	     If we have small register classes, we also can't do this if X
	     is a hard register.

	     If the "x = a;" insn has any REG_NOTES, we don't do this because
	     of the possibility that we are running after CSE and there is a
	     REG_EQUAL note that is only valid if the branch has already been
	     taken.  If we move the insn with the REG_EQUAL note, we may
	     fold the comparison to always be false in a later CSE pass.
	     (We could also delete the REG_NOTES when moving the insn, but it
	     seems simpler to not move it.)  An exception is that we can move
	     the insn if the only note is a REG_EQUAL or REG_EQUIV whose
	     value is the same as "a".

	     INSN is the goto.

	     We set:

	     TEMP to the jump insn preceding "x = a;"
	     TEMP1 to X
	     TEMP2 to the insn that sets "x = b;"
	     TEMP3 to the insn that sets "x = a;"
	     TEMP4 to the set of "x = a";  */

	  if (this_is_simplejump
	      && (temp2 = next_active_insn (insn)) != 0
	      && GET_CODE (temp2) == INSN
	      && (temp4 = single_set (temp2)) != 0
	      && GET_CODE (temp1 = SET_DEST (temp4)) == REG
	      && (! SMALL_REGISTER_CLASSES
		  || REGNO (temp1) >= FIRST_PSEUDO_REGISTER)
	      && (temp3 = prev_active_insn (insn)) != 0
	      && GET_CODE (temp3) == INSN
	      && (temp4 = single_set (temp3)) != 0
	      && rtx_equal_p (SET_DEST (temp4), temp1)
	      && ! side_effects_p (SET_SRC (temp4))
	      && ! may_trap_p (SET_SRC (temp4))
	      && (REG_NOTES (temp3) == 0
		  || ((REG_NOTE_KIND (REG_NOTES (temp3)) == REG_EQUAL
		       || REG_NOTE_KIND (REG_NOTES (temp3)) == REG_EQUIV)
		      && XEXP (REG_NOTES (temp3), 1) == 0
		      && rtx_equal_p (XEXP (REG_NOTES (temp3), 0),
				      SET_SRC (temp4))))
	      && (temp = prev_active_insn (temp3)) != 0
	      && condjump_p (temp) && ! simplejump_p (temp)
	      /* TEMP must skip over the "x = a;" insn */
	      && prev_real_insn (JUMP_LABEL (temp)) == insn
	      && no_labels_between_p (temp, insn))
	    {
	      rtx prev_label = JUMP_LABEL (temp);
	      rtx insert_after = prev_nonnote_insn (temp);

#ifdef HAVE_cc0
	      /* We cannot insert anything between a set of cc and its use.  */
	      if (insert_after && GET_RTX_CLASS (GET_CODE (insert_after)) == 'i'
		  && sets_cc0_p (PATTERN (insert_after)))
		insert_after = prev_nonnote_insn (insert_after);
#endif
	      ++LABEL_NUSES (prev_label);

	      if (insert_after
		  && no_labels_between_p (insert_after, temp)
		  && ! reg_referenced_between_p (temp1, insert_after, temp3)
		  && ! reg_referenced_between_p (temp1, temp3,
						 NEXT_INSN (temp2))
		  && ! reg_set_between_p (temp1, insert_after, temp)
		  && ! modified_between_p (SET_SRC (temp4), insert_after, temp)
		  /* Verify that registers used by the jump are not clobbered
		     by the instruction being moved.  */
		  && ! regs_set_between_p (PATTERN (temp),
					   PREV_INSN (temp3),
					   NEXT_INSN (temp3))
		  && invert_jump (temp, JUMP_LABEL (insn)))
		{
		  emit_insn_after_with_line_notes (PATTERN (temp3),
						   insert_after, temp3);
		  delete_insn (temp3);
		  delete_insn (insn);
		  /* Set NEXT to an insn that we know won't go away.  */
		  next = temp2;
		  changed = 1;
		}
	      if (prev_label && --LABEL_NUSES (prev_label) == 0)
		delete_insn (prev_label);
	      if (changed)
		continue;
	    }

#if !defined(HAVE_cc0) && !defined(HAVE_conditional_arithmetic)

	  /* If we have if (...) x = exp;  and branches are expensive,
	     EXP is a single insn, does not have any side effects, cannot
	     trap, and is not too costly, convert this to
	     t = exp; if (...) x = t;

	     Don't do this when we have CC0 because it is unlikely to help
	     and we'd need to worry about where to place the new insn and
	     the potential for conflicts.  We also can't do this when we have
	     notes on the insn for the same reason as above.

	     If we have conditional arithmetic, this will make this
	     harder to optimize later and isn't needed, so don't do it
	     in that case either.

	     We set:

	     TEMP to the "x = exp;" insn.
	     TEMP1 to the single set in the "x = exp;" insn.
	     TEMP2 to "x".  */

	  if (! reload_completed
	      && this_is_condjump && ! this_is_simplejump
	      && BRANCH_COST >= 3
	      && (temp = next_nonnote_insn (insn)) != 0
	      && GET_CODE (temp) == INSN
	      && REG_NOTES (temp) == 0
	      && (reallabelprev == temp
		  || ((temp2 = next_active_insn (temp)) != 0
		      && simplejump_p (temp2)
		      && JUMP_LABEL (temp2) == JUMP_LABEL (insn)))
	      && (temp1 = single_set (temp)) != 0
	      && (temp2 = SET_DEST (temp1), GET_CODE (temp2) == REG)
	      && (! SMALL_REGISTER_CLASSES
		  || REGNO (temp2) >= FIRST_PSEUDO_REGISTER)
	      && GET_CODE (SET_SRC (temp1)) != REG
	      && GET_CODE (SET_SRC (temp1)) != SUBREG
	      && GET_CODE (SET_SRC (temp1)) != CONST_INT
	      && ! side_effects_p (SET_SRC (temp1))
	      && ! may_trap_p (SET_SRC (temp1))
	      && rtx_cost (SET_SRC (temp1), SET) < 10)
	    {
	      rtx new = gen_reg_rtx (GET_MODE (temp2));

	      if ((temp3 = find_insert_position (insn, temp))
		  && validate_change (temp, &SET_DEST (temp1), new, 0))
		{
		  next = emit_insn_after (gen_move_insn (temp2, new), insn);
		  emit_insn_after_with_line_notes (PATTERN (temp), 
						   PREV_INSN (temp3), temp);
		  delete_insn (temp);
		  reallabelprev = prev_active_insn (JUMP_LABEL (insn));

		  if (after_regscan)
		    {
		      reg_scan_update (temp3, NEXT_INSN (next), old_max_reg);
		      old_max_reg = max_reg_num ();
		    }
		}
	    }

	  /* Similarly, if it takes two insns to compute EXP but they
	     have the same destination.  Here TEMP3 will be the second
	     insn and TEMP4 the SET from that insn.  */

	  if (! reload_completed
	      && this_is_condjump && ! this_is_simplejump
	      && BRANCH_COST >= 4
	      && (temp = next_nonnote_insn (insn)) != 0
	      && GET_CODE (temp) == INSN
	      && REG_NOTES (temp) == 0
	      && (temp3 = next_nonnote_insn (temp)) != 0
	      && GET_CODE (temp3) == INSN
	      && REG_NOTES (temp3) == 0
	      && (reallabelprev == temp3
		  || ((temp2 = next_active_insn (temp3)) != 0
		      && simplejump_p (temp2)
		      && JUMP_LABEL (temp2) == JUMP_LABEL (insn)))
	      && (temp1 = single_set (temp)) != 0
	      && (temp2 = SET_DEST (temp1), GET_CODE (temp2) == REG)
	      && GET_MODE_CLASS (GET_MODE (temp2)) == MODE_INT
	      && (! SMALL_REGISTER_CLASSES
		  || REGNO (temp2) >= FIRST_PSEUDO_REGISTER)
	      && ! side_effects_p (SET_SRC (temp1))
	      && ! may_trap_p (SET_SRC (temp1))
	      && rtx_cost (SET_SRC (temp1), SET) < 10
	      && (temp4 = single_set (temp3)) != 0
	      && rtx_equal_p (SET_DEST (temp4), temp2)
	      && ! side_effects_p (SET_SRC (temp4))
	      && ! may_trap_p (SET_SRC (temp4))
	      && rtx_cost (SET_SRC (temp4), SET) < 10)
	    {
	      rtx new = gen_reg_rtx (GET_MODE (temp2));

	      if ((temp5 = find_insert_position (insn, temp))
		  && (temp6 = find_insert_position (insn, temp3))
		  && validate_change (temp, &SET_DEST (temp1), new, 0))
		{
		  /* Use the earliest of temp5 and temp6. */
		  if (temp5 != insn)
		    temp6 = temp5;
		  next = emit_insn_after (gen_move_insn (temp2, new), insn);
		  emit_insn_after_with_line_notes (PATTERN (temp),
						   PREV_INSN (temp6), temp);
		  emit_insn_after_with_line_notes
		    (replace_rtx (PATTERN (temp3), temp2, new),
		     PREV_INSN (temp6), temp3);
		  delete_insn (temp);
		  delete_insn (temp3);
		  reallabelprev = prev_active_insn (JUMP_LABEL (insn));

		  if (after_regscan)
		    {
		      reg_scan_update (temp6, NEXT_INSN (next), old_max_reg);
		      old_max_reg = max_reg_num ();
		    }
		}
	    }

	  /* Finally, handle the case where two insns are used to 
	     compute EXP but a temporary register is used.  Here we must
	     ensure that the temporary register is not used anywhere else.  */

	  if (! reload_completed
	      && after_regscan
	      && this_is_condjump && ! this_is_simplejump
	      && BRANCH_COST >= 4
	      && (temp = next_nonnote_insn (insn)) != 0
	      && GET_CODE (temp) == INSN
	      && REG_NOTES (temp) == 0
	      && (temp3 = next_nonnote_insn (temp)) != 0
	      && GET_CODE (temp3) == INSN
	      && REG_NOTES (temp3) == 0
	      && (reallabelprev == temp3
		  || ((temp2 = next_active_insn (temp3)) != 0
		      && simplejump_p (temp2)
		      && JUMP_LABEL (temp2) == JUMP_LABEL (insn)))
	      && (temp1 = single_set (temp)) != 0
	      && (temp5 = SET_DEST (temp1),
		  (GET_CODE (temp5) == REG
		   || (GET_CODE (temp5) == SUBREG
		       && (temp5 = SUBREG_REG (temp5),
			   GET_CODE (temp5) == REG))))
	      && REGNO (temp5) >= FIRST_PSEUDO_REGISTER
	      && REGNO_FIRST_UID (REGNO (temp5)) == INSN_UID (temp)
	      && REGNO_LAST_UID (REGNO (temp5)) == INSN_UID (temp3)
	      && ! side_effects_p (SET_SRC (temp1))
	      && ! may_trap_p (SET_SRC (temp1))
	      && rtx_cost (SET_SRC (temp1), SET) < 10
	      && (temp4 = single_set (temp3)) != 0
	      && (temp2 = SET_DEST (temp4), GET_CODE (temp2) == REG)
	      && GET_MODE_CLASS (GET_MODE (temp2)) == MODE_INT
	      && (! SMALL_REGISTER_CLASSES
		  || REGNO (temp2) >= FIRST_PSEUDO_REGISTER)
	      && rtx_equal_p (SET_DEST (temp4), temp2)
	      && ! side_effects_p (SET_SRC (temp4))
	      && ! may_trap_p (SET_SRC (temp4))
	      && rtx_cost (SET_SRC (temp4), SET) < 10)
	    {
	      rtx new = gen_reg_rtx (GET_MODE (temp2));

	      if ((temp5 = find_insert_position (insn, temp))
		  && (temp6 = find_insert_position (insn, temp3))
		  && validate_change (temp3, &SET_DEST (temp4), new, 0))
		{
		  /* Use the earliest of temp5 and temp6. */
		  if (temp5 != insn)
		    temp6 = temp5;
		  next = emit_insn_after (gen_move_insn (temp2, new), insn);
		  emit_insn_after_with_line_notes (PATTERN (temp),
						   PREV_INSN (temp6), temp);
		  emit_insn_after_with_line_notes (PATTERN (temp3),
						   PREV_INSN (temp6), temp3);
		  delete_insn (temp);
		  delete_insn (temp3);
		  reallabelprev = prev_active_insn (JUMP_LABEL (insn));

		  if (after_regscan)
		    {
		      reg_scan_update (temp6, NEXT_INSN (next), old_max_reg);
		      old_max_reg = max_reg_num ();
		    }
		}
	    }
#endif /* HAVE_cc0 */

#ifdef HAVE_conditional_arithmetic
	  /* ??? This is disabled in genconfig, as this simple-minded
	     transformation can incredibly lengthen register lifetimes.

	     Consider this example from cexp.c's yyparse:

		234 (set (pc)
		      (if_then_else (ne (reg:DI 149) (const_int 0 [0x0]))
		        (label_ref 248) (pc)))
		237 (set (reg/i:DI 0 $0) (const_int 1 [0x1]))
		239 (set (pc) (label_ref 2382))
		248 (code_label ("yybackup"))

	     This will be transformed to:

		237 (set (reg/i:DI 0 $0)
		      (if_then_else:DI (eq (reg:DI 149) (const_int 0 [0x0]))
		        (const_int 1 [0x1]) (reg/i:DI 0 $0)))
		239 (set (pc)
		      (if_then_else (eq (reg:DI 149) (const_int 0 [0x0]))
		        (label_ref 2382) (pc)))

	     which, from this narrow viewpoint looks fine.  Except that
	     between this and 3 other ocurrences of the same pattern, $0
	     is now live for basically the entire function, and we'll 
	     get an abort in caller_save.

	     Any replacement for this code should recall that a set of
	     a register that is not live need not, and indeed should not,
	     be conditionalized.  Either that, or delay the transformation
	     until after register allocation.  */

	  /* See if this is a conditional jump around a small number of
	     instructions that we can conditionalize.  Don't do this before
	     the initial CSE pass or after reload.

	     We reject any insns that have side effects or may trap.
	     Strictly speaking, this is not needed since the machine may
	     support conditionalizing these too, but we won't deal with that
	     now.  Specifically, this means that we can't conditionalize a 
	     CALL_INSN, which some machines, such as the ARC, can do, but
	     this is a very minor optimization.  */
	  if (this_is_condjump && ! this_is_simplejump
	      && cse_not_expected && optimize > 0 && ! reload_completed
	      && BRANCH_COST > 2
	      && can_reverse_comparison_p (XEXP (SET_SRC (PATTERN (insn)), 0),
					   insn))
	    {
	      rtx ourcond = XEXP (SET_SRC (PATTERN (insn)), 0);
	      int num_insns = 0;
	      char *storage = (char *) oballoc (0);
	      int last_insn = 0, failed = 0;
	      rtx changed_jump = 0;

	      ourcond = gen_rtx (reverse_condition (GET_CODE (ourcond)),
				 VOIDmode, XEXP (ourcond, 0),
				 XEXP (ourcond, 1));

	      /* Scan forward BRANCH_COST real insns looking for the JUMP_LABEL
		 of this insn.  We see if we think we can conditionalize the
		 insns we pass.  For now, we only deal with insns that have
		 one SET.  We stop after an insn that modifies anything in
		 OURCOND, if we have too many insns, or if we have an insn
		 with a side effect or that may trip.  Note that we will
		 be modifying any unconditional jumps we encounter to be
		 conditional; this will have the effect of also doing this
		 optimization on the "else" the next time around.  */
	      for (temp1 = NEXT_INSN (insn);
		   num_insns <= BRANCH_COST && ! failed && temp1 != 0
		   && GET_CODE (temp1) != CODE_LABEL;
		   temp1 = NEXT_INSN (temp1))
		{
		  /* Ignore everything but an active insn.  */
		  if (GET_RTX_CLASS (GET_CODE (temp1)) != 'i'
		      || GET_CODE (PATTERN (temp1)) == USE
		      || GET_CODE (PATTERN (temp1)) == CLOBBER)
		    continue;

		  /* If this was an unconditional jump, record it since we'll
		     need to remove the BARRIER if we succeed.  We can only
		     have one such jump since there must be a label after
		     the BARRIER and it's either ours, in which case it's the
		     only one or some other, in which case we'd fail.
		     Likewise if it's a CALL_INSN followed by a BARRIER.  */

		  if (simplejump_p (temp1)
		      || (GET_CODE (temp1) == CALL_INSN
			  && NEXT_INSN (temp1) != 0
			  && GET_CODE (NEXT_INSN (temp1)) == BARRIER))
		    {
		      if (changed_jump == 0)
			changed_jump = temp1;
		      else
			changed_jump
			  = gen_rtx_INSN_LIST (VOIDmode, temp1, changed_jump);
		    }

		  /* See if we are allowed another insn and if this insn
		     if one we think we may be able to handle.  */
		  if (++num_insns > BRANCH_COST
		      || last_insn
		      || (((temp2 = single_set (temp1)) == 0
			   || side_effects_p (SET_SRC (temp2))
			   || may_trap_p (SET_SRC (temp2)))
			  && GET_CODE (temp1) != CALL_INSN))
		      failed = 1;
		  else if (temp2 != 0)
		    validate_change (temp1, &SET_SRC (temp2),
				     gen_rtx_IF_THEN_ELSE
				     (GET_MODE (SET_DEST (temp2)),
				      copy_rtx (ourcond),
				      SET_SRC (temp2), SET_DEST (temp2)),
				     1);
		  else
		    {
		      /* This is a CALL_INSN that doesn't have a SET.  */
		      rtx *call_loc = &PATTERN (temp1);

		      if (GET_CODE (*call_loc) == PARALLEL)
			call_loc = &XVECEXP (*call_loc, 0, 0);

		      validate_change (temp1, call_loc,
				       gen_rtx_IF_THEN_ELSE
				       (VOIDmode, copy_rtx (ourcond),
					*call_loc, const0_rtx),
				       1);
		    }


		  if (modified_in_p (ourcond, temp1))
		    last_insn = 1;
		}

	      /* If we've reached our jump label, haven't failed, and all
		 the changes above are valid, we can delete this jump
		 insn.  Also remove a BARRIER after any jump that used
		 to be unconditional and remove any REG_EQUAL or REG_EQUIV
		 that might have previously been present on insns we
		 made conditional.  */
	      if (temp1 == JUMP_LABEL (insn) && ! failed
		  && apply_change_group ())
		{
		  for (temp1 = NEXT_INSN (insn); temp1 != JUMP_LABEL (insn);
		       temp1 = NEXT_INSN (temp1))
		    if (GET_RTX_CLASS (GET_CODE (temp1)) == 'i')
		      for (temp2 = REG_NOTES (temp1); temp2 != 0;
			   temp2 = XEXP (temp2, 1))
			if (REG_NOTE_KIND (temp2) == REG_EQUAL
			    || REG_NOTE_KIND (temp2) == REG_EQUIV)
			  remove_note (temp1, temp2);

		  if (changed_jump != 0)
		    {
		      while (GET_CODE (changed_jump) == INSN_LIST)
			{
			  delete_barrier (NEXT_INSN (XEXP (changed_jump, 0)));
			  changed_jump = XEXP (changed_jump, 1);
			}

		      delete_barrier (NEXT_INSN (changed_jump));
		    }

		  delete_insn (insn);
		  changed = 1;
		  continue;
		}
	      else
		{
		  cancel_changes (0);
		  obfree (storage);
		}
	    }
#endif
	  /* If branches are expensive, convert
	        if (foo) bar++;    to    bar += (foo != 0);
	     and similarly for "bar--;" 

	     INSN is the conditional branch around the arithmetic.  We set:

	     TEMP is the arithmetic insn.
	     TEMP1 is the SET doing the arithmetic.
	     TEMP2 is the operand being incremented or decremented.
	     TEMP3 to the condition being tested.
	     TEMP4 to the earliest insn used to find the condition.  */

	  if ((BRANCH_COST >= 2
#ifdef HAVE_incscc
	       || HAVE_incscc
#endif
#ifdef HAVE_decscc
	       || HAVE_decscc
#endif
	      )
	      && ! reload_completed
	      && this_is_condjump && ! this_is_simplejump
	      && (temp = next_nonnote_insn (insn)) != 0
	      && (temp1 = single_set (temp)) != 0
	      && (temp2 = SET_DEST (temp1),
		  GET_MODE_CLASS (GET_MODE (temp2)) == MODE_INT)
	      && GET_CODE (SET_SRC (temp1)) == PLUS
	      && (XEXP (SET_SRC (temp1), 1) == const1_rtx
		  || XEXP (SET_SRC (temp1), 1) == constm1_rtx)
	      && rtx_equal_p (temp2, XEXP (SET_SRC (temp1), 0))
	      && ! side_effects_p (temp2)
	      && ! may_trap_p (temp2)
	      /* INSN must either branch to the insn after TEMP or the insn
		 after TEMP must branch to the same place as INSN.  */
	      && (reallabelprev == temp
		  || ((temp3 = next_active_insn (temp)) != 0
		      && simplejump_p (temp3)
		      && JUMP_LABEL (temp3) == JUMP_LABEL (insn)))
	      && (temp3 = get_condition (insn, &temp4)) != 0
	      /* We must be comparing objects whose modes imply the size.
		 We could handle BLKmode if (1) emit_store_flag could
		 and (2) we could find the size reliably.  */
	      && GET_MODE (XEXP (temp3, 0)) != BLKmode
	      && can_reverse_comparison_p (temp3, insn))
	    {
	      rtx temp6, target = 0, seq, init_insn = 0, init = temp2;
	      enum rtx_code code = reverse_condition (GET_CODE (temp3));

	      start_sequence ();

	      /* It must be the case that TEMP2 is not modified in the range
		 [TEMP4, INSN).  The one exception we make is if the insn
		 before INSN sets TEMP2 to something which is also unchanged
		 in that range.  In that case, we can move the initialization
		 into our sequence.  */

	      if ((temp5 = prev_active_insn (insn)) != 0
		  && no_labels_between_p (temp5, insn)
		  && GET_CODE (temp5) == INSN
		  && (temp6 = single_set (temp5)) != 0
		  && rtx_equal_p (temp2, SET_DEST (temp6))
		  && (CONSTANT_P (SET_SRC (temp6))
		      || GET_CODE (SET_SRC (temp6)) == REG
		      || GET_CODE (SET_SRC (temp6)) == SUBREG))
		{
		  emit_insn (PATTERN (temp5));
		  init_insn = temp5;
		  init = SET_SRC (temp6);
		}

	      if (CONSTANT_P (init)
		  || ! reg_set_between_p (init, PREV_INSN (temp4), insn))
		target = emit_store_flag (gen_reg_rtx (GET_MODE (temp2)), code,
					  XEXP (temp3, 0), XEXP (temp3, 1),
					  VOIDmode,
					  (code == LTU || code == LEU
					   || code == GTU || code == GEU), 1);

	      /* If we can do the store-flag, do the addition or
		 subtraction.  */

	      if (target)
		target = expand_binop (GET_MODE (temp2),
				       (XEXP (SET_SRC (temp1), 1) == const1_rtx
					? add_optab : sub_optab),
				       temp2, target, temp2, 0, OPTAB_WIDEN);

	      if (target != 0)
		{
		  /* Put the result back in temp2 in case it isn't already.
		     Then replace the jump, possible a CC0-setting insn in
		     front of the jump, and TEMP, with the sequence we have
		     made.  */

		  if (target != temp2)
		    emit_move_insn (temp2, target);

		  seq = get_insns ();
		  end_sequence ();

		  emit_insns_before (seq, temp4);
		  delete_insn (temp);

		  if (init_insn)
		    delete_insn (init_insn);

		  next = NEXT_INSN (insn);
#ifdef HAVE_cc0
		  delete_insn (prev_nonnote_insn (insn));
#endif
		  delete_insn (insn);

		  if (after_regscan)
		    {
		      reg_scan_update (seq, NEXT_INSN (next), old_max_reg);
		      old_max_reg = max_reg_num ();
		    }

		  changed = 1;
		  continue;
		}
	      else
		end_sequence ();
	    }

	  /* Try to use a conditional move (if the target has them), or a
	     store-flag insn.  If the target has conditional arithmetic as
	     well as conditional move, the above code will have done something.
	     Note that we prefer the above code since it is more general: the
	     code below can make changes that require work to undo.

	     The general case here is:

	     1) x = a; if (...) x = b; and
	     2) if (...) x = b;

	     If the jump would be faster, the machine should not have defined
	     the movcc or scc insns!.  These cases are often made by the
	     previous optimization.

	     The second case is treated as  x = x; if (...) x = b;.

	     INSN here is the jump around the store.  We set:

	     TEMP to the "x op= b;" insn.
	     TEMP1 to X.
	     TEMP2 to B.
	     TEMP3 to A (X in the second case).
	     TEMP4 to the condition being tested.
	     TEMP5 to the earliest insn used to find the condition.
	     TEMP6 to the SET of TEMP.  */

	  if (/* We can't do this after reload has completed.  */
	      ! reload_completed
#ifdef HAVE_conditional_arithmetic
	      /* Defer this until after CSE so the above code gets the
		 first crack at it.  */
	      && cse_not_expected
#endif
	      && this_is_condjump && ! this_is_simplejump
	      /* Set TEMP to the "x = b;" insn.  */
	      && (temp = next_nonnote_insn (insn)) != 0
	      && GET_CODE (temp) == INSN
	      && (temp6 = single_set (temp)) != NULL_RTX
	      && GET_CODE (temp1 = SET_DEST (temp6)) == REG
	      && (! SMALL_REGISTER_CLASSES
		  || REGNO (temp1) >= FIRST_PSEUDO_REGISTER)
	      && ! side_effects_p (temp2 = SET_SRC (temp6))
	      && ! may_trap_p (temp2)
	      /* Allow either form, but prefer the former if both apply. 
		 There is no point in using the old value of TEMP1 if
		 it is a register, since cse will alias them.  It can
		 lose if the old value were a hard register since CSE
		 won't replace hard registers.  Avoid using TEMP3 if
		 small register classes and it is a hard register.  */
	      && (((temp3 = reg_set_last (temp1, insn)) != 0
		   && ! (SMALL_REGISTER_CLASSES && GET_CODE (temp3) == REG
			 && REGNO (temp3) < FIRST_PSEUDO_REGISTER))
		  /* Make the latter case look like  x = x; if (...) x = b;  */
		  || (temp3 = temp1, 1))
	      /* INSN must either branch to the insn after TEMP or the insn
		 after TEMP must branch to the same place as INSN.  */
	      && (reallabelprev == temp
		  || ((temp4 = next_active_insn (temp)) != 0
		      && simplejump_p (temp4)
		      && JUMP_LABEL (temp4) == JUMP_LABEL (insn)))
	      && (temp4 = get_condition (insn, &temp5)) != 0
	      /* We must be comparing objects whose modes imply the size.
		 We could handle BLKmode if (1) emit_store_flag could
		 and (2) we could find the size reliably.  */
	      && GET_MODE (XEXP (temp4, 0)) != BLKmode
	      /* Even if branches are cheap, the store_flag optimization
		 can win when the operation to be performed can be
		 expressed directly.  */
#ifdef HAVE_cc0
	      /* If the previous insn sets CC0 and something else, we can't
		 do this since we are going to delete that insn.  */

	      && ! ((temp6 = prev_nonnote_insn (insn)) != 0
		    && GET_CODE (temp6) == INSN
		    && (sets_cc0_p (PATTERN (temp6)) == -1
			|| (sets_cc0_p (PATTERN (temp6)) == 1
			    && FIND_REG_INC_NOTE (temp6, NULL_RTX))))
#endif
	      )
	    {
#ifdef HAVE_conditional_move
	      /* First try a conditional move.  */
	      {
		enum rtx_code code = GET_CODE (temp4);
		rtx var = temp1;
		rtx cond0, cond1, aval, bval;
		rtx target, new_insn;

		/* Copy the compared variables into cond0 and cond1, so that
		   any side effects performed in or after the old comparison,
		   will not affect our compare which will come later.  */
		/* ??? Is it possible to just use the comparison in the jump
		   insn?  After all, we're going to delete it.  We'd have
		   to modify emit_conditional_move to take a comparison rtx
		   instead or write a new function.  */
		cond0 = gen_reg_rtx (GET_MODE (XEXP (temp4, 0)));
		/* We want the target to be able to simplify comparisons with
		   zero (and maybe other constants as well), so don't create
		   pseudos for them.  There's no need to either.  */
		if (GET_CODE (XEXP (temp4, 1)) == CONST_INT
		    || GET_CODE (XEXP (temp4, 1)) == CONST_DOUBLE)
		  cond1 = XEXP (temp4, 1);
		else
		  cond1 = gen_reg_rtx (GET_MODE (XEXP (temp4, 1)));

		/* Careful about copying these values -- an IOR or what may
		   need to do other things, like clobber flags.  */
		/* ??? Assume for the moment that AVAL is ok.  */
		aval = temp3;

		start_sequence ();

		/* We're dealing with a single_set insn with no side effects
		   on SET_SRC.  We do need to be reasonably certain that if
		   we need to force BVAL into a register that we won't 
		   clobber the flags -- general_operand should suffice.  */
		if (general_operand (temp2, GET_MODE (var)))
		  bval = temp2;
		else
		  {
		    bval = gen_reg_rtx (GET_MODE (var));
		    new_insn = copy_rtx (temp);
		    temp6 = single_set (new_insn);
		    SET_DEST (temp6) = bval;
		    emit_insn (PATTERN (new_insn));
		  }

		target = emit_conditional_move (var, code,
						cond0, cond1, VOIDmode,
						aval, bval, GET_MODE (var),
						(code == LTU || code == GEU
						 || code == LEU || code == GTU));

		if (target)
		  {
		    rtx seq1, seq2, last;
		    int copy_ok;

		    /* Save the conditional move sequence but don't emit it
		       yet.  On some machines, like the alpha, it is possible
		       that temp5 == insn, so next generate the sequence that
		       saves the compared values and then emit both
		       sequences ensuring seq1 occurs before seq2.  */
		    seq2 = get_insns ();
		    end_sequence ();

		    /* "Now that we can't fail..."  Famous last words.
		       Generate the copy insns that preserve the compared
		       values.  */
		    start_sequence ();
		    emit_move_insn (cond0, XEXP (temp4, 0));
		    if (cond1 != XEXP (temp4, 1))
		      emit_move_insn (cond1, XEXP (temp4, 1));
		    seq1 = get_insns ();
		    end_sequence ();

		    /* Validate the sequence -- this may be some weird
		       bit-extract-and-test instruction for which there
		       exists no complimentary bit-extract insn.  */
		    copy_ok = 1;
		    for (last = seq1; last ; last = NEXT_INSN (last))
		      if (recog_memoized (last) < 0)
			{
			  copy_ok = 0;
			  break;
			}

		    if (copy_ok)
		      {
		        emit_insns_before (seq1, temp5);

		        /* Insert conditional move after insn, to be sure
			   that the jump and a possible compare won't be
			   separated.  */
		        last = emit_insns_after (seq2, insn);

		        /* ??? We can also delete the insn that sets X to A.
		           Flow will do it too though.  */
		        delete_insn (temp);
		        next = NEXT_INSN (insn);
		        delete_jump (insn);

		        if (after_regscan)
		          {
			    reg_scan_update (seq1, NEXT_INSN (last),
					     old_max_reg);
			    old_max_reg = max_reg_num ();
		          }

		        changed = 1;
		        continue;
		      }
		  }
		else
		  end_sequence ();
	      }
#endif

	      /* That didn't work, try a store-flag insn.

		 We further divide the cases into:

		 1) x = a; if (...) x = b; and either A or B is zero,
		 2) if (...) x = 0; and jumps are expensive,
		 3) x = a; if (...) x = b; and A and B are constants where all
		 the set bits in A are also set in B and jumps are expensive,
		 4) x = a; if (...) x = b; and A and B non-zero, and jumps are
		 more expensive, and
		 5) if (...) x = b; if jumps are even more expensive.  */

	      if (GET_MODE_CLASS (GET_MODE (temp1)) == MODE_INT
		  /* We will be passing this as operand into expand_and.  No
		     good if it's not valid as an operand.  */
		  && general_operand (temp2, GET_MODE (temp2))
		  && ((GET_CODE (temp3) == CONST_INT)
		      /* Make the latter case look like
			 x = x; if (...) x = 0;  */
		      || (temp3 = temp1,
			  ((BRANCH_COST >= 2
			    && temp2 == const0_rtx)
			   || BRANCH_COST >= 3)))
		  /* If B is zero, OK; if A is zero, can only do (1) if we
		     can reverse the condition.  See if (3) applies possibly
		     by reversing the condition.  Prefer reversing to (4) when
		     branches are very expensive.  */
		  && (((BRANCH_COST >= 2
			|| STORE_FLAG_VALUE == -1
			|| (STORE_FLAG_VALUE == 1
			 /* Check that the mask is a power of two,
			    so that it can probably be generated
			    with a shift.  */
			    && GET_CODE (temp3) == CONST_INT
			    && exact_log2 (INTVAL (temp3)) >= 0))
		       && (reversep = 0, temp2 == const0_rtx))
		      || ((BRANCH_COST >= 2
			   || STORE_FLAG_VALUE == -1
			   || (STORE_FLAG_VALUE == 1
			       && GET_CODE (temp2) == CONST_INT
			       && exact_log2 (INTVAL (temp2)) >= 0))
			  && temp3 == const0_rtx
			  && (reversep = can_reverse_comparison_p (temp4, insn)))
		      || (BRANCH_COST >= 2
			  && GET_CODE (temp2) == CONST_INT
			  && GET_CODE (temp3) == CONST_INT
			  && ((INTVAL (temp2) & INTVAL (temp3)) == INTVAL (temp2)
			      || ((INTVAL (temp2) & INTVAL (temp3)) == INTVAL (temp3)
				  && (reversep = can_reverse_comparison_p (temp4,
									   insn)))))
		      || BRANCH_COST >= 3)
		  )
		{
		  enum rtx_code code = GET_CODE (temp4);
		  rtx uval, cval, var = temp1;
		  int normalizep;
		  rtx target;

		  /* If necessary, reverse the condition.  */
		  if (reversep)
		    code = reverse_condition (code), uval = temp2, cval = temp3;
		  else
		    uval = temp3, cval = temp2;

		  /* If CVAL is non-zero, normalize to -1.  Otherwise, if UVAL
		     is the constant 1, it is best to just compute the result
		     directly.  If UVAL is constant and STORE_FLAG_VALUE
		     includes all of its bits, it is best to compute the flag
		     value unnormalized and `and' it with UVAL.  Otherwise,
		     normalize to -1 and `and' with UVAL.  */
		  normalizep = (cval != const0_rtx ? -1
				: (uval == const1_rtx ? 1
				   : (GET_CODE (uval) == CONST_INT
				      && (INTVAL (uval) & ~STORE_FLAG_VALUE) == 0)
				   ? 0 : -1));

		  /* We will be putting the store-flag insn immediately in
		     front of the comparison that was originally being done,
		     so we know all the variables in TEMP4 will be valid.
		     However, this might be in front of the assignment of
		     A to VAR.  If it is, it would clobber the store-flag
		     we will be emitting.

		     Therefore, emit into a temporary which will be copied to
		     VAR immediately after TEMP.  */

		  start_sequence ();
		  target = emit_store_flag (gen_reg_rtx (GET_MODE (var)), code,
					    XEXP (temp4, 0), XEXP (temp4, 1),
					    VOIDmode,
					    (code == LTU || code == LEU 
					     || code == GEU || code == GTU),
					    normalizep);
		  if (target)
		    {
		      rtx seq;
		      rtx before = insn;

		      seq = get_insns ();
		      end_sequence ();

		      /* Put the store-flag insns in front of the first insn
			 used to compute the condition to ensure that we
			 use the same values of them as the current 
			 comparison.  However, the remainder of the insns we
			 generate will be placed directly in front of the
			 jump insn, in case any of the pseudos we use
			 are modified earlier.  */

		      emit_insns_before (seq, temp5);

		      start_sequence ();

		      /* Both CVAL and UVAL are non-zero.  */
		      if (cval != const0_rtx && uval != const0_rtx)
			{
			  rtx tem1, tem2;

			  tem1 = expand_and (uval, target, NULL_RTX);
			  if (GET_CODE (cval) == CONST_INT
			      && GET_CODE (uval) == CONST_INT
			      && (INTVAL (cval) & INTVAL (uval)) == INTVAL (cval))
			    tem2 = cval;
			  else
			    {
			      tem2 = expand_unop (GET_MODE (var), one_cmpl_optab,
						  target, NULL_RTX, 0);
			      tem2 = expand_and (cval, tem2,
						 (GET_CODE (tem2) == REG
						  ? tem2 : 0));
			    }

			  /* If we usually make new pseudos, do so here.  This
			     turns out to help machines that have conditional
			     move insns.  */
			  /* ??? Conditional moves have already been handled.
			     This may be obsolete.  */

			  if (flag_expensive_optimizations)
			    target = 0;

			  target = expand_binop (GET_MODE (var), ior_optab,
						 tem1, tem2, target,
						 1, OPTAB_WIDEN);
			}
		      else if (normalizep != 1)
			{
			  /* We know that either CVAL or UVAL is zero.  If
			     UVAL is zero, negate TARGET and `and' with CVAL.
			     Otherwise, `and' with UVAL.  */
			  if (uval == const0_rtx)
			    {
			      target = expand_unop (GET_MODE (var), one_cmpl_optab,
						    target, NULL_RTX, 0);
			      uval = cval;
			    }

			  target = expand_and (uval, target,
					       (GET_CODE (target) == REG
						&& ! preserve_subexpressions_p ()
						? target : NULL_RTX));
			}
		  
		      emit_move_insn (var, target);
		      seq = get_insns ();
		      end_sequence ();
#ifdef HAVE_cc0
		      /* If INSN uses CC0, we must not separate it from the
			 insn that sets cc0.  */
		      if (reg_mentioned_p (cc0_rtx, PATTERN (before)))
			before = prev_nonnote_insn (before);
#endif
		      emit_insns_before (seq, before);

		      delete_insn (temp);
		      next = NEXT_INSN (insn);
		      delete_jump (insn);

		      if (after_regscan)
			{
			  reg_scan_update (seq, NEXT_INSN (next), old_max_reg);
			  old_max_reg = max_reg_num ();
			}

		      changed = 1;
		      continue;
		    }
		  else
		    end_sequence ();
		}
	    }


	  /* Simplify   if (...) x = 1; else {...}  if (x) ...
	     We recognize this case scanning backwards as well.

	     TEMP is the assignment to x;
	     TEMP1 is the label at the head of the second if.  */
	  /* ?? This should call get_condition to find the values being
	     compared, instead of looking for a COMPARE insn when HAVE_cc0
	     is not defined.  This would allow it to work on the m88k.  */
	  /* ?? This optimization is only safe before cse is run if HAVE_cc0
	     is not defined and the condition is tested by a separate compare
	     insn.  This is because the code below assumes that the result
	     of the compare dies in the following branch.

	     Not only that, but there might be other insns between the
	     compare and branch whose results are live.  Those insns need
	     to be executed.

	     A way to fix this is to move the insns at JUMP_LABEL (insn)
	     to before INSN.  If we are running before flow, they will
	     be deleted if they aren't needed.   But this doesn't work
	     well after flow.

	     This is really a special-case of jump threading, anyway.  The
	     right thing to do is to replace this and jump threading with
	     much simpler code in cse.

	     This code has been turned off in the non-cc0 case in the
	     meantime.  */

#ifdef HAVE_cc0
	  else if (this_is_simplejump
		   /* Safe to skip USE and CLOBBER insns here
		      since they will not be deleted.  */
		   && (temp = prev_active_insn (insn))
		   && no_labels_between_p (temp, insn)
		   && GET_CODE (temp) == INSN
		   && GET_CODE (PATTERN (temp)) == SET
		   && GET_CODE (SET_DEST (PATTERN (temp))) == REG
		   && CONSTANT_P (SET_SRC (PATTERN (temp)))
		   && (temp1 = next_active_insn (JUMP_LABEL (insn)))
		   /* If we find that the next value tested is `x'
		      (TEMP1 is the insn where this happens), win.  */
		   && GET_CODE (temp1) == INSN
		   && GET_CODE (PATTERN (temp1)) == SET
#ifdef HAVE_cc0
		   /* Does temp1 `tst' the value of x?  */
		   && SET_SRC (PATTERN (temp1)) == SET_DEST (PATTERN (temp))
		   && SET_DEST (PATTERN (temp1)) == cc0_rtx
		   && (temp1 = next_nonnote_insn (temp1))
#else
		   /* Does temp1 compare the value of x against zero?  */
		   && GET_CODE (SET_SRC (PATTERN (temp1))) == COMPARE
		   && XEXP (SET_SRC (PATTERN (temp1)), 1) == const0_rtx
		   && (XEXP (SET_SRC (PATTERN (temp1)), 0)
		       == SET_DEST (PATTERN (temp)))
		   && GET_CODE (SET_DEST (PATTERN (temp1))) == REG
		   && (temp1 = find_next_ref (SET_DEST (PATTERN (temp1)), temp1))
#endif
		   && condjump_p (temp1))
	    {
	      /* Get the if_then_else from the condjump.  */
	      rtx choice = SET_SRC (PATTERN (temp1));
	      if (GET_CODE (choice) == IF_THEN_ELSE)
		{
		  enum rtx_code code = GET_CODE (XEXP (choice, 0));
		  rtx val = SET_SRC (PATTERN (temp));
		  rtx cond
		    = simplify_relational_operation (code, GET_MODE (SET_DEST (PATTERN (temp))),
						     val, const0_rtx);
		  rtx ultimate;

		  if (cond == const_true_rtx)
		    ultimate = XEXP (choice, 1);
		  else if (cond == const0_rtx)
		    ultimate = XEXP (choice, 2);
		  else
		    ultimate = 0;

		  if (ultimate == pc_rtx)
		    ultimate = get_label_after (temp1);
		  else if (ultimate && GET_CODE (ultimate) != RETURN)
		    ultimate = XEXP (ultimate, 0);

		  if (ultimate && JUMP_LABEL(insn) != ultimate)
		    changed |= redirect_jump (insn, ultimate);
		}
	    }
#endif

#if 0
	  /* @@ This needs a bit of work before it will be right.

	     Any type of comparison can be accepted for the first and
	     second compare.  When rewriting the first jump, we must
	     compute the what conditions can reach label3, and use the
	     appropriate code.  We can not simply reverse/swap the code
	     of the first jump.  In some cases, the second jump must be
	     rewritten also.

	     For example, 
	     <  == converts to >  ==
	     <  != converts to ==  >
	     etc.

	     If the code is written to only accept an '==' test for the second
	     compare, then all that needs to be done is to swap the condition
	     of the first branch.

	     It is questionable whether we want this optimization anyways,
	     since if the user wrote code like this because he/she knew that
	     the jump to label1 is taken most of the time, then rewriting
	     this gives slower code.  */
	  /* @@ This should call get_condition to find the values being
	     compared, instead of looking for a COMPARE insn when HAVE_cc0
	     is not defined.  This would allow it to work on the m88k.  */
	  /* @@ This optimization is only safe before cse is run if HAVE_cc0
	     is not defined and the condition is tested by a separate compare
	     insn.  This is because the code below assumes that the result
	     of the compare dies in the following branch.  */

	  /* Simplify  test a ~= b
		       condjump label1;
		       test a == b
		       condjump label2;
		       jump label3;
		       label1:

	     rewriting as
		       test a ~~= b
		       condjump label3
		       test a == b
		       condjump label2
		       label1:

	     where ~= is an inequality, e.g. >, and ~~= is the swapped
	     inequality, e.g. <.

	     We recognize this case scanning backwards.

	     TEMP is the conditional jump to `label2';
	     TEMP1 is the test for `a == b';
	     TEMP2 is the conditional jump to `label1';
	     TEMP3 is the test for `a ~= b'.  */
	  else if (this_is_simplejump
		   && (temp = prev_active_insn (insn))
		   && no_labels_between_p (temp, insn)
		   && condjump_p (temp)
		   && (temp1 = prev_active_insn (temp))
		   && no_labels_between_p (temp1, temp)
		   && GET_CODE (temp1) == INSN
		   && GET_CODE (PATTERN (temp1)) == SET
#ifdef HAVE_cc0
		   && sets_cc0_p (PATTERN (temp1)) == 1
#else
		   && GET_CODE (SET_SRC (PATTERN (temp1))) == COMPARE
		   && GET_CODE (SET_DEST (PATTERN (temp1))) == REG
		   && (temp == find_next_ref (SET_DEST (PATTERN (temp1)), temp1))
#endif
		   && (temp2 = prev_active_insn (temp1))
		   && no_labels_between_p (temp2, temp1)
		   && condjump_p (temp2)
		   && JUMP_LABEL (temp2) == next_nonnote_insn (NEXT_INSN (insn))
		   && (temp3 = prev_active_insn (temp2))
		   && no_labels_between_p (temp3, temp2)
		   && GET_CODE (PATTERN (temp3)) == SET
		   && rtx_equal_p (SET_DEST (PATTERN (temp3)),
				   SET_DEST (PATTERN (temp1)))
		   && rtx_equal_p (SET_SRC (PATTERN (temp1)),
				   SET_SRC (PATTERN (temp3)))
		   && ! inequality_comparisons_p (PATTERN (temp))
		   && inequality_comparisons_p (PATTERN (temp2)))
	    {
	      rtx fallthrough_label = JUMP_LABEL (temp2);

	      ++LABEL_NUSES (fallthrough_label);
	      if (swap_jump (temp2, JUMP_LABEL (insn)))
		{
		  delete_insn (insn);
		  changed = 1;
		}

	      if (--LABEL_NUSES (fallthrough_label) == 0)
		delete_insn (fallthrough_label);
	    }
#endif
	  /* Simplify  if (...) {... x = 1;} if (x) ...

	     We recognize this case backwards.

	     TEMP is the test of `x';
	     TEMP1 is the assignment to `x' at the end of the
	     previous statement.  */
	  /* @@ This should call get_condition to find the values being
	     compared, instead of looking for a COMPARE insn when HAVE_cc0
	     is not defined.  This would allow it to work on the m88k.  */
	  /* @@ This optimization is only safe before cse is run if HAVE_cc0
	     is not defined and the condition is tested by a separate compare
	     insn.  This is because the code below assumes that the result
	     of the compare dies in the following branch.  */

	  /* ??? This has to be turned off.  The problem is that the
	     unconditional jump might indirectly end up branching to the
	     label between TEMP1 and TEMP.  We can't detect this, in general,
	     since it may become a jump to there after further optimizations.
	     If that jump is done, it will be deleted, so we will retry
	     this optimization in the next pass, thus an infinite loop.

	     The present code prevents this by putting the jump after the
	     label, but this is not logically correct.  */
#if 0
	  else if (this_is_condjump
		   /* Safe to skip USE and CLOBBER insns here
		      since they will not be deleted.  */
		   && (temp = prev_active_insn (insn))
		   && no_labels_between_p (temp, insn)
		   && GET_CODE (temp) == INSN
		   && GET_CODE (PATTERN (temp)) == SET
#ifdef HAVE_cc0
		   && sets_cc0_p (PATTERN (temp)) == 1
		   && GET_CODE (SET_SRC (PATTERN (temp))) == REG
#else
		   /* Temp must be a compare insn, we can not accept a register
		      to register move here, since it may not be simply a
		      tst insn.  */
		   && GET_CODE (SET_SRC (PATTERN (temp))) == COMPARE
		   && XEXP (SET_SRC (PATTERN (temp)), 1) == const0_rtx
		   && GET_CODE (XEXP (SET_SRC (PATTERN (temp)), 0)) == REG
		   && GET_CODE (SET_DEST (PATTERN (temp))) == REG
		   && insn == find_next_ref (SET_DEST (PATTERN (temp)), temp)
#endif
		   /* May skip USE or CLOBBER insns here
		      for checking for opportunity, since we
		      take care of them later.  */
		   && (temp1 = prev_active_insn (temp))
		   && GET_CODE (temp1) == INSN
		   && GET_CODE (PATTERN (temp1)) == SET
#ifdef HAVE_cc0
		   && SET_SRC (PATTERN (temp)) == SET_DEST (PATTERN (temp1))
#else
		   && (XEXP (SET_SRC (PATTERN (temp)), 0)
		       == SET_DEST (PATTERN (temp1)))
#endif
		   && CONSTANT_P (SET_SRC (PATTERN (temp1)))
		   /* If this isn't true, cse will do the job.  */
		   && ! no_labels_between_p (temp1, temp))
	    {
	      /* Get the if_then_else from the condjump.  */
	      rtx choice = SET_SRC (PATTERN (insn));
	      if (GET_CODE (choice) == IF_THEN_ELSE
		  && (GET_CODE (XEXP (choice, 0)) == EQ
		      || GET_CODE (XEXP (choice, 0)) == NE))
		{
		  int want_nonzero = (GET_CODE (XEXP (choice, 0)) == NE);
		  rtx last_insn;
		  rtx ultimate;
		  rtx p;

		  /* Get the place that condjump will jump to
		     if it is reached from here.  */
		  if ((SET_SRC (PATTERN (temp1)) != const0_rtx)
		      == want_nonzero)
		    ultimate = XEXP (choice, 1);
		  else
		    ultimate = XEXP (choice, 2);
		  /* Get it as a CODE_LABEL.  */
		  if (ultimate == pc_rtx)
		    ultimate = get_label_after (insn);
		  else
		    /* Get the label out of the LABEL_REF.  */
		    ultimate = XEXP (ultimate, 0);

		  /* Insert the jump immediately before TEMP, specifically
		     after the label that is between TEMP1 and TEMP.  */
		  last_insn = PREV_INSN (temp);

		  /* If we would be branching to the next insn, the jump
		     would immediately be deleted and the re-inserted in
		     a subsequent pass over the code.  So don't do anything
		     in that case.  */
		  if (next_active_insn (last_insn)
		      != next_active_insn (ultimate))
		    {
		      emit_barrier_after (last_insn);
		      p = emit_jump_insn_after (gen_jump (ultimate),
						last_insn);
		      JUMP_LABEL (p) = ultimate;
		      ++LABEL_NUSES (ultimate);
		      if (INSN_UID (ultimate) < max_jump_chain
			  && INSN_CODE (p) < max_jump_chain)
			{
			  jump_chain[INSN_UID (p)]
			    = jump_chain[INSN_UID (ultimate)];
			  jump_chain[INSN_UID (ultimate)] = p;
			}
		      changed = 1;
		      continue;
		    }
		}
	    }
#endif
#ifdef HAVE_trap
	  /* Detect a conditional jump jumping over an unconditional trap.  */
	  else if (HAVE_trap
		   && this_is_condjump && ! this_is_simplejump
		   && reallabelprev != 0
		   && GET_CODE (reallabelprev) == INSN
		   && GET_CODE (PATTERN (reallabelprev)) == TRAP_IF
		   && TRAP_CONDITION (PATTERN (reallabelprev)) == const_true_rtx
		   && prev_active_insn (reallabelprev) == insn
		   && no_labels_between_p (insn, reallabelprev)
		   && (temp2 = get_condition (insn, &temp4))
		   && can_reverse_comparison_p (temp2, insn))
	    {
	      rtx new = gen_cond_trap (reverse_condition (GET_CODE (temp2)),
				       XEXP (temp2, 0), XEXP (temp2, 1),
				       TRAP_CODE (PATTERN (reallabelprev)));

	      if (new)
		{
		  emit_insn_before (new, temp4);
		  delete_insn (reallabelprev);
		  delete_jump (insn);
		  changed = 1;
		  continue;
		}
	    }
	  /* Detect a jump jumping to an unconditional trap.  */
	  else if (HAVE_trap && this_is_condjump
		   && (temp = next_active_insn (JUMP_LABEL (insn)))
		   && GET_CODE (temp) == INSN
		   && GET_CODE (PATTERN (temp)) == TRAP_IF
		   && (this_is_simplejump
		       || (temp2 = get_condition (insn, &temp4))))
	    {
	      rtx tc = TRAP_CONDITION (PATTERN (temp));

	      if (tc == const_true_rtx
		  || (! this_is_simplejump && rtx_equal_p (temp2, tc)))
		{
		  rtx new;
		  /* Replace an unconditional jump to a trap with a trap.  */
		  if (this_is_simplejump)
		    {
		      emit_barrier_after (emit_insn_before (gen_trap (), insn));
		      delete_jump (insn);
		      changed = 1;
		      continue;
		    }
		  new = gen_cond_trap (GET_CODE (temp2), XEXP (temp2, 0),
				       XEXP (temp2, 1),
				       TRAP_CODE (PATTERN (temp)));
		  if (new)
		    {
		      emit_insn_before (new, temp4);
		      delete_jump (insn);
		      changed = 1;
		      continue;
		    }
		}
	      /* If the trap condition and jump condition are mutually
		 exclusive, redirect the jump to the following insn.  */
	      else if (GET_RTX_CLASS (GET_CODE (tc)) == '<'
		       && ! this_is_simplejump
		       && swap_condition (GET_CODE (temp2)) == GET_CODE (tc)
		       && rtx_equal_p (XEXP (tc, 0), XEXP (temp2, 0))
		       && rtx_equal_p (XEXP (tc, 1), XEXP (temp2, 1))
		       && redirect_jump (insn, get_label_after (temp)))
		{
		  changed = 1;
		  continue;
		}
	    }
#endif
	  else
	    {
	      /* Detect a jump to a jump.  */

	      /* Look for   if (foo) bar; else break;  */
	      /* The insns look like this:
		 insn = condjump label1;
		 ...range1 (some insns)...
		 jump label2;
		 label1:
		 ...range2 (some insns)...
		 jump somewhere unconditionally
		 label2:  */
	      {
		rtx label1 = next_label (insn);
		rtx range1end = label1 ? prev_active_insn (label1) : 0;
		/* Don't do this optimization on the first round, so that
		   jump-around-a-jump gets simplified before we ask here
		   whether a jump is unconditional.

		   Also don't do it when we are called after reload since
		   it will confuse reorg.  */
		if (! first
		    && (reload_completed ? ! flag_delayed_branch : 1)
		    /* Make sure INSN is something we can invert.  */
		    && condjump_p (insn)
		    && label1 != 0
		    && JUMP_LABEL (insn) == label1
		    && LABEL_NUSES (label1) == 1
		    && GET_CODE (range1end) == JUMP_INSN
		    && simplejump_p (range1end))
		  {
		    rtx label2 = next_label (label1);
		    rtx range2end = label2 ? prev_active_insn (label2) : 0;
		    if (range1end != range2end
			&& JUMP_LABEL (range1end) == label2
			&& GET_CODE (range2end) == JUMP_INSN
			&& GET_CODE (NEXT_INSN (range2end)) == BARRIER
			/* Invert the jump condition, so we
			   still execute the same insns in each case.  */
			&& invert_jump (insn, label1))
		      {
			rtx range1beg = next_active_insn (insn);
			rtx range2beg = next_active_insn (label1);
			rtx range1after, range2after;
			rtx range1before, range2before;
			rtx rangenext;

			/* Include in each range any notes before it, to be
			   sure that we get the line number note if any, even
			   if there are other notes here.  */
			while (PREV_INSN (range1beg)
			       && GET_CODE (PREV_INSN (range1beg)) == NOTE)
			  range1beg = PREV_INSN (range1beg);

			while (PREV_INSN (range2beg)
			       && GET_CODE (PREV_INSN (range2beg)) == NOTE)
			  range2beg = PREV_INSN (range2beg);

			/* Don't move NOTEs for blocks or loops; shift them
			   outside the ranges, where they'll stay put.  */
			range1beg = squeeze_notes (range1beg, range1end);
			range2beg = squeeze_notes (range2beg, range2end);

			/* Get current surrounds of the 2 ranges.  */
			range1before = PREV_INSN (range1beg);
			range2before = PREV_INSN (range2beg);
			range1after = NEXT_INSN (range1end);
			range2after = NEXT_INSN (range2end);

			/* Splice range2 where range1 was.  */
			NEXT_INSN (range1before) = range2beg;
			PREV_INSN (range2beg) = range1before;
			NEXT_INSN (range2end) = range1after;
			PREV_INSN (range1after) = range2end;
			/* Splice range1 where range2 was.  */
			NEXT_INSN (range2before) = range1beg;
			PREV_INSN (range1beg) = range2before;
			NEXT_INSN (range1end) = range2after;
			PREV_INSN (range2after) = range1end;

			/* Check for loop notes between the end of
			   range2, and the next code label.  If there is one,
			   then what we have really seen is
			   if (foo) break; end_of_loop;
			   and moved the break sequence outside the loop.
			   We must move LOOP_END, LOOP_VTOP and LOOP_CONT
			   notes (in order) to where the loop really ends now,
			   or we will confuse loop optimization.  Stop if we
			   find a LOOP_BEG note first, since we don't want to
			   move the notes in that case.  */
			for (;range2after != label2; range2after = rangenext)
			  {
			    rangenext = NEXT_INSN (range2after);
			    if (GET_CODE (range2after) == NOTE)
			      {
				int kind = NOTE_LINE_NUMBER (range2after);
				if (kind == NOTE_INSN_LOOP_END
				    || kind == NOTE_INSN_LOOP_VTOP
				    || kind == NOTE_INSN_LOOP_CONT)
				  {
				    NEXT_INSN (PREV_INSN (range2after))
				      = rangenext;
				    PREV_INSN (rangenext)
				      = PREV_INSN (range2after);
				    PREV_INSN (range2after) 
				      = PREV_INSN (range1beg);
				    NEXT_INSN (range2after) = range1beg;
				    NEXT_INSN (PREV_INSN (range1beg))
				      = range2after;
				    PREV_INSN (range1beg) = range2after;
				  }
				else if (NOTE_LINE_NUMBER (range2after)
					 == NOTE_INSN_LOOP_BEG)
				  break;
			      }
			  }
			changed = 1;
			continue;
		      }
		  }
	      }

	      /* Now that the jump has been tensioned,
		 try cross jumping: check for identical code
		 before the jump and before its target label.  */

	      /* First, cross jumping of conditional jumps:  */

	      if (cross_jump && condjump_p (insn))
		{
		  rtx newjpos, newlpos;
		  rtx x = prev_real_insn (JUMP_LABEL (insn));

		  /* A conditional jump may be crossjumped
		     only if the place it jumps to follows
		     an opposing jump that comes back here.  */

		  if (x != 0 && ! jump_back_p (x, insn))
		    /* We have no opposing jump;
		       cannot cross jump this insn.  */
		    x = 0;

		  newjpos = 0;
		  /* TARGET is nonzero if it is ok to cross jump
		     to code before TARGET.  If so, see if matches.  */
		  if (x != 0)
		    find_cross_jump (insn, x, 2,
				     &newjpos, &newlpos);

		  if (newjpos != 0)
		    {
		      do_cross_jump (insn, newjpos, newlpos);
		      /* Make the old conditional jump
			 into an unconditional one.  */
		      SET_SRC (PATTERN (insn))
			= gen_rtx_LABEL_REF (VOIDmode, JUMP_LABEL (insn));
		      INSN_CODE (insn) = -1;
		      emit_barrier_after (insn);
		      /* Add to jump_chain unless this is a new label
			 whose UID is too large.  */
		      if (INSN_UID (JUMP_LABEL (insn)) < max_jump_chain)
			{
			  jump_chain[INSN_UID (insn)]
			    = jump_chain[INSN_UID (JUMP_LABEL (insn))];
			  jump_chain[INSN_UID (JUMP_LABEL (insn))] = insn;
			}
		      changed = 1;
		      next = insn;
		    }
		}

	      /* Cross jumping of unconditional jumps:
		 a few differences.  */

	      if (cross_jump && simplejump_p (insn))
		{
		  rtx newjpos, newlpos;
		  rtx target;

		  newjpos = 0;

		  /* TARGET is nonzero if it is ok to cross jump
		     to code before TARGET.  If so, see if matches.  */
		  find_cross_jump (insn, JUMP_LABEL (insn), 1,
				   &newjpos, &newlpos);

		  /* If cannot cross jump to code before the label,
		     see if we can cross jump to another jump to
		     the same label.  */
		  /* Try each other jump to this label.  */
		  if (INSN_UID (JUMP_LABEL (insn)) < max_uid)
		    for (target = jump_chain[INSN_UID (JUMP_LABEL (insn))];
			 target != 0 && newjpos == 0;
			 target = jump_chain[INSN_UID (target)])
		      if (target != insn
			  && JUMP_LABEL (target) == JUMP_LABEL (insn)
			  /* Ignore TARGET if it's deleted.  */
			  && ! INSN_DELETED_P (target))
			find_cross_jump (insn, target, 2,
					 &newjpos, &newlpos);

		  if (newjpos != 0)
		    {
		      do_cross_jump (insn, newjpos, newlpos);
		      changed = 1;
		      next = insn;
		    }
		}

	      /* This code was dead in the previous jump.c!  */
	      if (cross_jump && GET_CODE (PATTERN (insn)) == RETURN)
		{
		  /* Return insns all "jump to the same place"
		     so we can cross-jump between any two of them.  */

		  rtx newjpos, newlpos, target;

		  newjpos = 0;

		  /* If cannot cross jump to code before the label,
		     see if we can cross jump to another jump to
		     the same label.  */
		  /* Try each other jump to this label.  */
		  for (target = jump_chain[0];
		       target != 0 && newjpos == 0;
		       target = jump_chain[INSN_UID (target)])
		    if (target != insn
			&& ! INSN_DELETED_P (target)
			&& GET_CODE (PATTERN (target)) == RETURN)
		      find_cross_jump (insn, target, 2,
				       &newjpos, &newlpos);

		  if (newjpos != 0)
		    {
		      do_cross_jump (insn, newjpos, newlpos);
		      changed = 1;
		      next = insn;
		    }
		}
	    }
	}

      first = 0;
    }

  /* Delete extraneous line number notes.
     Note that two consecutive notes for different lines are not really
     extraneous.  There should be some indication where that line belonged,
     even if it became empty.  */

  {
    rtx last_note = 0;

    for (insn = f; insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) >= 0)
	{
	  /* Delete this note if it is identical to previous note.  */
	  if (last_note
	      && NOTE_SOURCE_FILE (insn) == NOTE_SOURCE_FILE (last_note)
	      && NOTE_LINE_NUMBER (insn) == NOTE_LINE_NUMBER (last_note))
	    {
	      delete_insn (insn);
	      continue;
	    }

	  last_note = insn;
	}
  }

#ifdef HAVE_return
  if (HAVE_return)
    {
      /* If we fall through to the epilogue, see if we can insert a RETURN insn
	 in front of it.  If the machine allows it at this point (we might be
	 after reload for a leaf routine), it will improve optimization for it
	 to be there.  We do this both here and at the start of this pass since
	 the RETURN might have been deleted by some of our optimizations.  */
      insn = get_last_insn ();
      while (insn && GET_CODE (insn) == NOTE)
	insn = PREV_INSN (insn);

      if (insn && GET_CODE (insn) != BARRIER)
	{
	  emit_jump_insn (gen_return ());
	  emit_barrier ();
	}
    }
#endif

  /* CAN_REACH_END is persistent for each function.  Once set it should
     not be cleared.  This is especially true for the case where we
     delete the NOTE_FUNCTION_END note.  CAN_REACH_END is cleared by
     the front-end before compiling each function.  */
  if (calculate_can_reach_end (last_insn, 0, 1))
    can_reach_end = 1;

end:
  /* Clean up.  */
  free (jump_chain);
  jump_chain = 0;
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

/* Delete insns following barriers, up to next label. 

   Also delete no-op jumps created by gcse.  */
static void
delete_barrier_successors (f)
     rtx f;
{
  rtx insn;

  for (insn = f; insn;)
    {
      if (GET_CODE (insn) == BARRIER)
	{
	  insn = NEXT_INSN (insn);

	  never_reached_warning (insn);

	  while (insn != 0 && GET_CODE (insn) != CODE_LABEL)
	    {
	      if (GET_CODE (insn) == NOTE
		  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_FUNCTION_END)
		insn = NEXT_INSN (insn);
	      else
		insn = delete_insn (insn);
	    }
	  /* INSN is now the code_label.  */
	}
      /* Also remove (set (pc) (pc)) insns which can be created by
	 gcse.  We eliminate such insns now to avoid having them
	 cause problems later.  */
      else if (GET_CODE (insn) == JUMP_INSN
	       && GET_CODE (PATTERN (insn)) == SET
	       && SET_SRC (PATTERN (insn)) == pc_rtx
	       && SET_DEST (PATTERN (insn)) == pc_rtx)
	insn = delete_insn (insn);

      else
	insn = NEXT_INSN (insn);
    }
}

/* Mark the label each jump jumps to.
   Combine consecutive labels, and count uses of labels.

   For each label, make a chain (using `jump_chain')
   of all the *unconditional* jumps that jump to it;
   also make a chain of all returns.

   CROSS_JUMP indicates whether we are doing cross jumping
   and if we are whether we will be paying attention to
   death notes or not.  */

static void
mark_all_labels (f, cross_jump)
     rtx f;
     int cross_jump;
{
  rtx insn;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      {
	mark_jump_label (PATTERN (insn), insn, cross_jump);
	if (! INSN_DELETED_P (insn) && GET_CODE (insn) == JUMP_INSN)
	  {
	    if (JUMP_LABEL (insn) != 0 && simplejump_p (insn))
	      {
		jump_chain[INSN_UID (insn)]
		  = jump_chain[INSN_UID (JUMP_LABEL (insn))];
		jump_chain[INSN_UID (JUMP_LABEL (insn))] = insn;
	      }
	    if (GET_CODE (PATTERN (insn)) == RETURN)
	      {
		jump_chain[INSN_UID (insn)] = jump_chain[0];
		jump_chain[0] = insn;
	      }
	  }
      }
}

/* Delete all labels already not referenced.
   Also find and return the last insn.  */

static rtx
delete_unreferenced_labels (f)
     rtx f;
{
  rtx final = NULL_RTX;
  rtx insn;

  for (insn = f; insn; )
    {
      if (GET_CODE (insn) == CODE_LABEL
          && LABEL_NUSES (insn) == 0
          && LABEL_ALTERNATE_NAME (insn) == NULL)
	insn = delete_insn (insn);
      else
	{
	  final = insn;
	  insn = NEXT_INSN (insn);
	}
    }

  return final;
}

/* Delete various simple forms of moves which have no necessary
   side effect.  */

static void
delete_noop_moves (f)
     rtx f;
{
  rtx insn, next;

  for (insn = f; insn; )
    {
      next = NEXT_INSN (insn);

      if (GET_CODE (insn) == INSN)
	{
	  register rtx body = PATTERN (insn);

/* Combine stack_adjusts with following push_insns.  */
#ifdef PUSH_ROUNDING
	  if (GET_CODE (body) == SET
	      && SET_DEST (body) == stack_pointer_rtx
	      && GET_CODE (SET_SRC (body)) == PLUS
	      && XEXP (SET_SRC (body), 0) == stack_pointer_rtx
	      && GET_CODE (XEXP (SET_SRC (body), 1)) == CONST_INT
	      && INTVAL (XEXP (SET_SRC (body), 1)) > 0)
	    {
	      rtx p;
	      rtx stack_adjust_insn = insn;
	      int stack_adjust_amount = INTVAL (XEXP (SET_SRC (body), 1));
	      int total_pushed = 0;
	      int pushes = 0;

	      /* Find all successive push insns.  */
	      p = insn;
	      /* Don't convert more than three pushes;
		 that starts adding too many displaced addresses
		 and the whole thing starts becoming a losing
		 proposition.  */
	      while (pushes < 3)
		{
		  rtx pbody, dest;
		  p = next_nonnote_insn (p);
		  if (p == 0 || GET_CODE (p) != INSN)
		    break;
		  pbody = PATTERN (p);
		  if (GET_CODE (pbody) != SET)
		    break;
		  dest = SET_DEST (pbody);
		  /* Allow a no-op move between the adjust and the push.  */
		  if (GET_CODE (dest) == REG
		      && GET_CODE (SET_SRC (pbody)) == REG
		      && REGNO (dest) == REGNO (SET_SRC (pbody)))
		    continue;
		  if (! (GET_CODE (dest) == MEM
			 && GET_CODE (XEXP (dest, 0)) == POST_INC
			 && XEXP (XEXP (dest, 0), 0) == stack_pointer_rtx))
		    break;
		  pushes++;
		  if (total_pushed + GET_MODE_SIZE (GET_MODE (SET_DEST (pbody)))
		      > stack_adjust_amount)
		    break;
		  total_pushed += GET_MODE_SIZE (GET_MODE (SET_DEST (pbody)));
		}

	      /* Discard the amount pushed from the stack adjust;
		 maybe eliminate it entirely.  */
	      if (total_pushed >= stack_adjust_amount)
		{
		  delete_computation (stack_adjust_insn);
		  total_pushed = stack_adjust_amount;
		}
	      else
		XEXP (SET_SRC (PATTERN (stack_adjust_insn)), 1)
		  = GEN_INT (stack_adjust_amount - total_pushed);

	      /* Change the appropriate push insns to ordinary stores.  */
	      p = insn;
	      while (total_pushed > 0)
		{
		  rtx pbody, dest;
		  p = next_nonnote_insn (p);
		  if (GET_CODE (p) != INSN)
		    break;
		  pbody = PATTERN (p);
		  if (GET_CODE (pbody) != SET)
		    break;
		  dest = SET_DEST (pbody);
		  /* Allow a no-op move between the adjust and the push.  */
		  if (GET_CODE (dest) == REG
		      && GET_CODE (SET_SRC (pbody)) == REG
		      && REGNO (dest) == REGNO (SET_SRC (pbody)))
		    continue;
		  if (! (GET_CODE (dest) == MEM
			 && GET_CODE (XEXP (dest, 0)) == POST_INC
			 && XEXP (XEXP (dest, 0), 0) == stack_pointer_rtx))
		    break;
		  total_pushed -= GET_MODE_SIZE (GET_MODE (SET_DEST (pbody)));
		  /* If this push doesn't fully fit in the space
		     of the stack adjust that we deleted,
		     make another stack adjust here for what we
		     didn't use up.  There should be peepholes
		     to recognize the resulting sequence of insns.  */
		  if (total_pushed < 0)
		    {
		      emit_insn_before (gen_add2_insn (stack_pointer_rtx,
						       GEN_INT (- total_pushed)),
					p);
		      break;
		    }
		  XEXP (dest, 0)
		    = plus_constant (stack_pointer_rtx, total_pushed);
		}
	    }
#endif

	  /* Detect and delete no-op move instructions
	     resulting from not allocating a parameter in a register.  */

	  if (GET_CODE (body) == SET
	      && (SET_DEST (body) == SET_SRC (body)
		  || (GET_CODE (SET_DEST (body)) == MEM
		      && GET_CODE (SET_SRC (body)) == MEM
		      && rtx_equal_p (SET_SRC (body), SET_DEST (body))))
	      && ! (GET_CODE (SET_DEST (body)) == MEM
		    && MEM_VOLATILE_P (SET_DEST (body)))
	      && ! (GET_CODE (SET_SRC (body)) == MEM
		    && MEM_VOLATILE_P (SET_SRC (body))))
	    delete_computation (insn);

	  /* Detect and ignore no-op move instructions
	     resulting from smart or fortuitous register allocation.  */

	  else if (GET_CODE (body) == SET)
	    {
	      int sreg = true_regnum (SET_SRC (body));
	      int dreg = true_regnum (SET_DEST (body));

	      if (sreg == dreg && sreg >= 0)
		delete_insn (insn);
	      else if (sreg >= 0 && dreg >= 0)
		{
		  rtx trial;
		  rtx tem = find_equiv_reg (NULL_RTX, insn, 0,
					    sreg, NULL_PTR, dreg,
					    GET_MODE (SET_SRC (body)));

		  if (tem != 0
		      && GET_MODE (tem) == GET_MODE (SET_DEST (body)))
		    {
		      /* DREG may have been the target of a REG_DEAD note in
			 the insn which makes INSN redundant.  If so, reorg
			 would still think it is dead.  So search for such a
			 note and delete it if we find it.  */
		      if (! find_regno_note (insn, REG_UNUSED, dreg))
			for (trial = prev_nonnote_insn (insn);
			     trial && GET_CODE (trial) != CODE_LABEL;
			     trial = prev_nonnote_insn (trial))
			  if (find_regno_note (trial, REG_DEAD, dreg))
			    {
			      remove_death (dreg, trial);
			      break;
			    }

		      /* Deleting insn could lose a death-note for SREG.  */
		      if ((trial = find_regno_note (insn, REG_DEAD, sreg)))
			{
			  /* Change this into a USE so that we won't emit
			     code for it, but still can keep the note.  */
			  PATTERN (insn)
			    = gen_rtx_USE (VOIDmode, XEXP (trial, 0));
			  INSN_CODE (insn) = -1;
			  /* Remove all reg notes but the REG_DEAD one.  */
			  REG_NOTES (insn) = trial;
			  XEXP (trial, 1) = NULL_RTX;
			}
		      else
			delete_insn (insn);
		    }
		}
	      else if (dreg >= 0 && CONSTANT_P (SET_SRC (body))
		       && find_equiv_reg (SET_SRC (body), insn, 0, dreg,
					  NULL_PTR, 0,
					  GET_MODE (SET_DEST (body))))
		{
		  /* This handles the case where we have two consecutive
		     assignments of the same constant to pseudos that didn't
		     get a hard reg.  Each SET from the constant will be
		     converted into a SET of the spill register and an
		     output reload will be made following it.  This produces
		     two loads of the same constant into the same spill
		     register.  */

		  rtx in_insn = insn;

		  /* Look back for a death note for the first reg.
		     If there is one, it is no longer accurate.  */
		  while (in_insn && GET_CODE (in_insn) != CODE_LABEL)
		    {
		      if ((GET_CODE (in_insn) == INSN
			   || GET_CODE (in_insn) == JUMP_INSN)
			  && find_regno_note (in_insn, REG_DEAD, dreg))
			{
			  remove_death (dreg, in_insn);
			  break;
			}
		      in_insn = PREV_INSN (in_insn);
		    }

		  /* Delete the second load of the value.  */
		  delete_insn (insn);
		}
	    }
	  else if (GET_CODE (body) == PARALLEL)
	    {
	      /* If each part is a set between two identical registers or
		 a USE or CLOBBER, delete the insn.  */
	      int i, sreg, dreg;
	      rtx tem;

	      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
		{
		  tem = XVECEXP (body, 0, i);
		  if (GET_CODE (tem) == USE || GET_CODE (tem) == CLOBBER)
		    continue;

		  if (GET_CODE (tem) != SET
		      || (sreg = true_regnum (SET_SRC (tem))) < 0
		      || (dreg = true_regnum (SET_DEST (tem))) < 0
		      || dreg != sreg)
		    break;
		}
		  
	      if (i < 0)
		delete_insn (insn);
	    }
	  /* Also delete insns to store bit fields if they are no-ops.  */
	  /* Not worth the hair to detect this in the big-endian case.  */
	  else if (! BYTES_BIG_ENDIAN
		   && GET_CODE (body) == SET
		   && GET_CODE (SET_DEST (body)) == ZERO_EXTRACT
		   && XEXP (SET_DEST (body), 2) == const0_rtx
		   && XEXP (SET_DEST (body), 0) == SET_SRC (body)
		   && ! (GET_CODE (SET_SRC (body)) == MEM
			 && MEM_VOLATILE_P (SET_SRC (body))))
	    delete_insn (insn);
	}
      insn = next;
    }
}

/* See if there is still a NOTE_INSN_FUNCTION_END in this function.
   If so indicate that this function can drop off the end by returning
   1, else return 0.

   CHECK_DELETED indicates whether we must check if the note being
   searched for has the deleted flag set.

   DELETE_FINAL_NOTE indicates whether we should delete the note
   if we find it.  */

static int
calculate_can_reach_end (last, check_deleted, delete_final_note)
     rtx last;
     int check_deleted;
     int delete_final_note;
{
  rtx insn = last;
  int n_labels = 1;

  while (insn != NULL_RTX)
    {
      int ok = 0;

      /* One label can follow the end-note: the return label.  */
      if (GET_CODE (insn) == CODE_LABEL && n_labels-- > 0)
	ok = 1;
      /* Ordinary insns can follow it if returning a structure.  */
      else if (GET_CODE (insn) == INSN)
	ok = 1;
      /* If machine uses explicit RETURN insns, no epilogue,
	 then one of them follows the note.  */
      else if (GET_CODE (insn) == JUMP_INSN
	       && GET_CODE (PATTERN (insn)) == RETURN)
	ok = 1;
      /* A barrier can follow the return insn.  */
      else if (GET_CODE (insn) == BARRIER)
	ok = 1;
      /* Other kinds of notes can follow also.  */
      else if (GET_CODE (insn) == NOTE
	       && NOTE_LINE_NUMBER (insn) != NOTE_INSN_FUNCTION_END)
	ok = 1;

      if (ok != 1)
	break;

      insn = PREV_INSN (insn);
    }

  /* See if we backed up to the appropriate type of note.  */
  if (insn != NULL_RTX
      && GET_CODE (insn) == NOTE
      && NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_END
      && (check_deleted == 0
	  || ! INSN_DELETED_P (insn)))
    {
      if (delete_final_note)
	delete_insn (insn);
      return 1;
    }

  return 0;
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
	      REG_NOTES (copy)
		= copy_insn_1 (gen_rtx_EXPR_LIST (REG_NOTE_KIND (link),
					       XEXP (link, 0),
					       REG_NOTES (copy)));
	  if (reg_map && REG_NOTES (copy))
	    replace_regs (REG_NOTES (copy), reg_map, max_reg, 1);
	  break;
	  
	case JUMP_INSN:
	  copy = emit_jump_insn_before (copy_insn (PATTERN (insn)), loop_start);
	  if (reg_map)
	    replace_regs (PATTERN (copy), reg_map, max_reg, 1);
	  mark_jump_label (PATTERN (copy), copy, 0);
	  if (REG_NOTES (insn))
	    {
	      REG_NOTES (copy) = copy_insn_1 (REG_NOTES (insn));
	      if (reg_map)
		replace_regs (REG_NOTES (copy), reg_map, max_reg, 1);
	    }
	  
	  /* If this is a simple jump, add it to the jump chain.  */
	  
	  if (INSN_UID (copy) < max_jump_chain && JUMP_LABEL (copy)
	      && simplejump_p (copy))
	    {
	      jump_chain[INSN_UID (copy)]
		= jump_chain[INSN_UID (JUMP_LABEL (copy))];
	      jump_chain[INSN_UID (JUMP_LABEL (copy))] = copy;
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
      if (INSN_UID (copy) < max_jump_chain
	  && INSN_UID (JUMP_LABEL (copy)) < max_jump_chain)
	{
	  jump_chain[INSN_UID (copy)]
	    = jump_chain[INSN_UID (JUMP_LABEL (copy))];
	  jump_chain[INSN_UID (JUMP_LABEL (copy))] = copy;
	}
      emit_barrier_before (loop_start);
    }

  /* Now scan from the first insn we copied to the last insn we copied
     (copy) for new pseudo registers.  Do this after the code to jump to
     the end label since that might create a new pseudo too.  */
  reg_scan_update (first_copy, copy, max_reg);

  /* Mark the exit code as the virtual top of the converted loop.  */
  emit_note_before (NOTE_INSN_LOOP_VTOP, exitcode);

  delete_insn (next_nonnote_insn (loop_start));
  
  /* Clean up.  */
  if (reg_map)
    free (reg_map);

  return 1;
}

/* Move all block-beg, block-end, loop-beg, loop-cont, loop-vtop, and
   loop-end notes between START and END out before START.  Assume that
   END is not such a note.  START may be such a note.  Returns the value
   of the new starting insn, which may be different if the original start
   was such a note.  */

rtx
squeeze_notes (start, end)
     rtx start, end;
{
  rtx insn;
  rtx next;

  for (insn = start; insn != end; insn = next)
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
    }

  return start;
}

/* Compare the instructions before insn E1 with those before E2
   to find an opportunity for cross jumping.
   (This means detecting identical sequences of insns followed by
   jumps to the same place, or followed by a label and a jump
   to that label, and replacing one with a jump to the other.)

   Assume E1 is a jump that jumps to label E2
   (that is not always true but it might as well be).
   Find the longest possible equivalent sequences
   and store the first insns of those sequences into *F1 and *F2.
   Store zero there if no equivalent preceding instructions are found.

   We give up if we find a label in stream 1.
   Actually we could transfer that label into stream 2.  */

static void
find_cross_jump (e1, e2, minimum, f1, f2)
     rtx e1, e2;
     int minimum;
     rtx *f1, *f2;
{
  register rtx i1 = e1, i2 = e2;
  register rtx p1, p2;
  int lose = 0;

  rtx last1 = 0, last2 = 0;
  rtx afterlast1 = 0, afterlast2 = 0;

  *f1 = 0;
  *f2 = 0;

  while (1)
    {
      i1 = prev_nonnote_insn (i1);

      i2 = PREV_INSN (i2);
      while (i2 && (GET_CODE (i2) == NOTE || GET_CODE (i2) == CODE_LABEL))
	i2 = PREV_INSN (i2);

      if (i1 == 0)
	break;

      /* Don't allow the range of insns preceding E1 or E2
	 to include the other (E2 or E1).  */
      if (i2 == e1 || i1 == e2)
	break;

      /* If we will get to this code by jumping, those jumps will be
	 tensioned to go directly to the new label (before I2),
	 so this cross-jumping won't cost extra.  So reduce the minimum.  */
      if (GET_CODE (i1) == CODE_LABEL)
	{
	  --minimum;
	  break;
	}

      if (i2 == 0 || GET_CODE (i1) != GET_CODE (i2))
	break;

      /* Avoid moving insns across EH regions if either of the insns
	 can throw.  */
      if (flag_exceptions
	  && (asynchronous_exceptions || GET_CODE (i1) == CALL_INSN)
	  && !in_same_eh_region (i1, i2))
	break;

      p1 = PATTERN (i1);
      p2 = PATTERN (i2);
	
      /* If this is a CALL_INSN, compare register usage information.
	 If we don't check this on stack register machines, the two
	 CALL_INSNs might be merged leaving reg-stack.c with mismatching
	 numbers of stack registers in the same basic block.
	 If we don't check this on machines with delay slots, a delay slot may
	 be filled that clobbers a parameter expected by the subroutine.

	 ??? We take the simple route for now and assume that if they're
	 equal, they were constructed identically.  */

      if (GET_CODE (i1) == CALL_INSN
	  && ! rtx_equal_p (CALL_INSN_FUNCTION_USAGE (i1),
			    CALL_INSN_FUNCTION_USAGE (i2)))
	lose = 1;

#ifdef STACK_REGS
      /* If cross_jump_death_matters is not 0, the insn's mode
	 indicates whether or not the insn contains any stack-like
	 regs.  */

      if (!lose && cross_jump_death_matters && stack_regs_mentioned (i1))
	{
	  /* If register stack conversion has already been done, then
	     death notes must also be compared before it is certain that
	     the two instruction streams match.  */

	  rtx note;
	  HARD_REG_SET i1_regset, i2_regset;

	  CLEAR_HARD_REG_SET (i1_regset);
	  CLEAR_HARD_REG_SET (i2_regset);

	  for (note = REG_NOTES (i1); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD
		&& STACK_REG_P (XEXP (note, 0)))
	      SET_HARD_REG_BIT (i1_regset, REGNO (XEXP (note, 0)));

	  for (note = REG_NOTES (i2); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD
		&& STACK_REG_P (XEXP (note, 0)))
	      SET_HARD_REG_BIT (i2_regset, REGNO (XEXP (note, 0)));

	  GO_IF_HARD_REG_EQUAL (i1_regset, i2_regset, done);

	  lose = 1;

	done:
	  ;
	}
#endif

      /* Don't allow old-style asm or volatile extended asms to be accepted
	 for cross jumping purposes.  It is conceptually correct to allow
	 them, since cross-jumping preserves the dynamic instruction order
	 even though it is changing the static instruction order.  However,
	 if an asm is being used to emit an assembler pseudo-op, such as
	 the MIPS `.set reorder' pseudo-op, then the static instruction order
	 matters and it must be preserved.  */
      if (GET_CODE (p1) == ASM_INPUT || GET_CODE (p2) == ASM_INPUT
	  || (GET_CODE (p1) == ASM_OPERANDS && MEM_VOLATILE_P (p1))
	  || (GET_CODE (p2) == ASM_OPERANDS && MEM_VOLATILE_P (p2)))
	lose = 1;

      if (lose || GET_CODE (p1) != GET_CODE (p2)
	  || ! rtx_renumbered_equal_p (p1, p2))
	{
	  /* The following code helps take care of G++ cleanups.  */
	  rtx equiv1;
	  rtx equiv2;

	  if (!lose && GET_CODE (p1) == GET_CODE (p2)
	      && ((equiv1 = find_reg_note (i1, REG_EQUAL, NULL_RTX)) != 0
		  || (equiv1 = find_reg_note (i1, REG_EQUIV, NULL_RTX)) != 0)
	      && ((equiv2 = find_reg_note (i2, REG_EQUAL, NULL_RTX)) != 0
		  || (equiv2 = find_reg_note (i2, REG_EQUIV, NULL_RTX)) != 0)
	      /* If the equivalences are not to a constant, they may
		 reference pseudos that no longer exist, so we can't
		 use them.  */
	      && CONSTANT_P (XEXP (equiv1, 0))
	      && rtx_equal_p (XEXP (equiv1, 0), XEXP (equiv2, 0)))
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
		    goto win;
		}
	    }

	  /* Insns fail to match; cross jumping is limited to the following
	     insns.  */

#ifdef HAVE_cc0
	  /* Don't allow the insn after a compare to be shared by
	     cross-jumping unless the compare is also shared.
	     Here, if either of these non-matching insns is a compare,
	     exclude the following insn from possible cross-jumping.  */
	  if (sets_cc0_p (p1) || sets_cc0_p (p2))
	    last1 = afterlast1, last2 = afterlast2, ++minimum;
#endif

	  /* If cross-jumping here will feed a jump-around-jump
	     optimization, this jump won't cost extra, so reduce
	     the minimum.  */
	  if (GET_CODE (i1) == JUMP_INSN
	      && JUMP_LABEL (i1)
	      && prev_real_insn (JUMP_LABEL (i1)) == e1)
	    --minimum;
	  break;
	}

    win:
      if (GET_CODE (p1) != USE && GET_CODE (p1) != CLOBBER)
	{
	  /* Ok, this insn is potentially includable in a cross-jump here.  */
	  afterlast1 = last1, afterlast2 = last2;
	  last1 = i1, last2 = i2, --minimum;
	}
    }

  if (minimum <= 0 && last1 != 0 && last1 != e1)
    *f1 = last1, *f2 = last2;
}

static void
do_cross_jump (insn, newjpos, newlpos)
     rtx insn, newjpos, newlpos;
{
  /* Find an existing label at this point
     or make a new one if there is none.  */
  register rtx label = get_label_before (newlpos);

  /* Make the same jump insn jump to the new point.  */
  if (GET_CODE (PATTERN (insn)) == RETURN)
    {
      /* Remove from jump chain of returns.  */
      delete_from_jump_chain (insn);
      /* Change the insn.  */
      PATTERN (insn) = gen_jump (label);
      INSN_CODE (insn) = -1;
      JUMP_LABEL (insn) = label;
      LABEL_NUSES (label)++;
      /* Add to new the jump chain.  */
      if (INSN_UID (label) < max_jump_chain
	  && INSN_UID (insn) < max_jump_chain)
	{
	  jump_chain[INSN_UID (insn)] = jump_chain[INSN_UID (label)];
	  jump_chain[INSN_UID (label)] = insn;
	}
    }
  else
    redirect_jump (insn, label);

  /* Delete the matching insns before the jump.  Also, remove any REG_EQUAL
     or REG_EQUIV note in the NEWLPOS stream that isn't also present in
     the NEWJPOS stream.  */

  while (newjpos != insn)
    {
      rtx lnote;

      for (lnote = REG_NOTES (newlpos); lnote; lnote = XEXP (lnote, 1))
	if ((REG_NOTE_KIND (lnote) == REG_EQUAL
	     || REG_NOTE_KIND (lnote) == REG_EQUIV)
	    && ! find_reg_note (newjpos, REG_EQUAL, XEXP (lnote, 0))
	    && ! find_reg_note (newjpos, REG_EQUIV, XEXP (lnote, 0)))
	  remove_note (newlpos, lnote);

      delete_insn (newjpos);
      newjpos = next_real_insn (newjpos);
      newlpos = next_real_insn (newlpos);
    }
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

/* Return 1 if INSN is a jump that jumps to right after TARGET
   only on the condition that TARGET itself would drop through.
   Assumes that TARGET is a conditional jump.  */

static int
jump_back_p (insn, target)
     rtx insn, target;
{
  rtx cinsn, ctarget;
  enum rtx_code codei, codet;

  if (simplejump_p (insn) || ! condjump_p (insn)
      || simplejump_p (target)
      || target != prev_real_insn (JUMP_LABEL (insn)))
    return 0;

  cinsn = XEXP (SET_SRC (PATTERN (insn)), 0);
  ctarget = XEXP (SET_SRC (PATTERN (target)), 0);

  codei = GET_CODE (cinsn);
  codet = GET_CODE (ctarget);

  if (XEXP (SET_SRC (PATTERN (insn)), 1) == pc_rtx)
    {
      if (! can_reverse_comparison_p (cinsn, insn))
	return 0;
      codei = reverse_condition (codei);
    }

  if (XEXP (SET_SRC (PATTERN (target)), 2) == pc_rtx)
    {
      if (! can_reverse_comparison_p (ctarget, target))
	return 0;
      codet = reverse_condition (codet);
    }

  return (codei == codet
	  && rtx_renumbered_equal_p (XEXP (cinsn, 0), XEXP (ctarget, 0))
	  && rtx_renumbered_equal_p (XEXP (cinsn, 1), XEXP (ctarget, 1)));
}

/* Given a comparison, COMPARISON, inside a conditional jump insn, INSN,
   return non-zero if it is safe to reverse this comparison.  It is if our
   floating-point is not IEEE, if this is an NE or EQ comparison, or if
   this is known to be an integer comparison.  */

int
can_reverse_comparison_p (comparison, insn)
     rtx comparison;
     rtx insn;
{
  rtx arg0;

  /* If this is not actually a comparison, we can't reverse it.  */
  if (GET_RTX_CLASS (GET_CODE (comparison)) != '<')
    return 0;

  if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
      /* If this is an NE comparison, it is safe to reverse it to an EQ
	 comparison and vice versa, even for floating point.  If no operands
	 are NaNs, the reversal is valid.  If some operand is a NaN, EQ is
	 always false and NE is always true, so the reversal is also valid.  */
      || flag_fast_math
      || GET_CODE (comparison) == NE
      || GET_CODE (comparison) == EQ)
    return 1;

  arg0 = XEXP (comparison, 0);

  /* Make sure ARG0 is one of the actual objects being compared.  If we
     can't do this, we can't be sure the comparison can be reversed. 

     Handle cc0 and a MODE_CC register.  */
  if ((GET_CODE (arg0) == REG && GET_MODE_CLASS (GET_MODE (arg0)) == MODE_CC)
#ifdef HAVE_cc0
      || arg0 == cc0_rtx
#endif
      )
    {
      rtx prev = prev_nonnote_insn (insn);
      rtx set;

      /* First see if the condition code mode alone if enough to say we can
	 reverse the condition.  If not, then search backwards for a set of
	 ARG0. We do not need to check for an insn clobbering it since valid
	 code will contain set a set with no intervening clobber.  But
	 stop when we reach a label.  */
#ifdef REVERSIBLE_CC_MODE
      if (GET_MODE_CLASS (GET_MODE (arg0)) == MODE_CC
	  && REVERSIBLE_CC_MODE (GET_MODE (arg0)))
	return 1;
#endif
	
      for (prev = prev_nonnote_insn (insn);
	   prev != 0 && GET_CODE (prev) != CODE_LABEL;
	   prev = prev_nonnote_insn (prev))
	if ((set = single_set (prev)) != 0
	    && rtx_equal_p (SET_DEST (set), arg0))
	  {
	    arg0 = SET_SRC (set);

	    if (GET_CODE (arg0) == COMPARE)
	      arg0 = XEXP (arg0, 0);
	    break;
	  }
    }

  /* We can reverse this if ARG0 is a CONST_INT or if its mode is
     not VOIDmode and neither a MODE_CC nor MODE_FLOAT type.  */
  return (GET_CODE (arg0) == CONST_INT
	  || (GET_MODE (arg0) != VOIDmode
	      && GET_MODE_CLASS (GET_MODE (arg0)) != MODE_CC
	      && GET_MODE_CLASS (GET_MODE (arg0)) != MODE_FLOAT));
}

/* Given an rtx-code for a comparison, return the code
   for the negated comparison.
   WATCH OUT!  reverse_condition is not safe to use on a jump
   that might be acting on the results of an IEEE floating point comparison,
   because of the special treatment of non-signaling nans in comparisons.  
   Use can_reverse_comparison_p to be sure.  */

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

    default:
      abort ();
      return UNKNOWN;
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

    default:
      abort ();
      return UNKNOWN;
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
  if (code1 == code2)
    return 1;

  switch (code1)
    {
    case EQ:
      if (code2 == LE || code2 == LEU || code2 == GE || code2 == GEU)
	return 1;
      break;

    case LT:
      if (code2 == LE || code2 == NE)
	return 1;
      break;

    case GT:
      if (code2 == GE || code2 == NE)
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
   and nothing more.  */

int
condjump_p (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);

  if (GET_CODE (x) != SET
      || GET_CODE (SET_DEST (x)) != PC)
    return 0;

  x = SET_SRC (x);
  if (GET_CODE (x) == LABEL_REF)
    return 1;
  else return (GET_CODE (x) == IF_THEN_ELSE
	       && ((GET_CODE (XEXP (x, 2)) == PC
		    && (GET_CODE (XEXP (x, 1)) == LABEL_REF
			|| GET_CODE (XEXP (x, 1)) == RETURN))
		   || (GET_CODE (XEXP (x, 1)) == PC
		       && (GET_CODE (XEXP (x, 2)) == LABEL_REF
			   || GET_CODE (XEXP (x, 2)) == RETURN))));

  return 0;
}

/* Return nonzero if INSN is a (possibly) conditional jump inside a
   PARALLEL.  */

int
condjump_in_parallel_p (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);

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

/* Return the label of a conditional jump.  */

rtx
condjump_label (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);

  if (GET_CODE (x) == PARALLEL)
    x = XVECEXP (x, 0, 0);
  if (GET_CODE (x) != SET)
    return NULL_RTX;
  if (GET_CODE (SET_DEST (x)) != PC)
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
  return x && GET_CODE (x) == RETURN;
}

int
returnjump_p (insn)
     rtx insn;
{
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

/* Return 1 if X is an RTX that does nothing but set the condition codes
   and CLOBBER or USE registers.
   Return -1 if X does explicitly set the condition codes,
   but also does other things.  */

int
sets_cc0_p (x)
     rtx x ATTRIBUTE_UNUSED;
{
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
  register rtx insn;
  register rtx next;
  register rtx value = label;
  register int depth;

  for (depth = 0;
       (depth < 10
	&& (insn = next_active_insn (value)) != 0
	&& GET_CODE (insn) == JUMP_INSN
	&& ((JUMP_LABEL (insn) != 0 && simplejump_p (insn))
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

/* Assuming that field IDX of X is a vector of label_refs,
   replace each of them by the ultimate label reached by it.
   Return nonzero if a change is made.
   If IGNORE_LOOPS is 0, we do not chain across a NOTE_INSN_LOOP_BEG.  */

static int
tension_vector_labels (x, idx)
     register rtx x;
     register int idx;
{
  int changed = 0;
  register int i;
  for (i = XVECLEN (x, idx) - 1; i >= 0; i--)
    {
      register rtx olabel = XEXP (XVECEXP (x, idx, i), 0);
      register rtx nlabel = follow_jumps (olabel);
      if (nlabel && nlabel != olabel)
	{
	  XEXP (XVECEXP (x, idx, i), 0) = nlabel;
	  ++LABEL_NUSES (nlabel);
	  if (--LABEL_NUSES (olabel) == 0)
	    delete_insn (olabel);
	  changed = 1;
	}
    }
  return changed;
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
   that loop-optimization is done with.

   Once reload has completed (CROSS_JUMP non-zero), we need not consider
   two labels distinct if they are separated by only USE or CLOBBER insns.  */

static void
mark_jump_label (x, insn, cross_jump)
     register rtx x;
     rtx insn;
     int cross_jump;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register const char *fmt;

  switch (code)
    {
    case PC:
    case CC0:
    case REG:
    case SUBREG:
    case CONST_INT:
    case SYMBOL_REF:
    case CONST_DOUBLE:
    case CLOBBER:
    case CALL:
      return;

    case MEM:
      /* If this is a constant-pool reference, see if it is a label.  */
      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))
	mark_jump_label (get_pool_constant (XEXP (x, 0)), insn, cross_jump);
      break;

    case LABEL_REF:
      {
	rtx label = XEXP (x, 0);
	rtx olabel = label;
	rtx note;
	rtx next;

	if (GET_CODE (label) != CODE_LABEL)
	  abort ();

	/* Ignore references to labels of containing functions.  */
	if (LABEL_REF_NONLOCAL_P (x))
	  break;

	/* If there are other labels following this one,
	   replace it with the last of the consecutive labels.  */
	for (next = NEXT_INSN (label); next; next = NEXT_INSN (next))
	  {
	    if (GET_CODE (next) == CODE_LABEL)
	      label = next;
	    else if (cross_jump && GET_CODE (next) == INSN
		     && (GET_CODE (PATTERN (next)) == USE
			 || GET_CODE (PATTERN (next)) == CLOBBER))
	      continue;
	    else if (GET_CODE (next) != NOTE)
	      break;
	    else if (! cross_jump
		     && (NOTE_LINE_NUMBER (next) == NOTE_INSN_LOOP_BEG
			 || NOTE_LINE_NUMBER (next) == NOTE_INSN_FUNCTION_END
			 /* ??? Optional.  Disables some optimizations, but
			    makes gcov output more accurate with -O.  */
			 || (flag_test_coverage && NOTE_LINE_NUMBER (next) > 0)))
	      break;
	  }

	XEXP (x, 0) = label;
	if (! insn || ! INSN_DELETED_P (insn))
	  ++LABEL_NUSES (label);

	if (insn)
	  {
	    if (GET_CODE (insn) == JUMP_INSN)
	      JUMP_LABEL (insn) = label;

	    /* If we've changed OLABEL and we had a REG_LABEL note
	       for it, update it as well.  */
	    else if (label != olabel
		     && (note = find_reg_note (insn, REG_LABEL, olabel)) != 0)
	      XEXP (note, 0) = label;

	    /* Otherwise, add a REG_LABEL note for LABEL unless there already
	       is one.  */
	    else if (! find_reg_note (insn, REG_LABEL, label))
	      {
		/* This code used to ignore labels which refered to dispatch
		   tables to avoid flow.c generating worse code.

		   However, in the presense of global optimizations like
		   gcse which call find_basic_blocks without calling
		   life_analysis, not recording such labels will lead
		   to compiler aborts because of inconsistencies in the
		   flow graph.  So we go ahead and record the label.

		   It may also be the case that the optimization argument
		   is no longer valid because of the more accurate cfg
		   we build in find_basic_blocks -- it no longer pessimizes
		   code when it finds a REG_LABEL note.  */
		REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_LABEL, label,
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
	    mark_jump_label (XVECEXP (x, eltnum, i), NULL_RTX, cross_jump);
	}
      return;
      
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	mark_jump_label (XEXP (x, i), insn, cross_jump);
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    mark_jump_label (XVECEXP (x, i, j), insn, cross_jump);
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
  register rtx set = single_set (insn);

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
	  && (! CONST_CALL_P (our_prev)
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
		    = dest_regno + (dest_regno < FIRST_PSEUDO_REGISTER 
		      ? HARD_REGNO_NREGS (dest_regno,
				GET_MODE (SET_DEST (pat))) : 1);
	      int regno = REGNO (reg);
	      int endregno = regno + (regno < FIRST_PSEUDO_REGISTER 
			     ? HARD_REGNO_NREGS (regno, GET_MODE (reg)) : 1);

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
		    = gen_rtx_EXPR_LIST (REG_UNUSED, reg, REG_NOTES (our_prev));

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
  rtx set;

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

#ifdef INSN_SCHEDULING
  /* ?!? The schedulers do not keep REG_DEAD notes accurate after
     reload has completed.  The schedulers need to be fixed.  Until
     they are, we must not rely on the death notes here.  */
  if (reload_completed && flag_schedule_insns_after_reload)
    {
      delete_insn (insn);
      return;
    }
#endif

  /* The REG_DEAD note may have been omitted for a register
     which is both set and used by the insn.  */
  set = single_set (insn);
  if (set && GET_CODE (SET_DEST (set)) == REG)
    {
    int dest_regno = REGNO (SET_DEST (set));
    int dest_endregno
	  = dest_regno + (dest_regno < FIRST_PSEUDO_REGISTER 
	    ? HARD_REGNO_NREGS (dest_regno,
				GET_MODE (SET_DEST (set))) : 1);
    int i;

    for (i = dest_regno; i < dest_endregno; i++)
      {
	if (! refers_to_regno_p (i, i + 1, SET_SRC (set), NULL_PTR)
	    || find_regno_note (insn, REG_DEAD, i))
	  continue;

	note = gen_rtx_EXPR_LIST (REG_DEAD, (i < FIRST_PSEUDO_REGISTER
					     ? gen_rtx_REG (reg_raw_mode[i], i)
					     : SET_DEST (set)), NULL_RTX);
	delete_prior_computation (note, insn);
      }
    }

  for (note = REG_NOTES (insn); note; note = next)
    {
      next = XEXP (note, 1);

      if (REG_NOTE_KIND (note) != REG_DEAD
	  /* Verify that the REG_NOTE is legitimate.  */
	  || GET_CODE (XEXP (note, 0)) != REG)
	continue;

      delete_prior_computation (note, insn);
    }

  delete_insn (insn);
}

/* Delete insn INSN from the chain of insns and update label ref counts.
   May delete some following insns as a consequence; may even delete
   a label elsewhere and insns that follow it.

   Returns the first insn after INSN that was not deleted.  */

rtx
delete_insn (insn)
     register rtx insn;
{
  register rtx next = NEXT_INSN (insn);
  register rtx prev = PREV_INSN (insn);
  register int was_code_label = (GET_CODE (insn) == CODE_LABEL);
  register int dont_really_delete = 0;

  while (next && INSN_DELETED_P (next))
    next = NEXT_INSN (next);

  /* This insn is already deleted => return first following nondeleted.  */
  if (INSN_DELETED_P (insn))
    return next;

  if (was_code_label)
    remove_node_from_expr_list (insn, &nonlocal_goto_handler_labels);

  /* Don't delete user-declared labels.  Convert them to special NOTEs
     instead.  */
  if (was_code_label && LABEL_NAME (insn) != 0
      && optimize && ! dont_really_delete)
    {
      PUT_CODE (insn, NOTE);
      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED_LABEL;
      NOTE_SOURCE_FILE (insn) = 0;
      dont_really_delete = 1;
    }
  else
    /* Mark this insn as deleted.  */
    INSN_DELETED_P (insn) = 1;

  /* If this is an unconditional jump, delete it from the jump chain.  */
  if (simplejump_p (insn))
    delete_from_jump_chain (insn);

  /* If instruction is followed by a barrier,
     delete the barrier too.  */

  if (next != 0 && GET_CODE (next) == BARRIER)
    {
      INSN_DELETED_P (next) = 1;
      next = NEXT_INSN (next);
    }

  /* Patch out INSN (and the barrier if any) */

  if (optimize && ! dont_really_delete)
    {
      if (prev)
	{
	  NEXT_INSN (prev) = next;
	  if (GET_CODE (prev) == INSN && GET_CODE (PATTERN (prev)) == SEQUENCE)
	    NEXT_INSN (XVECEXP (PATTERN (prev), 0,
				XVECLEN (PATTERN (prev), 0) - 1)) = next;
	}

      if (next)
	{
	  PREV_INSN (next) = prev;
	  if (GET_CODE (next) == INSN && GET_CODE (PATTERN (next)) == SEQUENCE)
	    PREV_INSN (XVECEXP (PATTERN (next), 0, 0)) = prev;
	}

      if (prev && NEXT_INSN (prev) == 0)
	set_last_insn (prev);
    }

  /* If deleting a jump, decrement the count of the label,
     and delete the label if it is now unused.  */

  if (GET_CODE (insn) == JUMP_INSN && JUMP_LABEL (insn))
    {
      rtx lab = JUMP_LABEL (insn), lab_next;

      if (--LABEL_NUSES (lab) == 0)
	{
	  /* This can delete NEXT or PREV,
	     either directly if NEXT is JUMP_LABEL (INSN),
	     or indirectly through more levels of jumps.  */
	  delete_insn (lab);

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
	     We may not be able to kill the label immediately preceeding
	     just yet, as it might be referenced in code leading up to
	     the tablejump.  */
	  delete_insn (lab_next);
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
	if (--LABEL_NUSES (XEXP (XVECEXP (pat, diff_vec_p, i), 0)) == 0)
	  delete_insn (XEXP (XVECEXP (pat, diff_vec_p, i), 0));
      while (next && INSN_DELETED_P (next))
	next = NEXT_INSN (next);
      return next;
    }

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
    next = delete_insn (NEXT_INSN (insn));

  /* If INSN was a label, delete insns following it if now unreachable.  */

  if (was_code_label && prev && GET_CODE (prev) == BARRIER)
    {
      register RTX_CODE code;
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
	    next = delete_insn (next);
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
     register rtx from, to;
{
  register rtx insn = from;

  while (1)
    {
      register rtx next = NEXT_INSN (insn);
      register rtx prev = PREV_INSN (insn);

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
never_reached_warning (avoided_insn)
     rtx avoided_insn;
{
  rtx insn;
  rtx a_line_note = NULL;
  int two_avoided_lines = 0;
  int contains_insn = 0;
  
  if (! warn_notreached)
    return;

  /* Scan forwards, looking at LINE_NUMBER notes, until
     we hit a LABEL or we run out of insns.  */
  
  for (insn = avoided_insn; insn != NULL; insn = NEXT_INSN (insn))
    {
       if (GET_CODE (insn) == CODE_LABEL)
	 break;
       else if (GET_CODE (insn) == NOTE		/* A line number note? */ 
		&& NOTE_LINE_NUMBER (insn) >= 0)
	{
	  if (a_line_note == NULL)
	    a_line_note = insn;
	  else
	    two_avoided_lines |= (NOTE_LINE_NUMBER (a_line_note)
				  != NOTE_LINE_NUMBER (insn));
	}
       else if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	 contains_insn = 1;
    }
  if (two_avoided_lines && contains_insn)
    warning_with_file_and_line (NOTE_SOURCE_FILE (a_line_note),
				NOTE_LINE_NUMBER (a_line_note),
				"will never be executed");
}

/* Invert the condition of the jump JUMP, and make it jump
   to label NLABEL instead of where it jumps now.  */

int
invert_jump (jump, nlabel)
     rtx jump, nlabel;
{
  /* We have to either invert the condition and change the label or
     do neither.  Either operation could fail.  We first try to invert
     the jump. If that succeeds, we try changing the label.  If that fails,
     we invert the jump back to what it was.  */

  if (! invert_exp (PATTERN (jump), jump))
    return 0;

  if (redirect_jump (jump, nlabel))
    {
      if (flag_branch_probabilities)
	{
	  rtx note = find_reg_note (jump, REG_BR_PROB, 0);

	  /* An inverted jump means that a probability taken becomes a
	     probability not taken.  Subtract the branch probability from the
	     probability base to convert it back to a taken probability.
	     (We don't flip the probability on a branch that's never taken.  */
	  if (note && XINT (XEXP (note, 0), 0) >= 0)
	    XINT (XEXP (note, 0), 0) = REG_BR_PROB_BASE - XINT (XEXP (note, 0), 0);
	}

      return 1;
    }

  if (! invert_exp (PATTERN (jump), jump))
    /* This should just be putting it back the way it was.  */
    abort ();

  return  0;
}

/* Invert the jump condition of rtx X contained in jump insn, INSN. 

   Return 1 if we can do so, 0 if we cannot find a way to do so that
   matches a pattern.  */

int
invert_exp (x, insn)
     rtx x;
     rtx insn;
{
  register RTX_CODE code;
  register int i;
  register const char *fmt;

  code = GET_CODE (x);

  if (code == IF_THEN_ELSE)
    {
      register rtx comp = XEXP (x, 0);
      register rtx tem;

      /* We can do this in two ways:  The preferable way, which can only
	 be done if this is not an integer comparison, is to reverse
	 the comparison code.  Otherwise, swap the THEN-part and ELSE-part
	 of the IF_THEN_ELSE.  If we can't do either, fail.  */

      if (can_reverse_comparison_p (comp, insn)
	  && validate_change (insn, &XEXP (x, 0),
			      gen_rtx_fmt_ee (reverse_condition (GET_CODE (comp)),
					      GET_MODE (comp), XEXP (comp, 0),
					      XEXP (comp, 1)), 0))
	return 1;
				       
      tem = XEXP (x, 1);
      validate_change (insn, &XEXP (x, 1), XEXP (x, 2), 1);
      validate_change (insn, &XEXP (x, 2), tem, 1);
      return apply_change_group ();
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (! invert_exp (XEXP (x, i), insn))
	    return 0;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (!invert_exp (XVECEXP (x, i, j), insn))
	      return 0;
	}
    }

  return 1;
}

/* Make jump JUMP jump to label NLABEL instead of where it jumps now.
   If the old jump target label is unused as a result,
   it and the code following it may be deleted.

   If NLABEL is zero, we are to turn the jump into a (possibly conditional)
   RETURN insn.

   The return value will be 1 if the change was made, 0 if it wasn't (this
   can only occur for NLABEL == 0).  */

int
redirect_jump (jump, nlabel)
     rtx jump, nlabel;
{
  register rtx olabel = JUMP_LABEL (jump);

  if (nlabel == olabel)
    return 1;

  if (! redirect_exp (&PATTERN (jump), olabel, nlabel, jump))
    return 0;

  /* If this is an unconditional branch, delete it from the jump_chain of
     OLABEL and add it to the jump_chain of NLABEL (assuming both labels
     have UID's in range and JUMP_CHAIN is valid).  */
  if (jump_chain && (simplejump_p (jump)
		     || GET_CODE (PATTERN (jump)) == RETURN))
    {
      int label_index = nlabel ? INSN_UID (nlabel) : 0;

      delete_from_jump_chain (jump);
      if (label_index < max_jump_chain
	  && INSN_UID (jump) < max_jump_chain)
	{
	  jump_chain[INSN_UID (jump)] = jump_chain[label_index];
	  jump_chain[label_index] = jump;
	}
    }

  JUMP_LABEL (jump) = nlabel;
  if (nlabel)
    ++LABEL_NUSES (nlabel);

  if (olabel && --LABEL_NUSES (olabel) == 0)
    delete_insn (olabel);

  return 1;
}

/* Delete the instruction JUMP from any jump chain it might be on.  */

static void
delete_from_jump_chain (jump)
     rtx jump;
{
  int index;
  rtx olabel = JUMP_LABEL (jump);

  /* Handle unconditional jumps.  */
  if (jump_chain && olabel != 0
      && INSN_UID (olabel) < max_jump_chain
      && simplejump_p (jump))
    index = INSN_UID (olabel);
  /* Handle return insns.  */
  else if (jump_chain && GET_CODE (PATTERN (jump)) == RETURN)
    index = 0;
  else return;

  if (jump_chain[index] == jump)
    jump_chain[index] = jump_chain[INSN_UID (jump)];
  else
    {
      rtx insn;

      for (insn = jump_chain[index];
	   insn != 0;
	   insn = jump_chain[INSN_UID (insn)])
	if (jump_chain[INSN_UID (insn)] == jump)
	  {
	    jump_chain[INSN_UID (insn)] = jump_chain[INSN_UID (jump)];
	    break;
	  }
    }
}

/* If NLABEL is nonzero, throughout the rtx at LOC,
   alter (LABEL_REF OLABEL) to (LABEL_REF NLABEL).  If OLABEL is
   zero, alter (RETURN) to (LABEL_REF NLABEL).

   If NLABEL is zero, alter (LABEL_REF OLABEL) to (RETURN) and check
   validity with validate_change.  Convert (set (pc) (label_ref olabel))
   to (return).

   Return 0 if we found a change we would like to make but it is invalid.
   Otherwise, return 1.  */

int
redirect_exp (loc, olabel, nlabel, insn)
     rtx *loc;
     rtx olabel, nlabel;
     rtx insn;
{
  register rtx x = *loc;
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register const char *fmt;

  if (code == LABEL_REF)
    {
      if (XEXP (x, 0) == olabel)
	{
	  if (nlabel)
	    XEXP (x, 0) = nlabel;
	  else
	    return validate_change (insn, loc, gen_rtx_RETURN (VOIDmode), 0);
	  return 1;
	}
    }
  else if (code == RETURN && olabel == 0)
    {
      x = gen_rtx_LABEL_REF (VOIDmode, nlabel);
      if (loc == &PATTERN (insn))
	x = gen_rtx_SET (VOIDmode, pc_rtx, x);
      return validate_change (insn, loc, x, 0);
    }

  if (code == SET && nlabel == 0 && SET_DEST (x) == pc_rtx
      && GET_CODE (SET_SRC (x)) == LABEL_REF
      && XEXP (SET_SRC (x), 0) == olabel)
    return validate_change (insn, loc, gen_rtx_RETURN (VOIDmode), 0);

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (! redirect_exp (&XEXP (x, i), olabel, nlabel, insn))
	    return 0;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (! redirect_exp (&XVECEXP (x, i, j), olabel, nlabel, insn))
	      return 0;
	}
    }

  return 1;
}

/* Make jump JUMP jump to label NLABEL, assuming it used to be a tablejump.

   If the old jump target label (before the dispatch table) becomes unused,
   it and the dispatch table may be deleted.  In that case, find the insn
   before the jump references that label and delete it and logical successors
   too.  */

static void
redirect_tablejump (jump, nlabel)
     rtx jump, nlabel;
{
  register rtx olabel = JUMP_LABEL (jump);

  /* Add this jump to the jump_chain of NLABEL.  */
  if (jump_chain && INSN_UID (nlabel) < max_jump_chain
      && INSN_UID (jump) < max_jump_chain)
    {
      jump_chain[INSN_UID (jump)] = jump_chain[INSN_UID (nlabel)];
      jump_chain[INSN_UID (nlabel)] = jump;
    }

  PATTERN (jump) = gen_jump (nlabel);
  JUMP_LABEL (jump) = nlabel;
  ++LABEL_NUSES (nlabel);
  INSN_CODE (jump) = -1;

  if (--LABEL_NUSES (olabel) == 0)
    {
      delete_labelref_insn (jump, olabel, 0);
      delete_insn (olabel);
    }
}

/* Find the insn referencing LABEL that is a logical predecessor of INSN.
   If we found one, delete it and then delete this insn if DELETE_THIS is
   non-zero.  Return non-zero if INSN or a predecessor references LABEL.  */

static int
delete_labelref_insn (insn, label, delete_this)
     rtx insn, label;
     int delete_this;
{
  int deleted = 0;
  rtx link;

  if (GET_CODE (insn) != NOTE
      && reg_mentioned_p (label, PATTERN (insn)))
    {
      if (delete_this)
	{
	  delete_insn (insn);
	  deleted = 1;
	}
      else
	return 1;
    }

  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
    if (delete_labelref_insn (XEXP (link, 0), label, 1))
      {
	if (delete_this)
	  {
	    delete_insn (insn);
	    deleted = 1;
	  }
	else
	  return 1;
      }

  return deleted;
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
  register int i;
  register RTX_CODE code = GET_CODE (x);
  register const char *fmt;
      
  if (x == y)
    return 1;

  if ((code == REG || (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG))
      && (GET_CODE (y) == REG || (GET_CODE (y) == SUBREG
				  && GET_CODE (SUBREG_REG (y)) == REG)))
    {
      int reg_x = -1, reg_y = -1;
      int word_x = 0, word_y = 0;

      if (GET_MODE (x) != GET_MODE (y))
	return 0;

      /* If we haven't done any renumbering, don't
	 make any assumptions.  */
      if (reg_renumber == 0)
	return rtx_equal_p (x, y);

      if (code == SUBREG)
	{
	  reg_x = REGNO (SUBREG_REG (x));
	  word_x = SUBREG_WORD (x);

	  if (reg_renumber[reg_x] >= 0)
	    {
	      reg_x = reg_renumber[reg_x] + word_x;
	      word_x = 0;
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
	  word_y = SUBREG_WORD (y);

	  if (reg_renumber[reg_y] >= 0)
	    {
	      reg_y = reg_renumber[reg_y];
	      word_y = 0;
	    }
	}

      else
	{
	  reg_y = REGNO (y);
	  if (reg_renumber[reg_y] >= 0)
	    reg_y = reg_renumber[reg_y];
	}

      return reg_x >= 0 && reg_x == reg_y && word_x == word_y;
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
      register int j;
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
	return SUBREG_WORD (x) + base;
    }
  return -1;
}

/* Optimize code of the form:

	for (x = a[i]; x; ...)
	  ...
	for (x = a[i]; x; ...)
	  ...
      foo:

   Loop optimize will change the above code into

	if (x = a[i])
	  for (;;)
	     { ...; if (! (x = ...)) break; }
	if (x = a[i])
	  for (;;)
	     { ...; if (! (x = ...)) break; }
      foo:

   In general, if the first test fails, the program can branch
   directly to `foo' and skip the second try which is doomed to fail.
   We run this after loop optimization and before flow analysis.  */
   
/* When comparing the insn patterns, we track the fact that different
   pseudo-register numbers may have been used in each computation.
   The following array stores an equivalence -- same_regs[I] == J means
   that pseudo register I was used in the first set of tests in a context
   where J was used in the second set.  We also count the number of such
   pending equivalences.  If nonzero, the expressions really aren't the
   same.  */

static int *same_regs;

static int num_same_regs;

/* Track any registers modified between the target of the first jump and
   the second jump.  They never compare equal.  */

static char *modified_regs;

/* Record if memory was modified.  */

static int modified_mem;

/* Called via note_stores on each insn between the target of the first 
   branch and the second branch.  It marks any changed registers.  */

static void
mark_modified_reg (dest, x, data)
     rtx dest;
     rtx x ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
{
  int regno, i;

  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (GET_CODE (dest) == MEM)
    modified_mem = 1;

  if (GET_CODE (dest) != REG)
    return;

  regno = REGNO (dest);
  if (regno >= FIRST_PSEUDO_REGISTER)
    modified_regs[regno] = 1;
  else
    for (i = 0; i < HARD_REGNO_NREGS (regno, GET_MODE (dest)); i++)
      modified_regs[regno + i] = 1;
}

/* F is the first insn in the chain of insns.  */
   
void
thread_jumps (f, max_reg, flag_before_loop)
     rtx f;
     int max_reg;
     int flag_before_loop;
{
  /* Basic algorithm is to find a conditional branch,
     the label it may branch to, and the branch after
     that label.  If the two branches test the same condition,
     walk back from both branch paths until the insn patterns
     differ, or code labels are hit.  If we make it back to
     the target of the first branch, then we know that the first branch
     will either always succeed or always fail depending on the relative
     senses of the two branches.  So adjust the first branch accordingly
     in this case.  */
     
  rtx label, b1, b2, t1, t2;
  enum rtx_code code1, code2;
  rtx b1op0, b1op1, b2op0, b2op1;
  int changed = 1;
  int i;
  int *all_reset;

  /* Allocate register tables and quick-reset table.  */
  modified_regs = (char *) xmalloc (max_reg * sizeof (char));
  same_regs = (int *) xmalloc (max_reg * sizeof (int));
  all_reset = (int *) xmalloc (max_reg * sizeof (int));
  for (i = 0; i < max_reg; i++)
    all_reset[i] = -1;
    
  while (changed)
    {
      changed = 0;

      for (b1 = f; b1; b1 = NEXT_INSN (b1))
	{
	  /* Get to a candidate branch insn.  */
	  if (GET_CODE (b1) != JUMP_INSN
	      || ! condjump_p (b1) || simplejump_p (b1)
	      || JUMP_LABEL (b1) == 0)
	    continue;

	  bzero (modified_regs, max_reg * sizeof (char));
	  modified_mem = 0;

	  bcopy ((char *) all_reset, (char *) same_regs,
		 max_reg * sizeof (int));
	  num_same_regs = 0;

	  label = JUMP_LABEL (b1);

	  /* Look for a branch after the target.  Record any registers and
	     memory modified between the target and the branch.  Stop when we
	     get to a label since we can't know what was changed there.  */
	  for (b2 = NEXT_INSN (label); b2; b2 = NEXT_INSN (b2))
	    {
	      if (GET_CODE (b2) == CODE_LABEL)
		break;

	      else if (GET_CODE (b2) == JUMP_INSN)
		{
		  /* If this is an unconditional jump and is the only use of
		     its target label, we can follow it.  */
		  if (simplejump_p (b2)
		      && JUMP_LABEL (b2) != 0
		      && LABEL_NUSES (JUMP_LABEL (b2)) == 1)
		    {
		      b2 = JUMP_LABEL (b2);
		      continue;
		    }
		  else
		    break;
		}

	      if (GET_CODE (b2) != CALL_INSN && GET_CODE (b2) != INSN)
		continue;

	      if (GET_CODE (b2) == CALL_INSN)
		{
		  modified_mem = 1;
		  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		    if (call_used_regs[i] && ! fixed_regs[i]
			&& i != STACK_POINTER_REGNUM
			&& i != FRAME_POINTER_REGNUM
			&& i != HARD_FRAME_POINTER_REGNUM
			&& i != ARG_POINTER_REGNUM)
		      modified_regs[i] = 1;
		}

	      note_stores (PATTERN (b2), mark_modified_reg, NULL);
	    }

	  /* Check the next candidate branch insn from the label
	     of the first.  */
	  if (b2 == 0
	      || GET_CODE (b2) != JUMP_INSN
	      || b2 == b1
	      || ! condjump_p (b2)
	      || simplejump_p (b2))
	    continue;

	  /* Get the comparison codes and operands, reversing the
	     codes if appropriate.  If we don't have comparison codes,
	     we can't do anything.  */
	  b1op0 = XEXP (XEXP (SET_SRC (PATTERN (b1)), 0), 0);
	  b1op1 = XEXP (XEXP (SET_SRC (PATTERN (b1)), 0), 1);
	  code1 = GET_CODE (XEXP (SET_SRC (PATTERN (b1)), 0));
	  if (XEXP (SET_SRC (PATTERN (b1)), 1) == pc_rtx)
	    code1 = reverse_condition (code1);

	  b2op0 = XEXP (XEXP (SET_SRC (PATTERN (b2)), 0), 0);
	  b2op1 = XEXP (XEXP (SET_SRC (PATTERN (b2)), 0), 1);
	  code2 = GET_CODE (XEXP (SET_SRC (PATTERN (b2)), 0));
	  if (XEXP (SET_SRC (PATTERN (b2)), 1) == pc_rtx)
	    code2 = reverse_condition (code2);

	  /* If they test the same things and knowing that B1 branches
	     tells us whether or not B2 branches, check if we
	     can thread the branch.  */
	  if (rtx_equal_for_thread_p (b1op0, b2op0, b2)
	      && rtx_equal_for_thread_p (b1op1, b2op1, b2)
	      && (comparison_dominates_p (code1, code2)
		  || (comparison_dominates_p (code1, reverse_condition (code2))
		      && can_reverse_comparison_p (XEXP (SET_SRC (PATTERN (b1)),
							 0),
						   b1))))
	    {
	      t1 = prev_nonnote_insn (b1);
	      t2 = prev_nonnote_insn (b2);
	      
	      while (t1 != 0 && t2 != 0)
		{
		  if (t2 == label)
		    {
		      /* We have reached the target of the first branch.
		         If there are no pending register equivalents,
			 we know that this branch will either always
			 succeed (if the senses of the two branches are
			 the same) or always fail (if not).  */
		      rtx new_label;

		      if (num_same_regs != 0)
			break;

		      if (comparison_dominates_p (code1, code2))
		      	new_label = JUMP_LABEL (b2);
		      else
			new_label = get_label_after (b2);

		      if (JUMP_LABEL (b1) != new_label)
			{
			  rtx prev = PREV_INSN (new_label);

			  if (flag_before_loop
			      && GET_CODE (prev) == NOTE
			      && NOTE_LINE_NUMBER (prev) == NOTE_INSN_LOOP_BEG)
			    {
			      /* Don't thread to the loop label.  If a loop
				 label is reused, loop optimization will
				 be disabled for that loop.  */
			      new_label = gen_label_rtx ();
			      emit_label_after (new_label, PREV_INSN (prev));
			    }
			  changed |= redirect_jump (b1, new_label);
			}
		      break;
		    }
		    
		  /* If either of these is not a normal insn (it might be
		     a JUMP_INSN, CALL_INSN, or CODE_LABEL) we fail.  (NOTEs
		     have already been skipped above.)  Similarly, fail
		     if the insns are different.  */
		  if (GET_CODE (t1) != INSN || GET_CODE (t2) != INSN
		      || recog_memoized (t1) != recog_memoized (t2)
		      || ! rtx_equal_for_thread_p (PATTERN (t1),
						   PATTERN (t2), t2))
		    break;
		    
		  t1 = prev_nonnote_insn (t1);
		  t2 = prev_nonnote_insn (t2);
		}
	    }
	}
    }

  /* Clean up.  */
  free (modified_regs);
  free (same_regs);
  free (all_reset);
}

/* This is like RTX_EQUAL_P except that it knows about our handling of
   possibly equivalent registers and knows to consider volatile and
   modified objects as not equal.
   
   YINSN is the insn containing Y.  */

int
rtx_equal_for_thread_p (x, y, yinsn)
     rtx x, y;
     rtx yinsn;
{
  register int i;
  register int j;
  register enum rtx_code code;
  register const char *fmt;

  code = GET_CODE (x);
  /* Rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* For floating-point, consider everything unequal.  This is a bit
     pessimistic, but this pass would only rarely do anything for FP
     anyway.  */
  if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
      && FLOAT_MODE_P (GET_MODE (x)) && ! flag_fast_math)
    return 0;

  /* For commutative operations, the RTX match if the operand match in any
     order.  Also handle the simple binary and unary cases without a loop.  */
  if (code == EQ || code == NE || GET_RTX_CLASS (code) == 'c')
    return ((rtx_equal_for_thread_p (XEXP (x, 0), XEXP (y, 0), yinsn)
	     && rtx_equal_for_thread_p (XEXP (x, 1), XEXP (y, 1), yinsn))
	    || (rtx_equal_for_thread_p (XEXP (x, 0), XEXP (y, 1), yinsn)
		&& rtx_equal_for_thread_p (XEXP (x, 1), XEXP (y, 0), yinsn)));
  else if (GET_RTX_CLASS (code) == '<' || GET_RTX_CLASS (code) == '2')
    return (rtx_equal_for_thread_p (XEXP (x, 0), XEXP (y, 0), yinsn)
	    && rtx_equal_for_thread_p (XEXP (x, 1), XEXP (y, 1), yinsn));
  else if (GET_RTX_CLASS (code) == '1')
    return rtx_equal_for_thread_p (XEXP (x, 0), XEXP (y, 0), yinsn);

  /* Handle special-cases first.  */
  switch (code)
    {
    case REG:
      if (REGNO (x) == REGNO (y) && ! modified_regs[REGNO (x)])
        return 1;

      /* If neither is user variable or hard register, check for possible
	 equivalence.  */
      if (REG_USERVAR_P (x) || REG_USERVAR_P (y)
	  || REGNO (x) < FIRST_PSEUDO_REGISTER
	  || REGNO (y) < FIRST_PSEUDO_REGISTER)
	return 0;

      if (same_regs[REGNO (x)] == -1)
	{
	  same_regs[REGNO (x)] = REGNO (y);
	  num_same_regs++;

	  /* If this is the first time we are seeing a register on the `Y'
	     side, see if it is the last use.  If not, we can't thread the 
	     jump, so mark it as not equivalent.  */
	  if (REGNO_LAST_UID (REGNO (y)) != INSN_UID (yinsn))
	    return 0;

	  return 1;
	}
      else
	return (same_regs[REGNO (x)] == REGNO (y));

      break;

    case MEM:
      /* If memory modified or either volatile, not equivalent.
	 Else, check address.  */
      if (modified_mem || MEM_VOLATILE_P (x) || MEM_VOLATILE_P (y))
	return 0;

      return rtx_equal_for_thread_p (XEXP (x, 0), XEXP (y, 0), yinsn);

    case ASM_INPUT:
      if (MEM_VOLATILE_P (x) || MEM_VOLATILE_P (y))
	return 0;

      break;

    case SET:
      /* Cancel a pending `same_regs' if setting equivalenced registers.
	 Then process source.  */
      if (GET_CODE (SET_DEST (x)) == REG
          && GET_CODE (SET_DEST (y)) == REG)
	{
          if (same_regs[REGNO (SET_DEST (x))] == REGNO (SET_DEST (y)))
	    {
	      same_regs[REGNO (SET_DEST (x))] = -1;
	      num_same_regs--;
	    }
	  else if (REGNO (SET_DEST (x)) != REGNO (SET_DEST (y)))
	    return 0;
	}
      else
	if (rtx_equal_for_thread_p (SET_DEST (x), SET_DEST (y), yinsn) == 0)
	  return 0;

      return rtx_equal_for_thread_p (SET_SRC (x), SET_SRC (y), yinsn);

    case LABEL_REF:
      return XEXP (x, 0) == XEXP (y, 0);

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);
      
    default:
      break;
    }

  if (x == y)
    return 1;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'n':
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'V':
	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_for_thread_p (XVECEXP (x, i, j),
	    			        XVECEXP (y, i, j), yinsn) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_thread_p (XEXP (x, i), XEXP (y, i), yinsn) == 0)
	    return 0;
	  break;

	case 'S':
	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'u':
	  /* These are just backpointers, so they don't matter.  */
	  break;

	case '0':
	case 't':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1;
}


#if !defined(HAVE_cc0) && !defined(HAVE_conditional_arithmetic)
/* Return the insn that NEW can be safely inserted in front of starting at
   the jump insn INSN.  Return 0 if it is not safe to do this jump
   optimization.  Note that NEW must contain a single set. */

static rtx
find_insert_position (insn, new)
     rtx insn;
     rtx new;
{
  int i;
  rtx prev;

  /* If NEW does not clobber, it is safe to insert NEW before INSN. */
  if (GET_CODE (PATTERN (new)) != PARALLEL)
    return insn;

  for (i = XVECLEN (PATTERN (new), 0) - 1; i >= 0; i--)
    if (GET_CODE (XVECEXP (PATTERN (new), 0, i)) == CLOBBER
	&& reg_overlap_mentioned_p (XEXP (XVECEXP (PATTERN (new), 0, i), 0),
				    insn))
      break;

  if (i < 0)
    return insn;

  /* There is a good chance that the previous insn PREV sets the thing
     being clobbered (often the CC in a hard reg).  If PREV does not
     use what NEW sets, we can insert NEW before PREV. */

  prev = prev_active_insn (insn);
  for (i = XVECLEN (PATTERN (new), 0) - 1; i >= 0; i--)
    if (GET_CODE (XVECEXP (PATTERN (new), 0, i)) == CLOBBER
	&& reg_overlap_mentioned_p (XEXP (XVECEXP (PATTERN (new), 0, i), 0),
				    insn)
	&& ! modified_in_p (XEXP (XVECEXP (PATTERN (new), 0, i), 0),
			    prev))
      return 0;

  return reg_mentioned_p (SET_DEST (single_set (new)), prev) ? 0 : prev;
}
#endif /* !HAVE_cc0 */
