/* Combine stack adjustments.
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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

/* Track stack adjustments and stack memory references.  Attempt to
   reduce the number of stack adjustments by back-propagating across
   the memory references.

   This is intended primarily for use with targets that do not define
   ACCUMULATE_OUTGOING_ARGS.  It is of significantly more value to
   targets that define PREFERRED_STACK_BOUNDARY more aligned than
   STACK_BOUNDARY (e.g. x86), or if not all registers can be pushed
   (e.g. x86 fp regs) which would ordinarily have to be implemented
   as a sub/mov pair due to restrictions in calls.c.

   Propagation stops when any of the insns that need adjusting are
   (a) no longer valid because we've exceeded their range, (b) a
   non-trivial push instruction, or (c) a call instruction.

   Restriction B is based on the assumption that push instructions
   are smaller or faster.  If a port really wants to remove all
   pushes, it should have defined ACCUMULATE_OUTGOING_ARGS.  The
   one exception that is made is for an add immediately followed
   by a push.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "insn-config.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfgrtl.h"
#include "tree-pass.h"
#include "rtl-iter.h"


/* This structure records two kinds of stack references between stack
   adjusting instructions: stack references in memory addresses for
   regular insns and all stack references for debug insns.  */

struct csa_reflist
{
  HOST_WIDE_INT sp_offset;
  rtx_insn *insn;
  rtx *ref;
  struct csa_reflist *next;
};

static int stack_memref_p (rtx);
static rtx single_set_for_csa (rtx_insn *);
static void free_csa_reflist (struct csa_reflist *);
static struct csa_reflist *record_one_stack_ref (rtx_insn *, rtx *,
						 struct csa_reflist *);
static int try_apply_stack_adjustment (rtx_insn *, struct csa_reflist *,
				       HOST_WIDE_INT, HOST_WIDE_INT);
static void combine_stack_adjustments_for_block (basic_block);


/* Main entry point for stack adjustment combination.  */

static void
combine_stack_adjustments (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    combine_stack_adjustments_for_block (bb);
}

/* Recognize a MEM of the form (sp) or (plus sp const).  */

static int
stack_memref_p (rtx x)
{
  if (!MEM_P (x))
    return 0;
  x = XEXP (x, 0);

  if (x == stack_pointer_rtx)
    return 1;
  if (GET_CODE (x) == PLUS
      && XEXP (x, 0) == stack_pointer_rtx
      && CONST_INT_P (XEXP (x, 1)))
    return 1;

  return 0;
}

/* Recognize either normal single_set or the hack in i386.md for
   tying fp and sp adjustments.  */

static rtx
single_set_for_csa (rtx_insn *insn)
{
  int i;
  rtx tmp = single_set (insn);
  if (tmp)
    return tmp;

  if (!NONJUMP_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) != PARALLEL)
    return NULL_RTX;

  tmp = PATTERN (insn);
  if (GET_CODE (XVECEXP (tmp, 0, 0)) != SET)
    return NULL_RTX;

  for (i = 1; i < XVECLEN (tmp, 0); ++i)
    {
      rtx this_rtx = XVECEXP (tmp, 0, i);

      /* The special case is allowing a no-op set.  */
      if (GET_CODE (this_rtx) == SET
	  && SET_SRC (this_rtx) == SET_DEST (this_rtx))
	;
      else if (GET_CODE (this_rtx) != CLOBBER
	       && GET_CODE (this_rtx) != CLOBBER_HIGH
	       && GET_CODE (this_rtx) != USE)
	return NULL_RTX;
    }

  return XVECEXP (tmp, 0, 0);
}

/* Free the list of csa_reflist nodes.  */

static void
free_csa_reflist (struct csa_reflist *reflist)
{
  struct csa_reflist *next;
  for (; reflist ; reflist = next)
    {
      next = reflist->next;
      free (reflist);
    }
}

/* Create a new csa_reflist node from the given stack reference.
   It is already known that the reference is either a MEM satisfying the
   predicate stack_memref_p or a REG representing the stack pointer.  */

static struct csa_reflist *
record_one_stack_ref (rtx_insn *insn, rtx *ref, struct csa_reflist *next_reflist)
{
  struct csa_reflist *ml;

  ml = XNEW (struct csa_reflist);

  if (REG_P (*ref) || XEXP (*ref, 0) == stack_pointer_rtx)
    ml->sp_offset = 0;
  else
    ml->sp_offset = INTVAL (XEXP (XEXP (*ref, 0), 1));

  ml->insn = insn;
  ml->ref = ref;
  ml->next = next_reflist;

  return ml;
}

/* We only know how to adjust the CFA; no other frame-related changes
   may appear in any insn to be deleted.  */

static bool
no_unhandled_cfa (rtx_insn *insn)
{
  if (!RTX_FRAME_RELATED_P (insn))
    return true;

  /* No CFA notes at all is a legacy interpretation like
     FRAME_RELATED_EXPR, and is context sensitive within
     the prologue state machine.  We can't handle that here.  */
  bool has_cfa_adjust = false;

  for (rtx link = REG_NOTES (insn); link; link = XEXP (link, 1))
    switch (REG_NOTE_KIND (link))
      {
      default:
        break;
      case REG_CFA_ADJUST_CFA:
	has_cfa_adjust = true;
	break;

      case REG_FRAME_RELATED_EXPR:
      case REG_CFA_DEF_CFA:
      case REG_CFA_OFFSET:
      case REG_CFA_REGISTER:
      case REG_CFA_EXPRESSION:
      case REG_CFA_RESTORE:
      case REG_CFA_SET_VDRAP:
      case REG_CFA_WINDOW_SAVE:
      case REG_CFA_FLUSH_QUEUE:
      case REG_CFA_TOGGLE_RA_MANGLE:
	return false;
      }

  return has_cfa_adjust;
}

/* Attempt to apply ADJUST to the stack adjusting insn INSN, as well
   as each of the memories and stack references in REFLIST.  Return true
   on success.  */

static int
try_apply_stack_adjustment (rtx_insn *insn, struct csa_reflist *reflist,
			    HOST_WIDE_INT new_adjust, HOST_WIDE_INT delta)
{
  struct csa_reflist *ml;
  rtx set;

  set = single_set_for_csa (insn);
  if (MEM_P (SET_DEST (set)))
    validate_change (insn, &SET_DEST (set),
		     replace_equiv_address (SET_DEST (set), stack_pointer_rtx),
		     1);
  else
    validate_change (insn, &XEXP (SET_SRC (set), 1), GEN_INT (new_adjust), 1);

  for (ml = reflist; ml ; ml = ml->next)
    {
      rtx new_addr = plus_constant (Pmode, stack_pointer_rtx,
				    ml->sp_offset - delta);
      rtx new_val;

      if (MEM_P (*ml->ref))
	new_val = replace_equiv_address_nv (*ml->ref, new_addr);
      else if (GET_MODE (*ml->ref) == GET_MODE (stack_pointer_rtx))
	new_val = new_addr;
      else
	new_val = lowpart_subreg (GET_MODE (*ml->ref), new_addr,
				  GET_MODE (new_addr));
      validate_change (ml->insn, ml->ref, new_val, 1);
    }

  if (apply_change_group ())
    {
      /* Succeeded.  Update our knowledge of the stack references.  */
      for (ml = reflist; ml ; ml = ml->next)
	ml->sp_offset -= delta;

      return 1;
    }
  else
    return 0;
}

/* For non-debug insns, record all stack memory references in INSN
   and return true if there were no other (unrecorded) references to the
   stack pointer.  For debug insns, record all stack references regardless
   of context and unconditionally return true.  */

static bool
record_stack_refs (rtx_insn *insn, struct csa_reflist **reflist)
{
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, &PATTERN (insn), NONCONST)
    {
      rtx *loc = *iter;
      rtx x = *loc;
      switch (GET_CODE (x))
	{
	case MEM:
	  if (!reg_mentioned_p (stack_pointer_rtx, x))
	    iter.skip_subrtxes ();
	  /* We are not able to handle correctly all possible memrefs
	     containing stack pointer, so this check is necessary.  */
	  else if (stack_memref_p (x))
	    {
	      *reflist = record_one_stack_ref (insn, loc, *reflist);
	      iter.skip_subrtxes ();
	    }
	  /* Try harder for DEBUG_INSNs, handle e.g.
	     (mem (mem (sp + 16) + 4).  */
	  else if (!DEBUG_INSN_P (insn))
	    return false;
	  break;

	case REG:
	  /* ??? We want be able to handle non-memory stack pointer
	     references later.  For now just discard all insns referring to
	     stack pointer outside mem expressions.  We would probably
	     want to teach validate_replace to simplify expressions first.

	     We can't just compare with STACK_POINTER_RTX because the
	     reference to the stack pointer might be in some other mode.
	     In particular, an explicit clobber in an asm statement will
	     result in a QImode clobber.

	     In DEBUG_INSNs, we want to replace all occurrences, otherwise
	     they will cause -fcompare-debug failures.  */
	  if (REGNO (x) == STACK_POINTER_REGNUM)
	    {
	      if (!DEBUG_INSN_P (insn))
		return false;
	      *reflist = record_one_stack_ref (insn, loc, *reflist);
	    }
	  break;

	default:
	  break;
	}
    }
  return true;
}

/* If INSN has a REG_ARGS_SIZE note, move it to LAST.
   AFTER is true iff LAST follows INSN in the instruction stream.  */

static void
maybe_move_args_size_note (rtx_insn *last, rtx_insn *insn, bool after)
{
  rtx note, last_note;

  note = find_reg_note (insn, REG_ARGS_SIZE, NULL_RTX);
  if (note == NULL)
    return;

  last_note = find_reg_note (last, REG_ARGS_SIZE, NULL_RTX);
  if (last_note)
    {
      /* The ARGS_SIZE notes are *not* cumulative.  They represent an
	 absolute value, and the "most recent" note wins.  */
      if (!after)
        XEXP (last_note, 0) = XEXP (note, 0);
    }
  else
    add_reg_note (last, REG_ARGS_SIZE, XEXP (note, 0));
}

/* Merge any REG_CFA_ADJUST_CFA note from SRC into DST.
   AFTER is true iff DST follows SRC in the instruction stream.  */

static void
maybe_merge_cfa_adjust (rtx_insn *dst, rtx_insn *src, bool after)
{
  rtx snote = NULL, dnote = NULL;
  rtx sexp, dexp;
  rtx exp1, exp2;

  if (RTX_FRAME_RELATED_P (src))
    snote = find_reg_note (src, REG_CFA_ADJUST_CFA, NULL_RTX);
  if (snote == NULL)
    return;
  sexp = XEXP (snote, 0);

  if (RTX_FRAME_RELATED_P (dst))
    dnote = find_reg_note (dst, REG_CFA_ADJUST_CFA, NULL_RTX);
  if (dnote == NULL)
    {
      add_reg_note (dst, REG_CFA_ADJUST_CFA, sexp);
      return;
    }
  dexp = XEXP (dnote, 0);

  gcc_assert (GET_CODE (sexp) == SET);
  gcc_assert (GET_CODE (dexp) == SET);

  if (after)
    exp1 = dexp, exp2 = sexp;
  else
    exp1 = sexp, exp2 = dexp;

  SET_SRC (exp1) = simplify_replace_rtx (SET_SRC (exp1), SET_DEST (exp2),
					 SET_SRC (exp2));
  XEXP (dnote, 0) = exp1;
}

/* Return the next (or previous) active insn within BB.  */

static rtx_insn *
prev_active_insn_bb (basic_block bb, rtx_insn *insn)
{
  for (insn = PREV_INSN (insn);
       insn != PREV_INSN (BB_HEAD (bb));
       insn = PREV_INSN (insn))
    if (active_insn_p (insn))
      return insn;
  return NULL;
}

static rtx_insn *
next_active_insn_bb (basic_block bb, rtx_insn *insn)
{
  for (insn = NEXT_INSN (insn);
       insn != NEXT_INSN (BB_END (bb));
       insn = NEXT_INSN (insn))
    if (active_insn_p (insn))
      return insn;
  return NULL;
}

/* If INSN has a REG_ARGS_SIZE note, if possible move it to PREV.  Otherwise
   search for a nearby candidate within BB where we can stick the note.  */

static void
force_move_args_size_note (basic_block bb, rtx_insn *prev, rtx_insn *insn)
{
  rtx note;
  rtx_insn *test, *next_candidate, *prev_candidate;

  /* If PREV exists, tail-call to the logic in the other function.  */
  if (prev)
    {
      maybe_move_args_size_note (prev, insn, false);
      return;
    }

  /* First, make sure there's anything that needs doing.  */
  note = find_reg_note (insn, REG_ARGS_SIZE, NULL_RTX);
  if (note == NULL)
    return;

  /* We need to find a spot between the previous and next exception points
     where we can place the note and "properly" deallocate the arguments.  */
  next_candidate = prev_candidate = NULL;

  /* It is often the case that we have insns in the order:
	call
	add sp (previous deallocation)
	sub sp (align for next arglist)
	push arg
     and the add/sub cancel.  Therefore we begin by searching forward.  */

  test = insn;
  while ((test = next_active_insn_bb (bb, test)) != NULL)
    {
      /* Found an existing note: nothing to do.  */
      if (find_reg_note (test, REG_ARGS_SIZE, NULL_RTX))
        return;
      /* Found something that affects unwinding.  Stop searching.  */
      if (CALL_P (test) || !insn_nothrow_p (test))
	break;
      if (next_candidate == NULL)
	next_candidate = test;
    }

  test = insn;
  while ((test = prev_active_insn_bb (bb, test)) != NULL)
    {
      rtx tnote;
      /* Found a place that seems logical to adjust the stack.  */
      tnote = find_reg_note (test, REG_ARGS_SIZE, NULL_RTX);
      if (tnote)
	{
	  XEXP (tnote, 0) = XEXP (note, 0);
	  return;
	}
      if (prev_candidate == NULL)
	prev_candidate = test;
      /* Found something that affects unwinding.  Stop searching.  */
      if (CALL_P (test) || !insn_nothrow_p (test))
	break;
    }

  if (prev_candidate)
    test = prev_candidate;
  else if (next_candidate)
    test = next_candidate;
  else
    {
      /* ??? We *must* have a place, lest we ICE on the lost adjustment.
	 Options are: dummy clobber insn, nop, or prevent the removal of
	 the sp += 0 insn.  */
      /* TODO: Find another way to indicate to the dwarf2 code that we
	 have not in fact lost an adjustment.  */
      test = emit_insn_before (gen_rtx_CLOBBER (VOIDmode, const0_rtx), insn);
    }
  add_reg_note (test, REG_ARGS_SIZE, XEXP (note, 0));
}

/* Subroutine of combine_stack_adjustments, called for each basic block.  */

static void
combine_stack_adjustments_for_block (basic_block bb)
{
  HOST_WIDE_INT last_sp_adjust = 0;
  rtx_insn *last_sp_set = NULL;
  rtx_insn *last2_sp_set = NULL;
  struct csa_reflist *reflist = NULL;
  rtx_insn *insn, *next;
  rtx set;
  bool end_of_block = false;

  for (insn = BB_HEAD (bb); !end_of_block ; insn = next)
    {
      end_of_block = insn == BB_END (bb);
      next = NEXT_INSN (insn);

      if (! INSN_P (insn))
	continue;

      set = single_set_for_csa (insn);
      if (set && find_reg_note (insn, REG_STACK_CHECK, NULL_RTX))
	set = NULL_RTX;
      if (set)
	{
	  rtx dest = SET_DEST (set);
	  rtx src = SET_SRC (set);

	  /* Find constant additions to the stack pointer.  */
	  if (dest == stack_pointer_rtx
	      && GET_CODE (src) == PLUS
	      && XEXP (src, 0) == stack_pointer_rtx
	      && CONST_INT_P (XEXP (src, 1)))
	    {
	      HOST_WIDE_INT this_adjust = INTVAL (XEXP (src, 1));

	      /* If we've not seen an adjustment previously, record
		 it now and continue.  */
	      if (! last_sp_set)
		{
		  last_sp_set = insn;
		  last_sp_adjust = this_adjust;
		  continue;
		}

	      /* If not all recorded refs can be adjusted, or the
		 adjustment is now too large for a constant addition,
		 we cannot merge the two stack adjustments.

		 Also we need to be careful to not move stack pointer
		 such that we create stack accesses outside the allocated
		 area.  We can combine an allocation into the first insn,
		 or a deallocation into the second insn.  We can not
		 combine an allocation followed by a deallocation.

		 The only somewhat frequent occurrence of the later is when
		 a function allocates a stack frame but does not use it.
		 For this case, we would need to analyze rtl stream to be
		 sure that allocated area is really unused.  This means not
		 only checking the memory references, but also all registers
		 or global memory references possibly containing a stack
		 frame address.

		 Perhaps the best way to address this problem is to teach
		 gcc not to allocate stack for objects never used.  */

	      /* Combine an allocation into the first instruction.  */
	      if (STACK_GROWS_DOWNWARD ? this_adjust <= 0 : this_adjust >= 0)
		{
		  if (no_unhandled_cfa (insn)
		      && try_apply_stack_adjustment (last_sp_set, reflist,
						     last_sp_adjust
						     + this_adjust,
						     this_adjust))
		    {
		      /* It worked!  */
		      maybe_move_args_size_note (last_sp_set, insn, false);
		      maybe_merge_cfa_adjust (last_sp_set, insn, false);
		      delete_insn (insn);
		      last_sp_adjust += this_adjust;
		      continue;
		    }
		}

	      /* Otherwise we have a deallocation.  Do not combine with
		 a previous allocation.  Combine into the second insn.  */
	      else if (STACK_GROWS_DOWNWARD
		       ? last_sp_adjust >= 0 : last_sp_adjust <= 0)
		{
		  if (no_unhandled_cfa (last_sp_set)
		      && try_apply_stack_adjustment (insn, reflist,
						     last_sp_adjust
						     + this_adjust,
						     -last_sp_adjust))
		    {
		      /* It worked!  */
		      maybe_move_args_size_note (insn, last_sp_set, true);
		      maybe_merge_cfa_adjust (insn, last_sp_set, true);
		      delete_insn (last_sp_set);
		      last_sp_set = insn;
		      last_sp_adjust += this_adjust;
		      free_csa_reflist (reflist);
		      reflist = NULL;
		      continue;
		    }
		}

	      /* Combination failed.  Restart processing from here.  If
		 deallocation+allocation conspired to cancel, we can
		 delete the old deallocation insn.  */
	      if (last_sp_set)
		{
		  if (last_sp_adjust == 0 && no_unhandled_cfa (last_sp_set))
		    {
		      maybe_move_args_size_note (insn, last_sp_set, true);
		      maybe_merge_cfa_adjust (insn, last_sp_set, true);
		      delete_insn (last_sp_set);
		    }
		  else
		    last2_sp_set = last_sp_set;
		}
	      free_csa_reflist (reflist);
	      reflist = NULL;
	      last_sp_set = insn;
	      last_sp_adjust = this_adjust;
	      continue;
	    }

	  /* Find a store with pre-(dec|inc)rement or pre-modify of exactly
	     the previous adjustment and turn it into a simple store.  This
	     is equivalent to anticipating the stack adjustment so this must
	     be an allocation.  */
	  if (MEM_P (dest)
	      && ((STACK_GROWS_DOWNWARD
		   ? (GET_CODE (XEXP (dest, 0)) == PRE_DEC
		      && known_eq (last_sp_adjust,
				   GET_MODE_SIZE (GET_MODE (dest))))
		   : (GET_CODE (XEXP (dest, 0)) == PRE_INC
		      && known_eq (-last_sp_adjust,
				   GET_MODE_SIZE (GET_MODE (dest)))))
		  || ((STACK_GROWS_DOWNWARD
		       ? last_sp_adjust >= 0 : last_sp_adjust <= 0)
		      && GET_CODE (XEXP (dest, 0)) == PRE_MODIFY
		      && GET_CODE (XEXP (XEXP (dest, 0), 1)) == PLUS
		      && XEXP (XEXP (XEXP (dest, 0), 1), 0)
			 == stack_pointer_rtx
		      && GET_CODE (XEXP (XEXP (XEXP (dest, 0), 1), 1))
		         == CONST_INT
		      && INTVAL (XEXP (XEXP (XEXP (dest, 0), 1), 1))
		         == -last_sp_adjust))
	      && XEXP (XEXP (dest, 0), 0) == stack_pointer_rtx
	      && !reg_mentioned_p (stack_pointer_rtx, src)
	      && memory_address_p (GET_MODE (dest), stack_pointer_rtx)
	      && try_apply_stack_adjustment (insn, reflist, 0,
					     -last_sp_adjust))
	    {
	      if (last2_sp_set)
		maybe_move_args_size_note (last2_sp_set, last_sp_set, false);
	      else
	        maybe_move_args_size_note (insn, last_sp_set, true);
	      delete_insn (last_sp_set);
	      free_csa_reflist (reflist);
	      reflist = NULL;
	      last_sp_set = NULL;
	      last_sp_adjust = 0;
	      continue;
	    }
	}

      if (!CALL_P (insn) && last_sp_set
	  && record_stack_refs (insn, &reflist))
	continue;

      /* Otherwise, we were not able to process the instruction.
	 Do not continue collecting data across such a one.  */
      if (last_sp_set
	  && (CALL_P (insn)
	      || reg_mentioned_p (stack_pointer_rtx, PATTERN (insn))))
	{
	  if (last_sp_set && last_sp_adjust == 0)
	    {
	      force_move_args_size_note (bb, last2_sp_set, last_sp_set);
	      delete_insn (last_sp_set);
	    }
	  free_csa_reflist (reflist);
	  reflist = NULL;
	  last2_sp_set = NULL;
	  last_sp_set = NULL;
	  last_sp_adjust = 0;
	}
    }

  if (last_sp_set && last_sp_adjust == 0)
    {
      force_move_args_size_note (bb, last2_sp_set, last_sp_set);
      delete_insn (last_sp_set);
    }

  if (reflist)
    free_csa_reflist (reflist);
}

static unsigned int
rest_of_handle_stack_adjustments (void)
{
  df_note_add_problem ();
  df_analyze ();
  combine_stack_adjustments ();
  return 0;
}

namespace {

const pass_data pass_data_stack_adjustments =
{
  RTL_PASS, /* type */
  "csa", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_COMBINE_STACK_ADJUST, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_stack_adjustments : public rtl_opt_pass
{
public:
  pass_stack_adjustments (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_stack_adjustments, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *)
    {
      return rest_of_handle_stack_adjustments ();
    }

}; // class pass_stack_adjustments

bool
pass_stack_adjustments::gate (function *)
{
  /* This is kind of a heuristic.  We need to run combine_stack_adjustments
     even for machines with possibly nonzero TARGET_RETURN_POPS_ARGS
     and ACCUMULATE_OUTGOING_ARGS.  We expect that only ports having
     push instructions will have popping returns.  */
#ifndef PUSH_ROUNDING
  if (ACCUMULATE_OUTGOING_ARGS)
    return false;
#endif
  return flag_combine_stack_adjustments;
}

} // anon namespace

rtl_opt_pass *
make_pass_stack_adjustments (gcc::context *ctxt)
{
  return new pass_stack_adjustments (ctxt);
}
