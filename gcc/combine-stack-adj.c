/* Combine stack adjustments.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "basic-block.h"
#include "df.h"
#include "except.h"
#include "toplev.h"
#include "reload.h"
#include "timevar.h"
#include "tree-pass.h"


/* Turn STACK_GROWS_DOWNWARD into a boolean.  */
#ifdef STACK_GROWS_DOWNWARD
#undef STACK_GROWS_DOWNWARD
#define STACK_GROWS_DOWNWARD 1
#else
#define STACK_GROWS_DOWNWARD 0
#endif

/* This structure records stack memory references between stack adjusting
   instructions.  */

struct csa_memlist
{
  HOST_WIDE_INT sp_offset;
  rtx insn, *mem;
  struct csa_memlist *next;
};

static int stack_memref_p (rtx);
static rtx single_set_for_csa (rtx);
static void free_csa_memlist (struct csa_memlist *);
static struct csa_memlist *record_one_stack_memref (rtx, rtx *,
						    struct csa_memlist *);
static int try_apply_stack_adjustment (rtx, struct csa_memlist *,
				       HOST_WIDE_INT, HOST_WIDE_INT);
static void combine_stack_adjustments_for_block (basic_block);
static int record_stack_memrefs (rtx *, void *);


/* Main entry point for stack adjustment combination.  */

static void
combine_stack_adjustments (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
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
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    return 1;

  return 0;
}

/* Recognize either normal single_set or the hack in i386.md for
   tying fp and sp adjustments.  */

static rtx
single_set_for_csa (rtx insn)
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
	       && GET_CODE (this_rtx) != USE)
	return NULL_RTX;
    }

  return XVECEXP (tmp, 0, 0);
}

/* Free the list of csa_memlist nodes.  */

static void
free_csa_memlist (struct csa_memlist *memlist)
{
  struct csa_memlist *next;
  for (; memlist ; memlist = next)
    {
      next = memlist->next;
      free (memlist);
    }
}

/* Create a new csa_memlist node from the given memory reference.
   It is already known that the memory is stack_memref_p.  */

static struct csa_memlist *
record_one_stack_memref (rtx insn, rtx *mem, struct csa_memlist *next_memlist)
{
  struct csa_memlist *ml;

  ml = XNEW (struct csa_memlist);

  if (XEXP (*mem, 0) == stack_pointer_rtx)
    ml->sp_offset = 0;
  else
    ml->sp_offset = INTVAL (XEXP (XEXP (*mem, 0), 1));

  ml->insn = insn;
  ml->mem = mem;
  ml->next = next_memlist;

  return ml;
}

/* Attempt to apply ADJUST to the stack adjusting insn INSN, as well
   as each of the memories in MEMLIST.  Return true on success.  */

static int
try_apply_stack_adjustment (rtx insn, struct csa_memlist *memlist, HOST_WIDE_INT new_adjust,
			    HOST_WIDE_INT delta)
{
  struct csa_memlist *ml;
  rtx set;

  set = single_set_for_csa (insn);
  validate_change (insn, &XEXP (SET_SRC (set), 1), GEN_INT (new_adjust), 1);

  for (ml = memlist; ml ; ml = ml->next)
    validate_change
      (ml->insn, ml->mem,
       replace_equiv_address_nv (*ml->mem,
				 plus_constant (stack_pointer_rtx,
						ml->sp_offset - delta)), 1);

  if (apply_change_group ())
    {
      /* Succeeded.  Update our knowledge of the memory references.  */
      for (ml = memlist; ml ; ml = ml->next)
	ml->sp_offset -= delta;

      return 1;
    }
  else
    return 0;
}

/* Called via for_each_rtx and used to record all stack memory references in
   the insn and discard all other stack pointer references.  */
struct record_stack_memrefs_data
{
  rtx insn;
  struct csa_memlist *memlist;
};

static int
record_stack_memrefs (rtx *xp, void *data)
{
  rtx x = *xp;
  struct record_stack_memrefs_data *d =
    (struct record_stack_memrefs_data *) data;
  if (!x)
    return 0;
  switch (GET_CODE (x))
    {
    case MEM:
      if (!reg_mentioned_p (stack_pointer_rtx, x))
	return -1;
      /* We are not able to handle correctly all possible memrefs containing
         stack pointer, so this check is necessary.  */
      if (stack_memref_p (x))
	{
	  d->memlist = record_one_stack_memref (d->insn, xp, d->memlist);
	  return -1;
	}
      return 1;
    case REG:
      /* ??? We want be able to handle non-memory stack pointer
	 references later.  For now just discard all insns referring to
	 stack pointer outside mem expressions.  We would probably
	 want to teach validate_replace to simplify expressions first.

	 We can't just compare with STACK_POINTER_RTX because the
	 reference to the stack pointer might be in some other mode.
	 In particular, an explicit clobber in an asm statement will
	 result in a QImode clobber.  */
      if (REGNO (x) == STACK_POINTER_REGNUM)
	return 1;
      break;
    default:
      break;
    }
  return 0;
}

/* Adjust or create REG_FRAME_RELATED_EXPR note when merging a stack
   adjustment into a frame related insn.  */

static void
adjust_frame_related_expr (rtx last_sp_set, rtx insn,
			   HOST_WIDE_INT this_adjust)
{
  rtx note = find_reg_note (last_sp_set, REG_FRAME_RELATED_EXPR, NULL_RTX);
  rtx new_expr = NULL_RTX;

  if (note == NULL_RTX && RTX_FRAME_RELATED_P (insn))
    return;

  if (note
      && GET_CODE (XEXP (note, 0)) == SEQUENCE
      && XVECLEN (XEXP (note, 0), 0) >= 2)
    {
      rtx expr = XEXP (note, 0);
      rtx last = XVECEXP (expr, 0, XVECLEN (expr, 0) - 1);
      int i;

      if (GET_CODE (last) == SET
	  && RTX_FRAME_RELATED_P (last) == RTX_FRAME_RELATED_P (insn)
	  && SET_DEST (last) == stack_pointer_rtx
	  && GET_CODE (SET_SRC (last)) == PLUS
	  && XEXP (SET_SRC (last), 0) == stack_pointer_rtx
	  && GET_CODE (XEXP (SET_SRC (last), 1)) == CONST_INT)
	{
	  XEXP (SET_SRC (last), 1)
	    = GEN_INT (INTVAL (XEXP (SET_SRC (last), 1)) + this_adjust);
	  return;
	}

      new_expr = gen_rtx_SEQUENCE (VOIDmode,
				   rtvec_alloc (XVECLEN (expr, 0) + 1));
      for (i = 0; i < XVECLEN (expr, 0); i++)
	XVECEXP (new_expr, 0, i) = XVECEXP (expr, 0, i);
    }
  else
    {
      new_expr = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (2));
      if (note)
	XVECEXP (new_expr, 0, 0) = XEXP (note, 0);
      else
	{
	  rtx expr = copy_rtx (single_set_for_csa (last_sp_set));

	  XEXP (SET_SRC (expr), 1)
	    = GEN_INT (INTVAL (XEXP (SET_SRC (expr), 1)) - this_adjust);
	  RTX_FRAME_RELATED_P (expr) = 1;
	  XVECEXP (new_expr, 0, 0) = expr;
	}
    }

  XVECEXP (new_expr, 0, XVECLEN (new_expr, 0) - 1)
    = copy_rtx (single_set_for_csa (insn));
  RTX_FRAME_RELATED_P (XVECEXP (new_expr, 0, XVECLEN (new_expr, 0) - 1))
    = RTX_FRAME_RELATED_P (insn);
  if (note)
    XEXP (note, 0) = new_expr;
  else
    REG_NOTES (last_sp_set)
      = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, new_expr,
			   REG_NOTES (last_sp_set));
}

/* Subroutine of combine_stack_adjustments, called for each basic block.  */

static void
combine_stack_adjustments_for_block (basic_block bb)
{
  HOST_WIDE_INT last_sp_adjust = 0;
  rtx last_sp_set = NULL_RTX;
  struct csa_memlist *memlist = NULL;
  rtx insn, next, set;
  struct record_stack_memrefs_data data;
  bool end_of_block = false;

  for (insn = BB_HEAD (bb); !end_of_block ; insn = next)
    {
      end_of_block = insn == BB_END (bb);
      next = NEXT_INSN (insn);

      if (! INSN_P (insn))
	continue;

      set = single_set_for_csa (insn);
      if (set)
	{
	  rtx dest = SET_DEST (set);
	  rtx src = SET_SRC (set);

	  /* Find constant additions to the stack pointer.  */
	  if (dest == stack_pointer_rtx
	      && GET_CODE (src) == PLUS
	      && XEXP (src, 0) == stack_pointer_rtx
	      && GET_CODE (XEXP (src, 1)) == CONST_INT)
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

	      /* If not all recorded memrefs can be adjusted, or the
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
		  if (try_apply_stack_adjustment (last_sp_set, memlist,
						  last_sp_adjust + this_adjust,
						  this_adjust))
		    {
		      if (RTX_FRAME_RELATED_P (last_sp_set))
			adjust_frame_related_expr (last_sp_set, insn,
						   this_adjust);
		      /* It worked!  */
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
		  if (try_apply_stack_adjustment (insn, memlist,
						  last_sp_adjust + this_adjust,
						  -last_sp_adjust))
		    {
		      /* It worked!  */
		      delete_insn (last_sp_set);
		      last_sp_set = insn;
		      last_sp_adjust += this_adjust;
		      free_csa_memlist (memlist);
		      memlist = NULL;
		      continue;
		    }
		}

	      /* Combination failed.  Restart processing from here.  If
		 deallocation+allocation conspired to cancel, we can
		 delete the old deallocation insn.  */
	      if (last_sp_set && last_sp_adjust == 0)
		delete_insn (last_sp_set);
	      free_csa_memlist (memlist);
	      memlist = NULL;
	      last_sp_set = insn;
	      last_sp_adjust = this_adjust;
	      continue;
	    }

	  /* Find a predecrement of exactly the previous adjustment and
	     turn it into a direct store.  Obviously we can't do this if
	     there were any intervening uses of the stack pointer.  */
	  if (memlist == NULL
	      && MEM_P (dest)
	      && ((GET_CODE (XEXP (dest, 0)) == PRE_DEC
		   && (last_sp_adjust
		       == (HOST_WIDE_INT) GET_MODE_SIZE (GET_MODE (dest))))
		  || (GET_CODE (XEXP (dest, 0)) == PRE_MODIFY
		      && GET_CODE (XEXP (XEXP (dest, 0), 1)) == PLUS
		      && XEXP (XEXP (XEXP (dest, 0), 1), 0) == stack_pointer_rtx
		      && (GET_CODE (XEXP (XEXP (XEXP (dest, 0), 1), 1))
		          == CONST_INT)
		      && (INTVAL (XEXP (XEXP (XEXP (dest, 0), 1), 1))
		          == -last_sp_adjust)))
	      && XEXP (XEXP (dest, 0), 0) == stack_pointer_rtx
	      && ! reg_mentioned_p (stack_pointer_rtx, src)
	      && memory_address_p (GET_MODE (dest), stack_pointer_rtx)
	      && validate_change (insn, &SET_DEST (set),
				  replace_equiv_address (dest,
							 stack_pointer_rtx),
				  0))
	    {
	      delete_insn (last_sp_set);
	      free_csa_memlist (memlist);
	      memlist = NULL;
	      last_sp_set = NULL_RTX;
	      last_sp_adjust = 0;
	      continue;
	    }
	}

      data.insn = insn;
      data.memlist = memlist;
      if (!CALL_P (insn) && last_sp_set
	  && !for_each_rtx (&PATTERN (insn), record_stack_memrefs, &data))
	{
	   memlist = data.memlist;
	   continue;
	}
      memlist = data.memlist;

      /* Otherwise, we were not able to process the instruction.
	 Do not continue collecting data across such a one.  */
      if (last_sp_set
	  && (CALL_P (insn)
	      || reg_mentioned_p (stack_pointer_rtx, PATTERN (insn))))
	{
	  if (last_sp_set && last_sp_adjust == 0)
	    delete_insn (last_sp_set);
	  free_csa_memlist (memlist);
	  memlist = NULL;
	  last_sp_set = NULL_RTX;
	  last_sp_adjust = 0;
	}
    }

  if (last_sp_set && last_sp_adjust == 0)
    delete_insn (last_sp_set);

  if (memlist)
    free_csa_memlist (memlist);
}


static bool
gate_handle_stack_adjustments (void)
{
  return (optimize > 0);
}

static unsigned int
rest_of_handle_stack_adjustments (void)
{
  cleanup_cfg (flag_crossjumping ? CLEANUP_CROSSJUMP : 0);

  /* This is kind of a heuristic.  We need to run combine_stack_adjustments
     even for machines with possibly nonzero RETURN_POPS_ARGS
     and ACCUMULATE_OUTGOING_ARGS.  We expect that only ports having
     push instructions will have popping returns.  */
#ifndef PUSH_ROUNDING
  if (!ACCUMULATE_OUTGOING_ARGS)
#endif
    {
      df_note_add_problem ();
      df_analyze ();
      combine_stack_adjustments ();
    }
  return 0;
}

struct rtl_opt_pass pass_stack_adjustments =
{
 {
  RTL_PASS,
  "csa",                                /* name */
  gate_handle_stack_adjustments,        /* gate */
  rest_of_handle_stack_adjustments,     /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  0,                                    /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_dump_func |
  TODO_ggc_collect,                     /* todo_flags_finish */
 }
};

