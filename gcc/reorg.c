/* Perform instruction reorganizations for delay slot filling.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu).
   Hacked by Michael Tiemann (tiemann@cygnus.com).

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

/* Instruction reorganization pass.

   This pass runs after register allocation and final jump
   optimization.  It should be the last pass to run before peephole.
   It serves primarily to fill delay slots of insns, typically branch
   and call insns.  Other insns typically involve more complicated
   interactions of data dependencies and resource constraints, and
   are better handled by scheduling before register allocation (by the
   function `schedule_insns').

   The Branch Penalty is the number of extra cycles that are needed to
   execute a branch insn.  On an ideal machine, branches take a single
   cycle, and the Branch Penalty is 0.  Several RISC machines approach
   branch delays differently:

   The MIPS and AMD 29000 have a single branch delay slot.  Most insns
   (except other branches) can be used to fill this slot.  When the
   slot is filled, two insns execute in two cycles, reducing the
   branch penalty to zero.

   The Motorola 88000 conditionally exposes its branch delay slot,
   so code is shorter when it is turned off, but will run faster
   when useful insns are scheduled there.

   The IBM ROMP has two forms of branch and call insns, both with and
   without a delay slot.  Much like the 88k, insns not using the delay
   slot can be shorted (2 bytes vs. 4 bytes), but will run slowed.

   The SPARC always has a branch delay slot, but its effects can be
   annulled when the branch is not taken.  This means that failing to
   find other sources of insns, we can hoist an insn from the branch
   target that would only be safe to execute knowing that the branch
   is taken.

   The HP-PA always has a branch delay slot.  For unconditional branches
   its effects can be annulled when the branch is taken.  The effects 
   of the delay slot in a conditional branch can be nullified for forward
   taken branches, or for untaken backward branches.  This means
   we can hoist insns from the fall-through path for forward branches or
   steal insns from the target of backward branches.

   Three techniques for filling delay slots have been implemented so far:

   (1) `fill_simple_delay_slots' is the simplest, most efficient way
   to fill delay slots.  This pass first looks for insns which come
   from before the branch and which are safe to execute after the
   branch.  Then it searches after the insn requiring delay slots or,
   in the case of a branch, for insns that are after the point at
   which the branch merges into the fallthrough code, if such a point
   exists.  When such insns are found, the branch penalty decreases
   and no code expansion takes place.

   (2) `fill_eager_delay_slots' is more complicated: it is used for
   scheduling conditional jumps, or for scheduling jumps which cannot
   be filled using (1).  A machine need not have annulled jumps to use
   this strategy, but it helps (by keeping more options open).
   `fill_eager_delay_slots' tries to guess the direction the branch
   will go; if it guesses right 100% of the time, it can reduce the
   branch penalty as much as `fill_simple_delay_slots' does.  If it
   guesses wrong 100% of the time, it might as well schedule nops (or
   on the m88k, unexpose the branch slot).  When
   `fill_eager_delay_slots' takes insns from the fall-through path of
   the jump, usually there is no code expansion; when it takes insns
   from the branch target, there is code expansion if it is not the
   only way to reach that target.

   (3) `relax_delay_slots' uses a set of rules to simplify code that
   has been reorganized by (1) and (2).  It finds cases where
   conditional test can be eliminated, jumps can be threaded, extra
   insns can be eliminated, etc.  It is the job of (1) and (2) to do a
   good job of scheduling locally; `relax_delay_slots' takes care of
   making the various individual schedules work well together.  It is
   especially tuned to handle the control flow interactions of branch
   insns.  It does nothing for insns with delay slots that do not
   branch.

   On machines that use CC0, we are very conservative.  We will not make
   a copy of an insn involving CC0 since we want to maintain a 1-1
   correspondence between the insn that sets and uses CC0.  The insns are
   allowed to be separated by placing an insn that sets CC0 (but not an insn
   that uses CC0; we could do this, but it doesn't seem worthwhile) in a
   delay slot.  In that case, we point each insn at the other with REG_CC_USER
   and REG_CC_SETTER notes.  Note that these restrictions affect very few
   machines because most RISC machines with delay slots will not use CC0
   (the RT is the only known exception at this point).

   Not yet implemented:

   The Acorn Risc Machine can conditionally execute most insns, so
   it is profitable to move single insns into a position to execute
   based on the condition code of the previous insn.

   The HP-PA can conditionally nullify insns, providing a similar
   effect to the ARM, differing mostly in which insn is "in charge".   */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "insn-config.h"
#include "conditions.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "regs.h"
#include "insn-flags.h"
#include "recog.h"
#include "flags.h"
#include "output.h"
#include "obstack.h"
#include "insn-attr.h"

#ifdef DELAY_SLOTS

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

#ifndef ANNUL_IFTRUE_SLOTS
#define eligible_for_annul_true(INSN, SLOTS, TRIAL, FLAGS) 0
#endif
#ifndef ANNUL_IFFALSE_SLOTS
#define eligible_for_annul_false(INSN, SLOTS, TRIAL, FLAGS) 0
#endif

/* Insns which have delay slots that have not yet been filled.  */

static struct obstack unfilled_slots_obstack;
static rtx *unfilled_firstobj;

/* Define macros to refer to the first and last slot containing unfilled
   insns.  These are used because the list may move and its address
   should be recomputed at each use.  */

#define unfilled_slots_base	\
  ((rtx *) obstack_base (&unfilled_slots_obstack))

#define unfilled_slots_next	\
  ((rtx *) obstack_next_free (&unfilled_slots_obstack))

/* This structure is used to indicate which hardware resources are set or
   needed by insns so far.  */

struct resources
{
  char memory;			/* Insn sets or needs a memory location.  */
  char unch_memory;		/* Insn sets of needs a "unchanging" MEM. */
  char volatil;			/* Insn sets or needs a volatile memory loc. */
  char cc;			/* Insn sets or needs the condition codes.  */
  HARD_REG_SET regs;		/* Which registers are set or needed.  */
};

/* Macro to clear all resources.  */
#define CLEAR_RESOURCE(RES)	\
 do { (RES)->memory = (RES)->unch_memory = (RES)->volatil = (RES)->cc = 0; \
      CLEAR_HARD_REG_SET ((RES)->regs); } while (0)

/* Indicates what resources are required at the beginning of the epilogue.  */
static struct resources start_of_epilogue_needs;

/* Indicates what resources are required at function end.  */
static struct resources end_of_function_needs;

/* Points to the label before the end of the function.  */
static rtx end_of_function_label;

/* This structure is used to record liveness information at the targets or
   fallthrough insns of branches.  We will most likely need the information
   at targets again, so save them in a hash table rather than recomputing them
   each time.  */

struct target_info
{
  int uid;			/* INSN_UID of target.  */
  struct target_info *next;	/* Next info for same hash bucket.  */
  HARD_REG_SET live_regs;	/* Registers live at target.  */
  int block;			/* Basic block number containing target.  */
  int bb_tick;			/* Generation count of basic block info.  */
};

#define TARGET_HASH_PRIME 257

/* Define the hash table itself.  */
static struct target_info **target_hash_table;

/* For each basic block, we maintain a generation number of its basic
   block info, which is updated each time we move an insn from the
   target of a jump.  This is the generation number indexed by block
   number.  */

static int *bb_ticks;

/* Mapping between INSN_UID's and position in the code since INSN_UID's do
   not always monotonically increase.  */
static int *uid_to_ruid;

/* Highest valid index in `uid_to_ruid'.  */
static int max_uid;

static void mark_referenced_resources PROTO((rtx, struct resources *, int));
static void mark_set_resources	PROTO((rtx, struct resources *, int, int));
static int stop_search_p	PROTO((rtx, int));
static int resource_conflicts_p	PROTO((struct resources *,
				       struct resources *));
static int insn_references_resource_p PROTO((rtx, struct resources *, int));
static int insn_sets_resources_p PROTO((rtx, struct resources *, int));
static rtx find_end_label	PROTO((void));
static rtx emit_delay_sequence	PROTO((rtx, rtx, int, int));
static rtx add_to_delay_list	PROTO((rtx, rtx));
static void delete_from_delay_slot PROTO((rtx));
static void delete_scheduled_jump PROTO((rtx));
static void note_delay_statistics PROTO((int, int));
static rtx optimize_skip	PROTO((rtx));
static int get_jump_flags PROTO((rtx, rtx));
static int rare_destination PROTO((rtx));
static int mostly_true_jump	PROTO((rtx, rtx));
static rtx get_branch_condition	PROTO((rtx, rtx));
static int condition_dominates_p PROTO((rtx, rtx));
static rtx steal_delay_list_from_target PROTO((rtx, rtx, rtx, rtx,
					       struct resources *,
					       struct resources *,
					       struct resources *,
					       int, int *, int *, rtx *));
static rtx steal_delay_list_from_fallthrough PROTO((rtx, rtx, rtx, rtx,
						    struct resources *,
						    struct resources *,
						    struct resources *,
						    int, int *, int *));
static void try_merge_delay_insns PROTO((rtx, rtx));
static rtx redundant_insn	PROTO((rtx, rtx, rtx));
static int own_thread_p		PROTO((rtx, rtx, int));
static int find_basic_block	PROTO((rtx));
static void update_block	PROTO((rtx, rtx));
static int reorg_redirect_jump PROTO((rtx, rtx));
static void update_reg_dead_notes PROTO((rtx, rtx));
static void update_reg_unused_notes PROTO((rtx, rtx));
static void update_live_status	PROTO((rtx, rtx));
static rtx next_insn_no_annul	PROTO((rtx));
static void mark_target_live_regs PROTO((rtx, struct resources *));
static void fill_simple_delay_slots PROTO((rtx, int));
static rtx fill_slots_from_thread PROTO((rtx, rtx, rtx, rtx, int, int,
					 int, int, int, int *));
static void fill_eager_delay_slots PROTO((rtx));
static void relax_delay_slots	PROTO((rtx));
static void make_return_insns	PROTO((rtx));
static int redirect_with_delay_slots_safe_p PROTO ((rtx, rtx, rtx));
static int redirect_with_delay_list_safe_p PROTO ((rtx, rtx, rtx));

/* Given X, some rtl, and RES, a pointer to a `struct resource', mark
   which resources are references by the insn.  If INCLUDE_CALLED_ROUTINE
   is TRUE, resources used by the called routine will be included for
   CALL_INSNs.  */

static void
mark_referenced_resources (x, res, include_delayed_effects)
     register rtx x;
     register struct resources *res;
     register int include_delayed_effects;
{
  register enum rtx_code code = GET_CODE (x);
  register int i, j;
  register char *format_ptr;

  /* Handle leaf items for which we set resource flags.  Also, special-case
     CALL, SET and CLOBBER operators.  */
  switch (code)
    {
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case PC:
    case SYMBOL_REF:
    case LABEL_REF:
      return;

    case SUBREG:
      if (GET_CODE (SUBREG_REG (x)) != REG)
	mark_referenced_resources (SUBREG_REG (x), res, 0);
      else
	{
	  int regno = REGNO (SUBREG_REG (x)) + SUBREG_WORD (x);
	  int last_regno = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));
	  for (i = regno; i < last_regno; i++)
	    SET_HARD_REG_BIT (res->regs, i);
	}
      return;

    case REG:
      for (i = 0; i < HARD_REGNO_NREGS (REGNO (x), GET_MODE (x)); i++)
	SET_HARD_REG_BIT (res->regs, REGNO (x) + i);
      return;

    case MEM:
      /* If this memory shouldn't change, it really isn't referencing
	 memory.  */
      if (RTX_UNCHANGING_P (x))
	res->unch_memory = 1;
      else
	res->memory = 1;
      res->volatil = MEM_VOLATILE_P (x);

      /* Mark registers used to access memory.  */
      mark_referenced_resources (XEXP (x, 0), res, 0);
      return;

    case CC0:
      res->cc = 1;
      return;

    case UNSPEC_VOLATILE:
    case ASM_INPUT:
      /* Traditional asm's are always volatile.  */
      res->volatil = 1;
      return;

    case ASM_OPERANDS:
      res->volatil = MEM_VOLATILE_P (x);

      /* For all ASM_OPERANDS, we must traverse the vector of input operands.
	 We can not just fall through here since then we would be confused
	 by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	 traditional asms unlike their normal usage.  */
      
      for (i = 0; i < ASM_OPERANDS_INPUT_LENGTH (x); i++)
	mark_referenced_resources (ASM_OPERANDS_INPUT (x, i), res, 0);
      return;

    case CALL:
      /* The first operand will be a (MEM (xxx)) but doesn't really reference
	 memory.  The second operand may be referenced, though.  */
      mark_referenced_resources (XEXP (XEXP (x, 0), 0), res, 0);
      mark_referenced_resources (XEXP (x, 1), res, 0);
      return;

    case SET:
      /* Usually, the first operand of SET is set, not referenced.  But
	 registers used to access memory are referenced.  SET_DEST is
	 also referenced if it is a ZERO_EXTRACT or SIGN_EXTRACT.  */

      mark_referenced_resources (SET_SRC (x), res, 0);

      x = SET_DEST (x);
      if (GET_CODE (x) == SIGN_EXTRACT || GET_CODE (x) == ZERO_EXTRACT)
	mark_referenced_resources (x, res, 0);
      else if (GET_CODE (x) == SUBREG)
	x = SUBREG_REG (x);
      if (GET_CODE (x) == MEM)
	mark_referenced_resources (XEXP (x, 0), res, 0);
      return;

    case CLOBBER:
      return;

    case CALL_INSN:
      if (include_delayed_effects)
	{
	  /* A CALL references memory, the frame pointer if it exists, the
	     stack pointer, any global registers and any registers given in
	     USE insns immediately in front of the CALL.

	     However, we may have moved some of the parameter loading insns
	     into the delay slot of this CALL.  If so, the USE's for them
	     don't count and should be skipped.  */
	  rtx insn = PREV_INSN (x);
	  rtx sequence = 0;
	  int seq_size = 0;
	  rtx next = NEXT_INSN (x);
	  int i;

	  /* If we are part of a delay slot sequence, point at the SEQUENCE. */
	  if (NEXT_INSN (insn) != x)
	    {
	      next = NEXT_INSN (NEXT_INSN (insn));
	      sequence = PATTERN (NEXT_INSN (insn));
	      seq_size = XVECLEN (sequence, 0);
	      if (GET_CODE (sequence) != SEQUENCE)
		abort ();
	    }

	  res->memory = 1;
	  SET_HARD_REG_BIT (res->regs, STACK_POINTER_REGNUM);
	  if (frame_pointer_needed)
	    {
	      SET_HARD_REG_BIT (res->regs, FRAME_POINTER_REGNUM);
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
	      SET_HARD_REG_BIT (res->regs, HARD_FRAME_POINTER_REGNUM);
#endif
	    }

	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (global_regs[i])
	      SET_HARD_REG_BIT (res->regs, i);

	  /* Check for a NOTE_INSN_SETJMP.  If it exists, then we must
	     assume that this call can need any register.

	     This is done to be more conservative about how we handle setjmp.
	     We assume that they both use and set all registers.  Using all
	     registers ensures that a register will not be considered dead
	     just because it crosses a setjmp call.  A register should be
	     considered dead only if the setjmp call returns non-zero.  */
	  if (next && GET_CODE (next) == NOTE
	      && NOTE_LINE_NUMBER (next) == NOTE_INSN_SETJMP)
	    SET_HARD_REG_SET (res->regs);

	  {
	    rtx link;

	    for (link = CALL_INSN_FUNCTION_USAGE (x);
		 link;
		 link = XEXP (link, 1))
	      if (GET_CODE (XEXP (link, 0)) == USE)
		{
		  for (i = 1; i < seq_size; i++)
		    {
		      rtx slot_pat = PATTERN (XVECEXP (sequence, 0, i));
		      if (GET_CODE (slot_pat) == SET
			  && rtx_equal_p (SET_DEST (slot_pat),
					  SET_DEST (XEXP (link, 0))))
			break;
		    }
		  if (i >= seq_size)
		    mark_referenced_resources (SET_DEST (XEXP (link, 0)),
					       res, 0);
		}
	  }
	}

      /* ... fall through to other INSN processing ... */

    case INSN:
    case JUMP_INSN:

#ifdef INSN_REFERENCES_ARE_DELAYED
      if (! include_delayed_effects
	  && INSN_REFERENCES_ARE_DELAYED (x))
	return;
#endif

      /* No special processing, just speed up.  */
      mark_referenced_resources (PATTERN (x), res, include_delayed_effects);
      return;
    }

  /* Process each sub-expression and flag what it needs.  */
  format_ptr = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    switch (*format_ptr++)
      {
      case 'e':
	mark_referenced_resources (XEXP (x, i), res, include_delayed_effects);
	break;

      case 'E':
	for (j = 0; j < XVECLEN (x, i); j++)
	  mark_referenced_resources (XVECEXP (x, i, j), res,
				     include_delayed_effects);
	break;
      }
}

/* Given X, a part of an insn, and a pointer to a `struct resource', RES,
   indicate which resources are modified by the insn. If INCLUDE_CALLED_ROUTINE
   is nonzero, also mark resources potentially set by the called routine.

   If IN_DEST is nonzero, it means we are inside a SET.  Otherwise,
   objects are being referenced instead of set.

   We never mark the insn as modifying the condition code unless it explicitly
   SETs CC0 even though this is not totally correct.  The reason for this is
   that we require a SET of CC0 to immediately precede the reference to CC0.
   So if some other insn sets CC0 as a side-effect, we know it cannot affect
   our computation and thus may be placed in a delay slot.   */

static void
mark_set_resources (x, res, in_dest, include_delayed_effects)
     register rtx x;
     register struct resources *res;
     int in_dest;
     int include_delayed_effects;
{
  register enum rtx_code code;
  register int i, j;
  register char *format_ptr;

 restart:

  code = GET_CODE (x);

  switch (code)
    {
    case NOTE:
    case BARRIER:
    case CODE_LABEL:
    case USE:
    case CONST_INT:
    case CONST_DOUBLE:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
    case PC:
      /* These don't set any resources.  */
      return;

    case CC0:
      if (in_dest)
	res->cc = 1;
      return;

    case CALL_INSN:
      /* Called routine modifies the condition code, memory, any registers
	 that aren't saved across calls, global registers and anything
	 explicitly CLOBBERed immediately after the CALL_INSN.  */

      if (include_delayed_effects)
	{
	  rtx next = NEXT_INSN (x);
	  rtx prev = PREV_INSN (x);
	  rtx link;

	  res->cc = res->memory = 1;
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (call_used_regs[i] || global_regs[i])
	      SET_HARD_REG_BIT (res->regs, i);

	  /* If X is part of a delay slot sequence, then NEXT should be
	     the first insn after the sequence.  */
	  if (NEXT_INSN (prev) != x)
	    next = NEXT_INSN (NEXT_INSN (prev));

	  for (link = CALL_INSN_FUNCTION_USAGE (x);
	       link; link = XEXP (link, 1))
	    if (GET_CODE (XEXP (link, 0)) == CLOBBER)
	      mark_set_resources (SET_DEST (XEXP (link, 0)), res, 1, 0);

	  /* Check for a NOTE_INSN_SETJMP.  If it exists, then we must
	     assume that this call can clobber any register.  */
	  if (next && GET_CODE (next) == NOTE
	      && NOTE_LINE_NUMBER (next) == NOTE_INSN_SETJMP)
	    SET_HARD_REG_SET (res->regs);
	}

      /* ... and also what it's RTL says it modifies, if anything.  */

    case JUMP_INSN:
    case INSN:

	/* An insn consisting of just a CLOBBER (or USE) is just for flow
	   and doesn't actually do anything, so we ignore it.  */

#ifdef INSN_SETS_ARE_DELAYED
      if (! include_delayed_effects
	  && INSN_SETS_ARE_DELAYED (x))
	return;
#endif

      x = PATTERN (x);
      if (GET_CODE (x) != USE && GET_CODE (x) != CLOBBER)
	goto restart;
      return;

    case SET:
      /* If the source of a SET is a CALL, this is actually done by
	 the called routine.  So only include it if we are to include the
	 effects of the calling routine.  */

      mark_set_resources (SET_DEST (x), res,
			  (include_delayed_effects
			   || GET_CODE (SET_SRC (x)) != CALL),
			  0);

      mark_set_resources (SET_SRC (x), res, 0, 0);
      return;

    case CLOBBER:
      mark_set_resources (XEXP (x, 0), res, 1, 0);
      return;
      
    case SEQUENCE:
      for (i = 0; i < XVECLEN (x, 0); i++)
	if (! (INSN_ANNULLED_BRANCH_P (XVECEXP (x, 0, 0))
	       && INSN_FROM_TARGET_P (XVECEXP (x, 0, i))))
	  mark_set_resources (XVECEXP (x, 0, i), res, 0,
			      include_delayed_effects);
      return;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
      mark_set_resources (XEXP (x, 0), res, 1, 0);
      return;

    case ZERO_EXTRACT:
      mark_set_resources (XEXP (x, 0), res, in_dest, 0);
      mark_set_resources (XEXP (x, 1), res, 0, 0);
      mark_set_resources (XEXP (x, 2), res, 0, 0);
      return;

    case MEM:
      if (in_dest)
	{
	  res->memory = 1;
	  res->unch_memory = RTX_UNCHANGING_P (x);
	  res->volatil = MEM_VOLATILE_P (x);
	}

      mark_set_resources (XEXP (x, 0), res, 0, 0);
      return;

    case SUBREG:
      if (in_dest)
	{
	  if (GET_CODE (SUBREG_REG (x)) != REG)
	    mark_set_resources (SUBREG_REG (x), res,
				in_dest, include_delayed_effects);
	  else
	    {
	      int regno = REGNO (SUBREG_REG (x)) + SUBREG_WORD (x);
	      int last_regno = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));
	      for (i = regno; i < last_regno; i++)
		SET_HARD_REG_BIT (res->regs, i);
	    }
	}
      return;

    case REG:
      if (in_dest)
        for (i = 0; i < HARD_REGNO_NREGS (REGNO (x), GET_MODE (x)); i++)
	  SET_HARD_REG_BIT (res->regs, REGNO (x) + i);
      return;
    }

  /* Process each sub-expression and flag what it needs.  */
  format_ptr = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    switch (*format_ptr++)
      {
      case 'e':
	mark_set_resources (XEXP (x, i), res, in_dest, include_delayed_effects);
	break;

      case 'E':
	for (j = 0; j < XVECLEN (x, i); j++)
	  mark_set_resources (XVECEXP (x, i, j), res, in_dest,
			      include_delayed_effects);
	break;
      }
}

/* Return TRUE if this insn should stop the search for insn to fill delay
   slots.  LABELS_P indicates that labels should terminate the search.
   In all cases, jumps terminate the search.  */

static int
stop_search_p (insn, labels_p)
     rtx insn;
     int labels_p;
{
  if (insn == 0)
    return 1;

  switch (GET_CODE (insn))
    {
    case NOTE:
    case CALL_INSN:
      return 0;

    case CODE_LABEL:
      return labels_p;

    case JUMP_INSN:
    case BARRIER:
      return 1;

    case INSN:
      /* OK unless it contains a delay slot or is an `asm' insn of some type.
	 We don't know anything about these.  */
      return (GET_CODE (PATTERN (insn)) == SEQUENCE
	      || GET_CODE (PATTERN (insn)) == ASM_INPUT
	      || asm_noperands (PATTERN (insn)) >= 0);

    default:
      abort ();
    }
}

/* Return TRUE if any resources are marked in both RES1 and RES2 or if either
   resource set contains a volatile memory reference.  Otherwise, return FALSE.  */

static int
resource_conflicts_p (res1, res2)
     struct resources *res1, *res2;
{
  if ((res1->cc && res2->cc) || (res1->memory && res2->memory)
      || (res1->unch_memory && res2->unch_memory)
      || res1->volatil || res2->volatil)
    return 1;

#ifdef HARD_REG_SET
  return (res1->regs & res2->regs) != HARD_CONST (0);
#else
  {
    int i;

    for (i = 0; i < HARD_REG_SET_LONGS; i++)
      if ((res1->regs[i] & res2->regs[i]) != 0)
	return 1;
    return 0;
  }
#endif
}

/* Return TRUE if any resource marked in RES, a `struct resources', is
   referenced by INSN.  If INCLUDE_CALLED_ROUTINE is set, return if the called
   routine is using those resources.

   We compute this by computing all the resources referenced by INSN and
   seeing if this conflicts with RES.  It might be faster to directly check
   ourselves, and this is the way it used to work, but it means duplicating
   a large block of complex code.  */

static int
insn_references_resource_p (insn, res, include_delayed_effects)
     register rtx insn;
     register struct resources *res;
     int include_delayed_effects;
{
  struct resources insn_res;

  CLEAR_RESOURCE (&insn_res);
  mark_referenced_resources (insn, &insn_res, include_delayed_effects);
  return resource_conflicts_p (&insn_res, res);
}

/* Return TRUE if INSN modifies resources that are marked in RES.
   INCLUDE_CALLED_ROUTINE is set if the actions of that routine should be
   included.   CC0 is only modified if it is explicitly set; see comments
   in front of mark_set_resources for details.  */

static int
insn_sets_resource_p (insn, res, include_delayed_effects)
     register rtx insn;
     register struct resources *res;
     int include_delayed_effects;
{
  struct resources insn_sets;

  CLEAR_RESOURCE (&insn_sets);
  mark_set_resources (insn, &insn_sets, 0, include_delayed_effects);
  return resource_conflicts_p (&insn_sets, res);
}

/* Find a label at the end of the function or before a RETURN.  If there is
   none, make one.  */

static rtx
find_end_label ()
{
  rtx insn;

  /* If we found one previously, return it.  */
  if (end_of_function_label)
    return end_of_function_label;

  /* Otherwise, see if there is a label at the end of the function.  If there
     is, it must be that RETURN insns aren't needed, so that is our return
     label and we don't have to do anything else.  */

  insn = get_last_insn ();
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)))
    insn = PREV_INSN (insn);

  /* When a target threads its epilogue we might already have a 
     suitable return insn.  If so put a label before it for the
     end_of_function_label.  */
  if (GET_CODE (insn) == BARRIER
      && GET_CODE (PREV_INSN (insn)) == JUMP_INSN
      && GET_CODE (PATTERN (PREV_INSN (insn))) == RETURN)
    {
      rtx temp = PREV_INSN (PREV_INSN (insn));
      end_of_function_label = gen_label_rtx ();
      LABEL_NUSES (end_of_function_label) = 0;

      /* Put the label before an USE insns that may proceed the RETURN insn. */
      while (GET_CODE (temp) == USE)
	temp = PREV_INSN (temp);

      emit_label_after (end_of_function_label, temp);
    }

  else if (GET_CODE (insn) == CODE_LABEL)
    end_of_function_label = insn;
  else
    {
      /* Otherwise, make a new label and emit a RETURN and BARRIER,
	 if needed.  */
      end_of_function_label = gen_label_rtx ();
      LABEL_NUSES (end_of_function_label) = 0;
      emit_label (end_of_function_label);
#ifdef HAVE_return
      if (HAVE_return)
	{
	  /* The return we make may have delay slots too.  */
	  rtx insn = gen_return ();
	  insn = emit_jump_insn (insn);
	  emit_barrier ();
          if (num_delay_slots (insn) > 0)
	    obstack_ptr_grow (&unfilled_slots_obstack, insn);
	}
#endif
    }

  /* Show one additional use for this label so it won't go away until
     we are done.  */
  ++LABEL_NUSES (end_of_function_label);

  return end_of_function_label;
}

/* Put INSN and LIST together in a SEQUENCE rtx of LENGTH, and replace
   the pattern of INSN with the SEQUENCE.

   Chain the insns so that NEXT_INSN of each insn in the sequence points to
   the next and NEXT_INSN of the last insn in the sequence points to
   the first insn after the sequence.  Similarly for PREV_INSN.  This makes
   it easier to scan all insns.

   Returns the SEQUENCE that replaces INSN.  */

static rtx
emit_delay_sequence (insn, list, length, avail)
     rtx insn;
     rtx list;
     int length;
     int avail;
{
  register int i = 1;
  register rtx li;
  int had_barrier = 0;

  /* Allocate the the rtvec to hold the insns and the SEQUENCE. */
  rtvec seqv = rtvec_alloc (length + 1);
  rtx seq = gen_rtx (SEQUENCE, VOIDmode, seqv);
  rtx seq_insn = make_insn_raw (seq);
  rtx first = get_insns ();
  rtx last = get_last_insn ();

  /* Make a copy of the insn having delay slots. */
  rtx delay_insn = copy_rtx (insn);

  /* If INSN is followed by a BARRIER, delete the BARRIER since it will only
     confuse further processing.  Update LAST in case it was the last insn.  
     We will put the BARRIER back in later.  */
  if (NEXT_INSN (insn) && GET_CODE (NEXT_INSN (insn)) == BARRIER)
    {
      delete_insn (NEXT_INSN (insn));
      last = get_last_insn ();
      had_barrier = 1;
    }

  /* Splice our SEQUENCE into the insn stream where INSN used to be.  */
  NEXT_INSN (seq_insn) = NEXT_INSN (insn);
  PREV_INSN (seq_insn) = PREV_INSN (insn);

  if (insn == last)
    set_new_first_and_last_insn (first, seq_insn);
  else
    PREV_INSN (NEXT_INSN (seq_insn)) = seq_insn;

  if (insn == first)
    set_new_first_and_last_insn (seq_insn, last);
  else
    NEXT_INSN (PREV_INSN (seq_insn)) = seq_insn;

  /* Build our SEQUENCE and rebuild the insn chain.  */
  XVECEXP (seq, 0, 0) = delay_insn;
  INSN_DELETED_P (delay_insn) = 0;
  PREV_INSN (delay_insn) = PREV_INSN (seq_insn);

  for (li = list; li; li = XEXP (li, 1), i++)
    {
      rtx tem = XEXP (li, 0);
      rtx note;

      /* Show that this copy of the insn isn't deleted.  */
      INSN_DELETED_P (tem) = 0;

      XVECEXP (seq, 0, i) = tem;
      PREV_INSN (tem) = XVECEXP (seq, 0, i - 1);
      NEXT_INSN (XVECEXP (seq, 0, i - 1)) = tem;

      /* Remove any REG_DEAD notes because we can't rely on them now
	 that the insn has been moved.  */
      for (note = REG_NOTES (tem); note; note = XEXP (note, 1))
	if (REG_NOTE_KIND (note) == REG_DEAD)
	  XEXP (note, 0) = const0_rtx;
    }

  NEXT_INSN (XVECEXP (seq, 0, length)) = NEXT_INSN (seq_insn);

  /* If the previous insn is a SEQUENCE, update the NEXT_INSN pointer on the
     last insn in that SEQUENCE to point to us.  Similarly for the first
     insn in the following insn if it is a SEQUENCE.  */

  if (PREV_INSN (seq_insn) && GET_CODE (PREV_INSN (seq_insn)) == INSN
      && GET_CODE (PATTERN (PREV_INSN (seq_insn))) == SEQUENCE)
    NEXT_INSN (XVECEXP (PATTERN (PREV_INSN (seq_insn)), 0,
			XVECLEN (PATTERN (PREV_INSN (seq_insn)), 0) - 1))
      = seq_insn;

  if (NEXT_INSN (seq_insn) && GET_CODE (NEXT_INSN (seq_insn)) == INSN
      && GET_CODE (PATTERN (NEXT_INSN (seq_insn))) == SEQUENCE)
    PREV_INSN (XVECEXP (PATTERN (NEXT_INSN (seq_insn)), 0, 0)) = seq_insn;
    
  /* If there used to be a BARRIER, put it back.  */
  if (had_barrier)
    emit_barrier_after (seq_insn);

  if (i != length + 1)
    abort ();

  return seq_insn;
}

/* Add INSN to DELAY_LIST and return the head of the new list.  The list must
   be in the order in which the insns are to be executed.  */

static rtx
add_to_delay_list (insn, delay_list)
     rtx insn;
     rtx delay_list;
{
  /* If we have an empty list, just make a new list element.  If
     INSN has it's block number recorded, clear it since we may
     be moving the insn to a new block.  */

  if (delay_list == 0)
    {
      struct target_info *tinfo;
      
      for (tinfo = target_hash_table[INSN_UID (insn) % TARGET_HASH_PRIME];
	   tinfo; tinfo = tinfo->next)
	if (tinfo->uid == INSN_UID (insn))
	  break;

      if (tinfo)
	tinfo->block = -1;

      return gen_rtx (INSN_LIST, VOIDmode, insn, NULL_RTX);
    }

  /* Otherwise this must be an INSN_LIST.  Add INSN to the end of the
     list.  */
  XEXP (delay_list, 1) = add_to_delay_list (insn, XEXP (delay_list, 1));

  return delay_list;
}   

/* Delete INSN from the the delay slot of the insn that it is in.  This may
   produce an insn without anything in its delay slots.  */

static void
delete_from_delay_slot (insn)
     rtx insn;
{
  rtx trial, seq_insn, seq, prev;
  rtx delay_list = 0;
  int i;

  /* We first must find the insn containing the SEQUENCE with INSN in its
     delay slot.  Do this by finding an insn, TRIAL, where
     PREV_INSN (NEXT_INSN (TRIAL)) != TRIAL.  */

  for (trial = insn;
       PREV_INSN (NEXT_INSN (trial)) == trial;
       trial = NEXT_INSN (trial))
    ;

  seq_insn = PREV_INSN (NEXT_INSN (trial));
  seq = PATTERN (seq_insn);

  /* Create a delay list consisting of all the insns other than the one
     we are deleting (unless we were the only one).  */
  if (XVECLEN (seq, 0) > 2)
    for (i = 1; i < XVECLEN (seq, 0); i++)
      if (XVECEXP (seq, 0, i) != insn)
	delay_list = add_to_delay_list (XVECEXP (seq, 0, i), delay_list);

  /* Delete the old SEQUENCE, re-emit the insn that used to have the delay
     list, and rebuild the delay list if non-empty.  */
  prev = PREV_INSN (seq_insn);
  trial = XVECEXP (seq, 0, 0);
  delete_insn (seq_insn);
  add_insn_after (trial, prev);

  if (GET_CODE (trial) == JUMP_INSN
      && (simplejump_p (trial) || GET_CODE (PATTERN (trial)) == RETURN))
    emit_barrier_after (trial);

  /* If there are any delay insns, remit them.  Otherwise clear the
     annul flag.  */
  if (delay_list)
    trial = emit_delay_sequence (trial, delay_list, XVECLEN (seq, 0) - 2, 0);
  else
    INSN_ANNULLED_BRANCH_P (trial) = 0;

  INSN_FROM_TARGET_P (insn) = 0;

  /* Show we need to fill this insn again.  */
  obstack_ptr_grow (&unfilled_slots_obstack, trial);
}

/* Delete INSN, a JUMP_INSN.  If it is a conditional jump, we must track down
   the insn that sets CC0 for it and delete it too.  */

static void
delete_scheduled_jump (insn)
     rtx insn;
{
  /* Delete the insn that sets cc0 for us.  On machines without cc0, we could
     delete the insn that sets the condition code, but it is hard to find it.
     Since this case is rare anyway, don't bother trying; there would likely
     be other insns that became dead anyway, which we wouldn't know to
     delete.  */

#ifdef HAVE_cc0
  if (reg_mentioned_p (cc0_rtx, insn))
    {
      rtx note = find_reg_note (insn, REG_CC_SETTER, NULL_RTX);

      /* If a reg-note was found, it points to an insn to set CC0.  This
	 insn is in the delay list of some other insn.  So delete it from
	 the delay list it was in.  */
      if (note)
	{
	  if (! FIND_REG_INC_NOTE (XEXP (note, 0), NULL_RTX)
	      && sets_cc0_p (PATTERN (XEXP (note, 0))) == 1)
	    delete_from_delay_slot (XEXP (note, 0));
	}
      else
	{
	  /* The insn setting CC0 is our previous insn, but it may be in
	     a delay slot.  It will be the last insn in the delay slot, if
	     it is.  */
	  rtx trial = previous_insn (insn);
	  if (GET_CODE (trial) == NOTE)
	    trial = prev_nonnote_insn (trial);
	  if (sets_cc0_p (PATTERN (trial)) != 1
	      || FIND_REG_INC_NOTE (trial, 0))
	    return;
	  if (PREV_INSN (NEXT_INSN (trial)) == trial)
	    delete_insn (trial);
	  else
	    delete_from_delay_slot (trial);
	}
    }
#endif

  delete_insn (insn);
}

/* Counters for delay-slot filling.  */

#define NUM_REORG_FUNCTIONS 2
#define MAX_DELAY_HISTOGRAM 3
#define MAX_REORG_PASSES 2

static int num_insns_needing_delays[NUM_REORG_FUNCTIONS][MAX_REORG_PASSES];

static int num_filled_delays[NUM_REORG_FUNCTIONS][MAX_DELAY_HISTOGRAM+1][MAX_REORG_PASSES];

static int reorg_pass_number;

static void
note_delay_statistics (slots_filled, index)
     int slots_filled, index;
{
  num_insns_needing_delays[index][reorg_pass_number]++;
  if (slots_filled > MAX_DELAY_HISTOGRAM)
    slots_filled = MAX_DELAY_HISTOGRAM;
  num_filled_delays[index][slots_filled][reorg_pass_number]++;
}

#if defined(ANNUL_IFFALSE_SLOTS) || defined(ANNUL_IFTRUE_SLOTS)

/* Optimize the following cases:

   1.  When a conditional branch skips over only one instruction,
       use an annulling branch and put that insn in the delay slot.
       Use either a branch that annuls when the condition if true or
       invert the test with a branch that annuls when the condition is
       false.  This saves insns, since otherwise we must copy an insn
       from the L1 target.

        (orig)		 (skip)		(otherwise)
	Bcc.n L1	Bcc',a L1	Bcc,a L1'
	insn		insn		insn2
      L1:	      L1:	      L1:
	insn2		insn2		insn2
	insn3		insn3	      L1':
					insn3

   2.  When a conditional branch skips over only one instruction,
       and after that, it unconditionally branches somewhere else,
       perform the similar optimization. This saves executing the
       second branch in the case where the inverted condition is true.

	Bcc.n L1	Bcc',a L2
	insn		insn
      L1:	      L1:
	Bra L2		Bra L2

   INSN is a JUMP_INSN.

   This should be expanded to skip over N insns, where N is the number
   of delay slots required.  */

static rtx
optimize_skip (insn)
     register rtx insn;
{
  register rtx trial = next_nonnote_insn (insn);
  rtx next_trial = next_active_insn (trial);
  rtx delay_list = 0;
  rtx target_label;
  int flags;

  flags = get_jump_flags (insn, JUMP_LABEL (insn));

  if (trial == 0
      || GET_CODE (trial) != INSN
      || GET_CODE (PATTERN (trial)) == SEQUENCE
      || recog_memoized (trial) < 0
      || (! eligible_for_annul_false (insn, 0, trial, flags)
	  && ! eligible_for_annul_true (insn, 0, trial, flags)))
    return 0;

  /* There are two cases where we are just executing one insn (we assume
     here that a branch requires only one insn; this should be generalized
     at some point):  Where the branch goes around a single insn or where
     we have one insn followed by a branch to the same label we branch to.
     In both of these cases, inverting the jump and annulling the delay
     slot give the same effect in fewer insns.  */
  if ((next_trial == next_active_insn (JUMP_LABEL (insn)))
      || (next_trial != 0
	  && GET_CODE (next_trial) == JUMP_INSN
	  && JUMP_LABEL (insn) == JUMP_LABEL (next_trial)
	  && (simplejump_p (next_trial)
	      || GET_CODE (PATTERN (next_trial)) == RETURN)))
    {
      if (eligible_for_annul_false (insn, 0, trial, flags))
	{
	  if (invert_jump (insn, JUMP_LABEL (insn)))
	    INSN_FROM_TARGET_P (trial) = 1;
	  else if (! eligible_for_annul_true (insn, 0, trial, flags))
	    return 0;
	}

      delay_list = add_to_delay_list (trial, NULL_RTX);
      next_trial = next_active_insn (trial);
      update_block (trial, trial);
      delete_insn (trial);

      /* Also, if we are targeting an unconditional
	 branch, thread our jump to the target of that branch.  Don't
	 change this into a RETURN here, because it may not accept what
	 we have in the delay slot.  We'll fix this up later.  */
      if (next_trial && GET_CODE (next_trial) == JUMP_INSN
	  && (simplejump_p (next_trial)
	      || GET_CODE (PATTERN (next_trial)) == RETURN))
	{
	  target_label = JUMP_LABEL (next_trial);
	  if (target_label == 0)
	    target_label = find_end_label ();

	  /* Recompute the flags based on TARGET_LABEL since threading
	     the jump to TARGET_LABEL may change the direction of the
	     jump (which may change the circumstances in which the
	     delay slot is nullified).  */
	  flags = get_jump_flags (insn, target_label);
	  if (eligible_for_annul_true (insn, 0, trial, flags))
	    reorg_redirect_jump (insn, target_label);
	}

      INSN_ANNULLED_BRANCH_P (insn) = 1;
    }

  return delay_list;
}
#endif


/*  Encode and return branch direction and prediction information for
    INSN assuming it will jump to LABEL.

    Non conditional branches return no direction information and
    are predicted as very likely taken.  */
static int
get_jump_flags (insn, label)
     rtx insn, label;
{
  int flags;

  /* get_jump_flags can be passed any insn with delay slots, these may
     be INSNs, CALL_INSNs, or JUMP_INSNs.  Only JUMP_INSNs have branch
     direction information, and only if they are conditional jumps.

     If LABEL is zero, then there is no way to determine the branch
     direction.  */
  if (GET_CODE (insn) == JUMP_INSN
      && (condjump_p (insn) || condjump_in_parallel_p (insn))
      && INSN_UID (insn) <= max_uid
      && label != 0
      && INSN_UID (label) <= max_uid)
    flags 
      = (uid_to_ruid[INSN_UID (label)] > uid_to_ruid[INSN_UID (insn)])
	 ? ATTR_FLAG_forward : ATTR_FLAG_backward;
  /* No valid direction information.  */
  else
    flags = 0;
  
  /* If insn is a conditional branch call mostly_true_jump to get
     determine the branch prediction.  

     Non conditional branches are predicted as very likely taken.  */
  if (GET_CODE (insn) == JUMP_INSN
      && (condjump_p (insn) || condjump_in_parallel_p (insn)))
    {
      int prediction;

      prediction = mostly_true_jump (insn, get_branch_condition (insn, label));
      switch (prediction)
	{
	  case 2:
	    flags |= (ATTR_FLAG_very_likely | ATTR_FLAG_likely);
	    break;
	  case 1:
	    flags |= ATTR_FLAG_likely;
	    break;
	  case 0:
	    flags |= ATTR_FLAG_unlikely;
	    break;
	  case -1:
	    flags |= (ATTR_FLAG_very_unlikely | ATTR_FLAG_unlikely);
	    break;

	  default:
	    abort();
	}
    }
  else
    flags |= (ATTR_FLAG_very_likely | ATTR_FLAG_likely);

  return flags;
}

/* Return 1 if INSN is a destination that will be branched to rarely (the
   return point of a function); return 2 if DEST will be branched to very
   rarely (a call to a function that doesn't return).  Otherwise,
   return 0.  */

static int
rare_destination (insn)
     rtx insn;
{
  int jump_count = 0;
  rtx next;

  for (; insn; insn = next)
    {
      if (GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	insn = XVECEXP (PATTERN (insn), 0, 0);

      next = NEXT_INSN (insn);

      switch (GET_CODE (insn))
	{
	case CODE_LABEL:
	  return 0;
	case BARRIER:
	  /* A BARRIER can either be after a JUMP_INSN or a CALL_INSN.  We 
	     don't scan past JUMP_INSNs, so any barrier we find here must
	     have been after a CALL_INSN and hence mean the call doesn't
	     return.  */
	  return 2;
	case JUMP_INSN:
	  if (GET_CODE (PATTERN (insn)) == RETURN)
	    return 1;
	  else if (simplejump_p (insn)
		   && jump_count++ < 10)
	    next = JUMP_LABEL (insn);
	  else
	    return 0;
	}
    }

  /* If we got here it means we hit the end of the function.  So this
     is an unlikely destination.  */

  return 1;
}

/* Return truth value of the statement that this branch
   is mostly taken.  If we think that the branch is extremely likely
   to be taken, we return 2.  If the branch is slightly more likely to be
   taken, return 1.  If the branch is slightly less likely to be taken,
   return 0 and if the branch is highly unlikely to be taken, return -1.

   CONDITION, if non-zero, is the condition that JUMP_INSN is testing.  */

static int
mostly_true_jump (jump_insn, condition)
     rtx jump_insn, condition;
{
  rtx target_label = JUMP_LABEL (jump_insn);
  rtx insn;
  int rare_dest = rare_destination (target_label);
  int rare_fallthrough = rare_destination (NEXT_INSN (jump_insn));

  /* If this is a branch outside a loop, it is highly unlikely.  */
  if (GET_CODE (PATTERN (jump_insn)) == SET
      && GET_CODE (SET_SRC (PATTERN (jump_insn))) == IF_THEN_ELSE
      && ((GET_CODE (XEXP (SET_SRC (PATTERN (jump_insn)), 1)) == LABEL_REF
	   && LABEL_OUTSIDE_LOOP_P (XEXP (SET_SRC (PATTERN (jump_insn)), 1)))
	  || (GET_CODE (XEXP (SET_SRC (PATTERN (jump_insn)), 2)) == LABEL_REF
	      && LABEL_OUTSIDE_LOOP_P (XEXP (SET_SRC (PATTERN (jump_insn)), 2)))))
    return -1;

  if (target_label)
    {
      /* If this is the test of a loop, it is very likely true.  We scan
	 backwards from the target label.  If we find a NOTE_INSN_LOOP_BEG
	 before the next real insn, we assume the branch is to the top of 
	 the loop.  */
      for (insn = PREV_INSN (target_label);
	   insn && GET_CODE (insn) == NOTE;
	   insn = PREV_INSN (insn))
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	  return 2;

      /* If this is a jump to the test of a loop, it is likely true.  We scan
	 forwards from the target label.  If we find a NOTE_INSN_LOOP_VTOP
	 before the next real insn, we assume the branch is to the loop branch
	 test.  */
      for (insn = NEXT_INSN (target_label);
	   insn && GET_CODE (insn) == NOTE;
	   insn = PREV_INSN (insn))
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_VTOP)
	  return 1;
    }

  /* Look at the relative rarities of the fallthrough and destination.  If
     they differ, we can predict the branch that way. */

  switch (rare_fallthrough - rare_dest)
    {
    case -2:
      return -1;
    case -1:
      return 0;
    case 0:
      break;
    case 1:
      return 1;
    case 2:
      return 2;
    }

  /* If we couldn't figure out what this jump was, assume it won't be 
     taken.  This should be rare.  */
  if (condition == 0)
    return 0;

  /* EQ tests are usually false and NE tests are usually true.  Also,
     most quantities are positive, so we can make the appropriate guesses
     about signed comparisons against zero.  */
  switch (GET_CODE (condition))
    {
    case CONST_INT:
      /* Unconditional branch.  */
      return 1;
    case EQ:
      return 0;
    case NE:
      return 1;
    case LE:
    case LT:
      if (XEXP (condition, 1) == const0_rtx)
        return 0;
      break;
    case GE:
    case GT:
      if (XEXP (condition, 1) == const0_rtx)
	return 1;
      break;
    }

  /* Predict backward branches usually take, forward branches usually not.  If
     we don't know whether this is forward or backward, assume the branch
     will be taken, since most are.  */
  return (target_label == 0 || INSN_UID (jump_insn) > max_uid
	  || INSN_UID (target_label) > max_uid
	  || (uid_to_ruid[INSN_UID (jump_insn)]
	      > uid_to_ruid[INSN_UID (target_label)]));;
}

/* Return the condition under which INSN will branch to TARGET.  If TARGET
   is zero, return the condition under which INSN will return.  If INSN is
   an unconditional branch, return const_true_rtx.  If INSN isn't a simple
   type of jump, or it doesn't go to TARGET, return 0.  */

static rtx
get_branch_condition (insn, target)
     rtx insn;
     rtx target;
{
  rtx pat = PATTERN (insn);
  rtx src;
  
  if (condjump_in_parallel_p (insn))
    pat = XVECEXP (pat, 0, 0);

  if (GET_CODE (pat) == RETURN)
    return target == 0 ? const_true_rtx : 0;

  else if (GET_CODE (pat) != SET || SET_DEST (pat) != pc_rtx)
    return 0;

  src = SET_SRC (pat);
  if (GET_CODE (src) == LABEL_REF && XEXP (src, 0) == target)
    return const_true_rtx;

  else if (GET_CODE (src) == IF_THEN_ELSE
	   && ((target == 0 && GET_CODE (XEXP (src, 1)) == RETURN)
	       || (GET_CODE (XEXP (src, 1)) == LABEL_REF
		   && XEXP (XEXP (src, 1), 0) == target))
	   && XEXP (src, 2) == pc_rtx)
    return XEXP (src, 0);

  else if (GET_CODE (src) == IF_THEN_ELSE
	   && ((target == 0 && GET_CODE (XEXP (src, 2)) == RETURN)
	       || (GET_CODE (XEXP (src, 2)) == LABEL_REF
		   && XEXP (XEXP (src, 2), 0) == target))
	   && XEXP (src, 1) == pc_rtx)
    return gen_rtx (reverse_condition (GET_CODE (XEXP (src, 0))),
		    GET_MODE (XEXP (src, 0)),
		    XEXP (XEXP (src, 0), 0), XEXP (XEXP (src, 0), 1));

  return 0;
}

/* Return non-zero if CONDITION is more strict than the condition of
   INSN, i.e., if INSN will always branch if CONDITION is true.  */

static int
condition_dominates_p (condition, insn)
     rtx condition;
     rtx insn;
{
  rtx other_condition = get_branch_condition (insn, JUMP_LABEL (insn));
  enum rtx_code code = GET_CODE (condition);
  enum rtx_code other_code;

  if (rtx_equal_p (condition, other_condition)
      || other_condition == const_true_rtx)
    return 1;

  else if (condition == const_true_rtx || other_condition == 0)
    return 0;

  other_code = GET_CODE (other_condition);
  if (GET_RTX_LENGTH (code) != 2 || GET_RTX_LENGTH (other_code) != 2
      || ! rtx_equal_p (XEXP (condition, 0), XEXP (other_condition, 0))
      || ! rtx_equal_p (XEXP (condition, 1), XEXP (other_condition, 1)))
    return 0;

  return comparison_dominates_p (code, other_code);
}

/* Return non-zero if redirecting JUMP to NEWLABEL does not invalidate
   any insns already in the delay slot of JUMP.  */

static int
redirect_with_delay_slots_safe_p (jump, newlabel, seq)
     rtx jump, newlabel, seq;
{
  int flags, slots, i;
  rtx pat = PATTERN (seq);

  /* Make sure all the delay slots of this jump would still
     be valid after threading the jump.  If they are still
     valid, then return non-zero.  */

  flags = get_jump_flags (jump, newlabel);
  for (i = 1; i < XVECLEN (pat, 0); i++)
    if (! (
#ifdef ANNUL_IFFALSE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)))
	   ? eligible_for_annul_false (jump, i - 1,
				       XVECEXP (pat, 0, i), flags) :
#endif
#ifdef ANNUL_IFTRUE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && ! INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)))
	   ? eligible_for_annul_true (jump, i - 1,
				      XVECEXP (pat, 0, i), flags) :
#endif
	   eligible_for_delay (jump, i -1, XVECEXP (pat, 0, i), flags)))
      break;

  return (i == XVECLEN (pat, 0));
}

/* Return non-zero if redirecting JUMP to NEWLABEL does not invalidate
   any insns we wish to place in the delay slot of JUMP.  */

static int
redirect_with_delay_list_safe_p (jump, newlabel, delay_list)
     rtx jump, newlabel, delay_list;
{
  int flags, i;
  rtx li;

  /* Make sure all the insns in DELAY_LIST would still be
     valid after threading the jump.  If they are still
     valid, then return non-zero.  */

  flags = get_jump_flags (jump, newlabel);
  for (li = delay_list, i = 0; li; li = XEXP (li, 1), i++)
    if (! (
#ifdef ANNUL_IFFALSE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && INSN_FROM_TARGET_P (XEXP (li, 0)))
	   ? eligible_for_annul_false (jump, i, XEXP (li, 0), flags) :
#endif
#ifdef ANNUL_IFTRUE_SLOTS
	   (INSN_ANNULLED_BRANCH_P (jump)
	    && ! INSN_FROM_TARGET_P (XEXP (li, 0)))
	   ? eligible_for_annul_true (jump, i, XEXP (li, 0), flags) :
#endif
	   eligible_for_delay (jump, i, XEXP (li, 0), flags)))
      break;

  return (li == NULL);
}


/* INSN branches to an insn whose pattern SEQ is a SEQUENCE.  Given that
   the condition tested by INSN is CONDITION and the resources shown in
   OTHER_NEEDED are needed after INSN, see whether INSN can take all the insns
   from SEQ's delay list, in addition to whatever insns it may execute
   (in DELAY_LIST).   SETS and NEEDED are denote resources already set and
   needed while searching for delay slot insns.  Return the concatenated
   delay list if possible, otherwise, return 0.

   SLOTS_TO_FILL is the total number of slots required by INSN, and
   PSLOTS_FILLED points to the number filled so far (also the number of
   insns in DELAY_LIST).  It is updated with the number that have been
   filled from the SEQUENCE, if any.

   PANNUL_P points to a non-zero value if we already know that we need
   to annul INSN.  If this routine determines that annulling is needed,
   it may set that value non-zero.

   PNEW_THREAD points to a location that is to receive the place at which
   execution should continue.  */

static rtx
steal_delay_list_from_target (insn, condition, seq, delay_list,
			      sets, needed, other_needed,
			      slots_to_fill, pslots_filled, pannul_p,
			      pnew_thread)
     rtx insn, condition;
     rtx seq;
     rtx delay_list;
     struct resources *sets, *needed, *other_needed;
     int slots_to_fill;
     int *pslots_filled;
     int *pannul_p;
     rtx *pnew_thread;
{
  rtx temp;
  int slots_remaining = slots_to_fill - *pslots_filled;
  int total_slots_filled = *pslots_filled;
  rtx new_delay_list = 0;
  int must_annul = *pannul_p;
  int i;

  /* We can't do anything if there are more delay slots in SEQ than we
     can handle, or if we don't know that it will be a taken branch.
     We know that it will be a taken branch if it is either an unconditional
     branch or a conditional branch with a stricter branch condition.

     Also, exit if the branch has more than one set, since then it is computing
     other results that can't be ignored, e.g. the HPPA mov&branch instruction.
     ??? It may be possible to move other sets into INSN in addition to
     moving the instructions in the delay slots.  */

  if (XVECLEN (seq, 0) - 1 > slots_remaining
      || ! condition_dominates_p (condition, XVECEXP (seq, 0, 0))
      || ! single_set (XVECEXP (seq, 0, 0)))
    return delay_list;

  for (i = 1; i < XVECLEN (seq, 0); i++)
    {
      rtx trial = XVECEXP (seq, 0, i);
      int flags;

      if (insn_references_resource_p (trial, sets, 0)
	  || insn_sets_resource_p (trial, needed, 0)
	  || insn_sets_resource_p (trial, sets, 0)
#ifdef HAVE_cc0
	  /* If TRIAL sets CC0, we can't copy it, so we can't steal this
	     delay list.  */
	  || find_reg_note (trial, REG_CC_USER, NULL_RTX)
#endif
	  /* If TRIAL is from the fallthrough code of an annulled branch insn
	     in SEQ, we cannot use it.  */
	  || (INSN_ANNULLED_BRANCH_P (XVECEXP (seq, 0, 0))
	      && ! INSN_FROM_TARGET_P (trial)))
	return delay_list;

      /* If this insn was already done (usually in a previous delay slot),
	 pretend we put it in our delay slot.  */
      if (redundant_insn (trial, insn, new_delay_list))
	continue;

      /* We will end up re-vectoring this branch, so compute flags
	 based on jumping to the new label.  */
      flags = get_jump_flags (insn, JUMP_LABEL (XVECEXP (seq, 0, 0)));

      if (! must_annul
	  && ((condition == const_true_rtx
	       || (! insn_sets_resource_p (trial, other_needed, 0)
		   && ! may_trap_p (PATTERN (trial)))))
	  ? eligible_for_delay (insn, total_slots_filled, trial, flags)
	  : (must_annul = 1,
	     eligible_for_annul_false (insn, total_slots_filled, trial, flags)))
	{
	  temp = copy_rtx (trial);
	  INSN_FROM_TARGET_P (temp) = 1;
	  new_delay_list = add_to_delay_list (temp, new_delay_list);
	  total_slots_filled++;

	  if (--slots_remaining == 0)
	    break;
	}
      else
	return delay_list;
    }

  /* Show the place to which we will be branching.  */
  *pnew_thread = next_active_insn (JUMP_LABEL (XVECEXP (seq, 0, 0)));

  /* Add any new insns to the delay list and update the count of the
     number of slots filled.  */
  *pslots_filled = total_slots_filled;
  *pannul_p = must_annul;

  if (delay_list == 0)
    return new_delay_list;

  for (temp = new_delay_list; temp; temp = XEXP (temp, 1))
    delay_list = add_to_delay_list (XEXP (temp, 0), delay_list);

  return delay_list;
}

/* Similar to steal_delay_list_from_target except that SEQ is on the 
   fallthrough path of INSN.  Here we only do something if the delay insn
   of SEQ is an unconditional branch.  In that case we steal its delay slot
   for INSN since unconditional branches are much easier to fill.  */

static rtx
steal_delay_list_from_fallthrough (insn, condition, seq, 
				   delay_list, sets, needed, other_needed,
				   slots_to_fill, pslots_filled, pannul_p)
     rtx insn, condition;
     rtx seq;
     rtx delay_list;
     struct resources *sets, *needed, *other_needed;
     int slots_to_fill;
     int *pslots_filled;
     int *pannul_p;
{
  int i;
  int flags;

  flags = get_jump_flags (insn, JUMP_LABEL (insn));

  /* We can't do anything if SEQ's delay insn isn't an
     unconditional branch.  */

  if (! simplejump_p (XVECEXP (seq, 0, 0))
      && GET_CODE (PATTERN (XVECEXP (seq, 0, 0))) != RETURN)
    return delay_list;

  for (i = 1; i < XVECLEN (seq, 0); i++)
    {
      rtx trial = XVECEXP (seq, 0, i);

      /* If TRIAL sets CC0, stealing it will move it too far from the use
	 of CC0.  */
      if (insn_references_resource_p (trial, sets, 0)
	  || insn_sets_resource_p (trial, needed, 0)
	  || insn_sets_resource_p (trial, sets, 0)
#ifdef HAVE_cc0
	  || sets_cc0_p (PATTERN (trial))
#endif
	  )

	break;

      /* If this insn was already done, we don't need it.  */
      if (redundant_insn (trial, insn, delay_list))
	{
	  delete_from_delay_slot (trial);
	  continue;
	}

      if (! *pannul_p
	  && ((condition == const_true_rtx
	       || (! insn_sets_resource_p (trial, other_needed, 0)
		   && ! may_trap_p (PATTERN (trial)))))
	  ? eligible_for_delay (insn, *pslots_filled, trial, flags)
	  : (*pannul_p = 1,
	     eligible_for_annul_true (insn, *pslots_filled, trial, flags)))
	{
	  delete_from_delay_slot (trial);
	  delay_list = add_to_delay_list (trial, delay_list);

	  if (++(*pslots_filled) == slots_to_fill)
	    break;
	}
      else
	break;
    }

  return delay_list;
}

/* Try merging insns starting at THREAD which match exactly the insns in
   INSN's delay list.

   If all insns were matched and the insn was previously annulling, the
   annul bit will be cleared.

   For each insn that is merged, if the branch is or will be non-annulling,
   we delete the merged insn.  */

static void
try_merge_delay_insns (insn, thread)
     rtx insn, thread;
{
  rtx trial, next_trial;
  rtx delay_insn = XVECEXP (PATTERN (insn), 0, 0);
  int annul_p = INSN_ANNULLED_BRANCH_P (delay_insn);
  int slot_number = 1;
  int num_slots = XVECLEN (PATTERN (insn), 0);
  rtx next_to_match = XVECEXP (PATTERN (insn), 0, slot_number);
  struct resources set, needed;
  rtx merged_insns = 0;
  int i;
  int flags;

  flags = get_jump_flags (delay_insn, JUMP_LABEL (delay_insn));

  CLEAR_RESOURCE (&needed);
  CLEAR_RESOURCE (&set);

  /* If this is not an annulling branch, take into account anything needed in
     NEXT_TO_MATCH.  This prevents two increments from being incorrectly
     folded into one.  If we are annulling, this would be the correct
     thing to do.  (The alternative, looking at things set in NEXT_TO_MATCH
     will essentially disable this optimization.  This method is somewhat of
     a kludge, but I don't see a better way.)  */
  if (! annul_p)
    mark_referenced_resources (next_to_match, &needed, 1);

  for (trial = thread; !stop_search_p (trial, 1); trial = next_trial)
    {
      rtx pat = PATTERN (trial);
      rtx oldtrial = trial;

      next_trial = next_nonnote_insn (trial);

      /* TRIAL must be a CALL_INSN or INSN.  Skip USE and CLOBBER.  */
      if (GET_CODE (trial) == INSN
	  && (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER))
	continue;

      if (GET_CODE (next_to_match) == GET_CODE (trial)
#ifdef HAVE_cc0
	  /* We can't share an insn that sets cc0.  */
	  && ! sets_cc0_p (pat)
#endif
	  && ! insn_references_resource_p (trial, &set, 1)
	  && ! insn_sets_resource_p (trial, &set, 1)
	  && ! insn_sets_resource_p (trial, &needed, 1)
	  && (trial = try_split (pat, trial, 0)) != 0
	  /* Update next_trial, in case try_split succeeded.  */
	  && (next_trial = next_nonnote_insn (trial))
	  /* Likewise THREAD.  */
	  && (thread = oldtrial == thread ? trial : thread)
	  && rtx_equal_p (PATTERN (next_to_match), PATTERN (trial))
	  /* Have to test this condition if annul condition is different
	     from (and less restrictive than) non-annulling one.  */
	  && eligible_for_delay (delay_insn, slot_number - 1, trial, flags))
	{

	  if (! annul_p)
	    {
	      update_block (trial, thread);
	      if (trial == thread)
		thread = next_active_insn (thread);

	      delete_insn (trial);
	      INSN_FROM_TARGET_P (next_to_match) = 0;
	    }
	  else
	    merged_insns = gen_rtx (INSN_LIST, VOIDmode, trial, merged_insns);

	  if (++slot_number == num_slots)
	    break;

	  next_to_match = XVECEXP (PATTERN (insn), 0, slot_number);
	  if (! annul_p)
	    mark_referenced_resources (next_to_match, &needed, 1);
	}

      mark_set_resources (trial, &set, 0, 1);
      mark_referenced_resources (trial, &needed, 1);
    }

  /* See if we stopped on a filled insn.  If we did, try to see if its
     delay slots match.  */
  if (slot_number != num_slots
      && trial && GET_CODE (trial) == INSN
      && GET_CODE (PATTERN (trial)) == SEQUENCE
      && ! INSN_ANNULLED_BRANCH_P (XVECEXP (PATTERN (trial), 0, 0)))
    {
      rtx pat = PATTERN (trial);
      rtx filled_insn = XVECEXP (pat, 0, 0);

      /* Account for resources set/needed by the filled insn.  */
      mark_set_resources (filled_insn, &set, 0, 1);
      mark_referenced_resources (filled_insn, &needed, 1);

      for (i = 1; i < XVECLEN (pat, 0); i++)
	{
	  rtx dtrial = XVECEXP (pat, 0, i);

	  if (! insn_references_resource_p (dtrial, &set, 1)
	      && ! insn_sets_resource_p (dtrial, &set, 1)
	      && ! insn_sets_resource_p (dtrial, &needed, 1)
#ifdef HAVE_cc0
	      && ! sets_cc0_p (PATTERN (dtrial))
#endif
	      && rtx_equal_p (PATTERN (next_to_match), PATTERN (dtrial))
	      && eligible_for_delay (delay_insn, slot_number - 1, dtrial, flags))
	    {
	      if (! annul_p)
		{
		  update_block (dtrial, thread);
		  delete_from_delay_slot (dtrial);
		  INSN_FROM_TARGET_P (next_to_match) = 0;
		}
	      else
		merged_insns = gen_rtx (INSN_LIST, SImode, dtrial,
					merged_insns);

	      if (++slot_number == num_slots)
		break;

	      next_to_match = XVECEXP (PATTERN (insn), 0, slot_number);
	    }
	}
    }

  /* If all insns in the delay slot have been matched and we were previously
     annulling the branch, we need not any more.  In that case delete all the
     merged insns.  Also clear the INSN_FROM_TARGET_P bit of each insn the
     the delay list so that we know that it isn't only being used at the
     target.  */
  if (slot_number == num_slots && annul_p)
    {
      for (; merged_insns; merged_insns = XEXP (merged_insns, 1))
	{
	  if (GET_MODE (merged_insns) == SImode)
	    {
	      update_block (XEXP (merged_insns, 0), thread);
	      delete_from_delay_slot (XEXP (merged_insns, 0));
	    }
	  else
	    {
	      update_block (XEXP (merged_insns, 0), thread);
	      delete_insn (XEXP (merged_insns, 0));
	    }
	}

      INSN_ANNULLED_BRANCH_P (delay_insn) = 0;

      for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	INSN_FROM_TARGET_P (XVECEXP (PATTERN (insn), 0, i)) = 0;
    }
}

/* See if INSN is redundant with an insn in front of TARGET.  Often this
   is called when INSN is a candidate for a delay slot of TARGET.
   DELAY_LIST are insns that will be placed in delay slots of TARGET in front
   of INSN.  Often INSN will be redundant with an insn in a delay slot of
   some previous insn.  This happens when we have a series of branches to the
   same label; in that case the first insn at the target might want to go
   into each of the delay slots.

   If we are not careful, this routine can take up a significant fraction
   of the total compilation time (4%), but only wins rarely.  Hence we
   speed this routine up by making two passes.  The first pass goes back
   until it hits a label and sees if it find an insn with an identical
   pattern.  Only in this (relatively rare) event does it check for
   data conflicts.

   We do not split insns we encounter.  This could cause us not to find a
   redundant insn, but the cost of splitting seems greater than the possible
   gain in rare cases.  */

static rtx
redundant_insn (insn, target, delay_list)
     rtx insn;
     rtx target;
     rtx delay_list;
{
  rtx target_main = target;
  rtx ipat = PATTERN (insn);
  rtx trial, pat;
  struct resources needed, set;
  int i;

  /* Scan backwards looking for a match.  */
  for (trial = PREV_INSN (target); trial; trial = PREV_INSN (trial))
    {
      if (GET_CODE (trial) == CODE_LABEL)
	return 0;

      if (GET_RTX_CLASS (GET_CODE (trial)) != 'i')
	continue;

      pat = PATTERN (trial);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
	continue;

      if (GET_CODE (pat) == SEQUENCE)
	{
	  /* Stop for a CALL and its delay slots because it is difficult to
	     track its resource needs correctly.  */
	  if (GET_CODE (XVECEXP (pat, 0, 0)) == CALL_INSN)
	    return 0;

	  /* Stop for an INSN or JUMP_INSN with delayed effects and its delay
	     slots because it is difficult to track its resource needs 
	     correctly.  */

#ifdef INSN_SETS_ARE_DELAYED
	  if (INSN_SETS_ARE_DELAYED (XVECEXP (pat, 0, 0)))
	    return 0; 
#endif

#ifdef INSN_REFERENCES_ARE_DELAYED
	  if (INSN_REFERENCES_ARE_DELAYED (XVECEXP (pat, 0, 0)))
	    return 0; 
#endif

	  /* See if any of the insns in the delay slot match, updating
	     resource requirements as we go.  */
	  for (i = XVECLEN (pat, 0) - 1; i > 0; i--)
	    if (GET_CODE (XVECEXP (pat, 0, i)) == GET_CODE (insn)
		&& rtx_equal_p (PATTERN (XVECEXP (pat, 0, i)), ipat))
	      break;

	  /* If found a match, exit this loop early.  */
	  if (i > 0)
	    break;
	}

      else if (GET_CODE (trial) == GET_CODE (insn) && rtx_equal_p (pat, ipat))
	break;
    }

  /* If we didn't find an insn that matches, return 0.  */
  if (trial == 0)
    return 0;

  /* See what resources this insn sets and needs.  If they overlap, or
     if this insn references CC0, it can't be redundant.  */

  CLEAR_RESOURCE (&needed);
  CLEAR_RESOURCE (&set);
  mark_set_resources (insn, &set, 0, 1);
  mark_referenced_resources (insn, &needed, 1);

  /* If TARGET is a SEQUENCE, get the main insn.  */
  if (GET_CODE (target) == INSN && GET_CODE (PATTERN (target)) == SEQUENCE)
    target_main = XVECEXP (PATTERN (target), 0, 0);

  if (resource_conflicts_p (&needed, &set)
#ifdef HAVE_cc0
      || reg_mentioned_p (cc0_rtx, ipat)
#endif
      /* The insn requiring the delay may not set anything needed or set by
	 INSN.  */
      || insn_sets_resource_p (target_main, &needed, 1)
      || insn_sets_resource_p (target_main, &set, 1))
    return 0;

  /* Insns we pass may not set either NEEDED or SET, so merge them for
     simpler tests.  */
  needed.memory |= set.memory;
  needed.unch_memory |= set.unch_memory;
  IOR_HARD_REG_SET (needed.regs, set.regs);

  /* This insn isn't redundant if it conflicts with an insn that either is
     or will be in a delay slot of TARGET.  */

  while (delay_list)
    {
      if (insn_sets_resource_p (XEXP (delay_list, 0), &needed, 1))
	return 0;
      delay_list = XEXP (delay_list, 1);
    }

  if (GET_CODE (target) == INSN && GET_CODE (PATTERN (target)) == SEQUENCE)
    for (i = 1; i < XVECLEN (PATTERN (target), 0); i++)
      if (insn_sets_resource_p (XVECEXP (PATTERN (target), 0, i), &needed, 1))
	return 0;

  /* Scan backwards until we reach a label or an insn that uses something
     INSN sets or sets something insn uses or sets.  */

  for (trial = PREV_INSN (target);
       trial && GET_CODE (trial) != CODE_LABEL;
       trial = PREV_INSN (trial))
    {
      if (GET_CODE (trial) != INSN && GET_CODE (trial) != CALL_INSN
	  && GET_CODE (trial) != JUMP_INSN)
	continue;

      pat = PATTERN (trial);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
	continue;

      if (GET_CODE (pat) == SEQUENCE)
	{
	  /* If this is a CALL_INSN and its delay slots, it is hard to track
	     the resource needs properly, so give up.  */
	  if (GET_CODE (XVECEXP (pat, 0, 0)) == CALL_INSN)
	    return 0;

	  /* If this this is an INSN or JUMP_INSN with delayed effects, it
	     is hard to track the resource needs properly, so give up.  */

#ifdef INSN_SETS_ARE_DELAYED
	  if (INSN_SETS_ARE_DELAYED (XVECEXP (pat, 0, 0)))
	    return 0; 
#endif

#ifdef INSN_REFERENCES_ARE_DELAYED
	  if (INSN_REFERENCES_ARE_DELAYED (XVECEXP (pat, 0, 0)))
	    return 0; 
#endif

	  /* See if any of the insns in the delay slot match, updating
	     resource requirements as we go.  */
	  for (i = XVECLEN (pat, 0) - 1; i > 0; i--)
	    {
	      rtx candidate = XVECEXP (pat, 0, i);

	      /* If an insn will be annulled if the branch is false, it isn't
		 considered as a possible duplicate insn.  */
	      if (rtx_equal_p (PATTERN (candidate), ipat)
		  && ! (INSN_ANNULLED_BRANCH_P (XVECEXP (pat, 0, 0))
			&& INSN_FROM_TARGET_P (candidate)))
		{
		  /* Show that this insn will be used in the sequel.  */
		  INSN_FROM_TARGET_P (candidate) = 0;
		  return candidate;
		}

	      /* Unless this is an annulled insn from the target of a branch,
		 we must stop if it sets anything needed or set by INSN.  */
	      if ((! INSN_ANNULLED_BRANCH_P (XVECEXP (pat, 0, 0))
		   || ! INSN_FROM_TARGET_P (candidate))
		  && insn_sets_resource_p (candidate, &needed, 1))
		return 0;
	    }


	  /* If the insn requiring the delay slot conflicts with INSN, we 
	     must stop.  */
	  if (insn_sets_resource_p (XVECEXP (pat, 0, 0), &needed, 1))
	    return 0;
	}
      else
	{
	  /* See if TRIAL is the same as INSN.  */
	  pat = PATTERN (trial);
	  if (rtx_equal_p (pat, ipat))
	    return trial;

	  /* Can't go any further if TRIAL conflicts with INSN.  */
	  if (insn_sets_resource_p (trial, &needed, 1))
	    return 0;
	}
    }

  return 0;
}

/* Return 1 if THREAD can only be executed in one way.  If LABEL is non-zero,
   it is the target of the branch insn being scanned.  If ALLOW_FALLTHROUGH
   is non-zero, we are allowed to fall into this thread; otherwise, we are
   not.

   If LABEL is used more than one or we pass a label other than LABEL before
   finding an active insn, we do not own this thread.  */

static int
own_thread_p (thread, label, allow_fallthrough)
     rtx thread;
     rtx label;
     int allow_fallthrough;
{
  rtx active_insn;
  rtx insn;

  /* We don't own the function end.  */
  if (thread == 0)
    return 0;

  /* Get the first active insn, or THREAD, if it is an active insn.  */
  active_insn = next_active_insn (PREV_INSN (thread));

  for (insn = thread; insn != active_insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == CODE_LABEL
	&& (insn != label || LABEL_NUSES (insn) != 1))
      return 0;

  if (allow_fallthrough)
    return 1;

  /* Ensure that we reach a BARRIER before any insn or label.  */
  for (insn = prev_nonnote_insn (thread);
       insn == 0 || GET_CODE (insn) != BARRIER;
       insn = prev_nonnote_insn (insn))
    if (insn == 0
	|| GET_CODE (insn) == CODE_LABEL
	|| (GET_CODE (insn) == INSN
	    && GET_CODE (PATTERN (insn)) != USE
	    && GET_CODE (PATTERN (insn)) != CLOBBER))
      return 0;

  return 1;
}

/* Find the number of the basic block that starts closest to INSN.  Return -1
   if we couldn't find such a basic block.  */

static int
find_basic_block (insn)
     rtx insn;
{
  int i;

  /* Scan backwards to the previous BARRIER.  Then see if we can find a
     label that starts a basic block.  Return the basic block number.  */

  for (insn = prev_nonnote_insn (insn);
       insn && GET_CODE (insn) != BARRIER;
       insn = prev_nonnote_insn (insn))
    ;

  /* The start of the function is basic block zero.  */
  if (insn == 0)
    return 0;

  /* See if any of the upcoming CODE_LABELs start a basic block.  If we reach
     anything other than a CODE_LABEL or note, we can't find this code.  */
  for (insn = next_nonnote_insn (insn);
       insn && GET_CODE (insn) == CODE_LABEL;
       insn = next_nonnote_insn (insn))
    {
      for (i = 0; i < n_basic_blocks; i++)
	if (insn == basic_block_head[i])
	  return i;
    }

  return -1;
}

/* Called when INSN is being moved from a location near the target of a jump.
   We leave a marker of the form (use (INSN)) immediately in front
   of WHERE for mark_target_live_regs.  These markers will be deleted when
   reorg finishes.

   We used to try to update the live status of registers if WHERE is at
   the start of a basic block, but that can't work since we may remove a
   BARRIER in relax_delay_slots.  */

static void
update_block (insn, where)
     rtx insn;
     rtx where;
{
  int b;

  /* Ignore if this was in a delay slot and it came from the target of 
     a branch.  */
  if (INSN_FROM_TARGET_P (insn))
    return;

  emit_insn_before (gen_rtx (USE, VOIDmode, insn), where);

  /* INSN might be making a value live in a block where it didn't use to
     be.  So recompute liveness information for this block.  */

  b = find_basic_block (insn);
  if (b != -1)
    bb_ticks[b]++;
}

/* Similar to REDIRECT_JUMP except that we update the BB_TICKS entry for
   the basic block containing the jump.  */

static int
reorg_redirect_jump (jump, nlabel)
     rtx jump;
     rtx nlabel;
{
  int b = find_basic_block (jump);

  if (b != -1)
    bb_ticks[b]++;

  return redirect_jump (jump, nlabel);
}

/* Called when INSN is being moved forward into a delay slot of DELAYED_INSN.
   We check every instruction between INSN and DELAYED_INSN for REG_DEAD notes
   that reference values used in INSN.  If we find one, then we move the
   REG_DEAD note to INSN.

   This is needed to handle the case where an later insn (after INSN) has a
   REG_DEAD note for a register used by INSN, and this later insn subsequently
   gets moved before a CODE_LABEL because it is a redundant insn.  In this
   case, mark_target_live_regs may be confused into thinking the register
   is dead because it sees a REG_DEAD note immediately before a CODE_LABEL.  */

static void
update_reg_dead_notes (insn, delayed_insn)
     rtx insn, delayed_insn;
{
  rtx p, link, next;

  for (p = next_nonnote_insn (insn); p != delayed_insn;
       p = next_nonnote_insn (p))
    for (link = REG_NOTES (p); link; link = next)
      {
	next = XEXP (link, 1);

	if (REG_NOTE_KIND (link) != REG_DEAD
	    || GET_CODE (XEXP (link, 0)) != REG)
	  continue;

	if (reg_referenced_p (XEXP (link, 0), PATTERN (insn)))
	  {
	    /* Move the REG_DEAD note from P to INSN.  */
	    remove_note (p, link);
	    XEXP (link, 1) = REG_NOTES (insn);
	    REG_NOTES (insn) = link;
	  }
      }
}

/* Delete any REG_UNUSED notes that exist on INSN but not on REDUNDANT_INSN.

   This handles the case of udivmodXi4 instructions which optimize their
   output depending on whether any REG_UNUSED notes are present.
   we must make sure that INSN calculates as many results as REDUNDANT_INSN
   does.  */

static void
update_reg_unused_notes (insn, redundant_insn)
     rtx insn, redundant_insn;
{
  rtx p, link, next;

  for (link = REG_NOTES (insn); link; link = next)
    {
      next = XEXP (link, 1);

      if (REG_NOTE_KIND (link) != REG_UNUSED
	  || GET_CODE (XEXP (link, 0)) != REG)
	continue;

      if (! find_regno_note (redundant_insn, REG_UNUSED,
			     REGNO (XEXP (link, 0))))
	remove_note (insn, link);
    }
}

/* Marks registers possibly live at the current place being scanned by
   mark_target_live_regs.  Used only by next two function.    */

static HARD_REG_SET current_live_regs;

/* Marks registers for which we have seen a REG_DEAD note but no assignment.
   Also only used by the next two functions.  */

static HARD_REG_SET pending_dead_regs;

/* Utility function called from mark_target_live_regs via note_stores.
   It deadens any CLOBBERed registers and livens any SET registers.  */

static void
update_live_status (dest, x)
     rtx dest;
     rtx x;
{
  int first_regno, last_regno;
  int i;

  if (GET_CODE (dest) != REG
      && (GET_CODE (dest) != SUBREG || GET_CODE (SUBREG_REG (dest)) != REG))
    return;

  if (GET_CODE (dest) == SUBREG)
    first_regno = REGNO (SUBREG_REG (dest)) + SUBREG_WORD (dest);
  else
    first_regno = REGNO (dest);

  last_regno = first_regno + HARD_REGNO_NREGS (first_regno, GET_MODE (dest));

  if (GET_CODE (x) == CLOBBER)
    for (i = first_regno; i < last_regno; i++)
      CLEAR_HARD_REG_BIT (current_live_regs, i);
  else
    for (i = first_regno; i < last_regno; i++)
      {
	SET_HARD_REG_BIT (current_live_regs, i);
	CLEAR_HARD_REG_BIT (pending_dead_regs, i);
      }
}

/* Similar to next_insn, but ignores insns in the delay slots of
   an annulled branch.  */

static rtx
next_insn_no_annul (insn)
     rtx insn;
{
  if (insn)
    {
      /* If INSN is an annulled branch, skip any insns from the target
	 of the branch.  */
      if (INSN_ANNULLED_BRANCH_P (insn)
	  && NEXT_INSN (PREV_INSN (insn)) != insn)
	while (INSN_FROM_TARGET_P (NEXT_INSN (insn)))
	  insn = NEXT_INSN (insn);

      insn = NEXT_INSN (insn);
      if (insn && GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SEQUENCE)
	insn = XVECEXP (PATTERN (insn), 0, 0);
    }

  return insn;
}

/* Set the resources that are live at TARGET.

   If TARGET is zero, we refer to the end of the current function and can
   return our precomputed value.

   Otherwise, we try to find out what is live by consulting the basic block
   information.  This is tricky, because we must consider the actions of
   reload and jump optimization, which occur after the basic block information
   has been computed.

   Accordingly, we proceed as follows::

   We find the previous BARRIER and look at all immediately following labels
   (with no intervening active insns) to see if any of them start a basic
   block.  If we hit the start of the function first, we use block 0.

   Once we have found a basic block and a corresponding first insns, we can
   accurately compute the live status from basic_block_live_regs and
   reg_renumber.  (By starting at a label following a BARRIER, we are immune
   to actions taken by reload and jump.)  Then we scan all insns between
   that point and our target.  For each CLOBBER (or for call-clobbered regs
   when we pass a CALL_INSN), mark the appropriate registers are dead.  For
   a SET, mark them as live.

   We have to be careful when using REG_DEAD notes because they are not
   updated by such things as find_equiv_reg.  So keep track of registers
   marked as dead that haven't been assigned to, and mark them dead at the
   next CODE_LABEL since reload and jump won't propagate values across labels.

   If we cannot find the start of a basic block (should be a very rare
   case, if it can happen at all), mark everything as potentially live.

   Next, scan forward from TARGET looking for things set or clobbered
   before they are used.  These are not live.

   Because we can be called many times on the same target, save our results
   in a hash table indexed by INSN_UID.  */

static void
mark_target_live_regs (target, res)
     rtx target;
     struct resources *res;
{
  int b = -1;
  int i;
  struct target_info *tinfo;
  rtx insn, next;
  rtx jump_insn = 0;
  rtx jump_target;
  HARD_REG_SET scratch;
  struct resources set, needed;
  int jump_count = 0;

  /* Handle end of function.  */
  if (target == 0)
    {
      *res = end_of_function_needs;
      return;
    }

  /* We have to assume memory is needed, but the CC isn't.  */
  res->memory = 1;
  res->volatil = res->unch_memory = 0;
  res->cc = 0;

  /* See if we have computed this value already.  */
  for (tinfo = target_hash_table[INSN_UID (target) % TARGET_HASH_PRIME];
       tinfo; tinfo = tinfo->next)
    if (tinfo->uid == INSN_UID (target))
      break;

  /* Start by getting the basic block number.  If we have saved information,
     we can get it from there unless the insn at the start of the basic block
     has been deleted.  */
  if (tinfo && tinfo->block != -1
      && ! INSN_DELETED_P (basic_block_head[tinfo->block]))
    b = tinfo->block;

  if (b == -1)
    b = find_basic_block (target);

  if (tinfo)
    {
      /* If the information is up-to-date, use it.  Otherwise, we will
	 update it below.  */
      if (b == tinfo->block && b != -1 && tinfo->bb_tick == bb_ticks[b])
	{
	  COPY_HARD_REG_SET (res->regs, tinfo->live_regs);
	  return;
	}
    }
  else
    {
      /* Allocate a place to put our results and chain it into the 
	 hash table.  */
      tinfo = (struct target_info *) oballoc (sizeof (struct target_info));
      tinfo->uid = INSN_UID (target);
      tinfo->block = b;
      tinfo->next = target_hash_table[INSN_UID (target) % TARGET_HASH_PRIME];
      target_hash_table[INSN_UID (target) % TARGET_HASH_PRIME] = tinfo;
    }

  CLEAR_HARD_REG_SET (pending_dead_regs);

  /* If we found a basic block, get the live registers from it and update
     them with anything set or killed between its start and the insn before
     TARGET.  Otherwise, we must assume everything is live.  */
  if (b != -1)
    {
      regset regs_live = basic_block_live_at_start[b];
      int offset, j;
      REGSET_ELT_TYPE bit;
      int regno;
      rtx start_insn, stop_insn;

      /* Compute hard regs live at start of block -- this is the real hard regs
	 marked live, plus live pseudo regs that have been renumbered to
	 hard regs.  */

#ifdef HARD_REG_SET
      current_live_regs = *regs_live;
#else
      COPY_HARD_REG_SET (current_live_regs, regs_live);
#endif

      for (offset = 0, i = 0; offset < regset_size; offset++)
	{
	  if (regs_live[offset] == 0)
	    i += REGSET_ELT_BITS;
	  else
	    for (bit = 1; bit && i < max_regno; bit <<= 1, i++)
	      if ((regs_live[offset] & bit)
		  && (regno = reg_renumber[i]) >= 0)
		for (j = regno;
		     j < regno + HARD_REGNO_NREGS (regno,
						   PSEUDO_REGNO_MODE (i));
		     j++)
		  SET_HARD_REG_BIT (current_live_regs, j);
	}

      /* Get starting and ending insn, handling the case where each might
	 be a SEQUENCE.  */
      start_insn = (b == 0 ? get_insns () : basic_block_head[b]);
      stop_insn = target;

      if (GET_CODE (start_insn) == INSN
	  && GET_CODE (PATTERN (start_insn)) == SEQUENCE)
	start_insn = XVECEXP (PATTERN (start_insn), 0, 0);

      if (GET_CODE (stop_insn) == INSN
	  && GET_CODE (PATTERN (stop_insn)) == SEQUENCE)
	stop_insn = next_insn (PREV_INSN (stop_insn));

      for (insn = start_insn; insn != stop_insn;
	   insn = next_insn_no_annul (insn))
	{
	  rtx link;
	  rtx real_insn = insn;

	  /* If this insn is from the target of a branch, it isn't going to
	     be used in the sequel.  If it is used in both cases, this
	     test will not be true.  */
	  if (INSN_FROM_TARGET_P (insn))
	    continue;

	  /* If this insn is a USE made by update_block, we care about the
	     underlying insn.  */
	  if (GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == USE
	      && GET_RTX_CLASS (GET_CODE (XEXP (PATTERN (insn), 0))) == 'i')
	      real_insn = XEXP (PATTERN (insn), 0);

	  if (GET_CODE (real_insn) == CALL_INSN)
	    {
	      /* CALL clobbers all call-used regs that aren't fixed except
		 sp, ap, and fp.  Do this before setting the result of the
		 call live.  */
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (call_used_regs[i]
		    && i != STACK_POINTER_REGNUM && i != FRAME_POINTER_REGNUM
		    && i != ARG_POINTER_REGNUM
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
		    && i != HARD_FRAME_POINTER_REGNUM
#endif
#if ARG_POINTER_REGNUM != FRAME_POINTER_REGNUM
		    && ! (i == ARG_POINTER_REGNUM && fixed_regs[i])
#endif
#ifdef PIC_OFFSET_TABLE_REGNUM
		    && ! (i == PIC_OFFSET_TABLE_REGNUM && flag_pic)
#endif
		    )
		  CLEAR_HARD_REG_BIT (current_live_regs, i);

	      /* A CALL_INSN sets any global register live, since it may
		 have been modified by the call.  */
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (global_regs[i])
		  SET_HARD_REG_BIT (current_live_regs, i);
	    }

	  /* Mark anything killed in an insn to be deadened at the next
	     label.  Ignore USE insns; the only REG_DEAD notes will be for
	     parameters.  But they might be early.  A CALL_INSN will usually
	     clobber registers used for parameters.  It isn't worth bothering
	     with the unlikely case when it won't.  */
	  if ((GET_CODE (real_insn) == INSN
	       && GET_CODE (PATTERN (real_insn)) != USE
	       && GET_CODE (PATTERN (real_insn)) != CLOBBER)
	      || GET_CODE (real_insn) == JUMP_INSN
	      || GET_CODE (real_insn) == CALL_INSN)
	    {
	      for (link = REG_NOTES (real_insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_DEAD
		    && GET_CODE (XEXP (link, 0)) == REG
		    && REGNO (XEXP (link, 0)) < FIRST_PSEUDO_REGISTER)
		  {
		    int first_regno = REGNO (XEXP (link, 0));
		    int last_regno
		      = (first_regno
			 + HARD_REGNO_NREGS (first_regno,
					     GET_MODE (XEXP (link, 0))));
			 
		    for (i = first_regno; i < last_regno; i++)
		      SET_HARD_REG_BIT (pending_dead_regs, i);
		  }

	      note_stores (PATTERN (real_insn), update_live_status);

	      /* If any registers were unused after this insn, kill them.
		 These notes will always be accurate.  */
	      for (link = REG_NOTES (real_insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_UNUSED
		    && GET_CODE (XEXP (link, 0)) == REG
		    && REGNO (XEXP (link, 0)) < FIRST_PSEUDO_REGISTER)
		  {
		    int first_regno = REGNO (XEXP (link, 0));
		    int last_regno
		      = (first_regno
			 + HARD_REGNO_NREGS (first_regno,
					     GET_MODE (XEXP (link, 0))));
			 
		    for (i = first_regno; i < last_regno; i++)
		      CLEAR_HARD_REG_BIT (current_live_regs, i);
		  }
	    }

	  else if (GET_CODE (real_insn) == CODE_LABEL)
	    {
	      /* A label clobbers the pending dead registers since neither
		 reload nor jump will propagate a value across a label.  */
	      AND_COMPL_HARD_REG_SET (current_live_regs, pending_dead_regs);
	      CLEAR_HARD_REG_SET (pending_dead_regs);
	    }

	  /* The beginning of the epilogue corresponds to the end of the
	     RTL chain when there are no epilogue insns.  Certain resources
	     are implicitly required at that point.  */
	  else if (GET_CODE (real_insn) == NOTE
 		   && NOTE_LINE_NUMBER (real_insn) == NOTE_INSN_EPILOGUE_BEG)
	    IOR_HARD_REG_SET (current_live_regs, start_of_epilogue_needs.regs);
	}

      COPY_HARD_REG_SET (res->regs, current_live_regs);
      tinfo->block = b;
      tinfo->bb_tick = bb_ticks[b];
    }
  else
    /* We didn't find the start of a basic block.  Assume everything
       in use.  This should happen only extremely rarely.  */
    SET_HARD_REG_SET (res->regs);

  /* Now step forward from TARGET looking for registers that are set before
     they are used.  These are dead.  If we pass a label, any pending dead
     registers that weren't yet used can be made dead.  Stop when we pass a
     conditional JUMP_INSN; follow the first few unconditional branches.  */

  CLEAR_RESOURCE (&set);
  CLEAR_RESOURCE (&needed);

  for (insn = target; insn; insn = next)
    {
      rtx this_jump_insn = insn;

      next = NEXT_INSN (insn);
      switch (GET_CODE (insn))
	{
	case CODE_LABEL:
	  AND_COMPL_HARD_REG_SET (pending_dead_regs, needed.regs);
	  AND_COMPL_HARD_REG_SET (res->regs, pending_dead_regs);
	  CLEAR_HARD_REG_SET (pending_dead_regs);
	  continue;

	case BARRIER:
	case NOTE:
	  continue;

	case INSN:
	  if (GET_CODE (PATTERN (insn)) == USE)
	    {
	      /* If INSN is a USE made by update_block, we care about the
		 underlying insn.  Any registers set by the underlying insn
		 are live since the insn is being done somewhere else.  */
	      if (GET_RTX_CLASS (GET_CODE (XEXP (PATTERN (insn), 0))) == 'i')
		mark_set_resources (XEXP (PATTERN (insn), 0), res, 0, 1);

	      /* All other USE insns are to be ignored.  */
	      continue;
	    }
	  else if (GET_CODE (PATTERN (insn)) == CLOBBER)
	    continue;
	  else if (GET_CODE (PATTERN (insn)) == SEQUENCE)
	    {
	      /* An unconditional jump can be used to fill the delay slot
		 of a call, so search for a JUMP_INSN in any position.  */
	      for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
		{
		  this_jump_insn = XVECEXP (PATTERN (insn), 0, i);
		  if (GET_CODE (this_jump_insn) == JUMP_INSN)
		    break;
		}
	    }
	}

      if (GET_CODE (this_jump_insn) == JUMP_INSN)
	{
	  if (jump_count++ < 10
	      && (simplejump_p (this_jump_insn)
		  || GET_CODE (PATTERN (this_jump_insn)) == RETURN))
	    {
	      next = next_active_insn (JUMP_LABEL (this_jump_insn));
	      if (jump_insn == 0)
		{
		  jump_insn = insn;
		  jump_target = JUMP_LABEL (this_jump_insn);
		}
	    }
	  else
	    break;
	}

      mark_referenced_resources (insn, &needed, 1);
      mark_set_resources (insn, &set, 0, 1);

      COPY_HARD_REG_SET (scratch, set.regs);
      AND_COMPL_HARD_REG_SET (scratch, needed.regs);
      AND_COMPL_HARD_REG_SET (res->regs, scratch);
    }

  /* If we hit an unconditional branch, we have another way of finding out
     what is live: we can see what is live at the branch target and include
     anything used but not set before the branch.  The only things that are
     live are those that are live using the above test and the test below.

     Don't try this if we expired our jump count above, since that would
     mean there may be an infinite loop in the function being compiled.  */

  if (jump_insn && jump_count < 10)
    {
      struct resources new_resources;
      rtx stop_insn = next_active_insn (jump_insn);

      mark_target_live_regs (next_active_insn (jump_target), &new_resources);
      CLEAR_RESOURCE (&set);
      CLEAR_RESOURCE (&needed);

      /* Include JUMP_INSN in the needed registers.  */
      for (insn = target; insn != stop_insn; insn = next_active_insn (insn))
	{
	  mark_referenced_resources (insn, &needed, 1);

	  COPY_HARD_REG_SET (scratch, needed.regs);
	  AND_COMPL_HARD_REG_SET (scratch, set.regs);
	  IOR_HARD_REG_SET (new_resources.regs, scratch);

	  mark_set_resources (insn, &set, 0, 1);
	}

      AND_HARD_REG_SET (res->regs, new_resources.regs);
    }

  COPY_HARD_REG_SET (tinfo->live_regs, res->regs);
}

/* Scan a function looking for insns that need a delay slot and find insns to
   put into the delay slot.

   NON_JUMPS_P is non-zero if we are to only try to fill non-jump insns (such
   as calls).  We do these first since we don't want jump insns (that are
   easier to fill) to get the only insns that could be used for non-jump insns.
   When it is zero, only try to fill JUMP_INSNs.

   When slots are filled in this manner, the insns (including the
   delay_insn) are put together in a SEQUENCE rtx.  In this fashion,
   it is possible to tell whether a delay slot has really been filled
   or not.  `final' knows how to deal with this, by communicating
   through FINAL_SEQUENCE.  */

static void
fill_simple_delay_slots (first, non_jumps_p)
     rtx first;
     int non_jumps_p;
{
  register rtx insn, pat, trial, next_trial;
  register int i, j;
  int num_unfilled_slots = unfilled_slots_next - unfilled_slots_base;
  struct resources needed, set;
  register int slots_to_fill, slots_filled;
  rtx delay_list;

  for (i = 0; i < num_unfilled_slots; i++)
    {
      int flags;
      /* Get the next insn to fill.  If it has already had any slots assigned,
	 we can't do anything with it.  Maybe we'll improve this later.  */

      insn = unfilled_slots_base[i];
      if (insn == 0
	  || INSN_DELETED_P (insn)
	  || (GET_CODE (insn) == INSN
	      && GET_CODE (PATTERN (insn)) == SEQUENCE)
	  || (GET_CODE (insn) == JUMP_INSN && non_jumps_p)
	  || (GET_CODE (insn) != JUMP_INSN && ! non_jumps_p))
	continue;
     
      if (GET_CODE (insn) == JUMP_INSN)
	flags = get_jump_flags (insn, JUMP_LABEL (insn));
      else
	flags = get_jump_flags (insn, NULL_RTX);
      slots_to_fill = num_delay_slots (insn);
      if (slots_to_fill == 0)
	abort ();

      /* This insn needs, or can use, some delay slots.  SLOTS_TO_FILL
	 says how many.  After initialization, first try optimizing

	 call _foo		call _foo
	 nop			add %o7,.-L1,%o7
	 b,a L1
	 nop

	 If this case applies, the delay slot of the call is filled with
	 the unconditional jump.  This is done first to avoid having the
	 delay slot of the call filled in the backward scan.  Also, since
	 the unconditional jump is likely to also have a delay slot, that
	 insn must exist when it is subsequently scanned.

	 This is tried on each insn with delay slots as some machines
	 have insns which perform calls, but are not represented as 
	 CALL_INSNs.  */

      slots_filled = 0;
      delay_list = 0;

      if ((trial = next_active_insn (insn))
	  && GET_CODE (trial) == JUMP_INSN
	  && simplejump_p (trial)
	  && eligible_for_delay (insn, slots_filled, trial, flags)
	  && no_labels_between_p (insn, trial))
	{
	  rtx *tmp;
	  slots_filled++;
	  delay_list = add_to_delay_list (trial, delay_list);

	  /* TRIAL may have had its delay slot filled, then unfilled.  When
	     the delay slot is unfilled, TRIAL is placed back on the unfilled
	     slots obstack.  Unfortunately, it is placed on the end of the
	     obstack, not in its original location.  Therefore, we must search
	     from entry i + 1 to the end of the unfilled slots obstack to
	     try and find TRIAL.  */
	  tmp = &unfilled_slots_base[i + 1];
	  while (*tmp != trial && tmp != unfilled_slots_next)
	    tmp++;

	  /* Remove the unconditional jump from consideration for delay slot
	     filling and unthread it.   */
	  if (*tmp == trial)
	    *tmp = 0;
	  {
	    rtx next = NEXT_INSN (trial);
	    rtx prev = PREV_INSN (trial);
	    if (prev)
	      NEXT_INSN (prev) = next;
	    if (next)
	      PREV_INSN (next) = prev;
	  }
	}

      /* Now, scan backwards from the insn to search for a potential
	 delay-slot candidate.  Stop searching when a label or jump is hit.

	 For each candidate, if it is to go into the delay slot (moved
	 forward in execution sequence), it must not need or set any resources
	 that were set by later insns and must not set any resources that
	 are needed for those insns.
	 
	 The delay slot insn itself sets resources unless it is a call
	 (in which case the called routine, not the insn itself, is doing
	 the setting).  */

      if (slots_filled < slots_to_fill)
	{
	  CLEAR_RESOURCE (&needed);
	  CLEAR_RESOURCE (&set);
	  mark_set_resources (insn, &set, 0, 0);
	  mark_referenced_resources (insn, &needed, 0);

	  for (trial = prev_nonnote_insn (insn); ! stop_search_p (trial, 1);
	       trial = next_trial)
	    {
	      next_trial = prev_nonnote_insn (trial);

	      /* This must be an INSN or CALL_INSN.  */
	      pat = PATTERN (trial);

	      /* USE and CLOBBER at this level was just for flow; ignore it.  */
	      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
		continue;

	      /* Check for resource conflict first, to avoid unnecessary 
		 splitting.  */
	      if (! insn_references_resource_p (trial, &set, 1)
		  && ! insn_sets_resource_p (trial, &set, 1)
		  && ! insn_sets_resource_p (trial, &needed, 1)
#ifdef HAVE_cc0
		  /* Can't separate set of cc0 from its use.  */
		  && ! (reg_mentioned_p (cc0_rtx, pat)
			&& ! sets_cc0_p (cc0_rtx, pat))
#endif
		  )
		{
		  trial = try_split (pat, trial, 1);
		  next_trial = prev_nonnote_insn (trial);
		  if (eligible_for_delay (insn, slots_filled, trial, flags))
		    {
		      /* In this case, we are searching backward, so if we
			 find insns to put on the delay list, we want
			 to put them at the head, rather than the
			 tail, of the list.  */

		      update_reg_dead_notes (trial, insn);
		      delay_list = gen_rtx (INSN_LIST, VOIDmode,
					    trial, delay_list);
		      update_block (trial, trial);
		      delete_insn (trial);
		      if (slots_to_fill == ++slots_filled)
			break;
		      continue;
		    }
		}

	      mark_set_resources (trial, &set, 0, 1);
	      mark_referenced_resources (trial, &needed, 1);
	    }
	}

      /* If all needed slots haven't been filled, we come here.  */

      /* Try to optimize case of jumping around a single insn.  */
#if defined(ANNUL_IFFALSE_SLOTS) || defined(ANNUL_IFTRUE_SLOTS)
      if (slots_filled != slots_to_fill
	  && delay_list == 0
	  && GET_CODE (insn) == JUMP_INSN 
	  && (condjump_p (insn) || condjump_in_parallel_p (insn)))
	{
	  delay_list = optimize_skip (insn);
	  if (delay_list)
	    slots_filled += 1;
	}
#endif

      /* Try to get insns from beyond the insn needing the delay slot.
	 These insns can neither set or reference resources set in insns being
	 skipped, cannot set resources in the insn being skipped, and, if this
	 is a CALL_INSN (or a CALL_INSN is passed), cannot trap (because the
	 call might not return).

	 There used to be code which continued past the target label if
	 we saw all uses of the target label.  This code did not work,
	 because it failed to account for some instructions which were
	 both annulled and marked as from the target.  This can happen as a
	 result of optimize_skip.  Since this code was redundant with
	 fill_eager_delay_slots anyways, it was just deleted.  */

      if (slots_filled != slots_to_fill
          && (GET_CODE (insn) != JUMP_INSN
	      || ((condjump_p (insn) || condjump_in_parallel_p (insn))
		   && ! simplejump_p (insn)
		   && JUMP_LABEL (insn) != 0)))
	{
	  rtx target = 0;
	  int maybe_never = 0;
	  struct resources needed_at_jump;

	  CLEAR_RESOURCE (&needed);
	  CLEAR_RESOURCE (&set);

	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      mark_set_resources (insn, &set, 0, 1);
	      mark_referenced_resources (insn, &needed, 1);
	      maybe_never = 1;
	    }
	  else 
	    {
	      mark_set_resources (insn, &set, 0, 1);
	      mark_referenced_resources (insn, &needed, 1);
	      if (GET_CODE (insn) == JUMP_INSN)
		target = JUMP_LABEL (insn);
	    }

	  for (trial = next_nonnote_insn (insn); trial; trial = next_trial)
	    {
	      rtx pat, trial_delay;

	      next_trial = next_nonnote_insn (trial);

	      if (GET_CODE (trial) == CODE_LABEL
		  || GET_CODE (trial) == BARRIER)
		break;

	      /* We must have an INSN, JUMP_INSN, or CALL_INSN.  */
	      pat = PATTERN (trial);

	      /* Stand-alone USE and CLOBBER are just for flow.  */
	      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
		continue;

	      /* If this already has filled delay slots, get the insn needing
		 the delay slots.  */
	      if (GET_CODE (pat) == SEQUENCE)
		trial_delay = XVECEXP (pat, 0, 0);
	      else
		trial_delay = trial;

	      /* If this is a jump insn to our target, indicate that we have
		 seen another jump to it.  If we aren't handling a conditional
		 jump, stop our search. Otherwise, compute the needs at its
		 target and add them to NEEDED.  */
	      if (GET_CODE (trial_delay) == JUMP_INSN)
		{
		  if (target == 0)
		    break;
		  else if (JUMP_LABEL (trial_delay) != target)
		    {
		      mark_target_live_regs
			(next_active_insn (JUMP_LABEL (trial_delay)),
			 &needed_at_jump);
		      needed.memory |= needed_at_jump.memory;
		      needed.unch_memory |= needed_at_jump.unch_memory;
		      IOR_HARD_REG_SET (needed.regs, needed_at_jump.regs);
		    }
		}

	      /* See if we have a resource problem before we try to
		 split.   */
	      if (target == 0
		  && GET_CODE (pat) != SEQUENCE
		  && ! insn_references_resource_p (trial, &set, 1)
		  && ! insn_sets_resource_p (trial, &set, 1)
		  && ! insn_sets_resource_p (trial, &needed, 1)
#ifdef HAVE_cc0
		  && ! (reg_mentioned_p (cc0_rtx, pat) && ! sets_cc0_p (pat))
#endif
		  && ! (maybe_never && may_trap_p (pat))
		  && (trial = try_split (pat, trial, 0))
		  && eligible_for_delay (insn, slots_filled, trial, flags))
		{
		  next_trial = next_nonnote_insn (trial);
		  delay_list = add_to_delay_list (trial, delay_list);

#ifdef HAVE_cc0
		  if (reg_mentioned_p (cc0_rtx, pat))
		    link_cc0_insns (trial);
#endif

		  delete_insn (trial);
		  if (slots_to_fill == ++slots_filled)
		    break;
		  continue;
		}

	      mark_set_resources (trial, &set, 0, 1);
	      mark_referenced_resources (trial, &needed, 1);

	      /* Ensure we don't put insns between the setting of cc and the
		 comparison by moving a setting of cc into an earlier delay
		 slot since these insns could clobber the condition code.  */
	      set.cc = 1;

	      /* If this is a call or jump, we might not get here.  */
	      if (GET_CODE (trial_delay) == CALL_INSN
		  || GET_CODE (trial_delay) == JUMP_INSN)
		maybe_never = 1;
	    }

	  /* If there are slots left to fill and our search was stopped by an
	     unconditional branch, try the insn at the branch target.  We can
	     redirect the branch if it works. 

	     Don't do this if the insn at the branch target is a branch.  */
	  if (slots_to_fill != slots_filled
	      && trial
	      && GET_CODE (trial) == JUMP_INSN
	      && simplejump_p (trial)
	      && (target == 0 || JUMP_LABEL (trial) == target)
	      && (next_trial = next_active_insn (JUMP_LABEL (trial))) != 0
	      && ! (GET_CODE (next_trial) == INSN
		    && GET_CODE (PATTERN (next_trial)) == SEQUENCE)
	      && GET_CODE (next_trial) != JUMP_INSN
	      && ! insn_references_resource_p (next_trial, &set, 1)
	      && ! insn_sets_resource_p (next_trial, &set, 1)
	      && ! insn_sets_resource_p (next_trial, &needed, 1)
#ifdef HAVE_cc0
	      && ! reg_mentioned_p (cc0_rtx, PATTERN (next_trial))
#endif
	      && ! (maybe_never && may_trap_p (PATTERN (next_trial)))
	      && (next_trial = try_split (PATTERN (next_trial), next_trial, 0))
	      && eligible_for_delay (insn, slots_filled, next_trial, flags))
	    {
	      rtx new_label = next_active_insn (next_trial);

	      if (new_label != 0)
		new_label = get_label_before (new_label);
	      else
		new_label = find_end_label ();

	      delay_list 
		= add_to_delay_list (copy_rtx (next_trial), delay_list);
	      slots_filled++;
	      reorg_redirect_jump (trial, new_label);

	      /* If we merged because we both jumped to the same place,
		 redirect the original insn also.  */
	      if (target)
		reorg_redirect_jump (insn, new_label);
	    }
	}

      if (delay_list)
	unfilled_slots_base[i]
	  = emit_delay_sequence (insn, delay_list,
				 slots_filled, slots_to_fill);

      if (slots_to_fill == slots_filled)
	unfilled_slots_base[i] = 0;

      note_delay_statistics (slots_filled, 0);
    }

#ifdef DELAY_SLOTS_FOR_EPILOGUE
  /* See if the epilogue needs any delay slots.  Try to fill them if so.
     The only thing we can do is scan backwards from the end of the 
     function.  If we did this in a previous pass, it is incorrect to do it
     again.  */
  if (current_function_epilogue_delay_list)
    return;

  slots_to_fill = DELAY_SLOTS_FOR_EPILOGUE;
  if (slots_to_fill == 0)
    return;

  slots_filled = 0;
  CLEAR_RESOURCE (&set);

  /* The frame pointer and stack pointer are needed at the beginning of
     the epilogue, so instructions setting them can not be put in the
     epilogue delay slot.  However, everything else needed at function
     end is safe, so we don't want to use end_of_function_needs here.  */
  CLEAR_RESOURCE (&needed);
  if (frame_pointer_needed)
    {
      SET_HARD_REG_BIT (needed.regs, FRAME_POINTER_REGNUM);
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
      SET_HARD_REG_BIT (needed.regs, HARD_FRAME_POINTER_REGNUM);
#endif
#ifdef EXIT_IGNORE_STACK
      if (! EXIT_IGNORE_STACK)
#endif
	SET_HARD_REG_BIT (needed.regs, STACK_POINTER_REGNUM);
    }
  else
    SET_HARD_REG_BIT (needed.regs, STACK_POINTER_REGNUM);

  for (trial = get_last_insn (); ! stop_search_p (trial, 1);
       trial = PREV_INSN (trial))
    {
      if (GET_CODE (trial) == NOTE)
	continue;
      pat = PATTERN (trial);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
	continue;

      if (! insn_references_resource_p (trial, &set, 1)
	  && ! insn_sets_resource_p (trial, &needed, 1)
	  && ! insn_sets_resource_p (trial, &set, 1)
#ifdef HAVE_cc0
	  /* Don't want to mess with cc0 here.  */
	  && ! reg_mentioned_p (cc0_rtx, pat)
#endif
	  )
	{
	  trial = try_split (pat, trial, 1);
	  if (ELIGIBLE_FOR_EPILOGUE_DELAY (trial, slots_filled))
	    {
	      /* Here as well we are searching backward, so put the
		 insns we find on the head of the list.  */

	      current_function_epilogue_delay_list
		= gen_rtx (INSN_LIST, VOIDmode, trial,
			   current_function_epilogue_delay_list);
	      mark_referenced_resources (trial, &end_of_function_needs, 1);
	      update_block (trial, trial);
	      delete_insn (trial);

	      /* Clear deleted bit so final.c will output the insn.  */
	      INSN_DELETED_P (trial) = 0;

	      if (slots_to_fill == ++slots_filled)
		break;
	      continue;
	    }
	}

      mark_set_resources (trial, &set, 0, 1);
      mark_referenced_resources (trial, &needed, 1);
    }

  note_delay_statistics (slots_filled, 0);
#endif
}

/* Try to find insns to place in delay slots.

   INSN is the jump needing SLOTS_TO_FILL delay slots.  It tests CONDITION
   or is an unconditional branch if CONDITION is const_true_rtx.
   *PSLOTS_FILLED is updated with the number of slots that we have filled.

   THREAD is a flow-of-control, either the insns to be executed if the
   branch is true or if the branch is false, THREAD_IF_TRUE says which.

   OPPOSITE_THREAD is the thread in the opposite direction.  It is used
   to see if any potential delay slot insns set things needed there.

   LIKELY is non-zero if it is extremely likely that the branch will be
   taken and THREAD_IF_TRUE is set.  This is used for the branch at the
   end of a loop back up to the top.

   OWN_THREAD and OWN_OPPOSITE_THREAD are true if we are the only user of the
   thread.  I.e., it is the fallthrough code of our jump or the target of the
   jump when we are the only jump going there.

   If OWN_THREAD is false, it must be the "true" thread of a jump.  In that
   case, we can only take insns from the head of the thread for our delay
   slot.  We then adjust the jump to point after the insns we have taken.  */

static rtx
fill_slots_from_thread (insn, condition, thread, opposite_thread, likely,
			thread_if_true, own_thread, own_opposite_thread,
			slots_to_fill, pslots_filled)
     rtx insn;
     rtx condition;
     rtx thread, opposite_thread;
     int likely;
     int thread_if_true;
     int own_thread, own_opposite_thread;
     int slots_to_fill, *pslots_filled;
{
  rtx new_thread;
  rtx delay_list = 0;
  struct resources opposite_needed, set, needed;
  rtx trial;
  int lose = 0;
  int must_annul = 0;
  int flags;

  /* Validate our arguments.  */
  if ((condition == const_true_rtx && ! thread_if_true)
      || (! own_thread && ! thread_if_true))
    abort ();

  flags = get_jump_flags (insn, JUMP_LABEL (insn));

  /* If our thread is the end of subroutine, we can't get any delay
     insns from that.  */
  if (thread == 0)
    return 0;

  /* If this is an unconditional branch, nothing is needed at the
     opposite thread.  Otherwise, compute what is needed there.  */
  if (condition == const_true_rtx)
    CLEAR_RESOURCE (&opposite_needed);
  else
    mark_target_live_regs (opposite_thread, &opposite_needed);

  /* If the insn at THREAD can be split, do it here to avoid having to
     update THREAD and NEW_THREAD if it is done in the loop below.  Also
     initialize NEW_THREAD.  */

  new_thread = thread = try_split (PATTERN (thread), thread, 0);

  /* Scan insns at THREAD.  We are looking for an insn that can be removed
     from THREAD (it neither sets nor references resources that were set
     ahead of it and it doesn't set anything needs by the insns ahead of
     it) and that either can be placed in an annulling insn or aren't
     needed at OPPOSITE_THREAD.  */

  CLEAR_RESOURCE (&needed);
  CLEAR_RESOURCE (&set);

  /* If we do not own this thread, we must stop as soon as we find
     something that we can't put in a delay slot, since all we can do
     is branch into THREAD at a later point.  Therefore, labels stop
     the search if this is not the `true' thread.  */

  for (trial = thread;
       ! stop_search_p (trial, ! thread_if_true) && (! lose || own_thread);
       trial = next_nonnote_insn (trial))
    {
      rtx pat, old_trial;

      /* If we have passed a label, we no longer own this thread.  */
      if (GET_CODE (trial) == CODE_LABEL)
	{
	  own_thread = 0;
	  continue;
	}

      pat = PATTERN (trial);
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
	continue;

      /* If TRIAL conflicts with the insns ahead of it, we lose.  Also,
	 don't separate or copy insns that set and use CC0.  */
      if (! insn_references_resource_p (trial, &set, 1)
	  && ! insn_sets_resource_p (trial, &set, 1)
	  && ! insn_sets_resource_p (trial, &needed, 1)
#ifdef HAVE_cc0
	  && ! (reg_mentioned_p (cc0_rtx, pat)
		&& (! own_thread || ! sets_cc0_p (pat)))
#endif
	  )
	{
	  rtx prior_insn;

	  /* If TRIAL is redundant with some insn before INSN, we don't
	     actually need to add it to the delay list; we can merely pretend
	     we did.  */
	  if (prior_insn = redundant_insn (trial, insn, delay_list))
	    {
	      if (own_thread)
		{
		  update_block (trial, thread);
		  if (trial == thread)
		    {
		      thread = next_active_insn (thread);
		      if (new_thread == trial)
			new_thread = thread;
		    }

		  delete_insn (trial);
		}
	      else
		{
		  update_reg_unused_notes (prior_insn, trial);
		  new_thread = next_active_insn (trial);
		}

	      continue;
	    }

	  /* There are two ways we can win:  If TRIAL doesn't set anything
	     needed at the opposite thread and can't trap, or if it can
	     go into an annulled delay slot.  */
	  if (condition == const_true_rtx
	      || (! insn_sets_resource_p (trial, &opposite_needed, 1)
		  && ! may_trap_p (pat)))
	    {
	      old_trial = trial;
	      trial = try_split (pat, trial, 0);
	      if (new_thread == old_trial)
		new_thread = trial;
	      if (thread == old_trial)
		thread = trial;
	      pat = PATTERN (trial);
	      if (eligible_for_delay (insn, *pslots_filled, trial, flags))
		goto winner;
	    }
	  else if (0
#ifdef ANNUL_IFTRUE_SLOTS
		   || ! thread_if_true
#endif
#ifdef ANNUL_IFFALSE_SLOTS
		   || thread_if_true
#endif
		   )
	    {
	      old_trial = trial;
	      trial = try_split (pat, trial, 0);
	      if (new_thread == old_trial)
		new_thread = trial;
	      if (thread == old_trial)
		thread = trial;
	      pat = PATTERN (trial);
	      if ((thread_if_true
		   ? eligible_for_annul_false (insn, *pslots_filled, trial, flags)
		   : eligible_for_annul_true (insn, *pslots_filled, trial, flags)))
		{
		  rtx temp;

		  must_annul = 1;
		winner:

#ifdef HAVE_cc0
		  if (reg_mentioned_p (cc0_rtx, pat))
		    link_cc0_insns (trial);
#endif

		  /* If we own this thread, delete the insn.  If this is the
		     destination of a branch, show that a basic block status
		     may have been updated.  In any case, mark the new
		     starting point of this thread.  */
		  if (own_thread)
		    {
		      update_block (trial, thread);
		      delete_insn (trial);
		    }
		  else
		    new_thread = next_active_insn (trial);

		  temp = own_thread ? trial : copy_rtx (trial);
		  if (thread_if_true)
		    INSN_FROM_TARGET_P (temp) = 1;

		  delay_list = add_to_delay_list (temp, delay_list);

		  if (slots_to_fill == ++(*pslots_filled))
		    {
		      /* Even though we have filled all the slots, we
			 may be branching to a location that has a
			 redundant insn.  Skip any if so.  */
		      while (new_thread && ! own_thread
			     && ! insn_sets_resource_p (new_thread, &set, 1)
			     && ! insn_sets_resource_p (new_thread, &needed, 1)
			     && ! insn_references_resource_p (new_thread,
							      &set, 1)
			     && redundant_insn (new_thread, insn, delay_list))
			new_thread = next_active_insn (new_thread);
		      break;
		    }

		  continue;
		}
	    }
	}

      /* This insn can't go into a delay slot.  */
      lose = 1;
      mark_set_resources (trial, &set, 0, 1);
      mark_referenced_resources (trial, &needed, 1);

      /* Ensure we don't put insns between the setting of cc and the comparison
	 by moving a setting of cc into an earlier delay slot since these insns
	 could clobber the condition code.  */
      set.cc = 1;

      /* If this insn is a register-register copy and the next insn has
	 a use of our destination, change it to use our source.  That way,
	 it will become a candidate for our delay slot the next time
	 through this loop.  This case occurs commonly in loops that
	 scan a list.

	 We could check for more complex cases than those tested below,
	 but it doesn't seem worth it.  It might also be a good idea to try
	 to swap the two insns.  That might do better.

	 We can't do this if the next insn modifies our destination, because
	 that would make the replacement into the insn invalid.  We also can't
	 do this if it modifies our source, because it might be an earlyclobber
	 operand.  This latter test also prevents updating the contents of
	 a PRE_INC.  */

      if (GET_CODE (trial) == INSN && GET_CODE (pat) == SET
	  && GET_CODE (SET_SRC (pat)) == REG
	  && GET_CODE (SET_DEST (pat)) == REG)
	{
	  rtx next = next_nonnote_insn (trial);

	  if (next && GET_CODE (next) == INSN
	      && GET_CODE (PATTERN (next)) != USE
	      && ! reg_set_p (SET_DEST (pat), next)
	      && ! reg_set_p (SET_SRC (pat), next)
	      && reg_referenced_p (SET_DEST (pat), PATTERN (next)))
	    validate_replace_rtx (SET_DEST (pat), SET_SRC (pat), next);
	}
    }

  /* If we stopped on a branch insn that has delay slots, see if we can
     steal some of the insns in those slots.  */
  if (trial && GET_CODE (trial) == INSN
      && GET_CODE (PATTERN (trial)) == SEQUENCE
      && GET_CODE (XVECEXP (PATTERN (trial), 0, 0)) == JUMP_INSN)
    {
      /* If this is the `true' thread, we will want to follow the jump,
	 so we can only do this if we have taken everything up to here.  */
      if (thread_if_true && trial == new_thread)
	delay_list
	  = steal_delay_list_from_target (insn, condition, PATTERN (trial),
					  delay_list, &set, &needed,
					  &opposite_needed, slots_to_fill,
					  pslots_filled, &must_annul,
					  &new_thread);
      else if (! thread_if_true)
	delay_list
	  = steal_delay_list_from_fallthrough (insn, condition,
					       PATTERN (trial),
					       delay_list, &set, &needed,
					       &opposite_needed, slots_to_fill,
					       pslots_filled, &must_annul);
    }

  /* If we haven't found anything for this delay slot and it is very
     likely that the branch will be taken, see if the insn at our target
     increments or decrements a register with an increment that does not
     depend on the destination register.  If so, try to place the opposite
     arithmetic insn after the jump insn and put the arithmetic insn in the
     delay slot.  If we can't do this, return.  */
  if (delay_list == 0 && likely && new_thread && GET_CODE (new_thread) == INSN)
    {
      rtx pat = PATTERN (new_thread);
      rtx dest;
      rtx src;

      trial = new_thread;
      pat = PATTERN (trial);

      if (GET_CODE (trial) != INSN || GET_CODE (pat) != SET
	  || ! eligible_for_delay (insn, 0, trial, flags))
	return 0;

      dest = SET_DEST (pat), src = SET_SRC (pat);
      if ((GET_CODE (src) == PLUS || GET_CODE (src) == MINUS)
	  && rtx_equal_p (XEXP (src, 0), dest)
	  && ! reg_overlap_mentioned_p (dest, XEXP (src, 1)))
	{
	  rtx other = XEXP (src, 1);
	  rtx new_arith;
	  rtx ninsn;

	  /* If this is a constant adjustment, use the same code with
	     the negated constant.  Otherwise, reverse the sense of the
	     arithmetic.  */
	  if (GET_CODE (other) == CONST_INT)
	    new_arith = gen_rtx (GET_CODE (src), GET_MODE (src), dest,
				 negate_rtx (GET_MODE (src), other));
	  else
	    new_arith = gen_rtx (GET_CODE (src) == PLUS ? MINUS : PLUS,
				 GET_MODE (src), dest, other);

	  ninsn = emit_insn_after (gen_rtx (SET, VOIDmode, dest, new_arith),
				   insn);

	  if (recog_memoized (ninsn) < 0
	      || (insn_extract (ninsn),
		  ! constrain_operands (INSN_CODE (ninsn), 1)))
	    {
	      delete_insn (ninsn);
	      return 0;
	    }

	  if (own_thread)
	    {
	      update_block (trial, thread);
	      delete_insn (trial);
	    }
	  else
	    new_thread = next_active_insn (trial);

	  ninsn = own_thread ? trial : copy_rtx (trial);
	  if (thread_if_true)
	    INSN_FROM_TARGET_P (ninsn) = 1;

	  delay_list = add_to_delay_list (ninsn, NULL_RTX);
	  (*pslots_filled)++;
	}
    }

  if (delay_list && must_annul)
    INSN_ANNULLED_BRANCH_P (insn) = 1;

  /* If we are to branch into the middle of this thread, find an appropriate
     label or make a new one if none, and redirect INSN to it.  If we hit the
     end of the function, use the end-of-function label.  */
  if (new_thread != thread)
    {
      rtx label;

      if (! thread_if_true)
	abort ();

      if (new_thread && GET_CODE (new_thread) == JUMP_INSN
	  && (simplejump_p (new_thread)
	      || GET_CODE (PATTERN (new_thread)) == RETURN)
	  && redirect_with_delay_list_safe_p (insn,
					      JUMP_LABEL (new_thread),
					      delay_list))
	new_thread = follow_jumps (JUMP_LABEL (new_thread));

      if (new_thread == 0)
	label = find_end_label ();
      else if (GET_CODE (new_thread) == CODE_LABEL)
	label = new_thread;
      else
	label = get_label_before (new_thread);

      reorg_redirect_jump (insn, label);
    }

  return delay_list;
}

/* Make another attempt to find insns to place in delay slots.

   We previously looked for insns located in front of the delay insn
   and, for non-jump delay insns, located behind the delay insn.

   Here only try to schedule jump insns and try to move insns from either
   the target or the following insns into the delay slot.  If annulling is
   supported, we will be likely to do this.  Otherwise, we can do this only
   if safe.  */

static void
fill_eager_delay_slots (first)
     rtx first;
{
  register rtx insn;
  register int i;
  int num_unfilled_slots = unfilled_slots_next - unfilled_slots_base;

  for (i = 0; i < num_unfilled_slots; i++)
    {
      rtx condition;
      rtx target_label, insn_at_target, fallthrough_insn;
      rtx delay_list = 0;
      int own_target;
      int own_fallthrough;
      int prediction, slots_to_fill, slots_filled;

      insn = unfilled_slots_base[i];
      if (insn == 0
	  || INSN_DELETED_P (insn)
	  || GET_CODE (insn) != JUMP_INSN
	  || ! (condjump_p (insn) || condjump_in_parallel_p (insn)))
	continue;

      slots_to_fill = num_delay_slots (insn);
      if (slots_to_fill == 0)
	abort ();

      slots_filled = 0;
      target_label = JUMP_LABEL (insn);
      condition = get_branch_condition (insn, target_label);

      if (condition == 0)
	continue;

      /* Get the next active fallthrough and target insns and see if we own
	 them.  Then see whether the branch is likely true.  We don't need
	 to do a lot of this for unconditional branches.  */

      insn_at_target = next_active_insn (target_label);
      own_target = own_thread_p (target_label, target_label, 0);

      if (condition == const_true_rtx)
	{
	  own_fallthrough = 0;
	  fallthrough_insn = 0;
	  prediction = 2;
	}
      else
	{
	  fallthrough_insn = next_active_insn (insn);
	  own_fallthrough = own_thread_p (NEXT_INSN (insn), NULL_RTX, 1);
	  prediction = mostly_true_jump (insn, condition);
	}

      /* If this insn is expected to branch, first try to get insns from our
	 target, then our fallthrough insns.  If it is not, expected to branch,
	 try the other order.  */

      if (prediction > 0)
	{
	  delay_list
	    = fill_slots_from_thread (insn, condition, insn_at_target,
				      fallthrough_insn, prediction == 2, 1,
				      own_target, own_fallthrough,
				      slots_to_fill, &slots_filled);

	  if (delay_list == 0 && own_fallthrough)
	    {
	      /* Even though we didn't find anything for delay slots,
		 we might have found a redundant insn which we deleted
		 from the thread that was filled.  So we have to recompute
		 the next insn at the target.  */
	      target_label = JUMP_LABEL (insn);
	      insn_at_target = next_active_insn (target_label);

	      delay_list
		= fill_slots_from_thread (insn, condition, fallthrough_insn,
					  insn_at_target, 0, 0,
					  own_fallthrough, own_target,
					  slots_to_fill, &slots_filled);
	    }
	}
      else
	{
	  if (own_fallthrough)
	    delay_list
	      = fill_slots_from_thread (insn, condition, fallthrough_insn,
					insn_at_target, 0, 0,
					own_fallthrough, own_target,
					slots_to_fill, &slots_filled);

	  if (delay_list == 0)
	    delay_list
	      = fill_slots_from_thread (insn, condition, insn_at_target,
					next_active_insn (insn), 0, 1,
					own_target, own_fallthrough,
					slots_to_fill, &slots_filled);
	}

      if (delay_list)
	unfilled_slots_base[i]
	  = emit_delay_sequence (insn, delay_list,
				 slots_filled, slots_to_fill);

      if (slots_to_fill == slots_filled)
	unfilled_slots_base[i] = 0;

      note_delay_statistics (slots_filled, 1);
    }
}

/* Once we have tried two ways to fill a delay slot, make a pass over the
   code to try to improve the results and to do such things as more jump
   threading.  */

static void
relax_delay_slots (first)
     rtx first;
{
  register rtx insn, next, pat;
  register rtx trial, delay_insn, target_label;

  /* Look at every JUMP_INSN and see if we can improve it.  */
  for (insn = first; insn; insn = next)
    {
      rtx other;

      next = next_active_insn (insn);

      /* If this is a jump insn, see if it now jumps to a jump, jumps to
	 the next insn, or jumps to a label that is not the last of a
	 group of consecutive labels.  */
      if (GET_CODE (insn) == JUMP_INSN
	  && (condjump_p (insn) || condjump_in_parallel_p (insn))
	  && (target_label = JUMP_LABEL (insn)) != 0)
	{
	  target_label = follow_jumps (target_label);
	  target_label = prev_label (next_active_insn (target_label));

	  if (target_label == 0)
	    target_label = find_end_label ();

	  if (next_active_insn (target_label) == next
	      && ! condjump_in_parallel_p (insn))
	    {
	      delete_jump (insn);
	      continue;
	    }

	  if (target_label != JUMP_LABEL (insn))
	    reorg_redirect_jump (insn, target_label);

	  /* See if this jump branches around a unconditional jump.
	     If so, invert this jump and point it to the target of the
	     second jump.  */
	  if (next && GET_CODE (next) == JUMP_INSN
	      && (simplejump_p (next) || GET_CODE (PATTERN (next)) == RETURN)
	      && next_active_insn (target_label) == next_active_insn (next)
	      && no_labels_between_p (insn, next))
	    {
	      rtx label = JUMP_LABEL (next);

	      /* Be careful how we do this to avoid deleting code or
		 labels that are momentarily dead.  See similar optimization
		 in jump.c.

		 We also need to ensure we properly handle the case when
		 invert_jump fails.  */

	      ++LABEL_NUSES (target_label);
	      if (label)
		++LABEL_NUSES (label);

	      if (invert_jump (insn, label))
		{
		  delete_insn (next);
		  next = insn;
		}

	      if (label)
		--LABEL_NUSES (label);

	      if (--LABEL_NUSES (target_label) == 0)
		delete_insn (target_label);

	      continue;
	    }
	}
	  
      /* If this is an unconditional jump and the previous insn is a
	 conditional jump, try reversing the condition of the previous
	 insn and swapping our targets.  The next pass might be able to
	 fill the slots.

	 Don't do this if we expect the conditional branch to be true, because
	 we would then be making the more common case longer.  */

      if (GET_CODE (insn) == JUMP_INSN
	  && (simplejump_p (insn) || GET_CODE (PATTERN (insn)) == RETURN)
	  && (other = prev_active_insn (insn)) != 0
	  && (condjump_p (other) || condjump_in_parallel_p (other))
	  && no_labels_between_p (other, insn)
	  && 0 < mostly_true_jump (other,
				   get_branch_condition (other,
							 JUMP_LABEL (other))))
	{
	  rtx other_target = JUMP_LABEL (other);
	  target_label = JUMP_LABEL (insn);

	  /* Increment the count of OTHER_TARGET, so it doesn't get deleted
	     as we move the label.  */
	  if (other_target)
	    ++LABEL_NUSES (other_target);

	  if (invert_jump (other, target_label))
	    reorg_redirect_jump (insn, other_target);

	  if (other_target)
	    --LABEL_NUSES (other_target);
	}

      /* Now look only at cases where we have filled a delay slot.  */
      if (GET_CODE (insn) != INSN
	  || GET_CODE (PATTERN (insn)) != SEQUENCE)
	continue;

      pat = PATTERN (insn);
      delay_insn = XVECEXP (pat, 0, 0);

      /* See if the first insn in the delay slot is redundant with some
	 previous insn.  Remove it from the delay slot if so; then set up
	 to reprocess this insn.  */
      if (redundant_insn (XVECEXP (pat, 0, 1), delay_insn, 0))
	{
	  delete_from_delay_slot (XVECEXP (pat, 0, 1));
	  next = prev_active_insn (next);
	  continue;
	}

      /* Now look only at the cases where we have a filled JUMP_INSN.  */
      if (GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) != JUMP_INSN
	  || ! (condjump_p (XVECEXP (PATTERN (insn), 0, 0))
		|| condjump_in_parallel_p (XVECEXP (PATTERN (insn), 0, 0))))
	continue;

      target_label = JUMP_LABEL (delay_insn);

      if (target_label)
	{
	  /* If this jump goes to another unconditional jump, thread it, but
	     don't convert a jump into a RETURN here.  */
	  trial = follow_jumps (target_label);
	  /* We use next_real_insn instead of next_active_insn, so that
	     the special USE insns emitted by reorg won't be ignored.
	     If they are ignored, then they will get deleted if target_label
	     is now unreachable, and that would cause mark_target_live_regs
	     to fail.  */
	  trial = prev_label (next_real_insn (trial));
	  if (trial == 0 && target_label != 0)
	    trial = find_end_label ();

	  if (trial != target_label 
	      && redirect_with_delay_slots_safe_p (delay_insn, trial, insn))
	    {
	      reorg_redirect_jump (delay_insn, trial);
	      target_label = trial;
	    }

	  /* If the first insn at TARGET_LABEL is redundant with a previous
	     insn, redirect the jump to the following insn process again.  */
	  trial = next_active_insn (target_label);
	  if (trial && GET_CODE (PATTERN (trial)) != SEQUENCE
	      && redundant_insn (trial, insn, 0))
	    {
	      trial = next_active_insn (trial);
	      if (trial == 0)
		target_label = find_end_label ();
	      else
		target_label = get_label_before (trial);
	      reorg_redirect_jump (delay_insn, target_label);
	      next = insn;
	      continue;
	    }

	  /* Similarly, if it is an unconditional jump with one insn in its
	     delay list and that insn is redundant, thread the jump.  */
	  if (trial && GET_CODE (PATTERN (trial)) == SEQUENCE
	      && XVECLEN (PATTERN (trial), 0) == 2
	      && GET_CODE (XVECEXP (PATTERN (trial), 0, 0)) == JUMP_INSN
	      && (simplejump_p (XVECEXP (PATTERN (trial), 0, 0))
		  || GET_CODE (PATTERN (XVECEXP (PATTERN (trial), 0, 0))) == RETURN)
	      && redundant_insn (XVECEXP (PATTERN (trial), 0, 1), insn, 0))
	    {
	      target_label = JUMP_LABEL (XVECEXP (PATTERN (trial), 0, 0));
	      if (target_label == 0)
		target_label = find_end_label ();

	      if (redirect_with_delay_slots_safe_p (delay_insn, target_label, 
						    insn))
		{
		  reorg_redirect_jump (delay_insn, target_label);
		  next = insn;
		  continue;
		}
	    }
	}

      if (! INSN_ANNULLED_BRANCH_P (delay_insn)
	  && prev_active_insn (target_label) == insn
	  && ! condjump_in_parallel_p (delay_insn)
#ifdef HAVE_cc0
	  /* If the last insn in the delay slot sets CC0 for some insn,
	     various code assumes that it is in a delay slot.  We could
	     put it back where it belonged and delete the register notes,
	     but it doesn't seem worthwhile in this uncommon case.  */
	  && ! find_reg_note (XVECEXP (pat, 0, XVECLEN (pat, 0) - 1),
			      REG_CC_USER, NULL_RTX)
#endif
	  )
	{
	  int i;

	  /* All this insn does is execute its delay list and jump to the
	     following insn.  So delete the jump and just execute the delay
	     list insns.

	     We do this by deleting the INSN containing the SEQUENCE, then
	     re-emitting the insns separately, and then deleting the jump.
	     This allows the count of the jump target to be properly
	     decremented.  */

	  /* Clear the from target bit, since these insns are no longer
	     in delay slots.  */
	  for (i = 0; i < XVECLEN (pat, 0); i++)
	    INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)) = 0;

	  trial = PREV_INSN (insn);
	  delete_insn (insn);
	  emit_insn_after (pat, trial);
	  delete_scheduled_jump (delay_insn);
	  continue;
	}

      /* See if this is an unconditional jump around a single insn which is
	 identical to the one in its delay slot.  In this case, we can just
	 delete the branch and the insn in its delay slot.  */
      if (next && GET_CODE (next) == INSN
	  && prev_label (next_active_insn (next)) == target_label
	  && simplejump_p (insn)
	  && XVECLEN (pat, 0) == 2
	  && rtx_equal_p (PATTERN (next), PATTERN (XVECEXP (pat, 0, 1))))
	{
	  delete_insn (insn);
	  continue;
	}

      /* See if this jump (with its delay slots) branches around another
	 jump (without delay slots).  If so, invert this jump and point
	 it to the target of the second jump.  We cannot do this for
	 annulled jumps, though.  Again, don't convert a jump to a RETURN
	 here.  */
      if (! INSN_ANNULLED_BRANCH_P (delay_insn)
	  && next && GET_CODE (next) == JUMP_INSN
	  && (simplejump_p (next) || GET_CODE (PATTERN (next)) == RETURN)
	  && next_active_insn (target_label) == next_active_insn (next)
	  && no_labels_between_p (insn, next))
	{
	  rtx label = JUMP_LABEL (next);
	  rtx old_label = JUMP_LABEL (delay_insn);

	  if (label == 0)
	    label = find_end_label ();

	  if (redirect_with_delay_slots_safe_p (delay_insn, label, insn))
	    {
	      /* Be careful how we do this to avoid deleting code or labels
		 that are momentarily dead.  See similar optimization in
		 jump.c  */
	      if (old_label)
		++LABEL_NUSES (old_label);

	      if (invert_jump (delay_insn, label))
		{
		  int i;

		  /* Must update the INSN_FROM_TARGET_P bits now that
		     the branch is reversed, so that mark_target_live_regs
		     will handle the delay slot insn correctly.  */
		  for (i = 1; i < XVECLEN (PATTERN (insn), 0); i++)
		    {
		      rtx slot = XVECEXP (PATTERN (insn), 0, i);
		      INSN_FROM_TARGET_P (slot) = ! INSN_FROM_TARGET_P (slot);
		    }

		  delete_insn (next);
		  next = insn;
		}

	      if (old_label && --LABEL_NUSES (old_label) == 0)
		delete_insn (old_label);
	      continue;
	    }
	}

      /* If we own the thread opposite the way this insn branches, see if we
	 can merge its delay slots with following insns.  */
      if (INSN_FROM_TARGET_P (XVECEXP (pat, 0, 1))
	  && own_thread_p (NEXT_INSN (insn), 0, 1))
	try_merge_delay_insns (insn, next);
      else if (! INSN_FROM_TARGET_P (XVECEXP (pat, 0, 1))
	       && own_thread_p (target_label, target_label, 0))
	try_merge_delay_insns (insn, next_active_insn (target_label));

      /* If we get here, we haven't deleted INSN.  But we may have deleted
	 NEXT, so recompute it.  */
      next = next_active_insn (insn);
    }
}

#ifdef HAVE_return

/* Look for filled jumps to the end of function label.  We can try to convert
   them into RETURN insns if the insns in the delay slot are valid for the
   RETURN as well.  */

static void
make_return_insns (first)
     rtx first;
{
  rtx insn, jump_insn, pat;
  rtx real_return_label = end_of_function_label;
  int slots, i;

  /* See if there is a RETURN insn in the function other than the one we
     made for END_OF_FUNCTION_LABEL.  If so, set up anything we can't change
     into a RETURN to jump to it.  */
  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == JUMP_INSN && GET_CODE (PATTERN (insn)) == RETURN)
      {
	real_return_label = get_label_before (insn);
	break;
      }
  
  /* Show an extra usage of REAL_RETURN_LABEL so it won't go away if it
     was equal to END_OF_FUNCTION_LABEL.  */
  LABEL_NUSES (real_return_label)++;

  /* Clear the list of insns to fill so we can use it.  */
  obstack_free (&unfilled_slots_obstack, unfilled_firstobj);

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      int flags;

      /* Only look at filled JUMP_INSNs that go to the end of function
	 label.  */
      if (GET_CODE (insn) != INSN
	  || GET_CODE (PATTERN (insn)) != SEQUENCE
	  || GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) != JUMP_INSN
	  || JUMP_LABEL (XVECEXP (PATTERN (insn), 0, 0)) != end_of_function_label)
	continue;

      pat = PATTERN (insn);
      jump_insn = XVECEXP (pat, 0, 0);

      /* If we can't make the jump into a RETURN, try to redirect it to the best
	 RETURN and go on to the next insn.  */
      if (! reorg_redirect_jump (jump_insn, NULL_RTX))
	{
	  /* Make sure redirecting the jump will not invalidate the delay
	     slot insns.  */
	  if (redirect_with_delay_slots_safe_p (jump_insn,
						real_return_label,
						insn))
	    reorg_redirect_jump (jump_insn, real_return_label);
	  continue;
	}

      /* See if this RETURN can accept the insns current in its delay slot.
	 It can if it has more or an equal number of slots and the contents
	 of each is valid.  */

      flags = get_jump_flags (jump_insn, JUMP_LABEL (jump_insn));
      slots = num_delay_slots (jump_insn);
      if (slots >= XVECLEN (pat, 0) - 1)
	{
	  for (i = 1; i < XVECLEN (pat, 0); i++)
	    if (! (
#ifdef ANNUL_IFFALSE_SLOTS
		   (INSN_ANNULLED_BRANCH_P (jump_insn)
		    && INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)))
		   ? eligible_for_annul_false (jump_insn, i - 1,
					       XVECEXP (pat, 0, i), flags) :
#endif
#ifdef ANNUL_IFTRUE_SLOTS
		   (INSN_ANNULLED_BRANCH_P (jump_insn)
		    && ! INSN_FROM_TARGET_P (XVECEXP (pat, 0, i)))
		   ? eligible_for_annul_true (jump_insn, i - 1,
					      XVECEXP (pat, 0, i), flags) :
#endif
		   eligible_for_delay (jump_insn, i -1, XVECEXP (pat, 0, i), flags)))
	      break;
	}
      else
	i = 0;

      if (i == XVECLEN (pat, 0))
	continue;

      /* We have to do something with this insn.  If it is an unconditional
	 RETURN, delete the SEQUENCE and output the individual insns,
	 followed by the RETURN.  Then set things up so we try to find
	 insns for its delay slots, if it needs some.  */
      if (GET_CODE (PATTERN (jump_insn)) == RETURN)
	{
	  rtx prev = PREV_INSN (insn);

	  delete_insn (insn);
	  for (i = 1; i < XVECLEN (pat, 0); i++)
	    prev = emit_insn_after (PATTERN (XVECEXP (pat, 0, i)), prev);

	  insn = emit_jump_insn_after (PATTERN (jump_insn), prev);
	  emit_barrier_after (insn);

	  if (slots)
	    obstack_ptr_grow (&unfilled_slots_obstack, insn);
	}
      else
	/* It is probably more efficient to keep this with its current
	   delay slot as a branch to a RETURN.  */
	reorg_redirect_jump (jump_insn, real_return_label);
    }

  /* Now delete REAL_RETURN_LABEL if we never used it.  Then try to fill any
     new delay slots we have created.  */
  if (--LABEL_NUSES (real_return_label) == 0)
    delete_insn (real_return_label);

  fill_simple_delay_slots (first, 1);
  fill_simple_delay_slots (first, 0);
}
#endif

/* Try to find insns to place in delay slots.  */

void
dbr_schedule (first, file)
     rtx first;
     FILE *file;
{
  rtx insn, next, epilogue_insn = 0;
  int i;
#if 0
  int old_flag_no_peephole = flag_no_peephole;

  /* Execute `final' once in prescan mode to delete any insns that won't be
     used.  Don't let final try to do any peephole optimization--it will
     ruin dataflow information for this pass.  */

  flag_no_peephole = 1;
  final (first, 0, NO_DEBUG, 1, 1);
  flag_no_peephole = old_flag_no_peephole;
#endif

  /* If the current function has no insns other than the prologue and 
     epilogue, then do not try to fill any delay slots.  */
  if (n_basic_blocks == 0)
    return;

  /* Find the highest INSN_UID and allocate and initialize our map from
     INSN_UID's to position in code.  */
  for (max_uid = 0, insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (INSN_UID (insn) > max_uid)
	max_uid = INSN_UID (insn);
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_EPILOGUE_BEG)
	epilogue_insn = insn;
    }

  uid_to_ruid = (int *) alloca ((max_uid + 1) * sizeof (int *));
  for (i = 0, insn = first; insn; i++, insn = NEXT_INSN (insn))
    uid_to_ruid[INSN_UID (insn)] = i;
  
  /* Initialize the list of insns that need filling.  */
  if (unfilled_firstobj == 0)
    {
      gcc_obstack_init (&unfilled_slots_obstack);
      unfilled_firstobj = (rtx *) obstack_alloc (&unfilled_slots_obstack, 0);
    }

  for (insn = next_active_insn (first); insn; insn = next_active_insn (insn))
    {
      rtx target;

      INSN_ANNULLED_BRANCH_P (insn) = 0;
      INSN_FROM_TARGET_P (insn) = 0;

      /* Skip vector tables.  We can't get attributes for them.  */
      if (GET_CODE (insn) == JUMP_INSN
	  && (GET_CODE (PATTERN (insn)) == ADDR_VEC
	      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC))
	continue;
    
      if (num_delay_slots (insn) > 0)
	obstack_ptr_grow (&unfilled_slots_obstack, insn);

      /* Ensure all jumps go to the last of a set of consecutive labels.  */
      if (GET_CODE (insn) == JUMP_INSN 
	  && (condjump_p (insn) || condjump_in_parallel_p (insn))
	  && JUMP_LABEL (insn) != 0
	  && ((target = prev_label (next_active_insn (JUMP_LABEL (insn))))
	      != JUMP_LABEL (insn)))
	redirect_jump (insn, target);
    }

  /* Indicate what resources are required to be valid at the end of the current
     function.  The condition code never is and memory always is.  If the
     frame pointer is needed, it is and so is the stack pointer unless
     EXIT_IGNORE_STACK is non-zero.  If the frame pointer is not needed, the
     stack pointer is.  Registers used to return the function value are
     needed.  Registers holding global variables are needed.  */

  end_of_function_needs.cc = 0;
  end_of_function_needs.memory = 1;
  end_of_function_needs.unch_memory = 0;
  CLEAR_HARD_REG_SET (end_of_function_needs.regs);

  if (frame_pointer_needed)
    {
      SET_HARD_REG_BIT (end_of_function_needs.regs, FRAME_POINTER_REGNUM);
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
      SET_HARD_REG_BIT (end_of_function_needs.regs, HARD_FRAME_POINTER_REGNUM);
#endif
#ifdef EXIT_IGNORE_STACK
      if (! EXIT_IGNORE_STACK)
#endif
	SET_HARD_REG_BIT (end_of_function_needs.regs, STACK_POINTER_REGNUM);
    }
  else
    SET_HARD_REG_BIT (end_of_function_needs.regs, STACK_POINTER_REGNUM);

  if (current_function_return_rtx != 0
      && GET_CODE (current_function_return_rtx) == REG)
    mark_referenced_resources (current_function_return_rtx,
			       &end_of_function_needs, 1);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (global_regs[i])
      SET_HARD_REG_BIT (end_of_function_needs.regs, i);

  /* The registers required to be live at the end of the function are
     represented in the flow information as being dead just prior to
     reaching the end of the function.  For example, the return of a value
     might be represented by a USE of the return register immediately
     followed by an unconditional jump to the return label where the
     return label is the end of the RTL chain.  The end of the RTL chain
     is then taken to mean that the return register is live.

     This sequence is no longer maintained when epilogue instructions are
     added to the RTL chain.  To reconstruct the original meaning, the
     start of the epilogue (NOTE_INSN_EPILOGUE_BEG) is regarded as the
     point where these registers become live (start_of_epilogue_needs).
     If epilogue instructions are present, the registers set by those
     instructions won't have been processed by flow.  Thus, those
     registers are additionally required at the end of the RTL chain
     (end_of_function_needs).  */

  start_of_epilogue_needs = end_of_function_needs;

  while (epilogue_insn = next_nonnote_insn (epilogue_insn))
    mark_set_resources (epilogue_insn, &end_of_function_needs, 0, 1);

  /* Show we haven't computed an end-of-function label yet.  */
  end_of_function_label = 0;

  /* Allocate and initialize the tables used by mark_target_live_regs.  */
  target_hash_table
    = (struct target_info **) alloca ((TARGET_HASH_PRIME
				       * sizeof (struct target_info *)));
  bzero ((char *) target_hash_table,
	 TARGET_HASH_PRIME * sizeof (struct target_info *));

  bb_ticks = (int *) alloca (n_basic_blocks * sizeof (int));
  bzero ((char *) bb_ticks, n_basic_blocks * sizeof (int));

  /* Initialize the statistics for this function.  */
  bzero ((char *) num_insns_needing_delays, sizeof num_insns_needing_delays);
  bzero ((char *) num_filled_delays, sizeof num_filled_delays);

  /* Now do the delay slot filling.  Try everything twice in case earlier
     changes make more slots fillable.  */

  for (reorg_pass_number = 0;
       reorg_pass_number < MAX_REORG_PASSES;
       reorg_pass_number++)
    {
      fill_simple_delay_slots (first, 1);
      fill_simple_delay_slots (first, 0);
      fill_eager_delay_slots (first);
      relax_delay_slots (first);
    }

  /* Delete any USE insns made by update_block; subsequent passes don't need
     them or know how to deal with them.  */
  for (insn = first; insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if (GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == USE
	  && GET_RTX_CLASS (GET_CODE (XEXP (PATTERN (insn), 0))) == 'i')
	next = delete_insn (insn);
    }

  /* If we made an end of function label, indicate that it is now
     safe to delete it by undoing our prior adjustment to LABEL_NUSES.
     If it is now unused, delete it.  */
  if (end_of_function_label && --LABEL_NUSES (end_of_function_label) == 0)
    delete_insn (end_of_function_label);

#ifdef HAVE_return
  if (HAVE_return && end_of_function_label != 0)
    make_return_insns (first);
#endif

  obstack_free (&unfilled_slots_obstack, unfilled_firstobj);

  /* It is not clear why the line below is needed, but it does seem to be.  */
  unfilled_firstobj = (rtx *) obstack_alloc (&unfilled_slots_obstack, 0);

  /* Reposition the prologue and epilogue notes in case we moved the
     prologue/epilogue insns.  */
  reposition_prologue_and_epilogue_notes (first);

  if (file)
    {
      register int i, j, need_comma;

      for (reorg_pass_number = 0;
	   reorg_pass_number < MAX_REORG_PASSES;
	   reorg_pass_number++)
	{
	  fprintf (file, ";; Reorg pass #%d:\n", reorg_pass_number + 1);
	  for (i = 0; i < NUM_REORG_FUNCTIONS; i++)
	    {
	      need_comma = 0;
	      fprintf (file, ";; Reorg function #%d\n", i);

	      fprintf (file, ";; %d insns needing delay slots\n;; ",
		       num_insns_needing_delays[i][reorg_pass_number]);

	      for (j = 0; j < MAX_DELAY_HISTOGRAM; j++)
		if (num_filled_delays[i][j][reorg_pass_number])
		  {
		    if (need_comma)
		      fprintf (file, ", ");
		    need_comma = 1;
		    fprintf (file, "%d got %d delays",
			     num_filled_delays[i][j][reorg_pass_number], j);
		  }
	      fprintf (file, "\n");
	    }
	}
    }
}
#endif /* DELAY_SLOTS */
