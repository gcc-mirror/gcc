/* Definitions for computing resource usage of specific insns.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
#include "toplev.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "function.h"
#include "regs.h"
#include "flags.h"
#include "output.h"
#include "resource.h"
#include "insn-attr.h"

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

/* Indicates what resources are required at the beginning of the epilogue.  */
static struct resources start_of_epilogue_needs;

/* Indicates what resources are required at function end.  */
static struct resources end_of_function_needs;

/* Define the hash table itself.  */
static struct target_info **target_hash_table = NULL;

/* For each basic block, we maintain a generation number of its basic
   block info, which is updated each time we move an insn from the
   target of a jump.  This is the generation number indexed by block
   number.  */

static int *bb_ticks;

/* Marks registers possibly live at the current place being scanned by
   mark_target_live_regs.  Used only by next two function.    */

static HARD_REG_SET current_live_regs;

/* Marks registers for which we have seen a REG_DEAD note but no assignment.
   Also only used by the next two functions.  */

static HARD_REG_SET pending_dead_regs;

static void update_live_status		PARAMS ((rtx, rtx, void *));
static int find_basic_block		PARAMS ((rtx));
static rtx next_insn_no_annul		PARAMS ((rtx));
static rtx find_dead_or_set_registers	PARAMS ((rtx, struct resources*,
						rtx*, int, struct resources,
						struct resources));

/* Utility function called from mark_target_live_regs via note_stores.
   It deadens any CLOBBERed registers and livens any SET registers.  */

static void
update_live_status (dest, x, data)
     rtx dest;
     rtx x;
     void *data ATTRIBUTE_UNUSED;
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
	if (insn == BLOCK_HEAD (i))
	  return i;
    }

  return -1;
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

/* Given X, some rtl, and RES, a pointer to a `struct resource', mark
   which resources are references by the insn.  If INCLUDE_DELAYED_EFFECTS
   is TRUE, resources used by the called routine will be included for
   CALL_INSNs.  */

void
mark_referenced_resources (x, res, include_delayed_effects)
     register rtx x;
     register struct resources *res;
     register int include_delayed_effects;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  unsigned int r;
  register const char *format_ptr;

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
	  unsigned int regno = REGNO (SUBREG_REG (x)) + SUBREG_WORD (x);
	  unsigned int last_regno
	    = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));

	  for (r = regno; r < last_regno; r++)
	    SET_HARD_REG_BIT (res->regs, r);
	}
      return;

    case REG:
      for (r = 0; r < HARD_REGNO_NREGS (REGNO (x), GET_MODE (x)); r++)
	SET_HARD_REG_BIT (res->regs, REGNO (x) + r);
      return;

    case MEM:
      /* If this memory shouldn't change, it really isn't referencing
	 memory.  */
      if (RTX_UNCHANGING_P (x))
	res->unch_memory = 1;
      else
	res->memory = 1;
      res->volatil |= MEM_VOLATILE_P (x);

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

    case TRAP_IF:
      res->volatil = 1;
      break;

    case ASM_OPERANDS:
      res->volatil |= MEM_VOLATILE_P (x);

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
      if (GET_CODE (x) == SIGN_EXTRACT
	  || GET_CODE (x) == ZERO_EXTRACT
	  || GET_CODE (x) == STRICT_LOW_PART)
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

	  /* If we are part of a delay slot sequence, point at the SEQUENCE.  */
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
					  XEXP (XEXP (link, 0), 0)))
			break;
		    }
		  if (i >= seq_size)
		    mark_referenced_resources (XEXP (XEXP (link, 0), 0),
					       res, 0);
		}
	  }
	}

      /* ... fall through to other INSN processing ...  */

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

    default:
      break;
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

/* A subroutine of mark_target_live_regs.  Search forward from TARGET
   looking for registers that are set before they are used.  These are dead. 
   Stop after passing a few conditional jumps, and/or a small
   number of unconditional branches.  */

static rtx
find_dead_or_set_registers (target, res, jump_target, jump_count, set, needed)
     rtx target;
     struct resources *res;
     rtx *jump_target;
     int jump_count;
     struct resources set, needed;
{
  HARD_REG_SET scratch;
  rtx insn, next;
  rtx jump_insn = 0;
  int i;

  for (insn = target; insn; insn = next)
    {
      rtx this_jump_insn = insn;

      next = NEXT_INSN (insn);
      switch (GET_CODE (insn))
	{
	case CODE_LABEL:
	  /* After a label, any pending dead registers that weren't yet
	     used can be made dead.  */
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

	default:
	  break;
	}

      if (GET_CODE (this_jump_insn) == JUMP_INSN)
	{
	  if (jump_count++ < 10)
	    {
	      if (simplejump_p (this_jump_insn)
		  || GET_CODE (PATTERN (this_jump_insn)) == RETURN)
		{
		  next = JUMP_LABEL (this_jump_insn);
		  if (jump_insn == 0)
		    {
		      jump_insn = insn;
		      if (jump_target)
			*jump_target = JUMP_LABEL (this_jump_insn);
		    }
		}
	      else if (condjump_p (this_jump_insn)
		       || condjump_in_parallel_p (this_jump_insn))
		{
		  struct resources target_set, target_res;
		  struct resources fallthrough_res;

		  /* We can handle conditional branches here by following
		     both paths, and then IOR the results of the two paths
		     together, which will give us registers that are dead
		     on both paths.  Since this is expensive, we give it
		     a much higher cost than unconditional branches.  The
		     cost was chosen so that we will follow at most 1
		     conditional branch.  */

		  jump_count += 4;
		  if (jump_count >= 10)
		    break;

		  mark_referenced_resources (insn, &needed, 1);

		  /* For an annulled branch, mark_set_resources ignores slots
		     filled by instructions from the target.  This is correct
		     if the branch is not taken.  Since we are following both
		     paths from the branch, we must also compute correct info
		     if the branch is taken.  We do this by inverting all of
		     the INSN_FROM_TARGET_P bits, calling mark_set_resources,
		     and then inverting the INSN_FROM_TARGET_P bits again.  */

		  if (GET_CODE (PATTERN (insn)) == SEQUENCE
		      && INSN_ANNULLED_BRANCH_P (this_jump_insn))
		    {
		      for (i = 1; i < XVECLEN (PATTERN (insn), 0); i++)
			INSN_FROM_TARGET_P (XVECEXP (PATTERN (insn), 0, i))
			  = ! INSN_FROM_TARGET_P (XVECEXP (PATTERN (insn), 0, i));

		      target_set = set;
		      mark_set_resources (insn, &target_set, 0, 1);

		      for (i = 1; i < XVECLEN (PATTERN (insn), 0); i++)
			INSN_FROM_TARGET_P (XVECEXP (PATTERN (insn), 0, i))
			  = ! INSN_FROM_TARGET_P (XVECEXP (PATTERN (insn), 0, i));

		      mark_set_resources (insn, &set, 0, 1);
		    }
		  else
		    {
		      mark_set_resources (insn, &set, 0, 1);
		      target_set = set;
		    }

		  target_res = *res;
		  COPY_HARD_REG_SET (scratch, target_set.regs);
		  AND_COMPL_HARD_REG_SET (scratch, needed.regs);
		  AND_COMPL_HARD_REG_SET (target_res.regs, scratch);

		  fallthrough_res = *res;
		  COPY_HARD_REG_SET (scratch, set.regs);
		  AND_COMPL_HARD_REG_SET (scratch, needed.regs);
		  AND_COMPL_HARD_REG_SET (fallthrough_res.regs, scratch);

		  find_dead_or_set_registers (JUMP_LABEL (this_jump_insn),
					      &target_res, 0, jump_count,
					      target_set, needed);
		  find_dead_or_set_registers (next,
					      &fallthrough_res, 0, jump_count,
					      set, needed);
		  IOR_HARD_REG_SET (fallthrough_res.regs, target_res.regs);
		  AND_HARD_REG_SET (res->regs, fallthrough_res.regs);
		  break;
		}
	      else
		break;
	    }
	  else
	    {
	      /* Don't try this optimization if we expired our jump count
		 above, since that would mean there may be an infinite loop
		 in the function being compiled.  */
	      jump_insn = 0;
	      break;
	    }
	}

      mark_referenced_resources (insn, &needed, 1);
      mark_set_resources (insn, &set, 0, 1);

      COPY_HARD_REG_SET (scratch, set.regs);
      AND_COMPL_HARD_REG_SET (scratch, needed.regs);
      AND_COMPL_HARD_REG_SET (res->regs, scratch);
    }

  return jump_insn;
}

/* Given X, a part of an insn, and a pointer to a `struct resource',
   RES, indicate which resources are modified by the insn. If
   INCLUDE_DELAYED_EFFECTS is nonzero, also mark resources potentially
   set by the called routine.

   If IN_DEST is nonzero, it means we are inside a SET.  Otherwise,
   objects are being referenced instead of set.

   We never mark the insn as modifying the condition code unless it explicitly
   SETs CC0 even though this is not totally correct.  The reason for this is
   that we require a SET of CC0 to immediately precede the reference to CC0.
   So if some other insn sets CC0 as a side-effect, we know it cannot affect
   our computation and thus may be placed in a delay slot.   */

void
mark_set_resources (x, res, in_dest, include_delayed_effects)
     register rtx x;
     register struct resources *res;
     int in_dest;
     int include_delayed_effects;
{
  enum rtx_code code;
  int i, j;
  unsigned int r;
  const char *format_ptr;

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
	  for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	    if (call_used_regs[r] || global_regs[r])
	      SET_HARD_REG_BIT (res->regs, r);

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

      /* ... and also what its RTL says it modifies, if anything.  */

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
	  res->unch_memory |= RTX_UNCHANGING_P (x);
	  res->volatil |= MEM_VOLATILE_P (x);
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
	      unsigned int regno = REGNO (SUBREG_REG (x)) + SUBREG_WORD (x);
	      unsigned int last_regno
		= regno + HARD_REGNO_NREGS (regno, GET_MODE (x));

	      for (r = regno; r < last_regno; r++)
		SET_HARD_REG_BIT (res->regs, r);
	    }
	}
      return;

    case REG:
      if (in_dest)
        for (r = 0; r < HARD_REGNO_NREGS (REGNO (x), GET_MODE (x)); r++)
	  SET_HARD_REG_BIT (res->regs, REGNO (x) + r);
      return;

    case UNSPEC_VOLATILE:
    case ASM_INPUT:
      /* Traditional asm's are always volatile.  */
      res->volatil = 1;
      return;

    case TRAP_IF:
      res->volatil = 1;
      break;

    case ASM_OPERANDS:
      res->volatil |= MEM_VOLATILE_P (x);

      /* For all ASM_OPERANDS, we must traverse the vector of input operands.
	 We can not just fall through here since then we would be confused
	 by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	 traditional asms unlike their normal usage.  */
      
      for (i = 0; i < ASM_OPERANDS_INPUT_LENGTH (x); i++)
	mark_set_resources (ASM_OPERANDS_INPUT (x, i), res, in_dest, 0);
      return;

    default:
      break;
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
   in a hash table indexed by INSN_UID.  This is only done if the function
   init_resource_info () was invoked before we are called.  */

void
mark_target_live_regs (insns, target, res)
     rtx insns;
     rtx target;
     struct resources *res;
{
  int b = -1;
  int i;
  struct target_info *tinfo = NULL;
  rtx insn;
  rtx jump_insn = 0;
  rtx jump_target;
  HARD_REG_SET scratch;
  struct resources set, needed;

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
  if (target_hash_table != NULL)
    {
      for (tinfo = target_hash_table[INSN_UID (target) % TARGET_HASH_PRIME];
	   tinfo; tinfo = tinfo->next)
	if (tinfo->uid == INSN_UID (target))
	  break;

      /* Start by getting the basic block number.  If we have saved
	 information, we can get it from there unless the insn at the
	 start of the basic block has been deleted.  */
      if (tinfo && tinfo->block != -1
	  && ! INSN_DELETED_P (BLOCK_HEAD (tinfo->block)))
	b = tinfo->block;
    }

  if (b == -1)
    b = find_basic_block (target);

  if (target_hash_table != NULL)
    {
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
    }

  CLEAR_HARD_REG_SET (pending_dead_regs);

  /* If we found a basic block, get the live registers from it and update
     them with anything set or killed between its start and the insn before
     TARGET.  Otherwise, we must assume everything is live.  */
  if (b != -1)
    {
      regset regs_live = BASIC_BLOCK (b)->global_live_at_start;
      unsigned int j;
      unsigned int regno;
      rtx start_insn, stop_insn;

      /* Compute hard regs live at start of block -- this is the real hard regs
	 marked live, plus live pseudo regs that have been renumbered to
	 hard regs.  */

      REG_SET_TO_HARD_REG_SET (current_live_regs, regs_live);

      EXECUTE_IF_SET_IN_REG_SET
	(regs_live, FIRST_PSEUDO_REGISTER, i,
	 {
	   if (reg_renumber[i] >= 0)
	     {
	       regno = reg_renumber[i];
	       for (j = regno;
		    j < regno + HARD_REGNO_NREGS (regno,
						  PSEUDO_REGNO_MODE (i));
		    j++)
		 SET_HARD_REG_BIT (current_live_regs, j);
	     }
	 });

      /* Get starting and ending insn, handling the case where each might
	 be a SEQUENCE.  */
      start_insn = (b == 0 ? insns : BLOCK_HEAD (b));
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
#if defined (PIC_OFFSET_TABLE_REGNUM) && !defined (PIC_OFFSET_TABLE_REG_CALL_CLOBBERED)
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

	      note_stores (PATTERN (real_insn), update_live_status, NULL);

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
      if (tinfo != NULL)
	{
	  tinfo->block = b;
	  tinfo->bb_tick = bb_ticks[b];
	}
    }
  else
    /* We didn't find the start of a basic block.  Assume everything
       in use.  This should happen only extremely rarely.  */
    SET_HARD_REG_SET (res->regs);

  CLEAR_RESOURCE (&set);
  CLEAR_RESOURCE (&needed);

  jump_insn = find_dead_or_set_registers (target, res, &jump_target, 0,
					  set, needed);

  /* If we hit an unconditional branch, we have another way of finding out
     what is live: we can see what is live at the branch target and include
     anything used but not set before the branch.  We add the live
     resources found using the test below to those found until now. */

  if (jump_insn)
    {
      struct resources new_resources;
      rtx stop_insn = next_active_insn (jump_insn);

      mark_target_live_regs (insns, next_active_insn (jump_target),
			     &new_resources);
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

      IOR_HARD_REG_SET (res->regs, new_resources.regs);
    }

  if (tinfo != NULL)
    {
      COPY_HARD_REG_SET (tinfo->live_regs, res->regs);
    }
}

/* Initialize the resources required by mark_target_live_regs ().
   This should be invoked before the first call to mark_target_live_regs.  */

void
init_resource_info (epilogue_insn)
     rtx epilogue_insn;
{
  int i;

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
      if (! EXIT_IGNORE_STACK
	  || current_function_sp_is_unchanging)
#endif
	SET_HARD_REG_BIT (end_of_function_needs.regs, STACK_POINTER_REGNUM);
    }
  else
    SET_HARD_REG_BIT (end_of_function_needs.regs, STACK_POINTER_REGNUM);

  if (current_function_return_rtx != 0)
    mark_referenced_resources (current_function_return_rtx,
			       &end_of_function_needs, 1);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (global_regs[i]
#ifdef EPILOGUE_USES
	|| EPILOGUE_USES (i)
#endif
	)
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

  while ((epilogue_insn = next_nonnote_insn (epilogue_insn)))
    mark_set_resources (epilogue_insn, &end_of_function_needs, 0, 1);

  /* Allocate and initialize the tables used by mark_target_live_regs.  */
  target_hash_table = (struct target_info **)
    xcalloc (TARGET_HASH_PRIME, sizeof (struct target_info *));
  bb_ticks = (int *) xcalloc (n_basic_blocks, sizeof (int));
}

/* Free up the resources allcated to mark_target_live_regs ().  This
   should be invoked after the last call to mark_target_live_regs ().  */

void
free_resource_info ()
{
  if (target_hash_table != NULL)
    {
      free (target_hash_table);
      target_hash_table = NULL;
    }

  if (bb_ticks != NULL)
    {
      free (bb_ticks);
      bb_ticks = NULL;
    }
}

/* Clear any hashed information that we have stored for INSN.  */

void
clear_hashed_info_for_insn (insn)
     rtx insn;
{
  struct target_info *tinfo;
      
  if (target_hash_table != NULL)
    {
      for (tinfo = target_hash_table[INSN_UID (insn) % TARGET_HASH_PRIME];
	   tinfo; tinfo = tinfo->next)
	if (tinfo->uid == INSN_UID (insn))
	  break;

      if (tinfo)
	tinfo->block = -1;
    }
}

/* Increment the tick count for the basic block that contains INSN.  */

void
incr_ticks_for_insn (insn)
     rtx insn;
{
  int b = find_basic_block (insn);

  if (b != -1)
    bb_ticks[b]++;
}

/* Add TRIAL to the set of resources used at the end of the current
   function. */
void
mark_end_of_function_resources (trial, include_delayed_effects)
     rtx trial;
     int include_delayed_effects;
{
  mark_referenced_resources (trial, &end_of_function_needs,
			     include_delayed_effects);
}

/* Try to find a hard register of mode MODE, matching the register class in
   CLASS_STR, which is available at the beginning of insn CURRENT_INSN and
   remains available until the end of LAST_INSN.  LAST_INSN may be NULL_RTX,
   in which case the only condition is that the register must be available
   before CURRENT_INSN.
   Registers that already have bits set in REG_SET will not be considered.

   If an appropriate register is available, it will be returned and the
   corresponding bit(s) in REG_SET will be set; otherwise, NULL_RTX is
   returned.  */

rtx
find_free_register (current_insn, last_insn, class_str, mode, reg_set)
     rtx current_insn, last_insn;
     const char *class_str;
     int mode;
     HARD_REG_SET *reg_set;
{
  int i, j;
  struct resources used;
  unsigned char clet = class_str[0];
  enum reg_class class
    = (clet == 'r' ? GENERAL_REGS :  REG_CLASS_FROM_LETTER (clet));

  mark_target_live_regs (get_insns (), current_insn, &used);
  if (last_insn)
    while (current_insn != last_insn)
      {
	/* Exclude anything set in this insn.  */
	mark_set_resources (PATTERN (current_insn), &used, 0, 1);
	current_insn = next_nonnote_insn (current_insn);
      }


  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int regno;
      int success;

#ifdef REG_ALLOC_ORDER
      regno = reg_alloc_order [i];
#else
      regno = i;
#endif

      /* Don't allocate fixed registers.  */
      if (fixed_regs[regno])
	continue;
      /* Make sure the register is of the right class.  */
      if (! TEST_HARD_REG_BIT (reg_class_contents[class], regno))
	continue;
      /* And can support the mode we need.  */
      if (! HARD_REGNO_MODE_OK (regno, mode))
	continue;
      /* And that we don't create an extra save/restore.  */
      if (! call_used_regs[regno] && ! regs_ever_live[regno])
	continue;
      /* And we don't clobber traceback for noreturn functions.  */
      if ((regno == FRAME_POINTER_REGNUM || regno == HARD_FRAME_POINTER_REGNUM)
	  && (! reload_completed || frame_pointer_needed))
	continue;

      success = 1;
      for (j = HARD_REGNO_NREGS (regno, mode) - 1; j >= 0; j--)
	{
	  if (TEST_HARD_REG_BIT (*reg_set, regno + j)
	      || TEST_HARD_REG_BIT (used.regs, regno + j))
	    {
	      success = 0;
	      break;
	    }
	}
      if (success)
	{
	  for (j = HARD_REGNO_NREGS (regno, mode) - 1; j >= 0; j--)
	    {
	      SET_HARD_REG_BIT (*reg_set, regno + j);
	    }
	  return gen_rtx_REG (mode, regno);
	}
    }
  return NULL_RTX;
}

/* Return true if REG is dead at CURRENT_INSN.  */

int
reg_dead_p (current_insn, reg)
     rtx current_insn, reg;
{
  struct resources used;
  int regno, j;

  mark_target_live_regs (get_insns (), current_insn, &used);
  
  regno = REGNO (reg);
  for (j = HARD_REGNO_NREGS (regno, GET_MODE (reg)) - 1; j >= 0; j--)
    {
      if (TEST_HARD_REG_BIT (used.regs, regno + j))
	return 0;
    }

  return 1;
}
