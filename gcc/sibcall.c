/* Generic sibling call optimization support
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

#include "rtl.h"
#include "regs.h"
#include "function.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "basic-block.h"
#include "output.h"
#include "except.h"

static int identify_call_return_value	PARAMS ((rtx, rtx *, rtx *));
static rtx skip_copy_to_return_value	PARAMS ((rtx, rtx, rtx));
static rtx skip_use_of_return_value	PARAMS ((rtx, enum rtx_code));
static rtx skip_stack_adjustment	PARAMS ((rtx));
static rtx skip_jump_insn		PARAMS ((rtx));
static int uses_addressof		PARAMS ((rtx));
static int sequence_uses_addressof	PARAMS ((rtx));
static void purge_reg_equiv_notes	PARAMS ((void));

/* Examine a CALL_PLACEHOLDER pattern and determine where the call's
   return value is located.  P_HARD_RETURN receives the hard register
   that the function used; P_SOFT_RETURN receives the pseudo register
   that the sequence used.  Return non-zero if the values were located.  */

static int
identify_call_return_value (cp, p_hard_return, p_soft_return)
     rtx cp;
     rtx *p_hard_return, *p_soft_return;
{
  rtx insn, set, hard, soft;

  /* Search forward through the "normal" call sequence to the CALL insn.  */
  insn = XEXP (cp, 0);
  while (GET_CODE (insn) != CALL_INSN)
    insn = NEXT_INSN (insn);

  /* Assume the pattern is (set (dest) (call ...)), or that the first
     member of a parallel is.  This is the hard return register used
     by the function.  */
  if (GET_CODE (PATTERN (insn)) == SET
      && GET_CODE (SET_SRC (PATTERN (insn))) == CALL)
    hard = SET_DEST (PATTERN (insn));
  else if (GET_CODE (PATTERN (insn)) == PARALLEL
	   && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET
	   && GET_CODE (SET_SRC (XVECEXP (PATTERN (insn), 0, 0))) == CALL)
    hard = SET_DEST (XVECEXP (PATTERN (insn), 0, 0));
  else
    return 0;

  /* If we didn't get a single hard register (e.g. a parallel), give up.  */
  if (GET_CODE (hard) != REG)
    return 0;
    
  /* If there's nothing after, there's no soft return value.  */
  insn = NEXT_INSN (insn);
  if (! insn)
    return 0;
  
  /* We're looking for a source of the hard return register.  */
  set = single_set (insn);
  if (! set || SET_SRC (set) != hard)
    return 0;

  soft = SET_DEST (set);
  insn = NEXT_INSN (insn);

  /* Allow this first destination to be copied to a second register,
     as might happen if the first register wasn't the particular pseudo
     we'd been expecting.  */
  if (insn
      && (set = single_set (insn)) != NULL_RTX
      && SET_SRC (set) == soft)
    {
      soft = SET_DEST (set);
      insn = NEXT_INSN (insn);
    }

  /* Don't fool with anything but pseudo registers.  */
  if (GET_CODE (soft) != REG || REGNO (soft) < FIRST_PSEUDO_REGISTER)
    return 0;

  /* This value must not be modified before the end of the sequence.  */
  if (reg_set_between_p (soft, insn, NULL_RTX))
    return 0;

  *p_hard_return = hard;
  *p_soft_return = soft;

  return 1;
}

/* If the first real insn after ORIG_INSN copies to this function's
   return value from RETVAL, then return the insn which performs the
   copy.  Otherwise return ORIG_INSN.  */

static rtx
skip_copy_to_return_value (orig_insn, hardret, softret)
     rtx orig_insn;
     rtx hardret, softret;
{
  rtx insn, set = NULL_RTX;

  insn = next_nonnote_insn (orig_insn);
  if (! insn)
    return orig_insn;

  set = single_set (insn);
  if (! set)
    return orig_insn;

  /* The destination must be the same as the called function's return
     value to ensure that any return value is put in the same place by the
     current function and the function we're calling. 

     Further, the source must be the same as the pseudo into which the
     called function's return value was copied.  Otherwise we're returning
     some other value.  */

  if (SET_DEST (set) == current_function_return_rtx
      && REG_P (SET_DEST (set))
      && REGNO (SET_DEST (set)) == REGNO (hardret)
      && SET_SRC (set) == softret)
    return insn;

  /* It did not look like a copy of the return value, so return the
     same insn we were passed.  */
  return orig_insn;
}

/* If the first real insn after ORIG_INSN is a CODE of this function's return
   value, return insn.  Otherwise return ORIG_INSN.  */

static rtx
skip_use_of_return_value (orig_insn, code)
     rtx orig_insn;
     enum rtx_code code;
{
  rtx insn;

  insn = next_nonnote_insn (orig_insn);

  if (insn
      && GET_CODE (insn) == INSN
      && GET_CODE (PATTERN (insn)) == code
      && (XEXP (PATTERN (insn), 0) == current_function_return_rtx
	  || XEXP (PATTERN (insn), 0) == const0_rtx))
    return insn;

  return orig_insn;
}

/* If the first real insn after ORIG_INSN adjusts the stack pointer
   by a constant, return the insn with the stack pointer adjustment.
   Otherwise return ORIG_INSN.  */

static rtx
skip_stack_adjustment (orig_insn)
     rtx orig_insn;
{
  rtx insn, set = NULL_RTX;

  insn = next_nonnote_insn (orig_insn);

  if (insn)
    set = single_set (insn);

  /* The source must be the same as the current function's return value to
     ensure that any return value is put in the same place by the current
     function and the function we're calling.   The destination register
     must be a pseudo.  */
  if (insn
      && set
      && GET_CODE (SET_SRC (set)) == PLUS
      && XEXP (SET_SRC (set), 0) == stack_pointer_rtx
      && GET_CODE (XEXP (SET_SRC (set), 1)) == CONST_INT
      && SET_DEST (set) == stack_pointer_rtx)
    return insn;

  /* It did not look like a copy of the return value, so return the
     same insn we were passed.  */
  return orig_insn;
}

/* If the first real insn after ORIG_INSN is a jump, return the JUMP_INSN.
   Otherwise return ORIG_INSN.  */

static rtx
skip_jump_insn (orig_insn)
     rtx orig_insn;
{
  rtx insn;

  insn = next_nonnote_insn (orig_insn);

  if (insn
      && GET_CODE (insn) == JUMP_INSN
      && simplejump_p (insn))
    return insn;

  return orig_insn;
}

/* Scan the rtx X for an ADDRESSOF expressions.  Return nonzero if an ADDRESSOF
   expresion is found, else return zero.  */

static int
uses_addressof (x)
     rtx x;
{
  RTX_CODE code;
  int i, j;
  const char *fmt;

  if (x == NULL_RTX)
    return 0;

  code = GET_CODE (x);

  if (code == ADDRESSOF)
    return 1;

  /* Scan all subexpressions. */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    {
      if (*fmt == 'e')
	{
	  if (uses_addressof (XEXP (x, i)))
	    return 1;
	}
      else if (*fmt == 'E')
	{
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (uses_addressof (XVECEXP (x, i, j)))
	      return 1;
	}
    }
  return 0;
}

/* Scan the sequence of insns in SEQ to see if any have an ADDRESSOF
   rtl expression.  If an ADDRESSOF expression is found, return nonzero,
   else return zero.

   This function handles CALL_PLACEHOLDERs which contain multiple sequences
   of insns.  */

static int
sequence_uses_addressof (seq)
     rtx seq;
{
  rtx insn;

  for (insn = seq; insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      {
	/* If this is a CALL_PLACEHOLDER, then recursively call ourselves
	   with each nonempty sequence attached to the CALL_PLACEHOLDER.  */
	if (GET_CODE (insn) == CALL_INSN
	    && GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	  {
	    if (XEXP (PATTERN (insn), 0) != NULL_RTX
		&& sequence_uses_addressof (XEXP (PATTERN (insn), 0)))
	      return 1;
	    if (XEXP (PATTERN (insn), 1) != NULL_RTX
		&& sequence_uses_addressof (XEXP (PATTERN (insn), 1)))
	      return 1;
	    if (XEXP (PATTERN (insn), 2) != NULL_RTX
		&& sequence_uses_addressof (XEXP (PATTERN (insn), 2)))
	      return 1;
	  }
	else if (uses_addressof (PATTERN (insn))
		 || (REG_NOTES (insn) && uses_addressof (REG_NOTES (insn))))
	  return 1;
      }
  return 0;
}

/* Remove all REG_EQUIV notes found in the insn chain.  */

static void
purge_reg_equiv_notes ()
{
  rtx insn;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      while (1)
	{
	  rtx note = find_reg_note (insn, REG_EQUIV, 0);
	  if (note)
	    {
	      /* Remove the note and keep looking at the notes for
		 this insn.  */
	      remove_note (insn, note);
	      continue;
	    }
	  break;
	}
    }
}

/* Replace the CALL_PLACEHOLDER with one of its children.  INSN should be
   the CALL_PLACEHOLDER insn; USE tells which child to use.  */

void
replace_call_placeholder (insn, use)
     rtx insn;
     sibcall_use_t use;
{
  if (use == sibcall_use_tail_recursion)
    emit_insns_before (XEXP (PATTERN (insn), 2), insn);
  else if (use == sibcall_use_sibcall)
    emit_insns_before (XEXP (PATTERN (insn), 1), insn);
  else if (use == sibcall_use_normal)
    emit_insns_before (XEXP (PATTERN (insn), 0), insn);
  else
    abort();

  /* Turn off LABEL_PRESERVE_P for the tail recursion label if it
     exists.  We only had to set it long enough to keep the jump
     pass above from deleting it as unused.  */
  if (XEXP (PATTERN (insn), 3))
    LABEL_PRESERVE_P (XEXP (PATTERN (insn), 3)) = 0;
  
  /* "Delete" the placeholder insn. */
  PUT_CODE (insn, NOTE);
  NOTE_SOURCE_FILE (insn) = 0;
  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
}


/* Given a (possibly empty) set of potential sibling or tail recursion call
   sites, determine if optimization is possible.

   Potential sibling or tail recursion calls are marked with CALL_PLACEHOLDER
   insns.  The CALL_PLACEHOLDER insn holds chains of insns to implement a
   normal call, sibling call or tail recursive call.

   Replace the CALL_PLACEHOLDER with an appropriate insn chain.  */

void
optimize_sibling_and_tail_recursive_calls ()
{
  rtx insn, insns;
  basic_block alternate_exit = EXIT_BLOCK_PTR;
  int current_function_uses_addressof;
  int successful_sibling_call = 0;
  int replaced_call_placeholder = 0;
  edge e;

  insns = get_insns ();

  /* We do not perform these calls when flag_exceptions is true, so this
     is probably a NOP at the current time.  However, we may want to support
     sibling and tail recursion optimizations in the future, so let's plan
     ahead and find all the EH labels.  */
  find_exception_handler_labels ();

  /* Run a jump optimization pass to clean up the CFG.  We primarily want
     this to thread jumps so that it is obvious which blocks jump to the
     epilouge.  */
  jump_optimize_minimal (insns);

  /* We need cfg information to determine which blocks are succeeded
     only by the epilogue.  */
  find_basic_blocks (insns, max_reg_num (), 0);
  cleanup_cfg (insns);

  /* If there are no basic blocks, then there is nothing to do.  */
  if (n_basic_blocks == 0)
    return;

  /* Find the exit block.

     It is possible that we have blocks which can reach the exit block
     directly.  However, most of the time a block will jump (or fall into)
     N_BASIC_BLOCKS - 1, which in turn falls into the exit block.  */
  for (e = EXIT_BLOCK_PTR->pred;
       e && alternate_exit == EXIT_BLOCK_PTR;
       e = e->pred_next)
    {
      rtx insn;

      if (e->dest != EXIT_BLOCK_PTR || e->succ_next != NULL)
	continue;

      /* Walk forwards through the last normal block and see if it
	 does nothing except fall into the exit block.  */
      for (insn = BLOCK_HEAD (n_basic_blocks - 1);
	   insn;
	   insn = NEXT_INSN (insn))
	{
	  /* This should only happen once, at the start of this block.  */
	  if (GET_CODE (insn) == CODE_LABEL)
	    continue;

	  if (GET_CODE (insn) == NOTE)
	    continue;

	  if (GET_CODE (insn) == INSN
	      && GET_CODE (PATTERN (insn)) == USE)
	    continue;

	  break;
	}

      /* If INSN is zero, then the search walked all the way through the
	 block without hitting anything interesting.  This block is a
	 valid alternate exit block.  */
      if (insn == NULL)
	alternate_exit = e->src;
    }

  /* If the function uses ADDRESSOF, we can't (easily) determine
     at this point if the value will end up on the stack.  */
  current_function_uses_addressof = sequence_uses_addressof (insns);

  /* Walk the insn chain and find any CALL_PLACEHOLDER insns.  We need to
     select one of the insn sequences attached to each CALL_PLACEHOLDER.

     The different sequences represent different ways to implement the call,
     ie, tail recursion, sibling call or normal call.

     Since we do not create nested CALL_PLACEHOLDERs, the scan
     continues with the insn that was after a replaced CALL_PLACEHOLDER;
     we don't rescan the replacement insns.  */
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CALL_INSN
	  && GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	{
	  int sibcall = (XEXP (PATTERN (insn), 1) != NULL_RTX);
	  int tailrecursion = (XEXP (PATTERN (insn), 2) != NULL_RTX);
	  basic_block succ_block, call_block;
	  rtx temp, hardret, softret;

	  /* We must be careful with stack slots which are live at
	     potential optimization sites.

	     ?!? This test is overly conservative and will be replaced.  */
	  if (frame_offset)
	    goto failure;

	  /* alloca (until we have stack slot life analysis) inhibits
	     sibling call optimizations, but not tail recursion.

	     Similarly if we have ADDRESSOF expressions.

	     Similarly if we use varargs or stdarg since they implicitly
	     may take the address of an argument.  */
 	  if (current_function_calls_alloca || current_function_uses_addressof
	      || current_function_varargs || current_function_stdarg)
	    sibcall = 0;

	  call_block = BLOCK_FOR_INSN (insn);

	  /* If the block has more than one successor, then we can not
	     perform sibcall or tail recursion optimizations.  */
	  if (call_block->succ == NULL
	      || call_block->succ->succ_next != NULL)
	    goto failure;

	  /* If the single successor is not the exit block, then we can not
	     perform sibcall or tail recursion optimizations. 

	     Note that this test combined with the previous is sufficient
	     to prevent tail call optimization in the presense of active
	     exception handlers.  */
	  succ_block = call_block->succ->dest;
	  if (succ_block != EXIT_BLOCK_PTR && succ_block != alternate_exit)
	    goto failure;

	  /* If the call was the end of the block, then we're OK.  */
	  temp = insn;
	  if (temp == call_block->end)
	    goto success;

	  /* Skip over copying from the call's return value pseudo into
	     this function's hard return register.  */
	  if (identify_call_return_value (PATTERN (insn), &hardret, &softret))
	    {
	      temp = skip_copy_to_return_value (temp, hardret, softret);
	      if (temp == call_block->end)
	        goto success;
	    }

	  /* Skip any stack adjustment.  */
	  temp = skip_stack_adjustment (temp);
	  if (temp == call_block->end)
	    goto success;

	  /* Skip over a CLOBBER of the return value (as a hard reg).  */
	  temp = skip_use_of_return_value (temp, CLOBBER);
	  if (temp == call_block->end)
	    goto success;

	  /* Skip over a USE of the return value (as a hard reg).  */
	  temp = skip_use_of_return_value (temp, USE);
	  if (temp == call_block->end)
	    goto success;

	  /* Skip over the JUMP_INSN at the end of the block.  */
	  temp = skip_jump_insn (temp);
	  if (GET_CODE (temp) == NOTE)
	    temp = next_nonnote_insn (temp);
	  if (temp == call_block->end)
	    goto success;

	  /* There are operations at the end of the block which we must
	     execute after returning from the function call.  So this call
	     can not be optimized.  */
failure:
	  sibcall = 0, tailrecursion = 0;
success:

	  /* Select a set of insns to implement the call and emit them.
	     Tail recursion is the most efficient, so select it over
	     a tail/sibling call.  */
  
	  if (sibcall)
	    successful_sibling_call = 1;
	  replaced_call_placeholder = 1;
	  replace_call_placeholder (insn, 
				    tailrecursion != 0 
				      ? sibcall_use_tail_recursion
				      : sibcall != 0
					 ? sibcall_use_sibcall
					 : sibcall_use_normal);
	}
    }

  /* A sibling call sequence invalidates any REG_EQUIV notes made for
     this function's incoming arguments. 

     At the start of RTL generation we know the only REG_EQUIV notes
     in the rtl chain are those for incoming arguments, so we can safely
     flush any REG_EQUIV note. 

     This is (slight) overkill.  We could keep track of the highest argument
     we clobber and be more selective in removing notes, but it does not
     seem to be worth the effort.  */
  if (successful_sibling_call)
    purge_reg_equiv_notes ();

  /* There may have been NOTE_INSN_BLOCK_{BEGIN,END} notes in the 
     CALL_PLACEHOLDER alternatives that we didn't emit.  Rebuild the
     lexical block tree to correspond to the notes that still exist.  */
  if (replaced_call_placeholder)
    unroll_block_trees ();

  /* This information will be invalid after inline expansion.  Kill it now.  */
  free_basic_block_vars (0);
}
