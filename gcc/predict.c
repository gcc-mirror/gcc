/* Branch prediction routines for the GNU compiler.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

/* References:

   [1] "Branch Prediction for Free"
       Ball and Larus; PLDI '93.
   [2] "Static Branch Frequency and Program Profile Analysis"
       Wu and Larus; MICRO-27.
   [3] "Corpus-based Static Branch Prediction"
       Calder, Grunwald, Lindsay, Martin, Mozer, and Zorn; PLDI '95.
*/


#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "insn-flags.h"
#include "expr.h"



/* Statically estimate the probability that a branch will be taken.
   ??? In the next revision there will be a number of other predictors added
   from the above references. Further, each heuristic will be factored out
   into its own function for clarity (and to facilitate the combination of
   predictions).  */

void
estimate_probability (loops_info)
     struct loops *loops_info;
{
  int i;

  /* Try to predict out blocks in a loop that are not part of a
     natural loop.  */
  for (i = 0; i < loops_info->num; i++)
    {
      int j;

      for (j = loops_info->array[i].first->index;
	   j <= loops_info->array[i].last->index;
	   ++j)
	{
	  edge e;
	  
	  if (! TEST_BIT (loops_info->array[i].nodes, j))
	    for (e = BASIC_BLOCK(j)->pred; e; e = e->pred_next)
	      if (TEST_BIT (loops_info->array[i].nodes, e->src->index))
		{
		  rtx last_insn = BLOCK_END (e->src->index);
		  rtx cond, earliest;

		  if (GET_CODE (last_insn) != JUMP_INSN
		      || ! condjump_p (last_insn) || simplejump_p (last_insn))
		    continue;
		  cond = get_condition (last_insn, &earliest);
		  if (! cond)
		    continue;
		  if (! find_reg_note (last_insn, REG_BR_PROB, 0))
		    REG_NOTES (last_insn)
		      = gen_rtx_EXPR_LIST (REG_BR_PROB,
					   GEN_INT (REG_BR_PROB_BASE),
					   REG_NOTES (last_insn));
		}
	}
    }

  /* Attempt to predict conditional jumps using a number of heuristics.
     For each conditional jump, we try each heuristic in a fixed order.
     If more than one heuristic applies to a particular branch, the first
     is used as the prediction for the branch.  */
  for (i = 0; i < n_basic_blocks - 1; i++)
    {
      rtx last_insn = BLOCK_END (i);
      rtx cond, earliest;
      int prob = 0;

      if (GET_CODE (last_insn) != JUMP_INSN
	  || ! condjump_p (last_insn) || simplejump_p (last_insn))
	continue;
      cond = get_condition (last_insn, &earliest);
      if (! cond)
	continue;

      /* Try "pointer heuristic."
	 A comparison ptr == 0 is predicted as false.
	 Similarly, a comparison ptr1 == ptr2 is predicted as false.  */
      prob = 0;
      switch (GET_CODE (cond))
	{
	case EQ:
	  if (GET_CODE (XEXP (cond, 0)) == REG
	      && REGNO_POINTER_FLAG (REGNO (XEXP (cond, 0)))
	      && (XEXP (cond, 1) == const0_rtx
		  || (GET_CODE (XEXP (cond, 1)) == REG
		      && REGNO_POINTER_FLAG (REGNO (XEXP (cond, 1))))))
	    prob = REG_BR_PROB_BASE / 10;
	  break;
	case NE:
	  if (GET_CODE (XEXP (cond, 0)) == REG
	      && REGNO_POINTER_FLAG (REGNO (XEXP (cond, 0)))
	      && (XEXP (cond, 1) == const0_rtx
		  || (GET_CODE (XEXP (cond, 1)) == REG
		      && REGNO_POINTER_FLAG (REGNO (XEXP (cond, 1))))))
	    prob = REG_BR_PROB_BASE / 2;
	  break;
	default:
	  prob = 0;
	}
	if (prob && ! find_reg_note (last_insn, REG_BR_PROB, 0))
	  REG_NOTES (last_insn)
	    = gen_rtx_EXPR_LIST (REG_BR_PROB, GEN_INT (prob),
				 REG_NOTES (last_insn));

      /* Try "opcode heuristic."
	 EQ tests are usually false and NE tests are usually true. Also,
	 most quantities are positive, so we can make the appropriate guesses
	 about signed comparisons against zero.  */
      switch (GET_CODE (cond))
	{
	case CONST_INT:
	  /* Unconditional branch.  */
	  prob = REG_BR_PROB_BASE / 2;
	  break;
	case EQ:
	  prob = REG_BR_PROB_BASE / 10;
	  break;
	case NE:
	  prob = REG_BR_PROB_BASE / 2;
	  break;
	case LE:
	case LT:
	  if (XEXP (cond, 1) == const0_rtx)
	    prob = REG_BR_PROB_BASE / 10;
	  break;
	case GE:
	case GT:
	  if (XEXP (cond, 1) == const0_rtx
	      || (GET_CODE (XEXP (cond, 1)) == CONST_INT
		  && INTVAL (XEXP (cond, 1)) == -1))
	    prob = REG_BR_PROB_BASE / 2;
	  break;

	default:
	  prob = 0;
	}
      if (! find_reg_note (last_insn, REG_BR_PROB, 0))
	REG_NOTES (last_insn)
	  = gen_rtx_EXPR_LIST (REG_BR_PROB, GEN_INT (prob),
			       REG_NOTES (last_insn));
    }
}

/* __builtin_expect dropped tokens into the insn stream describing
   expected values of registers.  Generate branch probabilities 
   based off these values.  */

static rtx find_expected_value		PARAMS ((rtx, rtx));

void
expected_value_to_br_prob ()
{
  rtx insn, cond, earliest, ev;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    {
      /* Look for simple conditional branches.  */
      if (GET_CODE (insn) != JUMP_INSN)
	continue;
      if (! condjump_p (insn) || simplejump_p (insn))
	continue;

      /* Collect the branch condition.  Some machines can branch on
	 user values directly, others need a compare instruction.  If
	 the branch condition involves a MODE_INT register, try that
	 expression first.  Otherwise use get_condition.  */
      cond = XEXP (SET_SRC (PATTERN (insn)), 0);
      if (GET_RTX_CLASS (GET_CODE (cond)) != '<')
	abort ();
      if (GET_CODE (XEXP (cond, 0)) == REG
	  && GET_MODE_CLASS (GET_MODE (XEXP (cond, 0))) == MODE_INT
	  && (ev = find_expected_value (cond, insn)) != NULL_RTX)
	;
      else if ((cond = get_condition (insn, &earliest)) == NULL_RTX
	       || (ev = find_expected_value (cond, earliest)) == NULL_RTX)
	continue;

      /* Substitute and simplify.  Given that the expression we're 
	 building involves two constants, we should wind up with either
	 true or false.  */
      cond = gen_rtx_fmt_ee (GET_CODE (cond), VOIDmode,
			     XEXP (ev, 1), XEXP (cond, 1));
      cond = simplify_rtx (cond);

      /* Turn the condition into a scaled branch probability.  */
      if (cond == const0_rtx)
	cond = const1_rtx;
      else if (cond == const1_rtx)
	cond = GEN_INT (REG_BR_PROB_BASE - 1);
      else
	abort ();
      REG_NOTES (insn) = alloc_EXPR_LIST (REG_BR_PROB, cond, REG_NOTES (insn));
    }
}

/* Search backwards for a NOTE_INSN_EXPECTED_VALUE note with a register
   that matches the condition.  */

static rtx
find_expected_value (cond, earliest)
     rtx cond, earliest;
{
  rtx insn, reg = XEXP (cond, 0);
  int timeout;

  /* The condition should be (op (reg) (const_int)), otherwise we
     won't be able to intuit anything about it.  */
  if (GET_CODE (reg) != REG
      || GET_CODE (XEXP (cond, 1)) != CONST_INT
      || GET_MODE_CLASS (GET_MODE (reg)) != MODE_INT)
    return NULL_RTX;

  /* Assuming the user wrote something like `if (__builtin_expect(...))',
     we shouldn't have to search too far.  Also stop if we reach a code
     label or if REG is modified.  */
  for (insn = earliest, timeout = 10;
       insn && timeout > 0;
       insn = PREV_INSN (insn), --timeout)
    {
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_EXPECTED_VALUE
	  && XEXP (NOTE_EXPECTED_VALUE (insn), 0) == reg)
	return NOTE_EXPECTED_VALUE (insn);

      if (GET_CODE (insn) == CODE_LABEL || reg_set_p (reg, insn))
	break;
    }

  return NULL_RTX;
}
