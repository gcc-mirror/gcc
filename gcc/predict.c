/* Branch prediction routines for the GNU compiler.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "expr.h"
#include "predict.h"

/* Random guesstimation given names.  */
#define PROB_NEVER		(0)
#define PROB_VERY_UNLIKELY	(REG_BR_PROB_BASE / 10 - 1)
#define PROB_UNLIKELY		(REG_BR_PROB_BASE * 4 / 10 - 1)
#define PROB_EVEN		(REG_BR_PROB_BASE / 2)
#define PROB_LIKELY		(REG_BR_PROB_BASE - PROB_UNLIKELY)
#define PROB_VERY_LIKELY	(REG_BR_PROB_BASE - PROB_VERY_UNLIKELY)
#define PROB_ALWAYS		(REG_BR_PROB_BASE)

static void combine_predictions_for_insn PARAMS ((rtx, basic_block));
static void dump_prediction		 PARAMS ((enum br_predictor, int,
						  basic_block));

/* Information we hold about each branch predictor.
   Filled using information from predict.def.  */
struct predictor_info
{
  const char *name;	/* Name used in the debugging dumps.  */
  int hitrate;		/* Expected hitrate used by
			   predict_insn_def call.  */
};

#define DEF_PREDICTOR(ENUM, NAME, HITRATE) {NAME, HITRATE},
struct predictor_info predictor_info[] = {
#include "predict.def"

  /* Upper bound on non-language-specific builtins. */
  {NULL, 0}
};
#undef DEF_PREDICTOR

void
predict_insn (insn, predictor, probability)
     rtx insn;
     int probability;
     enum br_predictor predictor;
{
  if (!any_condjump_p (insn))
    abort ();
  REG_NOTES (insn)
    = gen_rtx_EXPR_LIST (REG_BR_PRED,
			 gen_rtx_CONCAT (VOIDmode,
					 GEN_INT ((int) predictor),
					 GEN_INT ((int) probability)),
			 REG_NOTES (insn));
}

/* Predict insn by given predictor.  */
void
predict_insn_def (insn, predictor, taken)
     rtx insn;
     enum br_predictor predictor;
     enum prediction taken;
{
   int probability = predictor_info[(int) predictor].hitrate;
   if (taken != TAKEN)
     probability = REG_BR_PROB_BASE - probability;
   predict_insn (insn, predictor, probability);
}

/* Predict edge E with given probability if possible.  */
void
predict_edge (e, predictor, probability)
     edge e;
     int probability;
     enum br_predictor predictor;
{
  rtx last_insn;
  last_insn = e->src->end;

  /* We can store the branch prediction information only about
     conditional jumps.  */
  if (!any_condjump_p (last_insn))
    return;

  /* We always store probability of branching.  */
  if (e->flags & EDGE_FALLTHRU)
    probability = REG_BR_PROB_BASE - probability;

  predict_insn (last_insn, predictor, probability);
}

/* Predict edge E by given predictor if possible.  */
void
predict_edge_def (e, predictor, taken)
     edge e;
     enum br_predictor predictor;
     enum prediction taken;
{
   int probability = predictor_info[(int) predictor].hitrate;

   if (taken != TAKEN)
     probability = REG_BR_PROB_BASE - probability;
   predict_edge (e, predictor, probability);
}

/* Invert all branch predictions or probability notes in the INSN.  This needs
   to be done each time we invert the condition used by the jump.  */
void
invert_br_probabilities (insn)
     rtx insn;
{
  rtx note = REG_NOTES (insn);

  while (note)
    {
      if (REG_NOTE_KIND (note) == REG_BR_PROB)
	XEXP (note, 0) = GEN_INT (REG_BR_PROB_BASE - INTVAL (XEXP (note, 0)));
      else if (REG_NOTE_KIND (note) == REG_BR_PRED)
	XEXP (XEXP (note, 0), 1)
	  = GEN_INT (REG_BR_PROB_BASE - INTVAL (XEXP (XEXP (note, 0), 1)));
      note = XEXP (note, 1);
    }
}

/* Dump information about the branch prediction to the output file.  */
static void
dump_prediction (predictor, probability, bb)
     enum br_predictor predictor;
     int probability;
     basic_block bb;
{
  edge e = bb->succ;

  if (!rtl_dump_file)
    return;

  while (e->flags & EDGE_FALLTHRU)
    e = e->succ_next;

  fprintf (rtl_dump_file, "  %s heuristics: %.1f%%",
	   predictor_info[predictor].name,
	   probability * 100.0 / REG_BR_PROB_BASE);

  if (bb->count)
    fprintf (rtl_dump_file, "  exec %i hit %i (%.1f%%)",
	     bb->count, e->count, e->count * 100.0 / bb->count);
  fprintf (rtl_dump_file, "\n");
}

/* Combine all REG_BR_PRED notes into single probability and attach REG_BR_PROB
   note if not already present.  Remove now useless REG_BR_PRED notes.  */
static void
combine_predictions_for_insn (insn, bb)
     rtx insn;
     basic_block bb;
{
  rtx prob_note = find_reg_note (insn, REG_BR_PROB, 0);
  rtx *pnote = &REG_NOTES (insn);
  int best_probability = PROB_EVEN;
  int best_predictor = END_PREDICTORS;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Predictions for insn %i\n", INSN_UID (insn));

  /* We implement "first match" heuristics and use probability guessed
     by predictor with smallest index.  In future we will use better
     probability combination techniques.  */
  while (*pnote)
    {
      rtx *next_pnote = &XEXP (*pnote, 1);
      if (REG_NOTE_KIND (*pnote) == REG_BR_PRED)
	{
	  int predictor = INTVAL (XEXP (XEXP (*pnote, 0), 0));
	  int probability = INTVAL (XEXP (XEXP (*pnote, 0), 1));

	  dump_prediction (predictor, probability, bb);
	  if (best_predictor > predictor)
	    best_probability = probability, best_predictor = predictor;
	  *pnote = XEXP (*pnote, 1);
	}
      pnote = next_pnote;
    }
  dump_prediction (PRED_FIRST_MATCH, best_probability, bb);
  if (!prob_note)
    {
      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_BR_PROB,
			     GEN_INT (best_probability), REG_NOTES (insn));
    }
}

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
	  if (TEST_BIT (loops_info->array[i].nodes, j))
	    {
	      int header_found = 0;
	      edge e;
	  
	      /* Loop branch heruistics - predict as taken an edge back to
	         a loop's head.  */
	      for (e = BASIC_BLOCK(j)->succ; e; e = e->succ_next)
		if (e->dest == loops_info->array[i].header)
		  {
		    header_found = 1;
		    predict_edge_def (e, PRED_LOOP_BRANCH, TAKEN);
		  }
	      /* Loop exit heruistics - predict as not taken an edge exiting
	         the loop if the conditinal has no loop header successors  */
	      if (!header_found)
		for (e = BASIC_BLOCK(j)->succ; e; e = e->succ_next)
		  if (e->dest->index <= 0
		      || !TEST_BIT (loops_info->array[i].nodes, e->dest->index))
		    predict_edge_def (e, PRED_LOOP_EXIT, NOT_TAKEN);
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
      edge e;

      if (GET_CODE (last_insn) != JUMP_INSN
	  || ! any_condjump_p (last_insn))
	continue;

      if (find_reg_note (last_insn, REG_BR_PROB, 0))
	continue;

      /* If one of the successor blocks has no successors, predict
	 that side not taken.  */
      /* ??? Ought to do the same for any subgraph with no exit.  */
      for (e = BASIC_BLOCK (i)->succ; e; e = e->succ_next)
	if (e->dest->succ == NULL)
	  predict_edge_def (e, PRED_NORETURN, NOT_TAKEN);

      cond = get_condition (last_insn, &earliest);
      if (! cond)
	continue;

      /* Try "pointer heuristic."
	 A comparison ptr == 0 is predicted as false.
	 Similarly, a comparison ptr1 == ptr2 is predicted as false.  */
      switch (GET_CODE (cond))
	{
	case EQ:
	  if (GET_CODE (XEXP (cond, 0)) == REG
	      && REG_POINTER (XEXP (cond, 0))
	      && (XEXP (cond, 1) == const0_rtx
		  || (GET_CODE (XEXP (cond, 1)) == REG
		      && REG_POINTER (XEXP (cond, 1)))))
	    
	    predict_insn_def (last_insn, PRED_POINTER, NOT_TAKEN);
	  break;
	case NE:
	  if (GET_CODE (XEXP (cond, 0)) == REG
	      && REG_POINTER (XEXP (cond, 0))
	      && (XEXP (cond, 1) == const0_rtx
		  || (GET_CODE (XEXP (cond, 1)) == REG
		      && REG_POINTER (XEXP (cond, 1)))))
	    predict_insn_def (last_insn, PRED_POINTER, TAKEN);
	  break;

	default:
	  break;
	}

      /* Try "opcode heuristic."
	 EQ tests are usually false and NE tests are usually true. Also,
	 most quantities are positive, so we can make the appropriate guesses
	 about signed comparisons against zero.  */
      switch (GET_CODE (cond))
	{
	case CONST_INT:
	  /* Unconditional branch.  */
	  predict_insn_def (last_insn, PRED_UNCONDITIONAL,
			    cond == const0_rtx ? NOT_TAKEN : TAKEN);
	  break;

	case EQ:
	case UNEQ:
	  predict_insn_def (last_insn, PRED_OPCODE, NOT_TAKEN);
	  break;
	case NE:
	case LTGT:
	  predict_insn_def (last_insn, PRED_OPCODE, TAKEN);
	  break;
	case ORDERED:
	  predict_insn_def (last_insn, PRED_OPCODE, TAKEN);
	  break;
	case UNORDERED:
	  predict_insn_def (last_insn, PRED_OPCODE, NOT_TAKEN);
	  break;
	case LE:
	case LT:
	  if (XEXP (cond, 1) == const0_rtx)
	    predict_insn_def (last_insn, PRED_OPCODE, NOT_TAKEN);
	  break;
	case GE:
	case GT:
	  if (XEXP (cond, 1) == const0_rtx
	      || (GET_CODE (XEXP (cond, 1)) == CONST_INT
		  && INTVAL (XEXP (cond, 1)) == -1))
	    predict_insn_def (last_insn, PRED_OPCODE, TAKEN);
	  break;

	default:
	  break;
	}
    }

  /* Attach the combined probability to each conditional jump.  */
  for (i = 0; i < n_basic_blocks - 1; i++)
    {
      rtx last_insn = BLOCK_END (i);

      if (GET_CODE (last_insn) != JUMP_INSN
	  || ! any_condjump_p (last_insn))
	continue;
      combine_predictions_for_insn (last_insn, BASIC_BLOCK (i));
    }
}

/* __builtin_expect dropped tokens into the insn stream describing
   expected values of registers.  Generate branch probabilities 
   based off these values.  */

void
expected_value_to_br_prob ()
{
  rtx insn, cond, ev = NULL_RTX, ev_reg = NULL_RTX;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case NOTE:
	  /* Look for expected value notes.  */
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EXPECTED_VALUE)
	    {
	      ev = NOTE_EXPECTED_VALUE (insn);
	      ev_reg = XEXP (ev, 0);
	    }
	  continue;

	case CODE_LABEL:
	  /* Never propagate across labels.  */
	  ev = NULL_RTX;
	  continue;

	default:
	  /* Look for insns that clobber the EV register.  */
	  if (ev && reg_set_p (ev_reg, insn))
	    ev = NULL_RTX;
	  continue;

	case JUMP_INSN:
	  /* Look for simple conditional branches.  If we havn't got an
	     expected value yet, no point going further.  */
	  if (GET_CODE (insn) != JUMP_INSN || ev == NULL_RTX)
	    continue;
	  if (! any_condjump_p (insn))
	    continue;
	  break;
	}

      /* Collect the branch condition, hopefully relative to EV_REG.  */
      /* ???  At present we'll miss things like
		(expected_value (eq r70 0))
		(set r71 -1)
		(set r80 (lt r70 r71))
		(set pc (if_then_else (ne r80 0) ...))
	 as canonicalize_condition will render this to us as 
		(lt r70, r71)
	 Could use cselib to try and reduce this further.  */
      cond = XEXP (SET_SRC (PATTERN (insn)), 0);
      cond = canonicalize_condition (insn, cond, 0, NULL, ev_reg);
      if (! cond
	  || XEXP (cond, 0) != ev_reg
	  || GET_CODE (XEXP (cond, 1)) != CONST_INT)
	continue;

      /* Substitute and simplify.  Given that the expression we're 
	 building involves two constants, we should wind up with either
	 true or false.  */
      cond = gen_rtx_fmt_ee (GET_CODE (cond), VOIDmode,
			     XEXP (ev, 1), XEXP (cond, 1));
      cond = simplify_rtx (cond);

      /* Turn the condition into a scaled branch probability.  */
      if (cond != const1_rtx && cond != const0_rtx)
	abort ();
      predict_insn_def (insn, PRED_BUILTIN_EXPECT,
		        cond == const1_rtx ? TAKEN : NOT_TAKEN);
    }
}
