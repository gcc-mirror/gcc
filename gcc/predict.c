/* Branch prediction routines for the GNU compiler.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

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

/* References:

   [1] "Branch Prediction for Free"
       Ball and Larus; PLDI '93.
   [2] "Static Branch Frequency and Program Profile Analysis"
       Wu and Larus; MICRO-27.
   [3] "Corpus-based Static Branch Prediction"
       Calder, Grunwald, Lindsay, Martin, Mozer, and Zorn; PLDI '95.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
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
#include "coverage.h"
#include "sreal.h"
#include "params.h"
#include "target.h"
#include "loop.h"
#include "cfgloop.h"

/* real constants: 0, 1, 1-1/REG_BR_PROB_BASE, REG_BR_PROB_BASE,
		   1/REG_BR_PROB_BASE, 0.5, BB_FREQ_MAX.  */
static sreal real_zero, real_one, real_almost_one, real_br_prob_base,
	     real_inv_br_prob_base, real_one_half, real_bb_freq_max;

/* Random guesstimation given names.  */
#define PROB_VERY_UNLIKELY	(REG_BR_PROB_BASE / 100 - 1)
#define PROB_EVEN		(REG_BR_PROB_BASE / 2)
#define PROB_VERY_LIKELY	(REG_BR_PROB_BASE - PROB_VERY_UNLIKELY)
#define PROB_ALWAYS		(REG_BR_PROB_BASE)

static bool predicted_by_p (basic_block, enum br_predictor);
static void combine_predictions_for_insn (rtx, basic_block);
static void dump_prediction (enum br_predictor, int, basic_block, int);
static void estimate_loops_at_level (struct loop *loop);
static void propagate_freq (struct loop *);
static void estimate_bb_frequencies (struct loops *);
static void counts_to_freqs (void);
static void process_note_predictions (basic_block, int *);
static void process_note_prediction (basic_block, int *, int, int);
static bool last_basic_block_p (basic_block);
static void compute_function_frequency (void);
static void choose_function_section (void);
static bool can_predict_insn_p (rtx);

/* Information we hold about each branch predictor.
   Filled using information from predict.def.  */

struct predictor_info
{
  const char *const name;	/* Name used in the debugging dumps.  */
  const int hitrate;		/* Expected hitrate used by
				   predict_insn_def call.  */
  const int flags;
};

/* Use given predictor without Dempster-Shaffer theory if it matches
   using first_match heuristics.  */
#define PRED_FLAG_FIRST_MATCH 1

/* Recompute hitrate in percent to our representation.  */

#define HITRATE(VAL) ((int) ((VAL) * REG_BR_PROB_BASE + 50) / 100)

#define DEF_PREDICTOR(ENUM, NAME, HITRATE, FLAGS) {NAME, HITRATE, FLAGS},
static const struct predictor_info predictor_info[]= {
#include "predict.def"

  /* Upper bound on predictors.  */
  {NULL, 0, 0}
};
#undef DEF_PREDICTOR

/* Return true in case BB can be CPU intensive and should be optimized
   for maximal performance.  */

bool
maybe_hot_bb_p (basic_block bb)
{
  if (profile_info && flag_branch_probabilities
      && (bb->count
	  < profile_info->sum_max / PARAM_VALUE (HOT_BB_COUNT_FRACTION)))
    return false;
  if (bb->frequency < BB_FREQ_MAX / PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION))
    return false;
  return true;
}

/* Return true in case BB is cold and should be optimized for size.  */

bool
probably_cold_bb_p (basic_block bb)
{
  if (profile_info && flag_branch_probabilities
      && (bb->count
	  < profile_info->sum_max / PARAM_VALUE (HOT_BB_COUNT_FRACTION)))
    return true;
  if (bb->frequency < BB_FREQ_MAX / PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION))
    return true;
  return false;
}

/* Return true in case BB is probably never executed.  */
bool
probably_never_executed_bb_p (basic_block bb)
{
  if (profile_info && flag_branch_probabilities)
    return ((bb->count + profile_info->runs / 2) / profile_info->runs) == 0;
  return false;
}

/* Return true if the one of outgoing edges is already predicted by
   PREDICTOR.  */

static bool
predicted_by_p (basic_block bb, enum br_predictor predictor)
{
  rtx note;
  if (!INSN_P (BB_END (bb)))
    return false;
  for (note = REG_NOTES (BB_END (bb)); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_BR_PRED
	&& INTVAL (XEXP (XEXP (note, 0), 0)) == (int)predictor)
      return true;
  return false;
}

void
predict_insn (rtx insn, enum br_predictor predictor, int probability)
{
  if (!any_condjump_p (insn))
    abort ();
  if (!flag_guess_branch_prob)
    return;

  REG_NOTES (insn)
    = gen_rtx_EXPR_LIST (REG_BR_PRED,
			 gen_rtx_CONCAT (VOIDmode,
					 GEN_INT ((int) predictor),
					 GEN_INT ((int) probability)),
			 REG_NOTES (insn));
}

/* Predict insn by given predictor.  */

void
predict_insn_def (rtx insn, enum br_predictor predictor,
		  enum prediction taken)
{
   int probability = predictor_info[(int) predictor].hitrate;

   if (taken != TAKEN)
     probability = REG_BR_PROB_BASE - probability;

   predict_insn (insn, predictor, probability);
}

/* Predict edge E with given probability if possible.  */

void
predict_edge (edge e, enum br_predictor predictor, int probability)
{
  rtx last_insn;
  last_insn = BB_END (e->src);

  /* We can store the branch prediction information only about
     conditional jumps.  */
  if (!any_condjump_p (last_insn))
    return;

  /* We always store probability of branching.  */
  if (e->flags & EDGE_FALLTHRU)
    probability = REG_BR_PROB_BASE - probability;

  predict_insn (last_insn, predictor, probability);
}

/* Return true when we can store prediction on insn INSN.
   At the moment we represent predictions only on conditional
   jumps, not at computed jump or other complicated cases.  */
static bool
can_predict_insn_p (rtx insn)
{
  return (GET_CODE (insn) == JUMP_INSN
	  && any_condjump_p (insn)
	  && BLOCK_FOR_INSN (insn)->succ->succ_next);
}

/* Predict edge E by given predictor if possible.  */

void
predict_edge_def (edge e, enum br_predictor predictor,
		  enum prediction taken)
{
   int probability = predictor_info[(int) predictor].hitrate;

   if (taken != TAKEN)
     probability = REG_BR_PROB_BASE - probability;

   predict_edge (e, predictor, probability);
}

/* Invert all branch predictions or probability notes in the INSN.  This needs
   to be done each time we invert the condition used by the jump.  */

void
invert_br_probabilities (rtx insn)
{
  rtx note;

  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_BR_PROB)
      XEXP (note, 0) = GEN_INT (REG_BR_PROB_BASE - INTVAL (XEXP (note, 0)));
    else if (REG_NOTE_KIND (note) == REG_BR_PRED)
      XEXP (XEXP (note, 0), 1)
	= GEN_INT (REG_BR_PROB_BASE - INTVAL (XEXP (XEXP (note, 0), 1)));
}

/* Dump information about the branch prediction to the output file.  */

static void
dump_prediction (enum br_predictor predictor, int probability,
		 basic_block bb, int used)
{
  edge e = bb->succ;

  if (!rtl_dump_file)
    return;

  while (e && (e->flags & EDGE_FALLTHRU))
    e = e->succ_next;

  fprintf (rtl_dump_file, "  %s heuristics%s: %.1f%%",
	   predictor_info[predictor].name,
	   used ? "" : " (ignored)", probability * 100.0 / REG_BR_PROB_BASE);

  if (bb->count)
    {
      fprintf (rtl_dump_file, "  exec ");
      fprintf (rtl_dump_file, HOST_WIDEST_INT_PRINT_DEC, bb->count);
      if (e)
	{
	  fprintf (rtl_dump_file, " hit ");
	  fprintf (rtl_dump_file, HOST_WIDEST_INT_PRINT_DEC, e->count);
	  fprintf (rtl_dump_file, " (%.1f%%)", e->count * 100.0 / bb->count);
	}
    }

  fprintf (rtl_dump_file, "\n");
}

/* Combine all REG_BR_PRED notes into single probability and attach REG_BR_PROB
   note if not already present.  Remove now useless REG_BR_PRED notes.  */

static void
combine_predictions_for_insn (rtx insn, basic_block bb)
{
  rtx prob_note = find_reg_note (insn, REG_BR_PROB, 0);
  rtx *pnote = &REG_NOTES (insn);
  rtx note;
  int best_probability = PROB_EVEN;
  int best_predictor = END_PREDICTORS;
  int combined_probability = REG_BR_PROB_BASE / 2;
  int d;
  bool first_match = false;
  bool found = false;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Predictions for insn %i bb %i\n", INSN_UID (insn),
	     bb->index);

  /* We implement "first match" heuristics and use probability guessed
     by predictor with smallest index.  In the future we will use better
     probability combination techniques.  */
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_BR_PRED)
      {
	int predictor = INTVAL (XEXP (XEXP (note, 0), 0));
	int probability = INTVAL (XEXP (XEXP (note, 0), 1));

	found = true;
	if (best_predictor > predictor)
	  best_probability = probability, best_predictor = predictor;

	d = (combined_probability * probability
	     + (REG_BR_PROB_BASE - combined_probability)
	     * (REG_BR_PROB_BASE - probability));

	/* Use FP math to avoid overflows of 32bit integers.  */
	if (d == 0)
	  /* If one probability is 0% and one 100%, avoid division by zero.  */
	  combined_probability = REG_BR_PROB_BASE / 2;
	else
	  combined_probability = (((double) combined_probability) * probability
				  * REG_BR_PROB_BASE / d + 0.5);
      }

  /* Decide which heuristic to use.  In case we didn't match anything,
     use no_prediction heuristic, in case we did match, use either
     first match or Dempster-Shaffer theory depending on the flags.  */

  if (predictor_info [best_predictor].flags & PRED_FLAG_FIRST_MATCH)
    first_match = true;

  if (!found)
    dump_prediction (PRED_NO_PREDICTION, combined_probability, bb, true);
  else
    {
      dump_prediction (PRED_DS_THEORY, combined_probability, bb, !first_match);
      dump_prediction (PRED_FIRST_MATCH, best_probability, bb, first_match);
    }

  if (first_match)
    combined_probability = best_probability;
  dump_prediction (PRED_COMBINED, combined_probability, bb, true);

  while (*pnote)
    {
      if (REG_NOTE_KIND (*pnote) == REG_BR_PRED)
	{
	  int predictor = INTVAL (XEXP (XEXP (*pnote, 0), 0));
	  int probability = INTVAL (XEXP (XEXP (*pnote, 0), 1));

	  dump_prediction (predictor, probability, bb,
			   !first_match || best_predictor == predictor);
	  *pnote = XEXP (*pnote, 1);
	}
      else
	pnote = &XEXP (*pnote, 1);
    }

  if (!prob_note)
    {
      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_BR_PROB,
			     GEN_INT (combined_probability), REG_NOTES (insn));

      /* Save the prediction into CFG in case we are seeing non-degenerated
	 conditional jump.  */
      if (bb->succ->succ_next)
	{
	  BRANCH_EDGE (bb)->probability = combined_probability;
	  FALLTHRU_EDGE (bb)->probability
	    = REG_BR_PROB_BASE - combined_probability;
	}
    }
}

/* Statically estimate the probability that a branch will be taken.
   ??? In the next revision there will be a number of other predictors added
   from the above references. Further, each heuristic will be factored out
   into its own function for clarity (and to facilitate the combination of
   predictions).  */

void
estimate_probability (struct loops *loops_info)
{
  basic_block bb;
  unsigned i;

  connect_infinite_loops_to_exit ();
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  /* Try to predict out blocks in a loop that are not part of a
     natural loop.  */
  for (i = 1; i < loops_info->num; i++)
    {
      basic_block bb, *bbs;
      unsigned j;
      int exits;
      struct loop *loop = loops_info->parray[i];
      struct loop_desc desc;
      unsigned HOST_WIDE_INT niter;

      flow_loop_scan (loop, LOOP_EXIT_EDGES);
      exits = loop->num_exits;

      if (simple_loop_p (loop, &desc) && desc.const_iter)
	{
	  int prob;
	  niter = desc.niter + 1;
	  if (niter == 0)        /* We might overflow here.  */
	    niter = desc.niter;

	  prob = (REG_BR_PROB_BASE
		  - (REG_BR_PROB_BASE + niter /2) / niter);
	  /* Branch prediction algorithm gives 0 frequency for everything
	     after the end of loop for loop having 0 probability to finish.  */
	  if (prob == REG_BR_PROB_BASE)
	    prob = REG_BR_PROB_BASE - 1;
	  predict_edge (desc.in_edge, PRED_LOOP_ITERATIONS,
			prob);
	}

      bbs = get_loop_body (loop);
      for (j = 0; j < loop->num_nodes; j++)
	{
	  int header_found = 0;
	  edge e;

	  bb = bbs[j];

	  /* Bypass loop heuristics on continue statement.  These
	     statements construct loops via "non-loop" constructs
	     in the source language and are better to be handled
	     separately.  */
	  if (!can_predict_insn_p (BB_END (bb))
	      || predicted_by_p (bb, PRED_CONTINUE))
	    continue;

	  /* Loop branch heuristics - predict an edge back to a
	     loop's head as taken.  */
	  for (e = bb->succ; e; e = e->succ_next)
	    if (e->dest == loop->header
		&& e->src == loop->latch)
	      {
		header_found = 1;
		predict_edge_def (e, PRED_LOOP_BRANCH, TAKEN);
	      }

	  /* Loop exit heuristics - predict an edge exiting the loop if the
	     conditional has no loop header successors as not taken.  */
	  if (!header_found)
	    for (e = bb->succ; e; e = e->succ_next)
	      if (e->dest->index < 0
		  || !flow_bb_inside_loop_p (loop, e->dest))
		predict_edge
		  (e, PRED_LOOP_EXIT,
		   (REG_BR_PROB_BASE
		    - predictor_info [(int) PRED_LOOP_EXIT].hitrate)
		   / exits);
	}
      
      /* Free basic blocks from get_loop_body.  */
      free (bbs);
    }

  /* Attempt to predict conditional jumps using a number of heuristics.  */
  FOR_EACH_BB (bb)
    {
      rtx last_insn = BB_END (bb);
      rtx cond, earliest;
      edge e;

      if (! can_predict_insn_p (last_insn))
	continue;

      for (e = bb->succ; e; e = e->succ_next)
	{
	  /* Predict early returns to be probable, as we've already taken
	     care for error returns and other are often used for fast paths
	     trought function.  */
	  if ((e->dest == EXIT_BLOCK_PTR
	       || (e->dest->succ && !e->dest->succ->succ_next
		   && e->dest->succ->dest == EXIT_BLOCK_PTR))
	       && !predicted_by_p (bb, PRED_NULL_RETURN)
	       && !predicted_by_p (bb, PRED_CONST_RETURN)
	       && !predicted_by_p (bb, PRED_NEGATIVE_RETURN)
	       && !last_basic_block_p (e->dest))
	    predict_edge_def (e, PRED_EARLY_RETURN, TAKEN);

	  /* Look for block we are guarding (ie we dominate it,
	     but it doesn't postdominate us).  */
	  if (e->dest != EXIT_BLOCK_PTR && e->dest != bb
	      && dominated_by_p (CDI_DOMINATORS, e->dest, e->src)
	      && !dominated_by_p (CDI_POST_DOMINATORS, e->src, e->dest))
	    {
	      rtx insn;

	      /* The call heuristic claims that a guarded function call
		 is improbable.  This is because such calls are often used
		 to signal exceptional situations such as printing error
		 messages.  */
	      for (insn = BB_HEAD (e->dest); insn != NEXT_INSN (BB_END (e->dest));
		   insn = NEXT_INSN (insn))
		if (GET_CODE (insn) == CALL_INSN
		    /* Constant and pure calls are hardly used to signalize
		       something exceptional.  */
		    && ! CONST_OR_PURE_CALL_P (insn))
		  {
		    predict_edge_def (e, PRED_CALL, NOT_TAKEN);
		    break;
		  }
	    }
	}

      cond = get_condition (last_insn, &earliest, false);
      if (! cond)
	continue;

      /* Try "pointer heuristic."
	 A comparison ptr == 0 is predicted as false.
	 Similarly, a comparison ptr1 == ptr2 is predicted as false.  */
      if (GET_RTX_CLASS (GET_CODE (cond)) == '<'
	  && ((REG_P (XEXP (cond, 0)) && REG_POINTER (XEXP (cond, 0)))
	      || (REG_P (XEXP (cond, 1)) && REG_POINTER (XEXP (cond, 1)))))
	{
	  if (GET_CODE (cond) == EQ)
	    predict_insn_def (last_insn, PRED_POINTER, NOT_TAKEN);
	  else if (GET_CODE (cond) == NE)
	    predict_insn_def (last_insn, PRED_POINTER, TAKEN);
	}
      else

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
	    /* Floating point comparisons appears to behave in a very
	       unpredictable way because of special role of = tests in
	       FP code.  */
	    if (FLOAT_MODE_P (GET_MODE (XEXP (cond, 0))))
	      ;
	    /* Comparisons with 0 are often used for booleans and there is
	       nothing useful to predict about them.  */
	    else if (XEXP (cond, 1) == const0_rtx
		     || XEXP (cond, 0) == const0_rtx)
	      ;
	    else
	      predict_insn_def (last_insn, PRED_OPCODE_NONEQUAL, NOT_TAKEN);
	    break;

	  case NE:
	  case LTGT:
	    /* Floating point comparisons appears to behave in a very
	       unpredictable way because of special role of = tests in
	       FP code.  */
	    if (FLOAT_MODE_P (GET_MODE (XEXP (cond, 0))))
	      ;
	    /* Comparisons with 0 are often used for booleans and there is
	       nothing useful to predict about them.  */
	    else if (XEXP (cond, 1) == const0_rtx
		     || XEXP (cond, 0) == const0_rtx)
	      ;
	    else
	      predict_insn_def (last_insn, PRED_OPCODE_NONEQUAL, TAKEN);
	    break;

	  case ORDERED:
	    predict_insn_def (last_insn, PRED_FPOPCODE, TAKEN);
	    break;

	  case UNORDERED:
	    predict_insn_def (last_insn, PRED_FPOPCODE, NOT_TAKEN);
	    break;

	  case LE:
	  case LT:
	    if (XEXP (cond, 1) == const0_rtx || XEXP (cond, 1) == const1_rtx
		|| XEXP (cond, 1) == constm1_rtx)
	      predict_insn_def (last_insn, PRED_OPCODE_POSITIVE, NOT_TAKEN);
	    break;

	  case GE:
	  case GT:
	    if (XEXP (cond, 1) == const0_rtx || XEXP (cond, 1) == const1_rtx
		|| XEXP (cond, 1) == constm1_rtx)
	      predict_insn_def (last_insn, PRED_OPCODE_POSITIVE, TAKEN);
	    break;

	  default:
	    break;
	  }
    }

  /* Attach the combined probability to each conditional jump.  */
  FOR_EACH_BB (bb)
    if (GET_CODE (BB_END (bb)) == JUMP_INSN
	&& any_condjump_p (BB_END (bb))
	&& bb->succ->succ_next != NULL)
      combine_predictions_for_insn (BB_END (bb), bb);

  free_dominance_info (CDI_POST_DOMINATORS);

  remove_fake_edges ();
  estimate_bb_frequencies (loops_info);
}

/* __builtin_expect dropped tokens into the insn stream describing expected
   values of registers.  Generate branch probabilities based off these
   values.  */

void
expected_value_to_br_prob (void)
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
	      delete_insn (insn);
	    }
	  continue;

	case CODE_LABEL:
	  /* Never propagate across labels.  */
	  ev = NULL_RTX;
	  continue;

	case JUMP_INSN:
	  /* Look for simple conditional branches.  If we haven't got an
	     expected value yet, no point going further.  */
	  if (GET_CODE (insn) != JUMP_INSN || ev == NULL_RTX
	      || ! any_condjump_p (insn))
	    continue;
	  break;

	default:
	  /* Look for insns that clobber the EV register.  */
	  if (ev && reg_set_p (ev_reg, insn))
	    ev = NULL_RTX;
	  continue;
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
      cond = XEXP (SET_SRC (pc_set (insn)), 0);
      cond = canonicalize_condition (insn, cond, 0, NULL, ev_reg, false);
      if (! cond || XEXP (cond, 0) != ev_reg
	  || GET_CODE (XEXP (cond, 1)) != CONST_INT)
	continue;

      /* Substitute and simplify.  Given that the expression we're
	 building involves two constants, we should wind up with either
	 true or false.  */
      cond = gen_rtx_fmt_ee (GET_CODE (cond), VOIDmode,
			     XEXP (ev, 1), XEXP (cond, 1));
      cond = simplify_rtx (cond);

      /* Turn the condition into a scaled branch probability.  */
      if (cond != const_true_rtx && cond != const0_rtx)
	abort ();
      predict_insn_def (insn, PRED_BUILTIN_EXPECT,
		        cond == const_true_rtx ? TAKEN : NOT_TAKEN);
    }
}

/* Check whether this is the last basic block of function.  Commonly
   there is one extra common cleanup block.  */
static bool
last_basic_block_p (basic_block bb)
{
  if (bb == EXIT_BLOCK_PTR)
    return false;

  return (bb->next_bb == EXIT_BLOCK_PTR
	  || (bb->next_bb->next_bb == EXIT_BLOCK_PTR
	      && bb->succ && !bb->succ->succ_next
	      && bb->succ->dest->next_bb == EXIT_BLOCK_PTR));
}

/* Sets branch probabilities according to PREDiction and
   FLAGS. HEADS[bb->index] should be index of basic block in that we
   need to alter branch predictions (i.e. the first of our dominators
   such that we do not post-dominate it) (but we fill this information
   on demand, so -1 may be there in case this was not needed yet).  */

static void
process_note_prediction (basic_block bb, int *heads, int pred, int flags)
{
  edge e;
  int y;
  bool taken;

  taken = flags & IS_TAKEN;

  if (heads[bb->index] < 0)
    {
      /* This is first time we need this field in heads array; so
         find first dominator that we do not post-dominate (we are
         using already known members of heads array).  */
      basic_block ai = bb;
      basic_block next_ai = get_immediate_dominator (CDI_DOMINATORS, bb);
      int head;

      while (heads[next_ai->index] < 0)
	{
	  if (!dominated_by_p (CDI_POST_DOMINATORS, next_ai, bb))
	    break;
	  heads[next_ai->index] = ai->index;
	  ai = next_ai;
	  next_ai = get_immediate_dominator (CDI_DOMINATORS, next_ai);
	}
      if (!dominated_by_p (CDI_POST_DOMINATORS, next_ai, bb))
	head = next_ai->index;
      else
	head = heads[next_ai->index];
      while (next_ai != bb)
	{
	  next_ai = ai;
	  if (heads[ai->index] == ENTRY_BLOCK)
	    ai = ENTRY_BLOCK_PTR;
	  else
	    ai = BASIC_BLOCK (heads[ai->index]);
	  heads[next_ai->index] = head;
	}
    }
  y = heads[bb->index];

  /* Now find the edge that leads to our branch and aply the prediction.  */

  if (y == last_basic_block || !can_predict_insn_p (BB_END (BASIC_BLOCK (y))))
    return;
  for (e = BASIC_BLOCK (y)->succ; e; e = e->succ_next)
    if (e->dest->index >= 0
	&& dominated_by_p (CDI_POST_DOMINATORS, e->dest, bb))
      predict_edge_def (e, pred, taken);
}

/* Gathers NOTE_INSN_PREDICTIONs in given basic block and turns them
   into branch probabilities.  For description of heads array, see
   process_note_prediction.  */

static void
process_note_predictions (basic_block bb, int *heads)
{
  rtx insn;
  edge e;

  /* Additionally, we check here for blocks with no successors.  */
  int contained_noreturn_call = 0;
  int was_bb_head = 0;
  int noreturn_block = 1;

  for (insn = BB_END (bb); insn;
       was_bb_head |= (insn == BB_HEAD (bb)), insn = PREV_INSN (insn))
    {
      if (GET_CODE (insn) != NOTE)
	{
	  if (was_bb_head)
	    break;
	  else
	    {
	      /* Noreturn calls cause program to exit, therefore they are
	         always predicted as not taken.  */
	      if (GET_CODE (insn) == CALL_INSN
		  && find_reg_note (insn, REG_NORETURN, NULL))
		contained_noreturn_call = 1;
	      continue;
	    }
	}
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_PREDICTION)
	{
	  int alg = (int) NOTE_PREDICTION_ALG (insn);
	  /* Process single prediction note.  */
	  process_note_prediction (bb,
				   heads,
				   alg, (int) NOTE_PREDICTION_FLAGS (insn));
	  delete_insn (insn);
	}
    }
  for (e = bb->succ; e; e = e->succ_next)
    if (!(e->flags & EDGE_FAKE))
      noreturn_block = 0;
  if (contained_noreturn_call)
    {
      /* This block ended from other reasons than because of return.
         If it is because of noreturn call, this should certainly not
         be taken.  Otherwise it is probably some error recovery.  */
      process_note_prediction (bb, heads, PRED_NORETURN, NOT_TAKEN);
    }
}

/* Gathers NOTE_INSN_PREDICTIONs and turns them into
   branch probabilities.  */

void
note_prediction_to_br_prob (void)
{
  basic_block bb;
  int *heads;

  /* To enable handling of noreturn blocks.  */
  add_noreturn_fake_exit_edges ();
  connect_infinite_loops_to_exit ();

  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);

  heads = xmalloc (sizeof (int) * last_basic_block);
  memset (heads, -1, sizeof (int) * last_basic_block);
  heads[ENTRY_BLOCK_PTR->next_bb->index] = last_basic_block;

  /* Process all prediction notes.  */

  FOR_EACH_BB (bb)
    process_note_predictions (bb, heads);

  free_dominance_info (CDI_POST_DOMINATORS);
  free_dominance_info (CDI_DOMINATORS);
  free (heads);

  remove_fake_edges ();
}

/* This is used to carry information about basic blocks.  It is
   attached to the AUX field of the standard CFG block.  */

typedef struct block_info_def
{
  /* Estimated frequency of execution of basic_block.  */
  sreal frequency;

  /* To keep queue of basic blocks to process.  */
  basic_block next;

  /* True if block needs to be visited in propagate_freq.  */
  unsigned int tovisit:1;

  /* Number of predecessors we need to visit first.  */
  int npredecessors;
} *block_info;

/* Similar information for edges.  */
typedef struct edge_info_def
{
  /* In case edge is an loopback edge, the probability edge will be reached
     in case header is.  Estimated number of iterations of the loop can be
     then computed as 1 / (1 - back_edge_prob).  */
  sreal back_edge_prob;
  /* True if the edge is an loopback edge in the natural loop.  */
  unsigned int back_edge:1;
} *edge_info;

#define BLOCK_INFO(B)	((block_info) (B)->aux)
#define EDGE_INFO(E)	((edge_info) (E)->aux)

/* Helper function for estimate_bb_frequencies.
   Propagate the frequencies for LOOP.  */

static void
propagate_freq (struct loop *loop)
{
  basic_block head = loop->header;
  basic_block bb;
  basic_block last;
  edge e;
  basic_block nextbb;

  /* For each basic block we need to visit count number of his predecessors
     we need to visit first.  */
  FOR_EACH_BB (bb)
    {
      if (BLOCK_INFO (bb)->tovisit)
	{
	  int count = 0;

	  for (e = bb->pred; e; e = e->pred_next)
	    if (BLOCK_INFO (e->src)->tovisit && !(e->flags & EDGE_DFS_BACK))
	      count++;
	    else if (BLOCK_INFO (e->src)->tovisit
		     && rtl_dump_file && !EDGE_INFO (e)->back_edge)
	      fprintf (rtl_dump_file,
		       "Irreducible region hit, ignoring edge to %i->%i\n",
		       e->src->index, bb->index);
	  BLOCK_INFO (bb)->npredecessors = count;
	}
    }

  memcpy (&BLOCK_INFO (head)->frequency, &real_one, sizeof (real_one));
  last = head;
  for (bb = head; bb; bb = nextbb)
    {
      sreal cyclic_probability, frequency;

      memcpy (&cyclic_probability, &real_zero, sizeof (real_zero));
      memcpy (&frequency, &real_zero, sizeof (real_zero));

      nextbb = BLOCK_INFO (bb)->next;
      BLOCK_INFO (bb)->next = NULL;

      /* Compute frequency of basic block.  */
      if (bb != head)
	{
#ifdef ENABLE_CHECKING
	  for (e = bb->pred; e; e = e->pred_next)
	    if (BLOCK_INFO (e->src)->tovisit && !(e->flags & EDGE_DFS_BACK))
	      abort ();
#endif

	  for (e = bb->pred; e; e = e->pred_next)
	    if (EDGE_INFO (e)->back_edge)
	      {
		sreal_add (&cyclic_probability, &cyclic_probability,
			   &EDGE_INFO (e)->back_edge_prob);
	      }
	    else if (!(e->flags & EDGE_DFS_BACK))
	      {
		sreal tmp;

		/*  frequency += (e->probability
				  * BLOCK_INFO (e->src)->frequency /
				  REG_BR_PROB_BASE);  */

		sreal_init (&tmp, e->probability, 0);
		sreal_mul (&tmp, &tmp, &BLOCK_INFO (e->src)->frequency);
		sreal_mul (&tmp, &tmp, &real_inv_br_prob_base);
		sreal_add (&frequency, &frequency, &tmp);
	      }

	  if (sreal_compare (&cyclic_probability, &real_zero) == 0)
	    {
	      memcpy (&BLOCK_INFO (bb)->frequency, &frequency,
		      sizeof (frequency));
	    }
	  else
	    {
	      if (sreal_compare (&cyclic_probability, &real_almost_one) > 0)
		{
		  memcpy (&cyclic_probability, &real_almost_one,
			  sizeof (real_almost_one));
		}

	      /* BLOCK_INFO (bb)->frequency = frequency
					      / (1 - cyclic_probability) */

	      sreal_sub (&cyclic_probability, &real_one, &cyclic_probability);
	      sreal_div (&BLOCK_INFO (bb)->frequency,
			 &frequency, &cyclic_probability);
	    }
	}

      BLOCK_INFO (bb)->tovisit = 0;

      /* Compute back edge frequencies.  */
      for (e = bb->succ; e; e = e->succ_next)
	if (e->dest == head)
	  {
	    sreal tmp;

	    /* EDGE_INFO (e)->back_edge_prob
		  = ((e->probability * BLOCK_INFO (bb)->frequency)
		     / REG_BR_PROB_BASE); */

	    sreal_init (&tmp, e->probability, 0);
	    sreal_mul (&tmp, &tmp, &BLOCK_INFO (bb)->frequency);
	    sreal_mul (&EDGE_INFO (e)->back_edge_prob,
		       &tmp, &real_inv_br_prob_base);
	  }

      /* Propagate to successor blocks.  */
      for (e = bb->succ; e; e = e->succ_next)
	if (!(e->flags & EDGE_DFS_BACK)
	    && BLOCK_INFO (e->dest)->npredecessors)
	  {
	    BLOCK_INFO (e->dest)->npredecessors--;
	    if (!BLOCK_INFO (e->dest)->npredecessors)
	      {
		if (!nextbb)
		  nextbb = e->dest;
		else
		  BLOCK_INFO (last)->next = e->dest;

		last = e->dest;
	      }
	   }
    }
}

/* Estimate probabilities of loopback edges in loops at same nest level.  */

static void
estimate_loops_at_level (struct loop *first_loop)
{
  struct loop *loop;

  for (loop = first_loop; loop; loop = loop->next)
    {
      edge e;
      basic_block *bbs;
      unsigned i;

      estimate_loops_at_level (loop->inner);

      if (loop->latch->succ)  /* Do not do this for dummy function loop.  */
	{
	  /* Find current loop back edge and mark it.  */
	  e = loop_latch_edge (loop);
	  EDGE_INFO (e)->back_edge = 1;
       }

      bbs = get_loop_body (loop);
      for (i = 0; i < loop->num_nodes; i++)
	BLOCK_INFO (bbs[i])->tovisit = 1;
      free (bbs);
      propagate_freq (loop);
    }
}

/* Convert counts measured by profile driven feedback to frequencies.  */

static void
counts_to_freqs (void)
{
  gcov_type count_max = 1;
  basic_block bb;

  FOR_EACH_BB (bb)
    count_max = MAX (bb->count, count_max);

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->frequency = (bb->count * BB_FREQ_MAX + count_max / 2) / count_max;
}

/* Return true if function is likely to be expensive, so there is no point to
   optimize performance of prologue, epilogue or do inlining at the expense
   of code size growth.  THRESHOLD is the limit of number of instructions
   function can execute at average to be still considered not expensive.  */

bool
expensive_function_p (int threshold)
{
  unsigned int sum = 0;
  basic_block bb;
  unsigned int limit;

  /* We can not compute accurately for large thresholds due to scaled
     frequencies.  */
  if (threshold > BB_FREQ_MAX)
    abort ();

  /* Frequencies are out of range.  This either means that function contains
     internal loop executing more than BB_FREQ_MAX times or profile feedback
     is available and function has not been executed at all.  */
  if (ENTRY_BLOCK_PTR->frequency == 0)
    return true;

  /* Maximally BB_FREQ_MAX^2 so overflow won't happen.  */
  limit = ENTRY_BLOCK_PTR->frequency * threshold;
  FOR_EACH_BB (bb)
    {
      rtx insn;

      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	if (active_insn_p (insn))
	  {
	    sum += bb->frequency;
	    if (sum > limit)
	      return true;
	}
    }

  return false;
}

/* Estimate basic blocks frequency by given branch probabilities.  */

static void
estimate_bb_frequencies (struct loops *loops)
{
  basic_block bb;
  sreal freq_max;

  if (flag_branch_probabilities)
    counts_to_freqs ();
  else
    {
      static int real_values_initialized = 0;

      if (!real_values_initialized)
        {
	  real_values_initialized = 1;
	  sreal_init (&real_zero, 0, 0);
	  sreal_init (&real_one, 1, 0);
	  sreal_init (&real_br_prob_base, REG_BR_PROB_BASE, 0);
	  sreal_init (&real_bb_freq_max, BB_FREQ_MAX, 0);
	  sreal_init (&real_one_half, 1, -1);
	  sreal_div (&real_inv_br_prob_base, &real_one, &real_br_prob_base);
	  sreal_sub (&real_almost_one, &real_one, &real_inv_br_prob_base);
	}

      mark_dfs_back_edges ();
      /* Fill in the probability values in flowgraph based on the REG_BR_PROB
         notes.  */
      FOR_EACH_BB (bb)
	{
	  rtx last_insn = BB_END (bb);

	  if (!can_predict_insn_p (last_insn))
	    {
	      /* We can predict only conditional jumps at the moment.
	         Expect each edge to be equally probable.
	         ?? In the future we want to make abnormal edges improbable.  */
	      int nedges = 0;
	      edge e;

	      for (e = bb->succ; e; e = e->succ_next)
		{
		  nedges++;
		  if (e->probability != 0)
		    break;
		}
	      if (!e)
		for (e = bb->succ; e; e = e->succ_next)
		  e->probability = (REG_BR_PROB_BASE + nedges / 2) / nedges;
	    }
	}

      ENTRY_BLOCK_PTR->succ->probability = REG_BR_PROB_BASE;

      /* Set up block info for each basic block.  */
      alloc_aux_for_blocks (sizeof (struct block_info_def));
      alloc_aux_for_edges (sizeof (struct edge_info_def));
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
	{
	  edge e;

	  BLOCK_INFO (bb)->tovisit = 0;
	  for (e = bb->succ; e; e = e->succ_next)
	    {
	      sreal_init (&EDGE_INFO (e)->back_edge_prob, e->probability, 0);
	      sreal_mul (&EDGE_INFO (e)->back_edge_prob,
			 &EDGE_INFO (e)->back_edge_prob,
			 &real_inv_br_prob_base);
	    }
	}

      /* First compute probabilities locally for each loop from innermost
         to outermost to examine probabilities for back edges.  */
      estimate_loops_at_level (loops->tree_root);

      memcpy (&freq_max, &real_zero, sizeof (real_zero));
      FOR_EACH_BB (bb)
	if (sreal_compare (&freq_max, &BLOCK_INFO (bb)->frequency) < 0)
	  memcpy (&freq_max, &BLOCK_INFO (bb)->frequency, sizeof (freq_max));

      sreal_div (&freq_max, &real_bb_freq_max, &freq_max);
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
	{
	  sreal tmp;

	  sreal_mul (&tmp, &BLOCK_INFO (bb)->frequency, &freq_max);
	  sreal_add (&tmp, &tmp, &real_one_half);
	  bb->frequency = sreal_to_int (&tmp);
	}

      free_aux_for_blocks ();
      free_aux_for_edges ();
    }
  compute_function_frequency ();
  if (flag_reorder_functions)
    choose_function_section ();
}

/* Decide whether function is hot, cold or unlikely executed.  */
static void
compute_function_frequency (void)
{
  basic_block bb;

  if (!profile_info || !flag_branch_probabilities)
    return;
  cfun->function_frequency = FUNCTION_FREQUENCY_UNLIKELY_EXECUTED;
  FOR_EACH_BB (bb)
    {
      if (maybe_hot_bb_p (bb))
	{
	  cfun->function_frequency = FUNCTION_FREQUENCY_HOT;
	  return;
	}
      if (!probably_never_executed_bb_p (bb))
	cfun->function_frequency = FUNCTION_FREQUENCY_NORMAL;
    }
}

/* Choose appropriate section for the function.  */
static void
choose_function_section (void)
{
  if (DECL_SECTION_NAME (current_function_decl)
      || !targetm.have_named_sections
      /* Theoretically we can split the gnu.linkonce text section too,
	 but this requires more work as the frequency needs to match
	 for all generated objects so we need to merge the frequency
	 of all instances.  For now just never set frequency for these.  */
      || DECL_ONE_ONLY (current_function_decl))
    return;
  if (cfun->function_frequency == FUNCTION_FREQUENCY_HOT)
    DECL_SECTION_NAME (current_function_decl) =
      build_string (strlen (HOT_TEXT_SECTION_NAME), HOT_TEXT_SECTION_NAME);
  if (cfun->function_frequency == FUNCTION_FREQUENCY_UNLIKELY_EXECUTED)
    DECL_SECTION_NAME (current_function_decl) =
      build_string (strlen (UNLIKELY_EXECUTED_TEXT_SECTION_NAME),
		    UNLIKELY_EXECUTED_TEXT_SECTION_NAME);
}
