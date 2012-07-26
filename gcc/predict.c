/* Branch prediction routines for the GNU compiler.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
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
#include "function.h"
#include "except.h"
#include "diagnostic-core.h"
#include "recog.h"
#include "expr.h"
#include "predict.h"
#include "coverage.h"
#include "sreal.h"
#include "params.h"
#include "target.h"
#include "cfgloop.h"
#include "tree-flow.h"
#include "ggc.h"
#include "tree-pass.h"
#include "tree-scalar-evolution.h"
#include "cfgloop.h"
#include "pointer-set.h"

/* real constants: 0, 1, 1-1/REG_BR_PROB_BASE, REG_BR_PROB_BASE,
		   1/REG_BR_PROB_BASE, 0.5, BB_FREQ_MAX.  */
static sreal real_zero, real_one, real_almost_one, real_br_prob_base,
	     real_inv_br_prob_base, real_one_half, real_bb_freq_max;

/* Random guesstimation given names.
   PROV_VERY_UNLIKELY should be small enough so basic block predicted
   by it gets bellow HOT_BB_FREQUENCY_FRANCTION.  */
#define PROB_VERY_UNLIKELY	(REG_BR_PROB_BASE / 2000 - 1)
#define PROB_EVEN		(REG_BR_PROB_BASE / 2)
#define PROB_VERY_LIKELY	(REG_BR_PROB_BASE - PROB_VERY_UNLIKELY)
#define PROB_ALWAYS		(REG_BR_PROB_BASE)

static void combine_predictions_for_insn (rtx, basic_block);
static void dump_prediction (FILE *, enum br_predictor, int, basic_block, int);
static void predict_paths_leading_to (basic_block, enum br_predictor, enum prediction);
static void predict_paths_leading_to_edge (edge, enum br_predictor, enum prediction);
static bool can_predict_insn_p (const_rtx);

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

/* Return TRUE if frequency FREQ is considered to be hot.  */

static inline bool
maybe_hot_frequency_p (int freq)
{
  struct cgraph_node *node = cgraph_get_node (current_function_decl);
  if (!profile_info || !flag_branch_probabilities)
    {
      if (node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
        return false;
      if (node->frequency == NODE_FREQUENCY_HOT)
        return true;
    }
  if (profile_status == PROFILE_ABSENT)
    return true;
  if (node->frequency == NODE_FREQUENCY_EXECUTED_ONCE
      && freq < (ENTRY_BLOCK_PTR->frequency * 2 / 3))
    return false;
  if (freq < ENTRY_BLOCK_PTR->frequency / PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION))
    return false;
  return true;
}

/* Return TRUE if frequency FREQ is considered to be hot.  */

static inline bool
maybe_hot_count_p (gcov_type count)
{
  if (profile_status != PROFILE_READ)
    return true;
  /* Code executed at most once is not hot.  */
  if (profile_info->runs >= count)
    return false;
  return (count
	  > profile_info->sum_max / PARAM_VALUE (HOT_BB_COUNT_FRACTION));
}

/* Return true in case BB can be CPU intensive and should be optimized
   for maximal performance.  */

bool
maybe_hot_bb_p (const_basic_block bb)
{
  /* Make sure CFUN exists, for dump_bb_info.  */
  gcc_assert (cfun);
  if (profile_status == PROFILE_READ)
    return maybe_hot_count_p (bb->count);
  return maybe_hot_frequency_p (bb->frequency);
}

/* Return true if the call can be hot.  */

bool
cgraph_maybe_hot_edge_p (struct cgraph_edge *edge)
{
  if (profile_info && flag_branch_probabilities
      && (edge->count
	  <= profile_info->sum_max / PARAM_VALUE (HOT_BB_COUNT_FRACTION)))
    return false;
  if (edge->caller->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED
      || edge->callee->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
    return false;
  if (edge->caller->frequency > NODE_FREQUENCY_UNLIKELY_EXECUTED
      && edge->callee->frequency <= NODE_FREQUENCY_EXECUTED_ONCE)
    return false;
  if (optimize_size)
    return false;
  if (edge->caller->frequency == NODE_FREQUENCY_HOT)
    return true;
  if (edge->caller->frequency == NODE_FREQUENCY_EXECUTED_ONCE
      && edge->frequency < CGRAPH_FREQ_BASE * 3 / 2)
    return false;
  if (flag_guess_branch_prob
      && edge->frequency <= (CGRAPH_FREQ_BASE
      			     / PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION)))
    return false;
  return true;
}

/* Return true in case BB can be CPU intensive and should be optimized
   for maximal performance.  */

bool
maybe_hot_edge_p (edge e)
{
  if (profile_status == PROFILE_READ)
    return maybe_hot_count_p (e->count);
  return maybe_hot_frequency_p (EDGE_FREQUENCY (e));
}


/* Return true in case BB is probably never executed.  */

bool
probably_never_executed_bb_p (const_basic_block bb)
{
  /* Make sure CFUN exists, for dump_bb_info.  */
  gcc_assert (cfun);
  if (profile_info && flag_branch_probabilities)
    return ((bb->count + profile_info->runs / 2) / profile_info->runs) == 0;
  if ((!profile_info || !flag_branch_probabilities)
      && (cgraph_get_node (current_function_decl)->frequency
	  == NODE_FREQUENCY_UNLIKELY_EXECUTED))
    return true;
  return false;
}

/* Return true if NODE should be optimized for size.  */

bool
cgraph_optimize_for_size_p (struct cgraph_node *node)
{
  if (optimize_size)
    return true;
  if (node && (node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED))
    return true;
  else
    return false;
}

/* Return true when current function should always be optimized for size.  */

bool
optimize_function_for_size_p (struct function *fun)
{
  if (optimize_size)
    return true;
  if (!fun || !fun->decl)
    return false;
  return cgraph_optimize_for_size_p (cgraph_get_node (fun->decl));
}

/* Return true when current function should always be optimized for speed.  */

bool
optimize_function_for_speed_p (struct function *fun)
{
  return !optimize_function_for_size_p (fun);
}

/* Return TRUE when BB should be optimized for size.  */

bool
optimize_bb_for_size_p (const_basic_block bb)
{
  return optimize_function_for_size_p (cfun) || !maybe_hot_bb_p (bb);
}

/* Return TRUE when BB should be optimized for speed.  */

bool
optimize_bb_for_speed_p (const_basic_block bb)
{
  return !optimize_bb_for_size_p (bb);
}

/* Return TRUE when BB should be optimized for size.  */

bool
optimize_edge_for_size_p (edge e)
{
  return optimize_function_for_size_p (cfun) || !maybe_hot_edge_p (e);
}

/* Return TRUE when BB should be optimized for speed.  */

bool
optimize_edge_for_speed_p (edge e)
{
  return !optimize_edge_for_size_p (e);
}

/* Return TRUE when BB should be optimized for size.  */

bool
optimize_insn_for_size_p (void)
{
  return optimize_function_for_size_p (cfun) || !crtl->maybe_hot_insn_p;
}

/* Return TRUE when BB should be optimized for speed.  */

bool
optimize_insn_for_speed_p (void)
{
  return !optimize_insn_for_size_p ();
}

/* Return TRUE when LOOP should be optimized for size.  */

bool
optimize_loop_for_size_p (struct loop *loop)
{
  return optimize_bb_for_size_p (loop->header);
}

/* Return TRUE when LOOP should be optimized for speed.  */

bool
optimize_loop_for_speed_p (struct loop *loop)
{
  return optimize_bb_for_speed_p (loop->header);
}

/* Return TRUE when LOOP nest should be optimized for speed.  */

bool
optimize_loop_nest_for_speed_p (struct loop *loop)
{
  struct loop *l = loop;
  if (optimize_loop_for_speed_p (loop))
    return true;
  l = loop->inner;
  while (l && l != loop)
    {
      if (optimize_loop_for_speed_p (l))
        return true;
      if (l->inner)
        l = l->inner;
      else if (l->next)
        l = l->next;
      else
        {
	  while (l != loop && !l->next)
	    l = loop_outer (l);
	  if (l != loop)
	    l = l->next;
	}
    }
  return false;
}

/* Return TRUE when LOOP nest should be optimized for size.  */

bool
optimize_loop_nest_for_size_p (struct loop *loop)
{
  return !optimize_loop_nest_for_speed_p (loop);
}

/* Return true when edge E is likely to be well predictable by branch
   predictor.  */

bool
predictable_edge_p (edge e)
{
  if (profile_status == PROFILE_ABSENT)
    return false;
  if ((e->probability
       <= PARAM_VALUE (PARAM_PREDICTABLE_BRANCH_OUTCOME) * REG_BR_PROB_BASE / 100)
      || (REG_BR_PROB_BASE - e->probability
          <= PARAM_VALUE (PARAM_PREDICTABLE_BRANCH_OUTCOME) * REG_BR_PROB_BASE / 100))
    return true;
  return false;
}


/* Set RTL expansion for BB profile.  */

void
rtl_profile_for_bb (basic_block bb)
{
  crtl->maybe_hot_insn_p = maybe_hot_bb_p (bb);
}

/* Set RTL expansion for edge profile.  */

void
rtl_profile_for_edge (edge e)
{
  crtl->maybe_hot_insn_p = maybe_hot_edge_p (e);
}

/* Set RTL expansion to default mode (i.e. when profile info is not known).  */
void
default_rtl_profile (void)
{
  crtl->maybe_hot_insn_p = true;
}

/* Return true if the one of outgoing edges is already predicted by
   PREDICTOR.  */

bool
rtl_predicted_by_p (const_basic_block bb, enum br_predictor predictor)
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

/* This map contains for a basic block the list of predictions for the
   outgoing edges.  */

static struct pointer_map_t *bb_predictions;

/*  Structure representing predictions in tree level. */

struct edge_prediction {
    struct edge_prediction *ep_next;
    edge ep_edge;
    enum br_predictor ep_predictor;
    int ep_probability;
};

/* Return true if the one of outgoing edges is already predicted by
   PREDICTOR.  */

bool
gimple_predicted_by_p (const_basic_block bb, enum br_predictor predictor)
{
  struct edge_prediction *i;
  void **preds = pointer_map_contains (bb_predictions, bb);

  if (!preds)
    return false;

  for (i = (struct edge_prediction *) *preds; i; i = i->ep_next)
    if (i->ep_predictor == predictor)
      return true;
  return false;
}

/* Return true when the probability of edge is reliable.

   The profile guessing code is good at predicting branch outcome (ie.
   taken/not taken), that is predicted right slightly over 75% of time.
   It is however notoriously poor on predicting the probability itself.
   In general the profile appear a lot flatter (with probabilities closer
   to 50%) than the reality so it is bad idea to use it to drive optimization
   such as those disabling dynamic branch prediction for well predictable
   branches.

   There are two exceptions - edges leading to noreturn edges and edges
   predicted by number of iterations heuristics are predicted well.  This macro
   should be able to distinguish those, but at the moment it simply check for
   noreturn heuristic that is only one giving probability over 99% or bellow
   1%.  In future we might want to propagate reliability information across the
   CFG if we find this information useful on multiple places.   */
static bool
probability_reliable_p (int prob)
{
  return (profile_status == PROFILE_READ
	  || (profile_status == PROFILE_GUESSED
	      && (prob <= HITRATE (1) || prob >= HITRATE (99))));
}

/* Same predicate as above, working on edges.  */
bool
edge_probability_reliable_p (const_edge e)
{
  return probability_reliable_p (e->probability);
}

/* Same predicate as edge_probability_reliable_p, working on notes.  */
bool
br_prob_note_reliable_p (const_rtx note)
{
  gcc_assert (REG_NOTE_KIND (note) == REG_BR_PROB);
  return probability_reliable_p (INTVAL (XEXP (note, 0)));
}

static void
predict_insn (rtx insn, enum br_predictor predictor, int probability)
{
  gcc_assert (any_condjump_p (insn));
  if (!flag_guess_branch_prob)
    return;

  add_reg_note (insn, REG_BR_PRED,
		gen_rtx_CONCAT (VOIDmode,
				GEN_INT ((int) predictor),
				GEN_INT ((int) probability)));
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
rtl_predict_edge (edge e, enum br_predictor predictor, int probability)
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

/* Predict edge E with the given PROBABILITY.  */
void
gimple_predict_edge (edge e, enum br_predictor predictor, int probability)
{
  gcc_assert (profile_status != PROFILE_GUESSED);
  if ((e->src != ENTRY_BLOCK_PTR && EDGE_COUNT (e->src->succs) > 1)
      && flag_guess_branch_prob && optimize)
    {
      struct edge_prediction *i = XNEW (struct edge_prediction);
      void **preds = pointer_map_insert (bb_predictions, e->src);

      i->ep_next = (struct edge_prediction *) *preds;
      *preds = i;
      i->ep_probability = probability;
      i->ep_predictor = predictor;
      i->ep_edge = e;
    }
}

/* Remove all predictions on given basic block that are attached
   to edge E.  */
void
remove_predictions_associated_with_edge (edge e)
{
  void **preds;

  if (!bb_predictions)
    return;

  preds = pointer_map_contains (bb_predictions, e->src);

  if (preds)
    {
      struct edge_prediction **prediction = (struct edge_prediction **) preds;
      struct edge_prediction *next;

      while (*prediction)
	{
	  if ((*prediction)->ep_edge == e)
	    {
	      next = (*prediction)->ep_next;
	      free (*prediction);
	      *prediction = next;
	    }
	  else
	    prediction = &((*prediction)->ep_next);
	}
    }
}

/* Clears the list of predictions stored for BB.  */

static void
clear_bb_predictions (basic_block bb)
{
  void **preds = pointer_map_contains (bb_predictions, bb);
  struct edge_prediction *pred, *next;

  if (!preds)
    return;

  for (pred = (struct edge_prediction *) *preds; pred; pred = next)
    {
      next = pred->ep_next;
      free (pred);
    }
  *preds = NULL;
}

/* Return true when we can store prediction on insn INSN.
   At the moment we represent predictions only on conditional
   jumps, not at computed jump or other complicated cases.  */
static bool
can_predict_insn_p (const_rtx insn)
{
  return (JUMP_P (insn)
	  && any_condjump_p (insn)
	  && EDGE_COUNT (BLOCK_FOR_INSN (insn)->succs) >= 2);
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
dump_prediction (FILE *file, enum br_predictor predictor, int probability,
		 basic_block bb, int used)
{
  edge e;
  edge_iterator ei;

  if (!file)
    return;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (! (e->flags & EDGE_FALLTHRU))
      break;

  fprintf (file, "  %s heuristics%s: %.1f%%",
	   predictor_info[predictor].name,
	   used ? "" : " (ignored)", probability * 100.0 / REG_BR_PROB_BASE);

  if (bb->count)
    {
      fprintf (file, "  exec ");
      fprintf (file, HOST_WIDEST_INT_PRINT_DEC, bb->count);
      if (e)
	{
	  fprintf (file, " hit ");
	  fprintf (file, HOST_WIDEST_INT_PRINT_DEC, e->count);
	  fprintf (file, " (%.1f%%)", e->count * 100.0 / bb->count);
	}
    }

  fprintf (file, "\n");
}

/* We can not predict the probabilities of outgoing edges of bb.  Set them
   evenly and hope for the best.  */
static void
set_even_probabilities (basic_block bb)
{
  int nedges = 0;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & (EDGE_EH | EDGE_FAKE)))
      nedges ++;
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & (EDGE_EH | EDGE_FAKE)))
      e->probability = (REG_BR_PROB_BASE + nedges / 2) / nedges;
    else
      e->probability = 0;
}

/* Combine all REG_BR_PRED notes into single probability and attach REG_BR_PROB
   note if not already present.  Remove now useless REG_BR_PRED notes.  */

static void
combine_predictions_for_insn (rtx insn, basic_block bb)
{
  rtx prob_note;
  rtx *pnote;
  rtx note;
  int best_probability = PROB_EVEN;
  enum br_predictor best_predictor = END_PREDICTORS;
  int combined_probability = REG_BR_PROB_BASE / 2;
  int d;
  bool first_match = false;
  bool found = false;

  if (!can_predict_insn_p (insn))
    {
      set_even_probabilities (bb);
      return;
    }

  prob_note = find_reg_note (insn, REG_BR_PROB, 0);
  pnote = &REG_NOTES (insn);
  if (dump_file)
    fprintf (dump_file, "Predictions for insn %i bb %i\n", INSN_UID (insn),
	     bb->index);

  /* We implement "first match" heuristics and use probability guessed
     by predictor with smallest index.  */
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_BR_PRED)
      {
	enum br_predictor predictor = ((enum br_predictor)
				       INTVAL (XEXP (XEXP (note, 0), 0)));
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
    dump_prediction (dump_file, PRED_NO_PREDICTION,
		     combined_probability, bb, true);
  else
    {
      dump_prediction (dump_file, PRED_DS_THEORY, combined_probability,
		       bb, !first_match);
      dump_prediction (dump_file, PRED_FIRST_MATCH, best_probability,
		       bb, first_match);
    }

  if (first_match)
    combined_probability = best_probability;
  dump_prediction (dump_file, PRED_COMBINED, combined_probability, bb, true);

  while (*pnote)
    {
      if (REG_NOTE_KIND (*pnote) == REG_BR_PRED)
	{
	  enum br_predictor predictor = ((enum br_predictor)
					 INTVAL (XEXP (XEXP (*pnote, 0), 0)));
	  int probability = INTVAL (XEXP (XEXP (*pnote, 0), 1));

	  dump_prediction (dump_file, predictor, probability, bb,
			   !first_match || best_predictor == predictor);
	  *pnote = XEXP (*pnote, 1);
	}
      else
	pnote = &XEXP (*pnote, 1);
    }

  if (!prob_note)
    {
      add_reg_note (insn, REG_BR_PROB, GEN_INT (combined_probability));

      /* Save the prediction into CFG in case we are seeing non-degenerated
	 conditional jump.  */
      if (!single_succ_p (bb))
	{
	  BRANCH_EDGE (bb)->probability = combined_probability;
	  FALLTHRU_EDGE (bb)->probability
	    = REG_BR_PROB_BASE - combined_probability;
	}
    }
  else if (!single_succ_p (bb))
    {
      int prob = INTVAL (XEXP (prob_note, 0));

      BRANCH_EDGE (bb)->probability = prob;
      FALLTHRU_EDGE (bb)->probability = REG_BR_PROB_BASE - prob;
    }
  else
    single_succ_edge (bb)->probability = REG_BR_PROB_BASE;
}

/* Combine predictions into single probability and store them into CFG.
   Remove now useless prediction entries.  */

static void
combine_predictions_for_bb (basic_block bb)
{
  int best_probability = PROB_EVEN;
  enum br_predictor best_predictor = END_PREDICTORS;
  int combined_probability = REG_BR_PROB_BASE / 2;
  int d;
  bool first_match = false;
  bool found = false;
  struct edge_prediction *pred;
  int nedges = 0;
  edge e, first = NULL, second = NULL;
  edge_iterator ei;
  void **preds;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & (EDGE_EH | EDGE_FAKE)))
      {
	nedges ++;
	if (first && !second)
	  second = e;
	if (!first)
	  first = e;
      }

  /* When there is no successor or only one choice, prediction is easy.

     We are lazy for now and predict only basic blocks with two outgoing
     edges.  It is possible to predict generic case too, but we have to
     ignore first match heuristics and do more involved combining.  Implement
     this later.  */
  if (nedges != 2)
    {
      if (!bb->count)
	set_even_probabilities (bb);
      clear_bb_predictions (bb);
      if (dump_file)
	fprintf (dump_file, "%i edges in bb %i predicted to even probabilities\n",
		 nedges, bb->index);
      return;
    }

  if (dump_file)
    fprintf (dump_file, "Predictions for bb %i\n", bb->index);

  preds = pointer_map_contains (bb_predictions, bb);
  if (preds)
    {
      /* We implement "first match" heuristics and use probability guessed
	 by predictor with smallest index.  */
      for (pred = (struct edge_prediction *) *preds; pred; pred = pred->ep_next)
	{
	  enum br_predictor predictor = pred->ep_predictor;
	  int probability = pred->ep_probability;

	  if (pred->ep_edge != first)
	    probability = REG_BR_PROB_BASE - probability;

	  found = true;
	  /* First match heuristics would be widly confused if we predicted
	     both directions.  */
	  if (best_predictor > predictor)
	    {
              struct edge_prediction *pred2;
	      int prob = probability;

              for (pred2 = (struct edge_prediction *) *preds; pred2; pred2 = pred2->ep_next)
	       if (pred2 != pred && pred2->ep_predictor == pred->ep_predictor)
	         {
	           int probability2 = pred->ep_probability;

		   if (pred2->ep_edge != first)
		     probability2 = REG_BR_PROB_BASE - probability2;

		   if ((probability < REG_BR_PROB_BASE / 2) !=
		       (probability2 < REG_BR_PROB_BASE / 2))
		     break;

		   /* If the same predictor later gave better result, go for it! */
		   if ((probability >= REG_BR_PROB_BASE / 2 && (probability2 > probability))
		       || (probability <= REG_BR_PROB_BASE / 2 && (probability2 < probability)))
		     prob = probability2;
		 }
	      if (!pred2)
	        best_probability = prob, best_predictor = predictor;
	    }

	  d = (combined_probability * probability
	       + (REG_BR_PROB_BASE - combined_probability)
	       * (REG_BR_PROB_BASE - probability));

	  /* Use FP math to avoid overflows of 32bit integers.  */
	  if (d == 0)
	    /* If one probability is 0% and one 100%, avoid division by zero.  */
	    combined_probability = REG_BR_PROB_BASE / 2;
	  else
	    combined_probability = (((double) combined_probability)
				    * probability
		    		    * REG_BR_PROB_BASE / d + 0.5);
	}
    }

  /* Decide which heuristic to use.  In case we didn't match anything,
     use no_prediction heuristic, in case we did match, use either
     first match or Dempster-Shaffer theory depending on the flags.  */

  if (predictor_info [best_predictor].flags & PRED_FLAG_FIRST_MATCH)
    first_match = true;

  if (!found)
    dump_prediction (dump_file, PRED_NO_PREDICTION, combined_probability, bb, true);
  else
    {
      dump_prediction (dump_file, PRED_DS_THEORY, combined_probability, bb,
		       !first_match);
      dump_prediction (dump_file, PRED_FIRST_MATCH, best_probability, bb,
		       first_match);
    }

  if (first_match)
    combined_probability = best_probability;
  dump_prediction (dump_file, PRED_COMBINED, combined_probability, bb, true);

  if (preds)
    {
      for (pred = (struct edge_prediction *) *preds; pred; pred = pred->ep_next)
	{
	  enum br_predictor predictor = pred->ep_predictor;
	  int probability = pred->ep_probability;

	  if (pred->ep_edge != EDGE_SUCC (bb, 0))
	    probability = REG_BR_PROB_BASE - probability;
	  dump_prediction (dump_file, predictor, probability, bb,
			   !first_match || best_predictor == predictor);
	}
    }
  clear_bb_predictions (bb);

  if (!bb->count)
    {
      first->probability = combined_probability;
      second->probability = REG_BR_PROB_BASE - combined_probability;
    }
}

/* Check if T1 and T2 satisfy the IV_COMPARE condition.
   Return the SSA_NAME if the condition satisfies, NULL otherwise.

   T1 and T2 should be one of the following cases:
     1. T1 is SSA_NAME, T2 is NULL
     2. T1 is SSA_NAME, T2 is INTEGER_CST between [-4, 4]
     3. T2 is SSA_NAME, T1 is INTEGER_CST between [-4, 4]  */

static tree
strips_small_constant (tree t1, tree t2)
{
  tree ret = NULL;
  int value = 0;

  if (!t1)
    return NULL;
  else if (TREE_CODE (t1) == SSA_NAME)
    ret = t1;
  else if (host_integerp (t1, 0))
    value = tree_low_cst (t1, 0);
  else
    return NULL;

  if (!t2)
    return ret;
  else if (host_integerp (t2, 0))
    value = tree_low_cst (t2, 0);
  else if (TREE_CODE (t2) == SSA_NAME)
    {
      if (ret)
        return NULL;
      else
        ret = t2;
    }

  if (value <= 4 && value >= -4)
    return ret;
  else
    return NULL;
}

/* Return the SSA_NAME in T or T's operands.
   Return NULL if SSA_NAME cannot be found.  */

static tree
get_base_value (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    return t;

  if (!BINARY_CLASS_P (t))
    return NULL;

  switch (TREE_OPERAND_LENGTH (t))
    {
    case 1:
      return strips_small_constant (TREE_OPERAND (t, 0), NULL);
    case 2:
      return strips_small_constant (TREE_OPERAND (t, 0),
				    TREE_OPERAND (t, 1));
    default:
      return NULL;
    }
}

/* Check the compare STMT in LOOP. If it compares an induction
   variable to a loop invariant, return true, and save
   LOOP_INVARIANT, COMPARE_CODE and LOOP_STEP.
   Otherwise return false and set LOOP_INVAIANT to NULL.  */

static bool
is_comparison_with_loop_invariant_p (gimple stmt, struct loop *loop,
				     tree *loop_invariant,
				     enum tree_code *compare_code,
				     int *loop_step,
				     tree *loop_iv_base)
{
  tree op0, op1, bound, base;
  affine_iv iv0, iv1;
  enum tree_code code;
  int step;

  code = gimple_cond_code (stmt);
  *loop_invariant = NULL;

  switch (code)
    {
    case GT_EXPR:
    case GE_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
      break;

    default:
      return false;
    }

  op0 = gimple_cond_lhs (stmt);
  op1 = gimple_cond_rhs (stmt);

  if ((TREE_CODE (op0) != SSA_NAME && TREE_CODE (op0) != INTEGER_CST) 
       || (TREE_CODE (op1) != SSA_NAME && TREE_CODE (op1) != INTEGER_CST))
    return false;
  if (!simple_iv (loop, loop_containing_stmt (stmt), op0, &iv0, true))
    return false;
  if (!simple_iv (loop, loop_containing_stmt (stmt), op1, &iv1, true))
    return false;
  if (TREE_CODE (iv0.step) != INTEGER_CST
      || TREE_CODE (iv1.step) != INTEGER_CST)
    return false;
  if ((integer_zerop (iv0.step) && integer_zerop (iv1.step))
      || (!integer_zerop (iv0.step) && !integer_zerop (iv1.step)))
    return false;

  if (integer_zerop (iv0.step))
    {
      if (code != NE_EXPR && code != EQ_EXPR)
	code = invert_tree_comparison (code, false);
      bound = iv0.base;
      base = iv1.base;
      if (host_integerp (iv1.step, 0))
	step = tree_low_cst (iv1.step, 0);
      else
	return false;
    }
  else
    {
      bound = iv1.base;
      base = iv0.base;
      if (host_integerp (iv0.step, 0))
	step = tree_low_cst (iv0.step, 0);  
      else
	return false;
    }

  if (TREE_CODE (bound) != INTEGER_CST)
    bound = get_base_value (bound);
  if (!bound)
    return false;
  if (TREE_CODE (base) != INTEGER_CST)
    base = get_base_value (base);
  if (!base)
    return false;

  *loop_invariant = bound;
  *compare_code = code;
  *loop_step = step;
  *loop_iv_base = base;
  return true;
}

/* Compare two SSA_NAMEs: returns TRUE if T1 and T2 are value coherent.  */

static bool
expr_coherent_p (tree t1, tree t2)
{
  gimple stmt;
  tree ssa_name_1 = NULL;
  tree ssa_name_2 = NULL;

  gcc_assert (TREE_CODE (t1) == SSA_NAME || TREE_CODE (t1) == INTEGER_CST);
  gcc_assert (TREE_CODE (t2) == SSA_NAME || TREE_CODE (t2) == INTEGER_CST);

  if (t1 == t2)
    return true;

  if (TREE_CODE (t1) == INTEGER_CST && TREE_CODE (t2) == INTEGER_CST)
    return true;
  if (TREE_CODE (t1) == INTEGER_CST || TREE_CODE (t2) == INTEGER_CST)
    return false;

  /* Check to see if t1 is expressed/defined with t2.  */
  stmt = SSA_NAME_DEF_STMT (t1);
  gcc_assert (stmt != NULL);
  if (is_gimple_assign (stmt))
    {
      ssa_name_1 = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_USE);
      if (ssa_name_1 && ssa_name_1 == t2)
	return true;
    }

  /* Check to see if t2 is expressed/defined with t1.  */
  stmt = SSA_NAME_DEF_STMT (t2);
  gcc_assert (stmt != NULL);
  if (is_gimple_assign (stmt))
    {
      ssa_name_2 = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_USE);
      if (ssa_name_2 && ssa_name_2 == t1)
	return true;
    }

  /* Compare if t1 and t2's def_stmts are identical.  */
  if (ssa_name_2 != NULL && ssa_name_1 == ssa_name_2)
    return true;
  else
    return false;
}

/* Predict branch probability of BB when BB contains a branch that compares
   an induction variable in LOOP with LOOP_IV_BASE_VAR to LOOP_BOUND_VAR. The
   loop exit is compared using LOOP_BOUND_CODE, with step of LOOP_BOUND_STEP.

   E.g.
     for (int i = 0; i < bound; i++) {
       if (i < bound - 2)
	 computation_1();
       else
	 computation_2();
     }

  In this loop, we will predict the branch inside the loop to be taken.  */

static void
predict_iv_comparison (struct loop *loop, basic_block bb,
		       tree loop_bound_var,
		       tree loop_iv_base_var,
		       enum tree_code loop_bound_code,
		       int loop_bound_step)
{
  gimple stmt;
  tree compare_var, compare_base;
  enum tree_code compare_code;
  int compare_step;
  edge then_edge;
  edge_iterator ei;

  if (predicted_by_p (bb, PRED_LOOP_ITERATIONS_GUESSED)
      || predicted_by_p (bb, PRED_LOOP_ITERATIONS)
      || predicted_by_p (bb, PRED_LOOP_EXIT))
    return;

  stmt = last_stmt (bb);
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return;
  if (!is_comparison_with_loop_invariant_p (stmt, loop, &compare_var,
					    &compare_code,
					    &compare_step,
					    &compare_base))
    return;

  /* Find the taken edge.  */
  FOR_EACH_EDGE (then_edge, ei, bb->succs)
    if (then_edge->flags & EDGE_TRUE_VALUE)
      break;

  /* When comparing an IV to a loop invariant, NE is more likely to be
     taken while EQ is more likely to be not-taken.  */
  if (compare_code == NE_EXPR)
    {
      predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, TAKEN);
      return;
    }
  else if (compare_code == EQ_EXPR)
    {
      predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, NOT_TAKEN);
      return;
    }

  if (!expr_coherent_p (loop_iv_base_var, compare_base))
    return;

  /* If loop bound, base and compare bound are all constants, we can
     calculate the probability directly.  */
  if (host_integerp (loop_bound_var, 0)
      && host_integerp (compare_var, 0)
      && host_integerp (compare_base, 0))
    {
      int probability;
      HOST_WIDE_INT compare_count;
      HOST_WIDE_INT loop_bound = tree_low_cst (loop_bound_var, 0);
      HOST_WIDE_INT compare_bound = tree_low_cst (compare_var, 0);
      HOST_WIDE_INT base = tree_low_cst (compare_base, 0);
      HOST_WIDE_INT loop_count = (loop_bound - base) / compare_step;

      if ((compare_step > 0)
          ^ (compare_code == LT_EXPR || compare_code == LE_EXPR))
	compare_count = (loop_bound - compare_bound) / compare_step;
      else
	compare_count = (compare_bound - base) / compare_step;

      if (compare_code == LE_EXPR || compare_code == GE_EXPR)
	compare_count ++;
      if (loop_bound_code == LE_EXPR || loop_bound_code == GE_EXPR)
	loop_count ++;
      if (compare_count < 0)
	compare_count = 0;
      if (loop_count < 0)
	loop_count = 0;

      if (loop_count == 0)
	probability = 0;
      else if (compare_count > loop_count)
	probability = REG_BR_PROB_BASE;
      else
	probability = (double) REG_BR_PROB_BASE * compare_count / loop_count;
      predict_edge (then_edge, PRED_LOOP_IV_COMPARE, probability);
      return;
    }

  if (expr_coherent_p (loop_bound_var, compare_var))
    {
      if ((loop_bound_code == LT_EXPR || loop_bound_code == LE_EXPR)
	  && (compare_code == LT_EXPR || compare_code == LE_EXPR))
	predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, TAKEN);
      else if ((loop_bound_code == GT_EXPR || loop_bound_code == GE_EXPR)
	       && (compare_code == GT_EXPR || compare_code == GE_EXPR))
	predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, TAKEN);
      else if (loop_bound_code == NE_EXPR)
	{
	  /* If the loop backedge condition is "(i != bound)", we do
	     the comparison based on the step of IV:
	     * step < 0 : backedge condition is like (i > bound)
	     * step > 0 : backedge condition is like (i < bound)  */
	  gcc_assert (loop_bound_step != 0);
	  if (loop_bound_step > 0
	      && (compare_code == LT_EXPR
		  || compare_code == LE_EXPR))
	    predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, TAKEN);
	  else if (loop_bound_step < 0
		   && (compare_code == GT_EXPR
		       || compare_code == GE_EXPR))
	    predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, TAKEN);
	  else
	    predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, NOT_TAKEN);
	}
      else
	/* The branch is predicted not-taken if loop_bound_code is
	   opposite with compare_code.  */
	predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, NOT_TAKEN);
    }
  else if (expr_coherent_p (loop_iv_base_var, compare_var))
    {
      /* For cases like:
	   for (i = s; i < h; i++)
	     if (i > s + 2) ....
	 The branch should be predicted taken.  */
      if (loop_bound_step > 0
	  && (compare_code == GT_EXPR || compare_code == GE_EXPR))
	predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, TAKEN);
      else if (loop_bound_step < 0
	       && (compare_code == LT_EXPR || compare_code == LE_EXPR))
	predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, TAKEN);
      else
	predict_edge_def (then_edge, PRED_LOOP_IV_COMPARE_GUESS, NOT_TAKEN);
    }
}
 
/* Predict edge probabilities by exploiting loop structure.  */

static void
predict_loops (void)
{
  loop_iterator li;
  struct loop *loop;

  /* Try to predict out blocks in a loop that are not part of a
     natural loop.  */
  FOR_EACH_LOOP (li, loop, 0)
    {
      basic_block bb, *bbs;
      unsigned j, n_exits;
      VEC (edge, heap) *exits;
      struct tree_niter_desc niter_desc;
      edge ex;
      struct nb_iter_bound *nb_iter;
      enum tree_code loop_bound_code = ERROR_MARK;
      int loop_bound_step = 0;
      tree loop_bound_var = NULL;
      tree loop_iv_base = NULL;
      gimple stmt = NULL;

      exits = get_loop_exit_edges (loop);
      n_exits = VEC_length (edge, exits);

      FOR_EACH_VEC_ELT (edge, exits, j, ex)
	{
	  tree niter = NULL;
	  HOST_WIDE_INT nitercst;
	  int max = PARAM_VALUE (PARAM_MAX_PREDICTED_ITERATIONS);
	  int probability;
	  enum br_predictor predictor;

	  if (number_of_iterations_exit (loop, ex, &niter_desc, false))
	    niter = niter_desc.niter;
	  if (!niter || TREE_CODE (niter_desc.niter) != INTEGER_CST)
	    niter = loop_niter_by_eval (loop, ex);

	  if (TREE_CODE (niter) == INTEGER_CST)
	    {
	      if (host_integerp (niter, 1)
		  && compare_tree_int (niter, max-1) == -1)
		nitercst = tree_low_cst (niter, 1) + 1;
	      else
		nitercst = max;
	      predictor = PRED_LOOP_ITERATIONS;
	    }
	  /* If we have just one exit and we can derive some information about
	     the number of iterations of the loop from the statements inside
	     the loop, use it to predict this exit.  */
	  else if (n_exits == 1)
	    {
	      nitercst = estimated_stmt_executions_int (loop);
	      if (nitercst < 0)
		continue;
	      if (nitercst > max)
		nitercst = max;

	      predictor = PRED_LOOP_ITERATIONS_GUESSED;
	    }
	  else
	    continue;

	  probability = ((REG_BR_PROB_BASE + nitercst / 2) / nitercst);
	  predict_edge (ex, predictor, probability);
	}
      VEC_free (edge, heap, exits);

      /* Find information about loop bound variables.  */
      for (nb_iter = loop->bounds; nb_iter;
	   nb_iter = nb_iter->next)
	if (nb_iter->stmt
	    && gimple_code (nb_iter->stmt) == GIMPLE_COND)
	  {
	    stmt = nb_iter->stmt;
	    break;
	  }
      if (!stmt && last_stmt (loop->header)
	  && gimple_code (last_stmt (loop->header)) == GIMPLE_COND)
	stmt = last_stmt (loop->header);
      if (stmt)
	is_comparison_with_loop_invariant_p (stmt, loop,
					     &loop_bound_var,
					     &loop_bound_code,
					     &loop_bound_step,
					     &loop_iv_base);

      bbs = get_loop_body (loop);

      for (j = 0; j < loop->num_nodes; j++)
	{
	  int header_found = 0;
	  edge e;
	  edge_iterator ei;

	  bb = bbs[j];

	  /* Bypass loop heuristics on continue statement.  These
	     statements construct loops via "non-loop" constructs
	     in the source language and are better to be handled
	     separately.  */
	  if (predicted_by_p (bb, PRED_CONTINUE))
	    continue;

	  /* Loop branch heuristics - predict an edge back to a
	     loop's head as taken.  */
	  if (bb == loop->latch)
	    {
	      e = find_edge (loop->latch, loop->header);
	      if (e)
		{
		  header_found = 1;
		  predict_edge_def (e, PRED_LOOP_BRANCH, TAKEN);
		}
	    }

	  /* Loop exit heuristics - predict an edge exiting the loop if the
	     conditional has no loop header successors as not taken.  */
	  if (!header_found
	      /* If we already used more reliable loop exit predictors, do not
		 bother with PRED_LOOP_EXIT.  */
	      && !predicted_by_p (bb, PRED_LOOP_ITERATIONS_GUESSED)
	      && !predicted_by_p (bb, PRED_LOOP_ITERATIONS))
	    {
	      /* For loop with many exits we don't want to predict all exits
	         with the pretty large probability, because if all exits are
		 considered in row, the loop would be predicted to iterate
		 almost never.  The code to divide probability by number of
		 exits is very rough.  It should compute the number of exits
		 taken in each patch through function (not the overall number
		 of exits that might be a lot higher for loops with wide switch
		 statements in them) and compute n-th square root.

		 We limit the minimal probability by 2% to avoid
		 EDGE_PROBABILITY_RELIABLE from trusting the branch prediction
		 as this was causing regression in perl benchmark containing such
		 a wide loop.  */

	      int probability = ((REG_BR_PROB_BASE
		                  - predictor_info [(int) PRED_LOOP_EXIT].hitrate)
				 / n_exits);
	      if (probability < HITRATE (2))
		probability = HITRATE (2);
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (e->dest->index < NUM_FIXED_BLOCKS
		    || !flow_bb_inside_loop_p (loop, e->dest))
		  predict_edge (e, PRED_LOOP_EXIT, probability);
	    }
	  if (loop_bound_var)
	    predict_iv_comparison (loop, bb, loop_bound_var, loop_iv_base,
				   loop_bound_code,
				   loop_bound_step);
	}

      /* Free basic blocks from get_loop_body.  */
      free (bbs);
    }
}

/* Attempt to predict probabilities of BB outgoing edges using local
   properties.  */
static void
bb_estimate_probability_locally (basic_block bb)
{
  rtx last_insn = BB_END (bb);
  rtx cond;

  if (! can_predict_insn_p (last_insn))
    return;
  cond = get_condition (last_insn, NULL, false, false);
  if (! cond)
    return;

  /* Try "pointer heuristic."
     A comparison ptr == 0 is predicted as false.
     Similarly, a comparison ptr1 == ptr2 is predicted as false.  */
  if (COMPARISON_P (cond)
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

/* Set edge->probability for each successor edge of BB.  */
void
guess_outgoing_edge_probabilities (basic_block bb)
{
  bb_estimate_probability_locally (bb);
  combine_predictions_for_insn (BB_END (bb), bb);
}

static tree expr_expected_value (tree, bitmap);

/* Helper function for expr_expected_value.  */

static tree
expr_expected_value_1 (tree type, tree op0, enum tree_code code,
		       tree op1, bitmap visited)
{
  gimple def;

  if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS)
    {
      if (TREE_CONSTANT (op0))
	return op0;

      if (code != SSA_NAME)
	return NULL_TREE;

      def = SSA_NAME_DEF_STMT (op0);

      /* If we were already here, break the infinite cycle.  */
      if (!bitmap_set_bit (visited, SSA_NAME_VERSION (op0)))
	return NULL;

      if (gimple_code (def) == GIMPLE_PHI)
	{
	  /* All the arguments of the PHI node must have the same constant
	     length.  */
	  int i, n = gimple_phi_num_args (def);
	  tree val = NULL, new_val;

	  for (i = 0; i < n; i++)
	    {
	      tree arg = PHI_ARG_DEF (def, i);

	      /* If this PHI has itself as an argument, we cannot
		 determine the string length of this argument.  However,
		 if we can find an expected constant value for the other
		 PHI args then we can still be sure that this is
		 likely a constant.  So be optimistic and just
		 continue with the next argument.  */
	      if (arg == PHI_RESULT (def))
		continue;

	      new_val = expr_expected_value (arg, visited);
	      if (!new_val)
		return NULL;
	      if (!val)
		val = new_val;
	      else if (!operand_equal_p (val, new_val, false))
		return NULL;
	    }
	  return val;
	}
      if (is_gimple_assign (def))
	{
	  if (gimple_assign_lhs (def) != op0)
	    return NULL;

	  return expr_expected_value_1 (TREE_TYPE (gimple_assign_lhs (def)),
					gimple_assign_rhs1 (def),
					gimple_assign_rhs_code (def),
					gimple_assign_rhs2 (def),
					visited);
	}

      if (is_gimple_call (def))
	{
	  tree decl = gimple_call_fndecl (def);
	  if (!decl)
	    return NULL;
	  if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	    switch (DECL_FUNCTION_CODE (decl))
	      {
	      case BUILT_IN_EXPECT:
		{
		  tree val;
		  if (gimple_call_num_args (def) != 2)
		    return NULL;
		  val = gimple_call_arg (def, 0);
		  if (TREE_CONSTANT (val))
		    return val;
		  return gimple_call_arg (def, 1);
		}

	      case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_N:
	      case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1:
	      case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_2:
	      case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_4:
	      case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_8:
	      case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_16:
	      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE:
	      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_N:
	      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1:
	      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_2:
	      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4:
	      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8:
	      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_16:
		/* Assume that any given atomic operation has low contention,
		   and thus the compare-and-swap operation succeeds.  */
		return boolean_true_node;
	    }
	}

      return NULL;
    }

  if (get_gimple_rhs_class (code) == GIMPLE_BINARY_RHS)
    {
      tree res;
      op0 = expr_expected_value (op0, visited);
      if (!op0)
	return NULL;
      op1 = expr_expected_value (op1, visited);
      if (!op1)
	return NULL;
      res = fold_build2 (code, type, op0, op1);
      if (TREE_CONSTANT (res))
	return res;
      return NULL;
    }
  if (get_gimple_rhs_class (code) == GIMPLE_UNARY_RHS)
    {
      tree res;
      op0 = expr_expected_value (op0, visited);
      if (!op0)
	return NULL;
      res = fold_build1 (code, type, op0);
      if (TREE_CONSTANT (res))
	return res;
      return NULL;
    }
  return NULL;
}

/* Return constant EXPR will likely have at execution time, NULL if unknown.
   The function is used by builtin_expect branch predictor so the evidence
   must come from this construct and additional possible constant folding.

   We may want to implement more involved value guess (such as value range
   propagation based prediction), but such tricks shall go to new
   implementation.  */

static tree
expr_expected_value (tree expr, bitmap visited)
{
  enum tree_code code;
  tree op0, op1;

  if (TREE_CONSTANT (expr))
    return expr;

  extract_ops_from_tree (expr, &code, &op0, &op1);
  return expr_expected_value_1 (TREE_TYPE (expr),
				op0, code, op1, visited);
}


/* Get rid of all builtin_expect calls and GIMPLE_PREDICT statements
   we no longer need.  */
static unsigned int
strip_predict_hints (void)
{
  basic_block bb;
  gimple ass_stmt;
  tree var;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator bi;
      for (bi = gsi_start_bb (bb); !gsi_end_p (bi);)
	{
	  gimple stmt = gsi_stmt (bi);

	  if (gimple_code (stmt) == GIMPLE_PREDICT)
	    {
	      gsi_remove (&bi, true);
	      continue;
	    }
	  else if (gimple_code (stmt) == GIMPLE_CALL)
	    {
	      tree fndecl = gimple_call_fndecl (stmt);

	      if (fndecl
		  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
		  && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_EXPECT
		  && gimple_call_num_args (stmt) == 2)
		{
		  var = gimple_call_lhs (stmt);
		  if (var)
		    {
		      ass_stmt
			= gimple_build_assign (var, gimple_call_arg (stmt, 0));
		      gsi_replace (&bi, ass_stmt, true);
		    }
		  else
		    {
		      gsi_remove (&bi, true);
		      continue;
		    }
		}
	    }
	  gsi_next (&bi);
	}
    }
  return 0;
}

/* Predict using opcode of the last statement in basic block.  */
static void
tree_predict_by_opcode (basic_block bb)
{
  gimple stmt = last_stmt (bb);
  edge then_edge;
  tree op0, op1;
  tree type;
  tree val;
  enum tree_code cmp;
  bitmap visited;
  edge_iterator ei;

  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return;
  FOR_EACH_EDGE (then_edge, ei, bb->succs)
    if (then_edge->flags & EDGE_TRUE_VALUE)
      break;
  op0 = gimple_cond_lhs (stmt);
  op1 = gimple_cond_rhs (stmt);
  cmp = gimple_cond_code (stmt);
  type = TREE_TYPE (op0);
  visited = BITMAP_ALLOC (NULL);
  val = expr_expected_value_1 (boolean_type_node, op0, cmp, op1, visited);
  BITMAP_FREE (visited);
  if (val)
    {
      if (integer_zerop (val))
	predict_edge_def (then_edge, PRED_BUILTIN_EXPECT, NOT_TAKEN);
      else
	predict_edge_def (then_edge, PRED_BUILTIN_EXPECT, TAKEN);
      return;
    }
  /* Try "pointer heuristic."
     A comparison ptr == 0 is predicted as false.
     Similarly, a comparison ptr1 == ptr2 is predicted as false.  */
  if (POINTER_TYPE_P (type))
    {
      if (cmp == EQ_EXPR)
	predict_edge_def (then_edge, PRED_TREE_POINTER, NOT_TAKEN);
      else if (cmp == NE_EXPR)
	predict_edge_def (then_edge, PRED_TREE_POINTER, TAKEN);
    }
  else

  /* Try "opcode heuristic."
     EQ tests are usually false and NE tests are usually true. Also,
     most quantities are positive, so we can make the appropriate guesses
     about signed comparisons against zero.  */
    switch (cmp)
      {
      case EQ_EXPR:
      case UNEQ_EXPR:
	/* Floating point comparisons appears to behave in a very
	   unpredictable way because of special role of = tests in
	   FP code.  */
	if (FLOAT_TYPE_P (type))
	  ;
	/* Comparisons with 0 are often used for booleans and there is
	   nothing useful to predict about them.  */
	else if (integer_zerop (op0) || integer_zerop (op1))
	  ;
	else
	  predict_edge_def (then_edge, PRED_TREE_OPCODE_NONEQUAL, NOT_TAKEN);
	break;

      case NE_EXPR:
      case LTGT_EXPR:
	/* Floating point comparisons appears to behave in a very
	   unpredictable way because of special role of = tests in
	   FP code.  */
	if (FLOAT_TYPE_P (type))
	  ;
	/* Comparisons with 0 are often used for booleans and there is
	   nothing useful to predict about them.  */
	else if (integer_zerop (op0)
		 || integer_zerop (op1))
	  ;
	else
	  predict_edge_def (then_edge, PRED_TREE_OPCODE_NONEQUAL, TAKEN);
	break;

      case ORDERED_EXPR:
	predict_edge_def (then_edge, PRED_TREE_FPOPCODE, TAKEN);
	break;

      case UNORDERED_EXPR:
	predict_edge_def (then_edge, PRED_TREE_FPOPCODE, NOT_TAKEN);
	break;

      case LE_EXPR:
      case LT_EXPR:
	if (integer_zerop (op1)
	    || integer_onep (op1)
	    || integer_all_onesp (op1)
	    || real_zerop (op1)
	    || real_onep (op1)
	    || real_minus_onep (op1))
	  predict_edge_def (then_edge, PRED_TREE_OPCODE_POSITIVE, NOT_TAKEN);
	break;

      case GE_EXPR:
      case GT_EXPR:
	if (integer_zerop (op1)
	    || integer_onep (op1)
	    || integer_all_onesp (op1)
	    || real_zerop (op1)
	    || real_onep (op1)
	    || real_minus_onep (op1))
	  predict_edge_def (then_edge, PRED_TREE_OPCODE_POSITIVE, TAKEN);
	break;

      default:
	break;
      }
}

/* Try to guess whether the value of return means error code.  */

static enum br_predictor
return_prediction (tree val, enum prediction *prediction)
{
  /* VOID.  */
  if (!val)
    return PRED_NO_PREDICTION;
  /* Different heuristics for pointers and scalars.  */
  if (POINTER_TYPE_P (TREE_TYPE (val)))
    {
      /* NULL is usually not returned.  */
      if (integer_zerop (val))
	{
	  *prediction = NOT_TAKEN;
	  return PRED_NULL_RETURN;
	}
    }
  else if (INTEGRAL_TYPE_P (TREE_TYPE (val)))
    {
      /* Negative return values are often used to indicate
         errors.  */
      if (TREE_CODE (val) == INTEGER_CST
	  && tree_int_cst_sgn (val) < 0)
	{
	  *prediction = NOT_TAKEN;
	  return PRED_NEGATIVE_RETURN;
	}
      /* Constant return values seems to be commonly taken.
         Zero/one often represent booleans so exclude them from the
	 heuristics.  */
      if (TREE_CONSTANT (val)
	  && (!integer_zerop (val) && !integer_onep (val)))
	{
	  *prediction = TAKEN;
	  return PRED_CONST_RETURN;
	}
    }
  return PRED_NO_PREDICTION;
}

/* Find the basic block with return expression and look up for possible
   return value trying to apply RETURN_PREDICTION heuristics.  */
static void
apply_return_prediction (void)
{
  gimple return_stmt = NULL;
  tree return_val;
  edge e;
  gimple phi;
  int phi_num_args, i;
  enum br_predictor pred;
  enum prediction direction;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    {
      return_stmt = last_stmt (e->src);
      if (return_stmt
	  && gimple_code (return_stmt) == GIMPLE_RETURN)
	break;
    }
  if (!e)
    return;
  return_val = gimple_return_retval (return_stmt);
  if (!return_val)
    return;
  if (TREE_CODE (return_val) != SSA_NAME
      || !SSA_NAME_DEF_STMT (return_val)
      || gimple_code (SSA_NAME_DEF_STMT (return_val)) != GIMPLE_PHI)
    return;
  phi = SSA_NAME_DEF_STMT (return_val);
  phi_num_args = gimple_phi_num_args (phi);
  pred = return_prediction (PHI_ARG_DEF (phi, 0), &direction);

  /* Avoid the degenerate case where all return values form the function
     belongs to same category (ie they are all positive constants)
     so we can hardly say something about them.  */
  for (i = 1; i < phi_num_args; i++)
    if (pred != return_prediction (PHI_ARG_DEF (phi, i), &direction))
      break;
  if (i != phi_num_args)
    for (i = 0; i < phi_num_args; i++)
      {
	pred = return_prediction (PHI_ARG_DEF (phi, i), &direction);
	if (pred != PRED_NO_PREDICTION)
	  predict_paths_leading_to_edge (gimple_phi_arg_edge (phi, i), pred,
				         direction);
      }
}

/* Look for basic block that contains unlikely to happen events
   (such as noreturn calls) and mark all paths leading to execution
   of this basic blocks as unlikely.  */

static void
tree_bb_level_predictions (void)
{
  basic_block bb;
  bool has_return_edges = false;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    if (!(e->flags & (EDGE_ABNORMAL | EDGE_FAKE | EDGE_EH)))
      {
        has_return_edges = true;
	break;
      }

  apply_return_prediction ();

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree decl;

	  if (is_gimple_call (stmt))
	    {
	      if ((gimple_call_flags (stmt) & ECF_NORETURN)
	          && has_return_edges)
		predict_paths_leading_to (bb, PRED_NORETURN,
					  NOT_TAKEN);
	      decl = gimple_call_fndecl (stmt);
	      if (decl
		  && lookup_attribute ("cold",
				       DECL_ATTRIBUTES (decl)))
		predict_paths_leading_to (bb, PRED_COLD_FUNCTION,
					  NOT_TAKEN);
	    }
	  else if (gimple_code (stmt) == GIMPLE_PREDICT)
	    {
	      predict_paths_leading_to (bb, gimple_predict_predictor (stmt),
					gimple_predict_outcome (stmt));
	      /* Keep GIMPLE_PREDICT around so early inlining will propagate
	         hints to callers.  */
	    }
	}
    }
}

#ifdef ENABLE_CHECKING

/* Callback for pointer_map_traverse, asserts that the pointer map is
   empty.  */

static bool
assert_is_empty (const void *key ATTRIBUTE_UNUSED, void **value,
		 void *data ATTRIBUTE_UNUSED)
{
  gcc_assert (!*value);
  return false;
}
#endif

/* Predict branch probabilities and estimate profile for basic block BB.  */

static void
tree_estimate_probability_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;
  gimple last;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      /* Predict edges to user labels with attributes.  */
      if (e->dest != EXIT_BLOCK_PTR)
	{
	  gimple_stmt_iterator gi;
	  for (gi = gsi_start_bb (e->dest); !gsi_end_p (gi); gsi_next (&gi))
	    {
	      gimple stmt = gsi_stmt (gi);
	      tree decl;

	      if (gimple_code (stmt) != GIMPLE_LABEL)
		break;
	      decl = gimple_label_label (stmt);
	      if (DECL_ARTIFICIAL (decl))
		continue;

	      /* Finally, we have a user-defined label.  */
	      if (lookup_attribute ("cold", DECL_ATTRIBUTES (decl)))
		predict_edge_def (e, PRED_COLD_LABEL, NOT_TAKEN);
	      else if (lookup_attribute ("hot", DECL_ATTRIBUTES (decl)))
		predict_edge_def (e, PRED_HOT_LABEL, TAKEN);
	    }
	}

      /* Predict early returns to be probable, as we've already taken
	 care for error returns and other cases are often used for
	 fast paths through function.

	 Since we've already removed the return statements, we are
	 looking for CFG like:

	 if (conditional)
	 {
	 ..
	 goto return_block
	 }
	 some other blocks
	 return_block:
	 return_stmt.  */
      if (e->dest != bb->next_bb
	  && e->dest != EXIT_BLOCK_PTR
	  && single_succ_p (e->dest)
	  && single_succ_edge (e->dest)->dest == EXIT_BLOCK_PTR
	  && (last = last_stmt (e->dest)) != NULL
	  && gimple_code (last) == GIMPLE_RETURN)
	{
	  edge e1;
	  edge_iterator ei1;

	  if (single_succ_p (bb))
	    {
	      FOR_EACH_EDGE (e1, ei1, bb->preds)
		if (!predicted_by_p (e1->src, PRED_NULL_RETURN)
		    && !predicted_by_p (e1->src, PRED_CONST_RETURN)
		    && !predicted_by_p (e1->src, PRED_NEGATIVE_RETURN))
		  predict_edge_def (e1, PRED_TREE_EARLY_RETURN, NOT_TAKEN);
	    }
	  else
	    if (!predicted_by_p (e->src, PRED_NULL_RETURN)
		&& !predicted_by_p (e->src, PRED_CONST_RETURN)
		&& !predicted_by_p (e->src, PRED_NEGATIVE_RETURN))
	      predict_edge_def (e, PRED_TREE_EARLY_RETURN, NOT_TAKEN);
	}

      /* Look for block we are guarding (ie we dominate it,
	 but it doesn't postdominate us).  */
      if (e->dest != EXIT_BLOCK_PTR && e->dest != bb
	  && dominated_by_p (CDI_DOMINATORS, e->dest, e->src)
	  && !dominated_by_p (CDI_POST_DOMINATORS, e->src, e->dest))
	{
	  gimple_stmt_iterator bi;

	  /* The call heuristic claims that a guarded function call
	     is improbable.  This is because such calls are often used
	     to signal exceptional situations such as printing error
	     messages.  */
	  for (bi = gsi_start_bb (e->dest); !gsi_end_p (bi);
	       gsi_next (&bi))
	    {
	      gimple stmt = gsi_stmt (bi);
	      if (is_gimple_call (stmt)
		  /* Constant and pure calls are hardly used to signalize
		     something exceptional.  */
		  && gimple_has_side_effects (stmt))
		{
		  predict_edge_def (e, PRED_CALL, NOT_TAKEN);
		  break;
		}
	    }
	}
    }
  tree_predict_by_opcode (bb);
}

/* Predict branch probabilities and estimate profile of the tree CFG.
   This function can be called from the loop optimizers to recompute
   the profile information.  */

void
tree_estimate_probability (void)
{
  basic_block bb;

  add_noreturn_fake_exit_edges ();
  connect_infinite_loops_to_exit ();
  /* We use loop_niter_by_eval, which requires that the loops have
     preheaders.  */
  create_preheaders (CP_SIMPLE_PREHEADERS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  bb_predictions = pointer_map_create ();
  tree_bb_level_predictions ();
  record_loop_exits ();

  if (number_of_loops () > 1)
    predict_loops ();

  FOR_EACH_BB (bb)
    tree_estimate_probability_bb (bb);

  FOR_EACH_BB (bb)
    combine_predictions_for_bb (bb);

#ifdef ENABLE_CHECKING
  pointer_map_traverse (bb_predictions, assert_is_empty, NULL);
#endif
  pointer_map_destroy (bb_predictions);
  bb_predictions = NULL;

  estimate_bb_frequencies ();
  free_dominance_info (CDI_POST_DOMINATORS);
  remove_fake_exit_edges ();
}

/* Predict branch probabilities and estimate profile of the tree CFG.
   This is the driver function for PASS_PROFILE.  */

static unsigned int
tree_estimate_probability_driver (void)
{
  unsigned nb_loops;

  loop_optimizer_init (0);
  if (dump_file && (dump_flags & TDF_DETAILS))
    flow_loops_dump (dump_file, NULL, 0);

  mark_irreducible_loops ();

  nb_loops = number_of_loops ();
  if (nb_loops > 1)
    scev_initialize ();

  tree_estimate_probability ();

  if (nb_loops > 1)
    scev_finalize ();

  loop_optimizer_finalize ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    gimple_dump_cfg (dump_file, dump_flags);
  if (profile_status == PROFILE_ABSENT)
    profile_status = PROFILE_GUESSED;
  return 0;
}

/* Predict edges to successors of CUR whose sources are not postdominated by
   BB by PRED and recurse to all postdominators.  */

static void
predict_paths_for_bb (basic_block cur, basic_block bb,
		      enum br_predictor pred,
		      enum prediction taken,
		      bitmap visited)
{
  edge e;
  edge_iterator ei;
  basic_block son;

  /* We are looking for all edges forming edge cut induced by
     set of all blocks postdominated by BB.  */
  FOR_EACH_EDGE (e, ei, cur->preds)
    if (e->src->index >= NUM_FIXED_BLOCKS
	&& !dominated_by_p (CDI_POST_DOMINATORS, e->src, bb))
    {
      edge e2;
      edge_iterator ei2;
      bool found = false;

      /* Ignore fake edges and eh, we predict them as not taken anyway.  */
      if (e->flags & (EDGE_EH | EDGE_FAKE))
	continue;
      gcc_assert (bb == cur || dominated_by_p (CDI_POST_DOMINATORS, cur, bb));

      /* See if there is an edge from e->src that is not abnormal
	 and does not lead to BB.  */
      FOR_EACH_EDGE (e2, ei2, e->src->succs)
	if (e2 != e
	    && !(e2->flags & (EDGE_EH | EDGE_FAKE))
	    && !dominated_by_p (CDI_POST_DOMINATORS, e2->dest, bb))
	  {
	    found = true;
	    break;
	  }

      /* If there is non-abnormal path leaving e->src, predict edge
	 using predictor.  Otherwise we need to look for paths
	 leading to e->src.

	 The second may lead to infinite loop in the case we are predicitng
	 regions that are only reachable by abnormal edges.  We simply
	 prevent visiting given BB twice.  */
      if (found)
        predict_edge_def (e, pred, taken);
      else if (bitmap_set_bit (visited, e->src->index))
	predict_paths_for_bb (e->src, e->src, pred, taken, visited);
    }
  for (son = first_dom_son (CDI_POST_DOMINATORS, cur);
       son;
       son = next_dom_son (CDI_POST_DOMINATORS, son))
    predict_paths_for_bb (son, bb, pred, taken, visited);
}

/* Sets branch probabilities according to PREDiction and
   FLAGS.  */

static void
predict_paths_leading_to (basic_block bb, enum br_predictor pred,
			  enum prediction taken)
{
  bitmap visited = BITMAP_ALLOC (NULL);
  predict_paths_for_bb (bb, bb, pred, taken, visited);
  BITMAP_FREE (visited);
}

/* Like predict_paths_leading_to but take edge instead of basic block.  */

static void
predict_paths_leading_to_edge (edge e, enum br_predictor pred,
			       enum prediction taken)
{
  bool has_nonloop_edge = false;
  edge_iterator ei;
  edge e2;

  basic_block bb = e->src;
  FOR_EACH_EDGE (e2, ei, bb->succs)
    if (e2->dest != e->src && e2->dest != e->dest
	&& !(e->flags & (EDGE_EH | EDGE_FAKE))
	&& !dominated_by_p (CDI_POST_DOMINATORS, e->src, e2->dest))
      {
	has_nonloop_edge = true;
	break;
      }
  if (!has_nonloop_edge)
    {
      bitmap visited = BITMAP_ALLOC (NULL);
      predict_paths_for_bb (bb, bb, pred, taken, visited);
      BITMAP_FREE (visited);
    }
  else
    predict_edge_def (e, pred, taken);
}

/* This is used to carry information about basic blocks.  It is
   attached to the AUX field of the standard CFG block.  */

typedef struct block_info_def
{
  /* Estimated frequency of execution of basic_block.  */
  sreal frequency;

  /* To keep queue of basic blocks to process.  */
  basic_block next;

  /* Number of predecessors we need to visit first.  */
  int npredecessors;
} *block_info;

/* Similar information for edges.  */
typedef struct edge_info_def
{
  /* In case edge is a loopback edge, the probability edge will be reached
     in case header is.  Estimated number of iterations of the loop can be
     then computed as 1 / (1 - back_edge_prob).  */
  sreal back_edge_prob;
  /* True if the edge is a loopback edge in the natural loop.  */
  unsigned int back_edge:1;
} *edge_info;

#define BLOCK_INFO(B)	((block_info) (B)->aux)
#define EDGE_INFO(E)	((edge_info) (E)->aux)

/* Helper function for estimate_bb_frequencies.
   Propagate the frequencies in blocks marked in
   TOVISIT, starting in HEAD.  */

static void
propagate_freq (basic_block head, bitmap tovisit)
{
  basic_block bb;
  basic_block last;
  unsigned i;
  edge e;
  basic_block nextbb;
  bitmap_iterator bi;

  /* For each basic block we need to visit count number of his predecessors
     we need to visit first.  */
  EXECUTE_IF_SET_IN_BITMAP (tovisit, 0, i, bi)
    {
      edge_iterator ei;
      int count = 0;

      bb = BASIC_BLOCK (i);

      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  bool visit = bitmap_bit_p (tovisit, e->src->index);

	  if (visit && !(e->flags & EDGE_DFS_BACK))
	    count++;
	  else if (visit && dump_file && !EDGE_INFO (e)->back_edge)
	    fprintf (dump_file,
		     "Irreducible region hit, ignoring edge to %i->%i\n",
		     e->src->index, bb->index);
	}
      BLOCK_INFO (bb)->npredecessors = count;
      /* When function never returns, we will never process exit block.  */
      if (!count && bb == EXIT_BLOCK_PTR)
	bb->count = bb->frequency = 0;
    }

  memcpy (&BLOCK_INFO (head)->frequency, &real_one, sizeof (real_one));
  last = head;
  for (bb = head; bb; bb = nextbb)
    {
      edge_iterator ei;
      sreal cyclic_probability, frequency;

      memcpy (&cyclic_probability, &real_zero, sizeof (real_zero));
      memcpy (&frequency, &real_zero, sizeof (real_zero));

      nextbb = BLOCK_INFO (bb)->next;
      BLOCK_INFO (bb)->next = NULL;

      /* Compute frequency of basic block.  */
      if (bb != head)
	{
#ifdef ENABLE_CHECKING
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    gcc_assert (!bitmap_bit_p (tovisit, e->src->index)
			|| (e->flags & EDGE_DFS_BACK));
#endif

	  FOR_EACH_EDGE (e, ei, bb->preds)
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

      bitmap_clear_bit (tovisit, bb->index);

      e = find_edge (bb, head);
      if (e)
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
      FOR_EACH_EDGE (e, ei, bb->succs)
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
      bitmap tovisit = BITMAP_ALLOC (NULL);

      estimate_loops_at_level (loop->inner);

      /* Find current loop back edge and mark it.  */
      e = loop_latch_edge (loop);
      EDGE_INFO (e)->back_edge = 1;

      bbs = get_loop_body (loop);
      for (i = 0; i < loop->num_nodes; i++)
	bitmap_set_bit (tovisit, bbs[i]->index);
      free (bbs);
      propagate_freq (loop->header, tovisit);
      BITMAP_FREE (tovisit);
    }
}

/* Propagates frequencies through structure of loops.  */

static void
estimate_loops (void)
{
  bitmap tovisit = BITMAP_ALLOC (NULL);
  basic_block bb;

  /* Start by estimating the frequencies in the loops.  */
  if (number_of_loops () > 1)
    estimate_loops_at_level (current_loops->tree_root->inner);

  /* Now propagate the frequencies through all the blocks.  */
  FOR_ALL_BB (bb)
    {
      bitmap_set_bit (tovisit, bb->index);
    }
  propagate_freq (ENTRY_BLOCK_PTR, tovisit);
  BITMAP_FREE (tovisit);
}

/* Convert counts measured by profile driven feedback to frequencies.
   Return nonzero iff there was any nonzero execution count.  */

int
counts_to_freqs (void)
{
  gcov_type count_max, true_count_max = 0;
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    true_count_max = MAX (bb->count, true_count_max);

  count_max = MAX (true_count_max, 1);
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->frequency = (bb->count * BB_FREQ_MAX + count_max / 2) / count_max;

  return true_count_max;
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
  gcc_assert (threshold <= BB_FREQ_MAX);

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

void
estimate_bb_frequencies (void)
{
  basic_block bb;
  sreal freq_max;

  if (profile_status != PROFILE_READ || !counts_to_freqs ())
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

      single_succ_edge (ENTRY_BLOCK_PTR)->probability = REG_BR_PROB_BASE;

      /* Set up block info for each basic block.  */
      alloc_aux_for_blocks (sizeof (struct block_info_def));
      alloc_aux_for_edges (sizeof (struct edge_info_def));
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
	{
	  edge e;
	  edge_iterator ei;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      sreal_init (&EDGE_INFO (e)->back_edge_prob, e->probability, 0);
	      sreal_mul (&EDGE_INFO (e)->back_edge_prob,
			 &EDGE_INFO (e)->back_edge_prob,
			 &real_inv_br_prob_base);
	    }
	}

      /* First compute probabilities locally for each loop from innermost
         to outermost to examine probabilities for back edges.  */
      estimate_loops ();

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
}

/* Decide whether function is hot, cold or unlikely executed.  */
void
compute_function_frequency (void)
{
  basic_block bb;
  struct cgraph_node *node = cgraph_get_node (current_function_decl);
  if (DECL_STATIC_CONSTRUCTOR (current_function_decl)
      || MAIN_NAME_P (DECL_NAME (current_function_decl)))
    node->only_called_at_startup = true;
  if (DECL_STATIC_DESTRUCTOR (current_function_decl))
    node->only_called_at_exit = true;

  if (!profile_info || !flag_branch_probabilities)
    {
      int flags = flags_from_decl_or_type (current_function_decl);
      if (lookup_attribute ("cold", DECL_ATTRIBUTES (current_function_decl))
	  != NULL)
        node->frequency = NODE_FREQUENCY_UNLIKELY_EXECUTED;
      else if (lookup_attribute ("hot", DECL_ATTRIBUTES (current_function_decl))
	       != NULL)
        node->frequency = NODE_FREQUENCY_HOT;
      else if (flags & ECF_NORETURN)
        node->frequency = NODE_FREQUENCY_EXECUTED_ONCE;
      else if (MAIN_NAME_P (DECL_NAME (current_function_decl)))
        node->frequency = NODE_FREQUENCY_EXECUTED_ONCE;
      else if (DECL_STATIC_CONSTRUCTOR (current_function_decl)
	       || DECL_STATIC_DESTRUCTOR (current_function_decl))
        node->frequency = NODE_FREQUENCY_EXECUTED_ONCE;
      return;
    }
  node->frequency = NODE_FREQUENCY_UNLIKELY_EXECUTED;
  FOR_EACH_BB (bb)
    {
      if (maybe_hot_bb_p (bb))
	{
	  node->frequency = NODE_FREQUENCY_HOT;
	  return;
	}
      if (!probably_never_executed_bb_p (bb))
	node->frequency = NODE_FREQUENCY_NORMAL;
    }
}

static bool
gate_estimate_probability (void)
{
  return flag_guess_branch_prob;
}

/* Build PREDICT_EXPR.  */
tree
build_predict_expr (enum br_predictor predictor, enum prediction taken)
{
  tree t = build1 (PREDICT_EXPR, void_type_node,
		   build_int_cst (integer_type_node, predictor));
  SET_PREDICT_EXPR_OUTCOME (t, taken);
  return t;
}

const char *
predictor_name (enum br_predictor predictor)
{
  return predictor_info[predictor].name;
}

struct gimple_opt_pass pass_profile =
{
 {
  GIMPLE_PASS,
  "profile_estimate",			/* name */
  gate_estimate_probability,		/* gate */
  tree_estimate_probability_driver,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_BRANCH_PROB,			/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_ggc_collect | TODO_verify_ssa			/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_strip_predict_hints =
{
 {
  GIMPLE_PASS,
  "*strip_predict_hints",		/* name */
  NULL,					/* gate */
  strip_predict_hints,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_BRANCH_PROB,			/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_ggc_collect | TODO_verify_ssa			/* todo_flags_finish */
 }
};

/* Rebuild function frequencies.  Passes are in general expected to
   maintain profile by hand, however in some cases this is not possible:
   for example when inlining several functions with loops freuqencies might run
   out of scale and thus needs to be recomputed.  */

void
rebuild_frequencies (void)
{
  timevar_push (TV_REBUILD_FREQUENCIES);
  if (profile_status == PROFILE_GUESSED)
    {
      loop_optimizer_init (0);
      add_noreturn_fake_exit_edges ();
      mark_irreducible_loops ();
      connect_infinite_loops_to_exit ();
      estimate_bb_frequencies ();
      remove_fake_exit_edges ();
      loop_optimizer_finalize ();
    }
  else if (profile_status == PROFILE_READ)
    counts_to_freqs ();
  else
    gcc_unreachable ();
  timevar_pop (TV_REBUILD_FREQUENCIES);
}
