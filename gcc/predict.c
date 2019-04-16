/* Branch prediction routines for the GNU compiler.
   Copyright (C) 2000-2019 Free Software Foundation, Inc.

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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "coverage.h"
#include "diagnostic-core.h"
#include "gimple-predict.h"
#include "fold-const.h"
#include "calls.h"
#include "cfganal.h"
#include "profile.h"
#include "sreal.h"
#include "params.h"
#include "cfgloop.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
#include "ipa-utils.h"
#include "gimple-pretty-print.h"
#include "selftest.h"
#include "cfgrtl.h"
#include "stringpool.h"
#include "attribs.h"

/* Enum with reasons why a predictor is ignored.  */

enum predictor_reason
{
  REASON_NONE,
  REASON_IGNORED,
  REASON_SINGLE_EDGE_DUPLICATE,
  REASON_EDGE_PAIR_DUPLICATE
};

/* String messages for the aforementioned enum.  */

static const char *reason_messages[] = {"", " (ignored)",
    " (single edge duplicate)", " (edge pair duplicate)"};

/* real constants: 0, 1, 1-1/REG_BR_PROB_BASE, REG_BR_PROB_BASE,
		   1/REG_BR_PROB_BASE, 0.5, BB_FREQ_MAX.  */
static sreal real_almost_one, real_br_prob_base,
	     real_inv_br_prob_base, real_one_half, real_bb_freq_max;

static void combine_predictions_for_insn (rtx_insn *, basic_block);
static void dump_prediction (FILE *, enum br_predictor, int, basic_block,
			     enum predictor_reason, edge);
static void predict_paths_leading_to (basic_block, enum br_predictor,
				      enum prediction,
				      struct loop *in_loop = NULL);
static void predict_paths_leading_to_edge (edge, enum br_predictor,
					   enum prediction,
					   struct loop *in_loop = NULL);
static bool can_predict_insn_p (const rtx_insn *);
static HOST_WIDE_INT get_predictor_value (br_predictor, HOST_WIDE_INT);
static void determine_unlikely_bbs ();

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

static gcov_type min_count = -1;

/* Determine the threshold for hot BB counts.  */

gcov_type
get_hot_bb_threshold ()
{
  if (min_count == -1)
    {
      min_count
	= profile_info->sum_max / PARAM_VALUE (HOT_BB_COUNT_FRACTION);
      if (dump_file)
	fprintf (dump_file, "Setting hotness threshold to %" PRId64 ".\n",
		 min_count);
    }
  return min_count;
}

/* Set the threshold for hot BB counts.  */

void
set_hot_bb_threshold (gcov_type min)
{
  min_count = min;
}

/* Return TRUE if frequency FREQ is considered to be hot.  */

bool
maybe_hot_count_p (struct function *fun, profile_count count)
{
  if (!count.initialized_p ())
    return true;
  if (count.ipa () == profile_count::zero ())
    return false;
  if (!count.ipa_p ())
    {
      struct cgraph_node *node = cgraph_node::get (fun->decl);
      if (!profile_info || profile_status_for_fn (fun) != PROFILE_READ)
	{
	  if (node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
	    return false;
	  if (node->frequency == NODE_FREQUENCY_HOT)
	    return true;
	}
      if (profile_status_for_fn (fun) == PROFILE_ABSENT)
	return true;
      if (node->frequency == NODE_FREQUENCY_EXECUTED_ONCE
	  && count < (ENTRY_BLOCK_PTR_FOR_FN (fun)->count.apply_scale (2, 3)))
	return false;
      if (PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION) == 0)
	return false;
      if (count.apply_scale (PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION), 1)
	  < ENTRY_BLOCK_PTR_FOR_FN (fun)->count)
	return false;
      return true;
    }
  /* Code executed at most once is not hot.  */
  if (count <= MAX (profile_info ? profile_info->runs : 1, 1))
    return false;
  return (count.to_gcov_type () >= get_hot_bb_threshold ());
}

/* Return true in case BB can be CPU intensive and should be optimized
   for maximal performance.  */

bool
maybe_hot_bb_p (struct function *fun, const_basic_block bb)
{
  gcc_checking_assert (fun);
  return maybe_hot_count_p (fun, bb->count);
}

/* Return true in case BB can be CPU intensive and should be optimized
   for maximal performance.  */

bool
maybe_hot_edge_p (edge e)
{
  return maybe_hot_count_p (cfun, e->count ());
}

/* Return true if profile COUNT and FREQUENCY, or function FUN static
   node frequency reflects never being executed.  */
   
static bool
probably_never_executed (struct function *fun,
                         profile_count count)
{
  gcc_checking_assert (fun);
  if (count.ipa () == profile_count::zero ())
    return true;
  /* Do not trust adjusted counts.  This will make us to drop int cold section
     code with low execution count as a result of inlining. These low counts
     are not safe even with read profile and may lead us to dropping
     code which actually gets executed into cold section of binary that is not
     desirable.  */
  if (count.precise_p () && profile_status_for_fn (fun) == PROFILE_READ)
    {
      int unlikely_count_fraction = PARAM_VALUE (UNLIKELY_BB_COUNT_FRACTION);
      if (count.apply_scale (unlikely_count_fraction, 1) >= profile_info->runs)
	return false;
      return true;
    }
  if ((!profile_info || profile_status_for_fn (fun) != PROFILE_READ)
      && (cgraph_node::get (fun->decl)->frequency
	  == NODE_FREQUENCY_UNLIKELY_EXECUTED))
    return true;
  return false;
}


/* Return true in case BB is probably never executed.  */

bool
probably_never_executed_bb_p (struct function *fun, const_basic_block bb)
{
  return probably_never_executed (fun, bb->count);
}


/* Return true if E is unlikely executed for obvious reasons.  */

static bool
unlikely_executed_edge_p (edge e)
{
  return (e->count () == profile_count::zero ()
	  || e->probability == profile_probability::never ())
	 || (e->flags & (EDGE_EH | EDGE_FAKE));
}

/* Return true in case edge E is probably never executed.  */

bool
probably_never_executed_edge_p (struct function *fun, edge e)
{
  if (unlikely_executed_edge_p (e))
    return true;
  return probably_never_executed (fun, e->count ());
}

/* Return true when current function should always be optimized for size.  */

bool
optimize_function_for_size_p (struct function *fun)
{
  if (!fun || !fun->decl)
    return optimize_size;
  cgraph_node *n = cgraph_node::get (fun->decl);
  return n && n->optimize_for_size_p ();
}

/* Return true when current function should always be optimized for speed.  */

bool
optimize_function_for_speed_p (struct function *fun)
{
  return !optimize_function_for_size_p (fun);
}

/* Return the optimization type that should be used for the function FUN.  */

optimization_type
function_optimization_type (struct function *fun)
{
  return (optimize_function_for_speed_p (fun)
	  ? OPTIMIZE_FOR_SPEED
	  : OPTIMIZE_FOR_SIZE);
}

/* Return TRUE when BB should be optimized for size.  */

bool
optimize_bb_for_size_p (const_basic_block bb)
{
  return (optimize_function_for_size_p (cfun)
	  || (bb && !maybe_hot_bb_p (cfun, bb)));
}

/* Return TRUE when BB should be optimized for speed.  */

bool
optimize_bb_for_speed_p (const_basic_block bb)
{
  return !optimize_bb_for_size_p (bb);
}

/* Return the optimization type that should be used for block BB.  */

optimization_type
bb_optimization_type (const_basic_block bb)
{
  return (optimize_bb_for_speed_p (bb)
	  ? OPTIMIZE_FOR_SPEED
	  : OPTIMIZE_FOR_SIZE);
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
  if (!e->probability.initialized_p ())
    return false;
  if ((e->probability.to_reg_br_prob_base ()
       <= PARAM_VALUE (PARAM_PREDICTABLE_BRANCH_OUTCOME) * REG_BR_PROB_BASE / 100)
      || (REG_BR_PROB_BASE - e->probability.to_reg_br_prob_base ()
          <= PARAM_VALUE (PARAM_PREDICTABLE_BRANCH_OUTCOME) * REG_BR_PROB_BASE / 100))
    return true;
  return false;
}


/* Set RTL expansion for BB profile.  */

void
rtl_profile_for_bb (basic_block bb)
{
  crtl->maybe_hot_insn_p = maybe_hot_bb_p (cfun, bb);
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

/*  Structure representing predictions in tree level. */

struct edge_prediction {
    struct edge_prediction *ep_next;
    edge ep_edge;
    enum br_predictor ep_predictor;
    int ep_probability;
};

/* This map contains for a basic block the list of predictions for the
   outgoing edges.  */

static hash_map<const_basic_block, edge_prediction *> *bb_predictions;

/* Return true if the one of outgoing edges is already predicted by
   PREDICTOR.  */

bool
gimple_predicted_by_p (const_basic_block bb, enum br_predictor predictor)
{
  struct edge_prediction *i;
  edge_prediction **preds = bb_predictions->get (bb);

  if (!preds)
    return false;

  for (i = *preds; i; i = i->ep_next)
    if (i->ep_predictor == predictor)
      return true;
  return false;
}

/* Return true if the one of outgoing edges is already predicted by
   PREDICTOR for edge E predicted as TAKEN.  */

bool
edge_predicted_by_p (edge e, enum br_predictor predictor, bool taken)
{
  struct edge_prediction *i;
  basic_block bb = e->src;
  edge_prediction **preds = bb_predictions->get (bb);
  if (!preds)
    return false;

  int probability = predictor_info[(int) predictor].hitrate;

  if (taken != TAKEN)
    probability = REG_BR_PROB_BASE - probability;

  for (i = *preds; i; i = i->ep_next)
    if (i->ep_predictor == predictor
	&& i->ep_edge == e
	&& i->ep_probability == probability)
      return true;
  return false;
}

/* Same predicate as above, working on edges.  */
bool
edge_probability_reliable_p (const_edge e)
{
  return e->probability.probably_reliable_p ();
}

/* Same predicate as edge_probability_reliable_p, working on notes.  */
bool
br_prob_note_reliable_p (const_rtx note)
{
  gcc_assert (REG_NOTE_KIND (note) == REG_BR_PROB);
  return profile_probability::from_reg_br_prob_note
		 (XINT (note, 0)).probably_reliable_p ();
}

static void
predict_insn (rtx_insn *insn, enum br_predictor predictor, int probability)
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
predict_insn_def (rtx_insn *insn, enum br_predictor predictor,
		  enum prediction taken)
{
   int probability = predictor_info[(int) predictor].hitrate;
   gcc_assert (probability != PROB_UNINITIALIZED);

   if (taken != TAKEN)
     probability = REG_BR_PROB_BASE - probability;

   predict_insn (insn, predictor, probability);
}

/* Predict edge E with given probability if possible.  */

void
rtl_predict_edge (edge e, enum br_predictor predictor, int probability)
{
  rtx_insn *last_insn;
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
  if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
      && EDGE_COUNT (e->src->succs) > 1
      && flag_guess_branch_prob
      && optimize)
    {
      struct edge_prediction *i = XNEW (struct edge_prediction);
      edge_prediction *&preds = bb_predictions->get_or_insert (e->src);

      i->ep_next = preds;
      preds = i;
      i->ep_probability = probability;
      i->ep_predictor = predictor;
      i->ep_edge = e;
    }
}

/* Filter edge predictions PREDS by a function FILTER.  DATA are passed
   to the filter function.  */

void
filter_predictions (edge_prediction **preds,
		    bool (*filter) (edge_prediction *, void *), void *data)
{
  if (!bb_predictions)
    return;

  if (preds)
    {
      struct edge_prediction **prediction = preds;
      struct edge_prediction *next;

      while (*prediction)
	{
	  if ((*filter) (*prediction, data))
	    prediction = &((*prediction)->ep_next);
	  else
	    {
	      next = (*prediction)->ep_next;
	      free (*prediction);
	      *prediction = next;
	    }
	}
    }
}

/* Filter function predicate that returns true for a edge predicate P
   if its edge is equal to DATA.  */

bool
equal_edge_p (edge_prediction *p, void *data)
{
  return p->ep_edge == (edge)data;
}

/* Remove all predictions on given basic block that are attached
   to edge E.  */
void
remove_predictions_associated_with_edge (edge e)
{
  if (!bb_predictions)
    return;

  edge_prediction **preds = bb_predictions->get (e->src);
  filter_predictions (preds, equal_edge_p, e);
}

/* Clears the list of predictions stored for BB.  */

static void
clear_bb_predictions (basic_block bb)
{
  edge_prediction **preds = bb_predictions->get (bb);
  struct edge_prediction *pred, *next;

  if (!preds)
    return;

  for (pred = *preds; pred; pred = next)
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
can_predict_insn_p (const rtx_insn *insn)
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
      XINT (note, 0) = profile_probability::from_reg_br_prob_note
			 (XINT (note, 0)).invert ().to_reg_br_prob_note ();
    else if (REG_NOTE_KIND (note) == REG_BR_PRED)
      XEXP (XEXP (note, 0), 1)
	= GEN_INT (REG_BR_PROB_BASE - INTVAL (XEXP (XEXP (note, 0), 1)));
}

/* Dump information about the branch prediction to the output file.  */

static void
dump_prediction (FILE *file, enum br_predictor predictor, int probability,
		 basic_block bb, enum predictor_reason reason = REASON_NONE,
		 edge ep_edge = NULL)
{
  edge e = ep_edge;
  edge_iterator ei;

  if (!file)
    return;

  if (e == NULL)
    FOR_EACH_EDGE (e, ei, bb->succs)
      if (! (e->flags & EDGE_FALLTHRU))
	break;

  char edge_info_str[128];
  if (ep_edge)
    sprintf (edge_info_str, " of edge %d->%d", ep_edge->src->index,
	     ep_edge->dest->index);
  else
    edge_info_str[0] = '\0';

  fprintf (file, "  %s heuristics%s%s: %.2f%%",
	   predictor_info[predictor].name,
	   edge_info_str, reason_messages[reason],
	   probability * 100.0 / REG_BR_PROB_BASE);

  if (bb->count.initialized_p ())
    {
      fprintf (file, "  exec ");
      bb->count.dump (file);
      if (e)
	{
	  fprintf (file, " hit ");
	  e->count ().dump (file);
	  fprintf (file, " (%.1f%%)", e->count ().to_gcov_type() * 100.0
		   / bb->count.to_gcov_type ());
	}
    }

  fprintf (file, "\n");

  /* Print output that be easily read by analyze_brprob.py script. We are
     interested only in counts that are read from GCDA files.  */
  if (dump_file && (dump_flags & TDF_DETAILS)
      && bb->count.precise_p ()
      && reason == REASON_NONE)
    {
      gcc_assert (e->count ().precise_p ());
      fprintf (file, ";;heuristics;%s;%" PRId64 ";%" PRId64 ";%.1f;\n",
	       predictor_info[predictor].name,
	       bb->count.to_gcov_type (), e->count ().to_gcov_type (),
	       probability * 100.0 / REG_BR_PROB_BASE);
    }
}

/* Return true if STMT is known to be unlikely executed.  */

static bool
unlikely_executed_stmt_p (gimple *stmt)
{
  if (!is_gimple_call (stmt))
    return false;
  /* NORETURN attribute alone is not strong enough: exit() may be quite
     likely executed once during program run.  */
  if (gimple_call_fntype (stmt)
      && lookup_attribute ("cold",
			   TYPE_ATTRIBUTES (gimple_call_fntype (stmt)))
      && !lookup_attribute ("cold", DECL_ATTRIBUTES (current_function_decl)))
    return true;
  tree decl = gimple_call_fndecl (stmt);
  if (!decl)
    return false;
  if (lookup_attribute ("cold", DECL_ATTRIBUTES (decl))
      && !lookup_attribute ("cold", DECL_ATTRIBUTES (current_function_decl)))
    return true;

  cgraph_node *n = cgraph_node::get (decl);
  if (!n)
    return false;

  availability avail;
  n = n->ultimate_alias_target (&avail);
  if (avail < AVAIL_AVAILABLE)
    return false;
  if (!n->analyzed
      || n->decl == current_function_decl)
    return false;
  return n->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED;
}

/* Return true if BB is unlikely executed.  */

static bool
unlikely_executed_bb_p (basic_block bb)
{
  if (bb->count == profile_count::zero ())
    return true;
  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun) || bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return false;
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      if (unlikely_executed_stmt_p (gsi_stmt (gsi)))
        return true;
      if (stmt_can_terminate_bb_p (gsi_stmt (gsi)))
	return false;
    }
  return false;
}

/* We cannot predict the probabilities of outgoing edges of bb.  Set them
   evenly and hope for the best.  If UNLIKELY_EDGES is not null, distribute
   even probability for all edges not mentioned in the set.  These edges
   are given PROB_VERY_UNLIKELY probability.  Similarly for LIKELY_EDGES,
   if we have exactly one likely edge, make the other edges predicted
   as not probable.  */

static void
set_even_probabilities (basic_block bb,
			hash_set<edge> *unlikely_edges = NULL,
			hash_set<edge_prediction *> *likely_edges = NULL)
{
  unsigned nedges = 0, unlikely_count = 0;
  edge e = NULL;
  edge_iterator ei;
  profile_probability all = profile_probability::always ();

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->probability.initialized_p ())
      all -= e->probability;
    else if (!unlikely_executed_edge_p (e))
      {
	nedges++;
        if (unlikely_edges != NULL && unlikely_edges->contains (e))
	  {
	    all -= profile_probability::very_unlikely ();
	    unlikely_count++;
	  }
      }

  /* Make the distribution even if all edges are unlikely.  */
  unsigned likely_count = likely_edges ? likely_edges->elements () : 0;
  if (unlikely_count == nedges)
    {
      unlikely_edges = NULL;
      unlikely_count = 0;
    }

  /* If we have one likely edge, then use its probability and distribute
     remaining probabilities as even.  */
  if (likely_count == 1)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->probability.initialized_p ())
	  ;
	else if (!unlikely_executed_edge_p (e))
	  {
	    edge_prediction *prediction = *likely_edges->begin ();
	    int p = prediction->ep_probability;
	    profile_probability prob
	      = profile_probability::from_reg_br_prob_base (p);

	    if (prediction->ep_edge == e)
	      e->probability = prob;
	    else if (unlikely_edges != NULL && unlikely_edges->contains (e))
	      e->probability = profile_probability::very_unlikely ();
	    else
	      {
		profile_probability remainder = prob.invert ();
		remainder -= profile_probability::very_unlikely ()
		  .apply_scale (unlikely_count, 1);
		int count = nedges - unlikely_count - 1;
		gcc_assert (count >= 0);

		e->probability = remainder.apply_scale (1, count);
	      }
	  }
	else
	  e->probability = profile_probability::never ();
    }
  else
    {
      /* Make all unlikely edges unlikely and the rest will have even
	 probability.  */
      unsigned scale = nedges - unlikely_count;
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->probability.initialized_p ())
	  ;
	else if (!unlikely_executed_edge_p (e))
	  {
	    if (unlikely_edges != NULL && unlikely_edges->contains (e))
	      e->probability = profile_probability::very_unlikely ();
	    else
	      e->probability = all.apply_scale (1, scale);
	  }
	else
	  e->probability = profile_probability::never ();
    }
}

/* Add REG_BR_PROB note to JUMP with PROB.  */

void
add_reg_br_prob_note (rtx_insn *jump, profile_probability prob)
{
  gcc_checking_assert (JUMP_P (jump) && !find_reg_note (jump, REG_BR_PROB, 0));
  add_int_reg_note (jump, REG_BR_PROB, prob.to_reg_br_prob_note ());
}

/* Combine all REG_BR_PRED notes into single probability and attach REG_BR_PROB
   note if not already present.  Remove now useless REG_BR_PRED notes.  */

static void
combine_predictions_for_insn (rtx_insn *insn, basic_block bb)
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
	if (best_predictor > predictor
	    && predictor_info[predictor].flags & PRED_FLAG_FIRST_MATCH)
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

  if (best_predictor != END_PREDICTORS)
    first_match = true;

  if (!found)
    dump_prediction (dump_file, PRED_NO_PREDICTION,
		     combined_probability, bb);
  else
    {
      if (!first_match)
	dump_prediction (dump_file, PRED_DS_THEORY, combined_probability,
			 bb, !first_match ? REASON_NONE : REASON_IGNORED);
      else
	dump_prediction (dump_file, PRED_FIRST_MATCH, best_probability,
			 bb, first_match ? REASON_NONE : REASON_IGNORED);
    }

  if (first_match)
    combined_probability = best_probability;
  dump_prediction (dump_file, PRED_COMBINED, combined_probability, bb);

  while (*pnote)
    {
      if (REG_NOTE_KIND (*pnote) == REG_BR_PRED)
	{
	  enum br_predictor predictor = ((enum br_predictor)
					 INTVAL (XEXP (XEXP (*pnote, 0), 0)));
	  int probability = INTVAL (XEXP (XEXP (*pnote, 0), 1));

	  dump_prediction (dump_file, predictor, probability, bb,
			   (!first_match || best_predictor == predictor)
			   ? REASON_NONE : REASON_IGNORED);
	  *pnote = XEXP (*pnote, 1);
	}
      else
	pnote = &XEXP (*pnote, 1);
    }

  if (!prob_note)
    {
      profile_probability p
	 = profile_probability::from_reg_br_prob_base (combined_probability);
      add_reg_br_prob_note (insn, p);

      /* Save the prediction into CFG in case we are seeing non-degenerated
	 conditional jump.  */
      if (!single_succ_p (bb))
	{
	  BRANCH_EDGE (bb)->probability = p;
	  FALLTHRU_EDGE (bb)->probability
	    = BRANCH_EDGE (bb)->probability.invert ();
	}
    }
  else if (!single_succ_p (bb))
    {
      profile_probability prob = profile_probability::from_reg_br_prob_note
					(XINT (prob_note, 0));

      BRANCH_EDGE (bb)->probability = prob;
      FALLTHRU_EDGE (bb)->probability = prob.invert ();
    }
  else
    single_succ_edge (bb)->probability = profile_probability::always ();
}

/* Edge prediction hash traits.  */

struct predictor_hash: pointer_hash <edge_prediction>
{

  static inline hashval_t hash (const edge_prediction *);
  static inline bool equal (const edge_prediction *, const edge_prediction *);
};

/* Calculate hash value of an edge prediction P based on predictor and
   normalized probability.  */

inline hashval_t
predictor_hash::hash (const edge_prediction *p)
{
  inchash::hash hstate;
  hstate.add_int (p->ep_predictor);

  int prob = p->ep_probability;
  if (prob > REG_BR_PROB_BASE / 2)
    prob = REG_BR_PROB_BASE - prob;

  hstate.add_int (prob);

  return hstate.end ();
}

/* Return true whether edge predictions P1 and P2 use the same predictor and
   have equal (or opposed probability).  */

inline bool
predictor_hash::equal (const edge_prediction *p1, const edge_prediction *p2)
{
  return (p1->ep_predictor == p2->ep_predictor
	  && (p1->ep_probability == p2->ep_probability
	      || p1->ep_probability == REG_BR_PROB_BASE - p2->ep_probability));
}

struct predictor_hash_traits: predictor_hash,
  typed_noop_remove <edge_prediction *> {};

/* Return true if edge prediction P is not in DATA hash set.  */

static bool
not_removed_prediction_p (edge_prediction *p, void *data)
{
  hash_set<edge_prediction *> *remove = (hash_set<edge_prediction *> *) data;
  return !remove->contains (p);
}

/* Prune predictions for a basic block BB.  Currently we do following
   clean-up steps:

   1) remove duplicate prediction that is guessed with the same probability
      (different than 1/2) to both edge
   2) remove duplicates for a prediction that belongs with the same probability
      to a single edge

  */

static void
prune_predictions_for_bb (basic_block bb)
{
  edge_prediction **preds = bb_predictions->get (bb);

  if (preds)
    {
      hash_table <predictor_hash_traits> s (13);
      hash_set <edge_prediction *> remove;

      /* Step 1: identify predictors that should be removed.  */
      for (edge_prediction *pred = *preds; pred; pred = pred->ep_next)
	{
	  edge_prediction *existing = s.find (pred);
	  if (existing)
	    {
	      if (pred->ep_edge == existing->ep_edge
		  && pred->ep_probability == existing->ep_probability)
		{
		  /* Remove a duplicate predictor.  */
		  dump_prediction (dump_file, pred->ep_predictor,
				   pred->ep_probability, bb,
				   REASON_SINGLE_EDGE_DUPLICATE, pred->ep_edge);

		  remove.add (pred);
		}
	      else if (pred->ep_edge != existing->ep_edge
		       && pred->ep_probability == existing->ep_probability
		       && pred->ep_probability != REG_BR_PROB_BASE / 2)
		{
		  /* Remove both predictors as they predict the same
		     for both edges.  */
		  dump_prediction (dump_file, existing->ep_predictor,
				   pred->ep_probability, bb,
				   REASON_EDGE_PAIR_DUPLICATE,
				   existing->ep_edge);
		  dump_prediction (dump_file, pred->ep_predictor,
				   pred->ep_probability, bb,
				   REASON_EDGE_PAIR_DUPLICATE,
				   pred->ep_edge);

		  remove.add (existing);
		  remove.add (pred);
		}
	    }

	  edge_prediction **slot2 = s.find_slot (pred, INSERT);
	  *slot2 = pred;
	}

      /* Step 2: Remove predictors.  */
      filter_predictions (preds, not_removed_prediction_p, &remove);
    }
}

/* Combine predictions into single probability and store them into CFG.
   Remove now useless prediction entries.
   If DRY_RUN is set, only produce dumps and do not modify profile.  */

static void
combine_predictions_for_bb (basic_block bb, bool dry_run)
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
  int nzero = 0;
  int nunknown = 0;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (!unlikely_executed_edge_p (e))
        {
	  nedges ++;
	  if (first && !second)
	    second = e;
	  if (!first)
	    first = e;
        }
      else if (!e->probability.initialized_p ())
        e->probability = profile_probability::never ();
     if (!e->probability.initialized_p ())
        nunknown++;
     else if (e->probability == profile_probability::never ())
	nzero++;
    }

  /* When there is no successor or only one choice, prediction is easy.

     When we have a basic block with more than 2 successors, the situation
     is more complicated as DS theory cannot be used literally.
     More precisely, let's assume we predicted edge e1 with probability p1,
     thus: m1({b1}) = p1.  As we're going to combine more than 2 edges, we
     need to find probability of e.g. m1({b2}), which we don't know.
     The only approximation is to equally distribute 1-p1 to all edges
     different from b1.

     According to numbers we've got from SPEC2006 benchark, there's only
     one interesting reliable predictor (noreturn call), which can be
     handled with a bit easier approach.  */
  if (nedges != 2)
    {
      hash_set<edge> unlikely_edges (4);
      hash_set<edge_prediction *> likely_edges (4);

      /* Identify all edges that have a probability close to very unlikely.
	 Doing the approach for very unlikely doesn't worth for doing as
	 there's no such probability in SPEC2006 benchmark.  */
      edge_prediction **preds = bb_predictions->get (bb);
      if (preds)
	for (pred = *preds; pred; pred = pred->ep_next)
	  {
	    if (pred->ep_probability <= PROB_VERY_UNLIKELY
		|| pred->ep_predictor == PRED_COLD_LABEL)
	      unlikely_edges.add (pred->ep_edge);
	    else if (pred->ep_probability >= PROB_VERY_LIKELY
		     || pred->ep_predictor == PRED_BUILTIN_EXPECT
		     || pred->ep_predictor == PRED_HOT_LABEL)
	      likely_edges.add (pred);
	  }

      /* It can happen that an edge is both in likely_edges and unlikely_edges.
	 Clear both sets in that situation.  */
      for (hash_set<edge_prediction *>::iterator it = likely_edges.begin ();
	   it != likely_edges.end (); ++it)
	if (unlikely_edges.contains ((*it)->ep_edge))
	  {
	    likely_edges.empty ();
	    unlikely_edges.empty ();
	    break;
	  }

      if (!dry_run)
	set_even_probabilities (bb, &unlikely_edges, &likely_edges);
      clear_bb_predictions (bb);
      if (dump_file)
	{
	  fprintf (dump_file, "Predictions for bb %i\n", bb->index);
	  if (unlikely_edges.elements () == 0)
	    fprintf (dump_file,
		     "%i edges in bb %i predicted to even probabilities\n",
		     nedges, bb->index);
	  else
	    {
	      fprintf (dump_file,
		       "%i edges in bb %i predicted with some unlikely edges\n",
		       nedges, bb->index);
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (!unlikely_executed_edge_p (e))
		  dump_prediction (dump_file, PRED_COMBINED,
		   e->probability.to_reg_br_prob_base (), bb, REASON_NONE, e);
	    }
	}
      return;
    }

  if (dump_file)
    fprintf (dump_file, "Predictions for bb %i\n", bb->index);

  prune_predictions_for_bb (bb);

  edge_prediction **preds = bb_predictions->get (bb);

  if (preds)
    {
      /* We implement "first match" heuristics and use probability guessed
	 by predictor with smallest index.  */
      for (pred = *preds; pred; pred = pred->ep_next)
	{
	  enum br_predictor predictor = pred->ep_predictor;
	  int probability = pred->ep_probability;

	  if (pred->ep_edge != first)
	    probability = REG_BR_PROB_BASE - probability;

	  found = true;
	  /* First match heuristics would be widly confused if we predicted
	     both directions.  */
	  if (best_predictor > predictor
	    && predictor_info[predictor].flags & PRED_FLAG_FIRST_MATCH)
	    {
              struct edge_prediction *pred2;
	      int prob = probability;

	      for (pred2 = (struct edge_prediction *) *preds;
		   pred2; pred2 = pred2->ep_next)
	       if (pred2 != pred && pred2->ep_predictor == pred->ep_predictor)
	         {
		   int probability2 = pred2->ep_probability;

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

  if (best_predictor != END_PREDICTORS)
    first_match = true;

  if (!found)
    dump_prediction (dump_file, PRED_NO_PREDICTION, combined_probability, bb);
  else
    {
      if (!first_match)
	dump_prediction (dump_file, PRED_DS_THEORY, combined_probability, bb,
			 !first_match ? REASON_NONE : REASON_IGNORED);
      else
	dump_prediction (dump_file, PRED_FIRST_MATCH, best_probability, bb,
			 first_match ? REASON_NONE : REASON_IGNORED);
    }

  if (first_match)
    combined_probability = best_probability;
  dump_prediction (dump_file, PRED_COMBINED, combined_probability, bb);

  if (preds)
    {
      for (pred = (struct edge_prediction *) *preds; pred; pred = pred->ep_next)
	{
	  enum br_predictor predictor = pred->ep_predictor;
	  int probability = pred->ep_probability;

	  dump_prediction (dump_file, predictor, probability, bb,
			   (!first_match || best_predictor == predictor)
			   ? REASON_NONE : REASON_IGNORED, pred->ep_edge);
	}
    }
  clear_bb_predictions (bb);


  /* If we have only one successor which is unknown, we can compute missing
     probablity.  */
  if (nunknown == 1)
    {
      profile_probability prob = profile_probability::always ();
      edge missing = NULL;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->probability.initialized_p ())
	  prob -= e->probability;
	else if (missing == NULL)
	  missing = e;
	else
	  gcc_unreachable ();
       missing->probability = prob;
    }
  /* If nothing is unknown, we have nothing to update.  */
  else if (!nunknown && nzero != (int)EDGE_COUNT (bb->succs))
    ;
  else if (!dry_run)
    {
      first->probability
	 = profile_probability::from_reg_br_prob_base (combined_probability);
      second->probability = first->probability.invert ();
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
  else if (tree_fits_shwi_p (t1))
    value = tree_to_shwi (t1);
  else
    return NULL;

  if (!t2)
    return ret;
  else if (tree_fits_shwi_p (t2))
    value = tree_to_shwi (t2);
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
is_comparison_with_loop_invariant_p (gcond *stmt, struct loop *loop,
				     tree *loop_invariant,
				     enum tree_code *compare_code,
				     tree *loop_step,
				     tree *loop_iv_base)
{
  tree op0, op1, bound, base;
  affine_iv iv0, iv1;
  enum tree_code code;
  tree step;

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
      if (tree_fits_shwi_p (iv1.step))
	step = iv1.step;
      else
	return false;
    }
  else
    {
      bound = iv1.base;
      base = iv0.base;
      if (tree_fits_shwi_p (iv0.step))
	step = iv0.step;
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
  gimple *stmt;
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

/* Return true if E is predicted by one of loop heuristics.  */

static bool
predicted_by_loop_heuristics_p (basic_block bb)
{
  struct edge_prediction *i;
  edge_prediction **preds = bb_predictions->get (bb);

  if (!preds)
    return false;

  for (i = *preds; i; i = i->ep_next)
    if (i->ep_predictor == PRED_LOOP_ITERATIONS_GUESSED
	|| i->ep_predictor == PRED_LOOP_ITERATIONS_MAX
	|| i->ep_predictor == PRED_LOOP_ITERATIONS
	|| i->ep_predictor == PRED_LOOP_EXIT
	|| i->ep_predictor == PRED_LOOP_EXIT_WITH_RECURSION
	|| i->ep_predictor == PRED_LOOP_EXTRA_EXIT)
      return true;
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
  gimple *stmt;
  tree compare_var, compare_base;
  enum tree_code compare_code;
  tree compare_step_var;
  edge then_edge;
  edge_iterator ei;

  if (predicted_by_loop_heuristics_p (bb))
    return;

  stmt = last_stmt (bb);
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return;
  if (!is_comparison_with_loop_invariant_p (as_a <gcond *> (stmt),
					    loop, &compare_var,
					    &compare_code,
					    &compare_step_var,
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
  if (tree_fits_shwi_p (loop_bound_var)
      && tree_fits_shwi_p (compare_var)
      && tree_fits_shwi_p (compare_base))
    {
      int probability;
      wi::overflow_type overflow;
      bool overall_overflow = false;
      widest_int compare_count, tem;

      /* (loop_bound - base) / compare_step */
      tem = wi::sub (wi::to_widest (loop_bound_var),
		     wi::to_widest (compare_base), SIGNED, &overflow);
      overall_overflow |= overflow;
      widest_int loop_count = wi::div_trunc (tem,
					     wi::to_widest (compare_step_var),
					     SIGNED, &overflow);
      overall_overflow |= overflow;

      if (!wi::neg_p (wi::to_widest (compare_step_var))
          ^ (compare_code == LT_EXPR || compare_code == LE_EXPR))
	{
	  /* (loop_bound - compare_bound) / compare_step */
	  tem = wi::sub (wi::to_widest (loop_bound_var),
			 wi::to_widest (compare_var), SIGNED, &overflow);
	  overall_overflow |= overflow;
	  compare_count = wi::div_trunc (tem, wi::to_widest (compare_step_var),
					 SIGNED, &overflow);
	  overall_overflow |= overflow;
	}
      else
        {
	  /* (compare_bound - base) / compare_step */
	  tem = wi::sub (wi::to_widest (compare_var),
			 wi::to_widest (compare_base), SIGNED, &overflow);
	  overall_overflow |= overflow;
          compare_count = wi::div_trunc (tem, wi::to_widest (compare_step_var),
					 SIGNED, &overflow);
	  overall_overflow |= overflow;
	}
      if (compare_code == LE_EXPR || compare_code == GE_EXPR)
	++compare_count;
      if (loop_bound_code == LE_EXPR || loop_bound_code == GE_EXPR)
	++loop_count;
      if (wi::neg_p (compare_count))
        compare_count = 0;
      if (wi::neg_p (loop_count))
        loop_count = 0;
      if (loop_count == 0)
	probability = 0;
      else if (wi::cmps (compare_count, loop_count) == 1)
	probability = REG_BR_PROB_BASE;
      else
        {
	  tem = compare_count * REG_BR_PROB_BASE;
	  tem = wi::udiv_trunc (tem, loop_count);
	  probability = tem.to_uhwi ();
	}

      /* FIXME: The branch prediction seems broken. It has only 20% hitrate.  */
      if (!overall_overflow)
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

/* Predict for extra loop exits that will lead to EXIT_EDGE. The extra loop
   exits are resulted from short-circuit conditions that will generate an
   if_tmp. E.g.:

   if (foo() || global > 10)
     break;

   This will be translated into:

   BB3:
     loop header...
   BB4:
     if foo() goto BB6 else goto BB5
   BB5:
     if global > 10 goto BB6 else goto BB7
   BB6:
     goto BB7
   BB7:
     iftmp = (PHI 0(BB5), 1(BB6))
     if iftmp == 1 goto BB8 else goto BB3
   BB8:
     outside of the loop...

   The edge BB7->BB8 is loop exit because BB8 is outside of the loop.
   From the dataflow, we can infer that BB4->BB6 and BB5->BB6 are also loop
   exits. This function takes BB7->BB8 as input, and finds out the extra loop
   exits to predict them using PRED_LOOP_EXTRA_EXIT.  */

static void
predict_extra_loop_exits (edge exit_edge)
{
  unsigned i;
  bool check_value_one;
  gimple *lhs_def_stmt;
  gphi *phi_stmt;
  tree cmp_rhs, cmp_lhs;
  gimple *last;
  gcond *cmp_stmt;

  last = last_stmt (exit_edge->src);
  if (!last)
    return;
  cmp_stmt = dyn_cast <gcond *> (last);
  if (!cmp_stmt)
    return;

  cmp_rhs = gimple_cond_rhs (cmp_stmt);
  cmp_lhs = gimple_cond_lhs (cmp_stmt);
  if (!TREE_CONSTANT (cmp_rhs)
      || !(integer_zerop (cmp_rhs) || integer_onep (cmp_rhs)))
    return;
  if (TREE_CODE (cmp_lhs) != SSA_NAME)
    return;

  /* If check_value_one is true, only the phi_args with value '1' will lead
     to loop exit. Otherwise, only the phi_args with value '0' will lead to
     loop exit.  */
  check_value_one = (((integer_onep (cmp_rhs))
		    ^ (gimple_cond_code (cmp_stmt) == EQ_EXPR))
		    ^ ((exit_edge->flags & EDGE_TRUE_VALUE) != 0));

  lhs_def_stmt = SSA_NAME_DEF_STMT (cmp_lhs);
  if (!lhs_def_stmt)
    return;

  phi_stmt = dyn_cast <gphi *> (lhs_def_stmt);
  if (!phi_stmt)
    return;

  for (i = 0; i < gimple_phi_num_args (phi_stmt); i++)
    {
      edge e1;
      edge_iterator ei;
      tree val = gimple_phi_arg_def (phi_stmt, i);
      edge e = gimple_phi_arg_edge (phi_stmt, i);

      if (!TREE_CONSTANT (val) || !(integer_zerop (val) || integer_onep (val)))
	continue;
      if ((check_value_one ^ integer_onep (val)) == 1)
	continue;
      if (EDGE_COUNT (e->src->succs) != 1)
	{
	  predict_paths_leading_to_edge (e, PRED_LOOP_EXTRA_EXIT, NOT_TAKEN);
	  continue;
	}

      FOR_EACH_EDGE (e1, ei, e->src->preds)
	predict_paths_leading_to_edge (e1, PRED_LOOP_EXTRA_EXIT, NOT_TAKEN);
    }
}


/* Predict edge probabilities by exploiting loop structure.  */

static void
predict_loops (void)
{
  struct loop *loop;
  basic_block bb;
  hash_set <struct loop *> with_recursion(10);

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      tree decl;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	if (is_gimple_call (gsi_stmt (gsi))
	    && (decl = gimple_call_fndecl (gsi_stmt (gsi))) != NULL
	    && recursive_call_p (current_function_decl, decl))
	  {
	    loop = bb->loop_father;
	    while (loop && !with_recursion.add (loop))
	      loop = loop_outer (loop);
	  }
    }

  /* Try to predict out blocks in a loop that are not part of a
     natural loop.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      basic_block bb, *bbs;
      unsigned j, n_exits = 0;
      vec<edge> exits;
      struct tree_niter_desc niter_desc;
      edge ex;
      struct nb_iter_bound *nb_iter;
      enum tree_code loop_bound_code = ERROR_MARK;
      tree loop_bound_step = NULL;
      tree loop_bound_var = NULL;
      tree loop_iv_base = NULL;
      gcond *stmt = NULL;
      bool recursion = with_recursion.contains (loop);

      exits = get_loop_exit_edges (loop);
      FOR_EACH_VEC_ELT (exits, j, ex)
	if (!unlikely_executed_edge_p (ex) && !(ex->flags & EDGE_ABNORMAL_CALL))
	  n_exits ++;
      if (!n_exits)
	{
          exits.release ();
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Predicting loop %i%s with %i exits.\n",
		 loop->num, recursion ? " (with recursion)":"", n_exits);
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && max_loop_iterations_int (loop) >= 0)
	{
	  fprintf (dump_file,
		   "Loop %d iterates at most %i times.\n", loop->num,
		   (int)max_loop_iterations_int (loop));
	}
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && likely_max_loop_iterations_int (loop) >= 0)
	{
	  fprintf (dump_file, "Loop %d likely iterates at most %i times.\n",
		   loop->num, (int)likely_max_loop_iterations_int (loop));
	}

      FOR_EACH_VEC_ELT (exits, j, ex)
	{
	  tree niter = NULL;
	  HOST_WIDE_INT nitercst;
	  int max = PARAM_VALUE (PARAM_MAX_PREDICTED_ITERATIONS);
	  int probability;
	  enum br_predictor predictor;
	  widest_int nit;

	  if (unlikely_executed_edge_p (ex)
	      || (ex->flags & EDGE_ABNORMAL_CALL))
	    continue;
	  /* Loop heuristics do not expect exit conditional to be inside
	     inner loop.  We predict from innermost to outermost loop.  */
	  if (predicted_by_loop_heuristics_p (ex->src))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Skipping exit %i->%i because "
			 "it is already predicted.\n",
			 ex->src->index, ex->dest->index);
	      continue;
	    }
	  predict_extra_loop_exits (ex);

	  if (number_of_iterations_exit (loop, ex, &niter_desc, false, false))
	    niter = niter_desc.niter;
	  if (!niter || TREE_CODE (niter_desc.niter) != INTEGER_CST)
	    niter = loop_niter_by_eval (loop, ex);
	  if (dump_file && (dump_flags & TDF_DETAILS)
	      && TREE_CODE (niter) == INTEGER_CST)
	    {
	      fprintf (dump_file, "Exit %i->%i %d iterates ",
		       ex->src->index, ex->dest->index,
		       loop->num);
	      print_generic_expr (dump_file, niter, TDF_SLIM);
	      fprintf (dump_file, " times.\n");
	    }

	  if (TREE_CODE (niter) == INTEGER_CST)
	    {
	      if (tree_fits_uhwi_p (niter)
		  && max
		  && compare_tree_int (niter, max - 1) == -1)
		nitercst = tree_to_uhwi (niter) + 1;
	      else
		nitercst = max;
	      predictor = PRED_LOOP_ITERATIONS;
	    }
	  /* If we have just one exit and we can derive some information about
	     the number of iterations of the loop from the statements inside
	     the loop, use it to predict this exit.  */
	  else if (n_exits == 1
		   && estimated_stmt_executions (loop, &nit))
	    {
	      if (wi::gtu_p (nit, max))
		nitercst = max;
	      else
		nitercst = nit.to_shwi ();
	      predictor = PRED_LOOP_ITERATIONS_GUESSED;
	    }
	  /* If we have likely upper bound, trust it for very small iteration
	     counts.  Such loops would otherwise get mispredicted by standard
	     LOOP_EXIT heuristics.  */
	  else if (n_exits == 1
		   && likely_max_stmt_executions (loop, &nit)
		   && wi::ltu_p (nit,
				 RDIV (REG_BR_PROB_BASE,
				       REG_BR_PROB_BASE
					 - predictor_info
						 [recursion
						  ? PRED_LOOP_EXIT_WITH_RECURSION
						  : PRED_LOOP_EXIT].hitrate)))
	    {
	      nitercst = nit.to_shwi ();
	      predictor = PRED_LOOP_ITERATIONS_MAX;
	    }
	  else
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Nothing known about exit %i->%i.\n",
			 ex->src->index, ex->dest->index);
	      continue;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Recording prediction to %i iterations by %s.\n",
		     (int)nitercst, predictor_info[predictor].name);
	  /* If the prediction for number of iterations is zero, do not
	     predict the exit edges.  */
	  if (nitercst == 0)
	    continue;

	  probability = RDIV (REG_BR_PROB_BASE, nitercst);
	  predict_edge (ex, predictor, probability);
	}
      exits.release ();

      /* Find information about loop bound variables.  */
      for (nb_iter = loop->bounds; nb_iter;
	   nb_iter = nb_iter->next)
	if (nb_iter->stmt
	    && gimple_code (nb_iter->stmt) == GIMPLE_COND)
	  {
	    stmt = as_a <gcond *> (nb_iter->stmt);
	    break;
	  }
      if (!stmt && last_stmt (loop->header)
	  && gimple_code (last_stmt (loop->header)) == GIMPLE_COND)
	stmt = as_a <gcond *> (last_stmt (loop->header));
      if (stmt)
	is_comparison_with_loop_invariant_p (stmt, loop,
					     &loop_bound_var,
					     &loop_bound_code,
					     &loop_bound_step,
					     &loop_iv_base);

      bbs = get_loop_body (loop);

      for (j = 0; j < loop->num_nodes; j++)
	{
	  edge e;
	  edge_iterator ei;

	  bb = bbs[j];

	  /* Bypass loop heuristics on continue statement.  These
	     statements construct loops via "non-loop" constructs
	     in the source language and are better to be handled
	     separately.  */
	  if (predicted_by_p (bb, PRED_CONTINUE))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "BB %i predicted by continue.\n",
			 bb->index);
	      continue;
	    }

	  /* If we already used more reliable loop exit predictors, do not
	     bother with PRED_LOOP_EXIT.  */
	  if (!predicted_by_loop_heuristics_p (bb))
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
		                  - predictor_info
				     [recursion
				      ? PRED_LOOP_EXIT_WITH_RECURSION
				      : PRED_LOOP_EXIT].hitrate)
				 / n_exits);
	      if (probability < HITRATE (2))
		probability = HITRATE (2);
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (e->dest->index < NUM_FIXED_BLOCKS
		    || !flow_bb_inside_loop_p (loop, e->dest))
		  {
		    if (dump_file && (dump_flags & TDF_DETAILS))
		      fprintf (dump_file,
			       "Predicting exit %i->%i with prob %i.\n",
			       e->src->index, e->dest->index, probability);
		    predict_edge (e,
				  recursion ? PRED_LOOP_EXIT_WITH_RECURSION
			          : PRED_LOOP_EXIT, probability);
		  }
	    }
	  if (loop_bound_var)
	    predict_iv_comparison (loop, bb, loop_bound_var, loop_iv_base,
				   loop_bound_code,
				   tree_to_shwi (loop_bound_step));
	}

      /* In the following code
	 for (loop1)
	   if (cond)
	     for (loop2)
	       body;
	 guess that cond is unlikely.  */
      if (loop_outer (loop)->num)
	{
	  basic_block bb = NULL;
	  edge preheader_edge = loop_preheader_edge (loop);

	  if (single_pred_p (preheader_edge->src)
	      && single_succ_p (preheader_edge->src))
	    preheader_edge = single_pred_edge (preheader_edge->src);

	  gimple *stmt = last_stmt (preheader_edge->src);
	  /* Pattern match fortran loop preheader:
	     _16 = BUILTIN_EXPECT (_15, 1, PRED_FORTRAN_LOOP_PREHEADER);
	     _17 = (logical(kind=4)) _16;
	     if (_17 != 0)
	       goto <bb 11>;
	     else
	       goto <bb 13>;

	     Loop guard branch prediction says nothing about duplicated loop
	     headers produced by fortran frontend and in this case we want
	     to predict paths leading to this preheader.  */

	  if (stmt
	      && gimple_code (stmt) == GIMPLE_COND
	      && gimple_cond_code (stmt) == NE_EXPR
	      && TREE_CODE (gimple_cond_lhs (stmt)) == SSA_NAME
	      && integer_zerop (gimple_cond_rhs (stmt)))
	     {
	       gimple *call_stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (stmt));
	       if (gimple_code (call_stmt) == GIMPLE_ASSIGN
		   && gimple_expr_code (call_stmt) == NOP_EXPR
		   && TREE_CODE (gimple_assign_rhs1 (call_stmt)) == SSA_NAME)
		 call_stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (call_stmt));
	       if (gimple_call_internal_p (call_stmt, IFN_BUILTIN_EXPECT)
		   && TREE_CODE (gimple_call_arg (call_stmt, 2)) == INTEGER_CST
		   && tree_fits_uhwi_p (gimple_call_arg (call_stmt, 2))
		   && tree_to_uhwi (gimple_call_arg (call_stmt, 2))
			== PRED_FORTRAN_LOOP_PREHEADER)
		 bb = preheader_edge->src;
	     }
	  if (!bb)
	    {
	      if (!dominated_by_p (CDI_DOMINATORS,
				   loop_outer (loop)->latch, loop->header))
		predict_paths_leading_to_edge (loop_preheader_edge (loop),
					       recursion
					       ? PRED_LOOP_GUARD_WITH_RECURSION
					       : PRED_LOOP_GUARD,
					       NOT_TAKEN,
					       loop_outer (loop));
	    }
	  else
	    {
	      if (!dominated_by_p (CDI_DOMINATORS,
				   loop_outer (loop)->latch, bb))
		predict_paths_leading_to (bb,
					  recursion
					  ? PRED_LOOP_GUARD_WITH_RECURSION
					  : PRED_LOOP_GUARD,
					  NOT_TAKEN,
					  loop_outer (loop));
	    }
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
  rtx_insn *last_insn = BB_END (bb);
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

static tree expr_expected_value (tree, bitmap, enum br_predictor *predictor,
				 HOST_WIDE_INT *probability);

/* Helper function for expr_expected_value.  */

static tree
expr_expected_value_1 (tree type, tree op0, enum tree_code code,
		       tree op1, bitmap visited, enum br_predictor *predictor,
		       HOST_WIDE_INT *probability)
{
  gimple *def;

  /* Reset returned probability value.  */
  *probability = -1;
  *predictor = PRED_UNCONDITIONAL;

  if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS)
    {
      if (TREE_CONSTANT (op0))
	return op0;

      if (code == IMAGPART_EXPR)
	{
	  if (TREE_CODE (TREE_OPERAND (op0, 0)) == SSA_NAME)
	    {
	      def = SSA_NAME_DEF_STMT (TREE_OPERAND (op0, 0));
	      if (is_gimple_call (def)
		  && gimple_call_internal_p (def)
		  && (gimple_call_internal_fn (def)
		      == IFN_ATOMIC_COMPARE_EXCHANGE))
		{
		  /* Assume that any given atomic operation has low contention,
		     and thus the compare-and-swap operation succeeds.  */
		  *predictor = PRED_COMPARE_AND_SWAP;
		  return build_one_cst (TREE_TYPE (op0));
		}
	    }
	}

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
	      enum br_predictor predictor2;

	      /* If this PHI has itself as an argument, we cannot
		 determine the string length of this argument.  However,
		 if we can find an expected constant value for the other
		 PHI args then we can still be sure that this is
		 likely a constant.  So be optimistic and just
		 continue with the next argument.  */
	      if (arg == PHI_RESULT (def))
		continue;

	      HOST_WIDE_INT probability2;
	      new_val = expr_expected_value (arg, visited, &predictor2,
					     &probability2);

	      /* It is difficult to combine value predictors.  Simply assume
		 that later predictor is weaker and take its prediction.  */
	      if (*predictor < predictor2)
		{
		  *predictor = predictor2;
		  *probability = probability2;
		}
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
					visited, predictor, probability);
	}

      if (is_gimple_call (def))
	{
	  tree decl = gimple_call_fndecl (def);
	  if (!decl)
	    {
	      if (gimple_call_internal_p (def)
		  && gimple_call_internal_fn (def) == IFN_BUILTIN_EXPECT)
		{
		  gcc_assert (gimple_call_num_args (def) == 3);
		  tree val = gimple_call_arg (def, 0);
		  if (TREE_CONSTANT (val))
		    return val;
		  tree val2 = gimple_call_arg (def, 2);
		  gcc_assert (TREE_CODE (val2) == INTEGER_CST
			      && tree_fits_uhwi_p (val2)
			      && tree_to_uhwi (val2) < END_PREDICTORS);
		  *predictor = (enum br_predictor) tree_to_uhwi (val2);
		  if (*predictor == PRED_BUILTIN_EXPECT)
		    *probability
		      = HITRATE (PARAM_VALUE (BUILTIN_EXPECT_PROBABILITY));
		  return gimple_call_arg (def, 1);
		}
	      return NULL;
	    }

	  if (DECL_IS_MALLOC (decl) || DECL_IS_OPERATOR_NEW (decl))
	    {
	      if (predictor)
		*predictor = PRED_MALLOC_NONNULL;
	      return boolean_true_node;
	    }

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
		  *predictor = PRED_BUILTIN_EXPECT;
		  *probability
		    = HITRATE (PARAM_VALUE (BUILTIN_EXPECT_PROBABILITY));
		  return gimple_call_arg (def, 1);
		}
	      case BUILT_IN_EXPECT_WITH_PROBABILITY:
		{
		  tree val;
		  if (gimple_call_num_args (def) != 3)
		    return NULL;
		  val = gimple_call_arg (def, 0);
		  if (TREE_CONSTANT (val))
		    return val;
		  /* Compute final probability as:
		     probability * REG_BR_PROB_BASE.  */
		  tree prob = gimple_call_arg (def, 2);
		  tree t = TREE_TYPE (prob);
		  tree base = build_int_cst (integer_type_node,
					     REG_BR_PROB_BASE);
		  base = build_real_from_int_cst (t, base);
		  tree r = fold_build2_initializer_loc (UNKNOWN_LOCATION,
							MULT_EXPR, t, prob, base);
		  if (TREE_CODE (r) != REAL_CST)
		    {
		      error_at (gimple_location (def),
				"probability %qE must be "
				"constant floating-point expression", prob);
		      return NULL;
		    }
		  HOST_WIDE_INT probi
		    = real_to_integer (TREE_REAL_CST_PTR (r));
		  if (probi >= 0 && probi <= REG_BR_PROB_BASE)
		    {
		      *predictor = PRED_BUILTIN_EXPECT_WITH_PROBABILITY;
		      *probability = probi;
		    }
		  else
		    error_at (gimple_location (def),
			      "probability %qE is outside "
			      "the range [0.0, 1.0]", prob);

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
		*predictor = PRED_COMPARE_AND_SWAP;
		return boolean_true_node;
	      case BUILT_IN_REALLOC:
		if (predictor)
		  *predictor = PRED_MALLOC_NONNULL;
		return boolean_true_node;
	      default:
		break;
	    }
	}

      return NULL;
    }

  if (get_gimple_rhs_class (code) == GIMPLE_BINARY_RHS)
    {
      tree res;
      enum br_predictor predictor2;
      HOST_WIDE_INT probability2;
      op0 = expr_expected_value (op0, visited, predictor, probability);
      if (!op0)
	return NULL;
      op1 = expr_expected_value (op1, visited, &predictor2, &probability2);
      if (!op1)
	return NULL;
      res = fold_build2 (code, type, op0, op1);
      if (TREE_CODE (res) == INTEGER_CST
	  && TREE_CODE (op0) == INTEGER_CST
	  && TREE_CODE (op1) == INTEGER_CST)
	{
	  /* Combine binary predictions.  */
	  if (*probability != -1 || probability2 != -1)
	    {
	      HOST_WIDE_INT p1 = get_predictor_value (*predictor, *probability);
	      HOST_WIDE_INT p2 = get_predictor_value (predictor2, probability2);
	      *probability = RDIV (p1 * p2, REG_BR_PROB_BASE);
	    }

	  if (*predictor < predictor2)
	    *predictor = predictor2;

	  return res;
	}
      return NULL;
    }
  if (get_gimple_rhs_class (code) == GIMPLE_UNARY_RHS)
    {
      tree res;
      op0 = expr_expected_value (op0, visited, predictor, probability);
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
expr_expected_value (tree expr, bitmap visited,
		     enum br_predictor *predictor,
		     HOST_WIDE_INT *probability)
{
  enum tree_code code;
  tree op0, op1;

  if (TREE_CONSTANT (expr))
    {
      *predictor = PRED_UNCONDITIONAL;
      *probability = -1;
      return expr;
    }

  extract_ops_from_tree (expr, &code, &op0, &op1);
  return expr_expected_value_1 (TREE_TYPE (expr),
				op0, code, op1, visited, predictor,
				probability);
}


/* Return probability of a PREDICTOR.  If the predictor has variable
   probability return passed PROBABILITY.  */

static HOST_WIDE_INT
get_predictor_value (br_predictor predictor, HOST_WIDE_INT probability)
{
  switch (predictor)
    {
    case PRED_BUILTIN_EXPECT:
    case PRED_BUILTIN_EXPECT_WITH_PROBABILITY:
      gcc_assert (probability != -1);
      return probability;
    default:
      gcc_assert (probability == -1);
      return predictor_info[(int) predictor].hitrate;
    }
}

/* Predict using opcode of the last statement in basic block.  */
static void
tree_predict_by_opcode (basic_block bb)
{
  gimple *stmt = last_stmt (bb);
  edge then_edge;
  tree op0, op1;
  tree type;
  tree val;
  enum tree_code cmp;
  edge_iterator ei;
  enum br_predictor predictor;
  HOST_WIDE_INT probability;

  if (!stmt)
    return;

  if (gswitch *sw = dyn_cast <gswitch *> (stmt))
    {
      tree index = gimple_switch_index (sw);
      tree val = expr_expected_value (index, auto_bitmap (),
				      &predictor, &probability);
      if (val && TREE_CODE (val) == INTEGER_CST)
	{
	  edge e = find_taken_edge_switch_expr (sw, val);
	  if (predictor == PRED_BUILTIN_EXPECT)
	    {
	      int percent = PARAM_VALUE (BUILTIN_EXPECT_PROBABILITY);
	      gcc_assert (percent >= 0 && percent <= 100);
	      predict_edge (e, PRED_BUILTIN_EXPECT,
			    HITRATE (percent));
	    }
	  else
	    predict_edge_def (e, predictor, TAKEN);
	}
    }

  if (gimple_code (stmt) != GIMPLE_COND)
    return;
  FOR_EACH_EDGE (then_edge, ei, bb->succs)
    if (then_edge->flags & EDGE_TRUE_VALUE)
      break;
  op0 = gimple_cond_lhs (stmt);
  op1 = gimple_cond_rhs (stmt);
  cmp = gimple_cond_code (stmt);
  type = TREE_TYPE (op0);
  val = expr_expected_value_1 (boolean_type_node, op0, cmp, op1, auto_bitmap (),
			       &predictor, &probability);
  if (val && TREE_CODE (val) == INTEGER_CST)
    {
      HOST_WIDE_INT prob = get_predictor_value (predictor, probability);
      if (integer_zerop (val))
	prob = REG_BR_PROB_BASE - prob;
      predict_edge (then_edge, predictor, prob);
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

/* Returns TRUE if the STMT is exit(0) like statement. */

static bool
is_exit_with_zero_arg (const gimple *stmt)
{
  /* This is not exit, _exit or _Exit. */
  if (!gimple_call_builtin_p (stmt, BUILT_IN_EXIT)
      && !gimple_call_builtin_p (stmt, BUILT_IN__EXIT)
      && !gimple_call_builtin_p (stmt, BUILT_IN__EXIT2))
    return false;

  /* Argument is an interger zero. */
  return integer_zerop (gimple_call_arg (stmt, 0));
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
	  *prediction = NOT_TAKEN;
	  return PRED_CONST_RETURN;
	}
    }
  return PRED_NO_PREDICTION;
}

/* Return zero if phi result could have values other than -1, 0 or 1,
   otherwise return a bitmask, with bits 0, 1 and 2 set if -1, 0 and 1
   values are used or likely.  */

static int
zero_one_minusone (gphi *phi, int limit)
{
  int phi_num_args = gimple_phi_num_args (phi);
  int ret = 0;
  for (int i = 0; i < phi_num_args; i++)
    {
      tree t = PHI_ARG_DEF (phi, i);
      if (TREE_CODE (t) != INTEGER_CST)
	continue;
      wide_int w = wi::to_wide (t);
      if (w == -1)
	ret |= 1;
      else if (w == 0)
	ret |= 2;
      else if (w == 1)
	ret |= 4;
      else
	return 0;
    }
  for (int i = 0; i < phi_num_args; i++)
    {
      tree t = PHI_ARG_DEF (phi, i);
      if (TREE_CODE (t) == INTEGER_CST)
	continue;
      if (TREE_CODE (t) != SSA_NAME)
	return 0;
      gimple *g = SSA_NAME_DEF_STMT (t);
      if (gimple_code (g) == GIMPLE_PHI && limit > 0)
	if (int r = zero_one_minusone (as_a <gphi *> (g), limit - 1))
	  {
	    ret |= r;
	    continue;
	  }
      if (!is_gimple_assign (g))
	return 0;
      if (gimple_assign_cast_p (g))
	{
	  tree rhs1 = gimple_assign_rhs1 (g);
	  if (TREE_CODE (rhs1) != SSA_NAME
	      || !INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	      || TYPE_PRECISION (TREE_TYPE (rhs1)) != 1
	      || !TYPE_UNSIGNED (TREE_TYPE (rhs1)))
	    return 0;
	  ret |= (2 | 4);
	  continue;
	}
      if (TREE_CODE_CLASS (gimple_assign_rhs_code (g)) != tcc_comparison)
	return 0;
      ret |= (2 | 4);
    }
  return ret;
}

/* Find the basic block with return expression and look up for possible
   return value trying to apply RETURN_PREDICTION heuristics.  */
static void
apply_return_prediction (void)
{
  greturn *return_stmt = NULL;
  tree return_val;
  edge e;
  gphi *phi;
  int phi_num_args, i;
  enum br_predictor pred;
  enum prediction direction;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      gimple *last = last_stmt (e->src);
      if (last
	  && gimple_code (last) == GIMPLE_RETURN)
	{
	  return_stmt = as_a <greturn *> (last);
	  break;
	}
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
  phi = as_a <gphi *> (SSA_NAME_DEF_STMT (return_val));
  phi_num_args = gimple_phi_num_args (phi);
  pred = return_prediction (PHI_ARG_DEF (phi, 0), &direction);

  /* Avoid the case where the function returns -1, 0 and 1 values and
     nothing else.  Those could be qsort etc. comparison functions
     where the negative return isn't less probable than positive.
     For this require that the function returns at least -1 or 1
     or -1 and a boolean value or comparison result, so that functions
     returning just -1 and 0 are treated as if -1 represents error value.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (return_val))
      && !TYPE_UNSIGNED (TREE_TYPE (return_val))
      && TYPE_PRECISION (TREE_TYPE (return_val)) > 1)
    if (int r = zero_one_minusone (phi, 3))
      if ((r & (1 | 4)) == (1 | 4))
	return;

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

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if (!unlikely_executed_edge_p (e) && !(e->flags & EDGE_ABNORMAL_CALL))
      {
        has_return_edges = true;
	break;
      }

  apply_return_prediction ();

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree decl;

	  if (is_gimple_call (stmt))
	    {
	      if (gimple_call_noreturn_p (stmt)
		  && has_return_edges
		  && !is_exit_with_zero_arg (stmt))
		predict_paths_leading_to (bb, PRED_NORETURN,
					  NOT_TAKEN);
	      decl = gimple_call_fndecl (stmt);
	      if (decl
		  && lookup_attribute ("cold",
				       DECL_ATTRIBUTES (decl)))
		predict_paths_leading_to (bb, PRED_COLD_FUNCTION,
					  NOT_TAKEN);
	      if (decl && recursive_call_p (current_function_decl, decl))
		predict_paths_leading_to (bb, PRED_RECURSIVE_CALL,
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

/* Callback for hash_map::traverse, asserts that the pointer map is
   empty.  */

bool
assert_is_empty (const_basic_block const &, edge_prediction *const &value,
		 void *)
{
  gcc_assert (!value);
  return false;
}

/* Predict branch probabilities and estimate profile for basic block BB.
   When LOCAL_ONLY is set do not use any global properties of CFG.  */

static void
tree_estimate_probability_bb (basic_block bb, bool local_only)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      /* Look for block we are guarding (ie we dominate it,
	 but it doesn't postdominate us).  */
      if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun) && e->dest != bb
	  && !local_only
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
	      gimple *stmt = gsi_stmt (bi);
	      if (is_gimple_call (stmt)
		  && !gimple_inexpensive_call_p (as_a <gcall *>  (stmt))
		  /* Constant and pure calls are hardly used to signalize
		     something exceptional.  */
		  && gimple_has_side_effects (stmt))
		{
		  if (gimple_call_fndecl (stmt))
		    predict_edge_def (e, PRED_CALL, NOT_TAKEN);
		  else if (virtual_method_call_p (gimple_call_fn (stmt)))
		    predict_edge_def (e, PRED_POLYMORPHIC_CALL, NOT_TAKEN);
		  else
		    predict_edge_def (e, PRED_INDIR_CALL, TAKEN);
		  break;
		}
	    }
	}
    }
  tree_predict_by_opcode (bb);
}

/* Predict branch probabilities and estimate profile of the tree CFG.
   This function can be called from the loop optimizers to recompute
   the profile information.
   If DRY_RUN is set, do not modify CFG and only produce dump files.  */

void
tree_estimate_probability (bool dry_run)
{
  basic_block bb;

  add_noreturn_fake_exit_edges ();
  connect_infinite_loops_to_exit ();
  /* We use loop_niter_by_eval, which requires that the loops have
     preheaders.  */
  create_preheaders (CP_SIMPLE_PREHEADERS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  /* Decide which edges are known to be unlikely.  This improves later
     branch prediction. */
  determine_unlikely_bbs ();

  bb_predictions = new hash_map<const_basic_block, edge_prediction *>;
  tree_bb_level_predictions ();
  record_loop_exits ();

  if (number_of_loops (cfun) > 1)
    predict_loops ();

  FOR_EACH_BB_FN (bb, cfun)
    tree_estimate_probability_bb (bb, false);

  FOR_EACH_BB_FN (bb, cfun)
    combine_predictions_for_bb (bb, dry_run);

  if (flag_checking)
    bb_predictions->traverse<void *, assert_is_empty> (NULL);

  delete bb_predictions;
  bb_predictions = NULL;

  if (!dry_run)
    estimate_bb_frequencies (false);
  free_dominance_info (CDI_POST_DOMINATORS);
  remove_fake_exit_edges ();
}

/* Set edge->probability for each successor edge of BB.  */
void
tree_guess_outgoing_edge_probabilities (basic_block bb)
{
  bb_predictions = new hash_map<const_basic_block, edge_prediction *>;
  tree_estimate_probability_bb (bb, true);
  combine_predictions_for_bb (bb, false);
  if (flag_checking)
    bb_predictions->traverse<void *, assert_is_empty> (NULL);
  delete bb_predictions;
  bb_predictions = NULL;
}

/* Predict edges to successors of CUR whose sources are not postdominated by
   BB by PRED and recurse to all postdominators.  */

static void
predict_paths_for_bb (basic_block cur, basic_block bb,
		      enum br_predictor pred,
		      enum prediction taken,
		      bitmap visited, struct loop *in_loop = NULL)
{
  edge e;
  edge_iterator ei;
  basic_block son;

  /* If we exited the loop or CUR is unconditional in the loop, there is
     nothing to do.  */
  if (in_loop
      && (!flow_bb_inside_loop_p (in_loop, cur)
	  || dominated_by_p (CDI_DOMINATORS, in_loop->latch, cur)))
    return;

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
      if (unlikely_executed_edge_p (e))
	continue;
      gcc_assert (bb == cur || dominated_by_p (CDI_POST_DOMINATORS, cur, bb));

      /* See if there is an edge from e->src that is not abnormal
	 and does not lead to BB and does not exit the loop.  */
      FOR_EACH_EDGE (e2, ei2, e->src->succs)
	if (e2 != e
	    && !unlikely_executed_edge_p (e2)
	    && !dominated_by_p (CDI_POST_DOMINATORS, e2->dest, bb)
	    && (!in_loop || !loop_exit_edge_p (in_loop, e2)))
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
	{
	  if (!edge_predicted_by_p (e, pred, taken))
            predict_edge_def (e, pred, taken);
	}
      else if (bitmap_set_bit (visited, e->src->index))
	predict_paths_for_bb (e->src, e->src, pred, taken, visited, in_loop);
    }
  for (son = first_dom_son (CDI_POST_DOMINATORS, cur);
       son;
       son = next_dom_son (CDI_POST_DOMINATORS, son))
    predict_paths_for_bb (son, bb, pred, taken, visited, in_loop);
}

/* Sets branch probabilities according to PREDiction and
   FLAGS.  */

static void
predict_paths_leading_to (basic_block bb, enum br_predictor pred,
			  enum prediction taken, struct loop *in_loop)
{
  predict_paths_for_bb (bb, bb, pred, taken, auto_bitmap (), in_loop);
}

/* Like predict_paths_leading_to but take edge instead of basic block.  */

static void
predict_paths_leading_to_edge (edge e, enum br_predictor pred,
			       enum prediction taken, struct loop *in_loop)
{
  bool has_nonloop_edge = false;
  edge_iterator ei;
  edge e2;

  basic_block bb = e->src;
  FOR_EACH_EDGE (e2, ei, bb->succs)
    if (e2->dest != e->src && e2->dest != e->dest
	&& !unlikely_executed_edge_p (e)
	&& !dominated_by_p (CDI_POST_DOMINATORS, e->src, e2->dest))
      {
	has_nonloop_edge = true;
	break;
      }
  if (!has_nonloop_edge)
    {
      predict_paths_for_bb (bb, bb, pred, taken, auto_bitmap (), in_loop);
    }
  else
    predict_edge_def (e, pred, taken);
}

/* This is used to carry information about basic blocks.  It is
   attached to the AUX field of the standard CFG block.  */

struct block_info
{
  /* Estimated frequency of execution of basic_block.  */
  sreal frequency;

  /* To keep queue of basic blocks to process.  */
  basic_block next;

  /* Number of predecessors we need to visit first.  */
  int npredecessors;
};

/* Similar information for edges.  */
struct edge_prob_info
{
  /* In case edge is a loopback edge, the probability edge will be reached
     in case header is.  Estimated number of iterations of the loop can be
     then computed as 1 / (1 - back_edge_prob).  */
  sreal back_edge_prob;
  /* True if the edge is a loopback edge in the natural loop.  */
  unsigned int back_edge:1;
};

#define BLOCK_INFO(B)	((block_info *) (B)->aux)
#undef EDGE_INFO
#define EDGE_INFO(E)	((edge_prob_info *) (E)->aux)

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

      bb = BASIC_BLOCK_FOR_FN (cfun, i);

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
      if (!count && bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
	bb->count = profile_count::zero ();
    }

  BLOCK_INFO (head)->frequency = 1;
  last = head;
  for (bb = head; bb; bb = nextbb)
    {
      edge_iterator ei;
      sreal cyclic_probability = 0;
      sreal frequency = 0;

      nextbb = BLOCK_INFO (bb)->next;
      BLOCK_INFO (bb)->next = NULL;

      /* Compute frequency of basic block.  */
      if (bb != head)
	{
	  if (flag_checking)
	    FOR_EACH_EDGE (e, ei, bb->preds)
	      gcc_assert (!bitmap_bit_p (tovisit, e->src->index)
			  || (e->flags & EDGE_DFS_BACK));

	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (EDGE_INFO (e)->back_edge)
	      {
		cyclic_probability += EDGE_INFO (e)->back_edge_prob;
	      }
	    else if (!(e->flags & EDGE_DFS_BACK))
	      {
		/*  frequency += (e->probability
				  * BLOCK_INFO (e->src)->frequency /
				  REG_BR_PROB_BASE);  */

		/* FIXME: Graphite is producing edges with no profile. Once
		   this is fixed, drop this.  */
		sreal tmp = e->probability.initialized_p () ?
			    e->probability.to_reg_br_prob_base () : 0;
		tmp *= BLOCK_INFO (e->src)->frequency;
		tmp *= real_inv_br_prob_base;
		frequency += tmp;
	      }

	  if (cyclic_probability == 0)
	    {
	      BLOCK_INFO (bb)->frequency = frequency;
	    }
	  else
	    {
	      if (cyclic_probability > real_almost_one)
		cyclic_probability = real_almost_one;

	      /* BLOCK_INFO (bb)->frequency = frequency
					      / (1 - cyclic_probability) */

	      cyclic_probability = sreal (1) - cyclic_probability;
	      BLOCK_INFO (bb)->frequency = frequency / cyclic_probability;
	    }
	}

      bitmap_clear_bit (tovisit, bb->index);

      e = find_edge (bb, head);
      if (e)
	{
	  /* EDGE_INFO (e)->back_edge_prob
	     = ((e->probability * BLOCK_INFO (bb)->frequency)
	     / REG_BR_PROB_BASE); */

	  /* FIXME: Graphite is producing edges with no profile. Once
	     this is fixed, drop this.  */
	  sreal tmp = e->probability.initialized_p () ?
		      e->probability.to_reg_br_prob_base () : 0;
	  tmp *= BLOCK_INFO (bb)->frequency;
	  EDGE_INFO (e)->back_edge_prob = tmp * real_inv_br_prob_base;
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

/* Estimate frequencies in loops at same nest level.  */

static void
estimate_loops_at_level (struct loop *first_loop)
{
  struct loop *loop;

  for (loop = first_loop; loop; loop = loop->next)
    {
      edge e;
      basic_block *bbs;
      unsigned i;
      auto_bitmap tovisit;

      estimate_loops_at_level (loop->inner);

      /* Find current loop back edge and mark it.  */
      e = loop_latch_edge (loop);
      EDGE_INFO (e)->back_edge = 1;

      bbs = get_loop_body (loop);
      for (i = 0; i < loop->num_nodes; i++)
	bitmap_set_bit (tovisit, bbs[i]->index);
      free (bbs);
      propagate_freq (loop->header, tovisit);
    }
}

/* Propagates frequencies through structure of loops.  */

static void
estimate_loops (void)
{
  auto_bitmap tovisit;
  basic_block bb;

  /* Start by estimating the frequencies in the loops.  */
  if (number_of_loops (cfun) > 1)
    estimate_loops_at_level (current_loops->tree_root->inner);

  /* Now propagate the frequencies through all the blocks.  */
  FOR_ALL_BB_FN (bb, cfun)
    {
      bitmap_set_bit (tovisit, bb->index);
    }
  propagate_freq (ENTRY_BLOCK_PTR_FOR_FN (cfun), tovisit);
}

/* Drop the profile for NODE to guessed, and update its frequency based on
   whether it is expected to be hot given the CALL_COUNT.  */

static void
drop_profile (struct cgraph_node *node, profile_count call_count)
{
  struct function *fn = DECL_STRUCT_FUNCTION (node->decl);
  /* In the case where this was called by another function with a
     dropped profile, call_count will be 0. Since there are no
     non-zero call counts to this function, we don't know for sure
     whether it is hot, and therefore it will be marked normal below.  */
  bool hot = maybe_hot_count_p (NULL, call_count);

  if (dump_file)
    fprintf (dump_file,
	     "Dropping 0 profile for %s. %s based on calls.\n",
	     node->dump_name (),
	     hot ? "Function is hot" : "Function is normal");
  /* We only expect to miss profiles for functions that are reached
     via non-zero call edges in cases where the function may have
     been linked from another module or library (COMDATs and extern
     templates). See the comments below for handle_missing_profiles.
     Also, only warn in cases where the missing counts exceed the
     number of training runs. In certain cases with an execv followed
     by a no-return call the profile for the no-return call is not
     dumped and there can be a mismatch.  */
  if (!DECL_COMDAT (node->decl) && !DECL_EXTERNAL (node->decl)
      && call_count > profile_info->runs)
    {
      if (flag_profile_correction)
        {
          if (dump_file)
            fprintf (dump_file,
		     "Missing counts for called function %s\n",
		     node->dump_name ());
        }
      else
	warning (0, "Missing counts for called function %s",
		 node->dump_name ());
    }

  basic_block bb;
  if (opt_for_fn (node->decl, flag_guess_branch_prob))
    {
      bool clear_zeros
	 = !ENTRY_BLOCK_PTR_FOR_FN (fn)->count.nonzero_p ();
      FOR_ALL_BB_FN (bb, fn)
	if (clear_zeros || !(bb->count == profile_count::zero ()))
	  bb->count = bb->count.guessed_local ();
      fn->cfg->count_max = fn->cfg->count_max.guessed_local ();
    }
  else
    {
      FOR_ALL_BB_FN (bb, fn)
	bb->count = profile_count::uninitialized ();
      fn->cfg->count_max = profile_count::uninitialized ();
    }

  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    e->count = gimple_bb (e->call_stmt)->count;
  for (e = node->indirect_calls; e; e = e->next_callee)
    e->count = gimple_bb (e->call_stmt)->count;
  node->count = ENTRY_BLOCK_PTR_FOR_FN (fn)->count;
  
  profile_status_for_fn (fn)
      = (flag_guess_branch_prob ? PROFILE_GUESSED : PROFILE_ABSENT);
  node->frequency
      = hot ? NODE_FREQUENCY_HOT : NODE_FREQUENCY_NORMAL;
}

/* In the case of COMDAT routines, multiple object files will contain the same
   function and the linker will select one for the binary. In that case
   all the other copies from the profile instrument binary will be missing
   profile counts. Look for cases where this happened, due to non-zero
   call counts going to 0-count functions, and drop the profile to guessed
   so that we can use the estimated probabilities and avoid optimizing only
   for size.
   
   The other case where the profile may be missing is when the routine
   is not going to be emitted to the object file, e.g. for "extern template"
   class methods. Those will be marked DECL_EXTERNAL. Emit a warning in
   all other cases of non-zero calls to 0-count functions.  */

void
handle_missing_profiles (void)
{
  struct cgraph_node *node;
  int unlikely_count_fraction = PARAM_VALUE (UNLIKELY_BB_COUNT_FRACTION);
  auto_vec<struct cgraph_node *, 64> worklist;

  /* See if 0 count function has non-0 count callers.  In this case we
     lost some profile.  Drop its function profile to PROFILE_GUESSED.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      struct cgraph_edge *e;
      profile_count call_count = profile_count::zero ();
      gcov_type max_tp_first_run = 0;
      struct function *fn = DECL_STRUCT_FUNCTION (node->decl);

      if (node->count.ipa ().nonzero_p ())
        continue;
      for (e = node->callers; e; e = e->next_caller)
	if (e->count.ipa ().initialized_p () && e->count.ipa () > 0)
	  {
            call_count = call_count + e->count.ipa ();

	    if (e->caller->tp_first_run > max_tp_first_run)
	      max_tp_first_run = e->caller->tp_first_run;
	  }

      /* If time profile is missing, let assign the maximum that comes from
	 caller functions.  */
      if (!node->tp_first_run && max_tp_first_run)
	node->tp_first_run = max_tp_first_run + 1;

      if (call_count > 0
          && fn && fn->cfg
          && (call_count.apply_scale (unlikely_count_fraction, 1)
	      >= profile_info->runs))
        {
          drop_profile (node, call_count);
          worklist.safe_push (node);
        }
    }

  /* Propagate the profile dropping to other 0-count COMDATs that are
     potentially called by COMDATs we already dropped the profile on.  */
  while (worklist.length () > 0)
    {
      struct cgraph_edge *e;

      node = worklist.pop ();
      for (e = node->callees; e; e = e->next_caller)
        {
          struct cgraph_node *callee = e->callee;
          struct function *fn = DECL_STRUCT_FUNCTION (callee->decl);

          if (!(e->count.ipa () == profile_count::zero ())
	      && callee->count.ipa ().nonzero_p ())
            continue;
          if ((DECL_COMDAT (callee->decl) || DECL_EXTERNAL (callee->decl))
	      && fn && fn->cfg
              && profile_status_for_fn (fn) == PROFILE_READ)
            {
              drop_profile (node, profile_count::zero ());
              worklist.safe_push (callee);
            }
        }
    }
}

/* Convert counts measured by profile driven feedback to frequencies.
   Return nonzero iff there was any nonzero execution count.  */

bool
update_max_bb_count (void)
{
  profile_count true_count_max = profile_count::uninitialized ();
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    true_count_max = true_count_max.max (bb->count);

  cfun->cfg->count_max = true_count_max;

  return true_count_max.ipa ().nonzero_p ();
}

/* Return true if function is likely to be expensive, so there is no point to
   optimize performance of prologue, epilogue or do inlining at the expense
   of code size growth.  THRESHOLD is the limit of number of instructions
   function can execute at average to be still considered not expensive.  */

bool
expensive_function_p (int threshold)
{
  basic_block bb;

  /* If profile was scaled in a way entry block has count 0, then the function
     is deifnitly taking a lot of time.  */
  if (!ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.nonzero_p ())
    return true;

  profile_count limit = ENTRY_BLOCK_PTR_FOR_FN
			   (cfun)->count.apply_scale (threshold, 1);
  profile_count sum = profile_count::zero ();
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;

      if (!bb->count.initialized_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "Function is considered expensive because"
		     " count of bb %i is not initialized\n", bb->index);
	  return true;
	}

      FOR_BB_INSNS (bb, insn)
	if (active_insn_p (insn))
	  {
	    sum += bb->count;
	    if (sum > limit)
	      return true;
	}
    }

  return false;
}

/* All basic blocks that are reachable only from unlikely basic blocks are
   unlikely.  */

void
propagate_unlikely_bbs_forward (void)
{
  auto_vec<basic_block, 64> worklist;
  basic_block bb;
  edge_iterator ei;
  edge e;

  if (!(ENTRY_BLOCK_PTR_FOR_FN (cfun)->count == profile_count::zero ()))
    {
      ENTRY_BLOCK_PTR_FOR_FN (cfun)->aux = (void *)(size_t) 1;
      worklist.safe_push (ENTRY_BLOCK_PTR_FOR_FN (cfun));

      while (worklist.length () > 0)
	{
	  bb = worklist.pop ();
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (!(e->count () == profile_count::zero ())
		&& !(e->dest->count == profile_count::zero ())
		&& !e->dest->aux)
	      {
		e->dest->aux = (void *)(size_t) 1;
		worklist.safe_push (e->dest);
	      }
	}
    }

  FOR_ALL_BB_FN (bb, cfun)
    {
      if (!bb->aux)
	{
	  if (!(bb->count == profile_count::zero ())
	      && (dump_file && (dump_flags & TDF_DETAILS)))
	    fprintf (dump_file,
		     "Basic block %i is marked unlikely by forward prop\n",
		     bb->index);
	  bb->count = profile_count::zero ();
	}
      else
        bb->aux = NULL;
    }
}

/* Determine basic blocks/edges that are known to be unlikely executed and set
   their counters to zero.
   This is done with first identifying obviously unlikely BBs/edges and then
   propagating in both directions.  */

static void
determine_unlikely_bbs ()
{
  basic_block bb;
  auto_vec<basic_block, 64> worklist;
  edge_iterator ei;
  edge e;

  FOR_EACH_BB_FN (bb, cfun)
    {
      if (!(bb->count == profile_count::zero ())
	  && unlikely_executed_bb_p (bb))
	{
          if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Basic block %i is locally unlikely\n",
		     bb->index);
	  bb->count = profile_count::zero ();
	}

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (!(e->probability == profile_probability::never ())
	    && unlikely_executed_edge_p (e))
	  {
            if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Edge %i->%i is locally unlikely\n",
		       bb->index, e->dest->index);
	    e->probability = profile_probability::never ();
	  }

      gcc_checking_assert (!bb->aux);
    }
  propagate_unlikely_bbs_forward ();

  auto_vec<int, 64> nsuccs;
  nsuccs.safe_grow_cleared (last_basic_block_for_fn (cfun));
  FOR_ALL_BB_FN (bb, cfun)
    if (!(bb->count == profile_count::zero ())
	&& bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
      {
	nsuccs[bb->index] = 0;
        FOR_EACH_EDGE (e, ei, bb->succs)
	  if (!(e->probability == profile_probability::never ())
	      && !(e->dest->count == profile_count::zero ()))
	    nsuccs[bb->index]++;
	if (!nsuccs[bb->index])
	  worklist.safe_push (bb);
      }
  while (worklist.length () > 0)
    {
      bb = worklist.pop ();
      if (bb->count == profile_count::zero ())
	continue;
      if (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  bool found = false;
          for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
               !gsi_end_p (gsi); gsi_next (&gsi))
	    if (stmt_can_terminate_bb_p (gsi_stmt (gsi))
		/* stmt_can_terminate_bb_p special cases noreturns because it
		   assumes that fake edges are created.  We want to know that
		   noreturn alone does not imply BB to be unlikely.  */
		|| (is_gimple_call (gsi_stmt (gsi))
		    && (gimple_call_flags (gsi_stmt (gsi)) & ECF_NORETURN)))
	      {
		found = true;
		break;
	      }
	  if (found)
	    continue;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Basic block %i is marked unlikely by backward prop\n",
		 bb->index);
      bb->count = profile_count::zero ();
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!(e->probability == profile_probability::never ()))
	  {
	    if (!(e->src->count == profile_count::zero ()))
	      {
		gcc_checking_assert (nsuccs[e->src->index] > 0);
	        nsuccs[e->src->index]--;
	        if (!nsuccs[e->src->index])
		  worklist.safe_push (e->src);
	      }
	  }
    }
  /* Finally all edges from non-0 regions to 0 are unlikely.  */
  FOR_ALL_BB_FN (bb, cfun)
    {
      if (!(bb->count == profile_count::zero ()))
	FOR_EACH_EDGE (e, ei, bb->succs)
	  if (!(e->probability == profile_probability::never ())
	      && e->dest->count == profile_count::zero ())
	     {
	       if (dump_file && (dump_flags & TDF_DETAILS))
		 fprintf (dump_file, "Edge %i->%i is unlikely because "
			  "it enters unlikely block\n",
			  bb->index, e->dest->index);
	       e->probability = profile_probability::never ();
	     }

      edge other = NULL;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->probability == profile_probability::never ())
	  ;
	else if (other)
	  {
	    other = NULL;
	    break;
	  }
	else
	  other = e;
      if (other
	  && !(other->probability == profile_probability::always ()))
	{
            if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Edge %i->%i is locally likely\n",
		       bb->index, other->dest->index);
	  other->probability = profile_probability::always ();
	}
    }
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count == profile_count::zero ())
    cgraph_node::get (current_function_decl)->count = profile_count::zero ();
}

/* Estimate and propagate basic block frequencies using the given branch
   probabilities.  If FORCE is true, the frequencies are used to estimate
   the counts even when there are already non-zero profile counts.  */

void
estimate_bb_frequencies (bool force)
{
  basic_block bb;
  sreal freq_max;

  determine_unlikely_bbs ();

  if (force || profile_status_for_fn (cfun) != PROFILE_READ
      || !update_max_bb_count ())
    {
      static int real_values_initialized = 0;

      if (!real_values_initialized)
        {
	  real_values_initialized = 1;
	  real_br_prob_base = REG_BR_PROB_BASE;
	  /* Scaling frequencies up to maximal profile count may result in
	     frequent overflows especially when inlining loops.
	     Small scalling results in unnecesary precision loss.  Stay in
	     the half of the (exponential) range.  */
	  real_bb_freq_max = (uint64_t)1 << (profile_count::n_bits / 2);
	  real_one_half = sreal (1, -1);
	  real_inv_br_prob_base = sreal (1) / real_br_prob_base;
	  real_almost_one = sreal (1) - real_inv_br_prob_base;
	}

      mark_dfs_back_edges ();

      single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun))->probability =
	 profile_probability::always ();

      /* Set up block info for each basic block.  */
      alloc_aux_for_blocks (sizeof (block_info));
      alloc_aux_for_edges (sizeof (edge_prob_info));
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
	{
	  edge e;
	  edge_iterator ei;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      /* FIXME: Graphite is producing edges with no profile. Once
		 this is fixed, drop this.  */
	      if (e->probability.initialized_p ())
	        EDGE_INFO (e)->back_edge_prob
		   = e->probability.to_reg_br_prob_base ();
	      else
		EDGE_INFO (e)->back_edge_prob = REG_BR_PROB_BASE / 2;
	      EDGE_INFO (e)->back_edge_prob *= real_inv_br_prob_base;
	    }
	}

      /* First compute frequencies locally for each loop from innermost
         to outermost to examine frequencies for back edges.  */
      estimate_loops ();

      freq_max = 0;
      FOR_EACH_BB_FN (bb, cfun)
	if (freq_max < BLOCK_INFO (bb)->frequency)
	  freq_max = BLOCK_INFO (bb)->frequency;

      freq_max = real_bb_freq_max / freq_max;
      if (freq_max < 16)
	freq_max = 16;
      profile_count ipa_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.ipa ();
      cfun->cfg->count_max = profile_count::uninitialized ();
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
	{
	  sreal tmp = BLOCK_INFO (bb)->frequency * freq_max + real_one_half;
	  profile_count count = profile_count::from_gcov_type (tmp.to_int ());	

	  /* If we have profile feedback in which this function was never
	     executed, then preserve this info.  */
	  if (!(bb->count == profile_count::zero ()))
	    bb->count = count.guessed_local ().combine_with_ipa_count (ipa_count);
          cfun->cfg->count_max = cfun->cfg->count_max.max (bb->count);
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
  struct cgraph_node *node = cgraph_node::get (current_function_decl);

  if (DECL_STATIC_CONSTRUCTOR (current_function_decl)
      || MAIN_NAME_P (DECL_NAME (current_function_decl)))
    node->only_called_at_startup = true;
  if (DECL_STATIC_DESTRUCTOR (current_function_decl))
    node->only_called_at_exit = true;

  if (profile_status_for_fn (cfun) != PROFILE_READ)
    {
      int flags = flags_from_decl_or_type (current_function_decl);
      if ((ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.ipa_p ()
	   && ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.ipa() == profile_count::zero ())
	  || lookup_attribute ("cold", DECL_ATTRIBUTES (current_function_decl))
	     != NULL)
	{
          node->frequency = NODE_FREQUENCY_UNLIKELY_EXECUTED;
	  warn_function_cold (current_function_decl);
	}
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
  warn_function_cold (current_function_decl);
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.ipa() == profile_count::zero ())
    return;
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (maybe_hot_bb_p (cfun, bb))
	{
	  node->frequency = NODE_FREQUENCY_HOT;
	  return;
	}
      if (!probably_never_executed_bb_p (cfun, bb))
	node->frequency = NODE_FREQUENCY_NORMAL;
    }
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

/* Predict branch probabilities and estimate profile of the tree CFG. */

namespace {

const pass_data pass_data_profile =
{
  GIMPLE_PASS, /* type */
  "profile_estimate", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_BRANCH_PROB, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_profile : public gimple_opt_pass
{
public:
  pass_profile (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_profile, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_guess_branch_prob; }
  virtual unsigned int execute (function *);

}; // class pass_profile

unsigned int
pass_profile::execute (function *fun)
{
  unsigned nb_loops;

  if (profile_status_for_fn (cfun) == PROFILE_GUESSED)
    return 0;

  loop_optimizer_init (LOOPS_NORMAL);
  if (dump_file && (dump_flags & TDF_DETAILS))
    flow_loops_dump (dump_file, NULL, 0);

  mark_irreducible_loops ();

  nb_loops = number_of_loops (fun);
  if (nb_loops > 1)
    scev_initialize ();

  tree_estimate_probability (false);

  if (nb_loops > 1)
    scev_finalize ();

  loop_optimizer_finalize ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    gimple_dump_cfg (dump_file, dump_flags);
 if (profile_status_for_fn (fun) == PROFILE_ABSENT)
    profile_status_for_fn (fun) = PROFILE_GUESSED;
 if (dump_file && (dump_flags & TDF_DETAILS))
   {
     struct loop *loop;
     FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
       if (loop->header->count.initialized_p ())
         fprintf (dump_file, "Loop got predicted %d to iterate %i times.\n",
       	   loop->num,
       	   (int)expected_loop_iterations_unbounded (loop));
   }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_profile (gcc::context *ctxt)
{
  return new pass_profile (ctxt);
}

/* Return true when PRED predictor should be removed after early
   tree passes.  Most of the predictors are beneficial to survive
   as early inlining can also distribute then into caller's bodies.  */

static bool
strip_predictor_early (enum br_predictor pred)
{
  switch (pred)
    {
    case PRED_TREE_EARLY_RETURN:
      return true;
    default:
      return false;
    }
}

/* Get rid of all builtin_expect calls and GIMPLE_PREDICT statements
   we no longer need.  EARLY is set to true when called from early
   optimizations.  */

unsigned int
strip_predict_hints (function *fun, bool early)
{
  basic_block bb;
  gimple *ass_stmt;
  tree var;
  bool changed = false;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator bi;
      for (bi = gsi_start_bb (bb); !gsi_end_p (bi);)
	{
	  gimple *stmt = gsi_stmt (bi);

	  if (gimple_code (stmt) == GIMPLE_PREDICT)
	    {
	      if (!early
		  || strip_predictor_early (gimple_predict_predictor (stmt)))
		{
		  gsi_remove (&bi, true);
		  changed = true;
		  continue;
		}
	    }
	  else if (is_gimple_call (stmt))
	    {
	      tree fndecl = gimple_call_fndecl (stmt);

	      if (!early
		  && ((fndecl != NULL_TREE
		       && fndecl_built_in_p (fndecl, BUILT_IN_EXPECT)
		       && gimple_call_num_args (stmt) == 2)
		      || (fndecl != NULL_TREE
			  && fndecl_built_in_p (fndecl,
						BUILT_IN_EXPECT_WITH_PROBABILITY)
			  && gimple_call_num_args (stmt) == 3)
		      || (gimple_call_internal_p (stmt)
			  && gimple_call_internal_fn (stmt) == IFN_BUILTIN_EXPECT)))
		{
		  var = gimple_call_lhs (stmt);
	          changed = true;
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
  return changed ? TODO_cleanup_cfg : 0;
}

namespace {

const pass_data pass_data_strip_predict_hints =
{
  GIMPLE_PASS, /* type */
  "*strip_predict_hints", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_BRANCH_PROB, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_strip_predict_hints : public gimple_opt_pass
{
public:
  pass_strip_predict_hints (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_strip_predict_hints, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_strip_predict_hints (m_ctxt); }
  void set_pass_param (unsigned int n, bool param)
    {
      gcc_assert (n == 0);
      early_p = param;
    }

  virtual unsigned int execute (function *);

private:
  bool early_p;

}; // class pass_strip_predict_hints

unsigned int
pass_strip_predict_hints::execute (function *fun)
{
  return strip_predict_hints (fun, early_p);
}

} // anon namespace

gimple_opt_pass *
make_pass_strip_predict_hints (gcc::context *ctxt)
{
  return new pass_strip_predict_hints (ctxt);
}

/* Rebuild function frequencies.  Passes are in general expected to
   maintain profile by hand, however in some cases this is not possible:
   for example when inlining several functions with loops freuqencies might run
   out of scale and thus needs to be recomputed.  */

void
rebuild_frequencies (void)
{
  timevar_push (TV_REBUILD_FREQUENCIES);

  /* When the max bb count in the function is small, there is a higher
     chance that there were truncation errors in the integer scaling
     of counts by inlining and other optimizations. This could lead
     to incorrect classification of code as being cold when it isn't.
     In that case, force the estimation of bb counts/frequencies from the
     branch probabilities, rather than computing frequencies from counts,
     which may also lead to frequencies incorrectly reduced to 0. There
     is less precision in the probabilities, so we only do this for small
     max counts.  */
  cfun->cfg->count_max = profile_count::uninitialized ();
  basic_block bb;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    cfun->cfg->count_max = cfun->cfg->count_max.max (bb->count);

  if (profile_status_for_fn (cfun) == PROFILE_GUESSED)
    {
      loop_optimizer_init (0);
      add_noreturn_fake_exit_edges ();
      mark_irreducible_loops ();
      connect_infinite_loops_to_exit ();
      estimate_bb_frequencies (true);
      remove_fake_exit_edges ();
      loop_optimizer_finalize ();
    }
  else if (profile_status_for_fn (cfun) == PROFILE_READ)
    update_max_bb_count ();
  else if (profile_status_for_fn (cfun) == PROFILE_ABSENT
	   && !flag_guess_branch_prob)
    ;
  else
    gcc_unreachable ();
  timevar_pop (TV_REBUILD_FREQUENCIES);
}

/* Perform a dry run of the branch prediction pass and report comparsion of
   the predicted and real profile into the dump file.  */

void
report_predictor_hitrates (void)
{
  unsigned nb_loops;

  loop_optimizer_init (LOOPS_NORMAL);
  if (dump_file && (dump_flags & TDF_DETAILS))
    flow_loops_dump (dump_file, NULL, 0);

  mark_irreducible_loops ();

  nb_loops = number_of_loops (cfun);
  if (nb_loops > 1)
    scev_initialize ();

  tree_estimate_probability (true);

  if (nb_loops > 1)
    scev_finalize ();

  loop_optimizer_finalize ();
}

/* Force edge E to be cold.
   If IMPOSSIBLE is true, for edge to have count and probability 0 otherwise
   keep low probability to represent possible error in a guess.  This is used
   i.e. in case we predict loop to likely iterate given number of times but
   we are not 100% sure.

   This function locally updates profile without attempt to keep global
   consistency which cannot be reached in full generality without full profile
   rebuild from probabilities alone.  Doing so is not necessarily a good idea
   because frequencies and counts may be more realistic then probabilities.

   In some cases (such as for elimination of early exits during full loop
   unrolling) the caller can ensure that profile will get consistent
   afterwards.  */

void
force_edge_cold (edge e, bool impossible)
{
  profile_count count_sum = profile_count::zero ();
  profile_probability prob_sum = profile_probability::never ();
  edge_iterator ei;
  edge e2;
  bool uninitialized_exit = false;

  /* When branch probability guesses are not known, then do nothing.  */
  if (!impossible && !e->count ().initialized_p ())
    return;

  profile_probability goal = (impossible ? profile_probability::never ()
			      : profile_probability::very_unlikely ());

  /* If edge is already improbably or cold, just return.  */
  if (e->probability <= goal
      && (!impossible || e->count () == profile_count::zero ()))
    return;
  FOR_EACH_EDGE (e2, ei, e->src->succs)
    if (e2 != e)
      {
	if (e->flags & EDGE_FAKE)
	  continue;
	if (e2->count ().initialized_p ())
	  count_sum += e2->count ();
	if (e2->probability.initialized_p ())
	  prob_sum += e2->probability;
	else 
	  uninitialized_exit = true;
      }

  /* If we are not guessing profiles but have some other edges out,
     just assume the control flow goes elsewhere.  */
  if (uninitialized_exit)
    e->probability = goal;
  /* If there are other edges out of e->src, redistribute probabilitity
     there.  */
  else if (prob_sum > profile_probability::never ())
    {
      if (!(e->probability < goal))
	e->probability = goal;

      profile_probability prob_comp = prob_sum / e->probability.invert ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Making edge %i->%i %s by redistributing "
		 "probability to other edges.\n",
		 e->src->index, e->dest->index,
		 impossible ? "impossible" : "cold");
      FOR_EACH_EDGE (e2, ei, e->src->succs)
	if (e2 != e)
	  {
	    e2->probability /= prob_comp;
	  }
      if (current_ir_type () != IR_GIMPLE
	  && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	update_br_prob_note (e->src);
    }
  /* If all edges out of e->src are unlikely, the basic block itself
     is unlikely.  */
  else
    {
      if (prob_sum == profile_probability::never ())
        e->probability = profile_probability::always ();
      else
	{
	  if (impossible)
	    e->probability = profile_probability::never ();
	  /* If BB has some edges out that are not impossible, we cannot
	     assume that BB itself is.  */
	  impossible = false;
	}
      if (current_ir_type () != IR_GIMPLE
	  && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	update_br_prob_note (e->src);
      if (e->src->count == profile_count::zero ())
	return;
      if (count_sum == profile_count::zero () && impossible)
	{
	  bool found = false;
	  if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    ;
	  else if (current_ir_type () == IR_GIMPLE)
	    for (gimple_stmt_iterator gsi = gsi_start_bb (e->src);
	         !gsi_end_p (gsi); gsi_next (&gsi))
	      {
	        if (stmt_can_terminate_bb_p (gsi_stmt (gsi)))
		  {
		    found = true;
	            break;
		  }
	      }
	  /* FIXME: Implement RTL path.  */
	  else 
	    found = true;
	  if (!found)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "Making bb %i impossible and dropping count to 0.\n",
			 e->src->index);
	      e->src->count = profile_count::zero ();
	      FOR_EACH_EDGE (e2, ei, e->src->preds)
		force_edge_cold (e2, impossible);
	      return;
	    }
	}

      /* If we did not adjusting, the source basic block has no likely edeges
 	 leaving other direction. In that case force that bb cold, too.
	 This in general is difficult task to do, but handle special case when
	 BB has only one predecestor.  This is common case when we are updating
	 after loop transforms.  */
      if (!(prob_sum > profile_probability::never ())
	  && count_sum == profile_count::zero ()
	  && single_pred_p (e->src) && e->src->count.to_frequency (cfun)
	     > (impossible ? 0 : 1))
	{
	  int old_frequency = e->src->count.to_frequency (cfun);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Making bb %i %s.\n", e->src->index,
		     impossible ? "impossible" : "cold");
	  int new_frequency = MIN (e->src->count.to_frequency (cfun),
				   impossible ? 0 : 1);
	  if (impossible)
	    e->src->count = profile_count::zero ();
	  else
	    e->src->count = e->count ().apply_scale (new_frequency,
						     old_frequency);
	  force_edge_cold (single_pred_edge (e->src), impossible);
	}
      else if (dump_file && (dump_flags & TDF_DETAILS)
	       && maybe_hot_bb_p (cfun, e->src))
	fprintf (dump_file, "Giving up on making bb %i %s.\n", e->src->index,
		 impossible ? "impossible" : "cold");
    }
}

#if CHECKING_P

namespace selftest {

/* Test that value range of predictor values defined in predict.def is
   within range (50, 100].  */

struct branch_predictor
{
  const char *name;
  int probability;
};

#define DEF_PREDICTOR(ENUM, NAME, HITRATE, FLAGS) { NAME, HITRATE },

static void
test_prediction_value_range ()
{
  branch_predictor predictors[] = {
#include "predict.def"
    { NULL, PROB_UNINITIALIZED }
  };

  for (unsigned i = 0; predictors[i].name != NULL; i++)
    {
      if (predictors[i].probability == PROB_UNINITIALIZED)
	continue;

      unsigned p = 100 * predictors[i].probability / REG_BR_PROB_BASE;
      ASSERT_TRUE (p >= 50 && p <= 100);
    }
}

#undef DEF_PREDICTOR

/* Run all of the selfests within this file.  */

void
predict_c_tests ()
{
  test_prediction_value_range ();
}

} // namespace selftest
#endif /* CHECKING_P.  */
