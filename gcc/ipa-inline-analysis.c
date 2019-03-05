/* Analysis used by inlining decision heuristics.
   Copyright (C) 2003-2019 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "tree-streamer.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "print-tree.h"
#include "tree-inline.h"
#include "gimple-pretty-print.h"
#include "params.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "ipa-inline.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "ipa-utils.h"
#include "cfgexpand.h"
#include "gimplify.h"

/* Cached node/edge growths.  */
call_summary<edge_growth_cache_entry *> *edge_growth_cache = NULL;

/* Give initial reasons why inlining would fail on EDGE.  This gets either
   nullified or usually overwritten by more precise reasons later.  */

void
initialize_inline_failed (struct cgraph_edge *e)
{
  struct cgraph_node *callee = e->callee;

  if (e->inline_failed && e->inline_failed != CIF_BODY_NOT_AVAILABLE
      && cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
    ;
  else if (e->indirect_unknown_callee)
    e->inline_failed = CIF_INDIRECT_UNKNOWN_CALL;
  else if (!callee->definition)
    e->inline_failed = CIF_BODY_NOT_AVAILABLE;
  else if (callee->local.redefined_extern_inline)
    e->inline_failed = CIF_REDEFINED_EXTERN_INLINE;
  else
    e->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
  gcc_checking_assert (!e->call_stmt_cannot_inline_p
		       || cgraph_inline_failed_type (e->inline_failed)
			    == CIF_FINAL_ERROR);
}


/* Free growth caches.  */

void
free_growth_caches (void)
{
  delete edge_growth_cache;
  edge_growth_cache = NULL;
}

/* Return hints derrived from EDGE.   */

int
simple_edge_hints (struct cgraph_edge *edge)
{
  int hints = 0;
  struct cgraph_node *to = (edge->caller->global.inlined_to
			    ? edge->caller->global.inlined_to : edge->caller);
  struct cgraph_node *callee = edge->callee->ultimate_alias_target ();
  int to_scc_no = ipa_fn_summaries->get (to)->scc_no;
  int callee_scc_no = ipa_fn_summaries->get (callee)->scc_no;

  if (to_scc_no && to_scc_no  == callee_scc_no && !edge->recursive_p ())
    hints |= INLINE_HINT_same_scc;

  if (callee->lto_file_data && edge->caller->lto_file_data
      && edge->caller->lto_file_data != callee->lto_file_data
      && !callee->merged_comdat && !callee->icf_merged)
    hints |= INLINE_HINT_cross_module;

  return hints;
}

/* Estimate the time cost for the caller when inlining EDGE.
   Only to be called via estimate_edge_time, that handles the
   caching mechanism.

   When caching, also update the cache entry.  Compute both time and
   size, since we always need both metrics eventually.  */

sreal
do_estimate_edge_time (struct cgraph_edge *edge)
{
  sreal time, nonspec_time;
  int size;
  ipa_hints hints;
  struct cgraph_node *callee;
  clause_t clause, nonspec_clause;
  vec<tree> known_vals;
  vec<ipa_polymorphic_call_context> known_contexts;
  vec<ipa_agg_jump_function_p> known_aggs;
  struct ipa_call_summary *es = ipa_call_summaries->get (edge);
  int min_size;

  callee = edge->callee->ultimate_alias_target ();

  gcc_checking_assert (edge->inline_failed);
  evaluate_properties_for_edge (edge, true,
				&clause, &nonspec_clause, &known_vals,
				&known_contexts, &known_aggs);
  estimate_node_size_and_time (callee, clause, nonspec_clause, known_vals,
			       known_contexts, known_aggs, &size, &min_size,
			       &time, &nonspec_time, &hints, es->param);

  /* When we have profile feedback, we can quite safely identify hot
     edges and for those we disable size limits.  Don't do that when
     probability that caller will call the callee is low however, since it
     may hurt optimization of the caller's hot path.  */
  if (edge->count.ipa ().initialized_p () && edge->maybe_hot_p ()
      && (edge->count.ipa ().apply_scale (2, 1)
          > (edge->caller->global.inlined_to
	     ? edge->caller->global.inlined_to->count.ipa ()
	     : edge->caller->count.ipa ())))
    hints |= INLINE_HINT_known_hot;

  known_vals.release ();
  known_contexts.release ();
  known_aggs.release ();
  gcc_checking_assert (size >= 0);
  gcc_checking_assert (time >= 0);

  /* When caching, update the cache entry.  */
  if (edge_growth_cache != NULL)
    {
      ipa_fn_summaries->get_create (edge->callee)->min_size = min_size;
      edge_growth_cache_entry *entry
	= edge_growth_cache->get_create (edge);
      entry->time = time;
      entry->nonspec_time = nonspec_time;

      entry->size = size + (size >= 0);
      hints |= simple_edge_hints (edge);
      entry->hints = hints + 1;
    }
  return time;
}


/* Return estimated callee growth after inlining EDGE.
   Only to be called via estimate_edge_size.  */

int
do_estimate_edge_size (struct cgraph_edge *edge)
{
  int size;
  struct cgraph_node *callee;
  clause_t clause, nonspec_clause;
  vec<tree> known_vals;
  vec<ipa_polymorphic_call_context> known_contexts;
  vec<ipa_agg_jump_function_p> known_aggs;

  /* When we do caching, use do_estimate_edge_time to populate the entry.  */

  if (edge_growth_cache != NULL)
    {
      do_estimate_edge_time (edge);
      size = edge_growth_cache->get (edge)->size;
      gcc_checking_assert (size);
      return size - (size > 0);
    }

  callee = edge->callee->ultimate_alias_target ();

  /* Early inliner runs without caching, go ahead and do the dirty work.  */
  gcc_checking_assert (edge->inline_failed);
  evaluate_properties_for_edge (edge, true,
				&clause, &nonspec_clause,
				&known_vals, &known_contexts,
				&known_aggs);
  estimate_node_size_and_time (callee, clause, nonspec_clause, known_vals,
			       known_contexts, known_aggs, &size, NULL, NULL,
			       NULL, NULL, vNULL);
  known_vals.release ();
  known_contexts.release ();
  known_aggs.release ();
  return size;
}


/* Estimate the growth of the caller when inlining EDGE.
   Only to be called via estimate_edge_size.  */

ipa_hints
do_estimate_edge_hints (struct cgraph_edge *edge)
{
  ipa_hints hints;
  struct cgraph_node *callee;
  clause_t clause, nonspec_clause;
  vec<tree> known_vals;
  vec<ipa_polymorphic_call_context> known_contexts;
  vec<ipa_agg_jump_function_p> known_aggs;

  /* When we do caching, use do_estimate_edge_time to populate the entry.  */

  if (edge_growth_cache != NULL)
    {
      do_estimate_edge_time (edge);
      hints = edge_growth_cache->get (edge)->hints;
      gcc_checking_assert (hints);
      return hints - 1;
    }

  callee = edge->callee->ultimate_alias_target ();

  /* Early inliner runs without caching, go ahead and do the dirty work.  */
  gcc_checking_assert (edge->inline_failed);
  evaluate_properties_for_edge (edge, true,
				&clause, &nonspec_clause,
				&known_vals, &known_contexts,
				&known_aggs);
  estimate_node_size_and_time (callee, clause, nonspec_clause, known_vals,
			       known_contexts, known_aggs, NULL, NULL,
			       NULL, NULL, &hints, vNULL);
  known_vals.release ();
  known_contexts.release ();
  known_aggs.release ();
  hints |= simple_edge_hints (edge);
  return hints;
}

/* Estimate the size of NODE after inlining EDGE which should be an
   edge to either NODE or a call inlined into NODE.  */

int
estimate_size_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  struct ipa_call_summary *es = ipa_call_summaries->get (edge);
  ipa_fn_summary *s = ipa_fn_summaries->get (node);
  if (!es->predicate || *es->predicate != false)
    {
      int size = s->size + estimate_edge_growth (edge);
      gcc_assert (size >= 0);
      return size;
    }
  return s->size;
}


struct growth_data
{
  struct cgraph_node *node;
  bool self_recursive;
  bool uninlinable;
  int growth;
};


/* Worker for do_estimate_growth.  Collect growth for all callers.  */

static bool
do_estimate_growth_1 (struct cgraph_node *node, void *data)
{
  struct cgraph_edge *e;
  struct growth_data *d = (struct growth_data *) data;

  for (e = node->callers; e; e = e->next_caller)
    {
      gcc_checking_assert (e->inline_failed);

      if (cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR
	  || !opt_for_fn (e->caller->decl, optimize))
	{
	  d->uninlinable = true;
          continue;
	}

      if (e->recursive_p ())
	{
	  d->self_recursive = true;
	  continue;
	}
      d->growth += estimate_edge_growth (e);
    }
  return false;
}


/* Estimate the growth caused by inlining NODE into all callees.  */

int
estimate_growth (struct cgraph_node *node)
{
  struct growth_data d = { node, false, false, 0 };
  struct ipa_fn_summary *info = ipa_fn_summaries->get (node);

  node->call_for_symbol_and_aliases (do_estimate_growth_1, &d, true);

  /* For self recursive functions the growth estimation really should be
     infinity.  We don't want to return very large values because the growth
     plays various roles in badness computation fractions.  Be sure to not
     return zero or negative growths. */
  if (d.self_recursive)
    d.growth = d.growth < info->size ? info->size : d.growth;
  else if (DECL_EXTERNAL (node->decl) || d.uninlinable)
    ;
  else
    {
      if (node->will_be_removed_from_program_if_no_direct_calls_p ())
	d.growth -= info->size;
      /* COMDAT functions are very often not shared across multiple units
         since they come from various template instantiations.
         Take this into account.  */
      else if (DECL_COMDAT (node->decl)
	       && node->can_remove_if_no_direct_calls_p ())
	d.growth -= (info->size
		     * (100 - PARAM_VALUE (PARAM_COMDAT_SHARING_PROBABILITY))
		     + 50) / 100;
    }

  return d.growth;
}

/* Verify if there are fewer than MAX_CALLERS.  */

static bool
check_callers (cgraph_node *node, int *max_callers)
{
  ipa_ref *ref;

  if (!node->can_remove_if_no_direct_calls_and_refs_p ())
    return true;

  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    {
      (*max_callers)--;
      if (!*max_callers
	  || cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
	return true;
    }

  FOR_EACH_ALIAS (node, ref)
    if (check_callers (dyn_cast <cgraph_node *> (ref->referring), max_callers))
      return true;

  return false;
}


/* Make cheap estimation if growth of NODE is likely positive knowing
   EDGE_GROWTH of one particular edge. 
   We assume that most of other edges will have similar growth
   and skip computation if there are too many callers.  */

bool
growth_likely_positive (struct cgraph_node *node,
		        int edge_growth)
{
  int max_callers;
  struct cgraph_edge *e;
  gcc_checking_assert (edge_growth > 0);

  /* First quickly check if NODE is removable at all.  */
  if (DECL_EXTERNAL (node->decl))
    return true;
  if (!node->can_remove_if_no_direct_calls_and_refs_p ()
      || node->address_taken)
    return true;

  max_callers = ipa_fn_summaries->get (node)->size * 4 / edge_growth + 2;

  for (e = node->callers; e; e = e->next_caller)
    {
      max_callers--;
      if (!max_callers
	  || cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
	return true;
    }

  ipa_ref *ref;
  FOR_EACH_ALIAS (node, ref)
    if (check_callers (dyn_cast <cgraph_node *> (ref->referring), &max_callers))
      return true;

  /* Unlike for functions called once, we play unsafe with
     COMDATs.  We can allow that since we know functions
     in consideration are small (and thus risk is small) and
     moreover grow estimates already accounts that COMDAT
     functions may or may not disappear when eliminated from
     current unit. With good probability making aggressive
     choice in all units is going to make overall program
     smaller.  */
  if (DECL_COMDAT (node->decl))
    {
      if (!node->can_remove_if_no_direct_calls_p ())
	return true;
    }
  else if (!node->will_be_removed_from_program_if_no_direct_calls_p ())
    return true;

  return estimate_growth (node) > 0;
}
