/* Analysis used by inlining decision heuristics.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
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

#define INCLUDE_MEMORY
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
#include "cfganal.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "ipa-inline.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "ipa-utils.h"
#include "cfgexpand.h"
#include "gimplify.h"
#include "attribs.h"

/* Cached node/edge growths.  */
fast_call_summary<edge_growth_cache_entry *, va_heap> *edge_growth_cache = NULL;

/* The context cache remembers estimated time/size and hints for given
   ipa_call_context of a call.  */
class node_context_cache_entry
{
public:
  ipa_cached_call_context ctx;
  sreal time, nonspec_time;
  int size;
  ipa_hints hints;

  node_context_cache_entry ()
  : ctx ()
  {
  }
  ~node_context_cache_entry ()
  {
    ctx.release ();
  }
};

/* At the moment we implement primitive single entry LRU cache.  */
class node_context_summary
{
public:
  node_context_cache_entry entry;

  node_context_summary ()
  : entry ()
  {
  }
  ~node_context_summary ()
  {
  }
};

/* Summary holding the context cache.  */
static fast_function_summary <node_context_summary *, va_heap>
	*node_context_cache = NULL;
/* Statistics about the context cache effectivity.  */
static long node_context_cache_hit, node_context_cache_miss,
	    node_context_cache_clear;

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
  else if (callee->redefined_extern_inline)
    e->inline_failed = CIF_REDEFINED_EXTERN_INLINE;
  else
    e->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
  gcc_checking_assert (!e->call_stmt_cannot_inline_p
		       || cgraph_inline_failed_type (e->inline_failed)
			    == CIF_FINAL_ERROR);
}

/* Allocate edge growth caches.  */

void
initialize_growth_caches ()
{
  edge_growth_cache
    = new fast_call_summary<edge_growth_cache_entry *, va_heap> (symtab);
  node_context_cache
    = new fast_function_summary<node_context_summary *, va_heap> (symtab);
  edge_growth_cache->disable_duplication_hook ();
  node_context_cache->disable_insertion_hook ();
  node_context_cache->disable_duplication_hook ();
}

/* Free growth caches.  */

void
free_growth_caches (void)
{
  delete edge_growth_cache;
  delete node_context_cache;
  edge_growth_cache = NULL;
  node_context_cache = NULL;
  if (dump_file)
    fprintf (dump_file, "node context cache: %li hits, %li misses,"
		   	" %li initializations\n",
	     node_context_cache_hit, node_context_cache_miss,
	     node_context_cache_clear);
  node_context_cache_hit = 0;
  node_context_cache_miss = 0;
  node_context_cache_clear = 0;
}

/* Return hints derived from EDGE.   */

int
simple_edge_hints (struct cgraph_edge *edge)
{
  int hints = 0;
  struct cgraph_node *to = (edge->caller->inlined_to
			    ? edge->caller->inlined_to : edge->caller);
  struct cgraph_node *callee = edge->callee->ultimate_alias_target ();
  int to_scc_no = ipa_fn_summaries->get (to)->scc_no;
  int callee_scc_no = ipa_fn_summaries->get (callee)->scc_no;

  if (to_scc_no && to_scc_no  == callee_scc_no && !edge->recursive_p ())
    hints |= INLINE_HINT_same_scc;

  if (cross_module_call_p (edge))
    hints |= INLINE_HINT_cross_module;

  return hints;
}

/* Estimate the time cost for the caller when inlining EDGE.
   Only to be called via estimate_edge_time, that handles the
   caching mechanism.

   When caching, also update the cache entry.  Compute both time and
   size, since we always need both metrics eventually.  */

sreal
do_estimate_edge_time (struct cgraph_edge *edge, sreal *ret_nonspec_time)
{
  sreal time, nonspec_time;
  int size;
  ipa_hints hints;
  struct cgraph_node *callee;
  clause_t clause, nonspec_clause;
  ipa_auto_call_arg_values avals;
  class ipa_call_summary *es = ipa_call_summaries->get (edge);
  int min_size = -1;

  callee = edge->callee->ultimate_alias_target ();

  gcc_checking_assert (edge->inline_failed);
  evaluate_properties_for_edge (edge, true, &clause, &nonspec_clause,
				&avals, true);
  ipa_call_context ctx (callee, clause, nonspec_clause, es->param, &avals);
  if (node_context_cache != NULL)
    {
      node_context_summary *e = node_context_cache->get_create (callee);
      if (e->entry.ctx.equal_to (ctx))
	{
	  node_context_cache_hit++;
	  size = e->entry.size;
	  time = e->entry.time;
	  nonspec_time = e->entry.nonspec_time;
	  hints = e->entry.hints;
	  if (flag_checking
	      && !opt_for_fn (callee->decl, flag_profile_partial_training)
	      && !callee->count.ipa_p ())
	    {
	      ipa_call_estimates chk_estimates;
	      ctx.estimate_size_and_time (&chk_estimates);
	      gcc_assert (chk_estimates.size == size
			  && chk_estimates.time == time
		  	  && chk_estimates.nonspecialized_time == nonspec_time
			  && chk_estimates.hints == hints);
	    }
	}
      else
	{
	  if (e->entry.ctx.exists_p ())
	    node_context_cache_miss++;
	  else
	    node_context_cache_clear++;
	  e->entry.ctx.release ();
	  ipa_call_estimates estimates;
	  ctx.estimate_size_and_time (&estimates);
	  size = estimates.size;
	  e->entry.size = size;
	  time = estimates.time;
	  e->entry.time = time;
	  nonspec_time = estimates.nonspecialized_time;
	  e->entry.nonspec_time = nonspec_time;
	  hints = estimates.hints;
	  e->entry.hints = hints;
	  e->entry.ctx.duplicate_from (ctx);
	}
    }
  else
    {
      ipa_call_estimates estimates;
      ctx.estimate_size_and_time (&estimates);
      size = estimates.size;
      time = estimates.time;
      nonspec_time = estimates.nonspecialized_time;
      hints = estimates.hints;
    }

  /* When we have profile feedback or function attribute, we can quite safely
     identify hot edges and for those we disable size limits.  Don't do that
     when probability that caller will call the callee is low however, since it
     may hurt optimization of the caller's hot path.  */
  if ((edge->count.ipa ().initialized_p () && edge->maybe_hot_p ()
      && (edge->count.ipa () * 2
	  > (edge->caller->inlined_to
	     ? edge->caller->inlined_to->count.ipa ()
	     : edge->caller->count.ipa ())))
      || (lookup_attribute ("hot", DECL_ATTRIBUTES (edge->caller->decl))
	  != NULL
	 && lookup_attribute ("hot", DECL_ATTRIBUTES (edge->callee->decl))
	  != NULL))
    hints |= INLINE_HINT_known_hot;

  gcc_checking_assert (size >= 0);
  gcc_checking_assert (time >= 0);

  /* When caching, update the cache entry.  */
  if (edge_growth_cache != NULL)
    {
      if (min_size >= 0)
        ipa_fn_summaries->get (edge->callee->function_symbol ())->min_size
	   = min_size;
      edge_growth_cache_entry *entry
	= edge_growth_cache->get_create (edge);
      entry->time = time;
      entry->nonspec_time = nonspec_time;

      entry->size = size + (size >= 0);
      hints |= simple_edge_hints (edge);
      entry->hints = hints + 1;
    }
  if (ret_nonspec_time)
    *ret_nonspec_time = nonspec_time;
  return time;
}

/* Reset cache for NODE.
   This must be done each time NODE body is modified.  */
void
reset_node_cache (struct cgraph_node *node)
{
  if (node_context_cache)
    node_context_cache->remove (node);
}

/* Remove EDGE from caches once it was inlined.  */
void
ipa_remove_from_growth_caches (struct cgraph_edge *edge)
{
  if (node_context_cache)
    node_context_cache->remove (edge->callee);
  if (edge_growth_cache)
    edge_growth_cache->remove (edge);
}

/* Return estimated callee growth after inlining EDGE.
   Only to be called via estimate_edge_size.  */

int
do_estimate_edge_size (struct cgraph_edge *edge)
{
  int size;
  struct cgraph_node *callee;
  clause_t clause, nonspec_clause;

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
  ipa_auto_call_arg_values avals;
  evaluate_properties_for_edge (edge, true, &clause, &nonspec_clause,
				&avals, true);
  ipa_call_context ctx (callee, clause, nonspec_clause, vNULL, &avals);
  ipa_call_estimates estimates;
  ctx.estimate_size_and_time (&estimates, false, false);
  return estimates.size;
}


/* Estimate the growth of the caller when inlining EDGE.
   Only to be called via estimate_edge_size.  */

ipa_hints
do_estimate_edge_hints (struct cgraph_edge *edge)
{
  struct cgraph_node *callee;
  clause_t clause, nonspec_clause;

  /* When we do caching, use do_estimate_edge_time to populate the entry.  */

  if (edge_growth_cache != NULL)
    {
      do_estimate_edge_time (edge);
      ipa_hints hints = edge_growth_cache->get (edge)->hints;
      gcc_checking_assert (hints);
      return hints - 1;
    }

  callee = edge->callee->ultimate_alias_target ();

  /* Early inliner runs without caching, go ahead and do the dirty work.  */
  gcc_checking_assert (edge->inline_failed);
  ipa_auto_call_arg_values avals;
  evaluate_properties_for_edge (edge, true, &clause, &nonspec_clause,
				&avals, true);
  ipa_call_context ctx (callee, clause, nonspec_clause, vNULL, &avals);
  ipa_call_estimates estimates;
  ctx.estimate_size_and_time (&estimates, false, true);
  ipa_hints hints = estimates.hints | simple_edge_hints (edge);
  return hints;
}

/* Estimate the size of NODE after inlining EDGE which should be an
   edge to either NODE or a call inlined into NODE.  */

int
estimate_size_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  class ipa_call_summary *es = ipa_call_summaries->get (edge);
  ipa_size_summary *s = ipa_size_summaries->get (node);
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
  int cap;
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
	  if (d->cap < INT_MAX)
	    return true;
          continue;
	}

      if (e->recursive_p ())
	{
	  d->self_recursive = true;
	  if (d->cap < INT_MAX)
	    return true;
	  continue;
	}
      d->growth += estimate_edge_growth (e);
      if (d->growth > d->cap)
	return true;
    }
  return false;
}

/* Return estimated savings for eliminating offline copy of NODE by inlining
   it everywhere.  */

static int
offline_size (struct cgraph_node *node, ipa_size_summary *info)
{
  if (!DECL_EXTERNAL (node->decl))
    {
      if (node->will_be_removed_from_program_if_no_direct_calls_p ())
	return info->size;
      /* COMDAT functions are very often not shared across multiple units
         since they come from various template instantiations.
         Take this into account.  */
      else if (DECL_COMDAT (node->decl)
	       && node->can_remove_if_no_direct_calls_p ())
	{
	  int prob = opt_for_fn (node->decl, param_comdat_sharing_probability);
	  return (info->size * (100 - prob) + 50) / 100;
	}
    }
  return 0;
}

/* Estimate the growth caused by inlining NODE into all callers.  */

int
estimate_growth (struct cgraph_node *node)
{
  struct growth_data d = { node, false, false, 0, INT_MAX };
  ipa_size_summary *info = ipa_size_summaries->get (node);

  if (node->call_for_symbol_and_aliases (do_estimate_growth_1, &d, true))
    return 1;

  /* For self recursive functions the growth estimation really should be
     infinity.  We don't want to return very large values because the growth
     plays various roles in badness computation fractions.  Be sure to not
     return zero or negative growths. */
  if (d.self_recursive)
    d.growth = d.growth < info->size ? info->size : d.growth;
  else if (!d.uninlinable)
    d.growth -= offline_size (node, info);

  return d.growth;
}

/* Verify if there are fewer than MAX_CALLERS.  */

static bool
check_callers (cgraph_node *node, int *growth, int *n, int offline,
	       int min_size, struct cgraph_edge *known_edge)
{
  ipa_ref *ref;

  if (!node->can_remove_if_no_direct_calls_and_refs_p ())
    return true;

  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    {
      edge_growth_cache_entry *entry;

      if (e == known_edge)
	continue;
      if (cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
	return true;
      if (edge_growth_cache != NULL
	  && (entry = edge_growth_cache->get (e)) != NULL
	  && entry->size != 0)
	*growth += entry->size - (entry->size > 0);
      else
	{
	  class ipa_call_summary *es = ipa_call_summaries->get (e);
	  if (!es)
	    return true;
	  *growth += min_size - es->call_stmt_size;
	  if (--(*n) < 0)
	    return false;
	}
      if (*growth > offline)
	return true;
    }

  if (*n > 0)
    FOR_EACH_ALIAS (node, ref)
      if (check_callers (dyn_cast <cgraph_node *> (ref->referring), growth, n,
			 offline, min_size, known_edge))
	return true;

  return false;
}


/* Decide if growth of NODE is positive.  This is cheaper than calculating
   actual growth.  If edge growth of KNOWN_EDGE is known
   it is passed by EDGE_GROWTH.  */

bool
growth_positive_p (struct cgraph_node *node,
		   struct cgraph_edge * known_edge, int edge_growth)
{
  struct cgraph_edge *e;

  ipa_size_summary *s = ipa_size_summaries->get (node);

  /* First quickly check if NODE is removable at all.  */
  int offline = offline_size (node, s);
  if (offline <= 0 && known_edge && edge_growth > 0)
    return true;

  int min_size = ipa_fn_summaries->get (node)->min_size;
  int n = 10;

  int min_growth = known_edge ? edge_growth : 0;
  for (e = node->callers; e; e = e->next_caller)
    {
      edge_growth_cache_entry *entry;

      if (cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
	return true;
      if (e == known_edge)
	continue;
      if (edge_growth_cache != NULL
	  && (entry = edge_growth_cache->get (e)) != NULL
	  && entry->size != 0)
	min_growth += entry->size - (entry->size > 0);
      else
	{
	  class ipa_call_summary *es = ipa_call_summaries->get (e);
	  if (!es)
	    return true;
	  min_growth += min_size - es->call_stmt_size;
	  if (--n <= 0)
	    break;
	}
      if (min_growth > offline)
	return true;
    }

  ipa_ref *ref;
  if (n > 0)
    FOR_EACH_ALIAS (node, ref)
      if (check_callers (dyn_cast <cgraph_node *> (ref->referring),
			 &min_growth, &n, offline, min_size, known_edge))
	return true;

  struct growth_data d = { node, false, false, 0, offline };
  if (node->call_for_symbol_and_aliases (do_estimate_growth_1, &d, true))
    return true;
  if (d.self_recursive || d.uninlinable)
    return true;
  return (d.growth > offline);
}
