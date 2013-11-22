/* Inlining decision heuristics.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

/* Analysis used by the inliner and other passes limiting code size growth.

   We estimate for each function
     - function body size
     - average function execution time
     - inlining size benefit (that is how much of function body size
       and its call sequence is expected to disappear by inlining)
     - inlining time benefit
     - function frame size
   For each call
     - call statement size and time

   inlinie_summary datastructures store above information locally (i.e.
   parameters of the function itself) and globally (i.e. parameters of
   the function created by applying all the inline decisions already
   present in the callgraph).

   We provide accestor to the inline_summary datastructure and
   basic logic updating the parameters when inlining is performed. 

   The summaries are context sensitive.  Context means
     1) partial assignment of known constant values of operands
     2) whether function is inlined into the call or not.
   It is easy to add more variants.  To represent function size and time
   that depends on context (i.e. it is known to be optimized away when
   context is known either by inlining or from IP-CP and clonning),
   we use predicates. Predicates are logical formulas in
   conjunctive-disjunctive form consisting of clauses. Clauses are bitmaps
   specifying what conditions must be true. Conditions are simple test
   of the form described above.

   In order to make predicate (possibly) true, all of its clauses must
   be (possibly) true. To make clause (possibly) true, one of conditions
   it mentions must be (possibly) true.  There are fixed bounds on
   number of clauses and conditions and all the manipulation functions
   are conservative in positive direction. I.e. we may lose precision
   by thinking that predicate may be true even when it is not.

   estimate_edge_size and estimate_edge_growth can be used to query
   function size/time in the given context.  inline_merge_summary merges
   properties of caller and callee after inlining.

   Finally pass_inline_parameters is exported.  This is used to drive
   computation of function parameters used by the early inliner. IPA
   inlined performs analysis via its analyze_function method. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "print-tree.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "flags.h"
#include "diagnostic.h"
#include "gimple-pretty-print.h"
#include "params.h"
#include "tree-pass.h"
#include "coverage.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "ipa-prop.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "tree-streamer.h"
#include "ipa-inline.h"
#include "alloc-pool.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "ipa-utils.h"
#include "cilk.h"
#include "cfgexpand.h"

/* Estimate runtime of function can easilly run into huge numbers with many
   nested loops.  Be sure we can compute time * INLINE_SIZE_SCALE * 2 in an
   integer.  For anything larger we use gcov_type.  */
#define MAX_TIME 500000

/* Number of bits in integer, but we really want to be stable across different
   hosts.  */
#define NUM_CONDITIONS 32

enum predicate_conditions
{
  predicate_false_condition = 0,
  predicate_not_inlined_condition = 1,
  predicate_first_dynamic_condition = 2
};

/* Special condition code we use to represent test that operand is compile time
   constant.  */
#define IS_NOT_CONSTANT ERROR_MARK
/* Special condition code we use to represent test that operand is not changed
   across invocation of the function.  When operand IS_NOT_CONSTANT it is always
   CHANGED, however i.e. loop invariants can be NOT_CHANGED given percentage
   of executions even when they are not compile time constants.  */
#define CHANGED IDENTIFIER_NODE

/* Holders of ipa cgraph hooks: */
static struct cgraph_node_hook_list *function_insertion_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_2edge_hook_list *edge_duplication_hook_holder;
static struct cgraph_edge_hook_list *edge_removal_hook_holder;
static void inline_node_removal_hook (struct cgraph_node *, void *);
static void inline_node_duplication_hook (struct cgraph_node *,
					  struct cgraph_node *, void *);
static void inline_edge_removal_hook (struct cgraph_edge *, void *);
static void inline_edge_duplication_hook (struct cgraph_edge *,
					  struct cgraph_edge *, void *);

/* VECtor holding inline summaries.  
   In GGC memory because conditions might point to constant trees.  */
vec<inline_summary_t, va_gc> *inline_summary_vec;
vec<inline_edge_summary_t> inline_edge_summary_vec;

/* Cached node/edge growths.  */
vec<int> node_growth_cache;
vec<edge_growth_cache_entry> edge_growth_cache;

/* Edge predicates goes here.  */
static alloc_pool edge_predicate_pool;

/* Return true predicate (tautology).
   We represent it by empty list of clauses.  */

static inline struct predicate
true_predicate (void)
{
  struct predicate p;
  p.clause[0] = 0;
  return p;
}


/* Return predicate testing single condition number COND.  */

static inline struct predicate
single_cond_predicate (int cond)
{
  struct predicate p;
  p.clause[0] = 1 << cond;
  p.clause[1] = 0;
  return p;
}


/* Return false predicate.  First clause require false condition.  */

static inline struct predicate
false_predicate (void)
{
  return single_cond_predicate (predicate_false_condition);
}


/* Return true if P is (false).  */

static inline bool
true_predicate_p (struct predicate *p)
{
  return !p->clause[0];
}


/* Return true if P is (false).  */

static inline bool
false_predicate_p (struct predicate *p)
{
  if (p->clause[0] == (1 << predicate_false_condition))
    {
      gcc_checking_assert (!p->clause[1]
			   && p->clause[0] == 1 << predicate_false_condition);
      return true;
    }
  return false;
}


/* Return predicate that is set true when function is not inlined.  */

static inline struct predicate
not_inlined_predicate (void)
{
  return single_cond_predicate (predicate_not_inlined_condition);
}

/* Simple description of whether a memory load or a condition refers to a load
   from an aggregate and if so, how and where from in the aggregate.
   Individual fields have the same meaning like fields with the same name in
   struct condition.  */

struct agg_position_info
{
  HOST_WIDE_INT offset;
  bool agg_contents;
  bool by_ref;
};

/* Add condition to condition list CONDS.  AGGPOS describes whether the used
   oprand is loaded from an aggregate and where in the aggregate it is.  It can
   be NULL, which means this not a load from an aggregate.  */

static struct predicate
add_condition (struct inline_summary *summary, int operand_num,
	       struct agg_position_info *aggpos,
	       enum tree_code code, tree val)
{
  int i;
  struct condition *c;
  struct condition new_cond;
  HOST_WIDE_INT offset;
  bool agg_contents, by_ref;

  if (aggpos)
    {
      offset = aggpos->offset;
      agg_contents = aggpos->agg_contents;
      by_ref = aggpos->by_ref;
    }
  else
    {
      offset = 0;
      agg_contents = false;
      by_ref = false;
    }

  gcc_checking_assert (operand_num >= 0);
  for (i = 0; vec_safe_iterate (summary->conds, i, &c); i++)
    {
      if (c->operand_num == operand_num
	  && c->code == code
	  && c->val == val
	  && c->agg_contents == agg_contents
	  && (!agg_contents || (c->offset == offset && c->by_ref == by_ref)))
	return single_cond_predicate (i + predicate_first_dynamic_condition);
    }
  /* Too many conditions.  Give up and return constant true.  */
  if (i == NUM_CONDITIONS - predicate_first_dynamic_condition)
    return true_predicate ();

  new_cond.operand_num = operand_num;
  new_cond.code = code;
  new_cond.val = val;
  new_cond.agg_contents = agg_contents;
  new_cond.by_ref = by_ref;
  new_cond.offset = offset;
  vec_safe_push (summary->conds, new_cond);
  return single_cond_predicate (i + predicate_first_dynamic_condition);
}


/* Add clause CLAUSE into the predicate P.  */

static inline void
add_clause (conditions conditions, struct predicate *p, clause_t clause)
{
  int i;
  int i2;
  int insert_here = -1;
  int c1, c2;

  /* True clause.  */
  if (!clause)
    return;

  /* False clause makes the whole predicate false.  Kill the other variants.  */
  if (clause == (1 << predicate_false_condition))
    {
      p->clause[0] = (1 << predicate_false_condition);
      p->clause[1] = 0;
      return;
    }
  if (false_predicate_p (p))
    return;

  /* No one should be sily enough to add false into nontrivial clauses.  */
  gcc_checking_assert (!(clause & (1 << predicate_false_condition)));

  /* Look where to insert the clause.  At the same time prune out
     clauses of P that are implied by the new clause and thus
     redundant.  */
  for (i = 0, i2 = 0; i <= MAX_CLAUSES; i++)
    {
      p->clause[i2] = p->clause[i];

      if (!p->clause[i])
	break;

      /* If p->clause[i] implies clause, there is nothing to add.  */
      if ((p->clause[i] & clause) == p->clause[i])
	{
	  /* We had nothing to add, none of clauses should've become
	     redundant.  */
	  gcc_checking_assert (i == i2);
	  return;
	}

      if (p->clause[i] < clause && insert_here < 0)
	insert_here = i2;

      /* If clause implies p->clause[i], then p->clause[i] becomes redundant.
         Otherwise the p->clause[i] has to stay.  */
      if ((p->clause[i] & clause) != clause)
	i2++;
    }

  /* Look for clauses that are obviously true.  I.e.
     op0 == 5 || op0 != 5.  */
  for (c1 = predicate_first_dynamic_condition; c1 < NUM_CONDITIONS; c1++)
    {
      condition *cc1;
      if (!(clause & (1 << c1)))
	continue;
      cc1 = &(*conditions)[c1 - predicate_first_dynamic_condition];
      /* We have no way to represent !CHANGED and !IS_NOT_CONSTANT
         and thus there is no point for looking for them.  */
      if (cc1->code == CHANGED || cc1->code == IS_NOT_CONSTANT)
	continue;
      for (c2 = c1 + 1; c2 < NUM_CONDITIONS; c2++)
	if (clause & (1 << c2))
	  {
	    condition *cc1 =
	      &(*conditions)[c1 - predicate_first_dynamic_condition];
	    condition *cc2 =
	      &(*conditions)[c2 - predicate_first_dynamic_condition];
	    if (cc1->operand_num == cc2->operand_num
		&& cc1->val == cc2->val
		&& cc2->code != IS_NOT_CONSTANT
		&& cc2->code != CHANGED
		&& cc1->code == invert_tree_comparison
				(cc2->code,
				 HONOR_NANS (TYPE_MODE (TREE_TYPE (cc1->val)))))
	      return;
	  }
    }


  /* We run out of variants.  Be conservative in positive direction.  */
  if (i2 == MAX_CLAUSES)
    return;
  /* Keep clauses in decreasing order. This makes equivalence testing easy.  */
  p->clause[i2 + 1] = 0;
  if (insert_here >= 0)
    for (; i2 > insert_here; i2--)
      p->clause[i2] = p->clause[i2 - 1];
  else
    insert_here = i2;
  p->clause[insert_here] = clause;
}


/* Return P & P2.  */

static struct predicate
and_predicates (conditions conditions,
		struct predicate *p, struct predicate *p2)
{
  struct predicate out = *p;
  int i;

  /* Avoid busy work.  */
  if (false_predicate_p (p2) || true_predicate_p (p))
    return *p2;
  if (false_predicate_p (p) || true_predicate_p (p2))
    return *p;

  /* See how far predicates match.  */
  for (i = 0; p->clause[i] && p->clause[i] == p2->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
    }

  /* Combine the predicates rest.  */
  for (; p2->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      add_clause (conditions, &out, p2->clause[i]);
    }
  return out;
}


/* Return true if predicates are obviously equal.  */

static inline bool
predicates_equal_p (struct predicate *p, struct predicate *p2)
{
  int i;
  for (i = 0; p->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      gcc_checking_assert (p->clause[i] > p->clause[i + 1]);
      gcc_checking_assert (!p2->clause[i]
			   || p2->clause[i] > p2->clause[i + 1]);
      if (p->clause[i] != p2->clause[i])
	return false;
    }
  return !p2->clause[i];
}


/* Return P | P2.  */

static struct predicate
or_predicates (conditions conditions,
	       struct predicate *p, struct predicate *p2)
{
  struct predicate out = true_predicate ();
  int i, j;

  /* Avoid busy work.  */
  if (false_predicate_p (p2) || true_predicate_p (p))
    return *p;
  if (false_predicate_p (p) || true_predicate_p (p2))
    return *p2;
  if (predicates_equal_p (p, p2))
    return *p;

  /* OK, combine the predicates.  */
  for (i = 0; p->clause[i]; i++)
    for (j = 0; p2->clause[j]; j++)
      {
	gcc_checking_assert (i < MAX_CLAUSES && j < MAX_CLAUSES);
	add_clause (conditions, &out, p->clause[i] | p2->clause[j]);
      }
  return out;
}


/* Having partial truth assignment in POSSIBLE_TRUTHS, return false
   if predicate P is known to be false.  */

static bool
evaluate_predicate (struct predicate *p, clause_t possible_truths)
{
  int i;

  /* True remains true.  */
  if (true_predicate_p (p))
    return true;

  gcc_assert (!(possible_truths & (1 << predicate_false_condition)));

  /* See if we can find clause we can disprove.  */
  for (i = 0; p->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      if (!(p->clause[i] & possible_truths))
	return false;
    }
  return true;
}

/* Return the probability in range 0...REG_BR_PROB_BASE that the predicated
   instruction will be recomputed per invocation of the inlined call.  */

static int
predicate_probability (conditions conds,
		       struct predicate *p, clause_t possible_truths,
		       vec<inline_param_summary_t> inline_param_summary)
{
  int i;
  int combined_prob = REG_BR_PROB_BASE;

  /* True remains true.  */
  if (true_predicate_p (p))
    return REG_BR_PROB_BASE;

  if (false_predicate_p (p))
    return 0;

  gcc_assert (!(possible_truths & (1 << predicate_false_condition)));

  /* See if we can find clause we can disprove.  */
  for (i = 0; p->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      if (!(p->clause[i] & possible_truths))
	return 0;
      else
	{
	  int this_prob = 0;
	  int i2;
	  if (!inline_param_summary.exists ())
	    return REG_BR_PROB_BASE;
	  for (i2 = 0; i2 < NUM_CONDITIONS; i2++)
	    if ((p->clause[i] & possible_truths) & (1 << i2))
	      {
		if (i2 >= predicate_first_dynamic_condition)
		  {
		    condition *c =
		      &(*conds)[i2 - predicate_first_dynamic_condition];
		    if (c->code == CHANGED
			&& (c->operand_num <
			    (int) inline_param_summary.length ()))
		      {
			int iprob =
			  inline_param_summary[c->operand_num].change_prob;
			this_prob = MAX (this_prob, iprob);
		      }
		    else
		      this_prob = REG_BR_PROB_BASE;
		  }
		else
		  this_prob = REG_BR_PROB_BASE;
	      }
	  combined_prob = MIN (this_prob, combined_prob);
	  if (!combined_prob)
	    return 0;
	}
    }
  return combined_prob;
}


/* Dump conditional COND.  */

static void
dump_condition (FILE *f, conditions conditions, int cond)
{
  condition *c;
  if (cond == predicate_false_condition)
    fprintf (f, "false");
  else if (cond == predicate_not_inlined_condition)
    fprintf (f, "not inlined");
  else
    {
      c = &(*conditions)[cond - predicate_first_dynamic_condition];
      fprintf (f, "op%i", c->operand_num);
      if (c->agg_contents)
	fprintf (f, "[%soffset: " HOST_WIDE_INT_PRINT_DEC "]",
		 c->by_ref ? "ref " : "", c->offset);
      if (c->code == IS_NOT_CONSTANT)
	{
	  fprintf (f, " not constant");
	  return;
	}
      if (c->code == CHANGED)
	{
	  fprintf (f, " changed");
	  return;
	}
      fprintf (f, " %s ", op_symbol_code (c->code));
      print_generic_expr (f, c->val, 1);
    }
}


/* Dump clause CLAUSE.  */

static void
dump_clause (FILE *f, conditions conds, clause_t clause)
{
  int i;
  bool found = false;
  fprintf (f, "(");
  if (!clause)
    fprintf (f, "true");
  for (i = 0; i < NUM_CONDITIONS; i++)
    if (clause & (1 << i))
      {
	if (found)
	  fprintf (f, " || ");
	found = true;
	dump_condition (f, conds, i);
      }
  fprintf (f, ")");
}


/* Dump predicate PREDICATE.  */

static void
dump_predicate (FILE *f, conditions conds, struct predicate *pred)
{
  int i;
  if (true_predicate_p (pred))
    dump_clause (f, conds, 0);
  else
    for (i = 0; pred->clause[i]; i++)
      {
	if (i)
	  fprintf (f, " && ");
	dump_clause (f, conds, pred->clause[i]);
      }
  fprintf (f, "\n");
}


/* Dump inline hints.  */
void
dump_inline_hints (FILE *f, inline_hints hints)
{
  if (!hints)
    return;
  fprintf (f, "inline hints:");
  if (hints & INLINE_HINT_indirect_call)
    {
      hints &= ~INLINE_HINT_indirect_call;
      fprintf (f, " indirect_call");
    }
  if (hints & INLINE_HINT_loop_iterations)
    {
      hints &= ~INLINE_HINT_loop_iterations;
      fprintf (f, " loop_iterations");
    }
  if (hints & INLINE_HINT_loop_stride)
    {
      hints &= ~INLINE_HINT_loop_stride;
      fprintf (f, " loop_stride");
    }
  if (hints & INLINE_HINT_same_scc)
    {
      hints &= ~INLINE_HINT_same_scc;
      fprintf (f, " same_scc");
    }
  if (hints & INLINE_HINT_in_scc)
    {
      hints &= ~INLINE_HINT_in_scc;
      fprintf (f, " in_scc");
    }
  if (hints & INLINE_HINT_cross_module)
    {
      hints &= ~INLINE_HINT_cross_module;
      fprintf (f, " cross_module");
    }
  if (hints & INLINE_HINT_declared_inline)
    {
      hints &= ~INLINE_HINT_declared_inline;
      fprintf (f, " declared_inline");
    }
  if (hints & INLINE_HINT_array_index)
    {
      hints &= ~INLINE_HINT_array_index;
      fprintf (f, " array_index");
    }
  gcc_assert (!hints);
}


/* Record SIZE and TIME under condition PRED into the inline summary.  */

static void
account_size_time (struct inline_summary *summary, int size, int time,
		   struct predicate *pred)
{
  size_time_entry *e;
  bool found = false;
  int i;

  if (false_predicate_p (pred))
    return;

  /* We need to create initial empty unconitional clause, but otherwie
     we don't need to account empty times and sizes.  */
  if (!size && !time && summary->entry)
    return;

  /* Watch overflow that might result from insane profiles.  */
  if (time > MAX_TIME * INLINE_TIME_SCALE)
    time = MAX_TIME * INLINE_TIME_SCALE;
  gcc_assert (time >= 0);

  for (i = 0; vec_safe_iterate (summary->entry, i, &e); i++)
    if (predicates_equal_p (&e->predicate, pred))
      {
	found = true;
	break;
      }
  if (i == 256)
    {
      i = 0;
      found = true;
      e = &(*summary->entry)[0];
      gcc_assert (!e->predicate.clause[0]);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "\t\tReached limit on number of entries, "
		 "ignoring the predicate.");
    }
  if (dump_file && (dump_flags & TDF_DETAILS) && (time || size))
    {
      fprintf (dump_file,
	       "\t\tAccounting size:%3.2f, time:%3.2f on %spredicate:",
	       ((double) size) / INLINE_SIZE_SCALE,
	       ((double) time) / INLINE_TIME_SCALE, found ? "" : "new ");
      dump_predicate (dump_file, summary->conds, pred);
    }
  if (!found)
    {
      struct size_time_entry new_entry;
      new_entry.size = size;
      new_entry.time = time;
      new_entry.predicate = *pred;
      vec_safe_push (summary->entry, new_entry);
    }
  else
    {
      e->size += size;
      e->time += time;
      if (e->time > MAX_TIME * INLINE_TIME_SCALE)
	e->time = MAX_TIME * INLINE_TIME_SCALE;
    }
}

/* Set predicate for edge E.  */

static void
edge_set_predicate (struct cgraph_edge *e, struct predicate *predicate)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  if (predicate && !true_predicate_p (predicate))
    {
      if (!es->predicate)
	es->predicate = (struct predicate *) pool_alloc (edge_predicate_pool);
      *es->predicate = *predicate;
    }
  else
    {
      if (es->predicate)
	pool_free (edge_predicate_pool, es->predicate);
      es->predicate = NULL;
    }
}

/* Set predicate for hint *P.  */

static void
set_hint_predicate (struct predicate **p, struct predicate new_predicate)
{
  if (false_predicate_p (&new_predicate) || true_predicate_p (&new_predicate))
    {
      if (*p)
	pool_free (edge_predicate_pool, *p);
      *p = NULL;
    }
  else
    {
      if (!*p)
	*p = (struct predicate *) pool_alloc (edge_predicate_pool);
      **p = new_predicate;
    }
}


/* KNOWN_VALS is partial mapping of parameters of NODE to constant values.
   KNOWN_AGGS is a vector of aggreggate jump functions for each parameter.
   Return clause of possible truths. When INLINE_P is true, assume that we are
   inlining.

   ERROR_MARK means compile time invariant.  */

static clause_t
evaluate_conditions_for_known_args (struct cgraph_node *node,
				    bool inline_p,
				    vec<tree> known_vals,
				    vec<ipa_agg_jump_function_p>
				    known_aggs)
{
  clause_t clause = inline_p ? 0 : 1 << predicate_not_inlined_condition;
  struct inline_summary *info = inline_summary (node);
  int i;
  struct condition *c;

  for (i = 0; vec_safe_iterate (info->conds, i, &c); i++)
    {
      tree val;
      tree res;

      /* We allow call stmt to have fewer arguments than the callee function
         (especially for K&R style programs).  So bound check here (we assume
         known_aggs vector, if non-NULL, has the same length as
         known_vals).  */
      gcc_checking_assert (!known_aggs.exists ()
			   || (known_vals.length () == known_aggs.length ()));
      if (c->operand_num >= (int) known_vals.length ())
	{
	  clause |= 1 << (i + predicate_first_dynamic_condition);
	  continue;
	}

      if (c->agg_contents)
	{
	  struct ipa_agg_jump_function *agg;

	  if (c->code == CHANGED
	      && !c->by_ref
	      && (known_vals[c->operand_num] == error_mark_node))
	    continue;

	  if (known_aggs.exists ())
	    {
	      agg = known_aggs[c->operand_num];
	      val = ipa_find_agg_cst_for_param (agg, c->offset, c->by_ref);
	    }
	  else
	    val = NULL_TREE;
	}
      else
	{
	  val = known_vals[c->operand_num];
	  if (val == error_mark_node && c->code != CHANGED)
	    val = NULL_TREE;
	}

      if (!val)
	{
	  clause |= 1 << (i + predicate_first_dynamic_condition);
	  continue;
	}
      if (c->code == IS_NOT_CONSTANT || c->code == CHANGED)
	continue;
      res = fold_binary_to_constant (c->code, boolean_type_node, val, c->val);
      if (res && integer_zerop (res))
	continue;
      clause |= 1 << (i + predicate_first_dynamic_condition);
    }
  return clause;
}


/* Work out what conditions might be true at invocation of E.  */

static void
evaluate_properties_for_edge (struct cgraph_edge *e, bool inline_p,
			      clause_t *clause_ptr,
			      vec<tree> *known_vals_ptr,
			      vec<tree> *known_binfos_ptr,
			      vec<ipa_agg_jump_function_p> *known_aggs_ptr)
{
  struct cgraph_node *callee =
    cgraph_function_or_thunk_node (e->callee, NULL);
  struct inline_summary *info = inline_summary (callee);
  vec<tree> known_vals = vNULL;
  vec<ipa_agg_jump_function_p> known_aggs = vNULL;

  if (clause_ptr)
    *clause_ptr = inline_p ? 0 : 1 << predicate_not_inlined_condition;
  if (known_vals_ptr)
    known_vals_ptr->create (0);
  if (known_binfos_ptr)
    known_binfos_ptr->create (0);

  if (ipa_node_params_vector.exists ()
      && !e->call_stmt_cannot_inline_p
      && ((clause_ptr && info->conds) || known_vals_ptr || known_binfos_ptr))
    {
      struct ipa_node_params *parms_info;
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      struct inline_edge_summary *es = inline_edge_summary (e);
      int i, count = ipa_get_cs_argument_count (args);

      if (e->caller->global.inlined_to)
	parms_info = IPA_NODE_REF (e->caller->global.inlined_to);
      else
	parms_info = IPA_NODE_REF (e->caller);

      if (count && (info->conds || known_vals_ptr))
	known_vals.safe_grow_cleared (count);
      if (count && (info->conds || known_aggs_ptr))
	known_aggs.safe_grow_cleared (count);
      if (count && known_binfos_ptr)
	known_binfos_ptr->safe_grow_cleared (count);

      for (i = 0; i < count; i++)
	{
	  struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, i);
	  tree cst = ipa_value_from_jfunc (parms_info, jf);
	  if (cst)
	    {
	      if (known_vals.exists () && TREE_CODE (cst) != TREE_BINFO)
		known_vals[i] = cst;
	      else if (known_binfos_ptr != NULL
		       && TREE_CODE (cst) == TREE_BINFO)
		(*known_binfos_ptr)[i] = cst;
	    }
	  else if (inline_p && !es->param[i].change_prob)
	    known_vals[i] = error_mark_node;
	  /* TODO: When IPA-CP starts propagating and merging aggregate jump
	     functions, use its knowledge of the caller too, just like the
	     scalar case above.  */
	  known_aggs[i] = &jf->agg;
	}
    }

  if (clause_ptr)
    *clause_ptr = evaluate_conditions_for_known_args (callee, inline_p,
						      known_vals, known_aggs);

  if (known_vals_ptr)
    *known_vals_ptr = known_vals;
  else
    known_vals.release ();

  if (known_aggs_ptr)
    *known_aggs_ptr = known_aggs;
  else
    known_aggs.release ();
}


/* Allocate the inline summary vector or resize it to cover all cgraph nodes. */

static void
inline_summary_alloc (void)
{
  if (!node_removal_hook_holder)
    node_removal_hook_holder =
      cgraph_add_node_removal_hook (&inline_node_removal_hook, NULL);
  if (!edge_removal_hook_holder)
    edge_removal_hook_holder =
      cgraph_add_edge_removal_hook (&inline_edge_removal_hook, NULL);
  if (!node_duplication_hook_holder)
    node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&inline_node_duplication_hook, NULL);
  if (!edge_duplication_hook_holder)
    edge_duplication_hook_holder =
      cgraph_add_edge_duplication_hook (&inline_edge_duplication_hook, NULL);

  if (vec_safe_length (inline_summary_vec) <= (unsigned) cgraph_max_uid)
    vec_safe_grow_cleared (inline_summary_vec, cgraph_max_uid + 1);
  if (inline_edge_summary_vec.length () <= (unsigned) cgraph_edge_max_uid)
    inline_edge_summary_vec.safe_grow_cleared (cgraph_edge_max_uid + 1);
  if (!edge_predicate_pool)
    edge_predicate_pool = create_alloc_pool ("edge predicates",
					     sizeof (struct predicate), 10);
}

/* We are called multiple time for given function; clear
   data from previous run so they are not cumulated.  */

static void
reset_inline_edge_summary (struct cgraph_edge *e)
{
  if (e->uid < (int) inline_edge_summary_vec.length ())
    {
      struct inline_edge_summary *es = inline_edge_summary (e);

      es->call_stmt_size = es->call_stmt_time = 0;
      if (es->predicate)
	pool_free (edge_predicate_pool, es->predicate);
      es->predicate = NULL;
      es->param.release ();
    }
}

/* We are called multiple time for given function; clear
   data from previous run so they are not cumulated.  */

static void
reset_inline_summary (struct cgraph_node *node)
{
  struct inline_summary *info = inline_summary (node);
  struct cgraph_edge *e;

  info->self_size = info->self_time = 0;
  info->estimated_stack_size = 0;
  info->estimated_self_stack_size = 0;
  info->stack_frame_offset = 0;
  info->size = 0;
  info->time = 0;
  info->growth = 0;
  info->scc_no = 0;
  if (info->loop_iterations)
    {
      pool_free (edge_predicate_pool, info->loop_iterations);
      info->loop_iterations = NULL;
    }
  if (info->loop_stride)
    {
      pool_free (edge_predicate_pool, info->loop_stride);
      info->loop_stride = NULL;
    }
  if (info->array_index)
    {
      pool_free (edge_predicate_pool, info->array_index);
      info->array_index = NULL;
    }
  vec_free (info->conds);
  vec_free (info->entry);
  for (e = node->callees; e; e = e->next_callee)
    reset_inline_edge_summary (e);
  for (e = node->indirect_calls; e; e = e->next_callee)
    reset_inline_edge_summary (e);
}

/* Hook that is called by cgraph.c when a node is removed.  */

static void
inline_node_removal_hook (struct cgraph_node *node,
			  void *data ATTRIBUTE_UNUSED)
{
  struct inline_summary *info;
  if (vec_safe_length (inline_summary_vec) <= (unsigned) node->uid)
    return;
  info = inline_summary (node);
  reset_inline_summary (node);
  memset (info, 0, sizeof (inline_summary_t));
}

/* Remap predicate P of former function to be predicate of duplicated functoin.
   POSSIBLE_TRUTHS is clause of possible truths in the duplicated node,
   INFO is inline summary of the duplicated node.  */

static struct predicate
remap_predicate_after_duplication (struct predicate *p,
				   clause_t possible_truths,
				   struct inline_summary *info)
{
  struct predicate new_predicate = true_predicate ();
  int j;
  for (j = 0; p->clause[j]; j++)
    if (!(possible_truths & p->clause[j]))
      {
	new_predicate = false_predicate ();
	break;
      }
    else
      add_clause (info->conds, &new_predicate,
		  possible_truths & p->clause[j]);
  return new_predicate;
}

/* Same as remap_predicate_after_duplication but handle hint predicate *P.
   Additionally care about allocating new memory slot for updated predicate
   and set it to NULL when it becomes true or false (and thus uninteresting).
 */

static void
remap_hint_predicate_after_duplication (struct predicate **p,
					clause_t possible_truths,
					struct inline_summary *info)
{
  struct predicate new_predicate;

  if (!*p)
    return;

  new_predicate = remap_predicate_after_duplication (*p,
						     possible_truths, info);
  /* We do not want to free previous predicate; it is used by node origin.  */
  *p = NULL;
  set_hint_predicate (p, new_predicate);
}


/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
inline_node_duplication_hook (struct cgraph_node *src,
			      struct cgraph_node *dst,
			      ATTRIBUTE_UNUSED void *data)
{
  struct inline_summary *info;
  inline_summary_alloc ();
  info = inline_summary (dst);
  memcpy (info, inline_summary (src), sizeof (struct inline_summary));
  /* TODO: as an optimization, we may avoid copying conditions
     that are known to be false or true.  */
  info->conds = vec_safe_copy (info->conds);

  /* When there are any replacements in the function body, see if we can figure
     out that something was optimized out.  */
  if (ipa_node_params_vector.exists () && dst->clone.tree_map)
    {
      vec<size_time_entry, va_gc> *entry = info->entry;
      /* Use SRC parm info since it may not be copied yet.  */
      struct ipa_node_params *parms_info = IPA_NODE_REF (src);
      vec<tree> known_vals = vNULL;
      int count = ipa_get_param_count (parms_info);
      int i, j;
      clause_t possible_truths;
      struct predicate true_pred = true_predicate ();
      size_time_entry *e;
      int optimized_out_size = 0;
      bool inlined_to_p = false;
      struct cgraph_edge *edge;

      info->entry = 0;
      known_vals.safe_grow_cleared (count);
      for (i = 0; i < count; i++)
	{
	  struct ipa_replace_map *r;

	  for (j = 0; vec_safe_iterate (dst->clone.tree_map, j, &r); j++)
	    {
	      if (((!r->old_tree && r->parm_num == i)
		   || (r->old_tree && r->old_tree == ipa_get_param (parms_info, i)))
		   && r->replace_p && !r->ref_p)
		{
		  known_vals[i] = r->new_tree;
		  break;
		}
	    }
	}
      possible_truths = evaluate_conditions_for_known_args (dst, false,
							    known_vals,
							    vNULL);
      known_vals.release ();

      account_size_time (info, 0, 0, &true_pred);

      /* Remap size_time vectors.
         Simplify the predicate by prunning out alternatives that are known
         to be false.
         TODO: as on optimization, we can also eliminate conditions known
         to be true.  */
      for (i = 0; vec_safe_iterate (entry, i, &e); i++)
	{
	  struct predicate new_predicate;
	  new_predicate = remap_predicate_after_duplication (&e->predicate,
							     possible_truths,
							     info);
	  if (false_predicate_p (&new_predicate))
	    optimized_out_size += e->size;
	  else
	    account_size_time (info, e->size, e->time, &new_predicate);
	}

      /* Remap edge predicates with the same simplification as above.
         Also copy constantness arrays.   */
      for (edge = dst->callees; edge; edge = edge->next_callee)
	{
	  struct predicate new_predicate;
	  struct inline_edge_summary *es = inline_edge_summary (edge);

	  if (!edge->inline_failed)
	    inlined_to_p = true;
	  if (!es->predicate)
	    continue;
	  new_predicate = remap_predicate_after_duplication (es->predicate,
							     possible_truths,
							     info);
	  if (false_predicate_p (&new_predicate)
	      && !false_predicate_p (es->predicate))
	    {
	      optimized_out_size += es->call_stmt_size * INLINE_SIZE_SCALE;
	      edge->frequency = 0;
	    }
	  edge_set_predicate (edge, &new_predicate);
	}

      /* Remap indirect edge predicates with the same simplificaiton as above. 
         Also copy constantness arrays.   */
      for (edge = dst->indirect_calls; edge; edge = edge->next_callee)
	{
	  struct predicate new_predicate;
	  struct inline_edge_summary *es = inline_edge_summary (edge);

	  gcc_checking_assert (edge->inline_failed);
	  if (!es->predicate)
	    continue;
	  new_predicate = remap_predicate_after_duplication (es->predicate,
							     possible_truths,
							     info);
	  if (false_predicate_p (&new_predicate)
	      && !false_predicate_p (es->predicate))
	    {
	      optimized_out_size += es->call_stmt_size * INLINE_SIZE_SCALE;
	      edge->frequency = 0;
	    }
	  edge_set_predicate (edge, &new_predicate);
	}
      remap_hint_predicate_after_duplication (&info->loop_iterations,
					      possible_truths, info);
      remap_hint_predicate_after_duplication (&info->loop_stride,
					      possible_truths, info);
      remap_hint_predicate_after_duplication (&info->array_index,
					      possible_truths, info);

      /* If inliner or someone after inliner will ever start producing
         non-trivial clones, we will get trouble with lack of information
         about updating self sizes, because size vectors already contains
         sizes of the calees.  */
      gcc_assert (!inlined_to_p || !optimized_out_size);
    }
  else
    {
      info->entry = vec_safe_copy (info->entry);
      if (info->loop_iterations)
	{
	  predicate p = *info->loop_iterations;
	  info->loop_iterations = NULL;
	  set_hint_predicate (&info->loop_iterations, p);
	}
      if (info->loop_stride)
	{
	  predicate p = *info->loop_stride;
	  info->loop_stride = NULL;
	  set_hint_predicate (&info->loop_stride, p);
	}
      if (info->array_index)
	{
	  predicate p = *info->array_index;
	  info->array_index = NULL;
	  set_hint_predicate (&info->array_index, p);
	}
    }
  inline_update_overall_summary (dst);
}


/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
inline_edge_duplication_hook (struct cgraph_edge *src,
			      struct cgraph_edge *dst,
			      ATTRIBUTE_UNUSED void *data)
{
  struct inline_edge_summary *info;
  struct inline_edge_summary *srcinfo;
  inline_summary_alloc ();
  info = inline_edge_summary (dst);
  srcinfo = inline_edge_summary (src);
  memcpy (info, srcinfo, sizeof (struct inline_edge_summary));
  info->predicate = NULL;
  edge_set_predicate (dst, srcinfo->predicate);
  info->param = srcinfo->param.copy ();
}


/* Keep edge cache consistent across edge removal.  */

static void
inline_edge_removal_hook (struct cgraph_edge *edge,
			  void *data ATTRIBUTE_UNUSED)
{
  if (edge_growth_cache.exists ())
    reset_edge_growth_cache (edge);
  reset_inline_edge_summary (edge);
}


/* Initialize growth caches.  */

void
initialize_growth_caches (void)
{
  if (cgraph_edge_max_uid)
    edge_growth_cache.safe_grow_cleared (cgraph_edge_max_uid);
  if (cgraph_max_uid)
    node_growth_cache.safe_grow_cleared (cgraph_max_uid);
}


/* Free growth caches.  */

void
free_growth_caches (void)
{
  edge_growth_cache.release ();
  node_growth_cache.release ();
}


/* Dump edge summaries associated to NODE and recursively to all clones.
   Indent by INDENT.  */

static void
dump_inline_edge_summary (FILE *f, int indent, struct cgraph_node *node,
			  struct inline_summary *info)
{
  struct cgraph_edge *edge;
  for (edge = node->callees; edge; edge = edge->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (edge);
      struct cgraph_node *callee =
	cgraph_function_or_thunk_node (edge->callee, NULL);
      int i;

      fprintf (f,
	       "%*s%s/%i %s\n%*s  loop depth:%2i freq:%4i size:%2i"
	       " time: %2i callee size:%2i stack:%2i",
	       indent, "", callee->name (), callee->order,
	       !edge->inline_failed
	       ? "inlined" : cgraph_inline_failed_string (edge-> inline_failed),
	       indent, "", es->loop_depth, edge->frequency,
	       es->call_stmt_size, es->call_stmt_time,
	       (int) inline_summary (callee)->size / INLINE_SIZE_SCALE,
	       (int) inline_summary (callee)->estimated_stack_size);

      if (es->predicate)
	{
	  fprintf (f, " predicate: ");
	  dump_predicate (f, info->conds, es->predicate);
	}
      else
	fprintf (f, "\n");
      if (es->param.exists ())
	for (i = 0; i < (int) es->param.length (); i++)
	  {
	    int prob = es->param[i].change_prob;

	    if (!prob)
	      fprintf (f, "%*s op%i is compile time invariant\n",
		       indent + 2, "", i);
	    else if (prob != REG_BR_PROB_BASE)
	      fprintf (f, "%*s op%i change %f%% of time\n", indent + 2, "", i,
		       prob * 100.0 / REG_BR_PROB_BASE);
	  }
      if (!edge->inline_failed)
	{
	  fprintf (f, "%*sStack frame offset %i, callee self size %i,"
		   " callee size %i\n",
		   indent + 2, "",
		   (int) inline_summary (callee)->stack_frame_offset,
		   (int) inline_summary (callee)->estimated_self_stack_size,
		   (int) inline_summary (callee)->estimated_stack_size);
	  dump_inline_edge_summary (f, indent + 2, callee, info);
	}
    }
  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (edge);
      fprintf (f, "%*sindirect call loop depth:%2i freq:%4i size:%2i"
	       " time: %2i",
	       indent, "",
	       es->loop_depth,
	       edge->frequency, es->call_stmt_size, es->call_stmt_time);
      if (es->predicate)
	{
	  fprintf (f, "predicate: ");
	  dump_predicate (f, info->conds, es->predicate);
	}
      else
	fprintf (f, "\n");
    }
}


void
dump_inline_summary (FILE *f, struct cgraph_node *node)
{
  if (node->definition)
    {
      struct inline_summary *s = inline_summary (node);
      size_time_entry *e;
      int i;
      fprintf (f, "Inline summary for %s/%i", node->name (),
	       node->order);
      if (DECL_DISREGARD_INLINE_LIMITS (node->decl))
	fprintf (f, " always_inline");
      if (s->inlinable)
	fprintf (f, " inlinable");
      fprintf (f, "\n  self time:       %i\n", s->self_time);
      fprintf (f, "  global time:     %i\n", s->time);
      fprintf (f, "  self size:       %i\n", s->self_size);
      fprintf (f, "  global size:     %i\n", s->size);
      fprintf (f, "  self stack:      %i\n",
	       (int) s->estimated_self_stack_size);
      fprintf (f, "  global stack:    %i\n", (int) s->estimated_stack_size);
      if (s->growth)
	fprintf (f, "  estimated growth:%i\n", (int) s->growth);
      if (s->scc_no)
	fprintf (f, "  In SCC:          %i\n", (int) s->scc_no);
      for (i = 0; vec_safe_iterate (s->entry, i, &e); i++)
	{
	  fprintf (f, "    size:%f, time:%f, predicate:",
		   (double) e->size / INLINE_SIZE_SCALE,
		   (double) e->time / INLINE_TIME_SCALE);
	  dump_predicate (f, s->conds, &e->predicate);
	}
      if (s->loop_iterations)
	{
	  fprintf (f, "  loop iterations:");
	  dump_predicate (f, s->conds, s->loop_iterations);
	}
      if (s->loop_stride)
	{
	  fprintf (f, "  loop stride:");
	  dump_predicate (f, s->conds, s->loop_stride);
	}
      if (s->array_index)
	{
	  fprintf (f, "  array index:");
	  dump_predicate (f, s->conds, s->array_index);
	}
      fprintf (f, "  calls:\n");
      dump_inline_edge_summary (f, 4, node, s);
      fprintf (f, "\n");
    }
}

DEBUG_FUNCTION void
debug_inline_summary (struct cgraph_node *node)
{
  dump_inline_summary (stderr, node);
}

void
dump_inline_summaries (FILE *f)
{
  struct cgraph_node *node;

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->global.inlined_to)
      dump_inline_summary (f, node);
}

/* Give initial reasons why inlining would fail on EDGE.  This gets either
   nullified or usually overwritten by more precise reasons later.  */

void
initialize_inline_failed (struct cgraph_edge *e)
{
  struct cgraph_node *callee = e->callee;

  if (e->indirect_unknown_callee)
    e->inline_failed = CIF_INDIRECT_UNKNOWN_CALL;
  else if (!callee->definition)
    e->inline_failed = CIF_BODY_NOT_AVAILABLE;
  else if (callee->local.redefined_extern_inline)
    e->inline_failed = CIF_REDEFINED_EXTERN_INLINE;
  else if (e->call_stmt_cannot_inline_p)
    e->inline_failed = CIF_MISMATCHED_ARGUMENTS;
  else if (cfun && fn_contains_cilk_spawn_p (cfun))
    /* We can't inline if the function is spawing a function.  */
    e->inline_failed = CIF_FUNCTION_NOT_INLINABLE;
  else
    e->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
}

/* Callback of walk_aliased_vdefs.  Flags that it has been invoked to the
   boolean variable pointed to by DATA.  */

static bool
mark_modified (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef ATTRIBUTE_UNUSED,
	       void *data)
{
  bool *b = (bool *) data;
  *b = true;
  return true;
}

/* If OP refers to value of function parameter, return the corresponding
   parameter.  */

static tree
unmodified_parm_1 (gimple stmt, tree op)
{
  /* SSA_NAME referring to parm default def?  */
  if (TREE_CODE (op) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (op)
      && TREE_CODE (SSA_NAME_VAR (op)) == PARM_DECL)
    return SSA_NAME_VAR (op);
  /* Non-SSA parm reference?  */
  if (TREE_CODE (op) == PARM_DECL)
    {
      bool modified = false;

      ao_ref refd;
      ao_ref_init (&refd, op);
      walk_aliased_vdefs (&refd, gimple_vuse (stmt), mark_modified, &modified,
			  NULL);
      if (!modified)
	return op;
    }
  return NULL_TREE;
}

/* If OP refers to value of function parameter, return the corresponding
   parameter.  Also traverse chains of SSA register assignments.  */

static tree
unmodified_parm (gimple stmt, tree op)
{
  tree res = unmodified_parm_1 (stmt, op);
  if (res)
    return res;

  if (TREE_CODE (op) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (op)
      && gimple_assign_single_p (SSA_NAME_DEF_STMT (op)))
    return unmodified_parm (SSA_NAME_DEF_STMT (op),
			    gimple_assign_rhs1 (SSA_NAME_DEF_STMT (op)));
  return NULL_TREE;
}

/* If OP refers to a value of a function parameter or value loaded from an
   aggregate passed to a parameter (either by value or reference), return TRUE
   and store the number of the parameter to *INDEX_P and information whether
   and how it has been loaded from an aggregate into *AGGPOS.  INFO describes
   the function parameters, STMT is the statement in which OP is used or
   loaded.  */

static bool
unmodified_parm_or_parm_agg_item (struct ipa_node_params *info,
				  gimple stmt, tree op, int *index_p,
				  struct agg_position_info *aggpos)
{
  tree res = unmodified_parm_1 (stmt, op);

  gcc_checking_assert (aggpos);
  if (res)
    {
      *index_p = ipa_get_param_decl_index (info, res);
      if (*index_p < 0)
	return false;
      aggpos->agg_contents = false;
      aggpos->by_ref = false;
      return true;
    }

  if (TREE_CODE (op) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (op)
	  || !gimple_assign_single_p (SSA_NAME_DEF_STMT (op)))
	return false;
      stmt = SSA_NAME_DEF_STMT (op);
      op = gimple_assign_rhs1 (stmt);
      if (!REFERENCE_CLASS_P (op))
	return unmodified_parm_or_parm_agg_item (info, stmt, op, index_p,
						 aggpos);
    }

  aggpos->agg_contents = true;
  return ipa_load_from_parm_agg (info, stmt, op, index_p, &aggpos->offset,
				 &aggpos->by_ref);
}

/* See if statement might disappear after inlining.
   0 - means not eliminated
   1 - half of statements goes away
   2 - for sure it is eliminated.
   We are not terribly sophisticated, basically looking for simple abstraction
   penalty wrappers.  */

static int
eliminated_by_inlining_prob (gimple stmt)
{
  enum gimple_code code = gimple_code (stmt);
  enum tree_code rhs_code;

  if (!optimize)
    return 0;

  switch (code)
    {
    case GIMPLE_RETURN:
      return 2;
    case GIMPLE_ASSIGN:
      if (gimple_num_ops (stmt) != 2)
	return 0;

      rhs_code = gimple_assign_rhs_code (stmt);

      /* Casts of parameters, loads from parameters passed by reference
         and stores to return value or parameters are often free after
         inlining dua to SRA and further combining.
         Assume that half of statements goes away.  */
      if (rhs_code == CONVERT_EXPR
	  || rhs_code == NOP_EXPR
	  || rhs_code == VIEW_CONVERT_EXPR
	  || rhs_code == ADDR_EXPR
	  || gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS)
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  tree lhs = gimple_assign_lhs (stmt);
	  tree inner_rhs = get_base_address (rhs);
	  tree inner_lhs = get_base_address (lhs);
	  bool rhs_free = false;
	  bool lhs_free = false;

	  if (!inner_rhs)
	    inner_rhs = rhs;
	  if (!inner_lhs)
	    inner_lhs = lhs;

	  /* Reads of parameter are expected to be free.  */
	  if (unmodified_parm (stmt, inner_rhs))
	    rhs_free = true;
	  /* Match expressions of form &this->field. Those will most likely
	     combine with something upstream after inlining.  */
	  else if (TREE_CODE (inner_rhs) == ADDR_EXPR)
	    {
	      tree op = get_base_address (TREE_OPERAND (inner_rhs, 0));
	      if (TREE_CODE (op) == PARM_DECL)
		rhs_free = true;
	      else if (TREE_CODE (op) == MEM_REF
		       && unmodified_parm (stmt, TREE_OPERAND (op, 0)))
		rhs_free = true;
	    }

	  /* When parameter is not SSA register because its address is taken
	     and it is just copied into one, the statement will be completely
	     free after inlining (we will copy propagate backward).   */
	  if (rhs_free && is_gimple_reg (lhs))
	    return 2;

	  /* Reads of parameters passed by reference
	     expected to be free (i.e. optimized out after inlining).  */
	  if (TREE_CODE (inner_rhs) == MEM_REF
	      && unmodified_parm (stmt, TREE_OPERAND (inner_rhs, 0)))
	    rhs_free = true;

	  /* Copying parameter passed by reference into gimple register is
	     probably also going to copy propagate, but we can't be quite
	     sure.  */
	  if (rhs_free && is_gimple_reg (lhs))
	    lhs_free = true;

	  /* Writes to parameters, parameters passed by value and return value
	     (either dirrectly or passed via invisible reference) are free.  

	     TODO: We ought to handle testcase like
	     struct a {int a,b;};
	     struct a
	     retrurnsturct (void)
	     {
	     struct a a ={1,2};
	     return a;
	     }

	     This translate into:

	     retrurnsturct ()
	     {
	     int a$b;
	     int a$a;
	     struct a a;
	     struct a D.2739;

	     <bb 2>:
	     D.2739.a = 1;
	     D.2739.b = 2;
	     return D.2739;

	     }
	     For that we either need to copy ipa-split logic detecting writes
	     to return value.  */
	  if (TREE_CODE (inner_lhs) == PARM_DECL
	      || TREE_CODE (inner_lhs) == RESULT_DECL
	      || (TREE_CODE (inner_lhs) == MEM_REF
		  && (unmodified_parm (stmt, TREE_OPERAND (inner_lhs, 0))
		      || (TREE_CODE (TREE_OPERAND (inner_lhs, 0)) == SSA_NAME
			  && SSA_NAME_VAR (TREE_OPERAND (inner_lhs, 0))
			  && TREE_CODE (SSA_NAME_VAR (TREE_OPERAND
						      (inner_lhs,
						       0))) == RESULT_DECL))))
	    lhs_free = true;
	  if (lhs_free
	      && (is_gimple_reg (rhs) || is_gimple_min_invariant (rhs)))
	    rhs_free = true;
	  if (lhs_free && rhs_free)
	    return 1;
	}
      return 0;
    default:
      return 0;
    }
}


/* If BB ends by a conditional we can turn into predicates, attach corresponding
   predicates to the CFG edges.   */

static void
set_cond_stmt_execution_predicate (struct ipa_node_params *info,
				   struct inline_summary *summary,
				   basic_block bb)
{
  gimple last;
  tree op;
  int index;
  struct agg_position_info aggpos;
  enum tree_code code, inverted_code;
  edge e;
  edge_iterator ei;
  gimple set_stmt;
  tree op2;

  last = last_stmt (bb);
  if (!last || gimple_code (last) != GIMPLE_COND)
    return;
  if (!is_gimple_ip_invariant (gimple_cond_rhs (last)))
    return;
  op = gimple_cond_lhs (last);
  /* TODO: handle conditionals like
     var = op0 < 4;
     if (var != 0).  */
  if (unmodified_parm_or_parm_agg_item (info, last, op, &index, &aggpos))
    {
      code = gimple_cond_code (last);
      inverted_code
	= invert_tree_comparison (code,
				  HONOR_NANS (TYPE_MODE (TREE_TYPE (op))));

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  struct predicate p = add_condition (summary, index, &aggpos,
					      e->flags & EDGE_TRUE_VALUE
					      ? code : inverted_code,
					      gimple_cond_rhs (last));
	  e->aux = pool_alloc (edge_predicate_pool);
	  *(struct predicate *) e->aux = p;
	}
    }

  if (TREE_CODE (op) != SSA_NAME)
    return;
  /* Special case
     if (builtin_constant_p (op))
     constant_code
     else
     nonconstant_code.
     Here we can predicate nonconstant_code.  We can't
     really handle constant_code since we have no predicate
     for this and also the constant code is not known to be
     optimized away when inliner doen't see operand is constant.
     Other optimizers might think otherwise.  */
  if (gimple_cond_code (last) != NE_EXPR
      || !integer_zerop (gimple_cond_rhs (last)))
    return;
  set_stmt = SSA_NAME_DEF_STMT (op);
  if (!gimple_call_builtin_p (set_stmt, BUILT_IN_CONSTANT_P)
      || gimple_call_num_args (set_stmt) != 1)
    return;
  op2 = gimple_call_arg (set_stmt, 0);
  if (!unmodified_parm_or_parm_agg_item
      (info, set_stmt, op2, &index, &aggpos))
    return;
  FOR_EACH_EDGE (e, ei, bb->succs) if (e->flags & EDGE_FALSE_VALUE)
    {
      struct predicate p = add_condition (summary, index, &aggpos,
					  IS_NOT_CONSTANT, NULL_TREE);
      e->aux = pool_alloc (edge_predicate_pool);
      *(struct predicate *) e->aux = p;
    }
}


/* If BB ends by a switch we can turn into predicates, attach corresponding
   predicates to the CFG edges.   */

static void
set_switch_stmt_execution_predicate (struct ipa_node_params *info,
				     struct inline_summary *summary,
				     basic_block bb)
{
  gimple last;
  tree op;
  int index;
  struct agg_position_info aggpos;
  edge e;
  edge_iterator ei;
  size_t n;
  size_t case_idx;

  last = last_stmt (bb);
  if (!last || gimple_code (last) != GIMPLE_SWITCH)
    return;
  op = gimple_switch_index (last);
  if (!unmodified_parm_or_parm_agg_item (info, last, op, &index, &aggpos))
    return;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      e->aux = pool_alloc (edge_predicate_pool);
      *(struct predicate *) e->aux = false_predicate ();
    }
  n = gimple_switch_num_labels (last);
  for (case_idx = 0; case_idx < n; ++case_idx)
    {
      tree cl = gimple_switch_label (last, case_idx);
      tree min, max;
      struct predicate p;

      e = find_edge (bb, label_to_block (CASE_LABEL (cl)));
      min = CASE_LOW (cl);
      max = CASE_HIGH (cl);

      /* For default we might want to construct predicate that none
         of cases is met, but it is bit hard to do not having negations
         of conditionals handy.  */
      if (!min && !max)
	p = true_predicate ();
      else if (!max)
	p = add_condition (summary, index, &aggpos, EQ_EXPR, min);
      else
	{
	  struct predicate p1, p2;
	  p1 = add_condition (summary, index, &aggpos, GE_EXPR, min);
	  p2 = add_condition (summary, index, &aggpos, LE_EXPR, max);
	  p = and_predicates (summary->conds, &p1, &p2);
	}
      *(struct predicate *) e->aux
	= or_predicates (summary->conds, &p, (struct predicate *) e->aux);
    }
}


/* For each BB in NODE attach to its AUX pointer predicate under
   which it is executable.  */

static void
compute_bb_predicates (struct cgraph_node *node,
		       struct ipa_node_params *parms_info,
		       struct inline_summary *summary)
{
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  bool done = false;
  basic_block bb;

  FOR_EACH_BB_FN (bb, my_function)
    {
      set_cond_stmt_execution_predicate (parms_info, summary, bb);
      set_switch_stmt_execution_predicate (parms_info, summary, bb);
    }

  /* Entry block is always executable.  */
  ENTRY_BLOCK_PTR_FOR_FN (my_function)->aux
    = pool_alloc (edge_predicate_pool);
  *(struct predicate *) ENTRY_BLOCK_PTR_FOR_FN (my_function)->aux
    = true_predicate ();

  /* A simple dataflow propagation of predicates forward in the CFG.
     TODO: work in reverse postorder.  */
  while (!done)
    {
      done = true;
      FOR_EACH_BB_FN (bb, my_function)
	{
	  struct predicate p = false_predicate ();
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      if (e->src->aux)
		{
		  struct predicate this_bb_predicate
		    = *(struct predicate *) e->src->aux;
		  if (e->aux)
		    this_bb_predicate
		      = and_predicates (summary->conds, &this_bb_predicate,
					(struct predicate *) e->aux);
		  p = or_predicates (summary->conds, &p, &this_bb_predicate);
		  if (true_predicate_p (&p))
		    break;
		}
	    }
	  if (false_predicate_p (&p))
	    gcc_assert (!bb->aux);
	  else
	    {
	      if (!bb->aux)
		{
		  done = false;
		  bb->aux = pool_alloc (edge_predicate_pool);
		  *((struct predicate *) bb->aux) = p;
		}
	      else if (!predicates_equal_p (&p, (struct predicate *) bb->aux))
		{
		  done = false;
		  *((struct predicate *) bb->aux) = p;
		}
	    }
	}
    }
}


/* We keep info about constantness of SSA names.  */

typedef struct predicate predicate_t;
/* Return predicate specifying when the STMT might have result that is not
   a compile time constant.  */

static struct predicate
will_be_nonconstant_expr_predicate (struct ipa_node_params *info,
				    struct inline_summary *summary,
				    tree expr,
				    vec<predicate_t> nonconstant_names)
{
  tree parm;
  int index;

  while (UNARY_CLASS_P (expr))
    expr = TREE_OPERAND (expr, 0);

  parm = unmodified_parm (NULL, expr);
  if (parm && (index = ipa_get_param_decl_index (info, parm)) >= 0)
    return add_condition (summary, index, NULL, CHANGED, NULL_TREE);
  if (is_gimple_min_invariant (expr))
    return false_predicate ();
  if (TREE_CODE (expr) == SSA_NAME)
    return nonconstant_names[SSA_NAME_VERSION (expr)];
  if (BINARY_CLASS_P (expr) || COMPARISON_CLASS_P (expr))
    {
      struct predicate p1 = will_be_nonconstant_expr_predicate
	(info, summary, TREE_OPERAND (expr, 0),
	 nonconstant_names);
      struct predicate p2;
      if (true_predicate_p (&p1))
	return p1;
      p2 = will_be_nonconstant_expr_predicate (info, summary,
					       TREE_OPERAND (expr, 1),
					       nonconstant_names);
      return or_predicates (summary->conds, &p1, &p2);
    }
  else if (TREE_CODE (expr) == COND_EXPR)
    {
      struct predicate p1 = will_be_nonconstant_expr_predicate
	(info, summary, TREE_OPERAND (expr, 0),
	 nonconstant_names);
      struct predicate p2;
      if (true_predicate_p (&p1))
	return p1;
      p2 = will_be_nonconstant_expr_predicate (info, summary,
					       TREE_OPERAND (expr, 1),
					       nonconstant_names);
      if (true_predicate_p (&p2))
	return p2;
      p1 = or_predicates (summary->conds, &p1, &p2);
      p2 = will_be_nonconstant_expr_predicate (info, summary,
					       TREE_OPERAND (expr, 2),
					       nonconstant_names);
      return or_predicates (summary->conds, &p1, &p2);
    }
  else
    {
      debug_tree (expr);
      gcc_unreachable ();
    }
  return false_predicate ();
}


/* Return predicate specifying when the STMT might have result that is not
   a compile time constant.  */

static struct predicate
will_be_nonconstant_predicate (struct ipa_node_params *info,
			       struct inline_summary *summary,
			       gimple stmt,
			       vec<predicate_t> nonconstant_names)
{
  struct predicate p = true_predicate ();
  ssa_op_iter iter;
  tree use;
  struct predicate op_non_const;
  bool is_load;
  int base_index;
  struct agg_position_info aggpos;

  /* What statments might be optimized away
     when their arguments are constant
     TODO: also trivial builtins.
     builtin_constant_p is already handled later.  */
  if (gimple_code (stmt) != GIMPLE_ASSIGN
      && gimple_code (stmt) != GIMPLE_COND
      && gimple_code (stmt) != GIMPLE_SWITCH)
    return p;

  /* Stores will stay anyway.  */
  if (gimple_store_p (stmt))
    return p;

  is_load = gimple_assign_load_p (stmt);

  /* Loads can be optimized when the value is known.  */
  if (is_load)
    {
      tree op;
      gcc_assert (gimple_assign_single_p (stmt));
      op = gimple_assign_rhs1 (stmt);
      if (!unmodified_parm_or_parm_agg_item (info, stmt, op, &base_index,
					     &aggpos))
	return p;
    }
  else
    base_index = -1;

  /* See if we understand all operands before we start
     adding conditionals.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      tree parm = unmodified_parm (stmt, use);
      /* For arguments we can build a condition.  */
      if (parm && ipa_get_param_decl_index (info, parm) >= 0)
	continue;
      if (TREE_CODE (use) != SSA_NAME)
	return p;
      /* If we know when operand is constant,
	 we still can say something useful.  */
      if (!true_predicate_p (&nonconstant_names[SSA_NAME_VERSION (use)]))
	continue;
      return p;
    }

  if (is_load)
    op_non_const =
      add_condition (summary, base_index, &aggpos, CHANGED, NULL);
  else
    op_non_const = false_predicate ();
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      tree parm = unmodified_parm (stmt, use);
      int index;

      if (parm && (index = ipa_get_param_decl_index (info, parm)) >= 0)
	{
	  if (index != base_index)
	    p = add_condition (summary, index, NULL, CHANGED, NULL_TREE);
	  else
	    continue;
	}
      else
	p = nonconstant_names[SSA_NAME_VERSION (use)];
      op_non_const = or_predicates (summary->conds, &p, &op_non_const);
    }
  if (gimple_code (stmt) == GIMPLE_ASSIGN
      && TREE_CODE (gimple_assign_lhs (stmt)) == SSA_NAME)
    nonconstant_names[SSA_NAME_VERSION (gimple_assign_lhs (stmt))]
      = op_non_const;
  return op_non_const;
}

struct record_modified_bb_info
{
  bitmap bb_set;
  gimple stmt;
};

/* Callback of walk_aliased_vdefs.  Records basic blocks where the value may be
   set except for info->stmt.  */

static bool
record_modified (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef, void *data)
{
  struct record_modified_bb_info *info =
    (struct record_modified_bb_info *) data;
  if (SSA_NAME_DEF_STMT (vdef) == info->stmt)
    return false;
  bitmap_set_bit (info->bb_set,
		  SSA_NAME_IS_DEFAULT_DEF (vdef)
		  ? ENTRY_BLOCK_PTR_FOR_FN (cfun)->index
		  : gimple_bb (SSA_NAME_DEF_STMT (vdef))->index);
  return false;
}

/* Return probability (based on REG_BR_PROB_BASE) that I-th parameter of STMT
   will change since last invocation of STMT. 

   Value 0 is reserved for compile time invariants.
   For common parameters it is REG_BR_PROB_BASE.  For loop invariants it
   ought to be REG_BR_PROB_BASE / estimated_iters.  */

static int
param_change_prob (gimple stmt, int i)
{
  tree op = gimple_call_arg (stmt, i);
  basic_block bb = gimple_bb (stmt);
  tree base;

  /* Global invariants neve change.  */
  if (is_gimple_min_invariant (op))
    return 0;
  /* We would have to do non-trivial analysis to really work out what
     is the probability of value to change (i.e. when init statement
     is in a sibling loop of the call). 

     We do an conservative estimate: when call is executed N times more often
     than the statement defining value, we take the frequency 1/N.  */
  if (TREE_CODE (op) == SSA_NAME)
    {
      int init_freq;

      if (!bb->frequency)
	return REG_BR_PROB_BASE;

      if (SSA_NAME_IS_DEFAULT_DEF (op))
	init_freq = ENTRY_BLOCK_PTR_FOR_FN (cfun)->frequency;
      else
	init_freq = gimple_bb (SSA_NAME_DEF_STMT (op))->frequency;

      if (!init_freq)
	init_freq = 1;
      if (init_freq < bb->frequency)
	return MAX (GCOV_COMPUTE_SCALE (init_freq, bb->frequency), 1);
      else
	return REG_BR_PROB_BASE;
    }

  base = get_base_address (op);
  if (base)
    {
      ao_ref refd;
      int max;
      struct record_modified_bb_info info;
      bitmap_iterator bi;
      unsigned index;
      tree init = ctor_for_folding (base);

      if (init != error_mark_node)
	return 0;
      if (!bb->frequency)
	return REG_BR_PROB_BASE;
      ao_ref_init (&refd, op);
      info.stmt = stmt;
      info.bb_set = BITMAP_ALLOC (NULL);
      walk_aliased_vdefs (&refd, gimple_vuse (stmt), record_modified, &info,
			  NULL);
      if (bitmap_bit_p (info.bb_set, bb->index))
	{
	  BITMAP_FREE (info.bb_set);
	  return REG_BR_PROB_BASE;
	}

      /* Assume that every memory is initialized at entry.
         TODO: Can we easilly determine if value is always defined
         and thus we may skip entry block?  */
      if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->frequency)
	max = ENTRY_BLOCK_PTR_FOR_FN (cfun)->frequency;
      else
	max = 1;

      EXECUTE_IF_SET_IN_BITMAP (info.bb_set, 0, index, bi)
	max = MIN (max, BASIC_BLOCK (index)->frequency);

      BITMAP_FREE (info.bb_set);
      if (max < bb->frequency)
	return MAX (GCOV_COMPUTE_SCALE (max, bb->frequency), 1);
      else
	return REG_BR_PROB_BASE;
    }
  return REG_BR_PROB_BASE;
}

/* Find whether a basic block BB is the final block of a (half) diamond CFG
   sub-graph and if the predicate the condition depends on is known.  If so,
   return true and store the pointer the predicate in *P.  */

static bool
phi_result_unknown_predicate (struct ipa_node_params *info,
			      struct inline_summary *summary, basic_block bb,
			      struct predicate *p,
			      vec<predicate_t> nonconstant_names)
{
  edge e;
  edge_iterator ei;
  basic_block first_bb = NULL;
  gimple stmt;

  if (single_pred_p (bb))
    {
      *p = false_predicate ();
      return true;
    }

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (single_succ_p (e->src))
	{
	  if (!single_pred_p (e->src))
	    return false;
	  if (!first_bb)
	    first_bb = single_pred (e->src);
	  else if (single_pred (e->src) != first_bb)
	    return false;
	}
      else
	{
	  if (!first_bb)
	    first_bb = e->src;
	  else if (e->src != first_bb)
	    return false;
	}
    }

  if (!first_bb)
    return false;

  stmt = last_stmt (first_bb);
  if (!stmt
      || gimple_code (stmt) != GIMPLE_COND
      || !is_gimple_ip_invariant (gimple_cond_rhs (stmt)))
    return false;

  *p = will_be_nonconstant_expr_predicate (info, summary,
					   gimple_cond_lhs (stmt),
					   nonconstant_names);
  if (true_predicate_p (p))
    return false;
  else
    return true;
}

/* Given a PHI statement in a function described by inline properties SUMMARY
   and *P being the predicate describing whether the selected PHI argument is
   known, store a predicate for the result of the PHI statement into
   NONCONSTANT_NAMES, if possible.  */

static void
predicate_for_phi_result (struct inline_summary *summary, gimple phi,
			  struct predicate *p,
			  vec<predicate_t> nonconstant_names)
{
  unsigned i;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg (phi, i)->def;
      if (!is_gimple_min_invariant (arg))
	{
	  gcc_assert (TREE_CODE (arg) == SSA_NAME);
	  *p = or_predicates (summary->conds, p,
			      &nonconstant_names[SSA_NAME_VERSION (arg)]);
	  if (true_predicate_p (p))
	    return;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\t\tphi predicate: ");
      dump_predicate (dump_file, summary->conds, p);
    }
  nonconstant_names[SSA_NAME_VERSION (gimple_phi_result (phi))] = *p;
}

/* Return predicate specifying when array index in access OP becomes non-constant.  */

static struct predicate
array_index_predicate (struct inline_summary *info,
		       vec< predicate_t> nonconstant_names, tree op)
{
  struct predicate p = false_predicate ();
  while (handled_component_p (op))
    {
      if (TREE_CODE (op) == ARRAY_REF || TREE_CODE (op) == ARRAY_RANGE_REF)
	{
	  if (TREE_CODE (TREE_OPERAND (op, 1)) == SSA_NAME)
	    p = or_predicates (info->conds, &p,
			       &nonconstant_names[SSA_NAME_VERSION
						  (TREE_OPERAND (op, 1))]);
	}
      op = TREE_OPERAND (op, 0);
    }
  return p;
}

/* For a typical usage of __builtin_expect (a<b, 1), we
   may introduce an extra relation stmt:
   With the builtin, we have
     t1 = a <= b;
     t2 = (long int) t1;
     t3 = __builtin_expect (t2, 1);
     if (t3 != 0)
       goto ...
   Without the builtin, we have
     if (a<=b)
       goto...
   This affects the size/time estimation and may have
   an impact on the earlier inlining.
   Here find this pattern and fix it up later.  */

static gimple
find_foldable_builtin_expect (basic_block bb)
{
  gimple_stmt_iterator bsi;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple stmt = gsi_stmt (bsi);
      if (gimple_call_builtin_p (stmt, BUILT_IN_EXPECT))
        {
          tree var = gimple_call_lhs (stmt);
          tree arg = gimple_call_arg (stmt, 0);
          use_operand_p use_p;
          gimple use_stmt;
          bool match = false;
          bool done = false;

          if (!var || !arg)
            continue;
          gcc_assert (TREE_CODE (var) == SSA_NAME);

          while (TREE_CODE (arg) == SSA_NAME)
            {
              gimple stmt_tmp = SSA_NAME_DEF_STMT (arg);
              if (!is_gimple_assign (stmt_tmp))
                break;
              switch (gimple_assign_rhs_code (stmt_tmp))
                {
                  case LT_EXPR:
                  case LE_EXPR:
                  case GT_EXPR:
                  case GE_EXPR:
                  case EQ_EXPR:
                  case NE_EXPR:
                    match = true;
                    done = true;
                    break;
                  case NOP_EXPR:
                    break;
                  default:
                    done = true;
                    break;
                }
              if (done)
                break;
              arg = gimple_assign_rhs1 (stmt_tmp);
            }

          if (match && single_imm_use (var, &use_p, &use_stmt)
              && gimple_code (use_stmt) == GIMPLE_COND)
            return use_stmt;
        }
    }
  return NULL;
}

/* Compute function body size parameters for NODE.
   When EARLY is true, we compute only simple summaries without
   non-trivial predicates to drive the early inliner.  */

static void
estimate_function_body_sizes (struct cgraph_node *node, bool early)
{
  gcov_type time = 0;
  /* Estimate static overhead for function prologue/epilogue and alignment. */
  int size = 2;
  /* Benefits are scaled by probability of elimination that is in range
     <0,2>.  */
  basic_block bb;
  gimple_stmt_iterator bsi;
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  int freq;
  struct inline_summary *info = inline_summary (node);
  struct predicate bb_predicate;
  struct ipa_node_params *parms_info = NULL;
  vec<predicate_t> nonconstant_names = vNULL;
  int nblocks, n;
  int *order;
  predicate array_index = true_predicate ();
  gimple fix_builtin_expect_stmt;

  info->conds = NULL;
  info->entry = NULL;

  if (optimize && !early)
    {
      calculate_dominance_info (CDI_DOMINATORS);
      loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);

      if (ipa_node_params_vector.exists ())
	{
	  parms_info = IPA_NODE_REF (node);
	  nonconstant_names.safe_grow_cleared
	    (SSANAMES (my_function)->length ());
	}
    }

  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function body size: %s\n",
	     node->name ());

  /* When we run into maximal number of entries, we assign everything to the
     constant truth case.  Be sure to have it in list. */
  bb_predicate = true_predicate ();
  account_size_time (info, 0, 0, &bb_predicate);

  bb_predicate = not_inlined_predicate ();
  account_size_time (info, 2 * INLINE_SIZE_SCALE, 0, &bb_predicate);

  gcc_assert (my_function && my_function->cfg);
  if (parms_info)
    compute_bb_predicates (node, parms_info, info);
  gcc_assert (cfun == my_function);
  order = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  nblocks = pre_and_rev_post_order_compute (NULL, order, false);
  for (n = 0; n < nblocks; n++)
    {
      bb = BASIC_BLOCK (order[n]);
      freq = compute_call_stmt_bb_frequency (node->decl, bb);

      /* TODO: Obviously predicates can be propagated down across CFG.  */
      if (parms_info)
	{
	  if (bb->aux)
	    bb_predicate = *(struct predicate *) bb->aux;
	  else
	    bb_predicate = false_predicate ();
	}
      else
	bb_predicate = true_predicate ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n BB %i predicate:", bb->index);
	  dump_predicate (dump_file, info->conds, &bb_predicate);
	}

      if (parms_info && nonconstant_names.exists ())
	{
	  struct predicate phi_predicate;
	  bool first_phi = true;

	  for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	    {
	      if (first_phi
		  && !phi_result_unknown_predicate (parms_info, info, bb,
						    &phi_predicate,
						    nonconstant_names))
		break;
	      first_phi = false;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "  ");
		  print_gimple_stmt (dump_file, gsi_stmt (bsi), 0, 0);
		}
	      predicate_for_phi_result (info, gsi_stmt (bsi), &phi_predicate,
					nonconstant_names);
	    }
	}

      fix_builtin_expect_stmt = find_foldable_builtin_expect (bb);

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
	  int this_size = estimate_num_insns (stmt, &eni_size_weights);
	  int this_time = estimate_num_insns (stmt, &eni_time_weights);
	  int prob;
	  struct predicate will_be_nonconstant;

          /* This relation stmt should be folded after we remove
             buildin_expect call. Adjust the cost here.  */
	  if (stmt == fix_builtin_expect_stmt)
            {
              this_size--;
              this_time--;
            }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  ");
	      print_gimple_stmt (dump_file, stmt, 0, 0);
	      fprintf (dump_file, "\t\tfreq:%3.2f size:%3i time:%3i\n",
		       ((double) freq) / CGRAPH_FREQ_BASE, this_size,
		       this_time);
	    }

	  if (gimple_assign_load_p (stmt) && nonconstant_names.exists ())
	    {
	      struct predicate this_array_index;
	      this_array_index =
		array_index_predicate (info, nonconstant_names,
				       gimple_assign_rhs1 (stmt));
	      if (!false_predicate_p (&this_array_index))
		array_index =
		  and_predicates (info->conds, &array_index,
				  &this_array_index);
	    }
	  if (gimple_store_p (stmt) && nonconstant_names.exists ())
	    {
	      struct predicate this_array_index;
	      this_array_index =
		array_index_predicate (info, nonconstant_names,
				       gimple_get_lhs (stmt));
	      if (!false_predicate_p (&this_array_index))
		array_index =
		  and_predicates (info->conds, &array_index,
				  &this_array_index);
	    }


	  if (is_gimple_call (stmt))
	    {
	      struct cgraph_edge *edge = cgraph_edge (node, stmt);
	      struct inline_edge_summary *es = inline_edge_summary (edge);

	      /* Special case: results of BUILT_IN_CONSTANT_P will be always
	         resolved as constant.  We however don't want to optimize
	         out the cgraph edges.  */
	      if (nonconstant_names.exists ()
		  && gimple_call_builtin_p (stmt, BUILT_IN_CONSTANT_P)
		  && gimple_call_lhs (stmt)
		  && TREE_CODE (gimple_call_lhs (stmt)) == SSA_NAME)
		{
		  struct predicate false_p = false_predicate ();
		  nonconstant_names[SSA_NAME_VERSION (gimple_call_lhs (stmt))]
		    = false_p;
		}
	      if (ipa_node_params_vector.exists ())
		{
		  int count = gimple_call_num_args (stmt);
		  int i;

		  if (count)
		    es->param.safe_grow_cleared (count);
		  for (i = 0; i < count; i++)
		    {
		      int prob = param_change_prob (stmt, i);
		      gcc_assert (prob >= 0 && prob <= REG_BR_PROB_BASE);
		      es->param[i].change_prob = prob;
		    }
		}

	      es->call_stmt_size = this_size;
	      es->call_stmt_time = this_time;
	      es->loop_depth = bb_loop_depth (bb);
	      edge_set_predicate (edge, &bb_predicate);
	    }

	  /* TODO: When conditional jump or swithc is known to be constant, but
	     we did not translate it into the predicates, we really can account
	     just maximum of the possible paths.  */
	  if (parms_info)
	    will_be_nonconstant
	      = will_be_nonconstant_predicate (parms_info, info,
					       stmt, nonconstant_names);
	  if (this_time || this_size)
	    {
	      struct predicate p;

	      this_time *= freq;

	      prob = eliminated_by_inlining_prob (stmt);
	      if (prob == 1 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "\t\t50%% will be eliminated by inlining\n");
	      if (prob == 2 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\t\tWill be eliminated by inlining\n");

	      if (parms_info)
		p = and_predicates (info->conds, &bb_predicate,
				    &will_be_nonconstant);
	      else
		p = true_predicate ();

	      if (!false_predicate_p (&p))
		{
		  time += this_time;
		  size += this_size;
		  if (time > MAX_TIME * INLINE_TIME_SCALE)
		    time = MAX_TIME * INLINE_TIME_SCALE;
		}

	      /* We account everything but the calls.  Calls have their own
	         size/time info attached to cgraph edges.  This is necessary
	         in order to make the cost disappear after inlining.  */
	      if (!is_gimple_call (stmt))
		{
		  if (prob)
		    {
		      struct predicate ip = not_inlined_predicate ();
		      ip = and_predicates (info->conds, &ip, &p);
		      account_size_time (info, this_size * prob,
					 this_time * prob, &ip);
		    }
		  if (prob != 2)
		    account_size_time (info, this_size * (2 - prob),
				       this_time * (2 - prob), &p);
		}

	      gcc_assert (time >= 0);
	      gcc_assert (size >= 0);
	    }
	}
    }
  set_hint_predicate (&inline_summary (node)->array_index, array_index);
  time = (time + CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
  if (time > MAX_TIME)
    time = MAX_TIME;
  free (order);

  if (!early && nonconstant_names.exists ())
    {
      struct loop *loop;
      predicate loop_iterations = true_predicate ();
      predicate loop_stride = true_predicate ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	flow_loops_dump (dump_file, NULL, 0);
      scev_initialize ();
      FOR_EACH_LOOP (loop, 0)
	{
	  vec<edge> exits;
	  edge ex;
	  unsigned int j, i;
	  struct tree_niter_desc niter_desc;
	  basic_block *body = get_loop_body (loop);
	  bb_predicate = *(struct predicate *) loop->header->aux;

	  exits = get_loop_exit_edges (loop);
	  FOR_EACH_VEC_ELT (exits, j, ex)
	    if (number_of_iterations_exit (loop, ex, &niter_desc, false)
		&& !is_gimple_min_invariant (niter_desc.niter))
	    {
	      predicate will_be_nonconstant
		= will_be_nonconstant_expr_predicate (parms_info, info,
						      niter_desc.niter,
						      nonconstant_names);
	      if (!true_predicate_p (&will_be_nonconstant))
		will_be_nonconstant = and_predicates (info->conds,
						      &bb_predicate,
						      &will_be_nonconstant);
	      if (!true_predicate_p (&will_be_nonconstant)
		  && !false_predicate_p (&will_be_nonconstant))
		/* This is slightly inprecise.  We may want to represent each
		   loop with independent predicate.  */
		loop_iterations =
		  and_predicates (info->conds, &loop_iterations,
				  &will_be_nonconstant);
	    }
	  exits.release ();

	  for (i = 0; i < loop->num_nodes; i++)
	    {
	      gimple_stmt_iterator gsi;
	      bb_predicate = *(struct predicate *) body[i]->aux;
	      for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi);
		   gsi_next (&gsi))
		{
		  gimple stmt = gsi_stmt (gsi);
		  affine_iv iv;
		  ssa_op_iter iter;
		  tree use;

		  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
		  {
		    predicate will_be_nonconstant;

		    if (!simple_iv
			(loop, loop_containing_stmt (stmt), use, &iv, true)
			|| is_gimple_min_invariant (iv.step))
		      continue;
		    will_be_nonconstant
		      = will_be_nonconstant_expr_predicate (parms_info, info,
							    iv.step,
							    nonconstant_names);
		    if (!true_predicate_p (&will_be_nonconstant))
		      will_be_nonconstant
			 = and_predicates (info->conds,
					   &bb_predicate,
					   &will_be_nonconstant);
		    if (!true_predicate_p (&will_be_nonconstant)
			&& !false_predicate_p (&will_be_nonconstant))
		      /* This is slightly inprecise.  We may want to represent
			 each loop with independent predicate.  */
		      loop_stride =
			and_predicates (info->conds, &loop_stride,
					&will_be_nonconstant);
		  }
		}
	    }
	  free (body);
	}
      set_hint_predicate (&inline_summary (node)->loop_iterations,
			  loop_iterations);
      set_hint_predicate (&inline_summary (node)->loop_stride, loop_stride);
      scev_finalize ();
    }
  FOR_ALL_BB_FN (bb, my_function)
    {
      edge e;
      edge_iterator ei;

      if (bb->aux)
	pool_free (edge_predicate_pool, bb->aux);
      bb->aux = NULL;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->aux)
	    pool_free (edge_predicate_pool, e->aux);
	  e->aux = NULL;
	}
    }
  inline_summary (node)->self_time = time;
  inline_summary (node)->self_size = size;
  nonconstant_names.release ();
  if (optimize && !early)
    {
      loop_optimizer_finalize ();
      free_dominance_info (CDI_DOMINATORS);
    }
  if (dump_file)
    {
      fprintf (dump_file, "\n");
      dump_inline_summary (dump_file, node);
    }
}


/* Compute parameters of functions used by inliner.
   EARLY is true when we compute parameters for the early inliner  */

void
compute_inline_parameters (struct cgraph_node *node, bool early)
{
  HOST_WIDE_INT self_stack_size;
  struct cgraph_edge *e;
  struct inline_summary *info;

  gcc_assert (!node->global.inlined_to);

  inline_summary_alloc ();

  info = inline_summary (node);
  reset_inline_summary (node);

  /* FIXME: Thunks are inlinable, but tree-inline don't know how to do that.
     Once this happen, we will need to more curefully predict call
     statement size.  */
  if (node->thunk.thunk_p)
    {
      struct inline_edge_summary *es = inline_edge_summary (node->callees);
      struct predicate t = true_predicate ();

      info->inlinable = 0;
      node->callees->call_stmt_cannot_inline_p = true;
      node->local.can_change_signature = false;
      es->call_stmt_time = 1;
      es->call_stmt_size = 1;
      account_size_time (info, 0, 0, &t);
      return;
    }

  /* Even is_gimple_min_invariant rely on current_function_decl.  */
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));

  /* Estimate the stack size for the function if we're optimizing.  */
  self_stack_size = optimize ? estimated_stack_frame_size (node) : 0;
  info->estimated_self_stack_size = self_stack_size;
  info->estimated_stack_size = self_stack_size;
  info->stack_frame_offset = 0;

  /* Can this function be inlined at all?  */
  if (!optimize && !lookup_attribute ("always_inline",
				      DECL_ATTRIBUTES (node->decl)))
    info->inlinable = false;
  else
    info->inlinable = tree_inlinable_function_p (node->decl);

  /* Type attributes can use parameter indices to describe them.  */
  if (TYPE_ATTRIBUTES (TREE_TYPE (node->decl)))
    node->local.can_change_signature = false;
  else
    {
      /* Otherwise, inlinable functions always can change signature.  */
      if (info->inlinable)
	node->local.can_change_signature = true;
      else
	{
	  /* Functions calling builtin_apply can not change signature.  */
	  for (e = node->callees; e; e = e->next_callee)
	    {
	      tree cdecl = e->callee->decl;
	      if (DECL_BUILT_IN (cdecl)
		  && DECL_BUILT_IN_CLASS (cdecl) == BUILT_IN_NORMAL
		  && (DECL_FUNCTION_CODE (cdecl) == BUILT_IN_APPLY_ARGS
		      || DECL_FUNCTION_CODE (cdecl) == BUILT_IN_VA_START))
		break;
	    }
	  node->local.can_change_signature = !e;
	}
    }
  estimate_function_body_sizes (node, early);

  /* Inlining characteristics are maintained by the cgraph_mark_inline.  */
  info->time = info->self_time;
  info->size = info->self_size;
  info->stack_frame_offset = 0;
  info->estimated_stack_size = info->estimated_self_stack_size;
#ifdef ENABLE_CHECKING
  inline_update_overall_summary (node);
  gcc_assert (info->time == info->self_time && info->size == info->self_size);
#endif

  pop_cfun ();
}


/* Compute parameters of functions used by inliner using
   current_function_decl.  */

static unsigned int
compute_inline_parameters_for_current (void)
{
  compute_inline_parameters (cgraph_get_node (current_function_decl), true);
  return 0;
}

namespace {

const pass_data pass_data_inline_parameters =
{
  GIMPLE_PASS, /* type */
  "inline_param", /* name */
  OPTGROUP_INLINE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_INLINE_PARAMETERS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_inline_parameters : public gimple_opt_pass
{
public:
  pass_inline_parameters (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_inline_parameters, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_inline_parameters (m_ctxt); }
  unsigned int execute () {
    return compute_inline_parameters_for_current ();
  }

}; // class pass_inline_parameters

} // anon namespace

gimple_opt_pass *
make_pass_inline_parameters (gcc::context *ctxt)
{
  return new pass_inline_parameters (ctxt);
}


/* Estimate benefit devirtualizing indirect edge IE, provided KNOWN_VALS and
   KNOWN_BINFOS.  */

static bool
estimate_edge_devirt_benefit (struct cgraph_edge *ie,
			      int *size, int *time,
			      vec<tree> known_vals,
			      vec<tree> known_binfos,
			      vec<ipa_agg_jump_function_p> known_aggs)
{
  tree target;
  struct cgraph_node *callee;
  struct inline_summary *isummary;

  if (!known_vals.exists () && !known_binfos.exists ())
    return false;
  if (!flag_indirect_inlining)
    return false;

  target = ipa_get_indirect_edge_target (ie, known_vals, known_binfos,
					 known_aggs);
  if (!target)
    return false;

  /* Account for difference in cost between indirect and direct calls.  */
  *size -= (eni_size_weights.indirect_call_cost - eni_size_weights.call_cost);
  *time -= (eni_time_weights.indirect_call_cost - eni_time_weights.call_cost);
  gcc_checking_assert (*time >= 0);
  gcc_checking_assert (*size >= 0);

  callee = cgraph_get_node (target);
  if (!callee || !callee->definition)
    return false;
  isummary = inline_summary (callee);
  return isummary->inlinable;
}

/* Increase SIZE and TIME for size and time needed to handle edge E.  */

static inline void
estimate_edge_size_and_time (struct cgraph_edge *e, int *size, int *time,
			     int prob,
			     vec<tree> known_vals,
			     vec<tree> known_binfos,
			     vec<ipa_agg_jump_function_p> known_aggs,
			     inline_hints *hints)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  int call_size = es->call_stmt_size;
  int call_time = es->call_stmt_time;
  if (!e->callee
      && estimate_edge_devirt_benefit (e, &call_size, &call_time,
				       known_vals, known_binfos, known_aggs)
      && hints && cgraph_maybe_hot_edge_p (e))
    *hints |= INLINE_HINT_indirect_call;
  *size += call_size * INLINE_SIZE_SCALE;
  *time += apply_probability ((gcov_type) call_time, prob)
    * e->frequency * (INLINE_TIME_SCALE / CGRAPH_FREQ_BASE);
  if (*time > MAX_TIME * INLINE_TIME_SCALE)
    *time = MAX_TIME * INLINE_TIME_SCALE;
}



/* Increase SIZE and TIME for size and time needed to handle all calls in NODE.
   POSSIBLE_TRUTHS, KNOWN_VALS and KNOWN_BINFOS describe context of the call
   site.  */

static void
estimate_calls_size_and_time (struct cgraph_node *node, int *size, int *time,
			      inline_hints *hints,
			      clause_t possible_truths,
			      vec<tree> known_vals,
			      vec<tree> known_binfos,
			      vec<ipa_agg_jump_function_p> known_aggs)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      if (!es->predicate
	  || evaluate_predicate (es->predicate, possible_truths))
	{
	  if (e->inline_failed)
	    {
	      /* Predicates of calls shall not use NOT_CHANGED codes,
	         sowe do not need to compute probabilities.  */
	      estimate_edge_size_and_time (e, size, time, REG_BR_PROB_BASE,
					   known_vals, known_binfos,
					   known_aggs, hints);
	    }
	  else
	    estimate_calls_size_and_time (e->callee, size, time, hints,
					  possible_truths,
					  known_vals, known_binfos,
					  known_aggs);
	}
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      if (!es->predicate
	  || evaluate_predicate (es->predicate, possible_truths))
	estimate_edge_size_and_time (e, size, time, REG_BR_PROB_BASE,
				     known_vals, known_binfos, known_aggs,
				     hints);
    }
}


/* Estimate size and time needed to execute NODE assuming
   POSSIBLE_TRUTHS clause, and KNOWN_VALS and KNOWN_BINFOS information
   about NODE's arguments. */

static void
estimate_node_size_and_time (struct cgraph_node *node,
			     clause_t possible_truths,
			     vec<tree> known_vals,
			     vec<tree> known_binfos,
			     vec<ipa_agg_jump_function_p> known_aggs,
			     int *ret_size, int *ret_time,
			     inline_hints *ret_hints,
			     vec<inline_param_summary_t>
			     inline_param_summary)
{
  struct inline_summary *info = inline_summary (node);
  size_time_entry *e;
  int size = 0;
  int time = 0;
  inline_hints hints = 0;
  int i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      bool found = false;
      fprintf (dump_file, "   Estimating body: %s/%i\n"
	       "   Known to be false: ", node->name (),
	       node->order);

      for (i = predicate_not_inlined_condition;
	   i < (predicate_first_dynamic_condition
		+ (int) vec_safe_length (info->conds)); i++)
	if (!(possible_truths & (1 << i)))
	  {
	    if (found)
	      fprintf (dump_file, ", ");
	    found = true;
	    dump_condition (dump_file, info->conds, i);
	  }
    }

  for (i = 0; vec_safe_iterate (info->entry, i, &e); i++)
    if (evaluate_predicate (&e->predicate, possible_truths))
      {
	size += e->size;
	gcc_checking_assert (e->time >= 0);
	gcc_checking_assert (time >= 0);
	if (!inline_param_summary.exists ())
	  time += e->time;
	else
	  {
	    int prob = predicate_probability (info->conds,
					      &e->predicate,
					      possible_truths,
					      inline_param_summary);
	    gcc_checking_assert (prob >= 0);
	    gcc_checking_assert (prob <= REG_BR_PROB_BASE);
	    time += apply_probability ((gcov_type) e->time, prob);
	  }
	if (time > MAX_TIME * INLINE_TIME_SCALE)
	  time = MAX_TIME * INLINE_TIME_SCALE;
	gcc_checking_assert (time >= 0);

      }
  gcc_checking_assert (size >= 0);
  gcc_checking_assert (time >= 0);

  if (info->loop_iterations
      && !evaluate_predicate (info->loop_iterations, possible_truths))
    hints |= INLINE_HINT_loop_iterations;
  if (info->loop_stride
      && !evaluate_predicate (info->loop_stride, possible_truths))
    hints |= INLINE_HINT_loop_stride;
  if (info->array_index
      && !evaluate_predicate (info->array_index, possible_truths))
    hints |= INLINE_HINT_array_index;
  if (info->scc_no)
    hints |= INLINE_HINT_in_scc;
  if (DECL_DECLARED_INLINE_P (node->decl))
    hints |= INLINE_HINT_declared_inline;

  estimate_calls_size_and_time (node, &size, &time, &hints, possible_truths,
				known_vals, known_binfos, known_aggs);
  gcc_checking_assert (size >= 0);
  gcc_checking_assert (time >= 0);
  time = RDIV (time, INLINE_TIME_SCALE);
  size = RDIV (size, INLINE_SIZE_SCALE);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n   size:%i time:%i\n", (int) size, (int) time);
  if (ret_time)
    *ret_time = time;
  if (ret_size)
    *ret_size = size;
  if (ret_hints)
    *ret_hints = hints;
  return;
}


/* Estimate size and time needed to execute callee of EDGE assuming that
   parameters known to be constant at caller of EDGE are propagated.
   KNOWN_VALS and KNOWN_BINFOS are vectors of assumed known constant values
   and types for parameters.  */

void
estimate_ipcp_clone_size_and_time (struct cgraph_node *node,
				   vec<tree> known_vals,
				   vec<tree> known_binfos,
				   vec<ipa_agg_jump_function_p> known_aggs,
				   int *ret_size, int *ret_time,
				   inline_hints *hints)
{
  clause_t clause;

  clause = evaluate_conditions_for_known_args (node, false, known_vals,
					       known_aggs);
  estimate_node_size_and_time (node, clause, known_vals, known_binfos,
			       known_aggs, ret_size, ret_time, hints, vNULL);
}

/* Translate all conditions from callee representation into caller
   representation and symbolically evaluate predicate P into new predicate.

   INFO is inline_summary of function we are adding predicate into, CALLEE_INFO
   is summary of function predicate P is from. OPERAND_MAP is array giving
   callee formal IDs the caller formal IDs. POSSSIBLE_TRUTHS is clausule of all
   callee conditions that may be true in caller context.  TOPLEV_PREDICATE is
   predicate under which callee is executed.  OFFSET_MAP is an array of of
   offsets that need to be added to conditions, negative offset means that
   conditions relying on values passed by reference have to be discarded
   because they might not be preserved (and should be considered offset zero
   for other purposes).  */

static struct predicate
remap_predicate (struct inline_summary *info,
		 struct inline_summary *callee_info,
		 struct predicate *p,
		 vec<int> operand_map,
		 vec<int> offset_map,
		 clause_t possible_truths, struct predicate *toplev_predicate)
{
  int i;
  struct predicate out = true_predicate ();

  /* True predicate is easy.  */
  if (true_predicate_p (p))
    return *toplev_predicate;
  for (i = 0; p->clause[i]; i++)
    {
      clause_t clause = p->clause[i];
      int cond;
      struct predicate clause_predicate = false_predicate ();

      gcc_assert (i < MAX_CLAUSES);

      for (cond = 0; cond < NUM_CONDITIONS; cond++)
	/* Do we have condition we can't disprove?   */
	if (clause & possible_truths & (1 << cond))
	  {
	    struct predicate cond_predicate;
	    /* Work out if the condition can translate to predicate in the
	       inlined function.  */
	    if (cond >= predicate_first_dynamic_condition)
	      {
		struct condition *c;

		c = &(*callee_info->conds)[cond
					   -
					   predicate_first_dynamic_condition];
		/* See if we can remap condition operand to caller's operand.
		   Otherwise give up.  */
		if (!operand_map.exists ()
		    || (int) operand_map.length () <= c->operand_num
		    || operand_map[c->operand_num] == -1
		    /* TODO: For non-aggregate conditions, adding an offset is
		       basically an arithmetic jump function processing which
		       we should support in future.  */
		    || ((!c->agg_contents || !c->by_ref)
			&& offset_map[c->operand_num] > 0)
		    || (c->agg_contents && c->by_ref
			&& offset_map[c->operand_num] < 0))
		  cond_predicate = true_predicate ();
		else
		  {
		    struct agg_position_info ap;
		    HOST_WIDE_INT offset_delta = offset_map[c->operand_num];
		    if (offset_delta < 0)
		      {
			gcc_checking_assert (!c->agg_contents || !c->by_ref);
			offset_delta = 0;
		      }
		    gcc_assert (!c->agg_contents
				|| c->by_ref || offset_delta == 0);
		    ap.offset = c->offset + offset_delta;
		    ap.agg_contents = c->agg_contents;
		    ap.by_ref = c->by_ref;
		    cond_predicate = add_condition (info,
						    operand_map[c->operand_num],
						    &ap, c->code, c->val);
		  }
	      }
	    /* Fixed conditions remains same, construct single
	       condition predicate.  */
	    else
	      {
		cond_predicate.clause[0] = 1 << cond;
		cond_predicate.clause[1] = 0;
	      }
	    clause_predicate = or_predicates (info->conds, &clause_predicate,
					      &cond_predicate);
	  }
      out = and_predicates (info->conds, &out, &clause_predicate);
    }
  return and_predicates (info->conds, &out, toplev_predicate);
}


/* Update summary information of inline clones after inlining.
   Compute peak stack usage.  */

static void
inline_update_callee_summaries (struct cgraph_node *node, int depth)
{
  struct cgraph_edge *e;
  struct inline_summary *callee_info = inline_summary (node);
  struct inline_summary *caller_info = inline_summary (node->callers->caller);
  HOST_WIDE_INT peak;

  callee_info->stack_frame_offset
    = caller_info->stack_frame_offset
    + caller_info->estimated_self_stack_size;
  peak = callee_info->stack_frame_offset
    + callee_info->estimated_self_stack_size;
  if (inline_summary (node->global.inlined_to)->estimated_stack_size < peak)
      inline_summary (node->global.inlined_to)->estimated_stack_size = peak;
  ipa_propagate_frequency (node);
  for (e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	inline_update_callee_summaries (e->callee, depth);
      inline_edge_summary (e)->loop_depth += depth;
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    inline_edge_summary (e)->loop_depth += depth;
}

/* Update change_prob of EDGE after INLINED_EDGE has been inlined.
   When functoin A is inlined in B and A calls C with parameter that
   changes with probability PROB1 and C is known to be passthroug
   of argument if B that change with probability PROB2, the probability
   of change is now PROB1*PROB2.  */

static void
remap_edge_change_prob (struct cgraph_edge *inlined_edge,
			struct cgraph_edge *edge)
{
  if (ipa_node_params_vector.exists ())
    {
      int i;
      struct ipa_edge_args *args = IPA_EDGE_REF (edge);
      struct inline_edge_summary *es = inline_edge_summary (edge);
      struct inline_edge_summary *inlined_es
	= inline_edge_summary (inlined_edge);

      for (i = 0; i < ipa_get_cs_argument_count (args); i++)
	{
	  struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, i);
	  if (jfunc->type == IPA_JF_PASS_THROUGH
	      && (ipa_get_jf_pass_through_formal_id (jfunc)
		  < (int) inlined_es->param.length ()))
	    {
	      int jf_formal_id = ipa_get_jf_pass_through_formal_id (jfunc);
	      int prob1 = es->param[i].change_prob;
	      int prob2 = inlined_es->param[jf_formal_id].change_prob;
	      int prob = combine_probabilities (prob1, prob2);

	      if (prob1 && prob2 && !prob)
		prob = 1;

	      es->param[i].change_prob = prob;
	    }
	}
    }
}

/* Update edge summaries of NODE after INLINED_EDGE has been inlined.

   Remap predicates of callees of NODE.  Rest of arguments match
   remap_predicate.

   Also update change probabilities.  */

static void
remap_edge_summaries (struct cgraph_edge *inlined_edge,
		      struct cgraph_node *node,
		      struct inline_summary *info,
		      struct inline_summary *callee_info,
		      vec<int> operand_map,
		      vec<int> offset_map,
		      clause_t possible_truths,
		      struct predicate *toplev_predicate)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      struct predicate p;

      if (e->inline_failed)
	{
	  remap_edge_change_prob (inlined_edge, e);

	  if (es->predicate)
	    {
	      p = remap_predicate (info, callee_info,
				   es->predicate, operand_map, offset_map,
				   possible_truths, toplev_predicate);
	      edge_set_predicate (e, &p);
	      /* TODO: We should remove the edge for code that will be
	         optimized out, but we need to keep verifiers and tree-inline
	         happy.  Make it cold for now.  */
	      if (false_predicate_p (&p))
		{
		  e->count = 0;
		  e->frequency = 0;
		}
	    }
	  else
	    edge_set_predicate (e, toplev_predicate);
	}
      else
	remap_edge_summaries (inlined_edge, e->callee, info, callee_info,
			      operand_map, offset_map, possible_truths,
			      toplev_predicate);
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      struct predicate p;

      remap_edge_change_prob (inlined_edge, e);
      if (es->predicate)
	{
	  p = remap_predicate (info, callee_info,
			       es->predicate, operand_map, offset_map,
			       possible_truths, toplev_predicate);
	  edge_set_predicate (e, &p);
	  /* TODO: We should remove the edge for code that will be optimized
	     out, but we need to keep verifiers and tree-inline happy.
	     Make it cold for now.  */
	  if (false_predicate_p (&p))
	    {
	      e->count = 0;
	      e->frequency = 0;
	    }
	}
      else
	edge_set_predicate (e, toplev_predicate);
    }
}

/* Same as remap_predicate, but set result into hint *HINT.  */

static void
remap_hint_predicate (struct inline_summary *info,
		      struct inline_summary *callee_info,
		      struct predicate **hint,
		      vec<int> operand_map,
		      vec<int> offset_map,
		      clause_t possible_truths,
		      struct predicate *toplev_predicate)
{
  predicate p;

  if (!*hint)
    return;
  p = remap_predicate (info, callee_info,
		       *hint,
		       operand_map, offset_map,
		       possible_truths, toplev_predicate);
  if (!false_predicate_p (&p) && !true_predicate_p (&p))
    {
      if (!*hint)
	set_hint_predicate (hint, p);
      else
	**hint = and_predicates (info->conds, *hint, &p);
    }
}

/* We inlined EDGE.  Update summary of the function we inlined into.  */

void
inline_merge_summary (struct cgraph_edge *edge)
{
  struct inline_summary *callee_info = inline_summary (edge->callee);
  struct cgraph_node *to = (edge->caller->global.inlined_to
			    ? edge->caller->global.inlined_to : edge->caller);
  struct inline_summary *info = inline_summary (to);
  clause_t clause = 0;		/* not_inline is known to be false.  */
  size_time_entry *e;
  vec<int> operand_map = vNULL;
  vec<int> offset_map = vNULL;
  int i;
  struct predicate toplev_predicate;
  struct predicate true_p = true_predicate ();
  struct inline_edge_summary *es = inline_edge_summary (edge);

  if (es->predicate)
    toplev_predicate = *es->predicate;
  else
    toplev_predicate = true_predicate ();

  if (ipa_node_params_vector.exists () && callee_info->conds)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (edge);
      int count = ipa_get_cs_argument_count (args);
      int i;

      evaluate_properties_for_edge (edge, true, &clause, NULL, NULL, NULL);
      if (count)
	{
	  operand_map.safe_grow_cleared (count);
	  offset_map.safe_grow_cleared (count);
	}
      for (i = 0; i < count; i++)
	{
	  struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, i);
	  int map = -1;

	  /* TODO: handle non-NOPs when merging.  */
	  if (jfunc->type == IPA_JF_PASS_THROUGH)
	    {
	      if (ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
		map = ipa_get_jf_pass_through_formal_id (jfunc);
	      if (!ipa_get_jf_pass_through_agg_preserved (jfunc))
		offset_map[i] = -1;
	    }
	  else if (jfunc->type == IPA_JF_ANCESTOR)
	    {
	      HOST_WIDE_INT offset = ipa_get_jf_ancestor_offset (jfunc);
	      if (offset >= 0 && offset < INT_MAX)
		{
		  map = ipa_get_jf_ancestor_formal_id (jfunc);
		  if (!ipa_get_jf_ancestor_agg_preserved (jfunc))
		    offset = -1;
		  offset_map[i] = offset;
		}
	    }
	  operand_map[i] = map;
	  gcc_assert (map < ipa_get_param_count (IPA_NODE_REF (to)));
	}
    }
  for (i = 0; vec_safe_iterate (callee_info->entry, i, &e); i++)
    {
      struct predicate p = remap_predicate (info, callee_info,
					    &e->predicate, operand_map,
					    offset_map, clause,
					    &toplev_predicate);
      if (!false_predicate_p (&p))
	{
	  gcov_type add_time = ((gcov_type) e->time * edge->frequency
				+ CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
	  int prob = predicate_probability (callee_info->conds,
					    &e->predicate,
					    clause, es->param);
	  add_time = apply_probability ((gcov_type) add_time, prob);
	  if (add_time > MAX_TIME * INLINE_TIME_SCALE)
	    add_time = MAX_TIME * INLINE_TIME_SCALE;
	  if (prob != REG_BR_PROB_BASE
	      && dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\t\tScaling time by probability:%f\n",
		       (double) prob / REG_BR_PROB_BASE);
	    }
	  account_size_time (info, e->size, add_time, &p);
	}
    }
  remap_edge_summaries (edge, edge->callee, info, callee_info, operand_map,
			offset_map, clause, &toplev_predicate);
  remap_hint_predicate (info, callee_info,
			&callee_info->loop_iterations,
			operand_map, offset_map, clause, &toplev_predicate);
  remap_hint_predicate (info, callee_info,
			&callee_info->loop_stride,
			operand_map, offset_map, clause, &toplev_predicate);
  remap_hint_predicate (info, callee_info,
			&callee_info->array_index,
			operand_map, offset_map, clause, &toplev_predicate);

  inline_update_callee_summaries (edge->callee,
				  inline_edge_summary (edge)->loop_depth);

  /* We do not maintain predicates of inlined edges, free it.  */
  edge_set_predicate (edge, &true_p);
  /* Similarly remove param summaries.  */
  es->param.release ();
  operand_map.release ();
  offset_map.release ();
}

/* For performance reasons inline_merge_summary is not updating overall size
   and time.  Recompute it.  */

void
inline_update_overall_summary (struct cgraph_node *node)
{
  struct inline_summary *info = inline_summary (node);
  size_time_entry *e;
  int i;

  info->size = 0;
  info->time = 0;
  for (i = 0; vec_safe_iterate (info->entry, i, &e); i++)
    {
      info->size += e->size, info->time += e->time;
      if (info->time > MAX_TIME * INLINE_TIME_SCALE)
	info->time = MAX_TIME * INLINE_TIME_SCALE;
    }
  estimate_calls_size_and_time (node, &info->size, &info->time, NULL,
				~(clause_t) (1 << predicate_false_condition),
				vNULL, vNULL, vNULL);
  info->time = (info->time + INLINE_TIME_SCALE / 2) / INLINE_TIME_SCALE;
  info->size = (info->size + INLINE_SIZE_SCALE / 2) / INLINE_SIZE_SCALE;
}

/* Return hints derrived from EDGE.   */
int
simple_edge_hints (struct cgraph_edge *edge)
{
  int hints = 0;
  struct cgraph_node *to = (edge->caller->global.inlined_to
			    ? edge->caller->global.inlined_to : edge->caller);
  if (inline_summary (to)->scc_no
      && inline_summary (to)->scc_no == inline_summary (edge->callee)->scc_no
      && !cgraph_edge_recursive_p (edge))
    hints |= INLINE_HINT_same_scc;

  if (to->lto_file_data && edge->callee->lto_file_data
      && to->lto_file_data != edge->callee->lto_file_data)
    hints |= INLINE_HINT_cross_module;

  return hints;
}

/* Estimate the time cost for the caller when inlining EDGE.
   Only to be called via estimate_edge_time, that handles the
   caching mechanism.

   When caching, also update the cache entry.  Compute both time and
   size, since we always need both metrics eventually.  */

int
do_estimate_edge_time (struct cgraph_edge *edge)
{
  int time;
  int size;
  inline_hints hints;
  struct cgraph_node *callee;
  clause_t clause;
  vec<tree> known_vals;
  vec<tree> known_binfos;
  vec<ipa_agg_jump_function_p> known_aggs;
  struct inline_edge_summary *es = inline_edge_summary (edge);

  callee = cgraph_function_or_thunk_node (edge->callee, NULL);

  gcc_checking_assert (edge->inline_failed);
  evaluate_properties_for_edge (edge, true,
				&clause, &known_vals, &known_binfos,
				&known_aggs);
  estimate_node_size_and_time (callee, clause, known_vals, known_binfos,
			       known_aggs, &size, &time, &hints, es->param);
  known_vals.release ();
  known_binfos.release ();
  known_aggs.release ();
  gcc_checking_assert (size >= 0);
  gcc_checking_assert (time >= 0);

  /* When caching, update the cache entry.  */
  if (edge_growth_cache.exists ())
    {
      if ((int) edge_growth_cache.length () <= edge->uid)
	edge_growth_cache.safe_grow_cleared (cgraph_edge_max_uid);
      edge_growth_cache[edge->uid].time = time + (time >= 0);

      edge_growth_cache[edge->uid].size = size + (size >= 0);
      hints |= simple_edge_hints (edge);
      edge_growth_cache[edge->uid].hints = hints + 1;
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
  clause_t clause;
  vec<tree> known_vals;
  vec<tree> known_binfos;
  vec<ipa_agg_jump_function_p> known_aggs;

  /* When we do caching, use do_estimate_edge_time to populate the entry.  */

  if (edge_growth_cache.exists ())
    {
      do_estimate_edge_time (edge);
      size = edge_growth_cache[edge->uid].size;
      gcc_checking_assert (size);
      return size - (size > 0);
    }

  callee = cgraph_function_or_thunk_node (edge->callee, NULL);

  /* Early inliner runs without caching, go ahead and do the dirty work.  */
  gcc_checking_assert (edge->inline_failed);
  evaluate_properties_for_edge (edge, true,
				&clause, &known_vals, &known_binfos,
				&known_aggs);
  estimate_node_size_and_time (callee, clause, known_vals, known_binfos,
			       known_aggs, &size, NULL, NULL, vNULL);
  known_vals.release ();
  known_binfos.release ();
  known_aggs.release ();
  return size;
}


/* Estimate the growth of the caller when inlining EDGE.
   Only to be called via estimate_edge_size.  */

inline_hints
do_estimate_edge_hints (struct cgraph_edge *edge)
{
  inline_hints hints;
  struct cgraph_node *callee;
  clause_t clause;
  vec<tree> known_vals;
  vec<tree> known_binfos;
  vec<ipa_agg_jump_function_p> known_aggs;

  /* When we do caching, use do_estimate_edge_time to populate the entry.  */

  if (edge_growth_cache.exists ())
    {
      do_estimate_edge_time (edge);
      hints = edge_growth_cache[edge->uid].hints;
      gcc_checking_assert (hints);
      return hints - 1;
    }

  callee = cgraph_function_or_thunk_node (edge->callee, NULL);

  /* Early inliner runs without caching, go ahead and do the dirty work.  */
  gcc_checking_assert (edge->inline_failed);
  evaluate_properties_for_edge (edge, true,
				&clause, &known_vals, &known_binfos,
				&known_aggs);
  estimate_node_size_and_time (callee, clause, known_vals, known_binfos,
			       known_aggs, NULL, NULL, &hints, vNULL);
  known_vals.release ();
  known_binfos.release ();
  known_aggs.release ();
  hints |= simple_edge_hints (edge);
  return hints;
}


/* Estimate self time of the function NODE after inlining EDGE.  */

int
estimate_time_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  struct inline_edge_summary *es = inline_edge_summary (edge);
  if (!es->predicate || !false_predicate_p (es->predicate))
    {
      gcov_type time =
	inline_summary (node)->time + estimate_edge_time (edge);
      if (time < 0)
	time = 0;
      if (time > MAX_TIME)
	time = MAX_TIME;
      return time;
    }
  return inline_summary (node)->time;
}


/* Estimate the size of NODE after inlining EDGE which should be an
   edge to either NODE or a call inlined into NODE.  */

int
estimate_size_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  struct inline_edge_summary *es = inline_edge_summary (edge);
  if (!es->predicate || !false_predicate_p (es->predicate))
    {
      int size = inline_summary (node)->size + estimate_edge_growth (edge);
      gcc_assert (size >= 0);
      return size;
    }
  return inline_summary (node)->size;
}


struct growth_data
{
  struct cgraph_node *node;
  bool self_recursive;
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

      if (e->caller == d->node
	  || (e->caller->global.inlined_to
	      && e->caller->global.inlined_to == d->node))
	d->self_recursive = true;
      d->growth += estimate_edge_growth (e);
    }
  return false;
}


/* Estimate the growth caused by inlining NODE into all callees.  */

int
do_estimate_growth (struct cgraph_node *node)
{
  struct growth_data d = { node, 0, false };
  struct inline_summary *info = inline_summary (node);

  cgraph_for_node_and_aliases (node, do_estimate_growth_1, &d, true);

  /* For self recursive functions the growth estimation really should be
     infinity.  We don't want to return very large values because the growth
     plays various roles in badness computation fractions.  Be sure to not
     return zero or negative growths. */
  if (d.self_recursive)
    d.growth = d.growth < info->size ? info->size : d.growth;
  else if (DECL_EXTERNAL (node->decl))
    ;
  else
    {
      if (cgraph_will_be_removed_from_program_if_no_direct_calls (node))
	d.growth -= info->size;
      /* COMDAT functions are very often not shared across multiple units
         since they come from various template instantiations.
         Take this into account.  */
      else if (DECL_COMDAT (node->decl)
	       && cgraph_can_remove_if_no_direct_calls_p (node))
	d.growth -= (info->size
		     * (100 - PARAM_VALUE (PARAM_COMDAT_SHARING_PROBABILITY))
		     + 50) / 100;
    }

  if (node_growth_cache.exists ())
    {
      if ((int) node_growth_cache.length () <= node->uid)
	node_growth_cache.safe_grow_cleared (cgraph_max_uid);
      node_growth_cache[node->uid] = d.growth + (d.growth >= 0);
    }
  return d.growth;
}


/* This function performs intraprocedural analysis in NODE that is required to
   inline indirect calls.  */

static void
inline_indirect_intraprocedural_analysis (struct cgraph_node *node)
{
  ipa_analyze_node (node);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      ipa_print_node_params (dump_file, node);
      ipa_print_node_jump_functions (dump_file, node);
    }
}


/* Note function body size.  */

static void
inline_analyze_function (struct cgraph_node *node)
{
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));

  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function: %s/%u\n",
	     node->name (), node->order);
  if (optimize && !node->thunk.thunk_p)
    inline_indirect_intraprocedural_analysis (node);
  compute_inline_parameters (node, false);
  if (!optimize)
    {
      struct cgraph_edge *e;
      for (e = node->callees; e; e = e->next_callee)
	{
	  if (e->inline_failed == CIF_FUNCTION_NOT_CONSIDERED)
	    e->inline_failed = CIF_FUNCTION_NOT_OPTIMIZED;
	  e->call_stmt_cannot_inline_p = true;
	}
      for (e = node->indirect_calls; e; e = e->next_callee)
	{
	  if (e->inline_failed == CIF_FUNCTION_NOT_CONSIDERED)
	    e->inline_failed = CIF_FUNCTION_NOT_OPTIMIZED;
	  e->call_stmt_cannot_inline_p = true;
	}
    }

  pop_cfun ();
}


/* Called when new function is inserted to callgraph late.  */

static void
add_new_function (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  inline_analyze_function (node);
}


/* Note function body size.  */

void
inline_generate_summary (void)
{
  struct cgraph_node *node;

  /* When not optimizing, do not bother to analyze.  Inlining is still done
     because edge redirection needs to happen there.  */
  if (!optimize && !flag_lto && !flag_wpa)
    return;

  function_insertion_hook_holder =
    cgraph_add_function_insertion_hook (&add_new_function, NULL);

  ipa_register_cgraph_hooks ();
  inline_free_summary ();

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->alias)
      inline_analyze_function (node);
}


/* Read predicate from IB.  */

static struct predicate
read_predicate (struct lto_input_block *ib)
{
  struct predicate out;
  clause_t clause;
  int k = 0;

  do
    {
      gcc_assert (k <= MAX_CLAUSES);
      clause = out.clause[k++] = streamer_read_uhwi (ib);
    }
  while (clause);

  /* Zero-initialize the remaining clauses in OUT.  */
  while (k <= MAX_CLAUSES)
    out.clause[k++] = 0;

  return out;
}


/* Write inline summary for edge E to OB.  */

static void
read_inline_edge_summary (struct lto_input_block *ib, struct cgraph_edge *e)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  struct predicate p;
  int length, i;

  es->call_stmt_size = streamer_read_uhwi (ib);
  es->call_stmt_time = streamer_read_uhwi (ib);
  es->loop_depth = streamer_read_uhwi (ib);
  p = read_predicate (ib);
  edge_set_predicate (e, &p);
  length = streamer_read_uhwi (ib);
  if (length)
    {
      es->param.safe_grow_cleared (length);
      for (i = 0; i < length; i++)
	es->param[i].change_prob = streamer_read_uhwi (ib);
    }
}


/* Stream in inline summaries from the section.  */

static void
inline_read_section (struct lto_file_decl_data *file_data, const char *data,
		     size_t len)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int cfg_offset = sizeof (struct lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  struct lto_input_block ib;
  unsigned int i, count2, j;
  unsigned int f_count;

  LTO_INIT_INPUT_BLOCK (ib, (const char *) data + main_offset, 0,
			header->main_size);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, vNULL);
  f_count = streamer_read_uhwi (&ib);
  for (i = 0; i < f_count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      struct inline_summary *info;
      lto_symtab_encoder_t encoder;
      struct bitpack_d bp;
      struct cgraph_edge *e;
      predicate p;

      index = streamer_read_uhwi (&ib);
      encoder = file_data->symtab_node_encoder;
      node = cgraph (lto_symtab_encoder_deref (encoder, index));
      info = inline_summary (node);

      info->estimated_stack_size
	= info->estimated_self_stack_size = streamer_read_uhwi (&ib);
      info->size = info->self_size = streamer_read_uhwi (&ib);
      info->time = info->self_time = streamer_read_uhwi (&ib);

      bp = streamer_read_bitpack (&ib);
      info->inlinable = bp_unpack_value (&bp, 1);

      count2 = streamer_read_uhwi (&ib);
      gcc_assert (!info->conds);
      for (j = 0; j < count2; j++)
	{
	  struct condition c;
	  c.operand_num = streamer_read_uhwi (&ib);
	  c.code = (enum tree_code) streamer_read_uhwi (&ib);
	  c.val = stream_read_tree (&ib, data_in);
	  bp = streamer_read_bitpack (&ib);
	  c.agg_contents = bp_unpack_value (&bp, 1);
	  c.by_ref = bp_unpack_value (&bp, 1);
	  if (c.agg_contents)
	    c.offset = streamer_read_uhwi (&ib);
	  vec_safe_push (info->conds, c);
	}
      count2 = streamer_read_uhwi (&ib);
      gcc_assert (!info->entry);
      for (j = 0; j < count2; j++)
	{
	  struct size_time_entry e;

	  e.size = streamer_read_uhwi (&ib);
	  e.time = streamer_read_uhwi (&ib);
	  e.predicate = read_predicate (&ib);

	  vec_safe_push (info->entry, e);
	}

      p = read_predicate (&ib);
      set_hint_predicate (&info->loop_iterations, p);
      p = read_predicate (&ib);
      set_hint_predicate (&info->loop_stride, p);
      p = read_predicate (&ib);
      set_hint_predicate (&info->array_index, p);
      for (e = node->callees; e; e = e->next_callee)
	read_inline_edge_summary (&ib, e);
      for (e = node->indirect_calls; e; e = e->next_callee)
	read_inline_edge_summary (&ib, e);
    }

  lto_free_section_data (file_data, LTO_section_inline_summary, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}


/* Read inline summary.  Jump functions are shared among ipa-cp
   and inliner, so when ipa-cp is active, we don't need to write them
   twice.  */

void
inline_read_summary (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  inline_summary_alloc ();

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data = lto_get_section_data (file_data,
					       LTO_section_inline_summary,
					       NULL, &len);
      if (data)
	inline_read_section (file_data, data, len);
      else
	/* Fatal error here.  We do not want to support compiling ltrans units
	   with different version of compiler or different flags than the WPA
	   unit, so this should never happen.  */
	fatal_error ("ipa inline summary is missing in input file");
    }
  if (optimize)
    {
      ipa_register_cgraph_hooks ();
      if (!flag_ipa_cp)
	ipa_prop_read_jump_functions ();
    }
  function_insertion_hook_holder =
    cgraph_add_function_insertion_hook (&add_new_function, NULL);
}


/* Write predicate P to OB.  */

static void
write_predicate (struct output_block *ob, struct predicate *p)
{
  int j;
  if (p)
    for (j = 0; p->clause[j]; j++)
      {
	gcc_assert (j < MAX_CLAUSES);
	streamer_write_uhwi (ob, p->clause[j]);
      }
  streamer_write_uhwi (ob, 0);
}


/* Write inline summary for edge E to OB.  */

static void
write_inline_edge_summary (struct output_block *ob, struct cgraph_edge *e)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  int i;

  streamer_write_uhwi (ob, es->call_stmt_size);
  streamer_write_uhwi (ob, es->call_stmt_time);
  streamer_write_uhwi (ob, es->loop_depth);
  write_predicate (ob, es->predicate);
  streamer_write_uhwi (ob, es->param.length ());
  for (i = 0; i < (int) es->param.length (); i++)
    streamer_write_uhwi (ob, es->param[i].change_prob);
}


/* Write inline summary for node in SET.
   Jump functions are shared among ipa-cp and inliner, so when ipa-cp is
   active, we don't need to write them twice.  */

void
inline_write_summary (void)
{
  struct cgraph_node *node;
  struct output_block *ob = create_output_block (LTO_section_inline_summary);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  unsigned int count = 0;
  int i;

  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node> (snode);
      if (cnode && cnode->definition && !cnode->alias)
	count++;
    }
  streamer_write_uhwi (ob, count);

  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node> (snode);
      if (cnode && (node = cnode)->definition && !node->alias)
	{
	  struct inline_summary *info = inline_summary (node);
	  struct bitpack_d bp;
	  struct cgraph_edge *edge;
	  int i;
	  size_time_entry *e;
	  struct condition *c;

	  streamer_write_uhwi (ob,
			       lto_symtab_encoder_encode (encoder,
							  
							  node));
	  streamer_write_hwi (ob, info->estimated_self_stack_size);
	  streamer_write_hwi (ob, info->self_size);
	  streamer_write_hwi (ob, info->self_time);
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, info->inlinable, 1);
	  streamer_write_bitpack (&bp);
	  streamer_write_uhwi (ob, vec_safe_length (info->conds));
	  for (i = 0; vec_safe_iterate (info->conds, i, &c); i++)
	    {
	      streamer_write_uhwi (ob, c->operand_num);
	      streamer_write_uhwi (ob, c->code);
	      stream_write_tree (ob, c->val, true);
	      bp = bitpack_create (ob->main_stream);
	      bp_pack_value (&bp, c->agg_contents, 1);
	      bp_pack_value (&bp, c->by_ref, 1);
	      streamer_write_bitpack (&bp);
	      if (c->agg_contents)
		streamer_write_uhwi (ob, c->offset);
	    }
	  streamer_write_uhwi (ob, vec_safe_length (info->entry));
	  for (i = 0; vec_safe_iterate (info->entry, i, &e); i++)
	    {
	      streamer_write_uhwi (ob, e->size);
	      streamer_write_uhwi (ob, e->time);
	      write_predicate (ob, &e->predicate);
	    }
	  write_predicate (ob, info->loop_iterations);
	  write_predicate (ob, info->loop_stride);
	  write_predicate (ob, info->array_index);
	  for (edge = node->callees; edge; edge = edge->next_callee)
	    write_inline_edge_summary (ob, edge);
	  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
	    write_inline_edge_summary (ob, edge);
	}
    }
  streamer_write_char_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);

  if (optimize && !flag_ipa_cp)
    ipa_prop_write_jump_functions ();
}


/* Release inline summary.  */

void
inline_free_summary (void)
{
  struct cgraph_node *node;
  if (!inline_edge_summary_vec.exists ())
    return;
  FOR_EACH_DEFINED_FUNCTION (node)
    reset_inline_summary (node);
  if (function_insertion_hook_holder)
    cgraph_remove_function_insertion_hook (function_insertion_hook_holder);
  function_insertion_hook_holder = NULL;
  if (node_removal_hook_holder)
    cgraph_remove_node_removal_hook (node_removal_hook_holder);
  node_removal_hook_holder = NULL;
  if (edge_removal_hook_holder)
    cgraph_remove_edge_removal_hook (edge_removal_hook_holder);
  edge_removal_hook_holder = NULL;
  if (node_duplication_hook_holder)
    cgraph_remove_node_duplication_hook (node_duplication_hook_holder);
  node_duplication_hook_holder = NULL;
  if (edge_duplication_hook_holder)
    cgraph_remove_edge_duplication_hook (edge_duplication_hook_holder);
  edge_duplication_hook_holder = NULL;
  vec_free (inline_summary_vec);
  inline_edge_summary_vec.release ();
  if (edge_predicate_pool)
    free_alloc_pool (edge_predicate_pool);
  edge_predicate_pool = 0;
}
