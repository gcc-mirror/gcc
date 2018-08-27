/* Function summary pass.
   Copyright (C) 2003-2018 Free Software Foundation, Inc.
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

/* Analysis of function bodies used by inter-procedural passes

   We estimate for each function
     - function body size and size after specializing into given context
     - average function execution time in a given context
     - function frame size
   For each call
     - call statement size, time and how often the parameters change

   ipa_fn_summary data structures store above information locally (i.e.
   parameters of the function itself) and globally (i.e. parameters of
   the function created by applying all the inline decisions already
   present in the callgraph).

   We provide access to the ipa_fn_summary data structure and
   basic logic updating the parameters when inlining is performed. 

   The summaries are context sensitive.  Context means
     1) partial assignment of known constant values of operands
     2) whether function is inlined into the call or not.
   It is easy to add more variants.  To represent function size and time
   that depends on context (i.e. it is known to be optimized away when
   context is known either by inlining or from IP-CP and cloning),
   we use predicates.

   estimate_edge_size_and_time can be used to query
   function size/time in the given context.  ipa_merge_fn_summary_after_inlining merges
   properties of caller and callee after inlining.

   Finally pass_inline_parameters is exported.  This is used to drive
   computation of function parameters used by the early inliner. IPA
   inlined performs analysis via its analyze_function method. */

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
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "ipa-utils.h"
#include "cfgexpand.h"
#include "gimplify.h"
#include "stringpool.h"
#include "attribs.h"

/* Summaries.  */
function_summary <ipa_fn_summary *> *ipa_fn_summaries;
call_summary <ipa_call_summary *> *ipa_call_summaries;

/* Edge predicates goes here.  */
static object_allocator<predicate> edge_predicate_pool ("edge predicates");


/* Dump IPA hints.  */
void
ipa_dump_hints (FILE *f, ipa_hints hints)
{
  if (!hints)
    return;
  fprintf (f, "IPA hints:");
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
  if (hints & INLINE_HINT_known_hot)
    {
      hints &= ~INLINE_HINT_known_hot;
      fprintf (f, " known_hot");
    }
  gcc_assert (!hints);
}


/* Record SIZE and TIME to SUMMARY.
   The accounted code will be executed when EXEC_PRED is true.
   When NONCONST_PRED is false the code will evaulate to constant and
   will get optimized out in specialized clones of the function.   */

void
ipa_fn_summary::account_size_time (int size, sreal time,
				   const predicate &exec_pred,
				   const predicate &nonconst_pred_in)
{
  size_time_entry *e;
  bool found = false;
  int i;
  predicate nonconst_pred;

  if (exec_pred == false)
    return;

  nonconst_pred = nonconst_pred_in & exec_pred;

  if (nonconst_pred == false)
    return;

  /* We need to create initial empty unconitional clause, but otherwie
     we don't need to account empty times and sizes.  */
  if (!size && time == 0 && size_time_table)
    return;

  gcc_assert (time >= 0);

  for (i = 0; vec_safe_iterate (size_time_table, i, &e); i++)
    if (e->exec_predicate == exec_pred
	&& e->nonconst_predicate == nonconst_pred)
      {
	found = true;
	break;
      }
  if (i == 256)
    {
      i = 0;
      found = true;
      e = &(*size_time_table)[0];
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "\t\tReached limit on number of entries, "
		 "ignoring the predicate.");
    }
  if (dump_file && (dump_flags & TDF_DETAILS) && (time != 0 || size))
    {
      fprintf (dump_file,
	       "\t\tAccounting size:%3.2f, time:%3.2f on %spredicate exec:",
	       ((double) size) / ipa_fn_summary::size_scale,
	       (time.to_double ()), found ? "" : "new ");
      exec_pred.dump (dump_file, conds, 0);
      if (exec_pred != nonconst_pred)
	{
          fprintf (dump_file, " nonconst:");
          nonconst_pred.dump (dump_file, conds);
	}
      else
        fprintf (dump_file, "\n");
    }
  if (!found)
    {
      struct size_time_entry new_entry;
      new_entry.size = size;
      new_entry.time = time;
      new_entry.exec_predicate = exec_pred;
      new_entry.nonconst_predicate = nonconst_pred;
      vec_safe_push (size_time_table, new_entry);
    }
  else
    {
      e->size += size;
      e->time += time;
    }
}

/* We proved E to be unreachable, redirect it to __bultin_unreachable.  */

static struct cgraph_edge *
redirect_to_unreachable (struct cgraph_edge *e)
{
  struct cgraph_node *callee = !e->inline_failed ? e->callee : NULL;
  struct cgraph_node *target = cgraph_node::get_create
		      (builtin_decl_implicit (BUILT_IN_UNREACHABLE));

  if (e->speculative)
    e = e->resolve_speculation (target->decl);
  else if (!e->callee)
    e->make_direct (target);
  else
    e->redirect_callee (target);
  struct ipa_call_summary *es = ipa_call_summaries->get (e);
  e->inline_failed = CIF_UNREACHABLE;
  e->count = profile_count::zero ();
  es->call_stmt_size = 0;
  es->call_stmt_time = 0;
  if (callee)
    callee->remove_symbol_and_inline_clones ();
  return e;
}

/* Set predicate for edge E.  */

static void
edge_set_predicate (struct cgraph_edge *e, predicate *predicate)
{
  /* If the edge is determined to be never executed, redirect it
     to BUILTIN_UNREACHABLE to make it clear to IPA passes the call will
     be optimized out.  */
  if (predicate && *predicate == false
      /* When handling speculative edges, we need to do the redirection
         just once.  Do it always on the direct edge, so we do not
	 attempt to resolve speculation while duplicating the edge.  */
      && (!e->speculative || e->callee))
    e = redirect_to_unreachable (e);

  struct ipa_call_summary *es = ipa_call_summaries->get (e);
  if (predicate && *predicate != true)
    {
      if (!es->predicate)
	es->predicate = edge_predicate_pool.allocate ();
      *es->predicate = *predicate;
    }
  else
    {
      if (es->predicate)
	edge_predicate_pool.remove (es->predicate);
      es->predicate = NULL;
    }
}

/* Set predicate for hint *P.  */

static void
set_hint_predicate (predicate **p, predicate new_predicate)
{
  if (new_predicate == false || new_predicate == true)
    {
      if (*p)
	edge_predicate_pool.remove (*p);
      *p = NULL;
    }
  else
    {
      if (!*p)
	*p = edge_predicate_pool.allocate ();
      **p = new_predicate;
    }
}


/* Compute what conditions may or may not hold given invormation about
   parameters.  RET_CLAUSE returns truths that may hold in a specialized copy,
   whie RET_NONSPEC_CLAUSE returns truths that may hold in an nonspecialized
   copy when called in a given context.  It is a bitmask of conditions. Bit
   0 means that condition is known to be false, while bit 1 means that condition
   may or may not be true.  These differs - for example NOT_INLINED condition
   is always false in the second and also builtin_constant_p tests can not use
   the fact that parameter is indeed a constant.

   KNOWN_VALS is partial mapping of parameters of NODE to constant values.
   KNOWN_AGGS is a vector of aggreggate jump functions for each parameter.
   Return clause of possible truths. When INLINE_P is true, assume that we are
   inlining.

   ERROR_MARK means compile time invariant.  */

static void
evaluate_conditions_for_known_args (struct cgraph_node *node,
				    bool inline_p,
				    vec<tree> known_vals,
				    vec<ipa_agg_jump_function_p>
				    known_aggs,
				    clause_t *ret_clause,
				    clause_t *ret_nonspec_clause)
{
  clause_t clause = inline_p ? 0 : 1 << predicate::not_inlined_condition;
  clause_t nonspec_clause = 1 << predicate::not_inlined_condition;
  struct ipa_fn_summary *info = ipa_fn_summaries->get (node);
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
	  clause |= 1 << (i + predicate::first_dynamic_condition);
	  nonspec_clause |= 1 << (i + predicate::first_dynamic_condition);
	  continue;
	}

      if (c->agg_contents)
	{
	  struct ipa_agg_jump_function *agg;

	  if (c->code == predicate::changed
	      && !c->by_ref
	      && (known_vals[c->operand_num] == error_mark_node))
	    continue;

	  if (known_aggs.exists ())
	    {
	      agg = known_aggs[c->operand_num];
	      val = ipa_find_agg_cst_for_param (agg, known_vals[c->operand_num],
						c->offset, c->by_ref);
	    }
	  else
	    val = NULL_TREE;
	}
      else
	{
	  val = known_vals[c->operand_num];
	  if (val == error_mark_node && c->code != predicate::changed)
	    val = NULL_TREE;
	}

      if (!val)
	{
	  clause |= 1 << (i + predicate::first_dynamic_condition);
	  nonspec_clause |= 1 << (i + predicate::first_dynamic_condition);
	  continue;
	}
      if (c->code == predicate::changed)
	{
	  nonspec_clause |= 1 << (i + predicate::first_dynamic_condition);
	  continue;
	}

      if (tree_to_shwi (TYPE_SIZE (TREE_TYPE (val))) != c->size)
	{
	  clause |= 1 << (i + predicate::first_dynamic_condition);
	  nonspec_clause |= 1 << (i + predicate::first_dynamic_condition);
	  continue;
	}
      if (c->code == predicate::is_not_constant)
	{
	  nonspec_clause |= 1 << (i + predicate::first_dynamic_condition);
	  continue;
	}

      val = fold_unary (VIEW_CONVERT_EXPR, TREE_TYPE (c->val), val);
      res = val
	? fold_binary_to_constant (c->code, boolean_type_node, val, c->val)
	: NULL;

      if (res && integer_zerop (res))
	continue;

      clause |= 1 << (i + predicate::first_dynamic_condition);
      nonspec_clause |= 1 << (i + predicate::first_dynamic_condition);
    }
  *ret_clause = clause;
  if (ret_nonspec_clause)
    *ret_nonspec_clause = nonspec_clause;
}


/* Work out what conditions might be true at invocation of E.  */

void
evaluate_properties_for_edge (struct cgraph_edge *e, bool inline_p,
			      clause_t *clause_ptr,
			      clause_t *nonspec_clause_ptr,
			      vec<tree> *known_vals_ptr,
			      vec<ipa_polymorphic_call_context>
			      *known_contexts_ptr,
			      vec<ipa_agg_jump_function_p> *known_aggs_ptr)
{
  struct cgraph_node *callee = e->callee->ultimate_alias_target ();
  struct ipa_fn_summary *info = ipa_fn_summaries->get (callee);
  vec<tree> known_vals = vNULL;
  vec<ipa_agg_jump_function_p> known_aggs = vNULL;

  if (clause_ptr)
    *clause_ptr = inline_p ? 0 : 1 << predicate::not_inlined_condition;
  if (known_vals_ptr)
    known_vals_ptr->create (0);
  if (known_contexts_ptr)
    known_contexts_ptr->create (0);

  if (ipa_node_params_sum
      && !e->call_stmt_cannot_inline_p
      && ((clause_ptr && info->conds) || known_vals_ptr || known_contexts_ptr))
    {
      struct ipa_node_params *caller_parms_info, *callee_pi;
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      struct ipa_call_summary *es = ipa_call_summaries->get (e);
      int i, count = ipa_get_cs_argument_count (args);

      if (e->caller->global.inlined_to)
	caller_parms_info = IPA_NODE_REF (e->caller->global.inlined_to);
      else
	caller_parms_info = IPA_NODE_REF (e->caller);
      callee_pi = IPA_NODE_REF (e->callee);

      if (count && (info->conds || known_vals_ptr))
	known_vals.safe_grow_cleared (count);
      if (count && (info->conds || known_aggs_ptr))
	known_aggs.safe_grow_cleared (count);
      if (count && known_contexts_ptr)
	known_contexts_ptr->safe_grow_cleared (count);

      for (i = 0; i < count; i++)
	{
	  struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, i);
	  tree cst = ipa_value_from_jfunc (caller_parms_info, jf,
					   ipa_get_type (callee_pi, i));

	  if (!cst && e->call_stmt
	      && i < (int)gimple_call_num_args (e->call_stmt))
	    {
	      cst = gimple_call_arg (e->call_stmt, i);
	      if (!is_gimple_min_invariant (cst))
		cst = NULL;
	    }
	  if (cst)
	    {
	      gcc_checking_assert (TREE_CODE (cst) != TREE_BINFO);
	      if (known_vals.exists ())
		known_vals[i] = cst;
	    }
	  else if (inline_p && !es->param[i].change_prob)
	    known_vals[i] = error_mark_node;

	  if (known_contexts_ptr)
	    (*known_contexts_ptr)[i]
	      = ipa_context_from_jfunc (caller_parms_info, e, i, jf);
	  /* TODO: When IPA-CP starts propagating and merging aggregate jump
	     functions, use its knowledge of the caller too, just like the
	     scalar case above.  */
	  known_aggs[i] = &jf->agg;
	}
    }
  else if (e->call_stmt && !e->call_stmt_cannot_inline_p
	   && ((clause_ptr && info->conds) || known_vals_ptr))
    {
      int i, count = (int)gimple_call_num_args (e->call_stmt);

      if (count && (info->conds || known_vals_ptr))
	known_vals.safe_grow_cleared (count);
      for (i = 0; i < count; i++)
	{
	  tree cst = gimple_call_arg (e->call_stmt, i);
	  if (!is_gimple_min_invariant (cst))
	    cst = NULL;
	  if (cst)
	    known_vals[i] = cst;
	}
    }

  evaluate_conditions_for_known_args (callee, inline_p,
				      known_vals, known_aggs, clause_ptr,
				      nonspec_clause_ptr);

  if (known_vals_ptr)
    *known_vals_ptr = known_vals;
  else
    known_vals.release ();

  if (known_aggs_ptr)
    *known_aggs_ptr = known_aggs;
  else
    known_aggs.release ();
}


/* Allocate the function summary. */

static void
ipa_fn_summary_alloc (void)
{
  gcc_checking_assert (!ipa_fn_summaries);
  ipa_fn_summaries = ipa_fn_summary_t::create_ggc (symtab);
  ipa_call_summaries = new ipa_call_summary_t (symtab, false);
}

ipa_call_summary::~ipa_call_summary ()
{
  if (predicate)
    edge_predicate_pool.remove (predicate);

  param.release ();
}

ipa_fn_summary::~ipa_fn_summary ()
{
  if (loop_iterations)
    edge_predicate_pool.remove (loop_iterations);
  if (loop_stride)
    edge_predicate_pool.remove (loop_stride);
  if (array_index)
    edge_predicate_pool.remove (array_index);
  vec_free (conds);
  vec_free (size_time_table);
}

void
ipa_fn_summary_t::remove_callees (cgraph_node *node)
{
  cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    ipa_call_summaries->remove (e);
  for (e = node->indirect_calls; e; e = e->next_callee)
    ipa_call_summaries->remove (e);
}

/* Same as remap_predicate_after_duplication but handle hint predicate *P.
   Additionally care about allocating new memory slot for updated predicate
   and set it to NULL when it becomes true or false (and thus uninteresting).
 */

static void
remap_hint_predicate_after_duplication (predicate **p,
					clause_t possible_truths)
{
  predicate new_predicate;

  if (!*p)
    return;

  new_predicate = (*p)->remap_after_duplication (possible_truths);
  /* We do not want to free previous predicate; it is used by node origin.  */
  *p = NULL;
  set_hint_predicate (p, new_predicate);
}


/* Hook that is called by cgraph.c when a node is duplicated.  */
void
ipa_fn_summary_t::duplicate (cgraph_node *src,
			     cgraph_node *dst,
			     ipa_fn_summary *,
			     ipa_fn_summary *info)
{
  new (info) ipa_fn_summary (*ipa_fn_summaries->get (src));
  /* TODO: as an optimization, we may avoid copying conditions
     that are known to be false or true.  */
  info->conds = vec_safe_copy (info->conds);

  /* When there are any replacements in the function body, see if we can figure
     out that something was optimized out.  */
  if (ipa_node_params_sum && dst->clone.tree_map)
    {
      vec<size_time_entry, va_gc> *entry = info->size_time_table;
      /* Use SRC parm info since it may not be copied yet.  */
      struct ipa_node_params *parms_info = IPA_NODE_REF (src);
      vec<tree> known_vals = vNULL;
      int count = ipa_get_param_count (parms_info);
      int i, j;
      clause_t possible_truths;
      predicate true_pred = true;
      size_time_entry *e;
      int optimized_out_size = 0;
      bool inlined_to_p = false;
      struct cgraph_edge *edge, *next;

      info->size_time_table = 0;
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
      evaluate_conditions_for_known_args (dst, false,
					  known_vals,
					  vNULL,
					  &possible_truths,
					  /* We are going to specialize,
					     so ignore nonspec truths.  */
					  NULL);
      known_vals.release ();

      info->account_size_time (0, 0, true_pred, true_pred);

      /* Remap size_time vectors.
         Simplify the predicate by prunning out alternatives that are known
         to be false.
         TODO: as on optimization, we can also eliminate conditions known
         to be true.  */
      for (i = 0; vec_safe_iterate (entry, i, &e); i++)
	{
	  predicate new_exec_pred;
	  predicate new_nonconst_pred;
	  new_exec_pred = e->exec_predicate.remap_after_duplication
				 (possible_truths);
	  new_nonconst_pred = e->nonconst_predicate.remap_after_duplication
		  		 (possible_truths);
	  if (new_exec_pred == false || new_nonconst_pred == false)
	    optimized_out_size += e->size;
	  else
	    info->account_size_time (e->size, e->time, new_exec_pred,
			             new_nonconst_pred);
	}

      /* Remap edge predicates with the same simplification as above.
         Also copy constantness arrays.   */
      for (edge = dst->callees; edge; edge = next)
	{
	  predicate new_predicate;
	  struct ipa_call_summary *es = ipa_call_summaries->get_create (edge);
	  next = edge->next_callee;

	  if (!edge->inline_failed)
	    inlined_to_p = true;
	  if (!es->predicate)
	    continue;
	  new_predicate = es->predicate->remap_after_duplication
	    (possible_truths);
	  if (new_predicate == false && *es->predicate != false)
	    optimized_out_size += es->call_stmt_size * ipa_fn_summary::size_scale;
	  edge_set_predicate (edge, &new_predicate);
	}

      /* Remap indirect edge predicates with the same simplificaiton as above. 
         Also copy constantness arrays.   */
      for (edge = dst->indirect_calls; edge; edge = next)
	{
	  predicate new_predicate;
	  struct ipa_call_summary *es = ipa_call_summaries->get_create (edge);
	  next = edge->next_callee;

	  gcc_checking_assert (edge->inline_failed);
	  if (!es->predicate)
	    continue;
	  new_predicate = es->predicate->remap_after_duplication
				 (possible_truths);
	  if (new_predicate == false && *es->predicate != false)
	    optimized_out_size += es->call_stmt_size * ipa_fn_summary::size_scale;
	  edge_set_predicate (edge, &new_predicate);
	}
      remap_hint_predicate_after_duplication (&info->loop_iterations,
					      possible_truths);
      remap_hint_predicate_after_duplication (&info->loop_stride,
					      possible_truths);
      remap_hint_predicate_after_duplication (&info->array_index,
					      possible_truths);

      /* If inliner or someone after inliner will ever start producing
         non-trivial clones, we will get trouble with lack of information
         about updating self sizes, because size vectors already contains
         sizes of the calees.  */
      gcc_assert (!inlined_to_p || !optimized_out_size);
    }
  else
    {
      info->size_time_table = vec_safe_copy (info->size_time_table);
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
  if (!dst->global.inlined_to)
    ipa_update_overall_fn_summary (dst);
}


/* Hook that is called by cgraph.c when a node is duplicated.  */

void
ipa_call_summary_t::duplicate (struct cgraph_edge *src,
			       struct cgraph_edge *dst,
			       struct ipa_call_summary *srcinfo,
			       struct ipa_call_summary *info)
{
  new (info) ipa_call_summary (*srcinfo);
  info->predicate = NULL;
  edge_set_predicate (dst, srcinfo->predicate);
  info->param = srcinfo->param.copy ();
  if (!dst->indirect_unknown_callee && src->indirect_unknown_callee)
    {
      info->call_stmt_size -= (eni_size_weights.indirect_call_cost
			       - eni_size_weights.call_cost);
      info->call_stmt_time -= (eni_time_weights.indirect_call_cost
			       - eni_time_weights.call_cost);
    }
}

/* Dump edge summaries associated to NODE and recursively to all clones.
   Indent by INDENT.  */

static void
dump_ipa_call_summary (FILE *f, int indent, struct cgraph_node *node,
		       struct ipa_fn_summary *info)
{
  struct cgraph_edge *edge;
  for (edge = node->callees; edge; edge = edge->next_callee)
    {
      struct ipa_call_summary *es = ipa_call_summaries->get (edge);
      struct cgraph_node *callee = edge->callee->ultimate_alias_target ();
      int i;

      fprintf (f,
	       "%*s%s/%i %s\n%*s  loop depth:%2i freq:%4.2f size:%2i time: %2i",
	       indent, "", callee->name (), callee->order,
	       !edge->inline_failed
	       ? "inlined" : cgraph_inline_failed_string (edge-> inline_failed),
	       indent, "", es->loop_depth, edge->sreal_frequency ().to_double (),
	       es->call_stmt_size, es->call_stmt_time);

      ipa_fn_summary *s = ipa_fn_summaries->get (callee);
      if (s != NULL)
	fprintf (f, "callee size:%2i stack:%2i",
		 (int) (s->size / ipa_fn_summary::size_scale),
		 (int) s->estimated_stack_size);

      if (es->predicate)
	{
	  fprintf (f, " predicate: ");
	  es->predicate->dump (f, info->conds);
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
	  ipa_fn_summary *s = ipa_fn_summaries->get (callee);
	  fprintf (f, "%*sStack frame offset %i, callee self size %i,"
		   " callee size %i\n",
		   indent + 2, "",
		   (int) s->stack_frame_offset,
		   (int) s->estimated_self_stack_size,
		   (int) s->estimated_stack_size);
	  dump_ipa_call_summary (f, indent + 2, callee, info);
	}
    }
  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
    {
      struct ipa_call_summary *es = ipa_call_summaries->get (edge);
      fprintf (f, "%*sindirect call loop depth:%2i freq:%4.2f size:%2i"
	       " time: %2i",
	       indent, "",
	       es->loop_depth,
	       edge->sreal_frequency ().to_double (), es->call_stmt_size,
	       es->call_stmt_time);
      if (es->predicate)
	{
	  fprintf (f, "predicate: ");
	  es->predicate->dump (f, info->conds);
	}
      else
	fprintf (f, "\n");
    }
}


void
ipa_dump_fn_summary (FILE *f, struct cgraph_node *node)
{
  if (node->definition)
    {
      struct ipa_fn_summary *s = ipa_fn_summaries->get (node);
      if (s != NULL)
	{
	  size_time_entry *e;
	  int i;
	  fprintf (f, "IPA function summary for %s", node->dump_name ());
	  if (DECL_DISREGARD_INLINE_LIMITS (node->decl))
	    fprintf (f, " always_inline");
	  if (s->inlinable)
	    fprintf (f, " inlinable");
	  if (s->fp_expressions)
	    fprintf (f, " fp_expression");
	  fprintf (f, "\n  global time:     %f\n", s->time.to_double ());
	  fprintf (f, "  self size:       %i\n", s->self_size);
	  fprintf (f, "  global size:     %i\n", s->size);
	  fprintf (f, "  min size:       %i\n", s->min_size);
	  fprintf (f, "  self stack:      %i\n",
		   (int) s->estimated_self_stack_size);
	  fprintf (f, "  global stack:    %i\n", (int) s->estimated_stack_size);
	  if (s->growth)
	    fprintf (f, "  estimated growth:%i\n", (int) s->growth);
	  if (s->scc_no)
	    fprintf (f, "  In SCC:          %i\n", (int) s->scc_no);
	  for (i = 0; vec_safe_iterate (s->size_time_table, i, &e); i++)
	    {
	      fprintf (f, "    size:%f, time:%f",
		       (double) e->size / ipa_fn_summary::size_scale,
		       e->time.to_double ());
	      if (e->exec_predicate != true)
		{
		  fprintf (f, ",  executed if:");
		  e->exec_predicate.dump (f, s->conds, 0);
		}
	      if (e->exec_predicate != e->nonconst_predicate)
		{
		  fprintf (f, ",  nonconst if:");
		  e->nonconst_predicate.dump (f, s->conds, 0);
		}
	      fprintf (f, "\n");
	    }
	  if (s->loop_iterations)
	    {
	      fprintf (f, "  loop iterations:");
	      s->loop_iterations->dump (f, s->conds);
	    }
	  if (s->loop_stride)
	    {
	      fprintf (f, "  loop stride:");
	      s->loop_stride->dump (f, s->conds);
	    }
	  if (s->array_index)
	    {
	      fprintf (f, "  array index:");
	      s->array_index->dump (f, s->conds);
	    }
	  fprintf (f, "  calls:\n");
	  dump_ipa_call_summary (f, 4, node, s);
	  fprintf (f, "\n");
	}
      else
	fprintf (f, "IPA summary for %s is missing.\n", node->dump_name ());
    }
}

DEBUG_FUNCTION void
ipa_debug_fn_summary (struct cgraph_node *node)
{
  ipa_dump_fn_summary (stderr, node);
}

void
ipa_dump_fn_summaries (FILE *f)
{
  struct cgraph_node *node;

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->global.inlined_to)
      ipa_dump_fn_summary (f, node);
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
   parameter.  If non-NULL, the size of the memory load (or the SSA_NAME of the
   PARM_DECL) will be stored to *SIZE_P in that case too.  */

static tree
unmodified_parm_1 (gimple *stmt, tree op, HOST_WIDE_INT *size_p)
{
  /* SSA_NAME referring to parm default def?  */
  if (TREE_CODE (op) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (op)
      && TREE_CODE (SSA_NAME_VAR (op)) == PARM_DECL)
    {
      if (size_p)
	*size_p = tree_to_shwi (TYPE_SIZE (TREE_TYPE (op)));
      return SSA_NAME_VAR (op);
    }
  /* Non-SSA parm reference?  */
  if (TREE_CODE (op) == PARM_DECL)
    {
      bool modified = false;

      ao_ref refd;
      ao_ref_init (&refd, op);
      walk_aliased_vdefs (&refd, gimple_vuse (stmt), mark_modified, &modified,
			  NULL);
      if (!modified)
	{
	  if (size_p)
	    *size_p = tree_to_shwi (TYPE_SIZE (TREE_TYPE (op)));
	  return op;
	}
    }
  return NULL_TREE;
}

/* If OP refers to value of function parameter, return the corresponding
   parameter.  Also traverse chains of SSA register assignments.  If non-NULL,
   the size of the memory load (or the SSA_NAME of the PARM_DECL) will be
   stored to *SIZE_P in that case too.  */

static tree
unmodified_parm (gimple *stmt, tree op, HOST_WIDE_INT *size_p)
{
  tree res = unmodified_parm_1 (stmt, op, size_p);
  if (res)
    return res;

  if (TREE_CODE (op) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (op)
      && gimple_assign_single_p (SSA_NAME_DEF_STMT (op)))
    return unmodified_parm (SSA_NAME_DEF_STMT (op),
			    gimple_assign_rhs1 (SSA_NAME_DEF_STMT (op)),
			    size_p);
  return NULL_TREE;
}

/* If OP refers to a value of a function parameter or value loaded from an
   aggregate passed to a parameter (either by value or reference), return TRUE
   and store the number of the parameter to *INDEX_P, the access size into
   *SIZE_P, and information whether and how it has been loaded from an
   aggregate into *AGGPOS.  INFO describes the function parameters, STMT is the
   statement in which OP is used or loaded.  */

static bool
unmodified_parm_or_parm_agg_item (struct ipa_func_body_info *fbi,
				  gimple *stmt, tree op, int *index_p,
				  HOST_WIDE_INT *size_p,
				  struct agg_position_info *aggpos)
{
  tree res = unmodified_parm_1 (stmt, op, size_p);

  gcc_checking_assert (aggpos);
  if (res)
    {
      *index_p = ipa_get_param_decl_index (fbi->info, res);
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
	return unmodified_parm_or_parm_agg_item (fbi, stmt, op, index_p, size_p,
						 aggpos);
    }

  aggpos->agg_contents = true;
  return ipa_load_from_parm_agg (fbi, fbi->info->descriptors,
				 stmt, op, index_p, &aggpos->offset,
				 size_p, &aggpos->by_ref);
}

/* See if statement might disappear after inlining.
   0 - means not eliminated
   1 - half of statements goes away
   2 - for sure it is eliminated.
   We are not terribly sophisticated, basically looking for simple abstraction
   penalty wrappers.  */

static int
eliminated_by_inlining_prob (gimple *stmt)
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
      if (CONVERT_EXPR_CODE_P (rhs_code)
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
	  if (unmodified_parm (stmt, inner_rhs, NULL))
	    rhs_free = true;
	  /* Match expressions of form &this->field. Those will most likely
	     combine with something upstream after inlining.  */
	  else if (TREE_CODE (inner_rhs) == ADDR_EXPR)
	    {
	      tree op = get_base_address (TREE_OPERAND (inner_rhs, 0));
	      if (TREE_CODE (op) == PARM_DECL)
		rhs_free = true;
	      else if (TREE_CODE (op) == MEM_REF
		       && unmodified_parm (stmt, TREE_OPERAND (op, 0), NULL))
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
	      && unmodified_parm (stmt, TREE_OPERAND (inner_rhs, 0), NULL))
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
		  && (unmodified_parm (stmt, TREE_OPERAND (inner_lhs, 0), NULL)
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
set_cond_stmt_execution_predicate (struct ipa_func_body_info *fbi,
				   struct ipa_fn_summary *summary,
				   basic_block bb)
{
  gimple *last;
  tree op;
  int index;
  HOST_WIDE_INT size;
  struct agg_position_info aggpos;
  enum tree_code code, inverted_code;
  edge e;
  edge_iterator ei;
  gimple *set_stmt;
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
  if (unmodified_parm_or_parm_agg_item (fbi, last, op, &index, &size, &aggpos))
    {
      code = gimple_cond_code (last);
      inverted_code = invert_tree_comparison (code, HONOR_NANS (op));

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  enum tree_code this_code = (e->flags & EDGE_TRUE_VALUE
				      ? code : inverted_code);
	  /* invert_tree_comparison will return ERROR_MARK on FP
	     comparsions that are not EQ/NE instead of returning proper
	     unordered one.  Be sure it is not confused with NON_CONSTANT.  */
	  if (this_code != ERROR_MARK)
	    {
	      predicate p
		= add_condition (summary, index, size, &aggpos, this_code,
				 unshare_expr_without_location
				 (gimple_cond_rhs (last)));
	      e->aux = edge_predicate_pool.allocate ();
	      *(predicate *) e->aux = p;
	    }
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
  if (!unmodified_parm_or_parm_agg_item (fbi, set_stmt, op2, &index, &size,
					 &aggpos))
    return;
  FOR_EACH_EDGE (e, ei, bb->succs) if (e->flags & EDGE_FALSE_VALUE)
    {
      predicate p = add_condition (summary, index, size, &aggpos,
				   predicate::is_not_constant, NULL_TREE);
      e->aux = edge_predicate_pool.allocate ();
      *(predicate *) e->aux = p;
    }
}


/* If BB ends by a switch we can turn into predicates, attach corresponding
   predicates to the CFG edges.   */

static void
set_switch_stmt_execution_predicate (struct ipa_func_body_info *fbi,
				     struct ipa_fn_summary *summary,
				     basic_block bb)
{
  gimple *lastg;
  tree op;
  int index;
  HOST_WIDE_INT size;
  struct agg_position_info aggpos;
  edge e;
  edge_iterator ei;
  size_t n;
  size_t case_idx;

  lastg = last_stmt (bb);
  if (!lastg || gimple_code (lastg) != GIMPLE_SWITCH)
    return;
  gswitch *last = as_a <gswitch *> (lastg);
  op = gimple_switch_index (last);
  if (!unmodified_parm_or_parm_agg_item (fbi, last, op, &index, &size, &aggpos))
    return;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      e->aux = edge_predicate_pool.allocate ();
      *(predicate *) e->aux = false;
    }
  n = gimple_switch_num_labels (last);
  for (case_idx = 0; case_idx < n; ++case_idx)
    {
      tree cl = gimple_switch_label (last, case_idx);
      tree min, max;
      predicate p;

      e = gimple_switch_edge (cfun, last, case_idx);
      min = CASE_LOW (cl);
      max = CASE_HIGH (cl);

      /* For default we might want to construct predicate that none
         of cases is met, but it is bit hard to do not having negations
         of conditionals handy.  */
      if (!min && !max)
	p = true;
      else if (!max)
	p = add_condition (summary, index, size, &aggpos, EQ_EXPR,
			   unshare_expr_without_location (min));
      else
	{
	  predicate p1, p2;
	  p1 = add_condition (summary, index, size, &aggpos, GE_EXPR,
			      unshare_expr_without_location (min));
	  p2 = add_condition (summary, index, size, &aggpos, LE_EXPR,
			      unshare_expr_without_location (max));
	  p = p1 & p2;
	}
      *(struct predicate *) e->aux
	= p.or_with (summary->conds, *(struct predicate *) e->aux);
    }
}


/* For each BB in NODE attach to its AUX pointer predicate under
   which it is executable.  */

static void
compute_bb_predicates (struct ipa_func_body_info *fbi,
		       struct cgraph_node *node,
		       struct ipa_fn_summary *summary)
{
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  bool done = false;
  basic_block bb;

  FOR_EACH_BB_FN (bb, my_function)
    {
      set_cond_stmt_execution_predicate (fbi, summary, bb);
      set_switch_stmt_execution_predicate (fbi, summary, bb);
    }

  /* Entry block is always executable.  */
  ENTRY_BLOCK_PTR_FOR_FN (my_function)->aux
    = edge_predicate_pool.allocate ();
  *(predicate *) ENTRY_BLOCK_PTR_FOR_FN (my_function)->aux = true;

  /* A simple dataflow propagation of predicates forward in the CFG.
     TODO: work in reverse postorder.  */
  while (!done)
    {
      done = true;
      FOR_EACH_BB_FN (bb, my_function)
	{
	  predicate p = false;
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      if (e->src->aux)
		{
		  predicate this_bb_predicate
		    = *(predicate *) e->src->aux;
		  if (e->aux)
		    this_bb_predicate &= (*(struct predicate *) e->aux);
		  p = p.or_with (summary->conds, this_bb_predicate);
		  if (p == true)
		    break;
		}
	    }
	  if (p == false)
	    gcc_checking_assert (!bb->aux);
	  else
	    {
	      if (!bb->aux)
		{
		  done = false;
		  bb->aux = edge_predicate_pool.allocate ();
		  *((predicate *) bb->aux) = p;
		}
	      else if (p != *(predicate *) bb->aux)
		{
		  /* This OR operation is needed to ensure monotonous data flow
		     in the case we hit the limit on number of clauses and the
		     and/or operations above give approximate answers.  */
		  p = p.or_with (summary->conds, *(predicate *)bb->aux);
	          if (p != *(predicate *) bb->aux)
		    {
		      done = false;
		      *((predicate *) bb->aux) = p;
		    }
		}
	    }
	}
    }
}


/* Return predicate specifying when the STMT might have result that is not
   a compile time constant.  */

static predicate
will_be_nonconstant_expr_predicate (struct ipa_node_params *info,
				    struct ipa_fn_summary *summary,
				    tree expr,
				    vec<predicate> nonconstant_names)
{
  tree parm;
  int index;
  HOST_WIDE_INT size;

  while (UNARY_CLASS_P (expr))
    expr = TREE_OPERAND (expr, 0);

  parm = unmodified_parm (NULL, expr, &size);
  if (parm && (index = ipa_get_param_decl_index (info, parm)) >= 0)
    return add_condition (summary, index, size, NULL, predicate::changed,
			  NULL_TREE);
  if (is_gimple_min_invariant (expr))
    return false;
  if (TREE_CODE (expr) == SSA_NAME)
    return nonconstant_names[SSA_NAME_VERSION (expr)];
  if (BINARY_CLASS_P (expr) || COMPARISON_CLASS_P (expr))
    {
      predicate p1 = will_be_nonconstant_expr_predicate
	(info, summary, TREE_OPERAND (expr, 0),
	 nonconstant_names);
      if (p1 == true)
	return p1;

      predicate p2;
      p2 = will_be_nonconstant_expr_predicate (info, summary,
					       TREE_OPERAND (expr, 1),
					       nonconstant_names);
      return p1.or_with (summary->conds, p2);
    }
  else if (TREE_CODE (expr) == COND_EXPR)
    {
      predicate p1 = will_be_nonconstant_expr_predicate
	(info, summary, TREE_OPERAND (expr, 0),
	 nonconstant_names);
      if (p1 == true)
	return p1;

      predicate p2;
      p2 = will_be_nonconstant_expr_predicate (info, summary,
					       TREE_OPERAND (expr, 1),
					       nonconstant_names);
      if (p2 == true)
	return p2;
      p1 = p1.or_with (summary->conds, p2);
      p2 = will_be_nonconstant_expr_predicate (info, summary,
					       TREE_OPERAND (expr, 2),
					       nonconstant_names);
      return p2.or_with (summary->conds, p1);
    }
  else if (TREE_CODE (expr) == CALL_EXPR)
    return true;
  else
    {
      debug_tree (expr);
      gcc_unreachable ();
    }
  return false;
}


/* Return predicate specifying when the STMT might have result that is not
   a compile time constant.  */

static predicate
will_be_nonconstant_predicate (struct ipa_func_body_info *fbi,
			       struct ipa_fn_summary *summary,
			       gimple *stmt,
			       vec<predicate> nonconstant_names)
{
  predicate p = true;
  ssa_op_iter iter;
  tree use;
  predicate op_non_const;
  bool is_load;
  int base_index;
  HOST_WIDE_INT size;
  struct agg_position_info aggpos;

  /* What statments might be optimized away
     when their arguments are constant.  */
  if (gimple_code (stmt) != GIMPLE_ASSIGN
      && gimple_code (stmt) != GIMPLE_COND
      && gimple_code (stmt) != GIMPLE_SWITCH
      && (gimple_code (stmt) != GIMPLE_CALL
	  || !(gimple_call_flags (stmt) & ECF_CONST)))
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
      if (!unmodified_parm_or_parm_agg_item (fbi, stmt, op, &base_index, &size,
					     &aggpos))
	return p;
    }
  else
    base_index = -1;

  /* See if we understand all operands before we start
     adding conditionals.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      tree parm = unmodified_parm (stmt, use, NULL);
      /* For arguments we can build a condition.  */
      if (parm && ipa_get_param_decl_index (fbi->info, parm) >= 0)
	continue;
      if (TREE_CODE (use) != SSA_NAME)
	return p;
      /* If we know when operand is constant,
	 we still can say something useful.  */
      if (nonconstant_names[SSA_NAME_VERSION (use)] != true)
	continue;
      return p;
    }

  if (is_load)
    op_non_const =
      add_condition (summary, base_index, size, &aggpos, predicate::changed,
		     NULL);
  else
    op_non_const = false;
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      HOST_WIDE_INT size;
      tree parm = unmodified_parm (stmt, use, &size);
      int index;

      if (parm && (index = ipa_get_param_decl_index (fbi->info, parm)) >= 0)
	{
	  if (index != base_index)
	    p = add_condition (summary, index, size, NULL, predicate::changed,
			       NULL_TREE);
	  else
	    continue;
	}
      else
	p = nonconstant_names[SSA_NAME_VERSION (use)];
      op_non_const = p.or_with (summary->conds, op_non_const);
    }
  if ((gimple_code (stmt) == GIMPLE_ASSIGN || gimple_code (stmt) == GIMPLE_CALL)
      && gimple_op (stmt, 0)
      && TREE_CODE (gimple_op (stmt, 0)) == SSA_NAME)
    nonconstant_names[SSA_NAME_VERSION (gimple_op (stmt, 0))]
      = op_non_const;
  return op_non_const;
}

struct record_modified_bb_info
{
  tree op;
  bitmap bb_set;
  gimple *stmt;
};

/* Value is initialized in INIT_BB and used in USE_BB.  We want to copute
   probability how often it changes between USE_BB.
   INIT_BB->count/USE_BB->count is an estimate, but if INIT_BB
   is in different loop nest, we can do better.
   This is all just estimate.  In theory we look for minimal cut separating
   INIT_BB and USE_BB, but we only want to anticipate loop invariant motion
   anyway.  */

static basic_block
get_minimal_bb (basic_block init_bb, basic_block use_bb)
{
  struct loop *l = find_common_loop (init_bb->loop_father, use_bb->loop_father);
  if (l && l->header->count < init_bb->count)
    return l->header;
  return init_bb;
}

/* Callback of walk_aliased_vdefs.  Records basic blocks where the value may be
   set except for info->stmt.  */

static bool
record_modified (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef, void *data)
{
  struct record_modified_bb_info *info =
    (struct record_modified_bb_info *) data;
  if (SSA_NAME_DEF_STMT (vdef) == info->stmt)
    return false;
  if (gimple_clobber_p (SSA_NAME_DEF_STMT (vdef)))
    return false;
  bitmap_set_bit (info->bb_set,
		  SSA_NAME_IS_DEFAULT_DEF (vdef)
		  ? ENTRY_BLOCK_PTR_FOR_FN (cfun)->index
		  : get_minimal_bb
			 (gimple_bb (SSA_NAME_DEF_STMT (vdef)),
			  gimple_bb (info->stmt))->index);
  if (dump_file)
    {
      fprintf (dump_file, "     Param ");
      print_generic_expr (dump_file, info->op, TDF_SLIM);
      fprintf (dump_file, " changed at bb %i, minimal: %i stmt: ",
	       gimple_bb (SSA_NAME_DEF_STMT (vdef))->index,
	       get_minimal_bb
			 (gimple_bb (SSA_NAME_DEF_STMT (vdef)),
			  gimple_bb (info->stmt))->index);
      print_gimple_stmt (dump_file, SSA_NAME_DEF_STMT (vdef), 0);
    }
  return false;
}

/* Return probability (based on REG_BR_PROB_BASE) that I-th parameter of STMT
   will change since last invocation of STMT. 

   Value 0 is reserved for compile time invariants.
   For common parameters it is REG_BR_PROB_BASE.  For loop invariants it
   ought to be REG_BR_PROB_BASE / estimated_iters.  */

static int
param_change_prob (gimple *stmt, int i)
{
  tree op = gimple_call_arg (stmt, i);
  basic_block bb = gimple_bb (stmt);

  if (TREE_CODE (op) == WITH_SIZE_EXPR)
    op = TREE_OPERAND (op, 0);

  tree base = get_base_address (op);

  /* Global invariants never change.  */
  if (is_gimple_min_invariant (base))
    return 0;

  /* We would have to do non-trivial analysis to really work out what
     is the probability of value to change (i.e. when init statement
     is in a sibling loop of the call). 

     We do an conservative estimate: when call is executed N times more often
     than the statement defining value, we take the frequency 1/N.  */
  if (TREE_CODE (base) == SSA_NAME)
    {
      profile_count init_count;

      if (!bb->count.nonzero_p ())
	return REG_BR_PROB_BASE;

      if (SSA_NAME_IS_DEFAULT_DEF (base))
	init_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      else
	init_count = get_minimal_bb
		      (gimple_bb (SSA_NAME_DEF_STMT (base)),
		       gimple_bb (stmt))->count;

      if (init_count < bb->count)
        return MAX ((init_count.to_sreal_scale (bb->count)
		     * REG_BR_PROB_BASE).to_int (), 1);
      return REG_BR_PROB_BASE;
    }
  else
    {
      ao_ref refd;
      profile_count max = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      struct record_modified_bb_info info;
      tree init = ctor_for_folding (base);

      if (init != error_mark_node)
	return 0;
      if (!bb->count.nonzero_p ())
	return REG_BR_PROB_BASE;
      if (dump_file)
	{
	  fprintf (dump_file, "     Analyzing param change probablity of ");
          print_generic_expr (dump_file, op, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      ao_ref_init (&refd, op);
      info.op = op;
      info.stmt = stmt;
      info.bb_set = BITMAP_ALLOC (NULL);
      walk_aliased_vdefs (&refd, gimple_vuse (stmt), record_modified, &info,
			  NULL);
      if (bitmap_bit_p (info.bb_set, bb->index))
	{
	  if (dump_file)
	    fprintf (dump_file, "     Set in same BB as used.\n");
	  BITMAP_FREE (info.bb_set);
	  return REG_BR_PROB_BASE;
	}

      bitmap_iterator bi;
      unsigned index;
      /* Lookup the most frequent update of the value and believe that
	 it dominates all the other; precise analysis here is difficult.  */
      EXECUTE_IF_SET_IN_BITMAP (info.bb_set, 0, index, bi)
	max = max.max (BASIC_BLOCK_FOR_FN (cfun, index)->count);
      if (dump_file)
	{
          fprintf (dump_file, "     Set with count ");	
	  max.dump (dump_file);
          fprintf (dump_file, " and used with count ");	
	  bb->count.dump (dump_file);
          fprintf (dump_file, " freq %f\n",
		   max.to_sreal_scale (bb->count).to_double ());	
	}

      BITMAP_FREE (info.bb_set);
      if (max < bb->count)
        return MAX ((max.to_sreal_scale (bb->count)
		     * REG_BR_PROB_BASE).to_int (), 1);
      return REG_BR_PROB_BASE;
    }
}

/* Find whether a basic block BB is the final block of a (half) diamond CFG
   sub-graph and if the predicate the condition depends on is known.  If so,
   return true and store the pointer the predicate in *P.  */

static bool
phi_result_unknown_predicate (struct ipa_node_params *info,
			      ipa_fn_summary *summary, basic_block bb,
			      predicate *p,
			      vec<predicate> nonconstant_names)
{
  edge e;
  edge_iterator ei;
  basic_block first_bb = NULL;
  gimple *stmt;

  if (single_pred_p (bb))
    {
      *p = false;
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
  if (*p == true)
    return false;
  else
    return true;
}

/* Given a PHI statement in a function described by inline properties SUMMARY
   and *P being the predicate describing whether the selected PHI argument is
   known, store a predicate for the result of the PHI statement into
   NONCONSTANT_NAMES, if possible.  */

static void
predicate_for_phi_result (struct ipa_fn_summary *summary, gphi *phi,
			  predicate *p,
			  vec<predicate> nonconstant_names)
{
  unsigned i;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg (phi, i)->def;
      if (!is_gimple_min_invariant (arg))
	{
	  gcc_assert (TREE_CODE (arg) == SSA_NAME);
	  *p = p->or_with (summary->conds,
			   nonconstant_names[SSA_NAME_VERSION (arg)]);
	  if (*p == true)
	    return;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\t\tphi predicate: ");
      p->dump (dump_file, summary->conds);
    }
  nonconstant_names[SSA_NAME_VERSION (gimple_phi_result (phi))] = *p;
}

/* Return predicate specifying when array index in access OP becomes non-constant.  */

static predicate
array_index_predicate (ipa_fn_summary *info,
		       vec< predicate> nonconstant_names, tree op)
{
  predicate p = false;
  while (handled_component_p (op))
    {
      if (TREE_CODE (op) == ARRAY_REF || TREE_CODE (op) == ARRAY_RANGE_REF)
	{
	  if (TREE_CODE (TREE_OPERAND (op, 1)) == SSA_NAME)
	    p = p.or_with (info->conds, 
			   nonconstant_names[SSA_NAME_VERSION
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

static gimple *
find_foldable_builtin_expect (basic_block bb)
{
  gimple_stmt_iterator bsi;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);
      if (gimple_call_builtin_p (stmt, BUILT_IN_EXPECT)
	  || gimple_call_builtin_p (stmt, BUILT_IN_EXPECT_WITH_PROBABILITY)
	  || gimple_call_internal_p (stmt, IFN_BUILTIN_EXPECT))
        {
          tree var = gimple_call_lhs (stmt);
          tree arg = gimple_call_arg (stmt, 0);
          use_operand_p use_p;
	  gimple *use_stmt;
          bool match = false;
          bool done = false;

          if (!var || !arg)
            continue;
          gcc_assert (TREE_CODE (var) == SSA_NAME);

          while (TREE_CODE (arg) == SSA_NAME)
            {
	      gimple *stmt_tmp = SSA_NAME_DEF_STMT (arg);
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
                  CASE_CONVERT:
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

/* Return true when the basic blocks contains only clobbers followed by RESX.
   Such BBs are kept around to make removal of dead stores possible with
   presence of EH and will be optimized out by optimize_clobbers later in the
   game. 

   NEED_EH is used to recurse in case the clobber has non-EH predecestors
   that can be clobber only, too.. When it is false, the RESX is not necessary
   on the end of basic block.  */

static bool
clobber_only_eh_bb_p (basic_block bb, bool need_eh = true)
{
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  edge_iterator ei;
  edge e;

  if (need_eh)
    {
      if (gsi_end_p (gsi))
	return false;
      if (gimple_code (gsi_stmt (gsi)) != GIMPLE_RESX)
        return false;
      gsi_prev (&gsi);
    }
  else if (!single_succ_p (bb))
    return false;

  for (; !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	continue;
      if (gimple_clobber_p (stmt))
	continue;
      if (gimple_code (stmt) == GIMPLE_LABEL)
	break;
      return false;
    }

  /* See if all predecestors are either throws or clobber only BBs.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!(e->flags & EDGE_EH)
	&& !clobber_only_eh_bb_p (e->src, false))
      return false;

  return true;
}

/* Return true if STMT compute a floating point expression that may be affected
   by -ffast-math and similar flags.  */

static bool
fp_expression_p (gimple *stmt)
{
  ssa_op_iter i;
  tree op;

  FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_DEF|SSA_OP_USE)
    if (FLOAT_TYPE_P (TREE_TYPE (op)))
      return true;
  return false;
}

/* Analyze function body for NODE.
   EARLY indicates run from early optimization pipeline.  */

static void
analyze_function_body (struct cgraph_node *node, bool early)
{
  sreal time = 0;
  /* Estimate static overhead for function prologue/epilogue and alignment. */
  int size = 2;
  /* Benefits are scaled by probability of elimination that is in range
     <0,2>.  */
  basic_block bb;
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  sreal freq;
  struct ipa_fn_summary *info = ipa_fn_summaries->get_create (node);
  predicate bb_predicate;
  struct ipa_func_body_info fbi;
  vec<predicate> nonconstant_names = vNULL;
  int nblocks, n;
  int *order;
  predicate array_index = true;
  gimple *fix_builtin_expect_stmt;

  gcc_assert (my_function && my_function->cfg);
  gcc_assert (cfun == my_function);

  memset(&fbi, 0, sizeof(fbi));
  info->conds = NULL;
  info->size_time_table = NULL;

  /* When optimizing and analyzing for IPA inliner, initialize loop optimizer
     so we can produce proper inline hints.

     When optimizing and analyzing for early inliner, initialize node params
     so we can produce correct BB predicates.  */
     
  if (opt_for_fn (node->decl, optimize))
    {
      calculate_dominance_info (CDI_DOMINATORS);
      if (!early)
        loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
      else
	{
	  ipa_check_create_node_params ();
	  ipa_initialize_node_params (node);
	}

      if (ipa_node_params_sum)
	{
	  fbi.node = node;
	  fbi.info = IPA_NODE_REF (node);
	  fbi.bb_infos = vNULL;
	  fbi.bb_infos.safe_grow_cleared (last_basic_block_for_fn (cfun));
	  fbi.param_count = count_formal_params(node->decl);
	  nonconstant_names.safe_grow_cleared
	    (SSANAMES (my_function)->length ());
	}
    }

  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function body size: %s\n",
	     node->name ());

  /* When we run into maximal number of entries, we assign everything to the
     constant truth case.  Be sure to have it in list. */
  bb_predicate = true;
  info->account_size_time (0, 0, bb_predicate, bb_predicate);

  bb_predicate = predicate::not_inlined ();
  info->account_size_time (2 * ipa_fn_summary::size_scale, 0, bb_predicate,
		           bb_predicate);

  if (fbi.info)
    compute_bb_predicates (&fbi, node, info);
  order = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  nblocks = pre_and_rev_post_order_compute (NULL, order, false);
  for (n = 0; n < nblocks; n++)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, order[n]);
      freq = bb->count.to_sreal_scale (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count);
      if (clobber_only_eh_bb_p (bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\n Ignoring BB %i;"
		     " it will be optimized away by cleanup_clobbers\n",
		     bb->index);
	  continue;
	}

      /* TODO: Obviously predicates can be propagated down across CFG.  */
      if (fbi.info)
	{
	  if (bb->aux)
	    bb_predicate = *(predicate *) bb->aux;
	  else
	    bb_predicate = false;
	}
      else
	bb_predicate = true;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n BB %i predicate:", bb->index);
	  bb_predicate.dump (dump_file, info->conds);
	}

      if (fbi.info && nonconstant_names.exists ())
	{
	  predicate phi_predicate;
	  bool first_phi = true;

	  for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
	       gsi_next (&bsi))
	    {
	      if (first_phi
		  && !phi_result_unknown_predicate (fbi.info, info, bb,
						    &phi_predicate,
						    nonconstant_names))
		break;
	      first_phi = false;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "  ");
		  print_gimple_stmt (dump_file, gsi_stmt (bsi), 0);
		}
	      predicate_for_phi_result (info, bsi.phi (), &phi_predicate,
					nonconstant_names);
	    }
	}

      fix_builtin_expect_stmt = find_foldable_builtin_expect (bb);

      for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
	   gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  int this_size = estimate_num_insns (stmt, &eni_size_weights);
	  int this_time = estimate_num_insns (stmt, &eni_time_weights);
	  int prob;
	  predicate will_be_nonconstant;

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
	      print_gimple_stmt (dump_file, stmt, 0);
	      fprintf (dump_file, "\t\tfreq:%3.2f size:%3i time:%3i\n",
		       freq.to_double (), this_size,
		       this_time);
	    }

	  if (gimple_assign_load_p (stmt) && nonconstant_names.exists ())
	    {
	      predicate this_array_index;
	      this_array_index =
		array_index_predicate (info, nonconstant_names,
				       gimple_assign_rhs1 (stmt));
	      if (this_array_index != false)
		array_index &= this_array_index;
	    }
	  if (gimple_store_p (stmt) && nonconstant_names.exists ())
	    {
	      predicate this_array_index;
	      this_array_index =
		array_index_predicate (info, nonconstant_names,
				       gimple_get_lhs (stmt));
	      if (this_array_index != false)
		array_index &= this_array_index;
	    }


	  if (is_gimple_call (stmt)
	      && !gimple_call_internal_p (stmt))
	    {
	      struct cgraph_edge *edge = node->get_edge (stmt);
	      ipa_call_summary *es = ipa_call_summaries->get_create (edge);

	      /* Special case: results of BUILT_IN_CONSTANT_P will be always
	         resolved as constant.  We however don't want to optimize
	         out the cgraph edges.  */
	      if (nonconstant_names.exists ()
		  && gimple_call_builtin_p (stmt, BUILT_IN_CONSTANT_P)
		  && gimple_call_lhs (stmt)
		  && TREE_CODE (gimple_call_lhs (stmt)) == SSA_NAME)
		{
		  predicate false_p = false;
		  nonconstant_names[SSA_NAME_VERSION (gimple_call_lhs (stmt))]
		    = false_p;
		}
	      if (ipa_node_params_sum)
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
	  if (fbi.info)
	    will_be_nonconstant
	      = will_be_nonconstant_predicate (&fbi, info,
					       stmt, nonconstant_names);
	  else
	    will_be_nonconstant = true;
	  if (this_time || this_size)
	    {
	      sreal final_time = (sreal)this_time * freq;

	      prob = eliminated_by_inlining_prob (stmt);
	      if (prob == 1 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "\t\t50%% will be eliminated by inlining\n");
	      if (prob == 2 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\t\tWill be eliminated by inlining\n");

	      struct predicate p = bb_predicate & will_be_nonconstant;

	      /* We can ignore statement when we proved it is never going
		 to happen, but we can not do that for call statements
		 because edges are accounted specially.  */

	      if (*(is_gimple_call (stmt) ? &bb_predicate : &p) != false)
		{
		  time += final_time;
		  size += this_size;
		}

	      /* We account everything but the calls.  Calls have their own
	         size/time info attached to cgraph edges.  This is necessary
	         in order to make the cost disappear after inlining.  */
	      if (!is_gimple_call (stmt))
		{
		  if (prob)
		    {
		      predicate ip = bb_predicate & predicate::not_inlined ();
		      info->account_size_time (this_size * prob,
					       (this_time * prob) / 2, ip,
					       p);
		    }
		  if (prob != 2)
		    info->account_size_time (this_size * (2 - prob),
					     (this_time * (2 - prob) / 2),
					     bb_predicate,
					     p);
		}

	      if (!info->fp_expressions && fp_expression_p (stmt))
		{
		  info->fp_expressions = true;
		  if (dump_file)
		    fprintf (dump_file, "   fp_expression set\n");
		}

	      gcc_assert (time >= 0);
	      gcc_assert (size >= 0);
	    }
	}
    }
  set_hint_predicate (&ipa_fn_summaries->get_create (node)->array_index,
		      array_index);
  free (order);

  if (nonconstant_names.exists () && !early)
    {
      struct loop *loop;
      predicate loop_iterations = true;
      predicate loop_stride = true;

      if (dump_file && (dump_flags & TDF_DETAILS))
	flow_loops_dump (dump_file, NULL, 0);
      scev_initialize ();
      FOR_EACH_LOOP (loop, 0)
	{
	  vec<edge> exits;
	  edge ex;
	  unsigned int j;
	  struct tree_niter_desc niter_desc;
	  bb_predicate = *(predicate *) loop->header->aux;

	  exits = get_loop_exit_edges (loop);
	  FOR_EACH_VEC_ELT (exits, j, ex)
	    if (number_of_iterations_exit (loop, ex, &niter_desc, false)
		&& !is_gimple_min_invariant (niter_desc.niter))
	    {
	      predicate will_be_nonconstant
		= will_be_nonconstant_expr_predicate (fbi.info, info,
						      niter_desc.niter,
						      nonconstant_names);
	      if (will_be_nonconstant != true)
		will_be_nonconstant = bb_predicate & will_be_nonconstant;
	      if (will_be_nonconstant != true
		  && will_be_nonconstant != false)
		/* This is slightly inprecise.  We may want to represent each
		   loop with independent predicate.  */
		loop_iterations &= will_be_nonconstant;
	    }
	  exits.release ();
	}

      /* To avoid quadratic behavior we analyze stride predicates only
         with respect to the containing loop.  Thus we simply iterate
	 over all defs in the outermost loop body.  */
      for (loop = loops_for_fn (cfun)->tree_root->inner;
	   loop != NULL; loop = loop->next)
	{
	  basic_block *body = get_loop_body (loop);
	  for (unsigned i = 0; i < loop->num_nodes; i++)
	    {
	      gimple_stmt_iterator gsi;
	      bb_predicate = *(predicate *) body[i]->aux;
	      for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi);
		   gsi_next (&gsi))
		{
		  gimple *stmt = gsi_stmt (gsi);

		  if (!is_gimple_assign (stmt))
		    continue;

		  tree def = gimple_assign_lhs (stmt);
		  if (TREE_CODE (def) != SSA_NAME)
		    continue;

		  affine_iv iv;
		  if (!simple_iv (loop_containing_stmt (stmt),
				  loop_containing_stmt (stmt),
				  def, &iv, true)
		      || is_gimple_min_invariant (iv.step))
		    continue;

		  predicate will_be_nonconstant
		    = will_be_nonconstant_expr_predicate (fbi.info, info,
							  iv.step,
							  nonconstant_names);
		  if (will_be_nonconstant != true)
		    will_be_nonconstant = bb_predicate & will_be_nonconstant;
		  if (will_be_nonconstant != true
		      && will_be_nonconstant != false)
		    /* This is slightly inprecise.  We may want to represent
		       each loop with independent predicate.  */
		    loop_stride = loop_stride & will_be_nonconstant;
		}
	    }
	  free (body);
	}
      ipa_fn_summary *s = ipa_fn_summaries->get (node);
      set_hint_predicate (&s->loop_iterations, loop_iterations);
      set_hint_predicate (&s->loop_stride, loop_stride);
      scev_finalize ();
    }
  FOR_ALL_BB_FN (bb, my_function)
    {
      edge e;
      edge_iterator ei;

      if (bb->aux)
	edge_predicate_pool.remove ((predicate *)bb->aux);
      bb->aux = NULL;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->aux)
	    edge_predicate_pool.remove ((predicate *) e->aux);
	  e->aux = NULL;
	}
    }
  ipa_fn_summary *s = ipa_fn_summaries->get (node);
  s->time = time;
  s->self_size = size;
  nonconstant_names.release ();
  ipa_release_body_info (&fbi);
  if (opt_for_fn (node->decl, optimize))
    {
      if (!early)
        loop_optimizer_finalize ();
      else if (!ipa_edge_args_sum)
	ipa_free_all_node_params ();
      free_dominance_info (CDI_DOMINATORS);
    }
  if (dump_file)
    {
      fprintf (dump_file, "\n");
      ipa_dump_fn_summary (dump_file, node);
    }
}


/* Compute function summary.
   EARLY is true when we compute parameters during early opts.  */

void
compute_fn_summary (struct cgraph_node *node, bool early)
{
  HOST_WIDE_INT self_stack_size;
  struct cgraph_edge *e;
  struct ipa_fn_summary *info;

  gcc_assert (!node->global.inlined_to);

  if (!ipa_fn_summaries)
    ipa_fn_summary_alloc ();

  /* Create a new ipa_fn_summary.  */
  ((ipa_fn_summary_t *)ipa_fn_summaries)->remove_callees (node);
  ipa_fn_summaries->remove (node);
  info = ipa_fn_summaries->get_create (node);

  /* Estimate the stack size for the function if we're optimizing.  */
  self_stack_size = optimize && !node->thunk.thunk_p
		    ? estimated_stack_frame_size (node) : 0;
  info->estimated_self_stack_size = self_stack_size;
  info->estimated_stack_size = self_stack_size;
  info->stack_frame_offset = 0;

  if (node->thunk.thunk_p)
    {
      ipa_call_summary *es = ipa_call_summaries->get_create (node->callees);
      predicate t = true;

      node->local.can_change_signature = false;
      es->call_stmt_size = eni_size_weights.call_cost;
      es->call_stmt_time = eni_time_weights.call_cost;
      info->account_size_time (ipa_fn_summary::size_scale * 2, 2, t, t);
      t = predicate::not_inlined ();
      info->account_size_time (2 * ipa_fn_summary::size_scale, 0, t, t);
      ipa_update_overall_fn_summary (node);
      info->self_size = info->size;
      /* We can not inline instrumentation clones.  */
      if (node->thunk.add_pointer_bounds_args)
	{
          info->inlinable = false;
          node->callees->inline_failed = CIF_CHKP;
	}
      else if (stdarg_p (TREE_TYPE (node->decl)))
	{
	  info->inlinable = false;
	  node->callees->inline_failed = CIF_VARIADIC_THUNK;
	}
      else
        info->inlinable = true;
    }
  else
    {
       /* Even is_gimple_min_invariant rely on current_function_decl.  */
       push_cfun (DECL_STRUCT_FUNCTION (node->decl));

       /* Can this function be inlined at all?  */
       if (!opt_for_fn (node->decl, optimize)
	   && !lookup_attribute ("always_inline",
				 DECL_ATTRIBUTES (node->decl)))
	 info->inlinable = false;
       else
	 info->inlinable = tree_inlinable_function_p (node->decl);

       /* Type attributes can use parameter indices to describe them.  */
       if (TYPE_ATTRIBUTES (TREE_TYPE (node->decl))
	   /* Likewise for #pragma omp declare simd functions or functions
	      with simd attribute.  */
	   || lookup_attribute ("omp declare simd",
				DECL_ATTRIBUTES (node->decl)))
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
		   if (fndecl_built_in_p (cdecl, BUILT_IN_APPLY_ARGS)
		       || fndecl_built_in_p (cdecl, BUILT_IN_VA_START))
		     break;
		 }
	       node->local.can_change_signature = !e;
	     }
	 }
       /* Functions called by instrumentation thunk can't change signature
	  because instrumentation thunk modification is not supported.  */
       if (node->local.can_change_signature)
	 for (e = node->callers; e; e = e->next_caller)
	   if (e->caller->thunk.thunk_p
	       && e->caller->thunk.add_pointer_bounds_args)
	     {
	       node->local.can_change_signature = false;
	       break;
	     }
       analyze_function_body (node, early);
       pop_cfun ();
     }
  for (e = node->callees; e; e = e->next_callee)
    if (e->callee->comdat_local_p ())
      break;
  node->calls_comdat_local = (e != NULL);

  /* Inlining characteristics are maintained by the cgraph_mark_inline.  */
  info->size = info->self_size;
  info->stack_frame_offset = 0;
  info->estimated_stack_size = info->estimated_self_stack_size;

  /* Code above should compute exactly the same result as
     ipa_update_overall_fn_summary but because computation happens in
     different order the roundoff errors result in slight changes.  */
  ipa_update_overall_fn_summary (node);
  gcc_assert (info->size == info->self_size);
}


/* Compute parameters of functions used by inliner using
   current_function_decl.  */

static unsigned int
compute_fn_summary_for_current (void)
{
  compute_fn_summary (cgraph_node::get (current_function_decl), true);
  return 0;
}

/* Estimate benefit devirtualizing indirect edge IE, provided KNOWN_VALS,
   KNOWN_CONTEXTS and KNOWN_AGGS.  */

static bool
estimate_edge_devirt_benefit (struct cgraph_edge *ie,
			      int *size, int *time,
			      vec<tree> known_vals,
			      vec<ipa_polymorphic_call_context> known_contexts,
			      vec<ipa_agg_jump_function_p> known_aggs)
{
  tree target;
  struct cgraph_node *callee;
  struct ipa_fn_summary *isummary;
  enum availability avail;
  bool speculative;

  if (!known_vals.exists () && !known_contexts.exists ())
    return false;
  if (!opt_for_fn (ie->caller->decl, flag_indirect_inlining))
    return false;

  target = ipa_get_indirect_edge_target (ie, known_vals, known_contexts,
					 known_aggs, &speculative);
  if (!target || speculative)
    return false;

  /* Account for difference in cost between indirect and direct calls.  */
  *size -= (eni_size_weights.indirect_call_cost - eni_size_weights.call_cost);
  *time -= (eni_time_weights.indirect_call_cost - eni_time_weights.call_cost);
  gcc_checking_assert (*time >= 0);
  gcc_checking_assert (*size >= 0);

  callee = cgraph_node::get (target);
  if (!callee || !callee->definition)
    return false;
  callee = callee->function_symbol (&avail);
  if (avail < AVAIL_AVAILABLE)
    return false;
  isummary = ipa_fn_summaries->get (callee);
  return isummary->inlinable;
}

/* Increase SIZE, MIN_SIZE (if non-NULL) and TIME for size and time needed to
   handle edge E with probability PROB.
   Set HINTS if edge may be devirtualized.
   KNOWN_VALS, KNOWN_AGGS and KNOWN_CONTEXTS describe context of the call
   site.  */

static inline void
estimate_edge_size_and_time (struct cgraph_edge *e, int *size, int *min_size,
			     sreal *time,
			     int prob,
			     vec<tree> known_vals,
			     vec<ipa_polymorphic_call_context> known_contexts,
			     vec<ipa_agg_jump_function_p> known_aggs,
			     ipa_hints *hints)
{
  struct ipa_call_summary *es = ipa_call_summaries->get (e);
  int call_size = es->call_stmt_size;
  int call_time = es->call_stmt_time;
  int cur_size;
  if (!e->callee
      && estimate_edge_devirt_benefit (e, &call_size, &call_time,
				       known_vals, known_contexts, known_aggs)
      && hints && e->maybe_hot_p ())
    *hints |= INLINE_HINT_indirect_call;
  cur_size = call_size * ipa_fn_summary::size_scale;
  *size += cur_size;
  if (min_size)
    *min_size += cur_size;
  if (prob == REG_BR_PROB_BASE)
    *time += ((sreal)call_time) * e->sreal_frequency ();
  else
    *time += ((sreal)call_time * prob) * e->sreal_frequency ();
}



/* Increase SIZE, MIN_SIZE and TIME for size and time needed to handle all
   calls in NODE.  POSSIBLE_TRUTHS, KNOWN_VALS, KNOWN_AGGS and KNOWN_CONTEXTS
   describe context of the call site.  */

static void
estimate_calls_size_and_time (struct cgraph_node *node, int *size,
			      int *min_size, sreal *time,
			      ipa_hints *hints,
			      clause_t possible_truths,
			      vec<tree> known_vals,
			      vec<ipa_polymorphic_call_context> known_contexts,
			      vec<ipa_agg_jump_function_p> known_aggs)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      struct ipa_call_summary *es = ipa_call_summaries->get_create (e);

      /* Do not care about zero sized builtins.  */
      if (e->inline_failed && !es->call_stmt_size)
	{
	  gcc_checking_assert (!es->call_stmt_time);
	  continue;
	}
      if (!es->predicate
	  || es->predicate->evaluate (possible_truths))
	{
	  if (e->inline_failed)
	    {
	      /* Predicates of calls shall not use NOT_CHANGED codes,
	         sowe do not need to compute probabilities.  */
	      estimate_edge_size_and_time (e, size,
					   es->predicate ? NULL : min_size,
					   time, REG_BR_PROB_BASE,
					   known_vals, known_contexts,
					   known_aggs, hints);
	    }
	  else
	    estimate_calls_size_and_time (e->callee, size, min_size, time,
					  hints,
					  possible_truths,
					  known_vals, known_contexts,
					  known_aggs);
	}
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct ipa_call_summary *es = ipa_call_summaries->get_create (e);
      if (!es->predicate
	  || es->predicate->evaluate (possible_truths))
	estimate_edge_size_and_time (e, size,
				     es->predicate ? NULL : min_size,
				     time, REG_BR_PROB_BASE,
				     known_vals, known_contexts, known_aggs,
				     hints);
    }
}


/* Estimate size and time needed to execute NODE assuming
   POSSIBLE_TRUTHS clause, and KNOWN_VALS, KNOWN_AGGS and KNOWN_CONTEXTS
   information about NODE's arguments.  If non-NULL use also probability
   information present in INLINE_PARAM_SUMMARY vector.
   Additionally detemine hints determined by the context.  Finally compute
   minimal size needed for the call that is independent on the call context and
   can be used for fast estimates.  Return the values in RET_SIZE,
   RET_MIN_SIZE, RET_TIME and RET_HINTS.  */

void
estimate_node_size_and_time (struct cgraph_node *node,
			     clause_t possible_truths,
			     clause_t nonspec_possible_truths,
			     vec<tree> known_vals,
			     vec<ipa_polymorphic_call_context> known_contexts,
			     vec<ipa_agg_jump_function_p> known_aggs,
			     int *ret_size, int *ret_min_size,
			     sreal *ret_time,
			     sreal *ret_nonspecialized_time,
			     ipa_hints *ret_hints,
			     vec<inline_param_summary>
			     inline_param_summary)
{
  struct ipa_fn_summary *info = ipa_fn_summaries->get_create (node);
  size_time_entry *e;
  int size = 0;
  sreal time = 0;
  int min_size = 0;
  ipa_hints hints = 0;
  int i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      bool found = false;
      fprintf (dump_file, "   Estimating body: %s/%i\n"
	       "   Known to be false: ", node->name (),
	       node->order);

      for (i = predicate::not_inlined_condition;
	   i < (predicate::first_dynamic_condition
		+ (int) vec_safe_length (info->conds)); i++)
	if (!(possible_truths & (1 << i)))
	  {
	    if (found)
	      fprintf (dump_file, ", ");
	    found = true;
	    dump_condition (dump_file, info->conds, i);
	  }
    }

  estimate_calls_size_and_time (node, &size, &min_size, &time, &hints, possible_truths,
				known_vals, known_contexts, known_aggs);
  sreal nonspecialized_time = time;

  for (i = 0; vec_safe_iterate (info->size_time_table, i, &e); i++)
    {
      bool exec = e->exec_predicate.evaluate (nonspec_possible_truths);

      /* Because predicates are conservative, it can happen that nonconst is 1
	 but exec is 0.  */
      if (exec)
        {
          bool nonconst = e->nonconst_predicate.evaluate (possible_truths);

	  gcc_checking_assert (e->time >= 0);
	  gcc_checking_assert (time >= 0);

	  /* We compute specialized size only because size of nonspecialized
	     copy is context independent.

	     The difference between nonspecialized execution and specialized is
	     that nonspecialized is not going to have optimized out computations
	     known to be constant in a specialized setting.  */
	  if (nonconst)
	    size += e->size;
	  nonspecialized_time += e->time;
	  if (!nonconst)
	    ;
	  else if (!inline_param_summary.exists ())
	    {
	      if (nonconst)
	        time += e->time;
	    }
	  else
	    {
	      int prob = e->nonconst_predicate.probability 
					       (info->conds, possible_truths,
					        inline_param_summary);
	      gcc_checking_assert (prob >= 0);
	      gcc_checking_assert (prob <= REG_BR_PROB_BASE);
	      time += e->time * prob / REG_BR_PROB_BASE;
	    }
	  gcc_checking_assert (time >= 0);
        }
     }
  gcc_checking_assert ((*info->size_time_table)[0].exec_predicate == true);
  gcc_checking_assert ((*info->size_time_table)[0].nonconst_predicate == true);
  min_size = (*info->size_time_table)[0].size;
  gcc_checking_assert (size >= 0);
  gcc_checking_assert (time >= 0);
  /* nonspecialized_time should be always bigger than specialized time.
     Roundoff issues however may get into the way.  */
  gcc_checking_assert ((nonspecialized_time - time * 0.99) >= -1);

  /* Roundoff issues may make specialized time bigger than nonspecialized
     time.  We do not really want that to happen because some heurstics
     may get confused by seeing negative speedups.  */
  if (time > nonspecialized_time)
    time = nonspecialized_time;

  if (info->loop_iterations
      && !info->loop_iterations->evaluate (possible_truths))
    hints |= INLINE_HINT_loop_iterations;
  if (info->loop_stride
      && !info->loop_stride->evaluate (possible_truths))
    hints |= INLINE_HINT_loop_stride;
  if (info->array_index
      && !info->array_index->evaluate (possible_truths))
    hints |= INLINE_HINT_array_index;
  if (info->scc_no)
    hints |= INLINE_HINT_in_scc;
  if (DECL_DECLARED_INLINE_P (node->decl))
    hints |= INLINE_HINT_declared_inline;

  size = RDIV (size, ipa_fn_summary::size_scale);
  min_size = RDIV (min_size, ipa_fn_summary::size_scale);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n   size:%i time:%f nonspec time:%f\n", (int) size,
	     time.to_double (), nonspecialized_time.to_double ());
  if (ret_time)
    *ret_time = time;
  if (ret_nonspecialized_time)
    *ret_nonspecialized_time = nonspecialized_time;
  if (ret_size)
    *ret_size = size;
  if (ret_min_size)
    *ret_min_size = min_size;
  if (ret_hints)
    *ret_hints = hints;
  return;
}


/* Estimate size and time needed to execute callee of EDGE assuming that
   parameters known to be constant at caller of EDGE are propagated.
   KNOWN_VALS and KNOWN_CONTEXTS are vectors of assumed known constant values
   and types for parameters.  */

void
estimate_ipcp_clone_size_and_time (struct cgraph_node *node,
				   vec<tree> known_vals,
				   vec<ipa_polymorphic_call_context>
				   known_contexts,
				   vec<ipa_agg_jump_function_p> known_aggs,
				   int *ret_size, sreal *ret_time,
				   sreal *ret_nonspec_time,
				   ipa_hints *hints)
{
  clause_t clause, nonspec_clause;

  evaluate_conditions_for_known_args (node, false, known_vals, known_aggs,
				      &clause, &nonspec_clause);
  estimate_node_size_and_time (node, clause, nonspec_clause,
			       known_vals, known_contexts,
			       known_aggs, ret_size, NULL, ret_time,
			       ret_nonspec_time, hints, vNULL);
}


/* Update summary information of inline clones after inlining.
   Compute peak stack usage.  */

static void
inline_update_callee_summaries (struct cgraph_node *node, int depth)
{
  struct cgraph_edge *e;
  ipa_fn_summary *callee_info = ipa_fn_summaries->get (node);
  ipa_fn_summary *caller_info = ipa_fn_summaries->get (node->callers->caller);
  HOST_WIDE_INT peak;

  callee_info->stack_frame_offset
    = caller_info->stack_frame_offset
    + caller_info->estimated_self_stack_size;
  peak = callee_info->stack_frame_offset
    + callee_info->estimated_self_stack_size;

  ipa_fn_summary *s = ipa_fn_summaries->get (node->global.inlined_to);
  if (s->estimated_stack_size < peak)
    s->estimated_stack_size = peak;
  ipa_propagate_frequency (node);
  for (e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	inline_update_callee_summaries (e->callee, depth);
      ipa_call_summaries->get (e)->loop_depth += depth;
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    ipa_call_summaries->get (e)->loop_depth += depth;
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
  if (ipa_node_params_sum)
    {
      int i;
      struct ipa_edge_args *args = IPA_EDGE_REF (edge);
      struct ipa_call_summary *es = ipa_call_summaries->get (edge);
      struct ipa_call_summary *inlined_es
	= ipa_call_summaries->get (inlined_edge);

      for (i = 0; i < ipa_get_cs_argument_count (args); i++)
	{
	  struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, i);
	  if (jfunc->type == IPA_JF_PASS_THROUGH
	      || jfunc->type == IPA_JF_ANCESTOR)
	    {
	      int id = jfunc->type == IPA_JF_PASS_THROUGH
		       ? ipa_get_jf_pass_through_formal_id (jfunc)
		       : ipa_get_jf_ancestor_formal_id (jfunc);
	      if (id < (int) inlined_es->param.length ())
		{
		  int prob1 = es->param[i].change_prob;
		  int prob2 = inlined_es->param[id].change_prob;
		  int prob = combine_probabilities (prob1, prob2);

		  if (prob1 && prob2 && !prob)
		    prob = 1;

		  es->param[i].change_prob = prob;
		}
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
		      struct ipa_fn_summary *info,
		      struct ipa_fn_summary *callee_info,
		      vec<int> operand_map,
		      vec<int> offset_map,
		      clause_t possible_truths,
		      predicate *toplev_predicate)
{
  struct cgraph_edge *e, *next;
  for (e = node->callees; e; e = next)
    {
      struct ipa_call_summary *es = ipa_call_summaries->get (e);
      predicate p;
      next = e->next_callee;

      if (e->inline_failed)
	{
	  remap_edge_change_prob (inlined_edge, e);

	  if (es->predicate)
	    {
	      p = es->predicate->remap_after_inlining
				     (info, callee_info, operand_map,
				      offset_map, possible_truths,
				      *toplev_predicate);
	      edge_set_predicate (e, &p);
	    }
	  else
	    edge_set_predicate (e, toplev_predicate);
	}
      else
	remap_edge_summaries (inlined_edge, e->callee, info, callee_info,
			      operand_map, offset_map, possible_truths,
			      toplev_predicate);
    }
  for (e = node->indirect_calls; e; e = next)
    {
      struct ipa_call_summary *es = ipa_call_summaries->get (e);
      predicate p;
      next = e->next_callee;

      remap_edge_change_prob (inlined_edge, e);
      if (es->predicate)
	{
	  p = es->predicate->remap_after_inlining
				 (info, callee_info, operand_map, offset_map,
			          possible_truths, *toplev_predicate);
	  edge_set_predicate (e, &p);
	}
      else
	edge_set_predicate (e, toplev_predicate);
    }
}

/* Same as remap_predicate, but set result into hint *HINT.  */

static void
remap_hint_predicate (struct ipa_fn_summary *info,
		      struct ipa_fn_summary *callee_info,
		      predicate **hint,
		      vec<int> operand_map,
		      vec<int> offset_map,
		      clause_t possible_truths,
		      predicate *toplev_predicate)
{
  predicate p;

  if (!*hint)
    return;
  p = (*hint)->remap_after_inlining
			 (info, callee_info,
			  operand_map, offset_map,
			  possible_truths, *toplev_predicate);
  if (p != false && p != true)
    {
      if (!*hint)
	set_hint_predicate (hint, p);
      else
	**hint &= p;
    }
}

/* We inlined EDGE.  Update summary of the function we inlined into.  */

void
ipa_merge_fn_summary_after_inlining (struct cgraph_edge *edge)
{
  ipa_fn_summary *callee_info = ipa_fn_summaries->get (edge->callee);
  struct cgraph_node *to = (edge->caller->global.inlined_to
			    ? edge->caller->global.inlined_to : edge->caller);
  struct ipa_fn_summary *info = ipa_fn_summaries->get (to);
  clause_t clause = 0;	/* not_inline is known to be false.  */
  size_time_entry *e;
  vec<int> operand_map = vNULL;
  vec<int> offset_map = vNULL;
  int i;
  predicate toplev_predicate;
  predicate true_p = true;
  struct ipa_call_summary *es = ipa_call_summaries->get (edge);

  if (es->predicate)
    toplev_predicate = *es->predicate;
  else
    toplev_predicate = true;

  info->fp_expressions |= callee_info->fp_expressions;

  if (callee_info->conds)
    evaluate_properties_for_edge (edge, true, &clause, NULL, NULL, NULL, NULL);
  if (ipa_node_params_sum && callee_info->conds)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (edge);
      int count = ipa_get_cs_argument_count (args);
      int i;

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
  for (i = 0; vec_safe_iterate (callee_info->size_time_table, i, &e); i++)
    {
      predicate p;
      p = e->exec_predicate.remap_after_inlining
			     (info, callee_info, operand_map,
			      offset_map, clause,
			      toplev_predicate);
      predicate nonconstp;
      nonconstp = e->nonconst_predicate.remap_after_inlining
				     (info, callee_info, operand_map,
				      offset_map, clause,
				      toplev_predicate);
      if (p != false && nonconstp != false)
	{
	  sreal add_time = ((sreal)e->time * edge->sreal_frequency ());
	  int prob = e->nonconst_predicate.probability (callee_info->conds,
							clause, es->param);
	  add_time = add_time * prob / REG_BR_PROB_BASE;
	  if (prob != REG_BR_PROB_BASE
	      && dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\t\tScaling time by probability:%f\n",
		       (double) prob / REG_BR_PROB_BASE);
	    }
	  info->account_size_time (e->size, add_time, p, nonconstp);
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

  ipa_call_summary *s = ipa_call_summaries->get (edge);
  inline_update_callee_summaries (edge->callee, s->loop_depth);

  /* We do not maintain predicates of inlined edges, free it.  */
  edge_set_predicate (edge, &true_p);
  /* Similarly remove param summaries.  */
  es->param.release ();
  operand_map.release ();
  offset_map.release ();
}

/* For performance reasons ipa_merge_fn_summary_after_inlining is not updating overall size
   and time.  Recompute it.  */

void
ipa_update_overall_fn_summary (struct cgraph_node *node)
{
  struct ipa_fn_summary *info = ipa_fn_summaries->get_create (node);
  size_time_entry *e;
  int i;

  info->size = 0;
  info->time = 0;
  for (i = 0; vec_safe_iterate (info->size_time_table, i, &e); i++)
    {
      info->size += e->size;
      info->time += e->time;
    }
  estimate_calls_size_and_time (node, &info->size, &info->min_size,
				&info->time, NULL,
				~(clause_t) (1 << predicate::false_condition),
				vNULL, vNULL, vNULL);
  info->size = (info->size + ipa_fn_summary::size_scale / 2) / ipa_fn_summary::size_scale;
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

void
inline_analyze_function (struct cgraph_node *node)
{
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));

  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function: %s/%u\n",
	     node->name (), node->order);
  if (opt_for_fn (node->decl, optimize) && !node->thunk.thunk_p)
    inline_indirect_intraprocedural_analysis (node);
  compute_fn_summary (node, false);
  if (!optimize)
    {
      struct cgraph_edge *e;
      for (e = node->callees; e; e = e->next_callee)
	e->inline_failed = CIF_FUNCTION_NOT_OPTIMIZED;
      for (e = node->indirect_calls; e; e = e->next_callee)
	e->inline_failed = CIF_FUNCTION_NOT_OPTIMIZED;
    }

  pop_cfun ();
}


/* Called when new function is inserted to callgraph late.  */

void
ipa_fn_summary_t::insert (struct cgraph_node *node, ipa_fn_summary *)
{
  inline_analyze_function (node);
}

/* Note function body size.  */

static void
ipa_fn_summary_generate (void)
{
  struct cgraph_node *node;

  FOR_EACH_DEFINED_FUNCTION (node)
    if (DECL_STRUCT_FUNCTION (node->decl))
      node->local.versionable = tree_versionable_function_p (node->decl);

  ipa_fn_summary_alloc ();

  ipa_fn_summaries->enable_insertion_hook ();

  ipa_register_cgraph_hooks ();

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->alias
	&& (flag_generate_lto || flag_generate_offload|| flag_wpa
	    || opt_for_fn (node->decl, optimize)))
      inline_analyze_function (node);
}


/* Write inline summary for edge E to OB.  */

static void
read_ipa_call_summary (struct lto_input_block *ib, struct cgraph_edge *e)
{
  struct ipa_call_summary *es = ipa_call_summaries->get_create (e);
  predicate p;
  int length, i;

  es->call_stmt_size = streamer_read_uhwi (ib);
  es->call_stmt_time = streamer_read_uhwi (ib);
  es->loop_depth = streamer_read_uhwi (ib);

  bitpack_d bp = streamer_read_bitpack (ib);
  es->is_return_callee_uncaptured = bp_unpack_value (&bp, 1);

  p.stream_in (ib);
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
  unsigned int i, count2, j;
  unsigned int f_count;

  lto_input_block ib ((const char *) data + main_offset, header->main_size,
		      file_data->mode_table);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, vNULL);
  f_count = streamer_read_uhwi (&ib);
  for (i = 0; i < f_count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      struct ipa_fn_summary *info;
      lto_symtab_encoder_t encoder;
      struct bitpack_d bp;
      struct cgraph_edge *e;
      predicate p;

      index = streamer_read_uhwi (&ib);
      encoder = file_data->symtab_node_encoder;
      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref (encoder,
								index));
      info = ipa_fn_summaries->get_create (node);

      info->estimated_stack_size
	= info->estimated_self_stack_size = streamer_read_uhwi (&ib);
      info->size = info->self_size = streamer_read_uhwi (&ib);
      info->time = sreal::stream_in (&ib);

      bp = streamer_read_bitpack (&ib);
      info->inlinable = bp_unpack_value (&bp, 1);
      info->fp_expressions = bp_unpack_value (&bp, 1);

      count2 = streamer_read_uhwi (&ib);
      gcc_assert (!info->conds);
      for (j = 0; j < count2; j++)
	{
	  struct condition c;
	  c.operand_num = streamer_read_uhwi (&ib);
	  c.size = streamer_read_uhwi (&ib);
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
      gcc_assert (!info->size_time_table);
      for (j = 0; j < count2; j++)
	{
	  struct size_time_entry e;

	  e.size = streamer_read_uhwi (&ib);
	  e.time = sreal::stream_in (&ib);
	  e.exec_predicate.stream_in (&ib);
	  e.nonconst_predicate.stream_in (&ib);

	  vec_safe_push (info->size_time_table, e);
	}

      p.stream_in (&ib);
      set_hint_predicate (&info->loop_iterations, p);
      p.stream_in (&ib);
      set_hint_predicate (&info->loop_stride, p);
      p.stream_in (&ib);
      set_hint_predicate (&info->array_index, p);
      for (e = node->callees; e; e = e->next_callee)
	read_ipa_call_summary (&ib, e);
      for (e = node->indirect_calls; e; e = e->next_callee)
	read_ipa_call_summary (&ib, e);
    }

  lto_free_section_data (file_data, LTO_section_ipa_fn_summary, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}


/* Read inline summary.  Jump functions are shared among ipa-cp
   and inliner, so when ipa-cp is active, we don't need to write them
   twice.  */

static void
ipa_fn_summary_read (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  ipa_fn_summary_alloc ();

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data = lto_get_section_data (file_data,
					       LTO_section_ipa_fn_summary,
					       NULL, &len);
      if (data)
	inline_read_section (file_data, data, len);
      else
	/* Fatal error here.  We do not want to support compiling ltrans units
	   with different version of compiler or different flags than the WPA
	   unit, so this should never happen.  */
	fatal_error (input_location,
		     "ipa inline summary is missing in input file");
    }
  ipa_register_cgraph_hooks ();
  if (!flag_ipa_cp)
    ipa_prop_read_jump_functions ();

  gcc_assert (ipa_fn_summaries);
  ipa_fn_summaries->enable_insertion_hook ();
}


/* Write inline summary for edge E to OB.  */

static void
write_ipa_call_summary (struct output_block *ob, struct cgraph_edge *e)
{
  struct ipa_call_summary *es = ipa_call_summaries->get (e);
  int i;

  streamer_write_uhwi (ob, es->call_stmt_size);
  streamer_write_uhwi (ob, es->call_stmt_time);
  streamer_write_uhwi (ob, es->loop_depth);

  bitpack_d bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, es->is_return_callee_uncaptured, 1);
  streamer_write_bitpack (&bp);

  if (es->predicate)
    es->predicate->stream_out (ob);
  else
    streamer_write_uhwi (ob, 0);
  streamer_write_uhwi (ob, es->param.length ());
  for (i = 0; i < (int) es->param.length (); i++)
    streamer_write_uhwi (ob, es->param[i].change_prob);
}


/* Write inline summary for node in SET.
   Jump functions are shared among ipa-cp and inliner, so when ipa-cp is
   active, we don't need to write them twice.  */

static void
ipa_fn_summary_write (void)
{
  struct output_block *ob = create_output_block (LTO_section_ipa_fn_summary);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  unsigned int count = 0;
  int i;

  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
      if (cnode && cnode->definition && !cnode->alias)
	count++;
    }
  streamer_write_uhwi (ob, count);

  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
      if (cnode && cnode->definition && !cnode->alias)
	{
	  struct ipa_fn_summary *info = ipa_fn_summaries->get (cnode);
	  struct bitpack_d bp;
	  struct cgraph_edge *edge;
	  int i;
	  size_time_entry *e;
	  struct condition *c;

	  streamer_write_uhwi (ob, lto_symtab_encoder_encode (encoder, cnode));
	  streamer_write_hwi (ob, info->estimated_self_stack_size);
	  streamer_write_hwi (ob, info->self_size);
	  info->time.stream_out (ob);
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, info->inlinable, 1);
	  bp_pack_value (&bp, false, 1);
	  bp_pack_value (&bp, info->fp_expressions, 1);
	  streamer_write_bitpack (&bp);
	  streamer_write_uhwi (ob, vec_safe_length (info->conds));
	  for (i = 0; vec_safe_iterate (info->conds, i, &c); i++)
	    {
	      streamer_write_uhwi (ob, c->operand_num);
	      streamer_write_uhwi (ob, c->size);
	      streamer_write_uhwi (ob, c->code);
	      stream_write_tree (ob, c->val, true);
	      bp = bitpack_create (ob->main_stream);
	      bp_pack_value (&bp, c->agg_contents, 1);
	      bp_pack_value (&bp, c->by_ref, 1);
	      streamer_write_bitpack (&bp);
	      if (c->agg_contents)
		streamer_write_uhwi (ob, c->offset);
	    }
	  streamer_write_uhwi (ob, vec_safe_length (info->size_time_table));
	  for (i = 0; vec_safe_iterate (info->size_time_table, i, &e); i++)
	    {
	      streamer_write_uhwi (ob, e->size);
	      e->time.stream_out (ob);
	      e->exec_predicate.stream_out (ob);
	      e->nonconst_predicate.stream_out (ob);
	    }
	  if (info->loop_iterations)
	    info->loop_iterations->stream_out (ob);
 	  else
	    streamer_write_uhwi (ob, 0);
	  if (info->loop_stride)
	    info->loop_stride->stream_out (ob);
 	  else
	    streamer_write_uhwi (ob, 0);
	  if (info->array_index)
	    info->array_index->stream_out (ob);
	  else
	    streamer_write_uhwi (ob, 0);
	  for (edge = cnode->callees; edge; edge = edge->next_callee)
	    write_ipa_call_summary (ob, edge);
	  for (edge = cnode->indirect_calls; edge; edge = edge->next_callee)
	    write_ipa_call_summary (ob, edge);
	}
    }
  streamer_write_char_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);

  if (!flag_ipa_cp)
    ipa_prop_write_jump_functions ();
}


/* Release inline summary.  */

void
ipa_free_fn_summary (void)
{
  struct cgraph_node *node;
  if (!ipa_call_summaries)
    return;
  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->alias)
      ipa_fn_summaries->remove (node);
  ipa_fn_summaries->release ();
  ipa_fn_summaries = NULL;
  ipa_call_summaries->release ();
  delete ipa_call_summaries;
  ipa_call_summaries = NULL;
  edge_predicate_pool.release ();
}

namespace {

const pass_data pass_data_local_fn_summary =
{
  GIMPLE_PASS, /* type */
  "local-fnsummary", /* name */
  OPTGROUP_INLINE, /* optinfo_flags */
  TV_INLINE_PARAMETERS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_local_fn_summary : public gimple_opt_pass
{
public:
  pass_local_fn_summary (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_local_fn_summary, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_local_fn_summary (m_ctxt); }
  virtual unsigned int execute (function *)
    {
      return compute_fn_summary_for_current ();
    }

}; // class pass_local_fn_summary

} // anon namespace

gimple_opt_pass *
make_pass_local_fn_summary (gcc::context *ctxt)
{
  return new pass_local_fn_summary (ctxt);
}


/* Free inline summary.  */

namespace {

const pass_data pass_data_ipa_free_fn_summary =
{
  SIMPLE_IPA_PASS, /* type */
  "free-fnsummary", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_FREE_INLINE_SUMMARY, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_free_fn_summary : public simple_ipa_opt_pass
{
public:
  pass_ipa_free_fn_summary (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_free_fn_summary, ctxt),
      small_p (false)
  {}

  /* opt_pass methods: */
  opt_pass *clone () { return new pass_ipa_free_fn_summary (m_ctxt); }
  void set_pass_param (unsigned int n, bool param)
    {
      gcc_assert (n == 0);
      small_p = param;
    }
  virtual bool gate (function *) { return small_p || !flag_wpa; }
  virtual unsigned int execute (function *)
    {
      ipa_free_fn_summary ();
      /* Early optimizations may make function unreachable.  We can not
	 remove unreachable functions as part of the early opts pass because
	 TODOs are run before subpasses.  Do it here.  */
      return small_p ? TODO_remove_functions | TODO_dump_symtab : 0;
    }

private:
  bool small_p;
}; // class pass_ipa_free_fn_summary

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_free_fn_summary (gcc::context *ctxt)
{
  return new pass_ipa_free_fn_summary (ctxt);
}

namespace {

const pass_data pass_data_ipa_fn_summary =
{
  IPA_PASS, /* type */
  "fnsummary", /* name */
  OPTGROUP_INLINE, /* optinfo_flags */
  TV_IPA_FNSUMMARY, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_dump_symtab ), /* todo_flags_finish */
};

class pass_ipa_fn_summary : public ipa_opt_pass_d
{
public:
  pass_ipa_fn_summary (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_fn_summary, ctxt,
		      ipa_fn_summary_generate, /* generate_summary */
		      ipa_fn_summary_write, /* write_summary */
		      ipa_fn_summary_read, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return 0; }

}; // class pass_ipa_fn_summary

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_fn_summary (gcc::context *ctxt)
{
  return new pass_ipa_fn_summary (ctxt);
}

/* Reset all state within ipa-fnsummary.c so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
ipa_fnsummary_c_finalize (void)
{
  ipa_free_fn_summary ();
}
