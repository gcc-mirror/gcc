/* Function summary pass.
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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
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
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "ipa-utils.h"
#include "cfgexpand.h"
#include "gimplify.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-into-ssa.h"
#include "symtab-clones.h"
#include "gimple-range.h"
#include "tree-dfa.h"

/* Summaries.  */
fast_function_summary <ipa_fn_summary *, va_gc> *ipa_fn_summaries;
fast_function_summary <ipa_size_summary *, va_heap> *ipa_size_summaries;
fast_call_summary <ipa_call_summary *, va_heap> *ipa_call_summaries;

/* Edge predicates goes here.  */
static object_allocator<ipa_predicate> edge_predicate_pool ("edge predicates");


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
  if (hints & INLINE_HINT_known_hot)
    {
      hints &= ~INLINE_HINT_known_hot;
      fprintf (f, " known_hot");
    }
  if (hints & INLINE_HINT_builtin_constant_p)
    {
      hints &= ~INLINE_HINT_builtin_constant_p;
      fprintf (f, " builtin_constant_p");
    }
  gcc_assert (!hints);
}


/* Record SIZE and TIME to SUMMARY.
   The accounted code will be executed when EXEC_PRED is true.
   When NONCONST_PRED is false the code will evaluate to constant and
   will get optimized out in specialized clones of the function.
   If CALL is true account to call_size_time_table rather than
   size_time_table.   */

void
ipa_fn_summary::account_size_time (int size, sreal time,
				   const ipa_predicate &exec_pred,
				   const ipa_predicate &nonconst_pred_in,
				   bool call)
{
  size_time_entry *e;
  bool found = false;
  int i;
  ipa_predicate nonconst_pred;
  vec<size_time_entry> *table = call ? &call_size_time_table : &size_time_table;

  if (exec_pred == false)
    return;

  nonconst_pred = nonconst_pred_in & exec_pred;

  if (nonconst_pred == false)
    return;

  /* We need to create initial empty unconditional clause, but otherwise
     we don't need to account empty times and sizes.  */
  if (!size && time == 0 && table->length ())
    return;

  /* Only for calls we are unaccounting what we previously recorded.  */
  gcc_checking_assert (time >= 0 || call);

  for (i = 0; table->iterate (i, &e); i++)
    if (e->exec_predicate == exec_pred
	&& e->nonconst_predicate == nonconst_pred)
      {
	found = true;
	break;
      }
  if (i == max_size_time_table_size)
    {
      i = 0;
      found = true;
      e = &(*table)[0];
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
      class size_time_entry new_entry;
      new_entry.size = size;
      new_entry.time = time;
      new_entry.exec_predicate = exec_pred;
      new_entry.nonconst_predicate = nonconst_pred;
      if (call)
	call_size_time_table.safe_push (new_entry);
      else
	size_time_table.safe_push (new_entry);
    }
  else
    {
      e->size += size;
      e->time += time;
      /* FIXME: PR bootstrap/92653 gcc_checking_assert (e->time >= -1); */
      /* Tolerate small roundoff issues.  */
      if (e->time < 0)
	e->time = 0;
    }
}

/* We proved E to be unreachable, redirect it to __builtin_unreachable.  */

static struct cgraph_edge *
redirect_to_unreachable (struct cgraph_edge *e)
{
  struct cgraph_node *callee = !e->inline_failed ? e->callee : NULL;
  struct cgraph_node *target
    = cgraph_node::get_create (builtin_decl_unreachable ());

  if (e->speculative)
    e = cgraph_edge::resolve_speculation (e, target->decl);
  else if (!e->callee)
    e = cgraph_edge::make_direct (e, target);
  else
    e->redirect_callee (target);
  class ipa_call_summary *es = ipa_call_summaries->get (e);
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
edge_set_predicate (struct cgraph_edge *e, ipa_predicate *predicate)
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

  class ipa_call_summary *es = ipa_call_summaries->get (e);
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
set_hint_predicate (ipa_predicate **p, ipa_predicate new_predicate)
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

/* Find if NEW_PREDICATE is already in V and if so, increment its freq.
   Otherwise add a new item to the vector with this predicate and frerq equal
   to add_freq, unless the number of predicates would exceed MAX_NUM_PREDICATES
   in which case the function does nothing.  */

static void
add_freqcounting_predicate (vec<ipa_freqcounting_predicate, va_gc> **v,
			    const ipa_predicate &new_predicate, sreal add_freq,
			    unsigned max_num_predicates)
{
  if (new_predicate == false || new_predicate == true)
    return;
  ipa_freqcounting_predicate *f;
  for (int i = 0; vec_safe_iterate (*v, i, &f); i++)
    if (new_predicate == f->predicate)
      {
	f->freq += add_freq;
	return;
      }
  if (vec_safe_length (*v) >= max_num_predicates)
    /* Too many different predicates to account for.  */
    return;

  ipa_freqcounting_predicate fcp;
  fcp.predicate = NULL;
  set_hint_predicate (&fcp.predicate, new_predicate);
  fcp.freq = add_freq;
  vec_safe_push (*v, fcp);
  return;
}

/* Compute what conditions may or may not hold given information about
   parameters.  RET_CLAUSE returns truths that may hold in a specialized copy,
   while RET_NONSPEC_CLAUSE returns truths that may hold in an nonspecialized
   copy when called in a given context.  It is a bitmask of conditions. Bit
   0 means that condition is known to be false, while bit 1 means that condition
   may or may not be true.  These differs - for example NOT_INLINED condition
   is always false in the second and also builtin_constant_p tests cannot use
   the fact that parameter is indeed a constant.

   When INLINE_P is true, assume that we are inlining.  AVAL contains known
   information about argument values.  The function does not modify its content
   and so AVALs could also be of type ipa_call_arg_values but so far all
   callers work with the auto version and so we avoid the conversion for
   convenience.

   ERROR_MARK value of an argument means compile time invariant.  */

static void
evaluate_conditions_for_known_args (struct cgraph_node *node,
				    bool inline_p,
				    ipa_auto_call_arg_values *avals,
				    clause_t *ret_clause,
				    clause_t *ret_nonspec_clause,
				    ipa_call_summary *es)
{
  clause_t clause = inline_p ? 0 : 1 << ipa_predicate::not_inlined_condition;
  clause_t nonspec_clause = 1 << ipa_predicate::not_inlined_condition;
  class ipa_fn_summary *info = ipa_fn_summaries->get (node);
  int i;
  struct condition *c;

  for (i = 0; vec_safe_iterate (info->conds, i, &c); i++)
    {
      tree val = NULL;
      tree res;
      int j;
      struct expr_eval_op *op;

      if (c->code == ipa_predicate::not_sra_candidate)
	{
	  if (!inline_p
	      || !es
	      || (int)es->param.length () <= c->operand_num
	      || !es->param[c->operand_num].points_to_possible_sra_candidate)
	    clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
	  nonspec_clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
	  continue;
	}

      if (c->agg_contents)
	{
	  if (c->code == ipa_predicate::changed
	      && !c->by_ref
	      && (avals->safe_sval_at(c->operand_num) == error_mark_node))
	    continue;

	  if (tree sval = avals->safe_sval_at (c->operand_num))
	    val = ipa_find_agg_cst_from_init (sval, c->offset, c->by_ref);
	  if (!val)
	    {
	      ipa_argagg_value_list avs (avals);
	      val = avs.get_value (c->operand_num, c->offset / BITS_PER_UNIT,
				   c->by_ref);
	    }
	}
      else
	{
	  val = avals->safe_sval_at (c->operand_num);
	  if (val && val == error_mark_node
	      && c->code != ipa_predicate::changed)
	    val = NULL_TREE;
	}

      if (!val
	  && (c->code == ipa_predicate::changed
	      || c->code == ipa_predicate::is_not_constant))
	{
	  clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
	  nonspec_clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
	  continue;
	}
      if (c->code == ipa_predicate::changed)
	{
	  nonspec_clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
	  continue;
	}

      if (c->code == ipa_predicate::is_not_constant)
	{
	  nonspec_clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
	  continue;
	}

      if (val && TYPE_SIZE (c->type) == TYPE_SIZE (TREE_TYPE (val)))
	{
	  if (c->type != TREE_TYPE (val))
	    val = fold_unary (VIEW_CONVERT_EXPR, c->type, val);
	  for (j = 0; vec_safe_iterate (c->param_ops, j, &op); j++)
	    {
	      if (!val)
		break;
	      if (!op->val[0])
		val = fold_unary (op->code, op->type, val);
	      else if (!op->val[1])
		val = fold_binary (op->code, op->type,
				   op->index ? op->val[0] : val,
				   op->index ? val : op->val[0]);
	      else if (op->index == 0)
		val = fold_ternary (op->code, op->type,
				    val, op->val[0], op->val[1]);
	      else if (op->index == 1)
		val = fold_ternary (op->code, op->type,
				    op->val[0], val, op->val[1]);
	      else if (op->index == 2)
		val = fold_ternary (op->code, op->type,
				    op->val[0], op->val[1], val);
	      else
		val = NULL_TREE;
	    }

	  res = val
	    ? fold_binary_to_constant (c->code, boolean_type_node, val, c->val)
	    : NULL;

	  if (res && integer_zerop (res))
	    continue;
	  if (res && integer_onep (res))
	    {
	      clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
	      nonspec_clause
		|= 1 << (i + ipa_predicate::first_dynamic_condition);
	      continue;
	    }
	}
      if (c->operand_num < (int) avals->m_known_value_ranges.length ()
	  && !c->agg_contents
	  && (!val || TREE_CODE (val) != INTEGER_CST))
	{
	  Value_Range vr (avals->m_known_value_ranges[c->operand_num]);
	  if (!vr.undefined_p ()
	      && !vr.varying_p ()
	      && (TYPE_SIZE (c->type) == TYPE_SIZE (vr.type ())))
	    {
	      if (!useless_type_conversion_p (c->type, vr.type ()))
		range_cast (vr, c->type);

	      for (j = 0; vec_safe_iterate (c->param_ops, j, &op); j++)
		{
		  if (vr.varying_p () || vr.undefined_p ())
		    break;

		  Value_Range res (op->type);
		  if (!op->val[0])
		    {
		      Value_Range varying (op->type);
		      varying.set_varying (op->type);
		      range_op_handler handler (op->code);
		      if (!handler
			  || !res.supports_type_p (op->type)
			  || !handler.fold_range (res, op->type, vr, varying))
			res.set_varying (op->type);
		    }
		  else if (!op->val[1])
		    {
		      Value_Range op0 (op->type);
		      range_op_handler handler (op->code);

		      ipa_range_set_and_normalize (op0, op->val[0]);

		      if (!handler
			  || !res.supports_type_p (op->type)
			  || !handler.fold_range (res, op->type,
						  op->index ? op0 : vr,
						  op->index ? vr : op0))
			res.set_varying (op->type);
		    }
		  else
		    res.set_varying (op->type);
		  vr = res;
		}
	      if (!vr.varying_p () && !vr.undefined_p ())
		{
		  int_range<2> res;
		  Value_Range val_vr (TREE_TYPE (c->val));
		  range_op_handler handler (c->code);

		  ipa_range_set_and_normalize (val_vr, c->val);

		  if (!handler
		      || !val_vr.supports_type_p (TREE_TYPE (c->val))
		      || !handler.fold_range (res, boolean_type_node, vr, val_vr))
		    res.set_varying (boolean_type_node);

		  if (res.zero_p ())
		    continue;
		}
	    }
	}

      clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
      nonspec_clause |= 1 << (i + ipa_predicate::first_dynamic_condition);
    }
  *ret_clause = clause;
  if (ret_nonspec_clause)
    *ret_nonspec_clause = nonspec_clause;
}

/* Return true if VRP will be exectued on the function.
   We do not want to anticipate optimizations that will not happen.

   FIXME: This can be confused with -fdisable and debug counters and thus
   it should not be used for correctness (only to make heuristics work).
   This means that inliner should do its own optimizations of expressions
   that it predicts to be constant so wrong code can not be triggered by
   builtin_constant_p.  */

static bool
vrp_will_run_p (struct cgraph_node *node)
{
  return (opt_for_fn (node->decl, optimize)
	  && !opt_for_fn (node->decl, optimize_debug)
	  && opt_for_fn (node->decl, flag_tree_vrp));
}

/* Similarly about FRE.  */

static bool
fre_will_run_p (struct cgraph_node *node)
{
  return (opt_for_fn (node->decl, optimize)
	  && !opt_for_fn (node->decl, optimize_debug)
	  && opt_for_fn (node->decl, flag_tree_fre));
}

/* Work out what conditions might be true at invocation of E.
   Compute costs for inlined edge if INLINE_P is true.

   Return in CLAUSE_PTR the evaluated conditions and in NONSPEC_CLAUSE_PTR
   (if non-NULL) conditions evaluated for nonspecialized clone called
   in a given context.

   Vectors in AVALS will be populated with useful known information about
   argument values - information not known to have any uses will be omitted -
   except for m_known_contexts which will only be calculated if
   COMPUTE_CONTEXTS is true.  */

void
evaluate_properties_for_edge (struct cgraph_edge *e, bool inline_p,
			      clause_t *clause_ptr,
			      clause_t *nonspec_clause_ptr,
			      ipa_auto_call_arg_values *avals,
			      bool compute_contexts)
{
  struct cgraph_node *callee = e->callee->ultimate_alias_target ();
  class ipa_fn_summary *info = ipa_fn_summaries->get (callee);
  class ipa_edge_args *args;
  class ipa_call_summary *es = NULL;

  if (clause_ptr)
    *clause_ptr = inline_p ? 0 : 1 << ipa_predicate::not_inlined_condition;

  if (ipa_node_params_sum
      && !e->call_stmt_cannot_inline_p
      && (info->conds || compute_contexts)
      && (args = ipa_edge_args_sum->get (e)) != NULL)
    {
      struct cgraph_node *caller;
      class ipa_node_params *caller_parms_info, *callee_pi = NULL;
      int i, count = ipa_get_cs_argument_count (args);
      es = ipa_call_summaries->get (e);

      if (count)
	{
	  if (e->caller->inlined_to)
	    caller = e->caller->inlined_to;
	  else
	    caller = e->caller;
	  caller_parms_info = ipa_node_params_sum->get (caller);
          callee_pi = ipa_node_params_sum->get (callee);

	  /* Watch for thunks.  */
	  if (callee_pi)
	    /* Watch for variadic functions.  */
	    count = MIN (count, ipa_get_param_count (callee_pi));
	}

      if (callee_pi)
	for (i = 0; i < count; i++)
	  {
	    struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, i);

	    if (ipa_is_param_used_by_indirect_call (callee_pi, i)
		|| ipa_is_param_used_by_ipa_predicates (callee_pi, i))
	      {
		/* Determine if we know constant value of the parameter.  */
		tree type = ipa_get_type (callee_pi, i);
		tree cst = ipa_value_from_jfunc (caller_parms_info, jf, type);

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
		    if (!avals->m_known_vals.length ())
		      avals->m_known_vals.safe_grow_cleared (count, true);
		    avals->m_known_vals[i] = cst;
		  }
		else if (inline_p && !es->param[i].change_prob)
		  {
		    if (!avals->m_known_vals.length ())
		      avals->m_known_vals.safe_grow_cleared (count, true);
		    avals->m_known_vals[i] = error_mark_node;
		  }

		/* If we failed to get simple constant, try value range.  */
		if ((!cst || TREE_CODE (cst) != INTEGER_CST)
		    && vrp_will_run_p (caller)
		    && ipa_is_param_used_by_ipa_predicates (callee_pi, i))
		  {
		    Value_Range vr (type);

		    ipa_value_range_from_jfunc (vr, caller_parms_info, e, jf, type);
		    if (!vr.undefined_p () && !vr.varying_p ())
		      {
			if (!avals->m_known_value_ranges.length ())
			  avals->m_known_value_ranges.safe_grow_cleared (count,
									 true);
			avals->m_known_value_ranges[i] = vr;
		      }
		  }

		/* Determine known aggregate values.  */
		if (fre_will_run_p (caller))
		  ipa_push_agg_values_from_jfunc (caller_parms_info,
						  caller, &jf->agg, i,
						  &avals->m_known_aggs);
	      }

	    /* For calls used in polymorphic calls we further determine
	       polymorphic call context.  */
	    if (compute_contexts
		&& ipa_is_param_used_by_polymorphic_call (callee_pi, i))
	      {
		ipa_polymorphic_call_context
		   ctx = ipa_context_from_jfunc (caller_parms_info, e, i, jf);
		if (!ctx.useless_p ())
		  {
		    if (!avals->m_known_contexts.length ())
		      avals->m_known_contexts.safe_grow_cleared (count, true);
		    avals->m_known_contexts[i]
		      = ipa_context_from_jfunc (caller_parms_info, e, i, jf);
		  }
	       }
	  }
	else
	  gcc_assert (!count || callee->thunk);
    }
  else if (e->call_stmt && !e->call_stmt_cannot_inline_p && info->conds)
    {
      int i, count = (int)gimple_call_num_args (e->call_stmt);

      for (i = 0; i < count; i++)
	{
	  tree cst = gimple_call_arg (e->call_stmt, i);
	  if (!is_gimple_min_invariant (cst))
	    cst = NULL;
	  if (cst)
	    {
	      if (!avals->m_known_vals.length ())
		avals->m_known_vals.safe_grow_cleared (count, true);
	      avals->m_known_vals[i] = cst;
	    }
	}
    }

  evaluate_conditions_for_known_args (callee, inline_p, avals, clause_ptr,
				      nonspec_clause_ptr, es);
}


/* Allocate the function summary. */

static void
ipa_fn_summary_alloc (void)
{
  gcc_checking_assert (!ipa_fn_summaries);
  ipa_size_summaries = new ipa_size_summary_t (symtab);
  ipa_fn_summaries = ipa_fn_summary_t::create_ggc (symtab);
  ipa_call_summaries = new ipa_call_summary_t (symtab);
}

ipa_call_summary::~ipa_call_summary ()
{
  if (predicate)
    edge_predicate_pool.remove (predicate);

  param.release ();
}

ipa_fn_summary::~ipa_fn_summary ()
{
  unsigned len = vec_safe_length (loop_iterations);
  for (unsigned i = 0; i < len; i++)
    edge_predicate_pool.remove ((*loop_iterations)[i].predicate);
  len = vec_safe_length (loop_strides);
  for (unsigned i = 0; i < len; i++)
    edge_predicate_pool.remove ((*loop_strides)[i].predicate);
  vec_free (conds);
  call_size_time_table.release ();
  vec_free (loop_iterations);
  vec_free (loop_strides);
  builtin_constant_p_parms.release ();
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

/* Duplicate predicates in loop hint vector, allocating memory for them and
   remove and deallocate any uninteresting (true or false) ones.  Return the
   result.  */

static vec<ipa_freqcounting_predicate, va_gc> *
remap_freqcounting_preds_after_dup (vec<ipa_freqcounting_predicate, va_gc> *v,
				    clause_t possible_truths)
{
  if (vec_safe_length (v) == 0)
    return NULL;

  vec<ipa_freqcounting_predicate, va_gc> *res = v->copy ();
  int len = res->length();
  for (int i = len - 1; i >= 0; i--)
    {
      ipa_predicate new_predicate
	= (*res)[i].predicate->remap_after_duplication (possible_truths);
      /* We do not want to free previous predicate; it is used by node
	 origin.  */
      (*res)[i].predicate = NULL;
      set_hint_predicate (&(*res)[i].predicate, new_predicate);

      if (!(*res)[i].predicate)
	res->unordered_remove (i);
    }

  return res;
}


/* Hook that is called by cgraph.cc when a node is duplicated.  */
void
ipa_fn_summary_t::duplicate (cgraph_node *src,
			     cgraph_node *dst,
			     ipa_fn_summary *src_info,
			     ipa_fn_summary *info)
{
  new (info) ipa_fn_summary (*src_info);
  /* TODO: as an optimization, we may avoid copying conditions
     that are known to be false or true.  */
  info->conds = vec_safe_copy (info->conds);

  clone_info *cinfo = clone_info::get (dst);
  /* When there are any replacements in the function body, see if we can figure
     out that something was optimized out.  */
  if (ipa_node_params_sum && cinfo && cinfo->tree_map)
    {
      /* Use SRC parm info since it may not be copied yet.  */
      ipa_node_params *parms_info = ipa_node_params_sum->get (src);
      ipa_auto_call_arg_values avals;
      int count = ipa_get_param_count (parms_info);
      int i, j;
      clause_t possible_truths;
      ipa_predicate true_pred = true;
      size_time_entry *e;
      int optimized_out_size = 0;
      bool inlined_to_p = false;
      struct cgraph_edge *edge, *next;

      info->size_time_table.release ();
      avals.m_known_vals.safe_grow_cleared (count, true);
      for (i = 0; i < count; i++)
	{
	  struct ipa_replace_map *r;

	  for (j = 0; vec_safe_iterate (cinfo->tree_map, j, &r); j++)
	    {
	      if (r->parm_num == i)
		{
		  avals.m_known_vals[i] = r->new_tree;
		  break;
		}
	    }
	}
      evaluate_conditions_for_known_args (dst, false,
					  &avals,
					  &possible_truths,
					  /* We are going to specialize,
					     so ignore nonspec truths.  */
					  NULL,
					  NULL);

      info->account_size_time (0, 0, true_pred, true_pred);

      /* Remap size_time vectors.
         Simplify the predicate by pruning out alternatives that are known
         to be false.
         TODO: as on optimization, we can also eliminate conditions known
         to be true.  */
      for (i = 0; src_info->size_time_table.iterate (i, &e); i++)
	{
	  ipa_predicate new_exec_pred;
	  ipa_predicate new_nonconst_pred;
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
	  ipa_predicate new_predicate;
	  class ipa_call_summary *es = ipa_call_summaries->get (edge);
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

      /* Remap indirect edge predicates with the same simplification as above.
         Also copy constantness arrays.   */
      for (edge = dst->indirect_calls; edge; edge = next)
	{
	  ipa_predicate new_predicate;
	  class ipa_call_summary *es = ipa_call_summaries->get (edge);
	  next = edge->next_callee;

	  gcc_checking_assert (edge->inline_failed);
	  if (!es->predicate)
	    continue;
	  new_predicate = es->predicate->remap_after_duplication
				 (possible_truths);
	  if (new_predicate == false && *es->predicate != false)
	    optimized_out_size
		 += es->call_stmt_size * ipa_fn_summary::size_scale;
	  edge_set_predicate (edge, &new_predicate);
	}
      info->loop_iterations
	= remap_freqcounting_preds_after_dup (info->loop_iterations,
					      possible_truths);
      info->loop_strides
	= remap_freqcounting_preds_after_dup (info->loop_strides,
					      possible_truths);
      if (info->builtin_constant_p_parms.length())
	{
	  vec <int, va_heap, vl_ptr> parms = info->builtin_constant_p_parms;
	  int ip;
	  info->builtin_constant_p_parms = vNULL;
	  for (i = 0; parms.iterate (i, &ip); i++)
	    if (!avals.m_known_vals[ip])
	      info->builtin_constant_p_parms.safe_push (ip);
	}

      /* If inliner or someone after inliner will ever start producing
         non-trivial clones, we will get trouble with lack of information
         about updating self sizes, because size vectors already contains
         sizes of the callees.  */
      gcc_assert (!inlined_to_p || !optimized_out_size);
    }
  else
    {
      info->size_time_table = src_info->size_time_table.copy ();
      info->loop_iterations = vec_safe_copy (src_info->loop_iterations);
      info->loop_strides = vec_safe_copy (info->loop_strides);

      info->builtin_constant_p_parms
	     = info->builtin_constant_p_parms.copy ();

      ipa_freqcounting_predicate *f;
      for (int i = 0; vec_safe_iterate (info->loop_iterations, i, &f); i++)
	{
	  ipa_predicate p = *f->predicate;
	  f->predicate = NULL;
	  set_hint_predicate (&f->predicate, p);
	}
      for (int i = 0; vec_safe_iterate (info->loop_strides, i, &f); i++)
	{
	  ipa_predicate p = *f->predicate;
	  f->predicate = NULL;
	  set_hint_predicate (&f->predicate, p);
	}
    }
  if (!dst->inlined_to)
    ipa_update_overall_fn_summary (dst);
}


/* Hook that is called by cgraph.cc when a node is duplicated.  */

void
ipa_call_summary_t::duplicate (struct cgraph_edge *src,
			       struct cgraph_edge *dst,
			       class ipa_call_summary *srcinfo,
			       class ipa_call_summary *info)
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
		       class ipa_fn_summary *info)
{
  struct cgraph_edge *edge;
  for (edge = node->callees; edge; edge = edge->next_callee)
    {
      class ipa_call_summary *es = ipa_call_summaries->get (edge);
      struct cgraph_node *callee = edge->callee->ultimate_alias_target ();
      int i;

      fprintf (f,
	       "%*s%s %s\n%*s  freq:%4.2f",
	       indent, "", callee->dump_name (),
	       !edge->inline_failed
	       ? "inlined" : cgraph_inline_failed_string (edge-> inline_failed),
	       indent, "", edge->sreal_frequency ().to_double ());

      if (cross_module_call_p (edge))
	fprintf (f, " cross module");

      if (es)
	fprintf (f, " loop depth:%2i size:%2i time: %2i",
		 es->loop_depth, es->call_stmt_size, es->call_stmt_time);

      ipa_fn_summary *s = ipa_fn_summaries->get (callee);
      ipa_size_summary *ss = ipa_size_summaries->get (callee);
      if (s != NULL)
	fprintf (f, " callee size:%2i stack:%2i",
		 (int) (ss->size / ipa_fn_summary::size_scale),
		 (int) s->estimated_stack_size);

      if (es && es->predicate)
	{
	  fprintf (f, " predicate: ");
	  es->predicate->dump (f, info->conds);
	}
      else
	fprintf (f, "\n");
      if (es && es->param.exists ())
	for (i = 0; i < (int) es->param.length (); i++)
	  {
	    int prob = es->param[i].change_prob;

	    if (!prob)
	      fprintf (f, "%*s op%i is compile time invariant\n",
		       indent + 2, "", i);
	    else if (prob != REG_BR_PROB_BASE)
	      fprintf (f, "%*s op%i change %f%% of time\n", indent + 2, "", i,
		       prob * 100.0 / REG_BR_PROB_BASE);
	    if (es->param[i].points_to_local_or_readonly_memory)
	      fprintf (f, "%*s op%i points to local or readonly memory\n",
		       indent + 2, "", i);
	    if (es->param[i].points_to_possible_sra_candidate)
	      fprintf (f, "%*s op%i points to possible sra candidate\n",
		       indent + 2, "", i);
	  }
      if (!edge->inline_failed)
	{
	  ipa_size_summary *ss = ipa_size_summaries->get (callee);
	  fprintf (f, "%*sStack frame offset %i, callee self size %i\n",
		   indent + 2, "",
		   (int) ipa_get_stack_frame_offset (callee),
		   (int) ss->estimated_self_stack_size);
	  dump_ipa_call_summary (f, indent + 2, callee, info);
	}
    }
  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
    {
      class ipa_call_summary *es = ipa_call_summaries->get (edge);
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
      class ipa_fn_summary *s = ipa_fn_summaries->get (node);
      class ipa_size_summary *ss = ipa_size_summaries->get (node);
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
	  if (s->builtin_constant_p_parms.length ())
	    {
	      fprintf (f, " builtin_constant_p_parms");
	      for (unsigned int i = 0;
		   i < s->builtin_constant_p_parms.length (); i++)
		fprintf (f, " %i", s->builtin_constant_p_parms[i]);
	    }
	  fprintf (f, "\n  global time:     %f\n", s->time.to_double ());
	  fprintf (f, "  self size:       %i\n", ss->self_size);
	  fprintf (f, "  global size:     %i\n", ss->size);
	  fprintf (f, "  min size:       %i\n", s->min_size);
	  fprintf (f, "  self stack:      %i\n",
		   (int) ss->estimated_self_stack_size);
	  fprintf (f, "  global stack:    %i\n", (int) s->estimated_stack_size);
	  if (s->growth)
	    fprintf (f, "  estimated growth:%i\n", (int) s->growth);
	  if (s->scc_no)
	    fprintf (f, "  In SCC:          %i\n", (int) s->scc_no);
	  for (i = 0; s->size_time_table.iterate (i, &e); i++)
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
	  ipa_freqcounting_predicate *fcp;
	  bool first_fcp = true;
	  for (int i = 0; vec_safe_iterate (s->loop_iterations, i, &fcp); i++)
	    {
	      if (first_fcp)
		{
		  fprintf (f, "  loop iterations:");
		  first_fcp = false;
		}
	      fprintf (f, "  %3.2f for ", fcp->freq.to_double ());
	      fcp->predicate->dump (f, s->conds);
	    }
	  first_fcp = true;
	  for (int i = 0; vec_safe_iterate (s->loop_strides, i, &fcp); i++)
	    {
	      if (first_fcp)
		{
		  fprintf (f, "  loop strides:");
		  first_fcp = false;
		}
	      fprintf (f, "  %3.2f for :", fcp->freq.to_double ());
	      fcp->predicate->dump (f, s->conds);
	    }
	  fprintf (f, "  calls:\n");
	  dump_ipa_call_summary (f, 4, node, s);
	  fprintf (f, "\n");
	  if (s->target_info)
	    fprintf (f, "  target_info: %x\n", s->target_info);
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
    if (!node->inlined_to)
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
unmodified_parm_1 (ipa_func_body_info *fbi, gimple *stmt, tree op,
		   poly_int64 *size_p)
{
  /* SSA_NAME referring to parm default def?  */
  if (TREE_CODE (op) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (op)
      && TREE_CODE (SSA_NAME_VAR (op)) == PARM_DECL)
    {
      if (size_p)
	*size_p = tree_to_poly_int64 (TYPE_SIZE (TREE_TYPE (op)));
      return SSA_NAME_VAR (op);
    }
  /* Non-SSA parm reference?  */
  if (TREE_CODE (op) == PARM_DECL
      && fbi->aa_walk_budget > 0)
    {
      bool modified = false;

      ao_ref refd;
      ao_ref_init (&refd, op);
      int walked = walk_aliased_vdefs (&refd, gimple_vuse (stmt),
				       mark_modified, &modified, NULL, NULL,
				       fbi->aa_walk_budget);
      if (walked < 0)
	{
	  fbi->aa_walk_budget = 0;
	  return NULL_TREE;
	}
      fbi->aa_walk_budget -= walked;
      if (!modified)
	{
	  if (size_p)
	    *size_p = tree_to_poly_int64 (TYPE_SIZE (TREE_TYPE (op)));
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
unmodified_parm (ipa_func_body_info *fbi, gimple *stmt, tree op,
		 poly_int64 *size_p)
{
  tree res = unmodified_parm_1 (fbi, stmt, op, size_p);
  if (res)
    return res;

  if (TREE_CODE (op) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (op)
      && gimple_assign_single_p (SSA_NAME_DEF_STMT (op)))
    return unmodified_parm (fbi, SSA_NAME_DEF_STMT (op),
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
				  poly_int64 *size_p,
				  struct agg_position_info *aggpos)
{
  tree res = unmodified_parm_1 (fbi, stmt, op, size_p);

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

/* If stmt is simple load or store of value pointed to by a function parmaeter,
   return its index.  */

static int
load_or_store_of_ptr_parameter (ipa_func_body_info *fbi, gimple *stmt)
{
  if (!optimize)
    return -1;
  gassign *assign = dyn_cast <gassign *> (stmt);
  if (!assign)
    return -1;
  tree param;
  if (gimple_assign_load_p (stmt))
    param = gimple_assign_rhs1 (stmt);
  else if (gimple_store_p (stmt))
    param = gimple_assign_lhs (stmt);
  else
    return -1;
  tree base = get_base_address (param);
  if (TREE_CODE (base) != MEM_REF
      || TREE_CODE (TREE_OPERAND (base, 0)) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (base, 0)))
    return -1;
  tree p = SSA_NAME_VAR (TREE_OPERAND (base, 0));
  if (TREE_CODE (p) != PARM_DECL)
    return -1;
  return ipa_get_param_decl_index (fbi->info, p);
}

/* See if statement might disappear after inlining.
   0 - means not eliminated
   1 - half of statements goes away
   2 - for sure it is eliminated.
   We are not terribly sophisticated, basically looking for simple abstraction
   penalty wrappers.  */

static int
eliminated_by_inlining_prob (ipa_func_body_info *fbi, gimple *stmt)
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
         inlining due to SRA and further combining.
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
	  if (unmodified_parm (fbi, stmt, inner_rhs, NULL))
	    rhs_free = true;
	  /* Match expressions of form &this->field. Those will most likely
	     combine with something upstream after inlining.  */
	  else if (TREE_CODE (inner_rhs) == ADDR_EXPR)
	    {
	      tree op = get_base_address (TREE_OPERAND (inner_rhs, 0));
	      if (TREE_CODE (op) == PARM_DECL)
		rhs_free = true;
	      else if (TREE_CODE (op) == MEM_REF
		       && unmodified_parm (fbi, stmt, TREE_OPERAND (op, 0),
					   NULL))
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
	      && unmodified_parm (fbi, stmt, TREE_OPERAND (inner_rhs, 0), NULL))
	    rhs_free = true;

	  /* Copying parameter passed by reference into gimple register is
	     probably also going to copy propagate, but we can't be quite
	     sure.  */
	  if (rhs_free && is_gimple_reg (lhs))
	    lhs_free = true;

	  /* Writes to parameters, parameters passed by value and return value
	     (either directly or passed via invisible reference) are free.  

	     TODO: We ought to handle testcase like
	     struct a {int a,b;};
	     struct a
	     returnstruct (void)
	     {
	     struct a a ={1,2};
	     return a;
	     }

	     This translate into:

	     returnstruct ()
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
		  && (unmodified_parm (fbi, stmt, TREE_OPERAND (inner_lhs, 0),
				       NULL)
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

/* Analyze EXPR if it represents a series of simple operations performed on
   a function parameter and return true if so.  FBI, STMT, EXPR, INDEX_P and
   AGGPOS have the same meaning like in unmodified_parm_or_parm_agg_item.
   Type of the parameter or load from an aggregate via the parameter is
   stored in *TYPE_P.  Operations on the parameter are recorded to
   PARAM_OPS_P if it is not NULL.  */

static bool
decompose_param_expr (struct ipa_func_body_info *fbi,
		      gimple *stmt, tree expr,
		      int *index_p, tree *type_p,
		      struct agg_position_info *aggpos,
		      expr_eval_ops *param_ops_p = NULL)
{
  int op_limit = opt_for_fn (fbi->node->decl, param_ipa_max_param_expr_ops);
  int op_count = 0;

  if (param_ops_p)
    *param_ops_p = NULL;

  while (true)
    {
      expr_eval_op eval_op;
      unsigned rhs_count;
      unsigned cst_count = 0;

      if (unmodified_parm_or_parm_agg_item (fbi, stmt, expr, index_p, NULL,
					    aggpos))
	{
	  tree type = TREE_TYPE (expr);

	  if (aggpos->agg_contents)
	    {
	      /* Stop if containing bit-field.  */
	      if (TREE_CODE (expr) == BIT_FIELD_REF
		  || contains_bitfld_component_ref_p (expr))
		break;
	    }

	  *type_p = type;
	  return true;
	}

      if (TREE_CODE (expr) != SSA_NAME || SSA_NAME_IS_DEFAULT_DEF (expr))
	break;
      stmt = SSA_NAME_DEF_STMT (expr);

      if (gcall *call = dyn_cast <gcall *> (stmt))
	{
	  int flags = gimple_call_return_flags (call);
	  if (!(flags & ERF_RETURNS_ARG))
	    goto fail;
	  int arg = flags & ERF_RETURN_ARG_MASK;
	  if (arg >= (int)gimple_call_num_args (call))
	    goto fail;
	  expr = gimple_call_arg (stmt, arg);
	  continue;
	}

      if (!is_gimple_assign (stmt = SSA_NAME_DEF_STMT (expr)))
	break;

      switch (gimple_assign_rhs_class (stmt))
	{
	case GIMPLE_SINGLE_RHS:
	  expr = gimple_assign_rhs1 (stmt);
	  continue;

	case GIMPLE_UNARY_RHS:
	  rhs_count = 1;
	  break;

	case GIMPLE_BINARY_RHS:
	  rhs_count = 2;
	  break;

	case GIMPLE_TERNARY_RHS:
	  rhs_count = 3;
	  break;

	default:
	  goto fail;
	}

      /* Stop if expression is too complex.  */
      if (op_count++ == op_limit)
	break;

      if (param_ops_p)
	{
	  eval_op.code = gimple_assign_rhs_code (stmt);
	  eval_op.type = TREE_TYPE (gimple_assign_lhs (stmt));
	  eval_op.val[0] = NULL_TREE;
	  eval_op.val[1] = NULL_TREE;
	}

      expr = NULL_TREE;
      for (unsigned i = 0; i < rhs_count; i++)
	{
	  tree op = gimple_op (stmt, i + 1);

	  gcc_assert (op && !TYPE_P (op));
	  if (is_gimple_ip_invariant (op))
	    {
	      if (++cst_count == rhs_count)
		goto fail;

	      eval_op.val[cst_count - 1] = op;
	    }
	  else if (!expr)
	    {
	      /* Found a non-constant operand, and record its index in rhs
		 operands.  */
	      eval_op.index = i;
	      expr = op;
	    }
	  else
	    {
	      /* Found more than one non-constant operands.  */
	      goto fail;
	    }
	}

      if (param_ops_p)
	vec_safe_insert (*param_ops_p, 0, eval_op);
    }

  /* Failed to decompose, free resource and return.  */
fail:
  if (param_ops_p)
    vec_free (*param_ops_p);

  return false;
}

/* Record to SUMMARY that PARM is used by builtin_constant_p.  */

static void
add_builtin_constant_p_parm (class ipa_fn_summary *summary, int parm)
{
  int ip;

  /* Avoid duplicates.  */
  for (unsigned int i = 0;
       summary->builtin_constant_p_parms.iterate (i, &ip); i++)
    if (ip == parm)
      return;
  summary->builtin_constant_p_parms.safe_push (parm);
}

/* If BB ends by a conditional we can turn into predicates, attach corresponding
   predicates to the CFG edges.   */

static void
set_cond_stmt_execution_predicate (struct ipa_func_body_info *fbi,
				   class ipa_fn_summary *summary,
				   class ipa_node_params *params_summary,
				   basic_block bb)
{
  tree op, op2;
  int index;
  struct agg_position_info aggpos;
  enum tree_code code, inverted_code;
  edge e;
  edge_iterator ei;
  gimple *set_stmt;
  tree param_type;
  expr_eval_ops param_ops;

  gcond *last = safe_dyn_cast <gcond *> (*gsi_last_bb (bb));
  if (!last)
    return;
  if (!is_gimple_ip_invariant (gimple_cond_rhs (last)))
    return;
  op = gimple_cond_lhs (last);

  if (decompose_param_expr (fbi, last, op, &index, &param_type, &aggpos,
			    &param_ops))
    {
      code = gimple_cond_code (last);
      inverted_code = invert_tree_comparison (code, HONOR_NANS (op));

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  enum tree_code this_code = (e->flags & EDGE_TRUE_VALUE
				      ? code : inverted_code);
	  /* invert_tree_comparison will return ERROR_MARK on FP
	     comparisons that are not EQ/NE instead of returning proper
	     unordered one.  Be sure it is not confused with NON_CONSTANT.

	     And if the edge's target is the final block of diamond CFG graph
	     of this conditional statement, we do not need to compute
	     predicate for the edge because the final block's predicate must
	     be at least as that of the first block of the statement.  */
	  if (this_code != ERROR_MARK
	      && !dominated_by_p (CDI_POST_DOMINATORS, bb, e->dest))
	    {
	      ipa_predicate p
		= add_condition (summary, params_summary, index,
			       	 param_type, &aggpos,
				 this_code, gimple_cond_rhs (last), param_ops);
	      e->aux = edge_predicate_pool.allocate ();
	      *(ipa_predicate *) e->aux = p;
	    }
	}
      vec_free (param_ops);
      return;
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
     optimized away when inliner doesn't see operand is constant.
     Other optimizers might think otherwise.  */
  if (gimple_cond_code (last) != NE_EXPR
      || !integer_zerop (gimple_cond_rhs (last)))
    return;
  set_stmt = SSA_NAME_DEF_STMT (op);
  if (!gimple_call_builtin_p (set_stmt, BUILT_IN_CONSTANT_P)
      || gimple_call_num_args (set_stmt) != 1)
    return;
  op2 = gimple_call_arg (set_stmt, 0);
  if (!decompose_param_expr (fbi, set_stmt, op2, &index, &param_type, &aggpos))
    return;
  if (!aggpos.by_ref)
    add_builtin_constant_p_parm (summary, index);
  FOR_EACH_EDGE (e, ei, bb->succs) if (e->flags & EDGE_FALSE_VALUE)
    {
      ipa_predicate p = add_condition (summary, params_summary, index,
		     		   param_type, &aggpos,
				   ipa_predicate::is_not_constant, NULL_TREE);
      e->aux = edge_predicate_pool.allocate ();
      *(ipa_predicate *) e->aux = p;
    }
}


/* If BB ends by a switch we can turn into predicates, attach corresponding
   predicates to the CFG edges.   */

static void
set_switch_stmt_execution_predicate (struct ipa_func_body_info *fbi,
				     class ipa_fn_summary *summary,
				     class ipa_node_params *params_summary,
				     basic_block bb)
{
  tree op;
  int index;
  struct agg_position_info aggpos;
  edge e;
  edge_iterator ei;
  size_t n;
  size_t case_idx;
  tree param_type;
  expr_eval_ops param_ops;

  gswitch *last = safe_dyn_cast <gswitch *> (*gsi_last_bb (bb));
  if (!last)
    return;
  op = gimple_switch_index (last);
  if (!decompose_param_expr (fbi, last, op, &index, &param_type, &aggpos,
			     &param_ops))
    return;

  auto_vec<std::pair<tree, tree> > ranges;
  tree type = TREE_TYPE (op);
  int bound_limit = opt_for_fn (fbi->node->decl,
				param_ipa_max_switch_predicate_bounds);
  int bound_count = 0;
  // This can safely be an integer range, as switches can only hold
  // integers.
  int_range<2> vr;

  get_range_query (cfun)->range_of_expr (vr, op);
  if (vr.undefined_p ())
    vr.set_varying (TREE_TYPE (op));
  tree vr_min, vr_max;
  // TODO: This entire function could use a rewrite to use the irange
  // API, instead of trying to recreate its intersection/union logic.
  // Any use of get_legacy_range() is a serious code smell.
  value_range_kind vr_type = get_legacy_range (vr, vr_min, vr_max);
  wide_int vr_wmin = wi::to_wide (vr_min);
  wide_int vr_wmax = wi::to_wide (vr_max);

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      e->aux = edge_predicate_pool.allocate ();
      *(ipa_predicate *) e->aux = false;
    }

  e = gimple_switch_edge (cfun, last, 0);
  /* Set BOUND_COUNT to maximum count to bypass computing predicate for
     default case if its target basic block is in convergence point of all
     switch cases, which can be determined by checking whether it
     post-dominates the switch statement.  */
  if (dominated_by_p (CDI_POST_DOMINATORS, bb, e->dest))
    bound_count = INT_MAX;

  n = gimple_switch_num_labels (last);
  for (case_idx = 1; case_idx < n; ++case_idx)
    {
      tree cl = gimple_switch_label (last, case_idx);
      tree min = CASE_LOW (cl);
      tree max = CASE_HIGH (cl);
      ipa_predicate p;

      e = gimple_switch_edge (cfun, last, case_idx);

      /* The case value might not have same type as switch expression,
	 extend the value based on the expression type.  */
      if (TREE_TYPE (min) != type)
	min = wide_int_to_tree (type, wi::to_wide (min));

      if (!max)
	max = min;
      else if (TREE_TYPE (max) != type)
	max = wide_int_to_tree (type, wi::to_wide (max));

      /* The case's target basic block is in convergence point of all switch
	 cases, its predicate should be at least as that of the switch
	 statement.  */
      if (dominated_by_p (CDI_POST_DOMINATORS, bb, e->dest))
	p = true;
      else if (min == max)
	p = add_condition (summary, params_summary, index, param_type,
		           &aggpos, EQ_EXPR, min, param_ops);
      else
	{
	  ipa_predicate p1, p2;
	  p1 = add_condition (summary, params_summary, index, param_type,
			      &aggpos, GE_EXPR, min, param_ops);
	  p2 = add_condition (summary,  params_summary,index, param_type,
			      &aggpos, LE_EXPR, max, param_ops);
	  p = p1 & p2;
	}
      *(ipa_predicate *) e->aux
	= p.or_with (summary->conds, *(ipa_predicate *) e->aux);

      /* If there are too many disjoint case ranges, predicate for default
	 case might become too complicated.  So add a limit here.  */
      if (bound_count > bound_limit)
	continue;

      bool new_range = true;

      if (!ranges.is_empty ())
	{
	  wide_int curr_wmin = wi::to_wide (min);
	  wide_int last_wmax = wi::to_wide (ranges.last ().second);

	  /* Merge case ranges if they are continuous.  */
	  if (curr_wmin == last_wmax + 1)
	    new_range = false;
	  else if (vr_type == VR_ANTI_RANGE)
	    {
	      /* If two disjoint case ranges can be connected by anti-range
		 of switch index, combine them to one range.  */
	      if (wi::lt_p (vr_wmax, curr_wmin - 1, TYPE_SIGN (type)))
		vr_type = VR_UNDEFINED;
	      else if (wi::le_p (vr_wmin, last_wmax + 1, TYPE_SIGN (type)))
		new_range = false;
	    }
	}

      /* Create/extend a case range.  And we count endpoints of range set,
	 this number nearly equals to number of conditions that we will create
	 for predicate of default case.  */
      if (new_range)
	{
	  bound_count += (min == max) ? 1 : 2;
	  ranges.safe_push (std::make_pair (min, max));
	}
      else
	{
	  bound_count += (ranges.last ().first == ranges.last ().second);
	  ranges.last ().second = max;
	}
    }

  e = gimple_switch_edge (cfun, last, 0);
  if (bound_count > bound_limit)
    {
      *(ipa_predicate *) e->aux = true;
      vec_free (param_ops);
      return;
    }

  ipa_predicate p_seg = true;
  ipa_predicate p_all = false;

  if (vr_type != VR_RANGE)
    {
      vr_wmin = wi::to_wide (TYPE_MIN_VALUE (type));
      vr_wmax = wi::to_wide (TYPE_MAX_VALUE (type));
    }

  /* Construct predicate to represent default range set that is negation of
     all case ranges.  Case range is classified as containing single/non-single
     values.  Suppose a piece of case ranges in the following.

                [D1...D2]  [S1] ... [Sn]  [D3...D4]

     To represent default case's range sets between two non-single value
     case ranges (From D2 to D3), we construct predicate as:

              D2 < x < D3 && x != S1 && ... && x != Sn
   */
  for (size_t i = 0; i < ranges.length (); i++)
    {
      tree min = ranges[i].first;
      tree max = ranges[i].second;

      if (min == max)
	p_seg &= add_condition (summary, params_summary, index,
		       		param_type, &aggpos, NE_EXPR,
				min, param_ops);
      else
	{
	  /* Do not create sub-predicate for range that is beyond low bound
	     of switch index.  */
	  if (wi::lt_p (vr_wmin, wi::to_wide (min), TYPE_SIGN (type)))
	    {
	      p_seg &= add_condition (summary, params_summary, index,
			     	      param_type, &aggpos,
				      LT_EXPR, min, param_ops);
	      p_all = p_all.or_with (summary->conds, p_seg);
	    }

	  /* Do not create sub-predicate for range that is beyond up bound
	     of switch index.  */
	  if (wi::le_p (vr_wmax, wi::to_wide (max), TYPE_SIGN (type)))
	    {
	      p_seg = false;
	      break;
	    }

	  p_seg = add_condition (summary, params_summary, index,
				 param_type, &aggpos, GT_EXPR,
				 max, param_ops);
	}
    }

  p_all = p_all.or_with (summary->conds, p_seg);
  *(ipa_predicate *) e->aux
    = p_all.or_with (summary->conds, *(ipa_predicate *) e->aux);

  vec_free (param_ops);
}


/* For each BB in NODE attach to its AUX pointer predicate under
   which it is executable.  */

static void
compute_bb_predicates (struct ipa_func_body_info *fbi,
		       struct cgraph_node *node,
		       class ipa_fn_summary *summary,
		       class ipa_node_params *params_summary)
{
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  bool done = false;
  basic_block bb;

  FOR_EACH_BB_FN (bb, my_function)
    {
      set_cond_stmt_execution_predicate (fbi, summary, params_summary, bb);
      set_switch_stmt_execution_predicate (fbi, summary, params_summary, bb);
    }

  /* Entry block is always executable.  */
  ENTRY_BLOCK_PTR_FOR_FN (my_function)->aux
    = edge_predicate_pool.allocate ();
  *(ipa_predicate *) ENTRY_BLOCK_PTR_FOR_FN (my_function)->aux = true;

  /* A simple dataflow propagation of predicates forward in the CFG.
     TODO: work in reverse postorder.  */
  while (!done)
    {
      done = true;
      FOR_EACH_BB_FN (bb, my_function)
	{
	  ipa_predicate p = false;
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      if (e->src->aux)
		{
		  ipa_predicate this_bb_predicate
		    = *(ipa_predicate *) e->src->aux;
		  if (e->aux)
		    this_bb_predicate &= (*(ipa_predicate *) e->aux);
		  p = p.or_with (summary->conds, this_bb_predicate);
		  if (p == true)
		    break;
		}
	    }
	  if (p != false)
	    {
	      basic_block pdom_bb;

	      if (!bb->aux)
		{
		  done = false;
		  bb->aux = edge_predicate_pool.allocate ();
		  *((ipa_predicate *) bb->aux) = p;
		}
	      else if (p != *(ipa_predicate *) bb->aux)
		{
		  /* This OR operation is needed to ensure monotonous data flow
		     in the case we hit the limit on number of clauses and the
		     and/or operations above give approximate answers.  */
		  p = p.or_with (summary->conds, *(ipa_predicate *)bb->aux);
		  if (p != *(ipa_predicate *)bb->aux)
		    {
		      done = false;
		      *((ipa_predicate *)bb->aux) = p;
		    }
		}

	      /* For switch/if statement, we can OR-combine predicates of all
		 its cases/branches to get predicate for basic block in their
		 convergence point, but sometimes this will generate very
		 complicated predicate.  Actually, we can get simplified
		 predicate in another way by using the fact that predicate
		 for a basic block must also hold true for its post dominators.
		 To be specific, basic block in convergence point of
		 conditional statement should include predicate of the
		 statement.  */
	      pdom_bb = get_immediate_dominator (CDI_POST_DOMINATORS, bb);
	      if (pdom_bb == EXIT_BLOCK_PTR_FOR_FN (my_function) || !pdom_bb)
		;
	      else if (!pdom_bb->aux)
		{
		  done = false;
		  pdom_bb->aux = edge_predicate_pool.allocate ();
		  *((ipa_predicate *)pdom_bb->aux) = p;
		}
	      else if (p != *(ipa_predicate *)pdom_bb->aux)
		{
		  p = p.or_with (summary->conds,
				 *(ipa_predicate *)pdom_bb->aux);
		  if (p != *(ipa_predicate *)pdom_bb->aux)
		    {
		      done = false;
		      *((ipa_predicate *)pdom_bb->aux) = p;
		    }
		}
	    }
	}
    }
}


/* Return predicate specifying when the STMT might have result that is not
   a compile time constant.  */

static ipa_predicate
will_be_nonconstant_expr_predicate (ipa_func_body_info *fbi,
				    class ipa_fn_summary *summary,
				    class ipa_node_params *params_summary,
				    tree expr,
				    vec<ipa_predicate> nonconstant_names)
{
  tree parm;
  int index;

  while (UNARY_CLASS_P (expr))
    expr = TREE_OPERAND (expr, 0);

  parm = unmodified_parm (fbi, NULL, expr, NULL);
  if (parm && (index = ipa_get_param_decl_index (fbi->info, parm)) >= 0)
    return add_condition (summary, params_summary, index, TREE_TYPE (parm), NULL,
			  ipa_predicate::changed, NULL_TREE);
  if (is_gimple_min_invariant (expr))
    return false;
  if (TREE_CODE (expr) == SSA_NAME)
    return nonconstant_names[SSA_NAME_VERSION (expr)];
  if (BINARY_CLASS_P (expr) || COMPARISON_CLASS_P (expr))
    {
      ipa_predicate p1
	= will_be_nonconstant_expr_predicate (fbi, summary,
					      params_summary,
					      TREE_OPERAND (expr, 0),
					      nonconstant_names);
      if (p1 == true)
	return p1;

      ipa_predicate p2
	= will_be_nonconstant_expr_predicate (fbi, summary,
					      params_summary,
					      TREE_OPERAND (expr, 1),
					      nonconstant_names);
      return p1.or_with (summary->conds, p2);
    }
  else if (TREE_CODE (expr) == COND_EXPR)
    {
      ipa_predicate p1
	= will_be_nonconstant_expr_predicate (fbi, summary,
					      params_summary,
					      TREE_OPERAND (expr, 0),
					      nonconstant_names);
      if (p1 == true)
	return p1;

      ipa_predicate p2
	= will_be_nonconstant_expr_predicate (fbi, summary,
					      params_summary,
					      TREE_OPERAND (expr, 1),
					      nonconstant_names);
      if (p2 == true)
	return p2;
      p1 = p1.or_with (summary->conds, p2);
      p2 = will_be_nonconstant_expr_predicate (fbi, summary,
					       params_summary,
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
}


/* Return predicate specifying when the STMT might have result that is not
   a compile time constant.  */

static ipa_predicate
will_be_nonconstant_predicate (struct ipa_func_body_info *fbi,
			       class ipa_fn_summary *summary,
			       class ipa_node_params *params_summary,
			       gimple *stmt,
			       vec<ipa_predicate> nonconstant_names)
{
  ipa_predicate p = true;
  ssa_op_iter iter;
  tree use;
  tree param_type = NULL_TREE;
  ipa_predicate op_non_const;
  bool is_load;
  int base_index;
  struct agg_position_info aggpos;

  /* What statements might be optimized away
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
      tree op = gimple_assign_rhs1 (stmt);
      if (!decompose_param_expr (fbi, stmt, op, &base_index, &param_type,
				 &aggpos))
	return p;
    }
  else
    base_index = -1;

  /* See if we understand all operands before we start
     adding conditionals.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      tree parm = unmodified_parm (fbi, stmt, use, NULL);
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
      add_condition (summary, params_summary,
		     base_index, param_type, &aggpos,
		     ipa_predicate::changed, NULL_TREE);
  else
    op_non_const = false;
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      tree parm = unmodified_parm (fbi, stmt, use, NULL);
      int index;

      if (parm && (index = ipa_get_param_decl_index (fbi->info, parm)) >= 0)
	{
	  if (index != base_index)
	    p = add_condition (summary, params_summary, index,
			       TREE_TYPE (parm), NULL,
			       ipa_predicate::changed, NULL_TREE);
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

/* Value is initialized in INIT_BB and used in USE_BB.  We want to compute
   probability how often it changes between USE_BB.
   INIT_BB->count/USE_BB->count is an estimate, but if INIT_BB
   is in different loop nest, we can do better.
   This is all just estimate.  In theory we look for minimal cut separating
   INIT_BB and USE_BB, but we only want to anticipate loop invariant motion
   anyway.  */

static basic_block
get_minimal_bb (basic_block init_bb, basic_block use_bb)
{
  class loop *l = find_common_loop (init_bb->loop_father, use_bb->loop_father);
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
param_change_prob (ipa_func_body_info *fbi, gimple *stmt, int i)
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
      if (!bb->count.nonzero_p () || fbi->aa_walk_budget == 0)
	return REG_BR_PROB_BASE;
      if (dump_file)
	{
	  fprintf (dump_file, "     Analyzing param change probability of ");
          print_generic_expr (dump_file, op, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
      ao_ref_init (&refd, op);
      info.op = op;
      info.stmt = stmt;
      info.bb_set = BITMAP_ALLOC (NULL);
      int walked
	= walk_aliased_vdefs (&refd, gimple_vuse (stmt), record_modified, &info,
			      NULL, NULL, fbi->aa_walk_budget);
      if (walked > 0)
	fbi->aa_walk_budget -= walked;
      if (walked < 0 || bitmap_bit_p (info.bb_set, bb->index))
	{
	  if (walked < 0)
	    fbi->aa_walk_budget = 0;
	  if (dump_file)
	    {
	      if (walked < 0)
		fprintf (dump_file, "     Ran out of AA walking budget.\n");
	      else
		fprintf (dump_file, "     Set in same BB as used.\n");
	    }
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
phi_result_unknown_predicate (ipa_func_body_info *fbi,
			      ipa_fn_summary *summary,
			      class ipa_node_params *params_summary,
			      basic_block bb,
			      ipa_predicate *p,
			      vec<ipa_predicate> nonconstant_names)
{
  edge e;
  edge_iterator ei;
  basic_block first_bb = NULL;

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

  gcond *stmt = safe_dyn_cast <gcond *> (*gsi_last_bb (first_bb));
  if (!stmt
      || !is_gimple_ip_invariant (gimple_cond_rhs (stmt)))
    return false;

  *p = will_be_nonconstant_expr_predicate (fbi, summary, params_summary,
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
predicate_for_phi_result (class ipa_fn_summary *summary, gphi *phi,
			  ipa_predicate *p,
			  vec<ipa_predicate> nonconstant_names)
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

   NEED_EH is used to recurse in case the clobber has non-EH predecessors
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

  /* See if all predecessors are either throws or clobber only BBs.  */
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

/* Return true if T references memory location that is local
   for the function (that means, dead after return) or read-only.  */

bool
refs_local_or_readonly_memory_p (tree t)
{
  /* Non-escaping memory is fine.  */
  t = get_base_address (t);
  if ((TREE_CODE (t) == MEM_REF
      || TREE_CODE (t) == TARGET_MEM_REF))
    return points_to_local_or_readonly_memory_p (TREE_OPERAND (t, 0));

  /* Automatic variables are fine.  */
  if (DECL_P (t)
      && auto_var_in_fn_p (t, current_function_decl))
    return true;

  /* Read-only variables are fine.  */
  if (DECL_P (t) && TREE_READONLY (t))
    return true;

  return false;
}

/* Return true if T is a pointer pointing to memory location that is local
   for the function (that means, dead after return) or read-only.  */

bool
points_to_local_or_readonly_memory_p (tree t)
{
  /* See if memory location is clearly invalid.  */
  if (integer_zerop (t))
    return flag_delete_null_pointer_checks;
  if (TREE_CODE (t) == SSA_NAME)
    {
      /* For IPA passes we can consinder accesses to return slot local
	 even if it is not local in the sense that memory is dead by
	 the end of founction.
	 The outer function will see a store in the call assignment
	 and thus this will do right thing for all uses of this
	 function in the current IPA passes (modref, pure/const discovery
	 and inlining heuristics).  */
      if (DECL_RESULT (current_function_decl)
	  && DECL_BY_REFERENCE (DECL_RESULT (current_function_decl))
	  && t == ssa_default_def (cfun, DECL_RESULT (current_function_decl)))
	return true;
      return !ptr_deref_may_alias_global_p (t, false);
    }
  if (TREE_CODE (t) == ADDR_EXPR
      && (TREE_CODE (TREE_OPERAND (t, 0)) != TARGET_MEM_REF
	  || TREE_CODE (TREE_OPERAND (TREE_OPERAND (t, 0), 0)) != INTEGER_CST))
    return refs_local_or_readonly_memory_p (TREE_OPERAND (t, 0));
  return false;
}

/* Return true if T is a pointer pointing to memory location that is possible
   sra candidate if all functions it is passed to are inlined.  */

static bool
points_to_possible_sra_candidate_p (tree t)
{
  if (TREE_CODE (t) != ADDR_EXPR)
    return false;

  t = get_base_address (TREE_OPERAND (t, 0));

  /* Automatic variables are fine.  */
  if (DECL_P (t)
      && auto_var_in_fn_p (t, current_function_decl))
    return true;
  return false;
}

/* Analyze function body for NODE.
   EARLY indicates run from early optimization pipeline.  */

static void
analyze_function_body (struct cgraph_node *node, bool early)
{
  sreal time = opt_for_fn (node->decl, param_uninlined_function_time);
  /* Estimate static overhead for function prologue/epilogue and alignment. */
  int size = opt_for_fn (node->decl, param_uninlined_function_insns);
  /* Benefits are scaled by probability of elimination that is in range
     <0,2>.  */
  basic_block bb;
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  sreal freq;
  class ipa_fn_summary *info = ipa_fn_summaries->get_create (node);
  ipa_node_params *params_summary
    = early ? NULL : ipa_node_params_sum->get (node);
  ipa_predicate bb_predicate;
  struct ipa_func_body_info fbi;
  vec<ipa_predicate> nonconstant_names = vNULL;
  int nblocks, n;
  int *order;
  gimple *fix_builtin_expect_stmt;

  gcc_assert (my_function && my_function->cfg);
  gcc_assert (cfun == my_function);

  memset(&fbi, 0, sizeof(fbi));
  vec_free (info->conds);
  info->conds = NULL;
  info->size_time_table.release ();
  info->call_size_time_table.release ();

  /* When optimizing and analyzing for IPA inliner, initialize loop optimizer
     so we can produce proper inline hints.

     When optimizing and analyzing for early inliner, initialize node params
     so we can produce correct BB predicates.  */
     
  if (opt_for_fn (node->decl, optimize))
    {
      calculate_dominance_info (CDI_DOMINATORS);
      calculate_dominance_info (CDI_POST_DOMINATORS);
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
	  fbi.info = ipa_node_params_sum->get (node);
	  fbi.bb_infos = vNULL;
	  fbi.bb_infos.safe_grow_cleared (last_basic_block_for_fn (cfun), true);
	  fbi.param_count = count_formal_params (node->decl);
	  fbi.aa_walk_budget = opt_for_fn (node->decl, param_ipa_max_aa_steps);

	  nonconstant_names.safe_grow_cleared
	    (SSANAMES (my_function)->length (), true);
	}
    }

  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function body size: %s\n",
	     node->dump_name ());

  /* When we run into maximal number of entries, we assign everything to the
     constant truth case.  Be sure to have it in list. */
  bb_predicate = true;
  info->account_size_time (0, 0, bb_predicate, bb_predicate);

  bb_predicate = ipa_predicate::not_inlined ();
  info->account_size_time (opt_for_fn (node->decl,
				param_uninlined_function_insns)
			   * ipa_fn_summary::size_scale,
			   opt_for_fn (node->decl,
				param_uninlined_function_time),
			   bb_predicate,
		           bb_predicate);

  /* Only look for target information for inlinable functions.  */
  bool scan_for_target_info =
    info->inlinable
    && targetm.target_option.need_ipa_fn_target_info (node->decl,
						      info->target_info);

  if (fbi.info)
    compute_bb_predicates (&fbi, node, info, params_summary);
  const profile_count entry_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
  order = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  nblocks = pre_and_rev_post_order_compute (NULL, order, false);
  for (n = 0; n < nblocks; n++)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, order[n]);
      freq = bb->count.to_sreal_scale (entry_count);
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
	    bb_predicate = *(ipa_predicate *)bb->aux;
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
	  ipa_predicate phi_predicate;
	  bool first_phi = true;

	  for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
	       gsi_next (&bsi))
	    {
	      if (first_phi
		  && !phi_result_unknown_predicate (&fbi, info,
			  			    params_summary,
			 			    bb,
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

      for (gimple_stmt_iterator bsi = gsi_start_nondebug_bb (bb);
	   !gsi_end_p (bsi); gsi_next_nondebug (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  int this_size = estimate_num_insns (stmt, &eni_size_weights);
	  int this_time = estimate_num_insns (stmt, &eni_time_weights);
	  int prob;
	  ipa_predicate will_be_nonconstant;

          /* This relation stmt should be folded after we remove
             __builtin_expect call. Adjust the cost here.  */
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
		  ipa_predicate false_p = false;
		  nonconstant_names[SSA_NAME_VERSION (gimple_call_lhs (stmt))]
		    = false_p;
		}
	      if (ipa_node_params_sum)
		{
		  int count = gimple_call_num_args (stmt);
		  int i;

		  if (count)
		    es->param.safe_grow_cleared (count, true);
		  for (i = 0; i < count; i++)
		    {
		      int prob = param_change_prob (&fbi, stmt, i);
		      gcc_assert (prob >= 0 && prob <= REG_BR_PROB_BASE);
		      es->param[i].change_prob = prob;
		      es->param[i].points_to_local_or_readonly_memory
			 = points_to_local_or_readonly_memory_p
			     (gimple_call_arg (stmt, i));
		      es->param[i].points_to_possible_sra_candidate
			 = points_to_possible_sra_candidate_p
			     (gimple_call_arg (stmt, i));
		    }
		}
	      /* We cannot setup VLA parameters during inlining.  */
	      for (unsigned int i = 0; i < gimple_call_num_args (stmt); ++i)
		if (TREE_CODE (gimple_call_arg (stmt, i)) == WITH_SIZE_EXPR)
		  {
		    edge->inline_failed = CIF_FUNCTION_NOT_INLINABLE;
		    break;
		  }
	      es->call_stmt_size = this_size;
	      es->call_stmt_time = this_time;
	      es->loop_depth = bb_loop_depth (bb);
	      edge_set_predicate (edge, &bb_predicate);
	      if (edge->speculative)
		{
		  cgraph_edge *indirect
			= edge->speculative_call_indirect_edge ();
	          ipa_call_summary *es2
			 = ipa_call_summaries->get_create (indirect);
		  ipa_call_summaries->duplicate (edge, indirect,
						 es, es2);

		  /* Edge is the first direct call.
		     create and duplicate call summaries for multiple
		     speculative call targets.  */
		  for (cgraph_edge *direct
			 = edge->next_speculative_call_target ();
		       direct;
		       direct = direct->next_speculative_call_target ())
		    {
		      ipa_call_summary *es3
			= ipa_call_summaries->get_create (direct);
		      ipa_call_summaries->duplicate (edge, direct,
						     es, es3);
		    }
		}
	    }

	  /* TODO: When conditional jump or switch is known to be constant, but
	     we did not translate it into the predicates, we really can account
	     just maximum of the possible paths.  */
	  if (fbi.info)
	    will_be_nonconstant
	      = will_be_nonconstant_predicate (&fbi, info, params_summary,
					       stmt, nonconstant_names);
	  else
	    will_be_nonconstant = true;
	  if (this_time || this_size)
	    {
	      sreal final_time = (sreal)this_time * freq;
	      prob = eliminated_by_inlining_prob (&fbi, stmt);
	      if (prob == 1 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "\t\t50%% will be eliminated by inlining\n");
	      if (prob == 2 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\t\tWill be eliminated by inlining\n");

	      ipa_predicate p = bb_predicate & will_be_nonconstant;
	      int parm = load_or_store_of_ptr_parameter (&fbi, stmt);
	      ipa_predicate sra_predicate = true;
	      if (parm != -1)
		sra_predicate &= add_condition (info, params_summary, parm,
						ptr_type_node, NULL,
						ipa_predicate::not_sra_candidate, NULL, 0);

	      /* We can ignore statement when we proved it is never going
		 to happen, but we cannot do that for call statements
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
		      ipa_predicate ip
			= bb_predicate & ipa_predicate::not_inlined () & sra_predicate;
		      info->account_size_time (this_size * prob,
					       (final_time * prob) / 2, ip,
					       p);
		    }
		  if (prob != 2)
		    info->account_size_time (this_size * (2 - prob),
					     (final_time * (2 - prob) / 2),
					     bb_predicate & sra_predicate,
					     p);
		}

	      if (!info->fp_expressions && fp_expression_p (stmt))
		{
		  info->fp_expressions = true;
		  if (dump_file)
		    fprintf (dump_file, "   fp_expression set\n");
		}
	    }

	  /* For target specific information, we want to scan all statements
	     rather than those statements with non-zero weights, to avoid
	     missing to scan something interesting for target information,
	     such as: internal function calls.  */
	  if (scan_for_target_info)
	    scan_for_target_info =
	      targetm.target_option.update_ipa_fn_target_info
	      (info->target_info, stmt);

	  /* Account cost of address calculations in the statements.  */
	  for (unsigned int i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      for (tree op = gimple_op (stmt, i);
		   op && handled_component_p (op);
		   op = TREE_OPERAND (op, 0))
	        if ((TREE_CODE (op) == ARRAY_REF
		     || TREE_CODE (op) == ARRAY_RANGE_REF)
		    && TREE_CODE (TREE_OPERAND (op, 1)) == SSA_NAME)
		  {
		    ipa_predicate p = bb_predicate;
		    if (fbi.info)
		      p = p & will_be_nonconstant_expr_predicate
				 (&fbi, info, params_summary,
				  TREE_OPERAND (op, 1),
			          nonconstant_names);
		    if (p != false)
		      {
			time += freq;
			size += 1;
			if (dump_file)
			  fprintf (dump_file,
				   "\t\tAccounting address calculation.\n");
			info->account_size_time (ipa_fn_summary::size_scale,
						 freq,
						 bb_predicate,
						 p);
		      }
		  }
	    }

	}
    }
  free (order);

  if (nonconstant_names.exists () && !early)
    {
      ipa_fn_summary *s = ipa_fn_summaries->get (node);
      unsigned max_loop_predicates = opt_for_fn (node->decl,
						 param_ipa_max_loop_predicates);

      if (dump_file && (dump_flags & TDF_DETAILS))
	flow_loops_dump (dump_file, NULL, 0);
      scev_initialize ();
      for (auto loop : loops_list (cfun, 0))
	{
	  ipa_predicate loop_iterations = true;
	  sreal header_freq;
	  edge ex;
	  unsigned int j;
	  class tree_niter_desc niter_desc;
	  if (!loop->header->aux)
	    continue;

	  profile_count phdr_count = loop_preheader_edge (loop)->count ();
	  sreal phdr_freq = phdr_count.to_sreal_scale (entry_count);

	  bb_predicate = *(ipa_predicate *)loop->header->aux;
	  auto_vec<edge> exits = get_loop_exit_edges (loop);
	  FOR_EACH_VEC_ELT (exits, j, ex)
	    if (number_of_iterations_exit (loop, ex, &niter_desc, false)
		&& !is_gimple_min_invariant (niter_desc.niter))
	    {
	      ipa_predicate will_be_nonconstant
		= will_be_nonconstant_expr_predicate (&fbi, info,
						      params_summary,
						      niter_desc.niter,
						      nonconstant_names);
	      if (will_be_nonconstant != true)
		will_be_nonconstant = bb_predicate & will_be_nonconstant;
	      if (will_be_nonconstant != true
		  && will_be_nonconstant != false)
		loop_iterations &= will_be_nonconstant;
	    }
	  add_freqcounting_predicate (&s->loop_iterations, loop_iterations,
				      phdr_freq, max_loop_predicates);
	}

      /* To avoid quadratic behavior we analyze stride predicates only
         with respect to the containing loop.  Thus we simply iterate
	 over all defs in the outermost loop body.  */
      for (class loop *loop = loops_for_fn (cfun)->tree_root->inner;
	   loop != NULL; loop = loop->next)
	{
	  ipa_predicate loop_stride = true;
	  basic_block *body = get_loop_body (loop);
	  profile_count phdr_count = loop_preheader_edge (loop)->count ();
	  sreal phdr_freq = phdr_count.to_sreal_scale (entry_count);
	  for (unsigned i = 0; i < loop->num_nodes; i++)
	    {
	      gimple_stmt_iterator gsi;
	      if (!body[i]->aux)
		continue;

	      bb_predicate = *(ipa_predicate *)body[i]->aux;
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

		  ipa_predicate will_be_nonconstant
		    = will_be_nonconstant_expr_predicate (&fbi, info,
				    			  params_summary,
				   			  iv.step,
							  nonconstant_names);
		  if (will_be_nonconstant != true)
		    will_be_nonconstant = bb_predicate & will_be_nonconstant;
		  if (will_be_nonconstant != true
		      && will_be_nonconstant != false)
		    loop_stride = loop_stride & will_be_nonconstant;
		}
	    }
	  add_freqcounting_predicate (&s->loop_strides, loop_stride,
				      phdr_freq, max_loop_predicates);
	  free (body);
	}
      scev_finalize ();
    }
  FOR_ALL_BB_FN (bb, my_function)
    {
      edge e;
      edge_iterator ei;

      if (bb->aux)
	edge_predicate_pool.remove ((ipa_predicate *)bb->aux);
      bb->aux = NULL;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->aux)
	    edge_predicate_pool.remove ((ipa_predicate *)e->aux);
	  e->aux = NULL;
	}
    }
  ipa_fn_summary *s = ipa_fn_summaries->get (node);
  ipa_size_summary *ss = ipa_size_summaries->get (node);
  s->time = time;
  ss->self_size = size;
  nonconstant_names.release ();
  ipa_release_body_info (&fbi);
  if (opt_for_fn (node->decl, optimize))
    {
      if (!early)
        loop_optimizer_finalize ();
      else if (!ipa_edge_args_sum)
	ipa_free_all_node_params ();
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
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

  gcc_assert (!node->inlined_to);

  if (!ipa_fn_summaries)
    ipa_fn_summary_alloc ();

  /* Create a new ipa_fn_summary.  */
  ((ipa_fn_summary_t *)ipa_fn_summaries)->remove_callees (node);
  ipa_fn_summaries->remove (node);
  class ipa_fn_summary *info = ipa_fn_summaries->get_create (node);
  class ipa_size_summary *size_info = ipa_size_summaries->get_create (node);

  /* Estimate the stack size for the function if we're optimizing.  */
  self_stack_size = optimize && !node->thunk
		    ? estimated_stack_frame_size (node) : 0;
  size_info->estimated_self_stack_size = self_stack_size;
  info->estimated_stack_size = self_stack_size;

  if (node->thunk)
    {
      ipa_call_summary *es = ipa_call_summaries->get_create (node->callees);
      ipa_predicate t = true;

      node->can_change_signature = false;
      es->call_stmt_size = eni_size_weights.call_cost;
      es->call_stmt_time = eni_time_weights.call_cost;
      info->account_size_time (ipa_fn_summary::size_scale
			       * opt_for_fn (node->decl,
				 param_uninlined_function_thunk_insns),
			       opt_for_fn (node->decl,
				 param_uninlined_function_thunk_time), t, t);
      t = ipa_predicate::not_inlined ();
      info->account_size_time (2 * ipa_fn_summary::size_scale, 0, t, t);
      ipa_update_overall_fn_summary (node);
      size_info->self_size = size_info->size;
      if (stdarg_p (TREE_TYPE (node->decl)))
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

       /* During IPA profile merging we may be called w/o virtual SSA form
	  built.  */
       update_ssa (TODO_update_ssa_only_virtuals);

       /* Can this function be inlined at all?  */
       if (!opt_for_fn (node->decl, optimize)
	   && !lookup_attribute ("always_inline",
				 DECL_ATTRIBUTES (node->decl)))
	 info->inlinable = false;
       else
	 info->inlinable = tree_inlinable_function_p (node->decl);

       bool no_signature = false;
       /* Type attributes can use parameter indices to describe them.
	  Special case fn spec since we can safely preserve them in
	  modref summaries.  */
       for (tree list = TYPE_ATTRIBUTES (TREE_TYPE (node->decl));
	    list && !no_signature; list = TREE_CHAIN (list))
	if (!ipa_param_adjustments::type_attribute_allowed_p
			(get_attribute_name (list)))
	   {
	     if (dump_file)
		{
		  fprintf (dump_file, "No signature change:"
			   " function type has unhandled attribute %s.\n",
			   IDENTIFIER_POINTER (get_attribute_name (list)));
		}
	     no_signature = true;
	   }
       for (tree parm = DECL_ARGUMENTS (node->decl);
	    parm && !no_signature; parm = DECL_CHAIN (parm))
	 if (variably_modified_type_p (TREE_TYPE (parm), node->decl))
	   {
	     if (dump_file)
		{
		  fprintf (dump_file, "No signature change:"
			   " has parameter with variably modified type.\n");
		}
	     no_signature = true;
	   }

       /* Likewise for #pragma omp declare simd functions or functions
	  with simd attribute.  */
       if (no_signature
	   || lookup_attribute ("omp declare simd",
				DECL_ATTRIBUTES (node->decl)))
	 node->can_change_signature = false;
       else
	 {
	   /* Otherwise, inlinable functions always can change signature.  */
	   if (info->inlinable)
	     node->can_change_signature = true;
	   else
	     {
	       /* Functions calling builtin_apply cannot change signature.  */
	       for (e = node->callees; e; e = e->next_callee)
		 {
		   tree cdecl = e->callee->decl;
		   if (fndecl_built_in_p (cdecl, BUILT_IN_APPLY_ARGS,
						 BUILT_IN_VA_START))
		     break;
		 }
	       node->can_change_signature = !e;
	     }
	 }
       analyze_function_body (node, early);
       pop_cfun ();
     }

  /* Inlining characteristics are maintained by the cgraph_mark_inline.  */
  size_info->size = size_info->self_size;
  info->estimated_stack_size = size_info->estimated_self_stack_size;

  /* Code above should compute exactly the same result as
     ipa_update_overall_fn_summary except for case when speculative
     edges are present since these are accounted to size but not
     self_size. Do not compare time since different order the roundoff
     errors result in slight changes.  */
  ipa_update_overall_fn_summary (node);
  if (flag_checking)
    {
      for (e = node->indirect_calls; e; e = e->next_callee)
       if (e->speculative)
	 break;
      gcc_assert (e || size_info->size == size_info->self_size);
    }
}


/* Compute parameters of functions used by inliner using
   current_function_decl.  */

static unsigned int
compute_fn_summary_for_current (void)
{
  compute_fn_summary (cgraph_node::get (current_function_decl), true);
  return 0;
}

/* Estimate benefit devirtualizing indirect edge IE and return true if it can
   be devirtualized and inlined, provided m_known_vals, m_known_contexts and
   m_known_aggs in AVALS.  Return false straight away if AVALS is NULL.  */

static bool
estimate_edge_devirt_benefit (struct cgraph_edge *ie,
			      int *size, int *time,
			      ipa_call_arg_values *avals)
{
  tree target;
  struct cgraph_node *callee;
  class ipa_fn_summary *isummary;
  enum availability avail;
  bool speculative;

  if (!avals
      || (!avals->m_known_vals.length() && !avals->m_known_contexts.length ()))
    return false;
  if (!opt_for_fn (ie->caller->decl, flag_indirect_inlining))
    return false;

  target = ipa_get_indirect_edge_target (ie, avals, &speculative);
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
  if (isummary == NULL)
    return false;

  return isummary->inlinable;
}

/* Increase SIZE, MIN_SIZE (if non-NULL) and TIME for size and time needed to
   handle edge E with probability PROB.  Set HINTS accordingly if edge may be
   devirtualized.  AVALS, if non-NULL, describes the context of the call site
   as far as values of parameters are concerened.  */

static inline void
estimate_edge_size_and_time (struct cgraph_edge *e, int *size, int *min_size,
			     sreal *time, ipa_call_arg_values *avals,
			     ipa_hints *hints)
{
  class ipa_call_summary *es = ipa_call_summaries->get (e);
  int call_size = es->call_stmt_size;
  int call_time = es->call_stmt_time;
  int cur_size;

  if (!e->callee && hints && e->maybe_hot_p ()
      && estimate_edge_devirt_benefit (e, &call_size, &call_time, avals))
    *hints |= INLINE_HINT_indirect_call;
  cur_size = call_size * ipa_fn_summary::size_scale;
  *size += cur_size;
  if (min_size)
    *min_size += cur_size;
  if (time)
    *time += ((sreal)call_time) * e->sreal_frequency ();
}


/* Increase SIZE, MIN_SIZE and TIME for size and time needed to handle all
   calls in NODE.  POSSIBLE_TRUTHS and AVALS describe the context of the call
   site.

   Helper for estimate_calls_size_and_time which does the same but
   (in most cases) faster.  */

static void
estimate_calls_size_and_time_1 (struct cgraph_node *node, int *size,
			        int *min_size, sreal *time,
			        ipa_hints *hints,
			        clause_t possible_truths,
				ipa_call_arg_values *avals)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	{
	  gcc_checking_assert (!ipa_call_summaries->get (e));
	  estimate_calls_size_and_time_1 (e->callee, size, min_size, time,
					  hints, possible_truths, avals);

	  continue;
	}
      class ipa_call_summary *es = ipa_call_summaries->get (e);

      /* Do not care about zero sized builtins.  */
      if (!es->call_stmt_size)
	{
	  gcc_checking_assert (!es->call_stmt_time);
	  continue;
	}
      if (!es->predicate
	  || es->predicate->evaluate (possible_truths))
	{
	  /* Predicates of calls shall not use NOT_CHANGED codes,
	     so we do not need to compute probabilities.  */
	  estimate_edge_size_and_time (e, size,
				       es->predicate ? NULL : min_size,
				       time, avals, hints);
	}
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      class ipa_call_summary *es = ipa_call_summaries->get (e);
      if (!es->predicate
	  || es->predicate->evaluate (possible_truths))
	estimate_edge_size_and_time (e, size,
				     es->predicate ? NULL : min_size,
				     time, avals, hints);
    }
}

/* Populate sum->call_size_time_table for edges from NODE.  */

static void
summarize_calls_size_and_time (struct cgraph_node *node,
    			       ipa_fn_summary *sum)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	{
	  gcc_checking_assert (!ipa_call_summaries->get (e));
	  summarize_calls_size_and_time (e->callee, sum);
	  continue;
	}
      int size = 0;
      sreal time = 0;

      estimate_edge_size_and_time (e, &size, NULL, &time, NULL, NULL);

      ipa_predicate pred = true;
      class ipa_call_summary *es = ipa_call_summaries->get (e);

      if (es->predicate)
	pred = *es->predicate;
      sum->account_size_time (size, time, pred, pred, true);
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      int size = 0;
      sreal time = 0;

      estimate_edge_size_and_time (e, &size, NULL, &time, NULL, NULL);
      ipa_predicate pred = true;
      class ipa_call_summary *es = ipa_call_summaries->get (e);

      if (es->predicate)
	pred = *es->predicate;
      sum->account_size_time (size, time, pred, pred, true);
    }
}

/* Increase SIZE, MIN_SIZE and TIME for size and time needed to handle all
   calls in NODE.  POSSIBLE_TRUTHS and AVALS (the latter if non-NULL) describe
   context of the call site.  */

static void
estimate_calls_size_and_time (struct cgraph_node *node, int *size,
			      int *min_size, sreal *time,
			      ipa_hints *hints,
			      clause_t possible_truths,
			      ipa_call_arg_values *avals)
{
  class ipa_fn_summary *sum = ipa_fn_summaries->get (node);
  bool use_table = true;

  gcc_assert (node->callees || node->indirect_calls);

  /* During early inlining we do not calculate info for very
     large functions and thus there is no need for producing
     summaries.  */
  if (!ipa_node_params_sum)
    use_table = false;
  /* Do not calculate summaries for simple wrappers; it is waste
     of memory.  */
  else if (node->callees && node->indirect_calls
           && node->callees->inline_failed && !node->callees->next_callee)
    use_table = false;
  /* If there is an indirect edge that may be optimized, we need
     to go the slow way.  */
  else if (avals && hints
	   && (avals->m_known_vals.length ()
	       || avals->m_known_contexts.length ()
	       || avals->m_known_aggs.length ()))
    {
      ipa_node_params *params_summary = ipa_node_params_sum->get (node);
      unsigned int nargs = params_summary
			   ? ipa_get_param_count (params_summary) : 0;

      for (unsigned int i = 0; i < nargs && use_table; i++)
	{
	  if (ipa_is_param_used_by_indirect_call (params_summary, i)
	      && (avals->safe_sval_at (i)
		  || (ipa_argagg_value_list (avals).value_for_index_p (i))))
	    use_table = false;
	  else if (ipa_is_param_used_by_polymorphic_call (params_summary, i)
		   && (avals->m_known_contexts.length () > i
		       && !avals->m_known_contexts[i].useless_p ()))
	    use_table = false;
	}
    }

  /* Fast path is via the call size time table.  */
  if (use_table)
    {
      /* Build summary if it is absent.  */
      if (!sum->call_size_time_table.length ())
	{
	  ipa_predicate true_pred = true;
	  sum->account_size_time (0, 0, true_pred, true_pred, true);
	  summarize_calls_size_and_time (node, sum);
	}

      int old_size = *size;
      sreal old_time = time ? *time : 0;

      if (min_size)
	*min_size += sum->call_size_time_table[0].size;

      unsigned int i;
      size_time_entry *e;

      /* Walk the table and account sizes and times.  */
      for (i = 0; sum->call_size_time_table.iterate (i, &e);
	   i++)
	if (e->exec_predicate.evaluate (possible_truths))
	  {
	    *size += e->size;
	    if (time)
	      *time += e->time;
	  }

      /* Be careful and see if both methods agree.  */
      if ((flag_checking || dump_file)
	  /* Do not try to sanity check when we know we lost some
	     precision.  */
	  && sum->call_size_time_table.length ()
	     < ipa_fn_summary::max_size_time_table_size)
	{
	  estimate_calls_size_and_time_1 (node, &old_size, NULL, &old_time, NULL,
					  possible_truths, avals);
	  gcc_assert (*size == old_size);
	  if (time && (*time - old_time > 1 || *time - old_time < -1)
	      && dump_file)
	    fprintf (dump_file, "Time mismatch in call summary %f!=%f\n",
		     old_time.to_double (),
		     time->to_double ());
	}
    }
  /* Slow path by walking all edges.  */
  else
    estimate_calls_size_and_time_1 (node, size, min_size, time, hints,
				    possible_truths, avals);
}

/* Main constructor for ipa call context.  Memory allocation of ARG_VALUES
   is owned by the caller.  INLINE_PARAM_SUMMARY is also owned by the
   caller.  */

ipa_call_context::ipa_call_context (cgraph_node *node, clause_t possible_truths,
				    clause_t nonspec_possible_truths,
				    vec<inline_param_summary>
				      inline_param_summary,
				    ipa_auto_call_arg_values *arg_values)
: m_node (node), m_possible_truths (possible_truths),
  m_nonspec_possible_truths (nonspec_possible_truths),
  m_inline_param_summary (inline_param_summary),
  m_avals (arg_values)
{
}

/* Set THIS to be a duplicate of CTX.  Copy all relevant info.  */

void
ipa_cached_call_context::duplicate_from (const ipa_call_context &ctx)
{
  m_node = ctx.m_node;
  m_possible_truths = ctx.m_possible_truths;
  m_nonspec_possible_truths = ctx.m_nonspec_possible_truths;
  ipa_node_params *params_summary = ipa_node_params_sum->get (m_node);
  unsigned int nargs = params_summary
		       ? ipa_get_param_count (params_summary) : 0;

  m_inline_param_summary = vNULL;
  /* Copy the info only if there is at least one useful entry.  */
  if (ctx.m_inline_param_summary.exists ())
    {
      unsigned int n = MIN (ctx.m_inline_param_summary.length (), nargs);

      for (unsigned int i = 0; i < n; i++)
	if (ipa_is_param_used_by_ipa_predicates (params_summary, i)
	    && !ctx.m_inline_param_summary[i].useless_p ())
	  {
            m_inline_param_summary
		    = ctx.m_inline_param_summary.copy ();
	    break;
	  }
    }
  m_avals.m_known_vals = vNULL;
  if (ctx.m_avals.m_known_vals.exists ())
    {
      unsigned int n = MIN (ctx.m_avals.m_known_vals.length (), nargs);

      for (unsigned int i = 0; i < n; i++)
	if (ipa_is_param_used_by_indirect_call (params_summary, i)
	    && ctx.m_avals.m_known_vals[i])
	  {
	    m_avals.m_known_vals = ctx.m_avals.m_known_vals.copy ();
	    break;
	  }
    }

  m_avals.m_known_contexts = vNULL;
  if (ctx.m_avals.m_known_contexts.exists ())
    {
      unsigned int n = MIN (ctx.m_avals.m_known_contexts.length (), nargs);

      for (unsigned int i = 0; i < n; i++)
	if (ipa_is_param_used_by_polymorphic_call (params_summary, i)
	    && !ctx.m_avals.m_known_contexts[i].useless_p ())
	  {
	    m_avals.m_known_contexts = ctx.m_avals.m_known_contexts.copy ();
	    break;
	  }
    }

  m_avals.m_known_aggs = vNULL;
  if (ctx.m_avals.m_known_aggs.exists ())
    {
      const ipa_argagg_value_list avl (&ctx.m_avals);
      for (unsigned int i = 0; i < nargs; i++)
	if (ipa_is_param_used_by_indirect_call (params_summary, i)
	    && avl.value_for_index_p (i))
	  {
	    m_avals.m_known_aggs = ctx.m_avals.m_known_aggs.copy ();
	    break;
	  }
    }

  m_avals.m_known_value_ranges = vNULL;
}

/* Release memory used by known_vals/contexts/aggs vectors.  and
   inline_param_summary.  */

void
ipa_cached_call_context::release ()
{
  /* See if context is initialized at first place.  */
  if (!m_node)
    return;
  m_avals.m_known_aggs.release ();
  m_avals.m_known_vals.release ();
  m_avals.m_known_contexts.release ();
  m_inline_param_summary.release ();
}

/* Return true if CTX describes the same call context as THIS.  */

bool
ipa_call_context::equal_to (const ipa_call_context &ctx)
{
  if (m_node != ctx.m_node
      || m_possible_truths != ctx.m_possible_truths
      || m_nonspec_possible_truths != ctx.m_nonspec_possible_truths)
    return false;

  ipa_node_params *params_summary = ipa_node_params_sum->get (m_node);
  unsigned int nargs = params_summary
		       ? ipa_get_param_count (params_summary) : 0;

  if (m_inline_param_summary.exists () || ctx.m_inline_param_summary.exists ())
    {
      for (unsigned int i = 0; i < nargs; i++)
	{
	  if (!ipa_is_param_used_by_ipa_predicates (params_summary, i))
	    continue;
	  if (i >= m_inline_param_summary.length ()
	      || m_inline_param_summary[i].useless_p ())
	    {
	      if (i < ctx.m_inline_param_summary.length ()
		  && !ctx.m_inline_param_summary[i].useless_p ())
		return false;
	      continue;
	    }
	  if (i >= ctx.m_inline_param_summary.length ()
	      || ctx.m_inline_param_summary[i].useless_p ())
	    {
	      if (i < m_inline_param_summary.length ()
		  && !m_inline_param_summary[i].useless_p ())
		return false;
	      continue;
	    }
	  if (!m_inline_param_summary[i].equal_to
	     	 (ctx.m_inline_param_summary[i]))
	    return false;
	}
    }
  if (m_avals.m_known_vals.exists () || ctx.m_avals.m_known_vals.exists ())
    {
      for (unsigned int i = 0; i < nargs; i++)
	{
	  if (!ipa_is_param_used_by_indirect_call (params_summary, i))
	    continue;
	  if (i >= m_avals.m_known_vals.length () || !m_avals.m_known_vals[i])
	    {
	      if (i < ctx.m_avals.m_known_vals.length ()
		  && ctx.m_avals.m_known_vals[i])
		return false;
	      continue;
	    }
	  if (i >= ctx.m_avals.m_known_vals.length ()
	      || !ctx.m_avals.m_known_vals[i])
	    {
	      if (i < m_avals.m_known_vals.length () && m_avals.m_known_vals[i])
		return false;
	      continue;
	    }
	  if (m_avals.m_known_vals[i] != ctx.m_avals.m_known_vals[i])
	    return false;
	}
    }
  if (m_avals.m_known_contexts.exists ()
      || ctx.m_avals.m_known_contexts.exists ())
    {
      for (unsigned int i = 0; i < nargs; i++)
	{
	  if (!ipa_is_param_used_by_polymorphic_call (params_summary, i))
	    continue;
	  if (i >= m_avals.m_known_contexts.length ()
	      || m_avals.m_known_contexts[i].useless_p ())
	    {
	      if (i < ctx.m_avals.m_known_contexts.length ()
		  && !ctx.m_avals.m_known_contexts[i].useless_p ())
		return false;
	      continue;
	    }
	  if (i >= ctx.m_avals.m_known_contexts.length ()
	      || ctx.m_avals.m_known_contexts[i].useless_p ())
	    {
	      if (i < m_avals.m_known_contexts.length ()
		  && !m_avals.m_known_contexts[i].useless_p ())
		return false;
	      continue;
	    }
	  if (!m_avals.m_known_contexts[i].equal_to
	     	 (ctx.m_avals.m_known_contexts[i]))
	    return false;
	}
    }
  if (m_avals.m_known_aggs.exists () || ctx.m_avals.m_known_aggs.exists ())
    {
      unsigned i = 0, j = 0;
      while (i < m_avals.m_known_aggs.length ()
	     || j < ctx.m_avals.m_known_aggs.length ())
	{
	  if (i >= m_avals.m_known_aggs.length ())
	    {
	      int idx2 = ctx.m_avals.m_known_aggs[j].index;
	      if (ipa_is_param_used_by_indirect_call (params_summary, idx2))
		return false;
	      j++;
	      continue;
	    }
	  if (j >= ctx.m_avals.m_known_aggs.length ())
	    {
	      int idx1 = m_avals.m_known_aggs[i].index;
	      if (ipa_is_param_used_by_indirect_call (params_summary, idx1))
		return false;
	      i++;
	      continue;
	    }

	  int idx1 = m_avals.m_known_aggs[i].index;
	  int idx2 = ctx.m_avals.m_known_aggs[j].index;
	  if (idx1 < idx2)
	    {
	      if (ipa_is_param_used_by_indirect_call (params_summary, idx1))
		return false;
	      i++;
	      continue;
	    }
	  if (idx1 > idx2)
	    {
	      if (ipa_is_param_used_by_indirect_call (params_summary, idx2))
		return false;
	      j++;
	      continue;
	    }
	  if (!ipa_is_param_used_by_indirect_call (params_summary, idx1))
	    {
	      i++;
	      j++;
	      continue;
	    }

	  if ((m_avals.m_known_aggs[i].unit_offset
	       != ctx.m_avals.m_known_aggs[j].unit_offset)
	      || (m_avals.m_known_aggs[i].by_ref
	       != ctx.m_avals.m_known_aggs[j].by_ref)
	      || !operand_equal_p (m_avals.m_known_aggs[i].value,
				   ctx.m_avals.m_known_aggs[j].value))
	    return false;
	  i++;
	  j++;
	}
    }
  return true;
}

/* Fill in the selected fields in ESTIMATES with value estimated for call in
   this context.  Always compute size and min_size.  Only compute time and
   nonspecialized_time if EST_TIMES is true.  Only compute hints if EST_HINTS
   is true.  */

void
ipa_call_context::estimate_size_and_time (ipa_call_estimates *estimates,
					  bool est_times, bool est_hints)
{
  class ipa_fn_summary *info = ipa_fn_summaries->get (m_node);
  size_time_entry *e;
  int size = 0;
  sreal time = 0;
  int min_size = 0;
  ipa_hints hints = 0;
  sreal loops_with_known_iterations = 0;
  sreal loops_with_known_strides = 0;
  int i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      bool found = false;
      fprintf (dump_file, "   Estimating body: %s\n"
	       "   Known to be false: ", m_node->dump_name ());

      for (i = ipa_predicate::not_inlined_condition;
	   i < (ipa_predicate::first_dynamic_condition
		+ (int) vec_safe_length (info->conds)); i++)
	if (!(m_possible_truths & (1 << i)))
	  {
	    if (found)
	      fprintf (dump_file, ", ");
	    found = true;
	    dump_condition (dump_file, info->conds, i);
	  }
    }

  if (m_node->callees || m_node->indirect_calls)
    estimate_calls_size_and_time (m_node, &size, &min_size,
				  est_times ? &time : NULL,
				  est_hints ? &hints : NULL, m_possible_truths,
				  &m_avals);

  sreal nonspecialized_time = time;

  min_size += info->size_time_table[0].size;
  for (i = 0; info->size_time_table.iterate (i, &e); i++)
    {
      bool exec = e->exec_predicate.evaluate (m_nonspec_possible_truths);

      /* Because predicates are conservative, it can happen that nonconst is 1
	 but exec is 0.  */
      if (exec)
        {
          bool nonconst = e->nonconst_predicate.evaluate (m_possible_truths);

	  gcc_checking_assert (e->time >= 0);
	  gcc_checking_assert (time >= 0);

	  /* We compute specialized size only because size of nonspecialized
	     copy is context independent.

	     The difference between nonspecialized execution and specialized is
	     that nonspecialized is not going to have optimized out computations
	     known to be constant in a specialized setting.  */
	  if (nonconst)
	    size += e->size;
	  if (!est_times)
	    continue;
	  nonspecialized_time += e->time;
	  if (!nonconst)
	    ;
	  else if (!m_inline_param_summary.exists ())
	    {
	      if (nonconst)
	        time += e->time;
	    }
	  else
	    {
	      int prob = e->nonconst_predicate.probability 
					       (info->conds, m_possible_truths,
					        m_inline_param_summary);
	      gcc_checking_assert (prob >= 0);
	      gcc_checking_assert (prob <= REG_BR_PROB_BASE);
	      if (prob == REG_BR_PROB_BASE)
	        time += e->time;
	      else
	        time += e->time * prob / REG_BR_PROB_BASE;
	    }
	  gcc_checking_assert (time >= 0);
        }
     }
  gcc_checking_assert (info->size_time_table[0].exec_predicate == true);
  gcc_checking_assert (info->size_time_table[0].nonconst_predicate == true);
  gcc_checking_assert (min_size >= 0);
  gcc_checking_assert (size >= 0);
  gcc_checking_assert (time >= 0);
  /* nonspecialized_time should be always bigger than specialized time.
     Roundoff issues however may get into the way.  */
  gcc_checking_assert ((nonspecialized_time - time * 99 / 100) >= -1);

  /* Roundoff issues may make specialized time bigger than nonspecialized
     time.  We do not really want that to happen because some heuristics
     may get confused by seeing negative speedups.  */
  if (time > nonspecialized_time)
    time = nonspecialized_time;

  if (est_hints)
    {
      if (info->scc_no)
	hints |= INLINE_HINT_in_scc;
      if (DECL_DECLARED_INLINE_P (m_node->decl))
	hints |= INLINE_HINT_declared_inline;
      if (info->builtin_constant_p_parms.length ()
	  && DECL_DECLARED_INLINE_P (m_node->decl))
	hints |= INLINE_HINT_builtin_constant_p;

      ipa_freqcounting_predicate *fcp;
      for (i = 0; vec_safe_iterate (info->loop_iterations, i, &fcp); i++)
	if (!fcp->predicate->evaluate (m_possible_truths))
	  {
	    hints |= INLINE_HINT_loop_iterations;
	    loops_with_known_iterations += fcp->freq;
	  }
      estimates->loops_with_known_iterations = loops_with_known_iterations;

      for (i = 0; vec_safe_iterate (info->loop_strides, i, &fcp); i++)
	if (!fcp->predicate->evaluate (m_possible_truths))
	  {
	    hints |= INLINE_HINT_loop_stride;
	    loops_with_known_strides += fcp->freq;
	  }
      estimates->loops_with_known_strides = loops_with_known_strides;
    }

  size = RDIV (size, ipa_fn_summary::size_scale);
  min_size = RDIV (min_size, ipa_fn_summary::size_scale);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\n   size:%i", (int) size);
      if (est_times)
	fprintf (dump_file, " time:%f nonspec time:%f",
		 time.to_double (), nonspecialized_time.to_double ());
      if (est_hints)
	fprintf (dump_file, " loops with known iterations:%f "
		 "known strides:%f", loops_with_known_iterations.to_double (),
		 loops_with_known_strides.to_double ());
      fprintf (dump_file, "\n");
    }
  if (est_times)
    {
      estimates->time = time;
      estimates->nonspecialized_time = nonspecialized_time;
    }
  estimates->size = size;
  estimates->min_size = min_size;
  if (est_hints)
    estimates->hints = hints;
  return;
}


/* Estimate size and time needed to execute callee of EDGE assuming that
   parameters known to be constant at caller of EDGE are propagated.
   KNOWN_VALS and KNOWN_CONTEXTS are vectors of assumed known constant values
   and types for parameters.  */

void
estimate_ipcp_clone_size_and_time (struct cgraph_node *node,
				   ipa_auto_call_arg_values *avals,
				   ipa_call_estimates *estimates)
{
  clause_t clause, nonspec_clause;

  evaluate_conditions_for_known_args (node, false, avals, &clause,
				      &nonspec_clause, NULL);
  ipa_call_context ctx (node, clause, nonspec_clause, vNULL, avals);
  ctx.estimate_size_and_time (estimates);
}

/* Return stack frame offset where frame of NODE is supposed to start inside
   of the function it is inlined to.
   Return 0 for functions that are not inlined.  */

HOST_WIDE_INT
ipa_get_stack_frame_offset (struct cgraph_node *node)
{
  HOST_WIDE_INT offset = 0;
  if (!node->inlined_to)
    return 0;
  node = node->callers->caller;
  while (true)
    {
      offset += ipa_size_summaries->get (node)->estimated_self_stack_size;
      if (!node->inlined_to)
	return offset;
      node = node->callers->caller;
    }
}


/* Update summary information of inline clones after inlining.
   Compute peak stack usage.  */

static void
inline_update_callee_summaries (struct cgraph_node *node, int depth)
{
  struct cgraph_edge *e;

  ipa_propagate_frequency (node);
  for (e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	inline_update_callee_summaries (e->callee, depth);
      else
	ipa_call_summaries->get (e)->loop_depth += depth;
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    ipa_call_summaries->get (e)->loop_depth += depth;
}

/* Update change_prob and points_to_local_or_readonly_memory of EDGE after
   INLINED_EDGE has been inlined.

   When function A is inlined in B and A calls C with parameter that
   changes with probability PROB1 and C is known to be passthrough
   of argument if B that change with probability PROB2, the probability
   of change is now PROB1*PROB2.  */

static void
remap_edge_params (struct cgraph_edge *inlined_edge,
		   struct cgraph_edge *edge)
{
  if (ipa_node_params_sum)
    {
      int i;
      ipa_edge_args *args = ipa_edge_args_sum->get (edge);
      if (!args)
	return;
      class ipa_call_summary *es = ipa_call_summaries->get (edge);
      class ipa_call_summary *inlined_es
	= ipa_call_summaries->get (inlined_edge);

      if (es->param.length () == 0)
	return;

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

		  if (inlined_es
			->param[id].points_to_local_or_readonly_memory)
		    es->param[i].points_to_local_or_readonly_memory = true;
		  if (inlined_es
			->param[id].points_to_possible_sra_candidate)
		    es->param[i].points_to_possible_sra_candidate = true;
		}
	      if (!es->param[i].points_to_local_or_readonly_memory
		  && jfunc->type == IPA_JF_CONST
		  && points_to_local_or_readonly_memory_p
			 (ipa_get_jf_constant (jfunc)))
		es->param[i].points_to_local_or_readonly_memory = true;
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
		      class ipa_fn_summary *info,
		      class ipa_node_params *params_summary,
		      class ipa_fn_summary *callee_info,
		      const vec<int> &operand_map,
		      const vec<HOST_WIDE_INT> &offset_map,
		      clause_t possible_truths,
		      ipa_predicate *toplev_predicate)
{
  struct cgraph_edge *e, *next;
  for (e = node->callees; e; e = next)
    {
      ipa_predicate p;
      next = e->next_callee;

      if (e->inline_failed)
	{
          class ipa_call_summary *es = ipa_call_summaries->get (e);
	  remap_edge_params (inlined_edge, e);

	  if (es->predicate)
	    {
	      p = es->predicate->remap_after_inlining
				     (info, params_summary,
				      callee_info, operand_map,
				      offset_map, possible_truths,
				      *toplev_predicate);
	      edge_set_predicate (e, &p);
	    }
	  else
	    edge_set_predicate (e, toplev_predicate);
	}
      else
	remap_edge_summaries (inlined_edge, e->callee, info,
		              params_summary, callee_info,
			      operand_map, offset_map, possible_truths,
			      toplev_predicate);
    }
  for (e = node->indirect_calls; e; e = next)
    {
      class ipa_call_summary *es = ipa_call_summaries->get (e);
      ipa_predicate p;
      next = e->next_callee;

      remap_edge_params (inlined_edge, e);
      if (es->predicate)
	{
	  p = es->predicate->remap_after_inlining
				 (info, params_summary,
				  callee_info, operand_map, offset_map,
			          possible_truths, *toplev_predicate);
	  edge_set_predicate (e, &p);
	}
      else
	edge_set_predicate (e, toplev_predicate);
    }
}

/* Run remap_after_inlining on each predicate in V.  */

static void
remap_freqcounting_predicate (class ipa_fn_summary *info,
			      class ipa_node_params *params_summary,
			      class ipa_fn_summary *callee_info,
			      vec<ipa_freqcounting_predicate, va_gc> *v,
			      const vec<int> &operand_map,
			      const vec<HOST_WIDE_INT> &offset_map,
			      clause_t possible_truths,
			      ipa_predicate *toplev_predicate)

{
  ipa_freqcounting_predicate *fcp;
  for (int i = 0; vec_safe_iterate (v, i, &fcp); i++)
    {
      ipa_predicate p
	= fcp->predicate->remap_after_inlining (info, params_summary,
						callee_info, operand_map,
						offset_map, possible_truths,
						*toplev_predicate);
      if (p != false && p != true)
	*fcp->predicate &= p;
    }
}

/* We inlined EDGE.  Update summary of the function we inlined into.  */

void
ipa_merge_fn_summary_after_inlining (struct cgraph_edge *edge)
{
  ipa_fn_summary *callee_info = ipa_fn_summaries->get (edge->callee);
  struct cgraph_node *to = (edge->caller->inlined_to
			    ? edge->caller->inlined_to : edge->caller);
  class ipa_fn_summary *info = ipa_fn_summaries->get (to);
  clause_t clause = 0;	/* not_inline is known to be false.  */
  size_time_entry *e;
  auto_vec<int, 8> operand_map;
  auto_vec<HOST_WIDE_INT, 8> offset_map;
  int i;
  ipa_predicate toplev_predicate;
  class ipa_call_summary *es = ipa_call_summaries->get (edge);
  ipa_node_params *params_summary = (ipa_node_params_sum
				     ? ipa_node_params_sum->get (to) : NULL);

  if (es->predicate)
    toplev_predicate = *es->predicate;
  else
    toplev_predicate = true;

  info->fp_expressions |= callee_info->fp_expressions;
  info->target_info |= callee_info->target_info;

  if (callee_info->conds)
    {
      ipa_auto_call_arg_values avals;
      evaluate_properties_for_edge (edge, true, &clause, NULL, &avals, false);
    }
  if (ipa_node_params_sum && callee_info->conds)
    {
      ipa_edge_args *args = ipa_edge_args_sum->get (edge);
      int count = args ? ipa_get_cs_argument_count (args) : 0;
      int i;

      if (count)
	{
	  operand_map.safe_grow_cleared (count, true);
	  offset_map.safe_grow_cleared (count, true);
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
	  gcc_assert (map < ipa_get_param_count (params_summary));
	}

      int ip;
      for (i = 0; callee_info->builtin_constant_p_parms.iterate (i, &ip); i++)
	if (ip < count && operand_map[ip] >= 0)
	  add_builtin_constant_p_parm (info, operand_map[ip]);
    }
  sreal freq = edge->sreal_frequency ();
  for (i = 0; callee_info->size_time_table.iterate (i, &e); i++)
    {
      ipa_predicate p;
      p = e->exec_predicate.remap_after_inlining
			     (info, params_summary,
			      callee_info, operand_map,
			      offset_map, clause,
			      toplev_predicate);
      ipa_predicate nonconstp;
      nonconstp = e->nonconst_predicate.remap_after_inlining
				     (info, params_summary,
				      callee_info, operand_map,
				      offset_map, clause,
				      toplev_predicate);
      if (p != false && nonconstp != false)
	{
	  sreal add_time = ((sreal)e->time * freq);
	  int prob = e->nonconst_predicate.probability (callee_info->conds,
							clause, es->param);
	  if (prob != REG_BR_PROB_BASE)
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
  remap_edge_summaries (edge, edge->callee, info, params_summary,
		 	callee_info, operand_map,
			offset_map, clause, &toplev_predicate);
  remap_freqcounting_predicate (info, params_summary, callee_info,
				info->loop_iterations, operand_map,
				offset_map, clause, &toplev_predicate);
  remap_freqcounting_predicate (info, params_summary, callee_info,
				info->loop_strides, operand_map,
				offset_map, clause, &toplev_predicate);

  HOST_WIDE_INT stack_frame_offset = ipa_get_stack_frame_offset (edge->callee);
  HOST_WIDE_INT peak = stack_frame_offset + callee_info->estimated_stack_size;

  if (info->estimated_stack_size < peak)
    info->estimated_stack_size = peak;

  inline_update_callee_summaries (edge->callee, es->loop_depth);
  if (info->call_size_time_table.length ())
    {
      int edge_size = 0;
      sreal edge_time = 0;

      estimate_edge_size_and_time (edge, &edge_size, NULL, &edge_time, NULL, 0);
      /* Unaccount size and time of the optimized out call.  */
      info->account_size_time (-edge_size, -edge_time,
	 		       es->predicate ? *es->predicate : true,
	 		       es->predicate ? *es->predicate : true,
			       true);
      /* Account new calls.  */
      summarize_calls_size_and_time (edge->callee, info);
    }

  /* Free summaries that are not maintained for inline clones/edges.  */
  ipa_call_summaries->remove (edge);
  ipa_fn_summaries->remove (edge->callee);
  ipa_remove_from_growth_caches (edge);
}

/* For performance reasons ipa_merge_fn_summary_after_inlining is not updating
   overall size and time.  Recompute it.
   If RESET is true also recompute call_time_size_table.  */

void
ipa_update_overall_fn_summary (struct cgraph_node *node, bool reset)
{
  class ipa_fn_summary *info = ipa_fn_summaries->get (node);
  class ipa_size_summary *size_info = ipa_size_summaries->get (node);
  size_time_entry *e;
  int i;

  size_info->size = 0;
  info->time = 0;
  for (i = 0; info->size_time_table.iterate (i, &e); i++)
    {
      size_info->size += e->size;
      info->time += e->time;
    }
  info->min_size = info->size_time_table[0].size;
  if (reset)
    info->call_size_time_table.release ();
  if (node->callees || node->indirect_calls)
    estimate_calls_size_and_time (node, &size_info->size, &info->min_size,
				  &info->time, NULL,
				  ~(clause_t) (1 << ipa_predicate::false_condition),
				  NULL);
  size_info->size = RDIV (size_info->size, ipa_fn_summary::size_scale);
  info->min_size = RDIV (info->min_size, ipa_fn_summary::size_scale);
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
    fprintf (dump_file, "\nAnalyzing function: %s\n", node->dump_name ());
  if (opt_for_fn (node->decl, optimize) && !node->thunk)
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
      node->versionable = tree_versionable_function_p (node->decl);

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
read_ipa_call_summary (class lto_input_block *ib, struct cgraph_edge *e,
		       bool prevails)
{
  class ipa_call_summary *es = prevails
				? ipa_call_summaries->get_create (e) : NULL;
  ipa_predicate p;
  int length, i;

  int size = streamer_read_uhwi (ib);
  int time = streamer_read_uhwi (ib);
  int depth = streamer_read_uhwi (ib);

  if (es)
    {
      es->call_stmt_size = size;
      es->call_stmt_time = time;
      es->loop_depth = depth;
    }

  bitpack_d bp = streamer_read_bitpack (ib);
  if (es)
    es->is_return_callee_uncaptured = bp_unpack_value (&bp, 1);	
  else
    bp_unpack_value (&bp, 1);	

  p.stream_in (ib);
  if (es)
    edge_set_predicate (e, &p);
  length = streamer_read_uhwi (ib);
  if (length && es
      && (e->possibly_call_in_translation_unit_p ()
	  /* Also stream in jump functions to builtins in hope that they
	     will get fnspecs.  */
	  || fndecl_built_in_p (e->callee->decl, BUILT_IN_NORMAL)))
    {
      es->param.safe_grow_cleared (length, true);
      for (i = 0; i < length; i++)
	{
	  es->param[i].change_prob = streamer_read_uhwi (ib);
	  bitpack_d bp = streamer_read_bitpack (ib);
	  es->param[i].points_to_local_or_readonly_memory
	    = bp_unpack_value (&bp, 1);	
	  es->param[i].points_to_possible_sra_candidate
	    = bp_unpack_value (&bp, 1);	
	}
    }
  else
    {
      for (i = 0; i < length; i++)
	{
	  streamer_read_uhwi (ib);
	  streamer_read_uhwi (ib);
	}
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
  class data_in *data_in;
  unsigned int i, count2, j;
  unsigned int f_count;

  lto_input_block ib ((const char *) data + main_offset, header->main_size,
		      file_data);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, vNULL);
  f_count = streamer_read_uhwi (&ib);
  for (i = 0; i < f_count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      class ipa_fn_summary *info;
      class ipa_node_params *params_summary;
      class ipa_size_summary *size_info;
      lto_symtab_encoder_t encoder;
      struct bitpack_d bp;
      struct cgraph_edge *e;
      ipa_predicate p;

      index = streamer_read_uhwi (&ib);
      encoder = file_data->symtab_node_encoder;
      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref (encoder,
								index));
      info = node->prevailing_p () ? ipa_fn_summaries->get_create (node) : NULL;
      params_summary = node->prevailing_p ()
	               ? ipa_node_params_sum->get (node) : NULL;
      size_info = node->prevailing_p ()
		  ? ipa_size_summaries->get_create (node) : NULL;

      int stack_size = streamer_read_uhwi (&ib);
      int size = streamer_read_uhwi (&ib);
      sreal time = sreal::stream_in (&ib);

      if (info)
	{
	  info->estimated_stack_size
	    = size_info->estimated_self_stack_size = stack_size;
	  size_info->size = size_info->self_size = size;
	  info->time = time;
	}

      bp = streamer_read_bitpack (&ib);
      if (info)
	{
	  info->inlinable = bp_unpack_value (&bp, 1);
	  info->fp_expressions = bp_unpack_value (&bp, 1);
	  if (!lto_stream_offload_p)
	    info->target_info = streamer_read_uhwi (&ib);
	}
      else
	{
	  bp_unpack_value (&bp, 1);
	  bp_unpack_value (&bp, 1);
	  if (!lto_stream_offload_p)
	    streamer_read_uhwi (&ib);
	}

      count2 = streamer_read_uhwi (&ib);
      gcc_assert (!info || !info->conds);
      if (info)
        vec_safe_reserve_exact (info->conds, count2);
      for (j = 0; j < count2; j++)
	{
	  struct condition c;
	  unsigned int k, count3;
	  c.operand_num = streamer_read_uhwi (&ib);
	  c.code = (enum tree_code) streamer_read_uhwi (&ib);
	  c.type = stream_read_tree (&ib, data_in);
	  c.val = stream_read_tree (&ib, data_in);
	  bp = streamer_read_bitpack (&ib);
	  c.agg_contents = bp_unpack_value (&bp, 1);
	  c.by_ref = bp_unpack_value (&bp, 1);
	  if (c.agg_contents)
	    c.offset = streamer_read_uhwi (&ib);
	  count3 = streamer_read_uhwi (&ib);
	  c.param_ops = NULL;
	  if (info)
	    vec_safe_reserve_exact (c.param_ops, count3);
	  if (params_summary)
	    ipa_set_param_used_by_ipa_predicates
		    (params_summary, c.operand_num, true);
	  for (k = 0; k < count3; k++)
	    {
	      struct expr_eval_op op;
	      enum gimple_rhs_class rhs_class;
	      op.code = (enum tree_code) streamer_read_uhwi (&ib);
	      op.type = stream_read_tree (&ib, data_in);
	      switch (rhs_class = get_gimple_rhs_class (op.code))
		{
		case GIMPLE_UNARY_RHS:
		  op.index = 0;
		  op.val[0] = NULL_TREE;
		  op.val[1] = NULL_TREE;
		  break;

		case GIMPLE_BINARY_RHS:
		case GIMPLE_TERNARY_RHS:
		  bp = streamer_read_bitpack (&ib);
		  op.index = bp_unpack_value (&bp, 2);
		  op.val[0] = stream_read_tree (&ib, data_in);
		  if (rhs_class == GIMPLE_BINARY_RHS)
		    op.val[1] = NULL_TREE;
		  else
		    op.val[1] = stream_read_tree (&ib, data_in);
		  break;

		default:
		  fatal_error (UNKNOWN_LOCATION,
			       "invalid fnsummary in LTO stream");
		}
	      if (info)
	        c.param_ops->quick_push (op);
	    }
	  if (info)
	    info->conds->quick_push (c);
	}
      count2 = streamer_read_uhwi (&ib);
      gcc_assert (!info || !info->size_time_table.length ());
      if (info && count2)
	info->size_time_table.reserve_exact (count2);
      for (j = 0; j < count2; j++)
	{
	  class size_time_entry e;

	  e.size = streamer_read_uhwi (&ib);
	  e.time = sreal::stream_in (&ib);
	  e.exec_predicate.stream_in (&ib);
	  e.nonconst_predicate.stream_in (&ib);

	  if (info)
	    info->size_time_table.quick_push (e);
	}

      count2 = streamer_read_uhwi (&ib);
      for (j = 0; j < count2; j++)
	{
	  p.stream_in (&ib);
	  sreal fcp_freq = sreal::stream_in (&ib);
	  if (info)
	    {
	      ipa_freqcounting_predicate fcp;
	      fcp.predicate = NULL;
	      set_hint_predicate (&fcp.predicate, p);
	      fcp.freq = fcp_freq;
	      vec_safe_push (info->loop_iterations, fcp);
	    }
	}
      count2 = streamer_read_uhwi (&ib);
      for (j = 0; j < count2; j++)
	{
	  p.stream_in (&ib);
	  sreal fcp_freq = sreal::stream_in (&ib);
	  if (info)
	    {
	      ipa_freqcounting_predicate fcp;
	      fcp.predicate = NULL;
	      set_hint_predicate (&fcp.predicate, p);
	      fcp.freq = fcp_freq;
	      vec_safe_push (info->loop_strides, fcp);
	    }
	}
      count2 = streamer_read_uhwi (&ib);
      if (info && count2)
	info->builtin_constant_p_parms.reserve_exact (count2);
      for (j = 0; j < count2; j++)
	{
	  int parm = streamer_read_uhwi (&ib);
	  if (info)
	    info->builtin_constant_p_parms.quick_push (parm);
	}
      for (e = node->callees; e; e = e->next_callee)
	read_ipa_call_summary (&ib, e, info != NULL);
      for (e = node->indirect_calls; e; e = e->next_callee)
	read_ipa_call_summary (&ib, e, info != NULL);
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

  ipa_prop_read_jump_functions ();
  ipa_fn_summary_alloc ();

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data
	= lto_get_summary_section_data (file_data, LTO_section_ipa_fn_summary,
					&len);
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

  gcc_assert (ipa_fn_summaries);
  ipa_fn_summaries->enable_insertion_hook ();
}


/* Write inline summary for edge E to OB.  */

static void
write_ipa_call_summary (struct output_block *ob, struct cgraph_edge *e)
{
  class ipa_call_summary *es = ipa_call_summaries->get (e);
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
    {
      streamer_write_uhwi (ob, es->param[i].change_prob);
      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, es->param[i].points_to_local_or_readonly_memory, 1);
      bp_pack_value (&bp, es->param[i].points_to_possible_sra_candidate, 1);
      streamer_write_bitpack (&bp);
    }
}


/* Write inline summary for node in SET.
   Jump functions are shared among ipa-cp and inliner, so when ipa-cp is
   active, we don't need to write them twice.  */

static void
ipa_fn_summary_write (void)
{
  struct output_block *ob = create_output_block (LTO_section_ipa_fn_summary);
  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  unsigned int count = 0;

  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      cgraph_node *cnode = lsei_cgraph_node (lsei);
      if (cnode->definition && !cnode->alias)
	count++;
    }
  streamer_write_uhwi (ob, count);

  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      cgraph_node *cnode = lsei_cgraph_node (lsei);
      if (cnode->definition && !cnode->alias)
	{
	  class ipa_fn_summary *info = ipa_fn_summaries->get (cnode);
	  class ipa_size_summary *size_info = ipa_size_summaries->get (cnode);
	  struct bitpack_d bp;
	  struct cgraph_edge *edge;
	  int i;
	  size_time_entry *e;
	  struct condition *c;

	  streamer_write_uhwi (ob, lto_symtab_encoder_encode (encoder, cnode));
	  streamer_write_hwi (ob, size_info->estimated_self_stack_size);
	  streamer_write_hwi (ob, size_info->self_size);
	  info->time.stream_out (ob);
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, info->inlinable, 1);
	  bp_pack_value (&bp, info->fp_expressions, 1);
	  streamer_write_bitpack (&bp);
	  if (!lto_stream_offload_p)
	    streamer_write_uhwi (ob, info->target_info);
	  streamer_write_uhwi (ob, vec_safe_length (info->conds));
	  for (i = 0; vec_safe_iterate (info->conds, i, &c); i++)
	    {
	      int j;
	      struct expr_eval_op *op;

	      streamer_write_uhwi (ob, c->operand_num);
	      streamer_write_uhwi (ob, c->code);
	      stream_write_tree (ob, c->type, true);
	      stream_write_tree (ob, c->val, true);
	      bp = bitpack_create (ob->main_stream);
	      bp_pack_value (&bp, c->agg_contents, 1);
	      bp_pack_value (&bp, c->by_ref, 1);
	      streamer_write_bitpack (&bp);
	      if (c->agg_contents)
		streamer_write_uhwi (ob, c->offset);
	      streamer_write_uhwi (ob, vec_safe_length (c->param_ops));
	      for (j = 0; vec_safe_iterate (c->param_ops, j, &op); j++)
		{
		  streamer_write_uhwi (ob, op->code);
		  stream_write_tree (ob, op->type, true);
		  if (op->val[0])
		    {
		      bp = bitpack_create (ob->main_stream);
		      bp_pack_value (&bp, op->index, 2);
		      streamer_write_bitpack (&bp);
		      stream_write_tree (ob, op->val[0], true);
		      if (op->val[1])
			stream_write_tree (ob, op->val[1], true);
		    }
		}
	    }
	  streamer_write_uhwi (ob, info->size_time_table.length ());
	  for (i = 0; info->size_time_table.iterate (i, &e); i++)
	    {
	      streamer_write_uhwi (ob, e->size);
	      e->time.stream_out (ob);
	      e->exec_predicate.stream_out (ob);
	      e->nonconst_predicate.stream_out (ob);
	    }
	  ipa_freqcounting_predicate *fcp;
	  streamer_write_uhwi (ob, vec_safe_length (info->loop_iterations));
	  for (i = 0; vec_safe_iterate (info->loop_iterations, i, &fcp); i++)
	    {
	      fcp->predicate->stream_out (ob);
	      fcp->freq.stream_out (ob);
	    }
	  streamer_write_uhwi (ob, vec_safe_length (info->loop_strides));
	  for (i = 0; vec_safe_iterate (info->loop_strides, i, &fcp); i++)
	    {
	      fcp->predicate->stream_out (ob);
	      fcp->freq.stream_out (ob);
	    }
	  streamer_write_uhwi (ob, info->builtin_constant_p_parms.length ());
	  int ip;
	  for (i = 0; info->builtin_constant_p_parms.iterate (i, &ip);
	       i++)
	    streamer_write_uhwi (ob, ip);
	  for (edge = cnode->callees; edge; edge = edge->next_callee)
	    write_ipa_call_summary (ob, edge);
	  for (edge = cnode->indirect_calls; edge; edge = edge->next_callee)
	    write_ipa_call_summary (ob, edge);
	}
    }
  streamer_write_char_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);

  ipa_prop_write_jump_functions ();
}


/* Release function summary.  */

void
ipa_free_fn_summary (void)
{
  if (!ipa_call_summaries)
    return;
  ggc_delete (ipa_fn_summaries);
  ipa_fn_summaries = NULL;
  delete ipa_call_summaries;
  ipa_call_summaries = NULL;
  edge_predicate_pool.release ();
  /* During IPA this is one of largest datastructures to release.  */
  if (flag_wpa)
    ggc_trim ();
}

/* Release function summary.  */

void
ipa_free_size_summary (void)
{
  if (!ipa_size_summaries)
    return;
  delete ipa_size_summaries;
  ipa_size_summaries = NULL;
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
  opt_pass * clone () final override
  {
    return new pass_local_fn_summary (m_ctxt);
  }
  unsigned int execute (function *) final override
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
  opt_pass *clone () final override
  {
    return new pass_ipa_free_fn_summary (m_ctxt);
  }
  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      small_p = param;
    }
  bool gate (function *) final override { return true; }
  unsigned int execute (function *) final override
    {
      ipa_free_fn_summary ();
      /* Free ipa-prop structures if they are no longer needed.  */
      ipa_free_all_structures_after_iinln ();
      if (!flag_wpa)
	ipa_free_size_summary ();
      return 0;
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
  unsigned int execute (function *) final override { return 0; }

}; // class pass_ipa_fn_summary

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_fn_summary (gcc::context *ctxt)
{
  return new pass_ipa_fn_summary (ctxt);
}

/* Reset all state within ipa-fnsummary.cc so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
ipa_fnsummary_cc_finalize (void)
{
  ipa_free_fn_summary ();
  ipa_free_size_summary ();
}
