/* Inlining decision heuristics.
   Copyright (C) 2003, 2004, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
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
#include "tree-inline.h"
#include "langhooks.h"
#include "flags.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "params.h"
#include "tree-pass.h"
#include "coverage.h"
#include "ggc.h"
#include "tree-flow.h"
#include "ipa-prop.h"
#include "lto-streamer.h"
#include "ipa-inline.h"

/* Estimate runtime of function can easilly run into huge numbers with many
   nested loops.  Be sure we can compute time * INLINE_SIZE_SCALE in integer.
   For anything larger we use gcov_type.  */
#define MAX_TIME 1000000

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

/* Holders of ipa cgraph hooks: */
static struct cgraph_node_hook_list *function_insertion_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_edge_hook_list *edge_removal_hook_holder;
static void inline_node_removal_hook (struct cgraph_node *, void *);
static void inline_node_duplication_hook (struct cgraph_node *,
					  struct cgraph_node *, void *);

/* VECtor holding inline summaries.  
   In GGC memory because conditions might point to constant trees.  */
VEC(inline_summary_t,gc) *inline_summary_vec;

/* Cached node/edge growths.  */
VEC(int,heap) *node_growth_cache;
VEC(edge_growth_cache_entry,heap) *edge_growth_cache;


/* Return true predicate (tautology).
   We represent it by empty list of clauses.  */

static inline struct predicate
true_predicate (void)
{
  struct predicate p;
  p.clause[0]=0;
  return p;
}


/* Return predicate testing single condition number COND.  */

static inline struct predicate
single_cond_predicate (int cond)
{
  struct predicate p;
  p.clause[0]=1 << cond;
  p.clause[1]=0;
  return p;
}


/* Return false predicate.  First clause require false condition.  */

static inline struct predicate
false_predicate (void)
{
  return single_cond_predicate (predicate_false_condition);
}


/* Return predicate that is set true when function is not inlined.  */
static inline struct predicate
not_inlined_predicate (void)
{
  return single_cond_predicate (predicate_not_inlined_condition);
}


/* Add condition to condition list CONDS.  */

static struct predicate
add_condition (struct inline_summary *summary, int operand_num,
	       enum tree_code code, tree val)
{
  int i;
  struct condition *c;
  struct condition new_cond;

  for (i = 0; VEC_iterate (condition, summary->conds, i, c); i++)
    {
      if (c->operand_num == operand_num
	  && c->code == code
	  && c->val == val)
        return single_cond_predicate (i + predicate_first_dynamic_condition);
    }
  /* Too many conditions.  Give up and return constant true.  */
  if (i == NUM_CONDITIONS - predicate_first_dynamic_condition)
    return true_predicate ();

  new_cond.operand_num = operand_num;
  new_cond.code = code;
  new_cond.val = val;
  VEC_safe_push (condition, gc, summary->conds, &new_cond);
  return single_cond_predicate (i + predicate_first_dynamic_condition);
}


/* Add clause CLAUSE into the predicate.  */

static inline void
add_clause (struct predicate *p, clause_t clause)
{
  int i;
  int insert_here = -1;
  /* True clause.  */
  if (!clause)
    return;

  /* Flase clause makes the whole predicate false.  Kill the other variants.  */
  if (clause & (1 << predicate_false_condition))
    {
      p->clause[0] = (1 << predicate_false_condition);
      p->clause[1] = 0;
    }
  for (i = 0; i < MAX_CLAUSES - 1; i++)
    {
      if (p->clause[i] == clause)
        return;
      if (!p->clause[i])
	break;
      if (p->clause[i] < clause && !insert_here)
	insert_here = i;
    }
  /* We run out of variants.  Be conservative in positive direciton.  */
  if (i == MAX_CLAUSES)
    return;
  /* Keep clauses ordered by index, so equivalence testing is easy.  */
  p->clause[i + 1] = 0;
  if (insert_here >= 0)
    for (;i > insert_here; i--)
      p->clause[i] = p->clause[i - 1];
  else
    insert_here = i;
  p->clause[insert_here] = clause;
}


/* Return P & P2.  */

static struct predicate
and_predicates (struct predicate *p, struct predicate *p2)
{
  struct predicate out = *p;
  int i;
  for (i = 0; p2->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      add_clause (&out, p2->clause[i]);
    }
  return out;
}


/* Return P | P2.  */

static struct predicate
or_predicates (struct predicate *p, struct predicate *p2)
{
  struct predicate out = true_predicate ();
  int i,j;
  /* If one of conditions is false, return the other.  */
  if (p2->clause[0] == 1 << predicate_false_condition)
    {
      gcc_checking_assert (!p2->clause[1]);
      return *p;
    }
  if (p->clause[0] == 1 << predicate_false_condition)
    {
      gcc_checking_assert (!p->clause[1]);
      return *p2;
    }
  for (i = 0; p->clause[i]; i++)
    for (j = 0; p2->clause[j]; j++)
      {
        gcc_checking_assert (i < MAX_CLAUSES && j < MAX_CLAUSES);
        add_clause (&out, p->clause[i] | p2->clause[j]);
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
      if (p->clause[i] != p2->clause[i])
        return false;
    }
  return !p2->clause[i];
}


/* Having partial truth assignment in POSSIBLE_TRUTHS, return false if predicate P
   to be false.  */

static bool
evaulate_predicate (struct predicate *p, clause_t possible_truths)
{
  int i;

  /* True remains true.  */
  if (!p->clause[0])
    return true;

  /* See if we can find clause we can disprove.  */
  for (i = 0; p->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      if (!(p->clause[i] & possible_truths))
        return false;
    }
  return true;
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
      c = VEC_index (condition, conditions, cond - predicate_first_dynamic_condition);
      fprintf (f, "op%i", c->operand_num);
      if (c->code == IS_NOT_CONSTANT)
	{
	  fprintf (f, " not constant");
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
  if (!pred->clause[0])
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


/* Record SIZE and TIME under condition PRED into the inline summary.  */

static void
account_size_time (struct inline_summary *summary, int size, int time, struct predicate *pred)
{
  size_time_entry *e;
  bool found = false;
  int i;

  if (pred->clause[0] == (1 << predicate_false_condition))
    return;

  /* We need to create initial empty unconitional clause, but otherwie
     we don't need to account empty times and sizes.  */
  if (!size && !time && summary->conds)
    return;

  /* Watch overflow that might result from insane profiles.  */
  if (time > MAX_TIME * INLINE_TIME_SCALE)
    time = MAX_TIME * INLINE_TIME_SCALE;
  gcc_assert (time >= 0);

  for (i = 0; VEC_iterate (size_time_entry, summary->entry, i, e); i++)
    if (predicates_equal_p (&e->predicate, pred))
      {
	found = true;
        break;
      }
  if (i == 32)
    {
      i = 0;
      found = true;
      e = VEC_index (size_time_entry, summary->entry, 0);
      gcc_assert (!e->predicate.clause[0]);
    }
  if (dump_file && (dump_flags & TDF_DETAILS) && (time || size))
    {
      fprintf (dump_file, "\t\tAccounting size:%3.2f, time:%3.2f on %spredicate:",
	       ((double)size) / INLINE_SIZE_SCALE, ((double)time) / INLINE_TIME_SCALE,
	       found ? "" : "new ");
      dump_predicate (dump_file, summary->conds, pred);
    }
  if (!found)
    {
      struct size_time_entry new_entry;
      new_entry.size = size;
      new_entry.time = time;
      new_entry.predicate = *pred;
      VEC_safe_push (size_time_entry, gc, summary->entry, &new_entry);
    }
  else
    {
      e->size += size;
      e->time += time;
      if (e->time > MAX_TIME * INLINE_TIME_SCALE)
	e->time = MAX_TIME * INLINE_TIME_SCALE;
    }
}


/* Work out what conditions might be true at invocation of E.  */

static clause_t
evaulate_conditions_for_edge (struct cgraph_edge *e, bool inline_p)
{
  clause_t clause = inline_p ? 0 : 1 << predicate_not_inlined_condition;
  struct inline_summary *info = inline_summary (e->callee);
  int i;

  if (ipa_node_params_vector && info->conds
      /* FIXME: it seems that we forget to get argument count in some cases,
	 probaby for previously indirect edges or so.  */
      && ipa_get_cs_argument_count (IPA_EDGE_REF (e)))
    {
      struct ipa_node_params *parms_info;
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      int i, count = ipa_get_cs_argument_count (args);
      struct ipcp_lattice lat;
      struct condition *c;
      VEC (tree, heap) *known_vals = NULL;

      if (e->caller->global.inlined_to)
        parms_info = IPA_NODE_REF (e->caller->global.inlined_to);
      else
        parms_info = IPA_NODE_REF (e->caller);

      VEC_safe_grow_cleared (tree, heap, known_vals, count);
      for (i = 0; i < count; i++)
	{
	  ipa_lattice_from_jfunc (parms_info, &lat, ipa_get_ith_jump_func (args, i));
	  if (lat.type == IPA_CONST_VALUE)
	    VEC_replace (tree, known_vals, i, lat.constant);
	}
      for (i = 0; VEC_iterate (condition, info->conds, i, c); i++)
	{
	  tree val = VEC_index (tree, known_vals, c->operand_num);
	  tree res;

	  if (!val)
	    {
	      clause |= 1 << (i + predicate_first_dynamic_condition);
	      continue;
	    }
	  if (c->code == IS_NOT_CONSTANT)
	    continue;
	  res = fold_binary_to_constant (c->code, boolean_type_node, val, c->val);
	  if (res
	      && integer_zerop (res))
	    continue;
	  clause |= 1 << (i + predicate_first_dynamic_condition);
	}
      VEC_free (tree, heap, known_vals);
    }
  else
    for (i = 0; i < (int)VEC_length (condition, info->conds); i++)
      clause |= 1 << (i + predicate_first_dynamic_condition);

  return clause;
}


/* Allocate the inline summary vector or resize it to cover all cgraph nodes. */

static void
inline_summary_alloc (void)
{
  if (!node_removal_hook_holder)
    node_removal_hook_holder =
      cgraph_add_node_removal_hook (&inline_node_removal_hook, NULL);
  if (!node_duplication_hook_holder)
    node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&inline_node_duplication_hook, NULL);

  if (VEC_length (inline_summary_t, inline_summary_vec)
      <= (unsigned) cgraph_max_uid)
    VEC_safe_grow_cleared (inline_summary_t, gc,
			   inline_summary_vec, cgraph_max_uid + 1);
}

/* Hook that is called by cgraph.c when a node is removed.  */

static void
inline_node_removal_hook (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  struct inline_summary *info;
  if (VEC_length (inline_summary_t, inline_summary_vec)
      <= (unsigned)node->uid)
    return;
  info = inline_summary (node);
  reset_node_growth_cache (node);
  VEC_free (condition, gc, info->conds);
  VEC_free (size_time_entry, gc, info->entry);
  info->conds = NULL;
  info->entry = NULL;
  memset (info, 0, sizeof (inline_summary_t));
}

/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
inline_node_duplication_hook (struct cgraph_node *src, struct cgraph_node *dst,
			      ATTRIBUTE_UNUSED void *data)
{
  struct inline_summary *info;
  inline_summary_alloc ();
  info = inline_summary (dst);
  memcpy (info, inline_summary (src),
	  sizeof (struct inline_summary));
  info->conds = VEC_copy (condition, gc, info->conds);
  info->entry = VEC_copy (size_time_entry, gc, info->entry);
}


/* Keep edge cache consistent across edge removal.  */

static void
inline_edge_removal_hook (struct cgraph_edge *edge, void *data ATTRIBUTE_UNUSED)
{
  reset_edge_growth_cache (edge);
}


/* Initialize growth caches.  */

void
initialize_growth_caches (void)
{
  if (!edge_removal_hook_holder)
    edge_removal_hook_holder =
      cgraph_add_edge_removal_hook (&inline_edge_removal_hook, NULL);
  if (cgraph_edge_max_uid)
    VEC_safe_grow_cleared (edge_growth_cache_entry, heap, edge_growth_cache,
			   cgraph_edge_max_uid);
  if (cgraph_max_uid)
    VEC_safe_grow_cleared (int, heap, node_growth_cache, cgraph_max_uid);
}


/* Free growth caches.  */

void
free_growth_caches (void)
{
  if (edge_removal_hook_holder)
    cgraph_remove_edge_removal_hook (edge_removal_hook_holder);
  VEC_free (edge_growth_cache_entry, heap, edge_growth_cache);
  edge_growth_cache = 0;
  VEC_free (int, heap, node_growth_cache);
  node_growth_cache = 0;
}


static void
dump_inline_summary (FILE * f, struct cgraph_node *node)
{
  if (node->analyzed)
    {
      struct inline_summary *s = inline_summary (node);
      size_time_entry *e;
      int i;
      fprintf (f, "Inline summary for %s/%i", cgraph_node_name (node),
	       node->uid);
      if (DECL_DISREGARD_INLINE_LIMITS (node->decl))
	fprintf (f, " always_inline");
      if (s->inlinable)
	fprintf (f, " inlinable");
      if (s->versionable)
	fprintf (f, " versionable");
      fprintf (f, "\n  self time:       %i\n",
	       s->self_time);
      fprintf (f, "  global time:     %i\n", s->time);
      fprintf (f, "  self size:       %i\n",
	       s->self_size);
      fprintf (f, "  global size:     %i\n", s->size);
      fprintf (f, "  self stack:      %i\n",
	       (int) s->estimated_self_stack_size);
      fprintf (f, "  global stack:    %i\n",
	       (int) s->estimated_stack_size);
      for (i = 0;
	   VEC_iterate (size_time_entry, s->entry, i, e);
	   i++)
	{
	  fprintf (f, "    size:%f, time:%f, predicate:",
		   (double) e->size / INLINE_SIZE_SCALE,
		   (double) e->time / INLINE_TIME_SCALE);
	  dump_predicate (f, s->conds, &e->predicate);
	}
      fprintf (f, "\n");
    }
}

void
debug_inline_summary (struct cgraph_node *node)
{
  dump_inline_summary (stderr, node);
}

void
dump_inline_summaries (FILE *f)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
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
  else if (!callee->analyzed)
    e->inline_failed = CIF_BODY_NOT_AVAILABLE;
  else if (callee->local.redefined_extern_inline)
    e->inline_failed = CIF_REDEFINED_EXTERN_INLINE;
  else if (e->call_stmt && gimple_call_cannot_inline_p (e->call_stmt))
    e->inline_failed = CIF_MISMATCHED_ARGUMENTS;
  else
    e->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
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
  switch (code)
    {
      case GIMPLE_RETURN:
        return 2;
      case GIMPLE_ASSIGN:
	if (gimple_num_ops (stmt) != 2)
	  return 0;

	/* Casts of parameters, loads from parameters passed by reference
	   and stores to return value or parameters are often free after
	   inlining dua to SRA and further combining.
	   Assume that half of statements goes away.  */
	if (gimple_assign_rhs_code (stmt) == CONVERT_EXPR
	    || gimple_assign_rhs_code (stmt) == NOP_EXPR
	    || gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR
	    || gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS)
	  {
	    tree rhs = gimple_assign_rhs1 (stmt);
            tree lhs = gimple_assign_lhs (stmt);
	    tree inner_rhs = rhs;
	    tree inner_lhs = lhs;
	    bool rhs_free = false;
	    bool lhs_free = false;

 	    while (handled_component_p (inner_lhs)
		   || TREE_CODE (inner_lhs) == MEM_REF)
	      inner_lhs = TREE_OPERAND (inner_lhs, 0);
 	    while (handled_component_p (inner_rhs)
	           || TREE_CODE (inner_rhs) == ADDR_EXPR
		   || TREE_CODE (inner_rhs) == MEM_REF)
	      inner_rhs = TREE_OPERAND (inner_rhs, 0);


	    if (TREE_CODE (inner_rhs) == PARM_DECL
	        || (TREE_CODE (inner_rhs) == SSA_NAME
		    && SSA_NAME_IS_DEFAULT_DEF (inner_rhs)
		    && TREE_CODE (SSA_NAME_VAR (inner_rhs)) == PARM_DECL))
	      rhs_free = true;
	    if (rhs_free && is_gimple_reg (lhs))
	      lhs_free = true;
	    if (((TREE_CODE (inner_lhs) == PARM_DECL
	          || (TREE_CODE (inner_lhs) == SSA_NAME
		      && SSA_NAME_IS_DEFAULT_DEF (inner_lhs)
		      && TREE_CODE (SSA_NAME_VAR (inner_lhs)) == PARM_DECL))
		 && inner_lhs != lhs)
	        || TREE_CODE (inner_lhs) == RESULT_DECL
	        || (TREE_CODE (inner_lhs) == SSA_NAME
		    && TREE_CODE (SSA_NAME_VAR (inner_lhs)) == RESULT_DECL))
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


/* Return predicate that must be true when is E executable.  */

static struct predicate
edge_execution_predicate (struct ipa_node_params *info,
			  struct inline_summary *summary,
			  edge e)
{
  struct predicate p = true_predicate ();
  gimple last;
  tree op;
  int index;
  enum tree_code code;

  if (e->src == ENTRY_BLOCK_PTR)
    return p;

  last = last_stmt (e->src);
  /* TODO: handle switch.  */
  if (!last
      || gimple_code (last) != GIMPLE_COND)
    return p;
  if (!is_gimple_ip_invariant (gimple_cond_rhs (last)))
    return p;
  op = gimple_cond_lhs (last);
  /* TODO: handle conditionals like
     var = op0 < 4;
     if (var != 0)
     and __bulitin_constant_p.  */
  if (TREE_CODE (op) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (op))
    return p;
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op));
  if (index == -1)
    return p;
  code = gimple_cond_code (last);

  if (EDGE_TRUE_VALUE)
    code = invert_tree_comparison (code,
				   HONOR_NANS (TYPE_MODE (TREE_TYPE (op))));

  return add_condition (summary,
			index,
			gimple_cond_code (last),
			gimple_cond_rhs (last));
}

static struct predicate
will_be_nonconstant_predicate (struct ipa_node_params *info,
			       struct inline_summary *summary,
			       gimple stmt)
{
  struct predicate p = true_predicate ();
  ssa_op_iter iter;
  tree use;
  struct predicate op_non_const;

  /* What statments might be optimized away
     when their arguments are constant
     TODO: also trivial builtins, especially builtin_constant_p.  */
  if (gimple_code (stmt) != GIMPLE_ASSIGN
      && gimple_code (stmt) != GIMPLE_COND
      && gimple_code (stmt) != GIMPLE_SWITCH)
    return p;

  /* Stores and loads will stay anyway.  */
  if (gimple_vuse (stmt))
    return p;

  /* See if we understand all operands before we start
     adding conditionals.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      /* TODO: handle nested expressions and constant
	 array accesses.  */
      if (TREE_CODE (use) != SSA_NAME
	  || !SSA_NAME_IS_DEFAULT_DEF (use)
	  || ipa_get_param_decl_index (info, SSA_NAME_VAR (use)) < 0)
	return p;
    }
  op_non_const = false_predicate ();
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      p = add_condition (summary,
			 ipa_get_param_decl_index (info, SSA_NAME_VAR (use)),
			 IS_NOT_CONSTANT, NULL);
      op_non_const = or_predicates (&p, &op_non_const);
    }
  return op_non_const;
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
  struct ipa_node_params *parms_info;

  parms_info = ipa_node_params_vector && !early ? IPA_NODE_REF (node) : NULL;

  info->conds = 0;
  info->entry = 0;


  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function body size: %s\n",
	     cgraph_node_name (node));

  /* When we run into maximal number of entries, we assign everything to the
     constant truth case.  Be sure to have it in list. */
  bb_predicate = true_predicate ();
  account_size_time (info, 0, 0, &bb_predicate);

  bb_predicate = not_inlined_predicate ();
  account_size_time (info, 2 * INLINE_SIZE_SCALE, 0, &bb_predicate);


  gcc_assert (my_function && my_function->cfg);
  FOR_EACH_BB_FN (bb, my_function)
    {
      edge e;
      edge_iterator ei;

      freq = compute_call_stmt_bb_frequency (node->decl, bb);

      /* TODO: Obviously predicates can be propagated down across CFG.  */
      if (parms_info)
	{
          bb_predicate = false_predicate ();
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      struct predicate ep;
	      ep = edge_execution_predicate (parms_info, info, e);
	      bb_predicate = or_predicates (&ep, &bb_predicate);
	    }
	}
      else
	bb_predicate = true_predicate ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n BB %i predicate:", bb->index);
	  dump_predicate (dump_file, info->conds, &bb_predicate);
	}
      
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
	  int this_size = estimate_num_insns (stmt, &eni_size_weights);
	  int this_time = estimate_num_insns (stmt, &eni_time_weights);
	  int prob;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  ");
	      print_gimple_stmt (dump_file, stmt, 0, 0);
	      fprintf (dump_file, "\t\tfreq:%3.2f size:%3i time:%3i\n",
		       ((double)freq)/CGRAPH_FREQ_BASE, this_size, this_time);
	    }

	  if (is_gimple_call (stmt))
	    {
	      struct cgraph_edge *edge = cgraph_edge (node, stmt);
	      edge->call_stmt_size = this_size;
	      edge->call_stmt_time = this_time;

	      /* Do not inline calls where we cannot triviall work around
		 mismatches in argument or return types.  */
	      if (edge->callee
		  && !gimple_check_call_matching_types (stmt, edge->callee->decl))
		{
		  edge->call_stmt_cannot_inline_p = true;
		  gimple_call_set_cannot_inline (stmt, true);
		}
	      else
		gcc_assert (!gimple_call_cannot_inline_p (stmt));
	    }

	  if (this_time || this_size)
	    {
	      struct predicate will_be_nonconstant;
	      struct predicate p;

	      this_time *= freq;
	      time += this_time;
	      size += this_size;

	      prob = eliminated_by_inlining_prob (stmt);
	      if (prob == 1 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\t\t50%% will be eliminated by inlining\n");
	      if (prob == 2 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\t\twill eliminated by inlining\n");

	      if (parms_info)
		{
		  will_be_nonconstant
		     = will_be_nonconstant_predicate (parms_info, info, stmt);
		  p = and_predicates (&bb_predicate, &will_be_nonconstant);
		}
	      else
		p = true_predicate ();

	      /* We account everything but the calls.  Calls have their own
		 size/time info attached to cgraph edges.  This is neccesary
		 in order to make the cost disappear after inlining.  */
	      if (!is_gimple_call (stmt))
		{
		  if (prob)
		    {
		      struct predicate ip = not_inlined_predicate ();
		      ip = and_predicates (&ip, &p);
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
  time = (time + CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
  if (time > MAX_TIME)
    time = MAX_TIME;
  inline_summary (node)->self_time = time;
  inline_summary (node)->self_size = size;
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

  /* Estimate the stack size for the function if we're optimizing.  */
  self_stack_size = optimize ? estimated_stack_frame_size (node) : 0;
  info->estimated_self_stack_size = self_stack_size;
  info->estimated_stack_size = self_stack_size;
  info->stack_frame_offset = 0;

  /* Can this function be inlined at all?  */
  info->inlinable = tree_inlinable_function_p (node->decl);

  /* Inlinable functions always can change signature.  */
  if (info->inlinable)
    node->local.can_change_signature = true;
  else
    {
      /* Functions calling builtin_apply can not change signature.  */
      for (e = node->callees; e; e = e->next_callee)
	if (DECL_BUILT_IN (e->callee->decl)
	    && DECL_BUILT_IN_CLASS (e->callee->decl) == BUILT_IN_NORMAL
	    && DECL_FUNCTION_CODE (e->callee->decl) == BUILT_IN_APPLY_ARGS)
	  break;
      node->local.can_change_signature = !e;
    }
  estimate_function_body_sizes (node, early);

  /* Inlining characteristics are maintained by the cgraph_mark_inline.  */
  info->time = info->self_time;
  info->size = info->self_size;
  info->stack_frame_offset = 0;
  info->estimated_stack_size = info->estimated_self_stack_size;
}


/* Compute parameters of functions used by inliner using
   current_function_decl.  */

static unsigned int
compute_inline_parameters_for_current (void)
{
  compute_inline_parameters (cgraph_get_node (current_function_decl), true);
  return 0;
}

struct gimple_opt_pass pass_inline_parameters =
{
 {
  GIMPLE_PASS,
  "inline_param",			/* name */
  NULL,					/* gate */
  compute_inline_parameters_for_current,/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_INLINE_HEURISTICS,			/* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};


/* Increase SIZE and TIME for size and time needed to handle edge E.  */

static void
estimate_edge_size_and_time (struct cgraph_edge *e, int *size, int *time)
{
  *size += e->call_stmt_size * INLINE_SIZE_SCALE;
  *time += (e->call_stmt_time
	    * e->frequency * (INLINE_TIME_SCALE / CGRAPH_FREQ_BASE));
  if (*time > MAX_TIME * INLINE_TIME_SCALE)
    *time = MAX_TIME * INLINE_TIME_SCALE;
}


/* Increase SIZE and TIME for size and time needed to handle all calls in NODE.  */

static void
estimate_calls_size_and_time (struct cgraph_node *node, int *size, int *time)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    if (e->inline_failed)
      estimate_edge_size_and_time (e, size, time);
    else
      estimate_calls_size_and_time (e->callee, size, time);
  /* TODO: look for devirtualizing oppurtunities.  */
  for (e = node->indirect_calls; e; e = e->next_callee)
    estimate_edge_size_and_time (e, size, time);
}


/* Estimate size and time needed to execute callee of EDGE assuming
   that parameters known to be constant at caller of EDGE are
   propagated.  If INLINE_P is true, it is assumed that call will
   be inlined.  */

static void
estimate_callee_size_and_time (struct cgraph_edge *edge, bool inline_p,
		       	       int *ret_size, int *ret_time)
{
  struct inline_summary *info = inline_summary (edge->callee);
  clause_t clause = evaulate_conditions_for_edge (edge, inline_p);
  size_time_entry *e;
  int size = 0, time = 0;
  int i;

  if (dump_file
      && (dump_flags & TDF_DETAILS))
    {
      bool found = false;
      fprintf (dump_file, "   Estimating callee body: %s/%i\n"
			  "   Known to be false: ",
	       cgraph_node_name (edge->callee),
	       edge->callee->uid);

      for (i = predicate_not_inlined_condition;
	   i < (predicate_first_dynamic_condition
		+ (int)VEC_length (condition, info->conds)); i++)
	if (!(clause & (1 << i)))
	  {
	    if (found)
	      fprintf (dump_file, ", ");
	    found = true;
            dump_condition (dump_file, info->conds, i);
	  }
    }

  for (i = 0; VEC_iterate (size_time_entry, info->entry, i, e); i++)
    if (evaulate_predicate (&e->predicate, clause))
      time += e->time, size += e->size;

  if (time > MAX_TIME * INLINE_TIME_SCALE)
    time = MAX_TIME * INLINE_TIME_SCALE;

  estimate_calls_size_and_time (edge->callee, &size, &time);
  time = (time + INLINE_TIME_SCALE / 2) / INLINE_TIME_SCALE;
  size = (size + INLINE_SIZE_SCALE / 2) / INLINE_SIZE_SCALE;


  if (dump_file
      && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n   size:%i time:%i\n", size, time);
  if (ret_time)
    *ret_time = time;
  if (ret_size)
    *ret_size = size;
  return;
}


/* Translate all conditions from callee representation into caller representaiton and
   symbolically evaulate predicate P into new predicate.  */

static struct predicate
remap_predicate (struct inline_summary *info, struct inline_summary *callee_info,
		 struct predicate *p,
		 VEC (int, heap) *operand_map,
		 clause_t possible_truths)
{
  int i;
  struct predicate out = true_predicate ();

  /* True predicate is easy.  */
  if (p->clause[0] == 0)
    return *p;
  for (i = 0; p->clause[i]; i++)
    {
      clause_t clause = p->clause[i];
      int cond;
      struct predicate clause_predicate = false_predicate ();

      gcc_assert (i < MAX_CLAUSES);

      for (cond = 0; cond < NUM_CONDITIONS; cond ++)
	/* Do we have condition we can't disprove?   */
	if (clause & possible_truths & (1 << cond))
	  {
	    struct predicate cond_predicate;
	    /* Work out if the condition can translate to predicate in the
	       inlined function.  */
	    if (cond >= predicate_first_dynamic_condition)
	      {
		 struct condition *c;

		 c = VEC_index (condition, callee_info->conds,
				cond - predicate_first_dynamic_condition);
		 /* See if we can remap condition operand to caller's operand.
		    Otherwise give up.  */
		 if (!operand_map
		     || VEC_index (int, operand_map, c->operand_num) == -1)
		   cond_predicate = true_predicate ();
		 else
		   cond_predicate = add_condition (info,
						   VEC_index (int, operand_map,
							      c->operand_num),
						   c->code, c->val);
	      }
	    /* Fixed conditions remains same, construct single
	       condition predicate.  */
	    else
	      {
		cond_predicate.clause[0] = 1 << cond;
		cond_predicate.clause[1] = 0;
	      }
	    clause_predicate = or_predicates (&clause_predicate, &cond_predicate);
	  }
      out = and_predicates (&out, &clause_predicate);
    }
  return out;
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
  VEC (int, heap) *operand_map = NULL;
  int i;

  if (ipa_node_params_vector && callee_info->conds
      /* FIXME: it seems that we forget to get argument count in some cases,
	 probaby for previously indirect edges or so.
	 Removing the test leads to ICE on tramp3d.  */
      && ipa_get_cs_argument_count (IPA_EDGE_REF (edge)))
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (edge);
      int count = ipa_get_cs_argument_count (args);
      int i;

      clause = evaulate_conditions_for_edge (edge, true);
      VEC_safe_grow_cleared (int, heap, operand_map, count);
      for (i = 0; i < count; i++)
	{
	  struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, i);
	  int map = -1;
	  /* TODO: handle non-NOPs when merging.  */
	  if (jfunc->type == IPA_JF_PASS_THROUGH
	      && jfunc->value.pass_through.operation == NOP_EXPR)
	    map = jfunc->value.pass_through.formal_id;
	  VEC_replace (int, operand_map, i, map);
	  gcc_assert (map < ipa_get_param_count (IPA_NODE_REF (to)));
	}
    }
  for (i = 0; VEC_iterate (size_time_entry, callee_info->entry, i, e); i++)
    {
      struct predicate p = remap_predicate (info, callee_info,
					    &e->predicate, operand_map, clause);
      gcov_type add_time = ((gcov_type)e->time * edge->frequency
			    + CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
      if (add_time > MAX_TIME)
	add_time = MAX_TIME;
      account_size_time (info, e->size, add_time, &p);
    }
  info->size = 0;
  info->time = 0;
  for (i = 0; VEC_iterate (size_time_entry, info->entry, i, e); i++)
    info->size += e->size, info->time += e->time;
  estimate_calls_size_and_time (to, &info->size, &info->time);
  info->time = (info->time + INLINE_TIME_SCALE / 2) / INLINE_TIME_SCALE;
  info->size = (info->size + INLINE_SIZE_SCALE / 2) / INLINE_SIZE_SCALE;
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
  gcov_type ret;

  gcc_checking_assert (edge->inline_failed);
  estimate_callee_size_and_time (edge, true, &size, &time);

  ret = (((gcov_type)time - edge->call_stmt_time) * edge->frequency
	 + CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
  if (ret > MAX_TIME)
    ret = MAX_TIME;

  /* When caching, update the cache entry.  */
  if (edge_growth_cache)
    {
      int ret_size;
      if ((int)VEC_length (edge_growth_cache_entry, edge_growth_cache)
	  <= edge->uid)
	VEC_safe_grow_cleared (edge_growth_cache_entry, heap, edge_growth_cache,
			       cgraph_edge_max_uid);
      VEC_index (edge_growth_cache_entry, edge_growth_cache, edge->uid)->time
	= ret + (ret >= 0);

      ret_size = size - edge->call_stmt_size;
      gcc_checking_assert (edge->call_stmt_size);
      VEC_index (edge_growth_cache_entry, edge_growth_cache, edge->uid)->size
	= ret_size + (ret_size >= 0);
    }
  return ret;
}


/* Estimate the growth of the caller when inlining EDGE.
   Only to be called via estimate_edge_size.  */

int
do_estimate_edge_growth (struct cgraph_edge *edge)
{
  int size;

  /* When we do caching, use do_estimate_edge_time to populate the entry.  */

  if (edge_growth_cache)
    {
      do_estimate_edge_time (edge);
      size = VEC_index (edge_growth_cache_entry,
			edge_growth_cache,
			edge->uid)->size;
      gcc_checking_assert (size);
      return size - (size > 0);
    }

  /* Early inliner runs without caching, go ahead and do the dirty work.  */
  gcc_checking_assert (edge->inline_failed);
  estimate_callee_size_and_time (edge, true, &size, NULL);
  gcc_checking_assert (edge->call_stmt_size);
  return size - edge->call_stmt_size;
}


/* Estimate self time of the function NODE after inlining EDGE.  */

int
estimate_time_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  gcov_type time = inline_summary (node)->time + estimate_edge_time (edge);
  if (time < 0)
    time = 0;
  if (time > MAX_TIME)
    time = MAX_TIME;
  return time;
}


/* Estimate the size of NODE after inlining EDGE which should be an
   edge to either NODE or a call inlined into NODE.  */

int
estimate_size_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  int size = inline_summary (node)->size + estimate_edge_growth (edge);
  gcc_assert (size >= 0);
  return size;
}


/* Estimate the growth caused by inlining NODE into all callees.  */

int
do_estimate_growth (struct cgraph_node *node)
{
  int growth = 0;
  struct cgraph_edge *e;
  bool self_recursive = false;
  struct inline_summary *info = inline_summary (node);

  for (e = node->callers; e; e = e->next_caller)
    {
      gcc_checking_assert (e->inline_failed);

      if (e->caller == node
	  || (e->caller->global.inlined_to
	      && e->caller->global.inlined_to == node))
        self_recursive = true;
      growth += estimate_edge_growth (e);
    }
     

  /* For self recursive functions the growth estimation really should be
     infinity.  We don't want to return very large values because the growth
     plays various roles in badness computation fractions.  Be sure to not
     return zero or negative growths. */
  if (self_recursive)
    growth = growth < info->size ? info->size : growth;
  else
    {
      if (cgraph_will_be_removed_from_program_if_no_direct_calls (node)
	  && !DECL_EXTERNAL (node->decl))
	growth -= info->size;
      /* COMDAT functions are very often not shared across multiple units since they
	 come from various template instantiations.  Take this into account.  */
      else  if (DECL_COMDAT (node->decl)
		&& cgraph_can_remove_if_no_direct_calls_p (node))
	growth -= (info->size
		   * (100 - PARAM_VALUE (PARAM_COMDAT_SHARING_PROBABILITY)) + 50) / 100;
    }

  if (node_growth_cache)
    {
      if ((int)VEC_length (int, node_growth_cache) <= node->uid)
	VEC_safe_grow_cleared (int, heap, node_growth_cache, cgraph_max_uid);
      VEC_replace (int, node_growth_cache, node->uid, growth + (growth >= 0));
    }
  return growth;
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
  current_function_decl = node->decl;

  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function: %s/%u\n",
	     cgraph_node_name (node), node->uid);
  /* FIXME: We should remove the optimize check after we ensure we never run
     IPA passes when not optimizing.  */
  if (flag_indirect_inlining && optimize)
    inline_indirect_intraprocedural_analysis (node);
  compute_inline_parameters (node, false);

  current_function_decl = NULL;
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

  function_insertion_hook_holder =
      cgraph_add_function_insertion_hook (&add_new_function, NULL);

  if (flag_indirect_inlining)
    ipa_register_cgraph_hooks ();

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      inline_analyze_function (node);
}


/* Stream in inline summaries from the section.  */

static void
inline_read_section (struct lto_file_decl_data *file_data, const char *data,
		     size_t len)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int32_t cfg_offset = sizeof (struct lto_function_header);
  const int32_t main_offset = cfg_offset + header->cfg_size;
  const int32_t string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  struct lto_input_block ib;
  unsigned int i, count2, j;
  unsigned int f_count;

  LTO_INIT_INPUT_BLOCK (ib, (const char *) data + main_offset, 0,
			header->main_size);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, NULL);
  f_count = lto_input_uleb128 (&ib);
  for (i = 0; i < f_count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      struct inline_summary *info;
      lto_cgraph_encoder_t encoder;
      struct bitpack_d bp;

      index = lto_input_uleb128 (&ib);
      encoder = file_data->cgraph_node_encoder;
      node = lto_cgraph_encoder_deref (encoder, index);
      info = inline_summary (node);

      info->estimated_stack_size
	= info->estimated_self_stack_size = lto_input_uleb128 (&ib);
      info->size = info->self_size = lto_input_uleb128 (&ib);
      info->time = info->self_time = lto_input_uleb128 (&ib);

      bp = lto_input_bitpack (&ib);
      info->inlinable = bp_unpack_value (&bp, 1);
      info->versionable = bp_unpack_value (&bp, 1);

      count2 = lto_input_uleb128 (&ib);
      gcc_assert (!info->conds);
      for (j = 0; j < count2; j++)
	{
	  struct condition c;
	  c.operand_num = lto_input_uleb128 (&ib);
	  c.code = (enum tree_code) lto_input_uleb128 (&ib);
	  c.val = lto_input_tree (&ib, data_in);
	  VEC_safe_push (condition, gc, info->conds, &c);
	}
      count2 = lto_input_uleb128 (&ib);
      gcc_assert (!info->entry);
      for (j = 0; j < count2; j++)
	{
	  struct size_time_entry e;
	  clause_t clause;
	  int k = 0;

	  e.size = lto_input_uleb128 (&ib);
	  e.time = lto_input_uleb128 (&ib);
	  do 
	    {
	      clause = e.predicate.clause[k++] = lto_input_uleb128 (&ib);
	      gcc_assert (k < MAX_CLAUSES);
	    }
	  while (clause);

	  VEC_safe_push (size_time_entry, gc, info->entry, &e);
	}
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
      const char *data = lto_get_section_data (file_data, LTO_section_inline_summary, NULL, &len);
      if (data)
        inline_read_section (file_data, data, len);
      else
	/* Fatal error here.  We do not want to support compiling ltrans units with
	   different version of compiler or different flags than the WPA unit, so
	   this should never happen.  */
	fatal_error ("ipa inline summary is missing in input file");
    }
  if (flag_indirect_inlining)
    {
      ipa_register_cgraph_hooks ();
      if (!flag_ipa_cp)
        ipa_prop_read_jump_functions ();
    }
  function_insertion_hook_holder =
      cgraph_add_function_insertion_hook (&add_new_function, NULL);
}


/* Write inline summary for node in SET.
   Jump functions are shared among ipa-cp and inliner, so when ipa-cp is
   active, we don't need to write them twice.  */

void
inline_write_summary (cgraph_node_set set,
		      varpool_node_set vset ATTRIBUTE_UNUSED)
{
  struct cgraph_node *node;
  struct output_block *ob = create_output_block (LTO_section_inline_summary);
  lto_cgraph_encoder_t encoder = ob->decl_state->cgraph_node_encoder;
  unsigned int count = 0;
  int i;

  for (i = 0; i < lto_cgraph_encoder_size (encoder); i++)
    if (lto_cgraph_encoder_deref (encoder, i)->analyzed)
      count++;
  lto_output_uleb128_stream (ob->main_stream, count);

  for (i = 0; i < lto_cgraph_encoder_size (encoder); i++)
    {
      node = lto_cgraph_encoder_deref (encoder, i);
      if (node->analyzed)
	{
	  struct inline_summary *info = inline_summary (node);
	  struct bitpack_d bp;
	  int i;
	  size_time_entry *e;
	  struct condition *c;
	  

	  lto_output_uleb128_stream (ob->main_stream,
				     lto_cgraph_encoder_encode (encoder, node));
	  lto_output_sleb128_stream (ob->main_stream,
				     info->estimated_self_stack_size);
	  lto_output_sleb128_stream (ob->main_stream,
				     info->self_size);
	  lto_output_sleb128_stream (ob->main_stream,
				     info->self_time);
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, info->inlinable, 1);
	  bp_pack_value (&bp, info->versionable, 1);
	  lto_output_bitpack (&bp);
	  lto_output_uleb128_stream (ob->main_stream,
				     VEC_length (condition, info->conds));
	  for (i = 0; VEC_iterate (condition, info->conds, i, c); i++)
	    {
	      lto_output_uleb128_stream (ob->main_stream,
					 c->operand_num);
	      lto_output_uleb128_stream (ob->main_stream,
					 c->code);
	      lto_output_tree (ob, c->val, true);
	    }
	  lto_output_uleb128_stream (ob->main_stream,
				     VEC_length (size_time_entry, info->entry));
	  for (i = 0;
	       VEC_iterate (size_time_entry, info->entry, i, e);
	       i++)
	    {
	      int j;
	      lto_output_uleb128_stream (ob->main_stream,
					 e->size);
	      lto_output_uleb128_stream (ob->main_stream,
					 e->time);
	      for (j = 0; e->predicate.clause[j]; j++)
		{
		   gcc_assert (j < MAX_CLAUSES);
		   lto_output_uleb128_stream (ob->main_stream,
					      e->predicate.clause[j]);
		}
	      lto_output_uleb128_stream (ob->main_stream, 0);
	    }
	}
    }
  lto_output_1_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);

  if (flag_indirect_inlining && !flag_ipa_cp)
    ipa_prop_write_jump_functions (set);
}


/* Release inline summary.  */

void
inline_free_summary (void)
{
  if (function_insertion_hook_holder)
    cgraph_remove_function_insertion_hook (function_insertion_hook_holder);
  function_insertion_hook_holder = NULL;
  if (node_removal_hook_holder)
    cgraph_remove_node_removal_hook (node_removal_hook_holder);
  node_removal_hook_holder = NULL;
  if (node_duplication_hook_holder)
    cgraph_remove_node_duplication_hook (node_duplication_hook_holder);
  node_duplication_hook_holder = NULL;
  VEC_free (inline_summary_t, gc, inline_summary_vec);
  inline_summary_vec = NULL;
}
