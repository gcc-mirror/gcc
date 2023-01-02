/* Loop unswitching.
   Copyright (C) 2004-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
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
#include "tree-pass.h"
#include "ssa.h"
#include "fold-const.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-inline.h"
#include "gimple-iterator.h"
#include "cfghooks.h"
#include "tree-ssa-loop-manip.h"
#include "tree-vectorizer.h"
#include "tree-pretty-print.h"
#include "gimple-range.h"
#include "dbgcnt.h"
#include "cfganal.h"

/* This file implements the loop unswitching, i.e. transformation of loops like

   while (A)
     {
       if (inv)
         B;

       X;

       if (!inv)
	 C;
     }

   where inv is the loop invariant, into

   if (inv)
     {
       while (A)
	 {
           B;
	   X;
	 }
     }
   else
     {
       while (A)
	 {
	   X;
	   C;
	 }
     }

   Inv is considered invariant iff the values it compares are both invariant;
   tree-ssa-loop-im.cc ensures that all the suitable conditions are in this
   shape.  */

/* Loop unswitching algorithm for innermost loops works in the following steps:

   1) Number of instructions is estimated for each BB that belongs to a loop.
   2) Unswitching candidates are found for gcond and gswitch statements
      (note that an unswitching predicate for a gswitch actually corresponds
       to a non-default edge so it can contain multiple cases).
   3) The so called unswitch predicates are stored in a cache where the
      gimple_uid of the last stmt in a basic-block is an index to the cache.
   4) We consider one by one the unswitching candidates and calculate BBs that
      will be reachable in the unswitch version.
   5) A selected predicate is chosen and we simplify the CFG (dead edges) in
      both versions of the loop.  We utilize both Ranger for condition
      simplification and also symbol equivalence.  The folded if conditions
      are replaced with true/false values, while for gswitch we mark the
      corresponding edges with a pass-defined unreachable flag.
   6) Every time we unswitch a loop, we save unswitch_predicate to a vector
      together with information if true or false edge was taken.  Doing that
      we have a so called PREDICATE_PATH that is utilized for simplification
      of the cloned loop.
   7) The process is repeated until we reach a growth threshold or all
      unswitching opportunities are taken.  */

/* A tuple that holds a GENERIC condition and value range for an unswitching
   predicate.  */

struct unswitch_predicate
{
  /* CTOR for a switch edge predicate.  */
  unswitch_predicate (tree cond, tree lhs_, int edge_index_, edge e,
		      const int_range_max& edge_range)
    : condition (cond), lhs (lhs_),
      true_range (edge_range), edge_index (edge_index_), switch_p (true)
  {
    gcc_assert (!(e->flags & (EDGE_TRUE_VALUE|EDGE_FALSE_VALUE))
		&& irange::supports_p (TREE_TYPE (lhs)));
    false_range = true_range;
    if (!false_range.varying_p ()
	&& !false_range.undefined_p ())
      false_range.invert ();
    count = e->count ();
    num = predicates->length ();
    predicates->safe_push (this);
  }

  /* CTOR for a GIMPLE condition statement.  */
  unswitch_predicate (gcond *stmt)
    : switch_p (false)
  {
    basic_block bb = gimple_bb (stmt);
    if (EDGE_SUCC (bb, 0)->flags & EDGE_TRUE_VALUE)
      edge_index = 0;
    else
      edge_index = 1;
    lhs = gimple_cond_lhs (stmt);
    tree rhs = gimple_cond_rhs (stmt);
    enum tree_code code = gimple_cond_code (stmt);
    condition = build2 (code, boolean_type_node, lhs, rhs);
    count = EDGE_SUCC (bb, 0)->count ().max (EDGE_SUCC (bb, 1)->count ());
    if (irange::supports_p (TREE_TYPE (lhs)))
      {
	auto range_op = range_op_handler (code, TREE_TYPE (lhs));
	int_range<2> rhs_range (TREE_TYPE (rhs));
	if (CONSTANT_CLASS_P (rhs))
	  rhs_range.set (rhs, rhs);
	if (!range_op.op1_range (true_range, TREE_TYPE (lhs),
				 int_range<2> (boolean_true_node,
					       boolean_true_node), rhs_range)
	    || !range_op.op1_range (false_range, TREE_TYPE (lhs),
				    int_range<2> (boolean_false_node,
						  boolean_false_node),
				    rhs_range))
	  {
	    true_range.set_varying (TREE_TYPE (lhs));
	    false_range.set_varying (TREE_TYPE (lhs));
	  }
      }
    num = predicates->length ();
    predicates->safe_push (this);
  }

  /* Copy ranges for purpose of usage in predicate path.  */

  inline void
  copy_merged_ranges ()
  {
    merged_true_range = true_range;
    merged_false_range = false_range;
  }

  /* GENERIC unswitching expression testing LHS against CONSTANT.  */
  tree condition;

  /* LHS of the expression.  */
  tree lhs;

  /* Initial ranges (when the expression is true/false) for the expression.  */
  int_range_max true_range = {}, false_range = {};

  /* Modified range that is part of a predicate path.  */
  int_range_max merged_true_range = {}, merged_false_range = {};

  /* Index of the edge the predicate belongs to in the successor vector.  */
  int edge_index;

  /* The profile count of this predicate.  */
  profile_count count;

  /* Whether the predicate was created from a switch statement.  */
  bool switch_p;

  /* The number of the predicate in the predicates vector below.  */
  unsigned num;

  /* Vector of all used predicates, used for assigning a unique id that
     can be used for bitmap operations.  */
  static vec<unswitch_predicate *> *predicates;
};

vec<unswitch_predicate *> *unswitch_predicate::predicates;

/* Ranger instance used in the pass.  */
static gimple_ranger *ranger = NULL;

/* Cache storage for unswitch_predicate belonging to a basic block.  */
static vec<vec<unswitch_predicate *>> *bb_predicates;

/* The type represents a predicate path leading to a basic block.  */
typedef vec<std::pair<unswitch_predicate *, bool>> predicate_vector;

static class loop *tree_unswitch_loop (class loop *, edge, tree);
static bool tree_unswitch_single_loop (class loop *, dump_user_location_t,
				       predicate_vector &predicate_path,
				       unsigned loop_size, unsigned &budget,
				       int ignored_edge_flag, bitmap,
				       unswitch_predicate * = NULL,
				       basic_block = NULL);
static void
find_unswitching_predicates_for_bb (basic_block bb, class loop *loop,
				    class loop *&outer_loop,
				    vec<unswitch_predicate *> &candidates,
				    unswitch_predicate *&hottest,
				    basic_block &hottest_bb);
static bool tree_unswitch_outer_loop (class loop *);
static edge find_loop_guard (class loop *, vec<gimple *>&);
static bool empty_bb_without_guard_p (class loop *, basic_block,
				      vec<gimple *>&);
static bool used_outside_loop_p (class loop *, tree, vec<gimple *>&);
static void hoist_guard (class loop *, edge);
static bool check_exit_phi (class loop *);
static tree get_vop_from_header (class loop *);
static void clean_up_after_unswitching (int);

/* Return vector of predicates that belong to a basic block.  */

static vec<unswitch_predicate *> &
get_predicates_for_bb (basic_block bb)
{
  gimple *last = last_stmt (bb);
  return (*bb_predicates)[last == NULL ? 0 : gimple_uid (last)];
}

/* Save predicates that belong to a basic block.  */

static void
set_predicates_for_bb (basic_block bb, vec<unswitch_predicate *> predicates)
{
  gimple_set_uid (last_stmt (bb), bb_predicates->length ());
  bb_predicates->safe_push (predicates);
}

/* Initialize LOOP information reused during the unswitching pass.
   Return total number of instructions in the loop.  Adjusts LOOP to
   the outermost loop all candidates are invariant in.  */

static unsigned
init_loop_unswitch_info (class loop *&loop, unswitch_predicate *&hottest,
			 basic_block &hottest_bb)
{
  unsigned total_insns = 0;

  basic_block *bbs = get_loop_body (loop);

  /* Unswitch only nests with no sibling loops.  */
  class loop *outer_loop = loop;
  unsigned max_depth = param_max_unswitch_depth;
  while (loop_outer (outer_loop)->num != 0
	 && !loop_outer (outer_loop)->inner->next
	 && --max_depth != 0)
    outer_loop = loop_outer (outer_loop);
  hottest = NULL;
  hottest_bb = NULL;
  /* Find all unswitching candidates in the innermost loop.  */
  for (unsigned i = 0; i != loop->num_nodes; i++)
    {
      /* Find a bb to unswitch on.  */
      vec<unswitch_predicate *> candidates;
      candidates.create (1);
      find_unswitching_predicates_for_bb (bbs[i], loop, outer_loop, candidates,
					  hottest, hottest_bb);
      if (!candidates.is_empty ())
	set_predicates_for_bb (bbs[i], candidates);
      else
	{
	  candidates.release ();
	  gimple *last = last_stmt (bbs[i]);
	  if (last != NULL)
	    gimple_set_uid (last, 0);
	}
    }

  if (outer_loop != loop)
    {
      free (bbs);
      bbs = get_loop_body (outer_loop);
    }

  /* Calculate instruction count.  */
  for (unsigned i = 0; i < outer_loop->num_nodes; i++)
    {
      unsigned insns = 0;
      for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	insns += estimate_num_insns (gsi_stmt (gsi), &eni_size_weights);
      /* No predicates to unswitch on in the outer loops.  */
      if (!flow_bb_inside_loop_p (loop, bbs[i]))
	{
	  gimple *last = last_stmt (bbs[i]);
	  if (last != NULL)
	    gimple_set_uid (last, 0);
	}

      bbs[i]->aux = (void *)(uintptr_t)insns;
      total_insns += insns;
    }

  free (bbs);

  loop = outer_loop;
  return total_insns;
}

/* Main entry point.  Perform loop unswitching on all suitable loops.  */

unsigned int
tree_ssa_unswitch_loops (function *fun)
{
  bool changed_unswitch = false;
  bool changed_hoist = false;
  auto_edge_flag ignored_edge_flag (fun);

  ranger = enable_ranger (fun);

  /* Go through all loops starting from innermost, hoisting guards.  */
  for (auto loop : loops_list (fun, LI_FROM_INNERMOST))
    {
      if (loop->inner)
	changed_hoist |= tree_unswitch_outer_loop (loop);
    }

  /* Go through innermost loops, unswitching on invariant predicates
     within those.  */
  for (auto loop : loops_list (fun, LI_ONLY_INNERMOST))
    {
      /* Perform initial tests if unswitch is eligible.  */
      dump_user_location_t loc = find_loop_location (loop);

      /* Do not unswitch in cold regions. */
      if (optimize_loop_for_size_p (loop))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, loc,
			     "Not unswitching cold loops\n");
	  continue;
	}

      /* If the loop is not expected to iterate, there is no need
	 for unswitching.  */
      HOST_WIDE_INT iterations = estimated_loop_iterations_int (loop);
      if (iterations < 0)
	iterations = likely_max_loop_iterations_int (loop);
      if (iterations >= 0 && iterations <= 1)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, loc,
			     "Not unswitching, loop is not expected"
			     " to iterate\n");
	  continue;
	}

      bb_predicates = new vec<vec<unswitch_predicate *>> ();
      bb_predicates->safe_push (vec<unswitch_predicate *> ());
      unswitch_predicate::predicates = new vec<unswitch_predicate *> ();

      /* Unswitch loop.  */
      unswitch_predicate *hottest;
      basic_block hottest_bb;
      unsigned int loop_size = init_loop_unswitch_info (loop, hottest,
							hottest_bb);
      unsigned int budget = loop_size + param_max_unswitch_insns;

      predicate_vector predicate_path;
      predicate_path.create (8);
      auto_bitmap handled;
      changed_unswitch |= tree_unswitch_single_loop (loop, loc, predicate_path,
						     loop_size, budget,
						     ignored_edge_flag, handled,
						     hottest, hottest_bb);
      predicate_path.release ();

      for (auto predlist : bb_predicates)
	predlist.release ();
      bb_predicates->release ();
      delete bb_predicates;
      bb_predicates = NULL;

      for (auto pred : unswitch_predicate::predicates)
	delete pred;
      unswitch_predicate::predicates->release ();
      delete unswitch_predicate::predicates;
      unswitch_predicate::predicates = NULL;
    }

  disable_ranger (fun);
  clear_aux_for_blocks ();

  if (changed_unswitch)
    clean_up_after_unswitching (ignored_edge_flag);

  if (changed_unswitch || changed_hoist)
    return TODO_cleanup_cfg;

  return 0;
}

/* Return TRUE if an SSA_NAME maybe undefined and is therefore
   unsuitable for unswitching.  STMT is the statement we are
   considering for unswitching and LOOP is the loop it appears in.  */

static bool
is_maybe_undefined (const tree name, gimple *stmt, class loop *loop)
{
  /* The loop header is the only block we can trivially determine that
     will always be executed.  If the comparison is in the loop
     header, we know it's OK to unswitch on it.  */
  if (gimple_bb (stmt) == loop->header)
    return false;

  auto_bitmap visited_ssa;
  auto_vec<tree> worklist;
  worklist.safe_push (name);
  bitmap_set_bit (visited_ssa, SSA_NAME_VERSION (name));
  while (!worklist.is_empty ())
    {
      tree t = worklist.pop ();

      /* If it's obviously undefined, avoid further computations.  */
      if (ssa_undefined_value_p (t, true))
	return true;

      if (ssa_defined_default_def_p (t))
	continue;

      gimple *def = SSA_NAME_DEF_STMT (t);

      /* Check that all the PHI args are fully defined.  */
      if (gphi *phi = dyn_cast <gphi *> (def))
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree t = gimple_phi_arg_def (phi, i);
	      /* If an SSA has already been seen, it may be a loop,
		 but we can continue and ignore this use.  Otherwise,
		 add the SSA_NAME to the queue and visit it later.  */
	      if (TREE_CODE (t) == SSA_NAME
		  && bitmap_set_bit (visited_ssa, SSA_NAME_VERSION (t)))
		worklist.safe_push (t);
	    }
	  continue;
	}

      /* Uses in stmts always executed when the region header executes
	 are fine.  */
      if (dominated_by_p (CDI_DOMINATORS, loop->header, gimple_bb (def)))
	continue;

      /* Handle calls and memory loads conservatively.  */
      if (!is_gimple_assign (def)
	  || (gimple_assign_single_p (def)
	      && gimple_vuse (def)))
	return true;

      /* Check that any SSA names used to define NAME are also fully
	 defined.  */
      use_operand_p use_p;
      ssa_op_iter iter;
      FOR_EACH_SSA_USE_OPERAND (use_p, def, iter, SSA_OP_USE)
	{
	  tree t = USE_FROM_PTR (use_p);
	  /* If an SSA has already been seen, it may be a loop,
	     but we can continue and ignore this use.  Otherwise,
	     add the SSA_NAME to the queue and visit it later.  */
	  if (bitmap_set_bit (visited_ssa, SSA_NAME_VERSION (t)))
	    worklist.safe_push (t);
	}
    }
  return false;
}

/* Checks whether we can unswitch LOOP on condition at end of BB -- one of its
   basic blocks (for what it means see comments below).
   All candidates all filled to the provided vector CANDIDATES.
   OUTER_LOOP is updated to the innermost loop all found candidates are
   invariant in.  */

static void
find_unswitching_predicates_for_bb (basic_block bb, class loop *loop,
				    class loop *&outer_loop,
				    vec<unswitch_predicate *> &candidates,
				    unswitch_predicate *&hottest,
				    basic_block &hottest_bb)
{
  gimple *last, *def;
  tree use;
  basic_block def_bb;
  ssa_op_iter iter;

  /* BB must end in a simple conditional jump.  */
  last = last_stmt (bb);
  if (!last)
    return;

  if (gcond *stmt = safe_dyn_cast <gcond *> (last))
    {
      /* To keep the things simple, we do not directly remove the conditions,
	 but just replace tests with 0 != 0 resp. 1 != 0.  Prevent the infinite
	 loop where we would unswitch again on such a condition.  */
      if (gimple_cond_true_p (stmt) || gimple_cond_false_p (stmt))
	return;

      /* At least the LHS needs to be symbolic.  */
      if (TREE_CODE (gimple_cond_lhs (stmt)) != SSA_NAME)
	return;

      /* Condition must be invariant.  */
      FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	{
	  def = SSA_NAME_DEF_STMT (use);
	  def_bb = gimple_bb (def);
	  if (def_bb
	      && flow_bb_inside_loop_p (loop, def_bb))
	    return;
	  /* Unswitching on undefined values would introduce undefined
	     behavior that the original program might never exercise.  */
	  if (is_maybe_undefined (use, stmt, loop))
	    return;
	}
      /* Narrow OUTER_LOOP.  */
      if (outer_loop != loop)
	FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	  {
	    def = SSA_NAME_DEF_STMT (use);
	    def_bb = gimple_bb (def);
	    while (outer_loop != loop
		   && ((def_bb && flow_bb_inside_loop_p (outer_loop, def_bb))
		       || is_maybe_undefined (use, stmt, outer_loop)))
	      outer_loop = superloop_at_depth (loop,
					       loop_depth (outer_loop) + 1);
	  }

      unswitch_predicate *predicate = new unswitch_predicate (stmt);
      candidates.safe_push (predicate);
      /* If we unswitch on this predicate we isolate both paths, so
	 pick the highest count for updating of the hottest predicate
	 to unswitch on first.  */
      if (!hottest || predicate->count > hottest->count)
	{
	  hottest = predicate;
	  hottest_bb = bb;
	}
    }
  else if (gswitch *stmt = safe_dyn_cast <gswitch *> (last))
    {
      unsigned nlabels = gimple_switch_num_labels (stmt);
      tree idx = gimple_switch_index (stmt);
      tree idx_type = TREE_TYPE (idx);
      if (!gimple_range_ssa_p (idx) || nlabels < 1)
	return;
      /* Index must be invariant.  */
      def = SSA_NAME_DEF_STMT (idx);
      def_bb = gimple_bb (def);
      if (def_bb
	  && flow_bb_inside_loop_p (loop, def_bb))
	return;
      /* Unswitching on undefined values would introduce undefined
	 behavior that the original program might never exercise.  */
      if (is_maybe_undefined (idx, stmt, loop))
	return;
      /* Narrow OUTER_LOOP.  */
      while (outer_loop != loop
	     && ((def_bb && flow_bb_inside_loop_p (outer_loop, def_bb))
		 || is_maybe_undefined (idx, stmt, outer_loop)))
	outer_loop = superloop_at_depth (loop,
					 loop_depth (outer_loop) + 1);

      /* Build compound expression for all outgoing edges of the switch.  */
      auto_vec<tree, 16> preds;
      auto_vec<int_range_max> edge_range;
      preds.safe_grow_cleared (EDGE_COUNT (gimple_bb (stmt)->succs), true);
      edge_range.safe_grow_cleared (EDGE_COUNT (gimple_bb (stmt)->succs), true);
      edge e;
      edge_iterator ei;
      unsigned edge_index = 0;
      FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->succs)
	e->aux = (void *)(uintptr_t)edge_index++;
      for (unsigned i = 1; i < gimple_switch_num_labels (stmt); ++i)
	{
	  tree lab = gimple_switch_label (stmt, i);
	  tree cmp;
	  int_range<2> lab_range;
	  tree low = fold_convert (idx_type, CASE_LOW (lab));
	  if (CASE_HIGH (lab) != NULL_TREE)
	    {
	      tree high = fold_convert (idx_type, CASE_HIGH (lab));
	      tree cmp1 = fold_build2 (GE_EXPR, boolean_type_node, idx, low);
	      tree cmp2 = fold_build2 (LE_EXPR, boolean_type_node, idx, high);
	      cmp = fold_build2 (BIT_AND_EXPR, boolean_type_node, cmp1, cmp2);
	      lab_range.set (low, high);
	    }
	  else
	    {
	      cmp = fold_build2 (EQ_EXPR, boolean_type_node, idx, low);
	      lab_range.set (low, low);
	    }

	  /* Combine the expression with the existing one.  */
	  basic_block dest = label_to_block (cfun, CASE_LABEL (lab));
	  e = find_edge (gimple_bb (stmt), dest);
	  tree &expr = preds[(uintptr_t)e->aux];
	  if (expr == NULL_TREE)
	    expr = cmp;
	  else
	    expr = fold_build2 (BIT_IOR_EXPR, boolean_type_node, expr, cmp);
	  edge_range[(uintptr_t)e->aux].union_ (lab_range);
	}

      /* Now register the predicates.  */
      for (edge_index = 0; edge_index < preds.length (); ++edge_index)
	{
	  edge e = EDGE_SUCC (gimple_bb (stmt), edge_index);
	  e->aux = NULL;
	  if (preds[edge_index] != NULL_TREE)
	    {
	      unswitch_predicate *predicate
		= new unswitch_predicate (preds[edge_index], idx,
					  edge_index, e,
					  edge_range[edge_index]);
	      candidates.safe_push (predicate);
	      if (!hottest || predicate->count > hottest->count)
		{
		  hottest = predicate;
		  hottest_bb = bb;
		}
	    }
	}
    }
}

/* Merge ranges for the last item of PREDICATE_PATH with a predicate
   that shared the same LHS.  */

static void
merge_last (predicate_vector &predicate_path)
{
  unswitch_predicate *last_predicate = predicate_path.last ().first;

  for (int i = predicate_path.length () - 2; i >= 0; i--)
    {
      unswitch_predicate *predicate = predicate_path[i].first;
      bool true_edge = predicate_path[i].second;

      if (operand_equal_p (predicate->lhs, last_predicate->lhs, 0))
	{
	  irange &other = (true_edge ? predicate->merged_true_range
			   : predicate->merged_false_range);
	  last_predicate->merged_true_range.intersect (other);
	  last_predicate->merged_false_range.intersect (other);
	  return;
	}
    }
}

/* Add PREDICATE to PREDICATE_PATH on TRUE_EDGE.  */

static void
add_predicate_to_path (predicate_vector &predicate_path,
		       unswitch_predicate *predicate, bool true_edge)
{
  predicate->copy_merged_ranges ();
  predicate_path.safe_push (std::make_pair (predicate, true_edge));
  merge_last (predicate_path);
}

static bool
find_range_for_lhs (predicate_vector &predicate_path, tree lhs,
		    int_range_max &range)
{
  for (int i = predicate_path.length () - 1; i >= 0; i--)
    {
      unswitch_predicate *predicate = predicate_path[i].first;
      bool true_edge = predicate_path[i].second;

      if (operand_equal_p (predicate->lhs, lhs, 0))
	{
	  range = (true_edge ? predicate->merged_true_range
		   : predicate->merged_false_range);
	  return !range.undefined_p ();
	}
    }

  return false;
}

/* Simplifies STMT using the predicate we unswitched on which is the last
   in PREDICATE_PATH.  For switch statements add newly unreachable edges
   to IGNORED_EDGES (but do not set IGNORED_EDGE_FLAG on them).  */

static tree
evaluate_control_stmt_using_entry_checks (gimple *stmt,
					  predicate_vector &predicate_path,
					  int ignored_edge_flag,
					  hash_set<edge> *ignored_edges)
{
  unswitch_predicate *last_predicate = predicate_path.last ().first;
  bool true_edge = predicate_path.last ().second;

  if (gcond *cond = dyn_cast<gcond *> (stmt))
    {
      tree lhs = gimple_cond_lhs (cond);
      if (!operand_equal_p (lhs, last_predicate->lhs))
	return NULL_TREE;
      /* Try a symbolic match which works for floating point and fully
	 symbolic conditions.  */
      if (gimple_cond_code (cond) == TREE_CODE (last_predicate->condition)
	  && operand_equal_p (gimple_cond_rhs (cond),
			      TREE_OPERAND (last_predicate->condition, 1)))
	return true_edge ? boolean_true_node : boolean_false_node;
      /* Else try ranger if it supports LHS.  */
      else if (irange::supports_p (TREE_TYPE (lhs)))
	{
	  int_range<2> r;
	  int_range_max path_range;

	  if (find_range_for_lhs (predicate_path, lhs, path_range)
	      && fold_range (r, cond, path_range)
	      && r.singleton_p ())
	    return r.zero_p () ? boolean_false_node : boolean_true_node;
	}
    }
  else if (gswitch *swtch = dyn_cast<gswitch *> (stmt))
    {
      unsigned nlabels = gimple_switch_num_labels (swtch);

      tree idx = gimple_switch_index (swtch);

      /* Already folded switch.  */
      if (TREE_CONSTANT (idx))
	return NULL_TREE;

      int_range_max path_range;
      if (!find_range_for_lhs (predicate_path, idx, path_range))
	return NULL_TREE;

      tree result = NULL_TREE;
      edge single_edge = NULL;
      for (unsigned i = 0; i < nlabels; ++i)
	{
	  tree lab = gimple_switch_label (swtch, i);
	  basic_block dest = label_to_block (cfun, CASE_LABEL (lab));
	  edge e = find_edge (gimple_bb (stmt), dest);
	  if (e->flags & ignored_edge_flag)
	    continue;

	  int_range_max r;
	  if (!ranger->gori ().outgoing_edge_range_p (r, e, idx,
						      *get_global_range_query ()))
	    continue;
	  r.intersect (path_range);
	  if (r.undefined_p ())
	    ignored_edges->add (e);
	  else
	    {
	      if (!single_edge)
		{
		  single_edge = e;
		  result = CASE_LOW (lab);
		}
	      else if (single_edge != e)
		result = NULL;
	    }
	}

      /* Only one edge from the switch is alive.  */
      if (single_edge && result)
	return result;
    }

  return NULL_TREE;
}

/* Simplify LOOP based on PREDICATE_PATH where dead edges are properly
   marked.  */

static bool
simplify_loop_version (class loop *loop, predicate_vector &predicate_path,
		       int ignored_edge_flag, bitmap handled)
{
  bool changed = false;
  basic_block *bbs = get_loop_body (loop);

  hash_set<edge> ignored_edges;
  for (unsigned i = 0; i != loop->num_nodes; i++)
    {
      vec<unswitch_predicate *> &predicates = get_predicates_for_bb (bbs[i]);
      if (predicates.is_empty ())
	continue;

      gimple *stmt = last_stmt (bbs[i]);
      tree folded = evaluate_control_stmt_using_entry_checks (stmt,
							      predicate_path,
							      ignored_edge_flag,
							      &ignored_edges);

      if (gcond *cond = dyn_cast<gcond *> (stmt))
	{
	  if (folded)
	    {
	      /* Remove path.  */
	      if (integer_nonzerop (folded))
		gimple_cond_set_condition_from_tree (cond, boolean_true_node);
	      else
		gimple_cond_set_condition_from_tree (cond, boolean_false_node);

	      gcc_assert (predicates.length () == 1);
	      bitmap_set_bit (handled, predicates[0]->num);

	      update_stmt (cond);
	      changed = true;
	    }
	}
      else if (gswitch *swtch = dyn_cast<gswitch *> (stmt))
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bbs[i]->succs)
	    if (ignored_edges.contains (e))
	      e->flags |= ignored_edge_flag;

	  for (unsigned j = 0; j < predicates.length (); j++)
	    {
	      edge e = EDGE_SUCC (bbs[i], predicates[j]->edge_index);
	      if (ignored_edges.contains (e))
		bitmap_set_bit (handled, predicates[j]->num);
	    }

	  if (folded)
	    {
	      gimple_switch_set_index (swtch, folded);
	      update_stmt (swtch);
	      changed = true;
	    }
	}
    }

  free (bbs);
  return changed;
}

/* Evaluate reachable blocks in LOOP and call VISIT on them, aborting the
   DFS walk if VISIT returns true.  When PREDICATE_PATH is specified then
   take into account that when computing reachability, otherwise just
   look at the simplified state and IGNORED_EDGE_FLAG.  */

template <typename VisitOp>
static void
evaluate_bbs (class loop *loop, predicate_vector *predicate_path,
	      int ignored_edge_flag, VisitOp visit)
{
  auto_bb_flag reachable_flag (cfun);
  auto_vec<basic_block, 10> worklist (loop->num_nodes);
  auto_vec<basic_block, 10> reachable (loop->num_nodes);
  hash_set<edge> ignored_edges;

  loop->header->flags |= reachable_flag;
  worklist.quick_push (loop->header);
  reachable.safe_push (loop->header);

  while (!worklist.is_empty ())
    {
      edge e;
      edge_iterator ei;
      int flags = ignored_edge_flag;
      basic_block bb = worklist.pop ();

      if (visit (bb))
	break;

      gimple *last = last_stmt (bb);
      if (gcond *cond = safe_dyn_cast <gcond *> (last))
	{
	  if (gimple_cond_true_p (cond))
	    flags = EDGE_FALSE_VALUE;
	  else if (gimple_cond_false_p (cond))
	    flags = EDGE_TRUE_VALUE;
	  else if (predicate_path)
	    {
	      tree res;
	      if (!get_predicates_for_bb (bb).is_empty ()
		  && (res = evaluate_control_stmt_using_entry_checks
			      (cond, *predicate_path, ignored_edge_flag,
			       &ignored_edges)))
		flags = (integer_nonzerop (res)
			 ? EDGE_FALSE_VALUE : EDGE_TRUE_VALUE);
	    }
	}
      else if (gswitch *swtch = safe_dyn_cast<gswitch *> (last))
	if (predicate_path
	    && !get_predicates_for_bb (bb).is_empty ())
	  evaluate_control_stmt_using_entry_checks (swtch, *predicate_path,
						    ignored_edge_flag,
						    &ignored_edges);

      /* Note that for the moment we do not account reachable conditions
	 which are simplified to take a known edge as zero size nor
	 are we accounting for the required addition of the versioning
	 condition.  Those should cancel out conservatively.  */

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  basic_block dest = e->dest;

	  if (flow_bb_inside_loop_p (loop, dest)
	      && !(dest->flags & reachable_flag)
	      && !(e->flags & flags)
	      && !ignored_edges.contains (e))
	    {
	      dest->flags |= reachable_flag;
	      worklist.safe_push (dest);
	      reachable.safe_push (dest);
	    }
	}
    }

  /* Clear the flag from basic blocks.  */
  while (!reachable.is_empty ())
    reachable.pop ()->flags &= ~reachable_flag;
}

/* Evaluate how many instruction will we have if we unswitch LOOP (with BBS)
   based on PREDICATE predicate (using PREDICATE_PATH).  Store the
   result in TRUE_SIZE and FALSE_SIZE.  */

static void
evaluate_loop_insns_for_predicate (class loop *loop,
				   predicate_vector &predicate_path,
				   unswitch_predicate *predicate,
				   int ignored_edge_flag,
				   unsigned *true_size, unsigned *false_size)
{
  unsigned size = 0;
  auto sum_size = [&](basic_block bb) -> bool
    { size += (uintptr_t)bb->aux; return false; };

  add_predicate_to_path (predicate_path, predicate, true);
  evaluate_bbs (loop, &predicate_path, ignored_edge_flag, sum_size);
  predicate_path.pop ();
  unsigned true_loop_cost = size;

  size = 0;
  add_predicate_to_path (predicate_path, predicate, false);
  evaluate_bbs (loop, &predicate_path, ignored_edge_flag, sum_size);
  predicate_path.pop ();
  unsigned false_loop_cost = size;

  *true_size = true_loop_cost;
  *false_size = false_loop_cost;
}

/* Unswitch single LOOP.  PREDICATE_PATH contains so far used predicates
   for unswitching.  BUDGET is number of instruction for which we can increase
   the loop and is updated when unswitching occurs.  If HOTTEST is not
   NULL then pick this candidate as the one to unswitch on.  */

static bool
tree_unswitch_single_loop (class loop *loop, dump_user_location_t loc,
			   predicate_vector &predicate_path,
			   unsigned loop_size, unsigned &budget,
			   int ignored_edge_flag, bitmap handled,
			   unswitch_predicate *hottest, basic_block hottest_bb)
{
  class loop *nloop;
  bool changed = false;
  unswitch_predicate *predicate = NULL;
  basic_block predicate_bb = NULL;
  unsigned true_size = 0, false_size = 0;

  auto check_predicates = [&](basic_block bb) -> bool
    {
      for (auto pred : get_predicates_for_bb (bb))
	{
	  if (bitmap_bit_p (handled, pred->num))
	    continue;

	  evaluate_loop_insns_for_predicate (loop, predicate_path,
					     pred, ignored_edge_flag,
					     &true_size, &false_size);

	  /* We'll get LOOP replaced with a simplified version according
	     to PRED estimated to TRUE_SIZE and a copy simplified
	     according to the inverted PRED estimated to FALSE_SIZE.  */
	  if (true_size + false_size < budget + loop_size)
	    {
	      predicate = pred;
	      predicate_bb = bb;

	      /* There are cases where true_size and false_size add up to
		 less than the original loop_size.  We do not want to
		 grow the remaining budget because of that.  */
	      if (true_size + false_size > loop_size)
		budget -= (true_size + false_size - loop_size);

	      /* FIXME: right now we select first candidate, but we can
		 choose the cheapest or hottest one.  */
	      return true;
	    }
	  else if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, loc,
			     "not unswitching condition, cost too big "
			     "(%u insns copied to %u and %u)\n", loop_size,
			     true_size, false_size);
	}
      return false;
    };

  if (hottest)
    {
      predicate = hottest;
      predicate_bb = hottest_bb;
    }
  else
    /* Check predicates of reachable blocks.  */
    evaluate_bbs (loop, NULL, ignored_edge_flag, check_predicates);

  if (predicate != NULL)
    {
      if (!dbg_cnt (loop_unswitch))
	goto exit;

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
			   "unswitching %sloop %d on %qs with condition: %T\n",
			   loop->inner ? "outer " : "",
			   loop->num, predicate->switch_p ? "switch" : "if",
			   predicate->condition);
	  dump_printf_loc (MSG_NOTE, loc,
			   "optimized sizes estimated to %u (true) "
			   "and %u (false) from original size %u\n",
			   true_size, false_size, loop_size);
	}

      bitmap_set_bit (handled, predicate->num);
      initialize_original_copy_tables ();
      /* Unswitch the loop on this condition.  */
      nloop = tree_unswitch_loop (loop, EDGE_SUCC (predicate_bb,
						   predicate->edge_index),
				  predicate->condition);
      if (!nloop)
	{
	  free_original_copy_tables ();
	  goto exit;
	}

      /* Copy BB costs.  */
      basic_block *bbs2 = get_loop_body (nloop);
      for (unsigned i = 0; i < nloop->num_nodes; i++)
	bbs2[i]->aux = get_bb_original (bbs2[i])->aux;
      free (bbs2);

      free_original_copy_tables ();

      /* Update the SSA form after unswitching.  */
      update_ssa (TODO_update_ssa_no_phi);

      /* Invoke itself on modified loops.  */
      bitmap handled_copy = BITMAP_ALLOC (NULL);
      bitmap_copy (handled_copy, handled);
      add_predicate_to_path (predicate_path, predicate, false);
      changed |= simplify_loop_version (nloop, predicate_path,
					ignored_edge_flag, handled_copy);
      tree_unswitch_single_loop (nloop, loc, predicate_path,
				 false_size, budget,
				 ignored_edge_flag, handled_copy);
      predicate_path.pop ();
      BITMAP_FREE (handled_copy);

      /* FIXME: After unwinding above we have to reset all ->handled
	 flags as otherwise we fail to realize unswitching opportunities
	 in the below recursion.  See gcc.dg/loop-unswitch-16.c  */
      add_predicate_to_path (predicate_path, predicate, true);
      changed |= simplify_loop_version (loop, predicate_path,
					ignored_edge_flag, handled);
      tree_unswitch_single_loop (loop, loc, predicate_path,
				 true_size, budget,
				 ignored_edge_flag, handled);
      predicate_path.pop ();
      changed = true;
    }

exit:
  return changed;
}

/* Unswitch a LOOP w.r. to given EDGE_TRUE.  We only support unswitching of
   innermost loops.  COND is the condition determining which loop is entered;
   the new loop is entered if COND is true.  Returns NULL if impossible, new
   loop otherwise.  */

static class loop *
tree_unswitch_loop (class loop *loop, edge edge_true, tree cond)
{
  /* Some sanity checking.  */
  gcc_assert (flow_bb_inside_loop_p (loop, edge_true->src));
  gcc_assert (EDGE_COUNT (edge_true->src->succs) >= 2);

  profile_probability prob_true = edge_true->probability;
  return loop_version (loop, unshare_expr (cond),
		       NULL, prob_true,
		       prob_true.invert (),
		       prob_true, prob_true.invert (),
		       false);
}

/* Unswitch outer loops by hoisting invariant guard on
   inner loop without code duplication.  */
static bool
tree_unswitch_outer_loop (class loop *loop)
{
  edge exit, guard;
  HOST_WIDE_INT iterations;

  gcc_assert (loop->inner);
  if (loop->inner->next)
    return false;
  /* Accept loops with single exit only which is not from inner loop.  */
  exit = single_exit (loop);
  if (!exit || exit->src->loop_father != loop)
    return false;
  /* Check that phi argument of exit edge is not defined inside loop.  */
  if (!check_exit_phi (loop))
    return false;
  /* If the loop is not expected to iterate, there is no need
      for unswitching.  */
  iterations = estimated_loop_iterations_int (loop);
  if (iterations < 0)
    iterations = likely_max_loop_iterations_int (loop);
  if (iterations >= 0 && iterations <= 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, find_loop_location (loop),
			 "Not unswitching, loop is not expected"
			 " to iterate\n");
      return false;
    }

  bool changed = false;
  auto_vec<gimple *> dbg_to_reset;
  while ((guard = find_loop_guard (loop, dbg_to_reset)))
    {
      hoist_guard (loop, guard);
      for (gimple *debug_stmt : dbg_to_reset)
	{
	  gimple_debug_bind_reset_value (debug_stmt);
	  update_stmt (debug_stmt);
	}
      dbg_to_reset.truncate (0);
      changed = true;
    }
  return changed;
}

/* Checks if the body of the LOOP is within an invariant guard.  If this
   is the case, returns the edge that jumps over the real body of the loop,
   otherwise returns NULL.  */

static edge
find_loop_guard (class loop *loop, vec<gimple *> &dbg_to_reset)
{
  basic_block header = loop->header;
  edge guard_edge, te, fe;
  basic_block *body = NULL;
  unsigned i;
  tree use;
  ssa_op_iter iter;

  /* We check for the following situation:

     while (1)
       {
	 [header]]
         loop_phi_nodes;
	 something1;
	 if (cond1)
	   body;
	 nvar = phi(orig, bvar) ... for all variables changed in body;
	 [guard_end]
	 something2;
	 if (cond2)
	   break;
	 something3;
       }

     where:

     1) cond1 is loop invariant
     2) If cond1 is false, then the loop is essentially empty; i.e.,
	a) nothing in something1, something2 and something3 has side
	   effects
	b) anything defined in something1, something2 and something3
	   is not used outside of the loop.  */

  gcond *cond;
  do
    {
      basic_block next = NULL;
      if (single_succ_p (header))
	next = single_succ (header);
      else
	{
	  cond = safe_dyn_cast <gcond *> (last_stmt (header));
	  if (! cond)
	    return NULL;
	  extract_true_false_edges_from_block (header, &te, &fe);
	  /* Make sure to skip earlier hoisted guards that are left
	     in place as if (true).  */
	  if (gimple_cond_true_p (cond))
	    next = te->dest;
	  else if (gimple_cond_false_p (cond))
	    next = fe->dest;
	  else
	    break;
	}
      /* Never traverse a backedge.  */
      if (header->loop_father->header == next)
	return NULL;
      header = next;
    }
  while (1);
  if (!flow_bb_inside_loop_p (loop, te->dest)
      || !flow_bb_inside_loop_p (loop, fe->dest))
    return NULL;

  if (just_once_each_iteration_p (loop, te->dest)
      || (single_succ_p (te->dest)
	  && just_once_each_iteration_p (loop, single_succ (te->dest))))
    {
      if (just_once_each_iteration_p (loop, fe->dest))
	return NULL;
      guard_edge = te;
    }
  else if (just_once_each_iteration_p (loop, fe->dest)
	   || (single_succ_p (fe->dest)
	       && just_once_each_iteration_p (loop, single_succ (fe->dest))))
    guard_edge = fe;
  else
    return NULL;

  dump_user_location_t loc = find_loop_location (loop);

  /* Guard edge must skip inner loop.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->inner->header,
      guard_edge == fe ? te->dest : fe->dest))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			 "Guard edge %d --> %d is not around the loop!\n",
			 guard_edge->src->index, guard_edge->dest->index);
      return NULL;
    }
  if (guard_edge->dest == loop->latch)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			 "Guard edge destination is loop latch.\n");
      return NULL;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, loc,
		     "Considering guard %d -> %d in loop %d\n",
		     guard_edge->src->index, guard_edge->dest->index,
		     loop->num);
  /* Check if condition operands do not have definitions inside loop since
     any bb copying is not performed.  */
  FOR_EACH_SSA_TREE_OPERAND (use, cond, iter, SSA_OP_USE)
    {
      gimple *def = SSA_NAME_DEF_STMT (use);
      basic_block def_bb = gimple_bb (def);
      if (def_bb
          && flow_bb_inside_loop_p (loop, def_bb))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, loc, "guard operands have definitions"
			     " inside loop\n");
	  return NULL;
	}
    }

  body = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = body[i];
      if (bb->loop_father != loop)
	continue;
      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			     "Block %d is marked as irreducible in loop\n",
			     bb->index);
	  guard_edge = NULL;
	  goto end;
	}
      if (!empty_bb_without_guard_p (loop, bb, dbg_to_reset))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			     "Block %d has side effects\n", bb->index);
	  guard_edge = NULL;
	  goto end;
	}
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, loc,
		     "suitable to hoist\n");
end:
  if (body)
    free (body);
  return guard_edge;
}

/* Returns true if
   1) no statement in BB has side effects
   2) assuming that edge GUARD is always taken, all definitions in BB
      are noy used outside of the loop.
   KNOWN_INVARIANTS is a set of ssa names we know to be invariant, and
   PROCESSED is a set of ssa names for that we already tested whether they
   are invariant or not.  Uses in debug stmts outside of the loop are
   pushed to DBG_TO_RESET.  */

static bool
empty_bb_without_guard_p (class loop *loop, basic_block bb,
			  vec<gimple *> &dbg_to_reset)
{
  basic_block exit_bb = single_exit (loop)->src;
  bool may_be_used_outside = (bb == exit_bb
			      || !dominated_by_p (CDI_DOMINATORS, bb, exit_bb));
  tree name;
  ssa_op_iter op_iter;

  /* Phi nodes do not have side effects, but their results might be used
     outside of the loop.  */
  if (may_be_used_outside)
    {
      for (gphi_iterator gsi = gsi_start_phis (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  name = PHI_RESULT (phi);
	  if (virtual_operand_p (name))
	    continue;

	  if (used_outside_loop_p (loop, name, dbg_to_reset))
	    return false;
	}
    }

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	continue;

      if (gimple_has_side_effects (stmt))
	return false;

      if (gimple_vdef(stmt))
	return false;

      FOR_EACH_SSA_TREE_OPERAND (name, stmt, op_iter, SSA_OP_DEF)
	{
	  if (may_be_used_outside
	      && used_outside_loop_p (loop, name, dbg_to_reset))
	    return false;
	}
    }
  return true;
}

/* Return true if NAME is used outside of LOOP.  Pushes debug stmts that
   have such uses to DBG_TO_RESET but do not consider such uses.  */

static bool
used_outside_loop_p (class loop *loop, tree name, vec<gimple *> &dbg_to_reset)
{
  imm_use_iterator it;
  use_operand_p use;

  FOR_EACH_IMM_USE_FAST (use, it, name)
    {
      gimple *stmt = USE_STMT (use);
      if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	{
	  if (!is_gimple_debug (stmt))
	    return true;
	  dbg_to_reset.safe_push (stmt);
	}
    }

  return false;
}

/* Return argument for loop preheader edge in header virtual phi if any.  */

static tree
get_vop_from_header (class loop *loop)
{
  for (gphi_iterator gsi = gsi_start_phis (loop->header);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi)))
	continue;
      return PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
    }
  return NULL_TREE;
}

/* Move the check of GUARD outside of LOOP.  */

static void
hoist_guard (class loop *loop, edge guard)
{
  edge exit = single_exit (loop);
  edge preh = loop_preheader_edge (loop);
  basic_block pre_header = preh->src;
  basic_block bb;
  edge te, fe, e, new_edge;
  gimple *stmt;
  basic_block guard_bb = guard->src;
  edge not_guard;
  gimple_stmt_iterator gsi;
  int flags = 0;
  bool fix_dom_of_exit;
  gcond *cond_stmt, *new_cond_stmt;

  bb = get_immediate_dominator (CDI_DOMINATORS, exit->dest);
  fix_dom_of_exit = flow_bb_inside_loop_p (loop, bb);
  gsi = gsi_last_bb (guard_bb);
  stmt = gsi_stmt (gsi);
  gcc_assert (gimple_code (stmt) == GIMPLE_COND);
  cond_stmt = as_a <gcond *> (stmt);
  extract_true_false_edges_from_block (guard_bb, &te, &fe);
  /* Insert guard to PRE_HEADER.  */
  if (!empty_block_p (pre_header))
    gsi = gsi_last_bb (pre_header);
  else
    gsi = gsi_start_bb (pre_header);
  /* Create copy of COND_STMT.  */
  new_cond_stmt = gimple_build_cond (gimple_cond_code (cond_stmt),
				     gimple_cond_lhs (cond_stmt),
				     gimple_cond_rhs (cond_stmt),
				     NULL_TREE, NULL_TREE);
  gsi_insert_after (&gsi, new_cond_stmt, GSI_NEW_STMT);
  /* Convert COND_STMT to true/false conditional.  */
  if (guard == te)
    gimple_cond_make_false (cond_stmt);
  else
    gimple_cond_make_true (cond_stmt);
  update_stmt (cond_stmt);
  /* Create new loop pre-header.  */
  e = split_block (pre_header, last_stmt (pre_header));

  dump_user_location_t loc = find_loop_location (loop);

  if (dump_enabled_p ())
    {
      char buffer[64];
      guard->probability.dump (buffer);

      dump_printf_loc (MSG_NOTE, loc,
		       "Moving guard %i->%i (prob %s) to bb %i, "
		       "new preheader is %i\n",
		       guard->src->index, guard->dest->index,
		       buffer, e->src->index, e->dest->index);
    }

  gcc_assert (loop_preheader_edge (loop)->src == e->dest);

  if (guard == fe)
    {
      e->flags = EDGE_TRUE_VALUE;
      flags |= EDGE_FALSE_VALUE;
      not_guard = te;
    }
  else
    {
      e->flags = EDGE_FALSE_VALUE;
      flags |= EDGE_TRUE_VALUE;
      not_guard = fe;
    }
  new_edge = make_edge (pre_header, exit->dest, flags);

  /* Determine the probability that we skip the loop.  Assume that loop has
     same average number of iterations regardless outcome of guard.  */
  new_edge->probability = guard->probability;
  profile_count skip_count = guard->src->count.nonzero_p ()
		   ? guard->count ().apply_scale (pre_header->count,
					       guard->src->count)
		   : guard->count ().apply_probability (new_edge->probability);

  if (skip_count > e->count ())
    {
      fprintf (dump_file, "  Capping count; expect profile inconsistency\n");
      skip_count = e->count ();
    }
  if (dump_enabled_p ())
    {
      char buffer[64];
      new_edge->probability.dump (buffer);

      dump_printf_loc (MSG_NOTE, loc,
		       "Estimated probability of skipping loop is %s\n",
		       buffer);
    }

  /* Update profile after the transform:

     First decrease count of path from newly hoisted loop guard
     to loop header...  */
  e->probability = new_edge->probability.invert ();
  e->dest->count = e->count ();

  /* ... now update profile to represent that original guard will be optimized
     away ...  */
  guard->probability = profile_probability::never ();
  not_guard->probability = profile_probability::always ();

  /* ... finally scale everything in the loop except for guarded basic blocks
     where profile does not change.  */
  basic_block *body = get_loop_body (loop);

  for (unsigned int i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = body[i];
      if (!dominated_by_p (CDI_DOMINATORS, bb, not_guard->dest))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, loc,
			     "Scaling nonguarded BBs in loop: %i\n",
			     bb->index);
	  if (e->probability.initialized_p ())
            scale_bbs_frequencies (&bb, 1, e->probability);
  	}
    }

  if (fix_dom_of_exit)
    set_immediate_dominator (CDI_DOMINATORS, exit->dest, pre_header);
  /* Add NEW_ADGE argument for all phi in post-header block.  */
  bb = exit->dest;
  for (gphi_iterator gsi = gsi_start_phis (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree arg;
      if (virtual_operand_p (gimple_phi_result (phi)))
	{
	  arg = get_vop_from_header (loop);
	  if (arg == NULL_TREE)
	    /* Use exit edge argument.  */
	    arg =  PHI_ARG_DEF_FROM_EDGE (phi, exit);
	  add_phi_arg (phi, arg, new_edge, UNKNOWN_LOCATION);
	}
      else
	{
	  /* Use exit edge argument.  */
	  arg = PHI_ARG_DEF_FROM_EDGE (phi, exit);
	  add_phi_arg (phi, arg, new_edge, UNKNOWN_LOCATION);
	}
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
		     "Guard hoisted\n");

  free (body);
}

/* Return true if phi argument for exit edge can be used
   for edge around loop.  */

static bool
check_exit_phi (class loop *loop)
{
  edge exit = single_exit (loop);
  basic_block pre_header = loop_preheader_edge (loop)->src;

  for (gphi_iterator gsi = gsi_start_phis (exit->dest);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree arg;
      gimple *def;
      basic_block def_bb;
      if (virtual_operand_p (gimple_phi_result (phi)))
	continue;
      arg = PHI_ARG_DEF_FROM_EDGE (phi, exit);
      if (TREE_CODE (arg) != SSA_NAME)
	continue;
      def = SSA_NAME_DEF_STMT (arg);
      if (!def)
	continue;
      def_bb = gimple_bb (def);
      if (!def_bb)
	continue;
      if (!dominated_by_p (CDI_DOMINATORS, pre_header, def_bb))
	/* Definition inside loop!  */
	return false;
      /* Check loop closed phi invariant.  */
      if (!flow_bb_inside_loop_p (def_bb->loop_father, pre_header))
	return false;
    }
  return true;
}

/* Remove all dead cases from switches that are unswitched.  */

static void
clean_up_after_unswitching (int ignored_edge_flag)
{
  basic_block bb;
  edge e;
  edge_iterator ei;
  bool removed_edge = false;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gswitch *stmt= safe_dyn_cast <gswitch *> (last_stmt (bb));
      if (stmt && !CONSTANT_CLASS_P (gimple_switch_index (stmt)))
	{
	  unsigned nlabels = gimple_switch_num_labels (stmt);
	  unsigned index = 1;
	  tree lab = gimple_switch_default_label (stmt);
	  edge default_e = find_edge (gimple_bb (stmt),
				      label_to_block (cfun, CASE_LABEL (lab)));
	  for (unsigned i = 1; i < nlabels; ++i)
	    {
	      tree lab = gimple_switch_label (stmt, i);
	      basic_block dest = label_to_block (cfun, CASE_LABEL (lab));
	      edge e = find_edge (gimple_bb (stmt), dest);
	      if (e == NULL)
		; /* The edge is already removed.  */
	      else if (e->flags & ignored_edge_flag)
		{
		  /* We may not remove the default label so we also have
		     to preserve its edge.  But we can remove the
		     non-default CASE sharing the edge.  */
		  if (e != default_e)
		    {
		      remove_edge (e);
		      removed_edge = true;
		    }
		}
	      else
		{
		  gimple_switch_set_label (stmt, index, lab);
		  ++index;
		}
	    }

	  if (index != nlabels)
	    gimple_switch_set_num_labels (stmt, index);
	}

      /* Clean up the ignored_edge_flag from edges.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	e->flags &= ~ignored_edge_flag;
    }

  /* If we removed an edge we possibly have to recompute dominators.  */
  if (removed_edge)
    free_dominance_info (CDI_DOMINATORS);
}

/* Loop unswitching pass.  */

namespace {

const pass_data pass_data_tree_unswitch =
{
  GIMPLE_PASS, /* type */
  "unswitch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP_UNSWITCH, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_unswitch : public gimple_opt_pass
{
public:
  pass_tree_unswitch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_unswitch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return flag_unswitch_loops != 0; }
  unsigned int execute (function *) final override;

}; // class pass_tree_unswitch

unsigned int
pass_tree_unswitch::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return tree_ssa_unswitch_loops (fun);
}

} // anon namespace

gimple_opt_pass *
make_pass_tree_unswitch (gcc::context *ctxt)
{
  return new pass_tree_unswitch (ctxt);
}


