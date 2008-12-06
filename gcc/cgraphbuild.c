/* Callgraph construction.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-flow.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "cgraph.h"
#include "intl.h"
#include "gimple.h"
#include "tree-pass.h"

/* Walk tree and record all calls and references to functions/variables.
   Called via walk_tree: TP is pointer to tree to be examined.  */

static tree
record_reference (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;
  tree decl;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      if (TREE_STATIC (t) || DECL_EXTERNAL (t))
	{
	  varpool_mark_needed_node (varpool_node (t));
	  if (lang_hooks.callgraph.analyze_expr)
	    return lang_hooks.callgraph.analyze_expr (tp, walk_subtrees);
	}
      break;

    case FDESC_EXPR:
    case ADDR_EXPR:
      /* Record dereferences to the functions.  This makes the
	 functions reachable unconditionally.  */
      decl = TREE_OPERAND (*tp, 0);
      if (TREE_CODE (decl) == FUNCTION_DECL)
	cgraph_mark_needed_node (cgraph_node (decl));
      break;

    default:
      /* Save some cycles by not walking types and declaration as we
	 won't find anything useful there anyway.  */
      if (IS_TYPE_OR_DECL_P (*tp))
	{
	  *walk_subtrees = 0;
	  break;
	}

      if ((unsigned int) TREE_CODE (t) >= LAST_AND_UNUSED_TREE_CODE)
	return lang_hooks.callgraph.analyze_expr (tp, walk_subtrees);
      break;
    }

  return NULL_TREE;
}

/* Give initial reasons why inlining would fail on all calls from
   NODE.  Those get either nullified or usually overwritten by more precise
   reason later.  */

static void
initialize_inline_failed (struct cgraph_node *node)
{
  struct cgraph_edge *e;

  for (e = node->callers; e; e = e->next_caller)
    {
      gcc_assert (!e->callee->global.inlined_to);
      gcc_assert (e->inline_failed);
      if (node->local.redefined_extern_inline)
	e->inline_failed = N_("redefined extern inline functions are not "
			   "considered for inlining");
      else if (!node->local.inlinable)
	e->inline_failed = N_("function not inlinable");
      else if (gimple_call_cannot_inline_p (e->call_stmt))
	e->inline_failed = N_("mismatched arguments");
      else
	e->inline_failed = N_("function not considered for inlining");
    }
}

/* Computes the frequency of the call statement so that it can be stored in
   cgraph_edge.  BB is the basic block of the call statement.  */
int
compute_call_stmt_bb_frequency (basic_block bb)
{
  int entry_freq = ENTRY_BLOCK_PTR->frequency;
  int freq = bb->frequency;

  if (!entry_freq)
    entry_freq = 1, freq++;

  freq = freq * CGRAPH_FREQ_BASE / entry_freq;
  if (freq > CGRAPH_FREQ_MAX)
    freq = CGRAPH_FREQ_MAX;

  return freq;
}

/* Create cgraph edges for function calls.
   Also look for functions and variables having addresses taken.  */

static unsigned int
build_cgraph_edges (void)
{
  basic_block bb;
  struct cgraph_node *node = cgraph_node (current_function_decl);
  struct pointer_set_t *visited_nodes = pointer_set_create ();
  gimple_stmt_iterator gsi;
  tree step;

  /* Create the callgraph edges and record the nodes referenced by the function.
     body.  */
  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);
	tree decl;

	if (is_gimple_call (stmt) && (decl = gimple_call_fndecl (stmt)))
	  {
	    size_t i;
	    size_t n = gimple_call_num_args (stmt);
	    cgraph_create_edge (node, cgraph_node (decl), stmt,
				bb->count, compute_call_stmt_bb_frequency (bb),
				bb->loop_depth);
	    for (i = 0; i < n; i++)
	      walk_tree (gimple_call_arg_ptr (stmt, i), record_reference,
			 node, visited_nodes);
	    if (gimple_call_lhs (stmt))
	      walk_tree (gimple_call_lhs_ptr (stmt), record_reference, node,
		         visited_nodes);
	  }
	else
	  {
	    struct walk_stmt_info wi;
	    memset (&wi, 0, sizeof (wi));
	    wi.info = node;
	    wi.pset = visited_nodes;
	    walk_gimple_op (stmt, record_reference, &wi);
	    if (gimple_code (stmt) == GIMPLE_OMP_PARALLEL
		&& gimple_omp_parallel_child_fn (stmt))
	      {
		tree fn = gimple_omp_parallel_child_fn (stmt);
		cgraph_mark_needed_node (cgraph_node (fn));
	      }
	    if (gimple_code (stmt) == GIMPLE_OMP_TASK)
	      {
		tree fn = gimple_omp_task_child_fn (stmt);
		if (fn)
		  cgraph_mark_needed_node (cgraph_node (fn));
		fn = gimple_omp_task_copy_fn (stmt);
		if (fn)
		  cgraph_mark_needed_node (cgraph_node (fn));
	      }
	  }
      }

  /* Look for initializers of constant variables and private statics.  */
  for (step = cfun->local_decls;
       step;
       step = TREE_CHAIN (step))
    {
      tree decl = TREE_VALUE (step);
      if (TREE_CODE (decl) == VAR_DECL
	  && (TREE_STATIC (decl) && !DECL_EXTERNAL (decl)))
	varpool_finalize_decl (decl);
      else if (TREE_CODE (decl) == VAR_DECL && DECL_INITIAL (decl))
	walk_tree (&DECL_INITIAL (decl), record_reference, node, visited_nodes);
    }

  pointer_set_destroy (visited_nodes);
  initialize_inline_failed (node);
  return 0;
}

struct gimple_opt_pass pass_build_cgraph_edges =
{
 {
  GIMPLE_PASS,
  NULL,					/* name */
  NULL,					/* gate */
  build_cgraph_edges,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

/* Record references to functions and other variables present in the
   initial value of DECL, a variable.  */

void
record_references_in_initializer (tree decl)
{
  struct pointer_set_t *visited_nodes = pointer_set_create ();
  walk_tree (&DECL_INITIAL (decl), record_reference, NULL, visited_nodes);
  pointer_set_destroy (visited_nodes);
}

/* Rebuild cgraph edges for current function node.  This needs to be run after
   passes that don't update the cgraph.  */

unsigned int
rebuild_cgraph_edges (void)
{
  basic_block bb;
  struct cgraph_node *node = cgraph_node (current_function_decl);
  gimple_stmt_iterator gsi;

  cgraph_node_remove_callees (node);

  node->count = ENTRY_BLOCK_PTR->count;

  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);
	tree decl;

	if (is_gimple_call (stmt) && (decl = gimple_call_fndecl (stmt)))
	  cgraph_create_edge (node, cgraph_node (decl), stmt,
			      bb->count, compute_call_stmt_bb_frequency (bb),
			      bb->loop_depth);

      }
  initialize_inline_failed (node);
  gcc_assert (!node->global.inlined_to);
  return 0;
}

struct gimple_opt_pass pass_rebuild_cgraph_edges =
{
 {
  GIMPLE_PASS,
  NULL,					/* name */
  NULL,					/* gate */
  rebuild_cgraph_edges,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
 }
};
