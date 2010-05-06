/* Callgraph construction.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
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
#include "ipa-utils.h"

/* Context of record_reference.  */
struct record_reference_ctx
{
  bool only_vars;
  struct varpool_node *varpool_node;
};

/* Walk tree and record all calls and references to functions/variables.
   Called via walk_tree: TP is pointer to tree to be examined.
   When DATA is non-null, record references to callgraph.
   */

static tree
record_reference (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;
  tree decl;
  struct record_reference_ctx *ctx = (struct record_reference_ctx *)data;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case FUNCTION_DECL:
      gcc_unreachable ();
      break;

    case FDESC_EXPR:
    case ADDR_EXPR:
      /* Record dereferences to the functions.  This makes the
	 functions reachable unconditionally.  */
      decl = get_base_var (*tp);
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (!ctx->only_vars)
	  cgraph_mark_address_taken_node (cgraph_node (decl));
	  ipa_record_reference (NULL, ctx->varpool_node,
			        cgraph_node (decl), NULL,
			        IPA_REF_ADDR, NULL);
	}

      if (TREE_CODE (decl) == VAR_DECL)
	{
	  struct varpool_node *vnode = varpool_node (decl);
	  if (lang_hooks.callgraph.analyze_expr)
	    lang_hooks.callgraph.analyze_expr (&decl, walk_subtrees);
	  varpool_mark_needed_node (vnode);
	  if (vnode->alias && vnode->extra_name)
	    vnode = vnode->extra_name;
	  ipa_record_reference (NULL, ctx->varpool_node,
				NULL, vnode,
				IPA_REF_ADDR, NULL);
	}
      *walk_subtrees = 0;
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

/* Reset inlining information of all incoming call edges of NODE.  */

void
reset_inline_failed (struct cgraph_node *node)
{
  struct cgraph_edge *e;

  for (e = node->callers; e; e = e->next_caller)
    {
      e->callee->global.inlined_to = NULL;
      if (!node->analyzed)
	e->inline_failed = CIF_BODY_NOT_AVAILABLE;
      else if (node->local.redefined_extern_inline)
	e->inline_failed = CIF_REDEFINED_EXTERN_INLINE;
      else if (!node->local.inlinable)
	e->inline_failed = CIF_FUNCTION_NOT_INLINABLE;
      else if (e->call_stmt_cannot_inline_p)
	e->inline_failed = CIF_MISMATCHED_ARGUMENTS;
      else
	e->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
    }
}

/* Computes the frequency of the call statement so that it can be stored in
   cgraph_edge.  BB is the basic block of the call statement.  */
int
compute_call_stmt_bb_frequency (tree decl, basic_block bb)
{
  int entry_freq = ENTRY_BLOCK_PTR_FOR_FUNCTION
  		     (DECL_STRUCT_FUNCTION (decl))->frequency;
  int freq = bb->frequency;

  if (profile_status_for_function (DECL_STRUCT_FUNCTION (decl)) == PROFILE_ABSENT)
    return CGRAPH_FREQ_BASE;

  if (!entry_freq)
    entry_freq = 1, freq++;

  freq = freq * CGRAPH_FREQ_BASE / entry_freq;
  if (freq > CGRAPH_FREQ_MAX)
    freq = CGRAPH_FREQ_MAX;

  return freq;
}

/* Mark address taken in STMT.  */

static bool
mark_address (gimple stmt ATTRIBUTE_UNUSED, tree addr,
	      void *data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (addr) == FUNCTION_DECL)
    {
      struct cgraph_node *node = cgraph_node (addr);
      cgraph_mark_address_taken_node (node);
      ipa_record_reference ((struct cgraph_node *)data, NULL,
			    node, NULL,
			    IPA_REF_ADDR, stmt);
    }
  else
    {
      addr = get_base_address (addr);
      if (addr && TREE_CODE (addr) == VAR_DECL
	  && (TREE_STATIC (addr) || DECL_EXTERNAL (addr)))
	{
	  struct varpool_node *vnode = varpool_node (addr);
	  int walk_subtrees;

	  if (lang_hooks.callgraph.analyze_expr)
	    lang_hooks.callgraph.analyze_expr (&addr, &walk_subtrees);
	  varpool_mark_needed_node (vnode);
	  if (vnode->alias && vnode->extra_name)
	    vnode = vnode->extra_name;
	  ipa_record_reference ((struct cgraph_node *)data, NULL,
				NULL, vnode,
				IPA_REF_ADDR, stmt);
	}
    }

  return false;
}

/* Mark load of T.  */

static bool
mark_load (gimple stmt ATTRIBUTE_UNUSED, tree t,
	   void *data ATTRIBUTE_UNUSED)
{
  t = get_base_address (t);
  if (TREE_CODE (t) == VAR_DECL
      && (TREE_STATIC (t) || DECL_EXTERNAL (t)))
    {
      struct varpool_node *vnode = varpool_node (t);
      int walk_subtrees;

      if (lang_hooks.callgraph.analyze_expr)
	lang_hooks.callgraph.analyze_expr (&t, &walk_subtrees);
      varpool_mark_needed_node (vnode);
      if (vnode->alias && vnode->extra_name)
	vnode = vnode->extra_name;
      ipa_record_reference ((struct cgraph_node *)data, NULL,
			    NULL, vnode,
			    IPA_REF_LOAD, stmt);
    }
  return false;
}

/* Mark store of T.  */

static bool
mark_store (gimple stmt ATTRIBUTE_UNUSED, tree t,
	    void *data ATTRIBUTE_UNUSED)
{
  t = get_base_address (t);
  if (TREE_CODE (t) == VAR_DECL
      && (TREE_STATIC (t) || DECL_EXTERNAL (t)))
    {
      struct varpool_node *vnode = varpool_node (t);
      int walk_subtrees;

      if (lang_hooks.callgraph.analyze_expr)
	lang_hooks.callgraph.analyze_expr (&t, &walk_subtrees);
      varpool_mark_needed_node (vnode);
      if (vnode->alias && vnode->extra_name)
	vnode = vnode->extra_name;
      ipa_record_reference ((struct cgraph_node *)data, NULL,
			    NULL, vnode,
			    IPA_REF_STORE, NULL);
     }
  return false;
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
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree decl;

	  if (is_gimple_call (stmt) && (decl = gimple_call_fndecl (stmt)))
	    cgraph_create_edge (node, cgraph_node (decl), stmt,
				bb->count,
				compute_call_stmt_bb_frequency
				  (current_function_decl, bb),
				bb->loop_depth);
	  walk_stmt_load_store_addr_ops (stmt, node, mark_load,
					 mark_store, mark_address);
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
      for (gsi = gsi_start (phi_nodes (bb)); !gsi_end_p (gsi); gsi_next (&gsi))
	walk_stmt_load_store_addr_ops (gsi_stmt (gsi), node,
				       mark_load, mark_store, mark_address);
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
    }

  pointer_set_destroy (visited_nodes);
  return 0;
}

struct gimple_opt_pass pass_build_cgraph_edges =
{
 {
  GIMPLE_PASS,
  "*build_cgraph_edges",			/* name */
  NULL,					/* gate */
  build_cgraph_edges,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

/* Record references to functions and other variables present in the
   initial value of DECL, a variable.
   When ONLY_VARS is true, we mark needed only variables, not functions.  */

void
record_references_in_initializer (tree decl, bool only_vars)
{
  struct pointer_set_t *visited_nodes = pointer_set_create ();
  struct varpool_node *node = varpool_node (decl);
  struct record_reference_ctx ctx = {false, NULL};

  ctx.varpool_node = node;
  ctx.only_vars = only_vars;
  walk_tree (&DECL_INITIAL (decl), record_reference,
             &ctx, visited_nodes);
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
  ipa_remove_all_references (&node->ref_list);

  node->count = ENTRY_BLOCK_PTR->count;

  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree decl;

	  if (is_gimple_call (stmt) && (decl = gimple_call_fndecl (stmt)))
	    cgraph_create_edge (node, cgraph_node (decl), stmt,
				bb->count,
				compute_call_stmt_bb_frequency
				  (current_function_decl, bb),
				bb->loop_depth);
	  walk_stmt_load_store_addr_ops (stmt, node, mark_load,
					 mark_store, mark_address);

	}
      for (gsi = gsi_start (phi_nodes (bb)); !gsi_end_p (gsi); gsi_next (&gsi))
	walk_stmt_load_store_addr_ops (gsi_stmt (gsi), node,
				       mark_load, mark_store, mark_address);
    }
  gcc_assert (!node->global.inlined_to);

  return 0;
}

struct gimple_opt_pass pass_rebuild_cgraph_edges =
{
 {
  GIMPLE_PASS,
  "*rebuild_cgraph_edges",		/* name */
  NULL,					/* gate */
  rebuild_cgraph_edges,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
 }
};


static unsigned int
remove_cgraph_callee_edges (void)
{
  cgraph_node_remove_callees (cgraph_node (current_function_decl));
  return 0;
}

struct gimple_opt_pass pass_remove_cgraph_callee_edges =
{
 {
  GIMPLE_PASS,
  "*remove_cgraph_callee_edges",		/* name */
  NULL,					/* gate */
  remove_cgraph_callee_edges,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
 }
};
