/* Callgraph construction.
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
#include "except.h"
#include "ipa-inline.h"

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

  t = canonicalize_constructor_val (t, NULL);
  if (!t)
    t = *tp;
  else if (t != *tp)
    *tp = t;

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
	  struct cgraph_node *node = cgraph_get_create_real_symbol_node (decl);
	  if (!ctx->only_vars)
	    cgraph_mark_address_taken_node (node);
	  ipa_record_reference ((symtab_node)ctx->varpool_node,
				(symtab_node)node,
			        IPA_REF_ADDR, NULL);
	}

      if (TREE_CODE (decl) == VAR_DECL)
	{
	  struct varpool_node *vnode = varpool_node_for_decl (decl);
	  ipa_record_reference ((symtab_node)ctx->varpool_node,
				(symtab_node)vnode,
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
      break;
    }

  return NULL_TREE;
}

/* Record references to typeinfos in the type list LIST.  */

static void
record_type_list (struct cgraph_node *node, tree list)
{
  for (; list; list = TREE_CHAIN (list))
    {
      tree type = TREE_VALUE (list);
      
      if (TYPE_P (type))
	type = lookup_type_for_runtime (type);
      STRIP_NOPS (type);
      if (TREE_CODE (type) == ADDR_EXPR)
	{
	  type = TREE_OPERAND (type, 0);
	  if (TREE_CODE (type) == VAR_DECL)
	    {
	      struct varpool_node *vnode = varpool_node_for_decl (type);
	      ipa_record_reference ((symtab_node)node,
				    (symtab_node)vnode,
				    IPA_REF_ADDR, NULL);
	    }
	}
    }
}

/* Record all references we will introduce by producing EH tables
   for NODE.  */

static void
record_eh_tables (struct cgraph_node *node, struct function *fun)
{
  eh_region i;

  if (DECL_FUNCTION_PERSONALITY (node->symbol.decl))
    {
      struct cgraph_node *per_node;

      per_node = cgraph_get_create_real_symbol_node (DECL_FUNCTION_PERSONALITY (node->symbol.decl));
      ipa_record_reference ((symtab_node)node, (symtab_node)per_node, IPA_REF_ADDR, NULL);
      cgraph_mark_address_taken_node (per_node);
    }

  i = fun->eh->region_tree;
  if (!i)
    return;

  while (1)
    {
      switch (i->type)
	{
	case ERT_CLEANUP:
	case ERT_MUST_NOT_THROW:
	  break;

	case ERT_TRY:
	  {
	    eh_catch c;
	    for (c = i->u.eh_try.first_catch; c; c = c->next_catch)
	      record_type_list (node, c->type_list);
	  }
	  break;

	case ERT_ALLOWED_EXCEPTIONS:
	  record_type_list (node, i->u.allowed.type_list);
	  break;
	}
      /* If there are sub-regions, process them.  */
      if (i->inner)
	i = i->inner;
      /* If there are peers, process them.  */
      else if (i->next_peer)
	i = i->next_peer;
      /* Otherwise, step back up the tree to the next peer.  */
      else
	{
	  do
	    {
	      i = i->outer;
	      if (i == NULL)
		return;
	    }
	  while (i->next_peer == NULL);
	  i = i->next_peer;
	}
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
mark_address (gimple stmt, tree addr, void *data)
{
  addr = get_base_address (addr);
  if (TREE_CODE (addr) == FUNCTION_DECL)
    {
      struct cgraph_node *node = cgraph_get_create_real_symbol_node (addr);
      cgraph_mark_address_taken_node (node);
      ipa_record_reference ((symtab_node)data,
			    (symtab_node)node,
			    IPA_REF_ADDR, stmt);
    }
  else if (addr && TREE_CODE (addr) == VAR_DECL
	   && (TREE_STATIC (addr) || DECL_EXTERNAL (addr)))
    {
      struct varpool_node *vnode = varpool_node_for_decl (addr);

      ipa_record_reference ((symtab_node)data,
			    (symtab_node)vnode,
			    IPA_REF_ADDR, stmt);
    }

  return false;
}

/* Mark load of T.  */

static bool
mark_load (gimple stmt, tree t, void *data)
{
  t = get_base_address (t);
  if (t && TREE_CODE (t) == FUNCTION_DECL)
    {
      /* ??? This can happen on platforms with descriptors when these are
	 directly manipulated in the code.  Pretend that it's an address.  */
      struct cgraph_node *node = cgraph_get_create_real_symbol_node (t);
      cgraph_mark_address_taken_node (node);
      ipa_record_reference ((symtab_node)data,
			    (symtab_node)node,
			    IPA_REF_ADDR, stmt);
    }
  else if (t && TREE_CODE (t) == VAR_DECL
	   && (TREE_STATIC (t) || DECL_EXTERNAL (t)))
    {
      struct varpool_node *vnode = varpool_node_for_decl (t);

      ipa_record_reference ((symtab_node)data,
			    (symtab_node)vnode,
			    IPA_REF_LOAD, stmt);
    }
  return false;
}

/* Mark store of T.  */

static bool
mark_store (gimple stmt, tree t, void *data)
{
  t = get_base_address (t);
  if (t && TREE_CODE (t) == VAR_DECL
      && (TREE_STATIC (t) || DECL_EXTERNAL (t)))
    {
      struct varpool_node *vnode = varpool_node_for_decl (t);

      ipa_record_reference ((symtab_node)data,
			    (symtab_node)vnode,
			    IPA_REF_STORE, stmt);
     }
  return false;
}

/* Record all references from NODE that are taken in statement STMT.  */
void
ipa_record_stmt_references (struct cgraph_node *node, gimple stmt)
{
  walk_stmt_load_store_addr_ops (stmt, node, mark_load, mark_store,
				 mark_address);
}

/* Create cgraph edges for function calls.
   Also look for functions and variables having addresses taken.  */

static unsigned int
build_cgraph_edges (void)
{
  basic_block bb;
  struct cgraph_node *node = cgraph_get_node (current_function_decl);
  struct pointer_set_t *visited_nodes = pointer_set_create ();
  gimple_stmt_iterator gsi;
  tree decl;
  unsigned ix;

  /* Create the callgraph edges and record the nodes referenced by the function.
     body.  */
  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree decl;

	  if (is_gimple_debug (stmt))
	    continue;

	  if (is_gimple_call (stmt))
	    {
	      int freq = compute_call_stmt_bb_frequency (current_function_decl,
							 bb);
	      decl = gimple_call_fndecl (stmt);
	      if (decl)
		cgraph_create_edge (node, cgraph_get_create_node (decl),
				    stmt, bb->count, freq);
	      else
		cgraph_create_indirect_edge (node, stmt,
					     gimple_call_flags (stmt),
					     bb->count, freq);
	    }
	  ipa_record_stmt_references (node, stmt);
	  if (gimple_code (stmt) == GIMPLE_OMP_PARALLEL
	      && gimple_omp_parallel_child_fn (stmt))
	    {
	      tree fn = gimple_omp_parallel_child_fn (stmt);
	      ipa_record_reference ((symtab_node)node,
				    (symtab_node)cgraph_get_create_real_symbol_node (fn),
				    IPA_REF_ADDR, stmt);
	    }
	  if (gimple_code (stmt) == GIMPLE_OMP_TASK)
	    {
	      tree fn = gimple_omp_task_child_fn (stmt);
	      if (fn)
		ipa_record_reference ((symtab_node)node,
				      (symtab_node) cgraph_get_create_real_symbol_node (fn),
				      IPA_REF_ADDR, stmt);
	      fn = gimple_omp_task_copy_fn (stmt);
	      if (fn)
		ipa_record_reference ((symtab_node)node,
				      (symtab_node)cgraph_get_create_real_symbol_node (fn),
				      IPA_REF_ADDR, stmt);
	    }
	}
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	ipa_record_stmt_references (node, gsi_stmt (gsi));
   }

  /* Look for initializers of constant variables and private statics.  */
  FOR_EACH_LOCAL_DECL (cfun, ix, decl)
    if (TREE_CODE (decl) == VAR_DECL
	&& (TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
	&& !DECL_HAS_VALUE_EXPR_P (decl))
      varpool_finalize_decl (decl);
  record_eh_tables (node, cfun);

  pointer_set_destroy (visited_nodes);
  return 0;
}

namespace {

const pass_data pass_data_build_cgraph_edges =
{
  GIMPLE_PASS, /* type */
  "*build_cgraph_edges", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_build_cgraph_edges : public gimple_opt_pass
{
public:
  pass_build_cgraph_edges(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_build_cgraph_edges, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () { return build_cgraph_edges (); }

}; // class pass_build_cgraph_edges

} // anon namespace

gimple_opt_pass *
make_pass_build_cgraph_edges (gcc::context *ctxt)
{
  return new pass_build_cgraph_edges (ctxt);
}

/* Record references to functions and other variables present in the
   initial value of DECL, a variable.
   When ONLY_VARS is true, we mark needed only variables, not functions.  */

void
record_references_in_initializer (tree decl, bool only_vars)
{
  struct pointer_set_t *visited_nodes = pointer_set_create ();
  struct varpool_node *node = varpool_node_for_decl (decl);
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
  struct cgraph_node *node = cgraph_get_node (current_function_decl);
  gimple_stmt_iterator gsi;

  cgraph_node_remove_callees (node);
  ipa_remove_all_references (&node->symbol.ref_list);

  node->count = ENTRY_BLOCK_PTR->count;

  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree decl;

	  if (is_gimple_call (stmt))
	    {
	      int freq = compute_call_stmt_bb_frequency (current_function_decl,
							 bb);
	      decl = gimple_call_fndecl (stmt);
	      if (decl)
		cgraph_create_edge (node, cgraph_get_create_node (decl), stmt,
				    bb->count, freq);
	      else
		cgraph_create_indirect_edge (node, stmt,
					     gimple_call_flags (stmt),
					     bb->count, freq);
	    }
	  ipa_record_stmt_references (node, stmt);
	}
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	ipa_record_stmt_references (node, gsi_stmt (gsi));
    }
  record_eh_tables (node, cfun);
  gcc_assert (!node->global.inlined_to);

  return 0;
}

/* Rebuild cgraph edges for current function node.  This needs to be run after
   passes that don't update the cgraph.  */

void
cgraph_rebuild_references (void)
{
  basic_block bb;
  struct cgraph_node *node = cgraph_get_node (current_function_decl);
  gimple_stmt_iterator gsi;

  ipa_remove_all_references (&node->symbol.ref_list);

  node->count = ENTRY_BLOCK_PTR->count;

  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	ipa_record_stmt_references (node, gsi_stmt (gsi));
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	ipa_record_stmt_references (node, gsi_stmt (gsi));
    }
  record_eh_tables (node, cfun);
}

namespace {

const pass_data pass_data_rebuild_cgraph_edges =
{
  GIMPLE_PASS, /* type */
  "*rebuild_cgraph_edges", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_CGRAPH, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_rebuild_cgraph_edges : public gimple_opt_pass
{
public:
  pass_rebuild_cgraph_edges(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_rebuild_cgraph_edges, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_rebuild_cgraph_edges (ctxt_); }
  unsigned int execute () { return rebuild_cgraph_edges (); }

}; // class pass_rebuild_cgraph_edges

} // anon namespace

gimple_opt_pass *
make_pass_rebuild_cgraph_edges (gcc::context *ctxt)
{
  return new pass_rebuild_cgraph_edges (ctxt);
}


static unsigned int
remove_cgraph_callee_edges (void)
{
  struct cgraph_node *node = cgraph_get_node (current_function_decl);
  cgraph_node_remove_callees (node);
  ipa_remove_all_references (&node->symbol.ref_list);
  return 0;
}

namespace {

const pass_data pass_data_remove_cgraph_callee_edges =
{
  GIMPLE_PASS, /* type */
  "*remove_cgraph_callee_edges", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_remove_cgraph_callee_edges : public gimple_opt_pass
{
public:
  pass_remove_cgraph_callee_edges(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_remove_cgraph_callee_edges, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () {
    return new pass_remove_cgraph_callee_edges (ctxt_);
  }
  unsigned int execute () { return remove_cgraph_callee_edges (); }

}; // class pass_remove_cgraph_callee_edges

} // anon namespace

gimple_opt_pass *
make_pass_remove_cgraph_callee_edges (gcc::context *ctxt)
{
  return new pass_remove_cgraph_callee_edges (ctxt);
}
