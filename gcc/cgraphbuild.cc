/* Callgraph construction.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "gimple-walk.h"
#include "ipa-utils.h"
#include "except.h"
#include "gimplify.h"

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
  record_reference_ctx *ctx = (record_reference_ctx *)data;

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
	  cgraph_node *node = cgraph_node::get_create (decl);
	  if (!ctx->only_vars)
	    node->mark_address_taken ();
	  ctx->varpool_node->create_reference (node, IPA_REF_ADDR);
	}

      if (VAR_P (decl))
	{
	  /* Replace vars with their DECL_VALUE_EXPR if any.
	     This is normally done during gimplification, but
	     static var initializers are never gimplified.  */
	  if (DECL_HAS_VALUE_EXPR_P (decl))
	    {
	      tree *p;
	      for (p = tp; *p != decl; p = &TREE_OPERAND (*p, 0))
		;
	      *p = unshare_expr (DECL_VALUE_EXPR (decl));
	      return record_reference (tp, walk_subtrees, data);
	    }
	  varpool_node *vnode = varpool_node::get_create (decl);
	  ctx->varpool_node->create_reference (vnode, IPA_REF_ADDR);
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
record_type_list (cgraph_node *node, tree list)
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
	  if (VAR_P (type))
	    {
	      varpool_node *vnode = varpool_node::get_create (type);
	      node->create_reference (vnode, IPA_REF_ADDR);
	    }
	}
    }
}

/* Record all references we will introduce by producing EH tables
   for NODE.  */

static void
record_eh_tables (cgraph_node *node, function *fun)
{
  eh_region i;

  if (DECL_FUNCTION_PERSONALITY (node->decl))
    {
      tree per_decl = DECL_FUNCTION_PERSONALITY (node->decl);
      cgraph_node *per_node = cgraph_node::get_create (per_decl);

      node->create_reference (per_node, IPA_REF_ADDR);
      per_node->mark_address_taken ();
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
  return bb->count.to_cgraph_frequency
      (ENTRY_BLOCK_PTR_FOR_FN (DECL_STRUCT_FUNCTION (decl))->count);
}

/* Mark address taken in STMT.  */

static bool
mark_address (gimple *stmt, tree addr, tree, void *data)
{
  addr = get_base_address (addr);
  if (TREE_CODE (addr) == FUNCTION_DECL)
    {
      cgraph_node *node = cgraph_node::get_create (addr);
      node->mark_address_taken ();
      ((symtab_node *)data)->create_reference (node, IPA_REF_ADDR, stmt);
    }
  else if (addr && VAR_P (addr)
	   && (TREE_STATIC (addr) || DECL_EXTERNAL (addr)))
    {
      varpool_node *vnode = varpool_node::get_create (addr);

      ((symtab_node *)data)->create_reference (vnode, IPA_REF_ADDR, stmt);
    }

  return false;
}

/* Mark load of T.  */

static bool
mark_load (gimple *stmt, tree t, tree, void *data)
{
  t = get_base_address (t);
  if (t && TREE_CODE (t) == FUNCTION_DECL)
    {
      /* ??? This can happen on platforms with descriptors when these are
	 directly manipulated in the code.  Pretend that it's an address.  */
      cgraph_node *node = cgraph_node::get_create (t);
      node->mark_address_taken ();
      ((symtab_node *)data)->create_reference (node, IPA_REF_ADDR, stmt);
    }
  else if (t && VAR_P (t) && (TREE_STATIC (t) || DECL_EXTERNAL (t)))
    {
      varpool_node *vnode = varpool_node::get_create (t);

      ((symtab_node *)data)->create_reference (vnode, IPA_REF_LOAD, stmt);
    }
  return false;
}

/* Mark store of T.  */

static bool
mark_store (gimple *stmt, tree t, tree, void *data)
{
  t = get_base_address (t);
  if (t && VAR_P (t) && (TREE_STATIC (t) || DECL_EXTERNAL (t)))
    {
      varpool_node *vnode = varpool_node::get_create (t);

      ((symtab_node *)data)->create_reference (vnode, IPA_REF_STORE, stmt);
     }
  return false;
}

/* Record all references from cgraph_node that are taken in statement STMT.  */

void
cgraph_node::record_stmt_references (gimple *stmt)
{
  walk_stmt_load_store_addr_ops (stmt, this, mark_load, mark_store,
				 mark_address);
}

/* Create cgraph edges for function calls.
   Also look for functions and variables having addresses taken.  */

namespace {

const pass_data pass_data_build_cgraph_edges =
{
  GIMPLE_PASS, /* type */
  "*build_cgraph_edges", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
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
  pass_build_cgraph_edges (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_build_cgraph_edges, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute (function *) final override;

}; // class pass_build_cgraph_edges

unsigned int
pass_build_cgraph_edges::execute (function *fun)
{
  basic_block bb;
  cgraph_node *node = cgraph_node::get (current_function_decl);
  gimple_stmt_iterator gsi;
  tree decl;
  unsigned ix;

  /* Create the callgraph edges and record the nodes referenced by the function.
     body.  */
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree decl;

	  if (is_gimple_debug (stmt))
	    continue;

	  if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
	    {
	      decl = gimple_call_fndecl (call_stmt);
	      if (decl)
		node->create_edge (cgraph_node::get_create (decl), call_stmt, bb->count);
	      else if (gimple_call_internal_p (call_stmt))
		;
	      else
		node->create_indirect_edge (call_stmt,
					    gimple_call_flags (call_stmt),
					    bb->count);
	    }
	  node->record_stmt_references (stmt);
	  if (gomp_parallel *omp_par_stmt = dyn_cast <gomp_parallel *> (stmt))
	    {
	      tree fn = gimple_omp_parallel_child_fn (omp_par_stmt);
	      node->create_reference (cgraph_node::get_create (fn),
				      IPA_REF_ADDR, stmt);
	    }
	  if (gimple_code (stmt) == GIMPLE_OMP_TASK)
	    {
	      tree fn = gimple_omp_task_child_fn (stmt);
	      if (fn)
		node->create_reference (cgraph_node::get_create (fn),
					IPA_REF_ADDR, stmt);
	      fn = gimple_omp_task_copy_fn (stmt);
	      if (fn)
		node->create_reference (cgraph_node::get_create (fn),
					IPA_REF_ADDR, stmt);
	    }
	}
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	node->record_stmt_references (gsi_stmt (gsi));
   }

  /* Look for initializers of constant variables and private statics.  */
  FOR_EACH_LOCAL_DECL (fun, ix, decl)
    if (VAR_P (decl)
	&& (TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
	&& !DECL_HAS_VALUE_EXPR_P (decl)
	&& TREE_TYPE (decl) != error_mark_node)
      varpool_node::finalize_decl (decl);
  record_eh_tables (node, fun);

  return 0;
}

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
  varpool_node *node = varpool_node::get_create (decl);
  hash_set<tree> visited_nodes;
  record_reference_ctx ctx = {false, NULL};

  ctx.varpool_node = node;
  ctx.only_vars = only_vars;
  walk_tree (&DECL_INITIAL (decl), record_reference,
             &ctx, &visited_nodes);
}

/* Rebuild cgraph edges for current function node.  This needs to be run after
   passes that don't update the cgraph.  */

unsigned int
cgraph_edge::rebuild_edges (void)
{
  basic_block bb;
  cgraph_node *node = cgraph_node::get (current_function_decl);
  gimple_stmt_iterator gsi;

  node->remove_callees ();
  node->remove_all_references ();

  node->count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree decl;

	  if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
	    {
	      decl = gimple_call_fndecl (call_stmt);
	      if (decl)
		node->create_edge (cgraph_node::get_create (decl), call_stmt,
				   bb->count);
	      else if (gimple_call_internal_p (call_stmt))
		;
	      else
		node->create_indirect_edge (call_stmt,
					    gimple_call_flags (call_stmt),
					    bb->count);
	    }
	  node->record_stmt_references (stmt);
	}
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	node->record_stmt_references (gsi_stmt (gsi));
    }
  record_eh_tables (node, cfun);
  gcc_assert (!node->inlined_to);
  return 0;
}

/* Rebuild cgraph references for current function node.  This needs to be run
   after passes that don't update the cgraph.  */

void
cgraph_edge::rebuild_references (void)
{
  basic_block bb;
  cgraph_node *node = cgraph_node::get (current_function_decl);
  gimple_stmt_iterator gsi;
  ipa_ref *ref = NULL;
  int i;

  /* Keep speculative references for further cgraph edge expansion.  */
  for (i = 0; node->iterate_reference (i, ref);)
    if (!ref->speculative)
      ref->remove_reference ();
    else
      i++;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	node->record_stmt_references (gsi_stmt (gsi));
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	node->record_stmt_references (gsi_stmt (gsi));
    }
  record_eh_tables (node, cfun);
}

namespace {

const pass_data pass_data_rebuild_cgraph_edges =
{
  GIMPLE_PASS, /* type */
  "*rebuild_cgraph_edges", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
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
  pass_rebuild_cgraph_edges (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_rebuild_cgraph_edges, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override
  {
    return new pass_rebuild_cgraph_edges (m_ctxt);
  }
  unsigned int execute (function *) final override
  {
    return cgraph_edge::rebuild_edges ();
  }

}; // class pass_rebuild_cgraph_edges

} // anon namespace

gimple_opt_pass *
make_pass_rebuild_cgraph_edges (gcc::context *ctxt)
{
  return new pass_rebuild_cgraph_edges (ctxt);
}


namespace {

const pass_data pass_data_remove_cgraph_callee_edges =
{
  GIMPLE_PASS, /* type */
  "*remove_cgraph_callee_edges", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
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
  pass_remove_cgraph_callee_edges (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_remove_cgraph_callee_edges, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override {
    return new pass_remove_cgraph_callee_edges (m_ctxt);
  }
  unsigned int execute (function *) final override;

}; // class pass_remove_cgraph_callee_edges

unsigned int
pass_remove_cgraph_callee_edges::execute (function *)
{
  cgraph_node *node = cgraph_node::get (current_function_decl);
  node->remove_callees ();
  node->remove_all_references ();
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_remove_cgraph_callee_edges (gcc::context *ctxt)
{
  return new pass_remove_cgraph_callee_edges (ctxt);
}
