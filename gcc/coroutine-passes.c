/* coroutine expansion and optimisation passes.

   Copyright (C) 2018-2019 Free Software Foundation, Inc.

 Contributed by Iain Sandoe <iain@sandoe.co.uk> under contract to Facebook.

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
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "internal-fn.h"
#include "langhooks.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "gimple-fold.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "gimple-pretty-print.h"
#include "cfghooks.h"

/* Iterate through the statements in the sequence, lowering the coro
   FE builtins.  */

static tree
lower_coro_builtin (gimple_stmt_iterator *gsi, bool *handled_ops_p,
		    struct walk_stmt_info * wi ATTRIBUTE_UNUSED)
{
  gimple *stmt = gsi_stmt (*gsi);

  *handled_ops_p = !gimple_has_substatements (stmt);
  if (gimple_code (stmt) != GIMPLE_CALL)
    return NULL_TREE;

  tree decl = gimple_call_fndecl (stmt);
  if (decl && fndecl_built_in_p (decl, BUILT_IN_NORMAL))
    {
      unsigned call_idx = 0;
      switch (DECL_FUNCTION_CODE (decl))
      {
      default:
	break;
      case BUILT_IN_CORO_PROMISE:
	{
	  /* If we are discarding this, then skip it; the function has no
	     side-effects.  */
	  tree lhs = gimple_call_lhs (stmt);
	  if (!lhs)
	    {
	      gsi_remove (gsi, true);
	      *handled_ops_p = true;
	      return NULL_TREE;
	    }
	  /* The coro frame starts with three pointers (to the resume and
	     destroy() and suspended_p ()functions.  These are followed by
	     the promise which is aligned as per type [or user attribute].
	     The input pointer is the first argument.
	     The promise alignment is the second and the third is a bool
	     that is true when we are converting from a promise ptr to a
	     frame pointer, and false for the inverse.  */
	  tree ptr = gimple_call_arg (stmt, 0);
	  tree align_t = gimple_call_arg (stmt, 1);
	  tree from = gimple_call_arg (stmt, 2);
	  gcc_assert (TREE_CODE (align_t) == INTEGER_CST);
	  gcc_assert (TREE_CODE (from) == INTEGER_CST);
	  bool dir = wi::to_wide (from) != 0;
	  tree vptr = build_pointer_type (void_type_node);
	  HOST_WIDE_INT promise_align = TREE_INT_CST_LOW (align_t);
	  HOST_WIDE_INT psize = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (vptr));
	  HOST_WIDE_INT align = TYPE_ALIGN_UNIT (vptr);
	  align = MAX (align, promise_align);
	  psize *= 3; /* Start with three pointers.  */
	  psize = ROUND_UP (psize, align);
	  HOST_WIDE_INT offs = dir ? -psize : psize;
	  tree repl = build2 (POINTER_PLUS_EXPR, vptr, ptr,
			      build_int_cst (sizetype, offs));
	  gassign *grpl = gimple_build_assign (lhs, repl);
	  gsi_replace (gsi, grpl, true);
	  *handled_ops_p = true;
	}
	break;
      case BUILT_IN_CORO_DESTROY:
	call_idx = 1;
	/* FALLTHROUGH */
      case BUILT_IN_CORO_RESUME:
	{
	  tree ptr = gimple_call_arg (stmt, 0); /* frame ptr.  */
	  tree vptr = build_pointer_type (void_type_node);
	  HOST_WIDE_INT psize = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (vptr));
	  HOST_WIDE_INT offset = call_idx * psize;
	  tree fntype = TREE_TYPE (decl);
	  tree fntype_ptr = build_pointer_type (fntype);
	  tree fntype_ppp = build_pointer_type (fntype_ptr);
	  tree indirect = fold_build2 (MEM_REF, fntype_ptr, ptr,
				       wide_int_to_tree (fntype_ppp, offset));
	  tree f_ptr_tmp = make_ssa_name (TYPE_MAIN_VARIANT (fntype_ptr));
	  gassign *get_fptr = gimple_build_assign (f_ptr_tmp, indirect);
	  gsi_insert_before (gsi, get_fptr, GSI_SAME_STMT);
	  gimple_call_set_fn (static_cast<gcall *>(stmt), f_ptr_tmp);
	  *handled_ops_p = true;
	}
	break;
      case BUILT_IN_CORO_IS_SUSPENDED:
	{
	  /* If we are discarding this, then skip it; the function has no
	     side-effects.  */
	  tree lhs = gimple_call_lhs (stmt);
	  if (!lhs)
	    {
	      gsi_remove (gsi, true);
	      *handled_ops_p = true;
	      return NULL_TREE;
	    }
	  tree ptr = gimple_call_arg (stmt, 0); /* frame ptr.  */
	  tree vptr = build_pointer_type (void_type_node);
	  HOST_WIDE_INT psize = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (vptr));
	  HOST_WIDE_INT offset = 2 * psize;
	  tree fntype = TREE_TYPE (decl);
	  tree fntype_ptr = build_pointer_type (fntype);
	  tree fntype_ppp = build_pointer_type (fntype_ptr);
	  tree indirect = fold_build2 (MEM_REF, fntype_ptr, ptr,
				       wide_int_to_tree (fntype_ppp, offset));
	  tree f_ptr_tmp = make_ssa_name (TYPE_MAIN_VARIANT (fntype_ptr));
	  gassign *get_fptr = gimple_build_assign (f_ptr_tmp, indirect);
	  gsi_insert_before (gsi, get_fptr, GSI_SAME_STMT);
	  gimple_call_set_fn (static_cast<gcall *>(stmt), f_ptr_tmp);
	  *handled_ops_p = true;
	}
	break;
      case BUILT_IN_CORO_DONE:
	{
	  /* If we are discarding this, then skip it; the function has no
	     side-effects.  */
	  tree lhs = gimple_call_lhs (stmt);
	  if (!lhs)
	    {
	      gsi_remove (gsi, true);
	      *handled_ops_p = true;
	      return NULL_TREE;
	    }
	  /* When we're done, the resume fn is set to NULL.  */
	  tree ptr = gimple_call_arg (stmt, 0); /* frame ptr.  */
	  tree vptr = build_pointer_type (void_type_node);
	  tree vpp = build_pointer_type (vptr);
	  tree indirect = fold_build2 (MEM_REF, vpp, ptr,
				       wide_int_to_tree (vpp, 0));
	  tree d_ptr_tmp = make_ssa_name (TYPE_MAIN_VARIANT (vptr));
	  gassign *get_dptr = gimple_build_assign (d_ptr_tmp, indirect);
	  gsi_insert_before (gsi, get_dptr, GSI_SAME_STMT);
	  tree done = fold_build2 (EQ_EXPR, boolean_type_node, d_ptr_tmp,
				   wide_int_to_tree (vptr, 0));
	  gassign *get_res = gimple_build_assign (lhs, done);
	  gsi_replace (gsi, get_res, true);
	  *handled_ops_p = true;
	}
	break;
      }
    }
  return NULL_TREE;
}

/* Main entry point for lowering coroutine FE builtins.  */

static unsigned int
execute_lower_coro_builtins (void)
{
  struct walk_stmt_info wi;
  gimple_seq body;

  body = gimple_body (current_function_decl);
  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq_mod (&body, lower_coro_builtin, NULL, &wi);
  gimple_set_body (current_function_decl, body);

  return 0;
}

namespace {

const pass_data pass_data_coroutine_lower_builtins  =
{
  GIMPLE_PASS, /* type */
  "coro-lower-builtins", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0  /* todo_flags_finish */
};

class pass_coroutine_lower_builtins : public gimple_opt_pass
{
public:
  pass_coroutine_lower_builtins  (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_coroutine_lower_builtins , ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_coroutines; };

  virtual unsigned int execute (function *f ATTRIBUTE_UNUSED)
    {
      return execute_lower_coro_builtins ();
    }

}; // class pass_coroutine_lower_builtins

} // anon namespace

gimple_opt_pass *
make_pass_coroutine_lower_builtins (gcc::context *ctxt)
{
  return new pass_coroutine_lower_builtins (ctxt);
}

/* Iterate the function exanding the IFNs.  */

/* Helper that returns the first gimple statement that has a vuse in a
   BB. */
static gimple *
first_vuser (basic_block bb)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_vuse (stmt))
        return stmt;
    }
  return NULL;
}

/* This is a wrapper for redirect_edge_and_branch() that tries to minimise
   the VSSA changes, where possible.  */
static void
move_edge_and_update_vdef (edge e, tree act_vdef,
			   basic_block old_bb, basic_block new_bb)
{
  if (dump_file)
    fprintf (dump_file, "redirecting edge from bb %u to bb %u\n",
	     old_bb->index, new_bb->index);

  e = redirect_edge_and_branch (e, new_bb);
  if (!e && dump_file)
      fprintf (dump_file, "failed to redirect edge ..  \n");

  /* Die if we failed.  */
  gcc_checking_assert (e);

  /* redirect_edge_and_branch() doesn't do anything specific to ensure that
     the VSSA is sane or intact.  The new block connection will be added to
     the new block's incoming VPHI, if it has one - but there will be no
     vdef associated, causing a verification fail. */
  gphi *dest_phi = get_virtual_phi (new_bb);
  if (dest_phi)
    /* So, fist we look to see if the new destination has a VPHI and, if so
       add our vdef into that; we're then done.  */
    add_phi_arg (dest_phi, act_vdef, e, UNKNOWN_LOCATION);
  else if (gimple *first_user = first_vuser (new_bb))
    {
      /* The virtual operands renamer gets a bit crazy with duplication of
	 blocks - so we try to mimimise the differential when we can see
	 a reasonable way forward.
	 In the case that the target block has an incoming vuse, (but no
	 VPHI, of course), we splice in a VPHI and update its content to
	 refer to both the old and new edges. */
      tree p_vdef = make_ssa_name (gimple_vop(cfun));
      dest_phi = create_phi_node (p_vdef, new_bb);
      add_phi_arg (dest_phi, act_vdef, e, UNKNOWN_LOCATION);

      /* Get the first VUSE in the BB, which will be on a stmt, because
	 there's no VPHI.  */
      tree orig = gimple_vuse (first_user);

      /* Put that VUSE onto the original (singular) edge that the BB had
	 before we started.  */
      edge en;
      edge_iterator ein;
      FOR_EACH_EDGE (en, ein, new_bb->preds)
        if (en != e)
	  add_phi_arg (dest_phi, orig, en, UNKNOWN_LOCATION);

      /* Finally, replace that op with the VDEF from our new  VPHI.  */
      use_operand_p use_p;
      imm_use_iterator iu_it;
      gimple  *tf;
      FOR_EACH_IMM_USE_STMT (tf, iu_it, orig)
	{
	  if (tf == first_user)
	    FOR_EACH_IMM_USE_ON_STMT (use_p, iu_it)
	      SET_USE (use_p, p_vdef);
	}
    }
  /* else, we punt and let the renamer do its thing - which does seem to end
     up with quite a lot of label duplication.  */
}

static unsigned int
execute_expand_coro_ifns (void)
{
  /* Don't rebuild stuff unless we have to. */
  unsigned int todoflags = 0;

  /* Some of the possible YIELD points will hopefully have been removed by
     earlier optimisations, record the ones that are present.  */
  hash_map <int_hash <HOST_WIDE_INT, -1, -2>, tree> destinations;
  bool changed = false;

  basic_block bb;

  gimple_stmt_iterator gsi;
  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
      {
	gimple *stmt = gsi_stmt (gsi);
	if (!is_gimple_call (stmt) || !gimple_call_internal_p (stmt))
	  {
	    gsi_next (&gsi);
	    continue;
	  }
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_CO_ACTOR:
	    changed = true;
	    gsi_next (&gsi);
	    break;
	  case IFN_CO_YIELD:
	    {
	    /* .CO_YIELD (NUM, FINAL, RES_LAB, DEST_LAB);
	       NUM = await number.
	       FINAL = 1 if this is the final_suspend() await.
	       RES_LAB = resume point label.
	       DEST_LAB = destroy point label.
	    */
	    if (dump_file)
	      fprintf (dump_file, "saw CO_YIELD in BB %u\n", bb->index);
	    tree num = gimple_call_arg (stmt, 0); /* yield point.  */
	    HOST_WIDE_INT idx = TREE_INT_CST_LOW (num);
	    bool existed;
	    tree res_tgt = TREE_OPERAND (gimple_call_arg (stmt, 2), 0);
	    tree &res_dest = destinations.get_or_insert (idx, &existed);
	    if (existed && dump_file)
	      {
		fprintf (dump_file, "duplicate YIELD RESUME point ("
				     HOST_WIDE_INT_PRINT_DEC ") ?\n", idx);
		debug_gimple_stmt (stmt);
	      }
	    else
	      res_dest = res_tgt;
	    tree dst_tgt = TREE_OPERAND (gimple_call_arg (stmt, 3), 0);
	    tree &dst_dest = destinations.get_or_insert (idx+1, &existed);
	    if (existed && dump_file)
	      {
		fprintf (dump_file, "duplicate YIELD DESTROY point ("
				     HOST_WIDE_INT_PRINT_DEC ") ?\n", idx+1);
		debug_gimple_stmt (stmt);
	      }
	    else
	      dst_dest = dst_tgt;
	    unlink_stmt_vdef (stmt);
	    /* lose the co_yield.  */
	    gsi_remove (&gsi, true);
	    stmt = gsi_stmt (gsi); /* next. */
	    /* lose the copy present at O0.  */
	    if (is_gimple_assign (stmt))
	      {
	        unlink_stmt_vdef (stmt);
		gsi_remove (&gsi, true);
		stmt = gsi_stmt (gsi);
	       }
	    /* Simplify the switch or if following.  */
	    if (gswitch *gsw = dyn_cast <gswitch *> (stmt))
	      {
		gimple_switch_set_index (gsw, integer_zero_node);
		fold_stmt (&gsi);
	      }
	    else if (gcond *gif = dyn_cast <gcond*> (stmt))
	      {
		if (gimple_cond_code (gif) == EQ_EXPR)
		  gimple_cond_make_true (gif);
		else
		  gimple_cond_make_false (gif);
		fold_stmt (&gsi);
	      }
	    else
	      debug_gimple_stmt (stmt);
	    changed = true;
	    if (gsi_end_p (gsi))
	      break;
	    continue;
	    }
	  default:
	    gsi_next (&gsi);
	    break;
	  }
      }

  if (!changed)
    {
      if (dump_file)
	fprintf (dump_file, "coro: nothing to do\n");
      return todoflags;
    }

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
      {
	gimple *stmt = gsi_stmt (gsi);
	if (!is_gimple_call (stmt) || !gimple_call_internal_p (stmt))
	  {
	  gsi_next (&gsi);
	  continue;
	  }
	if (gimple_call_internal_fn (stmt) != IFN_CO_ACTOR)
	  gsi_next (&gsi);
	else
	  {
	    if (dump_file)
	      fprintf (dump_file, "saw CO_ACTOR in BB %u\n", bb->index);
	    /* get yield point.  */
	    HOST_WIDE_INT idx = TREE_INT_CST_LOW (gimple_call_arg (stmt, 0));
	    tree act_vdef = gimple_vdef (stmt);
	    tree *seen = destinations.get (idx);
	    if (!seen)
	      {
		/* If we never saw this index, it means that the CO_YIELD
		   associated was elided during earlier optimisations, so we
		    don't need to fix up the switch targets.  */
		if (dump_file)
		  fprintf (dump_file, "yield point " HOST_WIDE_INT_PRINT_DEC
			   " not used, removing it .. \n", idx);
		unlink_stmt_vdef (stmt);
		gsi_remove (&gsi, true);
		release_defs (stmt);
	      }
	    else
	      {
		/* So we need to switch the target of this switch case to
		   the relevant BB.  */
		basic_block new_bb = label_to_block (cfun, *seen);
		/* We expect the block we're modifying to contain a single
		   CO_ACTOR() followed by a goto <switch default bb>.  */
		gcc_checking_assert (EDGE_COUNT (bb->succs) == 1);
		edge e;
		edge_iterator ei;
		FOR_EACH_EDGE (e, ei, bb->succs)
		  {
		    basic_block old_bb = e->dest;
		    move_edge_and_update_vdef (e, act_vdef, old_bb, new_bb);
		  }
		/* Now we've connected the ACTOR VDEF to the new bb.  So unlink
		   it and delete the ACTOR placeholder.  */
		unlink_stmt_vdef (stmt);
		gsi_remove (&gsi, true);
		changed = true;
	      }
	    /* The remove advances the iterator.  */
	    if (gsi_end_p (gsi))
	      break;
	    continue;
	  }
      }

  if (changed)
    {
      /* We've changed the CFG, so have to update VSSA; the insert we do
	 above appears to be necessary but not sufficient.  */
      mark_virtual_operands_for_renaming (cfun);

      /* Sledgehammer fix up to DOM, however attempts to do it more cheaply
         were not terribly successful.  */
      free_dominance_info (CDI_DOMINATORS);
      calculate_dominance_info (CDI_DOMINATORS);

      /* Changed the CFG and the SSA.  */
      todoflags |= TODO_update_ssa | TODO_cleanup_cfg;
    }
  return todoflags;
}

namespace {

const pass_data pass_data_coroutine_expand_ifns  =
{
  GIMPLE_PASS, /* type */
  "coro-expand-ifns", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  (PROP_cfg | PROP_ssa), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0  /* todo_flags_finish, set this in the fn. */
};

class pass_coroutine_expand_ifns : public gimple_opt_pass
{
public:
  pass_coroutine_expand_ifns  (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_coroutine_expand_ifns , ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_coroutines; };

  virtual unsigned int execute (function *f ATTRIBUTE_UNUSED)
    {
      return execute_expand_coro_ifns ();
    }

}; // class pass_coroutine_expand_ifns

} // anon namespace

gimple_opt_pass *
make_pass_coroutine_expand_ifns (gcc::context *ctxt)
{
  return new pass_coroutine_expand_ifns (ctxt);
}
