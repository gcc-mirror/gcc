/* coroutine expansion and optimisation passes.

   Copyright (C) 2018-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "calls.h"
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
#include "tree-ssa-propagate.h"
#include "gimple-pretty-print.h"
#include "cfghooks.h"

/* Here we:
   * lower the internal function that implements an exit from scope.
   * expand the builtins that are used to implement the library
     interfaces to the coroutine frame.  */

static tree
lower_coro_builtin (gimple_stmt_iterator *gsi, bool *handled_ops_p,
		    struct walk_stmt_info *wi ATTRIBUTE_UNUSED)
{
  gimple *stmt = gsi_stmt (*gsi);
  *handled_ops_p = !gimple_has_substatements (stmt);

  if (gimple_code (stmt) != GIMPLE_CALL)
    return NULL_TREE;

  /* This internal function implements an exit from scope without
     performing any cleanups; it jumps directly to the label provided.  */
  if (gimple_call_internal_p (stmt)
      && gimple_call_internal_fn (stmt) == IFN_CO_SUSPN)
    {
      tree dest = TREE_OPERAND (gimple_call_arg (stmt, 0), 0);
      ggoto *g = gimple_build_goto (dest);
      gsi_replace (gsi, g, /* do EH */ false);
      *handled_ops_p = true;
      return NULL_TREE;
    }

  tree decl = gimple_call_fndecl (stmt);
  if (!decl || !fndecl_built_in_p (decl, BUILT_IN_NORMAL))
    return NULL_TREE;

  /* The remaining builtins implement the library interfaces to the coro
     frame.  */
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
	/* The coro frame starts with two pointers (to the resume and
	   destroy() functions).  These are followed by the promise which
	   is aligned as per type [or user attribute].
	   The input pointer is the first argument.
	   The promise alignment is the second and the third is a bool
	   that is true when we are converting from a promise ptr to a
	   frame pointer, and false for the inverse.  */
	tree ptr = gimple_call_arg (stmt, 0);
	tree align_t = gimple_call_arg (stmt, 1);
	tree from = gimple_call_arg (stmt, 2);
	gcc_checking_assert (TREE_CODE (align_t) == INTEGER_CST);
	gcc_checking_assert (TREE_CODE (from) == INTEGER_CST);
	bool dir = wi::to_wide (from) != 0;
	HOST_WIDE_INT promise_align = TREE_INT_CST_LOW (align_t);
	HOST_WIDE_INT psize =
	  TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ptr_type_node));
	HOST_WIDE_INT align = TYPE_ALIGN_UNIT (ptr_type_node);
	align = MAX (align, promise_align);
	psize *= 2; /* Start with two pointers.  */
	psize = ROUND_UP (psize, align);
	HOST_WIDE_INT offs = dir ? -psize : psize;
	tree repl = build2 (POINTER_PLUS_EXPR, ptr_type_node, ptr,
			    size_int (offs));
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
	HOST_WIDE_INT psize =
	  TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ptr_type_node));
	HOST_WIDE_INT offset = call_idx * psize;
	tree fntype = TREE_TYPE (decl);
	tree fntype_ptr = build_pointer_type (fntype);
	tree fntype_ppp = build_pointer_type (fntype_ptr);
	tree indirect = fold_build2 (MEM_REF, fntype_ptr, ptr,
				     build_int_cst (fntype_ppp, offset));
	tree f_ptr_tmp = make_ssa_name (TYPE_MAIN_VARIANT (fntype_ptr));
	gassign *get_fptr = gimple_build_assign (f_ptr_tmp, indirect);
	gsi_insert_before (gsi, get_fptr, GSI_SAME_STMT);
	gimple_call_set_fn (static_cast<gcall *> (stmt), f_ptr_tmp);
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
	tree vpp = build_pointer_type (ptr_type_node);
	tree indirect
	  = fold_build2 (MEM_REF, vpp, ptr, build_int_cst (vpp, 0));
	tree d_ptr_tmp = make_ssa_name (ptr_type_node);
	gassign *get_dptr = gimple_build_assign (d_ptr_tmp, indirect);
	gsi_insert_before (gsi, get_dptr, GSI_SAME_STMT);
	tree done = fold_build2 (EQ_EXPR, boolean_type_node, d_ptr_tmp,
				 null_pointer_node);
	gassign *get_res = gimple_build_assign (lhs, done);
	gsi_replace (gsi, get_res, true);
	*handled_ops_p = true;
      }
      break;
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

const pass_data pass_data_coroutine_lower_builtins = {
  GIMPLE_PASS,		 /* type */
  "coro-lower-builtins", /* name */
  OPTGROUP_NONE,	 /* optinfo_flags */
  TV_NONE,		 /* tv_id */
  0,			 /* properties_required */
  0,			 /* properties_provided */
  0,			 /* properties_destroyed */
  0,			 /* todo_flags_start */
  0			 /* todo_flags_finish */
};

class pass_coroutine_lower_builtins : public gimple_opt_pass
{
public:
  pass_coroutine_lower_builtins (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_coroutine_lower_builtins, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return flag_coroutines; };

  unsigned int execute (function *f ATTRIBUTE_UNUSED) final override
  {
    return execute_lower_coro_builtins ();
  }

}; // class pass_coroutine_lower_builtins

} // namespace

gimple_opt_pass *
make_pass_coroutine_lower_builtins (gcc::context *ctxt)
{
  return new pass_coroutine_lower_builtins (ctxt);
}

/* Expand the remaining coroutine IFNs.

   In the front end we construct a single actor function that contains
   the coroutine state machine.

   The actor function has three entry conditions:
    1. from the ramp, resume point 0 - to initial-suspend.
    2. when resume () is executed (resume point N).
    3. from the destroy () shim when that is executed.

   The actor function begins with two dispatchers; one for resume and
   one for destroy (where the initial entry from the ramp is a special-
   case of resume point 0).

   Each suspend point and each dispatch entry is marked with an IFN such
   that we can connect the relevant dispatchers to their target labels.

   So, if we have:

   CO_YIELD (NUM, FINAL, RES_LAB, DEST_LAB, FRAME_PTR)

   This is await point NUM, and is the final await if FINAL is non-zero.
   The resume point is RES_LAB, and the destroy point is DEST_LAB.

   We expect to find a CO_ACTOR (NUM) in the resume dispatcher and a
   CO_ACTOR (NUM+1) in the destroy dispatcher.

   Initially, the intent of keeping the resume and destroy paths together
   is that the conditionals controlling them are identical, and thus there
   would be duplication of any optimisation of those paths if the split
   were earlier.

   Subsequent inlining of the actor (and DCE) is then able to extract the
   resume and destroy paths as separate functions if that is found
   profitable by the optimisers.

   Once we have remade the connections to their correct postions, we elide
   the labels that the front end inserted.  */

static void
move_edge_and_update (edge e, basic_block old_bb, basic_block new_bb)
{
  if (dump_file)
    fprintf (dump_file, "redirecting edge from bb %u to bb %u\n", old_bb->index,
	     new_bb->index);

  e = redirect_edge_and_branch (e, new_bb);
  if (!e && dump_file)
    fprintf (dump_file, "failed to redirect edge ..  \n");

  /* Die if we failed.  */
  gcc_checking_assert (e);
}

static unsigned int
execute_early_expand_coro_ifns (void)
{
  /* Don't rebuild stuff unless we have to. */
  unsigned int todoflags = 0;
  bool changed = false;
  /* Some of the possible YIELD points will hopefully have been removed by
     earlier optimisations; record the ones that are still present.  */
  hash_map<int_hash<HOST_WIDE_INT, -1, -2>, tree> destinations;
  /* List of dispatch points to update.  */
  auto_vec<gimple_stmt_iterator, 16> actor_worklist;
  basic_block bb;
  gimple_stmt_iterator gsi;

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
      {
	gimple *stmt = gsi_stmt (gsi);

	/* Tell the user about 'alloca', we don't support it yet.  */
	if (gimple_alloca_call_p (stmt))
	  {
	    sorry_at (gimple_location (stmt),
		      "%<alloca%> is not yet supported in coroutines");
	    gsi_next (&gsi);
	    continue;
	  }

	if (!is_gimple_call (stmt) || !gimple_call_internal_p (stmt))
	  {
	    gsi_next (&gsi);
	    continue;
	  }
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_CO_FRAME:
	    {
	      /* This internal function is a placeholder for the frame
		 size.  In principle, we might lower it later (after some
		 optimisation had reduced the frame size).  At present,
		 without any such optimisation, we just set it here.  */
	      tree lhs = gimple_call_lhs (stmt);
	      tree size = gimple_call_arg (stmt, 0);
	      /* Right now, this is a trivial operation - copy through
		 the size computed during initial layout.  */
	      gassign *grpl = gimple_build_assign (lhs, size);
	      gsi_replace (&gsi, grpl, true);
	      gsi_next (&gsi);
	    }
	    break;
	  case IFN_CO_ACTOR:
	    changed = true;
	    actor_worklist.safe_push (gsi); /* Save for later.  */
	    gsi_next (&gsi);
	    break;
	  case IFN_CO_YIELD:
	    {
	      changed = true;
	      /* .CO_YIELD (NUM, FINAL, RES_LAB, DEST_LAB, FRAME_PTR);
		  NUM = await number.
		  FINAL = 1 if this is the final_suspend() await.
		  RES_LAB = resume point label.
		  DEST_LAB = destroy point label.
		  FRAME_PTR = is a null pointer with the type of the coro
			      frame, so that we can resize, if needed.  */
	      if (dump_file)
		fprintf (dump_file, "saw CO_YIELD in BB %u\n", bb->index);
	      tree num = gimple_call_arg (stmt, 0); /* yield point.  */
	      HOST_WIDE_INT idx = TREE_INT_CST_LOW (num);
	      bool existed;
	      tree res_tgt = TREE_OPERAND (gimple_call_arg (stmt, 2), 0);
	      tree &res_dest = destinations.get_or_insert (idx, &existed);
	      if (existed && dump_file)
		{
		  fprintf (
		    dump_file,
		    "duplicate YIELD RESUME point (" HOST_WIDE_INT_PRINT_DEC
		    ") ?\n",
		    idx);
		  print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
		}
	      else
		res_dest = res_tgt;
	      tree dst_tgt = TREE_OPERAND (gimple_call_arg (stmt, 3), 0);
	      tree &dst_dest = destinations.get_or_insert (idx + 1, &existed);
	      if (existed && dump_file)
		{
		  fprintf (
		    dump_file,
		    "duplicate YIELD DESTROY point (" HOST_WIDE_INT_PRINT_DEC
		    ") ?\n",
		    idx + 1);
		  print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
		}
	      else
		dst_dest = dst_tgt;
	      /* lose the co_yield.  */
	      gsi_remove (&gsi, true);
	      stmt = gsi_stmt (gsi); /* next. */
	      /* lose the copy present at O0.  */
	      if (is_gimple_assign (stmt))
		{
		  gsi_remove (&gsi, true);
		  stmt = gsi_stmt (gsi);
		}
	      /* Simplify the switch or if following.  */
	      if (gswitch *gsw = dyn_cast<gswitch *> (stmt))
		{
		  gimple_switch_set_index (gsw, integer_zero_node);
		  fold_stmt (&gsi);
		}
	      else if (gcond *gif = dyn_cast<gcond *> (stmt))
		{
		  if (gimple_cond_code (gif) == EQ_EXPR)
		    gimple_cond_make_true (gif);
		  else
		    gimple_cond_make_false (gif);
		  fold_stmt (&gsi);
		}
	      else if (dump_file)
		print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
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

  while (!actor_worklist.is_empty ())
    {
      gsi = actor_worklist.pop ();
      gimple *stmt = gsi_stmt (gsi);
      gcc_checking_assert (is_gimple_call (stmt)
			   && gimple_call_internal_p (stmt)
			   && gimple_call_internal_fn (stmt) == IFN_CO_ACTOR);
      bb = gsi_bb (gsi);
      HOST_WIDE_INT idx = TREE_INT_CST_LOW (gimple_call_arg (stmt, 0));
      tree *seen = destinations.get (idx);
      changed = true;

      if (dump_file)
	fprintf (dump_file, "saw CO_ACTOR in BB %u\n", bb->index);

      if (!seen)
	{
	  /* If we never saw this index, it means that the CO_YIELD
	  associated was elided during earlier optimisations, so we
	  don't need to fix up the switch targets.  */
	  if (dump_file)
	    fprintf (dump_file, "yield point " HOST_WIDE_INT_PRINT_DEC
		     " not used, removing it .. \n",  idx);
	  gsi_remove (&gsi, true);
	  release_defs (stmt);
	}
      else
	{
	  /* So we need to switch the target of this switch case to the
	     relevant BB.  */
	  basic_block new_bb = label_to_block (cfun, *seen);
	  /* We expect the block we're modifying to contain a single
	     CO_ACTOR() followed by a goto <switch default bb>.  */
	  gcc_checking_assert (EDGE_COUNT (bb->succs) == 1);
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      basic_block old_bb = e->dest;
	      move_edge_and_update (e, old_bb, new_bb);
	    }
	  gsi_remove (&gsi, true);
	}
    }

  /* Changed the CFG.  */
  todoflags |= TODO_cleanup_cfg;
  return todoflags;
}

namespace {

const pass_data pass_data_coroutine_early_expand_ifns = {
  GIMPLE_PASS,		    /* type */
  "coro-early-expand-ifns", /* name */
  OPTGROUP_NONE,	    /* optinfo_flags */
  TV_NONE,		    /* tv_id */
  (PROP_cfg),		    /* properties_required */
  0,			    /* properties_provided */
  0,			    /* properties_destroyed */
  0,			    /* todo_flags_start */
  0			    /* todo_flags_finish, set this in the fn. */
};

class pass_coroutine_early_expand_ifns : public gimple_opt_pass
{
public:
  pass_coroutine_early_expand_ifns (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_coroutine_early_expand_ifns, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *f) final override
    {
      return flag_coroutines && f->coroutine_component;
    }

  unsigned int execute (function *f ATTRIBUTE_UNUSED) final override
  {
    return execute_early_expand_coro_ifns ();
  }

}; // class pass_coroutine_expand_ifns

} // namespace

gimple_opt_pass *
make_pass_coroutine_early_expand_ifns (gcc::context *ctxt)
{
  return new pass_coroutine_early_expand_ifns (ctxt);
}
