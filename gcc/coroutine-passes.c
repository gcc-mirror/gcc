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

// FIXME: minimise headers ..
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
#include "tree-nested.h"
#include "stor-layout.h"
#include "common/common-target.h"
#include "lto-section-names.h"
#include "gimple-pretty-print.h"
#include "intl.h"
#include "stringpool.h"
#include "attribs.h"
#include "cfgloop.h"

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
	  /* The coro frame starts with two pointers (to the resume and
	     destroy() functions.  It is then followed by the promise
	     which is aligned as per type [or user attribute].
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
	  psize *= 2; /* Start with two pointers.  */
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

static unsigned int
execute_expand_coro_ifns (void)
{
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
	  case IFN_CO_FRAME:
	  case IFN_CO_YIELD:
	  case IFN_CO_ACTOR:
	    fprintf (stderr, "saw coro IFN %s in %s\n",
		     internal_fn_name (gimple_call_internal_fn (stmt)),
		     IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
	    //stmt = gimple_build_nop ();
	    gsi_remove(&gsi, true);
	    if (gsi_end_p (gsi))
	      break;
	    continue;
	  default:
	    gsi_next (&gsi);
	    break;
	  }
      }
  return 0;
}

namespace {

const pass_data pass_data_coroutine_expand_ifns  =
{
  GIMPLE_PASS, /* type */
  "coro-expand-ifns", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* PROP_cfg, *//* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0 /* maybe TODO_update_ssa | TODO_cleanup_cfg, *//* todo_flags_finish */
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
