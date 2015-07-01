/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990-2015 Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.
   Converted to use trees by Dale Johannesen, Apple Computer.

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

/* Generate basic block profile instrumentation and auxiliary files.
   Tree-based version.  See profile.c for overview.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "function.h"
#include "predict.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "diagnostic-core.h"
#include "coverage.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "fold-const.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "varasm.h"
#include "tree-nested.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "tree-cfg.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-into-ssa.h"
#include "tree-pass.h"
#include "value-prof.h"
#include "profile.h"
#include "target.h"
#include "tree-cfgcleanup.h"
#include "tree-nested.h"
#include "params.h"

static GTY(()) tree gcov_type_node;
static GTY(()) tree tree_interval_profiler_fn;
static GTY(()) tree tree_pow2_profiler_fn;
static GTY(()) tree tree_one_value_profiler_fn;
static GTY(()) tree tree_indirect_call_profiler_fn;
static GTY(()) tree tree_time_profiler_fn;
static GTY(()) tree tree_average_profiler_fn;
static GTY(()) tree tree_ior_profiler_fn;


static GTY(()) tree ic_void_ptr_var;
static GTY(()) tree ic_gcov_type_ptr_var;
static GTY(()) tree ptr_void;

/* Do initialization work for the edge profiler.  */

/* Add code:
   __thread gcov*	__gcov_indirect_call_counters; // pointer to actual counter
   __thread void*	__gcov_indirect_call_callee; // actual callee address
   __thread int __gcov_function_counter; // time profiler function counter
*/
static void
init_ic_make_global_vars (void)
{
  tree  gcov_type_ptr;

  ptr_void = build_pointer_type (void_type_node);

  ic_void_ptr_var
    = build_decl (UNKNOWN_LOCATION, VAR_DECL,
		  get_identifier (
			  (PARAM_VALUE (PARAM_INDIR_CALL_TOPN_PROFILE) ?
			   "__gcov_indirect_call_topn_callee" :
			   "__gcov_indirect_call_callee")),
		  ptr_void);
  TREE_PUBLIC (ic_void_ptr_var) = 1;
  DECL_EXTERNAL (ic_void_ptr_var) = 1;
  TREE_STATIC (ic_void_ptr_var) = 1;
  DECL_ARTIFICIAL (ic_void_ptr_var) = 1;
  DECL_INITIAL (ic_void_ptr_var) = NULL;
  if (targetm.have_tls)
    set_decl_tls_model (ic_void_ptr_var, decl_default_tls_model (ic_void_ptr_var));

  varpool_node::finalize_decl (ic_void_ptr_var);

  gcov_type_ptr = build_pointer_type (get_gcov_type ());

  ic_gcov_type_ptr_var
    = build_decl (UNKNOWN_LOCATION, VAR_DECL,
		  get_identifier (
			  (PARAM_VALUE (PARAM_INDIR_CALL_TOPN_PROFILE) ?
			   "__gcov_indirect_call_topn_counters" :
			   "__gcov_indirect_call_counters")),
		  gcov_type_ptr);
  TREE_PUBLIC (ic_gcov_type_ptr_var) = 1;
  DECL_EXTERNAL (ic_gcov_type_ptr_var) = 1;
  TREE_STATIC (ic_gcov_type_ptr_var) = 1;
  DECL_ARTIFICIAL (ic_gcov_type_ptr_var) = 1;
  DECL_INITIAL (ic_gcov_type_ptr_var) = NULL;
  if (targetm.have_tls)
    set_decl_tls_model (ic_gcov_type_ptr_var, decl_default_tls_model (ic_gcov_type_ptr_var));

  varpool_node::finalize_decl (ic_gcov_type_ptr_var);
}

/* Create the type and function decls for the interface with gcov.  */

void
gimple_init_edge_profiler (void)
{
  tree interval_profiler_fn_type;
  tree pow2_profiler_fn_type;
  tree one_value_profiler_fn_type;
  tree gcov_type_ptr;
  tree ic_profiler_fn_type;
  tree average_profiler_fn_type;
  tree time_profiler_fn_type;

  if (!gcov_type_node)
    {
      gcov_type_node = get_gcov_type ();
      gcov_type_ptr = build_pointer_type (gcov_type_node);

      /* void (*) (gcov_type *, gcov_type, int, unsigned)  */
      interval_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  integer_type_node,
					  unsigned_type_node, NULL_TREE);
      tree_interval_profiler_fn
	      = build_fn_decl ("__gcov_interval_profiler",
				     interval_profiler_fn_type);
      TREE_NOTHROW (tree_interval_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_interval_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_interval_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      pow2_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      tree_pow2_profiler_fn = build_fn_decl ("__gcov_pow2_profiler",
						   pow2_profiler_fn_type);
      TREE_NOTHROW (tree_pow2_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_pow2_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_pow2_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      one_value_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      tree_one_value_profiler_fn
	      = build_fn_decl ("__gcov_one_value_profiler",
				     one_value_profiler_fn_type);
      TREE_NOTHROW (tree_one_value_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_one_value_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_one_value_profiler_fn));

      init_ic_make_global_vars ();

      /* void (*) (gcov_type, void *)  */
      ic_profiler_fn_type
	       = build_function_type_list (void_type_node,
					  gcov_type_node,
					  ptr_void,
					  NULL_TREE);
      tree_indirect_call_profiler_fn
	      = build_fn_decl ( (PARAM_VALUE (PARAM_INDIR_CALL_TOPN_PROFILE) ?
				 "__gcov_indirect_call_topn_profiler":
				 "__gcov_indirect_call_profiler_v2"),
			       ic_profiler_fn_type);

      TREE_NOTHROW (tree_indirect_call_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_indirect_call_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_indirect_call_profiler_fn));

      /* void (*) (gcov_type *, gcov_type, void *)  */
      time_profiler_fn_type
	       = build_function_type_list (void_type_node,
					  gcov_type_ptr, NULL_TREE);
      tree_time_profiler_fn
	      = build_fn_decl ("__gcov_time_profiler",
				     time_profiler_fn_type);
      TREE_NOTHROW (tree_time_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_time_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_time_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      average_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node, NULL_TREE);
      tree_average_profiler_fn
	      = build_fn_decl ("__gcov_average_profiler",
				     average_profiler_fn_type);
      TREE_NOTHROW (tree_average_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_average_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_average_profiler_fn));
      tree_ior_profiler_fn
	      = build_fn_decl ("__gcov_ior_profiler",
				     average_profiler_fn_type);
      TREE_NOTHROW (tree_ior_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_ior_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_ior_profiler_fn));

      /* LTO streamer needs assembler names.  Because we create these decls
         late, we need to initialize them by hand.  */
      DECL_ASSEMBLER_NAME (tree_interval_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_pow2_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_one_value_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_indirect_call_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_time_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_average_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_ior_profiler_fn);
    }
}

/* Output instructions as GIMPLE trees to increment the edge
   execution count, and insert them on E.  We rely on
   gsi_insert_on_edge to preserve the order.  */

void
gimple_gen_edge_profiler (int edgeno, edge e)
{
  tree ref, one, gcov_type_tmp_var;
  gassign *stmt1, *stmt2, *stmt3;

  ref = tree_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
  one = build_int_cst (gcov_type_node, 1);
  gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
					  NULL, "PROF_edge_counter");
  stmt1 = gimple_build_assign (gcov_type_tmp_var, ref);
  gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
					  NULL, "PROF_edge_counter");
  stmt2 = gimple_build_assign (gcov_type_tmp_var, PLUS_EXPR,
			       gimple_assign_lhs (stmt1), one);
  stmt3 = gimple_build_assign (unshare_expr (ref), gimple_assign_lhs (stmt2));
  gsi_insert_on_edge (e, stmt1);
  gsi_insert_on_edge (e, stmt2);
  gsi_insert_on_edge (e, stmt3);
}

/* Emits code to get VALUE to instrument at GSI, and returns the
   variable containing the value.  */

static tree
prepare_instrumented_value (gimple_stmt_iterator *gsi, histogram_value value)
{
  tree val = value->hvalue.value;
  if (POINTER_TYPE_P (TREE_TYPE (val)))
    val = fold_convert (build_nonstandard_integer_type
			  (TYPE_PRECISION (TREE_TYPE (val)), 1), val);
  return force_gimple_operand_gsi (gsi, fold_convert (gcov_type_node, val),
				   true, NULL_TREE, true, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE trees to increment the interval histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_interval_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref = tree_coverage_counter_ref (tag, base), ref_ptr;
  gcall *call;
  tree val;
  tree start = build_int_cst_type (integer_type_node,
				   value->hdata.intvl.int_start);
  tree steps = build_int_cst_type (unsigned_type_node,
				   value->hdata.intvl.steps);

  ref_ptr = force_gimple_operand_gsi (&gsi,
				      build_addr (ref, current_function_decl),
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_interval_profiler_fn, 4,
			    ref_ptr, val, start, steps);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees to increment the power of two histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the tag
   of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_pow2_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_pow2_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees for code to find the most common value.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

void
gimple_gen_one_value_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_one_value_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call.
   VALUE is the call expression whose indirect callee is profiled.
   TAG is the tag of the section for counters, BASE is offset of the
   counter position.  */

void
gimple_gen_ic_profiler (histogram_value value, unsigned tag, unsigned base)
{
  tree tmp1;
  gassign *stmt1, *stmt2, *stmt3;
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);

  if ( (PARAM_VALUE (PARAM_INDIR_CALL_TOPN_PROFILE) &&
        tag == GCOV_COUNTER_V_INDIR) ||
       (!PARAM_VALUE (PARAM_INDIR_CALL_TOPN_PROFILE) &&
        tag == GCOV_COUNTER_ICALL_TOPNV))
    return;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);

  /* Insert code:

    stmt1: __gcov_indirect_call_counters = get_relevant_counter_ptr ();
    stmt2: tmp1 = (void *) (indirect call argument value)
    stmt3: __gcov_indirect_call_callee = tmp1;
   */

  stmt1 = gimple_build_assign (ic_gcov_type_ptr_var, ref_ptr);
  tmp1 = make_temp_ssa_name (ptr_void, NULL, "PROF");
  stmt2 = gimple_build_assign (tmp1, unshare_expr (value->hvalue.value));
  stmt3 = gimple_build_assign (ic_void_ptr_var, gimple_assign_lhs (stmt2));

  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call. Insert instructions at the
   beginning of every possible called function.
  */

void
gimple_gen_ic_func_profiler (void)
{
  struct cgraph_node * c_node = cgraph_node::get (current_function_decl);
  gimple_stmt_iterator gsi;
  gcall *stmt1;
  gassign *stmt2;
  tree tree_uid, cur_func, void0;

  if (c_node->only_called_directly_p ())
    return;

  gimple_init_edge_profiler ();

  /* Insert code:

    stmt1: __gcov_indirect_call_profiler_v2 (profile_id,
					     &current_function_decl)
   */
  gsi = gsi_after_labels (split_edge (single_succ_edge
					 (ENTRY_BLOCK_PTR_FOR_FN (cfun))));

  cur_func = force_gimple_operand_gsi (&gsi,
				       build_addr (current_function_decl,
						   current_function_decl),
				       true, NULL_TREE,
				       true, GSI_SAME_STMT);
  tree_uid = build_int_cst
	      (gcov_type_node,
	       cgraph_node::get (current_function_decl)->profile_id);
  stmt1 = gimple_build_call (tree_indirect_call_profiler_fn, 2,
			     tree_uid, cur_func);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);

  /* Set __gcov_indirect_call_callee to 0,
     so that calls from other modules won't get misattributed
     to the last caller of the current callee. */
  void0 = build_int_cst (build_pointer_type (void_type_node), 0);
  stmt2 = gimple_build_assign (ic_void_ptr_var, void0);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE tree at the beginning for each function.
   TAG is the tag of the section for counters, BASE is offset of the
   counter position and GSI is the iterator we place the counter.  */

void
gimple_gen_time_profiler (unsigned tag, unsigned base,
                          gimple_stmt_iterator &gsi)
{
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gcall *call;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  call = gimple_build_call (tree_time_profiler_fn, 1, ref_ptr);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees for code to find the most common value
   of a difference between two evaluations of an expression.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

void
gimple_gen_const_delta_profiler (histogram_value value ATTRIBUTE_UNUSED,
			       unsigned tag ATTRIBUTE_UNUSED,
			       unsigned base ATTRIBUTE_UNUSED)
{
  /* FIXME implement this.  */
#ifdef ENABLE_CHECKING
  internal_error ("unimplemented functionality");
#endif
  gcc_unreachable ();
}

/* Output instructions as GIMPLE trees to increment the average histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_average_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE,
				      true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_average_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees to increment the ior histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_ior_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_ior_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Profile all functions in the callgraph.  */

static unsigned int
tree_profiling (void)
{
  struct cgraph_node *node;

  /* This is a small-ipa pass that gets called only once, from
     cgraphunit.c:ipa_passes().  */
  gcc_assert (symtab->state == IPA_SSA);

  init_node_map (true);

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      if (!gimple_has_body_p (node->decl))
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      /* Do not instrument extern inline functions when testing coverage.
	 While this is not perfectly consistent (early inlined extern inlines
	 will get acocunted), testsuite expects that.  */
      if (DECL_EXTERNAL (node->decl)
	  && flag_test_coverage)
	continue;

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));

      /* Local pure-const may imply need to fixup the cfg.  */
      if (execute_fixup_cfg () & TODO_cleanup_cfg)
	cleanup_tree_cfg ();

      branch_prob ();

      if (! flag_branch_probabilities
	  && flag_profile_values)
	gimple_gen_ic_func_profiler ();

      if (flag_branch_probabilities
	  && flag_profile_values
	  && flag_value_profile_transformations)
	gimple_value_profile_transformations ();

      /* The above could hose dominator info.  Currently there is
	 none coming in, this is a safety valve.  It should be
	 easy to adjust it, if and when there is some.  */
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      pop_cfun ();
    }

  /* Drop pure/const flags from instrumented functions.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      if (!gimple_has_body_p (node->decl)
	  || !(!node->clone_of
	  || node->decl != node->clone_of->decl))
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      node->set_const_flag (false, false);
      node->set_pure_flag (false, false);
    }

  /* Update call statements and rebuild the cgraph.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      basic_block bb;

      if (!gimple_has_body_p (node->decl)
	  || !(!node->clone_of
	  || node->decl != node->clone_of->decl))
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));

      FOR_EACH_BB_FN (bb, cfun)
	{
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);
	      if (is_gimple_call (stmt))
		update_stmt (stmt);
	    }
	}

      /* re-merge split blocks.  */
      cleanup_tree_cfg ();
      update_ssa (TODO_update_ssa);

      cgraph_edge::rebuild_edges ();

      pop_cfun ();
    }

  handle_missing_profiles ();

  del_node_map ();
  return 0;
}

namespace {

const pass_data pass_data_ipa_tree_profile =
{
  SIMPLE_IPA_PASS, /* type */
  "profile", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PROFILE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_tree_profile : public simple_ipa_opt_pass
{
public:
  pass_ipa_tree_profile (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_tree_profile, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *) { return tree_profiling (); }

}; // class pass_ipa_tree_profile

bool
pass_ipa_tree_profile::gate (function *)
{
  /* When profile instrumentation, use or test coverage shall be performed.
     But for AutoFDO, this there is no instrumentation, thus this pass is
     diabled.  */
  return (!in_lto_p && !flag_auto_profile
	  && (flag_branch_probabilities || flag_test_coverage
	      || profile_arc_flag));
}

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_tree_profile (gcc::context *ctxt)
{
  return new pass_ipa_tree_profile (ctxt);
}

#include "gt-tree-profile.h"
