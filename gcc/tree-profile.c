/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.
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
#include "rtl.h"
#include "flags.h"
#include "output.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "toplev.h"
#include "coverage.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "value-prof.h"
#include "ggc.h"
#include "cgraph.h"

static GTY(()) tree gcov_type_node;
static GTY(()) tree gcov_type_tmp_var;
static GTY(()) tree tree_interval_profiler_fn;
static GTY(()) tree tree_pow2_profiler_fn;
static GTY(()) tree tree_one_value_profiler_fn;
static GTY(()) tree tree_indirect_call_profiler_fn;
static GTY(()) tree tree_average_profiler_fn;
static GTY(()) tree tree_ior_profiler_fn;


static GTY(()) tree ic_void_ptr_var;
static GTY(()) tree ic_gcov_type_ptr_var;
static GTY(()) tree ptr_void;

/* Do initialization work for the edge profiler.  */

/* Add code:
   static gcov*	__gcov_indirect_call_counters; // pointer to actual counter
   static void*	__gcov_indirect_call_callee; // actual callee address
*/
static void
tree_init_ic_make_global_vars (void)
{
  tree  gcov_type_ptr;

  ptr_void = build_pointer_type (void_type_node);
  
  ic_void_ptr_var 
    = build_decl (VAR_DECL, 
		  get_identifier ("__gcov_indirect_call_callee"), 
		  ptr_void);
  TREE_STATIC (ic_void_ptr_var) = 1;
  TREE_PUBLIC (ic_void_ptr_var) = 0;
  DECL_ARTIFICIAL (ic_void_ptr_var) = 1;
  DECL_INITIAL (ic_void_ptr_var) = NULL;
  assemble_variable (ic_void_ptr_var, 0, 0, 0);

  gcov_type_ptr = build_pointer_type (get_gcov_type ());
  ic_gcov_type_ptr_var 
    = build_decl (VAR_DECL, 
		  get_identifier ("__gcov_indirect_call_counters"), 
		  gcov_type_ptr);
  TREE_STATIC (ic_gcov_type_ptr_var) = 1;
  TREE_PUBLIC (ic_gcov_type_ptr_var) = 0;
  DECL_ARTIFICIAL (ic_gcov_type_ptr_var) = 1;
  DECL_INITIAL (ic_gcov_type_ptr_var) = NULL;
  assemble_variable (ic_gcov_type_ptr_var, 0, 0, 0);
}

static void
tree_init_edge_profiler (void)
{
  tree interval_profiler_fn_type;
  tree pow2_profiler_fn_type;
  tree one_value_profiler_fn_type;
  tree gcov_type_ptr;
  tree ic_profiler_fn_type;
  tree average_profiler_fn_type;

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

      /* void (*) (gcov_type *, gcov_type)  */
      pow2_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      tree_pow2_profiler_fn = build_fn_decl ("__gcov_pow2_profiler",
						   pow2_profiler_fn_type);

      /* void (*) (gcov_type *, gcov_type)  */
      one_value_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      tree_one_value_profiler_fn
	      = build_fn_decl ("__gcov_one_value_profiler",
				     one_value_profiler_fn_type);

      tree_init_ic_make_global_vars ();
      
      /* void (*) (gcov_type *, gcov_type, void *, void *)  */
      ic_profiler_fn_type
	       = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  ptr_void,
					  ptr_void, NULL_TREE);
      tree_indirect_call_profiler_fn
	      = build_fn_decl ("__gcov_indirect_call_profiler",
				     ic_profiler_fn_type);
      /* void (*) (gcov_type *, gcov_type)  */
      average_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node, NULL_TREE);
      tree_average_profiler_fn
	      = build_fn_decl ("__gcov_average_profiler",
				     average_profiler_fn_type);
      tree_ior_profiler_fn
	      = build_fn_decl ("__gcov_ior_profiler",
				     average_profiler_fn_type);
    }
}

/* New call was added, make goto call edges if neccesary.  */

static void
add_abnormal_goto_call_edges (gimple_stmt_iterator gsi)
{
  gimple stmt = gsi_stmt (gsi);

  if (!stmt_can_make_abnormal_goto (stmt))
    return;
  if (!gsi_end_p (gsi))
    split_block (gimple_bb (stmt), stmt);
  make_abnormal_goto_edges (gimple_bb (stmt), true);
}

/* Output instructions as GIMPLE trees to increment the edge 
   execution count, and insert them on E.  We rely on 
   gsi_insert_on_edge to preserve the order.  */

static void
tree_gen_edge_profiler (int edgeno, edge e)
{
  tree ref, one;
  gimple stmt1, stmt2, stmt3;

  /* We share one temporary variable declaration per function.  This
     gets re-set in tree_profiling.  */
  if (gcov_type_tmp_var == NULL_TREE)
    gcov_type_tmp_var = create_tmp_var (gcov_type_node, "PROF_edge_counter");
  ref = tree_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
  one = build_int_cst (gcov_type_node, 1);
  stmt1 = gimple_build_assign (gcov_type_tmp_var, ref);
  stmt2 = gimple_build_assign_with_ops (PLUS_EXPR, gcov_type_tmp_var,
					gcov_type_tmp_var, one);
  stmt3 = gimple_build_assign (unshare_expr (ref), gcov_type_tmp_var);
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
  return force_gimple_operand_gsi (gsi, fold_convert (gcov_type_node, val),
				   true, NULL_TREE, true, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE trees to increment the interval histogram 
   counter.  VALUE is the expression whose value is profiled.  TAG is the 
   tag of the section for counters, BASE is offset of the counter position.  */

static void
tree_gen_interval_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref = tree_coverage_counter_ref (tag, base), ref_ptr;
  gimple call;
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
  add_abnormal_goto_call_edges (gsi);
}

/* Output instructions as GIMPLE trees to increment the power of two histogram 
   counter.  VALUE is the expression whose value is profiled.  TAG is the tag 
   of the section for counters, BASE is offset of the counter position.  */

static void
tree_gen_pow2_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gimple call;
  tree val;
  
  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_pow2_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
  add_abnormal_goto_call_edges (gsi);
}

/* Output instructions as GIMPLE trees for code to find the most common value.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
tree_gen_one_value_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gimple call;
  tree val;
  
  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_one_value_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
  add_abnormal_goto_call_edges (gsi);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call.  
   VALUE is the call expression whose indirect callee is profiled.
   TAG is the tag of the section for counters, BASE is offset of the
   counter position.  */

static void
tree_gen_ic_profiler (histogram_value value, unsigned tag, unsigned base)
{
  tree tmp1;
  gimple stmt1, stmt2, stmt3;
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);

  /* Insert code:
    
    __gcov_indirect_call_counters = get_relevant_counter_ptr (); 
    __gcov_indirect_call_callee = (void *) indirect call argument;
   */

  tmp1 = create_tmp_var (ptr_void, "PROF");
  stmt1 = gimple_build_assign (ic_gcov_type_ptr_var, ref_ptr);
  stmt2 = gimple_build_assign (tmp1, unshare_expr (value->hvalue.value));
  stmt3 = gimple_build_assign (ic_void_ptr_var, tmp1);

  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call. Insert instructions at the
   beginning of every possible called function.
  */

static void
tree_gen_ic_func_profiler (void)
{
  struct cgraph_node * c_node = cgraph_node (current_function_decl);
  gimple_stmt_iterator gsi;
  edge e;
  basic_block bb;
  edge_iterator ei;
  gimple stmt1, stmt2;
  tree tree_uid, cur_func;

  if (!c_node->needed)
    return;
  
  tree_init_edge_profiler ();
  
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    {
      tree void0;

      bb = split_edge (e);
      gsi = gsi_start_bb (bb);

      cur_func = force_gimple_operand_gsi (&gsi,
					   build_addr (current_function_decl, 
						       current_function_decl),
					   true, NULL_TREE,
					   true, GSI_SAME_STMT);
      tree_uid = build_int_cst (gcov_type_node, c_node->pid);
      stmt1 = gimple_build_call (tree_indirect_call_profiler_fn, 4,
				 ic_gcov_type_ptr_var,
				 tree_uid,
				 cur_func,
				 ic_void_ptr_var);
      gsi_insert_after (&gsi, stmt1, GSI_NEW_STMT);
      gcc_assert (EDGE_COUNT (bb->succs) == 1);
      bb = split_edge (EDGE_I (bb->succs, 0));
      add_abnormal_goto_call_edges (gsi);

      gsi = gsi_start_bb (bb);
      /* Set __gcov_indirect_call_callee to 0,
         so that calls from other modules won't get misattributed
	 to the last caller of the current callee. */
      void0 = build_int_cst (build_pointer_type (void_type_node), 0);
      stmt2 = gimple_build_assign (ic_void_ptr_var, void0);
      gsi_insert_after (&gsi, stmt2, GSI_NEW_STMT);
    }
}

/* Output instructions as GIMPLE trees for code to find the most common value 
   of a difference between two evaluations of an expression.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
tree_gen_const_delta_profiler (histogram_value value ATTRIBUTE_UNUSED,
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

static void
tree_gen_average_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gimple call;
  tree val;
  
  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE,
				      true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_average_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
  add_abnormal_goto_call_edges (gsi);
}

/* Output instructions as GIMPLE trees to increment the ior histogram 
   counter.  VALUE is the expression whose value is profiled.  TAG is the 
   tag of the section for counters, BASE is offset of the counter position.  */

static void
tree_gen_ior_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, base);
  gimple call;
  tree val;
  
  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_ior_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
  add_abnormal_goto_call_edges (gsi);
}

/* Return 1 if tree-based profiling is in effect, else 0.
   If it is, set up hooks for tree-based profiling.
   Gate for pass_tree_profile.  */

static bool
do_tree_profiling (void)
{
  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      tree_register_profile_hooks ();
      gimple_register_value_prof_hooks ();
      return true;
    }
  return false;
}

static unsigned int
tree_profiling (void)
{
  /* Don't profile functions produced at destruction time, particularly
     the gcov datastructure initializer.  Don't profile if it has been
     already instrumented either (when OpenMP expansion creates
     child function from already instrumented body).  */
  if (cgraph_state == CGRAPH_STATE_FINISHED
      || cfun->after_tree_profile)
    return 0;

  /* Re-set global shared temporary variable for edge-counters.  */
  gcov_type_tmp_var = NULL_TREE;

  branch_prob ();

  if (! flag_branch_probabilities 
      && flag_profile_values)
    tree_gen_ic_func_profiler ();

  if (flag_branch_probabilities
      && flag_profile_values
      && flag_value_profile_transformations)
    value_profile_transformations ();
  /* The above could hose dominator info.  Currently there is
     none coming in, this is a safety valve.  It should be
     easy to adjust it, if and when there is some.  */
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  cfun->after_tree_profile = 1;
  return 0;
}

struct gimple_opt_pass pass_tree_profile = 
{
 {
  GIMPLE_PASS,
  "tree_profile",			/* name */
  do_tree_profiling,			/* gate */
  tree_profiling,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_BRANCH_PROB,			/* tv_id */
  PROP_gimple_leh | PROP_cfg,		/* properties_required */
  PROP_gimple_leh | PROP_cfg,		/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_verify_stmts | TODO_dump_func	/* todo_flags_finish */
 }
};

struct profile_hooks tree_profile_hooks =
{
  tree_init_edge_profiler,       /* init_edge_profiler */
  tree_gen_edge_profiler,	 /* gen_edge_profiler */
  tree_gen_interval_profiler,    /* gen_interval_profiler */
  tree_gen_pow2_profiler,        /* gen_pow2_profiler */
  tree_gen_one_value_profiler,   /* gen_one_value_profiler */
  tree_gen_const_delta_profiler, /* gen_const_delta_profiler */
  tree_gen_ic_profiler,		 /* gen_ic_profiler */
  tree_gen_average_profiler,     /* gen_average_profiler */
  tree_gen_ior_profiler          /* gen_ior_profiler */
};

#include "gt-tree-profile.h"
