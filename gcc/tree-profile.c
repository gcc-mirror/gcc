/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990-2019 Free Software Foundation, Inc.
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
#include "memmodel.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "coverage.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "varasm.h"
#include "tree-nested.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "value-prof.h"
#include "profile.h"
#include "tree-cfgcleanup.h"
#include "params.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-pretty-print.h"
#include "langhooks.h"
#include "stor-layout.h"
#include "xregex.h"

static GTY(()) tree gcov_type_node;
static GTY(()) tree tree_interval_profiler_fn;
static GTY(()) tree tree_pow2_profiler_fn;
static GTY(()) tree tree_one_value_profiler_fn;
static GTY(()) tree tree_indirect_call_profiler_fn;
static GTY(()) tree tree_average_profiler_fn;
static GTY(()) tree tree_ior_profiler_fn;
static GTY(()) tree tree_time_profiler_counter;


static GTY(()) tree ic_tuple_var;
static GTY(()) tree ic_tuple_counters_field;
static GTY(()) tree ic_tuple_callee_field;

/* Do initialization work for the edge profiler.  */

/* Add code:
   __thread gcov*	__gcov_indirect_call_counters; // pointer to actual counter
   __thread void*	__gcov_indirect_call_callee; // actual callee address
   __thread int __gcov_function_counter; // time profiler function counter
*/
static void
init_ic_make_global_vars (void)
{
  tree gcov_type_ptr;

  gcov_type_ptr = build_pointer_type (get_gcov_type ());

  tree tuple_type = lang_hooks.types.make_type (RECORD_TYPE);

  /* callee */
  ic_tuple_callee_field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
				      ptr_type_node);

  /* counters */
  ic_tuple_counters_field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
					NULL_TREE, gcov_type_ptr);
  DECL_CHAIN (ic_tuple_counters_field) = ic_tuple_callee_field;

  finish_builtin_struct (tuple_type, "indirect_call_tuple",
			 ic_tuple_counters_field, NULL_TREE);

  ic_tuple_var
    = build_decl (UNKNOWN_LOCATION, VAR_DECL,
		  get_identifier (
			  (PARAM_VALUE (PARAM_INDIR_CALL_TOPN_PROFILE) ?
			   "__gcov_indirect_call_topn" :
			   "__gcov_indirect_call")),
		  tuple_type);
  TREE_PUBLIC (ic_tuple_var) = 1;
  DECL_ARTIFICIAL (ic_tuple_var) = 1;
  DECL_INITIAL (ic_tuple_var) = NULL;
  DECL_EXTERNAL (ic_tuple_var) = 1;
  if (targetm.have_tls)
    set_decl_tls_model (ic_tuple_var, decl_default_tls_model (ic_tuple_var));
}

/* Create the type and function decls for the interface with gcov.  */

void
gimple_init_gcov_profiler (void)
{
  tree interval_profiler_fn_type;
  tree pow2_profiler_fn_type;
  tree one_value_profiler_fn_type;
  tree gcov_type_ptr;
  tree ic_profiler_fn_type;
  tree average_profiler_fn_type;
  const char *profiler_fn_name;
  const char *fn_name;

  if (!gcov_type_node)
    {
      const char *fn_suffix
	= flag_profile_update == PROFILE_UPDATE_ATOMIC ? "_atomic" : "";

      gcov_type_node = get_gcov_type ();
      gcov_type_ptr = build_pointer_type (gcov_type_node);

      /* void (*) (gcov_type *, gcov_type, int, unsigned)  */
      interval_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  integer_type_node,
					  unsigned_type_node, NULL_TREE);
      fn_name = concat ("__gcov_interval_profiler", fn_suffix, NULL);
      tree_interval_profiler_fn = build_fn_decl (fn_name,
						 interval_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_interval_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_interval_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_interval_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      pow2_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_pow2_profiler", fn_suffix, NULL);
      tree_pow2_profiler_fn = build_fn_decl (fn_name, pow2_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_pow2_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_pow2_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_pow2_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      one_value_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_one_value_profiler", fn_suffix, NULL);
      tree_one_value_profiler_fn = build_fn_decl (fn_name,
						  one_value_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_one_value_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_one_value_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_one_value_profiler_fn));

      init_ic_make_global_vars ();

      /* void (*) (gcov_type, void *)  */
      ic_profiler_fn_type
	       = build_function_type_list (void_type_node,
					  gcov_type_node,
					  ptr_type_node,
					  NULL_TREE);
      profiler_fn_name = "__gcov_indirect_call_profiler_v3";
      if (PARAM_VALUE (PARAM_INDIR_CALL_TOPN_PROFILE))
	profiler_fn_name = "__gcov_indirect_call_topn_profiler";

      tree_indirect_call_profiler_fn
	      = build_fn_decl (profiler_fn_name, ic_profiler_fn_type);

      TREE_NOTHROW (tree_indirect_call_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_indirect_call_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_indirect_call_profiler_fn));

      tree_time_profiler_counter
	= build_decl (UNKNOWN_LOCATION, VAR_DECL,
		      get_identifier ("__gcov_time_profiler_counter"),
		      get_gcov_type ());
      TREE_PUBLIC (tree_time_profiler_counter) = 1;
      DECL_EXTERNAL (tree_time_profiler_counter) = 1;
      TREE_STATIC (tree_time_profiler_counter) = 1;
      DECL_ARTIFICIAL (tree_time_profiler_counter) = 1;
      DECL_INITIAL (tree_time_profiler_counter) = NULL;

      /* void (*) (gcov_type *, gcov_type)  */
      average_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node, NULL_TREE);
      fn_name = concat ("__gcov_average_profiler", fn_suffix, NULL);
      tree_average_profiler_fn = build_fn_decl (fn_name,
						average_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_average_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_average_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_average_profiler_fn));
      fn_name = concat ("__gcov_ior_profiler", fn_suffix, NULL);
      tree_ior_profiler_fn = build_fn_decl (fn_name, average_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
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
  tree one;

  one = build_int_cst (gcov_type_node, 1);

  if (flag_profile_update == PROFILE_UPDATE_ATOMIC)
    {
      /* __atomic_fetch_add (&counter, 1, MEMMODEL_RELAXED); */
      tree addr = tree_coverage_counter_addr (GCOV_COUNTER_ARCS, edgeno);
      tree f = builtin_decl_explicit (LONG_LONG_TYPE_SIZE > 32
				      ? BUILT_IN_ATOMIC_FETCH_ADD_8:
				      BUILT_IN_ATOMIC_FETCH_ADD_4);
      gcall *stmt = gimple_build_call (f, 3, addr, one,
				       build_int_cst (integer_type_node,
						      MEMMODEL_RELAXED));
      gsi_insert_on_edge (e, stmt);
    }
  else
    {
      tree ref = tree_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
      tree gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
						   NULL, "PROF_edge_counter");
      gassign *stmt1 = gimple_build_assign (gcov_type_tmp_var, ref);
      gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
					      NULL, "PROF_edge_counter");
      gassign *stmt2 = gimple_build_assign (gcov_type_tmp_var, PLUS_EXPR,
					    gimple_assign_lhs (stmt1), one);
      gassign *stmt3 = gimple_build_assign (unshare_expr (ref),
					    gimple_assign_lhs (stmt2));
      gsi_insert_on_edge (e, stmt1);
      gsi_insert_on_edge (e, stmt2);
      gsi_insert_on_edge (e, stmt3);
    }
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
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref = tree_coverage_counter_ref (tag, base), ref_ptr;
  gcall *call;
  tree val;
  tree start = build_int_cst_type (integer_type_node,
				   value->hdata.intvl.int_start);
  tree steps = build_int_cst_type (unsigned_type_node,
				   value->hdata.intvl.steps);

  ref_ptr = force_gimple_operand_gsi (&gsi,
				      build_addr (ref),
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
  gimple *stmt = value->hvalue.stmt;
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
  gimple *stmt = value->hvalue.stmt;
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
  gimple *stmt = value->hvalue.stmt;
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

    stmt1: __gcov_indirect_call.counters = get_relevant_counter_ptr ();
    stmt2: tmp1 = (void *) (indirect call argument value)
    stmt3: __gcov_indirect_call.callee = tmp1;

    Example:
      f_1 = foo;
      __gcov_indirect_call.counters = &__gcov4.main[0];
      PROF_9 = f_1;
      __gcov_indirect_call_callee = PROF_9;
      _4 = f_1 ();
   */

  tree gcov_type_ptr = build_pointer_type (get_gcov_type ());

  tree counter_ref = build3 (COMPONENT_REF, gcov_type_ptr,
			     ic_tuple_var, ic_tuple_counters_field, NULL_TREE);

  stmt1 = gimple_build_assign (counter_ref, ref_ptr);
  tmp1 = make_temp_ssa_name (ptr_type_node, NULL, "PROF");
  stmt2 = gimple_build_assign (tmp1, unshare_expr (value->hvalue.value));
  tree callee_ref = build3 (COMPONENT_REF, ptr_type_node,
			     ic_tuple_var, ic_tuple_callee_field, NULL_TREE);
  stmt3 = gimple_build_assign (callee_ref, tmp1);

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
  gcall *stmt1;
  tree tree_uid, cur_func, void0;

  if (c_node->only_called_directly_p ())
    return;

  gimple_init_gcov_profiler ();

  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block cond_bb = split_edge (single_succ_edge (entry));
  basic_block update_bb = split_edge (single_succ_edge (cond_bb));

  /* We need to do an extra split in order to not create an input
     for a possible PHI node.  */
  split_edge (single_succ_edge (update_bb));

  edge true_edge = single_succ_edge (cond_bb);
  true_edge->flags = EDGE_TRUE_VALUE;

  profile_probability probability;
  if (DECL_VIRTUAL_P (current_function_decl))
    probability = profile_probability::very_likely ();
  else
    probability = profile_probability::unlikely ();

  true_edge->probability = probability;
  edge e = make_edge (cond_bb, single_succ_edge (update_bb)->dest,
		      EDGE_FALSE_VALUE);
  e->probability = true_edge->probability.invert ();

  /* Insert code:

     if (__gcov_indirect_call_callee != NULL)
       __gcov_indirect_call_profiler_v3 (profile_id, &current_function_decl);

     The function __gcov_indirect_call_profiler_v3 is responsible for
     resetting __gcov_indirect_call_callee to NULL.  */

  gimple_stmt_iterator gsi = gsi_start_bb (cond_bb);
  void0 = build_int_cst (ptr_type_node, 0);

  tree callee_ref = build3 (COMPONENT_REF, ptr_type_node,
			    ic_tuple_var, ic_tuple_callee_field, NULL_TREE);

  tree ref = force_gimple_operand_gsi (&gsi, callee_ref, true, NULL_TREE,
				       true, GSI_SAME_STMT);

  gcond *cond = gimple_build_cond (NE_EXPR, ref,
				   void0, NULL, NULL);
  gsi_insert_before (&gsi, cond, GSI_NEW_STMT);

  gsi = gsi_after_labels (update_bb);

  cur_func = force_gimple_operand_gsi (&gsi,
				       build_addr (current_function_decl),
				       true, NULL_TREE,
				       true, GSI_SAME_STMT);
  tree_uid = build_int_cst
	      (gcov_type_node,
	       cgraph_node::get (current_function_decl)->profile_id);
  stmt1 = gimple_build_call (tree_indirect_call_profiler_fn, 2,
			     tree_uid, cur_func);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE tree at the beginning for each function.
   TAG is the tag of the section for counters, BASE is offset of the
   counter position and GSI is the iterator we place the counter.  */

void
gimple_gen_time_profiler (unsigned tag, unsigned base)
{
  tree type = get_gcov_type ();
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block cond_bb = split_edge (single_succ_edge (entry));
  basic_block update_bb = split_edge (single_succ_edge (cond_bb));

  /* We need to do an extra split in order to not create an input
     for a possible PHI node.  */
  split_edge (single_succ_edge (update_bb));

  edge true_edge = single_succ_edge (cond_bb);
  true_edge->flags = EDGE_TRUE_VALUE;
  true_edge->probability = profile_probability::unlikely ();
  edge e
    = make_edge (cond_bb, single_succ_edge (update_bb)->dest, EDGE_FALSE_VALUE);
  e->probability = true_edge->probability.invert ();

  gimple_stmt_iterator gsi = gsi_start_bb (cond_bb);
  tree original_ref = tree_coverage_counter_ref (tag, base);
  tree ref = force_gimple_operand_gsi (&gsi, original_ref, true, NULL_TREE,
				       true, GSI_SAME_STMT);
  tree one = build_int_cst (type, 1);

  /* Emit: if (counters[0] != 0).  */
  gcond *cond = gimple_build_cond (EQ_EXPR, ref, build_int_cst (type, 0),
				   NULL, NULL);
  gsi_insert_before (&gsi, cond, GSI_NEW_STMT);

  gsi = gsi_start_bb (update_bb);

  /* Emit: counters[0] = ++__gcov_time_profiler_counter.  */
  if (flag_profile_update == PROFILE_UPDATE_ATOMIC)
    {
      tree ptr = make_temp_ssa_name (build_pointer_type (type), NULL,
				     "time_profiler_counter_ptr");
      tree addr = build1 (ADDR_EXPR, TREE_TYPE (ptr),
			  tree_time_profiler_counter);
      gassign *assign = gimple_build_assign (ptr, NOP_EXPR, addr);
      gsi_insert_before (&gsi, assign, GSI_NEW_STMT);
      tree f = builtin_decl_explicit (LONG_LONG_TYPE_SIZE > 32
				      ? BUILT_IN_ATOMIC_ADD_FETCH_8:
				      BUILT_IN_ATOMIC_ADD_FETCH_4);
      gcall *stmt = gimple_build_call (f, 3, ptr, one,
				       build_int_cst (integer_type_node,
						      MEMMODEL_RELAXED));
      tree result_type = TREE_TYPE (TREE_TYPE (f));
      tree tmp = make_temp_ssa_name (result_type, NULL, "time_profile");
      gimple_set_lhs (stmt, tmp);
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
      tmp = make_temp_ssa_name (type, NULL, "time_profile");
      assign = gimple_build_assign (tmp, NOP_EXPR,
				    gimple_call_lhs (stmt));
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (original_ref, tmp);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
    }
  else
    {
      tree tmp = make_temp_ssa_name (type, NULL, "time_profile");
      gassign *assign = gimple_build_assign (tmp, tree_time_profiler_counter);
      gsi_insert_before (&gsi, assign, GSI_NEW_STMT);

      tmp = make_temp_ssa_name (type, NULL, "time_profile");
      assign = gimple_build_assign (tmp, PLUS_EXPR, gimple_assign_lhs (assign),
				    one);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (original_ref, tmp);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (tree_time_profiler_counter, tmp);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
    }
}

/* Output instructions as GIMPLE trees to increment the average histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_average_profiler (histogram_value value, unsigned tag, unsigned base)
{
  gimple *stmt = value->hvalue.stmt;
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
  gimple *stmt = value->hvalue.stmt;
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

static vec<regex_t> profile_filter_files;
static vec<regex_t> profile_exclude_files;

/* Parse list of provided REGEX (separated with semi-collon) and
   create expressions (of type regex_t) and save them into V vector.
   If there is a regular expression parsing error, error message is
   printed for FLAG_NAME.  */

static void
parse_profile_filter (const char *regex, vec<regex_t> *v,
		      const char *flag_name)
{
  v->create (4);
  if (regex != NULL)
    {
      char *str = xstrdup (regex);
      for (char *p = strtok (str, ";"); p != NULL; p = strtok (NULL, ";"))
	{
	  regex_t r;
	  if (regcomp (&r, p, REG_EXTENDED | REG_NOSUB) != 0)
	    {
	      error ("invalid regular expression %qs in %qs",
		     p, flag_name);
	      return;
	    }

	  v->safe_push (r);
	}
    }
}

/* Parse values of -fprofile-filter-files and -fprofile-exclude-files
   options.  */

static void
parse_profile_file_filtering ()
{
  parse_profile_filter (flag_profile_filter_files, &profile_filter_files,
			"-fprofile-filter-files");
  parse_profile_filter (flag_profile_exclude_files, &profile_exclude_files,
			"-fprofile-exclude-files");
}

/* Parse vectors of regular expressions.  */

static void
release_profile_file_filtering ()
{
  profile_filter_files.release ();
  profile_exclude_files.release ();
}

/* Return true when FILENAME should be instrumented based on
   -fprofile-filter-files and -fprofile-exclude-files options.  */

static bool
include_source_file_for_profile (const char *filename)
{
  /* First check whether file is included in flag_profile_exclude_files.  */
  for (unsigned i = 0; i < profile_exclude_files.length (); i++)
    if (regexec (&profile_exclude_files[i],
		 filename, 0, NULL, 0) == REG_NOERROR)
      return false;

  /* For non-empty flag_profile_filter_files include only files matching a
     regex in the flag.  */
  if (profile_filter_files.is_empty ())
    return true;

  for (unsigned i = 0; i < profile_filter_files.length (); i++)
    if (regexec (&profile_filter_files[i], filename, 0, NULL, 0) == REG_NOERROR)
      return true;

  return false;
}

#ifndef HAVE_sync_compare_and_swapsi
#define HAVE_sync_compare_and_swapsi 0
#endif
#ifndef HAVE_atomic_compare_and_swapsi
#define HAVE_atomic_compare_and_swapsi 0
#endif

#ifndef HAVE_sync_compare_and_swapdi
#define HAVE_sync_compare_and_swapdi 0
#endif
#ifndef HAVE_atomic_compare_and_swapdi
#define HAVE_atomic_compare_and_swapdi 0
#endif

/* Profile all functions in the callgraph.  */

static unsigned int
tree_profiling (void)
{
  struct cgraph_node *node;

  /* Verify whether we can utilize atomic update operations.  */
  bool can_support_atomic = false;
  unsigned HOST_WIDE_INT gcov_type_size
    = tree_to_uhwi (TYPE_SIZE_UNIT (get_gcov_type ()));
  if (gcov_type_size == 4)
    can_support_atomic
      = HAVE_sync_compare_and_swapsi || HAVE_atomic_compare_and_swapsi;
  else if (gcov_type_size == 8)
    can_support_atomic
      = HAVE_sync_compare_and_swapdi || HAVE_atomic_compare_and_swapdi;

  if (flag_profile_update == PROFILE_UPDATE_ATOMIC
      && !can_support_atomic)
    {
      warning (0, "target does not support atomic profile update, "
	       "single mode is selected");
      flag_profile_update = PROFILE_UPDATE_SINGLE;
    }
  else if (flag_profile_update == PROFILE_UPDATE_PREFER_ATOMIC)
    flag_profile_update = can_support_atomic
      ? PROFILE_UPDATE_ATOMIC : PROFILE_UPDATE_SINGLE;

  /* This is a small-ipa pass that gets called only once, from
     cgraphunit.c:ipa_passes().  */
  gcc_assert (symtab->state == IPA_SSA);

  init_node_map (true);
  parse_profile_file_filtering ();

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      bool thunk = false;
      if (!gimple_has_body_p (node->decl) && !node->thunk.thunk_p)
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      if (lookup_attribute ("no_profile_instrument_function",
			    DECL_ATTRIBUTES (node->decl)))
	continue;
      /* Do not instrument extern inline functions when testing coverage.
	 While this is not perfectly consistent (early inlined extern inlines
	 will get acocunted), testsuite expects that.  */
      if (DECL_EXTERNAL (node->decl)
	  && flag_test_coverage)
	continue;

      const char *file = LOCATION_FILE (DECL_SOURCE_LOCATION (node->decl));
      if (!include_source_file_for_profile (file))
	continue;

      if (node->thunk.thunk_p)
	{
	  /* We cannot expand variadic thunks to Gimple.  */
	  if (stdarg_p (TREE_TYPE (node->decl)))
	    continue;
	  thunk = true;
	  /* When generate profile, expand thunk to gimple so it can be
	     instrumented same way as other functions.  */
	  if (profile_arc_flag)
	    node->expand_thunk (false, true);
	  /* Read cgraph profile but keep function as thunk at profile-use
	     time.  */
	  else
	    {
	      read_thunk_profile (node);
	      continue;
	    }
	}

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));

      if (dump_file)
	dump_function_header (dump_file, cfun->decl, dump_flags);

      /* Local pure-const may imply need to fixup the cfg.  */
      if (gimple_has_body_p (node->decl)
	  && (execute_fixup_cfg () & TODO_cleanup_cfg))
	cleanup_tree_cfg ();

      branch_prob (thunk);

      if (! flag_branch_probabilities
	  && flag_profile_values)
	gimple_gen_ic_func_profiler ();

      if (flag_branch_probabilities
	  && !thunk
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

  release_profile_file_filtering ();

  /* Drop pure/const flags from instrumented functions.  */
  if (profile_arc_flag || flag_test_coverage)
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
	      gimple *stmt = gsi_stmt (gsi);
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
  TODO_dump_symtab, /* todo_flags_finish */
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
