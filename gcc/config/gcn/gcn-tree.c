/* Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

/* {{{ Includes.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "gimple-iterator.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "tm_p.h"
#include "stringpool.h"
#include "fold-const.h"
#include "varasm.h"
#include "omp-low.h"
#include "omp-general.h"
#include "internal-fn.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "tree-ssa-operands.h"
#include "gimplify.h"
#include "tree-phinodes.h"
#include "cgraph.h"
#include "targhooks.h"
#include "langhooks-def.h"

/* }}}  */
/* {{{ OMP GCN pass.
 
   This pass is intended to make any GCN-specfic transformations to OpenMP
   target regions.
 
   At present, its only purpose is to convert some "omp" built-in functions
   to use closer-to-the-metal "gcn" built-in functions.  */

unsigned int
execute_omp_gcn (void)
{
  tree thr_num_tree = builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM);
  tree thr_num_id = DECL_NAME (thr_num_tree);
  tree team_num_tree = builtin_decl_explicit (BUILT_IN_OMP_GET_TEAM_NUM);
  tree team_num_id = DECL_NAME (team_num_tree);
  basic_block bb;
  gimple_stmt_iterator gsi;
  unsigned int todo = 0;

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *call = gsi_stmt (gsi);
      tree decl;

      if (is_gimple_call (call) && (decl = gimple_call_fndecl (call)))
	{
	  tree decl_id = DECL_NAME (decl);
	  tree lhs = gimple_get_lhs (call);

	  if (decl_id == thr_num_id)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "Replace '%s' with __builtin_gcn_dim_pos.\n",
			 IDENTIFIER_POINTER (decl_id));

	      /* Transform this:
	         lhs = __builtin_omp_get_thread_num ()
	         to this:
	         lhs = __builtin_gcn_dim_pos (1)  */
	      tree fn = targetm.builtin_decl (GCN_BUILTIN_OMP_DIM_POS, 0);
	      tree fnarg = build_int_cst (unsigned_type_node, 1);
	      gimple *stmt = gimple_build_call (fn, 1, fnarg);
	      gimple_call_set_lhs (stmt, lhs);
	      gsi_replace (&gsi, stmt, true);

	      todo |= TODO_update_ssa;
	    }
	  else if (decl_id == team_num_id)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "Replace '%s' with __builtin_gcn_dim_pos.\n",
			 IDENTIFIER_POINTER (decl_id));

	      /* Transform this:
	         lhs = __builtin_omp_get_team_num ()
	         to this:
	         lhs = __builtin_gcn_dim_pos (0)  */
	      tree fn = targetm.builtin_decl (GCN_BUILTIN_OMP_DIM_POS, 0);
	      tree fnarg = build_zero_cst (unsigned_type_node);
	      gimple *stmt = gimple_build_call (fn, 1, fnarg);
	      gimple_call_set_lhs (stmt, lhs);
	      gsi_replace (&gsi, stmt, true);

	      todo |= TODO_update_ssa;
	    }
	}
    }

  return todo;
}

namespace
{

  const pass_data pass_data_omp_gcn = {
    GIMPLE_PASS,
    "omp_gcn",			/* name */
    OPTGROUP_NONE,		/* optinfo_flags */
    TV_NONE,			/* tv_id */
    0,				/* properties_required */
    0,				/* properties_provided */
    0,				/* properties_destroyed */
    0,				/* todo_flags_start */
    TODO_df_finish,		/* todo_flags_finish */
  };

  class pass_omp_gcn : public gimple_opt_pass
  {
  public:
    pass_omp_gcn (gcc::context *ctxt)
      : gimple_opt_pass (pass_data_omp_gcn, ctxt)
    {
    }

    /* opt_pass methods: */
    virtual bool gate (function *)
    {
      return flag_openmp;
    }

    virtual unsigned int execute (function *)
    {
      return execute_omp_gcn ();
    }

  }; /* class pass_omp_gcn.  */

} /* anon namespace.  */

gimple_opt_pass *
make_pass_omp_gcn (gcc::context *ctxt)
{
  return new pass_omp_gcn (ctxt);
}

/* }}}  */
/* {{{ OpenACC reductions.  */

/* Global lock variable, needed for 128bit worker & gang reductions.  */

static GTY(()) tree global_lock_var;

/* Lazily generate the global_lock_var decl and return its address.  */

static tree
gcn_global_lock_addr ()
{
  tree v = global_lock_var;

  if (!v)
    {
      tree name = get_identifier ("__reduction_lock");
      tree type = build_qualified_type (unsigned_type_node,
					TYPE_QUAL_VOLATILE);
      v = build_decl (BUILTINS_LOCATION, VAR_DECL, name, type);
      global_lock_var = v;
      DECL_ARTIFICIAL (v) = 1;
      DECL_EXTERNAL (v) = 1;
      TREE_STATIC (v) = 1;
      TREE_PUBLIC (v) = 1;
      TREE_USED (v) = 1;
      mark_addressable (v);
      mark_decl_referenced (v);
    }

  return build_fold_addr_expr (v);
}

/* Helper function for gcn_reduction_update.

   Insert code to locklessly update *PTR with *PTR OP VAR just before
   GSI.  We use a lockless scheme for nearly all case, which looks
   like:
     actual = initval (OP);
     do {
       guess = actual;
       write = guess OP myval;
       actual = cmp&swap (ptr, guess, write)
     } while (actual bit-different-to guess);
   return write;

   This relies on a cmp&swap instruction, which is available for 32- and
   64-bit types.  Larger types must use a locking scheme.  */

static tree
gcn_lockless_update (location_t loc, gimple_stmt_iterator *gsi,
		     tree ptr, tree var, tree_code op)
{
  unsigned fn = GCN_BUILTIN_CMP_SWAP;
  tree_code code = NOP_EXPR;
  tree arg_type = unsigned_type_node;
  tree var_type = TREE_TYPE (var);

  if (TREE_CODE (var_type) == COMPLEX_TYPE
      || TREE_CODE (var_type) == REAL_TYPE)
    code = VIEW_CONVERT_EXPR;

  if (TYPE_SIZE (var_type) == TYPE_SIZE (long_long_unsigned_type_node))
    {
      arg_type = long_long_unsigned_type_node;
      fn = GCN_BUILTIN_CMP_SWAPLL;
    }

  tree swap_fn = gcn_builtin_decl (fn, true);

  gimple_seq init_seq = NULL;
  tree init_var = make_ssa_name (arg_type);
  tree init_expr = omp_reduction_init_op (loc, op, var_type);
  init_expr = fold_build1 (code, arg_type, init_expr);
  gimplify_assign (init_var, init_expr, &init_seq);
  gimple *init_end = gimple_seq_last (init_seq);

  gsi_insert_seq_before (gsi, init_seq, GSI_SAME_STMT);

  /* Split the block just after the init stmts.  */
  basic_block pre_bb = gsi_bb (*gsi);
  edge pre_edge = split_block (pre_bb, init_end);
  basic_block loop_bb = pre_edge->dest;
  pre_bb = pre_edge->src;
  /* Reset the iterator.  */
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));

  tree expect_var = make_ssa_name (arg_type);
  tree actual_var = make_ssa_name (arg_type);
  tree write_var = make_ssa_name (arg_type);

  /* Build and insert the reduction calculation.  */
  gimple_seq red_seq = NULL;
  tree write_expr = fold_build1 (code, var_type, expect_var);
  write_expr = fold_build2 (op, var_type, write_expr, var);
  write_expr = fold_build1 (code, arg_type, write_expr);
  gimplify_assign (write_var, write_expr, &red_seq);

  gsi_insert_seq_before (gsi, red_seq, GSI_SAME_STMT);

  /* Build & insert the cmp&swap sequence.  */
  gimple_seq latch_seq = NULL;
  tree swap_expr = build_call_expr_loc (loc, swap_fn, 3,
					ptr, expect_var, write_var);
  gimplify_assign (actual_var, swap_expr, &latch_seq);

  gcond *cond = gimple_build_cond (EQ_EXPR, actual_var, expect_var,
				   NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&latch_seq, cond);

  gimple *latch_end = gimple_seq_last (latch_seq);
  gsi_insert_seq_before (gsi, latch_seq, GSI_SAME_STMT);

  /* Split the block just after the latch stmts.  */
  edge post_edge = split_block (loop_bb, latch_end);
  basic_block post_bb = post_edge->dest;
  loop_bb = post_edge->src;
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));

  post_edge->flags ^= EDGE_TRUE_VALUE | EDGE_FALLTHRU;
  /* post_edge->probability = profile_probability::even ();  */
  edge loop_edge = make_edge (loop_bb, loop_bb, EDGE_FALSE_VALUE);
  /* loop_edge->probability = profile_probability::even ();  */
  set_immediate_dominator (CDI_DOMINATORS, loop_bb, pre_bb);
  set_immediate_dominator (CDI_DOMINATORS, post_bb, loop_bb);

  gphi *phi = create_phi_node (expect_var, loop_bb);
  add_phi_arg (phi, init_var, pre_edge, loc);
  add_phi_arg (phi, actual_var, loop_edge, loc);

  loop *loop = alloc_loop ();
  loop->header = loop_bb;
  loop->latch = loop_bb;
  add_loop (loop, loop_bb->loop_father);

  return fold_build1 (code, var_type, write_var);
}

/* Helper function for gcn_reduction_update.
   
   Insert code to lockfully update *PTR with *PTR OP VAR just before
   GSI.  This is necessary for types larger than 64 bits, where there
   is no cmp&swap instruction to implement a lockless scheme.  We use
   a lock variable in global memory.

   while (cmp&swap (&lock_var, 0, 1))
     continue;
   T accum = *ptr;
   accum = accum OP var;
   *ptr = accum;
   cmp&swap (&lock_var, 1, 0);
   return accum;

   A lock in global memory is necessary to force execution engine
   descheduling and avoid resource starvation that can occur if the
   lock is in shared memory.  */

static tree
gcn_lockfull_update (location_t loc, gimple_stmt_iterator *gsi,
		     tree ptr, tree var, tree_code op)
{
  tree var_type = TREE_TYPE (var);
  tree swap_fn = gcn_builtin_decl (GCN_BUILTIN_CMP_SWAP, true);
  tree uns_unlocked = build_int_cst (unsigned_type_node, 0);
  tree uns_locked = build_int_cst (unsigned_type_node, 1);

  /* Split the block just before the gsi.  Insert a gimple nop to make
     this easier.  */
  gimple *nop = gimple_build_nop ();
  gsi_insert_before (gsi, nop, GSI_SAME_STMT);
  basic_block entry_bb = gsi_bb (*gsi);
  edge entry_edge = split_block (entry_bb, nop);
  basic_block lock_bb = entry_edge->dest;
  /* Reset the iterator.  */
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));

  /* Build and insert the locking sequence.  */
  gimple_seq lock_seq = NULL;
  tree lock_var = make_ssa_name (unsigned_type_node);
  tree lock_expr = gcn_global_lock_addr ();
  lock_expr = build_call_expr_loc (loc, swap_fn, 3, lock_expr,
				   uns_unlocked, uns_locked);
  gimplify_assign (lock_var, lock_expr, &lock_seq);
  gcond *cond = gimple_build_cond (EQ_EXPR, lock_var, uns_unlocked,
				   NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&lock_seq, cond);
  gimple *lock_end = gimple_seq_last (lock_seq);
  gsi_insert_seq_before (gsi, lock_seq, GSI_SAME_STMT);

  /* Split the block just after the lock sequence.  */
  edge locked_edge = split_block (lock_bb, lock_end);
  basic_block update_bb = locked_edge->dest;
  lock_bb = locked_edge->src;
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));

  /* Create the lock loop.  */
  locked_edge->flags ^= EDGE_TRUE_VALUE | EDGE_FALLTHRU;
  locked_edge->probability = profile_probability::even ();
  edge loop_edge = make_edge (lock_bb, lock_bb, EDGE_FALSE_VALUE);
  loop_edge->probability = profile_probability::even ();
  set_immediate_dominator (CDI_DOMINATORS, lock_bb, entry_bb);
  set_immediate_dominator (CDI_DOMINATORS, update_bb, lock_bb);

  /* Create the loop structure.  */
  loop *lock_loop = alloc_loop ();
  lock_loop->header = lock_bb;
  lock_loop->latch = lock_bb;
  lock_loop->nb_iterations_estimate = 1;
  lock_loop->any_estimate = true;
  add_loop (lock_loop, entry_bb->loop_father);

  /* Build and insert the reduction calculation.  */
  gimple_seq red_seq = NULL;
  tree acc_in = make_ssa_name (var_type);
  tree ref_in = build_simple_mem_ref (ptr);
  TREE_THIS_VOLATILE (ref_in) = 1;
  gimplify_assign (acc_in, ref_in, &red_seq);

  tree acc_out = make_ssa_name (var_type);
  tree update_expr = fold_build2 (op, var_type, ref_in, var);
  gimplify_assign (acc_out, update_expr, &red_seq);

  tree ref_out = build_simple_mem_ref (ptr);
  TREE_THIS_VOLATILE (ref_out) = 1;
  gimplify_assign (ref_out, acc_out, &red_seq);

  gsi_insert_seq_before (gsi, red_seq, GSI_SAME_STMT);

  /* Build & insert the unlock sequence.  */
  gimple_seq unlock_seq = NULL;
  tree unlock_expr = gcn_global_lock_addr ();
  unlock_expr = build_call_expr_loc (loc, swap_fn, 3, unlock_expr,
				     uns_locked, uns_unlocked);
  gimplify_and_add (unlock_expr, &unlock_seq);
  gsi_insert_seq_before (gsi, unlock_seq, GSI_SAME_STMT);

  return acc_out;
}

/* Emit a sequence to update a reduction accumulator at *PTR with the
   value held in VAR using operator OP.  Return the updated value.

   TODO: optimize for atomic ops and independent complex ops.  */

static tree
gcn_reduction_update (location_t loc, gimple_stmt_iterator *gsi,
		      tree ptr, tree var, tree_code op)
{
  tree type = TREE_TYPE (var);
  tree size = TYPE_SIZE (type);

  if (size == TYPE_SIZE (unsigned_type_node)
      || size == TYPE_SIZE (long_long_unsigned_type_node))
    return gcn_lockless_update (loc, gsi, ptr, var, op);
  else
    return gcn_lockfull_update (loc, gsi, ptr, var, op);
}

/* Return a temporary variable decl to use for an OpenACC worker reduction.  */

static tree
gcn_goacc_get_worker_red_decl (tree type, unsigned offset)
{
  machine_function *machfun = cfun->machine;
  tree existing_decl;

  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  tree var_type
    = build_qualified_type (type,
			    (TYPE_QUALS (type)
			     | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_LDS)));

  if (machfun->reduc_decls
      && offset < machfun->reduc_decls->length ()
      && (existing_decl = (*machfun->reduc_decls)[offset]))
    {
      gcc_assert (TREE_TYPE (existing_decl) == var_type);
      return existing_decl;
    }
  else
    {
      char name[50];
      sprintf (name, ".oacc_reduction_%u", offset);
      tree decl = create_tmp_var_raw (var_type, name);

      DECL_CONTEXT (decl) = NULL_TREE;
      TREE_STATIC (decl) = 1;

      varpool_node::finalize_decl (decl);

      vec_safe_grow_cleared (machfun->reduc_decls, offset + 1);
      (*machfun->reduc_decls)[offset] = decl;

      return decl;
    }

  return NULL_TREE;
}

/* Expand IFN_GOACC_REDUCTION_SETUP.  */

static void
gcn_goacc_reduction_setup (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  gimple_seq seq = NULL;

  push_gimplify_context (true);

  if (level != GOMP_DIM_GANG)
    {
      /* Copy the receiver object.  */
      tree ref_to_res = gimple_call_arg (call, 1);

      if (!integer_zerop (ref_to_res))
	var = build_simple_mem_ref (ref_to_res);
    }

  if (level == GOMP_DIM_WORKER)
    {
      tree var_type = TREE_TYPE (var);
      /* Store incoming value to worker reduction buffer.  */
      tree offset = gimple_call_arg (call, 5);
      tree decl
	= gcn_goacc_get_worker_red_decl (var_type, TREE_INT_CST_LOW (offset));

      gimplify_assign (decl, var, &seq);
    }

  if (lhs)
    gimplify_assign (lhs, var, &seq);

  pop_gimplify_context (NULL);
  gsi_replace_with_seq (&gsi, seq, true);
}

/* Expand IFN_GOACC_REDUCTION_INIT.  */

static void
gcn_goacc_reduction_init (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  enum tree_code rcode
    = (enum tree_code) TREE_INT_CST_LOW (gimple_call_arg (call, 4));
  tree init = omp_reduction_init_op (gimple_location (call), rcode,
				     TREE_TYPE (var));
  gimple_seq seq = NULL;

  push_gimplify_context (true);

  if (level == GOMP_DIM_GANG)
    {
      /* If there's no receiver object, propagate the incoming VAR.  */
      tree ref_to_res = gimple_call_arg (call, 1);
      if (integer_zerop (ref_to_res))
	init = var;
    }

  if (lhs)
    gimplify_assign (lhs, init, &seq);

  pop_gimplify_context (NULL);
  gsi_replace_with_seq (&gsi, seq, true);
}

/* Expand IFN_GOACC_REDUCTION_FINI.  */

static void
gcn_goacc_reduction_fini (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree ref_to_res = gimple_call_arg (call, 1);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  enum tree_code op
    = (enum tree_code) TREE_INT_CST_LOW (gimple_call_arg (call, 4));
  gimple_seq seq = NULL;
  tree r = NULL_TREE;;

  push_gimplify_context (true);

  tree accum = NULL_TREE;

  if (level == GOMP_DIM_WORKER)
    {
      tree var_type = TREE_TYPE (var);
      tree offset = gimple_call_arg (call, 5);
      tree decl
	= gcn_goacc_get_worker_red_decl (var_type, TREE_INT_CST_LOW (offset));

      accum = build_fold_addr_expr (decl);
    }
  else if (integer_zerop (ref_to_res))
    r = var;
  else
    accum = ref_to_res;

  if (accum)
    {
      /* UPDATE the accumulator.  */
      gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
      seq = NULL;
      r = gcn_reduction_update (gimple_location (call), &gsi, accum, var, op);
    }

  if (lhs)
    gimplify_assign (lhs, r, &seq);
  pop_gimplify_context (NULL);

  gsi_replace_with_seq (&gsi, seq, true);
}

/* Expand IFN_GOACC_REDUCTION_TEARDOWN.  */

static void
gcn_goacc_reduction_teardown (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  gimple_seq seq = NULL;

  push_gimplify_context (true);

  if (level == GOMP_DIM_WORKER)
    {
      tree var_type = TREE_TYPE (var);

      /* Read the worker reduction buffer.  */
      tree offset = gimple_call_arg (call, 5);
      tree decl
	= gcn_goacc_get_worker_red_decl (var_type, TREE_INT_CST_LOW (offset));
      var = decl;
    }

  if (level != GOMP_DIM_GANG)
    {
      /* Write to the receiver object.  */
      tree ref_to_res = gimple_call_arg (call, 1);

      if (!integer_zerop (ref_to_res))
	gimplify_assign (build_simple_mem_ref (ref_to_res), var, &seq);
    }

  if (lhs)
    gimplify_assign (lhs, var, &seq);

  pop_gimplify_context (NULL);

  gsi_replace_with_seq (&gsi, seq, true);
}

/* Implement TARGET_GOACC_REDUCTION.
 
   Expand calls to the GOACC REDUCTION internal function, into a sequence of
   gimple instructions.  */

void
gcn_goacc_reduction (gcall *call)
{
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));

  if (level == GOMP_DIM_VECTOR)
    {
      default_goacc_reduction (call);
      return;
    }

  unsigned code = (unsigned) TREE_INT_CST_LOW (gimple_call_arg (call, 0));

  switch (code)
    {
    case IFN_GOACC_REDUCTION_SETUP:
      gcn_goacc_reduction_setup (call);
      break;

    case IFN_GOACC_REDUCTION_INIT:
      gcn_goacc_reduction_init (call);
      break;

    case IFN_GOACC_REDUCTION_FINI:
      gcn_goacc_reduction_fini (call);
      break;

    case IFN_GOACC_REDUCTION_TEARDOWN:
      gcn_goacc_reduction_teardown (call);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_GOACC_ADJUST_PROPAGATION_RECORD.
 
   Tweak (worker) propagation record, e.g. to put it in shared memory.  */

tree
gcn_goacc_adjust_propagation_record (tree record_type, bool sender,
				     const char *name)
{
  tree type = record_type;

  TYPE_ADDR_SPACE (type) = ADDR_SPACE_LDS;

  if (!sender)
    type = build_pointer_type (type);

  tree decl = create_tmp_var_raw (type, name);

  if (sender)
    {
      DECL_CONTEXT (decl) = NULL_TREE;
      TREE_STATIC (decl) = 1;
    }

  if (sender)
    varpool_node::finalize_decl (decl);

  return decl;
}

void
gcn_goacc_adjust_gangprivate_decl (tree var)
{
  tree type = TREE_TYPE (var);
  tree lds_type = build_qualified_type (type,
		    TYPE_QUALS_NO_ADDR_SPACE (type)
		    | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_LDS));
  machine_function *machfun = cfun->machine;

  TREE_TYPE (var) = lds_type;
  TREE_STATIC (var) = 1;

  /* We're making VAR static.  We have to mangle the name to avoid collisions
     between different local variables that share the same names.  */
  lhd_set_decl_assembler_name (var);

  varpool_node::finalize_decl (var);

  if (machfun)
    machfun->use_flat_addressing = true;
}

/* }}}  */
