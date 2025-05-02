/* Copyright (C) 2017-2025 Free Software Foundation, Inc.

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
#include "omp-offload.h"
#include "internal-fn.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "tree-ssa-operands.h"
#include "gimplify.h"
#include "tree-phinodes.h"
#include "cgraph.h"
#include "targhooks.h"
#include "langhooks-def.h"
#include "memmodel.h"

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

/* Pointer variables for array reduction buffers used.  */
static vec<tree> gcn_array_reduction_buffers;

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
      || SCALAR_FLOAT_TYPE_P (var_type))
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
  tree ref_in
    = build_simple_mem_ref (fold_convert (build_pointer_type (var_type), ptr));
  TREE_THIS_VOLATILE (ref_in) = 1;
  gimplify_assign (acc_in, ref_in, &red_seq);

  tree acc_out = make_ssa_name (var_type);
  tree update_expr = fold_build2 (op, var_type, ref_in, var);
  gimplify_assign (acc_out, update_expr, &red_seq);

  tree ref_out
    = build_simple_mem_ref (fold_convert (build_pointer_type (var_type), ptr));
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
		      tree ptr, tree var, tree_code op,
		      tree array_max_idx = NULL_TREE)
{
  tree type = TREE_TYPE (var);
  tree size = TYPE_SIZE (type);

  if (!VAR_P (ptr))
    {
      tree t = make_ssa_name (TREE_TYPE (ptr));
      gimple_seq seq = NULL;
      gimplify_assign (t, ptr, &seq);
      gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);
      ptr = t;
    }

  if (TREE_CODE (type) == ARRAY_TYPE
      || TREE_CODE (type) == POINTER_TYPE)
    {
      tree array_type;
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  array_type = TREE_TYPE (var);
	}
      else if (TREE_CODE (type) == POINTER_TYPE
	       && TREE_CODE (TREE_TYPE (type)) == ARRAY_TYPE)
	{
	  array_type = TREE_TYPE (TREE_TYPE (var));
	}
      else if (TREE_CODE (type) == POINTER_TYPE)
	{
	  array_type = TREE_TYPE (var);
	}
      else
	gcc_unreachable ();

      tree array_elem_type = TREE_TYPE (array_type);

      gimple *g;
      gimple_seq seq = NULL;
      tree max_index = array_max_idx;
      gcc_assert (array_max_idx);

      tree init_index = make_ssa_name (TREE_TYPE (max_index));
      tree loop_index = make_ssa_name (TREE_TYPE (max_index));
      tree update_index = make_ssa_name (TREE_TYPE (max_index));

      g = gimple_build_assign (init_index,
			       build_int_cst (TREE_TYPE (init_index), 0));
      gimple_seq_add_stmt (&seq, g);
      gimple *init_end = gimple_seq_last (seq);
      gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);

      basic_block init_bb = gsi_bb (*gsi);
      edge init_edge = split_block (init_bb, init_end);
      basic_block loop_bb = init_edge->dest;
      /* Reset the iterator.  */
      *gsi = gsi_for_stmt (gsi_stmt (*gsi));

      seq = NULL;
      g = gimple_build_assign (update_index, PLUS_EXPR, loop_index,
			       build_int_cst (TREE_TYPE (loop_index), 1));
      gimple_seq_add_stmt (&seq, g);

      g = gimple_build_cond (LE_EXPR, update_index, max_index, NULL, NULL);
      gimple_seq_add_stmt (&seq, g);
      gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);

      edge post_edge = split_block (loop_bb, g);
      basic_block post_bb = post_edge->dest;
      loop_bb = post_edge->src;
      /* Reset the iterator.  */
      *gsi = gsi_for_stmt (gsi_stmt (*gsi));

      /* Place where we insert reduction code below.  */
      gimple_stmt_iterator reduction_code_gsi = gsi_start_bb (loop_bb);

      post_edge->flags ^= EDGE_FALSE_VALUE | EDGE_FALLTHRU;
      post_edge->probability = profile_probability::even ();
      edge loop_edge = make_edge (loop_bb, loop_bb, EDGE_TRUE_VALUE);
      loop_edge->probability = profile_probability::even ();
      set_immediate_dominator (CDI_DOMINATORS, loop_bb, init_bb);
      set_immediate_dominator (CDI_DOMINATORS, post_bb, loop_bb);
      class loop *new_loop = alloc_loop ();
      new_loop->header = loop_bb;
      new_loop->latch = loop_bb;
      add_loop (new_loop, loop_bb->loop_father);

      gphi *phi = create_phi_node (loop_index, loop_bb);
      add_phi_arg (phi, init_index, init_edge, loc);
      add_phi_arg (phi, update_index, loop_edge, loc);

      tree var_ptr = fold_convert (build_pointer_type (array_elem_type),
				   var);
      tree idx = fold_build2 (MULT_EXPR, sizetype,
			      fold_convert (sizetype, loop_index),
			      TYPE_SIZE_UNIT (array_elem_type));
      var_ptr = build2 (POINTER_PLUS_EXPR, TREE_TYPE (var_ptr), var_ptr, idx);
      tree var_aref = build_simple_mem_ref (var_ptr);
      ptr = build2 (POINTER_PLUS_EXPR, TREE_TYPE (ptr), ptr, idx);

      gcn_reduction_update (loc, &reduction_code_gsi,
			    ptr, var_aref, op);

      return build_simple_mem_ref (ptr);
    }
  else if (TREE_CODE (type) == RECORD_TYPE)
    {
      for (tree fld = TYPE_FIELDS (type); fld; fld = TREE_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL)
	  {
	    tree var_fld_ref = build3 (COMPONENT_REF, TREE_TYPE (fld),
				       var, fld, NULL);
	    tree ptr_ref = build_simple_mem_ref (ptr);
	    tree ptr_fld_type
	      = build_qualified_type (TREE_TYPE (fld),
				      TYPE_QUALS (TREE_TYPE (ptr_ref)));
	    tree ptr_fld_ref = build3 (COMPONENT_REF, ptr_fld_type,
				       ptr_ref, fld, NULL);

	    if (TREE_CODE (TREE_TYPE (fld)) == ARRAY_TYPE)
	      {
		tree array_elem_ptr_type
		  = build_pointer_type (TREE_TYPE (TREE_TYPE (fld)));
		gcn_reduction_update
		  (loc, gsi,
		   fold_convert (array_elem_ptr_type,
				 build_fold_addr_expr (ptr_fld_ref)),
		   build_fold_addr_expr (var_fld_ref), op,
		   TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (fld))));
	      }
	    else
	      gcn_reduction_update (loc, gsi,
				    build_fold_addr_expr (ptr_fld_ref),
				    var_fld_ref, op);
	  }
      return build_simple_mem_ref (ptr);
    }

  if (size == TYPE_SIZE (unsigned_type_node)
      || size == TYPE_SIZE (long_long_unsigned_type_node))
    return gcn_lockless_update (loc, gsi, ptr, var, op);
  else
    return gcn_lockfull_update (loc, gsi, ptr, var, op);
}

/* Return a temporary variable decl to use for an OpenACC worker reduction.  */

static tree
gcn_goacc_get_worker_red_decl (tree type, tree offset_expr)
{
  machine_function *machfun = cfun->machine;

  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  tree var_type
    = build_qualified_type (type,
			    (TYPE_QUALS (type)
			     | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_LDS)));
  tree addr;
  if (TREE_CONSTANT (offset_expr))
    {
      unsigned offset = TREE_INT_CST_LOW (offset_expr);
      gcc_assert (offset
		  < (machfun->reduction_limit - machfun->reduction_base));
      tree ptr_type = build_pointer_type (var_type);
      addr = build_int_cst (ptr_type, machfun->reduction_base + offset);
    }
  else
    {
      tree ptr_type = build_pointer_type (var_type);
      tree red_base = build_int_cst (ptr_type, machfun->reduction_base);
      addr = build2 (POINTER_PLUS_EXPR, ptr_type,
		     red_base, fold_convert (size_type_node, offset_expr));
    }

  return build_simple_mem_ref (addr);
}

static tree
gcn_goacc_get_worker_array_reduction_buffer (tree array_type,
					     tree array_max_idx,
					     gimple_seq *seq)
{
  gcc_assert (!gcn_array_reduction_buffers.is_empty ());
  tree red_buf_ptr = gcn_array_reduction_buffers.last ();

  tree ptr = make_ssa_name (ptr_type_node);
  gimplify_assign (ptr, red_buf_ptr, seq);

  tree whole_block_ptr;
  if (TREE_CODE (array_type) == ARRAY_TYPE)
    whole_block_ptr = fold_convert (build_pointer_type (array_type), ptr);
  else
    whole_block_ptr = array_type;

  tree arg = build_int_cst (unsigned_type_node, GOMP_DIM_GANG);
  tree gang_id = make_ssa_name (integer_type_node);
  gimple *gang_id_call = gimple_build_call_internal (IFN_GOACC_DIM_POS, 1, arg);
  gimple_call_set_lhs (gang_id_call, gang_id);
  gimple_seq_add_stmt (seq, gang_id_call);

  tree len = fold_build2 (PLUS_EXPR, size_type_node, array_max_idx,
			  size_int (1));
  tree elem_size = TYPE_SIZE_UNIT (TREE_TYPE (array_type));
  tree array_size_expr = build2 (MULT_EXPR, size_type_node, len, elem_size);
  tree type_size = make_ssa_name (size_type_node);
  gimplify_assign (type_size, array_size_expr, seq);

  tree idx = make_ssa_name (size_type_node);
  gimplify_assign (idx, build2 (MULT_EXPR, size_type_node, type_size,
				fold_convert (size_type_node, gang_id)), seq);

  tree addr = fold_convert (ptr_type_node, whole_block_ptr);;
  addr = build2 (POINTER_PLUS_EXPR, ptr_type_node, addr, idx);
  addr = fold_convert (build_pointer_type (array_type), addr);

  tree addr_reg = make_ssa_name (build_pointer_type (array_type));
  gimplify_assign (addr_reg, addr, seq);

  return build_simple_mem_ref (addr_reg);
}

static void
gcn_create_if_else_seq (gimple_stmt_iterator *gsi_p, gimple *split_stmt,
			gimple_seq *then_seq, gimple_seq *else_seq)
{
  basic_block init_bb = gsi_bb (*gsi_p);

  edge fallthru_edge = split_block (init_bb, split_stmt);
  basic_block then_bb = fallthru_edge->dest;

  /* Reset the iterator.  */
  *gsi_p = gsi_for_stmt (gsi_stmt (*gsi_p));

  gimple *then_seq_end = gimple_seq_last (*then_seq);
  gsi_insert_seq_before (gsi_p, *then_seq, GSI_SAME_STMT);

  basic_block last_bb = then_bb;
  gimple *last_seq_end = then_seq_end;

  basic_block else_bb = NULL;
  edge then_else_fallthru_edge = NULL;
  if (else_seq)
    {
      then_else_fallthru_edge = split_block (then_bb, then_seq_end);
      else_bb = then_else_fallthru_edge->dest;

      /* Reset the iterator.  */
      *gsi_p = gsi_for_stmt (gsi_stmt (*gsi_p));

      gimple *else_seq_end = gimple_seq_last (*else_seq);
      gsi_insert_seq_before (gsi_p, *else_seq, GSI_SAME_STMT);

      last_bb = else_bb;
      last_seq_end = else_seq_end;
    }

  edge post_edge = split_block (last_bb, last_seq_end);
  basic_block post_bb = post_edge->dest;

  /* Reset the iterator.  */
  *gsi_p = gsi_for_stmt (gsi_stmt (*gsi_p));

  edge if_true_edge = make_edge (init_bb, (else_seq ? else_bb : post_bb),
				 EDGE_TRUE_VALUE);
  if_true_edge->probability = profile_probability::even ();
  fallthru_edge->flags = EDGE_FALSE_VALUE;
  fallthru_edge->probability = profile_probability::even ();

  post_edge->flags = EDGE_FALLTHRU;
  post_edge->probability = profile_probability::always ();

  set_immediate_dominator (CDI_DOMINATORS, then_bb, init_bb);
  set_immediate_dominator (CDI_DOMINATORS, post_bb, init_bb);

  if (else_seq)
    {
      redirect_edge_and_branch (then_else_fallthru_edge, post_bb);
      set_immediate_dominator (CDI_DOMINATORS, else_bb, init_bb);
    }
}

static void
gcn_create_do_while_loop_seq (gimple_stmt_iterator *gsi_p,
			      gimple_seq *body_seq, int edge_flags)
{
  gimple *g = NULL;
  basic_block init_bb = gsi_bb (*gsi_p);
  edge init_edge = split_block (init_bb, g);
  basic_block loop_bb = init_edge->dest;
  init_bb = init_edge->src;

  /* Reset the iterator.  */
  *gsi_p = gsi_for_stmt (gsi_stmt (*gsi_p));

  gimple_stmt_iterator loop_gsi = gsi_start_bb (loop_bb);

  gimple *body_seq_end = gimple_seq_last (*body_seq);
  gsi_insert_seq_before (&loop_gsi, *body_seq, GSI_SAME_STMT);

  edge post_edge = split_block (loop_bb, body_seq_end);
  basic_block post_bb = post_edge->dest;

  /* Reset the iterator.  */
  *gsi_p = gsi_for_stmt (gsi_stmt (*gsi_p));

  make_edge (loop_bb, loop_bb, edge_flags);
  post_edge->flags = EDGE_FALSE_VALUE;
  set_immediate_dominator (CDI_DOMINATORS, loop_bb, init_bb);
  set_immediate_dominator (CDI_DOMINATORS, post_bb, loop_bb);

  loop *loop = alloc_loop ();
  loop->header = loop_bb;
  loop->latch = loop_bb;
  add_loop (loop, loop_bb->loop_father);
}

/* Expand IFN_GOACC_REDUCTION_SETUP.  */

static void
gcn_goacc_reduction_setup (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));

  tree array_addr = gimple_call_arg (call, 6);
  tree array_max_idx = gimple_call_arg (call, 7);
  bool array_p = !integer_zerop (array_addr);

  tree array_type = NULL_TREE;
  if (array_p)
    array_type
      = (TREE_CODE (TREE_TYPE (array_addr)) == POINTER_TYPE
	 && TREE_CODE (TREE_TYPE (TREE_TYPE (array_addr))) == ARRAY_TYPE
	 ? TREE_TYPE (TREE_TYPE (array_addr))
	 : TREE_TYPE (array_addr));

  gimple_seq seq = NULL;

  push_gimplify_context (true);

  /* Copy the receiver object.  */
  tree ref_to_res = gimple_call_arg (call, 1);

  if (level != GOMP_DIM_GANG)
    {
      if (!integer_zerop (ref_to_res))
	{
	  if (!array_p)
	    var = build_simple_mem_ref (ref_to_res);
	}
    }

  if (array_p && !integer_zerop (ref_to_res))
    {
      gimple_seq condseq = NULL;

      /* Create global variable to store pointer to array reduction buffer.  */
      tree reduction_buffer_ptr_type
	= build_qualified_type (ptr_type_node, TYPE_QUAL_VOLATILE);
      tree reduction_buffer_ptr
	= build_decl (UNKNOWN_LOCATION, VAR_DECL,
		      create_tmp_var_name ("gcn_array_reduction_buf"),
		      reduction_buffer_ptr_type);
      TREE_STATIC (reduction_buffer_ptr) = 1;
      TREE_PUBLIC (reduction_buffer_ptr) = 0;
      DECL_INITIAL (reduction_buffer_ptr) = 0;
      DECL_EXTERNAL (reduction_buffer_ptr) = 0;

      varpool_node::add (reduction_buffer_ptr);

      tree reduction_buffer_ptr_addr = make_ssa_name (ptr_type_node);
      gimplify_assign (reduction_buffer_ptr_addr,
		       build_fold_addr_expr (reduction_buffer_ptr), &condseq);

      tree gang_dim_arg = build_int_cst (unsigned_type_node, GOMP_DIM_GANG);
      tree gang_pos = make_ssa_name (integer_type_node);
      gimple *gang_pos_call = gimple_build_call_internal (IFN_GOACC_DIM_POS,
							  1, gang_dim_arg);
      gimple_call_set_lhs (gang_pos_call, gang_pos);
      gimple_seq_add_stmt (&condseq, gang_pos_call);
      gimple *cond = gimple_build_cond (NE_EXPR, gang_pos, integer_zero_node,
					NULL, NULL);
      gimple_seq_add_stmt (&condseq, cond);
      gimple *cond_end = gimple_seq_last (condseq);
      gsi_insert_seq_before (&gsi, condseq, GSI_SAME_STMT);

      gimple_seq malloc_seq = NULL;
      tree gang_num = make_ssa_name (integer_type_node);
      gimple *gang_num_call = gimple_build_call_internal (IFN_GOACC_DIM_SIZE,
							  1, gang_dim_arg);
      gimple_call_set_lhs (gang_num_call, gang_num);
      gimple_seq_add_stmt (&malloc_seq, gang_num_call);

      tree len = fold_build2 (PLUS_EXPR, size_type_node, array_max_idx,
			      size_int (1));
      tree elem_size = TYPE_SIZE_UNIT (TREE_TYPE (array_type));
      tree malloc_size_expr = build2 (MULT_EXPR, size_type_node, len,
				      elem_size);
      malloc_size_expr = build2 (MULT_EXPR, size_type_node, malloc_size_expr,
				 fold_convert (size_type_node, gang_num));
      tree malloc_size = make_ssa_name (size_type_node);
      gimplify_assign (malloc_size, malloc_size_expr, &malloc_seq);

      tree ptr = make_ssa_name (ptr_type_node);
      tree malloc_decl = builtin_decl_explicit (BUILT_IN_MALLOC);
      gcall *stmt = gimple_build_call (malloc_decl, 1, malloc_size);
      gimple_call_set_lhs (stmt, ptr);
      gimple_seq_add_stmt (&malloc_seq, stmt);

      tree atomic_store_decl = builtin_decl_explicit (BUILT_IN_ATOMIC_STORE_8);
      gcall *atomic_store
	= gimple_build_call (atomic_store_decl, 3, reduction_buffer_ptr_addr,
			     ptr, build_int_cst (integer_type_node,
						 MEMMODEL_RELEASE));
      gimple_seq_add_stmt (&malloc_seq, atomic_store);

      gimple_seq wait_seq = NULL;
      gimple *nop = gimple_build_nop ();
      gimple_seq_add_stmt (&wait_seq, nop);

      gcn_create_if_else_seq (&gsi, cond_end, &malloc_seq, &wait_seq);

      /* Create cmp-swap loop for other gangs to wait for
	 gcn_array_reduction_buf.* to be properly set by gang zero.  */
      gimple_stmt_iterator ngsi = gsi_for_stmt (nop);

      gimple_seq atomic_load_seq = NULL;
      tree loadval = make_ssa_name (size_type_node);
      tree atomic_load_decl = builtin_decl_explicit (BUILT_IN_ATOMIC_LOAD_8);
      gcall *atomic_load
	= gimple_build_call (atomic_load_decl, 2, reduction_buffer_ptr_addr,
			     build_int_cst (integer_type_node,
					    MEMMODEL_ACQUIRE));
      gimple_call_set_lhs (atomic_load, loadval);
      gimple_seq_add_stmt (&atomic_load_seq, atomic_load);
      cond = gimple_build_cond (EQ_EXPR, loadval, size_zero_node,
				NULL_TREE, NULL_TREE);
      gimple_seq_add_stmt (&atomic_load_seq, cond);

      gcn_create_do_while_loop_seq (&ngsi, &atomic_load_seq, EDGE_TRUE_VALUE);
      gcn_array_reduction_buffers.safe_push (reduction_buffer_ptr);
    }

  if (level == GOMP_DIM_WORKER)
    {
      tree offset = gimple_call_arg (call, 5);
      if (array_p)
	{
	  tree decl = gcn_goacc_get_worker_array_reduction_buffer
	    (array_type, array_max_idx, &seq);
	  tree ptr = make_ssa_name (TREE_TYPE (array_addr));
	  gimplify_assign (ptr, build_fold_addr_expr (decl), &seq);

	  /* Store incoming value to worker reduction buffer.  */
	  oacc_build_array_copy (ptr, array_addr, array_max_idx, &seq);
	}
      else
	{
	  tree var_type = TREE_TYPE (var);
	  /* Store incoming value to worker reduction buffer.  */
	  tree decl = gcn_goacc_get_worker_red_decl (var_type, offset);
	  gimplify_assign (decl, var, &seq);
	}
    }

  if (lhs)
    gimplify_assign (lhs, unshare_expr (var), &seq);

  pop_gimplify_context (NULL);

  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
  gsi_remove (&gsi, true);
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

  tree array_addr = gimple_call_arg (call, 6);
  tree array_max_idx = gimple_call_arg (call, 7);
  bool array_p = !integer_zerop (array_addr);

  tree array_type = NULL_TREE;
  if (array_p)
    array_type
      = (TREE_CODE (TREE_TYPE (array_addr)) == POINTER_TYPE
	 && TREE_CODE (TREE_TYPE (TREE_TYPE (array_addr))) == ARRAY_TYPE
	 ? TREE_TYPE (TREE_TYPE (array_addr))
	 : TREE_TYPE (array_addr));

  tree init = NULL_TREE;
  gimple_seq seq = NULL;

  push_gimplify_context (true);

  if (array_p)
    {
      tree loop_index;
      gimple_stmt_iterator loop_body_gsi;
      oacc_build_indexed_ssa_loop (gimple_location (call), array_max_idx, &gsi,
				   &loop_index, &loop_body_gsi);

      tree init_type = TREE_TYPE (array_type);
      init = omp_reduction_init_op (gimple_location (call), rcode,
				    init_type);
      gimple_seq seq = NULL;

      tree ptr = fold_convert (ptr_type_node, array_addr);
      tree offset = build2 (MULT_EXPR, sizetype,
			    fold_convert (sizetype, loop_index),
			    TYPE_SIZE_UNIT (init_type));

      tree addr = build2 (POINTER_PLUS_EXPR, build_pointer_type (init_type),
			  ptr, offset);
      tree ref = build_simple_mem_ref (addr);

      push_gimplify_context (true);
      gimplify_assign (ref, init, &seq);
      pop_gimplify_context (NULL);
      gsi_insert_seq_before (&loop_body_gsi, seq, GSI_SAME_STMT);
      init = var;
    }
  else
    init = omp_reduction_init_op (gimple_location (call), rcode,
				  TREE_TYPE (var));

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

  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
  gsi_remove (&gsi, true);
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

  tree array_addr = gimple_call_arg (call, 6);
  tree array_max_idx = gimple_call_arg (call, 7);
  bool array_p = !integer_zerop (array_addr);

  gimple_seq seq = NULL;
  tree r = NULL_TREE;

  push_gimplify_context (true);

  tree accum = NULL_TREE;

  if (level == GOMP_DIM_WORKER)
    {
      tree offset = gimple_call_arg (call, 5);
      tree decl;
      if (array_p)
	{
	  tree array_type = TREE_TYPE (TREE_TYPE (array_addr));
	  decl = gcn_goacc_get_worker_array_reduction_buffer
	    (array_type, array_max_idx, &seq);
	}
      else
	{
	  tree var_type = TREE_TYPE (var);
	  decl = gcn_goacc_get_worker_red_decl (var_type, offset);
	}
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
      if (array_p)
	{
	  gcn_reduction_update (gimple_location (call), &gsi, accum,
				array_addr, op, array_max_idx);
	  r = var;
	}
      else
	r = gcn_reduction_update (gimple_location (call), &gsi, accum, var, op);
    }

  if (lhs)
    gimplify_assign (lhs, r, &seq);
  pop_gimplify_context (NULL);

  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
  gsi_remove (&gsi, true);
}

/* Expand IFN_GOACC_REDUCTION_TEARDOWN.  */

static void
gcn_goacc_reduction_teardown (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));

  tree array_addr = gimple_call_arg (call, 6);
  tree array_max_idx = gimple_call_arg (call, 7);
  bool array_p = !integer_zerop (array_addr);

  tree array_accum = NULL_TREE;

  gimple_seq seq = NULL;

  push_gimplify_context (true);

  if (level == GOMP_DIM_WORKER)
    {
      tree offset = gimple_call_arg (call, 5);
      if (array_p)
	{
	  tree array_type = TREE_TYPE (TREE_TYPE (array_addr));
	  array_accum = gcn_goacc_get_worker_array_reduction_buffer
	    (array_type, array_max_idx, &seq);
	}
      else
	{
	  tree var_type = TREE_TYPE (var);

	  /* Read the worker reduction buffer.  */
	  tree decl = gcn_goacc_get_worker_red_decl (var_type, offset);
	  var = decl;
	}
    }

  /* Write to the receiver object.  */
  tree ref_to_res = gimple_call_arg (call, 1);
  if (level != GOMP_DIM_GANG)
    {
      if (!integer_zerop (ref_to_res))
	{
	  if (array_p)
	    {
	      tree ptr
		= make_ssa_name (build_pointer_type (TREE_TYPE (array_addr)));
	      gimplify_assign (ptr, build_fold_addr_expr (array_accum), &seq);
	      oacc_build_array_copy (ref_to_res, ptr, array_max_idx, &seq);
	    }
	  else
	    gimplify_assign (build_simple_mem_ref (ref_to_res), var, &seq);
	}
      else if (array_p)
	{
	  tree ptr
	    = make_ssa_name (build_pointer_type (TREE_TYPE (array_accum)));
	  gimplify_assign (ptr, build_fold_addr_expr (array_accum), &seq);
	  oacc_build_array_copy (array_addr, ptr, array_max_idx, &seq);
	}
    }

  if (array_p && !integer_zerop (ref_to_res))
    {
      gimple_seq condseq = NULL;
      tree gang_dim_arg = build_int_cst (unsigned_type_node, GOMP_DIM_GANG);
      tree gang_pos = make_ssa_name (integer_type_node);
      gimple *gang_pos_call = gimple_build_call_internal (IFN_GOACC_DIM_POS,
							  1, gang_dim_arg);
      gimple_call_set_lhs (gang_pos_call, gang_pos);
      gimple_seq_add_stmt (&condseq, gang_pos_call);
      gimple *cond = gimple_build_cond (NE_EXPR, gang_pos, integer_zero_node,
					NULL, NULL);
      gimple_seq_add_stmt (&condseq, cond);
      gimple *cond_end = gimple_seq_last (condseq);
      gsi_insert_seq_before (&gsi, condseq, GSI_SAME_STMT);

      gimple_seq free_seq = NULL;
      gcc_assert (!gcn_array_reduction_buffers.is_empty ());
      tree red_buf_ptr = gcn_array_reduction_buffers.last ();

      tree ptr = make_ssa_name (ptr_type_node);
      gimplify_assign (ptr, red_buf_ptr, &free_seq);

      gcn_array_reduction_buffers.pop ();

      tree free_decl = builtin_decl_explicit (BUILT_IN_FREE);
      gcall *stmt = gimple_build_call (free_decl, 1, ptr);
      gimple_seq_add_stmt (&free_seq, stmt);

      gcn_create_if_else_seq (&gsi, cond_end, &free_seq, NULL);
    }

  if (lhs)
    gimplify_assign (lhs, unshare_expr (var), &seq);

  pop_gimplify_context (NULL);

  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
  gsi_remove (&gsi, true);
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

tree
gcn_goacc_adjust_private_decl (location_t, tree var, int level)
{
  if (level != GOMP_DIM_GANG)
    return var;

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

  return var;
}

/* Implement TARGET_GOACC_CREATE_WORKER_BROADCAST_RECORD.

   Create OpenACC worker state propagation record in shared memory.  */

tree
gcn_goacc_create_worker_broadcast_record (tree record_type, bool sender,
					  const char *name,
					  unsigned HOST_WIDE_INT offset)
{
  tree type = build_qualified_type (record_type,
				    TYPE_QUALS_NO_ADDR_SPACE (record_type)
				    | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_LDS));

  if (!sender)
    {
      tree ptr_type = build_pointer_type (type);
      return create_tmp_var_raw (ptr_type, name);
    }

  if (record_type == char_type_node)
    offset = 1;

  tree ptr_type = build_pointer_type (type);
  return build_int_cst (ptr_type, offset);
}

/* }}}  */
