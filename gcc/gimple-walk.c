/* Gimple walk support.

   Copyright (C) 2007-2013 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>

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
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gimple-walk.h"
#include "demangle.h"

/* Walk all the statements in the sequence *PSEQ calling walk_gimple_stmt
   on each one.  WI is as in walk_gimple_stmt.

   If walk_gimple_stmt returns non-NULL, the walk is stopped, and the
   value is stored in WI->CALLBACK_RESULT.  Also, the statement that
   produced the value is returned if this statement has not been
   removed by a callback (wi->removed_stmt).  If the statement has
   been removed, NULL is returned.

   Otherwise, all the statements are walked and NULL returned.  */

gimple
walk_gimple_seq_mod (gimple_seq *pseq, walk_stmt_fn callback_stmt,
		     walk_tree_fn callback_op, struct walk_stmt_info *wi)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (*pseq); !gsi_end_p (gsi); )
    {
      tree ret = walk_gimple_stmt (&gsi, callback_stmt, callback_op, wi);
      if (ret)
	{
	  /* If CALLBACK_STMT or CALLBACK_OP return a value, WI must exist
	     to hold it.  */
	  gcc_assert (wi);
	  wi->callback_result = ret;

	  return wi->removed_stmt ? NULL : gsi_stmt (gsi);
	}

      if (!wi->removed_stmt)
	gsi_next (&gsi);
    }

  if (wi)
    wi->callback_result = NULL_TREE;

  return NULL;
}


/* Like walk_gimple_seq_mod, but ensure that the head of SEQ isn't
   changed by the callbacks.  */

gimple
walk_gimple_seq (gimple_seq seq, walk_stmt_fn callback_stmt,
		 walk_tree_fn callback_op, struct walk_stmt_info *wi)
{
  gimple_seq seq2 = seq;
  gimple ret = walk_gimple_seq_mod (&seq2, callback_stmt, callback_op, wi);
  gcc_assert (seq2 == seq);
  return ret;
}


/* Helper function for walk_gimple_stmt.  Walk operands of a GIMPLE_ASM.  */

static tree
walk_gimple_asm (gimple stmt, walk_tree_fn callback_op,
		 struct walk_stmt_info *wi)
{
  tree ret, op;
  unsigned noutputs;
  const char **oconstraints;
  unsigned i, n;
  const char *constraint;
  bool allows_mem, allows_reg, is_inout;

  noutputs = gimple_asm_noutputs (stmt);
  oconstraints = (const char **) alloca ((noutputs) * sizeof (const char *));

  if (wi)
    wi->is_lhs = true;

  for (i = 0; i < noutputs; i++)
    {
      op = gimple_asm_output_op (stmt, i);
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (op)));
      oconstraints[i] = constraint;
      parse_output_constraint (&constraint, i, 0, 0, &allows_mem, &allows_reg,
	                       &is_inout);
      if (wi)
	wi->val_only = (allows_reg || !allows_mem);
      ret = walk_tree (&TREE_VALUE (op), callback_op, wi, NULL);
      if (ret)
	return ret;
    }

  n = gimple_asm_ninputs (stmt);
  for (i = 0; i < n; i++)
    {
      op = gimple_asm_input_op (stmt, i);
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (op)));
      parse_input_constraint (&constraint, 0, 0, noutputs, 0,
			      oconstraints, &allows_mem, &allows_reg);
      if (wi)
	{
	  wi->val_only = (allows_reg || !allows_mem);
          /* Although input "m" is not really a LHS, we need a lvalue.  */
	  wi->is_lhs = !wi->val_only;
	}
      ret = walk_tree (&TREE_VALUE (op), callback_op, wi, NULL);
      if (ret)
	return ret;
    }

  if (wi)
    {
      wi->is_lhs = false;
      wi->val_only = true;
    }

  n = gimple_asm_nlabels (stmt);
  for (i = 0; i < n; i++)
    {
      op = gimple_asm_label_op (stmt, i);
      ret = walk_tree (&TREE_VALUE (op), callback_op, wi, NULL);
      if (ret)
	return ret;
    }

  return NULL_TREE;
}


/* Helper function of WALK_GIMPLE_STMT.  Walk every tree operand in
   STMT.  CALLBACK_OP and WI are as in WALK_GIMPLE_STMT.

   CALLBACK_OP is called on each operand of STMT via walk_tree.
   Additional parameters to walk_tree must be stored in WI.  For each operand
   OP, walk_tree is called as:

	walk_tree (&OP, CALLBACK_OP, WI, WI->PSET)

   If CALLBACK_OP returns non-NULL for an operand, the remaining
   operands are not scanned.

   The return value is that returned by the last call to walk_tree, or
   NULL_TREE if no CALLBACK_OP is specified.  */

tree
walk_gimple_op (gimple stmt, walk_tree_fn callback_op,
		struct walk_stmt_info *wi)
{
  struct pointer_set_t *pset = (wi) ? wi->pset : NULL;
  unsigned i;
  tree ret = NULL_TREE;

  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      /* Walk the RHS operands.  If the LHS is of a non-renamable type or
         is a register variable, we may use a COMPONENT_REF on the RHS.  */
      if (wi)
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  wi->val_only
	    = (is_gimple_reg_type (TREE_TYPE (lhs)) && !is_gimple_reg (lhs))
	      || gimple_assign_rhs_class (stmt) != GIMPLE_SINGLE_RHS;
	}

      for (i = 1; i < gimple_num_ops (stmt); i++)
	{
	  ret = walk_tree (gimple_op_ptr (stmt, i), callback_op, wi,
			   pset);
	  if (ret)
	    return ret;
	}

      /* Walk the LHS.  If the RHS is appropriate for a memory, we
	 may use a COMPONENT_REF on the LHS.  */
      if (wi)
	{
          /* If the RHS is of a non-renamable type or is a register variable,
	     we may use a COMPONENT_REF on the LHS.  */
	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  wi->val_only
	    = (is_gimple_reg_type (TREE_TYPE (rhs1)) && !is_gimple_reg (rhs1))
	      || gimple_assign_rhs_class (stmt) != GIMPLE_SINGLE_RHS;
	  wi->is_lhs = true;
	}

      ret = walk_tree (gimple_op_ptr (stmt, 0), callback_op, wi, pset);
      if (ret)
	return ret;

      if (wi)
	{
	  wi->val_only = true;
	  wi->is_lhs = false;
	}
      break;

    case GIMPLE_CALL:
      if (wi)
	{
	  wi->is_lhs = false;
	  wi->val_only = true;
	}

      ret = walk_tree (gimple_call_chain_ptr (stmt), callback_op, wi, pset);
      if (ret)
        return ret;

      ret = walk_tree (gimple_call_fn_ptr (stmt), callback_op, wi, pset);
      if (ret)
        return ret;

      for (i = 0; i < gimple_call_num_args (stmt); i++)
	{
	  if (wi)
	    wi->val_only
	      = is_gimple_reg_type (TREE_TYPE (gimple_call_arg (stmt, i)));
	  ret = walk_tree (gimple_call_arg_ptr (stmt, i), callback_op, wi,
			   pset);
	  if (ret)
	    return ret;
	}

      if (gimple_call_lhs (stmt))
	{
	  if (wi)
	    {
	      wi->is_lhs = true;
	      wi->val_only
		= is_gimple_reg_type (TREE_TYPE (gimple_call_lhs (stmt)));
	    }

	  ret = walk_tree (gimple_call_lhs_ptr (stmt), callback_op, wi, pset);
	  if (ret)
	    return ret;
	}

      if (wi)
	{
	  wi->is_lhs = false;
	  wi->val_only = true;
	}
      break;

    case GIMPLE_CATCH:
      ret = walk_tree (gimple_catch_types_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_EH_FILTER:
      ret = walk_tree (gimple_eh_filter_types_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_ASM:
      ret = walk_gimple_asm (stmt, callback_op, wi);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_CONTINUE:
      ret = walk_tree (gimple_omp_continue_control_def_ptr (stmt),
	  	       callback_op, wi, pset);
      if (ret)
	return ret;

      ret = walk_tree (gimple_omp_continue_control_use_ptr (stmt),
	  	       callback_op, wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_CRITICAL:
      ret = walk_tree (gimple_omp_critical_name_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_FOR:
      ret = walk_tree (gimple_omp_for_clauses_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
	{
	  ret = walk_tree (gimple_omp_for_index_ptr (stmt, i), callback_op,
			   wi, pset);
	  if (ret)
	    return ret;
	  ret = walk_tree (gimple_omp_for_initial_ptr (stmt, i), callback_op,
			   wi, pset);
	  if (ret)
	    return ret;
	  ret = walk_tree (gimple_omp_for_final_ptr (stmt, i), callback_op,
			   wi, pset);
	  if (ret)
	    return ret;
	  ret = walk_tree (gimple_omp_for_incr_ptr (stmt, i), callback_op,
			   wi, pset);
	}
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_PARALLEL:
      ret = walk_tree (gimple_omp_parallel_clauses_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_parallel_child_fn_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_parallel_data_arg_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_TASK:
      ret = walk_tree (gimple_omp_task_clauses_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_child_fn_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_data_arg_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_copy_fn_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_arg_size_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      ret = walk_tree (gimple_omp_task_arg_align_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_SECTIONS:
      ret = walk_tree (gimple_omp_sections_clauses_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;

      ret = walk_tree (gimple_omp_sections_control_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;

      break;

    case GIMPLE_OMP_SINGLE:
      ret = walk_tree (gimple_omp_single_clauses_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_TARGET:
      ret = walk_tree (gimple_omp_target_clauses_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_TEAMS:
      ret = walk_tree (gimple_omp_teams_clauses_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_ATOMIC_LOAD:
      ret = walk_tree (gimple_omp_atomic_load_lhs_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;

      ret = walk_tree (gimple_omp_atomic_load_rhs_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_ATOMIC_STORE:
      ret = walk_tree (gimple_omp_atomic_store_val_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_TRANSACTION:
      ret = walk_tree (gimple_transaction_label_ptr (stmt), callback_op,
		       wi, pset);
      if (ret)
	return ret;
      break;

    case GIMPLE_OMP_RETURN:
      ret = walk_tree (gimple_omp_return_lhs_ptr (stmt), callback_op, wi,
		       pset);
      if (ret)
	return ret;
      break;

      /* Tuples that do not have operands.  */
    case GIMPLE_NOP:
    case GIMPLE_RESX:
    case GIMPLE_PREDICT:
      break;

    default:
      {
	enum gimple_statement_structure_enum gss;
	gss = gimple_statement_structure (stmt);
	if (gss == GSS_WITH_OPS || gss == GSS_WITH_MEM_OPS)
	  for (i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      ret = walk_tree (gimple_op_ptr (stmt, i), callback_op, wi, pset);
	      if (ret)
		return ret;
	    }
      }
      break;
    }

  return NULL_TREE;
}


/* Walk the current statement in GSI (optionally using traversal state
   stored in WI).  If WI is NULL, no state is kept during traversal.
   The callback CALLBACK_STMT is called.  If CALLBACK_STMT indicates
   that it has handled all the operands of the statement, its return
   value is returned.  Otherwise, the return value from CALLBACK_STMT
   is discarded and its operands are scanned.

   If CALLBACK_STMT is NULL or it didn't handle the operands,
   CALLBACK_OP is called on each operand of the statement via
   walk_gimple_op.  If walk_gimple_op returns non-NULL for any
   operand, the remaining operands are not scanned.  In this case, the
   return value from CALLBACK_OP is returned.

   In any other case, NULL_TREE is returned.  */

tree
walk_gimple_stmt (gimple_stmt_iterator *gsi, walk_stmt_fn callback_stmt,
		  walk_tree_fn callback_op, struct walk_stmt_info *wi)
{
  gimple ret;
  tree tree_ret;
  gimple stmt = gsi_stmt (*gsi);

  if (wi)
    {
      wi->gsi = *gsi;
      wi->removed_stmt = false;

      if (wi->want_locations && gimple_has_location (stmt))
	input_location = gimple_location (stmt);
    }

  ret = NULL;

  /* Invoke the statement callback.  Return if the callback handled
     all of STMT operands by itself.  */
  if (callback_stmt)
    {
      bool handled_ops = false;
      tree_ret = callback_stmt (gsi, &handled_ops, wi);
      if (handled_ops)
	return tree_ret;

      /* If CALLBACK_STMT did not handle operands, it should not have
	 a value to return.  */
      gcc_assert (tree_ret == NULL);

      if (wi && wi->removed_stmt)
	return NULL;

      /* Re-read stmt in case the callback changed it.  */
      stmt = gsi_stmt (*gsi);
    }

  /* If CALLBACK_OP is defined, invoke it on every operand of STMT.  */
  if (callback_op)
    {
      tree_ret = walk_gimple_op (stmt, callback_op, wi);
      if (tree_ret)
	return tree_ret;
    }

  /* If STMT can have statements inside (e.g. GIMPLE_BIND), walk them.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      ret = walk_gimple_seq_mod (gimple_bind_body_ptr (stmt), callback_stmt,
				 callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_CATCH:
      ret = walk_gimple_seq_mod (gimple_catch_handler_ptr (stmt), callback_stmt,
				 callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_EH_FILTER:
      ret = walk_gimple_seq_mod (gimple_eh_filter_failure_ptr (stmt), callback_stmt,
		             callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_EH_ELSE:
      ret = walk_gimple_seq_mod (gimple_eh_else_n_body_ptr (stmt),
			     callback_stmt, callback_op, wi);
      if (ret)
	return wi->callback_result;
      ret = walk_gimple_seq_mod (gimple_eh_else_e_body_ptr (stmt),
			     callback_stmt, callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_TRY:
      ret = walk_gimple_seq_mod (gimple_try_eval_ptr (stmt), callback_stmt, callback_op,
	                     wi);
      if (ret)
	return wi->callback_result;

      ret = walk_gimple_seq_mod (gimple_try_cleanup_ptr (stmt), callback_stmt,
	                     callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_OMP_FOR:
      ret = walk_gimple_seq_mod (gimple_omp_for_pre_body_ptr (stmt), callback_stmt,
		             callback_op, wi);
      if (ret)
	return wi->callback_result;

      /* FALL THROUGH.  */
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
      ret = walk_gimple_seq_mod (gimple_omp_body_ptr (stmt), callback_stmt,
			     callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_WITH_CLEANUP_EXPR:
      ret = walk_gimple_seq_mod (gimple_wce_cleanup_ptr (stmt), callback_stmt,
			     callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    case GIMPLE_TRANSACTION:
      ret = walk_gimple_seq_mod (gimple_transaction_body_ptr (stmt),
			     callback_stmt, callback_op, wi);
      if (ret)
	return wi->callback_result;
      break;

    default:
      gcc_assert (!gimple_has_substatements (stmt));
      break;
    }

  return NULL;
}

/* From a tree operand OP return the base of a load or store operation
   or NULL_TREE if OP is not a load or a store.  */

static tree
get_base_loadstore (tree op)
{
  while (handled_component_p (op))
    op = TREE_OPERAND (op, 0);
  if (DECL_P (op)
      || INDIRECT_REF_P (op)
      || TREE_CODE (op) == MEM_REF
      || TREE_CODE (op) == TARGET_MEM_REF)
    return op;
  return NULL_TREE;
}


/* For the statement STMT call the callbacks VISIT_LOAD, VISIT_STORE and
   VISIT_ADDR if non-NULL on loads, store and address-taken operands
   passing the STMT, the base of the operand and DATA to it.  The base
   will be either a decl, an indirect reference (including TARGET_MEM_REF)
   or the argument of an address expression.
   Returns the results of these callbacks or'ed.  */

bool
walk_stmt_load_store_addr_ops (gimple stmt, void *data,
			       bool (*visit_load)(gimple, tree, void *),
			       bool (*visit_store)(gimple, tree, void *),
			       bool (*visit_addr)(gimple, tree, void *))
{
  bool ret = false;
  unsigned i;
  if (gimple_assign_single_p (stmt))
    {
      tree lhs, rhs;
      if (visit_store)
	{
	  lhs = get_base_loadstore (gimple_assign_lhs (stmt));
	  if (lhs)
	    ret |= visit_store (stmt, lhs, data);
	}
      rhs = gimple_assign_rhs1 (stmt);
      while (handled_component_p (rhs))
	rhs = TREE_OPERAND (rhs, 0);
      if (visit_addr)
	{
	  if (TREE_CODE (rhs) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (rhs, 0), data);
	  else if (TREE_CODE (rhs) == TARGET_MEM_REF
		   && TREE_CODE (TMR_BASE (rhs)) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (TMR_BASE (rhs), 0), data);
	  else if (TREE_CODE (rhs) == OBJ_TYPE_REF
		   && TREE_CODE (OBJ_TYPE_REF_OBJECT (rhs)) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (OBJ_TYPE_REF_OBJECT (rhs),
						   0), data);
	  else if (TREE_CODE (rhs) == CONSTRUCTOR)
	    {
	      unsigned int ix;
	      tree val;

	      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), ix, val)
		if (TREE_CODE (val) == ADDR_EXPR)
		  ret |= visit_addr (stmt, TREE_OPERAND (val, 0), data);
		else if (TREE_CODE (val) == OBJ_TYPE_REF
			 && TREE_CODE (OBJ_TYPE_REF_OBJECT (val)) == ADDR_EXPR)
		  ret |= visit_addr (stmt,
				     TREE_OPERAND (OBJ_TYPE_REF_OBJECT (val),
						   0), data);
	    }
          lhs = gimple_assign_lhs (stmt);
	  if (TREE_CODE (lhs) == TARGET_MEM_REF
              && TREE_CODE (TMR_BASE (lhs)) == ADDR_EXPR)
            ret |= visit_addr (stmt, TREE_OPERAND (TMR_BASE (lhs), 0), data);
	}
      if (visit_load)
	{
	  rhs = get_base_loadstore (rhs);
	  if (rhs)
	    ret |= visit_load (stmt, rhs, data);
	}
    }
  else if (visit_addr
	   && (is_gimple_assign (stmt)
	       || gimple_code (stmt) == GIMPLE_COND))
    {
      for (i = 0; i < gimple_num_ops (stmt); ++i)
	{
	  tree op = gimple_op (stmt, i);
	  if (op == NULL_TREE)
	    ;
	  else if (TREE_CODE (op) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
	  /* COND_EXPR and VCOND_EXPR rhs1 argument is a comparison
	     tree with two operands.  */
	  else if (i == 1 && COMPARISON_CLASS_P (op))
	    {
	      if (TREE_CODE (TREE_OPERAND (op, 0)) == ADDR_EXPR)
		ret |= visit_addr (stmt, TREE_OPERAND (TREE_OPERAND (op, 0),
						       0), data);
	      if (TREE_CODE (TREE_OPERAND (op, 1)) == ADDR_EXPR)
		ret |= visit_addr (stmt, TREE_OPERAND (TREE_OPERAND (op, 1),
						       0), data);
	    }
	}
    }
  else if (is_gimple_call (stmt))
    {
      if (visit_store)
	{
	  tree lhs = gimple_call_lhs (stmt);
	  if (lhs)
	    {
	      lhs = get_base_loadstore (lhs);
	      if (lhs)
		ret |= visit_store (stmt, lhs, data);
	    }
	}
      if (visit_load || visit_addr)
	for (i = 0; i < gimple_call_num_args (stmt); ++i)
	  {
	    tree rhs = gimple_call_arg (stmt, i);
	    if (visit_addr
		&& TREE_CODE (rhs) == ADDR_EXPR)
	      ret |= visit_addr (stmt, TREE_OPERAND (rhs, 0), data);
	    else if (visit_load)
	      {
		rhs = get_base_loadstore (rhs);
		if (rhs)
		  ret |= visit_load (stmt, rhs, data);
	      }
	  }
      if (visit_addr
	  && gimple_call_chain (stmt)
	  && TREE_CODE (gimple_call_chain (stmt)) == ADDR_EXPR)
	ret |= visit_addr (stmt, TREE_OPERAND (gimple_call_chain (stmt), 0),
			   data);
      if (visit_addr
	  && gimple_call_return_slot_opt_p (stmt)
	  && gimple_call_lhs (stmt) != NULL_TREE
	  && TREE_ADDRESSABLE (TREE_TYPE (gimple_call_lhs (stmt))))
	ret |= visit_addr (stmt, gimple_call_lhs (stmt), data);
    }
  else if (gimple_code (stmt) == GIMPLE_ASM)
    {
      unsigned noutputs;
      const char *constraint;
      const char **oconstraints;
      bool allows_mem, allows_reg, is_inout;
      noutputs = gimple_asm_noutputs (stmt);
      oconstraints = XALLOCAVEC (const char *, noutputs);
      if (visit_store || visit_addr)
	for (i = 0; i < gimple_asm_noutputs (stmt); ++i)
	  {
	    tree link = gimple_asm_output_op (stmt, i);
	    tree op = get_base_loadstore (TREE_VALUE (link));
	    if (op && visit_store)
	      ret |= visit_store (stmt, op, data);
	    if (visit_addr)
	      {
		constraint = TREE_STRING_POINTER
		    (TREE_VALUE (TREE_PURPOSE (link)));
		oconstraints[i] = constraint;
		parse_output_constraint (&constraint, i, 0, 0, &allows_mem,
					 &allows_reg, &is_inout);
		if (op && !allows_reg && allows_mem)
		  ret |= visit_addr (stmt, op, data);
	      }
	  }
      if (visit_load || visit_addr)
	for (i = 0; i < gimple_asm_ninputs (stmt); ++i)
	  {
	    tree link = gimple_asm_input_op (stmt, i);
	    tree op = TREE_VALUE (link);
	    if (visit_addr
		&& TREE_CODE (op) == ADDR_EXPR)
	      ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
	    else if (visit_load || visit_addr)
	      {
		op = get_base_loadstore (op);
		if (op)
		  {
		    if (visit_load)
		      ret |= visit_load (stmt, op, data);
		    if (visit_addr)
		      {
			constraint = TREE_STRING_POINTER
			    (TREE_VALUE (TREE_PURPOSE (link)));
			parse_input_constraint (&constraint, 0, 0, noutputs,
						0, oconstraints,
						&allows_mem, &allows_reg);
			if (!allows_reg && allows_mem)
			  ret |= visit_addr (stmt, op, data);
		      }
		  }
	      }
	  }
    }
  else if (gimple_code (stmt) == GIMPLE_RETURN)
    {
      tree op = gimple_return_retval (stmt);
      if (op)
	{
	  if (visit_addr
	      && TREE_CODE (op) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
	  else if (visit_load)
	    {
	      op = get_base_loadstore (op);
	      if (op)
		ret |= visit_load (stmt, op, data);
	    }
	}
    }
  else if (visit_addr
	   && gimple_code (stmt) == GIMPLE_PHI)
    {
      for (i = 0; i < gimple_phi_num_args (stmt); ++i)
	{
	  tree op = gimple_phi_arg_def (stmt, i);
	  if (TREE_CODE (op) == ADDR_EXPR)
	    ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
	}
    }
  else if (visit_addr
	   && gimple_code (stmt) == GIMPLE_GOTO)
    {
      tree op = gimple_goto_dest (stmt);
      if (TREE_CODE (op) == ADDR_EXPR)
	ret |= visit_addr (stmt, TREE_OPERAND (op, 0), data);
    }

  return ret;
}

/* Like walk_stmt_load_store_addr_ops but with NULL visit_addr.  IPA-CP
   should make a faster clone for this case.  */

bool
walk_stmt_load_store_ops (gimple stmt, void *data,
			  bool (*visit_load)(gimple, tree, void *),
			  bool (*visit_store)(gimple, tree, void *))
{
  return walk_stmt_load_store_addr_ops (stmt, data,
					visit_load, visit_store, NULL);
}
