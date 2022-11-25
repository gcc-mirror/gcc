/*
   Execute symbolically all paths of the function.  Iterate loops only once․
   Copyright (C) 2006-2022 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.   */

#include "symb-execute-all-paths.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "cfgloop.h"
#include "gimple-range.h"
#include "tree-scalar-evolution.h"
#include "hwint.h"
#include "gimple-pretty-print.h"
#include "function.h"
#include "cfganal.h"

/* This function assigns symbolic values to the arguments of the fun.
   (Not complete).  */
void
crc_symb_execution::make_symbolic_func_args_and_sizes (function *fun,
						       state *initial_state)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Making symbolic function's following arguments:\n");
  /* Get size and name of function's arguments.  */
  for (tree arg = DECL_ARGUMENTS (fun->decl); arg; arg = DECL_CHAIN (arg))
    {
      /* If the argument has a name and the size is integer
	 print that information.  */
      if (TREE_CODE (DECL_SIZE (arg)) == INTEGER_CST && DECL_NAME (arg))
	{
	  unsigned HOST_WIDE_INT size = tree_to_uhwi (DECL_SIZE (arg));
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "%s : %lu; ",
		     IDENTIFIER_POINTER (DECL_NAME (arg)), size);
	  /* Add argument with its size to the state
	     and assign symbolic value.  */
	  initial_state->make_symbolic (arg, size);
	}
      else if (dump_file)
	fprintf (dump_file, "Argument not const or no name.\n");
    }
}

/* Add declared ssa variables to the state.  */
void
crc_symb_execution::add_function_local_ssa_vars (function *fun,
						 state *initial_state)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nAdding following ssa name declarations: \n");
  unsigned ix;
  tree name;
  /* Get ssa names of the function.
     Check type, add to the state with a size length array value.  */
  FOR_EACH_SSA_NAME (ix, name, fun)
      {
	if (TREE_CODE (TREE_TYPE (name)) == INTEGER_TYPE)
	  {
	    if (TYPE_UNSIGNED (TREE_TYPE (name)))
	      {
		// We need this info for symb execution.
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file,
			   "Unsigned, ");
	      }
	  }
	else if (TREE_CODE (TREE_TYPE (name)) == POINTER_TYPE)
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Pointer type, ");
	  }
	else
	  {
	    /* Other type of variables aren't needed for CRC calculation.  */
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		print_generic_expr (dump_file, name, dump_flags);
		fprintf (dump_file, ", %s type, won't be considered.\n",
			 get_tree_code_name (TREE_CODE (TREE_TYPE (name))));
	      }
	    continue;
	  }

	unsigned HOST_WIDE_INT size
	= tree_to_uhwi (TYPE_SIZE (TREE_TYPE (name)));

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    print_generic_expr (dump_file, name, dump_flags);
	    fprintf (dump_file, " size is %lu.\n", size);
	  }

	/* Add ssa variable with its size to the state,
	   assign symbolic value.  */
	initial_state->make_symbolic (name, size);
      }
}

/* Calculate value of the rhs operation and assign to lhs variable.  */
void
crc_symb_execution::execute_assign_statement (const gassign *gs)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (gs);
  tree lhs = gimple_assign_lhs (gs);
  state *current_state = states.last ();

  if (gimple_num_ops (gs) == 2)
    {
      tree op1 = gimple_assign_rhs1 (gs);
      switch (rhs_code)
	{
	  case BIT_NOT_EXPR:
	    current_state->do_complement (op1, lhs);
	    return;
	  case MEM_REF:
	    // do_mem_ref
	    return;
	  case NOP_EXPR:
	    current_state->do_assign (op1, lhs);
	    return;
	  default:
	    if (dump_file)
	      fprintf (dump_file,
		       "Warning, encountered unsupported unary operation "
		       "with %s code while executing assign statement!\n",
		       get_tree_code_name (rhs_code));
	    return;
	}
    }
  else if (gimple_num_ops (gs) == 3)
    {
      tree op1 = gimple_assign_rhs1 (gs);
      tree op2 = gimple_assign_rhs2 (gs);
      switch (rhs_code)
	{
	  case LSHIFT_EXPR:
	    current_state->do_shift_left (op1, op2, lhs);
	    return;
	  case RSHIFT_EXPR:
	    current_state->do_shift_right (op1, op2, lhs);
	    return;
	  case BIT_AND_EXPR:
	    current_state->do_and (op1, op2, lhs);
	    return;
	  case BIT_IOR_EXPR:
	    current_state->do_or (op1, op2, lhs);
	    return;
	  case BIT_XOR_EXPR:
	    current_state->do_xor (op1, op2, lhs);
	    return;
	  case PLUS_EXPR:
	    current_state->do_add (op1, op2, lhs);
	    return;
	  case MINUS_EXPR:
	    current_state->do_sub (op1, op2, lhs);
	    return;
	  case MULT_EXPR:
	    current_state->do_mul (op1, op2, lhs);
	    return;
	  case POINTER_PLUS_EXPR:
	    current_state->do_pointer_plus (op1, op2, lhs);
	    return;
	  case POINTER_DIFF_EXPR:
	    current_state->do_pointer_diff (op1, op2, lhs);
	    return;
	  default:
	    if (dump_file)
	      fprintf (dump_file,
		       "Warning, encountered unsupported binary operation "
		       "with %s code while executing assign statement!\n",
		       get_tree_code_name (rhs_code));
	    return;
	}
    }
  else
    {
      if (dump_file)
	fprintf (dump_file,
		 "Warning, encountered unsupported operation, "
		 "with %s code while executing assign statement!\n",
		 get_tree_code_name (rhs_code));
    }
}

/* Create new state for true and false branch.
   Keep conditions in new created states.  */
void
crc_symb_execution::resolve_condition (const gcond* cond)
{
  /* Remove last state.  */
  state* old_state = states.last ();

  /* Add new states for each branch.  */
  state* true_branch_state = new state (*old_state);
  state* false_branch_state = new state (*old_state);

  delete old_state;
  states.pop ();

  /* First insert false_branch_state then true_branch_state,
     as at first we will examine true branch's basic block, then false branch's,
     and state.last () is called to get current paths state.  */
  states.quick_push (false_branch_state);
  states.quick_push (true_branch_state);

  /* Keep conditions of each branch execution in its state.
     Ex.
       if (a == 0)

       true_branch_state.keep (a==0)
       false_branch_state.keep (a!=0)
  */

  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);
  switch (gimple_cond_code (cond))
    {
      case EQ_EXPR:
	true_branch_state->add_equal_cond (lhs, rhs);
	false_branch_state->add_not_equal_cond (lhs, rhs);
	break;
      case NE_EXPR:
	true_branch_state->add_not_equal_cond (lhs, rhs);
	false_branch_state->add_equal_cond (lhs, rhs);
	break;
      case GT_EXPR:
	true_branch_state->add_greater_than_cond (lhs, rhs);
	false_branch_state->add_less_or_equal_cond (lhs, rhs);
	break;
      case LT_EXPR:
	true_branch_state->add_less_than_cond (lhs, rhs);
	false_branch_state->add_greater_or_equal_cond (lhs, rhs);
	break;
      case GE_EXPR:
	true_branch_state->add_greater_or_equal_cond (lhs, rhs);
	false_branch_state->add_less_than_cond (lhs, rhs);
	break;
      case LE_EXPR:
	true_branch_state->add_less_or_equal_cond (lhs, rhs);
	false_branch_state->add_greater_than_cond (lhs, rhs);
	break;
      default:
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Unsupported condition.\n");
    }
}

/* Keep the calculated value of the return value
   and the conditions of the executed path.  */
void
crc_symb_execution::keep_return_val_and_conditions (const greturn* ret)
{
  tree return_op = gimple_return_retval (ret);

  if (return_op == nullptr)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "No return value.\n");
      return;
    }

  /* Get calculated return value.  */
  state * curr_state = states.last ();

  state * final_state = new state;
  /* Keep return value's calculated value and conditions in the final state.  */
  final_state->add_var_state (return_op, curr_state->get_bits (return_op));
  final_state->bulk_add_conditions (curr_state->get_conditions ());
  final_states.quick_push (final_state);
}


/* Execute gimple statements of BB.
   Keeping values of variables in the state.  */
void
crc_symb_execution::execute_bb_gimple_statements (basic_block bb)
{
  for (gimple_stmt_iterator bsi = gsi_start_bb (bb);
       !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple *gs = gsi_stmt (bsi);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Executing ");
	  print_gimple_stmt (dump_file, gs, dump_flags);
	}
      switch (gimple_code (gs))
	{
	  case GIMPLE_ASSIGN:
	    execute_assign_statement (as_a<const gassign *> (gs));
	    break;
	  case GIMPLE_COND:
	    resolve_condition (as_a<const gcond *> (gs));
	    break;
	  case GIMPLE_RETURN:
	    keep_return_val_and_conditions (as_a<const greturn *> (gs));
	    break;
	  default:
	    if (dump_file)
	      fprintf (dump_file,
		       "Warning, encountered unsupported statement, "
		       "while executing gimple statements!\n");
	    break;
	}
    }
}

/* Assign values of phi instruction to its result.
   Keep updated values in the state.  */
void
crc_symb_execution::execute_bb_phi_statements (basic_block bb,
					       edge incoming_edge)
{
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);

      /* Don't consider virtual operands.  */
      if (virtual_operand_p (lhs))
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Determining the value "
			      "for the following phi.\n");
	  print_gimple_stmt (dump_file, phi, dump_flags);
	}

      tree rhs = PHI_ARG_DEF_FROM_EDGE (phi, incoming_edge);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Found phi's value.\n\n");
	}
      state *current_state = states.last ();
      current_state->do_assign (rhs, lhs);
    }
}

/* Execute all statements of BB.
   Keeping values of variables in the state.  */
void
crc_symb_execution::execute_bb_statements (basic_block bb,
					   edge incoming_edge)
{
  execute_bb_phi_statements (bb, incoming_edge);
  execute_bb_gimple_statements (bb);
}

/* Traverse function fun's all paths from the first basic block to the last.
   Each time iterate loops only once.
   Symbolically execute statements of each path.  */
void
crc_symb_execution::traverse_function (function *fun)
{
  /* TODO: Check whether back_edges can be determined by BB index,
       if so, no need of EDGE_DFS_BACK flag.  */
  mark_dfs_back_edges (fun);
  /* Allocate stack for back-tracking up CFG.  */
  auto_vec<edge, 20> stack (n_basic_blocks_for_fn (fun) + 1);

  /* Push all successor edges of first block into the stack.
     No need to execute first block.  */
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (fun)->succs)
    stack.quick_push (e);

  while (!stack.is_empty ())
    {
      /* Look at the edge on the top of the stack.  */
      edge e = stack.last ();
      stack.pop ();

      /* Get dest block of the edge.  */
      basic_block bb = e->dest;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\n\nExecuting BB <%d>\n\n", bb->index);

      /* Symbolically execute statements.  */
      execute_bb_statements (bb, e);

      /* Add each outgoing edge of the current block to the stack,
	despite back edges.  */
      edge out_edge;
      edge_iterator ei;
      FOR_EACH_EDGE (out_edge, ei, bb->succs)
	if (!(out_edge->flags & EDGE_DFS_BACK)
	    && out_edge->dest != EXIT_BLOCK_PTR_FOR_FN (fun))
	  stack.quick_push (out_edge);
	else if (!states.is_empty ())
	  {
	    /* Delete the state after executing the full path,
	       or encountering back edge.  */
	    delete states.last ();
	    states.pop ();
	  }
    }
}

void
crc_symb_execution::execute_function (function *fun)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nExecuting CRC-like function.\n");

  /* Create initial state and push into the vector of states.  */
  states.quick_push (new state);
  state *initial_state = states.last ();

  make_symbolic_func_args_and_sizes (fun, initial_state);

  /* Add declared variables to the state.  */
  add_function_local_ssa_vars (fun, initial_state);

  /* Execute function's statements, keeping a state for each path.  */
  traverse_function (fun);
}
