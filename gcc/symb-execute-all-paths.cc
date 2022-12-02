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
bool
crc_symb_execution::make_symbolic_func_args_and_sizes (function *fun,
						       state *initial_state)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nMaking symbolic function's following arguments:\n");
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
    return true;
}

/* Add declared ssa variables to the state.  */
bool
crc_symb_execution::add_function_local_ssa_vars (function *fun,
						 state *initial_state)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nAdding following ssa name declarations: \n");
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
      return true;
}

/* Calculate value of the rhs operation and assign to lhs variable.  */
bool
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
	    if (current_state->do_complement (op1, lhs))
	      return true;
	    return false;
	  case MEM_REF:
	    if (current_state->do_mem_ref (op1, lhs))
	      return true;
	    return false;
	  case NOP_EXPR:
	    if (current_state->do_assign (op1, lhs))
	      return true;
	    return false;
	  case SSA_NAME:
	    if (current_state->do_assign (op1, lhs))
	      return true;
	    return false;
	  case VAR_DECL:
	    if (current_state->do_assign (op1, lhs))
	      return true;
	    return false;
	  default:
	    if (dump_file)
	      fprintf (dump_file,
		       "Warning, encountered unsupported unary operation "
		       "with %s code while executing assign statement!\n",
		       get_tree_code_name (rhs_code));
	    return false;
	}
    }
  else if (gimple_num_ops (gs) == 3)
    {
      tree op1 = gimple_assign_rhs1 (gs);
      tree op2 = gimple_assign_rhs2 (gs);
      switch (rhs_code)
	{
	  case LSHIFT_EXPR:
	    if (current_state->do_shift_left (op1, op2, lhs))
	      return true;
	    return false;
	  case RSHIFT_EXPR:
	    if (current_state->do_shift_right (op1, op2, lhs))
	      return true;
	    return false;
	  case BIT_AND_EXPR:
	    if (current_state->do_and (op1, op2, lhs))
	      return true;
	    return false;
	  case BIT_IOR_EXPR:
	    if (current_state->do_or (op1, op2, lhs))
	      return true;
	    return false;
	  case BIT_XOR_EXPR:
	    if (current_state->do_xor (op1, op2, lhs))
	      return true;
	    return false;
	  case PLUS_EXPR:
	    if (current_state->do_add (op1, op2, lhs))
	      return true;
	    return false;
	  case MINUS_EXPR:
	    if (current_state->do_sub (op1, op2, lhs))
	      return true;
	    return false;
	  case MULT_EXPR:
	    if (current_state->do_mul (op1, op2, lhs))
	      return true;
	    return false;
	  case POINTER_PLUS_EXPR:
	    if (current_state->do_pointer_plus (op1, op2, lhs))
	      return true;
	    return false;
	  case POINTER_DIFF_EXPR:
	    if (current_state->do_pointer_diff (op1, op2, lhs))
	      return true;
	    return false;
	  default:
	    if (dump_file)
	      fprintf (dump_file,
		       "Warning, encountered unsupported binary operation "
		       "with %s code while executing assign statement!\n",
		       get_tree_code_name (rhs_code));
	    return false;
	}
    }
  else
    {
      if (dump_file)
	fprintf (dump_file,
		 "Warning, encountered unsupported operation, "
		 "with %s code while executing assign statement!\n",
		 get_tree_code_name (rhs_code));
      return false;
    }
  return true;
}

/* If the next block of the edge1 dest is a back edge, add in the stack edge2.
   Otherwise, add edge1 (the real execution path).

   When loop counter is checked in the if condition,
   we mustn't continue on real path,
   as we don't want to iterate the loop second time.  */
void add_edge (edge edge1, edge edge2, auto_vec<edge, 20>& stack)
{
  edge next_bb_edge = EDGE_SUCC (edge1->dest, 0);
  if (next_bb_edge && (next_bb_edge->flags & EDGE_DFS_BACK))
    {
      // FIXME: Compared variable's value will be incorrect,
      //  not satisfiable for the path.
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Won't iterate loop once more.\n");
      stack.quick_push (edge2);
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Adding the edge into the stack.\n");

      /* If the result of the condition is true/false,
	 continue execution only by the true branch.  */
      stack.quick_push (edge1);
    }
}

/* Add next basic blocks of the conditional block
   for the execution path into the stack.
   If the condition depends on symbolic values, keep both edges.
   If the condition is true, keep true edge, else - false edge.  */
void crc_symb_execution::add_next_bbs (basic_block cond_bb,
				      state *new_branch_state,
				      auto_vec<edge, 20>& stack)
{
  edge true_edge;
  edge false_edge;
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);

  if (new_branch_state->get_last_cond_status () == CS_SYM)
    {

      /* Add true branch's state into the states.
	 False branch's states will be kept in the current state.  */
      states.quick_push (new_branch_state);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Adding true and false edges into the stack.\n");

      /* Add outgoing edges to the stack.  */
      stack.quick_push (false_edge);
      stack.quick_push (true_edge);

      return;
    }
  else if (new_branch_state->get_last_cond_status () == CS_TRUE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Condition is true.\n");
      add_edge (true_edge, false_edge, stack);
    }
  else if (new_branch_state->get_last_cond_status () == CS_FALSE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Condition is false.\n");
      add_edge (false_edge, true_edge, stack);
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Something went wrong "
			    "during handling conditional statement.\n");
    }

  /* When we continue execution of only one path,
     there's no need of new state.  */
  delete new_branch_state;
}

/* Keep conditions depending on symbolic variables in the states.  */
bool crc_symb_execution::add_condition (const gcond* cond,
					state* current_state,
					state* new_branch_state)
{
  /* Keep conditions of each branch execution in its state.
     Ex.
       if (a == 0)  // a's value is unknown

       new_branch_state.keep (a==0)
       current_state.keep (a!=0)
  */

  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);
  switch (gimple_cond_code (cond))
    {
      case EQ_EXPR:
	new_branch_state->add_equal_cond (lhs, rhs);
	if (new_branch_state->get_last_cond_status () == CS_SYM)
	  current_state->add_not_equal_cond (lhs, rhs);
	return true;
      case NE_EXPR:
	new_branch_state->add_not_equal_cond (lhs, rhs);
	if (new_branch_state->get_last_cond_status () == CS_SYM)
	  current_state->add_equal_cond (lhs, rhs);
	return true;
      case GT_EXPR:
	new_branch_state->add_greater_than_cond (lhs, rhs);
	if (new_branch_state->get_last_cond_status () == CS_SYM)
	  current_state->add_less_or_equal_cond (lhs, rhs);
	return true;
      case LT_EXPR:
	new_branch_state->add_less_than_cond (lhs, rhs);
	if (new_branch_state->get_last_cond_status () == CS_SYM)
	  current_state->add_greater_or_equal_cond (lhs, rhs);
	return true;
      case GE_EXPR:
	new_branch_state->add_greater_or_equal_cond (lhs, rhs);
	if (new_branch_state->get_last_cond_status () == CS_SYM)
	  current_state->add_less_than_cond (lhs, rhs);
	return true;
      case LE_EXPR:
	new_branch_state->add_less_or_equal_cond (lhs, rhs);
	if (new_branch_state->get_last_cond_status () == CS_SYM)
	  current_state->add_greater_than_cond (lhs, rhs);
	return true;
      default:
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Unsupported condition.\n");
	return false;
    }
}

/* Create new state for true and false branch.
   Keep conditions in new created states.  */
bool
crc_symb_execution::resolve_condition (const gcond* cond,
				       auto_vec<edge, 20>& stack)
{
  /* Remove last state.  */
  state* current_state = states.last ();
  state* new_branch_state = new state (*current_state);

  if (!add_condition (cond, current_state, new_branch_state))
    return false;

  add_next_bbs (cond->bb, new_branch_state, stack);
  return true;
}

/* Keep the calculated value of the return value
   and the conditions of the executed path.  */
bool
crc_symb_execution::keep_return_val_and_conditions (const greturn* ret)
{
  tree return_op = gimple_return_retval (ret);

  if (!return_op)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "No return value.\n");
      return false;
    }

  if (TREE_CODE (return_op) == INTEGER_CST)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Return value is a constant.\n");
      return false;
    }

  /* Get calculated return value.  */
  state * curr_state = states.last ();
  vec<value*> * return_value = curr_state->get_bits (return_op);

  if (!return_value)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Return value is not in the state.\n");
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Return value is ");
      state::print_bits (return_value);
    }

  state * final_state = new state;

  /* Keep return value's calculated value and conditions in the final state.  */
  final_state->add_var_state (return_op, return_value);
  final_state->bulk_add_conditions (curr_state->get_conditions ());
  final_states.quick_push (final_state);

  delete curr_state;
  states.pop ();

  return true;
}


/* Execute gimple statements of BB.
   Keeping values of variables in the state.  */
bool
crc_symb_execution::execute_bb_gimple_statements (basic_block bb,
						  auto_vec<edge, 20>& stack)
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
	   if (!execute_assign_statement (as_a<const gassign *> (gs)))
	     return false;
	   break;
	  case GIMPLE_COND:
	    if (!resolve_condition (as_a<const gcond *> (gs), stack))
	      return false;
	    return true;
	  case GIMPLE_RETURN:
	    if (!keep_return_val_and_conditions (as_a<const greturn *> (gs)))
	      return false;
	    return true;
	  default:
	    if (dump_file)
	      fprintf (dump_file,
		       "Warning, encountered unsupported statement, "
		       "while executing gimple statements!\n");
	    return false;
	}
    }

  /* Add each outgoing edge of the current block to the stack,
     despite back edges.
     This code isn't reachable if the last statement of the basic block
     is a conditional statement or return statement.
     Those cases are handled separately.  */
  edge out_edge;
  edge_iterator ei;
  FOR_EACH_EDGE (out_edge, ei, bb->succs)
    if (!(out_edge->flags & EDGE_DFS_BACK))
      stack.quick_push (out_edge);
    else if (!states.is_empty ())
      {
	/* Delete the state after executing the full path,
	   or encountering back edge.  */
	delete states.last ();
	states.pop ();
      }

  return true;
}

/* Assign values of phi instruction to its result.
   Keep updated values in the state.  */
bool
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

      state *current_state = states.last ();
      if (!current_state->do_assign (rhs, lhs))
	return false;
    }
    return true;
}

/* Execute all statements of BB.
   Keeping values of variables in the state.  */
bool
crc_symb_execution::execute_bb_statements (basic_block bb,
					   edge incoming_edge,
					   auto_vec<edge, 20>& stack)
{
  if (!execute_bb_phi_statements (bb, incoming_edge))
      return false;

  return execute_bb_gimple_statements (bb, stack);
}

/* Traverse function fun's all paths from the first basic block to the last.
   Each time iterate loops only once.
   Symbolically execute statements of each path.  */
bool
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
      if (!execute_bb_statements (bb, e, stack))
	return false;
    }
    return true;
}

bool
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
  return traverse_function (fun);
}

/* Determine which bit of data must be 1.  */
unsigned HOST_WIDE_INT
determine_index (tree data, bool is_shift_left)
{
  if (is_shift_left)
    // FIXME: may be the data's size is larger,
    //  but MSB is checked for the middle bit.
    return tree_to_uhwi (TYPE_SIZE (TREE_TYPE (data))) - 1;
  else
    return 0;
}


/* Assign appropriate values to data, crc
   and other phi results to calculate the polynomial.  */
void
assign_vals_to_header_phis (state * polynomial_state, basic_block bb,
			    gphi *crc, gphi *data,
			    bool is_shift_left)
{
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {

      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);

      /* Don't consider virtual operands.  */
      if (virtual_operand_p (lhs))
	continue;


      if ((data && phi == data) || (!data && phi == crc))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Assigning the required value to ");
	      print_generic_expr (dump_file, lhs, dump_flags);
	      fprintf (dump_file, " variable.\n");
	    }
	  polynomial_state->do_assign_pow2 (lhs,
					    determine_index (lhs,
							      is_shift_left));
	}
      else if (phi == crc)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Assigning 0 value to ");
	      print_generic_expr (dump_file, lhs, dump_flags);
	      fprintf (dump_file, " variable.\n");
	    }
	  polynomial_state->do_assign (build_zero_cst (TREE_TYPE (lhs)), lhs);
	}
      else if (TREE_CODE (PHI_ARG_DEF (phi, phi->nargs-1)) == INTEGER_CST)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "First value of phi is a constant, "
				  "assigning the number to ");
	      print_generic_expr (dump_file, lhs, dump_flags);
	      fprintf (dump_file, " variable.\n");
	    }
	  polynomial_state->do_assign (PHI_ARG_DEF (phi, phi->nargs-1), lhs);
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "First value of phi isn't constant, "
				  "assigning to ");
	      print_generic_expr (dump_file, lhs, dump_flags);
	      fprintf (dump_file, " variable.\n");
	    }
	  polynomial_state->do_assign (build_zero_cst (TREE_TYPE (lhs)), lhs);
	}
    }
}


/* Execute the loop, which calculates crc with initial values,
   to calculate the polynomial.  */
bool
crc_symb_execution::execute_crc_loop (loop *crc_loop, gphi *crc, gphi *data,
				      bool is_shift_left)
{
  basic_block bb = crc_loop->header;
  assign_vals_to_header_phis (states.last (), bb, crc, data, is_shift_left);

  auto_vec<edge, 20> stack (crc_loop->num_nodes);

  execute_bb_gimple_statements (bb, stack);

  /* stack may not be empty.  Successor BB's are added into the stack
     from the execute_bb_gimple_statements function.  */
  while (!stack.is_empty ())
    {
      /* Look at the edge on the top of the stack.  */
      edge e = stack.last ();
      stack.pop ();

      /* Get dest block of the edge.  */
      basic_block bb = e->dest;

      /* Execute only basic blocks of the crc_loop.  */
      if (!flow_bb_inside_loop_p (crc_loop, bb))
	continue;


      /* Execute statements.  */
      if (!execute_bb_statements (bb, e, stack))
	return false;
    }
  return true;
}


/* Execute the loop, which is expected to calculate CRC,
   to extract polynomial, assigning real numbers to crc and data.  */
vec<value*> *
crc_symb_execution::extract_polynomial (loop *crc_loop,
					gphi *crc, gphi *data,
					bool is_shift_left)
{
  if (dump_file)
    fprintf (dump_file, "\n\nTrying to calculate the polynomial.\n\n");

  state * polynomial_state = new state;
  states.quick_push (polynomial_state);

  execute_crc_loop (crc_loop, crc, data, is_shift_left);

  if (states.length () != 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "The number of states is not one.\n");
      return nullptr;
    }

  /* Get the tree which will contain the value of the polynomial
     at the end of the loop.  */
  tree calculated_crc = PHI_ARG_DEF (crc, 0);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Getting the value of ");
      print_generic_expr (dump_file, calculated_crc, dump_flags);
      fprintf (dump_file, " variable.\n");
    }


  /* Get the value of the tree.  */
  vec<value*> * polynomial = polynomial_state->get_bits (calculated_crc);
  if (polynomial == nullptr)
   {
    if (dump_file && (dump_flags & TDF_DETAILS))
       fprintf (dump_file, "Polynomial's value is null");
    return nullptr;
   }

   if (dump_file && (dump_flags & TDF_DETAILS))
     {
	/* Note: It may not be the real polynomial.
	   If it's a bit reflected crc, then to get a real polynomial,
	   it must be reflected and 1 bit added.  */
	fprintf (dump_file, "Polynomial's value is ");
	state::print_bits (polynomial);
     }
  return polynomial;
}