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
	    /* FIXME: There can be something like
	       MEM[(some_type *)data + 1B],
	       in this case we just pass data.  */
	    if (current_state->do_mem_ref (TREE_OPERAND (op1, 0), lhs))
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
  value * return_value = curr_state->get_value (return_op);

  if (!return_value)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Return value is not in the state.\n");
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Return value is ");
      state::print_value (return_value);
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
					   auto_vec<edge, 20> &stack)
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
assign_vals_to_header_phis (state *polynomial_state, basic_block bb,
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
      else if (TREE_CODE (PHI_ARG_DEF (phi, phi->nargs - 1)) == INTEGER_CST)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "First value of phi is a constant, "
				  "assigning the number to ");
	      print_generic_expr (dump_file, lhs, dump_flags);
	      fprintf (dump_file, " variable.\n");
	    }
	  polynomial_state->do_assign (PHI_ARG_DEF (phi, phi->nargs - 1), lhs);
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
crc_symb_execution::execute_crc_loop (class loop *crc_loop, gphi *crc,
				      gphi *data,
				      bool is_shift_left)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nTrying to calculate the polynomial.\n\n");

  states.quick_push (new state);

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

  if (states.length () != 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "The number of states is not one.\n");
      return false;
    }
  return true;
}


/* Return true if all bits of the polynomial are constants (0 or 1).
   Otherwise return false.  */
bool polynomial_is_known (const value * polynomial)
{
  for (size_t i = 0; i < polynomial->length (); i++)
    {
      if (!is_a<bit *> ((*polynomial)[i]))
	return false;
    }
  return true;
}


/* Execute the loop, which is expected to calculate CRC,
   to extract polynomial, assigning real numbers to crc and data.  */
value *
crc_symb_execution::extract_poly_and_create_lfsr (class loop *crc_loop,
						  gphi *crc, gphi *data,
						  bool is_shift_left)
{
  if (!execute_crc_loop (crc_loop, crc, data, is_shift_left))
    return nullptr;

  state *polynomial_state = states.last ();

  /* Get the tree which will contain the value of the polynomial
     at the end of the loop.  */
  tree calculated_crc = PHI_ARG_DEF (crc, 0);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Getting the value of ");
      print_generic_expr (dump_file, calculated_crc, dump_flags);
      fprintf (dump_file, " variable.\n");
    }

  /* Get the value (bit vector) of the tree (it may be the polynomial).  */
  value * polynomial = polynomial_state->get_value (calculated_crc);
  if (!polynomial)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Polynomial's value is null");
      return nullptr;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      /* Note: It may not be the real polynomial.
	 If it's a bit reflected crc,
	 then to get a real polynomial,
	 it must be reflected and 1 bit added.  */
      fprintf (dump_file, "Polynomial's value is ");
      state::print_value (polynomial);
    }

  /* Check that polynomial's all bits are constants.  */
  if (!polynomial_is_known (polynomial))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Polynomial's value is not constant.\n");
	}
      return nullptr;
    }

  return state::create_lfsr (calculated_crc, polynomial, is_shift_left);
}


/* Returns true if index1 and index2 differ by a factor of 8.  */
bool acceptable_diff (size_t index1, size_t index2)
{
  size_t diff;
  if (index1 > index2)
    diff = index1 - index2;
  else
    diff = index2 - index1;

  /* Indexes of the symbolic bit may differ by 0, 8, 16, 24, 32, ...  */
  return diff % 8 == 0;
}

/* Check whether the condition_exp and refernce_exp match.  */
bool
may_be_xors_condition (value_bit *reference_exp, value_bit *condition_exp,
		       size_t sb_index)
{
  if (!reference_exp)
    return false;

  if (!condition_exp)
    return false;

  if (is_a<symbolic_bit *> (reference_exp)
      && is_a<symbolic_bit *> (condition_exp))
    {
      symbolic_bit *condition_symbolic = as_a<symbolic_bit *> (condition_exp);
      symbolic_bit *reference_symbolic = as_a<symbolic_bit *> (reference_exp);
      return reference_symbolic->get_origin ()
      == condition_symbolic->get_origin ()
	 && acceptable_diff (sb_index,
			     condition_symbolic->get_index ());
    }
  if (is_a<symbolic_bit *> (reference_exp)
      && is_a<bit_xor_expression *> (condition_exp))
    {
      return may_be_xors_condition (as_a<symbolic_bit *> (reference_exp),
				    as_a<bit_xor_expression *>
					(condition_exp)->get_left (), sb_index)
	     || may_be_xors_condition (as_a<symbolic_bit *> (reference_exp),
				       as_a<bit_xor_expression *>
					   (condition_exp)->get_right (),
				       sb_index);
    }
  if (is_a<bit_xor_expression *> (reference_exp)
      && is_a<bit_xor_expression *> (condition_exp))
    {
      return may_be_xors_condition (as_a<bit_xor_expression *>
					(reference_exp)->get_left (),
				    as_a<bit_xor_expression *>
					(condition_exp)->get_left (), sb_index)
	     && may_be_xors_condition (as_a<bit_xor_expression *>
					   (reference_exp)->get_right (),
				       as_a<bit_xor_expression *>
					   (condition_exp)->get_right (),
				       sb_index);
    }
  return false;
}


/* Checks whether the condition is checked for significant bit being 0 or 1.
   If is_one is 1, when check whether the significant bit is 1 (xor branch),
   if 0 whether the significant bit is 0 (not xor branch).  */
bool
check_condition (value_bit *symb_exp, unsigned char is_one,
		 size_t sb_index, state *final_state)
{
  for (auto iter = final_state->get_conditions ().begin ();
       iter != final_state->get_conditions ().end (); ++iter)
    {
      bit_condition * condition = nullptr;

      if (is_a <bit_condition *> (*iter))
	condition = as_a <bit_condition *> (*iter);
      else
	continue;

      value_bit *cond_exp = (*iter)->get_left ();
      if (may_be_xors_condition (symb_exp, cond_exp, sb_index))
	{
	  if (!is_a <bit *> ((*iter)->get_right ()))
	    return false;

	  unsigned char comparison_val = as_a <bit *> ((*iter)->get_right ())
	      ->get_val ();
	  if (condition->get_cond_type () == EQ_EXPR)
	      return comparison_val == is_one;
	  if (condition->get_cond_type () == NE_EXPR)
	    return comparison_val != is_one;
	  return false;
	}
    }
  return false;
}


/* Returns true if the symb_exp is a bit_xor_expression and const_bit equals 1.
   Otherwise, returns false.  */
bool
is_one (value_bit *const_bit)
{
  return is_a<bit *> (const_bit)
	 && (as_a<bit *> (const_bit))->get_val () == 1;
}


/* Returns true if bit is symbolic and its index differs from LFSR bit's index
   by a factor of 8.
   Sometimes calculated crc value is returned
   after being shifted by 8's factor.  */
bool
is_a_valid_symb (value_bit *bit, size_t lfsr_bit_index)
{
  if (!is_a<symbolic_bit *> (bit))
    return false;

  size_t sym_bit_index = (as_a<symbolic_bit *> (bit))->get_index ();
  return acceptable_diff (sym_bit_index, lfsr_bit_index);
}


/* Returns true if bit is crc[index+8*i]^data[index+8*i],
   where i is a whole number.
   Otherwise, returns false.  */
bool
is_a_valid_xor (value_bit *bit, size_t lfsr_bit_index)
{
  return is_a<bit_xor_expression *> (bit)
	 && is_a_valid_symb ((as_a<bit_xor_expression *> (bit))->get_left (),
			     lfsr_bit_index)
	 && is_a_valid_symb ((as_a<bit_xor_expression *> (bit))->get_right (),
			     lfsr_bit_index);
}


/* Returns true if the bit is a valid
   crc[index+8*i] ^ data[index+8*i] ^ 1, or crc[index+8*i] ^ 1,
   where i is a whole number.
   index is the index of the LFSR bit from the same position as in CRC.

   If there is lfsr[8] at LFSR value vectors' 9-th bit,
   when in the crc vectors' 9-th bit's value we must be
   crc[8] or maybe crc[16],...

   Otherwise, returns false.  */
bool
is_a_valid_xor_one (value_bit *bit, size_t lfsr_bit_index)
{
  if (is_a<bit_xor_expression *> (bit))
    {
      value_bit *symb_exp = nullptr;
      bit_xor_expression *xor_exp = as_a<bit_xor_expression *> (bit);
      if (is_one (xor_exp->get_right ()))
	symb_exp = xor_exp->get_left ();
      else if (is_one (xor_exp->get_left ()))
	symb_exp = xor_exp->get_right ();
      else
	return false;
      return is_a_valid_xor (symb_exp, lfsr_bit_index)
	     || is_a_valid_symb (symb_exp, lfsr_bit_index);
    }
  else
    return false;
}


/* Check whether LSB/MSB of LFSR and calculated (maybe)CRC match.  */
bool
significant_bits_match (value_bit *crc, value_bit *lfsr,
			value_bit *conditions_crc,
			size_t sb_index, state *final_state)
{
  /* If LFSR's MSB/LSB value is a constant (0 or 1),
     then CRC's MSB/LSB must have the same value.  */
  if (is_a<bit *> (lfsr))
    {
      if (!((is_a<bit *> (crc)
	     && as_a<bit *> (crc)->get_val ()
		== as_a<bit *> (lfsr)->get_val ())))
	return false;
      return true;
    }
    /* If LFSR's MSB/LSB value is a symbolic_bit
       (that means MSB/LSB of the polynomial is 1),
       then CRC's MSB/LSB must be 0 if the condition is false,
       1 - if the condition is true.
       (Because crc is xored with the polynomial in case LSB/MSB is 1).  */
  else if (is_a<symbolic_bit *> (lfsr))
    {
      if (!(is_a<bit *> (crc) && ((as_a<bit *> (crc)->get_val () == 0
				   && check_condition (conditions_crc, 0,
						       sb_index, final_state))
				  || (as_a<bit *> (crc)->get_val () == 1
				      && check_condition (conditions_crc, 1,
							  sb_index,
							  final_state)))))
	return false;
      return true;
    }
  return false;
}

/* Check whether LSB/MSB of LFSR and calculated (maybe)CRC match.  */
bool
significant_bits_match (const value *lfsr,
			const value *crc_state,
			bool is_left_shift,
			value_bit *symb_val,
			size_t it_end,
			state *final_state)
{
  if (is_left_shift)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking 0 bit.\n");

      if (!significant_bits_match ((*crc_state)[0], (*lfsr)[0], symb_val,
				   it_end-1, final_state))
	return false;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking %lu bit.\n", it_end);

      if (!significant_bits_match ((*crc_state)[it_end],
				   (*lfsr)[it_end], symb_val, 0, final_state))
	return false;
    }
  return true;
}


/* Match the crc to the lfsr, where crc's all bit values are bit_xor_expression
   or bit_xor_expression ^ 1, besides MSB/LSB.

  Under this are considered the cases,
  1.  When sizes of crc and data are same.

  2.  When data is xored with crc in the loop.  */
bool
match_lfsr_case2 (const value *lfsr,
		  const value *crc_state,
		  bit_xor_expression *symb_val,
		  bool is_left_shift,
		  size_t i, size_t it_end, size_t sb_index,
		  state *final_state)
{
  /* Check whether significant bits of LFSR and CRC match.  */
  if (!significant_bits_match (lfsr, crc_state, is_left_shift, symb_val,
			       it_end, final_state))
    return false;

  for (; i < it_end; i++)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking %lu bit.\n", i);

      if (is_a<bit_xor_expression *> ((*lfsr)[i]))
	{
	  size_t index = (as_a<bit_xor_expression *> ((*lfsr)[i]))->get_left ()
	      ->get_index ();
	  if (!((is_a_valid_xor_one ((*crc_state)[i], index)
		 && check_condition (
		     as_a<bit_xor_expression *> ((*crc_state)[i])->get_left (),
		     1, sb_index, final_state))
		|| (is_a_valid_xor ((*crc_state)[i], index)
		    && check_condition ((*crc_state)[i], 0, sb_index,
					final_state))))
	    return false;
	}
      else if (is_a<symbolic_bit *> ((*lfsr)[i]))
	{
	  size_t index = (as_a<symbolic_bit *> ((*lfsr)[i]))->get_index ();
	  if (!(is_a_valid_xor ((*crc_state)[i], index)
		&& (check_condition ((*crc_state)[i], 0, sb_index, final_state)
		   || check_condition ((*crc_state)[i], 1, sb_index,
				       final_state))))
	    return false;
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not expected expression in LFSR.\n");
	  return false;
	}
    }
  return true;
}


/* Match the crc to the lfsr, where crc's all bit values are symbolic_bits
   or symbolic_bits ^ 1, besides MSB/LSB (it may be constant).

  Under this are considered the cases,
  1.  When sizes of crc and data are same.

  2.  When data is xored with crc before the loop.
  3.  When data is xor-ed with crc only for the condition and
      xor is done only on crc.  */
bool
match_lfsr_case1 (const value *lfsr,
		  const value *crc_state,
		  symbolic_bit *symb_val,
		  bool is_left_shift,
		  size_t i, size_t it_end, size_t sb_index,
		  state *final_state)
{

  /* Check whether significant bits of LFSR and CRC match.  */
  if (!significant_bits_match (lfsr, crc_state, is_left_shift, symb_val,
			       it_end, final_state))
    return false;

  for (; i < it_end; i++)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking %lu bit.\n", i);

      /* Check the case when in lfsr we have LFSR (i)^LFSR (SBi),
	 where 0<i<LFSR_size and SBi is the index of MSB/LSB (LFSR_size-1/0).
	 In that case in crc_state (resulting CRC)
	 we must have crc (i+8*k)^1 case, when condition is true
	 and crc (i+8*k) when condition is false,
	 (as crc is xor-ed with the polynomial only if the LSB/MSB is one)
	 where k is a whole number
	 (in some implementations crc is shifted left or right by 8, 16...).  */
      if (is_a<bit_xor_expression *> ((*lfsr)[i]))
	{
	  size_t index = (as_a<bit_xor_expression *> ((*lfsr)[i]))->get_left ()
	      ->get_index ();
	  if (!((is_a_valid_xor_one ((*crc_state)[i], index)
		 && check_condition (
		     as_a<bit_xor_expression *> ((*crc_state)[i])->get_left (),
		     1, sb_index, final_state))
		|| (is_a_valid_symb ((*crc_state)[i], index)
		    && check_condition ((*crc_state)[i], 0, sb_index,
					final_state))))
	    return false;
	}
	/* Check the case when in LFSR we have LFSR (i), where 0<i<LFSR_size.
	   In that case in resulting CRC we must have crc (i+8*k) case,
	   when condition is true or condition is false.
	   If we have just LFSR (i), that means polynomial's i+-1 bit is 0,
	   so despite CRC is xor-ed or not, we will have crc (i).  */
      else if (is_a<symbolic_bit *> ((*lfsr)[i]))
	{
	  size_t index = (as_a<symbolic_bit *> ((*lfsr)[i]))->get_index ();
	  if (!(is_a_valid_symb ((*crc_state)[i], index)
		&& (check_condition ((*crc_state)[i], 0, sb_index, final_state)
		    || check_condition ((*crc_state)[i], 1, sb_index,
					final_state))))
	    return false;
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not expected expression in LFSR.\n");
	  return false;
	}
    }
  return true;
}


/* Returns true if in the CRC value's vector we have one of the form of
   following consecutive bits
   1.  0, then 1
   2.  1, then 0
   3.  0, then 0
   4.  1, then 1
   Otherwise returns false.
   */
bool
maybe_neighbour_vals_of_crc (bit *curr_bit_val,
			     value_bit *next_bit_val)
{
  if (!curr_bit_val)
    return true;

  /* As the value doesn't matter,
     just check that next_bit_val is bit *.  */
  if (is_a<bit *> (next_bit_val))
    return true;
  return false;
}


/* Returns true if in the CRC value's vector we have one of the form of
   following consecutive bits
   1. crc[], then crc[]
   2. crc[], then crc[]^data[]
   3. crc[], then crc[]^data[]^1
   4. crc[], then crc[]^1
   5. data[], then crc[]^data[]
   6. data[], then crc[]^data[]^1
   7. crc[], then crc[]^data1[]^data2[] TODO: Maybe needs a correction.
   ...
   Otherwise returns false.  */
bool
maybe_neighbour_vals_of_crc (symbolic_bit *curr_bit_val,
			     value_bit *next_bit_val)
{
  if (!curr_bit_val)
    return true;

  /* If both are symbolic_bits,
     check that they are bits of the same variable.  */
  if (is_a<symbolic_bit *> (next_bit_val))
    {
      return curr_bit_val->get_origin ()
	     == as_a<symbolic_bit *> (next_bit_val)->get_origin ();
    }
  else if (is_a<bit_xor_expression *> (next_bit_val))
    {
      return maybe_neighbour_vals_of_crc (curr_bit_val,
					  as_a<bit_xor_expression *>
					      (next_bit_val)->get_left ())
	     || maybe_neighbour_vals_of_crc (curr_bit_val,
					     as_a<bit_xor_expression *>
						 (next_bit_val)->get_right ());
    }
  return false;
}


/* Returns true if in the CRC value's vector we have one of the form of
   following consecutive bits
   1. crc[]^1 or crc[]^data[]^1, then crc[]
   2. crc[]^data[]^1, then crc[]^data[]
   3. crc[]^data[], then crc[] or data[]
   4. crc[]^data[], then crc[]^data[]
   5. crc[]^data[], then crc[]^data[]^1
   Otherwise returns false.  */
bool
maybe_neighbour_vals_of_crc (bit_xor_expression *curr_xor_exp,
			     value_bit *next_bit_val)
{
  if (!curr_xor_exp)
    return true;

  /* The case when we have some_expression ^ 1
     for the current bit's value.  */
  if (is_a<bit *> (curr_xor_exp->get_right ()))
    {
      /* The case when current bit's value is crc[]^1 or crc[]^data[]^1,
	 next bit's - crc[].
	 There may not be ^ 0 case, as expressions are optimized.  */
      if (is_a<symbolic_bit *> (next_bit_val))
	{
	  if (is_a<symbolic_bit *> (curr_xor_exp->get_left ()))
	    return maybe_neighbour_vals_of_crc (as_a<symbolic_bit *>
						    (curr_xor_exp->get_left ()),
						next_bit_val);
	  if (is_a<bit_xor_expression *> (curr_xor_exp->get_left ()))
	    return maybe_neighbour_vals_of_crc (as_a<bit_xor_expression *>
						    (curr_xor_exp->get_left ()),
						next_bit_val);
	}
	/* The case when current bit's value is crc[]^data[]^1,
	  next bit's - crc[]^data[].  */
      else if (is_a<bit_xor_expression *> (curr_xor_exp->get_left ())
	       && is_a<bit_xor_expression *> (next_bit_val))
	return maybe_neighbour_vals_of_crc
	    (as_a<bit_xor_expression *> (curr_xor_exp->get_left ()),
					 next_bit_val);
      else
	return false;
    }
    /* The case when current bit's value is crc[]^data[].  */
  else if (is_a<symbolic_bit *> (curr_xor_exp->get_right ())
	   && is_a<symbolic_bit *> (curr_xor_exp->get_left ()))
    {
      symbolic_bit * curr_xor_exp_left_sym
      = as_a<symbolic_bit *> (curr_xor_exp->get_left ());
      symbolic_bit * curr_xor_exp_right_sym
	  = as_a<symbolic_bit *> (curr_xor_exp->get_right ());

      /* The case when current bit's value is crc[]^data[],
	 next bit's - crc[] or data[].  */
      if (is_a<symbolic_bit *> (next_bit_val))
	{
	  return maybe_neighbour_vals_of_crc (curr_xor_exp_left_sym,
					      next_bit_val)
		 || maybe_neighbour_vals_of_crc (curr_xor_exp_right_sym,
						 next_bit_val);
	}
	/* The case when current bit's value is crc[]^data[],
	   next bit's - something ^ something.  */
      else if (is_a<bit_xor_expression *> (next_bit_val))
	{
	  bit_xor_expression *curr_xor_exp = as_a<bit_xor_expression *>
	      (next_bit_val);
	  /* The case when current bit's value is crc[]^data[],
	     next bit's - crc[]^data[].  */
	  if (is_a<symbolic_bit *> (curr_xor_exp->get_right ())
	      && is_a<symbolic_bit *> (curr_xor_exp->get_left ()))
	    {
	      return
		  maybe_neighbour_vals_of_crc (curr_xor_exp_left_sym,
					       curr_xor_exp->get_left ())
		  && maybe_neighbour_vals_of_crc (curr_xor_exp_right_sym,
						  curr_xor_exp->get_right ());
	    }
	    /* The case when current bit's value is crc[]^data[],
	       next bit's - crc[]^data[]^1.  */
	  else if (is_a<bit *> (curr_xor_exp->get_right ())
		   && is_a<bit_xor_expression *> (curr_xor_exp->get_left ()))
	    return maybe_neighbour_vals_of_crc (curr_xor_exp,
						curr_xor_exp->get_left ());
	}
    }
  return false;
}


/* Returns true if crc_state matches the LFSR, otherwise - false.  */
bool
state_matches_lfsr (const value *lfsr,
					const value *crc_state,
					bool is_left_shift, state *final_state)
{
  bit * const_bit = nullptr;
  symbolic_bit *symb_bit = nullptr;
  value_bit *simple_xor_right = nullptr;
  bit_xor_expression *xor_exp = nullptr, *complicated_xor = nullptr;
  bool has_other_val = false;

  /* Depending on whether it is bit forward or reversed CRC,
     determine significant bit, to examine that bit separately.  */
  size_t it_beg = 0, sb_index = 0,
	 it_end = lfsr->length () < crc_state->length ()
			      ? lfsr->length () : crc_state->length ();
  if (is_left_shift)
    {
      sb_index = it_end-1;
      it_beg = 1;
    }
  else
    --it_end;

  /* Check what kind of values are in the returned state
     (which is expected to be CRC). Return false,
     if there is such a value which must exist in the CRC value.  */
  for (unsigned j = it_beg; j < it_end - 1; ++j)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking %u bit's "
			    "and %u bit's compatibility.\n", j, j+1);
      switch ((*crc_state)[j]->get_type ())
	{
	  case SYMBOLIC_BIT:
	    symb_bit = as_a<symbolic_bit *> ((*crc_state)[j]);
	    if (!maybe_neighbour_vals_of_crc (symb_bit, (*crc_state)[j+1]))
	      return false;
	    break;
	  case BIT:
	    const_bit = as_a<bit *> ((*crc_state)[j]);
	    if (!maybe_neighbour_vals_of_crc (const_bit, (*crc_state)[j+1]))
	      return false;
	    break;
	  case BIT_XOR_EXPRESSION:
	    {
	      /* Calculated CRC may contain crc[]^data[],
		 or crc[]^1, or crc[]^data[]^1.  */
	      xor_exp
		  = as_a<bit_xor_expression *> ((*crc_state)[j]);

	      if (!maybe_neighbour_vals_of_crc (xor_exp, (*crc_state)[j+1]))
		return false;

	      if (is_a<symbolic_bit *> (xor_exp->get_left ())
		  && (is_a<bit *> (xor_exp->get_right ())
		      || is_a<symbolic_bit *> (xor_exp->get_right ())))
		  simple_xor_right = xor_exp->get_right ();
	      else if (is_a<bit_xor_expression *> (xor_exp->get_left ())
		       && is_a<bit *> (xor_exp->get_right ()))
		{
		  complicated_xor = as_a<bit_xor_expression *>
		      (xor_exp->get_left ());
		  simple_xor_right = xor_exp->get_right ();
		}
	      else
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Not expected xor expression.\n");
		  return false;
		}
	      break;
	    }
	  default:
	    has_other_val = true;
	}
    }

  if (has_other_val)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Checks passed.  Starting bit by bit matching.\n");

  if (!const_bit && symb_bit && !complicated_xor
      && (!simple_xor_right
	  || (simple_xor_right && is_a<bit *> (simple_xor_right))))
    return match_lfsr_case1 (lfsr, crc_state, symb_bit, is_left_shift,
			     it_beg, it_end, sb_index, final_state);

  if (!const_bit && !symb_bit)
    return match_lfsr_case2 (lfsr, crc_state, xor_exp, is_left_shift,
			     it_beg, it_end, sb_index, final_state);
  return false;
}


/* Returns true if all states match the LFSR, otherwise - false.  */
bool
crc_symb_execution::all_states_match_lfsr (value *lfsr,
					   bool is_left_shift)
{
  while (!final_states.is_empty ())
    {
      state *final_state = final_states.last ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Matching LFSR and following returned state.\n");
	  state::print_value (final_state->get_first_value ());
	  final_state->print_conditions ();
	}
      if (!state_matches_lfsr (lfsr, final_state->get_first_value (),
			       is_left_shift, final_state))
	return false;
      final_states.pop ();
      delete final_state;
    }
  return true;
}
