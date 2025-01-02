/* Execute symbolically all paths of the loop.
   Calculate the value of the polynomial by executing loop with real values to
   create LFSR state.
   After each iteration check that final states of calculated CRC values match
   determined LFSR.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Mariam Arutunian <mariamarutunian@gmail.com>

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

#include "crc-verification.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "cfganal.h"
#include "tree-ssa-loop.h"

/* Check whether defined variable is used outside the loop, only
   CRC's definition is allowed to be used outside the loop.  */

bool
crc_symbolic_execution::is_used_outside_the_loop (tree def)
{
  imm_use_iterator imm_iter;
  gimple *use_stmt;
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    {
      if (!flow_bb_inside_loop_p (m_crc_loop, use_stmt->bb))
	{
	  if (is_a<gphi *> (use_stmt)
	      && as_a<gphi *> (use_stmt) == m_output_crc)
	    return false;
	  if (dump_file)
	    fprintf (dump_file, "Defined variable is used outside the loop.\n");
	  return true;
	}
    }
  return false;
}

/* Calculate value of the rhs operation of GS assigment statement
   and assign it to lhs variable.  */

bool
crc_symbolic_execution::execute_assign_statement (const gassign *gs)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (gs);
  tree lhs = gimple_assign_lhs (gs);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "lhs type : %s \n",
	     get_tree_code_name (TREE_CODE (lhs)));

  /* This will filter some normal cases too.  Ex.  usage of array.  */
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  /* Check uses only when m_output_crc is known.  */
  if (m_output_crc)
    if (is_used_outside_the_loop (lhs))
      return false;

  if (gimple_num_ops (gs) != 2 && gimple_num_ops (gs) != 3)
    {
      if (dump_file)
	fprintf (dump_file,
		 "Warning, encountered unsupported operation, "
		 "with %s code while executing assign statement!\n",
		 get_tree_code_name (rhs_code));
      return false;
    }

  tree op1 = gimple_assign_rhs1 (gs);
  tree op2 = nullptr;

  if (gimple_num_ops (gs) == 3)
    op2 = gimple_assign_rhs2 (gs);

  state *current_state = m_states.last ();
  return current_state->do_operation (rhs_code, op1, op2, lhs);
}

/* Add E edge into the STACK if it doesn't have an immediate
   successor edge going to the loop header.

   When loop counter is checked in the if condition,
   we mustn't continue on real path as we want to stop the execution before
   the second iteration.  */

bool
crc_symbolic_execution::add_edge (edge e, auto_vec<edge> &stack)
{
  if (EDGE_COUNT (e->dest->succs) == 0)
    return false;

  edge next_bb_edge = EDGE_SUCC (e->dest, 0);
  if (next_bb_edge && next_bb_edge->dest == m_crc_loop->header)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Completed one iteration.  "
			    "Won't iterate loop once more, yet.\n");

      return keep_states ();
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Adding the edge into the stack.\n");

      /* If the result of the condition is true/false,
	 continue execution only by the true/false branch.  */
      stack.quick_push (e);
    }
  return true;
}

/* Add next basic blocks of the conditional block COND_BB
   for the execution path into the STACK.
   If the condition depends on symbolic values, keep both edges.
   If the condition is true, keep true edge, else - false edge.
   Returns true if addition succeeds.  Otherwise - false.  */

bool
crc_symbolic_execution::add_next_bbs (basic_block cond_bb,
				      state *new_branch_state,
				      auto_vec<edge> &stack)
{
  edge true_edge;
  edge false_edge;
  extract_true_false_edges_from_block (cond_bb, &true_edge, &false_edge);

  /* When the condition depends on symbolic values.  */
  if (new_branch_state->get_last_cond_status () == CS_SYM)
    {
      /* Supported CRC cases may have only two states.  */
      if (m_states.length () == 2)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Going to add a new state, "
				"but there's already two states.\n");
	  return false;
	}
      /* Add true branch's state into the states.
	 False branch's state will be kept in the current state.  */
      m_states.quick_push (new_branch_state);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Adding true and false edges into the stack.\n");

      /* Add outgoing edges to the stack.  */
      stack.quick_push (false_edge);
      stack.quick_push (true_edge);

      return true;
    }
  /* When the condition evaluates to true.  */
  else if (new_branch_state->get_last_cond_status () == CS_TRUE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Condition is true.\n");
      add_edge (true_edge, stack);
    }
  /* When the condition evaluates to false.  */
  else if (new_branch_state->get_last_cond_status () == CS_FALSE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Condition is false.\n");
      add_edge (false_edge, stack);
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Something went wrong "
			    "during handling conditional statement.\n");
      return false;
    }

  /* When we continue execution of only one path,
     there's no need of new state.  */
  delete new_branch_state;
  return true;
}

/* Add conditions depending on symbolic variables in the states.

   Keep conditions of each branch execution in its state.
     Ex.
       if (a == 0)  // a's value is unknown

       new_branch_state.keep (a==0)
       current_state.keep (a!=0)

     The condition is kept in the bit level.
     For ex.
     If a's size is 8 and its value is {symb_a, 0, 0, 0, 0, 0, 0, 0},
     then for a == 0 we'll have symb_a == 0 condition.  */

bool
crc_symbolic_execution::add_condition (const gcond *cond,
				       state *current_state,
				       state *new_branch_state)
{
  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);
  switch (gimple_cond_code (cond))
    {
      case EQ_EXPR:
	{
	  new_branch_state->add_equal_cond (lhs, rhs);
	  if (new_branch_state->get_last_cond_status () == CS_SYM)
	    current_state->add_not_equal_cond (lhs, rhs);
	  return true;
	}
      case NE_EXPR:
	{
	  new_branch_state->add_not_equal_cond (lhs, rhs);
	  if (new_branch_state->get_last_cond_status () == CS_SYM)
	    current_state->add_equal_cond (lhs, rhs);
	  return true;
	}
      case GT_EXPR:
	{
	  new_branch_state->add_greater_than_cond (lhs, rhs);
	  if (new_branch_state->get_last_cond_status () == CS_SYM)
	    current_state->add_less_or_equal_cond (lhs, rhs);
	  return true;
	}
      case LT_EXPR:
	{
	  new_branch_state->add_less_than_cond (lhs, rhs);
	  if (new_branch_state->get_last_cond_status () == CS_SYM)
	    current_state->add_greater_or_equal_cond (lhs, rhs);
	  return true;
	}
      case GE_EXPR:
	{
	  new_branch_state->add_greater_or_equal_cond (lhs, rhs);
	  if (new_branch_state->get_last_cond_status () == CS_SYM)
	    current_state->add_less_than_cond (lhs, rhs);
	  return true;
	}
      case LE_EXPR:
	{
	  new_branch_state->add_less_or_equal_cond (lhs, rhs);
	  if (new_branch_state->get_last_cond_status () == CS_SYM)
	    current_state->add_greater_than_cond (lhs, rhs);
	  return true;
	}
      default:
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Unsupported condition.\n");
	  return false;
	}
    }
}

/* Create new states for true and false branches.
   Keep conditions in new created states.  */

bool
crc_symbolic_execution::resolve_condition (const gcond *cond,
					   auto_vec<edge> &stack)
{
  state *current_state = m_states.last ();
  state *new_branch_state = new state (*current_state);

  /* Create new states and for true and false branches keep corresponding
     conditions.  */
  if (!add_condition (cond, current_state, new_branch_state))
    return false;

  /* Add true and false edges to the stack.  */
  return add_next_bbs (cond->bb, new_branch_state, stack);
}

/* If final states are less than two, add new FINAL_STATE and return true.
   Otherwise, return false.
   Supported CRC cases may not have more than two final states.  */
bool crc_symbolic_execution::add_final_state (state *final_state)
{
  if (m_final_states.length () < 2)
      m_final_states.quick_push (final_state);
  else
    {
      if (dump_file)
	fprintf (dump_file,
		 "There are already two final states\n");
      return false;
    }
    return true;
}

/* Keep the state of the executed path in final states.  */

bool crc_symbolic_execution::keep_states ()
{
  if (m_states.is_empty ())
    return false;

  if (!add_final_state (m_states.last ()))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Couldn't add final state.\n");
      return false;
    }

  m_states.pop ();
  return true;
}

/* Execute gimple statements of BB.
   Keeping values of variables in the state.  */

bool
crc_symbolic_execution::execute_bb_gimple_statements (basic_block bb,
						      auto_vec<edge> &stack)
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
	    {
	      if (!execute_assign_statement (as_a<const gassign *> (gs)))
		return false;
	      break;
	    }
	  case GIMPLE_COND:
	    {
	      return resolve_condition (as_a<const gcond *> (gs), stack);
	    }
	  /* Just skip debug statements.  */
	  case GIMPLE_DEBUG:
	    break;
	  default:
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "Warning, encountered unsupported statement, "
			 "while executing gimple statements!\n");
	      return false;
	    }
	}
    }

  /* Add each outgoing edge of the current block to the stack,
     despite the edges going to the loop header.
     This code isn't reachable if the last statement of the basic block
     is a conditional statement or return statement.
     Those cases are handled separately.
     We mustn't encounter edges going to the CRC loop header.  */

  edge out_edge;
  edge_iterator ei;
  FOR_EACH_EDGE (out_edge, ei, bb->succs)
    if (out_edge->dest != m_crc_loop->header)
      stack.quick_push (out_edge);
    else
      return false;

  return true;
}

/* Assign values of phi instruction to its result.
   Keep updated values in the state.  */

bool
crc_symbolic_execution::execute_bb_phi_statements (basic_block bb,
						   edge incoming_edge)
{
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);

      /* Check uses only when m_output_crc is known.  */
      if (m_output_crc)
	if (is_used_outside_the_loop (lhs))
	  return false;

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

      state *current_state = m_states.last ();
      if (!current_state->do_operation (VAR_DECL, rhs, nullptr, lhs))
	return false;
    }
  return true;
}

/* Execute all statements of BB.
   Keeping values of variables in the state.  */

bool
crc_symbolic_execution::execute_bb_statements (basic_block bb,
					       edge incoming_edge,
					       auto_vec<edge> &stack)
{
  if (!execute_bb_phi_statements (bb, incoming_edge))
    return false;

  return execute_bb_gimple_statements (bb, stack);
}

/* If the phi statements' result variables have initial constant value in the
   beginning of the loop, initialize those variables.  */

void
assign_known_vals_to_header_phis (state *state, class loop *crc_loop)
{
  basic_block bb = crc_loop->header;
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {

      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);

      /* Don't consider virtual operands.  */
      if (virtual_operand_p (lhs))
	continue;

      tree inital_val = PHI_ARG_DEF_FROM_EDGE (phi,
					       loop_preheader_edge (crc_loop));
      if (TREE_CODE (inital_val) == INTEGER_CST)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "First value of phi is a constant, "
				  "assigning the number to ");
	      print_generic_expr (dump_file, lhs, dump_flags);
	      fprintf (dump_file, " variable.\n");
	    }
	  state->do_operation (VAR_DECL, inital_val,
			       nullptr, lhs);
	}
    }
}

/* If the phi statements' result variables have initial constant value in the
   beginning of the loop, initialize those variables with
   the value calculated during the previous iteration.  */

bool
assign_calc_vals_to_header_phis (const vec<state *> &prev_states,
				 state *curr_state,
				 class loop *crc_loop)
{
  basic_block bb = crc_loop->header;
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);

      /* Don't consider virtual operands.  */
      if (virtual_operand_p (lhs))
	continue;
      tree inital_val = PHI_ARG_DEF_FROM_EDGE (phi,
					       loop_preheader_edge (crc_loop));
      if (TREE_CODE (inital_val) == INTEGER_CST)
	{
	  tree input_tree = PHI_ARG_DEF_FROM_EDGE (phi,
						   loop_latch_edge (crc_loop));
	  value * val_st1 = prev_states[0]->get_value (input_tree),
	      *val_st2 = prev_states[1]->get_value (input_tree);
	  if (!state::is_bit_vector (val_st1)
	      || !state::is_bit_vector (val_st2))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "The calculated values of  ");
		  print_generic_expr (dump_file, input_tree, dump_flags);
		  fprintf (dump_file, " variable is not constant.\n");
		}
	      return false;
	    }
	  else if (!state::check_const_value_equality (val_st1, val_st2))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "The calculated values of  ");
		  print_generic_expr (dump_file, input_tree, dump_flags);
		  fprintf (dump_file, " variable is different in the previous "
				      "iteration paths.\n");
		}
	      return false;
	    }
	  else
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Assigning calculated number to ");
		  print_generic_expr (dump_file, lhs, dump_flags);
		  fprintf (dump_file, " variable.\n");
		}
	      unsigned HOST_WIDE_INT calc_number
		  = state::make_number (val_st1);
	      tree calc_num_tree = build_int_cstu (TREE_TYPE (lhs),
						   calc_number);
	      curr_state->do_operation (VAR_DECL, calc_num_tree, nullptr, lhs);
	    }
	}
    }
  return true;
}

/* Create initial state of the CRC_LOOP's header BB variables which have
   constant values.
   If it is the first iteration of the loop, initialise variables with the
   initial values, otherwise initialise the variable with the value calculated
   during the previous execution.  */

state *
crc_symbolic_execution::create_initial_state (class loop *crc_loop)
{
  state *curr_state = new state;
  if (!m_final_states.is_empty ())
    {
      if (!assign_calc_vals_to_header_phis (m_final_states, curr_state,
					    crc_loop))
	return nullptr;
      state::remove_states (&m_final_states);
    }
  else
    assign_known_vals_to_header_phis (curr_state, crc_loop);
  return curr_state;
}

/* Symbolically execute the CRC loop, doing one iteration.  */

bool
crc_symbolic_execution::symb_execute_crc_loop ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nExecuting the loop with symbolic values.\n\n");

  state *curr_state = create_initial_state (m_crc_loop);
  if (!curr_state)
    return false;

  m_states.quick_push (curr_state);

  auto_vec<edge> stack (m_crc_loop->num_nodes);

  basic_block header_bb = m_crc_loop->header;
  if (!execute_bb_gimple_statements (header_bb, stack))
    return false;

  /* Successor BB's are added into the stack
     from the execute_bb_gimple_statements function.  */
  while (!stack.is_empty ())
    {
      /* Look at the edge on the top of the stack.  */
      edge e = stack.last ();
      stack.pop ();

      /* Get destination block of the edge.  */
      basic_block dest_bb = e->dest;

      /* Execute only basic blocks of the m_crc_loop.
	 At the end of the execution path save states in final states.  */
      if (!flow_bb_inside_loop_p (m_crc_loop, dest_bb))
	{
	  m_is_last_iteration = true;
	  if (!keep_states ())
	    return false;
	  continue;
	}

      /* Execute statements.  */
      if (!execute_bb_statements (dest_bb, e, stack))
	return false;
    }
  return true;
}

/* Determine which bit of the DATA must be 1.
   We assume that last bit must be 1.  */

unsigned HOST_WIDE_INT
determine_index (tree data, bool is_shift_left)
{
  if (is_shift_left)
   /* This won't work correctly in the case when data's size is larger,
      but MSB is checked for the middle bit.  */
    return tree_to_uhwi (TYPE_SIZE (TREE_TYPE (data))) - 1;
  return 0;
}

/* Assign appropriate values to data, CRC
   and other phi results to calculate the polynomial.  */

void
assign_vals_to_header_phis (state *polynomial_state, class loop *crc_loop,
			    gphi *crc_phi, gphi *data_phi,
			    bool is_shift_left)
{
  basic_block bb = crc_loop->header;
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {

      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);

      /* Don't consider virtual operands.  */
      if (virtual_operand_p (lhs))
	continue;

      if ((data_phi && phi == data_phi) || (!data_phi && phi == crc_phi))
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
      else if (phi == crc_phi)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Assigning 0 value to ");
	      print_generic_expr (dump_file, lhs, dump_flags);
	      fprintf (dump_file, " variable.\n");
	    }
	  polynomial_state->do_operation (VAR_DECL,
					  build_zero_cst (TREE_TYPE (lhs)),
					  nullptr, lhs);
	}
      else
	{
	  edge loop_preheader = loop_preheader_edge (crc_loop);
	  tree inital_val = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader);
	  if (TREE_CODE (inital_val) == INTEGER_CST)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "First value of phi is a constant, "
				      "assigning the number to ");
		  print_generic_expr (dump_file, lhs, dump_flags);
		  fprintf (dump_file, " variable.\n");
		}
	      polynomial_state->do_operation (VAR_DECL, inital_val,
					      nullptr, lhs);
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
	      polynomial_state->do_operation (VAR_DECL,
					      build_zero_cst (TREE_TYPE (lhs)),
					      nullptr, lhs);
	    }
	}
    }
}

/* Execute the loop, which calculates CRC with initial values,
   to calculate the polynomial.  */

bool
crc_symbolic_execution::execute_crc_loop (gphi *crc_phi,
					  gphi *data_phi,
					  bool is_shift_left)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nTrying to calculate the polynomial.\n\n");

  m_states.quick_push (new state);

  basic_block bb = m_crc_loop->header;
  assign_vals_to_header_phis (m_states.last (), m_crc_loop, crc_phi, data_phi,
			      is_shift_left);

  auto_vec<edge> stack (m_crc_loop->num_nodes);

  if (!execute_bb_gimple_statements (bb, stack))
    return false;

  /* stack may not be empty.  Successor BB's are added into the stack
     from the execute_bb_gimple_statements function.  */
  while (!stack.is_empty ())
    {
      /* Look at the edge on the top of the stack.  */
      edge e = stack.last ();
      stack.pop ();

      /* Get dest block of the edge.  */
      basic_block bb = e->dest;

      /* Execute only basic blocks of the m_crc_loop.  */
      if (!flow_bb_inside_loop_p (m_crc_loop, bb))
	continue;

      /* Execute statements.  */
      if (!execute_bb_statements (bb, e, stack))
	return false;
    }

  if (m_final_states.length () != 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "The number of states is not one when executed "
			    "the loop for calculating the polynomial.\n");
      return false;
    }
  return true;
}

/* Return true if all bits of the POLYNOMIAL are constants (0 or 1).
   Otherwise return false.  */

bool
polynomial_is_known (const value *polynomial)
{
  for (size_t i = 0; i < polynomial->length (); i++)
    {
      if (!is_a<bit *> ((*polynomial)[i]))
	return false;
    }
  return true;
}

/* Execute the loop, which is expected to calculate CRC,
   to extract polynomial, assigning real numbers to CRC and data.
   Returns a pair, first value of the pair is the tree containing
   the value of the polynomial, second is the calculated polynomial.
   The pair may contain nullptr.  */

std::pair <tree, value *>
crc_symbolic_execution::extract_polynomial (gphi *crc_phi, gphi *data_phi,
					    tree calculated_crc,
					    bool is_shift_left)
{
  if (!execute_crc_loop (crc_phi, data_phi, is_shift_left))
    return std::make_pair (nullptr, nullptr);

  if (m_final_states.length () != 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "The number of states isn't one "
			    "after executing the loop.\n");
      return std::make_pair (nullptr, nullptr);
    }
  state *polynomial_state = m_final_states.last ();

  /* CALCULATED_CRC contains the value of the polynomial
     after one iteration of the loop.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Getting the value of ");
      print_generic_expr (dump_file, calculated_crc, dump_flags);
      fprintf (dump_file, " variable.\n");
    }

  /* Get the value (bit vector) of the tree (it may be the polynomial).  */
  value *polynomial = polynomial_state->get_value (calculated_crc);
  if (!polynomial)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Polynomial's value is null.\n");
      return std::make_pair (nullptr, nullptr);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      /* Note: It may not be the real polynomial.
	 If it's a bit reflected CRC,
	 then to get a real polynomial,
	 it must be reflected and 1 bit added.  */
      fprintf (dump_file, "Polynomial's value is ");
      state::print_value (polynomial);
    }

  /* Check that polynomial's all bits are constants.  */
  if (!polynomial_is_known (polynomial))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Polynomial's value is not constant.\n");
      return std::make_pair (nullptr, nullptr);
    }

  return std::make_pair (calculated_crc, polynomial);
}


/**************************** LFSR MATCHING *********************************/


/* Return true if CONST_BIT value equals to 1.
   Otherwise, return false.  */

bool
is_one (value_bit *const_bit)
{
  return is_a<bit *> (const_bit)
	 && (as_a<bit *> (const_bit))->get_val () == 1;
}

/* Return true if BIT is symbolic,
   its index is same as LFSR bit's index (LFSR_BIT_INDEX)
   and the origin is same as CRC_ORIGIN.  */

bool
is_a_valid_symb (value_bit *bit, tree crc_origin, size_t lfsr_bit_index)
{
  if (!is_a<symbolic_bit *> (bit))
    return false;

  symbolic_bit *sym_bit = as_a<symbolic_bit *> (bit);
  bool is_correct_index = (sym_bit->get_index () == lfsr_bit_index);
  bool is_same_crc_origin = (sym_bit->get_origin () == crc_origin);
  return is_correct_index && is_same_crc_origin;
}

/* Return true if the BIT is a valid crc[LFSR_BIT_INDEX] ^ 1,
   where i is a whole number and left part's origin is same as CRC_ORIGIN.
   LFSR_BIT_INDEX is the index of the LFSR bit from the same position as in CRC.

   If there is lfsr[8] at LFSR value vectors' 9-th bit,
   when in the CRC vectors' 9-th bit's value must be
   crc[8].

   Otherwise, return false.  */

bool
is_a_valid_xor_one (value_bit *bit, tree crc_origin, size_t lfsr_bit_index)
{
  if (is_a<bit_xor_expression *> (bit))
    {
      bit_xor_expression *xor_exp = as_a<bit_xor_expression *> (bit);
      if (is_one (xor_exp->get_right ()))
	return is_a_valid_symb (xor_exp->get_left (), crc_origin,
				lfsr_bit_index);
      return false;
    }
  return false;
}

/* Return true, if CONDITION_EXP checks CRC's MSB/LSB value
   (under which xor is/not done).
   Otherwise, return false.  */

bool
may_be_xors_condition (tree crc_origin, value_bit *condition_exp,
		       size_t sb_index)
{
  if (!crc_origin)
    return false;

  if (!condition_exp)
    return false;

  /* The CONDITION_EXP of CRC may be a symbolic bit, if CRC is xor-ed with
     the data, and updated CRC's significant bit is checked.
     So, the CONDITION_EXP will be CRC's condition if it's origin is the same as
     CRC_ORIGIN, and it's index equals to checked significant bit's index.  */
  if (is_a<symbolic_bit *> (condition_exp))
    {
      symbolic_bit *condition_symbolic = as_a<symbolic_bit *> (condition_exp);
      return crc_origin == condition_symbolic->get_origin ()
	     && sb_index == condition_symbolic->get_index ();
    }
    /* The CONDITION_EXP of CRC may be a bit_xor_expression,
       if CRC and data are xor-ed only for significant bit's check.
       I.e.  CONDITION_EXP in this case may be crc[]^data[].
       So, the CONDITION_EXP will be CRC's condition if it's left or right
       part's origin is the same as CRC_ORIGIN, and it's index equals to checked
       significant bit's index.  */
  else if (is_a<bit_xor_expression *> (condition_exp))
    {
      bit_xor_expression *condition_xor_exp = as_a<bit_xor_expression *>
	  (condition_exp);
      if (!(is_a<symbolic_bit *> (condition_xor_exp->get_left ())
	    && is_a<symbolic_bit *> (condition_xor_exp->get_right ())))
	return false;

      symbolic_bit *cond_left
	  = as_a<symbolic_bit *> (condition_xor_exp->get_left ());
      symbolic_bit *cond_right
	  = as_a<symbolic_bit *> (condition_xor_exp->get_right ());
      bool cond_left_is_crc = (crc_origin == cond_left->get_origin ()
			       && sb_index == cond_left->get_index ());
      bool cond_right_is_crc = (crc_origin == cond_right->get_origin ()
				&& sb_index == cond_right->get_index ());
      return cond_left_is_crc || cond_right_is_crc;
    }
  return false;
}

/* Check whether the condition is checked for significant bit being 0 or 1.
   If IS_ONE is 1, when check whether the significant bit is 1 (xor branch),
   if 0, whether the significant bit is 0 (not xor branch).  */

bool
is_crc_xor_condition (tree crc_origin, unsigned char is_one,
		      size_t sb_index, state *final_state)
{
  /* The CRC cases we detect must contain only one condition related to CRC.  */
  if (final_state->get_conditions ().elements () != 1)
    return false;

  auto condition_iter = final_state->get_conditions ().begin ();
  if (!is_a<bit_condition *> (*condition_iter))
    return false;

  /* If the condition is for checking MSB/LSB, then
     if is_one is 1 and the condition is for checking MSB/LSB being one, or
     if is_one is 0 and condition is for checking MSB/LSB being 0
     return true, otherwise - false.  */
  value_bit *cond_exp = (*condition_iter)->get_left ();
  if (may_be_xors_condition (crc_origin, cond_exp, sb_index))
    {
      if (!is_a<bit *> ((*condition_iter)->get_right ()))
	return false;

      bit_condition *condition = as_a<bit_condition *> (*condition_iter);
      unsigned char comparison_val
	  = as_a<bit *> ((*condition_iter)->get_right ())->get_val ();
      if (condition->get_code () == EQ_EXPR)
	return comparison_val == is_one;
      if (condition->get_code () == NE_EXPR)
	return comparison_val != is_one;
      return false;
    }
  return false;
}

/* Check whether LSB/MSB of LFSR and calculated (maybe)CRC match.
   If MSB is checked in the CRC loop, then here we check LSB, or vice versa.
   CHECKED_SB_VALUE indicates which state of CRC value is checked.
   If the CHECKED_SB_VALUE is 1 - then xor-ed CRC value is checked,
   otherwise, not xor-ed is checked.  */

bool
given_sb_match (value_bit *crc, value_bit *lfsr,
		unsigned short checked_sb_value)
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
       then CRC's MSB/LSB must be equal to CHECKED_SB_VALUE.  */
  else if (is_a<symbolic_bit *> (lfsr))
    {
      if (!(is_a<bit *> (crc)
	    && (as_a<bit *> (crc)->get_val () == checked_sb_value)))
	return false;
      return true;
    }
  return false;
}

/* Check whether significant bit of LFSR and calculated (maybe)CRC match.  */

bool
sb_match (const value *lfsr, const value *crc_value, size_t sb_index,
	  size_t it_end, unsigned short value)
{
  /* If it's bit-forward CRC, check 0 bit's value.  */
  if (sb_index == it_end - 1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking 0 bit.\n");

      if (!given_sb_match ((*crc_value)[0], (*lfsr)[0], value))
	return false;
    }
    /* If it's bit-reversed CRC, check last bit's value.  */
  else if (sb_index == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking %zu bit.\n", it_end);

      if (!given_sb_match ((*crc_value)[it_end], (*lfsr)[it_end], value))
	return false;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Significant bit index is incorrect.\n");
    }
  return true;
}

/* Match the CRC to the LFSR, where CRC's all bit values are
   symbolic_bit or symbolic_bit ^ 1, besides MSB/LSB (it may be constant).  */

bool
lfsr_and_crc_bits_match (const value *lfsr, const value *crc_state,
			 tree crc_origin, size_t i, size_t it_end,
			 size_t sb_index, unsigned short checked_sb_value)
{

  /* Check whether significant bits of LFSR and CRC match.  */
  if (!sb_match (lfsr, crc_state, sb_index, it_end, checked_sb_value))
    return false;

  for (; i < it_end; i++)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Checking %zu bit.\n", i);

      /* Check the case when in lfsr we have LFSR (i)^LFSR (SBi),
	 where 0<i<LFSR_size and SBi is the index of MSB/LSB (LFSR_size-1/0).
	 In that case in crc_state (resulting CRC)
	 we must have crc (i) ^ 1 case, when condition is true
	 and crc (i) when condition is false,
	 (as CRC is xor-ed with the polynomial only if the LSB/MSB is one)
	 where k is a whole number.  */
      if (is_a<bit_xor_expression *> ((*lfsr)[i]))
	{
	  size_t index = (as_a<bit_xor_expression *> ((*lfsr)[i]))->get_left ()
	      ->get_index ();
	  /* Check CRC value of xor branch.  */
	  if (checked_sb_value == 1)
	    {
	      if (!(is_a_valid_xor_one ((*crc_state)[i], crc_origin, index)))
		return false;
	    }
	  else /* Check CRC value of not xor branch.  */
	    {
	      if (!(is_a_valid_symb ((*crc_state)[i], crc_origin, index)))
		return false;
	    }
	}
	/* Check the case when in LFSR we have LFSR (i), where 0<i<LFSR_size.
	   In that case in resulting CRC we must have crc (i) case,
	   when condition is true or condition is false.
	   If we have just LFSR (i), that means polynomial's i ± 1 bit is 0,
	   so despite CRC is xor-ed or not, we will have crc (i).  */
      else if (is_a<symbolic_bit *> ((*lfsr)[i]))
	{
	  size_t index = (as_a<symbolic_bit *> ((*lfsr)[i]))->get_index ();
	  if (!(is_a_valid_symb ((*crc_state)[i], crc_origin, index)))
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

/* Return origin of CRC_BIT.
   The first tree in loop, from which CRC's calculation is started.  */

tree
get_origin_of_crc_from_symb_bit (value_bit *crc_bit)
{
  if (is_a<symbolic_bit *> (crc_bit))
    return as_a<symbolic_bit *> (crc_bit)->get_origin ();
  return nullptr;
}

/* Return origin of CRC_BIT.  The first tree in loop, from which CRC's
   calculation is started.  If the CRC_BIT is symbolic value, return its origin,
   otherwise return its left part's origin (right must be 1 if its CRC's
   value). */

tree
get_origin_of_crc (value_bit *crc_bit)
{
  tree origin = get_origin_of_crc_from_symb_bit (crc_bit);
  if (origin)
    return origin;
  else if (is_a<bit_xor_expression *> (crc_bit))
    {
      value_bit *crc_bit_left
	  = as_a<bit_xor_expression *> (crc_bit)->get_left ();
      return get_origin_of_crc_from_symb_bit (crc_bit_left);
    }
  return nullptr;
}

/* Determine and initialize significant bit index
   (if MSB is checked for CRC, then it's LSB index, and vice versa)
   and the remaining part's begin and end.
   SB_INDEX is the significant bit index.
   IT_BEG is the beginning of the remaining part.
   IT_END is the end of the remaining part.  */

void
init_sb_index_and_other_part_begin_end (size_t &it_beg, size_t &it_end,
					size_t &sb_index, size_t crc_size,
					bool is_bit_forward)
{
  it_end = crc_size;
  if (is_bit_forward)
    {
      sb_index = it_end - 1;
      it_beg = 1;
    }
  else
    {
      it_beg = 0;
      sb_index = 0;
      --it_end;
    }
}

/* Return true if CRC_STATE matches the LFSR, otherwise - false.
   LFSR - is created LFSR value for the given polynomial and CRC size.
   CRC_STATE - contains CRC's calculated value and execution path condition.
   IT_BEG and IT_END - are the border indexes of the value to be matched.
   SB_INDEX - is the significant bit index of the CRC value,
	      which will be checked separately.
	      IF MSB is checked for CRC, when sb_index will be the index of LSB.
	      Otherwise, will be the index of MSB.
   CHECKED_SB_VALUE - is the significant bit's value (used for CRC's condition).
	      If CHECKED_SB_VALUE is 1, it indicates that CRC_STATE is
	      xor-ed path's state.
	      If CHECKED_SB_VALUE is 0, then CRC_STATE is the state of the
	      not xor branch.  */

bool
lfsr_matches_crc_state (const value *lfsr, state *crc_state, value *crc_value,
			size_t it_beg, size_t it_end, size_t sb_index,
			unsigned short checked_sb_value)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Starting to match the following CRC value: ");
      state::print_value (crc_value);
    }

  /* Get the origin (name) of the calculated CRC value.
     All bits must have the same origin.  */
  tree crc_origin = get_origin_of_crc ((*crc_value)[it_beg]);
  if (!crc_origin)
    return false;

  if (!is_crc_xor_condition (crc_origin, checked_sb_value, sb_index, crc_state))
    return false;

  /* Check whether CRC_VALUE and LFSR bits match.  */
  return lfsr_and_crc_bits_match (lfsr, crc_value, crc_origin,
				  it_beg, it_end, sb_index, checked_sb_value);
}

/* Return true if in the CRC_VALUE exists xor expression.
   Otherwise, return false.  */

bool
is_xor_state (value *crc_value, size_t it_beg, size_t it_end)
{
   for (unsigned j = it_beg; j < it_end; ++j)
     if ((*crc_value)[j]->get_type () == BIT_XOR_EXPRESSION)
       return true;
   return false;
}

/* Keep the value of calculated CRC.  */

value *
get_crc_val (tree calculated_crc, state *curr_state)
{
  if (!calculated_crc)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Couldn't get the potential CRC variable.\n");
      return nullptr;
    }

  /* When the calculated CRC is constant, it's not possible to determine
     whether the CRC has been calculated.  */
  if (TREE_CODE (calculated_crc) == INTEGER_CST)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Calculated CRC is a constant.\n");
      return nullptr;
    }

  /* Get calculated return value.  */
  value * crc_value = curr_state->get_value (calculated_crc);

  if (!crc_value)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "CRC is not in the state.\n");
      return nullptr;
    }
  return crc_value;
}

/* Return true if all states from the FINAL_STATES match the LFSR,
   otherwise - false.  */

bool
all_states_match_lfsr (value *lfsr, bool is_bit_forward, tree calculated_crc,
		       const vec<state *> &final_states)
{
  if (final_states.length () != 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "The final states count isn't two.\n");
      return false;
    }

  value *crc_xor_value = get_crc_val (calculated_crc, final_states[0]);
  value *crc_not_xor_value = get_crc_val (calculated_crc, final_states[1]);

  /* LFSR's size must be equal to CRC's size.  */
  if ((crc_xor_value->length () != lfsr->length ())
      || (crc_not_xor_value->length () != lfsr->length ()))
    return false;

  /* Depending on whether it is bit-forward or reversed CRC,
     determine in which significant bit new value is added,
     to examine that bit separately.
     If in the CRC algorithm MSB (sb_index) is checked to be one for xor,
     then here we check LSB separately (marginal bit).
     If LSB (sb_index) is checked, then we separate MSB (marginal bit).  */
  size_t it_beg, it_end, sb_index;
  init_sb_index_and_other_part_begin_end (it_beg, it_end, sb_index,
					  crc_xor_value->length (),
					  is_bit_forward);

    size_t xor_st_index = 0, not_xor_st_index = 1;
  /* If first is not xor's state,
     then the second state is assumed to be xor's state.  */
  if (!is_xor_state (crc_xor_value, it_beg, it_end))
    {
      std::swap (crc_xor_value, crc_not_xor_value);
      xor_st_index = 1;
      not_xor_st_index = 0;
    }

  /*  If xor-ed CRC value doesn't match the LFSR value, return false.  */
  if (!lfsr_matches_crc_state (lfsr, final_states[xor_st_index], crc_xor_value,
			       it_beg, it_end, sb_index, 1))
    return false;

  /*  If not xor-ed CRC value doesn't match the LFSR value, return false.  */
  if (!lfsr_matches_crc_state (lfsr, final_states[not_xor_st_index],
			       crc_not_xor_value, it_beg, it_end, sb_index, 0))
    return false;

  return true;
}
