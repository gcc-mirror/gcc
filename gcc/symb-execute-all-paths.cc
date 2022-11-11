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
						       State *initial_state)
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
	  unsigned HOST_WIDE_INT
	  size = tree_to_uhwi (DECL_SIZE (arg));
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
						 State *initial_state)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nAdding following ssa name declarations: \n");
  unsigned ix;
  tree name;
  /* Get ssa names of the function, yet print sizes and names.  */
  FOR_EACH_SSA_NAME (ix, name, fun)
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    print_generic_expr (dump_file, name, dump_flags);
	  }
	unsigned HOST_WIDE_INT
	size;
	if (TREE_CODE (TREE_TYPE (name)) == INTEGER_TYPE)
	  {
	    if (TYPE_UNSIGNED (TREE_TYPE (name)))
	      {
		// We need this info for symb execution.
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file,
			   " unsigned,");
	      }
	    size = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (name)));
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, " size is %lu.\n", size);
	  }
	else if (TREE_CODE (TREE_TYPE (name)) == POINTER_TYPE)
	  {
	    size = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (name)));

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, " pointer type, size is %lu.\n", size);
	  }
	else
	  continue;

	/* Add ssa variable with its size to the state.  */
	initial_state->decl_var (name, size);
      }
}

/* Calculate value of the rhs operation and assign to lhs variable.  */
void
crc_symb_execution::execute_assign_statement (const gassign *gs)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (gs);
  tree lhs = gimple_assign_lhs (gs);
  State *current_state = states.last ();
  if (gimple_num_ops (gs) == 2 && rhs_code == BIT_NOT_EXPR)
    {
      tree op1 = gimple_assign_rhs1 (gs);
      // current_state->do_complement (op1, lhs);
    }
  else if (gimple_num_ops (gs) == 3)
    {
      tree op1 = gimple_assign_rhs1 (gs);
      tree op2 = gimple_assign_rhs2 (gs);
      switch (rhs_code)
	{
	  case LSHIFT_EXPR:
	    // current_state->do_shift_left (op1, op2, lhs);
	    break;
	  case RSHIFT_EXPR:
	    // current_state->do_shift_right (op1, op2, lhs);
	    break;
	  case BIT_AND_EXPR:
	    // current_state->do_and (op1, op2, lhs);
	    break;
	  case BIT_IOR_EXPR:
	   // current_state->do_or (op1, op2, lhs);
	    break;
	  case BIT_XOR_EXPR:
	   // current_state->do_xor (op1, op2, lhs);
	    break;
	  case PLUS_EXPR:
	   // current_state->do_add (op1, op2, lhs);
	    break;
	  case MINUS_EXPR:
	   // current_state->do_sub (op1, op2, lhs);
	    break;
	  case MULT_EXPR:
	    // current_state->do_mul (op1, op2, lhs);
	    break;
	  default:
	    if (dump_file)
	      fprintf (dump_file,
		       "Warning, encountered unsupported binary operation, "
		       "while executing assign statement!\n");
	  break;
	}
    }
  else
    {
      if (dump_file)
	fprintf (dump_file,
		 "Warning, encountered unsupported ternary operation, "
		 "while executing assign statement!\n");
    }
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
	  fprintf (dump_file, "Executing following statement:\n");
	  print_gimple_stmt (dump_file, gs, dump_flags);
	}
      switch (gimple_code (gs))
	{
	  case GIMPLE_ASSIGN:
	    execute_assign_statement (as_a<const gassign *> (gs));
	  break;
	  case GIMPLE_COND:
	    //TODO: Examine condition.  Fork state if needed and keep condition.
	    break;
	  case GIMPLE_RETURN:
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
crc_symb_execution::execute_bb_phi_statements (basic_block bb)
{
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi_stmt = gsi.phi ();
      //TODO: assign value to the result of phi.
    }
}

/* Execute all statements of BB.
   Keeping values of variables in the state.  */
void
crc_symb_execution::execute_bb_statements (basic_block bb)
{
  execute_bb_phi_statements (bb);
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
  auto_vec<basic_block, 20> stack (n_basic_blocks_for_fn (fun) + 1);

  /* Push the first block into the stack.  */
  stack.quick_push (ENTRY_BLOCK_PTR_FOR_FN (fun));

  while (!stack.is_empty ())
    {
      /* Look at the block on the top of the stack.  */
      basic_block bb = stack.last ();
      stack.pop ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\nExecuting BB <%d>\n", bb->index);

      /* Symbolically execute statements.  */
      execute_bb_statements (bb);

      /* Add each successor block of the current block to the stack,
       * despite the one connected with back edge.  */
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs) if (!(e->flags & EDGE_DFS_BACK))
	  stack.quick_push (e->dest);
    }
}

void
crc_symb_execution::execute_function (function *fun)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nExecuting CRC-like function.\n");

  /* Create initial state and push into the vector of states.  */
  states.quick_push (new State);
  State *initial_state = states.last ();

  make_symbolic_func_args_and_sizes (fun, initial_state);

  /* Add declared variables to the state.  */
  add_function_local_ssa_vars (fun, initial_state);

  /* Execute function's statements, keeping a state for each path.  */
  traverse_function (fun);
}
