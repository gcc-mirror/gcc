/* CRC optimization.
   Copyright (C) 2006-2022 Free Software Foundation, Inc.
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

/* This pass performs crc optimization.  */
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
#include "crc_verification.h"
#include "internal-fn.h"
#include "tree-into-ssa.h"
#include "tree-ssa-loop-manip.h"
#include "predict.h"
#include "cfghooks.h"
#include "tree-ssa.h"
#include "tree-ssa-live.h"
#include "tree-dfa.h"
#include "dominance.h"
#include "tree-ssa-dce.h"
#include "tree-cfgcleanup.h"

class crc_optimization {
 private:
  /* shift_before_xor will contain the statement doing shift one operation
     (if exists and satisfies the CRC) before xor operation.  */
  gimple *shift_before_xor;

  /* shift_after_xor will contain the statement doing shift one operation
     (if shift_before_xor doesn't exist and satisfies the CRC) after xor
     operation on the same variable.  */
  gimple *shift_after_xor;

  /* Phi statement, result may be the crc variable.  */
  gphi *first_phi_for_crc;

  /* Sometimes polynomial may not be constant
     and xor-ed variable may depend on two variables.
     The result of phi statement may contain the polynomial.  */
  gphi *second_phi_for_crc;

  /* Phi statement, result maybe data (if exists).  */
  gphi *data;

  /* The loop, which probably calculates CRC.  */
  class loop *crc_loop;

  unsigned HOST_WIDE_INT loop_iteration_number;

  /* Function's return value size.  */
  unsigned HOST_WIDE_INT return_size;

  /* Depending on the value of is_left_shift,
     may be forward or reversed CRC.
     If is_left_shift, then it is bit_forward implementations,
     otherwise bit_reversed.  */
  bool is_left_shift;

  /* Will be true, if crc variable and if condition depend on each other.  */
  bool crc_if_dep;

  /* If the value is false, then xor operation isn't for CRC calculation,
     otherwise it may calculate CRC.  */
  bool clean_xor_maybe_crc;

  void set_initial_values ()
  {
    shift_before_xor = nullptr;
    shift_after_xor = nullptr;
    first_phi_for_crc = nullptr;
    second_phi_for_crc = nullptr;
    data = nullptr;
    is_left_shift = false;
    crc_if_dep = false;
    clean_xor_maybe_crc = true;
  }

  /* This is the main function which checks whether given function
     calculates CRC and extracts the details of the CRC calculation.
     The main idea is to find innermost loop with 8, 16, 24, 32 iterations.
     Find xor in the loop (xor is the key operation for naive crc calculation).
     Check that before/after being xor-ed the variable is shifted by one.
     Xor must be done under condition of MSB/LSB being 1.  */
  bool function_may_calculate_crc (function *fun);

  /* Checks the loop iteration number.
     The loop for CRC calculation may do 8, 16, 24, 32 iterations.  */
  bool maybe_crc_iteration_count (class loop *func_loop);

  /* Finds and returns the block of a condition which is the destination
     block of the block containing the check of MSB/LSB being 1.
     If the block is found returns it, else returns nullptr.  */
  basic_block find_block_of_internal_condition (basic_block xor_bb);

  /* Check whether found xor_stmt is for calculating crc.
     The function crc_fun calculates crc only if there is a shift operation
     in the crc_loop.  */
  bool xor_calculates_crc (function *crc_fun, class loop *crc_loop,
			   const gimple *stmt);

  /* This function goes up through the def-chain of the name,
     until doesn't encounter a phi statement or
     if it does not meet certain conditions
     depending on the passed continue_to_check_dep function.
     The continue_to_check_dep may be the check_def_stmt_for_xor
     or check_def_stmt_for_if function.  */
  void
  get_dep (tree name,
	   bool (crc_optimization::*continue_to_check_dep) (gimple *stmt));

/* Checks whether the def_stmt statement, dependent from xor's operands,
   does shift operation for calculating crc
   or is a phi statement.  Keeps phi statements of the loop's header.
   Returns false, if there is an instruction which may not exist
   in the CRC loop.
   Returns true, if the def-chain examination must be continued.  */
  bool continue_to_check_dep_for_xor (gimple *stmt);

/* Checks whether if's condition and xor-ed variable
   are dependent from the same variable.
   (The crc variable is xor-ed if variable's MSB/LSB is 1).
   Also determines phi instruction's of data and crc
   (for further polynomial extraction).  */
  bool continue_to_check_dep_for_if (gimple *stmt);

  /* This function checks that xor is done under the condition
     of MSB/LSB being one.
     Checks that if condition's variable and xor-ed variable
     depend on same variable and if there is a shift 1 in the same block with
     xor, on the opposite branch must be another shift 1 to the same direction.
     xor_bb is the basic block, where xor operation is done.
     pred_bb is the predecessor basic block of the xor_bb,
     it is assumed that the last stmt of pred_bb checks the condition
     under which xor is done.  */
  bool
  crc_cond_and_shift (const basic_block &pred_bb, const basic_block &xor_bb);

  /* This function gets the condition cond and
     returns true if MSB/LSB bit is checked for 1.  */
  static bool cond_true_is_checked_for_bit_one (const gcond *cond);

  /* This functions checks whether the pair of xor's shift exists
     in the given bb.  If there is a shift with xor in the same block,
     then in the opposite block must be another shift.  */
  bool exists_shift_for_opp_xor_shift (basic_block bb);

  /* This function walks through the uses of xor-ed variable
     (within the crc_loop) and checks whether it is shifted one.
     Operations which may corrupt crc value mustn't occur between xor and shift,
     otherwise the loop isn't calculating CRC.  */
  void find_shift_after_xor (class loop *crc_loop, tree lhs);

  /* Checks whether assigment statement does shift operation
     (checks that shift 1 is done),
     if it doesn't - checks whether it is acceptable operation
     to be between shift and xor.
     find_shift_before_xor argument is for checking whether we search for shift
     before xor or after.  */
  bool can_not_be_shift_of_crc (gimple *assign_stmt,
				bool find_shift_before_xor);

  /* Checks whether it's ok that the statement is between shift and xor.
     This check is not that accurate.  But it's enough to filter not CRCs.  */
  static bool is_acceptable_statement (const tree_code &stmt_code);

  /* Get the return value size of the function
     and assign to return_size member.  */
  void set_return_value_size (function *crc_fun);

  /* This function checks whether calculated crc value
     (maybe modified) is returned.
     By walking down through the use-def chain of lhs
     return true if we encounter return statement.  */
  bool returned_value_depends_on_crc (tree lhs);

  /* Prints extracted details of CRC calculation.  */
  void print_crc_information ();

  /* Replaces function body with CRC_IFN call.
   Returns true if replacement is succeeded, otherwise false.  */
  bool
  faster_crc_code_generation (function *fun, unsigned HOST_WIDE_INT quotient);

 public:
  unsigned int execute (function *fun);
};


/* TODO: Move the function.
   Set GIMPLE_PHI and GIMPLE statements of the crc loop not visited.  */

void
set_loop_statements_not_visited (class loop *loop)
{
  // TODO: find faster way.
  basic_block *bbs = get_loop_body_in_dom_order (loop);
  for (unsigned int i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      /* Set phis not visited.  */
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *stmt = gsi.phi ();
	  gimple_set_visited (stmt, false);
	}

      /* Set statements not visited.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_visited (stmt, false);
	}
    }
  free (bbs);
}


/* TODO: Move the function.
   Set GIMPLE_PHI and GIMPLE statements of the function not visited.  */

static void
set_all_statements_not_visited (function *fun)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *stmt = gsi.phi ();
	  gimple_set_visited (stmt, false);
	}
      for (gimple_stmt_iterator gsi = gsi_start_nondebug_bb (bb);
	   !gsi_end_p (gsi); gsi_next_nondebug (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_visited (stmt, false);
	}
    }
}


/* Prints extracted details of CRC calculation.  */

void
crc_optimization::print_crc_information ()
{
  if (dump_file)
    {
      fprintf (dump_file,
	       "Loop iteration number is %ld.\n"
	       "Return size is %ld.\n",
	       loop_iteration_number, return_size);
      if (is_left_shift)
	fprintf (dump_file, "Bit forward.\n");
      else
	fprintf (dump_file, "Bit reversed.\n");
    }
}


/* This function checks whether calculated crc value
   (maybe modified) is returned.
   By walking down through the use-def chain of lhs
   return true if we encounter return statement.  */

bool
crc_optimization::returned_value_depends_on_crc (tree lhs)
{
  bool crc_is_returned = false;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  /* Iterate through the immediate uses of the current variable.
     If we encounter return statement - return true.  */
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
    {
      gimple *stmt = USE_STMT (use_p);
      if (gimple_visited_p (stmt))
	return false;

      gimple_set_visited (stmt, true);
      if (gimple_code (stmt) == GIMPLE_RETURN)
	return true;
      else if (gimple_code (stmt) == GIMPLE_PHI)
	crc_is_returned
	    = returned_value_depends_on_crc (gimple_phi_result (stmt));
      else if (is_gimple_assign (stmt))
	crc_is_returned
	    = returned_value_depends_on_crc (gimple_assign_lhs (stmt));
      if (crc_is_returned)
	return true;
    }
  return false;
}


/* Get the return value size of the function
   and assign to return_size member.  */

void
crc_optimization::set_return_value_size (function *crc_fun)
{
  return_size = 0;
  tree tree_return_value_size = DECL_SIZE (DECL_RESULT (crc_fun->decl));
  if (tree_fits_uhwi_p (tree_return_value_size))
    {
      return_size = tree_to_uhwi (tree_return_value_size);
    }
}


/* This function walks through the uses of xor-ed variable (within the crc_loop)
   and checks whether it is shifted one.
   Operations which may corrupt crc value mustn't occur between xor and shift,
   otherwise the loop isn't calculating CRC.  */

void
crc_optimization::find_shift_after_xor (class loop *crc_loop, tree lhs)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  if (TREE_CODE (lhs) != SSA_NAME)
    return;

  /* Iterate through the immediate uses of the current variable.
     If there is a shift return true,
     if before shift there is other instruction (besides phi) return false.  */
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
    {
      gimple *stmt = USE_STMT (use_p);
      if (gimple_visited_p (stmt))
	return;
      gimple_set_visited (stmt, true);
      // Consider only statements within the loop
      if (!flow_bb_inside_loop_p (crc_loop, gimple_bb (stmt)))
	return;

      /* If encountered phi statement, check immediate use of its result
	 otherwise, if encountered assign statement, check whether it does shift
	 (some other operations are allowed to be between shift and xor).  */
      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  /* Don't continue finding if encountered the loop's beginning.  */
	  if (bb_loop_header_p (gimple_bb (stmt)))
	    return;

	  find_shift_after_xor
	      (crc_loop, gimple_phi_result (stmt));
	}
      else if (is_gimple_assign (stmt)
	       && can_not_be_shift_of_crc (stmt, false))
	{
	  shift_after_xor = nullptr;
	  return;
	}
    }
}


/* This function checks whether the pair of xor's shift exists in the given bb.

   If there is a shift with xor in the same block,
   then in the opposite block must be another shift.  */

bool
crc_optimization::exists_shift_for_opp_xor_shift (basic_block bb)
{
  /* Walk through the instructions of given basic block.  */
  for (gimple_stmt_iterator bsi = gsi_start_nondebug_bb (bb);
       !gsi_end_p (bsi); gsi_next_nondebug (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);
      /* Find assigment statement with shift operation.
	 Check that shift's direction is same with the shift done
	 on the path with xor.  */
      if (is_gimple_assign (stmt))
	{
	  if (gimple_assign_rhs_code (stmt) == LSHIFT_EXPR && is_left_shift)
	    return true;
	  else if (gimple_assign_rhs_code (stmt) == RSHIFT_EXPR
		   && !is_left_shift)
	    return true;
	}
    }
  /* If there is no shift, return false.  */
  return false;
}


/* This function gets the condition cond and
   returns true if MSB/LSB bit is checked for 1.  */

bool
crc_optimization::cond_true_is_checked_for_bit_one (const gcond *cond)
{
  if (!cond)
    return false;
  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);
  enum tree_code code = gimple_cond_code (cond);

  /* If the condition is
     something == 1 or 1 == something -> return true.  */
  if ((integer_onep (lhs) || integer_onep (rhs)) && code == EQ_EXPR)
    return true;

  /* If the condition is
     0 != something  or 0 > something -> return true.  */
  if (integer_zerop (lhs) && (code == NE_EXPR || code == GT_EXPR))
    return true;

  /* If the condition is
     something != 0  or something < 0 -> return true.  */
  if (integer_zerop (rhs) && (code == NE_EXPR || code == LT_EXPR))
    return true;

  return false;
}


/* This function checks that xor is done under the condition
   of MSB/LSB being one.
   Checks that if condition's variable and xor-ed variable
   depend on same variable and if there is a shift1 in the same block with xor,
   on the opposite branch must be another shift1 to the same direction.

   xor_bb is the basic block, where xor operation is done.
   pred_bb is the predecessor basic block of the xor_bb,
   it is assumed that the last stmt of pred_bb checks the condition
   under which xor is done.  */

bool
crc_optimization::crc_cond_and_shift (const basic_block &pred_bb,
				      const basic_block &xor_bb)
{
  gcond *cond = nullptr;
  /* Check whether pred_bb contains condition.
     We will consider only those cases
     when xor is done immediately under condition.  */
  if (is_a<gcond *> (last_stmt (pred_bb)))
    cond = as_a<gcond *> (last_stmt (pred_bb));
  if (!cond)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "No condition.  Not crc.\n");
      return false;
    }

  edge true_edge;
  edge false_edge;
  extract_true_false_edges_from_block (pred_bb, &true_edge, &false_edge);

  basic_block xor_opposite_block;
  /* Check that xor is done in case the MSB/LSB is 1.  */
  if ((cond_true_is_checked_for_bit_one (cond) && true_edge->dest == xor_bb))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Xor is done on true branch.\n");

      xor_opposite_block = false_edge->dest;
    }
  else if (!cond_true_is_checked_for_bit_one (cond)
	   && false_edge->dest == xor_bb)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Xor is done on false branch.\n");

      xor_opposite_block = true_edge->dest;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Xor is done if MSB/LSB is not one, not crc.\n");

      return false;
    }

  /* Check whether there is a dependence between
     if condition's variable and xor-ed variable.
     Also determine phi statements of crc and data.  */
  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);
  set_loop_statements_not_visited (crc_loop);
  if (TREE_CODE (lhs) == SSA_NAME)
    get_dep (lhs, &crc_optimization::continue_to_check_dep_for_if);
  else if (TREE_CODE (rhs) == SSA_NAME)
    get_dep (rhs, &crc_optimization::continue_to_check_dep_for_if);
  else
    return false; /* Both parts of condition are not SSA_NAME.  */

  /* Return false if there is no dependence between if condition's variable
     and xor-ed variable.  */
  if (!crc_if_dep)
    return false;

  /* If the found shift is in the same block with xor,
  check whether another shift exists in the opposite block.  */
  if ((shift_before_xor && gimple_bb (shift_before_xor) == xor_bb)
      || (shift_after_xor && gimple_bb (shift_after_xor) == xor_bb))
    return exists_shift_for_opp_xor_shift (xor_opposite_block);
  return true;
}


/* Checks whether it's ok that the statement is between shift and xor.
  This check is not that accurate.  But it's enough to filter not CRCs.  */

bool
crc_optimization::is_acceptable_statement (const tree_code &stmt_code)
{
  return stmt_code == BIT_IOR_EXPR
	 || stmt_code == BIT_AND_EXPR
	 || stmt_code == BIT_XOR_EXPR
	 || stmt_code == MINUS_EXPR
	 || stmt_code == PLUS_EXPR
	 || TREE_CODE_CLASS (stmt_code) == tcc_unary;
}


/* Checks whether assigment statement does shift operation
   (checks that shift 1 is done),
   if it doesn't - checks whether it is acceptable operation
   to be between shift and xor.
   find_shift_before_xor argument is for checking whether we search for shift
   before xor or after.  */

bool
crc_optimization::can_not_be_shift_of_crc (gimple *assign_stmt,
					   bool find_shift_before_xor)
{
  tree_code stmt_code = gimple_assign_rhs_code (assign_stmt);
  if (stmt_code == LSHIFT_EXPR || stmt_code == RSHIFT_EXPR)
    {
      is_left_shift = (stmt_code == LSHIFT_EXPR);
      /* Check that shift one is done,
	 keep shift statement.  */
      if (integer_onep (gimple_assign_rhs2 (assign_stmt)))
	{
	  if (find_shift_before_xor)
	    {
	      if (shift_before_xor)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "Already there is one shift "
			     "on the path to xor, not crc.\n");

		  clean_xor_maybe_crc = false;
		  return true;
		}
	      shift_before_xor = assign_stmt;
	    }
	  else
	    {
	      shift_after_xor = assign_stmt;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "Found <<1 or >>1 after xor.\n");
	    }
	  return false;
	}
    }
    /* No need for more strict checks,
       not CRCs may be filtered by the verification stage.  */
  else if (!is_acceptable_statement (stmt_code))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "\nStmt with the following operation "
		 "code %s between xor and shift, "
		 "may not be CRC.\n", get_tree_code_name (stmt_code));

      return true;
    }
  return false;
}


/* This function goes up through the def-chain of the given name,
   until doesn't encounter a phi statement or
   if it does not meet certain conditions
   depending on the passed continue_to_check_dep function.
   The continue_to_check_dep may be the check_def_stmt_for_xor
   or check_def_stmt_for_if function.  */

void
crc_optimization::get_dep (tree name,
			   bool (crc_optimization::*continue_to_check_dep) (
			       gimple *ssa))
{
  if (!(name && TREE_CODE (name) == SSA_NAME))
    return;

  /* No definition chain for default defs.  */
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    return;

  gimple *stmt = SSA_NAME_DEF_STMT (name);

  if (!stmt)
    return;

  /* Don't go outside the loop.  */
  if (!flow_bb_inside_loop_p (crc_loop, gimple_bb (stmt)))
    return;

  /* Check the statement, extract important information.
     Stop examining def-chain if there is no need of other information.  */
  if (!((this->*continue_to_check_dep) (stmt)))
    return;

  /* If it is an assigment statement,
     get and check def-chain for the first and second operands.
     Otherwise if it's a phi statement, not declared in loop's header,
     get and check def-chain for its values.  */
  if (is_a<gassign *> (stmt))
    {
      tree ssa1 = gimple_assign_rhs1 (stmt);
      tree ssa2 = gimple_assign_rhs2 (stmt);

      get_dep (ssa1, continue_to_check_dep);
      get_dep (ssa2, continue_to_check_dep);
    }
  else if (is_a<gphi *> (stmt) && !bb_loop_header_p (gimple_bb (stmt)))
    {
      for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
	{
	  tree val = gimple_phi_arg_def (stmt, i);
	  get_dep (val, continue_to_check_dep);
	}
    }
}


/* Checks whether the def_stmt statement, dependent from xor's operands,
   does shift operation for calculating crc
   or is a phi statement.  Keeps phi statements of the loop's header.

   Returns false, if there is an instruction which may not exist
   in the CRC loop.
   Returns true, if the def-chain examination must be continued.  */

bool
crc_optimization::continue_to_check_dep_for_xor (gimple *def_stmt)
{

  if (!clean_xor_maybe_crc)
    return false;

  if (gimple_visited_p (def_stmt))
    return false;

  gimple_set_visited (def_stmt, true);

  if (is_gimple_assign (def_stmt))
    {
      if (can_not_be_shift_of_crc (def_stmt, true))
	{
	  clean_xor_maybe_crc = false;
	  return false;
	}
    }
  else if (is_a<gphi *> (def_stmt))
    {
      /* Keep phi statement.  */
      if (bb_loop_header_p (gimple_bb (def_stmt)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Phi's definition is in the loop header.\n");

	  /* The case when polynomial's value is determined by
	     a phi statement.  */
	  if (first_phi_for_crc)
	    {
	      second_phi_for_crc = as_a<gphi *> (def_stmt);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Set second phi.\n");
	    }
	  else
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Set first phi.\n");
	      first_phi_for_crc = as_a<gphi *> (def_stmt);
	    }
	  return true;
	}
    }
  return true;
}


/* Checks whether if's condition and xor-ed variable
   are dependent from the same variable.
   (The crc variable is xor-ed if variable's MSB/LSB is 1).

   Also determines phi instruction's of data and crc
   (for further polynomial extraction).  */

bool
crc_optimization::continue_to_check_dep_for_if (gimple *def_stmt)
{
  gimple_set_visited (def_stmt, true);

  if (is_a<gphi *> (def_stmt) && bb_loop_header_p (gimple_bb (def_stmt)))
    {
      /* Checks whether if's condition and xor-ed variable are dependent
	 from the same variable.  */
      if ((first_phi_for_crc && first_phi_for_crc == def_stmt)
	  || (second_phi_for_crc && second_phi_for_crc == def_stmt))
	{
	  crc_if_dep = true;
	  /* If the second phi equals def_stmt,
	     then it is used for crc calculation,
	     keep crc phi in first_phi_for_crc.
	     (This is needed for the polynomial extraction.)  */
	  if ((second_phi_for_crc && second_phi_for_crc == def_stmt))
	    {
	      first_phi_for_crc = second_phi_for_crc;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "The condition of if statement has dependence "
		       "from the same variable as xor.\n"
		       "CRC's phi statement is:\n");
	      print_gimple_stmt (dump_file, def_stmt, dump_flags);
	    }
	}
	/* If we encounter a phi statement which isn't the crc,
	   then it may be the data.
	   (This is needed for the polynomial extraction.)  */
      else
	{
	  /* Cond statement for crc calculation
	     may depend only on data and crc.
	     If it doesn't then it's not for crc calculation.  */
	  if (data && data != def_stmt)
	    {
	      crc_if_dep = false;
	      return false;
	    }

	  data = as_a<gphi *> (def_stmt);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "Found data's phi statement:\n");
	      print_gimple_stmt (dump_file, def_stmt, dump_flags);
	    }
	}
    }
  return true;
}

/* In the case when xor's block doesn't have single predecessor,
   we need to check whether it's parent blocks have single predecessor,
   and it's the same block for both predecessor blocks.
   If it is, most probably that block's predecessor may contain the check
   for MSB/LSB being one.

   This function searches the block of a condition which is the destination
   block of the block containing the check of MSB/LSB being 1.
   If the block is found returns it, else returns nullptr.

   Here we can detect implementations like the following:
   if ((crc & 0x80) != 0)
   {
     crc = (byte)(crc << 1);
     crc = ((byte)(b & (1 << i)) != 0) //block_of_internal_condition
	   ? (byte)(crc | 0x01) : (byte)(crc & 0xFE);
     crc = (byte)(crc ^ generator);
   }
   else
   {
     crc = (byte)(crc << 1);
     crc = ((byte)(b & (1 << i)) != 0)
	   ? (byte)(crc | 0x01) : (byte)(crc & 0xFE);
   }
*/

basic_block
crc_optimization::find_block_of_internal_condition (basic_block xor_bb)
{
  basic_block parent_bb = EDGE_PRED (xor_bb, 0)->src;
  basic_block block_of_internal_condition = EDGE_PRED (parent_bb, 0)->src;

  for (size_t i = 1; i < EDGE_COUNT (xor_bb->preds); ++i)
    {
      edge e = EDGE_PRED (xor_bb, i);
      if (!(single_pred_p (e->src)
      && EDGE_PRED (e->src, 0)->src == block_of_internal_condition))
	return nullptr;
    }
    return block_of_internal_condition;
}

/* Check whether found xor_stmt is for calculating crc.
   The function crc_fun calculates crc only if there is a shift operation
   in the crc_loop.  */

bool
crc_optimization::xor_calculates_crc (function *crc_fun, class loop *crc_loop,
				      const gimple *xor_stmt)
{
  tree crc_var = gimple_assign_lhs (xor_stmt);
  set_initial_values ();
  set_loop_statements_not_visited (crc_loop);
  get_dep (crc_var,
	   &crc_optimization::continue_to_check_dep_for_xor);

  if (!clean_xor_maybe_crc)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Xor isn't used for CRC calculation.\n");
      return false;
    }

  /* Check the case when shift is done after xor.  */
  if (!shift_before_xor)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "No shift before xor, trying to find after xor.\n");

      // TODO: It's time consuming to get loop's body,
      //  (set_all_statements_not_visited may be time consuming too).
      //  Do it better.
      set_loop_statements_not_visited (crc_loop);

      find_shift_after_xor (crc_loop, crc_var);
      if (!shift_after_xor)
	return false;
    }

  /* Check that xor is done if MSB/LSB is one.
     In presence of shift in the same loop with xor,
     check for its pair in another branch.
     If all checks succeed, then it is a crc.  */
  basic_block bb = gimple_bb (xor_stmt);
  if (!single_pred_p (bb))
    {
      bb = find_block_of_internal_condition (bb);
      if (!bb || !single_pred_p (bb))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Xor bb doesn't have single predecessor.\n");
	  return false;
	}
    }

  basic_block block_of_condition = single_pred (bb);

  if (crc_cond_and_shift (block_of_condition, bb))
    {
      set_all_statements_not_visited (crc_fun);
      bool crc_is_returned = returned_value_depends_on_crc (crc_var);
      if (dump_file)
	{
	  if (crc_is_returned)
	    {
	      fprintf (dump_file,
		       "\n%s function maybe calculates CRC "
		       "and returns it.\n",
		       function_name (crc_fun));
	    }
	  else
	    {
	      fprintf (dump_file,
		       "\n%s function maybe calculates CRC.\n",
		       function_name (crc_fun));
	    }
	}
      return true;
    }

  return false;
}


/* Checks the loop iteration number.
   The loop for CRC calculation may do 8, 16, 24, 32 iterations.  */

bool
crc_optimization::maybe_crc_iteration_count (class loop *func_loop)
{
  loop_iteration_number = 0;
  tree n_inters = number_of_latch_executions (func_loop);
  if (n_inters == NULL_TREE || n_inters == chrec_dont_know)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop iteration number is chrec_dont_know.\n");
      return false;

    }
  else if (tree_fits_uhwi_p (n_inters))
    {
      loop_iteration_number = tree_to_uhwi (n_inters);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Loop iteration number is %ld.\n",
		 loop_iteration_number);

      if (!(loop_iteration_number == 7 || loop_iteration_number == 15
	    || loop_iteration_number == 23 || loop_iteration_number == 31
	    || loop_iteration_number == 63))
	return false;
    }
  return true;
}


/* This is the main function which checks whether given function calculates CRC
   and extracts the details of the CRC calculation.
   The main idea is to find innermost loop with 8, 16, 24, 32 iterations.
   Find xor in the loop (xor is the key operation for naive crc calculation).
   Check that before/after being xor-ed the variable is shifted by one.
   Xor must be done under condition of MSB/LSB being 1.  */

bool
crc_optimization::function_may_calculate_crc (function *fun)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nExamining %s function.\n",
	     function_name (fun));

  if (number_of_loops (fun) <= 1)
    return false;

  /* Get loops of the function.  */
  auto loop_list = loops_list (fun, LI_ONLY_INNERMOST);
  for (auto loop: loop_list)
    {
      /* Only examine innermost loops.  */
      if (!loop || loop->inner)
	continue;

      if (!maybe_crc_iteration_count (loop))
	continue;

      crc_loop = loop;
      basic_block *bbs = get_loop_body_in_dom_order (loop);
      /* Walk bbs of the loop.  */
      for (unsigned int i = 0; i < loop->num_nodes; i++)
	{
	  basic_block bb = bbs[i];
	  /* Walk instructions of bb.  */
	  for (gimple_stmt_iterator bsi = gsi_start_nondebug_bb (bb);
	       !gsi_end_p (bsi); gsi_next_nondebug (&bsi))
	    {
	      gimple *stmt = gsi_stmt (bsi);
	      /* If there is an xor instruction,
		 check that it is calculating crc.  */
	      if (is_gimple_assign (stmt)
		  && gimple_assign_rhs_code (stmt) == BIT_XOR_EXPR)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "Found xor, "
			     "checking whether it is for crc calculation.\n");

		  if (xor_calculates_crc (fun, loop, stmt))
		    {
		      set_return_value_size (fun);
		      print_crc_information ();
		      free (bbs);
		      return true;
		    }
		}
	    }
	}
      free (bbs);
    }
  return false;
}

/* Remove all statements and basic blocks of the function
   except for the return statement.  */
gimple *
remove_stmts_besides_return (function *fun, class loop *crc_loop)
{
  gimple_stmt_iterator gsi;
  basic_block bb, next_bb;
  gimple *return_stmt = NULL;
  cancel_loop_tree (crc_loop);
  for (bb = fun->cfg->x_entry_block_ptr->next_bb;
       bb != fun->cfg->x_exit_block_ptr; bb = next_bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *gs = gsi_stmt (gsi);
	  // TODO: Check whether it's better to get return
	  //  statement during symbolic execution step and change.
	  if (gimple_code (gs) == GIMPLE_RETURN)
	    return_stmt = gs;
	}

      next_bb = bb->next_bb;
      // TODO: what if return statement's block isn't the last one?!
      if (!return_stmt)
	delete_basic_block (bb);
      else
	{
	  // TODO: Check whether there can be other statements
	  // besides return in the block.  Delete them if any.
	  /* Delete phi statements of the return block.  */
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);)
	    gsi_remove (&gsi, true);
	}
    }
  make_edge (fun->cfg->x_entry_block_ptr, return_stmt->bb,
	     EDGE_FALLTHRU);
  return return_stmt;
}

/* Replaces function body with CRC_IFN call.
   Returns true if replacement is succeeded, otherwise false.  */
bool
crc_optimization::faster_crc_code_generation (function *fun,
					      unsigned HOST_WIDE_INT quotient)
{
  if (!(data && first_phi_for_crc))
   return false;
  // TODO: Use another way of figuring out which argument is data, as this won't
  // work correctly in case when data is xor-ed with crc before the loop.
  tree crc_arg = PHI_ARG_DEF (first_phi_for_crc, 1);
  tree data_arg = PHI_ARG_DEF (data, 1);
  tree quotient_arg = build_int_cst (TREE_TYPE (crc_arg), quotient);

  if (!direct_internal_fn_supported_p
      (IFN_CRC, tree_pair (TREE_TYPE (data_arg), TREE_TYPE (crc_arg)),
       OPTIMIZE_FOR_BOTH))
  {
    if (dump_file)
      fprintf (dump_file, "No matching optab entry\n");
    return false;
  }

  gimple *return_stmt = remove_stmts_besides_return (fun, crc_loop);

  /* Add IFN call and return the value.  */
  gcall *call
      = gimple_build_call_internal (IFN_CRC, 3,
				    crc_arg,
				    data_arg,
				    quotient_arg);

  tree result = make_ssa_name (TREE_TYPE (DECL_RESULT (fun->decl)));
  gimple_call_set_lhs (call, result);
  gimple_set_location (call, fun->function_start_locus);
  gimple_stmt_iterator si = gsi_start_bb (return_stmt->bb);
  gsi_insert_before (&si, call, GSI_SAME_STMT);
  use_operand_p imm_use_p;
  imm_use_iterator iterator;
  gimple *stmt;
  FOR_EACH_IMM_USE_STMT (stmt, iterator,
			 gimple_return_retval (as_a<greturn *> (return_stmt)))
    {
      FOR_EACH_IMM_USE_ON_STMT (imm_use_p, iterator)
	SET_USE (imm_use_p, result);
      update_stmt (stmt);
    }

  /* Fix up CFG.  */
  remove_unused_locals ();
  scev_reset ();
  mark_virtual_operands_for_renaming (fun);
  free_dominance_info (fun, CDI_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  return true;
}

/* Calculate the quotient of polynomial long division of x^2n by the polynomial
   in GF (2^n).  */
unsigned HOST_WIDE_INT
gf2n_poly_long_div_quotient (value *polynomial, bool is_left_shift)
{
  vec<int> x2n, pol, q;
  size_t n = (*polynomial).length () * 2 + 1, m = (*polynomial).length () + 1;
  x2n.create (n);
  pol.create (m);

  for (size_t i = 0; i < (*polynomial).length (); i++)
    {
      value_bit *const_bit;
      if (is_left_shift)
	const_bit = (*polynomial)[i];
      else
	const_bit = (*polynomial)[(*polynomial).length () - 1 - i];
      pol.quick_push ((int) (as_a<bit *> (const_bit))->get_val ());
    }

  pol.quick_push (1);

  for (size_t i = 0; i < n - 1; i++)
    x2n.quick_push (0);
  x2n.quick_push (1);

  q.create (n - m + 1);
  for (size_t i = 0; i < n - m + 1; i++)
    q.quick_push (0);

  for (int i = n - m; i >= 0; i--)
    {
      int d = x2n[i + m - 1];
      if (d == 0)
	continue;
      for (int j = i + m - 1; j >= i; j--)
	x2n[j] = x2n[j] ^ (pol[j - i] * d);
      q[i] = d;
    }
  unsigned HOST_WIDE_INT quotient = 0;
  for (size_t i = 0; i < q.length (); i++)
    {
      quotient <<= 1;
      quotient = quotient | q[q.length () - i - 1];
    }

  return quotient;
}

unsigned int
crc_optimization::execute (function *fun)
{
  if (function_may_calculate_crc (fun))
    {
      crc_symbolic_execution symbolically_execute_function;
      if (!symbolically_execute_function.execute (fun))
	{
	  if (dump_file)
	    fprintf (dump_file, "\nCRC verification didn't succeed "
				"during symbolic execution!\n");
	  return 0;
	}

      crc_symbolic_execution execute_loop;
      std::pair <tree, value *>
	calc_polynom = execute_loop.extract_polynomial (crc_loop,
						       first_phi_for_crc,
						       data, is_left_shift);

      if (!calc_polynom.second)
	return 0;

      unsigned HOST_WIDE_INT
      quotient = gf2n_poly_long_div_quotient (calc_polynom.second,
					      is_left_shift);

      value *lfsr
	= state::create_lfsr (calc_polynom.first,
			      calc_polynom.second, is_left_shift);
      if (!lfsr)
	{
	  if (dump_file)
	    fprintf (dump_file, "Couldn't create LFSR!\n");
	  return 0;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nLFSR value is \n");
	  state::print_value (lfsr);
	}

      if (all_states_match_lfsr (lfsr, is_left_shift,
				 symbolically_execute_function
				     .get_final_states ()))
	{
	  if (dump_file)
	    fprintf (dump_file, "%s function calculates CRC!\n",
		     function_name (fun));
	  if (!faster_crc_code_generation (fun, quotient))
	    {
	      if (dump_file)
		fprintf (dump_file, "Couldn't generate faster CRC code.\n");
	    }
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Returned state and LFSR differ.\n");
	}

      delete lfsr;
    }
  return 0;
}

namespace
{

    const pass_data pass_data_crc_optimization
	= {
	    GIMPLE_PASS, /* type */
	    "crc", /* name */
	    OPTGROUP_NONE, /* optinfo_flags */
	    TV_GIMPLE_CRC_OPTIMIZATION, /* tv_id */
	    (PROP_cfg | PROP_ssa), /* properties_required */
	    0, /* properties_provided */
	    0, /* properties_destroyed */
	    0, /* todo_flags_start */
	    0, /* todo_flags_finish */
	};

    class pass_crc_optimization : public gimple_opt_pass {
     public:
      pass_crc_optimization (gcc::context *ctxt)
	  : gimple_opt_pass (pass_data_crc_optimization, ctxt)
      {}

      /* opt_pass methods: */
      virtual bool gate (function *)
      {
	return flag_gimple_crc_optimization;
      }

      virtual unsigned int execute (function *);

    }; // class pass_crc_optimization

    unsigned int
    pass_crc_optimization::execute (function *fun)
    {
      return crc_optimization ().execute (fun);
    }

} // anon namespace

gimple_opt_pass *
make_pass_crc_optimization (gcc::context *ctxt)
{
  return new pass_crc_optimization (ctxt);
}
