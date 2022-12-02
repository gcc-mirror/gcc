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
#include "symb-execute-all-paths.h"

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
  gphi * data;

  /* The loop, which probably calculates CRC.  */
  loop *crc_loop;

  unsigned HOST_WIDE_INT loop_iteration_number;

  /* Function's return value size.  */
  unsigned HOST_WIDE_INT return_size;

  /* Depending on the value, may be forward or reversed CRC.  */
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
  bool is_loop_of_crc_calculation (class loop *func_loop);

  /* Check whether found xor_stmt is for calculating crc.
   The function fun calculates crc only if there is a shift operation
   in the crc_loop.  */
  bool xor_calculates_crc (function *fun, class loop *crc_loop,
			   const gimple *stmt);

  /* This function goes up through the def-chain of the name,
   until doesn't encounter a phi statement or
   if it does not meet certain conditions
   depending on the passed continue_to_check_dep function.
   The continue_to_check_dep may be the check_def_stmt_for_xor
   or check_def_stmt_for_if function.  */
  void
  get_dep (tree name,
	   bool (crc_optimization::*continue_to_check_dep) (gimple* stmt));

/* Checks whether the def_stmt statement, dependent from xor's operands,
   does shift operation for calculating crc
   or is a phi statement.  Keeps phi statements of the loop's header.
   Returns false, if there is an instruction which may not exist
   in the CRC loop.
   Returns true, if the def-chain examination must be continued.  */
  bool continue_to_check_dep_for_xor (gimple* stmt);

/* Checks whether if's condition and xor-ed variable
   are dependent from the same variable.
   (The crc variable is xor-ed if variable's MSB/LSB is 1).
   Also determines phi instruction's of data and crc
   (for further polynomial extraction).  */
  bool continue_to_check_dep_for_if (gimple* stmt);

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

  bool can_not_be_shift_of_crc (gimple *assign_stmt,
				bool find_shift_before_xor);

  /* Checks whether it's ok that the statement is between shift and xor.
     This check is not that accurate.  But it's enough to filter not CRCs.  */
  static bool is_acceptable_statement (const tree_code &stmt_code);

  /* Get the return value size of the function
     and assign to return_size member.  */
  void set_return_value_size (function *fun);

  /* This function checks whether calculated crc value
     (maybe modified) is returned.
     By walking down through the use-def chain of lhs
     return true if we encounter return statement.  */
  bool returned_value_depends_on_crc (tree lhs);

  /* Prints extracted details of CRC calculation.  */
  void print_crc_information ();

 public:
  unsigned int execute (function *fun);
};


/* Set GIMPLE_PHI and GIMPLE statements of the crc loop not visited.  */
void
set_loop_statements_not_visited (loop *loop)
{
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
}

/* Set GIMPLE_PHI and GIMPLE statements of the function not visited.  */
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
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
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
crc_optimization::set_return_value_size (function *fun)
{
  return_size = 0;
  tree tree_return_value_size = DECL_SIZE (DECL_RESULT (fun->decl));
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
      else if (is_gimple_assign (stmt))
	if (can_not_be_shift_of_crc (stmt, false))
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
  for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (
      bsi); gsi_next (
      &bsi))
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
			 "Found shift1 after xor.\n");
	    }
	  return false;
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Found shift, but not with 1, not crc.\n");
	  clean_xor_maybe_crc = false;
	  return true;
	}

    }
    /* No need for more strict checks,
       not CRCs may be filtered by the verification stage.  */
  else if (!is_acceptable_statement (stmt_code))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "\nStmt with following operation "
		 "code %s between xor and shift, "
		 "may not be crc.\n", get_tree_code_name (stmt_code));

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
			       gimple* ssa))
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
		     "Phi's definition is in loop header.\n");

	  /* The case when polynomial's value is determined by
	     a phi statement.  */
	  if (first_phi_for_crc)
	    {
	      second_phi_for_crc = as_a <gphi *> (def_stmt);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Set second phi.\n");
	    }
	  else
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Set first phi.\n");
	      first_phi_for_crc = as_a <gphi *> (def_stmt);
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
		       "If condition has dependence "
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

	  data = as_a <gphi *> (def_stmt);
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


/* Check whether found xor_stmt is for calculating crc.
   The function fun calculates crc only if there is a shift operation
   in the crc_loop.  */

bool
crc_optimization::xor_calculates_crc (function *fun, class loop *crc_loop,
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
	fprintf (dump_file, "Xor doesn't calculate crc.\n");
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
  if (single_pred_p (bb))
    {
      if (crc_cond_and_shift (single_pred (bb), bb))
	{
	  set_all_statements_not_visited (fun);
	  bool crc_is_returned = returned_value_depends_on_crc (crc_var);
	  if (dump_file)
	    {
	      if (crc_is_returned)
		{
		  fprintf (dump_file,
			   "\nAttention! %s function calculates CRC.\n",
			   function_name (fun));
		}
	      else
		{
		  fprintf (dump_file,
			   "\nFound naive crc implementation in %s.\n",
			   function_name (fun));
		}
	    }
	  return true;
	}
    }
  else
    {
      if (dump_file)
	fprintf (dump_file,
		 "Xor bb doesn't have single predecessor.\n");
    }
  return false;
}


/* Checks the loop iteration number.
   The loop for CRC calculation may do 8, 16, 24, 32 iterations.  */

bool
crc_optimization::is_loop_of_crc_calculation (class loop *func_loop)
{
  loop_iteration_number = 0;
  tree n_inters = number_of_latch_executions (func_loop);
  if (n_inters == NULL_TREE || n_inters == chrec_dont_know)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop iteration number is chrec_dont_know.\n");

    }
  else if (tree_fits_uhwi_p (n_inters))
    {
      loop_iteration_number = tree_to_uhwi (n_inters);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Loop iteration number is %ld.\n",
		 loop_iteration_number);

      if (!(loop_iteration_number == 7 || loop_iteration_number == 15
	    || loop_iteration_number == 23 || loop_iteration_number == 31))
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

      if (!is_loop_of_crc_calculation (loop))
	continue;

      crc_loop = loop;
      basic_block *bbs = get_loop_body_in_dom_order (loop);
      /* Walk bbs of the loop.  */
      for (unsigned int i = 0; i < loop->num_nodes; i++)
	{
	  basic_block bb = bbs[i];
	  /* Walk instructions of bb.  */
	  for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (
	      bsi); gsi_next (&bsi))
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
		      return true;
		    }
		}
	    }
	}
    }
  return false;
}

unsigned int
crc_optimization::execute (function *fun)
{
  if (function_may_calculate_crc (fun))
  {
    crc_symb_execution symb_exec;
    if (!symb_exec.execute_function (fun))
      {
	if (dump_file)
	  fprintf (dump_file, "\nAttention! Not the CRC we want!\n");
	return 0;
      }

      crc_symb_execution execute_loop;
      vec<value*> * polynomial
      = execute_loop.extract_polynomial (crc_loop, first_phi_for_crc,
					 data, is_left_shift);

    if (!polynomial)
      {
	if (dump_file)
	  fprintf (dump_file, "\nCouldn't determine the polynomial!\n");
	return 0;
      }

    /* TODO: Create LFSR state.  */

    /* TODO: Match LFSR.  */
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
