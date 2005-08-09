/* Global, SSA-based optimizations using mathematical identities.
   Copyright (C) 2005 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Currently, the only mini-pass in this file tries to CSE reciprocal
   operations.  These are common in sequences such as this one:

	modulus = sqrt(x*x + y*y + z*z);
	x = x / modulus;
	y = y / modulus;
	z = z / modulus;

   that can be optimized to

	modulus = sqrt(x*x + y*y + z*z);
        rmodulus = 1.0 / modulus;
	x = x * rmodulus;
	y = y * rmodulus;
	z = z * rmodulus;

   We do this for loop invariant divisors, and with this pass whenever
   we notice that a division has the same divisor multiple times.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "tree.h"
#include "tree-flow.h"
#include "real.h"
#include "timevar.h"
#include "tree-pass.h"

static bool
gate_cse_reciprocals (void)
{
  return optimize && !optimize_size && flag_unsafe_math_optimizations;
}

/* Where to put the statement computing a reciprocal.  */
enum place_reciprocal
{
  PR_BEFORE_BSI,	/* Put it using bsi_insert_before.  */
  PR_AFTER_BSI,		/* Put it using bsi_insert_after.  */
  PR_ON_ENTRY_EDGE	/* Put it on the edge between the entry
			   and the first basic block.  */
};

/* Check if DEF's uses include more than one floating-point division,
   and if so replace them by multiplications with the reciprocal.  Add
   the statement computing the reciprocal according to WHERE.

   Does not check the type of DEF, nor that DEF is a GIMPLE register.
   This is done in the caller for speed, because otherwise this routine
   would be called for every definition and phi node.  */
static void
execute_cse_reciprocals_1 (block_stmt_iterator *bsi, tree def,
			   enum place_reciprocal where)
{
  use_operand_p use_p;
  imm_use_iterator use_iter;
  tree t, new_stmt, type;
  int count = 0;
  bool ok = !flag_trapping_math;

  /* Find uses.  */
  FOR_EACH_IMM_USE_FAST (use_p, use_iter, def)
    {
      tree use_stmt = USE_STMT (use_p);
      if (TREE_CODE (use_stmt) == MODIFY_EXPR
	  && TREE_CODE (TREE_OPERAND (use_stmt, 1)) == RDIV_EXPR
	  && TREE_OPERAND (TREE_OPERAND (use_stmt, 1), 1) == def)
        {
          ++count;
          /* Check if this use post-dominates the insertion point.  */
          if (ok || dominated_by_p (CDI_POST_DOMINATORS, bsi->bb,
				    bb_for_stmt (use_stmt)))
	    ok = true;
        }
      if (count >= 2 && ok)
        break;
    }

  if (count < 2 || !ok)
    return;

  /* Make a variable with the replacement and substitute it.  */
  type = TREE_TYPE (def);
  t = make_rename_temp (type, "reciptmp");
  new_stmt = build2 (MODIFY_EXPR, void_type_node, t,
		     fold_build2 (RDIV_EXPR, type, build_real (type, dconst1),
				  def));

  if (where == PR_BEFORE_BSI)
    bsi_insert_before (bsi, new_stmt, BSI_SAME_STMT);
  else if (where == PR_AFTER_BSI)
    bsi_insert_after (bsi, new_stmt, BSI_NEW_STMT);
  else if (where == PR_ON_ENTRY_EDGE)
    bsi_insert_on_edge (single_succ_edge (ENTRY_BLOCK_PTR), new_stmt);
  else
    gcc_unreachable ();

  FOR_EACH_IMM_USE_SAFE (use_p, use_iter, def)
    {
      tree use_stmt = USE_STMT (use_p);
      if (use_stmt != new_stmt
	  && TREE_CODE (use_stmt) == MODIFY_EXPR
	  && TREE_CODE (TREE_OPERAND (use_stmt, 1)) == RDIV_EXPR
	  && TREE_OPERAND (TREE_OPERAND (use_stmt, 1), 1) == def)
	{
	  TREE_SET_CODE (TREE_OPERAND (use_stmt, 1), MULT_EXPR);
	  SET_USE (use_p, t);
	}
    }
}

static void
execute_cse_reciprocals (void)
{
  basic_block bb;
  tree arg;

  if (flag_trapping_math)
    calculate_dominance_info (CDI_POST_DOMINATORS);

  if (single_succ_p (ENTRY_BLOCK_PTR))
    for (arg = DECL_ARGUMENTS (cfun->decl); arg; arg = TREE_CHAIN (arg))
      if (default_def (arg))
	{
	  block_stmt_iterator bsi;
	  bsi = bsi_start (single_succ (ENTRY_BLOCK_PTR));
	  execute_cse_reciprocals_1 (&bsi, default_def (arg),
				     PR_ON_ENTRY_EDGE);
	}

  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi;
      tree phi, def;
      for (bsi = bsi_start (bb);
	   !bsi_end_p (bsi) && TREE_CODE (bsi_stmt (bsi)) == LABEL_EXPR;
	   bsi_next (&bsi))
        ;

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  def = PHI_RESULT (phi);
	  if (FLOAT_TYPE_P (TREE_TYPE (def))
	      && is_gimple_reg (def))
	    execute_cse_reciprocals_1 (&bsi, def, PR_BEFORE_BSI);
	}

      for (; !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  tree stmt = bsi_stmt (bsi);
	  if (TREE_CODE (stmt) == MODIFY_EXPR
	      && (def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF)) != NULL
	      && FLOAT_TYPE_P (TREE_TYPE (def))
	      && TREE_CODE (def) == SSA_NAME)
	    execute_cse_reciprocals_1 (&bsi, def, PR_AFTER_BSI);
	}
    }

  if (flag_trapping_math)
    free_dominance_info (CDI_POST_DOMINATORS);
  
  if (single_succ_p (ENTRY_BLOCK_PTR))
    bsi_commit_one_edge_insert (single_succ_edge (ENTRY_BLOCK_PTR), NULL);
}

struct tree_opt_pass pass_cse_reciprocals =
{
  "recip",				/* name */
  gate_cse_reciprocals,			/* gate */
  execute_cse_reciprocals,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_update_ssa | TODO_verify_ssa
    | TODO_verify_stmts,                /* todo_flags_finish */
  0				        /* letter */
};
