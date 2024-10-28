/* Combining of if-expressions on trees.
   Copyright (C) 2007-2024 Free Software Foundation, Inc.
   Contributed by Richard Guenther <rguenther@suse.de>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ssa.h"
#include "tree-pretty-print.h"
/* rtl is needed only because arm back-end requires it for
   BRANCH_COST.  */
#include "fold-const.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "attribs.h"
#include "asan.h"
#include "bitmap.h"

#ifndef LOGICAL_OP_NON_SHORT_CIRCUIT
#define LOGICAL_OP_NON_SHORT_CIRCUIT \
  (BRANCH_COST (optimize_function_for_speed_p (cfun), \
                false) >= 2)
#endif

/* Return FALSE iff the COND_BB ends with a conditional whose result is not a
   known constant.  */

static bool
known_succ_p (basic_block cond_bb)
{
  gcond *cond = safe_dyn_cast <gcond *> (*gsi_last_bb (cond_bb));

  if (!cond)
    return true;

  return (CONSTANT_CLASS_P (gimple_cond_lhs (cond))
	  && CONSTANT_CLASS_P (gimple_cond_rhs (cond)));
}

/* This pass combines COND_EXPRs to simplify control flow.  It
   currently recognizes bit tests and comparisons in chains that
   represent logical and or logical or of two COND_EXPRs.

   It does so by walking basic blocks in a approximate reverse
   post-dominator order and trying to match CFG patterns that
   represent logical and or logical or of two COND_EXPRs.
   Transformations are done if the COND_EXPR conditions match
   either

     1. two single bit tests X & (1 << Yn) (for logical and)

     2. two bit tests X & Yn (for logical or)

     3. two comparisons X OPn Y (for logical or)

   To simplify this pass, removing basic blocks and dead code
   is left to CFG cleanup and DCE.  */


/* Recognize a if-then-else CFG pattern starting to match with the COND_BB
   basic-block containing the COND_EXPR.  If !SUCCS_ANY, the condition must not
   resolve to a constant for a match.  Returns true if the pattern matched,
   false otherwise.  In case of a !SUCCS_ANY match, the recognized then end
   else blocks are stored to *THEN_BB and *ELSE_BB.  If *THEN_BB and/or
   *ELSE_BB are already set, they are required to match the then and else
   basic-blocks to make the pattern match.  If SUCCS_ANY, *THEN_BB and *ELSE_BB
   will not be filled in, and they will be found to match even if reversed.  */

static bool
recognize_if_then_else (basic_block cond_bb,
			basic_block *then_bb, basic_block *else_bb,
			bool succs_any = false)
{
  edge t, e;

  if (EDGE_COUNT (cond_bb->succs) != 2
      || (!succs_any && known_succ_p (cond_bb)))
    return false;

  /* Find the then/else edges.  */
  t = EDGE_SUCC (cond_bb, 0);
  e = EDGE_SUCC (cond_bb, 1);

  if (succs_any)
    return ((t->dest == *then_bb && e->dest == *else_bb)
	    || (t->dest == *else_bb && e->dest == *then_bb));

  if (!(t->flags & EDGE_TRUE_VALUE))
    std::swap (t, e);
  if (!(t->flags & EDGE_TRUE_VALUE)
      || !(e->flags & EDGE_FALSE_VALUE))
    return false;

  /* Check if the edge destinations point to the required block.  */
  if (*then_bb
      && t->dest != *then_bb)
    return false;
  if (*else_bb
      && e->dest != *else_bb)
    return false;

  if (!*then_bb)
    *then_bb = t->dest;
  if (!*else_bb)
    *else_bb = e->dest;

  return true;
}

/* Verify if the basic block BB does not have side-effects.  Return
   true in this case, else false.  */

static bool
bb_no_side_effects_p (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (is_gimple_debug (stmt))
	continue;

      gassign *ass;
      enum tree_code rhs_code;
      if (gimple_has_side_effects (stmt)
	  || gimple_could_trap_p (stmt)
	  || gimple_vdef (stmt)
	  /* We need to rewrite stmts with undefined overflow to use
	     unsigned arithmetic but cannot do so for signed division.  */
	  || ((ass = dyn_cast <gassign *> (stmt))
	      && INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_lhs (ass)))
	      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (gimple_assign_lhs (ass)))
	      && ((rhs_code = gimple_assign_rhs_code (ass)), true)
	      && (rhs_code == TRUNC_DIV_EXPR
		  || rhs_code == CEIL_DIV_EXPR
		  || rhs_code == FLOOR_DIV_EXPR
		  || rhs_code == ROUND_DIV_EXPR)
	      /* We cannot use expr_not_equal_to since we'd have to restrict
		 flow-sensitive info to whats known at the outer if.  */
	      && (TREE_CODE (gimple_assign_rhs2 (ass)) != INTEGER_CST
		  || !integer_minus_onep (gimple_assign_rhs2 (ass))))
	  /* const calls don't match any of the above, yet they could
	     still have some side-effects - they could contain
	     gimple_could_trap_p statements, like floating point
	     exceptions or integer division by zero.  See PR70586.
	     FIXME: perhaps gimple_has_side_effects or gimple_could_trap_p
	     should handle this.  */
	  || is_gimple_call (stmt))
	return false;

      ssa_op_iter it;
      tree use;
      FOR_EACH_SSA_TREE_OPERAND (use, stmt, it, SSA_OP_USE)
	if (ssa_name_maybe_undef_p (use))
	  return false;
    }

  return true;
}

/* Return true if BB is an empty forwarder block to TO_BB.  */

static bool
forwarder_block_to (basic_block bb, basic_block to_bb)
{
  return empty_block_p (bb)
	 && single_succ_p (bb)
	 && single_succ (bb) == to_bb;
}

/* Verify if all PHI node arguments in DEST for edges from BB1 or
   BB2 to DEST are the same.  This makes the CFG merge point
   free from side-effects.  Return true in this case, else false.  */

static bool
same_phi_args_p (basic_block bb1, basic_block bb2, basic_block dest)
{
  edge e1 = find_edge (bb1, dest);
  edge e2 = find_edge (bb2, dest);
  gphi_iterator gsi;
  gphi *phi;

  for (gsi = gsi_start_phis (dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi.phi ();
      if (!operand_equal_p (PHI_ARG_DEF_FROM_EDGE (phi, e1),
			    PHI_ARG_DEF_FROM_EDGE (phi, e2), 0))
        return false;
    }

  return true;
}

/* Return the best representative SSA name for CANDIDATE which is used
   in a bit test.  */

static tree
get_name_for_bit_test (tree candidate)
{
  /* Skip single-use names in favor of using the name from a
     non-widening conversion definition.  */
  if (TREE_CODE (candidate) == SSA_NAME
      && has_single_use (candidate))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (candidate);
      if (is_gimple_assign (def_stmt)
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt)))
	{
	  if (TYPE_PRECISION (TREE_TYPE (candidate))
	      <= TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (def_stmt))))
	    return gimple_assign_rhs1 (def_stmt);
	}
    }

  return candidate;
}

/* Recognize a single bit test pattern in GIMPLE_COND and its defining
   statements.  Store the name being tested in *NAME and the bit
   in *BIT.  The GIMPLE_COND computes *NAME & (1 << *BIT).
   Returns true if the pattern matched, false otherwise.  */

static bool
recognize_single_bit_test (gcond *cond, tree *name, tree *bit, bool inv)
{
  gimple *stmt;

  /* Get at the definition of the result of the bit test.  */
  if (gimple_cond_code (cond) != (inv ? EQ_EXPR : NE_EXPR)
      || TREE_CODE (gimple_cond_lhs (cond)) != SSA_NAME
      || !integer_zerop (gimple_cond_rhs (cond)))
    return false;
  stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (cond));
  if (!is_gimple_assign (stmt))
    return false;

  /* Look at which bit is tested.  One form to recognize is
     D.1985_5 = state_3(D) >> control1_4(D);
     D.1986_6 = (int) D.1985_5;
     D.1987_7 = op0 & 1;
     if (D.1987_7 != 0)  */
  if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
      && integer_onep (gimple_assign_rhs2 (stmt))
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    {
      tree orig_name = gimple_assign_rhs1 (stmt);

      /* Look through copies and conversions to eventually
	 find the stmt that computes the shift.  */
      stmt = SSA_NAME_DEF_STMT (orig_name);

      while (is_gimple_assign (stmt)
	     && ((CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt))
		  && (TYPE_PRECISION (TREE_TYPE (gimple_assign_lhs (stmt)))
		      <= TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (stmt))))
		  && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
		 || gimple_assign_ssa_name_copy_p (stmt)))
	stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));

      /* If we found such, decompose it.  */
      if (is_gimple_assign (stmt)
	  && gimple_assign_rhs_code (stmt) == RSHIFT_EXPR)
	{
	  /* op0 & (1 << op1) */
	  *bit = gimple_assign_rhs2 (stmt);
	  *name = gimple_assign_rhs1 (stmt);
	}
      else
	{
	  /* t & 1 */
	  *bit = integer_zero_node;
	  *name = get_name_for_bit_test (orig_name);
	}

      return true;
    }

  /* Another form is
     D.1987_7 = op0 & (1 << CST)
     if (D.1987_7 != 0)  */
  if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
      && integer_pow2p (gimple_assign_rhs2 (stmt)))
    {
      *name = gimple_assign_rhs1 (stmt);
      *bit = build_int_cst (integer_type_node,
			    tree_log2 (gimple_assign_rhs2 (stmt)));
      return true;
    }

  /* Another form is
     D.1986_6 = 1 << control1_4(D)
     D.1987_7 = op0 & D.1986_6
     if (D.1987_7 != 0)  */
  if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
      && TREE_CODE (gimple_assign_rhs2 (stmt)) == SSA_NAME)
    {
      gimple *tmp;

      /* Both arguments of the BIT_AND_EXPR can be the single-bit
	 specifying expression.  */
      tmp = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
      if (is_gimple_assign (tmp)
	  && gimple_assign_rhs_code (tmp) == LSHIFT_EXPR
	  && integer_onep (gimple_assign_rhs1 (tmp)))
	{
	  *name = gimple_assign_rhs2 (stmt);
	  *bit = gimple_assign_rhs2 (tmp);
	  return true;
	}

      tmp = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));
      if (is_gimple_assign (tmp)
	  && gimple_assign_rhs_code (tmp) == LSHIFT_EXPR
	  && integer_onep (gimple_assign_rhs1 (tmp)))
	{
	  *name = gimple_assign_rhs1 (stmt);
	  *bit = gimple_assign_rhs2 (tmp);
	  return true;
	}
    }

  return false;
}

/* Recognize a bit test pattern in a GIMPLE_COND and its defining
   statements.  Store the name being tested in *NAME and the bits
   in *BITS.  The COND_EXPR computes *NAME & *BITS.
   Returns true if the pattern matched, false otherwise.  */

static bool
recognize_bits_test (gcond *cond, tree *name, tree *bits, bool inv)
{
  gimple *stmt;

  /* Get at the definition of the result of the bit test.  */
  if (gimple_cond_code (cond) != (inv ? EQ_EXPR : NE_EXPR)
      || TREE_CODE (gimple_cond_lhs (cond)) != SSA_NAME
      || !integer_zerop (gimple_cond_rhs (cond)))
    return false;
  stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (cond));
  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != BIT_AND_EXPR)
    return false;

  *name = get_name_for_bit_test (gimple_assign_rhs1 (stmt));
  *bits = gimple_assign_rhs2 (stmt);

  return true;
}


/* Update profile after code in either outer_cond_bb or inner_cond_bb was
   adjusted so that it has no condition.  */

static void
update_profile_after_ifcombine (basic_block inner_cond_bb,
			        basic_block outer_cond_bb)
{
  /* In the following we assume that inner_cond_bb has single predecessor.  */
  gcc_assert (single_pred_p (inner_cond_bb));

  basic_block outer_to_inner_bb = inner_cond_bb;
  profile_probability prob = profile_probability::always ();
  for (;;)
    {
      basic_block parent = single_pred (outer_to_inner_bb);
      prob *= find_edge (parent, outer_to_inner_bb)->probability;
      if (parent == outer_cond_bb)
	break;
      outer_to_inner_bb = parent;
    }

  edge outer_to_inner = find_edge (outer_cond_bb, outer_to_inner_bb);
  edge outer2 = (EDGE_SUCC (outer_cond_bb, 0) == outer_to_inner
		 ? EDGE_SUCC (outer_cond_bb, 1)
		 : EDGE_SUCC (outer_cond_bb, 0));
  edge inner_taken = EDGE_SUCC (inner_cond_bb, 0);
  edge inner_not_taken = EDGE_SUCC (inner_cond_bb, 1);

  if (inner_taken->dest != outer2->dest)
    std::swap (inner_taken, inner_not_taken);
  gcc_assert (inner_taken->dest == outer2->dest);

  if (outer_to_inner_bb == inner_cond_bb
      && known_succ_p (outer_cond_bb))
    {
      /* Path outer_cond_bb->(outer2) needs to be merged into path
	 outer_cond_bb->(outer_to_inner)->inner_cond_bb->(inner_taken)
	 and probability of inner_not_taken updated.  */

      inner_cond_bb->count = outer_cond_bb->count;

      /* Handle special case where inner_taken probability is always. In this
	 case we know that the overall outcome will be always as well, but
	 combining probabilities will be conservative because it does not know
	 that outer2->probability is inverse of
	 outer_to_inner->probability.  */
      if (inner_taken->probability == profile_probability::always ())
	;
      else
	inner_taken->probability = outer2->probability
	  + outer_to_inner->probability * inner_taken->probability;
      inner_not_taken->probability = profile_probability::always ()
	- inner_taken->probability;

      outer_to_inner->probability = profile_probability::always ();
      outer2->probability = profile_probability::never ();
    }
  else if (known_succ_p (inner_cond_bb))
    {
      /* Path inner_cond_bb->(inner_taken) needs to be merged into path
	 outer_cond_bb->(outer2).  We've accumulated the probabilities from
	 outer_cond_bb->(outer)->...->inner_cond_bb in prob, so we have to
	 adjust that by inner_taken, and make inner unconditional.  */

      prob *= inner_taken->probability;
      outer2->probability += prob;
      outer_to_inner->probability = profile_probability::always ()
	- outer2->probability;

      inner_taken->probability = profile_probability::never ();
      inner_not_taken->probability = profile_probability::always ();
    }
  else
    {
      /* We've moved part of the inner cond to outer, but we don't know the
	 probabilities for each part, so estimate the effects by moving half of
	 the odds of inner_taken to outer.  */

      inner_taken->probability *= profile_probability::even ();
      inner_not_taken->probability = profile_probability::always ()
	- inner_taken->probability;

      prob *= inner_taken->probability;
      outer2->probability += prob;
      outer_to_inner->probability = profile_probability::always ()
	- outer2->probability;
    }
}

/* Set NAME's bit in USED if OUTER dominates it.  */

static void
ifcombine_mark_ssa_name (bitmap used, tree name, basic_block outer)
{
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    return;

  gimple *def = SSA_NAME_DEF_STMT (name);
  basic_block bb = gimple_bb (def);
  if (!dominated_by_p (CDI_DOMINATORS, bb, outer))
    return;

  bitmap_set_bit (used, SSA_NAME_VERSION (name));
}

/* Data structure passed to ifcombine_mark_ssa_name.  */
struct ifcombine_mark_ssa_name_t
{
  /* SSA_NAMEs that have been referenced.  */
  bitmap used;
  /* Dominating block of DEFs that might need moving.  */
  basic_block outer;
};

/* Mark in DATA->used any SSA_NAMEs used in *t.  */

static tree
ifcombine_mark_ssa_name_walk (tree *t, int *, void *data_)
{
  ifcombine_mark_ssa_name_t *data = (ifcombine_mark_ssa_name_t *)data_;

  if (*t && TREE_CODE (*t) == SSA_NAME)
    ifcombine_mark_ssa_name (data->used, *t, data->outer);

  return NULL;
}

/* Rewrite a stmt, that presumably used to be guarded by conditions that could
   avoid undefined overflow, into one that has well-defined overflow, so that
   it won't invoke undefined behavior once the guarding conditions change.  */

static inline void
ifcombine_rewrite_to_defined_overflow (gimple_stmt_iterator gsi)
{
  gassign *ass = dyn_cast <gassign *> (gsi_stmt (gsi));
  if (!ass)
    return;
  tree lhs = gimple_assign_lhs (ass);
  if ((INTEGRAL_TYPE_P (TREE_TYPE (lhs))
       || POINTER_TYPE_P (TREE_TYPE (lhs)))
      && arith_code_with_undefined_signed_overflow
      (gimple_assign_rhs_code (ass)))
    rewrite_to_defined_overflow (&gsi);
}


/* Replace the conditions in INNER_COND and OUTER_COND with COND and COND2.
   COND and COND2 are computed for insertion at INNER_COND, with OUTER_COND
   replaced with a constant, but if there are intervening blocks, it's best to
   adjust COND for insertion at OUTER_COND, placing COND2 at INNER_COND.  */

static bool
ifcombine_replace_cond (gcond *inner_cond, bool inner_inv,
			gcond *outer_cond, bool outer_inv,
			tree cond, bool must_canon, tree cond2)
{
  bool split_single_cond = false;
  /* Split cond into cond2 if they're contiguous.  ??? We might be able to
     handle ORIF as well, inverting both conditions, but it's not clear that
     this would be enough, and it never comes up.  */
  if (!cond2
      && TREE_CODE (cond) == TRUTH_ANDIF_EXPR
      && single_pred (gimple_bb (inner_cond)) == gimple_bb (outer_cond))
    {
      cond2 = TREE_OPERAND (cond, 1);
      cond = TREE_OPERAND (cond, 0);
      split_single_cond = true;
    }

  bool outer_p = cond2 || (single_pred (gimple_bb (inner_cond))
			   != gimple_bb (outer_cond));
  bool result_inv = outer_p ? outer_inv : inner_inv;
  bool strictening_outer_cond = !split_single_cond && outer_p;

  if (result_inv)
    cond = fold_build1 (TRUTH_NOT_EXPR, TREE_TYPE (cond), cond);

  if (tree tcanon = canonicalize_cond_expr_cond (cond))
    cond = tcanon;
  else if (must_canon)
    return false;

  if (outer_p)
    {
      {
	auto_bitmap used;
	basic_block outer_bb = gimple_bb (outer_cond);

	bitmap_tree_view (used);

	/* Mark SSA DEFs that are referenced by cond and may thus need to be
	   moved to outer.  */
	{
	  ifcombine_mark_ssa_name_t data = { used, outer_bb };
	  walk_tree (&cond, ifcombine_mark_ssa_name_walk, &data, NULL);
	}

	if (!bitmap_empty_p (used))
	  {
	    const int max_stmts = 6;
	    auto_vec<gimple *, max_stmts> stmts;

	    /* Iterate up from inner_cond, moving DEFs identified as used by
	       cond, and marking USEs in the DEFs for moving as well.  */
	    for (basic_block bb = gimple_bb (inner_cond);
		 bb != outer_bb; bb = single_pred (bb))
	      {
		for (gimple_stmt_iterator gsitr = gsi_last_bb (bb);
		     !gsi_end_p (gsitr); gsi_prev (&gsitr))
		  {
		    gimple *stmt = gsi_stmt (gsitr);
		    bool move = false;
		    tree t;
		    ssa_op_iter it;

		    FOR_EACH_SSA_TREE_OPERAND (t, stmt, it, SSA_OP_DEF)
		      if (bitmap_bit_p (used, SSA_NAME_VERSION (t)))
			{
			  move = true;
			  break;
			}

		    if (!move)
		      continue;

		    if (stmts.length () < max_stmts)
		      stmts.quick_push (stmt);
		    else
		      return false;

		    /* Mark uses in STMT before moving it.  */
		    FOR_EACH_SSA_TREE_OPERAND (t, stmt, it, SSA_OP_USE)
		      ifcombine_mark_ssa_name (used, t, outer_bb);
		  }

		/* Surprisingly, there may be PHI nodes in single-predecessor
		   bocks, as in pr50682.C.  Fortunately, since they can't
		   involve back edges, there won't be references to parallel
		   nodes that we'd have to pay special attention to to keep
		   them parallel.  We can't move the PHI nodes, but we can turn
		   them into assignments.  */
		for (gphi_iterator gsi = gsi_start_phis (bb);
		     !gsi_end_p (gsi);)
		  {
		    gphi *phi = gsi.phi ();

		    gcc_assert (gimple_phi_num_args (phi) == 1);
		    tree def = gimple_phi_result (phi);

		    if (!bitmap_bit_p (used, SSA_NAME_VERSION (def)))
		      {
			gsi_next (&gsi);
			continue;
		      }

		    if (stmts.length () < max_stmts)
		      stmts.quick_push (phi);
		    else
		      return false;

		    /* Mark uses in STMT before moving it.  */
		    use_operand_p use_p;
		    ssa_op_iter it;
		    FOR_EACH_PHI_ARG (use_p, phi, it, SSA_OP_USE)
		      ifcombine_mark_ssa_name (used, USE_FROM_PTR (use_p),
					       outer_bb);
		  }
	      }

	    /* ??? Test whether it makes sense to move STMTS.  */

	    /* Move the STMTS that need moving.  From this point on, we're
	       committing to the attempted ifcombine.  */
	    gimple_stmt_iterator gsins = gsi_for_stmt (outer_cond);
	    unsigned i;
	    gimple *stmt;
	    FOR_EACH_VEC_ELT (stmts, i, stmt)
	      {
		if (gphi *phi = dyn_cast <gphi *> (stmt))
		  {
		    tree def = gimple_phi_result (phi);
		    tree use = gimple_phi_arg_def (phi, 0);
		    location_t loc = gimple_phi_arg_location (phi, 0);

		    gphi_iterator gsi = gsi_for_phi (phi);
		    remove_phi_node (&gsi, false);

		    gassign *a = gimple_build_assign (def, use);
		    gimple_set_location (a, loc);
		    gsi_insert_before (&gsins, a, GSI_NEW_STMT);
		  }
		else
		  {
		    gimple_stmt_iterator gsitr = gsi_for_stmt (stmt);
		    gsi_move_before (&gsitr, &gsins, GSI_NEW_STMT);
		  }
	      }

	    for (; gsi_stmt (gsins) != outer_cond; gsi_next (&gsins))
	      {
		/* Clear range info from all defs we've moved from under
		   conditions.  */
		tree t;
		ssa_op_iter it;
		FOR_EACH_SSA_TREE_OPERAND (t, gsi_stmt (gsins), it, SSA_OP_DEF)
		  reset_flow_sensitive_info (t);
		/* Avoid introducing undefined overflows while at that.  */
		ifcombine_rewrite_to_defined_overflow (gsins);
	      }
	  }
      }

      if (!is_gimple_condexpr_for_cond (cond))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (outer_cond);
	  cond = force_gimple_operand_gsi_1 (&gsi, cond,
					     is_gimple_condexpr_for_cond,
					     NULL, true, GSI_SAME_STMT);
	}

      /* Leave CFG optimization to cfg_cleanup.  */
      gimple_cond_set_condition_from_tree (outer_cond, cond);
      update_stmt (outer_cond);

      if (cond2)
	{
	  if (inner_inv)
	    cond2 = fold_build1 (TRUTH_NOT_EXPR, TREE_TYPE (cond2), cond2);

	  if (tree tcanon = canonicalize_cond_expr_cond (cond2))
	    cond2 = tcanon;
	  if (!is_gimple_condexpr_for_cond (cond2))
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (inner_cond);
	      cond2 = force_gimple_operand_gsi_1 (&gsi, cond2,
						  is_gimple_condexpr_for_cond,
						  NULL, true, GSI_SAME_STMT);
	    }
	  gimple_cond_set_condition_from_tree (inner_cond, cond2);
	}
      else
	gimple_cond_set_condition_from_tree (inner_cond,
					     inner_inv
					     ? boolean_false_node
					     : boolean_true_node);
      update_stmt (inner_cond);
    }
  else
    {
      if (!is_gimple_condexpr_for_cond (cond))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (inner_cond);
	  cond = force_gimple_operand_gsi_1 (&gsi, cond,
					     is_gimple_condexpr_for_cond,
					     NULL, true, GSI_SAME_STMT);
	}
      gimple_cond_set_condition_from_tree (inner_cond, cond);
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      gimple_cond_set_condition_from_tree (outer_cond,
					   outer_inv
					   ? boolean_false_node
					   : boolean_true_node);
      update_stmt (outer_cond);
    }

  /* We're changing conditions that guard inner blocks, so reset flow sensitive
     info and avoid introducing undefined behavior.  */
  for (basic_block bb = gimple_bb (inner_cond), end = gimple_bb (outer_cond);
       bb != end; bb = single_pred (bb))
    {
      /* Clear range info from all stmts in BB which is now guarded by
	 different conditionals.  */
      reset_flow_sensitive_info_in_bb (gimple_bb (inner_cond));

      /* We only need to worry about introducing undefined behavior if we've
	 relaxed the outer condition.  */
      if (strictening_outer_cond)
	continue;

      /* Avoid introducing undefined behavior as we move stmts that used to be
	 guarded by OUTER_COND.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (gimple_bb (inner_cond));
	   !gsi_end_p (gsi); gsi_next (&gsi))
	ifcombine_rewrite_to_defined_overflow (gsi);
    }

  update_profile_after_ifcombine (gimple_bb (inner_cond),
				  gimple_bb (outer_cond));

  return true;
}

/* Returns true if inner_cond_bb contains just the condition or 1/2 statements
   that define lhs or rhs with an integer conversion. */

static bool
can_combine_bbs_with_short_circuit (basic_block inner_cond_bb, tree lhs, tree rhs)
{
  gimple_stmt_iterator gsi;
  gsi = gsi_start_nondebug_after_labels_bb (inner_cond_bb);
  /* If only the condition, this should be allowed. */
  if (gsi_one_before_end_p (gsi))
    return true;
  /* Can have up to 2 statements defining each of lhs/rhs. */
  for (int i = 0; i < 2; i++)
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!is_gimple_assign (stmt)
	  || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt)))
	return false;
      /* The defining statement needs to match either the lhs or rhs of
	 the condition. */
      if (lhs != gimple_assign_lhs (stmt)
	  && rhs != gimple_assign_lhs (stmt))
	return false;
      gsi_next_nondebug (&gsi);
      if (gsi_one_before_end_p (gsi))
	return true;
    }
  return false;
}

/* If-convert on a and pattern with a common else block.  The inner
   if is specified by its INNER_COND_BB, the outer by OUTER_COND_BB.
   inner_inv, outer_inv indicate whether the conditions are inverted.
   Returns true if the edges to the common else basic-block were merged.  */

static bool
ifcombine_ifandif (basic_block inner_cond_bb, bool inner_inv,
		   basic_block outer_cond_bb, bool outer_inv)
{
  gimple_stmt_iterator gsi;
  tree name1, name2, bit1, bit2, bits1, bits2;

  gcond *inner_cond = safe_dyn_cast <gcond *> (*gsi_last_bb (inner_cond_bb));
  if (!inner_cond)
    return false;

  gcond *outer_cond = safe_dyn_cast <gcond *> (*gsi_last_bb (outer_cond_bb));
  if (!outer_cond)
    return false;

  /* See if we test a single bit of the same name in both tests.  In
     that case remove the outer test, merging both else edges,
     and change the inner one to test for
     name & (bit1 | bit2) == (bit1 | bit2).  */
  if (recognize_single_bit_test (inner_cond, &name1, &bit1, inner_inv)
      && recognize_single_bit_test (outer_cond, &name2, &bit2, outer_inv)
      && name1 == name2)
    {
      tree t, t2;

      if (TREE_CODE (name1) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name1))
	return false;

      /* Do it.  */
      gsi = gsi_for_stmt (inner_cond);
      t = fold_build2 (LSHIFT_EXPR, TREE_TYPE (name1),
		       build_int_cst (TREE_TYPE (name1), 1), bit1);
      t2 = fold_build2 (LSHIFT_EXPR, TREE_TYPE (name1),
		        build_int_cst (TREE_TYPE (name1), 1), bit2);
      t = fold_build2 (BIT_IOR_EXPR, TREE_TYPE (name1), t, t2);
      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				    true, GSI_SAME_STMT);
      t2 = fold_build2 (BIT_AND_EXPR, TREE_TYPE (name1), name1, t);
      t2 = force_gimple_operand_gsi (&gsi, t2, true, NULL_TREE,
				     true, GSI_SAME_STMT);

      t = fold_build2 (EQ_EXPR, boolean_type_node, t2, t);

      if (!ifcombine_replace_cond (inner_cond, inner_inv,
				   outer_cond, outer_inv,
				   t, true, NULL_TREE))
	return false;

      if (dump_file)
	{
	  fprintf (dump_file, "optimizing double bit test to ");
	  print_generic_expr (dump_file, name1);
	  fprintf (dump_file, " & T == T\nwith temporary T = (1 << ");
	  print_generic_expr (dump_file, bit1);
	  fprintf (dump_file, ") | (1 << ");
	  print_generic_expr (dump_file, bit2);
	  fprintf (dump_file, ")\n");
	}

      return true;
    }

  /* See if we have two bit tests of the same name in both tests.
     In that case remove the outer test and change the inner one to
     test for name & (bits1 | bits2) != 0.  */
  else if (recognize_bits_test (inner_cond, &name1, &bits1, !inner_inv)
	   && recognize_bits_test (outer_cond, &name2, &bits2, !outer_inv))
    {
      tree t;

      if ((TREE_CODE (name1) == SSA_NAME
	   && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name1))
	  || (TREE_CODE (name2) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name2)))
	return false;

      /* Find the common name which is bit-tested.  */
      if (name1 == name2)
	;
      else if (bits1 == bits2)
	{
	  std::swap (name2, bits2);
	  std::swap (name1, bits1);
	}
      else if (name1 == bits2)
	std::swap (name2, bits2);
      else if (bits1 == name2)
	std::swap (name1, bits1);
      else
	return false;

      /* As we strip non-widening conversions in finding a common
         name that is tested make sure to end up with an integral
	 type for building the bit operations.  */
      if (TYPE_PRECISION (TREE_TYPE (bits1))
	  >= TYPE_PRECISION (TREE_TYPE (bits2)))
	{
	  bits1 = fold_convert (unsigned_type_for (TREE_TYPE (bits1)), bits1);
	  name1 = fold_convert (TREE_TYPE (bits1), name1);
	  bits2 = fold_convert (unsigned_type_for (TREE_TYPE (bits2)), bits2);
	  bits2 = fold_convert (TREE_TYPE (bits1), bits2);
	}
      else
	{
	  bits2 = fold_convert (unsigned_type_for (TREE_TYPE (bits2)), bits2);
	  name1 = fold_convert (TREE_TYPE (bits2), name1);
	  bits1 = fold_convert (unsigned_type_for (TREE_TYPE (bits1)), bits1);
	  bits1 = fold_convert (TREE_TYPE (bits2), bits1);
	}

      t = fold_build2 (BIT_IOR_EXPR, TREE_TYPE (name1), bits1, bits2);
      t = fold_build2 (BIT_AND_EXPR, TREE_TYPE (name1), name1, t);
      t = fold_build2 (EQ_EXPR, boolean_type_node, t,
		       build_int_cst (TREE_TYPE (t), 0));
      if (!ifcombine_replace_cond (inner_cond, inner_inv,
				   outer_cond, outer_inv,
				   t, false, NULL_TREE))
	return false;

      if (dump_file)
	{
	  fprintf (dump_file, "optimizing bits or bits test to ");
	  print_generic_expr (dump_file, name1);
	  fprintf (dump_file, " & T != 0\nwith temporary T = ");
	  print_generic_expr (dump_file, bits1);
	  fprintf (dump_file, " | ");
	  print_generic_expr (dump_file, bits2);
	  fprintf (dump_file, "\n");
	}

      return true;
    }

  /* See if we have two comparisons that we can merge into one.  */
  else if (TREE_CODE_CLASS (gimple_cond_code (inner_cond)) == tcc_comparison
	   && TREE_CODE_CLASS (gimple_cond_code (outer_cond)) == tcc_comparison)
    {
      tree t, ts = NULL_TREE;
      enum tree_code inner_cond_code = gimple_cond_code (inner_cond);
      enum tree_code outer_cond_code = gimple_cond_code (outer_cond);

      /* Invert comparisons if necessary (and possible).  */
      if (inner_inv)
	inner_cond_code = invert_tree_comparison (inner_cond_code,
	  HONOR_NANS (gimple_cond_lhs (inner_cond)));
      if (inner_cond_code == ERROR_MARK)
	return false;
      if (outer_inv)
	outer_cond_code = invert_tree_comparison (outer_cond_code,
	  HONOR_NANS (gimple_cond_lhs (outer_cond)));
      if (outer_cond_code == ERROR_MARK)
	return false;
      /* Don't return false so fast, try maybe_fold_or_comparisons?  */

      if (!(t = maybe_fold_and_comparisons (boolean_type_node, inner_cond_code,
					    gimple_cond_lhs (inner_cond),
					    gimple_cond_rhs (inner_cond),
					    outer_cond_code,
					    gimple_cond_lhs (outer_cond),
					    gimple_cond_rhs (outer_cond),
					    gimple_bb (outer_cond))))
	{
	  tree t1, t2;
	  bool logical_op_non_short_circuit = LOGICAL_OP_NON_SHORT_CIRCUIT;
	  if (param_logical_op_non_short_circuit != -1)
	    logical_op_non_short_circuit
	      = param_logical_op_non_short_circuit;
	  if (!logical_op_non_short_circuit || sanitize_coverage_p ())
	    return false;
	  /* Only do this optimization if the inner bb contains only the conditional
	     or there is one or 2 statements which are nop conversion for the comparison. */
	  if (!can_combine_bbs_with_short_circuit (inner_cond_bb,
						   gimple_cond_lhs (inner_cond),
						   gimple_cond_rhs (inner_cond)))
	    return false;
	  t1 = fold_build2_loc (gimple_location (inner_cond),
				inner_cond_code,
				boolean_type_node,
				gimple_cond_lhs (inner_cond),
				gimple_cond_rhs (inner_cond));
	  t2 = fold_build2_loc (gimple_location (outer_cond),
				outer_cond_code,
				boolean_type_node,
				gimple_cond_lhs (outer_cond),
				gimple_cond_rhs (outer_cond));
	  t = fold_build2_loc (gimple_location (inner_cond),
			       TRUTH_AND_EXPR, boolean_type_node, t1, t2);
        }

      if (!ifcombine_replace_cond (inner_cond, inner_inv,
				   outer_cond, outer_inv,
				   t, false, ts))
	return false;

      if (dump_file)
	{
	  fprintf (dump_file, "optimizing two comparisons to ");
	  print_generic_expr (dump_file, t);
	  if (ts)
	    {
	      fprintf (dump_file, " and ");
	      print_generic_expr (dump_file, ts);
	    }
	  fprintf (dump_file, "\n");
	}

      return true;
    }

  return false;
}

/* Helper function for tree_ssa_ifcombine_bb.  Recognize a CFG pattern and
   dispatch to the appropriate if-conversion helper for a particular
   set of INNER_COND_BB, OUTER_COND_BB, THEN_BB and ELSE_BB.
   PHI_PRED_BB should be one of INNER_COND_BB, THEN_BB or ELSE_BB.
   OUTER_SUCC_BB is the successor of OUTER_COND_BB on the path towards
   INNER_COND_BB.  */

static bool
tree_ssa_ifcombine_bb_1 (basic_block inner_cond_bb, basic_block outer_cond_bb,
			 basic_block then_bb, basic_block else_bb,
			 basic_block phi_pred_bb, basic_block outer_succ_bb)
{
  /* The && form is characterized by a common else_bb with
     the two edges leading to it mergable.  The latter is
     guaranteed by matching PHI arguments in the else_bb and
     the inner cond_bb having no side-effects.  */
  if (phi_pred_bb != else_bb
      && recognize_if_then_else (outer_cond_bb, &outer_succ_bb, &else_bb)
      && same_phi_args_p (outer_cond_bb, phi_pred_bb, else_bb))
    {
      /* We have
	   <outer_cond_bb>
	     if (q) goto inner_cond_bb; else goto else_bb;
	   <inner_cond_bb>
	     if (p) goto ...; else goto else_bb;
	     ...
	   <else_bb>
	     ...
       */
      return ifcombine_ifandif (inner_cond_bb, false, outer_cond_bb, false);
    }

  /* And a version where the outer condition is negated.  */
  if (phi_pred_bb != else_bb
      && recognize_if_then_else (outer_cond_bb, &else_bb, &outer_succ_bb)
      && same_phi_args_p (outer_cond_bb, phi_pred_bb, else_bb))
    {
      /* We have
	   <outer_cond_bb>
	     if (q) goto else_bb; else goto inner_cond_bb;
	   <inner_cond_bb>
	     if (p) goto ...; else goto else_bb;
	     ...
	   <else_bb>
	     ...
       */
      return ifcombine_ifandif (inner_cond_bb, false, outer_cond_bb, true);
    }

  /* The || form is characterized by a common then_bb with the
     two edges leading to it mergable.  The latter is guaranteed
     by matching PHI arguments in the then_bb and the inner cond_bb
     having no side-effects.  */
  if (phi_pred_bb != then_bb
      && recognize_if_then_else (outer_cond_bb, &then_bb, &outer_succ_bb)
      && same_phi_args_p (outer_cond_bb, phi_pred_bb, then_bb))
    {
      /* We have
	   <outer_cond_bb>
	     if (q) goto then_bb; else goto inner_cond_bb;
	   <inner_cond_bb>
	     if (q) goto then_bb; else goto ...;
	   <then_bb>
	     ...
       */
      return ifcombine_ifandif (inner_cond_bb, true, outer_cond_bb, true);
    }

  /* And a version where the outer condition is negated.  */
  if (phi_pred_bb != then_bb
      && recognize_if_then_else (outer_cond_bb, &outer_succ_bb, &then_bb)
      && same_phi_args_p (outer_cond_bb, phi_pred_bb, then_bb))
    {
      /* We have
	   <outer_cond_bb>
	     if (q) goto inner_cond_bb; else goto then_bb;
	   <inner_cond_bb>
	     if (q) goto then_bb; else goto ...;
	   <then_bb>
	     ...
       */
      return ifcombine_ifandif (inner_cond_bb, true, outer_cond_bb, false);
    }

  return false;
}

/* Recognize a CFG pattern and dispatch to the appropriate
   if-conversion helper.  We start with BB as the innermost
   worker basic-block.  Returns true if a transformation was done.  */

static bool
tree_ssa_ifcombine_bb (basic_block inner_cond_bb)
{
  bool ret = false;
  basic_block then_bb = NULL, else_bb = NULL;

  if (!recognize_if_then_else (inner_cond_bb, &then_bb, &else_bb))
    return ret;

  /* Recognize && and || of two conditions with a common
     then/else block which entry edges we can merge.  That is:
       if (a || b)
	 ;
     and
       if (a && b)
	 ;
     This requires a single predecessor of the inner cond_bb.

     Look for an OUTER_COND_BBs to combine with INNER_COND_BB.  They need not
     be contiguous, as long as inner and intervening blocks have no side
     effects, and are either single-entry-single-exit or conditionals choosing
     between the same EXIT_BB with the same PHI args, and the path leading to
     INNER_COND_BB.  ??? We could potentially handle multi-block
     single-entry-single-exit regions, but the loop below only deals with
     single-entry-single-exit individual intervening blocks.  Larger regions
     without side effects are presumably rare, so it's probably not worth the
     effort.  */
  for (basic_block bb = inner_cond_bb, outer_cond_bb, exit_bb = NULL;
       single_pred_p (bb) && bb_no_side_effects_p (bb);
       bb = outer_cond_bb)
    {
      bool changed = false;

      outer_cond_bb = single_pred (bb);

      /* Skip blocks without conditions.  */
      if (single_succ_p (outer_cond_bb))
	continue;

      /* When considering noncontiguous conditions, make sure that all
	 non-final conditions lead to the same successor of the final
	 condition, when not taking the path to inner_bb, so that we can
	 combine C into A, both in A && (B && C), and in A || (B || C), but
	 neither in A && (B || C), nor A || (B && C).  Say, if C goes to
	 THEN_BB or ELSE_BB, then B must go to either of these, say X, besides
	 C (whether C is then or else), and A must go to X and B (whether then
	 or else).

	 We test for this, while allowing intervening nonconditional blocks, by
	 first taking note of which of the successors of the inner conditional
	 block is the exit path taken by the first considered outer conditional
	 block.

	 Having identified and saved the exit block in EXIT_BB at the end of
	 the loop, here we test that subsequent conditional blocks under
	 consideration also use the exit block as a successor, besides the
	 block that leads to inner_cond_bb, and that the edges to exit share
	 the same phi values.  */
      if (exit_bb
	  && !recognize_if_then_else (outer_cond_bb, &bb, &exit_bb, true))
	break;

      /* After checking dests and phi args, we can also skip blocks whose
	 conditions have been optimized down to a constant, without trying to
	 combine them, but we must not skip the computation of EXIT_BB and the
	 checking of same phi args.  */
      if (known_succ_p (outer_cond_bb))
	changed = false;
      else if (tree_ssa_ifcombine_bb_1 (inner_cond_bb, outer_cond_bb,
					then_bb, else_bb, inner_cond_bb, bb))
	changed = true;
      else if (forwarder_block_to (else_bb, then_bb))
	{
	  /* Other possibilities for the && form, if else_bb is
	     empty forwarder block to then_bb.  Compared to the above simpler
	     forms this can be treated as if then_bb and else_bb were swapped,
	     and the corresponding inner_cond_bb not inverted because of that.
	     For same_phi_args_p we look at equality of arguments between
	     edge from outer_cond_bb and the forwarder block.  */
	  if (tree_ssa_ifcombine_bb_1 (inner_cond_bb, outer_cond_bb, else_bb,
				       then_bb, else_bb, bb))
	    changed = true;
	}
      else if (forwarder_block_to (then_bb, else_bb))
	{
	  /* Other possibilities for the || form, if then_bb is
	     empty forwarder block to else_bb.  Compared to the above simpler
	     forms this can be treated as if then_bb and else_bb were swapped,
	     and the corresponding inner_cond_bb not inverted because of that.
	     For same_phi_args_p we look at equality of arguments between
	     edge from outer_cond_bb and the forwarder block.  */
	  if (tree_ssa_ifcombine_bb_1 (inner_cond_bb, outer_cond_bb, else_bb,
				       then_bb, then_bb, bb))
	    changed = true;
	}

      if (changed)
	ret = changed;

      /* If the inner condition is gone, there's no point in attempting to
	 combine it any further.  */
      if (changed && known_succ_p (inner_cond_bb))
	break;

      /* Record the exit path taken by the outer condition.  */
      if (!exit_bb)
	{
	  if (recognize_if_then_else (outer_cond_bb, &then_bb, &bb, true))
	    exit_bb = then_bb;
	  else if (recognize_if_then_else (outer_cond_bb, &bb, &else_bb, true))
	    exit_bb = else_bb;
	  else
	    break;
	}

      /* Before trying an earlier block, make sure INNER_COND_BB and the
	 current OUTER_COND_BB share the same PHI args at EXIT_BB.  We don't
	 need to check if the latest attempt at combining succeeded, because
	 that means we'll have already checked.  But we can't only check outer
	 and inner, we have to check that all intervening blocks also get to
	 exit with the same result, otherwise the transformation may change the
	 final result.  Consider (a ? 0 : b ? 1 : c ? 0 : -1).  If we combine
	 (a | c), yielding ((a | c) ? 0 : b ? 1 : [0 ? 0 :] -1), we'd get 0
	 rather than 1 when (!a&&b).  And if we were to replace inner instead
	 of outer, we'd get ([1 ? 0 :] b ? 1 : (a | c) ? 0 : -1), which would
	 yield 1 rather than 0 when (a).  */
      if (!changed
	  && !same_phi_args_p (outer_cond_bb, inner_cond_bb, exit_bb))
	break;
    }

  return ret;
}

/* Main entry for the tree if-conversion pass.  */

namespace {

const pass_data pass_data_tree_ifcombine =
{
  GIMPLE_PASS, /* type */
  "ifcombine", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_IFCOMBINE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_tree_ifcombine : public gimple_opt_pass
{
public:
  pass_tree_ifcombine (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_ifcombine, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute (function *) final override;

}; // class pass_tree_ifcombine

unsigned int
pass_tree_ifcombine::execute (function *fun)
{
  basic_block *bbs;
  bool cfg_changed = false;
  int i;

  bbs = single_pred_before_succ_order ();
  calculate_dominance_info (CDI_DOMINATORS);
  mark_ssa_maybe_undefs ();

  /* Search every basic block for COND_EXPR we may be able to optimize.

     We walk the blocks in order that guarantees that a block with
     a single predecessor is processed after the predecessor.
     This ensures that we collapse outter ifs before visiting the
     inner ones, and also that we do not try to visit a removed
     block.  This is opposite of PHI-OPT, because we cascade the
     combining rather than cascading PHIs. */
  for (i = n_basic_blocks_for_fn (fun) - NUM_FIXED_BLOCKS - 1; i >= 0; i--)
    {
      basic_block bb = bbs[i];

      if (safe_is_a <gcond *> (*gsi_last_bb (bb)))
	if (tree_ssa_ifcombine_bb (bb))
	  cfg_changed |= true;
    }

  free (bbs);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_tree_ifcombine (gcc::context *ctxt)
{
  return new pass_tree_ifcombine (ctxt);
}
