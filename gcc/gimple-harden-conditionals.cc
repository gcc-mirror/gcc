/* Harden conditionals.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimplify.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "basic-block.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "tree-eh.h"
#include "sbitmap.h"
#include "diagnostic.h"
#include "intl.h"

namespace {

/* These passes introduces redundant, but reversed conditionals at
   compares, such as those used in conditional branches, and those
   that compute boolean results.  This doesn't make much sense for
   abstract CPUs, but this kind of hardening may avoid undesirable
   execution paths on actual CPUs under such attacks as of power
   deprivation.  */

/* Define a pass to harden conditionals other than branches.  */

const pass_data pass_data_harden_compares = {
  GIMPLE_PASS,
  "hardcmp",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg | PROP_ssa, // properties_required
  0,	    // properties_provided
  0,	    // properties_destroyed
  0,	    // properties_start
  TODO_update_ssa
  | TODO_cleanup_cfg
  | TODO_verify_il, // properties_finish
};

class pass_harden_compares : public gimple_opt_pass
{
public:
  pass_harden_compares (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_harden_compares, ctxt)
  {}
  opt_pass *clone ()  final override
  {
    return new pass_harden_compares (m_ctxt);
  }
  bool gate (function *) final override
  {
    return flag_harden_compares;
  }
  unsigned int execute (function *) final override;
};

/* Define a pass to harden conditionals in branches.  This pass must
   run after the above, otherwise it will re-harden the checks
   introduced by the above.  */

const pass_data pass_data_harden_conditional_branches = {
  GIMPLE_PASS,
  "hardcbr",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg | PROP_ssa, // properties_required
  0,	    // properties_provided
  0,	    // properties_destroyed
  0,	    // properties_start
  TODO_update_ssa
  | TODO_cleanup_cfg
  | TODO_verify_il, // properties_finish
};

class pass_harden_conditional_branches : public gimple_opt_pass
{
public:
  pass_harden_conditional_branches (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_harden_conditional_branches, ctxt)
  {}
  opt_pass *clone () final override
  {
    return new pass_harden_conditional_branches (m_ctxt);
  }
  bool gate (function *) final override
  {
    return flag_harden_conditional_branches;
  }
  unsigned int execute (function *) final override;
};

}

/* If VAL is an SSA name, return an SSA name holding the same value,
   but without the compiler's knowing that it holds the same value, so
   that uses thereof can't be optimized the way VAL might.  Insert
   stmts that initialize it before *GSIP, with LOC.

   Otherwise, VAL must be an invariant, returned unchanged.  */

static inline tree
detach_value (location_t loc, gimple_stmt_iterator *gsip, tree val)
{
  if (TREE_CONSTANT (val) || TREE_CODE (val) != SSA_NAME)
    {
      gcc_checking_assert (is_gimple_min_invariant (val));
      return val;
    }

  /* Create a SSA "copy" of VAL.  It would be nice to have it named
     after the corresponding variable, but sharing the same decl is
     problematic when VAL is a DECL_BY_REFERENCE RESULT_DECL, and
     copying just the identifier hits -fcompare-debug failures.  */
  tree ret = make_ssa_name (TREE_TYPE (val));

  /* Some modes won't fit in general regs, so we fall back to memory
     for them.  ??? It would be ideal to try to identify an alternate,
     wider or more suitable register class, and use the corresponding
     constraint, but there's no logic to go from register class to
     constraint, even if there is a corresponding constraint, and even
     if we could enumerate constraints, we can't get to their string
     either.  So this will do for now.  */
  bool need_memory = true;
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (val));
  if (mode != BLKmode)
    for (int i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (TEST_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], i)
	  && targetm.hard_regno_mode_ok (i, mode))
	{
	  need_memory = false;
	  break;
	}

  tree asminput = val;
  tree asmoutput = ret;
  const char *constraint_out = need_memory ? "=m" : "=g";
  const char *constraint_in = need_memory ? "m" : "0";

  if (need_memory)
    {
      tree temp = create_tmp_var (TREE_TYPE (val), "dtch");
      mark_addressable (temp);

      gassign *copyin = gimple_build_assign (temp, asminput);
      gimple_set_location (copyin, loc);
      gsi_insert_before (gsip, copyin, GSI_SAME_STMT);

      asminput = asmoutput = temp;
    }

  /* Output an asm statement with matching input and output.  It does
     nothing, but after it the compiler no longer knows the output
     still holds the same value as the input.  */
  vec<tree, va_gc> *inputs = NULL;
  vec<tree, va_gc> *outputs = NULL;
  vec_safe_push (outputs,
		 build_tree_list
		 (build_tree_list
		  (NULL_TREE, build_string (strlen (constraint_out),
					    constraint_out)),
		  asmoutput));
  vec_safe_push (inputs,
		 build_tree_list
		 (build_tree_list
		  (NULL_TREE, build_string (strlen (constraint_in),
					    constraint_in)),
		  asminput));
  gasm *detach = gimple_build_asm_vec ("", inputs, outputs,
				       NULL, NULL);
  gimple_set_location (detach, loc);
  gsi_insert_before (gsip, detach, GSI_SAME_STMT);

  if (need_memory)
    {
      gassign *copyout = gimple_build_assign (ret, asmoutput);
      gimple_set_location (copyout, loc);
      gsi_insert_before (gsip, copyout, GSI_SAME_STMT);
      SSA_NAME_DEF_STMT (ret) = copyout;

      gassign *clobber = gimple_build_assign (asmoutput,
					      build_clobber
					      (TREE_TYPE (asmoutput)));
      gimple_set_location (clobber, loc);
      gsi_insert_before (gsip, clobber, GSI_SAME_STMT);
    }
  else
    SSA_NAME_DEF_STMT (ret) = detach;

  return ret;
}

/* Build a cond stmt out of COP, LHS, RHS, insert it before *GSIP with
   location LOC.  *GSIP must be at the end of a basic block.  The succ
   edge out of the block becomes the true or false edge opposite to
   that in FLAGS.  Create a new block with a single trap stmt, in the
   cold partition if the function is partitioned,, and a new edge to
   it as the other edge for the cond.  */

static inline void
insert_check_and_trap (location_t loc, gimple_stmt_iterator *gsip,
		       int flags, enum tree_code cop, tree lhs, tree rhs)
{
  basic_block chk = gsi_bb (*gsip);

  gcond *cond = gimple_build_cond (cop, lhs, rhs, NULL, NULL);
  gimple_set_location (cond, loc);
  gsi_insert_before (gsip, cond, GSI_SAME_STMT);

  basic_block trp = create_empty_bb (chk);
  trp->count = profile_count::zero ();

  gimple_stmt_iterator gsit = gsi_after_labels (trp);
  gcall *trap = gimple_build_call (builtin_decl_explicit (BUILT_IN_TRAP), 0);
  gimple_call_set_ctrl_altering (trap, true);
  gimple_set_location (trap, loc);
  gsi_insert_before (&gsit, trap, GSI_SAME_STMT);

  if (dump_file)
    fprintf (dump_file,
	     "Adding reversed compare to block %i, and trap to block %i\n",
	     chk->index, trp->index);

  if (BB_PARTITION (chk))
    BB_SET_PARTITION (trp, BB_COLD_PARTITION);

  int true_false_flag = flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
  gcc_assert (true_false_flag);
  int neg_true_false_flag = (~flags) & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

  /* Remove the fallthru bit, and set the truth value for the
     preexisting edge and for the newly-created one.  In hardcbr,
     FLAGS is taken from the edge of the original cond expr that we're
     dealing with, so the reversed compare is expected to yield the
     negated result, and the same result calls for a trap.  In
     hardcmp, we're comparing the boolean results of the original and
     of the reversed compare, so we're passed FLAGS to trap on
     equality.  */
  single_succ_edge (chk)->flags &= ~EDGE_FALLTHRU;
  single_succ_edge (chk)->flags |= neg_true_false_flag;
  single_succ_edge (chk)->probability = profile_probability::always ();
  edge e = make_edge (chk, trp, true_false_flag);
  e->goto_locus = loc;
  e->probability = profile_probability::never ();

  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, trp, chk);
  if (current_loops)
    add_bb_to_loop (trp, current_loops->tree_root);
}

/* Split edge E, and insert_check_and_trap (see above) in the
   newly-created block, using already-detached copies of LHS's and
   RHS's values (see detach_value above) for the COP compare.  */

static inline void
insert_edge_check_and_trap (location_t loc, edge e,
			    enum tree_code cop, tree lhs, tree rhs)
{
  int flags = e->flags;
  basic_block src = e->src;
  basic_block dest = e->dest;
  location_t eloc = e->goto_locus;

  basic_block chk = split_edge (e);
  e = NULL;

  single_pred_edge (chk)->goto_locus = loc;
  single_succ_edge (chk)->goto_locus = eloc;

  if (dump_file)
    fprintf (dump_file,
	     "Splitting edge %i->%i into block %i\n",
	     src->index, dest->index, chk->index);

  gimple_stmt_iterator gsik = gsi_after_labels (chk);

  insert_check_and_trap (loc, &gsik, flags, cop, lhs, rhs);
}

/* Harden cond stmts at the end of FUN's blocks.  */

unsigned int
pass_harden_conditional_branches::execute (function *fun)
{
  /* Record the preexisting blocks, to avoid visiting newly-created
     blocks.  */
  auto_sbitmap to_visit (last_basic_block_for_fn (fun));
  bitmap_clear (to_visit);

  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    bitmap_set_bit (to_visit, bb->index);

  sbitmap_iterator it;
  unsigned i;
  EXECUTE_IF_SET_IN_BITMAP (to_visit, 0, i, it)
    {
      bb = BASIC_BLOCK_FOR_FN (fun, i);

      gimple_stmt_iterator gsi = gsi_last_bb (bb);

      if (gsi_end_p (gsi))
	continue;

      gcond *cond = dyn_cast <gcond *> (gsi_stmt (gsi));
      if (!cond)
	continue;

      /* Turn:

	 if (x op y) goto l1; else goto l2;

	 into:

	 if (x op y) goto l1'; else goto l2';
	 l1': if (x' cop y') goto l1'trap; else goto l1;
	 l1'trap: __builtin_trap ();
	 l2': if (x' cop y') goto l2; else goto l2'trap;
	 l2'trap: __builtin_trap ();

	 where cop is a complementary boolean operation to op; l1', l1'trap,
	 l2' and l2'trap are newly-created labels; and x' and y' hold the same
	 value as x and y, but in a way that does not enable the compiler to
	 optimize the redundant compare away.
      */

      enum tree_code op = gimple_cond_code (cond);
      tree lhs = gimple_cond_lhs (cond);
      tree rhs = gimple_cond_rhs (cond);
      location_t loc = gimple_location (cond);

      enum tree_code cop = invert_tree_comparison (op, HONOR_NANS (lhs));

      if (cop == ERROR_MARK)
	/* ??? Can we do better?  */
	continue;

      /* Detach the values before the compares.  If we do so later,
	 the compiler may use values inferred from the compares.  */
      bool same_p = (lhs == rhs);
      lhs = detach_value (loc, &gsi, lhs);
      rhs = same_p ? lhs : detach_value (loc, &gsi, rhs);

      insert_edge_check_and_trap (loc, EDGE_SUCC (bb, 0), cop, lhs, rhs);
      insert_edge_check_and_trap (loc, EDGE_SUCC (bb, 1), cop, lhs, rhs);
    }

  return 0;
}

/* Instantiate a hardcbr pass.  */

gimple_opt_pass *
make_pass_harden_conditional_branches (gcc::context *ctxt)
{
  return new pass_harden_conditional_branches (ctxt);
}

/* Return the fallthru edge of a block whose other edge is an EH
   edge.  If EHP is not NULL, store the EH edge in it.  */
static inline edge
non_eh_succ_edge (basic_block bb, edge *ehp = NULL)
{
  gcc_checking_assert (EDGE_COUNT (bb->succs) == 2);

  edge ret = find_fallthru_edge (bb->succs);

  int eh_idx = EDGE_SUCC (bb, 0) == ret;
  edge eh = EDGE_SUCC (bb, eh_idx);

  gcc_checking_assert (!(ret->flags & EDGE_EH)
		       && (eh->flags & EDGE_EH));

  if (ehp)
    *ehp = eh;

  return ret;
}

/* Harden boolean-yielding compares in FUN.  */

unsigned int
pass_harden_compares::execute (function *fun)
{
  /* Record the preexisting blocks, to avoid visiting newly-created
     blocks.  */
  auto_sbitmap to_visit (last_basic_block_for_fn (fun));
  bitmap_clear (to_visit);

  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    bitmap_set_bit (to_visit, bb->index);

  sbitmap_iterator it;
  unsigned i;
  EXECUTE_IF_SET_IN_BITMAP (to_visit, 0, i, it)
    {
      bb = BASIC_BLOCK_FOR_FN (fun, i);

      for (gimple_stmt_iterator gsi = gsi_last_bb (bb);
	   !gsi_end_p (gsi); gsi_prev (&gsi))
	{
	  gassign *asgn = dyn_cast <gassign *> (gsi_stmt (gsi));
	  if (!asgn)
	    continue;

	  /* Turn:

	     z = x op y;

	     into:

	     z = x op y;
	     z' = x' cop y';
	     if (z == z') __builtin_trap ();

	     where cop is a complementary boolean operation to op; and x'
	     and y' hold the same value as x and y, but in a way that does
	     not enable the compiler to optimize the redundant compare
	     away.
	  */

	  enum tree_code op = gimple_assign_rhs_code (asgn);

	  enum tree_code cop;

	  switch (op)
	    {
	    case EQ_EXPR:
	    case NE_EXPR:
	    case GT_EXPR:
	    case GE_EXPR:
	    case LT_EXPR:
	    case LE_EXPR:
	    case LTGT_EXPR:
	    case UNEQ_EXPR:
	    case UNGT_EXPR:
	    case UNGE_EXPR:
	    case UNLT_EXPR:
	    case UNLE_EXPR:
	    case ORDERED_EXPR:
	    case UNORDERED_EXPR:
	      cop = invert_tree_comparison (op,
					    HONOR_NANS
					    (gimple_assign_rhs1 (asgn)));

	      if (cop == ERROR_MARK)
		/* ??? Can we do better?  */
		continue;

	      break;

	      /* ??? Maybe handle these too?  */
	    case TRUTH_NOT_EXPR:
	      /* ??? The code below assumes binary ops, it would have to
		 be adjusted for TRUTH_NOT_EXPR, since it's unary.  */
	    case TRUTH_ANDIF_EXPR:
	    case TRUTH_ORIF_EXPR:
	    case TRUTH_AND_EXPR:
	    case TRUTH_OR_EXPR:
	    case TRUTH_XOR_EXPR:
	    default:
	      continue;
	    }

	  /* These are the operands for the verification.  */
	  tree lhs = gimple_assign_lhs (asgn);
	  tree op1 = gimple_assign_rhs1 (asgn);
	  tree op2 = gimple_assign_rhs2 (asgn);
	  location_t loc = gimple_location (asgn);

	  /* Vector booleans can't be used in conditional branches.  ???
	     Can we do better?  How to reduce compare and
	     reversed-compare result vectors to a single boolean?  */
	  if (VECTOR_TYPE_P (TREE_TYPE (op1)))
	    continue;

	  /* useless_type_conversion_p enables conversions from 1-bit
	     integer types to boolean to be discarded.  */
	  gcc_checking_assert (TREE_CODE (TREE_TYPE (lhs)) == BOOLEAN_TYPE
			       || (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
				   && TYPE_PRECISION (TREE_TYPE (lhs)) == 1));

	  tree rhs = copy_ssa_name (lhs);

	  /* Detach the values before the compares, so that the
	     compiler infers nothing from them, not even from a
	     throwing compare that didn't throw.  */
	  bool same_p = (op1 == op2);
	  op1 = detach_value (loc, &gsi, op1);
	  op2 = same_p ? op1 : detach_value (loc, &gsi, op2);

	  gimple_stmt_iterator gsi_split = gsi;
	  /* Don't separate the original assignment from debug stmts
	     that might be associated with it, and arrange to split the
	     block after debug stmts, so as to make sure the split block
	     won't be debug stmts only.  */
	  gsi_next_nondebug (&gsi_split);

	  bool throwing_compare_p = stmt_ends_bb_p (asgn);
	  if (throwing_compare_p)
	    {
	      basic_block nbb = split_edge (non_eh_succ_edge
					    (gimple_bb (asgn)));
	      gsi_split = gsi_start_bb (nbb);

	      if (dump_file)
		fprintf (dump_file,
			 "Splitting non-EH edge from block %i into %i"
			 " after a throwing compare\n",
			 gimple_bb (asgn)->index, nbb->index);
	    }

	  gassign *asgnck = gimple_build_assign (rhs, cop, op1, op2);
	  gimple_set_location (asgnck, loc);
	  gsi_insert_before (&gsi_split, asgnck, GSI_SAME_STMT);

	  /* We wish to insert a cond_expr after the compare, so arrange
	     for it to be at the end of a block if it isn't, and for it
	     to have a single successor in case there's more than
	     one, as in PR104975.  */
	  if (!gsi_end_p (gsi_split)
	      || !single_succ_p (gsi_bb (gsi_split)))
	    {
	      if (!gsi_end_p (gsi_split))
		gsi_prev (&gsi_split);
	      else
		gsi_split = gsi_last_bb (gsi_bb (gsi_split));
	      basic_block obb = gsi_bb (gsi_split);
	      basic_block nbb = split_block (obb, gsi_stmt (gsi_split))->dest;
	      gsi_next (&gsi_split);
	      gcc_checking_assert (gsi_end_p (gsi_split));

	      single_succ_edge (bb)->goto_locus = loc;

	      if (dump_file)
		fprintf (dump_file,
			 "Splitting block %i into %i"
			 " before the conditional trap branch\n",
			 obb->index, nbb->index);
	    }

	  /* If the check assignment must end a basic block, we can't
	     insert the conditional branch in the same block, so split
	     the block again, and prepare to insert the conditional
	     branch in the new block.

	     Also assign an EH region to the compare.  Even though it's
	     unlikely that the hardening compare will throw after the
	     original compare didn't, the compiler won't even know that
	     it's the same compare operands, so add the EH edge anyway.  */
	  if (throwing_compare_p)
	    {
	      add_stmt_to_eh_lp (asgnck, lookup_stmt_eh_lp (asgn));
	      edge eh = make_eh_edge (asgnck);
	      /* This compare looks like it could raise an exception,
		 but it's dominated by the original compare, that
		 would raise an exception first, so the EH edge from
		 this one is never really taken.  */
	      eh->probability = profile_probability::never ();
	      if (eh->dest->count.initialized_p ())
		eh->dest->count += eh->count ();
	      else
		eh->dest->count = eh->count ();

	      edge ckeh;
	      basic_block nbb = split_edge (non_eh_succ_edge
					    (gimple_bb (asgnck), &ckeh));
	      gcc_checking_assert (eh == ckeh);
	      gsi_split = gsi_start_bb (nbb);

	      if (dump_file)
		fprintf (dump_file,
			 "Splitting non-EH edge from block %i into %i after"
			 " the newly-inserted reversed throwing compare\n",
			 gimple_bb (asgnck)->index, nbb->index);

	      if (!gimple_seq_empty_p (phi_nodes (ckeh->dest)))
		{
		  edge aseh;
		  non_eh_succ_edge (gimple_bb (asgn), &aseh);

		  gcc_checking_assert (aseh->dest == ckeh->dest);

		  for (gphi_iterator psi = gsi_start_phis (ckeh->dest);
		       !gsi_end_p (psi); gsi_next (&psi))
		    {
		      gphi *phi = psi.phi ();
		      add_phi_arg (phi, PHI_ARG_DEF_FROM_EDGE (phi, aseh), ckeh,
				   gimple_phi_arg_location_from_edge (phi, aseh));
		    }

		  if (dump_file)
		    fprintf (dump_file,
			     "Copying PHI args in EH block %i from %i to %i\n",
			     aseh->dest->index, aseh->src->index,
			     ckeh->src->index);
		}
	    }

	  gcc_checking_assert (single_succ_p (gsi_bb (gsi_split)));

	  insert_check_and_trap (loc, &gsi_split, EDGE_TRUE_VALUE,
				 EQ_EXPR, lhs, rhs);
	}
    }

  return 0;
}

/* Instantiate a hardcmp pass.  */

gimple_opt_pass *
make_pass_harden_compares (gcc::context *ctxt)
{
  return new pass_harden_compares (ctxt);
}
