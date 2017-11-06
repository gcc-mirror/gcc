/* Global, SSA-based optimizations using mathematical identities.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

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
   we notice that a division has the same divisor multiple times.

   Of course, like in PRE, we don't insert a division if a dominator
   already has one.  However, this cannot be done as an extension of
   PRE for several reasons.

   First of all, with some experiments it was found out that the
   transformation is not always useful if there are only two divisions
   by the same divisor.  This is probably because modern processors
   can pipeline the divisions; on older, in-order processors it should
   still be effective to optimize two divisions by the same number.
   We make this a param, and it shall be called N in the remainder of
   this comment.

   Second, if trapping math is active, we have less freedom on where
   to insert divisions: we can only do so in basic blocks that already
   contain one.  (If divisions don't trap, instead, we can insert
   divisions elsewhere, which will be in blocks that are common dominators
   of those that have the division).

   We really don't want to compute the reciprocal unless a division will
   be found.  To do this, we won't insert the division in a basic block
   that has less than N divisions *post-dominating* it.

   The algorithm constructs a subset of the dominator tree, holding the
   blocks containing the divisions and the common dominators to them,
   and walk it twice.  The first walk is in post-order, and it annotates
   each block with the number of divisions that post-dominate it: this
   gives information on where divisions can be inserted profitably.
   The second walk is in pre-order, and it inserts divisions as explained
   above, and replaces divisions by multiplications.

   In the best case, the cost of the pass is O(n_statements).  In the
   worst-case, the cost is due to creating the dominator tree subset,
   with a cost of O(n_basic_blocks ^ 2); however this can only happen
   for n_statements / n_basic_blocks statements.  So, the amortized cost
   of creating the dominator tree subset is O(n_basic_blocks) and the
   worst-case cost of the pass is O(n_statements * n_basic_blocks).

   More practically, the cost will be small because there are few
   divisions, and they tend to be in the same basic block, so insert_bb
   is called very few times.

   If we did this using domwalk.c, an efficient implementation would have
   to work on all the variables in a single pass, because we could not
   work on just a subset of the dominator tree, as we do now, and the
   cost would also be something like O(n_statements * n_basic_blocks).
   The data structures would be more complex in order to work on all the
   variables in a single pass.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "stor-layout.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "builtins.h"
#include "params.h"
#include "internal-fn.h"
#include "case-cfn-macros.h"
#include "optabs-libfuncs.h"
#include "tree-eh.h"
#include "targhooks.h"

/* This structure represents one basic block that either computes a
   division, or is a common dominator for basic block that compute a
   division.  */
struct occurrence {
  /* The basic block represented by this structure.  */
  basic_block bb;

  /* If non-NULL, the SSA_NAME holding the definition for a reciprocal
     inserted in BB.  */
  tree recip_def;

  /* If non-NULL, the GIMPLE_ASSIGN for a reciprocal computation that
     was inserted in BB.  */
  gimple *recip_def_stmt;

  /* Pointer to a list of "struct occurrence"s for blocks dominated
     by BB.  */
  struct occurrence *children;

  /* Pointer to the next "struct occurrence"s in the list of blocks
     sharing a common dominator.  */
  struct occurrence *next;

  /* The number of divisions that are in BB before compute_merit.  The
     number of divisions that are in BB or post-dominate it after
     compute_merit.  */
  int num_divisions;

  /* True if the basic block has a division, false if it is a common
     dominator for basic blocks that do.  If it is false and trapping
     math is active, BB is not a candidate for inserting a reciprocal.  */
  bool bb_has_division;
};

static struct
{
  /* Number of 1.0/X ops inserted.  */
  int rdivs_inserted;

  /* Number of 1.0/FUNC ops inserted.  */
  int rfuncs_inserted;
} reciprocal_stats;

static struct
{
  /* Number of cexpi calls inserted.  */
  int inserted;
} sincos_stats;

static struct
{
  /* Number of hand-written 16-bit nop / bswaps found.  */
  int found_16bit;

  /* Number of hand-written 32-bit nop / bswaps found.  */
  int found_32bit;

  /* Number of hand-written 64-bit nop / bswaps found.  */
  int found_64bit;
} nop_stats, bswap_stats;

static struct
{
  /* Number of widening multiplication ops inserted.  */
  int widen_mults_inserted;

  /* Number of integer multiply-and-accumulate ops inserted.  */
  int maccs_inserted;

  /* Number of fp fused multiply-add ops inserted.  */
  int fmas_inserted;

  /* Number of divmod calls inserted.  */
  int divmod_calls_inserted;
} widen_mul_stats;

/* The instance of "struct occurrence" representing the highest
   interesting block in the dominator tree.  */
static struct occurrence *occ_head;

/* Allocation pool for getting instances of "struct occurrence".  */
static object_allocator<occurrence> *occ_pool;



/* Allocate and return a new struct occurrence for basic block BB, and
   whose children list is headed by CHILDREN.  */
static struct occurrence *
occ_new (basic_block bb, struct occurrence *children)
{
  struct occurrence *occ;

  bb->aux = occ = occ_pool->allocate ();
  memset (occ, 0, sizeof (struct occurrence));

  occ->bb = bb;
  occ->children = children;
  return occ;
}


/* Insert NEW_OCC into our subset of the dominator tree.  P_HEAD points to a
   list of "struct occurrence"s, one per basic block, having IDOM as
   their common dominator.

   We try to insert NEW_OCC as deep as possible in the tree, and we also
   insert any other block that is a common dominator for BB and one
   block already in the tree.  */

static void
insert_bb (struct occurrence *new_occ, basic_block idom,
	   struct occurrence **p_head)
{
  struct occurrence *occ, **p_occ;

  for (p_occ = p_head; (occ = *p_occ) != NULL; )
    {
      basic_block bb = new_occ->bb, occ_bb = occ->bb;
      basic_block dom = nearest_common_dominator (CDI_DOMINATORS, occ_bb, bb);
      if (dom == bb)
	{
	  /* BB dominates OCC_BB.  OCC becomes NEW_OCC's child: remove OCC
	     from its list.  */
	  *p_occ = occ->next;
	  occ->next = new_occ->children;
	  new_occ->children = occ;

	  /* Try the next block (it may as well be dominated by BB).  */
	}

      else if (dom == occ_bb)
	{
	  /* OCC_BB dominates BB.  Tail recurse to look deeper.  */
	  insert_bb (new_occ, dom, &occ->children);
	  return;
	}

      else if (dom != idom)
	{
	  gcc_assert (!dom->aux);

	  /* There is a dominator between IDOM and BB, add it and make
	     two children out of NEW_OCC and OCC.  First, remove OCC from
	     its list.	*/
	  *p_occ = occ->next;
	  new_occ->next = occ;
	  occ->next = NULL;

	  /* None of the previous blocks has DOM as a dominator: if we tail
	     recursed, we would reexamine them uselessly. Just switch BB with
	     DOM, and go on looking for blocks dominated by DOM.  */
          new_occ = occ_new (dom, new_occ);
	}

      else
	{
	  /* Nothing special, go on with the next element.  */
	  p_occ = &occ->next;
	}
    }

  /* No place was found as a child of IDOM.  Make BB a sibling of IDOM.  */
  new_occ->next = *p_head;
  *p_head = new_occ;
}

/* Register that we found a division in BB.  */

static inline void
register_division_in (basic_block bb)
{
  struct occurrence *occ;

  occ = (struct occurrence *) bb->aux;
  if (!occ)
    {
      occ = occ_new (bb, NULL);
      insert_bb (occ, ENTRY_BLOCK_PTR_FOR_FN (cfun), &occ_head);
    }

  occ->bb_has_division = true;
  occ->num_divisions++;
}


/* Compute the number of divisions that postdominate each block in OCC and
   its children.  */

static void
compute_merit (struct occurrence *occ)
{
  struct occurrence *occ_child;
  basic_block dom = occ->bb;

  for (occ_child = occ->children; occ_child; occ_child = occ_child->next)
    {
      basic_block bb;
      if (occ_child->children)
        compute_merit (occ_child);

      if (flag_exceptions)
	bb = single_noncomplex_succ (dom);
      else
	bb = dom;

      if (dominated_by_p (CDI_POST_DOMINATORS, bb, occ_child->bb))
        occ->num_divisions += occ_child->num_divisions;
    }
}


/* Return whether USE_STMT is a floating-point division by DEF.  */
static inline bool
is_division_by (gimple *use_stmt, tree def)
{
  return is_gimple_assign (use_stmt)
	 && gimple_assign_rhs_code (use_stmt) == RDIV_EXPR
	 && gimple_assign_rhs2 (use_stmt) == def
	 /* Do not recognize x / x as valid division, as we are getting
	    confused later by replacing all immediate uses x in such
	    a stmt.  */
	 && gimple_assign_rhs1 (use_stmt) != def;
}

/* Walk the subset of the dominator tree rooted at OCC, setting the
   RECIP_DEF field to a definition of 1.0 / DEF that can be used in
   the given basic block.  The field may be left NULL, of course,
   if it is not possible or profitable to do the optimization.

   DEF_BSI is an iterator pointing at the statement defining DEF.
   If RECIP_DEF is set, a dominator already has a computation that can
   be used.  */

static void
insert_reciprocals (gimple_stmt_iterator *def_gsi, struct occurrence *occ,
		    tree def, tree recip_def, int threshold)
{
  tree type;
  gassign *new_stmt;
  gimple_stmt_iterator gsi;
  struct occurrence *occ_child;

  if (!recip_def
      && (occ->bb_has_division || !flag_trapping_math)
      && occ->num_divisions >= threshold)
    {
      /* Make a variable with the replacement and substitute it.  */
      type = TREE_TYPE (def);
      recip_def = create_tmp_reg (type, "reciptmp");
      new_stmt = gimple_build_assign (recip_def, RDIV_EXPR,
				      build_one_cst (type), def);

      if (occ->bb_has_division)
        {
          /* Case 1: insert before an existing division.  */
          gsi = gsi_after_labels (occ->bb);
          while (!gsi_end_p (gsi) && !is_division_by (gsi_stmt (gsi), def))
	    gsi_next (&gsi);

          gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);
        }
      else if (def_gsi && occ->bb == def_gsi->bb)
        {
          /* Case 2: insert right after the definition.  Note that this will
	     never happen if the definition statement can throw, because in
	     that case the sole successor of the statement's basic block will
	     dominate all the uses as well.  */
          gsi_insert_after (def_gsi, new_stmt, GSI_NEW_STMT);
        }
      else
        {
          /* Case 3: insert in a basic block not containing defs/uses.  */
          gsi = gsi_after_labels (occ->bb);
          gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);
        }

      reciprocal_stats.rdivs_inserted++;

      occ->recip_def_stmt = new_stmt;
    }

  occ->recip_def = recip_def;
  for (occ_child = occ->children; occ_child; occ_child = occ_child->next)
    insert_reciprocals (def_gsi, occ_child, def, recip_def, threshold);
}


/* Replace the division at USE_P with a multiplication by the reciprocal, if
   possible.  */

static inline void
replace_reciprocal (use_operand_p use_p)
{
  gimple *use_stmt = USE_STMT (use_p);
  basic_block bb = gimple_bb (use_stmt);
  struct occurrence *occ = (struct occurrence *) bb->aux;

  if (optimize_bb_for_speed_p (bb)
      && occ->recip_def && use_stmt != occ->recip_def_stmt)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
      gimple_assign_set_rhs_code (use_stmt, MULT_EXPR);
      SET_USE (use_p, occ->recip_def);
      fold_stmt_inplace (&gsi);
      update_stmt (use_stmt);
    }
}


/* Free OCC and return one more "struct occurrence" to be freed.  */

static struct occurrence *
free_bb (struct occurrence *occ)
{
  struct occurrence *child, *next;

  /* First get the two pointers hanging off OCC.  */
  next = occ->next;
  child = occ->children;
  occ->bb->aux = NULL;
  occ_pool->remove (occ);

  /* Now ensure that we don't recurse unless it is necessary.  */
  if (!child)
    return next;
  else
    {
      while (next)
	next = free_bb (next);

      return child;
    }
}


/* Look for floating-point divisions among DEF's uses, and try to
   replace them by multiplications with the reciprocal.  Add
   as many statements computing the reciprocal as needed.

   DEF must be a GIMPLE register of a floating-point type.  */

static void
execute_cse_reciprocals_1 (gimple_stmt_iterator *def_gsi, tree def)
{
  use_operand_p use_p;
  imm_use_iterator use_iter;
  struct occurrence *occ;
  int count = 0, threshold;

  gcc_assert (FLOAT_TYPE_P (TREE_TYPE (def)) && is_gimple_reg (def));

  FOR_EACH_IMM_USE_FAST (use_p, use_iter, def)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_division_by (use_stmt, def))
	{
	  register_division_in (gimple_bb (use_stmt));
	  count++;
	}
    }

  /* Do the expensive part only if we can hope to optimize something.  */
  threshold = targetm.min_divisions_for_recip_mul (TYPE_MODE (TREE_TYPE (def)));
  if (count >= threshold)
    {
      gimple *use_stmt;
      for (occ = occ_head; occ; occ = occ->next)
	{
	  compute_merit (occ);
	  insert_reciprocals (def_gsi, occ, def, NULL, threshold);
	}

      FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, def)
	{
	  if (is_division_by (use_stmt, def))
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, use_iter)
		replace_reciprocal (use_p);
	    }
	}
    }

  for (occ = occ_head; occ; )
    occ = free_bb (occ);

  occ_head = NULL;
}

/* Return an internal function that implements the reciprocal of CALL,
   or IFN_LAST if there is no such function that the target supports.  */

internal_fn
internal_fn_reciprocal (gcall *call)
{
  internal_fn ifn;

  switch (gimple_call_combined_fn (call))
    {
    CASE_CFN_SQRT:
    CASE_CFN_SQRT_FN:
      ifn = IFN_RSQRT;
      break;

    default:
      return IFN_LAST;
    }

  tree_pair types = direct_internal_fn_types (ifn, call);
  if (!direct_internal_fn_supported_p (ifn, types, OPTIMIZE_FOR_SPEED))
    return IFN_LAST;

  return ifn;
}

/* Go through all the floating-point SSA_NAMEs, and call
   execute_cse_reciprocals_1 on each of them.  */
namespace {

const pass_data pass_data_cse_reciprocals =
{
  GIMPLE_PASS, /* type */
  "recip", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_cse_reciprocals : public gimple_opt_pass
{
public:
  pass_cse_reciprocals (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cse_reciprocals, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return optimize && flag_reciprocal_math; }
  virtual unsigned int execute (function *);

}; // class pass_cse_reciprocals

unsigned int
pass_cse_reciprocals::execute (function *fun)
{
  basic_block bb;
  tree arg;

  occ_pool = new object_allocator<occurrence> ("dominators for recip");

  memset (&reciprocal_stats, 0, sizeof (reciprocal_stats));
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  if (flag_checking)
    FOR_EACH_BB_FN (bb, fun)
      gcc_assert (!bb->aux);

  for (arg = DECL_ARGUMENTS (fun->decl); arg; arg = DECL_CHAIN (arg))
    if (FLOAT_TYPE_P (TREE_TYPE (arg))
	&& is_gimple_reg (arg))
      {
	tree name = ssa_default_def (fun, arg);
	if (name)
	  execute_cse_reciprocals_1 (NULL, name);
      }

  FOR_EACH_BB_FN (bb, fun)
    {
      tree def;

      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  def = PHI_RESULT (phi);
	  if (! virtual_operand_p (def)
	      && FLOAT_TYPE_P (TREE_TYPE (def)))
	    execute_cse_reciprocals_1 (NULL, def);
	}

      for (gimple_stmt_iterator gsi = gsi_after_labels (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
        {
	  gimple *stmt = gsi_stmt (gsi);

	  if (gimple_has_lhs (stmt)
	      && (def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF)) != NULL
	      && FLOAT_TYPE_P (TREE_TYPE (def))
	      && TREE_CODE (def) == SSA_NAME)
	    execute_cse_reciprocals_1 (&gsi, def);
	}

      if (optimize_bb_for_size_p (bb))
        continue;

      /* Scan for a/func(b) and convert it to reciprocal a*rfunc(b).  */
      for (gimple_stmt_iterator gsi = gsi_after_labels (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
        {
	  gimple *stmt = gsi_stmt (gsi);

	  if (is_gimple_assign (stmt)
	      && gimple_assign_rhs_code (stmt) == RDIV_EXPR)
	    {
	      tree arg1 = gimple_assign_rhs2 (stmt);
	      gimple *stmt1;

	      if (TREE_CODE (arg1) != SSA_NAME)
		continue;

	      stmt1 = SSA_NAME_DEF_STMT (arg1);

	      if (is_gimple_call (stmt1)
		  && gimple_call_lhs (stmt1))
		{
		  bool fail;
		  imm_use_iterator ui;
		  use_operand_p use_p;
		  tree fndecl = NULL_TREE;

		  gcall *call = as_a <gcall *> (stmt1);
		  internal_fn ifn = internal_fn_reciprocal (call);
		  if (ifn == IFN_LAST)
		    {
		      fndecl = gimple_call_fndecl (call);
		      if (!fndecl
			  || DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_MD)
			continue;
		      fndecl = targetm.builtin_reciprocal (fndecl);
		      if (!fndecl)
			continue;
		    }

		  /* Check that all uses of the SSA name are divisions,
		     otherwise replacing the defining statement will do
		     the wrong thing.  */
		  fail = false;
		  FOR_EACH_IMM_USE_FAST (use_p, ui, arg1)
		    {
		      gimple *stmt2 = USE_STMT (use_p);
		      if (is_gimple_debug (stmt2))
			continue;
		      if (!is_gimple_assign (stmt2)
			  || gimple_assign_rhs_code (stmt2) != RDIV_EXPR
			  || gimple_assign_rhs1 (stmt2) == arg1
			  || gimple_assign_rhs2 (stmt2) != arg1)
			{
			  fail = true;
			  break;
			}
		    }
		  if (fail)
		    continue;

		  gimple_replace_ssa_lhs (call, arg1);
		  if (gimple_call_internal_p (call) != (ifn != IFN_LAST))
		    {
		      auto_vec<tree, 4> args;
		      for (unsigned int i = 0;
			   i < gimple_call_num_args (call); i++)
			args.safe_push (gimple_call_arg (call, i));
		      gcall *stmt2;
		      if (ifn == IFN_LAST)
			stmt2 = gimple_build_call_vec (fndecl, args);
		      else
			stmt2 = gimple_build_call_internal_vec (ifn, args);
		      gimple_call_set_lhs (stmt2, arg1);
		      if (gimple_vdef (call))
			{
			  gimple_set_vdef (stmt2, gimple_vdef (call));
			  SSA_NAME_DEF_STMT (gimple_vdef (stmt2)) = stmt2;
			}
		      gimple_call_set_nothrow (stmt2,
					       gimple_call_nothrow_p (call));
		      gimple_set_vuse (stmt2, gimple_vuse (call));
		      gimple_stmt_iterator gsi2 = gsi_for_stmt (call);
		      gsi_replace (&gsi2, stmt2, true);
		    }
		  else
		    {
		      if (ifn == IFN_LAST)
			gimple_call_set_fndecl (call, fndecl);
		      else
			gimple_call_set_internal_fn (call, ifn);
		      update_stmt (call);
		    }
		  reciprocal_stats.rfuncs_inserted++;

		  FOR_EACH_IMM_USE_STMT (stmt, ui, arg1)
		    {
		      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
		      gimple_assign_set_rhs_code (stmt, MULT_EXPR);
		      fold_stmt_inplace (&gsi);
		      update_stmt (stmt);
		    }
		}
	    }
	}
    }

  statistics_counter_event (fun, "reciprocal divs inserted",
			    reciprocal_stats.rdivs_inserted);
  statistics_counter_event (fun, "reciprocal functions inserted",
			    reciprocal_stats.rfuncs_inserted);

  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  delete occ_pool;
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_cse_reciprocals (gcc::context *ctxt)
{
  return new pass_cse_reciprocals (ctxt);
}

/* Records an occurrence at statement USE_STMT in the vector of trees
   STMTS if it is dominated by *TOP_BB or dominates it or this basic block
   is not yet initialized.  Returns true if the occurrence was pushed on
   the vector.  Adjusts *TOP_BB to be the basic block dominating all
   statements in the vector.  */

static bool
maybe_record_sincos (vec<gimple *> *stmts,
		     basic_block *top_bb, gimple *use_stmt)
{
  basic_block use_bb = gimple_bb (use_stmt);
  if (*top_bb
      && (*top_bb == use_bb
	  || dominated_by_p (CDI_DOMINATORS, use_bb, *top_bb)))
    stmts->safe_push (use_stmt);
  else if (!*top_bb
	   || dominated_by_p (CDI_DOMINATORS, *top_bb, use_bb))
    {
      stmts->safe_push (use_stmt);
      *top_bb = use_bb;
    }
  else
    return false;

  return true;
}

/* Look for sin, cos and cexpi calls with the same argument NAME and
   create a single call to cexpi CSEing the result in this case.
   We first walk over all immediate uses of the argument collecting
   statements that we can CSE in a vector and in a second pass replace
   the statement rhs with a REALPART or IMAGPART expression on the
   result of the cexpi call we insert before the use statement that
   dominates all other candidates.  */

static bool
execute_cse_sincos_1 (tree name)
{
  gimple_stmt_iterator gsi;
  imm_use_iterator use_iter;
  tree fndecl, res, type;
  gimple *def_stmt, *use_stmt, *stmt;
  int seen_cos = 0, seen_sin = 0, seen_cexpi = 0;
  auto_vec<gimple *> stmts;
  basic_block top_bb = NULL;
  int i;
  bool cfg_changed = false;

  type = TREE_TYPE (name);
  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, name)
    {
      if (gimple_code (use_stmt) != GIMPLE_CALL
	  || !gimple_call_lhs (use_stmt))
	continue;

      switch (gimple_call_combined_fn (use_stmt))
	{
	CASE_CFN_COS:
	  seen_cos |= maybe_record_sincos (&stmts, &top_bb, use_stmt) ? 1 : 0;
	  break;

	CASE_CFN_SIN:
	  seen_sin |= maybe_record_sincos (&stmts, &top_bb, use_stmt) ? 1 : 0;
	  break;

	CASE_CFN_CEXPI:
	  seen_cexpi |= maybe_record_sincos (&stmts, &top_bb, use_stmt) ? 1 : 0;
	  break;

	default:;
	}
    }

  if (seen_cos + seen_sin + seen_cexpi <= 1)
    return false;

  /* Simply insert cexpi at the beginning of top_bb but not earlier than
     the name def statement.  */
  fndecl = mathfn_built_in (type, BUILT_IN_CEXPI);
  if (!fndecl)
    return false;
  stmt = gimple_build_call (fndecl, 1, name);
  res = make_temp_ssa_name (TREE_TYPE (TREE_TYPE (fndecl)), stmt, "sincostmp");
  gimple_call_set_lhs (stmt, res);

  def_stmt = SSA_NAME_DEF_STMT (name);
  if (!SSA_NAME_IS_DEFAULT_DEF (name)
      && gimple_code (def_stmt) != GIMPLE_PHI
      && gimple_bb (def_stmt) == top_bb)
    {
      gsi = gsi_for_stmt (def_stmt);
      gsi_insert_after (&gsi, stmt, GSI_SAME_STMT);
    }
  else
    {
      gsi = gsi_after_labels (top_bb);
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
    }
  sincos_stats.inserted++;

  /* And adjust the recorded old call sites.  */
  for (i = 0; stmts.iterate (i, &use_stmt); ++i)
    {
      tree rhs = NULL;

      switch (gimple_call_combined_fn (use_stmt))
	{
	CASE_CFN_COS:
	  rhs = fold_build1 (REALPART_EXPR, type, res);
	  break;

	CASE_CFN_SIN:
	  rhs = fold_build1 (IMAGPART_EXPR, type, res);
	  break;

	CASE_CFN_CEXPI:
	  rhs = res;
	  break;

	default:;
	  gcc_unreachable ();
	}

	/* Replace call with a copy.  */
	stmt = gimple_build_assign (gimple_call_lhs (use_stmt), rhs);

	gsi = gsi_for_stmt (use_stmt);
	gsi_replace (&gsi, stmt, true);
	if (gimple_purge_dead_eh_edges (gimple_bb (stmt)))
	  cfg_changed = true;
    }

  return cfg_changed;
}

/* To evaluate powi(x,n), the floating point value x raised to the
   constant integer exponent n, we use a hybrid algorithm that
   combines the "window method" with look-up tables.  For an
   introduction to exponentiation algorithms and "addition chains",
   see section 4.6.3, "Evaluation of Powers" of Donald E. Knuth,
   "Seminumerical Algorithms", Vol. 2, "The Art of Computer Programming",
   3rd Edition, 1998, and Daniel M. Gordon, "A Survey of Fast Exponentiation
   Methods", Journal of Algorithms, Vol. 27, pp. 129-146, 1998.  */

/* Provide a default value for POWI_MAX_MULTS, the maximum number of
   multiplications to inline before calling the system library's pow
   function.  powi(x,n) requires at worst 2*bits(n)-2 multiplications,
   so this default never requires calling pow, powf or powl.  */

#ifndef POWI_MAX_MULTS
#define POWI_MAX_MULTS  (2*HOST_BITS_PER_WIDE_INT-2)
#endif

/* The size of the "optimal power tree" lookup table.  All
   exponents less than this value are simply looked up in the
   powi_table below.  This threshold is also used to size the
   cache of pseudo registers that hold intermediate results.  */
#define POWI_TABLE_SIZE 256

/* The size, in bits of the window, used in the "window method"
   exponentiation algorithm.  This is equivalent to a radix of
   (1<<POWI_WINDOW_SIZE) in the corresponding "m-ary method".  */
#define POWI_WINDOW_SIZE 3

/* The following table is an efficient representation of an
   "optimal power tree".  For each value, i, the corresponding
   value, j, in the table states than an optimal evaluation
   sequence for calculating pow(x,i) can be found by evaluating
   pow(x,j)*pow(x,i-j).  An optimal power tree for the first
   100 integers is given in Knuth's "Seminumerical algorithms".  */

static const unsigned char powi_table[POWI_TABLE_SIZE] =
  {
      0,   1,   1,   2,   2,   3,   3,   4,  /*   0 -   7 */
      4,   6,   5,   6,   6,  10,   7,   9,  /*   8 -  15 */
      8,  16,   9,  16,  10,  12,  11,  13,  /*  16 -  23 */
     12,  17,  13,  18,  14,  24,  15,  26,  /*  24 -  31 */
     16,  17,  17,  19,  18,  33,  19,  26,  /*  32 -  39 */
     20,  25,  21,  40,  22,  27,  23,  44,  /*  40 -  47 */
     24,  32,  25,  34,  26,  29,  27,  44,  /*  48 -  55 */
     28,  31,  29,  34,  30,  60,  31,  36,  /*  56 -  63 */
     32,  64,  33,  34,  34,  46,  35,  37,  /*  64 -  71 */
     36,  65,  37,  50,  38,  48,  39,  69,  /*  72 -  79 */
     40,  49,  41,  43,  42,  51,  43,  58,  /*  80 -  87 */
     44,  64,  45,  47,  46,  59,  47,  76,  /*  88 -  95 */
     48,  65,  49,  66,  50,  67,  51,  66,  /*  96 - 103 */
     52,  70,  53,  74,  54, 104,  55,  74,  /* 104 - 111 */
     56,  64,  57,  69,  58,  78,  59,  68,  /* 112 - 119 */
     60,  61,  61,  80,  62,  75,  63,  68,  /* 120 - 127 */
     64,  65,  65, 128,  66, 129,  67,  90,  /* 128 - 135 */
     68,  73,  69, 131,  70,  94,  71,  88,  /* 136 - 143 */
     72, 128,  73,  98,  74, 132,  75, 121,  /* 144 - 151 */
     76, 102,  77, 124,  78, 132,  79, 106,  /* 152 - 159 */
     80,  97,  81, 160,  82,  99,  83, 134,  /* 160 - 167 */
     84,  86,  85,  95,  86, 160,  87, 100,  /* 168 - 175 */
     88, 113,  89,  98,  90, 107,  91, 122,  /* 176 - 183 */
     92, 111,  93, 102,  94, 126,  95, 150,  /* 184 - 191 */
     96, 128,  97, 130,  98, 133,  99, 195,  /* 192 - 199 */
    100, 128, 101, 123, 102, 164, 103, 138,  /* 200 - 207 */
    104, 145, 105, 146, 106, 109, 107, 149,  /* 208 - 215 */
    108, 200, 109, 146, 110, 170, 111, 157,  /* 216 - 223 */
    112, 128, 113, 130, 114, 182, 115, 132,  /* 224 - 231 */
    116, 200, 117, 132, 118, 158, 119, 206,  /* 232 - 239 */
    120, 240, 121, 162, 122, 147, 123, 152,  /* 240 - 247 */
    124, 166, 125, 214, 126, 138, 127, 153,  /* 248 - 255 */
  };


/* Return the number of multiplications required to calculate
   powi(x,n) where n is less than POWI_TABLE_SIZE.  This is a
   subroutine of powi_cost.  CACHE is an array indicating
   which exponents have already been calculated.  */

static int
powi_lookup_cost (unsigned HOST_WIDE_INT n, bool *cache)
{
  /* If we've already calculated this exponent, then this evaluation
     doesn't require any additional multiplications.  */
  if (cache[n])
    return 0;

  cache[n] = true;
  return powi_lookup_cost (n - powi_table[n], cache)
	 + powi_lookup_cost (powi_table[n], cache) + 1;
}

/* Return the number of multiplications required to calculate
   powi(x,n) for an arbitrary x, given the exponent N.  This
   function needs to be kept in sync with powi_as_mults below.  */

static int
powi_cost (HOST_WIDE_INT n)
{
  bool cache[POWI_TABLE_SIZE];
  unsigned HOST_WIDE_INT digit;
  unsigned HOST_WIDE_INT val;
  int result;

  if (n == 0)
    return 0;

  /* Ignore the reciprocal when calculating the cost.  */
  val = (n < 0) ? -n : n;

  /* Initialize the exponent cache.  */
  memset (cache, 0, POWI_TABLE_SIZE * sizeof (bool));
  cache[1] = true;

  result = 0;

  while (val >= POWI_TABLE_SIZE)
    {
      if (val & 1)
	{
	  digit = val & ((1 << POWI_WINDOW_SIZE) - 1);
	  result += powi_lookup_cost (digit, cache)
		    + POWI_WINDOW_SIZE + 1;
	  val >>= POWI_WINDOW_SIZE;
	}
      else
	{
	  val >>= 1;
	  result++;
	}
    }

  return result + powi_lookup_cost (val, cache);
}

/* Recursive subroutine of powi_as_mults.  This function takes the
   array, CACHE, of already calculated exponents and an exponent N and
   returns a tree that corresponds to CACHE[1]**N, with type TYPE.  */

static tree
powi_as_mults_1 (gimple_stmt_iterator *gsi, location_t loc, tree type,
		 HOST_WIDE_INT n, tree *cache)
{
  tree op0, op1, ssa_target;
  unsigned HOST_WIDE_INT digit;
  gassign *mult_stmt;

  if (n < POWI_TABLE_SIZE && cache[n])
    return cache[n];

  ssa_target = make_temp_ssa_name (type, NULL, "powmult");

  if (n < POWI_TABLE_SIZE)
    {
      cache[n] = ssa_target;
      op0 = powi_as_mults_1 (gsi, loc, type, n - powi_table[n], cache);
      op1 = powi_as_mults_1 (gsi, loc, type, powi_table[n], cache);
    }
  else if (n & 1)
    {
      digit = n & ((1 << POWI_WINDOW_SIZE) - 1);
      op0 = powi_as_mults_1 (gsi, loc, type, n - digit, cache);
      op1 = powi_as_mults_1 (gsi, loc, type, digit, cache);
    }
  else
    {
      op0 = powi_as_mults_1 (gsi, loc, type, n >> 1, cache);
      op1 = op0;
    }

  mult_stmt = gimple_build_assign (ssa_target, MULT_EXPR, op0, op1);
  gimple_set_location (mult_stmt, loc);
  gsi_insert_before (gsi, mult_stmt, GSI_SAME_STMT);

  return ssa_target;
}

/* Convert ARG0**N to a tree of multiplications of ARG0 with itself.
   This function needs to be kept in sync with powi_cost above.  */

static tree
powi_as_mults (gimple_stmt_iterator *gsi, location_t loc,
	       tree arg0, HOST_WIDE_INT n)
{
  tree cache[POWI_TABLE_SIZE], result, type = TREE_TYPE (arg0);
  gassign *div_stmt;
  tree target;

  if (n == 0)
    return build_real (type, dconst1);

  memset (cache, 0,  sizeof (cache));
  cache[1] = arg0;

  result = powi_as_mults_1 (gsi, loc, type, (n < 0) ? -n : n, cache);
  if (n >= 0)
    return result;

  /* If the original exponent was negative, reciprocate the result.  */
  target = make_temp_ssa_name (type, NULL, "powmult");
  div_stmt = gimple_build_assign (target, RDIV_EXPR,
				  build_real (type, dconst1), result);
  gimple_set_location (div_stmt, loc);
  gsi_insert_before (gsi, div_stmt, GSI_SAME_STMT);

  return target;
}

/* ARG0 and N are the two arguments to a powi builtin in GSI with
   location info LOC.  If the arguments are appropriate, create an
   equivalent sequence of statements prior to GSI using an optimal
   number of multiplications, and return an expession holding the
   result.  */

static tree
gimple_expand_builtin_powi (gimple_stmt_iterator *gsi, location_t loc, 
			    tree arg0, HOST_WIDE_INT n)
{
  /* Avoid largest negative number.  */
  if (n != -n
      && ((n >= -1 && n <= 2)
	  || (optimize_function_for_speed_p (cfun)
	      && powi_cost (n) <= POWI_MAX_MULTS)))
    return powi_as_mults (gsi, loc, arg0, n);

  return NULL_TREE;
}

/* Build a gimple call statement that calls FN with argument ARG.
   Set the lhs of the call statement to a fresh SSA name.  Insert the
   statement prior to GSI's current position, and return the fresh
   SSA name.  */

static tree
build_and_insert_call (gimple_stmt_iterator *gsi, location_t loc,
		       tree fn, tree arg)
{
  gcall *call_stmt;
  tree ssa_target;

  call_stmt = gimple_build_call (fn, 1, arg);
  ssa_target = make_temp_ssa_name (TREE_TYPE (arg), NULL, "powroot");
  gimple_set_lhs (call_stmt, ssa_target);
  gimple_set_location (call_stmt, loc);
  gsi_insert_before (gsi, call_stmt, GSI_SAME_STMT);

  return ssa_target;
}

/* Build a gimple binary operation with the given CODE and arguments
   ARG0, ARG1, assigning the result to a new SSA name for variable
   TARGET.  Insert the statement prior to GSI's current position, and
   return the fresh SSA name.*/

static tree
build_and_insert_binop (gimple_stmt_iterator *gsi, location_t loc,
			const char *name, enum tree_code code,
			tree arg0, tree arg1)
{
  tree result = make_temp_ssa_name (TREE_TYPE (arg0), NULL, name);
  gassign *stmt = gimple_build_assign (result, code, arg0, arg1);
  gimple_set_location (stmt, loc);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
  return result;
}

/* Build a gimple reference operation with the given CODE and argument
   ARG, assigning the result to a new SSA name of TYPE with NAME.
   Insert the statement prior to GSI's current position, and return
   the fresh SSA name.  */

static inline tree
build_and_insert_ref (gimple_stmt_iterator *gsi, location_t loc, tree type,
		      const char *name, enum tree_code code, tree arg0)
{
  tree result = make_temp_ssa_name (type, NULL, name);
  gimple *stmt = gimple_build_assign (result, build1 (code, type, arg0));
  gimple_set_location (stmt, loc);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
  return result;
}

/* Build a gimple assignment to cast VAL to TYPE.  Insert the statement
   prior to GSI's current position, and return the fresh SSA name.  */

static tree
build_and_insert_cast (gimple_stmt_iterator *gsi, location_t loc,
		       tree type, tree val)
{
  tree result = make_ssa_name (type);
  gassign *stmt = gimple_build_assign (result, NOP_EXPR, val);
  gimple_set_location (stmt, loc);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
  return result;
}

struct pow_synth_sqrt_info
{
  bool *factors;
  unsigned int deepest;
  unsigned int num_mults;
};

/* Return true iff the real value C can be represented as a
   sum of powers of 0.5 up to N.  That is:
   C == SUM<i from 1..N> (a[i]*(0.5**i)) where a[i] is either 0 or 1.
   Record in INFO the various parameters of the synthesis algorithm such
   as the factors a[i], the maximum 0.5 power and the number of
   multiplications that will be required.  */

bool
representable_as_half_series_p (REAL_VALUE_TYPE c, unsigned n,
				 struct pow_synth_sqrt_info *info)
{
  REAL_VALUE_TYPE factor = dconsthalf;
  REAL_VALUE_TYPE remainder = c;

  info->deepest = 0;
  info->num_mults = 0;
  memset (info->factors, 0, n * sizeof (bool));

  for (unsigned i = 0; i < n; i++)
    {
      REAL_VALUE_TYPE res;

      /* If something inexact happened bail out now.  */
      if (real_arithmetic (&res, MINUS_EXPR, &remainder, &factor))
	return false;

      /* We have hit zero.  The number is representable as a sum
         of powers of 0.5.  */
      if (real_equal (&res, &dconst0))
	{
	  info->factors[i] = true;
	  info->deepest = i + 1;
	  return true;
	}
      else if (!REAL_VALUE_NEGATIVE (res))
	{
	  remainder = res;
	  info->factors[i] = true;
	  info->num_mults++;
	}
      else
	info->factors[i] = false;

      real_arithmetic (&factor, MULT_EXPR, &factor, &dconsthalf);
    }
  return false;
}

/* Return the tree corresponding to FN being applied
   to ARG N times at GSI and LOC.
   Look up previous results from CACHE if need be.
   cache[0] should contain just plain ARG i.e. FN applied to ARG 0 times.  */

static tree
get_fn_chain (tree arg, unsigned int n, gimple_stmt_iterator *gsi,
	      tree fn, location_t loc, tree *cache)
{
  tree res = cache[n];
  if (!res)
    {
      tree prev = get_fn_chain (arg, n - 1, gsi, fn, loc, cache);
      res = build_and_insert_call (gsi, loc, fn, prev);
      cache[n] = res;
    }

  return res;
}

/* Print to STREAM the repeated application of function FNAME to ARG
   N times.  So, for FNAME = "foo", ARG = "x", N = 2 it would print:
   "foo (foo (x))".  */

static void
print_nested_fn (FILE* stream, const char *fname, const char* arg,
		 unsigned int n)
{
  if (n == 0)
    fprintf (stream, "%s", arg);
  else
    {
      fprintf (stream, "%s (", fname);
      print_nested_fn (stream, fname, arg, n - 1);
      fprintf (stream, ")");
    }
}

/* Print to STREAM the fractional sequence of sqrt chains
   applied to ARG, described by INFO.  Used for the dump file.  */

static void
dump_fractional_sqrt_sequence (FILE *stream, const char *arg,
			        struct pow_synth_sqrt_info *info)
{
  for (unsigned int i = 0; i < info->deepest; i++)
    {
      bool is_set = info->factors[i];
      if (is_set)
	{
	  print_nested_fn (stream, "sqrt", arg, i + 1);
	  if (i != info->deepest - 1)
	    fprintf (stream, " * ");
	}
    }
}

/* Print to STREAM a representation of raising ARG to an integer
   power N.  Used for the dump file.  */

static void
dump_integer_part (FILE *stream, const char* arg, HOST_WIDE_INT n)
{
  if (n > 1)
    fprintf (stream, "powi (%s, " HOST_WIDE_INT_PRINT_DEC ")", arg, n);
  else if (n == 1)
    fprintf (stream, "%s", arg);
}

/* Attempt to synthesize a POW[F] (ARG0, ARG1) call using chains of
   square roots.  Place at GSI and LOC.  Limit the maximum depth
   of the sqrt chains to MAX_DEPTH.  Return the tree holding the
   result of the expanded sequence or NULL_TREE if the expansion failed.

   This routine assumes that ARG1 is a real number with a fractional part
   (the integer exponent case will have been handled earlier in
   gimple_expand_builtin_pow).

   For ARG1 > 0.0:
   * For ARG1 composed of a whole part WHOLE_PART and a fractional part
     FRAC_PART i.e. WHOLE_PART == floor (ARG1) and
                    FRAC_PART == ARG1 - WHOLE_PART:
     Produce POWI (ARG0, WHOLE_PART) * POW (ARG0, FRAC_PART) where
     POW (ARG0, FRAC_PART) is expanded as a product of square root chains
     if it can be expressed as such, that is if FRAC_PART satisfies:
     FRAC_PART == <SUM from i = 1 until MAX_DEPTH> (a[i] * (0.5**i))
     where integer a[i] is either 0 or 1.

     Example:
     POW (x, 3.625) == POWI (x, 3) * POW (x, 0.625)
       --> POWI (x, 3) * SQRT (x) * SQRT (SQRT (SQRT (x)))

   For ARG1 < 0.0 there are two approaches:
   * (A) Expand to 1.0 / POW (ARG0, -ARG1) where POW (ARG0, -ARG1)
         is calculated as above.

     Example:
     POW (x, -5.625) == 1.0 / POW (x, 5.625)
       -->  1.0 / (POWI (x, 5) * SQRT (x) * SQRT (SQRT (SQRT (x))))

   * (B) : WHOLE_PART := - ceil (abs (ARG1))
           FRAC_PART  := ARG1 - WHOLE_PART
     and expand to POW (x, FRAC_PART) / POWI (x, WHOLE_PART).
     Example:
     POW (x, -5.875) == POW (x, 0.125) / POWI (X, 6)
       --> SQRT (SQRT (SQRT (x))) / (POWI (x, 6))

   For ARG1 < 0.0 we choose between (A) and (B) depending on
   how many multiplications we'd have to do.
   So, for the example in (B): POW (x, -5.875), if we were to
   follow algorithm (A) we would produce:
   1.0 / POWI (X, 5) * SQRT (X) * SQRT (SQRT (X)) * SQRT (SQRT (SQRT (X)))
   which contains more multiplications than approach (B).

   Hopefully, this approach will eliminate potentially expensive POW library
   calls when unsafe floating point math is enabled and allow the compiler to
   further optimise the multiplies, square roots and divides produced by this
   function.  */

static tree
expand_pow_as_sqrts (gimple_stmt_iterator *gsi, location_t loc,
		     tree arg0, tree arg1, HOST_WIDE_INT max_depth)
{
  tree type = TREE_TYPE (arg0);
  machine_mode mode = TYPE_MODE (type);
  tree sqrtfn = mathfn_built_in (type, BUILT_IN_SQRT);
  bool one_over = true;

  if (!sqrtfn)
    return NULL_TREE;

  if (TREE_CODE (arg1) != REAL_CST)
    return NULL_TREE;

  REAL_VALUE_TYPE exp_init = TREE_REAL_CST (arg1);

  gcc_assert (max_depth > 0);
  tree *cache = XALLOCAVEC (tree, max_depth + 1);

  struct pow_synth_sqrt_info synth_info;
  synth_info.factors = XALLOCAVEC (bool, max_depth + 1);
  synth_info.deepest = 0;
  synth_info.num_mults = 0;

  bool neg_exp = REAL_VALUE_NEGATIVE (exp_init);
  REAL_VALUE_TYPE exp = real_value_abs (&exp_init);

  /* The whole and fractional parts of exp.  */
  REAL_VALUE_TYPE whole_part;
  REAL_VALUE_TYPE frac_part;

  real_floor (&whole_part, mode, &exp);
  real_arithmetic (&frac_part, MINUS_EXPR, &exp, &whole_part);


  REAL_VALUE_TYPE ceil_whole = dconst0;
  REAL_VALUE_TYPE ceil_fract = dconst0;

  if (neg_exp)
    {
      real_ceil (&ceil_whole, mode, &exp);
      real_arithmetic (&ceil_fract, MINUS_EXPR, &ceil_whole, &exp);
    }

  if (!representable_as_half_series_p (frac_part, max_depth, &synth_info))
    return NULL_TREE;

  /* Check whether it's more profitable to not use 1.0 / ...  */
  if (neg_exp)
    {
      struct pow_synth_sqrt_info alt_synth_info;
      alt_synth_info.factors = XALLOCAVEC (bool, max_depth + 1);
      alt_synth_info.deepest = 0;
      alt_synth_info.num_mults = 0;

      if (representable_as_half_series_p (ceil_fract, max_depth,
					   &alt_synth_info)
	  && alt_synth_info.deepest <= synth_info.deepest
	  && alt_synth_info.num_mults < synth_info.num_mults)
	{
	  whole_part = ceil_whole;
	  frac_part = ceil_fract;
	  synth_info.deepest = alt_synth_info.deepest;
	  synth_info.num_mults = alt_synth_info.num_mults;
	  memcpy (synth_info.factors, alt_synth_info.factors,
		  (max_depth + 1) * sizeof (bool));
	  one_over = false;
	}
    }

  HOST_WIDE_INT n = real_to_integer (&whole_part);
  REAL_VALUE_TYPE cint;
  real_from_integer (&cint, VOIDmode, n, SIGNED);

  if (!real_identical (&whole_part, &cint))
    return NULL_TREE;

  if (powi_cost (n) + synth_info.num_mults > POWI_MAX_MULTS)
    return NULL_TREE;

  memset (cache, 0, (max_depth + 1) * sizeof (tree));

  tree integer_res = n == 0 ? build_real (type, dconst1) : arg0;

  /* Calculate the integer part of the exponent.  */
  if (n > 1)
    {
      integer_res = gimple_expand_builtin_powi (gsi, loc, arg0, n);
      if (!integer_res)
	return NULL_TREE;
    }

  if (dump_file)
    {
      char string[64];

      real_to_decimal (string, &exp_init, sizeof (string), 0, 1);
      fprintf (dump_file, "synthesizing pow (x, %s) as:\n", string);

      if (neg_exp)
	{
	  if (one_over)
	    {
	      fprintf (dump_file, "1.0 / (");
	      dump_integer_part (dump_file, "x", n);
	      if (n > 0)
	        fprintf (dump_file, " * ");
	      dump_fractional_sqrt_sequence (dump_file, "x", &synth_info);
	      fprintf (dump_file, ")");
	    }
	  else
	    {
	      dump_fractional_sqrt_sequence (dump_file, "x", &synth_info);
	      fprintf (dump_file, " / (");
	      dump_integer_part (dump_file, "x", n);
	      fprintf (dump_file, ")");
	    }
	}
      else
	{
	  dump_fractional_sqrt_sequence (dump_file, "x", &synth_info);
	  if (n > 0)
	    fprintf (dump_file, " * ");
	  dump_integer_part (dump_file, "x", n);
	}

      fprintf (dump_file, "\ndeepest sqrt chain: %d\n", synth_info.deepest);
    }


  tree fract_res = NULL_TREE;
  cache[0] = arg0;

  /* Calculate the fractional part of the exponent.  */
  for (unsigned i = 0; i < synth_info.deepest; i++)
    {
      if (synth_info.factors[i])
	{
	  tree sqrt_chain = get_fn_chain (arg0, i + 1, gsi, sqrtfn, loc, cache);

	  if (!fract_res)
	      fract_res = sqrt_chain;

	  else
	    fract_res = build_and_insert_binop (gsi, loc, "powroot", MULT_EXPR,
					   fract_res, sqrt_chain);
	}
    }

  tree res = NULL_TREE;

  if (neg_exp)
    {
      if (one_over)
	{
	  if (n > 0)
	    res = build_and_insert_binop (gsi, loc, "powroot", MULT_EXPR,
					   fract_res, integer_res);
	  else
	    res = fract_res;

	  res = build_and_insert_binop (gsi, loc, "powrootrecip", RDIV_EXPR,
					  build_real (type, dconst1), res);
	}
      else
	{
	  res = build_and_insert_binop (gsi, loc, "powroot", RDIV_EXPR,
					 fract_res, integer_res);
	}
    }
  else
    res = build_and_insert_binop (gsi, loc, "powroot", MULT_EXPR,
				   fract_res, integer_res);
  return res;
}

/* ARG0 and ARG1 are the two arguments to a pow builtin call in GSI
   with location info LOC.  If possible, create an equivalent and
   less expensive sequence of statements prior to GSI, and return an
   expession holding the result.  */

static tree
gimple_expand_builtin_pow (gimple_stmt_iterator *gsi, location_t loc, 
			   tree arg0, tree arg1)
{
  REAL_VALUE_TYPE c, cint, dconst1_3, dconst1_4, dconst1_6;
  REAL_VALUE_TYPE c2, dconst3;
  HOST_WIDE_INT n;
  tree type, sqrtfn, cbrtfn, sqrt_arg0, result, cbrt_x, powi_cbrt_x;
  machine_mode mode;
  bool speed_p = optimize_bb_for_speed_p (gsi_bb (*gsi));
  bool hw_sqrt_exists, c_is_int, c2_is_int;

  dconst1_4 = dconst1;
  SET_REAL_EXP (&dconst1_4, REAL_EXP (&dconst1_4) - 2);

  /* If the exponent isn't a constant, there's nothing of interest
     to be done.  */
  if (TREE_CODE (arg1) != REAL_CST)
    return NULL_TREE;

  /* Don't perform the operation if flag_signaling_nans is on
     and the operand is a signaling NaN.  */
  if (HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg1)))
      && ((TREE_CODE (arg0) == REAL_CST
	   && REAL_VALUE_ISSIGNALING_NAN (TREE_REAL_CST (arg0)))
	  || REAL_VALUE_ISSIGNALING_NAN (TREE_REAL_CST (arg1))))
    return NULL_TREE;

  /* If the exponent is equivalent to an integer, expand to an optimal
     multiplication sequence when profitable.  */
  c = TREE_REAL_CST (arg1);
  n = real_to_integer (&c);
  real_from_integer (&cint, VOIDmode, n, SIGNED);
  c_is_int = real_identical (&c, &cint);

  if (c_is_int
      && ((n >= -1 && n <= 2)
	  || (flag_unsafe_math_optimizations
	      && speed_p
	      && powi_cost (n) <= POWI_MAX_MULTS)))
    return gimple_expand_builtin_powi (gsi, loc, arg0, n);

  /* Attempt various optimizations using sqrt and cbrt.  */
  type = TREE_TYPE (arg0);
  mode = TYPE_MODE (type);
  sqrtfn = mathfn_built_in (type, BUILT_IN_SQRT);

  /* Optimize pow(x,0.5) = sqrt(x).  This replacement is always safe
     unless signed zeros must be maintained.  pow(-0,0.5) = +0, while
     sqrt(-0) = -0.  */
  if (sqrtfn
      && real_equal (&c, &dconsthalf)
      && !HONOR_SIGNED_ZEROS (mode))
    return build_and_insert_call (gsi, loc, sqrtfn, arg0);

  hw_sqrt_exists = optab_handler (sqrt_optab, mode) != CODE_FOR_nothing;

  /* Optimize pow(x,1./3.) = cbrt(x).  This requires unsafe math
     optimizations since 1./3. is not exactly representable.  If x
     is negative and finite, the correct value of pow(x,1./3.) is
     a NaN with the "invalid" exception raised, because the value
     of 1./3. actually has an even denominator.  The correct value
     of cbrt(x) is a negative real value.  */
  cbrtfn = mathfn_built_in (type, BUILT_IN_CBRT);
  dconst1_3 = real_value_truncate (mode, dconst_third ());

  if (flag_unsafe_math_optimizations
      && cbrtfn
      && (!HONOR_NANS (mode) || tree_expr_nonnegative_p (arg0))
      && real_equal (&c, &dconst1_3))
    return build_and_insert_call (gsi, loc, cbrtfn, arg0);
  
  /* Optimize pow(x,1./6.) = cbrt(sqrt(x)).  Don't do this optimization
     if we don't have a hardware sqrt insn.  */
  dconst1_6 = dconst1_3;
  SET_REAL_EXP (&dconst1_6, REAL_EXP (&dconst1_6) - 1);

  if (flag_unsafe_math_optimizations
      && sqrtfn
      && cbrtfn
      && (!HONOR_NANS (mode) || tree_expr_nonnegative_p (arg0))
      && speed_p
      && hw_sqrt_exists
      && real_equal (&c, &dconst1_6))
    {
      /* sqrt(x)  */
      sqrt_arg0 = build_and_insert_call (gsi, loc, sqrtfn, arg0);

      /* cbrt(sqrt(x))  */
      return build_and_insert_call (gsi, loc, cbrtfn, sqrt_arg0);
    }


  /* Attempt to expand the POW as a product of square root chains.
     Expand the 0.25 case even when otpimising for size.  */
  if (flag_unsafe_math_optimizations
      && sqrtfn
      && hw_sqrt_exists
      && (speed_p || real_equal (&c, &dconst1_4))
      && !HONOR_SIGNED_ZEROS (mode))
    {
      unsigned int max_depth = speed_p
				? PARAM_VALUE (PARAM_MAX_POW_SQRT_DEPTH)
				: 2;

      tree expand_with_sqrts
	= expand_pow_as_sqrts (gsi, loc, arg0, arg1, max_depth);

      if (expand_with_sqrts)
	return expand_with_sqrts;
    }

  real_arithmetic (&c2, MULT_EXPR, &c, &dconst2);
  n = real_to_integer (&c2);
  real_from_integer (&cint, VOIDmode, n, SIGNED);
  c2_is_int = real_identical (&c2, &cint);

  /* Optimize pow(x,c), where 3c = n for some nonzero integer n, into

     powi(x, n/3) * powi(cbrt(x), n%3),                    n > 0;
     1.0 / (powi(x, abs(n)/3) * powi(cbrt(x), abs(n)%3)),  n < 0.

     Do not calculate the first factor when n/3 = 0.  As cbrt(x) is
     different from pow(x, 1./3.) due to rounding and behavior with
     negative x, we need to constrain this transformation to unsafe
     math and positive x or finite math.  */
  real_from_integer (&dconst3, VOIDmode, 3, SIGNED);
  real_arithmetic (&c2, MULT_EXPR, &c, &dconst3);
  real_round (&c2, mode, &c2);
  n = real_to_integer (&c2);
  real_from_integer (&cint, VOIDmode, n, SIGNED);
  real_arithmetic (&c2, RDIV_EXPR, &cint, &dconst3);
  real_convert (&c2, mode, &c2);

  if (flag_unsafe_math_optimizations
      && cbrtfn
      && (!HONOR_NANS (mode) || tree_expr_nonnegative_p (arg0))
      && real_identical (&c2, &c)
      && !c2_is_int
      && optimize_function_for_speed_p (cfun)
      && powi_cost (n / 3) <= POWI_MAX_MULTS)
    {
      tree powi_x_ndiv3 = NULL_TREE;

      /* Attempt to fold powi(arg0, abs(n/3)) into multiplies.  If not
         possible or profitable, give up.  Skip the degenerate case when
         abs(n) < 3, where the result is always 1.  */
      if (absu_hwi (n) >= 3)
	{
	  powi_x_ndiv3 = gimple_expand_builtin_powi (gsi, loc, arg0,
						     abs_hwi (n / 3));
	  if (!powi_x_ndiv3)
	    return NULL_TREE;
	}

      /* Calculate powi(cbrt(x), n%3).  Don't use gimple_expand_builtin_powi
         as that creates an unnecessary variable.  Instead, just produce
         either cbrt(x) or cbrt(x) * cbrt(x).  */
      cbrt_x = build_and_insert_call (gsi, loc, cbrtfn, arg0);

      if (absu_hwi (n) % 3 == 1)
	powi_cbrt_x = cbrt_x;
      else
	powi_cbrt_x = build_and_insert_binop (gsi, loc, "powroot", MULT_EXPR,
					      cbrt_x, cbrt_x);

      /* Multiply the two subexpressions, unless powi(x,abs(n)/3) = 1.  */
      if (absu_hwi (n) < 3)
	result = powi_cbrt_x;
      else
	result = build_and_insert_binop (gsi, loc, "powroot", MULT_EXPR,
					 powi_x_ndiv3, powi_cbrt_x);

      /* If n is negative, reciprocate the result.  */
      if (n < 0)
	result = build_and_insert_binop (gsi, loc, "powroot", RDIV_EXPR,
					 build_real (type, dconst1), result);

      return result;
    }

  /* No optimizations succeeded.  */
  return NULL_TREE;
}

/* ARG is the argument to a cabs builtin call in GSI with location info
   LOC.  Create a sequence of statements prior to GSI that calculates
   sqrt(R*R + I*I), where R and I are the real and imaginary components
   of ARG, respectively.  Return an expression holding the result.  */

static tree
gimple_expand_builtin_cabs (gimple_stmt_iterator *gsi, location_t loc, tree arg)
{
  tree real_part, imag_part, addend1, addend2, sum, result;
  tree type = TREE_TYPE (TREE_TYPE (arg));
  tree sqrtfn = mathfn_built_in (type, BUILT_IN_SQRT);
  machine_mode mode = TYPE_MODE (type);

  if (!flag_unsafe_math_optimizations
      || !optimize_bb_for_speed_p (gimple_bb (gsi_stmt (*gsi)))
      || !sqrtfn
      || optab_handler (sqrt_optab, mode) == CODE_FOR_nothing)
    return NULL_TREE;

  real_part = build_and_insert_ref (gsi, loc, type, "cabs",
				    REALPART_EXPR, arg);
  addend1 = build_and_insert_binop (gsi, loc, "cabs", MULT_EXPR,
				    real_part, real_part);
  imag_part = build_and_insert_ref (gsi, loc, type, "cabs",
				    IMAGPART_EXPR, arg);
  addend2 = build_and_insert_binop (gsi, loc, "cabs", MULT_EXPR,
				    imag_part, imag_part);
  sum = build_and_insert_binop (gsi, loc, "cabs", PLUS_EXPR, addend1, addend2);
  result = build_and_insert_call (gsi, loc, sqrtfn, sum);

  return result;
}

/* Go through all calls to sin, cos and cexpi and call execute_cse_sincos_1
   on the SSA_NAME argument of each of them.  Also expand powi(x,n) into
   an optimal number of multiplies, when n is a constant.  */

namespace {

const pass_data pass_data_cse_sincos =
{
  GIMPLE_PASS, /* type */
  "sincos", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  PROP_gimple_opt_math, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_cse_sincos : public gimple_opt_pass
{
public:
  pass_cse_sincos (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cse_sincos, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* We no longer require either sincos or cexp, since powi expansion
	 piggybacks on this pass.  */
      return optimize;
    }

  virtual unsigned int execute (function *);

}; // class pass_cse_sincos

unsigned int
pass_cse_sincos::execute (function *fun)
{
  basic_block bb;
  bool cfg_changed = false;

  calculate_dominance_info (CDI_DOMINATORS);
  memset (&sincos_stats, 0, sizeof (sincos_stats));

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      bool cleanup_eh = false;

      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
	  gimple *stmt = gsi_stmt (gsi);

	  /* Only the last stmt in a bb could throw, no need to call
	     gimple_purge_dead_eh_edges if we change something in the middle
	     of a basic block.  */
	  cleanup_eh = false;

	  if (is_gimple_call (stmt)
	      && gimple_call_lhs (stmt))
	    {
	      tree arg, arg0, arg1, result;
	      HOST_WIDE_INT n;
	      location_t loc;

	      switch (gimple_call_combined_fn (stmt))
		{
		CASE_CFN_COS:
		CASE_CFN_SIN:
		CASE_CFN_CEXPI:
		  /* Make sure we have either sincos or cexp.  */
		  if (!targetm.libc_has_function (function_c99_math_complex)
		      && !targetm.libc_has_function (function_sincos))
		    break;

		  arg = gimple_call_arg (stmt, 0);
		  if (TREE_CODE (arg) == SSA_NAME)
		    cfg_changed |= execute_cse_sincos_1 (arg);
		  break;

		CASE_CFN_POW:
		  arg0 = gimple_call_arg (stmt, 0);
		  arg1 = gimple_call_arg (stmt, 1);

		  loc = gimple_location (stmt);
		  result = gimple_expand_builtin_pow (&gsi, loc, arg0, arg1);

		  if (result)
		    {
		      tree lhs = gimple_get_lhs (stmt);
		      gassign *new_stmt = gimple_build_assign (lhs, result);
		      gimple_set_location (new_stmt, loc);
		      unlink_stmt_vdef (stmt);
		      gsi_replace (&gsi, new_stmt, true);
		      cleanup_eh = true;
		      if (gimple_vdef (stmt))
			release_ssa_name (gimple_vdef (stmt));
		    }
		  break;

		CASE_CFN_POWI:
		  arg0 = gimple_call_arg (stmt, 0);
		  arg1 = gimple_call_arg (stmt, 1);
		  loc = gimple_location (stmt);

		  if (real_minus_onep (arg0))
		    {
                      tree t0, t1, cond, one, minus_one;
		      gassign *stmt;

		      t0 = TREE_TYPE (arg0);
		      t1 = TREE_TYPE (arg1);
		      one = build_real (t0, dconst1);
		      minus_one = build_real (t0, dconstm1);

		      cond = make_temp_ssa_name (t1, NULL, "powi_cond");
		      stmt = gimple_build_assign (cond, BIT_AND_EXPR,
						  arg1, build_int_cst (t1, 1));
		      gimple_set_location (stmt, loc);
		      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);

		      result = make_temp_ssa_name (t0, NULL, "powi");
		      stmt = gimple_build_assign (result, COND_EXPR, cond,
						  minus_one, one);
		      gimple_set_location (stmt, loc);
		      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
		    }
		  else
		    {
		      if (!tree_fits_shwi_p (arg1))
			break;

		      n = tree_to_shwi (arg1);
		      result = gimple_expand_builtin_powi (&gsi, loc, arg0, n);
		    }

		  if (result)
		    {
		      tree lhs = gimple_get_lhs (stmt);
		      gassign *new_stmt = gimple_build_assign (lhs, result);
		      gimple_set_location (new_stmt, loc);
		      unlink_stmt_vdef (stmt);
		      gsi_replace (&gsi, new_stmt, true);
		      cleanup_eh = true;
		      if (gimple_vdef (stmt))
			release_ssa_name (gimple_vdef (stmt));
		    }
		  break;

		CASE_CFN_CABS:
		  arg0 = gimple_call_arg (stmt, 0);
		  loc = gimple_location (stmt);
		  result = gimple_expand_builtin_cabs (&gsi, loc, arg0);

		  if (result)
		    {
		      tree lhs = gimple_get_lhs (stmt);
		      gassign *new_stmt = gimple_build_assign (lhs, result);
		      gimple_set_location (new_stmt, loc);
		      unlink_stmt_vdef (stmt);
		      gsi_replace (&gsi, new_stmt, true);
		      cleanup_eh = true;
		      if (gimple_vdef (stmt))
			release_ssa_name (gimple_vdef (stmt));
		    }
		  break;

		default:;
		}
	    }
	}
      if (cleanup_eh)
	cfg_changed |= gimple_purge_dead_eh_edges (bb);
    }

  statistics_counter_event (fun, "sincos statements inserted",
			    sincos_stats.inserted);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_cse_sincos (gcc::context *ctxt)
{
  return new pass_cse_sincos (ctxt);
}

/* A symbolic number structure is used to detect byte permutation and selection
   patterns of a source.  To achieve that, its field N contains an artificial
   number consisting of BITS_PER_MARKER sized markers tracking where does each
   byte come from in the source:

   0	   - target byte has the value 0
   FF	   - target byte has an unknown value (eg. due to sign extension)
   1..size - marker value is the byte index in the source (0 for lsb).

   To detect permutations on memory sources (arrays and structures), a symbolic
   number is also associated:
   - a base address BASE_ADDR and an OFFSET giving the address of the source;
   - a range which gives the difference between the highest and lowest accessed
     memory location to make such a symbolic number;
   - the address SRC of the source element of lowest address as a convenience
     to easily get BASE_ADDR + offset + lowest bytepos;
   - number of expressions N_OPS bitwise ored together to represent
     approximate cost of the computation.

   Note 1: the range is different from size as size reflects the size of the
   type of the current expression.  For instance, for an array char a[],
   (short) a[0] | (short) a[3] would have a size of 2 but a range of 4 while
   (short) a[0] | ((short) a[0] << 1) would still have a size of 2 but this
   time a range of 1.

   Note 2: for non-memory sources, range holds the same value as size.

   Note 3: SRC points to the SSA_NAME in case of non-memory source.  */

struct symbolic_number {
  uint64_t n;
  tree type;
  tree base_addr;
  tree offset;
  HOST_WIDE_INT bytepos;
  tree src;
  tree alias_set;
  tree vuse;
  unsigned HOST_WIDE_INT range;
  int n_ops;
};

#define BITS_PER_MARKER 8
#define MARKER_MASK ((1 << BITS_PER_MARKER) - 1)
#define MARKER_BYTE_UNKNOWN MARKER_MASK
#define HEAD_MARKER(n, size) \
  ((n) & ((uint64_t) MARKER_MASK << (((size) - 1) * BITS_PER_MARKER)))

/* The number which the find_bswap_or_nop_1 result should match in
   order to have a nop.  The number is masked according to the size of
   the symbolic number before using it.  */
#define CMPNOP (sizeof (int64_t) < 8 ? 0 : \
  (uint64_t)0x08070605 << 32 | 0x04030201)

/* The number which the find_bswap_or_nop_1 result should match in
   order to have a byte swap.  The number is masked according to the
   size of the symbolic number before using it.  */
#define CMPXCHG (sizeof (int64_t) < 8 ? 0 : \
  (uint64_t)0x01020304 << 32 | 0x05060708)

/* Perform a SHIFT or ROTATE operation by COUNT bits on symbolic
   number N.  Return false if the requested operation is not permitted
   on a symbolic number.  */

static inline bool
do_shift_rotate (enum tree_code code,
		 struct symbolic_number *n,
		 int count)
{
  int i, size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;
  unsigned head_marker;

  if (count % BITS_PER_UNIT != 0)
    return false;
  count = (count / BITS_PER_UNIT) * BITS_PER_MARKER;

  /* Zero out the extra bits of N in order to avoid them being shifted
     into the significant bits.  */
  if (size < 64 / BITS_PER_MARKER)
    n->n &= ((uint64_t) 1 << (size * BITS_PER_MARKER)) - 1;

  switch (code)
    {
    case LSHIFT_EXPR:
      n->n <<= count;
      break;
    case RSHIFT_EXPR:
      head_marker = HEAD_MARKER (n->n, size);
      n->n >>= count;
      /* Arithmetic shift of signed type: result is dependent on the value.  */
      if (!TYPE_UNSIGNED (n->type) && head_marker)
	for (i = 0; i < count / BITS_PER_MARKER; i++)
	  n->n |= (uint64_t) MARKER_BYTE_UNKNOWN
		  << ((size - 1 - i) * BITS_PER_MARKER);
      break;
    case LROTATE_EXPR:
      n->n = (n->n << count) | (n->n >> ((size * BITS_PER_MARKER) - count));
      break;
    case RROTATE_EXPR:
      n->n = (n->n >> count) | (n->n << ((size * BITS_PER_MARKER) - count));
      break;
    default:
      return false;
    }
  /* Zero unused bits for size.  */
  if (size < 64 / BITS_PER_MARKER)
    n->n &= ((uint64_t) 1 << (size * BITS_PER_MARKER)) - 1;
  return true;
}

/* Perform sanity checking for the symbolic number N and the gimple
   statement STMT.  */

static inline bool
verify_symbolic_number_p (struct symbolic_number *n, gimple *stmt)
{
  tree lhs_type;

  lhs_type = gimple_expr_type (stmt);

  if (TREE_CODE (lhs_type) != INTEGER_TYPE)
    return false;

  if (TYPE_PRECISION (lhs_type) != TYPE_PRECISION (n->type))
    return false;

  return true;
}

/* Initialize the symbolic number N for the bswap pass from the base element
   SRC manipulated by the bitwise OR expression.  */

static bool
init_symbolic_number (struct symbolic_number *n, tree src)
{
  int size;

  if (! INTEGRAL_TYPE_P (TREE_TYPE (src)))
    return false;

  n->base_addr = n->offset = n->alias_set = n->vuse = NULL_TREE;
  n->src = src;

  /* Set up the symbolic number N by setting each byte to a value between 1 and
     the byte size of rhs1.  The highest order byte is set to n->size and the
     lowest order byte to 1.  */
  n->type = TREE_TYPE (src);
  size = TYPE_PRECISION (n->type);
  if (size % BITS_PER_UNIT != 0)
    return false;
  size /= BITS_PER_UNIT;
  if (size > 64 / BITS_PER_MARKER)
    return false;
  n->range = size;
  n->n = CMPNOP;
  n->n_ops = 1;

  if (size < 64 / BITS_PER_MARKER)
    n->n &= ((uint64_t) 1 << (size * BITS_PER_MARKER)) - 1;

  return true;
}

/* Check if STMT might be a byte swap or a nop from a memory source and returns
   the answer. If so, REF is that memory source and the base of the memory area
   accessed and the offset of the access from that base are recorded in N.  */

bool
find_bswap_or_nop_load (gimple *stmt, tree ref, struct symbolic_number *n)
{
  /* Leaf node is an array or component ref. Memorize its base and
     offset from base to compare to other such leaf node.  */
  HOST_WIDE_INT bitsize, bitpos;
  machine_mode mode;
  int unsignedp, reversep, volatilep;
  tree offset, base_addr;

  /* Not prepared to handle PDP endian.  */
  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    return false;

  if (!gimple_assign_load_p (stmt) || gimple_has_volatile_ops (stmt))
    return false;

  base_addr = get_inner_reference (ref, &bitsize, &bitpos, &offset, &mode,
				   &unsignedp, &reversep, &volatilep);

  if (TREE_CODE (base_addr) == MEM_REF)
    {
      offset_int bit_offset = 0;
      tree off = TREE_OPERAND (base_addr, 1);

      if (!integer_zerop (off))
	{
	  offset_int boff, coff = mem_ref_offset (base_addr);
	  boff = coff << LOG2_BITS_PER_UNIT;
	  bit_offset += boff;
	}

      base_addr = TREE_OPERAND (base_addr, 0);

      /* Avoid returning a negative bitpos as this may wreak havoc later.  */
      if (wi::neg_p (bit_offset))
	{
	  offset_int mask = wi::mask <offset_int> (LOG2_BITS_PER_UNIT, false);
	  offset_int tem = wi::bit_and_not (bit_offset, mask);
	  /* TEM is the bitpos rounded to BITS_PER_UNIT towards -Inf.
	     Subtract it to BIT_OFFSET and add it (scaled) to OFFSET.  */
	  bit_offset -= tem;
	  tem >>= LOG2_BITS_PER_UNIT;
	  if (offset)
	    offset = size_binop (PLUS_EXPR, offset,
				    wide_int_to_tree (sizetype, tem));
	  else
	    offset = wide_int_to_tree (sizetype, tem);
	}

      bitpos += bit_offset.to_shwi ();
    }

  if (bitpos % BITS_PER_UNIT)
    return false;
  if (bitsize % BITS_PER_UNIT)
    return false;
  if (reversep)
    return false;

  if (!init_symbolic_number (n, ref))
    return false;
  n->base_addr = base_addr;
  n->offset = offset;
  n->bytepos = bitpos / BITS_PER_UNIT;
  n->alias_set = reference_alias_ptr_type (ref);
  n->vuse = gimple_vuse (stmt);
  return true;
}

/* Compute the symbolic number N representing the result of a bitwise OR on 2
   symbolic number N1 and N2 whose source statements are respectively
   SOURCE_STMT1 and SOURCE_STMT2.  */

static gimple *
perform_symbolic_merge (gimple *source_stmt1, struct symbolic_number *n1,
			gimple *source_stmt2, struct symbolic_number *n2,
			struct symbolic_number *n)
{
  int i, size;
  uint64_t mask;
  gimple *source_stmt;
  struct symbolic_number *n_start;

  tree rhs1 = gimple_assign_rhs1 (source_stmt1);
  if (TREE_CODE (rhs1) == BIT_FIELD_REF
      && TREE_CODE (TREE_OPERAND (rhs1, 0)) == SSA_NAME)
    rhs1 = TREE_OPERAND (rhs1, 0);
  tree rhs2 = gimple_assign_rhs1 (source_stmt2);
  if (TREE_CODE (rhs2) == BIT_FIELD_REF
      && TREE_CODE (TREE_OPERAND (rhs2, 0)) == SSA_NAME)
    rhs2 = TREE_OPERAND (rhs2, 0);

  /* Sources are different, cancel bswap if they are not memory location with
     the same base (array, structure, ...).  */
  if (rhs1 != rhs2)
    {
      uint64_t inc;
      HOST_WIDE_INT start_sub, end_sub, end1, end2, end;
      struct symbolic_number *toinc_n_ptr, *n_end;
      basic_block bb1, bb2;

      if (!n1->base_addr || !n2->base_addr
	  || !operand_equal_p (n1->base_addr, n2->base_addr, 0))
	return NULL;

      if (!n1->offset != !n2->offset
	  || (n1->offset && !operand_equal_p (n1->offset, n2->offset, 0)))
	return NULL;

      if (n1->bytepos < n2->bytepos)
	{
	  n_start = n1;
	  start_sub = n2->bytepos - n1->bytepos;
	}
      else
	{
	  n_start = n2;
	  start_sub = n1->bytepos - n2->bytepos;
	}

      bb1 = gimple_bb (source_stmt1);
      bb2 = gimple_bb (source_stmt2);
      if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
	source_stmt = source_stmt1;
      else
	source_stmt = source_stmt2;

      /* Find the highest address at which a load is performed and
	 compute related info.  */
      end1 = n1->bytepos + (n1->range - 1);
      end2 = n2->bytepos + (n2->range - 1);
      if (end1 < end2)
	{
	  end = end2;
	  end_sub = end2 - end1;
	}
      else
	{
	  end = end1;
	  end_sub = end1 - end2;
	}
      n_end = (end2 > end1) ? n2 : n1;

      /* Find symbolic number whose lsb is the most significant.  */
      if (BYTES_BIG_ENDIAN)
	toinc_n_ptr = (n_end == n1) ? n2 : n1;
      else
	toinc_n_ptr = (n_start == n1) ? n2 : n1;

      n->range = end - n_start->bytepos + 1;

      /* Check that the range of memory covered can be represented by
	 a symbolic number.  */
      if (n->range > 64 / BITS_PER_MARKER)
	return NULL;

      /* Reinterpret byte marks in symbolic number holding the value of
	 bigger weight according to target endianness.  */
      inc = BYTES_BIG_ENDIAN ? end_sub : start_sub;
      size = TYPE_PRECISION (n1->type) / BITS_PER_UNIT;
      for (i = 0; i < size; i++, inc <<= BITS_PER_MARKER)
	{
	  unsigned marker
	    = (toinc_n_ptr->n >> (i * BITS_PER_MARKER)) & MARKER_MASK;
	  if (marker && marker != MARKER_BYTE_UNKNOWN)
	    toinc_n_ptr->n += inc;
	}
    }
  else
    {
      n->range = n1->range;
      n_start = n1;
      source_stmt = source_stmt1;
    }

  if (!n1->alias_set
      || alias_ptr_types_compatible_p (n1->alias_set, n2->alias_set))
    n->alias_set = n1->alias_set;
  else
    n->alias_set = ptr_type_node;
  n->vuse = n_start->vuse;
  n->base_addr = n_start->base_addr;
  n->offset = n_start->offset;
  n->src = n_start->src;
  n->bytepos = n_start->bytepos;
  n->type = n_start->type;
  size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;

  for (i = 0, mask = MARKER_MASK; i < size; i++, mask <<= BITS_PER_MARKER)
    {
      uint64_t masked1, masked2;

      masked1 = n1->n & mask;
      masked2 = n2->n & mask;
      if (masked1 && masked2 && masked1 != masked2)
	return NULL;
    }
  n->n = n1->n | n2->n;
  n->n_ops = n1->n_ops + n2->n_ops;

  return source_stmt;
}

/* find_bswap_or_nop_1 invokes itself recursively with N and tries to perform
   the operation given by the rhs of STMT on the result.  If the operation
   could successfully be executed the function returns a gimple stmt whose
   rhs's first tree is the expression of the source operand and NULL
   otherwise.  */

static gimple *
find_bswap_or_nop_1 (gimple *stmt, struct symbolic_number *n, int limit)
{
  enum tree_code code;
  tree rhs1, rhs2 = NULL;
  gimple *rhs1_stmt, *rhs2_stmt, *source_stmt1;
  enum gimple_rhs_class rhs_class;

  if (!limit || !is_gimple_assign (stmt))
    return NULL;

  rhs1 = gimple_assign_rhs1 (stmt);

  if (find_bswap_or_nop_load (stmt, rhs1, n))
    return stmt;

  /* Handle BIT_FIELD_REF.  */
  if (TREE_CODE (rhs1) == BIT_FIELD_REF
      && TREE_CODE (TREE_OPERAND (rhs1, 0)) == SSA_NAME)
    {
      unsigned HOST_WIDE_INT bitsize = tree_to_uhwi (TREE_OPERAND (rhs1, 1));
      unsigned HOST_WIDE_INT bitpos = tree_to_uhwi (TREE_OPERAND (rhs1, 2));
      if (bitpos % BITS_PER_UNIT == 0
	  && bitsize % BITS_PER_UNIT == 0
	  && init_symbolic_number (n, TREE_OPERAND (rhs1, 0)))
	{
	  /* Handle big-endian bit numbering in BIT_FIELD_REF.  */
	  if (BYTES_BIG_ENDIAN)
	    bitpos = TYPE_PRECISION (n->type) - bitpos - bitsize;

	  /* Shift.  */
	  if (!do_shift_rotate (RSHIFT_EXPR, n, bitpos))
	    return NULL;

	  /* Mask.  */
	  uint64_t mask = 0;
	  uint64_t tmp = (1 << BITS_PER_UNIT) - 1;
	  for (unsigned i = 0; i < bitsize / BITS_PER_UNIT;
	       i++, tmp <<= BITS_PER_UNIT)
	    mask |= (uint64_t) MARKER_MASK << (i * BITS_PER_MARKER);
	  n->n &= mask;

	  /* Convert.  */
	  n->type = TREE_TYPE (rhs1);
	  if (!n->base_addr)
	    n->range = TYPE_PRECISION (n->type) / BITS_PER_UNIT;

	  return verify_symbolic_number_p (n, stmt) ? stmt : NULL;
	}

      return NULL;
    }

  if (TREE_CODE (rhs1) != SSA_NAME)
    return NULL;

  code = gimple_assign_rhs_code (stmt);
  rhs_class = gimple_assign_rhs_class (stmt);
  rhs1_stmt = SSA_NAME_DEF_STMT (rhs1);

  if (rhs_class == GIMPLE_BINARY_RHS)
    rhs2 = gimple_assign_rhs2 (stmt);

  /* Handle unary rhs and binary rhs with integer constants as second
     operand.  */

  if (rhs_class == GIMPLE_UNARY_RHS
      || (rhs_class == GIMPLE_BINARY_RHS
	  && TREE_CODE (rhs2) == INTEGER_CST))
    {
      if (code != BIT_AND_EXPR
	  && code != LSHIFT_EXPR
	  && code != RSHIFT_EXPR
	  && code != LROTATE_EXPR
	  && code != RROTATE_EXPR
	  && !CONVERT_EXPR_CODE_P (code))
	return NULL;

      source_stmt1 = find_bswap_or_nop_1 (rhs1_stmt, n, limit - 1);

      /* If find_bswap_or_nop_1 returned NULL, STMT is a leaf node and
	 we have to initialize the symbolic number.  */
      if (!source_stmt1)
	{
	  if (gimple_assign_load_p (stmt)
	      || !init_symbolic_number (n, rhs1))
	    return NULL;
	  source_stmt1 = stmt;
	}

      switch (code)
	{
	case BIT_AND_EXPR:
	  {
	    int i, size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;
	    uint64_t val = int_cst_value (rhs2), mask = 0;
	    uint64_t tmp = (1 << BITS_PER_UNIT) - 1;

	    /* Only constants masking full bytes are allowed.  */
	    for (i = 0; i < size; i++, tmp <<= BITS_PER_UNIT)
	      if ((val & tmp) != 0 && (val & tmp) != tmp)
		return NULL;
	      else if (val & tmp)
		mask |= (uint64_t) MARKER_MASK << (i * BITS_PER_MARKER);

	    n->n &= mask;
	  }
	  break;
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	case LROTATE_EXPR:
	case RROTATE_EXPR:
	  if (!do_shift_rotate (code, n, (int) TREE_INT_CST_LOW (rhs2)))
	    return NULL;
	  break;
	CASE_CONVERT:
	  {
	    int i, type_size, old_type_size;
	    tree type;

	    type = gimple_expr_type (stmt);
	    type_size = TYPE_PRECISION (type);
	    if (type_size % BITS_PER_UNIT != 0)
	      return NULL;
	    type_size /= BITS_PER_UNIT;
	    if (type_size > 64 / BITS_PER_MARKER)
	      return NULL;

	    /* Sign extension: result is dependent on the value.  */
	    old_type_size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;
	    if (!TYPE_UNSIGNED (n->type) && type_size > old_type_size
		&& HEAD_MARKER (n->n, old_type_size))
	      for (i = 0; i < type_size - old_type_size; i++)
		n->n |= (uint64_t) MARKER_BYTE_UNKNOWN
			<< ((type_size - 1 - i) * BITS_PER_MARKER);

	    if (type_size < 64 / BITS_PER_MARKER)
	      {
		/* If STMT casts to a smaller type mask out the bits not
		   belonging to the target type.  */
		n->n &= ((uint64_t) 1 << (type_size * BITS_PER_MARKER)) - 1;
	      }
	    n->type = type;
	    if (!n->base_addr)
	      n->range = type_size;
	  }
	  break;
	default:
	  return NULL;
	};
      return verify_symbolic_number_p (n, stmt) ? source_stmt1 : NULL;
    }

  /* Handle binary rhs.  */

  if (rhs_class == GIMPLE_BINARY_RHS)
    {
      struct symbolic_number n1, n2;
      gimple *source_stmt, *source_stmt2;

      if (code != BIT_IOR_EXPR)
	return NULL;

      if (TREE_CODE (rhs2) != SSA_NAME)
	return NULL;

      rhs2_stmt = SSA_NAME_DEF_STMT (rhs2);

      switch (code)
	{
	case BIT_IOR_EXPR:
	  source_stmt1 = find_bswap_or_nop_1 (rhs1_stmt, &n1, limit - 1);

	  if (!source_stmt1)
	    return NULL;

	  source_stmt2 = find_bswap_or_nop_1 (rhs2_stmt, &n2, limit - 1);

	  if (!source_stmt2)
	    return NULL;

	  if (TYPE_PRECISION (n1.type) != TYPE_PRECISION (n2.type))
	    return NULL;

	  if (!n1.vuse != !n2.vuse
	      || (n1.vuse && !operand_equal_p (n1.vuse, n2.vuse, 0)))
	    return NULL;

	  source_stmt
	    = perform_symbolic_merge (source_stmt1, &n1, source_stmt2, &n2, n);

	  if (!source_stmt)
	    return NULL;

	  if (!verify_symbolic_number_p (n, stmt))
	    return NULL;

	  break;
	default:
	  return NULL;
	}
      return source_stmt;
    }
  return NULL;
}

/* Check if STMT completes a bswap implementation or a read in a given
   endianness consisting of ORs, SHIFTs and ANDs and sets *BSWAP
   accordingly.  It also sets N to represent the kind of operations
   performed: size of the resulting expression and whether it works on
   a memory source, and if so alias-set and vuse.  At last, the
   function returns a stmt whose rhs's first tree is the source
   expression.  */

static gimple *
find_bswap_or_nop (gimple *stmt, struct symbolic_number *n, bool *bswap)
{
  unsigned rsize;
  uint64_t tmpn, mask;
/* The number which the find_bswap_or_nop_1 result should match in order
   to have a full byte swap.  The number is shifted to the right
   according to the size of the symbolic number before using it.  */
  uint64_t cmpxchg = CMPXCHG;
  uint64_t cmpnop = CMPNOP;

  gimple *ins_stmt;
  int limit;

  /* The last parameter determines the depth search limit.  It usually
     correlates directly to the number n of bytes to be touched.  We
     increase that number by log2(n) + 1 here in order to also
     cover signed -> unsigned conversions of the src operand as can be seen
     in libgcc, and for initial shift/and operation of the src operand.  */
  limit = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (gimple_expr_type (stmt)));
  limit += 1 + (int) ceil_log2 ((unsigned HOST_WIDE_INT) limit);
  ins_stmt = find_bswap_or_nop_1 (stmt, n, limit);

  if (!ins_stmt)
    return NULL;

  /* Find real size of result (highest non-zero byte).  */
  if (n->base_addr)
    for (tmpn = n->n, rsize = 0; tmpn; tmpn >>= BITS_PER_MARKER, rsize++);
  else
    rsize = n->range;

  /* Zero out the bits corresponding to untouched bytes in original gimple
     expression.  */
  if (n->range < (int) sizeof (int64_t))
    {
      mask = ((uint64_t) 1 << (n->range * BITS_PER_MARKER)) - 1;
      cmpxchg >>= (64 / BITS_PER_MARKER - n->range) * BITS_PER_MARKER;
      cmpnop &= mask;
    }

  /* Zero out the bits corresponding to unused bytes in the result of the
     gimple expression.  */
  if (rsize < n->range)
    {
      if (BYTES_BIG_ENDIAN)
	{
	  mask = ((uint64_t) 1 << (rsize * BITS_PER_MARKER)) - 1;
	  cmpxchg &= mask;
	  cmpnop >>= (n->range - rsize) * BITS_PER_MARKER;
	}
      else
	{
	  mask = ((uint64_t) 1 << (rsize * BITS_PER_MARKER)) - 1;
	  cmpxchg >>= (n->range - rsize) * BITS_PER_MARKER;
	  cmpnop &= mask;
	}
      n->range = rsize;
    }

  /* A complete byte swap should make the symbolic number to start with
     the largest digit in the highest order byte. Unchanged symbolic
     number indicates a read with same endianness as target architecture.  */
  if (n->n == cmpnop)
    *bswap = false;
  else if (n->n == cmpxchg)
    *bswap = true;
  else
    return NULL;

  /* Useless bit manipulation performed by code.  */
  if (!n->base_addr && n->n == cmpnop && n->n_ops == 1)
    return NULL;

  n->range *= BITS_PER_UNIT;
  return ins_stmt;
}

namespace {

const pass_data pass_data_optimize_bswap =
{
  GIMPLE_PASS, /* type */
  "bswap", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_optimize_bswap : public gimple_opt_pass
{
public:
  pass_optimize_bswap (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_optimize_bswap, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return flag_expensive_optimizations && optimize;
    }

  virtual unsigned int execute (function *);

}; // class pass_optimize_bswap

/* Perform the bswap optimization: replace the expression computed in the rhs
   of CUR_STMT by an equivalent bswap, load or load + bswap expression.
   Which of these alternatives replace the rhs is given by N->base_addr (non
   null if a load is needed) and BSWAP.  The type, VUSE and set-alias of the
   load to perform are also given in N while the builtin bswap invoke is given
   in FNDEL.  Finally, if a load is involved, SRC_STMT refers to one of the
   load statements involved to construct the rhs in CUR_STMT and N->range gives
   the size of the rhs expression for maintaining some statistics.

   Note that if the replacement involve a load, CUR_STMT is moved just after
   SRC_STMT to do the load with the same VUSE which can lead to CUR_STMT
   changing of basic block.  */

static bool
bswap_replace (gimple *cur_stmt, gimple *ins_stmt, tree fndecl,
	       tree bswap_type, tree load_type, struct symbolic_number *n,
	       bool bswap)
{
  gimple_stmt_iterator gsi;
  tree src, tmp, tgt;
  gimple *bswap_stmt;

  gsi = gsi_for_stmt (cur_stmt);
  src = n->src;
  tgt = gimple_assign_lhs (cur_stmt);

  /* Need to load the value from memory first.  */
  if (n->base_addr)
    {
      gimple_stmt_iterator gsi_ins = gsi_for_stmt (ins_stmt);
      tree addr_expr, addr_tmp, val_expr, val_tmp;
      tree load_offset_ptr, aligned_load_type;
      gimple *addr_stmt, *load_stmt;
      unsigned align;
      HOST_WIDE_INT load_offset = 0;
      basic_block ins_bb, cur_bb;

      ins_bb = gimple_bb (ins_stmt);
      cur_bb = gimple_bb (cur_stmt);
      if (!dominated_by_p (CDI_DOMINATORS, cur_bb, ins_bb))
	return false;

      align = get_object_alignment (src);

      /* Move cur_stmt just before  one of the load of the original
	 to ensure it has the same VUSE.  See PR61517 for what could
	 go wrong.  */
      if (gimple_bb (cur_stmt) != gimple_bb (ins_stmt))
	reset_flow_sensitive_info (gimple_assign_lhs (cur_stmt));
      gsi_move_before (&gsi, &gsi_ins);
      gsi = gsi_for_stmt (cur_stmt);

      /* Compute address to load from and cast according to the size
	 of the load.  */
      addr_expr = build_fold_addr_expr (unshare_expr (src));
      if (is_gimple_mem_ref_addr (addr_expr))
	addr_tmp = addr_expr;
      else
	{
	  addr_tmp = make_temp_ssa_name (TREE_TYPE (addr_expr), NULL,
					 "load_src");
	  addr_stmt = gimple_build_assign (addr_tmp, addr_expr);
	  gsi_insert_before (&gsi, addr_stmt, GSI_SAME_STMT);
	}

      /* Perform the load.  */
      aligned_load_type = load_type;
      if (align < TYPE_ALIGN (load_type))
	aligned_load_type = build_aligned_type (load_type, align);
      load_offset_ptr = build_int_cst (n->alias_set, load_offset);
      val_expr = fold_build2 (MEM_REF, aligned_load_type, addr_tmp,
			      load_offset_ptr);

      if (!bswap)
	{
	  if (n->range == 16)
	    nop_stats.found_16bit++;
	  else if (n->range == 32)
	    nop_stats.found_32bit++;
	  else
	    {
	      gcc_assert (n->range == 64);
	      nop_stats.found_64bit++;
	    }

	  /* Convert the result of load if necessary.  */
	  if (!useless_type_conversion_p (TREE_TYPE (tgt), load_type))
	    {
	      val_tmp = make_temp_ssa_name (aligned_load_type, NULL,
					    "load_dst");
	      load_stmt = gimple_build_assign (val_tmp, val_expr);
	      gimple_set_vuse (load_stmt, n->vuse);
	      gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);
	      gimple_assign_set_rhs_with_ops (&gsi, NOP_EXPR, val_tmp);
	    }
	  else
	    {
	      gimple_assign_set_rhs_with_ops (&gsi, MEM_REF, val_expr);
	      gimple_set_vuse (cur_stmt, n->vuse);
	    }
	  update_stmt (cur_stmt);

	  if (dump_file)
	    {
	      fprintf (dump_file,
		       "%d bit load in target endianness found at: ",
		       (int) n->range);
	      print_gimple_stmt (dump_file, cur_stmt, 0);
	    }
	  return true;
	}
      else
	{
	  val_tmp = make_temp_ssa_name (aligned_load_type, NULL, "load_dst");
	  load_stmt = gimple_build_assign (val_tmp, val_expr);
	  gimple_set_vuse (load_stmt, n->vuse);
	  gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);
	}
      src = val_tmp;
    }
  else if (!bswap)
    {
      gimple *g;
      if (!useless_type_conversion_p (TREE_TYPE (tgt), TREE_TYPE (src)))
	{
	  if (!is_gimple_val (src))
	    return false;
	  g = gimple_build_assign (tgt, NOP_EXPR, src);
	}
      else
	g = gimple_build_assign (tgt, src);
      if (n->range == 16)
	nop_stats.found_16bit++;
      else if (n->range == 32)
	nop_stats.found_32bit++;
      else
	{
	  gcc_assert (n->range == 64);
	  nop_stats.found_64bit++;
	}
      if (dump_file)
	{
	  fprintf (dump_file,
		   "%d bit reshuffle in target endianness found at: ",
		   (int) n->range);
	  print_gimple_stmt (dump_file, cur_stmt, 0);
	}
      gsi_replace (&gsi, g, true);
      return true;
    }
  else if (TREE_CODE (src) == BIT_FIELD_REF)
    src = TREE_OPERAND (src, 0);

  if (n->range == 16)
    bswap_stats.found_16bit++;
  else if (n->range == 32)
    bswap_stats.found_32bit++;
  else
    {
      gcc_assert (n->range == 64);
      bswap_stats.found_64bit++;
    }

  tmp = src;

  /* Convert the src expression if necessary.  */
  if (!useless_type_conversion_p (TREE_TYPE (tmp), bswap_type))
    {
      gimple *convert_stmt;

      tmp = make_temp_ssa_name (bswap_type, NULL, "bswapsrc");
      convert_stmt = gimple_build_assign (tmp, NOP_EXPR, src);
      gsi_insert_before (&gsi, convert_stmt, GSI_SAME_STMT);
    }

  /* Canonical form for 16 bit bswap is a rotate expression.  Only 16bit values
     are considered as rotation of 2N bit values by N bits is generally not
     equivalent to a bswap.  Consider for instance 0x01020304 r>> 16 which
     gives 0x03040102 while a bswap for that value is 0x04030201.  */
  if (bswap && n->range == 16)
    {
      tree count = build_int_cst (NULL, BITS_PER_UNIT);
      src = fold_build2 (LROTATE_EXPR, bswap_type, tmp, count);
      bswap_stmt = gimple_build_assign (NULL, src);
    }
  else
    bswap_stmt = gimple_build_call (fndecl, 1, tmp);

  tmp = tgt;

  /* Convert the result if necessary.  */
  if (!useless_type_conversion_p (TREE_TYPE (tgt), bswap_type))
    {
      gimple *convert_stmt;

      tmp = make_temp_ssa_name (bswap_type, NULL, "bswapdst");
      convert_stmt = gimple_build_assign (tgt, NOP_EXPR, tmp);
      gsi_insert_after (&gsi, convert_stmt, GSI_SAME_STMT);
    }

  gimple_set_lhs (bswap_stmt, tmp);

  if (dump_file)
    {
      fprintf (dump_file, "%d bit bswap implementation found at: ",
	       (int) n->range);
      print_gimple_stmt (dump_file, cur_stmt, 0);
    }

  gsi_insert_after (&gsi, bswap_stmt, GSI_SAME_STMT);
  gsi_remove (&gsi, true);
  return true;
}

/* Find manual byte swap implementations as well as load in a given
   endianness. Byte swaps are turned into a bswap builtin invokation
   while endian loads are converted to bswap builtin invokation or
   simple load according to the target endianness.  */

unsigned int
pass_optimize_bswap::execute (function *fun)
{
  basic_block bb;
  bool bswap32_p, bswap64_p;
  bool changed = false;
  tree bswap32_type = NULL_TREE, bswap64_type = NULL_TREE;

  if (BITS_PER_UNIT != 8)
    return 0;

  bswap32_p = (builtin_decl_explicit_p (BUILT_IN_BSWAP32)
	       && optab_handler (bswap_optab, SImode) != CODE_FOR_nothing);
  bswap64_p = (builtin_decl_explicit_p (BUILT_IN_BSWAP64)
	       && (optab_handler (bswap_optab, DImode) != CODE_FOR_nothing
		   || (bswap32_p && word_mode == SImode)));

  /* Determine the argument type of the builtins.  The code later on
     assumes that the return and argument type are the same.  */
  if (bswap32_p)
    {
      tree fndecl = builtin_decl_explicit (BUILT_IN_BSWAP32);
      bswap32_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
    }

  if (bswap64_p)
    {
      tree fndecl = builtin_decl_explicit (BUILT_IN_BSWAP64);
      bswap64_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
    }

  memset (&nop_stats, 0, sizeof (nop_stats));
  memset (&bswap_stats, 0, sizeof (bswap_stats));
  calculate_dominance_info (CDI_DOMINATORS);

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;

      /* We do a reverse scan for bswap patterns to make sure we get the
	 widest match. As bswap pattern matching doesn't handle previously
	 inserted smaller bswap replacements as sub-patterns, the wider
	 variant wouldn't be detected.  */
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi);)
        {
	  gimple *ins_stmt, *cur_stmt = gsi_stmt (gsi);
	  tree fndecl = NULL_TREE, bswap_type = NULL_TREE, load_type;
	  enum tree_code code;
	  struct symbolic_number n;
	  bool bswap;

	  /* This gsi_prev (&gsi) is not part of the for loop because cur_stmt
	     might be moved to a different basic block by bswap_replace and gsi
	     must not points to it if that's the case.  Moving the gsi_prev
	     there make sure that gsi points to the statement previous to
	     cur_stmt while still making sure that all statements are
	     considered in this basic block.  */
	  gsi_prev (&gsi);

	  if (!is_gimple_assign (cur_stmt))
	    continue;

	  code = gimple_assign_rhs_code (cur_stmt);
	  switch (code)
	    {
	    case LROTATE_EXPR:
	    case RROTATE_EXPR:
	      if (!tree_fits_uhwi_p (gimple_assign_rhs2 (cur_stmt))
		  || tree_to_uhwi (gimple_assign_rhs2 (cur_stmt))
		     % BITS_PER_UNIT)
		continue;
	      /* Fall through.  */
	    case BIT_IOR_EXPR:
	      break;
	    default:
	      continue;
	    }

	  ins_stmt = find_bswap_or_nop (cur_stmt, &n, &bswap);

	  if (!ins_stmt)
	    continue;

	  switch (n.range)
	    {
	    case 16:
	      /* Already in canonical form, nothing to do.  */
	      if (code == LROTATE_EXPR || code == RROTATE_EXPR)
		continue;
	      load_type = bswap_type = uint16_type_node;
	      break;
	    case 32:
	      load_type = uint32_type_node;
	      if (bswap32_p)
		{
		  fndecl = builtin_decl_explicit (BUILT_IN_BSWAP32);
		  bswap_type = bswap32_type;
		}
	      break;
	    case 64:
	      load_type = uint64_type_node;
	      if (bswap64_p)
		{
		  fndecl = builtin_decl_explicit (BUILT_IN_BSWAP64);
		  bswap_type = bswap64_type;
		}
	      break;
	    default:
	      continue;
	    }

	  if (bswap && !fndecl && n.range != 16)
	    continue;

	  if (bswap_replace (cur_stmt, ins_stmt, fndecl, bswap_type, load_type,
			     &n, bswap))
	    changed = true;
	}
    }

  statistics_counter_event (fun, "16-bit nop implementations found",
			    nop_stats.found_16bit);
  statistics_counter_event (fun, "32-bit nop implementations found",
			    nop_stats.found_32bit);
  statistics_counter_event (fun, "64-bit nop implementations found",
			    nop_stats.found_64bit);
  statistics_counter_event (fun, "16-bit bswap implementations found",
			    bswap_stats.found_16bit);
  statistics_counter_event (fun, "32-bit bswap implementations found",
			    bswap_stats.found_32bit);
  statistics_counter_event (fun, "64-bit bswap implementations found",
			    bswap_stats.found_64bit);

  return (changed ? TODO_update_ssa : 0);
}

} // anon namespace

gimple_opt_pass *
make_pass_optimize_bswap (gcc::context *ctxt)
{
  return new pass_optimize_bswap (ctxt);
}

/* Return true if stmt is a type conversion operation that can be stripped
   when used in a widening multiply operation.  */
static bool
widening_mult_conversion_strippable_p (tree result_type, gimple *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);

  if (TREE_CODE (result_type) == INTEGER_TYPE)
    {
      tree op_type;
      tree inner_op_type;

      if (!CONVERT_EXPR_CODE_P (rhs_code))
	return false;

      op_type = TREE_TYPE (gimple_assign_lhs (stmt));

      /* If the type of OP has the same precision as the result, then
	 we can strip this conversion.  The multiply operation will be
	 selected to create the correct extension as a by-product.  */
      if (TYPE_PRECISION (result_type) == TYPE_PRECISION (op_type))
	return true;

      /* We can also strip a conversion if it preserves the signed-ness of
	 the operation and doesn't narrow the range.  */
      inner_op_type = TREE_TYPE (gimple_assign_rhs1 (stmt));

      /* If the inner-most type is unsigned, then we can strip any
	 intermediate widening operation.  If it's signed, then the
	 intermediate widening operation must also be signed.  */
      if ((TYPE_UNSIGNED (inner_op_type)
	   || TYPE_UNSIGNED (op_type) == TYPE_UNSIGNED (inner_op_type))
	  && TYPE_PRECISION (op_type) > TYPE_PRECISION (inner_op_type))
	return true;

      return false;
    }

  return rhs_code == FIXED_CONVERT_EXPR;
}

/* Return true if RHS is a suitable operand for a widening multiplication,
   assuming a target type of TYPE.
   There are two cases:

     - RHS makes some value at least twice as wide.  Store that value
       in *NEW_RHS_OUT if so, and store its type in *TYPE_OUT.

     - RHS is an integer constant.  Store that value in *NEW_RHS_OUT if so,
       but leave *TYPE_OUT untouched.  */

static bool
is_widening_mult_rhs_p (tree type, tree rhs, tree *type_out,
			tree *new_rhs_out)
{
  gimple *stmt;
  tree type1, rhs1;

  if (TREE_CODE (rhs) == SSA_NAME)
    {
      stmt = SSA_NAME_DEF_STMT (rhs);
      if (is_gimple_assign (stmt))
	{
	  if (! widening_mult_conversion_strippable_p (type, stmt))
	    rhs1 = rhs;
	  else
	    {
	      rhs1 = gimple_assign_rhs1 (stmt);

	      if (TREE_CODE (rhs1) == INTEGER_CST)
		{
		  *new_rhs_out = rhs1;
		  *type_out = NULL;
		  return true;
		}
	    }
	}
      else
	rhs1 = rhs;

      type1 = TREE_TYPE (rhs1);

      if (TREE_CODE (type1) != TREE_CODE (type)
	  || TYPE_PRECISION (type1) * 2 > TYPE_PRECISION (type))
	return false;

      *new_rhs_out = rhs1;
      *type_out = type1;
      return true;
    }

  if (TREE_CODE (rhs) == INTEGER_CST)
    {
      *new_rhs_out = rhs;
      *type_out = NULL;
      return true;
    }

  return false;
}

/* Return true if STMT performs a widening multiplication, assuming the
   output type is TYPE.  If so, store the unwidened types of the operands
   in *TYPE1_OUT and *TYPE2_OUT respectively.  Also fill *RHS1_OUT and
   *RHS2_OUT such that converting those operands to types *TYPE1_OUT
   and *TYPE2_OUT would give the operands of the multiplication.  */

static bool
is_widening_mult_p (gimple *stmt,
		    tree *type1_out, tree *rhs1_out,
		    tree *type2_out, tree *rhs2_out)
{
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));

  if (TREE_CODE (type) != INTEGER_TYPE
      && TREE_CODE (type) != FIXED_POINT_TYPE)
    return false;

  if (!is_widening_mult_rhs_p (type, gimple_assign_rhs1 (stmt), type1_out,
			       rhs1_out))
    return false;

  if (!is_widening_mult_rhs_p (type, gimple_assign_rhs2 (stmt), type2_out,
			       rhs2_out))
    return false;

  if (*type1_out == NULL)
    {
      if (*type2_out == NULL || !int_fits_type_p (*rhs1_out, *type2_out))
	return false;
      *type1_out = *type2_out;
    }

  if (*type2_out == NULL)
    {
      if (!int_fits_type_p (*rhs2_out, *type1_out))
	return false;
      *type2_out = *type1_out;
    }

  /* Ensure that the larger of the two operands comes first. */
  if (TYPE_PRECISION (*type1_out) < TYPE_PRECISION (*type2_out))
    {
      std::swap (*type1_out, *type2_out);
      std::swap (*rhs1_out, *rhs2_out);
    }

  return true;
}

/* Check to see if the CALL statement is an invocation of copysign
   with 1. being the first argument.  */
static bool
is_copysign_call_with_1 (gimple *call)
{
  gcall *c = dyn_cast <gcall *> (call);
  if (! c)
    return false;

  enum combined_fn code = gimple_call_combined_fn (c);

  if (code == CFN_LAST)
    return false;

  if (builtin_fn_p (code))
    {
      switch (as_builtin_fn (code))
	{
	CASE_FLT_FN (BUILT_IN_COPYSIGN):
	CASE_FLT_FN_FLOATN_NX (BUILT_IN_COPYSIGN):
	  return real_onep (gimple_call_arg (c, 0));
	default:
	  return false;
	}
    }

  if (internal_fn_p (code))
    {
      switch (as_internal_fn (code))
	{
	case IFN_COPYSIGN:
	  return real_onep (gimple_call_arg (c, 0));
	default:
	  return false;
	}
    }

   return false;
}

/* Try to expand the pattern x * copysign (1, y) into xorsign (x, y).
   This only happens when the the xorsign optab is defined, if the
   pattern is not a xorsign pattern or if expansion fails FALSE is
   returned, otherwise TRUE is returned.  */
static bool
convert_expand_mult_copysign (gimple *stmt, gimple_stmt_iterator *gsi)
{
  tree treeop0, treeop1, lhs, type;
  location_t loc = gimple_location (stmt);
  lhs = gimple_assign_lhs (stmt);
  treeop0 = gimple_assign_rhs1 (stmt);
  treeop1 = gimple_assign_rhs2 (stmt);
  type = TREE_TYPE (lhs);
  machine_mode mode = TYPE_MODE (type);

  if (HONOR_SNANS (type))
    return false;

  if (TREE_CODE (treeop0) == SSA_NAME && TREE_CODE (treeop1) == SSA_NAME)
    {
      gimple *call0 = SSA_NAME_DEF_STMT (treeop0);
      if (!has_single_use (treeop0) || !is_copysign_call_with_1 (call0))
	{
	  call0 = SSA_NAME_DEF_STMT (treeop1);
	  if (!has_single_use (treeop1) || !is_copysign_call_with_1 (call0))
	    return false;

	  treeop1 = treeop0;
	}
	if (optab_handler (xorsign_optab, mode) == CODE_FOR_nothing)
	  return false;

	gcall *c = as_a<gcall*> (call0);
	treeop0 = gimple_call_arg (c, 1);

	gcall *call_stmt
	  = gimple_build_call_internal (IFN_XORSIGN, 2, treeop1, treeop0);
	gimple_set_lhs (call_stmt, lhs);
	gimple_set_location (call_stmt, loc);
	gsi_replace (gsi, call_stmt, true);
	return true;
    }

  return false;
}

/* Process a single gimple statement STMT, which has a MULT_EXPR as
   its rhs, and try to convert it into a WIDEN_MULT_EXPR.  The return
   value is true iff we converted the statement.  */

static bool
convert_mult_to_widen (gimple *stmt, gimple_stmt_iterator *gsi)
{
  tree lhs, rhs1, rhs2, type, type1, type2;
  enum insn_code handler;
  scalar_int_mode to_mode, from_mode, actual_mode;
  optab op;
  int actual_precision;
  location_t loc = gimple_location (stmt);
  bool from_unsigned1, from_unsigned2;

  lhs = gimple_assign_lhs (stmt);
  type = TREE_TYPE (lhs);
  if (TREE_CODE (type) != INTEGER_TYPE)
    return false;

  if (!is_widening_mult_p (stmt, &type1, &rhs1, &type2, &rhs2))
    return false;

  to_mode = SCALAR_INT_TYPE_MODE (type);
  from_mode = SCALAR_INT_TYPE_MODE (type1);
  if (to_mode == from_mode)
    return false;

  from_unsigned1 = TYPE_UNSIGNED (type1);
  from_unsigned2 = TYPE_UNSIGNED (type2);

  if (from_unsigned1 && from_unsigned2)
    op = umul_widen_optab;
  else if (!from_unsigned1 && !from_unsigned2)
    op = smul_widen_optab;
  else
    op = usmul_widen_optab;

  handler = find_widening_optab_handler_and_mode (op, to_mode, from_mode,
						  &actual_mode);

  if (handler == CODE_FOR_nothing)
    {
      if (op != smul_widen_optab)
	{
	  /* We can use a signed multiply with unsigned types as long as
	     there is a wider mode to use, or it is the smaller of the two
	     types that is unsigned.  Note that type1 >= type2, always.  */
	  if ((TYPE_UNSIGNED (type1)
	       && TYPE_PRECISION (type1) == GET_MODE_PRECISION (from_mode))
	      || (TYPE_UNSIGNED (type2)
		  && TYPE_PRECISION (type2) == GET_MODE_PRECISION (from_mode)))
	    {
	      if (!GET_MODE_WIDER_MODE (from_mode).exists (&from_mode)
		  || GET_MODE_SIZE (to_mode) <= GET_MODE_SIZE (from_mode))
		return false;
	    }

	  op = smul_widen_optab;
	  handler = find_widening_optab_handler_and_mode (op, to_mode,
							  from_mode,
							  &actual_mode);

	  if (handler == CODE_FOR_nothing)
	    return false;

	  from_unsigned1 = from_unsigned2 = false;
	}
      else
	return false;
    }

  /* Ensure that the inputs to the handler are in the correct precison
     for the opcode.  This will be the full mode size.  */
  actual_precision = GET_MODE_PRECISION (actual_mode);
  if (2 * actual_precision > TYPE_PRECISION (type))
    return false;
  if (actual_precision != TYPE_PRECISION (type1)
      || from_unsigned1 != TYPE_UNSIGNED (type1))
    rhs1 = build_and_insert_cast (gsi, loc,
				  build_nonstandard_integer_type
				    (actual_precision, from_unsigned1), rhs1);
  if (actual_precision != TYPE_PRECISION (type2)
      || from_unsigned2 != TYPE_UNSIGNED (type2))
    rhs2 = build_and_insert_cast (gsi, loc,
				  build_nonstandard_integer_type
				    (actual_precision, from_unsigned2), rhs2);

  /* Handle constants.  */
  if (TREE_CODE (rhs1) == INTEGER_CST)
    rhs1 = fold_convert (type1, rhs1);
  if (TREE_CODE (rhs2) == INTEGER_CST)
    rhs2 = fold_convert (type2, rhs2);

  gimple_assign_set_rhs1 (stmt, rhs1);
  gimple_assign_set_rhs2 (stmt, rhs2);
  gimple_assign_set_rhs_code (stmt, WIDEN_MULT_EXPR);
  update_stmt (stmt);
  widen_mul_stats.widen_mults_inserted++;
  return true;
}

/* Process a single gimple statement STMT, which is found at the
   iterator GSI and has a either a PLUS_EXPR or a MINUS_EXPR as its
   rhs (given by CODE), and try to convert it into a
   WIDEN_MULT_PLUS_EXPR or a WIDEN_MULT_MINUS_EXPR.  The return value
   is true iff we converted the statement.  */

static bool
convert_plusminus_to_widen (gimple_stmt_iterator *gsi, gimple *stmt,
			    enum tree_code code)
{
  gimple *rhs1_stmt = NULL, *rhs2_stmt = NULL;
  gimple *conv1_stmt = NULL, *conv2_stmt = NULL, *conv_stmt;
  tree type, type1, type2, optype;
  tree lhs, rhs1, rhs2, mult_rhs1, mult_rhs2, add_rhs;
  enum tree_code rhs1_code = ERROR_MARK, rhs2_code = ERROR_MARK;
  optab this_optab;
  enum tree_code wmult_code;
  enum insn_code handler;
  scalar_mode to_mode, from_mode, actual_mode;
  location_t loc = gimple_location (stmt);
  int actual_precision;
  bool from_unsigned1, from_unsigned2;

  lhs = gimple_assign_lhs (stmt);
  type = TREE_TYPE (lhs);
  if (TREE_CODE (type) != INTEGER_TYPE
      && TREE_CODE (type) != FIXED_POINT_TYPE)
    return false;

  if (code == MINUS_EXPR)
    wmult_code = WIDEN_MULT_MINUS_EXPR;
  else
    wmult_code = WIDEN_MULT_PLUS_EXPR;

  rhs1 = gimple_assign_rhs1 (stmt);
  rhs2 = gimple_assign_rhs2 (stmt);

  if (TREE_CODE (rhs1) == SSA_NAME)
    {
      rhs1_stmt = SSA_NAME_DEF_STMT (rhs1);
      if (is_gimple_assign (rhs1_stmt))
	rhs1_code = gimple_assign_rhs_code (rhs1_stmt);
    }

  if (TREE_CODE (rhs2) == SSA_NAME)
    {
      rhs2_stmt = SSA_NAME_DEF_STMT (rhs2);
      if (is_gimple_assign (rhs2_stmt))
	rhs2_code = gimple_assign_rhs_code (rhs2_stmt);
    }

  /* Allow for one conversion statement between the multiply
     and addition/subtraction statement.  If there are more than
     one conversions then we assume they would invalidate this
     transformation.  If that's not the case then they should have
     been folded before now.  */
  if (CONVERT_EXPR_CODE_P (rhs1_code))
    {
      conv1_stmt = rhs1_stmt;
      rhs1 = gimple_assign_rhs1 (rhs1_stmt);
      if (TREE_CODE (rhs1) == SSA_NAME)
	{
	  rhs1_stmt = SSA_NAME_DEF_STMT (rhs1);
	  if (is_gimple_assign (rhs1_stmt))
	    rhs1_code = gimple_assign_rhs_code (rhs1_stmt);
	}
      else
	return false;
    }
  if (CONVERT_EXPR_CODE_P (rhs2_code))
    {
      conv2_stmt = rhs2_stmt;
      rhs2 = gimple_assign_rhs1 (rhs2_stmt);
      if (TREE_CODE (rhs2) == SSA_NAME)
	{
	  rhs2_stmt = SSA_NAME_DEF_STMT (rhs2);
	  if (is_gimple_assign (rhs2_stmt))
	    rhs2_code = gimple_assign_rhs_code (rhs2_stmt);
	}
      else
	return false;
    }

  /* If code is WIDEN_MULT_EXPR then it would seem unnecessary to call
     is_widening_mult_p, but we still need the rhs returns.

     It might also appear that it would be sufficient to use the existing
     operands of the widening multiply, but that would limit the choice of
     multiply-and-accumulate instructions.

     If the widened-multiplication result has more than one uses, it is
     probably wiser not to do the conversion.  */
  if (code == PLUS_EXPR
      && (rhs1_code == MULT_EXPR || rhs1_code == WIDEN_MULT_EXPR))
    {
      if (!has_single_use (rhs1)
	  || !is_widening_mult_p (rhs1_stmt, &type1, &mult_rhs1,
				  &type2, &mult_rhs2))
	return false;
      add_rhs = rhs2;
      conv_stmt = conv1_stmt;
    }
  else if (rhs2_code == MULT_EXPR || rhs2_code == WIDEN_MULT_EXPR)
    {
      if (!has_single_use (rhs2)
	  || !is_widening_mult_p (rhs2_stmt, &type1, &mult_rhs1,
				  &type2, &mult_rhs2))
	return false;
      add_rhs = rhs1;
      conv_stmt = conv2_stmt;
    }
  else
    return false;

  to_mode = SCALAR_TYPE_MODE (type);
  from_mode = SCALAR_TYPE_MODE (type1);
  if (to_mode == from_mode)
    return false;

  from_unsigned1 = TYPE_UNSIGNED (type1);
  from_unsigned2 = TYPE_UNSIGNED (type2);
  optype = type1;

  /* There's no such thing as a mixed sign madd yet, so use a wider mode.  */
  if (from_unsigned1 != from_unsigned2)
    {
      if (!INTEGRAL_TYPE_P (type))
	return false;
      /* We can use a signed multiply with unsigned types as long as
	 there is a wider mode to use, or it is the smaller of the two
	 types that is unsigned.  Note that type1 >= type2, always.  */
      if ((from_unsigned1
	   && TYPE_PRECISION (type1) == GET_MODE_PRECISION (from_mode))
	  || (from_unsigned2
	      && TYPE_PRECISION (type2) == GET_MODE_PRECISION (from_mode)))
	{
	  if (!GET_MODE_WIDER_MODE (from_mode).exists (&from_mode)
	      || GET_MODE_SIZE (from_mode) >= GET_MODE_SIZE (to_mode))
	    return false;
	}

      from_unsigned1 = from_unsigned2 = false;
      optype = build_nonstandard_integer_type (GET_MODE_PRECISION (from_mode),
					       false);
    }

  /* If there was a conversion between the multiply and addition
     then we need to make sure it fits a multiply-and-accumulate.
     The should be a single mode change which does not change the
     value.  */
  if (conv_stmt)
    {
      /* We use the original, unmodified data types for this.  */
      tree from_type = TREE_TYPE (gimple_assign_rhs1 (conv_stmt));
      tree to_type = TREE_TYPE (gimple_assign_lhs (conv_stmt));
      int data_size = TYPE_PRECISION (type1) + TYPE_PRECISION (type2);
      bool is_unsigned = TYPE_UNSIGNED (type1) && TYPE_UNSIGNED (type2);

      if (TYPE_PRECISION (from_type) > TYPE_PRECISION (to_type))
	{
	  /* Conversion is a truncate.  */
	  if (TYPE_PRECISION (to_type) < data_size)
	    return false;
	}
      else if (TYPE_PRECISION (from_type) < TYPE_PRECISION (to_type))
	{
	  /* Conversion is an extend.  Check it's the right sort.  */
	  if (TYPE_UNSIGNED (from_type) != is_unsigned
	      && !(is_unsigned && TYPE_PRECISION (from_type) > data_size))
	    return false;
	}
      /* else convert is a no-op for our purposes.  */
    }

  /* Verify that the machine can perform a widening multiply
     accumulate in this mode/signedness combination, otherwise
     this transformation is likely to pessimize code.  */
  this_optab = optab_for_tree_code (wmult_code, optype, optab_default);
  handler = find_widening_optab_handler_and_mode (this_optab, to_mode,
						  from_mode, &actual_mode);

  if (handler == CODE_FOR_nothing)
    return false;

  /* Ensure that the inputs to the handler are in the correct precison
     for the opcode.  This will be the full mode size.  */
  actual_precision = GET_MODE_PRECISION (actual_mode);
  if (actual_precision != TYPE_PRECISION (type1)
      || from_unsigned1 != TYPE_UNSIGNED (type1))
    mult_rhs1 = build_and_insert_cast (gsi, loc,
				       build_nonstandard_integer_type
				         (actual_precision, from_unsigned1),
				       mult_rhs1);
  if (actual_precision != TYPE_PRECISION (type2)
      || from_unsigned2 != TYPE_UNSIGNED (type2))
    mult_rhs2 = build_and_insert_cast (gsi, loc,
				       build_nonstandard_integer_type
					 (actual_precision, from_unsigned2),
				       mult_rhs2);

  if (!useless_type_conversion_p (type, TREE_TYPE (add_rhs)))
    add_rhs = build_and_insert_cast (gsi, loc, type, add_rhs);

  /* Handle constants.  */
  if (TREE_CODE (mult_rhs1) == INTEGER_CST)
    mult_rhs1 = fold_convert (type1, mult_rhs1);
  if (TREE_CODE (mult_rhs2) == INTEGER_CST)
    mult_rhs2 = fold_convert (type2, mult_rhs2);

  gimple_assign_set_rhs_with_ops (gsi, wmult_code, mult_rhs1, mult_rhs2,
				  add_rhs);
  update_stmt (gsi_stmt (*gsi));
  widen_mul_stats.maccs_inserted++;
  return true;
}

/* Combine the multiplication at MUL_STMT with operands MULOP1 and MULOP2
   with uses in additions and subtractions to form fused multiply-add
   operations.  Returns true if successful and MUL_STMT should be removed.  */

static bool
convert_mult_to_fma (gimple *mul_stmt, tree op1, tree op2)
{
  tree mul_result = gimple_get_lhs (mul_stmt);
  tree type = TREE_TYPE (mul_result);
  gimple *use_stmt, *neguse_stmt;
  gassign *fma_stmt;
  use_operand_p use_p;
  imm_use_iterator imm_iter;

  if (FLOAT_TYPE_P (type)
      && flag_fp_contract_mode == FP_CONTRACT_OFF)
    return false;

  /* We don't want to do bitfield reduction ops.  */
  if (INTEGRAL_TYPE_P (type)
      && !type_has_mode_precision_p (type))
    return false;

  /* If the target doesn't support it, don't generate it.  We assume that
     if fma isn't available then fms, fnma or fnms are not either.  */
  if (optab_handler (fma_optab, TYPE_MODE (type)) == CODE_FOR_nothing)
    return false;

  /* If the multiplication has zero uses, it is kept around probably because
     of -fnon-call-exceptions.  Don't optimize it away in that case,
     it is DCE job.  */
  if (has_zero_uses (mul_result))
    return false;

  /* Make sure that the multiplication statement becomes dead after
     the transformation, thus that all uses are transformed to FMAs.
     This means we assume that an FMA operation has the same cost
     as an addition.  */
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, mul_result)
    {
      enum tree_code use_code;
      tree result = mul_result;
      bool negate_p = false;

      use_stmt = USE_STMT (use_p);

      if (is_gimple_debug (use_stmt))
	continue;

      /* For now restrict this operations to single basic blocks.  In theory
	 we would want to support sinking the multiplication in
	 m = a*b;
	 if ()
	   ma = m + c;
	 else
	   d = m;
	 to form a fma in the then block and sink the multiplication to the
	 else block.  */
      if (gimple_bb (use_stmt) != gimple_bb (mul_stmt))
	return false;

      if (!is_gimple_assign (use_stmt))
	return false;

      use_code = gimple_assign_rhs_code (use_stmt);

      /* A negate on the multiplication leads to FNMA.  */
      if (use_code == NEGATE_EXPR)
	{
	  ssa_op_iter iter;
	  use_operand_p usep;

	  result = gimple_assign_lhs (use_stmt);

	  /* Make sure the negate statement becomes dead with this
	     single transformation.  */
	  if (!single_imm_use (gimple_assign_lhs (use_stmt),
			       &use_p, &neguse_stmt))
	    return false;

	  /* Make sure the multiplication isn't also used on that stmt.  */
	  FOR_EACH_PHI_OR_STMT_USE (usep, neguse_stmt, iter, SSA_OP_USE)
	    if (USE_FROM_PTR (usep) == mul_result)
	      return false;

	  /* Re-validate.  */
	  use_stmt = neguse_stmt;
	  if (gimple_bb (use_stmt) != gimple_bb (mul_stmt))
	    return false;
	  if (!is_gimple_assign (use_stmt))
	    return false;

	  use_code = gimple_assign_rhs_code (use_stmt);
	  negate_p = true;
	}

      switch (use_code)
	{
	case MINUS_EXPR:
	  if (gimple_assign_rhs2 (use_stmt) == result)
	    negate_p = !negate_p;
	  break;
	case PLUS_EXPR:
	  break;
	default:
	  /* FMA can only be formed from PLUS and MINUS.  */
	  return false;
	}

      /* If the subtrahend (gimple_assign_rhs2 (use_stmt)) is computed
	 by a MULT_EXPR that we'll visit later, we might be able to
	 get a more profitable match with fnma.
	 OTOH, if we don't, a negate / fma pair has likely lower latency
	 that a mult / subtract pair.  */
      if (use_code == MINUS_EXPR && !negate_p
	  && gimple_assign_rhs1 (use_stmt) == result
	  && optab_handler (fms_optab, TYPE_MODE (type)) == CODE_FOR_nothing
	  && optab_handler (fnma_optab, TYPE_MODE (type)) != CODE_FOR_nothing)
	{
	  tree rhs2 = gimple_assign_rhs2 (use_stmt);

	  if (TREE_CODE (rhs2) == SSA_NAME)
	    {
	      gimple *stmt2 = SSA_NAME_DEF_STMT (rhs2);
	      if (has_single_use (rhs2)
		  && is_gimple_assign (stmt2)
		  && gimple_assign_rhs_code (stmt2) == MULT_EXPR)
	      return false;
	    }
	}

      /* We can't handle a * b + a * b.  */
      if (gimple_assign_rhs1 (use_stmt) == gimple_assign_rhs2 (use_stmt))
	return false;

      /* While it is possible to validate whether or not the exact form
	 that we've recognized is available in the backend, the assumption
	 is that the transformation is never a loss.  For instance, suppose
	 the target only has the plain FMA pattern available.  Consider
	 a*b-c -> fma(a,b,-c): we've exchanged MUL+SUB for FMA+NEG, which
	 is still two operations.  Consider -(a*b)-c -> fma(-a,b,-c): we
	 still have 3 operations, but in the FMA form the two NEGs are
	 independent and could be run in parallel.  */
    }

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, mul_result)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
      enum tree_code use_code;
      tree addop, mulop1 = op1, result = mul_result;
      bool negate_p = false;

      if (is_gimple_debug (use_stmt))
	continue;

      use_code = gimple_assign_rhs_code (use_stmt);
      if (use_code == NEGATE_EXPR)
	{
	  result = gimple_assign_lhs (use_stmt);
	  single_imm_use (gimple_assign_lhs (use_stmt), &use_p, &neguse_stmt);
	  gsi_remove (&gsi, true);
	  release_defs (use_stmt);

	  use_stmt = neguse_stmt;
	  gsi = gsi_for_stmt (use_stmt);
	  use_code = gimple_assign_rhs_code (use_stmt);
	  negate_p = true;
	}

      if (gimple_assign_rhs1 (use_stmt) == result)
	{
	  addop = gimple_assign_rhs2 (use_stmt);
	  /* a * b - c -> a * b + (-c)  */
	  if (gimple_assign_rhs_code (use_stmt) == MINUS_EXPR)
	    addop = force_gimple_operand_gsi (&gsi,
					      build1 (NEGATE_EXPR,
						      type, addop),
					      true, NULL_TREE, true,
					      GSI_SAME_STMT);
	}
      else
	{
	  addop = gimple_assign_rhs1 (use_stmt);
	  /* a - b * c -> (-b) * c + a */
	  if (gimple_assign_rhs_code (use_stmt) == MINUS_EXPR)
	    negate_p = !negate_p;
	}

      if (negate_p)
	mulop1 = force_gimple_operand_gsi (&gsi,
					   build1 (NEGATE_EXPR,
						   type, mulop1),
					   true, NULL_TREE, true,
					   GSI_SAME_STMT);

      fma_stmt = gimple_build_assign (gimple_assign_lhs (use_stmt),
				      FMA_EXPR, mulop1, op2, addop);
      gsi_replace (&gsi, fma_stmt, true);
      widen_mul_stats.fmas_inserted++;
    }

  return true;
}


/* Helper function of match_uaddsub_overflow.  Return 1
   if USE_STMT is unsigned overflow check ovf != 0 for
   STMT, -1 if USE_STMT is unsigned overflow check ovf == 0
   and 0 otherwise.  */

static int
uaddsub_overflow_check_p (gimple *stmt, gimple *use_stmt)
{
  enum tree_code ccode = ERROR_MARK;
  tree crhs1 = NULL_TREE, crhs2 = NULL_TREE;
  if (gimple_code (use_stmt) == GIMPLE_COND)
    {
      ccode = gimple_cond_code (use_stmt);
      crhs1 = gimple_cond_lhs (use_stmt);
      crhs2 = gimple_cond_rhs (use_stmt);
    }
  else if (is_gimple_assign (use_stmt))
    {
      if (gimple_assign_rhs_class (use_stmt) == GIMPLE_BINARY_RHS)
	{
	  ccode = gimple_assign_rhs_code (use_stmt);
	  crhs1 = gimple_assign_rhs1 (use_stmt);
	  crhs2 = gimple_assign_rhs2 (use_stmt);
	}
      else if (gimple_assign_rhs_code (use_stmt) == COND_EXPR)
	{
	  tree cond = gimple_assign_rhs1 (use_stmt);
	  if (COMPARISON_CLASS_P (cond))
	    {
	      ccode = TREE_CODE (cond);
	      crhs1 = TREE_OPERAND (cond, 0);
	      crhs2 = TREE_OPERAND (cond, 1);
	    }
	  else
	    return 0;
	}
      else
	return 0;
    }
  else
    return 0;

  if (TREE_CODE_CLASS (ccode) != tcc_comparison)
    return 0;

  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);

  switch (ccode)
    {
    case GT_EXPR:
    case LE_EXPR:
      /* r = a - b; r > a or r <= a
	 r = a + b; a > r or a <= r or b > r or b <= r.  */
      if ((code == MINUS_EXPR && crhs1 == lhs && crhs2 == rhs1)
	  || (code == PLUS_EXPR && (crhs1 == rhs1 || crhs1 == rhs2)
	      && crhs2 == lhs))
	return ccode == GT_EXPR ? 1 : -1;
      break;
    case LT_EXPR:
    case GE_EXPR:
      /* r = a - b; a < r or a >= r
	 r = a + b; r < a or r >= a or r < b or r >= b.  */
      if ((code == MINUS_EXPR && crhs1 == rhs1 && crhs2 == lhs)
	  || (code == PLUS_EXPR && crhs1 == lhs
	      && (crhs2 == rhs1 || crhs2 == rhs2)))
	return ccode == LT_EXPR ? 1 : -1;
      break;
    default:
      break;
    }
  return 0;
}

/* Recognize for unsigned x
   x = y - z;
   if (x > y)
   where there are other uses of x and replace it with
   _7 = SUB_OVERFLOW (y, z);
   x = REALPART_EXPR <_7>;
   _8 = IMAGPART_EXPR <_7>;
   if (_8)
   and similarly for addition.  */

static bool
match_uaddsub_overflow (gimple_stmt_iterator *gsi, gimple *stmt,
			enum tree_code code)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree type = TREE_TYPE (lhs);
  use_operand_p use_p;
  imm_use_iterator iter;
  bool use_seen = false;
  bool ovf_use_seen = false;
  gimple *use_stmt;

  gcc_checking_assert (code == PLUS_EXPR || code == MINUS_EXPR);
  if (!INTEGRAL_TYPE_P (type)
      || !TYPE_UNSIGNED (type)
      || has_zero_uses (lhs)
      || has_single_use (lhs)
      || optab_handler (code == PLUS_EXPR ? uaddv4_optab : usubv4_optab,
			TYPE_MODE (type)) == CODE_FOR_nothing)
    return false;

  FOR_EACH_IMM_USE_FAST (use_p, iter, lhs)
    {
      use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;

      if (uaddsub_overflow_check_p (stmt, use_stmt))
	ovf_use_seen = true;
      else
	use_seen = true;
      if (ovf_use_seen && use_seen)
	break;
    }

  if (!ovf_use_seen || !use_seen)
    return false;

  tree ctype = build_complex_type (type);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  gcall *g = gimple_build_call_internal (code == PLUS_EXPR
					 ? IFN_ADD_OVERFLOW : IFN_SUB_OVERFLOW,
					 2, rhs1, rhs2);
  tree ctmp = make_ssa_name (ctype);
  gimple_call_set_lhs (g, ctmp);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  gassign *g2 = gimple_build_assign (lhs, REALPART_EXPR,
				     build1 (REALPART_EXPR, type, ctmp));
  gsi_replace (gsi, g2, true);
  tree ovf = make_ssa_name (type);
  g2 = gimple_build_assign (ovf, IMAGPART_EXPR,
			    build1 (IMAGPART_EXPR, type, ctmp));
  gsi_insert_after (gsi, g2, GSI_NEW_STMT);

  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
    {
      if (is_gimple_debug (use_stmt))
	continue;

      int ovf_use = uaddsub_overflow_check_p (stmt, use_stmt);
      if (ovf_use == 0)
	continue;
      if (gimple_code (use_stmt) == GIMPLE_COND)
	{
	  gcond *cond_stmt = as_a <gcond *> (use_stmt);
	  gimple_cond_set_lhs (cond_stmt, ovf);
	  gimple_cond_set_rhs (cond_stmt, build_int_cst (type, 0));
	  gimple_cond_set_code (cond_stmt, ovf_use == 1 ? NE_EXPR : EQ_EXPR);
	}
      else
	{
	  gcc_checking_assert (is_gimple_assign (use_stmt));
	  if (gimple_assign_rhs_class (use_stmt) == GIMPLE_BINARY_RHS)
	    {
	      gimple_assign_set_rhs1 (use_stmt, ovf);
	      gimple_assign_set_rhs2 (use_stmt, build_int_cst (type, 0));
	      gimple_assign_set_rhs_code (use_stmt,
					  ovf_use == 1 ? NE_EXPR : EQ_EXPR);
	    }
	  else
	    {
	      gcc_checking_assert (gimple_assign_rhs_code (use_stmt)
				   == COND_EXPR);
	      tree cond = build2 (ovf_use == 1 ? NE_EXPR : EQ_EXPR,
				  boolean_type_node, ovf,
				  build_int_cst (type, 0));
	      gimple_assign_set_rhs1 (use_stmt, cond);
	    }
	}
      update_stmt (use_stmt);
    }
  return true;
}

/* Return true if target has support for divmod.  */

static bool
target_supports_divmod_p (optab divmod_optab, optab div_optab, machine_mode mode) 
{
  /* If target supports hardware divmod insn, use it for divmod.  */
  if (optab_handler (divmod_optab, mode) != CODE_FOR_nothing)
    return true;

  /* Check if libfunc for divmod is available.  */
  rtx libfunc = optab_libfunc (divmod_optab, mode);
  if (libfunc != NULL_RTX)
    {
      /* If optab_handler exists for div_optab, perhaps in a wider mode,
	 we don't want to use the libfunc even if it exists for given mode.  */ 
      machine_mode div_mode;
      FOR_EACH_MODE_FROM (div_mode, mode)
	if (optab_handler (div_optab, div_mode) != CODE_FOR_nothing)
	  return false;

      return targetm.expand_divmod_libfunc != NULL;
    }
  
  return false; 
}

/* Check if stmt is candidate for divmod transform.  */

static bool
divmod_candidate_p (gassign *stmt)
{
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  machine_mode mode = TYPE_MODE (type);
  optab divmod_optab, div_optab;

  if (TYPE_UNSIGNED (type))
    {
      divmod_optab = udivmod_optab;
      div_optab = udiv_optab;
    }
  else
    {
      divmod_optab = sdivmod_optab;
      div_optab = sdiv_optab;
    }

  tree op1 = gimple_assign_rhs1 (stmt);
  tree op2 = gimple_assign_rhs2 (stmt);

  /* Disable the transform if either is a constant, since division-by-constant
     may have specialized expansion.  */
  if (CONSTANT_CLASS_P (op1) || CONSTANT_CLASS_P (op2))
    return false;

  /* Exclude the case where TYPE_OVERFLOW_TRAPS (type) as that should
     expand using the [su]divv optabs.  */
  if (TYPE_OVERFLOW_TRAPS (type))
    return false;
  
  if (!target_supports_divmod_p (divmod_optab, div_optab, mode)) 
    return false;

  return true;
}

/* This function looks for:
   t1 = a TRUNC_DIV_EXPR b;
   t2 = a TRUNC_MOD_EXPR b;
   and transforms it to the following sequence:
   complex_tmp = DIVMOD (a, b);
   t1 = REALPART_EXPR(a);
   t2 = IMAGPART_EXPR(b);
   For conditions enabling the transform see divmod_candidate_p().

   The pass has three parts:
   1) Find top_stmt which is trunc_div or trunc_mod stmt and dominates all
      other trunc_div_expr and trunc_mod_expr stmts.
   2) Add top_stmt and all trunc_div and trunc_mod stmts dominated by top_stmt
      to stmts vector.
   3) Insert DIVMOD call just before top_stmt and update entries in
      stmts vector to use return value of DIMOVD (REALEXPR_PART for div,
      IMAGPART_EXPR for mod).  */

static bool
convert_to_divmod (gassign *stmt)
{
  if (stmt_can_throw_internal (stmt)
      || !divmod_candidate_p (stmt))
    return false;

  tree op1 = gimple_assign_rhs1 (stmt);
  tree op2 = gimple_assign_rhs2 (stmt);
  
  imm_use_iterator use_iter;
  gimple *use_stmt;
  auto_vec<gimple *> stmts; 

  gimple *top_stmt = stmt; 
  basic_block top_bb = gimple_bb (stmt);

  /* Part 1: Try to set top_stmt to "topmost" stmt that dominates
     at-least stmt and possibly other trunc_div/trunc_mod stmts
     having same operands as stmt.  */

  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, op1)
    {
      if (is_gimple_assign (use_stmt)
	  && (gimple_assign_rhs_code (use_stmt) == TRUNC_DIV_EXPR
	      || gimple_assign_rhs_code (use_stmt) == TRUNC_MOD_EXPR)
	  && operand_equal_p (op1, gimple_assign_rhs1 (use_stmt), 0)
	  && operand_equal_p (op2, gimple_assign_rhs2 (use_stmt), 0))
	{
	  if (stmt_can_throw_internal (use_stmt))
	    continue;

	  basic_block bb = gimple_bb (use_stmt);

	  if (bb == top_bb)
	    {
	      if (gimple_uid (use_stmt) < gimple_uid (top_stmt))
		top_stmt = use_stmt;
	    }
	  else if (dominated_by_p (CDI_DOMINATORS, top_bb, bb))
	    {
	      top_bb = bb;
	      top_stmt = use_stmt;
	    }
	}
    }

  tree top_op1 = gimple_assign_rhs1 (top_stmt);
  tree top_op2 = gimple_assign_rhs2 (top_stmt);

  stmts.safe_push (top_stmt);
  bool div_seen = (gimple_assign_rhs_code (top_stmt) == TRUNC_DIV_EXPR);

  /* Part 2: Add all trunc_div/trunc_mod statements domianted by top_bb
     to stmts vector. The 2nd loop will always add stmt to stmts vector, since
     gimple_bb (top_stmt) dominates gimple_bb (stmt), so the
     2nd loop ends up adding at-least single trunc_mod_expr stmt.  */  

  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, top_op1)
    {
      if (is_gimple_assign (use_stmt)
	  && (gimple_assign_rhs_code (use_stmt) == TRUNC_DIV_EXPR
	      || gimple_assign_rhs_code (use_stmt) == TRUNC_MOD_EXPR)
	  && operand_equal_p (top_op1, gimple_assign_rhs1 (use_stmt), 0)
	  && operand_equal_p (top_op2, gimple_assign_rhs2 (use_stmt), 0))
	{
	  if (use_stmt == top_stmt
	      || stmt_can_throw_internal (use_stmt)
	      || !dominated_by_p (CDI_DOMINATORS, gimple_bb (use_stmt), top_bb))
	    continue;

	  stmts.safe_push (use_stmt);
	  if (gimple_assign_rhs_code (use_stmt) == TRUNC_DIV_EXPR)
	    div_seen = true;
	}
    }

  if (!div_seen)
    return false;

  /* Part 3: Create libcall to internal fn DIVMOD:
     divmod_tmp = DIVMOD (op1, op2).  */

  gcall *call_stmt = gimple_build_call_internal (IFN_DIVMOD, 2, op1, op2);
  tree res = make_temp_ssa_name (build_complex_type (TREE_TYPE (op1)),
				 call_stmt, "divmod_tmp");
  gimple_call_set_lhs (call_stmt, res);
  /* We rejected throwing statements above.  */
  gimple_call_set_nothrow (call_stmt, true);

  /* Insert the call before top_stmt.  */
  gimple_stmt_iterator top_stmt_gsi = gsi_for_stmt (top_stmt);
  gsi_insert_before (&top_stmt_gsi, call_stmt, GSI_SAME_STMT);

  widen_mul_stats.divmod_calls_inserted++;		

  /* Update all statements in stmts vector:
     lhs = op1 TRUNC_DIV_EXPR op2 -> lhs = REALPART_EXPR<divmod_tmp>
     lhs = op1 TRUNC_MOD_EXPR op2 -> lhs = IMAGPART_EXPR<divmod_tmp>.  */

  for (unsigned i = 0; stmts.iterate (i, &use_stmt); ++i)
    {
      tree new_rhs;

      switch (gimple_assign_rhs_code (use_stmt))
	{
	  case TRUNC_DIV_EXPR:
	    new_rhs = fold_build1 (REALPART_EXPR, TREE_TYPE (op1), res);
	    break;

	  case TRUNC_MOD_EXPR:
	    new_rhs = fold_build1 (IMAGPART_EXPR, TREE_TYPE (op1), res);
	    break;

	  default:
	    gcc_unreachable ();
	}

      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
      gimple_assign_set_rhs_from_tree (&gsi, new_rhs);
      update_stmt (use_stmt);
    }

  return true; 
}    

/* Find integer multiplications where the operands are extended from
   smaller types, and replace the MULT_EXPR with a WIDEN_MULT_EXPR
   where appropriate.  */

namespace {

const pass_data pass_data_optimize_widening_mul =
{
  GIMPLE_PASS, /* type */
  "widening_mul", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_optimize_widening_mul : public gimple_opt_pass
{
public:
  pass_optimize_widening_mul (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_optimize_widening_mul, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return flag_expensive_optimizations && optimize;
    }

  virtual unsigned int execute (function *);

}; // class pass_optimize_widening_mul

unsigned int
pass_optimize_widening_mul::execute (function *fun)
{
  basic_block bb;
  bool cfg_changed = false;

  memset (&widen_mul_stats, 0, sizeof (widen_mul_stats));
  calculate_dominance_info (CDI_DOMINATORS);
  renumber_gimple_stmt_uids ();

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi);)
        {
	  gimple *stmt = gsi_stmt (gsi);
	  enum tree_code code;

	  if (is_gimple_assign (stmt))
	    {
	      code = gimple_assign_rhs_code (stmt);
	      switch (code)
		{
		case MULT_EXPR:
		  if (!convert_mult_to_widen (stmt, &gsi)
		      && !convert_expand_mult_copysign (stmt, &gsi)
		      && convert_mult_to_fma (stmt,
					      gimple_assign_rhs1 (stmt),
					      gimple_assign_rhs2 (stmt)))
		    {
		      gsi_remove (&gsi, true);
		      release_defs (stmt);
		      continue;
		    }
		  break;

		case PLUS_EXPR:
		case MINUS_EXPR:
		  if (!convert_plusminus_to_widen (&gsi, stmt, code))
		    match_uaddsub_overflow (&gsi, stmt, code);
		  break;

		case TRUNC_MOD_EXPR:
		  convert_to_divmod (as_a<gassign *> (stmt));
		  break;

		default:;
		}
	    }
	  else if (is_gimple_call (stmt)
		   && gimple_call_lhs (stmt))
	    {
	      tree fndecl = gimple_call_fndecl (stmt);
	      if (fndecl
		  && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
		{
		  switch (DECL_FUNCTION_CODE (fndecl))
		    {
		      case BUILT_IN_POWF:
		      case BUILT_IN_POW:
		      case BUILT_IN_POWL:
			if (TREE_CODE (gimple_call_arg (stmt, 1)) == REAL_CST
			    && real_equal
			         (&TREE_REAL_CST (gimple_call_arg (stmt, 1)),
				  &dconst2)
			    && convert_mult_to_fma (stmt,
						    gimple_call_arg (stmt, 0),
						    gimple_call_arg (stmt, 0)))
			  {
			    unlink_stmt_vdef (stmt);
			    if (gsi_remove (&gsi, true)
				&& gimple_purge_dead_eh_edges (bb))
			      cfg_changed = true;
			    release_defs (stmt);
			    continue;
			  }
			  break;

		      default:;
		    }
		}
	    }
	  gsi_next (&gsi);
	}
    }

  statistics_counter_event (fun, "widening multiplications inserted",
			    widen_mul_stats.widen_mults_inserted);
  statistics_counter_event (fun, "widening maccs inserted",
			    widen_mul_stats.maccs_inserted);
  statistics_counter_event (fun, "fused multiply-adds inserted",
			    widen_mul_stats.fmas_inserted);
  statistics_counter_event (fun, "divmod calls inserted",
			    widen_mul_stats.divmod_calls_inserted);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_optimize_widening_mul (gcc::context *ctxt)
{
  return new pass_optimize_widening_mul (ctxt);
}
