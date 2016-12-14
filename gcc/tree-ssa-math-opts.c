/* Global, SSA-based optimizations using mathematical identities.
   Copyright (C) 2005-2015 Free Software Foundation, Inc.

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
   hy the same divisor.  This is probably because modern processors
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
#include "tm.h"
#include "flags.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "predict.h"
#include "hard-reg-set.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "stor-layout.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "hashtab.h"
#include "rtl.h"
#include "statistics.h"
#include "real.h"
#include "fixed-value.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "alloc-pool.h"
#include "target.h"
#include "gimple-pretty-print.h"
#include "builtins.h"

/* FIXME: RTL headers have to be included here for optabs.  */
#include "rtl.h"		/* Because optabs.h wants enum rtx_code.  */
#include "expr.h"		/* Because optabs.h wants sepops.  */
#include "insn-codes.h"
#include "optabs.h"

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
  gimple recip_def_stmt;

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
} widen_mul_stats;

/* The instance of "struct occurrence" representing the highest
   interesting block in the dominator tree.  */
static struct occurrence *occ_head;

/* Allocation pool for getting instances of "struct occurrence".  */
static alloc_pool occ_pool;



/* Allocate and return a new struct occurrence for basic block BB, and
   whose children list is headed by CHILDREN.  */
static struct occurrence *
occ_new (basic_block bb, struct occurrence *children)
{
  struct occurrence *occ;

  bb->aux = occ = (struct occurrence *) pool_alloc (occ_pool);
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
is_division_by (gimple use_stmt, tree def)
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
  gimple use_stmt = USE_STMT (use_p);
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
  pool_free (occ_pool, occ);

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
      gimple use_stmt = USE_STMT (use_p);
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
      gimple use_stmt;
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

  occ_pool = create_alloc_pool ("dominators for recip",
				sizeof (struct occurrence),
				n_basic_blocks_for_fn (fun) / 3 + 1);

  memset (&reciprocal_stats, 0, sizeof (reciprocal_stats));
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

#ifdef ENABLE_CHECKING
  FOR_EACH_BB_FN (bb, fun)
    gcc_assert (!bb->aux);
#endif

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
	  gimple stmt = gsi_stmt (gsi);

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
	  gimple stmt = gsi_stmt (gsi);
	  tree fndecl;

	  if (is_gimple_assign (stmt)
	      && gimple_assign_rhs_code (stmt) == RDIV_EXPR)
	    {
	      tree arg1 = gimple_assign_rhs2 (stmt);
	      gimple stmt1;

	      if (TREE_CODE (arg1) != SSA_NAME)
		continue;

	      stmt1 = SSA_NAME_DEF_STMT (arg1);

	      if (is_gimple_call (stmt1)
		  && gimple_call_lhs (stmt1)
		  && (fndecl = gimple_call_fndecl (stmt1))
		  && (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
		      || DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD))
		{
		  enum built_in_function code;
		  bool md_code, fail;
		  imm_use_iterator ui;
		  use_operand_p use_p;

		  code = DECL_FUNCTION_CODE (fndecl);
		  md_code = DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD;

		  fndecl = targetm.builtin_reciprocal (code, md_code, false);
		  if (!fndecl)
		    continue;

		  /* Check that all uses of the SSA name are divisions,
		     otherwise replacing the defining statement will do
		     the wrong thing.  */
		  fail = false;
		  FOR_EACH_IMM_USE_FAST (use_p, ui, arg1)
		    {
		      gimple stmt2 = USE_STMT (use_p);
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

		  gimple_replace_ssa_lhs (stmt1, arg1);
		  gimple_call_set_fndecl (stmt1, fndecl);
		  update_stmt (stmt1);
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
  free_alloc_pool (occ_pool);
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
maybe_record_sincos (vec<gimple> *stmts,
		     basic_block *top_bb, gimple use_stmt)
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
  gimple def_stmt, use_stmt, stmt;
  int seen_cos = 0, seen_sin = 0, seen_cexpi = 0;
  auto_vec<gimple> stmts;
  basic_block top_bb = NULL;
  int i;
  bool cfg_changed = false;

  type = TREE_TYPE (name);
  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, name)
    {
      if (gimple_code (use_stmt) != GIMPLE_CALL
	  || !gimple_call_lhs (use_stmt)
	  || !(fndecl = gimple_call_fndecl (use_stmt))
	  || !gimple_call_builtin_p (use_stmt, BUILT_IN_NORMAL))
	continue;

      switch (DECL_FUNCTION_CODE (fndecl))
	{
	CASE_FLT_FN (BUILT_IN_COS):
	  seen_cos |= maybe_record_sincos (&stmts, &top_bb, use_stmt) ? 1 : 0;
	  break;

	CASE_FLT_FN (BUILT_IN_SIN):
	  seen_sin |= maybe_record_sincos (&stmts, &top_bb, use_stmt) ? 1 : 0;
	  break;

	CASE_FLT_FN (BUILT_IN_CEXPI):
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
      fndecl = gimple_call_fndecl (use_stmt);

      switch (DECL_FUNCTION_CODE (fndecl))
	{
	CASE_FLT_FN (BUILT_IN_COS):
	  rhs = fold_build1 (REALPART_EXPR, type, res);
	  break;

	CASE_FLT_FN (BUILT_IN_SIN):
	  rhs = fold_build1 (IMAGPART_EXPR, type, res);
	  break;

	CASE_FLT_FN (BUILT_IN_CEXPI):
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
  gimple stmt = gimple_build_assign (result, build1 (code, type, arg0));
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

/* ARG0 and ARG1 are the two arguments to a pow builtin call in GSI
   with location info LOC.  If possible, create an equivalent and
   less expensive sequence of statements prior to GSI, and return an
   expession holding the result.  */

static tree
gimple_expand_builtin_pow (gimple_stmt_iterator *gsi, location_t loc, 
			   tree arg0, tree arg1)
{
  REAL_VALUE_TYPE c, cint, dconst1_4, dconst3_4, dconst1_3, dconst1_6;
  REAL_VALUE_TYPE c2, dconst3;
  HOST_WIDE_INT n;
  tree type, sqrtfn, cbrtfn, sqrt_arg0, sqrt_sqrt, result, cbrt_x, powi_cbrt_x;
  machine_mode mode;
  bool hw_sqrt_exists, c_is_int, c2_is_int;

  /* If the exponent isn't a constant, there's nothing of interest
     to be done.  */
  if (TREE_CODE (arg1) != REAL_CST)
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
	      && optimize_bb_for_speed_p (gsi_bb (*gsi))
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
      && REAL_VALUES_EQUAL (c, dconsthalf)
      && !HONOR_SIGNED_ZEROS (mode))
    return build_and_insert_call (gsi, loc, sqrtfn, arg0);

  /* Optimize pow(x,0.25) = sqrt(sqrt(x)).  Assume on most machines that
     a builtin sqrt instruction is smaller than a call to pow with 0.25,
     so do this optimization even if -Os.  Don't do this optimization
     if we don't have a hardware sqrt insn.  */
  dconst1_4 = dconst1;
  SET_REAL_EXP (&dconst1_4, REAL_EXP (&dconst1_4) - 2);
  hw_sqrt_exists = optab_handler (sqrt_optab, mode) != CODE_FOR_nothing;

  if (flag_unsafe_math_optimizations
      && sqrtfn
      && REAL_VALUES_EQUAL (c, dconst1_4)
      && hw_sqrt_exists)
    {
      /* sqrt(x)  */
      sqrt_arg0 = build_and_insert_call (gsi, loc, sqrtfn, arg0);

      /* sqrt(sqrt(x))  */
      return build_and_insert_call (gsi, loc, sqrtfn, sqrt_arg0);
    }
      
  /* Optimize pow(x,0.75) = sqrt(x) * sqrt(sqrt(x)) unless we are
     optimizing for space.  Don't do this optimization if we don't have
     a hardware sqrt insn.  */
  real_from_integer (&dconst3_4, VOIDmode, 3, SIGNED);
  SET_REAL_EXP (&dconst3_4, REAL_EXP (&dconst3_4) - 2);

  if (flag_unsafe_math_optimizations
      && sqrtfn
      && optimize_function_for_speed_p (cfun)
      && REAL_VALUES_EQUAL (c, dconst3_4)
      && hw_sqrt_exists)
    {
      /* sqrt(x)  */
      sqrt_arg0 = build_and_insert_call (gsi, loc, sqrtfn, arg0);

      /* sqrt(sqrt(x))  */
      sqrt_sqrt = build_and_insert_call (gsi, loc, sqrtfn, sqrt_arg0);

      /* sqrt(x) * sqrt(sqrt(x))  */
      return build_and_insert_binop (gsi, loc, "powroot", MULT_EXPR,
				     sqrt_arg0, sqrt_sqrt);
    }

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
      && (gimple_val_nonnegative_real_p (arg0) || !HONOR_NANS (mode))
      && REAL_VALUES_EQUAL (c, dconst1_3))
    return build_and_insert_call (gsi, loc, cbrtfn, arg0);
  
  /* Optimize pow(x,1./6.) = cbrt(sqrt(x)).  Don't do this optimization
     if we don't have a hardware sqrt insn.  */
  dconst1_6 = dconst1_3;
  SET_REAL_EXP (&dconst1_6, REAL_EXP (&dconst1_6) - 1);

  if (flag_unsafe_math_optimizations
      && sqrtfn
      && cbrtfn
      && (gimple_val_nonnegative_real_p (arg0) || !HONOR_NANS (mode))
      && optimize_function_for_speed_p (cfun)
      && hw_sqrt_exists
      && REAL_VALUES_EQUAL (c, dconst1_6))
    {
      /* sqrt(x)  */
      sqrt_arg0 = build_and_insert_call (gsi, loc, sqrtfn, arg0);

      /* cbrt(sqrt(x))  */
      return build_and_insert_call (gsi, loc, cbrtfn, sqrt_arg0);
    }

  /* Optimize pow(x,c), where n = 2c for some nonzero integer n
     and c not an integer, into

       sqrt(x) * powi(x, n/2),                n > 0;
       1.0 / (sqrt(x) * powi(x, abs(n/2))),   n < 0.

     Do not calculate the powi factor when n/2 = 0.  */
  real_arithmetic (&c2, MULT_EXPR, &c, &dconst2);
  n = real_to_integer (&c2);
  real_from_integer (&cint, VOIDmode, n, SIGNED);
  c2_is_int = real_identical (&c2, &cint);

  if (flag_unsafe_math_optimizations
      && sqrtfn
      && c2_is_int
      && !c_is_int
      && optimize_function_for_speed_p (cfun))
    {
      tree powi_x_ndiv2 = NULL_TREE;

      /* Attempt to fold powi(arg0, abs(n/2)) into multiplies.  If not
         possible or profitable, give up.  Skip the degenerate case when
         n is 1 or -1, where the result is always 1.  */
      if (absu_hwi (n) != 1)
	{
	  powi_x_ndiv2 = gimple_expand_builtin_powi (gsi, loc, arg0,
						     abs_hwi (n / 2));
	  if (!powi_x_ndiv2)
	    return NULL_TREE;
	}

      /* Calculate sqrt(x).  When n is not 1 or -1, multiply it by the
	 result of the optimal multiply sequence just calculated.  */
      sqrt_arg0 = build_and_insert_call (gsi, loc, sqrtfn, arg0);

      if (absu_hwi (n) == 1)
	result = sqrt_arg0;
      else
	result = build_and_insert_binop (gsi, loc, "powroot", MULT_EXPR,
					 sqrt_arg0, powi_x_ndiv2);

      /* If n is negative, reciprocate the result.  */
      if (n < 0)
	result = build_and_insert_binop (gsi, loc, "powroot", RDIV_EXPR,
					 build_real (type, dconst1), result);
      return result;
    }

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
      && (gimple_val_nonnegative_real_p (arg0) || !HONOR_NANS (mode))
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
  0, /* properties_provided */
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
	  gimple stmt = gsi_stmt (gsi);
	  tree fndecl;

	  /* Only the last stmt in a bb could throw, no need to call
	     gimple_purge_dead_eh_edges if we change something in the middle
	     of a basic block.  */
	  cleanup_eh = false;

	  if (is_gimple_call (stmt)
	      && gimple_call_lhs (stmt)
	      && (fndecl = gimple_call_fndecl (stmt))
	      && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
	    {
	      tree arg, arg0, arg1, result;
	      HOST_WIDE_INT n;
	      location_t loc;

	      switch (DECL_FUNCTION_CODE (fndecl))
		{
		CASE_FLT_FN (BUILT_IN_COS):
		CASE_FLT_FN (BUILT_IN_SIN):
		CASE_FLT_FN (BUILT_IN_CEXPI):
		  /* Make sure we have either sincos or cexp.  */
		  if (!targetm.libc_has_function (function_c99_math_complex)
		      && !targetm.libc_has_function (function_sincos))
		    break;

		  arg = gimple_call_arg (stmt, 0);
		  if (TREE_CODE (arg) == SSA_NAME)
		    cfg_changed |= execute_cse_sincos_1 (arg);
		  break;

		CASE_FLT_FN (BUILT_IN_POW):
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

		CASE_FLT_FN (BUILT_IN_POWI):
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

		CASE_FLT_FN (BUILT_IN_CABS):
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

  free_dominance_info (CDI_DOMINATORS);
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
     to easily get BASE_ADDR + offset + lowest bytepos.

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
verify_symbolic_number_p (struct symbolic_number *n, gimple stmt)
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

  if (size < 64 / BITS_PER_MARKER)
    n->n &= ((uint64_t) 1 << (size * BITS_PER_MARKER)) - 1;

  return true;
}

/* Check if STMT might be a byte swap or a nop from a memory source and returns
   the answer. If so, REF is that memory source and the base of the memory area
   accessed and the offset of the access from that base are recorded in N.  */

bool
find_bswap_or_nop_load (gimple stmt, tree ref, struct symbolic_number *n)
{
  /* Leaf node is an array or component ref. Memorize its base and
     offset from base to compare to other such leaf node.  */
  HOST_WIDE_INT bitsize, bitpos;
  machine_mode mode;
  int unsignedp, volatilep;
  tree offset, base_addr;

  /* Not prepared to handle PDP endian.  */
  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    return false;

  if (!gimple_assign_load_p (stmt) || gimple_has_volatile_ops (stmt))
    return false;

  base_addr = get_inner_reference (ref, &bitsize, &bitpos, &offset, &mode,
				   &unsignedp, &volatilep, false);

  if (TREE_CODE (base_addr) == MEM_REF)
    {
      offset_int bit_offset = 0;
      tree off = TREE_OPERAND (base_addr, 1);

      if (!integer_zerop (off))
	{
	  offset_int boff, coff = mem_ref_offset (base_addr);
	  boff = wi::lshift (coff, LOG2_BITS_PER_UNIT);
	  bit_offset += boff;
	}

      base_addr = TREE_OPERAND (base_addr, 0);

      /* Avoid returning a negative bitpos as this may wreak havoc later.  */
      if (wi::neg_p (bit_offset))
	{
	  offset_int mask = wi::mask <offset_int> (LOG2_BITS_PER_UNIT, false);
	  offset_int tem = bit_offset.and_not (mask);
	  /* TEM is the bitpos rounded to BITS_PER_UNIT towards -Inf.
	     Subtract it to BIT_OFFSET and add it (scaled) to OFFSET.  */
	  bit_offset -= tem;
	  tem = wi::arshift (tem, LOG2_BITS_PER_UNIT);
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

static gimple
perform_symbolic_merge (gimple source_stmt1, struct symbolic_number *n1,
			gimple source_stmt2, struct symbolic_number *n2,
			struct symbolic_number *n)
{
  int i, size;
  uint64_t mask;
  gimple source_stmt;
  struct symbolic_number *n_start;

  /* Sources are different, cancel bswap if they are not memory location with
     the same base (array, structure, ...).  */
  if (gimple_assign_rhs1 (source_stmt1) != gimple_assign_rhs1 (source_stmt2))
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

  return source_stmt;
}

/* find_bswap_or_nop_1 invokes itself recursively with N and tries to perform
   the operation given by the rhs of STMT on the result.  If the operation
   could successfully be executed the function returns a gimple stmt whose
   rhs's first tree is the expression of the source operand and NULL
   otherwise.  */

static gimple
find_bswap_or_nop_1 (gimple stmt, struct symbolic_number *n, int limit)
{
  enum tree_code code;
  tree rhs1, rhs2 = NULL;
  gimple rhs1_stmt, rhs2_stmt, source_stmt1;
  enum gimple_rhs_class rhs_class;

  if (!limit || !is_gimple_assign (stmt))
    return NULL;

  rhs1 = gimple_assign_rhs1 (stmt);

  if (find_bswap_or_nop_load (stmt, rhs1, n))
    return stmt;

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
      gimple source_stmt, source_stmt2;

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

static gimple
find_bswap_or_nop (gimple stmt, struct symbolic_number *n, bool *bswap)
{
  /* The number which the find_bswap_or_nop_1 result should match in order
     to have a full byte swap.  The number is shifted to the right
     according to the size of the symbolic number before using it.  */
  uint64_t cmpxchg = CMPXCHG;
  uint64_t cmpnop = CMPNOP;

  gimple ins_stmt;
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
    {
      unsigned HOST_WIDE_INT rsize;
      uint64_t tmpn;

      for (tmpn = n->n, rsize = 0; tmpn; tmpn >>= BITS_PER_MARKER, rsize++);
      if (BYTES_BIG_ENDIAN && n->range != rsize)
	/* This implies an offset, which is currently not handled by
	   bswap_replace.  */
	return NULL;
      n->range = rsize;
    }

  /* Zero out the extra bits of N and CMP*.  */
  if (n->range < (int) sizeof (int64_t))
    {
      uint64_t mask;

      mask = ((uint64_t) 1 << (n->range * BITS_PER_MARKER)) - 1;
      cmpxchg >>= (64 / BITS_PER_MARKER - n->range) * BITS_PER_MARKER;
      cmpnop &= mask;
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
  if (!n->base_addr && n->n == cmpnop)
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
bswap_replace (gimple cur_stmt, gimple ins_stmt, tree fndecl, tree bswap_type,
	       tree load_type, struct symbolic_number *n, bool bswap)
{
  gimple_stmt_iterator gsi;
  tree src, tmp, tgt;
  gimple bswap_stmt;

  gsi = gsi_for_stmt (cur_stmt);
  src = n->src;
  tgt = gimple_assign_lhs (cur_stmt);

  /* Need to load the value from memory first.  */
  if (n->base_addr)
    {
      gimple_stmt_iterator gsi_ins = gsi_for_stmt (ins_stmt);
      tree addr_expr, addr_tmp, val_expr, val_tmp;
      tree load_offset_ptr, aligned_load_type;
      gimple addr_stmt, load_stmt;
      unsigned align;
      HOST_WIDE_INT load_offset = 0;
      basic_block ins_bb, cur_bb;

      ins_bb = gimple_bb (ins_stmt);
      cur_bb = gimple_bb (cur_stmt);
      if (!dominated_by_p (CDI_DOMINATORS, cur_bb, ins_bb))
	return false;

      align = get_object_alignment (src);
      /* If the new access is smaller than the original one, we need
	 to perform big endian adjustment.  */
      if (BYTES_BIG_ENDIAN)
	{
	  HOST_WIDE_INT bitsize, bitpos;
	  machine_mode mode;
	  int unsignedp, volatilep;
	  tree offset;

	  get_inner_reference (src, &bitsize, &bitpos, &offset, &mode,
			       &unsignedp, &volatilep, false);
	  if (n->range < (unsigned HOST_WIDE_INT) bitsize)
	    {
	      load_offset = (bitsize - n->range) / BITS_PER_UNIT;
	      unsigned HOST_WIDE_INT l
		= (load_offset * BITS_PER_UNIT) & (align - 1);
	      if (l)
		align = l & -l;
	    }
	}

      if (bswap
	  && align < GET_MODE_ALIGNMENT (TYPE_MODE (load_type))
	  && SLOW_UNALIGNED_ACCESS (TYPE_MODE (load_type), align))
	return false;

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
	      print_gimple_stmt (dump_file, cur_stmt, 0, 0);
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
      gimple convert_stmt;

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
      gimple convert_stmt;

      tmp = make_temp_ssa_name (bswap_type, NULL, "bswapdst");
      convert_stmt = gimple_build_assign (tgt, NOP_EXPR, tmp);
      gsi_insert_after (&gsi, convert_stmt, GSI_SAME_STMT);
    }

  gimple_set_lhs (bswap_stmt, tmp);

  if (dump_file)
    {
      fprintf (dump_file, "%d bit bswap implementation found at: ",
	       (int) n->range);
      print_gimple_stmt (dump_file, cur_stmt, 0, 0);
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
	  gimple ins_stmt, cur_stmt = gsi_stmt (gsi);
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
widening_mult_conversion_strippable_p (tree result_type, gimple stmt)
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
  gimple stmt;
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
is_widening_mult_p (gimple stmt,
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
      tree tmp;
      tmp = *type1_out;
      *type1_out = *type2_out;
      *type2_out = tmp;
      tmp = *rhs1_out;
      *rhs1_out = *rhs2_out;
      *rhs2_out = tmp;
    }

  return true;
}

/* Process a single gimple statement STMT, which has a MULT_EXPR as
   its rhs, and try to convert it into a WIDEN_MULT_EXPR.  The return
   value is true iff we converted the statement.  */

static bool
convert_mult_to_widen (gimple stmt, gimple_stmt_iterator *gsi)
{
  tree lhs, rhs1, rhs2, type, type1, type2;
  enum insn_code handler;
  machine_mode to_mode, from_mode, actual_mode;
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

  to_mode = TYPE_MODE (type);
  from_mode = TYPE_MODE (type1);
  from_unsigned1 = TYPE_UNSIGNED (type1);
  from_unsigned2 = TYPE_UNSIGNED (type2);

  if (from_unsigned1 && from_unsigned2)
    op = umul_widen_optab;
  else if (!from_unsigned1 && !from_unsigned2)
    op = smul_widen_optab;
  else
    op = usmul_widen_optab;

  handler = find_widening_optab_handler_and_mode (op, to_mode, from_mode,
						  0, &actual_mode);

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
	      from_mode = GET_MODE_WIDER_MODE (from_mode);
	      if (GET_MODE_SIZE (to_mode) <= GET_MODE_SIZE (from_mode))
		return false;
	    }

	  op = smul_widen_optab;
	  handler = find_widening_optab_handler_and_mode (op, to_mode,
							  from_mode, 0,
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
convert_plusminus_to_widen (gimple_stmt_iterator *gsi, gimple stmt,
			    enum tree_code code)
{
  gimple rhs1_stmt = NULL, rhs2_stmt = NULL;
  gimple conv1_stmt = NULL, conv2_stmt = NULL, conv_stmt;
  tree type, type1, type2, optype;
  tree lhs, rhs1, rhs2, mult_rhs1, mult_rhs2, add_rhs;
  enum tree_code rhs1_code = ERROR_MARK, rhs2_code = ERROR_MARK;
  optab this_optab;
  enum tree_code wmult_code;
  enum insn_code handler;
  machine_mode to_mode, from_mode, actual_mode;
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

  to_mode = TYPE_MODE (type);
  from_mode = TYPE_MODE (type1);
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
	  from_mode = GET_MODE_WIDER_MODE (from_mode);
	  if (GET_MODE_SIZE (from_mode) >= GET_MODE_SIZE (to_mode))
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
						  from_mode, 0, &actual_mode);

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
convert_mult_to_fma (gimple mul_stmt, tree op1, tree op2)
{
  tree mul_result = gimple_get_lhs (mul_stmt);
  tree type = TREE_TYPE (mul_result);
  gimple use_stmt, neguse_stmt;
  gassign *fma_stmt;
  use_operand_p use_p;
  imm_use_iterator imm_iter;

  if (FLOAT_TYPE_P (type)
      && flag_fp_contract_mode == FP_CONTRACT_OFF)
    return false;

  /* We don't want to do bitfield reduction ops.  */
  if (INTEGRAL_TYPE_P (type)
      && (TYPE_PRECISION (type)
	  != GET_MODE_PRECISION (TYPE_MODE (type))))
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
	      gimple stmt2 = SSA_NAME_DEF_STMT (rhs2);
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

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi);)
        {
	  gimple stmt = gsi_stmt (gsi);
	  enum tree_code code;

	  if (is_gimple_assign (stmt))
	    {
	      code = gimple_assign_rhs_code (stmt);
	      switch (code)
		{
		case MULT_EXPR:
		  if (!convert_mult_to_widen (stmt, &gsi)
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
		  convert_plusminus_to_widen (&gsi, stmt, code);
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
			    && REAL_VALUES_EQUAL
			         (TREE_REAL_CST (gimple_call_arg (stmt, 1)),
				  dconst2)
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

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_optimize_widening_mul (gcc::context *ctxt)
{
  return new pass_optimize_widening_mul (ctxt);
}
