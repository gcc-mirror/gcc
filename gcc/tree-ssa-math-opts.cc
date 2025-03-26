/* Global, SSA-based optimizations using mathematical identities.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.

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

   If we did this using domwalk.cc, an efficient implementation would have
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
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "stor-layout.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "builtins.h"
#include "internal-fn.h"
#include "case-cfn-macros.h"
#include "optabs-libfuncs.h"
#include "tree-eh.h"
#include "targhooks.h"
#include "domwalk.h"
#include "tree-ssa-math-opts.h"
#include "dbgcnt.h"
#include "cfghooks.h"

/* This structure represents one basic block that either computes a
   division, or is a common dominator for basic block that compute a
   division.  */
struct occurrence {
  /* The basic block represented by this structure.  */
  basic_block bb = basic_block();

  /* If non-NULL, the SSA_NAME holding the definition for a reciprocal
     inserted in BB.  */
  tree recip_def = tree();

  /* If non-NULL, the SSA_NAME holding the definition for a squared
     reciprocal inserted in BB.  */
  tree square_recip_def = tree();

  /* If non-NULL, the GIMPLE_ASSIGN for a reciprocal computation that
     was inserted in BB.  */
  gimple *recip_def_stmt = nullptr;

  /* Pointer to a list of "struct occurrence"s for blocks dominated
     by BB.  */
  struct occurrence *children = nullptr;

  /* Pointer to the next "struct occurrence"s in the list of blocks
     sharing a common dominator.  */
  struct occurrence *next = nullptr;

  /* The number of divisions that are in BB before compute_merit.  The
     number of divisions that are in BB or post-dominate it after
     compute_merit.  */
  int num_divisions = 0;

  /* True if the basic block has a division, false if it is a common
     dominator for basic blocks that do.  If it is false and trapping
     math is active, BB is not a candidate for inserting a reciprocal.  */
  bool bb_has_division = false;

  /* Construct a struct occurrence for basic block BB, and whose
     children list is headed by CHILDREN.  */
  occurrence (basic_block bb, struct occurrence *children)
  : bb (bb), children (children)
  {
    bb->aux = this;
  }

  /* Destroy a struct occurrence and remove it from its basic block.  */
  ~occurrence ()
  {
    bb->aux = nullptr;
  }

  /* Allocate memory for a struct occurrence from OCC_POOL.  */
  static void* operator new (size_t);

  /* Return memory for a struct occurrence to OCC_POOL.  */
  static void operator delete (void*, size_t);
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

  /* Number of conversions removed.  */
  int conv_removed;

} sincos_stats;

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

  /* Number of highpart multiplication ops inserted.  */
  int highpart_mults_inserted;
} widen_mul_stats;

/* The instance of "struct occurrence" representing the highest
   interesting block in the dominator tree.  */
static struct occurrence *occ_head;

/* Allocation pool for getting instances of "struct occurrence".  */
static object_allocator<occurrence> *occ_pool;

void* occurrence::operator new (size_t n)
{
  gcc_assert (n == sizeof(occurrence));
  return occ_pool->allocate_raw ();
}

void occurrence::operator delete (void *occ, size_t n)
{
  gcc_assert (n == sizeof(occurrence));
  occ_pool->remove_raw (occ);
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
	  new_occ = new occurrence (dom, new_occ);
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

/* Register that we found a division in BB.
   IMPORTANCE is a measure of how much weighting to give
   that division.  Use IMPORTANCE = 2 to register a single
   division.  If the division is going to be found multiple
   times use 1 (as it is with squares).  */

static inline void
register_division_in (basic_block bb, int importance)
{
  struct occurrence *occ;

  occ = (struct occurrence *) bb->aux;
  if (!occ)
    {
      occ = new occurrence (bb, NULL);
      insert_bb (occ, ENTRY_BLOCK_PTR_FOR_FN (cfun), &occ_head);
    }

  occ->bb_has_division = true;
  occ->num_divisions += importance;
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
	 && gimple_assign_rhs1 (use_stmt) != def
	 && !stmt_can_throw_internal (cfun, use_stmt);
}

/* Return TRUE if USE_STMT is a multiplication of DEF by A.  */
static inline bool
is_mult_by (gimple *use_stmt, tree def, tree a)
{
  if (gimple_code (use_stmt) == GIMPLE_ASSIGN
      && gimple_assign_rhs_code (use_stmt) == MULT_EXPR)
    {
      tree op0 = gimple_assign_rhs1 (use_stmt);
      tree op1 = gimple_assign_rhs2 (use_stmt);

      return (op0 == def && op1 == a)
	      || (op0 == a && op1 == def);
    }
  return 0;
}

/* Return whether USE_STMT is DEF * DEF.  */
static inline bool
is_square_of (gimple *use_stmt, tree def)
{
  return is_mult_by (use_stmt, def, def);
}

/* Return whether USE_STMT is a floating-point division by
   DEF * DEF.  */
static inline bool
is_division_by_square (gimple *use_stmt, tree def)
{
  if (gimple_code (use_stmt) == GIMPLE_ASSIGN
      && gimple_assign_rhs_code (use_stmt) == RDIV_EXPR
      && gimple_assign_rhs1 (use_stmt) != gimple_assign_rhs2 (use_stmt)
      && !stmt_can_throw_internal (cfun, use_stmt))
    {
      tree denominator = gimple_assign_rhs2 (use_stmt);
      if (TREE_CODE (denominator) == SSA_NAME)
	return is_square_of (SSA_NAME_DEF_STMT (denominator), def);
    }
  return 0;
}

/* Walk the subset of the dominator tree rooted at OCC, setting the
   RECIP_DEF field to a definition of 1.0 / DEF that can be used in
   the given basic block.  The field may be left NULL, of course,
   if it is not possible or profitable to do the optimization.

   DEF_BSI is an iterator pointing at the statement defining DEF.
   If RECIP_DEF is set, a dominator already has a computation that can
   be used.

   If should_insert_square_recip is set, then this also inserts
   the square of the reciprocal immediately after the definition
   of the reciprocal.  */

static void
insert_reciprocals (gimple_stmt_iterator *def_gsi, struct occurrence *occ,
		    tree def, tree recip_def, tree square_recip_def,
		    int should_insert_square_recip, int threshold)
{
  tree type;
  gassign *new_stmt, *new_square_stmt;
  gimple_stmt_iterator gsi;
  struct occurrence *occ_child;

  if (!recip_def
      && (occ->bb_has_division || !flag_trapping_math)
      /* Divide by two as all divisions are counted twice in
	 the costing loop.  */
      && occ->num_divisions / 2 >= threshold)
    {
      /* Make a variable with the replacement and substitute it.  */
      type = TREE_TYPE (def);
      recip_def = create_tmp_reg (type, "reciptmp");
      new_stmt = gimple_build_assign (recip_def, RDIV_EXPR,
				      build_one_cst (type), def);

      if (should_insert_square_recip)
	{
	  square_recip_def = create_tmp_reg (type, "powmult_reciptmp");
	  new_square_stmt = gimple_build_assign (square_recip_def, MULT_EXPR,
						 recip_def, recip_def);
	}

      if (occ->bb_has_division)
	{
	  /* Case 1: insert before an existing division.  */
	  gsi = gsi_after_labels (occ->bb);
	  while (!gsi_end_p (gsi)
		 && (!is_division_by (gsi_stmt (gsi), def))
		 && (!is_division_by_square (gsi_stmt (gsi), def)))
	    gsi_next (&gsi);

	  gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);
	  if (should_insert_square_recip)
	    gsi_insert_before (&gsi, new_square_stmt, GSI_SAME_STMT);
	}
      else if (def_gsi && occ->bb == gsi_bb (*def_gsi))
	{
	  /* Case 2: insert right after the definition.  Note that this will
	     never happen if the definition statement can throw, because in
	     that case the sole successor of the statement's basic block will
	     dominate all the uses as well.  */
	  gsi_insert_after (def_gsi, new_stmt, GSI_NEW_STMT);
	  if (should_insert_square_recip)
	    gsi_insert_after (def_gsi, new_square_stmt, GSI_NEW_STMT);
	}
      else
	{
	  /* Case 3: insert in a basic block not containing defs/uses.  */
	  gsi = gsi_after_labels (occ->bb);
	  gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);
	  if (should_insert_square_recip)
	    gsi_insert_before (&gsi, new_square_stmt, GSI_SAME_STMT);
	}

      reciprocal_stats.rdivs_inserted++;

      occ->recip_def_stmt = new_stmt;
    }

  occ->recip_def = recip_def;
  occ->square_recip_def = square_recip_def;
  for (occ_child = occ->children; occ_child; occ_child = occ_child->next)
    insert_reciprocals (def_gsi, occ_child, def, recip_def,
			square_recip_def, should_insert_square_recip,
			threshold);
}

/* Replace occurrences of expr / (x * x) with expr * ((1 / x) * (1 / x)).
   Take as argument the use for (x * x).  */
static inline void
replace_reciprocal_squares (use_operand_p use_p)
{
  gimple *use_stmt = USE_STMT (use_p);
  basic_block bb = gimple_bb (use_stmt);
  struct occurrence *occ = (struct occurrence *) bb->aux;

  if (optimize_bb_for_speed_p (bb) && occ->square_recip_def
      && occ->recip_def)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
      gimple_assign_set_rhs_code (use_stmt, MULT_EXPR);
      gimple_assign_set_rhs2 (use_stmt, occ->square_recip_def);
      SET_USE (use_p, occ->square_recip_def);
      fold_stmt_inplace (&gsi);
      update_stmt (use_stmt);
    }
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
  delete occ;

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

/* Transform sequences like
   t = sqrt (a)
   x = 1.0 / t;
   r1 = x * x;
   r2 = a * x;
   into:
   t = sqrt (a)
   r1 = 1.0 / a;
   r2 = t;
   x = r1 * r2;
   depending on the uses of x, r1, r2.  This removes one multiplication and
   allows the sqrt and division operations to execute in parallel.
   DEF_GSI is the gsi of the initial division by sqrt that defines
   DEF (x in the example above).  */

static void
optimize_recip_sqrt (gimple_stmt_iterator *def_gsi, tree def)
{
  gimple *use_stmt;
  imm_use_iterator use_iter;
  gimple *stmt = gsi_stmt (*def_gsi);
  tree x = def;
  tree orig_sqrt_ssa_name = gimple_assign_rhs2 (stmt);
  tree div_rhs1 = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (orig_sqrt_ssa_name) != SSA_NAME
      || TREE_CODE (div_rhs1) != REAL_CST
      || !real_equal (&TREE_REAL_CST (div_rhs1), &dconst1))
    return;

  gcall *sqrt_stmt
    = dyn_cast <gcall *> (SSA_NAME_DEF_STMT (orig_sqrt_ssa_name));

  if (!sqrt_stmt || !gimple_call_lhs (sqrt_stmt))
    return;

  switch (gimple_call_combined_fn (sqrt_stmt))
    {
    CASE_CFN_SQRT:
    CASE_CFN_SQRT_FN:
      break;

    default:
      return;
    }
  tree a = gimple_call_arg (sqrt_stmt, 0);

  /* We have 'a' and 'x'.  Now analyze the uses of 'x'.  */

  /* Statements that use x in x * x.  */
  auto_vec<gimple *> sqr_stmts;
  /* Statements that use x in a * x.  */
  auto_vec<gimple *> mult_stmts;
  bool has_other_use = false;
  bool mult_on_main_path = false;

  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, x)
    {
      if (is_gimple_debug (use_stmt))
	continue;
      if (is_square_of (use_stmt, x))
	{
	  sqr_stmts.safe_push (use_stmt);
	  if (gimple_bb (use_stmt) == gimple_bb (stmt))
	    mult_on_main_path = true;
	}
      else if (is_mult_by (use_stmt, x, a))
	{
	  mult_stmts.safe_push (use_stmt);
	  if (gimple_bb (use_stmt) == gimple_bb (stmt))
	    mult_on_main_path = true;
	}
      else
	has_other_use = true;
    }

  /* In the x * x and a * x cases we just rewire stmt operands or
     remove multiplications.  In the has_other_use case we introduce
     a multiplication so make sure we don't introduce a multiplication
     on a path where there was none.  */
  if (has_other_use && !mult_on_main_path)
    return;

  if (sqr_stmts.is_empty () && mult_stmts.is_empty ())
    return;

  /* If x = 1.0 / sqrt (a) has uses other than those optimized here we want
     to be able to compose it from the sqr and mult cases.  */
  if (has_other_use && (sqr_stmts.is_empty () || mult_stmts.is_empty ()))
    return;

  if (dump_file)
    {
      fprintf (dump_file, "Optimizing reciprocal sqrt multiplications of\n");
      print_gimple_stmt (dump_file, sqrt_stmt, 0, TDF_NONE);
      print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);
      fprintf (dump_file, "\n");
    }

  bool delete_div = !has_other_use;
  tree sqr_ssa_name = NULL_TREE;
  if (!sqr_stmts.is_empty ())
    {
      /* r1 = x * x.  Transform the original
	 x = 1.0 / t
	 into
	 tmp1 = 1.0 / a
	 r1 = tmp1.  */

      sqr_ssa_name
	= make_temp_ssa_name (TREE_TYPE (a), NULL, "recip_sqrt_sqr");

      if (dump_file)
	{
	  fprintf (dump_file, "Replacing original division\n");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);
	  fprintf (dump_file, "with new division\n");
	}
      stmt
	= gimple_build_assign (sqr_ssa_name, gimple_assign_rhs_code (stmt),
			       gimple_assign_rhs1 (stmt), a);
      gsi_insert_before (def_gsi, stmt, GSI_SAME_STMT);
      gsi_remove (def_gsi, true);
      *def_gsi = gsi_for_stmt (stmt);
      fold_stmt_inplace (def_gsi);
      update_stmt (stmt);

      if (dump_file)
	print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);

      delete_div = false;
      gimple *sqr_stmt;
      unsigned int i;
      FOR_EACH_VEC_ELT (sqr_stmts, i, sqr_stmt)
	{
	  gimple_stmt_iterator gsi2 = gsi_for_stmt (sqr_stmt);
	  gimple_assign_set_rhs_from_tree (&gsi2, sqr_ssa_name);
	  update_stmt (sqr_stmt);
	}
    }
  if (!mult_stmts.is_empty ())
    {
      /* r2 = a * x.  Transform this into:
	 r2 = t (The original sqrt (a)).  */
      unsigned int i;
      gimple *mult_stmt = NULL;
      FOR_EACH_VEC_ELT (mult_stmts, i, mult_stmt)
	{
	  gimple_stmt_iterator gsi2 = gsi_for_stmt (mult_stmt);

	  if (dump_file)
	    {
	      fprintf (dump_file, "Replacing squaring multiplication\n");
	      print_gimple_stmt (dump_file, mult_stmt, 0, TDF_NONE);
	      fprintf (dump_file, "with assignment\n");
	    }
	  gimple_assign_set_rhs_from_tree (&gsi2, orig_sqrt_ssa_name);
	  fold_stmt_inplace (&gsi2);
	  update_stmt (mult_stmt);
	  if (dump_file)
	    print_gimple_stmt (dump_file, mult_stmt, 0, TDF_NONE);
      }
    }

  if (has_other_use)
    {
      /* Using the two temporaries tmp1, tmp2 from above
	 the original x is now:
	 x = tmp1 * tmp2.  */
      gcc_assert (orig_sqrt_ssa_name);
      gcc_assert (sqr_ssa_name);

      gimple *new_stmt
	= gimple_build_assign (x, MULT_EXPR,
			       orig_sqrt_ssa_name, sqr_ssa_name);
      gsi_insert_after (def_gsi, new_stmt, GSI_NEW_STMT);
      update_stmt (stmt);
    }
  else if (delete_div)
    {
      /* Remove the original division.  */
      gimple_stmt_iterator gsi2 = gsi_for_stmt (stmt);
      gsi_remove (&gsi2, true);
      release_defs (stmt);
    }
  else
    release_ssa_name (x);
}

/* Look for floating-point divisions among DEF's uses, and try to
   replace them by multiplications with the reciprocal.  Add
   as many statements computing the reciprocal as needed.

   DEF must be a GIMPLE register of a floating-point type.  */

static void
execute_cse_reciprocals_1 (gimple_stmt_iterator *def_gsi, tree def)
{
  use_operand_p use_p, square_use_p;
  imm_use_iterator use_iter, square_use_iter;
  tree square_def;
  struct occurrence *occ;
  int count = 0;
  int threshold;
  int square_recip_count = 0;
  int sqrt_recip_count = 0;

  gcc_assert (FLOAT_TYPE_P (TREE_TYPE (def)) && TREE_CODE (def) == SSA_NAME);
  threshold = targetm.min_divisions_for_recip_mul (TYPE_MODE (TREE_TYPE (def)));

  /* If DEF is a square (x * x), count the number of divisions by x.
     If there are more divisions by x than by (DEF * DEF), prefer to optimize
     the reciprocal of x instead of DEF.  This improves cases like:
       def = x * x
       t0 = a / def
       t1 = b / def
       t2 = c / x
     Reciprocal optimization of x results in 1 division rather than 2 or 3.  */
  gimple *def_stmt = SSA_NAME_DEF_STMT (def);

  if (is_gimple_assign (def_stmt)
      && gimple_assign_rhs_code (def_stmt) == MULT_EXPR
      && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME
      && gimple_assign_rhs1 (def_stmt) == gimple_assign_rhs2 (def_stmt))
    {
      tree op0 = gimple_assign_rhs1 (def_stmt);

      FOR_EACH_IMM_USE_FAST (use_p, use_iter, op0)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (is_division_by (use_stmt, op0))
	    sqrt_recip_count++;
	}
    }

  FOR_EACH_IMM_USE_FAST (use_p, use_iter, def)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_division_by (use_stmt, def))
	{
	  register_division_in (gimple_bb (use_stmt), 2);
	  count++;
	}

      if (is_square_of (use_stmt, def))
	{
	  square_def = gimple_assign_lhs (use_stmt);
	  FOR_EACH_IMM_USE_FAST (square_use_p, square_use_iter, square_def)
	    {
	      gimple *square_use_stmt = USE_STMT (square_use_p);
	      if (is_division_by (square_use_stmt, square_def))
		{
		  /* This is executed twice for each division by a square.  */
		  register_division_in (gimple_bb (square_use_stmt), 1);
		  square_recip_count++;
		}
	    }
	}
    }

  /* Square reciprocals were counted twice above.  */
  square_recip_count /= 2;

  /* If it is more profitable to optimize 1 / x, don't optimize 1 / (x * x).  */
  if (sqrt_recip_count > square_recip_count)
    goto out;

  /* Do the expensive part only if we can hope to optimize something.  */
  if (count + square_recip_count >= threshold && count >= 1)
    {
      gimple *use_stmt;
      for (occ = occ_head; occ; occ = occ->next)
	{
	  compute_merit (occ);
	  insert_reciprocals (def_gsi, occ, def, NULL, NULL,
			      square_recip_count, threshold);
	}

      FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, def)
	{
	  if (is_division_by (use_stmt, def))
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, use_iter)
		replace_reciprocal (use_p);
	    }
	  else if (square_recip_count > 0 && is_square_of (use_stmt, def))
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, use_iter)
		{
		  /* Find all uses of the square that are divisions and
		   * replace them by multiplications with the inverse.  */
		  imm_use_iterator square_iterator;
		  gimple *powmult_use_stmt = USE_STMT (use_p);
		  tree powmult_def_name = gimple_assign_lhs (powmult_use_stmt);

		  FOR_EACH_IMM_USE_STMT (powmult_use_stmt,
					 square_iterator, powmult_def_name)
		    FOR_EACH_IMM_USE_ON_STMT (square_use_p, square_iterator)
		      {
			gimple *powmult_use_stmt = USE_STMT (square_use_p);
			if (is_division_by (powmult_use_stmt, powmult_def_name))
			  replace_reciprocal_squares (square_use_p);
		      }
		}
	    }
	}
    }

out:
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
  TV_TREE_RECIP, /* tv_id */
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
  bool gate (function *) final override
  {
    return optimize && flag_reciprocal_math;
  }
  unsigned int execute (function *) final override;

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
	    {
	      execute_cse_reciprocals_1 (&gsi, def);
	      stmt = gsi_stmt (gsi);
	      if (flag_unsafe_math_optimizations
		  && is_gimple_assign (stmt)
		  && gimple_assign_lhs (stmt) == def
		  && !stmt_can_throw_internal (cfun, stmt)
		  && gimple_assign_rhs_code (stmt) == RDIV_EXPR)
		optimize_recip_sqrt (&gsi, def);
	    }
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
			  || !fndecl_built_in_p (fndecl, BUILT_IN_MD))
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
		      gimple_move_vops (stmt2, call);
		      gimple_call_set_nothrow (stmt2,
					       gimple_call_nothrow_p (call));
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

/* If NAME is the result of a type conversion, look for other
   equivalent dominating or dominated conversions, and replace all
   uses with the earliest dominating name, removing the redundant
   conversions.  Return the prevailing name.  */

static tree
execute_cse_conv_1 (tree name, bool *cfg_changed)
{
  if (SSA_NAME_IS_DEFAULT_DEF (name)
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
    return name;

  gimple *def_stmt = SSA_NAME_DEF_STMT (name);

  if (!gimple_assign_cast_p (def_stmt))
    return name;

  tree src = gimple_assign_rhs1 (def_stmt);

  if (TREE_CODE (src) != SSA_NAME)
    return name;

  imm_use_iterator use_iter;
  gimple *use_stmt;

  /* Find the earliest dominating def.    */
  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, src)
    {
      if (use_stmt == def_stmt
	  || !gimple_assign_cast_p (use_stmt))
	continue;

      tree lhs = gimple_assign_lhs (use_stmt);

      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
	  || (gimple_assign_rhs1 (use_stmt)
	      != gimple_assign_rhs1 (def_stmt))
	  || !types_compatible_p (TREE_TYPE (name), TREE_TYPE (lhs)))
	continue;

      bool use_dominates;
      if (gimple_bb (def_stmt) == gimple_bb (use_stmt))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	  while (!gsi_end_p (gsi) && gsi_stmt (gsi) != def_stmt)
	    gsi_next (&gsi);
	  use_dominates = !gsi_end_p (gsi);
	}
      else if (dominated_by_p (CDI_DOMINATORS, gimple_bb (use_stmt),
			       gimple_bb (def_stmt)))
	use_dominates = false;
      else if (dominated_by_p (CDI_DOMINATORS, gimple_bb (def_stmt),
			       gimple_bb (use_stmt)))
	use_dominates = true;
      else
	continue;

      if (use_dominates)
	{
	  std::swap (name, lhs);
	  std::swap (def_stmt, use_stmt);
	}
    }

  /* Now go through all uses of SRC again, replacing the equivalent
     dominated conversions.  We may replace defs that were not
     dominated by the then-prevailing defs when we first visited
     them.  */
  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, src)
    {
      if (use_stmt == def_stmt
	  || !gimple_assign_cast_p (use_stmt))
	continue;

      tree lhs = gimple_assign_lhs (use_stmt);

      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
	  || (gimple_assign_rhs1 (use_stmt)
	      != gimple_assign_rhs1 (def_stmt))
	  || !types_compatible_p (TREE_TYPE (name), TREE_TYPE (lhs)))
	continue;

      basic_block use_bb = gimple_bb (use_stmt);
      if (gimple_bb (def_stmt) == use_bb
	  || dominated_by_p (CDI_DOMINATORS, use_bb, gimple_bb (def_stmt)))
	{
	  sincos_stats.conv_removed++;

	  gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	  replace_uses_by (lhs, name);
	  if (gsi_remove (&gsi, true)
	      && gimple_purge_dead_eh_edges (use_bb))
	    *cfg_changed = true;
	  release_defs (use_stmt);
	}
    }

  return name;
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
  tree fndecl, res, type = NULL_TREE;
  gimple *def_stmt, *use_stmt, *stmt;
  int seen_cos = 0, seen_sin = 0, seen_cexpi = 0;
  auto_vec<gimple *> stmts;
  basic_block top_bb = NULL;
  int i;
  bool cfg_changed = false;

  name = execute_cse_conv_1 (name, &cfg_changed);

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
	  continue;
	}

      tree t = mathfn_built_in_type (gimple_call_combined_fn (use_stmt));
      if (!type)
	{
	  type = t;
	  t = TREE_TYPE (name);
	}
      /* This checks that NAME has the right type in the first round,
	 and, in subsequent rounds, that the built_in type is the same
	 type, or a compatible type.  */
      if (type != t && !types_compatible_p (type, t))
	return false;
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
  val = absu_hwi (n);

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
		 unsigned HOST_WIDE_INT n, tree *cache)
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

tree
powi_as_mults (gimple_stmt_iterator *gsi, location_t loc,
	       tree arg0, HOST_WIDE_INT n)
{
  tree cache[POWI_TABLE_SIZE], result, type = TREE_TYPE (arg0);
  gassign *div_stmt;
  tree target;

  if (n == 0)
    return build_one_cst (type);

  memset (cache, 0, sizeof (cache));
  cache[1] = arg0;

  result = powi_as_mults_1 (gsi, loc, type, absu_hwi (n), cache);
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
  if ((n >= -1 && n <= 2)
      || (optimize_function_for_speed_p (cfun)
	  && powi_cost (n) <= POWI_MAX_MULTS))
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
				? param_max_pow_sqrt_depth
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

/* Go through all calls to sin, cos and cexpi and call execute_cse_sincos_1
   on the SSA_NAME argument of each of them.  */

namespace {

const pass_data pass_data_cse_sincos =
{
  GIMPLE_PASS, /* type */
  "sincos", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SINCOS, /* tv_id */
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
  bool gate (function *) final override
    {
      return optimize;
    }

  unsigned int execute (function *) final override;

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

      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
	  gimple *stmt = gsi_stmt (gsi);

	  if (is_gimple_call (stmt)
	      && gimple_call_lhs (stmt))
	    {
	      tree arg;
	      switch (gimple_call_combined_fn (stmt))
		{
		CASE_CFN_COS:
		CASE_CFN_SIN:
		CASE_CFN_CEXPI:
		  arg = gimple_call_arg (stmt, 0);
		  /* Make sure we have either sincos or cexp.  */
		  if (!targetm.libc_has_function (function_c99_math_complex,
						  TREE_TYPE (arg))
		      && !targetm.libc_has_function (function_sincos,
						     TREE_TYPE (arg)))
		    break;

		  if (TREE_CODE (arg) == SSA_NAME)
		    cfg_changed |= execute_cse_sincos_1 (arg);
		  break;
		default:
		  break;
		}
	    }
	}
    }

  statistics_counter_event (fun, "sincos statements inserted",
			    sincos_stats.inserted);
  statistics_counter_event (fun, "conv statements removed",
			    sincos_stats.conv_removed);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_cse_sincos (gcc::context *ctxt)
{
  return new pass_cse_sincos (ctxt);
}

/* Expand powi(x,n) into an optimal number of multiplies, when n is a
   constant.  */
namespace {

const pass_data pass_data_expand_pow =
{
  GIMPLE_PASS, /* type */
  "pow", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_POW, /* tv_id */
  PROP_ssa, /* properties_required */
  PROP_gimple_opt_math, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_expand_pow : public gimple_opt_pass
{
public:
  pass_expand_pow (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_expand_pow, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return optimize;
    }

  unsigned int execute (function *) final override;

}; // class pass_expand_pow

unsigned int
pass_expand_pow::execute (function *fun)
{
  basic_block bb;
  bool cfg_changed = false;

  calculate_dominance_info (CDI_DOMINATORS);

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
	      tree arg0, arg1, result;
	      HOST_WIDE_INT n;
	      location_t loc;

	      switch (gimple_call_combined_fn (stmt))
		{
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

		default:;
		}
	    }
	}
      if (cleanup_eh)
	cfg_changed |= gimple_purge_dead_eh_edges (bb);
    }

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_expand_pow (gcc::context *ctxt)
{
  return new pass_expand_pow (ctxt);
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
      /* Use tree_non_zero_bits to see if this operand is zero_extended
	 for unsigned widening multiplications or non-negative for
	 signed widening multiplications.  */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && (TYPE_PRECISION (type) & 1) == 0
	  && int_mode_for_size (TYPE_PRECISION (type) / 2, 1).exists ())
	{
	  unsigned int prec = TYPE_PRECISION (type);
	  unsigned int hprec = prec / 2;
	  wide_int bits = wide_int::from (tree_nonzero_bits (rhs), prec,
					  TYPE_SIGN (TREE_TYPE (rhs)));
	  if (TYPE_UNSIGNED (type)
	      && wi::bit_and (bits, wi::mask (hprec, true, prec)) == 0)
	    {
	      *type_out = build_nonstandard_integer_type (hprec, true);
	      /* X & MODE_MASK can be simplified to (T)X.  */
	      stmt = SSA_NAME_DEF_STMT (rhs);
	      if (is_gimple_assign (stmt)
		  && gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
		  && TREE_CODE (gimple_assign_rhs2 (stmt)) == INTEGER_CST
		  && wide_int::from (wi::to_wide (gimple_assign_rhs2 (stmt)),
				     prec, TYPE_SIGN (TREE_TYPE (rhs)))
		     == wi::mask (hprec, false, prec))
		*new_rhs_out = gimple_assign_rhs1 (stmt);
	      else
		*new_rhs_out = rhs;
	      return true;
	    }
	  else if (!TYPE_UNSIGNED (type)
		   && wi::bit_and (bits, wi::mask (hprec - 1, true, prec)) == 0)
	    {
	      *type_out = build_nonstandard_integer_type (hprec, false);
	      *new_rhs_out = rhs;
	      return true;
	    }
	}

      stmt = SSA_NAME_DEF_STMT (rhs);
      if (is_gimple_assign (stmt))
	{

	  if (widening_mult_conversion_strippable_p (type, stmt))
	    {
	      rhs1 = gimple_assign_rhs1 (stmt);

	      if (TREE_CODE (rhs1) == INTEGER_CST)
		{
		  *new_rhs_out = rhs1;
		  *type_out = NULL;
		  return true;
		}
	    }
	  else
	    rhs1 = rhs;
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

  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      if (TYPE_OVERFLOW_TRAPS (type))
	return false;
    }
  else if (TREE_CODE (type) != FIXED_POINT_TYPE)
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
   This only happens when the xorsign optab is defined, if the
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

  /* if any one of rhs1 and rhs2 is subject to abnormal coalescing,
     avoid the tranform. */
  if ((TREE_CODE (rhs1) == SSA_NAME
       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1))
      || (TREE_CODE (rhs2) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs2)))
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
	{
	  /* Expand can synthesize smul_widen_optab if the target
	     supports umul_widen_optab.  */
	  op = umul_widen_optab;
	  handler = find_widening_optab_handler_and_mode (op, to_mode,
							  from_mode,
							  &actual_mode);
	  if (handler == CODE_FOR_nothing)
	    return false;
	}
    }

  /* Ensure that the inputs to the handler are in the correct precison
     for the opcode.  This will be the full mode size.  */
  actual_precision = GET_MODE_PRECISION (actual_mode);
  if (2 * actual_precision > TYPE_PRECISION (type))
    return false;
  if (actual_precision != TYPE_PRECISION (type1)
      || from_unsigned1 != TYPE_UNSIGNED (type1))
    {
      if (!useless_type_conversion_p (type1, TREE_TYPE (rhs1)))
	{
	  if (TREE_CODE (rhs1) == INTEGER_CST)
	    rhs1 = fold_convert (type1, rhs1);
	  else
	    rhs1 = build_and_insert_cast (gsi, loc, type1, rhs1);
	}
      type1 = build_nonstandard_integer_type (actual_precision,
					      from_unsigned1);
    }
  if (!useless_type_conversion_p (type1, TREE_TYPE (rhs1)))
    {
      if (TREE_CODE (rhs1) == INTEGER_CST)
	rhs1 = fold_convert (type1, rhs1);
      else
	rhs1 = build_and_insert_cast (gsi, loc, type1, rhs1);
    }
  if (actual_precision != TYPE_PRECISION (type2)
      || from_unsigned2 != TYPE_UNSIGNED (type2))
    {
      if (!useless_type_conversion_p (type2, TREE_TYPE (rhs2)))
	{
	  if (TREE_CODE (rhs2) == INTEGER_CST)
	    rhs2 = fold_convert (type2, rhs2);
	  else
	    rhs2 = build_and_insert_cast (gsi, loc, type2, rhs2);
	}
      type2 = build_nonstandard_integer_type (actual_precision,
					      from_unsigned2);
    }
  if (!useless_type_conversion_p (type2, TREE_TYPE (rhs2)))
    {
      if (TREE_CODE (rhs2) == INTEGER_CST)
	rhs2 = fold_convert (type2, rhs2);
      else
	rhs2 = build_and_insert_cast (gsi, loc, type2, rhs2);
    }

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
  if ((TREE_CODE (type) != INTEGER_TYPE
       && TREE_CODE (type) != FIXED_POINT_TYPE)
      || !type_has_mode_precision_p (type))
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
     probably wiser not to do the conversion.  Also restrict this operation
     to single basic block to avoid moving the multiply to a different block
     with a higher execution frequency.  */
  if (code == PLUS_EXPR
      && (rhs1_code == MULT_EXPR || rhs1_code == WIDEN_MULT_EXPR))
    {
      if (!has_single_use (rhs1)
	  || gimple_bb (rhs1_stmt) != gimple_bb (stmt)
	  || !is_widening_mult_p (rhs1_stmt, &type1, &mult_rhs1,
				  &type2, &mult_rhs2))
	return false;
      add_rhs = rhs2;
      conv_stmt = conv1_stmt;
    }
  else if (rhs2_code == MULT_EXPR || rhs2_code == WIDEN_MULT_EXPR)
    {
      if (!has_single_use (rhs2)
	  || gimple_bb (rhs2_stmt) != gimple_bb (stmt)
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
    {
      if (!useless_type_conversion_p (type1, TREE_TYPE (mult_rhs1)))
	{
	  if (TREE_CODE (mult_rhs1) == INTEGER_CST)
	    mult_rhs1 = fold_convert (type1, mult_rhs1);
	  else
	    mult_rhs1 = build_and_insert_cast (gsi, loc, type1, mult_rhs1);
	}
      type1 = build_nonstandard_integer_type (actual_precision,
					      from_unsigned1);
    }
  if (!useless_type_conversion_p (type1, TREE_TYPE (mult_rhs1)))
    {
      if (TREE_CODE (mult_rhs1) == INTEGER_CST)
	mult_rhs1 = fold_convert (type1, mult_rhs1);
      else
	mult_rhs1 = build_and_insert_cast (gsi, loc, type1, mult_rhs1);
    }
  if (actual_precision != TYPE_PRECISION (type2)
      || from_unsigned2 != TYPE_UNSIGNED (type2))
    {
      if (!useless_type_conversion_p (type2, TREE_TYPE (mult_rhs2)))
	{
	  if (TREE_CODE (mult_rhs2) == INTEGER_CST)
	    mult_rhs2 = fold_convert (type2, mult_rhs2);
	  else
	    mult_rhs2 = build_and_insert_cast (gsi, loc, type2, mult_rhs2);
	}
      type2 = build_nonstandard_integer_type (actual_precision,
					      from_unsigned2);
    }
  if (!useless_type_conversion_p (type2, TREE_TYPE (mult_rhs2)))
    {
      if (TREE_CODE (mult_rhs2) == INTEGER_CST)
	mult_rhs2 = fold_convert (type2, mult_rhs2);
      else
	mult_rhs2 = build_and_insert_cast (gsi, loc, type2, mult_rhs2);
    }

  if (!useless_type_conversion_p (type, TREE_TYPE (add_rhs)))
    add_rhs = build_and_insert_cast (gsi, loc, type, add_rhs);

  gimple_assign_set_rhs_with_ops (gsi, wmult_code, mult_rhs1, mult_rhs2,
				  add_rhs);
  update_stmt (gsi_stmt (*gsi));
  widen_mul_stats.maccs_inserted++;
  return true;
}

/* Given a result MUL_RESULT which is a result of a multiplication of OP1 and
   OP2 and which we know is used in statements that can be, together with the
   multiplication, converted to FMAs, perform the transformation.  */

static void
convert_mult_to_fma_1 (tree mul_result, tree op1, tree op2)
{
  tree type = TREE_TYPE (mul_result);
  gimple *use_stmt;
  imm_use_iterator imm_iter;
  gcall *fma_stmt;

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, mul_result)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
      tree addop, mulop1 = op1, result = mul_result;
      bool negate_p = false;
      gimple_seq seq = NULL;

      if (is_gimple_debug (use_stmt))
	continue;

      if (is_gimple_assign (use_stmt)
	  && gimple_assign_rhs_code (use_stmt) == NEGATE_EXPR)
	{
	  result = gimple_assign_lhs (use_stmt);
	  use_operand_p use_p;
	  gimple *neguse_stmt;
	  single_imm_use (gimple_assign_lhs (use_stmt), &use_p, &neguse_stmt);
	  gsi_remove (&gsi, true);
	  release_defs (use_stmt);

	  use_stmt = neguse_stmt;
	  gsi = gsi_for_stmt (use_stmt);
	  negate_p = true;
	}

      tree cond, else_value, ops[3], len, bias;
      tree_code code;
      if (!can_interpret_as_conditional_op_p (use_stmt, &cond, &code,
					      ops, &else_value,
					      &len, &bias))
	gcc_unreachable ();
      addop = ops[0] == result ? ops[1] : ops[0];

      if (code == MINUS_EXPR)
	{
	  if (ops[0] == result)
	    /* a * b - c -> a * b + (-c)  */
	    addop = gimple_build (&seq, NEGATE_EXPR, type, addop);
	  else
	    /* a - b * c -> (-b) * c + a */
	    negate_p = !negate_p;
	}

      if (negate_p)
	mulop1 = gimple_build (&seq, NEGATE_EXPR, type, mulop1);

      if (seq)
	gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);

      if (len)
	fma_stmt
	  = gimple_build_call_internal (IFN_COND_LEN_FMA, 7, cond, mulop1, op2,
					addop, else_value, len, bias);
      else if (cond)
	fma_stmt = gimple_build_call_internal (IFN_COND_FMA, 5, cond, mulop1,
					       op2, addop, else_value);
      else
	fma_stmt = gimple_build_call_internal (IFN_FMA, 3, mulop1, op2, addop);
      gimple_set_lhs (fma_stmt, gimple_get_lhs (use_stmt));
      gimple_call_set_nothrow (fma_stmt, !stmt_can_throw_internal (cfun,
								   use_stmt));
      gsi_replace (&gsi, fma_stmt, true);
      /* Follow all SSA edges so that we generate FMS, FNMA and FNMS
	 regardless of where the negation occurs.  */
      gimple *orig_stmt = gsi_stmt (gsi);
      if (fold_stmt (&gsi, follow_all_ssa_edges))
	{
	  if (maybe_clean_or_replace_eh_stmt (orig_stmt, gsi_stmt (gsi)))
	    gcc_unreachable ();
	  update_stmt (gsi_stmt (gsi));
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Generated FMA ");
	  print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, TDF_NONE);
	  fprintf (dump_file, "\n");
	}

      /* If the FMA result is negated in a single use, fold the negation
	 too.  */
      orig_stmt = gsi_stmt (gsi);
      use_operand_p use_p;
      gimple *neg_stmt;
      if (is_gimple_call (orig_stmt)
	  && gimple_call_internal_p (orig_stmt)
	  && gimple_call_lhs (orig_stmt)
	  && TREE_CODE (gimple_call_lhs (orig_stmt)) == SSA_NAME
	  && single_imm_use (gimple_call_lhs (orig_stmt), &use_p, &neg_stmt)
	  && is_gimple_assign (neg_stmt)
	  && gimple_assign_rhs_code (neg_stmt) == NEGATE_EXPR
	  && !stmt_could_throw_p (cfun, neg_stmt))
	{
	  gsi = gsi_for_stmt (neg_stmt);
	  if (fold_stmt (&gsi, follow_all_ssa_edges))
	    {
	      if (maybe_clean_or_replace_eh_stmt (neg_stmt, gsi_stmt (gsi)))
		gcc_unreachable ();
	      update_stmt (gsi_stmt (gsi));
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Folded FMA negation ");
		  print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, TDF_NONE);
		  fprintf (dump_file, "\n");
		}
	    }
	}

      widen_mul_stats.fmas_inserted++;
    }
}

/* Data necessary to perform the actual transformation from a multiplication
   and an addition to an FMA after decision is taken it should be done and to
   then delete the multiplication statement from the function IL.  */

struct fma_transformation_info
{
  gimple *mul_stmt;
  tree mul_result;
  tree op1;
  tree op2;
};

/* Structure containing the current state of FMA deferring, i.e. whether we are
   deferring, whether to continue deferring, and all data necessary to come
   back and perform all deferred transformations.  */

class fma_deferring_state
{
public:
  /* Class constructor.  Pass true as PERFORM_DEFERRING in order to actually
     do any deferring.  */

  fma_deferring_state (bool perform_deferring)
    : m_candidates (), m_mul_result_set (), m_initial_phi (NULL),
      m_last_result (NULL_TREE), m_deferring_p (perform_deferring) {}

  /* List of FMA candidates for which we the transformation has been determined
     possible but we at this point in BB analysis we do not consider them
     beneficial.  */
  auto_vec<fma_transformation_info, 8> m_candidates;

  /* Set of results of multiplication that are part of an already deferred FMA
     candidates.  */
  hash_set<tree> m_mul_result_set;

  /* The PHI that supposedly feeds back result of a FMA to another over loop
     boundary.  */
  gphi *m_initial_phi;

  /* Result of the last produced FMA candidate or NULL if there has not been
     one.  */
  tree m_last_result;

  /* If true, deferring might still be profitable.  If false, transform all
     candidates and no longer defer.  */
  bool m_deferring_p;
};

/* Transform all deferred FMA candidates and mark STATE as no longer
   deferring.  */

static void
cancel_fma_deferring (fma_deferring_state *state)
{
  if (!state->m_deferring_p)
    return;

  for (unsigned i = 0; i < state->m_candidates.length (); i++)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Generating deferred FMA\n");

      const fma_transformation_info &fti = state->m_candidates[i];
      convert_mult_to_fma_1 (fti.mul_result, fti.op1, fti.op2);

      gimple_stmt_iterator gsi = gsi_for_stmt (fti.mul_stmt);
      gsi_remove (&gsi, true);
      release_defs (fti.mul_stmt);
    }
  state->m_deferring_p = false;
}

/* If OP is an SSA name defined by a PHI node, return the PHI statement.
   Otherwise return NULL.  */

static gphi *
result_of_phi (tree op)
{
  if (TREE_CODE (op) != SSA_NAME)
    return NULL;

  return dyn_cast <gphi *> (SSA_NAME_DEF_STMT (op));
}

/* After processing statements of a BB and recording STATE, return true if the
   initial phi is fed by the last FMA candidate result ore one such result from
   previously processed BBs marked in LAST_RESULT_SET.  */

static bool
last_fma_candidate_feeds_initial_phi (fma_deferring_state *state,
				      hash_set<tree> *last_result_set)
{
  ssa_op_iter iter;
  use_operand_p use;
  FOR_EACH_PHI_ARG (use, state->m_initial_phi, iter, SSA_OP_USE)
    {
      tree t = USE_FROM_PTR (use);
      if (t == state->m_last_result
	  || last_result_set->contains (t))
	return true;
    }

  return false;
}

/* Combine the multiplication at MUL_STMT with operands MULOP1 and MULOP2
   with uses in additions and subtractions to form fused multiply-add
   operations.  Returns true if successful and MUL_STMT should be removed.
   If MUL_COND is nonnull, the multiplication in MUL_STMT is conditional
   on MUL_COND, otherwise it is unconditional.

   If STATE indicates that we are deferring FMA transformation, that means
   that we do not produce FMAs for basic blocks which look like:

    <bb 6>
    # accumulator_111 = PHI <0.0(5), accumulator_66(6)>
    _65 = _14 * _16;
    accumulator_66 = _65 + accumulator_111;

  or its unrolled version, i.e. with several FMA candidates that feed result
  of one into the addend of another.  Instead, we add them to a list in STATE
  and if we later discover an FMA candidate that is not part of such a chain,
  we go back and perform all deferred past candidates.  */

static bool
convert_mult_to_fma (gimple *mul_stmt, tree op1, tree op2,
		     fma_deferring_state *state, tree mul_cond = NULL_TREE,
		     tree mul_len = NULL_TREE, tree mul_bias = NULL_TREE)
{
  tree mul_result = gimple_get_lhs (mul_stmt);
  /* If there isn't a LHS then this can't be an FMA.  There can be no LHS
     if the statement was left just for the side-effects.  */
  if (!mul_result)
    return false;
  tree type = TREE_TYPE (mul_result);
  gimple *use_stmt, *neguse_stmt;
  use_operand_p use_p;
  imm_use_iterator imm_iter;

  if (FLOAT_TYPE_P (type)
      && flag_fp_contract_mode != FP_CONTRACT_FAST)
    return false;

  /* We don't want to do bitfield reduction ops.  */
  if (INTEGRAL_TYPE_P (type)
      && (!type_has_mode_precision_p (type) || TYPE_OVERFLOW_TRAPS (type)))
    return false;

  /* If the target doesn't support it, don't generate it.  We assume that
     if fma isn't available then fms, fnma or fnms are not either.  */
  optimization_type opt_type = bb_optimization_type (gimple_bb (mul_stmt));
  if (!direct_internal_fn_supported_p (IFN_FMA, type, opt_type))
    return false;

  /* If the multiplication has zero uses, it is kept around probably because
     of -fnon-call-exceptions.  Don't optimize it away in that case,
     it is DCE job.  */
  if (has_zero_uses (mul_result))
    return false;

  bool check_defer
    = (state->m_deferring_p
       && maybe_le (tree_to_poly_int64 (TYPE_SIZE (type)),
		    param_avoid_fma_max_bits));
  bool defer = check_defer;
  bool seen_negate_p = false;

  /* There is no numerical difference between fused and unfused integer FMAs,
     and the assumption below that FMA is as cheap as addition is unlikely
     to be true, especially if the multiplication occurs multiple times on
     the same chain.  E.g., for something like:

	 (((a * b) + c) >> 1) + (a * b)

     we do not want to duplicate the a * b into two additions, not least
     because the result is not a natural FMA chain.  */
  if (ANY_INTEGRAL_TYPE_P (type)
      && !has_single_use (mul_result))
    return false;

  if (!dbg_cnt (form_fma))
    return false;

  /* Make sure that the multiplication statement becomes dead after
     the transformation, thus that all uses are transformed to FMAs.
     This means we assume that an FMA operation has the same cost
     as an addition.  */
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, mul_result)
    {
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

      /* A negate on the multiplication leads to FNMA.  */
      if (is_gimple_assign (use_stmt)
	  && gimple_assign_rhs_code (use_stmt) == NEGATE_EXPR)
	{
	  ssa_op_iter iter;
	  use_operand_p usep;

	  /* If (due to earlier missed optimizations) we have two
	     negates of the same value, treat them as equivalent
	     to a single negate with multiple uses.  */
	  if (seen_negate_p)
	    return false;

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

	  negate_p = seen_negate_p = true;
	}

      tree cond, else_value, ops[3], len, bias;
      tree_code code;
      if (!can_interpret_as_conditional_op_p (use_stmt, &cond, &code, ops,
					      &else_value, &len, &bias))
	return false;

      switch (code)
	{
	case MINUS_EXPR:
	  if (ops[1] == result)
	    negate_p = !negate_p;
	  break;
	case PLUS_EXPR:
	  break;
	default:
	  /* FMA can only be formed from PLUS and MINUS.  */
	  return false;
	}

      if (len)
	{
	  /* For COND_LEN_* operations, we may have dummpy mask which is
	     the all true mask.  Such TREE type may be mul_cond != cond
	     but we still consider they are equal.  */
	  if (mul_cond && cond != mul_cond
	      && !(integer_truep (mul_cond) && integer_truep (cond)))
	    return false;

	  if (else_value == result)
	    return false;

	  if (!direct_internal_fn_supported_p (IFN_COND_LEN_FMA, type,
					       opt_type))
	    return false;

	  if (mul_len)
	    {
	      poly_int64 mul_value, value;
	      if (poly_int_tree_p (mul_len, &mul_value)
		  && poly_int_tree_p (len, &value)
		  && maybe_ne (mul_value, value))
		return false;
	      else if (mul_len != len)
		return false;

	      if (wi::to_widest (mul_bias) != wi::to_widest (bias))
		return false;
	    }
	}
      else
	{
	  if (mul_cond && cond != mul_cond)
	    return false;

	  if (cond)
	    {
	      if (cond == result || else_value == result)
		return false;
	      if (!direct_internal_fn_supported_p (IFN_COND_FMA, type,
						   opt_type))
		return false;
	    }
	}

      /* If the subtrahend (OPS[1]) is computed by a MULT_EXPR that
	 we'll visit later, we might be able to get a more profitable
	 match with fnma.
	 OTOH, if we don't, a negate / fma pair has likely lower latency
	 that a mult / subtract pair.  */
      if (code == MINUS_EXPR
	  && !negate_p
	  && ops[0] == result
	  && !direct_internal_fn_supported_p (IFN_FMS, type, opt_type)
	  && direct_internal_fn_supported_p (IFN_FNMA, type, opt_type)
	  && TREE_CODE (ops[1]) == SSA_NAME
	  && has_single_use (ops[1]))
	{
	  gimple *stmt2 = SSA_NAME_DEF_STMT (ops[1]);
	  if (is_gimple_assign (stmt2)
	      && gimple_assign_rhs_code (stmt2) == MULT_EXPR)
	    return false;
	}

      /* We can't handle a * b + a * b.  */
      if (ops[0] == ops[1])
	return false;
      /* If deferring, make sure we are not looking at an instruction that
	 wouldn't have existed if we were not.  */
      if (state->m_deferring_p
	  && (state->m_mul_result_set.contains (ops[0])
	      || state->m_mul_result_set.contains (ops[1])))
	return false;

      if (check_defer)
	{
	  tree use_lhs = gimple_get_lhs (use_stmt);
	  if (state->m_last_result)
	    {
	      if (ops[1] == state->m_last_result
		  || ops[0] == state->m_last_result)
		defer = true;
	      else
		defer = false;
	    }
	  else
	    {
	      gcc_checking_assert (!state->m_initial_phi);
	      gphi *phi;
	      if (ops[0] == result)
		phi = result_of_phi (ops[1]);
	      else
		{
		  gcc_assert (ops[1] == result);
		  phi = result_of_phi (ops[0]);
		}

	      if (phi)
		{
		  state->m_initial_phi = phi;
		  defer = true;
		}
	      else
		defer = false;
	    }

	  state->m_last_result = use_lhs;
	  check_defer = false;
	}
      else
	defer = false;

      /* While it is possible to validate whether or not the exact form that
	 we've recognized is available in the backend, the assumption is that
	 if the deferring logic above did not trigger, the transformation is
	 never a loss.  For instance, suppose the target only has the plain FMA
	 pattern available.  Consider a*b-c -> fma(a,b,-c): we've exchanged
	 MUL+SUB for FMA+NEG, which is still two operations.  Consider
         -(a*b)-c -> fma(-a,b,-c): we still have 3 operations, but in the FMA
	 form the two NEGs are independent and could be run in parallel.  */
    }

  if (defer)
    {
      fma_transformation_info fti;
      fti.mul_stmt = mul_stmt;
      fti.mul_result = mul_result;
      fti.op1 = op1;
      fti.op2 = op2;
      state->m_candidates.safe_push (fti);
      state->m_mul_result_set.add (mul_result);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Deferred generating FMA for multiplication ");
	  print_gimple_stmt (dump_file, mul_stmt, 0, TDF_NONE);
	  fprintf (dump_file, "\n");
	}

      return false;
    }
  else
    {
      if (state->m_deferring_p)
	cancel_fma_deferring (state);
      convert_mult_to_fma_1 (mul_result, op1, op2);
      return true;
    }
}


/* Helper function of match_arith_overflow.  For MUL_OVERFLOW, if we have
   a check for non-zero like:
   _1 = x_4(D) * y_5(D);
   *res_7(D) = _1;
   if (x_4(D) != 0)
     goto <bb 3>; [50.00%]
   else
     goto <bb 4>; [50.00%]

   <bb 3> [local count: 536870913]:
   _2 = _1 / x_4(D);
   _9 = _2 != y_5(D);
   _10 = (int) _9;

   <bb 4> [local count: 1073741824]:
   # iftmp.0_3 = PHI <_10(3), 0(2)>
   then in addition to using .MUL_OVERFLOW (x_4(D), y_5(D)) we can also
   optimize the x_4(D) != 0 condition to 1.  */

static void
maybe_optimize_guarding_check (vec<gimple *> &mul_stmts, gimple *cond_stmt,
			       gimple *div_stmt, bool *cfg_changed)
{
  basic_block bb = gimple_bb (cond_stmt);
  if (gimple_bb (div_stmt) != bb || !single_pred_p (bb))
    return;
  edge pred_edge = single_pred_edge (bb);
  basic_block pred_bb = pred_edge->src;
  if (EDGE_COUNT (pred_bb->succs) != 2)
    return;
  edge other_edge = EDGE_SUCC (pred_bb, EDGE_SUCC (pred_bb, 0) == pred_edge);
  edge other_succ_edge = NULL;
  if (gimple_code (cond_stmt) == GIMPLE_COND)
    {
      if (EDGE_COUNT (bb->succs) != 2)
	return;
      other_succ_edge = EDGE_SUCC (bb, 0);
      if (gimple_cond_code (cond_stmt) == NE_EXPR)
	{
	  if (other_succ_edge->flags & EDGE_TRUE_VALUE)
	    other_succ_edge = EDGE_SUCC (bb, 1);
	}
      else if (other_succ_edge->flags & EDGE_FALSE_VALUE)
	other_succ_edge = EDGE_SUCC (bb, 0);
      if (other_edge->dest != other_succ_edge->dest)
	return;
    }
  else if (!single_succ_p (bb) || other_edge->dest != single_succ (bb))
    return;
  gcond *zero_cond = safe_dyn_cast <gcond *> (*gsi_last_bb (pred_bb));
  if (zero_cond == NULL
      || (gimple_cond_code (zero_cond)
	  != ((pred_edge->flags & EDGE_TRUE_VALUE) ? NE_EXPR : EQ_EXPR))
      || !integer_zerop (gimple_cond_rhs (zero_cond)))
    return;
  tree zero_cond_lhs = gimple_cond_lhs (zero_cond);
  if (TREE_CODE (zero_cond_lhs) != SSA_NAME)
    return;
  if (gimple_assign_rhs2 (div_stmt) != zero_cond_lhs)
    {
      /* Allow the divisor to be result of a same precision cast
	 from zero_cond_lhs.  */
      tree rhs2 = gimple_assign_rhs2 (div_stmt);
      if (TREE_CODE (rhs2) != SSA_NAME)
	return;
      gimple *g = SSA_NAME_DEF_STMT (rhs2);
      if (!gimple_assign_cast_p (g)
	  || gimple_assign_rhs1 (g) != gimple_cond_lhs (zero_cond)
	  || !INTEGRAL_TYPE_P (TREE_TYPE (zero_cond_lhs))
	  || (TYPE_PRECISION (TREE_TYPE (zero_cond_lhs))
	      != TYPE_PRECISION (TREE_TYPE (rhs2))))
	return;
    }
  gimple_stmt_iterator gsi = gsi_after_labels (bb);
  mul_stmts.quick_push (div_stmt);
  if (is_gimple_debug (gsi_stmt (gsi)))
    gsi_next_nondebug (&gsi);
  unsigned cast_count = 0;
  while (gsi_stmt (gsi) != cond_stmt)
    {
      /* If original mul_stmt has a single use, allow it in the same bb,
	 we are looking then just at __builtin_mul_overflow_p.
	 Though, in that case the original mul_stmt will be replaced
	 by .MUL_OVERFLOW, REALPART_EXPR and IMAGPART_EXPR stmts.  */
      gimple *mul_stmt;
      unsigned int i;
      bool ok = false;
      FOR_EACH_VEC_ELT (mul_stmts, i, mul_stmt)
	{
	  if (gsi_stmt (gsi) == mul_stmt)
	    {
	      ok = true;
	      break;
	    }
	}
      if (!ok && gimple_assign_cast_p (gsi_stmt (gsi)) && ++cast_count < 4)
	ok = true;
      if (!ok)
	return;
      gsi_next_nondebug (&gsi);
    }
  if (gimple_code (cond_stmt) == GIMPLE_COND)
    {
      basic_block succ_bb = other_edge->dest;
      for (gphi_iterator gpi = gsi_start_phis (succ_bb); !gsi_end_p (gpi);
	   gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  tree v1 = gimple_phi_arg_def (phi, other_edge->dest_idx);
	  tree v2 = gimple_phi_arg_def (phi, other_succ_edge->dest_idx);
	  if (!operand_equal_p (v1, v2, 0))
	    return;
	}
    }
  else
    {
      tree lhs = gimple_assign_lhs (cond_stmt);
      if (!lhs || !INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
	return;
      gsi_next_nondebug (&gsi);
      if (!gsi_end_p (gsi))
	{
	  if (gimple_assign_rhs_code (cond_stmt) == COND_EXPR)
	    return;
	  gimple *cast_stmt = gsi_stmt (gsi);
	  if (!gimple_assign_cast_p (cast_stmt))
	    return;
	  tree new_lhs = gimple_assign_lhs (cast_stmt);
	  gsi_next_nondebug (&gsi);
	  if (!gsi_end_p (gsi)
	      || !new_lhs
	      || !INTEGRAL_TYPE_P (TREE_TYPE (new_lhs))
	      || TYPE_PRECISION (TREE_TYPE (new_lhs)) <= 1)
	    return;
	  lhs = new_lhs;
	}
      edge succ_edge = single_succ_edge (bb);
      basic_block succ_bb = succ_edge->dest;
      gsi = gsi_start_phis (succ_bb);
      if (gsi_end_p (gsi))
	return;
      gphi *phi = as_a <gphi *> (gsi_stmt (gsi));
      gsi_next (&gsi);
      if (!gsi_end_p (gsi))
	return;
      if (gimple_phi_arg_def (phi, succ_edge->dest_idx) != lhs)
	return;
      tree other_val = gimple_phi_arg_def (phi, other_edge->dest_idx);
      if (gimple_assign_rhs_code (cond_stmt) == COND_EXPR)
	{
	  tree cond = gimple_assign_rhs1 (cond_stmt);
	  if (TREE_CODE (cond) == NE_EXPR)
	    {
	      if (!operand_equal_p (other_val,
				    gimple_assign_rhs3 (cond_stmt), 0))
		return;
	    }
	  else if (!operand_equal_p (other_val,
				     gimple_assign_rhs2 (cond_stmt), 0))
	    return;
	}
      else if (gimple_assign_rhs_code (cond_stmt) == NE_EXPR)
	{
	  if (!integer_zerop (other_val))
	    return;
	}
      else if (!integer_onep (other_val))
	return;
    }
  if (pred_edge->flags & EDGE_TRUE_VALUE)
    gimple_cond_make_true (zero_cond);
  else
    gimple_cond_make_false (zero_cond);
  update_stmt (zero_cond);
  *cfg_changed = true;
}

/* Helper function for arith_overflow_check_p.  Return true
   if VAL1 is equal to VAL2 cast to corresponding integral type
   with other signedness or vice versa.  */

static bool
arith_cast_equal_p (tree val1, tree val2)
{
  if (TREE_CODE (val1) == INTEGER_CST && TREE_CODE (val2) == INTEGER_CST)
    return wi::eq_p (wi::to_wide (val1), wi::to_wide (val2));
  else if (TREE_CODE (val1) != SSA_NAME || TREE_CODE (val2) != SSA_NAME)
    return false;
  if (gimple_assign_cast_p (SSA_NAME_DEF_STMT (val1))
      && gimple_assign_rhs1 (SSA_NAME_DEF_STMT (val1)) == val2)
    return true;
  if (gimple_assign_cast_p (SSA_NAME_DEF_STMT (val2))
      && gimple_assign_rhs1 (SSA_NAME_DEF_STMT (val2)) == val1)
    return true;
  return false;
}

/* Helper function of match_arith_overflow.  Return 1
   if USE_STMT is unsigned overflow check ovf != 0 for
   STMT, -1 if USE_STMT is unsigned overflow check ovf == 0
   and 0 otherwise.  */

static int
arith_overflow_check_p (gimple *stmt, gimple *cast_stmt, gimple *&use_stmt,
			tree maxval, tree *other)
{
  enum tree_code ccode = ERROR_MARK;
  tree crhs1 = NULL_TREE, crhs2 = NULL_TREE;
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (cast_stmt ? cast_stmt : stmt);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree multop = NULL_TREE, divlhs = NULL_TREE;
  gimple *cur_use_stmt = use_stmt;

  if (code == MULT_EXPR)
    {
      if (!is_gimple_assign (use_stmt))
	return 0;
      if (gimple_assign_rhs_code (use_stmt) != TRUNC_DIV_EXPR)
	return 0;
      if (gimple_assign_rhs1 (use_stmt) != lhs)
	return 0;
      if (cast_stmt)
	{
	  if (arith_cast_equal_p (gimple_assign_rhs2 (use_stmt), rhs1))
	    multop = rhs2;
	  else if (arith_cast_equal_p (gimple_assign_rhs2 (use_stmt), rhs2))
	    multop = rhs1;
	  else
	    return 0;
	}
      else if (gimple_assign_rhs2 (use_stmt) == rhs1)
	multop = rhs2;
      else if (operand_equal_p (gimple_assign_rhs2 (use_stmt), rhs2, 0))
	multop = rhs1;
      else
	return 0;
      if (stmt_ends_bb_p (use_stmt))
	return 0;
      divlhs = gimple_assign_lhs (use_stmt);
      if (!divlhs)
	return 0;
      use_operand_p use;
      if (!single_imm_use (divlhs, &use, &cur_use_stmt))
	return 0;
      if (cast_stmt && gimple_assign_cast_p (cur_use_stmt))
	{
	  tree cast_lhs = gimple_assign_lhs (cur_use_stmt);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (cast_lhs))
	      && TYPE_UNSIGNED (TREE_TYPE (cast_lhs))
	      && (TYPE_PRECISION (TREE_TYPE (cast_lhs))
		  == TYPE_PRECISION (TREE_TYPE (divlhs)))
	      && single_imm_use (cast_lhs, &use, &cur_use_stmt))
	    {
	      cast_stmt = NULL;
	      divlhs = cast_lhs;
	    }
	  else
	    return 0;
	}
    }
  if (gimple_code (cur_use_stmt) == GIMPLE_COND)
    {
      ccode = gimple_cond_code (cur_use_stmt);
      crhs1 = gimple_cond_lhs (cur_use_stmt);
      crhs2 = gimple_cond_rhs (cur_use_stmt);
    }
  else if (is_gimple_assign (cur_use_stmt))
    {
      if (gimple_assign_rhs_class (cur_use_stmt) == GIMPLE_BINARY_RHS)
	{
	  ccode = gimple_assign_rhs_code (cur_use_stmt);
	  crhs1 = gimple_assign_rhs1 (cur_use_stmt);
	  crhs2 = gimple_assign_rhs2 (cur_use_stmt);
	}
      else if (gimple_assign_rhs_code (cur_use_stmt) == COND_EXPR)
	{
	  tree cond = gimple_assign_rhs1 (cur_use_stmt);
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

  if (maxval
      && ccode == RSHIFT_EXPR
      && crhs1 == lhs
      && TREE_CODE (crhs2) == INTEGER_CST
      && wi::to_widest (crhs2) == TYPE_PRECISION (TREE_TYPE (maxval)))
    {
      tree shiftlhs = gimple_assign_lhs (use_stmt);
      if (!shiftlhs)
	return 0;
      use_operand_p use;
      if (!single_imm_use (shiftlhs, &use, &cur_use_stmt))
	return 0;
      if (gimple_code (cur_use_stmt) == GIMPLE_COND)
	{
	  ccode = gimple_cond_code (cur_use_stmt);
	  crhs1 = gimple_cond_lhs (cur_use_stmt);
	  crhs2 = gimple_cond_rhs (cur_use_stmt);
	}
      else if (is_gimple_assign (cur_use_stmt))
	{
	  if (gimple_assign_rhs_class (cur_use_stmt) == GIMPLE_BINARY_RHS)
	    {
	      ccode = gimple_assign_rhs_code (cur_use_stmt);
	      crhs1 = gimple_assign_rhs1 (cur_use_stmt);
	      crhs2 = gimple_assign_rhs2 (cur_use_stmt);
	    }
	  else if (gimple_assign_rhs_code (cur_use_stmt) == COND_EXPR)
	    {
	      tree cond = gimple_assign_rhs1 (cur_use_stmt);
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
	    {
	      enum tree_code sc = gimple_assign_rhs_code (cur_use_stmt);
	      tree castlhs = gimple_assign_lhs (cur_use_stmt);
	      if (!CONVERT_EXPR_CODE_P (sc)
		  || !castlhs
		  || !INTEGRAL_TYPE_P (TREE_TYPE (castlhs))
		  || (TYPE_PRECISION (TREE_TYPE (castlhs))
		      > TYPE_PRECISION (TREE_TYPE (maxval))))
		return 0;
	      return 1;
	    }
	}
      else
	return 0;
      if ((ccode != EQ_EXPR && ccode != NE_EXPR)
	  || crhs1 != shiftlhs
	  || !integer_zerop (crhs2))
	return 0;
      return 1;
    }

  if (TREE_CODE_CLASS (ccode) != tcc_comparison)
    return 0;

  switch (ccode)
    {
    case GT_EXPR:
    case LE_EXPR:
      if (maxval)
	{
	  /* r = a + b; r > maxval or r <= maxval  */
	  if (crhs1 == lhs
	      && TREE_CODE (crhs2) == INTEGER_CST
	      && tree_int_cst_equal (crhs2, maxval))
	    return ccode == GT_EXPR ? 1 : -1;
	  break;
	}
      /* r = a - b; r > a or r <= a
	 r = a + b; a > r or a <= r or b > r or b <= r.  */
      if ((code == MINUS_EXPR && crhs1 == lhs && crhs2 == rhs1)
	  || (code == PLUS_EXPR && (crhs1 == rhs1 || crhs1 == rhs2)
	      && crhs2 == lhs))
	return ccode == GT_EXPR ? 1 : -1;
      /* r = ~a; b > r or b <= r.  */
      if (code == BIT_NOT_EXPR && crhs2 == lhs)
	{
	  if (other)
	    *other = crhs1;
	  return ccode == GT_EXPR ? 1 : -1;
	}
      break;
    case LT_EXPR:
    case GE_EXPR:
      if (maxval)
	break;
      /* r = a - b; a < r or a >= r
	 r = a + b; r < a or r >= a or r < b or r >= b.  */
      if ((code == MINUS_EXPR && crhs1 == rhs1 && crhs2 == lhs)
	  || (code == PLUS_EXPR && crhs1 == lhs
	      && (crhs2 == rhs1 || crhs2 == rhs2)))
	return ccode == LT_EXPR ? 1 : -1;
      /* r = ~a; r < b or r >= b.  */
      if (code == BIT_NOT_EXPR && crhs1 == lhs)
	{
	  if (other)
	    *other = crhs2;
	  return ccode == LT_EXPR ? 1 : -1;
	}
      break;
    case EQ_EXPR:
    case NE_EXPR:
      /* r = a * b; _1 = r / a; _1 == b
	 r = a * b; _1 = r / b; _1 == a
	 r = a * b; _1 = r / a; _1 != b
	 r = a * b; _1 = r / b; _1 != a.  */
      if (code == MULT_EXPR)
	{
	  if (cast_stmt)
	    {
	      if ((crhs1 == divlhs && arith_cast_equal_p (crhs2, multop))
		  || (crhs2 == divlhs && arith_cast_equal_p (crhs1, multop)))
		{
		  use_stmt = cur_use_stmt;
		  return ccode == NE_EXPR ? 1 : -1;
		}
	    }
	  else if ((crhs1 == divlhs && operand_equal_p (crhs2, multop, 0))
		   || (crhs2 == divlhs && crhs1 == multop))
	    {
	      use_stmt = cur_use_stmt;
	      return ccode == NE_EXPR ? 1 : -1;
	    }
	}
      break;
    default:
      break;
    }
  return 0;
}

extern bool gimple_unsigned_integer_sat_add (tree, tree*, tree (*)(tree));
extern bool gimple_unsigned_integer_sat_sub (tree, tree*, tree (*)(tree));
extern bool gimple_unsigned_integer_sat_trunc (tree, tree*, tree (*)(tree));

extern bool gimple_signed_integer_sat_add (tree, tree*, tree (*)(tree));
extern bool gimple_signed_integer_sat_sub (tree, tree*, tree (*)(tree));
extern bool gimple_signed_integer_sat_trunc (tree, tree*, tree (*)(tree));

static void
build_saturation_binary_arith_call_and_replace (gimple_stmt_iterator *gsi,
						internal_fn fn, tree lhs,
						tree op_0, tree op_1)
{
  if (direct_internal_fn_supported_p (fn, TREE_TYPE (lhs), OPTIMIZE_FOR_BOTH))
    {
      gcall *call = gimple_build_call_internal (fn, 2, op_0, op_1);
      gimple_call_set_lhs (call, lhs);
      gsi_replace (gsi, call, /* update_eh_info */ true);
    }
}

static bool
build_saturation_binary_arith_call_and_insert (gimple_stmt_iterator *gsi,
					       internal_fn fn, tree lhs,
					       tree op_0, tree op_1)
{
  if (!direct_internal_fn_supported_p (fn, TREE_TYPE (op_0), OPTIMIZE_FOR_BOTH))
    return false;

  gcall *call = gimple_build_call_internal (fn, 2, op_0, op_1);
  gimple_call_set_lhs (call, lhs);
  gsi_insert_before (gsi, call, GSI_SAME_STMT);

  return true;
}

/*
 * Try to match saturation unsigned add with assign.
 *   _7 = _4 + _6;
 *   _8 = _4 > _7;
 *   _9 = (long unsigned int) _8;
 *   _10 = -_9;
 *   _12 = _7 | _10;
 *   =>
 *   _12 = .SAT_ADD (_4, _6);  */

static void
match_unsigned_saturation_add (gimple_stmt_iterator *gsi, gassign *stmt)
{
  tree ops[2];
  tree lhs = gimple_assign_lhs (stmt);

  if (gimple_unsigned_integer_sat_add (lhs, ops, NULL))
    build_saturation_binary_arith_call_and_replace (gsi, IFN_SAT_ADD, lhs,
						    ops[0], ops[1]);
}

/*
 * Try to match saturation add with PHI.
 * For unsigned integer:
 *   <bb 2> :
 *   _1 = x_3(D) + y_4(D);
 *   if (_1 >= x_3(D))
 *     goto <bb 3>; [INV]
 *   else
 *     goto <bb 4>; [INV]
 *
 *   <bb 3> :
 *
 *   <bb 4> :
 *   # _2 = PHI <255(2), _1(3)>
 *   =>
 *   <bb 4> [local count: 1073741824]:
 *   _2 = .SAT_ADD (x_4(D), y_5(D));
 *
 * For signed integer:
 *   x.0_1 = (long unsigned int) x_7(D);
 *   y.1_2 = (long unsigned int) y_8(D);
 *   _3 = x.0_1 + y.1_2;
 *   sum_9 = (int64_t) _3;
 *   _4 = x_7(D) ^ y_8(D);
 *   _5 = x_7(D) ^ sum_9;
 *   _15 = ~_4;
 *   _16 = _5 & _15;
 *   if (_16 < 0)
 *     goto <bb 3>; [41.00%]
 *   else
 *     goto <bb 4>; [59.00%]
 *   _11 = x_7(D) < 0;
 *   _12 = (long int) _11;
 *   _13 = -_12;
 *   _14 = _13 ^ 9223372036854775807;
 *   # _6 = PHI <_14(3), sum_9(2)>
 *   =>
 *   _6 = .SAT_ADD (x_5(D), y_6(D)); [tail call]  */

static bool
match_saturation_add (gimple_stmt_iterator *gsi, gphi *phi)
{
  if (gimple_phi_num_args (phi) != 2)
    return false;

  tree ops[2];
  tree phi_result = gimple_phi_result (phi);

  if (!gimple_unsigned_integer_sat_add (phi_result, ops, NULL)
      && !gimple_signed_integer_sat_add (phi_result, ops, NULL))
    return false;

  if (!TYPE_UNSIGNED (TREE_TYPE (ops[0])) && TREE_CODE (ops[1]) == INTEGER_CST)
    ops[1] = fold_convert (TREE_TYPE (ops[0]), ops[1]);

  return build_saturation_binary_arith_call_and_insert (gsi, IFN_SAT_ADD,
							phi_result, ops[0],
							ops[1]);
}

/*
 * Try to match saturation unsigned sub.
 *   _1 = _4 >= _5;
 *   _3 = _4 - _5;
 *   _6 = _1 ? _3 : 0;
 *   =>
 *   _6 = .SAT_SUB (_4, _5);  */

static void
match_unsigned_saturation_sub (gimple_stmt_iterator *gsi, gassign *stmt)
{
  tree ops[2];
  tree lhs = gimple_assign_lhs (stmt);

  if (gimple_unsigned_integer_sat_sub (lhs, ops, NULL))
    build_saturation_binary_arith_call_and_replace (gsi, IFN_SAT_SUB, lhs,
						    ops[0], ops[1]);
}

/*
 * Try to match saturation unsigned sub.
 *  <bb 2> [local count: 1073741824]:
 *  if (x_2(D) > y_3(D))
 *    goto <bb 3>; [50.00%]
 *  else
 *    goto <bb 4>; [50.00%]
 *
 *  <bb 3> [local count: 536870912]:
 *  _4 = x_2(D) - y_3(D);
 *
 *  <bb 4> [local count: 1073741824]:
 *  # _1 = PHI <0(2), _4(3)>
 *  =>
 *  <bb 4> [local count: 1073741824]:
 *  _1 = .SAT_SUB (x_2(D), y_3(D));  */
static bool
match_saturation_sub (gimple_stmt_iterator *gsi, gphi *phi)
{
  if (gimple_phi_num_args (phi) != 2)
    return false;

  tree ops[2];
  tree phi_result = gimple_phi_result (phi);

  if (!gimple_unsigned_integer_sat_sub (phi_result, ops, NULL)
      && !gimple_signed_integer_sat_sub (phi_result, ops, NULL))
    return false;

  return build_saturation_binary_arith_call_and_insert (gsi, IFN_SAT_SUB,
							phi_result, ops[0],
							ops[1]);
}

/*
 * Try to match saturation unsigned sub.
 * uint16_t x_4(D);
 * uint8_t _6;
 * overflow_5 = x_4(D) > 255;
 * _1 = (unsigned char) x_4(D);
 * _2 = (unsigned char) overflow_5;
 * _3 = -_2;
 * _6 = _1 | _3;
 * =>
 * _6 = .SAT_TRUNC (x_4(D));
 * */
static void
match_unsigned_saturation_trunc (gimple_stmt_iterator *gsi, gassign *stmt)
{
  tree ops[1];
  tree lhs = gimple_assign_lhs (stmt);
  tree type = TREE_TYPE (lhs);

  if (gimple_unsigned_integer_sat_trunc (lhs, ops, NULL)
    && direct_internal_fn_supported_p (IFN_SAT_TRUNC,
				       tree_pair (type, TREE_TYPE (ops[0])),
				       OPTIMIZE_FOR_BOTH))
    {
      gcall *call = gimple_build_call_internal (IFN_SAT_TRUNC, 1, ops[0]);
      gimple_call_set_lhs (call, lhs);
      gsi_replace (gsi, call, /* update_eh_info */ true);
    }
}

/*
 * Try to match saturation truncate.
 * Aka:
 *   x.0_1 = (unsigned long) x_4(D);
 *   _2 = x.0_1 + 2147483648;
 *   if (_2 > 4294967295)
 *     goto <bb 4>; [50.00%]
 *   else
 *     goto <bb 3>; [50.00%]
 * ;;    succ:       4
 * ;;                3
 *
 * ;;   basic block 3, loop depth 0
 * ;;    pred:       2
 *   trunc_5 = (int32_t) x_4(D);
 *   goto <bb 5>; [100.00%]
 * ;;    succ:       5
 *
 * ;;   basic block 4, loop depth 0
 * ;;    pred:       2
 *   _7 = x_4(D) < 0;
 *   _8 = (int) _7;
 *   _9 = -_8;
 *   _10 = _9 ^ 2147483647;
 * ;;    succ:       5
 *
 * ;;   basic block 5, loop depth 0
 * ;;    pred:       3
 * ;;                4
 *   # _3 = PHI <trunc_5(3), _10(4)>
 * =>
 * _6 = .SAT_TRUNC (x_4(D));
 */

static bool
match_saturation_trunc (gimple_stmt_iterator *gsi, gphi *phi)
{
  if (gimple_phi_num_args (phi) != 2)
    return false;

  tree ops[1];
  tree phi_result = gimple_phi_result (phi);
  tree type = TREE_TYPE (phi_result);

  if (!gimple_unsigned_integer_sat_trunc (phi_result, ops, NULL)
      && !gimple_signed_integer_sat_trunc (phi_result, ops, NULL))
    return false;

  if (!direct_internal_fn_supported_p (IFN_SAT_TRUNC,
				       tree_pair (type, TREE_TYPE (ops[0])),
				       OPTIMIZE_FOR_BOTH))
    return false;

  gcall *call = gimple_build_call_internal (IFN_SAT_TRUNC, 1, ops[0]);
  gimple_call_set_lhs (call, phi_result);
  gsi_insert_before (gsi, call, GSI_SAME_STMT);

  return true;
}

/* Recognize for unsigned x
   x = y - z;
   if (x > y)
   where there are other uses of x and replace it with
   _7 = .SUB_OVERFLOW (y, z);
   x = REALPART_EXPR <_7>;
   _8 = IMAGPART_EXPR <_7>;
   if (_8)
   and similarly for addition.

   Also recognize:
   yc = (type) y;
   zc = (type) z;
   x = yc + zc;
   if (x > max)
   where y and z have unsigned types with maximum max
   and there are other uses of x and all of those cast x
   back to that unsigned type and again replace it with
   _7 = .ADD_OVERFLOW (y, z);
   _9 = REALPART_EXPR <_7>;
   _8 = IMAGPART_EXPR <_7>;
   if (_8)
   and replace (utype) x with _9.
   Or with x >> popcount (max) instead of x > max.

   Also recognize:
   x = ~z;
   if (y > x)
   and replace it with
   _7 = .ADD_OVERFLOW (y, z);
   _8 = IMAGPART_EXPR <_7>;
   if (_8)

   And also recognize:
   z = x * y;
   if (x != 0)
     goto <bb 3>; [50.00%]
   else
     goto <bb 4>; [50.00%]

   <bb 3> [local count: 536870913]:
   _2 = z / x;
   _9 = _2 != y;
   _10 = (int) _9;

   <bb 4> [local count: 1073741824]:
   # iftmp.0_3 = PHI <_10(3), 0(2)>
   and replace it with
   _7 = .MUL_OVERFLOW (x, y);
   z = IMAGPART_EXPR <_7>;
   _8 = IMAGPART_EXPR <_7>;
   _9 = _8 != 0;
   iftmp.0_3 = (int) _9;  */

static bool
match_arith_overflow (gimple_stmt_iterator *gsi, gimple *stmt,
		      enum tree_code code, bool *cfg_changed)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree type = TREE_TYPE (lhs);
  use_operand_p use_p;
  imm_use_iterator iter;
  bool use_seen = false;
  bool ovf_use_seen = false;
  gimple *use_stmt;
  gimple *add_stmt = NULL;
  bool add_first = false;
  gimple *cond_stmt = NULL;
  gimple *cast_stmt = NULL;
  tree cast_lhs = NULL_TREE;

  gcc_checking_assert (code == PLUS_EXPR
		       || code == MINUS_EXPR
		       || code == MULT_EXPR
		       || code == BIT_NOT_EXPR);
  if (!INTEGRAL_TYPE_P (type)
      || !TYPE_UNSIGNED (type)
      || has_zero_uses (lhs)
      || (code != PLUS_EXPR
	  && code != MULT_EXPR
	  && optab_handler (code == MINUS_EXPR ? usubv4_optab : uaddv4_optab,
			    TYPE_MODE (type)) == CODE_FOR_nothing))
    return false;

  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  FOR_EACH_IMM_USE_FAST (use_p, iter, lhs)
    {
      use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;

      tree other = NULL_TREE;
      if (arith_overflow_check_p (stmt, NULL, use_stmt, NULL_TREE, &other))
	{
	  if (code == BIT_NOT_EXPR)
	    {
	      gcc_assert (other);
	      if (TREE_CODE (other) != SSA_NAME)
		return false;
	      if (rhs2 == NULL)
		rhs2 = other;
	      else
		return false;
	      cond_stmt = use_stmt;
	    }
	  ovf_use_seen = true;
	}
      else
	{
	  use_seen = true;
	  if (code == MULT_EXPR
	      && cast_stmt == NULL
	      && gimple_assign_cast_p (use_stmt))
	    {
	      cast_lhs = gimple_assign_lhs (use_stmt);
	      if (INTEGRAL_TYPE_P (TREE_TYPE (cast_lhs))
		  && !TYPE_UNSIGNED (TREE_TYPE (cast_lhs))
		  && (TYPE_PRECISION (TREE_TYPE (cast_lhs))
		      == TYPE_PRECISION (TREE_TYPE (lhs))))
		cast_stmt = use_stmt;
	      else
		cast_lhs = NULL_TREE;
	    }
	}
      if (ovf_use_seen && use_seen)
	break;
    }

  if (!ovf_use_seen
      && code == MULT_EXPR
      && cast_stmt)
    {
      if (TREE_CODE (rhs1) != SSA_NAME
	  || (TREE_CODE (rhs2) != SSA_NAME && TREE_CODE (rhs2) != INTEGER_CST))
	return false;
      FOR_EACH_IMM_USE_FAST (use_p, iter, cast_lhs)
	{
	  use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;

	  if (arith_overflow_check_p (stmt, cast_stmt, use_stmt,
				      NULL_TREE, NULL))
	    ovf_use_seen = true;
	}
    }
  else
    {
      cast_stmt = NULL;
      cast_lhs = NULL_TREE;
    }

  tree maxval = NULL_TREE;
  if (!ovf_use_seen
      || (code != MULT_EXPR && (code == BIT_NOT_EXPR ? use_seen : !use_seen))
      || (code == PLUS_EXPR
	  && optab_handler (uaddv4_optab,
			    TYPE_MODE (type)) == CODE_FOR_nothing)
      || (code == MULT_EXPR
	  && optab_handler (cast_stmt ? mulv4_optab : umulv4_optab,
			    TYPE_MODE (type)) == CODE_FOR_nothing
	  && (use_seen
	      || cast_stmt
	      || !can_mult_highpart_p (TYPE_MODE (type), true))))
    {
      if (code != PLUS_EXPR)
	return false;
      if (TREE_CODE (rhs1) != SSA_NAME
	  || !gimple_assign_cast_p (SSA_NAME_DEF_STMT (rhs1)))
	return false;
      rhs1 = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (rhs1));
      tree type1 = TREE_TYPE (rhs1);
      if (!INTEGRAL_TYPE_P (type1)
	  || !TYPE_UNSIGNED (type1)
	  || TYPE_PRECISION (type1) >= TYPE_PRECISION (type)
	  || (TYPE_PRECISION (type1)
	      != GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (type1))))
	return false;
      if (TREE_CODE (rhs2) == INTEGER_CST)
	{
	  if (wi::ne_p (wi::rshift (wi::to_wide (rhs2),
	  			    TYPE_PRECISION (type1),
				    UNSIGNED), 0))
	    return false;
	  rhs2 = fold_convert (type1, rhs2);
	}
      else
	{
	  if (TREE_CODE (rhs2) != SSA_NAME
	      || !gimple_assign_cast_p (SSA_NAME_DEF_STMT (rhs2)))
	    return false;
	  rhs2 = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (rhs2));
	  tree type2 = TREE_TYPE (rhs2);
	  if (!INTEGRAL_TYPE_P (type2)
	      || !TYPE_UNSIGNED (type2)
	      || TYPE_PRECISION (type2) >= TYPE_PRECISION (type)
	      || (TYPE_PRECISION (type2)
		  != GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (type2))))
	    return false;
	}
      if (TYPE_PRECISION (type1) >= TYPE_PRECISION (TREE_TYPE (rhs2)))
	type = type1;
      else
	type = TREE_TYPE (rhs2);

      if (TREE_CODE (type) != INTEGER_TYPE
	  || optab_handler (uaddv4_optab,
			    TYPE_MODE (type)) == CODE_FOR_nothing)
	return false;

      maxval = wide_int_to_tree (type, wi::max_value (TYPE_PRECISION (type),
						      UNSIGNED));
      ovf_use_seen = false;
      use_seen = false;
      basic_block use_bb = NULL;
      FOR_EACH_IMM_USE_FAST (use_p, iter, lhs)
	{
	  use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;

	  if (arith_overflow_check_p (stmt, NULL, use_stmt, maxval, NULL))
	    {
	      ovf_use_seen = true;
	      use_bb = gimple_bb (use_stmt);
	    }
	  else
	    {
	      if (!gimple_assign_cast_p (use_stmt)
		  || gimple_assign_rhs_code (use_stmt) == VIEW_CONVERT_EXPR)
		return false;
	      tree use_lhs = gimple_assign_lhs (use_stmt);
	      if (!INTEGRAL_TYPE_P (TREE_TYPE (use_lhs))
		  || (TYPE_PRECISION (TREE_TYPE (use_lhs))
		      > TYPE_PRECISION (type)))
		return false;
	      use_seen = true;
	    }
	}
      if (!ovf_use_seen)
	return false;
      if (!useless_type_conversion_p (type, TREE_TYPE (rhs1)))
	{
	  if (!use_seen)
	    return false;
	  tree new_rhs1 = make_ssa_name (type);
	  gimple *g = gimple_build_assign (new_rhs1, NOP_EXPR, rhs1);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  rhs1 = new_rhs1;
	}
      else if (!useless_type_conversion_p (type, TREE_TYPE (rhs2)))
	{
	  if (!use_seen)
	    return false;
	  tree new_rhs2 = make_ssa_name (type);
	  gimple *g = gimple_build_assign (new_rhs2, NOP_EXPR, rhs2);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  rhs2 = new_rhs2;
	}
      else if (!use_seen)
	{
	  /* If there are no uses of the wider addition, check if
	     forwprop has not created a narrower addition.
	     Require it to be in the same bb as the overflow check.  */
	  FOR_EACH_IMM_USE_FAST (use_p, iter, rhs1)
	    {
	      use_stmt = USE_STMT (use_p);
	      if (is_gimple_debug (use_stmt))
		continue;

	      if (use_stmt == stmt)
		continue;

	      if (!is_gimple_assign (use_stmt)
		  || gimple_bb (use_stmt) != use_bb
		  || gimple_assign_rhs_code (use_stmt) != PLUS_EXPR)
		continue;

	      if (gimple_assign_rhs1 (use_stmt) == rhs1)
		{
		  if (!operand_equal_p (gimple_assign_rhs2 (use_stmt),
					rhs2, 0))
		    continue;
		}
	      else if (gimple_assign_rhs2 (use_stmt) == rhs1)
		{
		  if (gimple_assign_rhs1 (use_stmt) != rhs2)
		    continue;
		}
	      else
		continue;

	      add_stmt = use_stmt;
	      break;
	    }
	  if (add_stmt == NULL)
	    return false;

	  /* If stmt and add_stmt are in the same bb, we need to find out
	     which one is earlier.  If they are in different bbs, we've
	     checked add_stmt is in the same bb as one of the uses of the
	     stmt lhs, so stmt needs to dominate add_stmt too.  */
	  if (gimple_bb (stmt) == gimple_bb (add_stmt))
	    {
	      gimple_stmt_iterator gsif = *gsi;
	      gimple_stmt_iterator gsib = *gsi;
	      int i;
	      /* Search both forward and backward from stmt and have a small
		 upper bound.  */
	      for (i = 0; i < 128; i++)
		{
		  if (!gsi_end_p (gsib))
		    {
		      gsi_prev_nondebug (&gsib);
		      if (gsi_stmt (gsib) == add_stmt)
			{
			  add_first = true;
			  break;
			}
		    }
		  else if (gsi_end_p (gsif))
		    break;
		  if (!gsi_end_p (gsif))
		    {
		      gsi_next_nondebug (&gsif);
		      if (gsi_stmt (gsif) == add_stmt)
			break;
		    }
		}
	      if (i == 128)
		return false;
	      if (add_first)
		*gsi = gsi_for_stmt (add_stmt);
	    }
	}
    }

  if (code == BIT_NOT_EXPR)
    *gsi = gsi_for_stmt (cond_stmt);

  auto_vec<gimple *, 8> mul_stmts;
  if (code == MULT_EXPR && cast_stmt)
    {
      type = TREE_TYPE (cast_lhs);
      gimple *g = SSA_NAME_DEF_STMT (rhs1);
      if (gimple_assign_cast_p (g)
	  && useless_type_conversion_p (type,
					TREE_TYPE (gimple_assign_rhs1 (g)))
	  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs1 (g)))
	rhs1 = gimple_assign_rhs1 (g);
      else
	{
	  g = gimple_build_assign (make_ssa_name (type), NOP_EXPR, rhs1);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  rhs1 = gimple_assign_lhs (g);
	  mul_stmts.quick_push (g);
	}
      if (TREE_CODE (rhs2) == INTEGER_CST)
	rhs2 = fold_convert (type, rhs2);
      else
	{
	  g = SSA_NAME_DEF_STMT (rhs2);
	  if (gimple_assign_cast_p (g)
	      && useless_type_conversion_p (type,
					    TREE_TYPE (gimple_assign_rhs1 (g)))
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs1 (g)))
	    rhs2 = gimple_assign_rhs1 (g);
	  else
	    {
	      g = gimple_build_assign (make_ssa_name (type), NOP_EXPR, rhs2);
	      gsi_insert_before (gsi, g, GSI_SAME_STMT);
	      rhs2 = gimple_assign_lhs (g);
	      mul_stmts.quick_push (g);
	    }
	}
    }
  tree ctype = build_complex_type (type);
  gcall *g = gimple_build_call_internal (code == MULT_EXPR
					 ? IFN_MUL_OVERFLOW
					 : code != MINUS_EXPR
					 ? IFN_ADD_OVERFLOW : IFN_SUB_OVERFLOW,
					 2, rhs1, rhs2);
  tree ctmp = make_ssa_name (ctype);
  gimple_call_set_lhs (g, ctmp);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  tree new_lhs = (maxval || cast_stmt) ? make_ssa_name (type) : lhs;
  gassign *g2;
  if (code != BIT_NOT_EXPR)
    {
      g2 = gimple_build_assign (new_lhs, REALPART_EXPR,
				build1 (REALPART_EXPR, type, ctmp));
      if (maxval || cast_stmt)
	{
	  gsi_insert_before (gsi, g2, GSI_SAME_STMT);
	  if (add_first)
	    *gsi = gsi_for_stmt (stmt);
	}
      else
	gsi_replace (gsi, g2, true);
      if (code == MULT_EXPR)
	{
	  mul_stmts.quick_push (g);
	  mul_stmts.quick_push (g2);
	  if (cast_stmt)
	    {
	      g2 = gimple_build_assign (lhs, NOP_EXPR, new_lhs);
	      gsi_replace (gsi, g2, true);
	      mul_stmts.quick_push (g2);
	    }
	}
    }
  tree ovf = make_ssa_name (type);
  g2 = gimple_build_assign (ovf, IMAGPART_EXPR,
			    build1 (IMAGPART_EXPR, type, ctmp));
  if (code != BIT_NOT_EXPR)
    gsi_insert_after (gsi, g2, GSI_NEW_STMT);
  else
    gsi_insert_before (gsi, g2, GSI_SAME_STMT);
  if (code == MULT_EXPR)
    mul_stmts.quick_push (g2);

  FOR_EACH_IMM_USE_STMT (use_stmt, iter, cast_lhs ? cast_lhs : lhs)
    {
      if (is_gimple_debug (use_stmt))
	continue;

      gimple *orig_use_stmt = use_stmt;
      int ovf_use = arith_overflow_check_p (stmt, cast_stmt, use_stmt,
					    maxval, NULL);
      if (ovf_use == 0)
	{
	  gcc_assert (code != BIT_NOT_EXPR);
	  if (maxval)
	    {
	      tree use_lhs = gimple_assign_lhs (use_stmt);
	      gimple_assign_set_rhs1 (use_stmt, new_lhs);
	      if (useless_type_conversion_p (TREE_TYPE (use_lhs),
					     TREE_TYPE (new_lhs)))
		gimple_assign_set_rhs_code (use_stmt, SSA_NAME);
	      update_stmt (use_stmt);
	    }
	  continue;
	}
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
	      if (gimple_assign_rhs_code (use_stmt) == RSHIFT_EXPR)
		{
		  g2 = gimple_build_assign (make_ssa_name (boolean_type_node),
					    ovf_use == 1 ? NE_EXPR : EQ_EXPR,
					    ovf, build_int_cst (type, 0));
		  gimple_stmt_iterator gsiu = gsi_for_stmt (use_stmt);
		  gsi_insert_before (&gsiu, g2, GSI_SAME_STMT);
		  gimple_assign_set_rhs_with_ops (&gsiu, NOP_EXPR,
						  gimple_assign_lhs (g2));
		  update_stmt (use_stmt);
		  use_operand_p use;
		  single_imm_use (gimple_assign_lhs (use_stmt), &use,
				  &use_stmt);
		  if (gimple_code (use_stmt) == GIMPLE_COND)
		    {
		      gcond *cond_stmt = as_a <gcond *> (use_stmt);
		      gimple_cond_set_lhs (cond_stmt, ovf);
		      gimple_cond_set_rhs (cond_stmt, build_int_cst (type, 0));
		    }
		  else
		    {
		      gcc_checking_assert (is_gimple_assign (use_stmt));
		      if (gimple_assign_rhs_class (use_stmt)
			  == GIMPLE_BINARY_RHS)
			{
			  gimple_assign_set_rhs1 (use_stmt, ovf);
			  gimple_assign_set_rhs2 (use_stmt,
						  build_int_cst (type, 0));
			}
		      else if (gimple_assign_cast_p (use_stmt))
			gimple_assign_set_rhs1 (use_stmt, ovf);
		      else
			{
			  tree_code sc = gimple_assign_rhs_code (use_stmt);
			  gcc_checking_assert (sc == COND_EXPR);
			  tree cond = gimple_assign_rhs1 (use_stmt);
			  cond = build2 (TREE_CODE (cond),
					 boolean_type_node, ovf,
					 build_int_cst (type, 0));
			  gimple_assign_set_rhs1 (use_stmt, cond);
			}
		    }
		  update_stmt (use_stmt);
		  gsi_remove (&gsiu, true);
		  gsiu = gsi_for_stmt (g2);
		  gsi_remove (&gsiu, true);
		  continue;
		}
	      else
		{
		  gimple_assign_set_rhs1 (use_stmt, ovf);
		  gimple_assign_set_rhs2 (use_stmt, build_int_cst (type, 0));
		  gimple_assign_set_rhs_code (use_stmt,
					      ovf_use == 1
					      ? NE_EXPR : EQ_EXPR);
		}
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
      if (code == MULT_EXPR && use_stmt != orig_use_stmt)
	{
	  gimple_stmt_iterator gsi2 = gsi_for_stmt (orig_use_stmt);
	  maybe_optimize_guarding_check (mul_stmts, use_stmt, orig_use_stmt,
					 cfg_changed);
	  use_operand_p use;
	  gimple *cast_stmt;
	  if (single_imm_use (gimple_assign_lhs (orig_use_stmt), &use,
			      &cast_stmt)
	      && gimple_assign_cast_p (cast_stmt))
	    {
	      gimple_stmt_iterator gsi3 = gsi_for_stmt (cast_stmt);
	      gsi_remove (&gsi3, true);
	      release_ssa_name (gimple_assign_lhs (cast_stmt));
	    }
	  gsi_remove (&gsi2, true);
	  release_ssa_name (gimple_assign_lhs (orig_use_stmt));
	}
    }
  if (maxval)
    {
      gimple_stmt_iterator gsi2 = gsi_for_stmt (stmt);
      gsi_remove (&gsi2, true);
      if (add_stmt)
	{
	  gimple *g = gimple_build_assign (gimple_assign_lhs (add_stmt),
					   new_lhs);
	  gsi2 = gsi_for_stmt (add_stmt);
	  gsi_replace (&gsi2, g, true);
	}
    }
  else if (code == BIT_NOT_EXPR)
    {
      *gsi = gsi_for_stmt (stmt);
      gsi_remove (gsi, true);
      release_ssa_name (lhs);
      return true;
    }
  return false;
}

/* Helper of match_uaddc_usubc.  Look through an integral cast
   which should preserve [0, 1] range value (unless source has
   1-bit signed type) and the cast has single use.  */

static gimple *
uaddc_cast (gimple *g)
{
  if (!gimple_assign_cast_p (g))
    return g;
  tree op = gimple_assign_rhs1 (g);
  if (TREE_CODE (op) == SSA_NAME
      && INTEGRAL_TYPE_P (TREE_TYPE (op))
      && (TYPE_PRECISION (TREE_TYPE (op)) > 1
	  || TYPE_UNSIGNED (TREE_TYPE (op)))
      && has_single_use (gimple_assign_lhs (g)))
    return SSA_NAME_DEF_STMT (op);
  return g;
}

/* Helper of match_uaddc_usubc.  Look through a NE_EXPR
   comparison with 0 which also preserves [0, 1] value range.  */

static gimple *
uaddc_ne0 (gimple *g)
{
  if (is_gimple_assign (g)
      && gimple_assign_rhs_code (g) == NE_EXPR
      && integer_zerop (gimple_assign_rhs2 (g))
      && TREE_CODE (gimple_assign_rhs1 (g)) == SSA_NAME
      && has_single_use (gimple_assign_lhs (g)))
    return SSA_NAME_DEF_STMT (gimple_assign_rhs1 (g));
  return g;
}

/* Return true if G is {REAL,IMAG}PART_EXPR PART with SSA_NAME
   operand.  */

static bool
uaddc_is_cplxpart (gimple *g, tree_code part)
{
  return (is_gimple_assign (g)
	  && gimple_assign_rhs_code (g) == part
	  && TREE_CODE (TREE_OPERAND (gimple_assign_rhs1 (g), 0)) == SSA_NAME);
}

/* Try to match e.g.
   _29 = .ADD_OVERFLOW (_3, _4);
   _30 = REALPART_EXPR <_29>;
   _31 = IMAGPART_EXPR <_29>;
   _32 = .ADD_OVERFLOW (_30, _38);
   _33 = REALPART_EXPR <_32>;
   _34 = IMAGPART_EXPR <_32>;
   _35 = _31 + _34;
   as
   _36 = .UADDC (_3, _4, _38);
   _33 = REALPART_EXPR <_36>;
   _35 = IMAGPART_EXPR <_36>;
   or
   _22 = .SUB_OVERFLOW (_6, _5);
   _23 = REALPART_EXPR <_22>;
   _24 = IMAGPART_EXPR <_22>;
   _25 = .SUB_OVERFLOW (_23, _37);
   _26 = REALPART_EXPR <_25>;
   _27 = IMAGPART_EXPR <_25>;
   _28 = _24 | _27;
   as
   _29 = .USUBC (_6, _5, _37);
   _26 = REALPART_EXPR <_29>;
   _288 = IMAGPART_EXPR <_29>;
   provided _38 or _37 above have [0, 1] range
   and _3, _4 and _30 or _6, _5 and _23 are unsigned
   integral types with the same precision.  Whether + or | or ^ is
   used on the IMAGPART_EXPR results doesn't matter, with one of
   added or subtracted operands in [0, 1] range at most one
   .ADD_OVERFLOW or .SUB_OVERFLOW will indicate overflow.  */

static bool
match_uaddc_usubc (gimple_stmt_iterator *gsi, gimple *stmt, tree_code code)
{
  tree rhs[4];
  rhs[0] = gimple_assign_rhs1 (stmt);
  rhs[1] = gimple_assign_rhs2 (stmt);
  rhs[2] = NULL_TREE;
  rhs[3] = NULL_TREE;
  tree type = TREE_TYPE (rhs[0]);
  if (!INTEGRAL_TYPE_P (type) || !TYPE_UNSIGNED (type))
    return false;

  auto_vec<gimple *, 2> temp_stmts;
  if (code != BIT_IOR_EXPR && code != BIT_XOR_EXPR)
    {
      /* If overflow flag is ignored on the MSB limb, we can end up with
	 the most significant limb handled as r = op1 + op2 + ovf1 + ovf2;
	 or r = op1 - op2 - ovf1 - ovf2; or various equivalent expressions
	 thereof.  Handle those like the ovf = ovf1 + ovf2; case to recognize
	 the limb below the MSB, but also create another .UADDC/.USUBC call
	 for the last limb.

	 First look through assignments with the same rhs code as CODE,
	 with the exception that subtraction of a constant is canonicalized
	 into addition of its negation.  rhs[0] will be minuend for
	 subtractions and one of addends for addition, all other assigned
	 rhs[i] operands will be subtrahends or other addends.  */
      while (TREE_CODE (rhs[0]) == SSA_NAME && !rhs[3])
	{
	  gimple *g = SSA_NAME_DEF_STMT (rhs[0]);
	  if (has_single_use (rhs[0])
	      && is_gimple_assign (g)
	      && (gimple_assign_rhs_code (g) == code
		  || (code == MINUS_EXPR
		      && gimple_assign_rhs_code (g) == PLUS_EXPR
		      && TREE_CODE (gimple_assign_rhs2 (g)) == INTEGER_CST)))
	    {
	      tree r2 = gimple_assign_rhs2 (g);
	      if (gimple_assign_rhs_code (g) != code)
		{
		  r2 = const_unop (NEGATE_EXPR, TREE_TYPE (r2), r2);
		  if (!r2)
		    break;
		}
	      rhs[0] = gimple_assign_rhs1 (g);
	      tree &r = rhs[2] ? rhs[3] : rhs[2];
	      r = r2;
	      temp_stmts.quick_push (g);
	    }
	  else
	    break;
	}
      for (int i = 1; i <= 2; ++i)
	while (rhs[i] && TREE_CODE (rhs[i]) == SSA_NAME && !rhs[3])
	  {
	    gimple *g = SSA_NAME_DEF_STMT (rhs[i]);
	    if (has_single_use (rhs[i])
		&& is_gimple_assign (g)
		&& gimple_assign_rhs_code (g) == PLUS_EXPR)
	      {
		rhs[i] = gimple_assign_rhs1 (g);
		if (rhs[2])
		  rhs[3] = gimple_assign_rhs2 (g);
		else
		  rhs[2] = gimple_assign_rhs2 (g);
		temp_stmts.quick_push (g);
	      }
	    else
	      break;
	  }
      /* If there are just 3 addends or one minuend and two subtrahends,
	 check for UADDC or USUBC being pattern recognized earlier.
	 Say r = op1 + op2 + ovf1 + ovf2; where the (ovf1 + ovf2) part
	 got pattern matched earlier as __imag__ .UADDC (arg1, arg2, arg3)
	 etc.  */
      if (rhs[2] && !rhs[3])
	{
	  for (int i = (code == MINUS_EXPR ? 1 : 0); i < 3; ++i)
	    if (TREE_CODE (rhs[i]) == SSA_NAME)
	      {
		gimple *im = uaddc_cast (SSA_NAME_DEF_STMT (rhs[i]));
		im = uaddc_ne0 (im);
		if (uaddc_is_cplxpart (im, IMAGPART_EXPR))
		  {
		    /* We found one of the 3 addends or 2 subtrahends to be
		       __imag__ of something, verify it is .UADDC/.USUBC.  */
		    tree rhs1 = gimple_assign_rhs1 (im);
		    gimple *ovf = SSA_NAME_DEF_STMT (TREE_OPERAND (rhs1, 0));
		    tree ovf_lhs = NULL_TREE;
		    tree ovf_arg1 = NULL_TREE, ovf_arg2 = NULL_TREE;
		    if (gimple_call_internal_p (ovf, code == PLUS_EXPR
						     ? IFN_ADD_OVERFLOW
						     : IFN_SUB_OVERFLOW))
		      {
			/* Or verify it is .ADD_OVERFLOW/.SUB_OVERFLOW.
			   This is for the case of 2 chained .UADDC/.USUBC,
			   where the first one uses 0 carry-in and the second
			   one ignores the carry-out.
			   So, something like:
			   _16 = .ADD_OVERFLOW (_1, _2);
			   _17 = REALPART_EXPR <_16>;
			   _18 = IMAGPART_EXPR <_16>;
			   _15 = _3 + _4;
			   _12 = _15 + _18;
			   where the first 3 statements come from the lower
			   limb addition and the last 2 from the higher limb
			   which ignores carry-out.  */
			ovf_lhs = gimple_call_lhs (ovf);
			tree ovf_lhs_type = TREE_TYPE (TREE_TYPE (ovf_lhs));
			ovf_arg1 = gimple_call_arg (ovf, 0);
			ovf_arg2 = gimple_call_arg (ovf, 1);
			/* In that case we need to punt if the types don't
			   mismatch.  */
			if (!types_compatible_p (type, ovf_lhs_type)
			    || !types_compatible_p (type, TREE_TYPE (ovf_arg1))
			    || !types_compatible_p (type,
						    TREE_TYPE (ovf_arg2)))
			  ovf_lhs = NULL_TREE;
			else
			  {
			    for (int i = (code == PLUS_EXPR ? 1 : 0);
				 i >= 0; --i)
			      {
				tree r = gimple_call_arg (ovf, i);
				if (TREE_CODE (r) != SSA_NAME)
				  continue;
				if (uaddc_is_cplxpart (SSA_NAME_DEF_STMT (r),
						       REALPART_EXPR))
				  {
				    /* Punt if one of the args which isn't
				       subtracted isn't __real__; that could
				       then prevent better match later.
				       Consider:
				       _3 = .ADD_OVERFLOW (_1, _2);
				       _4 = REALPART_EXPR <_3>;
				       _5 = IMAGPART_EXPR <_3>;
				       _7 = .ADD_OVERFLOW (_4, _6);
				       _8 = REALPART_EXPR <_7>;
				       _9 = IMAGPART_EXPR <_7>;
				       _12 = _10 + _11;
				       _13 = _12 + _9;
				       _14 = _13 + _5;
				       We want to match this when called on
				       the last stmt as a pair of .UADDC calls,
				       but without this check we could turn
				       that prematurely on _13 = _12 + _9;
				       stmt into .UADDC with 0 carry-in just
				       on the second .ADD_OVERFLOW call and
				       another replacing the _12 and _13
				       additions.  */
				    ovf_lhs = NULL_TREE;
				    break;
				  }
			      }
			  }
			if (ovf_lhs)
			  {
			    use_operand_p use_p;
			    imm_use_iterator iter;
			    tree re_lhs = NULL_TREE;
			    FOR_EACH_IMM_USE_FAST (use_p, iter, ovf_lhs)
			      {
				gimple *use_stmt = USE_STMT (use_p);
				if (is_gimple_debug (use_stmt))
				  continue;
				if (use_stmt == im)
				  continue;
				if (!uaddc_is_cplxpart (use_stmt,
							REALPART_EXPR))
				  {
				    ovf_lhs = NULL_TREE;
				    break;
				  }
				re_lhs = gimple_assign_lhs (use_stmt);
			      }
			    if (ovf_lhs && re_lhs)
			      {
				FOR_EACH_IMM_USE_FAST (use_p, iter, re_lhs)
				  {
				    gimple *use_stmt = USE_STMT (use_p);
				    if (is_gimple_debug (use_stmt))
				      continue;
				    internal_fn ifn
				      = gimple_call_internal_fn (ovf);
				    /* Punt if the __real__ of lhs is used
				       in the same .*_OVERFLOW call.
				       Consider:
				       _3 = .ADD_OVERFLOW (_1, _2);
				       _4 = REALPART_EXPR <_3>;
				       _5 = IMAGPART_EXPR <_3>;
				       _7 = .ADD_OVERFLOW (_4, _6);
				       _8 = REALPART_EXPR <_7>;
				       _9 = IMAGPART_EXPR <_7>;
				       _12 = _10 + _11;
				       _13 = _12 + _5;
				       _14 = _13 + _9;
				       We want to match this when called on
				       the last stmt as a pair of .UADDC calls,
				       but without this check we could turn
				       that prematurely on _13 = _12 + _5;
				       stmt into .UADDC with 0 carry-in just
				       on the first .ADD_OVERFLOW call and
				       another replacing the _12 and _13
				       additions.  */
				    if (gimple_call_internal_p (use_stmt, ifn))
				      {
					ovf_lhs = NULL_TREE;
					break;
				      }
				  }
			      }
			  }
		      }
		    if ((ovf_lhs
			 || gimple_call_internal_p (ovf,
						    code == PLUS_EXPR
						    ? IFN_UADDC : IFN_USUBC))
			&& (optab_handler (code == PLUS_EXPR
					   ? uaddc5_optab : usubc5_optab,
					   TYPE_MODE (type))
			    != CODE_FOR_nothing))
		      {
			/* And in that case build another .UADDC/.USUBC
			   call for the most significand limb addition.
			   Overflow bit is ignored here.  */
			if (i != 2)
			  std::swap (rhs[i], rhs[2]);
			gimple *g
			  = gimple_build_call_internal (code == PLUS_EXPR
							? IFN_UADDC
							: IFN_USUBC,
							3, rhs[0], rhs[1],
							rhs[2]);
			tree nlhs = make_ssa_name (build_complex_type (type));
			gimple_call_set_lhs (g, nlhs);
			gsi_insert_before (gsi, g, GSI_SAME_STMT);
			tree ilhs = gimple_assign_lhs (stmt);
			g = gimple_build_assign (ilhs, REALPART_EXPR,
						 build1 (REALPART_EXPR,
							 TREE_TYPE (ilhs),
							 nlhs));
			gsi_replace (gsi, g, true);
			/* And if it is initialized from result of __imag__
			   of .{ADD,SUB}_OVERFLOW call, replace that
			   call with .U{ADD,SUB}C call with the same arguments,
			   just 0 added as third argument.  This isn't strictly
			   necessary, .ADD_OVERFLOW (x, y) and .UADDC (x, y, 0)
			   produce the same result, but may result in better
			   generated code on some targets where the backend can
			   better prepare in how the result will be used.  */
			if (ovf_lhs)
			  {
			    tree zero = build_zero_cst (type);
			    g = gimple_build_call_internal (code == PLUS_EXPR
							    ? IFN_UADDC
							    : IFN_USUBC,
							    3, ovf_arg1,
							    ovf_arg2, zero);
			    gimple_call_set_lhs (g, ovf_lhs);
			    gimple_stmt_iterator gsi2 = gsi_for_stmt (ovf);
			    gsi_replace (&gsi2, g, true);
			  }
			return true;
		      }
		  }
	      }
	  return false;
	}
      if (code == MINUS_EXPR && !rhs[2])
	return false;
      if (code == MINUS_EXPR)
	/* Code below expects rhs[0] and rhs[1] to have the IMAGPART_EXPRs.
	   So, for MINUS_EXPR swap the single added rhs operand (others are
	   subtracted) to rhs[3].  */
	std::swap (rhs[0], rhs[3]);
    }
  /* Walk from both operands of STMT (for +/- even sometimes from
     all the 4 addends or 3 subtrahends), see through casts and != 0
     statements which would preserve [0, 1] range of values and
     check which is initialized from __imag__.  */
  gimple *im1 = NULL, *im2 = NULL;
  for (int i = 0; i < (code == MINUS_EXPR ? 3 : 4); i++)
    if (rhs[i] && TREE_CODE (rhs[i]) == SSA_NAME)
      {
	gimple *im = uaddc_cast (SSA_NAME_DEF_STMT (rhs[i]));
	im = uaddc_ne0 (im);
	if (uaddc_is_cplxpart (im, IMAGPART_EXPR))
	  {
	    if (im1 == NULL)
	      {
		im1 = im;
		if (i != 0)
		  std::swap (rhs[0], rhs[i]);
	      }
	    else
	      {
		im2 = im;
		if (i != 1)
		  std::swap (rhs[1], rhs[i]);
		break;
	      }
	  }
      }
  /* If we don't find at least two, punt.  */
  if (!im2)
    return false;
  /* Check they are __imag__ of .ADD_OVERFLOW or .SUB_OVERFLOW call results,
     either both .ADD_OVERFLOW or both .SUB_OVERFLOW and that we have
     uaddc5/usubc5 named pattern for the corresponding mode.  */
  gimple *ovf1
    = SSA_NAME_DEF_STMT (TREE_OPERAND (gimple_assign_rhs1 (im1), 0));
  gimple *ovf2
    = SSA_NAME_DEF_STMT (TREE_OPERAND (gimple_assign_rhs1 (im2), 0));
  internal_fn ifn;
  if (!is_gimple_call (ovf1)
      || !gimple_call_internal_p (ovf1)
      || ((ifn = gimple_call_internal_fn (ovf1)) != IFN_ADD_OVERFLOW
	  && ifn != IFN_SUB_OVERFLOW)
      || !gimple_call_internal_p (ovf2, ifn)
      || optab_handler (ifn == IFN_ADD_OVERFLOW ? uaddc5_optab : usubc5_optab,
			TYPE_MODE (type)) == CODE_FOR_nothing
      || (rhs[2]
	  && optab_handler (code == PLUS_EXPR ? uaddc5_optab : usubc5_optab,
			    TYPE_MODE (type)) == CODE_FOR_nothing))
    return false;
  tree arg1, arg2, arg3 = NULL_TREE;
  gimple *re1 = NULL, *re2 = NULL;
  /* On one of the two calls, one of the .ADD_OVERFLOW/.SUB_OVERFLOW arguments
     should be initialized from __real__ of the other of the two calls.
     Though, for .SUB_OVERFLOW, it has to be the first argument, not the
     second one.  */
  for (int i = (ifn == IFN_ADD_OVERFLOW ? 1 : 0); i >= 0; --i)
    for (gimple *ovf = ovf1; ovf; ovf = (ovf == ovf1 ? ovf2 : NULL))
      {
	tree arg = gimple_call_arg (ovf, i);
	if (TREE_CODE (arg) != SSA_NAME)
	  continue;
	re1 = SSA_NAME_DEF_STMT (arg);
	if (uaddc_is_cplxpart (re1, REALPART_EXPR)
	    && (SSA_NAME_DEF_STMT (TREE_OPERAND (gimple_assign_rhs1 (re1), 0))
		== (ovf == ovf1 ? ovf2 : ovf1)))
	  {
	    if (ovf == ovf1)
	      {
		/* Make sure ovf2 is the .*_OVERFLOW call with argument
		   initialized from __real__ of ovf1.  */
		std::swap (rhs[0], rhs[1]);
		std::swap (im1, im2);
		std::swap (ovf1, ovf2);
	      }
	    arg3 = gimple_call_arg (ovf, 1 - i);
	    i = -1;
	    break;
	  }
      }
  if (!arg3)
    return false;
  arg1 = gimple_call_arg (ovf1, 0);
  arg2 = gimple_call_arg (ovf1, 1);
  if (!types_compatible_p (type, TREE_TYPE (arg1)))
    return false;
  int kind[2] = { 0, 0 };
  tree arg_im[2] = { NULL_TREE, NULL_TREE };
  /* At least one of arg2 and arg3 should have type compatible
     with arg1/rhs[0], and the other one should have value in [0, 1]
     range.  If both are in [0, 1] range and type compatible with
     arg1/rhs[0], try harder to find after looking through casts,
     != 0 comparisons which one is initialized to __imag__ of
     .{ADD,SUB}_OVERFLOW or .U{ADD,SUB}C call results.  */
  for (int i = 0; i < 2; ++i)
    {
      tree arg = i == 0 ? arg2 : arg3;
      if (types_compatible_p (type, TREE_TYPE (arg)))
	kind[i] = 1;
      if (!INTEGRAL_TYPE_P (TREE_TYPE (arg))
	  || (TYPE_PRECISION (TREE_TYPE (arg)) == 1
	      && !TYPE_UNSIGNED (TREE_TYPE (arg))))
	continue;
      if (tree_zero_one_valued_p (arg))
	kind[i] |= 2;
      if (TREE_CODE (arg) == SSA_NAME)
	{
	  gimple *g = SSA_NAME_DEF_STMT (arg);
	  if (gimple_assign_cast_p (g))
	    {
	      tree op = gimple_assign_rhs1 (g);
	      if (TREE_CODE (op) == SSA_NAME
		  && INTEGRAL_TYPE_P (TREE_TYPE (op)))
		g = SSA_NAME_DEF_STMT (op);
	    }
	  g = uaddc_ne0 (g);
	  if (!uaddc_is_cplxpart (g, IMAGPART_EXPR))
	    continue;
	  arg_im[i] = gimple_assign_lhs (g);
	  g = SSA_NAME_DEF_STMT (TREE_OPERAND (gimple_assign_rhs1 (g), 0));
	  if (!is_gimple_call (g) || !gimple_call_internal_p (g))
	    continue;
	  switch (gimple_call_internal_fn (g))
	    {
	    case IFN_ADD_OVERFLOW:
	    case IFN_SUB_OVERFLOW:
	    case IFN_UADDC:
	    case IFN_USUBC:
	      break;
	    default:
	      continue;
	    }
	  kind[i] |= 4;
	}
    }
  /* Make arg2 the one with compatible type and arg3 the one
     with [0, 1] range.  If both is true for both operands,
     prefer as arg3 result of __imag__ of some ifn.  */
  if ((kind[0] & 1) == 0 || ((kind[1] & 1) != 0 && kind[0] > kind[1]))
    {
      std::swap (arg2, arg3);
      std::swap (kind[0], kind[1]);
      std::swap (arg_im[0], arg_im[1]);
    }
  if ((kind[0] & 1) == 0 || (kind[1] & 6) == 0)
    return false;
  if (!has_single_use (gimple_assign_lhs (im1))
      || !has_single_use (gimple_assign_lhs (im2))
      || !has_single_use (gimple_assign_lhs (re1))
      || num_imm_uses (gimple_call_lhs (ovf1)) != 2)
    return false;
  /* Check that ovf2's result is used in __real__ and set re2
     to that statement.  */
  use_operand_p use_p;
  imm_use_iterator iter;
  tree lhs = gimple_call_lhs (ovf2);
  FOR_EACH_IMM_USE_FAST (use_p, iter, lhs)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;
      if (use_stmt == im2)
	continue;
      if (re2)
	return false;
      if (!uaddc_is_cplxpart (use_stmt, REALPART_EXPR))
	return false;
      re2 = use_stmt;
    }
  /* Build .UADDC/.USUBC call which will be placed before the stmt.  */
  gimple_stmt_iterator gsi2 = gsi_for_stmt (ovf2);
  gimple *g;
  if ((kind[1] & 4) != 0 && types_compatible_p (type, TREE_TYPE (arg_im[1])))
    arg3 = arg_im[1];
  if ((kind[1] & 1) == 0)
    {
      if (TREE_CODE (arg3) == INTEGER_CST)
	arg3 = fold_convert (type, arg3);
      else
	{
	  g = gimple_build_assign (make_ssa_name (type), NOP_EXPR, arg3);
	  gsi_insert_before (&gsi2, g, GSI_SAME_STMT);
	  arg3 = gimple_assign_lhs (g);
	}
    }
  g = gimple_build_call_internal (ifn == IFN_ADD_OVERFLOW
				  ? IFN_UADDC : IFN_USUBC,
				  3, arg1, arg2, arg3);
  tree nlhs = make_ssa_name (TREE_TYPE (lhs));
  gimple_call_set_lhs (g, nlhs);
  gsi_insert_before (&gsi2, g, GSI_SAME_STMT);
  /* In the case where stmt is | or ^ of two overflow flags
     or addition of those, replace stmt with __imag__ of the above
     added call.  In case of arg1 + arg2 + (ovf1 + ovf2) or
     arg1 - arg2 - (ovf1 + ovf2) just emit it before stmt.  */
  tree ilhs = rhs[2] ? make_ssa_name (type) : gimple_assign_lhs (stmt);
  g = gimple_build_assign (ilhs, IMAGPART_EXPR,
			   build1 (IMAGPART_EXPR, TREE_TYPE (ilhs), nlhs));
  if (rhs[2])
    {
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      /* Remove some further statements which can't be kept in the IL because
	 they can use SSA_NAMEs whose setter is going to be removed too.  */
      for (gimple *g2 : temp_stmts)
	{
	  gsi2 = gsi_for_stmt (g2);
	  gsi_remove (&gsi2, true);
	  release_defs (g2);
	}
    }
  else
    gsi_replace (gsi, g, true);
  /* Remove some statements which can't be kept in the IL because they
     use SSA_NAME whose setter is going to be removed too.  */
  tree rhs1 = rhs[1];
  for (int i = 0; i < 2; i++)
    if (rhs1 == gimple_assign_lhs (im2))
      break;
    else
      {
	g = SSA_NAME_DEF_STMT (rhs1);
	rhs1 = gimple_assign_rhs1 (g);
	gsi2 = gsi_for_stmt (g);
	gsi_remove (&gsi2, true);
	release_defs (g);
      }
  gcc_checking_assert (rhs1 == gimple_assign_lhs (im2));
  gsi2 = gsi_for_stmt (im2);
  gsi_remove (&gsi2, true);
  release_defs (im2);
  /* Replace the re2 statement with __real__ of the newly added
     .UADDC/.USUBC call.  */
  if (re2)
    {
      gsi2 = gsi_for_stmt (re2);
      tree rlhs = gimple_assign_lhs (re2);
      g = gimple_build_assign (rlhs, REALPART_EXPR,
			       build1 (REALPART_EXPR, TREE_TYPE (rlhs), nlhs));
      gsi_replace (&gsi2, g, true);
    }
  if (rhs[2])
    {
      /* If this is the arg1 + arg2 + (ovf1 + ovf2) or
	 arg1 - arg2 - (ovf1 + ovf2) case for the most significant limb,
	 replace stmt with __real__ of another .UADDC/.USUBC call which
	 handles the most significant limb.  Overflow flag from this is
	 ignored.  */
      g = gimple_build_call_internal (code == PLUS_EXPR
				      ? IFN_UADDC : IFN_USUBC,
				      3, rhs[3], rhs[2], ilhs);
      nlhs = make_ssa_name (TREE_TYPE (lhs));
      gimple_call_set_lhs (g, nlhs);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      ilhs = gimple_assign_lhs (stmt);
      g = gimple_build_assign (ilhs, REALPART_EXPR,
			       build1 (REALPART_EXPR, TREE_TYPE (ilhs), nlhs));
      gsi_replace (gsi, g, true);
    }
  if (TREE_CODE (arg3) == SSA_NAME)
    {
      /* When pattern recognizing the second least significant limb
	 above (i.e. first pair of .{ADD,SUB}_OVERFLOW calls for one limb),
	 check if the [0, 1] range argument (i.e. carry in) isn't the
	 result of another .{ADD,SUB}_OVERFLOW call (one handling the
	 least significant limb).  Again look through casts and != 0.  */
      gimple *im3 = SSA_NAME_DEF_STMT (arg3);
      for (int i = 0; i < 2; ++i)
	{
	  gimple *im4 = uaddc_cast (im3);
	  if (im4 == im3)
	    break;
	  else
	    im3 = im4;
	}
      im3 = uaddc_ne0 (im3);
      if (uaddc_is_cplxpart (im3, IMAGPART_EXPR))
	{
	  gimple *ovf3
	    = SSA_NAME_DEF_STMT (TREE_OPERAND (gimple_assign_rhs1 (im3), 0));
	  if (gimple_call_internal_p (ovf3, ifn))
	    {
	      lhs = gimple_call_lhs (ovf3);
	      arg1 = gimple_call_arg (ovf3, 0);
	      arg2 = gimple_call_arg (ovf3, 1);
	      if (types_compatible_p (type, TREE_TYPE (TREE_TYPE (lhs)))
		  && types_compatible_p (type, TREE_TYPE (arg1))
		  && types_compatible_p (type, TREE_TYPE (arg2)))
		{
		  /* And if it is initialized from result of __imag__
		     of .{ADD,SUB}_OVERFLOW call, replace that
		     call with .U{ADD,SUB}C call with the same arguments,
		     just 0 added as third argument.  This isn't strictly
		     necessary, .ADD_OVERFLOW (x, y) and .UADDC (x, y, 0)
		     produce the same result, but may result in better
		     generated code on some targets where the backend can
		     better prepare in how the result will be used.  */
		  g = gimple_build_call_internal (ifn == IFN_ADD_OVERFLOW
						  ? IFN_UADDC : IFN_USUBC,
						  3, arg1, arg2,
						  build_zero_cst (type));
		  gimple_call_set_lhs (g, lhs);
		  gsi2 = gsi_for_stmt (ovf3);
		  gsi_replace (&gsi2, g, true);
		}
	    }
	}
    }
  return true;
}

/* Replace .POPCOUNT (x) == 1 or .POPCOUNT (x) != 1 with
   (x & (x - 1)) > x - 1 or (x & (x - 1)) <= x - 1 if .POPCOUNT
   isn't a direct optab.  Also handle `<=`/`>` to be
   `x & (x - 1) !=/== x`. */

static void
match_single_bit_test (gimple_stmt_iterator *gsi, gimple *stmt)
{
  tree clhs, crhs;
  enum tree_code code;
  bool was_le = false;
  if (gimple_code (stmt) == GIMPLE_COND)
    {
      clhs = gimple_cond_lhs (stmt);
      crhs = gimple_cond_rhs (stmt);
      code = gimple_cond_code (stmt);
    }
  else
    {
      clhs = gimple_assign_rhs1 (stmt);
      crhs = gimple_assign_rhs2 (stmt);
      code = gimple_assign_rhs_code (stmt);
    }
  if (code != LE_EXPR && code != GT_EXPR
      && code != EQ_EXPR && code != NE_EXPR)
    return;
  if (code == LE_EXPR || code == GT_EXPR)
    was_le = true;
  if (TREE_CODE (clhs) != SSA_NAME || !integer_onep (crhs))
    return;
  gimple *call = SSA_NAME_DEF_STMT (clhs);
  combined_fn cfn = gimple_call_combined_fn (call);
  switch (cfn)
    {
    CASE_CFN_POPCOUNT:
      break;
    default:
      return;
    }
  if (!has_single_use (clhs))
    return;
  tree arg = gimple_call_arg (call, 0);
  tree type = TREE_TYPE (arg);
  if (!INTEGRAL_TYPE_P (type))
    return;
  bool nonzero_arg = tree_expr_nonzero_p (arg);
  if (direct_internal_fn_supported_p (IFN_POPCOUNT, type, OPTIMIZE_FOR_BOTH))
    {
      /* Tell expand_POPCOUNT the popcount result is only used in equality
	 comparison with one, so that it can decide based on rtx costs.  */
      gimple *g = gimple_build_call_internal (IFN_POPCOUNT, 2, arg,
					      was_le ? integer_minus_one_node
					      : (nonzero_arg ? integer_zero_node
						 : integer_one_node));
      gimple_call_set_lhs (g, gimple_call_lhs (call));
      gimple_stmt_iterator gsi2 = gsi_for_stmt (call);
      gsi_replace (&gsi2, g, true);
      return;
    }
  tree argm1 = make_ssa_name (type);
  gimple *g = gimple_build_assign (argm1, PLUS_EXPR, arg,
				   build_int_cst (type, -1));
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  g = gimple_build_assign (make_ssa_name (type),
			   (nonzero_arg || was_le) ? BIT_AND_EXPR : BIT_XOR_EXPR,
			   arg, argm1);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  tree_code cmpcode;
  if (was_le)
    {
      argm1 = build_zero_cst (type);
      cmpcode = code == LE_EXPR ? EQ_EXPR : NE_EXPR;
    }
  else if (nonzero_arg)
    {
      argm1 = build_zero_cst (type);
      cmpcode = code;
    }
  else
    cmpcode = code == EQ_EXPR ? GT_EXPR : LE_EXPR;
  if (gcond *cond = dyn_cast <gcond *> (stmt))
    {
      gimple_cond_set_lhs (cond, gimple_assign_lhs (g));
      gimple_cond_set_rhs (cond, argm1);
      gimple_cond_set_code (cond, cmpcode);
    }
  else
    {
      gimple_assign_set_rhs1 (stmt, gimple_assign_lhs (g));
      gimple_assign_set_rhs2 (stmt, argm1);
      gimple_assign_set_rhs_code (stmt, cmpcode);
    }
  update_stmt (stmt);
  gimple_stmt_iterator gsi2 = gsi_for_stmt (call);
  gsi_remove (&gsi2, true);
  release_defs (call);
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
  if (CONSTANT_CLASS_P (op1))
    return false;

  if (CONSTANT_CLASS_P (op2))
    {
      if (integer_pow2p (op2))
	return false;

      if (element_precision (type) <= HOST_BITS_PER_WIDE_INT
	  && element_precision (type) <= BITS_PER_WORD)
	return false;

      /* If the divisor is not power of 2 and the precision wider than
	 HWI, expand_divmod punts on that, so in that case it is better
	 to use divmod optab or libfunc.  Similarly if choose_multiplier
	 might need pre/post shifts of BITS_PER_WORD or more.  */
    }

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
  if (stmt_can_throw_internal (cfun, stmt)
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
	  if (stmt_can_throw_internal (cfun, use_stmt))
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
	      || stmt_can_throw_internal (cfun, use_stmt)
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

/* Process a single gimple assignment STMT, which has a RSHIFT_EXPR as
   its rhs, and try to convert it into a MULT_HIGHPART_EXPR.  The return
   value is true iff we converted the statement.  */

static bool
convert_mult_to_highpart (gassign *stmt, gimple_stmt_iterator *gsi)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree stype = TREE_TYPE (lhs);
  tree sarg0 = gimple_assign_rhs1 (stmt);
  tree sarg1 = gimple_assign_rhs2 (stmt);

  if (TREE_CODE (stype) != INTEGER_TYPE
      || TREE_CODE (sarg1) != INTEGER_CST
      || TREE_CODE (sarg0) != SSA_NAME
      || !tree_fits_uhwi_p (sarg1)
      || !has_single_use (sarg0))
    return false;

  gassign *def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (sarg0));
  if (!def)
    return false;

  enum tree_code mcode = gimple_assign_rhs_code (def);
  if (mcode == NOP_EXPR)
    {
      tree tmp = gimple_assign_rhs1 (def);
      if (TREE_CODE (tmp) != SSA_NAME || !has_single_use (tmp))
	return false;
      def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (tmp));
      if (!def)
	return false;
      mcode = gimple_assign_rhs_code (def);
    }

  if (mcode != WIDEN_MULT_EXPR
      || gimple_bb (def) != gimple_bb (stmt))
    return false;
  tree mtype = TREE_TYPE (gimple_assign_lhs (def));
  if (TREE_CODE (mtype) != INTEGER_TYPE
      || TYPE_PRECISION (mtype) != TYPE_PRECISION (stype))
    return false;

  tree mop1 = gimple_assign_rhs1 (def);
  tree mop2 = gimple_assign_rhs2 (def);
  tree optype = TREE_TYPE (mop1);
  bool unsignedp = TYPE_UNSIGNED (optype);
  unsigned int prec = TYPE_PRECISION (optype);

  if (unsignedp != TYPE_UNSIGNED (mtype)
      || TYPE_PRECISION (mtype) != 2 * prec)
    return false;

  unsigned HOST_WIDE_INT bits = tree_to_uhwi (sarg1);
  if (bits < prec || bits >= 2 * prec)
    return false;

  /* For the time being, require operands to have the same sign.  */
  if (unsignedp != TYPE_UNSIGNED (TREE_TYPE (mop2)))
    return false;

  machine_mode mode = TYPE_MODE (optype);
  optab tab = unsignedp ? umul_highpart_optab : smul_highpart_optab;
  if (optab_handler (tab, mode) == CODE_FOR_nothing)
    return false;

  location_t loc = gimple_location (stmt);
  tree highpart1 = build_and_insert_binop (gsi, loc, "highparttmp",
					   MULT_HIGHPART_EXPR, mop1, mop2);
  tree highpart2 = highpart1;
  tree ntype = optype;

  if (TYPE_UNSIGNED (stype) != TYPE_UNSIGNED (optype))
    {
      ntype = TYPE_UNSIGNED (stype) ? unsigned_type_for (optype)
				    : signed_type_for (optype);
      highpart2 = build_and_insert_cast (gsi, loc, ntype, highpart1);
    }
  if (bits > prec)
    highpart2 = build_and_insert_binop (gsi, loc, "highparttmp",
					RSHIFT_EXPR, highpart2,
					build_int_cst (ntype, bits - prec));

  gassign *new_stmt = gimple_build_assign (lhs, NOP_EXPR, highpart2);
  gsi_replace (gsi, new_stmt, true);

  widen_mul_stats.highpart_mults_inserted++;
  return true;
}

/* If target has spaceship<MODE>3 expander, pattern recognize
   <bb 2> [local count: 1073741824]:
   if (a_2(D) == b_3(D))
     goto <bb 6>; [34.00%]
   else
     goto <bb 3>; [66.00%]

   <bb 3> [local count: 708669601]:
   if (a_2(D) < b_3(D))
     goto <bb 6>; [1.04%]
   else
     goto <bb 4>; [98.96%]

   <bb 4> [local count: 701299439]:
   if (a_2(D) > b_3(D))
     goto <bb 5>; [48.89%]
   else
     goto <bb 6>; [51.11%]

   <bb 5> [local count: 342865295]:

   <bb 6> [local count: 1073741824]:
   and turn it into:
   <bb 2> [local count: 1073741824]:
   _1 = .SPACESHIP (a_2(D), b_3(D), 0);
   if (_1 == 0)
     goto <bb 6>; [34.00%]
   else
     goto <bb 3>; [66.00%]

   <bb 3> [local count: 708669601]:
   if (_1 == -1)
     goto <bb 6>; [1.04%]
   else
     goto <bb 4>; [98.96%]

   <bb 4> [local count: 701299439]:
   if (_1 == 1)
     goto <bb 5>; [48.89%]
   else
     goto <bb 6>; [51.11%]

   <bb 5> [local count: 342865295]:

   <bb 6> [local count: 1073741824]:
   so that the backend can emit optimal comparison and
   conditional jump sequence.  If the
   <bb 6> [local count: 1073741824]:
   above has a single PHI like:
   # _27 = PHI<0(2), -1(3), 2(4), 1(5)>
   then replace it with effectively
   _1 = .SPACESHIP (a_2(D), b_3(D), 1);
   _27 = _1;  */

static void
optimize_spaceship (gcond *stmt)
{
  enum tree_code code = gimple_cond_code (stmt);
  if (code != EQ_EXPR && code != NE_EXPR)
    return;
  tree arg1 = gimple_cond_lhs (stmt);
  tree arg2 = gimple_cond_rhs (stmt);
  if ((!SCALAR_FLOAT_TYPE_P (TREE_TYPE (arg1))
       && !INTEGRAL_TYPE_P (TREE_TYPE (arg1)))
      || optab_handler (spaceship_optab,
			TYPE_MODE (TREE_TYPE (arg1))) == CODE_FOR_nothing
      || operand_equal_p (arg1, arg2, 0))
    return;

  basic_block bb0 = gimple_bb (stmt), bb1, bb2 = NULL;
  edge em1 = NULL, e1 = NULL, e2 = NULL;
  bb1 = EDGE_SUCC (bb0, 1)->dest;
  if (((EDGE_SUCC (bb0, 0)->flags & EDGE_TRUE_VALUE) != 0) ^ (code == EQ_EXPR))
    bb1 = EDGE_SUCC (bb0, 0)->dest;

  gcond *g = safe_dyn_cast <gcond *> (*gsi_last_bb (bb1));
  if (g == NULL
      || !single_pred_p (bb1)
      || (operand_equal_p (gimple_cond_lhs (g), arg1, 0)
	  ? !operand_equal_p (gimple_cond_rhs (g), arg2, 0)
	  : (!operand_equal_p (gimple_cond_lhs (g), arg2, 0)
	     || !operand_equal_p (gimple_cond_rhs (g), arg1, 0)))
      || !cond_only_block_p (bb1))
    return;

  enum tree_code ccode = (operand_equal_p (gimple_cond_lhs (g), arg1, 0)
			  ? LT_EXPR : GT_EXPR);
  switch (gimple_cond_code (g))
    {
    case LT_EXPR:
    case LE_EXPR:
      break;
    case GT_EXPR:
    case GE_EXPR:
      ccode = ccode == LT_EXPR ? GT_EXPR : LT_EXPR;
      break;
    default:
      return;
    }

  for (int i = 0; i < 2; ++i)
    {
      /* With NaNs, </<=/>/>= are false, so we need to look for the
	 third comparison on the false edge from whatever non-equality
	 comparison the second comparison is.  */
      if (HONOR_NANS (TREE_TYPE (arg1))
	  && (EDGE_SUCC (bb1, i)->flags & EDGE_TRUE_VALUE) != 0)
	continue;

      bb2 = EDGE_SUCC (bb1, i)->dest;
      g = safe_dyn_cast <gcond *> (*gsi_last_bb (bb2));
      if (g == NULL
	  || !single_pred_p (bb2)
	  || (operand_equal_p (gimple_cond_lhs (g), arg1, 0)
	      ? !operand_equal_p (gimple_cond_rhs (g), arg2, 0)
	      : (!operand_equal_p (gimple_cond_lhs (g), arg2, 0)
		 || !operand_equal_p (gimple_cond_rhs (g), arg1, 0)))
	  || !cond_only_block_p (bb2)
	  || EDGE_SUCC (bb2, 0)->dest == EDGE_SUCC (bb2, 1)->dest)
	continue;

      enum tree_code ccode2
	= (operand_equal_p (gimple_cond_lhs (g), arg1, 0) ? LT_EXPR : GT_EXPR);
      switch (gimple_cond_code (g))
	{
	case LT_EXPR:
	case LE_EXPR:
	  break;
	case GT_EXPR:
	case GE_EXPR:
	  ccode2 = ccode2 == LT_EXPR ? GT_EXPR : LT_EXPR;
	  break;
	default:
	  continue;
	}
      if (HONOR_NANS (TREE_TYPE (arg1)) && ccode == ccode2)
	continue;

      if ((ccode == LT_EXPR)
	  ^ ((EDGE_SUCC (bb1, i)->flags & EDGE_TRUE_VALUE) != 0))
	{
	  em1 = EDGE_SUCC (bb1, 1 - i);
	  e1 = EDGE_SUCC (bb2, 0);
	  e2 = EDGE_SUCC (bb2, 1);
	  if ((ccode2 == LT_EXPR) ^ ((e1->flags & EDGE_TRUE_VALUE) == 0))
	    std::swap (e1, e2);
	}
      else
	{
	  e1 = EDGE_SUCC (bb1, 1 - i);
	  em1 = EDGE_SUCC (bb2, 0);
	  e2 = EDGE_SUCC (bb2, 1);
	  if ((ccode2 != LT_EXPR) ^ ((em1->flags & EDGE_TRUE_VALUE) == 0))
	    std::swap (em1, e2);
	}
      break;
    }

  if (em1 == NULL)
    {
      if ((ccode == LT_EXPR)
	  ^ ((EDGE_SUCC (bb1, 0)->flags & EDGE_TRUE_VALUE) != 0))
	{
	  em1 = EDGE_SUCC (bb1, 1);
	  e1 = EDGE_SUCC (bb1, 0);
	  e2 = (e1->flags & EDGE_TRUE_VALUE) ? em1 : e1;
	}
      else
	{
	  em1 = EDGE_SUCC (bb1, 0);
	  e1 = EDGE_SUCC (bb1, 1);
	  e2 = (e1->flags & EDGE_TRUE_VALUE) ? em1 : e1;
	}
    }

  /* Check if there is a single bb into which all failed conditions
     jump to (perhaps through an empty block) and if it results in
     a single integral PHI which just sets it to -1, 0, 1, X
     (or -1, 0, 1 when NaNs can't happen).  In that case use 1 rather
     than 0 as last .SPACESHIP argument to tell backends it might
     consider different code generation and just cast the result
     of .SPACESHIP to the PHI result.  X above is some value
     other than -1, 0, 1, for libstdc++ 2, for libc++ -127.  */
  tree arg3 = integer_zero_node;
  edge e = EDGE_SUCC (bb0, 0);
  if (e->dest == bb1)
    e = EDGE_SUCC (bb0, 1);
  basic_block bbp = e->dest;
  gphi *phi = NULL;
  for (gphi_iterator psi = gsi_start_phis (bbp);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *gp = psi.phi ();
      tree res = gimple_phi_result (gp);

      if (phi != NULL
	  || virtual_operand_p (res)
	  || !INTEGRAL_TYPE_P (TREE_TYPE (res))
	  || TYPE_PRECISION (TREE_TYPE (res)) < 2)
	{
	  phi = NULL;
	  break;
	}
      phi = gp;
    }
  if (phi
      && integer_zerop (gimple_phi_arg_def_from_edge (phi, e))
      && EDGE_COUNT (bbp->preds) == (HONOR_NANS (TREE_TYPE (arg1)) ? 4 : 3))
    {
      HOST_WIDE_INT argval = SCALAR_FLOAT_TYPE_P (TREE_TYPE (arg1)) ? 2 : -1;
      for (unsigned i = 0; phi && i < EDGE_COUNT (bbp->preds) - 1; ++i)
	{
	  edge e3 = i == 0 ? e1 : i == 1 ? em1 : e2;
	  if (e3->dest != bbp)
	    {
	      if (!empty_block_p (e3->dest)
		  || !single_succ_p (e3->dest)
		  || single_succ (e3->dest) != bbp)
		{
		  phi = NULL;
		  break;
		}
	      e3 = single_succ_edge (e3->dest);
	    }
	  tree a = gimple_phi_arg_def_from_edge (phi, e3);
	  if (TREE_CODE (a) != INTEGER_CST
	      || (i == 0 && !integer_onep (a))
	      || (i == 1 && !integer_all_onesp (a)))
	    {
	      phi = NULL;
	      break;
	    }
	  if (i == 2)
	    {
	      tree minv = TYPE_MIN_VALUE (signed_char_type_node);
	      tree maxv = TYPE_MAX_VALUE (signed_char_type_node);
	      widest_int w = widest_int::from (wi::to_wide (a), SIGNED);
	      if ((w >= -1 && w <= 1)
		  || w < wi::to_widest (minv)
		  || w > wi::to_widest (maxv))
		{
		  phi = NULL;
		  break;
		}
	      argval = w.to_shwi ();
	    }
	}
      if (phi)
	arg3 = build_int_cst (integer_type_node,
			      TYPE_UNSIGNED (TREE_TYPE (arg1)) ? 1 : argval);
    }

  /* For integral <=> comparisons only use .SPACESHIP if it is turned
     into an integer (-1, 0, 1).  */
  if (!SCALAR_FLOAT_TYPE_P (TREE_TYPE (arg1)) && arg3 == integer_zero_node)
    return;

  gcall *gc = gimple_build_call_internal (IFN_SPACESHIP, 3, arg1, arg2, arg3);
  tree lhs = make_ssa_name (integer_type_node);
  gimple_call_set_lhs (gc, lhs);
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gsi_insert_before (&gsi, gc, GSI_SAME_STMT);

  wide_int wmin = wi::minus_one (TYPE_PRECISION (integer_type_node));
  wide_int wmax = wi::one (TYPE_PRECISION (integer_type_node));
  if (HONOR_NANS (TREE_TYPE (arg1)))
    {
      if (arg3 == integer_zero_node)
	wmax = wi::two (TYPE_PRECISION (integer_type_node));
      else if (tree_int_cst_sgn (arg3) < 0)
	wmin = wi::to_wide (arg3);
      else
	wmax = wi::to_wide (arg3);
    }
  int_range<1> vr (TREE_TYPE (lhs), wmin, wmax);
  set_range_info (lhs, vr);

  if (arg3 != integer_zero_node)
    {
      tree type = TREE_TYPE (gimple_phi_result (phi));
      if (!useless_type_conversion_p (type, integer_type_node))
	{
	  tree tem = make_ssa_name (type);
	  gimple *gcv = gimple_build_assign (tem, NOP_EXPR, lhs);
	  gsi_insert_before (&gsi, gcv, GSI_SAME_STMT);
	  lhs = tem;
	}
      SET_PHI_ARG_DEF_ON_EDGE (phi, e, lhs);
      gimple_cond_set_lhs (stmt, boolean_false_node);
      gimple_cond_set_rhs (stmt, boolean_false_node);
      gimple_cond_set_code (stmt, (e->flags & EDGE_TRUE_VALUE)
				  ? EQ_EXPR : NE_EXPR);
      update_stmt (stmt);
      return;
    }

  gimple_cond_set_lhs (stmt, lhs);
  gimple_cond_set_rhs (stmt, integer_zero_node);
  update_stmt (stmt);

  gcond *cond = as_a <gcond *> (*gsi_last_bb (bb1));
  gimple_cond_set_lhs (cond, lhs);
  if (em1->src == bb1 && e2 != em1)
    {
      gimple_cond_set_rhs (cond, integer_minus_one_node);
      gimple_cond_set_code (cond, (em1->flags & EDGE_TRUE_VALUE)
				  ? EQ_EXPR : NE_EXPR);
    }
  else
    {
      gcc_assert (e1->src == bb1 && e2 != e1);
      gimple_cond_set_rhs (cond, integer_one_node);
      gimple_cond_set_code (cond, (e1->flags & EDGE_TRUE_VALUE)
				  ? EQ_EXPR : NE_EXPR);
    }
  update_stmt (cond);

  if (e2 != e1 && e2 != em1)
    {
      cond = as_a <gcond *> (*gsi_last_bb (bb2));
      gimple_cond_set_lhs (cond, lhs);
      if (em1->src == bb2)
	gimple_cond_set_rhs (cond, integer_minus_one_node);
      else
	{
	  gcc_assert (e1->src == bb2);
	  gimple_cond_set_rhs (cond, integer_one_node);
	}
      gimple_cond_set_code (cond,
			    (e2->flags & EDGE_TRUE_VALUE) ? NE_EXPR : EQ_EXPR);
      update_stmt (cond);
    }
}


/* Find integer multiplications where the operands are extended from
   smaller types, and replace the MULT_EXPR with a WIDEN_MULT_EXPR
   or MULT_HIGHPART_EXPR where appropriate.  */

namespace {

const pass_data pass_data_optimize_widening_mul =
{
  GIMPLE_PASS, /* type */
  "widening_mul", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_WIDEN_MUL, /* tv_id */
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
  bool gate (function *) final override
    {
      return flag_expensive_optimizations && optimize;
    }

  unsigned int execute (function *) final override;

}; // class pass_optimize_widening_mul

/* Walker class to perform the transformation in reverse dominance order. */

class math_opts_dom_walker : public dom_walker
{
public:
  /* Constructor, CFG_CHANGED is a pointer to a boolean flag that will be set
     if walking modidifes the CFG.  */

  math_opts_dom_walker (bool *cfg_changed_p)
    : dom_walker (CDI_DOMINATORS), m_last_result_set (),
      m_cfg_changed_p (cfg_changed_p) {}

  /* The actual actions performed in the walk.  */

  void after_dom_children (basic_block) final override;

  /* Set of results of chains of multiply and add statement combinations that
     were not transformed into FMAs because of active deferring.  */
  hash_set<tree> m_last_result_set;

  /* Pointer to a flag of the user that needs to be set if CFG has been
     modified.  */
  bool *m_cfg_changed_p;
};

void
math_opts_dom_walker::after_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;

  fma_deferring_state fma_state (param_avoid_fma_max_bits > 0);

  for (gphi_iterator psi_next, psi = gsi_start_phis (bb); !gsi_end_p (psi);
       psi = psi_next)
    {
      psi_next = psi;
      gsi_next (&psi_next);

      gimple_stmt_iterator gsi = gsi_after_labels (bb);
      gphi *phi = psi.phi ();

      if (match_saturation_add (&gsi, phi)
	  || match_saturation_sub (&gsi, phi)
	  || match_saturation_trunc (&gsi, phi))
	remove_phi_node (&psi, /* release_lhs_p */ false);
    }

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
					  gimple_assign_rhs2 (stmt),
					  &fma_state))
		{
		  gsi_remove (&gsi, true);
		  release_defs (stmt);
		  continue;
		}
	      match_arith_overflow (&gsi, stmt, code, m_cfg_changed_p);
	      match_unsigned_saturation_sub (&gsi, as_a<gassign *> (stmt));
	      break;

	    case PLUS_EXPR:
	      match_unsigned_saturation_add (&gsi, as_a<gassign *> (stmt));
	      match_unsigned_saturation_sub (&gsi, as_a<gassign *> (stmt));
	      /* fall-through  */
	    case MINUS_EXPR:
	      if (!convert_plusminus_to_widen (&gsi, stmt, code))
		{
		  match_arith_overflow (&gsi, stmt, code, m_cfg_changed_p);
		  if (gsi_stmt (gsi) == stmt)
		    match_uaddc_usubc (&gsi, stmt, code);
		}
	      break;

	    case BIT_NOT_EXPR:
	      if (match_arith_overflow (&gsi, stmt, code, m_cfg_changed_p))
		continue;
	      break;

	    case TRUNC_MOD_EXPR:
	      convert_to_divmod (as_a<gassign *> (stmt));
	      break;

	    case RSHIFT_EXPR:
	      convert_mult_to_highpart (as_a<gassign *> (stmt), &gsi);
	      break;

	    case BIT_IOR_EXPR:
	      match_unsigned_saturation_add (&gsi, as_a<gassign *> (stmt));
	      match_unsigned_saturation_trunc (&gsi, as_a<gassign *> (stmt));
	      /* fall-through  */
	    case BIT_XOR_EXPR:
	      match_uaddc_usubc (&gsi, stmt, code);
	      break;

	    case EQ_EXPR:
	    case NE_EXPR:
	    case LE_EXPR:
	    case GT_EXPR:
	      match_single_bit_test (&gsi, stmt);
	      break;

	    case COND_EXPR:
	    case BIT_AND_EXPR:
	      match_unsigned_saturation_sub (&gsi, as_a<gassign *> (stmt));
	      break;

	    case NOP_EXPR:
	      match_unsigned_saturation_trunc (&gsi, as_a<gassign *> (stmt));
	      break;

	    default:;
	    }
	}
      else if (is_gimple_call (stmt))
	{
	  switch (gimple_call_combined_fn (stmt))
	    {
	    CASE_CFN_POW:
	      if (gimple_call_lhs (stmt)
		  && TREE_CODE (gimple_call_arg (stmt, 1)) == REAL_CST
		  && real_equal (&TREE_REAL_CST (gimple_call_arg (stmt, 1)),
				 &dconst2)
		  && convert_mult_to_fma (stmt,
					  gimple_call_arg (stmt, 0),
					  gimple_call_arg (stmt, 0),
					  &fma_state))
		{
		  unlink_stmt_vdef (stmt);
		  if (gsi_remove (&gsi, true)
		      && gimple_purge_dead_eh_edges (bb))
		    *m_cfg_changed_p = true;
		  release_defs (stmt);
		  continue;
		}
	      break;

	    case CFN_COND_MUL:
	      if (convert_mult_to_fma (stmt,
				       gimple_call_arg (stmt, 1),
				       gimple_call_arg (stmt, 2),
				       &fma_state,
				       gimple_call_arg (stmt, 0)))

		{
		  gsi_remove (&gsi, true);
		  release_defs (stmt);
		  continue;
		}
	      break;

	    case CFN_COND_LEN_MUL:
	      if (convert_mult_to_fma (stmt,
				       gimple_call_arg (stmt, 1),
				       gimple_call_arg (stmt, 2),
				       &fma_state,
				       gimple_call_arg (stmt, 0),
				       gimple_call_arg (stmt, 4),
				       gimple_call_arg (stmt, 5)))

		{
		  gsi_remove (&gsi, true);
		  release_defs (stmt);
		  continue;
		}
	      break;

	    case CFN_LAST:
	      cancel_fma_deferring (&fma_state);
	      break;

	    default:
	      break;
	    }
	}
      else if (gimple_code (stmt) == GIMPLE_COND)
	{
	  match_single_bit_test (&gsi, stmt);
	  optimize_spaceship (as_a <gcond *> (stmt));
	}
      gsi_next (&gsi);
    }
  if (fma_state.m_deferring_p
      && fma_state.m_initial_phi)
    {
      gcc_checking_assert (fma_state.m_last_result);
      if (!last_fma_candidate_feeds_initial_phi (&fma_state,
						 &m_last_result_set))
	cancel_fma_deferring (&fma_state);
      else
	m_last_result_set.add (fma_state.m_last_result);
    }
}


unsigned int
pass_optimize_widening_mul::execute (function *fun)
{
  bool cfg_changed = false;

  memset (&widen_mul_stats, 0, sizeof (widen_mul_stats));
  calculate_dominance_info (CDI_DOMINATORS);
  renumber_gimple_stmt_uids (cfun);

  math_opts_dom_walker (&cfg_changed).walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  statistics_counter_event (fun, "widening multiplications inserted",
			    widen_mul_stats.widen_mults_inserted);
  statistics_counter_event (fun, "widening maccs inserted",
			    widen_mul_stats.maccs_inserted);
  statistics_counter_event (fun, "fused multiply-adds inserted",
			    widen_mul_stats.fmas_inserted);
  statistics_counter_event (fun, "divmod calls inserted",
			    widen_mul_stats.divmod_calls_inserted);
  statistics_counter_event (fun, "highpart multiplications inserted",
			    widen_mul_stats.highpart_mults_inserted);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_optimize_widening_mul (gcc::context *ctxt)
{
  return new pass_optimize_widening_mul (ctxt);
}
