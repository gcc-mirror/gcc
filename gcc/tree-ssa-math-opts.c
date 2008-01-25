/* Global, SSA-based optimizations using mathematical identities.
   Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.
   
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
#include "tree.h"
#include "tree-flow.h"
#include "real.h"
#include "timevar.h"
#include "tree-pass.h"
#include "alloc-pool.h"
#include "basic-block.h"
#include "target.h"


/* This structure represents one basic block that either computes a
   division, or is a common dominator for basic block that compute a
   division.  */
struct occurrence {
  /* The basic block represented by this structure.  */
  basic_block bb;

  /* If non-NULL, the SSA_NAME holding the definition for a reciprocal
     inserted in BB.  */
  tree recip_def;

  /* If non-NULL, the GIMPLE_MODIFY_STMT for a reciprocal computation that
     was inserted in BB.  */
  tree recip_def_stmt;

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
      insert_bb (occ, ENTRY_BLOCK_PTR, &occ_head);
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
is_division_by (tree use_stmt, tree def)
{
  return TREE_CODE (use_stmt) == GIMPLE_MODIFY_STMT
	 && TREE_CODE (GIMPLE_STMT_OPERAND (use_stmt, 1)) == RDIV_EXPR
	 && TREE_OPERAND (GIMPLE_STMT_OPERAND (use_stmt, 1), 1) == def
	 /* Do not recognize x / x as valid division, as we are getting
	    confused later by replacing all immediate uses x in such
	    a stmt.  */
	 && TREE_OPERAND (GIMPLE_STMT_OPERAND (use_stmt, 1), 0) != def;
}

/* Walk the subset of the dominator tree rooted at OCC, setting the
   RECIP_DEF field to a definition of 1.0 / DEF that can be used in
   the given basic block.  The field may be left NULL, of course,
   if it is not possible or profitable to do the optimization.

   DEF_BSI is an iterator pointing at the statement defining DEF.
   If RECIP_DEF is set, a dominator already has a computation that can
   be used.  */

static void
insert_reciprocals (block_stmt_iterator *def_bsi, struct occurrence *occ,
		    tree def, tree recip_def, int threshold)
{
  tree type, new_stmt;
  block_stmt_iterator bsi;
  struct occurrence *occ_child;

  if (!recip_def
      && (occ->bb_has_division || !flag_trapping_math)
      && occ->num_divisions >= threshold)
    {
      /* Make a variable with the replacement and substitute it.  */
      type = TREE_TYPE (def);
      recip_def = make_rename_temp (type, "reciptmp");
      new_stmt = build_gimple_modify_stmt (recip_def,
					   fold_build2 (RDIV_EXPR, type,
							build_one_cst (type),
							def));
  
  
      if (occ->bb_has_division)
        {
          /* Case 1: insert before an existing division.  */
          bsi = bsi_after_labels (occ->bb);
          while (!bsi_end_p (bsi) && !is_division_by (bsi_stmt (bsi), def))
	    bsi_next (&bsi);

          bsi_insert_before (&bsi, new_stmt, BSI_SAME_STMT);
        }
      else if (def_bsi && occ->bb == def_bsi->bb)
        {
          /* Case 2: insert right after the definition.  Note that this will
	     never happen if the definition statement can throw, because in
	     that case the sole successor of the statement's basic block will
	     dominate all the uses as well.  */
          bsi_insert_after (def_bsi, new_stmt, BSI_NEW_STMT);
        }
      else
        {
          /* Case 3: insert in a basic block not containing defs/uses.  */
          bsi = bsi_after_labels (occ->bb);
          bsi_insert_before (&bsi, new_stmt, BSI_SAME_STMT);
        }

      occ->recip_def_stmt = new_stmt;
    }

  occ->recip_def = recip_def;
  for (occ_child = occ->children; occ_child; occ_child = occ_child->next)
    insert_reciprocals (def_bsi, occ_child, def, recip_def, threshold);
}


/* Replace the division at USE_P with a multiplication by the reciprocal, if
   possible.  */

static inline void
replace_reciprocal (use_operand_p use_p)
{
  tree use_stmt = USE_STMT (use_p);
  basic_block bb = bb_for_stmt (use_stmt);
  struct occurrence *occ = (struct occurrence *) bb->aux;

  if (occ->recip_def && use_stmt != occ->recip_def_stmt)
    {
      TREE_SET_CODE (GIMPLE_STMT_OPERAND (use_stmt, 1), MULT_EXPR);
      SET_USE (use_p, occ->recip_def);
      fold_stmt_inplace (use_stmt);
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
execute_cse_reciprocals_1 (block_stmt_iterator *def_bsi, tree def)
{
  use_operand_p use_p;
  imm_use_iterator use_iter;
  struct occurrence *occ;
  int count = 0, threshold;

  gcc_assert (FLOAT_TYPE_P (TREE_TYPE (def)) && is_gimple_reg (def));

  FOR_EACH_IMM_USE_FAST (use_p, use_iter, def)
    {
      tree use_stmt = USE_STMT (use_p);
      if (is_division_by (use_stmt, def))
	{
	  register_division_in (bb_for_stmt (use_stmt));
	  count++;
	}
    }
  
  /* Do the expensive part only if we can hope to optimize something.  */
  threshold = targetm.min_divisions_for_recip_mul (TYPE_MODE (TREE_TYPE (def)));
  if (count >= threshold)
    {
      tree use_stmt;
      for (occ = occ_head; occ; occ = occ->next)
	{
	  compute_merit (occ);
	  insert_reciprocals (def_bsi, occ, def, NULL, threshold);
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

static bool
gate_cse_reciprocals (void)
{
  return optimize && !optimize_size && flag_reciprocal_math;
}

/* Go through all the floating-point SSA_NAMEs, and call
   execute_cse_reciprocals_1 on each of them.  */
static unsigned int
execute_cse_reciprocals (void)
{
  basic_block bb;
  tree arg;

  occ_pool = create_alloc_pool ("dominators for recip",
				sizeof (struct occurrence),
				n_basic_blocks / 3 + 1);

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

#ifdef ENABLE_CHECKING
  FOR_EACH_BB (bb)
    gcc_assert (!bb->aux);
#endif

  for (arg = DECL_ARGUMENTS (cfun->decl); arg; arg = TREE_CHAIN (arg))
    if (gimple_default_def (cfun, arg)
	&& FLOAT_TYPE_P (TREE_TYPE (arg))
	&& is_gimple_reg (arg))
      execute_cse_reciprocals_1 (NULL, gimple_default_def (cfun, arg));

  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi;
      tree phi, def;

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  def = PHI_RESULT (phi);
	  if (FLOAT_TYPE_P (TREE_TYPE (def))
	      && is_gimple_reg (def))
	    execute_cse_reciprocals_1 (NULL, def);
	}

      for (bsi = bsi_after_labels (bb); !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  tree stmt = bsi_stmt (bsi);

	  if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT
	      && (def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF)) != NULL
	      && FLOAT_TYPE_P (TREE_TYPE (def))
	      && TREE_CODE (def) == SSA_NAME)
	    execute_cse_reciprocals_1 (&bsi, def);
	}

      /* Scan for a/func(b) and convert it to reciprocal a*rfunc(b).  */
      for (bsi = bsi_after_labels (bb); !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  tree stmt = bsi_stmt (bsi);
	  tree fndecl;

	  if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT
	      && TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 1)) == RDIV_EXPR)
	    {
	      tree arg1 = TREE_OPERAND (GIMPLE_STMT_OPERAND (stmt, 1), 1);
	      tree stmt1;

	      if (TREE_CODE (arg1) != SSA_NAME)
		continue;

	      stmt1 = SSA_NAME_DEF_STMT (arg1);

	      if (TREE_CODE (stmt1) == GIMPLE_MODIFY_STMT
		  && TREE_CODE (GIMPLE_STMT_OPERAND (stmt1, 1)) == CALL_EXPR
		  && (fndecl
		      = get_callee_fndecl (GIMPLE_STMT_OPERAND (stmt1, 1)))
		  && (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
		      || DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD))
		{
		  enum built_in_function code;
		  bool md_code;
		  tree arg10;
		  tree tmp;

		  code = DECL_FUNCTION_CODE (fndecl);
		  md_code = DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD;

		  fndecl = targetm.builtin_reciprocal (code, md_code, false);
		  if (!fndecl)
		    continue;

		  arg10 = CALL_EXPR_ARG (GIMPLE_STMT_OPERAND (stmt1, 1), 0);
		  tmp = build_call_expr (fndecl, 1, arg10);
		  GIMPLE_STMT_OPERAND (stmt1, 1) = tmp;
		  update_stmt (stmt1);

		  TREE_SET_CODE (GIMPLE_STMT_OPERAND (stmt, 1), MULT_EXPR);
		  fold_stmt_inplace (stmt);
		  update_stmt (stmt);
		}
	    }
	}
    }

  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  free_alloc_pool (occ_pool);
  return 0;
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

/* Records an occurrence at statement USE_STMT in the vector of trees
   STMTS if it is dominated by *TOP_BB or dominates it or this basic block
   is not yet initialized.  Returns true if the occurrence was pushed on
   the vector.  Adjusts *TOP_BB to be the basic block dominating all
   statements in the vector.  */

static bool
maybe_record_sincos (VEC(tree, heap) **stmts,
		     basic_block *top_bb, tree use_stmt)
{
  basic_block use_bb = bb_for_stmt (use_stmt);
  if (*top_bb
      && (*top_bb == use_bb
	  || dominated_by_p (CDI_DOMINATORS, use_bb, *top_bb)))
    VEC_safe_push (tree, heap, *stmts, use_stmt);
  else if (!*top_bb
	   || dominated_by_p (CDI_DOMINATORS, *top_bb, use_bb))
    {
      VEC_safe_push (tree, heap, *stmts, use_stmt);
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

static void
execute_cse_sincos_1 (tree name)
{
  block_stmt_iterator bsi;
  imm_use_iterator use_iter;
  tree def_stmt, use_stmt, fndecl, res, call, stmt, type;
  int seen_cos = 0, seen_sin = 0, seen_cexpi = 0;
  VEC(tree, heap) *stmts = NULL;
  basic_block top_bb = NULL;
  int i;

  type = TREE_TYPE (name);
  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, name)
    {
      if (TREE_CODE (use_stmt) != GIMPLE_MODIFY_STMT
	  || TREE_CODE (GIMPLE_STMT_OPERAND (use_stmt, 1)) != CALL_EXPR
	  || !(fndecl = get_callee_fndecl (GIMPLE_STMT_OPERAND (use_stmt, 1)))
	  || DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_NORMAL)
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
    {
      VEC_free(tree, heap, stmts);
      return;
    }

  /* Simply insert cexpi at the beginning of top_bb but not earlier than
     the name def statement.  */
  fndecl = mathfn_built_in (type, BUILT_IN_CEXPI);
  if (!fndecl)
    return;
  res = make_rename_temp (TREE_TYPE (TREE_TYPE (fndecl)), "sincostmp");
  call = build_call_expr (fndecl, 1, name);
  stmt = build_gimple_modify_stmt (res, call);
  def_stmt = SSA_NAME_DEF_STMT (name);
  if (!SSA_NAME_IS_DEFAULT_DEF (name)
      && TREE_CODE (def_stmt) != PHI_NODE
      && bb_for_stmt (def_stmt) == top_bb)
    {
      bsi = bsi_for_stmt (def_stmt);
      bsi_insert_after (&bsi, stmt, BSI_SAME_STMT);
    }
  else
    {
      bsi = bsi_after_labels (top_bb);
      bsi_insert_before (&bsi, stmt, BSI_SAME_STMT);
    }
  update_stmt (stmt);

  /* And adjust the recorded old call sites.  */
  for (i = 0; VEC_iterate(tree, stmts, i, use_stmt); ++i)
    {
      fndecl = get_callee_fndecl (GIMPLE_STMT_OPERAND (use_stmt, 1));
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	CASE_FLT_FN (BUILT_IN_COS):
	  GIMPLE_STMT_OPERAND (use_stmt, 1) = fold_build1 (REALPART_EXPR,
							   type, res);
	  break;

	CASE_FLT_FN (BUILT_IN_SIN):
	  GIMPLE_STMT_OPERAND (use_stmt, 1) = fold_build1 (IMAGPART_EXPR,
							   type, res);
	  break;

	CASE_FLT_FN (BUILT_IN_CEXPI):
	  GIMPLE_STMT_OPERAND (use_stmt, 1) = res;
	  break;

	default:;
	  gcc_unreachable ();
	}

	update_stmt (use_stmt);
    }

  VEC_free(tree, heap, stmts);
}

/* Go through all calls to sin, cos and cexpi and call execute_cse_sincos_1
   on the SSA_NAME argument of each of them.  */

static unsigned int
execute_cse_sincos (void)
{
  basic_block bb;

  calculate_dominance_info (CDI_DOMINATORS);

  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi;

      for (bsi = bsi_after_labels (bb); !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  tree stmt = bsi_stmt (bsi);
	  tree fndecl;

	  if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT
	      && TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 1)) == CALL_EXPR
	      && (fndecl = get_callee_fndecl (GIMPLE_STMT_OPERAND (stmt, 1)))
	      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	    {
	      tree arg;

	      switch (DECL_FUNCTION_CODE (fndecl))
		{
		CASE_FLT_FN (BUILT_IN_COS):
		CASE_FLT_FN (BUILT_IN_SIN):
		CASE_FLT_FN (BUILT_IN_CEXPI):
		  arg = GIMPLE_STMT_OPERAND (stmt, 1);
		  arg = CALL_EXPR_ARG (arg, 0);
		  if (TREE_CODE (arg) == SSA_NAME)
		    execute_cse_sincos_1 (arg);
		  break;

		default:;
		}
	    }
	}
    }

  free_dominance_info (CDI_DOMINATORS);
  return 0;
}

static bool
gate_cse_sincos (void)
{
  /* Make sure we have either sincos or cexp.  */
  return (TARGET_HAS_SINCOS
	  || TARGET_C99_FUNCTIONS)
	 && optimize;
}

struct tree_opt_pass pass_cse_sincos =
{
  "sincos",				/* name */
  gate_cse_sincos,			/* gate */
  execute_cse_sincos,			/* execute */
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

/* Find all expressions in the form of sqrt(a/b) and
   convert them to rsqrt(b/a).  */

static unsigned int
execute_convert_to_rsqrt (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi;

      for (bsi = bsi_after_labels (bb); !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  tree stmt = bsi_stmt (bsi);
	  tree fndecl;

	  if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT
	      && TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 1)) == CALL_EXPR
	      && (fndecl = get_callee_fndecl (GIMPLE_STMT_OPERAND (stmt, 1)))
	      && (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
		  || DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD))
	    {
	      enum built_in_function code;
	      bool md_code;
	      tree arg1;
	      tree stmt1;

	      code = DECL_FUNCTION_CODE (fndecl);
	      md_code = DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD;

	      fndecl = targetm.builtin_reciprocal (code, md_code, true);
	      if (!fndecl)
		continue;

	      arg1 = CALL_EXPR_ARG (GIMPLE_STMT_OPERAND (stmt, 1), 0);

	      if (TREE_CODE (arg1) != SSA_NAME)
		continue;

	      stmt1 = SSA_NAME_DEF_STMT (arg1);

	      if (TREE_CODE (stmt1) == GIMPLE_MODIFY_STMT
		  && TREE_CODE (GIMPLE_STMT_OPERAND (stmt1, 1)) == RDIV_EXPR)
		{
		  tree arg10, arg11;
		  tree tmp;

		  arg10 = TREE_OPERAND (GIMPLE_STMT_OPERAND (stmt1, 1), 0);
		  arg11 = TREE_OPERAND (GIMPLE_STMT_OPERAND (stmt1, 1), 1);

		  /* Swap operands of RDIV_EXPR.  */
		  TREE_OPERAND (GIMPLE_STMT_OPERAND (stmt1, 1), 0) = arg11;
		  TREE_OPERAND (GIMPLE_STMT_OPERAND (stmt1, 1), 1) = arg10;
		  fold_stmt_inplace (stmt1);
		  update_stmt (stmt1);

		  tmp = build_call_expr (fndecl, 1, arg1);
		  GIMPLE_STMT_OPERAND (stmt, 1) = tmp;
		  update_stmt (stmt);
		}
	    }
	}
    }

  return 0;
}

static bool
gate_convert_to_rsqrt (void)
{
  return flag_unsafe_math_optimizations && optimize;
}

struct tree_opt_pass pass_convert_to_rsqrt =
{
  "rsqrt",				/* name */
  gate_convert_to_rsqrt,		/* gate */
  execute_convert_to_rsqrt,		/* execute */
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
