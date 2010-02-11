/* Detection of Static Control Parts (SCoP) for Graphite.
   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Tobias Grosser <grosser@fim.uni-passau.de>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "toplev.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "domwalk.h"
#include "value-prof.h"
#include "pointer-set.h"
#include "gimple.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "cloog/cloog.h"
#include "ppl_c.h"
#include "graphite-ppl.h"
#include "graphite.h"
#include "graphite-poly.h"
#include "graphite-scop-detection.h"

/* The type of the analyzed basic block.  */

typedef enum gbb_type {
  GBB_UNKNOWN,
  GBB_LOOP_SING_EXIT_HEADER,
  GBB_LOOP_MULT_EXIT_HEADER,
  GBB_LOOP_EXIT,
  GBB_COND_HEADER,
  GBB_SIMPLE,
  GBB_LAST
} gbb_type;

/* Detect the type of BB.  Loop headers are only marked, if they are
   new.  This means their loop_father is different to LAST_LOOP.
   Otherwise they are treated like any other bb and their type can be
   any other type.  */

static gbb_type
get_bb_type (basic_block bb, struct loop *last_loop)
{
  VEC (basic_block, heap) *dom;
  int nb_dom, nb_suc;
  struct loop *loop = bb->loop_father;

  /* Check, if we entry into a new loop. */
  if (loop != last_loop)
    {
      if (single_exit (loop) != NULL)
        return GBB_LOOP_SING_EXIT_HEADER;
      else if (loop->num != 0)
        return GBB_LOOP_MULT_EXIT_HEADER;
      else
	return GBB_COND_HEADER;
    }

  dom = get_dominated_by (CDI_DOMINATORS, bb);
  nb_dom = VEC_length (basic_block, dom);
  VEC_free (basic_block, heap, dom);

  if (nb_dom == 0)
    return GBB_LAST;

  nb_suc = VEC_length (edge, bb->succs);

  if (nb_dom == 1 && nb_suc == 1)
    return GBB_SIMPLE;

  return GBB_COND_HEADER;
}

/* A SCoP detection region, defined using bbs as borders.

   All control flow touching this region, comes in passing basic_block
   ENTRY and leaves passing basic_block EXIT.  By using bbs instead of
   edges for the borders we are able to represent also regions that do
   not have a single entry or exit edge.

   But as they have a single entry basic_block and a single exit
   basic_block, we are able to generate for every sd_region a single
   entry and exit edge.

   1   2
    \ /
     3	<- entry
     |
     4
    / \			This region contains: {3, 4, 5, 6, 7, 8}
   5   6
   |   |
   7   8
    \ /
     9	<- exit  */


typedef struct sd_region_p
{
  /* The entry bb dominates all bbs in the sd_region.  It is part of
     the region.  */
  basic_block entry;

  /* The exit bb postdominates all bbs in the sd_region, but is not
     part of the region.  */
  basic_block exit;
} sd_region;

DEF_VEC_O(sd_region);
DEF_VEC_ALLOC_O(sd_region, heap);


/* Moves the scops from SOURCE to TARGET and clean up SOURCE.  */

static void
move_sd_regions (VEC (sd_region, heap) **source,
		 VEC (sd_region, heap) **target)
{
  sd_region *s;
  int i;

  for (i = 0; VEC_iterate (sd_region, *source, i, s); i++)
    VEC_safe_push (sd_region, heap, *target, s);

  VEC_free (sd_region, heap, *source);
}

/* Something like "n * m" is not allowed.  */

static bool
graphite_can_represent_init (tree e)
{
  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      return graphite_can_represent_init (CHREC_LEFT (e))
	&& graphite_can_represent_init (CHREC_RIGHT (e));

    case MULT_EXPR:
      if (chrec_contains_symbols (TREE_OPERAND (e, 0)))
	return graphite_can_represent_init (TREE_OPERAND (e, 0))
	  && host_integerp (TREE_OPERAND (e, 1), 0);
      else
	return graphite_can_represent_init (TREE_OPERAND (e, 1))
	  && host_integerp (TREE_OPERAND (e, 0), 0);

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return graphite_can_represent_init (TREE_OPERAND (e, 0))
	&& graphite_can_represent_init (TREE_OPERAND (e, 1));

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      return graphite_can_represent_init (TREE_OPERAND (e, 0));

   default:
     break;
    }

  return true;
}

/* Return true when SCEV can be represented in the polyhedral model.

   An expression can be represented, if it can be expressed as an
   affine expression.  For loops (i, j) and parameters (m, n) all
   affine expressions are of the form:

   x1 * i + x2 * j + x3 * m + x4 * n + x5 * 1 where x1..x5 element of Z

   1 i + 20 j + (-2) m + 25

   Something like "i * n" or "n * m" is not allowed.

   OUTERMOST_LOOP defines the outermost loop that can variate.  */

static bool
graphite_can_represent_scev (tree scev, int outermost_loop)
{
  if (chrec_contains_undetermined (scev))
    return false;

  switch (TREE_CODE (scev))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      return graphite_can_represent_scev (TREE_OPERAND (scev, 0), outermost_loop)
	&& graphite_can_represent_scev (TREE_OPERAND (scev, 1), outermost_loop);

    case MULT_EXPR:
      return !CONVERT_EXPR_CODE_P (TREE_CODE (TREE_OPERAND (scev, 0)))
	&& !CONVERT_EXPR_CODE_P (TREE_CODE (TREE_OPERAND (scev, 1)))
	&& !(chrec_contains_symbols (TREE_OPERAND (scev, 0))
	     && chrec_contains_symbols (TREE_OPERAND (scev, 1)))
	&& graphite_can_represent_init (scev)
	&& graphite_can_represent_scev (TREE_OPERAND (scev, 0), outermost_loop)
	&& graphite_can_represent_scev (TREE_OPERAND (scev, 1), outermost_loop);

    case POLYNOMIAL_CHREC:
      /* Check for constant strides.  With a non constant stride of
	 'n' we would have a value of 'iv * n'.  Also check that the
	 initial value can represented: for example 'n * m' cannot be
	 represented.  */
      if (!evolution_function_right_is_integer_cst (scev)
	  || !graphite_can_represent_init (scev))
	return false;

    default:
      break;
    }

  /* Only affine functions can be represented.  */
  if (!scev_is_linear_expression (scev))
    return false;

  return evolution_function_is_invariant_p (scev, outermost_loop)
    || evolution_function_is_affine_multivariate_p (scev, outermost_loop);
}


/* Return true when EXPR can be represented in the polyhedral model.

   This means an expression can be represented, if it is linear with
   respect to the loops and the strides are non parametric.
   LOOP is the place where the expr will be evaluated and OUTERMOST_LOOP
   defindes the outermost loop that can variate.  SCOP_ENTRY defines the
   entry of the region we analyse.  */

static bool
graphite_can_represent_expr (basic_block scop_entry, loop_p loop,
			     loop_p outermost_loop, tree expr)
{
  tree scev = analyze_scalar_evolution (loop, expr);

  scev = instantiate_scev (scop_entry, loop, scev);

  return graphite_can_represent_scev (scev, outermost_loop->num);
}

/* Return true if the data references of STMT can be represented by
   Graphite.  */

static bool
stmt_has_simple_data_refs_p (loop_p outermost_loop, gimple stmt)
{
  data_reference_p dr;
  unsigned i;
  int j;
  bool res = true;
  int loop = outermost_loop->num;
  VEC (data_reference_p, heap) *drs = VEC_alloc (data_reference_p, heap, 5);

  graphite_find_data_references_in_stmt (outermost_loop, stmt, &drs);

  for (j = 0; VEC_iterate (data_reference_p, drs, j, dr); j++)
    for (i = 0; i < DR_NUM_DIMENSIONS (dr); i++)
      if (!graphite_can_represent_scev (DR_ACCESS_FN (dr, i), loop))
	{
	  res = false;
	  goto done;
	}

 done:
  free_data_refs (drs);
  return res;
}

/* Return true only when STMT is simple enough for being handled by
   Graphite.  This depends on SCOP_ENTRY, as the parameters are
   initialized relatively to this basic block, the linear functions
   are initialized to OUTERMOST_LOOP and BB is the place where we try
   to evaluate the STMT.  */

static bool
stmt_simple_for_scop_p (basic_block scop_entry, loop_p outermost_loop,
			gimple stmt, basic_block bb)
{
  loop_p loop = bb->loop_father;

  gcc_assert (scop_entry);

  /* GIMPLE_ASM and GIMPLE_CALL may embed arbitrary side effects.
     Calls have side-effects, except those to const or pure
     functions.  */
  if (gimple_has_volatile_ops (stmt)
      || (gimple_code (stmt) == GIMPLE_CALL
	  && !(gimple_call_flags (stmt) & (ECF_CONST | ECF_PURE)))
      || (gimple_code (stmt) == GIMPLE_ASM))
    return false;

  if (is_gimple_debug (stmt))
    return true;

  if (!stmt_has_simple_data_refs_p (outermost_loop, stmt))
    return false;

  switch (gimple_code (stmt))
    {
    case GIMPLE_RETURN:
    case GIMPLE_LABEL:
      return true;

    case GIMPLE_COND:
      {
	tree op;
	ssa_op_iter op_iter;
        enum tree_code code = gimple_cond_code (stmt);

	/* We can handle all binary comparisons.  Inequalities are
	   also supported as they can be represented with union of
	   polyhedra.  */
        if (!(code == LT_EXPR
	      || code == GT_EXPR
	      || code == LE_EXPR
	      || code == GE_EXPR
	      || code == EQ_EXPR
	      || code == NE_EXPR))
          return false;

	FOR_EACH_SSA_TREE_OPERAND (op, stmt, op_iter, SSA_OP_ALL_USES)
	  if (!graphite_can_represent_expr (scop_entry, loop, outermost_loop,
					    op)
	      /* We can not handle REAL_TYPE. Failed for pr39260.  */
	      || TREE_CODE (TREE_TYPE (op)) == REAL_TYPE)
	    return false;

	return true;
      }

    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
      return true;

    default:
      /* These nodes cut a new scope.  */
      return false;
    }

  return false;
}

/* Returns the statement of BB that contains a harmful operation: that
   can be a function call with side effects, the induction variables
   are not linear with respect to SCOP_ENTRY, etc.  The current open
   scop should end before this statement.  The evaluation is limited using
   OUTERMOST_LOOP as outermost loop that may change.  */

static gimple
harmful_stmt_in_bb (basic_block scop_entry, loop_p outer_loop, basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    if (!stmt_simple_for_scop_p (scop_entry, outer_loop, gsi_stmt (gsi), bb))
      return gsi_stmt (gsi);

  return NULL;
}

/* Return true when it is not possible to represent LOOP in the
   polyhedral representation.  This is evaluated taking SCOP_ENTRY and
   OUTERMOST_LOOP in mind.  */

static bool
graphite_can_represent_loop (basic_block scop_entry, loop_p outermost_loop,
			     loop_p loop)
{
  tree niter = number_of_latch_executions (loop);

  /* Number of iterations unknown.  */
  if (chrec_contains_undetermined (niter))
    return false;

  /* Number of iterations not affine.  */
  if (!graphite_can_represent_expr (scop_entry, loop, outermost_loop, niter))
    return false;

  return true;
}

/* Store information needed by scopdet_* functions.  */

struct scopdet_info
{
  /* Exit of the open scop would stop if the current BB is harmful.  */
  basic_block exit;

  /* Where the next scop would start if the current BB is harmful.  */
  basic_block next;

  /* The bb or one of its children contains open loop exits.  That means
     loop exit nodes that are not surrounded by a loop dominated by bb.  */
  bool exits;

  /* The bb or one of its children contains only structures we can handle.  */
  bool difficult;
};

static struct scopdet_info build_scops_1 (basic_block, loop_p,
					  VEC (sd_region, heap) **, loop_p);

/* Calculates BB infos. If bb is difficult we add valid SCoPs dominated by BB
   to SCOPS.  TYPE is the gbb_type of BB.  */

static struct scopdet_info
scopdet_basic_block_info (basic_block bb, loop_p outermost_loop,
			  VEC (sd_region, heap) **scops, gbb_type type)
{
  loop_p loop = bb->loop_father;
  struct scopdet_info result;
  gimple stmt;

  /* XXX: ENTRY_BLOCK_PTR could be optimized in later steps.  */
  basic_block entry_block = ENTRY_BLOCK_PTR;
  stmt = harmful_stmt_in_bb (entry_block, outermost_loop, bb);
  result.difficult = (stmt != NULL);
  result.exit = NULL;

  switch (type)
    {
    case GBB_LAST:
      result.next = NULL;
      result.exits = false;

      /* Mark bbs terminating a SESE region difficult, if they start
	 a condition.  */
      if (!single_succ_p (bb))
	result.difficult = true;
      else
	result.exit = single_succ (bb);

      break;

    case GBB_SIMPLE:
      result.next = single_succ (bb);
      result.exits = false;
      result.exit = single_succ (bb);
      break;

    case GBB_LOOP_SING_EXIT_HEADER:
      {
	VEC (sd_region, heap) *regions = VEC_alloc (sd_region, heap, 3);
	struct scopdet_info sinfo;
	edge exit_e = single_exit (loop);

	sinfo = build_scops_1 (bb, outermost_loop, &regions, loop);

	if (!graphite_can_represent_loop (entry_block, outermost_loop, loop))
	  result.difficult = true;

	result.difficult |= sinfo.difficult;

	/* Try again with another loop level.  */
	if (result.difficult
	    && loop_depth (outermost_loop) + 1 == loop_depth (loop))
	  {
	    outermost_loop = loop;

	    VEC_free (sd_region, heap, regions);
	    regions = VEC_alloc (sd_region, heap, 3);

	    sinfo = scopdet_basic_block_info (bb, outermost_loop, scops, type);

	    result = sinfo;
	    result.difficult = true;

	    if (sinfo.difficult)
	      move_sd_regions (&regions, scops);
	    else
	      {
		sd_region open_scop;
		open_scop.entry = bb;
		open_scop.exit = exit_e->dest;
		VEC_safe_push (sd_region, heap, *scops, &open_scop);
		VEC_free (sd_region, heap, regions);
	      }
	  }
	else
	  {
	    result.exit = exit_e->dest;
	    result.next = exit_e->dest;

	    /* If we do not dominate result.next, remove it.  It's either
	       the EXIT_BLOCK_PTR, or another bb dominates it and will
	       call the scop detection for this bb.  */
	    if (!dominated_by_p (CDI_DOMINATORS, result.next, bb))
	      result.next = NULL;

	    if (exit_e->src->loop_father != loop)
	      result.next = NULL;

	    result.exits = false;

	    if (result.difficult)
	      move_sd_regions (&regions, scops);
	    else
	      VEC_free (sd_region, heap, regions);
	  }

	break;
      }

    case GBB_LOOP_MULT_EXIT_HEADER:
      {
        /* XXX: For now we just do not join loops with multiple exits.  If the
           exits lead to the same bb it may be possible to join the loop.  */
        VEC (sd_region, heap) *regions = VEC_alloc (sd_region, heap, 3);
        VEC (edge, heap) *exits = get_loop_exit_edges (loop);
        edge e;
        int i;
	build_scops_1 (bb, loop, &regions, loop);

	/* Scan the code dominated by this loop.  This means all bbs, that are
	   are dominated by a bb in this loop, but are not part of this loop.

	   The easiest case:
	     - The loop exit destination is dominated by the exit sources.

	   TODO: We miss here the more complex cases:
		  - The exit destinations are dominated by another bb inside
		    the loop.
		  - The loop dominates bbs, that are not exit destinations.  */
        for (i = 0; VEC_iterate (edge, exits, i, e); i++)
          if (e->src->loop_father == loop
	      && dominated_by_p (CDI_DOMINATORS, e->dest, e->src))
	    {
	      if (loop_outer (outermost_loop))
		outermost_loop = loop_outer (outermost_loop);

	      /* Pass loop_outer to recognize e->dest as loop header in
		 build_scops_1.  */
	      if (e->dest->loop_father->header == e->dest)
		build_scops_1 (e->dest, outermost_loop, &regions,
			       loop_outer (e->dest->loop_father));
	      else
		build_scops_1 (e->dest, outermost_loop, &regions,
			       e->dest->loop_father);
	    }

        result.next = NULL;
        result.exit = NULL;
        result.difficult = true;
        result.exits = false;
        move_sd_regions (&regions, scops);
        VEC_free (edge, heap, exits);
        break;
      }
    case GBB_COND_HEADER:
      {
	VEC (sd_region, heap) *regions = VEC_alloc (sd_region, heap, 3);
	struct scopdet_info sinfo;
	VEC (basic_block, heap) *dominated;
	int i;
	basic_block dom_bb;
	basic_block last_exit = NULL;
	edge e;
	result.exits = false;

	/* First check the successors of BB, and check if it is
	   possible to join the different branches.  */
	for (i = 0; VEC_iterate (edge, bb->succs, i, e); i++)
	  {
	    /* Ignore loop exits.  They will be handled after the loop
	       body.  */
	    if (is_loop_exit (loop, e->dest))
	      {
		result.exits = true;
		continue;
	      }

	    /* Do not follow edges that lead to the end of the
	       conditions block.  For example, in

               |   0
	       |  /|\
	       | 1 2 |
	       | | | |
	       | 3 4 |
	       |  \|/
               |   6

	       the edge from 0 => 6.  Only check if all paths lead to
	       the same node 6.  */

	    if (!single_pred_p (e->dest))
	      {
		/* Check, if edge leads directly to the end of this
		   condition.  */
		if (!last_exit)
		  last_exit = e->dest;

		if (e->dest != last_exit)
		  result.difficult = true;

		continue;
	      }

	    if (!dominated_by_p (CDI_DOMINATORS, e->dest, bb))
	      {
		result.difficult = true;
		continue;
	      }

	    sinfo = build_scops_1 (e->dest, outermost_loop, &regions, loop);

	    result.exits |= sinfo.exits;
	    result.difficult |= sinfo.difficult;

	    /* Checks, if all branches end at the same point.
	       If that is true, the condition stays joinable.
	       Have a look at the example above.  */
	    if (sinfo.exit)
	      {
		if (!last_exit)
		  last_exit = sinfo.exit;

		if (sinfo.exit != last_exit)
		  result.difficult = true;
	      }
	    else
	      result.difficult = true;
	  }

	if (!last_exit)
	  result.difficult = true;

	/* Join the branches of the condition if possible.  */
	if (!result.exits && !result.difficult)
	  {
	    /* Only return a next pointer if we dominate this pointer.
	       Otherwise it will be handled by the bb dominating it.  */
	    if (dominated_by_p (CDI_DOMINATORS, last_exit, bb)
		&& last_exit != bb)
	      result.next = last_exit;
	    else
	      result.next = NULL;

	    result.exit = last_exit;

	    VEC_free (sd_region, heap, regions);
	    break;
	  }

	/* Scan remaining bbs dominated by BB.  */
	dominated = get_dominated_by (CDI_DOMINATORS, bb);

	for (i = 0; VEC_iterate (basic_block, dominated, i, dom_bb); i++)
	  {
	    /* Ignore loop exits: they will be handled after the loop body.  */
	    if (loop_depth (find_common_loop (loop, dom_bb->loop_father))
		< loop_depth (loop))
	      {
		result.exits = true;
		continue;
	      }

	    /* Ignore the bbs processed above.  */
	    if (single_pred_p (dom_bb) && single_pred (dom_bb) == bb)
	      continue;

	    if (loop_depth (loop) > loop_depth (dom_bb->loop_father))
	      sinfo = build_scops_1 (dom_bb, outermost_loop, &regions,
				     loop_outer (loop));
	    else
	      sinfo = build_scops_1 (dom_bb, outermost_loop, &regions, loop);

	    result.exits |= sinfo.exits;
	    result.difficult = true;
	    result.exit = NULL;
	  }

	VEC_free (basic_block, heap, dominated);

	result.next = NULL;
	move_sd_regions (&regions, scops);

	break;
      }

    default:
      gcc_unreachable ();
    }

  return result;
}

/* Starting from CURRENT we walk the dominance tree and add new sd_regions to
   SCOPS. The analyse if a sd_region can be handled is based on the value
   of OUTERMOST_LOOP. Only loops inside OUTERMOST loops may change.  LOOP
   is the loop in which CURRENT is handled.

   TODO: These functions got a little bit big. They definitely should be cleaned
	 up.  */

static struct scopdet_info
build_scops_1 (basic_block current, loop_p outermost_loop,
	       VEC (sd_region, heap) **scops, loop_p loop)
{
  bool in_scop = false;
  sd_region open_scop;
  struct scopdet_info sinfo;

  /* Initialize result.  */
  struct scopdet_info result;
  result.exits = false;
  result.difficult = false;
  result.next = NULL;
  result.exit = NULL;
  open_scop.entry = NULL;
  open_scop.exit = NULL;
  sinfo.exit = NULL;

  /* Loop over the dominance tree.  If we meet a difficult bb, close
     the current SCoP.  Loop and condition header start a new layer,
     and can only be added if all bbs in deeper layers are simple.  */
  while (current != NULL)
    {
      sinfo = scopdet_basic_block_info (current, outermost_loop, scops,
					get_bb_type (current, loop));

      if (!in_scop && !(sinfo.exits || sinfo.difficult))
        {
	  open_scop.entry = current;
	  open_scop.exit = NULL;
          in_scop = true;
        }
      else if (in_scop && (sinfo.exits || sinfo.difficult))
        {
	  open_scop.exit = current;
          VEC_safe_push (sd_region, heap, *scops, &open_scop);
          in_scop = false;
        }

      result.difficult |= sinfo.difficult;
      result.exits |= sinfo.exits;

      current = sinfo.next;
    }

  /* Try to close open_scop, if we are still in an open SCoP.  */
  if (in_scop)
    {
      open_scop.exit = sinfo.exit;
      gcc_assert (open_scop.exit);
      VEC_safe_push (sd_region, heap, *scops, &open_scop);
    }

  result.exit = sinfo.exit;
  return result;
}

/* Checks if a bb is contained in REGION.  */

static bool
bb_in_sd_region (basic_block bb, sd_region *region)
{
  return bb_in_region (bb, region->entry, region->exit);
}

/* Returns the single entry edge of REGION, if it does not exits NULL.  */

static edge
find_single_entry_edge (sd_region *region)
{
  edge e;
  edge_iterator ei;
  edge entry = NULL;

  FOR_EACH_EDGE (e, ei, region->entry->preds)
    if (!bb_in_sd_region (e->src, region))
      {
	if (entry)
	  {
	    entry = NULL;
	    break;
	  }

	else
	  entry = e;
      }

  return entry;
}

/* Returns the single exit edge of REGION, if it does not exits NULL.  */

static edge
find_single_exit_edge (sd_region *region)
{
  edge e;
  edge_iterator ei;
  edge exit = NULL;

  FOR_EACH_EDGE (e, ei, region->exit->preds)
    if (bb_in_sd_region (e->src, region))
      {
	if (exit)
	  {
	    exit = NULL;
	    break;
	  }

	else
	  exit = e;
      }

  return exit;
}

/* Create a single entry edge for REGION.  */

static void
create_single_entry_edge (sd_region *region)
{
  if (find_single_entry_edge (region))
    return;

  /* There are multiple predecessors for bb_3

  |  1  2
  |  | /
  |  |/
  |  3	<- entry
  |  |\
  |  | |
  |  4 ^
  |  | |
  |  |/
  |  5

  There are two edges (1->3, 2->3), that point from outside into the region,
  and another one (5->3), a loop latch, lead to bb_3.

  We split bb_3.

  |  1  2
  |  | /
  |  |/
  |3.0
  |  |\     (3.0 -> 3.1) = single entry edge
  |3.1 |  	<- entry
  |  | |
  |  | |
  |  4 ^
  |  | |
  |  |/
  |  5

  If the loop is part of the SCoP, we have to redirect the loop latches.

  |  1  2
  |  | /
  |  |/
  |3.0
  |  |      (3.0 -> 3.1) = entry edge
  |3.1  	<- entry
  |  |\
  |  | |
  |  4 ^
  |  | |
  |  |/
  |  5  */

  if (region->entry->loop_father->header != region->entry
      || dominated_by_p (CDI_DOMINATORS,
			 loop_latch_edge (region->entry->loop_father)->src,
			 region->exit))
    {
      edge forwarder = split_block_after_labels (region->entry);
      region->entry = forwarder->dest;
    }
  else
    /* This case is never executed, as the loop headers seem always to have a
       single edge pointing from outside into the loop.  */
    gcc_unreachable ();

#ifdef ENABLE_CHECKING
  gcc_assert (find_single_entry_edge (region));
#endif
}

/* Check if the sd_region, mentioned in EDGE, has no exit bb.  */

static bool
sd_region_without_exit (edge e)
{
  sd_region *r = (sd_region *) e->aux;

  if (r)
    return r->exit == NULL;
  else
    return false;
}

/* Create a single exit edge for REGION.  */

static void
create_single_exit_edge (sd_region *region)
{
  edge e;
  edge_iterator ei;
  edge forwarder = NULL;
  basic_block exit;

  if (find_single_exit_edge (region))
    return;

  /* We create a forwarder bb (5) for all edges leaving this region
     (3->5, 4->5).  All other edges leading to the same bb, are moved
     to a new bb (6).  If these edges where part of another region (2->5)
     we update the region->exit pointer, of this region.

     To identify which edge belongs to which region we depend on the e->aux
     pointer in every edge.  It points to the region of the edge or to NULL,
     if the edge is not part of any region.

     1 2 3 4   	1->5 no region, 		2->5 region->exit = 5,
      \| |/    	3->5 region->exit = NULL, 	4->5 region->exit = NULL
        5	<- exit

     changes to

     1 2 3 4   	1->6 no region, 			2->6 region->exit = 6,
     | | \/	3->5 no region,				4->5 no region,
     | |  5
      \| /	5->6 region->exit = 6
	6

     Now there is only a single exit edge (5->6).  */
  exit = region->exit;
  region->exit = NULL;
  forwarder = make_forwarder_block (exit, &sd_region_without_exit, NULL);

  /* Unmark the edges, that are no longer exit edges.  */
  FOR_EACH_EDGE (e, ei, forwarder->src->preds)
    if (e->aux)
      e->aux = NULL;

  /* Mark the new exit edge.  */
  single_succ_edge (forwarder->src)->aux = region;

  /* Update the exit bb of all regions, where exit edges lead to
     forwarder->dest.  */
  FOR_EACH_EDGE (e, ei, forwarder->dest->preds)
    if (e->aux)
      ((sd_region *) e->aux)->exit = forwarder->dest;

#ifdef ENABLE_CHECKING
  gcc_assert (find_single_exit_edge (region));
#endif
}

/* Unmark the exit edges of all REGIONS.
   See comment in "create_single_exit_edge". */

static void
unmark_exit_edges (VEC (sd_region, heap) *regions)
{
  int i;
  sd_region *s;
  edge e;
  edge_iterator ei;

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    FOR_EACH_EDGE (e, ei, s->exit->preds)
      e->aux = NULL;
}


/* Mark the exit edges of all REGIONS.
   See comment in "create_single_exit_edge". */

static void
mark_exit_edges (VEC (sd_region, heap) *regions)
{
  int i;
  sd_region *s;
  edge e;
  edge_iterator ei;

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    FOR_EACH_EDGE (e, ei, s->exit->preds)
      if (bb_in_sd_region (e->src, s))
	e->aux = s;
}

/* Create for all scop regions a single entry and a single exit edge.  */

static void
create_sese_edges (VEC (sd_region, heap) *regions)
{
  int i;
  sd_region *s;

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    create_single_entry_edge (s);

  mark_exit_edges (regions);

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    create_single_exit_edge (s);

  unmark_exit_edges (regions);

  fix_loop_structure (NULL);

#ifdef ENABLE_CHECKING
  verify_loop_structure ();
  verify_dominators (CDI_DOMINATORS);
  verify_ssa (false);
#endif
}

/* Create graphite SCoPs from an array of scop detection REGIONS.  */

static void
build_graphite_scops (VEC (sd_region, heap) *regions,
		      VEC (scop_p, heap) **scops)
{
  int i;
  sd_region *s;

  for (i = 0; VEC_iterate (sd_region, regions, i, s); i++)
    {
      edge entry = find_single_entry_edge (s);
      edge exit = find_single_exit_edge (s);
      scop_p scop = new_scop (new_sese (entry, exit));
      VEC_safe_push (scop_p, heap, *scops, scop);

      /* Are there overlapping SCoPs?  */
#ifdef ENABLE_CHECKING
	{
	  int j;
	  sd_region *s2;

	  for (j = 0; VEC_iterate (sd_region, regions, j, s2); j++)
	    if (s != s2)
	      gcc_assert (!bb_in_sd_region (s->entry, s2));
	}
#endif
    }
}

/* Returns true when BB contains only close phi nodes.  */

static bool
contains_only_close_phi_nodes (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    if (gimple_code (gsi_stmt (gsi)) != GIMPLE_LABEL)
      return false;

  return true;
}

/* Print statistics for SCOP to FILE.  */

static void
print_graphite_scop_statistics (FILE* file, scop_p scop)
{
  long n_bbs = 0;
  long n_loops = 0;
  long n_stmts = 0;
  long n_conditions = 0;
  long n_p_bbs = 0;
  long n_p_loops = 0;
  long n_p_stmts = 0;
  long n_p_conditions = 0;

  basic_block bb;

  FOR_ALL_BB (bb)
    {
      gimple_stmt_iterator psi;
      loop_p loop = bb->loop_father;

      if (!bb_in_sese_p (bb, SCOP_REGION (scop)))
	continue;

      n_bbs++;
      n_p_bbs += bb->count;

      if (VEC_length (edge, bb->succs) > 1)
	{
	  n_conditions++;
	  n_p_conditions += bb->count;
	}

      for (psi = gsi_start_bb (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  n_stmts++;
	  n_p_stmts += bb->count;
	}

      if (loop->header == bb && loop_in_sese_p (loop, SCOP_REGION (scop)))
	{
	  n_loops++;
	  n_p_loops += bb->count;
	}

    }

  fprintf (file, "\nBefore limit_scops SCoP statistics (");
  fprintf (file, "BBS:%ld, ", n_bbs);
  fprintf (file, "LOOPS:%ld, ", n_loops);
  fprintf (file, "CONDITIONS:%ld, ", n_conditions);
  fprintf (file, "STMTS:%ld)\n", n_stmts);
  fprintf (file, "\nBefore limit_scops SCoP profiling statistics (");
  fprintf (file, "BBS:%ld, ", n_p_bbs);
  fprintf (file, "LOOPS:%ld, ", n_p_loops);
  fprintf (file, "CONDITIONS:%ld, ", n_p_conditions);
  fprintf (file, "STMTS:%ld)\n", n_p_stmts);
}

/* Print statistics for SCOPS to FILE.  */

static void
print_graphite_statistics (FILE* file, VEC (scop_p, heap) *scops)
{
  int i;
  scop_p scop;

  for (i = 0; VEC_iterate (scop_p, scops, i, scop); i++)
    print_graphite_scop_statistics (file, scop);
}

/* We limit all SCoPs to SCoPs, that are completely surrounded by a loop.

   Example:

   for (i      |
     {         |
       for (j  |  SCoP 1
       for (k  |
     }         |

   * SCoP frontier, as this line is not surrounded by any loop. *

   for (l      |  SCoP 2

   This is necessary as scalar evolution and parameter detection need a
   outermost loop to initialize parameters correctly.

   TODO: FIX scalar evolution and parameter detection to allow more flexible
         SCoP frontiers.  */

static void
limit_scops (VEC (scop_p, heap) **scops)
{
  VEC (sd_region, heap) *regions = VEC_alloc (sd_region, heap, 3);

  int i;
  scop_p scop;

  for (i = 0; VEC_iterate (scop_p, *scops, i, scop); i++)
    {
      int j;
      loop_p loop;
      sese region = SCOP_REGION (scop);
      build_sese_loop_nests (region);

      for (j = 0; VEC_iterate (loop_p, SESE_LOOP_NEST (region), j, loop); j++)
        if (!loop_in_sese_p (loop_outer (loop), region)
	    && single_exit (loop))
          {
	    sd_region open_scop;
	    open_scop.entry = loop->header;
	    open_scop.exit = single_exit (loop)->dest;

	    /* This is a hack on top of the limit_scops hack.  The
	       limit_scops hack should disappear all together.  */
	    if (single_succ_p (open_scop.exit)
		&& contains_only_close_phi_nodes (open_scop.exit))
	      open_scop.exit = single_succ_edge (open_scop.exit)->dest;

	    VEC_safe_push (sd_region, heap, regions, &open_scop);
	  }
    }

  free_scops (*scops);
  *scops = VEC_alloc (scop_p, heap, 3);

  create_sese_edges (regions);
  build_graphite_scops (regions, scops);
  VEC_free (sd_region, heap, regions);
}

/* Transforms LOOP to the canonical loop closed SSA form.  */

static void
canonicalize_loop_closed_ssa (loop_p loop)
{
  edge e = single_exit (loop);
  basic_block bb;

  if (!e || e->flags & EDGE_ABNORMAL)
    return;

  bb = e->dest;

  if (VEC_length (edge, bb->preds) == 1)
    split_block_after_labels (bb);
  else
    {
      gimple_stmt_iterator psi;
      basic_block close = split_edge (e);

      e = single_succ_edge (close);

      for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  gimple phi = gsi_stmt (psi);
	  unsigned i;

	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    if (gimple_phi_arg_edge (phi, i) == e)
	      {
		tree res, arg = gimple_phi_arg_def (phi, i);
		use_operand_p use_p;
		gimple close_phi;

		if (TREE_CODE (arg) != SSA_NAME)
		  continue;

		close_phi = create_phi_node (arg, close);
		res = create_new_def_for (gimple_phi_result (close_phi),
					  close_phi,
					  gimple_phi_result_ptr (close_phi));
		add_phi_arg (close_phi, arg,
			     gimple_phi_arg_edge (close_phi, 0),
			     UNKNOWN_LOCATION);
		use_p = gimple_phi_arg_imm_use_ptr (phi, i);
		replace_exp (use_p, res);
		update_stmt (phi);
	      }
	}
    }
}

/* Converts the current loop closed SSA form to a canonical form
   expected by the Graphite code generation.

   The loop closed SSA form has the following invariant: a variable
   defined in a loop that is used outside the loop appears only in the
   phi nodes in the destination of the loop exit.  These phi nodes are
   called close phi nodes.

   The canonical loop closed SSA form contains the extra invariants:

   - when the loop contains only one exit, the close phi nodes contain
   only one argument.  That implies that the basic block that contains
   the close phi nodes has only one predecessor, that is a basic block
   in the loop.

   - the basic block containing the close phi nodes does not contain
   other statements.
*/

static void
canonicalize_loop_closed_ssa_form (void)
{
  loop_iterator li;
  loop_p loop;

#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
#endif

  FOR_EACH_LOOP (li, loop, 0)
    canonicalize_loop_closed_ssa (loop);

  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  update_ssa (TODO_update_ssa);

#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
#endif
}

/* Find Static Control Parts (SCoP) in the current function and pushes
   them to SCOPS.  */

void
build_scops (VEC (scop_p, heap) **scops)
{
  struct loop *loop = current_loops->tree_root;
  VEC (sd_region, heap) *regions = VEC_alloc (sd_region, heap, 3);

  canonicalize_loop_closed_ssa_form ();
  build_scops_1 (single_succ (ENTRY_BLOCK_PTR), ENTRY_BLOCK_PTR->loop_father,
			      &regions, loop);
  create_sese_edges (regions);
  build_graphite_scops (regions, scops);

  if (dump_file && (dump_flags & TDF_DETAILS))
    print_graphite_statistics (dump_file, *scops);

  limit_scops (scops);
  VEC_free (sd_region, heap, regions);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nnumber of SCoPs: %d\n",
	     VEC_length (scop_p, *scops));
}

/* Pretty print to FILE all the SCoPs in DOT format and mark them with
   different colors.  If there are not enough colors, paint the
   remaining SCoPs in gray.

   Special nodes:
   - "*" after the node number denotes the entry of a SCoP,
   - "#" after the node number denotes the exit of a SCoP,
   - "()" around the node number denotes the entry or the
     exit nodes of the SCOP.  These are not part of SCoP.  */

static void
dot_all_scops_1 (FILE *file, VEC (scop_p, heap) *scops)
{
  basic_block bb;
  edge e;
  edge_iterator ei;
  scop_p scop;
  const char* color;
  int i;

  /* Disable debugging while printing graph.  */
  int tmp_dump_flags = dump_flags;
  dump_flags = 0;

  fprintf (file, "digraph all {\n");

  FOR_ALL_BB (bb)
    {
      int part_of_scop = false;

      /* Use HTML for every bb label.  So we are able to print bbs
         which are part of two different SCoPs, with two different
         background colors.  */
      fprintf (file, "%d [label=<\n  <TABLE BORDER=\"0\" CELLBORDER=\"1\" ",
                     bb->index);
      fprintf (file, "CELLSPACING=\"0\">\n");

      /* Select color for SCoP.  */
      for (i = 0; VEC_iterate (scop_p, scops, i, scop); i++)
	{
	  sese region = SCOP_REGION (scop);
	  if (bb_in_sese_p (bb, region)
	      || (SESE_EXIT_BB (region) == bb)
	      || (SESE_ENTRY_BB (region) == bb))
	    {
	      switch (i % 17)
		{
		case 0: /* red */
		  color = "#e41a1c";
		  break;
		case 1: /* blue */
		  color = "#377eb8";
		  break;
		case 2: /* green */
		  color = "#4daf4a";
		  break;
		case 3: /* purple */
		  color = "#984ea3";
		  break;
		case 4: /* orange */
		  color = "#ff7f00";
		  break;
		case 5: /* yellow */
		  color = "#ffff33";
		  break;
		case 6: /* brown */
		  color = "#a65628";
		  break;
		case 7: /* rose */
		  color = "#f781bf";
		  break;
		case 8:
		  color = "#8dd3c7";
		  break;
		case 9:
		  color = "#ffffb3";
		  break;
		case 10:
		  color = "#bebada";
		  break;
		case 11:
		  color = "#fb8072";
		  break;
		case 12:
		  color = "#80b1d3";
		  break;
		case 13:
		  color = "#fdb462";
		  break;
		case 14:
		  color = "#b3de69";
		  break;
		case 15:
		  color = "#fccde5";
		  break;
		case 16:
		  color = "#bc80bd";
		  break;
		default: /* gray */
		  color = "#999999";
		}

	      fprintf (file, "    <TR><TD WIDTH=\"50\" BGCOLOR=\"%s\">", color);

	      if (!bb_in_sese_p (bb, region))
		fprintf (file, " (");

	      if (bb == SESE_ENTRY_BB (region)
		  && bb == SESE_EXIT_BB (region))
		fprintf (file, " %d*# ", bb->index);
	      else if (bb == SESE_ENTRY_BB (region))
		fprintf (file, " %d* ", bb->index);
	      else if (bb == SESE_EXIT_BB (region))
		fprintf (file, " %d# ", bb->index);
	      else
		fprintf (file, " %d ", bb->index);

	      if (!bb_in_sese_p (bb,region))
		fprintf (file, ")");

	      fprintf (file, "</TD></TR>\n");
	      part_of_scop  = true;
	    }
	}

      if (!part_of_scop)
	{
	  fprintf (file, "    <TR><TD WIDTH=\"50\" BGCOLOR=\"#ffffff\">");
	  fprintf (file, " %d </TD></TR>\n", bb->index);
	}
      fprintf (file, "  </TABLE>>, shape=box, style=\"setlinewidth(0)\"]\n");
    }

  FOR_ALL_BB (bb)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	      fprintf (file, "%d -> %d;\n", bb->index, e->dest->index);
    }

  fputs ("}\n\n", file);

  /* Enable debugging again.  */
  dump_flags = tmp_dump_flags;
}

/* Display all SCoPs using dotty.  */

void
dot_all_scops (VEC (scop_p, heap) *scops)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  int x;
  FILE *stream = fopen ("/tmp/allscops.dot", "w");
  gcc_assert (stream);

  dot_all_scops_1 (stream, scops);
  fclose (stream);

  x = system ("dotty /tmp/allscops.dot &");
#else
  dot_all_scops_1 (stderr, scops);
#endif
}

/* Display all SCoPs using dotty.  */

void
dot_scop (scop_p scop)
{
  VEC (scop_p, heap) *scops = NULL;

  if (scop)
    VEC_safe_push (scop_p, heap, scops, scop);

  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  {
    int x;
    FILE *stream = fopen ("/tmp/allscops.dot", "w");
    gcc_assert (stream);

    dot_all_scops_1 (stream, scops);
    fclose (stream);
    x = system ("dotty /tmp/allscops.dot &");
  }
#else
  dot_all_scops_1 (stderr, scops);
#endif

  VEC_free (scop_p, heap, scops);
}

#endif
