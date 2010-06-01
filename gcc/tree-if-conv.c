/* If-conversion for vectorizer.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Devang Patel <dpatel@apple.com>

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

/* This pass implements a tree level if-conversion of loops.  Its
   initial goal is to help the vectorizer to vectorize loops with
   conditions.

   A short description of if-conversion:

     o Decide if a loop is if-convertible or not.
     o Walk all loop basic blocks in breadth first order (BFS order).
       o Remove conditional statements (at the end of basic block)
         and propagate condition into destination basic blocks'
	 predicate list.
       o Replace modify expression with conditional modify expression
         using current basic block's condition.
     o Merge all basic blocks
       o Replace phi nodes with conditional modify expr
       o Merge all basic blocks into header

     Sample transformation:

     INPUT
     -----

     # i_23 = PHI <0(0), i_18(10)>;
     <L0>:;
     j_15 = A[i_23];
     if (j_15 > 41) goto <L1>; else goto <L17>;

     <L17>:;
     goto <bb 3> (<L3>);

     <L1>:;

     # iftmp.2_4 = PHI <0(8), 42(2)>;
     <L3>:;
     A[i_23] = iftmp.2_4;
     i_18 = i_23 + 1;
     if (i_18 <= 15) goto <L19>; else goto <L18>;

     <L19>:;
     goto <bb 1> (<L0>);

     <L18>:;

     OUTPUT
     ------

     # i_23 = PHI <0(0), i_18(10)>;
     <L0>:;
     j_15 = A[i_23];

     <L3>:;
     iftmp.2_4 = j_15 > 41 ? 42 : 0;
     A[i_23] = iftmp.2_4;
     i_18 = i_23 + 1;
     if (i_18 <= 15) goto <L19>; else goto <L18>;

     <L19>:;
     goto <bb 1> (<L0>);

     <L18>:;
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "timevar.h"
#include "basic-block.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"

/* List of basic blocks in if-conversion-suitable order.  */
static basic_block *ifc_bbs;

/* Create a new temp variable of type TYPE.  Add GIMPLE_ASSIGN to assign EXP
   to the new variable.  */

static gimple
ifc_temp_var (tree type, tree exp)
{
  const char *name = "_ifc_";
  tree var, new_name;
  gimple stmt;

  /* Create new temporary variable.  */
  var = create_tmp_var (type, name);
  add_referenced_var (var);

  /* Build new statement to assign EXP to new variable.  */
  stmt = gimple_build_assign (var, exp);

  /* Get SSA name for the new variable and set make new statement
     its definition statement.  */
  new_name = make_ssa_name (var, stmt);
  gimple_assign_set_lhs (stmt, new_name);
  SSA_NAME_DEF_STMT (new_name) = stmt;
  update_stmt (stmt);

  return stmt;
}

/* Return true when COND is a true predicate.  */

static inline bool
is_true_predicate (tree cond)
{
  return (cond == NULL_TREE
	  || cond == boolean_true_node
	  || integer_onep (cond));
}

/* Returns true when BB has a predicate that is not trivial: true or
   NULL_TREE.  */

static inline bool
is_predicated (basic_block bb)
{
  return !is_true_predicate ((tree) bb->aux);
}

/* Add condition NEW_COND to the predicate list of basic block BB.  */

static inline void
add_to_predicate_list (basic_block bb, tree new_cond)
{
  tree cond = (tree) bb->aux;

  bb->aux = is_true_predicate (cond) ? new_cond :
    fold_build2_loc (EXPR_LOCATION (cond),
		     TRUTH_OR_EXPR, boolean_type_node,
		     cond, new_cond);
}

/* Add the condition COND to the previous condition PREV_COND, and add
   this to the predicate list of the destination of edge E.  LOOP is
   the loop to be if-converted.  */

static void
add_to_dst_predicate_list (struct loop *loop, edge e,
			   tree prev_cond, tree cond)
{
  if (!flow_bb_inside_loop_p (loop, e->dest))
    return;

  if (!is_true_predicate (prev_cond))
    cond = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			prev_cond, cond);

  add_to_predicate_list (e->dest, cond);
}

/* Return true if one of the successor edges of BB exits LOOP.  */

static bool
bb_with_exit_edge_p (struct loop *loop, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (loop_exit_edge_p (loop, e))
      return true;

  return false;
}

/* Return true when PHI is if-convertible.  PHI is part of loop LOOP
   and it belongs to basic block BB.

   PHI is not if-convertible if:
   - it has more than 2 arguments,
   - virtual PHI is immediately used in another PHI node,
   - virtual PHI on BB other than header.  */

static bool
if_convertible_phi_p (struct loop *loop, basic_block bb, gimple phi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "-------------------------\n");
      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
    }

  if (bb != loop->header && gimple_phi_num_args (phi) != 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "More than two phi node args.\n");
      return false;
    }

  if (!is_gimple_reg (SSA_NAME_VAR (gimple_phi_result (phi))))
    {
      imm_use_iterator imm_iter;
      use_operand_p use_p;

      if (bb != loop->header)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Virtual phi not on loop header.\n");
	  return false;
	}
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, gimple_phi_result (phi))
	{
	  if (gimple_code (USE_STMT (use_p)) == GIMPLE_PHI)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Difficult to handle this virtual phi.\n");
	      return false;
	    }
	}
    }

  return true;
}

/* Return true when STMT is if-convertible.

   GIMPLE_ASSIGN statement is not if-convertible if,
   - it is not movable,
   - it could trap,
   - LHS is not var decl.

   GIMPLE_ASSIGN is part of block BB, which is inside loop LOOP.  */

static bool
if_convertible_gimple_assign_stmt_p (struct loop *loop, basic_block bb,
    				     gimple stmt)
{
  tree lhs = gimple_assign_lhs (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "-------------------------\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  /* Some of these constrains might be too conservative.  */
  if (stmt_ends_bb_p (stmt)
      || gimple_has_volatile_ops (stmt)
      || (TREE_CODE (lhs) == SSA_NAME
          && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
      || gimple_has_side_effects (stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "stmt not suitable for ifcvt\n");
      return false;
    }

  if (gimple_assign_rhs_could_trap_p (stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "tree could trap...\n");
      return false;
    }

  if (TREE_CODE (lhs) != SSA_NAME
      && bb != loop->header
      && !bb_with_exit_edge_p (loop, bb))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "LHS is not var\n");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      return false;
    }

  return true;
}

/* Return true when STMT is if-convertible.

   A statement is if-convertible if:
   - it is an if-convertible GIMPLE_ASSGIN,
   - it is a GIMPLE_LABEL or a GIMPLE_COND.

   STMT is inside BB, which is inside loop LOOP.  */

static bool
if_convertible_stmt_p (struct loop *loop, basic_block bb, gimple stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_LABEL:
    case GIMPLE_DEBUG:
    case GIMPLE_COND:
      return true;

    case GIMPLE_ASSIGN:
      return if_convertible_gimple_assign_stmt_p (loop, bb, stmt);

    default:
      /* Don't know what to do with 'em so don't do anything.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "don't know what to do\n");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      return false;
      break;
    }

  return true;
}

/* Return true when BB is if-convertible.  This routine does not check
   basic block's statements and phis.

   A basic block is not if-convertible if:
   - it is non-empty and it is after the exit block (in BFS order),
   - it is after the exit block but before the latch,
   - its edges are not normal.

   EXIT_BB is the basic block containing the exit of the LOOP.  BB is
   inside LOOP.  */

static bool
if_convertible_bb_p (struct loop *loop, basic_block bb, basic_block exit_bb)
{
  edge e;
  edge_iterator ei;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "----------[%d]-------------\n", bb->index);

  if (EDGE_COUNT (bb->preds) > 2
      || EDGE_COUNT (bb->succs) > 2)
    return false;

  if (exit_bb)
    {
      if (bb != loop->latch)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "basic block after exit bb but before latch\n");
	  return false;
	}
      else if (!empty_block_p (bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "non empty basic block after exit bb\n");
	  return false;
	}
      else if (bb == loop->latch
	       && bb != exit_bb
	       && !dominated_by_p (CDI_DOMINATORS, bb, exit_bb))
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "latch is not dominated by exit_block\n");
	    return false;
	  }
    }

  /* Be less adventurous and handle only normal edges.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags &
	(EDGE_ABNORMAL_CALL | EDGE_EH | EDGE_ABNORMAL | EDGE_IRREDUCIBLE_LOOP))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Difficult to handle edges\n");
	return false;
      }

  return true;
}

/* Return true when all predecessor blocks of BB are visited.  The
   VISITED bitmap keeps track of the visited blocks.  */

static bool
pred_blocks_visited_p (basic_block bb, bitmap *visited)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!bitmap_bit_p (*visited, e->src->index))
      return false;

  return true;
}

/* Get body of a LOOP in suitable order for if-conversion.  It is
   caller's responsibility to deallocate basic block list.
   If-conversion suitable order is, breadth first sort (BFS) order
   with an additional constraint: select a block only if all its
   predecessors are already selected.  */

static basic_block *
get_loop_body_in_if_conv_order (const struct loop *loop)
{
  basic_block *blocks, *blocks_in_bfs_order;
  basic_block bb;
  bitmap visited;
  unsigned int index = 0;
  unsigned int visited_count = 0;

  gcc_assert (loop->num_nodes);
  gcc_assert (loop->latch != EXIT_BLOCK_PTR);

  blocks = XCNEWVEC (basic_block, loop->num_nodes);
  visited = BITMAP_ALLOC (NULL);

  blocks_in_bfs_order = get_loop_body_in_bfs_order (loop);

  index = 0;
  while (index < loop->num_nodes)
    {
      bb = blocks_in_bfs_order [index];

      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	{
	  free (blocks_in_bfs_order);
	  BITMAP_FREE (visited);
	  free (blocks);
	  return NULL;
	}

      if (!bitmap_bit_p (visited, bb->index))
	{
	  if (pred_blocks_visited_p (bb, &visited)
	      || bb == loop->header)
	    {
	      /* This block is now visited.  */
	      bitmap_set_bit (visited, bb->index);
	      blocks[visited_count++] = bb;
	    }
	}

      index++;

      if (index == loop->num_nodes
	  && visited_count != loop->num_nodes)
	/* Not done yet.  */
	index = 0;
    }
  free (blocks_in_bfs_order);
  BITMAP_FREE (visited);
  return blocks;
}

/* Returns true when the analysis of the predicates for all the basic
   blocks in LOOP succeeded.

   predicate_bbs first clears the ->aux fields of the basic blocks.
   These fields are then initialized with the tree expressions
   representing the predicates under which a basic block is executed
   in the LOOP.  As the loop->header is executed at each iteration, it
   has the "true" predicate.  Other statements executed under a
   condition are predicated with that condition, for example

   | if (x)
   |   S1;
   | else
   |   S2;

   S1 will be predicated with "x", and S2 will be predicated with
   "!x".  */

static bool
predicate_bbs (loop_p loop)
{
  unsigned int i;

  for (i = 0; i < loop->num_nodes; i++)
    ifc_bbs[i]->aux = NULL;

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs [i];
      tree cond = (tree) bb->aux;
      gimple_stmt_iterator itr;

      for (itr = gsi_start_bb (bb); !gsi_end_p (itr); gsi_next (&itr))
	{
	  gimple stmt = gsi_stmt (itr);

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_LABEL:
	    case GIMPLE_ASSIGN:
	    case GIMPLE_CALL:
	      break;

	    case GIMPLE_DEBUG:
	      /* ??? Should there be conditional GIMPLE_DEBUG_BINDs?  */
	      if (gimple_debug_bind_p (gsi_stmt (itr)))
		{
		  gimple_debug_bind_reset_value (gsi_stmt (itr));
		  update_stmt (gsi_stmt (itr));
		}
	      break;

	    case GIMPLE_COND:
	      {
		tree c2;
		edge true_edge, false_edge;
		location_t loc = gimple_location (stmt);
		tree c = fold_build2_loc (loc, gimple_cond_code (stmt),
					  boolean_type_node,
					  gimple_cond_lhs (stmt),
					  gimple_cond_rhs (stmt));

		extract_true_false_edges_from_block (gimple_bb (stmt),
						     &true_edge, &false_edge);

		/* Add new condition into destination's predicate list.  */

		/* If C is true, then TRUE_EDGE is taken.  */
		add_to_dst_predicate_list (loop, true_edge, cond, c);

		/* If C is false, then FALSE_EDGE is taken.  */
		c2 = invert_truthvalue_loc (loc, unshare_expr (c));
		add_to_dst_predicate_list (loop, false_edge, cond, c2);

		cond = NULL_TREE;
		break;
	      }

	    case GIMPLE_SWITCH:
	      /* Not handled yet in if-conversion.  */
	      return false;

	    default:
	      gcc_unreachable ();
	    }
	}

      /* If current bb has only one successor, then consider it as an
	 unconditional goto.  */
      if (single_succ_p (bb))
	{
	  basic_block bb_n = single_succ (bb);

	  /* The successor bb inherits the predicate of its
	     predecessor.  If there is no predicate in the predecessor
	     bb, then consider the successor bb as always executed.  */
	  if (cond == NULL_TREE)
	    cond = boolean_true_node;

	  add_to_predicate_list (bb_n, cond);
	}
    }

  /* The loop header is always executed.  */
  loop->header->aux = boolean_true_node;

  return true;
}

/* Return true when LOOP is if-convertible.
   LOOP is if-convertible if:
   - it is innermost,
   - it has two or more basic blocks,
   - it has only one exit,
   - loop header is not the exit edge,
   - if its basic blocks and phi nodes are if convertible.  */

static bool
if_convertible_loop_p (struct loop *loop)
{
  unsigned int i;
  edge e;
  edge_iterator ei;
  basic_block exit_bb = NULL;

  /* Handle only innermost loop.  */
  if (!loop || loop->inner)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "not innermost loop\n");
      return false;
    }

  /* If only one block, no need for if-conversion.  */
  if (loop->num_nodes <= 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "less than 2 basic blocks\n");
      return false;
    }

  /* More than one loop exit is too much to handle.  */
  if (!single_exit (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "multiple exits\n");
      return false;
    }

  /* ??? Check target's vector conditional operation support for vectorizer.  */

  /* If one of the loop header's edge is exit edge then do not apply
     if-conversion.  */
  FOR_EACH_EDGE (e, ei, loop->header->succs)
    {
      if (loop_exit_edge_p (loop, e))
	return false;
    }

  /* Don't if-convert the loop when the data dependences cannot be
     computed: the loop won't be vectorized in that case.  */
  {
    VEC (data_reference_p, heap) *refs = VEC_alloc (data_reference_p, heap, 5);
    VEC (ddr_p, heap) *ddrs = VEC_alloc (ddr_p, heap, 25);
    bool res = compute_data_dependences_for_loop (loop, true, &refs, &ddrs);

    free_data_refs (refs);
    free_dependence_relations (ddrs);

    if (!res)
      return false;
  }

  calculate_dominance_info (CDI_DOMINATORS);

  /* Allow statements that can be handled during if-conversion.  */
  ifc_bbs = get_loop_body_in_if_conv_order (loop);
  if (!ifc_bbs)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Irreducible loop\n");
      return false;
    }

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];

      if (!if_convertible_bb_p (loop, bb, exit_bb))
	return false;

      if (bb_with_exit_edge_p (loop, bb))
	exit_bb = bb;
    }

  if (!predicate_bbs (loop))
    return false;

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      gimple_stmt_iterator itr;

      for (itr = gsi_start_phis (bb); !gsi_end_p (itr); gsi_next (&itr))
	if (!if_convertible_phi_p (loop, bb, gsi_stmt (itr)))
	  return false;

      /* For non predicated BBs, don't check their statements.  */
      if (!is_predicated (bb))
	continue;

      for (itr = gsi_start_bb (bb); !gsi_end_p (itr); gsi_next (&itr))
	if (!if_convertible_stmt_p (loop, bb, gsi_stmt (itr)))
	  return false;
    }

  if (dump_file)
    fprintf (dump_file, "Applying if-conversion\n");

  return true;
}

/* During if-conversion, the bb->aux field is used to hold a predicate
   list.  This function cleans for all the basic blocks in the given
   LOOP their predicate list.  */

static void
clean_predicate_lists (struct loop *loop)
{
  unsigned int i;
  basic_block *bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    bbs[i]->aux = NULL;

  free (bbs);
}

/* Basic block BB has two predecessors.  Using predecessor's bb->aux
   field, set appropriate condition COND for the PHI node replacement.
   Return true block whose phi arguments are selected when cond is
   true.  LOOP is the loop containing the if-converted region, GSI is
   the place to insert the code for the if-conversion.  */

static basic_block
find_phi_replacement_condition (struct loop *loop,
				basic_block bb, tree *cond,
                                gimple_stmt_iterator *gsi)
{
  edge first_edge, second_edge;
  tree tmp_cond;

  gcc_assert (EDGE_COUNT (bb->preds) == 2);
  first_edge = EDGE_PRED (bb, 0);
  second_edge = EDGE_PRED (bb, 1);

  /* Use condition based on following criteria:
     1)
       S1: x = !c ? a : b;

       S2: x = c ? b : a;

       S2 is preferred over S1. Make 'b' first_bb and use its condition.

     2) Do not make loop header first_bb.

     3)
       S1: x = !(c == d)? a : b;

       S21: t1 = c == d;
       S22: x = t1 ? b : a;

       S3: x = (c == d) ? b : a;

       S3 is preferred over S1 and S2*, Make 'b' first_bb and use
       its condition.

     4) If  pred B is dominated by pred A then use pred B's condition.
        See PR23115.  */

  /* Select condition that is not TRUTH_NOT_EXPR.  */
  tmp_cond = (tree) (first_edge->src)->aux;
  gcc_assert (tmp_cond);

  if (TREE_CODE (tmp_cond) == TRUTH_NOT_EXPR)
    {
      edge tmp_edge;

      tmp_edge = first_edge;
      first_edge = second_edge;
      second_edge = tmp_edge;
    }

  /* Check if FIRST_BB is loop header or not and make sure that
     FIRST_BB does not dominate SECOND_BB.  */
  if (first_edge->src == loop->header
      || dominated_by_p (CDI_DOMINATORS,
			 second_edge->src, first_edge->src))
    {
      *cond = (tree) (second_edge->src)->aux;

      if (TREE_CODE (*cond) == TRUTH_NOT_EXPR)
	*cond = invert_truthvalue (*cond);
      else
	/* Select non loop header bb.  */
	first_edge = second_edge;
    }
  else
    *cond = (tree) (first_edge->src)->aux;

  /* Gimplify the condition: the vectorizer prefers to have gimple
     values as conditions.  Various targets use different means to
     communicate conditions in vector compare operations.  Using a
     gimple value allows the compiler to emit vector compare and
     select RTL without exposing compare's result.  */
  *cond = force_gimple_operand_gsi (gsi, unshare_expr (*cond),
				    false, NULL_TREE,
				    true, GSI_SAME_STMT);
  if (!is_gimple_reg (*cond) && !is_gimple_condexpr (*cond))
    {
      gimple new_stmt;

      new_stmt = ifc_temp_var (TREE_TYPE (*cond), unshare_expr (*cond));
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
      *cond = gimple_assign_lhs (new_stmt);
    }

  gcc_assert (*cond);

  return first_edge->src;
}

/* Replace PHI node with conditional modify expr using COND.  This
   routine does not handle PHI nodes with more than two arguments.

   For example,
     S1: A = PHI <x1(1), x2(5)
   is converted into,
     S2: A = cond ? x1 : x2;

   The generated code is inserted at GSI that points to the top of
   basic block's statement list.  When COND is true, phi arg from
   TRUE_BB is selected.  */

static void
replace_phi_with_cond_gimple_assign_stmt (gimple phi, tree cond,
    					  basic_block true_bb,
                                   	  gimple_stmt_iterator *gsi)
{
  gimple new_stmt;
  basic_block bb;
  tree rhs;
  tree arg;

  gcc_assert (gimple_code (phi) == GIMPLE_PHI
	      && gimple_phi_num_args (phi) == 2);

  bb = gimple_bb (phi);

  arg = degenerate_phi_result (phi);
  if (arg)
    rhs = arg;
  else
    {
      tree arg_0, arg_1;
      /* Use condition that is not TRUTH_NOT_EXPR in conditional modify expr.  */
      if (EDGE_PRED (bb, 1)->src == true_bb)
	{
	  arg_0 = gimple_phi_arg_def (phi, 1);
	  arg_1 = gimple_phi_arg_def (phi, 0);
	}
      else
	{
	  arg_0 = gimple_phi_arg_def (phi, 0);
	  arg_1 = gimple_phi_arg_def (phi, 1);
	}

      /* Build new RHS using selected condition and arguments.  */
      rhs = build3 (COND_EXPR, TREE_TYPE (PHI_RESULT (phi)),
		    unshare_expr (cond), arg_0, arg_1);
    }

  new_stmt = gimple_build_assign (PHI_RESULT (phi), rhs);
  SSA_NAME_DEF_STMT (gimple_phi_result (phi)) = new_stmt;
  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
  update_stmt (new_stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "new phi replacement stmt\n");
      print_gimple_stmt (dump_file, new_stmt, 0, TDF_SLIM);
    }
}

/* Process phi nodes for the given LOOP.  Replace phi nodes with
   conditional modify expressions.  */

static void
process_phi_nodes (struct loop *loop)
{
  basic_block bb;
  unsigned int orig_loop_num_nodes = loop->num_nodes;
  unsigned int i;

  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      gimple phi;
      tree cond = NULL_TREE;
      gimple_stmt_iterator gsi, phi_gsi;
      basic_block true_bb = NULL;
      bb = ifc_bbs[i];

      if (bb == loop->header)
	continue;

      phi_gsi = gsi_start_phis (bb);
      gsi = gsi_after_labels (bb);

      /* BB has two predecessors.  Using predecessor's aux field, set
	 appropriate condition for the PHI node replacement.  */
      if (!gsi_end_p (phi_gsi))
	true_bb = find_phi_replacement_condition (loop, bb, &cond, &gsi);

      while (!gsi_end_p (phi_gsi))
	{
	  phi = gsi_stmt (phi_gsi);
	  replace_phi_with_cond_gimple_assign_stmt (phi, cond, true_bb, &gsi);
	  release_phi_node (phi);
	  gsi_next (&phi_gsi);
	}
      set_phi_nodes (bb, NULL);
    }
}

/* Remove all GIMPLE_CONDs and GIMPLE_LABELs of all the basic blocks
   other than the exit and latch of the LOOP.  */

static void
remove_conditions_and_labels (loop_p loop)
{
  gimple_stmt_iterator gsi;
  unsigned int i;

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs [i];

      if (bb_with_exit_edge_p (loop, bb)
        || bb == loop->latch)
      continue;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
      if (gimple_code (gsi_stmt (gsi)) == GIMPLE_COND
          || gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL)
        gsi_remove (&gsi, true);
      else
        gsi_next (&gsi);
    }
}

/* Combine all the basic blocks from LOOP into one or two super basic
   blocks.  Replace PHI nodes with conditional modify expressions.  */

static void
combine_blocks (struct loop *loop)
{
  basic_block bb, exit_bb, merge_target_bb;
  unsigned int orig_loop_num_nodes = loop->num_nodes;
  unsigned int i;
  edge e;
  edge_iterator ei;

  remove_conditions_and_labels (loop);

  /* Process phi nodes to prepare blocks for merge.  */
  process_phi_nodes (loop);

  /* Merge basic blocks: first remove all the edges in the loop,
     except for those from the exit block.  */
  exit_bb = NULL;
  for (i = 0; i < orig_loop_num_nodes; i++)
    {
      bb = ifc_bbs[i];
      if (bb_with_exit_edge_p (loop, bb))
	{
	  exit_bb = bb;
	  break;
	}
    }
  gcc_assert (exit_bb != loop->latch);

  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      bb = ifc_bbs[i];

      for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei));)
	{
	  if (e->src == exit_bb)
	    ei_next (&ei);
	  else
	    remove_edge (e);
	}
    }

  if (exit_bb != NULL)
    {
      if (exit_bb != loop->header)
	{
	  /* Connect this node to loop header.  */
	  make_edge (loop->header, exit_bb, EDGE_FALLTHRU);
	  set_immediate_dominator (CDI_DOMINATORS, exit_bb, loop->header);
	}

      /* Redirect non-exit edges to loop->latch.  */
      FOR_EACH_EDGE (e, ei, exit_bb->succs)
	{
	  if (!loop_exit_edge_p (loop, e))
	    redirect_edge_and_branch (e, loop->latch);
	}
      set_immediate_dominator (CDI_DOMINATORS, loop->latch, exit_bb);
    }
  else
    {
      /* If the loop does not have an exit, reconnect header and latch.  */
      make_edge (loop->header, loop->latch, EDGE_FALLTHRU);
      set_immediate_dominator (CDI_DOMINATORS, loop->latch, loop->header);
    }

  merge_target_bb = loop->header;
  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      gimple_stmt_iterator gsi;
      gimple_stmt_iterator last;

      bb = ifc_bbs[i];

      if (bb == exit_bb || bb == loop->latch)
	continue;

      /* Make stmts member of loop->header.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	gimple_set_bb (gsi_stmt (gsi), merge_target_bb);

      /* Update stmt list.  */
      last = gsi_last_bb (merge_target_bb);
      gsi_insert_seq_after (&last, bb_seq (bb), GSI_NEW_STMT);
      set_bb_seq (bb, NULL);

      delete_basic_block (bb);
    }

  /* If possible, merge loop header to the block with the exit edge.
     This reduces the number of basic blocks to two, to please the
     vectorizer that handles only loops with two nodes.

     FIXME: Call cleanup_tree_cfg.  */
  if (exit_bb
      && exit_bb != loop->header
      && can_merge_blocks_p (loop->header, exit_bb))
    merge_blocks (loop->header, exit_bb);
}

/* If-convert LOOP when it is legal.  For the moment this pass has no
   profitability analysis.  */

static void
tree_if_conversion (struct loop *loop)
{
  ifc_bbs = NULL;

  if (!if_convertible_loop_p (loop))
    goto cleanup;

  /* Now all statements are if-convertible.  Combine all the basic
     blocks into one huge basic block doing the if-conversion
     on-the-fly.  */
  combine_blocks (loop);

 cleanup:
  clean_predicate_lists (loop);
  if (ifc_bbs)
    {
      free (ifc_bbs);
      ifc_bbs = NULL;
    }
}

/* Tree if-conversion pass management.  */

static unsigned int
main_tree_if_conversion (void)
{
  loop_iterator li;
  struct loop *loop;

  if (number_of_loops () <= 1)
    return 0;

  FOR_EACH_LOOP (li, loop, 0)
    tree_if_conversion (loop);

  return 0;
}

static bool
gate_tree_if_conversion (void)
{
  return flag_tree_vectorize != 0;
}

struct gimple_opt_pass pass_if_conversion =
{
 {
  GIMPLE_PASS,
  "ifcvt",				/* name */
  gate_tree_if_conversion,		/* gate */
  main_tree_if_conversion,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_stmts | TODO_verify_flow
                                        /* todo_flags_finish */
 }
};
