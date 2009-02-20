/* If-conversion for vectorizer.
   Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
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

/* This pass implements tree level if-conversion transformation of loops.
   Initial goal is to help vectorizer vectorize loops with conditions.

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
#include "c-common.h"
#include "flags.h"
#include "timevar.h"
#include "varray.h"
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "target.h"


/* local function prototypes */
static unsigned int main_tree_if_conversion (void);
static tree tree_if_convert_stmt (struct loop *loop, gimple, tree,
				  gimple_stmt_iterator *);
static void tree_if_convert_cond_stmt (struct loop *, gimple, tree,
				       gimple_stmt_iterator *);
static bool if_convertible_phi_p (struct loop *, basic_block, gimple);
static bool if_convertible_gimple_assign_stmt_p (struct loop *, basic_block,
    						 gimple);
static bool if_convertible_stmt_p (struct loop *, basic_block, gimple);
static bool if_convertible_bb_p (struct loop *, basic_block, basic_block);
static bool if_convertible_loop_p (struct loop *, bool);
static void add_to_predicate_list (basic_block, tree);
static tree add_to_dst_predicate_list (struct loop * loop, edge,
				       tree, tree,
				       gimple_stmt_iterator *);
static void clean_predicate_lists (struct loop *loop);
static basic_block find_phi_replacement_condition (struct loop *loop,
						   basic_block, tree *,
						   gimple_stmt_iterator *);
static void replace_phi_with_cond_gimple_assign_stmt (gimple, tree,
						      basic_block,
						      gimple_stmt_iterator *);
static void process_phi_nodes (struct loop *);
static void combine_blocks (struct loop *);
static gimple ifc_temp_var (tree, tree);
static bool pred_blocks_visited_p (basic_block, bitmap *);
static basic_block * get_loop_body_in_if_conv_order (const struct loop *loop);
static bool bb_with_exit_edge_p (struct loop *, basic_block);

/* List of basic blocks in if-conversion-suitable order.  */
static basic_block *ifc_bbs;

/* Main entry point.
   Apply if-conversion to the LOOP. Return true if successful otherwise return
   false. If false is returned then loop remains unchanged.
   FOR_VECTORIZER is a boolean flag. It indicates whether if-conversion is used
   for vectorizer or not. If it is used for vectorizer, additional checks are
   used. (Vectorization checks are not yet implemented).  */

static bool
tree_if_conversion (struct loop *loop, bool for_vectorizer)
{
  basic_block bb;
  gimple_stmt_iterator itr;
  unsigned int i;

  ifc_bbs = NULL;

  /* if-conversion is not appropriate for all loops. First, check if loop  is
     if-convertible or not.  */
  if (!if_convertible_loop_p (loop, for_vectorizer))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,"-------------------------\n");
      if (ifc_bbs)
	{
	  free (ifc_bbs);
	  ifc_bbs = NULL;
	}
      free_dominance_info (CDI_POST_DOMINATORS);
      return false;
    }

  /* Do actual work now.  */
  for (i = 0; i < loop->num_nodes; i++)
    {
      tree cond;

      bb = ifc_bbs [i];

      /* Update condition using predicate list.  */
      cond = (tree) bb->aux;

      /* Process all statements in this basic block.
	 Remove conditional expression, if any, and annotate
	 destination basic block(s) appropriately.  */
      for (itr = gsi_start_bb (bb); !gsi_end_p (itr); /* empty */)
	{
	  gimple t = gsi_stmt (itr);
	  cond = tree_if_convert_stmt (loop, t, cond, &itr);
	  if (!gsi_end_p (itr))
	    gsi_next (&itr);
	}

      /* If current bb has only one successor, then consider it as an
	 unconditional goto.  */
      if (single_succ_p (bb))
	{
	  basic_block bb_n = single_succ (bb);

	  /* Successor bb inherits predicate of its predecessor. If there
	     is no predicate in predecessor bb, then consider successor bb
	     as always executed.  */
	  if (cond == NULL_TREE)
	    cond = boolean_true_node;

	  add_to_predicate_list (bb_n, cond);
	}
    }

  /* Now, all statements are if-converted and basic blocks are
     annotated appropriately. Combine all basic block into one huge
     basic block.  */
  combine_blocks (loop);

  /* clean up */
  clean_predicate_lists (loop);
  free (ifc_bbs);
  ifc_bbs = NULL;

  return true;
}

/* if-convert stmt T which is part of LOOP.
   If T is a GIMPLE_ASSIGN then it is converted into conditional modify
   expression using COND.  For conditional expressions, add condition in the
   destination basic block's predicate list and remove conditional
   expression itself. BSI is the iterator used to traverse statements of
   loop. It is used here when it is required to delete current statement.  */

static tree
tree_if_convert_stmt (struct loop *  loop, gimple t, tree cond,
		      gimple_stmt_iterator *gsi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "------if-convert stmt\n");
      print_gimple_stmt (dump_file, t, 0, TDF_SLIM);
      print_generic_stmt (dump_file, cond, TDF_SLIM);
    }

  switch (gimple_code (t))
    {
      /* Labels are harmless here.  */
    case GIMPLE_LABEL:
      break;

    case GIMPLE_ASSIGN:
      /* This GIMPLE_ASSIGN is killing previous value of LHS. Appropriate
	 value will be selected by PHI node based on condition. It is possible
	 that before this transformation, PHI nodes was selecting default
	 value and now it will use this new value. This is OK because it does 
	 not change validity the program.  */
      break;

    case GIMPLE_COND:
      /* Update destination blocks' predicate list and remove this
	 condition expression.  */
      tree_if_convert_cond_stmt (loop, t, cond, gsi);
      cond = NULL_TREE;
      break;

    default:
      gcc_unreachable ();
    }
  return cond;
}

/* STMT is a GIMPLE_COND. Update two destination's predicate list.
   Remove COND_EXPR, if it is not the loop exit condition. Otherwise
   update loop exit condition appropriately.  GSI is the iterator
   used to traverse statement list. STMT is part of loop LOOP.  */

static void
tree_if_convert_cond_stmt (struct loop *loop, gimple stmt, tree cond,
			   gimple_stmt_iterator *gsi)
{
  tree c, c2;
  edge true_edge, false_edge;

  gcc_assert (gimple_code (stmt) == GIMPLE_COND);

  c = fold_build2 (gimple_cond_code (stmt), boolean_type_node,
		   gimple_cond_lhs (stmt), gimple_cond_rhs (stmt));

  extract_true_false_edges_from_block (gimple_bb (stmt),
 				       &true_edge, &false_edge);

  /* Add new condition into destination's predicate list.  */

  /* If C is true then TRUE_EDGE is taken.  */
  add_to_dst_predicate_list (loop, true_edge, cond, c, gsi);

  /* If 'c' is false then FALSE_EDGE is taken.  */
  c2 = invert_truthvalue (unshare_expr (c));
  add_to_dst_predicate_list (loop, false_edge, cond, c2, gsi);

  /* Now this conditional statement is redundant. Remove it.
     But, do not remove exit condition! Update exit condition
     using new condition.  */
  if (!bb_with_exit_edge_p (loop, gimple_bb (stmt)))
    {
      gsi_remove (gsi, true);
      cond = NULL_TREE;
    }
  return;
}

/* Return true, iff PHI is if-convertible. PHI is part of loop LOOP
   and it belongs to basic block BB.
   PHI is not if-convertible
   - if it has more than 2 arguments.
   - Virtual PHI is immediately used in another PHI node.
   - Virtual PHI on BB other than header.  */

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

/* Return true, if STMT is if-convertible.
   GIMPLE_ASSIGN statement is not if-convertible if,
   - It is not movable.
   - It could trap.
   - LHS is not var decl.
  GIMPLE_ASSIGN is part of block BB, which is inside loop LOOP.  */

static bool
if_convertible_gimple_assign_stmt_p (struct loop *loop, basic_block bb,
    				     gimple stmt)
{
  tree lhs;

  if (!is_gimple_assign (stmt))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "-------------------------\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  lhs = gimple_assign_lhs (stmt);

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

  /* See if it needs speculative loading or not.  */
  if (bb != loop->header
      && gimple_assign_rhs_could_trap_p (stmt))
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

/* Return true, iff STMT is if-convertible.
   Statement is if-convertible if,
   - It is if-convertible GIMPLE_ASSGIN
   - It is GIMPLE_LABEL or GIMPLE_COND.
   STMT is inside block BB, which is inside loop LOOP.  */

static bool
if_convertible_stmt_p (struct loop *loop, basic_block bb, gimple stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_LABEL:
      break;

    case GIMPLE_ASSIGN:

      if (!if_convertible_gimple_assign_stmt_p (loop, bb, stmt))
	return false;
      break;

    case GIMPLE_COND:
      break;

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

/* Return true, iff BB is if-convertible.
   Note: This routine does _not_ check basic block statements and phis.
   Basic block is not if-convertible if,
   - Basic block is non-empty and it is after exit block (in BFS order).
   - Basic block is after exit block but before latch.
   - Basic block edge(s) is not normal.
   EXIT_BB_SEEN is true if basic block with exit edge is already seen.
   BB is inside loop LOOP.  */

static bool
if_convertible_bb_p (struct loop *loop, basic_block bb, basic_block exit_bb)
{
  edge e;
  edge_iterator ei;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "----------[%d]-------------\n", bb->index);

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
	  fprintf (dump_file,"Difficult to handle edges\n");
	return false;
      }

  return true;
}

/* Return true, iff LOOP is if-convertible.
   LOOP is if-convertible if,
   - It is innermost.
   - It has two or more basic blocks.
   - It has only one exit.
   - Loop header is not the exit edge.
   - If its basic blocks and phi nodes are if convertible. See above for
     more info.
   FOR_VECTORIZER enables vectorizer specific checks. For example, support
   for vector conditions, data dependency checks etc.. (Not implemented yet).  */

static bool
if_convertible_loop_p (struct loop *loop, bool for_vectorizer ATTRIBUTE_UNUSED)
{
  basic_block bb;
  gimple_stmt_iterator itr;
  unsigned int i;
  edge e;
  edge_iterator ei;
  basic_block exit_bb = NULL;

  /* Handle only inner most loop.  */
  if (!loop || loop->inner)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "not inner most loop\n");
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

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  /* Allow statements that can be handled during if-conversion.  */
  ifc_bbs = get_loop_body_in_if_conv_order (loop);
  if (!ifc_bbs)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,"Irreducible loop\n");
      free_dominance_info (CDI_POST_DOMINATORS);
      return false;
    }

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = ifc_bbs[i];

      if (!if_convertible_bb_p (loop, bb, exit_bb))
	return false;

      /* Check statements.  */
      for (itr = gsi_start_bb (bb); !gsi_end_p (itr); gsi_next (&itr))
	if (!if_convertible_stmt_p (loop, bb, gsi_stmt (itr)))
	  return false;
      /* ??? Check data dependency for vectorizer.  */

      /* What about phi nodes ? */
      itr = gsi_start_phis (bb);

      /* Clear aux field of incoming edges to a bb with a phi node.  */
      if (!gsi_end_p (itr))
	FOR_EACH_EDGE (e, ei, bb->preds)
	  e->aux = NULL;

      /* Check statements.  */
      for (; !gsi_end_p (itr); gsi_next (&itr))
	if (!if_convertible_phi_p (loop, bb, gsi_stmt (itr)))
	  return false;

      if (bb_with_exit_edge_p (loop, bb))
	exit_bb = bb;
    }

  /* OK. Did not find any potential issues so go ahead in if-convert
     this loop. Now there is no looking back.  */
  if (dump_file)
    fprintf (dump_file,"Applying if-conversion\n");

  free_dominance_info (CDI_POST_DOMINATORS);
  return true;
}

/* Add condition COND into predicate list of basic block BB.  */

static void
add_to_predicate_list (basic_block bb, tree new_cond)
{
  tree cond = (tree) bb->aux;

  if (cond)
    cond = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
			unshare_expr (cond), new_cond);
  else
    cond = new_cond;

  bb->aux = cond;
}

/* Add condition COND into BB's predicate list.  PREV_COND is
   existing condition.  */

static tree
add_to_dst_predicate_list (struct loop * loop, edge e,
			   tree prev_cond, tree cond,
			   gimple_stmt_iterator *gsi)
{
  tree new_cond = NULL_TREE;

  if (!flow_bb_inside_loop_p (loop, e->dest))
    return NULL_TREE;

  if (prev_cond == boolean_true_node || !prev_cond)
    new_cond = unshare_expr (cond);
  else
    {
      tree tmp;
      gimple tmp_stmt = NULL;

      prev_cond = force_gimple_operand_gsi (gsi, unshare_expr (prev_cond),
					    true, NULL, true, GSI_SAME_STMT);

      cond = force_gimple_operand_gsi (gsi, unshare_expr (cond),
				       true, NULL, true, GSI_SAME_STMT);

      /* Add the condition to aux field of the edge.  In case edge
	 destination is a PHI node, this condition will be ANDed with
	 block predicate to construct complete condition.  */
      e->aux = cond;

      /* new_cond == prev_cond AND cond */
      tmp = build2 (TRUTH_AND_EXPR, boolean_type_node,
		    unshare_expr (prev_cond), cond);
      tmp_stmt = ifc_temp_var (boolean_type_node, tmp);
      gsi_insert_before (gsi, tmp_stmt, GSI_SAME_STMT);
      new_cond = gimple_assign_lhs (tmp_stmt);
    }
  add_to_predicate_list (e->dest, new_cond);
  return new_cond;
}

/* During if-conversion aux field from basic block structure is used to hold
   predicate list. Clean each basic block's predicate list for the given LOOP.
   Also clean aux field of successor edges, used to hold true and false
   condition from conditional expression.  */

static void
clean_predicate_lists (struct loop *loop)
{
  basic_block *bb;
  unsigned int i;
  edge e;
  edge_iterator ei;

  bb = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      bb[i]->aux = NULL;
      FOR_EACH_EDGE (e, ei, bb[i]->succs)
	e->aux = NULL;
    }
  free (bb);
}

/* Basic block BB has two predecessors. Using predecessor's aux field, set
   appropriate condition COND for the PHI node replacement. Return true block
   whose phi arguments are selected when cond is true.  */

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

      /* If there is a condition on an incoming edge,
	 AND it with the incoming bb predicate.  */
      if (second_edge->aux)
	*cond = build2 (TRUTH_AND_EXPR, boolean_type_node,
			*cond, (tree) second_edge->aux);

      if (TREE_CODE (*cond) == TRUTH_NOT_EXPR)
	/* We can be smart here and choose inverted
	   condition without switching bbs.  */
	*cond = invert_truthvalue (*cond);
      else
	/* Select non loop header bb.  */
	first_edge = second_edge;
    }
  else
    {
      /* FIRST_BB is not loop header */
      *cond = (tree) (first_edge->src)->aux;

      /* If there is a condition on an incoming edge,
	 AND it with the incoming bb predicate.  */
      if (first_edge->aux)
	*cond = build2 (TRUTH_AND_EXPR, boolean_type_node,
			*cond, (tree) first_edge->aux);
    }

  /* Create temp. for the condition. Vectorizer prefers to have gimple
     value as condition. Various targets use different means to communicate
     condition in vector compare operation. Using gimple value allows
     compiler to emit vector compare and select RTL without exposing
     compare's result.  */
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


/* Replace PHI node with conditional modify expr using COND.
   This routine does not handle PHI nodes with more than two arguments.
   For example,
     S1: A = PHI <x1(1), x2(5)
   is converted into,
     S2: A = cond ? x1 : x2;
   S2 is inserted at the top of basic block's statement list.
   When COND is true, phi arg from TRUE_BB is selected.
*/

static void
replace_phi_with_cond_gimple_assign_stmt (gimple phi, tree cond,
    					  basic_block true_bb,
                                   	  gimple_stmt_iterator *gsi)
{
  gimple new_stmt;
  basic_block bb;
  tree rhs;
  tree arg_0, arg_1;

  gcc_assert (gimple_code (phi) == GIMPLE_PHI);
  
  /* If this is not filtered earlier, then now it is too late.  */
  gcc_assert (gimple_phi_num_args (phi) == 2);

  /* Find basic block and initialize iterator.  */
  bb = gimple_bb (phi);

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
	        unshare_expr (cond), unshare_expr (arg_0),
	        unshare_expr (arg_1));

  /* Create new GIMPLE_ASSIGN statement using RHS.  */
  new_stmt = gimple_build_assign (unshare_expr (PHI_RESULT (phi)), rhs);

  /* Make new statement definition of the original phi result.  */
  SSA_NAME_DEF_STMT (gimple_phi_result (phi)) = new_stmt;

  /* Insert using iterator.  */
  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
  update_stmt (new_stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "new phi replacement stmt\n");
      print_gimple_stmt (dump_file, new_stmt, 0, TDF_SLIM);
    }
}

/* Process phi nodes for the given  LOOP.  Replace phi nodes with cond
   modify expr.  */

static void
process_phi_nodes (struct loop *loop)
{
  basic_block bb;
  unsigned int orig_loop_num_nodes = loop->num_nodes;
  unsigned int i;

  /* Replace phi nodes with cond. modify expr.  */
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

      /* BB has two predecessors. Using predecessor's aux field, set
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
  return;
}

/* Combine all basic block from the given LOOP into one or two super
   basic block.  Replace PHI nodes with conditional modify expression.  */

static void
combine_blocks (struct loop *loop)
{
  basic_block bb, exit_bb, merge_target_bb;
  unsigned int orig_loop_num_nodes = loop->num_nodes;
  unsigned int i;
  edge e;
  edge_iterator ei;

  /* Process phi nodes to prepare blocks for merge.  */
  process_phi_nodes (loop);

  /* Merge basic blocks.  First remove all the edges in the loop, except
     for those from the exit block.  */
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
	  /* Connect this node with loop header.  */
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
      /* If the loop does not have exit then reconnect header and latch.  */
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

      /* Remove labels and make stmts member of loop->header.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
	{
	  if (gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL)
	    gsi_remove (&gsi, true);
	  else
	    {
	      gimple_set_bb (gsi_stmt (gsi), merge_target_bb);
	      gsi_next (&gsi);
	    }
	}

      /* Update stmt list.  */
      last = gsi_last_bb (merge_target_bb);
      gsi_insert_seq_after (&last, bb_seq (bb), GSI_NEW_STMT);
      set_bb_seq (bb, NULL);

      delete_basic_block (bb);
    }

  /* Now if possible, merge loop header and block with exit edge.
     This reduces number of basic blocks to 2. Auto vectorizer addresses
     loops with two nodes only.  FIXME: Use cleanup_tree_cfg().  */
  if (exit_bb
      && exit_bb != loop->header
      && can_merge_blocks_p (loop->header, exit_bb))
    merge_blocks (loop->header, exit_bb);
}

/* Make a new temp variable of type TYPE. Add GIMPLE_ASSIGN to assign EXP
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


/* Return TRUE iff, all pred blocks of BB are visited.
   Bitmap VISITED keeps history of visited blocks.  */

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

/* Get body of a LOOP in suitable order for if-conversion.
   It is caller's responsibility to deallocate basic block
   list.  If-conversion suitable order is, BFS order with one
   additional constraint. Select block in BFS block, if all
   pred are already selected.  */

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
	{
	  /* Not done yet.  */
	  index = 0;
	}
    }
  free (blocks_in_bfs_order);
  BITMAP_FREE (visited);
  return blocks;
}

/* Return true if one of the basic block BB edge is exit of LOOP.  */

static bool
bb_with_exit_edge_p (struct loop *loop, basic_block bb)
{
  edge e;
  edge_iterator ei;
  bool exit_edge_found = false;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (loop_exit_edge_p (loop, e))
      {
	exit_edge_found = true;
	break;
      }

  return exit_edge_found;
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
    {
      tree_if_conversion (loop, true);
    }
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
  0,					/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_loops | TODO_verify_stmts | TODO_verify_flow
                                        /* todo_flags_finish */
 }
};
