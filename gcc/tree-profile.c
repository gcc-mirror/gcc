/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.
   Converted to use trees by Dale Johannesen, Apple Computer.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Generate basic block profile instrumentation and auxiliary files.
   Tree-based version.  See profile.c for overview.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "flags.h"
#include "output.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "toplev.h"
#include "coverage.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "value-prof.h"



/* Do initialization work for the edge profiler.  */

static void
tree_init_edge_profiler (void)
{
}

/* Output instructions as GIMPLE trees to increment the edge 
   execution count, and insert them on E.  We rely on 
   bsi_insert_on_edge to preserve the order.  */

static void
tree_gen_edge_profiler (int edgeno, edge e)
{
  tree gcov_type_node = get_gcov_type ();
  tree tmp1 = create_tmp_var (gcov_type_node, "PROF");
  tree tmp2 = create_tmp_var (gcov_type_node, "PROF");
  tree ref = tree_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
  tree stmt1 = build (MODIFY_EXPR, gcov_type_node, tmp1, ref);
  tree stmt2 = build (MODIFY_EXPR, gcov_type_node, tmp2,
		      build (PLUS_EXPR, gcov_type_node, 
			     tmp1, integer_one_node));
  tree stmt3 = build (MODIFY_EXPR, gcov_type_node, ref, tmp2);
  bsi_insert_on_edge (e, stmt1);
  bsi_insert_on_edge (e, stmt2);
  bsi_insert_on_edge (e, stmt3);
}

/* Output instructions as GIMPLE trees to increment the interval histogram 
   counter.  VALUE is the expression whose value is profiled.  TAG is the 
   tag of the section for counters, BASE is offset of the counter position.  */

static void
tree_gen_interval_profiler (histogram_value value, unsigned tag, unsigned base)
{
  tree op, op1, op2, op1copy, op2copy;
  tree tmp1, tmp2, tmp3, val, index;
  tree label_decl2, label_decl3, label_decl4, label_decl5, label_decl6;
  edge e12, e23, e34, e45, e56;
  tree label2, label3, label4, label5, label6;
  tree stmt1, stmt2, stmt3, stmt4;
  /* Initializations are to prevent bogus uninitialized warnings. */
  tree bb1end = NULL_TREE, bb2end = NULL_TREE, bb3end = NULL_TREE;
  tree bb4end = NULL_TREE, bb5end = NULL_TREE;
  tree ref = tree_coverage_counter_ref (tag, base), ref2;
  basic_block bb2, bb3, bb4, bb5, bb6;
  tree stmt = value->hvalue.tree.stmt;
  block_stmt_iterator bsi = bsi_for_stmt (stmt);
  basic_block bb = bb_for_stmt (stmt);
  tree gcov_type_node = get_gcov_type ();
  tree optype;

  op = stmt;
  if (TREE_CODE (stmt) == RETURN_EXPR
      && TREE_OPERAND (stmt, 0)
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
    op = TREE_OPERAND (stmt, 0);
  /* op == MODIFY_EXPR */
  op = TREE_OPERAND (op, 1);
  /* op == TRUNC_DIV or TRUNC_MOD */
  op1 = TREE_OPERAND (op, 0);
  op2 = TREE_OPERAND (op, 1);
  optype = TREE_TYPE (op);

  /* Blocks:
     Original = 1 
     For 2nd compare = 2
     Normal case, neither more nor less = 3
     More = 4
     Less = 5
     End = 6  */
  label_decl2 = create_artificial_label ();
  label_decl3 = create_artificial_label ();
  label_decl4 = create_artificial_label ();
  label_decl5 = create_artificial_label ();
  label_decl6 = create_artificial_label ();

  /* Do not evaluate op1 or op2 more than once.  Probably
     volatile loads are the only things that could cause
     a problem, but this is harmless in any case.  */
  op1copy = create_tmp_var (optype, "PROF");
  op2copy = create_tmp_var (optype, "PROF");
  stmt1 = build2 (MODIFY_EXPR, optype, op1copy, op1);
  stmt2 = build2 (MODIFY_EXPR, optype, op2copy, op2);
  TREE_OPERAND (op, 0) = op1copy;
  TREE_OPERAND (op, 1) = op2copy;

  val = create_tmp_var (optype, "PROF");
  stmt3 = build2 (MODIFY_EXPR, optype, val,
		  build2 (TRUNC_DIV_EXPR, optype, op1copy, op2copy));
  stmt4 = build2 (MODIFY_EXPR, optype, val,
		  build2 (MINUS_EXPR, optype, val,
				build_int_cst (optype, value->hdata.intvl.int_start)));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt4, BSI_SAME_STMT);

  index = create_tmp_var (gcov_type_node, "PROF");

  /* Check for too big.  */
  stmt1 = build3 (COND_EXPR, void_type_node,
	    build2 (GE_EXPR, boolean_type_node, val,
			build_int_cst (optype, value->hdata.intvl.steps)),
	    build1 (GOTO_EXPR, void_type_node, label_decl4),
	    build1 (GOTO_EXPR, void_type_node, label_decl2));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb1end = stmt1;

  /* Check for too small.  */
  label2 = build1 (LABEL_EXPR, void_type_node, label_decl2);
  bsi_insert_before (&bsi, label2, BSI_SAME_STMT);
  stmt1 = build3 (COND_EXPR, void_type_node,
	    build2 (LT_EXPR, boolean_type_node, val, integer_zero_node),
	    build1 (GOTO_EXPR, void_type_node, label_decl5),
	    build1 (GOTO_EXPR, void_type_node, label_decl3));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb2end = stmt1;

  /* Normal case, within range. */
  label3 = build1 (LABEL_EXPR, void_type_node, label_decl3);
  bsi_insert_before (&bsi, label3, BSI_SAME_STMT);
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, index,
		  build1 (NOP_EXPR, gcov_type_node, val));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb3end = stmt1;

  /* Too big */
  label4 = build1 (LABEL_EXPR, void_type_node, label_decl4);
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, index,
		  build_int_cst (gcov_type_node, value->hdata.intvl.steps));
  bsi_insert_before (&bsi, label4, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb4end = stmt1;

  /* Too small */
  label5 = build1 (LABEL_EXPR, void_type_node, label_decl5);
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, index,
		  build_int_cst (gcov_type_node,
				 value->hdata.intvl.steps + 1));
  bsi_insert_before (&bsi, label5, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb5end = stmt1;

  /* Increment appropriate counter. */
  label6 = build1 (LABEL_EXPR, void_type_node, label_decl6);
  bsi_insert_before (&bsi, label6, BSI_SAME_STMT);

  tmp1 = create_tmp_var (gcov_type_node, "PROF");
  tmp2 = create_tmp_var (gcov_type_node, "PROF");
  tmp3 = create_tmp_var (gcov_type_node, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, tmp1,
		  build2 (PLUS_EXPR, gcov_type_node, index,
			  TREE_OPERAND (ref, 1)));
  TREE_OPERAND (ref, 1) = tmp1;
  /* Make a copy to avoid sharing complaints. */
  ref2 = build4 (ARRAY_REF, TREE_TYPE (ref), TREE_OPERAND (ref, 0), 
		TREE_OPERAND (ref, 1), TREE_OPERAND (ref, 2), 
		TREE_OPERAND (ref, 3));

  stmt2 = build2 (MODIFY_EXPR, gcov_type_node, tmp2, ref);
  stmt3 = build2 (MODIFY_EXPR, gcov_type_node, tmp3,
		  build2 (PLUS_EXPR, gcov_type_node, tmp2, integer_one_node));
  stmt4 = build2 (MODIFY_EXPR, gcov_type_node, ref2, tmp3);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt4, BSI_SAME_STMT);

  /* Now fix up the CFG. */
  /* 1->2,4; 2->3,5; 3->6; 4->6; 5->6 */
  e12 = split_block (bb, bb1end);
  bb2 = e12->dest;
  e23 = split_block (bb2, bb2end);
  bb3 = e23->dest;
  e34 = split_block (bb3, bb3end);
  bb4 = e34->dest;
  e45 = split_block (bb4, bb4end);
  bb5 = e45->dest;
  e56 = split_block (bb5, bb5end);
  bb6 = e56->dest;

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_FALSE_VALUE;
  make_edge (bb, bb4, EDGE_TRUE_VALUE);
  e23->flags &= ~EDGE_FALLTHRU;
  e23->flags |= EDGE_FALSE_VALUE;
  make_edge (bb2, bb5, EDGE_TRUE_VALUE);
  remove_edge (e34);
  make_edge (bb3, bb6, EDGE_FALLTHRU);
  remove_edge (e45);
  make_edge (bb4, bb6, EDGE_FALLTHRU);
}

/* Output instructions as GIMPLE trees to increment the power of two histogram 
   counter.  VALUE is the expression whose value is profiled.  TAG is the tag 
   of the section for counters, BASE is offset of the counter position.  */

static void
tree_gen_pow2_profiler (histogram_value value, unsigned tag, unsigned base)
{
  tree op;
  tree tmp1, tmp2, tmp3;
  tree index, denom;
  tree label_decl1 = create_artificial_label ();
  tree label_decl2 = create_artificial_label ();
  tree label_decl3 = create_artificial_label ();
  tree label1, label2, label3;
  tree stmt1, stmt2, stmt3, stmt4;
  tree bb1end, bb2end, bb3end;
  tree ref = tree_coverage_counter_ref (tag, base), ref2;
  basic_block bb2, bb3, bb4;
  tree stmt = value->hvalue.tree.stmt;
  block_stmt_iterator bsi = bsi_for_stmt (stmt);
  basic_block bb = bb_for_stmt (stmt);
  tree gcov_type_node = get_gcov_type ();
  tree optype, optypesigned, optypeunsigned;

  op = stmt;
  if (TREE_CODE (stmt) == RETURN_EXPR
      && TREE_OPERAND (stmt, 0)
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
    op = TREE_OPERAND (stmt, 0);
  /* op == MODIFY_EXPR */
  op = TREE_OPERAND (op, 1);
  /* op == TRUNC_DIV or TRUNC_MOD */
  op = TREE_OPERAND (op, 1);
  /* op == denominator */
  optype = TREE_TYPE (op);
  if (TYPE_UNSIGNED (optype))
    {
      /* Right shift must be unsigned. */
      optypeunsigned = optype;
      optypesigned = build_distinct_type_copy (optype);
      TYPE_UNSIGNED (optypesigned) = false;
    }
  else
    {
      /* Compare to zero must be signed. */
      optypesigned = optype;
      optypeunsigned = build_distinct_type_copy (optype);
      TYPE_UNSIGNED (optypeunsigned) = true;
    }

  /* Set up variables and check if denominator is negative when considered
     as signed.  */
  index = create_tmp_var (gcov_type_node, "PROF");
  denom = create_tmp_var (optype, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, index, integer_zero_node);
  stmt2 = build2 (MODIFY_EXPR, optype, denom, op);
  if (optypesigned == optype)
    {
      tmp1 = denom;
      stmt3 = NULL_TREE;
    }
  else
    {
      tmp1 = create_tmp_var (optypesigned, "PROF");
      stmt3 = build2 (MODIFY_EXPR, optypesigned, tmp1,
			    build1 (NOP_EXPR, optypesigned, denom));
    }
  stmt4 = build3 (COND_EXPR, void_type_node,
		build2 (LE_EXPR, boolean_type_node, tmp1, integer_zero_node),
		build1 (GOTO_EXPR, void_type_node, label_decl3),
		build1 (GOTO_EXPR, void_type_node, label_decl1));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  if (stmt3)
    bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt4, BSI_SAME_STMT);
  bb1end = stmt4;

  /* Nonnegative.  Check if denominator is power of 2. */
  label1 = build1 (LABEL_EXPR, void_type_node, label_decl1);
  tmp1 = create_tmp_var (optype, "PROF");
  tmp2 = create_tmp_var (optype, "PROF");
  stmt1 = build2 (MODIFY_EXPR, optype, tmp1,
		    build2 (PLUS_EXPR, optype, denom, integer_minus_one_node));
  stmt2 = build2 (MODIFY_EXPR, optype, tmp2,
		    build2 (BIT_AND_EXPR, optype, tmp1, denom));
  stmt3 = build3 (COND_EXPR, void_type_node, 
		build2 (NE_EXPR, boolean_type_node, tmp2, integer_zero_node),
		build1 (GOTO_EXPR, void_type_node, label_decl3),
		build1 (GOTO_EXPR, void_type_node, label_decl2));
  bsi_insert_before (&bsi, label1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bb2end = stmt3;

  /* Loop.  Increment index, shift denominator, repeat if denominator nonzero. */
  label2 = build1 (LABEL_EXPR, void_type_node, label_decl2);
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, index,
		  build2 (PLUS_EXPR, gcov_type_node, index, integer_one_node));
  if (optypeunsigned == optype)
    {
      tmp1 = denom;
      stmt2 = NULL_TREE;
    }
  else
    {
      tmp1 = create_tmp_var (optypeunsigned, "PROF");
      stmt2 = build2 (MODIFY_EXPR, optypeunsigned, tmp1,
			build1 (NOP_EXPR, optypeunsigned, denom));
    }
  stmt3 = build2 (MODIFY_EXPR, optype, denom,
		    build2 (RSHIFT_EXPR, optype, tmp1, integer_one_node));
  stmt4 = build3 (COND_EXPR, void_type_node, 
		build2 (NE_EXPR, boolean_type_node, denom, integer_zero_node),
		build1 (GOTO_EXPR, void_type_node, label_decl2),
		build1 (GOTO_EXPR, void_type_node, label_decl3));
  bsi_insert_before (&bsi, label2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  if (stmt2)
    bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt4, BSI_SAME_STMT);
  bb3end = stmt4;

  /* Increment the appropriate counter.  */
  label3 = build1 (LABEL_EXPR, void_type_node, label_decl3);
  tmp1 = create_tmp_var (gcov_type_node, "PROF");
  tmp2 = create_tmp_var (gcov_type_node, "PROF");
  tmp3 = create_tmp_var (gcov_type_node, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, tmp1,
		  build2 (PLUS_EXPR, gcov_type_node,
			  index, TREE_OPERAND (ref, 1)));
  TREE_OPERAND (ref, 1) = tmp1;
  /* Make a copy to avoid sharing complaints. */
  ref2 = build4 (ARRAY_REF, TREE_TYPE (ref), TREE_OPERAND (ref, 0), 
		TREE_OPERAND (ref, 1), TREE_OPERAND (ref, 2), TREE_OPERAND (ref, 3));
  stmt2 = build2 (MODIFY_EXPR, gcov_type_node, tmp2, ref);
  stmt3 = build2 (MODIFY_EXPR, gcov_type_node, tmp3,
		  build2 (PLUS_EXPR, gcov_type_node, tmp2, integer_one_node));
  stmt4 = build2 (MODIFY_EXPR, gcov_type_node, ref2, tmp3);
  bsi_insert_before (&bsi, label3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt4, BSI_SAME_STMT);

  /* Now fix up the CFG. */
  bb2 = (split_block (bb, bb1end))->dest;
  bb3 = (split_block (bb2, bb2end))->dest;
  bb4 = (split_block (bb3, bb3end))->dest;

  EDGE_SUCC (bb, 0)->flags &= ~EDGE_FALLTHRU;
  EDGE_SUCC (bb, 0)->flags |= EDGE_FALSE_VALUE;
  make_edge (bb, bb4, EDGE_TRUE_VALUE);

  EDGE_SUCC (bb2, 0)->flags &= ~EDGE_FALLTHRU;
  EDGE_SUCC (bb2, 0)->flags |= EDGE_FALSE_VALUE;
  make_edge (bb2, bb4, EDGE_TRUE_VALUE);

  EDGE_SUCC (bb3, 0)->flags &= ~EDGE_FALLTHRU;
  EDGE_SUCC (bb3, 0)->flags |= EDGE_FALSE_VALUE;
  make_edge (bb3, bb3, EDGE_TRUE_VALUE);
}

/* Output instructions as GIMPLE trees for code to find the most common value.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
tree_gen_one_value_profiler (histogram_value value, unsigned tag, unsigned base)
{
  tree op;
  tree tmp1, tmp2, tmp3;
  tree label_decl1 = create_artificial_label ();
  tree label_decl2 = create_artificial_label ();
  tree label_decl3 = create_artificial_label ();
  tree label_decl4 = create_artificial_label ();
  tree label_decl5 = create_artificial_label ();
  tree label1, label2, label3, label4, label5;
  tree stmt1, stmt2, stmt3, stmt4;
  tree bb1end, bb2end, bb3end, bb4end, bb5end;
  tree ref1 = tree_coverage_counter_ref (tag, base);
  tree ref2 = tree_coverage_counter_ref (tag, base + 1);
  tree ref3 = tree_coverage_counter_ref (tag, base + 2);
  basic_block bb2, bb3, bb4, bb5, bb6;
  tree stmt = value->hvalue.tree.stmt;
  block_stmt_iterator bsi = bsi_for_stmt (stmt);
  basic_block bb = bb_for_stmt (stmt);
  tree gcov_type_node = get_gcov_type ();
  tree optype;

  op = stmt;
  if (TREE_CODE (stmt) == RETURN_EXPR
      && TREE_OPERAND (stmt, 0)
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
    op = TREE_OPERAND (stmt, 0);
  /* op == MODIFY_EXPR */
  op = TREE_OPERAND (op, 1);
  /* op == TRUNC_DIV or TRUNC_MOD */
  op = TREE_OPERAND (op, 1);
  /* op == denominator */
  optype = TREE_TYPE (op);

  /* Check if the stored value matches. */
  tmp1 = create_tmp_var (gcov_type_node, "PROF");
  tmp2 = create_tmp_var (optype, "PROF");
  tmp3 = create_tmp_var (optype, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, tmp1, ref1);
  stmt2 = build2 (MODIFY_EXPR, optype, tmp2, 
		    build1 (NOP_EXPR, optype, tmp1));
  stmt3 = build2 (MODIFY_EXPR, optype, tmp3, op);
  stmt4 = build3 (COND_EXPR, void_type_node, 
		build2 (EQ_EXPR, boolean_type_node, tmp2, tmp3),
		build1 (GOTO_EXPR, void_type_node, label_decl4),
		build1 (GOTO_EXPR, void_type_node, label_decl1));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt4, BSI_SAME_STMT);
  bb1end = stmt4;

  /* Does not match; check whether the counter is zero. */
  label1 = build1 (LABEL_EXPR, void_type_node, label_decl1);
  tmp1 = create_tmp_var (gcov_type_node, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, tmp1, ref2);
  stmt2 = build3 (COND_EXPR, void_type_node, 
		build2 (EQ_EXPR, boolean_type_node, tmp1, integer_zero_node),
		build1 (GOTO_EXPR, void_type_node, label_decl3), 
		build1 (GOTO_EXPR, void_type_node, label_decl2));
  bsi_insert_before (&bsi, label1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bb2end = stmt2;

  /* Counter is not zero yet, decrement. */
  label2 = build1 (LABEL_EXPR, void_type_node, label_decl2);
  tmp1 = create_tmp_var (gcov_type_node, "PROF");
  tmp2 = create_tmp_var (gcov_type_node, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, tmp1, ref2);
  stmt2 = build2 (MODIFY_EXPR, gcov_type_node, tmp2,
		  build (MINUS_EXPR, gcov_type_node, tmp1, integer_one_node));
  stmt3 = build2 (MODIFY_EXPR, gcov_type_node, ref2, tmp2);
  bsi_insert_before (&bsi, label2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bb3end = stmt3;

  /* Counter was zero, store new value. */
  label3 = build1 (LABEL_EXPR, void_type_node, label_decl3);
  tmp1 = create_tmp_var (optype, "PROF");
  tmp2 = create_tmp_var (gcov_type_node, "PROF");
  stmt1 = build2 (MODIFY_EXPR, optype, tmp1, op);
  stmt2 = build2 (MODIFY_EXPR, gcov_type_node, tmp2,
		  build1 (NOP_EXPR, gcov_type_node, tmp1));
  stmt3 = build2 (MODIFY_EXPR, gcov_type_node, ref1, tmp2);
  bsi_insert_before (&bsi, label3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bb4end = stmt3;
  /* (fall through) */

  /* Increment counter.  */
  label4 = build1 (LABEL_EXPR, void_type_node, label_decl4);
  tmp1 = create_tmp_var (gcov_type_node, "PROF");
  tmp2 = create_tmp_var (gcov_type_node, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, tmp1, ref2);
  stmt2 = build2 (MODIFY_EXPR, gcov_type_node, tmp2,
		  build (PLUS_EXPR, gcov_type_node, tmp1, integer_one_node));
  stmt3 = build2 (MODIFY_EXPR, gcov_type_node, ref2, tmp2);
  bsi_insert_before (&bsi, label4, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bb5end = stmt3;

  /* Increment the counter of all executions; this seems redundant given
     that we have counts for edges in cfg, but it may happen that some
     optimization will change the counts for the block (either because
     it is unable to update them correctly, or because it will duplicate
     the block or its part).  */
  label5 = build1 (LABEL_EXPR, void_type_node, label_decl5);
  tmp1 = create_tmp_var (gcov_type_node, "PROF");
  tmp2 = create_tmp_var (gcov_type_node, "PROF");
  stmt1 = build2 (MODIFY_EXPR, gcov_type_node, tmp1, ref3);
  stmt2 = build2 (MODIFY_EXPR, gcov_type_node, tmp2,
		  build (PLUS_EXPR, gcov_type_node, tmp1, integer_one_node));
  stmt3 = build2 (MODIFY_EXPR, gcov_type_node, ref3, tmp2);
  bsi_insert_before (&bsi, label5, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);

  /* Now fix up the CFG. */
  bb2 = (split_block (bb, bb1end))->dest;
  bb3 = (split_block (bb2, bb2end))->dest;
  bb4 = (split_block (bb3, bb3end))->dest;
  bb5 = (split_block (bb4, bb4end))->dest;
  bb6 = (split_block (bb5, bb5end))->dest;

  EDGE_SUCC (bb, 0)->flags &= ~EDGE_FALLTHRU;
  EDGE_SUCC (bb, 0)->flags |= EDGE_FALSE_VALUE;
  make_edge (bb, bb5, EDGE_TRUE_VALUE);

  EDGE_SUCC (bb2, 0)->flags &= ~EDGE_FALLTHRU;
  EDGE_SUCC (bb2, 0)->flags |= EDGE_FALSE_VALUE;
  make_edge (bb2, bb4, EDGE_TRUE_VALUE);

  remove_edge (EDGE_SUCC (bb3, 0));
  make_edge (bb3, bb6, EDGE_FALLTHRU);
}

/* Output instructions as GIMPLE trees for code to find the most common value 
   of a difference between two evaluations of an expression.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
tree_gen_const_delta_profiler (histogram_value value ATTRIBUTE_UNUSED, 
				unsigned tag ATTRIBUTE_UNUSED,
				unsigned base ATTRIBUTE_UNUSED)
{
  /* FIXME implement this.  */
#ifdef ENABLE_CHECKING
  internal_error ("unimplemented functionality");
#endif
  gcc_unreachable ();
}

/* Return 1 if tree-based profiling is in effect, else 0.
   If it is, set up hooks for tree-based profiling.
   Gate for pass_tree_profile.  */

static bool
do_tree_profiling (void)
{
  if (flag_tree_based_profiling
      && (profile_arc_flag || flag_test_coverage || flag_branch_probabilities))
    {
      tree_register_profile_hooks ();
      tree_register_value_prof_hooks ();
      return true;
    }
  return false;
}

/* Return the file on which profile dump output goes, if any.  */

static FILE *tree_profile_dump_file (void) {
  return dump_file;
}

static void
tree_profiling (void)
{
  branch_prob ();
  if (flag_branch_probabilities
      && flag_profile_values
      && flag_value_profile_transformations)
    value_profile_transformations ();
  /* The above could hose dominator info.  Currently there is
     none coming in, this is a safety valve.  It should be
     easy to adjust it, if and when there is some.  */
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
}

struct tree_opt_pass pass_tree_profile = 
{
  "tree_profile",			/* name */
  do_tree_profiling,			/* gate */
  tree_profiling,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_BRANCH_PROB,			/* tv_id */
  PROP_gimple_leh | PROP_cfg,		/* properties_required */
  PROP_gimple_leh | PROP_cfg,		/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_verify_stmts,			/* todo_flags_finish */
  0					/* letter */
};

struct profile_hooks tree_profile_hooks =
{
  tree_init_edge_profiler,      /* init_edge_profiler */
  tree_gen_edge_profiler,	/* gen_edge_profiler */
  tree_gen_interval_profiler,   /* gen_interval_profiler */
  tree_gen_pow2_profiler,       /* gen_pow2_profiler */
  tree_gen_one_value_profiler,  /* gen_one_value_profiler */
  tree_gen_const_delta_profiler,/* gen_const_delta_profiler */
  tree_profile_dump_file	/* profile_dump_file */
};
