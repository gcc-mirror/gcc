/* Lower complex operations to scalar operations.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tm.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "flags.h"


/* Force EXP to be a gimple_val.  */

static tree
gimplify_val (block_stmt_iterator *bsi, tree type, tree exp)
{
  tree t, new_stmt, orig_stmt;

  if (is_gimple_val (exp))
    return exp;

  t = make_rename_temp (type, NULL);
  new_stmt = build (MODIFY_EXPR, type, t, exp);

  orig_stmt = bsi_stmt (*bsi);
  SET_EXPR_LOCUS (new_stmt, EXPR_LOCUS (orig_stmt));
  TREE_BLOCK (new_stmt) = TREE_BLOCK (orig_stmt);

  bsi_insert_before (bsi, new_stmt, BSI_SAME_STMT);

  return t;
}

/* Extract the real or imaginary part of a complex variable or constant.
   Make sure that it's a proper gimple_val and gimplify it if not.
   Emit any new code before BSI.  */

static tree
extract_component (block_stmt_iterator *bsi, tree t, bool imagpart_p)
{
  tree ret, inner_type;

  inner_type = TREE_TYPE (TREE_TYPE (t));
  switch (TREE_CODE (t))
    {
    case COMPLEX_CST:
      ret = (imagpart_p ? TREE_IMAGPART (t) : TREE_REALPART (t));
      break;

    case COMPLEX_EXPR:
      ret = TREE_OPERAND (t, imagpart_p);
      break;

    case VAR_DECL:
    case PARM_DECL:
      ret = build1 ((imagpart_p ? IMAGPART_EXPR : REALPART_EXPR),
		    inner_type, t);
      break;

    default:
      abort ();
    }

  return gimplify_val (bsi, inner_type, ret);
}

/* Build a binary operation and gimplify it.  Emit code before BSI.
   Return the gimple_val holding the result.  */

static tree
do_binop (block_stmt_iterator *bsi, enum tree_code code,
	  tree type, tree a, tree b)
{
  tree ret;

  ret = fold (build (code, type, a, b));
  STRIP_NOPS (ret);

  return gimplify_val (bsi, type, ret);
}

/* Build a unary operation and gimplify it.  Emit code before BSI.
   Return the gimple_val holding the result.  */

static tree
do_unop (block_stmt_iterator *bsi, enum tree_code code, tree type, tree a)
{
  tree ret;

  ret = fold (build1 (code, type, a));
  STRIP_NOPS (ret);

  return gimplify_val (bsi, type, ret);
}

/* Update an assignment to a complex variable in place.  */

static void
update_complex_assignment (block_stmt_iterator *bsi, tree r, tree i)
{
  tree stmt = bsi_stmt (*bsi);
  tree type;

  modify_stmt (stmt);
  if (TREE_CODE (stmt) == RETURN_EXPR)
    stmt = TREE_OPERAND (stmt, 0);
  
  type = TREE_TYPE (TREE_OPERAND (stmt, 1));
  TREE_OPERAND (stmt, 1) = build (COMPLEX_EXPR, type, r, i);
}

/* Expand complex addition to scalars:
	a + b = (ar + br) + i(ai + bi)
	a - b = (ar - br) + i(ai + bi)
*/

static void
expand_complex_addition (block_stmt_iterator *bsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code)
{
  tree rr, ri;

  rr = do_binop (bsi, code, inner_type, ar, br);
  ri = do_binop (bsi, code, inner_type, ai, bi);

  update_complex_assignment (bsi, rr, ri);
}

/* Expand complex multiplication to scalars:
	a * b = (ar*br - ai*bi) + i(ar*bi + br*ai)
*/

static void
expand_complex_multiplication (block_stmt_iterator *bsi, tree inner_type,
			       tree ar, tree ai, tree br, tree bi)
{
  tree t1, t2, t3, t4, rr, ri;

  t1 = do_binop (bsi, MULT_EXPR, inner_type, ar, br);
  t2 = do_binop (bsi, MULT_EXPR, inner_type, ai, bi);
  t3 = do_binop (bsi, MULT_EXPR, inner_type, ar, bi);

  /* Avoid expanding redundant multiplication for the common
     case of squaring a complex number.  */
  if (ar == br && ai == bi)
    t4 = t3;
  else
    t4 = do_binop (bsi, MULT_EXPR, inner_type, ai, br);

  rr = do_binop (bsi, MINUS_EXPR, inner_type, t1, t2);
  ri = do_binop (bsi, PLUS_EXPR, inner_type, t3, t4);

  update_complex_assignment (bsi, rr, ri);
}

/* Expand complex division to scalars, straightforward algorithm.
	a / b = ((ar*br + ai*bi)/t) + i((ai*br - ar*bi)/t)
	    t = br*br + bi*bi
*/

static void
expand_complex_div_straight (block_stmt_iterator *bsi, tree inner_type,
			     tree ar, tree ai, tree br, tree bi,
			     enum tree_code code)
{
  tree rr, ri, div, t1, t2, t3;

  t1 = do_binop (bsi, MULT_EXPR, inner_type, br, br);
  t2 = do_binop (bsi, MULT_EXPR, inner_type, bi, bi);
  div = do_binop (bsi, PLUS_EXPR, inner_type, t1, t2);

  t1 = do_binop (bsi, MULT_EXPR, inner_type, ar, br);
  t2 = do_binop (bsi, MULT_EXPR, inner_type, ai, bi);
  t3 = do_binop (bsi, PLUS_EXPR, inner_type, t1, t2);
  rr = do_binop (bsi, code, inner_type, t3, div);

  t1 = do_binop (bsi, MULT_EXPR, inner_type, ai, br);
  t2 = do_binop (bsi, MULT_EXPR, inner_type, ar, bi);
  t3 = do_binop (bsi, MINUS_EXPR, inner_type, t1, t2);
  ri = do_binop (bsi, code, inner_type, t3, div);

  update_complex_assignment (bsi, rr, ri);
}

/* Expand complex division to scalars, modified algorithm to minimize
   overflow with wide input ranges.  */

static void
expand_complex_div_wide (block_stmt_iterator *bsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code)
{
  tree rr, ri, ratio, div, t1, t2, min, max, cond;

  /* Examine |br| < |bi|, and branch.  */
  t1 = do_unop (bsi, ABS_EXPR, inner_type, br);
  t2 = do_unop (bsi, ABS_EXPR, inner_type, bi);
  cond = fold (build (LT_EXPR, boolean_type_node, t1, t2));
  STRIP_NOPS (cond);

  if (TREE_CONSTANT (cond))
    {
      if (integer_zerop (cond))
	min = bi, max = br;
      else
	min = br, max = bi;
    }
  else
    {
      basic_block bb_cond, bb_true, bb_false, bb_join;
      tree l1, l2, l3;
      edge e;

      l1 = create_artificial_label ();
      t1 = build (GOTO_EXPR, void_type_node, l1);
      l2 = create_artificial_label ();
      t2 = build (GOTO_EXPR, void_type_node, l2);
      cond = build (COND_EXPR, void_type_node, cond, t1, t2);
      bsi_insert_before (bsi, cond, BSI_SAME_STMT);

      min = make_rename_temp (inner_type, NULL);
      max = make_rename_temp (inner_type, NULL);
      l3 = create_artificial_label ();

      /* Split the original block, and create the TRUE and FALSE blocks.  */
      e = split_block (bsi->bb, cond);
      bb_cond = e->src;
      bb_join = e->dest;
      bb_true = create_empty_bb (bb_cond);
      bb_false = create_empty_bb (bb_true);

      /* Wire the blocks together.  */
      e->flags = EDGE_TRUE_VALUE;
      redirect_edge_succ (e, bb_true);
      make_edge (bb_cond, bb_false, EDGE_FALSE_VALUE);
      make_edge (bb_true, bb_join, 0);
      make_edge (bb_false, bb_join, 0);

      /* Update dominance info.  Note that bb_join's data was
         updated by split_block.  */
      if (dom_computed[CDI_DOMINATORS] >= DOM_CONS_OK)
        {
          set_immediate_dominator (CDI_DOMINATORS, bb_true, bb_cond);
          set_immediate_dominator (CDI_DOMINATORS, bb_false, bb_cond);
        }

      /* Compute min and max for TRUE block.  */
      *bsi = bsi_start (bb_true);
      t1 = build (LABEL_EXPR, void_type_node, l1);
      bsi_insert_after (bsi, t1, BSI_NEW_STMT);
      t1 = build (MODIFY_EXPR, inner_type, min, br);
      bsi_insert_after (bsi, t1, BSI_NEW_STMT);
      t1 = build (MODIFY_EXPR, inner_type, max, bi);
      bsi_insert_after (bsi, t1, BSI_NEW_STMT);

      /* Compute min and max for FALSE block.  */
      *bsi = bsi_start (bb_false);
      t1 = build (LABEL_EXPR, void_type_node, l2);
      bsi_insert_after (bsi, t1, BSI_NEW_STMT);
      t1 = build (MODIFY_EXPR, inner_type, min, bi);
      bsi_insert_after (bsi, t1, BSI_NEW_STMT);
      t1 = build (MODIFY_EXPR, inner_type, max, br);
      bsi_insert_after (bsi, t1, BSI_NEW_STMT);

      /* Insert the join label into the tail of the original block.  */
      *bsi = bsi_start (bb_join);
      t1 = build (LABEL_EXPR, void_type_node, l3);
      bsi_insert_before (bsi, t1, BSI_SAME_STMT);
    }
  
  /* Now we have MIN(|br|, |bi|) and MAX(|br|, |bi|).  We now use the
     ratio min/max to scale both the dividend and divisor.  */
  ratio = do_binop (bsi, code, inner_type, min, max);

  /* Calculate the divisor: min*ratio + max.  */
  t1 = do_binop (bsi, MULT_EXPR, inner_type, min, ratio);
  div = do_binop (bsi, PLUS_EXPR, inner_type, t1, max);

  /* Result is now ((ar + ai*ratio)/div) + i((ai - ar*ratio)/div). */
  t1 = do_binop (bsi, MULT_EXPR, inner_type, ai, ratio);
  t2 = do_binop (bsi, PLUS_EXPR, inner_type, ar, t1);
  rr = do_binop (bsi, code, inner_type, t2, div);

  t1 = do_binop (bsi, MULT_EXPR, inner_type, ar, ratio);
  t2 = do_binop (bsi, MINUS_EXPR, inner_type, ai, t1);
  ri = do_binop (bsi, code, inner_type, t2, div);

  update_complex_assignment (bsi, rr, ri);
}

/* Expand complex division to scalars.  */

static void
expand_complex_division (block_stmt_iterator *bsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code)
{
  switch (flag_complex_divide_method)
    {
    case 0:
      /* straightforward implementation of complex divide acceptable.  */
      expand_complex_div_straight (bsi, inner_type, ar, ai, br, bi, code);
      break;
    case 1:
      /* wide ranges of inputs must work for complex divide.  */
      expand_complex_div_wide (bsi, inner_type, ar, ai, br, bi, code);
      break;
    default:
      /* C99-like requirements for complex divide (not yet implemented).  */
      abort ();
    }
}

/* Expand complex negation to scalars:
	-a = (-ar) + i(-ai)
*/

static void
expand_complex_negation (block_stmt_iterator *bsi, tree inner_type,
			 tree ar, tree ai)
{
  tree rr, ri;

  rr = do_unop (bsi, NEGATE_EXPR, inner_type, ar);
  ri = do_unop (bsi, NEGATE_EXPR, inner_type, ai);

  update_complex_assignment (bsi, rr, ri);
}

/* Expand complex conjugate to scalars:
	~a = (ar) + i(-ai)
*/

static void
expand_complex_conjugate (block_stmt_iterator *bsi, tree inner_type,
			  tree ar, tree ai)
{
  tree ri;

  ri = do_unop (bsi, NEGATE_EXPR, inner_type, ai);

  update_complex_assignment (bsi, ar, ri);
}

/* Expand complex comparison (EQ or NE only).  */

static void
expand_complex_comparison (block_stmt_iterator *bsi, tree ar, tree ai,
			   tree br, tree bi, enum tree_code code)
{
  tree cr, ci, cc, stmt, type;

  cr = do_binop (bsi, code, boolean_type_node, ar, br);
  ci = do_binop (bsi, code, boolean_type_node, ai, bi);
  cc = do_binop (bsi, (code == EQ_EXPR ? TRUTH_AND_EXPR : TRUTH_OR_EXPR),
		 boolean_type_node, cr, ci);

  stmt = bsi_stmt (*bsi);
  modify_stmt (stmt);

  switch (TREE_CODE (stmt))
    {
    case RETURN_EXPR:
      stmt = TREE_OPERAND (stmt, 0);
      /* FALLTHRU */
    case MODIFY_EXPR:
      type = TREE_TYPE (TREE_OPERAND (stmt, 1));
      TREE_OPERAND (stmt, 1) = convert (type, cc);
      break;
    case COND_EXPR:
      TREE_OPERAND (stmt, 0) = cc;
      break;
    default:
      abort ();
    }
}

/* Process one statement.  If we identify a complex operation, expand it.  */

static void
expand_complex_operations_1 (block_stmt_iterator *bsi)
{
  tree stmt = bsi_stmt (*bsi);
  tree rhs, type, inner_type;
  tree ac, ar, ai, bc, br, bi;
  enum tree_code code;

  switch (TREE_CODE (stmt))
    {
    case RETURN_EXPR:
      stmt = TREE_OPERAND (stmt, 0);
      if (!stmt)
	return;
      if (TREE_CODE (stmt) != MODIFY_EXPR)
	return;
      /* FALLTHRU */

    case MODIFY_EXPR:
      rhs = TREE_OPERAND (stmt, 1);
      break;

    case COND_EXPR:
      rhs = TREE_OPERAND (stmt, 0);
      break;

    default:
      return;
    }

  type = TREE_TYPE (rhs);
  code = TREE_CODE (rhs);

  /* Initial filter for operations we handle.  */
  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
    case NEGATE_EXPR:
    case CONJ_EXPR:
      if (TREE_CODE (type) != COMPLEX_TYPE)
	return;
      inner_type = TREE_TYPE (type);
      break;

    case EQ_EXPR:
    case NE_EXPR:
      inner_type = TREE_TYPE (TREE_OPERAND (rhs, 1));
      if (TREE_CODE (inner_type) != COMPLEX_TYPE)
	return;
      break;

    default:
      return;
    }

  /* Extract the components of the two complex values.  Make sure and
     handle the common case of the same value used twice specially.  */
  ac = TREE_OPERAND (rhs, 0);
  ar = extract_component (bsi, ac, 0);
  ai = extract_component (bsi, ac, 1);

  if (TREE_CODE_CLASS (code) == '1')
    bc = br = bi = NULL;
  else
    {
      bc = TREE_OPERAND (rhs, 1);
      if (ac == bc)
	br = ar, bi = ai;
      else
	{
	  br = extract_component (bsi, bc, 0);
	  bi = extract_component (bsi, bc, 1);
	}
    }

  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      expand_complex_addition (bsi, inner_type, ar, ai, br, bi, code);
      break;

    case MULT_EXPR:
      expand_complex_multiplication (bsi, inner_type, ar, ai, br, bi);
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
      expand_complex_division (bsi, inner_type, ar, ai, br, bi, code);
      break;
      
    case NEGATE_EXPR:
      expand_complex_negation (bsi, inner_type, ar, ai);
      break;

    case CONJ_EXPR:
      expand_complex_conjugate (bsi, inner_type, ar, ai);
      break;

    case EQ_EXPR:
    case NE_EXPR:
      expand_complex_comparison (bsi, ar, ai, br, bi, code);
      break;

    default:
      abort ();
    }
}

/* Main loop to process each statement.  */
/* ??? Could use dominator bits to propagate from complex_expr at the
   same time.  This might reveal more constants, particularly in cases
   such as (complex = complex op scalar).  This may not be relevant
   after SRA and subsequent cleanups.  Proof of this would be if we
   verify that the code generated by expand_complex_div_wide is
   simplified properly to straight-line code.  */

static void
expand_complex_operations (void)
{
  int old_last_basic_block = last_basic_block;
  block_stmt_iterator bsi;
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      if (bb->index >= old_last_basic_block)
	continue;
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	expand_complex_operations_1 (&bsi);
    }
}

struct tree_opt_pass pass_lower_complex = 
{
  "complex",				/* name */
  NULL,					/* gate */
  expand_complex_operations,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars
    | TODO_ggc_collect | TODO_verify_ssa
    | TODO_verify_stmts | TODO_verify_flow /* todo_flags_finish */
};
