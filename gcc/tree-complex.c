/* Lower complex number operations to scalar operations.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "expr.h"
#include "insn-codes.h"
#include "diagnostic.h"
#include "optabs.h"
#include "machmode.h"
#include "langhooks.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "flags.h"
#include "ggc.h"


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
      gcc_unreachable ();
    }

  return gimplify_val (bsi, inner_type, ret);
}

/* Update an assignment to a complex variable in place.  */

static void
update_complex_assignment (block_stmt_iterator *bsi, tree r, tree i)
{
  tree stmt = bsi_stmt (*bsi);
  tree type;

  if (TREE_CODE (stmt) == RETURN_EXPR)
    stmt = TREE_OPERAND (stmt, 0);
  
  type = TREE_TYPE (TREE_OPERAND (stmt, 1));
  TREE_OPERAND (stmt, 1) = build (COMPLEX_EXPR, type, r, i);
  mark_stmt_modified (stmt);
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

  rr = gimplify_build2 (bsi, code, inner_type, ar, br);
  ri = gimplify_build2 (bsi, code, inner_type, ai, bi);

  update_complex_assignment (bsi, rr, ri);
}

/* Expand a complex multiplication or division to a libcall to the c99
   compliant routines.  */

static void
expand_complex_libcall (block_stmt_iterator *bsi, tree ar, tree ai,
			tree br, tree bi, enum tree_code code)
{
  enum machine_mode mode;
  enum built_in_function bcode;
  tree args, fn, stmt, type;

  args = tree_cons (NULL, bi, NULL);
  args = tree_cons (NULL, br, args);
  args = tree_cons (NULL, ai, args);
  args = tree_cons (NULL, ar, args);

  stmt = bsi_stmt (*bsi);
  type = TREE_TYPE (TREE_OPERAND (stmt, 1));

  mode = TYPE_MODE (type);
  gcc_assert (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT);
  if (code == MULT_EXPR)
    bcode = BUILT_IN_COMPLEX_MUL_MIN + mode - MIN_MODE_COMPLEX_FLOAT;
  else if (code == RDIV_EXPR)
    bcode = BUILT_IN_COMPLEX_DIV_MIN + mode - MIN_MODE_COMPLEX_FLOAT;
  else
    gcc_unreachable ();
  fn = built_in_decls[bcode];

  TREE_OPERAND (stmt, 1)
    = build3 (CALL_EXPR, type, build_fold_addr_expr (fn), args, NULL);
  update_stmt (stmt);
}

/* Expand complex multiplication to scalars:
	a * b = (ar*br - ai*bi) + i(ar*bi + br*ai)
*/

static void
expand_complex_multiplication (block_stmt_iterator *bsi, tree inner_type,
			       tree ar, tree ai, tree br, tree bi)
{
  tree t1, t2, t3, t4, rr, ri;

  if (flag_complex_method == 2 && SCALAR_FLOAT_TYPE_P (inner_type))
    {
      expand_complex_libcall (bsi, ar, ai, br, bi, MULT_EXPR);
      return;
    }

  t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ar, br);
  t2 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ai, bi);
  t3 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ar, bi);

  /* Avoid expanding redundant multiplication for the common
     case of squaring a complex number.  */
  if (ar == br && ai == bi)
    t4 = t3;
  else
    t4 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ai, br);

  rr = gimplify_build2 (bsi, MINUS_EXPR, inner_type, t1, t2);
  ri = gimplify_build2 (bsi, PLUS_EXPR, inner_type, t3, t4);

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

  t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, br, br);
  t2 = gimplify_build2 (bsi, MULT_EXPR, inner_type, bi, bi);
  div = gimplify_build2 (bsi, PLUS_EXPR, inner_type, t1, t2);

  t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ar, br);
  t2 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ai, bi);
  t3 = gimplify_build2 (bsi, PLUS_EXPR, inner_type, t1, t2);
  rr = gimplify_build2 (bsi, code, inner_type, t3, div);

  t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ai, br);
  t2 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ar, bi);
  t3 = gimplify_build2 (bsi, MINUS_EXPR, inner_type, t1, t2);
  ri = gimplify_build2 (bsi, code, inner_type, t3, div);

  update_complex_assignment (bsi, rr, ri);
}

/* Expand complex division to scalars, modified algorithm to minimize
   overflow with wide input ranges.  */

static void
expand_complex_div_wide (block_stmt_iterator *bsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code)
{
  tree rr, ri, ratio, div, t1, t2, tr, ti, cond;
  basic_block bb_cond, bb_true, bb_false, bb_join;

  /* Examine |br| < |bi|, and branch.  */
  t1 = gimplify_build1 (bsi, ABS_EXPR, inner_type, br);
  t2 = gimplify_build1 (bsi, ABS_EXPR, inner_type, bi);
  cond = fold (build (LT_EXPR, boolean_type_node, t1, t2));
  STRIP_NOPS (cond);

  bb_cond = bb_true = bb_false = bb_join = NULL;
  rr = ri = tr = ti = NULL;
  if (!TREE_CONSTANT (cond))
    {
      edge e;

      cond = build (COND_EXPR, void_type_node, cond, NULL, NULL);
      bsi_insert_before (bsi, cond, BSI_SAME_STMT);

      /* Split the original block, and create the TRUE and FALSE blocks.  */
      e = split_block (bsi->bb, cond);
      bb_cond = e->src;
      bb_join = e->dest;
      bb_true = create_empty_bb (bb_cond);
      bb_false = create_empty_bb (bb_true);

      t1 = build (GOTO_EXPR, void_type_node, tree_block_label (bb_true));
      t2 = build (GOTO_EXPR, void_type_node, tree_block_label (bb_false));
      COND_EXPR_THEN (cond) = t1;
      COND_EXPR_ELSE (cond) = t2;

      /* Wire the blocks together.  */
      e->flags = EDGE_TRUE_VALUE;
      redirect_edge_succ (e, bb_true);
      make_edge (bb_cond, bb_false, EDGE_FALSE_VALUE);
      make_edge (bb_true, bb_join, EDGE_FALLTHRU);
      make_edge (bb_false, bb_join, EDGE_FALLTHRU);

      /* Update dominance info.  Note that bb_join's data was
         updated by split_block.  */
      if (dom_info_available_p (CDI_DOMINATORS))
        {
          set_immediate_dominator (CDI_DOMINATORS, bb_true, bb_cond);
          set_immediate_dominator (CDI_DOMINATORS, bb_false, bb_cond);
        }

      rr = make_rename_temp (inner_type, NULL);
      ri = make_rename_temp (inner_type, NULL);
    }

  /* In the TRUE branch, we compute
      ratio = br/bi;
      div = (br * ratio) + bi;
      tr = (ar * ratio) + ai;
      ti = (ai * ratio) - ar;
      tr = tr / div;
      ti = ti / div;  */
  if (bb_true || integer_nonzerop (cond))
    {
      if (bb_true)
	{
	  *bsi = bsi_last (bb_true);
	  bsi_insert_after (bsi, build_empty_stmt (), BSI_NEW_STMT);
	}

      ratio = gimplify_build2 (bsi, code, inner_type, br, bi);

      t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, br, ratio);
      div = gimplify_build2 (bsi, PLUS_EXPR, inner_type, t1, bi);

      t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ar, ratio);
      tr = gimplify_build2 (bsi, PLUS_EXPR, inner_type, t1, ai);

      t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ai, ratio);
      ti = gimplify_build2 (bsi, MINUS_EXPR, inner_type, t1, ar);

      tr = gimplify_build2 (bsi, code, inner_type, tr, div);
      ti = gimplify_build2 (bsi, code, inner_type, ti, div);

     if (bb_true)
       {
	 t1 = build (MODIFY_EXPR, inner_type, rr, tr);
	 bsi_insert_before (bsi, t1, BSI_SAME_STMT);
	 t1 = build (MODIFY_EXPR, inner_type, ri, ti);
	 bsi_insert_before (bsi, t1, BSI_SAME_STMT);
	 bsi_remove (bsi);
       }
    }

  /* In the FALSE branch, we compute
      ratio = d/c;
      divisor = (d * ratio) + c;
      tr = (b * ratio) + a;
      ti = b - (a * ratio);
      tr = tr / div;
      ti = ti / div;  */
  if (bb_false || integer_zerop (cond))
    {
      if (bb_false)
	{
	  *bsi = bsi_last (bb_false);
	  bsi_insert_after (bsi, build_empty_stmt (), BSI_NEW_STMT);
	}

      ratio = gimplify_build2 (bsi, code, inner_type, bi, br);

      t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, bi, ratio);
      div = gimplify_build2 (bsi, PLUS_EXPR, inner_type, t1, br);

      t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ai, ratio);
      tr = gimplify_build2 (bsi, PLUS_EXPR, inner_type, t1, ar);

      t1 = gimplify_build2 (bsi, MULT_EXPR, inner_type, ar, ratio);
      ti = gimplify_build2 (bsi, MINUS_EXPR, inner_type, ai, t1);

      tr = gimplify_build2 (bsi, code, inner_type, tr, div);
      ti = gimplify_build2 (bsi, code, inner_type, ti, div);

     if (bb_false)
       {
	 t1 = build (MODIFY_EXPR, inner_type, rr, tr);
	 bsi_insert_before (bsi, t1, BSI_SAME_STMT);
	 t1 = build (MODIFY_EXPR, inner_type, ri, ti);
	 bsi_insert_before (bsi, t1, BSI_SAME_STMT);
	 bsi_remove (bsi);
       }
    }

  if (bb_join)
    *bsi = bsi_start (bb_join);
  else
    rr = tr, ri = ti;

  update_complex_assignment (bsi, rr, ri);
}

/* Expand complex division to scalars.  */

static void
expand_complex_division (block_stmt_iterator *bsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code)
{
  switch (flag_complex_method)
    {
    case 0:
      /* straightforward implementation of complex divide acceptable.  */
      expand_complex_div_straight (bsi, inner_type, ar, ai, br, bi, code);
      break;

    case 2:
      if (SCALAR_FLOAT_TYPE_P (inner_type))
	{
	  expand_complex_libcall (bsi, ar, ai, br, bi, code);
	  return;
	}
      /* FALLTHRU */

    case 1:
      /* wide ranges of inputs must work for complex divide.  */
      expand_complex_div_wide (bsi, inner_type, ar, ai, br, bi, code);
      break;

    default:
      gcc_unreachable ();
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

  rr = gimplify_build1 (bsi, NEGATE_EXPR, inner_type, ar);
  ri = gimplify_build1 (bsi, NEGATE_EXPR, inner_type, ai);

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

  ri = gimplify_build1 (bsi, NEGATE_EXPR, inner_type, ai);

  update_complex_assignment (bsi, ar, ri);
}

/* Expand complex comparison (EQ or NE only).  */

static void
expand_complex_comparison (block_stmt_iterator *bsi, tree ar, tree ai,
			   tree br, tree bi, enum tree_code code)
{
  tree cr, ci, cc, stmt, expr, type;

  cr = gimplify_build2 (bsi, code, boolean_type_node, ar, br);
  ci = gimplify_build2 (bsi, code, boolean_type_node, ai, bi);
  cc = gimplify_build2 (bsi,
			(code == EQ_EXPR ? TRUTH_AND_EXPR : TRUTH_OR_EXPR),
			boolean_type_node, cr, ci);

  stmt = expr = bsi_stmt (*bsi);

  switch (TREE_CODE (stmt))
    {
    case RETURN_EXPR:
      expr = TREE_OPERAND (stmt, 0);
      /* FALLTHRU */
    case MODIFY_EXPR:
      type = TREE_TYPE (TREE_OPERAND (expr, 1));
      TREE_OPERAND (expr, 1) = fold_convert (type, cc);
      break;
    case COND_EXPR:
      TREE_OPERAND (stmt, 0) = cc;
      break;
    default:
      gcc_unreachable ();
    }

  mark_stmt_modified (stmt);
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

  if (TREE_CODE_CLASS (code) == tcc_unary)
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
      gcc_unreachable ();
    }
  update_stmt_if_modified (stmt);
}

static void
tree_lower_complex (void)
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
  "cplxlower",				/* name */
  0,					/* gate */
  tree_lower_complex,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect
    | TODO_verify_stmts,		/* todo_flags_finish */
  0					/* letter */
};
