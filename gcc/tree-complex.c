/* Lower complex number and vector operations to scalar operations.
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
    case RESULT_DECL:
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
  modify_stmt (stmt);
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
  modify_stmt (stmt);
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

  modify_stmt (stmt);
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
}

/* Build a constant of type TYPE, made of VALUE's bits replicated
   every TYPE_SIZE (INNER_TYPE) bits to fit TYPE's precision.  */
static tree
build_replicated_const (tree type, tree inner_type, HOST_WIDE_INT value)
{
  int width = tree_low_cst (TYPE_SIZE (inner_type), 1);
  int n = HOST_BITS_PER_WIDE_INT / width;
  unsigned HOST_WIDE_INT low, high, mask;
  tree ret;

  gcc_assert (n);

  if (width == HOST_BITS_PER_WIDE_INT)
    low = value;
  else
    {
      mask = ((HOST_WIDE_INT)1 << width) - 1;
      low = (unsigned HOST_WIDE_INT) ~0 / mask * (value & mask);
    }

  if (TYPE_PRECISION (type) < HOST_BITS_PER_WIDE_INT)
    low &= ((HOST_WIDE_INT)1 << TYPE_PRECISION (type)) - 1, high = 0;
  else if (TYPE_PRECISION (type) == HOST_BITS_PER_WIDE_INT)
    high = 0;
  else if (TYPE_PRECISION (type) == 2 * HOST_BITS_PER_WIDE_INT)
    high = low;
  else
    gcc_unreachable ();

  ret = build_int_cst_wide (type, low, high);
  return ret;
}

static GTY(()) tree vector_inner_type;
static GTY(()) tree vector_last_type;
static GTY(()) int vector_last_nunits;

/* Return a suitable vector types made of SUBPARTS units each of mode
   "word_mode" (the global variable).  */
static tree
build_word_mode_vector_type (int nunits)
{
  if (!vector_inner_type)
    vector_inner_type = lang_hooks.types.type_for_mode (word_mode, 1);
  else if (vector_last_nunits == nunits)
    {
      gcc_assert (TREE_CODE (vector_last_type) == VECTOR_TYPE);
      return vector_last_type;
    }

  /* We build a new type, but we canonicalize it nevertheless,
     because it still saves some memory.  */
  vector_last_nunits = nunits;
  vector_last_type = type_hash_canon (nunits,
				      build_vector_type (vector_inner_type,
							 nunits));
  return vector_last_type;
}

typedef tree (*elem_op_func) (block_stmt_iterator *,
			      tree, tree, tree, tree, tree, enum tree_code);

static inline tree
tree_vec_extract (block_stmt_iterator *bsi, tree type,
		  tree t, tree bitsize, tree bitpos)
{
  if (bitpos)
    return gimplify_build3 (bsi, BIT_FIELD_REF, type, t, bitsize, bitpos);
  else
    return gimplify_build1 (bsi, VIEW_CONVERT_EXPR, type, t);
}

static tree
do_unop (block_stmt_iterator *bsi, tree inner_type, tree a,
	 tree b ATTRIBUTE_UNUSED, tree bitpos, tree bitsize,
	 enum tree_code code)
{
  a = tree_vec_extract (bsi, inner_type, a, bitsize, bitpos);
  return gimplify_build1 (bsi, code, inner_type, a);
}

static tree
do_binop (block_stmt_iterator *bsi, tree inner_type, tree a, tree b,
	  tree bitpos, tree bitsize, enum tree_code code)
{
  a = tree_vec_extract (bsi, inner_type, a, bitsize, bitpos);
  b = tree_vec_extract (bsi, inner_type, b, bitsize, bitpos);
  return gimplify_build2 (bsi, code, inner_type, a, b);
}

/* Expand vector addition to scalars.  This does bit twiddling
   in order to increase parallelism:

   a + b = (((int) a & 0x7f7f7f7f) + ((int) b & 0x7f7f7f7f)) ^
           (a ^ b) & 0x80808080

   a - b =  (((int) a | 0x80808080) - ((int) b & 0x7f7f7f7f)) ^
            (a ^ ~b) & 0x80808080

   -b = (0x80808080 - ((int) b & 0x7f7f7f7f)) ^ (~b & 0x80808080)

   This optimization should be done only if 4 vector items or more
   fit into a word.  */
static tree
do_plus_minus (block_stmt_iterator *bsi, tree word_type, tree a, tree b,
	       tree bitpos ATTRIBUTE_UNUSED, tree bitsize ATTRIBUTE_UNUSED,
	       enum tree_code code)
{
  tree inner_type = TREE_TYPE (TREE_TYPE (a));
  unsigned HOST_WIDE_INT max;
  tree low_bits, high_bits, a_low, b_low, result_low, signs;

  max = GET_MODE_MASK (TYPE_MODE (inner_type));
  low_bits = build_replicated_const (word_type, inner_type, max >> 1);
  high_bits = build_replicated_const (word_type, inner_type, max & ~(max >> 1));

  a = tree_vec_extract (bsi, word_type, a, bitsize, bitpos);
  b = tree_vec_extract (bsi, word_type, b, bitsize, bitpos);

  signs = gimplify_build2 (bsi, BIT_XOR_EXPR, word_type, a, b);
  b_low = gimplify_build2 (bsi, BIT_AND_EXPR, word_type, b, low_bits);
  if (code == PLUS_EXPR)
    a_low = gimplify_build2 (bsi, BIT_AND_EXPR, word_type, a, low_bits);
  else
    {
      a_low = gimplify_build2 (bsi, BIT_IOR_EXPR, word_type, a, high_bits);
      signs = gimplify_build1 (bsi, BIT_NOT_EXPR, word_type, signs);
    }

  signs = gimplify_build2 (bsi, BIT_AND_EXPR, word_type, signs, high_bits);
  result_low = gimplify_build2 (bsi, code, word_type, a_low, b_low);
  return gimplify_build2 (bsi, BIT_XOR_EXPR, word_type, result_low, signs);
}

static tree
do_negate (block_stmt_iterator *bsi, tree word_type, tree b,
	   tree unused ATTRIBUTE_UNUSED, tree bitpos ATTRIBUTE_UNUSED,
	   tree bitsize ATTRIBUTE_UNUSED,
	   enum tree_code code ATTRIBUTE_UNUSED)
{
  tree inner_type = TREE_TYPE (TREE_TYPE (b));
  HOST_WIDE_INT max;
  tree low_bits, high_bits, b_low, result_low, signs;

  max = GET_MODE_MASK (TYPE_MODE (inner_type));
  low_bits = build_replicated_const (word_type, inner_type, max >> 1);
  high_bits = build_replicated_const (word_type, inner_type, max & ~(max >> 1));

  b = tree_vec_extract (bsi, word_type, b, bitsize, bitpos);

  b_low = gimplify_build2 (bsi, BIT_AND_EXPR, word_type, b, low_bits);
  signs = gimplify_build1 (bsi, BIT_NOT_EXPR, word_type, b);
  signs = gimplify_build2 (bsi, BIT_AND_EXPR, word_type, signs, high_bits);
  result_low = gimplify_build2 (bsi, MINUS_EXPR, word_type, high_bits, b_low);
  return gimplify_build2 (bsi, BIT_XOR_EXPR, word_type, result_low, signs);
}

/* Expand a vector operation to scalars, by using many operations
   whose type is the vector type's inner type.  */
static tree
expand_vector_piecewise (block_stmt_iterator *bsi, elem_op_func f,
			 tree type, tree inner_type,
			 tree a, tree b, enum tree_code code)
{
  tree head, *chain = &head;
  tree part_width = TYPE_SIZE (inner_type);
  tree index = bitsize_int (0);
  int nunits = TYPE_VECTOR_SUBPARTS (type);
  int delta = tree_low_cst (part_width, 1)
	      / tree_low_cst (TYPE_SIZE (TREE_TYPE (type)), 1);
  int i;

  for (i = 0; i < nunits;
       i += delta, index = int_const_binop (PLUS_EXPR, index, part_width, 0))
    {
      tree result = f (bsi, inner_type, a, b, index, part_width, code);
      *chain = tree_cons (NULL_TREE, result, NULL_TREE);
      chain = &TREE_CHAIN (*chain);
    }

  return build1 (CONSTRUCTOR, type, head);
}

/* Expand a vector operation to scalars with the freedom to use
   a scalar integer type, or to use a different size for the items
   in the vector type.  */
static tree
expand_vector_parallel (block_stmt_iterator *bsi, elem_op_func f, tree type,
			tree a, tree b,
			enum tree_code code)
{
  tree result, compute_type;
  enum machine_mode mode;
  int n_words = tree_low_cst (TYPE_SIZE_UNIT (type), 1) / UNITS_PER_WORD;

  /* We have three strategies.  If the type is already correct, just do
     the operation an element at a time.  Else, if the vector is wider than
     one word, do it a word at a time; finally, if the vector is smaller
     than one word, do it as a scalar.  */
  if (TYPE_MODE (TREE_TYPE (type)) == word_mode)
     return expand_vector_piecewise (bsi, f,
				     type, TREE_TYPE (type),
				     a, b, code);
  else if (n_words > 1)
    {
      tree word_type = build_word_mode_vector_type (n_words);
      result = expand_vector_piecewise (bsi, f,
				        word_type, TREE_TYPE (word_type),
					a, b, code);
      result = gimplify_val (bsi, word_type, result);
    }
  else
    {
      /* Use a single scalar operation with a mode no wider than word_mode.  */
      mode = mode_for_size (tree_low_cst (TYPE_SIZE (type), 1), MODE_INT, 0);
      compute_type = lang_hooks.types.type_for_mode (mode, 1);
      result = f (bsi, compute_type, a, b, NULL_TREE, NULL_TREE, code);
    }

  return build1 (VIEW_CONVERT_EXPR, type, result);
}

/* Expand a vector operation to scalars; for integer types we can use
   special bit twiddling tricks to do the sums a word at a time, using
   function F_PARALLEL instead of F.  These tricks are done only if
   they can process at least four items, that is, only if the vector
   holds at least four items and if a word can hold four items.  */
static tree
expand_vector_addition (block_stmt_iterator *bsi,
			elem_op_func f, elem_op_func f_parallel,
			tree type, tree a, tree b, enum tree_code code)
{
  int parts_per_word = UNITS_PER_WORD
	  	       / tree_low_cst (TYPE_SIZE_UNIT (TREE_TYPE (type)), 1);

  if (INTEGRAL_TYPE_P (TREE_TYPE (type))
      && parts_per_word >= 4
      && TYPE_VECTOR_SUBPARTS (type) >= 4)
    return expand_vector_parallel (bsi, f_parallel,
				   type, a, b, code);
  else
    return expand_vector_piecewise (bsi, f,
				    type, TREE_TYPE (type),
				    a, b, code);
}

/* Return a type for the widest vector mode whose components are of mode
   INNER_MODE, or NULL_TREE if none is found.  */
static tree
type_for_widest_vector_mode (enum machine_mode inner_mode, optab op)
{
  enum machine_mode best_mode = VOIDmode, mode;
  int best_nunits = 0;

  if (GET_MODE_CLASS (inner_mode) == MODE_FLOAT)
    mode = MIN_MODE_VECTOR_FLOAT;
  else
    mode = MIN_MODE_VECTOR_INT;

  for (; mode != VOIDmode; mode = GET_MODE_WIDER_MODE (mode))
    if (GET_MODE_INNER (mode) == inner_mode
        && GET_MODE_NUNITS (mode) > best_nunits
	&& op->handlers[mode].insn_code != CODE_FOR_nothing)
      best_mode = mode, best_nunits = GET_MODE_NUNITS (mode);

  if (best_mode == VOIDmode)
    return NULL_TREE;
  else
    return lang_hooks.types.type_for_mode (best_mode, 1);
}

/* Process one statement.  If we identify a vector operation, expand it.  */

static void
expand_vector_operations_1 (block_stmt_iterator *bsi)
{
  tree stmt = bsi_stmt (*bsi);
  tree *p_rhs, rhs, type, compute_type;
  enum tree_code code;
  enum machine_mode compute_mode;
  optab op;

  switch (TREE_CODE (stmt))
    {
    case RETURN_EXPR:
      stmt = TREE_OPERAND (stmt, 0);
      if (!stmt || TREE_CODE (stmt) != MODIFY_EXPR)
	return;

      /* FALLTHRU */

    case MODIFY_EXPR:
      p_rhs = &TREE_OPERAND (stmt, 1);
      rhs = *p_rhs;
      break;

    default:
      return;
    }

  type = TREE_TYPE (rhs);
  if (TREE_CODE (type) != VECTOR_TYPE)
    return;

  code = TREE_CODE (rhs);
  if (TREE_CODE_CLASS (code) != tcc_unary
      && TREE_CODE_CLASS (code) != tcc_binary)
    return;

  if (code == NOP_EXPR || code == VIEW_CONVERT_EXPR)
    return;
  
  gcc_assert (code != CONVERT_EXPR);
  op = optab_for_tree_code (code, type);

  /* Optabs will try converting a negation into a subtraction, so
     look for it as well.  TODO: negation of floating-point vectors
     might be turned into an exclusive OR toggling the sign bit.  */
  if (op == NULL
      && code == NEGATE_EXPR
      && INTEGRAL_TYPE_P (TREE_TYPE (type)))
    op = optab_for_tree_code (MINUS_EXPR, type);

  /* For very wide vectors, try using a smaller vector mode.  */
  compute_type = type;
  if (TYPE_MODE (type) == BLKmode && op)
    {
      tree vector_compute_type
        = type_for_widest_vector_mode (TYPE_MODE (TREE_TYPE (type)), op);
      if (vector_compute_type != NULL_TREE)
        compute_type = vector_compute_type;
    }

  compute_mode = TYPE_MODE (compute_type);

  /* If we are breaking a BLKmode vector into smaller pieces,
     type_for_widest_vector_mode has already looked into the optab,
     so skip these checks.  */
  if (compute_type == type)
    {
      if ((GET_MODE_CLASS (compute_mode) == MODE_VECTOR_INT
	   || GET_MODE_CLASS (compute_mode) == MODE_VECTOR_FLOAT)
          && op != NULL
	  && op->handlers[compute_mode].insn_code != CODE_FOR_nothing)
	return;
      else
	{
	  /* There is no operation in hardware, so fall back to scalars.  */
	  compute_type = TREE_TYPE (type);
	  compute_mode = TYPE_MODE (compute_type);
	}
    }

  /* If the compute mode is not a vector mode (hence we are decomposing
     a BLKmode vector to smaller, hardware-supported vectors), we may
     want to expand the operations in parallel.  */
  if (GET_MODE_CLASS (compute_mode) != MODE_VECTOR_INT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_FLOAT)
    switch (code)
      {
      case PLUS_EXPR:
      case MINUS_EXPR:
        if (TYPE_TRAP_SIGNED (type))
	  break;

        *p_rhs = expand_vector_addition (bsi, do_binop, do_plus_minus, type,
		      		         TREE_OPERAND (rhs, 0),
				         TREE_OPERAND (rhs, 1), code);
	modify_stmt (bsi_stmt (*bsi));
        return;

      case NEGATE_EXPR:
        if (TYPE_TRAP_SIGNED (type))
	  break;

        *p_rhs = expand_vector_addition (bsi, do_unop, do_negate, type,
		      		         TREE_OPERAND (rhs, 0),
					 NULL_TREE, code);
	modify_stmt (bsi_stmt (*bsi));
        return;

      case BIT_AND_EXPR:
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
        *p_rhs = expand_vector_parallel (bsi, do_binop, type,
		      		         TREE_OPERAND (rhs, 0),
				         TREE_OPERAND (rhs, 1), code);
	modify_stmt (bsi_stmt (*bsi));
        return;

      case BIT_NOT_EXPR:
        *p_rhs = expand_vector_parallel (bsi, do_unop, type,
		      		         TREE_OPERAND (rhs, 0),
					 NULL_TREE, code);
	modify_stmt (bsi_stmt (*bsi));
        return;

      default:
	break;
      }

  if (TREE_CODE_CLASS (code) == tcc_unary)
    *p_rhs = expand_vector_piecewise (bsi, do_unop, type, compute_type,
				      TREE_OPERAND (rhs, 0),
				      NULL_TREE, code);
  else
    *p_rhs = expand_vector_piecewise (bsi, do_binop, type, compute_type,
				      TREE_OPERAND (rhs, 0),
				      TREE_OPERAND (rhs, 1), code);

  modify_stmt (bsi_stmt (*bsi));
}

static void
expand_vector_operations (void)
{
  block_stmt_iterator bsi;
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	expand_vector_operations_1 (&bsi);
    }
}

static void
tree_lower_operations (void)
{
  int old_last_basic_block = last_basic_block;
  block_stmt_iterator bsi;
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      if (bb->index >= old_last_basic_block)
	continue;
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  expand_complex_operations_1 (&bsi);
	  expand_vector_operations_1 (&bsi);
	}
    }
}


struct tree_opt_pass pass_lower_vector_ssa = 
{
  "vector",				/* name */
  NULL,					/* gate */
  expand_vector_operations,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars	/* todo_flags_finish */
    | TODO_ggc_collect | TODO_verify_ssa
    | TODO_verify_stmts | TODO_verify_flow,
  0					/* letter */
};

struct tree_opt_pass pass_pre_expand = 
{
  "oplower",				/* name */
  0,					/* gate */
  tree_lower_operations,		/* execute */
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

#include "gt-tree-complex.h"
