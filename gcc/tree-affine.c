/* Operations with affine combinations of trees.
   Copyright (C) 2005, 2007, 2008 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "output.h"
#include "diagnostic.h"
#include "tree-dump.h"
#include "pointer-set.h"
#include "tree-affine.h"
#include "gimple.h"
#include "flags.h"

/* Extends CST as appropriate for the affine combinations COMB.  */

double_int
double_int_ext_for_comb (double_int cst, aff_tree *comb)
{
  return double_int_sext (cst, TYPE_PRECISION (comb->type));
}

/* Initializes affine combination COMB so that its value is zero in TYPE.  */

static void
aff_combination_zero (aff_tree *comb, tree type)
{
  comb->type = type;
  comb->offset = double_int_zero;
  comb->n = 0;
  comb->rest = NULL_TREE;
}

/* Sets COMB to CST.  */

void
aff_combination_const (aff_tree *comb, tree type, double_int cst)
{
  aff_combination_zero (comb, type);
  comb->offset = double_int_ext_for_comb (cst, comb);
}

/* Sets COMB to single element ELT.  */

void
aff_combination_elt (aff_tree *comb, tree type, tree elt)
{
  aff_combination_zero (comb, type);

  comb->n = 1;
  comb->elts[0].val = elt;
  comb->elts[0].coef = double_int_one;
}

/* Scales COMB by SCALE.  */

void
aff_combination_scale (aff_tree *comb, double_int scale)
{
  unsigned i, j;

  scale = double_int_ext_for_comb (scale, comb);
  if (double_int_one_p (scale))
    return;

  if (double_int_zero_p (scale))
    {
      aff_combination_zero (comb, comb->type);
      return;
    }

  comb->offset
    = double_int_ext_for_comb (double_int_mul (scale, comb->offset), comb);
  for (i = 0, j = 0; i < comb->n; i++)
    {
      double_int new_coef;

      new_coef
	= double_int_ext_for_comb (double_int_mul (scale, comb->elts[i].coef),
				   comb);
      /* A coefficient may become zero due to overflow.  Remove the zero
	 elements.  */
      if (double_int_zero_p (new_coef))
	continue;
      comb->elts[j].coef = new_coef;
      comb->elts[j].val = comb->elts[i].val;
      j++;
    }
  comb->n = j;

  if (comb->rest)
    {
      tree type = comb->type;
      if (POINTER_TYPE_P (type))
	type = sizetype;
      if (comb->n < MAX_AFF_ELTS)
	{
	  comb->elts[comb->n].coef = scale;
	  comb->elts[comb->n].val = comb->rest;
	  comb->rest = NULL_TREE;
	  comb->n++;
	}
      else
	comb->rest = fold_build2 (MULT_EXPR, type, comb->rest,
				  double_int_to_tree (type, scale));
    }
}

/* Adds ELT * SCALE to COMB.  */

void
aff_combination_add_elt (aff_tree *comb, tree elt, double_int scale)
{
  unsigned i;
  tree type;

  scale = double_int_ext_for_comb (scale, comb);
  if (double_int_zero_p (scale))
    return;

  for (i = 0; i < comb->n; i++)
    if (operand_equal_p (comb->elts[i].val, elt, 0))
      {
	double_int new_coef;

	new_coef = double_int_add (comb->elts[i].coef, scale);
	new_coef = double_int_ext_for_comb (new_coef, comb);
	if (!double_int_zero_p (new_coef))
	  {
	    comb->elts[i].coef = new_coef;
	    return;
	  }

	comb->n--;
	comb->elts[i] = comb->elts[comb->n];

	if (comb->rest)
	  {
	    gcc_assert (comb->n == MAX_AFF_ELTS - 1);
	    comb->elts[comb->n].coef = double_int_one;
	    comb->elts[comb->n].val = comb->rest;
	    comb->rest = NULL_TREE;
	    comb->n++;
	  }
	return;
      }
  if (comb->n < MAX_AFF_ELTS)
    {
      comb->elts[comb->n].coef = scale;
      comb->elts[comb->n].val = elt;
      comb->n++;
      return;
    }

  type = comb->type;
  if (POINTER_TYPE_P (type))
    type = sizetype;

  if (double_int_one_p (scale))
    elt = fold_convert (type, elt);
  else
    elt = fold_build2 (MULT_EXPR, type,
		       fold_convert (type, elt),
		       double_int_to_tree (type, scale));

  if (comb->rest)
    comb->rest = fold_build2 (PLUS_EXPR, type, comb->rest,
			      elt);
  else
    comb->rest = elt;
}

/* Adds CST to C.  */

static void
aff_combination_add_cst (aff_tree *c, double_int cst)
{
  c->offset = double_int_ext_for_comb (double_int_add (c->offset, cst), c);
}

/* Adds COMB2 to COMB1.  */

void
aff_combination_add (aff_tree *comb1, aff_tree *comb2)
{
  unsigned i;

  aff_combination_add_cst (comb1, comb2->offset);
  for (i = 0; i < comb2->n; i++)
    aff_combination_add_elt (comb1, comb2->elts[i].val, comb2->elts[i].coef);
  if (comb2->rest)
    aff_combination_add_elt (comb1, comb2->rest, double_int_one);
}

/* Converts affine combination COMB to TYPE.  */

void
aff_combination_convert (aff_tree *comb, tree type)
{
  unsigned i, j;
  tree comb_type = comb->type;

  if  (TYPE_PRECISION (type) > TYPE_PRECISION (comb_type))
    {
      tree val = fold_convert (type, aff_combination_to_tree (comb));
      tree_to_aff_combination (val, type, comb);
      return;
    }

  comb->type = type;
  if (comb->rest && !POINTER_TYPE_P (type))
    comb->rest = fold_convert (type, comb->rest);

  if (TYPE_PRECISION (type) == TYPE_PRECISION (comb_type))
    return;

  comb->offset = double_int_ext_for_comb (comb->offset, comb);
  for (i = j = 0; i < comb->n; i++)
    {
      double_int new_coef = double_int_ext_for_comb (comb->elts[i].coef, comb);
      if (double_int_zero_p (new_coef))
	continue;
      comb->elts[j].coef = new_coef;
      comb->elts[j].val = fold_convert (type, comb->elts[i].val);
      j++;
    }

  comb->n = j;
  if (comb->n < MAX_AFF_ELTS && comb->rest)
    {
      comb->elts[comb->n].coef = double_int_one;
      comb->elts[comb->n].val = comb->rest;
      comb->rest = NULL_TREE;
      comb->n++;
    }
}

/* Splits EXPR into an affine combination of parts.  */

void
tree_to_aff_combination (tree expr, tree type, aff_tree *comb)
{
  aff_tree tmp;
  enum tree_code code;
  tree cst, core, toffset;
  HOST_WIDE_INT bitpos, bitsize;
  enum machine_mode mode;
  int unsignedp, volatilep;

  STRIP_NOPS (expr);

  code = TREE_CODE (expr);
  switch (code)
    {
    case INTEGER_CST:
      aff_combination_const (comb, type, tree_to_double_int (expr));
      return;

    case POINTER_PLUS_EXPR:
      tree_to_aff_combination (TREE_OPERAND (expr, 0), type, comb);
      tree_to_aff_combination (TREE_OPERAND (expr, 1), sizetype, &tmp);
      aff_combination_add (comb, &tmp);
      return;

    case PLUS_EXPR:
    case MINUS_EXPR:
      tree_to_aff_combination (TREE_OPERAND (expr, 0), type, comb);
      tree_to_aff_combination (TREE_OPERAND (expr, 1), type, &tmp);
      if (code == MINUS_EXPR)
	aff_combination_scale (&tmp, double_int_minus_one);
      aff_combination_add (comb, &tmp);
      return;

    case MULT_EXPR:
      cst = TREE_OPERAND (expr, 1);
      if (TREE_CODE (cst) != INTEGER_CST)
	break;
      tree_to_aff_combination (TREE_OPERAND (expr, 0), type, comb);
      aff_combination_scale (comb, tree_to_double_int (cst));
      return;

    case NEGATE_EXPR:
      tree_to_aff_combination (TREE_OPERAND (expr, 0), type, comb);
      aff_combination_scale (comb, double_int_minus_one);
      return;

    case BIT_NOT_EXPR:
      /* ~x = -x - 1 */
      tree_to_aff_combination (TREE_OPERAND (expr, 0), type, comb);
      aff_combination_scale (comb, double_int_minus_one);
      aff_combination_add_cst (comb, double_int_minus_one);
      return;

    case ADDR_EXPR:
      core = get_inner_reference (TREE_OPERAND (expr, 0), &bitsize, &bitpos,
				  &toffset, &mode, &unsignedp, &volatilep,
				  false);
      if (bitpos % BITS_PER_UNIT != 0)
	break;
      aff_combination_const (comb, type,
			     uhwi_to_double_int (bitpos / BITS_PER_UNIT));
      core = build_fold_addr_expr (core);
      if (TREE_CODE (core) == ADDR_EXPR)
	aff_combination_add_elt (comb, core, double_int_one);
      else
	{
	  tree_to_aff_combination (core, type, &tmp);
	  aff_combination_add (comb, &tmp);
	}
      if (toffset)
	{
	  tree_to_aff_combination (toffset, type, &tmp);
	  aff_combination_add (comb, &tmp);
	}
      return;

    default:
      break;
    }

  aff_combination_elt (comb, type, expr);
}

/* Creates EXPR + ELT * SCALE in TYPE.  EXPR is taken from affine
   combination COMB.  */

static tree
add_elt_to_tree (tree expr, tree type, tree elt, double_int scale,
		 aff_tree *comb)
{
  enum tree_code code;
  tree type1 = type;
  if (POINTER_TYPE_P (type))
    type1 = sizetype;

  scale = double_int_ext_for_comb (scale, comb);
  elt = fold_convert (type1, elt);

  if (double_int_one_p (scale))
    {
      if (!expr)
	return fold_convert (type, elt);

      if (POINTER_TYPE_P (type))
        return fold_build2 (POINTER_PLUS_EXPR, type, expr, elt);
      return fold_build2 (PLUS_EXPR, type, expr, elt);
    }

  if (double_int_minus_one_p (scale))
    {
      if (!expr)
	return fold_convert (type, fold_build1 (NEGATE_EXPR, type1, elt));

      if (POINTER_TYPE_P (type))
	{
	  elt = fold_build1 (NEGATE_EXPR, type1, elt);
	  return fold_build2 (POINTER_PLUS_EXPR, type, expr, elt);
	}
      return fold_build2 (MINUS_EXPR, type, expr, elt);
    }

  if (!expr)
    return fold_convert (type,
			 fold_build2 (MULT_EXPR, type1, elt,
				      double_int_to_tree (type1, scale)));

  if (double_int_negative_p (scale))
    {
      code = MINUS_EXPR;
      scale = double_int_neg (scale);
    }
  else
    code = PLUS_EXPR;

  elt = fold_build2 (MULT_EXPR, type1, elt,
		     double_int_to_tree (type1, scale));
  if (POINTER_TYPE_P (type))
    {
      if (code == MINUS_EXPR)
        elt = fold_build1 (NEGATE_EXPR, type1, elt);
      return fold_build2 (POINTER_PLUS_EXPR, type, expr, elt);
    }
  return fold_build2 (code, type, expr, elt);
}

/* Makes tree from the affine combination COMB.  */

tree
aff_combination_to_tree (aff_tree *comb)
{
  tree type = comb->type;
  tree expr = comb->rest;
  unsigned i;
  double_int off, sgn;
  tree type1 = type;
  if (POINTER_TYPE_P (type))
    type1 = sizetype;

  gcc_assert (comb->n == MAX_AFF_ELTS || comb->rest == NULL_TREE);

  for (i = 0; i < comb->n; i++)
    expr = add_elt_to_tree (expr, type, comb->elts[i].val, comb->elts[i].coef,
			    comb);

  /* Ensure that we get x - 1, not x + (-1) or x + 0xff..f if x is
     unsigned.  */
  if (double_int_negative_p (comb->offset))
    {
      off = double_int_neg (comb->offset);
      sgn = double_int_minus_one;
    }
  else
    {
      off = comb->offset;
      sgn = double_int_one;
    }
  return add_elt_to_tree (expr, type, double_int_to_tree (type1, off), sgn,
			  comb);
}

/* Copies the tree elements of COMB to ensure that they are not shared.  */

void
unshare_aff_combination (aff_tree *comb)
{
  unsigned i;

  for (i = 0; i < comb->n; i++)
    comb->elts[i].val = unshare_expr (comb->elts[i].val);
  if (comb->rest)
    comb->rest = unshare_expr (comb->rest);
}

/* Remove M-th element from COMB.  */

void
aff_combination_remove_elt (aff_tree *comb, unsigned m)
{
  comb->n--;
  if (m <= comb->n)
    comb->elts[m] = comb->elts[comb->n];
  if (comb->rest)
    {
      comb->elts[comb->n].coef = double_int_one;
      comb->elts[comb->n].val = comb->rest;
      comb->rest = NULL_TREE;
      comb->n++;
    }
}

/* Adds C * COEF * VAL to R.  VAL may be NULL, in that case only
   C * COEF is added to R.  */


static void
aff_combination_add_product (aff_tree *c, double_int coef, tree val,
			     aff_tree *r)
{
  unsigned i;
  tree aval, type;

  for (i = 0; i < c->n; i++)
    {
      aval = c->elts[i].val;
      if (val)
	{
	  type = TREE_TYPE (aval);
	  aval = fold_build2 (MULT_EXPR, type, aval,
			      fold_convert (type, val));
	}

      aff_combination_add_elt (r, aval,
			       double_int_mul (coef, c->elts[i].coef));
    }

  if (c->rest)
    {
      aval = c->rest;
      if (val)
	{
	  type = TREE_TYPE (aval);
	  aval = fold_build2 (MULT_EXPR, type, aval,
			      fold_convert (type, val));
	}

      aff_combination_add_elt (r, aval, coef);
    }

  if (val)
    aff_combination_add_elt (r, val,
			     double_int_mul (coef, c->offset));
  else
    aff_combination_add_cst (r, double_int_mul (coef, c->offset));
}

/* Multiplies C1 by C2, storing the result to R  */

void
aff_combination_mult (aff_tree *c1, aff_tree *c2, aff_tree *r)
{
  unsigned i;
  gcc_assert (TYPE_PRECISION (c1->type) == TYPE_PRECISION (c2->type));

  aff_combination_zero (r, c1->type);

  for (i = 0; i < c2->n; i++)
    aff_combination_add_product (c1, c2->elts[i].coef, c2->elts[i].val, r);
  if (c2->rest)
    aff_combination_add_product (c1, double_int_one, c2->rest, r);
  aff_combination_add_product (c1, c2->offset, NULL, r);
}

/* Returns the element of COMB whose value is VAL, or NULL if no such
   element exists.  If IDX is not NULL, it is set to the index of VAL in
   COMB.  */

static struct aff_comb_elt *
aff_combination_find_elt (aff_tree *comb, tree val, unsigned *idx)
{
  unsigned i;

  for (i = 0; i < comb->n; i++)
    if (operand_equal_p (comb->elts[i].val, val, 0))
      {
	if (idx)
	  *idx = i;

	return &comb->elts[i];
      }

  return NULL;
}

/* Element of the cache that maps ssa name NAME to its expanded form
   as an affine expression EXPANSION.  */

struct name_expansion
{
  aff_tree expansion;

  /* True if the expansion for the name is just being generated.  */
  unsigned in_progress : 1;
};

/* Expands SSA names in COMB recursively.  CACHE is used to cache the
   results.  */

void
aff_combination_expand (aff_tree *comb ATTRIBUTE_UNUSED,
			struct pointer_map_t **cache ATTRIBUTE_UNUSED)
{
  unsigned i;
  aff_tree to_add, current, curre;
  tree e, rhs;
  gimple def;
  double_int scale;
  void **slot;
  struct name_expansion *exp;

  aff_combination_zero (&to_add, comb->type);
  for (i = 0; i < comb->n; i++)
    {
      tree type, name;
      enum tree_code code;

      e = comb->elts[i].val;
      type = TREE_TYPE (e);
      name = e;
      /* Look through some conversions.  */
      if (TREE_CODE (e) == NOP_EXPR
          && (TYPE_PRECISION (type)
	      >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (e, 0)))))
	name = TREE_OPERAND (e, 0);
      if (TREE_CODE (name) != SSA_NAME)
	continue;
      def = SSA_NAME_DEF_STMT (name);
      if (!is_gimple_assign (def) || gimple_assign_lhs (def) != name)
	continue;

      code = gimple_assign_rhs_code (def);
      if (code != SSA_NAME
	  && !IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code))
	  && (get_gimple_rhs_class (code) != GIMPLE_SINGLE_RHS
	      || !is_gimple_min_invariant (gimple_assign_rhs1 (def))))
	continue;

      /* We do not know whether the reference retains its value at the
	 place where the expansion is used.  */
      if (TREE_CODE_CLASS (code) == tcc_reference)
	continue;

      if (!*cache)
	*cache = pointer_map_create ();
      slot = pointer_map_insert (*cache, e);
      exp = (struct name_expansion *) *slot;

      if (!exp)
	{
	  exp = XNEW (struct name_expansion);
	  exp->in_progress = 1;
	  *slot = exp;
	  /* In principle this is a generally valid folding, but
	     it is not unconditionally an optimization, so do it
	     here and not in fold_unary.  */
	  /* Convert (T1)(X *+- CST) into (T1)X *+- (T1)CST if T1 is wider
	     than the type of X and overflow for the type of X is
	     undefined.  */
	  if (e != name
	      && INTEGRAL_TYPE_P (type)
	      && INTEGRAL_TYPE_P (TREE_TYPE (name))
	      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (name))
	      && TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (name))
	      && (code == PLUS_EXPR || code == MINUS_EXPR || code == MULT_EXPR)
	      && TREE_CODE (gimple_assign_rhs2 (def)) == INTEGER_CST)
	    rhs = fold_build2 (code, type,
			       fold_convert (type, gimple_assign_rhs1 (def)),
			       fold_convert (type, gimple_assign_rhs2 (def)));
	  else
	    {
	      rhs = gimple_assign_rhs_to_tree (def);
	      if (e != name)
		rhs = fold_convert (type, rhs);
	    }
	  tree_to_aff_combination_expand (rhs, comb->type, &current, cache);
	  exp->expansion = current;
	  exp->in_progress = 0;
	}
      else
	{
	  /* Since we follow the definitions in the SSA form, we should not
	     enter a cycle unless we pass through a phi node.  */
	  gcc_assert (!exp->in_progress);
	  current = exp->expansion;
	}

      /* Accumulate the new terms to TO_ADD, so that we do not modify
	 COMB while traversing it; include the term -coef * E, to remove
         it from COMB.  */
      scale = comb->elts[i].coef;
      aff_combination_zero (&curre, comb->type);
      aff_combination_add_elt (&curre, e, double_int_neg (scale));
      aff_combination_scale (&current, scale);
      aff_combination_add (&to_add, &current);
      aff_combination_add (&to_add, &curre);
    }
  aff_combination_add (comb, &to_add);
}

/* Similar to tree_to_aff_combination, but follows SSA name definitions
   and expands them recursively.  CACHE is used to cache the expansions
   of the ssa names, to avoid exponential time complexity for cases
   like

   a1 = a0 + a0;
   a2 = a1 + a1;
   a3 = a2 + a2;
   ...  */

void
tree_to_aff_combination_expand (tree expr, tree type, aff_tree *comb,
				struct pointer_map_t **cache)
{
  tree_to_aff_combination (expr, type, comb);
  aff_combination_expand (comb, cache);
}

/* Frees memory occupied by struct name_expansion in *VALUE.  Callback for
   pointer_map_traverse.  */

static bool
free_name_expansion (const void *key ATTRIBUTE_UNUSED, void **value,
		     void *data ATTRIBUTE_UNUSED)
{
  struct name_expansion *const exp = (struct name_expansion *) *value;

  free (exp);
  return true;
}

/* Frees memory allocated for the CACHE used by
   tree_to_aff_combination_expand.  */

void
free_affine_expand_cache (struct pointer_map_t **cache)
{
  if (!*cache)
    return;

  pointer_map_traverse (*cache, free_name_expansion, NULL);
  pointer_map_destroy (*cache);
  *cache = NULL;
}

/* If VAL != CST * DIV for any constant CST, returns false.
   Otherwise, if VAL != 0 (and hence CST != 0), and *MULT_SET is true,
   additionally compares CST and MULT, and if they are different,
   returns false.  Finally, if neither of these two cases occur,
   true is returned, and if CST != 0, CST is stored to MULT and
   MULT_SET is set to true.  */

static bool
double_int_constant_multiple_p (double_int val, double_int div,
				bool *mult_set, double_int *mult)
{
  double_int rem, cst;

  if (double_int_zero_p (val))
    return true;

  if (double_int_zero_p (div))
    return false;

  cst = double_int_sdivmod (val, div, FLOOR_DIV_EXPR, &rem);
  if (!double_int_zero_p (rem))
    return false;

  if (*mult_set && !double_int_equal_p (*mult, cst))
    return false;

  *mult_set = true;
  *mult = cst;
  return true;
}

/* Returns true if VAL = X * DIV for some constant X.  If this is the case,
   X is stored to MULT.  */

bool
aff_combination_constant_multiple_p (aff_tree *val, aff_tree *div,
				     double_int *mult)
{
  bool mult_set = false;
  unsigned i;

  if (val->n == 0 && double_int_zero_p (val->offset))
    {
      *mult = double_int_zero;
      return true;
    }
  if (val->n != div->n)
    return false;

  if (val->rest || div->rest)
    return false;

  if (!double_int_constant_multiple_p (val->offset, div->offset,
				       &mult_set, mult))
    return false;

  for (i = 0; i < div->n; i++)
    {
      struct aff_comb_elt *elt
	      = aff_combination_find_elt (val, div->elts[i].val, NULL);
      if (!elt)
	return false;
      if (!double_int_constant_multiple_p (elt->coef, div->elts[i].coef,
					   &mult_set, mult))
	return false;
    }

  gcc_assert (mult_set);
  return true;
}

/* Prints the affine VAL to the FILE. */

void
print_aff (FILE *file, aff_tree *val)
{
  unsigned i;
  bool uns = TYPE_UNSIGNED (val->type);
  if (POINTER_TYPE_P (val->type))
    uns = false;
  fprintf (file, "{\n  type = ");
  print_generic_expr (file, val->type, TDF_VOPS|TDF_MEMSYMS);
  fprintf (file, "\n  offset = ");
  dump_double_int (file, val->offset, uns);
  if (val->n > 0)
    {
      fprintf (file, "\n  elements = {\n");
      for (i = 0; i < val->n; i++)
	{
	  fprintf (file, "    [%d] = ", i);
	  print_generic_expr (file, val->elts[i].val, TDF_VOPS|TDF_MEMSYMS);

	  fprintf (file, " * ");
	  dump_double_int (file, val->elts[i].coef, uns);
	  if (i != val->n - 1)
	    fprintf (file, ", \n");
	}
      fprintf (file, "\n  }");
  }
  if (val->rest)
    {
      fprintf (file, "\n  rest = ");
      print_generic_expr (file, val->rest, TDF_VOPS|TDF_MEMSYMS);
    }
  fprintf (file, "\n}");
}

/* Prints the affine VAL to the standard error, used for debugging.  */

void
debug_aff (aff_tree *val)
{
  print_aff (stderr, val);
  fprintf (stderr, "\n");
}

/* Returns address of the reference REF in ADDR.  The size of the accessed
   location is stored to SIZE.  */

void
get_inner_reference_aff (tree ref, aff_tree *addr, double_int *size)
{
  HOST_WIDE_INT bitsize, bitpos;
  tree toff;
  enum machine_mode mode;
  int uns, vol;
  aff_tree tmp;
  tree base = get_inner_reference (ref, &bitsize, &bitpos, &toff, &mode,
				   &uns, &vol, false);
  tree base_addr = build_fold_addr_expr (base);

  /* ADDR = &BASE + TOFF + BITPOS / BITS_PER_UNIT.  */

  tree_to_aff_combination (base_addr, sizetype, addr);

  if (toff)
    {
      tree_to_aff_combination (toff, sizetype, &tmp);
      aff_combination_add (addr, &tmp);
    }

  aff_combination_const (&tmp, sizetype,
			 shwi_to_double_int (bitpos / BITS_PER_UNIT));
  aff_combination_add (addr, &tmp);

  *size = shwi_to_double_int ((bitsize + BITS_PER_UNIT - 1) / BITS_PER_UNIT);
}

