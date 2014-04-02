/* Translation of CLAST (CLooG AST) to Gimple.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com>.

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

#ifdef HAVE_cloog
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/list.h>
#include <isl/constraint.h>
#include <isl/ilp.h>
#include <isl/aff.h>
#include <cloog/cloog.h>
#include <cloog/isl/domain.h>
#endif

#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-ssa.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "cloog/cloog.h"
#include "graphite-poly.h"
#include "graphite-clast-to-gimple.h"
#include "graphite-htab.h"

typedef const struct clast_expr *clast_name_p;

#ifndef CLOOG_LANGUAGE_C
#define CLOOG_LANGUAGE_C LANGUAGE_C
#endif


/* Converts a GMP constant VAL to a tree and returns it.  */

static tree
gmp_cst_to_tree (tree type, mpz_t val)
{
  tree t = type ? type : integer_type_node;
  mpz_t tmp;

  mpz_init (tmp);
  mpz_set (tmp, val);
  wide_int wi = wi::from_mpz (t, tmp, true);
  mpz_clear (tmp);

  return wide_int_to_tree (t, wi);
}

/* Sets RES to the min of V1 and V2.  */

static void
value_min (mpz_t res, mpz_t v1, mpz_t v2)
{
  if (mpz_cmp (v1, v2) < 0)
    mpz_set (res, v1);
  else
    mpz_set (res, v2);
}

/* Sets RES to the max of V1 and V2.  */

static void
value_max (mpz_t res, mpz_t v1, mpz_t v2)
{
  if (mpz_cmp (v1, v2) < 0)
    mpz_set (res, v2);
  else
    mpz_set (res, v1);
}


/* This flag is set when an error occurred during the translation of
   CLAST to Gimple.  */
static bool gloog_error;

/* Verifies properties that GRAPHITE should maintain during translation.  */

static inline void
graphite_verify (void)
{
#ifdef ENABLE_CHECKING
  verify_loop_structure ();
  verify_loop_closed_ssa (true);
#endif
}

/* Stores the INDEX in a vector and the loop nesting LEVEL for a given
   clast NAME.  BOUND_ONE and BOUND_TWO represent the exact lower and
   upper bounds that can be inferred from the polyhedral representation.  */

typedef struct clast_name_index {
  int index;
  int level;
  mpz_t bound_one, bound_two;
  const char *name;
  /* If free_name is set, the content of name was allocated by us and needs
     to be freed.  */
  char *free_name;
} *clast_name_index_p;

/* Helper for hashing clast_name_index.  */

struct clast_index_hasher
{
  typedef clast_name_index value_type;
  typedef clast_name_index compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* Computes a hash function for database element E.  */

inline hashval_t
clast_index_hasher::hash (const value_type *e)
{
  hashval_t hash = 0;

  int length = strlen (e->name);
  int i;

  for (i = 0; i < length; ++i)
    hash = hash | (e->name[i] << (i % 4));

  return hash;
}

/* Compares database elements ELT1 and ELT2.  */

inline bool
clast_index_hasher::equal (const value_type *elt1, const compare_type *elt2)
{
  return strcmp (elt1->name, elt2->name) == 0;
}

/* Free the memory taken by a clast_name_index struct.  */

inline void
clast_index_hasher::remove (value_type *c)
{
  if (c->free_name)
    free (c->free_name);
  mpz_clear (c->bound_one);
  mpz_clear (c->bound_two);
  free (c);
}

typedef hash_table <clast_index_hasher> clast_index_htab_type;

/* Returns a pointer to a new element of type clast_name_index_p built
   from NAME, INDEX, LEVEL, BOUND_ONE, and BOUND_TWO.  */

static inline clast_name_index_p
new_clast_name_index (const char *name, int index, int level,
		      mpz_t bound_one, mpz_t bound_two)
{
  clast_name_index_p res = XNEW (struct clast_name_index);
  char *new_name = XNEWVEC (char, strlen (name) + 1);
  strcpy (new_name, name);

  res->name = new_name;
  res->free_name = new_name;
  res->level = level;
  res->index = index;
  mpz_init (res->bound_one);
  mpz_init (res->bound_two);
  mpz_set (res->bound_one, bound_one);
  mpz_set (res->bound_two, bound_two);
  return res;
}

/* For a given clast NAME, returns -1 if NAME is not in the
   INDEX_TABLE, otherwise returns the loop level for the induction
   variable NAME, or if it is a parameter, the parameter number in the
   vector of parameters.  */

static inline int
clast_name_to_level (clast_name_p name, clast_index_htab_type index_table)
{
  struct clast_name_index tmp;
  clast_name_index **slot;

  gcc_assert (name->type == clast_expr_name);
  tmp.name = ((const struct clast_name *) name)->name;
  tmp.free_name = NULL;

  slot = index_table.find_slot (&tmp, NO_INSERT);

  if (slot && *slot)
    return ((struct clast_name_index *) *slot)->level;

  return -1;
}

/* For a given clast NAME, returns -1 if it does not correspond to any
   parameter, or otherwise, returns the index in the PARAMS or
   SCATTERING_DIMENSIONS vector.  */

static inline int
clast_name_to_index (struct clast_name *name, clast_index_htab_type index_table)
{
  struct clast_name_index tmp;
  clast_name_index **slot;

  tmp.name = ((const struct clast_name *) name)->name;
  tmp.free_name = NULL;

  slot = index_table.find_slot (&tmp, NO_INSERT);

  if (slot && *slot)
    return (*slot)->index;

  return -1;
}

/* For a given clast NAME, initializes the lower and upper bounds BOUND_ONE
   and BOUND_TWO stored in the INDEX_TABLE.  Returns true when NAME has been
   found in the INDEX_TABLE, false otherwise.  */

static inline bool
clast_name_to_lb_ub (struct clast_name *name, clast_index_htab_type index_table,
		     mpz_t bound_one, mpz_t bound_two)
{
  struct clast_name_index tmp;
  clast_name_index **slot;

  tmp.name = name->name;
  tmp.free_name = NULL;

  slot = index_table.find_slot (&tmp, NO_INSERT);

  if (slot && *slot)
    {
      mpz_set (bound_one, ((struct clast_name_index *) *slot)->bound_one);
      mpz_set (bound_two, ((struct clast_name_index *) *slot)->bound_two);
      return true;
    }

  return false;
}

/* Records in INDEX_TABLE the INDEX and LEVEL for NAME.  */

static inline void
save_clast_name_index (clast_index_htab_type index_table, const char *name,
		       int index, int level, mpz_t bound_one, mpz_t bound_two)
{
  struct clast_name_index tmp;
  clast_name_index **slot;

  tmp.name = name;
  tmp.free_name = NULL;
  slot = index_table.find_slot (&tmp, INSERT);

  if (slot)
    {
      free (*slot);

      *slot = new_clast_name_index (name, index, level, bound_one, bound_two);
    }
}


/* NEWIVS_INDEX binds CLooG's scattering name to the index of the tree
   induction variable in NEWIVS.

   PARAMS_INDEX binds CLooG's parameter name to the index of the tree
   parameter in PARAMS.  */

typedef struct ivs_params {
  vec<tree> params, *newivs;
  clast_index_htab_type newivs_index, params_index;
  sese region;
} *ivs_params_p;

/* Returns the tree variable from the name NAME that was given in
   Cloog representation.  */

static tree
clast_name_to_gcc (struct clast_name *name, ivs_params_p ip)
{
  int index;

  if (ip->params.exists () && ip->params_index.is_created ())
    {
      index = clast_name_to_index (name, ip->params_index);

      if (index >= 0)
	return ip->params[index];
    }

  gcc_assert (ip->newivs && ip->newivs_index.is_created ());
  index = clast_name_to_index (name, ip->newivs_index);
  gcc_assert (index >= 0);

  return (*ip->newivs)[index];
}

/* Returns the maximal precision type for expressions TYPE1 and TYPE2.  */

static tree
max_precision_type (tree type1, tree type2)
{
  enum machine_mode mode;
  int p1, p2, precision;
  tree type;

  if (POINTER_TYPE_P (type1))
    return type1;

  if (POINTER_TYPE_P (type2))
    return type2;

  if (TYPE_UNSIGNED (type1)
      && TYPE_UNSIGNED (type2))
    return TYPE_PRECISION (type1) > TYPE_PRECISION (type2) ? type1 : type2;

  p1 = TYPE_PRECISION (type1);
  p2 = TYPE_PRECISION (type2);

  if (p1 > p2)
    precision = TYPE_UNSIGNED (type1) ? p1 * 2 : p1;
  else
    precision = TYPE_UNSIGNED (type2) ? p2 * 2 : p2;

  if (precision > BITS_PER_WORD)
    {
      gloog_error = true;
      return integer_type_node;
    }

  mode = smallest_mode_for_size (precision, MODE_INT);
  precision = GET_MODE_PRECISION (mode);
  type = build_nonstandard_integer_type (precision, false);

  if (!type)
    {
      gloog_error = true;
      return integer_type_node;
    }

  return type;
}

static tree
clast_to_gcc_expression (tree, struct clast_expr *, ivs_params_p);

/* Converts a Cloog reduction expression R with reduction operation OP
   to a GCC expression tree of type TYPE.  */

static tree
clast_to_gcc_expression_red (tree type, enum tree_code op,
			     struct clast_reduction *r, ivs_params_p ip)
{
  int i;
  tree res = clast_to_gcc_expression (type, r->elts[0], ip);
  tree operand_type = (op == POINTER_PLUS_EXPR) ? sizetype : type;

  for (i = 1; i < r->n; i++)
    {
      tree t = clast_to_gcc_expression (operand_type, r->elts[i], ip);
      res = fold_build2 (op, type, res, t);
    }

  return res;
}

/* Converts a Cloog AST expression E back to a GCC expression tree of
   type TYPE.  */

static tree
clast_to_gcc_expression (tree type, struct clast_expr *e, ivs_params_p ip)
{
  switch (e->type)
    {
    case clast_expr_name:
      {
	return clast_name_to_gcc ((struct clast_name *) e, ip);
      }
    case clast_expr_term:
      {
	struct clast_term *t = (struct clast_term *) e;

	if (t->var)
	  {
	    if (mpz_cmp_si (t->val, 1) == 0)
	      {
		tree name = clast_to_gcc_expression (type, t->var, ip);

		if (POINTER_TYPE_P (TREE_TYPE (name)) != POINTER_TYPE_P (type))
		  name = convert_to_ptrofftype (name);

		name = fold_convert (type, name);
		return name;
	      }

	    else if (mpz_cmp_si (t->val, -1) == 0)
	      {
		tree name = clast_to_gcc_expression (type, t->var, ip);

		if (POINTER_TYPE_P (TREE_TYPE (name)) != POINTER_TYPE_P (type))
		  name = convert_to_ptrofftype (name);

		name = fold_convert (type, name);

		return fold_build1 (NEGATE_EXPR, type, name);
	      }
	    else
	      {
		tree name = clast_to_gcc_expression (type, t->var, ip);
		tree cst = gmp_cst_to_tree (type, t->val);

		if (POINTER_TYPE_P (TREE_TYPE (name)) != POINTER_TYPE_P (type))
		  name = convert_to_ptrofftype (name);

		name = fold_convert (type, name);

		if (!POINTER_TYPE_P (type))
		  return fold_build2 (MULT_EXPR, type, cst, name);

		gloog_error = true;
		return cst;
	      }
	  }
	else
	  return gmp_cst_to_tree (type, t->val);
      }

    case clast_expr_red:
      {
        struct clast_reduction *r = (struct clast_reduction *) e;

        switch (r->type)
          {
	  case clast_red_sum:
	    return clast_to_gcc_expression_red
	      (type, POINTER_TYPE_P (type) ? POINTER_PLUS_EXPR : PLUS_EXPR,
	       r, ip);

	  case clast_red_min:
	    return clast_to_gcc_expression_red (type, MIN_EXPR, r, ip);

	  case clast_red_max:
	    return clast_to_gcc_expression_red (type, MAX_EXPR, r, ip);

	  default:
	    gcc_unreachable ();
          }
        break;
      }

    case clast_expr_bin:
      {
	struct clast_binary *b = (struct clast_binary *) e;
	struct clast_expr *lhs = (struct clast_expr *) b->LHS;
	tree tl = clast_to_gcc_expression (type, lhs, ip);
	tree tr = gmp_cst_to_tree (type, b->RHS);

	switch (b->type)
	  {
	  case clast_bin_fdiv:
	    return fold_build2 (FLOOR_DIV_EXPR, type, tl, tr);

	  case clast_bin_cdiv:
	    return fold_build2 (CEIL_DIV_EXPR, type, tl, tr);

	  case clast_bin_div:
	    return fold_build2 (EXACT_DIV_EXPR, type, tl, tr);

	  case clast_bin_mod:
	    return fold_build2 (TRUNC_MOD_EXPR, type, tl, tr);

	  default:
	    gcc_unreachable ();
	  }
      }

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Return a type that could represent the values between BOUND_ONE and
   BOUND_TWO.  */

static tree
type_for_interval (mpz_t bound_one, mpz_t bound_two)
{
  bool unsigned_p;
  tree type;
  enum machine_mode mode;
  int wider_precision;
  int precision = MAX (mpz_sizeinbase (bound_one, 2),
		       mpz_sizeinbase (bound_two, 2));

  if (precision > BITS_PER_WORD)
    {
      gloog_error = true;
      return integer_type_node;
    }

  if (mpz_cmp (bound_one, bound_two) <= 0)
    unsigned_p = (mpz_sgn (bound_one) >= 0);
  else
    unsigned_p = (mpz_sgn (bound_two) >= 0);

  mode = smallest_mode_for_size (precision, MODE_INT);
  wider_precision = GET_MODE_PRECISION (mode);

  /* As we want to generate signed types as much as possible, try to
     fit the interval [bound_one, bound_two] in a signed type.  For example,
     supposing that we have the interval [0, 100], instead of
     generating unsigned char, we want to generate a signed char.  */
  if (unsigned_p && precision < wider_precision)
    unsigned_p = false;

  type = build_nonstandard_integer_type (wider_precision, unsigned_p);

  if (!type)
    {
      gloog_error = true;
      return integer_type_node;
    }

  return type;
}

/* Return a type that could represent the integer value VAL, or
   otherwise return NULL_TREE.  */

static tree
type_for_value (mpz_t val)
{
  return type_for_interval (val, val);
}

static tree
type_for_clast_expr (struct clast_expr *, ivs_params_p, mpz_t, mpz_t);

/* Return the type for the clast_term T.  Initializes BOUND_ONE and
   BOUND_TWO to the bounds of the term.  */

static tree
type_for_clast_term (struct clast_term *t, ivs_params_p ip, mpz_t bound_one,
		     mpz_t bound_two)
{
  tree type;
  gcc_assert (t->expr.type == clast_expr_term);

  if (!t->var)
    {
      mpz_set (bound_one, t->val);
      mpz_set (bound_two, t->val);
      return type_for_value (t->val);
    }

  type = type_for_clast_expr (t->var, ip, bound_one, bound_two);

  mpz_mul (bound_one, bound_one, t->val);
  mpz_mul (bound_two, bound_two, t->val);

  return max_precision_type (type, type_for_interval (bound_one, bound_two));
}

/* Return the type for the clast_reduction R.  Initializes BOUND_ONE
   and BOUND_TWO to the bounds of the reduction expression.  */

static tree
type_for_clast_red (struct clast_reduction *r, ivs_params_p ip,
		    mpz_t bound_one, mpz_t bound_two)
{
  int i;
  tree type = type_for_clast_expr (r->elts[0], ip, bound_one, bound_two);
  mpz_t b1, b2, m1, m2;

  if (r->n == 1)
    return type;

  mpz_init (b1);
  mpz_init (b2);
  mpz_init (m1);
  mpz_init (m2);

  for (i = 1; i < r->n; i++)
    {
      tree t = type_for_clast_expr (r->elts[i], ip, b1, b2);
      type = max_precision_type (type, t);

      switch (r->type)
	{
	case clast_red_sum:
	  value_min (m1, bound_one, bound_two);
	  value_min (m2, b1, b2);
	  mpz_add (bound_one, m1, m2);

	  value_max (m1, bound_one, bound_two);
	  value_max (m2, b1, b2);
	  mpz_add (bound_two, m1, m2);
	  break;

	case clast_red_min:
	  value_min (bound_one, bound_one, bound_two);
	  value_min (bound_two, b1, b2);
	  break;

	case clast_red_max:
	  value_max (bound_one, bound_one, bound_two);
	  value_max (bound_two, b1, b2);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
    }

  mpz_clear (b1);
  mpz_clear (b2);
  mpz_clear (m1);
  mpz_clear (m2);

  /* Return a type that can represent the result of the reduction.  */
  return max_precision_type (type, type_for_interval (bound_one, bound_two));
}

/* Return the type for the clast_binary B used in STMT.  */

static tree
type_for_clast_bin (struct clast_binary *b, ivs_params_p ip, mpz_t bound_one,
		    mpz_t bound_two)
{
  mpz_t one;
  tree l = type_for_clast_expr ((struct clast_expr *) b->LHS, ip,
				bound_one, bound_two);
  tree r = type_for_value (b->RHS);
  tree type = max_precision_type (l, r);

  switch (b->type)
    {
    case clast_bin_fdiv:
      mpz_mdiv (bound_one, bound_one, b->RHS);
      mpz_mdiv (bound_two, bound_two, b->RHS);
      break;

    case clast_bin_cdiv:
      mpz_mdiv (bound_one, bound_one, b->RHS);
      mpz_mdiv (bound_two, bound_two, b->RHS);
      mpz_init (one);
      mpz_add (bound_one, bound_one, one);
      mpz_add (bound_two, bound_two, one);
      mpz_clear (one);
      break;

    case clast_bin_div:
      mpz_div (bound_one, bound_one, b->RHS);
      mpz_div (bound_two, bound_two, b->RHS);
      break;

    case clast_bin_mod:
      mpz_mod (bound_one, bound_one, b->RHS);
      mpz_mod (bound_two, bound_two, b->RHS);
      break;

    default:
      gcc_unreachable ();
    }

  /* Return a type that can represent the result of the reduction.  */
  return max_precision_type (type, type_for_interval (bound_one, bound_two));
}

/* Return the type for the clast_name NAME.  Initializes BOUND_ONE and
   BOUND_TWO to the bounds of the term.  */

static tree
type_for_clast_name (struct clast_name *name, ivs_params_p ip, mpz_t bound_one,
		     mpz_t bound_two)
{
  bool found = false;

  if (ip->params.exists () && ip->params_index.is_created ())
    found = clast_name_to_lb_ub (name, ip->params_index, bound_one, bound_two);

  if (!found)
    {
      gcc_assert (ip->newivs && ip->newivs_index.is_created ());
      found = clast_name_to_lb_ub (name, ip->newivs_index, bound_one,
				   bound_two);
      gcc_assert (found);
    }

    return TREE_TYPE (clast_name_to_gcc (name, ip));
}

/* Returns the type for the CLAST expression E when used in statement
   STMT.  */

static tree
type_for_clast_expr (struct clast_expr *e, ivs_params_p ip, mpz_t bound_one,
		     mpz_t bound_two)
{
  switch (e->type)
    {
    case clast_expr_term:
      return type_for_clast_term ((struct clast_term *) e, ip,
				  bound_one, bound_two);

    case clast_expr_red:
      return type_for_clast_red ((struct clast_reduction *) e, ip,
				 bound_one, bound_two);

    case clast_expr_bin:
      return type_for_clast_bin ((struct clast_binary *) e, ip,
				 bound_one, bound_two);

    case clast_expr_name:
      return type_for_clast_name ((struct clast_name *) e, ip,
				 bound_one, bound_two);

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Returns true if the clast expression E is a constant with VALUE.  */

static bool
clast_expr_const_value_p (struct clast_expr *e, int value)
{
  struct clast_term *t;
  if (e->type != clast_expr_term)
    return false;
  t = (struct clast_term *)e;
  if (t->var)
    return false;
  return 0 == mpz_cmp_si (t->val, value);
}

/* Translates a clast equation CLEQ to a tree.  */

static tree
graphite_translate_clast_equation (struct clast_equation *cleq,
				   ivs_params_p ip)
{
  enum tree_code comp;
  tree type, lhs, rhs, ltype, rtype;
  mpz_t bound_one, bound_two;
  struct clast_expr *clhs, *crhs;

  clhs = cleq->LHS;
  crhs = cleq->RHS;
  if (cleq->sign == 0)
    comp = EQ_EXPR;
  else if (cleq->sign > 0)
    comp = GE_EXPR;
  else
    comp = LE_EXPR;

  /* Special cases to reduce range of arguments to hopefully
     don't need types with larger precision than the input.  */
  if (crhs->type == clast_expr_red
      && comp != EQ_EXPR)
    {
      struct clast_reduction *r = (struct clast_reduction *) crhs;
      /* X >= A+1 --> X > A and
         X <= A-1 --> X < A  */
      if (r->n == 2
	  && r->type == clast_red_sum
	  && clast_expr_const_value_p (r->elts[1], comp == GE_EXPR ? 1 : -1))
	{
	  crhs = r->elts[0];
	  comp = comp == GE_EXPR ? GT_EXPR : LT_EXPR;
	}
    }

  mpz_init (bound_one);
  mpz_init (bound_two);

  ltype = type_for_clast_expr (clhs, ip, bound_one, bound_two);
  rtype = type_for_clast_expr (crhs, ip, bound_one, bound_two);

  mpz_clear (bound_one);
  mpz_clear (bound_two);
  type = max_precision_type (ltype, rtype);

  lhs = clast_to_gcc_expression (type, clhs, ip);
  rhs = clast_to_gcc_expression (type, crhs, ip);

  return fold_build2 (comp, boolean_type_node, lhs, rhs);
}

/* Creates the test for the condition in STMT.  */

static tree
graphite_create_guard_cond_expr (struct clast_guard *stmt,
				 ivs_params_p ip)
{
  tree cond = NULL;
  int i;

  for (i = 0; i < stmt->n; i++)
    {
      tree eq = graphite_translate_clast_equation (&stmt->eq[i], ip);

      if (cond)
	cond = fold_build2 (TRUTH_AND_EXPR, TREE_TYPE (eq), cond, eq);
      else
	cond = eq;
    }

  return cond;
}

/* Creates a new if region corresponding to Cloog's guard.  */

static edge
graphite_create_new_guard (edge entry_edge, struct clast_guard *stmt,
			   ivs_params_p ip)
{
  tree cond_expr = graphite_create_guard_cond_expr (stmt, ip);
  edge exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);
  return exit_edge;
}

/* Compute the lower bound LOW and upper bound UP for the parameter
   PARAM in scop SCOP based on the constraints in the context.  */

static void
compute_bounds_for_param (scop_p scop, int param, mpz_t low, mpz_t up)
{
  isl_int v;
  isl_aff *aff = isl_aff_zero_on_domain
    (isl_local_space_from_space (isl_set_get_space (scop->context)));

  aff = isl_aff_add_coefficient_si (aff, isl_dim_param, param, 1);

  isl_int_init (v);
  isl_set_min (scop->context, aff, &v);
  isl_int_get_gmp (v, low);
  isl_set_max (scop->context, aff, &v);
  isl_int_get_gmp (v, up);
  isl_int_clear (v);
  isl_aff_free (aff);
}

/* Compute the lower bound LOW and upper bound UP for the induction
   variable of loop LOOP.

   FIXME: This one is not entirely correct, as min/max expressions in the
	  calculation can yield to incorrect results. To be completely
	  correct, we need to evaluate each subexpression generated by
          CLooG. CLooG does not yet support this, so this is as good as
	  it can be. */

static void
compute_bounds_for_loop (struct clast_for *loop, mpz_t low, mpz_t up)
{
  isl_set *domain;
  isl_aff *dimension;
  isl_local_space *local_space;
  isl_int isl_value;
  enum isl_lp_result lp_result;

  domain = isl_set_copy (isl_set_from_cloog_domain (loop->domain));
  local_space = isl_local_space_from_space (isl_set_get_space (domain));
  dimension = isl_aff_zero_on_domain (local_space);
  dimension = isl_aff_add_coefficient_si (dimension, isl_dim_in,
					  isl_set_dim (domain, isl_dim_set) - 1,
					  1);

  isl_int_init (isl_value);

  lp_result = isl_set_min (domain, dimension, &isl_value);
  assert (lp_result == isl_lp_ok);
  isl_int_get_gmp (isl_value, low);

  lp_result = isl_set_max (domain, dimension, &isl_value);
  assert (lp_result == isl_lp_ok);
  isl_int_get_gmp (isl_value, up);

  isl_int_clear (isl_value);
  isl_set_free (domain);
  isl_aff_free (dimension);
}

/* Returns the type for the induction variable for the loop translated
   from STMT_FOR.  */

static tree
type_for_clast_for (struct clast_for *stmt_for, ivs_params_p ip)
{
  mpz_t bound_one, bound_two;
  tree lb_type, ub_type;

  mpz_init (bound_one);
  mpz_init (bound_two);

  lb_type = type_for_clast_expr (stmt_for->LB, ip, bound_one, bound_two);
  ub_type = type_for_clast_expr (stmt_for->UB, ip, bound_one, bound_two);

  mpz_clear (bound_one);
  mpz_clear (bound_two);

  return max_precision_type (lb_type, ub_type);
}

/* Creates a new LOOP corresponding to Cloog's STMT.  Inserts an
   induction variable for the new LOOP.  New LOOP is attached to CFG
   starting at ENTRY_EDGE.  LOOP is inserted into the loop tree and
   becomes the child loop of the OUTER_LOOP.  NEWIVS_INDEX binds
   CLooG's scattering name to the induction variable created for the
   loop of STMT.  The new induction variable is inserted in the NEWIVS
   vector and is of type TYPE.  */

static struct loop *
graphite_create_new_loop (edge entry_edge, struct clast_for *stmt,
			  loop_p outer, tree type, tree lb, tree ub,
			  int level, ivs_params_p ip)
{
  mpz_t low, up;

  tree stride = gmp_cst_to_tree (type, stmt->stride);
  tree ivvar = create_tmp_var (type, "graphite_IV");
  tree iv, iv_after_increment;
  loop_p loop = create_empty_loop_on_edge
    (entry_edge, lb, stride, ub, ivvar, &iv, &iv_after_increment,
     outer ? outer : entry_edge->src->loop_father);

  mpz_init (low);
  mpz_init (up);
  compute_bounds_for_loop (stmt, low, up);
  save_clast_name_index (ip->newivs_index, stmt->iterator,
			 (*ip->newivs).length (), level, low, up);
  mpz_clear (low);
  mpz_clear (up);
  (*ip->newivs).safe_push (iv);
  return loop;
}

/* Inserts in iv_map a tuple (OLD_LOOP->num, NEW_NAME) for the
   induction variables of the loops around GBB in SESE.  */

static void
build_iv_mapping (vec<tree> iv_map, struct clast_user_stmt *user_stmt,
		  ivs_params_p ip)
{
  struct clast_stmt *t;
  int depth = 0;
  CloogStatement *cs = user_stmt->statement;
  poly_bb_p pbb = (poly_bb_p) cs->usr;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
  mpz_t bound_one, bound_two;

  mpz_init (bound_one);
  mpz_init (bound_two);

  for (t = user_stmt->substitutions; t; t = t->next, depth++)
    {
      struct clast_expr *expr = (struct clast_expr *)
       ((struct clast_assignment *)t)->RHS;
      tree type = type_for_clast_expr (expr, ip, bound_one, bound_two);
      tree new_name = clast_to_gcc_expression (type, expr, ip);
      loop_p old_loop = gbb_loop_at_index (gbb, ip->region, depth);

      iv_map[old_loop->num] = new_name;
    }

  mpz_clear (bound_one);
  mpz_clear (bound_two);
}

/* Construct bb_pbb_def with BB and PBB.  */

static bb_pbb_def *
new_bb_pbb_def (basic_block bb, poly_bb_p pbb)
{
  bb_pbb_def *bb_pbb_p;

  bb_pbb_p = XNEW (bb_pbb_def);
  bb_pbb_p->bb = bb;
  bb_pbb_p->pbb = pbb;

  return bb_pbb_p;
}

/* Mark BB with it's relevant PBB via hashing table BB_PBB_MAPPING.  */

static void
mark_bb_with_pbb (poly_bb_p pbb, basic_block bb,
		  bb_pbb_htab_type bb_pbb_mapping)
{
  bb_pbb_def tmp;
  bb_pbb_def **x;

  tmp.bb = bb;
  x = bb_pbb_mapping.find_slot (&tmp, INSERT);

  if (x && !*x)
    *x = new_bb_pbb_def (bb, pbb);
}

/* Find BB's related poly_bb_p in hash table BB_PBB_MAPPING.  */

poly_bb_p
find_pbb_via_hash (bb_pbb_htab_type bb_pbb_mapping, basic_block bb)
{
  bb_pbb_def tmp;
  bb_pbb_def **slot;

  tmp.bb = bb;
  slot = bb_pbb_mapping.find_slot (&tmp, NO_INSERT);

  if (slot && *slot)
    return ((bb_pbb_def *) *slot)->pbb;

  return NULL;
}

/* Return the scop of the loop and initialize PBBS the set of
   poly_bb_p that belong to the LOOP.  BB_PBB_MAPPING is a map created
   by the CLAST code generator between a generated basic_block and its
   related poly_bb_p.  */

scop_p
get_loop_body_pbbs (loop_p loop, bb_pbb_htab_type bb_pbb_mapping,
		    vec<poly_bb_p> *pbbs)
{
  unsigned i;
  basic_block *bbs = get_loop_body_in_dom_order (loop);
  scop_p scop = NULL;

  for (i = 0; i < loop->num_nodes; i++)
    {
      poly_bb_p pbb = find_pbb_via_hash (bb_pbb_mapping, bbs[i]);

      if (pbb == NULL)
	continue;

      scop = PBB_SCOP (pbb);
      (*pbbs).safe_push (pbb);
    }

  free (bbs);
  return scop;
}

/* Translates a clast user statement STMT to gimple.

   - NEXT_E is the edge where new generated code should be attached.
   - CONTEXT_LOOP is the loop in which the generated code will be placed
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.  */

static edge
translate_clast_user (struct clast_user_stmt *stmt, edge next_e,
		      bb_pbb_htab_type bb_pbb_mapping, ivs_params_p ip)
{
  int i, nb_loops;
  basic_block new_bb;
  poly_bb_p pbb = (poly_bb_p) stmt->statement->usr;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
  vec<tree> iv_map;

  if (GBB_BB (gbb) == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    return next_e;

  nb_loops = number_of_loops (cfun);
  iv_map.create (nb_loops);
  for (i = 0; i < nb_loops; i++)
    iv_map.quick_push (NULL_TREE);

  build_iv_mapping (iv_map, stmt, ip);
  next_e = copy_bb_and_scalar_dependences (GBB_BB (gbb), ip->region,
					   next_e, iv_map, &gloog_error);
  iv_map.release ();

  new_bb = next_e->src;
  mark_bb_with_pbb (pbb, new_bb, bb_pbb_mapping);
  mark_virtual_operands_for_renaming (cfun);
  update_ssa (TODO_update_ssa);

  return next_e;
}

/* Creates a new if region protecting the loop to be executed, if the execution
   count is zero (lb > ub).  */

static edge
graphite_create_new_loop_guard (edge entry_edge, struct clast_for *stmt,
				tree *type, tree *lb, tree *ub,
				ivs_params_p ip)
{
  tree cond_expr;
  edge exit_edge;

  *type = type_for_clast_for (stmt, ip);
  *lb = clast_to_gcc_expression (*type, stmt->LB, ip);
  *ub = clast_to_gcc_expression (*type, stmt->UB, ip);

  /* When ub is simply a constant or a parameter, use lb <= ub.  */
  if (TREE_CODE (*ub) == INTEGER_CST || TREE_CODE (*ub) == SSA_NAME)
    cond_expr = fold_build2 (LE_EXPR, boolean_type_node, *lb, *ub);
  else
    {
      tree one = (POINTER_TYPE_P (*type)
		  ? convert_to_ptrofftype (integer_one_node)
		  : fold_convert (*type, integer_one_node));
      /* Adding +1 and using LT_EXPR helps with loop latches that have a
	 loop iteration count of "PARAMETER - 1".  For PARAMETER == 0 this becomes
	 2^k-1 due to integer overflow, and the condition lb <= ub is true,
	 even if we do not want this.  However lb < ub + 1 is false, as
	 expected.  */
      tree ub_one = fold_build2 (POINTER_TYPE_P (*type) ? POINTER_PLUS_EXPR
				 : PLUS_EXPR, *type, *ub, one);

      cond_expr = fold_build2 (LT_EXPR, boolean_type_node, *lb, ub_one);
    }

  exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);

  return exit_edge;
}

static edge
translate_clast (loop_p, struct clast_stmt *, edge, bb_pbb_htab_type,
		 int, ivs_params_p);

/* Create the loop for a clast for statement.

   - NEXT_E is the edge where new generated code should be attached.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.  */

static edge
translate_clast_for_loop (loop_p context_loop, struct clast_for *stmt,
			  edge next_e, bb_pbb_htab_type bb_pbb_mapping,
			  int level, tree type, tree lb, tree ub,
			  ivs_params_p ip)
{
  struct loop *loop = graphite_create_new_loop (next_e, stmt, context_loop,
						type, lb, ub, level, ip);
  edge last_e = single_exit (loop);
  edge to_body = single_succ_edge (loop->header);
  basic_block after = to_body->dest;

  /* Create a basic block for loop close phi nodes.  */
  last_e = single_succ_edge (split_edge (last_e));

  /* Translate the body of the loop.  */
  next_e = translate_clast (loop, stmt->body, to_body, bb_pbb_mapping,
			    level + 1, ip);
  redirect_edge_succ_nodup (next_e, after);
  set_immediate_dominator (CDI_DOMINATORS, next_e->dest, next_e->src);

  isl_set *domain = isl_set_from_cloog_domain (stmt->domain);
  int scheduling_dim = isl_set_n_dim (domain);

  if (flag_loop_parallelize_all
      && loop_is_parallel_p (loop, bb_pbb_mapping, scheduling_dim))
    loop->can_be_parallel = true;

  return last_e;
}

/* Translates a clast for statement STMT to gimple.  First a guard is created
   protecting the loop, if it is executed zero times.  In this guard we create
   the real loop structure.

   - NEXT_E is the edge where new generated code should be attached.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.  */

static edge
translate_clast_for (loop_p context_loop, struct clast_for *stmt, edge next_e,
		     bb_pbb_htab_type bb_pbb_mapping, int level,
		     ivs_params_p ip)
{
  tree type, lb, ub;
  edge last_e = graphite_create_new_loop_guard (next_e, stmt, &type,
						&lb, &ub, ip);
  edge true_e = get_true_edge_from_guard_bb (next_e->dest);

  translate_clast_for_loop (context_loop, stmt, true_e, bb_pbb_mapping, level,
			    type, lb, ub, ip);
  return last_e;
}

/* Translates a clast assignment STMT to gimple.

   - NEXT_E is the edge where new generated code should be attached.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.  */

static edge
translate_clast_assignment (struct clast_assignment *stmt, edge next_e,
			    int level, ivs_params_p ip)
{
  gimple_seq stmts;
  mpz_t bound_one, bound_two;
  tree type, new_name, var;
  edge res = single_succ_edge (split_edge (next_e));
  struct clast_expr *expr = (struct clast_expr *) stmt->RHS;

  mpz_init (bound_one);
  mpz_init (bound_two);
  type = type_for_clast_expr (expr, ip, bound_one, bound_two);
  var = create_tmp_var (type, "graphite_var");
  new_name = force_gimple_operand (clast_to_gcc_expression (type, expr, ip),
				   &stmts, true, var);
  if (stmts)
    {
      gsi_insert_seq_on_edge (next_e, stmts);
      gsi_commit_edge_inserts ();
    }

  save_clast_name_index (ip->newivs_index, stmt->LHS,
			 (*ip->newivs).length (), level,
			 bound_one, bound_two);
  (*ip->newivs).safe_push (new_name);

  mpz_clear (bound_one);
  mpz_clear (bound_two);

  return res;
}

/* Translates a clast guard statement STMT to gimple.

   - NEXT_E is the edge where new generated code should be attached.
   - CONTEXT_LOOP is the loop in which the generated code will be placed
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.  */

static edge
translate_clast_guard (loop_p context_loop, struct clast_guard *stmt,
		       edge next_e, bb_pbb_htab_type bb_pbb_mapping, int level,
		       ivs_params_p ip)
{
  edge last_e = graphite_create_new_guard (next_e, stmt, ip);
  edge true_e = get_true_edge_from_guard_bb (next_e->dest);

  translate_clast (context_loop, stmt->then, true_e, bb_pbb_mapping, level, ip);
  return last_e;
}

/* Translates a CLAST statement STMT to GCC representation in the
   context of a SESE.

   - NEXT_E is the edge where new generated code should be attached.
   - CONTEXT_LOOP is the loop in which the generated code will be placed
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.  */

static edge
translate_clast (loop_p context_loop, struct clast_stmt *stmt, edge next_e,
		 bb_pbb_htab_type bb_pbb_mapping, int level, ivs_params_p ip)
{
  if (!stmt)
    return next_e;

  if (CLAST_STMT_IS_A (stmt, stmt_root))
    ; /* Do nothing.  */

  else if (CLAST_STMT_IS_A (stmt, stmt_user))
    next_e = translate_clast_user ((struct clast_user_stmt *) stmt,
				   next_e, bb_pbb_mapping, ip);

  else if (CLAST_STMT_IS_A (stmt, stmt_for))
    next_e = translate_clast_for (context_loop, (struct clast_for *) stmt,
				  next_e, bb_pbb_mapping, level, ip);

  else if (CLAST_STMT_IS_A (stmt, stmt_guard))
    next_e = translate_clast_guard (context_loop, (struct clast_guard *) stmt,
				    next_e, bb_pbb_mapping, level, ip);

  else if (CLAST_STMT_IS_A (stmt, stmt_block))
    next_e = translate_clast (context_loop, ((struct clast_block *) stmt)->body,
			      next_e, bb_pbb_mapping, level, ip);

  else if (CLAST_STMT_IS_A (stmt, stmt_ass))
    next_e = translate_clast_assignment ((struct clast_assignment *) stmt,
					 next_e, level, ip);
  else
    gcc_unreachable ();

  recompute_all_dominators ();
  graphite_verify ();

  return translate_clast (context_loop, stmt->next, next_e, bb_pbb_mapping,
			  level, ip);
}

/* Add parameter and iterator names to the CloogUnionDomain.  */

static CloogUnionDomain *
add_names_to_union_domain (scop_p scop, CloogUnionDomain *union_domain,
			   int nb_scattering_dims,
			   clast_index_htab_type params_index)
{
  sese region = SCOP_REGION (scop);
  int i;
  int nb_iterators = scop_max_loop_depth (scop);
  int nb_parameters = SESE_PARAMS (region).length ();
  mpz_t bound_one, bound_two;

  mpz_init (bound_one);
  mpz_init (bound_two);

  for (i = 0; i < nb_parameters; i++)
    {
      tree param = SESE_PARAMS (region)[i];
      const char *name = get_name (param);
      int len;
      char *parameter;

      if (!name)
	name = "T";

      len = strlen (name);
      len += 17;
      parameter = XNEWVEC (char, len + 1);
      snprintf (parameter, len, "%s_%d", name, SSA_NAME_VERSION (param));
      save_clast_name_index (params_index, parameter, i, i, bound_one,
			     bound_two);
      union_domain = cloog_union_domain_set_name (union_domain, CLOOG_PARAM, i,
						  parameter);
      compute_bounds_for_param (scop, i, bound_one, bound_two);
      free (parameter);
    }

  mpz_clear (bound_one);
  mpz_clear (bound_two);

  for (i = 0; i < nb_iterators; i++)
    {
      int len = 4 + 16;
      char *iterator;
      iterator = XNEWVEC (char, len);
      snprintf (iterator, len, "git_%d", i);
      union_domain = cloog_union_domain_set_name (union_domain, CLOOG_ITER, i,
						  iterator);
      free (iterator);
    }

  for (i = 0; i < nb_scattering_dims; i++)
    {
      int len = 5 + 16;
      char *scattering;
      scattering = XNEWVEC (char, len);
      snprintf (scattering, len, "scat_%d", i);
      union_domain = cloog_union_domain_set_name (union_domain, CLOOG_SCAT, i,
						  scattering);
      free (scattering);
    }

  return union_domain;
}

/* Initialize a CLooG input file.  */

static FILE *
init_cloog_input_file (int scop_number)
{
  FILE *graphite_out_file;
  int len = strlen (dump_base_name);
  char *dumpname = XNEWVEC (char, len + 25);
  char *s_scop_number = XNEWVEC (char, 15);

  memcpy (dumpname, dump_base_name, len + 1);
  strip_off_ending (dumpname, len);
  sprintf (s_scop_number, ".%d", scop_number);
  strcat (dumpname, s_scop_number);
  strcat (dumpname, ".cloog");
  graphite_out_file = fopen (dumpname, "w+b");

  if (graphite_out_file == 0)
    fatal_error ("can%'t open %s for writing: %m", dumpname);

  free (dumpname);

  return graphite_out_file;
}

/* Extend the scattering to NEW_DIMS scattering dimensions.  */

static
isl_map *extend_scattering (isl_map *scattering, int new_dims)
{
  int old_dims, i;
  isl_space *space;
  isl_basic_map *change_scattering;
  isl_map *change_scattering_map;

  old_dims = isl_map_dim (scattering, isl_dim_out);

  space = isl_space_alloc (isl_map_get_ctx (scattering), 0, old_dims, new_dims);
  change_scattering = isl_basic_map_universe (isl_space_copy (space));

  for (i = 0; i < old_dims; i++)
    {
      isl_constraint *c;
      c = isl_equality_alloc
	(isl_local_space_from_space (isl_space_copy (space)));
      isl_constraint_set_coefficient_si (c, isl_dim_in, i, 1);
      isl_constraint_set_coefficient_si (c, isl_dim_out, i, -1);
      change_scattering = isl_basic_map_add_constraint (change_scattering, c);
    }

  for (i = old_dims; i < new_dims; i++)
    {
      isl_constraint *c;
      c = isl_equality_alloc
	(isl_local_space_from_space (isl_space_copy (space)));
      isl_constraint_set_coefficient_si (c, isl_dim_out, i, 1);
      change_scattering = isl_basic_map_add_constraint (change_scattering, c);
    }

  change_scattering_map = isl_map_from_basic_map (change_scattering);
  change_scattering_map = isl_map_align_params (change_scattering_map, space);
  return isl_map_apply_range (scattering, change_scattering_map);
}

/* Build cloog union domain for SCoP.  */

static CloogUnionDomain *
build_cloog_union_domain (scop_p scop, int nb_scattering_dims)
{
  int i;
  poly_bb_p pbb;
  CloogUnionDomain *union_domain =
    cloog_union_domain_alloc (scop_nb_params (scop));

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      CloogDomain *domain;
      CloogScattering *scattering;

      /* Dead code elimination: when the domain of a PBB is empty,
	 don't generate code for the PBB.  */
      if (isl_set_is_empty (pbb->domain))
	continue;

      domain = cloog_domain_from_isl_set (isl_set_copy (pbb->domain));
      scattering = cloog_scattering_from_isl_map
	(extend_scattering (isl_map_copy (pbb->transformed),
			    nb_scattering_dims));

      union_domain = cloog_union_domain_add_domain (union_domain, "", domain,
						    scattering, pbb);
    }

  return union_domain;
}

/* Return the options that will be used in GLOOG.  */

static CloogOptions *
set_cloog_options (void)
{
  CloogOptions *options = cloog_options_malloc (cloog_state);

  /* Change cloog output language to C.  If we do use FORTRAN instead, cloog
     will stop e.g. with "ERROR: unbounded loops not allowed in FORTRAN.", if
     we pass an incomplete program to cloog.  */
  options->language = CLOOG_LANGUAGE_C;

  /* Enable complex equality spreading: removes dummy statements
     (assignments) in the generated code which repeats the
     substitution equations for statements.  This is useless for
     GLooG.  */
  options->esp = 1;

  /* Silence CLooG to avoid failing tests due to debug output to stderr.  */
  options->quiet = 1;

  /* Allow cloog to build strides with a stride width different to one.
     This example has stride = 4:

     for (i = 0; i < 20; i += 4)
       A  */
  options->strides = 1;

  /* We want the clast to provide the iteration domains of the executed loops.
     This allows us to derive minimal/maximal values for the induction
     variables.  */
  options->save_domains = 1;

  /* Do not remove scalar dimensions.  CLooG by default removes scalar 
     dimensions very early from the input schedule.  However, they are 
     necessary to correctly derive from the saved domains 
     (options->save_domains) the relationship between the generated loops 
     and the schedule dimensions they are generated from.  */ 
  options->noscalars = 1;

  /* Disable optimizations and make cloog generate source code closer to the
     input.  This is useful for debugging,  but later we want the optimized
     code.

     XXX: We can not disable optimizations, as loop blocking is not working
     without them.  */
  if (0)
    {
      options->f = -1;
      options->l = INT_MAX;
    }

  return options;
}

/* Prints STMT to STDERR.  */

void
print_clast_stmt (FILE *file, struct clast_stmt *stmt)
{
  CloogOptions *options = set_cloog_options ();

  clast_pprint (file, stmt, 0, options);
  cloog_options_free (options);
}

/* Prints STMT to STDERR.  */

DEBUG_FUNCTION void
debug_clast_stmt (struct clast_stmt *stmt)
{
  print_clast_stmt (stderr, stmt);
}

/* Get the maximal number of scattering dimensions in the scop SCOP.  */

static
int get_max_scattering_dimensions (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int scattering_dims = 0;

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      int pbb_scatt_dims = isl_map_dim (pbb->transformed, isl_dim_out);
      if (pbb_scatt_dims > scattering_dims)
	scattering_dims = pbb_scatt_dims;
    }

  return scattering_dims;
}

static CloogInput *
generate_cloog_input (scop_p scop, clast_index_htab_type params_index)
{
  CloogUnionDomain *union_domain;
  CloogInput *cloog_input;
  CloogDomain *context;
  int nb_scattering_dims = get_max_scattering_dimensions (scop);

  union_domain = build_cloog_union_domain (scop, nb_scattering_dims);
  union_domain = add_names_to_union_domain (scop, union_domain,
					    nb_scattering_dims,
					    params_index);
  context = cloog_domain_from_isl_set (isl_set_copy (scop->context));

  cloog_input = cloog_input_alloc (context, union_domain);

  return cloog_input;
}

/* Translate SCOP to a CLooG program and clast.  These two
   representations should be freed together: a clast cannot be used
   without a program.  */

static struct clast_stmt *
scop_to_clast (scop_p scop, clast_index_htab_type params_index)
{
  CloogInput *cloog_input;
  struct clast_stmt *clast;
  CloogOptions *options = set_cloog_options ();

  cloog_input = generate_cloog_input (scop, params_index);

  /* Dump a .cloog input file, if requested.  This feature is only
     enabled in the Graphite branch.  */
  if (0)
  {
    static size_t file_scop_number = 0;
    FILE *cloog_file = init_cloog_input_file (file_scop_number);
    cloog_input_dump_cloog (cloog_file, cloog_input, options);
  }

  clast = cloog_clast_create_from_input (cloog_input, options);

  cloog_options_free (options);
  return clast;
}

/* Prints to FILE the code generated by CLooG for SCOP.  */

void
print_generated_program (FILE *file, scop_p scop)
{
  CloogOptions *options = set_cloog_options ();
  clast_index_htab_type params_index;
  struct clast_stmt *clast;

  params_index.create (10);

  clast = scop_to_clast (scop, params_index);

  fprintf (file, "       (clast: \n");
  clast_pprint (file, clast, 0, options);
  fprintf (file, "       )\n");

  cloog_options_free (options);
  cloog_clast_free (clast);
}

/* Prints to STDERR the code generated by CLooG for SCOP.  */

DEBUG_FUNCTION void
debug_generated_program (scop_p scop)
{
  print_generated_program (stderr, scop);
}

/* GIMPLE Loop Generator: generates loops from STMT in GIMPLE form for
   the given SCOP.  Return true if code generation succeeded.
   BB_PBB_MAPPING is a basic_block and it's related poly_bb_p mapping.
*/

bool
gloog (scop_p scop, bb_pbb_htab_type bb_pbb_mapping)
{
  auto_vec<tree, 10> newivs;
  loop_p context_loop;
  sese region = SCOP_REGION (scop);
  ifsese if_region = NULL;
  clast_index_htab_type newivs_index, params_index;
  struct clast_stmt *clast;
  struct ivs_params ip;

  timevar_push (TV_GRAPHITE_CODE_GEN);
  gloog_error = false;

  params_index.create (10);

  clast = scop_to_clast (scop, params_index);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCLAST generated by CLooG: \n");
      print_clast_stmt (dump_file, clast);
      fprintf (dump_file, "\n");
    }

  recompute_all_dominators ();
  graphite_verify ();

  if_region = move_sese_in_condition (region);
  sese_insert_phis_for_liveouts (region,
				 if_region->region->exit->src,
				 if_region->false_region->exit,
				 if_region->true_region->exit);
  recompute_all_dominators ();
  graphite_verify ();

  context_loop = SESE_ENTRY (region)->src->loop_father;
  newivs_index.create (10);

  ip.newivs = &newivs;
  ip.newivs_index = newivs_index;
  ip.params = SESE_PARAMS (region);
  ip.params_index = params_index;
  ip.region = region;

  translate_clast (context_loop, clast, if_region->true_region->entry,
		   bb_pbb_mapping, 0, &ip);
  graphite_verify ();
  scev_reset ();
  recompute_all_dominators ();
  graphite_verify ();

  if (gloog_error)
    set_ifsese_condition (if_region, integer_zero_node);

  free (if_region->true_region);
  free (if_region->region);
  free (if_region);

  newivs_index.dispose ();
  params_index.dispose ();
  cloog_clast_free (clast);
  timevar_pop (TV_GRAPHITE_CODE_GEN);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      loop_p loop;
      int num_no_dependency = 0;

      FOR_EACH_LOOP (loop, 0)
	if (loop->can_be_parallel)
	  num_no_dependency++;

      fprintf (dump_file, "\n%d loops carried no dependency.\n",
	       num_no_dependency);
    }

  return !gloog_error;
}
#endif
