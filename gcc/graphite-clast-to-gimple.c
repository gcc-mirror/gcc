/* Translation of CLAST (CLooG AST) to Gimple.
   Copyright (C) 2009 Free Software Foundation, Inc.
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
#include "graphite-clast-to-gimple.h"
#include "graphite-dependences.h"

/* This flag is set when an error occurred during the translation of
   CLAST to Gimple.  */
static bool gloog_error;

/* Verifies properties that GRAPHITE should maintain during translation.  */

static inline void
graphite_verify (void)
{
#ifdef ENABLE_CHECKING
  verify_loop_structure ();
  verify_dominators (CDI_DOMINATORS);
  verify_dominators (CDI_POST_DOMINATORS);
  verify_ssa (false);
  verify_loop_closed_ssa ();
#endif
}

/* Stores the INDEX in a vector for a given clast NAME.  */

typedef struct clast_name_index {
  int index;
  const char *name;
} *clast_name_index_p;

/* Returns a pointer to a new element of type clast_name_index_p built
   from NAME and INDEX.  */

static inline clast_name_index_p
new_clast_name_index (const char *name, int index)
{
  clast_name_index_p res = XNEW (struct clast_name_index);

  res->name = name;
  res->index = index;
  return res;
}

/* For a given clast NAME, returns -1 if it does not correspond to any
   parameter, or otherwise, returns the index in the PARAMS or
   SCATTERING_DIMENSIONS vector.  */

static inline int
clast_name_to_index (const char *name, htab_t index_table)
{
  struct clast_name_index tmp;
  PTR *slot;

  tmp.name = name;
  slot = htab_find_slot (index_table, &tmp, NO_INSERT);

  if (slot && *slot)
    return ((struct clast_name_index *) *slot)->index;

  return -1;
}

/* Records in INDEX_TABLE the INDEX for NAME.  */

static inline void
save_clast_name_index (htab_t index_table, const char *name, int index)
{
  struct clast_name_index tmp;
  PTR *slot;

  tmp.name = name;
  slot = htab_find_slot (index_table, &tmp, INSERT);

  if (slot)
    {
      if (*slot)
	free (*slot);

      *slot = new_clast_name_index (name, index);
    }
}

/* Print to stderr the element ELT.  */

static inline void
debug_clast_name_index (clast_name_index_p elt)
{
  fprintf (stderr, "(index = %d, name = %s)\n", elt->index, elt->name);
}

/* Helper function for debug_rename_map.  */

static inline int
debug_clast_name_indexes_1 (void **slot, void *s ATTRIBUTE_UNUSED)
{
  struct clast_name_index *entry = (struct clast_name_index *) *slot;
  debug_clast_name_index (entry);
  return 1;
}

/* Print to stderr all the elements of MAP.  */

void
debug_clast_name_indexes (htab_t map)
{
  htab_traverse (map, debug_clast_name_indexes_1, NULL);
}

/* Computes a hash function for database element ELT.  */

static inline hashval_t
clast_name_index_elt_info (const void *elt)
{
  return htab_hash_pointer (((const struct clast_name_index *) elt)->name);
}

/* Compares database elements E1 and E2.  */

static inline int
eq_clast_name_indexes (const void *e1, const void *e2)
{
  const struct clast_name_index *elt1 = (const struct clast_name_index *) e1;
  const struct clast_name_index *elt2 = (const struct clast_name_index *) e2;

  return (elt1->name == elt2->name);
}


/* For a given loop DEPTH in the loop nest of the original black box
   PBB, return the old induction variable associated to that loop.  */

static inline tree
pbb_to_depth_to_oldiv (poly_bb_p pbb, int depth)
{
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
  sese region = SCOP_REGION (PBB_SCOP (pbb));
  loop_p loop = gbb_loop_at_index (gbb, region, depth);

  return loop->single_iv;
}

/* For a given scattering dimension, return the new induction variable
   associated to it.  */

static inline tree
newivs_to_depth_to_newiv (VEC (tree, heap) *newivs, int depth)
{
  return VEC_index (tree, newivs, depth);
}



/* Returns the tree variable from the name NAME that was given in
   Cloog representation.  */

static tree
clast_name_to_gcc (const char *name, sese region, VEC (tree, heap) *newivs,
		   htab_t newivs_index, htab_t params_index)
{
  int index;
  VEC (tree, heap) *params = SESE_PARAMS (region);

  if (params && params_index)
    {
      index = clast_name_to_index (name, params_index);

      if (index >= 0)
	return VEC_index (tree, params, index);
    }

  gcc_assert (newivs && newivs_index);
  index = clast_name_to_index (name, newivs_index);
  gcc_assert (index >= 0);

  return newivs_to_depth_to_newiv (newivs, index);
}

/* Returns the maximal precision type for expressions E1 and E2.  */

static inline tree
max_precision_type (tree e1, tree e2)
{
  tree type1 = TREE_TYPE (e1);
  tree type2 = TREE_TYPE (e2);
  return TYPE_PRECISION (type1) > TYPE_PRECISION (type2) ? type1 : type2;
}

static tree
clast_to_gcc_expression (tree, struct clast_expr *, sese, VEC (tree, heap) *,
			 htab_t, htab_t);

/* Converts a Cloog reduction expression R with reduction operation OP
   to a GCC expression tree of type TYPE.  */

static tree
clast_to_gcc_expression_red (tree type, enum tree_code op,
			     struct clast_reduction *r,
			     sese region, VEC (tree, heap) *newivs,
			     htab_t newivs_index, htab_t params_index)
{
  int i;
  tree res = clast_to_gcc_expression (type, r->elts[0], region, newivs,
				      newivs_index, params_index);
  tree operand_type = (op == POINTER_PLUS_EXPR) ? sizetype : type;

  for (i = 1; i < r->n; i++)
    {
      tree t = clast_to_gcc_expression (operand_type, r->elts[i], region,
					newivs, newivs_index, params_index);
      res = fold_build2 (op, type, res, t);
    }

  return res;
}

/* Converts a Cloog AST expression E back to a GCC expression tree of
   type TYPE.  */

static tree
clast_to_gcc_expression (tree type, struct clast_expr *e,
			 sese region, VEC (tree, heap) *newivs,
			 htab_t newivs_index, htab_t params_index)
{
  switch (e->type)
    {
    case expr_term:
      {
	struct clast_term *t = (struct clast_term *) e;

	if (t->var)
	  {
	    if (value_one_p (t->val))
	      {
		tree name = clast_name_to_gcc (t->var, region, newivs,
					       newivs_index, params_index);

		if (POINTER_TYPE_P (TREE_TYPE (name)) != POINTER_TYPE_P (type))
		  name = fold_convert (sizetype, name);

		name = fold_convert (type, name);
		return name;
	      }

	    else if (value_mone_p (t->val))
	      {
		tree name = clast_name_to_gcc (t->var, region, newivs,
					       newivs_index, params_index);

		if (POINTER_TYPE_P (TREE_TYPE (name)) != POINTER_TYPE_P (type))
		  name = fold_convert (sizetype, name);

		name = fold_convert (type, name);

		return fold_build1 (NEGATE_EXPR, type, name);
	      }
	    else
	      {
		tree name = clast_name_to_gcc (t->var, region, newivs,
					       newivs_index, params_index);
		tree cst = gmp_cst_to_tree (type, t->val);

		if (POINTER_TYPE_P (TREE_TYPE (name)) != POINTER_TYPE_P (type))
		  name = fold_convert (sizetype, name);

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

    case expr_red:
      {
        struct clast_reduction *r = (struct clast_reduction *) e;

        switch (r->type)
          {
	  case clast_red_sum:
	    return clast_to_gcc_expression_red
	      (type, POINTER_TYPE_P (type) ? POINTER_PLUS_EXPR : PLUS_EXPR,
	       r, region, newivs, newivs_index, params_index);

	  case clast_red_min:
	    return clast_to_gcc_expression_red (type, MIN_EXPR, r, region,
						newivs, newivs_index,
						params_index);

	  case clast_red_max:
	    return clast_to_gcc_expression_red (type, MAX_EXPR, r, region,
						newivs, newivs_index,
						params_index);

	  default:
	    gcc_unreachable ();
          }
        break;
      }

    case expr_bin:
      {
	struct clast_binary *b = (struct clast_binary *) e;
	struct clast_expr *lhs = (struct clast_expr *) b->LHS;
	tree tl = clast_to_gcc_expression (type, lhs, region, newivs,
					   newivs_index, params_index);
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

/* Returns the type for the expression E.  */

static tree
gcc_type_for_clast_expr (struct clast_expr *e,
			 sese region, VEC (tree, heap) *newivs,
			 htab_t newivs_index, htab_t params_index)
{
  switch (e->type)
    {
    case expr_term:
      {
	struct clast_term *t = (struct clast_term *) e;

	if (t->var)
	  return TREE_TYPE (clast_name_to_gcc (t->var, region, newivs,
					       newivs_index, params_index));
	else
	  return NULL_TREE;
      }

    case expr_red:
      {
        struct clast_reduction *r = (struct clast_reduction *) e;

	if (r->n == 1)
	  return gcc_type_for_clast_expr (r->elts[0], region, newivs,
					  newivs_index, params_index);
	else
	  {
	    int i;
	    for (i = 0; i < r->n; i++)
	      {
		tree type = gcc_type_for_clast_expr (r->elts[i], region,
						     newivs, newivs_index,
						     params_index);
		if (type)
		  return type;
	      }
	    return NULL_TREE;
	  }
      }

    case expr_bin:
      {
	struct clast_binary *b = (struct clast_binary *) e;
	struct clast_expr *lhs = (struct clast_expr *) b->LHS;
	return gcc_type_for_clast_expr (lhs, region, newivs,
					newivs_index, params_index);
      }

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Returns the type for the equation CLEQ.  */

static tree
gcc_type_for_clast_eq (struct clast_equation *cleq,
		       sese region, VEC (tree, heap) *newivs,
		       htab_t newivs_index, htab_t params_index)
{
  tree type = gcc_type_for_clast_expr (cleq->LHS, region, newivs,
				       newivs_index, params_index);
  if (type)
    return type;

  return gcc_type_for_clast_expr (cleq->RHS, region, newivs, newivs_index,
				  params_index);
}

/* Translates a clast equation CLEQ to a tree.  */

static tree
graphite_translate_clast_equation (sese region,
				   struct clast_equation *cleq,
				   VEC (tree, heap) *newivs,
				   htab_t newivs_index, htab_t params_index)
{
  enum tree_code comp;
  tree type = gcc_type_for_clast_eq (cleq, region, newivs, newivs_index,
				     params_index);
  tree lhs = clast_to_gcc_expression (type, cleq->LHS, region, newivs,
				      newivs_index, params_index);
  tree rhs = clast_to_gcc_expression (type, cleq->RHS, region, newivs,
				      newivs_index, params_index);

  if (cleq->sign == 0)
    comp = EQ_EXPR;

  else if (cleq->sign > 0)
    comp = GE_EXPR;

  else
    comp = LE_EXPR;

  return fold_build2 (comp, boolean_type_node, lhs, rhs);
}

/* Creates the test for the condition in STMT.  */

static tree
graphite_create_guard_cond_expr (sese region, struct clast_guard *stmt,
				 VEC (tree, heap) *newivs,
				 htab_t newivs_index, htab_t params_index)
{
  tree cond = NULL;
  int i;

  for (i = 0; i < stmt->n; i++)
    {
      tree eq = graphite_translate_clast_equation (region, &stmt->eq[i],
						   newivs, newivs_index,
						   params_index);

      if (cond)
	cond = fold_build2 (TRUTH_AND_EXPR, TREE_TYPE (eq), cond, eq);
      else
	cond = eq;
    }

  return cond;
}

/* Creates a new if region corresponding to Cloog's guard.  */

static edge
graphite_create_new_guard (sese region, edge entry_edge,
			   struct clast_guard *stmt,
			   VEC (tree, heap) *newivs,
			   htab_t newivs_index, htab_t params_index)
{
  tree cond_expr = graphite_create_guard_cond_expr (region, stmt, newivs,
						    newivs_index, params_index);
  edge exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);
  return exit_edge;
}

/* Walks a CLAST and returns the first statement in the body of a
   loop.  */

static struct clast_user_stmt *
clast_get_body_of_loop (struct clast_stmt *stmt)
{
  if (!stmt
      || CLAST_STMT_IS_A (stmt, stmt_user))
    return (struct clast_user_stmt *) stmt;

  if (CLAST_STMT_IS_A (stmt, stmt_for))
    return clast_get_body_of_loop (((struct clast_for *) stmt)->body);

  if (CLAST_STMT_IS_A (stmt, stmt_guard))
    return clast_get_body_of_loop (((struct clast_guard *) stmt)->then);

  if (CLAST_STMT_IS_A (stmt, stmt_block))
    return clast_get_body_of_loop (((struct clast_block *) stmt)->body);

  gcc_unreachable ();
}

/* Given a CLOOG_IV, return the type that CLOOG_IV should have in GCC
   land.  The selected type is big enough to include the original loop
   iteration variable, but signed to work with the subtractions CLooG
   may have introduced.  If such a type is not available, we fail.

   TODO: Do not always return long_long, but the smallest possible
   type, that still holds the original type.

   TODO: Get the types using CLooG instead.  This enables further
   optimizations, but needs CLooG support.  */

static tree
gcc_type_for_cloog_iv (const char *cloog_iv, gimple_bb_p gbb)
{
  struct ivtype_map_elt_s tmp;
  PTR *slot;

  tmp.cloog_iv = cloog_iv;
  slot = htab_find_slot (GBB_CLOOG_IV_TYPES (gbb), &tmp, NO_INSERT);

  if (slot && *slot)
    {
      tree type = ((ivtype_map_elt) *slot)->type;
      int type_precision = TYPE_PRECISION (type);

      /* Find the smallest signed type possible.  */
      if (!TYPE_UNSIGNED (type))
	{
	  if (type_precision <= TYPE_PRECISION (integer_type_node))
	    return integer_type_node;

	  if (type_precision <= TYPE_PRECISION (long_integer_type_node))
	    return long_integer_type_node;

	  if (type_precision <= TYPE_PRECISION (long_long_integer_type_node))
	    return long_long_integer_type_node;

	  gcc_unreachable ();
	}

      if (type_precision < TYPE_PRECISION (integer_type_node))
	return integer_type_node;

      if (type_precision < TYPE_PRECISION (long_integer_type_node))
	return long_integer_type_node;

      if (type_precision < TYPE_PRECISION (long_long_integer_type_node))
	return long_long_integer_type_node;

      /* There is no signed type available, that is large enough to hold the
	 original value.  */
      gcc_unreachable ();
    }

  return long_long_integer_type_node;
}

/* Returns the induction variable for the loop that gets translated to
   STMT.  */

static tree
gcc_type_for_iv_of_clast_loop (struct clast_for *stmt_for)
{
  struct clast_stmt *stmt = (struct clast_stmt *) stmt_for;
  struct clast_user_stmt *body = clast_get_body_of_loop (stmt);
  const char *cloog_iv = stmt_for->iterator;
  CloogStatement *cs = body->statement;
  poly_bb_p pbb = (poly_bb_p) cloog_statement_usr (cs);

  return gcc_type_for_cloog_iv (cloog_iv, PBB_BLACK_BOX (pbb));
}

/* Creates a new LOOP corresponding to Cloog's STMT.  Inserts an
   induction variable for the new LOOP.  New LOOP is attached to CFG
   starting at ENTRY_EDGE.  LOOP is inserted into the loop tree and
   becomes the child loop of the OUTER_LOOP.  NEWIVS_INDEX binds
   CLooG's scattering name to the induction variable created for the
   loop of STMT.  The new induction variable is inserted in the NEWIVS
   vector.  */

static struct loop *
graphite_create_new_loop (sese region, edge entry_edge,
			  struct clast_for *stmt,
			  loop_p outer, VEC (tree, heap) **newivs,
			  htab_t newivs_index, htab_t params_index)
{
  tree type = gcc_type_for_iv_of_clast_loop (stmt);
  tree lb = clast_to_gcc_expression (type, stmt->LB, region, *newivs,
				     newivs_index, params_index);
  tree ub = clast_to_gcc_expression (type, stmt->UB, region, *newivs,
				     newivs_index, params_index);
  tree stride = gmp_cst_to_tree (type, stmt->stride);
  tree ivvar = create_tmp_var (type, "graphite_IV");
  tree iv, iv_after_increment;
  loop_p loop = create_empty_loop_on_edge
    (entry_edge, lb, stride, ub, ivvar, &iv, &iv_after_increment,
     outer ? outer : entry_edge->src->loop_father);

  add_referenced_var (ivvar);

  save_clast_name_index (newivs_index, stmt->iterator,
			 VEC_length (tree, *newivs));
  VEC_safe_push (tree, heap, *newivs, iv);
  return loop;
}

/* Inserts in MAP a tuple (OLD_NAME, NEW_NAME) for the induction
   variables of the loops around GBB in SESE.  */

static void
build_iv_mapping (htab_t map, sese region,
		  VEC (tree, heap) *newivs, htab_t newivs_index,
		  struct clast_user_stmt *user_stmt,
		  htab_t params_index)
{
  struct clast_stmt *t;
  int index = 0;
  CloogStatement *cs = user_stmt->statement;
  poly_bb_p pbb = (poly_bb_p) cloog_statement_usr (cs);

  for (t = user_stmt->substitutions; t; t = t->next, index++)
    {
      struct clast_expr *expr = (struct clast_expr *)
       ((struct clast_assignment *)t)->RHS;
      tree type = gcc_type_for_clast_expr (expr, region, newivs,
					   newivs_index, params_index);
      tree old_name = pbb_to_depth_to_oldiv (pbb, index);
      tree e = clast_to_gcc_expression (type, expr, region, newivs,
					newivs_index, params_index);
      set_rename (map, old_name, e);
    }
}

/* Helper function for htab_traverse.  */

static int
copy_renames (void **slot, void *s)
{
  struct rename_map_elt_s *entry = (struct rename_map_elt_s *) *slot;
  htab_t res = (htab_t) s;
  tree old_name = entry->old_name;
  tree expr = entry->expr;
  struct rename_map_elt_s tmp;
  PTR *x;

  tmp.old_name = old_name;
  x = htab_find_slot (res, &tmp, INSERT);

  if (x && !*x)
    *x = new_rename_map_elt (old_name, expr);

  return 1;
}

/* Construct bb_pbb_def with BB and PBB. */

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
mark_bb_with_pbb (poly_bb_p pbb, basic_block bb, htab_t bb_pbb_mapping)
{
  bb_pbb_def tmp;
  PTR *x;

  tmp.bb = bb;
  x = htab_find_slot (bb_pbb_mapping, &tmp, INSERT);

  if (x && !*x)
    *x = new_bb_pbb_def (bb, pbb);
}

/* Find BB's related poly_bb_p in hash table BB_PBB_MAPPING.  */

static poly_bb_p
find_pbb_via_hash (htab_t bb_pbb_mapping, basic_block bb)
{
  bb_pbb_def tmp;
  PTR *slot;

  tmp.bb = bb;
  slot = htab_find_slot (bb_pbb_mapping, &tmp, NO_INSERT);

  if (slot && *slot)
    return ((bb_pbb_def *) *slot)->pbb;

  return NULL;
}

/* Check data dependency in LOOP at scattering level LEVEL.
   BB_PBB_MAPPING is a basic_block and it's related poly_bb_p
   mapping.  */

static bool
dependency_in_loop_p (loop_p loop, htab_t bb_pbb_mapping, int level)
{
  unsigned i,j;
  basic_block *bbs = get_loop_body_in_dom_order (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      poly_bb_p pbb1 = find_pbb_via_hash (bb_pbb_mapping, bbs[i]);

      if (pbb1 == NULL)
       continue;

      for (j = 0; j < loop->num_nodes; j++)
       {
	 poly_bb_p pbb2 = find_pbb_via_hash (bb_pbb_mapping, bbs[j]);

	 if (pbb2 == NULL)
	   continue;

	 if (dependency_between_pbbs_p (pbb1, pbb2, level))
	   {
	     free (bbs);
	     return true;
	   }
       }
    }

  free (bbs);

  return false;
}

static edge
translate_clast (sese, loop_p, struct clast_stmt *, edge, htab_t,
		 VEC (tree, heap) **, htab_t, htab_t, int, htab_t);

/* Translates a clast user statement STMT to gimple.

   - REGION is the sese region we used to generate the scop.
   - NEXT_E is the edge where new generated code should be attached.
   - CONTEXT_LOOP is the loop in which the generated code will be placed
   - RENAME_MAP contains a set of tuples of new names associated to
     the original variables names.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.
   - PARAMS_INDEX connects the cloog parameters with the gimple parameters in
     the sese region.  */
static edge
translate_clast_user (sese region, struct clast_user_stmt *stmt, edge next_e,
		      htab_t rename_map, VEC (tree, heap) **newivs,
		      htab_t newivs_index, htab_t bb_pbb_mapping,
		      htab_t params_index)
{
  gimple_bb_p gbb;
  basic_block new_bb;
  poly_bb_p pbb = (poly_bb_p) cloog_statement_usr (stmt->statement);
  gbb = PBB_BLACK_BOX (pbb);

  if (GBB_BB (gbb) == ENTRY_BLOCK_PTR)
    return next_e;

  build_iv_mapping (rename_map, region, *newivs, newivs_index, stmt,
		    params_index);
  next_e = copy_bb_and_scalar_dependences (GBB_BB (gbb), region,
					   next_e, rename_map);
  new_bb = next_e->src;
  mark_bb_with_pbb (pbb, new_bb, bb_pbb_mapping);
  update_ssa (TODO_update_ssa);

  return next_e;
}

static tree gcc_type_for_iv_of_clast_loop (struct clast_for *);


/* Creates a new if region protecting the loop to be executed, if the execution
   count is zero (lb > ub).  */
static edge
graphite_create_new_loop_guard (sese region, edge entry_edge,
				struct clast_for *stmt,
				VEC (tree, heap) *newivs,
				htab_t newivs_index, htab_t params_index)
{
  tree cond_expr;
  edge exit_edge;
  tree type = gcc_type_for_iv_of_clast_loop (stmt);
  tree lb = clast_to_gcc_expression (type, stmt->LB, region, newivs,
				     newivs_index, params_index);
  tree ub = clast_to_gcc_expression (type, stmt->UB, region, newivs,
				     newivs_index, params_index);

  /* XXX: Adding +1 and using LT_EXPR helps with loop latches that have a
     loop iteration count of "PARAMETER - 1".  For PARAMETER == 0 this becomes
     2^{32|64}, and the condition lb <= ub is true, even if we do not want this.
     However lb < ub + 1 is false, as expected.
     There might be a problem with cases where ub is 2^32.  */
  tree one;
  Value gmp_one;
  value_init (gmp_one);
  value_set_si (gmp_one, 1);
  one = gmp_cst_to_tree (type, gmp_one);
  value_clear (gmp_one);

  ub = fold_build2 (PLUS_EXPR, type, ub, one);
  cond_expr = fold_build2 (LT_EXPR, boolean_type_node, lb, ub);

  exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);

  return exit_edge;
}


/* Create the loop for a clast for statement.

   - REGION is the sese region we used to generate the scop.
   - NEXT_E is the edge where new generated code should be attached.
   - RENAME_MAP contains a set of tuples of new names associated to
     the original variables names.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.
   - PARAMS_INDEX connects the cloog parameters with the gimple parameters in
     the sese region.  */
static edge
translate_clast_for_loop (sese region, loop_p context_loop,
			  struct clast_for *stmt, edge next_e,
			  htab_t rename_map, VEC (tree, heap) **newivs,
			  htab_t newivs_index, htab_t bb_pbb_mapping,
			  int level, htab_t params_index)
{
  struct loop *loop = graphite_create_new_loop (region, next_e, stmt,
 						context_loop, newivs,
 						newivs_index, params_index);
  edge last_e = single_exit (loop);
  edge to_body = single_succ_edge (loop->header);
  basic_block after = to_body->dest;

  /* Create a basic block for loop close phi nodes.  */
  last_e = single_succ_edge (split_edge (last_e));

  /* Translate the body of the loop.  */
  next_e = translate_clast (region, loop, stmt->body, to_body, rename_map,
			    newivs, newivs_index, bb_pbb_mapping, level + 1,
			    params_index);
  redirect_edge_succ_nodup (next_e, after);
  set_immediate_dominator (CDI_DOMINATORS, next_e->dest, next_e->src);

   /* Remove from rename_map all the tuples containing variables
      defined in loop's body.  */
  insert_loop_close_phis (rename_map, loop);

  if (flag_loop_parallelize_all
      && !dependency_in_loop_p (loop, bb_pbb_mapping,
 				get_scattering_level (level)))
    loop->can_be_parallel = true;

  return last_e;
}

/* Translates a clast for statement STMT to gimple.  First a guard is created
   protecting the loop, if it is executed zero times.  In this guard we create
   the real loop structure.

   - REGION is the sese region we used to generate the scop.
   - NEXT_E is the edge where new generated code should be attached.
   - RENAME_MAP contains a set of tuples of new names associated to
     the original variables names.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.
   - PARAMS_INDEX connects the cloog parameters with the gimple parameters in
     the sese region.  */
static edge
translate_clast_for (sese region, loop_p context_loop, struct clast_for *stmt,
		     edge next_e, htab_t rename_map, VEC (tree, heap) **newivs,
		     htab_t newivs_index, htab_t bb_pbb_mapping, int level,
		     htab_t params_index)
{
  edge last_e = graphite_create_new_loop_guard (region, next_e, stmt, *newivs,
					   newivs_index, params_index);

  edge true_e = get_true_edge_from_guard_bb (next_e->dest);
  edge false_e = get_false_edge_from_guard_bb (next_e->dest);
  edge exit_true_e = single_succ_edge (true_e->dest);
  edge exit_false_e = single_succ_edge (false_e->dest);

  htab_t before_guard = htab_create (10, rename_map_elt_info,
				     eq_rename_map_elts, free);
  htab_traverse (rename_map, copy_renames, before_guard);

  next_e = translate_clast_for_loop (region, context_loop, stmt, true_e,
				     rename_map, newivs,
				     newivs_index, bb_pbb_mapping, level,
				     params_index);

  insert_guard_phis (last_e->src, exit_true_e, exit_false_e,
		     before_guard, rename_map);

  htab_delete (before_guard);

  return last_e;
}

/* Translates a clast guard statement STMT to gimple.

   - REGION is the sese region we used to generate the scop.
   - NEXT_E is the edge where new generated code should be attached.
   - CONTEXT_LOOP is the loop in which the generated code will be placed
   - RENAME_MAP contains a set of tuples of new names associated to
     the original variables names.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.
   - PARAMS_INDEX connects the cloog parameters with the gimple parameters in
     the sese region.  */
static edge
translate_clast_guard (sese region, loop_p context_loop,
		       struct clast_guard *stmt, edge next_e,
		       htab_t rename_map, VEC (tree, heap) **newivs,
		       htab_t newivs_index, htab_t bb_pbb_mapping, int level,
		       htab_t params_index)
{
  edge last_e = graphite_create_new_guard (region, next_e, stmt, *newivs,
					   newivs_index, params_index);

  edge true_e = get_true_edge_from_guard_bb (next_e->dest);
  edge false_e = get_false_edge_from_guard_bb (next_e->dest);
  edge exit_true_e = single_succ_edge (true_e->dest);
  edge exit_false_e = single_succ_edge (false_e->dest);

  htab_t before_guard = htab_create (10, rename_map_elt_info,
				     eq_rename_map_elts, free);
  htab_traverse (rename_map, copy_renames, before_guard);

  next_e = translate_clast (region, context_loop, stmt->then, true_e,
			    rename_map, newivs, newivs_index, bb_pbb_mapping,
			    level, params_index);

  insert_guard_phis (last_e->src, exit_true_e, exit_false_e,
		     before_guard, rename_map);

  htab_delete (before_guard);

  return last_e;
}

/* Translates a CLAST statement STMT to GCC representation in the
   context of a SESE.

   - NEXT_E is the edge where new generated code should be attached.
   - CONTEXT_LOOP is the loop in which the generated code will be placed
   - RENAME_MAP contains a set of tuples of new names associated to
     the original variables names.
   - BB_PBB_MAPPING is is a basic_block and it's related poly_bb_p mapping.  */
static edge
translate_clast (sese region, loop_p context_loop, struct clast_stmt *stmt,
		 edge next_e, htab_t rename_map, VEC (tree, heap) **newivs,
		 htab_t newivs_index, htab_t bb_pbb_mapping, int level,
		 htab_t params_index)
{
  if (!stmt)
    return next_e;

  if (CLAST_STMT_IS_A (stmt, stmt_root))
    ; /* Do nothing.  */

  else if (CLAST_STMT_IS_A (stmt, stmt_user))
    next_e = translate_clast_user (region, (struct clast_user_stmt *) stmt,
				   next_e, rename_map, newivs, newivs_index,
				   bb_pbb_mapping, params_index);

  else if (CLAST_STMT_IS_A (stmt, stmt_for))
    next_e = translate_clast_for (region, context_loop,
				  (struct clast_for *) stmt, next_e,
				  rename_map, newivs, newivs_index,
				  bb_pbb_mapping, level, params_index);

  else if (CLAST_STMT_IS_A (stmt, stmt_guard))
    next_e = translate_clast_guard (region, context_loop,
				    (struct clast_guard *) stmt, next_e,
				    rename_map, newivs, newivs_index,
				    bb_pbb_mapping, level, params_index);

  else if (CLAST_STMT_IS_A (stmt, stmt_block))
    next_e = translate_clast (region, context_loop,
			      ((struct clast_block *) stmt)->body,
			      next_e, rename_map, newivs, newivs_index,
			      bb_pbb_mapping, level, params_index);
  else
    gcc_unreachable();

  recompute_all_dominators ();
  graphite_verify ();

  return translate_clast (region, context_loop, stmt->next, next_e,
			  rename_map, newivs, newivs_index,
			  bb_pbb_mapping, level, params_index);
}

/* Returns the first cloog name used in EXPR.  */

static const char *
find_cloog_iv_in_expr (struct clast_expr *expr)
{
  struct clast_term *term = (struct clast_term *) expr;
  struct clast_reduction *red;
  int i;

  if (expr->type == expr_term)
    return term->var;

  if (expr->type != expr_red)
    return NULL;

  red = (struct clast_reduction *) expr;
  for (i = 0; i < red->n; i++)
    {
      const char *res = find_cloog_iv_in_expr (red->elts[i]);

      if (res)
	return res;
    }

  return NULL;
}

/* Build for USER_STMT a map between the CLAST induction variables and
   the corresponding GCC old induction variables.  This information is
   stored on each GRAPHITE_BB.  */

static void
compute_cloog_iv_types_1 (poly_bb_p pbb, struct clast_user_stmt *user_stmt)
{
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
  struct clast_stmt *t;
  int index = 0;

  for (t = user_stmt->substitutions; t; t = t->next, index++)
    {
      PTR *slot;
      struct ivtype_map_elt_s tmp;
      struct clast_expr *expr = (struct clast_expr *)
	((struct clast_assignment *)t)->RHS;

      /* Create an entry (clast_var, type).  */
      tmp.cloog_iv = find_cloog_iv_in_expr (expr);
      if (!tmp.cloog_iv)
	continue;

      slot = htab_find_slot (GBB_CLOOG_IV_TYPES (gbb), &tmp, INSERT);

      if (slot && !*slot)
	{
	  tree oldiv = pbb_to_depth_to_oldiv (pbb, index);
	  tree type = TREE_TYPE (oldiv);
	  *slot = new_ivtype_map_elt (tmp.cloog_iv, type);
	}
    }
}

/* Walk the CLAST tree starting from STMT and build for each
   clast_user_stmt a map between the CLAST induction variables and the
   corresponding GCC old induction variables.  This information is
   stored on each GRAPHITE_BB.  */

static void
compute_cloog_iv_types (struct clast_stmt *stmt)
{
  if (!stmt)
    return;

  if (CLAST_STMT_IS_A (stmt, stmt_root))
    goto next;

  if (CLAST_STMT_IS_A (stmt, stmt_user))
    {
      CloogStatement *cs = ((struct clast_user_stmt *) stmt)->statement;
      poly_bb_p pbb = (poly_bb_p) cloog_statement_usr (cs);
      gimple_bb_p gbb = PBB_BLACK_BOX (pbb);

      if (!GBB_CLOOG_IV_TYPES (gbb))
	GBB_CLOOG_IV_TYPES (gbb) = htab_create (10, ivtype_map_elt_info,
						eq_ivtype_map_elts, free);

      compute_cloog_iv_types_1 (pbb, (struct clast_user_stmt *) stmt);
      goto next;
    }

  if (CLAST_STMT_IS_A (stmt, stmt_for))
    {
      struct clast_stmt *s = ((struct clast_for *) stmt)->body;
      compute_cloog_iv_types (s);
      goto next;
    }

  if (CLAST_STMT_IS_A (stmt, stmt_guard))
    {
      struct clast_stmt *s = ((struct clast_guard *) stmt)->then;
      compute_cloog_iv_types (s);
      goto next;
    }

  if (CLAST_STMT_IS_A (stmt, stmt_block))
    {
      struct clast_stmt *s = ((struct clast_block *) stmt)->body;
      compute_cloog_iv_types (s);
      goto next;
    }

  gcc_unreachable ();

 next:
  compute_cloog_iv_types (stmt->next);
}

/* Free the SCATTERING domain list.  */

static void
free_scattering (CloogDomainList *scattering)
{
  while (scattering)
    {
      CloogDomain *dom = cloog_domain (scattering);
      CloogDomainList *next = cloog_next_domain (scattering);

      cloog_domain_free (dom);
      free (scattering);
      scattering = next;
    }
}

/* Initialize Cloog's parameter names from the names used in GIMPLE.
   Initialize Cloog's iterator names, using 'graphite_iterator_%d'
   from 0 to scop_nb_loops (scop).  */

static void
initialize_cloog_names (scop_p scop, CloogProgram *prog)
{
  sese region = SCOP_REGION (scop);
  int i;
  int nb_iterators = scop_max_loop_depth (scop);
  int nb_scattering = cloog_program_nb_scattdims (prog);
  int nb_parameters = VEC_length (tree, SESE_PARAMS (region));
  char **iterators = XNEWVEC (char *, nb_iterators * 2);
  char **scattering = XNEWVEC (char *, nb_scattering);
  char **parameters= XNEWVEC (char *, nb_parameters);

  cloog_program_set_names (prog, cloog_names_malloc ());

  for (i = 0; i < nb_parameters; i++)
    {
      tree param = VEC_index (tree, SESE_PARAMS(region), i);
      const char *name = get_name (param);
      int len;

      if (!name)
	name = "T";

      len = strlen (name);
      len += 17;
      parameters[i] = XNEWVEC (char, len + 1);
      snprintf (parameters[i], len, "%s_%d", name, SSA_NAME_VERSION (param));
    }

  cloog_names_set_nb_parameters (cloog_program_names (prog), nb_parameters);
  cloog_names_set_parameters (cloog_program_names (prog), parameters);

  for (i = 0; i < nb_iterators; i++)
    {
      int len = 4 + 16;
      iterators[i] = XNEWVEC (char, len);
      snprintf (iterators[i], len, "git_%d", i);
    }

  cloog_names_set_nb_iterators (cloog_program_names (prog),
				nb_iterators);
  cloog_names_set_iterators (cloog_program_names (prog),
			     iterators);

  for (i = 0; i < nb_scattering; i++)
    {
      int len = 5 + 16;
      scattering[i] = XNEWVEC (char, len);
      snprintf (scattering[i], len, "scat_%d", i);
    }

  cloog_names_set_nb_scattering (cloog_program_names (prog),
				 nb_scattering);
  cloog_names_set_scattering (cloog_program_names (prog),
			      scattering);
}

/* Build cloog program for SCoP.  */

static void
build_cloog_prog (scop_p scop, CloogProgram *prog)
{
  int i;
  int max_nb_loops = scop_max_loop_depth (scop);
  poly_bb_p pbb;
  CloogLoop *loop_list = NULL;
  CloogBlockList *block_list = NULL;
  CloogDomainList *scattering = NULL;
  int nbs = 2 * max_nb_loops + 1;
  int *scaldims;

  cloog_program_set_context
    (prog, new_Cloog_Domain_from_ppl_Pointset_Powerset (SCOP_CONTEXT (scop)));
  nbs = unify_scattering_dimensions (scop);
  scaldims = (int *) xmalloc (nbs * (sizeof (int)));
  cloog_program_set_nb_scattdims (prog, nbs);
  initialize_cloog_names (scop, prog);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    {
      CloogStatement *stmt;
      CloogBlock *block;

      /* Dead code elimination: when the domain of a PBB is empty,
	 don't generate code for the PBB.  */
      if (ppl_Pointset_Powerset_C_Polyhedron_is_empty (PBB_DOMAIN (pbb)))
	continue;

      /* Build the new statement and its block.  */
      stmt = cloog_statement_alloc (pbb_index (pbb));
      block = cloog_block_alloc (stmt, 0, NULL, pbb_dim_iter_domain (pbb));
      cloog_statement_set_usr (stmt, pbb);

      /* Build loop list.  */
      {
        CloogLoop *new_loop_list = cloog_loop_malloc ();
        cloog_loop_set_next (new_loop_list, loop_list);
        cloog_loop_set_domain
	  (new_loop_list,
	   new_Cloog_Domain_from_ppl_Pointset_Powerset (PBB_DOMAIN (pbb)));
        cloog_loop_set_block (new_loop_list, block);
        loop_list = new_loop_list;
      }

      /* Build block list.  */
      {
        CloogBlockList *new_block_list = cloog_block_list_malloc ();

        cloog_block_list_set_next (new_block_list, block_list);
        cloog_block_list_set_block (new_block_list, block);
        block_list = new_block_list;
      }

      /* Build scattering list.  */
      {
        /* XXX: Replace with cloog_domain_list_alloc(), when available.  */
        CloogDomainList *new_scattering
	  = (CloogDomainList *) xmalloc (sizeof (CloogDomainList));
        ppl_Polyhedron_t scat;
	CloogDomain *dom;

	scat = PBB_TRANSFORMED_SCATTERING (pbb);
	dom = new_Cloog_Domain_from_ppl_Polyhedron (scat);

        cloog_set_next_domain (new_scattering, scattering);
        cloog_set_domain (new_scattering, dom);
        scattering = new_scattering;
      }
    }

  cloog_program_set_loop (prog, loop_list);
  cloog_program_set_blocklist (prog, block_list);

  for (i = 0; i < nbs; i++)
    scaldims[i] = 0 ;

  cloog_program_set_scaldims (prog, scaldims);

  /* Extract scalar dimensions to simplify the code generation problem.  */
  cloog_program_extract_scalars (prog, scattering);

  /* Apply scattering.  */
  cloog_program_scatter (prog, scattering);
  free_scattering (scattering);

  /* Iterators corresponding to scalar dimensions have to be extracted.  */
  cloog_names_scalarize (cloog_program_names (prog), nbs,
			 cloog_program_scaldims (prog));

  /* Free blocklist.  */
  {
    CloogBlockList *next = cloog_program_blocklist (prog);

    while (next)
      {
        CloogBlockList *toDelete = next;
        next = cloog_block_list_next (next);
        cloog_block_list_set_next (toDelete, NULL);
        cloog_block_list_set_block (toDelete, NULL);
        cloog_block_list_free (toDelete);
      }
    cloog_program_set_blocklist (prog, NULL);
  }
}

/* Return the options that will be used in GLOOG.  */

static CloogOptions *
set_cloog_options (void)
{
  CloogOptions *options = cloog_options_malloc ();

  /* Change cloog output language to C.  If we do use FORTRAN instead, cloog
     will stop e.g. with "ERROR: unbounded loops not allowed in FORTRAN.", if
     we pass an incomplete program to cloog.  */
  options->language = LANGUAGE_C;

  /* Enable complex equality spreading: removes dummy statements
     (assignments) in the generated code which repeats the
     substitution equations for statements.  This is useless for
     GLooG.  */
  options->esp = 1;

  /* Enable C pretty-printing mode: normalizes the substitution
     equations for statements.  */
  options->cpp = 1;

  /* Allow cloog to build strides with a stride width different to one.
     This example has stride = 4:

     for (i = 0; i < 20; i += 4)
       A  */
  options->strides = 1;

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

  pprint (file, stmt, 0, options);
  cloog_options_free (options);
}

/* Prints STMT to STDERR.  */

void
debug_clast_stmt (struct clast_stmt *stmt)
{
  print_clast_stmt (stderr, stmt);
}

/* Translate SCOP to a CLooG program and clast.  These two
   representations should be freed together: a clast cannot be used
   without a program.  */

cloog_prog_clast
scop_to_clast (scop_p scop)
{
  CloogOptions *options = set_cloog_options ();
  cloog_prog_clast pc;

  /* Connect new cloog prog generation to graphite.  */
  pc.prog = cloog_program_malloc ();
  build_cloog_prog (scop, pc.prog);
  pc.prog = cloog_program_generate (pc.prog, options);
  pc.stmt = cloog_clast_create (pc.prog, options);

  cloog_options_free (options);
  return pc;
}

/* Prints to FILE the code generated by CLooG for SCOP.  */

void
print_generated_program (FILE *file, scop_p scop)
{
  CloogOptions *options = set_cloog_options ();
  cloog_prog_clast pc = scop_to_clast (scop);

  fprintf (file, "       (prog: \n");
  cloog_program_print (file, pc.prog);
  fprintf (file, "       )\n");

  fprintf (file, "       (clast: \n");
  pprint (file, pc.stmt, 0, options);
  fprintf (file, "       )\n");

  cloog_options_free (options);
  cloog_clast_free (pc.stmt);
  cloog_program_free (pc.prog);
}

/* Prints to STDERR the code generated by CLooG for SCOP.  */

void
debug_generated_program (scop_p scop)
{
  print_generated_program (stderr, scop);
}

/* Add CLooG names to parameter index.  The index is used to translate
   back from CLooG names to GCC trees.  */

static void
create_params_index (htab_t index_table, CloogProgram *prog) {
  CloogNames* names = cloog_program_names (prog);
  int nb_parameters = cloog_names_nb_parameters (names);
  char **parameters = cloog_names_parameters (names);
  int i;

  for (i = 0; i < nb_parameters; i++)
    save_clast_name_index (index_table, parameters[i], i);
}

/* GIMPLE Loop Generator: generates loops from STMT in GIMPLE form for
   the given SCOP.  Return true if code generation succeeded.
   BB_PBB_MAPPING is a basic_block and it's related poly_bb_p mapping.
*/

bool
gloog (scop_p scop, VEC (scop_p, heap) *scops, htab_t bb_pbb_mapping)
{
  VEC (tree, heap) *newivs = VEC_alloc (tree, heap, 10);
  loop_p context_loop;
  sese region = SCOP_REGION (scop);
  ifsese if_region = NULL;
  htab_t rename_map, newivs_index, params_index;
  cloog_prog_clast pc;
  int i;

  timevar_push (TV_GRAPHITE_CODE_GEN);
  gloog_error = false;

  pc = scop_to_clast (scop);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nCLAST generated by CLooG: \n");
      print_clast_stmt (dump_file, pc.stmt);
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
  compute_cloog_iv_types (pc.stmt);
  rename_map = htab_create (10, rename_map_elt_info, eq_rename_map_elts, free);
  newivs_index = htab_create (10, clast_name_index_elt_info,
			      eq_clast_name_indexes, free);
  params_index = htab_create (10, clast_name_index_elt_info,
			      eq_clast_name_indexes, free);

  create_params_index (params_index, pc.prog);

  translate_clast (region, context_loop, pc.stmt,
		   if_region->true_region->entry,
		   rename_map, &newivs, newivs_index,
		   bb_pbb_mapping, 1, params_index);
  graphite_verify ();
  sese_adjust_liveout_phis (region, rename_map,
			    if_region->region->exit->src,
			    if_region->false_region->exit,
			    if_region->true_region->exit);
  scev_reset_htab ();
  rename_nb_iterations (rename_map);

  for (i = 0; VEC_iterate (scop_p, scops, i, scop); i++)
    rename_sese_parameters (rename_map, SCOP_REGION (scop));

  recompute_all_dominators ();
  graphite_verify ();

  if (gloog_error)
    set_ifsese_condition (if_region, integer_zero_node);

  free (if_region->true_region);
  free (if_region->region);
  free (if_region);

  htab_delete (rename_map);
  htab_delete (newivs_index);
  htab_delete (params_index);
  VEC_free (tree, heap, newivs);
  cloog_clast_free (pc.stmt);
  cloog_program_free (pc.prog);
  timevar_pop (TV_GRAPHITE_CODE_GEN);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      loop_p loop;
      loop_iterator li;
      int num_no_dependency = 0;

      FOR_EACH_LOOP (li, loop, 0)
	if (loop->can_be_parallel)
	  num_no_dependency++;

      fprintf (dump_file, "\n%d loops carried no dependency.\n",
	       num_no_dependency);
    }

  return !gloog_error;
}

#endif
