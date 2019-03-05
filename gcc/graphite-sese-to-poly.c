/* Conversion of SESE regions to Polyhedra.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

#define USES_ISL

#include "config.h"

#ifdef HAVE_isl

#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "params.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "domwalk.h"
#include "tree-ssa-propagate.h"

#include <isl/constraint.h>
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/constraint.h>
#include <isl/aff.h>
#include <isl/val.h>

#include "graphite.h"

/* Return an isl identifier for the polyhedral basic block PBB.  */

static isl_id *
isl_id_for_pbb (scop_p s, poly_bb_p pbb)
{
  char name[14];
  snprintf (name, sizeof (name), "S_%d", pbb_index (pbb));
  return isl_id_alloc (s->isl_context, name, pbb);
}

static isl_pw_aff *extract_affine (scop_p, tree, __isl_take isl_space *space);

/* Extract an affine expression from the chain of recurrence E.  */

static isl_pw_aff *
extract_affine_chrec (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_pw_aff *lhs = extract_affine (s, CHREC_LEFT (e), isl_space_copy (space));
  isl_pw_aff *rhs = extract_affine (s, CHREC_RIGHT (e), isl_space_copy (space));
  isl_local_space *ls = isl_local_space_from_space (space);
  unsigned pos = sese_loop_depth (s->scop_info->region, get_chrec_loop (e)) - 1;
  isl_aff *loop = isl_aff_set_coefficient_si
    (isl_aff_zero_on_domain (ls), isl_dim_in, pos, 1);
  isl_pw_aff *l = isl_pw_aff_from_aff (loop);

  /* Before multiplying, make sure that the result is affine.  */
  gcc_assert (isl_pw_aff_is_cst (rhs)
	      || isl_pw_aff_is_cst (l));

  return isl_pw_aff_add (lhs, isl_pw_aff_mul (rhs, l));
}

/* Extract an affine expression from the mult_expr E.  */

static isl_pw_aff *
extract_affine_mul (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_pw_aff *lhs = extract_affine (s, TREE_OPERAND (e, 0),
				    isl_space_copy (space));
  isl_pw_aff *rhs = extract_affine (s, TREE_OPERAND (e, 1), space);

  if (!isl_pw_aff_is_cst (lhs)
      && !isl_pw_aff_is_cst (rhs))
    {
      isl_pw_aff_free (lhs);
      isl_pw_aff_free (rhs);
      return NULL;
    }

  return isl_pw_aff_mul (lhs, rhs);
}

/* Return an isl identifier from the name of the ssa_name E.  */

static isl_id *
isl_id_for_ssa_name (scop_p s, tree e)
{
  char name1[14];
  snprintf (name1, sizeof (name1), "P_%d", SSA_NAME_VERSION (e));
  return isl_id_alloc (s->isl_context, name1, e);
}

/* Return an isl identifier for the data reference DR.  Data references and
   scalar references get the same isl_id.  They need to be comparable and are
   distinguished through the first dimension, which contains the alias set or
   SSA_NAME_VERSION number.  */

static isl_id *
isl_id_for_dr (scop_p s)
{
  return isl_id_alloc (s->isl_context, "", 0);
}

/* Extract an affine expression from the ssa_name E.  */

static isl_pw_aff *
extract_affine_name (int dimension, __isl_take isl_space *space)
{
  isl_set *dom = isl_set_universe (isl_space_copy (space));
  isl_aff *aff = isl_aff_zero_on_domain (isl_local_space_from_space (space));
  aff = isl_aff_add_coefficient_si (aff, isl_dim_param, dimension, 1);
  return isl_pw_aff_alloc (dom, aff);
}

/* Convert WI to a isl_val with CTX.  */

static __isl_give isl_val *
isl_val_int_from_wi (isl_ctx *ctx, const widest_int &wi)
{
  if (wi::neg_p (wi, SIGNED))
    {
      widest_int mwi = -wi;
      return isl_val_neg (isl_val_int_from_chunks (ctx, mwi.get_len (),
						   sizeof (HOST_WIDE_INT),
						   mwi.get_val ()));
    }
  return isl_val_int_from_chunks (ctx, wi.get_len (), sizeof (HOST_WIDE_INT),
				  wi.get_val ());
}

/* Extract an affine expression from the gmp constant G.  */

static isl_pw_aff *
extract_affine_wi (const widest_int &g, __isl_take isl_space *space)
{
  isl_local_space *ls = isl_local_space_from_space (isl_space_copy (space));
  isl_aff *aff = isl_aff_zero_on_domain (ls);
  isl_set *dom = isl_set_universe (space);
  isl_ctx *ct = isl_aff_get_ctx (aff);
  isl_val *v = isl_val_int_from_wi (ct, g);
  aff = isl_aff_add_constant_val (aff, v);

  return isl_pw_aff_alloc (dom, aff);
}

/* Extract an affine expression from the integer_cst E.  */

static isl_pw_aff *
extract_affine_int (tree e, __isl_take isl_space *space)
{
  isl_pw_aff *res = extract_affine_wi (wi::to_widest (e), space);
  return res;
}

/* Compute pwaff mod 2^width.  */

static isl_pw_aff *
wrap (isl_pw_aff *pwaff, unsigned width)
{
  isl_val *mod;

  mod = isl_val_int_from_ui (isl_pw_aff_get_ctx (pwaff), width);
  mod = isl_val_2exp (mod);
  pwaff = isl_pw_aff_mod_val (pwaff, mod);

  return pwaff;
}

/* When parameter NAME is in REGION, returns its index in SESE_PARAMS.
   Otherwise returns -1.  */

static inline int
parameter_index_in_region (tree name, sese_info_p region)
{
  int i;
  tree p;
  FOR_EACH_VEC_ELT (region->params, i, p)
    if (p == name)
      return i;
  return -1;
}

/* Extract an affine expression from the tree E in the scop S.  */

static isl_pw_aff *
extract_affine (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_pw_aff *lhs, *rhs, *res;

  if (e == chrec_dont_know) {
    isl_space_free (space);
    return NULL;
  }

  tree type = TREE_TYPE (e);
  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      res = extract_affine_chrec (s, e, space);
      break;

    case MULT_EXPR:
      res = extract_affine_mul (s, e, space);
      break;

    case POINTER_PLUS_EXPR:
      {
	lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
	/* The RHS of a pointer-plus expression is to be interpreted
	   as signed value.  Try to look through a sign-changing conversion
	   first.  */
	tree tem = TREE_OPERAND (e, 1);
	STRIP_NOPS (tem);
	rhs = extract_affine (s, tem, space);
	if (TYPE_UNSIGNED (TREE_TYPE (tem)))
	  rhs = wrap (rhs, TYPE_PRECISION (type) - 1);
	res = isl_pw_aff_add (lhs, rhs);
	break;
      }

    case PLUS_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, TREE_OPERAND (e, 1), space);
      res = isl_pw_aff_add (lhs, rhs);
      break;

    case MINUS_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, TREE_OPERAND (e, 1), space);
      res = isl_pw_aff_sub (lhs, rhs);
      break;

    case BIT_NOT_EXPR:
      lhs = extract_affine (s, integer_minus_one_node, isl_space_copy (space));
      rhs = extract_affine (s, TREE_OPERAND (e, 0), space);
      res = isl_pw_aff_sub (lhs, rhs);
      /* We need to always wrap the result of a bitwise operation.  */
      return wrap (res, TYPE_PRECISION (type) - (TYPE_UNSIGNED (type) ? 0 : 1));

    case NEGATE_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, integer_minus_one_node, space);
      res = isl_pw_aff_mul (lhs, rhs);
      break;

    case SSA_NAME:
      {
	gcc_assert (! defined_in_sese_p (e, s->scop_info->region));
	int dim = parameter_index_in_region (e, s->scop_info);
	gcc_assert (dim != -1);
	/* No need to wrap a parameter.  */
	return extract_affine_name (dim, space);
      }

    case INTEGER_CST:
      res = extract_affine_int (e, space);
      /* No need to wrap a single integer.  */
      return res;

    CASE_CONVERT:
      {
	tree itype = TREE_TYPE (TREE_OPERAND (e, 0));
	res = extract_affine (s, TREE_OPERAND (e, 0), space);
	/* Signed values, even if overflow is undefined, get modulo-reduced.
	   But only if not all values of the old type fit in the new.  */
	if (! TYPE_UNSIGNED (type)
	    && ((TYPE_UNSIGNED (itype)
		 && TYPE_PRECISION (type) <= TYPE_PRECISION (itype))
		|| TYPE_PRECISION (type) < TYPE_PRECISION (itype)))
	  res = wrap (res, TYPE_PRECISION (type) - 1);
	else if (TYPE_UNSIGNED (type)
		 && (!TYPE_UNSIGNED (itype)
		     || TYPE_PRECISION (type) < TYPE_PRECISION (itype)))
	  res = wrap (res, TYPE_PRECISION (type));
	return res;
      }

    case NON_LVALUE_EXPR:
      res = extract_affine (s, TREE_OPERAND (e, 0), space);
      break;

    default:
      gcc_unreachable ();
      break;
    }

  /* For all wrapping arithmetic wrap the result.  */
  if (TYPE_OVERFLOW_WRAPS (type))
    res = wrap (res, TYPE_PRECISION (type));

  return res;
}

/* Returns a linear expression for tree T evaluated in PBB.  */

static isl_pw_aff *
create_pw_aff_from_tree (poly_bb_p pbb, loop_p loop, tree t)
{
  scop_p scop = PBB_SCOP (pbb);

  t = cached_scalar_evolution_in_region (scop->scop_info->region, loop, t);

  gcc_assert (!chrec_contains_undetermined (t));
  gcc_assert (!automatically_generated_chrec_p (t));

  return extract_affine (scop, t, isl_set_get_space (pbb->domain));
}

/* Add conditional statement STMT to pbb.  CODE is used as the comparison
   operator.  This allows us to invert the condition or to handle
   inequalities.  */

static void
add_condition_to_pbb (poly_bb_p pbb, gcond *stmt, enum tree_code code)
{
  loop_p loop = gimple_bb (stmt)->loop_father;
  isl_pw_aff *lhs = create_pw_aff_from_tree (pbb, loop, gimple_cond_lhs (stmt));
  isl_pw_aff *rhs = create_pw_aff_from_tree (pbb, loop, gimple_cond_rhs (stmt));

  isl_set *cond;
  switch (code)
    {
      case LT_EXPR:
	cond = isl_pw_aff_lt_set (lhs, rhs);
	break;

      case GT_EXPR:
	cond = isl_pw_aff_gt_set (lhs, rhs);
	break;

      case LE_EXPR:
	cond = isl_pw_aff_le_set (lhs, rhs);
	break;

      case GE_EXPR:
	cond = isl_pw_aff_ge_set (lhs, rhs);
	break;

      case EQ_EXPR:
	cond = isl_pw_aff_eq_set (lhs, rhs);
	break;

      case NE_EXPR:
	cond = isl_pw_aff_ne_set (lhs, rhs);
	break;

      default:
	gcc_unreachable ();
    }

  cond = isl_set_coalesce (cond);
  cond = isl_set_set_tuple_id (cond, isl_set_get_tuple_id (pbb->domain));
  pbb->domain = isl_set_coalesce (isl_set_intersect (pbb->domain, cond));
}

/* Add conditions to the domain of PBB.  */

static void
add_conditions_to_domain (poly_bb_p pbb)
{
  unsigned int i;
  gimple *stmt;
  gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (GBB_CONDITIONS (gbb).is_empty ())
    return;

  FOR_EACH_VEC_ELT (GBB_CONDITIONS (gbb), i, stmt)
    switch (gimple_code (stmt))
      {
      case GIMPLE_COND:
	  {
            /* Don't constrain on anything else than INTEGER_TYPE.  */
	    if (TREE_CODE (TREE_TYPE (gimple_cond_lhs (stmt))) != INTEGER_TYPE)
              break;

	    gcond *cond_stmt = as_a <gcond *> (stmt);
	    enum tree_code code = gimple_cond_code (cond_stmt);

	    /* The conditions for ELSE-branches are inverted.  */
	    if (!GBB_CONDITION_CASES (gbb)[i])
	      code = invert_tree_comparison (code, false);

	    add_condition_to_pbb (pbb, cond_stmt, code);
	    break;
	  }

      default:
	gcc_unreachable ();
	break;
      }
}

/* Add constraints on the possible values of parameter P from the type
   of P.  */

static void
add_param_constraints (scop_p scop, graphite_dim_t p, tree parameter)
{
  tree type = TREE_TYPE (parameter);
  wide_int min, max;

  gcc_assert (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type));

  if (INTEGRAL_TYPE_P (type)
      && get_range_info (parameter, &min, &max) == VR_RANGE)
    ;
  else
    {
      min = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
    }

  isl_space *space = isl_set_get_space (scop->param_context);
  isl_constraint *c = isl_inequality_alloc (isl_local_space_from_space (space));
  isl_val *v = isl_val_int_from_wi (scop->isl_context,
				    widest_int::from (min, TYPE_SIGN (type)));
  v = isl_val_neg (v);
  c = isl_constraint_set_constant_val (c, v);
  c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, 1);
  scop->param_context = isl_set_coalesce
      (isl_set_add_constraint (scop->param_context, c));

  space = isl_set_get_space (scop->param_context);
  c = isl_inequality_alloc (isl_local_space_from_space (space));
  v = isl_val_int_from_wi (scop->isl_context,
			   widest_int::from (max, TYPE_SIGN (type)));
  c = isl_constraint_set_constant_val (c, v);
  c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, -1);
  scop->param_context = isl_set_coalesce
      (isl_set_add_constraint (scop->param_context, c));
}

/* Add a constrain to the ACCESSES polyhedron for the alias set of
   data reference DR.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static isl_map *
pdr_add_alias_set (isl_map *acc, dr_info &dri)
{
  isl_constraint *c = isl_equality_alloc
      (isl_local_space_from_space (isl_map_get_space (acc)));
  /* Positive numbers for all alias sets.  */
  c = isl_constraint_set_constant_si (c, -dri.alias_set);
  c = isl_constraint_set_coefficient_si (c, isl_dim_out, 0, 1);

  return isl_map_add_constraint (acc, c);
}

/* Assign the affine expression INDEX to the output dimension POS of
   MAP and return the result.  */

static isl_map *
set_index (isl_map *map, int pos, isl_pw_aff *index)
{
  isl_map *index_map;
  int len = isl_map_dim (map, isl_dim_out);
  isl_id *id;

  index_map = isl_map_from_pw_aff (index);
  index_map = isl_map_insert_dims (index_map, isl_dim_out, 0, pos);
  index_map = isl_map_add_dims (index_map, isl_dim_out, len - pos - 1);

  id = isl_map_get_tuple_id (map, isl_dim_out);
  index_map = isl_map_set_tuple_id (index_map, isl_dim_out, id);
  id = isl_map_get_tuple_id (map, isl_dim_in);
  index_map = isl_map_set_tuple_id (index_map, isl_dim_in, id);

  return isl_map_intersect (map, index_map);
}

/* Add to ACCESSES polyhedron equalities defining the access functions
   to the memory.  ACCESSP_NB_DIMS is the dimension of the ACCESSES
   polyhedron, DOM_NB_DIMS is the dimension of the iteration domain.
   PBB is the poly_bb_p that contains the data reference DR.  */

static isl_map *
pdr_add_memory_accesses (isl_map *acc, dr_info &dri)
{
  data_reference_p dr = dri.dr;
  poly_bb_p pbb = dri.pbb;
  int i, nb_subscripts = DR_NUM_DIMENSIONS (dr);
  scop_p scop = PBB_SCOP (pbb);

  for (i = 0; i < nb_subscripts; i++)
    {
      isl_pw_aff *aff;
      tree afn = DR_ACCESS_FN (dr, i);

      aff = extract_affine (scop, afn,
			    isl_space_domain (isl_map_get_space (acc)));
      acc = set_index (acc, nb_subscripts - i , aff);
    }

  return isl_map_coalesce (acc);
}

/* Return true when the LOW and HIGH bounds of an array reference REF are valid
   to extract constraints on accessed elements of the array.  Returning false is
   the conservative answer.  */

static bool
bounds_are_valid (tree ref, tree low, tree high)
{
  if (!high)
    return false;

  if (!tree_fits_shwi_p (low)
      || !tree_fits_shwi_p (high))
    return false;

  /* 1-element arrays at end of structures may extend over
     their declared size.  */
  if (array_at_struct_end_p (ref)
      && operand_equal_p (low, high, 0))
    return false;

  /* Fortran has some arrays where high bound is -1 and low is 0.  */
  if (integer_onep (fold_build2 (LT_EXPR, boolean_type_node, high, low)))
    return false;

  return true;
}

/* Add constrains representing the size of the accessed data to the
   ACCESSES polyhedron.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static isl_set *
pdr_add_data_dimensions (isl_set *subscript_sizes, scop_p scop,
			 data_reference_p dr)
{
  tree ref = DR_REF (dr);

  int nb_subscripts = DR_NUM_DIMENSIONS (dr);
  for (int i = nb_subscripts - 1; i >= 0; i--, ref = TREE_OPERAND (ref, 0))
    {
      if (TREE_CODE (ref) != ARRAY_REF)
	return subscript_sizes;

      tree low = array_ref_low_bound (ref);
      tree high = array_ref_up_bound (ref);

      if (!bounds_are_valid (ref, low, high))
	continue;

      isl_space *space = isl_set_get_space (subscript_sizes);
      isl_pw_aff *lb = extract_affine_int (low, isl_space_copy (space));
      isl_pw_aff *ub = extract_affine_int (high, isl_space_copy (space));

      /* high >= 0 */
      isl_set *valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (ub));
      valid = isl_set_project_out (valid, isl_dim_set, 0,
				   isl_set_dim (valid, isl_dim_set));
      scop->param_context = isl_set_coalesce
	(isl_set_intersect (scop->param_context, valid));

      isl_aff *aff
	= isl_aff_zero_on_domain (isl_local_space_from_space (space));
      aff = isl_aff_add_coefficient_si (aff, isl_dim_in, i + 1, 1);
      isl_set *univ
	= isl_set_universe (isl_space_domain (isl_aff_get_space (aff)));
      isl_pw_aff *index = isl_pw_aff_alloc (univ, aff);

      isl_id *id = isl_set_get_tuple_id (subscript_sizes);
      lb = isl_pw_aff_set_tuple_id (lb, isl_dim_in, isl_id_copy (id));
      ub = isl_pw_aff_set_tuple_id (ub, isl_dim_in, id);

      /* low <= sub_i <= high */
      isl_set *lbs = isl_pw_aff_ge_set (isl_pw_aff_copy (index), lb);
      isl_set *ubs = isl_pw_aff_le_set (index, ub);
      subscript_sizes = isl_set_intersect (subscript_sizes, lbs);
      subscript_sizes = isl_set_intersect (subscript_sizes, ubs);
    }

  return isl_set_coalesce (subscript_sizes);
}

/* Build data accesses for DRI.  */

static void
build_poly_dr (dr_info &dri)
{
  isl_map *acc;
  isl_set *subscript_sizes;
  poly_bb_p pbb = dri.pbb;
  data_reference_p dr = dri.dr;
  scop_p scop = PBB_SCOP (pbb);
  isl_id *id = isl_id_for_dr (scop);

  {
    isl_space *dc = isl_set_get_space (pbb->domain);
    int nb_out = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_add_dims (isl_space_from_domain (dc),
					   isl_dim_out, nb_out);

    acc = isl_map_universe (space);
    acc = isl_map_set_tuple_id (acc, isl_dim_out, isl_id_copy (id));
  }

  acc = pdr_add_alias_set (acc, dri);
  acc = pdr_add_memory_accesses (acc, dri);

  {
    int nb = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_set_alloc (scop->isl_context, 0, nb);

    space = isl_space_set_tuple_id (space, isl_dim_set, id);
    subscript_sizes = isl_set_nat_universe (space);
    subscript_sizes = isl_set_fix_si (subscript_sizes, isl_dim_set, 0,
				      dri.alias_set);
    subscript_sizes = pdr_add_data_dimensions (subscript_sizes, scop, dr);
  }

  new_poly_dr (pbb, DR_STMT (dr), DR_IS_READ (dr) ? PDR_READ : PDR_WRITE,
	       acc, subscript_sizes);
}

static void
build_poly_sr_1 (poly_bb_p pbb, gimple *stmt, tree var, enum poly_dr_type kind,
		 isl_map *acc, isl_set *subscript_sizes)
{
  scop_p scop = PBB_SCOP (pbb);
  /* Each scalar variables has a unique alias set number starting from
     the maximum alias set assigned to a dr.  */
  int alias_set = scop->max_alias_set + SSA_NAME_VERSION (var);
  subscript_sizes = isl_set_fix_si (subscript_sizes, isl_dim_set, 0,
				    alias_set);

  /* Add a constrain to the ACCESSES polyhedron for the alias set of
     data reference DR.  */
  isl_constraint *c
    = isl_equality_alloc (isl_local_space_from_space (isl_map_get_space (acc)));
  c = isl_constraint_set_constant_si (c, -alias_set);
  c = isl_constraint_set_coefficient_si (c, isl_dim_out, 0, 1);

  new_poly_dr (pbb, stmt, kind, isl_map_add_constraint (acc, c),
	       subscript_sizes);
}

/* Record all cross basic block scalar variables in PBB.  */

static void
build_poly_sr (poly_bb_p pbb)
{
  scop_p scop = PBB_SCOP (pbb);
  gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);
  vec<scalar_use> &reads = gbb->read_scalar_refs;
  vec<tree> &writes = gbb->write_scalar_refs;

  isl_space *dc = isl_set_get_space (pbb->domain);
  int nb_out = 1;
  isl_space *space = isl_space_add_dims (isl_space_from_domain (dc),
					 isl_dim_out, nb_out);
  isl_id *id = isl_id_for_dr (scop);
  space = isl_space_set_tuple_id (space, isl_dim_set, isl_id_copy (id));
  isl_map *acc = isl_map_universe (isl_space_copy (space));
  acc = isl_map_set_tuple_id (acc, isl_dim_out, id);
  isl_set *subscript_sizes = isl_set_nat_universe (space);

  int i;
  tree var;
  FOR_EACH_VEC_ELT (writes, i, var)
    build_poly_sr_1 (pbb, SSA_NAME_DEF_STMT (var), var, PDR_WRITE,
		     isl_map_copy (acc), isl_set_copy (subscript_sizes));

  scalar_use *use;
  FOR_EACH_VEC_ELT (reads, i, use)
    build_poly_sr_1 (pbb, use->first, use->second, PDR_READ, isl_map_copy (acc),
		     isl_set_copy (subscript_sizes));

  isl_map_free (acc);
  isl_set_free (subscript_sizes);
}

/* Build data references in SCOP.  */

static void
build_scop_drs (scop_p scop)
{
  int i;
  dr_info *dri;
  FOR_EACH_VEC_ELT (scop->drs, i, dri)
    build_poly_dr (*dri);

  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    build_poly_sr (pbb);
}

/* Add to the iteration DOMAIN one extra dimension for LOOP->num.  */

static isl_set *
add_iter_domain_dimension (__isl_take isl_set *domain, loop_p loop, scop_p scop)
{
  int loop_index = isl_set_dim (domain, isl_dim_set);
  domain = isl_set_add_dims (domain, isl_dim_set, 1);
  char name[50];
  snprintf (name, sizeof(name), "i%d", loop->num);
  isl_id *label = isl_id_alloc (scop->isl_context, name, NULL);
  return isl_set_set_dim_id (domain, isl_dim_set, loop_index, label);
}

/* Add constraints to DOMAIN for each loop from LOOP up to CONTEXT.  */

static isl_set *
add_loop_constraints (scop_p scop, __isl_take isl_set *domain, loop_p loop,
		      loop_p context)
{
  if (loop == context)
    return domain;
  const sese_l &region = scop->scop_info->region;
  if (!loop_in_sese_p (loop, region))
    return domain;

  /* Recursion all the way up to the context loop.  */
  domain = add_loop_constraints (scop, domain, loop_outer (loop), context);

  /* Then, build constraints over the loop in post-order: outer to inner.  */

  int loop_index = isl_set_dim (domain, isl_dim_set);
  if (dump_file)
    fprintf (dump_file, "[sese-to-poly] adding one extra dimension to the "
	     "domain for loop_%d.\n", loop->num);
  domain = add_iter_domain_dimension (domain, loop, scop);
  isl_space *space = isl_set_get_space (domain);

  /* 0 <= loop_i */
  isl_local_space *ls = isl_local_space_from_space (isl_space_copy (space));
  isl_constraint *c = isl_inequality_alloc (ls);
  c = isl_constraint_set_coefficient_si (c, isl_dim_set, loop_index, 1);
  if (dump_file)
    {
      fprintf (dump_file, "[sese-to-poly] adding constraint to the domain: ");
      print_isl_constraint (dump_file, c);
    }
  domain = isl_set_add_constraint (domain, c);

  tree nb_iters = number_of_latch_executions (loop);
  if (TREE_CODE (nb_iters) == INTEGER_CST)
    {
      /* loop_i <= cst_nb_iters */
      isl_local_space *ls = isl_local_space_from_space (space);
      isl_constraint *c = isl_inequality_alloc (ls);
      c = isl_constraint_set_coefficient_si (c, isl_dim_set, loop_index, -1);
      isl_val *v
	= isl_val_int_from_wi (scop->isl_context, wi::to_widest (nb_iters));
      c = isl_constraint_set_constant_val (c, v);
      return isl_set_add_constraint (domain, c);
    }
  /* loop_i <= expr_nb_iters */
  gcc_assert (!chrec_contains_undetermined (nb_iters));
  nb_iters = cached_scalar_evolution_in_region (region, loop, nb_iters);
  gcc_assert (!chrec_contains_undetermined (nb_iters));

  isl_pw_aff *aff_nb_iters = extract_affine (scop, nb_iters,
					     isl_space_copy (space));
  isl_set *valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (aff_nb_iters));
  valid = isl_set_project_out (valid, isl_dim_set, 0,
			       isl_set_dim (valid, isl_dim_set));

  if (valid)
    scop->param_context = isl_set_intersect (scop->param_context, valid);

  ls = isl_local_space_from_space (isl_space_copy (space));
  isl_aff *loop_i = isl_aff_set_coefficient_si (isl_aff_zero_on_domain (ls),
						isl_dim_in, loop_index, 1);
  isl_set *le = isl_pw_aff_le_set (isl_pw_aff_from_aff (loop_i),
				   isl_pw_aff_copy (aff_nb_iters));
  if (dump_file)
    {
      fprintf (dump_file, "[sese-to-poly] adding constraint to the domain: ");
      print_isl_set (dump_file, le);
    }
  domain = isl_set_intersect (domain, le);

  widest_int nit;
  if (!max_stmt_executions (loop, &nit))
    {
      isl_pw_aff_free (aff_nb_iters);
      isl_space_free (space);
      return domain;
    }

  /* NIT is an upper bound to NB_ITERS: "NIT >= NB_ITERS", although we
     do not know whether the loop executes at least once.  */
  --nit;

  isl_pw_aff *approx = extract_affine_wi (nit, isl_space_copy (space));
  isl_set *x = isl_pw_aff_ge_set (approx, aff_nb_iters);
  x = isl_set_project_out (x, isl_dim_set, 0,
			   isl_set_dim (x, isl_dim_set));
  scop->param_context = isl_set_intersect (scop->param_context, x);

  ls = isl_local_space_from_space (space);
  c = isl_inequality_alloc (ls);
  c = isl_constraint_set_coefficient_si (c, isl_dim_set, loop_index, -1);
  isl_val *v = isl_val_int_from_wi (scop->isl_context, nit);
  c = isl_constraint_set_constant_val (c, v);

  if (dump_file)
    {
      fprintf (dump_file, "[sese-to-poly] adding constraint to the domain: ");
      print_isl_constraint (dump_file, c);
    }

  return isl_set_add_constraint (domain, c);
}

/* Builds the original iteration domains for each pbb in the SCOP.  */

static int
build_iteration_domains (scop_p scop, __isl_keep isl_set *context,
			 int index, loop_p context_loop)
{
  loop_p current = pbb_loop (scop->pbbs[index]);
  isl_set *domain = isl_set_copy (context);
  domain = add_loop_constraints (scop, domain, current, context_loop);
  const sese_l &region = scop->scop_info->region;

  int i;
  poly_bb_p pbb;
  FOR_EACH_VEC_ELT_FROM (scop->pbbs, i, pbb, index)
    {
      loop_p loop = pbb_loop (pbb);
      if (current == loop)
	{
	  pbb->iterators = isl_set_copy (domain);
	  pbb->domain = isl_set_copy (domain);
	  pbb->domain = isl_set_set_tuple_id (pbb->domain,
					      isl_id_for_pbb (scop, pbb));
	  add_conditions_to_domain (pbb);

	  if (dump_file)
	    {
	      fprintf (dump_file, "[sese-to-poly] set pbb_%d->domain: ",
		       pbb_index (pbb));
	      print_isl_set (dump_file, domain);
	    }
	  continue;
	}

      while (loop_in_sese_p (loop, region)
	     && current != loop)
	loop = loop_outer (loop);

      if (current != loop)
	{
	  /* A statement in a different loop nest than CURRENT loop.  */
	  isl_set_free (domain);
	  return i;
	}

      /* A statement nested in the CURRENT loop.  */
      i = build_iteration_domains (scop, domain, i, current);
      i--;
    }

  isl_set_free (domain);
  return i;
}

/* Assign dimension for each parameter in SCOP and add constraints for the
   parameters.  */

static void
build_scop_context (scop_p scop)
{
  sese_info_p region = scop->scop_info;
  unsigned nbp = sese_nb_params (region);
  isl_space *space = isl_space_set_alloc (scop->isl_context, nbp, 0);

  unsigned i;
  tree e;
  FOR_EACH_VEC_ELT (region->params, i, e)
    space = isl_space_set_dim_id (space, isl_dim_param, i,
                                  isl_id_for_ssa_name (scop, e));

  scop->param_context = isl_set_universe (space);

  FOR_EACH_VEC_ELT (region->params, i, e)
    add_param_constraints (scop, i, e);
}

/* Return true when loop A is nested in loop B.  */

static bool
nested_in (loop_p a, loop_p b)
{
  return b == find_common_loop (a, b);
}

/* Return the loop at a specific SCOP->pbbs[*INDEX].  */
static loop_p
loop_at (scop_p scop, int *index)
{
  return pbb_loop (scop->pbbs[*index]);
}

/* Return the index of any pbb belonging to loop or a subloop of A.  */

static int
index_outermost_in_loop (loop_p a, scop_p scop)
{
  int i, outermost = -1;
  int last_depth = -1;
  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    if (nested_in (pbb_loop (pbb), a)
	&& (last_depth == -1
	    || last_depth > (int) loop_depth (pbb_loop (pbb))))
      {
	outermost = i;
	last_depth = loop_depth (pbb_loop (pbb));
      }
  return outermost;
}

/* Return the index of any pbb belonging to loop or a subloop of A.  */

static int
index_pbb_in_loop (loop_p a, scop_p scop)
{
  int i;
  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    if (pbb_loop (pbb) == a)
      return i;
  return -1;
}

static poly_bb_p
outermost_pbb_in (loop_p loop, scop_p scop)
{
  int x = index_pbb_in_loop (loop, scop);
  if (x == -1)
    x = index_outermost_in_loop (loop, scop);
  return scop->pbbs[x];
}

static isl_schedule *
add_in_sequence (__isl_take isl_schedule *a, __isl_take isl_schedule *b)
{
  gcc_assert (a || b);

  if (!a)
    return b;

  if (!b)
    return a;

  return isl_schedule_sequence (a, b);
}

struct map_to_dimension_data {
  int n;
  isl_union_pw_multi_aff *res;
};

/* Create a function that maps the elements of SET to its N-th dimension and add
   it to USER->res.  */

static isl_stat
add_outer_projection (__isl_take isl_set *set, void *user)
{
  struct map_to_dimension_data *data = (struct map_to_dimension_data *) user;
  int dim = isl_set_dim (set, isl_dim_set);
  isl_space *space = isl_set_get_space (set);

  gcc_assert (dim >= data->n);
  isl_pw_multi_aff *pma
    = isl_pw_multi_aff_project_out_map (space, isl_dim_set, data->n,
					dim - data->n);
  data->res = isl_union_pw_multi_aff_add_pw_multi_aff (data->res, pma);

  isl_set_free (set);
  return isl_stat_ok;
}

/* Return SET in which all inner dimensions above N are removed.  */

static isl_multi_union_pw_aff *
outer_projection_mupa (__isl_take isl_union_set *set, int n)
{
  gcc_assert (n >= 0);
  gcc_assert (set);
  gcc_assert (!isl_union_set_is_empty (set));

  isl_space *space = isl_union_set_get_space (set);
  isl_union_pw_multi_aff *pwaff = isl_union_pw_multi_aff_empty (space);

  struct map_to_dimension_data data = {n, pwaff};

  if (isl_union_set_foreach_set (set, &add_outer_projection, &data) < 0)
    data.res = isl_union_pw_multi_aff_free (data.res);

  isl_union_set_free (set);
  return isl_multi_union_pw_aff_from_union_pw_multi_aff (data.res);
}

/* Embed SCHEDULE in the constraints of the LOOP domain.  */

static isl_schedule *
add_loop_schedule (__isl_take isl_schedule *schedule, loop_p loop,
		   scop_p scop)
{
  poly_bb_p pbb = outermost_pbb_in (loop, scop);
  isl_set *iterators = pbb->iterators;

  int empty = isl_set_is_empty (iterators);
  if (empty < 0 || empty)
    return empty < 0 ? isl_schedule_free (schedule) : schedule;

  isl_union_set *domain = isl_schedule_get_domain (schedule);
  /* We cannot apply an empty domain to pbbs in this loop so return early.  */
  if (isl_union_set_is_empty (domain))
    {
      isl_union_set_free (domain);
      return schedule;
    }

  isl_space *space = isl_set_get_space (iterators);
  int loop_index = isl_space_dim (space, isl_dim_set) - 1;

  loop_p ploop = pbb_loop (pbb);
  while (loop != ploop)
    {
      --loop_index;
      ploop = loop_outer (ploop);
    }

  isl_local_space *ls = isl_local_space_from_space (space);
  isl_aff *aff = isl_aff_var_on_domain (ls, isl_dim_set, loop_index);
  isl_multi_aff *prefix = isl_multi_aff_from_aff (aff);
  char name[50];
  snprintf (name, sizeof(name), "L_%d", loop->num);
  isl_id *label = isl_id_alloc (isl_schedule_get_ctx (schedule),
				name, NULL);
  prefix = isl_multi_aff_set_tuple_id (prefix, isl_dim_out, label);

  int n = isl_multi_aff_dim (prefix, isl_dim_in);
  isl_multi_union_pw_aff *mupa = outer_projection_mupa (domain, n);
  mupa = isl_multi_union_pw_aff_apply_multi_aff (mupa, prefix);
  return isl_schedule_insert_partial_schedule (schedule, mupa);
}

/* Build schedule for the pbb at INDEX.  */

static isl_schedule *
build_schedule_pbb (scop_p scop, int *index)
{
  poly_bb_p pbb = scop->pbbs[*index];
  ++*index;
  isl_set *domain = isl_set_copy (pbb->domain);
  isl_union_set *ud = isl_union_set_from_set (domain);
  return isl_schedule_from_domain (ud);
}

static isl_schedule *build_schedule_loop_nest (scop_p, int *, loop_p);

/* Build the schedule of the loop containing the SCOP pbb at INDEX.  */

static isl_schedule *
build_schedule_loop (scop_p scop, int *index)
{
  int max = scop->pbbs.length ();
  gcc_assert (*index < max);
  loop_p loop = loop_at (scop, index);

  isl_schedule *s = NULL;
  while (nested_in (loop_at (scop, index), loop))
    {
      if (loop == loop_at (scop, index))
	s = add_in_sequence (s, build_schedule_pbb (scop, index));
      else
	s = add_in_sequence (s, build_schedule_loop_nest (scop, index, loop));

      if (*index == max)
	break;
    }

  return add_loop_schedule (s, loop, scop);
}

/* S is the schedule of the loop LOOP.  Embed the schedule S in all outer loops.
   When CONTEXT_LOOP is null, embed the schedule in all loops contained in the
   SCOP surrounding LOOP.  When CONTEXT_LOOP is non null, only embed S in the
   maximal loop nest contained within CONTEXT_LOOP.  */

static isl_schedule *
embed_in_surrounding_loops (__isl_take isl_schedule *s, scop_p scop,
			    loop_p loop, int *index, loop_p context_loop)
{
  loop_p outer = loop_outer (loop);
  sese_l region = scop->scop_info->region;
  if (context_loop == outer
      || !loop_in_sese_p (outer, region))
    return s;

  int max = scop->pbbs.length ();
  if (*index == max
      || (context_loop && !nested_in (loop_at (scop, index), context_loop))
      || (!context_loop
	  && !loop_in_sese_p (find_common_loop (outer, loop_at (scop, index)),
			      region)))
    return embed_in_surrounding_loops (add_loop_schedule (s, outer, scop),
				       scop, outer, index, context_loop);

  bool a_pbb;
  while ((a_pbb = (outer == loop_at (scop, index)))
	 || nested_in (loop_at (scop, index), outer))
    {
      if (a_pbb)
	s = add_in_sequence (s, build_schedule_pbb (scop, index));
      else
	s = add_in_sequence (s, build_schedule_loop (scop, index));

      if (*index == max)
	break;
    }

  /* We reached the end of the OUTER loop: embed S in OUTER.  */
  return embed_in_surrounding_loops (add_loop_schedule (s, outer, scop), scop,
				     outer, index, context_loop);
}

/* Build schedule for the full loop nest containing the pbb at INDEX.  When
   CONTEXT_LOOP is null, build the schedule of all loops contained in the SCOP
   surrounding the pbb.  When CONTEXT_LOOP is non null, only build the maximal loop
   nest contained within CONTEXT_LOOP.  */

static isl_schedule *
build_schedule_loop_nest (scop_p scop, int *index, loop_p context_loop)
{
  gcc_assert (*index != (int) scop->pbbs.length ());

  loop_p loop = loop_at (scop, index);
  isl_schedule *s = build_schedule_loop (scop, index);
  return embed_in_surrounding_loops (s, scop, loop, index, context_loop);
}

/* Build the schedule of the SCOP.  */

static void
build_original_schedule (scop_p scop)
{
  int i = 0;
  int n = scop->pbbs.length ();
  while (i < n)
    {
      poly_bb_p pbb = scop->pbbs[i];
      isl_schedule *s = NULL;
      if (!loop_in_sese_p (pbb_loop (pbb), scop->scop_info->region))
	s = build_schedule_pbb (scop, &i);
      else
	s = build_schedule_loop_nest (scop, &i, NULL);

      scop->original_schedule = add_in_sequence (scop->original_schedule, s);
    }

  if (dump_file)
    {
      fprintf (dump_file, "[sese-to-poly] original schedule:\n");
      print_isl_schedule (dump_file, scop->original_schedule);
    }
}

/* Builds the polyhedral representation for a SESE region.  */

bool
build_poly_scop (scop_p scop)
{
  int old_err = isl_options_get_on_error (scop->isl_context);
  isl_options_set_on_error (scop->isl_context, ISL_ON_ERROR_CONTINUE);

  build_scop_context (scop);

  unsigned i = 0;
  unsigned n = scop->pbbs.length ();
  while (i < n)
    i = build_iteration_domains (scop, scop->param_context, i, NULL);

  build_scop_drs (scop);
  build_original_schedule (scop);

  enum isl_error err = isl_ctx_last_error (scop->isl_context);
  isl_ctx_reset_error (scop->isl_context);
  isl_options_set_on_error (scop->isl_context, old_err);
  if (err != isl_error_none
      && dump_enabled_p ())
    dump_printf (MSG_MISSED_OPTIMIZATION,
		 "ISL error while building poly scop\n");

  return err == isl_error_none;
}
#endif  /* HAVE_isl */
