/* Conversion of SESE regions to Polyhedra.
   Copyright (C) 2009-2015 Free Software Foundation, Inc.
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

#ifdef HAVE_isl
/* Workaround for GMP 5.1.3 bug, see PR56019.  */
#include <stddef.h>

#include <isl/constraint.h>
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/constraint.h>
#include <isl/aff.h>
#include <isl/val.h>

/* Since ISL-0.13, the extern is in val_gmp.h.  */
#if !defined(HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE) && defined(__cplusplus)
extern "C" {
#endif
#include <isl/val_gmp.h>
#if !defined(HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE) && defined(__cplusplus)
}
#endif

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
#include "graphite-poly.h"
#include "tree-ssa-propagate.h"
#include "graphite-sese-to-poly.h"

/* Assigns to RES the value of the INTEGER_CST T.  */

static inline void
tree_int_to_gmp (tree t, mpz_t res)
{
  wi::to_mpz (t, res, TYPE_SIGN (TREE_TYPE (t)));
}

/* Returns the index of the PHI argument defined in the outermost
   loop.  */

static size_t
phi_arg_in_outermost_loop (gphi *phi)
{
  loop_p loop = gimple_bb (phi)->loop_father;
  size_t i, res = 0;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (!flow_bb_inside_loop_p (loop, gimple_phi_arg_edge (phi, i)->src))
      {
	loop = gimple_phi_arg_edge (phi, i)->src->loop_father;
	res = i;
      }

  return res;
}

/* Removes a simple copy phi node "RES = phi (INIT, RES)" at position
   PSI by inserting on the loop ENTRY edge assignment "RES = INIT".  */

static void
remove_simple_copy_phi (gphi_iterator *psi)
{
  gphi *phi = psi->phi ();
  tree res = gimple_phi_result (phi);
  size_t entry = phi_arg_in_outermost_loop (phi);
  tree init = gimple_phi_arg_def (phi, entry);
  gassign *stmt = gimple_build_assign (res, init);
  edge e = gimple_phi_arg_edge (phi, entry);

  remove_phi_node (psi, false);
  gsi_insert_on_edge_immediate (e, stmt);
}

/* Removes an invariant phi node at position PSI by inserting on the
   loop ENTRY edge the assignment RES = INIT.  */

static void
remove_invariant_phi (sese_l &region, gphi_iterator *psi)
{
  gphi *phi = psi->phi ();
  loop_p loop = loop_containing_stmt (phi);
  tree res = gimple_phi_result (phi);
  tree scev = scalar_evolution_in_region (region, loop, res);
  size_t entry = phi_arg_in_outermost_loop (phi);
  edge e = gimple_phi_arg_edge (phi, entry);
  tree var;
  gassign *stmt;
  gimple_seq stmts = NULL;

  if (tree_contains_chrecs (scev, NULL))
    scev = gimple_phi_arg_def (phi, entry);

  var = force_gimple_operand (scev, &stmts, true, NULL_TREE);
  stmt = gimple_build_assign (res, var);
  remove_phi_node (psi, false);

  gimple_seq_add_stmt (&stmts, stmt);
  gsi_insert_seq_on_edge (e, stmts);
  gsi_commit_edge_inserts ();
  SSA_NAME_DEF_STMT (res) = stmt;
}

/* Returns true when the phi node at PSI is of the form "a = phi (a, x)".  */

static inline bool
simple_copy_phi_p (gphi *phi)
{
  if (gimple_phi_num_args (phi) != 2)
    return false;

  tree res = gimple_phi_result (phi);
  return (res == gimple_phi_arg_def (phi, 0)
	  || res == gimple_phi_arg_def (phi, 1));
}

/* Returns true when the phi node at position PSI is a reduction phi
   node in REGION.  Otherwise moves the pointer PSI to the next phi to
   be considered.  */

static bool
reduction_phi_p (sese_l &region, gphi_iterator *psi)
{
  loop_p loop;
  gphi *phi = psi->phi ();
  tree res = gimple_phi_result (phi);

  loop = loop_containing_stmt (phi);

  if (simple_copy_phi_p (phi))
    {
      /* PRE introduces phi nodes like these, for an example,
	 see id-5.f in the fortran graphite testsuite:

	 # prephitmp.85_265 = PHI <prephitmp.85_258(33), prephitmp.85_265(18)>
      */
      remove_simple_copy_phi (psi);
      return false;
    }

  if (scev_analyzable_p (res, region))
    {
      tree scev = scalar_evolution_in_region (region, loop, res);

      if (evolution_function_is_invariant_p (scev, loop->num))
	remove_invariant_phi (region, psi);
      else
	gsi_next (psi);

      return false;
    }

  /* All the other cases are considered reductions.  */
  return true;
}

/* Return an ISL identifier for the polyhedral basic block PBB.  */

static isl_id *
isl_id_for_pbb (scop_p s, poly_bb_p pbb)
{
  char name[10];
  snprintf (name, sizeof (name), "S_%d", pbb_index (pbb));
  return isl_id_alloc (s->isl_context, name, pbb);
}

/* Converts the STATIC_SCHEDULE of PBB into a scattering polyhedron.
   We generate SCATTERING_DIMENSIONS scattering dimensions.

   The scattering polyhedron consists of these dimensions: scattering,
   loop_iterators, parameters.

   Example:

   | scattering_dimensions = 5
   | nb_iterators = 1
   | scop_nb_params = 2
   |
   | Schedule:
   |   i
   | 4 5
   |
   | Scattering polyhedron:
   |
   | scattering: {s1, s2, s3, s4, s5}
   | loop_iterators: {i}
   | parameters: {p1, p2}
   |
   | s1  s2  s3  s4  s5  i   p1  p2  1
   | 1   0   0   0   0   0   0   0  -4  = 0
   | 0   1   0   0   0  -1   0   0   0  = 0
   | 0   0   1   0   0   0   0   0  -5  = 0  */

static void
build_pbb_scattering_polyhedrons (isl_aff *static_sched,
				  poly_bb_p pbb)
{
  isl_val *val;

  int scattering_dimensions = isl_set_dim (pbb->domain, isl_dim_set) * 2 + 1;

  isl_space *dc = isl_set_get_space (pbb->domain);
  isl_space *dm = isl_space_add_dims (isl_space_from_domain (dc),
				      isl_dim_out, scattering_dimensions);
  pbb->schedule = isl_map_universe (dm);

  for (int i = 0; i < scattering_dimensions; i++)
    {
      /* Textual order inside this loop.  */
      if ((i % 2) == 0)
	{
	  isl_constraint *c = isl_equality_alloc
	      (isl_local_space_from_space (isl_map_get_space (pbb->schedule)));

	  val = isl_aff_get_coefficient_val (static_sched, isl_dim_in, i / 2);
	  gcc_assert (val && isl_val_is_int (val));

	  val = isl_val_neg (val);
	  c = isl_constraint_set_constant_val (c, val);
	  c = isl_constraint_set_coefficient_si (c, isl_dim_out, i, 1);
	  pbb->schedule = isl_map_add_constraint (pbb->schedule, c);
	}

      /* Iterations of this loop.  */
      else /* if ((i % 2) == 1) */
	{
	  int loop = (i - 1) / 2;
	  pbb->schedule = isl_map_equate (pbb->schedule, isl_dim_in, loop,
					  isl_dim_out, i);
	}
    }

  pbb->transformed = isl_map_copy (pbb->schedule);
}

/* Build for BB the static schedule.

   The static schedule is a Dewey numbering of the abstract syntax
   tree: http://en.wikipedia.org/wiki/Dewey_Decimal_Classification

   The following example informally defines the static schedule:

   A
   for (i: ...)
     {
       for (j: ...)
         {
           B
           C
         }

       for (k: ...)
         {
           D
           E
         }
     }
   F

   Static schedules for A to F:

     DEPTH
     0 1 2
   A 0
   B 1 0 0
   C 1 0 1
   D 1 1 0
   E 1 1 1
   F 2
*/

static void
build_scop_scattering (scop_p scop)
{
  gimple_poly_bb_p previous_gbb = NULL;
  isl_space *dc = isl_set_get_space (scop->param_context);
  isl_aff *static_sched;

  dc = isl_space_add_dims (dc, isl_dim_set, number_of_loops (cfun));
  static_sched = isl_aff_zero_on_domain (isl_local_space_from_space (dc));

  /* We have to start schedules at 0 on the first component and
     because we cannot compare_prefix_loops against a previous loop,
     prefix will be equal to zero, and that index will be
     incremented before copying.  */
  static_sched = isl_aff_add_coefficient_si (static_sched, isl_dim_in, 0, -1);

  int i;
  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    {
      gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);
      int prefix = 0;

      if (previous_gbb)
	prefix = nb_common_loops (scop->scop_info->region, previous_gbb, gbb);

      previous_gbb = gbb;

      static_sched = isl_aff_add_coefficient_si (static_sched, isl_dim_in,
						 prefix, 1);
      build_pbb_scattering_polyhedrons (static_sched, pbb);
    }

  isl_aff_free (static_sched);
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

/* Return an ISL identifier from the name of the ssa_name E.  */

static isl_id *
isl_id_for_ssa_name (scop_p s, tree e)
{
  const char *name = get_name (e);
  isl_id *id;

  if (name)
    id = isl_id_alloc (s->isl_context, name, e);
  else
    {
      char name1[10];
      snprintf (name1, sizeof (name1), "P_%d", SSA_NAME_VERSION (e));
      id = isl_id_alloc (s->isl_context, name1, e);
    }

  return id;
}

/* Return an ISL identifier for the data reference DR.  */

static isl_id *
isl_id_for_dr (scop_p s, data_reference_p dr ATTRIBUTE_UNUSED)
{
  /* Data references all get the same isl_id.  They need to be comparable
     and are distinguished through the first dimension, which contains the
     alias set number.  */
  return isl_id_alloc (s->isl_context, "", 0);
}

/* Extract an affine expression from the ssa_name E.  */

static isl_pw_aff *
extract_affine_name (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_id *id = isl_id_for_ssa_name (s, e);
  int dimension = isl_space_find_dim_by_id (space, isl_dim_param, id);
  isl_id_free (id);
  isl_set *dom = isl_set_universe (isl_space_copy (space));
  isl_aff *aff = isl_aff_zero_on_domain (isl_local_space_from_space (space));
  aff = isl_aff_add_coefficient_si (aff, isl_dim_param, dimension, 1);
  return isl_pw_aff_alloc (dom, aff);
}

/* Extract an affine expression from the gmp constant G.  */

static isl_pw_aff *
extract_affine_gmp (mpz_t g, __isl_take isl_space *space)
{
  isl_local_space *ls = isl_local_space_from_space (isl_space_copy (space));
  isl_aff *aff = isl_aff_zero_on_domain (ls);
  isl_set *dom = isl_set_universe (space);
  isl_ctx *ct = isl_aff_get_ctx (aff);
  isl_val *v = isl_val_int_from_gmp (ct, g);
  aff = isl_aff_add_constant_val (aff, v);

  return isl_pw_aff_alloc (dom, aff);
}

/* Extract an affine expression from the integer_cst E.  */

static isl_pw_aff *
extract_affine_int (tree e, __isl_take isl_space *space)
{
  mpz_t g;

  mpz_init (g);
  tree_int_to_gmp (e, g);
  isl_pw_aff *res = extract_affine_gmp (g, space);
  mpz_clear (g);

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
parameter_index_in_region_1 (tree name, sese_info_p region)
{
  int i;
  tree p;

  gcc_assert (TREE_CODE (name) == SSA_NAME);

  FOR_EACH_VEC_ELT (SESE_PARAMS (region), i, p)
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

  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      res = extract_affine_chrec (s, e, space);
      break;

    case MULT_EXPR:
      res = extract_affine_mul (s, e, space);
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, TREE_OPERAND (e, 1), space);
      res = isl_pw_aff_add (lhs, rhs);
      break;

    case MINUS_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, TREE_OPERAND (e, 1), space);
      res = isl_pw_aff_sub (lhs, rhs);
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, integer_minus_one_node, space);
      res = isl_pw_aff_mul (lhs, rhs);
      break;

    case SSA_NAME:
      gcc_assert (-1 != parameter_index_in_region_1 (e, s->scop_info)
		  || !invariant_in_sese_p_rec (e, s->scop_info->region, NULL));
      res = extract_affine_name (s, e, space);
      break;

    case INTEGER_CST:
      res = extract_affine_int (e, space);
      /* No need to wrap a single integer.  */
      return res;

    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      res = extract_affine (s, TREE_OPERAND (e, 0), space);
      break;

    default:
      gcc_unreachable ();
      break;
    }

  tree type = TREE_TYPE (e);
  if (TYPE_UNSIGNED (type))
    res = wrap (res, TYPE_PRECISION (type));

  return res;
}

/* Assign dimension for each parameter in SCOP.  */

static void
set_scop_parameter_dim (scop_p scop)
{
  sese_info_p region = scop->scop_info;
  unsigned nbp = sese_nb_params (region);
  isl_space *space = isl_space_set_alloc (scop->isl_context, nbp, 0);

  unsigned i;
  tree e;
  FOR_EACH_VEC_ELT (SESE_PARAMS (region), i, e)
    space = isl_space_set_dim_id (space, isl_dim_param, i,
                                  isl_id_for_ssa_name (scop, e));

  scop->param_context = isl_set_universe (space);
}

/* Builds the constraint polyhedra for LOOP in SCOP.  OUTER_PH gives
   the constraints for the surrounding loops.  */

static void
build_loop_iteration_domains (scop_p scop, struct loop *loop,
                              int nb,
			      isl_set *outer, isl_set **doms)
{

  tree nb_iters = number_of_latch_executions (loop);
  sese_l region = scop->scop_info->region;
  gcc_assert (loop_in_sese_p (loop, region));

  isl_set *inner = isl_set_copy (outer);
  int pos = isl_set_dim (outer, isl_dim_set);
  isl_val *v;
  mpz_t g;

  mpz_init (g);

  inner = isl_set_add_dims (inner, isl_dim_set, 1);
  isl_space *space = isl_set_get_space (inner);

  /* 0 <= loop_i */
  isl_constraint *c = isl_inequality_alloc
      (isl_local_space_from_space (isl_space_copy (space)));
  c = isl_constraint_set_coefficient_si (c, isl_dim_set, pos, 1);
  inner = isl_set_add_constraint (inner, c);

  /* loop_i <= cst_nb_iters */
  if (TREE_CODE (nb_iters) == INTEGER_CST)
    {
      c = isl_inequality_alloc
	  (isl_local_space_from_space (isl_space_copy (space)));
      c = isl_constraint_set_coefficient_si (c, isl_dim_set, pos, -1);
      tree_int_to_gmp (nb_iters, g);
      v = isl_val_int_from_gmp (scop->isl_context, g);
      c = isl_constraint_set_constant_val (c, v);
      inner = isl_set_add_constraint (inner, c);
    }

  /* loop_i <= expr_nb_iters */
  else if (!chrec_contains_undetermined (nb_iters))
    {
      isl_pw_aff *aff;

      nb_iters = scalar_evolution_in_region (region, loop, nb_iters);

      aff = extract_affine (scop, nb_iters, isl_set_get_space (inner));
      isl_set *valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (aff));
      valid = isl_set_project_out (valid, isl_dim_set, 0,
				   isl_set_dim (valid, isl_dim_set));
      scop->param_context = isl_set_intersect (scop->param_context, valid);

      isl_local_space *ls = isl_local_space_from_space (isl_space_copy (space));
      isl_aff *al = isl_aff_set_coefficient_si (isl_aff_zero_on_domain (ls),
						isl_dim_in, pos, 1);
      isl_set *le = isl_pw_aff_le_set (isl_pw_aff_from_aff (al),
				       isl_pw_aff_copy (aff));
      inner = isl_set_intersect (inner, le);

      widest_int nit;
      if (max_stmt_executions (loop, &nit))
	{
	  /* Insert in the context the constraints from the
	     estimation of the number of iterations NIT and the
	     symbolic number of iterations (involving parameter
	     names) NB_ITERS.  First, build the affine expression
	     "NIT - NB_ITERS" and then say that it is positive,
	     i.e., NIT approximates NB_ITERS: "NIT >= NB_ITERS".  */
	  mpz_t g;
	  mpz_init (g);
	  wi::to_mpz (nit, g, SIGNED);
	  mpz_sub_ui (g, g, 1);

	  isl_pw_aff *approx
	    = extract_affine_gmp (g, isl_set_get_space (inner));
	  isl_set *x = isl_pw_aff_ge_set (approx, aff);
	  x = isl_set_project_out (x, isl_dim_set, 0,
				   isl_set_dim (x, isl_dim_set));
	  scop->param_context = isl_set_intersect (scop->param_context, x);

	  isl_constraint *c = isl_inequality_alloc
	      (isl_local_space_from_space (isl_space_copy (space)));
	  c = isl_constraint_set_coefficient_si (c, isl_dim_set, pos, -1);
	  v = isl_val_int_from_gmp (scop->isl_context, g);
	  mpz_clear (g);
	  c = isl_constraint_set_constant_val (c, v);
	  inner = isl_set_add_constraint (inner, c);
	}
      else
	isl_pw_aff_free (aff);
    }
  else
    gcc_unreachable ();

  if (loop->inner)
    build_loop_iteration_domains (scop, loop->inner, nb + 1,
				  isl_set_copy (inner), doms);

  if (nb != 0
      && loop->next
      && loop_in_sese_p (loop->next, region))
    build_loop_iteration_domains (scop, loop->next, nb,
				  isl_set_copy (outer), doms);

  doms[loop->num] = inner;

  isl_set_free (outer);
  isl_space_free (space);
  mpz_clear (g);
}

/* Returns a linear expression for tree T evaluated in PBB.  */

static isl_pw_aff *
create_pw_aff_from_tree (poly_bb_p pbb, tree t)
{
  scop_p scop = PBB_SCOP (pbb);

  t = scalar_evolution_in_region (scop->scop_info->region, pbb_loop (pbb), t);
  gcc_assert (!automatically_generated_chrec_p (t));

  return extract_affine (scop, t, isl_set_get_space (pbb->domain));
}

/* Add conditional statement STMT to pbb.  CODE is used as the comparison
   operator.  This allows us to invert the condition or to handle
   inequalities.  */

static void
add_condition_to_pbb (poly_bb_p pbb, gcond *stmt, enum tree_code code)
{
  isl_pw_aff *lhs = create_pw_aff_from_tree (pbb, gimple_cond_lhs (stmt));
  isl_pw_aff *rhs = create_pw_aff_from_tree (pbb, gimple_cond_rhs (stmt));
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
	isl_pw_aff_free (lhs);
	isl_pw_aff_free (rhs);
	return;
    }

  cond = isl_set_coalesce (cond);
  cond = isl_set_set_tuple_id (cond, isl_set_get_tuple_id (pbb->domain));
  pbb->domain = isl_set_intersect (pbb->domain, cond);
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

      case GIMPLE_SWITCH:
	/* Switch statements are not supported right now - fall through.  */

      default:
	gcc_unreachable ();
	break;
      }
}

/* Traverses all the GBBs of the SCOP and add their constraints to the
   iteration domains.  */

static void
add_conditions_to_constraints (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    add_conditions_to_domain (pbb);
}

/* Add constraints on the possible values of parameter P from the type
   of P.  */

static void
add_param_constraints (scop_p scop, graphite_dim_t p)
{
  tree parameter = SESE_PARAMS (scop->scop_info)[p];
  tree type = TREE_TYPE (parameter);
  tree lb = NULL_TREE;
  tree ub = NULL_TREE;

  if (POINTER_TYPE_P (type) || !TYPE_MIN_VALUE (type))
    lb = lower_bound_in_type (type, type);
  else
    lb = TYPE_MIN_VALUE (type);

  if (POINTER_TYPE_P (type) || !TYPE_MAX_VALUE (type))
    ub = upper_bound_in_type (type, type);
  else
    ub = TYPE_MAX_VALUE (type);

  if (lb)
    {
      isl_space *space = isl_set_get_space (scop->param_context);
      isl_constraint *c;
      mpz_t g;
      isl_val *v;

      c = isl_inequality_alloc (isl_local_space_from_space (space));
      mpz_init (g);
      tree_int_to_gmp (lb, g);
      v = isl_val_int_from_gmp (scop->isl_context, g);
      v = isl_val_neg (v);
      mpz_clear (g);
      c = isl_constraint_set_constant_val (c, v);
      c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, 1);

      scop->param_context = isl_set_add_constraint (scop->param_context, c);
    }

  if (ub)
    {
      isl_space *space = isl_set_get_space (scop->param_context);
      isl_constraint *c;
      mpz_t g;
      isl_val *v;

      c = isl_inequality_alloc (isl_local_space_from_space (space));

      mpz_init (g);
      tree_int_to_gmp (ub, g);
      v = isl_val_int_from_gmp (scop->isl_context, g);
      mpz_clear (g);
      c = isl_constraint_set_constant_val (c, v);
      c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, -1);

      scop->param_context = isl_set_add_constraint (scop->param_context, c);
    }
}

/* Build the context of the SCOP.  The context usually contains extra
   constraints that are added to the iteration domains that constrain
   some parameters.  */

static void
build_scop_context (scop_p scop)
{
  graphite_dim_t p, n = scop_nb_params (scop);

  for (p = 0; p < n; p++)
    add_param_constraints (scop, p);
}

/* Build the iteration domains: the loops belonging to the current
   SCOP, and that vary for the execution of the current basic block.
   Returns false if there is no loop in SCOP.  */

static void
build_scop_iteration_domain (scop_p scop)
{
  sese_info_p region = scop->scop_info;
  int nb_loops = number_of_loops (cfun);
  isl_set **doms = XCNEWVEC (isl_set *, nb_loops);

  int i;
  struct loop *loop;
  FOR_EACH_VEC_ELT (SESE_LOOP_NEST (region), i, loop)
    if (!loop_in_sese_p (loop_outer (loop), region->region))
      build_loop_iteration_domains (scop, loop, 0,
				    isl_set_copy (scop->param_context), doms);

  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    {
      loop = pbb_loop (pbb);

      if (doms[loop->num])
	pbb->domain = isl_set_copy (doms[loop->num]);
      else
	pbb->domain = isl_set_copy (scop->param_context);

      pbb->domain = isl_set_set_tuple_id (pbb->domain,
					  isl_id_for_pbb (scop, pbb));
    }

  for (int i = 0; i < nb_loops; i++)
    if (doms[i])
      isl_set_free (doms[i]);

  free (doms);
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
      tree afn = DR_ACCESS_FN (dr, nb_subscripts - 1 - i);

      aff = extract_affine (scop, afn,
			    isl_space_domain (isl_map_get_space (acc)));
      acc = set_index (acc, i + 1, aff);
    }

  return acc;
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

      /* XXX The PPL code dealt separately with
         subscript - low >= 0 and high - subscript >= 0 in case one of
	 the two bounds isn't known.  Do the same here?  */

      if (tree_fits_shwi_p (low)
	  && high
	  && tree_fits_shwi_p (high)
	  /* 1-element arrays at end of structures may extend over
	     their declared size.  */
	  && !(array_at_struct_end_p (ref)
	       && operand_equal_p (low, high, 0)))
	{
	  isl_id *id;
	  isl_aff *aff;
	  isl_set *univ, *lbs, *ubs;
	  isl_pw_aff *index;
	  isl_set *valid;
	  isl_space *space = isl_set_get_space (subscript_sizes);
	  isl_pw_aff *lb = extract_affine_int (low, isl_space_copy (space));
	  isl_pw_aff *ub = extract_affine_int (high, isl_space_copy (space));

	  /* high >= 0 */
	  valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (ub));
	  valid = isl_set_project_out (valid, isl_dim_set, 0,
				       isl_set_dim (valid, isl_dim_set));
	  scop->param_context = isl_set_intersect (scop->param_context, valid);

	  aff = isl_aff_zero_on_domain (isl_local_space_from_space (space));
	  aff = isl_aff_add_coefficient_si (aff, isl_dim_in, i + 1, 1);
	  univ = isl_set_universe (isl_space_domain (isl_aff_get_space (aff)));
	  index = isl_pw_aff_alloc (univ, aff);

	  id = isl_set_get_tuple_id (subscript_sizes);
	  lb = isl_pw_aff_set_tuple_id (lb, isl_dim_in, isl_id_copy (id));
	  ub = isl_pw_aff_set_tuple_id (ub, isl_dim_in, id);

	  /* low <= sub_i <= high */
	  lbs = isl_pw_aff_ge_set (isl_pw_aff_copy (index), lb);
	  ubs = isl_pw_aff_le_set (index, ub);
	  subscript_sizes = isl_set_intersect (subscript_sizes, lbs);
	  subscript_sizes = isl_set_intersect (subscript_sizes, ubs);
	}
    }

  return subscript_sizes;
}

/* Build data accesses for DR in PBB.  */

static void
build_poly_dr (dr_info &dri)
{
  isl_map *acc;
  isl_set *subscript_sizes;
  poly_bb_p pbb = dri.pbb;
  data_reference_p dr = dri.dr;
  scop_p scop = PBB_SCOP (pbb);

  {
    isl_space *dc = isl_set_get_space (pbb->domain);
    int nb_out = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_add_dims (isl_space_from_domain (dc),
					   isl_dim_out, nb_out);

    acc = isl_map_universe (space);
    acc = isl_map_set_tuple_id (acc, isl_dim_out, isl_id_for_dr (scop, dr));
  }

  acc = pdr_add_alias_set (acc, dri);
  acc = pdr_add_memory_accesses (acc, dri);

  {
    isl_id *id = isl_id_for_dr (scop, dr);
    int nb = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_set_alloc (scop->isl_context, 0, nb);

    space = isl_space_set_tuple_id (space, isl_dim_set, id);
    subscript_sizes = isl_set_nat_universe (space);
    subscript_sizes = isl_set_fix_si (subscript_sizes, isl_dim_set, 0,
				      dri.alias_set);
    subscript_sizes = pdr_add_data_dimensions (subscript_sizes, scop, dr);
  }

  new_poly_dr (pbb,
	       DR_IS_READ (dr) ? PDR_READ : PDR_WRITE,
	       dr, DR_NUM_DIMENSIONS (dr), acc, subscript_sizes);
}

/* Compute alias-sets for all data references in DRS.  */

static void
build_alias_set (scop_p scop)
{
  int num_vertices = scop->drs.length ();
  struct graph *g = new_graph (num_vertices);
  dr_info *dr1, *dr2;
  int i, j;
  int *all_vertices;

  FOR_EACH_VEC_ELT (scop->drs, i, dr1)
    for (j = i+1; scop->drs.iterate (j, &dr2); j++)
      if (dr_may_alias_p (dr1->dr, dr2->dr, true))
	{
	  add_edge (g, i, j);
	  add_edge (g, j, i);
	}

  all_vertices = XNEWVEC (int, num_vertices);
  for (i = 0; i < num_vertices; i++)
    all_vertices[i] = i;

  graphds_dfs (g, all_vertices, num_vertices, NULL, true, NULL);
  free (all_vertices);

  for (i = 0; i < g->n_vertices; i++)
    scop->drs[i].alias_set = g->vertices[i].component + 1;

  free_graph (g);
}

/* Build data references in SCOP.  */

static void
build_scop_drs (scop_p scop)
{
  int i, j;
  poly_bb_p pbb;

  /* Remove all the PBBs that do not have data references: these basic
     blocks are not handled in the polyhedral representation.  */
  for (i = 0; scop->pbbs.iterate (i, &pbb); i++)
    if (GBB_DATA_REFS (PBB_BLACK_BOX (pbb)).is_empty ())
      {
	free_gimple_poly_bb (PBB_BLACK_BOX (pbb));
	free_poly_bb (pbb);
	scop->pbbs.ordered_remove (i);
	i--;
      }

  data_reference_p dr;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    if (pbb)
      FOR_EACH_VEC_ELT (GBB_DATA_REFS (PBB_BLACK_BOX (pbb)), j, dr)
	scop->drs.safe_push (dr_info (dr, pbb));

  build_alias_set (scop);

  dr_info *dri;
  FOR_EACH_VEC_ELT (scop->drs, i, dri)
    build_poly_dr (*dri);
}

/* Analyze all the data references of STMTS and add them to the
   GBB_DATA_REFS vector of BB.  */

static void
analyze_drs_in_stmts (scop_p scop, basic_block bb, vec<gimple *> stmts)
{
  sese_l region = scop->scop_info->region;
  if (!bb_in_sese_p (bb, region))
    return;

  loop_p nest = outermost_loop_in_sese (region, bb);
  loop_p loop = bb->loop_father;
  if (!loop_in_sese_p (loop, region))
    loop = nest;

  gimple_poly_bb_p gbb = gbb_from_bb (bb);

  gimple *stmt;
  int i;
  FOR_EACH_VEC_ELT (stmts, i, stmt)
    {
      if (is_gimple_debug (stmt))
	continue;

      graphite_find_data_references_in_stmt (nest, loop, stmt,
					     &GBB_DATA_REFS (gbb));
    }
}

/* Insert STMT at the end of the STMTS sequence and then insert the
   statements from STMTS at INSERT_GSI and call analyze_drs_in_stmts
   on STMTS.  */

static void
insert_stmts (scop_p scop, gimple *stmt, gimple_seq stmts,
	      gimple_stmt_iterator insert_gsi)
{
  gimple_stmt_iterator gsi;
  auto_vec<gimple *, 3> x;

  gimple_seq_add_stmt (&stmts, stmt);
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    x.safe_push (gsi_stmt (gsi));

  gsi_insert_seq_before (&insert_gsi, stmts, GSI_SAME_STMT);
  analyze_drs_in_stmts (scop, gsi_bb (insert_gsi), x);
}

/* Insert the assignment "RES := EXPR" just after AFTER_STMT.  */

static void
insert_out_of_ssa_copy (scop_p scop, tree res, tree expr, gimple *after_stmt)
{
  gimple_stmt_iterator gsi;
  auto_vec<gimple *, 3> x;
  gimple_seq stmts;
  tree var = force_gimple_operand (expr, &stmts, true, NULL_TREE);
  gassign *stmt = gimple_build_assign (unshare_expr (res), var);

  gimple_seq_add_stmt (&stmts, stmt);

  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    x.safe_push (gsi_stmt (gsi));

  if (gimple_code (after_stmt) == GIMPLE_PHI)
    {
      gsi = gsi_after_labels (gimple_bb (after_stmt));
      gsi_insert_seq_before (&gsi, stmts, GSI_NEW_STMT);
    }
  else
    {
      gsi = gsi_for_stmt (after_stmt);
      gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
    }

  analyze_drs_in_stmts (scop, gimple_bb (after_stmt), x);
}

/* Creates a poly_bb_p for basic_block BB from the existing PBB.  */

static void
new_pbb_from_pbb (scop_p scop, poly_bb_p pbb, basic_block bb)
{
  vec<data_reference_p> drs;
  drs.create (3);
  gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);
  gimple_poly_bb_p gbb1 = new_gimple_poly_bb (bb, drs);
  poly_bb_p pbb1 = new_poly_bb (scop, gbb1);
  int index, n = scop->pbbs.length ();

  for (index = 0; index < n; index++)
    if (scop->pbbs[index] == pbb)
      break;

  pbb1->domain = isl_set_copy (pbb->domain);
  pbb1->domain = isl_set_set_tuple_id (pbb1->domain,
				       isl_id_for_pbb (scop, pbb1));

  GBB_PBB (gbb1) = pbb1;
  GBB_CONDITIONS (gbb1) = GBB_CONDITIONS (gbb).copy ();
  GBB_CONDITION_CASES (gbb1) = GBB_CONDITION_CASES (gbb).copy ();
  scop->pbbs.safe_insert (index + 1, pbb1);
}

/* Insert on edge E the assignment "RES := EXPR".  */

static void
insert_out_of_ssa_copy_on_edge (scop_p scop, edge e, tree res, tree expr)
{
  gimple_seq stmts = NULL;
  tree var = force_gimple_operand (expr, &stmts, true, NULL_TREE);
  gimple *stmt = gimple_build_assign (unshare_expr (res), var);
  auto_vec<gimple *, 3> x;

  gimple_seq_add_stmt (&stmts, stmt);
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    x.safe_push (gsi_stmt (gsi));

  gsi_insert_seq_on_edge (e, stmts);
  gsi_commit_edge_inserts ();
  basic_block bb = gimple_bb (stmt);

  if (!bb_in_sese_p (bb, scop->scop_info->region))
    return;

  if (!gbb_from_bb (bb))
    new_pbb_from_pbb (scop, pbb_from_bb (e->src), bb);

  analyze_drs_in_stmts (scop, bb, x);
}

/* Creates a zero dimension array of the same type as VAR.  */

static tree
create_zero_dim_array (tree var, const char *base_name)
{
  tree index_type = build_index_type (integer_zero_node);
  tree elt_type = TREE_TYPE (var);
  tree array_type = build_array_type (elt_type, index_type);
  tree base = create_tmp_var (array_type, base_name);

  return build4 (ARRAY_REF, elt_type, base, integer_zero_node, NULL_TREE,
		 NULL_TREE);
}

/* Returns true when PHI is a loop close phi node.  */

static bool
scalar_close_phi_node_p (gimple *phi)
{
  if (gimple_code (phi) != GIMPLE_PHI
      || virtual_operand_p (gimple_phi_result (phi)))
    return false;

  /* Note that loop close phi nodes should have a single argument
     because we translated the representation into a canonical form
     before Graphite: see canonicalize_loop_closed_ssa_form.  */
  return (gimple_phi_num_args (phi) == 1);
}

/* For a definition DEF in REGION, propagates the expression EXPR in
   all the uses of DEF outside REGION.  */

static void
propagate_expr_outside_region (tree def, tree expr, sese_l &region)
{
  gimple_seq stmts;
  bool replaced_once = false;

  gcc_assert (TREE_CODE (def) == SSA_NAME);

  expr = force_gimple_operand (unshare_expr (expr), &stmts, true,
			       NULL_TREE);

  imm_use_iterator imm_iter;
  gimple *use_stmt;
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    if (!is_gimple_debug (use_stmt)
	&& !bb_in_sese_p (gimple_bb (use_stmt), region))
      {
	ssa_op_iter iter;
	use_operand_p use_p;

	FOR_EACH_PHI_OR_STMT_USE (use_p, use_stmt, iter, SSA_OP_ALL_USES)
	  if (operand_equal_p (def, USE_FROM_PTR (use_p), 0)
	      && (replaced_once = true))
	    replace_exp (use_p, expr);

	update_stmt (use_stmt);
      }

  if (replaced_once)
    {
      gsi_insert_seq_on_edge (region.entry, stmts);
      gsi_commit_edge_inserts ();
    }
}

/* Rewrite out of SSA the reduction phi node at PSI by creating a zero
   dimension array for it.  */

static void
rewrite_close_phi_out_of_ssa (scop_p scop, gimple_stmt_iterator *psi)
{
  sese_l region = scop->scop_info->region;
  gimple *phi = gsi_stmt (*psi);
  tree res = gimple_phi_result (phi);
  basic_block bb = gimple_bb (phi);
  gimple_stmt_iterator gsi = gsi_after_labels (bb);
  tree arg = gimple_phi_arg_def (phi, 0);
  gimple *stmt;

  /* Note that loop close phi nodes should have a single argument
     because we translated the representation into a canonical form
     before Graphite: see canonicalize_loop_closed_ssa_form.  */
  gcc_assert (gimple_phi_num_args (phi) == 1);

  /* The phi node can be a non close phi node, when its argument is
     invariant, or a default definition.  */
  if (is_gimple_min_invariant (arg)
      || SSA_NAME_IS_DEFAULT_DEF (arg))
    {
      propagate_expr_outside_region (res, arg, region);
      gsi_next (psi);
      return;
    }

  else if (gimple_bb (SSA_NAME_DEF_STMT (arg))->loop_father == bb->loop_father)
    {
      propagate_expr_outside_region (res, arg, region);
      stmt = gimple_build_assign (res, arg);
      remove_phi_node (psi, false);
      gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
      return;
    }

  /* If res is scev analyzable and is not a scalar value, it is safe
     to ignore the close phi node: it will be code generated in the
     out of Graphite pass.  */
  else if (scev_analyzable_p (res, region))
    {
      loop_p loop = loop_containing_stmt (SSA_NAME_DEF_STMT (res));
      tree scev;

      if (!loop_in_sese_p (loop, region))
	{
	  loop = loop_containing_stmt (SSA_NAME_DEF_STMT (arg));
	  scev = scalar_evolution_in_region (region, loop, arg);
	  scev = compute_overall_effect_of_inner_loop (loop, scev);
	}
      else
	scev = scalar_evolution_in_region (region, loop, res);

      if (tree_does_not_contain_chrecs (scev))
	propagate_expr_outside_region (res, scev, region);

      gsi_next (psi);
      return;
    }
  else
    {
      tree zero_dim_array = create_zero_dim_array (res, "Close_Phi");

      stmt = gimple_build_assign (res, unshare_expr (zero_dim_array));

      if (TREE_CODE (arg) == SSA_NAME)
	insert_out_of_ssa_copy (scop, zero_dim_array, arg,
				SSA_NAME_DEF_STMT (arg));
      else
	insert_out_of_ssa_copy_on_edge (scop, single_pred_edge (bb),
					zero_dim_array, arg);
    }

  remove_phi_node (psi, false);
  SSA_NAME_DEF_STMT (res) = stmt;

  insert_stmts (scop, stmt, NULL, gsi_after_labels (bb));
}

/* Rewrite out of SSA the reduction phi node at PSI by creating a zero
   dimension array for it.  */

static void
rewrite_phi_out_of_ssa (scop_p scop, gphi_iterator *psi)
{
  gphi *phi = psi->phi ();
  basic_block bb = gimple_bb (phi);
  tree res = gimple_phi_result (phi);
  tree zero_dim_array = create_zero_dim_array (res, "phi_out_of_ssa");

  for (size_t i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);
      edge e = gimple_phi_arg_edge (phi, i);

      /* Avoid the insertion of code in the loop latch to please the
	 pattern matching of the vectorizer.  */
      if (TREE_CODE (arg) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (arg)
	  && e->src == bb->loop_father->latch)
	insert_out_of_ssa_copy (scop, zero_dim_array, arg,
				SSA_NAME_DEF_STMT (arg));
      else
	insert_out_of_ssa_copy_on_edge (scop, e, zero_dim_array, arg);
    }

  gimple *stmt = gimple_build_assign (res, unshare_expr (zero_dim_array));
  remove_phi_node (psi, false);
  insert_stmts (scop, stmt, NULL, gsi_after_labels (bb));
}

/* Rewrite the degenerate phi node at position PSI from the degenerate
   form "x = phi (y, y, ..., y)" to "x = y".  */

static void
rewrite_degenerate_phi (gphi_iterator *psi)
{
  gphi *phi = psi->phi ();
  tree res = gimple_phi_result (phi);

  basic_block bb = gimple_bb (phi);
  tree rhs = degenerate_phi_result (phi);
  gcc_assert (rhs);

  gimple *stmt = gimple_build_assign (res, rhs);
  remove_phi_node (psi, false);

  gimple_stmt_iterator gsi = gsi_after_labels (bb);
  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
}

/* Rewrite out of SSA all the reduction phi nodes of SCOP.  */

static void
rewrite_reductions_out_of_ssa (scop_p scop)
{
  int i;
  basic_block bb;
  FOR_EACH_VEC_ELT (scop->scop_info->bbs, i, bb)
    for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);)
      {
	gphi *phi = psi.phi ();

	if (virtual_operand_p (gimple_phi_result (phi)))
	  {
	    gsi_next (&psi);
	    continue;
	  }

	if (gimple_phi_num_args (phi) > 1
	    && degenerate_phi_result (phi))
	  rewrite_degenerate_phi (&psi);

	else if (scalar_close_phi_node_p (phi))
	  rewrite_close_phi_out_of_ssa (scop, &psi);

	else if (reduction_phi_p (scop->scop_info->region, &psi))
	  rewrite_phi_out_of_ssa (scop, &psi);
      }

  update_ssa (TODO_update_ssa);
  checking_verify_loop_closed_ssa (true);
}

/* Rewrite the scalar dependence of DEF used in USE_STMT with a memory
   read from ZERO_DIM_ARRAY.  */

static void
rewrite_cross_bb_scalar_dependence (scop_p scop, tree zero_dim_array,
				    tree def, gimple *use_stmt)
{
  gcc_assert (gimple_code (use_stmt) != GIMPLE_PHI);

  tree name = copy_ssa_name (def);
  gimple *name_stmt = gimple_build_assign (name, zero_dim_array);

  gimple_assign_set_lhs (name_stmt, name);
  insert_stmts (scop, name_stmt, NULL, gsi_for_stmt (use_stmt));

  ssa_op_iter iter;
  use_operand_p use_p;
  FOR_EACH_SSA_USE_OPERAND (use_p, use_stmt, iter, SSA_OP_ALL_USES)
    if (operand_equal_p (def, USE_FROM_PTR (use_p), 0))
      replace_exp (use_p, name);

  update_stmt (use_stmt);
}

/* For every definition DEF in the SCOP that is used outside the scop,
   insert a closing-scop definition in the basic block just after this
   SCOP.  */

static void
handle_scalar_deps_crossing_scop_limits (scop_p scop, tree def, gimple *stmt)
{
  tree var = create_tmp_reg (TREE_TYPE (def));
  tree new_name = make_ssa_name (var, stmt);
  bool needs_copy = false;
  sese_l region = scop->scop_info->region;

  imm_use_iterator imm_iter;
  gimple *use_stmt;
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    {
      if (!bb_in_sese_p (gimple_bb (use_stmt), region))
	{
	  use_operand_p use_p;
	  FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	    {
	      SET_USE (use_p, new_name);
	    }
	  update_stmt (use_stmt);
	  needs_copy = true;
	}
    }

  /* Insert in the empty BB just after the scop a use of DEF such
     that the rewrite of cross_bb_scalar_dependences won't insert
     arrays everywhere else.  */
  if (needs_copy)
    {
      gimple *assign = gimple_build_assign (new_name, def);
      gimple_stmt_iterator psi = gsi_after_labels (region.exit->dest);

      update_stmt (assign);
      gsi_insert_before (&psi, assign, GSI_SAME_STMT);
    }
}

/* Rewrite the scalar dependences crossing the boundary of the BB
   containing STMT with an array.  Return true when something has been
   changed.  */

static bool
rewrite_cross_bb_scalar_deps (scop_p scop, gimple_stmt_iterator *gsi)
{
  sese_l region = scop->scop_info->region;
  gimple *stmt = gsi_stmt (*gsi);
  imm_use_iterator imm_iter;
  tree def;
  tree zero_dim_array = NULL_TREE;
  gimple *use_stmt;
  bool res = false;

  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      def = gimple_assign_lhs (stmt);
      break;

    case GIMPLE_CALL:
      def = gimple_call_lhs (stmt);
      break;

    default:
      return false;
    }

  if (!def
      || !is_gimple_reg (def))
    return false;

  if (scev_analyzable_p (def, region))
    {
      loop_p loop = loop_containing_stmt (SSA_NAME_DEF_STMT (def));
      tree scev = scalar_evolution_in_region (region, loop, def);

      if (tree_contains_chrecs (scev, NULL))
	return false;

      propagate_expr_outside_region (def, scev, region);
      return true;
    }

  basic_block def_bb = gimple_bb (stmt);

  handle_scalar_deps_crossing_scop_limits (scop, def, stmt);

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    if (gphi *phi = dyn_cast <gphi *> (use_stmt))
      {
	res = true;
	gphi_iterator psi = gsi_for_phi (phi);

	if (scalar_close_phi_node_p (gsi_stmt (psi)))
	  rewrite_close_phi_out_of_ssa (scop, &psi);
	else
	  rewrite_phi_out_of_ssa (scop, &psi);
      }

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    if (gimple_code (use_stmt) != GIMPLE_PHI
	&& def_bb != gimple_bb (use_stmt)
	&& !is_gimple_debug (use_stmt)
	&& (res = true))
      {
	if (!zero_dim_array)
	  {
	    zero_dim_array = create_zero_dim_array
	      (def, "Cross_BB_scalar_dependence");
	    insert_out_of_ssa_copy (scop, zero_dim_array, def,
				    SSA_NAME_DEF_STMT (def));
	    gsi_next (gsi);
	  }

	rewrite_cross_bb_scalar_dependence (scop, unshare_expr (zero_dim_array),
					    def, use_stmt);
      }

  update_ssa (TODO_update_ssa);

  return res;
}

/* Rewrite out of SSA all the reduction phi nodes of SCOP.  */

static void
rewrite_cross_bb_scalar_deps_out_of_ssa (scop_p scop)
{
  gimple_stmt_iterator psi;
  sese_l region = scop->scop_info->region;
  bool changed = false;

  /* Create an extra empty BB after the scop.  */
  split_edge (region.exit);

  int i;
  basic_block bb;
  FOR_EACH_VEC_ELT (scop->scop_info->bbs, i, bb)
    for (psi = gsi_start_bb (bb); !gsi_end_p (psi); gsi_next (&psi))
      changed |= rewrite_cross_bb_scalar_deps (scop, &psi);

  if (changed)
    {
      scev_reset_htab ();
      update_ssa (TODO_update_ssa);
      checking_verify_loop_closed_ssa (true);
    }
}

/* Builds the polyhedral representation for a SESE region.  */

void
build_poly_scop (scop_p scop)
{
  set_scop_parameter_dim (scop);
  build_scop_iteration_domain (scop);
  build_scop_context (scop);
  add_conditions_to_constraints (scop);

  /* Rewrite out of SSA only after having translated the
     representation to the polyhedral representation to avoid scev
     analysis failures.  That means that these functions will insert
     new data references that they create in the right place.  */
  rewrite_reductions_out_of_ssa (scop);
  rewrite_cross_bb_scalar_deps_out_of_ssa (scop);

  build_scop_drs (scop);
  build_scop_scattering (scop);

  /* This SCoP has been translated to the polyhedral
     representation.  */
  scop->poly_scop_p = true;
}
#endif  /* HAVE_isl */
