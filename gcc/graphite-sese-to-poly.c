/* Conversion of SESE regions to Polyhedra.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.
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
#include <isl/constraint.h>
#include <isl/aff.h>
#include <cloog/cloog.h>
#include <cloog/cloog.h>
#include <cloog/isl/domain.h>
#endif

#include "system.h"
#include "coretypes.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "domwalk.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "graphite-poly.h"
#include "graphite-sese-to-poly.h"


/* Assigns to RES the value of the INTEGER_CST T.  */

static inline void
tree_int_to_gmp (tree t, mpz_t res)
{
  double_int di = tree_to_double_int (t);
  mpz_set_double_int (res, di, TYPE_UNSIGNED (TREE_TYPE (t)));
}

/* Returns the index of the PHI argument defined in the outermost
   loop.  */

static size_t
phi_arg_in_outermost_loop (gimple phi)
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
remove_simple_copy_phi (gimple_stmt_iterator *psi)
{
  gimple phi = gsi_stmt (*psi);
  tree res = gimple_phi_result (phi);
  size_t entry = phi_arg_in_outermost_loop (phi);
  tree init = gimple_phi_arg_def (phi, entry);
  gimple stmt = gimple_build_assign (res, init);
  edge e = gimple_phi_arg_edge (phi, entry);

  remove_phi_node (psi, false);
  gsi_insert_on_edge_immediate (e, stmt);
  SSA_NAME_DEF_STMT (res) = stmt;
}

/* Removes an invariant phi node at position PSI by inserting on the
   loop ENTRY edge the assignment RES = INIT.  */

static void
remove_invariant_phi (sese region, gimple_stmt_iterator *psi)
{
  gimple phi = gsi_stmt (*psi);
  loop_p loop = loop_containing_stmt (phi);
  tree res = gimple_phi_result (phi);
  tree scev = scalar_evolution_in_region (region, loop, res);
  size_t entry = phi_arg_in_outermost_loop (phi);
  edge e = gimple_phi_arg_edge (phi, entry);
  tree var;
  gimple stmt;
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
simple_copy_phi_p (gimple phi)
{
  tree res;

  if (gimple_phi_num_args (phi) != 2)
    return false;

  res = gimple_phi_result (phi);
  return (res == gimple_phi_arg_def (phi, 0)
	  || res == gimple_phi_arg_def (phi, 1));
}

/* Returns true when the phi node at position PSI is a reduction phi
   node in REGION.  Otherwise moves the pointer PSI to the next phi to
   be considered.  */

static bool
reduction_phi_p (sese region, gimple_stmt_iterator *psi)
{
  loop_p loop;
  gimple phi = gsi_stmt (*psi);
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

/* Store the GRAPHITE representation of BB.  */

static gimple_bb_p
new_gimple_bb (basic_block bb, vec<data_reference_p> drs)
{
  struct gimple_bb *gbb;

  gbb = XNEW (struct gimple_bb);
  bb->aux = gbb;
  GBB_BB (gbb) = bb;
  GBB_DATA_REFS (gbb) = drs;
  GBB_CONDITIONS (gbb).create (0);
  GBB_CONDITION_CASES (gbb).create (0);

  return gbb;
}

static void
free_data_refs_aux (vec<data_reference_p> datarefs)
{
  unsigned int i;
  struct data_reference *dr;

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    if (dr->aux)
      {
	base_alias_pair *bap = (base_alias_pair *)(dr->aux);

	free (bap->alias_set);

	free (bap);
	dr->aux = NULL;
      }
}
/* Frees GBB.  */

static void
free_gimple_bb (struct gimple_bb *gbb)
{
  free_data_refs_aux (GBB_DATA_REFS (gbb));
  free_data_refs (GBB_DATA_REFS (gbb));

  GBB_CONDITIONS (gbb).release ();
  GBB_CONDITION_CASES (gbb).release ();
  GBB_BB (gbb)->aux = 0;
  XDELETE (gbb);
}

/* Deletes all gimple bbs in SCOP.  */

static void
remove_gbbs_in_scop (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    free_gimple_bb (PBB_BLACK_BOX (pbb));
}

/* Deletes all scops in SCOPS.  */

void
free_scops (vec<scop_p> scops)
{
  int i;
  scop_p scop;

  FOR_EACH_VEC_ELT (scops, i, scop)
    {
      remove_gbbs_in_scop (scop);
      free_sese (SCOP_REGION (scop));
      free_scop (scop);
    }

  scops.release ();
}

/* Same as outermost_loop_in_sese, returns the outermost loop
   containing BB in REGION, but makes sure that the returned loop
   belongs to the REGION, and so this returns the first loop in the
   REGION when the loop containing BB does not belong to REGION.  */

static loop_p
outermost_loop_in_sese_1 (sese region, basic_block bb)
{
  loop_p nest = outermost_loop_in_sese (region, bb);

  if (loop_in_sese_p (nest, region))
    return nest;

  /* When the basic block BB does not belong to a loop in the region,
     return the first loop in the region.  */
  nest = nest->inner;
  while (nest)
    if (loop_in_sese_p (nest, region))
      break;
    else
      nest = nest->next;

  gcc_assert (nest);
  return nest;
}

/* Generates a polyhedral black box only if the bb contains interesting
   information.  */

static gimple_bb_p
try_generate_gimple_bb (scop_p scop, basic_block bb)
{
  vec<data_reference_p> drs;
  drs.create (5);
  sese region = SCOP_REGION (scop);
  loop_p nest = outermost_loop_in_sese_1 (region, bb);
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      loop_p loop;

      if (is_gimple_debug (stmt))
	continue;

      loop = loop_containing_stmt (stmt);
      if (!loop_in_sese_p (loop, region))
	loop = nest;

      graphite_find_data_references_in_stmt (nest, loop, stmt, &drs);
    }

  return new_gimple_bb (bb, drs);
}

/* Returns true if all predecessors of BB, that are not dominated by BB, are
   marked in MAP.  The predecessors dominated by BB are loop latches and will
   be handled after BB.  */

static bool
all_non_dominated_preds_marked_p (basic_block bb, sbitmap map)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!bitmap_bit_p (map, e->src->index)
	&& !dominated_by_p (CDI_DOMINATORS, e->src, bb))
	return false;

  return true;
}

/* Compare the depth of two basic_block's P1 and P2.  */

static int
compare_bb_depths (const void *p1, const void *p2)
{
  const_basic_block const bb1 = *(const_basic_block const*)p1;
  const_basic_block const bb2 = *(const_basic_block const*)p2;
  int d1 = loop_depth (bb1->loop_father);
  int d2 = loop_depth (bb2->loop_father);

  if (d1 < d2)
    return 1;

  if (d1 > d2)
    return -1;

  return 0;
}

/* Sort the basic blocks from DOM such that the first are the ones at
   a deepest loop level.  */

static void
graphite_sort_dominated_info (vec<basic_block> dom)
{
  dom.qsort (compare_bb_depths);
}

/* Recursive helper function for build_scops_bbs.  */

static void
build_scop_bbs_1 (scop_p scop, sbitmap visited, basic_block bb)
{
  sese region = SCOP_REGION (scop);
  vec<basic_block> dom;
  poly_bb_p pbb;

  if (bitmap_bit_p (visited, bb->index)
      || !bb_in_sese_p (bb, region))
    return;

  pbb = new_poly_bb (scop, try_generate_gimple_bb (scop, bb));
  SCOP_BBS (scop).safe_push (pbb);
  bitmap_set_bit (visited, bb->index);

  dom = get_dominated_by (CDI_DOMINATORS, bb);

  if (!dom.exists ())
    return;

  graphite_sort_dominated_info (dom);

  while (!dom.is_empty ())
    {
      int i;
      basic_block dom_bb;

      FOR_EACH_VEC_ELT (dom, i, dom_bb)
	if (all_non_dominated_preds_marked_p (dom_bb, visited))
	  {
	    build_scop_bbs_1 (scop, visited, dom_bb);
	    dom.unordered_remove (i);
	    break;
	  }
    }

  dom.release ();
}

/* Gather the basic blocks belonging to the SCOP.  */

static void
build_scop_bbs (scop_p scop)
{
  sbitmap visited = sbitmap_alloc (last_basic_block);
  sese region = SCOP_REGION (scop);

  bitmap_clear (visited);
  build_scop_bbs_1 (scop, visited, SESE_ENTRY_BB (region));
  sbitmap_free (visited);
}

/* Return an ISL identifier for the polyhedral basic block PBB.  */

static isl_id *
isl_id_for_pbb (scop_p s, poly_bb_p pbb)
{
  char name[50];
  snprintf (name, sizeof (name), "S_%d", pbb_index (pbb));
  return isl_id_alloc (s->ctx, name, pbb);
}

/* Converts the STATIC_SCHEDULE of PBB into a scattering polyhedron.
   We generate SCATTERING_DIMENSIONS scattering dimensions.

   CLooG 0.15.0 and previous versions require, that all
   scattering functions of one CloogProgram have the same number of
   scattering dimensions, therefore we allow to specify it.  This
   should be removed in future versions of CLooG.

   The scattering polyhedron consists of these dimensions: scattering,
   loop_iterators, parameters.

   Example:

   | scattering_dimensions = 5
   | used_scattering_dimensions = 3
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
				  poly_bb_p pbb, int scattering_dimensions)
{
  int i;
  int nb_iterators = pbb_dim_iter_domain (pbb);
  int used_scattering_dimensions = nb_iterators * 2 + 1;
  isl_int val;
  isl_space *dc, *dm;

  gcc_assert (scattering_dimensions >= used_scattering_dimensions);

  isl_int_init (val);

  dc = isl_set_get_space (pbb->domain);
  dm = isl_space_add_dims (isl_space_from_domain (dc),
			   isl_dim_out, scattering_dimensions);
  pbb->schedule = isl_map_universe (dm);

  for (i = 0; i < scattering_dimensions; i++)
    {
      /* Textual order inside this loop.  */
      if ((i % 2) == 0)
	{
	  isl_constraint *c = isl_equality_alloc
	      (isl_local_space_from_space (isl_map_get_space (pbb->schedule)));

	  if (0 != isl_aff_get_coefficient (static_sched, isl_dim_in,
					    i / 2, &val))
	    gcc_unreachable ();

	  isl_int_neg (val, val);
	  c = isl_constraint_set_constant (c, val);
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

  isl_int_clear (val);

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
  int i;
  poly_bb_p pbb;
  gimple_bb_p previous_gbb = NULL;
  isl_space *dc = isl_set_get_space (scop->context);
  isl_aff *static_sched;

  dc = isl_space_add_dims (dc, isl_dim_set, number_of_loops (cfun));
  static_sched = isl_aff_zero_on_domain (isl_local_space_from_space (dc));

  /* We have to start schedules at 0 on the first component and
     because we cannot compare_prefix_loops against a previous loop,
     prefix will be equal to zero, and that index will be
     incremented before copying.  */
  static_sched = isl_aff_add_coefficient_si (static_sched, isl_dim_in, 0, -1);

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
      int prefix;
      int nb_scat_dims = pbb_dim_iter_domain (pbb) * 2 + 1;

      if (previous_gbb)
	prefix = nb_common_loops (SCOP_REGION (scop), previous_gbb, gbb);
      else
	prefix = 0;

      previous_gbb = gbb;

      static_sched = isl_aff_add_coefficient_si (static_sched, isl_dim_in,
						 prefix, 1);
      build_pbb_scattering_polyhedrons (static_sched, pbb, nb_scat_dims);
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
  unsigned pos = sese_loop_depth ((sese) s->region, get_chrec_loop (e)) - 1;
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
    id = isl_id_alloc (s->ctx, name, e);
  else
    {
      char name1[50];
      snprintf (name1, sizeof (name1), "P_%d", SSA_NAME_VERSION (e));
      id = isl_id_alloc (s->ctx, name1, e);
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
  return isl_id_alloc (s->ctx, "", 0);
}

/* Extract an affine expression from the ssa_name E.  */

static isl_pw_aff *
extract_affine_name (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_aff *aff;
  isl_set *dom;
  isl_id *id;
  int dimension;

  id = isl_id_for_ssa_name (s, e);
  dimension = isl_space_find_dim_by_id (space, isl_dim_param, id);
  isl_id_free (id);
  dom = isl_set_universe (isl_space_copy (space));
  aff = isl_aff_zero_on_domain (isl_local_space_from_space (space));
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
  isl_int v;

  isl_int_init (v);
  isl_int_set_gmp (v, g);
  aff = isl_aff_add_constant (aff, v);
  isl_int_clear (v);

  return isl_pw_aff_alloc (dom, aff);
}

/* Extract an affine expression from the integer_cst E.  */

static isl_pw_aff *
extract_affine_int (tree e, __isl_take isl_space *space)
{
  isl_pw_aff *res;
  mpz_t g;

  mpz_init (g);
  tree_int_to_gmp (e, g);
  res = extract_affine_gmp (g, space);
  mpz_clear (g);

  return res;
}

/* Compute pwaff mod 2^width.  */

static isl_pw_aff *
wrap (isl_pw_aff *pwaff, unsigned width)
{
  isl_int mod;

  isl_int_init (mod);
  isl_int_set_si (mod, 1);
  isl_int_mul_2exp (mod, mod, width);

  pwaff = isl_pw_aff_mod (pwaff, mod);

  isl_int_clear (mod);

  return pwaff;
}

/* When parameter NAME is in REGION, returns its index in SESE_PARAMS.
   Otherwise returns -1.  */

static inline int
parameter_index_in_region_1 (tree name, sese region)
{
  int i;
  tree p;

  gcc_assert (TREE_CODE (name) == SSA_NAME);

  FOR_EACH_VEC_ELT (SESE_PARAMS (region), i, p)
    if (p == name)
      return i;

  return -1;
}

/* When the parameter NAME is in REGION, returns its index in
   SESE_PARAMS.  Otherwise this function inserts NAME in SESE_PARAMS
   and returns the index of NAME.  */

static int
parameter_index_in_region (tree name, sese region)
{
  int i;

  gcc_assert (TREE_CODE (name) == SSA_NAME);

  i = parameter_index_in_region_1 (name, region);
  if (i != -1)
    return i;

  gcc_assert (SESE_ADD_PARAMS (region));

  i = SESE_PARAMS (region).length ();
  SESE_PARAMS (region).safe_push (name);
  return i;
}

/* Extract an affine expression from the tree E in the scop S.  */

static isl_pw_aff *
extract_affine (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_pw_aff *lhs, *rhs, *res;
  tree type;

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
      gcc_assert (-1 != parameter_index_in_region_1 (e, SCOP_REGION (s)));
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

  type = TREE_TYPE (e);
  if (TYPE_UNSIGNED (type))
    res = wrap (res, TYPE_PRECISION (type));

  return res;
}

/* In the context of sese S, scan the expression E and translate it to
   a linear expression C.  When parsing a symbolic multiplication, K
   represents the constant multiplier of an expression containing
   parameters.  */

static void
scan_tree_for_params (sese s, tree e)
{
  if (e == chrec_dont_know)
    return;

  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      scan_tree_for_params (s, CHREC_LEFT (e));
      break;

    case MULT_EXPR:
      if (chrec_contains_symbols (TREE_OPERAND (e, 0)))
	scan_tree_for_params (s, TREE_OPERAND (e, 0));
      else
	scan_tree_for_params (s, TREE_OPERAND (e, 1));
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0));
      scan_tree_for_params (s, TREE_OPERAND (e, 1));
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0));
      break;

    case SSA_NAME:
      parameter_index_in_region (e, s);
      break;

    case INTEGER_CST:
    case ADDR_EXPR:
      break;

   default:
      gcc_unreachable ();
      break;
    }
}

/* Find parameters with respect to REGION in BB. We are looking in memory
   access functions, conditions and loop bounds.  */

static void
find_params_in_bb (sese region, gimple_bb_p gbb)
{
  int i;
  unsigned j;
  data_reference_p dr;
  gimple stmt;
  loop_p loop = GBB_BB (gbb)->loop_father;

  /* Find parameters in the access functions of data references.  */
  FOR_EACH_VEC_ELT (GBB_DATA_REFS (gbb), i, dr)
    for (j = 0; j < DR_NUM_DIMENSIONS (dr); j++)
      scan_tree_for_params (region, DR_ACCESS_FN (dr, j));

  /* Find parameters in conditional statements.  */
  FOR_EACH_VEC_ELT (GBB_CONDITIONS (gbb), i, stmt)
    {
      tree lhs = scalar_evolution_in_region (region, loop,
					     gimple_cond_lhs (stmt));
      tree rhs = scalar_evolution_in_region (region, loop,
					     gimple_cond_rhs (stmt));

      scan_tree_for_params (region, lhs);
      scan_tree_for_params (region, rhs);
    }
}

/* Record the parameters used in the SCOP.  A variable is a parameter
   in a scop if it does not vary during the execution of that scop.  */

static void
find_scop_parameters (scop_p scop)
{
  poly_bb_p pbb;
  unsigned i;
  sese region = SCOP_REGION (scop);
  struct loop *loop;
  int nbp;

  /* Find the parameters used in the loop bounds.  */
  FOR_EACH_VEC_ELT (SESE_LOOP_NEST (region), i, loop)
    {
      tree nb_iters = number_of_latch_executions (loop);

      if (!chrec_contains_symbols (nb_iters))
	continue;

      nb_iters = scalar_evolution_in_region (region, loop, nb_iters);
      scan_tree_for_params (region, nb_iters);
    }

  /* Find the parameters used in data accesses.  */
  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    find_params_in_bb (region, PBB_BLACK_BOX (pbb));

  nbp = sese_nb_params (region);
  scop_set_nb_params (scop, nbp);
  SESE_ADD_PARAMS (region) = false;

  {
    tree e;
    isl_space *space = isl_space_set_alloc (scop->ctx, nbp, 0);

    FOR_EACH_VEC_ELT (SESE_PARAMS (region), i, e)
      space = isl_space_set_dim_id (space, isl_dim_param, i,
				    isl_id_for_ssa_name (scop, e));

    scop->context = isl_set_universe (space);
  }
}

/* Builds the constraint polyhedra for LOOP in SCOP.  OUTER_PH gives
   the constraints for the surrounding loops.  */

static void
build_loop_iteration_domains (scop_p scop, struct loop *loop,
                              int nb,
			      isl_set *outer, isl_set **doms)
{
  tree nb_iters = number_of_latch_executions (loop);
  sese region = SCOP_REGION (scop);

  isl_set *inner = isl_set_copy (outer);
  isl_space *space;
  isl_constraint *c;
  int pos = isl_set_dim (outer, isl_dim_set);
  isl_int v;
  mpz_t g;

  mpz_init (g);
  isl_int_init (v);

  inner = isl_set_add_dims (inner, isl_dim_set, 1);
  space = isl_set_get_space (inner);

  /* 0 <= loop_i */
  c = isl_inequality_alloc
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
      isl_int_set_gmp (v, g);
      c = isl_constraint_set_constant (c, v);
      inner = isl_set_add_constraint (inner, c);
    }

  /* loop_i <= expr_nb_iters */
  else if (!chrec_contains_undetermined (nb_iters))
    {
      double_int nit;
      isl_pw_aff *aff;
      isl_set *valid;
      isl_local_space *ls;
      isl_aff *al;
      isl_set *le;

      nb_iters = scalar_evolution_in_region (region, loop, nb_iters);

      aff = extract_affine (scop, nb_iters, isl_set_get_space (inner));
      valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (aff));
      valid = isl_set_project_out (valid, isl_dim_set, 0,
				   isl_set_dim (valid, isl_dim_set));
      scop->context = isl_set_intersect (scop->context, valid);

      ls = isl_local_space_from_space (isl_space_copy (space));
      al = isl_aff_set_coefficient_si (isl_aff_zero_on_domain (ls),
				       isl_dim_in, pos, 1);
      le = isl_pw_aff_le_set (isl_pw_aff_from_aff (al),
			      isl_pw_aff_copy (aff));
      inner = isl_set_intersect (inner, le);

      if (max_stmt_executions (loop, &nit))
	{
	  /* Insert in the context the constraints from the
	     estimation of the number of iterations NIT and the
	     symbolic number of iterations (involving parameter
	     names) NB_ITERS.  First, build the affine expression
	     "NIT - NB_ITERS" and then say that it is positive,
	     i.e., NIT approximates NB_ITERS: "NIT >= NB_ITERS".  */
	  isl_pw_aff *approx;
	  mpz_t g;
	  isl_set *x;
	  isl_constraint *c;

	  mpz_init (g);
	  mpz_set_double_int (g, nit, false);
	  mpz_sub_ui (g, g, 1);
	  approx = extract_affine_gmp (g, isl_set_get_space (inner));
	  x = isl_pw_aff_ge_set (approx, aff);
	  x = isl_set_project_out (x, isl_dim_set, 0,
				   isl_set_dim (x, isl_dim_set));
	  scop->context = isl_set_intersect (scop->context, x);

	  c = isl_inequality_alloc
	      (isl_local_space_from_space (isl_space_copy (space)));
	  c = isl_constraint_set_coefficient_si (c, isl_dim_set, pos, -1);
	  isl_int_set_gmp (v, g);
	  mpz_clear (g);
	  c = isl_constraint_set_constant (c, v);
	  inner = isl_set_add_constraint (inner, c);
	}
      else
	isl_pw_aff_free (aff);
    }
  else
    gcc_unreachable ();

  if (loop->inner && loop_in_sese_p (loop->inner, region))
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
  isl_int_clear (v);
  mpz_clear (g);
}

/* Returns a linear expression for tree T evaluated in PBB.  */

static isl_pw_aff *
create_pw_aff_from_tree (poly_bb_p pbb, tree t)
{
  scop_p scop = PBB_SCOP (pbb);

  t = scalar_evolution_in_region (SCOP_REGION (scop), pbb_loop (pbb), t);
  gcc_assert (!automatically_generated_chrec_p (t));

  return extract_affine (scop, t, isl_set_get_space (pbb->domain));
}

/* Add conditional statement STMT to pbb.  CODE is used as the comparison
   operator.  This allows us to invert the condition or to handle
   inequalities.  */

static void
add_condition_to_pbb (poly_bb_p pbb, gimple stmt, enum tree_code code)
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
  gimple stmt;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (GBB_CONDITIONS (gbb).is_empty ())
    return;

  FOR_EACH_VEC_ELT (GBB_CONDITIONS (gbb), i, stmt)
    switch (gimple_code (stmt))
      {
      case GIMPLE_COND:
	  {
	    enum tree_code code = gimple_cond_code (stmt);

	    /* The conditions for ELSE-branches are inverted.  */
	    if (!GBB_CONDITION_CASES (gbb)[i])
	      code = invert_tree_comparison (code, false);

	    add_condition_to_pbb (pbb, stmt, code);
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

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    add_conditions_to_domain (pbb);
}

/* Returns a COND_EXPR statement when BB has a single predecessor, the
   edge between BB and its predecessor is not a loop exit edge, and
   the last statement of the single predecessor is a COND_EXPR.  */

static gimple
single_pred_cond_non_loop_exit (basic_block bb)
{
  if (single_pred_p (bb))
    {
      edge e = single_pred_edge (bb);
      basic_block pred = e->src;
      gimple stmt;

      if (loop_depth (pred->loop_father) > loop_depth (bb->loop_father))
	return NULL;

      stmt = last_stmt (pred);

      if (stmt && gimple_code (stmt) == GIMPLE_COND)
	return stmt;
    }

  return NULL;
}

class sese_dom_walker : public dom_walker
{
public:
  sese_dom_walker (cdi_direction, sese);
  ~sese_dom_walker ();

  virtual void before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

private:
  vec<gimple> m_conditions, m_cases;
  sese m_region;
};

sese_dom_walker::sese_dom_walker (cdi_direction direction, sese region)
  : dom_walker (direction), m_region (region)
{
  m_conditions.create (3);
  m_cases.create (3);
}

sese_dom_walker::~sese_dom_walker ()
{
  m_conditions.release ();
  m_cases.release ();
}

/* Call-back for dom_walk executed before visiting the dominated
   blocks.  */

void
sese_dom_walker::before_dom_children (basic_block bb)
{
  gimple_bb_p gbb;
  gimple stmt;

  if (!bb_in_sese_p (bb, m_region))
    return;

  stmt = single_pred_cond_non_loop_exit (bb);

  if (stmt)
    {
      edge e = single_pred_edge (bb);

      m_conditions.safe_push (stmt);

      if (e->flags & EDGE_TRUE_VALUE)
	m_cases.safe_push (stmt);
      else
	m_cases.safe_push (NULL);
    }

  gbb = gbb_from_bb (bb);

  if (gbb)
    {
      GBB_CONDITIONS (gbb) = m_conditions.copy ();
      GBB_CONDITION_CASES (gbb) = m_cases.copy ();
    }
}

/* Call-back for dom_walk executed after visiting the dominated
   blocks.  */

void
sese_dom_walker::after_dom_children (basic_block bb)
{
  if (!bb_in_sese_p (bb, m_region))
    return;

  if (single_pred_cond_non_loop_exit (bb))
    {
      m_conditions.pop ();
      m_cases.pop ();
    }
}

/* Add constraints on the possible values of parameter P from the type
   of P.  */

static void
add_param_constraints (scop_p scop, graphite_dim_t p)
{
  tree parameter = SESE_PARAMS (SCOP_REGION (scop))[p];
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
      isl_space *space = isl_set_get_space (scop->context);
      isl_constraint *c;
      mpz_t g;
      isl_int v;

      c = isl_inequality_alloc (isl_local_space_from_space (space));
      mpz_init (g);
      isl_int_init (v);
      tree_int_to_gmp (lb, g);
      isl_int_set_gmp (v, g);
      isl_int_neg (v, v);
      mpz_clear (g);
      c = isl_constraint_set_constant (c, v);
      isl_int_clear (v);
      c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, 1);

      scop->context = isl_set_add_constraint (scop->context, c);
    }

  if (ub)
    {
      isl_space *space = isl_set_get_space (scop->context);
      isl_constraint *c;
      mpz_t g;
      isl_int v;

      c = isl_inequality_alloc (isl_local_space_from_space (space));

      mpz_init (g);
      isl_int_init (v);
      tree_int_to_gmp (ub, g);
      isl_int_set_gmp (v, g);
      mpz_clear (g);
      c = isl_constraint_set_constant (c, v);
      isl_int_clear (v);
      c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, -1);

      scop->context = isl_set_add_constraint (scop->context, c);
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
  struct loop *loop;
  sese region = SCOP_REGION (scop);
  int i;
  poly_bb_p pbb;
  int nb_loops = number_of_loops (cfun);
  isl_set **doms = XCNEWVEC (isl_set *, nb_loops);

  FOR_EACH_VEC_ELT (SESE_LOOP_NEST (region), i, loop)
    if (!loop_in_sese_p (loop_outer (loop), region))
      build_loop_iteration_domains (scop, loop, 0,
				    isl_set_copy (scop->context), doms);

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      loop = pbb_loop (pbb);

      if (doms[loop->num])
	pbb->domain = isl_set_copy (doms[loop->num]);
      else
	pbb->domain = isl_set_copy (scop->context);

      pbb->domain = isl_set_set_tuple_id (pbb->domain,
					  isl_id_for_pbb (scop, pbb));
    }

  for (i = 0; i < nb_loops; i++)
    if (doms[i])
      isl_set_free (doms[i]);

  free (doms);
}

/* Add a constrain to the ACCESSES polyhedron for the alias set of
   data reference DR.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static isl_map *
pdr_add_alias_set (isl_map *acc, data_reference_p dr)
{
  isl_constraint *c;
  int alias_set_num = 0;
  base_alias_pair *bap = (base_alias_pair *)(dr->aux);

  if (bap && bap->alias_set)
    alias_set_num = *(bap->alias_set);

  c = isl_equality_alloc
      (isl_local_space_from_space (isl_map_get_space (acc)));
  c = isl_constraint_set_constant_si (c, -alias_set_num);
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
pdr_add_memory_accesses (isl_map *acc, data_reference_p dr, poly_bb_p pbb)
{
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
pdr_add_data_dimensions (isl_set *extent, scop_p scop, data_reference_p dr)
{
  tree ref = DR_REF (dr);
  int i, nb_subscripts = DR_NUM_DIMENSIONS (dr);

  for (i = nb_subscripts - 1; i >= 0; i--, ref = TREE_OPERAND (ref, 0))
    {
      tree low, high;

      if (TREE_CODE (ref) != ARRAY_REF)
	break;

      low = array_ref_low_bound (ref);
      high = array_ref_up_bound (ref);

      /* XXX The PPL code dealt separately with
         subscript - low >= 0 and high - subscript >= 0 in case one of
	 the two bounds isn't known.  Do the same here?  */

      if (host_integerp (low, 0)
	  && high
	  && host_integerp (high, 0)
	  /* 1-element arrays at end of structures may extend over
	     their declared size.  */
	  && !(array_at_struct_end_p (ref)
	       && operand_equal_p (low, high, 0)))
	{
	  isl_id *id;
	  isl_aff *aff;
	  isl_set *univ, *lbs, *ubs;
	  isl_pw_aff *index;
	  isl_space *space;
	  isl_set *valid;
	  isl_pw_aff *lb = extract_affine_int (low, isl_set_get_space (extent));
	  isl_pw_aff *ub = extract_affine_int (high, isl_set_get_space (extent));

	  /* high >= 0 */
	  valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (ub));
	  valid = isl_set_project_out (valid, isl_dim_set, 0,
				       isl_set_dim (valid, isl_dim_set));
	  scop->context = isl_set_intersect (scop->context, valid);

	  space = isl_set_get_space (extent);
	  aff = isl_aff_zero_on_domain (isl_local_space_from_space (space));
	  aff = isl_aff_add_coefficient_si (aff, isl_dim_in, i + 1, 1);
	  univ = isl_set_universe (isl_space_domain (isl_aff_get_space (aff)));
	  index = isl_pw_aff_alloc (univ, aff);

	  id = isl_set_get_tuple_id (extent);
	  lb = isl_pw_aff_set_tuple_id (lb, isl_dim_in, isl_id_copy (id));
	  ub = isl_pw_aff_set_tuple_id (ub, isl_dim_in, id);

	  /* low <= sub_i <= high */
	  lbs = isl_pw_aff_ge_set (isl_pw_aff_copy (index), lb);
	  ubs = isl_pw_aff_le_set (index, ub);
	  extent = isl_set_intersect (extent, lbs);
	  extent = isl_set_intersect (extent, ubs);
	}
    }

  return extent;
}

/* Build data accesses for DR in PBB.  */

static void
build_poly_dr (data_reference_p dr, poly_bb_p pbb)
{
  int dr_base_object_set;
  isl_map *acc;
  isl_set *extent;
  scop_p scop = PBB_SCOP (pbb);

  {
    isl_space *dc = isl_set_get_space (pbb->domain);
    int nb_out = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_add_dims (isl_space_from_domain (dc),
					   isl_dim_out, nb_out);

    acc = isl_map_universe (space);
    acc = isl_map_set_tuple_id (acc, isl_dim_out, isl_id_for_dr (scop, dr));
  }

  acc = pdr_add_alias_set (acc, dr);
  acc = pdr_add_memory_accesses (acc, dr, pbb);

  {
    isl_id *id = isl_id_for_dr (scop, dr);
    int nb = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_set_alloc (scop->ctx, 0, nb);
    int alias_set_num = 0;
    base_alias_pair *bap = (base_alias_pair *)(dr->aux);

    if (bap && bap->alias_set)
      alias_set_num = *(bap->alias_set);

    space = isl_space_set_tuple_id (space, isl_dim_set, id);
    extent = isl_set_nat_universe (space);
    extent = isl_set_fix_si (extent, isl_dim_set, 0, alias_set_num);
    extent = pdr_add_data_dimensions (extent, scop, dr);
  }

  gcc_assert (dr->aux);
  dr_base_object_set = ((base_alias_pair *)(dr->aux))->base_obj_set;

  new_poly_dr (pbb, dr_base_object_set,
	       DR_IS_READ (dr) ? PDR_READ : PDR_WRITE,
	       dr, DR_NUM_DIMENSIONS (dr), acc, extent);
}

/* Write to FILE the alias graph of data references in DIMACS format.  */

static inline bool
write_alias_graph_to_ascii_dimacs (FILE *file, char *comment,
				   vec<data_reference_p> drs)
{
  int num_vertex = drs.length ();
  int edge_num = 0;
  data_reference_p dr1, dr2;
  int i, j;

  if (num_vertex == 0)
    return true;

  FOR_EACH_VEC_ELT (drs, i, dr1)
    for (j = i + 1; drs.iterate (j, &dr2); j++)
      if (dr_may_alias_p (dr1, dr2, true))
	edge_num++;

  fprintf (file, "$\n");

  if (comment)
    fprintf (file, "c %s\n", comment);

  fprintf (file, "p edge %d %d\n", num_vertex, edge_num);

  FOR_EACH_VEC_ELT (drs, i, dr1)
    for (j = i + 1; drs.iterate (j, &dr2); j++)
      if (dr_may_alias_p (dr1, dr2, true))
	fprintf (file, "e %d %d\n", i + 1, j + 1);

  return true;
}

/* Write to FILE the alias graph of data references in DOT format.  */

static inline bool
write_alias_graph_to_ascii_dot (FILE *file, char *comment,
				vec<data_reference_p> drs)
{
  int num_vertex = drs.length ();
  data_reference_p dr1, dr2;
  int i, j;

  if (num_vertex == 0)
    return true;

  fprintf (file, "$\n");

  if (comment)
    fprintf (file, "c %s\n", comment);

  /* First print all the vertices.  */
  FOR_EACH_VEC_ELT (drs, i, dr1)
    fprintf (file, "n%d;\n", i);

  FOR_EACH_VEC_ELT (drs, i, dr1)
    for (j = i + 1; drs.iterate (j, &dr2); j++)
      if (dr_may_alias_p (dr1, dr2, true))
	fprintf (file, "n%d n%d\n", i, j);

  return true;
}

/* Write to FILE the alias graph of data references in ECC format.  */

static inline bool
write_alias_graph_to_ascii_ecc (FILE *file, char *comment,
				vec<data_reference_p> drs)
{
  int num_vertex = drs.length ();
  data_reference_p dr1, dr2;
  int i, j;

  if (num_vertex == 0)
    return true;

  fprintf (file, "$\n");

  if (comment)
    fprintf (file, "c %s\n", comment);

  FOR_EACH_VEC_ELT (drs, i, dr1)
    for (j = i + 1; drs.iterate (j, &dr2); j++)
      if (dr_may_alias_p (dr1, dr2, true))
	fprintf (file, "%d %d\n", i, j);

  return true;
}

/* Check if DR1 and DR2 are in the same object set.  */

static bool
dr_same_base_object_p (const struct data_reference *dr1,
		       const struct data_reference *dr2)
{
  return operand_equal_p (DR_BASE_OBJECT (dr1), DR_BASE_OBJECT (dr2), 0);
}

/* Uses DFS component number as representative of alias-sets. Also tests for
   optimality by verifying if every connected component is a clique. Returns
   true (1) if the above test is true, and false (0) otherwise.  */

static int
build_alias_set_optimal_p (vec<data_reference_p> drs)
{
  int num_vertices = drs.length ();
  struct graph *g = new_graph (num_vertices);
  data_reference_p dr1, dr2;
  int i, j;
  int num_connected_components;
  int v_indx1, v_indx2, num_vertices_in_component;
  int *all_vertices;
  int *vertices;
  struct graph_edge *e;
  int this_component_is_clique;
  int all_components_are_cliques = 1;

  FOR_EACH_VEC_ELT (drs, i, dr1)
    for (j = i+1; drs.iterate (j, &dr2); j++)
      if (dr_may_alias_p (dr1, dr2, true))
	{
	  add_edge (g, i, j);
	  add_edge (g, j, i);
	}

  all_vertices = XNEWVEC (int, num_vertices);
  vertices = XNEWVEC (int, num_vertices);
  for (i = 0; i < num_vertices; i++)
    all_vertices[i] = i;

  num_connected_components = graphds_dfs (g, all_vertices, num_vertices,
					  NULL, true, NULL);
  for (i = 0; i < g->n_vertices; i++)
    {
      data_reference_p dr = drs[i];
      base_alias_pair *bap;

      gcc_assert (dr->aux);
      bap = (base_alias_pair *)(dr->aux);

      bap->alias_set = XNEW (int);
      *(bap->alias_set) = g->vertices[i].component + 1;
    }

  /* Verify if the DFS numbering results in optimal solution.  */
  for (i = 0; i < num_connected_components; i++)
    {
      num_vertices_in_component = 0;
      /* Get all vertices whose DFS component number is the same as i.  */
      for (j = 0; j < num_vertices; j++)
	if (g->vertices[j].component == i)
	  vertices[num_vertices_in_component++] = j;

      /* Now test if the vertices in 'vertices' form a clique, by testing
	 for edges among each pair.  */
      this_component_is_clique = 1;
      for (v_indx1 = 0; v_indx1 < num_vertices_in_component; v_indx1++)
	{
	  for (v_indx2 = v_indx1+1; v_indx2 < num_vertices_in_component; v_indx2++)
	    {
	      /* Check if the two vertices are connected by iterating
		 through all the edges which have one of these are source.  */
	      e = g->vertices[vertices[v_indx2]].pred;
	      while (e)
		{
		  if (e->src == vertices[v_indx1])
		    break;
		  e = e->pred_next;
		}
	      if (!e)
		{
		  this_component_is_clique = 0;
		  break;
		}
	    }
	  if (!this_component_is_clique)
	    all_components_are_cliques = 0;
	}
    }

  free (all_vertices);
  free (vertices);
  free_graph (g);
  return all_components_are_cliques;
}

/* Group each data reference in DRS with its base object set num.  */

static void
build_base_obj_set_for_drs (vec<data_reference_p> drs)
{
  int num_vertex = drs.length ();
  struct graph *g = new_graph (num_vertex);
  data_reference_p dr1, dr2;
  int i, j;
  int *queue;

  FOR_EACH_VEC_ELT (drs, i, dr1)
    for (j = i + 1; drs.iterate (j, &dr2); j++)
      if (dr_same_base_object_p (dr1, dr2))
	{
	  add_edge (g, i, j);
	  add_edge (g, j, i);
	}

  queue = XNEWVEC (int, num_vertex);
  for (i = 0; i < num_vertex; i++)
    queue[i] = i;

  graphds_dfs (g, queue, num_vertex, NULL, true, NULL);

  for (i = 0; i < g->n_vertices; i++)
    {
      data_reference_p dr = drs[i];
      base_alias_pair *bap;

      gcc_assert (dr->aux);
      bap = (base_alias_pair *)(dr->aux);

      bap->base_obj_set = g->vertices[i].component + 1;
    }

  free (queue);
  free_graph (g);
}

/* Build the data references for PBB.  */

static void
build_pbb_drs (poly_bb_p pbb)
{
  int j;
  data_reference_p dr;
  vec<data_reference_p> gbb_drs = GBB_DATA_REFS (PBB_BLACK_BOX (pbb));

  FOR_EACH_VEC_ELT (gbb_drs, j, dr)
    build_poly_dr (dr, pbb);
}

/* Dump to file the alias graphs for the data references in DRS.  */

static void
dump_alias_graphs (vec<data_reference_p> drs)
{
  char comment[100];
  FILE *file_dimacs, *file_ecc, *file_dot;

  file_dimacs = fopen ("/tmp/dr_alias_graph_dimacs", "ab");
  if (file_dimacs)
    {
      snprintf (comment, sizeof (comment), "%s %s", main_input_filename,
		current_function_name ());
      write_alias_graph_to_ascii_dimacs (file_dimacs, comment, drs);
      fclose (file_dimacs);
    }

  file_ecc = fopen ("/tmp/dr_alias_graph_ecc", "ab");
  if (file_ecc)
    {
      snprintf (comment, sizeof (comment), "%s %s", main_input_filename,
		current_function_name ());
      write_alias_graph_to_ascii_ecc (file_ecc, comment, drs);
      fclose (file_ecc);
    }

  file_dot = fopen ("/tmp/dr_alias_graph_dot", "ab");
  if (file_dot)
    {
      snprintf (comment, sizeof (comment), "%s %s", main_input_filename,
		current_function_name ());
      write_alias_graph_to_ascii_dot (file_dot, comment, drs);
      fclose (file_dot);
    }
}

/* Build data references in SCOP.  */

static void
build_scop_drs (scop_p scop)
{
  int i, j;
  poly_bb_p pbb;
  data_reference_p dr;
  vec<data_reference_p> drs;
  drs.create (3);

  /* Remove all the PBBs that do not have data references: these basic
     blocks are not handled in the polyhedral representation.  */
  for (i = 0; SCOP_BBS (scop).iterate (i, &pbb); i++)
    if (GBB_DATA_REFS (PBB_BLACK_BOX (pbb)).is_empty ())
      {
	free_gimple_bb (PBB_BLACK_BOX (pbb));
	free_poly_bb (pbb);
	SCOP_BBS (scop).ordered_remove (i);
	i--;
      }

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    for (j = 0; GBB_DATA_REFS (PBB_BLACK_BOX (pbb)).iterate (j, &dr); j++)
      drs.safe_push (dr);

  FOR_EACH_VEC_ELT (drs, i, dr)
    dr->aux = XNEW (base_alias_pair);

  if (!build_alias_set_optimal_p (drs))
    {
      /* TODO: Add support when building alias set is not optimal.  */
      ;
    }

  build_base_obj_set_for_drs (drs);

  /* When debugging, enable the following code.  This cannot be used
     in production compilers.  */
  if (0)
    dump_alias_graphs (drs);

  drs.release ();

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    build_pbb_drs (pbb);
}

/* Return a gsi at the position of the phi node STMT.  */

static gimple_stmt_iterator
gsi_for_phi_node (gimple stmt)
{
  gimple_stmt_iterator psi;
  basic_block bb = gimple_bb (stmt);

  for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
    if (stmt == gsi_stmt (psi))
      return psi;

  gcc_unreachable ();
  return psi;
}

/* Analyze all the data references of STMTS and add them to the
   GBB_DATA_REFS vector of BB.  */

static void
analyze_drs_in_stmts (scop_p scop, basic_block bb, vec<gimple> stmts)
{
  loop_p nest;
  gimple_bb_p gbb;
  gimple stmt;
  int i;
  sese region = SCOP_REGION (scop);

  if (!bb_in_sese_p (bb, region))
    return;

  nest = outermost_loop_in_sese_1 (region, bb);
  gbb = gbb_from_bb (bb);

  FOR_EACH_VEC_ELT (stmts, i, stmt)
    {
      loop_p loop;

      if (is_gimple_debug (stmt))
	continue;

      loop = loop_containing_stmt (stmt);
      if (!loop_in_sese_p (loop, region))
	loop = nest;

      graphite_find_data_references_in_stmt (nest, loop, stmt,
					     &GBB_DATA_REFS (gbb));
    }
}

/* Insert STMT at the end of the STMTS sequence and then insert the
   statements from STMTS at INSERT_GSI and call analyze_drs_in_stmts
   on STMTS.  */

static void
insert_stmts (scop_p scop, gimple stmt, gimple_seq stmts,
	      gimple_stmt_iterator insert_gsi)
{
  gimple_stmt_iterator gsi;
  vec<gimple> x;
  x.create (3);

  gimple_seq_add_stmt (&stmts, stmt);
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    x.safe_push (gsi_stmt (gsi));

  gsi_insert_seq_before (&insert_gsi, stmts, GSI_SAME_STMT);
  analyze_drs_in_stmts (scop, gsi_bb (insert_gsi), x);
  x.release ();
}

/* Insert the assignment "RES := EXPR" just after AFTER_STMT.  */

static void
insert_out_of_ssa_copy (scop_p scop, tree res, tree expr, gimple after_stmt)
{
  gimple_seq stmts;
  gimple_stmt_iterator gsi;
  tree var = force_gimple_operand (expr, &stmts, true, NULL_TREE);
  gimple stmt = gimple_build_assign (unshare_expr (res), var);
  vec<gimple> x;
  x.create (3);

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
  x.release ();
}

/* Creates a poly_bb_p for basic_block BB from the existing PBB.  */

static void
new_pbb_from_pbb (scop_p scop, poly_bb_p pbb, basic_block bb)
{
  vec<data_reference_p> drs;
  drs.create (3);
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
  gimple_bb_p gbb1 = new_gimple_bb (bb, drs);
  poly_bb_p pbb1 = new_poly_bb (scop, gbb1);
  int index, n = SCOP_BBS (scop).length ();

  /* The INDEX of PBB in SCOP_BBS.  */
  for (index = 0; index < n; index++)
    if (SCOP_BBS (scop)[index] == pbb)
      break;

  pbb1->domain = isl_set_copy (pbb->domain);

  GBB_PBB (gbb1) = pbb1;
  GBB_CONDITIONS (gbb1) = GBB_CONDITIONS (gbb).copy ();
  GBB_CONDITION_CASES (gbb1) = GBB_CONDITION_CASES (gbb).copy ();
  SCOP_BBS (scop).safe_insert (index + 1, pbb1);
}

/* Insert on edge E the assignment "RES := EXPR".  */

static void
insert_out_of_ssa_copy_on_edge (scop_p scop, edge e, tree res, tree expr)
{
  gimple_stmt_iterator gsi;
  gimple_seq stmts = NULL;
  tree var = force_gimple_operand (expr, &stmts, true, NULL_TREE);
  gimple stmt = gimple_build_assign (unshare_expr (res), var);
  basic_block bb;
  vec<gimple> x;
  x.create (3);

  gimple_seq_add_stmt (&stmts, stmt);
  for (gsi = gsi_start (stmts); !gsi_end_p (gsi); gsi_next (&gsi))
    x.safe_push (gsi_stmt (gsi));

  gsi_insert_seq_on_edge (e, stmts);
  gsi_commit_edge_inserts ();
  bb = gimple_bb (stmt);

  if (!bb_in_sese_p (bb, SCOP_REGION (scop)))
    return;

  if (!gbb_from_bb (bb))
    new_pbb_from_pbb (scop, pbb_from_bb (e->src), bb);

  analyze_drs_in_stmts (scop, bb, x);
  x.release ();
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
scalar_close_phi_node_p (gimple phi)
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
propagate_expr_outside_region (tree def, tree expr, sese region)
{
  imm_use_iterator imm_iter;
  gimple use_stmt;
  gimple_seq stmts;
  bool replaced_once = false;

  gcc_assert (TREE_CODE (def) == SSA_NAME);

  expr = force_gimple_operand (unshare_expr (expr), &stmts, true,
			       NULL_TREE);

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
      gsi_insert_seq_on_edge (SESE_ENTRY (region), stmts);
      gsi_commit_edge_inserts ();
    }
}

/* Rewrite out of SSA the reduction phi node at PSI by creating a zero
   dimension array for it.  */

static void
rewrite_close_phi_out_of_ssa (scop_p scop, gimple_stmt_iterator *psi)
{
  sese region = SCOP_REGION (scop);
  gimple phi = gsi_stmt (*psi);
  tree res = gimple_phi_result (phi);
  basic_block bb = gimple_bb (phi);
  gimple_stmt_iterator gsi = gsi_after_labels (bb);
  tree arg = gimple_phi_arg_def (phi, 0);
  gimple stmt;

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
      SSA_NAME_DEF_STMT (res) = stmt;
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
rewrite_phi_out_of_ssa (scop_p scop, gimple_stmt_iterator *psi)
{
  size_t i;
  gimple phi = gsi_stmt (*psi);
  basic_block bb = gimple_bb (phi);
  tree res = gimple_phi_result (phi);
  tree zero_dim_array = create_zero_dim_array (res, "phi_out_of_ssa");
  gimple stmt;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);
      edge e = gimple_phi_arg_edge (phi, i);

      /* Avoid the insertion of code in the loop latch to please the
	 pattern matching of the vectorizer.  */
      if (TREE_CODE (arg) == SSA_NAME
	  && e->src == bb->loop_father->latch)
	insert_out_of_ssa_copy (scop, zero_dim_array, arg,
				SSA_NAME_DEF_STMT (arg));
      else
	insert_out_of_ssa_copy_on_edge (scop, e, zero_dim_array, arg);
    }

  stmt = gimple_build_assign (res, unshare_expr (zero_dim_array));
  remove_phi_node (psi, false);
  SSA_NAME_DEF_STMT (res) = stmt;
  insert_stmts (scop, stmt, NULL, gsi_after_labels (bb));
}

/* Rewrite the degenerate phi node at position PSI from the degenerate
   form "x = phi (y, y, ..., y)" to "x = y".  */

static void
rewrite_degenerate_phi (gimple_stmt_iterator *psi)
{
  tree rhs;
  gimple stmt;
  gimple_stmt_iterator gsi;
  gimple phi = gsi_stmt (*psi);
  tree res = gimple_phi_result (phi);
  basic_block bb;

  bb = gimple_bb (phi);
  rhs = degenerate_phi_result (phi);
  gcc_assert (rhs);

  stmt = gimple_build_assign (res, rhs);
  remove_phi_node (psi, false);
  SSA_NAME_DEF_STMT (res) = stmt;

  gsi = gsi_after_labels (bb);
  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
}

/* Rewrite out of SSA all the reduction phi nodes of SCOP.  */

static void
rewrite_reductions_out_of_ssa (scop_p scop)
{
  basic_block bb;
  gimple_stmt_iterator psi;
  sese region = SCOP_REGION (scop);

  FOR_EACH_BB (bb)
    if (bb_in_sese_p (bb, region))
      for (psi = gsi_start_phis (bb); !gsi_end_p (psi);)
	{
	  gimple phi = gsi_stmt (psi);

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

	  else if (reduction_phi_p (region, &psi))
	    rewrite_phi_out_of_ssa (scop, &psi);
	}

  update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa (true);
#endif
}

/* Rewrite the scalar dependence of DEF used in USE_STMT with a memory
   read from ZERO_DIM_ARRAY.  */

static void
rewrite_cross_bb_scalar_dependence (scop_p scop, tree zero_dim_array,
				    tree def, gimple use_stmt)
{
  gimple name_stmt;
  tree name;
  ssa_op_iter iter;
  use_operand_p use_p;

  gcc_assert (gimple_code (use_stmt) != GIMPLE_PHI);

  name = copy_ssa_name (def, NULL);
  name_stmt = gimple_build_assign (name, zero_dim_array);

  gimple_assign_set_lhs (name_stmt, name);
  insert_stmts (scop, name_stmt, NULL, gsi_for_stmt (use_stmt));

  FOR_EACH_SSA_USE_OPERAND (use_p, use_stmt, iter, SSA_OP_ALL_USES)
    if (operand_equal_p (def, USE_FROM_PTR (use_p), 0))
      replace_exp (use_p, name);

  update_stmt (use_stmt);
}

/* For every definition DEF in the SCOP that is used outside the scop,
   insert a closing-scop definition in the basic block just after this
   SCOP.  */

static void
handle_scalar_deps_crossing_scop_limits (scop_p scop, tree def, gimple stmt)
{
  tree var = create_tmp_reg (TREE_TYPE (def), NULL);
  tree new_name = make_ssa_name (var, stmt);
  bool needs_copy = false;
  use_operand_p use_p;
  imm_use_iterator imm_iter;
  gimple use_stmt;
  sese region = SCOP_REGION (scop);

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    {
      if (!bb_in_sese_p (gimple_bb (use_stmt), region))
	{
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
      gimple assign = gimple_build_assign (new_name, def);
      gimple_stmt_iterator psi = gsi_after_labels (SESE_EXIT (region)->dest);

      SSA_NAME_DEF_STMT (new_name) = assign;
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
  sese region = SCOP_REGION (scop);
  gimple stmt = gsi_stmt (*gsi);
  imm_use_iterator imm_iter;
  tree def;
  basic_block def_bb;
  tree zero_dim_array = NULL_TREE;
  gimple use_stmt;
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

  def_bb = gimple_bb (stmt);

  handle_scalar_deps_crossing_scop_limits (scop, def, stmt);

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    if (gimple_code (use_stmt) == GIMPLE_PHI
	&& (res = true))
      {
	gimple_stmt_iterator psi = gsi_for_stmt (use_stmt);

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

	rewrite_cross_bb_scalar_dependence (scop, zero_dim_array,
					    def, use_stmt);
      }

  return res;
}

/* Rewrite out of SSA all the reduction phi nodes of SCOP.  */

static void
rewrite_cross_bb_scalar_deps_out_of_ssa (scop_p scop)
{
  basic_block bb;
  gimple_stmt_iterator psi;
  sese region = SCOP_REGION (scop);
  bool changed = false;

  /* Create an extra empty BB after the scop.  */
  split_edge (SESE_EXIT (region));

  FOR_EACH_BB (bb)
    if (bb_in_sese_p (bb, region))
      for (psi = gsi_start_bb (bb); !gsi_end_p (psi); gsi_next (&psi))
	changed |= rewrite_cross_bb_scalar_deps (scop, &psi);

  if (changed)
    {
      scev_reset_htab ();
      update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
      verify_loop_closed_ssa (true);
#endif
    }
}

/* Returns the number of pbbs that are in loops contained in SCOP.  */

static int
nb_pbbs_in_loops (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int res = 0;

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    if (loop_in_sese_p (gbb_loop (PBB_BLACK_BOX (pbb)), SCOP_REGION (scop)))
      res++;

  return res;
}

/* Return the number of data references in BB that write in
   memory.  */

static int
nb_data_writes_in_bb (basic_block bb)
{
  int res = 0;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    if (gimple_vdef (gsi_stmt (gsi)))
      res++;

  return res;
}

/* Splits at STMT the basic block BB represented as PBB in the
   polyhedral form.  */

static edge
split_pbb (scop_p scop, poly_bb_p pbb, basic_block bb, gimple stmt)
{
  edge e1 = split_block (bb, stmt);
  new_pbb_from_pbb (scop, pbb, e1->dest);
  return e1;
}

/* Splits STMT out of its current BB.  This is done for reduction
   statements for which we want to ignore data dependences.  */

static basic_block
split_reduction_stmt (scop_p scop, gimple stmt)
{
  basic_block bb = gimple_bb (stmt);
  poly_bb_p pbb = pbb_from_bb (bb);
  gimple_bb_p gbb = gbb_from_bb (bb);
  edge e1;
  int i;
  data_reference_p dr;

  /* Do not split basic blocks with no writes to memory: the reduction
     will be the only write to memory.  */
  if (nb_data_writes_in_bb (bb) == 0
      /* Or if we have already marked BB as a reduction.  */
      || PBB_IS_REDUCTION (pbb_from_bb (bb)))
    return bb;

  e1 = split_pbb (scop, pbb, bb, stmt);

  /* Split once more only when the reduction stmt is not the only one
     left in the original BB.  */
  if (!gsi_one_before_end_p (gsi_start_nondebug_bb (bb)))
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      gsi_prev (&gsi);
      e1 = split_pbb (scop, pbb, bb, gsi_stmt (gsi));
    }

  /* A part of the data references will end in a different basic block
     after the split: move the DRs from the original GBB to the newly
     created GBB1.  */
  FOR_EACH_VEC_ELT (GBB_DATA_REFS (gbb), i, dr)
    {
      basic_block bb1 = gimple_bb (DR_STMT (dr));

      if (bb1 != bb)
	{
	  gimple_bb_p gbb1 = gbb_from_bb (bb1);
	  GBB_DATA_REFS (gbb1).safe_push (dr);
	  GBB_DATA_REFS (gbb).ordered_remove (i);
	  i--;
	}
    }

  return e1->dest;
}

/* Return true when stmt is a reduction operation.  */

static inline bool
is_reduction_operation_p (gimple stmt)
{
  enum tree_code code;

  gcc_assert (is_gimple_assign (stmt));
  code = gimple_assign_rhs_code (stmt);

  return flag_associative_math
    && commutative_tree_code (code)
    && associative_tree_code (code);
}

/* Returns true when PHI contains an argument ARG.  */

static bool
phi_contains_arg (gimple phi, tree arg)
{
  size_t i;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (operand_equal_p (arg, gimple_phi_arg_def (phi, i), 0))
      return true;

  return false;
}

/* Return a loop phi node that corresponds to a reduction containing LHS.  */

static gimple
follow_ssa_with_commutative_ops (tree arg, tree lhs)
{
  gimple stmt;

  if (TREE_CODE (arg) != SSA_NAME)
    return NULL;

  stmt = SSA_NAME_DEF_STMT (arg);

  if (gimple_code (stmt) == GIMPLE_NOP
      || gimple_code (stmt) == GIMPLE_CALL)
    return NULL;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      if (phi_contains_arg (stmt, lhs))
	return stmt;
      return NULL;
    }

  if (!is_gimple_assign (stmt))
    return NULL;

  if (gimple_num_ops (stmt) == 2)
    return follow_ssa_with_commutative_ops (gimple_assign_rhs1 (stmt), lhs);

  if (is_reduction_operation_p (stmt))
    {
      gimple res = follow_ssa_with_commutative_ops (gimple_assign_rhs1 (stmt), lhs);

      return res ? res :
	follow_ssa_with_commutative_ops (gimple_assign_rhs2 (stmt), lhs);
    }

  return NULL;
}

/* Detect commutative and associative scalar reductions starting at
   the STMT.  Return the phi node of the reduction cycle, or NULL.  */

static gimple
detect_commutative_reduction_arg (tree lhs, gimple stmt, tree arg,
				  vec<gimple> *in,
				  vec<gimple> *out)
{
  gimple phi = follow_ssa_with_commutative_ops (arg, lhs);

  if (!phi)
    return NULL;

  in->safe_push (stmt);
  out->safe_push (stmt);
  return phi;
}

/* Detect commutative and associative scalar reductions starting at
   STMT.  Return the phi node of the reduction cycle, or NULL.  */

static gimple
detect_commutative_reduction_assign (gimple stmt, vec<gimple> *in,
				     vec<gimple> *out)
{
  tree lhs = gimple_assign_lhs (stmt);

  if (gimple_num_ops (stmt) == 2)
    return detect_commutative_reduction_arg (lhs, stmt,
					     gimple_assign_rhs1 (stmt),
					     in, out);

  if (is_reduction_operation_p (stmt))
    {
      gimple res = detect_commutative_reduction_arg (lhs, stmt,
						     gimple_assign_rhs1 (stmt),
						     in, out);
      return res ? res
	: detect_commutative_reduction_arg (lhs, stmt,
					    gimple_assign_rhs2 (stmt),
					    in, out);
    }

  return NULL;
}

/* Return a loop phi node that corresponds to a reduction containing LHS.  */

static gimple
follow_inital_value_to_phi (tree arg, tree lhs)
{
  gimple stmt;

  if (!arg || TREE_CODE (arg) != SSA_NAME)
    return NULL;

  stmt = SSA_NAME_DEF_STMT (arg);

  if (gimple_code (stmt) == GIMPLE_PHI
      && phi_contains_arg (stmt, lhs))
    return stmt;

  return NULL;
}


/* Return the argument of the loop PHI that is the initial value coming
   from outside the loop.  */

static edge
edge_initial_value_for_loop_phi (gimple phi)
{
  size_t i;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      edge e = gimple_phi_arg_edge (phi, i);

      if (loop_depth (e->src->loop_father)
	  < loop_depth (e->dest->loop_father))
	return e;
    }

  return NULL;
}

/* Return the argument of the loop PHI that is the initial value coming
   from outside the loop.  */

static tree
initial_value_for_loop_phi (gimple phi)
{
  size_t i;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      edge e = gimple_phi_arg_edge (phi, i);

      if (loop_depth (e->src->loop_father)
	  < loop_depth (e->dest->loop_father))
	return gimple_phi_arg_def (phi, i);
    }

  return NULL_TREE;
}

/* Returns true when DEF is used outside the reduction cycle of
   LOOP_PHI.  */

static bool
used_outside_reduction (tree def, gimple loop_phi)
{
  use_operand_p use_p;
  imm_use_iterator imm_iter;
  loop_p loop = loop_containing_stmt (loop_phi);

  /* In LOOP, DEF should be used only in LOOP_PHI.  */
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, def)
    {
      gimple stmt = USE_STMT (use_p);

      if (stmt != loop_phi
	  && !is_gimple_debug (stmt)
	  && flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	return true;
    }

  return false;
}

/* Detect commutative and associative scalar reductions belonging to
   the SCOP starting at the loop closed phi node STMT.  Return the phi
   node of the reduction cycle, or NULL.  */

static gimple
detect_commutative_reduction (scop_p scop, gimple stmt, vec<gimple> *in,
			      vec<gimple> *out)
{
  if (scalar_close_phi_node_p (stmt))
    {
      gimple def, loop_phi, phi, close_phi = stmt;
      tree init, lhs, arg = gimple_phi_arg_def (close_phi, 0);

      if (TREE_CODE (arg) != SSA_NAME)
	return NULL;

      /* Note that loop close phi nodes should have a single argument
	 because we translated the representation into a canonical form
	 before Graphite: see canonicalize_loop_closed_ssa_form.  */
      gcc_assert (gimple_phi_num_args (close_phi) == 1);

      def = SSA_NAME_DEF_STMT (arg);
      if (!stmt_in_sese_p (def, SCOP_REGION (scop))
	  || !(loop_phi = detect_commutative_reduction (scop, def, in, out)))
	return NULL;

      lhs = gimple_phi_result (close_phi);
      init = initial_value_for_loop_phi (loop_phi);
      phi = follow_inital_value_to_phi (init, lhs);

      if (phi && (used_outside_reduction (lhs, phi)
		  || !has_single_use (gimple_phi_result (phi))))
	return NULL;

      in->safe_push (loop_phi);
      out->safe_push (close_phi);
      return phi;
    }

  if (gimple_code (stmt) == GIMPLE_ASSIGN)
    return detect_commutative_reduction_assign (stmt, in, out);

  return NULL;
}

/* Translate the scalar reduction statement STMT to an array RED
   knowing that its recursive phi node is LOOP_PHI.  */

static void
translate_scalar_reduction_to_array_for_stmt (scop_p scop, tree red,
					      gimple stmt, gimple loop_phi)
{
  tree res = gimple_phi_result (loop_phi);
  gimple assign = gimple_build_assign (res, unshare_expr (red));
  gimple_stmt_iterator gsi;

  insert_stmts (scop, assign, NULL, gsi_after_labels (gimple_bb (loop_phi)));

  assign = gimple_build_assign (unshare_expr (red), gimple_assign_lhs (stmt));
  gsi = gsi_for_stmt (stmt);
  gsi_next (&gsi);
  insert_stmts (scop, assign, NULL, gsi);
}

/* Removes the PHI node and resets all the debug stmts that are using
   the PHI_RESULT.  */

static void
remove_phi (gimple phi)
{
  imm_use_iterator imm_iter;
  tree def;
  use_operand_p use_p;
  gimple_stmt_iterator gsi;
  vec<gimple> update;
  update.create (3);
  unsigned int i;
  gimple stmt;

  def = PHI_RESULT (phi);
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, def)
    {
      stmt = USE_STMT (use_p);

      if (is_gimple_debug (stmt))
	{
	  gimple_debug_bind_reset_value (stmt);
	  update.safe_push (stmt);
	}
    }

  FOR_EACH_VEC_ELT (update, i, stmt)
    update_stmt (stmt);

  update.release ();

  gsi = gsi_for_phi_node (phi);
  remove_phi_node (&gsi, false);
}

/* Helper function for for_each_index.  For each INDEX of the data
   reference REF, returns true when its indices are valid in the loop
   nest LOOP passed in as DATA.  */

static bool
dr_indices_valid_in_loop (tree ref ATTRIBUTE_UNUSED, tree *index, void *data)
{
  loop_p loop;
  basic_block header, def_bb;
  gimple stmt;

  if (TREE_CODE (*index) != SSA_NAME)
    return true;

  loop = *((loop_p *) data);
  header = loop->header;
  stmt = SSA_NAME_DEF_STMT (*index);

  if (!stmt)
    return true;

  def_bb = gimple_bb (stmt);

  if (!def_bb)
    return true;

  return dominated_by_p (CDI_DOMINATORS, header, def_bb);
}

/* When the result of a CLOSE_PHI is written to a memory location,
   return a pointer to that memory reference, otherwise return
   NULL_TREE.  */

static tree
close_phi_written_to_memory (gimple close_phi)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  gimple stmt;
  tree res, def = gimple_phi_result (close_phi);

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, def)
    if ((stmt = USE_STMT (use_p))
	&& gimple_code (stmt) == GIMPLE_ASSIGN
	&& (res = gimple_assign_lhs (stmt)))
      {
	switch (TREE_CODE (res))
	  {
	  case VAR_DECL:
	  case PARM_DECL:
	  case RESULT_DECL:
	    return res;

	  case ARRAY_REF:
	  case MEM_REF:
	    {
	      tree arg = gimple_phi_arg_def (close_phi, 0);
	      loop_p nest = loop_containing_stmt (SSA_NAME_DEF_STMT (arg));

	      /* FIXME: this restriction is for id-{24,25}.f and
		 could be handled by duplicating the computation of
		 array indices before the loop of the close_phi.  */
	      if (for_each_index (&res, dr_indices_valid_in_loop, &nest))
		return res;
	    }
	    /* Fallthru.  */

	  default:
	    continue;
	  }
      }
  return NULL_TREE;
}

/* Rewrite out of SSA the reduction described by the loop phi nodes
   IN, and the close phi nodes OUT.  IN and OUT are structured by loop
   levels like this:

   IN: stmt, loop_n, ..., loop_0
   OUT: stmt, close_n, ..., close_0

   the first element is the reduction statement, and the next elements
   are the loop and close phi nodes of each of the outer loops.  */

static void
translate_scalar_reduction_to_array (scop_p scop,
				     vec<gimple> in,
				     vec<gimple> out)
{
  gimple loop_phi;
  unsigned int i = out.length () - 1;
  tree red = close_phi_written_to_memory (out[i]);

  FOR_EACH_VEC_ELT (in, i, loop_phi)
    {
      gimple close_phi = out[i];

      if (i == 0)
	{
	  gimple stmt = loop_phi;
	  basic_block bb = split_reduction_stmt (scop, stmt);
	  poly_bb_p pbb = pbb_from_bb (bb);
	  PBB_IS_REDUCTION (pbb) = true;
	  gcc_assert (close_phi == loop_phi);

	  if (!red)
	    red = create_zero_dim_array
	      (gimple_assign_lhs (stmt), "Commutative_Associative_Reduction");

	  translate_scalar_reduction_to_array_for_stmt (scop, red, stmt, in[1]);
	  continue;
	}

      if (i == in.length () - 1)
	{
	  insert_out_of_ssa_copy (scop, gimple_phi_result (close_phi),
				  unshare_expr (red), close_phi);
	  insert_out_of_ssa_copy_on_edge
	    (scop, edge_initial_value_for_loop_phi (loop_phi),
	     unshare_expr (red), initial_value_for_loop_phi (loop_phi));
	}

      remove_phi (loop_phi);
      remove_phi (close_phi);
    }
}

/* Rewrites out of SSA a commutative reduction at CLOSE_PHI.  Returns
   true when something has been changed.  */

static bool
rewrite_commutative_reductions_out_of_ssa_close_phi (scop_p scop,
						     gimple close_phi)
{
  bool res;
  vec<gimple> in;
  in.create (10);
  vec<gimple> out;
  out.create (10);

  detect_commutative_reduction (scop, close_phi, &in, &out);
  res = in.length () > 1;
  if (res)
    translate_scalar_reduction_to_array (scop, in, out);

  in.release ();
  out.release ();
  return res;
}

/* Rewrites all the commutative reductions from LOOP out of SSA.
   Returns true when something has been changed.  */

static bool
rewrite_commutative_reductions_out_of_ssa_loop (scop_p scop,
						loop_p loop)
{
  gimple_stmt_iterator gsi;
  edge exit = single_exit (loop);
  tree res;
  bool changed = false;

  if (!exit)
    return false;

  for (gsi = gsi_start_phis (exit->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    if ((res = gimple_phi_result (gsi_stmt (gsi)))
	&& !virtual_operand_p (res)
	&& !scev_analyzable_p (res, SCOP_REGION (scop)))
      changed |= rewrite_commutative_reductions_out_of_ssa_close_phi
	(scop, gsi_stmt (gsi));

  return changed;
}

/* Rewrites all the commutative reductions from SCOP out of SSA.  */

static void
rewrite_commutative_reductions_out_of_ssa (scop_p scop)
{
  loop_iterator li;
  loop_p loop;
  bool changed = false;
  sese region = SCOP_REGION (scop);

  FOR_EACH_LOOP (li, loop, 0)
    if (loop_in_sese_p (loop, region))
      changed |= rewrite_commutative_reductions_out_of_ssa_loop (scop, loop);

  if (changed)
    {
      scev_reset_htab ();
      gsi_commit_edge_inserts ();
      update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
      verify_loop_closed_ssa (true);
#endif
    }
}

/* Can all ivs be represented by a signed integer?
   As CLooG might generate negative values in its expressions, signed loop ivs
   are required in the backend. */

static bool
scop_ivs_can_be_represented (scop_p scop)
{
  loop_iterator li;
  loop_p loop;
  gimple_stmt_iterator psi;
  bool result = true;

  FOR_EACH_LOOP (li, loop, 0)
    {
      if (!loop_in_sese_p (loop, SCOP_REGION (scop)))
	continue;

      for (psi = gsi_start_phis (loop->header);
	   !gsi_end_p (psi); gsi_next (&psi))
	{
	  gimple phi = gsi_stmt (psi);
	  tree res = PHI_RESULT (phi);
	  tree type = TREE_TYPE (res);

	  if (TYPE_UNSIGNED (type)
	      && TYPE_PRECISION (type) >= TYPE_PRECISION (long_long_integer_type_node))
	    {
	      result = false;
	      break;
	    }
	}
      if (!result)
	FOR_EACH_LOOP_BREAK (li);
    }

  return result;
}

/* Builds the polyhedral representation for a SESE region.  */

void
build_poly_scop (scop_p scop)
{
  sese region = SCOP_REGION (scop);
  graphite_dim_t max_dim;

  build_scop_bbs (scop);

  /* FIXME: This restriction is needed to avoid a problem in CLooG.
     Once CLooG is fixed, remove this guard.  Anyways, it makes no
     sense to optimize a scop containing only PBBs that do not belong
     to any loops.  */
  if (nb_pbbs_in_loops (scop) == 0)
    return;

  if (!scop_ivs_can_be_represented (scop))
    return;

  if (flag_associative_math)
    rewrite_commutative_reductions_out_of_ssa (scop);

  build_sese_loop_nests (region);
  /* Record all conditions in REGION.  */
  sese_dom_walker (CDI_DOMINATORS, region).walk (cfun->cfg->x_entry_block_ptr);
  find_scop_parameters (scop);

  max_dim = PARAM_VALUE (PARAM_GRAPHITE_MAX_NB_SCOP_PARAMS);
  if (scop_nb_params (scop) > max_dim)
    return;

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
  scop_to_lst (scop);
  build_scop_scattering (scop);

  /* This SCoP has been translated to the polyhedral
     representation.  */
  POLY_SCOP_P (scop) = true;
}
#endif
