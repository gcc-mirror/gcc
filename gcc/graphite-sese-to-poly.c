/* Conversion of SESE regions to Polyhedra.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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
#include "graphite-sese-to-poly.h"

/* Check if VAR is used in a phi node, that is no loop header.  */

static bool
var_used_in_not_loop_header_phi_node (tree var)
{
  imm_use_iterator imm_iter;
  gimple stmt;
  bool result = false;

  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, var)
    {
      basic_block bb = gimple_bb (stmt);

      if (gimple_code (stmt) == GIMPLE_PHI
	  && bb->loop_father->header != bb)
	result = true;
    }

  return result;
}

/* Returns the index of the phi argument corresponding to the initial
   value in the loop.  */

static size_t
loop_entry_phi_arg (gimple phi)
{
  loop_p loop = gimple_bb (phi)->loop_father;
  size_t i;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (!flow_bb_inside_loop_p (loop, gimple_phi_arg_edge (phi, i)->src))
      return i;

  gcc_unreachable ();
  return 0;
}

/* Removes a simple copy phi node "RES = phi (INIT, RES)" at position
   PSI by inserting on the loop ENTRY edge assignment "RES = INIT".  */

static void
remove_simple_copy_phi (gimple_stmt_iterator *psi)
{
  gimple phi = gsi_stmt (*psi);
  tree res = gimple_phi_result (phi);
  size_t entry = loop_entry_phi_arg (phi);
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
  size_t entry = loop_entry_phi_arg (phi);
  edge e = gimple_phi_arg_edge (phi, entry);
  tree var;
  gimple stmt;
  gimple_seq stmts;
  gimple_stmt_iterator gsi;

  if (tree_contains_chrecs (scev, NULL))
    scev = gimple_phi_arg_def (phi, entry);

  var = force_gimple_operand (scev, &stmts, true, NULL_TREE);
  stmt = gimple_build_assign (res, var);
  remove_phi_node (psi, false);

  if (!stmts)
    stmts = gimple_seq_alloc ();

  gsi = gsi_last (stmts);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
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
  tree scev;
  affine_iv iv;
  gimple phi = gsi_stmt (*psi);
  tree res = gimple_phi_result (phi);

  if (!is_gimple_reg (res))
    {
      gsi_next (psi);
      return false;
    }

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

  /* Main induction variables with constant strides in LOOP are not
     reductions.  */
  if (simple_iv (loop, loop, res, &iv, true))
    {
      if (integer_zerop (iv.step))
	remove_invariant_phi (region, psi);
      else
	gsi_next (psi);

      return false;
    }

  scev = scalar_evolution_in_region (region, loop, res);
  if (chrec_contains_undetermined (scev))
    return true;

  if (evolution_function_is_invariant_p (scev, loop->num))
    {
      remove_invariant_phi (region, psi);
      return false;
    }

  /* All the other cases are considered reductions.  */
  return true;
}

/* Returns true when BB will be represented in graphite.  Return false
   for the basic blocks that contain code eliminated in the code
   generation pass: i.e. induction variables and exit conditions.  */

static bool
graphite_stmt_p (sese region, basic_block bb,
		 VEC (data_reference_p, heap) *drs)
{
  gimple_stmt_iterator gsi;
  loop_p loop = bb->loop_father;

  if (VEC_length (data_reference_p, drs) > 0)
    return true;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      switch (gimple_code (stmt))
        {
	case GIMPLE_DEBUG:
          /* Control flow expressions can be ignored, as they are
             represented in the iteration domains and will be
             regenerated by graphite.  */
	case GIMPLE_COND:
	case GIMPLE_GOTO:
	case GIMPLE_SWITCH:
	  break;

	case GIMPLE_ASSIGN:
	  {
	    tree var = gimple_assign_lhs (stmt);

	    /* We need these bbs to be able to construct the phi nodes.  */
	    if (var_used_in_not_loop_header_phi_node (var))
	      return true;

	    var = scalar_evolution_in_region (region, loop, var);
	    if (chrec_contains_undetermined (var))
	      return true;

	    break;
	  }

	default:
	  return true;
        }
    }

  return false;
}

/* Store the GRAPHITE representation of BB.  */

static gimple_bb_p
new_gimple_bb (basic_block bb, VEC (data_reference_p, heap) *drs)
{
  struct gimple_bb *gbb;

  gbb = XNEW (struct gimple_bb);
  bb->aux = gbb;
  GBB_BB (gbb) = bb;
  GBB_DATA_REFS (gbb) = drs;
  GBB_CONDITIONS (gbb) = NULL;
  GBB_CONDITION_CASES (gbb) = NULL;
  GBB_CLOOG_IV_TYPES (gbb) = NULL;

  return gbb;
}

static void
free_data_refs_aux (VEC (data_reference_p, heap) *datarefs)
{
  unsigned int i;
  struct data_reference *dr;

  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    if (dr->aux)
      {
	base_alias_pair *bap = (base_alias_pair *)(dr->aux);

	if (bap->alias_set)
	  free (bap->alias_set);

	free (bap);
	dr->aux = NULL;
      }
}
/* Frees GBB.  */

static void
free_gimple_bb (struct gimple_bb *gbb)
{
  if (GBB_CLOOG_IV_TYPES (gbb))
    htab_delete (GBB_CLOOG_IV_TYPES (gbb));

  free_data_refs_aux (GBB_DATA_REFS (gbb));
  free_data_refs (GBB_DATA_REFS (gbb));

  VEC_free (gimple, heap, GBB_CONDITIONS (gbb));
  VEC_free (gimple, heap, GBB_CONDITION_CASES (gbb));
  GBB_BB (gbb)->aux = 0;
  XDELETE (gbb);
}

/* Deletes all gimple bbs in SCOP.  */

static void
remove_gbbs_in_scop (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    free_gimple_bb (PBB_BLACK_BOX (pbb));
}

/* Deletes all scops in SCOPS.  */

void
free_scops (VEC (scop_p, heap) *scops)
{
  int i;
  scop_p scop;

  for (i = 0; VEC_iterate (scop_p, scops, i, scop); i++)
    {
      remove_gbbs_in_scop (scop);
      free_sese (SCOP_REGION (scop));
      free_scop (scop);
    }

  VEC_free (scop_p, heap, scops);
}

/* Generates a polyhedral black box only if the bb contains interesting
   information.  */

static void
try_generate_gimple_bb (scop_p scop, basic_block bb, sbitmap reductions)
{
  VEC (data_reference_p, heap) *drs = VEC_alloc (data_reference_p, heap, 5);
  loop_p nest = outermost_loop_in_sese (SCOP_REGION (scop), bb);
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      if (!is_gimple_debug (stmt))
	graphite_find_data_references_in_stmt (nest, stmt, &drs);
    }

  if (!graphite_stmt_p (SCOP_REGION (scop), bb, drs))
    free_data_refs (drs);
  else
    new_poly_bb (scop, new_gimple_bb (bb, drs), TEST_BIT (reductions,
							  bb->index));
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
    if (!TEST_BIT (map, e->src->index)
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
graphite_sort_dominated_info (VEC (basic_block, heap) *dom)
{
  size_t len = VEC_length (basic_block, dom);

  qsort (VEC_address (basic_block, dom), len, sizeof (basic_block),
	 compare_bb_depths);
}

/* Recursive helper function for build_scops_bbs.  */

static void
build_scop_bbs_1 (scop_p scop, sbitmap visited, basic_block bb, sbitmap reductions)
{
  sese region = SCOP_REGION (scop);
  VEC (basic_block, heap) *dom;

  if (TEST_BIT (visited, bb->index)
      || !bb_in_sese_p (bb, region))
    return;

  try_generate_gimple_bb (scop, bb, reductions);
  SET_BIT (visited, bb->index);

  dom = get_dominated_by (CDI_DOMINATORS, bb);

  if (dom == NULL)
    return;

  graphite_sort_dominated_info (dom);

  while (!VEC_empty (basic_block, dom))
    {
      int i;
      basic_block dom_bb;

      for (i = 0; VEC_iterate (basic_block, dom, i, dom_bb); i++)
	if (all_non_dominated_preds_marked_p (dom_bb, visited))
	  {
	    build_scop_bbs_1 (scop, visited, dom_bb, reductions);
	    VEC_unordered_remove (basic_block, dom, i);
	    break;
	  }
    }

  VEC_free (basic_block, heap, dom);
}

/* Gather the basic blocks belonging to the SCOP.  */

static void
build_scop_bbs (scop_p scop, sbitmap reductions)
{
  sbitmap visited = sbitmap_alloc (last_basic_block);
  sese region = SCOP_REGION (scop);

  sbitmap_zero (visited);
  build_scop_bbs_1 (scop, visited, SESE_ENTRY_BB (region), reductions);
  sbitmap_free (visited);
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
build_pbb_scattering_polyhedrons (ppl_Linear_Expression_t static_schedule,
				  poly_bb_p pbb, int scattering_dimensions)
{
  int i;
  scop_p scop = PBB_SCOP (pbb);
  int nb_iterators = pbb_dim_iter_domain (pbb);
  int used_scattering_dimensions = nb_iterators * 2 + 1;
  int nb_params = scop_nb_params (scop);
  ppl_Coefficient_t c;
  ppl_dimension_type dim = scattering_dimensions + nb_iterators + nb_params;
  Value v;

  gcc_assert (scattering_dimensions >= used_scattering_dimensions);

  value_init (v);
  ppl_new_Coefficient (&c);
  PBB_TRANSFORMED (pbb) = poly_scattering_new ();
  ppl_new_C_Polyhedron_from_space_dimension
    (&PBB_TRANSFORMED_SCATTERING (pbb), dim, 0);

  PBB_NB_SCATTERING_TRANSFORM (pbb) = scattering_dimensions;

  for (i = 0; i < scattering_dimensions; i++)
    {
      ppl_Constraint_t cstr;
      ppl_Linear_Expression_t expr;

      ppl_new_Linear_Expression_with_dimension (&expr, dim);
      value_set_si (v, 1);
      ppl_assign_Coefficient_from_mpz_t (c, v);
      ppl_Linear_Expression_add_to_coefficient (expr, i, c);

      /* Textual order inside this loop.  */
      if ((i % 2) == 0)
	{
	  ppl_Linear_Expression_coefficient (static_schedule, i / 2, c);
	  ppl_Coefficient_to_mpz_t (c, v);
	  value_oppose (v, v);
	  ppl_assign_Coefficient_from_mpz_t (c, v);
	  ppl_Linear_Expression_add_to_inhomogeneous (expr, c);
	}

      /* Iterations of this loop.  */
      else /* if ((i % 2) == 1) */
	{
	  int loop = (i - 1) / 2;

	  value_set_si (v, -1);
	  ppl_assign_Coefficient_from_mpz_t (c, v);
	  ppl_Linear_Expression_add_to_coefficient
	    (expr, scattering_dimensions + loop, c);
	}

      ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_Polyhedron_add_constraint (PBB_TRANSFORMED_SCATTERING (pbb), cstr);
      ppl_delete_Linear_Expression (expr);
      ppl_delete_Constraint (cstr);
    }

  value_clear (v);
  ppl_delete_Coefficient (c);

  PBB_ORIGINAL (pbb) = poly_scattering_copy (PBB_TRANSFORMED (pbb));
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
  ppl_Linear_Expression_t static_schedule;
  ppl_Coefficient_t c;
  Value v;

  value_init (v);
  ppl_new_Coefficient (&c);
  ppl_new_Linear_Expression (&static_schedule);

  /* We have to start schedules at 0 on the first component and
     because we cannot compare_prefix_loops against a previous loop,
     prefix will be equal to zero, and that index will be
     incremented before copying.  */
  value_set_si (v, -1);
  ppl_assign_Coefficient_from_mpz_t (c, v);
  ppl_Linear_Expression_add_to_coefficient (static_schedule, 0, c);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    {
      gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
      ppl_Linear_Expression_t common;
      int prefix;
      int nb_scat_dims = pbb_dim_iter_domain (pbb) * 2 + 1;

      if (previous_gbb)
	prefix = nb_common_loops (SCOP_REGION (scop), previous_gbb, gbb);
      else
	prefix = 0;

      previous_gbb = gbb;
      ppl_new_Linear_Expression_with_dimension (&common, prefix + 1);
      ppl_assign_Linear_Expression_from_Linear_Expression (common,
							   static_schedule);

      value_set_si (v, 1);
      ppl_assign_Coefficient_from_mpz_t (c, v);
      ppl_Linear_Expression_add_to_coefficient (common, prefix, c);
      ppl_assign_Linear_Expression_from_Linear_Expression (static_schedule,
							   common);

      build_pbb_scattering_polyhedrons (common, pbb, nb_scat_dims);

      ppl_delete_Linear_Expression (common);
    }

  value_clear (v);
  ppl_delete_Coefficient (c);
  ppl_delete_Linear_Expression (static_schedule);
}

/* Add the value K to the dimension D of the linear expression EXPR.  */

static void
add_value_to_dim (ppl_dimension_type d, ppl_Linear_Expression_t expr,
		  Value k)
{
  Value val;
  ppl_Coefficient_t coef;

  ppl_new_Coefficient (&coef);
  ppl_Linear_Expression_coefficient (expr, d, coef);
  value_init (val);
  ppl_Coefficient_to_mpz_t (coef, val);

  value_addto (val, val, k);

  ppl_assign_Coefficient_from_mpz_t (coef, val);
  ppl_Linear_Expression_add_to_coefficient (expr, d, coef);
  value_clear (val);
  ppl_delete_Coefficient (coef);
}

/* In the context of scop S, scan E, the right hand side of a scalar
   evolution function in loop VAR, and translate it to a linear
   expression EXPR.  */

static void
scan_tree_for_params_right_scev (sese s, tree e, int var,
				 ppl_Linear_Expression_t expr)
{
  if (expr)
    {
      loop_p loop = get_loop (var);
      ppl_dimension_type l = sese_loop_depth (s, loop) - 1;
      Value val;

      /* Scalar evolutions should happen in the sese region.  */
      gcc_assert (sese_loop_depth (s, loop) > 0);

      /* We can not deal with parametric strides like:

      | p = parameter;
      |
      | for i:
      |   a [i * p] = ...   */
      gcc_assert (TREE_CODE (e) == INTEGER_CST);

      value_init (val);
      value_set_si (val, int_cst_value (e));
      add_value_to_dim (l, expr, val);
      value_clear (val);
    }
}

/* Scan the integer constant CST, and add it to the inhomogeneous part of the
   linear expression EXPR.  K is the multiplier of the constant.  */

static void
scan_tree_for_params_int (tree cst, ppl_Linear_Expression_t expr, Value k)
{
  Value val;
  ppl_Coefficient_t coef;
  int v = int_cst_value (cst);

  value_init (val);
  value_set_si (val, 0);

  /* Necessary to not get "-1 = 2^n - 1". */
  if (v < 0)
    value_sub_int (val, val, -v);
  else
    value_add_int (val, val, v);

  value_multiply (val, val, k);
  ppl_new_Coefficient (&coef);
  ppl_assign_Coefficient_from_mpz_t (coef, val);
  ppl_Linear_Expression_add_to_inhomogeneous (expr, coef);
  value_clear (val);
  ppl_delete_Coefficient (coef);
}

/* When parameter NAME is in REGION, returns its index in SESE_PARAMS.
   Otherwise returns -1.  */

static inline int
parameter_index_in_region_1 (tree name, sese region)
{
  int i;
  tree p;

  gcc_assert (TREE_CODE (name) == SSA_NAME);

  for (i = 0; VEC_iterate (tree, SESE_PARAMS (region), i, p); i++)
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

  i = VEC_length (tree, SESE_PARAMS (region));
  VEC_safe_push (tree, heap, SESE_PARAMS (region), name);
  return i;
}

/* In the context of sese S, scan the expression E and translate it to
   a linear expression C.  When parsing a symbolic multiplication, K
   represents the constant multiplier of an expression containing
   parameters.  */

static void
scan_tree_for_params (sese s, tree e, ppl_Linear_Expression_t c,
		      Value k)
{
  if (e == chrec_dont_know)
    return;

  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      scan_tree_for_params_right_scev (s, CHREC_RIGHT (e),
				       CHREC_VARIABLE (e), c);
      scan_tree_for_params (s, CHREC_LEFT (e), c, k);
      break;

    case MULT_EXPR:
      if (chrec_contains_symbols (TREE_OPERAND (e, 0)))
	{
	  if (c)
	    {
	      Value val;
	      gcc_assert (host_integerp (TREE_OPERAND (e, 1), 0));
	      value_init (val);
	      value_set_si (val, int_cst_value (TREE_OPERAND (e, 1)));
	      value_multiply (val, val, k);
	      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, val);
	      value_clear (val);
	    }
	  else
	    scan_tree_for_params (s, TREE_OPERAND (e, 0), c, k);
	}
      else
	{
	  if (c)
	    {
	      Value val;
	      gcc_assert (host_integerp (TREE_OPERAND (e, 0), 0));
	      value_init (val);
	      value_set_si (val, int_cst_value (TREE_OPERAND (e, 0)));
	      value_multiply (val, val, k);
	      scan_tree_for_params (s, TREE_OPERAND (e, 1), c, val);
	      value_clear (val);
	    }
	  else
	    scan_tree_for_params (s, TREE_OPERAND (e, 1), c, k);
	}
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, k);
      scan_tree_for_params (s, TREE_OPERAND (e, 1), c, k);
      break;

    case MINUS_EXPR:
      {
	ppl_Linear_Expression_t tmp_expr = NULL;

        if (c)
	  {
	    ppl_dimension_type dim;
	    ppl_Linear_Expression_space_dimension (c, &dim);
	    ppl_new_Linear_Expression_with_dimension (&tmp_expr, dim);
	  }

	scan_tree_for_params (s, TREE_OPERAND (e, 0), c, k);
	scan_tree_for_params (s, TREE_OPERAND (e, 1), tmp_expr, k);

	if (c)
	  {
	    ppl_subtract_Linear_Expression_from_Linear_Expression (c,
								   tmp_expr);
	    ppl_delete_Linear_Expression (tmp_expr);
	  }

	break;
      }

    case NEGATE_EXPR:
      {
	ppl_Linear_Expression_t tmp_expr = NULL;

	if (c)
	  {
	    ppl_dimension_type dim;
	    ppl_Linear_Expression_space_dimension (c, &dim);
	    ppl_new_Linear_Expression_with_dimension (&tmp_expr, dim);
	  }

	scan_tree_for_params (s, TREE_OPERAND (e, 0), tmp_expr, k);

	if (c)
	  {
	    ppl_subtract_Linear_Expression_from_Linear_Expression (c,
								   tmp_expr);
	    ppl_delete_Linear_Expression (tmp_expr);
	  }

	break;
      }

    case BIT_NOT_EXPR:
      {
	ppl_Linear_Expression_t tmp_expr = NULL;

	if (c)
	  {
	    ppl_dimension_type dim;
	    ppl_Linear_Expression_space_dimension (c, &dim);
	    ppl_new_Linear_Expression_with_dimension (&tmp_expr, dim);
	  }

	scan_tree_for_params (s, TREE_OPERAND (e, 0), tmp_expr, k);

	if (c)
	  {
	    ppl_Coefficient_t coef;
	    Value minus_one;

	    ppl_subtract_Linear_Expression_from_Linear_Expression (c,
								   tmp_expr);
	    ppl_delete_Linear_Expression (tmp_expr);
	    value_init (minus_one);
	    value_set_si (minus_one, -1);
	    ppl_new_Coefficient_from_mpz_t (&coef, minus_one);
	    ppl_Linear_Expression_add_to_inhomogeneous (c, coef);
	    value_clear (minus_one);
	    ppl_delete_Coefficient (coef);
	  }

	break;
      }

    case SSA_NAME:
      {
	ppl_dimension_type p = parameter_index_in_region (e, s);

	if (c)
	  {
	    ppl_dimension_type dim;
	    ppl_Linear_Expression_space_dimension (c, &dim);
	    p += dim - sese_nb_params (s);
	    add_value_to_dim (p, c, k);
	  }
	break;
      }

    case INTEGER_CST:
      if (c)
	scan_tree_for_params_int (e, c, k);
      break;

    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      scan_tree_for_params (s, TREE_OPERAND (e, 0), c, k);
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
  Value one;

  value_init (one);
  value_set_si (one, 1);

  /* Find parameters in the access functions of data references.  */
  for (i = 0; VEC_iterate (data_reference_p, GBB_DATA_REFS (gbb), i, dr); i++)
    for (j = 0; j < DR_NUM_DIMENSIONS (dr); j++)
      scan_tree_for_params (region, DR_ACCESS_FN (dr, j), NULL, one);

  /* Find parameters in conditional statements.  */
  for (i = 0; VEC_iterate (gimple, GBB_CONDITIONS (gbb), i, stmt); i++)
    {
      tree lhs = scalar_evolution_in_region (region, loop,
					     gimple_cond_lhs (stmt));
      tree rhs = scalar_evolution_in_region (region, loop,
					     gimple_cond_rhs (stmt));

      scan_tree_for_params (region, lhs, NULL, one);
      scan_tree_for_params (region, rhs, NULL, one);
    }

  value_clear (one);
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
  Value one;

  value_init (one);
  value_set_si (one, 1);

  /* Find the parameters used in the loop bounds.  */
  for (i = 0; VEC_iterate (loop_p, SESE_LOOP_NEST (region), i, loop); i++)
    {
      tree nb_iters = number_of_latch_executions (loop);

      if (!chrec_contains_symbols (nb_iters))
	continue;

      nb_iters = scalar_evolution_in_region (region, loop, nb_iters);
      scan_tree_for_params (region, nb_iters, NULL, one);
    }

  value_clear (one);

  /* Find the parameters used in data accesses.  */
  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    find_params_in_bb (region, PBB_BLACK_BOX (pbb));

  scop_set_nb_params (scop, sese_nb_params (region));
  SESE_ADD_PARAMS (region) = false;

  ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension
    (&SCOP_CONTEXT (scop), scop_nb_params (scop), 0);
}

/* Returns a gimple_bb from BB.  */

static inline gimple_bb_p
gbb_from_bb (basic_block bb)
{
  return (gimple_bb_p) bb->aux;
}

/* Insert in the SCOP context constraints from the estimation of the
   number of iterations.  UB_EXPR is a linear expression describing
   the number of iterations in a loop.  This expression is bounded by
   the estimation NIT.  */

static void
add_upper_bounds_from_estimated_nit (scop_p scop, double_int nit,
				     ppl_dimension_type dim,
				     ppl_Linear_Expression_t ub_expr)
{
  Value val;
  ppl_Linear_Expression_t nb_iters_le;
  ppl_Polyhedron_t pol;
  ppl_Coefficient_t coef;
  ppl_Constraint_t ub;

  ppl_new_Linear_Expression_with_dimension (&ub_expr, dim);
  ppl_new_C_Polyhedron_from_space_dimension (&pol, dim, 0);
  ppl_new_Linear_Expression_from_Linear_Expression (&nb_iters_le,
						    ub_expr);

  /* Construct the negated number of last iteration in VAL.  */
  value_init (val);
  mpz_set_double_int (val, nit, false);
  value_sub_int (val, val, 1);
  value_oppose (val, val);

  /* NB_ITERS_LE holds the number of last iteration in
     parametrical form.  Subtract estimated number of last
     iteration and assert that result is not positive.  */
  ppl_new_Coefficient_from_mpz_t (&coef, val);
  ppl_Linear_Expression_add_to_inhomogeneous (nb_iters_le, coef);
  ppl_delete_Coefficient (coef);
  ppl_new_Constraint (&ub, nb_iters_le,
		      PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL);
  ppl_Polyhedron_add_constraint (pol, ub);

  /* Remove all but last GDIM dimensions from POL to obtain
     only the constraints on the parameters.  */
  {
    graphite_dim_t gdim = scop_nb_params (scop);
    ppl_dimension_type *dims = XNEWVEC (ppl_dimension_type, dim - gdim);
    graphite_dim_t i;

    for (i = 0; i < dim - gdim; i++)
      dims[i] = i;

    ppl_Polyhedron_remove_space_dimensions (pol, dims, dim - gdim);
    XDELETEVEC (dims);
  }

  /* Add the constraints on the parameters to the SCoP context.  */
  {
    ppl_Pointset_Powerset_C_Polyhedron_t constraints_ps;

    ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
      (&constraints_ps, pol);
    ppl_Pointset_Powerset_C_Polyhedron_intersection_assign
      (SCOP_CONTEXT (scop), constraints_ps);
    ppl_delete_Pointset_Powerset_C_Polyhedron (constraints_ps);
  }

  ppl_delete_Polyhedron (pol);
  ppl_delete_Linear_Expression (nb_iters_le);
  ppl_delete_Constraint (ub);
  value_clear (val);
}

/* Builds the constraint polyhedra for LOOP in SCOP.  OUTER_PH gives
   the constraints for the surrounding loops.  */

static void
build_loop_iteration_domains (scop_p scop, struct loop *loop,
                              ppl_Polyhedron_t outer_ph, int nb,
			      ppl_Pointset_Powerset_C_Polyhedron_t *domains)
{
  int i;
  ppl_Polyhedron_t ph;
  tree nb_iters = number_of_latch_executions (loop);
  ppl_dimension_type dim = nb + 1 + scop_nb_params (scop);
  sese region = SCOP_REGION (scop);

  {
    ppl_const_Constraint_System_t pcs;
    ppl_dimension_type *map
      = (ppl_dimension_type *) XNEWVEC (ppl_dimension_type, dim);

    ppl_new_C_Polyhedron_from_space_dimension (&ph, dim, 0);
    ppl_Polyhedron_get_constraints (outer_ph, &pcs);
    ppl_Polyhedron_add_constraints (ph, pcs);

    for (i = 0; i < (int) nb; i++)
      map[i] = i;
    for (i = (int) nb; i < (int) dim - 1; i++)
      map[i] = i + 1;
    map[dim - 1] = nb;

    ppl_Polyhedron_map_space_dimensions (ph, map, dim);
    free (map);
  }

  /* 0 <= loop_i */
  {
    ppl_Constraint_t lb;
    ppl_Linear_Expression_t lb_expr;

    ppl_new_Linear_Expression_with_dimension (&lb_expr, dim);
    ppl_set_coef (lb_expr, nb, 1);
    ppl_new_Constraint (&lb, lb_expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
    ppl_delete_Linear_Expression (lb_expr);
    ppl_Polyhedron_add_constraint (ph, lb);
    ppl_delete_Constraint (lb);
  }

  if (TREE_CODE (nb_iters) == INTEGER_CST)
    {
      ppl_Constraint_t ub;
      ppl_Linear_Expression_t ub_expr;

      ppl_new_Linear_Expression_with_dimension (&ub_expr, dim);

      /* loop_i <= cst_nb_iters */
      ppl_set_coef (ub_expr, nb, -1);
      ppl_set_inhomogeneous_tree (ub_expr, nb_iters);
      ppl_new_Constraint (&ub, ub_expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
      ppl_Polyhedron_add_constraint (ph, ub);
      ppl_delete_Linear_Expression (ub_expr);
      ppl_delete_Constraint (ub);
    }
  else if (!chrec_contains_undetermined (nb_iters))
    {
      Value one;
      ppl_Constraint_t ub;
      ppl_Linear_Expression_t ub_expr;
      double_int nit;

      value_init (one);
      value_set_si (one, 1);
      ppl_new_Linear_Expression_with_dimension (&ub_expr, dim);
      nb_iters = scalar_evolution_in_region (region, loop, nb_iters);
      scan_tree_for_params (SCOP_REGION (scop), nb_iters, ub_expr, one);
      value_clear (one);

      if (estimated_loop_iterations (loop, true, &nit))
	add_upper_bounds_from_estimated_nit (scop, nit, dim, ub_expr);

      /* loop_i <= expr_nb_iters */
      ppl_set_coef (ub_expr, nb, -1);
      ppl_new_Constraint (&ub, ub_expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
      ppl_Polyhedron_add_constraint (ph, ub);
      ppl_delete_Linear_Expression (ub_expr);
      ppl_delete_Constraint (ub);
    }
  else
    gcc_unreachable ();

  if (loop->inner && loop_in_sese_p (loop->inner, region))
    build_loop_iteration_domains (scop, loop->inner, ph, nb + 1, domains);

  if (nb != 0
      && loop->next
      && loop_in_sese_p (loop->next, region))
    build_loop_iteration_domains (scop, loop->next, outer_ph, nb, domains);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    (&domains[loop->num], ph);

  ppl_delete_Polyhedron (ph);
}

/* Returns a linear expression for tree T evaluated in PBB.  */

static ppl_Linear_Expression_t
create_linear_expr_from_tree (poly_bb_p pbb, tree t)
{
  Value one;
  ppl_Linear_Expression_t res;
  ppl_dimension_type dim;
  sese region = SCOP_REGION (PBB_SCOP (pbb));
  loop_p loop = pbb_loop (pbb);

  dim = pbb_dim_iter_domain (pbb) + pbb_nb_params (pbb);
  ppl_new_Linear_Expression_with_dimension (&res, dim);

  t = scalar_evolution_in_region (region, loop, t);
  gcc_assert (!automatically_generated_chrec_p (t));

  value_init (one);
  value_set_si (one, 1);
  scan_tree_for_params (region, t, res, one);
  value_clear (one);

  return res;
}

/* Returns the ppl constraint type from the gimple tree code CODE.  */

static enum ppl_enum_Constraint_Type
ppl_constraint_type_from_tree_code (enum tree_code code)
{
  switch (code)
    {
    /* We do not support LT and GT to be able to work with C_Polyhedron.
       As we work on integer polyhedron "a < b" can be expressed by
       "a + 1 <= b".  */
    case LT_EXPR:
    case GT_EXPR:
      gcc_unreachable ();

    case LE_EXPR:
      return PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL;

    case GE_EXPR:
      return PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL;

    case EQ_EXPR:
      return PPL_CONSTRAINT_TYPE_EQUAL;

    default:
      gcc_unreachable ();
    }
}

/* Add conditional statement STMT to PS.  It is evaluated in PBB and
   CODE is used as the comparison operator.  This allows us to invert the
   condition or to handle inequalities.  */

static void
add_condition_to_domain (ppl_Pointset_Powerset_C_Polyhedron_t ps, gimple stmt,
			 poly_bb_p pbb, enum tree_code code)
{
  Value v;
  ppl_Coefficient_t c;
  ppl_Linear_Expression_t left, right;
  ppl_Constraint_t cstr;
  enum ppl_enum_Constraint_Type type;

  left = create_linear_expr_from_tree (pbb, gimple_cond_lhs (stmt));
  right = create_linear_expr_from_tree (pbb, gimple_cond_rhs (stmt));

  /* If we have < or > expressions convert them to <= or >= by adding 1 to
     the left or the right side of the expression. */
  if (code == LT_EXPR)
    {
      value_init (v);
      value_set_si (v, 1);
      ppl_new_Coefficient (&c);
      ppl_assign_Coefficient_from_mpz_t (c, v);
      ppl_Linear_Expression_add_to_inhomogeneous (left, c);
      ppl_delete_Coefficient (c);
      value_clear (v);

      code = LE_EXPR;
    }
  else if (code == GT_EXPR)
    {
      value_init (v);
      value_set_si (v, 1);
      ppl_new_Coefficient (&c);
      ppl_assign_Coefficient_from_mpz_t (c, v);
      ppl_Linear_Expression_add_to_inhomogeneous (right, c);
      ppl_delete_Coefficient (c);
      value_clear (v);

      code = GE_EXPR;
    }

  type = ppl_constraint_type_from_tree_code (code);

  ppl_subtract_Linear_Expression_from_Linear_Expression (left, right);

  ppl_new_Constraint (&cstr, left, type);
  ppl_Pointset_Powerset_C_Polyhedron_add_constraint (ps, cstr);

  ppl_delete_Constraint (cstr);
  ppl_delete_Linear_Expression (left);
  ppl_delete_Linear_Expression (right);
}

/* Add conditional statement STMT to pbb.  CODE is used as the comparision
   operator.  This allows us to invert the condition or to handle
   inequalities.  */

static void
add_condition_to_pbb (poly_bb_p pbb, gimple stmt, enum tree_code code)
{
  if (code == NE_EXPR)
    {
      ppl_Pointset_Powerset_C_Polyhedron_t left = PBB_DOMAIN (pbb);
      ppl_Pointset_Powerset_C_Polyhedron_t right;
      ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
	(&right, left);
      add_condition_to_domain (left, stmt, pbb, LT_EXPR);
      add_condition_to_domain (right, stmt, pbb, GT_EXPR);
      ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign (left,
							       right);
      ppl_delete_Pointset_Powerset_C_Polyhedron (right);
    }
  else
    add_condition_to_domain (PBB_DOMAIN (pbb), stmt, pbb, code);
}

/* Add conditions to the domain of PBB.  */

static void
add_conditions_to_domain (poly_bb_p pbb)
{
  unsigned int i;
  gimple stmt;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
  VEC (gimple, heap) *conditions = GBB_CONDITIONS (gbb);

  if (VEC_empty (gimple, conditions))
    return;

  for (i = 0; VEC_iterate (gimple, conditions, i, stmt); i++)
    switch (gimple_code (stmt))
      {
      case GIMPLE_COND:
	  {
	    enum tree_code code = gimple_cond_code (stmt);

	    /* The conditions for ELSE-branches are inverted.  */
	    if (VEC_index (gimple, gbb->condition_cases, i) == NULL)
	      code = invert_tree_comparison (code, false);

	    add_condition_to_pbb (pbb, stmt, code);
	    break;
	  }

      case GIMPLE_SWITCH:
	/* Switch statements are not supported right now - fall throught.  */

      default:
	gcc_unreachable ();
	break;
      }
}

/* Structure used to pass data to dom_walk.  */

struct bsc
{
  VEC (gimple, heap) **conditions, **cases;
  sese region;
};

/* Returns non NULL when BB has a single predecessor and the last
   statement of that predecessor is a COND_EXPR.  */

static gimple
single_pred_cond (basic_block bb)
{
  if (single_pred_p (bb))
    {
      edge e = single_pred_edge (bb);
      basic_block pred = e->src;
      gimple stmt = last_stmt (pred);

      if (stmt && gimple_code (stmt) == GIMPLE_COND)
	return stmt;
    }
  return NULL;
}

/* Call-back for dom_walk executed before visiting the dominated
   blocks.  */

static void
build_sese_conditions_before (struct dom_walk_data *dw_data,
			      basic_block bb)
{
  struct bsc *data = (struct bsc *) dw_data->global_data;
  VEC (gimple, heap) **conditions = data->conditions;
  VEC (gimple, heap) **cases = data->cases;
  gimple_bb_p gbb = gbb_from_bb (bb);
  gimple stmt = single_pred_cond (bb);

  if (!bb_in_sese_p (bb, data->region))
    return;

  if (stmt)
    {
      edge e = single_pred_edge (bb);

      VEC_safe_push (gimple, heap, *conditions, stmt);

      if (e->flags & EDGE_TRUE_VALUE)
	VEC_safe_push (gimple, heap, *cases, stmt);
      else
	VEC_safe_push (gimple, heap, *cases, NULL);
    }

  if (gbb)
    {
      GBB_CONDITIONS (gbb) = VEC_copy (gimple, heap, *conditions);
      GBB_CONDITION_CASES (gbb) = VEC_copy (gimple, heap, *cases);
    }
}

/* Call-back for dom_walk executed after visiting the dominated
   blocks.  */

static void
build_sese_conditions_after (struct dom_walk_data *dw_data,
			     basic_block bb)
{
  struct bsc *data = (struct bsc *) dw_data->global_data;
  VEC (gimple, heap) **conditions = data->conditions;
  VEC (gimple, heap) **cases = data->cases;

  if (!bb_in_sese_p (bb, data->region))
    return;

  if (single_pred_cond (bb))
    {
      VEC_pop (gimple, *conditions);
      VEC_pop (gimple, *cases);
    }
}

/* Record all conditions in REGION.  */

static void
build_sese_conditions (sese region)
{
  struct dom_walk_data walk_data;
  VEC (gimple, heap) *conditions = VEC_alloc (gimple, heap, 3);
  VEC (gimple, heap) *cases = VEC_alloc (gimple, heap, 3);
  struct bsc data;

  data.conditions = &conditions;
  data.cases = &cases;
  data.region = region;

  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children = build_sese_conditions_before;
  walk_data.after_dom_children = build_sese_conditions_after;
  walk_data.global_data = &data;
  walk_data.block_local_data_size = 0;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, SESE_ENTRY_BB (region));
  fini_walk_dominator_tree (&walk_data);

  VEC_free (gimple, heap, conditions);
  VEC_free (gimple, heap, cases);
}

/* Traverses all the GBBs of the SCOP and add their constraints to the
   iteration domains.  */

static void
add_conditions_to_constraints (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    add_conditions_to_domain (pbb);
}

/* Add constraints on the possible values of parameter P from the type
   of P.  */

static void
add_param_constraints (scop_p scop, ppl_Polyhedron_t context, graphite_dim_t p)
{
  ppl_Constraint_t cstr;
  ppl_Linear_Expression_t le;
  tree parameter = VEC_index (tree, SESE_PARAMS (SCOP_REGION (scop)), p);
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
      ppl_new_Linear_Expression_with_dimension (&le, scop_nb_params (scop));
      ppl_set_coef (le, p, -1);
      ppl_set_inhomogeneous_tree (le, lb);
      ppl_new_Constraint (&cstr, le, PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL);
      ppl_Polyhedron_add_constraint (context, cstr);
      ppl_delete_Linear_Expression (le);
      ppl_delete_Constraint (cstr);
    }

  if (ub)
    {
      ppl_new_Linear_Expression_with_dimension (&le, scop_nb_params (scop));
      ppl_set_coef (le, p, -1);
      ppl_set_inhomogeneous_tree (le, ub);
      ppl_new_Constraint (&cstr, le, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
      ppl_Polyhedron_add_constraint (context, cstr);
      ppl_delete_Linear_Expression (le);
      ppl_delete_Constraint (cstr);
    }
}

/* Build the context of the SCOP.  The context usually contains extra
   constraints that are added to the iteration domains that constrain
   some parameters.  */

static void
build_scop_context (scop_p scop)
{
  ppl_Polyhedron_t context;
  ppl_Pointset_Powerset_C_Polyhedron_t ps;
  graphite_dim_t p, n = scop_nb_params (scop);

  ppl_new_C_Polyhedron_from_space_dimension (&context, n, 0);

  for (p = 0; p < n; p++)
    add_param_constraints (scop, context, p);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    (&ps, context);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign
    (SCOP_CONTEXT (scop), ps);

  ppl_delete_Pointset_Powerset_C_Polyhedron (ps);
  ppl_delete_Polyhedron (context);
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
  ppl_Polyhedron_t ph;
  poly_bb_p pbb;
  int nb_loops = number_of_loops ();
  ppl_Pointset_Powerset_C_Polyhedron_t *domains
    = XNEWVEC (ppl_Pointset_Powerset_C_Polyhedron_t, nb_loops);

  for (i = 0; i < nb_loops; i++)
    domains[i] = NULL;

  ppl_new_C_Polyhedron_from_space_dimension (&ph, scop_nb_params (scop), 0);

  for (i = 0; VEC_iterate (loop_p, SESE_LOOP_NEST (region), i, loop); i++)
    if (!loop_in_sese_p (loop_outer (loop), region))
      build_loop_iteration_domains (scop, loop, ph, 0, domains);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    if (domains[gbb_loop (PBB_BLACK_BOX (pbb))->num])
      ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
	(&PBB_DOMAIN (pbb), (ppl_const_Pointset_Powerset_C_Polyhedron_t)
	 domains[gbb_loop (PBB_BLACK_BOX (pbb))->num]);
    else
      ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
	(&PBB_DOMAIN (pbb), ph);

  for (i = 0; i < nb_loops; i++)
    if (domains[i])
      ppl_delete_Pointset_Powerset_C_Polyhedron (domains[i]);

  ppl_delete_Polyhedron (ph);
  free (domains);
}

/* Add a constrain to the ACCESSES polyhedron for the alias set of
   data reference DR.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static void
pdr_add_alias_set (ppl_Polyhedron_t accesses, data_reference_p dr,
		   ppl_dimension_type accessp_nb_dims,
		   ppl_dimension_type dom_nb_dims)
{
  ppl_Linear_Expression_t alias;
  ppl_Constraint_t cstr;
  int alias_set_num = 0;
  base_alias_pair *bap = (base_alias_pair *)(dr->aux);

  if (bap && bap->alias_set)
    alias_set_num = *(bap->alias_set);

  ppl_new_Linear_Expression_with_dimension (&alias, accessp_nb_dims);

  ppl_set_coef (alias, dom_nb_dims, 1);
  ppl_set_inhomogeneous (alias, -alias_set_num);
  ppl_new_Constraint (&cstr, alias, PPL_CONSTRAINT_TYPE_EQUAL);
  ppl_Polyhedron_add_constraint (accesses, cstr);

  ppl_delete_Linear_Expression (alias);
  ppl_delete_Constraint (cstr);
}

/* Add to ACCESSES polyhedron equalities defining the access functions
   to the memory.  ACCESSP_NB_DIMS is the dimension of the ACCESSES
   polyhedron, DOM_NB_DIMS is the dimension of the iteration domain.
   PBB is the poly_bb_p that contains the data reference DR.  */

static void
pdr_add_memory_accesses (ppl_Polyhedron_t accesses, data_reference_p dr,
			 ppl_dimension_type accessp_nb_dims,
			 ppl_dimension_type dom_nb_dims,
			 poly_bb_p pbb)
{
  int i, nb_subscripts = DR_NUM_DIMENSIONS (dr);
  Value v;
  scop_p scop = PBB_SCOP (pbb);
  sese region = SCOP_REGION (scop);

  value_init (v);

  for (i = 0; i < nb_subscripts; i++)
    {
      ppl_Linear_Expression_t fn, access;
      ppl_Constraint_t cstr;
      ppl_dimension_type subscript = dom_nb_dims + 1 + i;
      tree afn = DR_ACCESS_FN (dr, nb_subscripts - 1 - i);

      ppl_new_Linear_Expression_with_dimension (&fn, dom_nb_dims);
      ppl_new_Linear_Expression_with_dimension (&access, accessp_nb_dims);

      value_set_si (v, 1);
      scan_tree_for_params (region, afn, fn, v);
      ppl_assign_Linear_Expression_from_Linear_Expression (access, fn);

      ppl_set_coef (access, subscript, -1);
      ppl_new_Constraint (&cstr, access, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_Polyhedron_add_constraint (accesses, cstr);

      ppl_delete_Linear_Expression (fn);
      ppl_delete_Linear_Expression (access);
      ppl_delete_Constraint (cstr);
    }

  value_clear (v);
}

/* Add constrains representing the size of the accessed data to the
   ACCESSES polyhedron.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static void
pdr_add_data_dimensions (ppl_Polyhedron_t accesses, data_reference_p dr,
			 ppl_dimension_type accessp_nb_dims,
			 ppl_dimension_type dom_nb_dims)
{
  tree ref = DR_REF (dr);
  int i, nb_subscripts = DR_NUM_DIMENSIONS (dr);

  for (i = nb_subscripts - 1; i >= 0; i--, ref = TREE_OPERAND (ref, 0))
    {
      ppl_Linear_Expression_t expr;
      ppl_Constraint_t cstr;
      ppl_dimension_type subscript = dom_nb_dims + 1 + i;
      tree low, high;

      if (TREE_CODE (ref) != ARRAY_REF)
	break;

      low = array_ref_low_bound (ref);

      /* subscript - low >= 0 */
      if (host_integerp (low, 0))
	{
	  ppl_new_Linear_Expression_with_dimension (&expr, accessp_nb_dims);
	  ppl_set_coef (expr, subscript, 1);

	  ppl_set_inhomogeneous (expr, -int_cst_value (low));

	  ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
	  ppl_Polyhedron_add_constraint (accesses, cstr);
	  ppl_delete_Linear_Expression (expr);
	  ppl_delete_Constraint (cstr);
	}

      high = array_ref_up_bound (ref);

      /* high - subscript >= 0 */
      if (high && host_integerp (high, 0)
	  /* 1-element arrays at end of structures may extend over
	     their declared size.  */
	  && !(array_at_struct_end_p (ref)
	       && operand_equal_p (low, high, 0)))
	{
	  ppl_new_Linear_Expression_with_dimension (&expr, accessp_nb_dims);
	  ppl_set_coef (expr, subscript, -1);

	  ppl_set_inhomogeneous (expr, int_cst_value (high));

	  ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
	  ppl_Polyhedron_add_constraint (accesses, cstr);
	  ppl_delete_Linear_Expression (expr);
	  ppl_delete_Constraint (cstr);
	}
    }
}

/* Build data accesses for DR in PBB.  */

static void
build_poly_dr (data_reference_p dr, poly_bb_p pbb)
{
  ppl_Polyhedron_t accesses;
  ppl_Pointset_Powerset_C_Polyhedron_t accesses_ps;
  ppl_dimension_type dom_nb_dims;
  ppl_dimension_type accessp_nb_dims;
  int dr_base_object_set;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (PBB_DOMAIN (pbb),
						      &dom_nb_dims);
  accessp_nb_dims = dom_nb_dims + 1 + DR_NUM_DIMENSIONS (dr);

  ppl_new_C_Polyhedron_from_space_dimension (&accesses, accessp_nb_dims, 0);

  pdr_add_alias_set (accesses, dr, accessp_nb_dims, dom_nb_dims);
  pdr_add_memory_accesses (accesses, dr, accessp_nb_dims, dom_nb_dims, pbb);
  pdr_add_data_dimensions (accesses, dr, accessp_nb_dims, dom_nb_dims);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&accesses_ps,
							    accesses);
  ppl_delete_Polyhedron (accesses);

  if (dr->aux)
    dr_base_object_set = ((base_alias_pair *)(dr->aux))->base_obj_set;

  new_poly_dr (pbb, dr_base_object_set, accesses_ps, DR_IS_READ (dr) ? PDR_READ : PDR_WRITE,
	       dr, DR_NUM_DIMENSIONS (dr));
}

/* Write to FILE the alias graph of data references in DIMACS format.  */

static inline bool
write_alias_graph_to_ascii_dimacs (FILE *file, char *comment,
				   VEC (data_reference_p, heap) *drs)
{
  int num_vertex = VEC_length (data_reference_p, drs);
  int edge_num = 0;
  data_reference_p dr1, dr2;
  int i, j;

  if (num_vertex == 0)
    return true;

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr1); i++)
    for (j = i + 1; VEC_iterate (data_reference_p, drs, j, dr2); j++)
      if (dr_may_alias_p (dr1, dr2))
	edge_num++;

  fprintf (file, "$\n");

  if (comment)
    fprintf (file, "c %s\n", comment);

  fprintf (file, "p edge %d %d\n", num_vertex, edge_num);

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr1); i++)
    for (j = i + 1; VEC_iterate (data_reference_p, drs, j, dr2); j++)
      if (dr_may_alias_p (dr1, dr2))
	fprintf (file, "e %d %d\n", i + 1, j + 1);

  return true;
}

/* Write to FILE the alias graph of data references in DOT format.  */

static inline bool
write_alias_graph_to_ascii_dot (FILE *file, char *comment,
				VEC (data_reference_p, heap) *drs)
{
  int num_vertex = VEC_length (data_reference_p, drs);
  data_reference_p dr1, dr2;
  int i, j;

  if (num_vertex == 0)
    return true;

  fprintf (file, "$\n");

  if (comment)
    fprintf (file, "c %s\n", comment);

  /* First print all the vertices.  */
  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr1); i++)
    fprintf (file, "n%d;\n", i);

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr1); i++)
    for (j = i + 1; VEC_iterate (data_reference_p, drs, j, dr2); j++)
      if (dr_may_alias_p (dr1, dr2))
	fprintf (file, "n%d n%d\n", i, j);

  return true;
}

/* Write to FILE the alias graph of data references in ECC format.  */

static inline bool
write_alias_graph_to_ascii_ecc (FILE *file, char *comment,
				VEC (data_reference_p, heap) *drs)
{
  int num_vertex = VEC_length (data_reference_p, drs);
  data_reference_p dr1, dr2;
  int i, j;

  if (num_vertex == 0)
    return true;

  fprintf (file, "$\n");

  if (comment)
    fprintf (file, "c %s\n", comment);

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr1); i++)
    for (j = i + 1; VEC_iterate (data_reference_p, drs, j, dr2); j++)
      if (dr_may_alias_p (dr1, dr2))
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
build_alias_set_optimal_p (VEC (data_reference_p, heap) *drs)
{
  int num_vertices = VEC_length (data_reference_p, drs);
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

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr1); i++)
    for (j = i+1; VEC_iterate (data_reference_p, drs, j, dr2); j++)
      if (dr_may_alias_p (dr1, dr2))
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
      data_reference_p dr = VEC_index (data_reference_p, drs, i);
      base_alias_pair *bap;

      if (dr->aux)
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

/* Group each data reference in DRS with it's base object set num.  */

static void
build_base_obj_set_for_drs (VEC (data_reference_p, heap) *drs)
{
  int num_vertex = VEC_length (data_reference_p, drs);
  struct graph *g = new_graph (num_vertex);
  data_reference_p dr1, dr2;
  int i, j;
  int *queue;

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr1); i++)
    for (j = i + 1; VEC_iterate (data_reference_p, drs, j, dr2); j++)
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
      data_reference_p dr = VEC_index (data_reference_p, drs, i);
      base_alias_pair *bap;

      if (dr->aux)
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
  VEC (data_reference_p, heap) *gbb_drs = GBB_DATA_REFS (PBB_BLACK_BOX (pbb));

  for (j = 0; VEC_iterate (data_reference_p, gbb_drs, j, dr); j++)
    build_poly_dr (dr, pbb);
}

/* Dump to file the alias graphs for the data references in DRS.  */

static void
dump_alias_graphs (VEC (data_reference_p, heap) *drs)
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
  VEC (data_reference_p, heap) *drs = VEC_alloc (data_reference_p, heap, 3);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    for (j = 0; VEC_iterate (data_reference_p,
			     GBB_DATA_REFS (PBB_BLACK_BOX (pbb)), j, dr); j++)
      VEC_safe_push (data_reference_p, heap, drs, dr);

  for (i = 0; VEC_iterate (data_reference_p, drs, i, dr); i++)
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

  VEC_free (data_reference_p, heap, drs);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
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

/* Insert the assignment "RES := VAR" just after the definition of VAR.  */

static void
insert_out_of_ssa_copy (tree res, tree var)
{
  gimple stmt;
  gimple_seq stmts;
  gimple_stmt_iterator si;
  gimple_stmt_iterator gsi;

  var = force_gimple_operand (var, &stmts, true, NULL_TREE);
  stmt = gimple_build_assign (res, var);
  if (!stmts)
    stmts = gimple_seq_alloc ();
  si = gsi_last (stmts);
  gsi_insert_after (&si, stmt, GSI_NEW_STMT);

  stmt = SSA_NAME_DEF_STMT (var);
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      gsi = gsi_after_labels (gimple_bb (stmt));
      gsi_insert_seq_before (&gsi, stmts, GSI_NEW_STMT);
    }
  else
    {
      gsi = gsi_for_stmt (stmt);
      gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
    }
}

/* Insert on edge E the assignment "RES := EXPR".  */

static void
insert_out_of_ssa_copy_on_edge (edge e, tree res, tree expr)
{
  gimple_stmt_iterator gsi;
  gimple_seq stmts;
  tree var = force_gimple_operand (expr, &stmts, true, NULL_TREE);
  gimple stmt = gimple_build_assign (res, var);

  if (!stmts)
    stmts = gimple_seq_alloc ();

  gsi = gsi_last (stmts);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
  gsi_insert_seq_on_edge (e, stmts);
  gsi_commit_edge_inserts ();
}

/* Creates a zero dimension array of the same type as VAR.  */

static tree
create_zero_dim_array (tree var, const char *base_name)
{
  tree index_type = build_index_type (integer_zero_node);
  tree elt_type = TREE_TYPE (var);
  tree array_type = build_array_type (elt_type, index_type);
  tree base = create_tmp_var (array_type, base_name);

  add_referenced_var (base);

  return build4 (ARRAY_REF, elt_type, base, integer_zero_node, NULL_TREE,
		 NULL_TREE);
}

/* Returns true when PHI is a loop close phi node.  */

static bool
scalar_close_phi_node_p (gimple phi)
{
  if (gimple_code (phi) != GIMPLE_PHI
      || !is_gimple_reg (gimple_phi_result (phi)))
    return false;

  return (gimple_phi_num_args (phi) == 1);
}

/* Rewrite out of SSA the reduction phi node at PSI by creating a zero
   dimension array for it.  */

static void
rewrite_close_phi_out_of_ssa (gimple_stmt_iterator *psi)
{
  gimple phi = gsi_stmt (*psi);
  tree res = gimple_phi_result (phi);
  tree var = SSA_NAME_VAR (res);
  tree zero_dim_array = create_zero_dim_array (var, "Close_Phi");
  gimple_stmt_iterator gsi = gsi_after_labels (gimple_bb (phi));
  gimple stmt = gimple_build_assign (res, zero_dim_array);
  tree arg = gimple_phi_arg_def (phi, 0);

  if (TREE_CODE (arg) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (arg))
    insert_out_of_ssa_copy (zero_dim_array, arg);
  else
    insert_out_of_ssa_copy_on_edge (single_pred_edge (gimple_bb (phi)),
				    zero_dim_array, arg);

  remove_phi_node (psi, false);
  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
  SSA_NAME_DEF_STMT (res) = stmt;
}

/* Rewrite out of SSA the reduction phi node at PSI by creating a zero
   dimension array for it.  */

static void
rewrite_phi_out_of_ssa (gimple_stmt_iterator *psi)
{
  size_t i;
  gimple phi = gsi_stmt (*psi);
  basic_block bb = gimple_bb (phi);
  tree res = gimple_phi_result (phi);
  tree var = SSA_NAME_VAR (res);
  tree zero_dim_array = create_zero_dim_array (var, "General_Reduction");
  gimple_stmt_iterator gsi;
  gimple stmt;
  gimple_seq stmts;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      /* Try to avoid the insertion on edges as much as possible: this
	 would avoid the insertion of code on loop latch edges, making
	 the pattern matching of the vectorizer happy, or it would
	 avoid the insertion of useless basic blocks.  Note that it is
	 incorrect to insert out of SSA copies close by their
	 definition when they are more than two loop levels apart:
	 for example, starting from a double nested loop

	 | a = ...
	 | loop_1
	 |  loop_2
	 |    b = phi (a, c)
	 |    c = ...
	 |  end_2
	 | end_1

	 the following transform is incorrect

	 | a = ...
	 | Red[0] = a
	 | loop_1
	 |  loop_2
	 |    b = Red[0]
	 |    c = ...
	 |    Red[0] = c
	 |  end_2
	 | end_1

	 whereas inserting the copy on the incoming edge is correct

	 | a = ...
	 | loop_1
	 |  Red[0] = a
	 |  loop_2
	 |    b = Red[0]
	 |    c = ...
	 |    Red[0] = c
	 |  end_2
	 | end_1
      */
      if (TREE_CODE (arg) == SSA_NAME
	  && is_gimple_reg (arg)
	  && gimple_bb (SSA_NAME_DEF_STMT (arg))
	  && (flow_bb_inside_loop_p (bb->loop_father,
				     gimple_bb (SSA_NAME_DEF_STMT (arg)))
	      || flow_bb_inside_loop_p (loop_outer (bb->loop_father),
					gimple_bb (SSA_NAME_DEF_STMT (arg)))))
	insert_out_of_ssa_copy (zero_dim_array, arg);
      else
	insert_out_of_ssa_copy_on_edge (gimple_phi_arg_edge (phi, i),
					zero_dim_array, arg);
    }

  var = force_gimple_operand (zero_dim_array, &stmts, true, NULL_TREE);

  if (!stmts)
    stmts = gimple_seq_alloc ();

  stmt = gimple_build_assign (res, var);
  remove_phi_node (psi, false);
  SSA_NAME_DEF_STMT (res) = stmt;

  gsi = gsi_last (stmts);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  gsi = gsi_after_labels (bb);
  gsi_insert_seq_before (&gsi, stmts, GSI_NEW_STMT);
}

/* Return true when DEF can be analyzed in REGION by the scalar
   evolution analyzer.  */

static bool
scev_analyzable_p (tree def, sese region)
{
  gimple stmt = SSA_NAME_DEF_STMT (def);
  loop_p loop = loop_containing_stmt (stmt);
  tree scev = scalar_evolution_in_region (region, loop, def);

  return !chrec_contains_undetermined (scev);
}

/* Rewrite the scalar dependence of DEF used in USE_STMT with a memory
   read from ZERO_DIM_ARRAY.  */

static void
rewrite_cross_bb_scalar_dependence (tree zero_dim_array, tree def, gimple use_stmt)
{
  tree var = SSA_NAME_VAR (def);
  gimple name_stmt = gimple_build_assign (var, zero_dim_array);
  tree name = make_ssa_name (var, name_stmt);
  ssa_op_iter iter;
  use_operand_p use_p;
  gimple_stmt_iterator gsi;

  gcc_assert (gimple_code (use_stmt) != GIMPLE_PHI);

  gimple_assign_set_lhs (name_stmt, name);

  gsi = gsi_for_stmt (use_stmt);
  gsi_insert_before (&gsi, name_stmt, GSI_NEW_STMT);

  FOR_EACH_SSA_USE_OPERAND (use_p, use_stmt, iter, SSA_OP_ALL_USES)
    if (operand_equal_p (def, USE_FROM_PTR (use_p), 0))
      replace_exp (use_p, name);

  update_stmt (use_stmt);
}

/* Rewrite the scalar dependences crossing the boundary of the BB
   containing STMT with an array.  */

static void
rewrite_cross_bb_scalar_deps (sese region, gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  imm_use_iterator imm_iter;
  tree def;
  basic_block def_bb;
  tree zero_dim_array = NULL_TREE;
  gimple use_stmt;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return;

  def = gimple_assign_lhs (stmt);
  if (!is_gimple_reg (def)
      || scev_analyzable_p (def, region))
    return;

  def_bb = gimple_bb (stmt);

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    if (def_bb != gimple_bb (use_stmt)
	&& gimple_code (use_stmt) != GIMPLE_PHI
	&& !is_gimple_debug (use_stmt))
      {
	if (!zero_dim_array)
	  {
	    zero_dim_array = create_zero_dim_array
	      (SSA_NAME_VAR (def), "Cross_BB_scalar_dependence");
	    insert_out_of_ssa_copy (zero_dim_array, def);
	    gsi_next (gsi);
	  }

	rewrite_cross_bb_scalar_dependence (zero_dim_array, def, use_stmt);
      }
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
	  if (scalar_close_phi_node_p (gsi_stmt (psi)))
	    rewrite_close_phi_out_of_ssa (&psi);
	  else if (reduction_phi_p (region, &psi))
	    rewrite_phi_out_of_ssa (&psi);
	}

  update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
  verify_ssa (false);
  verify_loop_closed_ssa ();
#endif

  FOR_EACH_BB (bb)
    if (bb_in_sese_p (bb, region))
      for (psi = gsi_start_bb (bb); !gsi_end_p (psi); gsi_next (&psi))
	rewrite_cross_bb_scalar_deps (region, &psi);

  update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
  verify_ssa (false);
  verify_loop_closed_ssa ();
#endif
}

/* Returns the number of pbbs that are in loops contained in SCOP.  */

static int
nb_pbbs_in_loops (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int res = 0;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
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

/* Splits STMT out of its current BB.  */

static basic_block
split_reduction_stmt (gimple stmt)
{
  gimple_stmt_iterator gsi;
  basic_block bb = gimple_bb (stmt);
  edge e;

  /* Do not split basic blocks with no writes to memory: the reduction
     will be the only write to memory.  */
  if (nb_data_writes_in_bb (bb) == 0)
    return bb;

  split_block (bb, stmt);

  if (gsi_one_before_end_p (gsi_start_nondebug_bb (bb)))
    return bb;

  gsi = gsi_last_bb (bb);
  gsi_prev (&gsi);
  e = split_block (bb, gsi_stmt (gsi));

  return e->dest;
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
				  VEC (gimple, heap) **in,
				  VEC (gimple, heap) **out)
{
  gimple phi = follow_ssa_with_commutative_ops (arg, lhs);

  if (!phi)
    return NULL;

  VEC_safe_push (gimple, heap, *in, stmt);
  VEC_safe_push (gimple, heap, *out, stmt);
  return phi;
}

/* Detect commutative and associative scalar reductions starting at
   the STMT.  Return the phi node of the reduction cycle, or NULL.  */

static gimple
detect_commutative_reduction_assign (gimple stmt, VEC (gimple, heap) **in,
				     VEC (gimple, heap) **out)
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


/* Return the argument of the loop PHI that is the inital value coming
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

/* Return the argument of the loop PHI that is the inital value coming
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

/* Detect commutative and associative scalar reductions starting at
   the loop closed phi node CLOSE_PHI.  Return the phi node of the
   reduction cycle, or NULL.  */

static gimple
detect_commutative_reduction (gimple stmt, VEC (gimple, heap) **in,
			      VEC (gimple, heap) **out)
{
  if (scalar_close_phi_node_p (stmt))
    {
      tree arg = gimple_phi_arg_def (stmt, 0);
      gimple def, loop_phi;

      if (TREE_CODE (arg) != SSA_NAME)
	return NULL;

      def = SSA_NAME_DEF_STMT (arg);
      loop_phi = detect_commutative_reduction (def, in, out);

      if (loop_phi)
	{
	  tree lhs = gimple_phi_result (stmt);
	  tree init = initial_value_for_loop_phi (loop_phi);
	  gimple phi = follow_inital_value_to_phi (init, lhs);

	  VEC_safe_push (gimple, heap, *in, loop_phi);
	  VEC_safe_push (gimple, heap, *out, stmt);
	  return phi;
	}
      else
	return NULL;
    }

  if (gimple_code (stmt) == GIMPLE_ASSIGN)
    return detect_commutative_reduction_assign (stmt, in, out);

  return NULL;
}

/* Translate the scalar reduction statement STMT to an array RED
   knowing that its recursive phi node is LOOP_PHI.  */

static void
translate_scalar_reduction_to_array_for_stmt (tree red, gimple stmt,
					      gimple loop_phi)
{
  gimple_stmt_iterator insert_gsi = gsi_after_labels (gimple_bb (loop_phi));
  tree res = gimple_phi_result (loop_phi);
  gimple assign = gimple_build_assign (res, red);

  gsi_insert_before (&insert_gsi, assign, GSI_SAME_STMT);

  insert_gsi = gsi_after_labels (gimple_bb (stmt));
  assign = gimple_build_assign (red, gimple_assign_lhs (stmt));
  insert_gsi = gsi_for_stmt (stmt);
  gsi_insert_after (&insert_gsi, assign, GSI_SAME_STMT);
}

/* Insert the assignment "result (CLOSE_PHI) = RED".  */

static void
insert_copyout (tree red, gimple close_phi)
{
  tree res = gimple_phi_result (close_phi);
  basic_block bb = gimple_bb (close_phi);
  gimple_stmt_iterator insert_gsi = gsi_after_labels (bb);
  gimple assign = gimple_build_assign (res, red);

  gsi_insert_before (&insert_gsi, assign, GSI_SAME_STMT);
}

/* Insert the assignment "RED = initial_value (LOOP_PHI)".  */

static void
insert_copyin (tree red, gimple loop_phi)
{
  gimple_seq stmts;
  tree init = initial_value_for_loop_phi (loop_phi);
  tree expr = build2 (MODIFY_EXPR, TREE_TYPE (init), red, init);

  force_gimple_operand (expr, &stmts, true, NULL);
  gsi_insert_seq_on_edge (edge_initial_value_for_loop_phi (loop_phi), stmts);
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
  VEC (gimple, heap) *update = VEC_alloc (gimple, heap, 3);
  unsigned int i;
  gimple stmt;

  def = PHI_RESULT (phi);
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, def)
    {
      stmt = USE_STMT (use_p);

      if (is_gimple_debug (stmt))
	{
	  gimple_debug_bind_reset_value (stmt);
	  VEC_safe_push (gimple, heap, update, stmt);
	}
    }

  for (i = 0; VEC_iterate (gimple, update, i, stmt); i++)
    update_stmt (stmt);

  VEC_free (gimple, heap, update);

  gsi = gsi_for_phi_node (phi);
  remove_phi_node (&gsi, false);
}

/* Rewrite out of SSA the reduction described by the loop phi nodes
   IN, and the close phi nodes OUT.  IN and OUT are structured by loop
   levels like this:

   IN: stmt, loop_n, ..., loop_0
   OUT: stmt, close_n, ..., close_0

   the first element is the reduction statement, and the next elements
   are the loop and close phi nodes of each of the outer loops.  */

static void
translate_scalar_reduction_to_array (VEC (gimple, heap) *in,
				     VEC (gimple, heap) *out,
				     sbitmap reductions)
{
  unsigned int i;
  gimple loop_phi;
  tree red;

  for (i = 0; VEC_iterate (gimple, in, i, loop_phi); i++)
    {
      gimple close_phi = VEC_index (gimple, out, i);

      if (i == 0)
	{
	  gimple stmt = loop_phi;
	  basic_block bb = split_reduction_stmt (stmt);

	  SET_BIT (reductions, bb->index);
	  gcc_assert (close_phi == loop_phi);

	  red = create_zero_dim_array
	    (gimple_assign_lhs (stmt), "Commutative_Associative_Reduction");
	  translate_scalar_reduction_to_array_for_stmt
	    (red, stmt, VEC_index (gimple, in, 1));
	  continue;
	}

      if (i == VEC_length (gimple, in) - 1)
	{
	  insert_copyout (red, close_phi);
	  insert_copyin (red, loop_phi);
	}

      remove_phi (loop_phi);
      remove_phi (close_phi);
    }
}

/* Rewrites out of SSA a commutative reduction at CLOSE_PHI.  */

static void
rewrite_commutative_reductions_out_of_ssa_close_phi (gimple close_phi,
						     sbitmap reductions)
{
  VEC (gimple, heap) *in = VEC_alloc (gimple, heap, 10);
  VEC (gimple, heap) *out = VEC_alloc (gimple, heap, 10);

  detect_commutative_reduction (close_phi, &in, &out);
  if (VEC_length (gimple, in) > 0)
    translate_scalar_reduction_to_array (in, out, reductions);

  VEC_free (gimple, heap, in);
  VEC_free (gimple, heap, out);
}

/* Rewrites all the commutative reductions from LOOP out of SSA.  */

static void
rewrite_commutative_reductions_out_of_ssa_loop (loop_p loop,
						sbitmap reductions)
{
  gimple_stmt_iterator gsi;
  edge exit = single_exit (loop);

  if (!exit)
    return;

  for (gsi = gsi_start_phis (exit->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    rewrite_commutative_reductions_out_of_ssa_close_phi (gsi_stmt (gsi),
							 reductions);
}

/* Rewrites all the commutative reductions from SCOP out of SSA.  */

static void
rewrite_commutative_reductions_out_of_ssa (sese region, sbitmap reductions)
{
  loop_iterator li;
  loop_p loop;

  FOR_EACH_LOOP (li, loop, 0)
    if (loop_in_sese_p (loop, region))
      rewrite_commutative_reductions_out_of_ssa_loop (loop, reductions);

  gsi_commit_edge_inserts ();
  update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
  verify_ssa (false);
  verify_loop_closed_ssa ();
#endif
}

/* A LOOP is in normal form for Graphite when it contains only one
   scalar phi node that defines the main induction variable of the
   loop, only one increment of the IV, and only one exit condition.  */

static void
graphite_loop_normal_form (loop_p loop)
{
  struct tree_niter_desc niter;
  tree nit;
  gimple_seq stmts;
  edge exit = single_dom_exit (loop);

  bool known_niter = number_of_iterations_exit (loop, exit, &niter, false);

  /* At this point we should know the number of iterations.  */
  gcc_assert (known_niter);

  nit = force_gimple_operand (unshare_expr (niter.niter), &stmts, true,
			      NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);

  loop->single_iv = canonicalize_loop_ivs (loop, &nit, false);
}

/* Rewrite all the loops of SCOP in normal form: one induction
   variable per loop.  */

static void
scop_canonicalize_loops (scop_p scop)
{
  loop_iterator li;
  loop_p loop;

  FOR_EACH_LOOP (li, loop, 0)
    if (loop_in_sese_p (loop, SCOP_REGION (scop)))
      graphite_loop_normal_form (loop);
}

/* Java does not initialize long_long_integer_type_node.  */
#define my_long_long (long_long_integer_type_node ? long_long_integer_type_node : ssizetype)

/* Can all ivs be represented by a signed integer?
   As CLooG might generate negative values in its expressions, signed loop ivs
   are required in the backend. */
static bool
scop_ivs_can_be_represented (scop_p scop)
{
  loop_iterator li;
  loop_p loop;

  FOR_EACH_LOOP (li, loop, 0)
    {
      tree type;
      int precision;

      if (!loop_in_sese_p (loop, SCOP_REGION (scop)))
	continue;

      if (!loop->single_iv)
	continue;

      type = TREE_TYPE(loop->single_iv);
      precision = TYPE_PRECISION (type);

      if (TYPE_UNSIGNED (type)
	  && precision >= TYPE_PRECISION (my_long_long))
	return false;
    }

  return true;
}

#undef my_long_long

/* Builds the polyhedral representation for a SESE region.  */

void
build_poly_scop (scop_p scop)
{
  sese region = SCOP_REGION (scop);
  sbitmap reductions = sbitmap_alloc (last_basic_block * 2);
  graphite_dim_t max_dim;

  sbitmap_zero (reductions);
  rewrite_commutative_reductions_out_of_ssa (region, reductions);
  rewrite_reductions_out_of_ssa (scop);
  build_scop_bbs (scop, reductions);
  sbitmap_free (reductions);

  /* FIXME: This restriction is needed to avoid a problem in CLooG.
     Once CLooG is fixed, remove this guard.  Anyways, it makes no
     sense to optimize a scop containing only PBBs that do not belong
     to any loops.  */
  if (nb_pbbs_in_loops (scop) == 0)
    return;

  scop_canonicalize_loops (scop);
  if (!scop_ivs_can_be_represented (scop))
    return;

  build_sese_loop_nests (region);
  build_sese_conditions (region);
  find_scop_parameters (scop);

  max_dim = PARAM_VALUE (PARAM_GRAPHITE_MAX_NB_SCOP_PARAMS);
  if (scop_nb_params (scop) > max_dim)
    return;

  build_scop_iteration_domain (scop);
  build_scop_context (scop);

  add_conditions_to_constraints (scop);
  scop_to_lst (scop);
  build_scop_scattering (scop);
  build_scop_drs (scop);

  /* This SCoP has been translated to the polyhedral
     representation.  */
  POLY_SCOP_P (scop) = true;
}

/* Always return false.  Exercise the scop_to_clast function.  */

void
check_poly_representation (scop_p scop ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  cloog_prog_clast pc = scop_to_clast (scop);
  cloog_clast_free (pc.stmt);
  cloog_program_free (pc.prog);
#endif
}
#endif
