/* Conversion of SESE regions to Polyhedra.
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
      /* FIXME: PRE introduces phi nodes like these, for an example,
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

/* Frees GBB.  */

static void
free_gimple_bb (struct gimple_bb *gbb)
{
  if (GBB_CLOOG_IV_TYPES (gbb))
    htab_delete (GBB_CLOOG_IV_TYPES (gbb));

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
try_generate_gimple_bb (scop_p scop, basic_block bb)
{
  VEC (data_reference_p, heap) *drs = VEC_alloc (data_reference_p, heap, 5);
  loop_p nest = outermost_loop_in_sese (SCOP_REGION (scop), bb);
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    graphite_find_data_references_in_stmt (nest, gsi_stmt (gsi), &drs);

  if (!graphite_stmt_p (SCOP_REGION (scop), bb, drs))
    free_data_refs (drs);
  else
    new_poly_bb (scop, new_gimple_bb (bb, drs));
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
build_scop_bbs_1 (scop_p scop, sbitmap visited, basic_block bb)
{
  sese region = SCOP_REGION (scop);
  VEC (basic_block, heap) *dom;

  if (TEST_BIT (visited, bb->index)
      || !bb_in_sese_p (bb, region))
    return;

  try_generate_gimple_bb (scop, bb);
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
	    build_scop_bbs_1 (scop, visited, dom_bb);
	    VEC_unordered_remove (basic_block, dom, i);
	    break;
	  }
    }

  VEC_free (basic_block, heap, dom);
}

/* Gather the basic blocks belonging to the SCOP.  */

void
build_scop_bbs (scop_p scop)
{
  sbitmap visited = sbitmap_alloc (last_basic_block);
  sese region = SCOP_REGION (scop);

  sbitmap_zero (visited);
  build_scop_bbs_1 (scop, visited, SESE_ENTRY_BB (region));

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

  ppl_new_C_Polyhedron_from_C_Polyhedron (&PBB_ORIGINAL_SCATTERING (pbb),
					  PBB_TRANSFORMED_SCATTERING (pbb));
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

/* Saves in NV at index I a new name for variable P.  */

static void
save_var_name (char **nv, int i, tree p)
{
  const char *name = get_name (SSA_NAME_VAR (p));

  if (name)
    {
      int len = strlen (name) + 16;
      nv[i] = XNEWVEC (char, len);
      snprintf (nv[i], len, "%s_%d", name, SSA_NAME_VERSION (p));
    }
  else
    {
      nv[i] = XNEWVEC (char, 16);
      snprintf (nv[i], 2 + 16, "T_%d", SSA_NAME_VERSION (p));
    }
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
  save_var_name (SESE_PARAMS_NAMES (region), i, name);
  save_clast_name_index (SESE_PARAMS_INDEX (region),
			 SESE_PARAMS_NAMES (region)[i], i);
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

/* Data structure for idx_record_params.  */

struct irp_data
{
  struct loop *loop;
  sese region;
};

/* For a data reference with an ARRAY_REF as its BASE, record the
   parameters occurring in IDX.  DTA is passed in as complementary
   information, and is used by the automatic walker function.  This
   function is a callback for for_each_index.  */

static bool
idx_record_params (tree base, tree *idx, void *dta)
{
  struct irp_data *data = (struct irp_data *) dta;

  if (TREE_CODE (base) != ARRAY_REF)
    return true;

  if (TREE_CODE (*idx) == SSA_NAME)
    {
      tree scev;
      sese region = data->region;
      struct loop *loop = data->loop;
      Value one;

      scev = scalar_evolution_in_region (region, loop, *idx);

      value_init (one);
      value_set_si (one, 1);
      scan_tree_for_params (region, scev, NULL, one);
      value_clear (one);
    }

  return true;
}

/* Find parameters with respect to REGION in BB. We are looking in memory
   access functions, conditions and loop bounds.  */

static void
find_params_in_bb (sese region, gimple_bb_p gbb)
{
  int i;
  data_reference_p dr;
  gimple stmt;
  loop_p loop = GBB_BB (gbb)->loop_father;

  for (i = 0; VEC_iterate (data_reference_p, GBB_DATA_REFS (gbb), i, dr); i++)
    {
      struct irp_data irp;

      irp.loop = loop;
      irp.region = region;
      for_each_index (&dr->ref, idx_record_params, &irp);
    }

  /* Find parameters in conditional statements.  */
  for (i = 0; VEC_iterate (gimple, GBB_CONDITIONS (gbb), i, stmt); i++)
    {
      Value one;
      tree lhs = scalar_evolution_in_region (region, loop,
					     gimple_cond_lhs (stmt));
      tree rhs = scalar_evolution_in_region (region, loop,
					     gimple_cond_rhs (stmt));

      value_init (one);
      value_set_si (one, 1);
      scan_tree_for_params (region, lhs, NULL, one);
      scan_tree_for_params (region, rhs, NULL, one);
      value_clear (one);
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
}

/* Returns a gimple_bb from BB.  */

static inline gimple_bb_p
gbb_from_bb (basic_block bb)
{
  return (gimple_bb_p) bb->aux;
}

/* Builds the constraint polyhedra for LOOP in SCOP.  OUTER_PH gives
   the constraints for the surrounding loops.  */

static void
build_loop_iteration_domains (scop_p scop, struct loop *loop,
                              ppl_Polyhedron_t outer_ph, int nb)

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

      value_init (one);
      value_set_si (one, 1);
      ppl_new_Linear_Expression_with_dimension (&ub_expr, dim);
      nb_iters = scalar_evolution_in_region (region, loop, nb_iters);
      scan_tree_for_params (SCOP_REGION (scop), nb_iters, ub_expr, one);
      value_clear (one);

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
    build_loop_iteration_domains (scop, loop->inner, ph, nb + 1);

  if (nb != 0
      && loop->next
      && loop_in_sese_p (loop->next, region))
    build_loop_iteration_domains (scop, loop->next, outer_ph, nb);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    ((ppl_Pointset_Powerset_C_Polyhedron_t *) &loop->aux, ph);

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
  loop_p loop = GBB_BB (PBB_BLACK_BOX (pbb))->loop_father;

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
  tree lb, ub;

  /* Disabled until we fix CPU2006.  */
  return;

  if (!INTEGRAL_TYPE_P (type))
    return;

  lb = TYPE_MIN_VALUE (type);
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
  graphite_dim_t p, n = scop_nb_params (scop);

  ppl_new_C_Polyhedron_from_space_dimension (&context, n, 0);

  for (p = 0; p < n; p++)
    add_param_constraints (scop, context, p);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    (&SCOP_CONTEXT (scop), context);

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

  ppl_new_C_Polyhedron_from_space_dimension (&ph, scop_nb_params (scop), 0);

  for (i = 0; VEC_iterate (loop_p, SESE_LOOP_NEST (region), i, loop); i++)
    if (!loop_in_sese_p (loop_outer (loop), region))
      build_loop_iteration_domains (scop, loop, ph, 0);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    if (gbb_loop (PBB_BLACK_BOX (pbb))->aux)
      ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
	(&PBB_DOMAIN (pbb), (ppl_const_Pointset_Powerset_C_Polyhedron_t)
	 gbb_loop (PBB_BLACK_BOX (pbb))->aux);
    else
      ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
	(&PBB_DOMAIN (pbb), ph);

  for (i = 0; VEC_iterate (loop_p, SESE_LOOP_NEST (region), i, loop); i++)
    if (loop->aux)
      {
	ppl_delete_Pointset_Powerset_C_Polyhedron
	  ((ppl_Pointset_Powerset_C_Polyhedron_t) loop->aux);
	loop->aux = NULL;
      }

  ppl_delete_Polyhedron (ph);
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

  if (dr->aux != NULL)
    {
      alias_set_num = *((int *)(dr->aux));
      free (dr->aux);
      dr->aux = NULL;
    }

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
   DATA_CONTAINER polyhedron.  ACCESSP_NB_DIMS is the dimension of the
   DATA_CONTAINER polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static void
pdr_add_data_dimensions (ppl_Polyhedron_t data_container, data_reference_p dr,
			 ppl_dimension_type accessp_nb_dims,
			 ppl_dimension_type dom_nb_dims)
{
  tree ref = DR_REF (dr);
  int i, nb_subscripts = DR_NUM_DIMENSIONS (dr);
  tree array_size;
  HOST_WIDE_INT elt_size;

  array_size = TYPE_SIZE (TREE_TYPE (ref));
  if (array_size == NULL_TREE
      || TREE_CODE (array_size) != INTEGER_CST)
    return;

  elt_size = int_cst_value (array_size);

  for (i = nb_subscripts - 1; i >= 0; i--)
    {
      ppl_Linear_Expression_t expr;
      ppl_Constraint_t cstr;
      ppl_dimension_type subscript = dom_nb_dims + 1 + i;

      /* 0 <= subscript */
      ppl_new_Linear_Expression_with_dimension (&expr, accessp_nb_dims);
      ppl_set_coef (expr, subscript, 1);
      ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
      ppl_Polyhedron_add_constraint (data_container, cstr);
      ppl_delete_Linear_Expression (expr);
      ppl_delete_Constraint (cstr);

      ref = TREE_OPERAND (ref, 0);
      array_size = TYPE_SIZE (TREE_TYPE (ref));
      if (array_size == NULL_TREE
	  || TREE_CODE (array_size) != INTEGER_CST)
	break;

      /* subscript <= array_size */
      ppl_new_Linear_Expression_with_dimension (&expr, accessp_nb_dims);
      ppl_set_coef (expr, subscript, -1);

      if (elt_size)
	ppl_set_inhomogeneous (expr, int_cst_value (array_size) / elt_size);

      ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
      ppl_Polyhedron_add_constraint (data_container, cstr);
      ppl_delete_Linear_Expression (expr);
      ppl_delete_Constraint (cstr);

      elt_size = int_cst_value (array_size);
    }
}

/* Build data accesses for DR in PBB.  */

static void
build_poly_dr (data_reference_p dr, poly_bb_p pbb)
{
  ppl_Polyhedron_t accesses, data_container;
  ppl_Pointset_Powerset_C_Polyhedron_t accesses_ps, data_container_ps;
  ppl_dimension_type dom_nb_dims;
  ppl_dimension_type accessp_nb_dims;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (PBB_DOMAIN (pbb),
						      &dom_nb_dims);
  accessp_nb_dims = dom_nb_dims + 1 + DR_NUM_DIMENSIONS (dr);

  ppl_new_C_Polyhedron_from_space_dimension (&accesses, accessp_nb_dims, 0);
  ppl_new_C_Polyhedron_from_space_dimension (&data_container,
					     accessp_nb_dims, 0);

  pdr_add_alias_set (accesses, dr, accessp_nb_dims, dom_nb_dims);
  pdr_add_memory_accesses (accesses, dr, accessp_nb_dims, dom_nb_dims, pbb);
  pdr_add_data_dimensions (data_container, dr, accessp_nb_dims, dom_nb_dims);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&accesses_ps,
							    accesses);
  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&data_container_ps,
							    data_container);
  ppl_delete_Polyhedron (accesses);
  ppl_delete_Polyhedron (data_container);
  new_poly_dr (pbb, accesses_ps, data_container_ps,
	       DR_IS_READ (dr) ? PDR_READ : PDR_WRITE, dr);
}

/* Group each data reference in DRS with it's alias set num.  */

static void
build_alias_set_for_drs (VEC (data_reference_p, heap) **drs)
{
  int num_vertex = VEC_length (data_reference_p, *drs);
  struct graph *g = new_graph (num_vertex);
  data_reference_p dr1, dr2;
  int i, j;
  int num_component;
  int *queue;

  for (i = 0; VEC_iterate (data_reference_p, *drs, i, dr1); i++)
    for (j = i+1; VEC_iterate (data_reference_p, *drs, j, dr2); j++)
      if (dr_may_alias_p (dr1, dr2))
	{
	  add_edge (g, i, j);
	  add_edge (g, j, i);
	}

  queue = XNEWVEC (int, num_vertex);
  for (i = 0; i < num_vertex; i++)
    queue[i] = i;

  num_component = graphds_dfs (g, queue, num_vertex, NULL, true, NULL);

  for (i = 0; i < g->n_vertices; i++)
    {
      data_reference_p dr = VEC_index (data_reference_p, *drs, i);
      dr->aux = XNEW (int);
      *((int *)(dr->aux)) = g->vertices[i].component + 1;
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

/* Build data references in SCOP.  */

static void
build_scop_drs (scop_p scop)
{
  int i, j;
  poly_bb_p pbb;
  data_reference_p dr;
  VEC (data_reference_p, heap) *drs = VEC_alloc (data_reference_p, heap, 3);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    {
      VEC (data_reference_p, heap) *gbb_drs = GBB_DATA_REFS (PBB_BLACK_BOX (pbb));
      for (j = 0; VEC_iterate (data_reference_p, gbb_drs, j, dr); j++)
       VEC_safe_push (data_reference_p, heap, drs, dr);
    }

  build_alias_set_for_drs (&drs);
  VEC_free (data_reference_p, heap, drs);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    build_pbb_drs (pbb);
}

/* Return a gsi at the position of the VAR definition.  */

static gimple_stmt_iterator
gsi_for_ssa_name_def (tree var)
{
  gimple stmt;
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple_stmt_iterator psi;

  gcc_assert (TREE_CODE (var) == SSA_NAME);

  stmt = SSA_NAME_DEF_STMT (var);
  bb = gimple_bb (stmt);

  for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
    if (stmt == gsi_stmt (psi))
      return gsi_after_labels (bb);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    if (stmt == gsi_stmt (gsi))
      {
	gsi_next (&gsi);
	return gsi;
      }

  gcc_unreachable ();
  return gsi;
}

/* Insert the assignment "RES := VAR" just after the definition of VAR.  */

static void
insert_out_of_ssa_copy (tree res, tree var)
{
  gimple_stmt_iterator gsi = gsi_for_ssa_name_def (var);
  gimple stmt;
  gimple_seq stmts;
  gimple_stmt_iterator si;

  var = force_gimple_operand (var, &stmts, true, NULL_TREE);
  stmt = gimple_build_assign (res, var);
  if (!stmts)
    stmts = gimple_seq_alloc ();
  si = gsi_last (stmts);
  gsi_insert_after (&si, stmt, GSI_NEW_STMT);
  gsi_insert_seq_before (&gsi, stmts, GSI_NEW_STMT);
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
create_zero_dim_array (tree var)
{
  tree index_type = build_index_type (integer_zero_node);
  tree elt_type = TREE_TYPE (var);
  tree array_type = build_array_type (elt_type, index_type);
  tree base = create_tmp_var (array_type, "Red");

  add_referenced_var (base);

  return build4 (ARRAY_REF, elt_type, base, integer_zero_node, NULL_TREE,
		 NULL_TREE);
}

/* Returns true when PHI is a loop close phi node.  */

static bool
scalar_close_phi_node_p (gimple phi)
{
  gcc_assert (gimple_code (phi) == GIMPLE_PHI);

  if (!is_gimple_reg (gimple_phi_result (phi)))
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
  tree zero_dim_array = create_zero_dim_array (var);
  gimple_stmt_iterator gsi = gsi_after_labels (gimple_bb (phi));
  gimple stmt = gimple_build_assign (res, zero_dim_array);
  tree arg = gimple_phi_arg_def (phi, 0);

  insert_out_of_ssa_copy (zero_dim_array, arg);

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
  tree zero_dim_array = create_zero_dim_array (var);
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

	 whereas inserting the copy on the incomming edge is correct

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

/* Rewrite out of SSA all the reduction phi nodes of SCOP.  */

static void
rewrite_reductions_out_of_ssa (scop_p scop)
{
  basic_block bb;
  gimple_stmt_iterator psi;
  sese region = SCOP_REGION (scop);

  FOR_EACH_BB (bb)
    if (bb_in_region (bb, SESE_ENTRY_BB (region), SESE_EXIT_BB (region)))
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

/* Builds the polyhedral representation for a SESE region.  */

bool
build_poly_scop (scop_p scop)
{
  sese region = SCOP_REGION (scop);
  rewrite_reductions_out_of_ssa (scop);
  build_scop_bbs (scop);

  /* FIXME: This restriction is needed to avoid a problem in CLooG.
     Once CLooG is fixed, remove this guard.  Anyways, it makes no
     sense to optimize a scop containing only PBBs that do not belong
     to any loops.  */
  if (nb_pbbs_in_loops (scop) == 0)
    return false;

  build_sese_loop_nests (region);
  build_sese_conditions (region);
  find_scop_parameters (scop);

  build_scop_iteration_domain (scop);
  build_scop_context (scop);

  add_conditions_to_constraints (scop);
  build_scop_scattering (scop);
  build_scop_drs (scop);

  return true;
}

/* Always return false.  Exercise the scop_to_clast function.  */

void
check_poly_representation (scop_p scop)
{
#ifdef ENABLE_CHECKING
  cloog_prog_clast pc = scop_to_clast (scop);
  cloog_clast_free (pc.stmt);
  cloog_program_free (pc.prog);
#endif
}
#endif
