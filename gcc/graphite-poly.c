/* Graphite polyhedral representation.
   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Tobias Grosser <grosser@fim.uni-passau.de>.

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
#include "output.h"
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
#include "params.h"

#ifdef HAVE_cloog
#include "cloog/cloog.h"
#include "ppl_c.h"
#include "sese.h"
#include "graphite-ppl.h"
#include "graphite.h"
#include "graphite-poly.h"
#include "graphite-dependences.h"

/* Return the maximal loop depth in SCOP.  */

int
scop_max_loop_depth (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int max_nb_loops = 0;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    {
      int nb_loops = pbb_dim_iter_domain (pbb);
      if (max_nb_loops < nb_loops)
        max_nb_loops = nb_loops;
    }

  return max_nb_loops;
}

/* Extend the scattering matrix of PBB to MAX_SCATTERING scattering
   dimensions.  */

static void
extend_scattering (poly_bb_p pbb, int max_scattering)
{
  ppl_dimension_type nb_old_dims, nb_new_dims;
  int nb_added_dims, i;
  ppl_Coefficient_t coef;
  Value one;

  nb_added_dims = max_scattering - pbb_nb_scattering_transform (pbb);
  value_init (one);
  value_set_si (one, 1);
  ppl_new_Coefficient (&coef);
  ppl_assign_Coefficient_from_mpz_t (coef, one);

  gcc_assert (nb_added_dims >= 0);

  nb_old_dims = pbb_nb_scattering_transform (pbb) + pbb_dim_iter_domain (pbb)
    + scop_nb_params (PBB_SCOP (pbb));
  nb_new_dims = nb_old_dims + nb_added_dims;

  ppl_insert_dimensions (PBB_TRANSFORMED_SCATTERING (pbb),
			 pbb_nb_scattering_transform (pbb), nb_added_dims);
  PBB_NB_SCATTERING_TRANSFORM (pbb) += nb_added_dims;

  /* Add identity matrix for the added dimensions.  */
  for (i = max_scattering - nb_added_dims; i < max_scattering; i++)
    {
      ppl_Constraint_t cstr;
      ppl_Linear_Expression_t expr;

      ppl_new_Linear_Expression_with_dimension (&expr, nb_new_dims);
      ppl_Linear_Expression_add_to_coefficient (expr, i, coef);
      ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_Polyhedron_add_constraint (PBB_TRANSFORMED_SCATTERING (pbb), cstr);
      ppl_delete_Constraint (cstr);
      ppl_delete_Linear_Expression (expr);
    }

  ppl_delete_Coefficient (coef);
  value_clear (one);
}

/* All scattering matrices in SCOP will have the same number of scattering
   dimensions.  */

int
unify_scattering_dimensions (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  graphite_dim_t max_scattering = 0;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    max_scattering = MAX (pbb_nb_scattering_transform (pbb), max_scattering);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    extend_scattering (pbb, max_scattering);

  return max_scattering;
}

/* Prints to FILE the scattering function of PBB.  */

void
print_scattering_function (FILE *file, poly_bb_p pbb)
{
  graphite_dim_t i;

  if (!PBB_TRANSFORMED_SCATTERING (pbb))
    return;

  fprintf (file, "scattering bb_%d (\n", GBB_BB (PBB_BLACK_BOX (pbb))->index);
  fprintf (file, "#  eq");

  for (i = 0; i < pbb_nb_scattering_transform (pbb); i++)
    fprintf (file, "     s%d", (int) i);

  for (i = 0; i < pbb_nb_local_vars (pbb); i++)
    fprintf (file, "    lv%d", (int) i);

  for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
    fprintf (file, "     i%d", (int) i);

  for (i = 0; i < pbb_nb_params (pbb); i++)
    fprintf (file, "     p%d", (int) i);

  fprintf (file, "    cst\n");

  ppl_print_polyhedron_matrix (file, PBB_TRANSFORMED_SCATTERING (pbb));

  fprintf (file, ")\n");
}

/* Prints to FILE the iteration domain of PBB.  */

void
print_iteration_domain (FILE *file, poly_bb_p pbb)
{
  print_pbb_domain (file, pbb);
}

/* Prints to FILE the scattering functions of every PBB of SCOP.  */

void
print_scattering_functions (FILE *file, scop_p scop)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    print_scattering_function (file, pbb);
}

/* Prints to FILE the iteration domains of every PBB of SCOP.  */

void
print_iteration_domains (FILE *file, scop_p scop)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    print_iteration_domain (file, pbb);
}

/* Prints to STDERR the scattering function of PBB.  */

void
debug_scattering_function (poly_bb_p pbb)
{
  print_scattering_function (stderr, pbb);
}

/* Prints to STDERR the iteration domain of PBB.  */

void
debug_iteration_domain (poly_bb_p pbb)
{
  print_iteration_domain (stderr, pbb);
}

/* Prints to STDERR the scattering functions of every PBB of SCOP.  */

void
debug_scattering_functions (scop_p scop)
{
  print_scattering_functions (stderr, scop);
}

/* Prints to STDERR the iteration domains of every PBB of SCOP.  */

void
debug_iteration_domains (scop_p scop)
{
  print_iteration_domains (stderr, scop);
}

/* Apply graphite transformations to all the basic blocks of SCOP.  */

bool
apply_poly_transforms (scop_p scop)
{
  bool transform_done = false;

  gcc_assert (graphite_legal_transform (scop));

  /* Generate code even if we did not apply any real transformation.
     This also allows to check the performance for the identity
     transformation: GIMPLE -> GRAPHITE -> GIMPLE
     Keep in mind that CLooG optimizes in control, so the loop structure
     may change, even if we only use -fgraphite-identity.  */
  if (flag_graphite_identity)
    transform_done = true;

  if (flag_graphite_force_parallel)
    transform_done = true;

  if (flag_loop_block)
    gcc_unreachable (); /* Not yet supported.  */

  if (flag_loop_strip_mine)
    {
      transform_done |= scop_do_strip_mine (scop);
      gcc_assert (graphite_legal_transform (scop));
    }

  if (flag_loop_interchange)
    {
      transform_done |= scop_do_interchange (scop);
      gcc_assert (graphite_legal_transform (scop));
    }

  return transform_done;
}

/* Create a new polyhedral data reference and add it to PBB. It is defined by
   its ACCESSES, its TYPE*/

void
new_poly_dr (poly_bb_p pbb,
	     ppl_Pointset_Powerset_C_Polyhedron_t accesses,
	     ppl_Pointset_Powerset_C_Polyhedron_t data_container,
	     enum POLY_DR_TYPE type, void *cdr)
{
  poly_dr_p pdr = XNEW (struct poly_dr);

  PDR_PBB (pdr) = pbb;
  PDR_ACCESSES (pdr) = accesses;
  PDR_DATA_CONTAINER (pdr) = data_container;
  PDR_TYPE (pdr) = type;
  PDR_CDR (pdr) = cdr;
  VEC_safe_push (poly_dr_p, heap, PBB_DRS (pbb), pdr);
}

/* Free polyhedral data reference PDR.  */

void
free_poly_dr (poly_dr_p pdr)
{
  ppl_delete_Pointset_Powerset_C_Polyhedron (PDR_ACCESSES (pdr));
  ppl_delete_Pointset_Powerset_C_Polyhedron (PDR_DATA_CONTAINER (pdr));

  XDELETE (pdr);
}

/* Create a new polyhedral black box.  */

void
new_poly_bb (scop_p scop, void *black_box)
{
  poly_bb_p pbb = XNEW (struct poly_bb);

  PBB_DOMAIN (pbb) = NULL;
  PBB_SCOP (pbb) = scop;
  pbb_set_black_box (pbb, black_box);
  PBB_TRANSFORMED_SCATTERING (pbb) = NULL;
  PBB_ORIGINAL_SCATTERING (pbb) = NULL;
  PBB_DRS (pbb) = VEC_alloc (poly_dr_p, heap, 3);
  PBB_NB_SCATTERING_TRANSFORM (pbb) = 0;
  PBB_NB_LOCAL_VARIABLES (pbb) = 0;
  VEC_safe_push (poly_bb_p, heap, SCOP_BBS (scop), pbb);
}

/* Free polyhedral black box.  */

void
free_poly_bb (poly_bb_p pbb)
{
  int i;
  poly_dr_p pdr;

  ppl_delete_Pointset_Powerset_C_Polyhedron (PBB_DOMAIN (pbb));

  if (PBB_TRANSFORMED_SCATTERING (pbb))
    ppl_delete_Polyhedron (PBB_TRANSFORMED_SCATTERING (pbb));

  if (PBB_ORIGINAL_SCATTERING (pbb))
    ppl_delete_Polyhedron (PBB_ORIGINAL_SCATTERING (pbb));

  if (PBB_DRS (pbb))
    for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr); i++)
      free_poly_dr (pdr);

  VEC_free (poly_dr_p, heap, PBB_DRS (pbb));
  XDELETE (pbb);
}

static void
print_pdr_access_layout (FILE *file, poly_dr_p pdr)
{
  graphite_dim_t i;

  fprintf (file, "#  eq");

  for (i = 0; i < pdr_dim_iter_domain (pdr); i++)
    fprintf (file, "     i%d", (int) i);

  for (i = 0; i < pdr_nb_params (pdr); i++)
    fprintf (file, "     p%d", (int) i);

  fprintf (file, "  alias");

  for (i = 0; i < pdr_nb_subscripts (pdr); i++)
    fprintf (file, "   sub%d", (int) i);

  fprintf (file, "    cst\n");
}

/* Prints to FILE the polyhedral data reference PDR.  */

void
print_pdr (FILE *file, poly_dr_p pdr)
{
  fprintf (file, "pdr (");

  switch (PDR_TYPE (pdr))
    {
    case PDR_READ:
      fprintf (file, "read \n");
      break;

    case PDR_WRITE:
      fprintf (file, "write \n");
      break;

    case PDR_MAY_WRITE:
      fprintf (file, "may_write \n");
      break;

    default:
      gcc_unreachable ();
    }

  dump_data_reference (file, (data_reference_p) PDR_CDR (pdr));

  fprintf (file, "data accesses (\n");
  print_pdr_access_layout (file, pdr);
  ppl_print_powerset_matrix (file, PDR_ACCESSES (pdr));
  fprintf (file, ")\n");

  fprintf (file, "data container (\n");
  print_pdr_access_layout (file, pdr);
  ppl_print_powerset_matrix (file, PDR_DATA_CONTAINER (pdr));
  fprintf (file, ")\n");

  fprintf (file, ")\n");
}

/* Prints to STDERR the polyhedral data reference PDR.  */

void
debug_pdr (poly_dr_p pdr)
{
  print_pdr (stderr, pdr);
}

/* Creates a new SCOP containing REGION.  */

scop_p
new_scop (void *region)
{
  scop_p scop = XNEW (struct scop);

  SCOP_DEP_GRAPH (scop) = NULL;
  SCOP_CONTEXT (scop) = NULL;
  scop_set_region (scop, region);
  SCOP_BBS (scop) = VEC_alloc (poly_bb_p, heap, 3);
  SCOP_ORIGINAL_PDR_PAIRS (scop) = htab_create (10, hash_poly_dr_pair_p,
                                                eq_poly_dr_pair_p, free);
  return scop;
}

/* Deletes SCOP.  */

void
free_scop (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    free_poly_bb (pbb);

  VEC_free (poly_bb_p, heap, SCOP_BBS (scop));

  if (SCOP_CONTEXT (scop))
    ppl_delete_Pointset_Powerset_C_Polyhedron (SCOP_CONTEXT (scop));

  htab_delete (SCOP_ORIGINAL_PDR_PAIRS (scop));
  XDELETE (scop);
}

/* Print to FILE the domain of PBB.  */

void
print_pbb_domain (FILE *file, poly_bb_p pbb)
{
  graphite_dim_t i;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (!PBB_DOMAIN (pbb))
    return;

  fprintf (file, "domains bb_%d (\n", GBB_BB (gbb)->index);
  fprintf (file, "#  eq");

  for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
    fprintf (file, "     i%d", (int) i);

  for (i = 0; i < pbb_nb_params (pbb); i++)
    fprintf (file, "     p%d", (int) i);

  fprintf (file, "    cst\n");

  if (PBB_DOMAIN (pbb))
    ppl_print_powerset_matrix (file, PBB_DOMAIN (pbb));

  fprintf (file, ")\n");
}

/* Dump the cases of a graphite basic block GBB on FILE.  */

static void
dump_gbb_cases (FILE *file, gimple_bb_p gbb)
{
  int i;
  gimple stmt;
  VEC (gimple, heap) *cases;

  if (!gbb)
    return;

  cases = GBB_CONDITION_CASES (gbb);
  if (VEC_empty (gimple, cases))
    return;

  fprintf (file, "cases bb_%d (", GBB_BB (gbb)->index);

  for (i = 0; VEC_iterate (gimple, cases, i, stmt); i++)
    print_gimple_stmt (file, stmt, 0, 0);

  fprintf (file, ")\n");
}

/* Dump conditions of a graphite basic block GBB on FILE.  */

static void
dump_gbb_conditions (FILE *file, gimple_bb_p gbb)
{
  int i;
  gimple stmt;
  VEC (gimple, heap) *conditions;

  if (!gbb)
    return;

  conditions = GBB_CONDITIONS (gbb);
  if (VEC_empty (gimple, conditions))
    return;

  fprintf (file, "conditions bb_%d (", GBB_BB (gbb)->index);

  for (i = 0; VEC_iterate (gimple, conditions, i, stmt); i++)
    print_gimple_stmt (file, stmt, 0, 0);

  fprintf (file, ")\n");
}

/* Print to FILE all the data references of PBB.  */

void
print_pdrs (FILE *file, poly_bb_p pbb)
{
  int i;
  poly_dr_p pdr;

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr); i++)
    print_pdr (file, pdr);
}

/* Print to STDERR all the data references of PBB.  */

void
debug_pdrs (poly_bb_p pbb)
{
  print_pdrs (stderr, pbb);
}

/* Print to FILE the domain and scattering function of PBB.  */

void
print_pbb (FILE *file, poly_bb_p pbb)
{
  dump_gbb_conditions (file, PBB_BLACK_BOX (pbb));
  dump_gbb_cases (file, PBB_BLACK_BOX (pbb));
  print_pdrs (file, pbb);
  print_pbb_domain (file, pbb);
  print_scattering_function (file, pbb);
}

/* Print to FILE the parameters of SCOP.  */

void
print_scop_params (FILE *file, scop_p scop)
{
  int i;
  tree t;

  fprintf (file, "parameters (\n");
  for (i = 0; VEC_iterate (tree, SESE_PARAMS (SCOP_REGION (scop)), i, t); i++)
    {
      fprintf (file, "p_%d -> ", i);
      print_generic_expr (file, t, 0);
      fprintf (file, "\n");
    }
  fprintf (file, ")\n");
}

/* Print to FILE the context of SCoP.  */
void
print_scop_context (FILE *file, scop_p scop)
{
  graphite_dim_t i;

  fprintf (file, "context (\n");
  fprintf (file, "#  eq");

  for (i = 0; i < scop_nb_params (scop); i++)
    fprintf (file, "     p%d", (int) i);

  fprintf (file, "    cst\n");

  if (SCOP_CONTEXT (scop))
    ppl_print_powerset_matrix (file, SCOP_CONTEXT (scop));

  fprintf (file, ")\n");
}

/* Print to FILE the SCOP.  */

void
print_scop (FILE *file, scop_p scop)
{
  int i;
  poly_bb_p pbb;

  print_scop_params (file, scop);
  print_scop_context (file, scop);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    print_pbb (file, pbb);
}

/* Print to STDERR the domain of PBB.  */

void
debug_pbb_domain (poly_bb_p pbb)
{
  print_pbb_domain (stderr, pbb);
}

/* Print to FILE the domain and scattering function of PBB.  */

void
debug_pbb (poly_bb_p pbb)
{
  print_pbb (stderr, pbb);
}

/* Print to STDERR the context of SCOP.  */

void
debug_scop_context (scop_p scop)
{
  print_scop_context (stderr, scop);
}

/* Print to STDERR the SCOP.  */

void
debug_scop (scop_p scop)
{
  print_scop (stderr, scop);
}

/* Print to STDERR the parameters of SCOP.  */

void
debug_scop_params (scop_p scop)
{
  print_scop_params (stderr, scop);
}


/* The dimension in the transformed scattering polyhedron of PBB
   containing the scattering iterator for the loop at depth LOOP_DEPTH.  */

ppl_dimension_type
psct_scattering_dim_for_loop_depth (poly_bb_p pbb, graphite_dim_t loop_depth)
{
  ppl_const_Constraint_System_t pcs;
  ppl_Constraint_System_const_iterator_t cit, cend;
  ppl_const_Constraint_t cstr;
  ppl_Polyhedron_t ph = PBB_TRANSFORMED_SCATTERING (pbb);
  ppl_dimension_type iter = psct_iterator_dim (pbb, loop_depth);
  ppl_Linear_Expression_t expr;
  ppl_Coefficient_t coef;
  Value val;
  graphite_dim_t i;

  value_init (val);
  ppl_new_Coefficient (&coef);
  ppl_Polyhedron_get_constraints (ph, &pcs);
  ppl_new_Constraint_System_const_iterator (&cit);
  ppl_new_Constraint_System_const_iterator (&cend);

  for (ppl_Constraint_System_begin (pcs, cit),
	 ppl_Constraint_System_end (pcs, cend);
       !ppl_Constraint_System_const_iterator_equal_test (cit, cend);
       ppl_Constraint_System_const_iterator_increment (cit))
    {
      ppl_Constraint_System_const_iterator_dereference (cit, &cstr);
      ppl_new_Linear_Expression_from_Constraint (&expr, cstr);
      ppl_Linear_Expression_coefficient (expr, iter, coef);
      ppl_Coefficient_to_mpz_t (coef, val);

      if (value_zero_p (val))
	{
	  ppl_delete_Linear_Expression (expr);
	  continue;
	}

      for (i = 0; i < pbb_nb_scattering_transform (pbb); i++)
	{
	  ppl_dimension_type scatter = psct_scattering_dim (pbb, i);

	  ppl_Linear_Expression_coefficient (expr, scatter, coef);
	  ppl_Coefficient_to_mpz_t (coef, val);

	  if (value_notzero_p (val))
	    {
	      value_clear (val);
	      ppl_delete_Linear_Expression (expr);
	      ppl_delete_Coefficient (coef);
	      ppl_delete_Constraint_System_const_iterator (cit);
	      ppl_delete_Constraint_System_const_iterator (cend);

	      return scatter;
	    }
	}
    }

  gcc_unreachable ();
}

/* Returns the number of iterations NITER of the loop around PBB at
   depth LOOP_DEPTH.  */

void
pbb_number_of_iterations (poly_bb_p pbb,
			  graphite_dim_t loop_depth,
			  Value niter)
{
  ppl_dimension_type loop_iter = pbb_iterator_dim (pbb, loop_depth);
  ppl_Linear_Expression_t le;
  ppl_Coefficient_t num, denom;
  Value dv;
  int maximum;
  ppl_dimension_type dim;

  value_init (dv);
  ppl_new_Coefficient (&num);
  ppl_new_Coefficient (&denom);
  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (PBB_DOMAIN (pbb), &dim);
  ppl_new_Linear_Expression_with_dimension (&le, dim);
  ppl_set_coef (le, loop_iter, 1);
  ppl_Pointset_Powerset_C_Polyhedron_maximize (PBB_DOMAIN (pbb), le,
					       num, denom, &maximum);

  if (maximum == 1)
    {
      ppl_Coefficient_to_mpz_t (num, niter);
      ppl_Coefficient_to_mpz_t (denom, dv);
      value_division (niter, niter, dv);
    }
  else
    value_set_si (niter, -1);

  value_clear (dv);
  ppl_delete_Linear_Expression (le);
  ppl_delete_Coefficient (num);
  ppl_delete_Coefficient (denom);
}

#endif

