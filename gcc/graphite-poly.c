/* Graphite polyhedral representation.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

/* Prints to FILE the scattering function of PBB, at some VERBOSITY
   level.  */

static void
print_scattering_function_1 (FILE *file, poly_bb_p pbb, int verbosity)
{
  graphite_dim_t i;

  if (verbosity > 0)
    {
      fprintf (file, "# scattering bb_%d (\n", pbb_index (pbb));
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
    }

  /* Number of disjunct components.  Remove this when
     PBB_TRANSFORMED_SCATTERING will be a pointset_powerset.  */
  fprintf (file, "1\n");
  ppl_print_polyhedron_matrix (file, PBB_TRANSFORMED_SCATTERING (pbb)
			       ? PBB_TRANSFORMED_SCATTERING (pbb)
			       : PBB_ORIGINAL_SCATTERING (pbb));

  if (verbosity > 0)
    fprintf (file, "#)\n");
}

/* Prints to FILE the scattering function of PBB, at some VERBOSITY
   level.  */

void
print_scattering_function (FILE *file, poly_bb_p pbb, int verbosity)
{
  if (!PBB_TRANSFORMED (pbb))
    return;

  if (PBB_TRANSFORMED_SCATTERING (pbb)
      || PBB_ORIGINAL_SCATTERING (pbb))
    {
      if (verbosity > 0)
	fprintf (file, "# Scattering function is provided\n");

      fprintf (file, "1\n");
    }
  else
    {
      if (verbosity > 0)
	fprintf (file, "# Scattering function is not provided\n");

      fprintf (file, "0\n");
      return;
    }

  print_scattering_function_1 (file, pbb, verbosity);
}

/* Prints to FILE the iteration domain of PBB, at some VERBOSITY
   level.  */

void
print_iteration_domain (FILE *file, poly_bb_p pbb, int verbosity)
{
  print_pbb_domain (file, pbb, verbosity);
}

/* Prints to FILE the scattering functions of every PBB of SCOP.  */

void
print_scattering_functions (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    print_scattering_function (file, pbb, verbosity);
}

/* Prints to FILE the iteration domains of every PBB of SCOP, at some
   VERBOSITY level.  */

void
print_iteration_domains (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    print_iteration_domain (file, pbb, verbosity);
}

/* Prints to STDERR the scattering function of PBB, at some VERBOSITY
   level.  */

void
debug_scattering_function (poly_bb_p pbb, int verbosity)
{
  print_scattering_function (stderr, pbb, verbosity);
}

/* Prints to STDERR the iteration domain of PBB, at some VERBOSITY
   level.  */

void
debug_iteration_domain (poly_bb_p pbb, int verbosity)
{
  print_iteration_domain (stderr, pbb, verbosity);
}

/* Prints to STDERR the scattering functions of every PBB of SCOP, at
   some VERBOSITY level.  */

void
debug_scattering_functions (scop_p scop, int verbosity)
{
  print_scattering_functions (stderr, scop, verbosity);
}

/* Prints to STDERR the iteration domains of every PBB of SCOP, at
   some VERBOSITY level.  */

void
debug_iteration_domains (scop_p scop, int verbosity)
{
  print_iteration_domains (stderr, scop, verbosity);
}


/* Apply graphite transformations to all the basic blocks of SCOP.  */

bool
apply_poly_transforms (scop_p scop)
{
  bool transform_done = false;

  /* Generate code even if we did not apply any real transformation.
     This also allows to check the performance for the identity
     transformation: GIMPLE -> GRAPHITE -> GIMPLE
     Keep in mind that CLooG optimizes in control, so the loop structure
     may change, even if we only use -fgraphite-identity.  */
  if (flag_graphite_identity)
    transform_done = true;

  if (flag_loop_parallelize_all)
    transform_done = true;

  if (flag_loop_block)
    transform_done |= scop_do_block (scop);
  else
    {
      if (flag_loop_strip_mine)
	transform_done |= scop_do_strip_mine (scop);

      if (flag_loop_interchange)
	transform_done |= scop_do_interchange (scop);
    }

  return transform_done;
}

/* Returns true when it PDR1 is a duplicate of PDR2: same PBB, and
   their ACCESSES, TYPE, and NB_SUBSCRIPTS are the same.  */

static inline bool
can_collapse_pdrs (poly_dr_p pdr1, poly_dr_p pdr2)
{
  bool res;
  ppl_Pointset_Powerset_C_Polyhedron_t af1, af2, diff;

  if (PDR_PBB (pdr1) != PDR_PBB (pdr2)
      || PDR_NB_SUBSCRIPTS (pdr1) != PDR_NB_SUBSCRIPTS (pdr2)
      || PDR_TYPE (pdr1) != PDR_TYPE (pdr2))
    return false;

  af1 = PDR_ACCESSES (pdr1);
  af2 = PDR_ACCESSES (pdr2);
  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&diff, af1);
  ppl_Pointset_Powerset_C_Polyhedron_difference_assign (diff, af2);

  res = ppl_Pointset_Powerset_C_Polyhedron_is_empty (diff);
  ppl_delete_Pointset_Powerset_C_Polyhedron (diff);
  return res;
}

/* Removes duplicated data references in PBB.  */

void
pbb_remove_duplicate_pdrs (poly_bb_p pbb)
{
  int i, j;
  poly_dr_p pdr1, pdr2;
  unsigned n = VEC_length (poly_dr_p, PBB_DRS (pbb));
  VEC (poly_dr_p, heap) *collapsed = VEC_alloc (poly_dr_p, heap, n);

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr1); i++)
    for (j = 0; VEC_iterate (poly_dr_p, collapsed, j, pdr2); j++)
      if (!can_collapse_pdrs (pdr1, pdr2))
	VEC_quick_push (poly_dr_p, collapsed, pdr1);

  VEC_free (poly_dr_p, heap, collapsed);
  PBB_PDR_DUPLICATES_REMOVED (pbb) = true;
}

/* Create a new polyhedral data reference and add it to PBB.  It is
   defined by its ACCESSES, its TYPE, and the number of subscripts
   NB_SUBSCRIPTS.  */

void
new_poly_dr (poly_bb_p pbb, int dr_base_object_set,
	     ppl_Pointset_Powerset_C_Polyhedron_t accesses,
	     enum poly_dr_type type, void *cdr, graphite_dim_t nb_subscripts)
{
  static int id = 0;
  poly_dr_p pdr = XNEW (struct poly_dr);

  PDR_ID (pdr) = id++;
  PDR_BASE_OBJECT_SET (pdr) = dr_base_object_set;
  PDR_NB_REFS (pdr) = 1;
  PDR_PBB (pdr) = pbb;
  PDR_ACCESSES (pdr) = accesses;
  PDR_TYPE (pdr) = type;
  PDR_CDR (pdr) = cdr;
  PDR_NB_SUBSCRIPTS (pdr) = nb_subscripts;
  VEC_safe_push (poly_dr_p, heap, PBB_DRS (pbb), pdr);
}

/* Free polyhedral data reference PDR.  */

void
free_poly_dr (poly_dr_p pdr)
{
  ppl_delete_Pointset_Powerset_C_Polyhedron (PDR_ACCESSES (pdr));
  XDELETE (pdr);
}

/* Create a new polyhedral black box.  */

void
new_poly_bb (scop_p scop, void *black_box, bool reduction)
{
  poly_bb_p pbb = XNEW (struct poly_bb);

  PBB_DOMAIN (pbb) = NULL;
  PBB_SCOP (pbb) = scop;
  pbb_set_black_box (pbb, black_box);
  PBB_TRANSFORMED (pbb) = NULL;
  PBB_SAVED (pbb) = NULL;
  PBB_ORIGINAL (pbb) = NULL;
  PBB_DRS (pbb) = VEC_alloc (poly_dr_p, heap, 3);
  PBB_IS_REDUCTION (pbb) = reduction;
  PBB_PDR_DUPLICATES_REMOVED (pbb) = false;
  VEC_safe_push (poly_bb_p, heap, SCOP_BBS (scop), pbb);
}

/* Free polyhedral black box.  */

void
free_poly_bb (poly_bb_p pbb)
{
  int i;
  poly_dr_p pdr;

  ppl_delete_Pointset_Powerset_C_Polyhedron (PBB_DOMAIN (pbb));

  if (PBB_TRANSFORMED (pbb))
    poly_scattering_free (PBB_TRANSFORMED (pbb));

  if (PBB_SAVED (pbb))
    poly_scattering_free (PBB_SAVED (pbb));

  if (PBB_ORIGINAL (pbb))
    poly_scattering_free (PBB_ORIGINAL (pbb));

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

  for (i = 0; i < PDR_NB_SUBSCRIPTS (pdr); i++)
    fprintf (file, "   sub%d", (int) i);

  fprintf (file, "    cst\n");
}

/* Prints to FILE the polyhedral data reference PDR, at some VERBOSITY
   level.  */

void
print_pdr (FILE *file, poly_dr_p pdr, int verbosity)
{
  if (verbosity > 1)
    {
      fprintf (file, "# pdr_%d (", PDR_ID (pdr));

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
    }

  if (verbosity > 0)
    {
      fprintf (file, "# data accesses (\n");
      print_pdr_access_layout (file, pdr);
    }

  ppl_print_powerset_matrix (file, PDR_ACCESSES (pdr));

  if (verbosity > 0)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Prints to STDERR the polyhedral data reference PDR, at some
   VERBOSITY level.  */

void
debug_pdr (poly_dr_p pdr, int verbosity)
{
  print_pdr (stderr, pdr, verbosity);
}

/* Creates a new SCOP containing REGION.  */

scop_p
new_scop (void *region)
{
  scop_p scop = XNEW (struct scop);

  SCOP_CONTEXT (scop) = NULL;
  scop_set_region (scop, region);
  SCOP_BBS (scop) = VEC_alloc (poly_bb_p, heap, 3);
  SCOP_ORIGINAL_PDDRS (scop) = htab_create (10, hash_poly_ddr_p,
					    eq_poly_ddr_p, free_poly_ddr);
  SCOP_ORIGINAL_SCHEDULE (scop) = NULL;
  SCOP_TRANSFORMED_SCHEDULE (scop) = NULL;
  SCOP_SAVED_SCHEDULE (scop) = NULL;
  POLY_SCOP_P (scop) = false;

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

  htab_delete (SCOP_ORIGINAL_PDDRS (scop));
  free_lst (SCOP_ORIGINAL_SCHEDULE (scop));
  free_lst (SCOP_TRANSFORMED_SCHEDULE (scop));
  free_lst (SCOP_SAVED_SCHEDULE (scop));
  XDELETE (scop);
}

/* Print to FILE the domain of PBB, at some VERBOSITY level.  */

void
print_pbb_domain (FILE *file, poly_bb_p pbb, int verbosity)
{
  graphite_dim_t i;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (!PBB_DOMAIN (pbb))
    return;

  if (verbosity > 0)
    {
      fprintf (file, "# Iteration domain of bb_%d (\n", GBB_BB (gbb)->index);
      fprintf (file, "#  eq");

      for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
	fprintf (file, "     i%d", (int) i);

      for (i = 0; i < pbb_nb_params (pbb); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (PBB_DOMAIN (pbb))
    ppl_print_powerset_matrix (file, PBB_DOMAIN (pbb));
  else
    fprintf (file, "0\n");

  if (verbosity > 0)
    fprintf (file, "#)\n");
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

  fprintf (file, "# cases bb_%d (\n", GBB_BB (gbb)->index);

  for (i = 0; VEC_iterate (gimple, cases, i, stmt); i++)
    {
      fprintf (file, "# ");
      print_gimple_stmt (file, stmt, 0, 0);
    }

  fprintf (file, "#)\n");
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

  fprintf (file, "# conditions bb_%d (\n", GBB_BB (gbb)->index);

  for (i = 0; VEC_iterate (gimple, conditions, i, stmt); i++)
    {
      fprintf (file, "# ");
      print_gimple_stmt (file, stmt, 0, 0);
    }

  fprintf (file, "#)\n");
}

/* Print to FILE all the data references of PBB, at some VERBOSITY
   level.  */

void
print_pdrs (FILE *file, poly_bb_p pbb, int verbosity)
{
  int i;
  poly_dr_p pdr;
  int nb_reads = 0;
  int nb_writes = 0;

  if (VEC_length (poly_dr_p, PBB_DRS (pbb)) == 0)
    {
      if (verbosity > 0)
	fprintf (file, "# Access informations are not provided\n");\
      fprintf (file, "0\n");
      return;
    }

  if (verbosity > 1)
    fprintf (file, "# Data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Access informations are provided\n");
  fprintf (file, "1\n");

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr); i++)
    if (PDR_TYPE (pdr) == PDR_READ)
      nb_reads++;
    else
      nb_writes++;

  if (verbosity > 1)
    fprintf (file, "# Read data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Read access informations\n");
  fprintf (file, "%d\n", nb_reads);

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr); i++)
    if (PDR_TYPE (pdr) == PDR_READ)
      print_pdr (file, pdr, verbosity);

  if (verbosity > 1)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "# Write data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Write access informations\n");
  fprintf (file, "%d\n", nb_writes);

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr); i++)
    if (PDR_TYPE (pdr) != PDR_READ)
      print_pdr (file, pdr, verbosity);

  if (verbosity > 1)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to STDERR all the data references of PBB.  */

void
debug_pdrs (poly_bb_p pbb, int verbosity)
{
  print_pdrs (stderr, pbb, verbosity);
}

/* Print to FILE the body of PBB, at some VERBOSITY level.  */

static void
print_pbb_body (FILE *file, poly_bb_p pbb, int verbosity)
{
  if (verbosity > 1)
    fprintf (file, "# Body (\n");

  if (verbosity > 0)
    fprintf (file, "# Statement body is provided\n");
  fprintf (file, "1\n");

  if (verbosity > 0)
    fprintf (file, "# Original iterator names\n# Iterator names are not provided yet.\n");

  if (verbosity > 0)
    fprintf (file, "# Statement body\n");

  fprintf (file, "{\n");
  dump_bb (pbb_bb (pbb), file, 0);
  fprintf (file, "}\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to FILE the domain and scattering function of PBB, at some
   VERBOSITY level.  */

void
print_pbb (FILE *file, poly_bb_p pbb, int verbosity)
{
  if (verbosity > 1)
    {
      fprintf (file, "# pbb_%d (\n", pbb_index (pbb));
      dump_gbb_conditions (file, PBB_BLACK_BOX (pbb));
      dump_gbb_cases (file, PBB_BLACK_BOX (pbb));
    }

  print_pbb_domain (file, pbb, verbosity);
  print_scattering_function (file, pbb, verbosity);
  print_pdrs (file, pbb, verbosity);
  print_pbb_body (file, pbb, verbosity);

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to FILE the parameters of SCOP, at some VERBOSITY level.  */

void
print_scop_params (FILE *file, scop_p scop, int verbosity)
{
  int i;
  tree t;

  if (verbosity > 1)
    fprintf (file, "# parameters (\n");

  if (VEC_length (tree, SESE_PARAMS (SCOP_REGION (scop))))
    {
      if (verbosity > 0)
	fprintf (file, "# Parameter names are provided\n");

      fprintf (file, "1\n");

      if (verbosity > 0)
	fprintf (file, "# Parameter names\n");
    }
  else
    {
      if (verbosity > 0)
	fprintf (file, "# Parameter names are not provided\n");
      fprintf (file, "0\n");
    }

  for (i = 0; VEC_iterate (tree, SESE_PARAMS (SCOP_REGION (scop)), i, t); i++)
    {
      print_generic_expr (file, t, 0);
      fprintf (file, " ");
    }

  fprintf (file, "\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to FILE the context of SCoP, at some VERBOSITY level.  */

void
print_scop_context (FILE *file, scop_p scop, int verbosity)
{
  graphite_dim_t i;

  if (verbosity > 0)
    {
      fprintf (file, "# Context (\n");
      fprintf (file, "#  eq");

      for (i = 0; i < scop_nb_params (scop); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (SCOP_CONTEXT (scop))
    ppl_print_powerset_matrix (file, SCOP_CONTEXT (scop));
  else
    fprintf (file, "0 %d\n", (int) scop_nb_params (scop) + 2);

  if (verbosity > 0)
    fprintf (file, "# )\n");
}

/* Print to FILE the SCOP, at some VERBOSITY level.  */

void
print_scop (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  fprintf (file, "SCoP #(\n");
  fprintf (file, "# Language\nGimple\n");
  print_scop_context (file, scop, verbosity);
  print_scop_params (file, scop, verbosity);

  if (verbosity > 0)
    fprintf (file, "# Number of statements\n");

  fprintf (file, "%d\n",VEC_length (poly_bb_p, SCOP_BBS (scop)));

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    print_pbb (file, pbb, verbosity);

  if (verbosity > 1)
    {
      fprintf (file, "# original_lst (\n");
      print_lst (file, SCOP_ORIGINAL_SCHEDULE (scop), 0);
      fprintf (file, "\n#)\n");

      fprintf (file, "# transformed_lst (\n");
      print_lst (file, SCOP_TRANSFORMED_SCHEDULE (scop), 0);
      fprintf (file, "\n#)\n");
    }

  fprintf (file, "#)\n");
}

/* Print to FILE the input file that CLooG would expect as input, at
   some VERBOSITY level.  */

void
print_cloog (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  fprintf (file, "# SCoP (generated by GCC/Graphite\n");
  if (verbosity > 0)
    fprintf (file, "# CLooG output language\n");
  fprintf (file, "c\n");

  print_scop_context (file, scop, verbosity);
  print_scop_params (file, scop, verbosity);

  if (verbosity > 0)
    fprintf (file, "# Number of statements\n");

  fprintf (file, "%d\n", VEC_length (poly_bb_p, SCOP_BBS (scop)));

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    {
      if (verbosity > 1)
	fprintf (file, "# pbb_%d (\n", pbb_index (pbb));

      print_pbb_domain (file, pbb, verbosity);
      fprintf (file, "0 0 0");

      if (verbosity > 0)
	fprintf (file, "# For future CLooG options.\n");
      else
	fprintf (file, "\n");

      if (verbosity > 1)
	fprintf (file, "#)\n");
    }

  fprintf (file, "0");
  if (verbosity > 0)
    fprintf (file, "# Don't set the iterator names.\n");
  else
    fprintf (file, "\n");

  if (verbosity > 0)
    fprintf (file, "# Number of scattering functions\n");

  fprintf (file, "%d\n", VEC_length (poly_bb_p, SCOP_BBS (scop)));
  unify_scattering_dimensions (scop);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    {
      if (!PBB_TRANSFORMED (pbb)
	  || !(PBB_TRANSFORMED_SCATTERING (pbb)
	       || PBB_ORIGINAL_SCATTERING (pbb)))
	continue;

      if (verbosity > 1)
	fprintf (file, "# pbb_%d (\n", pbb_index (pbb));

      print_scattering_function_1 (file, pbb, verbosity);

      if (verbosity > 1)
	fprintf (file, "#)\n");
    }

  fprintf (file, "0");
  if (verbosity > 0)
    fprintf (file, "# Don't set the scattering dimension names.\n");
  else
    fprintf (file, "\n");

  fprintf (file, "#)\n");
}

/* Print to STDERR the domain of PBB, at some VERBOSITY level.  */

void
debug_pbb_domain (poly_bb_p pbb, int verbosity)
{
  print_pbb_domain (stderr, pbb, verbosity);
}

/* Print to FILE the domain and scattering function of PBB, at some
   VERBOSITY level.  */

void
debug_pbb (poly_bb_p pbb, int verbosity)
{
  print_pbb (stderr, pbb, verbosity);
}

/* Print to STDERR the context of SCOP, at some VERBOSITY level.  */

void
debug_scop_context (scop_p scop, int verbosity)
{
  print_scop_context (stderr, scop, verbosity);
}

/* Print to STDERR the SCOP, at some VERBOSITY level.  */

void
debug_scop (scop_p scop, int verbosity)
{
  print_scop (stderr, scop, verbosity);
}

/* Print to STDERR the SCOP under CLooG format, at some VERBOSITY
   level.  */

void
debug_cloog (scop_p scop, int verbosity)
{
  print_cloog (stderr, scop, verbosity);
}

/* Print to STDERR the parameters of SCOP, at some VERBOSITY
   level.  */

void
debug_scop_params (scop_p scop, int verbosity)
{
  print_scop_params (stderr, scop, verbosity);
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
  ppl_Linear_Expression_t le;
  ppl_dimension_type dim;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (PBB_DOMAIN (pbb), &dim);
  ppl_new_Linear_Expression_with_dimension (&le, dim);
  ppl_set_coef (le, pbb_iterator_dim (pbb, loop_depth), 1);
  value_set_si (niter, -1);
  ppl_max_for_le_pointset (PBB_DOMAIN (pbb), le, niter);
  ppl_delete_Linear_Expression (le);
}

/* Returns the number of iterations NITER of the loop around PBB at
   time(scattering) dimension TIME_DEPTH.  */

void
pbb_number_of_iterations_at_time (poly_bb_p pbb,
				  graphite_dim_t time_depth,
				  Value niter)
{
  ppl_Pointset_Powerset_C_Polyhedron_t ext_domain, sctr;
  ppl_Linear_Expression_t le;
  ppl_dimension_type dim;

  /* Takes together domain and scattering polyhedrons, and composes
     them into the bigger polyhedron that has the following format:

     t0..t_{n-1} | l0..l_{nlcl-1} | i0..i_{niter-1} | g0..g_{nparm-1}

     where
     | t0..t_{n-1} are time dimensions (scattering dimensions)
     | l0..l_{nclc-1} are local variables in scattering function
     | i0..i_{niter-1} are original iteration variables
     | g0..g_{nparam-1} are global parameters.  */

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&sctr,
      PBB_TRANSFORMED_SCATTERING (pbb));

  /* Extend the iteration domain with the scattering dimensions:
     0..0 | 0..0 | i0..i_{niter-1} | g0..g_{nparm-1}.   */
  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&ext_domain, PBB_DOMAIN (pbb));
  ppl_insert_dimensions_pointset (ext_domain, 0,
                                  pbb_nb_scattering_transform (pbb)
                                  + pbb_nb_local_vars (pbb));

  /* Add to sctr the extended domain.  */
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (sctr, ext_domain);

  /* Extract the number of iterations.  */
  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (sctr, &dim);
  ppl_new_Linear_Expression_with_dimension (&le, dim);
  ppl_set_coef (le, time_depth, 1);
  value_set_si (niter, -1);
  ppl_max_for_le_pointset (sctr, le, niter);

  ppl_delete_Linear_Expression (le);
  ppl_delete_Pointset_Powerset_C_Polyhedron (sctr);
  ppl_delete_Pointset_Powerset_C_Polyhedron (ext_domain);
}

/* Translates LOOP to LST.  */

static lst_p
loop_to_lst (loop_p loop, VEC (poly_bb_p, heap) *bbs, int *i)
{
  poly_bb_p pbb;
  VEC (lst_p, heap) *seq = VEC_alloc (lst_p, heap, 5);

  for (; VEC_iterate (poly_bb_p, bbs, *i, pbb); (*i)++)
    {
      lst_p stmt;
      basic_block bb = GBB_BB (PBB_BLACK_BOX (pbb));

      if (bb->loop_father == loop)
	stmt = new_lst_stmt (pbb);
      else if (flow_bb_inside_loop_p (loop, bb))
	{
	  loop_p next = loop->inner;

	  while (next && !flow_bb_inside_loop_p (next, bb))
	    next = next->next;

	  stmt = loop_to_lst (next, bbs, i);
	}
      else
	{
	  (*i)--;
	  return new_lst_loop (seq);
	}

      VEC_safe_push (lst_p, heap, seq, stmt);
    }

  return new_lst_loop (seq);
}

/* Reads the original scattering of the SCOP and returns an LST
   representing it.  */

void
scop_to_lst (scop_p scop)
{
  lst_p res;
  int i, n = VEC_length (poly_bb_p, SCOP_BBS (scop));
  VEC (lst_p, heap) *seq = VEC_alloc (lst_p, heap, 5);
  sese region = SCOP_REGION (scop);

  for (i = 0; i < n; i++)
    {
      poly_bb_p pbb = VEC_index (poly_bb_p, SCOP_BBS (scop), i);
      loop_p loop = outermost_loop_in_sese (region, GBB_BB (PBB_BLACK_BOX (pbb)));

      if (loop_in_sese_p (loop, region))
	res = loop_to_lst (loop, SCOP_BBS (scop), &i);
      else
	res = new_lst_stmt (pbb);

      VEC_safe_push (lst_p, heap, seq, res);
    }

  res = new_lst_loop (seq);
  SCOP_ORIGINAL_SCHEDULE (scop) = res;
  SCOP_TRANSFORMED_SCHEDULE (scop) = copy_lst (res);
}

/* Print to FILE on a new line COLUMN white spaces.  */

static void
lst_indent_to (FILE *file, int column)
{
  int i;

  if (column > 0)
    fprintf (file, "\n#");

  for (i = 0; i < column; i++)
    fprintf (file, " ");
}

/* Print LST to FILE with INDENT spaces of indentation.  */

void
print_lst (FILE *file, lst_p lst, int indent)
{
  if (!lst)
    return;

  lst_indent_to (file, indent);

  if (LST_LOOP_P (lst))
    {
      int i;
      lst_p l;

      if (LST_LOOP_FATHER (lst))
	fprintf (file, "%d (loop", lst_dewey_number (lst));
      else
	fprintf (file, "#(root");

      for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
	print_lst (file, l, indent + 2);

      fprintf (file, ")");
    }
  else
    fprintf (file, "%d stmt_%d", lst_dewey_number (lst), pbb_index (LST_PBB (lst)));
}

/* Print LST to STDERR.  */

void
debug_lst (lst_p lst)
{
  print_lst (stderr, lst, 0);
}

/* Pretty print to FILE the loop statement tree LST in DOT format.  */

static void
dot_lst_1 (FILE *file, lst_p lst)
{
  if (!lst)
    return;

  if (LST_LOOP_P (lst))
    {
      int i;
      lst_p l;

      if (!LST_LOOP_FATHER (lst))
	fprintf (file, "L -> L_%d_%d\n",
		 lst_depth (lst),
		 lst_dewey_number (lst));
      else
	fprintf (file, "L_%d_%d -> L_%d_%d\n",
		 lst_depth (LST_LOOP_FATHER (lst)),
		 lst_dewey_number (LST_LOOP_FATHER (lst)),
		 lst_depth (lst),
		 lst_dewey_number (lst));

      for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
	dot_lst_1 (file, l);
    }

  else
    fprintf (file, "L_%d_%d -> S_%d\n",
	     lst_depth (LST_LOOP_FATHER (lst)),
	     lst_dewey_number (LST_LOOP_FATHER (lst)),
	     pbb_index (LST_PBB (lst)));

}

/* Display the LST using dotty.  */

void
dot_lst (lst_p lst)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  int x;
  FILE *stream = fopen ("/tmp/lst.dot", "w");
  gcc_assert (stream);

  fputs ("digraph all {\n", stream);
  dot_lst_1 (stream, lst);
  fputs ("}\n\n", stream);
  fclose (stream);

  x = system ("dotty /tmp/lst.dot");
#else
  fputs ("digraph all {\n", stderr);
  dot_lst_1 (stderr, lst);
  fputs ("}\n\n", stderr);

#endif
}

#endif

