/* Graphite polyhedral representation.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
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

#ifdef HAVE_cloog
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
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
#include "tree-ssa-loop.h"
#include "dumpfile.h"
#include "gimple-pretty-print.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "graphite-poly.h"

#define OPENSCOP_MAX_STRING 256


/* Print to STDERR the GMP value VAL.  */

DEBUG_FUNCTION void
debug_gmp_value (mpz_t val)
{
  gmp_fprintf (stderr, "%Zd", val);
}

/* Return the maximal loop depth in SCOP.  */

int
scop_max_loop_depth (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int max_nb_loops = 0;

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      int nb_loops = pbb_dim_iter_domain (pbb);
      if (max_nb_loops < nb_loops)
        max_nb_loops = nb_loops;
    }

  return max_nb_loops;
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
      fprintf (file, "#eq");

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

  fprintf (file, "isl\n");
  print_isl_map (file, pbb->transformed ? pbb->transformed : pbb->schedule);

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

  if (pbb->schedule || pbb->transformed)
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

  if (verbosity > 0)
    fprintf (file, "# Scattering names are not provided\n");

  fprintf (file, "0\n");

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

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    print_scattering_function (file, pbb, verbosity);
}

/* Prints to FILE the iteration domains of every PBB of SCOP, at some
   VERBOSITY level.  */

void
print_iteration_domains (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    print_iteration_domain (file, pbb, verbosity);
}

/* Prints to STDERR the scattering function of PBB, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_scattering_function (poly_bb_p pbb, int verbosity)
{
  print_scattering_function (stderr, pbb, verbosity);
}

/* Prints to STDERR the iteration domain of PBB, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_iteration_domain (poly_bb_p pbb, int verbosity)
{
  print_iteration_domain (stderr, pbb, verbosity);
}

/* Prints to STDERR the scattering functions of every PBB of SCOP, at
   some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_scattering_functions (scop_p scop, int verbosity)
{
  print_scattering_functions (stderr, scop, verbosity);
}

/* Prints to STDERR the iteration domains of every PBB of SCOP, at
   some VERBOSITY level.  */

DEBUG_FUNCTION void
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
	transform_done |= scop_do_strip_mine (scop, 0);

      if (flag_loop_interchange)
	transform_done |= scop_do_interchange (scop);
    }

  /* This pass needs to be run at the final stage, as it does not
     update the lst.  */
  if (flag_loop_optimize_isl)
    transform_done |= optimize_isl (scop);

  return transform_done;
}

/* Create a new polyhedral data reference and add it to PBB.  It is
   defined by its ACCESSES, its TYPE, and the number of subscripts
   NB_SUBSCRIPTS.  */

void
new_poly_dr (poly_bb_p pbb, int dr_base_object_set,
	     enum poly_dr_type type, void *cdr, graphite_dim_t nb_subscripts,
	     isl_map *acc, isl_set *extent)
{
  static int id = 0;
  poly_dr_p pdr = XNEW (struct poly_dr);

  PDR_ID (pdr) = id++;
  PDR_BASE_OBJECT_SET (pdr) = dr_base_object_set;
  PDR_NB_REFS (pdr) = 1;
  PDR_PBB (pdr) = pbb;
  pdr->accesses = acc;
  pdr->extent = extent;
  PDR_TYPE (pdr) = type;
  PDR_CDR (pdr) = cdr;
  PDR_NB_SUBSCRIPTS (pdr) = nb_subscripts;
  PBB_DRS (pbb).safe_push (pdr);
}

/* Free polyhedral data reference PDR.  */

void
free_poly_dr (poly_dr_p pdr)
{
  isl_map_free (pdr->accesses);
  isl_set_free (pdr->extent);
  XDELETE (pdr);
}

/* Create a new polyhedral black box.  */

poly_bb_p
new_poly_bb (scop_p scop, void *black_box)
{
  poly_bb_p pbb = XNEW (struct poly_bb);

  pbb->domain = NULL;
  pbb->schedule = NULL;
  pbb->transformed = NULL;
  pbb->saved = NULL;
  PBB_SCOP (pbb) = scop;
  pbb_set_black_box (pbb, black_box);
  PBB_TRANSFORMED (pbb) = NULL;
  PBB_SAVED (pbb) = NULL;
  PBB_ORIGINAL (pbb) = NULL;
  PBB_DRS (pbb).create (3);
  PBB_IS_REDUCTION (pbb) = false;
  GBB_PBB ((gimple_bb_p) black_box) = pbb;

  return pbb;
}

/* Free polyhedral black box.  */

void
free_poly_bb (poly_bb_p pbb)
{
  int i;
  poly_dr_p pdr;

  isl_set_free (pbb->domain);
  isl_map_free (pbb->schedule);
  isl_map_free (pbb->transformed);
  isl_map_free (pbb->saved);

  if (PBB_DRS (pbb).exists ())
    FOR_EACH_VEC_ELT (PBB_DRS (pbb), i, pdr)
      free_poly_dr (pdr);

  PBB_DRS (pbb).release ();
  XDELETE (pbb);
}

static void
print_pdr_access_layout (FILE *file, poly_bb_p pbb, poly_dr_p pdr)
{
  graphite_dim_t i;

  fprintf (file, "#  eq");

  fprintf (file, "   alias");

  for (i = 0; i < PDR_NB_SUBSCRIPTS (pdr); i++)
    fprintf (file, "   sub%d", (int) i);

  for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
    fprintf (file, "     i%d", (int) i);

  for (i = 0; i < pbb_nb_params (pbb); i++)
    fprintf (file, "     p%d", (int) i);

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
      print_pdr_access_layout (file, PDR_PBB (pdr), pdr);
    }

  /* XXX isl dump accesses/subscripts */

  if (verbosity > 0)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Prints to STDERR the polyhedral data reference PDR, at some
   VERBOSITY level.  */

DEBUG_FUNCTION void
debug_pdr (poly_dr_p pdr, int verbosity)
{
  print_pdr (stderr, pdr, verbosity);
}

/* Creates a new SCOP containing REGION.  */

scop_p
new_scop (void *region)
{
  scop_p scop = XNEW (struct scop);

  scop->context = NULL;
  scop->must_raw = NULL;
  scop->may_raw = NULL;
  scop->must_raw_no_source = NULL;
  scop->may_raw_no_source = NULL;
  scop->must_war = NULL;
  scop->may_war = NULL;
  scop->must_war_no_source = NULL;
  scop->may_war_no_source = NULL;
  scop->must_waw = NULL;
  scop->may_waw = NULL;
  scop->must_waw_no_source = NULL;
  scop->may_waw_no_source = NULL;
  scop_set_region (scop, region);
  SCOP_BBS (scop).create (3);
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

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    free_poly_bb (pbb);

  SCOP_BBS (scop).release ();

  isl_set_free (scop->context);
  isl_union_map_free (scop->must_raw);
  isl_union_map_free (scop->may_raw);
  isl_union_map_free (scop->must_raw_no_source);
  isl_union_map_free (scop->may_raw_no_source);
  isl_union_map_free (scop->must_war);
  isl_union_map_free (scop->may_war);
  isl_union_map_free (scop->must_war_no_source);
  isl_union_map_free (scop->may_war_no_source);
  isl_union_map_free (scop->must_waw);
  isl_union_map_free (scop->may_waw);
  isl_union_map_free (scop->must_waw_no_source);
  isl_union_map_free (scop->may_waw_no_source);
  free_lst (SCOP_ORIGINAL_SCHEDULE (scop));
  free_lst (SCOP_TRANSFORMED_SCHEDULE (scop));
  free_lst (SCOP_SAVED_SCHEDULE (scop));
  XDELETE (scop);
}

/* Print to FILE the domain of PBB in OpenScop format, at some VERBOSITY
   level.  */

static void
openscop_print_pbb_domain (FILE *file, poly_bb_p pbb, int verbosity)
{
  graphite_dim_t i;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (!pbb->domain)
    return;

  if (verbosity > 0)
    {
      fprintf (file, "\n# Iteration domain of bb_%d (\n", GBB_BB (gbb)->index);
      fprintf (file, "#eq");

      for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
	fprintf (file, "     i%d", (int) i);

      for (i = 0; i < pbb_nb_params (pbb); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  fprintf (file, "XXX isl\n");

  if (verbosity > 0)
    fprintf (file, "#)\n");
}

/* Print to FILE the domain of PBB, at some VERBOSITY level.  */

void
print_pbb_domain (FILE *file, poly_bb_p pbb, int verbosity ATTRIBUTE_UNUSED)
{
  print_isl_set (file, pbb->domain);
}

/* Dump the cases of a graphite basic block GBB on FILE.  */

static void
dump_gbb_cases (FILE *file, gimple_bb_p gbb)
{
  int i;
  gimple stmt;
  vec<gimple> cases;

  if (!gbb)
    return;

  cases = GBB_CONDITION_CASES (gbb);
  if (cases.is_empty ())
    return;

  fprintf (file, "# cases bb_%d (\n", GBB_BB (gbb)->index);

  FOR_EACH_VEC_ELT (cases, i, stmt)
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
  vec<gimple> conditions;

  if (!gbb)
    return;

  conditions = GBB_CONDITIONS (gbb);
  if (conditions.is_empty ())
    return;

  fprintf (file, "# conditions bb_%d (\n", GBB_BB (gbb)->index);

  FOR_EACH_VEC_ELT (conditions, i, stmt)
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

  if (PBB_DRS (pbb).length () == 0)
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

  FOR_EACH_VEC_ELT (PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) == PDR_READ)
      nb_reads++;
    else
      nb_writes++;

  if (verbosity > 1)
    fprintf (file, "# Read data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Read access informations\n");
  fprintf (file, "%d\n", nb_reads);

  FOR_EACH_VEC_ELT (PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) == PDR_READ)
      print_pdr (file, pdr, verbosity);

  if (verbosity > 1)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "# Write data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Write access informations\n");
  fprintf (file, "%d\n", nb_writes);

  FOR_EACH_VEC_ELT (PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) != PDR_READ)
      print_pdr (file, pdr, verbosity);

  if (verbosity > 1)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to STDERR all the data references of PBB.  */

DEBUG_FUNCTION void
debug_pdrs (poly_bb_p pbb, int verbosity)
{
  print_pdrs (stderr, pbb, verbosity);
}

/* Print to FILE the body of PBB, at some VERBOSITY level.
   If statement_body_provided is false statement body is not printed.  */

static void
print_pbb_body (FILE *file, poly_bb_p pbb, int verbosity,
		bool statement_body_provided)
{
  if (verbosity > 1)
    fprintf (file, "# Body (\n");

  if (!statement_body_provided)
    {
      if (verbosity > 0)
	fprintf (file, "# Statement body is not provided\n");

      fprintf (file, "0\n");

      if (verbosity > 1)
	fprintf (file, "#)\n");
      return;
    }

  if (verbosity > 0)
    fprintf (file, "# Statement body is provided\n");
  fprintf (file, "1\n");

  if (verbosity > 0)
    fprintf (file, "# Original iterator names\n# Iterator names are not provided yet.\n");

  if (verbosity > 0)
    fprintf (file, "# Statement body\n");

  fprintf (file, "{\n");
  dump_bb (file, pbb_bb (pbb), 0, 0);
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

  openscop_print_pbb_domain (file, pbb, verbosity);
  print_scattering_function (file, pbb, verbosity);
  print_pdrs (file, pbb, verbosity);
  print_pbb_body (file, pbb, verbosity, false);

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

  if (SESE_PARAMS (SCOP_REGION (scop)).length ())
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

  FOR_EACH_VEC_ELT (SESE_PARAMS (SCOP_REGION (scop)), i, t)
    {
      print_generic_expr (file, t, 0);
      fprintf (file, " ");
    }

  fprintf (file, "\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to FILE the context of SCoP in OpenScop format, at some VERBOSITY
   level.  */

static void
openscop_print_scop_context (FILE *file, scop_p scop, int verbosity)
{
  graphite_dim_t i;

  if (verbosity > 0)
    {
      fprintf (file, "# Context (\n");
      fprintf (file, "#eq");

      for (i = 0; i < scop_nb_params (scop); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (scop->context)
    /* XXX isl print context */
    fprintf (file, "XXX isl\n");
  else
    fprintf (file, "0 %d 0 0 0 %d\n", (int) scop_nb_params (scop) + 2,
	     (int) scop_nb_params (scop));

  if (verbosity > 0)
    fprintf (file, "# )\n");
}

/* Print to FILE the context of SCoP, at some VERBOSITY level.  */

void
print_scop_context (FILE *file, scop_p scop, int verbosity)
{
  graphite_dim_t i;

  if (verbosity > 0)
    {
      fprintf (file, "# Context (\n");
      fprintf (file, "#eq");

      for (i = 0; i < scop_nb_params (scop); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (scop->context)
    print_isl_set (file, scop->context);
  else
    fprintf (file, "no isl context %d\n", (int) scop_nb_params (scop) + 2);

  if (verbosity > 0)
    fprintf (file, "# )\n");
}

/* Print to FILE the SCOP, at some VERBOSITY level.  */

void
print_scop (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  fprintf (file, "SCoP 1\n#(\n");
  fprintf (file, "# Language\nGimple\n");
  openscop_print_scop_context (file, scop, verbosity);
  print_scop_params (file, scop, verbosity);

  if (verbosity > 0)
    fprintf (file, "# Number of statements\n");

  fprintf (file, "%d\n", SCOP_BBS (scop).length ());

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
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

  fprintf (file, "%d\n", SCOP_BBS (scop).length ());

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
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

  fprintf (file, "%d\n", SCOP_BBS (scop).length ());

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      if (!(pbb->transformed || pbb->schedule))
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

DEBUG_FUNCTION void
debug_pbb_domain (poly_bb_p pbb, int verbosity)
{
  print_pbb_domain (stderr, pbb, verbosity);
}

/* Print to FILE the domain and scattering function of PBB, at some
   VERBOSITY level.  */

DEBUG_FUNCTION void
debug_pbb (poly_bb_p pbb, int verbosity)
{
  print_pbb (stderr, pbb, verbosity);
}

/* Print to STDERR the context of SCOP, at some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_scop_context (scop_p scop, int verbosity)
{
  print_scop_context (stderr, scop, verbosity);
}

/* Print to STDERR the SCOP, at some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_scop (scop_p scop, int verbosity)
{
  print_scop (stderr, scop, verbosity);
}

/* Print to STDERR the SCOP under CLooG format, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_cloog (scop_p scop, int verbosity)
{
  print_cloog (stderr, scop, verbosity);
}

/* Print to STDERR the parameters of SCOP, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_scop_params (scop_p scop, int verbosity)
{
  print_scop_params (stderr, scop, verbosity);
}

extern isl_ctx *the_isl_ctx;
void
print_isl_set (FILE *f, isl_set *set)
{
  isl_printer *p = isl_printer_to_file (the_isl_ctx, f);
  p = isl_printer_print_set (p, set);
  isl_printer_free (p);
}

DEBUG_FUNCTION void
debug_isl_set (isl_set *set)
{
  print_isl_set (stderr, set);
}

void
print_isl_map (FILE *f, isl_map *map)
{
  isl_printer *p = isl_printer_to_file (the_isl_ctx, f);
  p = isl_printer_print_map (p, map);
  isl_printer_free (p);
}

DEBUG_FUNCTION void
debug_isl_map (isl_map *map)
{
  print_isl_map (stderr, map);
}

void
print_isl_aff (FILE *f, isl_aff *aff)
{
  isl_printer *p = isl_printer_to_file (the_isl_ctx, f);
  p = isl_printer_print_aff (p, aff);
  isl_printer_free (p);
}

DEBUG_FUNCTION void
debug_isl_aff (isl_aff *aff)
{
  print_isl_aff (stderr, aff);
}

void
print_isl_constraint (FILE *f, isl_constraint *c)
{
  isl_printer *p = isl_printer_to_file (the_isl_ctx, f);
  p = isl_printer_print_constraint (p, c);
  isl_printer_free (p);
}

DEBUG_FUNCTION void
debug_isl_constraint (isl_constraint *c)
{
  print_isl_constraint (stderr, c);
}

/* Returns the number of iterations RES of the loop around PBB at
   time(scattering) dimension TIME_DEPTH.  */

void
pbb_number_of_iterations_at_time (poly_bb_p pbb,
				  graphite_dim_t time_depth,
				  mpz_t res)
{
  isl_set *transdomain;
  isl_space *dc;
  isl_aff *aff;
  isl_int isllb, islub;

  isl_int_init (isllb);
  isl_int_init (islub);

  /* Map the iteration domain through the current scatter, and work
     on the resulting set.  */
  transdomain = isl_set_apply (isl_set_copy (pbb->domain),
			       isl_map_copy (pbb->transformed));

  /* Select the time_depth' dimension via an affine expression.  */
  dc = isl_set_get_space (transdomain);
  aff = isl_aff_zero_on_domain (isl_local_space_from_space (dc));
  aff = isl_aff_set_coefficient_si (aff, isl_dim_in, time_depth, 1);

  /* And find the min/max for that function.  */
  /* XXX isl check results?  */
  isl_set_min (transdomain, aff, &isllb);
  isl_set_max (transdomain, aff, &islub);

  isl_int_sub (islub, islub, isllb);
  isl_int_add_ui (islub, islub, 1);
  isl_int_get_gmp (islub, res);

  isl_int_clear (isllb);
  isl_int_clear (islub);
  isl_aff_free (aff);
  isl_set_free (transdomain);
}

/* Translates LOOP to LST.  */

static lst_p
loop_to_lst (loop_p loop, vec<poly_bb_p> bbs, int *i)
{
  poly_bb_p pbb;
  vec<lst_p> seq;
  seq.create (5);

  for (; bbs.iterate (*i, &pbb); (*i)++)
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

      seq.safe_push (stmt);
    }

  return new_lst_loop (seq);
}

/* Reads the original scattering of the SCOP and returns an LST
   representing it.  */

void
scop_to_lst (scop_p scop)
{
  lst_p res;
  int i, n = SCOP_BBS (scop).length ();
  vec<lst_p> seq;
  seq.create (5);
  sese region = SCOP_REGION (scop);

  for (i = 0; i < n; i++)
    {
      poly_bb_p pbb = SCOP_BBS (scop)[i];
      loop_p loop = outermost_loop_in_sese (region, GBB_BB (PBB_BLACK_BOX (pbb)));

      if (loop_in_sese_p (loop, region))
	res = loop_to_lst (loop, SCOP_BBS (scop), &i);
      else
	res = new_lst_stmt (pbb);

      seq.safe_push (res);
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

      FOR_EACH_VEC_ELT (LST_SEQ (lst), i, l)
	print_lst (file, l, indent + 2);

      fprintf (file, ")");
    }
  else
    fprintf (file, "%d stmt_%d", lst_dewey_number (lst), pbb_index (LST_PBB (lst)));
}

/* Print LST to STDERR.  */

DEBUG_FUNCTION void
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

      FOR_EACH_VEC_ELT (LST_SEQ (lst), i, l)
	dot_lst_1 (file, l);
    }

  else
    fprintf (file, "L_%d_%d -> S_%d\n",
	     lst_depth (LST_LOOP_FATHER (lst)),
	     lst_dewey_number (LST_LOOP_FATHER (lst)),
	     pbb_index (LST_PBB (lst)));

}

/* Display the LST using dotty.  */

DEBUG_FUNCTION void
dot_lst (lst_p lst)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *stream = fopen ("/tmp/lst.dot", "w");
  gcc_assert (stream);

  fputs ("digraph all {\n", stream);
  dot_lst_1 (stream, lst);
  fputs ("}\n\n", stream);
  fclose (stream);

  system ("dotty /tmp/lst.dot &");
#else
  fputs ("digraph all {\n", stderr);
  dot_lst_1 (stderr, lst);
  fputs ("}\n\n", stderr);

#endif
}

/* Computes a checksum for the code generated by CLooG for SCOP.  */

DEBUG_FUNCTION void
cloog_checksum (scop_p scop ATTRIBUTE_UNUSED)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *stream = fopen ("/tmp/scop.cloog", "w");
  gcc_assert (stream);
  print_cloog (stream, scop, 0);
  fclose (stream);

  fputs ("\n", stdout);
  system ("cloog -compilable 1 /tmp/scop.cloog > /tmp/scop.c ; gcc -O0 -g /tmp/scop.c -lm -o /tmp/scop; /tmp/scop | md5sum ");
#endif
}

/* Reverse the loop around PBB at level DEPTH.  */

isl_map *
reverse_loop_at_level (poly_bb_p pbb, int depth)
{
  unsigned i, depth_dim = psct_dynamic_dim (pbb, depth);
  isl_space *d = isl_map_get_space (pbb->transformed);
  isl_space *d1 = isl_space_range (d);
  unsigned n = isl_space_dim (d1, isl_dim_out);
  isl_space *d2 = isl_space_add_dims (d1, isl_dim_in, n);
  isl_map *x = isl_map_universe (isl_space_copy (d2));
  isl_constraint *c = isl_equality_alloc (isl_local_space_from_space (d2));

  for (i = 0; i < n; i++)
    if (i != depth_dim)
      x = isl_map_equate (x, isl_dim_in, i, isl_dim_out, i);

  c = isl_constraint_set_coefficient_si (c, isl_dim_in, depth_dim, 1);
  c = isl_constraint_set_coefficient_si (c, isl_dim_out, depth_dim, 1);
  x = isl_map_add_constraint (x, c);
  return x;
}

/* Reverse the loop at level DEPTH for all the PBBS.  */

isl_union_map *
reverse_loop_for_pbbs (scop_p scop, vec<poly_bb_p> pbbs, int depth)
{
  poly_bb_p pbb;
  int i;
  isl_space *space = isl_space_from_domain (isl_set_get_space (scop->context));
  isl_union_map *res = isl_union_map_empty (space);

  for (i = 0; pbbs.iterate (i, &pbb); i++)
    res = isl_union_map_add_map (res, reverse_loop_at_level (pbb, depth));

  return res;
}


#endif

