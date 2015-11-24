/* Graphite polyhedral representation.
   Copyright (C) 2009-2015 Free Software Foundation, Inc.
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

#define USES_ISL

#include "config.h"

#ifdef HAVE_isl

#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-data-ref.h"

#include <isl/constraint.h>
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/constraint.h>
#include <isl/ilp.h>
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

#include "graphite.h"

#define OPENSCOP_MAX_STRING 256


/* Print to STDERR the GMP value VAL.  */

DEBUG_FUNCTION void
debug_gmp_value (mpz_t val)
{
  gmp_fprintf (stderr, "%Zd", val);
}

/* Prints to FILE the iteration domain of PBB.  */

void
print_iteration_domain (FILE *file, poly_bb_p pbb)
{
  print_pbb_domain (file, pbb);
}

/* Prints to FILE the iteration domains of every PBB of SCOP.  */

void
print_iteration_domains (FILE *file, scop_p scop)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    print_iteration_domain (file, pbb);
}

/* Prints to STDERR the iteration domain of PBB.  */

DEBUG_FUNCTION void
debug_iteration_domain (poly_bb_p pbb)
{
  print_iteration_domain (stderr, pbb);
}

/* Prints to STDERR the iteration domains of every PBB of SCOP.  */

DEBUG_FUNCTION void
debug_iteration_domains (scop_p scop)
{
  print_iteration_domains (stderr, scop);
}

/* Apply graphite transformations to all the basic blocks of SCOP.  */

bool
apply_poly_transforms (scop_p scop)
{
  bool transform_done = false;

  /* Generate code even if we did not apply any real transformation.
     This also allows to check the performance for the identity
     transformation: GIMPLE -> GRAPHITE -> GIMPLE.  */
  if (flag_graphite_identity)
    transform_done = true;

  if (flag_loop_parallelize_all)
    transform_done = true;

  if (flag_loop_optimize_isl)
    transform_done |= optimize_isl (scop);

  return transform_done;
}

/* Create a new polyhedral data reference and add it to PBB.  It is
   defined by its ACCESSES, its TYPE, and the number of subscripts
   NB_SUBSCRIPTS.  */

void
new_poly_dr (poly_bb_p pbb, gimple *stmt, enum poly_dr_type type,
	     isl_map *acc, isl_set *subscript_sizes)
{
  static int id = 0;
  poly_dr_p pdr = XNEW (struct poly_dr);

  pdr->stmt = stmt;
  PDR_ID (pdr) = id++;
  PDR_NB_REFS (pdr) = 1;
  PDR_PBB (pdr) = pbb;
  pdr->accesses = acc;
  pdr->subscript_sizes = subscript_sizes;
  PDR_TYPE (pdr) = type;
  PBB_DRS (pbb).safe_push (pdr);
}

/* Free polyhedral data reference PDR.  */

void
free_poly_dr (poly_dr_p pdr)
{
  isl_map_free (pdr->accesses);
  isl_set_free (pdr->subscript_sizes);
  XDELETE (pdr);
}

/* Create a new polyhedral black box.  */

poly_bb_p
new_poly_bb (scop_p scop, gimple_poly_bb_p black_box)
{
  poly_bb_p pbb = XNEW (struct poly_bb);

  pbb->domain = NULL;
  pbb->schedule = NULL;
  pbb->transformed = NULL;
  pbb->saved = NULL;
  PBB_SCOP (pbb) = scop;
  pbb_set_black_box (pbb, black_box);
  PBB_DRS (pbb).create (3);
  PBB_IS_REDUCTION (pbb) = false;
  GBB_PBB ((gimple_poly_bb_p) black_box) = pbb;

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

/* Prints to FILE the polyhedral data reference PDR.  */

void
print_pdr (FILE *file, poly_dr_p pdr)
{
  fprintf (file, "pdr_%d (", PDR_ID (pdr));

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

  fprintf (file, "in gimple stmt: ");
  print_gimple_stmt (file, pdr->stmt, 0, 0);
  fprintf (file, "data accesses: ");
  print_isl_map (file, pdr->accesses);
  fprintf (file, "subscript sizes: ");
  print_isl_set (file, pdr->subscript_sizes);
  fprintf (file, ")\n");
}

/* Prints to STDERR the polyhedral data reference PDR.  */

DEBUG_FUNCTION void
debug_pdr (poly_dr_p pdr)
{
  print_pdr (stderr, pdr);
}

/* Store the GRAPHITE representation of BB.  */

gimple_poly_bb_p
new_gimple_poly_bb (basic_block bb, vec<data_reference_p> drs,
		    vec<scalar_use> reads, vec<tree> writes)
{
  gimple_poly_bb_p gbb = XNEW (struct gimple_poly_bb);
  GBB_BB (gbb) = bb;
  GBB_DATA_REFS (gbb) = drs;
  gbb->read_scalar_refs = reads;
  gbb->write_scalar_refs = writes;
  GBB_CONDITIONS (gbb).create (0);
  GBB_CONDITION_CASES (gbb).create (0);

  return gbb;
}

/* Frees GBB.  */

void
free_gimple_poly_bb (gimple_poly_bb_p gbb)
{
  free_data_refs (GBB_DATA_REFS (gbb));
  GBB_CONDITIONS (gbb).release ();
  GBB_CONDITION_CASES (gbb).release ();
  gbb->read_scalar_refs.release ();
  gbb->write_scalar_refs.release ();
  XDELETE (gbb);
}

/* Deletes all gimple bbs in SCOP.  */

static void
remove_gbbs_in_scop (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    free_gimple_poly_bb (PBB_BLACK_BOX (pbb));
}

/* Creates a new SCOP containing the region (ENTRY, EXIT).  */

scop_p
new_scop (edge entry, edge exit)
{
  sese_info_p region = new_sese_info (entry, exit);
  scop_p scop = XNEW (struct scop);

  scop->param_context = NULL;
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
  scop->pbbs.create (3);
  scop->drs.create (3);

  return scop;
}

/* Deletes SCOP.  */

void
free_scop (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  remove_gbbs_in_scop (scop);
  free_sese_info (scop->scop_info);

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    free_poly_bb (pbb);

  scop->pbbs.release ();
  scop->drs.release ();

  isl_set_free (scop->param_context);
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
  XDELETE (scop);
}

/* Print to FILE the domain of PBB.  */

void
print_pbb_domain (FILE *file, poly_bb_p pbb)
{
  print_isl_set (file, pbb->domain);
}

/* Dump the cases of a graphite basic block GBB on FILE.  */

static void
dump_gbb_cases (FILE *file, gimple_poly_bb_p gbb)
{
  int i;
  gimple *stmt;
  vec<gimple *> cases;

  if (!gbb)
    return;

  cases = GBB_CONDITION_CASES (gbb);
  if (cases.is_empty ())
    return;

  fprintf (file, "cases bb_%d (\n", GBB_BB (gbb)->index);

  FOR_EACH_VEC_ELT (cases, i, stmt)
    print_gimple_stmt (file, stmt, 0, 0);

  fprintf (file, ")\n");
}

/* Dump conditions of a graphite basic block GBB on FILE.  */

static void
dump_gbb_conditions (FILE *file, gimple_poly_bb_p gbb)
{
  int i;
  gimple *stmt;
  vec<gimple *> conditions;

  if (!gbb)
    return;

  conditions = GBB_CONDITIONS (gbb);
  if (conditions.is_empty ())
    return;

  fprintf (file, "conditions bb_%d (\n", GBB_BB (gbb)->index);

  FOR_EACH_VEC_ELT (conditions, i, stmt)
    print_gimple_stmt (file, stmt, 0, 0);

  fprintf (file, ")\n");
}

/* Print to FILE all the data references of PBB.  */

void
print_pdrs (FILE *file, poly_bb_p pbb)
{
  int i;
  poly_dr_p pdr;
  int nb_reads = 0;
  int nb_writes = 0;

  if (PBB_DRS (pbb).is_empty ())
    return;

  fprintf (file, "Data references (\n");

  FOR_EACH_VEC_ELT (PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) == PDR_READ)
      nb_reads++;
    else
      nb_writes++;

  fprintf (file, "Read data references (\n");

  FOR_EACH_VEC_ELT (PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) == PDR_READ)
      print_pdr (file, pdr);

  fprintf (file, ")\n");
  fprintf (file, "Write data references (\n");
  FOR_EACH_VEC_ELT (PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) != PDR_READ)
      print_pdr (file, pdr);
  fprintf (file, ")\n");
  fprintf (file, ")\n");
}

/* Print to STDERR all the data references of PBB.  */

DEBUG_FUNCTION void
debug_pdrs (poly_bb_p pbb)
{
  print_pdrs (stderr, pbb);
}

/* Print to FILE the body of PBB.  */

static void
print_pbb_body (FILE *file, poly_bb_p pbb)
{
  fprintf (file, "Body (\n");
  dump_bb (file, pbb_bb (pbb), 0, 0);
  fprintf (file, ")\n");
}

/* Print to FILE the domain and scattering function of PBB.  */

void
print_pbb (FILE *file, poly_bb_p pbb)
{
  fprintf (file, "pbb_%d (\n", pbb_index (pbb));
  dump_gbb_conditions (file, PBB_BLACK_BOX (pbb));
  dump_gbb_cases (file, PBB_BLACK_BOX (pbb));

  print_pbb_domain (file, pbb);
  print_pdrs (file, pbb);
  print_pbb_body (file, pbb);

  fprintf (file, ")\n");
}

/* Print to FILE the parameters of SCOP.  */

void
print_scop_params (FILE *file, scop_p scop)
{
  if (scop->scop_info->params.is_empty ())
    return;

  int i;
  tree t;
  fprintf (file, "parameters (");
  FOR_EACH_VEC_ELT (scop->scop_info->params, i, t)
    {
      print_generic_expr (file, t, 0);
      fprintf (file, ", ");
    }
  fprintf (file, ")\n");
}

/* Print to FILE the context of SCoP.  */

void
print_scop_context (FILE *file, scop_p scop)
{
  if (!scop->param_context)
    return;

  fprintf (file, "Context (\n");
  print_isl_set (file, scop->param_context);
  fprintf (file, ")\n");
}

/* Print to FILE the SCOP.  */

void
print_scop (FILE *file, scop_p scop)
{
  int i;
  poly_bb_p pbb;

  fprintf (file, "SCoP (\n");
  print_scop_context (file, scop);
  print_scop_params (file, scop);

  fprintf (file, "Number of statements: ");
  fprintf (file, "%d\n", scop->pbbs.length ());

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    print_pbb (file, pbb);

  fprintf (file, ")\n");
}

/* Print to STDERR the domain of PBB.  */

DEBUG_FUNCTION void
debug_pbb_domain (poly_bb_p pbb)
{
  print_pbb_domain (stderr, pbb);
}

/* Print to FILE the domain and scattering function of PBB.  */

DEBUG_FUNCTION void
debug_pbb (poly_bb_p pbb)
{
  print_pbb (stderr, pbb);
}

/* Print to STDERR the context of SCOP.  */

DEBUG_FUNCTION void
debug_scop_context (scop_p scop)
{
  print_scop_context (stderr, scop);
}

/* Print to STDERR the SCOP.  */

DEBUG_FUNCTION void
debug_scop (scop_p scop)
{
  print_scop (stderr, scop);
}

/* Print to STDERR the parameters of SCOP.  */

DEBUG_FUNCTION void
debug_scop_params (scop_p scop)
{
  print_scop_params (stderr, scop);
}

extern isl_ctx *the_isl_ctx;
void
print_isl_set (FILE *f, isl_set *set)
{
  isl_printer *p = isl_printer_to_file (the_isl_ctx, f);
  p = isl_printer_print_set (p, set);
  p = isl_printer_print_str (p, "\n");
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
  p = isl_printer_print_str (p, "\n");
  isl_printer_free (p);
}

DEBUG_FUNCTION void
debug_isl_map (isl_map *map)
{
  print_isl_map (stderr, map);
}

void
print_isl_union_map (FILE *f, isl_union_map *map)
{
  isl_printer *p = isl_printer_to_file (the_isl_ctx, f);
  p = isl_printer_print_union_map (p, map);
  p = isl_printer_print_str (p, "\n");
  isl_printer_free (p);
}

DEBUG_FUNCTION void
debug_isl_union_map (isl_union_map *map)
{
  print_isl_union_map (stderr, map);
}


void
print_isl_aff (FILE *f, isl_aff *aff)
{
  isl_printer *p = isl_printer_to_file (the_isl_ctx, f);
  p = isl_printer_print_aff (p, aff);
  p = isl_printer_print_str (p, "\n");
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
  p = isl_printer_print_str (p, "\n");
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
  isl_val *isllb, *islub;

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
  isllb = isl_set_min_val (transdomain, aff);
  islub = isl_set_max_val (transdomain, aff);

  islub = isl_val_sub (islub, isllb);
  islub = isl_val_add_ui (islub, 1);
  isl_val_get_num_gmp (islub, res);

  isl_val_free (islub);
  isl_aff_free (aff);
  isl_set_free (transdomain);
}

#endif  /* HAVE_isl */

