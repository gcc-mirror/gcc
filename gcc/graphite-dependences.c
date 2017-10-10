/* Data dependence analysis for Graphite.
   Copyright (C) 2009-2017 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Konrad Trifunovic <konrad.trifunovic@inria.fr>.

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
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "graphite.h"

/* Add the constraints from the set S to the domain of MAP.  */

static isl_map *
constrain_domain (isl_map *map, isl_set *s)
{
  isl_space *d = isl_map_get_space (map);
  isl_id *id = isl_space_get_tuple_id (d, isl_dim_in);

  s = isl_set_set_tuple_id (s, id);
  isl_space_free (d);
  return isl_map_coalesce (isl_map_intersect_domain (map, s));
}

/* Constrain pdr->accesses with pdr->subscript_sizes and pbb->domain.  */

static isl_map *
add_pdr_constraints (poly_dr_p pdr, poly_bb_p pbb)
{
  isl_map *x = isl_map_intersect_range (isl_map_copy (pdr->accesses),
					isl_set_copy (pdr->subscript_sizes));
  x = isl_map_coalesce (x);
  return constrain_domain (x, isl_set_copy (pbb->domain));
}

/* Returns an isl description of all memory operations in SCOP.  The memory
   reads are returned in READS and writes in MUST_WRITES and MAY_WRITES.  */

static void
scop_get_reads_and_writes (scop_p scop, isl_union_map *reads,
			   isl_union_map *must_writes,
			   isl_union_map *may_writes)
{
  int i, j;
  poly_bb_p pbb;
  poly_dr_p pdr;

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    {
      FOR_EACH_VEC_ELT (PBB_DRS (pbb), j, pdr) {
	if (pdr_read_p (pdr))
	  {
	    if (dump_file)
	      {
		fprintf (dump_file, "Adding read to depedence graph: ");
		print_pdr (dump_file, pdr);
	      }
	    isl_union_map *um
	      = isl_union_map_from_map (add_pdr_constraints (pdr, pbb));
	    reads = isl_union_map_union (reads, um);
	    if (dump_file)
	      {
		fprintf (dump_file, "Reads depedence graph: ");
		print_isl_union_map (dump_file, reads);
	      }
	  }
	else if (pdr_write_p (pdr))
	  {
	    if (dump_file)
	      {
		fprintf (dump_file, "Adding must write to depedence graph: ");
		print_pdr (dump_file, pdr);
	      }
	    isl_union_map *um
	      = isl_union_map_from_map (add_pdr_constraints (pdr, pbb));
	    must_writes = isl_union_map_union (must_writes, um);
	    if (dump_file)
	      {
		fprintf (dump_file, "Must writes depedence graph: ");
		print_isl_union_map (dump_file, must_writes);
	      }
	  }
	else if (pdr_may_write_p (pdr))
	  {
	    if (dump_file)
	      {
		fprintf (dump_file, "Adding may write to depedence graph: ");
		print_pdr (dump_file, pdr);
	      }
	    isl_union_map *um
	      = isl_union_map_from_map (add_pdr_constraints (pdr, pbb));
	    may_writes = isl_union_map_union (may_writes, um);
	    if (dump_file)
	      {
		fprintf (dump_file, "May writes depedence graph: ");
		print_isl_union_map (dump_file, may_writes);
	      }
	  }
      }
    }
}

/* Helper function used on each MAP of a isl_union_map.  Computes the
   maximal output dimension.  */

static isl_stat
max_number_of_out_dimensions (__isl_take isl_map *map, void *user)
{
  int global_max = *((int *) user);
  isl_space *space = isl_map_get_space (map);
  int nb_out = isl_space_dim (space, isl_dim_out);

  if (global_max < nb_out)
    *((int *) user) = nb_out;

  isl_map_free (map);
  isl_space_free (space);
  return isl_stat_ok;
}

/* Extends the output dimension of MAP to MAX dimensions.  */

static __isl_give isl_map *
extend_map (__isl_take isl_map *map, int max)
{
  isl_space *space = isl_map_get_space (map);
  int n = isl_space_dim (space, isl_dim_out);

  isl_space_free (space);
  return isl_map_add_dims (map, isl_dim_out, max - n);
}

/* Structure used to pass parameters to extend_schedule_1.  */

struct extend_schedule_str {
  int max;
  isl_union_map *umap;
};

/* Helper function for extend_schedule.  */

static isl_stat
extend_schedule_1 (__isl_take isl_map *map, void *user)
{
  struct extend_schedule_str *str = (struct extend_schedule_str *) user;
  str->umap = isl_union_map_add_map (str->umap, extend_map (map, str->max));
  return isl_stat_ok;
}

/* Return a relation that has uniform output dimensions.  */

static __isl_give isl_union_map *
extend_schedule (__isl_take isl_union_map *x)
{
  int max = 0;
  struct extend_schedule_str str;

  isl_union_map_foreach_map (x, max_number_of_out_dimensions, (void *) &max);
  str.max = max;
  str.umap = isl_union_map_empty (isl_union_map_get_space (x));
  isl_union_map_foreach_map (x, extend_schedule_1, (void *) &str);
  isl_union_map_free (x);
  return isl_union_map_coalesce (str.umap);
}

/* Applies SCHEDULE to the in and out dimensions of the dependences
   DEPS and return the resulting relation.  */

static isl_map *
apply_schedule_on_deps (__isl_keep isl_union_map *schedule,
			__isl_keep isl_union_map *deps)
{
  isl_union_map *trans = extend_schedule (isl_union_map_copy (schedule));
  isl_union_map *ux = isl_union_map_copy (deps);
  ux = isl_union_map_apply_domain (ux, isl_union_map_copy (trans));
  ux = isl_union_map_apply_range (ux, trans);
  ux = isl_union_map_coalesce (ux);

  if (!isl_union_map_is_empty (ux))
    return isl_map_from_union_map (ux);

  isl_union_map_free (ux);
  return NULL;
}

/* Return true when DEPS is non empty and the intersection of LEX with
   the DEPS transformed by SCHEDULE is non empty.  LEX is the relation
   in which all the inputs before DEPTH occur at the same time as the
   output, and the input at DEPTH occurs before output.  */

bool
carries_deps (__isl_keep isl_union_map *schedule,
	      __isl_keep isl_union_map *deps,
	      int depth)
{
  if (isl_union_map_is_empty (deps))
    return false;

  isl_map *x = apply_schedule_on_deps (schedule, deps);
  if (x == NULL)
    return false;

  isl_space *space = isl_map_get_space (x);
  isl_map *lex = isl_map_lex_le (isl_space_range (space));
  isl_constraint *ineq = isl_inequality_alloc
    (isl_local_space_from_space (isl_map_get_space (x)));

  for (int i = 0; i < depth - 1; i++)
    lex = isl_map_equate (lex, isl_dim_in, i, isl_dim_out, i);

  /* in + 1 <= out  */
  ineq = isl_constraint_set_coefficient_si (ineq, isl_dim_out, depth - 1, 1);
  ineq = isl_constraint_set_coefficient_si (ineq, isl_dim_in, depth - 1, -1);
  ineq = isl_constraint_set_constant_si (ineq, -1);
  lex = isl_map_add_constraint (lex, ineq);
  lex = isl_map_coalesce (lex);
  x = isl_map_intersect (x, lex);
  bool res = !isl_map_is_empty (x);

  isl_map_free (x);
  return res;
}

/* Compute the dependence relations for the SCOP:
   RAW are read after write dependences,
   WAR are write after read dependences,
   WAW are write after write dependences.  */

void
scop_get_dependences (scop_p scop)
{
  if (scop->dependence)
    return;

  isl_space *space = isl_set_get_space (scop->param_context);
  isl_union_map *reads = isl_union_map_empty (isl_space_copy (space));
  isl_union_map *must_writes = isl_union_map_empty (isl_space_copy (space));
  isl_union_map *may_writes = isl_union_map_empty (space);
  scop_get_reads_and_writes (scop, reads, must_writes, may_writes);

  if (dump_file)
    {
      fprintf (dump_file, "\n--- Documentation for datarefs dump: ---\n");
      fprintf (dump_file, "Statements on the iteration domain are mapped to"
	       " array references.\n");
      fprintf (dump_file, "  To read the following data references:\n\n");
      fprintf (dump_file, "  S_5[i0] -> [106] : i0 >= 0 and i0 <= 3\n");
      fprintf (dump_file, "  S_8[i0] -> [1, i0] : i0 >= 0 and i0 <= 3\n\n");

      fprintf (dump_file, "  S_5[i0] is the dynamic instance of statement"
	       " bb_5 in a loop that accesses all iterations 0 <= i0 <= 3.\n");
      fprintf (dump_file, "  [1, i0] is a 'memref' with alias set 1"
	       " and first subscript access i0.\n");
      fprintf (dump_file, "  [106] is a 'scalar reference' which is the sum of"
	       " SSA_NAME_VERSION 6"
	       " and --param graphite-max-arrays-per-scop=100\n");
      fprintf (dump_file, "-----------------------\n\n");

      fprintf (dump_file, "data references (\n");
      fprintf (dump_file, "  reads: ");
      print_isl_union_map (dump_file, reads);
      fprintf (dump_file, "  must_writes: ");
      print_isl_union_map (dump_file, must_writes);
      fprintf (dump_file, "  may_writes: ");
      print_isl_union_map (dump_file, may_writes);
      fprintf (dump_file, ")\n");
    }

  gcc_assert (scop->original_schedule);

  isl_union_access_info *ai;
  ai = isl_union_access_info_from_sink (isl_union_map_copy (reads));
  ai = isl_union_access_info_set_must_source (ai, isl_union_map_copy (must_writes));
  ai = isl_union_access_info_set_may_source (ai, may_writes);
  ai = isl_union_access_info_set_schedule
    (ai, isl_schedule_copy (scop->original_schedule));
  isl_union_flow *flow = isl_union_access_info_compute_flow (ai);
  isl_union_map *raw = isl_union_flow_get_must_dependence (flow);
  isl_union_flow_free (flow);

  ai = isl_union_access_info_from_sink (isl_union_map_copy (must_writes));
  ai = isl_union_access_info_set_must_source (ai, must_writes);
  ai = isl_union_access_info_set_may_source (ai, reads);
  ai = isl_union_access_info_set_schedule
    (ai, isl_schedule_copy (scop->original_schedule));
  flow = isl_union_access_info_compute_flow (ai);

  isl_union_map *waw = isl_union_flow_get_must_dependence (flow);
  isl_union_map *war = isl_union_flow_get_may_dependence (flow);
  war = isl_union_map_subtract (war, isl_union_map_copy (waw));
  isl_union_flow_free (flow);

  raw = isl_union_map_coalesce (raw);
  waw = isl_union_map_coalesce (waw);
  war = isl_union_map_coalesce (war);

  isl_union_map *dependences = raw;
  dependences = isl_union_map_union (dependences, war);
  dependences = isl_union_map_union (dependences, waw);
  dependences = isl_union_map_coalesce (dependences);

  if (dump_file)
    {
      fprintf (dump_file, "data dependences (\n");
      print_isl_union_map (dump_file, dependences);
      fprintf (dump_file, ")\n");
    }

  scop->dependence = dependences;
}

#endif /* HAVE_isl */
