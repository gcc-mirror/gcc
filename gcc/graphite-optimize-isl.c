/* A scheduling optimizer for Graphite
   Copyright (C) 2012-2015 Free Software Foundation, Inc.
   Contributed by Tobias Grosser <tobias@grosser.es>.

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
#include <isl/union_set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/schedule.h>
#include <isl/band.h>
#include <isl/aff.h>
#include <isl/options.h>
#include <isl/ctx.h>

#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "tree.h"
#include "gimple.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "graphite-poly.h"
#include "params.h"
#include "dumpfile.h"

static isl_union_set *
scop_get_domains (scop_p scop ATTRIBUTE_UNUSED)
{
  int i;
  poly_bb_p pbb;
  isl_space *space = isl_set_get_space (scop->context);
  isl_union_set *res = isl_union_set_empty (space);

  FOR_EACH_VEC_ELT (scop->bbs, i, pbb)
    res = isl_union_set_add_set (res, isl_set_copy (pbb->domain));

  return res;
}

/* get_tile_map - Create a map that describes a n-dimensonal tiling.

   get_tile_map creates a map from a n-dimensional scattering space into an
   2*n-dimensional scattering space. The map describes a rectangular tiling.

   Example:
     SCHEDULE_DIMENSIONS = 2, PARAMETER_DIMENSIONS = 1, TILE_SIZE = 32

    tile_map := [p0] -> {[s0, s1] -> [t0, t1, s0, s1]:
			 t0 % 32 = 0 and t0 <= s0 < t0 + 32 and
			 t1 % 32 = 0 and t1 <= s1 < t1 + 32}

   Before tiling:

   for (i = 0; i < N; i++)
     for (j = 0; j < M; j++)
	S(i,j)

   After tiling:

   for (t_i = 0; t_i < N; i+=32)
     for (t_j = 0; t_j < M; j+=32)
	for (i = t_i; i < min(t_i + 32, N); i++)  | Unknown that N % 32 = 0
	  for (j = t_j; j < t_j + 32; j++)        |   Known that M % 32 = 0
	    S(i,j)
  */

static isl_basic_map *
get_tile_map (isl_ctx *ctx, int schedule_dimensions, int tile_size)
{
  /* We construct

     tile_map := [p0] -> {[s0, s1] -> [t0, t1, p0, p1, a0, a1]:
			s0 = a0 * 32 and s0 = p0 and t0 <= p0 < t0 + 32 and
			s1 = a1 * 32 and s1 = p1 and t1 <= p1 < t1 + 32}

     and project out the auxilary dimensions a0 and a1.  */
  isl_space *space
    = isl_space_alloc (ctx, 0, schedule_dimensions, schedule_dimensions * 3);
  isl_basic_map *tile_map = isl_basic_map_universe (isl_space_copy (space));

  isl_local_space *local_space = isl_local_space_from_space (space);

  for (int x = 0; x < schedule_dimensions; x++)
    {
      int sX = x;
      int tX = x;
      int pX = schedule_dimensions + x;
      int aX = 2 * schedule_dimensions + x;

      isl_constraint *c;

      /* sX = aX * tile_size; */
      c = isl_equality_alloc (isl_local_space_copy (local_space));
      isl_constraint_set_coefficient_si (c, isl_dim_out, sX, 1);
      isl_constraint_set_coefficient_si (c, isl_dim_out, aX, -tile_size);
      tile_map = isl_basic_map_add_constraint (tile_map, c);

      /* pX = sX; */
      c = isl_equality_alloc (isl_local_space_copy (local_space));
      isl_constraint_set_coefficient_si (c, isl_dim_out, pX, 1);
      isl_constraint_set_coefficient_si (c, isl_dim_in, sX, -1);
      tile_map = isl_basic_map_add_constraint (tile_map, c);

      /* tX <= pX */
      c = isl_inequality_alloc (isl_local_space_copy (local_space));
      isl_constraint_set_coefficient_si (c, isl_dim_out, pX, 1);
      isl_constraint_set_coefficient_si (c, isl_dim_out, tX, -1);
      tile_map = isl_basic_map_add_constraint (tile_map, c);

      /* pX <= tX + (tile_size - 1) */
      c = isl_inequality_alloc (isl_local_space_copy (local_space));
      isl_constraint_set_coefficient_si (c, isl_dim_out, tX, 1);
      isl_constraint_set_coefficient_si (c, isl_dim_out, pX, -1);
      isl_constraint_set_constant_si (c, tile_size - 1);
      tile_map = isl_basic_map_add_constraint (tile_map, c);
    }

  /* Project out auxiliary dimensions.

     The auxiliary dimensions are transformed into existentially quantified
     ones.
     This reduces the number of visible scattering dimensions and allows isl
     to produces better code.  */
  tile_map =
      isl_basic_map_project_out (tile_map, isl_dim_out,
				 2 * schedule_dimensions, schedule_dimensions);
  isl_local_space_free (local_space);
  return tile_map;
}

/* get_schedule_for_band - Get the schedule for this BAND.

   Polly applies transformations like tiling on top of the isl calculated
   value.
   This can influence the number of scheduling dimension. The number of
   schedule dimensions is returned in DIMENSIONS.  */

static isl_union_map *
get_schedule_for_band (isl_band *band, int *dimensions)
{
  isl_union_map *partial_schedule;
  isl_ctx *ctx;
  isl_space *space;
  isl_basic_map *tile_map;
  isl_union_map *tile_umap;

  partial_schedule = isl_band_get_partial_schedule (band);
  *dimensions = isl_band_n_member (band);

  /* It does not make any sense to tile a band with just one dimension.  */
  if (*dimensions == 1)
    {
      if (dump_file && dump_flags)
	fprintf (dump_file, "not tiled\n");
      return partial_schedule;
    }

  if (dump_file && dump_flags)
    fprintf (dump_file, "tiled by %d\n",
	     PARAM_VALUE (PARAM_LOOP_BLOCK_TILE_SIZE));

  ctx = isl_union_map_get_ctx (partial_schedule);
  space = isl_union_map_get_space (partial_schedule);

  tile_map = get_tile_map (ctx, *dimensions,
			   PARAM_VALUE (PARAM_LOOP_BLOCK_TILE_SIZE));
  tile_umap = isl_union_map_from_map (isl_map_from_basic_map (tile_map));
  tile_umap = isl_union_map_align_params (tile_umap, space);
  *dimensions = 2 * *dimensions;

  return isl_union_map_apply_range (partial_schedule, tile_umap);
}


/* get_schedule_for_band_list - Get the scheduling map for a list of bands.

   We walk recursively the forest of bands to combine the schedules of the
   individual bands to the overall schedule.  In case tiling is requested,
   the individual bands are tiled.  */

static isl_union_map *
get_schedule_for_band_list (isl_band_list *band_list)
{
  int num_bands, i;
  isl_union_map *schedule;
  isl_ctx *ctx;

  ctx = isl_band_list_get_ctx (band_list);
  num_bands = isl_band_list_n_band (band_list);
  schedule = isl_union_map_empty (isl_space_params_alloc (ctx, 0));

  for (i = 0; i < num_bands; i++)
    {
      isl_band *band;
      isl_union_map *partial_schedule;
      int schedule_dimensions;
      isl_space *space;

      band = isl_band_list_get_band (band_list, i);
      partial_schedule = get_schedule_for_band (band, &schedule_dimensions);
      space = isl_union_map_get_space (partial_schedule);

      if (isl_band_has_children (band))
	{
	  isl_band_list *children = isl_band_get_children (band);
	  isl_union_map *suffixSchedule
	    = get_schedule_for_band_list (children);
	  partial_schedule
	    = isl_union_map_flat_range_product (partial_schedule,
						suffixSchedule);
	  isl_band_list_free (children);
	}

      schedule = isl_union_map_union (schedule, partial_schedule);

      isl_band_free (band);
      isl_space_free (space);
    }

  return schedule;
}

static isl_union_map *
get_schedule_map (isl_schedule *schedule)
{
  isl_band_list *bandList = isl_schedule_get_band_forest (schedule);
  isl_union_map *schedule_map = get_schedule_for_band_list (bandList);
  isl_band_list_free (bandList);
  return schedule_map;
}

static isl_stat
get_single_map (__isl_take isl_map *map, void *user)
{
  isl_map **single_map = (isl_map **)user;
  *single_map = map;
  return isl_stat_ok;
}

static void
apply_schedule_map_to_scop (scop_p scop, isl_union_map *schedule_map)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (scop->bbs, i, pbb)
    {
      isl_set *domain = isl_set_copy (pbb->domain);
      isl_map *stmt_schedule;

      isl_union_map *stmt_band
	= isl_union_map_intersect_domain (isl_union_map_copy (schedule_map),
					  isl_union_set_from_set (domain));
      isl_union_map_foreach_map (stmt_band, get_single_map, &stmt_schedule);
      isl_map_free (pbb->transformed);
      pbb->transformed = stmt_schedule;
      isl_union_map_free (stmt_band);
    }
}

static const int CONSTANT_BOUND = 20;

/* Compute the schedule for SCOP based on its parameters, domain and set of
   constraints.  Then apply the schedule to SCOP.  */

bool
optimize_isl (scop_p scop)
{
#ifdef HAVE_ISL_CTX_MAX_OPERATIONS
  int old_max_operations = isl_ctx_get_max_operations (scop->ctx);
  int max_operations = PARAM_VALUE (PARAM_MAX_ISL_OPERATIONS);
  if (max_operations)
    isl_ctx_set_max_operations (scop->ctx, max_operations);
#endif
  isl_options_set_on_error (scop->ctx, ISL_ON_ERROR_CONTINUE);

  isl_union_set *domain = scop_get_domains (scop);
  isl_union_map *dependences = scop_get_dependences (scop);
  dependences
    = isl_union_map_gist_domain (dependences, isl_union_set_copy (domain));
  dependences
    = isl_union_map_gist_range (dependences, isl_union_set_copy (domain));
  isl_union_map *validity = dependences;
  isl_union_map *proximity = isl_union_map_copy (validity);

#ifdef HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE
  isl_schedule_constraints *schedule_constraints;
  schedule_constraints = isl_schedule_constraints_on_domain (domain);
  schedule_constraints
    = isl_schedule_constraints_set_proximity (schedule_constraints,
					      proximity);
  schedule_constraints
    = isl_schedule_constraints_set_validity (schedule_constraints,
					     isl_union_map_copy (validity));
  schedule_constraints
    = isl_schedule_constraints_set_coincidence (schedule_constraints,
						validity);
#endif

  isl_options_set_schedule_max_constant_term (scop->ctx, CONSTANT_BOUND);
  isl_options_set_schedule_maximize_band_depth (scop->ctx, 1);
#ifdef HAVE_ISL_OPTIONS_SET_SCHEDULE_SERIALIZE_SCCS
  /* ISL-0.15 or later.  */
  isl_options_set_schedule_serialize_sccs (scop->ctx, 1);
#else
  isl_options_set_schedule_fuse (scop->ctx, ISL_SCHEDULE_FUSE_MIN);
#endif

#ifdef HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE
  isl_schedule *schedule
    = isl_schedule_constraints_compute_schedule (schedule_constraints);
#else
  isl_schedule *schedule
    = isl_union_set_compute_schedule (domain, validity, proximity);
#endif

  isl_options_set_on_error (scop->ctx, ISL_ON_ERROR_ABORT);

#ifdef HAVE_ISL_CTX_MAX_OPERATIONS
  isl_ctx_reset_operations (scop->ctx);
  isl_ctx_set_max_operations (scop->ctx, old_max_operations);
  if (!schedule || isl_ctx_last_error (scop->ctx) == isl_error_quota)
    {
      if (dump_file && dump_flags)
	fprintf (dump_file, "ISL timed out at %d operations\n",
		 max_operations);
      if (schedule)
	isl_schedule_free (schedule);
      return false;
    }
#else
  if (!schedule)
    return false;
#endif

  isl_union_map *schedule_map = get_schedule_map (schedule);
  apply_schedule_map_to_scop (scop, schedule_map);

  isl_schedule_free (schedule);
  isl_union_map_free (schedule_map);
  return true;
}

#endif /* HAVE_isl */
