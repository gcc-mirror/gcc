/* A scheduling optimizer for Graphite
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
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
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "params.h"
#include "dumpfile.h"
#include "tree-vectorizer.h"
#include "graphite.h"


/* get_schedule_for_node_st - Improve schedule for the schedule node.
   Only Simple loop tiling is considered.  */

static __isl_give isl_schedule_node *
get_schedule_for_node_st (__isl_take isl_schedule_node *node, void *user)
{
  if (user)
    return node;

  if (isl_schedule_node_get_type (node) != isl_schedule_node_band
      || isl_schedule_node_n_children (node) != 1)
    return node;

  isl_space *space = isl_schedule_node_band_get_space (node);
  unsigned dims = isl_space_dim (space, isl_dim_set);
  isl_schedule_node *child = isl_schedule_node_get_child (node, 0);
  isl_schedule_node_type type = isl_schedule_node_get_type (child);
  isl_space_free (space);
  isl_schedule_node_free (child);

  if (type != isl_schedule_node_leaf)
    return node;

  long tile_size = PARAM_VALUE (PARAM_LOOP_BLOCK_TILE_SIZE);
  if (dims <= 1
      || tile_size == 0
      || !isl_schedule_node_band_get_permutable (node))
    {
      if (dump_file && dump_flags)
	fprintf (dump_file, "not tiled\n");
      return node;
    }

  /* Tile loops.  */
  space = isl_schedule_node_band_get_space (node);
  isl_multi_val *sizes = isl_multi_val_zero (space);
  isl_ctx *ctx = isl_schedule_node_get_ctx (node);

  for (unsigned i = 0; i < dims; i++)
    {
      sizes = isl_multi_val_set_val (sizes, i,
				     isl_val_int_from_si (ctx, tile_size));
      if (dump_file && dump_flags)
	fprintf (dump_file, "tiled by %ld\n", tile_size);
    }

  node = isl_schedule_node_band_tile (node, sizes);
  node = isl_schedule_node_child (node, 0);

  return node;
}

static isl_union_set *
scop_get_domains (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  isl_space *space = isl_set_get_space (scop->param_context);
  isl_union_set *res = isl_union_set_empty (space);

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    res = isl_union_set_add_set (res, isl_set_copy (pbb->domain));

  return res;
}

/* Compute the schedule for SCOP based on its parameters, domain and set of
   constraints.  Then apply the schedule to SCOP.  */

static bool
optimize_isl (scop_p scop)
{
  int old_err = isl_options_get_on_error (scop->isl_context);
  int old_max_operations = isl_ctx_get_max_operations (scop->isl_context);
  int max_operations = PARAM_VALUE (PARAM_MAX_ISL_OPERATIONS);
  if (max_operations)
    isl_ctx_set_max_operations (scop->isl_context, max_operations);
  isl_options_set_on_error (scop->isl_context, ISL_ON_ERROR_CONTINUE);

  isl_union_set *domain = scop_get_domains (scop);

  /* Simplify the dependences on the domain.  */
  scop_get_dependences (scop);
  isl_union_map *dependences
    = isl_union_map_gist_domain (isl_union_map_copy (scop->dependence),
				 isl_union_set_copy (domain));
  isl_union_map *validity
    = isl_union_map_gist_range (dependences, isl_union_set_copy (domain));

  /* FIXME: proximity should not be validity.  */
  isl_union_map *proximity = isl_union_map_copy (validity);

  isl_schedule_constraints *sc = isl_schedule_constraints_on_domain (domain);
  sc = isl_schedule_constraints_set_proximity (sc, proximity);
  sc = isl_schedule_constraints_set_validity (sc, isl_union_map_copy (validity));
  sc = isl_schedule_constraints_set_coincidence (sc, validity);

  isl_options_set_schedule_serialize_sccs (scop->isl_context, 0);
  isl_options_set_schedule_maximize_band_depth (scop->isl_context, 1);
  isl_options_set_schedule_max_constant_term (scop->isl_context, 20);
  isl_options_set_schedule_max_coefficient (scop->isl_context, 20);
  isl_options_set_tile_scale_tile_loops (scop->isl_context, 0);
  /* Generate loop upper bounds that consist of the current loop iterator, an
     operator (< or <=) and an expression not involving the iterator.  If this
     option is not set, then the current loop iterator may appear several times
     in the upper bound.  See the isl manual for more details.  */
  isl_options_set_ast_build_atomic_upper_bound (scop->isl_context, 1);

  scop->transformed_schedule = isl_schedule_constraints_compute_schedule (sc);
  scop->transformed_schedule =
    isl_schedule_map_schedule_node_bottom_up (scop->transformed_schedule,
					      get_schedule_for_node_st, NULL);

  isl_options_set_on_error (scop->isl_context, old_err);
  isl_ctx_reset_operations (scop->isl_context);
  isl_ctx_set_max_operations (scop->isl_context, old_max_operations);
  if (!scop->transformed_schedule
      || isl_ctx_last_error (scop->isl_context) != isl_error_none)
    {
      location_t loc = find_loop_location
	(scop->scop_info->region.entry->dest->loop_father);
      if (isl_ctx_last_error (scop->isl_context) == isl_error_quota)
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			 "loop nest not optimized, optimization timed out "
			 "after %d operations [--param max-isl-operations]\n",
			 max_operations);
      else
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			 "loop nest not optimized, ISL signalled an error\n");
      return false;
    }

  gcc_assert (scop->original_schedule);
  isl_union_map *original = isl_schedule_get_map (scop->original_schedule);
  isl_union_map *transformed = isl_schedule_get_map (scop->transformed_schedule);
  bool same_schedule = isl_union_map_is_equal (original, transformed);
  isl_union_map_free (original);
  isl_union_map_free (transformed);

  if (same_schedule)
    {
      location_t loc = find_loop_location
	(scop->scop_info->region.entry->dest->loop_father);
      dump_printf_loc (MSG_NOTE, loc,
		       "loop nest not optimized, optimized schedule is "
		       "identical to original schedule\n");
      if (dump_file)
	print_schedule_ast (dump_file, scop->original_schedule, scop);
      isl_schedule_free (scop->transformed_schedule);
      scop->transformed_schedule = isl_schedule_copy (scop->original_schedule);
      return flag_graphite_identity || flag_loop_parallelize_all;
    }

  return true;
}

/* Apply graphite transformations to all the basic blocks of SCOP.  */

bool
apply_poly_transforms (scop_p scop)
{
  if (flag_loop_nest_optimize)
    return optimize_isl (scop);

  if (!flag_graphite_identity && !flag_loop_parallelize_all)
    return false;

  /* Generate code even if we did not apply any real transformation.
     This also allows to check the performance for the identity
     transformation: GIMPLE -> GRAPHITE -> GIMPLE.  */
  gcc_assert (scop->original_schedule);
  scop->transformed_schedule = isl_schedule_copy (scop->original_schedule);
  return true;
}

#endif /* HAVE_isl */
