/* Data dependence analysis for Graphite.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

#include "config.h"

#ifdef HAVE_cloog
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/flow.h>
#include <isl/constraint.h>
#include <cloog/cloog.h>
#include <cloog/isl/domain.h>
#endif

#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "graphite-poly.h"
#include "graphite-htab.h"

/* Add the constraints from the set S to the domain of MAP.  */

static isl_map *
constrain_domain (isl_map *map, isl_set *s)
{
  isl_space *d = isl_map_get_space (map);
  isl_id *id = isl_space_get_tuple_id (d, isl_dim_in);

  s = isl_set_set_tuple_id (s, id);
  isl_space_free (d);
  return isl_map_intersect_domain (map, s);
}

/* Constrain pdr->accesses with pdr->extent and pbb->domain.  */

static isl_map *
add_pdr_constraints (poly_dr_p pdr, poly_bb_p pbb)
{
  isl_map *x = isl_map_intersect_range (isl_map_copy (pdr->accesses),
					isl_set_copy (pdr->extent));
  x = constrain_domain (x, isl_set_copy (pbb->domain));
  return x;
}

/* Returns all the memory reads in SCOP.  */

static isl_union_map *
scop_get_reads (scop_p scop, vec<poly_bb_p> pbbs)
{
  int i, j;
  poly_bb_p pbb;
  poly_dr_p pdr;
  isl_space *space = isl_set_get_space (scop->context);
  isl_union_map *res = isl_union_map_empty (space);

  FOR_EACH_VEC_ELT (pbbs, i, pbb)
    {
      FOR_EACH_VEC_ELT (PBB_DRS (pbb), j, pdr)
	if (pdr_read_p (pdr))
	  res = isl_union_map_add_map (res, add_pdr_constraints (pdr, pbb));
    }

  return res;
}

/* Returns all the memory must writes in SCOP.  */

static isl_union_map *
scop_get_must_writes (scop_p scop, vec<poly_bb_p> pbbs)
{
  int i, j;
  poly_bb_p pbb;
  poly_dr_p pdr;
  isl_space *space = isl_set_get_space (scop->context);
  isl_union_map *res = isl_union_map_empty (space);

  FOR_EACH_VEC_ELT (pbbs, i, pbb)
    {
      FOR_EACH_VEC_ELT (PBB_DRS (pbb), j, pdr)
	if (pdr_write_p (pdr))
	  res = isl_union_map_add_map (res, add_pdr_constraints (pdr, pbb));
    }

  return res;
}

/* Returns all the memory may writes in SCOP.  */

static isl_union_map *
scop_get_may_writes (scop_p scop, vec<poly_bb_p> pbbs)
{
  int i, j;
  poly_bb_p pbb;
  poly_dr_p pdr;
  isl_space *space = isl_set_get_space (scop->context);
  isl_union_map *res = isl_union_map_empty (space);

  FOR_EACH_VEC_ELT (pbbs, i, pbb)
    {
      FOR_EACH_VEC_ELT (PBB_DRS (pbb), j, pdr)
	if (pdr_may_write_p (pdr))
	  res = isl_union_map_add_map (res, add_pdr_constraints (pdr, pbb));
    }

  return res;
}

/* Returns all the original schedules in SCOP.  */

static isl_union_map *
scop_get_original_schedule (scop_p scop, vec<poly_bb_p> pbbs)
{
  int i;
  poly_bb_p pbb;
  isl_space *space = isl_set_get_space (scop->context);
  isl_union_map *res = isl_union_map_empty (space);

  FOR_EACH_VEC_ELT (pbbs, i, pbb)
    {
      res = isl_union_map_add_map
	(res, constrain_domain (isl_map_copy (pbb->schedule),
				isl_set_copy (pbb->domain)));
    }

  return res;
}

/* Returns all the transformed schedules in SCOP.  */

static isl_union_map *
scop_get_transformed_schedule (scop_p scop, vec<poly_bb_p> pbbs)
{
  int i;
  poly_bb_p pbb;
  isl_space *space = isl_set_get_space (scop->context);
  isl_union_map *res = isl_union_map_empty (space);

  FOR_EACH_VEC_ELT (pbbs, i, pbb)
    {
      res = isl_union_map_add_map
	(res, constrain_domain (isl_map_copy (pbb->transformed),
				isl_set_copy (pbb->domain)));
    }

  return res;
}

/* Helper function used on each MAP of a isl_union_map.  Computes the
   maximal output dimension.  */

static int
max_number_of_out_dimensions (__isl_take isl_map *map, void *user)
{
  int global_max = *((int *) user);
  isl_space *space = isl_map_get_space (map);
  int nb_out = isl_space_dim (space, isl_dim_out);

  if (global_max < nb_out)
    *((int *) user) = nb_out;

  isl_map_free (map);
  isl_space_free (space);
  return 0;
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

static int
extend_schedule_1 (__isl_take isl_map *map, void *user)
{
  struct extend_schedule_str *str = (struct extend_schedule_str *) user;
  str->umap = isl_union_map_add_map (str->umap, extend_map (map, str->max));
  return 0;
}

/* Return a relation that has uniform output dimensions.  */

__isl_give isl_union_map *
extend_schedule (__isl_take isl_union_map *x)
{
  int max = 0;
  int res;
  struct extend_schedule_str str;

  res = isl_union_map_foreach_map (x, max_number_of_out_dimensions, (void *) &max);
  gcc_assert (res == 0);

  str.max = max;
  str.umap = isl_union_map_empty (isl_union_map_get_space (x));
  res = isl_union_map_foreach_map (x, extend_schedule_1, (void *) &str);
  gcc_assert (res == 0);

  isl_union_map_free (x);
  return str.umap;
}

/* Applies SCHEDULE to the in and out dimensions of the dependences
   DEPS and return the resulting relation.  */

static isl_map *
apply_schedule_on_deps (__isl_keep isl_union_map *schedule,
			__isl_keep isl_union_map *deps)
{
  isl_map *x;
  isl_union_map *ux, *trans;

  trans = isl_union_map_copy (schedule);
  trans = extend_schedule (trans);
  ux = isl_union_map_copy (deps);
  ux = isl_union_map_apply_domain (ux, isl_union_map_copy (trans));
  ux = isl_union_map_apply_range (ux, trans);
  x = isl_map_from_union_map (ux);

  return x;
}

/* Return true when SCHEDULE does not violate the data DEPS: that is
   when the intersection of LEX with the DEPS transformed by SCHEDULE
   is empty.  LEX is the relation in which the outputs occur before
   the inputs.  */

static bool
no_violations (__isl_keep isl_union_map *schedule,
	       __isl_keep isl_union_map *deps)
{
  bool res;
  isl_space *space;
  isl_map *lex, *x;

  if (isl_union_map_is_empty (deps))
    return true;

  x = apply_schedule_on_deps (schedule, deps);
  space = isl_map_get_space (x);
  space = isl_space_range (space);
  lex = isl_map_lex_ge (space);
  x = isl_map_intersect (x, lex);
  res = isl_map_is_empty (x);

  isl_map_free (x);
  return res;
}

/* Return true when DEPS is non empty and the intersection of LEX with
   the DEPS transformed by SCHEDULE is non empty.  LEX is the relation
   in which all the inputs before DEPTH occur at the same time as the
   output, and the input at DEPTH occurs before output.  */

static bool
carries_deps (__isl_keep isl_union_map *schedule,
	      __isl_keep isl_union_map *deps,
	      int depth)
{
  bool res;
  int i;
  isl_space *space;
  isl_map *lex, *x;
  isl_constraint *ineq;

  if (isl_union_map_is_empty (deps))
    return false;

  x = apply_schedule_on_deps (schedule, deps);
  space = isl_map_get_space (x);
  space = isl_space_range (space);
  lex = isl_map_lex_le (space);
  space = isl_map_get_space (x);
  ineq = isl_inequality_alloc (isl_local_space_from_space (space));

  for (i = 0; i < depth - 1; i++)
    lex = isl_map_equate (lex, isl_dim_in, i, isl_dim_out, i);

  /* in + 1 <= out  */
  ineq = isl_constraint_set_coefficient_si (ineq, isl_dim_out, depth - 1, 1);
  ineq = isl_constraint_set_coefficient_si (ineq, isl_dim_in, depth - 1, -1);
  ineq = isl_constraint_set_constant_si (ineq, -1);
  lex = isl_map_add_constraint (lex, ineq);
  x = isl_map_intersect (x, lex);
  res = !isl_map_is_empty (x);

  isl_map_free (x);
  return res;
}

/* Subtract from the RAW, WAR, and WAW dependences those relations
   that have been marked as belonging to an associative commutative
   reduction.  */

static void
subtract_commutative_associative_deps (scop_p scop,
				       vec<poly_bb_p> pbbs,
				       isl_union_map *original,
				       isl_union_map **must_raw,
				       isl_union_map **may_raw,
				       isl_union_map **must_raw_no_source,
				       isl_union_map **may_raw_no_source,
				       isl_union_map **must_war,
				       isl_union_map **may_war,
				       isl_union_map **must_war_no_source,
				       isl_union_map **may_war_no_source,
				       isl_union_map **must_waw,
				       isl_union_map **may_waw,
				       isl_union_map **must_waw_no_source,
				       isl_union_map **may_waw_no_source)
{
  int i, j;
  poly_bb_p pbb;
  poly_dr_p pdr;
  isl_space *space = isl_set_get_space (scop->context);

  FOR_EACH_VEC_ELT (pbbs, i, pbb)
    if (PBB_IS_REDUCTION (pbb))
      {
	int res;
	isl_union_map *r = isl_union_map_empty (isl_space_copy (space));
	isl_union_map *must_w = isl_union_map_empty (isl_space_copy (space));
	isl_union_map *may_w = isl_union_map_empty (isl_space_copy (space));
	isl_union_map *all_w;
	isl_union_map *empty;
	isl_union_map *x_must_raw;
	isl_union_map *x_may_raw;
	isl_union_map *x_must_raw_no_source;
	isl_union_map *x_may_raw_no_source;
	isl_union_map *x_must_war;
	isl_union_map *x_may_war;
	isl_union_map *x_must_war_no_source;
	isl_union_map *x_may_war_no_source;
	isl_union_map *x_must_waw;
	isl_union_map *x_may_waw;
	isl_union_map *x_must_waw_no_source;
	isl_union_map *x_may_waw_no_source;

	FOR_EACH_VEC_ELT (PBB_DRS (pbb), j, pdr)
	  if (pdr_read_p (pdr))
	    r = isl_union_map_add_map (r, add_pdr_constraints (pdr, pbb));

	FOR_EACH_VEC_ELT (PBB_DRS (pbb), j, pdr)
	  if (pdr_write_p (pdr))
	    must_w = isl_union_map_add_map (must_w,
					    add_pdr_constraints (pdr, pbb));

	FOR_EACH_VEC_ELT (PBB_DRS (pbb), j, pdr)
	  if (pdr_may_write_p (pdr))
	    may_w = isl_union_map_add_map (may_w,
					   add_pdr_constraints (pdr, pbb));

	all_w = isl_union_map_union
	  (isl_union_map_copy (must_w), isl_union_map_copy (may_w));
	empty = isl_union_map_empty (isl_union_map_get_space (all_w));

	res = isl_union_map_compute_flow (isl_union_map_copy (r),
					  isl_union_map_copy (must_w),
					  isl_union_map_copy (may_w),
					  isl_union_map_copy (original),
					  &x_must_raw, &x_may_raw,
					  &x_must_raw_no_source,
					  &x_may_raw_no_source);
	gcc_assert (res == 0);
	res = isl_union_map_compute_flow (isl_union_map_copy (all_w),
					  r, empty,
					  isl_union_map_copy (original),
					  &x_must_war, &x_may_war,
					  &x_must_war_no_source,
					  &x_may_war_no_source);
	gcc_assert (res == 0);
	res = isl_union_map_compute_flow (all_w, must_w, may_w,
					  isl_union_map_copy (original),
					  &x_must_waw, &x_may_waw,
					  &x_must_waw_no_source,
					  &x_may_waw_no_source);
	gcc_assert (res == 0);

	*must_raw = isl_union_map_subtract (*must_raw, x_must_raw);
	*may_raw = isl_union_map_subtract (*may_raw, x_may_raw);
	*must_raw_no_source = isl_union_map_subtract (*must_raw_no_source,
						      x_must_raw_no_source);
	*may_raw_no_source = isl_union_map_subtract (*may_raw_no_source,
						     x_may_raw_no_source);
	*must_war = isl_union_map_subtract (*must_war, x_must_war);
	*may_war = isl_union_map_subtract (*may_war, x_may_war);
	*must_war_no_source = isl_union_map_subtract (*must_war_no_source,
						      x_must_war_no_source);
	*may_war_no_source = isl_union_map_subtract (*may_war_no_source,
						     x_may_war_no_source);
	*must_waw = isl_union_map_subtract (*must_waw, x_must_waw);
	*may_waw = isl_union_map_subtract (*may_waw, x_may_waw);
	*must_waw_no_source = isl_union_map_subtract (*must_waw_no_source,
						      x_must_waw_no_source);
	*may_waw_no_source = isl_union_map_subtract (*may_waw_no_source,
						     x_may_waw_no_source);
      }

  isl_union_map_free (original);
  isl_space_free (space);
}

/* Compute the original data dependences in SCOP for all the reads and
   writes in PBBS.  */

void
compute_deps (scop_p scop, vec<poly_bb_p> pbbs,
	      isl_union_map **must_raw,
	      isl_union_map **may_raw,
	      isl_union_map **must_raw_no_source,
	      isl_union_map **may_raw_no_source,
	      isl_union_map **must_war,
	      isl_union_map **may_war,
	      isl_union_map **must_war_no_source,
	      isl_union_map **may_war_no_source,
	      isl_union_map **must_waw,
	      isl_union_map **may_waw,
	      isl_union_map **must_waw_no_source,
	      isl_union_map **may_waw_no_source)
{
  isl_union_map *reads = scop_get_reads (scop, pbbs);
  isl_union_map *must_writes = scop_get_must_writes (scop, pbbs);
  isl_union_map *may_writes = scop_get_may_writes (scop, pbbs);
  isl_union_map *all_writes = isl_union_map_union
    (isl_union_map_copy (must_writes), isl_union_map_copy (may_writes));
  isl_space *space = isl_union_map_get_space (all_writes);
  isl_union_map *empty = isl_union_map_empty (space);
  isl_union_map *original = scop_get_original_schedule (scop, pbbs);
  int res;

  res = isl_union_map_compute_flow (isl_union_map_copy (reads),
				    isl_union_map_copy (must_writes),
				    isl_union_map_copy (may_writes),
				    isl_union_map_copy (original),
				    must_raw, may_raw, must_raw_no_source,
				    may_raw_no_source);
  gcc_assert (res == 0);
  res = isl_union_map_compute_flow (isl_union_map_copy (all_writes),
				    reads, empty,
				    isl_union_map_copy (original),
				    must_war, may_war, must_war_no_source,
				    may_war_no_source);
  gcc_assert (res == 0);
  res = isl_union_map_compute_flow (all_writes, must_writes, may_writes,
				    isl_union_map_copy (original),
				    must_waw, may_waw, must_waw_no_source,
				    may_waw_no_source);
  gcc_assert (res == 0);

  subtract_commutative_associative_deps
    (scop, pbbs, original,
     must_raw, may_raw, must_raw_no_source, may_raw_no_source,
     must_war, may_war, must_war_no_source, may_war_no_source,
     must_waw, may_waw, must_waw_no_source, may_waw_no_source);
}

/* Given a TRANSFORM, check whether it respects the original
   dependences in SCOP.  Returns true when TRANSFORM is a safe
   transformation.  */

static bool
transform_is_safe (scop_p scop, isl_union_map *transform)
{
  bool res;

  if (!scop->must_raw)
    compute_deps (scop, SCOP_BBS (scop),
		  &scop->must_raw, &scop->may_raw,
		  &scop->must_raw_no_source, &scop->may_raw_no_source,
		  &scop->must_war, &scop->may_war,
		  &scop->must_war_no_source, &scop->may_war_no_source,
		  &scop->must_waw, &scop->may_waw,
		  &scop->must_waw_no_source, &scop->may_waw_no_source);

  res = (no_violations (transform, scop->must_raw)
	 && no_violations (transform, scop->may_raw)
	 && no_violations (transform, scop->must_war)
	 && no_violations (transform, scop->may_war)
	 && no_violations (transform, scop->must_waw)
	 && no_violations (transform, scop->may_waw));

  isl_union_map_free (transform);
  return res;
}

/* Return true when the SCOP transformed schedule is correct.  */

bool
graphite_legal_transform (scop_p scop)
{
  int res;
  isl_union_map *transform;

  timevar_push (TV_GRAPHITE_DATA_DEPS);
  transform = scop_get_transformed_schedule (scop, SCOP_BBS (scop));
  res = transform_is_safe (scop, transform);
  timevar_pop (TV_GRAPHITE_DATA_DEPS);

  return res;
}

/* Return true when the loop at DEPTH carries dependences.  BODY is
   the body of the loop.  */

static bool
loop_level_carries_dependences (scop_p scop, vec<poly_bb_p> body,
				int depth)
{
  isl_union_map *transform = scop_get_transformed_schedule (scop, body);
  isl_union_map *must_raw, *may_raw;
  isl_union_map *must_war, *may_war;
  isl_union_map *must_waw, *may_waw;
  int res;

  compute_deps (scop, body,
		&must_raw, &may_raw, NULL, NULL,
		&must_war, &may_war, NULL, NULL,
		&must_waw, &may_waw, NULL, NULL);

  res = (carries_deps (transform, must_raw, depth)
	 || carries_deps (transform, may_raw, depth)
	 || carries_deps (transform, must_war, depth)
	 || carries_deps (transform, may_war, depth)
	 || carries_deps (transform, must_waw, depth)
	 || carries_deps (transform, may_waw, depth));

  isl_union_map_free (transform);
  isl_union_map_free (must_raw);
  isl_union_map_free (may_raw);
  isl_union_map_free (must_war);
  isl_union_map_free (may_war);
  isl_union_map_free (must_waw);
  isl_union_map_free (may_waw);
  return res;
}

/* Returns true when the loop L at level DEPTH is parallel.
   BB_PBB_MAPPING is a map between a basic_block and its related
   poly_bb_p.  */

bool
loop_is_parallel_p (loop_p loop, bb_pbb_htab_type bb_pbb_mapping, int depth)
{
  bool dependences;
  scop_p scop;

  timevar_push (TV_GRAPHITE_DATA_DEPS);
  auto_vec<poly_bb_p, 3> body;
  scop = get_loop_body_pbbs (loop, bb_pbb_mapping, &body);
  dependences = loop_level_carries_dependences (scop, body, depth);
  timevar_pop (TV_GRAPHITE_DATA_DEPS);

  return !dependences;
}

#endif
