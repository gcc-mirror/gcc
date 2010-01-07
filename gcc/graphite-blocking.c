/* Heuristics and transform for loop blocking and strip mining on
   polyhedral representation.

   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Pranav Garg  <pranav.garg2107@gmail.com>.

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


/* Strip mines with a factor STRIDE the scattering (time) dimension
   around PBB at depth TIME_DEPTH.

   The following example comes from the wiki page:
   http://gcc.gnu.org/wiki/Graphite/Strip_mine

   The strip mine of a loop with a tile of 64 can be obtained with a
   scattering function as follows:

   $ cat ./albert_strip_mine.cloog
   # language: C
   c

   # parameter {n | n >= 0}
   1 3
   #  n  1
   1  1  0
   1
   n

   1 # Number of statements:

   1
   # {i | 0 <= i <= n}
   2 4
   #  i  n   1
   1  1  0   0
   1 -1  1   0

   0  0  0
   1
   i

   1 # Scattering functions

   3 6
   #  NEW  OLD    i    n    1
   1  -64    0    1    0    0
   1   64    0   -1    0   63
   0    0    1   -1    0    0

   1
   NEW  OLD

   #the output of CLooG is like this:
   #$ cloog ./albert_strip_mine.cloog
   # for (NEW=0;NEW<=floord(n,64);NEW++) {
   #   for (OLD=max(64*NEW,0);OLD<=min(64*NEW+63,n);OLD++) {
   #     S1(i = OLD) ;
   #   }
   # }
*/

static bool
pbb_strip_mine_time_depth (poly_bb_p pbb, int time_depth, int stride)
{
  ppl_dimension_type iter, dim, strip;
  ppl_Polyhedron_t res = PBB_TRANSFORMED_SCATTERING (pbb);
  /* STRIP is the dimension that iterates with stride STRIDE.  */
  /* ITER is the dimension that enumerates single iterations inside
     one strip that has at most STRIDE iterations.  */
  strip = time_depth;
  iter = strip + 2;

  psct_add_scattering_dimension (pbb, strip);
  psct_add_scattering_dimension (pbb, strip + 1);

  ppl_Polyhedron_space_dimension (res, &dim);

  /* Lower bound of the striped loop.  */
  {
    ppl_Constraint_t new_cstr;
    ppl_Linear_Expression_t expr;

    ppl_new_Linear_Expression_with_dimension (&expr, dim);
    ppl_set_coef (expr, strip, -1 * stride);
    ppl_set_coef (expr, iter, 1);

    ppl_new_Constraint (&new_cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
    ppl_delete_Linear_Expression (expr);
    ppl_Polyhedron_add_constraint (res, new_cstr);
    ppl_delete_Constraint (new_cstr);
  }

  /* Upper bound of the striped loop.  */
  {
    ppl_Constraint_t new_cstr;
    ppl_Linear_Expression_t expr;

    ppl_new_Linear_Expression_with_dimension (&expr, dim);
    ppl_set_coef (expr, strip, stride);
    ppl_set_coef (expr, iter, -1);
    ppl_set_inhomogeneous (expr, stride - 1);

    ppl_new_Constraint (&new_cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
    ppl_delete_Linear_Expression (expr);
    ppl_Polyhedron_add_constraint (res, new_cstr);
    ppl_delete_Constraint (new_cstr);
  }

  /* Static scheduling for ITER level.
     This is mandatory to keep the 2d + 1 canonical scheduling format.  */
  {
    ppl_Constraint_t new_cstr;
    ppl_Linear_Expression_t expr;

    ppl_new_Linear_Expression_with_dimension (&expr, dim);
    ppl_set_coef (expr, strip + 1, 1);
    ppl_set_inhomogeneous (expr, 0);

    ppl_new_Constraint (&new_cstr, expr, PPL_CONSTRAINT_TYPE_EQUAL);
    ppl_delete_Linear_Expression (expr);
    ppl_Polyhedron_add_constraint (res, new_cstr);
    ppl_delete_Constraint (new_cstr);
  }

  return true;
}

/* Returns true when strip mining with STRIDE of the loop around PBB
   at DEPTH is profitable.  */

static bool
pbb_strip_mine_profitable_p (poly_bb_p pbb,
			     graphite_dim_t depth,
			     int stride)
{
  Value niter, strip_stride;
  bool res;

  value_init (strip_stride);
  value_init (niter);
  value_set_si (strip_stride, stride);
  pbb_number_of_iterations_at_time (pbb, psct_dynamic_dim (pbb, depth), niter);
  res = value_gt (niter, strip_stride);
  value_clear (strip_stride);
  value_clear (niter);

  return res;
}

/* Strip-mines all the loops of LST that are considered profitable to
   strip-mine.  Return true if it did strip-mined some loops.  */

static bool
lst_do_strip_mine_loop (lst_p lst, int depth)
{
  int i;
  lst_p l;
  int stride = PARAM_VALUE (PARAM_LOOP_BLOCK_TILE_SIZE);
  poly_bb_p pbb;

  if (!lst)
    return false;

  if (LST_LOOP_P (lst))
    {
      bool res = false;

      for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
	res |= lst_do_strip_mine_loop (l, depth);

      return res;
    }

  pbb = LST_PBB (lst);
  return pbb_strip_mine_time_depth (pbb, psct_dynamic_dim (pbb, depth),
				    stride);
}

/* Strip-mines all the loops of LST that are considered profitable to
   strip-mine.  Return true if it did strip-mined some loops.  */

static bool
lst_do_strip_mine (lst_p lst)
{
  int i;
  lst_p l;
  bool res = false;
  int stride = PARAM_VALUE (PARAM_LOOP_BLOCK_TILE_SIZE);
  int depth;

  if (!lst
      || !LST_LOOP_P (lst))
    return false;

  for (i = 0; VEC_iterate (lst_p, LST_SEQ (lst), i, l); i++)
    res |= lst_do_strip_mine (l);

  depth = lst_depth (lst);
  if (depth >= 0
      && pbb_strip_mine_profitable_p (LST_PBB (lst_find_first_pbb (lst)),
				      depth, stride))
    {
      res |= lst_do_strip_mine_loop (lst, lst_depth (lst));
      lst_add_loop_under_loop (lst);
    }

  return res;
}

/* Strip mines all the loops in SCOP.  Nothing profitable in all this:
   this is just a driver function.  */

bool
scop_do_strip_mine (scop_p scop)
{
  bool transform_done = false;

  store_scattering (scop);

  transform_done = lst_do_strip_mine (SCOP_TRANSFORMED_SCHEDULE (scop));

  if (!transform_done)
    return false;

  if (!graphite_legal_transform (scop))
    {
      restore_scattering (scop);
      return false;
    }

  return transform_done;
}

/* Loop blocks all the loops in SCOP.  Returns true when we manage to
   block some loops.  */

bool
scop_do_block (scop_p scop)
{
  bool strip_mined = false;
  bool interchanged = false;

  store_scattering (scop);

  strip_mined = lst_do_strip_mine (SCOP_TRANSFORMED_SCHEDULE (scop));
  interchanged = scop_do_interchange (scop);

  /* If we don't interchange loops, then the strip mine is not
     profitable, and the transform is not a loop blocking.  */
  if (!interchanged
      || !graphite_legal_transform (scop))
    {
      restore_scattering (scop);
      return false;
    }
  else if (strip_mined && interchanged
	   && dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "SCoP will be loop blocked.\n");

  return strip_mined || interchanged;
}

#endif
