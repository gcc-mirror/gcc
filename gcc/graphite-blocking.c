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
   at scattering time TIME_DEPTH is profitable.  */

static bool
pbb_strip_mine_profitable_p (poly_bb_p pbb,
			     graphite_dim_t time_depth,
			     int stride)
{
  Value niter, strip_stride;
  bool res;

  value_init (strip_stride);
  value_init (niter);
  value_set_si (strip_stride, stride);
  pbb_number_of_iterations_at_time (pbb, time_depth, niter);
  res = value_gt (niter, strip_stride);
  value_clear (strip_stride);
  value_clear (niter);

  return res;
}

/* Strip mines all the loops around PBB.  Nothing profitable in all this:
   this is just a driver function.  */

static bool
pbb_do_strip_mine (poly_bb_p pbb)
{
  graphite_dim_t s_dim;
  int stride = 64;
  bool transform_done = false;

  for (s_dim = 0; s_dim < pbb_nb_dynamic_scattering_transform (pbb); s_dim++)
    if (pbb_strip_mine_profitable_p (pbb, psct_dynamic_dim (pbb, s_dim),
                                     stride))
      {
	ppl_dimension_type d = psct_dynamic_dim (pbb, s_dim);
	transform_done |= pbb_strip_mine_time_depth (pbb, d, stride);
	s_dim++;
      }

  return transform_done;
}

/* Strip mines all the loops in SCOP.  Nothing profitable in all this:
   this is just a driver function.  */

bool
scop_do_strip_mine (scop_p scop)
{
  poly_bb_p pbb;
  int i;
  bool transform_done = false;

  store_scattering (scop);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    transform_done |= pbb_do_strip_mine (pbb);

  if (!transform_done)
    return false;

  if (!graphite_legal_transform (scop))
    {
      restore_scattering (scop);
      return false;
    }

  return true;
}

#endif
