/* Heuristics and transform for loop blocking and strip mining on
   polyhedral representation.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "ppl_c.h"
#include "graphite-ppl.h"
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

/* Returns true when strip mining with STRIDE of the loop LST is
   profitable.  */

static bool
lst_strip_mine_profitable_p (lst_p lst, int stride)
{
  mpz_t niter, strip_stride;
  bool res;

  gcc_assert (LST_LOOP_P (lst));
  mpz_init (strip_stride);
  mpz_init (niter);

  mpz_set_si (strip_stride, stride);
  lst_niter_for_loop (lst, niter);
  res = (mpz_cmp (niter, strip_stride) > 0);

  mpz_clear (strip_stride);
  mpz_clear (niter);
  return res;
}

/* Strip-mines all the loops of LST with STRIDE.  Return true if it
   did strip-mined some loops.  */

static bool
lst_do_strip_mine_loop (lst_p lst, int depth, int stride)
{
  int i;
  lst_p l;
  poly_bb_p pbb;

  if (!lst)
    return false;

  if (LST_LOOP_P (lst))
    {
      bool res = false;

      FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
	res |= lst_do_strip_mine_loop (l, depth, stride);

      return res;
    }

  pbb = LST_PBB (lst);
  return pbb_strip_mine_time_depth (pbb, psct_dynamic_dim (pbb, depth),
				    stride);
}

/* Strip-mines all the loops of LST with STRIDE.  When STRIDE is zero,
   read the stride from the PARAM_LOOP_BLOCK_TILE_SIZE.  Return true
   if it did strip-mined some loops.

   Strip mining transforms a loop

   | for (i = 0; i < N; i++)
   |   S (i);

   into the following loop nest:

   | for (k = 0; k < N; k += STRIDE)
   |   for (j = 0; j < STRIDE; j++)
   |     S (i = k + j);
*/

static bool
lst_do_strip_mine (lst_p lst, int stride)
{
  int i;
  lst_p l;
  bool res = false;
  int depth;

  if (!stride)
    stride = PARAM_VALUE (PARAM_LOOP_BLOCK_TILE_SIZE);

  if (!lst
      || !LST_LOOP_P (lst))
    return false;

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
    res |= lst_do_strip_mine (l, stride);

  depth = lst_depth (lst);
  if (depth >= 0
      && lst_strip_mine_profitable_p (lst, stride))
    {
      res |= lst_do_strip_mine_loop (lst, lst_depth (lst), stride);
      lst_add_loop_under_loop (lst);
    }

  return res;
}

/* Strip mines all the loops in SCOP.  Returns true when some loops
   have been strip-mined.  */

bool
scop_do_strip_mine (scop_p scop, int stride)
{
  return lst_do_strip_mine (SCOP_TRANSFORMED_SCHEDULE (scop), stride);
}

/* Loop blocks all the loops in SCOP.  Returns true when we manage to
   block some loops.  */

bool
scop_do_block (scop_p scop)
{
  bool strip_mined = false;
  bool interchanged = false;

  store_scattering (scop);

  strip_mined = lst_do_strip_mine (SCOP_TRANSFORMED_SCHEDULE (scop), 0);
  interchanged = scop_do_interchange (scop);

  /* If we don't interchange loops, the strip mine alone will not be
     profitable, and the transform is not a loop blocking: so revert
     the transform.  */
  if (!interchanged)
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
