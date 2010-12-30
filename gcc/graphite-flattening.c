/* Loop flattening for Graphite.
   Copyright (C) 2010 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com>.

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
#include "tree-scalar-evolution.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "ppl_c.h"
#include "graphite-ppl.h"
#include "graphite-poly.h"

/* The loop flattening pass transforms loop nests into a single loop,
   removing the loop nesting structure.  The auto-vectorization can
   then apply on the full loop body, without needing the outer-loop
   vectorization.

   The loop flattening pass that has been described in a very Fortran
   specific way in the 1992 paper by Reinhard von Hanxleden and Ken
   Kennedy: "Relaxing SIMD Control Flow Constraints using Loop
   Transformations" available from
   http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.54.5033

   The canonical example is as follows: suppose that we have a loop
   nest with known iteration counts

   | for (i = 1; i <= 6; i++)
   |   for (j = 1; j <= 6; j++)
   |     S1(i,j);

   The loop flattening is performed by linearizing the iteration space
   using the function "f (x) = 6 * i + j".  In this case, CLooG would
   produce this code:

   | for (c1=7;c1<=42;c1++) {
   |   i = floord(c1-1,6);
   |   S1(i,c1-6*i);
   | }

   There are several limitations for loop flattening that are linked
   to the expressivity of the polyhedral model.  One has to take an
   upper bound approximation to deal with the parametric case of loop
   flattening.  For example, in the loop nest:

   | for (i = 1; i <= N; i++)
   |   for (j = 1; j <= M; j++)
   |     S1(i,j);

   One would like to flatten this loop using a linearization function
   like this "f (x) = M * i + j".  However CLooG's schedules are not
   expressive enough to deal with this case, and so the parameter M
   has to be replaced by an integer upper bound approximation.  If we
   further know in the context of the scop that "M <= 6", then it is
   possible to linearize the loop with "f (x) = 6 * i + j".  In this
   case, CLooG would produce this code:

   | for (c1=7;c1<=6*M+N;c1++) {
   |   i = ceild(c1-N,6);
   |   if (i <= floord(c1-1,6)) {
   |     S1(i,c1-6*i);
   |   }
   | }

   For an arbitrarily complex loop nest the algorithm proceeds in two
   steps.  First, the LST is flattened by removing the loops structure
   and by inserting the statements in the order they appear in
   depth-first order.  Then, the scattering of each statement is
   transformed accordingly.

   Supposing that the original program is represented by the following
   LST:

   | (loop_1
   |  stmt_1
   |  (loop_2 stmt_3
   |   (loop_3 stmt_4)
   |   (loop_4 stmt_5 stmt_6)
   |   stmt_7
   |  )
   |  stmt_2
   | )

   Loop flattening traverses the LST in depth-first order, and
   flattens pairs of loops successively by projecting the inner loops
   in the iteration domain of the outer loops:

   lst_project_loop (loop_2, loop_3, stride)

   | (loop_1
   |  stmt_1
   |  (loop_2 stmt_3 stmt_4
   |   (loop_4 stmt_5 stmt_6)
   |   stmt_7
   |  )
   |  stmt_2
   | )

   lst_project_loop (loop_2, loop_4, stride)

   | (loop_1
   |  stmt_1
   |  (loop_2 stmt_3 stmt_4 stmt_5 stmt_6 stmt_7)
   |  stmt_2
   | )

   lst_project_loop (loop_1, loop_2, stride)

   | (loop_1
   |  stmt_1 stmt_3 stmt_4 stmt_5 stmt_6 stmt_7 stmt_2
   | )

   At each step, the iteration domain of the outer loop is enlarged to
   contain enough points to iterate over the inner loop domain.  */

/* Initializes RES to the number of iterations of the linearized loop
   LST.  RES is the cardinal of the iteration domain of LST.  */

static void
lst_linearized_niter (lst_p lst, mpz_t res)
{
  int i;
  lst_p l;
  mpz_t n;

  mpz_init (n);
  mpz_set_si (res, 0);

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
    if (LST_LOOP_P (l))
      {
	lst_linearized_niter (l, n);
	mpz_add (res, res, n);
      }

  if (LST_LOOP_P (lst))
    {
      lst_niter_for_loop (lst, n);

      if (mpz_cmp_si (res, 0) != 0)
	mpz_mul (res, res, n);
      else
	mpz_set (res, n);
    }

  mpz_clear (n);
}

/* Applies the translation "f (x) = x + OFFSET" to the loop containing
   STMT.  */

static void
lst_offset (lst_p stmt, mpz_t offset)
{
  lst_p inner = LST_LOOP_FATHER (stmt);
  poly_bb_p pbb = LST_PBB (stmt);
  ppl_Polyhedron_t poly = PBB_TRANSFORMED_SCATTERING (pbb);
  int inner_depth = lst_depth (inner);
  ppl_dimension_type inner_dim = psct_dynamic_dim (pbb, inner_depth);
  ppl_Linear_Expression_t expr;
  ppl_dimension_type dim;
  ppl_Coefficient_t one;
  mpz_t x;

  mpz_init (x);
  mpz_set_si (x, 1);
  ppl_new_Coefficient (&one);
  ppl_assign_Coefficient_from_mpz_t (one, x);

  ppl_Polyhedron_space_dimension (poly, &dim);
  ppl_new_Linear_Expression_with_dimension (&expr, dim);

  ppl_set_coef (expr, inner_dim, 1);
  ppl_set_inhomogeneous_gmp (expr, offset);
  ppl_Polyhedron_affine_image (poly, inner_dim, expr, one);
  ppl_delete_Linear_Expression (expr);
  ppl_delete_Coefficient (one);
}

/* Scale by FACTOR the loop LST containing STMT.  */

static void
lst_scale (lst_p lst, lst_p stmt, mpz_t factor)
{
  mpz_t x;
  ppl_Coefficient_t one;
  int outer_depth = lst_depth (lst);
  poly_bb_p pbb = LST_PBB (stmt);
  ppl_Polyhedron_t poly = PBB_TRANSFORMED_SCATTERING (pbb);
  ppl_dimension_type outer_dim = psct_dynamic_dim (pbb, outer_depth);
  ppl_Linear_Expression_t expr;
  ppl_dimension_type dim;

  mpz_init (x);
  mpz_set_si (x, 1);
  ppl_new_Coefficient (&one);
  ppl_assign_Coefficient_from_mpz_t (one, x);

  ppl_Polyhedron_space_dimension (poly, &dim);
  ppl_new_Linear_Expression_with_dimension (&expr, dim);

  /* outer_dim = factor * outer_dim.  */
  ppl_set_coef_gmp (expr, outer_dim, factor);
  ppl_Polyhedron_affine_image (poly, outer_dim, expr, one);
  ppl_delete_Linear_Expression (expr);

  mpz_clear (x);
  ppl_delete_Coefficient (one);
}

/* Project the INNER loop into the iteration domain of the OUTER loop.
   STRIDE is the number of iterations between two iterations of the
   outer loop.  */

static void
lst_project_loop (lst_p outer, lst_p inner, mpz_t stride)
{
  int i;
  lst_p stmt;
  mpz_t x;
  ppl_Coefficient_t one;
  int outer_depth = lst_depth (outer);
  int inner_depth = lst_depth (inner);

  mpz_init (x);
  mpz_set_si (x, 1);
  ppl_new_Coefficient (&one);
  ppl_assign_Coefficient_from_mpz_t (one, x);

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (inner), i, stmt)
    {
      poly_bb_p pbb = LST_PBB (stmt);
      ppl_Polyhedron_t poly = PBB_TRANSFORMED_SCATTERING (pbb);
      ppl_dimension_type outer_dim = psct_dynamic_dim (pbb, outer_depth);
      ppl_dimension_type inner_dim = psct_dynamic_dim (pbb, inner_depth);
      ppl_Linear_Expression_t expr;
      ppl_dimension_type dim;
      ppl_dimension_type *ds;

      /* There should be no loops under INNER.  */
      gcc_assert (!LST_LOOP_P (stmt));
      ppl_Polyhedron_space_dimension (poly, &dim);
      ppl_new_Linear_Expression_with_dimension (&expr, dim);

      /* outer_dim = outer_dim * stride + inner_dim.  */
      ppl_set_coef (expr, inner_dim, 1);
      ppl_set_coef_gmp (expr, outer_dim, stride);
      ppl_Polyhedron_affine_image (poly, outer_dim, expr, one);
      ppl_delete_Linear_Expression (expr);

      /* Project on inner_dim.  */
      ppl_new_Linear_Expression_with_dimension (&expr, dim - 1);
      ppl_Polyhedron_affine_image (poly, inner_dim, expr, one);
      ppl_delete_Linear_Expression (expr);

      /* Remove inner loop and the static schedule of its body.  */
      ds = XNEWVEC (ppl_dimension_type, 2);
      ds[0] = inner_dim;
      ds[1] = inner_dim + 1;
      ppl_Polyhedron_remove_space_dimensions (poly, ds, 2);
      PBB_NB_SCATTERING_TRANSFORM (pbb) -= 2;
      free (ds);
    }

  mpz_clear (x);
  ppl_delete_Coefficient (one);
}

/* Flattens the loop nest LST.  Return true when something changed.
   OFFSET is used to compute the number of iterations of the outermost
   loop before the current LST is executed.  */

static bool
lst_flatten_loop (lst_p lst, mpz_t init_offset)
{
  int i;
  lst_p l;
  bool res = false;
  mpz_t n, one, offset, stride;

  mpz_init (n);
  mpz_init (one);
  mpz_init (offset);
  mpz_init (stride);
  mpz_set (offset, init_offset);
  mpz_set_si (one, 1);

  lst_linearized_niter (lst, stride);
  lst_niter_for_loop (lst, n);
  mpz_tdiv_q (stride, stride, n);

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
    if (LST_LOOP_P (l))
      {
	res = true;

	lst_flatten_loop (l, offset);
	lst_niter_for_loop (l, n);

	lst_project_loop (lst, l, stride);

	/* The offset is the number of iterations minus 1, as we want
	   to execute the next statements at the same iteration as the
	   last iteration of the loop.  */
	mpz_sub (n, n, one);
	mpz_add (offset, offset, n);
      }
    else
      {
	lst_scale (lst, l, stride);
	if (mpz_cmp_si (offset, 0) != 0)
	  lst_offset (l, offset);
      }

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
    if (LST_LOOP_P (l))
      lst_remove_loop_and_inline_stmts_in_loop_father (l);

  mpz_clear (n);
  mpz_clear (one);
  mpz_clear (offset);
  mpz_clear (stride);
  return res;
}

/* Remove all but the first 3 dimensions of the scattering:
   - dim0: the static schedule for the loop
   - dim1: the dynamic schedule of the loop
   - dim2: the static schedule for the loop body.  */

static void
remove_unused_scattering_dimensions (lst_p lst)
{
  int i;
  lst_p stmt;
  mpz_t x;
  ppl_Coefficient_t one;

  mpz_init (x);
  mpz_set_si (x, 1);
  ppl_new_Coefficient (&one);
  ppl_assign_Coefficient_from_mpz_t (one, x);

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, stmt)
    {
      poly_bb_p pbb = LST_PBB (stmt);
      ppl_Polyhedron_t poly = PBB_TRANSFORMED_SCATTERING (pbb);
      int j, nb_dims_to_remove = PBB_NB_SCATTERING_TRANSFORM (pbb) - 3;
      ppl_dimension_type *ds;

      /* There should be no loops inside LST after flattening.  */
      gcc_assert (!LST_LOOP_P (stmt));

      if (!nb_dims_to_remove)
	continue;

      ds = XNEWVEC (ppl_dimension_type, nb_dims_to_remove);
      for (j = 0; j < nb_dims_to_remove; j++)
	ds[j] = j + 3;

      ppl_Polyhedron_remove_space_dimensions (poly, ds, nb_dims_to_remove);
      PBB_NB_SCATTERING_TRANSFORM (pbb) -= nb_dims_to_remove;
      free (ds);
    }

  mpz_clear (x);
  ppl_delete_Coefficient (one);
}

/* Flattens all the loop nests of LST.  Return true when something
   changed.  */

static bool
lst_do_flatten (lst_p lst)
{
  int i;
  lst_p l;
  bool res = false;
  mpz_t zero;

  if (!lst
      || !LST_LOOP_P (lst))
    return false;

  mpz_init (zero);
  mpz_set_si (zero, 0);

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
    if (LST_LOOP_P (l))
      {
	res |= lst_flatten_loop (l, zero);
	remove_unused_scattering_dimensions (l);
      }

  lst_update_scattering (lst);
  mpz_clear (zero);
  return res;
}

/* Flatten all the loop nests in SCOP.  Returns true when something
   changed.  */

bool
flatten_all_loops (scop_p scop)
{
  return lst_do_flatten (SCOP_TRANSFORMED_SCHEDULE (scop));
}

#endif
