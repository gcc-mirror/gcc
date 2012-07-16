/* Interchange heuristics and transform for loop interchange on
   polyhedral representation.

   Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Harsha Jagasia <harsha.jagasia@amd.com>.

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
#include <isl/aff.h>
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/ilp.h>
#include <cloog/cloog.h>
#include <cloog/isl/domain.h>
#endif

#include "system.h"
#include "coretypes.h"
#include "tree-flow.h"
#include "dumpfile.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "graphite-poly.h"

/* XXX isl rewrite following comment */
/* Builds a linear expression, of dimension DIM, representing PDR's
   memory access:

   L = r_{n}*r_{n-1}*...*r_{1}*s_{0} + ... + r_{n}*s_{n-1} + s_{n}.

   For an array A[10][20] with two subscript locations s0 and s1, the
   linear memory access is 20 * s0 + s1: a stride of 1 in subscript s0
   corresponds to a memory stride of 20.

   OFFSET is a number of dimensions to prepend before the
   subscript dimensions: s_0, s_1, ..., s_n.

   Thus, the final linear expression has the following format:
   0 .. 0_{offset} | 0 .. 0_{nit} | 0 .. 0_{gd} | 0 | c_0 c_1 ... c_n
   where the expression itself is:
   c_0 * s_0 + c_1 * s_1 + ... c_n * s_n.  */

static isl_constraint *
build_linearized_memory_access (isl_map *map, poly_dr_p pdr)
{
  isl_constraint *res;
  isl_local_space *ls = isl_local_space_from_space (isl_map_get_space (map));
  unsigned offset, nsubs;
  int i;
  isl_int size, subsize;

  res = isl_equality_alloc (ls);
  isl_int_init (size);
  isl_int_set_ui (size, 1);
  isl_int_init (subsize);
  isl_int_set_ui (subsize, 1);

  nsubs = isl_set_dim (pdr->extent, isl_dim_set);
  /* -1 for the already included L dimension.  */
  offset = isl_map_dim (map, isl_dim_out) - 1 - nsubs;
  res = isl_constraint_set_coefficient_si (res, isl_dim_out, offset + nsubs, -1);
  /* Go through all subscripts from last to first.  First dimension
     is the alias set, ignore it.  */
  for (i = nsubs - 1; i >= 1; i--)
    {
      isl_space *dc;
      isl_aff *aff;

      res = isl_constraint_set_coefficient (res, isl_dim_out, offset + i, size);

      dc = isl_set_get_space (pdr->extent);
      aff = isl_aff_zero_on_domain (isl_local_space_from_space (dc));
      aff = isl_aff_set_coefficient_si (aff, isl_dim_in, i, 1);
      isl_set_max (pdr->extent, aff, &subsize);
      isl_aff_free (aff);
      isl_int_mul (size, size, subsize);
    }

  isl_int_clear (subsize);
  isl_int_clear (size);

  return res;
}

/* Set STRIDE to the stride of PDR in memory by advancing by one in
   the loop at DEPTH.  */

static void
pdr_stride_in_loop (mpz_t stride, graphite_dim_t depth, poly_dr_p pdr)
{
  poly_bb_p pbb = PDR_PBB (pdr);
  isl_map *map;
  isl_set *set;
  isl_aff *aff;
  isl_space *dc;
  isl_constraint *lma, *c;
  isl_int islstride;
  graphite_dim_t time_depth;
  unsigned offset, nt;
  unsigned i;
  /* XXX isl rewrite following comments.  */
  /* Builds a partial difference equations and inserts them
     into pointset powerset polyhedron P.  Polyhedron is assumed
     to have the format: T|I|T'|I'|G|S|S'|l1|l2.

     TIME_DEPTH is the time dimension w.r.t. which we are
     differentiating.
     OFFSET represents the number of dimensions between
     columns t_{time_depth} and t'_{time_depth}.
     DIM_SCTR is the number of scattering dimensions.  It is
     essentially the dimensionality of the T vector.

     The following equations are inserted into the polyhedron P:
     | t_1 = t_1'
     | ...
     | t_{time_depth-1} = t'_{time_depth-1}
     | t_{time_depth} = t'_{time_depth} + 1
     | t_{time_depth+1} = t'_{time_depth + 1}
     | ...
     | t_{dim_sctr} = t'_{dim_sctr}.  */

  /* Add the equality: t_{time_depth} = t'_{time_depth} + 1.
     This is the core part of this alogrithm, since this
     constraint asks for the memory access stride (difference)
     between two consecutive points in time dimensions.  */

  /* Add equalities:
     | t1 = t1'
     | ...
     | t_{time_depth-1} = t'_{time_depth-1}
     | t_{time_depth+1} = t'_{time_depth+1}
     | ...
     | t_{dim_sctr} = t'_{dim_sctr}

     This means that all the time dimensions are equal except for
     time_depth, where the constraint is t_{depth} = t'_{depth} + 1
     step.  More to this: we should be careful not to add equalities
     to the 'coupled' dimensions, which happens when the one dimension
     is stripmined dimension, and the other dimension corresponds
     to the point loop inside stripmined dimension.  */

  /* pdr->accesses:    [P1..nb_param,I1..nb_domain]->[a,S1..nb_subscript]
          ??? [P] not used for PDRs?
     pdr->extent:      [a,S1..nb_subscript]
     pbb->domain:      [P1..nb_param,I1..nb_domain]
     pbb->transformed: [P1..nb_param,I1..nb_domain]->[T1..Tnb_sctr]
          [T] includes local vars (currently unused)
     
     First we create [P,I] -> [T,a,S].  */
  
  map = isl_map_flat_range_product (isl_map_copy (pbb->transformed),
				    isl_map_copy (pdr->accesses));
  /* Add a dimension for L: [P,I] -> [T,a,S,L].*/
  map = isl_map_add_dims (map, isl_dim_out, 1);
  /* Build a constraint for "lma[S] - L == 0", effectively calculating
     L in terms of subscripts.  */
  lma = build_linearized_memory_access (map, pdr);
  /* And add it to the map, so we now have:
     [P,I] -> [T,a,S,L] : lma([S]) == L.  */
  map = isl_map_add_constraint (map, lma);

  /* Then we create  [P,I,P',I'] -> [T,a,S,L,T',a',S',L'].  */
  map = isl_map_flat_product (map, isl_map_copy (map));

  /* Now add the equality T[time_depth] == T'[time_depth]+1.  This will
     force L' to be the linear address at T[time_depth] + 1. */
  time_depth = psct_dynamic_dim (pbb, depth);
  /* Length of [a,S] plus [L] ...  */
  offset = 1 + isl_map_dim (pdr->accesses, isl_dim_out);
  /* ... plus [T].  */
  offset += isl_map_dim (pbb->transformed, isl_dim_out);

  c = isl_equality_alloc (isl_local_space_from_space (isl_map_get_space (map)));
  c = isl_constraint_set_coefficient_si (c, isl_dim_out, time_depth, 1);
  c = isl_constraint_set_coefficient_si (c, isl_dim_out,
					 offset + time_depth, -1);
  c = isl_constraint_set_constant_si (c, 1);
  map = isl_map_add_constraint (map, c);

  /* Now we equate most of the T/T' elements (making PITaSL nearly
     the same is (PITaSL)', except for one dimension, namely for 'depth'
     (an index into [I]), after translating to index into [T].  Take care
     to not produce an empty map, which indicates we wanted to equate
     two dimensions that are already coupled via the above time_depth
     dimension.  Happens with strip mining where several scatter dimension
     are interdependend.  */
  /* Length of [T].  */
  nt = pbb_nb_scattering_transform (pbb) + pbb_nb_local_vars (pbb);
  for (i = 0; i < nt; i++)
    if (i != time_depth)
      {
	isl_map *temp = isl_map_equate (isl_map_copy (map),
					isl_dim_out, i,
					isl_dim_out, offset + i);
	if (isl_map_is_empty (temp))
	  isl_map_free (temp);
	else
	  {
	    isl_map_free (map);
	    map = temp;
	  }
      }

  /* Now maximize the expression L' - L.  */
  set = isl_map_range (map);
  dc = isl_set_get_space (set);
  aff = isl_aff_zero_on_domain (isl_local_space_from_space (dc));
  aff = isl_aff_set_coefficient_si (aff, isl_dim_in, offset - 1, -1);
  aff = isl_aff_set_coefficient_si (aff, isl_dim_in, offset + offset - 1, 1);
  isl_int_init (islstride);
  isl_set_max (set, aff, &islstride);
  isl_int_get_gmp (islstride, stride);
  isl_int_clear (islstride);
  isl_aff_free (aff);
  isl_set_free (set);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      char *str;
      void (*gmp_free) (void *, size_t);

      fprintf (dump_file, "\nStride in BB_%d, DR_%d, depth %d:",
	       pbb_index (pbb), PDR_ID (pdr), (int) depth);
      str = mpz_get_str (0, 10, stride);
      fprintf (dump_file, "  %s ", str);
      mp_get_memory_functions (NULL, NULL, &gmp_free);
      (*gmp_free) (str, strlen (str) + 1);
    }
}

/* Sets STRIDES to the sum of all the strides of the data references
   accessed in LOOP at DEPTH.  */

static void
memory_strides_in_loop_1 (lst_p loop, graphite_dim_t depth, mpz_t strides)
{
  int i, j;
  lst_p l;
  poly_dr_p pdr;
  mpz_t s, n;

  mpz_init (s);
  mpz_init (n);

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (loop), j, l)
    if (LST_LOOP_P (l))
      memory_strides_in_loop_1 (l, depth, strides);
    else
      FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (LST_PBB (l)), i, pdr)
	{
	  pdr_stride_in_loop (s, depth, pdr);
	  mpz_set_si (n, PDR_NB_REFS (pdr));
	  mpz_mul (s, s, n);
	  mpz_add (strides, strides, s);
	}

  mpz_clear (s);
  mpz_clear (n);
}

/* Sets STRIDES to the sum of all the strides of the data references
   accessed in LOOP at DEPTH.  */

static void
memory_strides_in_loop (lst_p loop, graphite_dim_t depth, mpz_t strides)
{
  if (mpz_cmp_si (loop->memory_strides, -1) == 0)
    {
      mpz_set_si (strides, 0);
      memory_strides_in_loop_1 (loop, depth, strides);
    }
  else
    mpz_set (strides, loop->memory_strides);
}

/* Return true when the interchange of loops LOOP1 and LOOP2 is
   profitable.

   Example:

   | int a[100][100];
   |
   | int
   | foo (int N)
   | {
   |   int j;
   |   int i;
   |
   |   for (i = 0; i < N; i++)
   |     for (j = 0; j < N; j++)
   |       a[j][2 * i] += 1;
   |
   |   return a[N][12];
   | }

   The data access A[j][i] is described like this:

   | i   j   N   a  s0  s1   1
   | 0   0   0   1   0   0  -5    = 0
   | 0  -1   0   0   1   0   0    = 0
   |-2   0   0   0   0   1   0    = 0
   | 0   0   0   0   1   0   0   >= 0
   | 0   0   0   0   0   1   0   >= 0
   | 0   0   0   0  -1   0 100   >= 0
   | 0   0   0   0   0  -1 100   >= 0

   The linearized memory access L to A[100][100] is:

   | i   j   N   a  s0  s1   1
   | 0   0   0   0 100   1   0

   TODO: the shown format is not valid as it does not show the fact
   that the iteration domain "i j" is transformed using the scattering.

   Next, to measure the impact of iterating once in loop "i", we build
   a maximization problem: first, we add to DR accesses the dimensions
   k, s2, s3, L1 = 100 * s0 + s1, L2, and D1: this is the polyhedron P1.
   L1 and L2 are the linearized memory access functions.

   | i   j   N   a  s0  s1   k  s2  s3  L1  L2  D1   1
   | 0   0   0   1   0   0   0   0   0   0   0   0  -5    = 0  alias = 5
   | 0  -1   0   0   1   0   0   0   0   0   0   0   0    = 0  s0 = j
   |-2   0   0   0   0   1   0   0   0   0   0   0   0    = 0  s1 = 2 * i
   | 0   0   0   0   1   0   0   0   0   0   0   0   0   >= 0
   | 0   0   0   0   0   1   0   0   0   0   0   0   0   >= 0
   | 0   0   0   0  -1   0   0   0   0   0   0   0 100   >= 0
   | 0   0   0   0   0  -1   0   0   0   0   0   0 100   >= 0
   | 0   0   0   0 100   1   0   0   0  -1   0   0   0    = 0  L1 = 100 * s0 + s1

   Then, we generate the polyhedron P2 by interchanging the dimensions
   (s0, s2), (s1, s3), (L1, L2), (k, i)

   | i   j   N   a  s0  s1   k  s2  s3  L1  L2  D1   1
   | 0   0   0   1   0   0   0   0   0   0   0   0  -5    = 0  alias = 5
   | 0  -1   0   0   0   0   0   1   0   0   0   0   0    = 0  s2 = j
   | 0   0   0   0   0   0  -2   0   1   0   0   0   0    = 0  s3 = 2 * k
   | 0   0   0   0   0   0   0   1   0   0   0   0   0   >= 0
   | 0   0   0   0   0   0   0   0   1   0   0   0   0   >= 0
   | 0   0   0   0   0   0   0  -1   0   0   0   0 100   >= 0
   | 0   0   0   0   0   0   0   0  -1   0   0   0 100   >= 0
   | 0   0   0   0   0   0   0 100   1   0  -1   0   0    = 0  L2 = 100 * s2 + s3

   then we add to P2 the equality k = i + 1:

   |-1   0   0   0   0   0   1   0   0   0   0   0  -1    = 0  k = i + 1

   and finally we maximize the expression "D1 = max (P1 inter P2, L2 - L1)".

   Similarly, to determine the impact of one iteration on loop "j", we
   interchange (k, j), we add "k = j + 1", and we compute D2 the
   maximal value of the difference.

   Finally, the profitability test is D1 < D2: if in the outer loop
   the strides are smaller than in the inner loop, then it is
   profitable to interchange the loops at DEPTH1 and DEPTH2.  */

static bool
lst_interchange_profitable_p (lst_p nest, int depth1, int depth2)
{
  mpz_t d1, d2;
  bool res;

  gcc_assert (depth1 < depth2);

  mpz_init (d1);
  mpz_init (d2);

  memory_strides_in_loop (nest, depth1, d1);
  memory_strides_in_loop (nest, depth2, d2);

  res = mpz_cmp (d1, d2) < 0;

  mpz_clear (d1);
  mpz_clear (d2);

  return res;
}

/* Interchanges the loops at DEPTH1 and DEPTH2 of the original
   scattering and assigns the resulting polyhedron to the transformed
   scattering.  */

static void
pbb_interchange_loop_depths (graphite_dim_t depth1, graphite_dim_t depth2,
			     poly_bb_p pbb)
{
  unsigned i;
  unsigned dim1 = psct_dynamic_dim (pbb, depth1);
  unsigned dim2 = psct_dynamic_dim (pbb, depth2);
  isl_space *d = isl_map_get_space (pbb->transformed);
  isl_space *d1 = isl_space_range (d);
  unsigned n = isl_space_dim (d1, isl_dim_out);
  isl_space *d2 = isl_space_add_dims (d1, isl_dim_in, n);
  isl_map *x = isl_map_universe (d2);

  x = isl_map_equate (x, isl_dim_in, dim1, isl_dim_out, dim2);
  x = isl_map_equate (x, isl_dim_in, dim2, isl_dim_out, dim1);

  for (i = 0; i < n; i++)
    if (i != dim1 && i != dim2)
      x = isl_map_equate (x, isl_dim_in, i, isl_dim_out, i);

  pbb->transformed = isl_map_apply_range (pbb->transformed, x);
}

/* Apply the interchange of loops at depths DEPTH1 and DEPTH2 to all
   the statements below LST.  */

static void
lst_apply_interchange (lst_p lst, int depth1, int depth2)
{
  if (!lst)
    return;

  if (LST_LOOP_P (lst))
    {
      int i;
      lst_p l;

      FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
	lst_apply_interchange (l, depth1, depth2);
    }
  else
    pbb_interchange_loop_depths (depth1, depth2, LST_PBB (lst));
}

/* Return true when the nest starting at LOOP1 and ending on LOOP2 is
   perfect: i.e. there are no sequence of statements.  */

static bool
lst_perfectly_nested_p (lst_p loop1, lst_p loop2)
{
  if (loop1 == loop2)
    return true;

  if (!LST_LOOP_P (loop1))
    return false;

  return VEC_length (lst_p, LST_SEQ (loop1)) == 1
    && lst_perfectly_nested_p (VEC_index (lst_p, LST_SEQ (loop1), 0), loop2);
}

/* Transform the loop nest between LOOP1 and LOOP2 into a perfect
   nest.  To continue the naming tradition, this function is called
   after perfect_nestify.  NEST is set to the perfectly nested loop
   that is created.  BEFORE/AFTER are set to the loops distributed
   before/after the loop NEST.  */

static void
lst_perfect_nestify (lst_p loop1, lst_p loop2, lst_p *before,
		     lst_p *nest, lst_p *after)
{
  poly_bb_p first, last;

  gcc_assert (loop1 && loop2
	      && loop1 != loop2
	      && LST_LOOP_P (loop1) && LST_LOOP_P (loop2));

  first = LST_PBB (lst_find_first_pbb (loop2));
  last = LST_PBB (lst_find_last_pbb (loop2));

  *before = copy_lst (loop1);
  *nest = copy_lst (loop1);
  *after = copy_lst (loop1);

  lst_remove_all_before_including_pbb (*before, first, false);
  lst_remove_all_before_including_pbb (*after, last, true);

  lst_remove_all_before_excluding_pbb (*nest, first, true);
  lst_remove_all_before_excluding_pbb (*nest, last, false);

  if (lst_empty_p (*before))
    {
      free_lst (*before);
      *before = NULL;
    }
  if (lst_empty_p (*after))
    {
      free_lst (*after);
      *after = NULL;
    }
  if (lst_empty_p (*nest))
    {
      free_lst (*nest);
      *nest = NULL;
    }
}

/* Try to interchange LOOP1 with LOOP2 for all the statements of the
   body of LOOP2.  LOOP1 contains LOOP2.  Return true if it did the
   interchange.  */

static bool
lst_try_interchange_loops (scop_p scop, lst_p loop1, lst_p loop2)
{
  int depth1 = lst_depth (loop1);
  int depth2 = lst_depth (loop2);
  lst_p transformed;

  lst_p before = NULL, nest = NULL, after = NULL;

  if (!lst_perfectly_nested_p (loop1, loop2))
    lst_perfect_nestify (loop1, loop2, &before, &nest, &after);

  if (!lst_interchange_profitable_p (loop2, depth1, depth2))
    return false;

  lst_apply_interchange (loop2, depth1, depth2);

  /* Sync the transformed LST information and the PBB scatterings
     before using the scatterings in the data dependence analysis.  */
  if (before || nest || after)
    {
      transformed = lst_substitute_3 (SCOP_TRANSFORMED_SCHEDULE (scop), loop1,
				      before, nest, after);
      lst_update_scattering (transformed);
      free_lst (transformed);
    }

  if (graphite_legal_transform (scop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loops at depths %d and %d will be interchanged.\n",
		 depth1, depth2);

      /* Transform the SCOP_TRANSFORMED_SCHEDULE of the SCOP.  */
      lst_insert_in_sequence (before, loop1, true);
      lst_insert_in_sequence (after, loop1, false);

      if (nest)
	{
	  lst_replace (loop1, nest);
	  free_lst (loop1);
	}

      return true;
    }

  /* Undo the transform.  */
  free_lst (before);
  free_lst (nest);
  free_lst (after);
  lst_apply_interchange (loop2, depth2, depth1);
  return false;
}

/* Selects the inner loop in LST_SEQ (INNER_FATHER) to be interchanged
   with the loop OUTER in LST_SEQ (OUTER_FATHER).  */

static bool
lst_interchange_select_inner (scop_p scop, lst_p outer_father, int outer,
			      lst_p inner_father)
{
  int inner;
  lst_p loop1, loop2;

  gcc_assert (outer_father
	      && LST_LOOP_P (outer_father)
	      && LST_LOOP_P (VEC_index (lst_p, LST_SEQ (outer_father), outer))
	      && inner_father
	      && LST_LOOP_P (inner_father));

  loop1 = VEC_index (lst_p, LST_SEQ (outer_father), outer);

  FOR_EACH_VEC_ELT (lst_p, LST_SEQ (inner_father), inner, loop2)
    if (LST_LOOP_P (loop2)
	&& (lst_try_interchange_loops (scop, loop1, loop2)
	    || lst_interchange_select_inner (scop, outer_father, outer, loop2)))
      return true;

  return false;
}

/* Interchanges all the loops of LOOP and the loops of its body that
   are considered profitable to interchange.  Return the number of
   interchanged loops.  OUTER is the index in LST_SEQ (LOOP) that
   points to the next outer loop to be considered for interchange.  */

static int
lst_interchange_select_outer (scop_p scop, lst_p loop, int outer)
{
  lst_p l;
  int res = 0;
  int i = 0;
  lst_p father;

  if (!loop || !LST_LOOP_P (loop))
    return 0;

  father = LST_LOOP_FATHER (loop);
  if (father)
    {
      while (lst_interchange_select_inner (scop, father, outer, loop))
	{
	  res++;
	  loop = VEC_index (lst_p, LST_SEQ (father), outer);
	}
    }

  if (LST_LOOP_P (loop))
    FOR_EACH_VEC_ELT (lst_p, LST_SEQ (loop), i, l)
      if (LST_LOOP_P (l))
	res += lst_interchange_select_outer (scop, l, i);

  return res;
}

/* Interchanges all the loop depths that are considered profitable for
   SCOP.  Return the number of interchanged loops.  */

int
scop_do_interchange (scop_p scop)
{
  int res = lst_interchange_select_outer
    (scop, SCOP_TRANSFORMED_SCHEDULE (scop), 0);

  lst_update_scattering (SCOP_TRANSFORMED_SCHEDULE (scop));

  return res;
}


#endif

