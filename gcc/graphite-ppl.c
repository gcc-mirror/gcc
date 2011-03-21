/* Gimple Represented as Polyhedra.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com>
   and Tobias Grosser <grosser@fim.uni-passau.de>

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

#ifdef HAVE_cloog

#include "ppl_c.h"
#include "graphite-cloog-util.h"
#include "graphite-ppl.h"

/* Set the inhomogeneous term of E to X.  */

void
ppl_set_inhomogeneous_gmp (ppl_Linear_Expression_t e, mpz_t x)
{
  mpz_t v0, v1;
  ppl_Coefficient_t c;

  mpz_init (v0);
  mpz_init (v1);
  ppl_new_Coefficient (&c);

  ppl_Linear_Expression_inhomogeneous_term (e, c);
  ppl_Coefficient_to_mpz_t (c, v1);
  mpz_neg (v1, v1);
  mpz_set (v0, x);
  mpz_add (v0, v0, v1);
  ppl_assign_Coefficient_from_mpz_t (c, v0);
  ppl_Linear_Expression_add_to_inhomogeneous (e, c);

  mpz_clear (v0);
  mpz_clear (v1);
  ppl_delete_Coefficient (c);
}

/* Set E[I] to X.  */

void
ppl_set_coef_gmp (ppl_Linear_Expression_t e, ppl_dimension_type i, mpz_t x)
{
  mpz_t v0, v1;
  ppl_Coefficient_t c;

  mpz_init (v0);
  mpz_init (v1);
  ppl_new_Coefficient (&c);

  ppl_Linear_Expression_coefficient (e, i, c);
  ppl_Coefficient_to_mpz_t (c, v1);
  mpz_neg (v1, v1);
  mpz_set (v0, x);
  mpz_add (v0, v0, v1);
  ppl_assign_Coefficient_from_mpz_t (c, v0);
  ppl_Linear_Expression_add_to_coefficient (e, i, c);

  mpz_clear (v0);
  mpz_clear (v1);
  ppl_delete_Coefficient (c);
}

/* Insert after X NB_NEW_DIMS empty dimensions into PH.

   With x = 3 and nb_new_dims = 4

   |  d0 d1 d2 d3 d4

   is transformed to

   |  d0 d1 d2 x0 x1 x2 x3 d3 d4

   | map = {0, 1, 2, 7, 8, 3, 4, 5, 6}
*/

void
ppl_insert_dimensions_pointset (ppl_Pointset_Powerset_C_Polyhedron_t ph, int x,
				int nb_new_dims)
{
  ppl_dimension_type i, dim;
  ppl_dimension_type *map;
  ppl_dimension_type x_ppl, nb_new_dims_ppl;

  x_ppl = (ppl_dimension_type) x;
  nb_new_dims_ppl = (ppl_dimension_type) nb_new_dims;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (ph, &dim);
  ppl_Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_embed (ph, nb_new_dims);

  map = (ppl_dimension_type *) XNEWVEC (ppl_dimension_type, dim + nb_new_dims);

  for (i = 0; i < x_ppl; i++)
    map[i] = i;

  for (i = x_ppl; i < x_ppl + nb_new_dims_ppl; i++)
    map[dim + i - x_ppl] = i;

  for (i = x_ppl + nb_new_dims_ppl; i < dim + nb_new_dims_ppl; i++)
    map[i - nb_new_dims_ppl] = i;

  ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions (ph, map, dim + nb_new_dims);
  free (map);
}

/* Insert after X NB_NEW_DIMS empty dimensions into PH.

   With x = 3 and nb_new_dims = 4

   |  d0 d1 d2 d3 d4

   is transformed to

   |  d0 d1 d2 x0 x1 x2 x3 d3 d4

   | map = {0, 1, 2, 7, 8, 3, 4, 5, 6}
*/

void
ppl_insert_dimensions (ppl_Polyhedron_t ph, int x,
		       int nb_new_dims)
{
  ppl_dimension_type i, dim;
  ppl_dimension_type *map;
  ppl_dimension_type x_ppl, nb_new_dims_ppl;

  x_ppl = (ppl_dimension_type) x;
  nb_new_dims_ppl = (ppl_dimension_type) nb_new_dims;

  ppl_Polyhedron_space_dimension (ph, &dim);
  ppl_Polyhedron_add_space_dimensions_and_embed (ph, nb_new_dims);

  map = (ppl_dimension_type *) XNEWVEC (ppl_dimension_type, dim + nb_new_dims);

  for (i = 0; i < x_ppl; i++)
    map[i] = i;

  for (i = x_ppl; i < x_ppl + nb_new_dims_ppl; i++)
    map[dim + i - x_ppl] = i;

  for (i = x_ppl + nb_new_dims_ppl; i < dim + nb_new_dims_ppl; i++)
    map[i - nb_new_dims_ppl] = i;

  ppl_Polyhedron_map_space_dimensions (ph, map, dim + nb_new_dims);
  free (map);
}

/* Based on the original polyhedron PH, returns a new polyhedron with
   an extra dimension placed at position LOOP + 1 that slices the
   dimension LOOP into strips of size STRIDE.  */

ppl_Polyhedron_t
ppl_strip_loop (ppl_Polyhedron_t ph, ppl_dimension_type loop, int stride)
{
  ppl_const_Constraint_System_t pcs;
  ppl_Constraint_System_const_iterator_t cit, end;
  ppl_const_Constraint_t cstr;
  ppl_Linear_Expression_t expr;
  int v;
  ppl_dimension_type dim;
  ppl_Polyhedron_t res;
  ppl_Coefficient_t c;
  mpz_t val;

  mpz_init (val);
  ppl_new_Coefficient (&c);

  ppl_Polyhedron_space_dimension (ph, &dim);
  ppl_Polyhedron_get_constraints (ph, &pcs);

  /* Start from a copy of the constraints.  */
  ppl_new_C_Polyhedron_from_space_dimension (&res, dim + 1, 0);
  ppl_Polyhedron_add_constraints (res, pcs);

  /* Add an empty dimension for the strip loop.  */
  ppl_insert_dimensions (res, loop, 1);

  /* Identify the constraints that define the lower and upper bounds
     of the strip-mined loop, and add them to the strip loop.  */
  {
    ppl_Polyhedron_t tmp;

    ppl_new_C_Polyhedron_from_space_dimension (&tmp, dim + 1, 0);
    ppl_new_Constraint_System_const_iterator (&cit);
    ppl_new_Constraint_System_const_iterator (&end);

    for (ppl_Constraint_System_begin (pcs, cit),
	   ppl_Constraint_System_end (pcs, end);
	 !ppl_Constraint_System_const_iterator_equal_test (cit, end);
	 ppl_Constraint_System_const_iterator_increment (cit))
      {
	ppl_Constraint_System_const_iterator_dereference (cit, &cstr);
	ppl_new_Linear_Expression_from_Constraint (&expr, cstr);
	ppl_Linear_Expression_coefficient (expr, loop, c);
	ppl_delete_Linear_Expression (expr);
	ppl_Coefficient_to_mpz_t (c, val);
	v = mpz_get_si (val);

	if (0 < v || v < 0)
	  ppl_Polyhedron_add_constraint (tmp, cstr);
      }
    ppl_delete_Constraint_System_const_iterator (cit);
    ppl_delete_Constraint_System_const_iterator (end);

    ppl_insert_dimensions (tmp, loop + 1, 1);
    ppl_Polyhedron_get_constraints (tmp, &pcs);
    ppl_Polyhedron_add_constraints (res, pcs);
    ppl_delete_Polyhedron (tmp);
  }

  /* Lower bound of a tile starts at "stride * outer_iv".  */
  {
    ppl_Constraint_t new_cstr;
    ppl_new_Linear_Expression_with_dimension (&expr, dim + 1);

    ppl_set_coef (expr, loop + 1, 1);
    ppl_set_coef (expr, loop, -1 * stride);

    ppl_new_Constraint (&new_cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
    ppl_delete_Linear_Expression (expr);
    ppl_Polyhedron_add_constraint (res, new_cstr);
    ppl_delete_Constraint (new_cstr);
  }

  /* Upper bound of a tile stops at "stride * outer_iv + stride - 1",
     or at the old upper bound that is not modified.  */
  {
    ppl_Constraint_t new_cstr;
    ppl_new_Linear_Expression_with_dimension (&expr, dim + 1);

    ppl_set_coef (expr, loop + 1, -1);
    ppl_set_coef (expr, loop, stride);
    ppl_set_inhomogeneous (expr, stride - 1);

    ppl_new_Constraint (&new_cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
    ppl_delete_Linear_Expression (expr);
    ppl_Polyhedron_add_constraint (res, new_cstr);
    ppl_delete_Constraint (new_cstr);
  }

  mpz_clear (val);
  ppl_delete_Coefficient (c);
  return res;
}

/* Lexicographically compares two linear expressions A and B and
   returns negative when A < B, 0 when A == B and positive when A > B.  */

int
ppl_lexico_compare_linear_expressions (ppl_Linear_Expression_t a,
				       ppl_Linear_Expression_t b)
{
  ppl_dimension_type min_length, length1, length2;
  ppl_dimension_type i;
  ppl_Coefficient_t c;
  int res;
  mpz_t va, vb;

  ppl_Linear_Expression_space_dimension (a, &length1);
  ppl_Linear_Expression_space_dimension (b, &length2);
  ppl_new_Coefficient (&c);
  mpz_init (va);
  mpz_init (vb);

  if (length1 < length2)
    min_length = length1;
  else
    min_length = length2;

  for (i = 0; i < min_length; i++)
    {
      ppl_Linear_Expression_coefficient (a, i, c);
      ppl_Coefficient_to_mpz_t (c, va);
      ppl_Linear_Expression_coefficient (b, i, c);
      ppl_Coefficient_to_mpz_t (c, vb);
      res = mpz_cmp (va, vb);

      if (res == 0)
	continue;

      mpz_clear (va);
      mpz_clear (vb);
      ppl_delete_Coefficient (c);
      return res;
    }

  mpz_clear (va);
  mpz_clear (vb);
  ppl_delete_Coefficient (c);
  return length1 - length2;
}

/* Print to FILE the polyhedron PH under its PolyLib matrix form.  */

void
ppl_print_polyhedron_matrix (FILE *file, ppl_const_Polyhedron_t ph)
{
  CloogMatrix *mat = new_Cloog_Matrix_from_ppl_Polyhedron (ph);
  cloog_matrix_print (file, mat);
  cloog_matrix_free (mat);
}

/* Print to FILE the linear expression LE.  */

void
ppl_print_linear_expr (FILE *file, ppl_Linear_Expression_t le)
{
  ppl_Constraint_t c;
  ppl_Polyhedron_t pol;
  ppl_dimension_type dim;

  ppl_Linear_Expression_space_dimension (le, &dim);
  ppl_new_C_Polyhedron_from_space_dimension (&pol, dim, 0);
  ppl_new_Constraint (&c, le, PPL_CONSTRAINT_TYPE_EQUAL);
  ppl_Polyhedron_add_constraint (pol, c);
  ppl_print_polyhedron_matrix (file, pol);
}

/* Print to STDERR the linear expression LE.  */

DEBUG_FUNCTION void
debug_ppl_linear_expr (ppl_Linear_Expression_t le)
{
  ppl_print_linear_expr (stderr, le);
}

/* Print to FILE the powerset PS in its PolyLib matrix form.  */

void
ppl_print_powerset_matrix (FILE *file,
			   ppl_Pointset_Powerset_C_Polyhedron_t ps)
{
  size_t nb_disjuncts;
  ppl_Pointset_Powerset_C_Polyhedron_iterator_t it, end;

  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&it);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&end);

  ppl_Pointset_Powerset_C_Polyhedron_size (ps, &nb_disjuncts);
  fprintf (file, "%d\n", (int) nb_disjuncts);

  for (ppl_Pointset_Powerset_C_Polyhedron_iterator_begin (ps, it),
       ppl_Pointset_Powerset_C_Polyhedron_iterator_end (ps, end);
       !ppl_Pointset_Powerset_C_Polyhedron_iterator_equal_test (it, end);
       ppl_Pointset_Powerset_C_Polyhedron_iterator_increment (it))
    {
      ppl_const_Polyhedron_t ph;

      ppl_Pointset_Powerset_C_Polyhedron_iterator_dereference (it, &ph);
      ppl_print_polyhedron_matrix (file, ph);
    }

  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (it);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (end);
}

/* Print to STDERR the polyhedron PH under its PolyLib matrix form.  */

DEBUG_FUNCTION void
debug_ppl_polyhedron_matrix (ppl_Polyhedron_t ph)
{
  ppl_print_polyhedron_matrix (stderr, ph);
}

/* Print to STDERR the powerset PS in its PolyLib matrix form.  */

DEBUG_FUNCTION void
debug_ppl_powerset_matrix (ppl_Pointset_Powerset_C_Polyhedron_t ps)
{
  ppl_print_powerset_matrix (stderr, ps);
}

/* Read from FILE a polyhedron under PolyLib matrix form and return a
   PPL polyhedron object.  */

void
ppl_read_polyhedron_matrix (ppl_Polyhedron_t *ph, FILE *file)
{
  CloogMatrix *mat = cloog_matrix_read (file);
  new_C_Polyhedron_from_Cloog_Matrix (ph, mat);
  cloog_matrix_free (mat);
}

/* Return in RES the maximum of the linear expression LE on the
   pointset powerset of polyhedra PS.  */

void
ppl_max_for_le_pointset (ppl_Pointset_Powerset_C_Polyhedron_t ps,
                         ppl_Linear_Expression_t le, mpz_t res)
{
  ppl_Coefficient_t num, denom;
  mpz_t dv, nv;
  int maximum, err;

  mpz_init (nv);
  mpz_init (dv);
  ppl_new_Coefficient (&num);
  ppl_new_Coefficient (&denom);
  err = ppl_Pointset_Powerset_C_Polyhedron_maximize (ps, le, num, denom, &maximum);

  if (err > 0)
    {
      ppl_Coefficient_to_mpz_t (num, nv);
      ppl_Coefficient_to_mpz_t (denom, dv);
      gcc_assert (mpz_sgn (dv) != 0);
      mpz_tdiv_q (res, nv, dv);
    }

  mpz_clear (nv);
  mpz_clear (dv);
  ppl_delete_Coefficient (num);
  ppl_delete_Coefficient (denom);
}

/* Return in RES the maximum of the linear expression LE on the
   polyhedron POL.  */

void
ppl_min_for_le_pointset (ppl_Pointset_Powerset_C_Polyhedron_t ps,
			 ppl_Linear_Expression_t le, mpz_t res)
{
  ppl_Coefficient_t num, denom;
  mpz_t dv, nv;
  int minimum, err;

  mpz_init (nv);
  mpz_init (dv);
  ppl_new_Coefficient (&num);
  ppl_new_Coefficient (&denom);
  err = ppl_Pointset_Powerset_C_Polyhedron_minimize (ps, le, num, denom, &minimum);

  if (err > 0)
    {
      ppl_Coefficient_to_mpz_t (num, nv);
      ppl_Coefficient_to_mpz_t (denom, dv);
      gcc_assert (mpz_sgn (dv) != 0);
      mpz_tdiv_q (res, nv, dv);
    }

  mpz_clear (nv);
  mpz_clear (dv);
  ppl_delete_Coefficient (num);
  ppl_delete_Coefficient (denom);
}

/* Builds a constraint in dimension DIM relating dimensions POS1 to
   POS2 as "POS1 - POS2 + C CSTR_TYPE 0" */

ppl_Constraint_t
ppl_build_relation (int dim, int pos1, int pos2, int c,
		    enum ppl_enum_Constraint_Type cstr_type)
{
  ppl_Linear_Expression_t expr;
  ppl_Constraint_t cstr;
  ppl_Coefficient_t coef;
  mpz_t v, v_op, v_c;

  mpz_init (v);
  mpz_init (v_op);
  mpz_init (v_c);

  mpz_set_si (v, 1);
  mpz_set_si (v_op, -1);
  mpz_set_si (v_c, c);

  ppl_new_Coefficient (&coef);
  ppl_new_Linear_Expression_with_dimension (&expr, dim);

  ppl_assign_Coefficient_from_mpz_t (coef, v);
  ppl_Linear_Expression_add_to_coefficient (expr, pos1, coef);
  ppl_assign_Coefficient_from_mpz_t (coef, v_op);
  ppl_Linear_Expression_add_to_coefficient (expr, pos2, coef);
  ppl_assign_Coefficient_from_mpz_t (coef, v_c);
  ppl_Linear_Expression_add_to_inhomogeneous (expr, coef);

  ppl_new_Constraint (&cstr, expr, cstr_type);

  ppl_delete_Linear_Expression (expr);
  ppl_delete_Coefficient (coef);
  mpz_clear (v);
  mpz_clear (v_op);
  mpz_clear (v_c);

  return cstr;
}

/* Print to STDERR the GMP value VAL.  */

DEBUG_FUNCTION void
debug_gmp_value (mpz_t val)
{
  char *str = mpz_get_str (0, 10, val);
  void (*gmp_free) (void *, size_t);

  fprintf (stderr, "%s", str);
  mp_get_memory_functions (NULL, NULL, &gmp_free);
  (*gmp_free) (str, strlen (str) + 1);
}

/* Checks for integer feasibility: returns true when the powerset
   polyhedron PS has no integer solutions.  */

bool
ppl_powerset_is_empty (ppl_Pointset_Powerset_C_Polyhedron_t ps)
{
  ppl_PIP_Problem_t pip;
  ppl_dimension_type d;
  ppl_const_Constraint_System_t pcs;
  ppl_Constraint_System_const_iterator_t first, last;
  ppl_Pointset_Powerset_C_Polyhedron_iterator_t it, end;
  bool has_integer_solutions = false;

  if (ppl_Pointset_Powerset_C_Polyhedron_is_empty (ps))
    return true;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (ps, &d);
  ppl_new_Constraint_System_const_iterator (&first);
  ppl_new_Constraint_System_const_iterator (&last);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&it);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&end);

  for (ppl_Pointset_Powerset_C_Polyhedron_iterator_begin (ps, it),
       ppl_Pointset_Powerset_C_Polyhedron_iterator_end (ps, end);
       !ppl_Pointset_Powerset_C_Polyhedron_iterator_equal_test (it, end);
       ppl_Pointset_Powerset_C_Polyhedron_iterator_increment (it))
    {
      ppl_const_Polyhedron_t ph;
      ppl_Pointset_Powerset_C_Polyhedron_iterator_dereference (it, &ph);

      ppl_Polyhedron_get_constraints (ph, &pcs);
      ppl_Constraint_System_begin (pcs, first);
      ppl_Constraint_System_end (pcs, last);

      ppl_new_PIP_Problem_from_constraints (&pip, d, first, last, 0, NULL);
      has_integer_solutions |= ppl_PIP_Problem_is_satisfiable (pip);

      ppl_delete_PIP_Problem (pip);
    }

  ppl_delete_Constraint_System_const_iterator (first);
  ppl_delete_Constraint_System_const_iterator (last);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (it);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (end);

  return !has_integer_solutions;
}

#endif
