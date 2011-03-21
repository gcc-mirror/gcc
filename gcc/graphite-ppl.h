/* Gimple Represented as Polyhedra.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@inria.fr>
   and Tobias Grosser <grosser@fim.uni-passau.de>.

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
#ifndef GCC_GRAPHITE_PPL_H
#define GCC_GRAPHITE_PPL_H

#include "double-int.h"
#include "tree.h"

ppl_Polyhedron_t ppl_strip_loop (ppl_Polyhedron_t, ppl_dimension_type, int);
int ppl_lexico_compare_linear_expressions (ppl_Linear_Expression_t,
					   ppl_Linear_Expression_t);

void ppl_print_polyhedron_matrix (FILE *, ppl_const_Polyhedron_t);
void ppl_print_powerset_matrix (FILE *, ppl_Pointset_Powerset_C_Polyhedron_t);
void debug_ppl_polyhedron_matrix (ppl_Polyhedron_t);
void debug_ppl_powerset_matrix (ppl_Pointset_Powerset_C_Polyhedron_t);
void ppl_print_linear_expr (FILE *, ppl_Linear_Expression_t);
void debug_ppl_linear_expr (ppl_Linear_Expression_t);
void ppl_read_polyhedron_matrix (ppl_Polyhedron_t *, FILE *);
void ppl_insert_dimensions (ppl_Polyhedron_t, int, int);
void ppl_insert_dimensions_pointset (ppl_Pointset_Powerset_C_Polyhedron_t, int,
				     int);
void ppl_set_inhomogeneous_gmp (ppl_Linear_Expression_t, mpz_t);
void ppl_set_coef_gmp (ppl_Linear_Expression_t, ppl_dimension_type, mpz_t);
void ppl_max_for_le_pointset (ppl_Pointset_Powerset_C_Polyhedron_t,
                              ppl_Linear_Expression_t, mpz_t);
void ppl_min_for_le_pointset (ppl_Pointset_Powerset_C_Polyhedron_t,
			      ppl_Linear_Expression_t, mpz_t);
ppl_Constraint_t ppl_build_relation (int, int, int, int,
				     enum ppl_enum_Constraint_Type);
void debug_gmp_value (mpz_t);
bool ppl_powerset_is_empty (ppl_Pointset_Powerset_C_Polyhedron_t);


/* Assigns to RES the value of the INTEGER_CST T.  */

static inline void
tree_int_to_gmp (tree t, mpz_t res)
{
  double_int di = tree_to_double_int (t);
  mpz_set_double_int (res, di, TYPE_UNSIGNED (TREE_TYPE (t)));
}

/* Converts a GMP constant VAL to a tree and returns it.  */

static inline tree
gmp_cst_to_tree (tree type, mpz_t val)
{
  tree t = type ? type : integer_type_node;
  mpz_t tmp;
  double_int di;

  mpz_init (tmp);
  mpz_set (tmp, val);
  di = mpz_get_double_int (t, tmp, true);
  mpz_clear (tmp);

  return double_int_to_tree (t, di);
}

/* Set the inhomogeneous term of E to the integer X.  */

static inline void
ppl_set_inhomogeneous (ppl_Linear_Expression_t e, int x)
{
  mpz_t v;
  mpz_init (v);
  mpz_set_si (v, x);
  ppl_set_inhomogeneous_gmp (e, v);
  mpz_clear (v);
}

/* Set the inhomogeneous term of E to the tree X.  */

static inline void
ppl_set_inhomogeneous_tree (ppl_Linear_Expression_t e, tree x)
{
  mpz_t v;
  mpz_init (v);
  tree_int_to_gmp (x, v);
  ppl_set_inhomogeneous_gmp (e, v);
  mpz_clear (v);
}

/* Set E[I] to integer X.  */

static inline void
ppl_set_coef (ppl_Linear_Expression_t e, ppl_dimension_type i, int x)
{
  mpz_t v;
  mpz_init (v);
  mpz_set_si (v, x);
  ppl_set_coef_gmp (e, i, v);
  mpz_clear (v);
}

/* Set E[I] to tree X.  */

static inline void
ppl_set_coef_tree (ppl_Linear_Expression_t e, ppl_dimension_type i, tree x)
{
  mpz_t v;
  mpz_init (v);
  tree_int_to_gmp (x, v);
  ppl_set_coef_gmp (e, i, v);
  mpz_clear (v);
}

/* Sets RES to the max of V1 and V2.  */

static inline void
value_max (mpz_t res, mpz_t v1, mpz_t v2)
{
  if (mpz_cmp (v1, v2) < 0)
    mpz_set (res, v2);
  mpz_set (res, v1);
}

/* Builds a new identity map for dimension DIM.  */

static inline ppl_dimension_type *
ppl_new_id_map (ppl_dimension_type dim)
{
  ppl_dimension_type *map, i;

  map = (ppl_dimension_type *) XNEWVEC (ppl_dimension_type, dim);

  for (i = 0; i < dim; i++)
    map[i] = i;

  return map;
}

/* Builds an interchange of dimensions A and B in MAP.  */

static inline void
ppl_interchange (ppl_dimension_type *map,
		 ppl_dimension_type a,
		 ppl_dimension_type b)
{
  map[a] = b;
  map[b] = a;
}

#endif

