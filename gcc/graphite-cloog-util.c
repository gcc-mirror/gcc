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

#include "config.h"
#include "system.h"
#include "coretypes.h"

#ifdef HAVE_cloog

#include "ppl_c.h"
#include "cloog/cloog.h"
#include "graphite-cloog-util.h"
#include "graphite-cloog-compat.h"

/* Counts the number of constraints in PCS.  */

static int
ppl_Constrain_System_number_of_constraints (ppl_const_Constraint_System_t pcs)
{
  ppl_Constraint_System_const_iterator_t cit, end;
  int num = 0;

  ppl_new_Constraint_System_const_iterator (&cit);
  ppl_new_Constraint_System_const_iterator (&end);

  for (ppl_Constraint_System_begin (pcs, cit),
       ppl_Constraint_System_end (pcs, end);
       !ppl_Constraint_System_const_iterator_equal_test (cit, end);
       ppl_Constraint_System_const_iterator_increment (cit))
    num++;

  ppl_delete_Constraint_System_const_iterator (cit);
  ppl_delete_Constraint_System_const_iterator (end);
  return num;
}

static void
oppose_constraint (CloogMatrix *m, int row)
{
  int k;

  /* Do not oppose the first column: it is the eq/ineq one.  */
  /* Cast needed to remove warning that is generated as CLooG isl
     is using an unsigned int for NbColumns and CLooG PPL is
     using a signed int for NBColumns.  */
  for (k = 1; k < (int)m->NbColumns; k++)
    mpz_neg (m->p[row][k], m->p[row][k]);
}

/* Inserts constraint CSTR at row ROW of matrix M.  */

static void
insert_constraint_into_matrix (CloogMatrix *m, int row,
			       ppl_const_Constraint_t cstr)
{
  ppl_Coefficient_t c;
  ppl_dimension_type i, dim, nb_cols = m->NbColumns;

  ppl_Constraint_space_dimension (cstr, &dim);
  ppl_new_Coefficient (&c);

  for (i = 0; i < dim; i++)
    {
      ppl_Constraint_coefficient (cstr, i, c);
      ppl_Coefficient_to_mpz_t (c, m->p[row][i + 1]);
    }

  for (i = dim; i < nb_cols - 1; i++)
    mpz_set_si (m->p[row][i + 1], 0);

  ppl_Constraint_inhomogeneous_term  (cstr, c);
  ppl_Coefficient_to_mpz_t (c, m->p[row][nb_cols - 1]);
  mpz_set_si (m->p[row][0], 1);

  switch (ppl_Constraint_type (cstr))
    {
    case PPL_CONSTRAINT_TYPE_LESS_THAN:
      oppose_constraint (m, row);
    case PPL_CONSTRAINT_TYPE_GREATER_THAN:
      mpz_sub_ui (m->p[row][nb_cols - 1],
		     m->p[row][nb_cols - 1], 1);
      break;

    case PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL:
      oppose_constraint (m, row);
    case PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL:
      break;

    case PPL_CONSTRAINT_TYPE_EQUAL:
      mpz_set_si (m->p[row][0], 0);
      break;

    default:
      /* Not yet implemented.  */
      gcc_unreachable();
    }

  ppl_delete_Coefficient (c);
}

/* Creates a CloogMatrix from constraint system PCS.  */

static CloogMatrix *
new_Cloog_Matrix_from_ppl_Constraint_System (ppl_const_Constraint_System_t pcs)
{
  CloogMatrix *matrix;
  ppl_Constraint_System_const_iterator_t cit, end;
  ppl_dimension_type dim;
  int rows;
  int row = 0;

  rows = ppl_Constrain_System_number_of_constraints (pcs);
  ppl_Constraint_System_space_dimension (pcs, &dim);
  matrix = cloog_matrix_alloc (rows, dim + 2);
  ppl_new_Constraint_System_const_iterator (&cit);
  ppl_new_Constraint_System_const_iterator (&end);

  for (ppl_Constraint_System_begin (pcs, cit),
       ppl_Constraint_System_end (pcs, end);
       !ppl_Constraint_System_const_iterator_equal_test (cit, end);
       ppl_Constraint_System_const_iterator_increment (cit))
    {
      ppl_const_Constraint_t c;
      ppl_Constraint_System_const_iterator_dereference (cit, &c);
      insert_constraint_into_matrix (matrix, row, c);
      row++;
    }

  ppl_delete_Constraint_System_const_iterator (cit);
  ppl_delete_Constraint_System_const_iterator (end);

  return matrix;
}

/* Creates a CloogMatrix from polyhedron PH.  */

CloogMatrix *
new_Cloog_Matrix_from_ppl_Polyhedron (ppl_const_Polyhedron_t ph)
{
  ppl_const_Constraint_System_t pcs;
  CloogMatrix *res;

  ppl_Polyhedron_get_constraints (ph, &pcs);
  res = new_Cloog_Matrix_from_ppl_Constraint_System (pcs);

  return res;
}

/* Translates row ROW of the CloogMatrix MATRIX to a PPL Constraint.  */

static ppl_Constraint_t
cloog_matrix_to_ppl_constraint (CloogMatrix *matrix, int row)
{
  int j;
  ppl_Constraint_t cstr;
  ppl_Coefficient_t coef;
  ppl_Linear_Expression_t expr;
  ppl_dimension_type dim = matrix->NbColumns - 2;

  ppl_new_Coefficient (&coef);
  ppl_new_Linear_Expression_with_dimension (&expr, dim);

  /* Cast needed to remove warning that is generated as CLooG isl
     is using an unsigned int for NbColumns and CLooG PPL is
     using a signed int for NBColumns.  */
  for (j = 1; j < (int)matrix->NbColumns - 1; j++)
    {
      ppl_assign_Coefficient_from_mpz_t (coef, matrix->p[row][j]);
      ppl_Linear_Expression_add_to_coefficient (expr, j - 1, coef);
    }

  ppl_assign_Coefficient_from_mpz_t (coef,
				     matrix->p[row][matrix->NbColumns - 1]);
  ppl_Linear_Expression_add_to_inhomogeneous (expr, coef);
  ppl_delete_Coefficient (coef);

  if (mpz_sgn (matrix->p[row][0]) == 0)
    ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_EQUAL);
  else
    ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);

  ppl_delete_Linear_Expression (expr);
  return cstr;
}

/* Creates a PPL constraint system from MATRIX.  */

static void
new_Constraint_System_from_Cloog_Matrix (ppl_Constraint_System_t *pcs,
					 CloogMatrix *matrix)
{
  int i;

  ppl_new_Constraint_System (pcs);

  /* Cast needed to remove warning that is generated as CLooG isl
     is using an unsigned int for NbColumns and CLooG PPL is
     using a signed int for NBColumns.  */
  for (i = 0; i < (int)matrix->NbRows; i++)
    {
      ppl_Constraint_t c = cloog_matrix_to_ppl_constraint (matrix, i);
      ppl_Constraint_System_insert_Constraint (*pcs, c);
      ppl_delete_Constraint (c);
    }
}

/* Creates a PPL Polyhedron from MATRIX.  */

void
new_C_Polyhedron_from_Cloog_Matrix (ppl_Polyhedron_t *ph,
				      CloogMatrix *matrix)
{
  ppl_Constraint_System_t cs;
  new_Constraint_System_from_Cloog_Matrix (&cs, matrix);
  ppl_new_C_Polyhedron_recycle_Constraint_System (ph, cs);
}

/* Creates a CloogDomain from polyhedron PH.  */

CloogDomain *
new_Cloog_Domain_from_ppl_Polyhedron (ppl_const_Polyhedron_t ph, int nb_params,
                                      CloogState *state ATTRIBUTE_UNUSED)
{
  CloogMatrix *mat = new_Cloog_Matrix_from_ppl_Polyhedron (ph);
  CloogDomain *res = cloog_domain_from_cloog_matrix (state, mat, nb_params);
  cloog_matrix_free (mat);
  return res;
}

/* Create a CloogScattering from polyhedron PH.  */

CloogScattering *
new_Cloog_Scattering_from_ppl_Polyhedron (ppl_const_Polyhedron_t ph,
                                          int nb_params ATTRIBUTE_UNUSED,
                                          int nb_scatt ATTRIBUTE_UNUSED,
                                          CloogState *state ATTRIBUTE_UNUSED)
{
#ifdef CLOOG_ORG
  CloogMatrix *mat = new_Cloog_Matrix_from_ppl_Polyhedron (ph);
  CloogScattering *res = cloog_scattering_from_cloog_matrix (state, mat,
                                                             nb_scatt,
                                                             nb_params);

  cloog_matrix_free (mat);
  return res;
#else
  return new_Cloog_Domain_from_ppl_Polyhedron (ph, nb_params, state);
#endif
}

/* Creates a CloogDomain from a pointset powerset PS.  */

CloogDomain *
new_Cloog_Domain_from_ppl_Pointset_Powerset
  (ppl_Pointset_Powerset_C_Polyhedron_t ps, int nb_params,
   CloogState *state ATTRIBUTE_UNUSED)
{
  CloogDomain *res = NULL;
  ppl_Pointset_Powerset_C_Polyhedron_iterator_t it, end;

  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&it);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&end);

  for (ppl_Pointset_Powerset_C_Polyhedron_iterator_begin (ps, it),
       ppl_Pointset_Powerset_C_Polyhedron_iterator_end (ps, end);
       !ppl_Pointset_Powerset_C_Polyhedron_iterator_equal_test (it, end);
       ppl_Pointset_Powerset_C_Polyhedron_iterator_increment (it))
    {
      ppl_const_Polyhedron_t ph;
      CloogDomain *tmp;

      ppl_Pointset_Powerset_C_Polyhedron_iterator_dereference (it, &ph);
      tmp = new_Cloog_Domain_from_ppl_Polyhedron (ph, nb_params, state);

      if (res == NULL)
	res = tmp;
      else
	res = cloog_domain_union (res, tmp);
    }

  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (it);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (end);

  gcc_assert (res != NULL);

  return res;
}

/* Print to FILE the matrix MAT in OpenScop format.  OUTPUT is the number
   of output dimensions, INPUT is the number of input dimensions, LOCALS
   is the number of existentially quantified variables and PARAMS is the
   number of parameters.  */

static void
openscop_print_cloog_matrix (FILE *file, CloogMatrix *mat,
			     int output, int input, int locals,
			     int params)
{
  int i, j;

  fprintf (file, "%d %d %d %d %d %d \n", cloog_matrix_nrows (mat),
	   cloog_matrix_ncolumns (mat), output, input, locals, params);

  for (i = 0; i < cloog_matrix_nrows (mat); i++)
    {
      for (j = 0; j < cloog_matrix_ncolumns (mat); j++)
        if (j == 0)
	  fprintf (file, "%ld ", mpz_get_si (mat->p[i][j]));
        else
	  fprintf (file, "%6ld ", mpz_get_si (mat->p[i][j]));

      fprintf (file, "\n");
    }
}

/* Print to FILE the polyhedron PH in OpenScop format.  OUTPUT is the number
   of output dimensions, INPUT is the number of input dimensions, LOCALS is
   the number of existentially quantified variables and PARAMS is the number
   of parameters.  */

void
openscop_print_polyhedron_matrix (FILE *file, ppl_const_Polyhedron_t ph,
				  int output, int input, int locals,
				  int params)
{
  CloogMatrix *mat = new_Cloog_Matrix_from_ppl_Polyhedron (ph);
  openscop_print_cloog_matrix (file, mat, output, input, locals, params);
  cloog_matrix_free (mat);
}

/* Read from FILE a matrix in OpenScop format.  OUTPUT is the number of
   output dimensions, INPUT is the number of input dimensions, LOCALS
   is the number of existentially quantified variables and PARAMS is the
   number of parameters.  */

static CloogMatrix *
openscop_read_cloog_matrix (FILE *file, int *output, int *input, int *locals,
			    int *params)
{
  int nb_rows, nb_cols, i, j;
  CloogMatrix *mat;
  int *openscop_matrix_header, *matrix_line;

  openscop_matrix_header = openscop_read_N_int (file, 6);

  nb_rows = openscop_matrix_header[0];
  nb_cols = openscop_matrix_header[1];
  *output = openscop_matrix_header[2];
  *input = openscop_matrix_header[3];
  *locals = openscop_matrix_header[4];
  *params = openscop_matrix_header[5];

  free (openscop_matrix_header);

  if (nb_rows == 0 || nb_cols == 0)
    return NULL;

  mat = cloog_matrix_alloc (nb_rows, nb_cols);
  mat->NbRows = nb_rows;
  mat->NbColumns = nb_cols;

  for (i = 0; i < nb_rows; i++)
    {
      matrix_line = openscop_read_N_int (file, nb_cols);

      for (j = 0; j < nb_cols; j++)
        mpz_set_si (mat->p[i][j], matrix_line[j]);
    }

  return mat;
}

/* Read from FILE the polyhedron PH in OpenScop format.  OUTPUT is the number
   of output dimensions, INPUT is the number of input dimensions, LOCALS is
   the number of existentially quantified variables and PARAMS is the number
   of parameters.  */

void
openscop_read_polyhedron_matrix (FILE *file, ppl_Polyhedron_t *ph,
				 int *output, int *input, int *locals,
				 int *params)
{
  CloogMatrix *mat;

  mat = openscop_read_cloog_matrix (file, output, input, locals, params);

  if (!mat)
    *ph = NULL;
  else
    {
      new_C_Polyhedron_from_Cloog_Matrix (ph, mat);
      cloog_matrix_free (mat);
    }
}

#endif
