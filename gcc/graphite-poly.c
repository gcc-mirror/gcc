/* Graphite polyhedral representation.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Tobias Grosser <grosser@fim.uni-passau.de>.

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
#include "diagnostic-core.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "gimple-pretty-print.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "ppl_c.h"
#include "graphite-ppl.h"
#include "graphite-poly.h"
#include "graphite-dependences.h"
#include "graphite-cloog-util.h"

#define OPENSCOP_MAX_STRING 256

/* Return the maximal loop depth in SCOP.  */

int
scop_max_loop_depth (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int max_nb_loops = 0;

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    {
      int nb_loops = pbb_dim_iter_domain (pbb);
      if (max_nb_loops < nb_loops)
        max_nb_loops = nb_loops;
    }

  return max_nb_loops;
}

/* Extend the scattering matrix of PBB to MAX_SCATTERING scattering
   dimensions.  */

static void
extend_scattering (poly_bb_p pbb, int max_scattering)
{
  ppl_dimension_type nb_old_dims, nb_new_dims;
  int nb_added_dims, i;
  ppl_Coefficient_t coef;
  mpz_t one;

  nb_added_dims = max_scattering - pbb_nb_scattering_transform (pbb);
  mpz_init (one);
  mpz_set_si (one, 1);
  ppl_new_Coefficient (&coef);
  ppl_assign_Coefficient_from_mpz_t (coef, one);

  gcc_assert (nb_added_dims >= 0);

  nb_old_dims = pbb_nb_scattering_transform (pbb) + pbb_dim_iter_domain (pbb)
    + scop_nb_params (PBB_SCOP (pbb));
  nb_new_dims = nb_old_dims + nb_added_dims;

  ppl_insert_dimensions (PBB_TRANSFORMED_SCATTERING (pbb),
			 pbb_nb_scattering_transform (pbb), nb_added_dims);
  PBB_NB_SCATTERING_TRANSFORM (pbb) += nb_added_dims;

  /* Add identity matrix for the added dimensions.  */
  for (i = max_scattering - nb_added_dims; i < max_scattering; i++)
    {
      ppl_Constraint_t cstr;
      ppl_Linear_Expression_t expr;

      ppl_new_Linear_Expression_with_dimension (&expr, nb_new_dims);
      ppl_Linear_Expression_add_to_coefficient (expr, i, coef);
      ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_Polyhedron_add_constraint (PBB_TRANSFORMED_SCATTERING (pbb), cstr);
      ppl_delete_Constraint (cstr);
      ppl_delete_Linear_Expression (expr);
    }

  ppl_delete_Coefficient (coef);
  mpz_clear (one);
}

/* All scattering matrices in SCOP will have the same number of scattering
   dimensions.  */

int
unify_scattering_dimensions (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  graphite_dim_t max_scattering = 0;

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    max_scattering = MAX (pbb_nb_scattering_transform (pbb), max_scattering);

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    extend_scattering (pbb, max_scattering);

  return max_scattering;
}

/* Print to FILE the pdr PH in OpenScop format.  NB_SUBSCRIPTS is the number
   of subscripts in PH, ALIAS_SET_DIM is the dimension of the alias set and
   NB_PARAMS is the number of parameters in PH.  */

static void
openscop_print_pdr_polyhedron (FILE *file, ppl_const_Polyhedron_t ph,
			       int nb_subscripts, int alias_set_dimension,
			       int nb_params)
{
  int input, locals, output;
  ppl_dimension_type alias_set_dim = (ppl_dimension_type) alias_set_dimension;
  ppl_dimension_type sub_dim_last = alias_set_dim + nb_subscripts;
  ppl_dimension_type *map, i, ph_space_dim = sub_dim_last + 1;
  ppl_Polyhedron_t pph;

  ppl_new_C_Polyhedron_from_C_Polyhedron (&pph, ph);

  map = (ppl_dimension_type *) XNEWVEC (ppl_dimension_type, ph_space_dim);

  for (i = 0; i < alias_set_dim - 1; i++)
    map[i] = nb_subscripts + 1 + i;

  for (i = alias_set_dim - 1; i < sub_dim_last; i++)
    map[i] = i - alias_set_dim + 1;

  ppl_Polyhedron_map_space_dimensions (pph, map, ph_space_dim - 1);

  locals = 0;
  input = alias_set_dim - nb_params - 1;

  /* According to OpenScop specification, the alias set column is a part of
     the output columns.  */
  output = nb_subscripts + 1;

  openscop_print_polyhedron_matrix (file, pph, output, input, locals, nb_params);
}

/* Print to FILE the powerset PDR.  NB_SUBSCRIPTS is the number of subscripts
   in PDR, ALIAS_SET_DIM is the dimension of the alias set in PDR and
   NB_PARAMS is the number of parameters in PDR.  */

static void
openscop_print_pdr_powerset (FILE *file,
			     ppl_Pointset_Powerset_C_Polyhedron_t ps,
			     int nb_subscripts,
			     int alias_set_dim,
			     int nb_params)
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
      openscop_print_pdr_polyhedron (file, ph, nb_subscripts, alias_set_dim,
				     nb_params);
    }

  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (it);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (end);
}

/* Print to FILE the powerset PS in its OpenScop matrix form.  */

static void
openscop_print_powerset_matrix (FILE *file,
				ppl_Pointset_Powerset_C_Polyhedron_t ps,
				int output, int input, int locals,
				int params)
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
      openscop_print_polyhedron_matrix (file, ph, output, input, locals,
				        params);
    }

  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (it);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (end);
}

/* Prints to FILE the scattering function of PBB in OpenScop format, at some
   VERBOSITY level.  */

static void
openscop_print_scattering_function_1 (FILE *file, poly_bb_p pbb, int verbosity)
{
  graphite_dim_t i;
  ppl_const_Polyhedron_t ph;

  if (verbosity > 0)
    {
      fprintf (file, "# scattering bb_%d (\n", pbb_index (pbb));
      fprintf (file, "#eq");

      for (i = 0; i < pbb_nb_scattering_transform (pbb); i++)
	fprintf (file, "     s%d", (int) i);

      for (i = 0; i < pbb_nb_local_vars (pbb); i++)
	fprintf (file, "    lv%d", (int) i);

      for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
	fprintf (file, "     i%d", (int) i);

      for (i = 0; i < pbb_nb_params (pbb); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  /* Number of disjunct components.  Remove this when
     PBB_TRANSFORMED_SCATTERING will be a pointset_powerset.  */
  fprintf (file, "1\n");

  ph = PBB_TRANSFORMED_SCATTERING (pbb)
    ? PBB_TRANSFORMED_SCATTERING (pbb)
    : PBB_ORIGINAL_SCATTERING (pbb);

  openscop_print_polyhedron_matrix (file, ph,
				    pbb_nb_scattering_transform (pbb),
				    pbb_dim_iter_domain (pbb),
				    pbb_nb_local_vars (pbb),
				    pbb_nb_params (pbb));

  if (verbosity > 0)
    fprintf (file, "#)\n");
}

/* Prints to FILE the scattering function of PBB, at some VERBOSITY
   level.  */

static void
print_scattering_function_1 (FILE *file, poly_bb_p pbb, int verbosity)
{
  graphite_dim_t i;

  if (verbosity > 0)
    {
      fprintf (file, "# scattering bb_%d (\n", pbb_index (pbb));
      fprintf (file, "#eq");

      for (i = 0; i < pbb_nb_scattering_transform (pbb); i++)
	fprintf (file, "     s%d", (int) i);

      for (i = 0; i < pbb_nb_local_vars (pbb); i++)
	fprintf (file, "    lv%d", (int) i);

      for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
	fprintf (file, "     i%d", (int) i);

      for (i = 0; i < pbb_nb_params (pbb); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  /* Number of disjunct components.  Remove this when
     PBB_TRANSFORMED_SCATTERING will be a pointset_powerset.  */
  fprintf (file, "1\n");
  ppl_print_polyhedron_matrix (file, PBB_TRANSFORMED_SCATTERING (pbb)
			       ? PBB_TRANSFORMED_SCATTERING (pbb)
			       : PBB_ORIGINAL_SCATTERING (pbb));

  if (verbosity > 0)
    fprintf (file, "#)\n");
}

/* Prints to FILE the scattering function of PBB, at some VERBOSITY
   level.  */

void
print_scattering_function (FILE *file, poly_bb_p pbb, int verbosity)
{
  if (!PBB_TRANSFORMED (pbb))
    return;

  if (PBB_TRANSFORMED_SCATTERING (pbb)
      || PBB_ORIGINAL_SCATTERING (pbb))
    {
      if (verbosity > 0)
	fprintf (file, "# Scattering function is provided\n");

      fprintf (file, "1\n");
    }
  else
    {
      if (verbosity > 0)
	fprintf (file, "# Scattering function is not provided\n");

      fprintf (file, "0\n");
      return;
    }

  openscop_print_scattering_function_1 (file, pbb, verbosity);

  if (verbosity > 0)
    fprintf (file, "# Scattering names are not provided\n");

  fprintf (file, "0\n");

}

/* Prints to FILE the iteration domain of PBB, at some VERBOSITY
   level.  */

void
print_iteration_domain (FILE *file, poly_bb_p pbb, int verbosity)
{
  print_pbb_domain (file, pbb, verbosity);
}

/* Prints to FILE the scattering functions of every PBB of SCOP.  */

void
print_scattering_functions (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    print_scattering_function (file, pbb, verbosity);
}

/* Prints to FILE the iteration domains of every PBB of SCOP, at some
   VERBOSITY level.  */

void
print_iteration_domains (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    print_iteration_domain (file, pbb, verbosity);
}

/* Prints to STDERR the scattering function of PBB, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_scattering_function (poly_bb_p pbb, int verbosity)
{
  print_scattering_function (stderr, pbb, verbosity);
}

/* Prints to STDERR the iteration domain of PBB, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_iteration_domain (poly_bb_p pbb, int verbosity)
{
  print_iteration_domain (stderr, pbb, verbosity);
}

/* Prints to STDERR the scattering functions of every PBB of SCOP, at
   some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_scattering_functions (scop_p scop, int verbosity)
{
  print_scattering_functions (stderr, scop, verbosity);
}

/* Prints to STDERR the iteration domains of every PBB of SCOP, at
   some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_iteration_domains (scop_p scop, int verbosity)
{
  print_iteration_domains (stderr, scop, verbosity);
}

/* Read N integer from FILE.  */

int *
openscop_read_N_int (FILE *file, int N)
{
  char s[OPENSCOP_MAX_STRING];
  char *str;
  int i, *res = (int *) xmalloc (OPENSCOP_MAX_STRING * sizeof (int));

  /* Skip blank and commented lines.  */
  while (fgets (s, sizeof s, file) == (char *) 0
	 || s[0] == '#'
	 || ISSPACE (s[0]))
    ;

  str = s;

  for (i = 0; i < N; i++)
    {
      sscanf (str, "%d", &res[i]);

      /* Jump the integer that was read.  */
      while ((*str) && !ISSPACE (*str) && (*str != '#'))
	str++;

      /* Jump spaces.  */
      while ((*str) && ISSPACE (*str) && (*str != '#'))
	str++;
    }

  return res;
}

/* Read one integer from FILE.  */

static int
openscop_read_one_int (FILE *file)
{
  int *x = openscop_read_N_int (file, 1);
  int res = *x;

  free (x);
  return res;
}

/* Read N string from FILE.  */

static char *
openscop_read_N_string (FILE *file, int N)
{
  int count, i;
  char str[OPENSCOP_MAX_STRING];
  char *tmp = (char *) xmalloc (sizeof (char) * OPENSCOP_MAX_STRING);
  char *s = NULL;

  /* Skip blank and commented lines.  */
  while (fgets (str, sizeof str, file) == (char *) 0
	 || str[0] == '#'
	 || ISSPACE (str[0]))
    ;

  s = str;
  count = 0;

  for (i = 0; i < N; i++)
    {
      /* Read the first word.  */
      for (; (*s) && (!ISSPACE (*s)) && (*s != '#'); ++count)
        tmp[count] = *(s++);

      tmp[count] = ' ';
      count++;

      /* Jump spaces.  */
      while ((*s) && ISSPACE (*s) && (*s != '#'))
	s++;
    }

  tmp[count-1] = '\0';

  return tmp;
}

/* Read one string from FILE.  */

static char *
openscop_read_one_string (FILE *file)
{
  return openscop_read_N_string (file, 1);
}

/* Read from FILE the powerset PS in its OpenScop matrix form.  OUTPUT is the
   number of output dimensions, INPUT is the number of input dimensions,
   LOCALS is the number of existentially quantified variables and PARAMS is
   the number of parameters.  */

static void
openscop_read_powerset_matrix (FILE *file,
			       ppl_Pointset_Powerset_C_Polyhedron_t *ps,
			       int *output, int *input, int *locals,
			       int *params)
{
  int nb_disjuncts, i;

  nb_disjuncts = openscop_read_one_int (file);

  for (i = 0; i < nb_disjuncts; i++)
    {
      ppl_Polyhedron_t ph;

      openscop_read_polyhedron_matrix (file, &ph, output, input, locals,
				       params);
      if (!ph)
        *ps = NULL;
      else if (i == 0)
        ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (ps, ph);
      else
        ppl_Pointset_Powerset_C_Polyhedron_add_disjunct (*ps, ph);
    }
}

/* Read a scattering function from FILE and save it to PBB.  Return whether
   the scattering function was provided or not.  */

static bool
graphite_read_scatt (FILE *file, poly_bb_p pbb)
{
  bool scattering_provided = false;
  int output, input, locals, params;
  ppl_Polyhedron_t newp;

  if (openscop_read_one_int (file) > 0)
    {
      /* Read number of disjunct components.  */
      openscop_read_one_int (file);

      /* Read scattering function.  */
      openscop_read_polyhedron_matrix (file, &newp, &output, &input,
				       &locals, &params);
      store_scattering (PBB_SCOP (pbb));
      PBB_TRANSFORMED (pbb) = poly_scattering_new ();
      PBB_TRANSFORMED_SCATTERING (pbb) = newp;
      PBB_NB_LOCAL_VARIABLES (pbb) = locals;

      /* New scattering dimension.  */
      PBB_NB_SCATTERING_TRANSFORM (pbb) = output;

      scattering_provided = true;
    }

  return scattering_provided;
}

/* Read a scop file.  Return true if the scop is transformed.  */

static bool
graphite_read_scop_file (FILE *file, scop_p scop)
{
  char *tmp, *language;
  size_t i, j, nb_statements, nbr, nbw;
  int input, output, locals, params;
  ppl_Pointset_Powerset_C_Polyhedron_t ps;
  poly_bb_p pbb;
  bool transform_done = false;

  /* Ensure that the file is in OpenScop format.  */
  tmp = openscop_read_N_string (file, 2);

  if (strcmp (tmp, "SCoP 1"))
    {
      error ("the file is not in OpenScop format");
      return false;
    }

  free (tmp);

  /* Read the language.  */
  language = openscop_read_one_string (file);

  if (strcmp (language, "Gimple"))
    {
      error ("the language is not recognized");
      return false;
    }

  free (language);

  /* Read the context but do not use it.  */
  openscop_read_powerset_matrix (file, &ps, &input, &output, &locals, &params);

  if ((size_t) params != scop->nb_params)
    {
      error ("parameters number in the scop file is different from the"
	     " internal scop parameter number");
      return false;
    }

  /* Read parameter names if provided.  */
  if (openscop_read_one_int (file))
    openscop_read_N_string (file, scop->nb_params);

  nb_statements = openscop_read_one_int (file);

  if (nb_statements != VEC_length (poly_bb_p, SCOP_BBS (scop)))
    {
      error ("number of statements in the OpenScop file does not match"
	     " the graphite internal statements number");
      return false;
    }

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    {
      /* Read iteration domain.  */
      openscop_read_powerset_matrix (file, &ps, &input, &output, &locals,
				     &params);

      /* Read scattering.  */
      transform_done = graphite_read_scatt (file, pbb);

      /* Scattering names.  */
      openscop_read_one_int (file);

      /* Read access functions.  */
      if (openscop_read_one_int (file) > 0)
	{
	  nbr = openscop_read_one_int (file);

	  /* Read access functions.  */
	  for (j = 0; j < nbr; j++)
	    openscop_read_powerset_matrix (file, &ps, &input, &output, &locals,
					   &params);

	  nbw = openscop_read_one_int (file);

	  /* Write access functions.  */
	  for (j = 0; j < nbw; j++)
	    openscop_read_powerset_matrix (file, &ps, &input, &output, &locals,
					   &params);
	}

      /* Statement body.  */
      openscop_read_one_int (file);
    }

  return transform_done;
}

/* Initialize and return a file that will be used to write a scop.  SCOP_NUMBER
   is a sequential number (identifier) used to differentiate scop files.
   Examples of the generated file names: dump_base_name.0.graphite,
   dump_base_name.1.graphite, dump_base_name.2.graphite, etc.  */

static FILE *
init_graphite_out_file (int scop_number)
{
  FILE *graphite_out_file;
  int len = strlen (dump_base_name);
  char *dumpname = XNEWVEC (char, len + 25);
  char *s_scop_number = XNEWVEC (char, 15);

  memcpy (dumpname, dump_base_name, len + 1);
  strip_off_ending (dumpname, len);
  sprintf (s_scop_number, ".%d", scop_number);
  strcat (dumpname, s_scop_number);
  strcat (dumpname, ".graphite");
  graphite_out_file = fopen (dumpname, "w+b");

  if (graphite_out_file == 0)
    fatal_error ("can%'t open %s for writing: %m", dumpname);

  free (dumpname);

  return graphite_out_file;
}

/* Open and return a file used for scop reading.  SCOP_NUMBER is a sequential
   number (identifier) used to differentiate scop files.  Examples of the
   generated file names: dump_base_name.0.graphite, dump_base_name.1.graphite,
   dump_base_name.2.graphite, etc.  */

static FILE *
init_graphite_in_file (int scop_number)
{
  FILE *graphite_in_file;
  int len = strlen (dump_base_name);
  char *dumpname = XNEWVEC (char, len + 25);
  char *s_scop_number = XNEWVEC (char, 15);

  memcpy (dumpname, dump_base_name, len + 1);
  strip_off_ending (dumpname, len);
  sprintf (s_scop_number, ".%d", scop_number);
  strcat (dumpname, s_scop_number);
  strcat (dumpname, ".graphite");
  graphite_in_file = fopen (dumpname, "r+b");

  if (graphite_in_file == 0)
    fatal_error ("can%'t open %s for reading: %m", dumpname);

  free (dumpname);

  return graphite_in_file;
}

/* Apply graphite transformations to all the basic blocks of SCOP.  */

bool
apply_poly_transforms (scop_p scop)
{
  bool transform_done = false;
  FILE *graphite_file;
  static size_t file_scop_number = 0;

  /* This feature is only enabled in the Graphite branch.  */
  if (0)
    {
      graphite_file = init_graphite_in_file (file_scop_number);
      transform_done |= graphite_read_scop_file (graphite_file, scop);

      /* We cannot check for the legality of the transform here: there
	 are cases where graphite_legal_transform cannot determine the
	 dependence at compile time.  For an example, see the
	 explanation of why http://gcc.gnu.org/PR45450 is invalid.  */
      if (0 && !graphite_legal_transform (scop))
	fatal_error ("the graphite file read for scop %d does not contain a legal transform",
		     (int) file_scop_number);

      file_scop_number++;
    }

  /* Generate code even if we did not apply any real transformation.
     This also allows to check the performance for the identity
     transformation: GIMPLE -> GRAPHITE -> GIMPLE
     Keep in mind that CLooG optimizes in control, so the loop structure
     may change, even if we only use -fgraphite-identity.  */
  if (flag_graphite_identity)
    transform_done = true;

  if (flag_loop_parallelize_all)
    transform_done = true;

  if (flag_loop_block)
    transform_done |= scop_do_block (scop);
  else
    {
      if (flag_loop_strip_mine)
	transform_done |= scop_do_strip_mine (scop, 0);

      if (flag_loop_interchange)
	transform_done |= scop_do_interchange (scop);
    }

  if (flag_loop_flatten)
    transform_done |= flatten_all_loops (scop);

  /* This feature is only enabled in the Graphite branch.  */
  if (0)
    {
      graphite_file = init_graphite_out_file (file_scop_number);
      print_scop (graphite_file, scop, 1);
      file_scop_number++;
    }

  return transform_done;
}

/* Returns true when it PDR1 is a duplicate of PDR2: same PBB, and
   their ACCESSES, TYPE, and NB_SUBSCRIPTS are the same.  */

static inline bool
can_collapse_pdrs (poly_dr_p pdr1, poly_dr_p pdr2)
{
  bool res;
  ppl_Pointset_Powerset_C_Polyhedron_t af1, af2, diff;

  if (PDR_PBB (pdr1) != PDR_PBB (pdr2)
      || PDR_NB_SUBSCRIPTS (pdr1) != PDR_NB_SUBSCRIPTS (pdr2)
      || PDR_TYPE (pdr1) != PDR_TYPE (pdr2))
    return false;

  af1 = PDR_ACCESSES (pdr1);
  af2 = PDR_ACCESSES (pdr2);
  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&diff, af1);
  ppl_Pointset_Powerset_C_Polyhedron_difference_assign (diff, af2);

  res = ppl_Pointset_Powerset_C_Polyhedron_is_empty (diff);
  ppl_delete_Pointset_Powerset_C_Polyhedron (diff);
  return res;
}

/* Removes duplicated data references in PBB.  */

void
pbb_remove_duplicate_pdrs (poly_bb_p pbb)
{
  int i, j;
  poly_dr_p pdr1, pdr2;

  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb), i, pdr1)
    for (j = i + 1; VEC_iterate (poly_dr_p, PBB_DRS (pbb), j, pdr2); j++)
      if (can_collapse_pdrs (pdr1, pdr2))
	{
	  PDR_NB_REFS (pdr1) += PDR_NB_REFS (pdr2);
	  free_poly_dr (pdr2);
	  VEC_ordered_remove (poly_dr_p, PBB_DRS (pbb), j);
	}

  PBB_PDR_DUPLICATES_REMOVED (pbb) = true;
}

/* Create a new polyhedral data reference and add it to PBB.  It is
   defined by its ACCESSES, its TYPE, and the number of subscripts
   NB_SUBSCRIPTS.  */

void
new_poly_dr (poly_bb_p pbb, int dr_base_object_set,
	     ppl_Pointset_Powerset_C_Polyhedron_t accesses,
	     enum poly_dr_type type, void *cdr, graphite_dim_t nb_subscripts)
{
  static int id = 0;
  poly_dr_p pdr = XNEW (struct poly_dr);

  PDR_ID (pdr) = id++;
  PDR_BASE_OBJECT_SET (pdr) = dr_base_object_set;
  PDR_NB_REFS (pdr) = 1;
  PDR_PBB (pdr) = pbb;
  PDR_ACCESSES (pdr) = accesses;
  PDR_TYPE (pdr) = type;
  PDR_CDR (pdr) = cdr;
  PDR_NB_SUBSCRIPTS (pdr) = nb_subscripts;
  VEC_safe_push (poly_dr_p, heap, PBB_DRS (pbb), pdr);
}

/* Free polyhedral data reference PDR.  */

void
free_poly_dr (poly_dr_p pdr)
{
  ppl_delete_Pointset_Powerset_C_Polyhedron (PDR_ACCESSES (pdr));
  XDELETE (pdr);
}

/* Create a new polyhedral black box.  */

poly_bb_p
new_poly_bb (scop_p scop, void *black_box)
{
  poly_bb_p pbb = XNEW (struct poly_bb);

  PBB_DOMAIN (pbb) = NULL;
  PBB_SCOP (pbb) = scop;
  pbb_set_black_box (pbb, black_box);
  PBB_TRANSFORMED (pbb) = NULL;
  PBB_SAVED (pbb) = NULL;
  PBB_ORIGINAL (pbb) = NULL;
  PBB_DRS (pbb) = VEC_alloc (poly_dr_p, heap, 3);
  PBB_IS_REDUCTION (pbb) = false;
  PBB_PDR_DUPLICATES_REMOVED (pbb) = false;
  GBB_PBB ((gimple_bb_p) black_box) = pbb;

  return pbb;
}

/* Free polyhedral black box.  */

void
free_poly_bb (poly_bb_p pbb)
{
  int i;
  poly_dr_p pdr;

  ppl_delete_Pointset_Powerset_C_Polyhedron (PBB_DOMAIN (pbb));

  if (PBB_TRANSFORMED (pbb))
    poly_scattering_free (PBB_TRANSFORMED (pbb));

  if (PBB_SAVED (pbb))
    poly_scattering_free (PBB_SAVED (pbb));

  if (PBB_ORIGINAL (pbb))
    poly_scattering_free (PBB_ORIGINAL (pbb));

  if (PBB_DRS (pbb))
    FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb), i, pdr)
      free_poly_dr (pdr);

  VEC_free (poly_dr_p, heap, PBB_DRS (pbb));
  XDELETE (pbb);
}

static void
print_pdr_access_layout (FILE *file, poly_bb_p pbb, poly_dr_p pdr)
{
  graphite_dim_t i;

  fprintf (file, "#  eq");

  fprintf (file, "   alias");

  for (i = 0; i < PDR_NB_SUBSCRIPTS (pdr); i++)
    fprintf (file, "   sub%d", (int) i);

  for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
    fprintf (file, "     i%d", (int) i);

  for (i = 0; i < pbb_nb_params (pbb); i++)
    fprintf (file, "     p%d", (int) i);

  fprintf (file, "    cst\n");
}

/* Prints to FILE the polyhedral data reference PDR, at some VERBOSITY
   level.  */

void
print_pdr (FILE *file, poly_dr_p pdr, int verbosity)
{
  int alias_set_dim;

  if (verbosity > 1)
    {
      fprintf (file, "# pdr_%d (", PDR_ID (pdr));

      switch (PDR_TYPE (pdr))
	{
	case PDR_READ:
	  fprintf (file, "read \n");
	  break;

	case PDR_WRITE:
	  fprintf (file, "write \n");
	  break;

	case PDR_MAY_WRITE:
	  fprintf (file, "may_write \n");
	  break;

	default:
	  gcc_unreachable ();
	}

      dump_data_reference (file, (data_reference_p) PDR_CDR (pdr));
    }

  if (verbosity > 0)
    {
      fprintf (file, "# data accesses (\n");
      print_pdr_access_layout (file, PDR_PBB (pdr), pdr);
    }

  alias_set_dim = pdr_alias_set_dim (pdr) + 1;

  openscop_print_pdr_powerset (file,
			       PDR_ACCESSES (pdr),
			       PDR_NB_SUBSCRIPTS (pdr),
			       alias_set_dim,
			       pbb_nb_params (PDR_PBB (pdr)));

  if (verbosity > 0)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Prints to STDERR the polyhedral data reference PDR, at some
   VERBOSITY level.  */

DEBUG_FUNCTION void
debug_pdr (poly_dr_p pdr, int verbosity)
{
  print_pdr (stderr, pdr, verbosity);
}

/* Creates a new SCOP containing REGION.  */

scop_p
new_scop (void *region)
{
  scop_p scop = XNEW (struct scop);

  SCOP_CONTEXT (scop) = NULL;
  scop_set_region (scop, region);
  SCOP_BBS (scop) = VEC_alloc (poly_bb_p, heap, 3);
  SCOP_ORIGINAL_PDDRS (scop) = htab_create (10, hash_poly_ddr_p,
					    eq_poly_ddr_p, free_poly_ddr);
  SCOP_ORIGINAL_SCHEDULE (scop) = NULL;
  SCOP_TRANSFORMED_SCHEDULE (scop) = NULL;
  SCOP_SAVED_SCHEDULE (scop) = NULL;
  POLY_SCOP_P (scop) = false;

  return scop;
}

/* Deletes SCOP.  */

void
free_scop (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    free_poly_bb (pbb);

  VEC_free (poly_bb_p, heap, SCOP_BBS (scop));

  if (SCOP_CONTEXT (scop))
    ppl_delete_Pointset_Powerset_C_Polyhedron (SCOP_CONTEXT (scop));

  htab_delete (SCOP_ORIGINAL_PDDRS (scop));
  free_lst (SCOP_ORIGINAL_SCHEDULE (scop));
  free_lst (SCOP_TRANSFORMED_SCHEDULE (scop));
  free_lst (SCOP_SAVED_SCHEDULE (scop));
  XDELETE (scop);
}

/* Print to FILE the domain of PBB in OpenScop format, at some VERBOSITY
   level.  */

static void
openscop_print_pbb_domain (FILE *file, poly_bb_p pbb, int verbosity)
{
  graphite_dim_t i;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (!PBB_DOMAIN (pbb))
    return;

  if (verbosity > 0)
    {
      fprintf (file, "\n# Iteration domain of bb_%d (\n", GBB_BB (gbb)->index);
      fprintf (file, "#eq");

      for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
	fprintf (file, "     i%d", (int) i);

      for (i = 0; i < pbb_nb_params (pbb); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (PBB_DOMAIN (pbb))
    openscop_print_powerset_matrix (file, PBB_DOMAIN (pbb),
				    pbb_dim_iter_domain (pbb),
				    0,
				    0,
				    pbb_nb_params (pbb));
  else
    fprintf (file, "0\n");

  if (verbosity > 0)
    fprintf (file, "#)\n");
}

/* Print to FILE the domain of PBB, at some VERBOSITY level.  */

void
print_pbb_domain (FILE *file, poly_bb_p pbb, int verbosity)
{
  graphite_dim_t i;
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (!PBB_DOMAIN (pbb))
    return;

  if (verbosity > 0)
    {
      fprintf (file, "# Iteration domain of bb_%d (\n", GBB_BB (gbb)->index);
      fprintf (file, "#  eq");

      for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
	fprintf (file, "     i%d", (int) i);

      for (i = 0; i < pbb_nb_params (pbb); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (PBB_DOMAIN (pbb))
    ppl_print_powerset_matrix (file, PBB_DOMAIN (pbb));
  else
    fprintf (file, "0\n");

  if (verbosity > 0)
    fprintf (file, "#)\n");
}

/* Dump the cases of a graphite basic block GBB on FILE.  */

static void
dump_gbb_cases (FILE *file, gimple_bb_p gbb)
{
  int i;
  gimple stmt;
  VEC (gimple, heap) *cases;

  if (!gbb)
    return;

  cases = GBB_CONDITION_CASES (gbb);
  if (VEC_empty (gimple, cases))
    return;

  fprintf (file, "# cases bb_%d (\n", GBB_BB (gbb)->index);

  FOR_EACH_VEC_ELT (gimple, cases, i, stmt)
    {
      fprintf (file, "# ");
      print_gimple_stmt (file, stmt, 0, 0);
    }

  fprintf (file, "#)\n");
}

/* Dump conditions of a graphite basic block GBB on FILE.  */

static void
dump_gbb_conditions (FILE *file, gimple_bb_p gbb)
{
  int i;
  gimple stmt;
  VEC (gimple, heap) *conditions;

  if (!gbb)
    return;

  conditions = GBB_CONDITIONS (gbb);
  if (VEC_empty (gimple, conditions))
    return;

  fprintf (file, "# conditions bb_%d (\n", GBB_BB (gbb)->index);

  FOR_EACH_VEC_ELT (gimple, conditions, i, stmt)
    {
      fprintf (file, "# ");
      print_gimple_stmt (file, stmt, 0, 0);
    }

  fprintf (file, "#)\n");
}

/* Print to FILE all the data references of PBB, at some VERBOSITY
   level.  */

void
print_pdrs (FILE *file, poly_bb_p pbb, int verbosity)
{
  int i;
  poly_dr_p pdr;
  int nb_reads = 0;
  int nb_writes = 0;

  if (VEC_length (poly_dr_p, PBB_DRS (pbb)) == 0)
    {
      if (verbosity > 0)
	fprintf (file, "# Access informations are not provided\n");\
      fprintf (file, "0\n");
      return;
    }

  if (verbosity > 1)
    fprintf (file, "# Data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Access informations are provided\n");
  fprintf (file, "1\n");

  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) == PDR_READ)
      nb_reads++;
    else
      nb_writes++;

  if (verbosity > 1)
    fprintf (file, "# Read data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Read access informations\n");
  fprintf (file, "%d\n", nb_reads);

  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) == PDR_READ)
      print_pdr (file, pdr, verbosity);

  if (verbosity > 1)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "# Write data references (\n");

  if (verbosity > 0)
    fprintf (file, "# Write access informations\n");
  fprintf (file, "%d\n", nb_writes);

  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb), i, pdr)
    if (PDR_TYPE (pdr) != PDR_READ)
      print_pdr (file, pdr, verbosity);

  if (verbosity > 1)
    fprintf (file, "#)\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to STDERR all the data references of PBB.  */

DEBUG_FUNCTION void
debug_pdrs (poly_bb_p pbb, int verbosity)
{
  print_pdrs (stderr, pbb, verbosity);
}

/* Print to FILE the body of PBB, at some VERBOSITY level.
   If statement_body_provided is false statement body is not printed.  */

static void
print_pbb_body (FILE *file, poly_bb_p pbb, int verbosity,
		bool statement_body_provided)
{
  if (verbosity > 1)
    fprintf (file, "# Body (\n");

  if (!statement_body_provided)
    {
      if (verbosity > 0)
	fprintf (file, "# Statement body is not provided\n");

      fprintf (file, "0\n");

      if (verbosity > 1)
	fprintf (file, "#)\n");
      return;
    }

  if (verbosity > 0)
    fprintf (file, "# Statement body is provided\n");
  fprintf (file, "1\n");

  if (verbosity > 0)
    fprintf (file, "# Original iterator names\n# Iterator names are not provided yet.\n");

  if (verbosity > 0)
    fprintf (file, "# Statement body\n");

  fprintf (file, "{\n");
  dump_bb (pbb_bb (pbb), file, 0);
  fprintf (file, "}\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to FILE the domain and scattering function of PBB, at some
   VERBOSITY level.  */

void
print_pbb (FILE *file, poly_bb_p pbb, int verbosity)
{
  if (verbosity > 1)
    {
      fprintf (file, "# pbb_%d (\n", pbb_index (pbb));
      dump_gbb_conditions (file, PBB_BLACK_BOX (pbb));
      dump_gbb_cases (file, PBB_BLACK_BOX (pbb));
    }

  openscop_print_pbb_domain (file, pbb, verbosity);
  print_scattering_function (file, pbb, verbosity);
  print_pdrs (file, pbb, verbosity);
  print_pbb_body (file, pbb, verbosity, false);

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to FILE the parameters of SCOP, at some VERBOSITY level.  */

void
print_scop_params (FILE *file, scop_p scop, int verbosity)
{
  int i;
  tree t;

  if (verbosity > 1)
    fprintf (file, "# parameters (\n");

  if (VEC_length (tree, SESE_PARAMS (SCOP_REGION (scop))))
    {
      if (verbosity > 0)
	fprintf (file, "# Parameter names are provided\n");

      fprintf (file, "1\n");

      if (verbosity > 0)
	fprintf (file, "# Parameter names\n");
    }
  else
    {
      if (verbosity > 0)
	fprintf (file, "# Parameter names are not provided\n");
      fprintf (file, "0\n");
    }

  FOR_EACH_VEC_ELT (tree, SESE_PARAMS (SCOP_REGION (scop)), i, t)
    {
      print_generic_expr (file, t, 0);
      fprintf (file, " ");
    }

  fprintf (file, "\n");

  if (verbosity > 1)
    fprintf (file, "#)\n");
}

/* Print to FILE the context of SCoP in OpenScop format, at some VERBOSITY
   level.  */

static void
openscop_print_scop_context (FILE *file, scop_p scop, int verbosity)
{
  graphite_dim_t i;

  if (verbosity > 0)
    {
      fprintf (file, "# Context (\n");
      fprintf (file, "#eq");

      for (i = 0; i < scop_nb_params (scop); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (SCOP_CONTEXT (scop))
    openscop_print_powerset_matrix (file, SCOP_CONTEXT (scop), 0, 0, 0,
				    scop_nb_params (scop));
  else
    fprintf (file, "0 %d 0 0 0 %d\n", (int) scop_nb_params (scop) + 2,
	     (int) scop_nb_params (scop));

  if (verbosity > 0)
    fprintf (file, "# )\n");
}

/* Print to FILE the context of SCoP, at some VERBOSITY level.  */

void
print_scop_context (FILE *file, scop_p scop, int verbosity)
{
  graphite_dim_t i;

  if (verbosity > 0)
    {
      fprintf (file, "# Context (\n");
      fprintf (file, "#eq");

      for (i = 0; i < scop_nb_params (scop); i++)
	fprintf (file, "     p%d", (int) i);

      fprintf (file, "    cst\n");
    }

  if (SCOP_CONTEXT (scop))
    ppl_print_powerset_matrix (file, SCOP_CONTEXT (scop));
  else
    fprintf (file, "0 %d\n", (int) scop_nb_params (scop) + 2);

  if (verbosity > 0)
    fprintf (file, "# )\n");
}

/* Print to FILE the SCOP, at some VERBOSITY level.  */

void
print_scop (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  fprintf (file, "SCoP 1\n#(\n");
  fprintf (file, "# Language\nGimple\n");
  openscop_print_scop_context (file, scop, verbosity);
  print_scop_params (file, scop, verbosity);

  if (verbosity > 0)
    fprintf (file, "# Number of statements\n");

  fprintf (file, "%d\n",VEC_length (poly_bb_p, SCOP_BBS (scop)));

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    print_pbb (file, pbb, verbosity);

  if (verbosity > 1)
    {
      fprintf (file, "# original_lst (\n");
      print_lst (file, SCOP_ORIGINAL_SCHEDULE (scop), 0);
      fprintf (file, "\n#)\n");

      fprintf (file, "# transformed_lst (\n");
      print_lst (file, SCOP_TRANSFORMED_SCHEDULE (scop), 0);
      fprintf (file, "\n#)\n");
    }

  fprintf (file, "#)\n");
}

/* Print to FILE the input file that CLooG would expect as input, at
   some VERBOSITY level.  */

void
print_cloog (FILE *file, scop_p scop, int verbosity)
{
  int i;
  poly_bb_p pbb;

  fprintf (file, "# SCoP (generated by GCC/Graphite\n");
  if (verbosity > 0)
    fprintf (file, "# CLooG output language\n");
  fprintf (file, "c\n");

  print_scop_context (file, scop, verbosity);
  print_scop_params (file, scop, verbosity);

  if (verbosity > 0)
    fprintf (file, "# Number of statements\n");

  fprintf (file, "%d\n", VEC_length (poly_bb_p, SCOP_BBS (scop)));

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    {
      if (verbosity > 1)
	fprintf (file, "# pbb_%d (\n", pbb_index (pbb));

      print_pbb_domain (file, pbb, verbosity);
      fprintf (file, "0 0 0");

      if (verbosity > 0)
	fprintf (file, "# For future CLooG options.\n");
      else
	fprintf (file, "\n");

      if (verbosity > 1)
	fprintf (file, "#)\n");
    }

  fprintf (file, "0");
  if (verbosity > 0)
    fprintf (file, "# Don't set the iterator names.\n");
  else
    fprintf (file, "\n");

  if (verbosity > 0)
    fprintf (file, "# Number of scattering functions\n");

  fprintf (file, "%d\n", VEC_length (poly_bb_p, SCOP_BBS (scop)));
  unify_scattering_dimensions (scop);

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb)
    {
      if (!PBB_TRANSFORMED (pbb)
	  || !(PBB_TRANSFORMED_SCATTERING (pbb)
	       || PBB_ORIGINAL_SCATTERING (pbb)))
	continue;

      if (verbosity > 1)
	fprintf (file, "# pbb_%d (\n", pbb_index (pbb));

      print_scattering_function_1 (file, pbb, verbosity);

      if (verbosity > 1)
	fprintf (file, "#)\n");
    }

  fprintf (file, "0");
  if (verbosity > 0)
    fprintf (file, "# Don't set the scattering dimension names.\n");
  else
    fprintf (file, "\n");

  fprintf (file, "#)\n");
}

/* Print to STDERR the domain of PBB, at some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_pbb_domain (poly_bb_p pbb, int verbosity)
{
  print_pbb_domain (stderr, pbb, verbosity);
}

/* Print to FILE the domain and scattering function of PBB, at some
   VERBOSITY level.  */

DEBUG_FUNCTION void
debug_pbb (poly_bb_p pbb, int verbosity)
{
  print_pbb (stderr, pbb, verbosity);
}

/* Print to STDERR the context of SCOP, at some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_scop_context (scop_p scop, int verbosity)
{
  print_scop_context (stderr, scop, verbosity);
}

/* Print to STDERR the SCOP, at some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_scop (scop_p scop, int verbosity)
{
  print_scop (stderr, scop, verbosity);
}

/* Print to STDERR the SCOP under CLooG format, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_cloog (scop_p scop, int verbosity)
{
  print_cloog (stderr, scop, verbosity);
}

/* Print to STDERR the parameters of SCOP, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_scop_params (scop_p scop, int verbosity)
{
  print_scop_params (stderr, scop, verbosity);
}


/* The dimension in the transformed scattering polyhedron of PBB
   containing the scattering iterator for the loop at depth LOOP_DEPTH.  */

ppl_dimension_type
psct_scattering_dim_for_loop_depth (poly_bb_p pbb, graphite_dim_t loop_depth)
{
  ppl_const_Constraint_System_t pcs;
  ppl_Constraint_System_const_iterator_t cit, cend;
  ppl_const_Constraint_t cstr;
  ppl_Polyhedron_t ph = PBB_TRANSFORMED_SCATTERING (pbb);
  ppl_dimension_type iter = psct_iterator_dim (pbb, loop_depth);
  ppl_Linear_Expression_t expr;
  ppl_Coefficient_t coef;
  mpz_t val;
  graphite_dim_t i;

  mpz_init (val);
  ppl_new_Coefficient (&coef);
  ppl_Polyhedron_get_constraints (ph, &pcs);
  ppl_new_Constraint_System_const_iterator (&cit);
  ppl_new_Constraint_System_const_iterator (&cend);

  for (ppl_Constraint_System_begin (pcs, cit),
	 ppl_Constraint_System_end (pcs, cend);
       !ppl_Constraint_System_const_iterator_equal_test (cit, cend);
       ppl_Constraint_System_const_iterator_increment (cit))
    {
      ppl_Constraint_System_const_iterator_dereference (cit, &cstr);
      ppl_new_Linear_Expression_from_Constraint (&expr, cstr);
      ppl_Linear_Expression_coefficient (expr, iter, coef);
      ppl_Coefficient_to_mpz_t (coef, val);

      if (mpz_sgn (val) == 0)
	{
	  ppl_delete_Linear_Expression (expr);
	  continue;
	}

      for (i = 0; i < pbb_nb_scattering_transform (pbb); i++)
	{
	  ppl_dimension_type scatter = psct_scattering_dim (pbb, i);

	  ppl_Linear_Expression_coefficient (expr, scatter, coef);
	  ppl_Coefficient_to_mpz_t (coef, val);

	  if (mpz_sgn (val) != 0)
	    {
	      mpz_clear (val);
	      ppl_delete_Linear_Expression (expr);
	      ppl_delete_Coefficient (coef);
	      ppl_delete_Constraint_System_const_iterator (cit);
	      ppl_delete_Constraint_System_const_iterator (cend);

	      return scatter;
	    }
	}
    }

  gcc_unreachable ();
}

/* Returns the number of iterations RES of the loop around PBB at
   time(scattering) dimension TIME_DEPTH.  */

void
pbb_number_of_iterations_at_time (poly_bb_p pbb,
				  graphite_dim_t time_depth,
				  mpz_t res)
{
  ppl_Pointset_Powerset_C_Polyhedron_t domain, sctr_lb, sctr_ub;
  ppl_dimension_type domain_dim, sctr_dim;
  graphite_dim_t dim_iter_domain = pbb_dim_iter_domain (pbb);
  ppl_Linear_Expression_t le;
  mpz_t lb, ub, diff, one;
  int i;

  ppl_Polyhedron_space_dimension (PBB_TRANSFORMED_SCATTERING (pbb), &sctr_dim);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&domain, PBB_DOMAIN (pbb));

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (domain, &domain_dim);

  mpz_init (diff);
  mpz_init (lb);
  mpz_init (ub);
  mpz_init (one);
  mpz_set_si (one, 1);

  /* Compute the upper bound on the original iteration domain and add
     that upper bound to the scattering.  */
  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    (&sctr_ub, PBB_TRANSFORMED_SCATTERING (pbb));
  for (i = 0; i < (int) dim_iter_domain; i++)
    {
      ppl_Linear_Expression_t eq;
      ppl_Constraint_t pc;
      ppl_Constraint_System_t cs;
      ppl_Polyhedron_t ph;
      ppl_Pointset_Powerset_C_Polyhedron_t pph;

      ppl_new_Linear_Expression_with_dimension (&le, domain_dim);
      ppl_set_coef (le, i, 1);
      ppl_min_for_le_pointset (domain, le, lb);
      ppl_max_for_le_pointset (domain, le, ub);
      mpz_sub (diff, ub, lb);
      mpz_add (diff, diff, one);

      ppl_new_Linear_Expression_with_dimension (&eq, sctr_dim);
      ppl_set_coef (eq, psct_iterator_dim (pbb, i), -1);
      ppl_set_inhomogeneous_gmp (eq, diff);

      ppl_new_Constraint (&pc, eq, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_new_Constraint_System_from_Constraint (&cs, pc);
      ppl_new_C_Polyhedron_from_Constraint_System (&ph, cs);
      ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&pph, ph);
      ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (sctr_ub, pph);

      ppl_delete_Linear_Expression (le);
      ppl_delete_Linear_Expression (eq);
      ppl_delete_Polyhedron (ph);
      ppl_delete_Pointset_Powerset_C_Polyhedron (pph);
      ppl_delete_Constraint (pc);
      ppl_delete_Constraint_System (cs);
    }

  /* Compute the lower bound on the original iteration domain and add
     it to the scattering.  */
  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    (&sctr_lb, PBB_TRANSFORMED_SCATTERING (pbb));
  for (i = 0; i < (int) dim_iter_domain; i++)
    {
      ppl_Linear_Expression_t eq;
      ppl_Constraint_t pc;
      ppl_Constraint_System_t cs;
      ppl_Polyhedron_t ph;
      ppl_Pointset_Powerset_C_Polyhedron_t pph;

      ppl_new_Linear_Expression_with_dimension (&le, domain_dim);
      ppl_set_coef (le, i, 1);
      ppl_min_for_le_pointset (domain, le, lb);

      ppl_new_Linear_Expression_with_dimension (&eq, sctr_dim);
      ppl_set_coef (eq, psct_iterator_dim (pbb, i), -1);
      ppl_set_inhomogeneous_gmp (eq, lb);

      ppl_new_Constraint (&pc, eq, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_new_Constraint_System_from_Constraint (&cs, pc);
      ppl_new_C_Polyhedron_from_Constraint_System (&ph, cs);
      ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&pph, ph);
      ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (sctr_lb, pph);

      ppl_delete_Linear_Expression (le);
      ppl_delete_Linear_Expression (eq);
      ppl_delete_Polyhedron (ph);
      ppl_delete_Pointset_Powerset_C_Polyhedron (pph);
      ppl_delete_Constraint (pc);
      ppl_delete_Constraint_System (cs);
    }

  /* Extract the number of iterations.  */
  ppl_new_Linear_Expression_with_dimension (&le, sctr_dim);
  ppl_set_coef (le, time_depth, 1);
  ppl_min_for_le_pointset (sctr_lb, le, lb);
  ppl_max_for_le_pointset (sctr_ub, le, ub);
  mpz_sub (res, ub, lb);

  mpz_clear (one);
  mpz_clear (diff);
  mpz_clear (lb);
  mpz_clear (ub);
  ppl_delete_Linear_Expression (le);
  ppl_delete_Pointset_Powerset_C_Polyhedron (sctr_ub);
  ppl_delete_Pointset_Powerset_C_Polyhedron (sctr_lb);
  ppl_delete_Pointset_Powerset_C_Polyhedron (domain);
}

/* Translates LOOP to LST.  */

static lst_p
loop_to_lst (loop_p loop, VEC (poly_bb_p, heap) *bbs, int *i)
{
  poly_bb_p pbb;
  VEC (lst_p, heap) *seq = VEC_alloc (lst_p, heap, 5);

  for (; VEC_iterate (poly_bb_p, bbs, *i, pbb); (*i)++)
    {
      lst_p stmt;
      basic_block bb = GBB_BB (PBB_BLACK_BOX (pbb));

      if (bb->loop_father == loop)
	stmt = new_lst_stmt (pbb);
      else if (flow_bb_inside_loop_p (loop, bb))
	{
	  loop_p next = loop->inner;

	  while (next && !flow_bb_inside_loop_p (next, bb))
	    next = next->next;

	  stmt = loop_to_lst (next, bbs, i);
	}
      else
	{
	  (*i)--;
	  return new_lst_loop (seq);
	}

      VEC_safe_push (lst_p, heap, seq, stmt);
    }

  return new_lst_loop (seq);
}

/* Reads the original scattering of the SCOP and returns an LST
   representing it.  */

void
scop_to_lst (scop_p scop)
{
  lst_p res;
  int i, n = VEC_length (poly_bb_p, SCOP_BBS (scop));
  VEC (lst_p, heap) *seq = VEC_alloc (lst_p, heap, 5);
  sese region = SCOP_REGION (scop);

  for (i = 0; i < n; i++)
    {
      poly_bb_p pbb = VEC_index (poly_bb_p, SCOP_BBS (scop), i);
      loop_p loop = outermost_loop_in_sese (region, GBB_BB (PBB_BLACK_BOX (pbb)));

      if (loop_in_sese_p (loop, region))
	res = loop_to_lst (loop, SCOP_BBS (scop), &i);
      else
	res = new_lst_stmt (pbb);

      VEC_safe_push (lst_p, heap, seq, res);
    }

  res = new_lst_loop (seq);
  SCOP_ORIGINAL_SCHEDULE (scop) = res;
  SCOP_TRANSFORMED_SCHEDULE (scop) = copy_lst (res);
}

/* Print to FILE on a new line COLUMN white spaces.  */

static void
lst_indent_to (FILE *file, int column)
{
  int i;

  if (column > 0)
    fprintf (file, "\n#");

  for (i = 0; i < column; i++)
    fprintf (file, " ");
}

/* Print LST to FILE with INDENT spaces of indentation.  */

void
print_lst (FILE *file, lst_p lst, int indent)
{
  if (!lst)
    return;

  lst_indent_to (file, indent);

  if (LST_LOOP_P (lst))
    {
      int i;
      lst_p l;

      if (LST_LOOP_FATHER (lst))
	fprintf (file, "%d (loop", lst_dewey_number (lst));
      else
	fprintf (file, "#(root");

      FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
	print_lst (file, l, indent + 2);

      fprintf (file, ")");
    }
  else
    fprintf (file, "%d stmt_%d", lst_dewey_number (lst), pbb_index (LST_PBB (lst)));
}

/* Print LST to STDERR.  */

DEBUG_FUNCTION void
debug_lst (lst_p lst)
{
  print_lst (stderr, lst, 0);
}

/* Pretty print to FILE the loop statement tree LST in DOT format.  */

static void
dot_lst_1 (FILE *file, lst_p lst)
{
  if (!lst)
    return;

  if (LST_LOOP_P (lst))
    {
      int i;
      lst_p l;

      if (!LST_LOOP_FATHER (lst))
	fprintf (file, "L -> L_%d_%d\n",
		 lst_depth (lst),
		 lst_dewey_number (lst));
      else
	fprintf (file, "L_%d_%d -> L_%d_%d\n",
		 lst_depth (LST_LOOP_FATHER (lst)),
		 lst_dewey_number (LST_LOOP_FATHER (lst)),
		 lst_depth (lst),
		 lst_dewey_number (lst));

      FOR_EACH_VEC_ELT (lst_p, LST_SEQ (lst), i, l)
	dot_lst_1 (file, l);
    }

  else
    fprintf (file, "L_%d_%d -> S_%d\n",
	     lst_depth (LST_LOOP_FATHER (lst)),
	     lst_dewey_number (LST_LOOP_FATHER (lst)),
	     pbb_index (LST_PBB (lst)));

}

/* Display the LST using dotty.  */

DEBUG_FUNCTION void
dot_lst (lst_p lst)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *stream = fopen ("/tmp/lst.dot", "w");
  gcc_assert (stream);

  fputs ("digraph all {\n", stream);
  dot_lst_1 (stream, lst);
  fputs ("}\n\n", stream);
  fclose (stream);

  system ("dotty /tmp/lst.dot &");
#else
  fputs ("digraph all {\n", stderr);
  dot_lst_1 (stderr, lst);
  fputs ("}\n\n", stderr);

#endif
}

/* Computes a checksum for the code generated by CLooG for SCOP.  */

DEBUG_FUNCTION void
cloog_checksum (scop_p scop ATTRIBUTE_UNUSED)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *stream = fopen ("/tmp/scop.cloog", "w");
  gcc_assert (stream);
  print_cloog (stream, scop, 0);
  fclose (stream);

  fputs ("\n", stdout);
  system ("cloog -compilable 1 /tmp/scop.cloog > /tmp/scop.c ; gcc -O0 -g /tmp/scop.c -lm -o /tmp/scop; /tmp/scop | md5sum ");
#endif
}

#endif

