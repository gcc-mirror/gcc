/* Data references and dependences detectors.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* This pass walks a given loop structure searching for array
   references.  The information about the array accesses is recorded
   in DATA_REFERENCE structures. 
   
   The basic test for determining the dependences is: 
   given two access functions chrec1 and chrec2 to a same array, and 
   x and y two vectors from the iteration domain, the same element of 
   the array is accessed twice at iterations x and y if and only if:
   |             chrec1 (x) == chrec2 (y).
   
   The goals of this analysis are:
   
   - to determine the independence: the relation between two
     independent accesses is qualified with the chrec_known (this
     information allows a loop parallelization),
     
   - when two data references access the same data, to qualify the
     dependence relation with classic dependence representations:
     
       - distance vectors
       - direction vectors
       - loop carried level dependence
       - polyhedron dependence
     or with the chains of recurrences based representation,
     
   - to define a knowledge base for storing the data dependence 
     information,
     
   - to define an interface to access this data.
   
   
   Definitions:
   
   - subscript: given two array accesses a subscript is the tuple
   composed of the access functions for a given dimension.  Example:
   Given A[f1][f2][f3] and B[g1][g2][g3], there are three subscripts:
   (f1, g1), (f2, g2), (f3, g3).

   - Diophantine equation: an equation whose coefficients and
   solutions are integer constants, for example the equation 
   |   3*x + 2*y = 1
   has an integer solution x = 1 and y = -1.
     
   References:
   
   - "Advanced Compilation for High Performance Computing" by Randy
   Allen and Ken Kennedy.
   http://citeseer.ist.psu.edu/goff91practical.html 
   
   - "Loop Transformations for Restructuring Compilers - The Foundations" 
   by Utpal Banerjee.

   
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"

/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"

/* This is the simplest data dependence test: determines whether the
   data references A and B access the same array/region.  Returns
   false when the property is not computable at compile time.
   Otherwise return true, and DIFFER_P will record the result. This
   utility will not be necessary when alias_sets_conflict_p will be
   less conservative.  */

bool
array_base_name_differ_p (struct data_reference *a,
                          struct data_reference *b,
                          bool *differ_p)
{
  tree base_a = DR_BASE_NAME (a);
  tree base_b = DR_BASE_NAME (b);
  tree ta, tb;

  if (!base_a || !base_b)
    return false;

  ta = TREE_TYPE (base_a);
  tb = TREE_TYPE (base_b);
  
  /* Determine if same base.  Example: for the array accesses
     a[i], b[i] or pointer accesses *a, *b, bases are a, b.  */
  if (base_a == base_b)
    {
      *differ_p = false;
      return true;
    }

  /* For pointer based accesses, (*p)[i], (*q)[j], the bases are (*p)
     and (*q)  */
  if (TREE_CODE (base_a) == INDIRECT_REF && TREE_CODE (base_b) == INDIRECT_REF
      && TREE_OPERAND (base_a, 0) == TREE_OPERAND (base_b, 0))
    {
      *differ_p = false;
      return true;
    }

  /* Record/union based accesses - s.a[i], t.b[j]. bases are s.a,t.b.  */ 
  if (TREE_CODE (base_a) == COMPONENT_REF && TREE_CODE (base_b) == COMPONENT_REF
      && TREE_OPERAND (base_a, 0) == TREE_OPERAND (base_b, 0)
      && TREE_OPERAND (base_a, 1) == TREE_OPERAND (base_b, 1))
    {
      *differ_p = false;
      return true;
    }


  /* Determine if different bases.  */

  /* At this point we know that base_a != base_b.  However, pointer
     accesses of the form x=(*p) and y=(*q), whose bases are p and q,
     may still be pointing to the same base. In SSAed GIMPLE p and q will
     be SSA_NAMES in this case.  Therefore, here we check if they are
     really two different declarations.  */
  if (TREE_CODE (base_a) == VAR_DECL && TREE_CODE (base_b) == VAR_DECL)
    {
      *differ_p = true;
      return true;
    }

  /* Compare two record/union bases s.a and t.b: s != t or (a != b and
     s and t are not unions).  */
  if (TREE_CODE (base_a) == COMPONENT_REF && TREE_CODE (base_b) == COMPONENT_REF
      && ((TREE_CODE (TREE_OPERAND (base_a, 0)) == VAR_DECL
           && TREE_CODE (TREE_OPERAND (base_b, 0)) == VAR_DECL
           && TREE_OPERAND (base_a, 0) != TREE_OPERAND (base_b, 0))
          || (TREE_CODE (TREE_TYPE (TREE_OPERAND (base_a, 0))) == RECORD_TYPE 
              && TREE_CODE (TREE_TYPE (TREE_OPERAND (base_b, 0))) == RECORD_TYPE
              && TREE_OPERAND (base_a, 1) != TREE_OPERAND (base_b, 1))))
    {
      *differ_p = true;
      return true;
    }

  /* Compare a record/union access and an array access.  */ 
  if ((TREE_CODE (base_a) == VAR_DECL
       && (TREE_CODE (base_b) == COMPONENT_REF
           && TREE_CODE (TREE_OPERAND (base_b, 0)) == VAR_DECL))
      || (TREE_CODE (base_b) == VAR_DECL
       && (TREE_CODE (base_a) == COMPONENT_REF
           && TREE_CODE (TREE_OPERAND (base_a, 0)) == VAR_DECL)))
    {
      *differ_p = true;
      return true;
    }

  return false;
}

/* Returns true iff A divides B.  */

static inline bool 
tree_fold_divides_p (tree type, 
		     tree a, 
		     tree b)
{
  /* Determines whether (A == gcd (A, B)).  */
  return integer_zerop 
    (fold (build (MINUS_EXPR, type, a, tree_fold_gcd (a, b))));
}

/* Compute the greatest common denominator of two numbers using
   Euclid's algorithm.  */

static int 
gcd (int a, int b)
{
  
  int x, y, z;
  
  x = abs (a);
  y = abs (b);

  while (x>0)
    {
      z = y % x;
      y = x;
      x = z;
    }

  return (y);
}

/* Returns true iff A divides B.  */

static inline bool 
int_divides_p (int a, int b)
{
  return ((b % a) == 0);
}



/* Dump into FILE all the data references from DATAREFS.  */ 

void 
dump_data_references (FILE *file, 
		      varray_type datarefs)
{
  unsigned int i;
  
  for (i = 0; i < VARRAY_ACTIVE_SIZE (datarefs); i++)
    dump_data_reference (file, VARRAY_GENERIC_PTR (datarefs, i));
}

/* Dump into FILE all the dependence relations from DDR.  */ 

void 
dump_data_dependence_relations (FILE *file, 
				varray_type ddr)
{
  unsigned int i;
  
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ddr); i++)
    dump_data_dependence_relation (file, VARRAY_GENERIC_PTR (ddr, i));
}

/* Dump function for a DATA_REFERENCE structure.  */

void 
dump_data_reference (FILE *outf, 
		     struct data_reference *dr)
{
  unsigned int i;
  
  fprintf (outf, "(Data Ref: \n  stmt: ");
  print_generic_stmt (outf, DR_STMT (dr), 0);
  fprintf (outf, "  ref: ");
  print_generic_stmt (outf, DR_REF (dr), 0);
  fprintf (outf, "  base_name: ");
  print_generic_stmt (outf, DR_BASE_NAME (dr), 0);
  
  for (i = 0; i < DR_NUM_DIMENSIONS (dr); i++)
    {
      fprintf (outf, "  Access function %d: ", i);
      print_generic_stmt (outf, DR_ACCESS_FN (dr, i), 0);
    }
  fprintf (outf, ")\n");
}

/* Dump function for a SUBSCRIPT structure.  */

void 
dump_subscript (FILE *outf, struct subscript *subscript)
{
  tree chrec = SUB_CONFLICTS_IN_A (subscript);

  fprintf (outf, "\n (subscript \n");
  fprintf (outf, "  iterations_that_access_an_element_twice_in_A: ");
  print_generic_stmt (outf, chrec, 0);
  if (chrec == chrec_known)
    fprintf (outf, "    (no dependence)\n");
  else if (chrec_contains_undetermined (chrec))
    fprintf (outf, "    (don't know)\n");
  else
    {
      tree last_iteration = SUB_LAST_CONFLICT (subscript);
      fprintf (outf, "  last_conflict: ");
      print_generic_stmt (outf, last_iteration, 0);
    }
	  
  chrec = SUB_CONFLICTS_IN_B (subscript);
  fprintf (outf, "  iterations_that_access_an_element_twice_in_B: ");
  print_generic_stmt (outf, chrec, 0);
  if (chrec == chrec_known)
    fprintf (outf, "    (no dependence)\n");
  else if (chrec_contains_undetermined (chrec))
    fprintf (outf, "    (don't know)\n");
  else
    {
      tree last_iteration = SUB_LAST_CONFLICT (subscript);
      fprintf (outf, "  last_conflict: ");
      print_generic_stmt (outf, last_iteration, 0);
    }

  fprintf (outf, "  (Subscript distance: ");
  print_generic_stmt (outf, SUB_DISTANCE (subscript), 0);
  fprintf (outf, "  )\n");
  fprintf (outf, " )\n");
}

/* Dump function for a DATA_DEPENDENCE_RELATION structure.  */

void 
dump_data_dependence_relation (FILE *outf, 
			       struct data_dependence_relation *ddr)
{
  struct data_reference *dra, *drb;

  dra = DDR_A (ddr);
  drb = DDR_B (ddr);
  fprintf (outf, "(Data Dep: \n");
  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    fprintf (outf, "    (don't know)\n");
  
  else if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    fprintf (outf, "    (no dependence)\n");
  
  else if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
    {
      unsigned int i;
      for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
	{
	  fprintf (outf, "  access_fn_A: ");
	  print_generic_stmt (outf, DR_ACCESS_FN (dra, i), 0);
	  fprintf (outf, "  access_fn_B: ");
	  print_generic_stmt (outf, DR_ACCESS_FN (drb, i), 0);
	  dump_subscript (outf, DDR_SUBSCRIPT (ddr, i));
	}
      if (DDR_DIST_VECT (ddr))
	{
	  fprintf (outf, "  distance_vect: ");
	  print_lambda_vector (outf, DDR_DIST_VECT (ddr), DDR_SIZE_VECT (ddr));
	}
      if (DDR_DIR_VECT (ddr))
	{
	  fprintf (outf, "  direction_vect: ");
	  print_lambda_vector (outf, DDR_DIR_VECT (ddr), DDR_SIZE_VECT (ddr));
	}
    }

  fprintf (outf, ")\n");
}



/* Dump function for a DATA_DEPENDENCE_DIRECTION structure.  */

void
dump_data_dependence_direction (FILE *file, 
				enum data_dependence_direction dir)
{
  switch (dir)
    {
    case dir_positive: 
      fprintf (file, "+");
      break;
      
    case dir_negative:
      fprintf (file, "-");
      break;
      
    case dir_equal:
      fprintf (file, "=");
      break;
      
    case dir_positive_or_negative:
      fprintf (file, "+-");
      break;
      
    case dir_positive_or_equal: 
      fprintf (file, "+=");
      break;
      
    case dir_negative_or_equal: 
      fprintf (file, "-=");
      break;
      
    case dir_star: 
      fprintf (file, "*"); 
      break;
      
    default: 
      break;
    }
}

/* Dumps the distance and direction vectors in FILE.  DDRS contains
   the dependence relations, and VECT_SIZE is the size of the
   dependence vectors, or in other words the number of loops in the
   considered nest.  */

void 
dump_dist_dir_vectors (FILE *file, varray_type ddrs)
{
  unsigned int i;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ddrs); i++)
    {
      struct data_dependence_relation *ddr = 
	(struct data_dependence_relation *) 
	VARRAY_GENERIC_PTR (ddrs, i);
      if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE
	  && DDR_AFFINE_P (ddr))
	{
	  fprintf (file, "DISTANCE_V (");
	  print_lambda_vector (file, DDR_DIST_VECT (ddr), DDR_SIZE_VECT (ddr));
	  fprintf (file, ")\n");
	  fprintf (file, "DIRECTION_V (");
	  print_lambda_vector (file, DDR_DIR_VECT (ddr), DDR_SIZE_VECT (ddr));
	  fprintf (file, ")\n");
	}
    }
  fprintf (file, "\n\n");
}

/* Dumps the data dependence relations DDRS in FILE.  */

void 
dump_ddrs (FILE *file, varray_type ddrs)
{
  unsigned int i;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ddrs); i++)
    {
      struct data_dependence_relation *ddr = 
	(struct data_dependence_relation *) 
	VARRAY_GENERIC_PTR (ddrs, i);
      dump_data_dependence_relation (file, ddr);
    }
  fprintf (file, "\n\n");
}



/* Compute the lowest iteration bound for LOOP.  It is an
   INTEGER_CST.  */

static void
compute_estimated_nb_iterations (struct loop *loop)
{
  tree estimation;
  struct nb_iter_bound *bound, *next;
  
  for (bound = loop->bounds; bound; bound = next)
    {
      next = bound->next;
      estimation = bound->bound;

      if (TREE_CODE (estimation) != INTEGER_CST)
	continue;

      if (loop->estimated_nb_iterations)
	{
	  /* Update only if estimation is smaller.  */
	  if (tree_int_cst_lt (estimation, loop->estimated_nb_iterations))
	    loop->estimated_nb_iterations = estimation;
	}
      else
	loop->estimated_nb_iterations = estimation;
    }
}

/* Estimate the number of iterations from the size of the data and the
   access functions.  */

static void
estimate_niter_from_size_of_data (struct loop *loop, 
				  tree opnd0, 
				  tree access_fn, 
				  tree stmt)
{
  tree estimation;
  tree array_size, data_size, element_size;
  tree init, step;

  init = initial_condition (access_fn);
  step = evolution_part_in_loop_num (access_fn, loop->num);

  array_size = TYPE_SIZE (TREE_TYPE (opnd0));
  element_size = TYPE_SIZE (TREE_TYPE (TREE_TYPE (opnd0)));
  if (array_size == NULL_TREE 
      || TREE_CODE (array_size) != INTEGER_CST
      || TREE_CODE (element_size) != INTEGER_CST)
    return;

  data_size = fold (build2 (EXACT_DIV_EXPR, integer_type_node, 
			    array_size, element_size));

  if (init != NULL_TREE
      && step != NULL_TREE
      && TREE_CODE (init) == INTEGER_CST
      && TREE_CODE (step) == INTEGER_CST)
    {
      estimation = fold (build2 (CEIL_DIV_EXPR, integer_type_node, 
				 fold (build2 (MINUS_EXPR, integer_type_node, 
					       data_size, init)), step));

      record_estimate (loop, estimation, boolean_true_node, stmt);
    }
}

/* Given an ARRAY_REF node REF, records its access functions.
   Example: given A[i][3], record in ACCESS_FNS the opnd1 function,
   i.e. the constant "3", then recursively call the function on opnd0,
   i.e. the ARRAY_REF "A[i]".  The function returns the base name:
   "A".  */

static tree
analyze_array_indexes (struct loop *loop,
		       varray_type *access_fns, 
		       tree ref, tree stmt)
{
  tree opnd0, opnd1;
  tree access_fn;
  
  opnd0 = TREE_OPERAND (ref, 0);
  opnd1 = TREE_OPERAND (ref, 1);
  
  /* The detection of the evolution function for this data access is
     postponed until the dependence test.  This lazy strategy avoids
     the computation of access functions that are of no interest for
     the optimizers.  */
  access_fn = instantiate_parameters 
    (loop, analyze_scalar_evolution (loop, opnd1));

  if (loop->estimated_nb_iterations == NULL_TREE)
    estimate_niter_from_size_of_data (loop, opnd0, access_fn, stmt);
  
  VARRAY_PUSH_TREE (*access_fns, access_fn);
  
  /* Recursively record other array access functions.  */
  if (TREE_CODE (opnd0) == ARRAY_REF)
    return analyze_array_indexes (loop, access_fns, opnd0, stmt);
  
  /* Return the base name of the data access.  */
  else
    return opnd0;
}

/* For a data reference REF contained in the statement STMT, initialize
   a DATA_REFERENCE structure, and return it.  IS_READ flag has to be
   set to true when REF is in the right hand side of an
   assignment.  */

struct data_reference *
analyze_array (tree stmt, tree ref, bool is_read)
{
  struct data_reference *res;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(analyze_array \n");
      fprintf (dump_file, "  (ref = ");
      print_generic_stmt (dump_file, ref, 0);
      fprintf (dump_file, ")\n");
    }
  
  res = xmalloc (sizeof (struct data_reference));
  
  DR_STMT (res) = stmt;
  DR_REF (res) = ref;
  VARRAY_TREE_INIT (DR_ACCESS_FNS (res), 3, "access_fns");
  DR_BASE_NAME (res) = analyze_array_indexes 
    (loop_containing_stmt (stmt), &(DR_ACCESS_FNS (res)), ref, stmt);
  DR_IS_READ (res) = is_read;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
  
  return res;
}

/* For a data reference REF contained in the statement STMT, initialize
   a DATA_REFERENCE structure, and return it.  */

struct data_reference *
init_data_ref (tree stmt, 
	       tree ref,
	       tree base,
	       tree access_fn,
	       bool is_read)
{
  struct data_reference *res;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(init_data_ref \n");
      fprintf (dump_file, "  (ref = ");
      print_generic_stmt (dump_file, ref, 0);
      fprintf (dump_file, ")\n");
    }
  
  res = xmalloc (sizeof (struct data_reference));
  
  DR_STMT (res) = stmt;
  DR_REF (res) = ref;
  VARRAY_TREE_INIT (DR_ACCESS_FNS (res), 5, "access_fns");
  DR_BASE_NAME (res) = base;
  VARRAY_PUSH_TREE (DR_ACCESS_FNS (res), access_fn);
  DR_IS_READ (res) = is_read;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
  
  return res;
}



/* Returns true when all the functions of a tree_vec CHREC are the
   same.  */

static bool 
all_chrecs_equal_p (tree chrec)
{
  int j;

  for (j = 0; j < TREE_VEC_LENGTH (chrec) - 1; j++)
    {
      tree chrec_j = TREE_VEC_ELT (chrec, j);
      tree chrec_j_1 = TREE_VEC_ELT (chrec, j + 1);
      if (!integer_zerop 
	  (chrec_fold_minus 
	   (integer_type_node, chrec_j, chrec_j_1)))
	return false;
    }
  return true;
}

/* Determine for each subscript in the data dependence relation DDR
   the distance.  */

static void
compute_subscript_distance (struct data_dependence_relation *ddr)
{
  if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
    {
      unsigned int i;
      
      for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
 	{
 	  tree conflicts_a, conflicts_b, difference;
 	  struct subscript *subscript;
 	  
 	  subscript = DDR_SUBSCRIPT (ddr, i);
 	  conflicts_a = SUB_CONFLICTS_IN_A (subscript);
 	  conflicts_b = SUB_CONFLICTS_IN_B (subscript);

	  if (TREE_CODE (conflicts_a) == TREE_VEC)
	    {
	      if (!all_chrecs_equal_p (conflicts_a))
		{
		  SUB_DISTANCE (subscript) = chrec_dont_know;
		  return;
		}
	      else
		conflicts_a = TREE_VEC_ELT (conflicts_a, 0);
	    }

	  if (TREE_CODE (conflicts_b) == TREE_VEC)
	    {
	      if (!all_chrecs_equal_p (conflicts_b))
		{
		  SUB_DISTANCE (subscript) = chrec_dont_know;
		  return;
		}
	      else
		conflicts_b = TREE_VEC_ELT (conflicts_b, 0);
	    }

	  difference = chrec_fold_minus 
	    (integer_type_node, conflicts_b, conflicts_a);
 	  
 	  if (evolution_function_is_constant_p (difference))
 	    SUB_DISTANCE (subscript) = difference;
 	  
 	  else
 	    SUB_DISTANCE (subscript) = chrec_dont_know;
 	}
    }
}

/* Initialize a ddr.  */

struct data_dependence_relation *
initialize_data_dependence_relation (struct data_reference *a, 
				     struct data_reference *b)
{
  struct data_dependence_relation *res;
  bool differ_p;
  
  res = xmalloc (sizeof (struct data_dependence_relation));
  DDR_A (res) = a;
  DDR_B (res) = b;

  if (a == NULL || b == NULL 
      || DR_BASE_NAME (a) == NULL_TREE
      || DR_BASE_NAME (b) == NULL_TREE)
    DDR_ARE_DEPENDENT (res) = chrec_dont_know;    

  /* When the dimensions of A and B differ, we directly initialize
     the relation to "there is no dependence": chrec_known.  */
  else if (DR_NUM_DIMENSIONS (a) != DR_NUM_DIMENSIONS (b)
	   || (array_base_name_differ_p (a, b, &differ_p) && differ_p))
    DDR_ARE_DEPENDENT (res) = chrec_known;
  
  else
    {
      unsigned int i;
      DDR_AFFINE_P (res) = true;
      DDR_ARE_DEPENDENT (res) = NULL_TREE;
      DDR_SUBSCRIPTS_VECTOR_INIT (res, DR_NUM_DIMENSIONS (a));
      DDR_SIZE_VECT (res) = 0;
      DDR_DIST_VECT (res) = NULL;
      DDR_DIR_VECT (res) = NULL;
      
      for (i = 0; i < DR_NUM_DIMENSIONS (a); i++)
	{
	  struct subscript *subscript;
	  
	  subscript = xmalloc (sizeof (struct subscript));
	  SUB_CONFLICTS_IN_A (subscript) = chrec_dont_know;
	  SUB_CONFLICTS_IN_B (subscript) = chrec_dont_know;
	  SUB_LAST_CONFLICT (subscript) = chrec_dont_know;
	  SUB_DISTANCE (subscript) = chrec_dont_know;
	  VARRAY_PUSH_GENERIC_PTR (DDR_SUBSCRIPTS (res), subscript);
	}
    }
  
  return res;
}

/* Set DDR_ARE_DEPENDENT to CHREC and finalize the subscript overlap
   description.  */

static inline void
finalize_ddr_dependent (struct data_dependence_relation *ddr, 
			tree chrec)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(dependence classified: ");
      print_generic_expr (dump_file, chrec, 0);
      fprintf (dump_file, ")\n");
    }

  DDR_ARE_DEPENDENT (ddr) = chrec;  
  varray_clear (DDR_SUBSCRIPTS (ddr));
}

/* The dependence relation DDR cannot be represented by a distance
   vector.  */

static inline void
non_affine_dependence_relation (struct data_dependence_relation *ddr)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(Dependence relation cannot be represented by distance vector.) \n");

  DDR_AFFINE_P (ddr) = false;
}



/* This section contains the classic Banerjee tests.  */

/* Returns true iff CHREC_A and CHREC_B are not dependent on any index
   variables, i.e., if the ZIV (Zero Index Variable) test is true.  */

static inline bool
ziv_subscript_p (tree chrec_a, 
		 tree chrec_b)
{
  return (evolution_function_is_constant_p (chrec_a)
	  && evolution_function_is_constant_p (chrec_b));
}

/* Returns true iff CHREC_A and CHREC_B are dependent on an index
   variable, i.e., if the SIV (Single Index Variable) test is true.  */

static bool
siv_subscript_p (tree chrec_a,
		 tree chrec_b)
{
  if ((evolution_function_is_constant_p (chrec_a)
       && evolution_function_is_univariate_p (chrec_b))
      || (evolution_function_is_constant_p (chrec_b)
	  && evolution_function_is_univariate_p (chrec_a)))
    return true;
  
  if (evolution_function_is_univariate_p (chrec_a)
      && evolution_function_is_univariate_p (chrec_b))
    {
      switch (TREE_CODE (chrec_a))
	{
	case POLYNOMIAL_CHREC:
	  switch (TREE_CODE (chrec_b))
	    {
	    case POLYNOMIAL_CHREC:
	      if (CHREC_VARIABLE (chrec_a) != CHREC_VARIABLE (chrec_b))
		return false;
	      
	    default:
	      return true;
	    }
	  
	default:
	  return true;
	}
    }
  
  return false;
}

/* Analyze a ZIV (Zero Index Variable) subscript.  *OVERLAPS_A and
   *OVERLAPS_B are initialized to the functions that describe the
   relation between the elements accessed twice by CHREC_A and
   CHREC_B.  For k >= 0, the following property is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void 
analyze_ziv_subscript (tree chrec_a, 
		       tree chrec_b, 
		       tree *overlaps_a,
		       tree *overlaps_b, 
		       tree *last_conflicts)
{
  tree difference;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_ziv_subscript \n");
  
  difference = chrec_fold_minus (integer_type_node, chrec_a, chrec_b);
  
  switch (TREE_CODE (difference))
    {
    case INTEGER_CST:
      if (integer_zerop (difference))
	{
	  /* The difference is equal to zero: the accessed index
	     overlaps for each iteration in the loop.  */
	  *overlaps_a = integer_zero_node;
	  *overlaps_b = integer_zero_node;
	  *last_conflicts = chrec_dont_know;
	}
      else
	{
	  /* The accesses do not overlap.  */
	  *overlaps_a = chrec_known;
	  *overlaps_b = chrec_known;
	  *last_conflicts = integer_zero_node;
	}
      break;
      
    default:
      /* We're not sure whether the indexes overlap.  For the moment, 
	 conservatively answer "don't know".  */
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
      *last_conflicts = chrec_dont_know;
      break;
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Analyze a SIV (Single Index Variable) subscript where CHREC_A is a
   constant, and CHREC_B is an affine function.  *OVERLAPS_A and
   *OVERLAPS_B are initialized to the functions that describe the
   relation between the elements accessed twice by CHREC_A and
   CHREC_B.  For k >= 0, the following property is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void
analyze_siv_subscript_cst_affine (tree chrec_a, 
				  tree chrec_b,
				  tree *overlaps_a, 
				  tree *overlaps_b, 
				  tree *last_conflicts)
{
  bool value0, value1, value2;
  tree difference = chrec_fold_minus 
    (integer_type_node, CHREC_LEFT (chrec_b), chrec_a);
  
  if (!chrec_is_positive (initial_condition (difference), &value0))
    {
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
      *last_conflicts = chrec_dont_know;
      return;
    }
  else
    {
      if (value0 == false)
	{
	  if (!chrec_is_positive (CHREC_RIGHT (chrec_b), &value1))
	    {
	      *overlaps_a = chrec_dont_know;
	      *overlaps_b = chrec_dont_know;      
	      *last_conflicts = chrec_dont_know;
	      return;
	    }
	  else
	    {
	      if (value1 == true)
		{
		  /* Example:  
		     chrec_a = 12
		     chrec_b = {10, +, 1}
		  */
		  
		  if (tree_fold_divides_p 
		      (integer_type_node, CHREC_RIGHT (chrec_b), difference))
		    {
		      *overlaps_a = integer_zero_node;
		      *overlaps_b = fold 
			(build (EXACT_DIV_EXPR, integer_type_node, 
				fold (build1 (ABS_EXPR, integer_type_node, difference)), 
				CHREC_RIGHT (chrec_b)));
		      *last_conflicts = integer_one_node;
		      return;
		    }
		  
		  /* When the step does not divides the difference, there are
		     no overlaps.  */
		  else
		    {
		      *overlaps_a = chrec_known;
		      *overlaps_b = chrec_known;      
		      *last_conflicts = integer_zero_node;
		      return;
		    }
		}
	      
	      else
		{
		  /* Example:  
		     chrec_a = 12
		     chrec_b = {10, +, -1}
		     
		     In this case, chrec_a will not overlap with chrec_b.  */
		  *overlaps_a = chrec_known;
		  *overlaps_b = chrec_known;
		  *last_conflicts = integer_zero_node;
		  return;
		}
	    }
	}
      else 
	{
	  if (!chrec_is_positive (CHREC_RIGHT (chrec_b), &value2))
	    {
	      *overlaps_a = chrec_dont_know;
	      *overlaps_b = chrec_dont_know;      
	      *last_conflicts = chrec_dont_know;
	      return;
	    }
	  else
	    {
	      if (value2 == false)
		{
		  /* Example:  
		     chrec_a = 3
		     chrec_b = {10, +, -1}
		  */
		  if (tree_fold_divides_p 
		      (integer_type_node, CHREC_RIGHT (chrec_b), difference))
		    {
		      *overlaps_a = integer_zero_node;
		      *overlaps_b = fold 
			(build (EXACT_DIV_EXPR, integer_type_node, difference, 
				CHREC_RIGHT (chrec_b)));
		      *last_conflicts = integer_one_node;
		      return;
		    }
		  
		  /* When the step does not divides the difference, there
		     are no overlaps.  */
		  else
		    {
		      *overlaps_a = chrec_known;
		      *overlaps_b = chrec_known;      
		      *last_conflicts = integer_zero_node;
		      return;
		    }
		}
	      else
		{
		  /* Example:  
		     chrec_a = 3  
		     chrec_b = {4, +, 1}
		 
		     In this case, chrec_a will not overlap with chrec_b.  */
		  *overlaps_a = chrec_known;
		  *overlaps_b = chrec_known;
		  *last_conflicts = integer_zero_node;
		  return;
		}
	    }
	}
    }
}

/* Helper recursive function for initializing the matrix A.  Returns
   the initial value of CHREC.  */

static int
initialize_matrix_A (lambda_matrix A, tree chrec, unsigned index, int mult)
{
  gcc_assert (chrec);

  if (TREE_CODE (chrec) != POLYNOMIAL_CHREC)
    return int_cst_value (chrec);

  A[index][0] = mult * int_cst_value (CHREC_RIGHT (chrec));
  return initialize_matrix_A (A, CHREC_LEFT (chrec), index + 1, mult);
}

#define FLOOR_DIV(x,y) ((x) / (y))

/* Solves the special case of the Diophantine equation: 
   | {0, +, STEP_A}_x (OVERLAPS_A) = {0, +, STEP_B}_y (OVERLAPS_B)

   Computes the descriptions OVERLAPS_A and OVERLAPS_B.  NITER is the
   number of iterations that loops X and Y run.  The overlaps will be
   constructed as evolutions in dimension DIM.  */

static void
compute_overlap_steps_for_affine_univar (int niter, int step_a, int step_b, 
					 tree *overlaps_a, tree *overlaps_b, 
					 tree *last_conflicts, int dim)
{
  if (((step_a > 0 && step_b > 0)
       || (step_a < 0 && step_b < 0)))
    {
      int step_overlaps_a, step_overlaps_b;
      int gcd_steps_a_b, last_conflict, tau2;

      gcd_steps_a_b = gcd (step_a, step_b);
      step_overlaps_a = step_b / gcd_steps_a_b;
      step_overlaps_b = step_a / gcd_steps_a_b;

      tau2 = FLOOR_DIV (niter, step_overlaps_a);
      tau2 = MIN (tau2, FLOOR_DIV (niter, step_overlaps_b));
      last_conflict = tau2;

      *overlaps_a = build_polynomial_chrec
	(dim, integer_zero_node,
	 build_int_cst (NULL_TREE, step_overlaps_a));
      *overlaps_b = build_polynomial_chrec
	(dim, integer_zero_node,
	 build_int_cst (NULL_TREE, step_overlaps_b));
      *last_conflicts = build_int_cst (NULL_TREE, last_conflict);
    }

  else
    {
      *overlaps_a = integer_zero_node;
      *overlaps_b = integer_zero_node;
      *last_conflicts = integer_zero_node;
    }
}


/* Solves the special case of a Diophantine equation where CHREC_A is
   an affine bivariate function, and CHREC_B is an affine univariate
   function.  For example, 

   | {{0, +, 1}_x, +, 1335}_y = {0, +, 1336}_z
   
   has the following overlapping functions: 

   | x (t, u, v) = {{0, +, 1336}_t, +, 1}_v
   | y (t, u, v) = {{0, +, 1336}_u, +, 1}_v
   | z (t, u, v) = {{{0, +, 1}_t, +, 1335}_u, +, 1}_v

   FORNOW: This is a specialized implementation for a case occurring in
   a common benchmark.  Implement the general algorithm.  */

static void
compute_overlap_steps_for_affine_1_2 (tree chrec_a, tree chrec_b, 
				      tree *overlaps_a, tree *overlaps_b, 
				      tree *last_conflicts)
{
  bool xz_p, yz_p, xyz_p;
  int step_x, step_y, step_z;
  int niter_x, niter_y, niter_z, niter;
  tree numiter_x, numiter_y, numiter_z;
  tree overlaps_a_xz, overlaps_b_xz, last_conflicts_xz;
  tree overlaps_a_yz, overlaps_b_yz, last_conflicts_yz;
  tree overlaps_a_xyz, overlaps_b_xyz, last_conflicts_xyz;

  step_x = int_cst_value (CHREC_RIGHT (CHREC_LEFT (chrec_a)));
  step_y = int_cst_value (CHREC_RIGHT (chrec_a));
  step_z = int_cst_value (CHREC_RIGHT (chrec_b));

  numiter_x = number_of_iterations_in_loop 
    (current_loops->parray[CHREC_VARIABLE (CHREC_LEFT (chrec_a))]);
  numiter_y = number_of_iterations_in_loop 
    (current_loops->parray[CHREC_VARIABLE (chrec_a)]);
  numiter_z = number_of_iterations_in_loop 
    (current_loops->parray[CHREC_VARIABLE (chrec_b)]);

  if (TREE_CODE (numiter_x) != INTEGER_CST)
    numiter_x = current_loops->parray[CHREC_VARIABLE (CHREC_LEFT (chrec_a))]
      ->estimated_nb_iterations;
  if (TREE_CODE (numiter_y) != INTEGER_CST)
    numiter_y = current_loops->parray[CHREC_VARIABLE (chrec_a)]
      ->estimated_nb_iterations;
  if (TREE_CODE (numiter_z) != INTEGER_CST)
    numiter_z = current_loops->parray[CHREC_VARIABLE (chrec_b)]
      ->estimated_nb_iterations;

  if (numiter_x == NULL_TREE || numiter_y == NULL_TREE 
      || numiter_z == NULL_TREE)
    {
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
      *last_conflicts = chrec_dont_know;
      return;
    }

  niter_x = int_cst_value (numiter_x);
  niter_y = int_cst_value (numiter_y);
  niter_z = int_cst_value (numiter_z);

  niter = MIN (niter_x, niter_z);
  compute_overlap_steps_for_affine_univar (niter, step_x, step_z,
					   &overlaps_a_xz,
					   &overlaps_b_xz,
					   &last_conflicts_xz, 1);
  niter = MIN (niter_y, niter_z);
  compute_overlap_steps_for_affine_univar (niter, step_y, step_z,
					   &overlaps_a_yz,
					   &overlaps_b_yz,
					   &last_conflicts_yz, 2);
  niter = MIN (niter_x, niter_z);
  niter = MIN (niter_y, niter);
  compute_overlap_steps_for_affine_univar (niter, step_x + step_y, step_z,
					   &overlaps_a_xyz,
					   &overlaps_b_xyz,
					   &last_conflicts_xyz, 3);

  xz_p = !integer_zerop (last_conflicts_xz);
  yz_p = !integer_zerop (last_conflicts_yz);
  xyz_p = !integer_zerop (last_conflicts_xyz);

  if (xz_p || yz_p || xyz_p)
    {
      *overlaps_a = make_tree_vec (2);
      TREE_VEC_ELT (*overlaps_a, 0) = integer_zero_node;
      TREE_VEC_ELT (*overlaps_a, 1) = integer_zero_node;
      *overlaps_b = integer_zero_node;
      if (xz_p)
	{
	  TREE_VEC_ELT (*overlaps_a, 0) = 
	    chrec_fold_plus (integer_type_node, TREE_VEC_ELT (*overlaps_a, 0),
			     overlaps_a_xz);
	  *overlaps_b = 
	    chrec_fold_plus (integer_type_node, *overlaps_b, overlaps_b_xz);
	  *last_conflicts = last_conflicts_xz;
	}
      if (yz_p)
	{
	  TREE_VEC_ELT (*overlaps_a, 1) = 
	    chrec_fold_plus (integer_type_node, TREE_VEC_ELT (*overlaps_a, 1),
			     overlaps_a_yz);
	  *overlaps_b = 
	    chrec_fold_plus (integer_type_node, *overlaps_b, overlaps_b_yz);
	  *last_conflicts = last_conflicts_yz;
	}
      if (xyz_p)
	{
	  TREE_VEC_ELT (*overlaps_a, 0) = 
	    chrec_fold_plus (integer_type_node, TREE_VEC_ELT (*overlaps_a, 0),
			     overlaps_a_xyz);
	  TREE_VEC_ELT (*overlaps_a, 1) = 
	    chrec_fold_plus (integer_type_node, TREE_VEC_ELT (*overlaps_a, 1),
			     overlaps_a_xyz);
	  *overlaps_b = 
	    chrec_fold_plus (integer_type_node, *overlaps_b, overlaps_b_xyz);
	  *last_conflicts = last_conflicts_xyz;
	}
    }
  else
    {
      *overlaps_a = integer_zero_node;
      *overlaps_b = integer_zero_node;
      *last_conflicts = integer_zero_node;
    }
}

/* Determines the overlapping elements due to accesses CHREC_A and
   CHREC_B, that are affine functions.  This is a part of the
   subscript analyzer.  */

static void
analyze_subscript_affine_affine (tree chrec_a, 
				 tree chrec_b,
				 tree *overlaps_a, 
				 tree *overlaps_b, 
				 tree *last_conflicts)
{
  unsigned nb_vars_a, nb_vars_b, dim;
  int init_a, init_b, gamma, gcd_alpha_beta;
  int tau1, tau2;
  lambda_matrix A, U, S;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_subscript_affine_affine \n");
  
  /* For determining the initial intersection, we have to solve a
     Diophantine equation.  This is the most time consuming part.
     
     For answering to the question: "Is there a dependence?" we have
     to prove that there exists a solution to the Diophantine
     equation, and that the solution is in the iteration domain,
     i.e. the solution is positive or zero, and that the solution
     happens before the upper bound loop.nb_iterations.  Otherwise
     there is no dependence.  This function outputs a description of
     the iterations that hold the intersections.  */

  
  nb_vars_a = nb_vars_in_chrec (chrec_a);
  nb_vars_b = nb_vars_in_chrec (chrec_b);

  dim = nb_vars_a + nb_vars_b;
  U = lambda_matrix_new (dim, dim);
  A = lambda_matrix_new (dim, 1);
  S = lambda_matrix_new (dim, 1);

  init_a = initialize_matrix_A (A, chrec_a, 0, 1);
  init_b = initialize_matrix_A (A, chrec_b, nb_vars_a, -1);
  gamma = init_b - init_a;

  /* Don't do all the hard work of solving the Diophantine equation
     when we already know the solution: for example, 
     | {3, +, 1}_1
     | {3, +, 4}_2
     | gamma = 3 - 3 = 0.
     Then the first overlap occurs during the first iterations: 
     | {3, +, 1}_1 ({0, +, 4}_x) = {3, +, 4}_2 ({0, +, 1}_x)
  */
  if (gamma == 0)
    {
      if (nb_vars_a == 1 && nb_vars_b == 1)
	{
	  int step_a, step_b;
	  int niter, niter_a, niter_b;
	  tree numiter_a, numiter_b;

	  numiter_a = number_of_iterations_in_loop 
	    (current_loops->parray[CHREC_VARIABLE (chrec_a)]);
	  numiter_b = number_of_iterations_in_loop 
	    (current_loops->parray[CHREC_VARIABLE (chrec_b)]);

	  if (TREE_CODE (numiter_a) != INTEGER_CST)
	    numiter_a = current_loops->parray[CHREC_VARIABLE (chrec_a)]
	      ->estimated_nb_iterations;
	  if (TREE_CODE (numiter_b) != INTEGER_CST)
	    numiter_b = current_loops->parray[CHREC_VARIABLE (chrec_b)]
	      ->estimated_nb_iterations;
	  if (numiter_a == NULL_TREE || numiter_b == NULL_TREE)
	    {
	      *overlaps_a = chrec_dont_know;
	      *overlaps_b = chrec_dont_know;
	      *last_conflicts = chrec_dont_know;
	      return;
	    }

	  niter_a = int_cst_value (numiter_a);
	  niter_b = int_cst_value (numiter_b);
	  niter = MIN (niter_a, niter_b);

	  step_a = int_cst_value (CHREC_RIGHT (chrec_a));
	  step_b = int_cst_value (CHREC_RIGHT (chrec_b));

	  compute_overlap_steps_for_affine_univar (niter, step_a, step_b, 
						   overlaps_a, overlaps_b, 
						   last_conflicts, 1);
	}

      else if (nb_vars_a == 2 && nb_vars_b == 1)
	compute_overlap_steps_for_affine_1_2
	  (chrec_a, chrec_b, overlaps_a, overlaps_b, last_conflicts);

      else if (nb_vars_a == 1 && nb_vars_b == 2)
	compute_overlap_steps_for_affine_1_2
	  (chrec_b, chrec_a, overlaps_b, overlaps_a, last_conflicts);

      else
	{
	  *overlaps_a = chrec_dont_know;
	  *overlaps_b = chrec_dont_know;
	  *last_conflicts = chrec_dont_know;
	}
      return;
    }

  /* U.A = S */
  lambda_matrix_right_hermite (A, dim, 1, S, U);

  if (S[0][0] < 0)
    {
      S[0][0] *= -1;
      lambda_matrix_row_negate (U, dim, 0);
    }
  gcd_alpha_beta = S[0][0];

  /* The classic "gcd-test".  */
  if (!int_divides_p (gcd_alpha_beta, gamma))
    {
      /* The "gcd-test" has determined that there is no integer
	 solution, i.e. there is no dependence.  */
      *overlaps_a = chrec_known;
      *overlaps_b = chrec_known;
      *last_conflicts = integer_zero_node;
    }

  /* Both access functions are univariate.  This includes SIV and MIV cases.  */
  else if (nb_vars_a == 1 && nb_vars_b == 1)
    {
      /* Both functions should have the same evolution sign.  */
      if (((A[0][0] > 0 && -A[1][0] > 0)
	   || (A[0][0] < 0 && -A[1][0] < 0)))
	{
	  /* The solutions are given by:
	     | 
	     | [GAMMA/GCD_ALPHA_BETA  t].[u11 u12]  = [x0]
	     |                           [u21 u22]    [y0]
	 
	     For a given integer t.  Using the following variables,
	 
	     | i0 = u11 * gamma / gcd_alpha_beta
	     | j0 = u12 * gamma / gcd_alpha_beta
	     | i1 = u21
	     | j1 = u22
	 
	     the solutions are:
	 
	     | x0 = i0 + i1 * t, 
	     | y0 = j0 + j1 * t.  */
      
	  int i0, j0, i1, j1;

	  /* X0 and Y0 are the first iterations for which there is a
	     dependence.  X0, Y0 are two solutions of the Diophantine
	     equation: chrec_a (X0) = chrec_b (Y0).  */
	  int x0, y0;
	  int niter, niter_a, niter_b;
	  tree numiter_a, numiter_b;

	  numiter_a = number_of_iterations_in_loop 
	    (current_loops->parray[CHREC_VARIABLE (chrec_a)]);
	  numiter_b = number_of_iterations_in_loop 
	    (current_loops->parray[CHREC_VARIABLE (chrec_b)]);

	  if (TREE_CODE (numiter_a) != INTEGER_CST)
	    numiter_a = current_loops->parray[CHREC_VARIABLE (chrec_a)]
	      ->estimated_nb_iterations;
	  if (TREE_CODE (numiter_b) != INTEGER_CST)
	    numiter_b = current_loops->parray[CHREC_VARIABLE (chrec_b)]
	      ->estimated_nb_iterations;
	  if (numiter_a == NULL_TREE || numiter_b == NULL_TREE)
	    {
	      *overlaps_a = chrec_dont_know;
	      *overlaps_b = chrec_dont_know;
	      *last_conflicts = chrec_dont_know;
	      return;
	    }

	  niter_a = int_cst_value (numiter_a);
	  niter_b = int_cst_value (numiter_b);
	  niter = MIN (niter_a, niter_b);

	  i0 = U[0][0] * gamma / gcd_alpha_beta;
	  j0 = U[0][1] * gamma / gcd_alpha_beta;
	  i1 = U[1][0];
	  j1 = U[1][1];

	  if ((i1 == 0 && i0 < 0)
	      || (j1 == 0 && j0 < 0))
	    {
	      /* There is no solution.  
		 FIXME: The case "i0 > nb_iterations, j0 > nb_iterations" 
		 falls in here, but for the moment we don't look at the 
		 upper bound of the iteration domain.  */
	      *overlaps_a = chrec_known;
	      *overlaps_b = chrec_known;
	      *last_conflicts = integer_zero_node;
	    }

	  else 
	    {
	      if (i1 > 0)
		{
		  tau1 = CEIL (-i0, i1);
		  tau2 = FLOOR_DIV (niter - i0, i1);

		  if (j1 > 0)
		    {
		      int last_conflict, min_multiple;
		      tau1 = MAX (tau1, CEIL (-j0, j1));
		      tau2 = MIN (tau2, FLOOR_DIV (niter - j0, j1));

		      x0 = i1 * tau1 + i0;
		      y0 = j1 * tau1 + j0;

		      /* At this point (x0, y0) is one of the
			 solutions to the Diophantine equation.  The
			 next step has to compute the smallest
			 positive solution: the first conflicts.  */
		      min_multiple = MIN (x0 / i1, y0 / j1);
		      x0 -= i1 * min_multiple;
		      y0 -= j1 * min_multiple;

		      tau1 = (x0 - i0)/i1;
		      last_conflict = tau2 - tau1;

		      *overlaps_a = build_polynomial_chrec
			(1,
			 build_int_cst (NULL_TREE, x0),
			 build_int_cst (NULL_TREE, i1));
		      *overlaps_b = build_polynomial_chrec
			(1,
			 build_int_cst (NULL_TREE, y0),
			 build_int_cst (NULL_TREE, j1));
		      *last_conflicts = build_int_cst (NULL_TREE, last_conflict);
		    }
		  else
		    {
		      /* FIXME: For the moment, the upper bound of the
			 iteration domain for j is not checked.  */
		      *overlaps_a = chrec_dont_know;
		      *overlaps_b = chrec_dont_know;
		      *last_conflicts = chrec_dont_know;
		    }
		}
	  
	      else
		{
		  /* FIXME: For the moment, the upper bound of the
		     iteration domain for i is not checked.  */
		  *overlaps_a = chrec_dont_know;
		  *overlaps_b = chrec_dont_know;
		  *last_conflicts = chrec_dont_know;
		}
	    }
	}
      else
	{
	  *overlaps_a = chrec_dont_know;
	  *overlaps_b = chrec_dont_know;
	  *last_conflicts = chrec_dont_know;
	}
    }

  else
    {
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
      *last_conflicts = chrec_dont_know;
    }


  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (overlaps_a = ");
      print_generic_expr (dump_file, *overlaps_a, 0);
      fprintf (dump_file, ")\n  (overlaps_b = ");
      print_generic_expr (dump_file, *overlaps_b, 0);
      fprintf (dump_file, ")\n");
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Analyze a SIV (Single Index Variable) subscript.  *OVERLAPS_A and
   *OVERLAPS_B are initialized to the functions that describe the
   relation between the elements accessed twice by CHREC_A and
   CHREC_B.  For k >= 0, the following property is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void
analyze_siv_subscript (tree chrec_a, 
		       tree chrec_b,
		       tree *overlaps_a, 
		       tree *overlaps_b, 
		       tree *last_conflicts)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_siv_subscript \n");
  
  if (evolution_function_is_constant_p (chrec_a)
      && evolution_function_is_affine_p (chrec_b))
    analyze_siv_subscript_cst_affine (chrec_a, chrec_b, 
				      overlaps_a, overlaps_b, last_conflicts);
  
  else if (evolution_function_is_affine_p (chrec_a)
	   && evolution_function_is_constant_p (chrec_b))
    analyze_siv_subscript_cst_affine (chrec_b, chrec_a, 
				      overlaps_b, overlaps_a, last_conflicts);
  
  else if (evolution_function_is_affine_p (chrec_a)
	   && evolution_function_is_affine_p (chrec_b))
    analyze_subscript_affine_affine (chrec_a, chrec_b, 
				     overlaps_a, overlaps_b, last_conflicts);
  else
    {
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
      *last_conflicts = chrec_dont_know;
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Return true when the evolution steps of an affine CHREC divide the
   constant CST.  */

static bool
chrec_steps_divide_constant_p (tree chrec, 
			       tree cst)
{
  switch (TREE_CODE (chrec))
    {
    case POLYNOMIAL_CHREC:
      return (tree_fold_divides_p (integer_type_node, CHREC_RIGHT (chrec), cst)
	      && chrec_steps_divide_constant_p (CHREC_LEFT (chrec), cst));
      
    default:
      /* On the initial condition, return true.  */
      return true;
    }
}

/* Analyze a MIV (Multiple Index Variable) subscript.  *OVERLAPS_A and
   *OVERLAPS_B are initialized to the functions that describe the
   relation between the elements accessed twice by CHREC_A and
   CHREC_B.  For k >= 0, the following property is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void
analyze_miv_subscript (tree chrec_a, 
		       tree chrec_b, 
		       tree *overlaps_a, 
		       tree *overlaps_b, 
		       tree *last_conflicts)
{
  /* FIXME:  This is a MIV subscript, not yet handled.
     Example: (A[{1, +, 1}_1] vs. A[{1, +, 1}_2]) that comes from 
     (A[i] vs. A[j]).  
     
     In the SIV test we had to solve a Diophantine equation with two
     variables.  In the MIV case we have to solve a Diophantine
     equation with 2*n variables (if the subscript uses n IVs).
  */
  tree difference;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_miv_subscript \n");
  
  difference = chrec_fold_minus (integer_type_node, chrec_a, chrec_b);
  
  if (chrec_zerop (difference))
    {
      /* Access functions are the same: all the elements are accessed
	 in the same order.  */
      *overlaps_a = integer_zero_node;
      *overlaps_b = integer_zero_node;
      *last_conflicts = number_of_iterations_in_loop 
	(current_loops->parray[CHREC_VARIABLE (chrec_a)]);
    }
  
  else if (evolution_function_is_constant_p (difference)
	   /* For the moment, the following is verified:
	      evolution_function_is_affine_multivariate_p (chrec_a) */
	   && !chrec_steps_divide_constant_p (chrec_a, difference))
    {
      /* testsuite/.../ssa-chrec-33.c
	 {{21, +, 2}_1, +, -2}_2  vs.  {{20, +, 2}_1, +, -2}_2 
        
	 The difference is 1, and the evolution steps are equal to 2,
	 consequently there are no overlapping elements.  */
      *overlaps_a = chrec_known;
      *overlaps_b = chrec_known;
      *last_conflicts = integer_zero_node;
    }
  
  else if (evolution_function_is_affine_multivariate_p (chrec_a)
	   && evolution_function_is_affine_multivariate_p (chrec_b))
    {
      /* testsuite/.../ssa-chrec-35.c
	 {0, +, 1}_2  vs.  {0, +, 1}_3
	 the overlapping elements are respectively located at iterations:
	 {0, +, 1}_x and {0, +, 1}_x, 
	 in other words, we have the equality: 
	 {0, +, 1}_2 ({0, +, 1}_x) = {0, +, 1}_3 ({0, +, 1}_x)
	 
	 Other examples: 
	 {{0, +, 1}_1, +, 2}_2 ({0, +, 1}_x, {0, +, 1}_y) = 
	 {0, +, 1}_1 ({{0, +, 1}_x, +, 2}_y)

	 {{0, +, 2}_1, +, 3}_2 ({0, +, 1}_y, {0, +, 1}_x) = 
	 {{0, +, 3}_1, +, 2}_2 ({0, +, 1}_x, {0, +, 1}_y)
      */
      analyze_subscript_affine_affine (chrec_a, chrec_b, 
				       overlaps_a, overlaps_b, last_conflicts);
    }
  
  else
    {
      /* When the analysis is too difficult, answer "don't know".  */
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
      *last_conflicts = chrec_dont_know;
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Determines the iterations for which CHREC_A is equal to CHREC_B.
   OVERLAP_ITERATIONS_A and OVERLAP_ITERATIONS_B are initialized with
   two functions that describe the iterations that contain conflicting
   elements.
   
   Remark: For an integer k >= 0, the following equality is true:
   
   CHREC_A (OVERLAP_ITERATIONS_A (k)) == CHREC_B (OVERLAP_ITERATIONS_B (k)).
*/

static void 
analyze_overlapping_iterations (tree chrec_a, 
				tree chrec_b, 
				tree *overlap_iterations_a, 
				tree *overlap_iterations_b, 
				tree *last_conflicts)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(analyze_overlapping_iterations \n");
      fprintf (dump_file, "  (chrec_a = ");
      print_generic_expr (dump_file, chrec_a, 0);
      fprintf (dump_file, ")\n  chrec_b = ");
      print_generic_expr (dump_file, chrec_b, 0);
      fprintf (dump_file, ")\n");
    }
  
  if (chrec_a == NULL_TREE
      || chrec_b == NULL_TREE
      || chrec_contains_undetermined (chrec_a)
      || chrec_contains_undetermined (chrec_b)
      || chrec_contains_symbols (chrec_a)
      || chrec_contains_symbols (chrec_b))
    {
      *overlap_iterations_a = chrec_dont_know;
      *overlap_iterations_b = chrec_dont_know;
    }
  
  else if (ziv_subscript_p (chrec_a, chrec_b))
    analyze_ziv_subscript (chrec_a, chrec_b, 
			   overlap_iterations_a, overlap_iterations_b,
			   last_conflicts);
  
  else if (siv_subscript_p (chrec_a, chrec_b))
    analyze_siv_subscript (chrec_a, chrec_b, 
			   overlap_iterations_a, overlap_iterations_b, 
			   last_conflicts);
  
  else
    analyze_miv_subscript (chrec_a, chrec_b, 
			   overlap_iterations_a, overlap_iterations_b,
			   last_conflicts);
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (overlap_iterations_a = ");
      print_generic_expr (dump_file, *overlap_iterations_a, 0);
      fprintf (dump_file, ")\n  (overlap_iterations_b = ");
      print_generic_expr (dump_file, *overlap_iterations_b, 0);
      fprintf (dump_file, ")\n");
    }
}



/* This section contains the affine functions dependences detector.  */

/* Computes the conflicting iterations, and initialize DDR.  */

static void
subscript_dependence_tester (struct data_dependence_relation *ddr)
{
  unsigned int i;
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  tree last_conflicts;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(subscript_dependence_tester \n");
  
  for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
    {
      tree overlaps_a, overlaps_b;
      struct subscript *subscript = DDR_SUBSCRIPT (ddr, i);
      
      analyze_overlapping_iterations (DR_ACCESS_FN (dra, i), 
				      DR_ACCESS_FN (drb, i),
				      &overlaps_a, &overlaps_b, 
				      &last_conflicts);
      
      if (chrec_contains_undetermined (overlaps_a)
 	  || chrec_contains_undetermined (overlaps_b))
 	{
 	  finalize_ddr_dependent (ddr, chrec_dont_know);
	  break;
 	}
      
      else if (overlaps_a == chrec_known
 	       || overlaps_b == chrec_known)
 	{
 	  finalize_ddr_dependent (ddr, chrec_known);
 	  break;
 	}
      
      else
 	{
 	  SUB_CONFLICTS_IN_A (subscript) = overlaps_a;
 	  SUB_CONFLICTS_IN_B (subscript) = overlaps_b;
	  SUB_LAST_CONFLICT (subscript) = last_conflicts;
 	}
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Compute the classic per loop distance vector.

   DDR is the data dependence relation to build a vector from.
   NB_LOOPS is the total number of loops we are considering.
   FIRST_LOOP_DEPTH is the loop->depth of the first loop in the analyzed
   loop nest.  
   Return FALSE if the dependence relation is outside of the loop nest
   starting at FIRST_LOOP_DEPTH. 
   Return TRUE otherwise.  */

static bool
build_classic_dist_vector (struct data_dependence_relation *ddr, 
			   int nb_loops, int first_loop_depth)
{
  unsigned i;
  lambda_vector dist_v, init_v;
  
  dist_v = lambda_vector_new (nb_loops);
  init_v = lambda_vector_new (nb_loops);
  lambda_vector_clear (dist_v, nb_loops);
  lambda_vector_clear (init_v, nb_loops);
  
  if (DDR_ARE_DEPENDENT (ddr) != NULL_TREE)
    return true;
  
  for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
    {
      tree access_fn_a, access_fn_b;
      struct subscript *subscript = DDR_SUBSCRIPT (ddr, i);

      if (chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	{
	  non_affine_dependence_relation (ddr);
	  return true;
	}

      access_fn_a = DR_ACCESS_FN (DDR_A (ddr), i);
      access_fn_b = DR_ACCESS_FN (DDR_B (ddr), i);

      if (TREE_CODE (access_fn_a) == POLYNOMIAL_CHREC 
	  && TREE_CODE (access_fn_b) == POLYNOMIAL_CHREC)
	{
	  int dist, loop_nb, loop_depth;
	  int loop_nb_a = CHREC_VARIABLE (access_fn_a);
	  int loop_nb_b = CHREC_VARIABLE (access_fn_b);
	  struct loop *loop_a = current_loops->parray[loop_nb_a];
	  struct loop *loop_b = current_loops->parray[loop_nb_b];

	  /* If the loop for either variable is at a lower depth than 
	     the first_loop's depth, then we can't possibly have a
	     dependency at this level of the loop.  */
	     
	  if (loop_a->depth < first_loop_depth
	      || loop_b->depth < first_loop_depth)
	    return false;

	  if (loop_nb_a != loop_nb_b
	      && !flow_loop_nested_p (loop_a, loop_b)
	      && !flow_loop_nested_p (loop_b, loop_a))
	    {
	      /* Example: when there are two consecutive loops,

		 | loop_1
		 |   A[{0, +, 1}_1]
		 | endloop_1
		 | loop_2
		 |   A[{0, +, 1}_2]
		 | endloop_2

		 the dependence relation cannot be captured by the
		 distance abstraction.  */
	      non_affine_dependence_relation (ddr);
	      return true;
	    }

	  /* The dependence is carried by the outermost loop.  Example:
	     | loop_1
	     |   A[{4, +, 1}_1]
	     |   loop_2
	     |     A[{5, +, 1}_2]
	     |   endloop_2
	     | endloop_1
	     In this case, the dependence is carried by loop_1.  */
	  loop_nb = loop_nb_a < loop_nb_b ? loop_nb_a : loop_nb_b;
	  loop_depth = current_loops->parray[loop_nb]->depth - first_loop_depth;

	  /* If the loop number is still greater than the number of
	     loops we've been asked to analyze, or negative,
	     something is borked.  */
	  gcc_assert (loop_depth >= 0);
	  gcc_assert (loop_depth < nb_loops);
	  if (chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	    {
	      non_affine_dependence_relation (ddr);
	      return true;
	    }
	  
	  dist = int_cst_value (SUB_DISTANCE (subscript));

	  /* This is the subscript coupling test.  
	     | loop i = 0, N, 1
	     |   T[i+1][i] = ...
	     |   ... = T[i][i]
	     | endloop
	     There is no dependence.  */
	  if (init_v[loop_depth] != 0
	      && dist_v[loop_depth] != dist)
	    {
	      finalize_ddr_dependent (ddr, chrec_known);
	      return true;
	    }

	  dist_v[loop_depth] = dist;
	  init_v[loop_depth] = 1;
	}
    }
  
  /* There is a distance of 1 on all the outer loops: 
     
     Example: there is a dependence of distance 1 on loop_1 for the array A.
     | loop_1
     |   A[5] = ...
     | endloop
  */
  {
    struct loop *lca, *loop_a, *loop_b;
    struct data_reference *a = DDR_A (ddr);
    struct data_reference *b = DDR_B (ddr);
    int lca_depth;
    loop_a = loop_containing_stmt (DR_STMT (a));
    loop_b = loop_containing_stmt (DR_STMT (b));
    
    /* Get the common ancestor loop.  */
    lca = find_common_loop (loop_a, loop_b); 
    
    lca_depth = lca->depth;
    lca_depth -= first_loop_depth;
    gcc_assert (lca_depth >= 0);
    gcc_assert (lca_depth < nb_loops);

    /* For each outer loop where init_v is not set, the accesses are
       in dependence of distance 1 in the loop.  */
    if (lca != loop_a
	&& lca != loop_b
	&& init_v[lca_depth] == 0)
      dist_v[lca_depth] = 1;
    
    lca = lca->outer;
    
    if (lca)
      {
	lca_depth = lca->depth - first_loop_depth;
	while (lca->depth != 0)
	  {
	    /* If we're considering just a sub-nest, then don't record
	       any information on the outer loops.  */
	    if (lca_depth < 0)
	      break;

	    gcc_assert (lca_depth < nb_loops);

	    if (init_v[lca_depth] == 0)
	      dist_v[lca_depth] = 1;
	    lca = lca->outer;
	    lca_depth = lca->depth - first_loop_depth;
	  
	  }
      }
  }
  
  DDR_DIST_VECT (ddr) = dist_v;
  DDR_SIZE_VECT (ddr) = nb_loops;
  return true;
}

/* Compute the classic per loop direction vector.  

   DDR is the data dependence relation to build a vector from.
   NB_LOOPS is the total number of loops we are considering.
   FIRST_LOOP_DEPTH is the loop->depth of the first loop in the analyzed 
   loop nest.
   Return FALSE if the dependence relation is outside of the loop nest
   at FIRST_LOOP_DEPTH. 
   Return TRUE otherwise.  */

static bool
build_classic_dir_vector (struct data_dependence_relation *ddr, 
			  int nb_loops, int first_loop_depth)
{
  unsigned i;
  lambda_vector dir_v, init_v;
  
  dir_v = lambda_vector_new (nb_loops);
  init_v = lambda_vector_new (nb_loops);
  lambda_vector_clear (dir_v, nb_loops);
  lambda_vector_clear (init_v, nb_loops);
  
  if (DDR_ARE_DEPENDENT (ddr) != NULL_TREE)
    return true;
  
  for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
    {
      tree access_fn_a, access_fn_b;
      struct subscript *subscript = DDR_SUBSCRIPT (ddr, i);

      if (chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	{
	  non_affine_dependence_relation (ddr);
	  return true;
	}

      access_fn_a = DR_ACCESS_FN (DDR_A (ddr), i);
      access_fn_b = DR_ACCESS_FN (DDR_B (ddr), i);
      if (TREE_CODE (access_fn_a) == POLYNOMIAL_CHREC
	  && TREE_CODE (access_fn_b) == POLYNOMIAL_CHREC)
	{
	  int dist, loop_nb, loop_depth;
	  enum data_dependence_direction dir = dir_star;
	  int loop_nb_a = CHREC_VARIABLE (access_fn_a);
	  int loop_nb_b = CHREC_VARIABLE (access_fn_b);
	  struct loop *loop_a = current_loops->parray[loop_nb_a];
	  struct loop *loop_b = current_loops->parray[loop_nb_b];
 
	  /* If the loop for either variable is at a lower depth than 
	     the first_loop's depth, then we can't possibly have a
	     dependency at this level of the loop.  */
	     
	  if (loop_a->depth < first_loop_depth
	      || loop_b->depth < first_loop_depth)
	    return false;

	  if (loop_nb_a != loop_nb_b
	      && !flow_loop_nested_p (loop_a, loop_b)
	      && !flow_loop_nested_p (loop_b, loop_a))
	    {
	      /* Example: when there are two consecutive loops,

		 | loop_1
		 |   A[{0, +, 1}_1]
		 | endloop_1
		 | loop_2
		 |   A[{0, +, 1}_2]
		 | endloop_2

		 the dependence relation cannot be captured by the
		 distance abstraction.  */
	      non_affine_dependence_relation (ddr);
	      return true;
	    }

	  /* The dependence is carried by the outermost loop.  Example:
	     | loop_1
	     |   A[{4, +, 1}_1]
	     |   loop_2
	     |     A[{5, +, 1}_2]
	     |   endloop_2
	     | endloop_1
	     In this case, the dependence is carried by loop_1.  */
	  loop_nb = loop_nb_a < loop_nb_b ? loop_nb_a : loop_nb_b;
	  loop_depth = current_loops->parray[loop_nb]->depth - first_loop_depth;

	  /* If the loop number is still greater than the number of
	     loops we've been asked to analyze, or negative,
	     something is borked.  */
	  gcc_assert (loop_depth >= 0);
	  gcc_assert (loop_depth < nb_loops);

	  if (chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	    {
	      non_affine_dependence_relation (ddr);
	      return true;
	    }

	  dist = int_cst_value (SUB_DISTANCE (subscript));

	  if (dist == 0)
	    dir = dir_equal;
	  else if (dist > 0)
	    dir = dir_positive;
	  else if (dist < 0)
	    dir = dir_negative;
	  
	  /* This is the subscript coupling test.  
	     | loop i = 0, N, 1
	     |   T[i+1][i] = ...
	     |   ... = T[i][i]
	     | endloop
	     There is no dependence.  */
	  if (init_v[loop_depth] != 0
	      && dir != dir_star
	      && (enum data_dependence_direction) dir_v[loop_depth] != dir
	      && (enum data_dependence_direction) dir_v[loop_depth] != dir_star)
	    {
	      finalize_ddr_dependent (ddr, chrec_known);
	      return true;
	    }
	  
	  dir_v[loop_depth] = dir;
	  init_v[loop_depth] = 1;
	}
    }
  
  /* There is a distance of 1 on all the outer loops: 
     
     Example: there is a dependence of distance 1 on loop_1 for the array A.
     | loop_1
     |   A[5] = ...
     | endloop
  */
  {
    struct loop *lca, *loop_a, *loop_b;
    struct data_reference *a = DDR_A (ddr);
    struct data_reference *b = DDR_B (ddr);
    int lca_depth;
    loop_a = loop_containing_stmt (DR_STMT (a));
    loop_b = loop_containing_stmt (DR_STMT (b));
    
    /* Get the common ancestor loop.  */
    lca = find_common_loop (loop_a, loop_b); 
    lca_depth = lca->depth - first_loop_depth;

    gcc_assert (lca_depth >= 0);
    gcc_assert (lca_depth < nb_loops);

    /* For each outer loop where init_v is not set, the accesses are
       in dependence of distance 1 in the loop.  */
    if (lca != loop_a
	&& lca != loop_b
	&& init_v[lca_depth] == 0)
      dir_v[lca_depth] = dir_positive;
    
    lca = lca->outer;
    if (lca)
      {
	lca_depth = lca->depth - first_loop_depth;
	while (lca->depth != 0)
	  {
	    /* If we're considering just a sub-nest, then don't record
	       any information on the outer loops.  */
	    if (lca_depth < 0)
	      break;

	    gcc_assert (lca_depth < nb_loops);

	    if (init_v[lca_depth] == 0)
	      dir_v[lca_depth] = dir_positive;
	    lca = lca->outer;
	    lca_depth = lca->depth - first_loop_depth;
	   
	  }
      }
  }
  
  DDR_DIR_VECT (ddr) = dir_v;
  DDR_SIZE_VECT (ddr) = nb_loops;
  return true;
}

/* Returns true when all the access functions of A are affine or
   constant.  */

static bool 
access_functions_are_affine_or_constant_p (struct data_reference *a)
{
  unsigned int i;
  varray_type fns = DR_ACCESS_FNS (a);
  
  for (i = 0; i < VARRAY_ACTIVE_SIZE (fns); i++)
    if (!evolution_function_is_constant_p (VARRAY_TREE (fns, i))
	&& !evolution_function_is_affine_multivariate_p (VARRAY_TREE (fns, i)))
      return false;
  
  return true;
}

/* This computes the affine dependence relation between A and B.
   CHREC_KNOWN is used for representing the independence between two
   accesses, while CHREC_DONT_KNOW is used for representing the unknown
   relation.
   
   Note that it is possible to stop the computation of the dependence
   relation the first time we detect a CHREC_KNOWN element for a given
   subscript.  */

void
compute_affine_dependence (struct data_dependence_relation *ddr)
{
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(compute_affine_dependence\n");
      fprintf (dump_file, "  (stmt_a = \n");
      print_generic_expr (dump_file, DR_STMT (dra), 0);
      fprintf (dump_file, ")\n  (stmt_b = \n");
      print_generic_expr (dump_file, DR_STMT (drb), 0);
      fprintf (dump_file, ")\n");
    }
  
  /* Analyze only when the dependence relation is not yet known.  */
  if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
    {
      if (access_functions_are_affine_or_constant_p (dra)
	  && access_functions_are_affine_or_constant_p (drb))
	subscript_dependence_tester (ddr);
      
      /* As a last case, if the dependence cannot be determined, or if
	 the dependence is considered too difficult to determine, answer
	 "don't know".  */
      else
	finalize_ddr_dependent (ddr, chrec_dont_know);
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Compute a subset of the data dependence relation graph.  Don't
   compute read-read relations, and avoid the computation of the
   opposite relation, i.e. when AB has been computed, don't compute BA.
   DATAREFS contains a list of data references, and the result is set
   in DEPENDENCE_RELATIONS.  */

static void 
compute_all_dependences (varray_type datarefs, 
			 varray_type *dependence_relations)
{
  unsigned int i, j, N;

  N = VARRAY_ACTIVE_SIZE (datarefs);

  for (i = 0; i < N; i++)
    for (j = i; j < N; j++)
      {
	struct data_reference *a, *b;
	struct data_dependence_relation *ddr;

	a = VARRAY_GENERIC_PTR (datarefs, i);
	b = VARRAY_GENERIC_PTR (datarefs, j);
	ddr = initialize_data_dependence_relation (a, b);

	VARRAY_PUSH_GENERIC_PTR (*dependence_relations, ddr);
	compute_affine_dependence (ddr);
	compute_subscript_distance (ddr);
      }
}

/* Search the data references in LOOP, and record the information into
   DATAREFS.  Returns chrec_dont_know when failing to analyze a
   difficult case, returns NULL_TREE otherwise.
   
   TODO: This function should be made smarter so that it can handle address
   arithmetic as if they were array accesses, etc.  */

tree 
find_data_references_in_loop (struct loop *loop, varray_type *datarefs)
{
  bool dont_know_node_not_inserted = true;
  basic_block bb, *bbs;
  unsigned int i;
  block_stmt_iterator bsi;

  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  tree stmt = bsi_stmt (bsi);
	  stmt_ann_t ann = stmt_ann (stmt);

	  if (TREE_CODE (stmt) != MODIFY_EXPR)
	    continue;

	  if (!VUSE_OPS (ann)
	      && !V_MUST_DEF_OPS (ann)
	      && !V_MAY_DEF_OPS (ann))
	    continue;
	  
	  /* In the GIMPLE representation, a modify expression
  	     contains a single load or store to memory.  */
	  if (TREE_CODE (TREE_OPERAND (stmt, 0)) == ARRAY_REF)
	    VARRAY_PUSH_GENERIC_PTR 
		    (*datarefs, analyze_array (stmt, TREE_OPERAND (stmt, 0), 
					       false));

	  else if (TREE_CODE (TREE_OPERAND (stmt, 1)) == ARRAY_REF)
	    VARRAY_PUSH_GENERIC_PTR 
		    (*datarefs, analyze_array (stmt, TREE_OPERAND (stmt, 1), 
					       true));
  	  else
	    {
	      if (dont_know_node_not_inserted)
		{
		  struct data_reference *res;
		  res = xmalloc (sizeof (struct data_reference));
		  DR_STMT (res) = NULL_TREE;
		  DR_REF (res) = NULL_TREE;
		  DR_ACCESS_FNS (res) = NULL;
		  DR_BASE_NAME (res) = NULL;
		  DR_IS_READ (res) = false;
		  VARRAY_PUSH_GENERIC_PTR (*datarefs, res);
		  dont_know_node_not_inserted = false;
		}
	    }

	  /* When there are no defs in the loop, the loop is parallel.  */
	  if (NUM_V_MAY_DEFS (STMT_V_MAY_DEF_OPS (stmt)) > 0
	      || NUM_V_MUST_DEFS (STMT_V_MUST_DEF_OPS (stmt)) > 0)
	    bb->loop_father->parallel_p = false;
	}

      if (bb->loop_father->estimated_nb_iterations == NULL_TREE)
	compute_estimated_nb_iterations (bb->loop_father);
    }

  free (bbs);

  return dont_know_node_not_inserted ? NULL_TREE : chrec_dont_know;
}



/* This section contains all the entry points.  */

/* Given a loop nest LOOP, the following vectors are returned:
   *DATAREFS is initialized to all the array elements contained in this loop, 
   *DEPENDENCE_RELATIONS contains the relations between the data references.  */

void
compute_data_dependences_for_loop (unsigned nb_loops, 
				   struct loop *loop,
				   varray_type *datarefs,
				   varray_type *dependence_relations)
{
  unsigned int i;
  varray_type allrelations;

  /* If one of the data references is not computable, give up without
     spending time to compute other dependences.  */
  if (find_data_references_in_loop (loop, datarefs) == chrec_dont_know)
    {
      struct data_dependence_relation *ddr;

      /* Insert a single relation into dependence_relations:
	 chrec_dont_know.  */
      ddr = initialize_data_dependence_relation (NULL, NULL);
      VARRAY_PUSH_GENERIC_PTR (*dependence_relations, ddr);
      build_classic_dist_vector (ddr, nb_loops, loop->depth);
      build_classic_dir_vector (ddr, nb_loops, loop->depth);
      return;
    }

  VARRAY_GENERIC_PTR_INIT (allrelations, 1, "Data dependence relations");
  compute_all_dependences (*datarefs, &allrelations);

  for (i = 0; i < VARRAY_ACTIVE_SIZE (allrelations); i++)
    {
      struct data_dependence_relation *ddr;
      ddr = VARRAY_GENERIC_PTR (allrelations, i);
      if (build_classic_dist_vector (ddr, nb_loops, loop->depth))
	{
	  VARRAY_PUSH_GENERIC_PTR (*dependence_relations, ddr);
	  build_classic_dir_vector (ddr, nb_loops, loop->depth);
	}
    }
}

/* Entry point (for testing only).  Analyze all the data references
   and the dependence relations.

   The data references are computed first.  
   
   A relation on these nodes is represented by a complete graph.  Some
   of the relations could be of no interest, thus the relations can be
   computed on demand.
   
   In the following function we compute all the relations.  This is
   just a first implementation that is here for:
   - for showing how to ask for the dependence relations, 
   - for the debugging the whole dependence graph,
   - for the dejagnu testcases and maintenance.
   
   It is possible to ask only for a part of the graph, avoiding to
   compute the whole dependence graph.  The computed dependences are
   stored in a knowledge base (KB) such that later queries don't
   recompute the same information.  The implementation of this KB is
   transparent to the optimizer, and thus the KB can be changed with a
   more efficient implementation, or the KB could be disabled.  */

void 
analyze_all_data_dependences (struct loops *loops)
{
  unsigned int i;
  varray_type datarefs;
  varray_type dependence_relations;
  int nb_data_refs = 10;

  VARRAY_GENERIC_PTR_INIT (datarefs, nb_data_refs, "datarefs");
  VARRAY_GENERIC_PTR_INIT (dependence_relations, 
			   nb_data_refs * nb_data_refs,
			   "dependence_relations");

  /* Compute DDs on the whole function.  */
  compute_data_dependences_for_loop (loops->num, loops->parray[0], 
				     &datarefs, &dependence_relations);

  if (dump_file)
    {
      dump_data_dependence_relations (dump_file, dependence_relations);
      fprintf (dump_file, "\n\n");

      if (dump_flags & TDF_DETAILS)
	dump_dist_dir_vectors (dump_file, dependence_relations);

      if (dump_flags & TDF_STATS)
	{
	  unsigned nb_top_relations = 0;
	  unsigned nb_bot_relations = 0;
	  unsigned nb_basename_differ = 0;
	  unsigned nb_chrec_relations = 0;

	  for (i = 0; i < VARRAY_ACTIVE_SIZE (dependence_relations); i++)
	    {
	      struct data_dependence_relation *ddr;
	      ddr = VARRAY_GENERIC_PTR (dependence_relations, i);
	  
	      if (chrec_contains_undetermined (DDR_ARE_DEPENDENT (ddr)))
		nb_top_relations++;
	  
	      else if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
		{
		  struct data_reference *a = DDR_A (ddr);
		  struct data_reference *b = DDR_B (ddr);
		  bool differ_p;	
	      
		  if (DR_NUM_DIMENSIONS (a) != DR_NUM_DIMENSIONS (b)
		      || (array_base_name_differ_p (a, b, &differ_p) && differ_p))
		    nb_basename_differ++;
		  else
		    nb_bot_relations++;
		}
	  
	      else 
		nb_chrec_relations++;
	    }
      
	  gather_stats_on_scev_database ();
	}
    }

  free_dependence_relations (dependence_relations);
  free_data_refs (datarefs);
}

/* Free the memory used by a data dependence relation DDR.  */

void
free_dependence_relation (struct data_dependence_relation *ddr)
{
  if (ddr == NULL)
    return;

  if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE && DDR_SUBSCRIPTS (ddr))
    varray_clear (DDR_SUBSCRIPTS (ddr));
  free (ddr);
}

/* Free the memory used by the data dependence relations from
   DEPENDENCE_RELATIONS.  */

void 
free_dependence_relations (varray_type dependence_relations)
{
  unsigned int i;
  if (dependence_relations == NULL)
    return;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (dependence_relations); i++)
    free_dependence_relation (VARRAY_GENERIC_PTR (dependence_relations, i));
  varray_clear (dependence_relations);
}

/* Free the memory used by the data references from DATAREFS.  */

void
free_data_refs (varray_type datarefs)
{
  unsigned int i;
  
  if (datarefs == NULL)
    return;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (datarefs); i++)
    {
      struct data_reference *dr = (struct data_reference *) 
	VARRAY_GENERIC_PTR (datarefs, i);
      if (dr)
	{
	  if (DR_ACCESS_FNS (dr))
	    varray_clear (DR_ACCESS_FNS (dr));
	  free (dr);
	}
    }
  varray_clear (datarefs);
}

