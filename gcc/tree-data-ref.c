/* Data references and dependences detectors.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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
     
   - to define a knowledge base for storing the data dependeces 
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
#include "lambda.h"

static unsigned int data_ref_id = 0;



/* Returns true iff A divides B.  */

static inline bool 
tree_fold_divides_p (tree type, 
		     tree a, 
		     tree b)
{
  if (integer_onep (a))
    return true;
  
  /* Determines whether (A == gcd (A, B)).  */
  return integer_zerop 
    (fold (build (MINUS_EXPR, type, a, tree_fold_gcd (a, b))));
}

/* Bezout: Let A1 and A2 be two integers; there exist two integers U11
   and U12 such that, 
   
   |  U11 * A1 + U12 * A2 = gcd (A1, A2).
   
   This function computes the greatest common divisor using the
   Blankinship algorithm.  The gcd is returned, and the coefficients
   of the unimodular matrix U are (U11, U12, U21, U22) such that, 

   |  U.A = S
   
   |  (U11 U12) (A1) = (gcd)
   |  (U21 U22) (A2)   (0)
   
   FIXME: Use lambda_..._hermite for implementing this function.
*/

static tree 
tree_fold_bezout (tree a1, 
		  tree a2,
		  tree *u11, tree *u12,
		  tree *u21, tree *u22)
{
  tree s1, s2;
  
  /* Initialize S with the coefficients of A.  */
  s1 = a1;
  s2 = a2;
  
  /* Initialize the U matrix */
  *u11 = integer_one_node; 
  *u12 = integer_zero_node;
  *u21 = integer_zero_node;
  *u22 = integer_one_node;
  
  if (integer_zerop (a1)
      || integer_zerop (a2))
    return integer_zero_node;
  
  while (!integer_zerop (s2))
    {
      int sign;
      tree z, zu21, zu22, zs2;
      
      sign = tree_int_cst_sgn (s1) * tree_int_cst_sgn (s2);
      z = fold (build (FLOOR_DIV_EXPR, integer_type_node, 
		       fold (build1 (ABS_EXPR, integer_type_node, s1)), 
		       fold (build1 (ABS_EXPR, integer_type_node, s2))));
      zu21 = fold (build (MULT_EXPR, integer_type_node, z, *u21));
      zu22 = fold (build (MULT_EXPR, integer_type_node, z, *u22));
      zs2 = fold (build (MULT_EXPR, integer_type_node, z, s2));
      
      /* row1 -= z * row2.  */
      if (sign < 0)
	{
	  *u11 = fold (build (PLUS_EXPR, integer_type_node, *u11, zu21));
	  *u12 = fold (build (PLUS_EXPR, integer_type_node, *u12, zu22));
	  s1 = fold (build (PLUS_EXPR, integer_type_node, s1, zs2));
	}
      else if (sign > 0)
	{
	  *u11 = fold (build (MINUS_EXPR, integer_type_node, *u11, zu21));
	  *u12 = fold (build (MINUS_EXPR, integer_type_node, *u12, zu22));
	  s1 = fold (build (MINUS_EXPR, integer_type_node, s1, zs2));
	}
      else
	/* Should not happen.  */
	abort ();
      
      /* Interchange row1 and row2.  */
      {
	tree flip;
	
	flip = *u11;
	*u11 = *u21;
	*u21 = flip;

	flip = *u12;
	*u12 = *u22;
	*u22 = flip;
	
	flip = s1;
	s1 = s2;
	s2 = flip;
      }
    }
  
  if (tree_int_cst_sgn (s1) < 0)
    {
      *u11 = fold (build (MULT_EXPR, integer_type_node, *u11, 
			  integer_minus_one_node));
      *u12 = fold (build (MULT_EXPR, integer_type_node, *u12, 
				 integer_minus_one_node));
      s1 = fold (build (MULT_EXPR, integer_type_node, s1, integer_minus_one_node));
    }
  
  return s1;
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
  
  fprintf (outf, "(Data Ref %d: \n  stmt: ", DR_ID (dr));
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

/* Dump function for a DATA_DEPENDENCE_RELATION structure.  */

void 
dump_data_dependence_relation (FILE *outf, 
			       struct data_dependence_relation *ddr)
{
  unsigned int i;
  struct data_reference *dra, *drb;
  
  dra = DDR_A (ddr);
  drb = DDR_B (ddr);
  
  if (dra && drb)
    fprintf (outf, "(Data Dep (A = %d, B = %d):", DR_ID (dra), DR_ID (drb));
  else
    fprintf (outf, "(Data Dep:");

  if (chrec_contains_undetermined (DDR_ARE_DEPENDENT (ddr)))
    fprintf (outf, "    (don't know)\n");
  
  else if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    fprintf (outf, "    (no dependence)\n");
  
  else
    {
      for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
	{
	  tree chrec;
	  struct subscript *subscript = DDR_SUBSCRIPT (ddr, i);
	  
	  fprintf (outf, "\n (subscript %d:\n", i);
	  fprintf (outf, "  access_fn_A: ");
	  print_generic_stmt (outf, DR_ACCESS_FN (dra, i), 0);
	  fprintf (outf, "  access_fn_B: ");
	  print_generic_stmt (outf, DR_ACCESS_FN (drb, i), 0);
	  
	  chrec = SUB_CONFLICTS_IN_A (subscript);
	  fprintf (outf, "  iterations_that_access_an_element_twice_in_A: ");
	  print_generic_stmt (outf, chrec, 0);
	  if (chrec == chrec_known)
	    fprintf (outf, "    (no dependence)\n");
	  else if (chrec_contains_undetermined (chrec))
	    fprintf (outf, "    (don't know)\n");
	  else
	    {
	      tree last_iteration = SUB_LAST_CONFLICT_IN_A (subscript);
	      fprintf (outf, "  last_iteration_that_access_an_element_twice_in_A: ");
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
	      tree last_iteration = SUB_LAST_CONFLICT_IN_B (subscript);
	      fprintf (outf, "  last_iteration_that_access_an_element_twice_in_B: ");
	      print_generic_stmt (outf, last_iteration, 0);
	    }
      
	  fprintf (outf, " )\n");
	}
  
      fprintf (outf, " (Distance Vector: \n");
      for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
	{
	  struct subscript *subscript = DDR_SUBSCRIPT (ddr, i);
      
	  fprintf (outf, "(");
	  print_generic_stmt (outf, SUB_DISTANCE (subscript), 0);
	  fprintf (outf, ")\n");
	}
      fprintf (outf, " )\n");
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



/* Given an ARRAY_REF node REF, records its access functions.
   Example: given A[i][3], record in ACCESS_FNS the opnd1 function,
   ie. the constant "3", then recursively call the function on opnd0,
   ie. the ARRAY_REF "A[i]".  The function returns the base name:
   "A".  */

static tree
analyze_array_indexes (struct loop *loop,
		       varray_type access_fns, 
		       tree ref)
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
  
  VARRAY_PUSH_TREE (access_fns, access_fn);
  
  /* Recursively record other array access functions.  */
  if (TREE_CODE (opnd0) == ARRAY_REF)
    return analyze_array_indexes (loop, access_fns, opnd0);
  
  /* Return the base name of the data access.  */
  else
    return opnd0;
}

/* For a data reference REF contained in the statemet STMT, initialize
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
  
  res = ggc_alloc (sizeof (struct data_reference));
  
  DR_ID (res) = data_ref_id++;
  DR_STMT (res) = stmt;
  DR_REF (res) = ref;
  VARRAY_TREE_INIT (DR_ACCESS_FNS (res), 3, "access_fns");
  DR_BASE_NAME (res) = analyze_array_indexes 
    (loop_containing_stmt (stmt), DR_ACCESS_FNS (res), ref);
  DR_IS_READ (res) = is_read;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
  
  return res;
}

/* For a data reference REF contained in the statemet STMT, initialize
   a DATA_REFERENCE structure, and return it.  */

struct data_reference *
init_data_ref (tree stmt, 
	       tree ref,
	       tree base,
	       tree access_fn)
{
  struct data_reference *res;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(init_data_ref \n");
      fprintf (dump_file, "  (ref = ");
      print_generic_stmt (dump_file, ref, 0);
      fprintf (dump_file, ")\n");
    }
  
  res = ggc_alloc (sizeof (struct data_reference));
  
  DR_ID (res) = data_ref_id++;
  DR_STMT (res) = stmt;
  DR_REF (res) = ref;
  VARRAY_TREE_INIT (DR_ACCESS_FNS (res), 5, "access_fns");
  DR_BASE_NAME (res) = base;
  VARRAY_PUSH_TREE (DR_ACCESS_FNS (res), access_fn);
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
  
  return res;
}



/* When there exists a dependence relation, determine its distance
   vector.  */

static void
compute_distance_vector (struct data_dependence_relation *ddr)
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
  
  res = ggc_alloc (sizeof (struct data_dependence_relation));
  DDR_A (res) = a;
  DDR_B (res) = b;

  if (a == NULL || b == NULL 
      || DR_BASE_NAME (a) == NULL_TREE
      || DR_BASE_NAME (b) == NULL_TREE)
    DDR_ARE_DEPENDENT (res) = chrec_dont_know;    

  /* When the dimensions of A and B differ, we directly initialize
     the relation to "there is no dependence": chrec_known.  */
  else if (DR_NUM_DIMENSIONS (a) != DR_NUM_DIMENSIONS (b)
	   || array_base_name_differ_p (a, b))
    DDR_ARE_DEPENDENT (res) = chrec_known;
  
  else
    {
      unsigned int i;
      DDR_ARE_DEPENDENT (res) = NULL_TREE;
      DDR_SUBSCRIPTS_VECTOR_INIT (res, DR_NUM_DIMENSIONS (a));
      
      for (i = 0; i < DR_NUM_DIMENSIONS (a); i++)
	{
	  struct subscript *subscript;
	  
	  subscript = ggc_alloc (sizeof (struct subscript));
	  SUB_CONFLICTS_IN_A (subscript) = chrec_dont_know;
	  SUB_CONFLICTS_IN_B (subscript) = chrec_dont_know;
	  SUB_LAST_CONFLICT_IN_A (subscript) = chrec_dont_know;
	  SUB_LAST_CONFLICT_IN_B (subscript) = chrec_dont_know;
	  SUB_DISTANCE (subscript) = chrec_dont_know;
	  SUB_DIRECTION (subscript) = dir_star;
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
  DDR_ARE_DEPENDENT (ddr) = chrec;  
  varray_clear (DDR_SUBSCRIPTS (ddr));
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
		       tree *overlaps_b)
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
	}
      else
	{
	  /* The accesses do not overlap.  */
	  *overlaps_a = chrec_known;
	  *overlaps_b = chrec_known;	  
	}
      break;
      
    default:
      /* We're not sure whether the indexes overlap.  For the moment, 
	 conservatively answer "don't know".  */
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;	  
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
				  tree *overlaps_b)
{
  bool value0, value1, value2;
  tree difference = chrec_fold_minus 
    (integer_type_node, CHREC_LEFT (chrec_b), chrec_a);
  
  if (!chrec_is_positive (initial_condition (difference), &value0))
    {
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
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
		      return;
		    }
		  
		  /* When the step does not divides the difference, there are
		     no overlaps.  */
		  else
		    {
		      *overlaps_a = chrec_known;
		      *overlaps_b = chrec_known;      
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
		      return;
		    }
		  
		  /* When the step does not divides the difference, there
		     are no overlaps.  */
		  else
		    {
		      *overlaps_a = chrec_known;
		      *overlaps_b = chrec_known;      
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
		  return;
		}
	    }
	}
    }
}

/* Analyze a SIV (Single Index Variable) subscript where CHREC_A is an
   affine function, and CHREC_B is a constant.  *OVERLAPS_A and
   *OVERLAPS_B are initialized to the functions that describe the
   relation between the elements accessed twice by CHREC_A and
   CHREC_B.  For k >= 0, the following property is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void
analyze_siv_subscript_affine_cst (tree chrec_a, 
				  tree chrec_b,
				  tree *overlaps_a, 
				  tree *overlaps_b)
{
  analyze_siv_subscript_cst_affine (chrec_b, chrec_a, overlaps_b, overlaps_a);
}

/* Determines the overlapping elements due to accesses CHREC_A and
   CHREC_B, that are affine functions.  This is a part of the
   subscript analyzer.  */

static void
analyze_subscript_affine_affine (tree chrec_a, 
				 tree chrec_b,
				 tree *overlaps_a, 
				 tree *overlaps_b)
{
  tree left_a, left_b, right_a, right_b;
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_subscript_affine_affine \n");
  
  /* For determining the initial intersection, we have to solve a
     Diophantine equation.  This is the most time consuming part.
     
     For answering to the question: "Is there a dependence?" we have
     to prove that there exists a solution to the Diophantine
     equation, and that the solution is in the iteration domain,
     ie. the solution is positive or zero, and that the solution
     happens before the upper bound loop.nb_iterations.  Otherwise
     there is no dependence.  This function outputs a description of
     the iterations that hold the intersections.  */

  left_a = CHREC_LEFT (chrec_a);
  left_b = CHREC_LEFT (chrec_b);
  right_a = CHREC_RIGHT (chrec_a);
  right_b = CHREC_RIGHT (chrec_b);
  
  if (chrec_zerop (chrec_fold_minus (integer_type_node, left_a, left_b)))
    {
      /* The first element accessed twice is on the first
	 iteration.  */
      *overlaps_a = build_polynomial_chrec 
	(CHREC_VARIABLE (chrec_b), integer_zero_node, integer_one_node);
      *overlaps_b = build_polynomial_chrec 
	(CHREC_VARIABLE (chrec_a), integer_zero_node, integer_one_node);
    }
  
  else if (TREE_CODE (left_a) == INTEGER_CST
	   && TREE_CODE (left_b) == INTEGER_CST
	   && TREE_CODE (right_a) == INTEGER_CST 
	   && TREE_CODE (right_b) == INTEGER_CST
	   
	   /* Both functions should have the same evolution sign.  */
	   && ((tree_int_cst_sgn (right_a) > 0 
		&& tree_int_cst_sgn (right_b) > 0)
	       || (tree_int_cst_sgn (right_a) < 0
		   && tree_int_cst_sgn (right_b) < 0)))
    {
      /* Here we have to solve the Diophantine equation.  Reference
	 book: "Loop Transformations for Restructuring Compilers - The
	 Foundations" by Utpal Banerjee, pages 59-80.
	 
	 ALPHA * X0 = BETA * Y0 + GAMMA.  
	 
	 with:
	 ALPHA = RIGHT_A
	 BETA = RIGHT_B
	 GAMMA = LEFT_B - LEFT_A
	 CHREC_A = {LEFT_A, +, RIGHT_A}
	 CHREC_B = {LEFT_B, +, RIGHT_B}
	 
	 The Diophantine equation has a solution if and only if gcd
	 (ALPHA, BETA) divides GAMMA.  This is commonly known under
	 the name of the "gcd-test".
      */
      tree alpha, beta, gamma;
      tree la, lb;
      tree gcd_alpha_beta;
      tree u11, u12, u21, u22;

      /* Both alpha and beta have to be integer_type_node. The gcd
	 function does not work correctly otherwise.  */
      alpha = copy_node (right_a);
      beta = copy_node (right_b);
      la = copy_node (left_a);
      lb = copy_node (left_b);
      TREE_TYPE (alpha) = integer_type_node;
      TREE_TYPE (beta) = integer_type_node;
      TREE_TYPE (la) = integer_type_node;
      TREE_TYPE (lb) = integer_type_node;
      
      gamma = fold (build (MINUS_EXPR, integer_type_node, lb, la));
      
      /* FIXME: Use lambda_*_Hermite for implementing Bezout.  */
      gcd_alpha_beta = tree_fold_bezout 
	(alpha, 
	 fold (build (MULT_EXPR, integer_type_node, beta, 
		      integer_minus_one_node)),
	 &u11, &u12, 
	 &u21, &u22);
      
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  (alpha = ");
	  print_generic_expr (dump_file, alpha, 0);
	  fprintf (dump_file, ")\n  (beta = ");
	  print_generic_expr (dump_file, beta, 0);
	  fprintf (dump_file, ")\n  (gamma = ");
	  print_generic_expr (dump_file, gamma, 0);
	  fprintf (dump_file, ")\n  (gcd_alpha_beta = ");
	  print_generic_expr (dump_file, gcd_alpha_beta, 0);
	  fprintf (dump_file, ")\n");
	}
      
      /* The classic "gcd-test".  */
      if (!tree_fold_divides_p (integer_type_node, gcd_alpha_beta, gamma))
	{
	  /* The "gcd-test" has determined that there is no integer
	     solution, ie. there is no dependence.  */
	  *overlaps_a = chrec_known;
	  *overlaps_b = chrec_known;
	}
      
      else
	{
	  /* The solutions are given by:
	     | 
	     | [GAMMA/GCD_ALPHA_BETA  t].[u11 u12]  = [X]
	     |                           [u21 u22]    [Y]
	     
	     For a given integer t.  Using the following variables,
	     
	     | i0 = u11 * gamma / gcd_alpha_beta
	     | j0 = u12 * gamma / gcd_alpha_beta
	     | i1 = u21
	     | j1 = u22
	     
	     the solutions are:
	     
	     | x = i0 + i1 * t, 
	     | y = j0 + j1 * t.  */
	  
	  tree i0, j0, i1, j1, t;
	  tree gamma_gcd;
	  
	  /* X0 and Y0 are the first iterations for which there is a
	     dependence.  X0, Y0 are two solutions of the Diophantine
	     equation: chrec_a (X0) = chrec_b (Y0).  */
	  tree x0, y0;
      
	  /* Exact div because in this case gcd_alpha_beta divides
	     gamma.  */
	  gamma_gcd = fold (build (EXACT_DIV_EXPR, integer_type_node, gamma, 
				   gcd_alpha_beta));
	  i0 = fold (build (MULT_EXPR, integer_type_node, u11, gamma_gcd));
	  j0 = fold (build (MULT_EXPR, integer_type_node, u12, gamma_gcd));
	  i1 = u21;
	  j1 = u22;
	  
	  if ((tree_int_cst_sgn (i1) == 0
	       && tree_int_cst_sgn (i0) < 0)
	      || (tree_int_cst_sgn (j1) == 0
		  && tree_int_cst_sgn (j0) < 0))
	    {
	      /* There is no solution.  
		 FIXME: The case "i0 > nb_iterations, j0 > nb_iterations" 
		 falls in here, but for the moment we don't look at the 
		 upper bound of the iteration domain.  */
	      *overlaps_a = chrec_known;
	      *overlaps_b = chrec_known;
  	    }
	  
	  else 
	    {
	      if (tree_int_cst_sgn (i1) > 0)
		{
		  t = fold 
		    (build (CEIL_DIV_EXPR, integer_type_node, 
			    fold (build (MULT_EXPR, integer_type_node, i0, 
					 integer_minus_one_node)), 
			    i1));
		  
		  if (tree_int_cst_sgn (j1) > 0)
		    {
		      t = fold 
			(build (MAX_EXPR, integer_type_node, t,
				fold (build (CEIL_DIV_EXPR, integer_type_node,
					     fold (build 
						   (MULT_EXPR,
						    integer_type_node, j0,
						    integer_minus_one_node)),
					     j1))));
		      
		      x0 = fold 
			(build (PLUS_EXPR, integer_type_node, i0, 
				fold (build 
				      (MULT_EXPR, integer_type_node, i1, t))));
		      y0 = fold 
			(build (PLUS_EXPR, integer_type_node, j0, 
				fold (build 
				      (MULT_EXPR, integer_type_node, j1, t))));
		      
		      *overlaps_a = build_polynomial_chrec 
			(CHREC_VARIABLE (chrec_b), x0, u21);
		      *overlaps_b = build_polynomial_chrec 
			(CHREC_VARIABLE (chrec_a), y0, u22);
		    }
		  else
		    {
		      /* FIXME: For the moment, the upper bound of the
			 iteration domain for j is not checked. */
		      *overlaps_a = chrec_dont_know;
		      *overlaps_b = chrec_dont_know;
		    }
		}
	      
	      else
		{
		  /* FIXME: For the moment, the upper bound of the
		     iteration domain for i is not checked. */
		  *overlaps_a = chrec_dont_know;
		  *overlaps_b = chrec_dont_know;
		}
	    }
	}
    }
  
  else
    {
      /* For the moment, "don't know".  */
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
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
		       tree *overlaps_b)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_siv_subscript \n");
  
  if (evolution_function_is_constant_p (chrec_a)
      && evolution_function_is_affine_p (chrec_b))
    analyze_siv_subscript_cst_affine (chrec_a, chrec_b, 
				      overlaps_a, overlaps_b);
  
  else if (evolution_function_is_affine_p (chrec_a)
	   && evolution_function_is_constant_p (chrec_b))
    analyze_siv_subscript_affine_cst (chrec_a, chrec_b, 
				      overlaps_a, overlaps_b);
  
  else if (evolution_function_is_affine_p (chrec_a)
	   && evolution_function_is_affine_p (chrec_b)
	   && (CHREC_VARIABLE (chrec_a) == CHREC_VARIABLE (chrec_b)))
    analyze_subscript_affine_affine (chrec_a, chrec_b, 
				     overlaps_a, overlaps_b);
  else
    {
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
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
		       tree *overlaps_b)
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
    }
  
  else if (evolution_function_is_univariate_p (chrec_a)
	   && evolution_function_is_univariate_p (chrec_b))
    {
      /* testsuite/.../ssa-chrec-35.c
	 {0, +, 1}_2  vs.  {0, +, 1}_3
	 the overlapping elements are respectively located at iterations:
	 {0, +, 1}_3 and {0, +, 1}_2.
      */
      if (evolution_function_is_affine_p (chrec_a)
	  && evolution_function_is_affine_p (chrec_b))
	analyze_subscript_affine_affine (chrec_a, chrec_b, 
					 overlaps_a, overlaps_b);
      else
	{
	  *overlaps_a = chrec_dont_know;
	  *overlaps_b = chrec_dont_know;
	}
    }
  
  else
    {
      /* When the analysis is too difficult, answer "don't know".  */
      *overlaps_a = chrec_dont_know;
      *overlaps_b = chrec_dont_know;
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
				tree *overlap_iterations_b)
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
			   overlap_iterations_a, overlap_iterations_b);
  
  else if (siv_subscript_p (chrec_a, chrec_b))
    analyze_siv_subscript (chrec_a, chrec_b, 
			   overlap_iterations_a, overlap_iterations_b);
  
  else
    analyze_miv_subscript (chrec_a, chrec_b, 
			   overlap_iterations_a, overlap_iterations_b);
  
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
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(subscript_dependence_tester \n");
  
  for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
    {
      tree overlaps_a, overlaps_b;
      struct subscript *subscript = DDR_SUBSCRIPT (ddr, i);
      
      analyze_overlapping_iterations (DR_ACCESS_FN (dra, i), 
				      DR_ACCESS_FN (drb, i),
				      &overlaps_a, &overlaps_b);
      
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
 	}
    }
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Compute the classic per loop distance vector.

   RES is the data dependence relation to build a vector from.
   CLASSIC_DIST is the varray to place the vector in.
   NB_LOOPS is the total number of loops we are considering.
   FIRST_LOOP is the loop->num of the first loop.  */

static void
build_classic_dist_vector (struct data_dependence_relation *res, 
			   varray_type *classic_dist, 
			   int nb_loops, unsigned int first_loop)
{
  unsigned i;
  lambda_vector dist_v, init_v;
  
  dist_v = lambda_vector_new (nb_loops);
  init_v = lambda_vector_new (nb_loops);
  lambda_vector_clear (dist_v, nb_loops);
  lambda_vector_clear (init_v, nb_loops);
  
  if (DDR_ARE_DEPENDENT (res) != NULL_TREE)
    return;
  
  for (i = 0; i < DDR_NUM_SUBSCRIPTS (res); i++)
    {
      struct subscript *subscript = DDR_SUBSCRIPT (res, i);

      if (chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	return;

      if (TREE_CODE (SUB_CONFLICTS_IN_A (subscript)) == POLYNOMIAL_CHREC)
	{
	  int dist;
	  int loop_nb;
	  loop_nb = CHREC_VARIABLE (SUB_CONFLICTS_IN_A (subscript));
	  loop_nb -= first_loop;
	  /* If the loop number is still greater than the number of
	     loops we've been asked to analyze, or negative,
	     something is borked.  */
	  if (loop_nb < 0 || loop_nb >= nb_loops)
	    abort ();
	  dist = int_cst_value (SUB_DISTANCE (subscript));

	  /* This is the subscript coupling test.  
	     | loop i = 0, N, 1
	     |   T[i+1][i] = ...
	     |   ... = T[i][i]
	     | endloop
	     There is no dependence.  */
	  if (init_v[loop_nb] != 0
	      && dist_v[loop_nb] != dist)
	    {
	      finalize_ddr_dependent (res, chrec_known);
	      return;
	    }

	  dist_v[loop_nb] = dist;
	  init_v[loop_nb] = 1;
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
    struct data_reference *a = DDR_A (res);
    struct data_reference *b = DDR_B (res);
    int lca_nb;
    loop_a = loop_containing_stmt (DR_STMT (a));
    loop_b = loop_containing_stmt (DR_STMT (b));
    
    /* Get the common ancestor loop.  */
    lca = find_common_loop (loop_a, loop_b); 
    
    lca_nb = lca->num;
    lca_nb -= first_loop;
    if (lca_nb < 0 || lca_nb >= nb_loops)
      abort ();
    /* For each outer loop where init_v is not set, the accesses are
       in dependence of distance 1 in the loop.  */
    if (lca != loop_a
	&& lca != loop_b
	&& init_v[lca_nb] == 0)
      dist_v[lca_nb] = 1;
    
    lca = lca->outer;
    
    if (lca)
      {
	lca_nb = lca->num - first_loop;
	while (lca->depth != 0)
	  {
	    if (lca_nb < 0 || lca_nb >= nb_loops)
	      abort ();
	    if (init_v[lca_nb] == 0)
	      dist_v[lca_nb] = 1;
	    lca = lca->outer;
	    lca_nb = lca->num - first_loop;
	  
	  }
      }
  }
  
  VARRAY_PUSH_GENERIC_PTR (*classic_dist, dist_v);
}

/* Compute the classic per loop direction vector.  

   RES is the data dependence relation to build a vector from.
   CLASSIC_DIR is the varray to place the vector in.
   NB_LOOPS is the total number of loops we are considering.
   FIRST_LOOP is the loop->num of the first loop.  */

static void
build_classic_dir_vector (struct data_dependence_relation *res, 
			  varray_type *classic_dir, 
			  int nb_loops, unsigned int first_loop)
{
  unsigned i;
  lambda_vector dir_v, init_v;
  
  dir_v = lambda_vector_new (nb_loops);
  init_v = lambda_vector_new (nb_loops);
  lambda_vector_clear (dir_v, nb_loops);
  lambda_vector_clear (init_v, nb_loops);
  
  if (DDR_ARE_DEPENDENT (res) != NULL_TREE)
    return;
  
  for (i = 0; i < DDR_NUM_SUBSCRIPTS (res); i++)
    {
      struct subscript *subscript = DDR_SUBSCRIPT (res, i);

      if (TREE_CODE (SUB_CONFLICTS_IN_A (subscript)) == POLYNOMIAL_CHREC
	  && TREE_CODE (SUB_CONFLICTS_IN_B (subscript)) == POLYNOMIAL_CHREC)
	{
	  int loop_nb;
	  
	  enum data_dependence_direction dir = dir_star;
	  loop_nb = CHREC_VARIABLE (SUB_CONFLICTS_IN_A (subscript));
	  loop_nb -= first_loop;

	  /* If the loop number is still greater than the number of
	     loops we've been asked to analyze, or negative,
	     something is borked.  */
	  if (loop_nb < 0 || loop_nb >= nb_loops)
	    abort ();	  
	  if (chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	    {
	      
	    }
	  else
	    {
	      int dist = int_cst_value (SUB_DISTANCE (subscript));
	      
	      if (dist == 0)
		dir = dir_equal;
	      else if (dist > 0)
		dir = dir_positive;
	      else if (dist < 0)
		dir = dir_negative;
	    }
	  
	  /* This is the subscript coupling test.  
	     | loop i = 0, N, 1
	     |   T[i+1][i] = ...
	     |   ... = T[i][i]
	     | endloop
	     There is no dependence.  */
	  if (init_v[loop_nb] != 0
	      && dir != dir_star
	      && (enum data_dependence_direction) dir_v[loop_nb] != dir
	      && (enum data_dependence_direction) dir_v[loop_nb] != dir_star)
	    {
	      finalize_ddr_dependent (res, chrec_known);
	      return;
	    }
	  
	  dir_v[loop_nb] = dir;
	  init_v[loop_nb] = 1;
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
    struct data_reference *a = DDR_A (res);
    struct data_reference *b = DDR_B (res);
    int lca_nb;
    loop_a = loop_containing_stmt (DR_STMT (a));
    loop_b = loop_containing_stmt (DR_STMT (b));
    
    /* Get the common ancestor loop.  */
    lca = find_common_loop (loop_a, loop_b); 
    lca_nb = lca->num - first_loop;

    if (lca_nb < 0 || lca_nb >= nb_loops)
      abort ();
    /* For each outer loop where init_v is not set, the accesses are
       in dependence of distance 1 in the loop.  */
    if (lca != loop_a
	&& lca != loop_b
	&& init_v[lca_nb] == 0)
      dir_v[lca_nb] = dir_positive;
    
    lca = lca->outer;
    if (lca)
      {
	lca_nb = lca->num - first_loop;
	while (lca->depth != 0)
	  {
	    if (lca_nb < 0 || lca_nb >= nb_loops)
	      abort ();
	    if (init_v[lca_nb] == 0)
	      dir_v[lca_nb] = dir_positive;
	    lca = lca->outer;
	    lca_nb = lca->num - first_loop;
	   
	  }
      }
  }
  
  VARRAY_PUSH_GENERIC_PTR (*classic_dir, dir_v);
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
      fprintf (dump_file, "(compute_affine_dependence (%d, %d)\n", 
	       DR_ID (dra), DR_ID (drb));
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
   opposite relation, ie. when AB has been computed, don't compute BA.
   DATAREFS contains a list of data references, and the result is set
   in DEPENDENCE_RELATIONS.  */

static void 
compute_rw_wr_ww_dependences (varray_type datarefs, 
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

	/* Don't compute the "read-read" relations.  */
	if (DR_IS_READ (a) && DR_IS_READ (b))
	  continue;

	ddr = initialize_data_dependence_relation (a, b);

	VARRAY_PUSH_GENERIC_PTR (*dependence_relations, ddr);
	compute_affine_dependence (ddr);
	compute_distance_vector (ddr);
      }
}

/* Search the data references in LOOP, and record the information into
   DATAREFS.  Returns chrec_dont_know when failing to analyze a
   difficult case, returns NULL_TREE otherwise.
   
   FIXME: This is a "dumb" walker over all the trees in the loop body.
   Find another technique that avoids this costly walk.  This is
   acceptable for the moment, since this function is used only for
   debugging purposes.  */

static tree 
find_data_references_in_loop (struct loop *loop, varray_type *datarefs)
{
  basic_block bb;
  block_stmt_iterator bsi;
  
  FOR_EACH_BB (bb)
    {
      if (!flow_bb_inside_loop_p (loop, bb))
	continue;
      
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
	    return chrec_dont_know;
	}
    }

  return NULL_TREE;
}



/* This section contains all the entry points.  */

/* Given a loop nest LOOP, the following vectors are returned:
   *DATAREFS is initialized to all the array elements contained in this loop, 
   *DEPENDENCE_RELATIONS contains the relations between the data references, 
   *CLASSIC_DIST contains the set of distance vectors,
   *CLASSIC_DIR contains the set of direction vectors.  */

void
compute_data_dependences_for_loop (unsigned nb_loops, 
				   struct loop *loop,
				   varray_type *datarefs,
				   varray_type *dependence_relations,
				   varray_type *classic_dist, 
				   varray_type *classic_dir)
{
  unsigned int i;

  /* If one of the data references is not computable, give up without
     spending time to compute other dependences.  */
  if (find_data_references_in_loop (loop, datarefs) == chrec_dont_know)
    {
      struct data_dependence_relation *ddr;

      /* Insert a single relation into dependence_relations:
	 chrec_dont_know.  */
      ddr = initialize_data_dependence_relation (NULL, NULL);
      VARRAY_PUSH_GENERIC_PTR (*dependence_relations, ddr);
      build_classic_dist_vector (ddr, classic_dist, nb_loops, loop->num);
      build_classic_dir_vector (ddr, classic_dir, nb_loops, loop->num);
      return;
    }

  compute_rw_wr_ww_dependences (*datarefs, dependence_relations);

  for (i = 0; i < VARRAY_ACTIVE_SIZE (*dependence_relations); i++)
    {
      struct data_dependence_relation *ddr;
      ddr = VARRAY_GENERIC_PTR (*dependence_relations, i);
      build_classic_dist_vector (ddr, classic_dist, nb_loops, loop->num);
      build_classic_dir_vector (ddr, classic_dir, nb_loops, loop->num);    
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
  varray_type classic_dist, classic_dir;
  int nb_data_refs = 10;

  VARRAY_GENERIC_PTR_INIT (classic_dist, 10, "classic_dist");
  VARRAY_GENERIC_PTR_INIT (classic_dir, 10, "classic_dir");
  VARRAY_GENERIC_PTR_INIT (datarefs, nb_data_refs, "datarefs");
  VARRAY_GENERIC_PTR_INIT (dependence_relations, 
			   nb_data_refs * nb_data_refs,
			   "dependence_relations");

  /* Compute DDs on the whole function.  */
  compute_data_dependences_for_loop (loops->num, loops->parray[0], 
				     &datarefs, &dependence_relations, 
				     &classic_dist, &classic_dir);

  if (dump_file)
    {
      dump_data_dependence_relations (dump_file, dependence_relations);
      fprintf (dump_file, "\n\n");
    }

  /* Don't dump distances in order to avoid to update the
     testsuite.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      for (i = 0; i < VARRAY_ACTIVE_SIZE (classic_dist); i++)
	{
	  fprintf (dump_file, "DISTANCE_V (");
	  print_lambda_vector (dump_file, 
			       VARRAY_GENERIC_PTR (classic_dist, i),
			       loops->num);
	  fprintf (dump_file, ")\n");
	}
      for (i = 0; i < VARRAY_ACTIVE_SIZE (classic_dir); i++)
	{
	  fprintf (dump_file, "DIRECTION_V (");
	  print_lambda_vector (dump_file, 
			       VARRAY_GENERIC_PTR (classic_dir, i),
			       loops->num);
	  fprintf (dump_file, ")\n");
	}
      fprintf (dump_file, "\n\n");
    }

  if (dump_file && (dump_flags & TDF_STATS))
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
	      
	      if (DR_NUM_DIMENSIONS (a) != DR_NUM_DIMENSIONS (b)
		  || array_base_name_differ_p (a, b))
		nb_basename_differ++;
	      else
		nb_bot_relations++;
	    }
	  
	  else 
	    nb_chrec_relations++;
	}
      
      fprintf (dump_file, "\n(\n");
      fprintf (dump_file, "%d\tnb_top_relations\n", nb_top_relations);
      fprintf (dump_file, "%d\tnb_bot_relations\n", nb_bot_relations);
      fprintf (dump_file, "%d\tnb_basename_differ\n", nb_basename_differ);
      fprintf (dump_file, "%d\tnb_distance_relations\n", (int) VARRAY_ACTIVE_SIZE (classic_dist));
      fprintf (dump_file, "%d\tnb_chrec_relations\n", nb_chrec_relations);
      fprintf (dump_file, "\n)\n");
      
      gather_stats_on_scev_database ();
    }
  
  varray_clear (dependence_relations);
  varray_clear (datarefs);
  varray_clear (classic_dist);
  varray_clear (classic_dir);
}


