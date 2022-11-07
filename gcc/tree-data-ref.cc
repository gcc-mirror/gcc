/* Data references and dependences detectors.
   Copyright (C) 2003-2022 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <pop@cri.ensmp.fr>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "expr.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "dumpfile.h"
#include "tree-affine.h"
#include "builtins.h"
#include "tree-eh.h"
#include "ssa.h"
#include "internal-fn.h"
#include "vr-values.h"
#include "range-op.h"
#include "tree-ssa-loop-ivopts.h"

static struct datadep_stats
{
  int num_dependence_tests;
  int num_dependence_dependent;
  int num_dependence_independent;
  int num_dependence_undetermined;

  int num_subscript_tests;
  int num_subscript_undetermined;
  int num_same_subscript_function;

  int num_ziv;
  int num_ziv_independent;
  int num_ziv_dependent;
  int num_ziv_unimplemented;

  int num_siv;
  int num_siv_independent;
  int num_siv_dependent;
  int num_siv_unimplemented;

  int num_miv;
  int num_miv_independent;
  int num_miv_dependent;
  int num_miv_unimplemented;
} dependence_stats;

static bool subscript_dependence_tester_1 (struct data_dependence_relation *,
					   unsigned int, unsigned int,
					   class loop *);
/* Returns true iff A divides B.  */

static inline bool
tree_fold_divides_p (const_tree a, const_tree b)
{
  gcc_assert (TREE_CODE (a) == INTEGER_CST);
  gcc_assert (TREE_CODE (b) == INTEGER_CST);
  return integer_zerop (int_const_binop (TRUNC_MOD_EXPR, b, a));
}

/* Returns true iff A divides B.  */

static inline bool
int_divides_p (lambda_int a, lambda_int b)
{
  return ((b % a) == 0);
}

/* Return true if reference REF contains a union access.  */

static bool
ref_contains_union_access_p (tree ref)
{
  while (handled_component_p (ref))
    {
      ref = TREE_OPERAND (ref, 0);
      if (TREE_CODE (TREE_TYPE (ref)) == UNION_TYPE
	  || TREE_CODE (TREE_TYPE (ref)) == QUAL_UNION_TYPE)
	return true;
    }
  return false;
}



/* Dump into FILE all the data references from DATAREFS.  */

static void
dump_data_references (FILE *file, vec<data_reference_p> datarefs)
{
  for (data_reference *dr : datarefs)
    dump_data_reference (file, dr);
}

/* Unified dump into FILE all the data references from DATAREFS.  */

DEBUG_FUNCTION void
debug (vec<data_reference_p> &ref)
{
  dump_data_references (stderr, ref);
}

DEBUG_FUNCTION void
debug (vec<data_reference_p> *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Dump into STDERR all the data references from DATAREFS.  */

DEBUG_FUNCTION void
debug_data_references (vec<data_reference_p> datarefs)
{
  dump_data_references (stderr, datarefs);
}

/* Print to STDERR the data_reference DR.  */

DEBUG_FUNCTION void
debug_data_reference (struct data_reference *dr)
{
  dump_data_reference (stderr, dr);
}

/* Dump function for a DATA_REFERENCE structure.  */

void
dump_data_reference (FILE *outf,
		     struct data_reference *dr)
{
  unsigned int i;

  fprintf (outf, "#(Data Ref: \n");
  fprintf (outf, "#  bb: %d \n", gimple_bb (DR_STMT (dr))->index);
  fprintf (outf, "#  stmt: ");
  print_gimple_stmt (outf, DR_STMT (dr), 0);
  fprintf (outf, "#  ref: ");
  print_generic_stmt (outf, DR_REF (dr));
  fprintf (outf, "#  base_object: ");
  print_generic_stmt (outf, DR_BASE_OBJECT (dr));

  for (i = 0; i < DR_NUM_DIMENSIONS (dr); i++)
    {
      fprintf (outf, "#  Access function %d: ", i);
      print_generic_stmt (outf, DR_ACCESS_FN (dr, i));
    }
  fprintf (outf, "#)\n");
}

/* Unified dump function for a DATA_REFERENCE structure.  */

DEBUG_FUNCTION void
debug (data_reference &ref)
{
  dump_data_reference (stderr, &ref);
}

DEBUG_FUNCTION void
debug (data_reference *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Dumps the affine function described by FN to the file OUTF.  */

DEBUG_FUNCTION void
dump_affine_function (FILE *outf, affine_fn fn)
{
  unsigned i;
  tree coef;

  print_generic_expr (outf, fn[0], TDF_SLIM);
  for (i = 1; fn.iterate (i, &coef); i++)
    {
      fprintf (outf, " + ");
      print_generic_expr (outf, coef, TDF_SLIM);
      fprintf (outf, " * x_%u", i);
    }
}

/* Dumps the conflict function CF to the file OUTF.  */

DEBUG_FUNCTION void
dump_conflict_function (FILE *outf, conflict_function *cf)
{
  unsigned i;

  if (cf->n == NO_DEPENDENCE)
    fprintf (outf, "no dependence");
  else if (cf->n == NOT_KNOWN)
    fprintf (outf, "not known");
  else
    {
      for (i = 0; i < cf->n; i++)
	{
	  if (i != 0)
	    fprintf (outf, " ");
	  fprintf (outf, "[");
	  dump_affine_function (outf, cf->fns[i]);
	  fprintf (outf, "]");
	}
    }
}

/* Dump function for a SUBSCRIPT structure.  */

DEBUG_FUNCTION void
dump_subscript (FILE *outf, struct subscript *subscript)
{
  conflict_function *cf = SUB_CONFLICTS_IN_A (subscript);

  fprintf (outf, "\n (subscript \n");
  fprintf (outf, "  iterations_that_access_an_element_twice_in_A: ");
  dump_conflict_function (outf, cf);
  if (CF_NONTRIVIAL_P (cf))
    {
      tree last_iteration = SUB_LAST_CONFLICT (subscript);
      fprintf (outf, "\n  last_conflict: ");
      print_generic_expr (outf, last_iteration);
    }

  cf = SUB_CONFLICTS_IN_B (subscript);
  fprintf (outf, "\n  iterations_that_access_an_element_twice_in_B: ");
  dump_conflict_function (outf, cf);
  if (CF_NONTRIVIAL_P (cf))
    {
      tree last_iteration = SUB_LAST_CONFLICT (subscript);
      fprintf (outf, "\n  last_conflict: ");
      print_generic_expr (outf, last_iteration);
    }

  fprintf (outf, "\n  (Subscript distance: ");
  print_generic_expr (outf, SUB_DISTANCE (subscript));
  fprintf (outf, " ))\n");
}

/* Print the classic direction vector DIRV to OUTF.  */

DEBUG_FUNCTION void
print_direction_vector (FILE *outf,
			lambda_vector dirv,
			int length)
{
  int eq;

  for (eq = 0; eq < length; eq++)
    {
      enum data_dependence_direction dir = ((enum data_dependence_direction)
					    dirv[eq]);

      switch (dir)
	{
	case dir_positive:
	  fprintf (outf, "    +");
	  break;
	case dir_negative:
	  fprintf (outf, "    -");
	  break;
	case dir_equal:
	  fprintf (outf, "    =");
	  break;
	case dir_positive_or_equal:
	  fprintf (outf, "   +=");
	  break;
	case dir_positive_or_negative:
	  fprintf (outf, "   +-");
	  break;
	case dir_negative_or_equal:
	  fprintf (outf, "   -=");
	  break;
	case dir_star:
	  fprintf (outf, "    *");
	  break;
	default:
	  fprintf (outf, "indep");
	  break;
	}
    }
  fprintf (outf, "\n");
}

/* Print a vector of direction vectors.  */

DEBUG_FUNCTION void
print_dir_vectors (FILE *outf, vec<lambda_vector> dir_vects,
		   int length)
{
  for (lambda_vector v : dir_vects)
    print_direction_vector (outf, v, length);
}

/* Print out a vector VEC of length N to OUTFILE.  */

DEBUG_FUNCTION void
print_lambda_vector (FILE * outfile, lambda_vector vector, int n)
{
  int i;

  for (i = 0; i < n; i++)
    fprintf (outfile, HOST_WIDE_INT_PRINT_DEC " ", vector[i]);
  fprintf (outfile, "\n");
}

/* Print a vector of distance vectors.  */

DEBUG_FUNCTION void
print_dist_vectors (FILE *outf, vec<lambda_vector> dist_vects,
		    int length)
{
  for (lambda_vector v : dist_vects)
    print_lambda_vector (outf, v, length);
}

/* Dump function for a DATA_DEPENDENCE_RELATION structure.  */

DEBUG_FUNCTION void
dump_data_dependence_relation (FILE *outf, const data_dependence_relation *ddr)
{
  struct data_reference *dra, *drb;

  fprintf (outf, "(Data Dep: \n");

  if (!ddr || DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    {
      if (ddr)
	{
	  dra = DDR_A (ddr);
	  drb = DDR_B (ddr);
	  if (dra)
	    dump_data_reference (outf, dra);
	  else
	    fprintf (outf, "    (nil)\n");
	  if (drb)
	    dump_data_reference (outf, drb);
	  else
	    fprintf (outf, "    (nil)\n");
	}
      fprintf (outf, "    (don't know)\n)\n");
      return;
    }

  dra = DDR_A (ddr);
  drb = DDR_B (ddr);
  dump_data_reference (outf, dra);
  dump_data_reference (outf, drb);

  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    fprintf (outf, "    (no dependence)\n");

  else if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
    {
      unsigned int i;
      class loop *loopi;

      subscript *sub;
      FOR_EACH_VEC_ELT (DDR_SUBSCRIPTS (ddr), i, sub)
	{
	  fprintf (outf, "  access_fn_A: ");
	  print_generic_stmt (outf, SUB_ACCESS_FN (sub, 0));
	  fprintf (outf, "  access_fn_B: ");
	  print_generic_stmt (outf, SUB_ACCESS_FN (sub, 1));
	  dump_subscript (outf, sub);
	}

      fprintf (outf, "  loop nest: (");
      FOR_EACH_VEC_ELT (DDR_LOOP_NEST (ddr), i, loopi)
	fprintf (outf, "%d ", loopi->num);
      fprintf (outf, ")\n");

      for (i = 0; i < DDR_NUM_DIST_VECTS (ddr); i++)
	{
	  fprintf (outf, "  distance_vector: ");
	  print_lambda_vector (outf, DDR_DIST_VECT (ddr, i),
			       DDR_NB_LOOPS (ddr));
	}

      for (i = 0; i < DDR_NUM_DIR_VECTS (ddr); i++)
	{
	  fprintf (outf, "  direction_vector: ");
	  print_direction_vector (outf, DDR_DIR_VECT (ddr, i),
				  DDR_NB_LOOPS (ddr));
	}
    }

  fprintf (outf, ")\n");
}

/* Debug version.  */

DEBUG_FUNCTION void
debug_data_dependence_relation (const struct data_dependence_relation *ddr)
{
  dump_data_dependence_relation (stderr, ddr);
}

/* Dump into FILE all the dependence relations from DDRS.  */

DEBUG_FUNCTION void
dump_data_dependence_relations (FILE *file, const vec<ddr_p> &ddrs)
{
  for (auto ddr : ddrs)
    dump_data_dependence_relation (file, ddr);
}

DEBUG_FUNCTION void
debug (vec<ddr_p> &ref)
{
  dump_data_dependence_relations (stderr, ref);
}

DEBUG_FUNCTION void
debug (vec<ddr_p> *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Dump to STDERR all the dependence relations from DDRS.  */

DEBUG_FUNCTION void
debug_data_dependence_relations (vec<ddr_p> ddrs)
{
  dump_data_dependence_relations (stderr, ddrs);
}

/* Dumps the distance and direction vectors in FILE.  DDRS contains
   the dependence relations, and VECT_SIZE is the size of the
   dependence vectors, or in other words the number of loops in the
   considered nest.  */

DEBUG_FUNCTION void
dump_dist_dir_vectors (FILE *file, vec<ddr_p> ddrs)
{
  for (data_dependence_relation *ddr : ddrs)
    if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE && DDR_AFFINE_P (ddr))
      {
	for (lambda_vector v : DDR_DIST_VECTS (ddr))
	  {
	    fprintf (file, "DISTANCE_V (");
	    print_lambda_vector (file, v, DDR_NB_LOOPS (ddr));
	    fprintf (file, ")\n");
	  }

	for (lambda_vector v : DDR_DIR_VECTS (ddr))
	  {
	    fprintf (file, "DIRECTION_V (");
	    print_direction_vector (file, v, DDR_NB_LOOPS (ddr));
	    fprintf (file, ")\n");
	  }
      }

  fprintf (file, "\n\n");
}

/* Dumps the data dependence relations DDRS in FILE.  */

DEBUG_FUNCTION void
dump_ddrs (FILE *file, vec<ddr_p> ddrs)
{
  for (data_dependence_relation *ddr : ddrs)
    dump_data_dependence_relation (file, ddr);

  fprintf (file, "\n\n");
}

DEBUG_FUNCTION void
debug_ddrs (vec<ddr_p> ddrs)
{
  dump_ddrs (stderr, ddrs);
}

/* If RESULT_RANGE is nonnull, set *RESULT_RANGE to the range of
   OP0 CODE OP1, where:

   - OP0 CODE OP1 has integral type TYPE
   - the range of OP0 is given by OP0_RANGE and
   - the range of OP1 is given by OP1_RANGE.

   Independently of RESULT_RANGE, try to compute:

     DELTA = ((sizetype) OP0 CODE (sizetype) OP1)
	     - (sizetype) (OP0 CODE OP1)

   as a constant and subtract DELTA from the ssizetype constant in *OFF.
   Return true on success, or false if DELTA is not known at compile time.

   Truncation and sign changes are known to distribute over CODE, i.e.

     (itype) (A CODE B) == (itype) A CODE (itype) B

   for any integral type ITYPE whose precision is no greater than the
   precision of A and B.  */

static bool
compute_distributive_range (tree type, value_range &op0_range,
			    tree_code code, value_range &op1_range,
			    tree *off, value_range *result_range)
{
  gcc_assert (INTEGRAL_TYPE_P (type) && !TYPE_OVERFLOW_TRAPS (type));
  if (result_range)
    {
      range_op_handler op (code, type);
      if (!op.fold_range (*result_range, type, op0_range, op1_range))
	result_range->set_varying (type);
    }

  /* The distributive property guarantees that if TYPE is no narrower
     than SIZETYPE,

       (sizetype) (OP0 CODE OP1) == (sizetype) OP0 CODE (sizetype) OP1

     and so we can treat DELTA as zero.  */
  if (TYPE_PRECISION (type) >= TYPE_PRECISION (sizetype))
    return true;

  /* If overflow is undefined, we can assume that:

       X == (ssizetype) OP0 CODE (ssizetype) OP1

     is within the range of TYPE, i.e.:

       X == (ssizetype) (TYPE) X

     Distributing the (TYPE) truncation over X gives:

       X == (ssizetype) (OP0 CODE OP1)

     Casting both sides to sizetype and distributing the sizetype cast
     over X gives:

       (sizetype) OP0 CODE (sizetype) OP1 == (sizetype) (OP0 CODE OP1)

     and so we can treat DELTA as zero.  */
  if (TYPE_OVERFLOW_UNDEFINED (type))
    return true;

  /* Compute the range of:

       (ssizetype) OP0 CODE (ssizetype) OP1

     The distributive property guarantees that this has the same bitpattern as:

       (sizetype) OP0 CODE (sizetype) OP1

     but its range is more conducive to analysis.  */
  range_cast (op0_range, ssizetype);
  range_cast (op1_range, ssizetype);
  value_range wide_range;
  range_op_handler op (code, ssizetype);
  bool saved_flag_wrapv = flag_wrapv;
  flag_wrapv = 1;
  if (!op.fold_range (wide_range, ssizetype, op0_range, op1_range))
    wide_range.set_varying (ssizetype);;
  flag_wrapv = saved_flag_wrapv;
  if (wide_range.num_pairs () != 1 || !range_int_cst_p (&wide_range))
    return false;

  wide_int lb = wide_range.lower_bound ();
  wide_int ub = wide_range.upper_bound ();

  /* Calculate the number of times that each end of the range overflows or
     underflows TYPE.  We can only calculate DELTA if the numbers match.  */
  unsigned int precision = TYPE_PRECISION (type);
  if (!TYPE_UNSIGNED (type))
    {
      wide_int type_min = wi::mask (precision - 1, true, lb.get_precision ());
      lb -= type_min;
      ub -= type_min;
    }
  wide_int upper_bits = wi::mask (precision, true, lb.get_precision ());
  lb &= upper_bits;
  ub &= upper_bits;
  if (lb != ub)
    return false;

  /* OP0 CODE OP1 overflows exactly arshift (LB, PRECISION) times, with
     negative values indicating underflow.  The low PRECISION bits of LB
     are clear, so DELTA is therefore LB (== UB).  */
  *off = wide_int_to_tree (ssizetype, wi::to_wide (*off) - lb);
  return true;
}

/* Return true if (sizetype) OP == (sizetype) (TO_TYPE) OP,
   given that OP has type FROM_TYPE and range RANGE.  Both TO_TYPE and
   FROM_TYPE are integral types.  */

static bool
nop_conversion_for_offset_p (tree to_type, tree from_type, value_range &range)
{
  gcc_assert (INTEGRAL_TYPE_P (to_type)
	      && INTEGRAL_TYPE_P (from_type)
	      && !TYPE_OVERFLOW_TRAPS (to_type)
	      && !TYPE_OVERFLOW_TRAPS (from_type));

  /* Converting to something no narrower than sizetype and then to sizetype
     is equivalent to converting directly to sizetype.  */
  if (TYPE_PRECISION (to_type) >= TYPE_PRECISION (sizetype))
    return true;

  /* Check whether TO_TYPE can represent all values that FROM_TYPE can.  */
  if (TYPE_PRECISION (from_type) < TYPE_PRECISION (to_type)
      && (TYPE_UNSIGNED (from_type) || !TYPE_UNSIGNED (to_type)))
    return true;

  /* For narrowing conversions, we could in principle test whether
     the bits in FROM_TYPE but not in TO_TYPE have a fixed value
     and apply a constant adjustment.

     For other conversions (which involve a sign change) we could
     check that the signs are always equal, and apply a constant
     adjustment if the signs are negative.

     However, both cases should be rare.  */
  return range_fits_type_p (&range, TYPE_PRECISION (to_type),
			    TYPE_SIGN (to_type));
}

static void
split_constant_offset (tree type, tree *var, tree *off,
		       value_range *result_range,
		       hash_map<tree, std::pair<tree, tree> > &cache,
		       unsigned *limit);

/* Helper function for split_constant_offset.  If TYPE is a pointer type,
   try to express OP0 CODE OP1 as:

     POINTER_PLUS <*VAR, (sizetype) *OFF>

   where:

   - *VAR has type TYPE
   - *OFF is a constant of type ssizetype.

   If TYPE is an integral type, try to express (sizetype) (OP0 CODE OP1) as:

     *VAR + (sizetype) *OFF

   where:

   - *VAR has type sizetype
   - *OFF is a constant of type ssizetype.

   In both cases, OP0 CODE OP1 has type TYPE.

   Return true on success.  A false return value indicates that we can't
   do better than set *OFF to zero.

   When returning true, set RESULT_RANGE to the range of OP0 CODE OP1,
   if RESULT_RANGE is nonnull and if we can do better than assume VR_VARYING.

   CACHE caches {*VAR, *OFF} pairs for SSA names that we've previously
   visited.  LIMIT counts down the number of SSA names that we are
   allowed to process before giving up.  */

static bool
split_constant_offset_1 (tree type, tree op0, enum tree_code code, tree op1,
			 tree *var, tree *off, value_range *result_range,
			 hash_map<tree, std::pair<tree, tree> > &cache,
			 unsigned *limit)
{
  tree var0, var1;
  tree off0, off1;
  value_range op0_range, op1_range;

  *var = NULL_TREE;
  *off = NULL_TREE;

  if (INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_TRAPS (type))
    return false;

  switch (code)
    {
    case INTEGER_CST:
      *var = size_int (0);
      *off = fold_convert (ssizetype, op0);
      if (result_range)
	result_range->set (op0, op0);
      return true;

    case POINTER_PLUS_EXPR:
      split_constant_offset (op0, &var0, &off0, nullptr, cache, limit);
      split_constant_offset (op1, &var1, &off1, nullptr, cache, limit);
      *var = fold_build2 (POINTER_PLUS_EXPR, type, var0, var1);
      *off = size_binop (PLUS_EXPR, off0, off1);
      return true;

    case PLUS_EXPR:
    case MINUS_EXPR:
      split_constant_offset (op0, &var0, &off0, &op0_range, cache, limit);
      split_constant_offset (op1, &var1, &off1, &op1_range, cache, limit);
      *off = size_binop (code, off0, off1);
      if (!compute_distributive_range (type, op0_range, code, op1_range,
				       off, result_range))
	return false;
      *var = fold_build2 (code, sizetype, var0, var1);
      return true;

    case MULT_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST)
	return false;

      split_constant_offset (op0, &var0, &off0, &op0_range, cache, limit);
      op1_range.set (op1, op1);
      *off = size_binop (MULT_EXPR, off0, fold_convert (ssizetype, op1));
      if (!compute_distributive_range (type, op0_range, code, op1_range,
				       off, result_range))
	return false;
      *var = fold_build2 (MULT_EXPR, sizetype, var0,
			  fold_convert (sizetype, op1));
      return true;

    case ADDR_EXPR:
      {
	tree base, poffset;
	poly_int64 pbitsize, pbitpos, pbytepos;
	machine_mode pmode;
	int punsignedp, preversep, pvolatilep;

	op0 = TREE_OPERAND (op0, 0);
	base
	  = get_inner_reference (op0, &pbitsize, &pbitpos, &poffset, &pmode,
				 &punsignedp, &preversep, &pvolatilep);

	if (!multiple_p (pbitpos, BITS_PER_UNIT, &pbytepos))
	  return false;
	base = build_fold_addr_expr (base);
	off0 = ssize_int (pbytepos);

	if (poffset)
	  {
	    split_constant_offset (poffset, &poffset, &off1, nullptr,
				   cache, limit);
	    off0 = size_binop (PLUS_EXPR, off0, off1);
	    base = fold_build_pointer_plus (base, poffset);
	  }

	var0 = fold_convert (type, base);

	/* If variable length types are involved, punt, otherwise casts
	   might be converted into ARRAY_REFs in gimplify_conversion.
	   To compute that ARRAY_REF's element size TYPE_SIZE_UNIT, which
	   possibly no longer appears in current GIMPLE, might resurface.
	   This perhaps could run
	   if (CONVERT_EXPR_P (var0))
	     {
	       gimplify_conversion (&var0);
	       // Attempt to fill in any within var0 found ARRAY_REF's
	       // element size from corresponding op embedded ARRAY_REF,
	       // if unsuccessful, just punt.
	     }  */
	while (POINTER_TYPE_P (type))
	  type = TREE_TYPE (type);
	if (int_size_in_bytes (type) < 0)
	  return false;

	*var = var0;
	*off = off0;
	return true;
      }

    case SSA_NAME:
      {
	if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0))
	  return false;

	gimple *def_stmt = SSA_NAME_DEF_STMT (op0);
	enum tree_code subcode;

	if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
	  return false;

	subcode = gimple_assign_rhs_code (def_stmt);

	/* We are using a cache to avoid un-CSEing large amounts of code.  */
	bool use_cache = false;
	if (!has_single_use (op0)
	    && (subcode == POINTER_PLUS_EXPR
		|| subcode == PLUS_EXPR
		|| subcode == MINUS_EXPR
		|| subcode == MULT_EXPR
		|| subcode == ADDR_EXPR
		|| CONVERT_EXPR_CODE_P (subcode)))
	  {
	    use_cache = true;
	    bool existed;
	    std::pair<tree, tree> &e = cache.get_or_insert (op0, &existed);
	    if (existed)
	      {
		if (integer_zerop (e.second))
		  return false;
		*var = e.first;
		*off = e.second;
		/* The caller sets the range in this case.  */
		return true;
	      }
	    e = std::make_pair (op0, ssize_int (0));
	  }

	if (*limit == 0)
	  return false;
	--*limit;

	var0 = gimple_assign_rhs1 (def_stmt);
	var1 = gimple_assign_rhs2 (def_stmt);

	bool res = split_constant_offset_1 (type, var0, subcode, var1,
					    var, off, nullptr, cache, limit);
	if (res && use_cache)
	  *cache.get (op0) = std::make_pair (*var, *off);
	/* The caller sets the range in this case.  */
	return res;
      }
    CASE_CONVERT:
      {
	/* We can only handle the following conversions:

	   - Conversions from one pointer type to another pointer type.

	   - Conversions from one non-trapping integral type to another
	     non-trapping integral type.  In this case, the recursive
	     call makes sure that:

	       (sizetype) OP0

	     can be expressed as a sizetype operation involving VAR and OFF,
	     and all we need to do is check whether:

	       (sizetype) OP0 == (sizetype) (TYPE) OP0

	   - Conversions from a non-trapping sizetype-size integral type to
	     a like-sized pointer type.  In this case, the recursive call
	     makes sure that:

	       (sizetype) OP0 == *VAR + (sizetype) *OFF

	     and we can convert that to:

	       POINTER_PLUS <(TYPE) *VAR, (sizetype) *OFF>

	   - Conversions from a sizetype-sized pointer type to a like-sized
	     non-trapping integral type.  In this case, the recursive call
	     makes sure that:

	       OP0 == POINTER_PLUS <*VAR, (sizetype) *OFF>

	     where the POINTER_PLUS and *VAR have the same precision as
	     TYPE (and the same precision as sizetype).  Then:

	       (sizetype) (TYPE) OP0 == (sizetype) *VAR + (sizetype) *OFF.  */
	tree itype = TREE_TYPE (op0);
	if ((POINTER_TYPE_P (itype)
	     || (INTEGRAL_TYPE_P (itype) && !TYPE_OVERFLOW_TRAPS (itype)))
	    && (POINTER_TYPE_P (type)
		|| (INTEGRAL_TYPE_P (type) && !TYPE_OVERFLOW_TRAPS (type)))
	    && (POINTER_TYPE_P (type) == POINTER_TYPE_P (itype)
		|| (TYPE_PRECISION (type) == TYPE_PRECISION (sizetype)
		    && TYPE_PRECISION (itype) == TYPE_PRECISION (sizetype))))
	  {
	    if (POINTER_TYPE_P (type))
	      {
		split_constant_offset (op0, var, off, nullptr, cache, limit);
		*var = fold_convert (type, *var);
	      }
	    else if (POINTER_TYPE_P (itype))
	      {
		split_constant_offset (op0, var, off, nullptr, cache, limit);
		*var = fold_convert (sizetype, *var);
	      }
	    else
	      {
		split_constant_offset (op0, var, off, &op0_range,
				       cache, limit);
		if (!nop_conversion_for_offset_p (type, itype, op0_range))
		  return false;
		if (result_range)
		  {
		    *result_range = op0_range;
		    range_cast (*result_range, type);
		  }
	      }
	    return true;
	  }
	return false;
      }

    default:
      return false;
    }
}

/* If EXP has pointer type, try to express it as:

     POINTER_PLUS <*VAR, (sizetype) *OFF>

   where:

   - *VAR has the same type as EXP
   - *OFF is a constant of type ssizetype.

   If EXP has an integral type, try to express (sizetype) EXP as:

     *VAR + (sizetype) *OFF

   where:

   - *VAR has type sizetype
   - *OFF is a constant of type ssizetype.

   If EXP_RANGE is nonnull, set it to the range of EXP.

   CACHE caches {*VAR, *OFF} pairs for SSA names that we've previously
   visited.  LIMIT counts down the number of SSA names that we are
   allowed to process before giving up.  */

static void
split_constant_offset (tree exp, tree *var, tree *off, value_range *exp_range,
		       hash_map<tree, std::pair<tree, tree> > &cache,
		       unsigned *limit)
{
  tree type = TREE_TYPE (exp), op0, op1;
  enum tree_code code;

  code = TREE_CODE (exp);
  if (exp_range)
    {
      *exp_range = type;
      if (code == SSA_NAME)
	{
	  value_range vr;
	  get_range_query (cfun)->range_of_expr (vr, exp);
	  if (vr.undefined_p ())
	    vr.set_varying (TREE_TYPE (exp));
	  wide_int var_min = wi::to_wide (vr.min ());
	  wide_int var_max = wi::to_wide (vr.max ());
	  value_range_kind vr_kind = vr.kind ();
	  wide_int var_nonzero = get_nonzero_bits (exp);
	  vr_kind = intersect_range_with_nonzero_bits (vr_kind,
						       &var_min, &var_max,
						       var_nonzero,
						       TYPE_SIGN (type));
	  /* This check for VR_VARYING is here because the old code
	     using get_range_info would return VR_RANGE for the entire
	     domain, instead of VR_VARYING.  The new code normalizes
	     full-domain ranges to VR_VARYING.  */
	  if (vr_kind == VR_RANGE || vr_kind == VR_VARYING)
	    *exp_range = value_range (type, var_min, var_max);
	}
    }

  if (!tree_is_chrec (exp)
      && get_gimple_rhs_class (TREE_CODE (exp)) != GIMPLE_TERNARY_RHS)
    {
      extract_ops_from_tree (exp, &code, &op0, &op1);
      if (split_constant_offset_1 (type, op0, code, op1, var, off,
				   exp_range, cache, limit))
	return;
    }

  *var = exp;
  if (INTEGRAL_TYPE_P (type))
    *var = fold_convert (sizetype, *var);
  *off = ssize_int (0);

  value_range r;
  if (exp_range && code != SSA_NAME
      && get_range_query (cfun)->range_of_expr (r, exp)
      && !r.undefined_p ())
    *exp_range = r;
}

/* Expresses EXP as VAR + OFF, where OFF is a constant.  VAR has the same
   type as EXP while OFF has type ssizetype.  */

void
split_constant_offset (tree exp, tree *var, tree *off)
{
  unsigned limit = param_ssa_name_def_chain_limit;
  static hash_map<tree, std::pair<tree, tree> > *cache;
  if (!cache)
    cache = new hash_map<tree, std::pair<tree, tree> > (37);
  split_constant_offset (exp, var, off, nullptr, *cache, &limit);
  *var = fold_convert (TREE_TYPE (exp), *var);
  cache->empty ();
}

/* Returns the address ADDR of an object in a canonical shape (without nop
   casts, and with type of pointer to the object).  */

static tree
canonicalize_base_object_address (tree addr)
{
  tree orig = addr;

  STRIP_NOPS (addr);

  /* The base address may be obtained by casting from integer, in that case
     keep the cast.  */
  if (!POINTER_TYPE_P (TREE_TYPE (addr)))
    return orig;

  if (TREE_CODE (addr) != ADDR_EXPR)
    return addr;

  return build_fold_addr_expr (TREE_OPERAND (addr, 0));
}

/* Analyze the behavior of memory reference REF within STMT.
   There are two modes:

   - BB analysis.  In this case we simply split the address into base,
     init and offset components, without reference to any containing loop.
     The resulting base and offset are general expressions and they can
     vary arbitrarily from one iteration of the containing loop to the next.
     The step is always zero.

   - loop analysis.  In this case we analyze the reference both wrt LOOP
     and on the basis that the reference occurs (is "used") in LOOP;
     see the comment above analyze_scalar_evolution_in_loop for more
     information about this distinction.  The base, init, offset and
     step fields are all invariant in LOOP.

   Perform BB analysis if LOOP is null, or if LOOP is the function's
   dummy outermost loop.  In other cases perform loop analysis.

   Return true if the analysis succeeded and store the results in DRB if so.
   BB analysis can only fail for bitfield or reversed-storage accesses.  */

opt_result
dr_analyze_innermost (innermost_loop_behavior *drb, tree ref,
		      class loop *loop, const gimple *stmt)
{
  poly_int64 pbitsize, pbitpos;
  tree base, poffset;
  machine_mode pmode;
  int punsignedp, preversep, pvolatilep;
  affine_iv base_iv, offset_iv;
  tree init, dinit, step;
  bool in_loop = (loop && loop->num);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "analyze_innermost: ");

  base = get_inner_reference (ref, &pbitsize, &pbitpos, &poffset, &pmode,
			      &punsignedp, &preversep, &pvolatilep);
  gcc_assert (base != NULL_TREE);

  poly_int64 pbytepos;
  if (!multiple_p (pbitpos, BITS_PER_UNIT, &pbytepos))
    return opt_result::failure_at (stmt,
				   "failed: bit offset alignment.\n");

  if (preversep)
    return opt_result::failure_at (stmt,
				   "failed: reverse storage order.\n");

  /* Calculate the alignment and misalignment for the inner reference.  */
  unsigned int HOST_WIDE_INT bit_base_misalignment;
  unsigned int bit_base_alignment;
  get_object_alignment_1 (base, &bit_base_alignment, &bit_base_misalignment);

  /* There are no bitfield references remaining in BASE, so the values
     we got back must be whole bytes.  */
  gcc_assert (bit_base_alignment % BITS_PER_UNIT == 0
	      && bit_base_misalignment % BITS_PER_UNIT == 0);
  unsigned int base_alignment = bit_base_alignment / BITS_PER_UNIT;
  poly_int64 base_misalignment = bit_base_misalignment / BITS_PER_UNIT;

  if (TREE_CODE (base) == MEM_REF)
    {
      if (!integer_zerop (TREE_OPERAND (base, 1)))
	{
	  /* Subtract MOFF from the base and add it to POFFSET instead.
	     Adjust the misalignment to reflect the amount we subtracted.  */
	  poly_offset_int moff = mem_ref_offset (base);
	  base_misalignment -= moff.force_shwi ();
	  tree mofft = wide_int_to_tree (sizetype, moff);
	  if (!poffset)
	    poffset = mofft;
	  else
	    poffset = size_binop (PLUS_EXPR, poffset, mofft);
	}
      base = TREE_OPERAND (base, 0);
    }
  else
    base = build_fold_addr_expr (base);

  if (in_loop)
    {
      if (!simple_iv (loop, loop, base, &base_iv, true))
	return opt_result::failure_at
	  (stmt, "failed: evolution of base is not affine.\n");
    }
  else
    {
      base_iv.base = base;
      base_iv.step = ssize_int (0);
      base_iv.no_overflow = true;
    }

  if (!poffset)
    {
      offset_iv.base = ssize_int (0);
      offset_iv.step = ssize_int (0);
    }
  else
    {
      if (!in_loop)
        {
          offset_iv.base = poffset;
          offset_iv.step = ssize_int (0);
        }
      else if (!simple_iv (loop, loop, poffset, &offset_iv, true))
	return opt_result::failure_at
	  (stmt, "failed: evolution of offset is not affine.\n");
    }

  init = ssize_int (pbytepos);

  /* Subtract any constant component from the base and add it to INIT instead.
     Adjust the misalignment to reflect the amount we subtracted.  */
  split_constant_offset (base_iv.base, &base_iv.base, &dinit);
  init = size_binop (PLUS_EXPR, init, dinit);
  base_misalignment -= TREE_INT_CST_LOW (dinit);

  split_constant_offset (offset_iv.base, &offset_iv.base, &dinit);
  init = size_binop (PLUS_EXPR, init, dinit);

  step = size_binop (PLUS_EXPR,
		     fold_convert (ssizetype, base_iv.step),
		     fold_convert (ssizetype, offset_iv.step));

  base = canonicalize_base_object_address (base_iv.base);

  /* See if get_pointer_alignment can guarantee a higher alignment than
     the one we calculated above.  */
  unsigned int HOST_WIDE_INT alt_misalignment;
  unsigned int alt_alignment;
  get_pointer_alignment_1 (base, &alt_alignment, &alt_misalignment);

  /* As above, these values must be whole bytes.  */
  gcc_assert (alt_alignment % BITS_PER_UNIT == 0
	      && alt_misalignment % BITS_PER_UNIT == 0);
  alt_alignment /= BITS_PER_UNIT;
  alt_misalignment /= BITS_PER_UNIT;

  if (base_alignment < alt_alignment)
    {
      base_alignment = alt_alignment;
      base_misalignment = alt_misalignment;
    }

  drb->base_address = base;
  drb->offset = fold_convert (ssizetype, offset_iv.base);
  drb->init = init;
  drb->step = step;
  if (known_misalignment (base_misalignment, base_alignment,
			  &drb->base_misalignment))
    drb->base_alignment = base_alignment;
  else
    {
      drb->base_alignment = known_alignment (base_misalignment);
      drb->base_misalignment = 0;
    }
  drb->offset_alignment = highest_pow2_factor (offset_iv.base);
  drb->step_alignment = highest_pow2_factor (step);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "success.\n");

  return opt_result::success ();
}

/* Return true if OP is a valid component reference for a DR access
   function.  This accepts a subset of what handled_component_p accepts.  */

static bool
access_fn_component_p (tree op)
{
  switch (TREE_CODE (op))
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case ARRAY_REF:
      return true;

    case COMPONENT_REF:
      return TREE_CODE (TREE_TYPE (TREE_OPERAND (op, 0))) == RECORD_TYPE;

    default:
      return false;
    }
}

/* Returns whether BASE can have a access_fn_component_p with BASE
   as base.  */

static bool
base_supports_access_fn_components_p (tree base)
{
  switch (TREE_CODE (TREE_TYPE (base)))
    {
    case COMPLEX_TYPE:
    case ARRAY_TYPE:
    case RECORD_TYPE:
      return true;
    default:
      return false;
    }
}

/* Determines the base object and the list of indices of memory reference
   DR, analyzed in LOOP and instantiated before NEST.  */

static void
dr_analyze_indices (struct indices *dri, tree ref, edge nest, loop_p loop)
{
  /* If analyzing a basic-block there are no indices to analyze
     and thus no access functions.  */
  if (!nest)
    {
      dri->base_object = ref;
      dri->access_fns.create (0);
      return;
    }

  vec<tree> access_fns = vNULL;

  /* REALPART_EXPR and IMAGPART_EXPR can be handled like accesses
     into a two element array with a constant index.  The base is
     then just the immediate underlying object.  */
  if (TREE_CODE (ref) == REALPART_EXPR)
    {
      ref = TREE_OPERAND (ref, 0);
      access_fns.safe_push (integer_zero_node);
    }
  else if (TREE_CODE (ref) == IMAGPART_EXPR)
    {
      ref = TREE_OPERAND (ref, 0);
      access_fns.safe_push (integer_one_node);
    }

  /* Analyze access functions of dimensions we know to be independent.
     The list of component references handled here should be kept in
     sync with access_fn_component_p.  */
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == ARRAY_REF)
	{
	  tree op = TREE_OPERAND (ref, 1);
	  tree access_fn = analyze_scalar_evolution (loop, op);
	  access_fn = instantiate_scev (nest, loop, access_fn);
	  access_fns.safe_push (access_fn);
	}
      else if (TREE_CODE (ref) == COMPONENT_REF
	       && TREE_CODE (TREE_TYPE (TREE_OPERAND (ref, 0))) == RECORD_TYPE)
	{
	  /* For COMPONENT_REFs of records (but not unions!) use the
	     FIELD_DECL offset as constant access function so we can
	     disambiguate a[i].f1 and a[i].f2.  */
	  tree off = component_ref_field_offset (ref);
	  off = size_binop (PLUS_EXPR,
			    size_binop (MULT_EXPR,
					fold_convert (bitsizetype, off),
					bitsize_int (BITS_PER_UNIT)),
			    DECL_FIELD_BIT_OFFSET (TREE_OPERAND (ref, 1)));
	  access_fns.safe_push (off);
	}
      else
	/* If we have an unhandled component we could not translate
	   to an access function stop analyzing.  We have determined
	   our base object in this case.  */
	break;

      ref = TREE_OPERAND (ref, 0);
    }

  /* If the address operand of a MEM_REF base has an evolution in the
     analyzed nest, add it as an additional independent access-function.  */
  if (TREE_CODE (ref) == MEM_REF)
    {
      tree op = TREE_OPERAND (ref, 0);
      tree access_fn = analyze_scalar_evolution (loop, op);
      access_fn = instantiate_scev (nest, loop, access_fn);
      STRIP_NOPS (access_fn);
      if (TREE_CODE (access_fn) == POLYNOMIAL_CHREC)
	{
	  tree memoff = TREE_OPERAND (ref, 1);
	  tree base = initial_condition (access_fn);
	  tree orig_type = TREE_TYPE (base);
	  STRIP_USELESS_TYPE_CONVERSION (base);
	  tree off;
	  split_constant_offset (base, &base, &off);
	  STRIP_USELESS_TYPE_CONVERSION (base);
	  /* Fold the MEM_REF offset into the evolutions initial
	     value to make more bases comparable.  */
	  if (!integer_zerop (memoff))
	    {
	      off = size_binop (PLUS_EXPR, off,
				fold_convert (ssizetype, memoff));
	      memoff = build_int_cst (TREE_TYPE (memoff), 0);
	    }
	  /* Adjust the offset so it is a multiple of the access type
	     size and thus we separate bases that can possibly be used
	     to produce partial overlaps (which the access_fn machinery
	     cannot handle).  */
	  wide_int rem;
	  if (TYPE_SIZE_UNIT (TREE_TYPE (ref))
	      && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (ref))) == INTEGER_CST
	      && !integer_zerop (TYPE_SIZE_UNIT (TREE_TYPE (ref))))
	    rem = wi::mod_trunc
	      (wi::to_wide (off),
	       wi::to_wide (TYPE_SIZE_UNIT (TREE_TYPE (ref))),
	       SIGNED);
	  else
	    /* If we can't compute the remainder simply force the initial
	       condition to zero.  */
	    rem = wi::to_wide (off);
	  off = wide_int_to_tree (ssizetype, wi::to_wide (off) - rem);
	  memoff = wide_int_to_tree (TREE_TYPE (memoff), rem);
	  /* And finally replace the initial condition.  */
	  access_fn = chrec_replace_initial_condition
	      (access_fn, fold_convert (orig_type, off));
	  /* ???  This is still not a suitable base object for
	     dr_may_alias_p - the base object needs to be an
	     access that covers the object as whole.  With
	     an evolution in the pointer this cannot be
	     guaranteed.
	     As a band-aid, mark the access so we can special-case
	     it in dr_may_alias_p.  */
	  tree old = ref;
	  ref = fold_build2_loc (EXPR_LOCATION (ref),
				 MEM_REF, TREE_TYPE (ref),
				 base, memoff);
	  MR_DEPENDENCE_CLIQUE (ref) = MR_DEPENDENCE_CLIQUE (old);
	  MR_DEPENDENCE_BASE (ref) = MR_DEPENDENCE_BASE (old);
	  dri->unconstrained_base = true;
	  access_fns.safe_push (access_fn);
	}
    }
  else if (DECL_P (ref))
    {
      /* Canonicalize DR_BASE_OBJECT to MEM_REF form.  */
      ref = build2 (MEM_REF, TREE_TYPE (ref),
		    build_fold_addr_expr (ref),
		    build_int_cst (reference_alias_ptr_type (ref), 0));
    }

  dri->base_object = ref;
  dri->access_fns = access_fns;
}

/* Extracts the alias analysis information from the memory reference DR.  */

static void
dr_analyze_alias (struct data_reference *dr)
{
  tree ref = DR_REF (dr);
  tree base = get_base_address (ref), addr;

  if (INDIRECT_REF_P (base)
      || TREE_CODE (base) == MEM_REF)
    {
      addr = TREE_OPERAND (base, 0);
      if (TREE_CODE (addr) == SSA_NAME)
	DR_PTR_INFO (dr) = SSA_NAME_PTR_INFO (addr);
    }
}

/* Frees data reference DR.  */

void
free_data_ref (data_reference_p dr)
{
  DR_ACCESS_FNS (dr).release ();
  if (dr->alt_indices.base_object)
    dr->alt_indices.access_fns.release ();
  free (dr);
}

/* Analyze memory reference MEMREF, which is accessed in STMT.
   The reference is a read if IS_READ is true, otherwise it is a write.
   IS_CONDITIONAL_IN_STMT indicates that the reference is conditional
   within STMT, i.e. that it might not occur even if STMT is executed
   and runs to completion.

   Return the data_reference description of MEMREF.  NEST is the outermost
   loop in which the reference should be instantiated, LOOP is the loop
   in which the data reference should be analyzed.  */

struct data_reference *
create_data_ref (edge nest, loop_p loop, tree memref, gimple *stmt,
		 bool is_read, bool is_conditional_in_stmt)
{
  struct data_reference *dr;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Creating dr for ");
      print_generic_expr (dump_file, memref, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  dr = XCNEW (struct data_reference);
  DR_STMT (dr) = stmt;
  DR_REF (dr) = memref;
  DR_IS_READ (dr) = is_read;
  DR_IS_CONDITIONAL_IN_STMT (dr) = is_conditional_in_stmt;

  dr_analyze_innermost (&DR_INNERMOST (dr), memref,
			nest != NULL ? loop : NULL, stmt);
  dr_analyze_indices (&dr->indices, DR_REF (dr), nest, loop);
  dr_analyze_alias (dr);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      unsigned i;
      fprintf (dump_file, "\tbase_address: ");
      print_generic_expr (dump_file, DR_BASE_ADDRESS (dr), TDF_SLIM);
      fprintf (dump_file, "\n\toffset from base address: ");
      print_generic_expr (dump_file, DR_OFFSET (dr), TDF_SLIM);
      fprintf (dump_file, "\n\tconstant offset from base address: ");
      print_generic_expr (dump_file, DR_INIT (dr), TDF_SLIM);
      fprintf (dump_file, "\n\tstep: ");
      print_generic_expr (dump_file, DR_STEP (dr), TDF_SLIM);
      fprintf (dump_file, "\n\tbase alignment: %d", DR_BASE_ALIGNMENT (dr));
      fprintf (dump_file, "\n\tbase misalignment: %d",
	       DR_BASE_MISALIGNMENT (dr));
      fprintf (dump_file, "\n\toffset alignment: %d",
	       DR_OFFSET_ALIGNMENT (dr));
      fprintf (dump_file, "\n\tstep alignment: %d", DR_STEP_ALIGNMENT (dr));
      fprintf (dump_file, "\n\tbase_object: ");
      print_generic_expr (dump_file, DR_BASE_OBJECT (dr), TDF_SLIM);
      fprintf (dump_file, "\n");
      for (i = 0; i < DR_NUM_DIMENSIONS (dr); i++)
	{
	  fprintf (dump_file, "\tAccess function %d: ", i);
	  print_generic_stmt (dump_file, DR_ACCESS_FN (dr, i), TDF_SLIM);
	}
    }

  return dr;
}

/*  A helper function computes order between two tree expressions T1 and T2.
    This is used in comparator functions sorting objects based on the order
    of tree expressions.  The function returns -1, 0, or 1.  */

int
data_ref_compare_tree (tree t1, tree t2)
{
  int i, cmp;
  enum tree_code code;
  char tclass;

  if (t1 == t2)
    return 0;
  if (t1 == NULL)
    return -1;
  if (t2 == NULL)
    return 1;

  STRIP_USELESS_TYPE_CONVERSION (t1);
  STRIP_USELESS_TYPE_CONVERSION (t2);
  if (t1 == t2)
    return 0;

  if (TREE_CODE (t1) != TREE_CODE (t2)
      && ! (CONVERT_EXPR_P (t1) && CONVERT_EXPR_P (t2)))
    return TREE_CODE (t1) < TREE_CODE (t2) ? -1 : 1;

  code = TREE_CODE (t1);
  switch (code)
    {
    case INTEGER_CST:
      return tree_int_cst_compare (t1, t2);

    case STRING_CST:
      if (TREE_STRING_LENGTH (t1) != TREE_STRING_LENGTH (t2))
	return TREE_STRING_LENGTH (t1) < TREE_STRING_LENGTH (t2) ? -1 : 1;
      return memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
		     TREE_STRING_LENGTH (t1));

    case SSA_NAME:
      if (SSA_NAME_VERSION (t1) != SSA_NAME_VERSION (t2))
	return SSA_NAME_VERSION (t1) < SSA_NAME_VERSION (t2) ? -1 : 1;
      break;

    default:
      if (POLY_INT_CST_P (t1))
	return compare_sizes_for_sort (wi::to_poly_widest (t1),
				       wi::to_poly_widest (t2));

      tclass = TREE_CODE_CLASS (code);

      /* For decls, compare their UIDs.  */
      if (tclass == tcc_declaration)
	{
	  if (DECL_UID (t1) != DECL_UID (t2))
	    return DECL_UID (t1) < DECL_UID (t2) ? -1 : 1;
	  break;
	}
      /* For expressions, compare their operands recursively.  */
      else if (IS_EXPR_CODE_CLASS (tclass))
	{
	  for (i = TREE_OPERAND_LENGTH (t1) - 1; i >= 0; --i)
	    {
	      cmp = data_ref_compare_tree (TREE_OPERAND (t1, i),
					   TREE_OPERAND (t2, i));
	      if (cmp != 0)
		return cmp;
	    }
	}
      else
	gcc_unreachable ();
    }

  return 0;
}

/* Return TRUE it's possible to resolve data dependence DDR by runtime alias
   check.  */

opt_result
runtime_alias_check_p (ddr_p ddr, class loop *loop, bool speed_p)
{
  if (dump_enabled_p ())
    dump_printf (MSG_NOTE,
		 "consider run-time aliasing test between %T and %T\n",
		 DR_REF (DDR_A (ddr)), DR_REF (DDR_B (ddr)));

  if (!speed_p)
    return opt_result::failure_at (DR_STMT (DDR_A (ddr)),
				   "runtime alias check not supported when"
				   " optimizing for size.\n");

  /* FORNOW: We don't support versioning with outer-loop in either
     vectorization or loop distribution.  */
  if (loop != NULL && loop->inner != NULL)
    return opt_result::failure_at (DR_STMT (DDR_A (ddr)),
				   "runtime alias check not supported for"
				   " outer loop.\n");

  return opt_result::success ();
}

/* Operator == between two dr_with_seg_len objects.

   This equality operator is used to make sure two data refs
   are the same one so that we will consider to combine the
   aliasing checks of those two pairs of data dependent data
   refs.  */

static bool
operator == (const dr_with_seg_len& d1,
	     const dr_with_seg_len& d2)
{
  return (operand_equal_p (DR_BASE_ADDRESS (d1.dr),
			   DR_BASE_ADDRESS (d2.dr), 0)
	  && data_ref_compare_tree (DR_OFFSET (d1.dr), DR_OFFSET (d2.dr)) == 0
	  && data_ref_compare_tree (DR_INIT (d1.dr), DR_INIT (d2.dr)) == 0
	  && data_ref_compare_tree (d1.seg_len, d2.seg_len) == 0
	  && known_eq (d1.access_size, d2.access_size)
	  && d1.align == d2.align);
}

/* Comparison function for sorting objects of dr_with_seg_len_pair_t
   so that we can combine aliasing checks in one scan.  */

static int
comp_dr_with_seg_len_pair (const void *pa_, const void *pb_)
{
  const dr_with_seg_len_pair_t* pa = (const dr_with_seg_len_pair_t *) pa_;
  const dr_with_seg_len_pair_t* pb = (const dr_with_seg_len_pair_t *) pb_;
  const dr_with_seg_len &a1 = pa->first, &a2 = pa->second;
  const dr_with_seg_len &b1 = pb->first, &b2 = pb->second;

  /* For DR pairs (a, b) and (c, d), we only consider to merge the alias checks
     if a and c have the same basic address snd step, and b and d have the same
     address and step.  Therefore, if any a&c or b&d don't have the same address
     and step, we don't care the order of those two pairs after sorting.  */
  int comp_res;

  if ((comp_res = data_ref_compare_tree (DR_BASE_ADDRESS (a1.dr),
					 DR_BASE_ADDRESS (b1.dr))) != 0)
    return comp_res;
  if ((comp_res = data_ref_compare_tree (DR_BASE_ADDRESS (a2.dr),
					 DR_BASE_ADDRESS (b2.dr))) != 0)
    return comp_res;
  if ((comp_res = data_ref_compare_tree (DR_STEP (a1.dr),
					 DR_STEP (b1.dr))) != 0)
    return comp_res;
  if ((comp_res = data_ref_compare_tree (DR_STEP (a2.dr),
					 DR_STEP (b2.dr))) != 0)
    return comp_res;
  if ((comp_res = data_ref_compare_tree (DR_OFFSET (a1.dr),
					 DR_OFFSET (b1.dr))) != 0)
    return comp_res;
  if ((comp_res = data_ref_compare_tree (DR_INIT (a1.dr),
					 DR_INIT (b1.dr))) != 0)
    return comp_res;
  if ((comp_res = data_ref_compare_tree (DR_OFFSET (a2.dr),
					 DR_OFFSET (b2.dr))) != 0)
    return comp_res;
  if ((comp_res = data_ref_compare_tree (DR_INIT (a2.dr),
					 DR_INIT (b2.dr))) != 0)
    return comp_res;

  return 0;
}

/* Dump information about ALIAS_PAIR, indenting each line by INDENT.  */

static void
dump_alias_pair (dr_with_seg_len_pair_t *alias_pair, const char *indent)
{
  dump_printf (MSG_NOTE, "%sreference:      %T vs. %T\n", indent,
	       DR_REF (alias_pair->first.dr),
	       DR_REF (alias_pair->second.dr));

  dump_printf (MSG_NOTE, "%ssegment length: %T", indent,
	       alias_pair->first.seg_len);
  if (!operand_equal_p (alias_pair->first.seg_len,
			alias_pair->second.seg_len, 0))
    dump_printf (MSG_NOTE, " vs. %T", alias_pair->second.seg_len);

  dump_printf (MSG_NOTE, "\n%saccess size:    ", indent);
  dump_dec (MSG_NOTE, alias_pair->first.access_size);
  if (maybe_ne (alias_pair->first.access_size, alias_pair->second.access_size))
    {
      dump_printf (MSG_NOTE, " vs. ");
      dump_dec (MSG_NOTE, alias_pair->second.access_size);
    }

  dump_printf (MSG_NOTE, "\n%salignment:      %d", indent,
	       alias_pair->first.align);
  if (alias_pair->first.align != alias_pair->second.align)
    dump_printf (MSG_NOTE, " vs. %d", alias_pair->second.align);

  dump_printf (MSG_NOTE, "\n%sflags:         ", indent);
  if (alias_pair->flags & DR_ALIAS_RAW)
    dump_printf (MSG_NOTE, " RAW");
  if (alias_pair->flags & DR_ALIAS_WAR)
    dump_printf (MSG_NOTE, " WAR");
  if (alias_pair->flags & DR_ALIAS_WAW)
    dump_printf (MSG_NOTE, " WAW");
  if (alias_pair->flags & DR_ALIAS_ARBITRARY)
    dump_printf (MSG_NOTE, " ARBITRARY");
  if (alias_pair->flags & DR_ALIAS_SWAPPED)
    dump_printf (MSG_NOTE, " SWAPPED");
  if (alias_pair->flags & DR_ALIAS_UNSWAPPED)
    dump_printf (MSG_NOTE, " UNSWAPPED");
  if (alias_pair->flags & DR_ALIAS_MIXED_STEPS)
    dump_printf (MSG_NOTE, " MIXED_STEPS");
  if (alias_pair->flags == 0)
    dump_printf (MSG_NOTE, " <none>");
  dump_printf (MSG_NOTE, "\n");
}

/* Merge alias checks recorded in ALIAS_PAIRS and remove redundant ones.
   FACTOR is number of iterations that each data reference is accessed.

   Basically, for each pair of dependent data refs store_ptr_0 & load_ptr_0,
   we create an expression:

   ((store_ptr_0 + store_segment_length_0) <= load_ptr_0)
   || (load_ptr_0 + load_segment_length_0) <= store_ptr_0))

   for aliasing checks.  However, in some cases we can decrease the number
   of checks by combining two checks into one.  For example, suppose we have
   another pair of data refs store_ptr_0 & load_ptr_1, and if the following
   condition is satisfied:

   load_ptr_0 < load_ptr_1  &&
   load_ptr_1 - load_ptr_0 - load_segment_length_0 < store_segment_length_0

   (this condition means, in each iteration of vectorized loop, the accessed
   memory of store_ptr_0 cannot be between the memory of load_ptr_0 and
   load_ptr_1.)

   we then can use only the following expression to finish the alising checks
   between store_ptr_0 & load_ptr_0 and store_ptr_0 & load_ptr_1:

   ((store_ptr_0 + store_segment_length_0) <= load_ptr_0)
   || (load_ptr_1 + load_segment_length_1 <= store_ptr_0))

   Note that we only consider that load_ptr_0 and load_ptr_1 have the same
   basic address.  */

void
prune_runtime_alias_test_list (vec<dr_with_seg_len_pair_t> *alias_pairs,
			       poly_uint64)
{
  if (alias_pairs->is_empty ())
    return;

  /* Canonicalize each pair so that the base components are ordered wrt
     data_ref_compare_tree.  This allows the loop below to merge more
     cases.  */
  unsigned int i;
  dr_with_seg_len_pair_t *alias_pair;
  FOR_EACH_VEC_ELT (*alias_pairs, i, alias_pair)
    {
      data_reference_p dr_a = alias_pair->first.dr;
      data_reference_p dr_b = alias_pair->second.dr;
      int comp_res = data_ref_compare_tree (DR_BASE_ADDRESS (dr_a),
					    DR_BASE_ADDRESS (dr_b));
      if (comp_res == 0)
	comp_res = data_ref_compare_tree (DR_OFFSET (dr_a), DR_OFFSET (dr_b));
      if (comp_res == 0)
	comp_res = data_ref_compare_tree (DR_INIT (dr_a), DR_INIT (dr_b));
      if (comp_res > 0)
	{
	  std::swap (alias_pair->first, alias_pair->second);
	  alias_pair->flags |= DR_ALIAS_SWAPPED;
	}
      else
	alias_pair->flags |= DR_ALIAS_UNSWAPPED;
    }

  /* Sort the collected data ref pairs so that we can scan them once to
     combine all possible aliasing checks.  */
  alias_pairs->qsort (comp_dr_with_seg_len_pair);

  /* Scan the sorted dr pairs and check if we can combine alias checks
     of two neighboring dr pairs.  */
  unsigned int last = 0;
  for (i = 1; i < alias_pairs->length (); ++i)
    {
      /* Deal with two ddrs (dr_a1, dr_b1) and (dr_a2, dr_b2).  */
      dr_with_seg_len_pair_t *alias_pair1 = &(*alias_pairs)[last];
      dr_with_seg_len_pair_t *alias_pair2 = &(*alias_pairs)[i];

      dr_with_seg_len *dr_a1 = &alias_pair1->first;
      dr_with_seg_len *dr_b1 = &alias_pair1->second;
      dr_with_seg_len *dr_a2 = &alias_pair2->first;
      dr_with_seg_len *dr_b2 = &alias_pair2->second;

      /* Remove duplicate data ref pairs.  */
      if (*dr_a1 == *dr_a2 && *dr_b1 == *dr_b2)
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "found equal ranges %T, %T and %T, %T\n",
			 DR_REF (dr_a1->dr), DR_REF (dr_b1->dr),
			 DR_REF (dr_a2->dr), DR_REF (dr_b2->dr));
	  alias_pair1->flags |= alias_pair2->flags;
	  continue;
	}

      /* Assume that we won't be able to merge the pairs, then correct
	 if we do.  */
      last += 1;
      if (last != i)
	(*alias_pairs)[last] = (*alias_pairs)[i];

      if (*dr_a1 == *dr_a2 || *dr_b1 == *dr_b2)
	{
	  /* We consider the case that DR_B1 and DR_B2 are same memrefs,
	     and DR_A1 and DR_A2 are two consecutive memrefs.  */
	  if (*dr_a1 == *dr_a2)
	    {
	      std::swap (dr_a1, dr_b1);
	      std::swap (dr_a2, dr_b2);
	    }

	  poly_int64 init_a1, init_a2;
	  /* Only consider cases in which the distance between the initial
	     DR_A1 and the initial DR_A2 is known at compile time.  */
	  if (!operand_equal_p (DR_BASE_ADDRESS (dr_a1->dr),
				DR_BASE_ADDRESS (dr_a2->dr), 0)
	      || !operand_equal_p (DR_OFFSET (dr_a1->dr),
				   DR_OFFSET (dr_a2->dr), 0)
	      || !poly_int_tree_p (DR_INIT (dr_a1->dr), &init_a1)
	      || !poly_int_tree_p (DR_INIT (dr_a2->dr), &init_a2))
	    continue;

	  /* Don't combine if we can't tell which one comes first.  */
	  if (!ordered_p (init_a1, init_a2))
	    continue;

	  /* Work out what the segment length would be if we did combine
	     DR_A1 and DR_A2:

	     - If DR_A1 and DR_A2 have equal lengths, that length is
	       also the combined length.

	     - If DR_A1 and DR_A2 both have negative "lengths", the combined
	       length is the lower bound on those lengths.

	     - If DR_A1 and DR_A2 both have positive lengths, the combined
	       length is the upper bound on those lengths.

	     Other cases are unlikely to give a useful combination.

	     The lengths both have sizetype, so the sign is taken from
	     the step instead.  */
	  poly_uint64 new_seg_len = 0;
	  bool new_seg_len_p = !operand_equal_p (dr_a1->seg_len,
						 dr_a2->seg_len, 0);
	  if (new_seg_len_p)
	    {
	      poly_uint64 seg_len_a1, seg_len_a2;
	      if (!poly_int_tree_p (dr_a1->seg_len, &seg_len_a1)
		  || !poly_int_tree_p (dr_a2->seg_len, &seg_len_a2))
		continue;

	      tree indicator_a = dr_direction_indicator (dr_a1->dr);
	      if (TREE_CODE (indicator_a) != INTEGER_CST)
		continue;

	      tree indicator_b = dr_direction_indicator (dr_a2->dr);
	      if (TREE_CODE (indicator_b) != INTEGER_CST)
		continue;

	      int sign_a = tree_int_cst_sgn (indicator_a);
	      int sign_b = tree_int_cst_sgn (indicator_b);

	      if (sign_a <= 0 && sign_b <= 0)
		new_seg_len = lower_bound (seg_len_a1, seg_len_a2);
	      else if (sign_a >= 0 && sign_b >= 0)
		new_seg_len = upper_bound (seg_len_a1, seg_len_a2);
	      else
		continue;
	    }
	  /* At this point we're committed to merging the refs.  */

	  /* Make sure dr_a1 starts left of dr_a2.  */
	  if (maybe_gt (init_a1, init_a2))
	    {
	      std::swap (*dr_a1, *dr_a2);
	      std::swap (init_a1, init_a2);
	    }

	  /* The DR_Bs are equal, so only the DR_As can introduce
	     mixed steps.  */
	  if (!operand_equal_p (DR_STEP (dr_a1->dr), DR_STEP (dr_a2->dr), 0))
	    alias_pair1->flags |= DR_ALIAS_MIXED_STEPS;

	  if (new_seg_len_p)
	    {
	      dr_a1->seg_len = build_int_cst (TREE_TYPE (dr_a1->seg_len),
					      new_seg_len);
	      dr_a1->align = MIN (dr_a1->align, known_alignment (new_seg_len));
	    }

	  /* This is always positive due to the swap above.  */
	  poly_uint64 diff = init_a2 - init_a1;

	  /* The new check will start at DR_A1.  Make sure that its access
	     size encompasses the initial DR_A2.  */
	  if (maybe_lt (dr_a1->access_size, diff + dr_a2->access_size))
	    {
	      dr_a1->access_size = upper_bound (dr_a1->access_size,
						diff + dr_a2->access_size);
	      unsigned int new_align = known_alignment (dr_a1->access_size);
	      dr_a1->align = MIN (dr_a1->align, new_align);
	    }
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "merging ranges for %T, %T and %T, %T\n",
			 DR_REF (dr_a1->dr), DR_REF (dr_b1->dr),
			 DR_REF (dr_a2->dr), DR_REF (dr_b2->dr));
	  alias_pair1->flags |= alias_pair2->flags;
	  last -= 1;
	}
    }
  alias_pairs->truncate (last + 1);

  /* Try to restore the original dr_with_seg_len order within each
     dr_with_seg_len_pair_t.  If we ended up combining swapped and
     unswapped pairs into the same check, we have to invalidate any
     RAW, WAR and WAW information for it.  */
  if (dump_enabled_p ())
    dump_printf (MSG_NOTE, "merged alias checks:\n");
  FOR_EACH_VEC_ELT (*alias_pairs, i, alias_pair)
    {
      unsigned int swap_mask = (DR_ALIAS_SWAPPED | DR_ALIAS_UNSWAPPED);
      unsigned int swapped = (alias_pair->flags & swap_mask);
      if (swapped == DR_ALIAS_SWAPPED)
	std::swap (alias_pair->first, alias_pair->second);
      else if (swapped != DR_ALIAS_UNSWAPPED)
	alias_pair->flags |= DR_ALIAS_ARBITRARY;
      alias_pair->flags &= ~swap_mask;
      if (dump_enabled_p ())
	dump_alias_pair (alias_pair, "  ");
    }
}

/* A subroutine of create_intersect_range_checks, with a subset of the
   same arguments.  Try to use IFN_CHECK_RAW_PTRS and IFN_CHECK_WAR_PTRS
   to optimize cases in which the references form a simple RAW, WAR or
   WAR dependence.  */

static bool
create_ifn_alias_checks (tree *cond_expr,
			 const dr_with_seg_len_pair_t &alias_pair)
{
  const dr_with_seg_len& dr_a = alias_pair.first;
  const dr_with_seg_len& dr_b = alias_pair.second;

  /* Check for cases in which:

     (a) we have a known RAW, WAR or WAR dependence
     (b) the accesses are well-ordered in both the original and new code
	 (see the comment above the DR_ALIAS_* flags for details); and
     (c) the DR_STEPs describe all access pairs covered by ALIAS_PAIR.  */
  if (alias_pair.flags & ~(DR_ALIAS_RAW | DR_ALIAS_WAR | DR_ALIAS_WAW))
    return false;

  /* Make sure that both DRs access the same pattern of bytes,
     with a constant length and step.  */
  poly_uint64 seg_len;
  if (!operand_equal_p (dr_a.seg_len, dr_b.seg_len, 0)
      || !poly_int_tree_p (dr_a.seg_len, &seg_len)
      || maybe_ne (dr_a.access_size, dr_b.access_size)
      || !operand_equal_p (DR_STEP (dr_a.dr), DR_STEP (dr_b.dr), 0)
      || !tree_fits_uhwi_p (DR_STEP (dr_a.dr)))
    return false;

  unsigned HOST_WIDE_INT bytes = tree_to_uhwi (DR_STEP (dr_a.dr));
  tree addr_a = DR_BASE_ADDRESS (dr_a.dr);
  tree addr_b = DR_BASE_ADDRESS (dr_b.dr);

  /* See whether the target suports what we want to do.  WAW checks are
     equivalent to WAR checks here.  */
  internal_fn ifn = (alias_pair.flags & DR_ALIAS_RAW
		     ? IFN_CHECK_RAW_PTRS
		     : IFN_CHECK_WAR_PTRS);
  unsigned int align = MIN (dr_a.align, dr_b.align);
  poly_uint64 full_length = seg_len + bytes;
  if (!internal_check_ptrs_fn_supported_p (ifn, TREE_TYPE (addr_a),
					   full_length, align))
    {
      full_length = seg_len + dr_a.access_size;
      if (!internal_check_ptrs_fn_supported_p (ifn, TREE_TYPE (addr_a),
					       full_length, align))
	return false;
    }

  /* Commit to using this form of test.  */
  addr_a = fold_build_pointer_plus (addr_a, DR_OFFSET (dr_a.dr));
  addr_a = fold_build_pointer_plus (addr_a, DR_INIT (dr_a.dr));

  addr_b = fold_build_pointer_plus (addr_b, DR_OFFSET (dr_b.dr));
  addr_b = fold_build_pointer_plus (addr_b, DR_INIT (dr_b.dr));

  *cond_expr = build_call_expr_internal_loc (UNKNOWN_LOCATION,
					     ifn, boolean_type_node,
					     4, addr_a, addr_b,
					     size_int (full_length),
					     size_int (align));

  if (dump_enabled_p ())
    {
      if (ifn == IFN_CHECK_RAW_PTRS)
	dump_printf (MSG_NOTE, "using an IFN_CHECK_RAW_PTRS test\n");
      else
	dump_printf (MSG_NOTE, "using an IFN_CHECK_WAR_PTRS test\n");
    }
  return true;
}

/* Try to generate a runtime condition that is true if ALIAS_PAIR is
   free of aliases, using a condition based on index values instead
   of a condition based on addresses.  Return true on success,
   storing the condition in *COND_EXPR.

   This can only be done if the two data references in ALIAS_PAIR access
   the same array object and the index is the only difference.  For example,
   if the two data references are DR_A and DR_B:

                       DR_A                           DR_B
      data-ref         arr[i]                         arr[j]
      base_object      arr                            arr
      index            {i_0, +, 1}_loop               {j_0, +, 1}_loop

   The addresses and their index are like:

        |<- ADDR_A    ->|          |<- ADDR_B    ->|
     ------------------------------------------------------->
        |   |   |   |   |          |   |   |   |   |
     ------------------------------------------------------->
        i_0 ...         i_0+4      j_0 ...         j_0+4

   We can create expression based on index rather than address:

     (unsigned) (i_0 - j_0 + 3) <= 6

   i.e. the indices are less than 4 apart.

   Note evolution step of index needs to be considered in comparison.  */

static bool
create_intersect_range_checks_index (class loop *loop, tree *cond_expr,
				     const dr_with_seg_len_pair_t &alias_pair)
{
  const dr_with_seg_len &dr_a = alias_pair.first;
  const dr_with_seg_len &dr_b = alias_pair.second;
  if ((alias_pair.flags & DR_ALIAS_MIXED_STEPS)
      || integer_zerop (DR_STEP (dr_a.dr))
      || integer_zerop (DR_STEP (dr_b.dr))
      || DR_NUM_DIMENSIONS (dr_a.dr) != DR_NUM_DIMENSIONS (dr_b.dr))
    return false;

  poly_uint64 seg_len1, seg_len2;
  if (!poly_int_tree_p (dr_a.seg_len, &seg_len1)
      || !poly_int_tree_p (dr_b.seg_len, &seg_len2))
    return false;

  if (!tree_fits_shwi_p (DR_STEP (dr_a.dr)))
    return false;

  if (!operand_equal_p (DR_BASE_OBJECT (dr_a.dr), DR_BASE_OBJECT (dr_b.dr), 0))
    return false;

  if (!operand_equal_p (DR_STEP (dr_a.dr), DR_STEP (dr_b.dr), 0))
    return false;

  gcc_assert (TREE_CODE (DR_STEP (dr_a.dr)) == INTEGER_CST);

  bool neg_step = tree_int_cst_compare (DR_STEP (dr_a.dr), size_zero_node) < 0;
  unsigned HOST_WIDE_INT abs_step = tree_to_shwi (DR_STEP (dr_a.dr));
  if (neg_step)
    {
      abs_step = -abs_step;
      seg_len1 = (-wi::to_poly_wide (dr_a.seg_len)).force_uhwi ();
      seg_len2 = (-wi::to_poly_wide (dr_b.seg_len)).force_uhwi ();
    }

  /* Infer the number of iterations with which the memory segment is accessed
     by DR.  In other words, alias is checked if memory segment accessed by
     DR_A in some iterations intersect with memory segment accessed by DR_B
     in the same amount iterations.
     Note segnment length is a linear function of number of iterations with
     DR_STEP as the coefficient.  */
  poly_uint64 niter_len1, niter_len2;
  if (!can_div_trunc_p (seg_len1 + abs_step - 1, abs_step, &niter_len1)
      || !can_div_trunc_p (seg_len2 + abs_step - 1, abs_step, &niter_len2))
    return false;

  /* Divide each access size by the byte step, rounding up.  */
  poly_uint64 niter_access1, niter_access2;
  if (!can_div_trunc_p (dr_a.access_size + abs_step - 1,
			abs_step, &niter_access1)
      || !can_div_trunc_p (dr_b.access_size + abs_step - 1,
			   abs_step, &niter_access2))
    return false;

  bool waw_or_war_p = (alias_pair.flags & ~(DR_ALIAS_WAR | DR_ALIAS_WAW)) == 0;

  int found = -1;
  for (unsigned int i = 0; i < DR_NUM_DIMENSIONS (dr_a.dr); i++)
    {
      tree access1 = DR_ACCESS_FN (dr_a.dr, i);
      tree access2 = DR_ACCESS_FN (dr_b.dr, i);
      /* Two indices must be the same if they are not scev, or not scev wrto
	 current loop being vecorized.  */
      if (TREE_CODE (access1) != POLYNOMIAL_CHREC
	  || TREE_CODE (access2) != POLYNOMIAL_CHREC
	  || CHREC_VARIABLE (access1) != (unsigned)loop->num
	  || CHREC_VARIABLE (access2) != (unsigned)loop->num)
	{
	  if (operand_equal_p (access1, access2, 0))
	    continue;

	  return false;
	}
      if (found >= 0)
	return false;
      found = i;
    }

  /* Ought not to happen in practice, since if all accesses are equal then the
     alias should be decidable at compile time.  */
  if (found < 0)
    return false;

  /* The two indices must have the same step.  */
  tree access1 = DR_ACCESS_FN (dr_a.dr, found);
  tree access2 = DR_ACCESS_FN (dr_b.dr, found);
  if (!operand_equal_p (CHREC_RIGHT (access1), CHREC_RIGHT (access2), 0))
    return false;

  tree idx_step = CHREC_RIGHT (access1);
  /* Index must have const step, otherwise DR_STEP won't be constant.  */
  gcc_assert (TREE_CODE (idx_step) == INTEGER_CST);
  /* Index must evaluate in the same direction as DR.  */
  gcc_assert (!neg_step || tree_int_cst_sign_bit (idx_step) == 1);

  tree min1 = CHREC_LEFT (access1);
  tree min2 = CHREC_LEFT (access2);
  if (!types_compatible_p (TREE_TYPE (min1), TREE_TYPE (min2)))
    return false;

  /* Ideally, alias can be checked against loop's control IV, but we
     need to prove linear mapping between control IV and reference
     index.  Although that should be true, we check against (array)
     index of data reference.  Like segment length, index length is
     linear function of the number of iterations with index_step as
     the coefficient, i.e, niter_len * idx_step.  */
  offset_int abs_idx_step = offset_int::from (wi::to_wide (idx_step),
					      SIGNED);
  if (neg_step)
    abs_idx_step = -abs_idx_step;
  poly_offset_int idx_len1 = abs_idx_step * niter_len1;
  poly_offset_int idx_len2 = abs_idx_step * niter_len2;
  poly_offset_int idx_access1 = abs_idx_step * niter_access1;
  poly_offset_int idx_access2 = abs_idx_step * niter_access2;

  gcc_assert (known_ge (idx_len1, 0)
	      && known_ge (idx_len2, 0)
	      && known_ge (idx_access1, 0)
	      && known_ge (idx_access2, 0));

  /* Each access has the following pattern, with lengths measured
     in units of INDEX:

	  <-- idx_len -->
	  <--- A: -ve step --->
	  +-----+-------+-----+-------+-----+
	  | n-1 | ..... |  0  | ..... | n-1 |
	  +-----+-------+-----+-------+-----+
			<--- B: +ve step --->
			<-- idx_len -->
			|
		       min

     where "n" is the number of scalar iterations covered by the segment
     and where each access spans idx_access units.

     A is the range of bytes accessed when the step is negative,
     B is the range when the step is positive.

     When checking for general overlap, we need to test whether
     the range:

       [min1 + low_offset1, min1 + high_offset1 + idx_access1 - 1]

     overlaps:

       [min2 + low_offset2, min2 + high_offset2 + idx_access2 - 1]

     where:

	low_offsetN = +ve step ? 0 : -idx_lenN;
       high_offsetN = +ve step ? idx_lenN : 0;

     This is equivalent to testing whether:

       min1 + low_offset1 <= min2 + high_offset2 + idx_access2 - 1
       && min2 + low_offset2 <= min1 + high_offset1 + idx_access1 - 1

     Converting this into a single test, there is an overlap if:

       0 <= min2 - min1 + bias <= limit

     where  bias = high_offset2 + idx_access2 - 1 - low_offset1
	   limit = (high_offset1 - low_offset1 + idx_access1 - 1)
		 + (high_offset2 - low_offset2 + idx_access2 - 1)
      i.e. limit = idx_len1 + idx_access1 - 1 + idx_len2 + idx_access2 - 1

     Combining the tests requires limit to be computable in an unsigned
     form of the index type; if it isn't, we fall back to the usual
     pointer-based checks.

     We can do better if DR_B is a write and if DR_A and DR_B are
     well-ordered in both the original and the new code (see the
     comment above the DR_ALIAS_* flags for details).  In this case
     we know that for each i in [0, n-1], the write performed by
     access i of DR_B occurs after access numbers j<=i of DR_A in
     both the original and the new code.  Any write or anti
     dependencies wrt those DR_A accesses are therefore maintained.

     We just need to make sure that each individual write in DR_B does not
     overlap any higher-indexed access in DR_A; such DR_A accesses happen
     after the DR_B access in the original code but happen before it in
     the new code.

     We know the steps for both accesses are equal, so by induction, we
     just need to test whether the first write of DR_B overlaps a later
     access of DR_A.  In other words, we need to move min1 along by
     one iteration:

       min1' = min1 + idx_step

     and use the ranges:

       [min1' + low_offset1', min1' + high_offset1' + idx_access1 - 1]

     and:

       [min2, min2 + idx_access2 - 1]

     where:

	low_offset1' = +ve step ? 0 : -(idx_len1 - |idx_step|)
       high_offset1' = +ve_step ? idx_len1 - |idx_step| : 0.  */
  if (waw_or_war_p)
    idx_len1 -= abs_idx_step;

  poly_offset_int limit = idx_len1 + idx_access1 - 1 + idx_access2 - 1;
  if (!waw_or_war_p)
    limit += idx_len2;

  tree utype = unsigned_type_for (TREE_TYPE (min1));
  if (!wi::fits_to_tree_p (limit, utype))
    return false;

  poly_offset_int low_offset1 = neg_step ? -idx_len1 : 0;
  poly_offset_int high_offset2 = neg_step || waw_or_war_p ? 0 : idx_len2;
  poly_offset_int bias = high_offset2 + idx_access2 - 1 - low_offset1;
  /* Equivalent to adding IDX_STEP to MIN1.  */
  if (waw_or_war_p)
    bias -= wi::to_offset (idx_step);

  tree subject = fold_build2 (MINUS_EXPR, utype,
			      fold_convert (utype, min2),
			      fold_convert (utype, min1));
  subject = fold_build2 (PLUS_EXPR, utype, subject,
			 wide_int_to_tree (utype, bias));
  tree part_cond_expr = fold_build2 (GT_EXPR, boolean_type_node, subject,
				     wide_int_to_tree (utype, limit));
  if (*cond_expr)
    *cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			      *cond_expr, part_cond_expr);
  else
    *cond_expr = part_cond_expr;
  if (dump_enabled_p ())
    {
      if (waw_or_war_p)
	dump_printf (MSG_NOTE, "using an index-based WAR/WAW test\n");
      else
	dump_printf (MSG_NOTE, "using an index-based overlap test\n");
    }
  return true;
}

/* A subroutine of create_intersect_range_checks, with a subset of the
   same arguments.  Try to optimize cases in which the second access
   is a write and in which some overlap is valid.  */

static bool
create_waw_or_war_checks (tree *cond_expr,
			  const dr_with_seg_len_pair_t &alias_pair)
{
  const dr_with_seg_len& dr_a = alias_pair.first;
  const dr_with_seg_len& dr_b = alias_pair.second;

  /* Check for cases in which:

     (a) DR_B is always a write;
     (b) the accesses are well-ordered in both the original and new code
	 (see the comment above the DR_ALIAS_* flags for details); and
     (c) the DR_STEPs describe all access pairs covered by ALIAS_PAIR.  */
  if (alias_pair.flags & ~(DR_ALIAS_WAR | DR_ALIAS_WAW))
    return false;

  /* Check for equal (but possibly variable) steps.  */
  tree step = DR_STEP (dr_a.dr);
  if (!operand_equal_p (step, DR_STEP (dr_b.dr)))
    return false;

  /* Make sure that we can operate on sizetype without loss of precision.  */
  tree addr_type = TREE_TYPE (DR_BASE_ADDRESS (dr_a.dr));
  if (TYPE_PRECISION (addr_type) != TYPE_PRECISION (sizetype))
    return false;

  /* All addresses involved are known to have a common alignment ALIGN.
     We can therefore subtract ALIGN from an exclusive endpoint to get
     an inclusive endpoint.  In the best (and common) case, ALIGN is the
     same as the access sizes of both DRs, and so subtracting ALIGN
     cancels out the addition of an access size.  */
  unsigned int align = MIN (dr_a.align, dr_b.align);
  poly_uint64 last_chunk_a = dr_a.access_size - align;
  poly_uint64 last_chunk_b = dr_b.access_size - align;

  /* Get a boolean expression that is true when the step is negative.  */
  tree indicator = dr_direction_indicator (dr_a.dr);
  tree neg_step = fold_build2 (LT_EXPR, boolean_type_node,
			       fold_convert (ssizetype, indicator),
			       ssize_int (0));

  /* Get lengths in sizetype.  */
  tree seg_len_a
    = fold_convert (sizetype, rewrite_to_non_trapping_overflow (dr_a.seg_len));
  step = fold_convert (sizetype, rewrite_to_non_trapping_overflow (step));

  /* Each access has the following pattern:

	  <- |seg_len| ->
	  <--- A: -ve step --->
	  +-----+-------+-----+-------+-----+
	  | n-1 | ..... |  0  | ..... | n-1 |
	  +-----+-------+-----+-------+-----+
			<--- B: +ve step --->
			<- |seg_len| ->
			|
		   base address

     where "n" is the number of scalar iterations covered by the segment.

     A is the range of bytes accessed when the step is negative,
     B is the range when the step is positive.

     We know that DR_B is a write.  We also know (from checking that
     DR_A and DR_B are well-ordered) that for each i in [0, n-1],
     the write performed by access i of DR_B occurs after access numbers
     j<=i of DR_A in both the original and the new code.  Any write or
     anti dependencies wrt those DR_A accesses are therefore maintained.

     We just need to make sure that each individual write in DR_B does not
     overlap any higher-indexed access in DR_A; such DR_A accesses happen
     after the DR_B access in the original code but happen before it in
     the new code.

     We know the steps for both accesses are equal, so by induction, we
     just need to test whether the first write of DR_B overlaps a later
     access of DR_A.  In other words, we need to move addr_a along by
     one iteration:

       addr_a' = addr_a + step

     and check whether:

       [addr_b, addr_b + last_chunk_b]

     overlaps:

       [addr_a' + low_offset_a, addr_a' + high_offset_a + last_chunk_a]

     where [low_offset_a, high_offset_a] spans accesses [1, n-1].  I.e.:

	low_offset_a = +ve step ? 0 : seg_len_a - step
       high_offset_a = +ve step ? seg_len_a - step : 0

     This is equivalent to testing whether:

       addr_a' + low_offset_a <= addr_b + last_chunk_b
       && addr_b <= addr_a' + high_offset_a + last_chunk_a

     Converting this into a single test, there is an overlap if:

       0 <= addr_b + last_chunk_b - addr_a' - low_offset_a <= limit

     where limit = high_offset_a - low_offset_a + last_chunk_a + last_chunk_b

     If DR_A is performed, limit + |step| - last_chunk_b is known to be
     less than the size of the object underlying DR_A.  We also know
     that last_chunk_b <= |step|; this is checked elsewhere if it isn't
     guaranteed at compile time.  There can therefore be no overflow if
     "limit" is calculated in an unsigned type with pointer precision.  */
  tree addr_a = fold_build_pointer_plus (DR_BASE_ADDRESS (dr_a.dr),
					 DR_OFFSET (dr_a.dr));
  addr_a = fold_build_pointer_plus (addr_a, DR_INIT (dr_a.dr));

  tree addr_b = fold_build_pointer_plus (DR_BASE_ADDRESS (dr_b.dr),
					 DR_OFFSET (dr_b.dr));
  addr_b = fold_build_pointer_plus (addr_b, DR_INIT (dr_b.dr));

  /* Advance ADDR_A by one iteration and adjust the length to compensate.  */
  addr_a = fold_build_pointer_plus (addr_a, step);
  tree seg_len_a_minus_step = fold_build2 (MINUS_EXPR, sizetype,
					   seg_len_a, step);
  if (!CONSTANT_CLASS_P (seg_len_a_minus_step))
    seg_len_a_minus_step = build1 (SAVE_EXPR, sizetype, seg_len_a_minus_step);

  tree low_offset_a = fold_build3 (COND_EXPR, sizetype, neg_step,
				   seg_len_a_minus_step, size_zero_node);
  if (!CONSTANT_CLASS_P (low_offset_a))
    low_offset_a = build1 (SAVE_EXPR, sizetype, low_offset_a);

  /* We could use COND_EXPR <neg_step, size_zero_node, seg_len_a_minus_step>,
     but it's usually more efficient to reuse the LOW_OFFSET_A result.  */
  tree high_offset_a = fold_build2 (MINUS_EXPR, sizetype, seg_len_a_minus_step,
				    low_offset_a);

  /* The amount added to addr_b - addr_a'.  */
  tree bias = fold_build2 (MINUS_EXPR, sizetype,
			   size_int (last_chunk_b), low_offset_a);

  tree limit = fold_build2 (MINUS_EXPR, sizetype, high_offset_a, low_offset_a);
  limit = fold_build2 (PLUS_EXPR, sizetype, limit,
		       size_int (last_chunk_a + last_chunk_b));

  tree subject = fold_build2 (POINTER_DIFF_EXPR, ssizetype, addr_b, addr_a);
  subject = fold_build2 (PLUS_EXPR, sizetype,
			 fold_convert (sizetype, subject), bias);

  *cond_expr = fold_build2 (GT_EXPR, boolean_type_node, subject, limit);
  if (dump_enabled_p ())
    dump_printf (MSG_NOTE, "using an address-based WAR/WAW test\n");
  return true;
}

/* If ALIGN is nonzero, set up *SEQ_MIN_OUT and *SEQ_MAX_OUT so that for
   every address ADDR accessed by D:

     *SEQ_MIN_OUT <= ADDR (== ADDR & -ALIGN) <= *SEQ_MAX_OUT

   In this case, every element accessed by D is aligned to at least
   ALIGN bytes.

   If ALIGN is zero then instead set *SEG_MAX_OUT so that:

     *SEQ_MIN_OUT <= ADDR < *SEQ_MAX_OUT.  */

static void
get_segment_min_max (const dr_with_seg_len &d, tree *seg_min_out,
		     tree *seg_max_out, HOST_WIDE_INT align)
{
  /* Each access has the following pattern:

	  <- |seg_len| ->
	  <--- A: -ve step --->
	  +-----+-------+-----+-------+-----+
	  | n-1 | ,.... |  0  | ..... | n-1 |
	  +-----+-------+-----+-------+-----+
			<--- B: +ve step --->
			<- |seg_len| ->
			|
		   base address

     where "n" is the number of scalar iterations covered by the segment.
     (This should be VF for a particular pair if we know that both steps
     are the same, otherwise it will be the full number of scalar loop
     iterations.)

     A is the range of bytes accessed when the step is negative,
     B is the range when the step is positive.

     If the access size is "access_size" bytes, the lowest addressed byte is:

	 base + (step < 0 ? seg_len : 0)   [LB]

     and the highest addressed byte is always below:

	 base + (step < 0 ? 0 : seg_len) + access_size   [UB]

     Thus:

	 LB <= ADDR < UB

     If ALIGN is nonzero, all three values are aligned to at least ALIGN
     bytes, so:

	 LB <= ADDR <= UB - ALIGN

     where "- ALIGN" folds naturally with the "+ access_size" and often
     cancels it out.

     We don't try to simplify LB and UB beyond this (e.g. by using
     MIN and MAX based on whether seg_len rather than the stride is
     negative) because it is possible for the absolute size of the
     segment to overflow the range of a ssize_t.

     Keeping the pointer_plus outside of the cond_expr should allow
     the cond_exprs to be shared with other alias checks.  */
  tree indicator = dr_direction_indicator (d.dr);
  tree neg_step = fold_build2 (LT_EXPR, boolean_type_node,
			       fold_convert (ssizetype, indicator),
			       ssize_int (0));
  tree addr_base = fold_build_pointer_plus (DR_BASE_ADDRESS (d.dr),
					    DR_OFFSET (d.dr));
  addr_base = fold_build_pointer_plus (addr_base, DR_INIT (d.dr));
  tree seg_len
    = fold_convert (sizetype, rewrite_to_non_trapping_overflow (d.seg_len));

  tree min_reach = fold_build3 (COND_EXPR, sizetype, neg_step,
				seg_len, size_zero_node);
  tree max_reach = fold_build3 (COND_EXPR, sizetype, neg_step,
				size_zero_node, seg_len);
  max_reach = fold_build2 (PLUS_EXPR, sizetype, max_reach,
			   size_int (d.access_size - align));

  *seg_min_out = fold_build_pointer_plus (addr_base, min_reach);
  *seg_max_out = fold_build_pointer_plus (addr_base, max_reach);
}

/* Generate a runtime condition that is true if ALIAS_PAIR is free of aliases,
   storing the condition in *COND_EXPR.  The fallback is to generate a
   a test that the two accesses do not overlap:

     end_a <= start_b || end_b <= start_a.  */

static void
create_intersect_range_checks (class loop *loop, tree *cond_expr,
			       const dr_with_seg_len_pair_t &alias_pair)
{
  const dr_with_seg_len& dr_a = alias_pair.first;
  const dr_with_seg_len& dr_b = alias_pair.second;
  *cond_expr = NULL_TREE;
  if (create_intersect_range_checks_index (loop, cond_expr, alias_pair))
    return;

  if (create_ifn_alias_checks (cond_expr, alias_pair))
    return;

  if (create_waw_or_war_checks (cond_expr, alias_pair))
    return;

  unsigned HOST_WIDE_INT min_align;
  tree_code cmp_code;
  /* We don't have to check DR_ALIAS_MIXED_STEPS here, since both versions
     are equivalent.  This is just an optimization heuristic.  */
  if (TREE_CODE (DR_STEP (dr_a.dr)) == INTEGER_CST
      && TREE_CODE (DR_STEP (dr_b.dr)) == INTEGER_CST)
    {
      /* In this case adding access_size to seg_len is likely to give
	 a simple X * step, where X is either the number of scalar
	 iterations or the vectorization factor.  We're better off
	 keeping that, rather than subtracting an alignment from it.

	 In this case the maximum values are exclusive and so there is
	 no alias if the maximum of one segment equals the minimum
	 of another.  */
      min_align = 0;
      cmp_code = LE_EXPR;
    }
  else
    {
      /* Calculate the minimum alignment shared by all four pointers,
	 then arrange for this alignment to be subtracted from the
	 exclusive maximum values to get inclusive maximum values.
	 This "- min_align" is cumulative with a "+ access_size"
	 in the calculation of the maximum values.  In the best
	 (and common) case, the two cancel each other out, leaving
	 us with an inclusive bound based only on seg_len.  In the
	 worst case we're simply adding a smaller number than before.

	 Because the maximum values are inclusive, there is an alias
	 if the maximum value of one segment is equal to the minimum
	 value of the other.  */
      min_align = MIN (dr_a.align, dr_b.align);
      cmp_code = LT_EXPR;
    }

  tree seg_a_min, seg_a_max, seg_b_min, seg_b_max;
  get_segment_min_max (dr_a, &seg_a_min, &seg_a_max, min_align);
  get_segment_min_max (dr_b, &seg_b_min, &seg_b_max, min_align);

  *cond_expr
    = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
	fold_build2 (cmp_code, boolean_type_node, seg_a_max, seg_b_min),
	fold_build2 (cmp_code, boolean_type_node, seg_b_max, seg_a_min));
  if (dump_enabled_p ())
    dump_printf (MSG_NOTE, "using an address-based overlap test\n");
}

/* Create a conditional expression that represents the run-time checks for
   overlapping of address ranges represented by a list of data references
   pairs passed in ALIAS_PAIRS.  Data references are in LOOP.  The returned
   COND_EXPR is the conditional expression to be used in the if statement
   that controls which version of the loop gets executed at runtime.  */

void
create_runtime_alias_checks (class loop *loop,
			     const vec<dr_with_seg_len_pair_t> *alias_pairs,
			     tree * cond_expr)
{
  tree part_cond_expr;

  fold_defer_overflow_warnings ();
  for (const dr_with_seg_len_pair_t &alias_pair : alias_pairs)
    {
      gcc_assert (alias_pair.flags);
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE,
		     "create runtime check for data references %T and %T\n",
		     DR_REF (alias_pair.first.dr),
		     DR_REF (alias_pair.second.dr));

      /* Create condition expression for each pair data references.  */
      create_intersect_range_checks (loop, &part_cond_expr, alias_pair);
      if (*cond_expr)
	*cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				  *cond_expr, part_cond_expr);
      else
	*cond_expr = part_cond_expr;
    }
  fold_undefer_and_ignore_overflow_warnings ();
}

/* Check if OFFSET1 and OFFSET2 (DR_OFFSETs of some data-refs) are identical
   expressions.  */
static bool
dr_equal_offsets_p1 (tree offset1, tree offset2)
{
  bool res;

  STRIP_NOPS (offset1);
  STRIP_NOPS (offset2);

  if (offset1 == offset2)
    return true;

  if (TREE_CODE (offset1) != TREE_CODE (offset2)
      || (!BINARY_CLASS_P (offset1) && !UNARY_CLASS_P (offset1)))
    return false;

  res = dr_equal_offsets_p1 (TREE_OPERAND (offset1, 0),
                             TREE_OPERAND (offset2, 0));

  if (!res || !BINARY_CLASS_P (offset1))
    return res;

  res = dr_equal_offsets_p1 (TREE_OPERAND (offset1, 1),
                             TREE_OPERAND (offset2, 1));

  return res;
}

/* Check if DRA and DRB have equal offsets.  */
bool
dr_equal_offsets_p (struct data_reference *dra,
                    struct data_reference *drb)
{
  tree offset1, offset2;

  offset1 = DR_OFFSET (dra);
  offset2 = DR_OFFSET (drb);

  return dr_equal_offsets_p1 (offset1, offset2);
}

/* Returns true if FNA == FNB.  */

static bool
affine_function_equal_p (affine_fn fna, affine_fn fnb)
{
  unsigned i, n = fna.length ();

  if (n != fnb.length ())
    return false;

  for (i = 0; i < n; i++)
    if (!operand_equal_p (fna[i], fnb[i], 0))
      return false;

  return true;
}

/* If all the functions in CF are the same, returns one of them,
   otherwise returns NULL.  */

static affine_fn
common_affine_function (conflict_function *cf)
{
  unsigned i;
  affine_fn comm;

  if (!CF_NONTRIVIAL_P (cf))
    return affine_fn ();

  comm = cf->fns[0];

  for (i = 1; i < cf->n; i++)
    if (!affine_function_equal_p (comm, cf->fns[i]))
      return affine_fn ();

  return comm;
}

/* Returns the base of the affine function FN.  */

static tree
affine_function_base (affine_fn fn)
{
  return fn[0];
}

/* Returns true if FN is a constant.  */

static bool
affine_function_constant_p (affine_fn fn)
{
  unsigned i;
  tree coef;

  for (i = 1; fn.iterate (i, &coef); i++)
    if (!integer_zerop (coef))
      return false;

  return true;
}

/* Returns true if FN is the zero constant function.  */

static bool
affine_function_zero_p (affine_fn fn)
{
  return (integer_zerop (affine_function_base (fn))
	  && affine_function_constant_p (fn));
}

/* Returns a signed integer type with the largest precision from TA
   and TB.  */

static tree
signed_type_for_types (tree ta, tree tb)
{
  if (TYPE_PRECISION (ta) > TYPE_PRECISION (tb))
    return signed_type_for (ta);
  else
    return signed_type_for (tb);
}

/* Applies operation OP on affine functions FNA and FNB, and returns the
   result.  */

static affine_fn
affine_fn_op (enum tree_code op, affine_fn fna, affine_fn fnb)
{
  unsigned i, n, m;
  affine_fn ret;
  tree coef;

  if (fnb.length () > fna.length ())
    {
      n = fna.length ();
      m = fnb.length ();
    }
  else
    {
      n = fnb.length ();
      m = fna.length ();
    }

  ret.create (m);
  for (i = 0; i < n; i++)
    {
      tree type = signed_type_for_types (TREE_TYPE (fna[i]),
					 TREE_TYPE (fnb[i]));
      ret.quick_push (fold_build2 (op, type, fna[i], fnb[i]));
    }

  for (; fna.iterate (i, &coef); i++)
    ret.quick_push (fold_build2 (op, signed_type_for (TREE_TYPE (coef)),
				 coef, integer_zero_node));
  for (; fnb.iterate (i, &coef); i++)
    ret.quick_push (fold_build2 (op, signed_type_for (TREE_TYPE (coef)),
				 integer_zero_node, coef));

  return ret;
}

/* Returns the sum of affine functions FNA and FNB.  */

static affine_fn
affine_fn_plus (affine_fn fna, affine_fn fnb)
{
  return affine_fn_op (PLUS_EXPR, fna, fnb);
}

/* Returns the difference of affine functions FNA and FNB.  */

static affine_fn
affine_fn_minus (affine_fn fna, affine_fn fnb)
{
  return affine_fn_op (MINUS_EXPR, fna, fnb);
}

/* Frees affine function FN.  */

static void
affine_fn_free (affine_fn fn)
{
  fn.release ();
}

/* Determine for each subscript in the data dependence relation DDR
   the distance.  */

static void
compute_subscript_distance (struct data_dependence_relation *ddr)
{
  conflict_function *cf_a, *cf_b;
  affine_fn fn_a, fn_b, diff;

  if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
    {
      unsigned int i;

      for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
 	{
 	  struct subscript *subscript;

 	  subscript = DDR_SUBSCRIPT (ddr, i);
 	  cf_a = SUB_CONFLICTS_IN_A (subscript);
 	  cf_b = SUB_CONFLICTS_IN_B (subscript);

	  fn_a = common_affine_function (cf_a);
	  fn_b = common_affine_function (cf_b);
	  if (!fn_a.exists () || !fn_b.exists ())
	    {
	      SUB_DISTANCE (subscript) = chrec_dont_know;
	      return;
	    }
	  diff = affine_fn_minus (fn_a, fn_b);

 	  if (affine_function_constant_p (diff))
 	    SUB_DISTANCE (subscript) = affine_function_base (diff);
 	  else
 	    SUB_DISTANCE (subscript) = chrec_dont_know;

	  affine_fn_free (diff);
 	}
    }
}

/* Returns the conflict function for "unknown".  */

static conflict_function *
conflict_fn_not_known (void)
{
  conflict_function *fn = XCNEW (conflict_function);
  fn->n = NOT_KNOWN;

  return fn;
}

/* Returns the conflict function for "independent".  */

static conflict_function *
conflict_fn_no_dependence (void)
{
  conflict_function *fn = XCNEW (conflict_function);
  fn->n = NO_DEPENDENCE;

  return fn;
}

/* Returns true if the address of OBJ is invariant in LOOP.  */

static bool
object_address_invariant_in_loop_p (const class loop *loop, const_tree obj)
{
  while (handled_component_p (obj))
    {
      if (TREE_CODE (obj) == ARRAY_REF)
	{
	  for (int i = 1; i < 4; ++i)
	    if (chrec_contains_symbols_defined_in_loop (TREE_OPERAND (obj, i),
							loop->num))
	      return false;
	}
      else if (TREE_CODE (obj) == COMPONENT_REF)
	{
	  if (chrec_contains_symbols_defined_in_loop (TREE_OPERAND (obj, 2),
						      loop->num))
	    return false;
	}
      obj = TREE_OPERAND (obj, 0);
    }

  if (!INDIRECT_REF_P (obj)
      && TREE_CODE (obj) != MEM_REF)
    return true;

  return !chrec_contains_symbols_defined_in_loop (TREE_OPERAND (obj, 0),
						  loop->num);
}

/* Returns false if we can prove that data references A and B do not alias,
   true otherwise.  If LOOP_NEST is false no cross-iteration aliases are
   considered.  */

bool
dr_may_alias_p (const struct data_reference *a, const struct data_reference *b,
		class loop *loop_nest)
{
  tree addr_a = DR_BASE_OBJECT (a);
  tree addr_b = DR_BASE_OBJECT (b);

  /* If we are not processing a loop nest but scalar code we
     do not need to care about possible cross-iteration dependences
     and thus can process the full original reference.  Do so,
     similar to how loop invariant motion applies extra offset-based
     disambiguation.  */
  if (!loop_nest)
    {
      tree tree_size_a = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (a)));
      tree tree_size_b = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (b)));

      if (DR_BASE_ADDRESS (a)
	  && DR_BASE_ADDRESS (b)
	  && operand_equal_p (DR_BASE_ADDRESS (a), DR_BASE_ADDRESS (b))
	  && operand_equal_p (DR_OFFSET (a), DR_OFFSET (b))
	  && poly_int_tree_p (tree_size_a)
	  && poly_int_tree_p (tree_size_b)
	  && !ranges_maybe_overlap_p (wi::to_poly_widest (DR_INIT (a)),
				      wi::to_poly_widest (tree_size_a),
				      wi::to_poly_widest (DR_INIT (b)),
				      wi::to_poly_widest (tree_size_b)))
	{
	  gcc_assert (integer_zerop (DR_STEP (a))
		      && integer_zerop (DR_STEP (b)));
	  return false;
	}

      aff_tree off1, off2;
      poly_widest_int size1, size2;
      get_inner_reference_aff (DR_REF (a), &off1, &size1);
      get_inner_reference_aff (DR_REF (b), &off2, &size2);
      aff_combination_scale (&off1, -1);
      aff_combination_add (&off2, &off1);
      if (aff_comb_cannot_overlap_p (&off2, size1, size2))
	return false;
    }

  if ((TREE_CODE (addr_a) == MEM_REF || TREE_CODE (addr_a) == TARGET_MEM_REF)
      && (TREE_CODE (addr_b) == MEM_REF || TREE_CODE (addr_b) == TARGET_MEM_REF)
      /* For cross-iteration dependences the cliques must be valid for the
	 whole loop, not just individual iterations.  */
      && (!loop_nest
	  || MR_DEPENDENCE_CLIQUE (addr_a) == 1
	  || MR_DEPENDENCE_CLIQUE (addr_a) == loop_nest->owned_clique)
      && MR_DEPENDENCE_CLIQUE (addr_a) == MR_DEPENDENCE_CLIQUE (addr_b)
      && MR_DEPENDENCE_BASE (addr_a) != MR_DEPENDENCE_BASE (addr_b))
    return false;

  /* If we had an evolution in a pointer-based MEM_REF BASE_OBJECT we
     do not know the size of the base-object.  So we cannot do any
     offset/overlap based analysis but have to rely on points-to
     information only.  */
  if (TREE_CODE (addr_a) == MEM_REF
      && (DR_UNCONSTRAINED_BASE (a)
	  || TREE_CODE (TREE_OPERAND (addr_a, 0)) == SSA_NAME))
    {
      /* For true dependences we can apply TBAA.  */
      if (flag_strict_aliasing
	  && DR_IS_WRITE (a) && DR_IS_READ (b)
	  && !alias_sets_conflict_p (get_alias_set (DR_REF (a)),
				     get_alias_set (DR_REF (b))))
	return false;
      if (TREE_CODE (addr_b) == MEM_REF)
	return ptr_derefs_may_alias_p (TREE_OPERAND (addr_a, 0),
				       TREE_OPERAND (addr_b, 0));
      else
	return ptr_derefs_may_alias_p (TREE_OPERAND (addr_a, 0),
				       build_fold_addr_expr (addr_b));
    }
  else if (TREE_CODE (addr_b) == MEM_REF
	   && (DR_UNCONSTRAINED_BASE (b)
	       || TREE_CODE (TREE_OPERAND (addr_b, 0)) == SSA_NAME))
    {
      /* For true dependences we can apply TBAA.  */
      if (flag_strict_aliasing
	  && DR_IS_WRITE (a) && DR_IS_READ (b)
	  && !alias_sets_conflict_p (get_alias_set (DR_REF (a)),
				     get_alias_set (DR_REF (b))))
	return false;
      if (TREE_CODE (addr_a) == MEM_REF)
	return ptr_derefs_may_alias_p (TREE_OPERAND (addr_a, 0),
				       TREE_OPERAND (addr_b, 0));
      else
	return ptr_derefs_may_alias_p (build_fold_addr_expr (addr_a),
				       TREE_OPERAND (addr_b, 0));
    }

  /* Otherwise DR_BASE_OBJECT is an access that covers the whole object
     that is being subsetted in the loop nest.  */
  if (DR_IS_WRITE (a) && DR_IS_WRITE (b))
    return refs_output_dependent_p (addr_a, addr_b);
  else if (DR_IS_READ (a) && DR_IS_WRITE (b))
    return refs_anti_dependent_p (addr_a, addr_b);
  return refs_may_alias_p (addr_a, addr_b);
}

/* REF_A and REF_B both satisfy access_fn_component_p.  Return true
   if it is meaningful to compare their associated access functions
   when checking for dependencies.  */

static bool
access_fn_components_comparable_p (tree ref_a, tree ref_b)
{
  /* Allow pairs of component refs from the following sets:

       { REALPART_EXPR, IMAGPART_EXPR }
       { COMPONENT_REF }
       { ARRAY_REF }.  */
  tree_code code_a = TREE_CODE (ref_a);
  tree_code code_b = TREE_CODE (ref_b);
  if (code_a == IMAGPART_EXPR)
    code_a = REALPART_EXPR;
  if (code_b == IMAGPART_EXPR)
    code_b = REALPART_EXPR;
  if (code_a != code_b)
    return false;

  if (TREE_CODE (ref_a) == COMPONENT_REF)
    /* ??? We cannot simply use the type of operand #0 of the refs here as
       the Fortran compiler smuggles type punning into COMPONENT_REFs.
       Use the DECL_CONTEXT of the FIELD_DECLs instead.  */
    return (DECL_CONTEXT (TREE_OPERAND (ref_a, 1))
	    == DECL_CONTEXT (TREE_OPERAND (ref_b, 1)));

  return types_compatible_p (TREE_TYPE (TREE_OPERAND (ref_a, 0)),
			     TREE_TYPE (TREE_OPERAND (ref_b, 0)));
}

/* Initialize a data dependence relation RES in LOOP_NEST.  USE_ALT_INDICES
   is true when the main indices of A and B were not comparable so we try again
   with alternate indices computed on an indirect reference.  */

struct data_dependence_relation *
initialize_data_dependence_relation (struct data_dependence_relation *res,
				     vec<loop_p> loop_nest,
				     bool use_alt_indices)
{
  struct data_reference *a = DDR_A (res);
  struct data_reference *b = DDR_B (res);
  unsigned int i;

  struct indices *indices_a = &a->indices;
  struct indices *indices_b = &b->indices;
  if (use_alt_indices)
    {
      if (TREE_CODE (DR_REF (a)) != MEM_REF)
	indices_a = &a->alt_indices;
      if (TREE_CODE (DR_REF (b)) != MEM_REF)
	indices_b = &b->alt_indices;
    }
  unsigned int num_dimensions_a = indices_a->access_fns.length ();
  unsigned int num_dimensions_b = indices_b->access_fns.length ();
  if (num_dimensions_a == 0 || num_dimensions_b == 0)
    {
      DDR_ARE_DEPENDENT (res) = chrec_dont_know;
      return res;
    }

  /* For unconstrained bases, the root (highest-indexed) subscript
     describes a variation in the base of the original DR_REF rather
     than a component access.  We have no type that accurately describes
     the new DR_BASE_OBJECT (whose TREE_TYPE describes the type *after*
     applying this subscript) so limit the search to the last real
     component access.

     E.g. for:

	void
	f (int a[][8], int b[][8])
	{
	  for (int i = 0; i < 8; ++i)
	    a[i * 2][0] = b[i][0];
	}

     the a and b accesses have a single ARRAY_REF component reference [0]
     but have two subscripts.  */
  if (indices_a->unconstrained_base)
    num_dimensions_a -= 1;
  if (indices_b->unconstrained_base)
    num_dimensions_b -= 1;

  /* These structures describe sequences of component references in
     DR_REF (A) and DR_REF (B).  Each component reference is tied to a
     specific access function.  */
  struct {
    /* The sequence starts at DR_ACCESS_FN (A, START_A) of A and
       DR_ACCESS_FN (B, START_B) of B (inclusive) and extends to higher
       indices.  In C notation, these are the indices of the rightmost
       component references; e.g. for a sequence .b.c.d, the start
       index is for .d.  */
    unsigned int start_a;
    unsigned int start_b;

    /* The sequence contains LENGTH consecutive access functions from
       each DR.  */
    unsigned int length;

    /* The enclosing objects for the A and B sequences respectively,
       i.e. the objects to which DR_ACCESS_FN (A, START_A + LENGTH - 1)
       and DR_ACCESS_FN (B, START_B + LENGTH - 1) are applied.  */
    tree object_a;
    tree object_b;
  } full_seq = {}, struct_seq = {};

  /* Before each iteration of the loop:

     - REF_A is what you get after applying DR_ACCESS_FN (A, INDEX_A) and
     - REF_B is what you get after applying DR_ACCESS_FN (B, INDEX_B).  */
  unsigned int index_a = 0;
  unsigned int index_b = 0;
  tree ref_a = DR_REF (a);
  tree ref_b = DR_REF (b);

  /* Now walk the component references from the final DR_REFs back up to
     the enclosing base objects.  Each component reference corresponds
     to one access function in the DR, with access function 0 being for
     the final DR_REF and the highest-indexed access function being the
     one that is applied to the base of the DR.

     Look for a sequence of component references whose access functions
     are comparable (see access_fn_components_comparable_p).  If more
     than one such sequence exists, pick the one nearest the base
     (which is the leftmost sequence in C notation).  Store this sequence
     in FULL_SEQ.

     For example, if we have:

	struct foo { struct bar s; ... } (*a)[10], (*b)[10];

	A: a[0][i].s.c.d
	B: __real b[0][i].s.e[i].f

     (where d is the same type as the real component of f) then the access
     functions would be:

			 0   1   2   3
	A:              .d  .c  .s [i]

		 0   1   2   3   4   5
	B:  __real  .f [i]  .e  .s [i]

     The A0/B2 column isn't comparable, since .d is a COMPONENT_REF
     and [i] is an ARRAY_REF.  However, the A1/B3 column contains two
     COMPONENT_REF accesses for struct bar, so is comparable.  Likewise
     the A2/B4 column contains two COMPONENT_REF accesses for struct foo,
     so is comparable.  The A3/B5 column contains two ARRAY_REFs that
     index foo[10] arrays, so is again comparable.  The sequence is
     therefore:

        A: [1, 3]  (i.e. [i].s.c)
        B: [3, 5]  (i.e. [i].s.e)

     Also look for sequences of component references whose access
     functions are comparable and whose enclosing objects have the same
     RECORD_TYPE.  Store this sequence in STRUCT_SEQ.  In the above
     example, STRUCT_SEQ would be:

        A: [1, 2]  (i.e. s.c)
        B: [3, 4]  (i.e. s.e)  */
  while (index_a < num_dimensions_a && index_b < num_dimensions_b)
    {
      /* The alternate indices form always has a single dimension
	 with unconstrained base.  */
      gcc_assert (!use_alt_indices);

      /* REF_A and REF_B must be one of the component access types
	 allowed by dr_analyze_indices.  */
      gcc_checking_assert (access_fn_component_p (ref_a));
      gcc_checking_assert (access_fn_component_p (ref_b));

      /* Get the immediately-enclosing objects for REF_A and REF_B,
	 i.e. the references *before* applying DR_ACCESS_FN (A, INDEX_A)
	 and DR_ACCESS_FN (B, INDEX_B).  */
      tree object_a = TREE_OPERAND (ref_a, 0);
      tree object_b = TREE_OPERAND (ref_b, 0);

      tree type_a = TREE_TYPE (object_a);
      tree type_b = TREE_TYPE (object_b);
      if (access_fn_components_comparable_p (ref_a, ref_b))
	{
	  /* This pair of component accesses is comparable for dependence
	     analysis, so we can include DR_ACCESS_FN (A, INDEX_A) and
	     DR_ACCESS_FN (B, INDEX_B) in the sequence.  */
	  if (full_seq.start_a + full_seq.length != index_a
	      || full_seq.start_b + full_seq.length != index_b)
	    {
	      /* The accesses don't extend the current sequence,
		 so start a new one here.  */
	      full_seq.start_a = index_a;
	      full_seq.start_b = index_b;
	      full_seq.length = 0;
	    }

	  /* Add this pair of references to the sequence.  */
	  full_seq.length += 1;
	  full_seq.object_a = object_a;
	  full_seq.object_b = object_b;

	  /* If the enclosing objects are structures (and thus have the
	     same RECORD_TYPE), record the new sequence in STRUCT_SEQ.  */
	  if (TREE_CODE (type_a) == RECORD_TYPE)
	    struct_seq = full_seq;

	  /* Move to the next containing reference for both A and B.  */
	  ref_a = object_a;
	  ref_b = object_b;
	  index_a += 1;
	  index_b += 1;
	  continue;
	}

      /* Try to approach equal type sizes.  */
      if (!COMPLETE_TYPE_P (type_a)
	  || !COMPLETE_TYPE_P (type_b)
	  || !tree_fits_uhwi_p (TYPE_SIZE_UNIT (type_a))
	  || !tree_fits_uhwi_p (TYPE_SIZE_UNIT (type_b)))
	break;

      unsigned HOST_WIDE_INT size_a = tree_to_uhwi (TYPE_SIZE_UNIT (type_a));
      unsigned HOST_WIDE_INT size_b = tree_to_uhwi (TYPE_SIZE_UNIT (type_b));
      if (size_a <= size_b)
	{
	  index_a += 1;
	  ref_a = object_a;
	}
      if (size_b <= size_a)
	{
	  index_b += 1;
	  ref_b = object_b;
	}
    }

  /* See whether FULL_SEQ ends at the base and whether the two bases
     are equal.  We do not care about TBAA or alignment info so we can
     use OEP_ADDRESS_OF to avoid false negatives.  */
  tree base_a = indices_a->base_object;
  tree base_b = indices_b->base_object;
  bool same_base_p = (full_seq.start_a + full_seq.length == num_dimensions_a
		      && full_seq.start_b + full_seq.length == num_dimensions_b
		      && (indices_a->unconstrained_base
			  == indices_b->unconstrained_base)
		      && operand_equal_p (base_a, base_b, OEP_ADDRESS_OF)
		      && (types_compatible_p (TREE_TYPE (base_a),
					      TREE_TYPE (base_b))
			  || (!base_supports_access_fn_components_p (base_a)
			      && !base_supports_access_fn_components_p (base_b)
			      && operand_equal_p
				   (TYPE_SIZE (TREE_TYPE (base_a)),
				    TYPE_SIZE (TREE_TYPE (base_b)), 0)))
		      && (!loop_nest.exists ()
			  || (object_address_invariant_in_loop_p
			      (loop_nest[0], base_a))));

  /* If the bases are the same, we can include the base variation too.
     E.g. the b accesses in:

       for (int i = 0; i < n; ++i)
         b[i + 4][0] = b[i][0];

     have a definite dependence distance of 4, while for:

       for (int i = 0; i < n; ++i)
         a[i + 4][0] = b[i][0];

     the dependence distance depends on the gap between a and b.

     If the bases are different then we can only rely on the sequence
     rooted at a structure access, since arrays are allowed to overlap
     arbitrarily and change shape arbitrarily.  E.g. we treat this as
     valid code:

       int a[256];
       ...
       ((int (*)[4][3]) &a[1])[i][0] += ((int (*)[4][3]) &a[2])[i][0];

     where two lvalues with the same int[4][3] type overlap, and where
     both lvalues are distinct from the object's declared type.  */
  if (same_base_p)
    {
      if (indices_a->unconstrained_base)
	full_seq.length += 1;
    }
  else
    full_seq = struct_seq;

  /* Punt if we didn't find a suitable sequence.  */
  if (full_seq.length == 0)
    {
      if (use_alt_indices
	  || (TREE_CODE (DR_REF (a)) == MEM_REF
	      && TREE_CODE (DR_REF (b)) == MEM_REF)
	  || may_be_nonaddressable_p (DR_REF (a))
	  || may_be_nonaddressable_p (DR_REF (b)))
	{
	  /* Fully exhausted possibilities.  */
	  DDR_ARE_DEPENDENT (res) = chrec_dont_know;
	  return res;
	}

      /* Try evaluating both DRs as dereferences of pointers.  */
      if (!a->alt_indices.base_object
	  && TREE_CODE (DR_REF (a)) != MEM_REF)
	{
	  tree alt_ref = build2 (MEM_REF, TREE_TYPE (DR_REF (a)),
				 build1 (ADDR_EXPR, ptr_type_node, DR_REF (a)),
				 build_int_cst
				   (reference_alias_ptr_type (DR_REF (a)), 0));
	  dr_analyze_indices (&a->alt_indices, alt_ref,
			      loop_preheader_edge (loop_nest[0]),
			      loop_containing_stmt (DR_STMT (a)));
	}
      if (!b->alt_indices.base_object
	  && TREE_CODE (DR_REF (b)) != MEM_REF)
	{
	  tree alt_ref = build2 (MEM_REF, TREE_TYPE (DR_REF (b)),
				 build1 (ADDR_EXPR, ptr_type_node, DR_REF (b)),
				 build_int_cst
				   (reference_alias_ptr_type (DR_REF (b)), 0));
	  dr_analyze_indices (&b->alt_indices, alt_ref,
			      loop_preheader_edge (loop_nest[0]),
			      loop_containing_stmt (DR_STMT (b)));
	}
      return initialize_data_dependence_relation (res, loop_nest, true);
    }

  if (!same_base_p)
    {
      /* Partial overlap is possible for different bases when strict aliasing
	 is not in effect.  It's also possible if either base involves a union
	 access; e.g. for:

	   struct s1 { int a[2]; };
	   struct s2 { struct s1 b; int c; };
	   struct s3 { int d; struct s1 e; };
	   union u { struct s2 f; struct s3 g; } *p, *q;

	 the s1 at "p->f.b" (base "p->f") partially overlaps the s1 at
	 "p->g.e" (base "p->g") and might partially overlap the s1 at
	 "q->g.e" (base "q->g").  */
      if (!flag_strict_aliasing
	  || ref_contains_union_access_p (full_seq.object_a)
	  || ref_contains_union_access_p (full_seq.object_b))
	{
	  DDR_ARE_DEPENDENT (res) = chrec_dont_know;
	  return res;
	}

      DDR_COULD_BE_INDEPENDENT_P (res) = true;
      if (!loop_nest.exists ()
	  || (object_address_invariant_in_loop_p (loop_nest[0],
						  full_seq.object_a)
	      && object_address_invariant_in_loop_p (loop_nest[0],
						     full_seq.object_b)))
	{
	  DDR_OBJECT_A (res) = full_seq.object_a;
	  DDR_OBJECT_B (res) = full_seq.object_b;
	}
    }

  DDR_AFFINE_P (res) = true;
  DDR_ARE_DEPENDENT (res) = NULL_TREE;
  DDR_SUBSCRIPTS (res).create (full_seq.length);
  DDR_LOOP_NEST (res) = loop_nest;
  DDR_SELF_REFERENCE (res) = false;

  for (i = 0; i < full_seq.length; ++i)
    {
      struct subscript *subscript;

      subscript = XNEW (struct subscript);
      SUB_ACCESS_FN (subscript, 0) = indices_a->access_fns[full_seq.start_a + i];
      SUB_ACCESS_FN (subscript, 1) = indices_b->access_fns[full_seq.start_b + i];
      SUB_CONFLICTS_IN_A (subscript) = conflict_fn_not_known ();
      SUB_CONFLICTS_IN_B (subscript) = conflict_fn_not_known ();
      SUB_LAST_CONFLICT (subscript) = chrec_dont_know;
      SUB_DISTANCE (subscript) = chrec_dont_know;
      DDR_SUBSCRIPTS (res).safe_push (subscript);
    }

  return res;
}

/* Initialize a data dependence relation between data accesses A and
   B.  NB_LOOPS is the number of loops surrounding the references: the
   size of the classic distance/direction vectors.  */

struct data_dependence_relation *
initialize_data_dependence_relation (struct data_reference *a,
				     struct data_reference *b,
				     vec<loop_p> loop_nest)
{
  data_dependence_relation *res = XCNEW (struct data_dependence_relation);
  DDR_A (res) = a;
  DDR_B (res) = b;
  DDR_LOOP_NEST (res).create (0);
  DDR_SUBSCRIPTS (res).create (0);
  DDR_DIR_VECTS (res).create (0);
  DDR_DIST_VECTS (res).create (0);

  if (a == NULL || b == NULL)
    {
      DDR_ARE_DEPENDENT (res) = chrec_dont_know;
      return res;
    }

  /* If the data references do not alias, then they are independent.  */
  if (!dr_may_alias_p (a, b, loop_nest.exists () ? loop_nest[0] : NULL))
    {
      DDR_ARE_DEPENDENT (res) = chrec_known;
      return res;
    }

  return initialize_data_dependence_relation (res, loop_nest, false);
}


/* Frees memory used by the conflict function F.  */

static void
free_conflict_function (conflict_function *f)
{
  unsigned i;

  if (CF_NONTRIVIAL_P (f))
    {
      for (i = 0; i < f->n; i++)
	affine_fn_free (f->fns[i]);
    }
  free (f);
}

/* Frees memory used by SUBSCRIPTS.  */

static void
free_subscripts (vec<subscript_p> subscripts)
{
  for (subscript_p s : subscripts)
    {
      free_conflict_function (s->conflicting_iterations_in_a);
      free_conflict_function (s->conflicting_iterations_in_b);
      free (s);
    }
  subscripts.release ();
}

/* Set DDR_ARE_DEPENDENT to CHREC and finalize the subscript overlap
   description.  */

static inline void
finalize_ddr_dependent (struct data_dependence_relation *ddr,
			tree chrec)
{
  DDR_ARE_DEPENDENT (ddr) = chrec;
  free_subscripts (DDR_SUBSCRIPTS (ddr));
  DDR_SUBSCRIPTS (ddr).create (0);
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
ziv_subscript_p (const_tree chrec_a, const_tree chrec_b)
{
  return (evolution_function_is_constant_p (chrec_a)
	  && evolution_function_is_constant_p (chrec_b));
}

/* Returns true iff CHREC_A and CHREC_B are dependent on an index
   variable, i.e., if the SIV (Single Index Variable) test is true.  */

static bool
siv_subscript_p (const_tree chrec_a, const_tree chrec_b)
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
	      /* FALLTHRU */

	    default:
	      return true;
	    }

	default:
	  return true;
	}
    }

  return false;
}

/* Creates a conflict function with N dimensions.  The affine functions
   in each dimension follow.  */

static conflict_function *
conflict_fn (unsigned n, ...)
{
  unsigned i;
  conflict_function *ret = XCNEW (conflict_function);
  va_list ap;

  gcc_assert (n > 0 && n <= MAX_DIM);
  va_start (ap, n);

  ret->n = n;
  for (i = 0; i < n; i++)
    ret->fns[i] = va_arg (ap, affine_fn);
  va_end (ap);

  return ret;
}

/* Returns constant affine function with value CST.  */

static affine_fn
affine_fn_cst (tree cst)
{
  affine_fn fn;
  fn.create (1);
  fn.quick_push (cst);
  return fn;
}

/* Returns affine function with single variable, CST + COEF * x_DIM.  */

static affine_fn
affine_fn_univar (tree cst, unsigned dim, tree coef)
{
  affine_fn fn;
  fn.create (dim + 1);
  unsigned i;

  gcc_assert (dim > 0);
  fn.quick_push (cst);
  for (i = 1; i < dim; i++)
    fn.quick_push (integer_zero_node);
  fn.quick_push (coef);
  return fn;
}

/* Analyze a ZIV (Zero Index Variable) subscript.  *OVERLAPS_A and
   *OVERLAPS_B are initialized to the functions that describe the
   relation between the elements accessed twice by CHREC_A and
   CHREC_B.  For k >= 0, the following property is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void
analyze_ziv_subscript (tree chrec_a,
		       tree chrec_b,
		       conflict_function **overlaps_a,
		       conflict_function **overlaps_b,
		       tree *last_conflicts)
{
  tree type, difference;
  dependence_stats.num_ziv++;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_ziv_subscript \n");

  type = signed_type_for_types (TREE_TYPE (chrec_a), TREE_TYPE (chrec_b));
  chrec_a = chrec_convert (type, chrec_a, NULL);
  chrec_b = chrec_convert (type, chrec_b, NULL);
  difference = chrec_fold_minus (type, chrec_a, chrec_b);

  switch (TREE_CODE (difference))
    {
    case INTEGER_CST:
      if (integer_zerop (difference))
	{
	  /* The difference is equal to zero: the accessed index
	     overlaps for each iteration in the loop.  */
	  *overlaps_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
	  *overlaps_b = conflict_fn (1, affine_fn_cst (integer_zero_node));
	  *last_conflicts = chrec_dont_know;
	  dependence_stats.num_ziv_dependent++;
	}
      else
	{
	  /* The accesses do not overlap.  */
	  *overlaps_a = conflict_fn_no_dependence ();
	  *overlaps_b = conflict_fn_no_dependence ();
	  *last_conflicts = integer_zero_node;
	  dependence_stats.num_ziv_independent++;
	}
      break;

    default:
      /* We're not sure whether the indexes overlap.  For the moment,
	 conservatively answer "don't know".  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "ziv test failed: difference is non-integer.\n");

      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      dependence_stats.num_ziv_unimplemented++;
      break;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Similar to max_stmt_executions_int, but returns the bound as a tree,
   and only if it fits to the int type.  If this is not the case, or the
   bound  on the number of iterations of LOOP could not be derived, returns
   chrec_dont_know.  */

static tree
max_stmt_executions_tree (class loop *loop)
{
  widest_int nit;

  if (!max_stmt_executions (loop, &nit))
    return chrec_dont_know;

  if (!wi::fits_to_tree_p (nit, unsigned_type_node))
    return chrec_dont_know;

  return wide_int_to_tree (unsigned_type_node, nit);
}

/* Determine whether the CHREC is always positive/negative.  If the expression
   cannot be statically analyzed, return false, otherwise set the answer into
   VALUE.  */

static bool
chrec_is_positive (tree chrec, bool *value)
{
  bool value0, value1, value2;
  tree end_value, nb_iter;

  switch (TREE_CODE (chrec))
    {
    case POLYNOMIAL_CHREC:
      if (!chrec_is_positive (CHREC_LEFT (chrec), &value0)
	  || !chrec_is_positive (CHREC_RIGHT (chrec), &value1))
	return false;

      /* FIXME -- overflows.  */
      if (value0 == value1)
	{
	  *value = value0;
	  return true;
	}

      /* Otherwise the chrec is under the form: "{-197, +, 2}_1",
	 and the proof consists in showing that the sign never
	 changes during the execution of the loop, from 0 to
	 loop->nb_iterations.  */
      if (!evolution_function_is_affine_p (chrec))
	return false;

      nb_iter = number_of_latch_executions (get_chrec_loop (chrec));
      if (chrec_contains_undetermined (nb_iter))
	return false;

#if 0
      /* TODO -- If the test is after the exit, we may decrease the number of
	 iterations by one.  */
      if (after_exit)
	nb_iter = chrec_fold_minus (type, nb_iter, build_int_cst (type, 1));
#endif

      end_value = chrec_apply (CHREC_VARIABLE (chrec), chrec, nb_iter);

      if (!chrec_is_positive (end_value, &value2))
	return false;

      *value = value0;
      return value0 == value1;

    case INTEGER_CST:
      switch (tree_int_cst_sgn (chrec))
	{
	case -1:
	  *value = false;
	  break;
	case 1:
	  *value = true;
	  break;
	default:
	  return false;
	}
      return true;

    default:
      return false;
    }
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
				  conflict_function **overlaps_a,
				  conflict_function **overlaps_b,
				  tree *last_conflicts)
{
  bool value0, value1, value2;
  tree type, difference, tmp;

  type = signed_type_for_types (TREE_TYPE (chrec_a), TREE_TYPE (chrec_b));
  chrec_a = chrec_convert (type, chrec_a, NULL);
  chrec_b = chrec_convert (type, chrec_b, NULL);
  difference = chrec_fold_minus (type, initial_condition (chrec_b), chrec_a);

  /* Special case overlap in the first iteration.  */
  if (integer_zerop (difference))
    {
      *overlaps_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *overlaps_b = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *last_conflicts = integer_one_node;
      return;
    }

  if (!chrec_is_positive (initial_condition (difference), &value0))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "siv test failed: chrec is not positive.\n");

      dependence_stats.num_siv_unimplemented++;
      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      return;
    }
  else
    {
      if (value0 == false)
	{
	  if (TREE_CODE (chrec_b) != POLYNOMIAL_CHREC
	      || !chrec_is_positive (CHREC_RIGHT (chrec_b), &value1))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "siv test failed: chrec not positive.\n");

	      *overlaps_a = conflict_fn_not_known ();
	      *overlaps_b = conflict_fn_not_known ();
	      *last_conflicts = chrec_dont_know;
	      dependence_stats.num_siv_unimplemented++;
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

		  if (tree_fold_divides_p (CHREC_RIGHT (chrec_b), difference))
		    {
		      HOST_WIDE_INT numiter;
		      class loop *loop = get_chrec_loop (chrec_b);

		      *overlaps_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
		      tmp = fold_build2 (EXACT_DIV_EXPR, type,
					 fold_build1 (ABS_EXPR, type, difference),
					 CHREC_RIGHT (chrec_b));
		      *overlaps_b = conflict_fn (1, affine_fn_cst (tmp));
		      *last_conflicts = integer_one_node;


		      /* Perform weak-zero siv test to see if overlap is
			 outside the loop bounds.  */
		      numiter = max_stmt_executions_int (loop);

		      if (numiter >= 0
			  && compare_tree_int (tmp, numiter) > 0)
			{
			  free_conflict_function (*overlaps_a);
			  free_conflict_function (*overlaps_b);
			  *overlaps_a = conflict_fn_no_dependence ();
			  *overlaps_b = conflict_fn_no_dependence ();
			  *last_conflicts = integer_zero_node;
			  dependence_stats.num_siv_independent++;
			  return;
			}
		      dependence_stats.num_siv_dependent++;
		      return;
		    }

		  /* When the step does not divide the difference, there are
		     no overlaps.  */
		  else
		    {
		      *overlaps_a = conflict_fn_no_dependence ();
		      *overlaps_b = conflict_fn_no_dependence ();
		      *last_conflicts = integer_zero_node;
		      dependence_stats.num_siv_independent++;
		      return;
		    }
		}

	      else
		{
		  /* Example:
		     chrec_a = 12
		     chrec_b = {10, +, -1}

		     In this case, chrec_a will not overlap with chrec_b.  */
		  *overlaps_a = conflict_fn_no_dependence ();
		  *overlaps_b = conflict_fn_no_dependence ();
		  *last_conflicts = integer_zero_node;
		  dependence_stats.num_siv_independent++;
		  return;
		}
	    }
	}
      else
	{
	  if (TREE_CODE (chrec_b) != POLYNOMIAL_CHREC
	      || !chrec_is_positive (CHREC_RIGHT (chrec_b), &value2))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "siv test failed: chrec not positive.\n");

	      *overlaps_a = conflict_fn_not_known ();
	      *overlaps_b = conflict_fn_not_known ();
	      *last_conflicts = chrec_dont_know;
	      dependence_stats.num_siv_unimplemented++;
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
		  if (tree_fold_divides_p (CHREC_RIGHT (chrec_b), difference))
		    {
		      HOST_WIDE_INT numiter;
		      class loop *loop = get_chrec_loop (chrec_b);

		      *overlaps_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
		      tmp = fold_build2 (EXACT_DIV_EXPR, type, difference,
					 CHREC_RIGHT (chrec_b));
		      *overlaps_b = conflict_fn (1, affine_fn_cst (tmp));
		      *last_conflicts = integer_one_node;

		      /* Perform weak-zero siv test to see if overlap is
			 outside the loop bounds.  */
		      numiter = max_stmt_executions_int (loop);

		      if (numiter >= 0
			  && compare_tree_int (tmp, numiter) > 0)
			{
			  free_conflict_function (*overlaps_a);
			  free_conflict_function (*overlaps_b);
			  *overlaps_a = conflict_fn_no_dependence ();
			  *overlaps_b = conflict_fn_no_dependence ();
			  *last_conflicts = integer_zero_node;
			  dependence_stats.num_siv_independent++;
			  return;
			}
		      dependence_stats.num_siv_dependent++;
		      return;
		    }

		  /* When the step does not divide the difference, there
		     are no overlaps.  */
		  else
		    {
		      *overlaps_a = conflict_fn_no_dependence ();
		      *overlaps_b = conflict_fn_no_dependence ();
		      *last_conflicts = integer_zero_node;
		      dependence_stats.num_siv_independent++;
		      return;
		    }
		}
	      else
		{
		  /* Example:
		     chrec_a = 3
		     chrec_b = {4, +, 1}

		     In this case, chrec_a will not overlap with chrec_b.  */
		  *overlaps_a = conflict_fn_no_dependence ();
		  *overlaps_b = conflict_fn_no_dependence ();
		  *last_conflicts = integer_zero_node;
		  dependence_stats.num_siv_independent++;
		  return;
		}
	    }
	}
    }
}

/* Helper recursive function for initializing the matrix A.  Returns
   the initial value of CHREC.  */

static tree
initialize_matrix_A (lambda_matrix A, tree chrec, unsigned index, int mult)
{
  gcc_assert (chrec);

  switch (TREE_CODE (chrec))
    {
    case POLYNOMIAL_CHREC:
      HOST_WIDE_INT chrec_right;
      if (!cst_and_fits_in_hwi (CHREC_RIGHT (chrec)))
	return chrec_dont_know;
      chrec_right = int_cst_value (CHREC_RIGHT (chrec));
      /* We want to be able to negate without overflow.  */
      if (chrec_right == HOST_WIDE_INT_MIN)
	return chrec_dont_know;
      A[index][0] = mult * chrec_right;
      return initialize_matrix_A (A, CHREC_LEFT (chrec), index + 1, mult);

    case PLUS_EXPR:
    case MULT_EXPR:
    case MINUS_EXPR:
      {
	tree op0 = initialize_matrix_A (A, TREE_OPERAND (chrec, 0), index, mult);
	tree op1 = initialize_matrix_A (A, TREE_OPERAND (chrec, 1), index, mult);

	return chrec_fold_op (TREE_CODE (chrec), chrec_type (chrec), op0, op1);
      }

    CASE_CONVERT:
      {
	tree op = initialize_matrix_A (A, TREE_OPERAND (chrec, 0), index, mult);
	return chrec_convert (chrec_type (chrec), op, NULL);
      }

    case BIT_NOT_EXPR:
      {
	/* Handle ~X as -1 - X.  */
	tree op = initialize_matrix_A (A, TREE_OPERAND (chrec, 0), index, mult);
	return chrec_fold_op (MINUS_EXPR, chrec_type (chrec),
			      build_int_cst (TREE_TYPE (chrec), -1), op);
      }

    case INTEGER_CST:
      return chrec;

    default:
      gcc_unreachable ();
      return NULL_TREE;
    }
}

#define FLOOR_DIV(x,y) ((x) / (y))

/* Solves the special case of the Diophantine equation:
   | {0, +, STEP_A}_x (OVERLAPS_A) = {0, +, STEP_B}_y (OVERLAPS_B)

   Computes the descriptions OVERLAPS_A and OVERLAPS_B.  NITER is the
   number of iterations that loops X and Y run.  The overlaps will be
   constructed as evolutions in dimension DIM.  */

static void
compute_overlap_steps_for_affine_univar (HOST_WIDE_INT niter,
					 HOST_WIDE_INT step_a,
					 HOST_WIDE_INT step_b,
					 affine_fn *overlaps_a,
					 affine_fn *overlaps_b,
					 tree *last_conflicts, int dim)
{
  if (((step_a > 0 && step_b > 0)
       || (step_a < 0 && step_b < 0)))
    {
      HOST_WIDE_INT step_overlaps_a, step_overlaps_b;
      HOST_WIDE_INT gcd_steps_a_b, last_conflict, tau2;

      gcd_steps_a_b = gcd (step_a, step_b);
      step_overlaps_a = step_b / gcd_steps_a_b;
      step_overlaps_b = step_a / gcd_steps_a_b;

      if (niter > 0)
	{
	  tau2 = FLOOR_DIV (niter, step_overlaps_a);
	  tau2 = MIN (tau2, FLOOR_DIV (niter, step_overlaps_b));
	  last_conflict = tau2;
	  *last_conflicts = build_int_cst (NULL_TREE, last_conflict);
	}
      else
	*last_conflicts = chrec_dont_know;

      *overlaps_a = affine_fn_univar (integer_zero_node, dim,
				      build_int_cst (NULL_TREE,
						     step_overlaps_a));
      *overlaps_b = affine_fn_univar (integer_zero_node, dim,
				      build_int_cst (NULL_TREE,
						     step_overlaps_b));
    }

  else
    {
      *overlaps_a = affine_fn_cst (integer_zero_node);
      *overlaps_b = affine_fn_cst (integer_zero_node);
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
				      conflict_function **overlaps_a,
				      conflict_function **overlaps_b,
				      tree *last_conflicts)
{
  bool xz_p, yz_p, xyz_p;
  HOST_WIDE_INT step_x, step_y, step_z;
  HOST_WIDE_INT niter_x, niter_y, niter_z, niter;
  affine_fn overlaps_a_xz, overlaps_b_xz;
  affine_fn overlaps_a_yz, overlaps_b_yz;
  affine_fn overlaps_a_xyz, overlaps_b_xyz;
  affine_fn ova1, ova2, ovb;
  tree last_conflicts_xz, last_conflicts_yz, last_conflicts_xyz;

  step_x = int_cst_value (CHREC_RIGHT (CHREC_LEFT (chrec_a)));
  step_y = int_cst_value (CHREC_RIGHT (chrec_a));
  step_z = int_cst_value (CHREC_RIGHT (chrec_b));

  niter_x = max_stmt_executions_int (get_chrec_loop (CHREC_LEFT (chrec_a)));
  niter_y = max_stmt_executions_int (get_chrec_loop (chrec_a));
  niter_z = max_stmt_executions_int (get_chrec_loop (chrec_b));

  if (niter_x < 0 || niter_y < 0 || niter_z < 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "overlap steps test failed: no iteration counts.\n");

      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      return;
    }

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
      ova1 = affine_fn_cst (integer_zero_node);
      ova2 = affine_fn_cst (integer_zero_node);
      ovb = affine_fn_cst (integer_zero_node);
      if (xz_p)
	{
	  affine_fn t0 = ova1;
	  affine_fn t2 = ovb;

	  ova1 = affine_fn_plus (ova1, overlaps_a_xz);
	  ovb = affine_fn_plus (ovb, overlaps_b_xz);
	  affine_fn_free (t0);
	  affine_fn_free (t2);
	  *last_conflicts = last_conflicts_xz;
	}
      if (yz_p)
	{
	  affine_fn t0 = ova2;
	  affine_fn t2 = ovb;

	  ova2 = affine_fn_plus (ova2, overlaps_a_yz);
	  ovb = affine_fn_plus (ovb, overlaps_b_yz);
	  affine_fn_free (t0);
	  affine_fn_free (t2);
	  *last_conflicts = last_conflicts_yz;
	}
      if (xyz_p)
	{
	  affine_fn t0 = ova1;
	  affine_fn t2 = ova2;
	  affine_fn t4 = ovb;

	  ova1 = affine_fn_plus (ova1, overlaps_a_xyz);
	  ova2 = affine_fn_plus (ova2, overlaps_a_xyz);
	  ovb = affine_fn_plus (ovb, overlaps_b_xyz);
	  affine_fn_free (t0);
	  affine_fn_free (t2);
	  affine_fn_free (t4);
	  *last_conflicts = last_conflicts_xyz;
	}
      *overlaps_a = conflict_fn (2, ova1, ova2);
      *overlaps_b = conflict_fn (1, ovb);
    }
  else
    {
      *overlaps_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *overlaps_b = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *last_conflicts = integer_zero_node;
    }

  affine_fn_free (overlaps_a_xz);
  affine_fn_free (overlaps_b_xz);
  affine_fn_free (overlaps_a_yz);
  affine_fn_free (overlaps_b_yz);
  affine_fn_free (overlaps_a_xyz);
  affine_fn_free (overlaps_b_xyz);
}

/* Copy the elements of vector VEC1 with length SIZE to VEC2.  */

static void
lambda_vector_copy (lambda_vector vec1, lambda_vector vec2,
		    int size)
{
  memcpy (vec2, vec1, size * sizeof (*vec1));
}

/* Copy the elements of M x N matrix MAT1 to MAT2.  */

static void
lambda_matrix_copy (lambda_matrix mat1, lambda_matrix mat2,
		    int m, int n)
{
  int i;

  for (i = 0; i < m; i++)
    lambda_vector_copy (mat1[i], mat2[i], n);
}

/* Store the N x N identity matrix in MAT.  */

static void
lambda_matrix_id (lambda_matrix mat, int size)
{
  int i, j;

  for (i = 0; i < size; i++)
    for (j = 0; j < size; j++)
      mat[i][j] = (i == j) ? 1 : 0;
}

/* Return the index of the first nonzero element of vector VEC1 between
   START and N.  We must have START <= N.
   Returns N if VEC1 is the zero vector.  */

static int
lambda_vector_first_nz (lambda_vector vec1, int n, int start)
{
  int j = start;
  while (j < n && vec1[j] == 0)
    j++;
  return j;
}

/* Add a multiple of row R1 of matrix MAT with N columns to row R2:
   R2 = R2 + CONST1 * R1.  */

static bool
lambda_matrix_row_add (lambda_matrix mat, int n, int r1, int r2,
		       lambda_int const1)
{
  int i;

  if (const1 == 0)
    return true;

  for (i = 0; i < n; i++)
    {
      bool ovf;
      lambda_int tem = mul_hwi (mat[r1][i], const1, &ovf);
      if (ovf)
	return false;
      lambda_int tem2 = add_hwi (mat[r2][i], tem, &ovf);
      if (ovf || tem2 == HOST_WIDE_INT_MIN)
	return false;
      mat[r2][i] = tem2;
    }

  return true;
}

/* Multiply vector VEC1 of length SIZE by a constant CONST1,
   and store the result in VEC2.  */

static void
lambda_vector_mult_const (lambda_vector vec1, lambda_vector vec2,
			  int size, lambda_int const1)
{
  int i;

  if (const1 == 0)
    lambda_vector_clear (vec2, size);
  else
    for (i = 0; i < size; i++)
      vec2[i] = const1 * vec1[i];
}

/* Negate vector VEC1 with length SIZE and store it in VEC2.  */

static void
lambda_vector_negate (lambda_vector vec1, lambda_vector vec2,
		      int size)
{
  lambda_vector_mult_const (vec1, vec2, size, -1);
}

/* Negate row R1 of matrix MAT which has N columns.  */

static void
lambda_matrix_row_negate (lambda_matrix mat, int n, int r1)
{
  lambda_vector_negate (mat[r1], mat[r1], n);
}

/* Return true if two vectors are equal.  */

static bool
lambda_vector_equal (lambda_vector vec1, lambda_vector vec2, int size)
{
  int i;
  for (i = 0; i < size; i++)
    if (vec1[i] != vec2[i])
      return false;
  return true;
}

/* Given an M x N integer matrix A, this function determines an M x
   M unimodular matrix U, and an M x N echelon matrix S such that
   "U.A = S".  This decomposition is also known as "right Hermite".

   Ref: Algorithm 2.1 page 33 in "Loop Transformations for
   Restructuring Compilers" Utpal Banerjee.  */

static bool
lambda_matrix_right_hermite (lambda_matrix A, int m, int n,
			     lambda_matrix S, lambda_matrix U)
{
  int i, j, i0 = 0;

  lambda_matrix_copy (A, S, m, n);
  lambda_matrix_id (U, m);

  for (j = 0; j < n; j++)
    {
      if (lambda_vector_first_nz (S[j], m, i0) < m)
	{
	  ++i0;
	  for (i = m - 1; i >= i0; i--)
	    {
	      while (S[i][j] != 0)
		{
		  lambda_int factor, a, b;

		  a = S[i-1][j];
		  b = S[i][j];
		  gcc_assert (a != HOST_WIDE_INT_MIN);
		  factor = a / b;

		  if (!lambda_matrix_row_add (S, n, i, i-1, -factor))
		    return false;
		  std::swap (S[i], S[i-1]);

		  if (!lambda_matrix_row_add (U, m, i, i-1, -factor))
		    return false;
		  std::swap (U[i], U[i-1]);
		}
	    }
	}
    }

  return true;
}

/* Determines the overlapping elements due to accesses CHREC_A and
   CHREC_B, that are affine functions.  This function cannot handle
   symbolic evolution functions, ie. when initial conditions are
   parameters, because it uses lambda matrices of integers.  */

static void
analyze_subscript_affine_affine (tree chrec_a,
				 tree chrec_b,
				 conflict_function **overlaps_a,
				 conflict_function **overlaps_b,
				 tree *last_conflicts)
{
  unsigned nb_vars_a, nb_vars_b, dim;
  lambda_int gamma, gcd_alpha_beta;
  lambda_matrix A, U, S;
  struct obstack scratch_obstack;

  if (eq_evolutions_p (chrec_a, chrec_b))
    {
      /* The accessed index overlaps for each iteration in the
	 loop.  */
      *overlaps_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *overlaps_b = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *last_conflicts = chrec_dont_know;
      return;
    }
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

  gcc_obstack_init (&scratch_obstack);

  dim = nb_vars_a + nb_vars_b;
  U = lambda_matrix_new (dim, dim, &scratch_obstack);
  A = lambda_matrix_new (dim, 1, &scratch_obstack);
  S = lambda_matrix_new (dim, 1, &scratch_obstack);

  tree init_a = initialize_matrix_A (A, chrec_a, 0, 1);
  tree init_b = initialize_matrix_A (A, chrec_b, nb_vars_a, -1);
  if (init_a == chrec_dont_know
      || init_b == chrec_dont_know)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "affine-affine test failed: "
		 "representation issue.\n");
      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      goto end_analyze_subs_aa;
    }
  gamma = int_cst_value (init_b) - int_cst_value (init_a);

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
	  HOST_WIDE_INT step_a, step_b;
	  HOST_WIDE_INT niter, niter_a, niter_b;
	  affine_fn ova, ovb;

	  niter_a = max_stmt_executions_int (get_chrec_loop (chrec_a));
	  niter_b = max_stmt_executions_int (get_chrec_loop (chrec_b));
	  niter = MIN (niter_a, niter_b);
	  step_a = int_cst_value (CHREC_RIGHT (chrec_a));
	  step_b = int_cst_value (CHREC_RIGHT (chrec_b));

	  compute_overlap_steps_for_affine_univar (niter, step_a, step_b,
						   &ova, &ovb,
						   last_conflicts, 1);
	  *overlaps_a = conflict_fn (1, ova);
	  *overlaps_b = conflict_fn (1, ovb);
	}

      else if (nb_vars_a == 2 && nb_vars_b == 1)
	compute_overlap_steps_for_affine_1_2
	  (chrec_a, chrec_b, overlaps_a, overlaps_b, last_conflicts);

      else if (nb_vars_a == 1 && nb_vars_b == 2)
	compute_overlap_steps_for_affine_1_2
	  (chrec_b, chrec_a, overlaps_b, overlaps_a, last_conflicts);

      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "affine-affine test failed: too many variables.\n");
	  *overlaps_a = conflict_fn_not_known ();
	  *overlaps_b = conflict_fn_not_known ();
	  *last_conflicts = chrec_dont_know;
	}
      goto end_analyze_subs_aa;
    }

  /* U.A = S */
  if (!lambda_matrix_right_hermite (A, dim, 1, S, U))
    {
      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      goto end_analyze_subs_aa;
    }

  if (S[0][0] < 0)
    {
      S[0][0] *= -1;
      lambda_matrix_row_negate (U, dim, 0);
    }
  gcd_alpha_beta = S[0][0];

  /* Something went wrong: for example in {1, +, 0}_5 vs. {0, +, 0}_5,
     but that is a quite strange case.  Instead of ICEing, answer
     don't know.  */
  if (gcd_alpha_beta == 0)
    {
      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      goto end_analyze_subs_aa;
    }

  /* The classic "gcd-test".  */
  if (!int_divides_p (gcd_alpha_beta, gamma))
    {
      /* The "gcd-test" has determined that there is no integer
	 solution, i.e. there is no dependence.  */
      *overlaps_a = conflict_fn_no_dependence ();
      *overlaps_b = conflict_fn_no_dependence ();
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
      	  HOST_WIDE_INT i0, j0, i1, j1;

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
	      *overlaps_a = conflict_fn_no_dependence ();
	      *overlaps_b = conflict_fn_no_dependence ();
	      *last_conflicts = integer_zero_node;
	      goto end_analyze_subs_aa;
	    }

	  if (i1 > 0 && j1 > 0)
	    {
	      HOST_WIDE_INT niter_a
		= max_stmt_executions_int (get_chrec_loop (chrec_a));
	      HOST_WIDE_INT niter_b
		= max_stmt_executions_int (get_chrec_loop (chrec_b));
	      HOST_WIDE_INT niter = MIN (niter_a, niter_b);

	      /* (X0, Y0) is a solution of the Diophantine equation:
		 "chrec_a (X0) = chrec_b (Y0)".  */
	      HOST_WIDE_INT tau1 = MAX (CEIL (-i0, i1),
					CEIL (-j0, j1));
	      HOST_WIDE_INT x0 = i1 * tau1 + i0;
	      HOST_WIDE_INT y0 = j1 * tau1 + j0;

	      /* (X1, Y1) is the smallest positive solution of the eq
		 "chrec_a (X1) = chrec_b (Y1)", i.e. this is where the
		 first conflict occurs.  */
	      HOST_WIDE_INT min_multiple = MIN (x0 / i1, y0 / j1);
	      HOST_WIDE_INT x1 = x0 - i1 * min_multiple;
	      HOST_WIDE_INT y1 = y0 - j1 * min_multiple;

	      if (niter > 0)
		{
		  /* If the overlap occurs outside of the bounds of the
		     loop, there is no dependence.  */
		  if (x1 >= niter_a || y1 >= niter_b)
		    {
		      *overlaps_a = conflict_fn_no_dependence ();
		      *overlaps_b = conflict_fn_no_dependence ();
		      *last_conflicts = integer_zero_node;
		      goto end_analyze_subs_aa;
		    }

		  /* max stmt executions can get quite large, avoid
		     overflows by using wide ints here.  */
		  widest_int tau2
		    = wi::smin (wi::sdiv_floor (wi::sub (niter_a, i0), i1),
				wi::sdiv_floor (wi::sub (niter_b, j0), j1));
		  widest_int last_conflict = wi::sub (tau2, (x1 - i0)/i1);
		  if (wi::min_precision (last_conflict, SIGNED)
		      <= TYPE_PRECISION (integer_type_node))
		    *last_conflicts
		       = build_int_cst (integer_type_node,
					last_conflict.to_shwi ());
		  else
		    *last_conflicts = chrec_dont_know;
		}
	      else
		*last_conflicts = chrec_dont_know;

	      *overlaps_a
		= conflict_fn (1,
			       affine_fn_univar (build_int_cst (NULL_TREE, x1),
						 1,
						 build_int_cst (NULL_TREE, i1)));
	      *overlaps_b
		= conflict_fn (1,
			       affine_fn_univar (build_int_cst (NULL_TREE, y1),
						 1,
						 build_int_cst (NULL_TREE, j1)));
	    }
	  else
	    {
	      /* FIXME: For the moment, the upper bound of the
		 iteration domain for i and j is not checked.  */
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "affine-affine test failed: unimplemented.\n");
	      *overlaps_a = conflict_fn_not_known ();
	      *overlaps_b = conflict_fn_not_known ();
	      *last_conflicts = chrec_dont_know;
	    }
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "affine-affine test failed: unimplemented.\n");
	  *overlaps_a = conflict_fn_not_known ();
	  *overlaps_b = conflict_fn_not_known ();
	  *last_conflicts = chrec_dont_know;
	}
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "affine-affine test failed: unimplemented.\n");
      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
    }

end_analyze_subs_aa:
  obstack_free (&scratch_obstack, NULL);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (overlaps_a = ");
      dump_conflict_function (dump_file, *overlaps_a);
      fprintf (dump_file, ")\n  (overlaps_b = ");
      dump_conflict_function (dump_file, *overlaps_b);
      fprintf (dump_file, "))\n");
    }
}

/* Returns true when analyze_subscript_affine_affine can be used for
   determining the dependence relation between chrec_a and chrec_b,
   that contain symbols.  This function modifies chrec_a and chrec_b
   such that the analysis result is the same, and such that they don't
   contain symbols, and then can safely be passed to the analyzer.

   Example: The analysis of the following tuples of evolutions produce
   the same results: {x+1, +, 1}_1 vs. {x+3, +, 1}_1, and {-2, +, 1}_1
   vs. {0, +, 1}_1

   {x+1, +, 1}_1 ({2, +, 1}_1) = {x+3, +, 1}_1 ({0, +, 1}_1)
   {-2, +, 1}_1 ({2, +, 1}_1) = {0, +, 1}_1 ({0, +, 1}_1)
*/

static bool
can_use_analyze_subscript_affine_affine (tree *chrec_a, tree *chrec_b)
{
  tree diff, type, left_a, left_b, right_b;

  if (chrec_contains_symbols (CHREC_RIGHT (*chrec_a))
      || chrec_contains_symbols (CHREC_RIGHT (*chrec_b)))
    /* FIXME: For the moment not handled.  Might be refined later.  */
    return false;

  type = chrec_type (*chrec_a);
  left_a = CHREC_LEFT (*chrec_a);
  left_b = chrec_convert (type, CHREC_LEFT (*chrec_b), NULL);
  diff = chrec_fold_minus (type, left_a, left_b);

  if (!evolution_function_is_constant_p (diff))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "can_use_subscript_aff_aff_for_symbolic \n");

  *chrec_a = build_polynomial_chrec (CHREC_VARIABLE (*chrec_a),
				     diff, CHREC_RIGHT (*chrec_a));
  right_b = chrec_convert (type, CHREC_RIGHT (*chrec_b), NULL);
  *chrec_b = build_polynomial_chrec (CHREC_VARIABLE (*chrec_b),
				     build_int_cst (type, 0),
				     right_b);
  return true;
}

/* Analyze a SIV (Single Index Variable) subscript.  *OVERLAPS_A and
   *OVERLAPS_B are initialized to the functions that describe the
   relation between the elements accessed twice by CHREC_A and
   CHREC_B.  For k >= 0, the following property is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void
analyze_siv_subscript (tree chrec_a,
		       tree chrec_b,
		       conflict_function **overlaps_a,
		       conflict_function **overlaps_b,
		       tree *last_conflicts,
		       int loop_nest_num)
{
  dependence_stats.num_siv++;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_siv_subscript \n");

  if (evolution_function_is_constant_p (chrec_a)
      && evolution_function_is_affine_in_loop (chrec_b, loop_nest_num))
    analyze_siv_subscript_cst_affine (chrec_a, chrec_b,
				      overlaps_a, overlaps_b, last_conflicts);

  else if (evolution_function_is_affine_in_loop (chrec_a, loop_nest_num)
	   && evolution_function_is_constant_p (chrec_b))
    analyze_siv_subscript_cst_affine (chrec_b, chrec_a,
				      overlaps_b, overlaps_a, last_conflicts);

  else if (evolution_function_is_affine_in_loop (chrec_a, loop_nest_num)
	   && evolution_function_is_affine_in_loop (chrec_b, loop_nest_num))
    {
      if (!chrec_contains_symbols (chrec_a)
	  && !chrec_contains_symbols (chrec_b))
	{
	  analyze_subscript_affine_affine (chrec_a, chrec_b,
					   overlaps_a, overlaps_b,
					   last_conflicts);

	  if (CF_NOT_KNOWN_P (*overlaps_a)
	      || CF_NOT_KNOWN_P (*overlaps_b))
	    dependence_stats.num_siv_unimplemented++;
	  else if (CF_NO_DEPENDENCE_P (*overlaps_a)
		   || CF_NO_DEPENDENCE_P (*overlaps_b))
	    dependence_stats.num_siv_independent++;
	  else
	    dependence_stats.num_siv_dependent++;
	}
      else if (can_use_analyze_subscript_affine_affine (&chrec_a,
							&chrec_b))
	{
	  analyze_subscript_affine_affine (chrec_a, chrec_b,
					   overlaps_a, overlaps_b,
					   last_conflicts);

	  if (CF_NOT_KNOWN_P (*overlaps_a)
	      || CF_NOT_KNOWN_P (*overlaps_b))
	    dependence_stats.num_siv_unimplemented++;
	  else if (CF_NO_DEPENDENCE_P (*overlaps_a)
		   || CF_NO_DEPENDENCE_P (*overlaps_b))
	    dependence_stats.num_siv_independent++;
	  else
	    dependence_stats.num_siv_dependent++;
	}
      else
	goto siv_subscript_dontknow;
    }

  else
    {
    siv_subscript_dontknow:;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  siv test failed: unimplemented");
      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      dependence_stats.num_siv_unimplemented++;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Returns false if we can prove that the greatest common divisor of the steps
   of CHREC does not divide CST, false otherwise.  */

static bool
gcd_of_steps_may_divide_p (const_tree chrec, const_tree cst)
{
  HOST_WIDE_INT cd = 0, val;
  tree step;

  if (!tree_fits_shwi_p (cst))
    return true;
  val = tree_to_shwi (cst);

  while (TREE_CODE (chrec) == POLYNOMIAL_CHREC)
    {
      step = CHREC_RIGHT (chrec);
      if (!tree_fits_shwi_p (step))
	return true;
      cd = gcd (cd, tree_to_shwi (step));
      chrec = CHREC_LEFT (chrec);
    }

  return val % cd == 0;
}

/* Analyze a MIV (Multiple Index Variable) subscript with respect to
   LOOP_NEST.  *OVERLAPS_A and *OVERLAPS_B are initialized to the
   functions that describe the relation between the elements accessed
   twice by CHREC_A and CHREC_B.  For k >= 0, the following property
   is verified:

   CHREC_A (*OVERLAPS_A (k)) = CHREC_B (*OVERLAPS_B (k)).  */

static void
analyze_miv_subscript (tree chrec_a,
		       tree chrec_b,
		       conflict_function **overlaps_a,
		       conflict_function **overlaps_b,
		       tree *last_conflicts,
		       class loop *loop_nest)
{
  tree type, difference;

  dependence_stats.num_miv++;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "(analyze_miv_subscript \n");

  type = signed_type_for_types (TREE_TYPE (chrec_a), TREE_TYPE (chrec_b));
  chrec_a = chrec_convert (type, chrec_a, NULL);
  chrec_b = chrec_convert (type, chrec_b, NULL);
  difference = chrec_fold_minus (type, chrec_a, chrec_b);

  if (eq_evolutions_p (chrec_a, chrec_b))
    {
      /* Access functions are the same: all the elements are accessed
	 in the same order.  */
      *overlaps_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *overlaps_b = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *last_conflicts = max_stmt_executions_tree (get_chrec_loop (chrec_a));
      dependence_stats.num_miv_dependent++;
    }

  else if (evolution_function_is_constant_p (difference)
	   && evolution_function_is_affine_multivariate_p (chrec_a,
							   loop_nest->num)
	   && !gcd_of_steps_may_divide_p (chrec_a, difference))
    {
      /* testsuite/.../ssa-chrec-33.c
	 {{21, +, 2}_1, +, -2}_2  vs.  {{20, +, 2}_1, +, -2}_2

	 The difference is 1, and all the evolution steps are multiples
	 of 2, consequently there are no overlapping elements.  */
      *overlaps_a = conflict_fn_no_dependence ();
      *overlaps_b = conflict_fn_no_dependence ();
      *last_conflicts = integer_zero_node;
      dependence_stats.num_miv_independent++;
    }

  else if (evolution_function_is_affine_in_loop (chrec_a, loop_nest->num)
	   && !chrec_contains_symbols (chrec_a, loop_nest)
	   && evolution_function_is_affine_in_loop (chrec_b, loop_nest->num)
	   && !chrec_contains_symbols (chrec_b, loop_nest))
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

      if (CF_NOT_KNOWN_P (*overlaps_a)
 	  || CF_NOT_KNOWN_P (*overlaps_b))
	dependence_stats.num_miv_unimplemented++;
      else if (CF_NO_DEPENDENCE_P (*overlaps_a)
	       || CF_NO_DEPENDENCE_P (*overlaps_b))
	dependence_stats.num_miv_independent++;
      else
	dependence_stats.num_miv_dependent++;
    }

  else
    {
      /* When the analysis is too difficult, answer "don't know".  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "analyze_miv_subscript test failed: unimplemented.\n");

      *overlaps_a = conflict_fn_not_known ();
      *overlaps_b = conflict_fn_not_known ();
      *last_conflicts = chrec_dont_know;
      dependence_stats.num_miv_unimplemented++;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ")\n");
}

/* Determines the iterations for which CHREC_A is equal to CHREC_B in
   with respect to LOOP_NEST.  OVERLAP_ITERATIONS_A and
   OVERLAP_ITERATIONS_B are initialized with two functions that
   describe the iterations that contain conflicting elements.

   Remark: For an integer k >= 0, the following equality is true:

   CHREC_A (OVERLAP_ITERATIONS_A (k)) == CHREC_B (OVERLAP_ITERATIONS_B (k)).
*/

static void
analyze_overlapping_iterations (tree chrec_a,
				tree chrec_b,
				conflict_function **overlap_iterations_a,
				conflict_function **overlap_iterations_b,
				tree *last_conflicts, class loop *loop_nest)
{
  unsigned int lnn = loop_nest->num;

  dependence_stats.num_subscript_tests++;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(analyze_overlapping_iterations \n");
      fprintf (dump_file, "  (chrec_a = ");
      print_generic_expr (dump_file, chrec_a);
      fprintf (dump_file, ")\n  (chrec_b = ");
      print_generic_expr (dump_file, chrec_b);
      fprintf (dump_file, ")\n");
    }

  if (chrec_a == NULL_TREE
      || chrec_b == NULL_TREE
      || chrec_contains_undetermined (chrec_a)
      || chrec_contains_undetermined (chrec_b))
    {
      dependence_stats.num_subscript_undetermined++;

      *overlap_iterations_a = conflict_fn_not_known ();
      *overlap_iterations_b = conflict_fn_not_known ();
    }

  /* If they are the same chrec, and are affine, they overlap
     on every iteration.  */
  else if (eq_evolutions_p (chrec_a, chrec_b)
	   && (evolution_function_is_affine_multivariate_p (chrec_a, lnn)
	       || operand_equal_p (chrec_a, chrec_b, 0)))
    {
      dependence_stats.num_same_subscript_function++;
      *overlap_iterations_a = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *overlap_iterations_b = conflict_fn (1, affine_fn_cst (integer_zero_node));
      *last_conflicts = chrec_dont_know;
    }

  /* If they aren't the same, and aren't affine, we can't do anything
     yet.  */
  else if ((chrec_contains_symbols (chrec_a)
	    || chrec_contains_symbols (chrec_b))
	   && (!evolution_function_is_affine_multivariate_p (chrec_a, lnn)
	       || !evolution_function_is_affine_multivariate_p (chrec_b, lnn)))
    {
      dependence_stats.num_subscript_undetermined++;
      *overlap_iterations_a = conflict_fn_not_known ();
      *overlap_iterations_b = conflict_fn_not_known ();
    }

  else if (ziv_subscript_p (chrec_a, chrec_b))
    analyze_ziv_subscript (chrec_a, chrec_b,
			   overlap_iterations_a, overlap_iterations_b,
			   last_conflicts);

  else if (siv_subscript_p (chrec_a, chrec_b))
    analyze_siv_subscript (chrec_a, chrec_b,
			   overlap_iterations_a, overlap_iterations_b,
			   last_conflicts, lnn);

  else
    analyze_miv_subscript (chrec_a, chrec_b,
			   overlap_iterations_a, overlap_iterations_b,
			   last_conflicts, loop_nest);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  (overlap_iterations_a = ");
      dump_conflict_function (dump_file, *overlap_iterations_a);
      fprintf (dump_file, ")\n  (overlap_iterations_b = ");
      dump_conflict_function (dump_file, *overlap_iterations_b);
      fprintf (dump_file, "))\n");
    }
}

/* Helper function for uniquely inserting distance vectors.  */

static void
save_dist_v (struct data_dependence_relation *ddr, lambda_vector dist_v)
{
  for (lambda_vector v : DDR_DIST_VECTS (ddr))
    if (lambda_vector_equal (v, dist_v, DDR_NB_LOOPS (ddr)))
      return;

  DDR_DIST_VECTS (ddr).safe_push (dist_v);
}

/* Helper function for uniquely inserting direction vectors.  */

static void
save_dir_v (struct data_dependence_relation *ddr, lambda_vector dir_v)
{
  for (lambda_vector v : DDR_DIR_VECTS (ddr))
    if (lambda_vector_equal (v, dir_v, DDR_NB_LOOPS (ddr)))
      return;

  DDR_DIR_VECTS (ddr).safe_push (dir_v);
}

/* Add a distance of 1 on all the loops outer than INDEX.  If we
   haven't yet determined a distance for this outer loop, push a new
   distance vector composed of the previous distance, and a distance
   of 1 for this outer loop.  Example:

   | loop_1
   |   loop_2
   |     A[10]
   |   endloop_2
   | endloop_1

   Saved vectors are of the form (dist_in_1, dist_in_2).  First, we
   save (0, 1), then we have to save (1, 0).  */

static void
add_outer_distances (struct data_dependence_relation *ddr,
		     lambda_vector dist_v, int index)
{
  /* For each outer loop where init_v is not set, the accesses are
     in dependence of distance 1 in the loop.  */
  while (--index >= 0)
    {
      lambda_vector save_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
      lambda_vector_copy (dist_v, save_v, DDR_NB_LOOPS (ddr));
      save_v[index] = 1;
      save_dist_v (ddr, save_v);
    }
}

/* Return false when fail to represent the data dependence as a
   distance vector.  A_INDEX is the index of the first reference
   (0 for DDR_A, 1 for DDR_B) and B_INDEX is the index of the
   second reference.  INIT_B is set to true when a component has been
   added to the distance vector DIST_V.  INDEX_CARRY is then set to
   the index in DIST_V that carries the dependence.  */

static bool
build_classic_dist_vector_1 (struct data_dependence_relation *ddr,
			     unsigned int a_index, unsigned int b_index,
			     lambda_vector dist_v, bool *init_b,
			     int *index_carry)
{
  unsigned i;
  lambda_vector init_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
  class loop *loop = DDR_LOOP_NEST (ddr)[0];

  for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
    {
      tree access_fn_a, access_fn_b;
      struct subscript *subscript = DDR_SUBSCRIPT (ddr, i);

      if (chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	{
	  non_affine_dependence_relation (ddr);
	  return false;
	}

      access_fn_a = SUB_ACCESS_FN (subscript, a_index);
      access_fn_b = SUB_ACCESS_FN (subscript, b_index);

      if (TREE_CODE (access_fn_a) == POLYNOMIAL_CHREC
	  && TREE_CODE (access_fn_b) == POLYNOMIAL_CHREC)
	{
	  HOST_WIDE_INT dist;
	  int index;
	  int var_a = CHREC_VARIABLE (access_fn_a);
	  int var_b = CHREC_VARIABLE (access_fn_b);

	  if (var_a != var_b
	      || chrec_contains_undetermined (SUB_DISTANCE (subscript)))
	    {
	      non_affine_dependence_relation (ddr);
	      return false;
	    }

	  /* When data references are collected in a loop while data
	     dependences are analyzed in loop nest nested in the loop, we
	     would have more number of access functions than number of
	     loops.  Skip access functions of loops not in the loop nest.

	     See PR89725 for more information.  */
	  if (flow_loop_nested_p (get_loop (cfun, var_a), loop))
	    continue;

	  dist = int_cst_value (SUB_DISTANCE (subscript));
	  index = index_in_loop_nest (var_a, DDR_LOOP_NEST (ddr));
	  *index_carry = MIN (index, *index_carry);

	  /* This is the subscript coupling test.  If we have already
	     recorded a distance for this loop (a distance coming from
	     another subscript), it should be the same.  For example,
	     in the following code, there is no dependence:

	     | loop i = 0, N, 1
	     |   T[i+1][i] = ...
	     |   ... = T[i][i]
	     | endloop
	  */
	  if (init_v[index] != 0 && dist_v[index] != dist)
	    {
	      finalize_ddr_dependent (ddr, chrec_known);
	      return false;
	    }

	  dist_v[index] = dist;
	  init_v[index] = 1;
	  *init_b = true;
	}
      else if (!operand_equal_p (access_fn_a, access_fn_b, 0))
	{
	  /* This can be for example an affine vs. constant dependence
	     (T[i] vs. T[3]) that is not an affine dependence and is
	     not representable as a distance vector.  */
	  non_affine_dependence_relation (ddr);
	  return false;
	}
      else
	*init_b = true;
    }

  return true;
}

/* Return true when the DDR contains only invariant access functions wrto. loop
   number LNUM.  */

static bool
invariant_access_functions (const struct data_dependence_relation *ddr,
			    int lnum)
{
  for (subscript *sub : DDR_SUBSCRIPTS (ddr))
    if (!evolution_function_is_invariant_p (SUB_ACCESS_FN (sub, 0), lnum)
	|| !evolution_function_is_invariant_p (SUB_ACCESS_FN (sub, 1), lnum))
      return false;

  return true;
}

/* Helper function for the case where DDR_A and DDR_B are the same
   multivariate access function with a constant step.  For an example
   see pr34635-1.c.  */

static void
add_multivariate_self_dist (struct data_dependence_relation *ddr, tree c_2)
{
  int x_1, x_2;
  tree c_1 = CHREC_LEFT (c_2);
  tree c_0 = CHREC_LEFT (c_1);
  lambda_vector dist_v;
  HOST_WIDE_INT v1, v2, cd;

  /* Polynomials with more than 2 variables are not handled yet.  When
     the evolution steps are parameters, it is not possible to
     represent the dependence using classical distance vectors.  */
  if (TREE_CODE (c_0) != INTEGER_CST
      || TREE_CODE (CHREC_RIGHT (c_1)) != INTEGER_CST
      || TREE_CODE (CHREC_RIGHT (c_2)) != INTEGER_CST)
    {
      DDR_AFFINE_P (ddr) = false;
      return;
    }

  x_2 = index_in_loop_nest (CHREC_VARIABLE (c_2), DDR_LOOP_NEST (ddr));
  x_1 = index_in_loop_nest (CHREC_VARIABLE (c_1), DDR_LOOP_NEST (ddr));

  /* For "{{0, +, 2}_1, +, 3}_2" the distance vector is (3, -2).  */
  dist_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
  v1 = int_cst_value (CHREC_RIGHT (c_1));
  v2 = int_cst_value (CHREC_RIGHT (c_2));
  cd = gcd (v1, v2);
  v1 /= cd;
  v2 /= cd;

  if (v2 < 0)
    {
      v2 = -v2;
      v1 = -v1;
    }

  dist_v[x_1] = v2;
  dist_v[x_2] = -v1;
  save_dist_v (ddr, dist_v);

  add_outer_distances (ddr, dist_v, x_1);
}

/* Helper function for the case where DDR_A and DDR_B are the same
   access functions.  */

static void
add_other_self_distances (struct data_dependence_relation *ddr)
{
  lambda_vector dist_v;
  unsigned i;
  int index_carry = DDR_NB_LOOPS (ddr);
  subscript *sub;
  class loop *loop = DDR_LOOP_NEST (ddr)[0];

  FOR_EACH_VEC_ELT (DDR_SUBSCRIPTS (ddr), i, sub)
    {
      tree access_fun = SUB_ACCESS_FN (sub, 0);

      if (TREE_CODE (access_fun) == POLYNOMIAL_CHREC)
	{
	  if (!evolution_function_is_univariate_p (access_fun, loop->num))
	    {
	      if (DDR_NUM_SUBSCRIPTS (ddr) != 1)
		{
		  DDR_ARE_DEPENDENT (ddr) = chrec_dont_know;
		  return;
		}

	      access_fun = SUB_ACCESS_FN (DDR_SUBSCRIPT (ddr, 0), 0);

	      if (TREE_CODE (CHREC_LEFT (access_fun)) == POLYNOMIAL_CHREC)
		add_multivariate_self_dist (ddr, access_fun);
	      else
		/* The evolution step is not constant: it varies in
		   the outer loop, so this cannot be represented by a
		   distance vector.  For example in pr34635.c the
		   evolution is {0, +, {0, +, 4}_1}_2.  */
		DDR_AFFINE_P (ddr) = false;

	      return;
	    }

	  /* When data references are collected in a loop while data
	     dependences are analyzed in loop nest nested in the loop, we
	     would have more number of access functions than number of
	     loops.  Skip access functions of loops not in the loop nest.

	     See PR89725 for more information.  */
	  if (flow_loop_nested_p (get_loop (cfun, CHREC_VARIABLE (access_fun)),
				  loop))
	    continue;

	  index_carry = MIN (index_carry,
			     index_in_loop_nest (CHREC_VARIABLE (access_fun),
						 DDR_LOOP_NEST (ddr)));
	}
    }

  dist_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
  add_outer_distances (ddr, dist_v, index_carry);
}

static void
insert_innermost_unit_dist_vector (struct data_dependence_relation *ddr)
{
  lambda_vector dist_v = lambda_vector_new (DDR_NB_LOOPS (ddr));

  dist_v[0] = 1;
  save_dist_v (ddr, dist_v);
}

/* Adds a unit distance vector to DDR when there is a 0 overlap.  This
   is the case for example when access functions are the same and
   equal to a constant, as in:

   | loop_1
   |   A[3] = ...
   |   ... = A[3]
   | endloop_1

   in which case the distance vectors are (0) and (1).  */

static void
add_distance_for_zero_overlaps (struct data_dependence_relation *ddr)
{
  unsigned i, j;

  for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
    {
      subscript_p sub = DDR_SUBSCRIPT (ddr, i);
      conflict_function *ca = SUB_CONFLICTS_IN_A (sub);
      conflict_function *cb = SUB_CONFLICTS_IN_B (sub);

      for (j = 0; j < ca->n; j++)
	if (affine_function_zero_p (ca->fns[j]))
	  {
	    insert_innermost_unit_dist_vector (ddr);
	    return;
	  }

      for (j = 0; j < cb->n; j++)
	if (affine_function_zero_p (cb->fns[j]))
	  {
	    insert_innermost_unit_dist_vector (ddr);
	    return;
	  }
    }
}

/* Return true when the DDR contains two data references that have the
   same access functions.  */

static inline bool
same_access_functions (const struct data_dependence_relation *ddr)
{
  for (subscript *sub : DDR_SUBSCRIPTS (ddr))
    if (!eq_evolutions_p (SUB_ACCESS_FN (sub, 0),
			  SUB_ACCESS_FN (sub, 1)))
      return false;

  return true;
}

/* Compute the classic per loop distance vector.  DDR is the data
   dependence relation to build a vector from.  Return false when fail
   to represent the data dependence as a distance vector.  */

static bool
build_classic_dist_vector (struct data_dependence_relation *ddr,
			   class loop *loop_nest)
{
  bool init_b = false;
  int index_carry = DDR_NB_LOOPS (ddr);
  lambda_vector dist_v;

  if (DDR_ARE_DEPENDENT (ddr) != NULL_TREE)
    return false;

  if (same_access_functions (ddr))
    {
      /* Save the 0 vector.  */
      dist_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
      save_dist_v (ddr, dist_v);

      if (invariant_access_functions (ddr, loop_nest->num))
	add_distance_for_zero_overlaps (ddr);

      if (DDR_NB_LOOPS (ddr) > 1)
	add_other_self_distances (ddr);

      return true;
    }

  dist_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
  if (!build_classic_dist_vector_1 (ddr, 0, 1, dist_v, &init_b, &index_carry))
    return false;

  /* Save the distance vector if we initialized one.  */
  if (init_b)
    {
      /* Verify a basic constraint: classic distance vectors should
	 always be lexicographically positive.

	 Data references are collected in the order of execution of
	 the program, thus for the following loop

	 | for (i = 1; i < 100; i++)
	 |   for (j = 1; j < 100; j++)
	 |     {
	 |       t = T[j+1][i-1];  // A
	 |       T[j][i] = t + 2;  // B
	 |     }

	 references are collected following the direction of the wind:
	 A then B.  The data dependence tests are performed also
	 following this order, such that we're looking at the distance
	 separating the elements accessed by A from the elements later
	 accessed by B.  But in this example, the distance returned by
	 test_dep (A, B) is lexicographically negative (-1, 1), that
	 means that the access A occurs later than B with respect to
	 the outer loop, ie. we're actually looking upwind.  In this
	 case we solve test_dep (B, A) looking downwind to the
	 lexicographically positive solution, that returns the
	 distance vector (1, -1).  */
      if (!lambda_vector_lexico_pos (dist_v, DDR_NB_LOOPS (ddr)))
	{
	  lambda_vector save_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
	  if (!subscript_dependence_tester_1 (ddr, 1, 0, loop_nest))
	    return false;
	  compute_subscript_distance (ddr);
	  if (!build_classic_dist_vector_1 (ddr, 1, 0, save_v, &init_b,
					    &index_carry))
	    return false;
	  save_dist_v (ddr, save_v);
	  DDR_REVERSED_P (ddr) = true;

	  /* In this case there is a dependence forward for all the
	     outer loops:

	     | for (k = 1; k < 100; k++)
	     |  for (i = 1; i < 100; i++)
	     |   for (j = 1; j < 100; j++)
	     |     {
	     |       t = T[j+1][i-1];  // A
	     |       T[j][i] = t + 2;  // B
	     |     }

	     the vectors are:
	     (0,  1, -1)
	     (1,  1, -1)
	     (1, -1,  1)
	  */
	  if (DDR_NB_LOOPS (ddr) > 1)
	    {
 	      add_outer_distances (ddr, save_v, index_carry);
	      add_outer_distances (ddr, dist_v, index_carry);
	    }
	}
      else
	{
	  lambda_vector save_v = lambda_vector_new (DDR_NB_LOOPS (ddr));
	  lambda_vector_copy (dist_v, save_v, DDR_NB_LOOPS (ddr));

	  if (DDR_NB_LOOPS (ddr) > 1)
	    {
	      lambda_vector opposite_v = lambda_vector_new (DDR_NB_LOOPS (ddr));

	      if (!subscript_dependence_tester_1 (ddr, 1, 0, loop_nest))
		return false;
	      compute_subscript_distance (ddr);
	      if (!build_classic_dist_vector_1 (ddr, 1, 0, opposite_v, &init_b,
						&index_carry))
		return false;

	      save_dist_v (ddr, save_v);
	      add_outer_distances (ddr, dist_v, index_carry);
	      add_outer_distances (ddr, opposite_v, index_carry);
	    }
	  else
	    save_dist_v (ddr, save_v);
	}
    }
  else
    {
      /* There is a distance of 1 on all the outer loops: Example:
	 there is a dependence of distance 1 on loop_1 for the array A.

	 | loop_1
	 |   A[5] = ...
	 | endloop
      */
      add_outer_distances (ddr, dist_v,
			   lambda_vector_first_nz (dist_v,
						   DDR_NB_LOOPS (ddr), 0));
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      unsigned i;

      fprintf (dump_file, "(build_classic_dist_vector\n");
      for (i = 0; i < DDR_NUM_DIST_VECTS (ddr); i++)
	{
	  fprintf (dump_file, "  dist_vector = (");
	  print_lambda_vector (dump_file, DDR_DIST_VECT (ddr, i),
			       DDR_NB_LOOPS (ddr));
	  fprintf (dump_file, "  )\n");
	}
      fprintf (dump_file, ")\n");
    }

  return true;
}

/* Return the direction for a given distance.
   FIXME: Computing dir this way is suboptimal, since dir can catch
   cases that dist is unable to represent.  */

static inline enum data_dependence_direction
dir_from_dist (int dist)
{
  if (dist > 0)
    return dir_positive;
  else if (dist < 0)
    return dir_negative;
  else
    return dir_equal;
}

/* Compute the classic per loop direction vector.  DDR is the data
   dependence relation to build a vector from.  */

static void
build_classic_dir_vector (struct data_dependence_relation *ddr)
{
  unsigned i, j;
  lambda_vector dist_v;

  FOR_EACH_VEC_ELT (DDR_DIST_VECTS (ddr), i, dist_v)
    {
      lambda_vector dir_v = lambda_vector_new (DDR_NB_LOOPS (ddr));

      for (j = 0; j < DDR_NB_LOOPS (ddr); j++)
	dir_v[j] = dir_from_dist (dist_v[j]);

      save_dir_v (ddr, dir_v);
    }
}

/* Helper function.  Returns true when there is a dependence between the
   data references.  A_INDEX is the index of the first reference (0 for
   DDR_A, 1 for DDR_B) and B_INDEX is the index of the second reference.  */

static bool
subscript_dependence_tester_1 (struct data_dependence_relation *ddr,
			       unsigned int a_index, unsigned int b_index,
			       class loop *loop_nest)
{
  unsigned int i;
  tree last_conflicts;
  struct subscript *subscript;
  tree res = NULL_TREE;

  for (i = 0; DDR_SUBSCRIPTS (ddr).iterate (i, &subscript); i++)
    {
      conflict_function *overlaps_a, *overlaps_b;

      analyze_overlapping_iterations (SUB_ACCESS_FN (subscript, a_index),
				      SUB_ACCESS_FN (subscript, b_index),
				      &overlaps_a, &overlaps_b,
				      &last_conflicts, loop_nest);

      if (SUB_CONFLICTS_IN_A (subscript))
	free_conflict_function (SUB_CONFLICTS_IN_A (subscript));
      if (SUB_CONFLICTS_IN_B (subscript))
	free_conflict_function (SUB_CONFLICTS_IN_B (subscript));

      SUB_CONFLICTS_IN_A (subscript) = overlaps_a;
      SUB_CONFLICTS_IN_B (subscript) = overlaps_b;
      SUB_LAST_CONFLICT (subscript) = last_conflicts;

      /* If there is any undetermined conflict function we have to
         give a conservative answer in case we cannot prove that
	 no dependence exists when analyzing another subscript.  */
      if (CF_NOT_KNOWN_P (overlaps_a)
 	  || CF_NOT_KNOWN_P (overlaps_b))
 	{
	  res = chrec_dont_know;
	  continue;
 	}

      /* When there is a subscript with no dependence we can stop.  */
      else if (CF_NO_DEPENDENCE_P (overlaps_a)
 	       || CF_NO_DEPENDENCE_P (overlaps_b))
 	{
	  res = chrec_known;
	  break;
 	}
    }

  if (res == NULL_TREE)
    return true;

  if (res == chrec_known)
    dependence_stats.num_dependence_independent++;
  else
    dependence_stats.num_dependence_undetermined++;
  finalize_ddr_dependent (ddr, res);
  return false;
}

/* Computes the conflicting iterations in LOOP_NEST, and initialize DDR.  */

static void
subscript_dependence_tester (struct data_dependence_relation *ddr,
			     class loop *loop_nest)
{
  if (subscript_dependence_tester_1 (ddr, 0, 1, loop_nest))
    dependence_stats.num_dependence_dependent++;

  compute_subscript_distance (ddr);
  if (build_classic_dist_vector (ddr, loop_nest))
    build_classic_dir_vector (ddr);
}

/* Returns true when all the access functions of A are affine or
   constant with respect to LOOP_NEST.  */

static bool
access_functions_are_affine_or_constant_p (const struct data_reference *a,
					   const class loop *loop_nest)
{
  vec<tree> fns = DR_ACCESS_FNS (a);
  for (tree t : fns)
    if (!evolution_function_is_invariant_p (t, loop_nest->num)
	&& !evolution_function_is_affine_multivariate_p (t, loop_nest->num))
      return false;

  return true;
}

/* This computes the affine dependence relation between A and B with
   respect to LOOP_NEST.  CHREC_KNOWN is used for representing the
   independence between two accesses, while CHREC_DONT_KNOW is used
   for representing the unknown relation.

   Note that it is possible to stop the computation of the dependence
   relation the first time we detect a CHREC_KNOWN element for a given
   subscript.  */

void
compute_affine_dependence (struct data_dependence_relation *ddr,
			   class loop *loop_nest)
{
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "(compute_affine_dependence\n");
      fprintf (dump_file, "  ref_a: ");
      print_generic_expr (dump_file, DR_REF (dra));
      fprintf (dump_file, ", stmt_a: ");
      print_gimple_stmt (dump_file, DR_STMT (dra), 0, TDF_SLIM);
      fprintf (dump_file, "  ref_b: ");
      print_generic_expr (dump_file, DR_REF (drb));
      fprintf (dump_file, ", stmt_b: ");
      print_gimple_stmt (dump_file, DR_STMT (drb), 0, TDF_SLIM);
    }

  /* Analyze only when the dependence relation is not yet known.  */
  if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
    {
      dependence_stats.num_dependence_tests++;

      if (access_functions_are_affine_or_constant_p (dra, loop_nest)
	  && access_functions_are_affine_or_constant_p (drb, loop_nest))
	subscript_dependence_tester (ddr, loop_nest);

      /* As a last case, if the dependence cannot be determined, or if
	 the dependence is considered too difficult to determine, answer
	 "don't know".  */
      else
	{
	  dependence_stats.num_dependence_undetermined++;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Data ref a:\n");
	      dump_data_reference (dump_file, dra);
	      fprintf (dump_file, "Data ref b:\n");
	      dump_data_reference (dump_file, drb);
	      fprintf (dump_file, "affine dependence test not usable: access function not affine or constant.\n");
	    }
	  finalize_ddr_dependent (ddr, chrec_dont_know);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
	fprintf (dump_file, ") -> no dependence\n");
      else if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
	fprintf (dump_file, ") -> dependence analysis failed\n");
      else
	fprintf (dump_file, ")\n");
    }
}

/* Compute in DEPENDENCE_RELATIONS the data dependence graph for all
   the data references in DATAREFS, in the LOOP_NEST.  When
   COMPUTE_SELF_AND_RR is FALSE, don't compute read-read and self
   relations.  Return true when successful, i.e. data references number
   is small enough to be handled.  */

bool
compute_all_dependences (const vec<data_reference_p> &datarefs,
			 vec<ddr_p> *dependence_relations,
			 const vec<loop_p> &loop_nest,
			 bool compute_self_and_rr)
{
  struct data_dependence_relation *ddr;
  struct data_reference *a, *b;
  unsigned int i, j;

  if ((int) datarefs.length ()
      > param_loop_max_datarefs_for_datadeps)
    {
      struct data_dependence_relation *ddr;

      /* Insert a single relation into dependence_relations:
	 chrec_dont_know.  */
      ddr = initialize_data_dependence_relation (NULL, NULL, loop_nest);
      dependence_relations->safe_push (ddr);
      return false;
    }

  FOR_EACH_VEC_ELT (datarefs, i, a)
    for (j = i + 1; datarefs.iterate (j, &b); j++)
      if (DR_IS_WRITE (a) || DR_IS_WRITE (b) || compute_self_and_rr)
	{
	  ddr = initialize_data_dependence_relation (a, b, loop_nest);
	  dependence_relations->safe_push (ddr);
          if (loop_nest.exists ())
   	    compute_affine_dependence (ddr, loop_nest[0]);
	}

  if (compute_self_and_rr)
    FOR_EACH_VEC_ELT (datarefs, i, a)
      {
	ddr = initialize_data_dependence_relation (a, a, loop_nest);
	dependence_relations->safe_push (ddr);
        if (loop_nest.exists ())
   	  compute_affine_dependence (ddr, loop_nest[0]);
      }

  return true;
}

/* Describes a location of a memory reference.  */

struct data_ref_loc
{
  /* The memory reference.  */
  tree ref;

  /* True if the memory reference is read.  */
  bool is_read;

  /* True if the data reference is conditional within the containing
     statement, i.e. if it might not occur even when the statement
     is executed and runs to completion.  */
  bool is_conditional_in_stmt;
};


/* Stores the locations of memory references in STMT to REFERENCES.  Returns
   true if STMT clobbers memory, false otherwise.  */

static bool
get_references_in_stmt (gimple *stmt, vec<data_ref_loc, va_heap> *references)
{
  bool clobbers_memory = false;
  data_ref_loc ref;
  tree op0, op1;
  enum gimple_code stmt_code = gimple_code (stmt);

  /* ASM_EXPR and CALL_EXPR may embed arbitrary side effects.
     As we cannot model data-references to not spelled out
     accesses give up if they may occur.  */
  if (stmt_code == GIMPLE_CALL
      && !(gimple_call_flags (stmt) & ECF_CONST))
    {
      /* Allow IFN_GOMP_SIMD_LANE in their own loops.  */
      if (gimple_call_internal_p (stmt))
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_GOMP_SIMD_LANE:
	    {
	      class loop *loop = gimple_bb (stmt)->loop_father;
	      tree uid = gimple_call_arg (stmt, 0);
	      gcc_assert (TREE_CODE (uid) == SSA_NAME);
	      if (loop == NULL
		  || loop->simduid != SSA_NAME_VAR (uid))
		clobbers_memory = true;
	      break;
	    }
	  case IFN_MASK_LOAD:
	  case IFN_MASK_STORE:
	    break;
	  default:
	    clobbers_memory = true;
	    break;
	  }
      else
	clobbers_memory = true;
    }
  else if (stmt_code == GIMPLE_ASM
	   && (gimple_asm_volatile_p (as_a <gasm *> (stmt))
	       || gimple_vuse (stmt)))
    clobbers_memory = true;

  if (!gimple_vuse (stmt))
    return clobbers_memory;

  if (stmt_code == GIMPLE_ASSIGN)
    {
      tree base;
      op0 = gimple_assign_lhs (stmt);
      op1 = gimple_assign_rhs1 (stmt);

      if (DECL_P (op1)
	  || (REFERENCE_CLASS_P (op1)
	      && (base = get_base_address (op1))
	      && TREE_CODE (base) != SSA_NAME
	      && !is_gimple_min_invariant (base)))
	{
	  ref.ref = op1;
	  ref.is_read = true;
	  ref.is_conditional_in_stmt = false;
	  references->safe_push (ref);
	}
    }
  else if (stmt_code == GIMPLE_CALL)
    {
      unsigned i, n;
      tree ptr, type;
      unsigned int align;

      ref.is_read = false;
      if (gimple_call_internal_p (stmt))
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_MASK_LOAD:
	    if (gimple_call_lhs (stmt) == NULL_TREE)
	      break;
	    ref.is_read = true;
	    /* FALLTHRU */
	  case IFN_MASK_STORE:
	    ptr = build_int_cst (TREE_TYPE (gimple_call_arg (stmt, 1)), 0);
	    align = tree_to_shwi (gimple_call_arg (stmt, 1));
	    if (ref.is_read)
	      type = TREE_TYPE (gimple_call_lhs (stmt));
	    else
	      type = TREE_TYPE (gimple_call_arg (stmt, 3));
	    if (TYPE_ALIGN (type) != align)
	      type = build_aligned_type (type, align);
	    ref.is_conditional_in_stmt = true;
	    ref.ref = fold_build2 (MEM_REF, type, gimple_call_arg (stmt, 0),
				   ptr);
	    references->safe_push (ref);
	    return false;
	  default:
	    break;
	  }

      op0 = gimple_call_lhs (stmt);
      n = gimple_call_num_args (stmt);
      for (i = 0; i < n; i++)
	{
	  op1 = gimple_call_arg (stmt, i);

	  if (DECL_P (op1)
	      || (REFERENCE_CLASS_P (op1) && get_base_address (op1)))
	    {
	      ref.ref = op1;
	      ref.is_read = true;
	      ref.is_conditional_in_stmt = false;
	      references->safe_push (ref);
	    }
	}
    }
  else
    return clobbers_memory;

  if (op0
      && (DECL_P (op0)
	  || (REFERENCE_CLASS_P (op0) && get_base_address (op0))))
    {
      ref.ref = op0;
      ref.is_read = false;
      ref.is_conditional_in_stmt = false;
      references->safe_push (ref);
    }
  return clobbers_memory;
}


/* Returns true if the loop-nest has any data reference.  */

bool
loop_nest_has_data_refs (loop_p loop)
{
  basic_block *bbs = get_loop_body (loop);
  auto_vec<data_ref_loc, 3> references;

  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator bsi;

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  get_references_in_stmt (stmt, &references);
	  if (references.length ())
	    {
	      free (bbs);
	      return true;
	    }
	}
    }
  free (bbs);
  return false;
}

/* Stores the data references in STMT to DATAREFS.  If there is an unanalyzable
   reference, returns false, otherwise returns true.  NEST is the outermost
   loop of the loop nest in which the references should be analyzed.  */

opt_result
find_data_references_in_stmt (class loop *nest, gimple *stmt,
			      vec<data_reference_p> *datarefs)
{
  auto_vec<data_ref_loc, 2> references;
  data_reference_p dr;

  if (get_references_in_stmt (stmt, &references))
    return opt_result::failure_at (stmt, "statement clobbers memory: %G",
				   stmt);

  for (const data_ref_loc &ref : references)
    {
      dr = create_data_ref (nest ? loop_preheader_edge (nest) : NULL,
			    loop_containing_stmt (stmt), ref.ref,
			    stmt, ref.is_read, ref.is_conditional_in_stmt);
      gcc_assert (dr != NULL);
      datarefs->safe_push (dr);
    }

  return opt_result::success ();
}

/* Stores the data references in STMT to DATAREFS.  If there is an
   unanalyzable reference, returns false, otherwise returns true.
   NEST is the outermost loop of the loop nest in which the references
   should be instantiated, LOOP is the loop in which the references
   should be analyzed.  */

bool
graphite_find_data_references_in_stmt (edge nest, loop_p loop, gimple *stmt,
				       vec<data_reference_p> *datarefs)
{
  auto_vec<data_ref_loc, 2> references;
  bool ret = true;
  data_reference_p dr;

  if (get_references_in_stmt (stmt, &references))
    return false;

  for (const data_ref_loc &ref : references)
    {
      dr = create_data_ref (nest, loop, ref.ref, stmt, ref.is_read,
			    ref.is_conditional_in_stmt);
      gcc_assert (dr != NULL);
      datarefs->safe_push (dr);
    }

  return ret;
}

/* Search the data references in LOOP, and record the information into
   DATAREFS.  Returns chrec_dont_know when failing to analyze a
   difficult case, returns NULL_TREE otherwise.  */

tree
find_data_references_in_bb (class loop *loop, basic_block bb,
                            vec<data_reference_p> *datarefs)
{
  gimple_stmt_iterator bsi;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);

      if (!find_data_references_in_stmt (loop, stmt, datarefs))
        {
          struct data_reference *res;
          res = XCNEW (struct data_reference);
          datarefs->safe_push (res);

          return chrec_dont_know;
        }
    }

  return NULL_TREE;
}

/* Search the data references in LOOP, and record the information into
   DATAREFS.  Returns chrec_dont_know when failing to analyze a
   difficult case, returns NULL_TREE otherwise.

   TODO: This function should be made smarter so that it can handle address
   arithmetic as if they were array accesses, etc.  */

tree
find_data_references_in_loop (class loop *loop,
			      vec<data_reference_p> *datarefs)
{
  basic_block bb, *bbs;
  unsigned int i;

  bbs = get_loop_body_in_dom_order (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      if (find_data_references_in_bb (loop, bb, datarefs) == chrec_dont_know)
        {
          free (bbs);
          return chrec_dont_know;
        }
    }
  free (bbs);

  return NULL_TREE;
}

/* Return the alignment in bytes that DRB is guaranteed to have at all
   times.  */

unsigned int
dr_alignment (innermost_loop_behavior *drb)
{
  /* Get the alignment of BASE_ADDRESS + INIT.  */
  unsigned int alignment = drb->base_alignment;
  unsigned int misalignment = (drb->base_misalignment
			       + TREE_INT_CST_LOW (drb->init));
  if (misalignment != 0)
    alignment = MIN (alignment, misalignment & -misalignment);

  /* Cap it to the alignment of OFFSET.  */
  if (!integer_zerop (drb->offset))
    alignment = MIN (alignment, drb->offset_alignment);

  /* Cap it to the alignment of STEP.  */
  if (!integer_zerop (drb->step))
    alignment = MIN (alignment, drb->step_alignment);

  return alignment;
}

/* If BASE is a pointer-typed SSA name, try to find the object that it
   is based on.  Return this object X on success and store the alignment
   in bytes of BASE - &X in *ALIGNMENT_OUT.  */

static tree
get_base_for_alignment_1 (tree base, unsigned int *alignment_out)
{
  if (TREE_CODE (base) != SSA_NAME || !POINTER_TYPE_P (TREE_TYPE (base)))
    return NULL_TREE;

  gimple *def = SSA_NAME_DEF_STMT (base);
  base = analyze_scalar_evolution (loop_containing_stmt (def), base);

  /* Peel chrecs and record the minimum alignment preserved by
     all steps.  */
  unsigned int alignment = MAX_OFILE_ALIGNMENT / BITS_PER_UNIT;
  while (TREE_CODE (base) == POLYNOMIAL_CHREC)
    {
      unsigned int step_alignment = highest_pow2_factor (CHREC_RIGHT (base));
      alignment = MIN (alignment, step_alignment);
      base = CHREC_LEFT (base);
    }

  /* Punt if the expression is too complicated to handle.  */
  if (tree_contains_chrecs (base, NULL) || !POINTER_TYPE_P (TREE_TYPE (base)))
    return NULL_TREE;

  /* The only useful cases are those for which a dereference folds to something
     other than an INDIRECT_REF.  */
  tree ref_type = TREE_TYPE (TREE_TYPE (base));
  tree ref = fold_indirect_ref_1 (UNKNOWN_LOCATION, ref_type, base);
  if (!ref)
    return NULL_TREE;

  /* Analyze the base to which the steps we peeled were applied.  */
  poly_int64 bitsize, bitpos, bytepos;
  machine_mode mode;
  int unsignedp, reversep, volatilep;
  tree offset;
  base = get_inner_reference (ref, &bitsize, &bitpos, &offset, &mode,
			      &unsignedp, &reversep, &volatilep);
  if (!base || !multiple_p (bitpos, BITS_PER_UNIT, &bytepos))
    return NULL_TREE;

  /* Restrict the alignment to that guaranteed by the offsets.  */
  unsigned int bytepos_alignment = known_alignment (bytepos);
  if (bytepos_alignment != 0)
    alignment = MIN (alignment, bytepos_alignment);
  if (offset)
    {
      unsigned int offset_alignment = highest_pow2_factor (offset);
      alignment = MIN (alignment, offset_alignment);
    }

  *alignment_out = alignment;
  return base;
}

/* Return the object whose alignment would need to be changed in order
   to increase the alignment of ADDR.  Store the maximum achievable
   alignment in *MAX_ALIGNMENT.  */

tree
get_base_for_alignment (tree addr, unsigned int *max_alignment)
{
  tree base = get_base_for_alignment_1 (addr, max_alignment);
  if (base)
    return base;

  if (TREE_CODE (addr) == ADDR_EXPR)
    addr = TREE_OPERAND (addr, 0);
  *max_alignment = MAX_OFILE_ALIGNMENT / BITS_PER_UNIT;
  return addr;
}

/* Recursive helper function.  */

static bool
find_loop_nest_1 (class loop *loop, vec<loop_p> *loop_nest)
{
  /* Inner loops of the nest should not contain siblings.  Example:
     when there are two consecutive loops,

     | loop_0
     |   loop_1
     |     A[{0, +, 1}_1]
     |   endloop_1
     |   loop_2
     |     A[{0, +, 1}_2]
     |   endloop_2
     | endloop_0

     the dependence relation cannot be captured by the distance
     abstraction.  */
  if (loop->next)
    return false;

  loop_nest->safe_push (loop);
  if (loop->inner)
    return find_loop_nest_1 (loop->inner, loop_nest);
  return true;
}

/* Return false when the LOOP is not well nested.  Otherwise return
   true and insert in LOOP_NEST the loops of the nest.  LOOP_NEST will
   contain the loops from the outermost to the innermost, as they will
   appear in the classic distance vector.  */

bool
find_loop_nest (class loop *loop, vec<loop_p> *loop_nest)
{
  loop_nest->safe_push (loop);
  if (loop->inner)
    return find_loop_nest_1 (loop->inner, loop_nest);
  return true;
}

/* Returns true when the data dependences have been computed, false otherwise.
   Given a loop nest LOOP, the following vectors are returned:
   DATAREFS is initialized to all the array elements contained in this loop,
   DEPENDENCE_RELATIONS contains the relations between the data references.
   Compute read-read and self relations if
   COMPUTE_SELF_AND_READ_READ_DEPENDENCES is TRUE.  */

bool
compute_data_dependences_for_loop (class loop *loop,
				   bool compute_self_and_read_read_dependences,
				   vec<loop_p> *loop_nest,
				   vec<data_reference_p> *datarefs,
				   vec<ddr_p> *dependence_relations)
{
  bool res = true;

  memset (&dependence_stats, 0, sizeof (dependence_stats));

  /* If the loop nest is not well formed, or one of the data references
     is not computable, give up without spending time to compute other
     dependences.  */
  if (!loop
      || !find_loop_nest (loop, loop_nest)
      || find_data_references_in_loop (loop, datarefs) == chrec_dont_know
      || !compute_all_dependences (*datarefs, dependence_relations, *loop_nest,
				   compute_self_and_read_read_dependences))
    res = false;

  if (dump_file && (dump_flags & TDF_STATS))
    {
      fprintf (dump_file, "Dependence tester statistics:\n");

      fprintf (dump_file, "Number of dependence tests: %d\n",
	       dependence_stats.num_dependence_tests);
      fprintf (dump_file, "Number of dependence tests classified dependent: %d\n",
	       dependence_stats.num_dependence_dependent);
      fprintf (dump_file, "Number of dependence tests classified independent: %d\n",
	       dependence_stats.num_dependence_independent);
      fprintf (dump_file, "Number of undetermined dependence tests: %d\n",
	       dependence_stats.num_dependence_undetermined);

      fprintf (dump_file, "Number of subscript tests: %d\n",
	       dependence_stats.num_subscript_tests);
      fprintf (dump_file, "Number of undetermined subscript tests: %d\n",
	       dependence_stats.num_subscript_undetermined);
      fprintf (dump_file, "Number of same subscript function: %d\n",
	       dependence_stats.num_same_subscript_function);

      fprintf (dump_file, "Number of ziv tests: %d\n",
	       dependence_stats.num_ziv);
      fprintf (dump_file, "Number of ziv tests returning dependent: %d\n",
	       dependence_stats.num_ziv_dependent);
      fprintf (dump_file, "Number of ziv tests returning independent: %d\n",
	       dependence_stats.num_ziv_independent);
      fprintf (dump_file, "Number of ziv tests unimplemented: %d\n",
	       dependence_stats.num_ziv_unimplemented);

      fprintf (dump_file, "Number of siv tests: %d\n",
	       dependence_stats.num_siv);
      fprintf (dump_file, "Number of siv tests returning dependent: %d\n",
	       dependence_stats.num_siv_dependent);
      fprintf (dump_file, "Number of siv tests returning independent: %d\n",
	       dependence_stats.num_siv_independent);
      fprintf (dump_file, "Number of siv tests unimplemented: %d\n",
	       dependence_stats.num_siv_unimplemented);

      fprintf (dump_file, "Number of miv tests: %d\n",
	       dependence_stats.num_miv);
      fprintf (dump_file, "Number of miv tests returning dependent: %d\n",
	       dependence_stats.num_miv_dependent);
      fprintf (dump_file, "Number of miv tests returning independent: %d\n",
	       dependence_stats.num_miv_independent);
      fprintf (dump_file, "Number of miv tests unimplemented: %d\n",
	       dependence_stats.num_miv_unimplemented);
    }

  return res;
}

/* Free the memory used by a data dependence relation DDR.  */

void
free_dependence_relation (struct data_dependence_relation *ddr)
{
  if (ddr == NULL)
    return;

  if (DDR_SUBSCRIPTS (ddr).exists ())
    free_subscripts (DDR_SUBSCRIPTS (ddr));
  DDR_DIST_VECTS (ddr).release ();
  DDR_DIR_VECTS (ddr).release ();

  free (ddr);
}

/* Free the memory used by the data dependence relations from
   DEPENDENCE_RELATIONS.  */

void
free_dependence_relations (vec<ddr_p>& dependence_relations)
{
  for (data_dependence_relation *ddr : dependence_relations)
    if (ddr)
      free_dependence_relation (ddr);

  dependence_relations.release ();
}

/* Free the memory used by the data references from DATAREFS.  */

void
free_data_refs (vec<data_reference_p>& datarefs)
{
  for (data_reference *dr : datarefs)
    free_data_ref (dr);
  datarefs.release ();
}

/* Common routine implementing both dr_direction_indicator and
   dr_zero_step_indicator.  Return USEFUL_MIN if the indicator is known
   to be >= USEFUL_MIN and -1 if the indicator is known to be negative.
   Return the step as the indicator otherwise.  */

static tree
dr_step_indicator (struct data_reference *dr, int useful_min)
{
  tree step = DR_STEP (dr);
  if (!step)
    return NULL_TREE;
  STRIP_NOPS (step);
  /* Look for cases where the step is scaled by a positive constant
     integer, which will often be the access size.  If the multiplication
     doesn't change the sign (due to overflow effects) then we can
     test the unscaled value instead.  */
  if (TREE_CODE (step) == MULT_EXPR
      && TREE_CODE (TREE_OPERAND (step, 1)) == INTEGER_CST
      && tree_int_cst_sgn (TREE_OPERAND (step, 1)) > 0)
    {
      tree factor = TREE_OPERAND (step, 1);
      step = TREE_OPERAND (step, 0);

      /* Strip widening and truncating conversions as well as nops.  */
      if (CONVERT_EXPR_P (step)
	  && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (step, 0))))
	step = TREE_OPERAND (step, 0);
      tree type = TREE_TYPE (step);

      /* Get the range of step values that would not cause overflow.  */
      widest_int minv = (wi::to_widest (TYPE_MIN_VALUE (ssizetype))
			 / wi::to_widest (factor));
      widest_int maxv = (wi::to_widest (TYPE_MAX_VALUE (ssizetype))
			 / wi::to_widest (factor));

      /* Get the range of values that the unconverted step actually has.  */
      wide_int step_min, step_max;
      value_range vr;
      if (TREE_CODE (step) != SSA_NAME
	  || !get_range_query (cfun)->range_of_expr (vr, step)
	  || vr.kind () != VR_RANGE)
	{
	  step_min = wi::to_wide (TYPE_MIN_VALUE (type));
	  step_max = wi::to_wide (TYPE_MAX_VALUE (type));
	}
      else
	{
	  step_min = vr.lower_bound ();
	  step_max = vr.upper_bound ();
	}

      /* Check whether the unconverted step has an acceptable range.  */
      signop sgn = TYPE_SIGN (type);
      if (wi::les_p (minv, widest_int::from (step_min, sgn))
	  && wi::ges_p (maxv, widest_int::from (step_max, sgn)))
	{
	  if (wi::ge_p (step_min, useful_min, sgn))
	    return ssize_int (useful_min);
	  else if (wi::lt_p (step_max, 0, sgn))
	    return ssize_int (-1);
	  else
	    return fold_convert (ssizetype, step);
	}
    }
  return DR_STEP (dr);
}

/* Return a value that is negative iff DR has a negative step.  */

tree
dr_direction_indicator (struct data_reference *dr)
{
  return dr_step_indicator (dr, 0);
}

/* Return a value that is zero iff DR has a zero step.  */

tree
dr_zero_step_indicator (struct data_reference *dr)
{
  return dr_step_indicator (dr, 1);
}

/* Return true if DR is known to have a nonnegative (but possibly zero)
   step.  */

bool
dr_known_forward_stride_p (struct data_reference *dr)
{
  tree indicator = dr_direction_indicator (dr);
  tree neg_step_val = fold_binary (LT_EXPR, boolean_type_node,
				   fold_convert (ssizetype, indicator),
				   ssize_int (0));
  return neg_step_val && integer_zerop (neg_step_val);
}
