/* The goal is to have both compile tests which verify the desired instruction
   generation and to functionally test the builtins for correctness.  This is 
   done in separate test files, vsx-vector-6-func-cmp-all.c and
   vsx-vector-6-func-cmp-all-run.c.  The vsx-vector-6-func-cmp-all.c test file
   only generates the calls so the instruction counts do not include the counts
   of the instructions generated as part of the result testing.  The result
   checking code differs for BE/LE.  */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void abort (void);

#define FLOAT_1ARG_TEST(NAME)                                         \
  bool __attribute__ ((noipa))                                        \
  float_1arg_##NAME (vector float f_src, bool f_##NAME##_expected)    \
  {                                                                   \
    return vec_##NAME (f_src); 		                              \
  }

FLOAT_1ARG_TEST (all_nan)
FLOAT_1ARG_TEST (all_numeric)
FLOAT_1ARG_TEST (any_nan)
FLOAT_1ARG_TEST (any_numeric)

#define FLOAT_2ARG_TEST(NAME)					      \
  bool __attribute__ ((noipa))                                        \
  float_2arg_##NAME (vector float f_src_a, vector float f_src_b,      \
		     bool f_##NAME##_expected)			      \
  {                                                                   \
    return vec_##NAME (f_src_a, f_src_b);			      \
  }
		 
FLOAT_2ARG_TEST (all_eq)
FLOAT_2ARG_TEST (all_gt)
FLOAT_2ARG_TEST (all_ge)
FLOAT_2ARG_TEST (all_lt)
FLOAT_2ARG_TEST (all_le)
FLOAT_2ARG_TEST (any_eq)
FLOAT_2ARG_TEST (any_gt)
FLOAT_2ARG_TEST (any_ge)
FLOAT_2ARG_TEST (any_lt)
FLOAT_2ARG_TEST (any_le)

#define DOUBLE_1ARG_TEST(NAME )						      \
  bool __attribute__ ((noipa))                                                \
  double_1arg_##NAME (vector double d_src, bool d_##NAME##_expected)	      \
  {                                                                           \
    return  vec_##NAME (d_src);						      \
  }

DOUBLE_1ARG_TEST (all_nan)
DOUBLE_1ARG_TEST (all_numeric)
DOUBLE_1ARG_TEST (any_nan)
DOUBLE_1ARG_TEST (any_numeric)

#define DOUBLE_2ARG_TEST(NAME)						      \
  bool __attribute__ ((noipa))                                                \
  double_2arg_##NAME (vector double d_src_a, vector double d_src_b,           \
		      bool d_##NAME##_expected)	                              \
  {                                                                           \
    return vec_##NAME (d_src_a, d_src_b);				      \
  }

DOUBLE_2ARG_TEST (all_eq)
DOUBLE_2ARG_TEST (all_gt)
DOUBLE_2ARG_TEST (all_ge)
DOUBLE_2ARG_TEST (all_lt)
DOUBLE_2ARG_TEST (all_le)
DOUBLE_2ARG_TEST (any_eq)
DOUBLE_2ARG_TEST (any_gt)
DOUBLE_2ARG_TEST (any_ge)
DOUBLE_2ARG_TEST (any_lt)
DOUBLE_2ARG_TEST (any_le)
