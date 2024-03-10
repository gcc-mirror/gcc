/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define DEBUG 0

#include "vsx-vector-6-func-cmp-all.h"

#define FLOAT_1ARG_CHECK(NAME)					          \
  f_result = vec_##NAME (f_src); 		                          \
  								          \
  if (f_result != f_##NAME##_expected)					  \
    {									  \
      if (DEBUG)							  \
	{								  \
	  printf("ERROR: vec_%s (float) expected value does not match\n", \
		 #NAME);						  \
	  printf("   expected = %d; result = %d\n",			  \
		 f_##NAME##_expected, f_result);			  \
	}								  \
      else								  \
	abort();							  \
      }

#define FLOAT_2ARG_CHECK(NAME)						\
  f_result = vec_##NAME (f_src_a, f_src_b);			\
  									\
  if (f_result != f_##NAME##_expected)					\
    {									\
      if (DEBUG)							\
	{								\
	  printf("ERROR: vec_%s (float, float) expected value does not match\n", \
		 #NAME);						\
	  printf("   expected = %d; result = %d\n",			\
		 f_##NAME##_expected, f_result);			\
	}								\
      else								\
	abort();							\
  }
		 
#define DOUBLE_1ARG_CHECK(NAME )					   \
  d_result = vec_##NAME (d_src);                                           \
  								           \
  if (d_result != d_##NAME##_expected)					   \
    {									   \
      if (DEBUG)							   \
	{								   \
	  printf("ERROR: vec_%s (double) expected value does not match\n", \
		 #NAME);						   \
	  printf("   expected = %d; result = %d\n",			   \
		 d_##NAME##_expected, d_result);			   \
	}                                                                  \
      else                                                                 \
	abort();                                                           \
  }

#define DOUBLE_2ARG_CHECK(NAME)					           \
  d_result = vec_##NAME (d_src_a, d_src_b);				   \
  									   \
  if (d_result != d_##NAME##_expected)					   \
    {									   \
      if (DEBUG)							   \
	{								   \
	  printf("ERROR: vec_%s (double, double) expected value does not match\n", \
		 #NAME);						   \
	  printf("   expected = %d; result = %d\n",			   \
		 d_##NAME##_expected, d_result);			   \
	}								   \
      else								   \
	abort();							   \
    }

int
main () {
  vector float f_src = {126.0, 23.0, -338.0, 17.0};
  vector float f_src_a = {126.0, 23.0, -338.0, 17.0};
  vector float f_src_b = {2.00, 23.0, 1.0, 4.0};
  bool f_result;
  bool f_all_eq_expected = 0;
  bool f_all_gt_expected = 0;
  bool f_all_ge_expected = 0;
  bool f_all_lt_expected = 0;
  bool f_all_le_expected = 0;
  bool f_all_nan_expected = 0;
  bool f_all_numeric_expected = 1;
  bool f_any_eq_expected = 1;
  bool f_any_gt_expected = 1;
  bool f_any_ge_expected = 1;
  bool f_any_lt_expected = 1;
  bool f_any_le_expected = 1;
  bool f_any_nan_expected = 0;
  bool f_any_numeric_expected = 1;

  vector double d_src = { 125.44, -338.56};
  vector double d_src_a = { 125.44, -338.56};
  vector double d_src_b = d_src_a;
  bool d_result;
  bool d_all_eq_expected = 1;
  bool d_all_gt_expected = 0;
  bool d_all_ge_expected = 1;
  bool d_all_lt_expected = 0;
  bool d_all_le_expected = 1;
  bool d_all_nan_expected = 0;
  bool d_all_numeric_expected = 1;
  bool d_any_eq_expected = 1;
  bool d_any_gt_expected = 0;
  bool d_any_ge_expected = 1;
  bool d_any_lt_expected = 0;
  bool d_any_le_expected = 1;
  bool d_any_nan_expected = 0;
  bool d_any_numeric_expected = 1;

  /* Run tests.  */
  FLOAT_1ARG_CHECK (all_nan)
  FLOAT_1ARG_CHECK (all_numeric)
  FLOAT_1ARG_CHECK (any_nan)
  FLOAT_1ARG_CHECK (any_numeric)

  FLOAT_2ARG_CHECK (all_eq)
  FLOAT_2ARG_CHECK (all_gt)
  FLOAT_2ARG_CHECK (all_ge)
  FLOAT_2ARG_CHECK (all_lt)
  FLOAT_2ARG_CHECK (all_le)
  FLOAT_2ARG_CHECK (any_eq)
  FLOAT_2ARG_CHECK (any_gt)
  FLOAT_2ARG_CHECK (any_ge)
  FLOAT_2ARG_CHECK (any_lt)
  FLOAT_2ARG_CHECK (any_le)

  DOUBLE_1ARG_CHECK (all_nan)
  DOUBLE_1ARG_CHECK (all_numeric)
  DOUBLE_1ARG_CHECK (any_nan)
  DOUBLE_1ARG_CHECK (any_numeric)

  DOUBLE_2ARG_CHECK (all_eq)
  DOUBLE_2ARG_CHECK (all_gt)
  DOUBLE_2ARG_CHECK (all_ge)
  DOUBLE_2ARG_CHECK (all_lt)
  DOUBLE_2ARG_CHECK (all_le)
  DOUBLE_2ARG_CHECK (any_eq)
  DOUBLE_2ARG_CHECK (any_gt)
  DOUBLE_2ARG_CHECK (any_ge)
  DOUBLE_2ARG_CHECK (any_lt)
  DOUBLE_2ARG_CHECK (any_le)

  return 0;
}
