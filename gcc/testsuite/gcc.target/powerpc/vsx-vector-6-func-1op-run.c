/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define DEBUG 0

/* Functional test of the one operand vector builtins.  */
#include "vsx-vector-6-func-1op.h"

/* Macros to check the results of the builtin tests.  */
#define FLOAT_CHECK(NAME)						\
  f_result = float_##NAME(f_src);			                \
                                                                        \
  if ((f_result[0] != f_##NAME##_expected[0]) ||			\
      (f_result[1] != f_##NAME##_expected[1]) ||			\
      (f_result[2] != f_##NAME##_expected[2]) ||			\
      (f_result[3] != f_##NAME##_expected[3]))				\
    {									\
      if (DEBUG) {							\
	printf("ERROR: vec_%s (float) expected value does not match\n",	\
	       #NAME);							\
	printf("   expected[0] = %f; result[0] = %f\n",			\
	       f_##NAME##_expected[0], f_result[0]);			\
	printf("   expected[1] = %f; result[1] = %f\n",			\
	       f_##NAME##_expected[1], f_result[1]);			\
	printf("   expected[2] = %f; result[2] = %f\n",			\
	       f_##NAME##_expected[2], f_result[2]);			\
	printf("   expected[3] = %f; result[3] = %f\n",			\
	       f_##NAME##_expected[3], f_result[3]);			\
      } else								\
	abort();							\
    }

#define DOUBLE_CHECK(NAME)						   \
  d_result = double_##NAME(d_src);					   \
                                                                           \
  if ((d_result[0] != d_##NAME##_expected[0])				   \
      || (d_result[1] != d_##NAME##_expected[1]))			   \
    {									   \
      if (DEBUG)							   \
	{								   \
	  printf("ERROR: vec_%s (double) expected value does not match\n", \
		 #NAME);						   \
	  printf("   expected[0] = %f; result[0] = %f\n",		   \
		 d_##NAME##_expected[0], d_result[0]);			   \
	  printf("   expected[1] = %f; result[1] = %f\n",		   \
		 d_##NAME##_expected[1], d_result[1]);			   \
	}								   \
      else								   \
	abort();							   \
    }

int
main () {
  vector float f_src = { 125.44, 23.04, -338.56, 17.64};
  vector float f_result;
  vector float f_abs_expected = { 125.44, 23.04, 338.56, 17.64};
  vector float f_ceil_expected = { 126.0, 24.0, -338, 18.0};
  vector float f_floor_expected = { 125.0, 23.0, -339, 17.0};
  vector float f_nearbyint_expected = { 125.0, 23.0, -339, 18.0};
  vector float f_rint_expected = { 125.0, 23.0, -339, 18.0};
  vector float f_sqrt_expected = { 11.2, 4.8, 18.4, 4.2};
  vector float f_trunc_expected = { 125.0, 23.0, -338, 17};

  vector double d_src = { 125.44, -338.56};
  vector double d_src_sqrt = { 125.44, 338.56};
  vector double d_abs_src;
  vector double d_result;
  vector double d_abs_expected = { 125.44, 338.56};
  vector double d_ceil_expected = { 126.0, -338.0};
  vector double d_floor_expected = { 125.0, -339.0};
  vector double d_nearbyint_expected = { 125.0, -339.0};
  vector double d_rint_expected = { 125.0, -339.0};
  vector double d_sqrt_expected = { 11.2, 18.4};
  vector double d_trunc_expected = { 125.0, -338.0};

  /* Run tests.  */
  FLOAT_CHECK (abs)
  FLOAT_CHECK (ceil)
  FLOAT_CHECK (floor)
  FLOAT_CHECK (nearbyint)
  FLOAT_CHECK (rint)
  FLOAT_CHECK (trunc)

  DOUBLE_CHECK (abs)
  DOUBLE_CHECK (ceil)
  DOUBLE_CHECK (floor)
  DOUBLE_CHECK (nearbyint)
  DOUBLE_CHECK (rint)
  DOUBLE_CHECK (trunc)

  /* Need to make sure the arguments for sqrt are always positive.  Do this
     test last as we have to change the input for the test.  */
  d_src = vec_abs (d_src);
  DOUBLE_CHECK (sqrt)

  return 0;
}  
