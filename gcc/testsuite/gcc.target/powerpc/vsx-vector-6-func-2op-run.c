/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define DEBUG 0

/* Functional test of the two operand vector builtins.  */
#include "vsx-vector-6-func-2op.h"

/* Macros to check the results of the builtin tests.  */
#define FLOAT_CHECK(NAME)						  \
  f_result = float_##NAME(f_src_a, f_src_b);		                  \
                                                                          \
  if ((f_result[0] != f_##NAME##_expected[0])				  \
      || (f_result[1] != f_##NAME##_expected[1])			  \
      || (f_result[2] != f_##NAME##_expected[2])			  \
      || (f_result[3] != f_##NAME##_expected[3]))			  \
    {									  \
      if (DEBUG)							  \
	{								  \
	  printf("ERROR: vec_%s (float) expected value does not match\n", \
		 #NAME);						  \
	  printf("   expected[0] = %f; result[0] = %f\n",		  \
		 f_##NAME##_expected[0], f_result[0]);			  \
	  printf("   expected[1] = %f; result[1] = %f\n",		  \
		 f_##NAME##_expected[1], f_result[1]);			  \
	  printf("   expected[2] = %f; result[2] = %f\n",		  \
		 f_##NAME##_expected[2], f_result[2]);			  \
	  printf("   expected[3] = %f; result[3] = %f\n",		  \
		 f_##NAME##_expected[3], f_result[3]);			  \
	}								  \
      else								  \
	abort();							  \
  }

#define DOUBLE_CHECK(NAME)						  \
  d_result = vec_##NAME (d_src_a, d_src_b);				  \
                                                                          \
  if ((d_result[0] != d_##NAME##_expected[0])				  \
      || (d_result[1] != d_##NAME##_expected[1]))			  \
    {									  \
      if (DEBUG)							  \
	{								  \
	  printf("ERROR: vec_%s(double) expected value does not match\n", \
		 #NAME);						  \
	  printf("   expected[0] = %f; result[0] = %f\n",		  \
		 d_##NAME##_expected[0], d_result[0]);			  \
	  printf("   expected[1] = %f; result[1] = %f\n",		  \
		 d_##NAME##_expected[1], d_result[1]);			  \
	}								  \
      else								  \
	abort();							  \
  }

void abort (void);

int
main () {
  int i;
  vector float f_src_a = { 126.0, 23.0, -338.0, 17.0};
  vector float f_src_b = { 2.00, -4.0, 1.0, 4.0};
  vector float f_result;
  vector float f_add_expected = { 128.0, 19.0, -337.0, 21.0};
  vector float f_div_expected = { 63.0, -5.75, -338, 4.25};
  vector float f_max_expected = { 126.0, 23.0, 1.0, 17.0};
  vector float f_min_expected = { 2.0, -4.0, -338.0, 4.0};
  vector float f_mul_expected = { 252, -92.0, -338, 68.0};
  vector float f_sub_expected = { 124.0, 27.0, -339.0, 13.0};

  vector double d_src_a = { 125.44, -338.56};
  vector double d_src_b = { 4.0, -2.0};
  vector double d_result;
  vector double d_add_expected = { 129.44, -340.56};
  vector double d_div_expected = { 31.360000, 169.280000};
  vector double d_max_expected = { 125.44, -2.0};
  vector double d_min_expected = { 4.0, -338.56};
  vector double d_mul_expected = { 501.760000, 677.120000};
  vector double d_sub_expected = { 121.440000, -336.560000};

  /* Run tests.  */
  FLOAT_CHECK (add)
  FLOAT_CHECK (div)
  FLOAT_CHECK (max)
  FLOAT_CHECK (min)
  FLOAT_CHECK (mul)
  FLOAT_CHECK (sub)
  
  DOUBLE_CHECK (add)
  DOUBLE_CHECK (div)
  DOUBLE_CHECK (max)
  DOUBLE_CHECK (min)
  DOUBLE_CHECK (mul)
  DOUBLE_CHECK (sub)

  return 0;
}
