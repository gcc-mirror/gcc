/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define DEBUG 0

/* This file just generates calls to the various builtins and verifies the
   expected number of instructions for each builtin were generated.  */

#include "vsx-vector-6-func-cmp.h"

/* Macros to check the results of the builtin tests.  */
#define FLOAT_CHECK(NAME)						  \
  f_result = vec_##NAME (f_src_a, f_src_b);				  \
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
	  printf("   expected[0] = 0x%x; result[0] =0x%x\n",		  \
		 f_##NAME##_expected[0], f_result[0]);			  \
	  printf("   expected[1] = 0x%x; result[1] = 0x%x\n",		  \
		 f_##NAME##_expected[1], f_result[1]);			  \
	  printf("   expected[2] = 0x%x; result[2] = 0x%x\n",		  \
		 f_##NAME##_expected[2], f_result[2]);			  \
	  printf("   expected[3] = 0x%x; result[3] = 0x%x\n",		  \
		 f_##NAME##_expected[3], f_result[3]);			  \
	}								  \
      else								  \
	abort();							  \
    }

#define DOUBLE_CHECK(NAME)						   \
  d_result = vec_##NAME (d_src_a, d_src_b);				   \
									   \
  if ((d_result[0] != d_##NAME##_expected[0])				   \
      || (d_result[1] != d_##NAME##_expected[1]))			   \
    {									   \
      if (DEBUG)							   \
	{								   \
	  printf("ERROR: vec_%s (double) expected value does not match\n", \
		 #NAME);						   \
	  printf("   expected[0] = 0x%lx; result[0] = 0x%lx\n",		   \
		 d_##NAME##_expected[0], d_result[0]);			   \
	  printf("   expected[1] = 0x%lx; result[1] = 0x%lx\n",		   \
		 d_##NAME##_expected[1], d_result[1]);			   \
	}								   \
      else								   \
	abort();							   \
    }

int
main () {
  int i;
  vector float f_src_a = { 126.0, 23.0, -338.0, 17.0};
  vector float f_src_b = { 2.00, 23.0, 1.0, 4.0};
  vector bool f_result;
  vector bool int f_cmpeq_expected = {0x0, 0xFFFFFFFF, 0x0, 0x0};
  vector bool int f_cmpgt_expected = {0xFFFFFFFF, 0x0, 0x0, 0xFFFFFFFF};
  vector bool int f_cmpge_expected = {0xFFFFFFFF, 0xFFFFFFFF, 0x0, 0xFFFFFFFF};
  vector bool int f_cmplt_expected = {0x0, 0x0, 0xFFFFFFFF, 0x0}; 
  vector bool int f_cmple_expected = {0x0, 0xFFFFFFFF, 0xFFFFFFFF, 0x0}; 

  vector double d_src_a = { 125.44, -338.56};
  vector double d_src_b = { 4.0, -338.56};
  vector bool long long d_result;
  vector bool long long d_cmpeq_expected = {0x0, 0xFFFFFFFFFFFFFFFF};
  vector bool long long d_cmpgt_expected = {0xFFFFFFFFFFFFFFFF, 0x0};
  vector bool long long d_cmpge_expected = {0xFFFFFFFFFFFFFFFF,
					    0xFFFFFFFFFFFFFFFF};
  vector bool long long d_cmplt_expected = {0x0, 0x0};
  vector bool long long d_cmple_expected = {0x0, 0xFFFFFFFFFFFFFFFF};

  FLOAT_CHECK (cmpeq)
  FLOAT_CHECK (cmpgt)
  FLOAT_CHECK (cmpge)
  FLOAT_CHECK (cmplt)
  FLOAT_CHECK (cmple)
 
  DOUBLE_CHECK (cmpeq)
  DOUBLE_CHECK (cmpgt)
  DOUBLE_CHECK (cmpge)
  DOUBLE_CHECK (cmplt)
  DOUBLE_CHECK (cmple)

  return 0;
}
