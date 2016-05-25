/* { dg-do run { target { powerpc64*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -mpower8-vector -O3" } */

/* Test that the vec_cmpne builtin works as expected for long long
   and double vectors.  */

#include "altivec.h"

#define N 4096

void abort ();

#define define_test_functions(VBTYPE, RTYPE, STYPE, NAME) \
\
RTYPE result_ne_##NAME[N] __attribute__((aligned(16))); \
RTYPE result_eq_##NAME[N] __attribute__((aligned(16))); \
STYPE operand1_##NAME[N] __attribute__((aligned(16))); \
STYPE operand2_##NAME[N] __attribute__((aligned(16))); \
RTYPE expected_##NAME[N] __attribute__((aligned(16))); \
\
__attribute__((noinline)) void vector_tests_##NAME () \
{ \
  vector STYPE v1_##NAME, v2_##NAME; \
  vector bool VBTYPE tmp_##NAME; \
  int i; \
  for (i = 0; i < N; i+=16/sizeof (STYPE)) \
    { \
      /* result_ne = operand1!=operand2.  */ \
      v1_##NAME = (vector STYPE) { operand1_##NAME[i], \
					 operand1_##NAME[i+1] }; \
      v2_##NAME = (vector STYPE) { operand2_##NAME[i], \
					 operand2_##NAME[i+1] }; \
\
      tmp_##NAME = vec_cmpeq (v1_##NAME, v2_##NAME); \
      result_eq_##NAME[i] = tmp_##NAME[0]; \
      result_eq_##NAME[i+1] = tmp_##NAME[1]; \
\
      tmp_##NAME = vec_cmpne (v1_##NAME, v2_##NAME); \
      result_ne_##NAME[i] = tmp_##NAME[0]; \
      result_ne_##NAME[i+1] = tmp_##NAME[1]; \
    } \
} \
\
__attribute__((noinline)) void init_##NAME () \
{ \
  int i; \
  for (i = 0; i < N; ++i) \
    { \
      result_ne_##NAME[i] = 7; \
      result_eq_##NAME[i] = 15; \
      if (i%3 == 0) \
	{ \
	  /* op1 < op2.  */ \
	  operand1_##NAME[i] = 1; \
	  operand2_##NAME[i] = 2; \
	} \
      else if (i%3 == 1) \
	{ \
	  /* op1 > op2.  */ \
	  operand1_##NAME[i] = 2; \
	  operand2_##NAME[i] = 1; \
	} \
      else if (i%3 == 2) \
	{ \
	  /* op1 == op2.  */ \
	  operand1_##NAME[i] = 3; \
	  operand2_##NAME[i] = 3; \
	} \
      /* For vector comparisons: "For each element of the result_ne, the \
	  value of each bit is 1 if the corresponding elements of ARG1 and \
	  ARG2 are equal." {or whatever the comparison is} "Otherwise, the \
	  value of each bit is 0."  */ \
    expected_##NAME[i] = -1 * (RTYPE)(operand1_##NAME[i] != operand2_##NAME[i]); \
  } \
} \
\
__attribute__((noinline)) void verify_results_##NAME () \
{ \
  int i; \
  for (i = 0; i < N; ++i) \
    { \
      if ( ((result_ne_##NAME[i] != expected_##NAME[i]) || \
	    (result_ne_##NAME[i] == result_eq_##NAME[i]))) \
	abort (); \
    } \
}


#define execute_test_functions(VBTYPE, RTYPE, STYPE, NAME) \
{ \
  init_##NAME (); \
  vector_tests_##NAME (); \
  verify_results_##NAME (); \
}


define_test_functions (long long, signed long long, signed long long, si);
define_test_functions (long long, signed long long, double, dd);

int main ()
{
  execute_test_functions (long long, signed long long, signed long long, si);
  execute_test_functions (long long, signed long long, double, dd);

  return 0;
}


