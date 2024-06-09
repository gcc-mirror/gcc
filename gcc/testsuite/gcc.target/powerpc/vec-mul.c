/* { dg-do compile { target { { ! vsx_hw } && powerpc_vsx } } } */
/* { dg-do run { target vsx_hw } } */
/* { dg-options "-mvsx -O3" } */

/* Test that the vec_mul builtin works as expected.  */

#include "altivec.h"

#define N 4096

void abort ();

#define define_test_functions(STYPE, NAMESUFFIX) \
\
STYPE result_##NAMESUFFIX[N]; \
STYPE operand1_##NAMESUFFIX[N]; \
STYPE operand2_##NAMESUFFIX[N]; \
STYPE expected_##NAMESUFFIX[N]; \
\
__attribute__((noinline)) void vector_tests_##NAMESUFFIX () \
{ \
  int i; \
  vector STYPE v1, v2, tmp; \
  for (i = 0; i < N; i+=16/sizeof (STYPE)) \
    { \
      /* result=operand1*operand2.  */ \
      v1 = vec_vsx_ld (0, &operand1_##NAMESUFFIX[i]); \
      v2 = vec_vsx_ld (0, &operand2_##NAMESUFFIX[i]); \
\
      tmp = vec_mul (v1, v2); \
      vec_vsx_st (tmp, 0, &result_##NAMESUFFIX[i]); \
    } \
} \
\
__attribute__((noinline)) void init_##NAMESUFFIX () \
{ \
  int i; \
  for (i = 0; i < N; ++i) \
    { \
      result_##NAMESUFFIX[i] = 0; \
      operand1_##NAMESUFFIX[i] = (i+1) % 31; \
      operand2_##NAMESUFFIX[i] = (i*2) % 15; \
      expected_##NAMESUFFIX[i] = operand1_##NAMESUFFIX[i] * \
				 operand2_##NAMESUFFIX[i]; \
    } \
} \
\
__attribute__((noinline)) void verify_results_##NAMESUFFIX () \
{ \
  int i; \
  for (i = 0; i < N; ++i) \
    { \
      if (result_##NAMESUFFIX[i] != expected_##NAMESUFFIX[i]) \
	abort (); \
    } \
}


#define execute_test_functions(STYPE, NAMESUFFIX) \
{ \
  init_##NAMESUFFIX (); \
  vector_tests_##NAMESUFFIX (); \
  verify_results_##NAMESUFFIX (); \
}


define_test_functions (signed int, si);
define_test_functions (unsigned int, ui);
define_test_functions (signed short, ss);
define_test_functions (unsigned short, us);
define_test_functions (signed char, sc);
define_test_functions (unsigned char, uc);
define_test_functions (float, f);

int main ()
{
  execute_test_functions (signed int, si);
  execute_test_functions (unsigned int, ui);
  execute_test_functions (signed short, ss);
  execute_test_functions (unsigned short, us);
  execute_test_functions (signed char, sc);
  execute_test_functions (unsigned char, uc);
  execute_test_functions (float, f);

  return 0;
}
