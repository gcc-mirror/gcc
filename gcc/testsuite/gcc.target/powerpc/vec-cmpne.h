#include "altivec.h"

#define N 4096

void abort ();

#define PRAGMA(X) _Pragma (#X)
#define UNROLL0 PRAGMA (GCC unroll 0)

#define define_test_functions(VBTYPE, RTYPE, STYPE, NAME)	\
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
  UNROLL0 \
  for (i = 0; i < N; i+=16/sizeof (STYPE))	\
    { \
      /* result_ne = operand1!=operand2.  */ \
      v1_##NAME = vec_vsx_ld (0, (const vector STYPE*)&operand1_##NAME[i]); \
      v2_##NAME = vec_vsx_ld (0, (const vector STYPE*)&operand2_##NAME[i]); \
\
      tmp_##NAME = vec_cmpeq (v1_##NAME, v2_##NAME); \
      vec_vsx_st (tmp_##NAME, 0, &result_eq_##NAME[i]); \
\
      tmp_##NAME = vec_cmpne (v1_##NAME, v2_##NAME); \
      vec_vsx_st (tmp_##NAME, 0, &result_ne_##NAME[i]); \
    } \
} \

#define define_init_verify_functions(VBTYPE, RTYPE, STYPE, NAME) \
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

