/* Test for _Atomic in C11.  Test floating-point exceptions for
   compound assignment are consistent with result (so that if multiple
   iterations of the compare-and-exchange loop are needed, exceptions
   get properly cleared).  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors -pthread -U_POSIX_C_SOURCE -D_POSIX_C_SOURCE=200809L" } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-mfp-trap-mode=sui" { target alpha*-*-* } } */
/* { dg-additional-options "-D_XOPEN_SOURCE=600" { target *-*-solaris2.1[0-9]* } } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-require-effective-target pthread } */
/* { dg-timeout-factor 2 } */

#include <fenv.h>
#include <float.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define TEST_ALL_EXCEPT (FE_DIVBYZERO		\
			 | FE_INEXACT		\
			 | FE_INVALID		\
			 | FE_OVERFLOW		\
			 | FE_UNDERFLOW)

#if defined __alpha__ || defined __aarch64__
  #define ITER_COUNT 100
#else
  #define ITER_COUNT 10000
#endif

static volatile _Atomic bool thread_ready, thread_stop;

/* Generate test code (with NAME used to name functions and variables)
   for atomic compound assignments to a variable of type LHSTYPE.  One
   thread repeatedly stores the values INIT1 and INIT2 in a variable,
   while the other repeatedly executes PRE var POST having set
   floating-point exceptions to BEXC.  If the value of the assignment
   operation satisfies VALTEST1 (var), the floating-point exceptions
   should be BEXC | EXC1; otherwise, they should be BEXC | EXC2.  A
   function test_main_##NAME is generated that returns nonzero on
   failure, zero on success.  */

#define TEST_FUNCS(NAME, LHSTYPE, PRE, POST, BEXC,			\
		   INIT1, VALTEST1, EXC1, INIT2, EXC2)			\
									\
static volatile _Atomic LHSTYPE var_##NAME;				\
									\
static void *								\
test_thread_##NAME (void *arg)						\
{									\
  thread_ready = true;							\
  while (!thread_stop)							\
    {									\
      var_##NAME = (INIT1);						\
      var_##NAME = (INIT2);						\
    }									\
  return NULL;								\
}									\
									\
static int								\
test_main_##NAME (void)							\
{									\
  thread_stop = false;							\
  thread_ready = false;							\
  var_##NAME = (INIT1);							\
  pthread_t thread_id;							\
  int pret = pthread_create (&thread_id, NULL, test_thread_##NAME,	\
			     NULL);					\
  if (pret != 0)							\
    {									\
      printf ("pthread_create failed: %d\n", pret);			\
      return 1;								\
    }									\
  int num_1_pass = 0, num_1_fail = 0, num_2_pass = 0, num_2_fail = 0;	\
  while (!thread_ready)							\
    ;									\
  for (int i = 0; i < ITER_COUNT; i++)					\
    {									\
      feclearexcept (FE_ALL_EXCEPT);					\
      feraiseexcept (BEXC);						\
      LHSTYPE r = (PRE var_##NAME POST);				\
      int rexc = fetestexcept (TEST_ALL_EXCEPT);			\
      if (VALTEST1 (r))							\
	{								\
	  if (rexc == ((BEXC) | (EXC1)))				\
	    num_1_pass++;						\
	  else								\
	    num_1_fail++;						\
	  var_##NAME = (INIT2);						\
	}								\
      else								\
	{								\
	  if (rexc == ((BEXC) | (EXC2)))				\
	    num_2_pass++;						\
	  else								\
	    num_2_fail++;						\
	  var_##NAME = (INIT1);						\
	}								\
    }									\
  thread_stop = true;							\
  pthread_join (thread_id, NULL);					\
  printf (#NAME " (a) %d pass, %d fail; (b) %d pass, %d fail\n",	\
	  num_1_pass, num_1_fail, num_2_pass, num_2_fail);		\
  return num_1_fail || num_2_fail;					\
}

TEST_FUNCS (float_add_invalid, float, , += __builtin_inff (), 0,
	    0, __builtin_isinf, 0,
	    -__builtin_inff (), FE_INVALID)
TEST_FUNCS (float_add_invalid_prev, float, , += __builtin_inff (),
	    FE_DIVBYZERO | FE_INEXACT | FE_OVERFLOW | FE_UNDERFLOW,
	    0, __builtin_isinf, 0,
	    -__builtin_inff (), FE_INVALID)
TEST_FUNCS (float_add_overflow, float, , += FLT_MAX, 0,
	    FLT_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_add_overflow_prev, float, , += FLT_MAX, FE_INVALID,
	    FLT_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_add_overflow_double, float, , += (double) FLT_MAX, 0,
	    FLT_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_add_overflow_long_double, float, , += (long double) FLT_MAX, 0,
	    FLT_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#define NOT_FLT_EPSILON_2(X) ((X) != FLT_EPSILON / 2)
TEST_FUNCS (float_add_inexact, float, , += FLT_EPSILON / 2, 0,
	    1.0f, NOT_FLT_EPSILON_2, FE_INEXACT,
	    0, 0)
#define NOT_0(X) ((X) != 0)
TEST_FUNCS (float_add_inexact_int, float, , += 1, 0,
	    FLT_EPSILON / 2, NOT_0, FE_INEXACT,
	    -1, 0)
TEST_FUNCS (float_preinc_inexact, float, ++, , 0,
	    FLT_EPSILON / 2, NOT_0, FE_INEXACT,
	    -1, 0)
#define NOT_MINUS_1(X) ((X) != -1)
TEST_FUNCS (float_postinc_inexact, float, , ++, 0,
	    FLT_EPSILON / 2, NOT_MINUS_1, FE_INEXACT,
	    -1, 0)
#if FLT_EVAL_METHOD == 0
TEST_FUNCS (long_add_float_inexact, long, , += 2 / FLT_EPSILON, 0,
	    1, NOT_0, FE_INEXACT,
	    -2 / FLT_EPSILON, 0)
#endif
#define REAL_ISINF(X) (__builtin_isinf (__real__ (X)))
TEST_FUNCS (complex_float_add_overflow, _Complex float, , += FLT_MAX, 0,
	    FLT_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_sub_invalid, float, , -= __builtin_inff (), 0,
	    0, __builtin_isinf, 0,
	    __builtin_inff (), FE_INVALID)
TEST_FUNCS (float_sub_overflow, float, , -= FLT_MAX, 0,
	    -FLT_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#define NOT_MINUS_FLT_EPSILON_2(X) ((X) != -FLT_EPSILON / 2)
TEST_FUNCS (float_sub_inexact, float, , -= FLT_EPSILON / 2, 0,
	    -1.0f, NOT_MINUS_FLT_EPSILON_2, FE_INEXACT,
	    0, 0)
#define NOT_0(X) ((X) != 0)
TEST_FUNCS (float_sub_inexact_int, float, , -= 1, 0,
	    -FLT_EPSILON / 2, NOT_0, FE_INEXACT,
	    1, 0)
TEST_FUNCS (float_predec_inexact, float, --, , 0,
	    -FLT_EPSILON / 2, NOT_0, FE_INEXACT,
	    1, 0)
#define NOT_1(X) ((X) != 1)
TEST_FUNCS (float_postdec_inexact, float, , --, 0,
	    -FLT_EPSILON / 2, NOT_1, FE_INEXACT,
	    1, 0)
#if FLT_EVAL_METHOD == 0
TEST_FUNCS (long_sub_float_inexact, long, , -= 2 / FLT_EPSILON, 0,
	    -1, NOT_0, FE_INEXACT,
	    2 / FLT_EPSILON, 0)
#endif
TEST_FUNCS (complex_float_sub_overflow, _Complex float, , -= FLT_MAX, 0,
	    -FLT_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_mul_invalid, float, , *= __builtin_inff (), 0,
	    __builtin_inff (), __builtin_isinf, 0,
	    0, FE_INVALID)
TEST_FUNCS (float_mul_overflow, float, , *= FLT_MAX, 0,
	    FLT_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#define IS_0(X) ((X) == 0)
TEST_FUNCS (float_mul_underflow, float, , *= FLT_MIN, 0,
	    FLT_MIN, IS_0, FE_UNDERFLOW | FE_INEXACT,
	    1, 0)
TEST_FUNCS (float_mul_inexact, float, , *= 1 + FLT_EPSILON, 0,
	    1 + FLT_EPSILON, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_mul_inexact_int, float, , *= 3, 0,
	    1 + FLT_EPSILON, NOT_0, FE_INEXACT,
	    0, 0)
#if FLT_EVAL_METHOD == 0
TEST_FUNCS(long_mul_float_inexact, long, , *= 3.0f, 0,
	   1 + 1 / FLT_EPSILON, NOT_0, FE_INEXACT,
	   0, 0)
#endif
TEST_FUNCS (complex_float_mul_overflow, _Complex float, , *= FLT_MAX, 0,
	    FLT_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_div_invalid_divbyzero, float, , /= 0.0f, 0,
	    1, __builtin_isinf, FE_DIVBYZERO,
	    0, FE_INVALID)
TEST_FUNCS (float_div_overflow, float, , /= FLT_MIN, 0,
	    FLT_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_div_underflow, float, , /= FLT_MAX, 0,
	    FLT_MIN, IS_0, FE_UNDERFLOW | FE_INEXACT,
	    FLT_MAX, 0)
TEST_FUNCS (float_div_inexact, float, , /= 3.0f, 0,
	    1, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (float_div_inexact_int, float, , /= 3, 0,
	    1, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (int_div_float_inexact, int, , /= 3.0f, 0,
	    4, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (complex_float_div_overflow, _Complex float, , /= FLT_MIN, 0,
	    FLT_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)

TEST_FUNCS (double_add_invalid, double, , += __builtin_inf (), 0,
	    0, __builtin_isinf, 0,
	    -__builtin_inf (), FE_INVALID)
TEST_FUNCS (double_add_overflow, double, , += DBL_MAX, 0,
	    DBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_add_overflow_long_double, double, , += (long double) DBL_MAX, 0,
	    DBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#define NOT_DBL_EPSILON_2(X) ((X) != DBL_EPSILON / 2)
TEST_FUNCS (double_add_inexact, double, , += DBL_EPSILON / 2, 0,
	    1.0, NOT_DBL_EPSILON_2, FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_add_inexact_int, double, , += 1, 0,
	    DBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    -1, 0)
TEST_FUNCS (double_preinc_inexact, double, ++, , 0,
	    DBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    -1, 0)
TEST_FUNCS (double_postinc_inexact, double, , ++, 0,
	    DBL_EPSILON / 2, NOT_MINUS_1, FE_INEXACT,
	    -1, 0)
#if FLT_EVAL_METHOD == 0
TEST_FUNCS (long_long_add_double_inexact, long long, , += 2 / DBL_EPSILON, 0,
	    1, NOT_0, FE_INEXACT,
	    -2 / DBL_EPSILON, 0)
#endif
TEST_FUNCS (complex_double_add_overflow, _Complex double, , += DBL_MAX, 0,
	    DBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_sub_invalid, double, , -= __builtin_inf (), 0,
	    0, __builtin_isinf, 0,
	    __builtin_inf (), FE_INVALID)
TEST_FUNCS (double_sub_overflow, double, , -= DBL_MAX, 0,
	    -DBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#define NOT_MINUS_DBL_EPSILON_2(X) ((X) != -DBL_EPSILON / 2)
TEST_FUNCS (double_sub_inexact, double, , -= DBL_EPSILON / 2, 0,
	    -1.0, NOT_MINUS_DBL_EPSILON_2, FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_sub_inexact_int, double, , -= 1, 0,
	    -DBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    1, 0)
TEST_FUNCS (double_predec_inexact, double, --, , 0,
	    -DBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    1, 0)
TEST_FUNCS (double_postdec_inexact, double, , --, 0,
	    -DBL_EPSILON / 2, NOT_1, FE_INEXACT,
	    1, 0)
#if FLT_EVAL_METHOD == 0
TEST_FUNCS (long_long_sub_double_inexact, long long, , -= 2 / DBL_EPSILON, 0,
	    -1, NOT_0, FE_INEXACT,
	    2 / DBL_EPSILON, 0)
#endif
TEST_FUNCS (complex_double_sub_overflow, _Complex double, , -= DBL_MAX, 0,
	    -DBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_mul_invalid, double, , *= __builtin_inf (), 0,
	    __builtin_inf (), __builtin_isinf, 0,
	    0, FE_INVALID)
TEST_FUNCS (double_mul_overflow, double, , *= DBL_MAX, 0,
	    DBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_mul_overflow_float, double, , *= FLT_MAX, 0,
	    DBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_mul_underflow, double, , *= DBL_MIN, 0,
	    DBL_MIN, IS_0, FE_UNDERFLOW | FE_INEXACT,
	    1, 0)
TEST_FUNCS (double_mul_inexact, double, , *= 1 + DBL_EPSILON, 0,
	    1 + DBL_EPSILON, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_mul_inexact_int, double, , *= 3, 0,
	    1 + DBL_EPSILON, NOT_0, FE_INEXACT,
	    0, 0)
#if FLT_EVAL_METHOD == 0
TEST_FUNCS(long_long_mul_double_inexact, long long, , *= 3.0, 0,
	   1 + 1 / DBL_EPSILON, NOT_0, FE_INEXACT,
	   0, 0)
#endif
TEST_FUNCS (complex_double_mul_overflow, _Complex double, , *= DBL_MAX, 0,
	    DBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_div_invalid_divbyzero, double, , /= 0.0, 0,
	    1, __builtin_isinf, FE_DIVBYZERO,
	    0, FE_INVALID)
TEST_FUNCS (double_div_overflow, double, , /= DBL_MIN, 0,
	    DBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_div_underflow, double, , /= DBL_MAX, 0,
	    DBL_MIN, IS_0, FE_UNDERFLOW | FE_INEXACT,
	    DBL_MAX, 0)
TEST_FUNCS (double_div_inexact, double, , /= 3.0, 0,
	    1, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (double_div_inexact_int, double, , /= 3, 0,
	    1, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (int_div_double_inexact, int, , /= 3.0, 0,
	    4, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (complex_double_div_overflow, _Complex double, , /= DBL_MIN, 0,
	    DBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)

TEST_FUNCS (long_double_add_invalid, long double, , += __builtin_infl (), 0,
	    0, __builtin_isinf, 0,
	    -__builtin_infl (), FE_INVALID)
#if LDBL_MANT_DIG != 106
TEST_FUNCS (long_double_add_overflow, long double, , += LDBL_MAX, 0,
	    LDBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#define NOT_LDBL_EPSILON_2(X) ((X) != LDBL_EPSILON / 2)
TEST_FUNCS (long_double_add_inexact, long double, , += LDBL_EPSILON / 2, 0,
	    1.0L, NOT_LDBL_EPSILON_2, FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_add_inexact_int, long double, , += 1, 0,
	    LDBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    -1, 0)
TEST_FUNCS (long_double_preinc_inexact, long double, ++, , 0,
	    LDBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    -1, 0)
TEST_FUNCS (long_double_postinc_inexact, long double, , ++, 0,
	    LDBL_EPSILON / 2, NOT_MINUS_1, FE_INEXACT,
	    -1, 0)
TEST_FUNCS (complex_long_double_add_overflow, _Complex long double, , += LDBL_MAX, 0,
	    LDBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#endif
TEST_FUNCS (long_double_sub_invalid, long double, , -= __builtin_infl (), 0,
	    0, __builtin_isinf, 0,
	    __builtin_infl (), FE_INVALID)
#if LDBL_MANT_DIG != 106
TEST_FUNCS (long_double_sub_overflow, long double, , -= LDBL_MAX, 0,
	    -LDBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#define NOT_MINUS_LDBL_EPSILON_2(X) ((X) != -LDBL_EPSILON / 2)
TEST_FUNCS (long_double_sub_inexact, long double, , -= LDBL_EPSILON / 2, 0,
	    -1.0L, NOT_MINUS_LDBL_EPSILON_2, FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_sub_inexact_int, long double, , -= 1, 0,
	    -LDBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    1, 0)
TEST_FUNCS (long_double_predec_inexact, long double, --, , 0,
	    -LDBL_EPSILON / 2, NOT_0, FE_INEXACT,
	    1, 0)
TEST_FUNCS (long_double_postdec_inexact, long double, , --, 0,
	    -LDBL_EPSILON / 2, NOT_1, FE_INEXACT,
	    1, 0)
TEST_FUNCS (complex_long_double_sub_overflow, _Complex long double, , -= LDBL_MAX, 0,
	    -LDBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
#endif
TEST_FUNCS (long_double_mul_invalid, long double, , *= __builtin_infl (), 0,
	    __builtin_infl (), __builtin_isinf, 0,
	    0, FE_INVALID)
TEST_FUNCS (long_double_mul_overflow, long double, , *= LDBL_MAX, 0,
	    LDBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_mul_overflow_float, long double, , *= FLT_MAX, 0,
	    LDBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_mul_overflow_double, long double, , *= DBL_MAX, 0,
	    LDBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_mul_underflow, long double, , *= LDBL_MIN, 0,
	    LDBL_MIN, IS_0, FE_UNDERFLOW | FE_INEXACT,
	    1, 0)
#if LDBL_MANT_DIG != 106
TEST_FUNCS (long_double_mul_inexact, long double, , *= 1 + LDBL_EPSILON, 0,
	    1 + LDBL_EPSILON, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_mul_inexact_int, long double, , *= 3, 0,
	    1 + LDBL_EPSILON, NOT_0, FE_INEXACT,
	    0, 0)
#endif
TEST_FUNCS (complex_long_double_mul_overflow, _Complex long double, , *= LDBL_MAX, 0,
	    LDBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_div_invalid_divbyzero, long double, , /= 0.0L, 0,
	    1, __builtin_isinf, FE_DIVBYZERO,
	    0, FE_INVALID)
TEST_FUNCS (long_double_div_overflow, long double, , /= LDBL_MIN, 0,
	    LDBL_MAX, __builtin_isinf, FE_OVERFLOW | FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_div_underflow, long double, , /= LDBL_MAX, 0,
	    LDBL_MIN, IS_0, FE_UNDERFLOW | FE_INEXACT,
	    LDBL_MAX, 0)
TEST_FUNCS (long_double_div_inexact, long double, , /= 3.0L, 0,
	    1, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (long_double_div_inexact_int, long double, , /= 3, 0,
	    1, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (int_div_long_double_inexact, int, , /= 3.0L, 0,
	    4, NOT_0, FE_INEXACT,
	    0, 0)
TEST_FUNCS (complex_long_double_div_overflow, _Complex long double, , /= LDBL_MIN, 0,
	    LDBL_MAX, REAL_ISINF, FE_OVERFLOW | FE_INEXACT,
	    0, 0)

int
main (void)
{
  int ret = 0;
  ret |= test_main_float_add_invalid ();
  ret |= test_main_float_add_invalid_prev ();
  ret |= test_main_float_add_overflow ();
  ret |= test_main_float_add_overflow_prev ();
  ret |= test_main_float_add_overflow_double ();
  ret |= test_main_float_add_overflow_long_double ();
  ret |= test_main_float_add_inexact ();
  ret |= test_main_float_add_inexact_int ();
  ret |= test_main_float_preinc_inexact ();
  ret |= test_main_float_postinc_inexact ();
#if FLT_EVAL_METHOD == 0
  ret |= test_main_long_add_float_inexact ();
#endif
  ret |= test_main_complex_float_add_overflow ();
  ret |= test_main_float_sub_invalid ();
  ret |= test_main_float_sub_overflow ();
  ret |= test_main_float_sub_inexact ();
  ret |= test_main_float_sub_inexact_int ();
  ret |= test_main_float_predec_inexact ();
  ret |= test_main_float_postdec_inexact ();
#if FLT_EVAL_METHOD == 0
  ret |= test_main_long_sub_float_inexact ();
#endif
  ret |= test_main_complex_float_sub_overflow ();
  ret |= test_main_float_mul_invalid ();
  ret |= test_main_float_mul_overflow ();
  ret |= test_main_float_mul_underflow ();
  ret |= test_main_float_mul_inexact ();
  ret |= test_main_float_mul_inexact_int ();
#if FLT_EVAL_METHOD == 0
  ret |= test_main_long_mul_float_inexact ();
#endif
  ret |= test_main_complex_float_mul_overflow ();
  ret |= test_main_float_div_invalid_divbyzero ();
  ret |= test_main_float_div_overflow ();
  ret |= test_main_float_div_underflow ();
  ret |= test_main_float_div_inexact ();
  ret |= test_main_float_div_inexact_int ();
  ret |= test_main_int_div_float_inexact ();
  ret |= test_main_complex_float_div_overflow ();
  ret |= test_main_double_add_invalid ();
  ret |= test_main_double_add_overflow ();
  ret |= test_main_double_add_overflow_long_double ();
  ret |= test_main_double_add_inexact ();
  ret |= test_main_double_add_inexact_int ();
  ret |= test_main_double_preinc_inexact ();
  ret |= test_main_double_postinc_inexact ();
#if FLT_EVAL_METHOD == 0
  ret |= test_main_long_long_add_double_inexact ();
#endif
  ret |= test_main_complex_double_add_overflow ();
  ret |= test_main_double_sub_invalid ();
  ret |= test_main_double_sub_overflow ();
  ret |= test_main_double_sub_inexact ();
  ret |= test_main_double_sub_inexact_int ();
  ret |= test_main_double_predec_inexact ();
  ret |= test_main_double_postdec_inexact ();
#if FLT_EVAL_METHOD == 0
  ret |= test_main_long_long_sub_double_inexact ();
#endif
  ret |= test_main_complex_double_sub_overflow ();
  ret |= test_main_double_mul_invalid ();
  ret |= test_main_double_mul_overflow ();
  ret |= test_main_double_mul_overflow_float ();
  ret |= test_main_double_mul_underflow ();
  ret |= test_main_double_mul_inexact ();
  ret |= test_main_double_mul_inexact_int ();
#if FLT_EVAL_METHOD == 0
  ret |= test_main_long_long_mul_double_inexact ();
#endif
  ret |= test_main_complex_double_mul_overflow ();
  ret |= test_main_double_div_invalid_divbyzero ();
  ret |= test_main_double_div_overflow ();
  ret |= test_main_double_div_underflow ();
  ret |= test_main_double_div_inexact ();
  ret |= test_main_double_div_inexact_int ();
  ret |= test_main_int_div_double_inexact ();
  ret |= test_main_complex_double_div_overflow ();
  ret |= test_main_long_double_add_invalid ();
#if LDBL_MANT_DIG != 106
  ret |= test_main_long_double_add_overflow ();
  ret |= test_main_long_double_add_inexact ();
  ret |= test_main_long_double_add_inexact_int ();
  ret |= test_main_long_double_preinc_inexact ();
  ret |= test_main_long_double_postinc_inexact ();
  ret |= test_main_complex_long_double_add_overflow ();
#endif
  ret |= test_main_long_double_sub_invalid ();
#if LDBL_MANT_DIG != 106
  ret |= test_main_long_double_sub_overflow ();
  ret |= test_main_long_double_sub_inexact ();
  ret |= test_main_long_double_sub_inexact_int ();
  ret |= test_main_long_double_predec_inexact ();
  ret |= test_main_long_double_postdec_inexact ();
  ret |= test_main_complex_long_double_sub_overflow ();
#endif
  ret |= test_main_long_double_mul_invalid ();
  ret |= test_main_long_double_mul_overflow ();
  ret |= test_main_long_double_mul_overflow_float ();
  ret |= test_main_long_double_mul_overflow_double ();
  ret |= test_main_long_double_mul_underflow ();
#if LDBL_MANT_DIG != 106
  ret |= test_main_long_double_mul_inexact ();
  ret |= test_main_long_double_mul_inexact_int ();
#endif
  ret |= test_main_complex_long_double_mul_overflow ();
  ret |= test_main_long_double_div_invalid_divbyzero ();
  ret |= test_main_long_double_div_overflow ();
  ret |= test_main_long_double_div_underflow ();
  ret |= test_main_long_double_div_inexact ();
  ret |= test_main_long_double_div_inexact_int ();
  ret |= test_main_int_div_long_double_inexact ();
  ret |= test_main_complex_long_double_div_overflow ();
  if (ret != 0)
    abort ();
  else
    exit (0);
}
