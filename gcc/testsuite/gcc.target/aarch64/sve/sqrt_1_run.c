/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast -mlow-precision-sqrt" } */

#include "sqrt_1.c"

#define N 77

#define TEST_LOOP(TYPE, FN)				\
  {							\
    TYPE a[N];						\
    for (int i = 0; i < N; ++i)				\
      a[i] = i;						\
    test_##TYPE (a, N);					\
    for (int i = 0; i < N; ++i)				\
      {							\
	double diff = a[i] - __builtin_sqrt (i);	\
	if (__builtin_fabs (diff) > 0x1.0p-8)		\
	  __builtin_abort ();				\
      }							\
  }

int
main (void)
{
  TEST_ALL (TEST_LOOP);
  return 0;
}
