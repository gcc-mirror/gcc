/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast -mlow-precision-sqrt" } */

#include "rsqrt_1.c"

#define N 77

#define TEST_LOOP(TYPE, FN)					\
  {								\
    TYPE a[N];							\
    for (int i = 0; i < N; ++i)					\
      a[i] = i + 1;						\
    test_##TYPE (a, N);						\
    for (int i = 0; i < N; ++i)					\
      {								\
	double diff = a[i] - 1.0 / __builtin_sqrt (i + 1);	\
	if (__builtin_fabs (diff) > 0x1.0p-8)			\
	  __builtin_abort ();					\
      }								\
  }

int
main (void)
{
  TEST_ALL (TEST_LOOP);
  return 0;
}
