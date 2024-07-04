/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -march=armv8-a+sve --param aarch64-vect-compare-costs=0" } */

#include "vcond_11.c"

#define N 133

#define TEST_LOOP(TYPE)							\
  {									\
    int a[N];								\
    TYPE b[N];								\
    for (int i = 0; i < N; ++i)						\
      {									\
	a[i] = i % 5;							\
	b[i] = i % 7;							\
      }									\
    test_##TYPE (a, b, 10, 11, 12, 13, N);				\
    for (int i = 0; i < N; ++i)						\
      if (a[i] != 10 + (i & 1) * 2 + (i % 5 == 0 || i % 7 == 3))	\
	__builtin_abort ();						\
  }

int
main (void)
{
  FOR_EACH_TYPE (TEST_LOOP);
  return 0;
}
