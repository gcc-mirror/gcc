/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include "cond_fmaxnm_2.c"

#define N 99

#define TEST_LOOP(FN, TYPE, NAME, CONST)				\
  {									\
    TYPE x[N], y[N], z[N];						\
    for (int i = 0; i < N; ++i)						\
      {									\
	y[i] = i % 13;							\
	z[i] = i * i;							\
      }									\
    test_##TYPE##_##NAME (x, y, z, N);					\
    for (int i = 0; i < N; ++i)						\
      {									\
	TYPE expected = y[i] < 8 ? FN (z[i], CONST) : y[i];		\
	if (x[i] != expected)						\
	  __builtin_abort ();						\
	asm volatile ("" ::: "memory");					\
      }									\
  }

int
main (void)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
