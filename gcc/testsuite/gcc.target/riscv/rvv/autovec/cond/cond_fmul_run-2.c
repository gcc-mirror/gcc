/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_fmul-2.c"

#define N 99

#define TEST_LOOP(TYPE, NAME, CONST)					\
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
	TYPE expected = y[i] < 8 ? z[i] * (TYPE) CONST : y[i];		\
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
