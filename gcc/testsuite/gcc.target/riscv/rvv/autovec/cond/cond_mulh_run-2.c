/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_mulh-2.c"

#define N 99

#define TEST_LOOP(TYPE1, WTYPE1, TYPE2, WTYPE2, TYPE3)                         \
  {                                                                            \
    TYPE1 a[N];                                                                \
    TYPE2 b[N];                                                                \
    TYPE3 c[N], pred[N];                                                       \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	a[i] = i * i;                                                          \
	b[i] = ((i + 2) % 3) * (i + 1);                                        \
	pred[i] = i % 3;                                                       \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    test_##TYPE1##_##TYPE2 (a, b, c, pred, N);                                 \
    for (int i = 0; i < N; ++i)                                                \
      if (c[i]                                                                 \
	  != (pred[i] ? (TYPE3) (((WTYPE1) a[i] * (WTYPE2) b[i])               \
				 >> sizeof (TYPE1) * 8)                        \
		      : c[i]))                                                 \
	__builtin_abort ();                                                    \
  }

int
main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
