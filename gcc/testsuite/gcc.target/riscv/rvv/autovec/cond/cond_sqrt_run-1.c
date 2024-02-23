/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model -ffast-math " } */

#include "cond_sqrt-1.c"
#include <stdio.h>

#define N 99

#define TEST_LOOP(TYPE, OP)                                                    \
  {                                                                            \
    TYPE r[N], a[N], pred[N];                                                  \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	a[i] = (i & 1 ? i : 3 * i) * (i % 3 == 0 ? 1 : 2);                     \
	pred[i] = (i % 7 < 4);                                                 \
	asm volatile("" ::: "memory");                                         \
      }                                                                        \
    test_##TYPE##_##OP (r, a, pred, N);                                        \
    for (int i = 0; i < N; ++i)                                                \
      if (r[i] != (pred[i] ? OP (a[i]) : a[i]))                                \
	__builtin_abort ();                                                    \
  }

int
main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
