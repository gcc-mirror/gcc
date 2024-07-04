/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_narrow_shift-1.c"

#define N 99

#define TEST_LOOP(TYPE1, TYPE2)                                                \
  {                                                                            \
    TYPE2 r[N], a[N];                                                          \
    TYPE1 b[N];                                                                \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	a[i] = (i & 1 ? i : 3 * i);                                            \
	b[i] = (i >> 4) << (i & 15);                                           \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    test_##TYPE1##_##TYPE2 (r, a, b, N);                                       \
    for (int i = 0; i < N; ++i)                                                \
      if (r[i] != (a[i] > 20 ? (TYPE2) (b[i] >> 3) : r[i]))                    \
	__builtin_abort ();                                                    \
  }

int
main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
