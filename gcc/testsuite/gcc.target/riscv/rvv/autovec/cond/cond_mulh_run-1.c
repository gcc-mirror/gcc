/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_mulh-1.c"

#define N 99

#define TEST_LOOP(TYPE, WTYPE)                                                 \
  {                                                                            \
    TYPE a[N], b[N], c[N], pred[N];                                            \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	a[i] = i * i;                                                          \
	b[i] = ((i + 2) % 3) * (i + 1);                                        \
	pred[i] = i % 3;                                                       \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    test_##TYPE##_##WTYPE (a, b, c, pred, N);                                  \
    for (int i = 0; i < N; ++i)                                                \
      if (c[i]                                                                 \
	  != (pred[i]                                                          \
		? (TYPE) (((WTYPE) a[i] * (WTYPE) b[i]) >> sizeof (TYPE) * 8)  \
		: c[i]))                                                       \
	__builtin_abort ();                                                    \
  }

int
main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
