/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_fmul-5.c"

#define N 99

#define TEST_LOOP(TYPE, PRED_TYPE, NAME, CONST)                                \
  {                                                                            \
    TYPE x[N], y[N], merged[N];                                                \
    PRED_TYPE pred[N];                                                         \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	y[i] = i * i;                                                          \
	pred[i] = i % 3;                                                       \
	merged[i] = i;                                                         \
      }                                                                        \
    test_##TYPE##_##NAME (x, y, pred, merged, N);                              \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	TYPE expected = i % 3 != 1 ? y[i] * (TYPE) CONST : merged[i];          \
	if (x[i] != expected)                                                  \
	  __builtin_abort ();                                                  \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
  }

int
main (void)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
