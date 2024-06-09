/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model -ffast-math" } */

#include "cond_arith-10.c"

#define N 99

#undef TEST
#define TEST(TYPE, NAME, OP)                                                   \
  {                                                                            \
    TYPE x[N], y[N], z[N], pred[N], merged[N];                                 \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	y[i] = i * i;                                                          \
	z[i] = ((i + 2) % 3) * (i + 1);                                        \
	pred[i] = i % 3;                                                       \
	merged[i] = i;                                                         \
      }                                                                        \
    test_##TYPE##_##NAME (x, y, z, pred, merged, N);                           \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	TYPE expected = i % 3 != 1 ? y[i] OP z[i] : merged[i];                 \
	if (x[i] != expected)                                                  \
	  __builtin_abort ();                                                  \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
  }

int
main (void)
{
  TEST_ALL
  return 0;
}
