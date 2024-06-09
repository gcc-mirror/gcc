/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model -ffast-math" } */

#include "cond_arith-3.c"

#define N 99

#undef TEST
#define TEST(DATA_TYPE, PRED_TYPE, NAME, OP)			\
  {								\
    DATA_TYPE x[N], y[N], z[N];					\
    PRED_TYPE pred[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	y[i] = i * i;						\
	z[i] = ((i + 2) % 3) * (i + 1);				\
	pred[i] = i % 3;					\
      }								\
    test_##DATA_TYPE##_##PRED_TYPE##_##NAME (x, y, z, pred, N);	\
    for (int i = 0; i < N; ++i)					\
      {								\
	DATA_TYPE expected = i % 3 != 1 ? y[i] OP z[i] : y[i];	\
	if (x[i] != expected)					\
	  __builtin_abort ();					\
	asm volatile ("" ::: "memory");				\
      }								\
  }

int
main (void)
{
  TEST_ALL
  return 0;
}
