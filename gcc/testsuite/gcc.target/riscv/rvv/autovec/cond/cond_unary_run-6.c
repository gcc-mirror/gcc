/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_unary-6.c"

#define TEST_LOOP(TYPE1, TYPE2, N, OP)				\
  {								\
    TYPE1 pred[N];						\
    TYPE2 r[N], a[N], b[N];					\
    for (int i = 0; i < N; ++i)					\
      {								\
	a[i] = (i & 1 ? i : 3 * i) * (i % 3 == 0 ? 1 : -1);	\
	b[i] = (i % 5) * (i % 6 + 3);				\
	pred[i] = (i % 4 < 2);					\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##TYPE1##_##TYPE2##_##OP (r, a, b, pred);		\
    for (int i = 0; i < N; ++i)					\
      if (r[i] != (pred[i] ? OP (a[i]) : b[i]))			\
	__builtin_abort ();					\
  }

int main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
