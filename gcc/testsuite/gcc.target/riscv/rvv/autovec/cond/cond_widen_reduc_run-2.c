/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -mrvv-max-lmul=m2 -fno-vect-cost-model -ffast-math" } */

#include "cond_widen_reduc-2.c"

#define RUN(TYPE1, TYPE2, N)                                                   \
  {                                                                            \
    TYPE2 a[N];                                                                \
    TYPE2 pred[N];                                                             \
    TYPE1 r = 0;                                                               \
    for (int i = 0; i < N; i++)                                                \
      {                                                                        \
	a[i] = (i * 0.1) * (i & 1 ? 1 : -1);                                   \
	pred[i] = i % 3;                                                       \
	if (pred[i])                                                           \
	  r += a[i];                                                           \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    if (r != reduc_##TYPE1##_##TYPE2 (a, pred))                                \
      __builtin_abort ();                                                      \
  }

int __attribute__ ((optimize (1)))
main ()
{
  TEST_ALL (RUN)
  return 0;
}
