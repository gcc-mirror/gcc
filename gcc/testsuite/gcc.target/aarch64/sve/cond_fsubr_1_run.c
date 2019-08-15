/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_fsubr_1.c"

#define N 99

#define TEST_LOOP(TYPE, PRED_TYPE, NAME, CONST)				\
  {									\
    TYPE x[N], y[N];							\
    PRED_TYPE pred[N];							\
    for (int i = 0; i < N; ++i)						\
      {									\
	y[i] = i * i;							\
	pred[i] = i % 3;						\
      }									\
    test_##TYPE##_##NAME (x, y, pred, N);				\
    for (int i = 0; i < N; ++i)						\
      {									\
	TYPE expected = i % 3 != 1 ? (TYPE) CONST - y[i] : y[i];	\
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
