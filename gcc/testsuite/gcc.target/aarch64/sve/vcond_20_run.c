/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "vcond_20.c"

#define N 97

#define TEST_LOOP(TYPE, NAME, CONST)				\
  {								\
    TYPE x[N], pred[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	pred[i] = i % 5 <= i % 6;				\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##TYPE##_##NAME (x, pred, N);				\
    for (int i = 0; i < N; ++i)					\
      {								\
	if (x[i] != (TYPE) (pred[i] > 0 ? CONST : 12.0))	\
	  __builtin_abort ();					\
	asm volatile ("" ::: "memory");				\
      }								\
  }

int __attribute__ ((optimize (1)))
main (int argc, char **argv)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
