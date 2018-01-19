/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline" } */

#include "live_1.c"

#define N 107
#define OP 70

#define TEST_LOOP(TYPE)				\
  {						\
    TYPE a[N];					\
    for (int i = 0; i < N; ++i)			\
      {						\
	a[i] = i * 2 + (i % 3);			\
	asm volatile ("" ::: "memory");		\
      }						\
    TYPE expected = a[N - 1];			\
    TYPE res = test_##TYPE (a, N, OP);		\
    if (res != expected)			\
      __builtin_abort ();			\
    for (int i = 0; i < N; ++i)			\
      {						\
	TYPE old = i * 2 + (i % 3);		\
	if (a[i] != (TYPE) (old * (TYPE) OP))	\
	  __builtin_abort ();			\
	asm volatile ("" ::: "memory");		\
      }						\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_LOOP);
  return 0;
}
