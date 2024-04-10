/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include "mulh-1.c"

#define N 79

#define TEST_LOOP(TYPE)				\
  {						\
    TYPE dst[N], src[N];			\
    for (int i = 0; i < N; ++i)			\
      {						\
	src[i] = i * 7 + i % 3;			\
	if (i % 11 > 7)				\
	  src[i] = -src[i];			\
	asm volatile ("" ::: "memory");		\
      }						\
    mod_##TYPE (dst, src, N);			\
    for (int i = 0; i < N; ++i)			\
      if (dst[i] != src[i] % 19)		\
	__builtin_abort ();			\
  }

int
main (void)
{
  TEST_ALL (TEST_LOOP);
  return 0;
}
