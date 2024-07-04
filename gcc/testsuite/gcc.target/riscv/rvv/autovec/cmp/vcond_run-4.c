/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include "vcond-4.c"

#define N 401

#define RUN_LOOP(TYPE, BINOP)						\
  {									\
    TYPE dest[N], src[N], a[N], b[N], c[N], d[N];			\
    for (int i = 0; i < N; ++i)						\
      {									\
	src[i] = i * i;							\
	a[i] = i % 5 < 3 ? __builtin_nan("") : i;			\
	b[i] = i % 7 < 4 ? __builtin_nan("") : i;			\
	c[i] = i % 9 < 5 ? __builtin_nan("") : i;			\
	d[i] = i % 11 < 6 ? __builtin_nan("") : i;			\
	asm volatile ("" ::: "memory");					\
      }									\
    test_##TYPE##_##BINOP (dest, src, a, b, c, d, 100, N);		\
    for (int i = 0; i < N; ++i)						\
      {									\
	int res = BINOP (__builtin_isunordered (a[i], b[i]),		\
			 __builtin_isunordered (c[i], d[i]));		\
	if (dest[i] != (res ? src[i] : 100.0))				\
	  __builtin_abort ();						\
      }									\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (RUN_LOOP)
  return 0;
}
