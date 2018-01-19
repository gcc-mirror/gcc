/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O3" } */

#include "nlogical_1.c"

#define N 128

#define TEST_VNLOGICAL(TYPE)				\
  {							\
    TYPE dst[N], src[N];				\
    for (int i = 0; i < N; ++i)				\
      {							\
	dst[i] = i ^ 42;				\
	asm volatile ("" ::: "memory");			\
      }							\
    vnlogical_not_##TYPE (dst, N);			\
    for (int i = 0; i < N; ++i)				\
      if (dst[i] != (TYPE) ~(i ^ 42))			\
	__builtin_abort ();				\
    for (int i = 0; i < N; ++i)				\
      {							\
	dst[i] = i ^ 42;				\
	src[i] = i % 5;					\
	asm volatile ("" ::: "memory");			\
      }							\
    vnlogical_bic_##TYPE (dst, src, N);			\
    for (int i = 0; i < N; ++i)				\
      if (dst[i] != (TYPE) ((i ^ 42) & ~(i % 5)))	\
	__builtin_abort ();				\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_VNLOGICAL)
  return 0;
}
