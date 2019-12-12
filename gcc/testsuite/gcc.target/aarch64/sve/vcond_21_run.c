/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "vcond_21.c"

#define N 97

#define TEST_LOOP(TYPE, ABS, NAME, OP)				\
  {								\
    TYPE r[N], a[N], b[N];					\
    for (int i = 0; i < N; ++i)					\
      {								\
	a[i] = i % 5 * (i & 1 ? -1 : 1);			\
	b[i] = i % 9 * (i & 2 ? -1 : 1);			\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##TYPE##_##NAME (r, a, b, N);				\
    for (int i = 0; i < N; ++i)					\
      {								\
	if (r[i] != (ABS (a[i]) OP ABS (b[i]) ? 1.0 : 0.0))	\
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
