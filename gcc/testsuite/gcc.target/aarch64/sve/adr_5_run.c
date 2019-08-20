/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "adr_5.c"

#define N 131

#define TEST_LOOP(FACTOR)						\
  {									\
    uint64_t a[N], b[N];						\
    for (int i = 0; i < N; ++i)						\
      {									\
	a[i] = (uint64_t) i * i + i % 5;				\
	b[i] = (uint64_t) (i * 3) << ((i & 7) * 8);			\
	asm volatile ("" ::: "memory");					\
      }									\
    test_##FACTOR (a, b, N);						\
    for (int i = 0; i < N; ++i)						\
      {									\
	uint64_t expected = ((uint64_t) (i * i + i % 5)			\
			     + (((uint64_t) (i * 3) << ((i & 7) * 8))	\
				& 0xffffffff) * FACTOR);		\
	if (a[i] != expected)						\
	  __builtin_abort ();						\
      }									\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_LOOP)
}
