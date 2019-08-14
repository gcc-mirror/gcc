/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "adr_1.c"

#define N 131

#define TEST_LOOP(TYPE)						\
  {								\
    TYPE a[N], b[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	a[i] = (TYPE) i * i + i % 5;				\
	b[i] = (TYPE) i * 3 + i % 7;				\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##TYPE (a, b, N);					\
    for (int i = 0; i < N; ++i)					\
      {								\
	TYPE expected = ((TYPE) (i * i + i % 5)			\
			 + ((TYPE) i * 3 + i % 7) * FACTOR);	\
	if (a[i] != expected)					\
	  __builtin_abort ();					\
      }								\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_LOOP)
}
