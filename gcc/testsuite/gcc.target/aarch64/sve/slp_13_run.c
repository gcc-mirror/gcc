/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "slp_13.c"

#define N1 (103 * 2)
#define N2 (111 * 2)

#define HARNESS(TYPE)						\
  {								\
    TYPE a[N2];							\
    TYPE expected = 0;						\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	a[i] = i * 2 + i % 5;					\
	if (i < N1)						\
	  expected += a[i] * (i & 1 ? 5 : 3);			\
	asm volatile ("");					\
      }								\
    if (vec_slp_##TYPE (a, N1 / 2) != expected)			\
      __builtin_abort ();					\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (HARNESS)
}
