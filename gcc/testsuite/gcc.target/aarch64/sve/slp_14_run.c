/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "slp_14.c"

#define N1 (103 * 2)
#define N2 (111 * 2)

#define HARNESS(TYPE)						\
  {								\
    TYPE a[N2], b[N2];						\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	a[i] = i * 2 + i % 5;					\
	b[i] = i % 11;						\
      }								\
    vec_slp_##TYPE (a, b, N1 / 2);				\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	TYPE orig_a = i * 2 + i % 5;				\
	TYPE orig_b = i % 11;					\
	TYPE expected_a = orig_a;				\
	if (i < N1 && orig_b > (i & 1 ? 2 : 1))			\
	  expected_a /= orig_b;					\
	if (a[i] != expected_a || b[i] != orig_b)		\
	  __builtin_abort ();					\
      }								\
  }

int
main (void)
{
  TEST_ALL (HARNESS)
}
