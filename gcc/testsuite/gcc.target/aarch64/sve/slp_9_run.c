/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "slp_9.c"

#define N1 (103 * 2)
#define N2 (111 * 2)

#define HARNESS(TYPE1, TYPE2)					\
  {								\
    TYPE1 a[N2];						\
    TYPE2 b[N2];						\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	a[i] = i * 2 + i % 5;					\
	b[i] = i * 3 + i % 7;					\
      }								\
    vec_slp_##TYPE1##_##TYPE2 (a, b, N1 / 2);			\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	TYPE1 orig_a = i * 2 + i % 5;				\
	TYPE2 orig_b = i * 3 + i % 7;				\
	TYPE1 expected_a = orig_a;				\
	TYPE2 expected_b = orig_b;				\
	if (i < N1)						\
	  {							\
	    expected_a += i & 1 ? 2 : 1;			\
	    expected_b += i & 1 ? 4 : 3;			\
	  }							\
	if (a[i] != expected_a || b[i] != expected_b)		\
	  __builtin_abort ();					\
      }								\
  }

int __attribute__ ((noinline, noclone))
main (void)
{
  TEST_ALL (HARNESS)
}
