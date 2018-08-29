/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "slp_11.c"

#define N1 (103 * 2)
#define N2 (111 * 2)

#define HARNESS(TYPE1, TYPE2)					\
  {								\
    TYPE1 a[N2];						\
    TYPE2 b[N2 * 2];						\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	a[i] = i * 2 + i % 5;					\
	b[i * 2] = i * 3 + i % 7;				\
	b[i * 2 + 1] = i * 5 + i % 9;				\
      }								\
    vec_slp_##TYPE1##_##TYPE2 (a, b, N1 / 2);			\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	TYPE1 orig_a = i * 2 + i % 5;				\
	TYPE2 orig_b1 = i * 3 + i % 7;				\
	TYPE2 orig_b2 = i * 5 + i % 9;				\
	TYPE1 expected_a = orig_a;				\
	TYPE2 expected_b1 = orig_b1;				\
	TYPE2 expected_b2 = orig_b2;				\
	if (i < N1)						\
	  {							\
	    expected_a += i & 1 ? 2 : 1;			\
	    expected_b1 += i & 1 ? 5 : 3;			\
	    expected_b2 += i & 1 ? 6 : 4;			\
	  }							\
	if (a[i] != expected_a					\
	    || b[i * 2] != expected_b1				\
	    || b[i * 2 + 1] != expected_b2)			\
	  __builtin_abort ();					\
      }								\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (HARNESS)
}
