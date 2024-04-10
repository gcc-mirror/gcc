/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "slp-13.c"

#define N1 (103 * 2)
#define N2 (111 * 2)

#define HARNESS(TYPE)						\
  {								\
    TYPE a[N2], b[N2 * 2];					\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	a[i] = i * 2 + i % 5;					\
	b[i * 2] = i * 3 + i % 7;				\
	b[i * 2 + 1] = i * 5 + i % 9;				\
      }								\
    vec_slp_##TYPE (a, b, N1 / 2);				\
    for (unsigned int i = 0; i < N2; ++i)			\
      {								\
	TYPE orig_a = i * 2 + i % 5;				\
	TYPE orig_b1 = i * 3 + i % 7;				\
	TYPE orig_b2 = i * 5 + i % 9;				\
	TYPE expected_a = orig_a;				\
	TYPE expected_b1 = orig_b1;				\
	TYPE expected_b2 = orig_b2;				\
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

/* This testcase is from aarch64 and floating-point operations are removed.
   TODO: We will add floating-point operations back and make them as common test in the future.  */
