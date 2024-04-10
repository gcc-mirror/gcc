/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "slp-10.c"

#define N (103 * 2)

#define HARNESS(TYPE)						\
  {								\
    TYPE a[N], b[2] = { 10, 17 };				\
    for (unsigned int i = 0; i < N; ++i)			\
      {								\
	a[i] = i * 2 + i % 5;					\
	asm volatile ("" ::: "memory");				\
      }								\
    vec_slp_##TYPE (a, N / 2);					\
    for (unsigned int i = 0; i < N; ++i)			\
      {								\
	TYPE orig = i * 2 + i % 5;				\
	TYPE expected = orig + b[i % 2];			\
	if (a[i] != expected)					\
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
