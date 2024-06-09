/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "mask_struct_load-6.c"

#define N 100

#undef TEST_LOOP
#define TEST_LOOP(NAME, OUTTYPE, INTYPE, MASKTYPE)	\
  {							\
    OUTTYPE out[N];					\
    INTYPE in[N * 7];					\
    MASKTYPE mask[N];					\
    for (int i = 0; i < N; ++i)				\
      {							\
	out[i] = i * 7 / 2;				\
	mask[i] = i % 5 <= i % 3;			\
	asm volatile ("" ::: "memory");			\
      }							\
    for (int i = 0; i < N * 7; ++i)			\
      in[i] = i * 9 / 2;				\
    NAME##_7 (out, in, mask, N);			\
    for (int i = 0; i < N; ++i)				\
      {							\
	OUTTYPE if_true = (in[i * 7]			\
			   + in[i * 7 + 1]		\
			   + in[i * 7 + 2]		\
			   + in[i * 7 + 3]		\
         + in[i * 7 + 4]		\
         + in[i * 7 + 5]		\
         + in[i * 7 + 6]);		\
	OUTTYPE if_false = i * 7 / 2;			\
	if (out[i] != (mask[i] ? if_true : if_false))	\
	  __builtin_abort ();				\
	asm volatile ("" ::: "memory");			\
      }							\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST (test);
  return 0;
}
