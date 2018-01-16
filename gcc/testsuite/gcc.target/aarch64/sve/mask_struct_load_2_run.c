/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include "mask_struct_load_2.c"

#define N 100

#undef TEST_LOOP
#define TEST_LOOP(NAME, OUTTYPE, INTYPE, MASKTYPE)	\
  {							\
    OUTTYPE out[N];					\
    INTYPE in[N * 3];					\
    MASKTYPE mask[N];					\
    for (int i = 0; i < N; ++i)				\
      {							\
	out[i] = i * 7 / 2;				\
	mask[i] = i % 5 <= i % 3;			\
	asm volatile ("" ::: "memory");			\
      }							\
    for (int i = 0; i < N * 3; ++i)			\
      in[i] = i * 9 / 2;				\
    NAME##_3 (out, in, mask, N);			\
    for (int i = 0; i < N; ++i)				\
      {							\
	OUTTYPE if_true = (in[i * 3]			\
			   + in[i * 3 + 1]		\
			   + in[i * 3 + 2]);		\
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
