/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math --param aarch64-sve-compare-costs=0" } */

#include "mask_struct_store_2.c"

#define N 100

#undef TEST_LOOP
#define TEST_LOOP(NAME, OUTTYPE, INTYPE, MASKTYPE)		\
  {								\
    OUTTYPE out[N * 3];						\
    INTYPE in[N];						\
    MASKTYPE mask[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	in[i] = i * 7 / 2;					\
	mask[i] = i % 5 <= i % 3;				\
	asm volatile ("" ::: "memory");				\
      }								\
    for (int i = 0; i < N * 3; ++i)				\
      out[i] = i * 9 / 2;					\
    NAME##_3 (out, in, mask, 11, N);				\
    for (int i = 0; i < N * 3; ++i)				\
      {								\
	OUTTYPE if_true = (INTYPE) (in[i / 3] + 11);		\
	OUTTYPE if_false = i * 9 / 2;				\
	if (out[i] != (mask[i / 3] ? if_true : if_false))	\
	  __builtin_abort ();					\
	asm volatile ("" ::: "memory");				\
      }								\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST (test);
  return 0;
}
