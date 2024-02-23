/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "mask_struct_store-1.c"

#define N 100

#undef TEST_LOOP
#define TEST_LOOP(NAME, OUTTYPE, INTYPE, MASKTYPE)		\
  {								\
    OUTTYPE out[N * 2];						\
    INTYPE in[N];						\
    MASKTYPE mask[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	in[i] = i * 7 / 2;					\
	mask[i] = i % 5 <= i % 3;				\
	asm volatile ("" ::: "memory");				\
      }								\
    for (int i = 0; i < N * 2; ++i)				\
      out[i] = i * 9 / 2;					\
    NAME##_2 (out, in, mask, 17, N);				\
    for (int i = 0; i < N * 2; ++i)				\
      {								\
	OUTTYPE if_true = (INTYPE) (in[i / 2] + 17);		\
	OUTTYPE if_false = i * 9 / 2;				\
	if (out[i] != (mask[i / 2] ? if_true : if_false))	\
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
