/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "mask_struct_store-6.c"

#define N 100

#undef TEST_LOOP
#define TEST_LOOP(NAME, OUTTYPE, INTYPE, MASKTYPE)		\
  {								\
    OUTTYPE out[N * 7];						\
    INTYPE in[N];						\
    MASKTYPE mask[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	in[i] = i * 7 / 2;					\
	mask[i] = i % 5 <= i % 3;				\
	asm volatile ("" ::: "memory");				\
      }								\
    for (int i = 0; i < N * 7; ++i)				\
      out[i] = i * 9 / 2;					\
    NAME##_7 (out, in, mask, 42, N);				\
    for (int i = 0; i < N * 7; ++i)				\
      {								\
	OUTTYPE if_true = (INTYPE) (in[i / 7] + 42);		\
	OUTTYPE if_false = i * 9 / 2;				\
	if (out[i] != (mask[i / 7] ? if_true : if_false))	\
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
