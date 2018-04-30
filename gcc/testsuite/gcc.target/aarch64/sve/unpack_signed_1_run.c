/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline" } */

#include "unpack_signed_1.c"

#define ARRAY_SIZE 33

#define TEST_LOOP(TYPED, TYPES)					\
  {								\
    TYPED arrayd[ARRAY_SIZE];					\
    TYPES arrays[ARRAY_SIZE];					\
    for (int i = 0; i < ARRAY_SIZE; i++)			\
      {								\
	arrays[i] = (i - 10) * 3;				\
	asm volatile ("" ::: "memory");				\
      }								\
    unpack_##TYPED##_##TYPES (arrayd, arrays, 7, ARRAY_SIZE);	\
    for (int i = 0; i < ARRAY_SIZE; i++)			\
      if (arrayd[i] != (TYPED) (TYPES) (((i - 10) * 3) | 7))	\
	__builtin_abort ();					\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
