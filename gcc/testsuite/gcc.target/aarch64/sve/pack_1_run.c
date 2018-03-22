/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "pack_1.c"

#define ARRAY_SIZE 57

#define TEST_LOOP(TYPED, TYPES)					\
  {								\
    TYPED arrayd[ARRAY_SIZE];					\
    TYPES arrays[ARRAY_SIZE];					\
    for (int i = 0; i < ARRAY_SIZE; i++)			\
      {								\
	arrays[i] = (i - 10) * 3;				\
	asm volatile ("" ::: "memory");				\
      }								\
    pack_##TYPED##_##TYPES (arrayd, arrays, ARRAY_SIZE);	\
    for (int i = 0; i < ARRAY_SIZE; i++)			\
      if (arrayd[i] != (TYPED) ((TYPES) ((i - 10) * 3) + 1))	\
	__builtin_abort ();					\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
