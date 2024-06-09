/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math" } */

#include "ternop-7.c"

#define TEST_LOOP(TYPE, NUM)                                                   \
  {                                                                            \
    TYPE array1_##NUM[NUM] = {};                                               \
    TYPE array2_##NUM[NUM] = {};                                               \
    TYPE array3_##NUM[NUM] = {};                                               \
    TYPE array4_##NUM[NUM] = {};                                               \
    for (int i = 0; i < NUM; ++i)                                              \
      {                                                                        \
	array1_##NUM[i] = (i & 1) + 5;                                         \
	array2_##NUM[i] = i - NUM / 3;                                         \
	array3_##NUM[i] = NUM - NUM / 3 - i;                                   \
	array4_##NUM[i] = NUM - NUM / 3 - i;                                   \
	asm volatile("" ::: "memory");                                         \
      }                                                                        \
    ternop_##TYPE (array3_##NUM, array1_##NUM, array2_##NUM, NUM);             \
    for (int i = 0; i < NUM; i++)                                              \
      if (array3_##NUM[i]                                                      \
	  != (TYPE) (array1_##NUM[i] * array2_##NUM[i] - array4_##NUM[i]))     \
	__builtin_abort ();                                                    \
  }

int __attribute__ ((optimize (0))) main ()
{
  TEST_LOOP (_Float16, 7)
  TEST_LOOP (_Float16, 16)
  TEST_LOOP (_Float16, 77)
  TEST_LOOP (_Float16, 128)
  TEST_LOOP (_Float16, 299)
  return 0;
}
