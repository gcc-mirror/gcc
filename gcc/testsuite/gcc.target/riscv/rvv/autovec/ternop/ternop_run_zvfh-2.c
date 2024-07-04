/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math" } */

#include "ternop-2.c"

#define TEST_LOOP(TYPE, NUM)                                                   \
  {                                                                            \
    TYPE array1_##NUM[NUM] = {};                                               \
    TYPE array2_##NUM[NUM] = {};                                               \
    TYPE array3_##NUM[NUM] = {};                                               \
    TYPE array4_##NUM[NUM] = {};                                               \
    TYPE array5_##NUM[NUM] = {};                                               \
    TYPE array6_##NUM[NUM] = {};                                               \
    TYPE array7_##NUM[NUM] = {};                                               \
    TYPE array8_##NUM[NUM] = {};                                               \
    for (int i = 0; i < NUM; ++i)                                              \
      {                                                                        \
	int val = NUM / 18.3;                                                  \
	int val2 = i / 18.7;                                                   \
	array1_##NUM[i] = (i & 1) + 5;                                         \
	array2_##NUM[i] = val2 - val / 3;                                      \
	array3_##NUM[i] = val - val / 3 - val2;                                \
	array6_##NUM[i] = val - val / 3 - val2;                                \
	array4_##NUM[i] = val - val / 2 + val2;                                \
	array7_##NUM[i] = val - val / 2 + val2;                                \
	array5_##NUM[i] = val + val2;                                          \
	array8_##NUM[i] = val + val2;                                          \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    ternop_##TYPE (array3_##NUM, array4_##NUM, array5_##NUM, array1_##NUM,     \
		   array2_##NUM, NUM);                                         \
    for (int i = 0; i < NUM; i++)                                              \
      {                                                                        \
	array6_##NUM[i]                                                        \
	  = (TYPE) (array1_##NUM[i] * array2_##NUM[i] + array6_##NUM[i]);      \
	if (array3_##NUM[i] != array6_##NUM[i])                                \
	  __builtin_abort ();                                                  \
	array7_##NUM[i]                                                        \
	  = (TYPE) (array1_##NUM[i] * array6_##NUM[i] + array7_##NUM[i]);      \
	if (array4_##NUM[i] != array7_##NUM[i])                                \
	  __builtin_abort ();                                                  \
	array8_##NUM[i]                                                        \
	  = (TYPE) (array2_##NUM[i] * array7_##NUM[i] + array8_##NUM[i]);      \
	if (array5_##NUM[i] != array8_##NUM[i])                                \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

int __attribute__ ((optimize (0))) main ()
{
  TEST_LOOP (_Float16, 7)
  TEST_LOOP (_Float16, 16)
  TEST_LOOP (_Float16, 94)
  TEST_LOOP (_Float16, 128)
  TEST_LOOP (_Float16, 199)
  return 0;
}
