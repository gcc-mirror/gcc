/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2)                                                \
  __attribute__ ((noipa)) void vwadd_##TYPE1_##TYPE2 (                         \
    TYPE1 *__restrict dst, TYPE1 *__restrict dst2, TYPE1 *__restrict dst3,     \
    TYPE1 *__restrict dst4, TYPE2 *__restrict a, TYPE2 *__restrict b,          \
    TYPE2 *__restrict a2, TYPE2 *__restrict b2, int n)                         \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      {                                                                        \
	dst[i] += -((TYPE1) a[i] * (TYPE1) b[i]);                              \
	dst2[i] += -((TYPE1) a2[i] * (TYPE1) b[i]);                            \
	dst3[i] += -((TYPE1) a2[i] * (TYPE1) a[i]);                            \
	dst4[i] += -((TYPE1) a[i] * (TYPE1) b2[i]);                            \
      }                                                                        \
  }

#define TEST_ALL()                                                             \
  TEST_TYPE (float, _Float16)                                                  \
  TEST_TYPE (double, float)

TEST_ALL ()

/* { dg-final { scan-assembler-times {\tvfwnmsac\.vv} 8 } } */
