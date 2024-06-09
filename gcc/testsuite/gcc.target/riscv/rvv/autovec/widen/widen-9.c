/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2, TYPE3)                                         \
  __attribute__ ((noipa)) void vwmacc_##TYPE1_##TYPE2 (TYPE1 *__restrict dst,  \
						       TYPE2 *__restrict a,    \
						       TYPE3 *__restrict b,    \
						       int n)                  \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] += (TYPE1) a[i] * (TYPE1) b[i];                                   \
  }

#define TEST_ALL()                                                             \
  TEST_TYPE (int16_t, int8_t, uint8_t)                                         \
  TEST_TYPE (int32_t, int16_t, uint16_t)                                       \
  TEST_TYPE (int64_t, int32_t, uint32_t)

TEST_ALL ()

/* { dg-final { scan-assembler-times {\tvwmaccsu\.vv} 3 } } */
