/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2)                                                \
  __attribute__ ((                                                             \
    noipa)) void vnshift_##TYPE1##_##TYPE2 (TYPE2 *__restrict dst,             \
					    TYPE1 *__restrict a, int n)        \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = a[i] >> 7;                                                      \
  }

#define TEST_ALL()                                                             \
  TEST_TYPE (int16_t, int8_t)                                                  \
  TEST_TYPE (int16_t, uint8_t)                                                 \
  TEST_TYPE (uint16_t, int8_t)                                                 \
  TEST_TYPE (uint16_t, uint8_t)                                                \
  TEST_TYPE (int32_t, int16_t)                                                 \
  TEST_TYPE (int32_t, uint16_t)                                                \
  TEST_TYPE (uint32_t, int16_t)                                                \
  TEST_TYPE (uint32_t, uint16_t)                                               \
  TEST_TYPE (int64_t, int32_t)                                                 \
  TEST_TYPE (int64_t, uint32_t)                                                \
  TEST_TYPE (uint64_t, int32_t)                                                \
  TEST_TYPE (uint64_t, uint32_t)

TEST_ALL ()

/* { dg-final { scan-assembler-times {\tvnsra\.wi} 6 } } */
/* { dg-final { scan-assembler-times {\tvnsrl\.wi} 6 } } */
