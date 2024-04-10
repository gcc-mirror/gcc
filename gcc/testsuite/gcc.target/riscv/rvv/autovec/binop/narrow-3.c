/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2)                                                \
  __attribute__ ((noipa)) void vnshift_##TYPE1##_##TYPE2 (                     \
    TYPE2 *__restrict dst, TYPE1 *__restrict a, TYPE2 b, int n)                \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = a[i] >> b;                                                      \
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

/* { dg-final { scan-assembler-times {\tvnsra\.wx} 8 } } */
/* { dg-final { scan-assembler-times {\tvnsrl\.wx} 4 } } */
