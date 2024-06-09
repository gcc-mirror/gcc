/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable" } */

#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2, TYPE3)                                         \
  __attribute__ ((noipa)) void vwadd_##TYPE1_##TYPE2 (                         \
    TYPE1 *__restrict dst, TYPE1 *__restrict dst2, TYPE1 *__restrict dst3,     \
    TYPE1 *__restrict dst4, TYPE2 *__restrict a, TYPE3 *__restrict b,          \
    TYPE3 *__restrict a2, TYPE2 *__restrict b2, int *__restrict pred, int n)   \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      {                                                                        \
	dst[i] = pred[i] ? (TYPE1) a[i] * (TYPE1) b[i] : dst[i];               \
	dst2[i] = pred[i] ? (TYPE1) a2[i] * (TYPE1) b[i] : dst2[i];            \
	dst3[i] = pred[i] ? (TYPE1) a2[i] * (TYPE1) a[i] : dst3[i];            \
	dst4[i] = pred[i] ? (TYPE1) a[i] * (TYPE1) b2[i] : dst4[i];            \
      }                                                                        \
  }

#define TEST_ALL()                                                             \
  TEST_TYPE (int16_t, int8_t, uint8_t)                                         \
  TEST_TYPE (int32_t, int16_t, uint16_t)                                       \
  TEST_TYPE (int64_t, int32_t, uint32_t)                                       \
  TEST_TYPE (int16_t, uint8_t, int8_t)                                         \
  TEST_TYPE (int32_t, uint16_t, int16_t)                                       \
  TEST_TYPE (int64_t, uint32_t, int32_t)

TEST_ALL ()

/* { dg-final { scan-assembler-times {\tvwmulsu\.vv} 12 } } */
/* { dg-final { scan-assembler-times {\tvwmul\.vv} 6 } } */
/* { dg-final { scan-assembler-times {\tvwmulu\.vv} 6 } } */
/* { dg-final { scan-assembler-not {\tvmul} } } */
/* { dg-final { scan-assembler-not {\tvmerge\.vvm\t} } } */
