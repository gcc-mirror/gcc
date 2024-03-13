/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh_zvl128b -mabi=lp64d -mrvv-vector-bits=zvl -mrvv-max-lmul=m2 -fno-vect-cost-model -ffast-math" } */
#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2, N)                                             \
  __attribute__((noipa)) TYPE1 reduc_##TYPE1##_##TYPE2(TYPE2 *restrict a) {    \
    TYPE1 sum = 0;                                                             \
    for (int i = 0; i < N; i += 1)                                             \
      sum += a[i];                                                             \
    return sum;                                                                \
  }

#define TEST_ALL(TEST)                                                         \
  TEST(int16_t, int8_t, 16)                                                    \
  TEST(int32_t, int16_t, 8)                                                    \
  TEST(int64_t, int32_t, 4)                                                    \
  TEST(uint16_t, uint8_t, 16)                                                  \
  TEST(uint32_t, uint16_t, 8)                                                  \
  TEST(uint64_t, uint32_t, 4)                                                  \
  TEST(float, _Float16, 8)                                                     \
  TEST(double, float, 4)

TEST_ALL(TEST_TYPE)

/* { dg-final { scan-assembler-times {\tvfwredusum\.vs} 2 } } */
/* { dg-final { scan-assembler-times {\tvwredsum\.vs} 3 } } */
/* { dg-final { scan-assembler-times {\tvwredsumu\.vs} 3 } } */
