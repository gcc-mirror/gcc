/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define TEST(TYPE, NAME, OP)                                                   \
  void __attribute__ ((noinline, noclone))                                     \
  test_##TYPE##_##NAME (TYPE *__restrict x, TYPE *__restrict y,                \
			TYPE *__restrict z, TYPE *__restrict pred, int n)      \
  {                                                                            \
    for (int i = 0; i < 128; ++i)                                              \
      x[i] = pred[i] != 1 ? y[i] OP z[i] : y[i];                               \
  }

#define TEST_TYPE(TYPE)                                                        \
  TEST (TYPE, add, +)                                                          \
  TEST (TYPE, sub, -)                                                          \
  TEST (TYPE, mul, *)                                                          \
  TEST (TYPE, div, /)

#define TEST_TYPE2(TYPE) TEST (TYPE, rem, %)

#define TEST_ALL                                                               \
  TEST_TYPE (int8_t)                                                           \
  TEST_TYPE (uint8_t)                                                          \
  TEST_TYPE (int16_t)                                                          \
  TEST_TYPE (uint16_t)                                                         \
  TEST_TYPE (int32_t)                                                          \
  TEST_TYPE (uint32_t)                                                         \
  TEST_TYPE (int64_t)                                                          \
  TEST_TYPE2 (int8_t)                                                          \
  TEST_TYPE2 (uint8_t)                                                         \
  TEST_TYPE2 (int16_t)                                                         \
  TEST_TYPE2 (uint16_t)                                                        \
  TEST_TYPE2 (int32_t)                                                         \
  TEST_TYPE2 (uint32_t)                                                        \
  TEST_TYPE2 (int64_t)                                                         \
  TEST_TYPE2 (uint64_t)                                                        \
  TEST_TYPE (_Float16)                                                         \
  TEST_TYPE (float)                                                            \
  TEST_TYPE (double)

TEST_ALL
