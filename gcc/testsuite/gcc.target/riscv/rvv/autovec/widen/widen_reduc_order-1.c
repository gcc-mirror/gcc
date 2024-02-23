/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */
#include <stdint-gcc.h>

#define TEST_TYPE(TYPE1, TYPE2)                                                \
  __attribute__((noipa))                                                       \
  TYPE1 reduc_##TYPE1##_##TYPE2(TYPE2 *restrict a, int n) {                    \
    TYPE1 sum = 0;                                                             \
    for (int i = 0; i < n; i += 1)                                             \
      sum += a[i];                                                             \
    return sum;                                                                \
  }

#define TEST_ALL(TEST)                                                         \
  TEST(float, _Float16)                                                        \
  TEST(double, float)

TEST_ALL(TEST_TYPE)

/* { dg-final { scan-assembler-times {\tvfwredosum\.vs} 2 } } */
