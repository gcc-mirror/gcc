/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define TEST_LOOP(NAME, TYPE)					\
  void __attribute__ ((noinline, noclone))			\
  NAME (TYPE *restrict dest, TYPE *restrict src, int n)		\
  {								\
    for (int i = 0; i < n; ++i)					\
      dest[i] += src[i * 4];					\
  }

#define TEST(NAME) \
  TEST_LOOP (NAME##_i8, int8_t) \
  TEST_LOOP (NAME##_i16, uint16_t) \
  TEST_LOOP (NAME##_f32, float) \
  TEST_LOOP (NAME##_f64, double)

TEST (test)

/* { dg-final { scan-assembler-times {vlseg4e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg4e16\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg4e32\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg4e64\.v} 1 } } */
