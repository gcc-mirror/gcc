/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -mrvv-vector-bits=scalable -fno-vect-cost-model -ffast-math -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#ifndef INDEX8
#define INDEX8 int8_t
#define INDEX16 int16_t
#define INDEX32 int32_t
#define INDEX64 int64_t
#endif

#define TEST_LOOP(DATA_TYPE, BITS)                                             \
  void __attribute__ ((noinline, noclone))                                     \
  f_##DATA_TYPE##_##BITS (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,   \
			  INDEX##BITS stride, INDEX##BITS n)                   \
  {                                                                            \
    for (INDEX##BITS i = 0; i < n; ++i)                                        \
      dest[i] += src[i * stride];                                              \
  }

#define TEST_TYPE(T, DATA_TYPE)                                                \
  T (DATA_TYPE, 8)                                                             \
  T (DATA_TYPE, 16)                                                            \
  T (DATA_TYPE, 32)                                                            \
  T (DATA_TYPE, 64)

#define TEST_ALL(T)                                                            \
  TEST_TYPE (T, int8_t)                                                        \
  TEST_TYPE (T, uint8_t)                                                       \
  TEST_TYPE (T, int16_t)                                                       \
  TEST_TYPE (T, uint16_t)                                                      \
  TEST_TYPE (T, _Float16)                                                      \
  TEST_TYPE (T, int32_t)                                                       \
  TEST_TYPE (T, uint32_t)                                                      \
  TEST_TYPE (T, float)                                                         \
  TEST_TYPE (T, int64_t)                                                       \
  TEST_TYPE (T, uint64_t)                                                      \
  TEST_TYPE (T, double)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-tree-dump-times " \.MASK_LEN_GATHER_LOAD" 66 "optimized" } } */
/* { dg-final { scan-tree-dump-not " \.GATHER_LOAD" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \.MASK_GATHER_LOAD" "optimized" } } */
