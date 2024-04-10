/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -fno-vect-cost-model -mrvv-vector-bits=scalable -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define EXTRACT_LAST(TYPE)                                                     \
  _Bool __attribute__ ((noipa))                                                \
  test_##TYPE (TYPE *restrict x, TYPE *restrict y, int n)		       \
  {                                                                            \
    _Bool last;                                                                \
    for (int j = 0; j < n; ++j)                                                \
      {                                                                        \
	last = !x[j];							       \
	y[j] = last;                                                           \
      }                                                                        \
    return last;                                                               \
  }

#define TEST_ALL(T)                                                            \
  T (int8_t)                                                                   \
  T (int16_t)                                                                  \
  T (int32_t)                                                                  \
  T (int64_t)                                                                  \
  T (uint8_t)                                                                  \
  T (uint16_t)                                                                 \
  T (uint32_t)                                                                 \
  T (uint64_t)

TEST_ALL (EXTRACT_LAST)

/* { dg-final { scan-tree-dump-times "\.VEC_EXTRACT" 8 "optimized" } } */
