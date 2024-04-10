/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -fno-vect-cost-model -mrvv-vector-bits=scalable -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define EXTRACT_LAST(TYPE)                                                     \
  TYPE __attribute__ ((noinline, noclone))                                     \
  test_##TYPE (TYPE *x, int n, TYPE value)                                     \
  {                                                                            \
    TYPE last;                                                                 \
    for (int j = 0; j < n; ++j)                                                \
      {                                                                        \
	last = x[j];                                                           \
	x[j] = last * value;                                                   \
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
  T (uint64_t)                                                                 \
  T (_Float16)                                                                 \
  T (float)                                                                    \
  T (double)

TEST_ALL (EXTRACT_LAST)

/* { dg-final { scan-tree-dump-times "\.VEC_EXTRACT" 11 "optimized" } } */
