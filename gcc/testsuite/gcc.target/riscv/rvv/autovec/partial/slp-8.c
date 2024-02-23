/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define VEC_PERM(TYPE)                                                         \
  TYPE __attribute__ ((noinline, noclone))                                     \
  vec_slp_##TYPE (TYPE *restrict a, TYPE b, TYPE c, int n)                     \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	a[i * 2] += b;                                                         \
	a[i * 2 + 1] += c;                                                     \
      }                                                                        \
  }

#define TEST_ALL(T)                                                            \
  T (int8_t)                                                                   \
  T (uint8_t)                                                                  \
  T (int16_t)                                                                  \
  T (uint16_t)                                                                 \
  T (int32_t)                                                                  \
  T (uint32_t)                                                                 \
  T (int64_t)                                                                  \
  T (uint64_t)

TEST_ALL (VEC_PERM)

/* { dg-final { scan-tree-dump-times "\.VEC_PERM" 2 "optimized" } } */
/* { dg-final { scan-assembler-times {viota.m} 2 } } */
