/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define VEC_PERM(TYPE)                                                         \
  TYPE __attribute__ ((noinline, noclone))                                     \
  vec_slp_##TYPE (TYPE *restrict a, int n)                                     \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	a[i * 2] += 10;                                                        \
	a[i * 2 + 1] += 17;                                                    \
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

/* { dg-final { scan-tree-dump-times "{ 10, 17, ... }" 8 "optimized" } } */
/* This testcase is from aarch64 and floating-point operations are removed.
   TODO: We will add floating-point operations back and make them as common test in the future.  */

