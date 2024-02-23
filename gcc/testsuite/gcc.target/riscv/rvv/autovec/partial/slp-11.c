/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define VEC_PERM(TYPE)                                                         \
  TYPE __attribute__ ((noinline, noclone))                                     \
  vec_slp_##TYPE (TYPE *restrict a, int n)                                     \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	a[i * 4] += 41;                                                        \
	a[i * 4 + 1] += 25;                                                    \
	a[i * 4 + 2] += 31;                                                    \
	a[i * 4 + 3] += 62;                                                    \
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

/* { dg-final { scan-tree-dump "{ 41, 25, 31, 62, ... }" "optimized" } } */
/* This testcase is from aarch64 and floating-point operations are removed.
   TODO: We will add floating-point operations back and make them as common test in the future.  */
