/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define N1 (19 * 2)

#define VEC_PERM(TYPE)                                                         \
  void __attribute__ ((noinline, noclone))                                     \
  vec_slp_##TYPE (TYPE *restrict a, TYPE *restrict b)                          \
  {                                                                            \
    for (int i = 0; i < N1; ++i)                                               \
      {                                                                        \
	a[i] += 1;                                                             \
	b[i * 4] += 2;                                                         \
	b[i * 4 + 1] += 3;                                                     \
	b[i * 4 + 2] += 4;                                                     \
	b[i * 4 + 3] += 5;                                                     \
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

/* This testcase is from aarch64 and floating-point operations are removed.
   TODO: We will add floating-point operations back and make them as common test in the future.  */
