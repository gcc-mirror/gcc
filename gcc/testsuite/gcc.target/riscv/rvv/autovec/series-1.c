/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl -mrvv-max-lmul=m4" } */

#include <stdint-gcc.h>

#define NUM_ELEMS(TYPE) (64 / sizeof (TYPE))

#define DEF_LOOP(TYPE, BASE, STEP, SUFFIX)                                     \
  void __attribute__ ((noinline, noclone))                                     \
  loop_##TYPE##_##SUFFIX (TYPE *restrict a)                                    \
  {                                                                            \
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)                                 \
      a[i] = (BASE) + i * (STEP);                                              \
  }

#define TEST_SEW32_TYPES(T, BASE, STEP, SUFFIX)                                \
  T (uint32_t, BASE, STEP, SUFFIX)                                             \
  T (int32_t, BASE, STEP, SUFFIX)

#define TEST_ALL(T)                                                            \
  TEST_SEW32_TYPES (T, 0, 1, b0s1)                                             \
  TEST_SEW32_TYPES (T, 0, 2, b0s2)                                             \
  TEST_SEW32_TYPES (T, 0, 3, b0s3)                                             \
  TEST_SEW32_TYPES (T, 0, 8, b0s8)                                             \
  TEST_SEW32_TYPES (T, 0, 9, b0s9)                                             \
  TEST_SEW32_TYPES (T, 0, 16, b0s16)                                           \
  TEST_SEW32_TYPES (T, 0, 17, b0s17)                                           \
  TEST_SEW32_TYPES (T, 0, 32, b0s32)                                           \
  TEST_SEW32_TYPES (T, 0, 33, b0s33)                                           \
  TEST_SEW32_TYPES (T, -16, 1, bm16s1)                                         \
  TEST_SEW32_TYPES (T, 15, 1, b15s1)                                           \
  TEST_SEW32_TYPES (T, -17, 1, bm17s1)                                         \
  TEST_SEW32_TYPES (T, 16, 1, b16s1)                                           \
  TEST_SEW32_TYPES (T, -16, 128, bm16s128)                                     \
  TEST_SEW32_TYPES (T, 15, 128, b15s128)                                       \
  TEST_SEW32_TYPES (T, -17, 128, bm17s128)                                     \
  TEST_SEW32_TYPES (T, 16, 128, b16s128)                                       \
  TEST_SEW32_TYPES (T, -16, 179, bm16s179)                                     \
  TEST_SEW32_TYPES (T, 15, 179, b15s179)                                       \
  TEST_SEW32_TYPES (T, -17, 179, bm17s179)                                     \
  TEST_SEW32_TYPES (T, 16, 179, b16s179)                                       \
  TEST_SEW32_TYPES (T, -16, 65536, bm16s65536)                                 \
  TEST_SEW32_TYPES (T, 15, 65536, b15s65536)                                   \
  TEST_SEW32_TYPES (T, -17, 65536, bm17s65536)                                 \
  TEST_SEW32_TYPES (T, 16, 65536, b16s65536)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vid\.v\s+v[0-9]+} 50 } } */
/* { dg-final { scan-assembler-times {vsll\.vi\s+v[0-9]+} 24 } } */
