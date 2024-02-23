/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_LOOP(TYPE, NAME, OP)                                               \
  void __attribute__ ((noipa))                                                 \
  test_##TYPE##_##NAME (TYPE *__restrict r, TYPE *__restrict a,                \
			TYPE *__restrict b, TYPE c, TYPE *__restrict pred,     \
			TYPE *__restrict merged, int n)                        \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      r[i] = pred[i] == 1 ? a[i] OP b[i] * c : merged[i];                      \
  }

#define TEST_TYPE(T, TYPE)                                                     \
  T (TYPE, add, +)                                                             \
  T (TYPE, sub, -)

#define TEST_ALL(T)                                                            \
  TEST_TYPE (T, uint8_t)                                                       \
  TEST_TYPE (T, uint16_t)                                                      \
  TEST_TYPE (T, uint32_t)                                                      \
  TEST_TYPE (T, uint64_t)                                                      \
  TEST_TYPE (T, _Float16)                                                      \
  TEST_TYPE (T, float)                                                         \
  TEST_TYPE (T, double)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vmacc\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vnmsac\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vfmacc\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vfnmsac\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* NOTE: 14 vmerge is need for other purpose.  */
/* { dg-final { scan-assembler-times {\tvf?merge\.v[vxi]m\t} 14 } } */
