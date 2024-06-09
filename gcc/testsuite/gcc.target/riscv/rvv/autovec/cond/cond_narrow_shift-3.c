/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_LOOP(TYPE1, TYPE2)                                                 \
  void __attribute__ ((noipa))                                                 \
  test_##TYPE1##_##TYPE2 (TYPE2 *__restrict r, TYPE2 *__restrict a,            \
			  TYPE1 *__restrict b, TYPE2 *__restrict shift, int n) \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      r[i] = a[i] > 20 ? (TYPE2) (b[i] >> shift[i]) : r[i];                    \
  }

#define TEST_ALL(T)                                                            \
  T (int16_t, int8_t)                                                          \
  T (int32_t, int16_t)                                                         \
  T (int64_t, int32_t)                                                         \
  T (uint16_t, uint8_t)                                                        \
  T (uint32_t, uint16_t)                                                       \
  T (uint64_t, uint32_t)

TEST_ALL (DEF_LOOP)

/* For int8_t and uint8_t, they are extended to int32_t and then shifted, so
   int8_t and uint8_t are use vsra.vv on e32 and cannot convert to vnsra.wv
   currently. */
/* { dg-final { scan-assembler-times {\tvnsrl\.wv\t} 2 } } */
/* { dg-final { scan-assembler-times {\tvnsra\.wv\t} 2 } } */
/* { dg-final { scan-assembler-not {\tvmerge\.vvm\t} } } */
