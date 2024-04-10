/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_LOOP(TYPE)                                                         \
  void __attribute__ ((noipa)) mod_##TYPE (TYPE *dst, TYPE *src, int count)    \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      dst[i] = src[i] % 19;                                                    \
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

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tvmulh\.vv} 4 } } */
/* { dg-final { scan-assembler-times {\tvmulhu\.vv} 4 } } */
