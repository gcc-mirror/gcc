/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_LOOP(TYPE, WTYPE)                                                  \
  void __attribute__ ((noipa))                                                 \
  test_##TYPE##_##WTYPE (TYPE *__restrict a, TYPE *__restrict b,               \
			 TYPE *__restrict c, TYPE *__restrict pred, int n)     \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      c[i] = pred[i]                                                           \
	       ? (TYPE) (((WTYPE) a[i] * (WTYPE) b[i]) >> sizeof (TYPE) * 8)   \
	       : c[i];                                                         \
  }

#define TEST_ALL(T)                                                            \
  T (int8_t, int16_t)                                                          \
  T (int16_t, int32_t)                                                         \
  T (int32_t, int64_t)                                                         \
  T (uint8_t, uint16_t)                                                        \
  T (uint16_t, uint32_t)                                                       \
  T (uint32_t, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tvmulh\.vv\tv[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {\tvmulhu\.vv\tv[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-not {\tvmerge\.vvm\t} } } */
