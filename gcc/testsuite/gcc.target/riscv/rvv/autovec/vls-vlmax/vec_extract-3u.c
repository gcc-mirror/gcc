/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -Wno-pedantic" } */

#include <stdint-gcc.h>

typedef uint64_t vnx8di __attribute__((vector_size (64)));
typedef uint32_t vnx16si __attribute__((vector_size (64)));
typedef uint16_t vnx32hi __attribute__((vector_size (64)));
typedef uint8_t vnx64qi __attribute__((vector_size (64)));

#define VEC_EXTRACT(S,V,IDX)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_##V##_##IDX (V v)			\
  {						\
    return v[IDX];				\
  }

#define VEC_EXTRACT_VAR3(S,V)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_var_##V (V v, int32_t idx)	\
  {						\
    return v[idx];				\
  }

#define TEST_ALL3(T)				\
  T (uint64_t, vnx8di, 0)			\
  T (uint64_t, vnx8di, 2)			\
  T (uint64_t, vnx8di, 4)			\
  T (uint64_t, vnx8di, 6)			\
  T (uint32_t, vnx16si, 0)			\
  T (uint32_t, vnx16si, 2)			\
  T (uint32_t, vnx16si, 6)			\
  T (uint32_t, vnx16si, 8)			\
  T (uint32_t, vnx16si, 14)			\
  T (uint16_t, vnx32hi, 0)			\
  T (uint16_t, vnx32hi, 2)			\
  T (uint16_t, vnx32hi, 14)			\
  T (uint16_t, vnx32hi, 16)			\
  T (uint16_t, vnx32hi, 30)			\
  T (uint8_t, vnx64qi, 0)			\
  T (uint8_t, vnx64qi, 2)			\
  T (uint8_t, vnx64qi, 30)			\
  T (uint8_t, vnx64qi, 32)			\
  T (uint8_t, vnx64qi, 63)			\

#define TEST_ALL_VAR3(T)			\
  T (uint64_t, vnx8di)				\
  T (uint32_t, vnx16si)				\
  T (uint16_t, vnx32hi)				\
  T (uint8_t, vnx64qi)				\

TEST_ALL3 (VEC_EXTRACT)
TEST_ALL_VAR3 (VEC_EXTRACT_VAR3)

/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e8,\s*m4,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e16,\s*m4,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e32,\s*m4,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e64,\s*m4,\s*ta,\s*ma} 5 } } */

/* { dg-final { scan-assembler-times {\tvslidedown.vi} 13 } } */
/* { dg-final { scan-assembler-times {\tvslidedown.vx} 6 } } */

/* { dg-final { scan-assembler-times {\tvmv.x.s} 23 } } */

/* { dg-final { scan-assembler-times {\tandi\ta0,a0,0xff} 6 } } */
/* { dg-final { scan-assembler-times {\tslli\ta0,a0,48} 6 } } */
/* { dg-final { scan-assembler-times {\tsrli\ta0,a0,48} 6 } } */
