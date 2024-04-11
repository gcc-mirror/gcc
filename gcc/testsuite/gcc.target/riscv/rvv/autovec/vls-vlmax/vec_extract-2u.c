/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -Wno-pedantic" } */

#include <stdint-gcc.h>

typedef uint64_t vnx4di __attribute__((vector_size (32)));
typedef uint32_t vnx8si __attribute__((vector_size (32)));
typedef uint16_t vnx16hi __attribute__((vector_size (32)));
typedef uint8_t vnx32qi __attribute__((vector_size (32)));

#define VEC_EXTRACT(S,V,IDX)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_##V##_##IDX (V v)			\
  {						\
    return v[IDX];				\
  }

#define VEC_EXTRACT_VAR2(S,V)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_var_##V (V v, int16_t idx)	\
  {						\
    return v[idx];				\
  }

#define TEST_ALL2(T)				\
  T (uint64_t, vnx4di, 0)			\
  T (uint64_t, vnx4di, 1)			\
  T (uint64_t, vnx4di, 2)			\
  T (uint64_t, vnx4di, 3)			\
  T (uint32_t, vnx8si, 0)			\
  T (uint32_t, vnx8si, 1)			\
  T (uint32_t, vnx8si, 3)			\
  T (uint32_t, vnx8si, 4)			\
  T (uint32_t, vnx8si, 7)			\
  T (uint16_t, vnx16hi, 0)			\
  T (uint16_t, vnx16hi, 1)			\
  T (uint16_t, vnx16hi, 7)			\
  T (uint16_t, vnx16hi, 8)			\
  T (uint16_t, vnx16hi, 15)			\
  T (uint8_t, vnx32qi, 0)			\
  T (uint8_t, vnx32qi, 1)			\
  T (uint8_t, vnx32qi, 15)			\
  T (uint8_t, vnx32qi, 16)			\
  T (uint8_t, vnx32qi, 31)			\

#define TEST_ALL_VAR2(T)			\
  T (uint64_t, vnx4di)				\
  T (uint32_t, vnx8si)				\
  T (uint16_t, vnx16hi)				\
  T (uint8_t, vnx32qi)				\

TEST_ALL2 (VEC_EXTRACT)
TEST_ALL_VAR2 (VEC_EXTRACT_VAR2)

/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e8,\s*m2,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e16,\s*m2,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e32,\s*m2,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e64,\s*m2,\s*ta,\s*ma} 5 } } */

/* { dg-final { scan-assembler-times {\tvslidedown.vi} 15 } } */
/* { dg-final { scan-assembler-times {\tvslidedown.vx} 4 } } */

/* { dg-final { scan-assembler-times {\tvmv.x.s} 23 } } */

/* { dg-final { scan-assembler-times {\tandi\ta0,a0,0xff} 6 } } */
/* { dg-final { scan-assembler-times {\tslli\ta0,a0,48} 6 } } */
/* { dg-final { scan-assembler-times {\tsrli\ta0,a0,48} 6 } } */
