/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -Wno-pedantic" } */

#include <stdint-gcc.h>

typedef uint64_t vnx16di __attribute__((vector_size (128)));
typedef uint32_t vnx32si __attribute__((vector_size (128)));
typedef uint16_t vnx64hi __attribute__((vector_size (128)));
typedef uint8_t vnx128qi __attribute__((vector_size (128)));

#define VEC_EXTRACT(S,V,IDX)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_##V##_##IDX (V v)			\
  {						\
    return v[IDX];				\
  }

#define VEC_EXTRACT_VAR4(S,V)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_var_##V (V v, int64_t idx)	\
  {						\
    return v[idx];				\
  }

#define TEST_ALL4(T)				\
  T (uint64_t, vnx16di, 0)			\
  T (uint64_t, vnx16di, 4)			\
  T (uint64_t, vnx16di, 8)			\
  T (uint64_t, vnx16di, 12)			\
  T (uint32_t, vnx32si, 0)			\
  T (uint32_t, vnx32si, 4)			\
  T (uint32_t, vnx32si, 12)			\
  T (uint32_t, vnx32si, 16)			\
  T (uint32_t, vnx32si, 28)			\
  T (uint16_t, vnx64hi, 0)			\
  T (uint16_t, vnx64hi, 4)			\
  T (uint16_t, vnx64hi, 28)			\
  T (uint16_t, vnx64hi, 32)			\
  T (uint16_t, vnx64hi, 60)			\
  T (uint8_t, vnx128qi, 0)			\
  T (uint8_t, vnx128qi, 4)			\
  T (uint8_t, vnx128qi, 30)			\
  T (uint8_t, vnx128qi, 60)			\
  T (uint8_t, vnx128qi, 64)			\
  T (uint8_t, vnx128qi, 127)			\

#define TEST_ALL_VAR4(T)			\
  T (uint64_t, vnx16di)				\
  T (uint32_t, vnx32si)				\
  T (uint16_t, vnx64hi)				\
  T (uint8_t, vnx128qi)				\

TEST_ALL4 (VEC_EXTRACT)
TEST_ALL_VAR4 (VEC_EXTRACT_VAR4)

/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e8,\s*m8,\s*ta,\s*ma} 7 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e16,\s*m8,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e32,\s*m8,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e64,\s*m8,\s*ta,\s*ma} 5 } } */

/* { dg-final { scan-assembler-times {\tvslidedown.vi} 11 } } */
/* { dg-final { scan-assembler-times {\tvslidedown.vx} 9 } } */

/* { dg-final { scan-assembler-times {\tvmv.x.s} 24 } } */

/* { dg-final { scan-assembler-times {\tandi\ta0,a0,0xff} 7 } } */
/* { dg-final { scan-assembler-times {\tslli\ta0,a0,48} 6 } } */
/* { dg-final { scan-assembler-times {\tsrli\ta0,a0,48} 6 } } */
