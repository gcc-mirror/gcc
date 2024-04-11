/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -Wno-pedantic" } */

#include <stdint-gcc.h>

typedef uint64_t vnx2di __attribute__((vector_size (16)));
typedef uint32_t vnx4si __attribute__((vector_size (16)));
typedef uint16_t vnx8hi __attribute__((vector_size (16)));
typedef uint8_t vnx16qi __attribute__((vector_size (16)));

#define VEC_EXTRACT(S,V,IDX)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_##V##_##IDX (V v)			\
  {						\
    return v[IDX];				\
  }

#define VEC_EXTRACT_VAR1(S,V)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_var_##V (V v, int8_t idx)		\
  {						\
    return v[idx];				\
  }

#define TEST_ALL1(T)				\
  T (uint64_t, vnx2di, 0)			\
  T (uint64_t, vnx2di, 1)			\
  T (uint32_t, vnx4si, 0)			\
  T (uint32_t, vnx4si, 1)			\
  T (uint32_t, vnx4si, 3)			\
  T (uint16_t, vnx8hi, 0)			\
  T (uint16_t, vnx8hi, 2)			\
  T (uint16_t, vnx8hi, 6)			\
  T (uint8_t, vnx16qi, 0)			\
  T (uint8_t, vnx16qi, 1)			\
  T (uint8_t, vnx16qi, 7)			\
  T (uint8_t, vnx16qi, 11)			\
  T (uint8_t, vnx16qi, 15)			\

#define TEST_ALL_VAR1(T)			\
  T (uint64_t, vnx2di)				\
  T (uint32_t, vnx4si)				\
  T (uint16_t, vnx8hi)				\
  T (uint8_t, vnx16qi)				\

TEST_ALL1 (VEC_EXTRACT)
TEST_ALL_VAR1 (VEC_EXTRACT_VAR1)

/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e8,\s*m1,\s*ta,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e16,\s*m1,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e32,\s*m1,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e64,\s*m1,\s*ta,\s*ma} 3 } } */

/* { dg-final { scan-assembler-times {\tvslidedown.vi} 9 } } */
/* { dg-final { scan-assembler-times {\tvslidedown.vx} 4 } } */

/* { dg-final { scan-assembler-times {\tvmv.x.s} 17 } } */

/* { dg-final { scan-assembler-times {\tandi\ta0,a0,0xff} 6 } } */
/* { dg-final { scan-assembler-times {\tslli\ta0,a0,48} 4 } } */
/* { dg-final { scan-assembler-times {\tsrli\ta0,a0,48} 4 } } */
