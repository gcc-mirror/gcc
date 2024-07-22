/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh -mabi=lp64d -Wno-pedantic" } */

#include <stdint-gcc.h>

typedef int64_t vnx2di __attribute__((vector_size (16)));
typedef int32_t vnx4si __attribute__((vector_size (16)));
typedef int16_t vnx8hi __attribute__((vector_size (16)));
typedef int8_t vnx16qi __attribute__((vector_size (16)));
typedef _Float16 vnx8hf __attribute__((vector_size (16)));
typedef float vnx4sf __attribute__((vector_size (16)));
typedef double vnx2df __attribute__((vector_size (16)));

#define VEC_SET(S,V,IDX)			\
  V						\
  __attribute__((noipa))			\
  vec_set_##V##_##IDX (V v, S s)		\
  {						\
    v[IDX] = s;					\
    return v;					\
  }

#define VEC_SET_VAR1(S,V)			\
  V						\
  __attribute__((noipa))			\
  vec_set_var_##V (V v, int8_t idx, S s)	\
  {						\
    v[idx] = s;					\
    return v;					\
  }						\

#define TEST_ALL1(T)				\
  T (_Float16, vnx8hf, 0)			\
  T (_Float16, vnx8hf, 3)			\
  T (_Float16, vnx8hf, 7)			\
  T (float, vnx4sf, 0)				\
  T (float, vnx4sf, 1)				\
  T (float, vnx4sf, 3)				\
  T (double, vnx2df, 0)				\
  T (double, vnx2df, 1)				\
  T (int64_t, vnx2di, 0)			\
  T (int64_t, vnx2di, 1)			\
  T (int32_t, vnx4si, 0)			\
  T (int32_t, vnx4si, 1)			\
  T (int32_t, vnx4si, 3)			\
  T (int16_t, vnx8hi, 0)			\
  T (int16_t, vnx8hi, 2)			\
  T (int16_t, vnx8hi, 6)			\
  T (int8_t, vnx16qi, 0)			\
  T (int8_t, vnx16qi, 1)			\
  T (int8_t, vnx16qi, 7)			\
  T (int8_t, vnx16qi, 11)			\
  T (int8_t, vnx16qi, 15)			\

#define TEST_ALL_VAR1(T)			\
  T (_Float16, vnx8hf)				\
  T (float, vnx4sf)				\
  T (double, vnx2df)				\
  T (int64_t, vnx2di)				\
  T (int32_t, vnx4si)				\
  T (int16_t, vnx8hi)				\
  T (int8_t, vnx16qi)				\

TEST_ALL1 (VEC_SET)
TEST_ALL_VAR1 (VEC_SET_VAR1)

/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e8,\s*m1,\s*tu,\s*ma} 6 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e16,\s*m1,\s*tu,\s*ma} 8 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e32,\s*m1,\s*tu,\s*ma} 8 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e64,\s*m1,\s*tu,\s*ma} 6 } } */

/* { dg-final { scan-assembler-times {\tvmv.v.x} 13 } } */
/* { dg-final { scan-assembler-times {\tvfmv.v.f} 8 } } */
/* { dg-final { scan-assembler-times {\tvslideup.vi} 14 } } */
/* { dg-final { scan-assembler-times {\tvslideup.vx} 7 } } */

/* { dg-final { scan-assembler-times {\tvfmv.s.f} 3 } } */
/* { dg-final { scan-assembler-times {\tvmv.s.x} 4 } } */
