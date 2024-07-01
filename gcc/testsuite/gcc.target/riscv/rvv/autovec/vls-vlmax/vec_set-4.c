/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh -mabi=lp64d -Wno-pedantic" } */

#include <stdint-gcc.h>

typedef int64_t vnx16di __attribute__((vector_size (128)));
typedef int32_t vnx32si __attribute__((vector_size (128)));
typedef int16_t vnx64hi __attribute__((vector_size (128)));
typedef int8_t vnx128qi __attribute__((vector_size (128)));
typedef _Float16 vnx64hf __attribute__((vector_size (128)));
typedef float vnx32sf __attribute__((vector_size (128)));
typedef double vnx16df __attribute__((vector_size (128)));

#define VEC_SET(S,V,IDX)			\
  V						\
  __attribute__((noipa))			\
  vec_set_##V##_##IDX (V v, S s)		\
  {						\
    v[IDX] = s;					\
    return v;					\
  }

#define VEC_SET_VAR4(S,V)			\
  V						\
  __attribute__((noipa))			\
  vec_set_var_##V (V v, int64_t idx, S s)	\
  {						\
    v[idx] = s;					\
    return v;					\
  }						\

#define TEST_ALL4(T)				\
  T (_Float16, vnx64hf, 0)			\
  T (_Float16, vnx64hf, 3)			\
  T (_Float16, vnx64hf, 7)			\
  T (_Float16, vnx64hf, 8)			\
  T (_Float16, vnx64hf, 16)			\
  T (_Float16, vnx64hf, 31)			\
  T (_Float16, vnx64hf, 42)			\
  T (_Float16, vnx64hf, 63)			\
  T (float, vnx32sf, 0)				\
  T (float, vnx32sf, 3)				\
  T (float, vnx32sf, 12)			\
  T (float, vnx32sf, 17)			\
  T (float, vnx32sf, 14)			\
  T (double, vnx16df, 0)			\
  T (double, vnx16df, 4)			\
  T (double, vnx16df, 8)			\
  T (double, vnx16df, 12)			\
  T (int64_t, vnx16di, 0)			\
  T (int64_t, vnx16di, 4)			\
  T (int64_t, vnx16di, 8)			\
  T (int64_t, vnx16di, 12)			\
  T (int32_t, vnx32si, 0)			\
  T (int32_t, vnx32si, 4)			\
  T (int32_t, vnx32si, 12)			\
  T (int32_t, vnx32si, 16)			\
  T (int32_t, vnx32si, 28)			\
  T (int16_t, vnx64hi, 0)			\
  T (int16_t, vnx64hi, 4)			\
  T (int16_t, vnx64hi, 28)			\
  T (int16_t, vnx64hi, 32)			\
  T (int16_t, vnx64hi, 60)			\
  T (int8_t, vnx128qi, 0)			\
  T (int8_t, vnx128qi, 4)			\
  T (int8_t, vnx128qi, 30)			\
  T (int8_t, vnx128qi, 60)			\
  T (int8_t, vnx128qi, 64)			\
  T (int8_t, vnx128qi, 127)			\

#define TEST_ALL_VAR4(T)			\
  T (_Float16, vnx64hf)				\
  T (float, vnx32sf)				\
  T (double, vnx16df)				\
  T (int64_t, vnx16di)				\
  T (int32_t, vnx32si)				\
  T (int16_t, vnx64hi)				\
  T (int8_t, vnx128qi)				\

TEST_ALL4 (VEC_SET)
TEST_ALL_VAR4 (VEC_SET_VAR4)

/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e8,\s*m8,\s*tu,\s*ma} 7 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e16,\s*m8,\s*tu,\s*ma} 15 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e32,\s*m8,\s*tu,\s*ma} 12 } } */
/* { dg-final { scan-assembler-times {vset[i]*vli\s+[a-z0-9,]+,\s*e64,\s*m8,\s*tu,\s*ma} 10 } } */

/* { dg-final { scan-assembler-times {\tvmv.v.x} 20 } } */
/* { dg-final { scan-assembler-times {\tvfmv.v.f} 17 } } */
/* { dg-final { scan-assembler-times {\tvslideup.vi} 23 } } */
/* { dg-final { scan-assembler-times {\tvslideup.vx} 14 } } */

/* { dg-final { scan-assembler-times {\tvfmv.s.f} 3 } } */
/* { dg-final { scan-assembler-times {\tvmv.s.x} 4 } } */
