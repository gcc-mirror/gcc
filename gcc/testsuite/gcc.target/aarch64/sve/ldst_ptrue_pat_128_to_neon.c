/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#include <arm_sve.h>

#define TEST(TYPE, TY, W, B)						\
  sv##TYPE								\
  ld1_##TY##W##B##_1 (TYPE *x)						\
  {									\
    svbool_t pg = svwhilelt_b##B (0, W);				\
    return svld1_##TY##B (pg, x);					\
  }									\
  sv##TYPE								\
  ld1_##TY##W##B##_2 (TYPE *x)						\
  {									\
    svbool_t pg = svptrue_pat_b##B ((enum svpattern) (W > 8 ? 9 : W));	\
    return svld1_##TY##B (pg, x);					\
  }									\
  void									\
  st1_##TY##W##B##_1 (TYPE *x, sv##TYPE data)						\
  {									\
    svbool_t pg = svwhilelt_b##B (0, W);				\
    return svst1_##TY##B (pg, x, data);					\
  }									\
  void									\
  st1_##TY##W##B##_2 (TYPE *x, sv##TYPE data)				\
  {									\
    svbool_t pg = svptrue_pat_b##B ((enum svpattern) (W > 8 ? 9 : W));	\
    return svst1_##TY##B (pg, x, data);					\
  }									\

#define TEST64(TYPE, TY, B)				\
  TEST (TYPE, TY, 1, B)					\
  TEST (TYPE, TY, 2, B)					\

#define TEST32(TYPE, TY, B)				\
  TEST64 (TYPE, TY, B)					\
  TEST (TYPE, TY, 4, B)					\

#define TEST16(TYPE, TY, B)				\
  TEST32 (TYPE, TY, B)					\
  TEST (TYPE, TY, 8, B)					\

#define TEST8(TYPE, TY, B)				\
  TEST16 (TYPE, TY, B)					\
  TEST (TYPE, TY, 16, B)

#define T(TYPE, TY, B)			\
  TEST##B (TYPE, TY, B)

T (bfloat16_t, bf, 16)
T (float16_t, f, 16)
T (float32_t, f, 32)
T (float64_t, f, 64)
T (int8_t, s, 8)
T (int16_t, s, 16)
T (int32_t, s, 32)
T (int64_t, s, 64)
T (uint8_t, u, 8)
T (uint16_t, u, 16)
T (uint32_t, u, 32)
T (uint64_t, u, 64)

/* { dg-final { scan-assembler-times {\tldr\tq0, \[x0\]} 24 } } */
/* { dg-final { scan-assembler-times {\tldr\td0, \[x0\]} 24 } } */
/* { dg-final { scan-assembler-times {\tldr\ts0, \[x0\]} 18 } } */
/* { dg-final { scan-assembler-times {\tldr\th0, \[x0\]} 12 } } */
/* { dg-final { scan-assembler-times {\tldr\tb0, \[x0\]} 4 } } */

/* { dg-final { scan-assembler-times {\tstr\tq0, \[x0\]} 24 } } */
/* { dg-final { scan-assembler-times {\tstr\td0, \[x0\]} 24 } } */
/* { dg-final { scan-assembler-times {\tstr\ts0, \[x0\]} 18 } } */
/* { dg-final { scan-assembler-times {\tstr\th0, \[x0\]} 12 } } */
/* { dg-final { scan-assembler-times {\tstr\tb0, \[x0\]} 4 } } */

svint8_t foo (int8_t *x)
{
  return svld1_s8 (svptrue_b16 (), x);
}
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.h, all\n\tld1b} 1 } } */