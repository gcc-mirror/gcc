/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=128" } */
/* { dg-require-effective-target aarch64_little_endian } */

#include <arm_sve.h>

#define TEST(TYPE, TY, B)				\
  sv##TYPE						\
  ld1_##TY##B (TYPE *x)					\
  {							\
    svbool_t pg = svptrue_b##B ();			\
    return svld1_##TY##B (pg, x);			\
  }							\
							\
  void							\
  st1_##TY##B (TYPE *x, sv##TYPE data)			\
  {							\
    svbool_t pg = svptrue_b##B ();			\
    svst1_##TY##B (pg, x, data);			\
  }							\
							\
  sv##TYPE						\
  ld1_vol_##TY##B (volatile sv##TYPE *ptr)		\
  {							\
    return *ptr;					\
  }							\
							\
  void							\
  st1_vol_##TY##B (volatile sv##TYPE *ptr, sv##TYPE x)	\
  {							\
    *ptr = x;						\
  }

TEST (bfloat16_t, bf, 16)
TEST (float16_t, f, 16)
TEST (float32_t, f, 32)
TEST (float64_t, f, 64)
TEST (int8_t, s, 8)
TEST (int16_t, s, 16)
TEST (int32_t, s, 32)
TEST (int64_t, s, 64)
TEST (uint8_t, u, 8)
TEST (uint16_t, u, 16)
TEST (uint32_t, u, 32)
TEST (uint64_t, u, 64)

/* { dg-final { scan-assembler-times {\tldr\tq0, \[x0\]} 24 } } */
/* { dg-final { scan-assembler-times {\tstr\tq0, \[x0\]} 24 } } */
