/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#include <arm_sve.h>

#define TEST(TYPE, TY, B)			\
  sv##TYPE ld_##TY (TYPE *x)			\
  {						\
    return svld1_##TY(svptrue_b##B (), x);	\
  }						\
  void st_##TY (TYPE *x, sv##TYPE data)		\
  {						\
    svst1_##TY(svptrue_b##B (), x, data);	\
  }

TEST(bfloat16_t, bf16, 16)
TEST(float16_t, f16, 16)
TEST(float32_t, f32, 32)
TEST(float64_t, f64, 64)
TEST(uint8_t, u8, 8)
TEST(uint16_t, u16, 16)
TEST(uint32_t, u32, 32)
TEST(uint64_t, u64, 64)
TEST(int8_t, s8, 8)
TEST(int16_t, s16, 16)
TEST(int32_t, s32, 32)
TEST(int64_t, s64, 64)

/* { dg-final { scan-assembler-times {\tldr\tz0, \[x0\]} 12 } } */
/* { dg-final { scan-assembler-times {\tstr\tz0, \[x0\]} 12 } } */