/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "arm_neon.h"
#include "arm_sve.h"

#define TEST(ret_type, param_type, suffix) \
ret_type test_##suffix(param_type x) \
{ \
  return svld1rq_##suffix (svptrue_b8 (), &x[0]); \
}

TEST(svint8_t, int8x16_t, s8)
TEST(svint16_t, int16x8_t, s16)
TEST(svint32_t, int32x4_t, s32)
TEST(svint64_t, int64x2_t, s64)

TEST(svuint8_t, uint8x16_t, u8)
TEST(svuint16_t, uint16x8_t, u16)
TEST(svuint32_t, uint32x4_t, u32)
TEST(svuint64_t, uint64x2_t, u64)

TEST(svfloat16_t, float16x8_t, f16)
TEST(svfloat32_t, float32x4_t, f32)
TEST(svfloat64_t, float64x2_t, f64)

TEST(svbfloat16_t, bfloat16x8_t, bf16)

/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.q, z[0-9]+\.q\[0\]} 12 { target aarch64_little_endian } } } */
