/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "arm_neon.h"
#include "arm_sve.h"

#define TEST(ret_type, param_type, suffix) \
ret_type test_##suffix(param_type *x) \
{ \
  return svld1rq_##suffix (svptrue_b8 (), &x[0]); \
}

TEST(svint8_t, int8_t, s8)
TEST(svint16_t, int16_t, s16)
TEST(svint32_t, int32_t, s32)
TEST(svint64_t, int64_t, s64)

TEST(svuint8_t, uint8_t, u8)
TEST(svuint16_t, uint16_t, u16)
TEST(svuint32_t, uint32_t, u32)
TEST(svuint64_t, uint64_t, u64)

TEST(svfloat16_t, float16_t, f16)
TEST(svfloat32_t, float32_t, f32)
TEST(svfloat64_t, float64_t, f64)

TEST(svbfloat16_t, bfloat16_t, bf16)

/* { dg-final { scan-assembler-not {\tdup\t} } } */
/* { dg-final { scan-assembler-times {\tld1rq} 12 } } */
