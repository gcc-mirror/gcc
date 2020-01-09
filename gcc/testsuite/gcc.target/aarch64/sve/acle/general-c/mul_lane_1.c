#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f2 (svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64)
{
  s8 = svmul_lane (s8, s8, 1); /* { dg-error {'svmul_lane' has no form that takes 'svint8_t' arguments} } */
  u8 = svmul_lane (u8, u8, 1); /* { dg-error {'svmul_lane' has no form that takes 'svuint8_t' arguments} } */
  s16 = svmul_lane (s16, s16, 1);
  u16 = svmul_lane (u16, u16, 1);
  f16 = svmul_lane (f16, f16, 1);
  s32 = svmul_lane (s32, s32, 1);
  u32 = svmul_lane (u32, u32, 1);
  f32 = svmul_lane (f32, f32, 1);
  s64 = svmul_lane (s64, s64, 1);
  u64 = svmul_lane (u64, u64, 1);
  f64 = svmul_lane (f64, f64, 1);
}

#pragma GCC target ("arch=armv8-a+sve")

void
f1 (svint8_t s8, svuint8_t u8, svint16_t s16)
{
  s8 = svmul_lane (s8, s8, 1); /* { dg-error {'svmul_lane' has no form that takes 'svint8_t' arguments} } */
  u8 = svmul_lane (u8, u8, 1); /* { dg-error {'svmul_lane' has no form that takes 'svuint8_t' arguments} } */
  s16 = svmul_lane (s16, s16, 1); /* { dg-error {ACLE function 'svmul_lane_s16' requires ISA extension 'sve2'} } */
}
