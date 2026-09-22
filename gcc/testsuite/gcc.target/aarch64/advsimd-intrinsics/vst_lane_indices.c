/* { dg-do compile } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>

void
f_vst2_lane_bf16 (bfloat16_t *p, bfloat16x4x2_t v)
{
  vst2_lane_bf16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst2_lane_bf16', which expects a value in the range \[0, 3\]} } */
  vst2_lane_bf16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_bf16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst2_lane_f16 (float16_t *p, float16x4x2_t v)
{
  vst2_lane_f16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst2_lane_f16', which expects a value in the range \[0, 3\]} } */
  vst2_lane_f16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_f16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst2_lane_f32 (float32_t *p, float32x2x2_t v)
{
  vst2_lane_f32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst2_lane_f32', which expects a value in the range \[0, 1\]} } */
  vst2_lane_f32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_f32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst2_lane_f64 (float64_t *p, float64x1x2_t v)
{
  vst2_lane_f64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst2_lane_f64', which expects the value 0} } */
  vst2_lane_f64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_f64', which expects the value 0} } */
}

void
f_vst2_lane_p8 (poly8_t *p, poly8x8x2_t v)
{
  vst2_lane_p8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst2_lane_p8', which expects a value in the range \[0, 7\]} } */
  vst2_lane_p8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_p8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst2_lane_s8 (int8_t *p, int8x8x2_t v)
{
  vst2_lane_s8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst2_lane_s8', which expects a value in the range \[0, 7\]} } */
  vst2_lane_s8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_s8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst2_lane_s16 (int16_t *p, int16x4x2_t v)
{
  vst2_lane_s16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst2_lane_s16', which expects a value in the range \[0, 3\]} } */
  vst2_lane_s16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_s16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst2_lane_s32 (int32_t *p, int32x2x2_t v)
{
  vst2_lane_s32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst2_lane_s32', which expects a value in the range \[0, 1\]} } */
  vst2_lane_s32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_s32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst2_lane_s64 (int64_t *p, int64x1x2_t v)
{
  vst2_lane_s64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst2_lane_s64', which expects the value 0} } */
  vst2_lane_s64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_s64', which expects the value 0} } */
}

void
f_vst2_lane_u8 (uint8_t *p, uint8x8x2_t v)
{
  vst2_lane_u8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst2_lane_u8', which expects a value in the range \[0, 7\]} } */
  vst2_lane_u8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_u8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst2_lane_u16 (uint16_t *p, uint16x4x2_t v)
{
  vst2_lane_u16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst2_lane_u16', which expects a value in the range \[0, 3\]} } */
  vst2_lane_u16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_u16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst2_lane_u32 (uint32_t *p, uint32x2x2_t v)
{
  vst2_lane_u32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst2_lane_u32', which expects a value in the range \[0, 1\]} } */
  vst2_lane_u32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_u32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst2_lane_u64 (uint64_t *p, uint64x1x2_t v)
{
  vst2_lane_u64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst2_lane_u64', which expects the value 0} } */
  vst2_lane_u64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2_lane_u64', which expects the value 0} } */
}

void
f_vst2q_lane_bf16 (bfloat16_t *p, bfloat16x8x2_t v)
{
  vst2q_lane_bf16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst2q_lane_bf16', which expects a value in the range \[0, 7\]} } */
  vst2q_lane_bf16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_bf16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst2q_lane_f16 (float16_t *p, float16x8x2_t v)
{
  vst2q_lane_f16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst2q_lane_f16', which expects a value in the range \[0, 7\]} } */
  vst2q_lane_f16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_f16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst2q_lane_f32 (float32_t *p, float32x4x2_t v)
{
  vst2q_lane_f32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst2q_lane_f32', which expects a value in the range \[0, 3\]} } */
  vst2q_lane_f32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_f32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst2q_lane_f64 (float64_t *p, float64x2x2_t v)
{
  vst2q_lane_f64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst2q_lane_f64', which expects a value in the range \[0, 1\]} } */
  vst2q_lane_f64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_f64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst2q_lane_p8 (poly8_t *p, poly8x16x2_t v)
{
  vst2q_lane_p8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst2q_lane_p8', which expects a value in the range \[0, 15\]} } */
  vst2q_lane_p8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_p8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst2q_lane_s8 (int8_t *p, int8x16x2_t v)
{
  vst2q_lane_s8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst2q_lane_s8', which expects a value in the range \[0, 15\]} } */
  vst2q_lane_s8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_s8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst2q_lane_s16 (int16_t *p, int16x8x2_t v)
{
  vst2q_lane_s16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst2q_lane_s16', which expects a value in the range \[0, 7\]} } */
  vst2q_lane_s16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_s16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst2q_lane_s32 (int32_t *p, int32x4x2_t v)
{
  vst2q_lane_s32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst2q_lane_s32', which expects a value in the range \[0, 3\]} } */
  vst2q_lane_s32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_s32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst2q_lane_s64 (int64_t *p, int64x2x2_t v)
{
  vst2q_lane_s64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst2q_lane_s64', which expects a value in the range \[0, 1\]} } */
  vst2q_lane_s64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_s64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst2q_lane_u8 (uint8_t *p, uint8x16x2_t v)
{
  vst2q_lane_u8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst2q_lane_u8', which expects a value in the range \[0, 15\]} } */
  vst2q_lane_u8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_u8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst2q_lane_u16 (uint16_t *p, uint16x8x2_t v)
{
  vst2q_lane_u16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst2q_lane_u16', which expects a value in the range \[0, 7\]} } */
  vst2q_lane_u16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_u16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst2q_lane_u32 (uint32_t *p, uint32x4x2_t v)
{
  vst2q_lane_u32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst2q_lane_u32', which expects a value in the range \[0, 3\]} } */
  vst2q_lane_u32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_u32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst2q_lane_u64 (uint64_t *p, uint64x2x2_t v)
{
  vst2q_lane_u64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst2q_lane_u64', which expects a value in the range \[0, 1\]} } */
  vst2q_lane_u64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst2q_lane_u64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst3_lane_bf16 (bfloat16_t *p, bfloat16x4x3_t v)
{
  vst3_lane_bf16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst3_lane_bf16', which expects a value in the range \[0, 3\]} } */
  vst3_lane_bf16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_bf16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst3_lane_f16 (float16_t *p, float16x4x3_t v)
{
  vst3_lane_f16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst3_lane_f16', which expects a value in the range \[0, 3\]} } */
  vst3_lane_f16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_f16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst3_lane_f32 (float32_t *p, float32x2x3_t v)
{
  vst3_lane_f32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst3_lane_f32', which expects a value in the range \[0, 1\]} } */
  vst3_lane_f32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_f32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst3_lane_f64 (float64_t *p, float64x1x3_t v)
{
  vst3_lane_f64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst3_lane_f64', which expects the value 0} } */
  vst3_lane_f64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_f64', which expects the value 0} } */
}

void
f_vst3_lane_p8 (poly8_t *p, poly8x8x3_t v)
{
  vst3_lane_p8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst3_lane_p8', which expects a value in the range \[0, 7\]} } */
  vst3_lane_p8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_p8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst3_lane_s8 (int8_t *p, int8x8x3_t v)
{
  vst3_lane_s8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst3_lane_s8', which expects a value in the range \[0, 7\]} } */
  vst3_lane_s8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_s8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst3_lane_s16 (int16_t *p, int16x4x3_t v)
{
  vst3_lane_s16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst3_lane_s16', which expects a value in the range \[0, 3\]} } */
  vst3_lane_s16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_s16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst3_lane_s32 (int32_t *p, int32x2x3_t v)
{
  vst3_lane_s32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst3_lane_s32', which expects a value in the range \[0, 1\]} } */
  vst3_lane_s32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_s32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst3_lane_s64 (int64_t *p, int64x1x3_t v)
{
  vst3_lane_s64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst3_lane_s64', which expects the value 0} } */
  vst3_lane_s64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_s64', which expects the value 0} } */
}

void
f_vst3_lane_u8 (uint8_t *p, uint8x8x3_t v)
{
  vst3_lane_u8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst3_lane_u8', which expects a value in the range \[0, 7\]} } */
  vst3_lane_u8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_u8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst3_lane_u16 (uint16_t *p, uint16x4x3_t v)
{
  vst3_lane_u16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst3_lane_u16', which expects a value in the range \[0, 3\]} } */
  vst3_lane_u16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_u16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst3_lane_u32 (uint32_t *p, uint32x2x3_t v)
{
  vst3_lane_u32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst3_lane_u32', which expects a value in the range \[0, 1\]} } */
  vst3_lane_u32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_u32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst3_lane_u64 (uint64_t *p, uint64x1x3_t v)
{
  vst3_lane_u64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst3_lane_u64', which expects the value 0} } */
  vst3_lane_u64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3_lane_u64', which expects the value 0} } */
}

void
f_vst3q_lane_bf16 (bfloat16_t *p, bfloat16x8x3_t v)
{
  vst3q_lane_bf16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst3q_lane_bf16', which expects a value in the range \[0, 7\]} } */
  vst3q_lane_bf16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_bf16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst3q_lane_f16 (float16_t *p, float16x8x3_t v)
{
  vst3q_lane_f16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst3q_lane_f16', which expects a value in the range \[0, 7\]} } */
  vst3q_lane_f16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_f16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst3q_lane_f32 (float32_t *p, float32x4x3_t v)
{
  vst3q_lane_f32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst3q_lane_f32', which expects a value in the range \[0, 3\]} } */
  vst3q_lane_f32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_f32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst3q_lane_f64 (float64_t *p, float64x2x3_t v)
{
  vst3q_lane_f64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst3q_lane_f64', which expects a value in the range \[0, 1\]} } */
  vst3q_lane_f64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_f64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst3q_lane_p8 (poly8_t *p, poly8x16x3_t v)
{
  vst3q_lane_p8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst3q_lane_p8', which expects a value in the range \[0, 15\]} } */
  vst3q_lane_p8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_p8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst3q_lane_s8 (int8_t *p, int8x16x3_t v)
{
  vst3q_lane_s8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst3q_lane_s8', which expects a value in the range \[0, 15\]} } */
  vst3q_lane_s8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_s8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst3q_lane_s16 (int16_t *p, int16x8x3_t v)
{
  vst3q_lane_s16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst3q_lane_s16', which expects a value in the range \[0, 7\]} } */
  vst3q_lane_s16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_s16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst3q_lane_s32 (int32_t *p, int32x4x3_t v)
{
  vst3q_lane_s32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst3q_lane_s32', which expects a value in the range \[0, 3\]} } */
  vst3q_lane_s32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_s32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst3q_lane_s64 (int64_t *p, int64x2x3_t v)
{
  vst3q_lane_s64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst3q_lane_s64', which expects a value in the range \[0, 1\]} } */
  vst3q_lane_s64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_s64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst3q_lane_u8 (uint8_t *p, uint8x16x3_t v)
{
  vst3q_lane_u8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst3q_lane_u8', which expects a value in the range \[0, 15\]} } */
  vst3q_lane_u8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_u8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst3q_lane_u16 (uint16_t *p, uint16x8x3_t v)
{
  vst3q_lane_u16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst3q_lane_u16', which expects a value in the range \[0, 7\]} } */
  vst3q_lane_u16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_u16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst3q_lane_u32 (uint32_t *p, uint32x4x3_t v)
{
  vst3q_lane_u32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst3q_lane_u32', which expects a value in the range \[0, 3\]} } */
  vst3q_lane_u32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_u32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst3q_lane_u64 (uint64_t *p, uint64x2x3_t v)
{
  vst3q_lane_u64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst3q_lane_u64', which expects a value in the range \[0, 1\]} } */
  vst3q_lane_u64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst3q_lane_u64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst4_lane_bf16 (bfloat16_t *p, bfloat16x4x4_t v)
{
  vst4_lane_bf16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst4_lane_bf16', which expects a value in the range \[0, 3\]} } */
  vst4_lane_bf16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_bf16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst4_lane_f16 (float16_t *p, float16x4x4_t v)
{
  vst4_lane_f16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst4_lane_f16', which expects a value in the range \[0, 3\]} } */
  vst4_lane_f16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_f16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst4_lane_f32 (float32_t *p, float32x2x4_t v)
{
  vst4_lane_f32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst4_lane_f32', which expects a value in the range \[0, 1\]} } */
  vst4_lane_f32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_f32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst4_lane_f64 (float64_t *p, float64x1x4_t v)
{
  vst4_lane_f64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst4_lane_f64', which expects the value 0} } */
  vst4_lane_f64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_f64', which expects the value 0} } */
}

void
f_vst4_lane_p8 (poly8_t *p, poly8x8x4_t v)
{
  vst4_lane_p8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst4_lane_p8', which expects a value in the range \[0, 7\]} } */
  vst4_lane_p8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_p8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst4_lane_s8 (int8_t *p, int8x8x4_t v)
{
  vst4_lane_s8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst4_lane_s8', which expects a value in the range \[0, 7\]} } */
  vst4_lane_s8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_s8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst4_lane_s16 (int16_t *p, int16x4x4_t v)
{
  vst4_lane_s16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst4_lane_s16', which expects a value in the range \[0, 3\]} } */
  vst4_lane_s16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_s16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst4_lane_s32 (int32_t *p, int32x2x4_t v)
{
  vst4_lane_s32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst4_lane_s32', which expects a value in the range \[0, 1\]} } */
  vst4_lane_s32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_s32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst4_lane_s64 (int64_t *p, int64x1x4_t v)
{
  vst4_lane_s64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst4_lane_s64', which expects the value 0} } */
  vst4_lane_s64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_s64', which expects the value 0} } */
}

void
f_vst4_lane_u8 (uint8_t *p, uint8x8x4_t v)
{
  vst4_lane_u8 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst4_lane_u8', which expects a value in the range \[0, 7\]} } */
  vst4_lane_u8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_u8', which expects a value in the range \[0, 7\]} } */
}

void
f_vst4_lane_u16 (uint16_t *p, uint16x4x4_t v)
{
  vst4_lane_u16 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst4_lane_u16', which expects a value in the range \[0, 3\]} } */
  vst4_lane_u16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_u16', which expects a value in the range \[0, 3\]} } */
}

void
f_vst4_lane_u32 (uint32_t *p, uint32x2x4_t v)
{
  vst4_lane_u32 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst4_lane_u32', which expects a value in the range \[0, 1\]} } */
  vst4_lane_u32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_u32', which expects a value in the range \[0, 1\]} } */
}

void
f_vst4_lane_u64 (uint64_t *p, uint64x1x4_t v)
{
  vst4_lane_u64 (p, v, 1); /* { dg-error {passing 1 to argument 3 of 'vst4_lane_u64', which expects the value 0} } */
  vst4_lane_u64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4_lane_u64', which expects the value 0} } */
}

void
f_vst4q_lane_bf16 (bfloat16_t *p, bfloat16x8x4_t v)
{
  vst4q_lane_bf16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst4q_lane_bf16', which expects a value in the range \[0, 7\]} } */
  vst4q_lane_bf16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_bf16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst4q_lane_f16 (float16_t *p, float16x8x4_t v)
{
  vst4q_lane_f16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst4q_lane_f16', which expects a value in the range \[0, 7\]} } */
  vst4q_lane_f16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_f16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst4q_lane_f32 (float32_t *p, float32x4x4_t v)
{
  vst4q_lane_f32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst4q_lane_f32', which expects a value in the range \[0, 3\]} } */
  vst4q_lane_f32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_f32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst4q_lane_f64 (float64_t *p, float64x2x4_t v)
{
  vst4q_lane_f64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst4q_lane_f64', which expects a value in the range \[0, 1\]} } */
  vst4q_lane_f64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_f64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst4q_lane_p8 (poly8_t *p, poly8x16x4_t v)
{
  vst4q_lane_p8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst4q_lane_p8', which expects a value in the range \[0, 15\]} } */
  vst4q_lane_p8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_p8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst4q_lane_s8 (int8_t *p, int8x16x4_t v)
{
  vst4q_lane_s8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst4q_lane_s8', which expects a value in the range \[0, 15\]} } */
  vst4q_lane_s8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_s8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst4q_lane_s16 (int16_t *p, int16x8x4_t v)
{
  vst4q_lane_s16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst4q_lane_s16', which expects a value in the range \[0, 7\]} } */
  vst4q_lane_s16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_s16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst4q_lane_s32 (int32_t *p, int32x4x4_t v)
{
  vst4q_lane_s32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst4q_lane_s32', which expects a value in the range \[0, 3\]} } */
  vst4q_lane_s32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_s32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst4q_lane_s64 (int64_t *p, int64x2x4_t v)
{
  vst4q_lane_s64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst4q_lane_s64', which expects a value in the range \[0, 1\]} } */
  vst4q_lane_s64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_s64', which expects a value in the range \[0, 1\]} } */
}

void
f_vst4q_lane_u8 (uint8_t *p, uint8x16x4_t v)
{
  vst4q_lane_u8 (p, v, 16); /* { dg-error {passing 16 to argument 3 of 'vst4q_lane_u8', which expects a value in the range \[0, 15\]} } */
  vst4q_lane_u8 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_u8', which expects a value in the range \[0, 15\]} } */
}

void
f_vst4q_lane_u16 (uint16_t *p, uint16x8x4_t v)
{
  vst4q_lane_u16 (p, v, 8); /* { dg-error {passing 8 to argument 3 of 'vst4q_lane_u16', which expects a value in the range \[0, 7\]} } */
  vst4q_lane_u16 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_u16', which expects a value in the range \[0, 7\]} } */
}

void
f_vst4q_lane_u32 (uint32_t *p, uint32x4x4_t v)
{
  vst4q_lane_u32 (p, v, 4); /* { dg-error {passing 4 to argument 3 of 'vst4q_lane_u32', which expects a value in the range \[0, 3\]} } */
  vst4q_lane_u32 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_u32', which expects a value in the range \[0, 3\]} } */
}

void
f_vst4q_lane_u64 (uint64_t *p, uint64x2x4_t v)
{
  vst4q_lane_u64 (p, v, 2); /* { dg-error {passing 2 to argument 3 of 'vst4q_lane_u64', which expects a value in the range \[0, 1\]} } */
  vst4q_lane_u64 (p, v, -1); /* { dg-error {passing -1 to argument 3 of 'vst4q_lane_u64', which expects a value in the range \[0, 1\]} } */
}
