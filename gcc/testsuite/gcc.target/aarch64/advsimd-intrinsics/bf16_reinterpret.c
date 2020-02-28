/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-save-temps" } */

#include <arm_neon.h>

float32x2_t
test_vbfdot_f32_s8 (float32x2_t r, int8x8_t a, int8x8_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_s8(a);
  bfloat16x4_t _b = vreinterpret_bf16_s8(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_s16 (float32x2_t r, int16x4_t a, int16x4_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_s16(a);
  bfloat16x4_t _b = vreinterpret_bf16_s16(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_s32 (float32x2_t r, int32x2_t a, int32x2_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_s32(a);
  bfloat16x4_t _b = vreinterpret_bf16_s32(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_s64 (float32x2_t r, int64x1_t a, int64x1_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_s64(a);
  bfloat16x4_t _b = vreinterpret_bf16_s64(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_u8 (float32x2_t r, uint8x8_t a, uint8x8_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_u8(a);
  bfloat16x4_t _b = vreinterpret_bf16_u8(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_u16 (float32x2_t r, uint16x4_t a, uint16x4_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_u16(a);
  bfloat16x4_t _b = vreinterpret_bf16_u16(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_u32 (float32x2_t r, uint32x2_t a, uint32x2_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_u32(a);
  bfloat16x4_t _b = vreinterpret_bf16_u32(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_u64 (float32x2_t r, uint64x1_t a, uint64x1_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_u64(a);
  bfloat16x4_t _b = vreinterpret_bf16_u64(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_p8 (float32x2_t r, poly8x8_t a, poly8x8_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_p8(a);
  bfloat16x4_t _b = vreinterpret_bf16_p8(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_p16 (float32x2_t r, poly16x4_t a, poly16x4_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_p16(a);
  bfloat16x4_t _b = vreinterpret_bf16_p16(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_p64 (float32x2_t r, poly64x1_t a, poly64x1_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_p64(a);
  bfloat16x4_t _b = vreinterpret_bf16_p64(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_f16(a);
  bfloat16x4_t _b = vreinterpret_bf16_f16(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_f32 (float32x2_t r, float32x2_t a, float32x2_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_f32(a);
  bfloat16x4_t _b = vreinterpret_bf16_f32(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x2_t
test_vbfdot_f32_f64 (float32x2_t r, float64x1_t a, float64x1_t b)
{
  bfloat16x4_t _a = vreinterpret_bf16_f64(a);
  bfloat16x4_t _b = vreinterpret_bf16_f64(b);

  return vbfdot_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_s8 (float32x4_t r, int8x16_t a, int8x16_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_s8(a);
  bfloat16x8_t _b = vreinterpretq_bf16_s8(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_s16 (float32x4_t r, int16x8_t a, int16x8_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_s16(a);
  bfloat16x8_t _b = vreinterpretq_bf16_s16(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_s32 (float32x4_t r, int32x4_t a, int32x4_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_s32(a);
  bfloat16x8_t _b = vreinterpretq_bf16_s32(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_s64 (float32x4_t r, int64x2_t a, int64x2_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_s64(a);
  bfloat16x8_t _b = vreinterpretq_bf16_s64(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_u8 (float32x4_t r, uint8x16_t a, uint8x16_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_u8(a);
  bfloat16x8_t _b = vreinterpretq_bf16_u8(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_u16 (float32x4_t r, uint16x8_t a, uint16x8_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_u16(a);
  bfloat16x8_t _b = vreinterpretq_bf16_u16(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_u32 (float32x4_t r, uint32x4_t a, uint32x4_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_u32(a);
  bfloat16x8_t _b = vreinterpretq_bf16_u32(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_u64 (float32x4_t r, uint64x2_t a, uint64x2_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_u64(a);
  bfloat16x8_t _b = vreinterpretq_bf16_u64(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_p8 (float32x4_t r, poly8x16_t a, poly8x16_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_p8(a);
  bfloat16x8_t _b = vreinterpretq_bf16_p8(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_p16 (float32x4_t r, poly16x8_t a, poly16x8_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_p16(a);
  bfloat16x8_t _b = vreinterpretq_bf16_p16(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_p64 (float32x4_t r, poly64x2_t a, poly64x2_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_p64(a);
  bfloat16x8_t _b = vreinterpretq_bf16_p64(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_p128 (float32x4_t r, poly128_t a, poly128_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_p128(a);
  bfloat16x8_t _b = vreinterpretq_bf16_p128(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_f16(a);
  bfloat16x8_t _b = vreinterpretq_bf16_f16(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_f32 (float32x4_t r, float32x4_t a, float32x4_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_f32(a);
  bfloat16x8_t _b = vreinterpretq_bf16_f32(b);

  return vbfdotq_f32 (r, _a, _b);
}

float32x4_t
test_vbfdotq_f32_f64 (float32x4_t r, float64x2_t a, float64x2_t b)
{
  bfloat16x8_t _a = vreinterpretq_bf16_f64(a);
  bfloat16x8_t _b = vreinterpretq_bf16_f64(b);

  return vbfdotq_f32 (r, _a, _b);
}

/* { dg-final { scan-assembler-times {bfdot\tv[0-9]+.2s, v[0-9]+.4h, v[0-9]+.4h} 14 } } */
/* { dg-final { scan-assembler-times {bfdot\tv[0-9]+.4s, v[0-9]+.8h, v[0-9]+.8h} 15 } } */

int8x8_t test_vreinterpret_s8_bf16 (bfloat16x4_t a, int8x8_t b)
{
  int8x8_t _a = vreinterpret_s8_bf16 (a);
  return vadd_s8 (_a, b);
}

int16x4_t test_vreinterpret_s16_bf16 (bfloat16x4_t a, int16x4_t b)
{
  int16x4_t _a = vreinterpret_s16_bf16 (a);
  return vadd_s16 (_a, b);
}

int32x2_t test_vreinterpret_s32_bf16 (bfloat16x4_t a, int32x2_t b)
{
  int32x2_t _a = vreinterpret_s32_bf16 (a);
  return vadd_s32 (_a, b);
}

int64x1_t test_vreinterpret_s64_bf16 (bfloat16x4_t a, int64x1_t b)
{
  int64x1_t _a = vreinterpret_s64_bf16 (a);
  return vrshl_s64 (_a, b);
}

uint8x8_t test_vreinterpret_u8_bf16 (bfloat16x4_t a, uint8x8_t b)
{
  uint8x8_t _a = vreinterpret_u8_bf16 (a);
  return vadd_u8 (_a, b);
}

uint16x4_t test_vreinterpret_u16_bf16 (bfloat16x4_t a, uint16x4_t b)
{
  uint16x4_t _a = vreinterpret_u16_bf16 (a);
  return vadd_u16 (_a, b);
}

uint32x2_t test_vreinterpret_u32_bf16 (bfloat16x4_t a, uint32x2_t b)
{
  uint32x2_t _a = vreinterpret_u32_bf16 (a);
  return vadd_u32 (_a, b);
}

uint64x1_t test_vreinterpret_u64_bf16 (bfloat16x4_t a, int64x1_t b)
{
  uint64x1_t _a = vreinterpret_u64_bf16 (a);
  return vrshl_u64 (_a, b);
}

poly8x8_t test_vreinterpret_p8_bf16 (bfloat16x4_t a, poly8x8_t b)
{
  poly8x8_t _a = vreinterpret_p8_bf16 (a);
  return vzip1_p8 (_a, b);
}

poly16x4_t test_vreinterpret_p16_bf16 (bfloat16x4_t a, poly16x4_t b)
{
  poly16x4_t _a = vreinterpret_p16_bf16 (a);
  return vzip1_p16 (_a, b);
}

poly64x1_t test_vreinterpret_p64_bf16 (bfloat16x4_t a, poly64x1_t b)
{
  poly64x1_t _a = vreinterpret_p64_bf16 (a);
  return vsli_n_p64 (_a, b, 3);
}

float32x2_t test_vreinterpret_f32_bf16 (bfloat16x4_t a, float32x2_t b)
{
  float32x2_t _a = vreinterpret_f32_bf16 (a);
  return vsub_f32 (_a, b);
}

float64x1_t test_vreinterpret_f64_bf16 (bfloat16x4_t a, float64x1_t b)
{
  float64x1_t _a = vreinterpret_f64_bf16 (a);
  return vsub_f64 (_a, b);
}

int8x16_t test_vreinterpretq_s8_bf16 (bfloat16x8_t a, int8x16_t b)
{
  int8x16_t _a = vreinterpretq_s8_bf16 (a);
  return vaddq_s8 (_a, b);
}

int16x8_t test_vreinterpretq_s16_bf16 (bfloat16x8_t a, int16x8_t b)
{
  int16x8_t _a = vreinterpretq_s16_bf16 (a);
  return vaddq_s16 (_a, b);
}

int32x4_t test_vreinterpretq_s32_bf16 (bfloat16x8_t a, int32x4_t b)
{
  int32x4_t _a = vreinterpretq_s32_bf16 (a);
  return vaddq_s32 (_a, b);
}

int64x2_t test_vreinterpretq_s64_bf16 (bfloat16x8_t a, int64x2_t b)
{
  int64x2_t _a = vreinterpretq_s64_bf16 (a);
  return vaddq_s64 (_a, b);
}

uint8x16_t test_vreinterpretq_u8_bf16 (bfloat16x8_t a, uint8x16_t b)
{
  uint8x16_t _a = vreinterpretq_u8_bf16 (a);
  return vaddq_u8 (_a, b);
}

uint16x8_t test_vreinterpretq_u16_bf16 (bfloat16x8_t a, uint16x8_t b)
{
  uint16x8_t _a = vreinterpretq_u16_bf16 (a);
  return vaddq_u16 (_a, b);
}

uint32x4_t test_vreinterpretq_u32_bf16 (bfloat16x8_t a, uint32x4_t b)
{
  uint32x4_t _a = vreinterpretq_u32_bf16 (a);
  return vaddq_u32 (_a, b);
}

uint64x2_t test_vreinterpretq_u64_bf16 (bfloat16x8_t a, uint64x2_t b)
{
  uint64x2_t _a = vreinterpretq_u64_bf16 (a);
  return vaddq_u64 (_a, b);
}

poly8x16_t test_vreinterpretq_p8_bf16 (bfloat16x8_t a, poly8x16_t b)
{
  poly8x16_t _a = vreinterpretq_p8_bf16 (a);
  return vzip1q_p8 (_a, b);
}

poly16x8_t test_vreinterpretq_p16_bf16 (bfloat16x8_t a, poly16x8_t b)
{
  poly16x8_t _a = vreinterpretq_p16_bf16 (a);
  return vzip1q_p16 (_a, b);
}

poly64x2_t test_vreinterpretq_p64_bf16 (bfloat16x8_t a, poly64x2_t b)
{
  poly64x2_t _a = vreinterpretq_p64_bf16 (a);
  return vsliq_n_p64 (_a, b, 3);
}

poly128_t test_vreinterpretq_p128_bf16 (bfloat16x8_t a, poly16x8_t b)
{
  poly128_t _a = vreinterpretq_p128_bf16 (a);
  return _a;
}

float32x4_t test_vreinterpretq_f32_bf16 (bfloat16x8_t a, float32x4_t b)
{
  float32x4_t _a = vreinterpretq_f32_bf16 (a);
  return vsubq_f32 (_a, b);
}

float64x2_t test_vreinterpretq_f64_bf16 (bfloat16x8_t a, float64x2_t b)
{
  float64x2_t _a = vreinterpretq_f64_bf16 (a);
  return vsubq_f64 (_a, b);
}

float16x4_t test_vreinterpret_f16_bf16 (bfloat16x4_t a)
{
  return vreinterpret_f16_bf16 (a);
}

float16x8_t test_vreinterpretq_f16_bf16 (bfloat16x8_t a)
{
  return vreinterpretq_f16_bf16 (a);
}

/* { dg-final { scan-assembler-times {add\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s} 2 } } */
/* { dg-final { scan-assembler-times {add\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h} 2 } } */
/* { dg-final { scan-assembler-times {add\tv[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b} 2 } } */

/* { dg-final { scan-assembler-times {add\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s} 2 } } */
/* { dg-final { scan-assembler-times {add\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h} 2 } } */
/* { dg-final { scan-assembler-times {add\tv[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b} 2 } } */

/* { dg-final { scan-assembler {fsub\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s} } } */
/* { dg-final { scan-assembler {fsub\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s} } } */
/* { dg-final { scan-assembler {fsub\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d} } } */
/* { dg-final { scan-assembler {fsub\td[0-9]+, d[0-9]+, d[0-9]+} } } */

/* { dg-final { scan-assembler {zip1\tv[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b} } } */
/* { dg-final { scan-assembler {zip1\tv[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b} } } */
/* { dg-final { scan-assembler {zip1\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h} } } */
/* { dg-final { scan-assembler {zip1\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h} } } */

/* { dg-final { scan-assembler {sli\tv[0-9]+.2d, v[0-9]+.2d, 3} } } */
/* { dg-final { scan-assembler {sli\td[0-9]+, d[0-9]+, 3} } } */

/* { dg-final { scan-assembler {urshl\td[0-9]+, d[0-9]+, d[0-9]+} } } */
/* { dg-final { scan-assembler {srshl\td[0-9]+, d[0-9]+, d[0-9]+} } } */
