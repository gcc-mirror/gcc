/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2 -fno-ipa-icf" } */

#include "arm_mve.h"
int8x16_t value1;
int64x2_t value2;
int32x4_t value3;
uint8x16_t value4;
uint16x8_t value5;
uint64x2_t value6;
uint32x4_t value7;
int16x8_t value8;
float32x4_t value9;

float16x8_t
foo ()
{
  float16x8_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_f16 (vreinterpretq_f16_s8 (value1), vreinterpretq_f16_s64 (value2));
  r2 = vaddq_f16 (r1, vreinterpretq_f16_s32 (value3));
  r3 = vaddq_f16 (r2, vreinterpretq_f16_u8 (value4));
  r4 = vaddq_f16 (r3, vreinterpretq_f16_u16 (value5));
  r5 = vaddq_f16 (r4, vreinterpretq_f16_u64 (value6));
  r6 = vaddq_f16 (r5, vreinterpretq_f16_u32 (value7));
  r7 = vaddq_f16 (r6, vreinterpretq_f16_s16 (value8));
  return vaddq_f16 (r7, vreinterpretq_f16_f32 (value9));
}

float16x8_t
foo1 ()
{
  float16x8_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_f16 (vreinterpretq_f16 (value1), vreinterpretq_f16 (value2));
  r2 = vaddq_f16 (r1, vreinterpretq_f16 (value3));
  r3 = vaddq_f16 (r2, vreinterpretq_f16 (value4));
  r4 = vaddq_f16 (r3, vreinterpretq_f16 (value5));
  r5 = vaddq_f16 (r4, vreinterpretq_f16 (value6));
  r6 = vaddq_f16 (r5, vreinterpretq_f16 (value7));
  r7 = vaddq_f16 (r6, vreinterpretq_f16 (value8));
  return vaddq_f16 (r7, vreinterpretq_f16 (value9));
}

/* { dg-final { scan-assembler-times "vadd.f16" 16 } } */
