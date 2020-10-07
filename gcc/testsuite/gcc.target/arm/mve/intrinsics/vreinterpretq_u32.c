/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2 -fno-ipa-icf" } */

#include "arm_mve.h"
int16x8_t value1;
int64x2_t value2;
int8x16_t value3;
uint8x16_t value4;
uint16x8_t value5;
uint64x2_t value6;
int32x4_t value7;
float16x8_t value8;
float32x4_t value9;

uint32x4_t
foo ()
{
  uint32x4_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_u32 (vreinterpretq_u32_s16 (value1), vreinterpretq_u32_s64 (value2));
  r2 = vaddq_u32 (r1, vreinterpretq_u32_s8 (value3));
  r3 = vaddq_u32 (r2, vreinterpretq_u32_u8 (value4));
  r4 = vaddq_u32 (r3, vreinterpretq_u32_u16 (value5));
  r5 = vaddq_u32 (r4, vreinterpretq_u32_u64 (value6));
  r6 = vaddq_u32 (r5, vreinterpretq_u32_s32 (value7));
  r7 = vaddq_u32 (r6, vreinterpretq_u32_f16 (value8));
  return vaddq_u32 (r7, vreinterpretq_u32_f32 (value9));
}

uint32x4_t
foo1 ()
{
  uint32x4_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_u32 (vreinterpretq_u32 (value1), vreinterpretq_u32 (value2));
  r2 = vaddq_u32 (r1, vreinterpretq_u32 (value3));
  r3 = vaddq_u32 (r2, vreinterpretq_u32 (value4));
  r4 = vaddq_u32 (r3, vreinterpretq_u32 (value5));
  r5 = vaddq_u32 (r4, vreinterpretq_u32 (value6));
  r6 = vaddq_u32 (r5, vreinterpretq_u32 (value7));
  r7 = vaddq_u32 (r6, vreinterpretq_u32 (value8));
  return vaddq_u32 (r7, vreinterpretq_u32 (value9));
}

/* { dg-final { scan-assembler-times "vadd.i32" 16 } } */
