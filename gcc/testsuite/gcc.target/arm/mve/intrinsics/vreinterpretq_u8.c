/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
int16x8_t value1;
int64x2_t value2;
int32x4_t value3;
int8x16_t value4;
uint16x8_t value5;
uint64x2_t value6;
uint32x4_t value7;
float16x8_t value8;
float32x4_t value9;

uint8x16_t
foo ()
{
  uint8x16_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_u8 (vreinterpretq_u8_s16 (value1), vreinterpretq_u8_s64 (value2));
  r2 = vaddq_u8 (r1, vreinterpretq_u8_s32 (value3));
  r3 = vaddq_u8 (r2, vreinterpretq_u8_s8 (value4));
  r4 = vaddq_u8 (r3, vreinterpretq_u8_u16 (value5));
  r5 = vaddq_u8 (r4, vreinterpretq_u8_u64 (value6));
  r6 = vaddq_u8 (r5, vreinterpretq_u8_u32 (value7));
  r7 = vaddq_u8 (r6, vreinterpretq_u8_f16 (value8));
  return vaddq_u8 (r7, vreinterpretq_u8_f32 (value9));
}

uint8x16_t
foo1 ()
{
  uint8x16_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_u8 (vreinterpretq_u8 (value1), vreinterpretq_u8 (value2));
  r2 = vaddq_u8 (r1, vreinterpretq_u8 (value3));
  r3 = vaddq_u8 (r2, vreinterpretq_u8 (value4));
  r4 = vaddq_u8 (r3, vreinterpretq_u8 (value5));
  r5 = vaddq_u8 (r4, vreinterpretq_u8 (value6));
  r6 = vaddq_u8 (r5, vreinterpretq_u8 (value7));
  r7 = vaddq_u8 (r6, vreinterpretq_u8 (value8));
  return vaddq_u8 (r7, vreinterpretq_u8 (value9));
}

/* { dg-final { scan-assembler-times "vadd.i8" 8 } } */
