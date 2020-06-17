/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
int16x8_t value1;
int64x2_t value2;
int32x4_t value3;
uint8x16_t value4;
uint16x8_t value5;
uint64x2_t value6;
uint32x4_t value7;
float16x8_t value8;
float32x4_t value9;

int8x16_t
foo ()
{
  int8x16_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_s8 (vreinterpretq_s8_s16 (value1), vreinterpretq_s8_s64 (value2));
  r2 = vaddq_s8 (r1, vreinterpretq_s8_s32 (value3));
  r3 = vaddq_s8 (r2, vreinterpretq_s8_u8 (value4));
  r4 = vaddq_s8 (r3, vreinterpretq_s8_u16 (value5));
  r5 = vaddq_s8 (r4, vreinterpretq_s8_u64 (value6));
  r6 = vaddq_s8 (r5, vreinterpretq_s8_u32 (value7));
  r7 = vaddq_s8 (r6, vreinterpretq_s8_f16 (value8));
  return vaddq_s8 (r7, vreinterpretq_s8_f32 (value9));
}

int8x16_t
foo1 ()
{
  int8x16_t r1,r2,r3,r4,r5,r6,r7;
  r1 = vaddq_s8 (vreinterpretq_s8 (value1), vreinterpretq_s8 (value2));
  r2 = vaddq_s8 (r1, vreinterpretq_s8 (value3));
  r3 = vaddq_s8 (r2, vreinterpretq_s8 (value4));
  r4 = vaddq_s8 (r3, vreinterpretq_s8 (value5));
  r5 = vaddq_s8 (r4, vreinterpretq_s8 (value6));
  r6 = vaddq_s8 (r5, vreinterpretq_s8 (value7));
  r7 = vaddq_s8 (r6, vreinterpretq_s8 (value8));
  return vaddq_s8 (r7, vreinterpretq_s8 (value9));
}

/* { dg-final { scan-assembler-times "vadd.i8" 8 } } */
