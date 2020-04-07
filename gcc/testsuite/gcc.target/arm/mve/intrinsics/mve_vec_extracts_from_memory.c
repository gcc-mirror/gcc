/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3" } */

#include "arm_mve.h"

uint8x16_t *vu8;
int8x16_t *vs8;
uint16x8_t *vu16;
int16x8_t *vs16;
uint32x4_t *vu32;
int32x4_t *vs32;
uint64x2_t *vu64;
int64x2_t *vs64;
float16x8_t *vf16;
float32x4_t *vf32;
uint8_t u8;
uint16_t u16;
uint32_t u32;
uint64_t u64;
int8_t s8;
int16_t s16;
int32_t s32;
int64_t s64;
float16_t f16;
float32_t f32;

void foo (void)
{
  u8 = vgetq_lane (*vu8, 1);
  u16 = vgetq_lane (*vu16, 1);
  u32 = vgetq_lane (*vu32, 1);
  u64 = vgetq_lane (*vu64, 1);
  s8 = vgetq_lane (*vs8, 1);
  s16 = vgetq_lane (*vs16, 1);
  s32 = vgetq_lane (*vs32, 1);
  s64 = vgetq_lane (*vs64, 1);
  f16 = vgetq_lane (*vf16, 1);
  f32 = vgetq_lane (*vf32, 1);
}
