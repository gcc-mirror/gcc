/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include <arm_mve.h>
mve_pred16_t p;

int32x4_t fn1 (int32x4_t vecIdx)
{
  return vaddq_m(vuninitializedq_s32(), vecIdx, 1, p);
}

int16x8_t fn2 (int16x8_t vecIdx)
{
  return vaddq_m(vuninitializedq_s16(), vecIdx, 1, p);
}

int8x16_t fn3 (int8x16_t vecIdx)
{
  return vaddq_m(vuninitializedq_s8(), vecIdx, 1, p);
}

uint32x4_t fn4 (uint32x4_t vecIdx)
{
  return vaddq_m(vuninitializedq_u32(), vecIdx, 1, p);
}

uint16x8_t fn5 (uint16x8_t vecIdx)
{
  return vaddq_m(vuninitializedq_u16(), vecIdx, 1, p);
}

uint8x16_t fn6 (uint8x16_t vecIdx)
{
  return vaddq_m(vuninitializedq_u8(), vecIdx, 1, p);
}

float32x4_t fn7 (float32x4_t vecIdx)
{
  return vaddq_m(vuninitializedq_f32(), vecIdx, (float32_t) 1.23, p);
}

float16x8_t fn8 (float16x8_t vecIdx)
{
  return vaddq_m(vuninitializedq_f16(), vecIdx, (float16_t) 1.40, p);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
