/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include <arm_mve.h>
int8x16_t foo (int8x16_t a, int16_t b)
{
  return vaddq (a, (b<<3));
}
int16x8_t foo1 (int16x8_t a, int16_t b)
{
  return vaddq (a, (b<<3));
}
int32x4_t foo2 (int32x4_t a, int16_t b)
{
  return vaddq (a, (b<<3));
}
uint8x16_t foo3 (uint8x16_t a, int16_t b)
{
  return vaddq (a, (b<<3));
}
uint16x8_t foo4 (uint16x8_t a, int16_t b)
{
  return vaddq (a, (b<<3));
}
uint32x4_t foo5 (uint32x4_t a, int16_t b)
{
  return vaddq (a, (b<<3));
}
float16x8_t foo6 (float16x8_t a)
{
  return vaddq (a, (float16_t)23.6);
}
float32x4_t foo7 (float32x4_t a)
{
  return vaddq (a, (float32_t)23.46);
}
float16x8_t foo8 (float16x8_t a)
{
  return vaddq (a, 23.6);
}
float32x4_t foo9 (float32x4_t a)
{
  return vaddq (a, 23.46);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
