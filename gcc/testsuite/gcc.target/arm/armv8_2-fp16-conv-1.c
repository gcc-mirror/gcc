/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

/* Test ARMv8.2 FP16 conversions.  */
#include <arm_fp16.h>

float
f16_to_f32 (__fp16 a)
{
  return (float)a;
}

float
f16_to_pf32 (__fp16* a)
{
  return (float)*a;
}

short
f16_to_s16 (__fp16 a)
{
  return (short)a;
}

short
pf16_to_s16 (__fp16* a)
{
  return (short)*a;
}

/* { dg-final { scan-assembler-times {vcvtb\.f32\.f16\ts[0-9]+, s[0-9]+} 4 } }  */

__fp16
f32_to_f16 (float a)
{
  return (__fp16)a;
}

void
f32_to_pf16 (__fp16* x, float a)
{
  *x = (__fp16)a;
}

__fp16
s16_to_f16 (short a)
{
  return (__fp16)a;
}

void
s16_to_pf16 (__fp16* x, short a)
{
  *x = (__fp16)a;
}

/* { dg-final { scan-assembler-times {vcvtb\.f16\.f32\ts[0-9]+, s[0-9]+} 4 } }  */

float
s16_to_f32 (short a)
{
  return (float)a;
}

/* { dg-final { scan-assembler-times {vcvt\.f32\.s32\ts[0-9]+, s[0-9]+} 3 } }  */

short
f32_to_s16 (float a)
{
  return (short)a;
}

/* { dg-final { scan-assembler-times {vcvt\.s32\.f32\ts[0-9]+, s[0-9]+} 3 } }  */

unsigned short
f32_to_u16 (float a)
{
  return (unsigned short)a;
}

/* { dg-final { scan-assembler-times {vcvt\.u32\.f32\ts[0-9]+, s[0-9]+} 1 } }  */

short
f64_to_s16 (double a)
{
  return (short)a;
}

/* { dg-final { scan-assembler-times {vcvt\.s32\.f64\ts[0-9]+, d[0-9]+} 1 } }  */

unsigned short
f64_to_u16 (double a)
{
  return (unsigned short)a;
}

/* { dg-final { scan-assembler-times {vcvt\.s32\.f64\ts[0-9]+, d[0-9]+} 1 } }  */


