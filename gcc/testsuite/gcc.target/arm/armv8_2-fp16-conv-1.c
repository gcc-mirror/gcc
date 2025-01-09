/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" }  */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */
/* { dg-final { check-function-bodies "**" "" } } */

/* Test ARMv8.2 FP16 conversions.  */
#include <arm_fp16.h>

/*
** f16_to_f32:
** ...
**	vcvtb\.f32\.f16	(s[0-9]+), \1
** ...
*/
float
f16_to_f32 (__fp16 a)
{
  return (float)a;
}

/*
** f16_to_pf32:
** ...
**	vcvtb\.f32\.f16	(s[0-9]+), \1
** ...
*/
float
f16_to_pf32 (__fp16* a)
{
  return (float)*a;
}

/*
** f16_to_s16:
** ...
**	vcvtb\.f32\.f16	(s[0-9]+), \1
**	vcvt\.s32\.f32	\1, \1
** ...
*/
short
f16_to_s16 (__fp16 a)
{
  return (short)a;
}

/*
** pf16_to_s16:
** ...
**	vcvtb\.f32\.f16	(s[0-9]+), \1
**	vcvt\.s32\.f32	\1, \1
** ...
*/
short
pf16_to_s16 (__fp16* a)
{
  return (short)*a;
}

/*
** f32_to_f16:
** ...
**	vcvtb\.f16\.f32	(s[0-9]+), \1
** ...
*/
__fp16
f32_to_f16 (float a)
{
  return (__fp16)a;
}

/*
** f32_to_pf16:
** ...
**	vcvtb\.f16\.f32	(s[0-9]+), \1
** ...
*/
void
f32_to_pf16 (__fp16* x, float a)
{
  *x = (__fp16)a;
}

/*
** s16_to_f16:
** ...
**	vcvt\.f32\.s32	(s[0-9]+), \1
**	vcvtb\.f16\.f32	\1, \1
** ...
*/
__fp16
s16_to_f16 (short a)
{
  return (__fp16)a;
}

/*
** s16_to_pf16:
** ...
**	vcvt\.f32\.s32	(s[0-9]+), \1
**	vcvtb\.f16\.f32	\1, \1
** ...
*/
void
s16_to_pf16 (__fp16* x, short a)
{
  *x = (__fp16)a;
}

/*
** s16_to_f32:
** ...
**	vcvt\.f32\.s32	(s[0-9]+), \1
** ...
*/
float
s16_to_f32 (short a)
{
  return (float)a;
}

/*
** f32_to_s16:
** ...
**	vcvt\.s32\.f32	(s[0-9]+), \1
** ...
*/
short
f32_to_s16 (float a)
{
  return (short)a;
}

/*
** f32_to_u16:
** ...
**	vcvt\.u32\.f32	(s[0-9]+), \1
** ...
*/
unsigned short
f32_to_u16 (float a)
{
  return (unsigned short)a;
}

/*
** f64_to_s16:
** ...
**	vcvt\.s32\.f64	s[0-9]+, d[0-9]+
** ...
*/
short
f64_to_s16 (double a)
{
  return (short)a;
}

/*
** f64_to_u16:
** ...
**	vcvt\.u32\.f64	s[0-9]+, d[0-9]+
** ...
*/
unsigned short
f64_to_u16 (double a)
{
  return (unsigned short)a;
}
