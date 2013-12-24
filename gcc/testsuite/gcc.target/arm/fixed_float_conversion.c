/* Check that vcvt is used for fixed and float data conversions.  */
/* { dg-do compile } */
/* { dg-options "-O1 -mfpu=vfp3" } */
/* { dg-require-effective-target arm_vfp_ok } */

float
fixed_to_float (int i)
{
  return ((float) i / (1 << 16));
}

int
float_to_fixed (float f)
{
  return ((int) (f * (1 << 16)));
}

/* { dg-final { scan-assembler "vcvt.f32.s32" } } */
/* { dg-final { scan-assembler "vcvt.s32.f32" } } */
