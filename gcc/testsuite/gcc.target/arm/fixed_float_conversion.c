/* Check that vcvt is used for fixed and float data conversions.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_vfp3_ok } */
/* { dg-options "-O1" } */
/* { dg-add-options arm_vfp3 } */

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
