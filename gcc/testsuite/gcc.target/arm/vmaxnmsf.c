/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-ffast-math" } */
/* { dg-add-options arm_v8_vfp } */

float
foo (float x, float y)
{
  return __builtin_fmaxf (x, y);
}

/* { dg-final { scan-assembler-times "vmaxnm.f32\ts\[0-9\]+" 1 } } */
