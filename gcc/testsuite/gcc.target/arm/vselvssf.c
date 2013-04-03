/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_vfp } */

float
foo (float x, float y)
{
  return __builtin_isunordered (x, y) ? x : y;
}

/* { dg-final { scan-assembler-times "vselvs.f32\ts\[0-9\]+" 1 } } */
