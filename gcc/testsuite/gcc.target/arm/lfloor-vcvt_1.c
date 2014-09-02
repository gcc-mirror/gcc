/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2 -march=armv8-a" } */
/* { dg-add-options arm_v8_vfp } */

int
foofloat (float x)
{
  return __builtin_lfloorf (x);
}

/* { dg-final { scan-assembler-times "vcvtm.s32.f32\ts\[0-9\]+, s\[0-9\]+" 1 } } */


int
foodouble (double x)
{
  return __builtin_lfloor (x);
}

/* { dg-final { scan-assembler-times "vcvtm.s32.f64\ts\[0-9\]+, d\[0-9\]+" 1 } } */
