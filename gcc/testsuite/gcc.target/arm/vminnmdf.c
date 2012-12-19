/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-ffast-math" } */
/* { dg-add-options arm_v8_vfp } */

double
foo (double x, double y)
{
  return __builtin_fmin (x, y);
}

/* { dg-final { scan-assembler-times "vminnm.f64\td\[0-9\]+" 1 } } */
