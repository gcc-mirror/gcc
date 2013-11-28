/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2 -marm -march=armv8-a" } */
/* { dg-add-options arm_v8_vfp } */

double foo (double a)
{
  if (a > 3.0)
    return  __builtin_round (a);

  return 0.0;
}

/* { dg-final { scan-assembler-times "vrinta.f64\td\[0-9\]+" 1 } } */

