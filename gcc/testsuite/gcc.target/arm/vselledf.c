/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_vfp } */

double
foo (double x, double y)
{
  volatile int i = 0;
  return i <= 0 ? x : y;
}

/* { dg-final { scan-assembler-times "vselgt.f64\td\[0-9\]+" 1 } } */
