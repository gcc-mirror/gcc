/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_vfp } */

double
foo (double x)
{
  return __builtin_rint (x);
}

/* { dg-final { scan-assembler-times "vrintx.f64\td\[0-9\]+" 1 } } */
