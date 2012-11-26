/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_vfp } */

double
foo (double x)
{
  return __builtin_floor (x);
}

/* { dg-final { scan-assembler-times "vrintm.f64\td\[0-9\]+" 1 } } */
