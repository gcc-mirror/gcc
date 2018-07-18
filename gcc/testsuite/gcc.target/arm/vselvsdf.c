/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2 -mcpu=cortex-a57" } */
/* { dg-add-options arm_v8_vfp } */

double
foo (double x, double y)
{
  return __builtin_isunordered (x, y) ? x : y;
}

/* { dg-final { scan-assembler-times "vselvs.f64\td\[0-9\]+" 1 } } */
