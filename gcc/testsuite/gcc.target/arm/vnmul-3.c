/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_dp_ok } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-options "-O2 -fno-rounding-math" } */
/* { dg-add-options arm_fp_dp } */

double
foo_d (double a, double b)
{
  /* { dg-final { scan-assembler "vnmul\\.f64" } } */
  return -(a * b);
}

float
foo_s (float a, float b)
{
  /* { dg-final { scan-assembler "vnmul\\.f32" } } */
  return -(a * b);
}
