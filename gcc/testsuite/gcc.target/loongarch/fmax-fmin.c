/* { dg-do compile } */
/* { dg-options "-mdouble-float -fno-finite-math-only" } */
/* { dg-final { scan-assembler "fmin\\.s" } } */
/* { dg-final { scan-assembler "fmin\\.d" } } */
/* { dg-final { scan-assembler "fmax\\.s" } } */
/* { dg-final { scan-assembler "fmax\\.d" } } */

double
_fmax(double a, double b)
{
  return __builtin_fmax(a, b);
}

float
_fmaxf(float a, float b)
{
  return __builtin_fmaxf(a, b);
}

double
_fmin(double a, double b)
{
  return __builtin_fmin(a, b);
}

float
_fminf(float a, float b)
{
  return __builtin_fminf(a, b);
}
