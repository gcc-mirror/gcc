/* { dg-do compile } */
/* { dg-options "-ffast-math -mips64 -mhard-float -mgp32" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\trsqrt.d\t" } } */
/* { dg-final { scan-assembler-times "\trsqrt.s\t" 2 } } */

extern double sqrt(double);
extern float sqrtf(float);

NOMIPS16 double f1 (double x)
{
  return 1.0 / sqrt (x);
}

NOMIPS16 double f2 (double x)
{
  return sqrt (1.0 / x);
}

NOMIPS16 float f3 (float x)
{
  return 1.0f / sqrtf (x);
}

NOMIPS16 float f4 (float x)
{
  return sqrtf (1.0f / x);
}
