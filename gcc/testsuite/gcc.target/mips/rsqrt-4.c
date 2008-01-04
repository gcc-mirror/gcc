/* { dg-do compile } */
/* { dg-mips-options "-O2 -ffast-math -mips64 -mhard-float -mgp32" } */
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
