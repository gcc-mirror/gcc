/* { dg-do compile } */
/* { dg-mips-options "-O2 -ffast-math -mips4 -mhard-float" } */
/* { dg-final { scan-assembler "rsqrt.d" } } */
/* { dg-final { scan-assembler "rsqrt.s" } } */

extern double sqrt(double);
extern float sqrtf(float);

double foo(double x)
{
  return 1.0/sqrt(x);
}

float bar(float x)
{
  return 1.0f/sqrtf(x);
}
