/* { dg-do compile } */
/* { dg-mips-options "-O2 -mips4 -mhard-float" } */
/* { dg-final { scan-assembler-not "rsqrt.d" } } */
/* { dg-final { scan-assembler-not "rsqrt.s" } } */

extern double sqrt(double);
extern float sqrtf(float);

double foo(double x)
{
  return 1.0/sqrt(x);
}

double bar(double x)
{
  return sqrt(1.0/x);
}

float foof(float x)
{
  return 1.0f/sqrtf(x);
}

float barf(float x)
{
  return sqrtf(1.0f/x);
}
