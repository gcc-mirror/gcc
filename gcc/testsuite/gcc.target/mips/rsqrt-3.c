/* { dg-do compile } */
/* { dg-options "isa=4 -mhard-float" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\trsqrt.d\t" } } */
/* { dg-final { scan-assembler-not "\trsqrt.s\t" } } */

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
