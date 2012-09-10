/* { dg-do compile } */
/* { dg-options "-ffast-math isa=4 -mhard-float -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\trsqrt.d\t" } } */
/* { dg-final { scan-assembler "\trsqrt.s\t" } } */

extern double sqrt(double);
extern float sqrtf(float);

NOMIPS16 double foo(double x)
{
  return 1.0/sqrt(x);
}

NOMIPS16 float bar(float x)
{
  return 1.0f/sqrtf(x);
}
