/* { dg-do compile } */
/* { dg-options "-fno-fast-math -ffinite-math-only -mmadd4 isa=4 -mhard-float" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tnmadd.s\t" } } */
/* { dg-final { scan-assembler "\tnmadd.d\t" } } */
/* { dg-final { scan-assembler "\tnmsub.s\t" } } */
/* { dg-final { scan-assembler "\tnmsub.d\t" } } */

NOMIPS16 float
sub1 (float f, float g, float h)
{
  return -((f * g) + h);
}

NOMIPS16 double
sub2 (double f, double g, double h)
{
  return -((f * g) + h);
}

NOMIPS16 float
sub3 (float f, float g, float h)
{
  return -((f * g) - h);
}

NOMIPS16 double
sub4 (double f, double g, double h)
{
  return -((f * g) - h);
}
