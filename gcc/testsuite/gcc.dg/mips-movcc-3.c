/* { dg-do compile { target "mips*-*-*" } } */
/* { dg-options "-O2 -mips4" } */
/* { dg-final { scan-assembler "movz.s" } } */
/* { dg-final { scan-assembler "movn.s" } } */
/* { dg-final { scan-assembler "movt.s" } } */
/* { dg-final { scan-assembler "movz.d" } } */
/* { dg-final { scan-assembler "movn.d" } } */
/* { dg-final { scan-assembler "movf.d" } } */

float
sub7 (float f, float g, int i)
{
  return i ? f : g;
}

float
sub8 (float f, float g, long l)
{
  return !l ? f : g;
}

float
sub9 (float f, float g, float h)
{
  return h ? f : g;
}

double
suba (double f, double g, int i)
{
  return i ? f : g;
}

double
subb (double f, double g, long l)
{
  return !l ? f : g;
}

double
subc (double f, double g, double h)
{
  return !h ? f : g;
}
