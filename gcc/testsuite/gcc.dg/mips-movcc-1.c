/* { dg-do compile { target "mips*-*-*" } } */
/* { dg-options "-O2 -mips4" } */
/* { dg-final { scan-assembler "movz" } } */
/* { dg-final { scan-assembler "movn" } } */
/* { dg-final { scan-assembler "movt" } } */

int
sub1 (int i, int j, int k)
{
  return k ? i : j;
}

int
sub2 (int i, int j, long l)
{
  return !l ? i : j;
}

int
sub3 (int i, int j, float f)
{
  return f ? i : j;
}
