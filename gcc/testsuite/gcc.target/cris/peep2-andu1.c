/* { dg-do compile } */
/* { dg-final { scan-assembler-not "and.d " } } */
/* { dg-final { scan-assembler-not "move.d " } } */
/* { dg-final { scan-assembler "cLear.b" } } */
/* { dg-final { scan-assembler "movu.b" } } */
/* { dg-final { scan-assembler "and.b" } } */
/* { dg-final { scan-assembler "movu.w" } } */
/* { dg-final { scan-assembler "and.w" } } */
/* { dg-final { scan-assembler "andq" } } */
/* { dg-options "-O2" } */

/* Test the "andu" peephole2 trivially, memory operand.  */

int
clearb (int x, int *y)
{
  return *y & 0xff00;
}

int
andb (int x, int *y)
{
  return *y & 0x3d;
}

int
andw (int x, int *y)
{
  return *y & 0xffd;
}

int
andq (int x, int *y)
{
  return *y & 0xf0;
}

int
andq2 (int x, int *y)
{
  return *y & 0xfff0;
}
