/* { dg-do compile } */
/* { dg-options "-O2" } */

int mulhisi3(short x, short y)
{
  return (int)x * (int)y;
}

long mulsidi3(int x, int y)
{
  return (long)x * (long)y;
}

/* { dg-final { scan-assembler-times "mul.wide.s16" 1 } } */
/* { dg-final { scan-assembler-times "mul.wide.s32" 1 } } */

