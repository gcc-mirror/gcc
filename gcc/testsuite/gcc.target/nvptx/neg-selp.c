/* { dg-do compile } */
/* { dg-options "-O2" } */

int neg(int x, int y)
{
  int t = (x == y) ? 1 : 0;
  return -t;
}

int not(int x, int y)
{
  int t = (x == y) ? 1 : 0;
  return ~t;
}

/* { dg-final { scan-assembler-not "neg.s32" } } */
/* { dg-final { scan-assembler-not "not.b32" } } */
