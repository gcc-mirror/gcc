/* { dg-do compile } */
/* { dg-options "-O2" } */
int f(int i)
{
  int t = i ? -1 : 0;
  return t | 1;
}

int f1(int i)
{
  int t = i ? -1 : 1;
  return t;
}

/* { dg-final { scan-assembler-times "negl" 2 } } */
/* { dg-final { scan-assembler-times "sbbl" 2 } } */
/* { dg-final { scan-assembler-times "orl" 2 } } */
/* { dg-final { scan-assembler-not "cmpl" } } */

