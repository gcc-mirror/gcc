/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f1 (int x)
{
  if (x & 1)
    return 1;
  return x;
}

/* { dg-final { scan-assembler "(tst|ands)\t(x|w)\[0-9\]*.*1" } } */
