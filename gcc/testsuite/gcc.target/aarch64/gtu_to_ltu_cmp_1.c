/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f1 (int x, int t)
{
  if (x == -1 || x == -2)
    t = 1;

  return t;
}

/* { dg-final { scan-assembler-times "cmn\\tw\[0-9\]+, #3" 1 } } */
