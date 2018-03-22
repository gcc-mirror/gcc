/* { dg-do compile } */
/* { dg-options "-O2" } */

/* X is both compared against zero and used.  Make sure we can still
   generate an ADDS and avoid an explicit comparison against zero.  */

int
foo (int x, int y)
{
  x += y;
  if (x != 0)
    x = x + 2;
  return x;
}

/* { dg-final { scan-assembler-times "adds\\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-not "cmp\\tw\[0-9\]+, 0" } } */
