/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f9 (unsigned char x, int y)
{
  if (y > 1 && x == 0)
    return 10;
  return x;
}

/* { dg-final { scan-assembler "ands\t(x|w)\[0-9\]+,\[ \t\]*(x|w)\[0-9\]+,\[ \t\]*255" } } */
