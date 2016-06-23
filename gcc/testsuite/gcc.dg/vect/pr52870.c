/* { dg-do compile } */
/* { dg-additional-options "-O1" } */

void foo (unsigned long int);

long
test (int *x)
{
  unsigned long sx, xprec;

  sx = *x >= 0 ? *x : -*x;

  xprec = sx * 64;

  if (sx < 16384)
    foo (sx);

  return xprec;
}
