/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize" } */

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
