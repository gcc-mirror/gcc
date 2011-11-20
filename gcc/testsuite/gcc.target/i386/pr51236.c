/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -mavx2" } */

long foo (long *p, int i)
{
  long x = 0;

  while (--i)
    x ^= p[i];

  return x;
}
