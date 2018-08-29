/* { dg-do compile } */
/* { dg-options "-O2 -flive-range-shrinkage" } */
/* { dg-require-effective-target int128 } */

int
a (int b, short c, int d, long e, __int128 f, short g, long h, __int128 i)
{
  d <<= f & 31;
  f >>= 127;
  g *= d > c;
  f >>= g;
  return b + e + f + h + i;
}
