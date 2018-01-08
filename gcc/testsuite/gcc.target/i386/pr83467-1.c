/* { dg-do compile } */
/* { dg-options "-O2 -flive-range-shrinkage -m8bit-idiv" } */
/* { dg-require-effective-target int128 } */

unsigned a;

__int128
b (unsigned c, short d, int e, long f, unsigned __int128 g, char h,
   int i, __int128 j)
{
  j %= 5;
  c *= i;
  e = e >> (g & 31);
  h &= e /= d;
  g <<= 0 <= 0;
  g &= h < j;
  return c + d + f + g + h + i + a + j;
}
