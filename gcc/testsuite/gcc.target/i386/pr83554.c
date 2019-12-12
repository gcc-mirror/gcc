/* PR target/83554 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Os" } */

unsigned a;
unsigned __int128
foo (unsigned char c, unsigned short d, unsigned e, unsigned long f,
     unsigned __int128 g, unsigned h, unsigned short i, unsigned long j)
{
  j /= (unsigned)-2;
  h += h &= c == c;
  h -= g;
  i = i >> 8 | i << 8;
  return c + d + e + f + g + h + i + j + a;
}
