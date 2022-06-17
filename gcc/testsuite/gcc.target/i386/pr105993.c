/* PR target/105993 */
/* { dg-do compile } */
/* { dg-options "-O -mxop" } */

typedef unsigned short __attribute__((__vector_size__ (16))) V;
V x, y, z;

char c;
short s;

V
foo (void)
{
  V u = __builtin_shufflevector (z, y, 2, 1, 0, 8, 4, 1, 7, 2);
  V v = ~(__builtin_bswap16 (s) & (u ^ c));

  return v;
}
