/* PR debug/99830 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-expensive-optimizations -fno-split-wide-types -g" } */

int foo (long a, __int128 b, short c, int d, unsigned e, __int128 f)
{
  __builtin_memmove (2 + (char *) &f, foo, 1);
  c >>= (char) f;
  return c;
}
