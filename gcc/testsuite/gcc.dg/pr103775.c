/* PR rtl-optimization/103775 */
/* { dg-do assemble { target int128 } } */
/* { dg-options "-Og -fno-forward-propagate -free -g" } */

int
foo (char a, short b, int c, __int128 d, char e, short f, int g, __int128 h)
{
  long i = __builtin_clrsbll ((char) h);
  __builtin_memset ((char *) &h + 4, d, 3);
  c &= (char) h;
  return c + i;
}
