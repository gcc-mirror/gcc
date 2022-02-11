/* PR debug/103808 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-fcompare-debug -O2 -ftrapv" } */

void
foo (__int128 x, int y)
{
  for (;;)
    {
      __int128 a, b;

      x |= !!y;
      a = x + 1;
      b = y ? ++y : ++x;
      y = a < b;
      asm ("" : "+r" (y));
      if (x >> 2)
        y *= 2;

      if (y == b)
        __builtin_unreachable ();
    }
}
