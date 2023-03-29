/* { dg-do compile { target int128 } } */
/* { dg-options "-msse4 -O2 -fno-tree-loop-im --param max-combine-insns=2 -Wno-shift-count-overflow" } */

unsigned __int128 n;

int
foo (int x)
{
  __int128 a = 0;
  int b = !!(n * 2);

  while (x < 2)
    {
      if (a)
        {
          if (n)
            n ^= 1;
          else
            x <<= 32;
        }

      a = 1;
    }

  return b;
}
