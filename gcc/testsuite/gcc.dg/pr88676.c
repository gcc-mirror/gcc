/* PR tree-optimization/88676 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "tree-ssa/pr88676.c"

__attribute__((noipa)) void
bar (int x, int y, int z)
{
  if (z != 115 && z != 116)
    __builtin_abort ();
  if (x == 98)
    {
      if (y != z)
	__builtin_abort ();
    }
  else if (x != 99)
    __builtin_abort ();
  else if (z == 115)
    {
      if (y != 116)
	__builtin_abort ();
    }
  else if (y != 115)
    __builtin_abort ();
}

int
main ()
{
  if (f1 (0) != 1 || f1 (1) != 2)
    __builtin_abort ();
  int i;
  for (i = -12; i < 12; i++)
    {
      f2 (i);
      f3 (i);
      f4 (i);
      f5 (i);
      f6 (i);
      f7 (i);
      f8 (i);
      f9 (i);
      if (f10 (i) != ((i & 1) ? 84 : 85))
	__builtin_abort ();
    }
  return 0;
}
