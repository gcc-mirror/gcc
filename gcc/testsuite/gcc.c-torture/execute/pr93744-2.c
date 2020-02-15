/* PR tree-optimization/93744 */

int w;

int
foo (int x, int y, int z)
{
  int r = z - ((z - w++) & -(x < y));
  return r;
}

int
main ()
{
  w = 4;
  if (foo (5, 7, 12) != 4 || w != 5)
    __builtin_abort ();
  if (foo (7, 5, 12) != 12 || w != 6)
    __builtin_abort ();
  return 0;
}
