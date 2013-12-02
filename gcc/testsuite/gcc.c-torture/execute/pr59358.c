/* PR tree-optimization/59358 */

__attribute__((noinline, noclone)) int
foo (int *x, int y)
{
  int z = *x;
  if (y > z && y <= 16)
    while (y > z)
      z *= 2;
  return z;
}

int
main ()
{
  int i;
  for (i = 1; i < 17; i++)
    {
      int j = foo (&i, 16);
      int k;
      if (i >= 8 && i <= 15)
	k = 16 + (i - 8) * 2;
      else if (i >= 4 && i <= 7)
	k = 16 + (i - 4) * 4;
      else if (i == 3)
	k = 24;
      else
	k = 16;
      if (j != k)
	__builtin_abort ();
      j = foo (&i, 7);
      if (i >= 7)
	k = i;
      else if (i >= 4)
	k = 8 + (i - 4) * 2;
      else if (i == 3)
	k = 12;
      else
	k = 8;
      if (j != k)
	__builtin_abort ();
    }
  return 0;
}
