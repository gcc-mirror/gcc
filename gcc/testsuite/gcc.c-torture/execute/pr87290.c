/* PR middle-end/87290 */

int c;

__attribute__((noipa)) void
f0 (void)
{
  c++;
}

__attribute__((noipa)) int
f1 (int x)
{
  return x % 16 == 13;
}

__attribute__((noipa)) int
f2 (int x)
{
  return x % 16 == -13;
}

__attribute__((noipa)) void
f3 (int x)
{
  if (x % 16 == 13)
    f0 ();
}

__attribute__((noipa)) void
f4 (int x)
{
  if (x % 16 == -13)
    f0 ();
}

int
main ()
{
  int i, j;
  for (i = -30; i < 30; i++)
    {
      if (f1 (13 + i * 16) != (i >= 0) || f2 (-13 + i * 16) != (i <= 0))
	__builtin_abort ();
      f3 (13 + i * 16);
      if (c != (i >= 0))
	__builtin_abort ();
      f4 (-13 + i * 16);
      if (c != 1 + (i == 0))
	__builtin_abort ();
      for (j = 1; j < 16; j++)
	{
	  if (f1 (13 + i * 16 + j) || f2 (-13 + i * 16 + j))
	    __builtin_abort ();
	  f3 (13 + i * 16 + j);
	  f4 (-13 + i * 16 + j);
	}
      if (c != 1 + (i == 0))
	__builtin_abort ();
      c = 0;
    }
  return 0;
}
