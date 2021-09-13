/* PR tree-optimization/95731 */

__attribute__((noipa)) int
foo (int x, int y, int z, int w, long long u, long long v)
{
  return x >= 0 && y >= 0 && z < 0 && u < 0 && w >= 0 && v < 0;
}

__attribute__((noipa)) int
bar (int x, int y, int z, int w, long long u, long long v)
{
  return u >= 0 && x >= 0 && y >= 0 && v < 0 && z >= 0 && w >= 0;
}

__attribute__((noipa)) int
baz (int x, int y, int z, int w, long long u, long long v)
{
  return x >= 0 || u < 0 || y >= 0 || v < 0 || z >= 0 || w >= 0;
}

int
main ()
{
  int i;
  for (i = 0; i < 64; i++)
    {
      int a = foo ((i & 1) ? -123 : 456, (i & 2) ? -123 : 456,
		   (i & 4) ? -123 : 456, (i & 8) ? -123 : 456,
		   (i & 16) ? -123 : 456, (i & 32) ? -123 : 456);
      int b = bar ((i & 1) ? -123 : 456, (i & 2) ? -123 : 456,
		   (i & 4) ? -123 : 456, (i & 8) ? -123 : 456,
		   (i & 16) ? -123 : 456, (i & 32) ? -123 : 456);
      int c = baz ((i & 1) ? -123 : 456, (i & 2) ? -123 : 456,
		   (i & 4) ? -123 : 456, (i & 8) ? -123 : 456,
		   (i & 16) ? -123 : 456, (i & 32) ? -123 : 456);
      if (a != (i == 52) || b != (i == 32) || c != (i != 15))
	__builtin_abort ();
    }
  return 0;
}
