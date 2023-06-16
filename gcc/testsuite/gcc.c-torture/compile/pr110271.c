/* PR tree-optimization/110271 */

unsigned a, b, c, d, e;

void
foo (unsigned *x, int y, unsigned int *z)
{
  for (int i = 0; i < y; i++)
    {
      b += d;
      a += b < d;
      a += c = (__PTRDIFF_TYPE__) x > 3;
      d = z[1] + (a < c);
      a += e;
      d += a < e;
    }
}

void
bar (unsigned int *z)
{
  unsigned *x = x;
  foo (x, 9, z);
}
