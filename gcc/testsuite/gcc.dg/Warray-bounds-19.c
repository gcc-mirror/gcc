/* PR tree-optimization/59124 */
/* { dg-options "-O3 -Warray-bounds" } */

unsigned baz[6];

void foo(unsigned *bar, unsigned n)
{
  unsigned i, j;

  if (n > 6)
    n = 6;

  for (i = 1; i < n; i++)
    for (j = i - 1; j > 0; j--)
      bar[j - 1] = baz[j - 1];
}

