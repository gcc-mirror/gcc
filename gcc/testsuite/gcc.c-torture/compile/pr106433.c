/* PR tree-optimization/106433 */

int m, *p;

__attribute__ ((simd,noipa)) int
bar (int x)
{
  if (x)
    {
      if (m < 1)
        for (m = 0; m < 1; ++m)
          ++x;
      p = &x;
      for (;;)
        ++m;
    }
  return 0;
}

__attribute__ ((simd)) int
foo (int x)
{
  return bar (x);
}
