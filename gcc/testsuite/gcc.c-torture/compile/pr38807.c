/* PR tree-optimization/38807 */

int
baz (short x)
{
  return x;
}

int a, b;

int
bar (int x)
{
  if (baz (a ^ x ^ a))
    return b;
  return 0;
}

int
foo (void)
{
  return bar (a == 0 || 1 == 1 - a) ? 1 : bar (1 && a);
}
