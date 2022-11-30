/* PR tree-optimization/107835 */

int *
foo (void)
{
  int *x = 0;
  unsigned n = n;
  for (; n; --n, ++x)
    ;
  return x;
}
