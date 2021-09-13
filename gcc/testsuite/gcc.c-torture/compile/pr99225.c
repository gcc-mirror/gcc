/* PR tree-optimization/99225 */

typedef int V __attribute__((vector_size (4 * sizeof (int))));

void
foo (V *x)
{
  x[2] = (x[0] & (1 << x[1])) != 0;
}

void
bar (V *x)
{
  x[2] = ((1 << x[1]) & x[0]) != 0;
}

void
baz (V *x)
{
  V a = 1 << x[1];
  V b = a & x[0];
  x[2] = b != 0;
}

void
qux (V *x)
{
  V a = 1 << x[1];
  V b = x[0] & a;
  x[2] = b != 0;
}
