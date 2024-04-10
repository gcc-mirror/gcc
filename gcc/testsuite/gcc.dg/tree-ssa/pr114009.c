/* PR tree-optimization/114009 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fdump-tree-forwprop1" } */
/* { dg-final { scan-tree-dump-times "  return 0;" 3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "  (?:return|<retval> =) { 0, 0, 0, 0 };" 1 "forwprop1" } } */

int
foo (int x)
{
  x = (x / 2) * 2;
  return (!x) * x;
}

int
bar (int x, int y)
{
  (void) x;
  return y * !y;
}

unsigned long long
baz (unsigned long long x)
{
  return (!x) * x;
}

typedef int V __attribute__((vector_size (4 * sizeof (int))));

V
qux (V x)
{
  return x * (x == 0);
}
