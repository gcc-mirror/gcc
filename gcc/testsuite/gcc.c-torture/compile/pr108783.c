/* PR tree-optimization/108783 */

__attribute__((returns_twice)) int baz (int, int);

int
bar (int x)
{
  return x;
}

int
foo (int x, int y)
{
  int a;

  a = bar (x);
  baz (x, y);

  return y && a && a;
}

int
qux (int x, int y)
{
  int a;

  a = bar (x);
  baz (x, y);

  return y && a != 42 && a >= 42;
}

int
corge (int x, int y)
{
  int a;

  a = bar (x);
  baz (x, y);

  return y || a == 42 || a > 42;
}
