/* PR tree-optimization/109410 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

__attribute__((returns_twice)) int baz (int, int);

int
bar (int x)
{
  return x;
}

int
foo (int x, int y)
{
  baz (x, y);
  int a = bar (x);
  return y || a == 42 || a > 42;
}
