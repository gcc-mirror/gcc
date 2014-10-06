/* PR tree-optimization/49000 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

static int
foo (int x, int y)
{
  return x * y;
}

static int
bar (int *z)
{
  return *z;
}

void
baz (void)
{
  int a = 42;
  int *b = &a;
  foo (bar (&a), 3);
}

void
test (void)
{
  baz ();
}
