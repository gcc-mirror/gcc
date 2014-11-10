/* PR tree-optimization/47428 */
/* { dg-require-effective-target untyped_assembly } */

struct S
{
  int s;
} a;
int b;

void bar (struct S);

int
baz (int x __attribute__((unused)), int y)
{
  int i;
  for (i = 0; i < 1; i = 1)
    for (y = 0; y < 1; y = 1);
  return y;
}

void
foo (void)
{
  fn (0);
}

int
fn (const int x, int y __attribute__((unused)))
{
  if (baz (baz (0, x), 0))
    return 0;
  else
    bar (a);
  return 0;
}

void
bar (struct S x)
{
  for (;;)
    for (; x.s;)
      b = 0 ? : baz (0, 0);
}
