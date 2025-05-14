/* PR tree-optimization/119616 */
/* { dg-do compile { target external_musttail } } */
/* { dg-options "-O2" } */

int foo (int *);
int bar (int);

int
baz (int x)
{
  if (!x)
    [[gnu::musttail]] return bar (x);
  return foo (&x);
}

int
qux (int x)
{
  if (!x)
    [[gnu::musttail]] return bar (x);
  foo (&x);
  return 1;
}
