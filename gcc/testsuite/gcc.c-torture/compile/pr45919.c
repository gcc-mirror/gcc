/* PR tree-optimization/45919 */

const struct S { int a; int b[]; } s = { 0, { 0 }};

int
foo (void)
{
  return s.b[0];
}
