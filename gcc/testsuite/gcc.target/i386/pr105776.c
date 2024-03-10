/* PR tree-optimization/105776 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -masm=att" } */
/* { dg-final { scan-tree-dump-times " = \.MUL_OVERFLOW " 5 "optimized" } } */
/* { dg-final { scan-assembler-times "\timull\t" 5 } } */
/* { dg-final { scan-assembler-times "\tsetno\t" 5 } } */

int
foo (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  return !x || ((int) r / (int) x) == (int) y;
}

int
bar (unsigned x, unsigned y)
{
  return !x || ((int) (x * y) / (int) x) == (int) y;
}

int
baz (unsigned x, unsigned y)
{
  if (x == 0)
    return 1;
  return ((int) (x * y) / (int) x) == y;
}

int
qux (unsigned x, unsigned y, unsigned *z)
{
  unsigned int r = x * y;
  *z = r;
  return !x || ((int) r / (int) x) == (int) y;
}

int
corge (unsigned x, unsigned y, unsigned *z)
{
  unsigned int r = x * y;
  *z = r;
  return !x || ((int) r / (int) x) == y;
}
