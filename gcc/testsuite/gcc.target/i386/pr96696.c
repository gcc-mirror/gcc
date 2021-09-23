/* PR tree-optimization/96696 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-times "\tidivl\t" 2 } } */
/* { dg-final { scan-assembler-times "\tdivl\t" 2 } } */
/* { dg-final { scan-assembler-not "\ti?mull\t" } } */

int
foo (int x, int y)
{
  return (x / y) * y;
}

int
bar (int x, int y)
{
  return x - (x % y);
}

unsigned
baz (unsigned x, unsigned y)
{
  return (x / y) * y;
}

unsigned
qux (unsigned x, unsigned y)
{
  return x - (x % y);
}
