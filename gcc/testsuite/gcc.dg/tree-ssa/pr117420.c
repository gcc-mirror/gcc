/* PR tree-optimization/117420 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */

int
foo (unsigned long x, unsigned long y)
{
  x |= 0x11000L;
  x &= ~0xfffL;
  x += 42L;
  y &= ~0x10ffL;
  y |= 0x100L;
  y += 21L;
  return x == y;
}
