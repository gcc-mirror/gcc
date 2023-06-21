/* PR tree-optimization/101856 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " .MUL_OVERFLOW " "optimized" { target i?86-*-* x86_64-*-* aarch64*-*-* powerpc64le-*-* } } } */

int
foo (unsigned long x, unsigned long y)
{
  unsigned long z = x * y;
  return z / y != x;
}
