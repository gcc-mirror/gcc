/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/111431 */

unsigned
foo (int a)
{
  int b = !a;
  return (a & b);
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
