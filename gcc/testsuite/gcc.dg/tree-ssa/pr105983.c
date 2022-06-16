/* PR tree-optimization/105983 */
/* { dg-do compile } */
/* { dg-options "-O2 --param=logical-op-non-short-circuit=1 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " != 0;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " & " "optimized" } } */

int
foo (unsigned a, unsigned b)
{
  return b != 0 && a >= b;
}

int
bar (unsigned a, unsigned b)
{
  return b != 0 & a >= b;
}
