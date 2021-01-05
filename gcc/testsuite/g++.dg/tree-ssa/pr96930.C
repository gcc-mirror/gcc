// PR tree-optimization/96930
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump " = a_\[0-9]\\\(D\\\) >> b_\[0-9]\\\(D\\\);" "optimized" } }

unsigned
foo (unsigned a, unsigned b)
{
  return a / (unsigned long long) (1U << b);
}
