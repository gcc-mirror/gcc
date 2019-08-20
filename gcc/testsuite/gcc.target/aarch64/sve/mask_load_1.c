/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256 -fdump-tree-optimized" } */

void
f (int *x)
{
  for (int i = 0; i < 8; ++i)
    x[i] += 1;
}

/* { dg-final { scan-tree-dump { = MEM <vector\(8\) int>} "optimized" } } */
/* { dg-final { scan-tree-dump { MEM <vector\(8\) int> \[[^]]*\] = } "optimized" } } */
