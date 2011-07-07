/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (unsigned long a, long b)
{
  return (a & !a) | (b & (b == 0));
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
