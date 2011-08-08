/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (long a, unsigned long b)
{
  return (a & (a == 0)) | (b & !b);
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
