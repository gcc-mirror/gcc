/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (_Bool x)
{
  return (x ^ 1);
}

/* { dg-final { scan-tree-dump-times "~x" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
