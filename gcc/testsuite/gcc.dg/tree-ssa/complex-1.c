/* Verify that we don't libcall for complex * real.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -O -fdump-tree-optimized" } */

typedef _Complex float C;

C foo(C x, float y)
{
  return x * y;
}

/* { dg-final { scan-tree-dump-times "__mul" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
