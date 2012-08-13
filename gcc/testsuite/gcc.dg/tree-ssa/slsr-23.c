/* Verify straight-line strength reduction for multiply candidates
   with variable stride and control flow.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int 
f (int n, int x, int stride)
{
  int a, x1, x2, x3;

  a = x * stride;
  x1 = x + 3;
  a += x1 * stride;

  if (n > 64)
    {
      x2 = x1 + 3;
      a += x2 * stride;
      x3 = x2 + 3;
      a += x3 * stride;
    }

  return a;
}

/* { dg-final { scan-tree-dump-times " \\* stride" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* 3" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
