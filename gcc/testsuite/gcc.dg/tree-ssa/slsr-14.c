/* Straight-line strength reduction control flow variation.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int n, int c, int s)
{
  int a1, a2, x, x1, x2, x3, x4;

  a1 = 2 * s;

  if (n > 64)
    {
      x1 = c + a1;
      a2 = 4 * s;
      x2 = c + a2;
      x = x1 + x2;
    }
  else
    {
      x3 = c + a1;
      a2 = 4 * s;
      x4 = c + a2;
      x = x4 / x3;
    }

  return x;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
