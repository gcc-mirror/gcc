/* Straight-line strength reduction control flow variation.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int n, int c, int s)
{
  int a2, a3, a4, x1, x2, x3, x4;

  x1 = c + s;
  a2 = 3 * s;
  x2 = c + a2;
  x3 = x4 = c;

  if (n > 64)
    {
      a3 = 5 * s;
      x3 = c + a3;
      a4 = 7 * s;
      x4 = c + a4;
    }

  return x1 + x2 + x3 + x4;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
