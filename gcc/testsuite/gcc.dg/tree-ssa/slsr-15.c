/* Straight-line strength reduction control flow variation.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int 
f (int n, int c, int s)
{
  int a, x1, x2, x3;

  x1 = x2 = x3 = c;

  if (n > 64)
    {
      a = 2 * s;
      x1 = c + a;
      a = 4 * s;
      x2 = c + a;
      a = 6 * s;
      x3 = c + a;
    }

  return x1 + x2 + x3;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
