/* Straight-line strength reduction control flow variation with incr = -1.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int n, int c, int s)
{
  int a1, a2, a3, a4, x1, x2, x3, x4;

  a1 = 4 * s;
  x1 = c + a1;
  x2 = x3 = x4 = c;

  if (n > 64)
    {
      a2 = 3 * s;
      x2 = c + a2;
      a3 = 2 * s;
      x3 = c + a3;
    }
  else
    {
      a4 = 3 * s;
      x4 = c + a4;
    }

  return x1 + x2 + x3 + x4;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */
