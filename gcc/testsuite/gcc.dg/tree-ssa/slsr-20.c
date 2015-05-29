/* Verify straight-line strength reduction for multiply candidates
   with stride in inconsistent positions.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int c, int s)
{
  int x1, x2, y1, y2;

  y1 = c + 2;
  x1 = y1 * s;
  y2 = y1 + 2;
  x2 = s * y2;
  return x1 + x2;
}

/* { dg-final { scan-tree-dump-times " \\* s" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* 2" 1 "optimized" } } */
