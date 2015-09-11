/* Verify straight-line strength reduction for a candidate with a basis
   hidden by a phi dependence and having an unknown stride.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int s, int c, int i)
{
  int a1, a2, a3, x1, x2, x3, x;

  a1 = i * s;
  x1 = c + a1;

  i = i + 2;
  a2 = i * s;
  x2 = c + a2;

  if (x2 > 6)
    i = i + 2;

  i = i + 2;
  a3 = i * s;
  x3 = c + a3;

  x = x1 + x2 + x3;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* s" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* 2" 1 "optimized" } } */
