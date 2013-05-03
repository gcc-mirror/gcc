/* Verify straight-line strength reduction for a candidate with a basis
   hidden by a phi dependence and having a known stride.  Variation using
   negative increments.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int c, int i)
{
  int a1, a2, a3, x1, x2, x3, x;

  a1 = i * 16;
  x1 = c + a1;

  i = i - 2;
  a2 = i * 16;
  x2 = c + a2;

  if (x2 > 6)
    i = i - 2;

  i = i - 2;
  a3 = i * 16;
  x3 = c + a3;

  x = x1 + x2 + x3;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
