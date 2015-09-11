/* Verify straight-line strength reduction for simple integer addition
   with stride reversed.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int s, int c)
{
  int a1, a2, a3, x1, x2, x3, x;

  a1 = 2 * s;
  x1 = a1 + c;
  a2 = 4 * s;
  x2 = a2 + c;
  a3 = 6 * s;
  x3 = a3 + c;
  x = x1 + x2 + x3;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */
