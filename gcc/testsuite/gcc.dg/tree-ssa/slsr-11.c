/* Verify straight-line strength reduction for simple integer addition
   with casts thrown in.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

long
f (int s, long c)
{
  int a1, a2, a3;
  long x1, x2, x3, x;

  a1 = 2 * s;
  x1 = c + a1;
  a2 = 4 * s;
  x2 = c + a2;
  a3 = 6 * s;
  x3 = c + a3;
  x = x1 + x2 + x3;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
