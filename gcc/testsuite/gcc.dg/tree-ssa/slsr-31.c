/* Verify straight-line strength reduction for add candidates in
   which the stride is unknown and increments appear that differ
   only in sign.  Verify the increments are shared.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int s, int c)
{
  int a1, a2, a3, a4, x1, x2, x3, x4, x;

  a1 = 2 * s;
  x1 = c + a1;
  a2 = 4 * s;  /* incr = +2  */
  x2 = c + a2;
  a3 = 7 * s;
  x3 = c + a3;
  a4 = 5 * s;  /* incr = -2  */
  x4 = c + a4;
  x = x1 + x2 + x3 + x4;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* 2" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* -2" 0 "optimized" } } */
