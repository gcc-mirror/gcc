/* PR tree-optimization/112628 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* For unsigned A, ~A < -A is A != 0 and ~A >= -A is A == 0.  */

int
f1 (unsigned a)
{
  return (~a) < -a;
}

int
f2 (unsigned a)
{
  return -a > ~a;
}

int
f3 (unsigned a)
{
  return (~a) >= -a;
}

int
f4 (unsigned a)
{
  return -a <= ~a;
}

/* { dg-final { scan-tree-dump-not "~" "optimized" } } */
/* { dg-final { scan-tree-dump-not "= -" "optimized" } } */
/* { dg-final { scan-tree-dump-times " != 0" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " == 0" 2 "optimized" } } */
