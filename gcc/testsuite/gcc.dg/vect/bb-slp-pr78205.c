/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-fdump-tree-optimized" } */

double x[2], a[4], b[4], c[5];

void foo ()
{
  a[0] = c[0];
  a[1] = c[1];
  a[2] = c[0];
  a[3] = c[1];
  b[0] = c[2];
  b[1] = c[3];
  b[2] = c[2];
  b[3] = c[3];
  x[0] = c[4];
  x[1] = c[4];
}

/* We may not vectorize the store to x[] as it accesses c out-of bounds
   but we do want to vectorize the other two store groups.  */

/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times "x\\\[\[0-1\]\\\] = " 2 "optimized" } } */
