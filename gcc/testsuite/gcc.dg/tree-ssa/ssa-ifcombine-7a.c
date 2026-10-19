/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1-details -fdump-tree-phiopt2" } */
/* This is a variant of ssa-ifcombine-7.c which tests phiopt rather than ifcombine,
   dealing with -1 formation.  */
/* PR tree-optimization/127654 */

int test1 (int i, int j)
{
  if (i >= j) {
    return -(i == j);
  }
  return -1;
}
int test2 (int i, int j)
{
  if (i >= j) {
    return ~(i == j);
  }
  return ~1;
}

/* The above should be optimized to a i <= j test by ifcombine.  */

/* { dg-final { scan-tree-dump-times " <= " 2 "phiopt2" } } */
/* { dg-final { scan-tree-dump-not " == " "phiopt2" } } */
/* Facting out the - and the cast out of test1 and test2 so 4 times.  */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from COND_EXPR" 4 "phiopt1" } } */
