/* { dg-do compile } */
/* { dg-options "-march=sierraforest -O2 -fdump-tree-slp-details" } */
/* { dg-final { scan-tree-dump-times {(?n)Detected avx256 cross-lane permutation} 1 "slp2" } } */

void
foo (double* a, double* __restrict b, int c, int n)
{
  a[0] = b[100] * b[2];
  a[1] = b[100] * b[3];
  a[2] = b[100] * b[0];
  a[3] = b[100] * b[1];
}

void
foo1 (double* a, double* __restrict b, int c, int n)
{
  a[0] = b[100] * b[0];
  a[1] = b[100] * b[1];
  a[2] = b[100] * b[3];
  a[3] = b[100] * b[2];
}
