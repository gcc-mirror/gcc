/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

void f(short * __restrict__ a, short * __restrict__ b, short * __restrict__ x)
{
  int i;
  for (i=0;i<1024;i++)
    x[i] = a[i] + b[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
