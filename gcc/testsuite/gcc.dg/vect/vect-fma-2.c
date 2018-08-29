/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-optimized -fassociative-math -fno-trapping-math -fno-signed-zeros" } */

#include "tree-vect.h"

#define N (VECTOR_BITS * 11 / 64 + 3)

double
dot_prod (double *x, double *y)
{
  double sum = 0;
  for (int i = 0; i < N; ++i)
    sum += x[i] * y[i];
  return sum;
}

/* { dg-final { scan-tree-dump { = \.COND_FMA } "optimized" { target { vect_double && { vect_fully_masked && scalar_all_fma } } } } } */
