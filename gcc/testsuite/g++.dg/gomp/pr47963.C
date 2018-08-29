// PR c/47963
// { dg-do compile }
// { dg-options "-fopenmp" }

void
foo (float n)
{
  int A[n][n];	// { dg-error "could not convert|has non-integral type|converted constant expression" }
#pragma omp parallel private(A)
  ;
}
