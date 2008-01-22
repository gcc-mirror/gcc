// PR c++/34607
// { dg-do compile }
// { dg-options "-fopenmp" }

void
foo ()
{
#pragma omp for
  for (int i =; i < 2; ++i)	// { dg-error "expected primary-expression" }
    ;
#pragma omp for
  for (T i = 54; i < 56; i++)	// { dg-error "was not declared|expected" }
    ;
  T j;				// { dg-error "was not declared|expected" }
#pragma omp for
  for (j = 1; j < 3; j++)	// { dg-error "was not declared" }
    ;				// { dg-error "expected" }
}
