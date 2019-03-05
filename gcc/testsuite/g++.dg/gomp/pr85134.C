// PR c++/85134
// { dg-do compile }
// { dg-options "-std=c++14 -fopenmp" }

void
foo (int i)
{
  constexpr int x[i] = {};	// { dg-error "17:'constexpr' variable 'x' has variably-modified type" }
#pragma omp parallel shared(x)
  ;
}
