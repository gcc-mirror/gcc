// PR c++/98383
// { dg-options "-fopenmp -O1" }

int bar (const int &);

void
foo (int *a)
{
#pragma omp simd
  for (int i = 0; i < bar (8); ++i)
    a[i]++;
#pragma omp simd
  for (int i = bar (9); i < 16; ++i)
    a[i]++;
#pragma omp simd
  for (int i = 0; i < 32; i += bar (10))
    a[i]++;
}
