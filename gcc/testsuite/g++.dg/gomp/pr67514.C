// PR c++/67514
// { dg-do compile }
// { dg-options "-fopenmp" }

template <class T>
void
foo (T x, T y)
{
  #pragma omp parallel
  #pragma omp for simd
  for (T i = x; i < y; ++i)
    ;
  #pragma omp parallel
  #pragma omp for simd collapse (2)
  for (T i = x; i < y; ++i)
    for (T j = x; j < y; j++)
      ;
}

void
bar (int *x, int *y)
{
  foo (x, y);
}

void
baz (int x, int y)
{
  foo (x, y);
}
