// PR c++/67523
// { dg-do compile }
// { dg-options "-fopenmp" }

struct S { int s; };

template <typename T>
void foo (T &x, T &y)
{
#pragma omp for simd
  for (T i = x; i < y; i++)	// { dg-error "used with class iteration variable" }
    ;
#pragma omp parallel for simd
  for (T i = x; i < y; i++)	// { dg-error "used with class iteration variable" }
    ;
#pragma omp target teams distribute parallel for simd
  for (T i = x; i < y; i++)	// { dg-error "used with class iteration variable" }
    ;
#pragma omp target teams distribute simd
  for (T i = x; i < y; i++)	// { dg-error "used with class iteration variable" }
    ;
}

void
bar ()
{
  S x, y;
  foo <S> (x, y);
}
