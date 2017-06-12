// PR c++/79672
// { dg-do compile }
// { dg-options "-Wduplicated-branches -fopenmp" }
// { dg-require-effective-target fopenmp }

template<int N> void foo()
{
  if (N > 0)
  {
#pragma omp parallel for
    for (int i = 0; i < 10; ++i) ;
  }
}

void bar()
{
  foo<0>();
}
