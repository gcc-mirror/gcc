// PR c++/84556
// { dg-do compile }
// { dg-options "-std=c++17 -fopenmp-simd" }

void
foo ()
{
  auto x = [] ()
  {
    #pragma omp simd
    for (int i = 0; i < 8; ++i)
      ;
  };
}
