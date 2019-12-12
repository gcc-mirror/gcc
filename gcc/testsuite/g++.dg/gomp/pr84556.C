// PR c++/84556
// { dg-do compile { target c++17 } }
// { dg-options "-fopenmp-simd" }

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
