// PR c++/84430
// { dg-do compile { target c++11 } }

void
foo ()
{
  auto a = [] {
    #pragma omp simd
    for (int i = 0; i < 10; ++i)
      ;
  };
}
