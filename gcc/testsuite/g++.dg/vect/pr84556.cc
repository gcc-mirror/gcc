// PR c++/84556
// { dg-require-effective-target c++11 }
// { dg-additional-options "-O2 -fopenmp-simd" }
// { dg-additional-options "-mavx" { target avx_runtime } }

int
main ()
{
  int y[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  auto x = [&y] ()
  {
    #pragma omp simd
    for (int i = 0; i < 8; ++i)
      y[i]++;
  };
  x ();
  x ();
#pragma GCC novector
  for (int i = 0; i < 8; ++i)
    if (y[i] != i + 3)
      __builtin_abort ();
}
