// PR middle-end/68762
// { dg-require-effective-target vect_simd_clones }
// { dg-additional-options "-fopenmp-simd -fno-inline" }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-additional-sources "pr68762-2.cc" }

#include "pr68762.h"

double v[64];

double
bar ()
{
  double sum = 0.0;
  #pragma omp simd reduction (+: sum)
  for (int i = 0; i < 64; i++)
    sum += foo (v[i]);
  return sum;
}

int
main ()
{
  if (bar () != 0.0)
    __builtin_abort ();
}
