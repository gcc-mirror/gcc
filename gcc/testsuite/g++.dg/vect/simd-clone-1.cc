// { dg-require-effective-target vect_simd_clones }
// { dg-additional-options "-fopenmp-simd -fno-inline" }
// { dg-additional-options "-mavx" { target avx_runtime } }

#include "../../gcc.dg/vect/tree-vect.h"

struct S
{
  int s;
  #pragma omp declare simd notinbranch linear(x)
  int f (int x);
};

#pragma omp declare simd notinbranch linear(x)
int
S::f (int x)
{
  return x;
}

template <int N>
struct T
{
  int t;
  #pragma omp declare simd notinbranch linear(x)
  int f (int x);
};

#pragma omp declare simd notinbranch linear(x)
template <int N>
int
T<N>::f (int x)
{
  return x;
}

void
do_main ()
{
  int i, r = 0;
  S s;
  T<0> t;
  #pragma omp simd reduction(+:r)
  for (i = 0; i < 64; i++)
    r += s.f (i) + t.f (i);
  if (r != 64 * 63)
    abort ();
}

int
main ()
{
  check_vect ();
  do_main ();
}
