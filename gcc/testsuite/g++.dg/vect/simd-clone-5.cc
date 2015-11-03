// { dg-require-effective-target vect_simd_clones }
// { dg-additional-options "-fopenmp-simd -fno-inline" }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-additional-sources "simd-clone-4.cc" }

#include "simd-clone-4.h"

#pragma omp declare simd notinbranch
template <int N>
int
S<N>::f0 (int x)
{
  return x + s;
}

#pragma omp declare simd notinbranch uniform(this)
template <int N>
int
S<N>::f1 (int x)
{
  return x + s;
}

#pragma omp declare simd notinbranch linear(this:sizeof(this)/sizeof(this))
template <int N>
int
S<N>::f2 (int x)
{
  return x + this->S::s;
}

#pragma omp declare simd uniform(this) aligned(this:32) linear(x)
template <int N>
int
T<N>::f3 (int x)
{
  return t[x];
}

template struct S<0>;
template struct T<0>;
