// PR c++/71257
// { dg-require-effective-target vect_simd_clones }
// { dg-additional-options "-fopenmp-simd -fno-inline" }
// { dg-additional-options "-mavx" { target avx_runtime } }

#include "../../gcc.dg/vect/tree-vect.h"

#define N 1024
struct S { int a; };
int c[N], e[N], f[N];
S d[N];

#pragma omp declare simd linear(ref(b, c) : 1)
int
foo (int a, S &b, int &c)
{
  return a + b.a + c;
}

void
do_main ()
{
  int i;
  for (i = 0; i < N; i++)
    {
      c[i] = i;
      d[i].a = 2 * i;
      f[i] = 3 * i;
    }
  #pragma omp simd
  for (i = 0; i < N; i++)
    e[i] = foo (c[i], d[i], f[i]);
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (e[i] != 6 * i)
      __builtin_abort ();
}

int
main ()
{
  check_vect ();
  return 0;
}
