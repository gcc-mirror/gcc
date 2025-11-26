/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd --param vect-partial-vector-usage=2 -w" } */
/* { dg-additional-options "-mavx512f" { target avx512f_runtime } } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-additional-sources vect-simd-clone-22a.c linkonly } */

#include <fenv.h>
#include "tree-vect.h"

#pragma omp declare simd simdlen(16) inbranch
float __attribute__((const)) baz (float x, float y);

float a[1024];
int c[1024];

void __attribute__((noipa))
foo (int n, float * __restrict b)
{
  for (int i = 0; i < n; ++i)
    {
      float aa = a[i];
      float bb = b[i];
      if (c[i] == 0)
	aa = baz (aa, bb);
      a[i] = aa;
    }
}

float b[1024];

int main()
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 1020; ++i)
    a[i] = b[i] = 2;
  foo (1020, b);
  if (fetestexcept (FE_DIVBYZERO) || fetestexcept (FE_INVALID))
    abort ();
#pragma GCC novector
  for (int i = 0; i < 1020; ++i)
    if (a[i] != 1)
      abort ();
}
