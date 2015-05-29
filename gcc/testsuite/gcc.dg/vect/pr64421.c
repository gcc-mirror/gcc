/* PR middle-end/64421 */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#pragma omp declare simd linear (y) notinbranch
int foo (int x, int y) __asm ("bar");

#pragma omp declare simd linear (y) notinbranch
int
foo (int x, int y)
{
  return x + y;
}

int a[1024] = { 1, 2 };

int
main ()
{
  int i;
  check_vect ();
  #pragma omp simd
  for (i = 0; i < 1024; i++)
    a[i] = foo (a[i], i);
  if (a[0] != 1 || a[1] != 3)
    abort ();
  for (i = 2; i < 1024; i++)
    if (a[i] != i)
      abort ();
  return 0;
}

