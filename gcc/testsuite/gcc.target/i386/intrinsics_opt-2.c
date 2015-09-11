/* { dg-do compile } */
/* { dg-options "-O -ffast-math -msse2 -fdump-tree-optimized" } */

#include <emmintrin.h>

int f(__m128d x){
  x = _mm_sub_pd (x, x);
  x = _mm_mul_pd (x, x);
  double r = 42;
  _mm_storeh_pd (&r, x);
  int z = r == 0;
  return __builtin_constant_p (z) && z;
}

/* { dg-final { scan-tree-dump "return 1;" "optimized" } } */
