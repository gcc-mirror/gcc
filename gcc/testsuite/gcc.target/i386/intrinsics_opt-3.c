/* { dg-do compile } */
/* { dg-options "-O -msse2" } */

#include <emmintrin.h>

double f(){
  __m128d x = _mm_set1_pd (0.);
  double r = 42;
  _mm_storeh_pd (&r, x);
  return r;
}

/* { dg-final { scan-assembler-not "unpckhpd" } } */
