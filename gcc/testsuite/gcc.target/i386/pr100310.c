/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
#include <immintrin.h>

double *p;
volatile __m512d x;
volatile __mmask8 m;

void foo()
{
  x = _mm512_mask_expandloadu_pd (x, 255, p);
}
