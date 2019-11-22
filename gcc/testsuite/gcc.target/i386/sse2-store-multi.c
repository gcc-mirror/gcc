/* { dg-do compile } */
/* { dg-options "-march=corei7 -O2" } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } } */

#include <emmintrin.h>

double a[8];

void store_1 (__m128d val)
{
  _mm_store_sd (&a[1], val);
  _mm_storeh_pd (&a[2], val);
}

void store_2 (__m128d val, double *a)
{
  _mm_store_sd (&a[1], val);
  _mm_storeh_pd (&a[2], val);
}

/* { dg-final { scan-assembler-times "movup" 2 } } */
