/* { dg-do run } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

#include <xmmintrin.h>

void __attribute__((noinline))
sse_test (void)
{
  char image[4];
  __m128 image4;
  float out[4] __attribute__ ((aligned (16)));
  int i;

  for (i = 0; i < 4; i++)
    image[i] = i + 1;

  image4 =
    _mm_cvtpi8_ps (_mm_setr_pi8
		   (image[0], image[1], image[2], image[3], 0, 0, 0, 0));
  _mm_store_ps (out, image4);
  _mm_empty ();

  for (i = 0; i < 4; i++)
    if (out[i] != (float) (i + 1))
      abort ();

  image4 =
    _mm_cvtpu8_ps (_mm_setr_pi8
		   (image[0], image[1], image[2], image[3], 0, 0, 0, 0));
  _mm_store_ps (out, image4);
  _mm_empty ();

  for (i = 0; i < 4; i++)
    if (out[i] != (float) (i + 1))
      abort ();
}
