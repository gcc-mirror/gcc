/* { dg-do run } */
/* { dg-options "-mmmx -mfpmath=387" } */

#include "mmx-check.h"

#include <mmintrin.h>

typedef float float32x2_t __attribute__ ((vector_size (8)));

float
foo32x2_be (float32x2_t x)
{
  _mm_empty ();
  return x[1];
}

static void
mmx_test (void)
{
  float32x2_t b = { 0.0f, 1.0f };

  if (foo32x2_be (b) != 1.0f)
    abort ();
}
