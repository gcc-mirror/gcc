/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include <string.h>

#define NUM 20

static void
init_movntdqa (int *src)
{
  int i, j, sign = 1;

  for (i = 0; i < NUM; i++)
    for (j = 0; j < 4; j++)
      {
	src[i * 4 + j] = j * i * i * sign;
	sign = -sign;
      }
}

static void
sse4_1_test (void)
{
  union
    {
      __m128i x[NUM];
      int i[NUM * 4];
    } dst, src;
  int i;

  init_movntdqa (src.i);

  for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_stream_load_si128 (&src.x[i]);

  for (i = 0; i < NUM; i++)
    if (memcmp (&dst.x[i], &src.x[i], sizeof(src.x[i])))
      abort ();
}
