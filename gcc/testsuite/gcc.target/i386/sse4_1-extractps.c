/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>

int masks[4];

#define msk0 0x00
#define msk1 0x01
#define msk2 0x02
#define msk3 0x03

static void
TEST (void)
{
  union
    {
      __m128 x;
      float f[4];
    } val1, val2;
  union
    {
      int i;
      float f;
    } res[4];
  float resm[4];
  int i;

  val1.f[0] = 10.;
  val1.f[1] = 2.;
  val1.f[2] = 3.;
  val1.f[3] = 40.;

  val2.f[0] = 77.;
  val2.f[1] = 21.;
  val2.f[2] = 34.;
  val2.f[3] = 49.;

  res[0].i = _mm_extract_ps (val1.x, msk0);
  res[1].i = _mm_extract_ps (val1.x, msk1);
  res[2].i = _mm_extract_ps (val1.x, msk2);
  res[3].i = _mm_extract_ps (val1.x, msk3);

  _MM_EXTRACT_FLOAT (resm[0], val2.x, msk0);
  _MM_EXTRACT_FLOAT (resm[1], val2.x, msk1);
  _MM_EXTRACT_FLOAT (resm[2], val2.x, msk2);
  _MM_EXTRACT_FLOAT (resm[3], val2.x, msk3);
  
  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;

  for( i=0; i < 4; i++ )
    {
      if (res[i].f != val1.f[masks[i]])
	abort ();
      if (resm[i] != val2.f[masks[i]])
	abort ();
    }
}
