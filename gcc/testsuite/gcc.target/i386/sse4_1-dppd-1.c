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

#define lmskN  0x00
#define lmsk0  0x01
#define lmsk1  0x02
#define lmsk01 0x03

#define hmskA  0x30
#define hmsk0  0x10
#define hmsk1  0x20
#define hmsk01 0x30
#define hmskN  0x00

#ifndef HIMASK
#define HIMASK hmskA
#endif

static void
TEST (void)
{
  union
    {
      __m128d x;
      double d[2];
    } val1, val2, res[4];
  int masks[4];
  int i, j;

  val1.d[0] = 2.;
  val1.d[1] = 3.;

  val2.d[0] = 10.;
  val2.d[1] = 100.;

  res[0].x = _mm_dp_pd (val1.x, val2.x, HIMASK | lmskN);
  res[1].x = _mm_dp_pd (val1.x, val2.x, HIMASK | lmsk0);
  res[2].x = _mm_dp_pd (val1.x, val2.x, HIMASK | lmsk1);
  res[3].x = _mm_dp_pd (val1.x, val2.x, HIMASK | lmsk01);

  masks[0] = HIMASK | lmskN;
  masks[1] = HIMASK | lmsk0;
  masks[2] = HIMASK | lmsk1;
  masks[3] = HIMASK | lmsk01; 

  for (i = 0; i < 4; i++)
    {
      double tmp = 0.;

      for (j = 0; j < 2; j++)
	if (HIMASK & (0x10 << j))
	  tmp = tmp + (val1.d[j] * val2.d[j]);

      for (j = 0; j < 2; j++)
	if ((masks[i] & (1 << j)) && res[i].d[j] != tmp)
	  abort ();
   }
} 
