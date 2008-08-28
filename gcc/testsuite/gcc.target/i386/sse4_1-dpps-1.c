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
#define lmsk2  0x04
#define lmsk3  0x08
#define lmsk01 0x03
#define lmsk02 0x05
#define lmsk03 0x09
#define lmsk12 0x06
#define lmsk13 0x0A
#define lmsk23 0x0C
#define lmskA  0x0F

#define hmskN  0x00
#define hmskA  0xF0
#define hmsk0  0x10
#define hmsk1  0x20
#define hmsk2  0x40
#define hmsk3  0x80
#define hmsk01 0x30
#define hmsk02 0x50
#define hmsk03 0x90
#define hmsk12 0x60
#define hmsk13 0xA0
#define hmsk23 0xC0

#ifndef HIMASK
#define HIMASK hmskA
#endif

static void
TEST (void)
{
  union
    {
      __m128 x;
      float f[4];
    } val1, val2, res[16];
  int masks[16];
  int i, j;

  val1.f[0] = 2.;
  val1.f[1] = 3.;
  val1.f[2] = 4.;
  val1.f[3] = 5.;

  val2.f[0] = 10.;
  val2.f[1] = 100.;
  val2.f[2] = 1000.;
  val2.f[3] = 10000.;

  res[0].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk0); 
  res[1].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk1); 
  res[2].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk2); 
  res[3].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk3); 
  res[4].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk01); 
  res[5].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk02); 
  res[6].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk03); 
  res[7].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk12); 
  res[8].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk13); 
  res[9].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmsk23); 
  res[10].x = _mm_dp_ps (val1.x, val2.x, HIMASK | (0x0F & ~lmsk0)); 
  res[11].x = _mm_dp_ps (val1.x, val2.x, HIMASK | (0x0F & ~lmsk1)); 
  res[12].x = _mm_dp_ps (val1.x, val2.x, HIMASK | (0x0F & ~lmsk2)); 
  res[13].x = _mm_dp_ps (val1.x, val2.x, HIMASK | (0x0F & ~lmsk3)); 
  res[14].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmskN); 
  res[15].x = _mm_dp_ps (val1.x, val2.x, HIMASK | lmskA); 

  masks[0] = HIMASK | lmsk0; 
  masks[1] = HIMASK | lmsk1; 
  masks[2] = HIMASK | lmsk2; 
  masks[3] = HIMASK | lmsk3; 
  masks[4] = HIMASK | lmsk01; 
  masks[5] = HIMASK | lmsk02; 
  masks[6] = HIMASK | lmsk03; 
  masks[7] = HIMASK | lmsk12; 
  masks[8] = HIMASK | lmsk13; 
  masks[9] = HIMASK | lmsk23; 
  masks[10] = HIMASK | (0x0F & ~lmsk0); 
  masks[11] = HIMASK | (0x0F & ~lmsk1); 
  masks[12] = HIMASK | (0x0F & ~lmsk2); 
  masks[13] = HIMASK | (0x0F & ~lmsk3); 
  masks[14] = HIMASK | lmskN; 
  masks[15] = HIMASK | lmskA; 

  for (i = 0; i <= 15; i++)
    {
      float tmp = 0.;

      for (j = 0; j < 4; j++)
	if ((HIMASK & (0x10 << j)))
	  tmp += val1.f[j] * val2.f[j];

      for (j = 0; j < 4; j++)
	if ((masks[i] & (1 << j)) && res[i].f[j] != tmp)
	  abort ();
   }
} 
