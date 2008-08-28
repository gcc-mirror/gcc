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

#include <string.h>

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

#ifndef LOMASK
#define LOMASK lmskA
#endif

static void
TEST (void)
{
  union
    {
      __m128 x;
      float f[4];
    } val1[16], val2[16], res[16], chk[16];
  int i,j;
  float tmp;

  for (i = 0; i < 16; i++)
    {
      val1[i].f[0] = 2.;
      val1[i].f[1] = 3.;
      val1[i].f[2] = 4.;
      val1[i].f[3] = 5.;

      val2[i].f[0] = 10.;
      val2[i].f[1] = 100.;
      val2[i].f[2] = 1000.;
      val2[i].f[3] = 10000.;

      tmp = 0.;
      for (j = 0; j < 4; j++)
        if ((HIMASK & (0x10 << j)))
	  tmp += val1[i].f [j] * val2[i].f [j];

      for (j = 0; j < 4; j++)
	if ((LOMASK & (1 << j)))
	  chk[i].f[j] = tmp;
    }

   for (i = 0; i < 16; i++)
     {
       res[i].x = _mm_dp_ps (val1[i].x, val2[i].x, HIMASK | LOMASK);
       if (memcmp (&res[i], &chk[i], sizeof (chk[i])))
	 abort ();
     }
} 
