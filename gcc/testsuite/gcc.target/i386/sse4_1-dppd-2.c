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
#define lmsk01 0x03

#define hmskA  0x30
#define hmsk0  0x10
#define hmsk1  0x20
#define hmsk01 0x30
#define hmskN  0x00

#ifndef HIMASK
#define HIMASK hmskA
#endif

#ifndef LOMASK
#define LOMASK lmsk01
#endif

static void
TEST (void)
{
  union
    {
      __m128d x;
      double d[2];
    } val1[4], val2[4], res[4], chk[4];
  int i, j;
  double tmp;

  for (i = 0; i < 4; i++)
    {
      val1[i].d [0] = 2.;
      val1[i].d [1] = 3.;

      val2[i].d [0] = 10.;
      val2[i].d [1] = 100.;

      tmp = 0.;
      for (j = 0; j < 2; j++)
	if ((HIMASK & (0x10 << j)))
	  tmp += val1[i].d [j] * val2[i].d [j];

      for (j = 0; j < 2; j++)
        if ((LOMASK & (1 << j)))
	  chk[i].d[j] = tmp;
    }

  for (i = 0; i < 4; i++)
    {
      res[i].x = _mm_dp_pd (val1[i].x, val2[i].x, HIMASK | LOMASK); 
      if (memcmp (&res[i], &chk[i], sizeof (chk[i])))
	abort ();
    }
}
