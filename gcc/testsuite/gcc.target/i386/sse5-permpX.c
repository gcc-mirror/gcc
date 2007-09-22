/* { dg-do run } */
/* { dg-require-effective-target sse5 } */
/* { dg-options "-O2 -msse5" } */

#include "sse5-check.h"

#include <bmmintrin.h>
#include <string.h>

union
{
  __m128 x[2];
  __m128d y[2];
  __m128i z[2];
  float f[8];
  double d[4];
  int i[8];
  long li[4];
} dst, res, src1, src2, src3;


static void
init_ddata ()
{
  int i;
  for (i = 0; i < 4; i++)
    {
      src1.d[i] = i;
      src2.d[i] = i + 2;
    }
 
  src3.li[0] = 3;
  src3.li[1] = 0;
  src3.li[2] = 1;
  src3.li[3] = 2;

  res.d[0] = 3.0;
  res.d[1] = 0.0;
  res.d[2] = 3.0;
  res.d[3] = 4.0;
}


static void 
init_fdata ()
{
  int i;
  for (i = 0; i < 8; i++)
    {
      src1.f[i] = i;
      src2.f[i] = i + 2;
    }

  src3.i[0] = 7;
  src3.i[1] = 5;
  src3.i[2] = 1;
  src3.i[3] = 2;
  src3.i[4] = 0;
  src3.i[5] = 4;
  src3.i[6] = 3;
  src3.i[7] = 6; 

  res.f[0] = 5.0;
  res.f[1] = 3.0;
  res.f[2] = 1.0;
  res.f[3] = 2.0;
  res.f[4] = 4.0;
  res.f[5] = 6.0;
  res.f[6] = 7.0;
  res.f[7] = 8.0;
}

static int
check_permpd ()
{
  int i, check_fails = 0;

  for (i = 0; i < 4; i++)
    {
      if (res.d[i] != dst.d[i])
	check_fails++;
    }
  return check_fails++;
}

static int
check_permps ()
{
  int i, check_fails = 0;

  for (i = 0; i < 8; i++)
    {
      if (res.f[i] != dst.f[i])
	check_fails++;
    }
  return check_fails++;
}

static void
sse5_test (void)
{
  int i;
  init_ddata();

  for (i = 0; i < 2; i++)
    dst.y[i] = _mm_perm_pd (src1.y[i], src2.y[i], src3.z[i]);
  
  if (check_permpd ())
    abort ();
  
  init_fdata();
  
  for (i = 0; i < 2; i++)
    dst.x[i] = _mm_perm_ps (src1.x[i], src2.x[i], src3.z[i]);
   
  if (check_permps ())
    abort (); 
}


