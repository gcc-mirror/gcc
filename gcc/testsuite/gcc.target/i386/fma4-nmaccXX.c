/* { dg-do run } */
/* { dg-require-effective-target fma4 } */
/* { dg-options "-O0 -mfma4" } */

#include "fma4-check.h"

#include <x86intrin.h>
#include <string.h>

#define NUM 20

union
{
  __m128 x[NUM];
  float f[NUM * 4];
  __m128d y[NUM];
  double d[NUM * 2];
} dst, res, src1, src2, src3;

/* Note that in macc*,msub*,mnmacc* and mnsub* instructions, the intermdediate 
   product is not rounded, only the addition is rounded. */

static void
init_nmaccps ()
{
  int i;
  for (i = 0; i < NUM * 4; i++)
    {
      src1.f[i] = i;
      src2.f[i] = i + 10;
      src3.f[i] = i + 20;
    }
}

static void
init_nmaccpd ()
{
  int i;
  for (i = 0; i < NUM * 4; i++)
    {
      src1.d[i] = i;
      src2.d[i] = i + 10;
      src3.d[i] = i + 20;
    }
}

static int
check_nmaccps ()
{
  int i, j, check_fails = 0;
  for (i = 0; i < NUM * 4; i = i + 4)
    for (j = 0; j < 4; j++)
      {
	res.f[i + j] = - (src1.f[i + j] * src2.f[i + j]) + src3.f[i + j];
	if (dst.f[i + j] != res.f[i + j]) 
	  check_fails++;
      }
  return check_fails;
}

static int
check_nmaccpd ()
{
  int i, j, check_fails = 0;
  for (i = 0; i < NUM * 2; i = i + 2)
    for (j = 0; j < 2; j++)
      {
	res.d[i + j] = - (src1.d[i + j] * src2.d[i + j]) + src3.d[i + j];
	if (dst.d[i + j] != res.d[i + j]) 
	  check_fails++;
      }
  return check_fails;
}

static int
check_nmaccss ()
{
  int i, j, check_fails = 0;
  for (i = 0; i < NUM * 4; i = i + 4)
    {
      res.f[i] = - (src1.f[i] * src2.f[i]) + src3.f[i];
      if (dst.f[i] != res.f[i]) 
	check_fails++;
    }	
  return check_fails;
}

static int
check_nmaccsd ()
{
  int i, j, check_fails = 0;
  for (i = 0; i < NUM * 2; i = i + 2)
    {
      res.d[i] = - (src1.d[i] * src2.d[i]) + src3.d[i];
      if (dst.d[i] != res.d[i]) 
	check_fails++;
    }
  return check_fails;
}

static void
fma4_test (void)
{
  int i;

  init_nmaccps ();
  
  for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_nmacc_ps (src1.x[i], src2.x[i], src3.x[i]);
  
  if (check_nmaccps ()) 
    abort ();
  
  for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_nmacc_ss (src1.x[i], src2.x[i], src3.x[i]);
  
  if (check_nmaccss ()) 
    abort ();

  init_nmaccpd ();
  
  for (i = 0; i < NUM; i++)
    dst.y[i] = _mm_nmacc_pd (src1.y[i], src2.y[i], src3.y[i]);
  
  if (check_nmaccpd ()) 
    abort ();

  for (i = 0; i < NUM; i++)
    dst.y[i] = _mm_nmacc_sd (src1.y[i], src2.y[i], src3.y[i]);
  
  if (check_nmaccsd ()) 
    abort ();
}
