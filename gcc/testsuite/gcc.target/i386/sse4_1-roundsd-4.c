/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-skip-if "no M_PI" { vxworks_kernel } } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include <math.h>
#include <string.h>

#define NUM 64

static void
init_round (double *src)
{
  int i, sign = 1;
  double d = rand ();

  for (i = 0; i < NUM; i++)
    {
      src[i] = (i + 1)* d * M_PI * sign;
      if (i < (NUM / 2))
	{
          if ((i % 6) == 0)
	    d = d * src[i];
        }
      else if (i == (NUM / 2))
	d = rand ();
      else if ((i % 6) == 0)
	d = 1 / (d * (i + 1) * src[i] * M_PI *sign);
      sign = -sign;
    }
}

static double
do_round (double f, int type)
{
  unsigned short saved_cw, new_cw, clr_mask;
  double ret;

  if ((type & 4))
    {
      type = 0;
      clr_mask = 0xFFFF;
    }
  else
    {
      type = 0x003F | ((type & 3) << 10);
      clr_mask = ~0x0C3F;
    }

  __asm__ ("fnstcw %0" : "=m" (saved_cw));

  new_cw = saved_cw & clr_mask;
  new_cw |= type;

  __asm__ ("fldcw %2\n\t"
	   "frndint\n\t"
	   "fldcw %3" : "=t" (ret)
		      : "0" (f), "m" (new_cw), "m" (saved_cw));
  return ret;
}

static void
sse4_1_test (void)
{
  int i;
  double f;
  union
    {
      __m128d x[NUM / 2];
      double d[NUM];
    } dst, src;

  init_round (src.d);
  memset (&dst, 0, NUM * sizeof(double));

  for (i = 0; i < NUM / 2 ; i++)
    dst.x[i] =  _mm_round_sd (dst.x[i], src.x[i], _MM_FROUND_TRUNC);

  for (i = 0; i < NUM; i += 2)
    {
      if (dst.d[i + 1] != 0.0)
	abort ();

      f = do_round (src.d[i], 0x03);
      if (f != dst.d[i])
	abort ();
    }
}
