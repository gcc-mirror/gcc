/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include "math_m_pi.h"
#include <string.h>

#define NUM 64

static void
init_round (float *src)
{
  int i, sign = 1;
  float f = rand ();

  for (i = 0; i < NUM; i++)
    {
      src[i] = (i + 1)* f * M_PI * sign;
      if (i < (NUM / 2))
	{
          if ((i % 6) == 0)
	    f = f * src[i];
        }
      else if (i == (NUM / 2))
	f = rand ();
      else if ((i % 6) == 0)
	f = 1 / (f * (i + 1) * src[i] * M_PI *sign);
      sign = -sign;
    }
}

static float
do_round (float f, int type)
{
  unsigned short saved_cw, new_cw, clr_mask;
  float ret;

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
  int i, j;
  float f;
  union
    {
      __m128 x[NUM / 4];
      float f[NUM];
    } dst, src;

  init_round (src.f);
  memset (&dst, 0, NUM * sizeof(float));

  for (i = 0; i < NUM / 4 ; i++)
    dst.x[i] =  _mm_round_ss (dst.x[i], src.x[i], _MM_FROUND_RINT);

  for (i = 0; i < NUM; i += 4)
    {
      for (j = 0; j < 3; j++)
	if (dst.f[i + j + 1] != 0.0)
	  abort ();

      f = do_round (src.f[i], 0x04);
      if (f != dst.f[i])
	abort ();
    }

  for (i = 0; i < NUM / 4 ; i++)
    dst.x[i] =  _mm_round_ss (dst.x[i], src.x[i], _MM_FROUND_NEARBYINT);

  for (i = 0; i < NUM; i += 4)
    {
      for (j = 0; j < 3; j++)
	if (dst.f[i + j + 1] != 0.0)
	  abort ();

      f = do_round (src.f[i], 0x0c);
      if (f != dst.f[i])
	abort ();
    }
}
