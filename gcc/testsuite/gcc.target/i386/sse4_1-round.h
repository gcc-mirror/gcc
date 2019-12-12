#include <smmintrin.h>
#include "math_m_pi.h"

#define NUM 64

static void
init_round (FP_T *src)
{
  int i, sign = 1;
  FP_T f = rand ();

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

static FP_T
do_round (FP_T f, int type)
{
  unsigned short saved_cw, new_cw, clr_mask;
  FP_T ret;

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
  FP_T f;
  union
    {
      VEC_T x[NUM / LOOP_INCREMENT];
      FP_T f[NUM];
    } dst, src;

  init_round (src.f);

  for (i = 0; i < NUM / LOOP_INCREMENT; i++)
    dst.x[i] =  ROUND_INTRIN (src.x[i], ROUND_MODE);

  for (i = 0; i < NUM; i += CHECK_LOOP_INCREMENT)
    {
      f = do_round (src.f[i], CHECK_ROUND_MODE);
     if (f != dst.f[i])
       abort ();
    }

  if (_MM_FROUND_TO_NEAREST_INT != 0x00
      || _MM_FROUND_TO_NEG_INF != 0x01
      || _MM_FROUND_TO_POS_INF != 0x02
      || _MM_FROUND_TO_ZERO != 0x03
      || _MM_FROUND_CUR_DIRECTION != 0x04
      || _MM_FROUND_RAISE_EXC != 0x00
      || _MM_FROUND_NO_EXC != 0x08
      || _MM_FROUND_NINT != 0x00
      || _MM_FROUND_FLOOR != 0x01
      || _MM_FROUND_CEIL != 0x02
      || _MM_FROUND_TRUNC != 0x03
      || _MM_FROUND_RINT != 0x04
      || _MM_FROUND_NEARBYINT != 0x0C)
    abort ();
}
