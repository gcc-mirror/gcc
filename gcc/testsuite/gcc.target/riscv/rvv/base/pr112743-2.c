/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvfh_zfh -mabi=lp64d -O2" } */

#include <sys/types.h>

union double_union
{
  double d;
  __uint32_t i[2];
};

#define word0(x)  (x.i[1])
#define word1(x)  (x.i[0])

#define P         53
#define Exp_shift 20
#define Exp_msk1  ((__uint32_t)0x100000L)
#define Exp_mask  ((__uint32_t)0x7ff00000L)

double ulp (double _x)
{
  union double_union x, a;
  register int L;

  x.d = _x;
  L = (word0 (x) & Exp_mask) - (P - 1) * Exp_msk1;

  if (L > 0)
    {
      L |= Exp_msk1 >> 4;
      word0 (a) = L;
      word1 (a) = 0;
    }
  else
    {
      L = -L >> Exp_shift;
      if (L < Exp_shift)
	{
	  word0 (a) = 0x80000 >> L;
	  word1 (a) = 0;
	}
      else
	{
	  word0 (a) = 0;
	  L -= Exp_shift;
	  word1 (a) = L >= 31 ? 1 : 1 << (31 - L);
	}
    }

  return a.d;
}
