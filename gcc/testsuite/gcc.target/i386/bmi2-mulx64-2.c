/* { dg-do run { target { bmi2 && { ! ia32 } } } } */
/* { dg-options "-mbmi2 -O2" } */

#include <x86intrin.h>
#include "bmi2-check.h"

__attribute__((noinline))
unsigned __int128
calc_mul_u64 (unsigned long long volatile a, unsigned long long b)
{
  unsigned __int128 res = 0;
  int i;
  for (i = 0; i < b; ++i)
    res += (unsigned __int128) a;

  return res;
}

__attribute__((noinline))
unsigned long long
calc_mulx_u64 (unsigned long long x,
	       unsigned long long y,
	       unsigned long long *res_h)
{
  return _mulx_u64 (x, y, res_h);
}


static void
bmi2_test ()
{
  unsigned i;
  unsigned long long a = 0xce7ace0ce7ace0;
  unsigned long long b = 0xface;
  unsigned long long res_l, res_h;
  unsigned __int128 res, res_ref;

  for (i=0; i<5; ++i) {
    a = a * (i + 1);
    b = b / (i + 1);

    res_ref = calc_mul_u64 (a, b);

    res_l = calc_mulx_u64 (a, b, &res_h);

    res = ((unsigned __int128) res_h << 64) | res_l;

    if (res != res_ref)
      abort();
  }
}
