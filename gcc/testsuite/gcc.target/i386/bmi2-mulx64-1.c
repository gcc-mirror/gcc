/* { dg-do run { target { bmi2 && { ! ia32 } } } } */
/* { dg-options "-mbmi2 -O2" } */

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

static void
bmi2_test ()
{
  unsigned i;
  unsigned long long a = 0xce7ace0ce7ace0;
  unsigned long long b = 0xface;
  unsigned __int128 res, res_ref;

  for (i=0; i<5; ++i) {
    a = a * (i + 1);
    b = b / (i + 1);

    res_ref = calc_mul_u64 (a, b);
    res = (unsigned __int128) a * b;

    if (res != res_ref)
      abort();
  }
}
