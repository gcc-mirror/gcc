/* { dg-do run { target { bmi2 && { ia32 } } } } */
/* { dg-options "-mbmi2 -O2" } */

#include "bmi2-check.h"

__attribute__((noinline))
unsigned long long
calc_mul_u32 (unsigned volatile a, unsigned b)
{
  unsigned long long res = 0;
  int i;
  for (i = 0; i < b; ++i)
    res += a;

  return res;
}

__attribute__((noinline))
unsigned long long
gen_mulx (unsigned a, unsigned b)
{
  unsigned long long res;

  res = (unsigned long long)a * b;

  return res;
}

static void
bmi2_test ()
{
  unsigned i;
  unsigned a = 0xce7ace0;
  unsigned b = 0xfacefff;
  unsigned long long res, res_ref;

  for (i = 0; i < 5; ++i) {
    a = a * (i + 1);
    b = b / (i + 1);

    res_ref = calc_mul_u32 (a, b);
    res = gen_mulx (a, b);

    if (res != res_ref)
      abort();
  }
}
