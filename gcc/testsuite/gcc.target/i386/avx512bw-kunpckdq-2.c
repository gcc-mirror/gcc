/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW

#include "avx512f-helper.h"

static __mmask64 __attribute__((noinline,noclone))
unpack (__mmask64 arg1, __mmask64 arg2)
{
  __mmask64 res;

  res = _mm512_kunpackd (arg1, arg2);

  return res;
}

void
TEST (void)
{
  if (unpack (0x07UL, 0x70UL) != 0x0700000070UL)
    __builtin_abort ();
}
