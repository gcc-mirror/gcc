/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW

#include "avx512f-helper.h"

static __mmask32 __attribute__((noinline,noclone))
unpack (__mmask32 arg1, __mmask32 arg2)
{
  __mmask32 res;

  res = _mm512_kunpackw (arg1, arg2);

  return res;
}

void
TEST (void)
{
  if (unpack (0x07, 0x70) != 0x070070)
    __builtin_abort ();
}
