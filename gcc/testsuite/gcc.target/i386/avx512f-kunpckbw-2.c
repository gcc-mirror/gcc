/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

static __mmask16 __attribute__((noinline,noclone))
unpack (__mmask16 arg1, __mmask16 arg2)
{
  __mmask16 res;

  res = _mm512_kunpackb (arg1, arg2);

  return res;
}

void
TEST (void)
{
  if (unpack (0x07, 0x70) != 0x0770)
    __builtin_abort ();
}
