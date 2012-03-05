/* Test case for SHIFT_COUNT_TRUNCATED on Loongson.  */

/* { dg-do run } */
/* loongson.h does not handle or check for MIPS16ness.  There doesn't
   seem any good reason for it to, given that the Loongson processors
   do not support MIPS16.  */
/* { dg-options "isa=loongson -mhard-float -mno-mips16 -O1" } */
/* See PR 52155.  */
/* { dg-options "isa=loongson -mhard-float -mno-mips16 -O1 -mlong64" { mips*-*-elf* && ilp32 } } */

#include "loongson.h"
#include <assert.h>

typedef union { int32x2_t v; int32_t a[2]; } int32x2_encap_t;

void
main1 (int shift)
{
  int32x2_encap_t s;
  int32x2_encap_t r;

  s.a[0] = 0xffffffff;
  s.a[1] = 0xffffffff;
  /* Loongson SIMD use low-order 7 bits to specify the shift amount.
     Thus V2SI << 0x40 == 0.  The below expression 'shift & 0x3f' will be
     mis-optimized as 'shift', if SHIFT_COUNT_TRUNCATED is nonzero.  */
  r.v = psllw_s (s.v, (shift & 0x3f));
  assert (r.a[0] == 0xffffffff);
  assert (r.a[1] == 0xffffffff);
}

int
main (void)
{
  main1 (0x40);
  return 0;
}
