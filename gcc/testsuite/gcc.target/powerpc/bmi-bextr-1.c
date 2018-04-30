/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>
#include "bmi-check.h"

long long calc_bextr_u64 (unsigned long long src1,
			  unsigned long long src2)
{
  long long res = 0;
  unsigned char start = (src2 & 0xff);
  unsigned char len = (int) ((src2 >> 8) & 0xff);
  if (start < 64) {
    unsigned i;
    unsigned last = (start+len) < 64 ? start+len : 64;

    src1 >>= start;
    for (i=start; i<last; ++i) {
      res |= (src1 & 1) << (i-start);
      src1 >>= 1;
    }
  }

  return res;
}

static void
bmi_test ()
{
  unsigned i;
  unsigned char start, len;
  unsigned long long src1 = 0xfacec0ffeefacec0;
  unsigned long long res, res_ref, src2;

  for (i=0; i<5; ++i) {
    start = (i * 1983) % 64;
    len = (i + (i * 1983)) % 64;

    src1 = src1 * 3;
    src2 = start | (((unsigned long long)len) << 8);

    res_ref = calc_bextr_u64 (src1, src2);
    res = __bextr_u64 (src1, src2);

    if (res != res_ref)
      abort ();
  }
}
