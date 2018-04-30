/* { dg-do run { target bmi } } */
/* { dg-options "-O2 -mbmi -fno-inline" } */

#include <x86intrin.h>

#include "bmi-check.h"

unsigned calc_bextr_u32 (unsigned src1, unsigned src2)
{
  unsigned res = 0;
  unsigned char start = (src2 & 0xff);
  unsigned char len = (int) ((src2 >> 8) & 0xff);
  if (start < 32) {
    unsigned i;
    unsigned last = (start+len) < 32 ? start+len : 32;

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
  unsigned src1 = 0xfacec0ff;
  unsigned res, res_ref, src2;

  for (i=0; i<5; ++i) {
    start = (i * 1983) % 32;
    len = i + (i * 1983) % 32;

    src1 = src1 * 3;
    src2 = start | (((unsigned)len) << 8);

    res_ref = calc_bextr_u32 (src1, src2);
    res = __bextr_u32 (src1, src2);

    if (res != res_ref)
      abort();
  }
}
