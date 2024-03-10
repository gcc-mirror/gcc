/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvl1024b -mabi=lp64d" } */

#include <stdint-gcc.h>

typedef int64_t vnx16di __attribute__ ((vector_size (1024)));

__attribute__ ((noipa)) void
f_vnx16di (int64_t a, int64_t b, int64_t *out)
{
  vnx16di v = {
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
  };
  *(vnx16di *) out = v;
}

/* { dg-final { scan-assembler-times {vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmerge\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0} 1 } } */
