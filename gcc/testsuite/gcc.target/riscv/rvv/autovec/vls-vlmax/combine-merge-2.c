/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include <stdint-gcc.h>

typedef int64_t vnx16di __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_vnx16di (int64_t a, int64_t b, int64_t *out)
{
  vnx16di v = {a, a, a, a, a, b, b, b, b, b, b, b, b, b, b, b};
  *(vnx16di *) out = v;
}

/* { dg-final { scan-assembler-times {vmv\.v\.x} 1 } } */
/* { dg-final { scan-assembler-times {vid\.v} 1 } } */
/* { dg-final { scan-assembler-times {vmsgtu\.vi} 1 } } */
/* { dg-final { scan-assembler-times {vmerge\.vxm} 1 } } */
