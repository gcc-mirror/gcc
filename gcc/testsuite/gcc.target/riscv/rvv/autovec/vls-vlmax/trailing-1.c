/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -mrvv-max-lmul=m8 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int64_t v16di __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_v16di (int64_t a, int64_t b, int64_t c, int64_t d, int64_t *out)
{
  v16di v = {a, b, c, d, d, d, d, d, d, d, d, d, d, d, d, d};
  *(v16di *) out = v;
}

/* { dg-final { scan-assembler-times {vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 3 } } */
