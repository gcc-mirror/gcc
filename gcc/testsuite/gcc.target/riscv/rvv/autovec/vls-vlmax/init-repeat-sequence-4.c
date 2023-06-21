/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include <stdint-gcc.h>

typedef int64_t vnx8di __attribute__ ((vector_size (64)));

__attribute__ ((noipa)) void
f_vnx8di (int64_t a, int64_t b, int64_t c, int64_t d, int64_t *out)
{
  vnx8di v = {a, b, c, d, a, b, c, d};
  *(vnx8di *) out = v;
}

/* { dg-final { scan-assembler-times {vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 7 } } */
