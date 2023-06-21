/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d" } */

#include <stdint-gcc.h>

typedef int64_t vnx2di __attribute__ ((vector_size (16)));
typedef int64_t vnx4di __attribute__ ((vector_size (32)));
typedef int64_t vnx8di __attribute__ ((vector_size (64)));
typedef int64_t vnx16di __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_vnx2di (int64_t a, int64_t b, int64_t *out)
{
  vnx2di v = {a, b};
  *(vnx2di *) out = v;
}

__attribute__ ((noipa)) void
f_vnx4di (int64_t a, int64_t b, int64_t c, int64_t d, int64_t *out)
{
  vnx4di v = {a, b, c, d};
  *(vnx4di *) out = v;
}

__attribute__ ((noipa)) void
f_vnx8di (int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, int64_t g, int64_t h, int64_t *out)
{
  vnx8di v = {a, b, c, d, e, f, g, h};
  *(vnx8di *) out = v;
}

__attribute__ ((noipa)) void
f_vnx16di (int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f,
	   int64_t g, int64_t h, int64_t a2, int64_t b2, int64_t c2, int64_t d2,
	   int64_t e2, int64_t f2, int64_t g2, int64_t h2, int64_t *out)
{
  vnx16di v = {a, b, c, d, e, f, g, h, a2, b2, c2, d2, e2, f2, g2, h2};
  *(vnx16di *) out = v;
}

/* { dg-final { scan-assembler-times {vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 52 } } */
