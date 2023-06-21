/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d" } */

#include <stdint-gcc.h>

typedef int8_t vnx8qi __attribute__ ((vector_size (8)));
typedef int8_t vnx16qi __attribute__ ((vector_size (16)));
typedef int8_t vnx32qi __attribute__ ((vector_size (32)));
typedef int8_t vnx64qi __attribute__ ((vector_size (64)));
typedef int8_t vnx128qi __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_vnx8qi (int8_t a, int8_t b, int8_t c, int8_t d, int8_t *out)
{
  vnx8qi v = {a, b, c, d, a, b, c, d};
  *(vnx8qi *) out = v;
}

__attribute__ ((noipa)) void
f_vnx16qi (int8_t a, int8_t b, int8_t c, int8_t d, int8_t *out)
{
  vnx16qi v = {a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d};
  *(vnx16qi *) out = v;
}

__attribute__ ((noipa)) void
f_vnx32qi (int8_t a, int8_t b, int8_t c, int8_t d, int8_t *out)
{
  vnx32qi v = {a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
	       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d};
  *(vnx32qi *) out = v;
}

__attribute__ ((noipa)) void
f_vnx64qi (int8_t a, int8_t b, int8_t c, int8_t d, int8_t *out)
{
  vnx64qi v = {a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
	       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
	       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
	       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d};
  *(vnx64qi *) out = v;
}

__attribute__ ((noipa)) void
f_vnx128qi (int8_t a, int8_t b, int8_t c, int8_t d, int8_t *out)
{
  vnx128qi v
    = {a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d,
       a, b, c, d, a, b, c, d, a, b, c, d, a, b, c, d};
  *(vnx128qi *) out = v;
}

/* { dg-final { scan-assembler-times {vmv.v.x\tv[0-9]+,\s*[a-x0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {slli} 15 } } */
/* { dg-final { scan-assembler-times {or} 15 } } */
