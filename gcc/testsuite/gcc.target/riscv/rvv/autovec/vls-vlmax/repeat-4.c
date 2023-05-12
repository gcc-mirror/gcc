/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include <stdint-gcc.h>

typedef float vnx4sf __attribute__ ((vector_size (16)));
typedef float vnx8sf __attribute__ ((vector_size (32)));
typedef float vnx16sf __attribute__ ((vector_size (64)));
typedef float vnx32sf __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_vnx4sf (float a, float b, float *out)
{
  vnx4sf v = {a, b, a, b};
  *(vnx4sf *) out = v;
}

__attribute__ ((noipa)) void
f_vnx8sf (float a, float b, float *out)
{
  vnx8sf v = {a, b, a, b, a, b, a, b};
  *(vnx8sf *) out = v;
}

__attribute__ ((noipa)) void
f_vnx16sf (float a, float b, float *out)
{
  vnx16sf v = {a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b};
  *(vnx16sf *) out = v;
}

__attribute__ ((noipa)) void
f_vnx32sf (float a, float b, float *out)
{
  vnx32sf v = {a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b};
  *(vnx32sf *) out = v;
}

/* { dg-final { scan-assembler-times {vmv\.v\.x\tv[0-9]+,\s*[a-x0-9]+} 4 } } */
