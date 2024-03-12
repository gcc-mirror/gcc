/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include <stdint-gcc.h>

typedef double vnx8df __attribute__ ((vector_size (64)));
typedef double vnx16df __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_vnx8df (double a, double b, double *out)
{
  vnx8df v = {a, b, a, b, a, b, a, b};
  *(vnx8df *) out = v;
}

__attribute__ ((noipa)) void
f_vnx16df (double a, double b, double *out)
{
  vnx16df v = {a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b};
  *(vnx16df *) out = v;
}

/* { dg-final { scan-assembler-times {vmv\.s\.x\tv[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfmerge\.vfm\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0} 1 } } */
