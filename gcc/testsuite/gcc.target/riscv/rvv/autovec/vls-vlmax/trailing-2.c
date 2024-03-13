/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -mrvv-max-lmul=m8 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef double v16df __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_v16df (double a, double b, double c, double d, double *out)
{
  v16df v = {a, b, c, d, d, d, d, d, d, d, d, d, d, d, d, d};
  *(v16df *) out = v;
}

/* { dg-final { scan-assembler-times {vfslide1up\.vf\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 3 } } */
