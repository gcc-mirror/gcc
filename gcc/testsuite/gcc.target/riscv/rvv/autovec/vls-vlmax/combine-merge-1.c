/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include <stdint-gcc.h>

typedef double vnx16df __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_vnx16df (double a, double b, double *out)
{
  vnx16df v = {a, a, a, a, a, b, b, b, b, b, b, b, b, b, b, b};
  *(vnx16df *) out = v;
}

/* { dg-final { scan-assembler-times {vfmv\.v\.f} 1 } } */
/* { dg-final { scan-assembler-times {vid\.v} 1 } } */
/* { dg-final { scan-assembler-times {vmsgtu\.vi} 1 } } */
/* { dg-final { scan-assembler-times {vfmerge\.vfm} 1 } } */
