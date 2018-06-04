/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <stdint.h>

typedef int64_t vnx2di __attribute__((vector_size(32)));

vnx2di
foo (vnx2di x, vnx2di y)
{
  return (vnx2di) { -1, 0, 0, -1 } ? x : y;
}

/* { dg-final { scan-assembler {\tldr\tp[0-9]+,} } } */
/* { dg-final { scan-assembler {\t\.byte\t1\n\t\.byte\t0\n\t\.byte\t0\n\t\.byte\t1\n} } } */
