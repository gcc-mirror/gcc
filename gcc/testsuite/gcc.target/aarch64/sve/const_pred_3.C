/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size(32)));

vnx4si
foo (vnx4si x, vnx4si y)
{
  return (vnx4si) { -1, 0, 0, -1, -1, -1, 0, 0 } ? x : y;
}

/* { dg-final { scan-assembler {\tldr\tp[0-9]+,} } } */
/* { dg-final { scan-assembler {\t\.byte\t1\n\t\.byte\t16\n\t\.byte\t17\n\t\.byte\t0\n} } } */
