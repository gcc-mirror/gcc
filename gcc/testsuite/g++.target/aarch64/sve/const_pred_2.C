/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <stdint.h>

typedef int16_t vnx8hi __attribute__((vector_size(32)));

vnx8hi
foo (vnx8hi x, vnx8hi y)
{
  return (vnx8hi) { -1, 0, 0, -1, -1, -1, 0, 0,
		    -1, -1, -1, -1, 0, 0, 0, 0 } ? x : y;
}

/* { dg-final { scan-assembler {\tldr\tp[0-9]+,} } } */
/* { dg-final { scan-assembler {\t\.byte\t65\n\t\.byte\t5\n\t\.byte\t85\n\t\.byte\t0\n} } } */
