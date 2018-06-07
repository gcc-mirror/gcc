/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size(32)));

vnx16qi
foo (vnx16qi x, vnx16qi y)
{
  return (vnx16qi) { -1, 0, 0, -1, -1, -1, 0, 0,
		     -1, -1, -1, -1, 0, 0, 0, 0,
		     -1, -1, -1, -1, -1, -1, -1, -1,
		     0, 0, 0, 0, 0, 0, 0, 0 } ? x : y;
}

/* { dg-final { scan-assembler {\tldr\tp[0-9]+,} } } */
/* { dg-final { scan-assembler {\t\.byte\t57\n\t\.byte\t15\n\t\.byte\t(255|-1)\n\t\.byte\t0\n} } } */
