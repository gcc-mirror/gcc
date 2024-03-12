/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

int8x16_t f_s8(int8_t x)
{
  return (int8x16_t) { x, 1, x, 2, x, 3, x, 4,
                       x, 5, x, 6, x, 7, x, 8 };
}

/* { dg-final { scan-assembler {\tdup\tv[0-9]+\.8b, w[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\td[0-9]+,} } } */
/* { dg-final { scan-assembler {\tzip1\tv[0-9]+\.16b, v[0-9]+\.16b, v[0-9]+\.16b} } } */
