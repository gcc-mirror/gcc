/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

int8x16_t f_s8(int8_t x, int8_t y)
{
  return (int8x16_t) { x, y, 1, y, 2, y, 3, y,
                       4, y, 5, y, 6, y, 7, y };
}

/* { dg-final { scan-assembler {\tdup\tv[0-9]+\.8b, w[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\td[0-9]+,} } } */
/* { dg-final { scan-assembler {\tins\tv[0-9]+\.b\[0|7\], w[0-9]+} } } */
/* { dg-final { scan-assembler {\tzip1\tv[0-9]+\.16b, v[0-9]+\.16b, v[0-9]+\.16b} } } */
