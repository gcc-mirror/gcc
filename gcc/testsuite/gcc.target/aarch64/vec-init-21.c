/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

int8x16_t f_s8(int8_t x, int8_t y)
{
  return (int8x16_t) { x, y, 1, 2, 3, 4, 5, 6,
                       7, 8, 9, 10, 11, 12, 13, 14 };
}

/* { dg-final { scan-assembler {\tldr\tq[0-9]+,} } } */
/* { dg-final { scan-assembler {\tins\tv[0-9]+\.b\[0|15\], w0} } } */
/* { dg-final { scan-assembler {\tins\tv[0-9]+\.b\[1|14\], w1} } } */
