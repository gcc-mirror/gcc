/* { dg-do assemble } */
/* { dg-options "-O" } */

#include <arm_neon.h>

uint8x16_t
foo (uint16x8_t a, uint8x8_t b)
{
  return vcombine_u8 (vmovn_u16 (vshrq_n_u16 (a, 9)), b);
}

uint8x16_t
foo2 (uint16x8_t a, uint8x8_t b)
{
  return vcombine_u8 (b, vmovn_u16 (vshrq_n_u16 (a, 15)));
}

