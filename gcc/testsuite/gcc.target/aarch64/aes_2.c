
/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a+aes" } */

#include "arm_neon.h"

uint8x16_t
test0 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaeseq_u8 (a, b);
  result = vaeseq_u8 (result, a);
  return result;
}

uint8x16_t
test1 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaeseq_u8 (a, b);
  result = vaeseq_u8 (a, result);
  return result;
}

uint8x16_t
test2 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaeseq_u8 (b, a);
  result = vaeseq_u8 (result, b);
  return result;
}

uint8x16_t
test3 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaeseq_u8 (b, a);
  result = vaeseq_u8 (b, result);
  return result;
}

uint8x16_t
test4 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaesdq_u8 (a, b);
  result = vaesdq_u8 (result, a);
  return result;
}

uint8x16_t
test5 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaesdq_u8 (a, b);
  result = vaesdq_u8 (a, result);
  return result;
}

uint8x16_t
test6 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaesdq_u8 (b, a);
  result = vaesdq_u8 (result, b);
  return result;
}

uint8x16_t
test7 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  result = vaesdq_u8 (b, a);
  result = vaesdq_u8 (b, result);
  return result;
}
/* { dg-final { scan-assembler-not "mov" } } */
/* { dg-final { scan-assembler "aesd\tv" } } */
/* { dg-final { scan-assembler "aese\tv" } } */

