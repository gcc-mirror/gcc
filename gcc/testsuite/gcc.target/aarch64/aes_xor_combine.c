/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=cortex-a55+crypto" } */
#include <arm_neon.h>

#define AESE(r, v, key) (r = vaeseq_u8 ((v), (key)));
#define AESD(r, v, key) (r = vaesdq_u8 ((v), (key)));

const uint8x16_t zero = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

uint8x16_t foo0 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESE(dummy, a ^ b, zero);
  return dummy;
}

uint8x16_t foo1 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESE(dummy, a ^ b, zero);
  AESE(dummy, dummy ^ a, zero);
  return dummy;
}

uint8x16_t bar0 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESE(dummy, zero, a ^ b);
  return dummy;
}

uint8x16_t bar1 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESE(dummy, zero, a ^ b);
  AESE(dummy, zero, b ^ dummy);
  return dummy;
}

uint8x16_t foo2 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESD(dummy, a ^ b, zero);
  return dummy;
}

uint8x16_t foo3 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESD(dummy, a ^ b, zero);
  AESD(dummy, dummy ^ a, zero);
  return dummy;
}

uint8x16_t bar2 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESD(dummy, zero, a ^ b);
  return dummy;
}

uint8x16_t bar3 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t dummy;
  AESD(dummy, zero, a ^ b);
  AESD(dummy, zero, b ^ dummy);
  return dummy;
}
/* { dg-final { scan-assembler-not "eor" } } */
/* { dg-final { scan-assembler-not "mov" } } */
