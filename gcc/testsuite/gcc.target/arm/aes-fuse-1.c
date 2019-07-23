/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */
/* { dg-additional-options "-mcpu=cortex-a72 -O3 -dp" } */

#include <arm_neon.h>

#define AESE(r, v, key) (r = vaeseq_u8 ((v), (key)));
#define AESMC(r, i) (r = vaesmcq_u8 (i))

const uint8x16_t zero = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

uint8x16_t dummy;
uint8x16_t a;
uint8x16_t b;
uint8x16_t c;
uint8x16_t d;
uint8x16_t x;
uint8x16_t y;
uint8x16_t k;

void foo (void)
{
  AESE (a, a, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESE (b, b, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESE (c, c, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESE (d, d, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);

  x = x ^ k;
  AESE (x, x, zero);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  y = y ^ k;
  AESE (y, y, zero);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);

  AESMC (d, d);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESMC (c, c);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESMC (b, b);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESMC (a, a);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);

  AESMC (y, y);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESMC (x, x);
}

/* { dg-final { scan-assembler-times "crypto_aese_fused" 6 } } */
/* { dg-final { scan-assembler-not "veor" } } */
