/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */
/* { dg-additional-options "-mcpu=cortex-a72 -O3 -dp" } */

#include <arm_neon.h>

#define AESD(r, v, key) (r = vaesdq_u8 ((v), (key)));
#define AESIMC(r, i) (r = vaesimcq_u8 (i))

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
  AESD (a, a, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESD (b, b, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESD (c, c, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESD (d, d, k);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);

  x = x ^ k;
  AESD (x, x, zero);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  y = y ^ k;
  AESD (y, y, zero);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);

  AESIMC (d, d);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESIMC (c, c);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESIMC (b, b);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESIMC (a, a);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);

  AESIMC (y, y);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESIMC (x, x);
}

/* { dg-final { scan-assembler-times "crypto_aesd_fused" 6 } } */
/* { dg-final { scan-assembler-not "veor" } } */
