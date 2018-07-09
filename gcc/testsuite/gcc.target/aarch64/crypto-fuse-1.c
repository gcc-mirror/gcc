/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=cortex-a72+crypto -dp" } */

#include <arm_neon.h>

#define AESE(r, v, key) (r = vaeseq_u8 ((v), (key)));
#define AESMC(r, i) (r = vaesmcq_u8 (i))

uint8x16_t dummy;
uint8x16_t a;
uint8x16_t b;
uint8x16_t c;
uint8x16_t d;
uint8x16_t e;

void
foo (void)
{
  AESE (a, a, e);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESE (b, b, e);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESE (c, c, e);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESE (d, d, e);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);

  AESMC (a, a);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESMC (b, b);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESMC (c, c);
  dummy = vaddq_u8 (dummy, dummy);
  dummy = vaddq_u8 (dummy, dummy);
  AESMC (d, d);
}

/* { dg-final { scan-assembler-times "crypto_aese_fused" 4 } } */

