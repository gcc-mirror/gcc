/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-options "-O2 -mfix-cortex-a57-aes-1742098 -dp" } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

uint8x16_t
foo (uint8x16_t v)
{
  const uint8x16_t key1 = {0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
			   0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f};
  const uint8x16_t key2 = {0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
			   0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f};
  int i;

  for (i = 0; i < 16; ++i)
    {
      v = vaesmcq_u8 (vaeseq_u8 (v, key1));
      v = vaesmcq_u8 (vaeseq_u8 (v, key2));
    }
  return v;
}

/* { dg-final { scan-assembler "aese.8\tq\[0-9\]+, q\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "aes_op_protect/2" 2} } */
/* { dg-final { scan-assembler-times "aes_op_protect/0" 1} } */
/* { dg-final { scan-assembler-times "(?:aesmc|aese_fused)_protected" 1} } */
