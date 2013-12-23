/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

int
foo (void)
{
  uint32_t val = 0xdeadbeef;
  return vsha1h_u32 (val);
}

/* { dg-final { scan-assembler "sha1h.32\tq\[0-9\]+, q\[0-9\]+" } } */
