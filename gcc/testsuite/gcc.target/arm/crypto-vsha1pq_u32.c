/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

int
foo (void)
{
  uint32_t hash = 0xdeadbeef;
  uint32x4_t a = {0, 1, 2, 3};
  uint32x4_t b = {3, 2, 1, 0};

  uint32x4_t res = vsha1pq_u32 (a, hash, b);
  return res[0];
}

/* { dg-final { scan-assembler "sha1p.32\tq\[0-9\]+, q\[0-9\]+" } } */
