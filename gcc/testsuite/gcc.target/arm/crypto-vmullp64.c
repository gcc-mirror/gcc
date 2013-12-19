/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

poly128_t
foo (void)
{
  poly64_t a = 0xdeadbeef;
  poly64_t b = 0xadadadad;
  return vmull_p64 (a, b);
}

/* { dg-final { scan-assembler "vmull.p64.*" } } */
