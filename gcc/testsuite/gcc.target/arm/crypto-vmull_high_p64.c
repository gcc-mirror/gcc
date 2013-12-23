/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

poly128_t
foo (void)
{
  poly64x2_t a = { 0xdeadbeef, 0xadabcaca };
  poly64x2_t b = { 0xdcdcdcdc, 0xbdbdbdbd };
  return vmull_high_p64 (a, b);
}

/* { dg-final { scan-assembler "vmull.p64.*" } } */
