/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

poly128_t
foo (poly128_t* ptr)
{
  return vldrq_p128 (ptr);
}

/* { dg-final { scan-assembler "vld1.64\t{d\[0-9\]+-d\[0-9\]+}.*" } } */
