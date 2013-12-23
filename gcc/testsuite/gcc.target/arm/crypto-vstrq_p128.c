/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

void
foo (poly128_t* ptr, poly128_t val)
{
  vstrq_p128 (ptr, val);
}

/* { dg-final { scan-assembler "vst1.64\t{d\[0-9\]+-d\[0-9\]+}.*" } } */
