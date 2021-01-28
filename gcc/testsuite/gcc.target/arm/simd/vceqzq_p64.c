/* Test the `vceqzq_p64' ARM Neon intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O2 -fno-inline" } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

poly64x2_t v2;
uint64x2_t result2;

void func()
{
  result2 = vceqzq_p64 (v2);
}

/* { dg-final { scan-assembler-times "vceq\.i32\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, #0\n" 2 } } */
