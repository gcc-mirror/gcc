/* Testing return address signing.  */
/* { dg-do compile } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-march=armv8.1-m.main+pacbti+fp -mbranch-protection=pac-ret+leaf -mthumb -mfloat-abi=hard --save-temps -O0" } */

#include "pac.h"

/* { dg-final { scan-assembler-times "pac\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-times "aut\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-not "\tbti" } } */

