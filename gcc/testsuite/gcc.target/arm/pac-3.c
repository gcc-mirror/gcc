/* Testing return address signing.  */
/* { dg-do run } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-options "-march=armv8.1-m.main+pacbti+fp -mbranch-protection=bti+pac-ret+leaf -mthumb -mfloat-abi=hard --save-temps -O2" } */

#include "pac.h"

/* { dg-final { scan-assembler-times "\tpacbti\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-times "\taut\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-not "\tbti" } } */
