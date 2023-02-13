/* Testing return address signing.  */
/* { dg-do compile } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-march=armv8.1-m.main+pacbti+fp -mthumb -mfloat-abi=hard --save-temps -O2" } */

#include "pac.h"

/* { dg-final { scan-assembler-not "\tbti\t" } } */
/* { dg-final { scan-assembler-not "\tpac\t" } } */
/* { dg-final { scan-assembler-not "\tpacbti\t" } } */
