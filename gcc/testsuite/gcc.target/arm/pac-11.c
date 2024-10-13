/* Testing return address signing.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-mbranch-protection=bti+pac-ret+leaf -mfloat-abi=hard --save-temps -O2" } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */

#include "pac.h"

/* { dg-final { scan-assembler-times "\tpacbti\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-times "\taut\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-not "\tbti" } } */
