/* Testing return address signing.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-mbranch-protection=pac-ret+leaf -mfloat-abi=hard --save-temps -O0" } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */

#include "pac.h"

/* { dg-final { scan-assembler-times "\tpac\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-times "\taut\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-not "\tbti" } } */

