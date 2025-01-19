/* Testing return address signing.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_link } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-options "-mbranch-protection=pac-ret -mfloat-abi=hard --save-temps -O0" } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */

#include "pac.h"

/* { dg-final { scan-assembler "\tpac\tip, lr, sp" } } */
/* { dg-final { scan-assembler "\taut\tip, lr, sp" } } */
/* { dg-final { scan-assembler-not "\tbti" } } */
