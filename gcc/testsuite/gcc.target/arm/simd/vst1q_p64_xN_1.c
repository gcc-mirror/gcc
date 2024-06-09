/* { dg-do assemble } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

void test_vst1q_p64_x2(poly64_t *ptr, poly64x2x2_t val)
{
    vst1q_p64_x2(ptr, val);
}

void test_vst1q_p64_x3(poly64_t *ptr, poly64x2x3_t val)
{
    vst1q_p64_x3(ptr, val);
}

void test_vst1q_p64_x4(poly64_t *ptr, poly64x2x4_t val)
{
    vst1q_p64_x4(ptr, val);
}

/* { dg-final { scan-assembler-times {vst1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vst1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]!\n} 2 } }  */
