/* { dg-do assemble } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

poly64x1x2_t test_vld1_p64_x2 (poly64_t * a)
{
    return vld1_p64_x2 (a);
}

poly64x1x3_t test_vld1_p64_x3 (poly64_t * a)
{
    return vld1_p64_x3 (a);
}

poly64x1x4_t test_vld1_p64_x4 (poly64_t * a)
{
    return vld1_p64_x4 (a);
}

/* { dg-final { scan-assembler-times {vld1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]\n} 3 } }  */
