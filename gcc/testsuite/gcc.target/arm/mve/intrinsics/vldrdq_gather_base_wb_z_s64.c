/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
#include "arm_mve.h"

int64x2_t foo (uint64x2_t * addr, mve_pred16_t p)
{
    return vldrdq_gather_base_wb_z_s64 (addr, 1016, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vldrdt.u64\tq\[0-9\]+, \\\[q\[0-9\]+, #\[0-9\]+\\\]!" } } */
/* { dg-final { scan-assembler-times "vldr.64" 1 } } */
/* { dg-final { scan-assembler-times "vstr.64" 1 } } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
