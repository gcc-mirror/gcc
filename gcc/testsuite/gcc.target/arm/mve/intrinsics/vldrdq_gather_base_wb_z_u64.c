/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
#include "arm_mve.h"

uint64x2_t foo (uint64x2_t * addr, mve_pred16_t p)
{
    return vldrdq_gather_base_wb_z_u64 (addr, 8, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vldrdt.u64\tq\[0-9\]+, \\\[q\[0-9\]+, #\[0-9\]+\\\]!" } } */
/* { dg-final { scan-assembler-times "vldrw.u32" 1 } } */
/* { dg-final { scan-assembler-times "vstrw.32" 1 } } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
