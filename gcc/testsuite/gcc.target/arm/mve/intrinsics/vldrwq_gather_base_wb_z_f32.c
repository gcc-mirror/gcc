/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4_t
foo (uint32x4_t * addr, mve_pred16_t p)
{
  return vldrwq_gather_base_wb_z_f32 (addr, 8, p);
}

/* { dg-final { scan-assembler "vldrw.32\tq\[0-9\]+, \\\[r\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "vmsr\t P0, r\[0-9\]+.*" } } */
/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vldrwt.u32\tq\[0-9\]+, \\\[q\[0-9\]+, #\[0-9\]+\\\]!" } } */
/* { dg-final { scan-assembler "vstrw.32\tq\[0-9\]+, \\\[r\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
