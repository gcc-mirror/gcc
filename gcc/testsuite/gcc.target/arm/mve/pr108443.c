/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
#include <arm_mve.h>

void
__attribute__ ((noipa)) partial_write_cst (uint32_t *a, uint32x4_t v)
{
  vstrwq_p_u32 (a, v, 0x00CC);
}

/* { dg-final { scan-assembler {mov\tr[0-9]+, #204} } } */

