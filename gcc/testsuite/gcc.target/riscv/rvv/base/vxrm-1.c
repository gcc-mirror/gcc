/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

size_t f0 ()
{
  return __RISCV_VXRM_RNU;
}

size_t f1 ()
{
  return __RISCV_VXRM_RNE;
}

size_t f2 ()
{
  return __RISCV_VXRM_RDN;
}

size_t f3 ()
{
  return __RISCV_VXRM_ROD;
}

/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*0} 1} } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*1} 1} } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*2} 1} } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*3} 1} } */
