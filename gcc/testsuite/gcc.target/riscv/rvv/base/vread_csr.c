/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

unsigned long vread_csr_vstart(void) {
  return vread_csr(RVV_VSTART);
}

unsigned long vread_csr_vxsat(void) {
  return vread_csr(RVV_VXSAT);
}

unsigned long vread_csr_vxrm(void) {
  return vread_csr(RVV_VXRM);
}

unsigned long vread_csr_vcsr(void) {
  return vread_csr(RVV_VCSR);
}

/* { dg-final { scan-assembler-times {csrr\s+[a-x0-9]+,\s*vstart} 1 } } */
/* { dg-final { scan-assembler-times {csrr\s+[a-x0-9]+,\s*vxsat} 1 } } */
/* { dg-final { scan-assembler-times {csrr\s+[a-x0-9]+,\s*vxrm} 1 } } */
/* { dg-final { scan-assembler-times {csrr\s+[a-x0-9]+,\s*vcsr} 1 } } */
