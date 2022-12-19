/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void vwrite_csr_vstart(unsigned long value) {
  vwrite_csr(RVV_VSTART, value);
}

void vwrite_csr_vxsat(unsigned long value) {
  vwrite_csr(RVV_VXSAT, value);
}

void vwrite_csr_vxrm(unsigned long value) {
  vwrite_csr(RVV_VXRM, value);
}

void vwrite_csr_vcsr(unsigned long value) {
  vwrite_csr(RVV_VCSR, value);
}

/* { dg-final { scan-assembler-times {csrw\s+vstart,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {csrw\s+vxsat,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {csrw\s+vxrm,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {csrw\s+vcsr,\s*[a-x0-9]+} 1 } } */
