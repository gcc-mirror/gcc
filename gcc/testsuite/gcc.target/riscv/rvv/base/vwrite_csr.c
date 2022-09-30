/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-skip-if "test intrinsic using rvv" { *-*-* } { "*" } { "-march=rv*v*zfh*" } } */

#include <riscv_vector.h>

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

/* { dg-final { scan-assembler-times {csrw\s+vstart,\s*(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7])} 1 } } */
/* { dg-final { scan-assembler-times {csrw\s+vxsat,\s*(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7])} 1 } } */
/* { dg-final { scan-assembler-times {csrw\s+vxrm,\s*(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7])} 1 } } */
/* { dg-final { scan-assembler-times {csrw\s+vcsr,\s*(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7])} 1 } } */