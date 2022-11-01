/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-skip-if "test intrinsic using rvv" { *-*-* } { "*" } { "-march=rv*v*zfh*" } } */

#include <riscv_vector.h>

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

/* { dg-final { scan-assembler-times {csrr\s+(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7]),\s*vstart} 1 } } */
/* { dg-final { scan-assembler-times {csrr\s+(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7]),\s*vxsat} 1 } } */
/* { dg-final { scan-assembler-times {csrr\s+(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7]),\s*vxrm} 1 } } */
/* { dg-final { scan-assembler-times {csrr\s+(?:ra|[sgtf]p|t[0-6]|s[0-9]|s10|s11|a[0-7]),\s*vcsr} 1 } } */
