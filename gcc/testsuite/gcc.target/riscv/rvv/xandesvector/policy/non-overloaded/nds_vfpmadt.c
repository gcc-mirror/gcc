/* { dg-do compile } */
/* { dg-options "-march=rv32gv_zvfh_xandesvpackfph -O3 -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-march=rv64gv_zvfh_xandesvpackfph -O3 -mabi=lp64d" { target { rv64 } } } */

#include "andes_vector.h"

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_tu(vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_tu(maskedoff, op1, op2, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_tu(vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_tu(maskedoff, op1, op2, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_tu(vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_tu(maskedoff, op1, op2, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_tu(vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_tu(maskedoff, op1, op2, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_tu(vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_tu(maskedoff, op1, op2, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_tu(vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_tu(maskedoff, op1, op2, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_tum(vbool64_t mask, vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_tum(mask, maskedoff, op1, op2, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_tum(vbool32_t mask, vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_tum(mask, maskedoff, op1, op2, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_tum(vbool16_t mask, vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_tum(mask, maskedoff, op1, op2, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_tum(vbool8_t mask, vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_tum(mask, maskedoff, op1, op2, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_tum(vbool4_t mask, vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_tum(mask, maskedoff, op1, op2, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_tum(vbool2_t mask, vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_tum(mask, maskedoff, op1, op2, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_tumu(vbool64_t mask, vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_tumu(mask, maskedoff, op1, op2, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_tumu(vbool32_t mask, vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_tumu(mask, maskedoff, op1, op2, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_tumu(vbool16_t mask, vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_tumu(mask, maskedoff, op1, op2, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_tumu(vbool8_t mask, vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_tumu(mask, maskedoff, op1, op2, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_tumu(vbool4_t mask, vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_tumu(mask, maskedoff, op1, op2, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_tumu(vbool2_t mask, vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_tumu(mask, maskedoff, op1, op2, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_mu(vbool64_t mask, vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_mu(mask, maskedoff, op1, op2, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_mu(vbool32_t mask, vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_mu(mask, maskedoff, op1, op2, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_mu(vbool16_t mask, vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_mu(mask, maskedoff, op1, op2, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_mu(vbool8_t mask, vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_mu(mask, maskedoff, op1, op2, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_mu(vbool4_t mask, vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_mu(mask, maskedoff, op1, op2, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_mu(vbool2_t mask, vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_mu(mask, maskedoff, op1, op2, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_rm_tu(vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_rm_tu(maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_rm_tu(vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_rm_tu(maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_rm_tu(vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_rm_tu(maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_rm_tu(vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_rm_tu(maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_rm_tu(vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_rm_tu(maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_rm_tu(vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_rm_tu(maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_rm_tum(vbool64_t mask, vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_rm_tum(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_rm_tum(vbool32_t mask, vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_rm_tum(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_rm_tum(vbool16_t mask, vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_rm_tum(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_rm_tum(vbool8_t mask, vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_rm_tum(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_rm_tum(vbool4_t mask, vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_rm_tum(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_rm_tum(vbool2_t mask, vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_rm_tum(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_rm_tumu(vbool64_t mask, vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_rm_tumu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_rm_tumu(vbool32_t mask, vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_rm_tumu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_rm_tumu(vbool16_t mask, vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_rm_tumu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_rm_tumu(vbool8_t mask, vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_rm_tumu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_rm_tumu(vbool4_t mask, vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_rm_tumu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_rm_tumu(vbool2_t mask, vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_rm_tumu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_rm_mu(vbool64_t mask, vfloat16mf4_t maskedoff, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf4_rm_mu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_rm_mu(vbool32_t mask, vfloat16mf2_t maskedoff, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16mf2_rm_mu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_rm_mu(vbool16_t mask, vfloat16m1_t maskedoff, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m1_rm_mu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_rm_mu(vbool8_t mask, vfloat16m2_t maskedoff, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m2_rm_mu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_rm_mu(vbool4_t mask, vfloat16m4_t maskedoff, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m4_rm_mu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_rm_mu(vbool2_t mask, vfloat16m8_t maskedoff, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt_vf_f16m8_rm_mu(mask, maskedoff, op1, op2, __RISCV_FRM_RNE, vl);
}

/* { dg-final { scan-assembler-times {vseti?vli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*mf?[1248],\s*t[au],\s*m[au]\s+nds\.vfpmadt\.vf\s+} 48 } } */
