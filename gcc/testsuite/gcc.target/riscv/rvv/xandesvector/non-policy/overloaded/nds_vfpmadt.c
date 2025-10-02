/* { dg-do compile } */
/* { dg-options "-march=rv32gv_zvfh_xandesvpackfph -O3 -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-march=rv64gv_zvfh_xandesvpackfph -O3 -mabi=lp64d" { target { rv64 } } } */

#include "andes_vector.h"

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4(vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2(vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1(vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2(vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4(vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8(vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_m(vbool64_t mask, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_m(vbool32_t mask, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_m(vbool16_t mask, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_m(vbool8_t mask, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_m(vbool4_t mask, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_m(vbool2_t mask, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_rm(vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_rm(vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_rm(vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_rm(vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_rm(vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_rm(vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_nds_vfpmadt_vf_f16mf4_rm_m(vbool64_t mask, vfloat16mf4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16mf2_t test_nds_vfpmadt_vf_f16mf2_rm_m(vbool32_t mask, vfloat16mf2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m1_t test_nds_vfpmadt_vf_f16m1_rm_m(vbool16_t mask, vfloat16m1_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m2_t test_nds_vfpmadt_vf_f16m2_rm_m(vbool8_t mask, vfloat16m2_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m4_t test_nds_vfpmadt_vf_f16m4_rm_m(vbool4_t mask, vfloat16m4_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, __RISCV_FRM_RNE, vl);
}

vfloat16m8_t test_nds_vfpmadt_vf_f16m8_rm_m(vbool2_t mask, vfloat16m8_t op1, float op2, size_t vl) {
  return __riscv_nds_vfpmadt(mask, op1, op2, __RISCV_FRM_RNE, vl);
}

/* { dg-final { scan-assembler-times {nds\.vfpmadt\.vf\s+} 24 } } */
