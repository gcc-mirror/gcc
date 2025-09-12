/* { dg-do compile } */
/* { dg-options "-march=rv32gv_xandesvsintload -O3 -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-march=rv64gv_xandesvsintload -O3 -mabi=lp64d" { target { rv64 } } } */

#include "andes_vector.h"

vint8mf8_t test_vln8_v_i8mf8(const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8mf8(rs1, vl);
}

vint8mf4_t test_vln8_v_i8mf4(const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8mf4(rs1, vl);
}

vint8mf2_t test_vln8_v_i8mf2(const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8mf2(rs1, vl);
}

vint8m1_t test_vln8_v_i8m1(const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m1(rs1, vl);
}

vint8m2_t test_vln8_v_i8m2(const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m2(rs1, vl);
}

vint8m4_t test_vln8_v_i8m4(const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m4(rs1, vl);
}

vint8m8_t test_vln8_v_i8m8(const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m8(rs1, vl);
}

vint8mf8_t test_vln8_v_i8mf8_m(vbool64_t vm, const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8mf8_m(vm, rs1, vl);
}

vint8mf4_t test_vln8_v_i8mf4_m(vbool32_t vm, const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8mf4_m(vm, rs1, vl);
}

vint8mf2_t test_vln8_v_i8mf2_m(vbool16_t vm, const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8mf2_m(vm, rs1, vl);
}

vint8m1_t test_vln8_v_i8m1_m(vbool8_t vm, const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m1_m(vm, rs1, vl);
}

vint8m2_t test_vln8_v_i8m2_m(vbool4_t vm, const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m2_m(vm, rs1, vl);
}

vint8m4_t test_vln8_v_i8m4_m(vbool2_t vm, const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m4_m(vm, rs1, vl);
}

vint8m8_t test_vln8_v_i8m8_m(vbool1_t vm, const void *rs1, size_t vl) {
  return __riscv_nds_vln8_v_i8m8_m(vm, rs1, vl);
}
/* { dg-final { scan-assembler-times {vseti?vli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*mf?[1248],\s*t[au],\s*m[au]\s+nds\.vln8\.v\s+} 14 } } */
