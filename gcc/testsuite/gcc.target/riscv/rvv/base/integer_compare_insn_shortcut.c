/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vbool1_t test_shortcut_for_riscv_vmseq_case_0(vint8m8_t v1, size_t vl) {
  return __riscv_vmseq_vv_i8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmseq_case_1(vint8m4_t v1, size_t vl) {
  return __riscv_vmseq_vv_i8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmseq_case_2(vint8m2_t v1, size_t vl) {
  return __riscv_vmseq_vv_i8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmseq_case_3(vint8m1_t v1, size_t vl) {
  return __riscv_vmseq_vv_i8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmseq_case_4(vint8mf2_t v1, size_t vl) {
  return __riscv_vmseq_vv_i8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmseq_case_5(vint8mf4_t v1, size_t vl) {
  return __riscv_vmseq_vv_i8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmseq_case_6(vint8mf8_t v1, size_t vl) {
  return __riscv_vmseq_vv_i8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsne_case_0(vint8m8_t v1, size_t vl) {
  return __riscv_vmsne_vv_i8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsne_case_1(vint8m4_t v1, size_t vl) {
  return __riscv_vmsne_vv_i8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsne_case_2(vint8m2_t v1, size_t vl) {
  return __riscv_vmsne_vv_i8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsne_case_3(vint8m1_t v1, size_t vl) {
  return __riscv_vmsne_vv_i8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsne_case_4(vint8mf2_t v1, size_t vl) {
  return __riscv_vmsne_vv_i8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsne_case_5(vint8mf4_t v1, size_t vl) {
  return __riscv_vmsne_vv_i8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsne_case_6(vint8mf8_t v1, size_t vl) {
  return __riscv_vmsne_vv_i8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmslt_case_0(vint8m8_t v1, size_t vl) {
  return __riscv_vmslt_vv_i8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmslt_case_1(vint8m4_t v1, size_t vl) {
  return __riscv_vmslt_vv_i8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmslt_case_2(vint8m2_t v1, size_t vl) {
  return __riscv_vmslt_vv_i8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmslt_case_3(vint8m1_t v1, size_t vl) {
  return __riscv_vmslt_vv_i8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmslt_case_4(vint8mf2_t v1, size_t vl) {
  return __riscv_vmslt_vv_i8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmslt_case_5(vint8mf4_t v1, size_t vl) {
  return __riscv_vmslt_vv_i8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmslt_case_6(vint8mf8_t v1, size_t vl) {
  return __riscv_vmslt_vv_i8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsltu_case_0(vuint8m8_t v1, size_t vl) {
  return __riscv_vmsltu_vv_u8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsltu_case_1(vuint8m4_t v1, size_t vl) {
  return __riscv_vmsltu_vv_u8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsltu_case_2(vuint8m2_t v1, size_t vl) {
  return __riscv_vmsltu_vv_u8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsltu_case_3(vuint8m1_t v1, size_t vl) {
  return __riscv_vmsltu_vv_u8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsltu_case_4(vuint8mf2_t v1, size_t vl) {
  return __riscv_vmsltu_vv_u8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsltu_case_5(vuint8mf4_t v1, size_t vl) {
  return __riscv_vmsltu_vv_u8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsltu_case_6(vuint8mf8_t v1, size_t vl) {
  return __riscv_vmsltu_vv_u8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsle_case_0(vint8m8_t v1, size_t vl) {
  return __riscv_vmsle_vv_i8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsle_case_1(vint8m4_t v1, size_t vl) {
  return __riscv_vmsle_vv_i8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsle_case_2(vint8m2_t v1, size_t vl) {
  return __riscv_vmsle_vv_i8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsle_case_3(vint8m1_t v1, size_t vl) {
  return __riscv_vmsle_vv_i8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsle_case_4(vint8mf2_t v1, size_t vl) {
  return __riscv_vmsle_vv_i8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsle_case_5(vint8mf4_t v1, size_t vl) {
  return __riscv_vmsle_vv_i8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsle_case_6(vint8mf8_t v1, size_t vl) {
  return __riscv_vmsle_vv_i8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsleu_case_0(vuint8m8_t v1, size_t vl) {
  return __riscv_vmsleu_vv_u8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsleu_case_1(vuint8m4_t v1, size_t vl) {
  return __riscv_vmsleu_vv_u8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsleu_case_2(vuint8m2_t v1, size_t vl) {
  return __riscv_vmsleu_vv_u8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsleu_case_3(vuint8m1_t v1, size_t vl) {
  return __riscv_vmsleu_vv_u8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsleu_case_4(vuint8mf2_t v1, size_t vl) {
  return __riscv_vmsleu_vv_u8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsleu_case_5(vuint8mf4_t v1, size_t vl) {
  return __riscv_vmsleu_vv_u8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsleu_case_6(vuint8mf8_t v1, size_t vl) {
  return __riscv_vmsleu_vv_u8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsgt_case_0(vint8m8_t v1, size_t vl) {
  return __riscv_vmsgt_vv_i8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsgt_case_1(vint8m4_t v1, size_t vl) {
  return __riscv_vmsgt_vv_i8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsgt_case_2(vint8m2_t v1, size_t vl) {
  return __riscv_vmsgt_vv_i8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsgt_case_3(vint8m1_t v1, size_t vl) {
  return __riscv_vmsgt_vv_i8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsgt_case_4(vint8mf2_t v1, size_t vl) {
  return __riscv_vmsgt_vv_i8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsgt_case_5(vint8mf4_t v1, size_t vl) {
  return __riscv_vmsgt_vv_i8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsgt_case_6(vint8mf8_t v1, size_t vl) {
  return __riscv_vmsgt_vv_i8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsgtu_case_0(vuint8m8_t v1, size_t vl) {
  return __riscv_vmsgtu_vv_u8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsgtu_case_1(vuint8m4_t v1, size_t vl) {
  return __riscv_vmsgtu_vv_u8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsgtu_case_2(vuint8m2_t v1, size_t vl) {
  return __riscv_vmsgtu_vv_u8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsgtu_case_3(vuint8m1_t v1, size_t vl) {
  return __riscv_vmsgtu_vv_u8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsgtu_case_4(vuint8mf2_t v1, size_t vl) {
  return __riscv_vmsgtu_vv_u8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsgtu_case_5(vuint8mf4_t v1, size_t vl) {
  return __riscv_vmsgtu_vv_u8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsgtu_case_6(vuint8mf8_t v1, size_t vl) {
  return __riscv_vmsgtu_vv_u8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsge_case_0(vint8m8_t v1, size_t vl) {
  return __riscv_vmsge_vv_i8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsge_case_1(vint8m4_t v1, size_t vl) {
  return __riscv_vmsge_vv_i8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsge_case_2(vint8m2_t v1, size_t vl) {
  return __riscv_vmsge_vv_i8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsge_case_3(vint8m1_t v1, size_t vl) {
  return __riscv_vmsge_vv_i8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsge_case_4(vint8mf2_t v1, size_t vl) {
  return __riscv_vmsge_vv_i8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsge_case_5(vint8mf4_t v1, size_t vl) {
  return __riscv_vmsge_vv_i8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsge_case_6(vint8mf8_t v1, size_t vl) {
  return __riscv_vmsge_vv_i8mf8_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmsgeu_case_0(vuint8m8_t v1, size_t vl) {
  return __riscv_vmsgeu_vv_u8m8_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmsgeu_case_1(vuint8m4_t v1, size_t vl) {
  return __riscv_vmsgeu_vv_u8m4_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmsgeu_case_2(vuint8m2_t v1, size_t vl) {
  return __riscv_vmsgeu_vv_u8m2_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmsgeu_case_3(vuint8m1_t v1, size_t vl) {
  return __riscv_vmsgeu_vv_u8m1_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmsgeu_case_4(vuint8mf2_t v1, size_t vl) {
  return __riscv_vmsgeu_vv_u8mf2_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmsgeu_case_5(vuint8mf4_t v1, size_t vl) {
  return __riscv_vmsgeu_vv_u8mf4_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmsgeu_case_6(vuint8mf8_t v1, size_t vl) {
  return __riscv_vmsgeu_vv_u8mf8_b64(v1, v1, vl);
}

/* { dg-final { scan-assembler-times {vmclr\.m\sv[0-9]} 35 } } */
/* { dg-final { scan-assembler-times {vmset\.m\sv[0-9]} 35 } } */
