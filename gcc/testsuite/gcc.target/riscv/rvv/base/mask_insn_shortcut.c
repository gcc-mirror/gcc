/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vbool1_t test_shortcut_for_riscv_vmand_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmand_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmand_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmand_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmand_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmand_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmand_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmand_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmand_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmand_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmand_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmand_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmand_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmand_mm_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmnand_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmnand_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmnand_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmnand_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmnand_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmnand_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmnand_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmnand_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmnand_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmnand_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmnand_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmnand_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmnand_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmnand_mm_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmandn_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmandn_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmandn_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmandn_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmandn_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmandn_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmandn_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmandn_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmandn_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmandn_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmandn_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmandn_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmandn_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmandn_mm_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmxor_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmxor_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmxor_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmxor_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmxor_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmxor_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmxor_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmxor_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmxor_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmxor_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmxor_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmxor_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmxor_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmxor_mm_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmor_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmor_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmor_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmor_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmor_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmor_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmor_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmor_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmor_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmor_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmor_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmor_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmor_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmor_mm_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmnor_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmnor_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmnor_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmnor_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmnor_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmnor_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmnor_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmnor_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmnor_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmnor_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmnor_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmnor_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmnor_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmnor_mm_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmorn_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmorn_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmorn_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmorn_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmorn_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmorn_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmorn_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmorn_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmorn_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmorn_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmorn_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmorn_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmorn_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmorn_mm_b64(v1, v1, vl);
}

vbool1_t test_shortcut_for_riscv_vmxnor_case_0(vbool1_t v1, size_t vl) {
  return __riscv_vmxnor_mm_b1(v1, v1, vl);
}

vbool2_t test_shortcut_for_riscv_vmxnor_case_1(vbool2_t v1, size_t vl) {
  return __riscv_vmxnor_mm_b2(v1, v1, vl);
}

vbool4_t test_shortcut_for_riscv_vmxnor_case_2(vbool4_t v1, size_t vl) {
  return __riscv_vmxnor_mm_b4(v1, v1, vl);
}

vbool8_t test_shortcut_for_riscv_vmxnor_case_3(vbool8_t v1, size_t vl) {
  return __riscv_vmxnor_mm_b8(v1, v1, vl);
}

vbool16_t test_shortcut_for_riscv_vmxnor_case_4(vbool16_t v1, size_t vl) {
  return __riscv_vmxnor_mm_b16(v1, v1, vl);
}

vbool32_t test_shortcut_for_riscv_vmxnor_case_5(vbool32_t v1, size_t vl) {
  return __riscv_vmxnor_mm_b32(v1, v1, vl);
}

vbool64_t test_shortcut_for_riscv_vmxnor_case_6(vbool64_t v1, size_t vl) {
  return __riscv_vmxnor_mm_b64(v1, v1, vl);
}

/* { dg-final { scan-assembler-not {vmand\.mm\s+v[0-9]+,\s*v[0-9]+} } } */
/* { dg-final { scan-assembler-not {vmnand\.mm\s+v[0-9]+,\s*v[0-9]+} } } */
/* { dg-final { scan-assembler-not {vmnandn\.mm\s+v[0-9]+,\s*v[0-9]+} } } */
/* { dg-final { scan-assembler-not {vmxor\.mm\s+v[0-9]+,\s*v[0-9]+} } } */
/* { dg-final { scan-assembler-not {vmor\.mm\s+v[0-9]+,\s*v[0-9]+} } } */
/* { dg-final { scan-assembler-not {vmnor\.mm\s+v[0-9]+,\s*v[0-9]+} } } */
/* { dg-final { scan-assembler-times {vmorn\.mm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 7 } } */
/* { dg-final { scan-assembler-not {vmxnor\.mm\s+v[0-9]+,\s*v[0-9]+} } } */
/* { dg-final { scan-assembler-times {vmclr\.m\s+v[0-9]+} 14 } } */
/* { dg-final { scan-assembler-times {vmset\.m\s+v[0-9]+} 7 } } */
/* { dg-final { scan-assembler-times {vmmv\.m\s+v[0-9]+,\s*v[0-9]+} 14 } } */
/* { dg-final { scan-assembler-times {vmnot\.m\s+v[0-9]+,\s*v[0-9]+} 14 } } */
