/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool1_t test___riscv_vlm_v_b1_vl(const uint8_t *base, size_t vl) {
  return __riscv_vlm_v_b1(base, vl);
}

void test___riscv_vsm_v_b1_vl(uint8_t *base, vbool1_t value, size_t vl) {
  __riscv_vsm_v_b1(base, value, vl);
}

vbool2_t test___riscv_vlm_v_b2_vl(const uint8_t *base, size_t vl) {
  return __riscv_vlm_v_b2(base, vl);
}

void test___riscv_vsm_v_b2_vl(uint8_t *base, vbool2_t value, size_t vl) {
  __riscv_vsm_v_b2(base, value, vl);
}

vbool4_t test___riscv_vlm_v_b4_vl(const uint8_t *base, size_t vl) {
  return __riscv_vlm_v_b4(base, vl);
}

void test___riscv_vsm_v_b4_vl(uint8_t *base, vbool4_t value, size_t vl) {
  __riscv_vsm_v_b4(base, value, vl);
}

vbool8_t test___riscv_vlm_v_b8_vl(const uint8_t *base, size_t vl) {
  return __riscv_vlm_v_b8(base, vl);
}

void test___riscv_vsm_v_b8_vl(uint8_t *base, vbool8_t value, size_t vl) {
  __riscv_vsm_v_b8(base, value, vl);
}

vbool16_t test___riscv_vlm_v_b16_vl(const uint8_t *base, size_t vl) {
  return __riscv_vlm_v_b16(base, vl);
}

void test___riscv_vsm_v_b16_vl(uint8_t *base, vbool16_t value, size_t vl) {
  __riscv_vsm_v_b16(base, value, vl);
}

vbool32_t test___riscv_vlm_v_b32_vl(const uint8_t *base, size_t vl) {
  return __riscv_vlm_v_b32(base, vl);
}

void test___riscv_vsm_v_b32_vl(uint8_t *base, vbool32_t value, size_t vl) {
  __riscv_vsm_v_b32(base, value, vl);
}

vbool64_t test___riscv_vlm_v_b64_vl(const uint8_t *base, size_t vl) {
  return __riscv_vlm_v_b64(base, vl);
}

void test___riscv_vsm_v_b64_vl(uint8_t *base, vbool64_t value, size_t vl) {
  __riscv_vsm_v_b64(base, value, vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 1 } } */
