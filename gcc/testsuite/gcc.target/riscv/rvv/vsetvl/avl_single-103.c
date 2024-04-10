/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f(void *base, void *out, void *mask_in, size_t m) {
  vbool64_t mask = *(vbool64_t*)mask_in;
  size_t vl = 105;
  vint32mf2_t v0 = __riscv_vle32_v_i32mf2(base + 1000, vl);
  __riscv_vse32_v_i32mf2 (out + 1000, v0, vl);
  for (size_t i = 0; i < m; i++) {
    if (i % 2 == 0) {
      vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i, vl);
      vint8mf8_t v1 = __riscv_vle8_v_i8mf8_tu(v0, base + i + 100, vl);
      v1 = __riscv_vadd_vv_i8mf8 (v0,v1,vl);
      __riscv_vse8_v_i8mf8 (out + i, v1, vl);
    } else {
      vint16mf4_t v0 = __riscv_vle16_v_i16mf4(base + i, vl);
      vint16mf4_t v1 = __riscv_vle16_v_i16mf4_mu(mask, v0, base + i + 100, vl);
      __riscv_vse16_v_i16mf4 (out + i, v1, vl);
    }
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*mu} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */
