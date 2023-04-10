/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(void *base, void *out, void *mask_in, size_t vl, size_t m, size_t n) {
  vbool64_t mask = *(vbool64_t*)mask_in;

  for (size_t i = 0; i < m; i++) {
    for (size_t j = 0; j < n; j++){
      if ((i + j) % 2 == 0) {
        vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i + j, 4);
        vint8mf8_t v1 = __riscv_vle8_v_i8mf8_tu(v0, base + i + j + 100, 4);
        __riscv_vse8_v_i8mf8 (out + i + j, v1, 4);
      } else {
        vint16mf4_t v0 = __riscv_vle16_v_i16mf4(base + i + j, 4);
        vint16mf4_t v1 = __riscv_vle16_v_i16mf4_mu(mask, v0, base + i + j + 100, 4);
        __riscv_vse16_v_i16mf4 (out + i + j, v1, 4);
      }
    }
  }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e8,\s*mf8,\s*tu,\s*mu} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */

