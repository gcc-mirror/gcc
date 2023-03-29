/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(void *base, void *out, void *mask_in, size_t m, size_t n) {

  size_t vl = 222;
  for (size_t i = 0; i < m; i++) {
    if (i % 2 == 0) {
      for (size_t j = 0; j < n; j++){
        if (j % 2 == 0) {
          vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i + j + 500, vl);
          __riscv_vse8_v_i8mf8 (out + i + j + 500, v0, vl);
        } else {
          vint16mf4_t v0 = __riscv_vle16_v_i16mf4(base + i + j + 600, vl);
          __riscv_vse16_v_i16mf4 (out + i + j + 600, v0, vl);
        }
      }
    } else {
      for (size_t j = 0; j < n; j++){
        vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i + j + 200, vl);
        vint8mf8_t v1 = __riscv_vle8_v_i8mf8_tu(v0, base + i + j + 300, vl);
        __riscv_vse8_v_i8mf8 (out + i + j + 400, v1, vl);
      }
    }
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
