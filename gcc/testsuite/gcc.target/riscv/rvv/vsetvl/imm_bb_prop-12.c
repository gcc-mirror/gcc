/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(void *base, void *out, void *mask_in, 
size_t vl, size_t m, size_t n, size_t a, size_t b) {

  for (size_t i = 0; i < m; i++) {
    if (i % 2 == 0) {
      for (size_t j = 0; j < n; j++){
        if (j % 2 == 0) {
          for (size_t k = 0; k < n; k++) {
            for (size_t i_a = 0; i_a < a; i_a++){
              for (size_t i_b = 0; i_b < b; i_b++){
                vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i + 500 + k + j + i_a + i_b, 4);
                vint8mf8_t v1 = __riscv_vle8_v_i8mf8_tu(v0, base + i + 600 + k + j + i_a + i_b, 4);
                __riscv_vse8_v_i8mf8 (out + i + 600 + j + k + i_a + i_b, v1, 4);
              }
            }
          }
        } else {
        }
      }
    }
  }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e8,\s*mf8,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

