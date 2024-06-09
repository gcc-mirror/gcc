/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t *base, int8_t *out, size_t m, size_t n) {
  int vl = 101;
  for (size_t i = 0; i < m; i++) {
    vint8mf8_t v0 = __riscv_vle8_v_i8mf8_tu(v0, base + i, vl);
    if (n > 100) {
      __riscv_vse8_v_i8mf8(out + i + 100, v0, vl);
    } else {
      __riscv_vse8_v_i8mf8(out + i, v0, vl);
    }
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
