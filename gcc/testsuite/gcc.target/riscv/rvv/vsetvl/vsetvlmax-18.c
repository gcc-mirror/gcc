/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(int8_t *base, int8_t *out, size_t vl, size_t m, size_t n) {
  vint8mf4_t v1 = *(vint8mf4_t*) (base + 100000);
  size_t avl = __riscv_vsetvlmax_e8mf8();
  for (size_t i = 0; i < m; i++) {
    vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i, avl);
    if (n > 100) {
      __riscv_vse8_v_i8mf4(out + i + 100, v1, avl);
    } else {
      __riscv_vse8_v_i8mf8(out + i, v0, avl);
    }
  }
}

/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
