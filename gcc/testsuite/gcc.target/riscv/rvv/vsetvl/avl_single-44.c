/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(int8_t *base, int8_t *out, size_t vl, size_t m) {
  vbool64_t mask = *(vbool64_t*) (base + 10000);
  vint8mf8_t v0;
  for (size_t i = 0; i < m; i++) {
    if (i % 2 == 0) {
      v0 = __riscv_vle8_v_i8mf8_tumu(mask, v0, base + i, vl);
    } else {
      __riscv_vse8_v_i8mf8(out + i, v0, vl);
    }
  }
}

/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
