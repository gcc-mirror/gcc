/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(int8_t *base, int8_t *out, size_t vl, size_t m, size_t n) {
  vint8mf4_t v1 = *(vint8mf4_t*) (base + 100000);
  size_t avl = __riscv_vsetvl_e8mf8(vl);
  for (size_t i = 0; i < m; i++) {
    vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i, avl);
    if (n > 100) {
      __riscv_vse8_v_i8mf4(out + i, v1, avl);
    } else {
      __riscv_vse8_v_i8mf8(out + i, v0, avl);
    }
  }
}

/* { dg-final { scan-assembler {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
