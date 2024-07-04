/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(int8_t *base, int8_t *out, size_t vl, size_t m, size_t k) {
  size_t avl;
  if (m > 100)
    avl = __riscv_vsetvl_e16mf4(vl << 4);
  else
    avl = __riscv_vsetvl_e32mf2(vl >> 8);
  for (size_t i = 0; i < m; i++) {
    vint8mf8_t v0 = __riscv_vle8_v_i8mf8(base + i, avl);
    v0 = __riscv_vadd_vv_i8mf8 (v0, v0, avl);
    __riscv_vse8_v_i8mf8(out + i, v0, avl);
  }
}

/* { dg-final { scan-assembler-times {slli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*4} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */
