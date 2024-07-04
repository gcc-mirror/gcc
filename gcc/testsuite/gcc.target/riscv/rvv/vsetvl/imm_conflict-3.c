/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int cond)
{
  if (n > cond) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 600, 5);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + 600, 5);
    __riscv_vse8_v_i8mf8 (out + 600, v2, 5);
  } else {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 700, 5);
    __riscv_vse8_v_i8mf8 (out + 700, v, 5);
  }

  for (int i = 0 ; i < n * n * n * n; i++) {
    vint8mf8_t v = *(vint8mf8_t*) (in + 900 + i);
    *(vint8mf8_t*) (out + 900 + i) = v;
  }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*5,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 2 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-g" } } } } */
