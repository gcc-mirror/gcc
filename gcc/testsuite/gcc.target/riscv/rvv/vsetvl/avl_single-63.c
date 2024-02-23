/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int cond)
{
  size_t vl = 111;
  if (n > cond) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 600, vl);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + 600, vl);
    __riscv_vse8_v_i8mf8 (out + 600, v2, vl);
  } else {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 700, vl);
    __riscv_vse8_v_i8mf8 (out + 700, v, vl);
  }

  for (int i = 0 ; i < n * n * n * n; i++) {
    vint8mf8_t v = *(vint8mf8_t*) (in + 900 + i);
    *(vint8mf8_t*) (out + 900 + i) = v;
  }
}

/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-g" } } } } */
