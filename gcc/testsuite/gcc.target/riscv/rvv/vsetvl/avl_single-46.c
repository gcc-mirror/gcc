/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int cond, size_t vl)
{
  vl = 101 + vl;
  if (n > cond) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 600, vl);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + 600, vl);
    __riscv_vse8_v_i8mf8 (out + 600, v2, vl);
  } else {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 700, vl);
    __riscv_vse8_v_i8mf8 (out + 700, v, vl);
  }

  for (int i = 0 ; i < n * n * n * n; i++) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 900 + i, vl);
    __riscv_vse8_v_i8mf8 (out + 900 + i, v, vl);
  }
}

/* { dg-final { scan-assembler {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]} { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
