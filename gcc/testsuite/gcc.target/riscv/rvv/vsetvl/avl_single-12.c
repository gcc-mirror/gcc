/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int cond)
{
  if (cond == 2) {
    size_t vl = 101;
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 900, vl);
    __riscv_vse8_v_i8mf8 (out + 900, v, vl);
    vl = 102;
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + 1000, vl);
    __riscv_vse8_v_i8mf8 (out + 1000, v2, vl);
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
