/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize -frename-registers" } */

#include "riscv_vector.h"

void f (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  v = __riscv_vle8_v_i8mf4_tu (v, base2 + 100, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vor_vx_i8mf4 (v, 101, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */
