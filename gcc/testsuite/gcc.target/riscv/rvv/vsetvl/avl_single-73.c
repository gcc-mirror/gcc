/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int cond, size_t vl, size_t vl2)
{
  for (size_t i = 0; i < n; i++)
    {
      vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + 300, vl);
      __riscv_vse8_v_i8mf8 (out + i + 300, v, vl);
    }
  
  for (size_t i = 0; i < n; i++)
    {
      vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i, vl2);
      __riscv_vse8_v_i8mf8 (out + i, v, vl2);
      
      vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + i + 100, vl2);
      __riscv_vse8_v_i8mf8 (out + i + 100, v2, vl2);
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
