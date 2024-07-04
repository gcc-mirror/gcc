/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n, int cond, size_t vl)
{
  vl = vl + 101;
  for (size_t i = 0; i < n; i++)
    {
      vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i, vl);
      __riscv_vse8_v_i8mf8 (out + i, v, vl);
      
      vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + i + 100, vl);
      __riscv_vse8_v_i8mf8 (out + i + 100, v2, vl);
    }
  
  for (size_t i = 0; i < n; i++)
    {
      vuint8mf8_t index = __riscv_vle8_v_u8mf8 (in + i + 300, vl);
      vfloat32mf2_t v2 = __riscv_vluxei8_v_f32mf2 (in + i + 200, index, vl);
      __riscv_vse32_v_f32mf2 (out + i + 200, v2, vl);
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
