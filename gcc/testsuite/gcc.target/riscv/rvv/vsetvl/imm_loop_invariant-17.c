/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      vint8mf2_t v = __riscv_vle8_v_i8mf2 (in + i*10 + j*10, 8);
      __riscv_vse8_v_i8mf2 (out + i*10 + j*10, v, 8);
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j + k, 17);
          __riscv_vse8_v_i8mf8 (out + i + j + k, v, 17);
        }
    }
  }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*8,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
