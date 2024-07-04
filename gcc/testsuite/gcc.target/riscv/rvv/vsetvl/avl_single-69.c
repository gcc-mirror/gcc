/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int l, int n, int m, size_t cond)
{
  size_t vl = 555 + cond;
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j + k, vl);
          __riscv_vse8_v_i8mf8 (out + i + j + k, v, vl);
        }
    }
  }
  
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          out[i+j+k+10000000] = out[i+j+k+10000000] + in[i+j+k+10000000];
        }
    }
  }

  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j + k + 10000, vl);
          __riscv_vse8_v_i8mf8 (out + i + j + k + 10000, v, vl);
        }
    }
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
