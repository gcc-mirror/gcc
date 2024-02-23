/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int m, int cond)
{
  size_t vl = 101;
  vbool64_t mask = *(vbool64_t*) (in + 1000000);
  for (size_t j = 0; j < m; j++){
    for (size_t i = 0; i < n; i++)
      {
        vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j, vl);
        __riscv_vse8_v_i8mf8 (out + i, v, vl);
      }

    for (size_t i = 0; i < n * n; i++)
      out[i] = out[i] * out[i];
    for (size_t i = 0; i < n * n * n; i++)
      out[i] = out[i] + out[i];
    for (size_t i = 0; i < n * n * n * n; i++)
      out[i] = out[i] + 2;
    for (size_t i = 0; i < n * n * n * n * n; i++)
      out[i] = out[i] * 100;
    for (size_t i = 0; i < n * n * n * n * n * n; i++)
      out[i] = out[i] - 77;
  
    for (size_t i = 0; i < n; i++)
      {
        vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((float *)(in + i + j + 200), vl);
        __riscv_vse32_v_f32mf2 ((float *)(out + i + j + 200), v, vl);
        
        vfloat32mf2_t v2 = __riscv_vle32_v_f32mf2_tumu (mask, v, (float *)(in + i + j + 300), vl);
        __riscv_vse32_v_f32mf2_m (mask, (float *)(out + i + j + 300), v2, vl);
      }
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*mu} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
