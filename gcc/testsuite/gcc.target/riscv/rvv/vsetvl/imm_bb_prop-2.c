/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int n2)
{
  for (int i = 0 ; i < n2; i++) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 800 + i, 9);
    __riscv_vse8_v_i8mf8 (out + 800 + i, v, 9);
  }

  for (int i = 0 ; i < n * n; i++)
    out[i] = out[i] + out[i];
  
  for (int i = 0 ; i < n * n * n; i++)
    out[i] = out[i] * out[i];

  for (int i = 0 ; i < n * n * n * n; i++)
    out[i] = out[i] * out[i];
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 900 + i, 5);
    __riscv_vse8_v_i8mf8 (out + 900 + i, v, 5);
  }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*9,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*5,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
