/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int n2)
{
  size_t vl;
  asm volatile ("li %0, 101" :"=r" (vl)::"memory");
  for (int i = 0 ; i < n2; i++) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 800 + i, vl);
    __riscv_vse8_v_i8mf8 (out + 800 + i, v, vl);
  }

  for (int i = 0 ; i < n * n; i++)
    out[i] = out[i] + out[i];
  
  for (int i = 0 ; i < n * n * n; i++)
    out[i] = out[i] * out[i];

  for (int i = 0 ; i < n * n * n * n; i++)
    out[i] = out[i] * out[i];
  
  asm volatile ("li %0, 102" :"=r" (vl)::"memory");
  for (int i = 0 ; i < n * n * n * n; i++) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 900 + i, vl);
    __riscv_vse8_v_i8mf8 (out + 900 + i, v, vl);
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
