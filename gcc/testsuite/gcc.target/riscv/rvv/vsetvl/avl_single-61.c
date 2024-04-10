/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, size_t n, size_t cond)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i != cond) {
        size_t vl = 55;
        vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + 100, vl);
        __riscv_vse8_v_i8mf8 (out + i + 100, v, vl);
      } else {
      size_t vl = 66;
        vint32m1_t v = __riscv_vle32_v_i32m1 (in + i + 200, vl);
        __riscv_vse32_v_i32m1 (out + i + 200, v, vl);
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
