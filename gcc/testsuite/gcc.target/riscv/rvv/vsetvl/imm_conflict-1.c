/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, size_t n, size_t cond)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i != cond) {
        vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + 100, 17);
        __riscv_vse8_v_i8mf8 (out + i + 100, v, 17);
      } else {
        vint32m1_t v = __riscv_vle32_v_i32m1 (in + i + 200, 4);
        __riscv_vse32_v_i32m1 (out + i + 200, v, 4);
      }
    }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 3 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
