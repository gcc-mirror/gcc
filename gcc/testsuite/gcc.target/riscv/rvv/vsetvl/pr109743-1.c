/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int32_t * a, int32_t * b, int n)
{
    if (n <= 0)
      return;
    int i = n;
    size_t vl = __riscv_vsetvl_e32m1 (i);
    for (; i >= 0; i--)
      {
        vint32m1_t v = __riscv_vle32_v_i32m1 (a, vl);
        __riscv_vse32_v_i32m1 (b, v, vl);

        if (i >= vl)
          continue;
        if (i == 0)
          return;
        vl = __riscv_vsetvl_e32m1 (i);
      }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
