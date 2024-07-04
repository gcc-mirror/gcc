/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j, 17);
          __riscv_vse8_v_i8mf8 (out + i + j, v, 17);
        }
    }
  }
}

/* { dg-final { scan-assembler-not {mv\s+[a-x0-9]+,[a-x0-9]+\s+mv\s+[a-x0-9]+,[a-x0-9]+\s+mv\s+[a-x0-9]+,[a-x0-9]+\s+mv\s+[a-x0-9]+,[a-x0-9]+\s+mv\s+[a-x0-9]+,[a-x0-9]+\s+vsetivli} } } */
