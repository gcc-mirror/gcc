/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void
foo (int i, int n, int m, int32_t *in, int32_t *out)
{
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, i);
  __riscv_vse32_v_i32m1 (out, v, i);
  for (; i < n; i += 1)
    {
      vint32m1_t v = __riscv_vle32_v_i32m1 (in + i, i);
      __riscv_vse32_v_i32m1 (out + i, v, i);
      for (int j = 0; j < m; j += 1)
	{
	  vint32m1_t v = __riscv_vle32_v_i32m1 (in + i * n + j, j);
	  __riscv_vse32_v_i32m1 (out + i * n + j, v, i);
	}
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 4 { target { { any-opts "-O2" "-O3" } && { no-opts "-g" "-funroll-loops" } } } } } */
