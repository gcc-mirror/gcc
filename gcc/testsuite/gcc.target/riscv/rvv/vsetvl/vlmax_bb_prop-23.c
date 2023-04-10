/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + 100);
      *(vuint32mf2_t*)(out + 100) = v;
    }
  else
    {
      if (cond == 2)
        {
          vuint32mf2_t v = *(vuint32mf2_t*)(in + 200);
          *(vuint32mf2_t*)(out + 200) = v;
          out[1000] = 8000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + i);
      *(vuint32mf2_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9]\:} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
