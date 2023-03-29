/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void foo5_3 (int32_t * restrict in, int32_t * restrict out, size_t n, int cond)
{
  vint8mf2_t v;
  for (size_t i = 0; i < n; i++)
    {
      if (i % 2 == 0) {
        v = *(vint8mf2_t*)(in + i);
      } else {
        *(vint8mf2_t*)(out + i) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+j\s+\.L[0-9]+} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
