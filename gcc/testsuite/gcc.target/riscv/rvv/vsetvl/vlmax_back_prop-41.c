/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void foo5_5 (int32_t * restrict in, int32_t * restrict out, size_t n, size_t m, int cond)
{
  vint8mf2_t v;
  for (size_t i = 0; i < n; i++)
    {
      if (i % 2) {
        for (size_t j = 0; j < m; j += 1) {
          if (j % 2 == 0) {
            v = *(vint8mf2_t*)(in + i + j);
          } else {
            *(vint8mf2_t*)(out + i + j) = v;
          }
        }
      } else {
        *(vint8mf2_t*)(out + i) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
