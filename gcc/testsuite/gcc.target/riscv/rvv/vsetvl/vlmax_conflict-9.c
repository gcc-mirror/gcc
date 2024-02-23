/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, size_t n, size_t m, size_t cond, size_t cond2)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i != cond) {
        vbool1_t v = *(vbool1_t*)(in + i + 400);
        *(vbool1_t*)(out + i + 400) = v;
        for (int j = 0; j < m; j++) {
          vint32mf2_t v = *(vint32mf2_t*)(in + i + 100 + j);
          *(vint32mf2_t*)(out + i + 100 + j) = v;
        }
      } else if (i == cond2) {
        vuint16mf2_t v = *(vuint16mf2_t*)(in + i + 200);
        *(vuint16mf2_t*)(out + i + 200) = v;
      } else {
        vint8mf8_t v = *(vint8mf8_t*)(in + i + 100);
        *(vint8mf8_t*)(out + i + 100) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 7 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
