/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void foo5_5 (int32_t * restrict in, int32_t * restrict out, size_t n, size_t m, int cond)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i % 2) {
        for (size_t j = 0; j < m; j += 1) {
          if (i % 16 == 0) {
          vint8mf8_t v = *(vint8mf8_t*)(in + i + 100 + j);
          *(vint8mf8_t*)(out + i + 100 + j) = v;
        } else if (i % 8 == 0) {
          vint16mf4_t v = *(vint16mf4_t*)(in + i + 200 + j);
          *(vint16mf4_t*)(out + i + 200 + j) = v;
        } else if (i % 4 == 0) {
          vint32mf2_t v = *(vint32mf2_t*)(in + i + 300 + j);
          *(vint32mf2_t*)(out + i + 300 + j) = v;
        } else {
          vbool64_t v = *(vbool64_t*)(in + i + 400 + j);
          *(vbool64_t*)(out + i + 400 + j) = v;
        }
        }
      } else {
        vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 88888);
        *(vfloat32mf2_t*)(out + 88888) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+j\s+\.L[0-9]+} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
