/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void foo5_3 (int32_t * restrict in, int32_t * restrict out, size_t n, int cond)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i % 16 == 0) {
        vint8mf8_t v = *(vint8mf8_t*)(in + i + 100);
        *(vint8mf8_t*)(out + i + 100) = v;
      } else if (i % 8 == 0) {
        vint16mf4_t v = *(vint16mf4_t*)(in + i + 200);
        *(vint16mf4_t*)(out + i + 200) = v;
      } else if (i % 4 == 0) {
        vint32mf2_t v = *(vint32mf2_t*)(in + i + 300);
        *(vint32mf2_t*)(out + i + 300) = v;
      } else {
        vbool64_t v = *(vbool64_t*)(in + i + 400);
        *(vbool64_t*)(out + i + 400) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */

