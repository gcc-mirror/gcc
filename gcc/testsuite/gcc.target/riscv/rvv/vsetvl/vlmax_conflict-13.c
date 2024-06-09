/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, size_t n, size_t cond, size_t cond2)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i > cond) {
        vint8mf8_t v = *(vint8mf8_t*)(in + i + 100);
        *(vint8mf8_t*)(out + i + 100) = v;
      } else {
        vbool1_t v = *(vbool1_t*)(in + i + 400);
        *(vbool1_t*)(out + i + 400) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
