/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, size_t n, size_t cond, size_t cond2)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i == cond) {
        vint8mf8_t v = *(vint8mf8_t*)(in + i + 100);
        *(vint8mf8_t*)(out + i + 100) = v;
      } else if (i == cond2) {
        vfloat32mf2_t v = *(vfloat32mf2_t*)(in + i + 200);
        *(vfloat32mf2_t*)(out + i + 200) = v;
      } else {
        vbool1_t v = *(vbool1_t*)(in + i + 400);
        *(vbool1_t*)(out + i + 400) = v;
      }
    }
}


/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */

