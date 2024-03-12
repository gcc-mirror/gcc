/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */
// PR113249
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n)
{
  vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 10000);
  *(vfloat32mf2_t*)(out + 10000) = v;
  for (int i = 0; i < n; i++)
    {
      vint16mf2_t v1 = *(vint16mf2_t*)(in + i + 1);
      vint32mf2_t v2 = *(vint32mf2_t*)(in + i + 2);
      *(vint16mf2_t*)(out + i + 1) = v1;
      *(vint32mf2_t*)(out + i + 2) = v2;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" no-opts "-funroll-loops" } } } } */
