/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

/* Check vsetvl instruction is hoisted outside the loop, so it should
   stay before label.  */

void foo2 (void * restrict in, void * restrict out, int n)
{
  for (int i = 0; i < n; i++)
    {
      vint32mf2_t v = *(vint32mf2_t*)(in + i);
      *(vint32mf2_t*)(out + i) = v;
    }
}
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9]\:\s+vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
