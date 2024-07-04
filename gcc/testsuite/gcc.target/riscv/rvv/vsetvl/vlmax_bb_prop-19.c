/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f6 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 100);
      *(vfloat32mf2_t*)(out + 100) = v;
    }
  else
    {
      vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 200);
      *(vfloat32mf2_t*)(out + 200) = v;
      if (cond == 2)
        {
          out[1000] = 8000;
        }
      else
        {
          out[2000] = 9000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vfloat32mf2_t v = *(vfloat32mf2_t*)(in + i);
      *(vfloat32mf2_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {add\ta[0-7],a[0-7],a[0-7]\s+\.L[0-9]\:\s+vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
