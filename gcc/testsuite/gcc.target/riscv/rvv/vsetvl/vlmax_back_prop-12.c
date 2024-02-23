/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, int n, int cond, int cond2, int cond3)
{
  for (int i = 0; i < n; i++) {
    vint8mf8_t v = *(vint8mf8_t*)(in + i);
    *(vint8mf8_t*)(out + i + 200) = v;
  }
  for (int i = 0; i < n; i++) {
    vint32mf2_t v = *(vint32mf2_t*)(in + 200 + i);
    *(vint32mf2_t*)(out + i + 400) = v;
  }
  for (int i = 0; i < n; i++) {
    vint64m1_t v = *(vint64m1_t*)(in + 300 + i);
    *(vint64m1_t*)(out + i + 400) = v;
  }
  for (int i = 0; i < n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 400 + i);
    *(vfloat32mf2_t*)(out + i + 500) = v;
  }
  for (int i = 0; i < n; i++) {
    vfloat64m1_t v = *(vfloat64m1_t*)(in + 500 + i);
    *(vfloat64m1_t*)(out + i + 600) = v;
  }

  if (cond == 0)
  {
    if (cond2 == 11)
    {
      for (int i = 0; i < n; i++)
      {
        out[i] = out[i] + 2;
      }
    }
    else if (cond2 == 111)
    {
      if (cond3 == 300)
      {
        for (int i = 0; i < n; i++)
        {
          out[i] = out[i] + out[i];
        }
      } else {
        for (int i = 0; i < n; i++) {
          vint8mf2_t v = *(vint8mf2_t*)(in + 2000 + i);
          *(vint8mf2_t*)(out + i + 4000) = v;
        }
      }
    }
  }

  for (int i = 0; i < n; i++) {
    vint16mf4_t v;
    *(vint16mf4_t*)(out + i + 700) = v;
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto" } } } } */
