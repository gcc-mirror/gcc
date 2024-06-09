/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n)
{
  switch (n)
  {
  case 0:{
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 500);
    *(vfloat32mf2_t*)(out + 500) = v;
    break;
  }
  case 1:{
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 500);
    *(vfloat32mf2_t*)(out + 500) = v;
    break;
  }
  default:{
    break;
  }
  }

  for (int i = 0 ; i < n * n; i++)
    out[i] = out[i] + out[i];
  
  for (int i = 0 ; i < n * n * n; i++)
    out[i] = out[i] * out[i];

  for (int i = 0 ; i < n * n * n * n; i++)
    out[i] = out[i] * out[i];
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 900 + i);
    *(vfloat32mf2_t*)(out + 900 + i) = v;
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
