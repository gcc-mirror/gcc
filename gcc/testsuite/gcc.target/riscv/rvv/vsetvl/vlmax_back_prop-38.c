/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, int32_t * restrict in2, int32_t * restrict out2,  int n, int cond, int cond2)
{
  if (cond == 0)
    {
      if (cond2) {
      for (int i = 0; i < n; i++) 
      {
        vfloat32mf2_t v1 = *(vfloat32mf2_t*)(in + 1000);
        *(vfloat32mf2_t*)(out + i + 1000) = v1;
      }
      for (int i = 0; i < n; i++) 
      {
        vint32mf2_t v1 = *(vint32mf2_t*)in;
        *(vint32mf2_t*)(out + i + 10) = v1;
      }
    for (int i = 0; i < n; i++) 
      {
        vint16mf4_t v1 = *(vint16mf4_t*)(in + 100);
        *(vint16mf4_t*)(out + i + 100) = v1;
      }
      } else {
        for (int i = 0; i < n; i++) 
      {
        vfloat32mf2_t v1 = *(vfloat32mf2_t*)(in + 2000);
        *(vfloat32mf2_t*)(out + i + 2000) = v1;
      }
      for (int i = 0; i < n; i++) 
      {
        vint32mf2_t v1 = *(vint32mf2_t*)(in + 200);
        *(vint32mf2_t*)(out + i + 200) = v1;
      }
    for (int i = 0; i < n; i++) 
      {
        vint16mf4_t v1 = *(vint16mf4_t*)(in + 300);
        *(vint16mf4_t*)(out + i + 300) = v1;
      }
      }
    }
  else
   {
     for (int i = 0; i < n; i++) 
      {
        vint16mf4_t v1 = *(vint16mf4_t*)(in + 30000);
        *(vint16mf4_t*)(out + i + 30000) = v1;
      }
    for (int i = 0; i < n; i++) 
      {
        vint32mf2_t v1 = *(vint32mf2_t*)(in + 40000);
        *(vint32mf2_t*)(out + i + 40000) = v1;
      }
    for (int i = 0; i < n; i++) 
      {
        vuint32mf2_t v1 = *(vuint32mf2_t*)(in + 50000);
        *(vuint32mf2_t*)(out + i + 50000) = v1;
      }
   }
  for (int i = 0; i < n; i++) 
    {
      vint8mf8_t v1;
      *(vint8mf8_t*)(out + i + 10) = v1;
    }
}


/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 4 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
