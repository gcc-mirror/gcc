/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void fn3 (void);

void f (int32_t * restrict in, int32_t * restrict out, int32_t * restrict in2, int32_t * restrict out2,  int n, int cond)
{
  if (cond == 0)
    {
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
    }
  else
   {
     fn3 ();
   }
  for (int i = 0; i < n; i++) 
    {
      vint8mf8_t v1;
      *(vint8mf8_t*)(out + i + 10) = v1;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
