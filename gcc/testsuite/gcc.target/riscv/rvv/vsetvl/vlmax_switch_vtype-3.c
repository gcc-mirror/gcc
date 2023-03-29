/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n)
{
  vint8mf8_t v1 = *(vint8mf8_t*)(in + 1);
  vint16mf4_t v2 = *(vint16mf4_t*)(in + 2);
  vint32mf2_t v3 = *(vint32mf2_t*)(in + 3);
  vfloat32mf2_t v4 = *(vfloat32mf2_t*)(in + 4);
  
  *(vint8mf8_t*)(out + 1) = v1;
  *(vint16mf4_t*)(out + 2) = v2;
  *(vint32mf2_t*)(out + 3) = v3;
  *(vfloat32mf2_t*)(out + 4) = v4;
  
  for (int i = 0; i < n; i++)
    {
      vint16mf2_t v = *(vint16mf2_t*)(in + i + 5);
      *(vint16mf2_t*)(out + i + 5) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" no-opts "-funroll-loops" } } } } */
