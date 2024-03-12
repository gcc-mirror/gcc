/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */
// PR113249
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void foo (int8_t * restrict in, int8_t * restrict out, int n, int cond1, int cond2)
{
  vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 10000);
  *(vfloat32mf2_t*)(out + 10000) = v;

  if (cond1)
    {
      vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 30000);
      *(vfloat32mf2_t*)(out + 30000) = v;
    }
  else
    {
      vint8mf8_t v = *(vint8mf8_t*)(in + 20000);
      *(vint8mf8_t*)(out + 20000) = v;
    }

  for (int i = 0; i < n; i++)
    {
      vint32mf2_t v0 = *(vint32mf2_t*)(in + i + 100);
      
      vint16mf2_t v1 = *(vint16mf2_t*)(in + i + 200);
      vint8mf2_t v2 = *(vint8mf2_t*)(in + i + 300);
      vint8mf4_t v3 = *(vint8mf4_t*)(in + i + 400);
      vint8mf8_t v4 = *(vint8mf8_t*)(in + i + 500);
      vbool1_t v5 = *(vbool1_t*)(in + i + 600);

      vint32mf2_t v6 = *(vint32mf2_t*)(in + i + 700);

      *(vint32mf2_t*)(out + i + 100) = v0;
      *(vint16mf2_t*)(out + i + 200) = v1;
      *(vint8mf2_t*)(out + i + 300) = v2;
      *(vint8mf4_t*)(out + i + 400) = v3;
      *(vint8mf8_t*)(out + i + 500) = v4;
      *(vbool1_t*)(out + i + 600) = v5;
      *(vint32mf2_t*)(out + i + 700) = v6;
    }
  
  if (cond2)
    {
      vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 40000);
      *(vfloat32mf2_t*)(out + 40000) = v;
    }
  else
    {
      vint16mf4_t v = *(vint16mf4_t*)(in + 30000);
      *(vint16mf4_t*)(out + 30000) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 13 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
