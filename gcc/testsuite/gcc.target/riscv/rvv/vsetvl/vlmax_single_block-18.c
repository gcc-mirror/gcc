/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */
// PR113249
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out)
{
  vint8mf8_t v1 = *(vint8mf8_t*)(in + 1);
  vint16mf4_t v2 = *(vint16mf4_t*)(in + 2);
  vint32mf2_t v3 = *(vint32mf2_t*)(in + 3);
  vfloat32mf2_t v4 = *(vfloat32mf2_t*)(in + 4);
  
  vint8mf4_t v5 = *(vint8mf4_t*)(in + 5);
  vint16mf2_t v6 = *(vint16mf2_t*)(in + 6);
  
  vint8mf2_t v7 = *(vint8mf2_t*)(in + 7);
  
  *(vint8mf8_t*)(out + 1) = v1;
  *(vint16mf4_t*)(out + 2) = v2;
  *(vint32mf2_t*)(out + 3) = v3;
  *(vfloat32mf2_t*)(out + 4) = v4;
  
  *(vint8mf4_t*)(out + 5) = v5;
  *(vint16mf2_t*)(out + 6) = v6;
  
  *(vint8mf2_t*)(out + 7) = v7;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 6 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
