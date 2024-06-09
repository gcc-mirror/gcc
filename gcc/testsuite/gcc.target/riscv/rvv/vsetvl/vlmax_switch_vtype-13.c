/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n)
{
  vint8mf8_t v1;
  vint16mf4_t v2;
  
  *(vint8mf8_t*)(out + 1) = v1;
  *(vint16mf4_t*)(out + 2) = v2;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-funroll-loops" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
