/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * in, int32_t * out, int n, int cond)
{
  if (cond) {
    vint32mf2_t v = *(vint32mf2_t*) (in + 100);
    *(vint32mf2_t*) (out + 100) = v;
  } else {
    vint16mf2_t v = *(vint16mf2_t*) (in + 200);
    *(vint16mf2_t*) (out + 200) = v;
  }
  
  for (int i = 0; i < n; i++) {
    vint8mf8_t v = *(vint8mf8_t*) (in + 300 + i);
    *(vint8mf8_t*) (out + 300 + i) = v;
  }
}

/* { dg-final { scan-assembler-times {\.L[0-9]+:\s+vle8\.v} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
