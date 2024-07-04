/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, int n, int cond)
{
  for (int i = 0; i < n; i++) {
    vint8mf8_t v = *(vint8mf8_t*)in;
    *(vint8mf8_t*)(out + i + 200) = v;
  }
  for (int i = 0; i < n; i++) {
    vint32mf2_t v = *(vint32mf2_t*)(in + 200);
    *(vint32mf2_t*)(out + i + 400) = v;
  }
  for (int i = 0; i < n; i++) {
    vint64m1_t v = *(vint64m1_t*)(in + 300);
    *(vint64m1_t*)(out + i + 400) = v;
  }
  for (int i = 0; i < n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 400);
    *(vfloat32mf2_t*)(out + i + 500) = v;
  }
  for (int i = 0; i < n; i++) {
    vfloat64m1_t v = *(vfloat64m1_t*)(in + 500);
    *(vfloat64m1_t*)(out + i + 600) = v;
  }

  if (cond == 0) {
    for (int i = 0; i < n; i++){
      vint8mf8_t v = *(vint8mf8_t*)(in + 771 + i);
      *(vint8mf8_t*)(out + 771 + i) = v;
      vint32mf2_t v2 = *(vint32mf2_t*)(in + 71 + i);
      *(vint32mf2_t*)(out + 71 + i) = v2;
      vfloat32mf2_t v3 = *(vfloat32mf2_t*)(in + 17 + i);
      *(vfloat32mf2_t*)(out + 17 + i) = v3;
       vuint32mf2_t v4 = *(vuint32mf2_t*)(in + 117 + i);
      *(vuint32mf2_t*)(out + 117 + i) = v4;
    }
  } else {
    vfloat32mf2_t v0 = *(vfloat32mf2_t*)(in + 1123);
    *(vfloat32mf2_t*)(out + 1123) = v0;
    vint8mf8_t v = *(vint8mf8_t*)(in + 333);
    *(vint8mf8_t*)(out + 333) = v;
    vbool64_t v2 = *(vbool64_t*)(in + 91);
    *(vbool64_t*)(out + 91) = v2;
  }
 
  for (int i = 0; i < n; i++) {
    vint16mf4_t v;
    *(vint16mf4_t*)(out + i + 700) = v;
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
