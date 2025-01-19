/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f2 (int32_t * restrict in, int32_t * restrict in2, int32_t * restrict out, int32_t * restrict out2, size_t n, size_t m, size_t cond, size_t cond2)
{
  for (int i = 0; i < n; i++){
    out2[i] = in2[i] + out[i];
  }
  for (int i = 0; i < n; i++){
    out[i] = in[i] & out2[i];
  }
  for (int i = 0; i < n; i++){
    out2[i] = out[i] * out2[i];
  }
  for (size_t i = 0; i < n; i++)
    {
      if (i > cond) {
        if (cond2) {
          for (int j = 0; j < m; j++) {
            vint8mf8_t v2 = *(vint8mf8_t*)(in2 + i + 100 + j);
            *(vint8mf8_t*)(out2 + i + 100 + j) = v2;
          }
        } else {
          for (int j = 0; j < m; j++) {
            vfloat32mf2_t v3 = *(vfloat32mf2_t*)(in2 + i + 300 + j);
            *(vfloat32mf2_t*)(out2 + i + 100 + j) = v3;
          }
        }
      } else {
        vbool64_t v = *(vbool64_t*)(in + i + 400);
        *(vbool64_t*)(out + i + 400) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
