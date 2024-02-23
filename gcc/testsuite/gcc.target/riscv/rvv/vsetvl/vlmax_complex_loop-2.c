/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, size_t n, size_t m, int cond)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i % 2) {
        
        if (cond) {
          vfloat32mf2_t v = *(vfloat32mf2_t*)(in + i + 2000);
          *(vfloat32mf2_t*)(out + i + 2000) = v;
        } else {
          vfloat32mf2_t v = *(vfloat32mf2_t*)(in + i + 3000);
          *(vfloat32mf2_t*)(out + i + 3000) = v;
        }
        
        for (size_t j = 0; j < m; j += 1) {
          if (j % 2 == 0) {
            vint8mf8_t v = *(vint8mf8_t*)(in + i + j + 100);
            *(vint8mf8_t*)(out + i + j + 100) = v;
          } else {
            vint8mf8_t v = *(vint8mf8_t*)(in + i + j + 200);
            *(vint8mf8_t*)(out + i + j + 200) = v;
          }
        }
        
        if (cond) {
          vuint16mf4_t v = *(vuint16mf4_t*)(in + i + 7000);
          *(vuint16mf4_t*)(out + i + 7000) = v;
        } else {
          vuint16mf4_t v = *(vuint16mf4_t*)(in + i + 8000);
          *(vuint16mf4_t*)(out + i + 8000) = v;
        }
      } else {
        if (cond) {
          vuint16mf2_t v = *(vuint16mf2_t*)(in + i + 4000);
          *(vuint16mf2_t*)(out + i + 4000) = v;
        } else {
          vuint16mf2_t v = *(vuint16mf2_t*)(in + i + 5000);
          *(vuint16mf2_t*)(out + i + 5000) = v;
        }
        
        vbool1_t v = *(vbool1_t*)(in + i + 300);
        *(vbool1_t*)(out + i + 300) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 5 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-not {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
