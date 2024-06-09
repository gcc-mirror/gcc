/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, size_t n, size_t cond, size_t cond2)
{
  for (size_t i = 0; i < n; i++)
    {
      if (i == cond) {
        vint8mf8_t v = *(vint8mf8_t*)(in + i + 100);
        *(vint8mf8_t*)(out + i + 100) = v;
      } else if (i == cond2) {
        vfloat32mf2_t v = *(vfloat32mf2_t*)(in + i + 200);
        *(vfloat32mf2_t*)(out + i + 200) = v;
      } else if (i == (cond2 - 1)) {
        vuint16mf2_t v = *(vuint16mf2_t*)(in + i + 300);
        *(vuint16mf2_t*)(out + i + 300) = v;
      } else {
        vint8mf4_t v = *(vint8mf4_t*)(in + i + 400);
        *(vint8mf4_t*)(out + i + 400) = v;
      }
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 4 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
