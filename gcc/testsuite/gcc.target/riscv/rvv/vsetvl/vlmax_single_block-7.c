/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */

/* Test insert-vsetvl PASS whether it's able to eliminate vsetvl for same vtype in VLMAX.  */

#include "riscv_vector.h"

void foo3 (void * restrict in, void * restrict out)
{
  vuint32mf2_t v1 = *(vuint32mf2_t*)(in + 1);
  vuint32mf2_t v2 = *(vuint32mf2_t*)(in + 2);
  vuint32mf2_t v3 = *(vuint32mf2_t*)(in + 3);
  vuint32mf2_t v4 = *(vuint32mf2_t*)(in + 4);
  vuint32mf2_t v5 = *(vuint32mf2_t*)(in + 5);
  vuint32mf2_t v6 = *(vuint32mf2_t*)(in + 6);
  vuint32mf2_t v7 = *(vuint32mf2_t*)(in + 7);
  vuint32mf2_t v8 = *(vuint32mf2_t*)(in + 8);
  *(vuint32mf2_t*)(out + 1) = v1;
  *(vuint32mf2_t*)(out + 2) = v2;
  *(vuint32mf2_t*)(out + 3) = v3;
  *(vuint32mf2_t*)(out + 4) = v4;
  *(vuint32mf2_t*)(out + 5) = v5;
  *(vuint32mf2_t*)(out + 6) = v6;
  *(vuint32mf2_t*)(out + 7) = v7;
  *(vuint32mf2_t*)(out + 8) = v8;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
