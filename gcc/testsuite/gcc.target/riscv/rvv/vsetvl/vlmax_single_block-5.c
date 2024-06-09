/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */

/* Test insert-vsetvl PASS whether it's able to eliminate vsetvl for same vtype in VLMAX.  */

#include "riscv_vector.h"

void foo2 (void * restrict in, void * restrict out)
{
  vuint16mf4_t v1 = *(vuint16mf4_t*)(in + 1);
  vuint16mf4_t v2 = *(vuint16mf4_t*)(in + 2);
  vuint16mf4_t v3 = *(vuint16mf4_t*)(in + 3);
  vuint16mf4_t v4 = *(vuint16mf4_t*)(in + 4);
  vuint16mf4_t v5 = *(vuint16mf4_t*)(in + 5);
  vuint16mf4_t v6 = *(vuint16mf4_t*)(in + 6);
  vuint16mf4_t v7 = *(vuint16mf4_t*)(in + 7);
  vuint16mf4_t v8 = *(vuint16mf4_t*)(in + 8);
  *(vuint16mf4_t*)(out + 1) = v1;
  *(vuint16mf4_t*)(out + 2) = v2;
  *(vuint16mf4_t*)(out + 3) = v3;
  *(vuint16mf4_t*)(out + 4) = v4;
  *(vuint16mf4_t*)(out + 5) = v5;
  *(vuint16mf4_t*)(out + 6) = v6;
  *(vuint16mf4_t*)(out + 7) = v7;
  *(vuint16mf4_t*)(out + 8) = v8;
}

void foo3 (void * restrict in, void * restrict out)
{
  vuint16mf2_t v1 = *(vuint16mf2_t*)(in + 1);
  vuint16mf2_t v2 = *(vuint16mf2_t*)(in + 2);
  vuint16mf2_t v3 = *(vuint16mf2_t*)(in + 3);
  vuint16mf2_t v4 = *(vuint16mf2_t*)(in + 4);
  vuint16mf2_t v5 = *(vuint16mf2_t*)(in + 5);
  vuint16mf2_t v6 = *(vuint16mf2_t*)(in + 6);
  vuint16mf2_t v7 = *(vuint16mf2_t*)(in + 7);
  vuint16mf2_t v8 = *(vuint16mf2_t*)(in + 8);
  *(vuint16mf2_t*)(out + 1) = v1;
  *(vuint16mf2_t*)(out + 2) = v2;
  *(vuint16mf2_t*)(out + 3) = v3;
  *(vuint16mf2_t*)(out + 4) = v4;
  *(vuint16mf2_t*)(out + 5) = v5;
  *(vuint16mf2_t*)(out + 6) = v6;
  *(vuint16mf2_t*)(out + 7) = v7;
  *(vuint16mf2_t*)(out + 8) = v8;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
