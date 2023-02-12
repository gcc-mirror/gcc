/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

/* Test insert-vsetvl PASS whether it's able to eliminate vsetvl for same vtype in VLMAX.  */

#include "riscv_vector.h"

void foo3 (void * restrict in, void * restrict out)
{
  vfloat32mf2_t v1 = *(vfloat32mf2_t*)(in + 1);
  vfloat32mf2_t v2 = *(vfloat32mf2_t*)(in + 2);
  vfloat32mf2_t v3 = *(vfloat32mf2_t*)(in + 3);
  vfloat32mf2_t v4 = *(vfloat32mf2_t*)(in + 4);
  vfloat32mf2_t v5 = *(vfloat32mf2_t*)(in + 5);
  vfloat32mf2_t v6 = *(vfloat32mf2_t*)(in + 6);
  vfloat32mf2_t v7 = *(vfloat32mf2_t*)(in + 7);
  vfloat32mf2_t v8 = *(vfloat32mf2_t*)(in + 8);
  *(vfloat32mf2_t*)(out + 1) = v1;
  *(vfloat32mf2_t*)(out + 2) = v2;
  *(vfloat32mf2_t*)(out + 3) = v3;
  *(vfloat32mf2_t*)(out + 4) = v4;
  *(vfloat32mf2_t*)(out + 5) = v5;
  *(vfloat32mf2_t*)(out + 6) = v6;
  *(vfloat32mf2_t*)(out + 7) = v7;
  *(vfloat32mf2_t*)(out + 8) = v8;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
