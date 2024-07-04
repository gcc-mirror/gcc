/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize" } */

#include "riscv_vector.h"

double f0 (int8_t * restrict in, int8_t * restrict out, int n, int m, unsigned cond, size_t vl, double scalar)
{
  vbool4_t mask = *(vbool4_t*) (in + 1000000);
  *(vbool4_t*) (out + 1000000) = mask;

  vfloat64m1_t v = *(vfloat64m1_t*)(in + 300000);
  for (size_t i = 0; i < n; i++)
    {
      v = __riscv_vfmv_s_f_f64m1_tu (v, (scalar + i), 3);
    }
  return __riscv_vfmv_f_s_f64m1_f64 (v);
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-not {vsetvli\s+zero,\s*zero} { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
