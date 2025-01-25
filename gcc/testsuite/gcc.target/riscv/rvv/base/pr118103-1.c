/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvfh -mabi=lp64d" } */

#include "riscv_vector.h"

#define N 4
typedef _Float16 float16_t;
float16_t a[N]; float16_t b[N];

extern void test ();

int test_exampe () {
  test ();

  size_t vl = N;
  vfloat16m1_t va = __riscv_vle16_v_f16m1(a, vl);
  va = __riscv_vfnmadd_vv_f16m1_rm(va, va, va, __RISCV_FRM_RDN, vl);
  va = __riscv_vfmsac_vv_f16m1(va, va, va, vl);

  __riscv_vse16_v_f16m1(b, va, vl);

  return 0;
}

/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 1 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 1 } } */
