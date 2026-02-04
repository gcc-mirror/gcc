/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcxtheadvector -mabi=lp64d -mtune=rocket" } */

/* In order to trigger the issue strided broadcast must be enabled
   and we must broadcast integer memory operands via strided load.
   This is disabled and there is currently no param to change it
   so the test will pass either way.  */

#include <riscv_vector.h>

void test(int16_t *dst, long vl) {
  vuint16m1_t v6 = __riscv_vmv_v_x_u16m1(*dst, vl);
  vfloat32m2_t s0 = __riscv_vfmv_v_f_f32m2(0, vl);
  __riscv_vse16_v_u16m1(dst, v6, vl);
  __riscv_vse32_v_f32m2((float*)dst, s0, vl);
}

/* { dg-final { scan-assembler-times "th.vsetvli\tzero,a1,e16,m1" 2 } } */
/* { dg-final { scan-assembler-times "th.vsetvli\tzero,a1,e32,m2" 2 } } */
