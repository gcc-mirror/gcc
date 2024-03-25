/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"

void foo(void *in1, void *out, size_t avl) {

  size_t vl = __riscv_vsetvl_e32m1(avl);
  vint32m1_t v = __riscv_vmv_v_x_i32m1 (vl, 16);
  __riscv_vse32_v_i32m1 (out, v, 16);
}
