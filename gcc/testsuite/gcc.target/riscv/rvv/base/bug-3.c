/* { dg-do run { target { riscv_vector } } } */
/* { dg-options "-O2" } */

#include "riscv_vector.h"
#include <stdio.h>

int main()
{
  int32_t a = 1;
  int32_t b[1] = {3};
  int32_t c[1] = {10};
  int32_t d[1] = {0};
  vint32m1_t vb = __riscv_vle32_v_i32m1 (b, 1);
  vint32m1_t vc = __riscv_vle32_v_i32m1 (c, 1);
  vint32m1_t vd = __riscv_vnmsub_vx_i32m1 (vb, a, vc, 1);
  __riscv_vse32_v_i32m1 (d, vd, 1);
  if (d[0] != 7){
      printf("d[0] should be 7, but got %d\n", d[0]);
      __builtin_abort ();
  }
  return 0;
}
